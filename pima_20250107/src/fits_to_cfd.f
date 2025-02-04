       PROGRAM    FITS_TO_CFD_MAIN
       IMPLICIT   NONE 
       INCLUDE   'pima.i'
       CHARACTER  STR*128
       INTEGER*8    STACK_SIZE_IN_BYTES, GB, IS
       PARAMETER  ( GB = 1024*1024*1024 )
       PARAMETER  ( STACK_SIZE_IN_BYTES = PIMA__STACK_SIZE_IN_GIGABYTES * GB )
       INTEGER*8, EXTERNAL :: SET_STACKSIZE 
!
! ---- Set stacksize
!
       IS = SET_STACKSIZE ( %VAL(STACK_SIZE_IN_BYTES) )
       CALL INCH8    ( STACK_SIZE_IN_BYTES/INT8(1024), STR )
       CALL SETENV   ( 'GOMP_STACKSIZE'//CHAR(0), TRIM(STR)//CHAR(0), %VAL(1) )
       CALL FITS_TO_CFD()
       END  PROGRAM  FITS_TO_CFD_MAIN
!
! ------------------------------------------------------------------------
!
       SUBROUTINE  FITS_TO_CFD()
! ************************************************************************
! *                                                                      *
! *   Program  FITS_TO_CFD  computes the correlated flux of a sources    *
! *   observed with the VLBA using table using both visilibity data in   *
! *   fits format and image map in fits format. The ascii table is       *
! *   either written in file or at the terminal.                         *
! *                                                                      *
! * ### 29-JAN-2007  FITS_TO_CFD  v1.1 (c)  L. Petrov  08-DEC-2024  ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE    'sou_map.i'
      INCLUDE    'pima_local.i'
      TYPE     ( SOUMAP__TYPE ) :: MAP
      TYPE     ( VIS__TYPE    ) :: VIS
      INTEGER*4  MARG, MBUF
      PARAMETER  ( MARG =   32 )
      PARAMETER  ( MBUF = 1024 )
      CHARACTER  FINAM_VIS*256, FINAM_MAP*256, ARGS(MARG)*256, FILOUT*128, &
     &           HELP_FILE*128, BUF(MBUF)*80
      CHARACTER  IMAGE_BAND*1, IMAGE_DATE*10, IMAGE_NAME*10, OUT(2)*88
      PARAMETER  ( HELP_FILE = PIMA__ROOT//'/share/pima/fits_to_cfd_help.txt' )
      REAL*8     MAGNIF, SIGLEV
      LOGICAL*4  FL_IMA, FL_CC, FL_WEI_USE
      REAL*8     FLUX_INT, FLUX_SHR, FLUX_MID, FLUX_UNR, FLUX_NOI, CUTOFF_NERR
      INTEGER*4  MODE, J1, J2, LBUF, L_OBS, L_SCA, LUN, IUER
      INTEGER*4, EXTERNAL :: GET_UNIT, ILEN, I_LEN
!
      IF ( IARGC() < 1 ) THEN
           WRITE ( 6, '(A)' ) 'Run fits_to_cfd -help for help'
           CALL EXIT ( 1 )
         ELSE
           FL_WEI_USE  = .FALSE.
           CUTOFF_NERR = 0.5
!
! -------- Get arguments
!
           DO 410 J1=1,IARGC()
              CALL GETARG ( J1, ARGS(J1) )
 410       CONTINUE 
!
           IF ( ARGS(1)(1:2) == '-h' .OR. ARGS(1)(1:3) == '--h' ) THEN
!
! ------------- Read the help file
!
                IUER = -1
                CALL RD_TEXT ( HELP_FILE, MBUF, BUF, LBUF, IUER )
                IF ( IUER .NE. 0 ) THEN
                     CALL ERR_LOG ( 1201, -2, 'FITS_TO_CFD', 'Failure in '// &
     &                   'an attempt to read a text file with help '// &
     &                   'information '//HELP_FILE )
                     CALL EXIT ( 1 )
                END IF
!
! ------------- ... and print ti at the screen
!
                IUER = -1
                CALL WR_TEXT ( LBUF, BUF, '-', IUER )
                CALL EXIT ( 0 )
           END IF
!
           MODE = 1
           FILOUT = '-'
!
! -------- Parse arguments
!
           DO 420 J2=1,IARGC()
              IF ( ARGS(J2)(1:I_LEN(ARGS(J2))) == '-o' ) THEN
                   FILOUT = ARGS(J2+1)
                   IF ( ILEN(FILOUT) == 0 ) THEN
                        CALL ERR_LOG ( 1401, -2, 'FITS_TO_CFD', 'No value '// &
     &                      'of qualifier -o was supplied' )
                        CALL EXIT ( 0 )
                   END IF
                 ELSE IF ( ARGS(J2)(1:I_LEN(ARGS(J2))) == '-mode' ) THEN
                   IF ( ILEN(ARGS(J2+1)) == 0 ) THEN
                        CALL ERR_LOG ( 1402, -2, 'FITS_TO_CFD', 'No value '// &
     &                      'of qualifier -mode was supplied' )
                        CALL EXIT ( 0 )
                   END IF
                   IF ( INDEX ( ARGS(J2+1), '.' ) .LE. 0 ) THEN
                        ARGS(J2+1) = ARGS(J2+1)(1:I_LEN(ARGS(J2+1)))//'.0'
                   END IF
                   READ ( UNIT=ARGS(J2+1), FMT='(F10.5)' ) MAGNIF
                 ELSE IF ( ARGS(J2)(1:I_LEN(ARGS(J2))) == '-wei' ) THEN
                   IF ( ILEN(ARGS(J2+1)) == 0 ) THEN
                        CALL ERR_LOG ( 1403, -2, 'FITS_TO_CFD', &
     &                      'No value of qualifier -gap was supplied' )
                        CALL EXIT ( 0 )
                   END IF
                   IF ( ARGS(J2+1)(1:1) == 'T' .OR. ARGS(J2+1)(1:1) == 't' ) THEN
                        FL_WEI_USE = .TRUE.
                      ELSE IF ( ARGS(J2+1)(1:1) == 'F' .OR. ARGS(J2+1)(1:1) == 'f' ) THEN
                        FL_WEI_USE = .FALSE.
                      ELSE 
                        CALL ERR_LOG ( 1404, -2, 'FITS_TO_CFD', &
     &                      'Wrong value of qualifier -weu was supplied: '// &
     &                       ARGS(J2+1)(1:I_LEN(ARGS(J2+1)))//' only T or F '// &
     &                      'are allowed' )
                        CALL EXIT ( 0 )
                   END IF
                 ELSE IF ( ARGS(J2)(1:4) == '-cut' ) THEN
                   IF ( ILEN(ARGS(J2+1)) == 0 ) THEN
                        CALL ERR_LOG ( 1405, -2, 'FITS_TO_CFD', &
     &                      'No value of qualifier -cutoff_err was supplied' )
                        CALL EXIT ( 0 )
                   END IF
                   IF ( INDEX ( ARGS(J2+1), '.' ) .LE. 0 ) THEN
                        ARGS(J2+1) = ARGS(J2+1)(1:I_LEN(ARGS(J2+1)))//'.0'
                   END IF
                   READ ( UNIT=ARGS(J2+1), FMT='(F10.5)' ) CUTOFF_NERR
              END IF
 420       CONTINUE 
           FINAM_VIS = ARGS(IARGC()-1)
           FINAM_MAP = ARGS(IARGC())
      END IF
!
      FL_IMA = .TRUE.
      FL_CC  = .TRUE.
!
      VIS%SKY_FRQ => NULL()
      VIS%MJD     => NULL()
      VIS%TAI     => NULL()
      VIS%VIS     => NULL()
      VIS%UV      => NULL()
      VIS%WEI     => NULL()
      VIS%IND_BAS => NULL()
      VIS%INT_TIM => NULL()
!
      MAP%FLUX_CC => NULL()
      MAP%COOR_CC => NULL()
      MAP%IMAGE   => NULL()
!
! --- Read fisibility data
!
      IUER = -1
      CALL GET_FITS_VIS ( FINAM_VIS, VIS, IUER )
      IF ( IUER .NE. 0 ) THEN
           CALL ERR_LOG ( 1202, -2, 'FITS_TO_CFD', 'Failure in an '// &
     &         'attempt to read the fits file with visibility data '// &
     &          FINAM_VIS )
           CALL EXIT ( 1 )
      END IF
!
! --- Read image 
!
      IUER = -1
      CALL GET_FITS_MAP ( FINAM_MAP, FL_CC, FL_IMA, MAP, IUER )
      IF ( IUER .NE. 0 ) THEN
           CALL ERR_LOG ( 1203, -2, 'FITS_TO_CFD', 'Failure in an '// &
     &         'attempt to read the fits file with image '//FINAM_MAP )
           CALL EXIT ( 1 )
      END IF
!
! --- Make the table with correlated flux densities and print it
!
      IUER = -1
      CALL GENERATE_CFD_TABLE ( VIS, MAP, MODE, FL_WEI_USE, CUTOFF_NERR, &
     &                          IMAGE_NAME, IMAGE_BAND, IMAGE_DATE, &
     &                          L_OBS, L_SCA, FLUX_INT, FLUX_SHR, FLUX_MID, &
     &                          FLUX_UNR, FLUX_NOI, 2, OUT, IUER )
      IF ( IUER .NE. 0 ) THEN
           CALL ERR_LOG ( 1204, -2, 'FITS_TO_CFD', 'Failure in an '// &
     &         'attempt to compute correlated flux densities and print '// &
     &         'the table' )
           CALL EXIT ( 1 )
      END IF
      IF ( FILOUT(1:1) == '-' ) THEN
           WRITE ( UNIT=6, FMT='(A)' ) OUT(1)
           WRITE ( UNIT=6, FMT='(A)' ) OUT(2)
         ELSE IF ( FILOUT(1:4) == '/dev/n' ) THEN
           CALL ERR_LOG ( 0, IUER )
           RETURN 
         ELSE 
           LUN = GET_UNIT()
           OPEN ( UNIT=LUN, FILE=FILOUT, STATUS='UNKNOWN', IOSTAT=IUER )
           IF ( IUER .NE. 0 ) THEN
                IUER = -1
                CALL ERR_LOG ( 1205, IUER, 'GENERATE_CFD_TABLE', 'Error in '// &
     &              'an attempt to open the output file '//FILOUT )
                CALL EXIT ( 1 )
           END IF
!
           WRITE ( UNIT=LUN, FMT='(A)', IOSTAT=IUER  ) OUT(1)
           IF ( IUER .NE. 0 ) THEN
                IUER = -1
                CALL ERR_LOG ( 1206, IUER, 'GENERATE_CFD_TABLE', 'Error in '// &
     &              'an attempt to write into the output file '//FILOUT )
                CALL EXIT ( 1 )
           END IF
!
           WRITE ( UNIT=LUN, FMT='(A)', IOSTAT=IUER  ) OUT(2)
           IF ( IUER .NE. 0 ) THEN
                IUER = -1
                CALL ERR_LOG ( 1207, IUER, 'GENERATE_CFD_TABLE', 'Error in '// &
     &              'an attempt to write into the output file '//FILOUT )
                CALL EXIT ( 1 )
           END IF
           CLOSE ( UNIT=LUN ) 
      END IF
!
      CALL EXIT ( 0 )
      END  SUBROUTINE  FITS_TO_CFD  !#!#
