       PROGRAM    FITS_TO_RADPLOT_MAIN
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
       CALL FITS_TO_RADPLOT()
       END  PROGRAM  FITS_TO_RADPLOT_MAIN
!
! ------------------------------------------------------------------------
!
       SUBROUTINE  FITS_TO_RADPLOT()
! ************************************************************************
! *                                                                      *
! *   Program  FITS_TO_RADPLOT  computes the amplitudes of scan-averaged *
! *   visibilities and prints the plot of their dependence on the        *
! *   length of the projected baseline to the plane tangential to the    *
! *   source direction either on the terminal or in the file in the      *
! *   Postscript or gif format.                                          *
! *                                                                      *
! *   Supports environment variable UV_PRINT for printing UV-data.       *
! *   The variable specifies the name of the output file.                *
! *                                                                      *
! *   If -auto is specified, then fits_to_radplot instead of a plot      *
! *   of calibrated cross-correlation amplitude versus baseline length   *
! *   will make a plot versus the length of the baseline projection to   *
! *   the direction which makes the scatter of the amplitude with        *
! *   respect to a smoothed curve minimal.                               *
! *                                                                      *
! *   FITS_RR_POL_ONLY                                                   *
! *                                                                      *
! * ### 29-JAN-2007  FITS_TO_RADPLOT  v1.6 (c) L. Petrov 13-OCT-2024 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE    'sou_map.i'
      TYPE     ( VIS__TYPE ) :: VIS
      CHARACTER  FINAM*256
      INTEGER*4  MARG
      PARAMETER  ( MARG = 32 )
      CHARACTER  ARGS(MARG)*256, FILOUT*128
      REAL*8     GAP_SCAN, CUTOFF_NERR
      LOGICAL*4  FL_WEI_USE, FL_AUTO
!
      INTEGER*4  J1, J2, IDEV, ICLR, ISIZE, IUER
      INTEGER*4, EXTERNAL :: ILEN, I_LEN
!
      IF ( IARGC() < 1 ) THEN
           WRITE ( 6, '(A)' ) 'Usage: fits_to_radplot -o output_file_or_XW '// &
     &                        '[-size code] [-color code] [-gap time] '// &
     &                        '[-wei T|F] [-cutoff_err value] [-auto] fits_vis_file'
           CALL EXIT ( 0 )
         ELSE
!
! -------- Get the arguments
!
           DO 410 J1=1,IARGC()
              CALL GETARG ( J1, ARGS(J1) )
 410       CONTINUE
!
! -------- Set defaults
!
           GAP_SCAN = 300.0D0
           FL_WEI_USE  = .FALSE.
           CUTOFF_NERR = 0.25

           ISIZE   = 3
           ICLR    = -1
           IDEV    = 1
           FL_AUTO = .FALSE.
!
! -------- Parse the arguments
!
           DO 420 J2=1,IARGC()
              IF ( ARGS(J2)(1:I_LEN(ARGS(J2))) == '-o' ) THEN
                   FILOUT = ARGS(J2+1)
                   IF ( ILEN(FILOUT) == 0 ) THEN
                        CALL ERR_LOG ( 1501, -2, 'FITS_TO_RADPLOT', 'No value '// &
     &                      'of qualifier -o was supplied' )
                        CALL EXIT ( 0 )
                   END IF
                   IF ( FILOUT(1:3) == '/XW' .OR. &
     &                  FILOUT(1:2) == 'XW'       ) THEN
                        CALL CLRCH ( FILOUT )
                        IDEV = 1
                      ELSE IF ( ILEN(FILOUT) > 4 ) THEN
                        IF ( FILOUT(ILEN(FILOUT)-2:ILEN(FILOUT)) == '/PS' .OR. &
     &                       FILOUT(ILEN(FILOUT)-2:ILEN(FILOUT)) == '/ps'      ) THEN
                             IDEV = 2
                             CALL CLRCH ( FILOUT(ILEN(FILOUT)-2:) )
                          ELSE IF ( FILOUT(ILEN(FILOUT)-3:ILEN(FILOUT)) == '/GIF' .OR. &
     &                       FILOUT(ILEN(FILOUT)-3:ILEN(FILOUT)) == '/gif'      ) THEN
!
                             IDEV = 3
                             CALL CLRCH ( FILOUT(ILEN(FILOUT)-3:) )
                          ELSE
                             IDEV = 2
                             IF ( INDEX ( FILOUT, '.ps' ) > 0 ) THEN
                                  IDEV = 2
                               ELSE IF ( INDEX ( FILOUT, '.gif' ) > 0 ) THEN
                                  IDEV = 3
                               ELSE IF ( INDEX ( FILOUT, '.txt' ) > 0 ) THEN
                                  IDEV = 4
                               ELSE 
                                  CALL ERR_LOG ( 1502, -2, 'FITS_TO_RADPLOT', 'unsupported '// &
     &                                'extension of the output file. One of .ps, .gif or .txt '// &
     &                                'were expected' )
                                  CALL EXIT ( 0 )
                             END IF
                        END IF
                      ELSE
                        IDEV = 2
                        IF ( INDEX ( FILOUT, '.gif' ) > 0 ) THEN
                             IDEV = 3
                           ELSE IF ( FILOUT == '-' ) THEN
                             IDEV = 4
                        END IF
                   END IF
                 ELSE IF ( ARGS(J2)(1:I_LEN(ARGS(J2))) == '-col' ) THEN
                   IF ( ILEN(ARGS(J2+1)) == 0 ) THEN
                        CALL ERR_LOG ( 1502, -2, 'FITS_TO_RADPLOT', 'No value '// &
     &                      'of qualifier -col was supplied' )
                        CALL EXIT ( 0 )
                   END IF
                   IF ( ARGS(J2+1)(1:4) == 'band'  .OR.  &
     &                  ARGS(J2+1)(1:4) == 'BAND'        ) THEN
                        ICLR = -1
                      ELSE IF ( ARGS(J2+1)(1:4) == 'grey' .OR. &
     &                          ARGS(J2+1)(1:4) == 'GREY' .OR. &
     &                          ARGS(J2+1)(1:2) == 'bw'   .OR. &
     &                          ARGS(J2+1)(1:2) == 'BW'        ) THEN
                        ICLR = 0
                      ELSE
                        CALL CHIN ( ARGS(J2+1), ICLR )
                        IF ( ICLR < 0 .OR. ICLR > 128 ) ICLR = 0
                   END IF
                 ELSE IF ( ARGS(J2)(1:I_LEN(ARGS(J2))) == '-size' ) THEN
                   IF ( ILEN(ARGS(J2+1)) == 0 ) THEN
                        CALL ERR_LOG ( 1503, -2, 'FITS_TO_RADPLOT', 'No value '// &
     &                      'of qualifier -size was supplied' )
                        CALL EXIT ( 0 )
                   END IF
                   CALL CHIN ( ARGS(J2+1), ISIZE )
                   IF ( ISIZE < 0 .OR. ISIZE > 5 ) ISIZE = 3
                 ELSE IF ( ARGS(J2)(1:I_LEN(ARGS(J2))) == '-gap' ) THEN
                   IF ( ILEN(ARGS(J2+1)) == 0 ) THEN
                        CALL ERR_LOG ( 1504, -2, 'FITS_TO_RADPLOT', &
     &                      'No value of qualifier -gap was supplied' )
                        CALL EXIT ( 0 )
                   END IF
                   IF ( INDEX ( ARGS(J2+1), '.' ) .LE. 0 ) THEN
                        ARGS(J2+1) = ARGS(J2+1)(1:I_LEN(ARGS(J2+1)))//'.0'
                   END IF
                   READ ( UNIT=ARGS(J2+1), FMT='(F10.5)' ) GAP_SCAN
                 ELSE IF ( ARGS(J2)(1:I_LEN(ARGS(J2))) == '-wei' ) THEN
                   IF ( ILEN(ARGS(J2+1)) == 0 ) THEN
                        CALL ERR_LOG ( 1505, -2, 'FITS_TO_RADPLOT', &
     &                      'No value of qualifier -gap was supplied' )
                        CALL EXIT ( 0 )
                   END IF
                   IF ( ARGS(J2+1)(1:1) == 'T' .OR. ARGS(J2+1)(1:1) == 't' ) THEN
                        FL_WEI_USE = .TRUE.
                      ELSE IF ( ARGS(J2+1)(1:1) == 'F' .OR. ARGS(J2+1)(1:1) == 'f' ) THEN
                        FL_WEI_USE = .FALSE.
                      ELSE
                        CALL ERR_LOG ( 1506, -2, 'FITS_TO_RADPLOT', &
     &                      'Wrong value of qualifier -wei was supplied: '// &
     &                       ARGS(J2+1)(1:I_LEN(ARGS(J2+1)))//' only T or F '// &
     &                      'are allowed' )
                        CALL EXIT ( 0 )
                   END IF
                 ELSE IF ( ARGS(J2)(1:4) == '-cut' ) THEN
                   IF ( ILEN(ARGS(J2+1)) == 0 ) THEN
                        CALL ERR_LOG ( 1507, -2, 'FITS_TO_RADPLOT', &
     &                      'No value of qualifier -cutoff_err was supplied' )
                        CALL EXIT ( 0 )
                   END IF
                   IF ( INDEX ( ARGS(J2+1), '.' ) .LE. 0 ) THEN
                        ARGS(J2+1) = ARGS(J2+1)(1:I_LEN(ARGS(J2+1)))//'.0'
                   END IF
                   READ ( UNIT=ARGS(J2+1), FMT='(F10.5)' ) CUTOFF_NERR
                 ELSE IF ( ARGS(J2)(1:5) == '-auto' ) THEN
                   FL_AUTO = .TRUE.
              END IF
 420       CONTINUE
           FINAM = ARGS(IARGC())
      END IF
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
! --- Get visibility data
!
      IUER = -1
      CALL GET_FITS_VIS ( FINAM, VIS, IUER )
      IF ( IUER .NE. 0 ) THEN
           CALL ERR_LOG ( 1508, -2, 'FITS_TO_RADPLOT', 'Failure in an '// &
     &         'attempt to read the fits file with visibility data '// &
     &          FINAM )
           CALL EXIT ( 1 )
      END IF
!
! --- Generate the rad plot
!
      IUER = -1
      CALL GEN_RADPLOT ( VIS, IDEV, ICLR, ISIZE, GAP_SCAN, FL_WEI_USE, &
     &                   FL_AUTO, CUTOFF_NERR, FILOUT, IUER )
      IF ( IUER .NE. 0 ) THEN
           CALL ERR_LOG ( 1509, -2, 'FITS_TO_RADPLOT', 'Failure in an '// &
     &         'attempt to generate the output rad-plot' )
           CALL EXIT ( 1 )
      END IF
!
      CALL EXIT ( 0 )
      END  SUBROUTINE  FITS_TO_RADPLOT  !#!#
