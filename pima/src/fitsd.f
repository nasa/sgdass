       PROGRAM    FITSD_MAIN
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
       CALL FITSD()
       END  PROGRAM  FITSD_MAIN
!
! ------------------------------------------------------------------------
!
       SUBROUTINE FITSD()
! ************************************************************************
! *                                                                      *
! *   Program FITSD
! *                                                                      *
! *  ### 13-SEP-2006    FITSD      v2.1 (c)  L. Petrov  06-APR-2014 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INTEGER*4  MF
      PARAMETER  ( MF = 32*1024 )
      TYPE  EXP__TYPE
            CHARACTER  SESS_NAME*16
            CHARACTER  SESS_BEG*22
            CHARACTER  SESS_END*22
            CHARACTER  FINAM*128
            INTEGER*4  MJD_BEG
            INTEGER*4  MJD_END
            REAL*8     TAI_BEG
            REAL*8     TAI_END
      END   TYPE EXP__TYPE
      TYPE ( EXP__TYPE ) :: EXP(MF)
      CHARACTER  DIR_NAME*128, FINAM*128, SESS_NAME*16, SESS_BEG*22, SESS_END*22
      INTEGER*4  ID, LEV, J1, J2, NF, IS, IUER
      INTEGER*8  DIR_DESC(16)
      INTEGER*2, EXTERNAL :: COMPAR_EXP
      INTEGER*4, EXTERNAL :: GET_FILE_FROM_DIR, ILEN, I_LEN
!
      IF ( IARGC() == 0 ) THEN
           WRITE ( 6, * ) 'Usage: fitsd {directory_name}'
           CALL EXIT ( 1 )
         ELSE
          CALL GETARG ( 1, DIR_NAME )
      END IF
!
      IF ( DIR_NAME(I_LEN(DIR_NAME):I_LEN(DIR_NAME)) .NE. '/' ) THEN
           DIR_NAME(I_LEN(DIR_NAME)+1:) = '/'
      END IF
      ID = ILEN(DIR_NAME)
!
      LEV = 0
      NF  = 0
      DO 410 J1=1,1024*1024
         IS = GET_FILE_FROM_DIR ( LEV, DIR_DESC, DIR_NAME, FINAM )
         IF ( IS .NE. 0 ) THEN
              CALL ERR_LOG ( 1, -1, 'FITSD', FINAM )
              CALL EXIT ( 1 )
         END IF
         IF ( LEV .EQ. 0 ) GOTO 810
!
         IUER = -1
         CALL FITS_INFO ( FINAM, SESS_NAME, SESS_BEG, SESS_END, IUER )
         IF ( IUER == 0 ) THEN
              NF = NF + 1
              EXP(NF)%SESS_NAME = SESS_NAME
              EXP(NF)%SESS_BEG  = SESS_BEG
              EXP(NF)%SESS_END  = SESS_END
              IUER = -1
              CALL DATE_TO_TIME ( EXP(NF)%SESS_BEG, EXP(NF)%MJD_BEG, &
     &                            EXP(NF)%TAI_BEG, IUER )
              IF ( IUER .NE. 0 ) THEN
                   CALL ERR_LOG ( 1281, -2, 'FITSD', 'Error in '// &
     &                 'parsing start date '//EXP(NF)%SESS_BEG// &
     &                 ' in input file '//FINAM )
                   CALL EXIT ( 1 )
              END IF
!
              IUER = -1
              CALL DATE_TO_TIME ( EXP(NF)%SESS_END, EXP(NF)%MJD_END, &
     &                            EXP(NF)%TAI_END, IUER )
              IF ( IUER .NE. 0 ) THEN
                   CALL ERR_LOG ( 1282, -2, 'FITSD', 'Error in '// &
     &                 'parsing end date '//EXP(NF)%SESS_END// &
     &                 ' in input file '//FINAM )
                   CALL EXIT ( 1 )
              END IF
              EXP(NF)%FINAM     = FINAM
         END IF
 410  CONTINUE
 810  CONTINUE
!
      CALL FOR_QSORT ( EXP, NF, SIZEOF(EXP(1)), COMPAR_EXP )
!
      WRITE ( 6, * ) ' NF=',NF
      DO 420 J2=1,NF
         WRITE ( 6, 110 ) J2, EXP(J2)%FINAM(1:I_LEN(EXP(J2)%FINAM)), &
     &                    EXP(J2)%SESS_NAME, EXP(J2)%SESS_BEG, EXP(J2)%SESS_END
 110     FORMAT ( I3, 2X, A, 2X, A, 2X, A, 2X, A )
 420  CONTINUE
      END  SUBROUTINE  FITSD  !#!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE FITS_INFO ( FINAM, SESS_NAME, SESS_BEG, SESS_END, IUER )
      IMPLICIT   NONE
      CHARACTER  FINAM*128, SESS_NAME*(*), SESS_BEG*(*), SESS_END*(*)
      INTEGER*4  MHDR, MKEY
      PARAMETER  ( MHDR =        512 )
      PARAMETER  ( MKEY =    32*1024 )
      INTEGER*8  FPTR
      INTEGER*4  LHDR, LKEY(MHDR), IUER
      CHARACTER*80, POINTER :: KEYS(:,:)
      CHARACTER  STR*128, SESS_DAT*32, SESS_RDAT*32
      LOGICAL*4  FL_AIPS_UV
      REAL*4     ARR_R4(32768)
      INTEGER*4  LENTAB_BYTES, NUM_ENT, IKEY, L_ROW, MAX_KEY, ARR_LEN, &
     &           J1, J2, J3, IP, IND_MON, IER
      CHARACTER  MON(12)*3
      DATA       MON &
     &           /  &
     &              'JAN', 'FEB', 'MAR', 'APR', 'MAY', 'JUN', &
     &              'JUL', 'AUG', 'SEP', 'OCT', 'NOV', 'DEC'  &
     &           /
      INTEGER*4, EXTERNAL :: LTM_DIF, ILEN, I_LEN
      INTERFACE
         SUBROUTINE FFITS_GET_KEYP ( FPTR, MHDR, MKEY, MAX_KEY, LHDR, LKEY, &
     &                               KEYS, IUER )
           INTEGER*8  FPTR
           INTEGER*4  MHDR, MKEY, MAX_KEY, LHDR, LKEY(MHDR), IUER
           CHARACTER*80, POINTER :: KEYS(:,:)
         END SUBROUTINE FFITS_GET_KEYP
      END INTERFACE
!
      CALL CLRCH ( SESS_NAME )
      CALL CLRCH ( SESS_BEG  )
      CALL CLRCH ( SESS_END  )
!
      CALL ERR_PASS   ( IUER, IER )
      CALL FFITS_OPEN ( FINAM, FPTR, 'OLD', IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 1811, IUER, 'FITS_INFO', 'Failure to open fits '// &
     &                    FINAM )
           RETURN
      END IF
!
      CALL ERR_PASS   ( IUER, IER )
      CALL FFITS_GET_KEYP ( FPTR, MHDR, MKEY, MAX_KEY, LHDR, LKEY, KEYS, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 1812, IUER, 'FITS_INFO', 'Failure to read keys '// &
     &                   'from fits-file '//FINAM )
           RETURN
      END IF
!
      SESS_BEG = 'ZZ'
      SESS_END = '  '
      DO 410 J1=1,LHDR
         DO 420 J2=1,LKEY(J1)
            IF ( KEYS(J2,J1)(1:16) == 'HISTORY OBSCODE ' ) THEN
                 SESS_NAME = KEYS(J2,J1)(19:)
                 CALL TRAN ( 12, SESS_NAME, SESS_NAME )
            END IF
            IF ( KEYS(J2,J1)(1:17) == 'HISTORY FILESTART' ) THEN
                 IND_MON = LTM_DIF ( 1, 12, MON, KEYS(J2,J1)(25:27) )
                 IF ( IND_MON < 1 ) THEN
                      CALL ERR_LOG ( 1813, IUER, 'FITS_INFO', 'Wrong month '// &
     &                    'in the field '//KEYS(J2,J1)(1:48) )
                      RETURN
                 END IF
                 CALL INCH ( IND_MON, STR(1:2) )
                 CALL CHASHR ( STR(1:2) )
                 CALL BLANK_TO_ZERO ( STR(1:2) )
                 SESS_DAT = KEYS(J2,J1)(21:24)//'_'// &
     &                      STR(1:2)//'_'//KEYS(J2,J1)(28:29)//'_'// &
     &                      KEYS(J2,J1)(31:32)//':'// &
     &                      KEYS(J2,J1)(34:35)//':'// &
     &                      KEYS(J2,J1)(37:41)
                 CALL BLANK_TO_ZERO ( SESS_DAT )
                 IF ( SESS_DAT < SESS_BEG ) SESS_BEG = SESS_DAT
            END IF
            IF ( KEYS(J2,J1)(1:17) == 'HISTORY FILESTOP ' ) THEN
                 IND_MON = LTM_DIF ( 1, 12, MON, KEYS(J2,J1)(25:27) )
                 IF ( IND_MON < 1 ) THEN
                      CALL ERR_LOG ( 1814, IUER, 'FITS_INFO', 'Wrong month '// &
     &                    'in the field '//KEYS(J2,J1)(1:48) )
                      RETURN
                 END IF
                 CALL INCH ( IND_MON, STR(1:2) )
                 CALL CHASHR ( STR(1:2) )
                 CALL BLANK_TO_ZERO ( STR(1:2) )
                 SESS_DAT = KEYS(J2,J1)(21:24)//'_'// &
     &                      STR(1:2)//'_'//KEYS(J2,J1)(28:29)//'_'// &
     &                      KEYS(J2,J1)(31:32)//':'// &
     &                      KEYS(J2,J1)(34:35)//':'// &
     &                      KEYS(J2,J1)(37:41)
                 CALL BLANK_TO_ZERO ( SESS_DAT )
                 IF ( SESS_DAT > SESS_END ) SESS_END = SESS_DAT
            END IF
            IF ( KEYS(J2,J1)(1:9) == 'RDATE   =' ) THEN
                 CALL CLRCH ( SESS_RDAT )
                 SESS_RDAT = KEYS(J2,J1)(12:15)//'_'//KEYS(J2,J1)(17:18)//'_'//KEYS(J2,J1)(20:21)// &
     &                       '_00:00:00.0'
            END IF
            IF ( KEYS(J2,J1)(1:9) == 'OBSCODE =' ) THEN
                 SESS_NAME = KEYS(J2,J1)(12:)
                 IP = INDEX ( SESS_NAME, "'" ) 
                 IF ( IP > 0 ) CALL CLRCH ( SESS_NAME(IP:) )
                 CALL TRAN ( 12, SESS_NAME, SESS_NAME )
            END IF
 420     CONTINUE
 410  CONTINUE
      IF ( SESS_BEG .EQ. 'ZZ' ) THEN
           SESS_BEG = SESS_RDAT
           SESS_END = SESS_RDAT
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL FFITS_CLOSE  ( FPTR, IER )
      IF ( IER .NE.0 ) THEN
           CALL ERR_LOG ( 1815, IUER, 'FITS_INFO', 'Failure to close '// &
     &         'fits-file '//FINAM )
           RETURN
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  FITS_INFO  !#!#
!
! ------------------------------------------------------------------------
!
      FUNCTION   COMPAR_EXP ( EXP1, EXP2 )
      INTEGER*2  COMPAR_EXP
      TYPE  EXP__TYPE
            CHARACTER  SESS_NAME*16
            CHARACTER  SESS_BEG*22
            CHARACTER  SESS_END*22
            CHARACTER  FINAM*128
            INTEGER*4  MJD_BEG
            INTEGER*4  MJD_END
            REAL*8     TAI_BEG
            REAL*8     TAI_END
      END   TYPE EXP__TYPE
      TYPE ( EXP__TYPE ) :: EXP1, EXP2
!
      IF ( EXP1%MJD_BEG > EXP2%MJD_BEG ) THEN
           COMPAR_EXP =  1
         ELSE IF ( EXP1%MJD_BEG < EXP2%MJD_BEG ) THEN
           COMPAR_EXP = -1
         ELSE
           IF ( EXP1%TAI_BEG > EXP2%TAI_BEG ) THEN
                COMPAR_EXP =  1
              ELSE IF ( EXP1%TAI_BEG < EXP2%TAI_BEG ) THEN
                COMPAR_EXP = -1
              ELSE
                COMPAR_EXP =  0
         END IF
      END IF
      RETURN
      END  FUNCTION   COMPAR_EXP  !#!#
