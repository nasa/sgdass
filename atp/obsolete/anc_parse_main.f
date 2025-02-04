#include <mk5_preprocessor_directives.inc>
      PROGRAM    ANC_PARSE_MAIN
! ************************************************************************
! *                                                                      *
! *   Program ANC_PARSE_MAIN
! *                                                                      *
! *  ### 09-MAY-2021               v1.0 (c)  L. Petrov  14-MAY-2021 ###  *
! *                                                                      *
! ************************************************************************
      INCLUDE   'anc.i'
      INCLUDE   'ners.i'
      INCLUDE   'ners_local.i'
      TYPE     ( ANC__TYP   ) :: ANC      
      TYPE     ( NERS__TYPE ) :: NERS
      LOGICAL*1  LEX
      CHARACTER  FILIN*128, NERS_CONFIG*128
      INTEGER*4  IUER
!
      FILIN = '/tmp/gs0011.anc'
!
! --- Get NERS_CONFIG file
! --- First, check environment variable NERS_CONFIG
!
      CALL GETENVAR ( 'NERS_CONFIG', NERS_CONFIG )
      IF ( ILEN(NERS_CONFIG) == 0 ) THEN
!
! -------- Second, check $HOME/.ners_config file
!
           CALL GETENVAR ( 'HOME', NERS_CONFIG )
           NERS_CONFIG = TRIM(NERS_CONFIG)//'/.ners_config'
           INQUIRE ( FILE=NERS_CONFIG, EXIST=LEX )
           IF ( .NOT. LEX ) THEN
!
! ------------- Third, check for the system-wide ners configuration file 
!
                NERS_CONFIG = NERS__CONFIG
           END IF
      END IF
!
! --- Innitialization of NERS structures, reading andparsing NERS configuration file
!
      IUER = -1
      CALL NERS_INIT ( NERS_CONFIG, NERS, -1.0D0, -1.0D0, IUER )
      IF ( IUER .NE. 0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 4001, IUER, 'ANC_PARSE_MAIN', 'Error in initializing '// &
     &         'NERS data structure' )
           CALL EXIT ( 1 )
      END IF
!
      IUER = -1
      CALL ANC_PARSE ( FILIN, ANC, NERS, IUER )
      IF ( IUER .NE. 0 ) CALL EXIT ( 1 )
!
      CALL ANC_TSYS_FILTER ( ANC, IUER )
!
      END  PROGRAM    ANC_PARSE_MAIN   !#!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE ANC_PARSE ( FILIN, ANC, NERS, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine ANC_PARSE 
! *                                                                      *
! *  ### 07-MAY-2021  ANC_PARSE   v1.0 (c)  L. Petrov  07-MAY-2021 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'anc.i'
      INCLUDE   'ners.i'
      CHARACTER  FILIN*128
      TYPE     ( ANC__TYP   ) :: ANC      
      TYPE     ( NERS__TYPE ) :: NERS
      CHARACTER, ALLOCATABLE :: BUF(:)*(ANC__MSTR)
      INTEGER*4  IUER
      CHARACTER  STR*128
      INTEGER*4  MIND
      PARAMETER  ( MIND = 32 )
      INTEGER*4  NBUF, J1, J2, J3, J4, LIND, IND(2,MIND), IND_DOO, IND_MET, &
     &           K_TPS, K_TSYS, IND_TPS, IER
      INTEGER*4  MJD_DOO, MJD_MET, MJD_TSYS, MJD
      REAL*8     TAI_DOO, TAI_MET, TAI_TSYS, TAI, UTC, UTC_MTAI
      INTEGER*4, EXTERNAL :: LTM_DIF
!
      ALLOCATE ( BUF(ANC__MBUF), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( INT8(ANC__MBUF)*INT8(ANC__MSTR), STR )
           CALL ERR_LOG ( 6821, IUER, 'ANC_PARSE', 'Error in an attempt '// &
     &         'to allocate '//TRIM(STR)//' bytes of dynamic memory' )
           RETURN 
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL ANC_READ ( FILIN, ANC__MBUF, NBUF, BUF, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 6822, IUER, 'ANC_PARSE', 'Error in an attempt '// &
     &         'to read file witn antenna calibration format '//FILIN )
           DEALLOCATE ( BUF )
           RETURN 
      END IF
!
      IF ( BUF(1)(1:24) .NE. '# ANTCAL Format  Version' ) THEN
           CALL ERR_LOG ( 6823, IUER, 'ANC_PARSE', 'Error in an attempt '// &
     &         'to read file witn antenna calibration format '//TRIM(FILIN)// &
     &         ' -- the first line does not contain magic' )
           DEALLOCATE ( BUF )
           RETURN 
      END IF
!
      MJD_DOO  = -1
      MJD_MET  = -1
      MJD_TSYS = -1
      K_TPS    =  0
      K_TSYS   =  0
!
      ANC%ANTCAL_FMT = BUF(1)
      DO 410 J1=2,NBUF
         CALL CLRCH (     STR )
         CALL INCH  ( J1, STR )
         IF ( BUF(J1)(1:1) == '#' ) GOTO 410
         CALL EXWORD ( BUF(J1), MIND, LIND, IND, CHAR(0)//CHAR(32)//CHAR(9), IER )
         IF ( LIND < 2 ) GOTO 410
         IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'STATION:' ) THEN
              ANC%STA_NAM  = BUF(J1)(IND(1,2):IND(2,2))
            ELSE IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'EXP_CODE:' ) THEN
              ANC%EXP_CODE = BUF(J1)(IND(1,2):IND(2,2))
            ELSE IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'UTC_MTAI:' ) THEN
              READ ( UNIT=BUF(J1)(IND(1,2):IND(2,2)), FMT='(F10.5)' ) ANC%UTC_MTAI
            ELSE IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'FILLERS:' ) THEN
              IF ( LIND < 4 ) THEN
                   CALL ERR_LOG ( 6824, IUER, 'ANC_PARSE', 'Too few words in '// &
     &                 'line '//TRIM(STR)//' of the antenna calibration file '//FILIN )
                   DEALLOCATE ( BUF )
                   RETURN
              END IF
              READ ( UNIT=BUF(J1)(IND(1,3):IND(2,2)), FMT='(F10.5)' ) ANC%FILLER_R8
              READ ( UNIT=BUF(J1)(IND(1,3):IND(2,3)), FMT='(I6)'    ) ANC%FILLER_I4
              ANC%FILLER_CH = BUF(J1)(IND(1,4):IND(2,4)) 
            ELSE IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'NUM_PROVENANCE:' ) THEN
              READ ( UNIT=BUF(J1)(IND(1,2):IND(2,2)), FMT='(I6)' ) ANC%NUM_PRV
            ELSE IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'NUM_DATA_ON:' ) THEN
              READ ( UNIT=BUF(J1)(IND(1,2):IND(2,2)), FMT='(I6)' ) ANC%NUM_DOO
              ALLOCATE ( ANC%DOO(ANC%NUM_DOO), STAT=IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 6825, IUER, 'ANC_PARSE', 'Error in allocation '// &
     &                 'memory for DATA_ON while pocessing antenna calibration '// &
     &                 'file '//FILIN )
                   DEALLOCATE ( BUF )
                   RETURN
              END IF
            ELSE IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'NUM_METEO:' ) THEN
              READ ( UNIT=BUF(J1)(IND(1,2):IND(2,2)), FMT='(I6)' ) ANC%NUM_MET
              ALLOCATE ( ANC%MET(ANC%NUM_MET), STAT=IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 6826, IUER, 'ANC_PARSE', 'Error in allocation '// &
     &                 'memory for METEO while pocessing antenna calibration '// &
     &                 'file '//FILIN )
                   DEALLOCATE ( BUF )
                   RETURN
              END IF
            ELSE IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'NUM_TP_SENSOR:' ) THEN
              READ ( UNIT=BUF(J1)(IND(1,2):IND(2,2)), FMT='(I6)' ) ANC%NUM_TPS
              ALLOCATE ( ANC%TPS(ANC%NUM_TPS), STAT=IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 6827, IUER, 'ANC_PARSE', 'Error in allocation '// &
     &                 'memory for METEO while pocessing antenna calibration '// &
     &                 'file '//FILIN )
                   DEALLOCATE ( BUF )
                   RETURN
              END IF
            ELSE IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'NUM_TSYS:' ) THEN
              READ ( UNIT=BUF(J1)(IND(1,2):IND(2,2)), FMT='(I6)' ) ANC%NUM_TSYS
              ALLOCATE ( ANC%TSYS(ANC%NUM_TSYS), STAT=IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 6828, IUER, 'ANC_PARSE', 'Error in allocation '// &
     &                 'memory for TSYS while pocessing antenna calibration '// &
     &                 'file '//FILIN )
                   DEALLOCATE ( BUF )
                   RETURN
              END IF
              DO 420 J2=1,ANC%NUM_TSYS
                 ALLOCATE ( ANC%TSYS(J2)%TSYS(ANC%NUM_TPS), STAT=IER )
                 IF ( IER .NE. 0 ) THEN
                      CALL ERR_LOG ( 6829, IUER, 'ANC_PARSE', 'Error in allocation '// &
     &                    'memory for TSYS while pocessing antenna calibration '// &
     &                    'file '//FILIN )
                      DEALLOCATE ( BUF )
                      RETURN
                 END IF
                 ANC%TSYS(J2)%TSYS = ANC%FILLER_R8
 420          CONTINUE 
            ELSE IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'NUM_TPI:' ) THEN
              READ ( UNIT=BUF(J1)(IND(1,2):IND(2,2)), FMT='(I6)' ) ANC%NUM_TPI
            ELSE IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'NUM_PC_SENSORS:' ) THEN
              READ ( UNIT=BUF(J1)(IND(1,2):IND(2,2)), FMT='(I6)' ) ANC%NUM_PCS
            ELSE IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'NUM_PCAL:' ) THEN
              READ ( UNIT=BUF(J1)(IND(1,2):IND(2,2)), FMT='(I6)' ) ANC%NUM_PCA
            ELSE IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'NUM_FMT2GPS_TIMER:' ) THEN
              READ ( UNIT=BUF(J1)(IND(1,2):IND(2,2)), FMT='(I6)' ) ANC%NUM_FMS
            ELSE IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'NUM_FMTGPS:' ) THEN
              READ ( UNIT=BUF(J1)(IND(1,2):IND(2,2)), FMT='(I6)' ) ANC%NUM_FMG
            ELSE IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'DATA_ON:' ) THEN
              IF ( LIND < 6 ) THEN
                   CALL ERR_LOG ( 6830, IUER, 'ANC_PARSE', 'Too few words in '// &
     &                 'line '//TRIM(STR)//' of the antenna calibration file '//FILIN )
                   DEALLOCATE ( BUF )
                   RETURN
              END IF
              IF ( .NOT. ASSOCIATED ( ANC%DOO ) ) THEN
                   CALL ERR_LOG ( 6831, IUER, 'ANC_PARSE', 'Malformed antenna '// &
     &                 'calibration file '//TRIM(FILIN)//' -- DATA_ON: preceeds '// &
     &                 'NUM_DATA_ON' )
                   DEALLOCATE ( BUF )
                   RETURN
              END IF
              CALL CHIN ( BUF(J1)(IND(1,2):IND(2,2)), IND_DOO )
!
              CALL ERR_PASS ( IUER, IER )
              CALL DATE_TO_TIME ( BUF(J1)(IND(1,3):IND(2,3)), MJD, UTC, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 6832, IUER, 'ANC_PARSE', 'Error in parsing '// &
     &                 'date '//BUF(J1)(IND(1,3):IND(2,3))//' in line '//TRIM(STR) )
                   DEALLOCATE ( BUF )
                   RETURN
              END IF
              IF ( MJD_DOO < 0 ) THEN
                   CALL ERR_PASS ( IUER, IER )
                   CALL NERS_GET_UTCMTAI ( NERS, (MJD - J2000__MJD)*86400.0D0 + UTC, &
     &                                     UTC_MTAI, IER )
                   IF ( IER .NE. 0 ) THEN
                        CALL ERR_LOG ( 6833, IUER, 'ANC_PARSE', 'Error in '// &
     &                      'getting UTC_MTAI' )
                        DEALLOCATE ( BUF )
                        RETURN
                   END IF
!
                   MJD_DOO = MJD
                   TAI_DOO = UTC - UTC_MTAI
              END IF
              TAI = UTC - UTC_MTAI
              ANC%DOO(IND_DOO)%TIM(1) = (MJD - MJD_DOO)*86400.D0 + (TAI - TAI_DOO)
!
              CALL ERR_PASS ( IUER, IER )
              CALL DATE_TO_TIME ( BUF(J1)(IND(1,4):IND(2,4)), MJD, UTC, IER )
              IF ( LIND < 6 ) THEN
                   CALL ERR_LOG ( 6834, IUER, 'ANC_PARSE', 'Error in parsing '// &
     &                 'date '//BUF(J1)(IND(1,3):IND(2,3))//' in line '//TRIM(STR) )
                   DEALLOCATE ( BUF )
                   RETURN
              END IF
!
              ANC%DOO(IND_DOO)%TIM(2) = (MJD - MJD_DOO)*86400.D0 + (TAI - TAI_DOO)
              ANC%DOO(IND_DOO)%SOU_NAM = BUF(J1)(IND(1,5):IND(2,5)) 
              ANC%DOO(IND_DOO)%SCA_NAM = BUF(J1)(IND(1,6):IND(2,6)) 
            ELSE IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'METEO:' ) THEN
              IF ( LIND < 8 ) THEN
                   CALL ERR_LOG ( 6835, IUER, 'ANC_PARSE', 'Too few words in '// &
     &                 'line '//TRIM(STR)//' of the antenna calibration file '//FILIN )
                   DEALLOCATE ( BUF )
                   RETURN
              END IF
              IF ( .NOT. ASSOCIATED ( ANC%MET ) ) THEN
                   CALL ERR_LOG ( 6836, IUER, 'ANC_PARSE', 'Malformed antenna '// &
     &                 'calibration file '//TRIM(FILIN)//' -- METEO: preceeds '// &
     &                 'NUM_METEO' )
                   DEALLOCATE ( BUF )
                   RETURN
              END IF
              CALL CHIN ( BUF(J1)(IND(1,2):IND(2,2)), IND_MET )
              IF ( IND_MET > ANC%NUM_MET ) THEN
                   CALL ERR_LOG ( 6837, IUER, 'ANC_PARSE', 'Malformed antenna '// &
     &                 'calibration file '//TRIM(FILIN)//' -- too many meteo '// &
     &                 'lines' )
                   DEALLOCATE ( BUF )
                   RETURN
              END IF
!
              CALL ERR_PASS ( IUER, IER )
              CALL DATE_TO_TIME ( BUF(J1)(IND(1,3):IND(2,3)), MJD, UTC, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 6838, IUER, 'ANC_PARSE', 'Error in parsing '// &
     &                 'date '//BUF(J1)(IND(1,3):IND(2,3))//' in line '//TRIM(STR) )
                   DEALLOCATE ( BUF )
                   RETURN
              END IF
              IF ( MJD_MET < 0 ) THEN
                   CALL ERR_PASS ( IUER, IER )
                   CALL NERS_GET_UTCMTAI ( NERS, (MJD - J2000__MJD)*86400.0D0 + UTC, &
     &                                     UTC_MTAI, IER )
                   IF ( IER .NE. 0 ) THEN
                        CALL ERR_LOG ( 6839, IUER, 'ANC_PARSE', 'Error in '// &
     &                      'getting UTC_MTAI' )
                        DEALLOCATE ( BUF )
                        RETURN
                   END IF
!
                   MJD_MET = MJD
                   TAI_MET = UTC - UTC_MTAI
              END IF
              TAI = UTC - UTC_MTAI
              ANC%MET(IND_MET)%TIM = (MJD - MJD_MET)*86400.D0 + (TAI - TAI_MET)
!
              READ ( UNIT=BUF(J1)(IND(1,4):IND(2,4)), FMT='(F10.5)' ) ANC%MET(IND_MET)%TEMP
              READ ( UNIT=BUF(J1)(IND(1,5):IND(2,5)), FMT='(F10.5)' ) ANC%MET(IND_MET)%PRES
              READ ( UNIT=BUF(J1)(IND(1,6):IND(2,6)), FMT='(F10.5)' ) ANC%MET(IND_MET)%HUMID
              ANC%MET(IND_MET)%SOU_NAM = BUF(J1)(IND(1,7):IND(2,7)) 
              ANC%MET(IND_MET)%SCA_NAM = BUF(J1)(IND(1,8):IND(2,8)) 
            ELSE IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'TP_SENSOR:' ) THEN
              IF ( LIND < 8 ) THEN
                   CALL ERR_LOG ( 6840, IUER, 'ANC_PARSE', 'Too few words in '// &
     &                 'line '//TRIM(STR)//' of the antenna calibration file '//FILIN )
                   DEALLOCATE ( BUF )
                   RETURN
              END IF
              K_TPS = K_TPS + 1
              IF ( K_TPS > ANC%NUM_TPS ) THEN
                   CALL ERR_LOG ( 6841, IUER, 'ANC_PARSE', 'Malformed antenna '// &
     &                 'calibration file '//TRIM(FILIN)//' -- too many TP sensors '// &
     &                 'lines' )
                   DEALLOCATE ( BUF )
                   RETURN
              END IF
              ANC%TPS(K_TPS)%TAG = BUF(J1)(IND(1,2):IND(2,2))
              ANC%TPS_TAG(K_TPS) = BUF(J1)(IND(1,2):IND(2,2))
              READ ( UNIT=BUF(J1)(IND(1,3):IND(2,3)), FMT='(F20.5)' ) ANC%TPS(K_TPS)%IF_FRQ
              READ ( UNIT=BUF(J1)(IND(1,4):IND(2,4)), FMT='(F20.5)' ) ANC%TPS(K_TPS)%LO_FRQ
              READ ( UNIT=BUF(J1)(IND(1,5):IND(2,5)), FMT='(F20.5)' ) ANC%TPS(K_TPS)%SKY_FRQ
              READ ( UNIT=BUF(J1)(IND(1,6):IND(2,6)), FMT='(F20.5)' ) ANC%TPS(K_TPS)%BDW
!
              IF ( BUF(J1)(IND(1,7):IND(2,7)) == 'R' ) THEN
                   ANC%TPS(K_TPS)%POL = ANC__R_POL
                 ELSE IF ( BUF(J1)(IND(1,7):IND(2,7)) == 'L' ) THEN
                   ANC%TPS(K_TPS)%POL = ANC__L_POL
                 ELSE IF ( BUF(J1)(IND(1,7):IND(2,7)) == 'H' ) THEN
                   ANC%TPS(K_TPS)%POL = ANC__H_POL
                 ELSE IF ( BUF(J1)(IND(1,7):IND(2,7)) == 'V' ) THEN
                   ANC%TPS(K_TPS)%POL = ANC__V_POL
                 ELSE IF ( BUF(J1)(IND(1,7):IND(2,7)) == 'X' ) THEN
                   ANC%TPS(K_TPS)%POL = ANC__X_POL
                 ELSE IF ( BUF(J1)(IND(1,7):IND(2,7)) == 'Y' ) THEN
                   ANC%TPS(K_TPS)%POL = ANC__Y_POL
                 ELSE 
                   CALL ERR_LOG ( 6842, IUER, 'ANC_PARSE', 'Unsupported polarization '// &
     &                 'code found in line '//TRIM(STR)// &
     &                 ' of the antenna calibration file '//FILIN )
                   DEALLOCATE ( BUF )
                   RETURN
              END IF
!
              ANC%TPS(K_TPS)%ID = BUF(J1)(IND(1,8):IND(2,8)) 
              READ ( UNIT=BUF(J1)(IND(1,9):IND(2,9)), FMT='(I6)' ) ANC%TPS(K_TPS)%IF_IND
              IF ( BUF(J1)(IND(1,10):IND(2,10)) == 'USB' ) THEN
                   ANC%TPS(K_TPS)%SUB = ANC__USB
                 ELSE IF ( BUF(J1)(IND(1,10):IND(2,10)) == 'LSB' ) THEN
                   ANC%TPS(K_TPS)%SUB = ANC__LSB
                 ELSE 
                   CALL ERR_LOG ( 6843, IUER, 'ANC_PARSE', 'Unsupported subband '// &
     &                 'code found in line '//TRIM(STR)// &
     &                 ' of the antenna calibration file '//FILIN )
                   DEALLOCATE ( BUF )
                   RETURN
              END IF
            ELSE IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'TSYS:' ) THEN
              IF ( LIND < 9 ) THEN
                   CALL ERR_LOG ( 6844, IUER, 'ANC_PARSE', 'Too few words in '// &
     &                 'line '//TRIM(STR)//' of the antenna calibration file '//FILIN )
                   DEALLOCATE ( BUF )
                   RETURN
              END IF
              CALL ERR_PASS ( IUER, IER )
              CALL DATE_TO_TIME ( BUF(J1)(IND(1,3):IND(2,3)), MJD, UTC, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 6845, IUER, 'ANC_PARSE', 'Error in parsing '// &
     &                 'date '//BUF(J1)(IND(1,3):IND(2,3))//' in line '//TRIM(STR) )
                   DEALLOCATE ( BUF )
                   RETURN
              END IF
              IF ( MJD_TSYS < 0 ) THEN
                   CALL ERR_PASS ( IUER, IER )
                   CALL NERS_GET_UTCMTAI ( NERS, (MJD - J2000__MJD)*86400.0D0 + UTC, &
     &                                     UTC_MTAI, IER )
                   IF ( IER .NE. 0 ) THEN
                        CALL ERR_LOG ( 6846, IUER, 'ANC_PARSE', 'Error in '// &
     &                      'getting UTC_MTAI' )
                        DEALLOCATE ( BUF )
                        RETURN
                   END IF
!
                   MJD_TSYS = MJD
                   TAI_TSYS = UTC - UTC_MTAI
              END IF
              TAI = UTC - UTC_MTAI
!
              IF ( K_TSYS == 0 ) THEN
                   K_TSYS = 1
                 ELSE
                   IF ( DABS( (MJD - MJD_TSYS)*86400.D0 + (TAI - TAI_TSYS) - &
     &                         ANC%TSYS(K_TSYS)%TIM ) > 3*ANC__EPS_TIM ) THEN
                        K_TSYS = K_TSYS + 1
                   END IF
              END IF
              IF ( K_TSYS > ANC%NUM_TSYS ) THEN
                   CALL ERR_LOG ( 6847, IUER, 'ANC_PARSE', 'Malformed antenna '// &
     &                 'calibration file '//TRIM(FILIN)//' -- too many TSYS '// &
     &                 'lines lines' )
                   DEALLOCATE ( BUF )
                   RETURN
              END IF
!
              ANC%TSYS(K_TSYS)%TIM = (MJD - MJD_TSYS)*86400.D0 + (TAI - TAI_TSYS)
              READ ( UNIT=BUF(J1)(IND(1,2):IND(2,2)), FMT='(I6)' ) ANC%TSYS(K_TSYS)%IND_SCA
              IND_TPS = LTM_DIF ( 0, ANC%NUM_TPS, ANC%TPS_TAG,  BUF(J1)(IND(1,4):IND(2,4)) )
              IF ( IND_TPS < 1 ) THEN
                   CALL ERR_LOG ( 6848, IUER, 'ANC_PARSE', 'Error in parsing '// &
     &                 'sensr tag '//BUF(J1)(IND(1,4):IND(2,4))//' in line '//TRIM(STR) )
                   DEALLOCATE ( BUF )
                   RETURN
              END IF
              READ ( UNIT=BUF(J1)(IND(1,5):IND(2,5)), FMT='(F20.12)' ) ANC%TSYS(K_TSYS)%TSYS(IND_TPS)
              READ ( UNIT=BUF(J1)(IND(1,6):IND(2,6)), FMT='(F20.12)' ) ANC%TSYS(K_TSYS)%AZ
              READ ( UNIT=BUF(J1)(IND(1,7):IND(2,7)), FMT='(F20.12)' ) ANC%TSYS(K_TSYS)%EL
              ANC%TSYS(K_TSYS)%AZ = ANC%TSYS(K_TSYS)%AZ*DEG__TO__RAD
              ANC%TSYS(K_TSYS)%EL = ANC%TSYS(K_TSYS)%EL*DEG__TO__RAD
              ANC%TSYS(K_TSYS)%SOU_NAM = BUF(J1)(IND(1,8):IND(2,8)) 
              ANC%TSYS(K_TSYS)%SCA_NAM = BUF(J1)(IND(1,9):IND(2,9)) 
         END IF
 410  CONTINUE 
!
      DEALLOCATE ( BUF )
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE ANC_PARSE   !#!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE ANC_READ ( FILIN, MBUF, NBUF, BUF, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine ANC_READ 
! *                                                                      *
! *  ### 07-MAY-2021    ANC_READ   v1.0 (c)  L. Petrov  07-MAY-2021 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'anc.i'
      INTEGER*4  MBUF, NBUF, IUER 
      CHARACTER  FILIN*128, BUF(MBUF)*(ANC__MSTR)
      CHARACTER  FILTMP*128, STR*128, COM*256, SHM_DIR*7
      PARAMETER  ( SHM_DIR = '/dev/shm' )
      LOGICAL*1  FL_BZIP2
      INTEGER*4  IP, IS, IL, PID, NTHR, IER
      INTEGER*8  DIR_DESC
      INTEGER*8, EXTERNAL :: FUNC_OPENDIR
      INTEGER*4, EXTERNAL :: CLOSEDIR, GETPID, SYSTEM, ILEN, I_LEN, &
     &                       OMP_GET_THREAD_NUM
      LOGICAL*4, EXTERNAL :: OMP_IN_PARALLEL
!
      IL = ILEN(FILIN)
      IF ( IL < 4 ) IL = 4
      IF ( FILIN(IL-3:IL) == '.bz2' ) THEN
           PID = GETPID()
           CALL INCH ( PID, FILTMP(1:8) )
           CALL CHASHR    ( FILTMP(1:8) )
           CALL BLANK_TO_ZERO ( FILTMP(1:8) )
           IF ( OMP_IN_PARALLEL() ) THEN
                FILTMP(9:9) = '_'
                CALL INCH ( OMP_GET_THREAD_NUM(), FILTMP(10:13) )
                CALL CHASHR    ( FILTMP(10:13) )
                CALL BLANK_TO_ZERO ( FILTMP(10:13) )
           END IF 
!          
           DIR_DESC = FUNC_OPENDIR ( TRIM(SHM_DIR)//CHAR(0) )
           IF ( DIR_DESC .EQ. 0 ) THEN
                FILTMP = '/tmp'//'/'//FILTMP
              ELSE
                IP = CLOSEDIR ( %VAL(DIR_DESC) )
                FILTMP = TRIM(SHM_DIR)//'/'//FILTMP
           END IF
!
! -------- Honor environment variable OMP_NUM_THREADS.
! -------- We limit the number of threads for lbzip2
!
           CALL GETENVAR ( 'OMP_NUM_THREADS', STR )
           IF ( ILEN(STR) > 0 ) THEN
                CALL CHIN ( STR, NTHR )
                IF ( NTHR < 1 ) NTHR = 1
                CALL CLRCH ( STR ) 
                CALL INCH  ( NTHR, STR )
                STR = '-n '//STR
              ELSE
!
! ------------- ... or do not use any limit when the variable is not set up
!
                CALL CLRCH ( STR )
           END IF
!
           FL_BZIP2 = .TRUE.
           IF ( OMP_IN_PARALLEL() ) THEN
                COM = 'lbzip2 -n 1 -dfc '//FILIN(1:I_LEN(FILIN))//' > '//FILTMP
              ELSE 
                COM = 'lbzip2 '//STR(1:I_LEN(STR))//' -dsfc '//FILIN(1:I_LEN(FILIN))//' > '//FILTMP
           END IF
           IS = SYSTEM ( COM(1:I_LEN(COM))//CHAR(0) )
           IF ( IS .NE. 0 ) THEN
!
! ------------- lbzip2 may fail because of "Cannot allocate memory". As a desperate
! ------------- attempt we try once more with using bzip2 and with only one thread 
! ------------- and a small block size
!
                CALL UNLINK ( FILTMP(1:I_LEN(FILTMP))//CHAR(0) )
                COM = 'bzip2 -dfc '//FILIN(1:I_LEN(FILIN))//' > '//FILTMP
                IS = SYSTEM ( COM(1:I_LEN(COM))//CHAR(0) )
           END IF
           IF ( IS .NE. 0 ) THEN
                WRITE ( 6, * ) 'System: IS = ', IS
                CALL CLRCH  ( STR )
                CALL GERROR ( STR )
                CALL ERR_LOG ( 6850, IUER, 'ANC_READ', 'Failure to '// &
     &              'uncompress the input heb-file '// &
     &              FILIN(1:I_LEN(FILIN))//' using command '// &
     &              COM(1:I_LEN(COM))//' -- error: '//STR )
                IF ( FL_BZIP2 ) CALL UNLINK ( FILTMP(1:I_LEN(FILTMP))//CHAR(0) )
                RETURN 
           END IF
         ELSE
           FL_BZIP2 = .FALSE.
           FILTMP = FILIN
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL RD_TEXT  ( FILTMP, MBUF, BUF, NBUF, IER )
      IF ( FL_BZIP2 ) THEN
           CALL UNLINK ( FILTMP(1:I_LEN(FILTMP))//CHAR(0) )
      END IF           
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 6851, IUER, 'ANC_READ', 'Failure in reading '// &
     &         'antenna calibration file '//FILTMP )
           RETURN 
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  ANC_READ  !#!  
!
! ------------------------------------------------------------------------
!
      SUBROUTINE ANC_TSYS_FILTER ( ANC, IUER )
      IMPLICIT   NONE 
      INCLUDE   'anc.i'
      TYPE     ( ANC__TYP   ) :: ANC      
      CHARACTER  IUER
      CHARACTER  STR*128
      REAL*8     T8(ANC__MEPC), X8(ANC__MEPC)
      REAL*8     TSYS_MIN, FRQ_MIN
      REAL*8     MAT_ARR(2048,2,1024), FRQ_ARR(1024) ! %%%
      CHARACTER  BUF_TS(2048)*8, FILMAT*128
      PARAMETER  ( TSYS_MIN = 5.0D0 )
      PARAMETER  ( FRQ_MIN  = 1500.0D0 )
      INTEGER*4  J1, J2, J3, J4, KP, KF, LUN, IER
!
      MAT_ARR = 0.0D0
      FRQ_ARR = 0.0D0
      BUF_TS = '        '
      KF = 0
      DO 410 J1=1,ANC%NUM_TPS
         KP = 0
         DO 420 J2=1,ANC%NUM_TSYS
            IF ( ANC%TSYS(J2)%TSYS(J1) > TSYS_MIN ) THEN
                 KP = KP + 1
                 T8(KP) = ANC%TSYS(J2)%TIM
                 X8(KP) = ANC%TSYS(J2)%TSYS(J1)
            END IF
 420     CONTINUE 
         CALL CLRCH ( STR )
         WRITE ( UNIT=STR(1:8), FMT='(F8.2)' ) ANC%TPS(J1)%SKY_FRQ
!!         WRITE ( 6, * ) ANC%TPS(J1)%SKY_FRQ
         IF ( ANC%TPS(J1)%SKY_FRQ > FRQ_MIN .AND. KP > 2 ) THEN
              KF = KF + 1
              MAT_ARR(1:KP,1,KF) = T8(1:KP)
              MAT_ARR(1:KP,2,KF) = X8(1:KP)
              FRQ_ARR(KF) = ANC%TPS(J1)%SKY_FRQ
              BUF_TS(KF)  = ANC%TPS(J1)%TAG(1:6)//' '//ANC__POL(ANC%TPS(J1)%POL)
!
!              CALL DIAGI_SETDEF ( IER, 'DIAGI_UNIT', 'Time since experiment start in sec' )
!              CALL DIAGI_SETDEF ( IER, 'DIAGI_CTIT', 'Tsys '//ANC%TPS(J1)%TAG//' '// &
!     &                                                STR(1:8)//' MHz Pol: '//ANC__POL(ANC%TPS(J1)%POL) )
!!              if ( ANC%TPS(J1)%SKY_FRQ > 10000.0 ) CALL DIAGI_1 ( KP, T8, X8, IER )
!!              CALL DIAGI_1 ( KP, T8, X8, IER )
            ELSE
              WRITE ( 6, * ) 'Too view data points for '//ANC%TPS(J1)%TAG(1:6)//' '// &
     &                        ANC__POL(ANC%TPS(J1)%POL)
         END IF
 410  CONTINUE 
!
      FILMAT = '/t0/temp/ancmat.dat'
      LUN = 18
      IER = -1
      CALL BINF_OPEN ( FILMAT, 'UNKNOWN', LUN, IER )
      IER = -1
      CALL WRBIN_ARRAY ( LUN, 'I4', 1, KF, IER )
      IER = -1
      CALL WRBIN_ARRAY ( LUN, 'B1', 8*KF,  %VAL(LOC(BUF_TS)), IER )
      IER = -1
      CALL WRBIN_ARRAY ( LUN, 'R8', KF, FRQ_ARR, IER )
      IER = -1
      CALL WRBIN_ARRAY ( LUN, 'R8', 2048*2*KF, MAT_ARR, IER )
      IER = -1
      CALL BINF_CLOSE ( LUN, IER )
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  ANC_TSYS_FILTER  !#!  
