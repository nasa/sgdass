      SUBROUTINE SUR_SKED_CONF ( EXP_FIL, SUR, VTD, IVRB, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  SUR_SKED_CONF 
! *                                                                      *
! * ### 10-OCT-2005   SUR_SKED_CONF   v3.7 (c) L. Petrov 06-JUL-2023 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'sur_sked.i'
      INCLUDE   'vtd.i'
      INCLUDE   'ners_local.i'
      INCLUDE   'getpar.i'
      TYPE     ( SUR__TYPE ) :: SUR
      TYPE     ( VTD__TYPE ) :: VTD
      CHARACTER  EXP_FIL*128
      INTEGER*4  IVRB, IUER 
      LOGICAL*4  LEX
      INTEGER*4  M_BUF, MIND
      PARAMETER  ( M_BUF = 8192 )
      PARAMETER  ( MIND  = 32   )
      CHARACTER  STR*128, STR1*128, BUF(M_BUF)*4096, BUF_LS(M_LS)*128
      CHARACTER  SOURCE_FILE*128, STATION_LINE*4096, OBSERVED_SOURCE_FILE*128, &
     &           CALIB_SOURCE_FILE*128, SEC_SOURCE_FILE*128, &
     &           PAIR_SOURCE_FILE*128, POCAL_SOURCE_FILE*128, &
     &           PLANET_SOURCE_FILE*128, NERS_CONFIG_FILE*128
      CHARACTER  COBS_SOU(SUR__M_SOU)*8, COBS_CAL(SUR__M_SOU)*8, &
     &           COBS_PAI(SUR__M_SOU)*8, COBS_POC(SUR__M_SOU)*8, &
     &           COBS_PLA(SUR__M_SOU)*8
      TYPE     ( SOURCE_CAT__TYPE ) :: OBS_CAT(SUR__M_SOU), &
     &                                 OBS_CAL(SUR__M_SOU), &
     &                                 OBS_PAI(SUR__M_SOU), &
     &                                 OBS_OBS(SUR__M_SOU), &
     &                                 OBS_SOP(SUR__M_SOU), &
     &                                 OBS_POC(SUR__M_SOU), &
     &                                 OBS_PLA(SUR__M_SOU)
      CHARACTER  J2000_NAME*10, B1950_NAME*8, ALPHA_STR*12, DELTA_STR*12
      REAL*8     TAI_OBS, ALPHA, DELTA, S_VEC(3), DIST_COS
      REAL*8     DUR_SEC, DUR_GAP_SEC, DUR_EFF_SEC, UTC_M_TAI, AZ, ELEV
      INTEGER*4  MJD_MID
      REAL*8     TAI_MID, COO_EAR(3), VEL_EAR(3), ACC_EAR(3), COO_SUN(3), &
     &           VEL_SUN(3), ACC_SUN(3), RD_SUN, SUN_DIST, SCHE_GAP(2,SUR__M_GAP), &
     &           SEG_INT_HR
      INTEGER*4  J1, J2, J3, J4, J5, J6, J7, J8, J9, J10, J11, J12, J13, &
     &           J14, J15, J16, J17, J18, J19, J20, J21, J22, &
     &           L_CAL, L_PAI, L_SOP, L_POC, L_PLA, &
     &           N_BUF, N_SUR, LIND, IND(2,MIND), INDW, INDE, INDG, LOBS_OBS, &
     &           LOBS_SOU, LOBS_SOU_SAVE, MODE, MJD_OBS, L_CHK, IDAY, &
     &           IND_SRC(2), IER
      INTEGER*4  L_MODE, IND_MODE(2,MIND), L_MDSC, IND_MDSC(2,MIND)
      LOGICAL*4  FL_FOUND
      INTEGER*4, EXTERNAL :: ILEN, I_LEN, LTM_DIF
      REAL*8,    EXTERNAL :: DP_VV_V
      CHARACTER, EXTERNAL :: GET_CDATE*19
!
! --- Check whether experimental file does exist
!
      INQUIRE ( FILE=EXP_FIL, EXIST=LEX )
      IF ( .NOT. LEX ) THEN
           CALL ERR_LOG ( 1611, IUER, 'SUR_SKED_CONF', 'Cannot find '// &
     &         'schedule file '//EXP_FIL )
           RETURN 
      END IF
!
! --- Read the configuration experiment file
!
      CALL ERR_PASS ( IUER, IER )
      CALL RD_TEXT ( EXP_FIL, M_BUF, BUF, N_BUF, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 1612, IUER, 'SUR_SKED_CONF', 'Error in reading '// &
     &        'experiment file '//EXP_FIL )
           RETURN 
      END IF
!
      IF ( BUF(1)(1:LEN(SUR__CNT_FORMAT)) == SUR__CNT_FORMAT ) THEN
           CONTINUE 
         ELSE 
           CALL CLRCH ( STR )
           CALL TRAN  ( 13, BUF(1), STR )
           CALL ERR_LOG ( 1613, IUER, 'SUR_SKED_CONF', 'Unrecognized '// &
     &         'format of the control file '//EXP_FIL(1:I_LEN(EXP_FIL))// &
     &         ' -- the first line is '//STR(1:I_LEN(STR))// &
     &         ' while '//SUR__CNT_FORMAT//' was expected' )
           RETURN 
      END IF
!
      N_SUR = 0
      DO 410 J1=1,N_BUF
         CALL EXWORD ( BUF(J1), MIND, LIND, IND, CHAR(0)//CHAR(32)//CHAR(9), -3 )
         IF ( BUF(J1)(1:1)  == '#' ) GOTO 410
         IF ( ILEN(BUF(J1)) ==  0  ) GOTO 410
         IF ( LIND          <   2  ) GOTO 410
!
         IF ( INDEX ( BUF(J1), 'EXPERIMENT_CODE:' ) == 1 ) THEN
              SUR%EXP_CODE  = BUF(J1)(IND(1,2):IND(2,2))
              N_SUR = N_SUR + 1
            ELSE IF ( INDEX ( BUF(J1), 'EXPERIMENT_DESCR:' ) == 1 ) THEN
              SUR%EXP_DESCR = BUF(J1)(IND(1,2):IND(2,LIND))
              N_SUR = N_SUR + 1
            ELSE IF ( INDEX ( BUF(J1), 'SCHEDULE_REVISION:' ) == 1 ) THEN
              SUR%SCHEDULE_REVISION = BUF(J1)(IND(1,2):IND(2,LIND))
              N_SUR = N_SUR + 1
            ELSE IF ( INDEX ( BUF(J1), 'PI_NAME:' ) == 1 ) THEN
              SUR%PI_NAME = BUF(J1)(IND(1,2):IND(2,LIND))
              N_SUR = N_SUR + 1
            ELSE IF ( INDEX ( BUF(J1), 'SCHEDULER_NAME:' ) == 1 ) THEN
              SUR%SCHEDULER_NAME = BUF(J1)(IND(1,2):IND(2,LIND))
              N_SUR = N_SUR + 1
            ELSE IF ( INDEX ( BUF(J1), 'SCHEDULER_EMAIL:' ) == 1 ) THEN
              SUR%SCHEDULER_EMAIL = BUF(J1)(IND(1,2):IND(2,LIND))
              N_SUR = N_SUR + 1
            ELSE IF ( INDEX ( BUF(J1), 'SCHEDULER_PHONE:' ) == 1 ) THEN
              SUR%SCHEDULER_PHONE = BUF(J1)(IND(1,2):IND(2,LIND))
              N_SUR = N_SUR + 1
            ELSE IF ( INDEX ( BUF(J1), 'OBSERVER_PHONE:' ) == 1 ) THEN
              SUR%OBSERVER_PHONE  = BUF(J1)(IND(1,2):IND(2,LIND))
              N_SUR = N_SUR + 1
            ELSE IF ( INDEX ( BUF(J1), 'CORR_SPECTRAL_RESOLUTION:' ) == 1 ) THEN
              READ ( UNIT=BUF(J1)(IND(1,2):IND(2,LIND)), FMT=*  ) SUR%CORR_SPECTRAL_RESOLUTION
              N_SUR = N_SUR + 1
            ELSE IF ( INDEX ( BUF(J1), 'CORR_TIME_RESOLUTION:' ) == 1 ) THEN
              READ ( UNIT=BUF(J1)(IND(1,2):IND(2,LIND)), FMT=*  ) SUR%CORR_TIME_RESOLUTION
              N_SUR = N_SUR + 1
            ELSE IF ( INDEX ( BUF(J1), 'ALGORITHM:' ) == 1 ) THEN
              SUR%ALGORITHM = BUF(J1)(IND(1,2):IND(2,2))
              IF ( SUR%ALGORITHM == 'FRINGE_SEARCH_01' ) THEN
                   CONTINUE 
                 ELSE IF ( SUR%ALGORITHM == 'FRINGE_SEARCH_02' ) THEN
                   CONTINUE
                 ELSE IF ( SUR%ALGORITHM == 'ASTROMET_01' ) THEN
                   CONTINUE 
                 ELSE IF ( SUR%ALGORITHM == 'ASTROMET_02' ) THEN
                   CONTINUE 
                 ELSE IF ( SUR%ALGORITHM == 'ASTROMET_03' ) THEN
                   CONTINUE 
                 ELSE IF ( SUR%ALGORITHM == 'ASTROMET_04' ) THEN
                   CONTINUE 
                 ELSE IF ( SUR%ALGORITHM == 'ASTROMET_05' ) THEN
                   CONTINUE 
                 ELSE IF ( SUR%ALGORITHM == 'ASTROMET_06' ) THEN
                   CONTINUE 
                 ELSE IF ( SUR%ALGORITHM == 'ASTROMET_07' ) THEN
                   CONTINUE 
                 ELSE IF ( SUR%ALGORITHM == 'ASTROMET_11' ) THEN
                   CONTINUE 
                 ELSE IF ( SUR%ALGORITHM == 'ASTROMET_12' ) THEN
                   CONTINUE 
                 ELSE IF ( SUR%ALGORITHM == 'ASTROMET_13' ) THEN
                   CONTINUE 
                 ELSE IF ( SUR%ALGORITHM == 'GEODETIC_01' ) THEN
                   CONTINUE 
                 ELSE IF ( SUR%ALGORITHM == 'GEODETIC_02' ) THEN
                   CONTINUE 
                 ELSE IF ( SUR%ALGORITHM == 'GEODETIC_03' ) THEN
                   CONTINUE 
                 ELSE IF ( SUR%ALGORITHM == 'GNSS_01' ) THEN
                   CONTINUE 
                 ELSE IF ( SUR%ALGORITHM == 'GNSS_02' ) THEN
                   CONTINUE 
                 ELSE IF ( SUR%ALGORITHM == 'SPACECRAFT_01' ) THEN
                   CONTINUE 
                 ELSE IF ( SUR%ALGORITHM == 'IMAGING_01' ) THEN
                   CONTINUE 
                 ELSE IF ( SUR%ALGORITHM == 'IMAGING_S1' ) THEN
                   CONTINUE 
                 ELSE
                   CALL ERR_LOG ( 1614, IUER, 'SUR_SKED_CONF', 'Error in '// &
     &                 'parsing experiment file '//EXP_FIL(1:I_LEN(EXP_FIL))// &
     &                 ' -- value '//BUF(J1)(IND(1,2):IND(2,2))// &
     &                 ' of the ALGORITHM keyword is not supported' )
                   RETURN 
              END IF
              N_SUR = N_SUR + 1
            ELSE IF ( INDEX ( BUF(J1), 'SOURCE_FILE:' ) == 1 ) THEN
              SOURCE_FILE = BUF(J1)(IND(1,2):IND(2,2))
              INQUIRE ( FILE=SOURCE_FILE, EXIST=LEX )
              IF ( .NOT. LEX ) THEN
                   CALL ERR_LOG ( 1615, IUER, 'SUR_SKED_CONF', 'Error in '// &
     &                 'parsing experiment file '//EXP_FIL(1:I_LEN(EXP_FIL))// &
     &                 ' -- SOURCE_FILE '//SOURCE_FILE(1:I_LEN(SOURCE_FILE))// &
     &                 ' was not found' )
                   RETURN 
              END IF
              N_SUR = N_SUR + 1
            ELSE IF ( INDEX ( BUF(J1), 'SECONDARY_SOURCE_FILE:' ) == 1 ) THEN
              SEC_SOURCE_FILE = BUF(J1)(IND(1,2):IND(2,2))
              INQUIRE ( FILE=SEC_SOURCE_FILE, EXIST=LEX )
              IF ( .NOT. LEX ) THEN
                   CALL ERR_LOG ( 1616, IUER, 'SUR_SKED_CONF', 'Error in '// &
     &                 'parsing experiment file '//EXP_FIL(1:I_LEN(EXP_FIL))// &
     &                 ' -- SOURCE_FILE '// &
     &                 SEC_SOURCE_FILE(1:I_LEN(SEC_SOURCE_FILE))// &
     &                 ' was not found' )
                   RETURN 
              END IF
              N_SUR = N_SUR + 1
            ELSE IF ( INDEX ( BUF(J1), 'OBSERVED_SOURCE_FILE:' ) == 1 ) THEN
              OBSERVED_SOURCE_FILE = BUF(J1)(IND(1,2):IND(2,2))
              INQUIRE ( FILE=OBSERVED_SOURCE_FILE, EXIST=LEX )
              IF ( .NOT. LEX ) THEN
                   CALL ERR_LOG ( 1617, IUER, 'SUR_SKED_CONF', 'Error in '// &
     &                 'parsing experiment file '//EXP_FIL(1:I_LEN(EXP_FIL))// &
     &                 ' -- OBSERVED_SOURCE_FILE '// &
     &                 OBSERVED_SOURCE_FILE(1:I_LEN(OBSERVED_SOURCE_FILE))// &
     &                 ' was not found' )
                   RETURN 
              END IF
              N_SUR = N_SUR + 1
            ELSE IF ( INDEX ( BUF(J1), 'CALIB_SOURCE_FILE:' ) == 1 ) THEN
              CALIB_SOURCE_FILE = BUF(J1)(IND(1,2):IND(2,2))
              INQUIRE ( FILE=CALIB_SOURCE_FILE, EXIST=LEX )
              IF ( .NOT. LEX ) THEN
                   CALL ERR_LOG ( 1618, IUER, 'SUR_SKED_CONF', 'Error in '// &
     &                 'parsing experiment file '//EXP_FIL(1:I_LEN(EXP_FIL))// &
     &                 ' -- CALIB_SOURCE_FILE '// &
     &                 CALIB_SOURCE_FILE(1:I_LEN(CALIB_SOURCE_FILE))// &
     &                 ' was not found' )
                   RETURN 
              END IF
              N_SUR = N_SUR + 1
            ELSE IF ( INDEX ( BUF(J1), 'PAIR_SOURCE_FILE:' ) == 1 ) THEN
              PAIR_SOURCE_FILE = BUF(J1)(IND(1,2):IND(2,2))
              INQUIRE ( FILE=PAIR_SOURCE_FILE, EXIST=LEX )
              IF ( PAIR_SOURCE_FILE(1:2) == 'no' .OR. PAIR_SOURCE_FILE(1:2) == 'NO' ) THEN
                   PAIR_SOURCE_FILE = 'NONE'
                 ELSE
                   IF ( .NOT. LEX ) THEN
                        CALL ERR_LOG ( 1619, IUER, 'SUR_SKED_CONF', 'Error in '// &
     &                      'parsing experiment file '//EXP_FIL(1:I_LEN(EXP_FIL))// &
     &                      ' -- PAIR_SOURCE_FILE '// &
     &                      PAIR_SOURCE_FILE(1:I_LEN(PAIR_SOURCE_FILE))// &
     &                      ' was not found' )
                        RETURN 
                   END IF
              END IF
              N_SUR = N_SUR + 1
            ELSE IF ( INDEX ( BUF(J1), 'POCAL_SOURCE_FILE:' ) == 1 ) THEN
              POCAL_SOURCE_FILE = BUF(J1)(IND(1,2):IND(2,2))
              IF ( POCAL_SOURCE_FILE(1:2) == 'no' .OR. POCAL_SOURCE_FILE(1:2) == 'NO' ) THEN
                   POCAL_SOURCE_FILE = 'NONE'
                 ELSE
                   INQUIRE ( FILE=POCAL_SOURCE_FILE, EXIST=LEX )
                   IF ( .NOT. LEX ) THEN
                        CALL ERR_LOG ( 1620, IUER, 'SUR_SKED_CONF', 'Error in '// &
     &                      'parsing experiment file '//EXP_FIL(1:I_LEN(EXP_FIL))// &
     &                      ' -- POCAL_SOURCE_FILE '// &
     &                      POCAL_SOURCE_FILE(1:I_LEN(POCAL_SOURCE_FILE))// &
     &                      ' was not found' )
                        RETURN 
                   END IF
              END IF
              N_SUR = N_SUR + 1
            ELSE IF ( INDEX ( BUF(J1), 'PLANET_SOURCE_FILE:' ) == 1 ) THEN
              PLANET_SOURCE_FILE = BUF(J1)(IND(1,2):IND(2,2))
              IF ( PLANET_SOURCE_FILE(1:2) == 'no' .OR. PLANET_SOURCE_FILE(1:2) == 'NO' ) THEN
                   PLANET_SOURCE_FILE = 'NONE'
                 ELSE
                   INQUIRE ( FILE=PLANET_SOURCE_FILE, EXIST=LEX )
                   IF ( .NOT. LEX ) THEN
                        CALL ERR_LOG ( 1621, IUER, 'SUR_SKED_CONF', 'Error in '// &
     &                      'parsing experiment file '//EXP_FIL(1:I_LEN(EXP_FIL))// &
     &                      ' -- PLANET_SOURCE_FILE '// &
     &                      PLANET_SOURCE_FILE(1:I_LEN(PLANET_SOURCE_FILE))// &
     &                      ' was not found' )
                        RETURN 
                   END IF
              END IF
              N_SUR = N_SUR + 1
            ELSE IF ( INDEX ( BUF(J1), 'START_TIME:' ) == 1 ) THEN
              SUR%DATE_START_UTC = BUF(J1)(IND(1,2):IND(2,2))
!
              CALL ERR_PASS ( IUER, IER )
              CALL DATE_TO_TIME ( SUR%DATE_START_UTC, SUR%MJD_UTC_START, &
     &                            SUR%UTC_START, IER )
              IF ( IER .NE. 0  ) THEN
                   CALL ERR_LOG ( 1622, IUER, 'SUR_SKED_CONF', 'Error in '// &
     &                 'parsing experiment file '//EXP_FIL(1:I_LEN(EXP_FIL))// &
     &                 ' -- START_TIME keyword was wrong format' )
                   RETURN 
              END IF
              N_SUR = N_SUR + 1
            ELSE IF ( INDEX ( BUF(J1), 'STOP_TIME:' ) == 1 ) THEN
              SUR%DATE_STOP_UTC = BUF(J1)(IND(1,2):IND(2,2))
!
              CALL ERR_PASS ( IUER, IER )
              CALL DATE_TO_TIME ( SUR%DATE_STOP_UTC, SUR%MJD_UTC_STOP, &
     &                            SUR%UTC_STOP, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 1623, IUER, 'SUR_SKED_CONF', 'Error in '// &
     &                 'parsing experiment file '//EXP_FIL(1:I_LEN(EXP_FIL))// &
     &                 ' -- STOP_TIME keyword was wrong format' )
                  RETURN 
              END IF
              N_SUR = N_SUR + 1
            ELSE IF ( INDEX ( BUF(J1), 'GAP_TIME:' ) == 1 ) THEN
              IF ( BUF(J1)(IND(1,2):IND(1,2)+1) == 'NO' .OR. &
     &             BUF(J1)(IND(1,2):IND(1,2))   ==  '0'      ) THEN
                   SUR%N_GAP = 0
                 ELSE
                   IF ( MOD(LIND,2) .NE. 1 ) THEN
                        CALL ERR_LOG ( 1623, IUER, 'SUR_SKED_CONF', 'Error in '// &
     &                      'parsing experiment file '//EXP_FIL(1:I_LEN(EXP_FIL))// &
     &                      ' -- GAP_TIME keyword should have even number of values' )
                        RETURN 
                   END IF
                   SUR%N_GAP = (LIND-1)/2
!
                   INDW = 2
                   INDE = 1
                   INDG = 1
                   DO 420 J2=1,2*SUR%N_GAP
                      READ ( UNIT=BUF(J1)(IND(1,INDW):IND(2,INDW)), &
     &                       FMT='(F6.3)', IOSTAT=IER ) SCHE_GAP(INDE,INDG)
                      IF ( IER .NE. 0 ) THEN
                           CALL CLRCH ( STR ) 
                           CALL INCH  ( INDW, STR )
                           CALL ERR_LOG ( 1623, IUER, 'SUR_SKED_CONF', 'Error in '// &
     &                      'parsing experiment file '//EXP_FIL(1:I_LEN(EXP_FIL))// &
     &                      ' -- GAP_TIME keyword '//TRIM(STR)//' has a wrong format '// &
     &                      '(real is expected)' )
                           RETURN 
                      END IF
                      INDW = INDW + 1
                      INDE = INDE + 1
                      IF ( INDE > 2 ) THEN
                           INDE = 1
                           INDG = INDG + 1
                      END IF
 420               CONTINUE 
              END IF
              N_SUR = N_SUR + 1
            ELSE IF ( INDEX ( BUF(J1), 'SCAN_LENGTH:' ) == 1 ) THEN
              IF ( INDEX ( BUF(J1)(IND(1,2):IND(2,2)), '.' ) == 0 ) THEN
                   STR = BUF(J1)(IND(1,2):IND(2,2))//'.0'
                 ELSE 
                   STR = BUF(J1)(IND(1,2):IND(2,2))
              END IF
              READ ( UNIT=STR(1:I_LEN(STR)), FMT='(F10.5)' ) SUR%SCAN_LEN 
              N_SUR = N_SUR + 1
            ELSE IF ( INDEX ( BUF(J1), 'CALIB_INTERVAL:' ) == 1 ) THEN
              IF ( INDEX ( BUF(J1)(IND(1,2):IND(2,2)), '.' ) == 0 ) THEN
                   STR = BUF(J1)(IND(1,2):IND(2,2))//'.0'
                 ELSE 
                   STR = BUF(J1)(IND(1,2):IND(2,2))
              END IF
              READ ( UNIT=STR(1:I_LEN(STR)), FMT='(F10.5)' ) SUR%CALIB_LEN
              N_SUR = N_SUR + 1
            ELSE IF ( INDEX ( BUF(J1), 'PRESES_INTERVAL:' ) == 1 ) THEN
              IF ( INDEX ( BUF(J1)(IND(1,2):IND(2,2)), '.' ) == 0 ) THEN
                   STR = BUF(J1)(IND(1,2):IND(2,2))//'.0'
                 ELSE 
                   STR = BUF(J1)(IND(1,2):IND(2,2))
              END IF
              READ ( UNIT=STR(1:I_LEN(STR)), FMT='(F10.5)' ) SUR%PRESES_INTERVAL
              N_SUR = N_SUR + 1
            ELSE IF ( INDEX ( BUF(J1), 'POSTSES_INTERVAL:' ) == 1 ) THEN
              IF ( INDEX ( BUF(J1)(IND(1,2):IND(2,2)), '.' ) == 0 ) THEN
                   STR = BUF(J1)(IND(1,2):IND(2,2))//'.0'
                 ELSE 
                   STR = BUF(J1)(IND(1,2):IND(2,2))
              END IF
              READ ( UNIT=STR(1:I_LEN(STR)), FMT='(F10.5)' ) SUR%POSTSES_INTERVAL
              N_SUR = N_SUR + 1
            ELSE IF ( INDEX ( BUF(J1), 'PREOBS_SHORT:' ) == 1 ) THEN
              IF ( INDEX ( BUF(J1)(IND(1,2):IND(2,2)), '.' ) == 0 ) THEN
                   STR = BUF(J1)(IND(1,2):IND(2,2))//'.0'
                 ELSE 
                   STR = BUF(J1)(IND(1,2):IND(2,2))
              END IF
              READ ( UNIT=STR(1:I_LEN(STR)), FMT='(F10.5)' ) SUR%PREOBS_SHORT
              N_SUR = N_SUR + 1
            ELSE IF ( INDEX ( BUF(J1), 'PREOBS_LONG:' ) == 1 ) THEN
              IF ( INDEX ( BUF(J1)(IND(1,2):IND(2,2)), '.' ) == 0 ) THEN
                   STR = BUF(J1)(IND(1,2):IND(2,2))//'.0'
                 ELSE 
                   STR = BUF(J1)(IND(1,2):IND(2,2))
              END IF
              READ ( UNIT=STR(1:I_LEN(STR)), FMT='(F10.5)' ) SUR%PREOBS_LONG
              N_SUR = N_SUR + 1
            ELSE IF ( INDEX ( BUF(J1), 'TAPE_LENGTH:' ) == 1 ) THEN
              IF ( INDEX ( BUF(J1)(IND(1,2):IND(2,2)), '.' ) == 0 ) THEN
                   STR = BUF(J1)(IND(1,2):IND(2,2))//'.0'
                 ELSE 
                   STR = BUF(J1)(IND(1,2):IND(2,2))
              END IF
              READ ( UNIT=STR(1:I_LEN(STR)), FMT='(F10.5)' ) SUR%TAPE_LENGTH
              N_SUR = N_SUR + 1
            ELSE IF ( INDEX ( BUF(J1), 'TAPE_CHANGE_TIME:' ) == 1 ) THEN
              IF ( INDEX ( BUF(J1)(IND(1,2):IND(2,2)), '.' ) == 0 ) THEN
                   STR = BUF(J1)(IND(1,2):IND(2,2))//'.0'
                 ELSE 
                   STR = BUF(J1)(IND(1,2):IND(2,2))
              END IF
              READ ( UNIT=STR(1:I_LEN(STR)), FMT='(F10.5)' ) SUR%TAPE_CHANGE_TIME
              N_SUR = N_SUR + 1
            ELSE IF ( INDEX ( BUF(J1), 'START_ROUNDING:' ) == 1 ) THEN
              IF ( INDEX ( BUF(J1)(IND(1,2):IND(2,2)), '.' ) == 0 ) THEN
                   STR = BUF(J1)(IND(1,2):IND(2,2))//'.0'
                 ELSE 
                   STR = BUF(J1)(IND(1,2):IND(2,2))
              END IF
              READ ( UNIT=STR(1:I_LEN(STR)), FMT='(F10.5)' ) SUR%START_ROUNDING
              IF ( SUR%START_ROUNDING .GE. 0.999D0 .AND. SUR%START_ROUNDING .LE. 1.001D0 ) THEN
                   SUR%START_ROUNDING = 1.0D0
                 ELSE IF ( SUR%START_ROUNDING .LE. 0.0999D0 ) THEN
                   CALL ERR_LOG ( 1624, IUER, 'SUR_SKED_CONF', 'Error in '// &
     &                 'parsing experiment file '//EXP_FIL(1:I_LEN(EXP_FIL))// &
     &                 ' -- error in parsing value '//BUF(J1)(IND(1,2):IND(2,2))// &
     &                 ' -- START_ROUNDING should be 0.1 or greater' )
                   RETURN 
              END IF
              N_SUR = N_SUR + 1
            ELSE IF ( INDEX ( BUF(J1), 'SKIP_PREOBS_LONG:' ) == 1 ) THEN
              READ ( UNIT=BUF(J1)(IND(1,2):IND(2,2)), FMT='(I6)' ) &
     &               SUR%SKIP_PREOBS_LONG
              N_SUR = N_SUR + 1
            ELSE IF ( INDEX ( BUF(J1), 'EL_CHANGE_TSYS:' ) == 1 ) THEN
              IF ( INDEX ( BUF(J1)(IND(1,2):IND(2,2)), '.' ) == 0 ) THEN
                   STR = BUF(J1)(IND(1,2):IND(2,2))//'.0'
                 ELSE 
                   STR = BUF(J1)(IND(1,2):IND(2,2))
              END IF
              READ ( UNIT=STR(1:I_LEN(STR)), FMT='(F10.5)' ) SUR%EL_CHANGE_TSYS
              SUR%EL_CHANGE_TSYS = SUR%EL_CHANGE_TSYS*DEG__TO__RAD  
              N_SUR = N_SUR + 1
            ELSE IF ( INDEX ( BUF(J1), 'RECORDING_PAUSE:' ) == 1 ) THEN
              READ ( UNIT=BUF(J1)(IND(1,2):IND(2,2)), FMT='(F10.5)' ) SUR%RECORDING_PAUSE
              N_SUR = N_SUR + 1
            ELSE IF ( INDEX ( BUF(J1), 'RECORDING_RATE:' ) == 1 ) THEN
              READ ( UNIT=BUF(J1)(IND(1,2):IND(2,2)), FMT='(F10.5)' ) SUR%RECORDING_RATE
              SUR%RECORDING_RATE = SUR%RECORDING_RATE*1.D6
              N_SUR = N_SUR + 1
            ELSE IF ( INDEX ( BUF(J1), 'AVERAGE_SLEW_TIME:' ) == 1 ) THEN
              READ ( UNIT=BUF(J1)(IND(1,2):IND(2,2)), FMT='(F10.5)', IOSTAT=IER ) SUR%AVR_SLEW_TIME
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 1624, IUER, 'SUR_SKED_CONF', 'Error in '// &
     &                 'parsing experiment file '//EXP_FIL(1:I_LEN(EXP_FIL))// &
     &                 ' -- error in parsing value '//BUF(J1)(IND(1,2):IND(2,2))// &
     &                 ' of the AVERAGE_SLEW_TIME keyword' )
                   RETURN 
              END IF
              N_SUR = N_SUR + 1
            ELSE IF ( INDEX ( BUF(J1), 'AVERAGE_SLEW_TROPO_TIME:' ) == 1 ) THEN
              READ ( UNIT=BUF(J1)(IND(1,2):IND(2,2)), FMT='(F10.5)', IOSTAT=IER ) SUR%AVR_SLEW_TROPO_TIME
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 1625, IUER, 'SUR_SKED_CONF', 'Error in '// &
     &                 'parsing experiment file '//EXP_FIL(1:I_LEN(EXP_FIL))// &
     &                 ' -- error in parsing value '//BUF(J1)(IND(1,2):IND(2,2))// &
     &                 ' of the AVERAGE_SLEW_TROPO_TIME keyword' )
                   RETURN 
              END IF
              N_SUR = N_SUR + 1
            ELSE IF ( INDEX ( BUF(J1), 'TROPO_BURST_INTERVAL:' ) == 1 ) THEN
              READ ( UNIT=BUF(J1)(IND(1,2):IND(2,2)), FMT='(F10.5)', IOSTAT=IER ) SUR%TROPO_BURST_INTERVAL
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 1626, IUER, 'SUR_SKED_CONF', 'Error in '// &
     &                 'parsing experiment file '//EXP_FIL(1:I_LEN(EXP_FIL))// &
     &                 ' -- error in parsing value '//BUF(J1)(IND(1,2):IND(2,2))// &
     &                 ' of the TROPO_BURST_INTERVAL keyword' )
                   RETURN 
              END IF
              N_SUR = N_SUR + 1
             ELSE IF ( INDEX ( BUF(J1), 'TROPO_SCAN_LENGTH:' ) == 1 ) THEN
              IF ( INDEX ( BUF(J1)(IND(1,2):IND(2,2)), '.' ) == 0 ) THEN
                   STR = BUF(J1)(IND(1,2):IND(2,2))//'.0'
                 ELSE 
                   STR = BUF(J1)(IND(1,2):IND(2,2))
              END IF
              READ ( UNIT=STR(1:I_LEN(STR)), FMT='(F10.5)' ) SUR%TROPO_SCAN_LEN 
              N_SUR = N_SUR + 1
            ELSE IF ( INDEX ( BUF(J1), 'TROPO_RANGE:' ) == 1 ) THEN
              READ ( UNIT=BUF(J1)(IND(1,2):IND(2,2)), FMT='(I6)', IOSTAT=IER ) SUR%TROPO_RANGE
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 1627, IUER, 'SUR_SKED_CONF', 'Error in '// &
     &                 'parsing experiment file '//EXP_FIL(1:I_LEN(EXP_FIL))// &
     &                 ' -- error in parsing value '//BUF(J1)(IND(1,2):IND(2,2))// &
     &                 ' of the TROPO_RANGE keyword' )
                   RETURN 
              END IF
              IF ( SUR%TROPO_RANGE < 0 .OR. SUR%TROPO_RANGE > SUR__TROPO_RANGE_MAX ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( SUR%TROPO_RANGE, STR )
                   CALL ERR_LOG ( 1628, IUER, 'SUR_SKED_CONF', 'Error in '// &
     &                 'parsing experiment file '//EXP_FIL(1:I_LEN(EXP_FIL))// &
     &                 ' -- error in parsing value '//BUF(J1)(IND(1,2):IND(2,2))// &
     &                 ' of the TROPO_RANGE keyword: It should be in the range '// &
     &                 '[1, '//STR(1:I_LEN(STR))//']' )
                   RETURN 
              END IF
              N_SUR = N_SUR + 1
            ELSE IF ( INDEX ( BUF(J1), 'TROPO_MIN_STA:' ) == 1 ) THEN
              READ ( UNIT=BUF(J1)(IND(1,2):IND(2,2)), FMT='(I6)', IOSTAT=IER ) SUR%TROPO_MIN_STA
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 1629, IUER, 'SUR_SKED_CONF', 'Error in '// &
     &                 'parsing experiment file '//EXP_FIL(1:I_LEN(EXP_FIL))// &
     &                 ' -- error in parsing value '//BUF(J1)(IND(1,2):IND(2,2))// &
     &                 ' of the TROPO_RANGE keyword' )
                   RETURN 
              END IF
              IF ( SUR%TROPO_MIN_STA < 1 ) THEN
                   CALL ERR_LOG ( 1630, IUER, 'SUR_SKED_CONF', 'Error in '// &
     &                 'parsing experiment file '//EXP_FIL(1:I_LEN(EXP_FIL))// &
     &                 ' -- the minimum number of stations for tropospere '// &
     &                 ' calibrators '//BUF(J1)(IND(1,2):IND(2,2))// &
     &                 ' should not be less than 2' )
                   RETURN 
              END IF
              N_SUR = N_SUR + 1
            ELSE IF ( INDEX ( BUF(J1), 'AZIM_180_MARGIN:' ) == 1 ) THEN
              READ ( UNIT=BUF(J1)(IND(1,2):IND(2,2)), FMT=*, IOSTAT=IER ) SUR%AZIM_180_MARGIN
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 1639, IUER, 'SUR_SKED_CONF', 'Error in '// &
     &                 'parsing experiment file '//EXP_FIL(1:I_LEN(EXP_FIL))// &
     &                 ' -- error in parsing value '//BUF(J1)(IND(1,2):IND(2,2))// &
     &                 ' of the AZIM_180_MARGIN keyword' )
                   RETURN 
              END IF
              IF ( SUR%AZIM_180_MARGIN > 90.0D0 ) THEN
                   CALL ERR_LOG ( 1630, IUER, 'SUR_SKED_CONF', 'Error in '// &
     &                 'parsing experiment file '//EXP_FIL(1:I_LEN(EXP_FIL))// &
     &                 ' -- the maximum AZIM_180_MARGIN should be less than 90.0 '// &
     &                 'degrees, but got '//BUF(J1)(IND(1,2):IND(2,2)) )
                   RETURN 
              END IF
              SUR%AZIM_180_MARGIN = SUR%AZIM_180_MARGIN*DEG__TO__RAD
              N_SUR = N_SUR + 1
            ELSE IF ( INDEX ( BUF(J1), 'SCAN_PER_SOURCE_NORM:' ) == 1 ) THEN
              READ ( UNIT=BUF(J1)(IND(1,2):IND(2,2)), FMT='(I6)', IOSTAT=IER ) SUR%SCAN_PER_SOURCE_NORM
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 1631, IUER, 'SUR_SKED_CONF', 'Error in '// &
     &                 'parsing experiment file '//EXP_FIL(1:I_LEN(EXP_FIL))// &
     &                 ' -- error in parsing value '//BUF(J1)(IND(1,2):IND(2,2))// &
     &                 ' of the SCAN_PER_SOURCE_NORM keyword' )
                   RETURN 
              END IF
              N_SUR = N_SUR + 1
            ELSE IF ( INDEX ( BUF(J1), 'SCAN_PER_SOURCE_MIN:' ) == 1 ) THEN
              READ ( UNIT=BUF(J1)(IND(1,2):IND(2,2)), FMT='(I6)', IOSTAT=IER ) SUR%SCAN_PER_SOURCE_MIN
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 1632, IUER, 'SUR_SKED_CONF', 'Error in '// &
     &                 'parsing experiment file '//EXP_FIL(1:I_LEN(EXP_FIL))// &
     &                 ' -- error in parsing value '//BUF(J1)(IND(1,2):IND(2,2))// &
     &                 ' of the SCAN_PER_SOURCE_MIN keyword' )
                   RETURN 
              END IF
              N_SUR = N_SUR + 1
            ELSE IF ( INDEX ( BUF(J1), 'SCAN_PER_SOURCE_MAX:' ) == 1 ) THEN
              READ ( UNIT=BUF(J1)(IND(1,2):IND(2,2)), FMT='(I6)', IOSTAT=IER ) SUR%SCAN_PER_SOURCE_MAX
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 1633, IUER, 'SUR_SKED_CONF', 'Error in '// &
     &                 'parsing experiment file '//EXP_FIL(1:I_LEN(EXP_FIL))// &
     &                 ' -- error in parsing value '//BUF(J1)(IND(1,2):IND(2,2))// &
     &                 ' of the SCAN_PER_SOURCE_MAX keyword' )
                   RETURN 
              END IF
              N_SUR = N_SUR + 1
            ELSE IF ( INDEX ( BUF(J1), 'SCAN_GAP_SOURCE_NORM:' ) == 1 ) THEN
              IF ( INDEX ( BUF(J1)(IND(1,2):IND(2,2)), '.' ) > 0 ) THEN
                   READ ( UNIT=BUF(J1)(IND(1,2):IND(2,2)), FMT='(F10.5)', IOSTAT=IER ) SUR%SCAN_GAP_SOURCE_NORM
                 ELSE 
                   CALL CLRCH ( STR )
                   STR = BUF(J1)(IND(1,2):IND(2,2))//'.0'
                   READ ( UNIT=STR, FMT='(F10.5)', IOSTAT=IER ) SUR%SCAN_GAP_SOURCE_NORM
              END IF
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 1634, IUER, 'SUR_SKED_CONF', 'Error in '// &
     &                 'parsing experiment file '//EXP_FIL(1:I_LEN(EXP_FIL))// &
     &                 ' -- error in parsing value '//BUF(J1)(IND(1,2):IND(2,2))// &
     &                 ' of the SCAN_GAP_SOURCE_NORM keyword' )
                   RETURN 
              END IF
              SUR%SCAN_GAP_SOURCE_NORM = 60.0D0*SUR%SCAN_GAP_SOURCE_NORM
              N_SUR = N_SUR + 1
            ELSE IF ( INDEX ( BUF(J1), 'SCAN_GAP_SOURCE_MIN:' ) == 1 ) THEN
              IF ( INDEX ( BUF(J1)(IND(1,2):IND(2,2)), '.' ) > 0 ) THEN
                   READ ( UNIT=BUF(J1)(IND(1,2):IND(2,2)), FMT='(F10.5)', IOSTAT=IER ) SUR%SCAN_GAP_SOURCE_MIN
                 ELSE 
                   CALL CLRCH ( STR )
                   STR = BUF(J1)(IND(1,2):IND(2,2))//'.0'
                   READ ( UNIT=STR, FMT='(F10.5)', IOSTAT=IER ) SUR%SCAN_GAP_SOURCE_MIN
              END IF
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 1635, IUER, 'SUR_SKED_CONF', 'Error in '// &
     &                 'parsing experiment file '//EXP_FIL(1:I_LEN(EXP_FIL))// &
     &                 ' -- error in parsing value '//BUF(J1)(IND(1,2):IND(2,2))// &
     &                 ' of the SCAN_GAP_SOURCE_MIN keyword' )
                   RETURN 
              END IF
              SUR%SCAN_GAP_SOURCE_MIN = 60.0D0*SUR%SCAN_GAP_SOURCE_MIN
              N_SUR = N_SUR + 1
            ELSE IF ( INDEX ( BUF(J1), 'NOBS_MIN:' ) == 1 ) THEN
              READ ( UNIT=BUF(J1)(IND(1,2):IND(2,2)), FMT='(I6)', IOSTAT=IER ) SUR%NOBS_MIN
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 1636, IUER, 'SUR_SKED_CONF', 'Error in '// &
     &                 'parsing experiment file '//EXP_FIL(1:I_LEN(EXP_FIL))// &
     &                 ' -- error in parsing value '//BUF(J1)(IND(1,2):IND(2,2))// &
     &                 ' of the NOBS_MIN keyword' )
                   RETURN 
              END IF
              N_SUR = N_SUR + 1
            ELSE IF ( INDEX ( BUF(J1), 'NOBS_MAX:' ) == 1 ) THEN
              READ ( UNIT=BUF(J1)(IND(1,2):IND(2,2)), FMT='(I6)', IOSTAT=IER ) SUR%NOBS_MAX
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 1637, IUER, 'SUR_SKED_CONF', 'Error in '// &
     &                 'parsing experiment file '//EXP_FIL(1:I_LEN(EXP_FIL))// &
     &                 ' -- error in parsing value '//BUF(J1)(IND(1,2):IND(2,2))// &
     &                 ' of the NOBS_MAX keyword' )
                   RETURN 
              END IF
              N_SUR = N_SUR + 1
            ELSE IF ( INDEX ( BUF(J1), 'DE_FILE:' ) == 1 ) THEN
              SUR%DE_FILE = BUF(J1)(IND(1,2):IND(2,2))
              INQUIRE ( FILE=SUR%DE_FILE, EXIST=LEX )
              IF ( .NOT. LEX ) THEN
                   CALL ERR_LOG ( 1639, IUER, 'SUR_SKED_CONF', 'Error in '// &
     &                 'parsing experiment file '//EXP_FIL(1:I_LEN(EXP_FIL))// &
     &                 ' -- DE_FILE '//SUR%DE_FILE(1:I_LEN(SUR%DE_FILE))// &
     &                 ' was not found' )
                   RETURN 
              END IF
              N_SUR = N_SUR + 1
            ELSE IF ( INDEX ( BUF(J1), 'SUN_DIST_MIN:' ) == 1 ) THEN
              READ ( UNIT=BUF(J1)(IND(1,2):IND(2,2)), FMT='(F8.2)', IOSTAT=IER ) &
     &               SUR%SUN_DIST_MIN
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 1640, IUER, 'SUR_SKED_CONF', 'Error in '// &
     &                 'parsing parameter SUN_DIST_MIN: '// &
     &                 BUF(J1)(IND(1,2):IND(2,2))//' a float number was '// &
     &                 'expected' )
                   RETURN 
              END IF
              SUR%SUN_DIST_MIN = SUR%SUN_DIST_MIN*DEG__TO__RAD
              N_SUR = N_SUR + 1
            ELSE IF ( INDEX ( BUF(J1), 'STATION_SLEW_DIR:' ) == 1 ) THEN
              SUR%STATION_SLEW_DIR = BUF(J1)(IND(1,2):IND(2,2))
              N_SUR = N_SUR + 1
            ELSE IF ( INDEX ( BUF(J1), 'STATIONS:' ) == 1 ) THEN
              STATION_LINE = BUF(J1)(IND(1,2):IND(2,2))
              N_SUR = N_SUR + 1
            ELSE IF ( INDEX ( BUF(J1), 'HEADER_VEX_TEMPLATE_FILE:' ) == 1 ) THEN
              SUR%HEADER_VEX_TEMPLATE_FILE = BUF(J1)(IND(1,2):IND(2,2))
              INQUIRE ( FILE=SUR%HEADER_VEX_TEMPLATE_FILE, EXIST=LEX )
              IF ( .NOT. LEX ) THEN
                   CALL ERR_LOG ( 1643, IUER, 'SUR_SKED_CONF', 'Error in '// &
     &                 'parsing experiment file '//EXP_FIL(1:I_LEN(EXP_FIL))// &
     &                 ' -- SUR%HEADER_VEX_TEMPLATE_FILE '// &
     &                 SUR%HEADER_VEX_TEMPLATE_FILE(1:I_LEN(SUR%HEADER_VEX_TEMPLATE_FILE))// &
     &                 ' was not found' )
                   RETURN 
              END IF
              N_SUR = N_SUR + 1
            ELSE IF ( INDEX ( BUF(J1), 'HEADER_KEY_TEMPLATE_FILE:' ) == 1 ) THEN
              SUR%HEADER_KEY_TEMPLATE_FILE = BUF(J1)(IND(1,2):IND(2,2))
              INQUIRE ( FILE=SUR%HEADER_KEY_TEMPLATE_FILE, EXIST=LEX )
              IF ( .NOT. LEX ) THEN
                   CALL ERR_LOG ( 1644, IUER, 'SUR_SKED_CONF', 'Error in '// &
     &                 'parsing experiment file '//EXP_FIL(1:I_LEN(EXP_FIL))// &
     &                 ' -- SUR%HEADER_KEY_TEMPLATE_FILE '// &
     &                 SUR%HEADER_KEY_TEMPLATE_FILE(1:I_LEN(SUR%HEADER_KEY_TEMPLATE_FILE))// &
     &                 ' was not found' )
                   RETURN 
              END IF
              N_SUR = N_SUR + 1
            ELSE IF ( INDEX ( BUF(J1), 'TLE:' ) == 1 ) THEN
              SUR%TLE_FILE = BUF(J1)(IND(1,2):IND(2,2))
              IF ( SUR%TLE_FILE == 'NO' ) THEN
                   CONTINUE 
                 ELSE
                   INQUIRE ( FILE=SUR%HEADER_KEY_TEMPLATE_FILE, EXIST=LEX )
                   IF ( .NOT. LEX ) THEN
                        CALL ERR_LOG ( 1645, IUER, 'SUR_SKED_CONF', 'Error in '// &
     &                      'parsing experiment file '//EXP_FIL(1:I_LEN(EXP_FIL))// &
     &                      ' -- SUR%HEADER_KEY_TEMPLATE_FILE '// &
     &                       SUR%HEADER_KEY_TEMPLATE_FILE(1:I_LEN(SUR%HEADER_KEY_TEMPLATE_FILE))// &
     &                      ' was not found' )
                        RETURN 
                   END IF
              END IF
              N_SUR = N_SUR + 1
            ELSE IF ( INDEX ( BUF(J1), 'HARDWARE_SETUP_NAME:' ) == 1 ) THEN
              SUR%HARDWARE_SETUP_NAME = BUF(J1)(IND(1,2):IND(2,2))
              N_SUR = N_SUR + 1
            ELSE IF ( INDEX ( BUF(J1), 'OBSERVING_MODE_NAME:' ) == 1 ) THEN
              SUR%OBSERVING_MODE_NAME = BUF(J1)(IND(1,2):IND(2,2))
              N_SUR = N_SUR + 1
            ELSE IF ( INDEX ( BUF(J1), 'OBSERVING_MODE_DESCR:' ) == 1 ) THEN
              SUR%OBSERVING_MODE_DESCR = BUF(J1)(IND(1,2):IND(2,LIND))
              N_SUR = N_SUR + 1
            ELSE IF ( INDEX ( BUF(J1), 'KEY_FILE_TYPE:' ) == 1 ) THEN
              IF ( BUF(J1)(IND(1,2):IND(2,2)) == 'TIME_ABS' ) THEN
                   SUR%KEY_TYP = KEY__TIME_ABS 
                 ELSE IF ( BUF(J1)(IND(1,2):IND(2,2)) == 'TIME_DUR' ) THEN
                   SUR%KEY_TYP = KEY__TIME_DUR
                 ELSE IF ( BUF(J1)(IND(1,2):IND(2,2)) == 'LST_PT' ) THEN
                   SUR%KEY_TYP = KEY__LST_PT  
                 ELSE IF ( BUF(J1)(IND(1,2):IND(2,2)) == 'LST_PA' ) THEN
                   SUR%KEY_TYP = KEY__LST_PA  
                 ELSE IF ( BUF(J1)(IND(1,2):IND(2,2)) == 'LST_JB' ) THEN
                   SUR%KEY_TYP = KEY__LST_JB  
                 ELSE IF ( BUF(J1)(IND(1,2):IND(2,2)) == 'LST_MC' ) THEN
                   SUR%KEY_TYP = KEY__LST_MC  
                 ELSE IF ( BUF(J1)(IND(1,2):IND(2,2)) == 'LST_EF' ) THEN
                   SUR%KEY_TYP = KEY__LST_EF
                 ELSE IF ( BUF(J1)(IND(1,2):IND(2,2)) == 'START_STOP' ) THEN
                   SUR%KEY_TYP = KEY__START_STOP
                 ELSE IF ( BUF(J1)(IND(1,2):IND(2,2)) == 'START_STOP_2STA' ) THEN
                   SUR%KEY_TYP = KEY__START_STOP_2STA
                 ELSE IF ( BUF(J1)(IND(1,2):IND(2,2)) == 'START_STOP_3STA' ) THEN
                   SUR%KEY_TYP = KEY__START_STOP_3STA
                 ELSE 
                   CALL ERR_LOG ( 1646, IUER, 'SUR_SKED_CONF', 'Error in '// &
     &                 'parsing experiment file '//EXP_FIL(1:I_LEN(EXP_FIL))// &
     &                 ' -- SUR%FREQ_SETUP_FILE : unrecognized type '// &
     &                 ' of key_file: '//BUF(J1)(IND(1,2):IND(2,2))// &
     &                 ' -- TIME_ABS, LST_PT, LST_PA, LST_JB, LST_LC, '// &
     &                 'START_STOP,  START_STOP_2STA, START_STOP_3STA '// &
     &                 'were expected' )
                   RETURN 
              END IF
              N_SUR = N_SUR + 1
            ELSE IF ( INDEX ( BUF(J1), 'POCAL_STYLE:' ) == 1 ) THEN
              SUR%POCAL_STYLE = BUF(J1)(IND(1,2):IND(2,2))
              IF ( SUR%POCAL_STYLE == 'NO' ) THEN
                   CONTINUE 
                 ELSE IF ( SUR%POCAL_STYLE == 'NONE' ) THEN
                   SUR%POCAL_STYLE = 'NO'
                   CONTINUE 
                 ELSE IF ( SUR%POCAL_STYLE == POCAL_STYLE__GBT_4HR ) THEN
                   CONTINUE 
                 ELSE IF ( SUR%POCAL_STYLE == POCAL_STYLE__KVN ) THEN
                   CONTINUE 
                 ELSE IF ( SUR%POCAL_STYLE == POCAL_STYLE__ATCA ) THEN
                   CONTINUE 
                 ELSE 
                   CALL ERR_LOG ( 1647, IUER, 'SUR_SKED_CONF', 'Error in '// &
     &                 'parsing experiment file '//EXP_FIL(1:I_LEN(EXP_FIL))// &
     &                 ' -- SUR%POCAL_STYLE : unrecognized style '// &
     &                 SUR%POCAL_STYLE )
                   RETURN 
              END IF
              N_SUR = N_SUR + 1
            ELSE IF ( INDEX ( BUF(J1), 'OUT_PLAN:' ) == 1 ) THEN
              SUR%OUT_PLAN = BUF(J1)(IND(1,2):IND(2,2))
              N_SUR = N_SUR + 1
            ELSE IF ( INDEX ( BUF(J1), 'OUT_VEX:' ) == 1 ) THEN
              SUR%OUT_VEX = BUF(J1)(IND(1,2):IND(2,2))
              N_SUR = N_SUR + 1
            ELSE IF ( INDEX ( BUF(J1), 'OUT_KEY:' ) == 1 ) THEN
              SUR%OUT_KEY = BUF(J1)(IND(1,2):IND(2,2))
              N_SUR = N_SUR + 1
            ELSE IF ( INDEX ( BUF(J1), 'OUT_STAT:' ) == 1 ) THEN
              SUR%OUT_STAT = BUF(J1)(IND(1,2):IND(2,2))
              N_SUR = N_SUR + 1
            ELSE IF ( INDEX ( BUF(J1), 'OUT_AST:' ) == 1 ) THEN
              SUR%OUT_AST = BUF(J1)(IND(1,2):IND(2,2))
              N_SUR = N_SUR + 1
            ELSE IF ( INDEX ( BUF(J1), 'OUT_SOU_LIST:' ) == 1 ) THEN
              SUR%OUT_SOU_LIST = BUF(J1)(IND(1,2):IND(2,2))
              N_SUR = N_SUR + 1
            ELSE 
              CALL CLRCH ( STR )
              CALL INCH  ( J1, STR )
              CALL ERR_LOG ( 1648, IUER, 'SUR_SKED_CONF', 'Unknown '// &
     &            'parameter '//BUF(J1)(IND(1,1):IND(2,1))//' was found '// &
     &            'at line '//STR(1:I_LEN(STR))//' of the control file '// &
     &             EXP_FIL )
              RETURN 
         END IF
 410  CONTINUE 
!
      IF ( N_SUR < M__SUR_PAR ) THEN
           CALL CLRCH ( STR  )
           CALL CLRCH ( STR1 )
           CALL INCH  ( N_SUR,      STR  )
           CALL INCH  ( M__SUR_PAR, STR1 )
           CALL ERR_LOG ( 1649, IUER, 'SUR_SKED_CONF', 'Not all parameters '// &
     &         'were found in experiment file '//EXP_FIL(1:I_LEN(EXP_FIL))// &
     &         ' -- only '//STR(1:I_LEN(STR))//' out of '//STR1 )
           RETURN 
      END IF
!
      IF ( SUR%NOBS_MIN > SUR%NOBS_MAX ) THEN
           CALL CLRCH ( STR  )
           CALL CLRCH ( STR1 )
           CALL INCH  ( SUR%NOBS_MIN, STR  )
           CALL INCH  ( SUR%NOBS_MAX, STR1 )
           CALL ERR_LOG ( 1650, IUER, 'SUR_SKED_CONF', 'NOBS_MIN: '// &
     &          STR(1:I_LEN(STR))//' is greater than NOBS_MAX: '// &
     &          STR1 )
           RETURN 
      END IF
      SUR%CDATE_START = GET_CDATE()
!
! --- Initialization of VTD object
!
      CALL ERR_PASS ( IUER, IER )
      CALL VTD_INIT ( VTD, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 1651, IUER, 'SUR_SKED_CONF', 'Error in '// &
     &        'initialization of VTD object' )
           RETURN 
      END IF
!
      VTD%CONF%FINAM_LEAPSEC = VTD__NERS_STR
      VTD%CONF%FINAM_EOP     = VTD__NERS_STR
      VTD%CONF%UZT_MODEL     = VTD__NERS
      VTD%CONF%PREC_EXP      = VTD__NERS 
      VTD%CONF%NUT_EXP       = VTD__NERS 
      VTD%CONF%FINAM_HEO     = VTD__NERS_STR 
      VTD%L_HEO              = VTD__NERS
      VTD%MJD_BEG = SUR%MJD_UTC_START
      VTD%TAI_BEG = SUR%UTC_START
      VTD%MJD_END = SUR%MJD_UTC_STOP
      VTD%TAI_END = SUR%UTC_STOP
!
! ---- Leap second is taken via NERS. Then let us initialize NERS
!
      CALL GETENVAR ( 'NERS_CONFIG', STR )
      IF ( ILEN(STR) > 0 ) THEN
           NERS_CONFIG_FILE = 'NERS_CONFIG'
         ELSE
           NERS_CONFIG_FILE = NERS__CONFIG
      END IF
      CALL ERR_PASS  ( IUER, IER )
      CALL NERS_INIT ( NERS_CONFIG_FILE, VTD%NERS, &
     &             (VTD%MJD_BEG - J2000__MJD)*86400.0D0 + VTD%TAI_BEG - VTD__EOP_TIM_MAR, &
     &             (VTD%MJD_END - J2000__MJD)*86400.0D0 + VTD%TAI_END + VTD__EOP_TIM_MAR, &
     &              IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 1652, IUER, 'SUR_SKED_CONF', 'Error in an attempt '// &
     &         'to initialize NERS data structure' )
           RETURN 
      END IF
      VTD%LEAPSEC%STATUS = VTD__NERS
!
! --- Load DE ephemerides from the external file specifed in VTD%CONF
!
      CALL ERR_PASS ( IUER, IER )
      CALL READ_DE_EPH ( SUR%DE_FILE, VTD%DE_EPH, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 1653, IUER, 'SUR_SKED_CONF', 'Error in an attempt '// &
     &         'to load planet ephemerides from the input file '//SUR%DE_FILE )
           RETURN
      END IF
      IF ( VTD%CONF%FINAM_EOP .NE. VTD__NERS_STR ) THEN
!
! -------- Load the file with the Earth orientation parameters from the external
! -------- file specifed in VTD%CONF
!
           CALL ERR_PASS  ( IUER, IER )
           CALL VTD_UEOP_INIT ( VTD, VTD%CONF%FINAM_EOP, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 1654, IUER, 'SUR_SKED_CONF', 'Error in an attempt '// &
     &              'to load apriori EOP using NERS' )
                RETURN
           END IF
      END IF
!
! --- Transform UTC tag to TAI time
!
      CALL ERR_PASS ( IUER, IER )
      CALL VTD_UTC_TO_TAI ( VTD, SUR%MJD_UTC_START, SUR%UTC_START, &
     &                      SUR%TAI_START, IER )
      IF ( IER .NE. 0 ) THEN
           WRITE ( 6, * ) 'VTD%LEAPSEC%STATUS = ', VTD%LEAPSEC%STATUS 
           CALL ERR_LOG ( 1655, IUER, 'SUR_SKED_CONF', 'Error in computing '// &
     &                   'UTC_M_TAI difference' )
           RETURN 
      END IF
      SUR%UTC_M_TAI = SUR%UTC_START - SUR%TAI_START
      IF ( SUR%UTC_M_TAI > 0.0D0 ) SUR%UTC_M_TAI = SUR%UTC_M_TAI - 86400.0D0
!
! --- Computing UTC_STOP at MJD_START epoch
! --- NB!! This is not a typo!!!
! --- Under agreement, if UTC leap occurred during experiment, UTC tag
! --- is not updated
!
      CALL ERR_PASS ( IUER, IER )
      CALL VTD_UTC_TO_TAI ( VTD, SUR%MJD_UTC_STOP, SUR%UTC_STOP, SUR%TAI_STOP, &
     &                      IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 1656, IUER, 'SUR_SKED_CONF', 'Error in computing '// &
     &                   'UTC_M_TAI difference' )
           RETURN 
      END IF
!
      IF ( SUR%TAI_START > 86400.0D0 ) THEN
           SUR%TAI_START = SUR%TAI_START - 86400.0D0
           SUR%MJD_START = SUR%MJD_UTC_START + 1
         ELSE 
           SUR%MJD_START = SUR%MJD_UTC_START 
      END IF
!
      IF ( SUR%TAI_STOP > 86400.0D0 ) THEN
           SUR%TAI_STOP = SUR%TAI_STOP - 86400.0D0
           SUR%MJD_STOP = SUR%MJD_UTC_STOP + 1
         ELSE 
           SUR%MJD_STOP = SUR%MJD_UTC_STOP 
      END IF
!
      DUR_GAP_SEC = 0.0D0
      IF ( SUR%N_GAP > 0 ) THEN
           DO 430 J3=1,SUR%N_GAP
              SUR%TAI_GAP(1,J3) = SUR%TAI_START + 3600.0D0*SCHE_GAP(1,J3)
              SUR%TAI_GAP(2,J3) = SUR%TAI_START + 3600.0D0*SCHE_GAP(2,J3)
!
              IF ( SUR%TAI_GAP(1,J3) > 86400.0D0 ) THEN
                   SUR%TAI_GAP(1,J3) = SUR%TAI_GAP(1,J3) - 86400.0D0
                   SUR%MJD_GAP(1,J3) = SUR%MJD_START + 1
                ELSE 
                   SUR%MJD_GAP(1,J3) = SUR%MJD_START
              END IF
!
              IF ( SUR%TAI_GAP(1,J3) > 86400.0D0 ) THEN
                   SUR%TAI_GAP(1,J3) = SUR%TAI_GAP(1,J3) - 86400.0D0
                   SUR%MJD_GAP(1,J3) = SUR%MJD_START + 1
              END IF
!!
              IF ( SUR%TAI_GAP(2,J3) > 86400.0D0 ) THEN
                   SUR%TAI_GAP(2,J3) = SUR%TAI_GAP(2,J3) - 86400.0D0
                   SUR%MJD_GAP(2,J3) = SUR%MJD_START + 1
                ELSE 
                   SUR%MJD_GAP(2,J3) = SUR%MJD_START
              END IF
!
              IF ( SUR%TAI_GAP(2,J3) > 86400.0D0 ) THEN
                   SUR%TAI_GAP(2,J3) = SUR%TAI_GAP(2,J3) - 86400.0D0
                   SUR%MJD_GAP(2,J3) = SUR%MJD_START + 1
              END IF
!
              DUR_GAP_SEC = DUR_GAP_SEC + (SCHE_GAP(2,J3) - SCHE_GAP(1,J3))*3600.0D0
 430       CONTINUE 
      END IF
!
      DUR_SEC = (SUR%MJD_STOP - SUR%MJD_START)*86400.0D0 + &
     &          (SUR%TAI_STOP - SUR%TAI_START)
!
      IF ( DUR_SEC < 120.0D0 ) THEN
           CALL ERR_LOG ( 1657, IUER, 'SUR_SKED_CONF', 'Too short '// &
     &         'experiment: ['//SUR%DATE_START_UTC//', '//SUR%DATE_STOP_UTC// &
     &         ' -- please check start/stop dates' )
           RETURN 
      END IF
!
      DUR_EFF_SEC = DUR_SEC - DUR_GAP_SEC
      IF ( IVRB .GE. 4 .OR. IVRB == -7 ) THEN
           WRITE ( 6, 210 ) DUR_EFF_SEC/3600.0D0, DUR_SEC/3600.0D0, DUR_GAP_SEC/3600.0D0
 210       FORMAT ( 'Effective experiment duration: ', F5.2, ' hr '/ &
     &              'Nominal   experiment duration: ', F5.2, ' hr '/ &
     &              'Experiment gaps:               ', F5.2, ' hr '/ )
           IF ( SUR%N_GAP > 0 ) THEN
                DO 440 J4=1,SUR%N_GAP + 1
                   IF ( J4 == 1 ) THEN
                        SEG_INT_HR = SCHE_GAP(1,J4)
                      ELSE IF ( J4 == SUR%N_GAP + 1 ) THEN
                        SEG_INT_HR = ((SUR%MJD_STOP - SUR%MJD_START)*86400.0D0 + &
     &                                (SUR%TAI_STOP - SUR%TAI_START))/3600.0D0 - &
     &                               SCHE_GAP(2,J4-1)
                      ELSE
                        SEG_INT_HR = SCHE_GAP(1,J4) - SCHE_GAP(2,J4-1)
                   END IF
                   WRITE ( 6, 220 ) J4, SEG_INT_HR
 220               FORMAT ( 'Segement ', I1, ' Duration: ', F5.2, ' hours' )
 440            CONTINUE 
           END IF
           IF ( IVRB == -7 ) THEN
                CALL EXIT ( 0 )
           END IF
      END IF
!
! --- Parse station file
!
      CALL ERR_PASS ( IUER, IER )
      CALL SUR_STA_CONF ( SUR, STATION_LINE, IVRB, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 1658, IUER, 'SUR_SKED_CONF', 'Error in parsing '// &
     &         'the station list '//STATION_LINE )
           RETURN 
      END IF
      IF ( SUR%REF_STA == 0 ) THEN
           CALL ERR_LOG ( 1659, IUER, 'SUR_SKED_CONF', 'Did not find '// &
     &         'the reference station in '//STATION_LINE )
           RETURN 
      END IF
!
! --- Read the list of observed sources
!
      CALL ERR_PASS ( IUER, IER )
      CALL READ_SOU ( OBSERVED_SOURCE_FILE, SUR__M_SOU, LOBS_OBS, &
     &                OBS_OBS, COBS_SOU, MODE, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 1660, IUER, 'SUR_SKED_CONF', 'Error in reading '// &
     &         'observed sources file '//OBSERVED_SOURCE_FILE )
           RETURN 
      END IF
!
! --- Read the list of amplitude calibrators
!
      CALL ERR_PASS ( IUER, IER )
      CALL READ_SOU ( CALIB_SOURCE_FILE, SUR__M_SOU, L_CAL, &
     &                OBS_CAL, COBS_CAL, MODE, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 1661, IUER, 'SUR_SKED_CONF', 'Error in reading '// &
     &         'observed sources file '//CALIB_SOURCE_FILE )
           RETURN 
      END IF
!
! --- Read the list of pairs
!
      IF ( PAIR_SOURCE_FILE == 'NONE' ) THEN
           L_PAI = 0
         ELSE 
           CALL ERR_PASS ( IUER, IER )
           CALL READ_SOU ( PAIR_SOURCE_FILE, SUR__M_SOU, L_PAI, &
     &                     OBS_PAI, COBS_PAI, MODE, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 1662, IUER, 'SUR_SKED_CONF', 'Error in reading '// &
     &              'observed sources file '//PAIR_SOURCE_FILE )
                RETURN 
           END IF
      END IF
!
! --- Read the list of pointing calibrators
!
      IF ( POCAL_SOURCE_FILE .EQ. 'NONE' ) THEN
           L_POC = 0
         ELSE 
           CALL ERR_PASS ( IUER, IER )
           CALL READ_SOU ( POCAL_SOURCE_FILE, SUR__M_SOU, L_POC, &
     &                     OBS_POC, COBS_POC, MODE, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 1663, IUER, 'SUR_SKED_CONF', 'Error in reading '// &
     &              'observed sources file '//PAIR_SOURCE_FILE )
               RETURN 
           END IF
      END IF
      SUR%L_SOP = L_POC
      IF ( SUR%L_SOP > 0 ) THEN
           DO 460 J6=1,SUR%L_SOP
              SUR%SOP(J6)%ALPHA = OBS_POC(J6)%ALP
              SUR%SOP(J6)%DELTA = OBS_POC(J6)%DEL
              SUR%SOP(J6)%S_VEC(1:3) = OBS_POC(J6)%S_VEC(1:3)
              CALL RH_TAT ( OBS_POC(J6)%ALP, 6, SUR%SOP(J6)%ALPHA_STR, -2 )
              CALL CHASHL ( SUR%SOP(J6)%ALPHA_STR )
              CALL RG_TAT ( OBS_POC(J6)%DEL, 5, SUR%SOP(J6)%DELTA_STR, -2 )
              IF ( SUR%SOP(J6)%DELTA_STR(1:1) == ' ' ) SUR%SOP(J6)%DELTA_STR(1:1) = '+'
              SUR%SOP(J6)%J2000_NAME = OBS_POC(J6)%J2000_NAME
              SUR%SOP(J6)%B1950_NAME = OBS_POC(J6)%B1950_NAME
 460       CONTINUE 
      END IF
!
! --- Read the list of planets
!
      IF ( PLANET_SOURCE_FILE .EQ. 'NONE' ) THEN
           L_PLA = 0
         ELSE 
           CALL ERR_PASS ( IUER, IER )
           CALL READ_SOU ( PLANET_SOURCE_FILE, SUR__M_SOU, L_PLA, &
     &                     OBS_PLA, COBS_PLA, MODE, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 1664, IUER, 'SUR_SKED_CONF', 'Error in reading '// &
     &              'observed sources file '//PAIR_SOURCE_FILE )
                RETURN 
           END IF
      END IF
      SUR%L_PLA = L_PLA
      IF ( SUR%L_PLA > 0 ) THEN
           DO 470 J7=1,SUR%L_PLA
              SUR%PLA(J7)%ALPHA = OBS_PLA(J7)%ALP
              SUR%PLA(J7)%DELTA = OBS_PLA(J7)%DEL
              SUR%PLA(J7)%S_VEC(1:3) = OBS_PLA(J7)%S_VEC(1:3)
              CALL RH_TAT ( OBS_PLA(J7)%ALP, 6, SUR%PLA(J7)%ALPHA_STR, -2 )
              CALL CHASHL ( SUR%PLA(J7)%ALPHA_STR )
              CALL RG_TAT ( OBS_PLA(J7)%DEL, 5, SUR%PLA(J7)%DELTA_STR, -2 )
              IF ( SUR%PLA(J7)%DELTA_STR(1:1) == ' ' ) SUR%PLA(J7)%DELTA_STR(1:1) = '+'
              SUR%PLA(J7)%J2000_NAME = OBS_PLA(J7)%J2000_NAME
              SUR%PLA(J7)%B1950_NAME = OBS_PLA(J7)%B1950_NAME
 470       CONTINUE 
      END IF
!
      LOBS_SOU = 0
      CALL ERR_PASS   ( IUER, IER )
      CALL SUR_SOURCE ( SUR, VTD, SOURCE_FILE, SUR%L_SOU, SUR%SOU, LOBS_OBS, &
     &                  OBS_OBS, DUR_SEC, SUR%SCAN_LEN, SUR__TYP_TAG, &
     &                  IVRB, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 1665, IUER, 'SUR_SKED_CONF', 'Error in parsing '// &
     &         'source catalogue '//SOURCE_FILE )
           RETURN 
      END IF
      LOBS_SOU_SAVE = LOBS_SOU
!
      DO 480 J8=1,SUR%L_SOU
         LOBS_SOU = LOBS_SOU + 1
         OBS_CAT(LOBS_SOU)%S_VEC(1) = SUR%SOU(J8)%S_VEC(1)
         OBS_CAT(LOBS_SOU)%S_VEC(2) = SUR%SOU(J8)%S_VEC(2)
         OBS_CAT(LOBS_SOU)%S_VEC(3) = SUR%SOU(J8)%S_VEC(3)
 480  CONTINUE 
!
! --- Get Sun coordinates at the middle of the session
!
      MJD_MID = SUR%MJD_START
      TAI_MID = SUR%TAI_START + ( SUR%MJD_STOP - SUR%MJD_START )*86400.0D0 + &
     &                          ( SUR%TAI_STOP - SUR%TAI_START )
      IF ( TAI_MID > 86400.0D0 ) THEN
           TAI_MID = TAI_MID - 86400.0D0
           MJD_MID = MJD_MID + 1
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL PLANETA_DE_EPH ( VTD%DE_EPH, MJD_MID, TAI_MID, 'EARTH', &
     &                      COO_EAR, VEL_EAR, ACC_EAR, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 1666, IUER, 'SUR_SKED_CONF', 'Failure in attempt '// &
     &         'to get baricentri coordinats of the Earth' )
           RETURN
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL PLANETA_DE_EPH ( VTD%DE_EPH, MJD_MID, TAI_MID, 'SUN', &
     &                      COO_SUN, VEL_SUN, ACC_SUN, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 1667, IUER, 'SUR_SKED_CONF', 'Failure in attempt '// &
     &         'to get baricentri coordinats of the Earth' )
           RETURN
      END IF
      COO_SUN = COO_SUN - COO_EAR
      CALL NORM_VEC ( 3, COO_SUN, RD_SUN )
!
! --- Put information of the amplitude calibrator to SUR%CAL
!
      SUR%L_CAL = 0
      DO 490 J9=1,L_CAL
         SUR%L_CAL = SUR%L_CAL + 1
         SUR%CAL(SUR%L_CAL)%J2000_NAME = 'C'//OBS_CAL(J9)%J2000_NAME(2:10)
         SUR%CAL(SUR%L_CAL)%B1950_NAME = OBS_CAL(J9)%B1950_NAME
         SUR%CAL(SUR%L_CAL)%ALPHA = OBS_CAL(J9)%ALP
         SUR%CAL(SUR%L_CAL)%DELTA = OBS_CAL(J9)%DEL
         SUR%CAL(SUR%L_CAL)%S_VEC(1) = OBS_CAL(J9)%S_VEC(1)
         SUR%CAL(SUR%L_CAL)%S_VEC(2) = OBS_CAL(J9)%S_VEC(2)
         SUR%CAL(SUR%L_CAL)%S_VEC(3) = OBS_CAL(J9)%S_VEC(3)
         SUR%CAL(SUR%L_CAL)%FL_USE   = .FALSE.
         CALL RH_TAT ( SUR%CAL(SUR%L_CAL)%ALPHA, 13, STR, -3 )
         CALL CHASHL ( STR(1:15) )
         SUR%CAL(SUR%L_CAL)%ALPHA_STR = STR(1:15)
         CALL RG_TAT ( SUR%CAL(SUR%L_CAL)%DELTA, 12, &
     &                 SUR%CAL(SUR%L_CAL)%DELTA_STR, -3 )
!
         SUN_DIST = DACOS ( DP_VV_V ( 3, SUR%CAL(SUR%L_CAL)%S_VEC(1), COO_SUN ) ) 
         IF ( SUN_DIST < SUR%SUN_DIST_MIN ) THEN
              IF ( IVRB .GE. 3 ) THEN
                   WRITE ( 6, 230 ) 'Calibrator', &
     &                              SUR%CAL(SUR%L_CAL)%J2000_NAME, &
     &                              SUN_DIST/DEG__TO__RAD
 230               FORMAT ( A, 1X, A, ' is too close to the Sun: ', &
     &                      F6.2, ' deg, so it is discarded' )
              END IF
              SUR%L_CAL = SUR%L_CAL - 1
              GOTO 490
         END IF
!
         IF ( SUR%ALGORITHM .NE. 'IMAGING_01'  .AND. &
     &        SUR%ALGORITHM .NE. 'IMAGING_S1'  .AND. &
     &        SUR%ALGORITHM .NE. 'GEODETIC_01' .AND. &
     &        SUR%ALGORITHM .NE. 'GEODETIC_02' .AND. &
     &        SUR%ALGORITHM .NE. 'GEODETIC_03'       ) THEN
!
! ----------- Check whether the calibrator source has the same name as 
! ----------- a target source
!
              DO 4100 J10=1,SUR%L_SOU
                 IF ( SUR%CAL(SUR%L_CAL)%J2000_NAME == SUR%SOU(J10)%J2000_NAME ) THEN
                      SUR%CAL(SUR%L_CAL)%J2000_NAME(10:10) = 'Z'
                 END IF
!
                 IF ( SUR%CAL(SUR%L_CAL)%B1950_NAME == SUR%SOU(J10)%B1950_NAME  ) THEN
                      SUR%CAL(SUR%L_CAL)%B1950_NAME(8:8) = 'Z'
                 END IF
 4100         CONTINUE 
!
              FL_FOUND = .FALSE.
              DO 4110 J11=1,LOBS_SOU 
                 DIST_COS = DP_VV_V ( 3, SUR%CAL(SUR%L_CAL)%S_VEC, &
     &                                   OBS_CAT(J11)%S_VEC )
                 IF ( DIST_COS > DCOS(SUR__DIST_LIM) ) THEN
                      FL_FOUND = .TRUE.
                 END IF
 4110         CONTINUE 
              IF ( FL_FOUND ) THEN
                   SUR%L_CAL = SUR%L_CAL - 1
                   GOTO 490
              END IF
         END IF
!
         LOBS_SOU = LOBS_SOU + 1
         OBS_CAT(LOBS_SOU)%S_VEC(1) = SUR%CAL(SUR%L_CAL)%S_VEC(1) 
         OBS_CAT(LOBS_SOU)%S_VEC(2) = SUR%CAL(SUR%L_CAL)%S_VEC(2) 
         OBS_CAT(LOBS_SOU)%S_VEC(3) = SUR%CAL(SUR%L_CAL)%S_VEC(3) 
 490  CONTINUE 
!
      CALL ERR_PASS   ( IUER, IER )
      CALL SUR_SOURCE ( SUR, VTD, SEC_SOURCE_FILE, SUR%L_SO2, SUR%SO2, &
     &                  LOBS_OBS, OBS_OBS, DUR_SEC, SUR%SCAN_LEN, &
     &                  SUR__TYP_SEC, IVRB, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 1668, IUER, 'SUR_SKED_CONF', 'Error in parsing '// &
     &         'secondary source catalogue '//SEC_SOURCE_FILE )
           RETURN 
      END IF
      LOBS_SOU = LOBS_SOU_SAVE 
!
      CALL NOUT_I4 ( SUR__M_SOU, SUR%IND_PAI )
      CALL NOUT    ( SUR__M_STA*SUR__M_SCN, SUR%OBS_STA )
!
      SUR%L_PAI = 0
      IF ( L_PAI > 0 ) THEN
           CALL ERR_PASS ( IUER, IER )
           CALL RD_TEXT  ( PAIR_SOURCE_FILE, M_BUF, BUF, N_BUF, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 1669, IUER, 'SUR_SKED_CONF', 'Error in '// &
     &              'attempt to read the file with source pairs '// &
     &               PAIR_SOURCE_FILE )
                RETURN 
           END IF
           DO 4120 J12=2,N_BUF
              IF ( BUF(J12)(1:1) == '#' ) GOTO 4120
!
              IND_SRC(1) = 0
              DO 4130 J13=1,L_PAI
                 IF ( OBS_PAI(J13)%J2000_NAME == BUF(J12)(13:20) ) THEN
                      IND_SRC(1) = J13
                 END IF
 4130         CONTINUE 
!
              IND_SRC(2) = 0
              DO 4140 J14=1,SUR%L_SOU 
                 IF ( SUR%SOU(J14)%J2000_NAME == BUF(J12)(91:100) ) THEN
                      IND_SRC(2) = J14
                 END IF
 4140         CONTINUE 
!
              IF ( IND_SRC(1) .LE. 0 ) THEN
                   CALL ERR_LOG ( 1670, IUER, 'SUR_SKED_CONF', 'Cannot '// &
     &                 'find the pair source '//BUF(J12)(11:20)//' the '// &
     &                 'second time in file '//PAIR_SOURCE_FILE )
                   RETURN 
              END IF
!
              IF ( IND_SRC(2) .LE. 0 ) THEN
                   CALL ERR_LOG ( 1671, IUER, 'SUR_SKED_CONF', 'Cannot '// &
     &                 'find the pair source '//BUF(J12)(91:100)//' '// &
     &                 'in file '//PAIR_SOURCE_FILE )
                   RETURN 
              END IF
!
              SUR%IND_PAI(IND_SRC(2)) = IND_SRC(1)
!
              SUR%L_PAI = SUR%L_PAI + 1
              SUR%PAI(SUR%L_PAI)%J2000_NAME = OBS_PAI(IND_SRC(1))%J2000_NAME  
              SUR%PAI(SUR%L_PAI)%B1950_NAME = OBS_PAI(IND_SRC(1))%B1950_NAME
              SUR%PAI(SUR%L_PAI)%ALPHA = OBS_PAI(IND_SRC(1))%ALP
              SUR%PAI(SUR%L_PAI)%DELTA = OBS_PAI(IND_SRC(1))%DEL
              SUR%PAI(SUR%L_PAI)%S_VEC(1) = OBS_PAI(IND_SRC(1))%S_VEC(1)
              SUR%PAI(SUR%L_PAI)%S_VEC(2) = OBS_PAI(IND_SRC(1))%S_VEC(2)
              SUR%PAI(SUR%L_PAI)%S_VEC(3) = OBS_PAI(IND_SRC(1))%S_VEC(3)
              SUR%PAI(SUR%L_PAI)%FL_USE   = .FALSE.
              CALL RH_TAT ( SUR%PAI(SUR%L_PAI)%ALPHA, 13, STR, -3 )
              CALL CHASHL ( STR(1:15) )
              SUR%PAI(SUR%L_PAI)%ALPHA_STR = STR(1:15)
              CALL RG_TAT ( SUR%PAI(SUR%L_PAI)%DELTA, 12, &
     &                 SUR%PAI(SUR%L_PAI)%DELTA_STR, -3 )
 4120      CONTINUE 
      END IF
!
      IF ( SUR%L_SOU > 0 .AND. LOBS_OBS > 0 ) THEN
           DO 4150 J15=1,SUR%L_SOU
              SUR%SOU(J15)%NOBS = 0
              DO 4160 J16=1,LOBS_OBS
                 IF ( SUR%SOU(J15)%J2000_NAME == OBS_OBS(J16)%J2000_NAME ) THEN
                      SUR%SOU(J15)%NOBS = OBS_OBS(J16)%NOBS_TOTAL
                      SUR%NOBS_SRC_ORIG(J15) = OBS_OBS(J16)%NOBS_TOTAL
                 END IF
 4160         CONTINUE 
              IF ( IVRB .GE. 5 ) THEN
                   WRITE ( 6, '(I4,") sou: ", A, " nobs: ",I3, " nsca_max: ", I3)' ) J15, SUR%SOU(J15)%J2000_NAME, &
     &                                                                               SUR%SOU(J15)%NOBS, SUR%SOU(J15)%NSCA_MAX
              END IF 
 4150      CONTINUE 
      END IF
      IF ( IVRB .GE. 22 ) THEN
           WRITE ( 6, * ) 'LOBS_OBS= ', LOBS_OBS
           WRITE ( 6, * ) 'OBS_OBS-1 = ', OBS_OBS(1)%J2000_NAME, OBS_OBS(1)%NOBS_TOTAL
           WRITE ( 6, * ) 'OBS_OBS-2 = ', OBS_OBS(2)%J2000_NAME, OBS_OBS(2)%NOBS_TOTAL
      END IF
!
      IF ( SUR%L_SO2 > 0 .AND. LOBS_OBS > 0 ) THEN
           DO 4170 J17=1,SUR%L_SO2
              SUR%SO2(J17)%NOBS = 0
              DO 4180 J18=1,LOBS_OBS
                 IF ( SUR%SO2(J17)%J2000_NAME == OBS_OBS(J18)%J2000_NAME ) THEN
                      SUR%SO2(J17)%NOBS = OBS_OBS(J18)%NOBS_TOTAL
                      SUR%NOBS_SO2_ORIG(J17) = OBS_OBS(J18)%NOBS_TOTAL
                      SUR%NOBS_SO2(J17)      = OBS_OBS(J18)%NOBS_TOTAL
                 END IF
 4180         CONTINUE 
              IF ( IVRB .GE. 5 ) THEN
                   WRITE ( 6, '(I4,") so2: ", A, " nobs: ",I3, " nobs_so2: ",I3, " nsca_max: ", I3)' ) J17, &
     &                    SUR%SO2(J17)%J2000_NAME, SUR%SO2(J17)%NOBS, SUR%NOBS_SO2(J17), &
     &                    SUR%SO2(J17)%NSCA_MAX
              END IF 
 4170      CONTINUE 
      END IF
!
      IF ( SUR%POCAL_STYLE == POCAL_STYLE__GBT_4HR ) THEN
           CALL ERR_PASS ( IUER, IER )
           CALL READ_SOU ( SUR__POCAL_FIL, SUR__M_SOU, L_SOP, OBS_SOP, &
     &                     COBS_POC, MODE, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 1672, IUER, 'SUR_SKED_CONF', 'Error in '// &
     &              'reading file with the catalogue of the sources suitable '// &
     &              'for being used as pointing calibration' )
               RETURN 
          END IF
!
          SUR%L_SOP = 0
          DO 4200 J20=1,L_SOP
              SUR%L_SOP = SUR%L_SOP + 1
              SUR%SOP(SUR%L_SOP)%J2000_NAME = OBS_SOP(J20)%J2000_NAME  
              SUR%SOP(SUR%L_SOP)%B1950_NAME = OBS_SOP(J20)%B1950_NAME
              SUR%SOP(SUR%L_SOP)%ALPHA = OBS_SOP(J20)%ALP
              SUR%SOP(SUR%L_SOP)%DELTA = OBS_SOP(J20)%DEL
              SUR%SOP(SUR%L_SOP)%S_VEC(1) = OBS_SOP(J20)%S_VEC(1)
              SUR%SOP(SUR%L_SOP)%S_VEC(2) = OBS_SOP(J20)%S_VEC(2)
              SUR%SOP(SUR%L_SOP)%S_VEC(3) = OBS_SOP(J20)%S_VEC(3)
              SUR%SOP(SUR%L_SOP)%FL_USE   = .FALSE.
              CALL RH_TAT ( SUR%SOP(SUR%L_SOP)%ALPHA, 13, STR, -3 )
              CALL CHASHL ( STR(1:15) )
              SUR%SOP(SUR%L_SOP)%ALPHA_STR = STR(1:15)
              CALL RG_TAT ( SUR%SOP(SUR%L_SOP)%DELTA, 12, &
     &                 SUR%SOP(SUR%L_SOP)%DELTA_STR, -3 )
!
              SUN_DIST = DACOS ( DP_VV_V ( 3, SUR%SOP(SUR%L_SOP)%S_VEC(1), COO_SUN ) ) 
              IF ( SUN_DIST < SUR%SUN_DIST_MIN ) THEN
                   IF ( IVRB .GE. 3 ) THEN
                        WRITE ( 6, 230 ) 'Pocal_calib', &
     &                                   SUR%SOP(SUR%L_SOP)%J2000_NAME, &
     &                                   SUN_DIST/DEG__TO__RAD
                   END IF
                   SUR%L_SOP = SUR%L_SOP - 1
                   GOTO 4200
              END IF
 4200     CONTINUE 
      END IF
      IF ( INDEX ( SUR%HARDWARE_SETUP_NAME, ',' ) > 0 ) THEN
           CALL EXWORD ( SUR%HARDWARE_SETUP_NAME, MIND, SUR%L_HDS, IND, CHAR(32)//',', -3 )
           DO 4210 J21=1,SUR%L_HDS
              SUR%HARDWARE_SETUP_NAMS(J21) = SUR%HARDWARE_SETUP_NAME(IND(1,J21):IND(2,J21))
 4210      CONTINUE 
         ELSE
           SUR%L_HDS = 1
           SUR%HARDWARE_SETUP_NAMS(1) = SUR%HARDWARE_SETUP_NAME
      END IF
!
      IF ( IVRB .GE. 3 ) THEN
           WRITE ( 6, * ) ' SUR%L_STA = ', SUR%L_STA 
           WRITE ( 6, * ) ' SUR%L_SOU = ', SUR%L_SOU
           WRITE ( 6, * ) ' SUR%L_SO2 = ', SUR%L_SO2
           WRITE ( 6, * ) ' SUR%L_CAL = ', SUR%L_CAL
           WRITE ( 6, * ) ' SUR%L_PAI = ', SUR%L_PAI
           WRITE ( 6, * ) ' SUR%L_SOP = ', SUR%L_SOP
           WRITE ( 6, * ) ' SUR%L_PLA = ', SUR%L_PLA
           WRITE ( 6, * ) ' SUR%N_GAP = ', SUR%N_GAP
           IF ( IVRB .GE. 6 .AND. SUR%N_GAP > 0 ) THEN
                WRITE ( 6, * ) '              MJD_START     = ', SUR%MJD_START, &
     &                         ' TAI_START=      ', SNGL(SUR%TAI_START)
                DO 4220 J22=1,SUR%N_GAP
                   WRITE ( 6, * ) 'Gap: ', INT2(J22), ' MJD_GAP_START = ', SUR%MJD_GAP(1,J22),       &
     &                                                ' TAI_GAP_START = ', SNGL(SUR%TAI_GAP(1,J22)), &
     &                                                ' MJD_GAP_STOP  = ', SUR%MJD_GAP(2,J22),       &
     &                                                ' TAI_GAP_STOP  = ', SNGL(SUR%TAI_GAP(1,J22))   
 4220           CONTINUE 
           END IF
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  SUR_SKED_CONF !#!  
