      SUBROUTINE SUR_PRINT_RES ( SUR, VTD, IVRB, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine SUR_PRINT_RES
! *                                                                      *
! * ### 11-OCT-2005  SUR_PRINT_RES v2.38 (c) L. Petrov  12-FEB-2024 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'astro_constants.i'
      INCLUDE   'sur_sked.i'
      INCLUDE   'vtd.i'
      TYPE     ( SUR__TYPE ) :: SUR
      TYPE     ( VTD__TYPE ) :: VTD
      INTEGER*4  IVRB, IUER
      INTEGER*4    MBUF, MIND
      PARAMETER  ( MBUF = 128*1024 )
      PARAMETER  ( MIND =   32 )
      CHARACTER  STR*4096, J2000_NAME*10, B1950_NAME*8, PAIR_NAME*8, OBJECT*7, &
     &           BUF_VEX_HEAD(MBUF)*8192, BUF_KEY_HEAD(MBUF)*512, &
     &           BUF_TAG(MBUF)*512, BUF_CAL(MBUF)*512, &
     &           WRAP_STR*4, STR_VEX_DATE*17, STR_KEY_DATE*17, &
     &           STR_VEX_DATE_ORIGSTART*17, STR_POCAL_VEX_DATE*17, &
     &           STR_VEX_END_DATE*17, STR_DOY*3, STR_DOY_END*3, &
     &           BUF_FLX(MBUF)*64, STR_KEY_SCAN*4096, STR_KEY_STATIONS*4096, &
     &           OBJECT_STR*32, STR_VEX_START*17, STR_VEX_STOP*17, &
     &           MODE_START*128, MODE_SHORT*128, MODE_LONG*128, &
     &           MODE_TAPE_CHANGE*128, MODE_FINAL*128, MODE_POINTING*128, &
     &           MODE_FLUX_CALIB*128, STR_DUR*32, SCAN_STR_AST*6, &
     &           STR_KEY_START*17, STR_KEY_STOP*17, STR_DWELL*5, PRESESS_STR*8, &
     &           HDS_PAT*16, STA_NAM*8, STA_LST*8
      CHARACTER  AST_BUF(MBUF)*512
!
      REAL*8     ACC_DATA(SUR__M_STA), TIME_SLEW, DUR_SA,  DUR_SCAN, &
     &           AZ, EL, HA, DIF_AZ, DIF_EL, SCAN_LEN, LST_PT, REC_TIME, TAI_REC, &
     &           AZ_WRAP, DUR, DUR_FUDGE, DUR_SLEW, DUR_RECORD, &
     &           SLEW_1ST, TIME_ANT_SLEW(SUR__M_STA), S_ANG, RA, DEC, &
     &           SLEW_TIME, TAI_CUR_SAVE, UTC_START_PREOBS, &
     &           UTC_START_RECORD, UTC_STOP_RECORD, SEC, SEC_NOW, &
     &           SLEW_ARR(SUR__M_STA), SLEW_2_STA, SLEW_3_STA, &
     &           SCA_START_OFF, STA_START_OFF, SCA_STA_DUR, NOM_SLEW_TIME, &
     &           NOM_SCA_LEN, ADJ_SLEW_TIME, SLEW_STA_SRT(SUR__M_STA), &
     &           UTC_ADJ_START_RAW, UTC_ADJ_START, TIM, &
     &           SLEW_AZ, SLEW_EL, SLEW_DEL, SLEW_HA, &
     &           SLEW_A, SLEW_B, AZIM_180_LIM
      REAL*8     SUR__SHARE_SA, ROU_EPS, AZ__MARGIN
      PARAMETER  ( SUR__SHARE_SA = 0.67D0 )
      PARAMETER  ( ROU_EPS = 0.01D0 )
      PARAMETER  ( AZ__MARGIN = 0.25D0*DEG__TO__RAD )
      REAL*8     ALP, DEL, DIF_DEL, DIF_HA, EL_OBS_PREV, &
     &           AZ_ACC_OBS_PREV, HA_ACC_OBS_PREV, AZ_OLD, PAR_ANG
      INTEGER*4  J1, J2, J3, J4, J5, J6, J7, J8, J9, J10, J11, J12, J13, &
     &           J14, J15, J16, J17, J18, J19, J20, J21, J22, J23, J24, J25, &
     &           IOS, IS_TAG, IS_CAL, IP, DOY, NSL, LIND, IND(2,MIND), &
     &           L_MODE, IND_MODE(2,MIND), L_MDSC, IND_MDSC(2,MIND), &
     &           K1, K2, INDM, LUN_PLAN, LUN_VEX, LUN_KEY, LUN_AST, &
     &           NVEX_HEAD, NKEY_HEAD, LAST_SCA(SUR__M_STA), &
     &           IND_TAP, N_FLX, IL, K_STA, MJD_REC, L_SCN, MJD_CUR_SAVE, &
     &           MJD_ADJ_START, MJD, MJD_NOW, L_SCN_SAVE, KS_STA, SPL_STATUS, NA, &
     &           IND_STA, IER
      CHARACTER  USER_NAME*128, USER_REALNAME*128, USER_E_ADDRESS*128, WORD*4, &
     &           DISK_STR*9, STA_CODE(SUR__M_STA)*1, STA_STR*(SUR__M_STA*2), &
     &           STR_NUM_STA*2, STR_POCAL*19, STR1*30, STR2*30, &
     &           ALPHA_STR*15, DELTA_STR*15, TYPE_STR*10, SOU_CUR*10, &
     &           SOU_BPRE*8, SOU_PREV*10, AXIS_NAME(2)*12, GB_STR*9
      LOGICAL*4  FL_FOUND, FL_STA_USED(SUR__M_STA), FL_LBA_KEY, FL_VLBA_KEY, &
     &           FL_CVN_KEY, FL_EVN_KEY, FL_STA(SUR__M_STA), FL_PREV, &
     &           FL_SITE, FL_ANTENNA, FL_EXT, FL_CLOCK, FL_VEX_V2, FL_MODE_DEF
      REAL*8,    EXTERNAL :: SUR_SLEW_TIME, GET_LST, ATAN_CS
      CHARACTER, EXTERNAL :: MJDSEC_TO_DATE*30, MJDSEC_TO_VEX*22, GET_CDATE*19
      LOGICAL*4, EXTERNAL :: SUR_CHECK_VIS
      INTEGER*4, EXTERNAL :: GET_UNIT, ILEN, I_LEN, LTM_DIF
      REAL*8,    EXTERNAL :: UPROUND
!
      IF ( SUR%L_SCN == 0 ) THEN
           CALL ERR_LOG ( 1511, IUER, 'SUR_PRINT_RES', 'No scans were '// &
     &         'scheduled: nothing to write down' )
           RETURN 
      END IF
!
      CALL EXWORD ( SUR%OBSERVING_MODE_NAME,  MIND, L_MODE, IND_MODE, ",", IER )
      CALL EXWORD ( SUR%OBSERVING_MODE_DESCR, MIND, L_MDSC, IND_MDSC, ",", IER )
!
      STA_CODE = '@@@@@@@@@@@@@@@@@@@@'
      FL_LBA_KEY  = .FALSE.
      FL_EVN_KEY  = .FALSE.
      FL_VLBA_KEY = .FALSE.
      FL_CVN_KEY  = .FALSE.
      DUR_FUDGE   = 0.0D0
      SUR%CDATE_STOP = GET_CDATE()
!
      CALL GETENVAR ( 'SUR_SKED_180_AZIM_CHECK', STR )
      IF ( ILEN(STR) .NE. 0 ) THEN
           READ ( UNIT=STR, FMT=*, IOSTAT=IER ) AZIM_180_LIM
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 1512, IUER, 'SUR_PRINT_RES', 'Error in '// &
     &              'parsing environment variable SUR_SKED_180_AZIM_CHECK: '// &
     &              '-- a real number was expected, but got '//STR )
                RETURN
           END IF
           AZIM_180_LIM = AZIM_180_LIM*DEG__TO__RAD
           CALL SUR_KNOCK_OUT_180_AZIM ( SUR, AZIM_180_LIM, IVRB )
     END IF
!
      LUN_PLAN = GET_UNIT()
      OPEN ( UNIT=LUN_PLAN, FILE=SUR%OUT_PLAN, STATUS='UNKNOWN', IOSTAT=IOS )
      IF ( IOS .NE. 0 ) THEN
           CALL ERR_LOG ( 1513, IUER, 'SUR_PRINT_RES', 'Error in an attempt '// &
     &         'to open output file '//SUR%OUT_PLAN )
           RETURN
      END IF
!
      LUN_VEX = GET_UNIT()
      OPEN ( UNIT=LUN_VEX, FILE=SUR%OUT_VEX, STATUS='UNKNOWN', IOSTAT=IOS )
      IF ( IOS .NE. 0 ) THEN
           CALL ERR_LOG ( 1514, IUER, 'SUR_PRINT_RES', 'Error in an attempt '// &
     &         'to open output file '//SUR%OUT_VEX )
           RETURN
      END IF
!
      LUN_KEY = GET_UNIT()
      OPEN ( UNIT=LUN_KEY, FILE=SUR%OUT_KEY, STATUS='UNKNOWN', IOSTAT=IOS )
      IF ( IOS .NE. 0 ) THEN
           CALL ERR_LOG ( 1515, IUER, 'SUR_PRINT_RES', 'Error in an attempt '// &
     &         'to open output file '//SUR%OUT_KEY )
           RETURN
      END IF
!
      LUN_AST = GET_UNIT()
      OPEN ( UNIT=LUN_AST, FILE=SUR%OUT_AST, STATUS='UNKNOWN', IOSTAT=IOS )
      IF ( IOS .NE. 0 ) THEN
           CALL ERR_LOG ( 1516, IUER, 'SUR_PRINT_RES', 'Error in an attempt '// &
     &         'to open output file '//SUR%OUT_AST )
           RETURN
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL RD_TEXT  ( SUR%HEADER_VEX_TEMPLATE_FILE, MBUF, BUF_VEX_HEAD, &
     &                NVEX_HEAD, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 1517, IUER, 'SUR_PRINT_RES', 'Error in an attempt '// &
     &         'to read vex template header file '//SUR%HEADER_VEX_TEMPLATE_FILE )
           RETURN
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL RD_TEXT  ( SUR%HEADER_KEY_TEMPLATE_FILE, MBUF, BUF_KEY_HEAD, &
     &                NKEY_HEAD, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 1518, IUER, 'SUR_PRINT_RES', 'Error in an attempt '// &
     &         'to read key template header file '//SUR%HEADER_KEY_TEMPLATE_FILE )
           RETURN
      END IF
!
      CALL GETINFO_USER ( USER_NAME, USER_REALNAME, USER_E_ADDRESS )
!
      STR_VEX_START    = '?????????????????'
      MODE_START       = 'GEO1K'
      MODE_SHORT       = 'GEO1K_SHORT'
      MODE_LONG        = 'GEO1K'
      MODE_TAPE_CHANGE = 'GEO1K_TC'
      MODE_FINAL       = 'GEO1K'
      MODE_POINTING    = 'GEO1K'
      MODE_FLUX_CALIB  = 'GEO1K'
      FL_MODE_DEF      = .FALSE.
!
      MODE_START       = SUR%HARDWARE_SETUP_NAMS(1)
      MODE_SHORT       = SUR%HARDWARE_SETUP_NAMS(1)
      MODE_LONG        = SUR%HARDWARE_SETUP_NAMS(1)
      MODE_TAPE_CHANGE = SUR%HARDWARE_SETUP_NAMS(1)
      MODE_FINAL       = SUR%HARDWARE_SETUP_NAMS(1)
      MODE_POINTING    = SUR%HARDWARE_SETUP_NAMS(1)
      MODE_FLUX_CALIB  = SUR%HARDWARE_SETUP_NAMS(1)
!
      NA = 0
      NA = NA + 1 ; WRITE ( UNIT=AST_BUF(NA), FMT='(A)' ) SUR__AST_FORMAT
      NA = NA + 1 ; WRITE ( UNIT=AST_BUF(NA), FMT='(A)' ) '#'
      NA = NA + 1 ; WRITE ( UNIT=AST_BUF(NA), FMT='(A)' ) '# Generated on          '//GET_CDATE()
      NA = NA + 1 ; WRITE ( UNIT=AST_BUF(NA), FMT='(A)' ) '# Generated by:         '//USER_REALNAME
      NA = NA + 1 ; WRITE ( UNIT=AST_BUF(NA), FMT='(A)' ) '# Generated with:       '//SUR__LABEL
      NA = NA + 1 ; WRITE ( UNIT=AST_BUF(NA), FMT='(A)' ) '#'
      NA = NA + 1 ; WRITE ( UNIT=AST_BUF(NA), FMT='(A)' ) 'Experiment:                 '//TRIM(SUR%EXP_CODE)
      NA = NA + 1 ; WRITE ( UNIT=AST_BUF(NA), FMT='(A)' ) '  Experiment_description:   '//SUR%EXP_CODE(1:8)//'  '//TRIM(SUR%EXP_DESCR)
      NA = NA + 1 ; WRITE ( UNIT=AST_BUF(NA), FMT='(A)' ) '  Schedule_revision:        '//SUR%EXP_CODE(1:8)//'  '//TRIM(SUR%SCHEDULE_REVISION)
      NA = NA + 1 ; WRITE ( UNIT=AST_BUF(NA), FMT='(A)' ) '  PI_name:                  '//SUR%EXP_CODE(1:8)//'  '//TRIM(SUR%PI_NAME)
      NA = NA + 1 ; WRITE ( UNIT=AST_BUF(NA), FMT='(A)' ) '  Scheduler_name:           '//SUR%EXP_CODE(1:8)//'  '//TRIM(SUR%SCHEDULER_NAME)
      NA = NA + 1 ; WRITE ( UNIT=AST_BUF(NA), FMT='(A)' ) '  Scheduler_email:          '//SUR%EXP_CODE(1:8)//'  '//TRIM(SUR%SCHEDULER_EMAIL)
      NA = NA + 1 ; WRITE ( UNIT=AST_BUF(NA), FMT='(A)' ) '  Scheduler_phone:          '//SUR%EXP_CODE(1:8)//'  '//TRIM(SUR%SCHEDULER_PHONE)
      NA = NA + 1 ; WRITE ( UNIT=AST_BUF(NA), FMT='(A)' ) '  Observer_phone:           '//SUR%EXP_CODE(1:8)//'  '//TRIM(SUR%OBSERVER_PHONE)
      STR1 = MJDSEC_TO_DATE ( SUR%MJD_UTC_START, SUR%UTC_START, IER )
      STR2 = MJDSEC_TO_DATE ( SUR%MJD_UTC_STOP, SUR%UTC_STOP, IER )
      NA = NA + 1 ; WRITE ( UNIT=AST_BUF(NA), FMT='(A)' ) '  UTC_experiment_dates:     '//SUR%EXP_CODE(1:8)//'  '// &
     &                              STR1(1:21)//' '//STR2(1:21)
      WRITE ( UNIT=STR1, FMT='(F8.4)' ) 1.0D-6*SUR%CORR_SPECTRAL_RESOLUTION
      CALL CHASHL ( STR1 )
      NA = NA + 1 ; WRITE ( UNIT=AST_BUF(NA), FMT='(A,A,A)'  ) '  Corr_spectral_resolution: '//SUR%EXP_CODE(1:8)//'  ', &
     &                                                            TRIM(STR1), '  MHz'
      WRITE ( UNIT=STR1, FMT='(F8.3)' ) SUR%CORR_TIME_RESOLUTION
      CALL CHASHL ( STR1 )
      NA = NA + 1 ; WRITE ( UNIT=AST_BUF(NA), FMT='(A,A,A)'  ) '  Corr_time_resolution:     '//SUR%EXP_CODE(1:8)//'  ', &
     &                                  TRIM(STR1), '   sec'
!
      FL_SITE = .FALSE.
      FL_ANTENNA = .FALSE.
      IND_STA = 0
      FL_EXT  = .FALSE.
      DO 410 J1=1,NVEX_HEAD
         IF ( J1 == 1 ) THEN
              IF ( INDEX ( BUF_VEX_HEAD(J1), "1.5" ) > 0 ) THEN
                   FL_VEX_V2 = .FALSE.
                 ELSE
                   FL_VEX_V2 = .TRUE.
              END IF
         END IF
         CALL EXWORD ( BUF_VEX_HEAD(J1), MIND, LIND, IND, &
     &                 CHAR(32)//CHAR(9)//',;', IER )
         DO 510 K1=1,16
            IP = INDEX ( BUF_VEX_HEAD(J1), '__@EXP_CODE@__' )
            IF ( IP > 0 ) THEN
                 BUF_VEX_HEAD(J1) = BUF_VEX_HEAD(J1)(1:IP-1)// &
     &                       SUR%EXP_CODE(1:I_LEN(SUR%EXP_CODE))// &
     &                       BUF_VEX_HEAD(J1)(IP+ILEN('__@EXP_CODE@__'):)
            END IF
 510     CONTINUE 
!
         IP = INDEX ( BUF_VEX_HEAD(J1), '__@EXP_DESCR@__' )
         IF ( IP > 0 ) THEN
              BUF_VEX_HEAD(J1) = BUF_VEX_HEAD(J1)(1:IP-1)// &
     &                       SUR%EXP_DESCR(1:I_LEN(SUR%EXP_DESCR))// &
     &                       BUF_VEX_HEAD(J1)(IP+ILEN('__@EXP_DESCR@__'):)
         END IF
!
         IP = INDEX ( BUF_VEX_HEAD(J1), '__@SCHEDULE_REVISION@__' )
         IF ( IP > 0 ) THEN
              BUF_VEX_HEAD(J1) = BUF_VEX_HEAD(J1)(1:IP-1)// &
     &                       SUR%SCHEDULE_REVISION(1:I_LEN(SUR%SCHEDULE_REVISION))// &
     &                       BUF_VEX_HEAD(J1)(IP+ILEN('__@SCHEDULE_REVISION@__'):)
         END IF
!
         IP = INDEX ( BUF_VEX_HEAD(J1), '__@PI_NAME@__' )
         IF ( IP > 0 ) THEN
              BUF_VEX_HEAD(J1) = BUF_VEX_HEAD(J1)(1:IP-1)// &
     &                       SUR%PI_NAME(1:I_LEN(SUR%PI_NAME))// &
     &                       BUF_VEX_HEAD(J1)(IP+ILEN('__@PI_NAME@__'):)
         END IF
!
         IP = INDEX ( BUF_VEX_HEAD(J1), '__@SCHEDULER_NAME@__' )
         IF ( IP > 0 ) THEN
              BUF_VEX_HEAD(J1) = BUF_VEX_HEAD(J1)(1:IP-1)// &
     &                       SUR%SCHEDULER_NAME(1:I_LEN(SUR%SCHEDULER_NAME))// &
     &                       BUF_VEX_HEAD(J1)(IP+ILEN('__@SCHEDULER_NAME@__'):)
         END IF
!
         IP = INDEX ( BUF_VEX_HEAD(J1), '__@SCHEDULER_EMAIL@__' )
         IF ( IP > 0 ) THEN
              BUF_VEX_HEAD(J1) = BUF_VEX_HEAD(J1)(1:IP-1)// &
     &                       SUR%SCHEDULER_EMAIL(1:I_LEN(SUR%SCHEDULER_EMAIL))// &
     &                       BUF_VEX_HEAD(J1)(IP+ILEN('__@SCHEDULER_EMAIL@__'):)
         END IF
!
         IP = INDEX ( BUF_VEX_HEAD(J1), '__@CORR_TIME_RESOLUTION@__' )
         IF ( IP > 0 ) THEN
              WRITE ( UNIT=STR1, FMT='(F8.3)' ) SUR%CORR_TIME_RESOLUTION
              CALL CHASHL ( STR1 )
              BUF_VEX_HEAD(J1) = BUF_VEX_HEAD(J1)(1:IP-1)//TRIM(STR1)// &
     &                           BUF_VEX_HEAD(J1)(IP+ILEN('__@SUR%CORR_TIME_RESOLUTION@__'):)

         END IF
!
         IP = INDEX ( BUF_VEX_HEAD(J1), '__@DATE_START@__' )
         IF ( IP > 0 ) THEN
              STR = MJDSEC_TO_VEX( SUR%MJD_UTC_START, SUR%UTC_START, -3 )
              BUF_VEX_HEAD(J1) = BUF_VEX_HEAD(J1)(1:IP-1)//STR(1:17)//'s'// &
     &                 BUF_VEX_HEAD(J1)(IP+ILEN('__@DATE_START@__'):)
         END IF
!
         IP = INDEX ( BUF_VEX_HEAD(J1), '__@DATE_STOP@__' )
         IF ( IP > 0 ) THEN
              STR = MJDSEC_TO_VEX( SUR%MJD_UTC_STOP, SUR%UTC_STOP, -3 )
              BUF_VEX_HEAD(J1) = BUF_VEX_HEAD(J1)(1:IP-1)//STR(1:17)//'s'// &
     &                 BUF_VEX_HEAD(J1)(IP+ILEN('__@DATE_STOP@__'):)
         END IF
!
         IP = INDEX ( BUF_VEX_HEAD(J1), '__@DATE_GEN@__' )
         IF ( IP > 0 ) THEN
              BUF_VEX_HEAD(J1) = BUF_VEX_HEAD(J1)(1:IP-1)//GET_CDATE()
              BUF_VEX_HEAD(J1)(79:79) = '#'
         END IF
!
         IP = INDEX ( BUF_VEX_HEAD(J1), '__@GEN_NAME@__' )
         IF ( IP > 0 ) THEN
              BUF_VEX_HEAD(J1) = BUF_VEX_HEAD(J1)(1:IP-1)//USER_REALNAME
              BUF_VEX_HEAD(J1)(79:79) = '#'
         END IF
!
         IP = INDEX ( BUF_VEX_HEAD(J1), '__@GEN_PROGNAME@__' )
         IF ( IP > 0 ) THEN
              BUF_VEX_HEAD(J1) = BUF_VEX_HEAD(J1)(1:IP-1)//SUR__LABEL
              BUF_VEX_HEAD(J1)(79:79) = '#'
         END IF
!
         IP = INDEX ( BUF_VEX_HEAD(J1), '__@NOMINAL_START@__' )
         IF ( IP > 0 ) THEN
              BUF_VEX_HEAD(J1) = BUF_VEX_HEAD(J1)(1:IP-1)// &
     &                 MJDSEC_TO_DATE( SUR%MJD_UTC_START, SUR%UTC_START, -3 )// &
     &                 BUF_VEX_HEAD(J1)(IP+ILEN('__@NOMINAL_START@__'):)
         END IF
!
         IP = INDEX ( BUF_VEX_HEAD(J1), '__@DURATION@__' )
         IF ( IP > 0 ) THEN
              WRITE ( UNIT=STR(1:6), FMT='(F6.2)' ) &
     &                ( (SUR%MJD_STOP - SUR%MJD_START)*86400.0D0 + &
     &                  (SUR%TAI_STOP - SUR%TAI_START) )/3600.0D0
              BUF_VEX_HEAD(J1) = BUF_VEX_HEAD(J1)(1:IP-1)//STR(1:6)// &
     &                       BUF_VEX_HEAD(J1)(IP+ILEN('__@DURATION@__'):)
         END IF
         DO 520 K1=1,SUR%L_HDS
            IF ( SUR%L_HDS == 1 ) THEN
                 HDS_PAT = '__@HDS@__'
               ELSE
                 CALL INCH ( K1, STR )
                 HDS_PAT = '__@HDS'//TRIM(STR)//'@__'
            END IF
            DO 522 K2=1,16
               IP = INDEX ( BUF_VEX_HEAD(J1), TRIM(HDS_PAT) )
               IF ( IP > 0 ) THEN
                    BUF_VEX_HEAD(J1) = BUF_VEX_HEAD(J1)(1:IP-1)// &
     &                                 TRIM(SUR%HARDWARE_SETUP_NAMS(K1))// &
     &                                 BUF_VEX_HEAD(J1)(IP+ILEN(HDS_PAT):)
               END IF
!
               IP = INDEX ( BUF_VEX_HEAD(J1), '__@OMN@__' )
               IF ( IP > 0 ) THEN
                    BUF_VEX_HEAD(J1) = BUF_VEX_HEAD(J1)(1:IP-1)// &
     &                                 TRIM(SUR%OBSERVING_MODE_NAME)// &
     &                                 BUF_VEX_HEAD(J1)(IP+ILEN('__@OMN@__'):)
               END IF
 522        CONTINUE 
 520     CONTINUE 
         IP = INDEX ( BUF_VEX_HEAD(J1), '__@EXP_REVISION@__' )
         IF ( IP > 0 ) THEN
              BUF_VEX_HEAD(J1) = BUF_VEX_HEAD(J1)(1:IP-1)// &
     &                           TRIM(SUR%SCHEDULE_REVISION)// &
     &                           BUF_VEX_HEAD(J1)(IP+ILEN('__@EXP_REVISION@__'):)
         END IF
!
         IP = INDEX ( BUF_VEX_HEAD(J1), '__@SUR_SKED_VERSION@__' )
         IF ( IP > 0 ) THEN
              BUF_VEX_HEAD(J1) = BUF_VEX_HEAD(J1)(1:IP-1)// &
     &                           SUR__LABEL(23:LEN(SUR__LABEL))// &
     &                           BUF_VEX_HEAD(J1)(IP+ILEN('__@SUR_SKED_VERSION@__'):)
         END IF
         CALL DATE_TO_TIME ( GET_CDATE(), MJD_NOW, SEC_NOW, IER )
         STR_VEX_DATE = MJDSEC_TO_VEX ( MJD_NOW, SEC_NOW, IER )
!
         IP = INDEX ( BUF_VEX_HEAD(J1), '__@GEN_DATE@__' )
         IF ( IP > 0 ) THEN
              BUF_VEX_HEAD(J1) = BUF_VEX_HEAD(J1)(1:IP-1)// &
     &                           STR_VEX_DATE// &
     &                           BUF_VEX_HEAD(J1)(IP+ILEN('__@GEN_DATE@__'):)
         END IF
!
         DO 525 K2=1,7
            IP = INDEX ( BUF_VEX_HEAD(J1), '__@MOD1@__' )
            IF ( IP > 0 ) THEN
                 STR = SUR%OBSERVING_MODE_NAME(IND_MODE(1,1):IND_MODE(2,1))
                 BUF_VEX_HEAD(J1) = BUF_VEX_HEAD(J1)(1:IP-1)// &
     &                              TRIM(STR)// &
     &                              BUF_VEX_HEAD(J1)(IP+ILEN('__@MOD1@__'):)
            END IF
 525     CONTINUE 
         IP = INDEX ( BUF_VEX_HEAD(J1), 'setmode___@MOD' )
         IF ( IP > 1 ) THEN
              CALL CHIN ( BUF_VEX_HEAD(J1)(IP+14:IP+14), INDM )
              IF ( INDM .LE. L_MODE ) THEN
                   STR = SUR%OBSERVING_MODE_NAME(IND_MODE(1,INDM):IND_MODE(2,INDM))
                   IP = INDEX ( BUF_VEX_HEAD(J1), '__@MOD' )
                   BUF_VEX_HEAD(J1) = BUF_VEX_HEAD(J1)(1:IP-1)// &
     &                                TRIM(STR)// &
     &                                BUF_VEX_HEAD(J1)(IP+ILEN('__@MOD')+4:)
                 ELSE
                   GOTO 410
              END IF
         END IF
!
         IF ( BUF_VEX_HEAD(J1)(1:22) == '* __MODE_Definitions__' ) THEN
              CALL EXWORD ( BUF_VEX_HEAD(J1), MIND, LIND, IND, &
     &                      CHAR(32)//CHAR(9)//',;', IER )
              IF ( LIND < 7 ) THEN
                   CALL ERR_LOG ( 1519, IUER, 'SUR_PRINT_RES', &
     &                 'Line '//BUF_VEX_HEAD(J1)(1:I_LEN(BUF_VEX_HEAD(J1)))// &
     &                 ' should have at least 7 words' )
                   RETURN 
              END IF
              IF ( .NOT. FL_MODE_DEF ) THEN
                   MODE_START = BUF_VEX_HEAD(J1)(IND(1,3):IND(2,3))
                   MODE_SHORT = BUF_VEX_HEAD(J1)(IND(1,4):IND(2,4))
                   MODE_LONG  = BUF_VEX_HEAD(J1)(IND(1,5):IND(2,5))
                   MODE_TAPE_CHANGE = BUF_VEX_HEAD(J1)(IND(1,6):IND(2,6))
                   MODE_FINAL = BUF_VEX_HEAD(J1)(IND(1,7):IND(2,7))
                   IF ( LIND .GE. 8 ) THEN
                        MODE_POINTING = BUF_VEX_HEAD(J1)(IND(1,8):IND(2,8))
                   END IF
                   IF ( LIND .GE. 9 ) THEN
                        MODE_FLUX_CALIB = BUF_VEX_HEAD(J1)(IND(1,9):IND(2,9))
                   END IF
              END IF
              FL_MODE_DEF = .TRUE.
         END IF
!
         IP = INDEX ( BUF_VEX_HEAD(J1), '__@MOD1_DESCR@__' )
         IF ( IP > 0 ) THEN
              STR = SUR%OBSERVING_MODE_DESCR(IND_MDSC(1,1):IND_MDSC(2,1))
              BUF_VEX_HEAD(J1) = BUF_VEX_HEAD(J1)(1:IP-1)// &
     &                           TRIM(STR)// &
     &                           BUF_VEX_HEAD(J1)(IP+ILEN('__@MOD1_DESCR@__'):)
         END IF
!
         IF ( INDEX ( BUF_VEX_HEAD(J1)(IND(1,1):IND(2,1)), '$SITE' ) > 0 ) THEN
              FL_SITE    = .TRUE.
              FL_ANTENNA = .FALSE.
              IND_STA = 0
            ELSE IF ( BUF_VEX_HEAD(J1)(IND(1,1):IND(2,1)) == '$ANTENNA' ) THEN
              FL_SITE    = .FALSE. 
              FL_ANTENNA = .TRUE.
              IND_STA = 0
            ELSE IF ( BUF_VEX_HEAD(J1)(IND(1,1):IND(1,1)) == '$' ) THEN
              FL_SITE = .FALSE.
              FL_ANTENNA = .FALSE.
              IND_STA = 0
         END IF
!
         IF ( FL_SITE ) THEN
              IF ( BUF_VEX_HEAD(J1)(IND(1,1):IND(2,1)) == 'def' ) THEN
                   STA_NAM = BUF_VEX_HEAD(J1)(IND(1,2):IND(2,2))
                   IND_STA = 0
                   DO 530 J3=1,SUR%L_STA
                      IF ( STA_NAM == SUR%STA(J3)%NAME ) THEN
                           IND_STA = J3
                      END IF
 530               CONTINUE 
              END IF
         END IF
!
         IF ( FL_ANTENNA ) THEN
              IF ( BUF_VEX_HEAD(J1)(IND(1,1):IND(2,1)) == 'def' ) THEN
                   STA_NAM = BUF_VEX_HEAD(J1)(IND(1,2):IND(2,2))
                   IND_STA = 0
                   DO 540 J4=1,SUR%L_STA
                      IF ( STA_NAM == SUR%STA(J4)%NAME ) THEN
                           IND_STA = J4
                      END IF
 540               CONTINUE 
              END IF
         END IF
!
         IF ( INDEX ( BUF_VEX_HEAD(J1), '@horizon_map_az@' ) > 0 .AND. &
     &        FL_SITE                                            .AND. &
     &        IND_STA > 0                                              ) THEN
!
              IP = INDEX ( BUF_VEX_HEAD(J1), '@horizon_map_az@' ) 
              CALL CLRCH ( STR )
              IF ( SUR%STA(IND_STA)%N_HM > 0 ) THEN
                   STR(IP:) = 'horizon_map_az      ='
                   DO 550 J5=1,SUR%STA(IND_STA)%N_HM
                      IL = ILEN(STR)
                      WRITE ( UNIT=STR(IL+2:IL+10), FMT='(F6.2, " : ")' ) SUR%STA(IND_STA)%AZ_HM(J5)/DEG__TO__RAD
 550               CONTINUE 
                   STR = STR(1:33)//' deg '//STR(34:ILEN(STR)-1)//';'
                ELSE
                   STR = 'horizon_map_az = 0.0 deg : 360.0 ;'
              END IF
              BUF_VEX_HEAD(J1) = STR
           ELSE IF ( INDEX ( BUF_VEX_HEAD(J1), '@horizon_map_az@' ) > 0 .AND. &
     &               FL_SITE .AND. &
     &               IND_STA == 0 ) THEN
              GOTO 410
         END IF
!
         IF ( INDEX ( BUF_VEX_HEAD(J1), '@horizon_map_el@' ) > 0 .AND. &
     &        FL_SITE                                            .AND. &
     &        IND_STA > 0                                              ) THEN
!
              IP = INDEX ( BUF_VEX_HEAD(J1), '@horizon_map_el@' ) 
              CALL CLRCH ( STR )
              IF ( SUR%STA(IND_STA)%N_HM > 0 ) THEN
                   STR(IP:) = 'horizon_map_el      ='
                   DO 560 J6=1,SUR%STA(IND_STA)%N_HM
                      IL = ILEN(STR)
                      WRITE ( UNIT=STR(IL+2:IL+10), FMT='(F6.2, " : ")' ) SUR%STA(IND_STA)%EL_HM(J6)/DEG__TO__RAD
 560               CONTINUE 
                   STR = STR(1:33)//' deg '//STR(34:ILEN(STR)-1)//';'
                 ELSE
                   STR = 'horizon_map_el  0.0 deg : 0.0'
              END IF
              BUF_VEX_HEAD(J1) = STR
           ELSE IF ( INDEX ( BUF_VEX_HEAD(J1), '@horizon_map_el@' ) > 0 .AND. &
     &               FL_SITE                                            .AND. &
     &               IND_STA == 0                                             ) THEN
              GOTO 410
         END IF
!
         IF ( INDEX ( BUF_VEX_HEAD(J1), '@antenna_motion@' ) > 0 .AND. &
     &        FL_ANTENNA                                         .AND. &
     &        IND_STA > 0                                              ) THEN
              WRITE ( LUN_VEX, FMT=203 ) 'az', SUR%STA(IND_STA)%SLEW_RATE_AZ/DEG__TO__RAD, &
     &                                         SUR%STA(IND_STA)%TIME_SETTLE_AZ, &
     &                                         SUR%STA(IND_STA)%SLEW_ACCL_AZ/DEG__TO__RAD
              WRITE ( LUN_VEX, FMT=203 ) 'el', SUR%STA(IND_STA)%SLEW_RATE_EL/DEG__TO__RAD, &
     &                                         SUR%STA(IND_STA)%TIME_SETTLE_EL, &
     &                                         SUR%STA(IND_STA)%SLEW_ACCL_EL/DEG__TO__RAD
 203          FORMAT ( '    antenna_motion  = ',A, ' : ', F5.2, ' deg/sec : ', &
     &                 F5.2, ' sec : ', F5.2, ' deg/sec^2 ;' )
              GOTO 410
           ELSE IF ( INDEX ( BUF_VEX_HEAD(J1), '@antenna_motion@' ) > 0 .AND. &
     &        FL_ANTENNA                                                .AND. &
     &        IND_STA == 0                                                    ) THEN
              GOTO 410
         END IF
!
         IF ( INDEX ( BUF_VEX_HEAD(J1), '@pointing_sector@' ) > 0 .AND. &
     &        FL_ANTENNA                                          .AND. &
     &        IND_STA > 0                                               ) THEN
              WRITE ( LUN_VEX, FMT=206 ) '&ccw', SUR%STA(IND_STA)%AZ_RANGE(1)/DEG__TO__RAD, &
     &                                           SUR%STA(IND_STA)%AZ_RANGE(2)/DEG__TO__RAD, &
     &                                           SUR%STA(IND_STA)%EL_MIN/DEG__TO__RAD,      &
     &                                           SUR%STA(IND_STA)%EL_MAX/DEG__TO__RAD, 'ccw'
              WRITE ( LUN_VEX, FMT=206 ) '&n  ', SUR%STA(IND_STA)%AZ_RANGE(2)/DEG__TO__RAD, &
     &                                           SUR%STA(IND_STA)%AZ_RANGE(3)/DEG__TO__RAD, &
     &                                           SUR%STA(IND_STA)%EL_MIN/DEG__TO__RAD,      &
     &                                           SUR%STA(IND_STA)%EL_MAX/DEG__TO__RAD, 'n  '
              WRITE ( LUN_VEX, FMT=206 ) '&cw ', SUR%STA(IND_STA)%AZ_RANGE(3)/DEG__TO__RAD, &
     &                                           SUR%STA(IND_STA)%AZ_RANGE(4)/DEG__TO__RAD, &
     &                                           SUR%STA(IND_STA)%EL_MIN/DEG__TO__RAD,      &
     &                                           SUR%STA(IND_STA)%EL_MAX/DEG__TO__RAD, 'cw '
 206          FORMAT ( '    pointing_sector = ', A, ' : az : ', F6.1, &
     &                 ' deg : ', F6.1,' deg : el : ',F6.1, ' deg : ', F6.1, ' deg : ', A, ' ;' )
              GOTO 410
           ELSE IF ( INDEX ( BUF_VEX_HEAD(J1), '@pointing_sector@' ) > 0 .AND. &
     &        FL_ANTENNA                                                 .AND. &
     &        IND_STA == 0                                                     ) THEN
              GOTO 410
         END IF
         IF ( INDEX ( BUF_VEX_HEAD(J1), '@backend@' ) > 0 ) THEN
              STA_NAM = BUF_VEX_HEAD(J1)(IND(1,5):IND(2,5))
              CALL TRAN ( 12, STA_NAM, STA_NAM )
              IND_STA = 0
              DO 570 J7=1,SUR%L_STA
                 STR = SUR%STA(J7)%SHORT_NAME 
                 CALL TRAN ( 12, STR, STR )
                 IF ( STA_NAM(1:2) == STR(1:2) ) THEN
                      IND_STA = J7
                 END IF
 570          CONTINUE 
!
              IF ( IND_STA > 0 ) THEN
                   IP = INDEX ( BUF_VEX_HEAD(J1), '@backend@' ) 
                   BUF_VEX_HEAD(J1) = BUF_VEX_HEAD(J1)(1:IP-1)//TRIM(SUR%STA(IND_STA)%BACKEND)// &
     &                                BUF_VEX_HEAD(J1)(IP+LEN('@backend@'):)
                   WRITE ( LUN_VEX, FMT='(A)' ) BUF_VEX_HEAD(J1)(1:I_LEN(BUF_VEX_HEAD(J1)))
              END IF
              GOTO 410
         END IF
         IF ( BUF_VEX_HEAD(J1)(1:1) .NE. '*'                          .AND. &
     &        INDEX ( BUF_VEX_HEAD(J1), 'def' ) > 0                   .AND. &
     &        INDEX ( BUF_VEX_HEAD(J1), 'PECULAIR_CLOCK_OFFSET' ) > 0       ) THEN
              FL_CLOCK = .TRUE.
         END IF
         IF ( FL_CLOCK                                    .AND. &
     &        INDEX ( BUF_VEX_HEAD(J1), 'extension' ) > 0 .AND. &
     &        INDEX ( BUF_VEX_HEAD(J1), 'NASA'      ) > 0       ) THEN
              IND_STA = 0
              DO 580 J8=1,SUR%L_STA
                 STR = SUR%STA(J8)%SHORT_NAME 
                 CALL TRAN ( 12, STR, STR )
                 IF ( BUF_VEX_HEAD(J1)(IND(1,5):IND(2,5)) == STR(1:2) ) THEN
                      IND_STA = J8
                 END IF
 580          CONTINUE 
              IF ( IND_STA > 0 ) THEN
                   IF ( ILEN(SUR%STA(IND_STA)%CLOCK_DATE_BEG) > 0 ) THEN
                        WRITE ( UNIT=STR(1:6), FMT='(F6.3)' ) 1.D6*SUR%STA(IND_STA)%CLOCK_OFFSET
                        IF ( STR(1:6) == '******' ) THEN
                             WRITE ( UNIT=STR(1:6), FMT='(1PD10.3)' ) 1.D6*SUR%STA(IND_STA)%CLOCK_OFFSET
                        END IF
                        CALL ERR_PASS ( IUER, IER )
                        BUF_VEX_HEAD(J1) = BUF_VEX_HEAD(J1)(1:IND(1,6)+1)// &
     &                                     TRIM(STR)//BUF_VEX_HEAD(J1)(IND(2,7)+1:)
                        CALL DATE_TO_TIME ( SUR%STA(IND_STA)%CLOCK_DATE_BEG, MJD, SEC, IER )
                        STR = MJDSEC_TO_VEX ( MJD, SEC, IER )
                        CALL EXWORD ( BUF_VEX_HEAD(J1), MIND, LIND, IND, &
     &                                CHAR(32)//CHAR(9)//',;', IER )
                        BUF_VEX_HEAD(J1) = BUF_VEX_HEAD(J1)(1:IND(1,11)-1)// &
     &                                     STR(1:17)//'s'//BUF_VEX_HEAD(J1)(IND(2,11)+1:)
                        CALL EXWORD ( BUF_VEX_HEAD(J1), MIND, LIND, IND, &
     &                                CHAR(32)//CHAR(9)//',;', IER )
                        BUF_VEX_HEAD(J1) = BUF_VEX_HEAD(J1)(1:IND(1,13)-1)// &
     &                                     "stp  "//BUF_VEX_HEAD(J1)(IND(2,13)+1:)
                   END IF
              END IF
         END IF
         IF ( BUF_VEX_HEAD(J1)(1:1) .NE. '*'           .AND. &
     &        INDEX ( BUF_VEX_HEAD(J1), 'enddef' ) > 0       ) THEN
              FL_CLOCK = .FALSE.
         END IF
!
         WRITE ( LUN_VEX, FMT='(A)' ) BUF_VEX_HEAD(J1)(1:I_LEN(BUF_VEX_HEAD(J1)))
 410  CONTINUE
!
      DO 420 J2=1,NKEY_HEAD
         IP = INDEX ( BUF_KEY_HEAD(J2), '__@EXP_CODE@__' )
         IF ( IP > 0 ) THEN
              BUF_KEY_HEAD(J2) = BUF_KEY_HEAD(J2)(1:IP-1)// &
     &                       SUR%EXP_CODE(1:I_LEN(SUR%EXP_CODE))// &
     &                       BUF_KEY_HEAD(J2)(IP+ILEN('__@EXP_CODE@__'):)
         END IF
!
         IP = INDEX ( BUF_KEY_HEAD(J2), '__@EXP_DESCR@__' )
         IF ( IP > 0 ) THEN
              BUF_KEY_HEAD(J2) = BUF_KEY_HEAD(J2)(1:IP-1)// &
     &                       SUR%EXP_DESCR(1:I_LEN(SUR%EXP_DESCR))// &
     &                       BUF_KEY_HEAD(J2)(IP+ILEN('__@EXP_DESCR@__'):)
         END IF
!
         IP = INDEX ( BUF_KEY_HEAD(J2), '__@SCHEDULE_REVISION@__' )
         IF ( IP > 0 ) THEN
              BUF_KEY_HEAD(J2) = BUF_KEY_HEAD(J2)(1:IP-1)// &
     &                       SUR%SCHEDULE_REVISION(1:I_LEN(SUR%SCHEDULE_REVISION))// &
     &                       BUF_KEY_HEAD(J2)(IP+ILEN('__@SCHEDULE_REVISION@__'):)
         END IF
!
         IP = INDEX ( BUF_KEY_HEAD(J2), '__@PI_NAME@__' )
         IF ( IP > 0 ) THEN
              BUF_KEY_HEAD(J2) = BUF_KEY_HEAD(J2)(1:IP-1)// &
     &                       SUR%PI_NAME(1:I_LEN(SUR%PI_NAME))// &
     &                       BUF_KEY_HEAD(J2)(IP+ILEN('__@PI_NAME@__'):)
         END IF
!
         IP = INDEX ( BUF_KEY_HEAD(J2), '__@SCHEDULER_NAME@__' )
         IF ( IP > 0 ) THEN
              BUF_KEY_HEAD(J2) = BUF_KEY_HEAD(J2)(1:IP-1)// &
     &                       SUR%SCHEDULER_NAME(1:I_LEN(SUR%SCHEDULER_NAME))// &
     &                       BUF_KEY_HEAD(J2)(IP+ILEN('__@SCHEDULER_NAME@__'):)
         END IF
!
         IP = INDEX ( BUF_KEY_HEAD(J2), '__@SCHEDULER_EMAIL@__' )
         IF ( IP > 0 ) THEN
              BUF_KEY_HEAD(J2) = BUF_KEY_HEAD(J2)(1:IP-1)// &
     &                       SUR%SCHEDULER_EMAIL(1:I_LEN(SUR%SCHEDULER_EMAIL))// &
     &                       BUF_KEY_HEAD(J2)(IP+ILEN('__@SCHEDULER_EMAIL@__'):)
         END IF
!
         IP = INDEX ( BUF_KEY_HEAD(J2), '__@CORR_TIME_RESOLUTION@__' )
         IF ( IP > 0 ) THEN
              WRITE ( UNIT=STR1, FMT='(F8.3)' ) SUR%CORR_TIME_RESOLUTION
              CALL CHASHL ( STR1 )
              BUF_KEY_HEAD(J2) = BUF_KEY_HEAD(J2)(1:IP-1)//TRIM(STR1)// &
     &                           BUF_KEY_HEAD(J2)(IP+ILEN('__@SUR%CORR_TIME_RESOLUTION@__'):)
         END IF
!
         IP = INDEX ( BUF_KEY_HEAD(J2), '__@SCHEDULER_PHONE@__' )
         IF ( IP > 0 ) THEN
              BUF_KEY_HEAD(J2) = BUF_KEY_HEAD(J2)(1:IP-1)// &
     &                       SUR%SCHEDULER_PHONE(1:I_LEN(SUR%SCHEDULER_PHONE))// &
     &                       BUF_KEY_HEAD(J2)(IP+ILEN('__@SCHEDULER_PHONE@__'):)
         END IF
!
         IP = INDEX ( BUF_KEY_HEAD(J2), '__@OBSERVER_PHONE@__' )
         IF ( IP > 0 ) THEN
              BUF_KEY_HEAD(J2) = BUF_KEY_HEAD(J2)(1:IP-1)// &
     &                       SUR%OBSERVER_PHONE(1:I_LEN(SUR%OBSERVER_PHONE))// &
     &                       BUF_KEY_HEAD(J2)(IP+ILEN('__@OBSERVER_PHONE@__'):)
         END IF
!
         IP = INDEX ( BUF_KEY_HEAD(J2), '__@DATE_GEN@__' )
         IF ( IP > 0 ) THEN
              BUF_KEY_HEAD(J2) = BUF_KEY_HEAD(J2)(1:IP-1)//GET_CDATE()
              BUF_KEY_HEAD(J2)(79:79) = '#'
         END IF
!
         IP = INDEX ( BUF_KEY_HEAD(J2), '__@GEN_NAME@__' )
         IF ( IP > 0 ) THEN
              BUF_KEY_HEAD(J2) = BUF_KEY_HEAD(J2)(1:IP-1)//USER_REALNAME
              BUF_KEY_HEAD(J2)(79:79) = '#'
         END IF
!
         IP = INDEX ( BUF_KEY_HEAD(J2), '__@GEN_PROGNAME@__' )
         IF ( IP > 0 ) THEN
              BUF_KEY_HEAD(J2) = BUF_KEY_HEAD(J2)(1:IP-1)//SUR__LABEL
              BUF_KEY_HEAD(J2)(79:79) = '#'
         END IF
         WRITE ( LUN_KEY, FMT='(A)' ) BUF_KEY_HEAD(J2)(1:I_LEN(BUF_KEY_HEAD(J2)))
 420  CONTINUE 
!
      WRITE ( LUN_VEX, FMT='(A)' ) '*'
      WRITE ( LUN_VEX, FMT='(A)' ) '*------------------------------------------------------------------------------'
      WRITE ( LUN_VEX, FMT='(A)' ) '$SOURCE;'
      WRITE ( LUN_VEX, FMT='(A)' ) '*'
!
      WRITE ( LUN_KEY, FMT='(A)' ) ' '
      WRITE ( LUN_KEY, FMT='(A)' ) '! =========================================================='
      WRITE ( LUN_KEY, FMT='(A)' ) '! ====================  Source coordinates   ==============='
      WRITE ( LUN_KEY, FMT='(A)' ) '! =========================================================='
      WRITE ( LUN_KEY, FMT='(A)' ) ' '
      WRITE ( LUN_KEY, FMT='(A)' ) 'srccat /'
!
      DO 440 J4=1,SUR%L_STA
         IF ( SUR%STA(J4)%NAME == 'ATCA-104' .OR. &
     &        SUR%STA(J4)%NAME == 'ATCAP104' .OR. &
     &        SUR%STA(J4)%NAME == 'CEDUNA  ' .OR. &
     &        SUR%STA(J4)%NAME == 'MOPRA   ' .OR. &
     &        SUR%STA(J4)%NAME == 'WARK12M ' .OR. &
     &        SUR%STA(J4)%NAME == 'ASKAP   '      ) THEN
!
              FL_LBA_KEY = .TRUE.
         END IF
!%%         fl_lba_key = .false. ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
         IF ( SUR%STA(J4)%NAME == 'TORUN   ' .OR. &
     &        SUR%STA(J4)%NAME == 'YEBES40M' .OR. &
     &        SUR%STA(J4)%NAME == 'NOTO    '      ) THEN
!
              FL_EVN_KEY = .TRUE.
         END IF
         IF ( SUR%STA(J4)%NAME == 'BR-VLBA ' .OR. &
     &        SUR%STA(J4)%NAME == 'FD-VLBA ' .OR. &
     &        SUR%STA(J4)%NAME == 'HN-VLBA ' .OR. &
     &        SUR%STA(J4)%NAME == 'KP-VLBA ' .OR. &
     &        SUR%STA(J4)%NAME == 'LA-VLBA ' .OR. &
     &        SUR%STA(J4)%NAME == 'MK-VLBA ' .OR. &
     &        SUR%STA(J4)%NAME == 'NL-VLBA ' .OR. &
     &        SUR%STA(J4)%NAME == 'OV-VLBA ' .OR. &
     &        SUR%STA(J4)%NAME == 'PIETOWN ' .OR. &
     &        SUR%STA(J4)%NAME == 'SC-VLBA '      ) THEN
              FL_VLBA_KEY = .TRUE.
         END IF
         IF ( SUR%EXP_CODE(1:2) == 'cn' ) THEN
              FL_CVN_KEY = .TRUE.
         END IF
!
         IF ( SUR%STA(J4)%MOUNT_TYPE == MT__ALTAZ ) THEN
              AXIS_NAME(1) = 'azimuth'
              AXIS_NAME(2) = 'elevation'
            ELSE IF ( SUR%STA(J4)%MOUNT_TYPE == MT__EQUAT ) THEN
              AXIS_NAME(1) = 'hour_angle'
              AXIS_NAME(2) = 'declination'
            ELSE IF ( SUR%STA(J4)%MOUNT_TYPE == MT__XY_N  .OR. &
     &                SUR%STA(J4)%MOUNT_TYPE == MT__XY_E       ) THEN
              AXIS_NAME(1) = 'X-axis'
              AXIS_NAME(2) = 'Y-axis'
         END IF
         NA = NA + 1 ; WRITE ( UNIT=AST_BUF(NA), FMT='(A)' ) '#'
         NA = NA + 1 ; WRITE ( UNIT=AST_BUF(NA), FMT='(A)' ) 'Station_parameters:         '//SUR%STA(J4)%NAME// &
     &                            '  Short_name:    '//SUR%STA(J4)%SHORT_NAME
         NA = NA + 1 ; WRITE ( UNIT=AST_BUF(NA), FMT='(A,A,3X,A)' ) &
     &                    '  Last_update:              ', SUR%STA(J4)%NAME, &
     &                                                  SUR%STA(J4)%LAST_UPDATE
         NA = NA + 1 ; WRITE ( UNIT=AST_BUF(NA), FMT='(A,A,1X,F13.3,1X,F13.3,1X,F13.3,A)' ) &
     &                    '  Coordinates:              ', SUR%STA(J4)%NAME, SUR%STA(J4)%COO_TRS, ' meter'
         NA = NA + 1 ; WRITE ( UNIT=AST_BUF(NA), FMT='(A,A,3X,A)' )                         &
     &                    '  Mount:                    ', SUR%STA(J4)%NAME, TRIM(SUR%STA(J4)%MOUNT_TYPE)
         NA = NA + 1 ; WRITE ( UNIT=AST_BUF(NA), FMT='(A,A,2X,4(F6.1,1X),1X,A,2X,A,A)' ) &
     &                    '  1st_axis_range:           ', SUR%STA(J4)%NAME, &
     &                                                  SUR%STA(J4)%AZ_RANGE/DEG__TO__RAD, &
     &                                                  'deg', 'Axis: ', AXIS_NAME(1)
         NA = NA + 1 ; WRITE ( UNIT=AST_BUF(NA), FMT='(A,A,2X,2(F6.1,1X),1X,A,16X,A,A)' ) &
     &                    '  2nd_axis_range:           ', SUR%STA(J4)%NAME, &
     &                                                  SUR%STA(J4)%EL_MIN/DEG__TO__RAD, &
     &                                                  SUR%STA(J4)%EL_MAX/DEG__TO__RAD, &
     &                                                  'deg', 'Axis: ', AXIS_NAME(2)
         NA = NA + 1 ; WRITE ( UNIT=AST_BUF(NA), FMT='(A,A,3X,F5.2,4X,A,17X,A,A)' ) &
     &                    '  1st_axis_slewing_rate:    ', SUR%STA(J4)%NAME, &
     &                                                  SUR%STA(J4)%SLEW_RATE_AZ/DEG__TO__RAD, &
     &                                                  'deg/sec', 'Axis: ', AXIS_NAME(1)
         NA = NA + 1 ; WRITE ( UNIT=AST_BUF(NA), FMT='(A,A,3X,F5.2,4X,A,17X,A,A)' ) &
     &                    '  2nd_axis_slewing_rate:    ', SUR%STA(J4)%NAME, &
     &                                                  SUR%STA(J4)%SLEW_RATE_EL/DEG__TO__RAD, &
     &                                                  'deg/sec', 'Axis: ', AXIS_NAME(2)
         NA = NA + 1 ; WRITE ( UNIT=AST_BUF(NA), FMT='(A,A,3X,F5.2,4X,A,15X,A,A)' ) &
     &                    '  1st_axis_slewing_accl:    ', SUR%STA(J4)%NAME, &
     &                                                  SUR%STA(J4)%SLEW_ACCL_AZ/DEG__TO__RAD, &
     &                                                  'deg/sec^2', 'Axis: ', AXIS_NAME(1)
         NA = NA + 1 ; WRITE ( UNIT=AST_BUF(NA), FMT='(A,A,3X,F5.2,4X,A,15X,A,A)' ) &
     &                    '  2nd_axis_slewing_accl:    ', SUR%STA(J4)%NAME, &
     &                                                  SUR%STA(J4)%SLEW_ACCL_EL/DEG__TO__RAD, &
     &                                                  'deg/sec^2', 'Axis: ', AXIS_NAME(2)
         NA = NA + 1 ; WRITE ( UNIT=AST_BUF(NA), FMT='(A,A,3X,F5.2,4X,A,17X,A,A)' ) &
     &                    '  1st_axis_settle_time:     ', SUR%STA(J4)%NAME, &
     &                                                  SUR%STA(J4)%TIME_SETTLE_AZ, &
     &                                                  'sec', '    Axis: ', AXIS_NAME(1)
         NA = NA + 1 ; WRITE ( UNIT=AST_BUF(NA), FMT='(A,A,3X,F5.2,4X,A,17X,A,A)' ) &
     &                    '  2nd_axis_settle_time:     ', SUR%STA(J4)%NAME, &
     &                                                  SUR%STA(J4)%TIME_SETTLE_EL, &
     &                                                  'sec', '    Axis: ', AXIS_NAME(2)
         NA = NA + 1 ; WRITE ( UNIT=AST_BUF(NA), FMT='(A,A,3X,F5.1,4X,A)' ) '  Preob_proc_duration:      ', SUR%STA(J4)%NAME, SUR%STA(J4)%PREOB,           'sec'
         NA = NA + 1 ; WRITE ( UNIT=AST_BUF(NA), FMT='(A,A,3X,F5.1,4X,A)' ) '  Postob_proc_duration:     ', SUR%STA(J4)%NAME, SUR%STA(J4)%POSTOB,          'sec'
         NA = NA + 1 ; WRITE ( UNIT=AST_BUF(NA), FMT='(A,A,3X,A)' )         '  Recorder:                 ', SUR%STA(J4)%NAME, SUR%STA(J4)%RECORDER
         NA = NA + 1 ; WRITE ( UNIT=AST_BUF(NA), FMT='(A,A,2X,F6.3,4X,A)' ) '  Recording_rate:           ', SUR%STA(J4)%NAME, SUR%RECORDING_RATE/1.D9,     'Gbps'
         NA = NA + 1 ; WRITE ( UNIT=AST_BUF(NA), FMT='(A,A,2X,F6.1,4X,A)' ) '  Setmode_duration:         ', SUR%STA(J4)%NAME, SUR%STA(J4)%SETMODE,         ' '
 440  CONTINUE
!
      IS_TAG = 1
      CALL CLRCH ( BUF_TAG(IS_TAG) )
      BUF_TAG(IS_TAG) = '# SOURCE-NAMES  v 2.0 2005.09.06'
      IS_TAG = 2
      CALL CLRCH ( BUF_TAG(IS_TAG) )
      BUF_TAG(IS_TAG) = '# '
      IS_TAG = 3
      CALL CLRCH ( BUF_TAG(IS_TAG) )
      BUF_TAG(IS_TAG) = '# Created on '//GET_CDATE()
      IS_TAG = 4
      CALL CLRCH ( BUF_TAG(IS_TAG) )
      BUF_TAG(IS_TAG) = '# '
      IS_TAG = 5
      CALL CLRCH ( BUF_TAG(IS_TAG) )
      BUF_TAG(IS_TAG) = '# Number of sources:           '
      IS_TAG = 6
      CALL CLRCH ( BUF_TAG(IS_TAG) )
      BUF_TAG(IS_TAG) = '# '
      IS_CAL = IS_TAG
      BUF_CAL = BUF_TAG
!
! --- Write the source name table
!
      N_FLX = 0
      CALL SUR_SOU_OUT ( SUR, VTD, SUR__TYP_TAG, LUN_VEX, LUN_KEY, &
     &                   MBUF, IS_TAG, BUF_TAG, N_FLX, BUF_FLX, IER )
      CALL SUR_SOU_OUT ( SUR, VTD, SUR__TYP_SEC, LUN_VEX, LUN_KEY, &
     &                   MBUF, IS_TAG, BUF_TAG, N_FLX, BUF_FLX, IER )
      CALL SUR_SOU_OUT ( SUR, VTD, SUR__TYP_CAL, LUN_VEX, LUN_KEY, &
     &                   MBUF, IS_CAL, BUF_CAL, N_FLX, BUF_FLX, IER )
      CALL SUR_SOU_OUT ( SUR, VTD, SUR__TYP_POC, LUN_VEX, LUN_KEY, &
     &                   MBUF, IS_CAL, BUF_CAL, N_FLX, BUF_FLX, IER )
!
      CALL ERR_PASS ( IUER, IER )
      CALL SUR_SOU_OUT ( SUR, VTD, SUR__TYP_PLA, LUN_VEX, LUN_KEY, &
     &                   MBUF, IS_CAL, BUF_CAL, N_FLX, BUF_FLX, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 1520, IUER, 'SUR_PRINT_RES', 'Error in an attempt '// &
     &         'to write coordinats of the source list' )
           RETURN
      END IF
!
      CALL SUR_SOU_OUT ( SUR, VTD, SUR__TYP_PAI, LUN_VEX, LUN_KEY, &
     &                   MBUF, IS_CAL, BUF_CAL, N_FLX, BUF_FLX, IER )
!
      CALL ERR_PASS ( IUER, IER )
      CALL WR_TEXT  ( IS_TAG, BUF_TAG, SUR%OUT_SOU_LIST, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 1521, IUER, 'SUR_PRINT_RES', 'Error in an attempt '// &
     &         'to write output file '//SUR%OUT_SOU_LIST )
           RETURN
      END IF
!
      WRITE ( UNIT=LUN_KEY, FMT='(A)' ) "endcat /"
      WRITE ( UNIT=LUN_KEY, FMT='(A)' ) " "
      IF ( SUR%KEY_TYP == KEY__TIME_ABS ) THEN
           WRITE ( UNIT=LUN_KEY, FMT='(A)' ) "!  Duration time accounts only for on-source time, excluding slewing"
         ELSE IF ( SUR%KEY_TYP == KEY__TIME_DUR ) THEN
           WRITE ( UNIT=LUN_KEY, FMT='(A)' ) "!  Duration time accounts for both slewing and on-source time"
      END IF
      WRITE ( UNIT=STR(1:5), FMT='(F5.1)' ) SUR%UTC_M_TAI
      WRITE ( UNIT=LUN_KEY, FMT='(A)' ) "!  Time tag is UTC.  UTC_M_TAI: "//STR(1:5)
      WRITE ( UNIT=LUN_KEY, FMT='(A)' ) " "
      IF ( SUR%KEY_TYP == KEY__LST_PT ) THEN
           STA_LST = 'PIETOWN '
           CALL ERR_PASS ( IUER, IER )
           LST_PT = GET_LST ( SUR%MJD_START, SUR%TAI_START, STA_LST, &
     &                        SUR, VTD, S_ANG, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 1522, IUER, 'SUR_PRINT_RES', 'Failure to '// &
     &              'compute LST time for PIETOWN' )
                RETURN 
           END IF
           CALL  CLRCH ( STR )
           IER = -1
           CALL RH_TAT ( LST_PT, 0, STR, IER )
           CALL CHASHL ( STR )
           STR(3:3) = ':'
           STR(6:6) = ':'
           STR_VEX_DATE = MJDSEC_TO_VEX ( SUR%MJD_START, SUR%TAI_START, IER )
           WRITE ( LUN_KEY, FMT='(A)' ) ' '
           WRITE ( LUN_KEY, FMT='(A)' ) 'LST   = VLBA_PT'
           WRITE ( LUN_KEY, FMT='(A)' ) ' '
           CALL RH_TAT ( S_ANG, 1, STR1, IER )
           WRITE ( LUN_KEY, FMT='(A)' ) '! GST = '//STR1(2:9)
           WRITE ( LUN_KEY, FMT='(A)' ) ' '
           WRITE ( LUN_KEY, FMT='(A)' ) 'year  = '//STR_VEX_DATE(1:4)
           WRITE ( LUN_KEY, FMT='(A)' ) 'day   = '//STR_VEX_DATE(6:8)
           WRITE ( LUN_KEY, FMT='(A)' ) 'start = '//STR(1:8)
           WRITE ( LUN_KEY, FMT='(A)' ) ' '
!!           write ( 6, * ) 'STR(1:10)=',STR(1:10),' VEX='//STR_VEX_DATE(1:20)
        ELSE IF ( SUR%KEY_TYP == KEY__LST_PA ) THEN
           STA_LST = 'PARKES  '
           CALL ERR_PASS ( IUER, IER )
           LST_PT = GET_LST ( SUR%MJD_START, SUR%TAI_START, STA_LST, &
     &                        SUR, VTD, S_ANG, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 1523, IUER, 'SUR_PRINT_RES', 'Failure to '// &
     &              'compute LST time for PIETOWN' )
                RETURN 
           END IF
           CALL  CLRCH ( STR )
           IER = -1
           CALL RH_TAT ( LST_PT, 0, STR, IER )
           CALL CHASHL ( STR )
           STR(3:3) = ':'
           STR(6:6) = ':'
           STR_VEX_DATE = MJDSEC_TO_VEX ( SUR%MJD_START, SUR%TAI_START, IER )
           WRITE ( LUN_KEY, FMT='(A)' ) ' '
           WRITE ( LUN_KEY, FMT='(A)' ) 'LST   = PARK_MK5'
           WRITE ( LUN_KEY, FMT='(A)' ) 'year  = '//STR_VEX_DATE(1:4)
           WRITE ( LUN_KEY, FMT='(A)' ) 'day   = '//STR_VEX_DATE(6:8)
           WRITE ( LUN_KEY, FMT='(A)' ) 'start = '//STR(1:8)
           WRITE ( LUN_KEY, FMT='(A)' ) ' '
        ELSE IF ( SUR%KEY_TYP == KEY__LST_JB ) THEN
           STA_LST = 'JODRELL2'
           CALL ERR_PASS ( IUER, IER )
           LST_PT = GET_LST ( SUR%MJD_START, SUR%TAI_START, STA_LST, &
     &                        SUR, VTD, S_ANG, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 1524, IUER, 'SUR_PRINT_RES', 'Failure to '// &
     &              'compute LST time for JODRELL2' )
                RETURN 
           END IF
           CALL  CLRCH ( STR )
           IER = -1
           CALL RH_TAT ( LST_PT, 0, STR, IER )
           CALL CHASHL ( STR )
           STR(3:3) = ':'
           STR(6:6) = ':'
           STR_VEX_DATE = MJDSEC_TO_VEX ( SUR%MJD_START, SUR%TAI_START, IER )
           WRITE ( LUN_KEY, FMT='(A)' ) ' '
           WRITE ( LUN_KEY, FMT='(A)' ) 'LST   = JODRELL2'
           WRITE ( LUN_KEY, FMT='(A)' ) 'year  = '//STR_VEX_DATE(1:4)
           WRITE ( LUN_KEY, FMT='(A)' ) 'day   = '//STR_VEX_DATE(6:8)
           WRITE ( LUN_KEY, FMT='(A)' ) 'start = '//STR(1:8)
           WRITE ( LUN_KEY, FMT='(A)' ) ' '
        ELSE IF ( SUR%KEY_TYP == KEY__LST_MC ) THEN
           STA_LST = 'MEDICINA'
           CALL ERR_PASS ( IUER, IER )
           LST_PT = GET_LST ( SUR%MJD_START, SUR%TAI_START, STA_LST, &
     &                        SUR, VTD, S_ANG, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 1525, IUER, 'SUR_PRINT_RES', 'Failure to '// &
     &              'compute LST time for MEDICINA' )
                RETURN 
           END IF
           CALL  CLRCH ( STR )
           IER = -1
           CALL RH_TAT ( LST_PT, 0, STR, IER )
           CALL CHASHL ( STR )
           STR(3:3) = ':'
           STR(6:6) = ':'
           STR_VEX_DATE = MJDSEC_TO_VEX ( SUR%MJD_START, SUR%TAI_START, IER )
           WRITE ( LUN_KEY, FMT='(A)' ) ' '
           WRITE ( LUN_KEY, FMT='(A)' ) 'LST   = MEDICINA'
           WRITE ( LUN_KEY, FMT='(A)' ) 'year  = '//STR_VEX_DATE(1:4)
           WRITE ( LUN_KEY, FMT='(A)' ) 'day   = '//STR_VEX_DATE(6:8)
           WRITE ( LUN_KEY, FMT='(A)' ) 'start = '//STR(1:8)
           WRITE ( LUN_KEY, FMT='(A)' ) ' '
        ELSE IF ( SUR%KEY_TYP == KEY__LST_EF ) THEN
           STA_LST = 'EFLSBERG'
           CALL ERR_PASS ( IUER, IER )
           LST_PT = GET_LST ( SUR%MJD_START, SUR%TAI_START, STA_LST, &
     &                        SUR, VTD, S_ANG, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 1526, IUER, 'SUR_PRINT_RES', 'Failure to '// &
     &              'compute LST time for EFLSBERG' )
                RETURN 
           END IF
           CALL  CLRCH ( STR )
           IER = -1
           CALL RH_TAT ( LST_PT, 0, STR, IER )
           CALL CHASHL ( STR )
           STR(3:3) = ':'
           STR(6:6) = ':'
           STR_VEX_DATE = MJDSEC_TO_VEX ( SUR%MJD_START, SUR%TAI_START, IER )
           WRITE ( LUN_KEY, FMT='(A)' ) ' '
           WRITE ( LUN_KEY, FMT='(A)' ) 'LST   = EFLSBERG'
           WRITE ( LUN_KEY, FMT='(A)' ) 'year  = '//STR_VEX_DATE(1:4)
           WRITE ( LUN_KEY, FMT='(A)' ) 'day   = '//STR_VEX_DATE(6:8)
           WRITE ( LUN_KEY, FMT='(A)' ) 'start = '//STR(1:8)
           WRITE ( LUN_KEY, FMT='(A)' ) ' '
      END IF
!
      WRITE ( LUN_VEX, FMT='(A)' ) '*'
      WRITE ( LUN_VEX, FMT='(A)' ) '*------------------------------------------------------------------------------'
      WRITE ( LUN_VEX, FMT='(A)' ) '$SCHED;'
      WRITE ( LUN_VEX, FMT='(A)' ) '*'
!
      CALL NOUT_R8 ( SUR__M_STA, ACC_DATA )
      MJD_REC = SUR%MJD_START
      TAI_REC = SUR%TAI_START
      IF ( SUR%RECORDING_PAUSE > 10.0D0 ) THEN
           WRITE ( UNIT=LUN_KEY, FMT='(A)' ) ' '
           WRITE ( UNIT=LUN_KEY, FMT='(A)' ) 'minpause 10'
           WRITE ( UNIT=LUN_KEY, FMT='(A)' ) 'prescan   0'
           WRITE ( UNIT=LUN_KEY, FMT='(A)' ) ' '
        ELSE IF ( SUR%RECORDING_PAUSE > 1.0D0 .OR. SUR%RECORDING_PAUSE < -1.0D0 ) THEN
           WRITE ( UNIT=LUN_KEY, FMT='(A)' ) ' '
           WRITE ( UNIT=LUN_KEY, FMT='(A)' ) 'minpause  0'
           WRITE ( UNIT=LUN_KEY, FMT='(A)' ) ' '
      END IF
!
      LAST_SCA = 0
      SOU_PREV = '          '
      SOU_BPRE = '        '
      DO 450 J5=1,SUR%L_SCN
         CALL CLRCH ( PAIR_NAME )
         IF ( SUR%SRC_TYP(J5) == SUR__TYP_TAG ) THEN
              J2000_NAME = SUR%SOU(SUR%IND_SRC(J5))%J2000_NAME
              B1950_NAME = SUR%SOU(SUR%IND_SRC(J5))%B1950_NAME
              OBJECT = 'SOURCE:'
              IF ( SUR%IND_PAI(SUR%IND_SRC(J5)) > 0 ) THEN
                   PAIR_NAME = SUR%PAI(SUR%IND_PAI(SUR%IND_SRC(J5)))%B1950_NAME
              END IF
              ALPHA_STR = SUR%SOU(SUR%IND_SRC(J5))%ALPHA_STR
              DELTA_STR = SUR%SOU(SUR%IND_SRC(J5))%DELTA_STR
              TYPE_STR  = 'primary'
              DUR_SCAN  = SUR%SOU(SUR%IND_SRC(J5))%DUR
            ELSE IF ( SUR%SRC_TYP(J5) == SUR__TYP_SEC ) THEN
              J2000_NAME = SUR%SO2(SUR%IND_SRC(J5))%J2000_NAME
              B1950_NAME = SUR%SO2(SUR%IND_SRC(J5))%B1950_NAME
              OBJECT = 'Source:'
              ALPHA_STR = SUR%SO2(SUR%IND_SRC(J5))%ALPHA_STR
              DELTA_STR = SUR%SO2(SUR%IND_SRC(J5))%DELTA_STR
              TYPE_STR  = 'secondary'
              DUR_SCAN  = SUR%SO2(SUR%IND_SRC(J5))%DUR
            ELSE IF ( SUR%SRC_TYP(J5) == SUR__TYP_CAL ) THEN
              J2000_NAME = SUR%CAL(SUR%IND_SRC(J5))%J2000_NAME
              B1950_NAME = SUR%CAL(SUR%IND_SRC(J5))%B1950_NAME
              OBJECT = 'Calibr:'
              ALPHA_STR = SUR%CAL(SUR%IND_SRC(J5))%ALPHA_STR
              DELTA_STR = SUR%CAL(SUR%IND_SRC(J5))%DELTA_STR
              TYPE_STR  = 'calibrator'
              DUR_SCAN  = SUR%TROPO_SCAN_LEN ! ? they may be different ...
            ELSE IF ( SUR%SRC_TYP(J5) == SUR__TYP_POC ) THEN
              J2000_NAME = SUR%SOP(SUR%IND_SRC(J5))%J2000_NAME
              B1950_NAME = SUR%SOP(SUR%IND_SRC(J5))%B1950_NAME
              OBJECT = 'Point: '
              ALPHA_STR = SUR%SOP(SUR%IND_SRC(J5))%ALPHA_STR
              DELTA_STR = SUR%SOP(SUR%IND_SRC(J5))%DELTA_STR
              TYPE_STR  = 'pointing'
              DUR_SCAN  = SUR%SOP(SUR%IND_SRC(J5))%DUR 
            ELSE IF ( SUR%SRC_TYP(J5) == SUR__TYP_PLA ) THEN
              J2000_NAME = SUR%PLA(SUR%IND_SRC(J5))%J2000_NAME
              B1950_NAME = SUR%PLA(SUR%IND_SRC(J5))%B1950_NAME
              OBJECT = 'Planet:'
              ALPHA_STR = SUR%PLA(SUR%IND_SRC(J5))%ALPHA_STR
              DELTA_STR = SUR%PLA(SUR%IND_SRC(J5))%DELTA_STR
              TYPE_STR  = 'planet'
              DUR_SCAN  = SUR%PLA(SUR%IND_SRC(J5))%DUR 
         END IF
         ALPHA_STR(3:3) = ':'
         ALPHA_STR(6:6) = ':'
         DELTA_STR(4:4) = ':'
         DELTA_STR(7:7) = ':'
!
         SOU_CUR  = 'J'//J2000_NAME(2:10)
         WRITE ( UNIT=STR(1:4), FMT='(I4)' ) J5
         WRITE ( UNIT=SCAN_STR_AST(1:6), FMT='("no",I4)' ) J5
         CALL BLANK_TO_ZERO ( STR(1:6) )
         CALL BLANK_TO_ZERO ( SCAN_STR_AST(1:6) )
         IF ( J5 > 1 ) THEN
              STR1 = MJDSEC_TO_DATE ( SUR%MJD_OBS_END(J5-1), &
     &                                SUR%TAI_OBS_END(J5-1) + SUR%UTC_M_TAI, IER )
            ELSE 
              STR1 = MJDSEC_TO_DATE ( SUR%MJD_START, &
     &                                SUR%TAI_START + SUR%UTC_M_TAI, IER )
         END IF
         STR2 = MJDSEC_TO_DATE ( SUR%MJD_OBS_END(J5), &
     &                           SUR%TAI_OBS_END(J5) + SUR%UTC_M_TAI, IER )
         NA = NA + 1 ; WRITE ( UNIT=AST_BUF(NA), FMT='(A)' ) '#'
         NA = NA + 1 ; WRITE ( UNIT=AST_BUF(NA), FMT='(A)' ) 'Scan: '//SCAN_STR_AST// &
     &                                '  Source: '//B1950_NAME// &
     &                                ' Alt_source_name: '//SOU_CUR// &
     &                                '  Ra: '//ALPHA_STR// &
     &                                ' Dec: '//DELTA_STR// &
     &                                ' Start_time: '//STR1(1:21)// &
     &                                ' Stop_time: '//STR2(1:21)// &
     &                                '  Type: '//TRIM(TYPE_STR)
         IF ( SUR%ALGORITHM == 'FRINGE_SEARCH_01' ) THEN
              WRITE ( UNIT=LUN_VEX, FMT='(A)' ) 'scan No'//STR(1:4)//';'
!
              IF ( J5 == SUR%L_SCN ) THEN
                   WRITE ( UNIT=LUN_VEX, FMT='(A)' ) '    mode='// &
     &                     MODE_FINAL(1:I_LEN(MODE_FINAL))//';'
                 ELSE
                   IF ( SUR%SCAN_TYPE(J5) == SUR__FIRST ) THEN
                        WRITE ( UNIT=LUN_VEX, FMT='(A)' ) '    mode='// &
     &                          MODE_START(1:I_LEN(MODE_START))//';'
                      ELSE IF ( SUR%SCAN_TYPE(J5) == SUR__SHORT ) THEN
                        WRITE ( UNIT=LUN_VEX, FMT='(A)' ) '    mode='// &
     &                          MODE_SHORT(1:I_LEN(MODE_SHORT))//';'
                      ELSE IF ( SUR%SCAN_TYPE(J5) == SUR__LONG ) THEN
                        WRITE ( UNIT=LUN_VEX, FMT='(A)' ) '    mode='// &
     &                          MODE_LONG(1:I_LEN(MODE_LONG))//';'
                      ELSE IF ( SUR%SCAN_TYPE(J5) == SUR__TAPE ) THEN
                        WRITE ( UNIT=LUN_VEX, FMT='(A)' ) '    mode='// &
     &                          MODE_TAPE_CHANGE(1:I_LEN(MODE_TAPE_CHANGE))//';'
                      ELSE IF ( SUR%SCAN_TYPE(J5) == SUR__LAST ) THEN
                        WRITE ( UNIT=LUN_VEX, FMT='(A)' ) '    mode='// &
     &                          MODE_FINAL(1:I_LEN(MODE_FINAL))//';'
                   END IF
              END IF
           ELSE IF ( SUR%ALGORITHM == 'FRINGE_SEARCH_02' ) THEN
              WRITE ( UNIT=LUN_VEX, FMT='(A)' ) 'scan No'//STR(1:4)//';'
              WRITE ( UNIT=LUN_VEX, FMT='(A)' ) '    mode=512Mbps;'
!           ELSE IF ( SUR%ALGORITHM == 'ASTROMET_01' ) THEN
!              WRITE ( UNIT=LUN_VEX, FMT='(A)' ) 'scan No'//STR(1:4)//';'
!              IF ( SUR%SCAN_TYPE(J5) == SUR__LONG ) THEN
!                   WRITE ( UNIT=LUN_VEX, FMT='(A)' ) '    mode=VERA7MM;'
!                 ELSE IF ( SUR%SCAN_TYPE(J5) == SUR__TAPE ) THEN
!                   WRITE ( UNIT=LUN_VEX, FMT='(A)' ) '    mode=VERA7MM_TC;'
!              END IF
           ELSE IF ( SUR%ALGORITHM == 'ASTROMET_01' .OR. &
     &               SUR%ALGORITHM == 'ASTROMET_02' .OR. &
     &               SUR%ALGORITHM == 'ASTROMET_03' .OR. &
     &               SUR%ALGORITHM == 'ASTROMET_04' .OR. &
     &               SUR%ALGORITHM == 'ASTROMET_05' .OR. &
     &               SUR%ALGORITHM == 'ASTROMET_06' .OR. &
     &               SUR%ALGORITHM == 'ASTROMET_07' .OR. &
     &               SUR%ALGORITHM == 'ASTROMET_11' .OR. &
     &               SUR%ALGORITHM == 'ASTROMET_12' .OR. &
     &               SUR%ALGORITHM == 'ASTROMET_13' .OR. &
     &               SUR%ALGORITHM == 'GEODETIC_01' .OR. &
     &               SUR%ALGORITHM == 'GEODETIC_02' .OR. &
     &               SUR%ALGORITHM == 'GEODETIC_03' .OR. &
     &               SUR%ALGORITHM == 'GNSS_01'     .OR. &
     &               SUR%ALGORITHM == 'GNSS_02'     .OR. &
     &               SUR%ALGORITHM == 'SPACECRAFT_01' .OR. &
     &               SUR%ALGORITHM == 'IMAGING_01'       ) THEN
              WRITE ( UNIT=LUN_VEX, FMT='(A)' ) 'scan No'//STR(1:4)//';'
              IF ( J5 == SUR%L_SCN ) THEN
                   WRITE ( UNIT=LUN_VEX, FMT='(A)' ) '    mode='// &
     &                     MODE_FINAL(1:I_LEN(MODE_FINAL))//';'
                 ELSE
                   IF ( SUR%SRC_TYP(J5) == SUR__TYP_POC ) THEN
                        WRITE ( UNIT=LUN_VEX, FMT='(A)' ) '    mode='// &
     &                          MODE_POINTING(1:I_LEN(MODE_POINTING))//';'
                     ELSE IF ( SUR%SRC_TYP(J5) == SUR__TYP_PLA ) THEN
                        WRITE ( UNIT=LUN_VEX, FMT='(A)' ) '    mode='// &
     &                          MODE_FLUX_CALIB(1:I_LEN(MODE_FLUX_CALIB))//';'
                     ELSE IF ( SUR%SCAN_TYPE(J5) == SUR__FIRST ) THEN
                        WRITE ( UNIT=LUN_VEX, FMT='(A)' ) '    mode='// &
     &                          MODE_START(1:I_LEN(MODE_START))//';'
                      ELSE IF ( SUR%SCAN_TYPE(J5) == SUR__SHORT ) THEN
                        WRITE ( UNIT=LUN_VEX, FMT='(A)' ) '    mode='// &
     &                          MODE_SHORT(1:I_LEN(MODE_SHORT))//';'
                      ELSE IF ( SUR%SCAN_TYPE(J5) == SUR__LONG ) THEN
                        WRITE ( UNIT=LUN_VEX, FMT='(A)' ) '    mode='// &
     &                          MODE_LONG(1:I_LEN(MODE_LONG))//';'
                      ELSE IF ( SUR%SCAN_TYPE(J5) == SUR__TAPE ) THEN
                        WRITE ( UNIT=LUN_VEX, FMT='(A)' ) '    mode='// &
     &                          MODE_TAPE_CHANGE(1:I_LEN(MODE_TAPE_CHANGE))//';'
                      ELSE IF ( SUR%SCAN_TYPE(J5) == SUR__LAST ) THEN
                        WRITE ( UNIT=LUN_VEX, FMT='(A)' ) '    mode='// &
     &                          MODE_FINAL(1:I_LEN(MODE_FINAL))//';'
                   END IF
              END IF
         END IF
!
!%         IF ( SUR%SRC_TYP(J5) == SUR__TYP_CAL ) THEN
!%              IF ( BUF_VEX_HEAD(1)(1:14) == 'VEX_rev = 2.0;' ) THEN
!%                   WRITE ( UNIT=LUN_VEX, FMT='(A)' ) '    intent = : CALIBRATE_BANDPASS : TRUE;'
!%                   WRITE ( UNIT=LUN_VEX, FMT='(A)' ) '    intent = : FRINGE_FINDER      : TRUE;'
!%                 ELSE 
!%                   WRITE ( UNIT=LUN_VEX, FMT='(A)' ) '*vex1  intent = : CALIBRATE_BANDPASS : TRUE;'
!%                   WRITE ( UNIT=LUN_VEX, FMT='(A)' ) '*vex1  intent = : FRINGE_FINDER      : TRUE;'
!%              END IF
!%         END IF
!
         CALL ERR_PASS ( IUER, IER )
         STR_VEX_DATE = MJDSEC_TO_VEX ( SUR%MJD_OBS_BEG(J5), &
     &                                  SUR%TAI_OBS_BEG(J5) + SUR%UTC_M_TAI, IER )
         IF ( IER .NE. 0 ) THEN
              WRITE ( 6, * ) ' MJD/TAI = ', SUR%MJD_OBS_BEG(J5), &
     &                                      SUR%TAI_OBS_BEG(J5)
              CALL CLRCH ( STR )
              CALL INCH ( J5, STR )
              CALL ERR_LOG ( 1527, IUER, 'SUR_PRINT_RES', 'Wrong begin date '// &
     &            'for scan '//STR )
              RETURN
         END IF
!
         STR_VEX_END_DATE = MJDSEC_TO_VEX ( SUR%MJD_OBS_END(J5), &
     &                                      SUR%TAI_OBS_END(J5) + SUR%UTC_M_TAI, IER )
!
         NOM_SCA_LEN = (SUR%MJD_OBS_END(J5) - SUR%MJD_OBS_BEG(J5))*86400.0D0 + &
     &                 (SUR%TAI_OBS_END(J5) - SUR%TAI_OBS_BEG(J5))
         SLEW_2_STA = 0.0D0
         SLEW_3_STA = 0.0D0
         IF ( J5 > 1 ) THEN
              DO 460 J6=1,SUR%L_STA
                 IF ( SUR%OBS_STA(J6,J5-1) .EQ. SUR__USED ) THEN
                      SLEW_STA_SRT(J6) = SUR%SLEW_DUR(J6,J5)
                    ELSE 
                      SLEW_STA_SRT(J6) = 0.1D0
                 END IF
 460          CONTINUE 
              CALL SORT_R8 ( SUR%L_STA, SLEW_STA_SRT )
              KS_STA = 0
              DO 470 J7=1,SUR%L_STA
                  IF ( SLEW_STA_SRT(J7) > 0.01 ) THEN
                       KS_STA = KS_STA + 1
                       IF ( KS_STA == 2  ) THEN
                            SLEW_2_STA = SLEW_STA_SRT(J7)
                          ELSE IF ( KS_STA == 3  ) THEN
                            SLEW_3_STA = SLEW_STA_SRT(J7)
                       END IF
                   END IF
 470          CONTINUE 
         END IF
         IF ( J5 > 1 ) THEN
              NOM_SLEW_TIME = (SUR%MJD_OBS_BEG(J5) - SUR%MJD_OBS_END(J5-1))*86400.0D0 + &
     &                        (SUR%TAI_OBS_BEG(J5) - SUR%TAI_OBS_END(J5-1)) - &
     &                        SUR%PREOBS_SHORT
            ELSE
              NOM_SLEW_TIME = 0.0D0
         END IF
         ADJ_SLEW_TIME = 0.0D0
         UTC_ADJ_START_RAW = 0.0D0
         IF ( SUR%KEY_TYP == KEY__START_STOP      .OR. &
     &        SUR%KEY_TYP == KEY__START_STOP_2STA .OR. &
     &        SUR%KEY_TYP == KEY__START_STOP_3STA      ) THEN
              IF ( J5 == 1 ) THEN
                   STR_VEX_START = MJDSEC_TO_VEX ( SUR%MJD_START, &
     &                                             SUR%TAI_START + SUR%UTC_M_TAI, IER )
                 ELSE 
!
! ---------------- Slewing for all antennas
!
                   SLEW_1ST = SUR%MJD_OBS_BEG(J5)*86400.0D0 + SUR%TAI_OBS_BEG(J5) - &
     &                      ( SUR%MJD_OBS_END(J5-1)*86400.0D0 + SUR%TAI_OBS_END(J5-1) )
                   IF ( SUR%KEY_TYP == KEY__START_STOP_2STA ) THEN
                        SLEW_1ST = SLEW_2_STA
                      ELSE IF ( SUR%KEY_TYP == KEY__START_STOP_3STA ) THEN
                        SLEW_1ST = SLEW_3_STA
                   END IF                    
!
! ---------------- 
! ----------------  eeeeeeeee  End of J5-1 scan
! ---------------- 
! ----------------       oooo  SLEW_1ST
! ---------------- 
! ----------------         e1  end of slewing antenna #1
! ----------------         e2  end of slewing antenna #2 bbbbbbbbbbb  Adjusted scan start time
! ----------------         e4  end of slewing antenna #4
! ---------------- 
! ---------------- 
! ----------------  BBBBBBBBB  Nominal start of the J5-1th scan
! ---------------- 
! ----------------  NOM_SLEW_TIME -- nominal slewing time: interval from EEE(J5-1) to BBB(J5)
! ----------------  SCA_START_OFF -- bbb - BBB -- offset of adjusted scan start time wrt nominal start time.
! ----------------                                Usually it is negative
! ----------------  ADJ_SLEW_TIME -- adjusted slewing time: s2 - eee, i.e. tim inergal between 
! ----------------                   adjusted start time and the end of the previous scan
! ----------------  NB: recording pause is added to the slewing time
! ---------------- 
!
                   IF ( SLEW_1ST > 0.01D0 .AND. &
     &                  SUR%MJD_OBS_BEG(J5)*86400.0D0   + SUR%TAI_OBS_BEG(J5) < &
     &                  SUR%MJD_OBS_END(J5-1)*86400.0D0 + SUR%TAI_OBS_END(J5-1) + DABS(SUR%RECORDING_PAUSE) ) THEN
!
! --------------------- Nominal slew time is shorter than recording pause. 
! --------------------- Make the adjusted scan start time equal to the recording pause
!
                        MJD_ADJ_START = SUR%MJD_OBS_END(J5-1)
                        UTC_ADJ_START_RAW = SUR%TAI_OBS_END(J5-1) + DABS(SUR%RECORDING_PAUSE) + &
     &                                      SUR%UTC_M_TAI
                        UTC_ADJ_START = UPROUND ( SUR%TAI_OBS_END(J5-1) + DABS(SUR%RECORDING_PAUSE) + &
     &                                            SUR%UTC_M_TAI , 1.0D0, ROU_EPS )
                        STR_VEX_START = MJDSEC_TO_VEX ( MJD_ADJ_START, UTC_ADJ_START, IER )
                        SCA_START_OFF = DABS(SUR%RECORDING_PAUSE) - NOM_SLEW_TIME 
                        ADJ_SLEW_TIME = 0.0 ! DABS(SUR%RECORDING_PAUSE) 
                      ELSE IF ( SLEW_1ST > 0.01D0 ) THEN
                        MJD_ADJ_START = SUR%MJD_OBS_END(J5-1)
                        UTC_ADJ_START_RAW = SUR%TAI_OBS_END(J5-1) + SLEW_1ST + DABS(SUR%RECORDING_PAUSE) + &
     &                                      SUR%UTC_M_TAI
                        UTC_ADJ_START = UPROUND ( SUR%TAI_OBS_END(J5-1) + SLEW_1ST + DABS(SUR%RECORDING_PAUSE) + &
     &                                            SUR%UTC_M_TAI, 1.0D0, ROU_EPS )
                        STR_VEX_START = MJDSEC_TO_VEX ( MJD_ADJ_START, UTC_ADJ_START, IER )
                        SCA_START_OFF = SLEW_1ST - NOM_SLEW_TIME + DABS(SUR%RECORDING_PAUSE)
                        ADJ_SLEW_TIME = SLEW_1ST
                      ELSE 
                        STR_VEX_START = MJDSEC_TO_VEX ( SUR%MJD_OBS_BEG(J5), &
     &                                                  SUR%TAI_OBS_BEG(J5) + SUR%UTC_M_TAI, IER )
                        SCA_START_OFF = 0.0
                        ADJ_SLEW_TIME = NOM_SLEW_TIME
                   END IF
              END IF
            ELSE
              SLEW_1ST = NOM_SLEW_TIME + DABS(SUR%RECORDING_PAUSE)
              ADJ_SLEW_TIME = SLEW_1ST
         END IF
         STR_VEX_START = MJDSEC_TO_VEX ( SUR%MJD_OBS_BEG(J5), &
     &                                   SUR%TAI_OBS_BEG(J5) + SUR%UTC_M_TAI, &
     &                                   IER )
         STR_VEX_STOP = MJDSEC_TO_VEX ( SUR%MJD_OBS_END(J5), &
     &                                  SUR%TAI_OBS_END(J5) + SUR%UTC_M_TAI, &
     &                                  IER )
         STR_VEX_END_DATE = MJDSEC_TO_VEX ( SUR%MJD_OBS_END(J5), &
     &                                      SUR%TAI_OBS_END(J5) + SUR%UTC_M_TAI, IER )
!
         STR_DOY_END = STR_VEX_END_DATE(6:8)
!
         IF ( SUR%RECORDING_PAUSE > 0.0D0  .AND.  &
     &        SUR%SCAN_TYPE(J5) .NE. SUR__TAPE ) THEN
!
              IF ( J5 == 1 ) THEN
                   WRITE ( UNIT=LUN_VEX, FMT='(A)' ) '    start='//STR_VEX_START//'s;'
                 ELSE 
                   STR_VEX_DATE_ORIGSTART = MJDSEC_TO_VEX ( SUR%MJD_OBS_BEG(J5), &
     &                                                      SUR%TAI_OBS_BEG(J5) + DABS(SUR%RECORDING_PAUSE) + &
     &                                                      SUR%UTC_M_TAI, IER )
                   WRITE ( UNIT=LUN_VEX, FMT='(A)' ) '*   start='//STR_VEX_DATE_ORIGSTART//'s; <= original start, modified for recording pause'
                   WRITE ( UNIT=LUN_VEX, FMT='(A)' ) '    start='//STR_VEX_START//'s;'
              END IF
            ELSE 
              WRITE ( UNIT=LUN_VEX, FMT='(A)' ) '    start='//STR_VEX_START//'s;'
         END IF
         WRITE ( UNIT=LUN_VEX, FMT='(A)' ) '*    stop='//STR_VEX_STOP//'s;'
         IF ( SUR%ALGORITHM == 'FRINGE_SEARCH_01' ) THEN
              WRITE ( UNIT=LUN_VEX, FMT='(A)' ) '    source1='//B1950_NAME//';'
            ELSE IF ( SUR%ALGORITHM == 'FRINGE_SEARCH_02' ) THEN
              WRITE ( UNIT=LUN_VEX, FMT='(A)' ) '    source='//B1950_NAME//';'
            ELSE IF ( SUR%ALGORITHM == 'ASTROMET_01' ) THEN
              IF ( ILEN(PAIR_NAME) == 0 ) THEN
!!                   WRITE ( UNIT=LUN_VEX, FMT='(A)' ) '    source1=DUMMY;'
                   WRITE ( UNIT=LUN_VEX, FMT='(A)' ) '    source='//B1950_NAME//';'
                 ELSE
                   WRITE ( UNIT=LUN_VEX, FMT='(A)' ) '    source1='//PAIR_NAME//';'
                   WRITE ( UNIT=LUN_VEX, FMT='(A)' ) '    source2='//B1950_NAME//';'
              END IF
            ELSE IF ( SUR%ALGORITHM == 'ASTROMET_02'   .OR. &
     &                SUR%ALGORITHM == 'ASTROMET_03'   .OR. &
     &                SUR%ALGORITHM == 'ASTROMET_04'   .OR. &
     &                SUR%ALGORITHM == 'ASTROMET_05'   .OR. &
     &                SUR%ALGORITHM == 'ASTROMET_06'   .OR. &
     &                SUR%ALGORITHM == 'ASTROMET_07'   .OR. &
     &                SUR%ALGORITHM == 'ASTROMET_11'   .OR. &
     &                SUR%ALGORITHM == 'ASTROMET_12'   .OR. &
     &                SUR%ALGORITHM == 'ASTROMET_13'   .OR. &
     &                SUR%ALGORITHM == 'GEODETIC_01'   .OR. &
     &                SUR%ALGORITHM == 'GEODETIC_02'   .OR. &
     &                SUR%ALGORITHM == 'GEODETIC_03'   .OR. &
     &                SUR%ALGORITHM == 'GNSS_01'       .OR. &
     &                SUR%ALGORITHM == 'GNSS_02'       .OR. &
     &                SUR%ALGORITHM == 'SPACECRAFT_01' .OR. &
     &                SUR%ALGORITHM == 'IMAGING_01'         ) THEN
              WRITE ( UNIT=LUN_VEX, FMT='(A)' ) '    source='//B1950_NAME//';'
         END IF
!
         K_STA = 0
         CALL CLRCH ( STA_STR )
!
!%    write ( 6, * ) 'SPR-1343 scan= ', int2(j5), ' ng= ', sur%n_gap, ' mjd/tai= ', sur%mjd_obs_beg(j5), sngl(sur%tai_obs_beg(j5))     ! %%%%%
!%    write ( 6, * ) 'SPR-1343 gap1= ', sur%mjd_gap(1,1), ' tai_beg1= ', sngl(sur%tai_gap(1,1)), ' tai_end1= ', sngl(sur%tai_gap(2,1)) ! %%%%%
!%    write ( 6, * ) 'SPR-1344 gap2= ', sur%mjd_gap(1,2), ' tai_beg2= ', sngl(sur%tai_gap(1,2)), ' tai_end2= ', sngl(sur%tai_gap(2,2)) ! %%%%%
!%    write ( 6, * ) 'SPR-1345 gap3= ', sur%mjd_gap(1,3), ' tai_beg3= ', sngl(sur%tai_gap(1,3)), ' tai_end3= ', sngl(sur%tai_gap(2,3)) ! %%%%%
         IF ( J5 > 1 .AND. SUR%N_GAP > 0 ) THEN
              DO 480 J8=1,SUR%N_GAP
                 IF ( (SUR%MJD_OBS_BEG(J5-1) - SUR%MJD_GAP(2,J8))*86400.0D0 + &
     &                (SUR%TAI_OBS_BEG(J5-1) - SUR%TAI_GAP(2,J8))             < 0.0D0 .AND. &
                      (SUR%MJD_OBS_BEG(J5)   - SUR%MJD_GAP(2,J8))*86400.0D0   + &
     &                (SUR%TAI_OBS_BEG(J5)   - SUR%TAI_GAP(2,J8))             > 0.0D0       ) THEN
!
!%    write ( 6, * ) 'SPR-1364 scan= ', int2(j5), ' gap ', j8 ! %%%%%%%%%%%%%%
                      STR1 = MJDSEC_TO_DATE ( SUR%MJD_GAP(1,J8), SUR%MJD_GAP(1,J8) + SUR%UTC_M_TAI, -2 )
                      STR2 = MJDSEC_TO_DATE ( SUR%MJD_GAP(2,J8), SUR%MJD_GAP(2,J8) + SUR%UTC_M_TAI, -2 )
                      WRITE ( UNIT=LUN_KEY, FMT='(A)' ) '!'
                      WRITE ( UNIT=LUN_KEY, FMT='(A,I2,A,1X,A,2X,A,1X,A)' ) '! COMMENT: schedule gap ', &
     &                        J8, ' ', STR1(1:19), STR2(1:19),'UTC'
                      STR  = MJDSEC_TO_DATE ( SUR%MJD_OBS_BEG(J5), SUR%TAI_OBS_BEG(J5) + SUR%UTC_M_TAI, -2 )
                      STR1 = MJDSEC_TO_DATE ( SUR%MJD_OBS_END(J5-1), SUR%TAI_OBS_END(J5-1) + SUR%UTC_M_TAI, -2 )
                      WRITE ( UNIT=LUN_KEY, FMT='(A)' ) '! COMMENT: Schedule is resumed at '//STR(1:19)// &
     &                                                  '  Last scan ended at '//STR1(1:19)
!
                      CALL ERR_PASS ( IUER, IER )
                      LST_PT = GET_LST ( SUR%MJD_OBS_BEG(J5), SUR%TAI_OBS_BEG(J5) + SUR%UTC_M_TAI, &
     &                                   STA_LST, SUR, VTD, S_ANG, IER )
                      IF ( IER .NE. 0 ) THEN
                           CALL ERR_LOG ( 1528, IUER, 'SUR_PRINT_RES', 'Failure to '// &
     &                         'compute LST time for '//STA_LST )
                           RETURN 
                      END IF
!
                      CALL  CLRCH ( STR )
                      IER = -1
                      CALL RH_TAT ( LST_PT, 0, STR, IER )
                      CALL CHASHL ( STR )
                      STR(3:3) = ':'
                      STR(6:6) = ':'
!
                      WRITE ( UNIT=LUN_KEY, FMT='(A,I1,A)' ) '! COMMENT: segment ', J8+1, ' year  = '//STR_VEX_DATE(1:4)
                      WRITE ( UNIT=LUN_KEY, FMT='(A,I1,A)' ) '! COMMENT: segment ', J8+1, ' day   = '//STR_VEX_DATE(6:8)
                      WRITE ( UNIT=LUN_KEY, FMT='(A,I1,A)' ) '! COMMENT: segment ', J8+1, ' start = '//STR(1:8)
                      WRITE ( UNIT=LUN_KEY, FMT='(A)'      ) '!'
                 END IF
 480          CONTINUE 
         END IF
!%    write ( 6, * ) 'SPR-1373 ' ! %%%%%%%%%%%%%%%%
         IF ( SUR%POCAL_STYLE == POCAL_STYLE__GBT_4HR  .AND. &
     &        SUR%SOU_POCAL(J5) .NE. 0                      ) THEN
              WRITE ( UNIT=LUN_KEY, FMT='(A)' ) "COMMENT= 'GBT pointing scan.'"
              WRITE ( UNIT=LUN_KEY, FMT='(A)' ) "PEAK= 1"
              WRITE ( UNIT=LUN_KEY, FMT='(A)' ) "STATIONS=  GBT_COLD"
!
              STR_VEX_START = MJDSEC_TO_VEX ( SUR%MJD_POCAL(J5), &
     &                           SUR%TAI_POCAL(J5) + SUR%UTC_M_TAI, &
     &                           IER )
              STR_VEX_STOP  = MJDSEC_TO_VEX ( SUR%MJD_POCAL(J5), &
     &                           SUR%TAI_POCAL(J5) + SUR%UTC_M_TAI + POCAL_DUR__GBT_4HR, &
     &                           IER )
              IF ( SUR%KEY_TYP == KEY__TIME_ABS  .OR. &
     &             SUR%KEY_TYP == KEY__TIME_DUR       ) THEN
                   WRITE ( UNIT=LUN_KEY, FMT='(A)' ) &
     &                     "year= "//STR_VEX_START(1:4)// &
     &                     "  day= "//STR_VEX_STOP(6:8)// &
     &                     "  start= "//STR_VEX_START(10:11)// &
     &                     ":"//STR_VEX_START(13:14)// &
     &                     ":"//STR_VEX_START(16:17)// &
     &                     "  source= "// &
     &                     SUR%SOP(SUR%SOU_POCAL(J5))%B1950_NAME// &
     &                     "  dur= 480  vlamode='VA' norecord /"
               ELSE IF ( SUR%KEY_TYP == KEY__START_STOP ) THEN
                 WRITE ( UNIT=LUN_KEY, FMT='(A)' ) &
                      "year= "//STR_VEX_START(1:4)// &
     &                "  day= "//STR_VEX_START(6:8)// &
     &                "  start= "//STR_VEX_START(10:11)// &
     &                ":"//STR_VEX_START(13:14)// &
     &                ":"//STR_VEX_START(16:17)// &
     &                " year= "//STR_VEX_STOP(1:4)// &
     &                "  day= "//STR_VEX_STOP(6:8)// &
     &                "  stop= "//STR_VEX_STOP(10:11)// &
     &                ":"//STR_VEX_STOP(13:14)// &
     &                ":"//STR_VEX_STOP(16:17)// &
     &                " source= "//B1950_NAME// &
     &                " vlamode='VA' norecord / Pointing scan for GBT"
                 ELSE 
                   WRITE ( UNIT=LUN_KEY, FMT='(A)' ) "  source= "// &
     &                     SUR%SOP(SUR%SOU_POCAL(J5))%B1950_NAME// &
     &                     "  duration= 480  vlamode='VA' norecord /"
              END IF
!
              WRITE ( UNIT=LUN_KEY, FMT='(A)' ) 'NOPEAK'
              WRITE ( UNIT=LUN_KEY, FMT='(A)' ) 'RECORD'
              WRITE ( UNIT=LUN_KEY, FMT='(A)' ) ' '
         END IF
         STR_KEY_STATIONS = "stations= "
         NSL = 0
         DO 4120 j12=1,SUR%L_STA
            IF ( J5 > 1 ) THEN
                 IF ( SUR%SLEW_DUR(J12,J5) + SUR%STA(J12)%PREOB > 0.99D0 ) THEN
                      NSL = NSL + 1
                      SLEW_ARR(NSL) = SUR%SLEW_DUR(J12,J5) + SUR%STA(J12)%PREOB 
                 END IF
                 IF ( SUR%SCA_PREV(J12,J5) > 0 ) THEN
                      SUR%STA(J12)%EL_CUR = SUR%EL_OBS(J12,SUR%SCA_PREV(J12,J5))
                      SUR%STA(J12)%AZ_CUR = SUR%AZ_OBS(J12,SUR%SCA_PREV(J12,J5))
                      SUR%STA(J12)%HA_CUR = SUR%HA_OBS(J12,SUR%SCA_PREV(J12,J5))
                      SUR%STA(J12)%AZ_ACC_CUR = SUR%AZ_ACC_OBS(J12,SUR%SCA_PREV(J12,J5))
                      SUR%STA(J12)%HA_ACC_CUR = SUR%HA_ACC_OBS(J12,SUR%SCA_PREV(J12,J5))
                    ELSE
                      SUR%STA(J12)%EL_CUR = 0.0D0
                      SUR%STA(J12)%AZ_CUR = 0.0D0
                      SUR%STA(J12)%HA_CUR = 0.0D0 
                      SUR%STA(J12)%AZ_ACC_CUR = 0.0D0
                      SUR%STA(J12)%HA_ACC_CUR = 0.0D0
                      SUR%SCA_PREV(J12,J5) = J5-1
                 END IF
            END IF
            IF ( NSL == 0 ) THEN
                 SLEW_2_STA = 1.0D0
               ELSE IF ( NSL == 1 ) THEN
                 SLEW_2_STA = SLEW_ARR(1) 
               ELSE
                 CALL SORT_R8 ( NSL, SLEW_ARR )
                 SLEW_2_STA = SLEW_ARR(2)
            END IF
 4120    CONTINUE 
         DO 4130 J13=1,SUR%L_STA
            FL_STA_USED(J13) = .FALSE.
            IF ( J5 > 1 ) THEN
                 IF ( SUR%SCA_PREV(J13,J5) > 0 ) THEN
                      SUR%MJD_CUR = SUR%MJD_OBS_END(SUR%SCA_PREV(J13,J5))   
                      SUR%TAI_CUR = SUR%TAI_OBS_END(SUR%SCA_PREV(J13,J5))
                      FL_PREV = .TRUE.
                    ELSE
                      SUR%MJD_CUR = SUR%MJD_START
                      SUR%TAI_CUR = SUR%TAI_START
                      FL_PREV = .FALSE.
                 END IF
            END IF
!
            IF ( J5 > 1  .AND.  SUR%STA(J13)%TAGALONE ) THEN
!
! -------------- This is a tag-alone station. We need to check, whether
! -------------- this station can observe this scan
!
                 TIME_SLEW = 0.0001D0
                 IF ( LAST_SCA(J13) > 0 ) THEN
                      CALL ERR_PASS ( IUER, IER )
                      MJD_CUR_SAVE = SUR%MJD_CUR 
                      TAI_CUR_SAVE = SUR%TAI_CUR 
                      SUR%MJD_CUR = SUR%MJD_OBS_END(J5)
                      SUR%TAI_CUR = SUR%TAI_OBS_END(J5)
                      IER = -1
                      TIME_SLEW = SUR_SLEW_TIME ( SUR, VTD, SUR%SRC_TYP(J5) , &
     &                                            SUR%IND_SRC(J5), &
     &                                            SUR%IND_SRC(LAST_SCA(J13)), &
     &                                            SUR%SRC_TYP(LAST_SCA(J13)), &
     &                                            J13, SUR__FINE, IER )
                      IF ( .NOT. SUR_CHECK_VIS ( SUR, J13, SUR%SRC_TYP(J5), SUR%IND_SRC(J5), &
     &                                           AZ, EL, HA, IER ) ) THEN
                           TIME_SLEW = -1.0D0
                      END IF
                      SUR%MJD_CUR = MJD_CUR_SAVE
                      SUR%TAI_CUR = TAI_CUR_SAVE
                      IF ( IER .NE. 0 ) THEN
                           CALL ERR_LOG ( 1529, IUER, 'SUR_PRINT_RES', 'Error in '// &
     &                         'an attempt to compute slew time' )
                           RETURN
                      END IF
!
                      DUR_SA = (SUR%TAI_OBS_END(J5) - SUR%TAI_OBS_END(LAST_SCA(J13)) - TIME_SLEW - SUR%PREOBS_LONG ) + &
     &                         (SUR%MJD_OBS_END(J5) - SUR%MJD_OBS_END(LAST_SCA(J13)))*86400.0D0
                    ELSE IF ( LAST_SCA(J13) == 0 ) THEN
                      DUR_SA = DUR_SCAN
                      TIME_SLEW = 0.001D0
                      CALL ERR_PASS ( IUER, IER )
                      CALL SUR_AZEL ( SUR, VTD, SUR%SRC_TYP(J5), &
     &                                SUR%MJD_OBS_BEG(J5), &
     &                                SUR%TAI_OBS_BEG(J5), &
     &                                J13, SUR%IND_SRC(J5), &
     &                                AZ, EL, HA, IER )
                      IF ( .NOT. SUR_CHECK_VIS ( SUR, J13, SUR%SRC_TYP(J5), SUR%IND_SRC(J5), &
     &                                           AZ, EL, HA, IER ) ) THEN
                           TIME_SLEW = -1.0D0
                      END IF
                 END IF
                 IF ( TIME_SLEW < 0.0D0 ) THEN
!
! ------------------- The scan is rejected because the tag-alone station
! ------------------- cannot see the source
!
                      GOTO 4130
                    ELSE IF ( DUR_SA < SUR__SHARE_SA * DUR_SCAN ) THEN
!
! ------------------- The scan is rejected for this tag-alone station,
! ------------------- because there is no time enough to slew
!
                      GOTO 4130
                    ELSE
!
! ------------------- The scan is accepted for this tag-alone station
!
! ------------------- Compute azimuth, elevation and hour angle for the J13th station 
! 
                      SPL_STATUS = SUR%STATUS_SPL(SUR%SRC_TYP(J5))
                      SUR%STATUS_SPL(SUR%SRC_TYP(J5)) = 0
                      CALL ERR_PASS ( IUER, IER )
                      CALL SUR_AZEL ( SUR, VTD, SUR%SRC_TYP(J5), &
     &                                SUR%MJD_OBS_BEG(J5), &
     &                                SUR%TAI_OBS_BEG(J5), &
     &                                J13, SUR%IND_SRC(J5), &
     &                                AZ, EL, HA, IER )
                      SUR%STATUS_SPL(SUR%SRC_TYP(J5)) = SUR%STATUS_SPL(SUR%SRC_TYP(J5)) 
!
! ------------------- Just for the case, check visibility once again
!
                      IF ( SUR%STA(J13)%MOUNT_TYPE == MT__ALTAZ ) THEN
!
! ------------------------ Update accumulative aziumth in order to take into account
! ------------------------ cable wrap
!
                           DIF_AZ = (AZ - SUR%STA(J13)%AZ_CUR)
                           DIF_AZ = DIF_AZ - PI2*IDNINT(DIF_AZ/PI2)
!
                           IF ( DIF_AZ > 0.0D0 ) THEN
!
! ----------------------------- The shortest move is clock-wise
!
                                IF ( SUR%STA(J13)%AZ_ACC_CUR + DIF_AZ < SUR%STA(J13)%AZ_ACC_MAX - AZ__MARGIN  ) THEN
!
                                     SUR%STA(J13)%AZ_ACC_CUR = SUR%STA(J13)%AZ_ACC_CUR + DIF_AZ
                                   ELSE
!
! ---------------------------------- The shortest way is not possible, move the longest way
! ---------------------------------- counter-clock-wise
!
                                     SUR%STA(J13)%AZ_ACC_CUR = SUR%STA(J13)%AZ_ACC_CUR - &
     &                                                         (PI2 - DIF_AZ)
                                     DIF_AZ = (PI2 - DIF_AZ)
                                END IF
                              ELSE
!
! ----------------------------- The shortest move is counter-clock-wise
!
                                IF ( SUR%STA(J13)%AZ_ACC_CUR + DIF_AZ > SUR%STA(J13)%AZ_ACC_MIN - AZ__MARGIN  ) THEN
                                     SUR%STA(J13)%AZ_ACC_CUR = SUR%STA(J13)%AZ_ACC_CUR + DIF_AZ
                                  ELSE
!
! ---------------------------------- The shortest way is not possible, move the longest way
! ---------------------------------- clock-wise
!
                                     SUR%STA(J13)%AZ_ACC_CUR = SUR%STA(J13)%AZ_ACC_CUR + &
     &                                                         (PI2 + DIF_AZ)
                                     DIF_AZ = (PI2 + DIF_AZ)
                                END IF
                           END IF
!
! ------------------------ Compute the slew time
!
                           IF ( DABS(DIF_AZ) > SUR%STA(J13)%SLEW_RATE_AZ**2/SUR%STA(J13)%SLEW_ACCL_AZ ) THEN
                                SLEW_AZ = (DABS(DIF_AZ) - SUR%STA(J13)%SLEW_RATE_AZ**2/SUR%STA(J13)%SLEW_ACCL_AZ)/SUR%STA(J13)%SLEW_RATE_AZ + &
     &                                     2.0D0*SUR%STA(J13)%SLEW_RATE_AZ/SUR%STA(J13)%SLEW_ACCL_AZ
                              ELSE
                                SLEW_AZ = 2.D0*DSQRT(DABS(DIF_AZ)/SUR%STA(J13)%SLEW_ACCL_AZ)
                           END IF
                           IF ( DABS(DIF_EL) > SUR%STA(J13)%SLEW_RATE_EL**2/SUR%STA(J13)%SLEW_ACCL_EL ) THEN
                                SLEW_EL = DABS(DIF_EL)/SUR%STA(J13)%SLEW_RATE_EL + &
     &                                         SUR%STA(J13)%SLEW_RATE_EL/SUR%STA(J13)%SLEW_ACCL_EL
                             ELSE
                                SLEW_EL = 2.D0*DSQRT(DABS(DIF_EL)/SUR%STA(J13)%SLEW_ACCL_EL)
                           END IF
                           TIME_SLEW = MAX(SLEW_AZ+SUR%STA(J13)%TIME_SETTLE_AZ,   &
     &                                     SLEW_EL+SUR%STA(J13)%TIME_SETTLE_EL) + &
     &                                 SUR%STA(J13)%POSTOB
                           SUR%AZ_ACC_OBS(J13,J5) = SUR%STA(J13)%AZ_ACC_CUR
                           SUR%HA_ACC_OBS(J13,J5) = HA
                        ELSE IF ( SUR%STA(J13)%MOUNT_TYPE == MT__EQUAT ) THEN
                           IF ( SUR%SRC_TYP(J5) == SUR__TYP_TAG ) THEN
                                 ALP = SUR%SOU(SUR%IND_SRC(J5))%ALPHA
                                 DEL = SUR%SOU(SUR%IND_SRC(J5))%DELTA
                              ELSE IF ( SUR%SRC_TYP(J5) == SUR__TYP_SEC ) THEN
                                 ALP = SUR%SO2(SUR%IND_SRC(J5))%ALPHA
                                 DEL = SUR%SO2(SUR%IND_SRC(J5))%DELTA
                              ELSE IF ( SUR%SRC_TYP(J5) == SUR__TYP_CAL ) THEN
                                 ALP = SUR%CAL(SUR%IND_SRC(J5))%ALPHA
                                 DEL = SUR%CAL(SUR%IND_SRC(J5))%DELTA
                              ELSE IF ( SUR%SRC_TYP(J5) == SUR__TYP_POC ) THEN
                                 ALP = SUR%SOP(SUR%IND_SRC(J5))%ALPHA
                                 DEL = SUR%SOP(SUR%IND_SRC(J5))%DELTA
                              ELSE IF ( SUR%SRC_TYP(J5) == SUR__TYP_CAL ) THEN
                                 ALP = SUR%PLA(SUR%IND_SRC(J5))%ALPHA
                                 DEL = SUR%PLA(SUR%IND_SRC(J5))%DELTA
                           END IF
                           DIF_DEL = DEL - SUR%STA(J13)%DEL_CUR
                           DIF_HA = (HA - SUR%STA(J13)%HA_CUR)
                           IF ( DABS(DIF_HA) < P2I ) THEN
!
! ----------------------------- The shortest move is clock-wise
!
                                SUR%STA(J13)%HA_ACC_CUR = HA 
                              ELSE
!
! ----------------------------- The shortest move is counter-clock-wise
!
                                IF ( SUR%STA(J13)%HA_ACC_CUR + DIF_HA > SUR%STA(J13)%AZ_ACC_MIN ) THEN
                                     SUR%STA(J13)%HA_ACC_CUR = SUR%STA(J13)%HA_ACC_CUR + DIF_HA
                                   ELSE
!
! ---------------------------------- The shortest way is not possible, move the longest way
! ---------------------------------- clock-wise
!
                                     SUR%STA(J13)%HA_ACC_CUR = SUR%STA(J13)%HA_ACC_CUR + &
     &                                                        (PI2 + DIF_HA)
                               END IF
                           END IF
!
! ------------------------ Compute the slew time
!
                           IF ( DABS(DIF_HA) > SUR%STA(J13)%SLEW_RATE_AZ**2/SUR%STA(J13)%SLEW_ACCL_AZ ) THEN
                                SLEW_HA = (DABS(DIF_HA) - SUR%STA(J13)%SLEW_RATE_AZ**2/SUR%STA(J13)%SLEW_ACCL_AZ)/SUR%STA(J13)%SLEW_RATE_AZ + &
     &                                     2.0D0*SUR%STA(J13)%SLEW_RATE_AZ/SUR%STA(J13)%SLEW_ACCL_AZ
                              ELSE
                                SLEW_HA = 2.D0*DSQRT(DABS(DIF_HA)/SUR%STA(J13)%SLEW_ACCL_AZ)
                           END IF
                           IF ( DABS(DIF_DEL) > SUR%STA(J13)%SLEW_RATE_EL**2/SUR%STA(J13)%SLEW_ACCL_EL ) THEN
                                SLEW_DEL = DABS(DIF_DEL)/SUR%STA(J13)%SLEW_RATE_EL + &
     &                                     SUR%STA(J13)%SLEW_RATE_EL/SUR%STA(J13)%SLEW_ACCL_EL
                              ELSE
                                SLEW_DEL = 2.D0*DSQRT(DABS(DIF_DEL)/SUR%STA(J13)%SLEW_ACCL_EL)
                           END IF
                           TIME_SLEW =  MAX(SLEW_HA+SUR%STA(J13)%TIME_SETTLE_AZ, &
     &                                               SLEW_DEL+SUR%STA(J13)%TIME_SETTLE_EL) + &
     &                                      SUR%STA(J13)%POSTOB
                      END IF
!
                      SUR%AZ_ACC_OBS(J13,J5) = AZ
                      SUR%HA_ACC_OBS(J13,J5) = SUR%STA(J13)%HA_ACC_CUR
!
                      SUR%STA(J13)%AZ_CUR = AZ
                      SUR%STA(J13)%EL_CUR = EL
                      SUR%STA(J13)%HA_CUR = HA
!
                      SUR%AZ_OBS(J13,J5) = AZ
                      SUR%EL_OBS(J13,J5) = EL
                      SUR%HA_OBS(J13,J5) = HA
                 END IF
!
                 DUR_SA = (SUR%TAI_OBS_END(J5) - SUR%TAI_OBS_END(LAST_SCA(J13)) - TIME_SLEW - SUR%PREOBS_LONG ) + &
     &                    (SUR%MJD_OBS_END(J5) - SUR%MJD_OBS_END(LAST_SCA(J13)))*86400.0D0
                 IF ( DUR_SA < SUR__SHARE_SA * DUR_SCAN ) THEN
                      GOTO 4130
                 END IF
                 SUR%SLEW_DUR(J13,J5) = TIME_SLEW
                 SUR%OBS_STA(J13,J5) = SUR__USED
                 DO 4210 J21=J5-1,1,-1
                    IF ( SUR%OBS_STA(J13,J21) == SUR__USED ) THEN
                         SUR%SCA_PREV(J13,J5) = J21
                         GOTO 8210
                    END IF
 4210            CONTINUE 
 8210            CONTINUE 
!
                 IF ( IVRB .GE. 6 ) THEN
                      WRITE ( 6, * ) 'SUR_PRINT_RES. Slewing report for scan ', INT2(J5)
                      L_SCN = SUR%L_SCN
                      SUR%L_SCN = J5
                      CALL SUR_SLEW_REPORT ( SUR )
                      SUR%L_SCN = L_SCN 
                      WRITE ( 6, * ) ' '
                 END IF
                 IF ( IVRB .GE. 5 ) THEN
                      WRITE ( 6 ,* ) 'Added a tagalone station ', SUR%STA(J13)%NAME, ' to scan ', INT2(J5), &
     &                               ' DUR_SA = ', SNGL(DUR_SA), ' Ts = ', SNGL(TIME_SLEW), &
     &                               ' Sd = ', SNGL(SUR%TAI_OBS_END(J5) - SUR%TAI_OBS_END(LAST_SCA(J13)))
                 END IF
            END IF
            WRAP_STR = '&n'
            IF (         SUR%AZ_ACC_OBS(J13,J5) .GE. SUR%STA(J13)%AZ_RANGE(1) .AND. &
     &                   SUR%AZ_ACC_OBS(J13,J5) .LT. SUR%STA(J13)%AZ_RANGE(2)       ) THEN
                 WRAP_STR = '&ccw'
               ELSE IF ( SUR%AZ_ACC_OBS(J13,J5) .GE. SUR%STA(J13)%AZ_RANGE(2) .AND. &
     &                   SUR%AZ_ACC_OBS(J13,J5) .LT. SUR%STA(J13)%AZ_RANGE(3)       ) THEN
                 WRAP_STR = '&n  '
               ELSE IF ( SUR%AZ_ACC_OBS(J13,J5) .GE. SUR%STA(J13)%AZ_RANGE(3) .AND. &
     &                   SUR%AZ_ACC_OBS(J13,J5) .LE. SUR%STA(J13)%AZ_RANGE(4)       ) THEN
                 WRAP_STR = '&cw '
               ELSE
                 IF ( SUR%STA(J13)%MOUNT_TYPE == MT__ALTAZ ) THEN
                      WRITE ( 6, * ) 'SUR_PRINT_RES: Azimuth is out of range Scan= ', int2(j11),' sta ', SUR%STA(J13)%NAME, ' az= ', SNGL(SUR%AZ_ACC_OBS(J13,J5)/DEG__TO__RAD) ! %%%
                 END IF
            END IF
!
            IF ( J5 > 1  ) THEN
                 WRITE ( UNIT=STR(1:4), FMT='(I4)' ) J5
                 CALL BLANK_TO_ZERO ( STR(1:4) )
                 IF ( SUR%OBS_STA(J13,J5) == SUR__USED ) THEN
                      NA = NA + 1 ; WRITE ( UNIT=AST_BUF(NA), FMT='(A)' ) '  Station: '//SUR%STA(J13)%NAME// &
     &                                              ' Scan: '//SCAN_STR_AST// &
     &                                              ' Operation: observing  Source: '// &
     &                                              B1950_NAME//' Alt_source_name: J'//J2000_NAME(2:10)
                      IF ( LAST_SCA(J13) > 0 ) THEN
                           IER = -1
                           STR1 = MJDSEC_TO_DATE ( SUR%MJD_OBS_END(SUR%SCA_PREV(J13,J5)), &
     &                                             SUR%TAI_OBS_END(SUR%SCA_PREV(J13,J5)) + &
     &                                             SUR%STA(J13)%POSTOB + SUR%UTC_M_TAI , IER )
                           IER = -1
                           STR2 = MJDSEC_TO_DATE ( SUR%MJD_OBS_END(SUR%SCA_PREV(J13,J5)), &
     &                                             SUR%TAI_OBS_END(SUR%SCA_PREV(J13,J5)) + &
     &                                             SUR%UTC_M_TAI + SUR%SLEW_DUR(J13,J5), IER )
                         ELSE
                           IER = -1
                           STR1 = MJDSEC_TO_DATE ( SUR%MJD_OBS_END(J5-1), &
     &                                             SUR%TAI_OBS_END(J5-1) + SUR%UTC_M_TAI, IER )
                           IER = -1
                           STR2 = MJDSEC_TO_DATE ( SUR%MJD_OBS_END(J5-1), &
     &                                             SUR%TAI_OBS_END(J5-1) + SUR%UTC_M_TAI, IER )
                      END IF
!
                      EL_OBS_PREV = SUR%EL_OBS(J13,J5)
                      AZ_ACC_OBS_PREV = SUR%AZ_ACC_OBS(J13,J5)
                      HA_ACC_OBS_PREV = SUR%HA_ACC_OBS(J13,J5)
                      SOU_PREV = '          '
                      DO 4220 J22=J5-1,1,-1
                          IF ( SUR%OBS_STA(J13,J22) == SUR__USED ) THEN
                               IF ( SUR%SRC_TYP(J22) == SUR__TYP_TAG ) THEN
                                    SOU_PREV = SUR%SOU(SUR%IND_SRC(J22))%J2000_NAME
                                    SOU_BPRE = SUR%SOU(SUR%IND_SRC(J22))%B1950_NAME
                                 ELSE IF ( SUR%SRC_TYP(J22) == SUR__TYP_SEC ) THEN
                                    SOU_PREV = SUR%SO2(SUR%IND_SRC(J22))%J2000_NAME
                                    SOU_BPRE = SUR%SO2(SUR%IND_SRC(J22))%B1950_NAME
                                 ELSE IF ( SUR%SRC_TYP(J22) == SUR__TYP_CAL ) THEN
                                    SOU_PREV = SUR%CAL(SUR%IND_SRC(J22))%J2000_NAME
                                    SOU_BPRE = SUR%CAL(SUR%IND_SRC(J22))%B1950_NAME
                                 ELSE IF ( SUR%SRC_TYP(J22) == SUR__TYP_POC ) THEN
                                    SOU_PREV = SUR%SOP(SUR%IND_SRC(J22))%J2000_NAME
                                    SOU_BPRE = SUR%SOP(SUR%IND_SRC(J22))%B1950_NAME
                                 ELSE IF ( SUR%SRC_TYP(J22) == SUR__TYP_PLA ) THEN
                                    SOU_PREV = SUR%PLA(SUR%IND_SRC(J22))%J2000_NAME
                                    SOU_BPRE = SUR%PLA(SUR%IND_SRC(J22))%B1950_NAME
                               END IF
                               SOU_PREV(1:1) = 'J'
!
                               EL_OBS_PREV = SUR%EL_OBS(J13,J22)
                               AZ_ACC_OBS_PREV = SUR%AZ_ACC_OBS(J13,J22)
                               HA_ACC_OBS_PREV = SUR%HA_ACC_OBS(J13,J22)
                               GOTO 8220
                          END IF
 4220                 CONTINUE 
 8220                 CONTINUE 
                      IF ( LAST_SCA(J13) == 0 ) THEN
                           EL_OBS_PREV     = SUR%EL_OBS(J13,J5)
                           AZ_ACC_OBS_PREV = SUR%AZ_ACC_OBS(J13,J5)
                           HA_ACC_OBS_PREV = SUR%HA_ACC_OBS(J13,J5)
                           SUR%SLEW_DUR(J13,J5) = 0.001D0
                      END IF
                      IF ( ILEN(SOU_BPRE) == 0 ) SOU_BPRE = B1950_NAME
!
                      SPL_STATUS = SUR%STATUS_SPL(SUR%SRC_TYP(J5))
                      SUR%STATUS_SPL(SUR%SRC_TYP(J5)) = 0
                      CALL ERR_PASS ( IUER, IER )
                      CALL SUR_AZEL ( SUR, VTD, SUR%SRC_TYP(J5), &
     &                                SUR%MJD_OBS_BEG(J5), &
     &                                SUR%TAI_OBS_BEG(J5), &
     &                                J13, SUR%IND_SRC(J5), &
     &                                SUR%AZ_OBS(J13,J5), &
     &                                SUR%EL_OBS(J13,J5), &
     &                                SUR%HA_OBS(J13,J5), IER )
                      SUR%STATUS_SPL(SUR%SRC_TYP(J5)) = SUR%STATUS_SPL(SUR%SRC_TYP(J5)) 
!
                      NA = NA + 1 ; WRITE ( UNIT=AST_BUF(NA), FMT=210 ) SUR%STA(J13)%NAME, &
     &                        STR1(1:21)//' '//STR2(1:21)//' Scan: '//SCAN_STR_AST// &
     &                        ' Sources: '//SOU_BPRE//'  '//B1950_NAME//'  Duration: ', &
     &                        SUR%SLEW_DUR(J13,J5) - SUR%STA(J13)%POSTOB, '  Elevs: ', &
     &                        EL_OBS_PREV/DEG__TO__RAD, &
     &                        SUR%EL_OBS(J13,J5)/DEG__TO__RAD, &
     &                        ' Azims: ', AZ_ACC_OBS_PREV/DEG__TO__RAD, &
     &                        SUR%AZ_ACC_OBS(J13,J5)/DEG__TO__RAD, &
     &                        ' Hour_angles: ', HA_ACC_OBS_PREV/DEG__TO__RAD, &
     &                        SUR%HA_ACC_OBS(J13,J5)/DEG__TO__RAD, '  Wrap: '//TRIM(WRAP_STR)
 210                  FORMAT ( '    Slew:     ',A, 1X, A,F6.1,A,F5.2,1X, &
     &                         F5.2,A,F7.2,1X,F7.2,1X,A,F7.2,1X,F7.2, A )
                    ELSE
                      NA = NA + 1 ; WRITE ( UNIT=AST_BUF(NA), FMT='(A)' ) '  Station: '//SUR%STA(J13)%NAME// &
     &                                              ' Scan: '//SCAN_STR_AST// &
     &                                              ' Operation: skipping   Source: '//B1950_NAME
                 END IF
               ELSE ! 1st observation
                 WRITE ( UNIT=STR(1:4), FMT='(I4)' ) J5
                 CALL BLANK_TO_ZERO ( STR(1:4) )
                 IF ( SUR%OBS_STA(J13,J5) == SUR__USED ) THEN
                      NA = NA + 1 ; WRITE ( UNIT=AST_BUF(NA), FMT='(A)' ) '  Station: '//SUR%STA(J13)%NAME(1:8)//' Scan: no0001  Operation: observing  Source: '//B1950_NAME
                    ELSE
                      NA = NA + 1 ; WRITE ( UNIT=AST_BUF(NA), FMT='(A)' ) '  Station: '//SUR%STA(J13)%NAME(1:8)//' Scan: no0001  Operation: skipping   Source: '//B1950_NAME
                 END IF
                 IER = -1
                 STR1 = MJDSEC_TO_DATE ( SUR%MJD_OBS_BEG(J5), SUR%TAI_OBS_BEG(J5) + SUR%UTC_M_TAI, IER )
                 IER = -1
                 STR2 = MJDSEC_TO_DATE ( SUR%MJD_OBS_BEG(J5), SUR%TAI_OBS_BEG(J5) + SUR%UTC_M_TAI, IER )
                 WRITE ( UNIT=PRESESS_STR, FMT='(F6.1)' ) SUR%PRESES_INTERVAL
                 NA = NA + 1 ; WRITE ( UNIT=AST_BUF(NA), FMT='(A)' ) '    Set_mode: '//SUR%STA(J13)%NAME(1:8)//' '// &
     &                                          STR1(1:21)//' ' //STR2(1:21)// &
     &                                          ' Scan: no0001 Hardware_setup_mode:  '// &
     &                                          TRIM(SUR%HARDWARE_SETUP_NAME)// &
     &                                          ' Wrap:  '//TRIM(WRAP_STR)// &
     &                                          ' Duration: '//TRIM(PRESESS_STR)
!
                 IF ( SUR%OBS_STA(J13,J5) == SUR__USED ) THEN
                      IER = -1
                      STR1 = MJDSEC_TO_DATE ( SUR%MJD_OBS_BEG(J5), SUR%TAI_OBS_BEG(J5) + SUR%UTC_M_TAI, IER )
                      IER = -1
                      STR2 = MJDSEC_TO_DATE ( SUR%MJD_OBS_BEG(J5), SUR%TAI_OBS_BEG(J5) + SUR%STA(J13)%PREOB + SUR%UTC_M_TAI, IER )
                      CALL CLRCH ( STR_DUR )
                      WRITE ( UNIT=STR_DUR, FMT='(F7.1)' ) SUR%STA(J13)%PREOB
                      NA = NA + 1 ; WRITE ( UNIT=AST_BUF(NA), FMT='(A)' ) '    Preob:    '//SUR%STA(J13)%NAME(1:8)// &
     &                                                     ' '//STR1(1:21)//' '//STR2(1:21)// &
     &                                                     ' Scan: no0001 Source:  '//B1950_NAME// &
     &                                                     '  Duration: '//TRIM(STR_DUR)// &
     &                                                     '  Proc_name: Dummy'
!     
                      DUR = (SUR%MJD_OBS_END(J5) - SUR%MJD_OBS_BEG(J5))*86400.0D0 + &
     &                      (SUR%TAI_OBS_END(J5) - SUR%TAI_OBS_BEG(J5)) - SUR%STA(J13)%PREOB
                      IER = -1
                      STR1 = MJDSEC_TO_DATE ( SUR%MJD_OBS_BEG(J5), SUR%TAI_OBS_BEG(J5) + SUR%UTC_M_TAI + SUR%STA(J13)%PREOB, IER )
                      IER = -1
                      STR2 = MJDSEC_TO_DATE ( SUR%MJD_OBS_END(J5), SUR%TAI_OBS_END(J5) + SUR%UTC_M_TAI, IER )
!
                      CALL HR_TAT ( ALPHA_STR, RA,  IER )
                      CALL GR_TAT ( DELTA_STR, DEC, IER )
                      PAR_ANG = DATAN ( DSIN(SUR%HA_OBS(J13,J5))/ &
     &                                ( DCOS(DEC)*DTAN(SUR%STA(J13)%LAT_GDT) - DSIN(DEC)*DCOS(SUR%HA_OBS(J13,J5)) ) )
!
                      NA = NA + 1 ; WRITE ( UNIT=AST_BUF(NA), FMT=220 ) '    Record:  ', SUR%STA(J13)%NAME, &
     &                        STR1(1:21)//' '//STR2(1:21)//' Scan: '//SCAN_STR_AST// &
     &                        ' Source:  '//B1950_NAME//'  Duration: ', DUR, &
     &                        '  Elev: ', SUR%EL_OBS(J13,J5)/DEG__TO__RAD, &
     &                        ' Azim: ', SUR%AZ_OBS(J13,J5)/DEG__TO__RAD, &
     &                        ' Hour_angle: ', SUR%HA_OBS(J13,J5)/DEG__TO__RAD, &
     &                        ' Par_ang: ', PAR_ANG/DEG__TO__RAD
 220                  FORMAT( A, 1X,A, 1X,A, F7.1, A, F5.2, A, F7.2, A, F7.2, A, F7.2 )
                      STR1 = STR2
                      STR2 = MJDSEC_TO_DATE ( SUR%MJD_OBS_END(J5), SUR%TAI_OBS_END(J5) + &
     &                                        SUR%STA(J13)%POSTOB + SUR%UTC_M_TAI, IER )
                      IER = -1
                      WRITE ( UNIT=STR_DUR, FMT='(F7.1)' ) SUR%STA(J13)%POSTOB
                      NA = NA + 1 ; WRITE ( UNIT=AST_BUF(NA), FMT='(A)' ) '    Postob:   '//SUR%STA(J13)%NAME(1:8)// &
     &                                                     ' '//STR1(1:21)//' '//STR2(1:21)// &
     &                                                     ' Scan: no0001 Source:  '//B1950_NAME// &
     &                                                     '  Duration: '//TRIM(STR_DUR)// &
     &                                                     '  Proc_name: dummy'
                 END IF
            END IF
!
            STR = MJDSEC_TO_DATE ( SUR%MJD_OBS_BEG(J5), SUR%TAI_OBS_BEG(J5), -2 )
            IF ( SUR%SCAN_TYPE(J5) == SUR__FIRST ) THEN
                 WORD = 'SCan'
               ELSE IF ( SUR%SCAN_TYPE(J5) == SUR__SHORT ) THEN
                 WORD = 'scan'
               ELSE IF ( SUR%SCAN_TYPE(J5) == SUR__LONG ) THEN
                 WORD = 'Scan'
               ELSE IF ( SUR%SCAN_TYPE(J5) == SUR__TAPE ) THEN
                 WORD = 'SCAN'
               ELSE IF ( SUR%SCAN_TYPE(J5) == SUR__LAST ) THEN
                 WORD = 'scAN'
               ELSE
                 WORD = 'Sca?'
            END IF
!
            IF ( .NOT. SUR%OBS_STA(J13,J5) == SUR__USED ) THEN
                  IF ( SUR%SOU_POCAL(J5) .NE. 0 .AND. J13 == 1 ) THEN
                       STR_POCAL = MJDSEC_TO_DATE ( SUR%MJD_POCAL(J5), &
     &                                              SUR%TAI_POCAL(J5), -2 )
                       WRITE ( UNIT=LUN_PLAN, FMT=110 ) 'POIN', &
     &                    J5, IND_TAP, 'Point.:', &
     &                    SUR%SOP(SUR%SOU_POCAL(J5))%J2000_NAME, &
     &                    STR_POCAL(1:19), SUR%STA(J13)%NAME, &
     &                    SUR%EL_OBS(J13,J5)/DEG__TO__RAD, &
     &                    SUR%AZ_OBS(J13,J5)/DEG__TO__RAD, &
     &                    SUR%AZ_ACC_OBS(J13,J5)/DEG__TO__RAD
                     ELSE 
                       WRITE ( UNIT=LUN_PLAN, &
     &                    FMT='(A,":",I5,12X,A,1X,A,2X,"Skipping",27X,A)' ) WORD, &
     &                    J5, OBJECT, J2000_NAME, SUR%STA(J13)%NAME
                  END IF
                  GOTO 4130
            END IF
!
            K_STA = K_STA + 1
            STA_STR = STA_STR(1:I_LEN(STA_STR))//STA_CODE(J13)//'-'
            FL_STA_USED(J13) = .TRUE.
!
            LAST_SCA(J13) = J5
            IF ( SUR%AZ_OBS(J13,J5) < 0.0D0 ) SUR%AZ_OBS(J13,J5)= SUR%AZ_OBS(J13,J5) + PI2
!
            IF ( SUR%TAPE_CHANGE_TIME > 0.0D0 ) THEN
                 IND_TAP = SUR%IND_TAP(J5)
               ELSE
                 IND_TAP = 1
            END IF
            IF ( K_STA == 1 ) THEN
                 IF ( FL_LBA_KEY .AND. SUR%STA(J13)%SHORT_NAME == 'Pa' ) THEN
                      STR_KEY_STATIONS = STR_KEY_STATIONS(1:I_LEN(STR_KEY_STATIONS))// &
     &                                   '  PARK_MK5'
                    ELSE IF ( FL_LBA_KEY .AND. SUR%STA(J13)%SHORT_NAME == 'Ho' ) THEN
                      STR_KEY_STATIONS = STR_KEY_STATIONS(1:I_LEN(STR_KEY_STATIONS))// &
     &                                   '  HOB_DBBC'
                    ELSE IF ( FL_LBA_KEY .AND. SUR%STA(J13)%SHORT_NAME == 'Ti' ) THEN
                      STR_KEY_STATIONS = STR_KEY_STATIONS(1:I_LEN(STR_KEY_STATIONS))// &
     &                                   '     DSS34'
                    ELSE IF ( FL_LBA_KEY .AND. SUR%STA(J13)%SHORT_NAME == 'Ww' ) THEN
                      STR_KEY_STATIONS = STR_KEY_STATIONS(1:I_LEN(STR_KEY_STATIONS))// &
     &                                   '   WARK12M'
                    ELSE IF ( FL_LBA_KEY .AND. SUR%STA(J13)%SHORT_NAME == 'Wa' ) THEN
                      STR_KEY_STATIONS = STR_KEY_STATIONS(1:I_LEN(STR_KEY_STATIONS))// &
     &                                   '  Wark30m'
                    ELSE IF ( FL_CVN_KEY .AND. SUR%STA(J13)%SHORT_NAME == 'Sh' ) THEN
                      STR_KEY_STATIONS = STR_KEY_STATIONS(1:I_LEN(STR_KEY_STATIONS))// &
     &                                   '  Shanghai'
                    ELSE IF ( FL_CVN_KEY .AND. SUR%STA(J13)%SHORT_NAME == 'Ur' ) THEN
                      STR_KEY_STATIONS = STR_KEY_STATIONS(1:I_LEN(STR_KEY_STATIONS))// &
     &                                   '  Urumqi'
                    ELSE IF ( FL_CVN_KEY .AND. SUR%STA(J13)%SHORT_NAME == 'Km' ) THEN
                      STR_KEY_STATIONS = STR_KEY_STATIONS(1:I_LEN(STR_KEY_STATIONS))// &
     &                                   '  Kunming'
                    ELSE IF ( FL_CVN_KEY .AND. SUR%STA(J13)%SHORT_NAME == 'Kv' ) THEN
                      STR_KEY_STATIONS = STR_KEY_STATIONS(1:I_LEN(STR_KEY_STATIONS))// &
     &                                   '  Sejong'
                    ELSE IF ( FL_LBA_KEY .AND. SUR%STA(J13)%SHORT_NAME == 'Cd' ) THEN
                      STR_KEY_STATIONS = STR_KEY_STATIONS(1:I_LEN(STR_KEY_STATIONS))// &
     &                                   '  CDDBBC'
                    ELSE IF ( FL_VLBA_KEY ) THEN
                      IF ( SUR%STA(J13)%SHORT_NAME == 'GB' ) THEN
                           STR_KEY_STATIONS = STR_KEY_STATIONS(1:I_LEN(STR_KEY_STATIONS))// &
     &                                        '  GBT_COLD'
                         ELSE 
                           IF ( SUR%STA(J13)%NAME(4:4) .NE. '_' ) THEN
                                STR_KEY_STATIONS = STR_KEY_STATIONS(1:I_LEN(STR_KEY_STATIONS))// &
     &                                             '  VLBA_'//TRIM(SUR%STA(J13)%SHORT_NAME)
                              ELSE
                                STR_KEY_STATIONS = STR_KEY_STATIONS(1:I_LEN(STR_KEY_STATIONS))// &
     &                                             '  '//TRIM(SUR%STA(J13)%NAME)
                           END IF
                      END IF
                      CALL TRAN ( 11, STR_KEY_STATIONS, STR_KEY_STATIONS )
                    ELSE 
                      STR_KEY_STATIONS = STR_KEY_STATIONS(1:I_LEN(STR_KEY_STATIONS))// &
     &                                   '  '//TRIM(SUR%STA(J13)%SHORT_NAME)
                 END IF
               ELSE 
                 IF ( FL_LBA_KEY .AND. SUR%STA(J13)%SHORT_NAME == 'Pa' ) THEN
                      STR_KEY_STATIONS = STR_KEY_STATIONS(1:I_LEN(STR_KEY_STATIONS))// &
     &                                   ',  PARK_MK5'
                    ELSE IF ( FL_LBA_KEY .AND. SUR%STA(J13)%SHORT_NAME == 'Ho' ) THEN
                      STR_KEY_STATIONS = STR_KEY_STATIONS(1:I_LEN(STR_KEY_STATIONS))// &
     &                                   ', HOB_DBBC'
                    ELSE IF ( FL_LBA_KEY .AND. SUR%STA(J13)%SHORT_NAME == 'Ti' ) THEN
                      STR_KEY_STATIONS = STR_KEY_STATIONS(1:I_LEN(STR_KEY_STATIONS))// &
     &                                   ',    DSS34'
                    ELSE IF ( FL_LBA_KEY .AND. SUR%STA(J13)%SHORT_NAME == 'Ww' ) THEN
                      STR_KEY_STATIONS = STR_KEY_STATIONS(1:I_LEN(STR_KEY_STATIONS))// &
     &                                   ',  WARK12M'
                    ELSE IF ( FL_LBA_KEY .AND. SUR%STA(J13)%SHORT_NAME == 'Wa' ) THEN
                      STR_KEY_STATIONS = STR_KEY_STATIONS(1:I_LEN(STR_KEY_STATIONS))// &
     &                                   ',  Wark30m'
                    ELSE IF ( FL_LBA_KEY .AND. SUR%STA(J13)%SHORT_NAME == 'Cd' ) THEN
                      STR_KEY_STATIONS = STR_KEY_STATIONS(1:I_LEN(STR_KEY_STATIONS))// &
     &                                   ',   CDDBBC'
                    ELSE IF ( FL_VLBA_KEY ) THEN
                      IF ( SUR%STA(J13)%NAME(4:4) .NE. '_' ) THEN
                           STR_KEY_STATIONS = STR_KEY_STATIONS(1:I_LEN(STR_KEY_STATIONS))// &
     &                                        ',  VLBA_'//SUR%STA(J13)%SHORT_NAME
                         ELSE
                           STR_KEY_STATIONS = STR_KEY_STATIONS(1:I_LEN(STR_KEY_STATIONS))// &
     &                                        ',  '//TRIM(SUR%STA(J13)%NAME)
                      END IF
                      CALL TRAN ( 11, STR_KEY_STATIONS, STR_KEY_STATIONS )
                    ELSE IF ( FL_CVN_KEY .AND. SUR%STA(J13)%SHORT_NAME == 'Sh' ) THEN
                      STR_KEY_STATIONS = STR_KEY_STATIONS(1:I_LEN(STR_KEY_STATIONS))// &
     &                                   ',  Shanghai'
                    ELSE IF ( FL_CVN_KEY .AND. SUR%STA(J13)%SHORT_NAME == 'Ur' ) THEN
                      STR_KEY_STATIONS = STR_KEY_STATIONS(1:I_LEN(STR_KEY_STATIONS))// &
     &                                   ',  Urumqi'
                    ELSE IF ( FL_CVN_KEY .AND. SUR%STA(J13)%SHORT_NAME == 'Km' ) THEN
                      STR_KEY_STATIONS = STR_KEY_STATIONS(1:I_LEN(STR_KEY_STATIONS))// &
     &                                   ',  Kunming'
                    ELSE IF ( FL_CVN_KEY .AND. SUR%STA(J13)%SHORT_NAME == 'Kv' ) THEN
                      STR_KEY_STATIONS = STR_KEY_STATIONS(1:I_LEN(STR_KEY_STATIONS))// &
     &                                   ',  Sejong'
                    ELSE IF ( FL_EVN_KEY .AND. SUR%STA(J13)%SHORT_NAME == 'Jb' ) THEN
                      STR_KEY_STATIONS = STR_KEY_STATIONS(1:I_LEN(STR_KEY_STATIONS))// &
     &                                   ',  JODRELL2'
                    ELSE IF ( FL_EVN_KEY .AND. SUR%STA(J13)%SHORT_NAME == 'On' ) THEN
                      STR_KEY_STATIONS = STR_KEY_STATIONS(1:I_LEN(STR_KEY_STATIONS))// &
     &                                   ',  ONSALA60'
                    ELSE 
                      STR_KEY_STATIONS = STR_KEY_STATIONS(1:I_LEN(STR_KEY_STATIONS))// &
     &                                   ',  '//TRIM(SUR%STA(J13)%SHORT_NAME)
                 END IF
            END IF
!
            IF ( SUR%STA(J13)%MOUNT_TYPE == MT__ALTAZ ) THEN
                 AZ_WRAP = SUR%AZ_ACC_OBS(J13,J5)
                 WRAP_STR = 'Az  '
               ELSE IF ( SUR%STA(J13)%MOUNT_TYPE == MT__EQUAT ) THEN
                 AZ_WRAP = SUR%HA_ACC_OBS(J13,J5)
                 WRAP_STR = 'Ha  '
               ELSE 
                 AZ_WRAP = SUR%AZ_OBS(J13,J5)
                 WRAP_STR = 'Az  '
            END IF
            WRITE ( LUN_PLAN, 110 ) WORD, J5, IND_TAP, OBJECT, &
     &                              J2000_NAME, &
     &                              STR(1:19), SUR%STA(J13)%NAME, &
     &                              SUR%EL_OBS(J13,J5)/DEG__TO__RAD, &
     &                              SUR%AZ_OBS(J13,J5)/DEG__TO__RAD, &
     &                              WRAP_STR(1:2), AZ_WRAP/DEG__TO__RAD, &
     &                              SUR%HA_OBS(J13,J5)/DEG__TO__RAD
 110        FORMAT ( A,': ',I4,'  Tape: ',I2, 2X,A,1X, A,'  Time: ',A, &
     &                 ' Station: ',A, ' El:', F7.2, ' Az: ',F7.2, &
     &                 ' ', A, '_wrap: ', F7.2, ' Ha: ', F7.2 )
            WRAP_STR = '&n'
            IF (         SUR%AZ_ACC_OBS(J13,J5) .GE. SUR%STA(J13)%AZ_RANGE(1) .AND. &
     &                   SUR%AZ_ACC_OBS(J13,J5) .LT. SUR%STA(J13)%AZ_RANGE(2)       ) THEN
                 WRAP_STR = '&ccw'
               ELSE IF ( SUR%AZ_ACC_OBS(J13,J5) .GE. SUR%STA(J13)%AZ_RANGE(2) .AND. &
     &                   SUR%AZ_ACC_OBS(J13,J5) .LT. SUR%STA(J13)%AZ_RANGE(3)       ) THEN
                 WRAP_STR = '&n  '
               ELSE IF ( SUR%AZ_ACC_OBS(J13,J5) .GE. SUR%STA(J13)%AZ_RANGE(3) .AND. &
     &                   SUR%AZ_ACC_OBS(J13,J5) .LE. SUR%STA(J13)%AZ_RANGE(4)       ) THEN
                 WRAP_STR = '&cw '
               ELSE
                 IF ( SUR%STA(J13)%MOUNT_TYPE == MT__ALTAZ ) THEN
                      WRITE ( 6, * ) 'SUR_PRINT_RES: Azimuth is out of range Scan= ', int2(j11),' sta ', SUR%STA(J13)%NAME, ' az= ', SNGL(SUR%AZ_ACC_OBS(J13,J5)/DEG__TO__RAD) ! %%%
                 END IF
            END IF
            IF ( J5 > 1 .AND. SUR%OBS_STA(J13,J5) == SUR__USED ) THEN
                 IF ( SUR%SCA_PREV(J13,J5) == 0 ) THEN
                      SUR%SCA_PREV(J13,J5) = J5 - 1
                      SUR%SLEW_DUR(J13,J5) = 0.0D0
                 END IF
!
! -------------- Apply rounding
!
                 UTC_START_PREOBS = UPROUND ( SUR%TAI_OBS_END(SUR%SCA_PREV(J13,J5)) + SUR%UTC_M_TAI + &
     &                                        SUR%SLEW_DUR(J13,J5) + DABS(SUR%RECORDING_PAUSE), &
     &                                        SUR%START_ROUNDING, ROU_EPS )
                 IF ( SUR%MJD_OBS_END(SUR%SCA_PREV(J13,J5)) == SUR%MJD_OBS_END(J5-1) ) THEN
                      IF ( UTC_START_PREOBS < SUR%TAI_OBS_END(J5-1) + SLEW_1ST + SUR%UTC_M_TAI ) THEN
                           UTC_START_PREOBS = UPROUND ( SUR%TAI_OBS_END(J5-1) + SLEW_1ST + &
     &                                                  DABS(SUR%RECORDING_PAUSE) + SUR%UTC_M_TAI, &
     &                                                  SUR%START_ROUNDING, ROU_EPS )
                      END IF
                    ELSE
                      IF ( UTC_START_PREOBS < SUR%TAI_OBS_END(J5-1) + 86400.0D0 + SLEW_1ST + SUR%UTC_M_TAI ) THEN
                           UTC_START_PREOBS = UPROUND ( SUR%TAI_OBS_END(J5-1) + 86400.0D0 + SLEW_1ST + &
     &                                                  DABS(SUR%RECORDING_PAUSE) + SUR%UTC_M_TAI, &
     &                                                  SUR%START_ROUNDING, ROU_EPS )
                      END IF
                 END IF
!
                 UTC_START_RECORD = UPROUND ( UTC_START_PREOBS + SUR%STA(J13)%PREOB, 1.0D0, ROU_EPS )
                 IF ( UTC_START_RECORD < SUR%TAI_OBS_END(J5-1) + &
     &                SLEW_1ST + SUR%UTC_M_TAI ) THEN
!
! ------------------- Recording should not start too early
!
                      UTC_START_RECORD = UPROUND ( SUR%TAI_OBS_END(J5-1) + &
     &                                             SLEW_1ST + &
     &                                             SUR%UTC_M_TAI, &
     &                                             1.0D0, ROU_EPS )
                 END IF
                 UTC_STOP_RECORD = SUR%TAI_OBS_END(J5) + SUR%UTC_M_TAI
                 DUR_RECORD      = UTC_STOP_RECORD - UTC_START_RECORD
                 IF ( DUR_RECORD < - 86400.0D0/2.0D0 ) THEN
                      DUR_RECORD = UTC_STOP_RECORD - ( UTC_START_RECORD - 86400.0D0 )
                 END IF 
!
                 IER = -1
                 IF ( UTC_START_PREOBS < 86400.0D0 ) THEN
                      STR1 = MJDSEC_TO_DATE ( SUR%MJD_OBS_END(SUR%SCA_PREV(J13,J5)), &
     &                                        UTC_START_PREOBS, IER )
                    ELSE
                      STR1 = MJDSEC_TO_DATE ( SUR%MJD_OBS_END(SUR%SCA_PREV(J13,J5)) + 1, &
     &                                        UTC_START_PREOBS - 86400.0D0 , IER )
                 END IF
                 IER = -1
                 IF ( UTC_START_PREOBS + SUR%STA(J13)%PREOB < 86400.0D0 ) THEN
                      STR2 = MJDSEC_TO_DATE ( SUR%MJD_OBS_END(SUR%SCA_PREV(J13,J5)), &
     &                                        UTC_START_PREOBS + SUR%STA(J13)%PREOB, IER )
                    ELSE
                      STR2 = MJDSEC_TO_DATE ( SUR%MJD_OBS_END(SUR%SCA_PREV(J13,J5)) + 1, &
     &                                        UTC_START_PREOBS + SUR%STA(J13)%PREOB - 86400.0D0, IER )
                 END IF 
                 WRITE ( UNIT=STR(1:4), FMT='(I4)' ) J5
                 CALL BLANK_TO_ZERO ( STR(1:4) )
                 CALL CLRCH ( STR_DUR )
                 WRITE ( UNIT=STR_DUR, FMT='(F7.1)' ) SUR%STA(J13)%PREOB
                 NA = NA + 1 ; WRITE ( UNIT=AST_BUF(NA), FMT='(A)' ) '    Preob:    '//SUR%STA(J13)%NAME//' '// &
     &                                        STR1(1:21)//' '//STR2(1:21)// &
     &                                        ' Scan: '//SCAN_STR_AST//' Source:  '//B1950_NAME// &
     &                                        '  Duration: '//TRIM(STR_DUR)//' Proc_name: proc_dummy'
!
                 IER = -1
                 IF ( UTC_START_RECORD < 86400.0D0 ) THEN
                      STR1 = MJDSEC_TO_DATE ( SUR%MJD_OBS_END(SUR%SCA_PREV(J13,J5)), &
     &                                        UTC_START_RECORD, IER )
                    ELSE
                      STR1 = MJDSEC_TO_DATE ( SUR%MJD_OBS_END(SUR%SCA_PREV(J13,J5)) + 1, &
     &                                        UTC_START_RECORD - 86400.0D0 , IER )
                 END IF
                 IER = -1
                 IF ( UTC_STOP_RECORD < 86400.0D0 ) THEN
                      STR2 = MJDSEC_TO_DATE ( SUR%MJD_OBS_END(J5), UTC_STOP_RECORD, IER )
                    ELSE
                      STR2 = MJDSEC_TO_DATE ( SUR%MJD_OBS_END(J5) + 1, UTC_STOP_RECORD - 86400.0D0, IER )
                 END IF
                 SPL_STATUS = SUR%STATUS_SPL(SUR%SRC_TYP(J5))
                 SUR%STATUS_SPL(SUR%SRC_TYP(J5)) = 0
                 CALL ERR_PASS ( IUER, IER )
                 AZ_OLD = SUR%AZ_OBS(J13,J5)
                 CALL SUR_AZEL ( SUR, VTD, SUR%SRC_TYP(J5), &
     &                           SUR%MJD_OBS_END(SUR%SCA_PREV(J13,J5)), &
     &                           UTC_START_PREOBS - SUR%UTC_M_TAI, &
     &                           J13, SUR%IND_SRC(J5), &
     &                           SUR%AZ_OBS(J13,J5), &
     &                           SUR%EL_OBS(J13,J5), &
     &                           SUR%HA_OBS(J13,J5), IER )
                 SUR%AZ_ACC_OBS(J13,J5) = SUR%AZ_ACC_OBS(J13,J5) + (SUR%AZ_OBS(J13,J5) - AZ_OLD)
                 SUR%STATUS_SPL(SUR%SRC_TYP(J5)) = SUR%STATUS_SPL(SUR%SRC_TYP(J5)) 
!
! -------------- Recompute wrap_str, since azimith may slighly change
!
                 WRAP_STR = '&n'
                 IF (         SUR%AZ_ACC_OBS(J13,J5) .GE. SUR%STA(J13)%AZ_RANGE(1) .AND. &
     &                        SUR%AZ_ACC_OBS(J13,J5) .LT. SUR%STA(J13)%AZ_RANGE(2)       ) THEN
                       WRAP_STR = '&ccw'
                    ELSE IF ( SUR%AZ_ACC_OBS(J13,J5) .GE. SUR%STA(J13)%AZ_RANGE(2) .AND. &
     &                        SUR%AZ_ACC_OBS(J13,J5) .LT. SUR%STA(J13)%AZ_RANGE(3)       ) THEN
                       WRAP_STR = '&n  '
                    ELSE IF ( SUR%AZ_ACC_OBS(J13,J5) .GE. SUR%STA(J13)%AZ_RANGE(3) .AND. &
     &                        SUR%AZ_ACC_OBS(J13,J5) .LE. SUR%STA(J13)%AZ_RANGE(4)       ) THEN
                       WRAP_STR = '&cw '
                 END IF
                 WRITE ( AST_BUF(NA-1)(141:145), FMT='(F5.2)' ) SUR%EL_OBS(J13,J5)/DEG__TO__RAD
                 WRITE ( AST_BUF(NA-1)(162:168), FMT='(F7.2)' ) SUR%AZ_OBS(J13,J5)/DEG__TO__RAD
                 WRITE ( AST_BUF(NA-1)(192:198), FMT='(F7.2)' ) SUR%HA_OBS(J13,J5)/DEG__TO__RAD
                 AST_BUF(NA-1)(207:) = TRIM(WRAP_STR)
                 CALL HR_TAT ( ALPHA_STR, RA,  IER )
                 CALL GR_TAT ( DELTA_STR, DEC, IER )
                 PAR_ANG = DATAN ( DSIN(SUR%HA_OBS(J13,J5))/ &
     &                           ( DCOS(DEC)*DTAN(SUR%STA(J13)%LAT_GDT) - DSIN(DEC)*DCOS(SUR%HA_OBS(J13,J5)) ) )
                 NA = NA + 1 ; WRITE ( UNIT=AST_BUF(NA), FMT=230 ) '    Record:   '//SUR%STA(J13)%NAME//' '// &
     &                                  STR1(1:21)//' '//STR2(1:21)//' Scan: '//SCAN_STR_AST// &
     &                                 ' Source:  '//B1950_NAME//'  Duration: ', DUR_RECORD, &
     &                                 ' Elev: ', SUR%EL_OBS(J13,J5)/DEG__TO__RAD, &
     &                                 ' Azim: ', SUR%AZ_OBS(J13,J5)/DEG__TO__RAD, &
     &                                 ' Hour_angle: ', SUR%HA_OBS(J13,J5)/DEG__TO__RAD, &
     &                                 ' Par_ang: ', PAR_ANG/DEG__TO__RAD
 230             FORMAT ( A,F7.1, A,F5.2, A,F7.2, A,F7.2, A,F7.2 )
                 WRITE ( UNIT=STR_DUR, FMT='(F7.1)' ) SUR%STA(J13)%POSTOB
                 STR1 = STR2
                 STR2 = MJDSEC_TO_DATE ( SUR%MJD_OBS_END(J5), SUR%TAI_OBS_END(J5) + &
     &                                   SUR%STA(J13)%POSTOB + SUR%UTC_M_TAI, IER )
                 NA = NA + 1 ; WRITE ( UNIT=AST_BUF(NA), FMT='(A)' ) '    Postob:   '//SUR%STA(J13)%NAME//' '// &
     &                                        STR1(1:21)//' '//STR2(1:21)// &
     &                                        ' Scan: '//SCAN_STR_AST//' Source:  '//B1950_NAME// &
     &                                        '  Duration: '//TRIM(STR_DUR)//' Proc_name: proc_dummy'
            END IF
!
            SCAN_LEN = (SUR%MJD_OBS_END(J5) - SUR%MJD_OBS_BEG(J5))*86400.0D0 + &
     &                 (SUR%TAI_OBS_END(J5) - SUR%TAI_OBS_BEG(J5))
!
            IF ( J5 == 1  .AND. &
     &           SUR%ALGORITHM == 'FRINGE_SEARCH_02' ) THEN
                 WRITE ( UNIT=STR_DWELL, FMT='(F5.0)'  ) SCAN_LEN + SUR__FIRST_EXTRA_TIM
               ELSE IF ( SUR%ALGORITHM == 'FRINGE_SEARCH_01' .OR. &
     &                   SUR%ALGORITHM == 'FRINGE_SEARCH_02' .OR. &
     &                   SUR%ALGORITHM == 'ASTROMET_01'   .OR. &
     &                   SUR%ALGORITHM == 'ASTROMET_02'   .OR. &
     &                   SUR%ALGORITHM == 'ASTROMET_03'   .OR. &
     &                   SUR%ALGORITHM == 'ASTROMET_04'   .OR. &
     &                   SUR%ALGORITHM == 'ASTROMET_05'   .OR. &
     &                   SUR%ALGORITHM == 'ASTROMET_06'   .OR. &
     &                   SUR%ALGORITHM == 'ASTROMET_07'   .OR. &
     &                   SUR%ALGORITHM == 'ASTROMET_11'   .OR. &
     &                   SUR%ALGORITHM == 'ASTROMET_12'   .OR. &
     &                   SUR%ALGORITHM == 'ASTROMET_13'   .OR. &
     &                   SUR%ALGORITHM == 'GEODETIC_01'   .OR. &
     &                   SUR%ALGORITHM == 'GEODETIC_02'   .OR. &
     &                   SUR%ALGORITHM == 'GEODETIC_03'   .OR. &
     &                   SUR%ALGORITHM == 'GNSS_01'       .OR. &
     &                   SUR%ALGORITHM == 'GNSS_02'       .OR. &
     &                   SUR%ALGORITHM == 'SPACECRAFT_01' .OR. &
     &                   SUR%ALGORITHM == 'IMAGING_01'         ) THEN
                 WRITE ( UNIT=STR_DWELL, FMT='(F5.0)'  ) SCAN_LEN
            END IF
!
            CALL INCH ( J5, STR_KEY_SCAN(1:5) )
            CALL CHASHR (    STR_KEY_SCAN(1:5) )
            STR_DOY  = STR_VEX_DATE(6:8)
!
            CALL CLRCH  (        STR_NUM_STA )
            CALL INCH   ( K_STA, STR_NUM_STA )
            CALL CHASHR (        STR_NUM_STA )
!
            IF ( SUR%SRC_TYP(J5) == SUR__TYP_TAG ) THEN
                 OBJECT_STR = 'object: primary target'
               ELSE IF ( SUR%SRC_TYP(J5) == SUR__TYP_SEC ) THEN
                 OBJECT_STR = 'object: secondary target'
               ELSE IF ( SUR%SRC_TYP(J5) == SUR__TYP_CAL ) THEN
                 OBJECT_STR = 'object: amplitude calibrator'
            END IF
!
            STR_KEY_START = MJDSEC_TO_VEX ( SUR%MJD_OBS_BEG(J5), &
     &                                      SUR%TAI_OBS_BEG(J5) &
     &                                      + SUR%UTC_M_TAI, IER ) 
            STR_KEY_STOP  = MJDSEC_TO_VEX ( SUR%MJD_OBS_END(J5), &
     &                                      SUR%TAI_OBS_END(J5) &
     &                                      + SUR%UTC_M_TAI, IER ) 
            IF ( SUR%KEY_TYP == KEY__TIME_ABS  .OR.  &
     &           SUR%KEY_TYP == KEY__TIME_DUR        ) THEN
!
! -------------- This is intentional. DOY for key-file corresponds to the 
! -------------- scan end time!
!
                 IF ( SUR%MJD_OBS_END(J5) > SUR%MJD_OBS_BEG(J5) .AND. &
     &                SUR%TAI_OBS_END(J5) + SUR%UTC_M_TAI > 0.0D0 ) THEN
                      CALL CHIN ( STR_DOY,   DOY )
                      CALL INCH ( DOY+1, STR_DOY )
                      CALL CHASHR (      STR_DOY )
                      CALL BLANK_TO_ZERO ( STR_DOY )
                 END IF
                 CALL CHASHL ( STR_KEY_SCAN(1:5) )
                 STR_KEY_SCAN = "year= "//STR_VEX_DATE(1:4)// &
     &                     "  day= "//STR_DOY_END// &
     &                     "  start= "//STR_VEX_DATE(10:11)// &
     &                     ":"//STR_VEX_DATE(13:14)// &
     &                     ":"//STR_VEX_DATE(16:17)// &
     &                     "  source= "//B1950_NAME// &
     &                     "  dur= "//STR_DWELL// &
     &                     "  / ! scan "//STR_KEY_SCAN(1:5)// &
     &                     " num_sta "//STR_NUM_STA// &
     &                     " "//OBJECT_STR
              ELSE IF ( SUR%KEY_TYP == KEY__START_STOP      .OR. &
     &                  SUR%KEY_TYP == KEY__START_STOP_2STA .OR. &
     &                  SUR%KEY_TYP == KEY__START_STOP_3STA      ) THEN
                 STR_KEY_SCAN = "year= "//STR_KEY_START(1:4)// &
     &                          "  day= "//STR_KEY_START(6:8)// &
     &                          "  start= "//STR_KEY_START(10:11)// &
     &                          ":"//STR_KEY_START(13:14)// &
     &                          ":"//STR_KEY_START(16:17)// &
     &                          " year= "//STR_KEY_STOP(1:4)// &
     &                          "  day= "//STR_KEY_STOP(6:8)// &
     &                          "  stop= "//STR_KEY_STOP(10:11)// &
     &                          ":"//STR_KEY_STOP(13:14)// &
     &                          ":"//STR_KEY_STOP(16:17)// &
     &                          "  source= "//B1950_NAME// &
     &                          "  / ! scan "//STR_KEY_SCAN(1:5)// &
     &                          " num_sta "//STR_NUM_STA// &
     &                          " "//OBJECT_STR
              ELSE IF ( SUR%KEY_TYP == KEY__LST_PT .OR. &
     &                  SUR%KEY_TYP == KEY__LST_PA .OR. &
     &                  SUR%KEY_TYP == KEY__LST_JB .OR. &
     &                  SUR%KEY_TYP == KEY__LST_MC .OR. &
     &                  SUR%KEY_TYP == KEY__LST_EF      ) THEN
                 WRITE ( UNIT=STR, FMT='(F5.0)'  ) SCAN_LEN
                 STR_KEY_SCAN = "  source= "//B1950_NAME// &
     &                          "  dwell= "//STR_DWELL// &
     &                          "  / ! scan "//STR_KEY_SCAN(1:5)// &
     &                          " num_sta "//STR_NUM_STA// &
     &                          " "//OBJECT_STR
            END IF
            IF ( J5 == 1 ) THEN
                 ACC_DATA(J13) = ACC_DATA(J13) + &
     &                           (SCAN_LEN + SUR__FIRST_EXTRA_TIM)* &
     &                            SUR%RECORDING_RATE/1.D9/8.0
            END IF
            WRITE ( UNIT=DISK_STR, FMT='(F9.3)' ) ACC_DATA(J13)
!
            IF ( SUR%ALGORITHM == 'FRINGE_SEARCH_01' .OR. &
     &           SUR%ALGORITHM == 'ASTROMET_01'      .OR. &
     &           SUR%ALGORITHM == 'ASTROMET_02'      .OR. &
     &           SUR%ALGORITHM == 'ASTROMET_03'      .OR. &
     &           SUR%ALGORITHM == 'ASTROMET_04'      .OR. &
     &           SUR%ALGORITHM == 'ASTROMET_05'      .OR. &
     &           SUR%ALGORITHM == 'ASTROMET_06'      .OR. &
     &           SUR%ALGORITHM == 'ASTROMET_07'      .OR. &
     &           SUR%ALGORITHM == 'ASTROMET_11'      .OR. &
     &           SUR%ALGORITHM == 'ASTROMET_12'      .OR. &
     &           SUR%ALGORITHM == 'ASTROMET_13'      .OR. &
     &           SUR%ALGORITHM == 'GEODETIC_01'      .OR. &
     &           SUR%ALGORITHM == 'GEODETIC_02'      .OR. &
     &           SUR%ALGORITHM == 'GEODETIC_03'      .OR. &
     &           SUR%ALGORITHM == 'GNSS_01'          .OR. &
     &           SUR%ALGORITHM == 'GNSS_02'          .OR. &
     &           SUR%ALGORITHM == 'SPACECRAFT_01'    .OR. &
     &           SUR%ALGORITHM == 'IMAGING_01'            ) THEN
!
                 GB_STR = ' GB:   : '
                 IF ( TRIM(SUR%STA(J13)%SHORT_NAME) == 'At' .OR. &
     &                TRIM(SUR%STA(J13)%SHORT_NAME) == 'Pa' .OR. &
     &                TRIM(SUR%STA(J13)%SHORT_NAME) == 'Mp'      ) THEN
                      GB_STR = ' GB: 0 : '
                 END IF
                 IF ( J5 > 1 ) THEN
                      SCA_START_OFF = 0.0
!
! ------------------- STA_START_OFF -- offset of station start time wrt the adjusted 
! -------------------                  scan start time
!
                      STA_START_OFF = SUR%SLEW_DUR(J13,J5) - ADJ_SLEW_TIME
                      IF ( SUR%SCA_PREV(J13,J5) > 0 ) THEN
                           IF ( SUR%OBS_STA(J13,J5-1) .NE. SUR__USED ) THEN
!
! ----------------------------- Case when the station did not observe the previous scan
!
                                STA_START_OFF = SUR%SLEW_DUR(J13,J5) - &
     &                                         ((SUR%TAI_OBS_BEG(J5) - SUR%TAI_OBS_END(SUR%SCA_PREV(J13,J5))) + &
     &                                          (SUR%MJD_OBS_BEG(J5) - SUR%MJD_OBS_END(SUR%SCA_PREV(J13,J5)))*86400.0D0 + &
     &                                           SCA_START_OFF )
                           END IF
                      END IF
                      IF ( STA_START_OFF < 0.0D0                 ) STA_START_OFF = 0.0
                      IF ( SUR%START_ROUNDING > 1.0001 ) THEN
                           UTC_ADJ_START = 0.0D0
                           STA_START_OFF = UPROUND ( UTC_ADJ_START_RAW + STA_START_OFF, SUR%START_ROUNDING, ROU_EPS ) - UTC_ADJ_START 
                      END IF
                      SCA_STA_DUR = NOM_SCA_LEN - SCA_START_OFF - STA_START_OFF
                      ACC_DATA(J13) = ACC_DATA(J13) + SUR%RECORDING_RATE/1.D9/8.0*(SCA_STA_DUR - SUR%STA(J13)%PREOB)
                      WRITE ( UNIT=DISK_STR, FMT='(F9.3)' ) ACC_DATA(J13)
                      WRITE ( UNIT=LUN_VEX, FMT='(A,I4,A,I4,A)' ) '    station = '// &
     &                                      TRIM(SUR%STA(J13)%SHORT_NAME)//' : ', &
     &                                      IDINT(STA_START_OFF), &
     &                                      ' sec: ', IDINT(STA_START_OFF+SCA_STA_DUR), &
     &                                      ' sec: '//DISK_STR//GB_STR//WRAP_STR//'   : 1;'
                    ELSE 
                      WRITE ( UNIT=LUN_VEX, FMT='(A,I4,A,I4,A)' ) '    station= '//  &
     &                                      TRIM(SUR%STA(J13)%SHORT_NAME)//' : ', 0, &
     &                                      ' sec: ', IDINT(NOM_SCA_LEN), &
     &                                      ' sec: '//DISK_STR//GB_STR//WRAP_STR//'   : 1;'
                 END IF
               ELSE IF ( ( SUR%ALGORITHM == 'ASTROMET_98' .OR. &
     &                     SUR%ALGORITHM == 'ASTROMET_99'      ) .AND. &
     &                     SUR%EXP_CODE(1:2) .NE.  'ep' ) THEN
                 WRITE ( UNIT=LUN_VEX, FMT='(A)' ) '    station = '// &
     &                                        TRIM(SUR%STA(J13)%SHORT_NAME)// &
     &                                        ':    0 sec: '//STR(1:4)// &
     &                                        ' sec:    0 sec:      '// &
     &                                        ':      : 1;'
               ELSE IF ( SUR%ALGORITHM == 'FRINGE_SEARCH_02' .OR. &
     &                   ( SUR%ALGORITHM == 'ASTROMET_02' .AND.   &
                           SUR%EXP_CODE(1:2) .EQ.  'ep' )         ) THEN
                 WRITE ( UNIT=LUN_VEX, FMT='(A)' ) '    station = '// &
     &                                        TRIM(SUR%STA(J13)%SHORT_NAME)// &
     &                                        ' :    0 sec: '//STR(1:4)// &
     &                                        ' sec: '//DISK_STR// &
     &                                        ' GB:   :    : 1;'
            END IF
 4130    CONTINUE
         IF ( J5 < SUR%L_SCN ) THEN
              SLEW_TIME = (SUR%MJD_OBS_BEG(J5+1) - SUR%MJD_OBS_END(J5))*86400.0D0 + &
     &                    (SUR%TAI_OBS_BEG(J5+1) - SUR%TAI_OBS_END(J5))
              WRITE ( UNIT=STR, FMT='(A,F9.1,A,I5,A)' ) '# Slew time: ',&
     &                SLEW_TIME, ' sec'// &
     &                '  Next scan: ', J5+1, '                                  '// &
     &                '                                               '
              CALL BLANK_TO_ZERO ( STR(40:44) )
              WRITE ( UNIT=LUN_PLAN, FMT='(A)' ) STR(1:125)
         END IF
         WRITE ( UNIT=LUN_VEX, FMT='(A)' ) 'endscan;'
         WRITE ( UNIT=LUN_VEX, FMT='(A)' ) '*'
!
         IF ( SUR%ALGORITHM == 'ASTROMET_01'   .OR. &
     &        SUR%ALGORITHM == 'ASTROMET_02'   .OR. &
     &        SUR%ALGORITHM == 'ASTROMET_03'   .OR. &
     &        SUR%ALGORITHM == 'ASTROMET_04'   .OR. &
     &        SUR%ALGORITHM == 'ASTROMET_05'   .OR. &
     &        SUR%ALGORITHM == 'ASTROMET_06'   .OR. &
     &        SUR%ALGORITHM == 'ASTROMET_07'   .OR. &
     &        SUR%ALGORITHM == 'ASTROMET_11'   .OR. &
     &        SUR%ALGORITHM == 'ASTROMET_12'   .OR. &
     &        SUR%ALGORITHM == 'ASTROMET_13'   .OR. &
     &        SUR%ALGORITHM == 'GEODETIC_01'   .OR. &
     &        SUR%ALGORITHM == 'GEODETIC_02'   .OR. &
     &        SUR%ALGORITHM == 'GEODETIC_03'   .OR. &
     &        SUR%ALGORITHM == 'GNSS_01'       .OR. &
     &        SUR%ALGORITHM == 'GNSS_02'       .OR. &
     &        SUR%ALGORITHM == 'SPACECRAFT_01' .OR. &
     &        SUR%ALGORITHM == 'IMAGING_01'         ) THEN
!
              CALL CLRCH ( STR )
              STR(1:8) = B1950_NAME
              IF ( SUR%SCAN_TYPE(J5) == SUR__SHORT ) THEN
                   WRITE ( UNIT=STR(10:12), FMT='(I3)' ) IDNINT(SUR%PREOBS_SHORT)
                 ELSE IF ( SUR%SCAN_TYPE(J5) == SUR__LONG ) THEN
                   WRITE ( UNIT=STR(10:12), FMT='(I3)' ) IDNINT(SUR%PREOBS_LONG)
                 ELSE IF ( SUR%SCAN_TYPE(J5) == SUR__TAPE ) THEN
                   WRITE ( UNIT=STR(10:12), FMT='(I3)' ) IDNINT(SUR%PREOBS_LONG)
                 ELSE
                   WRITE ( UNIT=STR(10:12), FMT='(I3)' ) IDNINT(SUR%PREOBS_LONG)
              END IF
              STR(14:21) = 'VE PREOB'
              STR(44:) = 'MIDOB      0 POSTOB '//STA_STR(1:I_LEN(STA_STR))
              STR(24:34) = STR_VEX_DATE(3:4)//STR_VEX_DATE(6:8)// &
     &                     STR_VEX_DATE(10:11)//STR_VEX_DATE(13:14)// &
     &                     STR_VEX_DATE(16:17)
              WRITE ( UNIT=STR(40:43), FMT='(I3)' ) IDNINT(SCAN_LEN)
!
              DO 4230 J23=1,K_STA
                 IL = ILEN(STR)
                 STR(IL+2:) = '1F000000'
 4230         CONTINUE
!
              IL = ILEN(STR)
              STR(IL+2:) = 'YYNN'
              IL = ILEN(STR)
!
              DO 4240 J24=1,K_STA
                 WRITE ( UNIT=STR(IL+3+(J24-1)*6:IL+3+J24*6), FMT='(I6)' ) IDNINT(SCAN_LEN)
 4240         CONTINUE
              IF ( SUR%SCAN_TYPE(J5) == SUR__TAPE ) THEN
                   STR(I_LEN(STR)+2:) = 'NEW_TAPE'
              END IF
!
              DUR = (SUR%MJD_OBS_END(J5) - SUR%MJD_OBS_BEG(J5))*86400.0D0 + &
     &              (SUR%TAI_OBS_END(J5) - SUR%TAI_OBS_BEG(J5)) 
!
              CALL CLRCH    (      STR      )
              CALL INCH     ( J5, STR(1:4) )
              CALL CHASHR   (      STR(1:4) )
              IF ( J5 == 1 ) THEN
                   STR_KEY_START = MJDSEC_TO_VEX ( SUR%MJD_OBS_BEG(J5), &
     &                                             SUR%TAI_OBS_BEG(J5) + SUR%PREOBS_LONG + SUR%UTC_M_TAI, IER ) 
                   STR_KEY_STOP  = MJDSEC_TO_VEX ( SUR%MJD_OBS_END(J5), &
     &                                             SUR%TAI_OBS_END(J5) + SUR%UTC_M_TAI, IER ) 
!
                   IF ( SUR%KEY_TYP == KEY__TIME_DUR .OR. &
     &                  SUR%KEY_TYP == KEY__TIME_ABS      ) THEN
                        WRITE ( UNIT=STR_DWELL, FMT='(F5.0)'  ) DUR - DUR_FUDGE
                        CALL INCH ( J5, STR_KEY_SCAN(1:4) )
                        STR_KEY_SCAN = "year= "//STR_VEX_DATE(1:4)// &
     &                           "  day= "//STR_DOY// &
     &                           "  start= "//STR_VEX_DATE(10:11)// &
     &                           ":"//STR_VEX_DATE(13:14)// &
     &                           ":"//STR_VEX_DATE(16:17)// &
     &                           "  source= "//B1950_NAME// &
     &                           "  dur= "//STR_DWELL// &
     &                           "  / ! scan "//STR_KEY_SCAN(1:4)// &
     &                           " num_sta "//STR_NUM_STA// &
     &                           " "//OBJECT_STR
                     ELSE IF ( SUR%KEY_TYP == KEY__START_STOP ) THEN
                        STR_KEY_SCAN = "year= "//STR_KEY_START(1:4)// &
     &                            "  day= "//STR_KEY_START(6:8)// &
     &                            "  start= "//STR_KEY_START(10:11)// &
     &                            ":"//STR_KEY_START(13:14)// &
     &                            ":"//STR_KEY_START(16:17)// &
     &                            " year= "//STR_KEY_STOP(1:4)// &
     &                            "  day= "//STR_KEY_STOP(6:8)// &
     &                            "  stop= "//STR_KEY_STOP(10:11)// &
     &                            ":"//STR_KEY_STOP(13:14)// &
     &                            ":"//STR_KEY_STOP(16:17)// &
     &                            "  source= "//B1950_NAME// &
     &                            "  / ! scan "//STR(1:4)// &
     &                            " num_sta "//STR_NUM_STA// &
     &                            " "//OBJECT_STR
                   END IF
!
                   IF ( SUR%KEY_TYP == KEY__LST_PT .OR. &
     &                  SUR%KEY_TYP == KEY__LST_PA .OR. &
     &                  SUR%KEY_TYP == KEY__LST_JB .OR. &
     &                  SUR%KEY_TYP == KEY__LST_EF .OR. &
     &                  SUR%KEY_TYP == KEY__LST_MC      ) THEN
!
                        STR_KEY_SCAN = "  source= "//B1950_NAME// &
     &                          "  dwell= "//STR_DWELL// &
     &                          "  / ! scan     1"// &
     &                          " num_sta "//STR_NUM_STA// &
     &                          " "//OBJECT_STR
                   END IF
                 ELSE
                   IF ( SUR%KEY_TYP == KEY__START_STOP ) THEN
                        IF ( SUR%RECORDING_PAUSE > 1.0D0 .AND. K_STA > 1 ) THEN
                             IF ( SUR%RECORDING_PAUSE > 0.0D0 ) THEN
                                  STR_KEY_START = MJDSEC_TO_VEX ( SUR%MJD_OBS_BEG(J5), &
     &                                                            SUR%TAI_OBS_BEG(J5) + SUR%RECORDING_PAUSE + &
     &                                                            SUR%UTC_M_TAI, IER ) 
                                  WRITE ( UNIT=LUN_KEY, FMT='(A,I3,A)' ) 'gap ', IABS(IDNINT(SUR%RECORDING_PAUSE)), &
     &                                                                  ' ! to guarantee firing the noise diode'
                            END IF
                       END IF
                       STR_KEY_START = MJDSEC_TO_VEX ( SUR%MJD_OBS_BEG(J5), &
     &                                                 SUR%TAI_OBS_BEG(J5) + SUR%PREOBS_LONG + SUR%UTC_M_TAI, IER ) 
                       STR_KEY_STOP  = MJDSEC_TO_VEX ( SUR%MJD_OBS_END(J5), &
     &                                                 SUR%TAI_OBS_END(J5) + SUR%UTC_M_TAI, IER ) 
                       STR_KEY_SCAN = "year= "//STR_KEY_START(1:4)// &
     &                          "  day= "//STR_KEY_START(6:8)// &
     &                          "  start= "//STR_KEY_START(10:11)// &
     &                          ":"//STR_KEY_START(13:14)// &
     &                          ":"//STR_KEY_START(16:17)// &
     &                          " year= "//STR_KEY_STOP(1:4)// &
     &                          "  day= "//STR_KEY_STOP(6:8)// &
     &                          "  stop= "//STR_KEY_STOP(10:11)// &
     &                          ":"//STR_KEY_STOP(13:14)// &
     &                          ":"//STR_KEY_STOP(16:17)// &
     &                          "  source= "//B1950_NAME// &
     &                          "  / ! scan "//STR(1:4)// &
     &                          " num_sta "//STR_NUM_STA// &
     &                          " "//OBJECT_STR
                     ELSE IF ( SUR%KEY_TYP == KEY__START_STOP_2STA ) THEN
                       IF ( SCA_START_OFF < 0 ) THEN
                            WRITE ( UNIT=LUN_KEY, FMT='(A,I3,A)' ) 'prestart ', IABS(IDNINT(-SCA_START_OFF)), &
     &                                                             ' ! to start recording as soon as 2 stations are on-source'
                       END IF
                       STR_KEY_START = MJDSEC_TO_VEX ( SUR%MJD_OBS_BEG(J5), &
     &                                                 SUR%TAI_OBS_BEG(J5) + SUR%PREOBS_LONG + SUR%UTC_M_TAI, IER ) 
                       STR_KEY_STOP  = MJDSEC_TO_VEX ( SUR%MJD_OBS_END(J5), &
     &                                                 SUR%TAI_OBS_END(J5) + SUR%UTC_M_TAI, IER ) 
                       STR_KEY_SCAN = "year= "//STR_KEY_START(1:4)// &
     &                          "  day= "//STR_KEY_START(6:8)// &
     &                          "  start= "//STR_KEY_START(10:11)// &
     &                          ":"//STR_KEY_START(13:14)// &
     &                          ":"//STR_KEY_START(16:17)// &
     &                          " year= "//STR_KEY_STOP(1:4)// &
     &                          "  day= "//STR_KEY_STOP(6:8)// &
     &                          "  stop= "//STR_KEY_STOP(10:11)// &
     &                          ":"//STR_KEY_STOP(13:14)// &
     &                          ":"//STR_KEY_STOP(16:17)// &
     &                          "  source= "//B1950_NAME// &
     &                          "  / ! scan "//STR(1:4)// &
     &                          " num_sta "//STR_NUM_STA// &
     &                          " "//OBJECT_STR
                     ELSE IF ( SUR%KEY_TYP == KEY__START_STOP_3STA ) THEN
                       IF ( SCA_START_OFF < 0 ) THEN
                            WRITE ( UNIT=LUN_KEY, FMT='(A,I3,A)' ) 'prestart ', IABS(IDNINT(-SCA_START_OFF)), &
     &                                                             ' ! to start recording as soon as 3 stations are on-source'
                       END IF
                       STR_KEY_START = MJDSEC_TO_VEX ( SUR%MJD_OBS_BEG(J5), &
     &                                                 SUR%TAI_OBS_BEG(J5) + SUR%PREOBS_LONG + SUR%UTC_M_TAI, IER ) 
                       STR_KEY_STOP  = MJDSEC_TO_VEX ( SUR%MJD_OBS_END(J5), &
     &                                                 SUR%TAI_OBS_END(J5) + SUR%UTC_M_TAI, IER ) 
                       STR_KEY_SCAN = "year= "//STR_KEY_START(1:4)// &
     &                          "  day= "//STR_KEY_START(6:8)// &
     &                          "  start= "//STR_KEY_START(10:11)// &
     &                          ":"//STR_KEY_START(13:14)// &
     &                          ":"//STR_KEY_START(16:17)// &
     &                          " year= "//STR_KEY_STOP(1:4)// &
     &                          "  day= "//STR_KEY_STOP(6:8)// &
     &                          "  stop= "//STR_KEY_STOP(10:11)// &
     &                          ":"//STR_KEY_STOP(13:14)// &
     &                          ":"//STR_KEY_STOP(16:17)// &
     &                          "  source= "//B1950_NAME// &
     &                          "  / ! scan "//STR(1:4)// &
     &                          " num_sta "//STR_NUM_STA// &
     &                          " "//OBJECT_STR
                     ELSE IF ( SUR%KEY_TYP == KEY__TIME_ABS ) THEN
!
! ---------------------- This is intentional. DOY for key-file corresponds to the 
! ---------------------- scan end time!
!
                         STR_VEX_DATE = MJDSEC_TO_VEX ( SUR%MJD_OBS_END(J5), &
     &                                                  SUR%TAI_OBS_END(J5) &
     &                                                  + SUR%UTC_M_TAI, IER ) 
                         STR_DOY  = STR_VEX_DATE(6:8)
!
! ---------------------- Now get the date for start time
!
                         STR_VEX_DATE = MJDSEC_TO_VEX ( SUR%MJD_OBS_BEG(J5), &
     &                                  SUR%TAI_OBS_BEG(J5) &
     &                                  + SUR%UTC_M_TAI &
     &                                  + DUR_FUDGE, IER )
                         WRITE ( UNIT=STR_DWELL, FMT='(F5.0)'  ) DUR - DUR_FUDGE
!
                         CALL INCH ( J5, STR_KEY_SCAN(1:4) )
                         STR_KEY_SCAN = "year= "//STR_VEX_DATE(1:4)// &
     &                           "  day= "//STR_DOY// &
     &                           "  start= "//STR_VEX_DATE(10:11)// &
     &                           ":"//STR_VEX_DATE(13:14)// &
     &                           ":"//STR_VEX_DATE(16:17)// &
     &                           "  source= "//B1950_NAME// &
     &                           "  dur= "//STR_DWELL// &
     &                           "  / ! scan "//STR_KEY_SCAN(1:4)// &
     &                           " num_sta "//STR_NUM_STA// &
     &                           " "//OBJECT_STR
                       ELSE IF ( SUR%KEY_TYP == KEY__TIME_DUR ) THEN
!
! ---------------------- This is intentional. DOY for key-file corresponds to the 
! ---------------------- scan end time!
!
                         IF ( J5 == 1 ) THEN
                              STR_VEX_DATE = MJDSEC_TO_VEX ( SUR%MJD_OBS_BEG(J5), &
     &                                                       SUR%TAI_OBS_BEG(J5) &
     &                                                       + SUR%UTC_M_TAI, IER ) 
                              DUR_SLEW = 0.0D0
                            ELSE 
                              STR_VEX_DATE = MJDSEC_TO_VEX ( SUR%MJD_OBS_END(J5-1), &
     &                                                       SUR%TAI_OBS_END(J5-1) &
     &                                                       + SUR%UTC_M_TAI, IER ) 
                              DUR_SLEW = (SUR%MJD_OBS_BEG(J5) - SUR%MJD_OBS_END(J5-1))*86400.D0 + &
     &                                   (SUR%TAI_OBS_BEG(J5) - SUR%TAI_OBS_END(J5-1))
                         END IF 
                         STR_DOY  = STR_VEX_DATE(6:8)
                         WRITE ( UNIT=STR_DWELL, FMT='(F5.0)'  ) DUR + DUR_SLEW
!
                         CALL INCH ( J5, STR_KEY_SCAN(1:4) )
                         STR_KEY_SCAN = "year= "//STR_VEX_DATE(1:4)// &
     &                           "  day= "//STR_DOY// &
     &                           "  start= "//STR_VEX_DATE(10:11)// &
     &                           ":"//STR_VEX_DATE(13:14)// &
     &                           ":"//STR_VEX_DATE(16:17)// &
     &                           "  source= "//B1950_NAME// &
     &                           "  dur= "//STR_DWELL// &
     &                           "  / ! scan "//STR_KEY_SCAN(1:4)// &
     &                           " num_sta "//STR_NUM_STA// &
     &                           " "//OBJECT_STR
                       ELSE IF ( SUR%KEY_TYP == KEY__START_STOP ) THEN
                         CALL INCH ( J5, STR_KEY_SCAN(1:5) )
                         STR_KEY_START = MJDSEC_TO_VEX ( SUR%MJD_OBS_BEG(J5), &
     &                                                   SUR%TAI_OBS_BEG(J5) &
     &                                                   + SUR%UTC_M_TAI, IER ) 
                         STR_KEY_STOP = MJDSEC_TO_VEX ( SUR%MJD_OBS_END(J5), &
     &                                                  SUR%TAI_OBS_END(J5) &
     &                                                  + SUR%UTC_M_TAI, IER ) 
                         STR_KEY_SCAN = "year= "//STR_KEY_START(1:4)// &
     &                          "  day= "//STR_KEY_START(6:8)// &
     &                          "  start= "//STR_KEY_START(10:11)// &
     &                          ":"//STR_KEY_START(13:14)// &
     &                          ":"//STR_KEY_START(16:17)// &
     &                          " year= "//STR_KEY_STOP(1:4)// &
     &                          "  day= "//STR_KEY_STOP(6:8)// &
     &                          "  stop= "//STR_KEY_STOP(10:11)// &
     &                          ":"//STR_KEY_STOP(13:14)// &
     &                          ":"//STR_KEY_STOP(16:17)// &
     &                          "  source= "//B1950_NAME// &
     &                          "  / ! scan "//STR_KEY_SCAN(1:5)// &
     &                          " num_sta "//STR_NUM_STA// &
     &                          " "//OBJECT_STR
                    END IF 
              END IF
!
              WRITE ( UNIT=LUN_KEY, FMT='(A)' ) STR_KEY_STATIONS(1:I_LEN(STR_KEY_STATIONS))
              IF ( SUR%KEY_TYP == KEY__LST_PT .OR. &
     &             SUR%KEY_TYP == KEY__LST_PA .OR. &
     &             SUR%KEY_TYP == KEY__LST_JB .OR. &
     &             SUR%KEY_TYP == KEY__LST_EF .OR. &
     &             SUR%KEY_TYP == KEY__LST_MC      ) THEN
!
                   WRITE ( UNIT=LUN_KEY, FMT='(A)' ) '!* start: '//STR_KEY_START// &
     &                                               ' stop: '//STR_KEY_STOP
              END IF
              WRITE ( UNIT=LUN_KEY, FMT='(A)' ) STR_KEY_SCAN(1:I_LEN(STR_KEY_SCAN))
              WRITE ( UNIT=LUN_KEY, FMT='(A)' ) ' '
         END IF
 450  CONTINUE
!
      CALL ERR_PASS ( IUER, IER )
      STR = MJDSEC_TO_DATE ( SUR%MJD_OBS_END(SUR%L_SCN), &
     &                       SUR%TAI_OBS_END(SUR%L_SCN), IER )
      IF ( IER .NE. 0 ) THEN
           WRITE ( 6, * ) ' SUR%L_SCN = ', SUR%L_SCN
           WRITE ( 6, * ) ' SUR%MJD_OBS_BEG(SUR%L_SCN) = ', &
     &                      SUR%MJD_OBS_BEG(SUR%L_SCN), &
     &                    ' SUR%MJD_OBS_END(SUR%L_SCN) = ', &
     &                      SUR%MJD_OBS_END(SUR%L_SCN), &
     &                    ' SUR%TAI_OBS_BEG(SUR%L_SCN) = ', &
     &                      SUR%TAI_OBS_BEG(SUR%L_SCN), &
     &                    ' SUR%TAI_OBS_END(SUR%L_SCN) = ', &
     &                      SUR%TAI_OBS_END(SUR%L_SCN)
           CALL ERR_LOG ( 1530, IUER, 'SUR_PRINT_RES', 'Wring date for '// &
     &         'the last scan' )
           RETURN 
      END IF
      WRITE ( UNIT=LUN_PLAN, FMT='(A)' ) '# Schedule ended on '//STR(1:21)
      WRITE ( UNIT=LUN_KEY, FMT='(A)' ) ' ' 
      WRITE ( UNIT=LUN_KEY, FMT='(A)' ) '! End of schedule '// &
     &                                  SUR%EXP_CODE(1:I_LEN(SUR%EXP_CODE))
!
      CLOSE ( UNIT=LUN_PLAN )
      CLOSE ( UNIT=LUN_VEX )
      CLOSE ( UNIT=LUN_KEY )
      DO 4250 J25=1,NA
         WRITE ( UNIT=LUN_AST, FMT='(A)' ) TRIM(AST_BUF(J25))
 4250 CONTINUE
      CLOSE ( UNIT=LUN_AST )
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  SUR_PRINT_RES  !#!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE SUR_SOU_OUT ( SUR, VTD, I_TYP, LUN_VEX, LUN_KEY, &
     &                         MBUF, L_TAB, BUF_TAB, N_FLX, BUF_FLX, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine SOU_OUT
! *                                                                      *
! *  ### 24-NOV-2012 SUR_SOU_OUT   v1.2 (c)  L. Petrov  13-JUN-2016 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'sur_sked.i'
      INCLUDE   'vtd.i'
      TYPE     ( SUR__TYPE ) :: SUR
      TYPE     ( VTD__TYPE ) :: VTD
      TYPE     ( SUR_SOU__TYPE ) :: SOU(SUR__M_SOU)
      INTEGER*4  I_TYP, LUN_VEX, LUN_KEY, MBUF, L_TAB, &
     &           N_FLX, IUER
      LOGICAL*1  FL_FOUND, FL_PRI, FL_SCN(SUR__M_SCN)
      CHARACTER  STR*512, PLA_NAM*8
      INTEGER*4  L_SOU, J1, J2, J3, J4, J5, J6, I_SOU, MJD_OBS, IER
      REAL*8     PLA_COO(3), TAI_OBS, RD
      CHARACTER  BUF_TAB(MBUF)*(*), BUF_FLX(MBUF)*(*)
      INTEGER*4, EXTERNAL :: ILEN, I_LEN
!      
      FL_SCN = .FALSE.
      IF ( I_TYP == SUR__TYP_TAG ) THEN
           L_SOU = SUR%L_SOU 
           IF ( L_SOU < 1 ) THEN
                CALL ERR_LOG ( 0, IUER )
                RETURN 
           END IF
           SOU(1:L_SOU) = SUR%SOU(1:L_SOU)
         ELSE IF ( I_TYP == SUR__TYP_SEC ) THEN
           L_SOU = SUR%L_SO2
           IF ( L_SOU < 1 ) THEN
                CALL ERR_LOG ( 0, IUER )
                RETURN 
           END IF
           SOU(1:L_SOU) = SUR%SO2(1:L_SOU)
         ELSE IF ( I_TYP == SUR__TYP_CAL ) THEN
           L_SOU = SUR%L_CAL
           IF ( L_SOU < 1 ) THEN
                CALL ERR_LOG ( 0, IUER )
                RETURN 
           END IF
           SOU(1:L_SOU) = SUR%CAL(1:L_SOU)
         ELSE IF ( I_TYP == SUR__TYP_POC ) THEN
           L_SOU = SUR%L_SOP
           IF ( L_SOU < 1 ) THEN
                CALL ERR_LOG ( 0, IUER )
                RETURN 
           END IF
           SOU(1:L_SOU) = SUR%SOP(1:L_SOU)
         ELSE IF ( I_TYP == SUR__TYP_PLA ) THEN
           IF ( SUR%L_PLA < 1 ) THEN
                CALL ERR_LOG ( 0, IUER )
                RETURN 
           END IF
!
           L_SOU = 0
           DO 410 J1=1,SUR%L_PLA
              I_SOU = 0
              DO 420 J2=1,SUR%L_SCN
                 IF ( SUR%SRC_TYP(J2) == SUR__TYP_PLA .AND. &
     &                SUR%IND_SRC(J2) == J1           .AND. &
     &                .NOT. FL_SCN(J2)                      ) THEN
!
                      L_SOU = L_SOU + 1
                      SUR%IND_SRC(J2) = L_SOU
                      I_SOU = I_SOU + 1
                      SOU(L_SOU) = SUR%PLA(J1)
                      WRITE ( UNIT=SOU(L_SOU)%B1950_NAME(4:8),  FMT='(I5)' ) I_SOU
                      CALL BLANK_TO_ZERO ( SOU(L_SOU)%B1950_NAME(4:8) )
                      WRITE ( UNIT=SOU(L_SOU)%J2000_NAME(6:10), FMT='(I5)' ) I_SOU
                      CALL BLANK_TO_ZERO ( SOU(L_SOU)%J2000_NAME(6:10) )
!
                      MJD_OBS = SUR%MJD_OBS_BEG(J2)
                      TAI_OBS = (SUR%TAI_OBS_BEG(J2) + SUR%TAI_OBS_BEG(J2))/2.0D0
                      PLA_NAM = '????????'
                      IF ( SOU(L_SOU)%J2000_NAME(1:4) == 'MERC' ) PLA_NAM = 'MERCURY '
                      IF ( SOU(L_SOU)%J2000_NAME(1:4) == 'VENU' ) PLA_NAM = 'VENUS   '
                      IF ( SOU(L_SOU)%J2000_NAME(1:4) == 'MARS' ) PLA_NAM = 'MARS    '
                      IF ( SOU(L_SOU)%J2000_NAME(1:4) == 'JUPI' ) PLA_NAM = 'JUPITER '
                      IF ( SOU(L_SOU)%J2000_NAME(1:4) == 'SATU' ) PLA_NAM = 'SATURN  '
                      IF ( SOU(L_SOU)%J2000_NAME(1:4) == 'URAN' ) PLA_NAM = 'URANUS  '
                      IF ( SOU(L_SOU)%J2000_NAME(1:4) == 'NEPT' ) PLA_NAM = 'NEPTUNE '
                      IF ( PLA_NAM == '????????' ) THEN
                           CALL ERR_LOG ( 1537, IUER, 'SUR_SOU_OUT', 'Unknown planet '// &
     &                         'name '//SOU(L_SOU)%J2000_NAME(1:4) )
                           RETURN 
                      END IF
!
                      CALL ERR_PASS ( IUER, IER )
                      CALL GET_PLANET_COO ( PLA_NAM, VTD, MJD_OBS, TAI_OBS, &
     &                                      PLA_COO, IER )
                      IF ( IER .NE. 0 ) THEN
                           CALL ERR_LOG ( 1538, IUER, 'SUR_SOU_OUT', 'Error in '// &
     &                         'an attempt to compute coordinates of planet '//PLA_NAM )
                           RETURN 
                      END IF
                      SOU(L_SOU)%S_VEC = PLA_COO
                      IER = -1
                      CALL DECPOL ( 3, SOU(L_SOU)%S_VEC, RD, SOU(L_SOU)%ALPHA, &
     &                              SOU(L_SOU)%DELTA, IER )
                      CALL RH_TAT ( SOU(L_SOU)%ALPHA, 6, SOU(L_SOU)%ALPHA_STR, IER )
                      CALL RG_TAT ( SOU(L_SOU)%DELTA, 5, SOU(L_SOU)%DELTA_STR, IER )
                      CALL CHASHL ( SOU(L_SOU)%ALPHA_STR )
                      IF ( SOU(L_SOU)%DELTA_STR(1:1) == ' ' ) SOU(L_SOU)%DELTA_STR(1:1) = '+'
                      FL_SCN(J2) = .TRUE.
                 END IF
 420          CONTINUE 
 410       CONTINUE 
           SUR%PLA(1:L_SOU) = SOU(1:L_SOU)
         ELSE IF ( I_TYP == SUR__TYP_PAI ) THEN
           L_SOU = SUR%L_PAI
           IF ( L_SOU < 1 ) THEN
                CALL ERR_LOG ( 0, IUER )
                RETURN 
           END IF
           SOU(1:L_SOU) = SUR%PAI(1:L_SOU)
      END IF
!
      DO 430 J3=1,L_SOU
!
! ------ Check, whether this source has been put in the schedule
!
         FL_FOUND = .FALSE.
         DO 440 J4=1,SUR%L_SCN
            IF ( SUR%IND_SRC(J4) == J3  .AND. &
     &           SUR%SRC_TYP(J4) == I_TYP     ) FL_FOUND = .TRUE.
 440     CONTINUE
!
! ------ Not? Then bypass...
!
         IF ( .NOT. FL_FOUND ) GOTO 430
         IF ( I_TYP == SUR__TYP_SEC ) THEN
!
! ----------- This is the list of secondary sources
!
              FL_PRI = .FALSE.
!
! ----------- Check whether this source was also the primary target
!
              DO 450 J5=1,SUR%L_SCN
                 IF ( SUR%SRC_TYP(J5) == SUR__TYP_TAG ) THEN
                      IF ( SUR%SOU(SUR%IND_SRC(J5))%B1950_NAME == SOU(J3)%B1950_NAME ) FL_PRI = .TRUE.
                 END IF
 450          CONTINUE
!
! ----------- It was? Then bypass it
!
              IF ( FL_PRI ) GOTO 430
         END IF
         IF ( I_TYP == SUR__TYP_CAL ) THEN
!
! ----------- This is the list of calibrator sources
!
              FL_PRI = .FALSE.
!
! ----------- Check whether this source was also the primary target
!
              DO 460 J6=1,SUR%L_SCN
                 IF ( SUR%SRC_TYP(J6) == SUR__TYP_TAG ) THEN
                      IF ( SUR%SOU(SUR%IND_SRC(J6))%B1950_NAME == SOU(J3)%B1950_NAME ) FL_PRI = .TRUE.
                 END IF
 460          CONTINUE
!
! ----------- It was? Then bypass it
!
              IF ( FL_PRI ) GOTO 430
         END IF
!
         WRITE ( LUN_VEX, FMT='(A)' ) 'def '//SOU(J3)%B1950_NAME//';'
         WRITE ( LUN_VEX, FMT='(A)' ) '*   source_position_ref = '//SOU(J3)%J2000_NAME//'; '// &
     &                                     SUR__TYP_STR(I_TYP)
         WRITE ( LUN_VEX, FMT='(A)' ) '    source_name = '//SOU(J3)%B1950_NAME//';'
         WRITE ( LUN_VEX, FMT='(A)' ) '    IAU_name = '//SOU(J3)%B1950_NAME//';'
!
         STR  = '    ra = '// &
     &           SOU(J3)%ALPHA_STR(1:2)//'h'// &
     &           SOU(J3)%ALPHA_STR(4:5)//'m'// &
     &           SOU(J3)%ALPHA_STR(7:12)//'000s;'
         CALL BLANK_TO_ZERO ( STR(10:25) )
         WRITE ( LUN_VEX, FMT='(A)' ) STR(1:I_LEN(STR))
!
         STR = '    dec = '// &
     &          SOU(J3)%DELTA_STR(1:3)//'d'// &
     &          SOU(J3)%DELTA_STR(5:6)//"'"// &
     &          SOU(J3)%DELTA_STR(8:12)//'000";'
         IF ( STR(11:11) == ' ' ) STR(11:11) = '+'
         CALL BLANK_TO_ZERO ( STR(12:25) )
         WRITE ( LUN_VEX, FMT='(A)' ) STR(1:I_LEN(STR))
!
         WRITE ( LUN_VEX, FMT='(A)' ) '    ref_coord_frame = J2000;'
         WRITE ( LUN_VEX, FMT='(A)' ) 'enddef;'
         WRITE ( LUN_VEX, FMT='(A)' ) '*'
!
         CALL CLRCH ( STR )
         WRITE ( UNIT=STR, FMT='(A)' ) ' '// &
     &                                SOU(J3)%B1950_NAME//' $        '// &
     &                                SOU(J3)%ALPHA_STR(1:2)//' '// &
     &                                SOU(J3)%ALPHA_STR(4:5)//' '// &
     &                                SOU(J3)%ALPHA_STR(7:12)//'0      '// &
     &                                SOU(J3)%DELTA_STR(1:3)//' '// &
     &                                SOU(J3)%DELTA_STR(5:6)//' '// &
     &                                SOU(J3)%DELTA_STR(8:12)// &
     &                                '0        2000.0 0.0 '
         IF ( STR(39:39) == ' ' ) STR(39:39) = '+'
         CALL BLANK_TO_ZERO ( STR(26:32) )
         CALL BLANK_TO_ZERO ( STR(46:51) )
!
         N_FLX = N_FLX + 1
         BUF_FLX(N_FLX) = SOU(J3)%B1950_NAME//'  X  B 1.00   1.00 13000.0'
!
         L_TAB = L_TAB + 1
         CALL CLRCH ( BUF_TAB(L_TAB) )
         BUF_TAB(L_TAB)(1:8)   = SOU(J3)%B1950_NAME
         BUF_TAB(L_TAB)(11:20) = SOU(J3)%J2000_NAME
         BUF_TAB(L_TAB)(23:30) = SOU(J3)%B1950_NAME
         BUF_TAB(L_TAB)(43:43) = 'N'
         BUF_TAB(L_TAB)(46:58) = SOU(J3)%ALPHA_STR//'0'
         BUF_TAB(L_TAB)(60:72) = SOU(J3)%DELTA_STR//'0'
         CALL BLANK_TO_ZERO ( BUF_TAB(L_TAB)(46:58) )
         CALL BLANK_TO_ZERO ( BUF_TAB(L_TAB)(60:72) )
!
         STR = "source='"//SOU(J3)%B1950_NAME//"'  RA="// &
     &         SOU(J3)%ALPHA_STR(1:2)//":"// &
     &         SOU(J3)%ALPHA_STR(4:5)//":"// &
     &         SOU(J3)%ALPHA_STR(7:12)//"0  DEC="// &
     &         SOU(J3)%DELTA_STR(1:3)//":"// &
     &         SOU(J3)%DELTA_STR(5:6)//":"// &
     &         SOU(J3)%DELTA_STR(8:12)// &
     &         "0  EQUINOX='J2000' // "//SUR__TYP_STR(I_TYP)
         CALL BLANK_TO_ZERO ( STR(32:35) )
         CALL BLANK_TO_ZERO ( STR(49:53) )
         IF ( STR(42:42) == ' ' ) STR(42:42) = '+'
         WRITE ( UNIT=LUN_KEY, FMT='(A)' ) STR(1:I_LEN(STR))
         N_FLX = N_FLX + 1
         BUF_FLX(N_FLX) = SUR%SO2(J3)%B1950_NAME//'  X  B 1.00   1.00 13000.0'
 430  CONTINUE
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  SUR_SOU_OUT  !#!  
!
! ------------------------------------------------------------------------
!
      FUNCTION   UPROUND ( VAL, ROU, EPS )
! ************************************************************************
! *                                                                      *
! *   Axilliary routine UPROUND rounds REAL*8 value VAL over ROU from    *
! *   the upper limit with tolearance EPS.                               *
! *                                                                      *
! *  ### 09-JAN-2018    UPROUND    v1.0 (c)  L. Petrov  09-JAN-2018 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      REAL*8     UPROUND
      REAL*8     VAL, ROU, EPS
      INTEGER*4  IP
!
      IP = IDNINT(VAL/ROU)
      IF ( VAL - IP*ROU > EPS ) IP = IP + 1
      UPROUND = IP*ROU
!
      RETURN
      END  FUNCTION   UPROUND  !#!  
