      SUBROUTINE SUR_ASTRO_TROPO ( SUR, VTD, IVRB, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine SUR_ASTRO_TROPO
! *                                                                      *
! * ### 15-JAN-2007  SUR_ASTRO_TROPO  v2.7 (c) L. Petrov 09-JUL-2021 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'astro_constants.i'
      INCLUDE   'sur_sked.i'
      INCLUDE   'vtd.i'
      INCLUDE   'getpar.i'
      TYPE     ( SUR__TYPE ) :: SUR
      TYPE     ( SUR__TYPE ) :: SUR_SAVED
      TYPE     ( VTD__TYPE ) :: VTD
      TYPE     ( SUR_STA__TYPE ) SUR_STA_SAVE(SUR__M_STA)
      INTEGER*4  IVRB, IUER
      INTEGER*4  M_RANGE, M_TRY, M_DEC, M_DEC2, MU_TRY_LOW, MU_TRY_HIGH, LU_TRY_LOW
      PARAMETER  ( M_RANGE = 4        )
      PARAMETER  ( M_TRY   = 48*1024*1024 )
      PARAMETER  ( MU_TRY_LOW  =      512 )
      PARAMETER  ( MU_TRY_HIGH =  16*1024 )
      PARAMETER  ( LU_TRY_LOW  = 512*1024 )
      PARAMETER  ( M_DEC   = 50 )
      PARAMETER  ( M_DEC2  = 90 )
      LOGICAL*1  FL_TAPE_CHANGE, FL_DEC
      TYPE     ( SUR__OBS_TYPE ) :: SUR_OBS(M_RANGE)
      REAL*8     EL_RANGE(2,M_RANGE), EL_RANGE5_MIN, EL_RANGE5_MAX, &
     &           EL_MIN_TROPO, EL_MAX_TROPO, EL_MIN_RAN, EL_MAX_RAN
      REAL*8     AZ__MARGIN, EL__MARGIN, DUR_BLOCK21
      PARAMETER  ( AZ__MARGIN = 0.5D0*DEG__TO__RAD )
      PARAMETER  ( EL__MARGIN = 0.5D0*DEG__TO__RAD  )
      PARAMETER  ( DUR_BLOCK21 = 1200.0 )
      INTEGER*4  SUR_MJD_CUR, SUR_L_OBS_TAG, SUR_L_SCN, SUR_L_SCN_CAL
      REAL*8     SUR_TAI_CUR, SUR_STA_AZ_CUR(SUR__M_STA), &
     &           SUR_STA_EL_CUR(SUR__M_STA), SUR_STA_AZ_ACC_CUR(SUR__M_STA), &
     &           SCORE(SUR__M_SOU), SCORE_MAX, SLEW_TIME_SCAN(SUR__M_SCN), &
     &           AZ, EL, HA, TC, TW
      REAL*8     TAI_OBS, SLEW_TIME, SLEW_TIME_POST, &
     &           TIM_SLEW_MIN, FACTOR__SLEW, &
     &           TIM1, TIM2, AVR_SLEW, TROPO_LEN(4), TAI_TROPO(4), &
     &           TAI_0, SLEW_TIME_MIN, EL_MIN, EL_MAX, OBS_STA_SAVE(SUR__M_STA,SUR__M_SCN)
      REAL*8,    ALLOCATABLE :: TIM_SEQ(:,:)
      INTEGER*2, ALLOCATABLE :: SEQ_IND(:,:)
      PARAMETER  ( FACTOR__SLEW = 2.0D0 )
      INTEGER*4  J1, J2, J3, J4, J5, J6, J7, J8, J9, J10, J11, J12, J13, J14, &
     &           J15, J16, J17, J18, J19, J20, J21, J22, J23, J24, L_RANGE, &
     &           K1, K2, K3, K4, &
     &           MJD_OBS, MJD_0, IND_SRC(M_RANGE), L_TRY, LL_TRY, K_TRY, &
     &           U_TRY, TYP_PRE, IND_SLEW, IND_CAL, L_VIS(M_RANGE), &
     &           K_SOU(4), I_SOU(4), UTC_OBS_INT, K_STA(5), KK_STA(SUR__M_STA), &
     &           K_STA_SUM, LAST_TYP, MJD_TROPO(4), &
     &           L_SCN_SAVE, L_SCN_BEG, L_SCN_END, &
     &           KSTA_ARR(4,SUR__M_SOU), IER
      INTEGER*8  N_TRY
      REAL*8     TIM_CPU, TIM_WALL
      CHARACTER  STR*128, STR1*128, STR2*128, STR3*128
      LOGICAL*1  FL_VIS_UP(SUR__M_SOU), &
     &           FL_STA(SUR__M_STA,4), FL_FIRST
      LOGICAL*1, ALLOCATABLE :: FL_VIS_CAL(:,:,:) 
      CHARACTER  MJDSEC_TO_DATE*30, GET_CDATE*19
      INTEGER*4, EXTERNAL :: ILEN, I_LEN, MAX_LIST_R8, MIN_LIST_R8
      LOGICAL*1, EXTERNAL :: SUR_CHECK_VIS
      REAL*8,    EXTERNAL :: SUR_TROPO_GET_SLEW, SUR_GET_ELEV_REF, CPU_TIMER, WALL_TIMER
#ifdef GNU
      INTEGER*4, EXTERNAL :: COMPAR_R82
      INTEGER*4, EXTERNAL :: INT4
#else
      INTEGER*2, EXTERNAL :: COMPAR_R82
#endif
!
      L_SCN_BEG = SUR%L_SCN
      ALLOCATE ( SEQ_IND(M_RANGE,M_TRY) )
      ALLOCATE ( TIM_SEQ(2,M_TRY) )
      ALLOCATE ( FL_VIS_CAL(SUR__M_STA,SUR%L_CAL,4) )
      SEQ_IND = 0
      TIM_SEQ = -1.D-12
      TC = CPU_TIMER  ( %VAL(0) ) 
      TW = WALL_TIMER ( %VAL(0) ) 
      IF ( IVRB .GE. 5 ) THEN
           WRITE ( 6, 210 ) GET_CDATE(), SUR%L_SCN
 210       FORMAT ( ' SUR_ASTRO_TROPO  Start: ', A, ' Scan: ', I4 )
           CALL FLUSH ( 6 ) 
      END IF
!
      IF ( SUR%L_SCN < 9011 )  THEN
           FL_DEC = .FALSE.
         ELSE
           FL_DEC = .TRUE.
      END IF
      IF ( SUR%TROPO_RANGE == 0 ) THEN
           CALL ERR_LOG ( 0, IUER )
           RETURN 
        ELSE IF ( SUR%TROPO_RANGE == 1 ) THEN
           EL_RANGE(1,1) = 15.0D0*DEG__TO__RAD; EL_RANGE(2,1) = 40.0D0*DEG__TO__RAD
           EL_RANGE(1,2) = 30.0D0*DEG__TO__RAD; EL_RANGE(2,2) = 60.0D0*DEG__TO__RAD
           EL_RANGE(1,3) = 50.0D0*DEG__TO__RAD; EL_RANGE(2,3) = 90.0D0*DEG__TO__RAD
           EL_RANGE(1,4) = 15.0D0*DEG__TO__RAD; EL_RANGE(2,4) = 40.0D0*DEG__TO__RAD
           TROPO_LEN = SUR%TROPO_SCAN_LEN
           EL_MIN_TROPO = 1.D8
           EL_MAX_TROPO = 1.D8
           L_RANGE = 4
         ELSE IF ( SUR%TROPO_RANGE == 2 ) THEN
           EL_RANGE(1,1) = 12.0D0*DEG__TO__RAD; EL_RANGE(2,1) = 45.0D0*DEG__TO__RAD
           EL_RANGE(1,2) = 32.0D0*DEG__TO__RAD; EL_RANGE(2,2) = 65.0D0*DEG__TO__RAD
           EL_RANGE(1,3) = 45.0D0*DEG__TO__RAD; EL_RANGE(2,3) = 84.0D0*DEG__TO__RAD
           EL_RANGE(1,4) = 12.0D0*DEG__TO__RAD; EL_RANGE(2,4) = 45.0D0*DEG__TO__RAD
           TROPO_LEN = SUR%TROPO_SCAN_LEN
           EL_MIN_TROPO = 1.D8
           EL_MAX_TROPO = 1.D8
           L_RANGE = 4
         ELSE IF ( SUR%TROPO_RANGE == 3 ) THEN
           EL_RANGE(1,1) = 12.0D0*DEG__TO__RAD; EL_RANGE(2,1) = 45.0D0*DEG__TO__RAD
           EL_RANGE(1,2) = 30.0D0*DEG__TO__RAD; EL_RANGE(2,2) = 84.0D0*DEG__TO__RAD
           EL_RANGE(1,3) = 12.0D0*DEG__TO__RAD; EL_RANGE(2,3) = 45.0D0*DEG__TO__RAD
           EL_RANGE(1,4) = 30.0D0*DEG__TO__RAD; EL_RANGE(2,4) = 84.0D0*DEG__TO__RAD
           TROPO_LEN = SUR%TROPO_SCAN_LEN
           EL_MIN_TROPO = 1.D8
           EL_MAX_TROPO = 1.D8
           L_RANGE = 4
         ELSE IF ( SUR%TROPO_RANGE == 4 ) THEN
           EL_RANGE(1,1) = 10.0D0*DEG__TO__RAD; EL_RANGE(2,1) = 40.0D0*DEG__TO__RAD
           EL_RANGE(1,2) = 40.0D0*DEG__TO__RAD; EL_RANGE(2,2) = 65.0D0*DEG__TO__RAD
           EL_RANGE(1,3) = 55.0D0*DEG__TO__RAD; EL_RANGE(2,3) = 90.0D0*DEG__TO__RAD
           EL_RANGE(1,4) = 10.0D0*DEG__TO__RAD; EL_RANGE(2,4) = 40.0D0*DEG__TO__RAD
           TROPO_LEN(1)  =   SUR%TROPO_SCAN_LEN
           TROPO_LEN(2)  = 2*SUR%TROPO_SCAN_LEN
           TROPO_LEN(3)  =   SUR%TROPO_SCAN_LEN
           TROPO_LEN(4)  = 2*SUR%TROPO_SCAN_LEN
           EL_MIN_TROPO  = 1.D8
           EL_MAX_TROPO  = 1.D8
           L_RANGE = 4
         ELSE IF ( SUR%TROPO_RANGE == 5 ) THEN
           EL_RANGE(1,1) = 45.0D0*DEG__TO__RAD; EL_RANGE(2,1) = 90.0D0*DEG__TO__RAD
           EL_RANGE(1,2) = 13.0D0*DEG__TO__RAD; EL_RANGE(2,2) = 35.0D0*DEG__TO__RAD
           EL_RANGE(1,3) = 45.0D0*DEG__TO__RAD; EL_RANGE(2,3) = 90.0D0*DEG__TO__RAD
           EL_RANGE(1,4) = 13.0D0*DEG__TO__RAD; EL_RANGE(2,4) = 35.0D0*DEG__TO__RAD
           TROPO_LEN(1)  = 2*SUR%TROPO_SCAN_LEN
           TROPO_LEN(2)  =   SUR%TROPO_SCAN_LEN
           TROPO_LEN(3)  = 2*SUR%TROPO_SCAN_LEN
           TROPO_LEN(4)  =   SUR%TROPO_SCAN_LEN
           EL_MIN_TROPO  = 1.D8
           EL_MAX_TROPO  = 1.D8
           L_RANGE = 4
         ELSE IF ( SUR%TROPO_RANGE == 6 ) THEN
           EL_RANGE(1,1) = 45.0D0*DEG__TO__RAD; EL_RANGE(2,1) = 88.0D0*DEG__TO__RAD
           EL_RANGE(1,2) = 14.5D0*DEG__TO__RAD; EL_RANGE(2,2) = 35.0D0*DEG__TO__RAD
           EL_RANGE(1,3) = 45.0D0*DEG__TO__RAD; EL_RANGE(2,3) = 88.0D0*DEG__TO__RAD
           EL_RANGE(1,4) = 14.5D0*DEG__TO__RAD; EL_RANGE(2,4) = 35.0D0*DEG__TO__RAD
           TROPO_LEN(1)  = SUR%TROPO_SCAN_LEN
           TROPO_LEN(2)  = SUR%TROPO_SCAN_LEN
           TROPO_LEN(3)  = SUR%TROPO_SCAN_LEN
           TROPO_LEN(4)  = SUR%TROPO_SCAN_LEN
           EL_RANGE5_MIN = 30.0D0*DEG__TO__RAD
           EL_RANGE5_MAX = 90.0D0*DEG__TO__RAD
           EL_MIN_TROPO  = 5.0D0*DEG__TO__RAD
           EL_MAX_TROPO  = 88.0D0*DEG__TO__RAD
           L_RANGE = 4
         ELSE IF ( SUR%TROPO_RANGE == 7 ) THEN
           EL_RANGE(1,1) = 50.0D0*DEG__TO__RAD; EL_RANGE(2,1) = 88.0D0*DEG__TO__RAD
           EL_RANGE(1,2) = 15.0D0*DEG__TO__RAD; EL_RANGE(2,2) = 35.0D0*DEG__TO__RAD
           EL_RANGE(1,3) = 50.0D0*DEG__TO__RAD; EL_RANGE(2,3) = 88.0D0*DEG__TO__RAD
           EL_RANGE(1,4) = 15.0D0*DEG__TO__RAD; EL_RANGE(2,4) = 35.0D0*DEG__TO__RAD
           TROPO_LEN(1)  = SUR%TROPO_SCAN_LEN
           TROPO_LEN(2)  = SUR%TROPO_SCAN_LEN
           TROPO_LEN(3)  = SUR%TROPO_SCAN_LEN
           TROPO_LEN(4)  = SUR%TROPO_SCAN_LEN
           EL_RANGE5_MIN = 35.0D0*DEG__TO__RAD
           EL_RANGE5_MAX = 90.0D0*DEG__TO__RAD
           EL_MIN_TROPO  =  5.0D0*DEG__TO__RAD
           EL_MAX_TROPO  = 88.0D0*DEG__TO__RAD
           L_RANGE = 4
         ELSE IF ( SUR%TROPO_RANGE == 8 ) THEN
           EL_RANGE(1,1) = 30.0D0*DEG__TO__RAD; EL_RANGE(2,1) = 88.0D0*DEG__TO__RAD
           EL_RANGE(1,2) = 30.0D0*DEG__TO__RAD; EL_RANGE(2,2) = 88.0D0*DEG__TO__RAD
           EL_RANGE(1,3) = 30.0D0*DEG__TO__RAD; EL_RANGE(2,3) = 88.0D0*DEG__TO__RAD
           EL_RANGE(1,4) = 30.0D0*DEG__TO__RAD; EL_RANGE(2,4) = 88.0D0*DEG__TO__RAD
           TROPO_LEN(1)  = SUR%TROPO_SCAN_LEN
           TROPO_LEN(2)  = SUR%TROPO_SCAN_LEN
           TROPO_LEN(3)  = SUR%TROPO_SCAN_LEN
           TROPO_LEN(4)  = SUR%TROPO_SCAN_LEN
           EL_MIN_TROPO  = 30.0D0*DEG__TO__RAD
           EL_MAX_TROPO  = 88.0D0*DEG__TO__RAD
           L_RANGE = 1
         ELSE IF ( SUR%TROPO_RANGE == 9 ) THEN
           EL_RANGE(1,1) = 30.0D0*DEG__TO__RAD; EL_RANGE(2,1) = 60.0D0*DEG__TO__RAD
           EL_RANGE(1,2) = 60.0D0*DEG__TO__RAD; EL_RANGE(2,2) = 88.0D0*DEG__TO__RAD
           EL_RANGE(1,3) = 30.0D0*DEG__TO__RAD; EL_RANGE(2,3) = 60.0D0*DEG__TO__RAD
           EL_RANGE(1,4) = 60.0D0*DEG__TO__RAD; EL_RANGE(2,4) = 88.0D0*DEG__TO__RAD
           TROPO_LEN(1)  = SUR%TROPO_SCAN_LEN
           TROPO_LEN(2)  = SUR%TROPO_SCAN_LEN
           TROPO_LEN(3)  = SUR%TROPO_SCAN_LEN
           TROPO_LEN(4)  = SUR%TROPO_SCAN_LEN
           EL_MIN_TROPO  = 30.0D0*DEG__TO__RAD
           EL_MAX_TROPO  = 88.0D0*DEG__TO__RAD
           L_RANGE = 4
         ELSE IF ( SUR%TROPO_RANGE == 10 ) THEN
           EL_RANGE(1,1) = 10.0D0*DEG__TO__RAD; EL_RANGE(2,1) = 88.0D0*DEG__TO__RAD
           EL_RANGE(1,2) = 10.0D0*DEG__TO__RAD; EL_RANGE(2,2) = 88.0D0*DEG__TO__RAD
           EL_RANGE(1,3) = 10.0D0*DEG__TO__RAD; EL_RANGE(2,3) = 88.0D0*DEG__TO__RAD
           EL_RANGE(1,4) = 10.0D0*DEG__TO__RAD; EL_RANGE(2,4) = 88.0D0*DEG__TO__RAD
           TROPO_LEN(1)  = SUR%TROPO_SCAN_LEN
           TROPO_LEN(2)  = 0.0D0
           TROPO_LEN(3)  = 0.0D0
           TROPO_LEN(4)  = 0.0D0
           EL_MIN_TROPO  = 10.0D0*DEG__TO__RAD
           EL_MAX_TROPO  = 88.0D0*DEG__TO__RAD
           L_RANGE = 1
         ELSE IF ( SUR%TROPO_RANGE == 11 ) THEN
           EL_RANGE(1,1) = 12.0D0*DEG__TO__RAD; EL_RANGE(2,1) = 30.0D0*DEG__TO__RAD
           EL_RANGE(1,2) = 50.0D0*DEG__TO__RAD; EL_RANGE(2,2) = 88.0D0*DEG__TO__RAD
           EL_RANGE(1,3) = 12.0D0*DEG__TO__RAD; EL_RANGE(2,3) = 30.0D0*DEG__TO__RAD
           EL_RANGE(1,4) = 50.0D0*DEG__TO__RAD; EL_RANGE(2,4) = 88.0D0*DEG__TO__RAD
           TROPO_LEN(1)  = SUR%TROPO_SCAN_LEN
           TROPO_LEN(2)  = SUR%TROPO_SCAN_LEN
           TROPO_LEN(3)  = 0.0D0
           TROPO_LEN(4)  = 0.0D0
           EL_MIN_TROPO  = 12.0D0*DEG__TO__RAD
           EL_MAX_TROPO  = 88.0D0*DEG__TO__RAD
           L_RANGE = 2
         ELSE IF ( SUR%TROPO_RANGE == 12 ) THEN
           EL_RANGE(1,1) = 30.0D0*DEG__TO__RAD; EL_RANGE(2,1) = 88.0D0*DEG__TO__RAD
           EL_RANGE(1,2) = 30.0D0*DEG__TO__RAD; EL_RANGE(2,2) = 88.0D0*DEG__TO__RAD
           EL_RANGE(1,3) = 30.0D0*DEG__TO__RAD; EL_RANGE(2,3) = 88.0D0*DEG__TO__RAD
           EL_RANGE(1,4) = 30.0D0*DEG__TO__RAD; EL_RANGE(2,4) = 88.0D0*DEG__TO__RAD
           TROPO_LEN(1)  = SUR%TROPO_SCAN_LEN
           TROPO_LEN(2)  = SUR%TROPO_SCAN_LEN
           TROPO_LEN(3)  = SUR%TROPO_SCAN_LEN
           TROPO_LEN(4)  = SUR%TROPO_SCAN_LEN
           EL_MIN_TROPO  = 30.0D0*DEG__TO__RAD
           EL_MAX_TROPO  = 88.0D0*DEG__TO__RAD
           L_RANGE = 2
         ELSE IF ( SUR%TROPO_RANGE == 13 ) THEN
           EL_RANGE(1,1) = 16.0D0*DEG__TO__RAD; EL_RANGE(2,1) = 79.0D0*DEG__TO__RAD
           EL_RANGE(1,2) = 16.0D0*DEG__TO__RAD; EL_RANGE(2,2) = 79.0D0*DEG__TO__RAD
           EL_RANGE(1,3) = 16.0D0*DEG__TO__RAD; EL_RANGE(2,3) = 79.0D0*DEG__TO__RAD
           EL_RANGE(1,4) = 16.0D0*DEG__TO__RAD; EL_RANGE(2,4) = 79.0D0*DEG__TO__RAD
           TROPO_LEN(1)  = SUR%TROPO_SCAN_LEN
           TROPO_LEN(2)  = 0.0D0
           TROPO_LEN(3)  = 0.0D0
           TROPO_LEN(4)  = 0.0D0
           EL_MIN_TROPO  = 16.0D0*DEG__TO__RAD
           EL_MAX_TROPO  = 79.0D0*DEG__TO__RAD
           L_RANGE = 1
         ELSE IF ( SUR%TROPO_RANGE == 14 ) THEN
           EL_RANGE(1,1) = 45.0D0*DEG__TO__RAD; EL_RANGE(2,1) = 84.0D0*DEG__TO__RAD
           EL_RANGE(1,2) = 12.0D0*DEG__TO__RAD; EL_RANGE(2,2) = 45.0D0*DEG__TO__RAD
           EL_RANGE(1,3) = 45.0D0*DEG__TO__RAD; EL_RANGE(2,3) = 84.0D0*DEG__TO__RAD
           EL_RANGE(1,4) = 12.0D0*DEG__TO__RAD; EL_RANGE(2,4) = 45.0D0*DEG__TO__RAD
           TROPO_LEN(1)  = SUR%TROPO_SCAN_LEN
           TROPO_LEN(2)  = SUR%TROPO_SCAN_LEN
           TROPO_LEN(3)  = SUR%TROPO_SCAN_LEN
           TROPO_LEN(4)  = SUR%TROPO_SCAN_LEN
           EL_MIN_TROPO  = 12.0D0*DEG__TO__RAD
           EL_MAX_TROPO  = 84.0D0*DEG__TO__RAD
           L_RANGE = 4
         ELSE IF ( SUR%TROPO_RANGE == 15 ) THEN
           EL_RANGE(1,1) = 10.0D0*DEG__TO__RAD; EL_RANGE(2,1) = 40.0D0*DEG__TO__RAD
           EL_RANGE(1,2) = 30.0D0*DEG__TO__RAD; EL_RANGE(2,2) = 60.0D0*DEG__TO__RAD
           EL_RANGE(1,3) = 10.0D0*DEG__TO__RAD; EL_RANGE(2,3) = 40.0D0*DEG__TO__RAD
           EL_RANGE(1,4) = 30.0D0*DEG__TO__RAD; EL_RANGE(2,4) = 60.0D0*DEG__TO__RAD
           TROPO_LEN(1)  = SUR%TROPO_SCAN_LEN
           TROPO_LEN(2)  = SUR%TROPO_SCAN_LEN
           TROPO_LEN(3)  = SUR%TROPO_SCAN_LEN
           TROPO_LEN(4)  = SUR%TROPO_SCAN_LEN
           EL_MIN_TROPO  = 10.0D0*DEG__TO__RAD
           EL_MAX_TROPO  = 60.0D0*DEG__TO__RAD
           L_RANGE = 4
         ELSE IF ( SUR%TROPO_RANGE == 16 ) THEN
           EL_RANGE(1,1) = 10.0D0*DEG__TO__RAD; EL_RANGE(2,1) = 60.0D0*DEG__TO__RAD
           EL_RANGE(1,2) = 10.0D0*DEG__TO__RAD; EL_RANGE(2,2) = 60.0D0*DEG__TO__RAD
           EL_RANGE(1,3) = 10.0D0*DEG__TO__RAD; EL_RANGE(2,3) = 60.0D0*DEG__TO__RAD
           EL_RANGE(1,4) = 10.0D0*DEG__TO__RAD; EL_RANGE(2,4) = 60.0D0*DEG__TO__RAD
           TROPO_LEN(1)  = SUR%TROPO_SCAN_LEN
           TROPO_LEN(2)  = SUR%TROPO_SCAN_LEN
           TROPO_LEN(3)  = SUR%TROPO_SCAN_LEN
           TROPO_LEN(4)  = 0.0
           EL_MIN_TROPO  = 10.0D0*DEG__TO__RAD
           EL_MAX_TROPO  = 60.0D0*DEG__TO__RAD
           L_RANGE = 3
         ELSE IF ( SUR%TROPO_RANGE == 17 ) THEN
           EL_RANGE(1,1) = 20.0D0*DEG__TO__RAD; EL_RANGE(2,1) = 88.0D0*DEG__TO__RAD
           EL_RANGE(1,2) = 20.0D0*DEG__TO__RAD; EL_RANGE(2,2) = 88.0D0*DEG__TO__RAD
           EL_RANGE(1,3) = 20.0D0*DEG__TO__RAD; EL_RANGE(2,3) = 88.0D0*DEG__TO__RAD
           EL_RANGE(1,4) = 20.0D0*DEG__TO__RAD; EL_RANGE(2,4) = 88.0D0*DEG__TO__RAD
           TROPO_LEN(1)  = SUR%TROPO_SCAN_LEN
           TROPO_LEN(2)  = SUR%TROPO_SCAN_LEN
           TROPO_LEN(3)  = SUR%TROPO_SCAN_LEN
           TROPO_LEN(4)  = SUR%TROPO_SCAN_LEN
           EL_MIN_TROPO  = 20.0D0*DEG__TO__RAD
           EL_MAX_TROPO  = 88.0D0*DEG__TO__RAD
           L_RANGE = 2
         ELSE IF ( SUR%TROPO_RANGE == 18 ) THEN
           EL_RANGE(1,1) = 10.0D0*DEG__TO__RAD; EL_RANGE(2,1) = 30.0D0*DEG__TO__RAD
           EL_RANGE(1,2) = 40.0D0*DEG__TO__RAD; EL_RANGE(2,2) = 88.0D0*DEG__TO__RAD
           EL_RANGE(1,3) = 10.0D0*DEG__TO__RAD; EL_RANGE(2,3) = 30.0D0*DEG__TO__RAD
           EL_RANGE(1,4) = 40.0D0*DEG__TO__RAD; EL_RANGE(2,4) = 88.0D0*DEG__TO__RAD
           TROPO_LEN(1)  = SUR%TROPO_SCAN_LEN
           TROPO_LEN(2)  = SUR%TROPO_SCAN_LEN
           TROPO_LEN(3)  = SUR%TROPO_SCAN_LEN
           TROPO_LEN(4)  = SUR%TROPO_SCAN_LEN
           EL_MIN_TROPO  = 10.0D0*DEG__TO__RAD
           EL_MAX_TROPO  = 88.0D0*DEG__TO__RAD
           L_RANGE = 4
         ELSE IF ( SUR%TROPO_RANGE == 19 ) THEN
           EL_RANGE(1,1) = 10.0D0*DEG__TO__RAD; EL_RANGE(2,1) = 30.0D0*DEG__TO__RAD
           EL_RANGE(1,2) = 50.0D0*DEG__TO__RAD; EL_RANGE(2,2) = 88.0D0*DEG__TO__RAD
           EL_RANGE(1,3) = 10.0D0*DEG__TO__RAD; EL_RANGE(2,3) = 30.0D0*DEG__TO__RAD
           EL_RANGE(1,4) = 50.0D0*DEG__TO__RAD; EL_RANGE(2,4) = 88.0D0*DEG__TO__RAD
           TROPO_LEN(1)  = SUR%TROPO_SCAN_LEN
           TROPO_LEN(2)  = SUR%TROPO_SCAN_LEN
           TROPO_LEN(3)  = SUR%TROPO_SCAN_LEN
           TROPO_LEN(4)  = SUR%TROPO_SCAN_LEN
           EL_MIN_TROPO  = 10.0D0*DEG__TO__RAD
           EL_MAX_TROPO  = 88.0D0*DEG__TO__RAD
           L_RANGE = 4
         ELSE IF ( SUR%TROPO_RANGE == 21 ) THEN
           EL_RANGE(1,1) = 45.0D0*DEG__TO__RAD; EL_RANGE(2,1) = 84.0D0*DEG__TO__RAD
           EL_RANGE(1,2) = 12.0D0*DEG__TO__RAD; EL_RANGE(2,2) = 45.0D0*DEG__TO__RAD
           EL_RANGE(1,3) = 45.0D0*DEG__TO__RAD; EL_RANGE(2,3) = 84.0D0*DEG__TO__RAD
           EL_RANGE(1,4) = 12.0D0*DEG__TO__RAD; EL_RANGE(2,4) = 45.0D0*DEG__TO__RAD
           TROPO_LEN(1)  = SUR%TROPO_SCAN_LEN
           TROPO_LEN(2)  = SUR%TROPO_SCAN_LEN
           TROPO_LEN(3)  = SUR%TROPO_SCAN_LEN
           TROPO_LEN(4)  = SUR%TROPO_SCAN_LEN
           EL_MIN_TROPO  = 12.0D0*DEG__TO__RAD
           EL_MAX_TROPO  = 84.0D0*DEG__TO__RAD
           L_RANGE = 4
      END IF
      EL_MIN_RAN = MIN ( EL_RANGE(1,1), EL_RANGE(1,2), EL_RANGE(1,3), EL_RANGE(1,4) )
      EL_MAX_RAN = MAX ( EL_RANGE(2,1), EL_RANGE(2,2), EL_RANGE(2,3), EL_RANGE(2,4) )
!
      AVR_SLEW = SUR%AVR_SLEW_TROPO_TIME
      IF ( SUR%TROPO_BURST_INTERVAL > 4*86400.0D0 ) THEN
           CALL ERR_LOG ( 0, IUER )
           RETURN
      END IF
      SUR_STA_SAVE = SUR%STA
      DO 410 J1=1,SUR%L_STA
         IF ( SUR%L_SCN == 0 ) THEN
              FL_FIRST = .TRUE.
              SUR%STA(J1)%EL_CUR      = -101.D0
              SUR%STA(J1)%AZ_CUR      = -101.D0
              SUR%STA(J1)%ALP_CUR     = -101.D0
              SUR%STA(J1)%DEL_CUR     = -101.D0
              SUR%STA(J1)%HA_CUR      = -101.D0
              SUR%STA(J1)%AZ_ACC_CUR  = -101.D0
              SUR%STA(J1)%HA_ACC_CUR  = -101.D0
            ELSE
              FL_FIRST = .FALSE.
         END IF
 410  CONTINUE
      IF ( SUR%POCAL_STYLE == POCAL_STYLE__KVN ) THEN
           CALL ERR_PASS ( IUER, IER )
           CALL SUR_POCAL_KVN ( SUR, VTD, IVRB, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 1791, IUER, 'SUR_ASTRO_TROPO', 'Error '// &
     &              'in an attempt to reserve time for point calibration' )
                SUR%STA = SUR_STA_SAVE
                RETURN
           END IF
           FL_FIRST = .FALSE.
        ELSE IF ( SUR%POCAL_STYLE == POCAL_STYLE__ATCA ) THEN
           CALL ERR_PASS ( IUER, IER )
           CALL SUR_POCAL_ATCA ( SUR, VTD, IVRB, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 1792, IUER, 'SUR_ASTRO_TROPO', 'Error '// &
     &              'in an attempt to reserve time for point calibration' )
                SUR%STA = SUR_STA_SAVE
                RETURN
           END IF
           FL_FIRST = .FALSE.
      END IF
!
! --- Save current state
!
      CALL SUR_CUR_SAVE ( SUR, SUR_OBS(1) )
      OBS_STA_SAVE = SUR%OBS_STA
      SUR%OBS_STA(1:SUR%L_STA,1) = SUR__UND
      SUR%L_SCN = 2
      SLEW_TIME_SCAN = 0.0D0
!
! --- Set anticipated start time
!
      DO 420 J2=1,4
         IF ( J2 == 1 ) THEN
              MJD_TROPO(J2) = SUR%MJD_CUR
              TAI_TROPO(J2) = SUR%TAI_CUR
            ELSE
              MJD_TROPO(J2) = MJD_TROPO(J2-1)
              TAI_TROPO(J2) = TAI_TROPO(J2-1) + AVR_SLEW + TROPO_LEN(J2-1) + &
     &                        MAX(0.0D0, SUR%PREOBS_LONG)
         END IF
         IF ( TAI_TROPO(J2) > 86400.0D0 ) THEN
              TAI_TROPO(J2) = TAI_TROPO(J2) - 86400.0D0
              MJD_TROPO(J2) = MJD_TROPO(J2) + 1
         END IF
         STR = MJDSEC_TO_DATE ( MJD_TROPO(J2), TAI_TROPO(J2), -2 )
         IF ( IVRB .GE. 5 ) THEN
              WRITE ( 6, * ) 'SUR_ASTRO_TROPO Expected epoch: ', INT2(J2), ' Date: '// STR(1:21)
              CALL FLUSH ( 6 )
         END IF
 420  CONTINUE
      K_SOU = 0
!
      DO 430 J3=1,SUR%L_CAL
         CALL ERR_PASS ( IUER, IER )
         FL_VIS_CAL(1:SUR__M_STA,J3,1:4) = .FALSE. 
         SLEW_TIME = SUR_TROPO_GET_SLEW ( SUR, VTD, MJD_TROPO(2), TAI_TROPO(2), &
     &                             TROPO_LEN(2), SUR__TYP_CAL, J3, EL_MIN_RAN, &
     &                             EL_MAX_RAN, SUR__FINE, K_STA(5), FL_STA(1,1), &
     &                             IER )
         IF ( IVRB .GE. 7 ) THEN
              WRITE ( 6, 230 ) SUR%CAL(J3)%J2000_NAME, FL_STA(1:SUR%L_STA,1)
 230          FORMAT ( 8X, 'SUR_ASTRO_TROPO Sou: ', A, ' FL_STA= ', 32(L1,1X) )
         END IF
         IF ( K_STA(5) < SUR%TROPO_MIN_STA ) THEN
              FL_VIS_UP(J3) = .FALSE.
            ELSE 
              FL_VIS_UP(J3) = .TRUE.
         END IF      
         IF ( FL_VIS_UP(J3) ) THEN
              DO 440 J4=1,4
                 CALL ERR_PASS ( IUER, IER )
                 SLEW_TIME = SUR_TROPO_GET_SLEW ( SUR, VTD, MJD_TROPO(J4), TAI_TROPO(J4), &
     &                                 TROPO_LEN(J4), SUR__TYP_CAL, J3, &
     &                                 EL_RANGE(1,J4), EL_RANGE(2,J4), &
     &                                 SUR__FINE, KSTA_ARR(J4,J3), FL_VIS_CAL(1,J3,J4), IER )
 440          CONTINUE 
              K_SOU(1) = K_SOU(1) + 1
              IF ( IVRB .GE. 5 ) THEN
                   WRITE ( UNIT=STR,  FMT='(32(L1,1X))' ) FL_VIS_CAL(1:MIN(32,SUR%L_STA),J3,1)
                   WRITE ( UNIT=STR1, FMT='(32(L1,1X))' ) FL_VIS_CAL(1:MIN(32,SUR%L_STA),J3,2)
                   WRITE ( UNIT=STR2, FMT='(32(L1,1X))' ) FL_VIS_CAL(1:MIN(32,SUR%L_STA),J3,3)
                   WRITE ( UNIT=STR3, FMT='(32(L1,1X))' ) FL_VIS_CAL(1:MIN(32,SUR%L_STA),J3,4)
!
                   WRITE ( 6, 240 ) J3, SUR%CAL(J3)%J2000_NAME, &
     &                                  STR(1:I_LEN(STR)), &
     &                                  STR1(1:I_LEN(STR1)), &
     &                                  STR2(1:I_LEN(STR2)), &
     &                                  STR3(1:I_LEN(STR3)), &
     &                                  KSTA_ARR(1,J3), KSTA_ARR(2,J3), &
     &                                  KSTA_ARR(3,J3), KSTA_ARR(4,J3), &
     &                                  K_STA(5)
 240               FORMAT ( I4,' )  Sou: ', A, 1X, &
     &                         ' Range_1: ', A, &
     &                         ' Range_2: ', A, &
     &                         ' Range_3: ', A, &
     &                         ' Range_4: ', A, 1X, &
     &                         ' K_sta: ', 4(I2, 2X), ' K_all: ', I2 )
              END IF
         END IF
 430  CONTINUE 
 830  CONTINUE 
      CALL SUR_CUR_RESTORE ( SUR, SUR_OBS(1) )
!
      IF ( IVRB .GE. 3 ) THEN
           WRITE ( 6, 110 ) SUR%L_SCN, K_SOU(1)
 110       FORMAT ( '  Tropo  Scan: ',I4,'  # sources up: ', I4 )
           CALL FLUSH ( 6 )
      END IF
!
      L_TRY = 0
      N_TRY = 0
!
      DO 450 J5=1,SUR%L_CAL
         IF ( .NOT. FL_VIS_UP(J5) ) GOTO 450
         IF ( L_RANGE == 1 ) THEN
              N_TRY = N_TRY + 1
              L_TRY = L_TRY + 1
              SEQ_IND(1,L_TRY) = J5
              GOTO 450
         END IF
         DO 460 J6=1,SUR%L_CAL
            IF ( J6 == J5 ) GOTO 460
            IF ( .NOT. FL_VIS_UP(J6) ) GOTO 460
            DO 470 J7=1,SUR%L_CAL
               IF ( J7 == J5 ) GOTO 470
               IF ( J7 == J6 ) GOTO 470
               IF ( .NOT. FL_VIS_UP(J7) ) GOTO 470
               DO 480 J8=1,SUR%L_CAL
                  IF ( J8 == J5 ) GOTO 480
                  IF ( J8 == J6 ) GOTO 480
                  IF ( J8 == J7 ) GOTO 480
                  IF ( .NOT. FL_VIS_UP(J8) ) GOTO 480
                  N_TRY = N_TRY + 1
                  DO 490 J9=1,SUR%L_STA
                     IF ( SUR%STA(J9)%TAGALONE ) GOTO 490
                     IF ( SUR%TROPO_RANGE == 33 .OR. &
     &                    SUR%TROPO_RANGE == 5 .OR. &
     &                    SUR%TROPO_RANGE == 6 .OR. &
     &                    SUR%TROPO_RANGE == 7      ) THEN
                          L_VIS = 0
!
                          IF ( FL_VIS_CAL(J9,J8,1) ) L_VIS(1) = L_VIS(1) + 1
                          IF ( FL_VIS_CAL(J9,J7,1) ) L_VIS(1) = L_VIS(1) + 1
                          IF ( FL_VIS_CAL(J9,J6,1) ) L_VIS(1) = L_VIS(1) + 1
                          IF ( FL_VIS_CAL(J9,J5,1) ) L_VIS(1) = L_VIS(1) + 1
!
                          IF ( FL_VIS_CAL(J9,J8,2) ) L_VIS(2) = L_VIS(2) + 1
                          IF ( FL_VIS_CAL(J9,J7,2) ) L_VIS(2) = L_VIS(2) + 1
                          IF ( FL_VIS_CAL(J9,J6,2) ) L_VIS(2) = L_VIS(2) + 1
                          IF ( FL_VIS_CAL(J9,J5,2) ) L_VIS(2) = L_VIS(2) + 1
!
                          IF ( FL_VIS_CAL(J9,J8,3) ) L_VIS(1) = L_VIS(1) + 1
                          IF ( FL_VIS_CAL(J9,J7,3) ) L_VIS(1) = L_VIS(1) + 1
                          IF ( FL_VIS_CAL(J9,J6,3) ) L_VIS(1) = L_VIS(1) + 1
                          IF ( FL_VIS_CAL(J9,J5,3) ) L_VIS(1) = L_VIS(1) + 1
!
                          IF ( FL_VIS_CAL(J9,J8,4) ) L_VIS(2) = L_VIS(2) + 1
                          IF ( FL_VIS_CAL(J9,J7,4) ) L_VIS(2) = L_VIS(2) + 1
                          IF ( FL_VIS_CAL(J9,J6,4) ) L_VIS(2) = L_VIS(2) + 1
                          IF ( FL_VIS_CAL(J9,J5,4) ) L_VIS(2) = L_VIS(2) + 1
!
                          IF ( SUR%TROPO_RANGE == 3 ) THEN
                               IF ( L_VIS(1) < 2 ) GOTO 480
                               IF ( L_VIS(2) < 2 ) GOTO 480
                             ELSE
                               IF ( L_VIS(1) < 1 ) GOTO 480
                               IF ( L_VIS(2) < 1 ) GOTO 480
                          END IF
                        ELSE 
                          CONTINUE 
                     END IF
 490              CONTINUE 
!
                  IF ( KSTA_ARR(1,J8) .GE. SUR%TROPO_MIN_STA .AND. &
     &                 KSTA_ARR(2,J7) .GE. SUR%TROPO_MIN_STA .AND. &
     &                 KSTA_ARR(3,J6) .GE. SUR%TROPO_MIN_STA .AND. &
     &                 KSTA_ARR(4,J5) .GE. SUR%TROPO_MIN_STA .AND. &
     &                 KSTA_ARR(1,J8) .GE. SUR%TROPO_MIN_STA .AND. &
     &                 KSTA_ARR(2,J7) .GE. SUR%TROPO_MIN_STA .AND. &
     &                 KSTA_ARR(3,J6) .GE. SUR%TROPO_MIN_STA .AND. &
     &                 KSTA_ARR(4,J5) .GE. SUR%TROPO_MIN_STA       ) THEN
                       L_TRY = L_TRY + 1
                       IF ( L_TRY > M_TRY ) THEN
                            L_TRY = L_TRY - 1
                            GOTO 850
                       END IF
                       SEQ_IND(1,L_TRY) = J8
                       SEQ_IND(2,L_TRY) = J7
                       SEQ_IND(3,L_TRY) = J6
                       SEQ_IND(4,L_TRY) = J5
                  END IF
 480           CONTINUE 
 470        CONTINUE 
 460     CONTINUE 
 450  CONTINUE 
 850  CONTINUE 
!
      IF ( IVRB .GE. 6 ) THEN 
           WRITE ( 6, * ) ' SUR_ASTRO_TROPO  L_TRY:  ', L_TRY, ' N_TRY: ', N_TRY
           WRITE ( 6, * ) ' SUR_ASTRO_TROPO  Now: '//GET_CDATE()
           CALL FLUSH ( 6 )
      END IF
      IF ( L_TRY == 0 ) THEN
           STR = MJDSEC_TO_DATE ( SUR%MJD_CUR, SUR%TAi_CUR,  -2 )  
           CALL ERR_LOG ( 1793, IUER, 'SUR_ASTRO_TROPO', 'No '// &
     &         'tropospheric calibrators were found in '//STR(1:24) )
           SUR%STA = SUR_STA_SAVE
           RETURN
      END IF
!
      MJD_0 = SUR%MJD_CUR
      TAI_0 = SUR%TAI_CUR
      CALL SUR_CUR_SAVE ( SUR, SUR_OBS(1) )
      U_TRY = 0
      LL_TRY = L_TRY
      DO 4110 J11=1,L_TRY
         CALL SUR_CUR_RESTORE ( SUR, SUR_OBS(1) )
         MJD_OBS = SUR%MJD_CUR
         TAI_OBS = SUR%TAI_CUR
         TIM_SEQ(1,J11) =  1.0D8 + J11
         TIM_SEQ(2,J11) = -1.01
!
! ------ KK_STA(k) is the number of sources that station k sees during
! ------ this burst
!
         KK_STA = 0
         DO 4120 J12=1,M_RANGE
            IF ( J12 > L_RANGE ) GOTO 4120
!
! --------- K_STA -- the number of stations that see the source at 
! --------- a given elevation range for the J12-th observations
!
            K_STA(J12) = 0
            DO 4130 J13=1,SUR%L_STA
               IF ( .NOT. SUR%STA(J13)%TAGALONE .AND. &
     &              FL_VIS_CAL(J13,SEQ_IND(J12,J11),J12) ) THEN
!
                    K_STA(J12)  =  K_STA(J12) + 1
                    KK_STA(J13) = KK_STA(J13) + 1
               END IF
 4130       CONTINUE 
!
! --------- Discard the sequence that has a source that is up for 
! --------- too few stations
!
            IF ( K_STA(J12) < SUR%TROPO_MIN_STA ) GOTO 4110
 4120    CONTINUE 
         IF ( IVRB .GE. 16 ) THEN
              WRITE ( 6, *  ) 'SUR_ASTRO_TROPO try-1 J11= ',  J11, ' U_TRY = ', INT2(U_TRY)
         END IF
!
         IF ( L_RANGE > 2 .AND. &
     &        .NOT. ( SUR%TROPO_RANGE == 5 .OR. &
     &                SUR%TROPO_RANGE == 6 .OR. &
     &                SUR%TROPO_RANGE == 7      ) ) THEN
    
!
! ----------- Discard the sequence if there are stations that 
! ----------- had less than 2 calibrator observations
!
              DO 4140 J14=1,SUR%L_STA
                 IF ( .NOT. SUR%STA(J14)%TAGALONE .AND. &
     &                KK_STA(J14) < 2                   ) GOTO 4110
 4140         CONTINUE 
           ELSE IF ( SUR%TROPO_RANGE == 12 ) THEN
!
! ----------- Discard a sequence that has less than 
!
              DO 5140 J14=1,SUR%L_STA
                 IF ( ( .NOT. SUR%STA(J14)%TAGALONE ) .AND. KK_STA(J14) < 1 ) GOTO 4110
 5140         CONTINUE 
         END IF
         IF ( IVRB .GE. 16 ) THEN
              WRITE ( 6, *  ) 'SUR_ASTRO_TROPO try-2 J11= ',  J11, ' U_TRY = ', INT2(U_TRY)
         END IF
!
         K_STA_SUM = 0
         DO 4150 J15=1,M_RANGE
            CALL ERR_PASS ( IUER, IER )
            IF ( SUR%TROPO_RANGE == 5 .OR. &
     &           SUR%TROPO_RANGE == 6 .OR. &
     &           SUR%TROPO_RANGE == 7      ) THEN
!
                 SLEW_TIME = SUR_TROPO_GET_SLEW ( SUR, VTD, MJD_OBS, TAI_OBS + MAX(0.0D0, SUR%PREOBS_LONG), &
     &                                 TROPO_LEN(J15), SUR__TYP_CAL, &
     &                                 INT4(SEQ_IND(J15,J11)), EL_MIN_RAN, P2I, &
     &                                 SUR__FINE, K_STA(J15), FL_STA(1,J15), &
     &                                 IER )
               ELSE 
                 SLEW_TIME = SUR_TROPO_GET_SLEW ( SUR, VTD, MJD_OBS, TAI_OBS + MAX(0.0D0, SUR%PREOBS_LONG), &
     &                                 TROPO_LEN(J15), SUR__TYP_CAL, INT4(SEQ_IND(J15,J11)), &
     &                                 EL_RANGE(1,J15), EL_RANGE(2,J15), &
     &                                 SUR__FINE, K_STA(J15), FL_STA(1,J15), &
     &                                 IER )
            END IF
            IF ( IER .NE. 0 ) THEN
                 CALL ERR_LOG ( 1794, IUER, 'SUR_ASTRO_TROPO', 'Error '// &
     &               'in an attempt to compute slewing time for '// &
     &               'source '//SUR%CAL(SEQ_IND(J15,J11))%J2000_NAME )
                 SUR%STA = SUR_STA_SAVE
                 RETURN 
            END IF
            IF ( K_STA(J15) < SUR%TROPO_MIN_STA ) THEN
                 TIM_SEQ(2,J11) = -2.01
                 GOTO 4110
            END IF
            IF ( SLEW_TIME .LE. 0.0D0           ) THEN
                 TIM_SEQ(2,J11) = -3.01
                 GOTO 4110
            END IF
!
            IER = 0
            CALL SUR_TROPO_SCAN_UPDATE ( SUR, VTD, SUR__TYP_CAL, INT4(SEQ_IND(J15,J11)), &
     &                                   MJD_OBS, TAI_OBS + SLEW_TIME, TROPO_LEN(J15), &
     &                                   FL_STA(1,J15), IER )
            IF ( K_STA(J15) == SUR%L_STA  .AND.  IER .NE. 0 ) THEN
                 SUR%STA = SUR_STA_SAVE
                 IF ( IVRB .GE. 6 ) THEN
                      WRITE ( 6, *  ) 'SUR_ASTRO_TROPO Retry J11= ',  J11, &
     &                                ' IER = ', INT2(IER), ' U_TRY = ', INT2(U_TRY)
                 END IF
!
! -------------- Make a new try
!
                 GOTO 4110
            END IF
            MJD_OBS = SUR%MJD_CUR
            TAI_OBS = SUR%TAI_CUR
            K_STA_SUM = K_STA_SUM + K_STA(J15)
            IF ( J15 .GE. L_RANGE ) GOTO 8150
 4150    CONTINUE 
 8150    CONTINUE 
         IF ( SUR%ALGORITHM == 'ASTROMET_13' ) THEN
              TIM_SEQ(1,J11) = P2I - SUR_GET_ELEV_REF ( SUR, VTD, SUR__TYP_CAL, &
     &                         INT4(SEQ_IND(1,J11)), IER )
            ELSE 
              TIM_SEQ(1,J11) = (MJD_OBS - MJD_0)*86400.0D0 + (TAI_OBS - TAI_0)
         END IF
         IF ( SUR%TROPO_RANGE == 17 ) THEN
              TIM_SEQ(1,J11) = TIM_SEQ(1,J11)*(1.0D0 + 2*SUR%L_STA - K_STA_SUM)
         END IF
!
         TIM_SEQ(2,J11) = J11 + 0.1
         U_TRY = U_TRY + 1
         IF ( J11 .GE. LU_TRY_LOW ) THEN
              IF ( U_TRY > MU_TRY_LOW ) THEN
                   LL_TRY = J11
                   GOTO 8110
              END IF
            ELSE
              IF ( U_TRY > MU_TRY_HIGH ) THEN
                   LL_TRY = J11
                   GOTO 8110
              END IF
         END IF
 4110 CONTINUE 
 8110 CONTINUE 
!
      CALL FOR_QSORT ( TIM_SEQ, LL_TRY, 16, COMPAR_R82 )
!
      CALL SUR_CUR_RESTORE ( SUR, SUR_OBS(1) )
      SUR%OBS_STA = OBS_STA_SAVE 
      MJD_OBS = SUR%MJD_CUR
      TAI_OBS = SUR%TAI_CUR
      IF ( IVRB .GE. 6 ) THEN 
           STR = MJDSEC_TO_DATE ( MJD_OBS, TAI_OBS, -2 )
           WRITE ( 6, * ) ' SUR_ASTRO_TROPO  LL_TRY: ', LL_TRY, ' U_TRY= ', U_TRY,  ' OBS_TAI: ', STR(1:19)
           WRITE ( 6, * ) ' SUR_ASTRO_TROPO  Now: '//GET_CDATE()
           CALL FLUSH ( 6 )
      END IF
!
      DO 4160 J16=1,LL_TRY
         K_TRY = TIM_SEQ(2,J16)
         IF ( K_TRY < 1 ) GOTO 4160
         DO 4170 J17=1,M_RANGE
            IND_CAL = SEQ_IND(J17,K_TRY)
            IF ( J17 == 1  .AND.  FL_FIRST ) THEN
                 SUR%L_SCN = 1
                 SUR%IND_SRC(SUR%L_SCN) = IND_CAL
                 LAST_TYP = SUR__TYP_CAL
               ELSE
                 LAST_TYP = SUR%SRC_TYP(SUR%L_SCN)
            END IF
!
! --------- A special trick to avoid checking for initial azimuth if this
! --------- observation is not the first
!
            L_SCN_SAVE = SUR%L_SCN
            IF ( SUR%L_SCN == 1 .AND. J17 > 1 ) SUR%L_SCN = 2
!
            IF ( EL_MIN_TROPO > P2I ) THEN
                 EL_MIN = EL_RANGE(1,J17)
                 EL_MAX = EL_RANGE(2,J17)
               ELSE
                 EL_MIN = EL_MIN_TROPO
                 EL_MAX = EL_MAX_TROPO
            END IF
            CALL ERR_PASS ( IUER, IER )
            IF ( IVRB == 6  ) IER = -6
            SLEW_TIME = SUR_TROPO_GET_SLEW ( SUR, VTD, MJD_OBS, TAI_OBS + MAX(0.0D0, SUR%PREOBS_LONG), &
     &                                       TROPO_LEN(J17), SUR__TYP_CAL, IND_CAL, &
     &                                       EL_MIN, EL_MAX, SUR__FINE, K_STA(J17), &
     &                                       FL_STA(1,J17), IER )
            SUR%L_SCN = L_SCN_SAVE
            IF ( IER .NE. 0 ) THEN
                 CALL ERR_LOG ( 1795, IUER, 'SUR_ASTRO_TROPO', 'Error '// &
     &               'in an attempt to compute slewing time for source '// &
     &                SUR%CAL(IND_CAL)%J2000_NAME )
                 SUR%STA = SUR_STA_SAVE
                 RETURN
            END IF
!
            IF ( SUR%L_SCN == 1 ) THEN
!
! -------------- Set the tape counter
!
                 SUR%L_TAP = 1
!
                 SUR%MJD_TAPE_START_CUR = SUR%MJD_CUR 
                 SUR%TAI_TAPE_START_CUR = SUR%TAI_CUR 
            END IF
!
            IF ( ( .NOT. FL_FIRST .AND. SLEW_TIME .LE. 0.0D0 ) .OR. &
     &           K_STA(J17) <  SUR%TROPO_MIN_STA                    ) THEN
!
                 CALL SUR_CUR_RESTORE ( SUR, SUR_OBS(1) )
                 IF ( IVRB .GE. 6 ) THEN 
                      STR = MJDSEC_TO_DATE (  SUR%MJD_CUR,  SUR%TAI_CUR, -2 )
                      WRITE ( 6, * ) 'K-RESTORE:  CUR_TAI: ', STR(1:19)
                 END IF
                 IF ( FL_FIRST ) THEN
                      MJD_OBS = SUR%MJD_START
                      TAI_OBS = SUR%TAI_START
                   ELSE
                      MJD_OBS = SUR%MJD_CUR
                      TAI_OBS = SUR%TAI_CUR
                 END IF
                 GOTO 4160
            END IF
!
            IF ( SUR%TAPE_LENGTH > 0.0D0 ) THEN
                 IF ( (MJD_OBS - SUR%MJD_TAPE_START_CUR)*86400.0D0 + &
     &                (TAI_OBS - SUR%TAI_TAPE_START_CUR) + MAX(0.0D0, SUR%PREOBS_LONG) + &
     &                SLEW_TIME + TROPO_LEN(J17) > SUR%TAPE_LENGTH ) THEN
!
! ------------------- If the tape has been immediately changed, set slewing
! ------------------- time to zero, since we assume it was enough time during
! ------------------- tape change
!
                      FL_TAPE_CHANGE  = .TRUE.
                      SLEW_TIME = SUR%TAPE_CHANGE_TIME
                 END IF
            END IF
!
            TAI_OBS = TAI_OBS + SLEW_TIME
            IF ( K_STA(J17) == SUR%L_STA ) THEN
                 CALL ERR_PASS ( IUER, IER )
               ELSE
                 IER= 0
            END IF
            IF ( J17 == 1  .AND.  FL_FIRST ) THEN
                 SUR%L_SCN = 0
            END IF
            IF ( IVRB .GE. 6 ) THEN 
                 STR = MJDSEC_TO_DATE ( MJD_OBS, TAI_OBS, -2 )
                 WRITE ( 6, * ) 'K_TRY: ', J17, ' OBS_TAI: ', STR(1:19), ' Slew= ', SLEW_TIME
            END IF
!
            CALL SUR_TROPO_SCAN_UPDATE ( SUR, VTD, SUR__TYP_CAL, IND_CAL, &
     &                                   MJD_OBS, TAI_OBS, TROPO_LEN(J17), &
     &                                   FL_STA(1,J17), IER )
            SLEW_TIME_SCAN(SUR%L_SCN) = SLEW_TIME
            K_STA(J17) = 0
            DO 4180 J18=1,SUR%L_STA
               IF ( ( .NOT. SUR%STA(J18)%TAGALONE ) .AND. FL_STA(J18,J17) ) THEN
                    K_STA(J17) = K_STA(J17) + 1
               END IF
 4180       CONTINUE
            IF ( K_STA(J17) == SUR%L_STA  .AND.  IER .NE. 0 ) THEN
                 CALL ERR_LOG ( 1796, IUER, 'SUR_ASTRO_TROPO', 'Error in '// &
     &               'an attempt to update slewing time for source '// &
     &               SUR%CAL(IND_CAL)%J2000_NAME )
                 SUR%STA = SUR_STA_SAVE
                 RETURN
            END IF
!
            IF ( J17 == 1 .AND. FL_TAPE_CHANGE ) THEN
!
! -------------- If the tape has been immediately changed, set scan type "tape"
!
                 SUR%SCAN_TYPE(SUR%L_SCN) = SUR__TAPE
               ELSE
                 IF ( SUR%L_SCN > 1 ) THEN
                      SUR%SCAN_TYPE(SUR%L_SCN) = SUR__LONG
                    ELSE 
                      IF ( SUR%PRESES_INTERVAL .LE. 0.0 ) THEN
                           SUR%SCAN_TYPE(SUR%L_SCN) = SUR__LONG
                         ELSE
                           SUR%SCAN_TYPE(SUR%L_SCN) = SUR__FIRST
                      END IF
                 END IF
            END IF
!
            MJD_OBS = SUR%MJD_CUR
            TAI_OBS = SUR%TAI_CUR
!
	    SUR%MJD_TROPO_CUR = SUR%MJD_CUR
	    SUR%TAI_TROPO_CUR = SUR%TAI_CUR
!
            IF ( (SUR%MJD_OBS_END(SUR%L_SCN) - SUR%MJD_STOP)*86400.D0 + &
     &           (SUR%TAI_OBS_END(SUR%L_SCN) - (SUR%TAI_STOP - SUR%POSTSES_INTERVAL) ) > 0.0D0 ) THEN
!
! -------------- End of the session
!
                 SUR%L_SCN = SUR%L_SCN - 1
                 SUR%L_OBS_TAG = SUR%L_OBS_TAG - 1
                 SUR%L_SCN_CAL = SUR%L_SCN_CAL - 1
                 CALL ERR_LOG ( 0, IUER )
                 SUR%STA = SUR_STA_SAVE
                 RETURN
            END IF
            IF ( J17 .GE. L_RANGE ) GOTO 8160
 4170    CONTINUE
         GOTO 8160
 4160 CONTINUE
 8160 CONTINUE
      IF ( SUR%TROPO_RANGE == 7      ) THEN
!
! -------- Adding the 5th calibrator
!
           IF ( IVRB .GE. 3 ) THEN
                WRITE ( 6, * ) ' Searching for the 5th calibrator'
                write ( 6 ,* ) ' sur%l_scn = ', sur%l_scn, ' l_scn_beg = ', l_scn_beg, ' l_range = ', l_range ! %%%%%%%%%%%%%%%%%%%
           END IF
!
! -------- The fifth calibrator that is visible at all stations
!
           MJD_OBS = SUR%MJD_CUR
           TAI_OBS = SUR%TAI_CUR
           SCORE = -1.0D0
           SCORE_MAX = -1.0D0
           IND_CAL = -1
           CALL SUR_CUR_SAVE ( SUR, SUR_OBS(1) )
           DO 4190 J19=1,SUR%L_CAL
              DO 4200 J20=SUR%L_SCN,SUR%L_SCN-3,-1
                 IF ( J20 < 1 ) GOTO 4200
                 IF ( SUR%SRC_TYP(J20) == SUR__TYP_CAL .AND. &
     &                SUR%IND_SRC(J20) == J19                ) THEN
!
! ------------------- If this source observe in this burst of troposphere
! ------------------- calibrators, skip it.
!
                      GOTO 4190
                 END IF
 4200         CONTINUE
              DO 4210 J21=1,SUR%L_STA
                 CALL ERR_PASS ( IUER, IER )
                 CALL SUR_AZEL ( SUR, VTD, SUR__TYP_CAL, MJD_OBS, &
     &                           TAI_OBS, J21, J19, AZ, EL, HA, IER )
                 IF ( IER .NE. 0 ) THEN
                      CALL ERR_LOG ( 1798, IUER, 'SUR_ASTRO_TROPO', 'Error '// &
     &                    'in computing azimuth and elevation for station '// &
     &                     SUR%STA(J21)%NAME//' and sources '// &
     &                     SUR%CAL(J19)%B1950_NAME )
                      SUR%STA = SUR_STA_SAVE
                      RETURN
                 END IF
                 IF ( .NOT. SUR_CHECK_VIS ( SUR, J21, SUR__TYP_CAL, J19, AZ, &
     &                                      EL, HA, IER ) ) THEN
                      GOTO 4190
                 END IF
!
                 IF ( EL < EL_RANGE5_MIN + EL__MARGIN ) GOTO 4190
                 IF ( EL > EL_RANGE5_MAX - EL__MARGIN ) GOTO 4190
 4210         CONTINUE
!
              SLEW_TIME = SUR_TROPO_GET_SLEW ( SUR, VTD, MJD_OBS, &
     &                    TAI_OBS + MAX(0.0D0, SUR%PREOBS_LONG), &
     &                    3.0D0*SUR%TROPO_SCAN_LEN, &
     &                    SUR__TYP_CAL, J19, EL_RANGE5_MIN, EL_RANGE5_MAX, &
     &                    SUR__FINE, K_STA(1), FL_STA(1,1), IER )
              IF ( SLEW_TIME < 1.0D0 ) GOTO 4190
              SCORE(J19) = 1000.0D0/SLEW_TIME
              IF ( SUR%TROPO_RANGE == 13 .AND. J19 == 1 ) THEN
                   SCORE(J19) = 100.0D0* SCORE(J19) 
              END IF
              IF ( SCORE(J19) > SCORE_MAX ) THEN
                   IND_CAL = J19
                   SCORE_MAX = SCORE(J19)
                   SLEW_TIME_MIN = SLEW_TIME
              END IF
 4190      CONTINUE
!
           CALL SUR_CUR_RESTORE ( SUR, SUR_OBS(1) )
           IF ( IND_CAL > 0 ) THEN
                TAI_OBS = TAI_OBS + SLEW_TIME_MIN
                CALL SUR_TROPO_SCAN_UPDATE ( SUR, VTD, SUR__TYP_CAL, IND_CAL, &
     &               MJD_OBS, TAI_OBS, 3.0D0*SUR%TROPO_SCAN_LEN, FL_STA, IER )
                IF ( IVRB .GE. 3 ) THEN
                     WRITE ( 6, * ) ' Added the 5th calibrator'
                END IF
                SLEW_TIME_SCAN(SUR%L_SCN) = SLEW_TIME_MIN 
              ELSE
                IF ( IVRB .GE. 1 ) THEN
                     WRITE ( 6, * ) ' Cannot find the 5th calibrator'
                END IF
           END IF
      END IF ! End of searchin for the 5th calibrator
!
      IF ( SUR%L_SCN > 8  .AND.  SUR%POCAL_STYLE == POCAL_STYLE__GBT_4HR ) THEN
           IUER = -1
           CALL SUR_POCAL_GBT_4HR ( SUR, VTD, IVRB, IUER )
           IF ( IUER .NE. 0 ) THEN
                CALL ERR_LOG ( 1799, IUER, 'SUR_ASTRO_TROPO', 'Error '// &
     &              'in an attempt to reserve time for point calibration' )
                SUR%STA = SUR_STA_SAVE
                RETURN
           END IF
      END IF
!      IF ( SUR%POCAL_STYLE == POCAL_STYLE__KVN ) THEN
!           IUER = -1
!           CALL SUR_POCAL_KVN ( SUR, VTD, IVRB, IUER )
!           IF ( IUER .NE. 0 ) THEN
!                CALL ERR_LOG ( 1800, IUER, 'SUR_ASTRO_TROPO', 'Error '// &
!     &              'in an attempt to reserve time for point calibration' )
!                SUR%STA = SUR_STA_SAVE
!                RETURN
!           END IF
!      END IF
!
      IF ( SUR%L_SCN < L_SCN_BEG  + L_RANGE ) THEN
           write ( 6 ,* ) ' sur%l_scn = ', sur%l_scn, ' l_scn_beg = ', l_scn_beg, ' l_range = ', l_range ! %%%%%%%%%%%%%%%%%%%
           STR = MJDSEC_TO_DATE ( SUR%MJD_CUR, SUR%TAI_CUR, -2 )
           CALL ERR_LOG ( 1801, IUER, 'SUR_ASTRO_TROPO', 'Did not '// &
     &         'find troposphere calibrators for epoch '//STR )
           RETURN 
      END IF
      IF ( IVRB .GE. 3 ) THEN
           STR = MJDSEC_TO_DATE ( MJD_OBS, TAI_OBS, -2 )
           WRITE ( 6, 120 ) SUR%L_SCN, U_TRY, L_TRY, STR(1:19)
 120       FORMAT ( 2X,'Tropo  Scan: ',I4,'  # tries explored: ', &
     &              I8, ' ( ', I8, ' ) ', 2X, A )
           CALL FLUSH ( 6 )
      END IF
!
      IF ( IVRB .GE. 6 ) THEN
           L_SCN_END = SUR%L_SCN
           K1 = 0 
           DO 4220 J22=L_SCN_BEG+1,L_SCN_END
              WRITE ( 6, * ) ' '
              SUR%L_SCN = J22
              K1 = K1 + 1
              IF ( SUR%L_SCN > 1 ) THEN
                   SUR%MJD_CUR = SUR%MJD_OBS_BEG(SUR%L_SCN-1)
                   SUR%TAI_CUR = SUR%TAI_OBS_BEG(SUR%L_SCN-1)
                   DO 5230 J23=1,SUR%L_STA
                      SUR%STA(J23)%EL_CUR     = SUR%EL_OBS(J23,SUR%L_SCN-1)
                      SUR%STA(J23)%HA_CUR     = SUR%HA_OBS(J23,SUR%L_SCN-1)
                      SUR%STA(J23)%AZ_CUR     = SUR%AZ_OBS(J23,SUR%L_SCN-1)
                      SUR%STA(J23)%AZ_ACC_CUR = SUR%AZ_ACC_OBS(J23,SUR%L_SCN-1)
                      SUR%STA(J23)%HA_ACC_CUR = SUR%HA_ACC_OBS(J23,SUR%L_SCN-1)
 5230              CONTINUE 
              END IF
              IER = 0
              SLEW_TIME = SUR_TROPO_GET_SLEW ( SUR, VTD, MJD_OBS, TAI_OBS + MAX(0.0D0, SUR%PREOBS_LONG), &
     &                                         TROPO_LEN(K1), SUR__TYP_CAL, &
     &                                         SUR%IND_SRC(SUR%L_SCN), EL_MIN_RAN, P2I, &
     &                                         SUR__FINE, K_STA(K1), FL_STA(1,K1), &
     &                                         IER )
              CALL SUR_SLEW_REPORT ( SUR )
              WRITE ( 6, '("Scan ", I4, "  Slew_time_max: ", F6.1)' ) SUR%L_SCN, SLEW_TIME_SCAN(J22)
 4220      CONTINUE
           WRITE ( 6, * ) ' '
           SUR%L_SCN = L_SCN_END
      END IF
!
! --- Restore station status, in particular status "used"
!
      SUR%STA = SUR_STA_SAVE
!
      CALL ERR_PASS ( IUER, IER )
      CALL SUR_SET_CUR ( SUR, VTD, IER  )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 1802, IUER, 'SUR_ASTRO_TROPO', 'Error '// &
     &         'in an attempt to update the current scan az, el, ha' )
           RETURN
      END IF
      IF ( IVRB .GE. 5 ) THEN
           TC = CPU_TIMER  ( %VAL(2) ) 
           TW = WALL_TIMER ( %VAL(2) ) 
           WRITE ( 6, 220 ) TC, TW, SUR%L_SCN
 220       FORMAT ( '  SUR_ASTRO_TROPO  CPU_Time: ', F9.2, ' WALL_Time: ',F9.2, ' Scan: ', I4 )
           CALL FLUSH ( 6 ) 
      END IF
!
      DEALLOCATE ( SEQ_IND )
      DEALLOCATE ( TIM_SEQ )
      DEALLOCATE ( FL_VIS_CAL )
      IF ( IVRB .GE. 4 ) THEN 
           STR = MJDSEC_TO_DATE ( SUR%MJD_CUR, SUR%TAI_CUR, -2 )
           WRITE ( 6, * ) ' END of sur_astro_tropo  DAT_CUR: ', STR(1:24), &
     &                   ' L_SCN = ', INT2(SUR%L_SCN) 
           CALL FLUSH ( 6 )
      END IF
!
      IF ( SUR%TROPO_RANGE == 21 ) THEN
           SUR_SAVED = SUR
           SUR%ALGORITHM = 'GEODETIC_01'
           SUR%TROPO_BURST_INTERVAL = 0.0D0
           SUR%TAI_STOP  = SUR%TAI_CUR + DUR_BLOCK21
           SUR%MJD_STOP  = SUR%MJD_CUR
           IF ( SUR%TAI_STOP > 86400.0D0 ) THEN
                SUR%TAI_STOP = SUR%TAI_STOP - 86400.0D0
                SUR%MJD_STOP = SUR%MJD_STOP +  1
           END IF 
!
           DO 4230 J23=1,SUR%L_SOU
              SUR%SOU(J23)%PRI = 0.0
              DO 4240 J24=1,SUR%L_SO2
                 IF ( SUR%SO2(J24)%J2000_NAME == SUR%SOU(J23)%J2000_NAME ) THEN
                      SUR%SOU(J23)%PRI = SUR%SO2(J24)%PRI 
                 END IF
 4240         CONTINUE 
 4230      CONTINUE 
!
           IF ( IVRB .GE. 6 ) THEN
                STR1 = MJDSEC_TO_DATE ( SUR%MJD_CUR,  SUR%TAI_CUR,   -2 )
                STR2 = MJDSEC_TO_DATE ( SUR%MJD_STOP, SUR%TAI_STOP, -2 )
                WRITE ( 6, * ) 'Started geodetic block since '//STR1(1:19)//' through '//STR2(1:19)
                CALL FLUSH ( 6 )
           END IF
           CALL ERR_PASS ( IUER, IER )
!?? recursion ?? it crashes on deva          CALL SUR_ASTRO_SEQ ( SUR, VTD, IVRB, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 1803, IUER, 'SUR_ASTRO_TROPO', 'Error in attempt '// &
     &              'to find an optimal sequence of observation for the '// &
     &              'geodetic block' )
                RETURN
           END IF
           SUR%ALGORITHM = SUR_SAVED%ALGORITHM 
           SUR%TAI_STOP  = SUR_SAVED%TAI_STOP  
           SUR%MJD_STOP  = SUR_SAVED%MJD_STOP
           SUR%SOU       = SUR_SAVED%SOU       
           SUR%TROPO_BURST_INTERVAL = SUR_SAVED%TROPO_BURST_INTERVAL 
           IF ( IVRB .GE. 6 ) THEN
                STR1 = MJDSEC_TO_DATE ( SUR%MJD_CUR, SUR%TAI_CUR,   -2 )
                WRITE ( 6, * ) 'Ended geodetic block '//STR1(1:19)
           END IF
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE   SUR_ASTRO_TROPO  !#!#
!
! ------------------------------------------------------------------------
!
      FUNCTION   SUR_TROPO_GET_SLEW ( SUR, VTD, MJD_OBS, TAI_OBS, OBS_LEN, &
     &                                I_TYP, IND_SRC, &
     &                                EL_MIN, EL_MAX, PROC_CODE, K_STA, &
     &                                FL_STA, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  SUR_TROPO_GET_SLEW
! *                                                                      *
! * ## 15-JAN-2007  SUR_TROPO_GET_SLEW v1.1 (c) L. Petrov 19-JAN-2008 ## *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'astro_constants.i'
      INCLUDE   'sur_sked.i'
      INCLUDE   'vtd.i'
      INCLUDE   'getpar.i'
      TYPE     ( SUR__TYPE ) :: SUR
      TYPE     ( VTD__TYPE ) :: VTD
      REAL*8     SUR_TROPO_GET_SLEW
      INTEGER*4  MJD_OBS, I_TYP, IND_SRC, PROC_CODE, K_STA, IUER
      LOGICAL*1  FL_STA(SUR__M_STA)
      REAL*8     TAI_OBS, OBS_LEN, EL_MIN, EL_MAX
      REAL*8     AZ, EL, HA, AZ_BEG, EL_BEG, HA_BEG, AZ_END, EL_END, HA_END, &
     &           SLEW_EL, SLEW_AZ, DIF_AZ, SLEW_STA(SUR__M_STA)
      REAL*8     AZ__MARGIN, EL__MARGIN
      PARAMETER  ( AZ__MARGIN = 0.5D0*DEG__TO__RAD )
      PARAMETER  ( EL__MARGIN = 1.0D0*DEG__TO__RAD )
      INTEGER*4  M_ITER
      PARAMETER  ( M_ITER = 3 )
      INTEGER*4  J1, J2, J3, IND_SLEW, IER
      CHARACTER, EXTERNAL :: MJDSEC_TO_DATE*30
      LOGICAL*1, EXTERNAL :: SUR_CHECK_VIS
      REAL*8,    EXTERNAL :: SUR_SLEW_TIME
      INTEGER*4, EXTERNAL :: MAX_LIST_R8
!
      SUR_TROPO_GET_SLEW = 0.0D0
      K_STA = 0
      IF ( SUR%L_SCN > 0 ) THEN
           DO 410 J1=1,SUR%L_STA
              IF ( SUR%STA(J1)%TAGALONE ) GOTO 410
              CALL ERR_PASS ( IUER, IER )
              IF ( IUER < -6 ) IER = -1
              SLEW_STA(J1) = SUR_SLEW_TIME ( SUR, VTD, I_TYP, IND_SRC, &
     &                                       SUR%IND_SRC(SUR%L_SCN), &
     &                                       SUR%SRC_TYP(SUR%L_SCN), &
     &                                       J1, PROC_CODE, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 1811, IUER, 'SUR_TROPO_GET_SLEW', 'Error in '// &
     &                 'computing slew time' )
                   SUR_TROPO_GET_SLEW = -21.0D0
                   RETURN
              END IF
!
              IF ( SLEW_STA(J1) .LE. 0.0D0 ) THEN
                   FL_STA(J1) = .FALSE.
                 ELSE
                   SUR_TROPO_GET_SLEW = MAX ( SUR_TROPO_GET_SLEW, SLEW_STA(J1) )
                   FL_STA(J1) = .TRUE.
                   K_STA = K_STA + 1
              END IF
              IF ( IUER == -6 ) THEN
!
! ---------------- Print slewing time
!
                   WRITE ( 6, * ) 'SUR_TROPO_GET_SLEW  L_SCA: ', INT2(SUR%L_SCN), &
     &                            ' Sta: ', SUR%STA(J1)%NAME, ' SRC: ', SUR%CAL(IND_SRC)%J2000_NAME, &
     &                            ' SLEW_STA = ', SNGL(SLEW_STA(J1))
              END IF
 410       CONTINUE
         ELSE 
           SUR_TROPO_GET_SLEW = 0.001D0
           FL_STA = .TRUE.
      END IF
!
      K_STA = 0
      IF ( SUR%L_SCN > 0  .OR.  PROC_CODE == SUR__FINE ) THEN
           DO 420 J2=1,SUR%L_STA
              IF ( .NOT. FL_STA(J2) ) GOTO 420
              FL_STA(J2) = .FALSE.
              IF ( SUR%STA(J2)%TAGALONE ) GOTO 420
!
! ----------- Compute for each station the azimuth and elevation at the 
! ----------- start of the observation
!
              IF ( IUER < 0 ) IER = -1
              CALL SUR_AZEL ( SUR, VTD, I_TYP, MJD_OBS, &
     &                        TAI_OBS + SUR_TROPO_GET_SLEW, &
     &                        J2, IND_SRC, AZ_BEG, EL_BEG, HA_BEG, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 1812, IUER, 'SUR_TROPO_GET_SLEW', 'Error in '// &
     &                 'computing azimuth and elevation for station '// &
     &                  SUR%STA(J2)%NAME )
                   SUR_TROPO_GET_SLEW = -1.0D0
                   RETURN
              END IF
!
! ----------- Compute for each station the azimuth and elevation at the end of
! ----------- the observation
!
              IF ( IUER < 0 ) IER = -1
              CALL SUR_AZEL ( SUR, VTD, I_TYP, MJD_OBS, &
     &                        TAI_OBS + SUR_TROPO_GET_SLEW + OBS_LEN, &
     &                        J2, IND_SRC, AZ_END, EL_END, HA_END, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 1813, IUER, 'SUR_TROPO_GET_SLEW', 'Error in '// &
     &                 'computing azimuth and elevation for station '// &
     &                  SUR%STA(J2)%NAME )
                   SUR_TROPO_GET_SLEW = -1.0D0
                   RETURN
              END IF
!
              IF ( EL_BEG < EL_MIN + EL__MARGIN .OR. &
     &             EL_BEG > EL_MAX - EL__MARGIN      ) THEN
!
                   FL_STA(J2) = .FALSE.
                   GOTO 420
              END IF
              IF ( EL_END < EL_MIN + EL__MARGIN .OR. &
     &             EL_END > EL_MAX - EL__MARGIN      ) THEN
!
                   FL_STA(J2) = .FALSE.
                   GOTO 420
              END IF
!
              IF ( .NOT. SUR_CHECK_VIS ( SUR, J2, SUR__TYP_CAL, IND_SRC, &
     &                                   AZ_BEG, EL_BEG, HA_BEG, IER ) ) THEN
!
! ---------------- Does not see?
!
                   FL_STA(J2) = .FALSE.
                   GOTO 420
              END IF
              IF ( .NOT. SUR_CHECK_VIS ( SUR, J2, SUR__TYP_CAL, IND_SRC, &
     &                                   AZ_END, EL_END, HA_END, IER ) ) THEN
!
! ---------------- Does not see?
!
                   FL_STA(J2) = .FALSE.
                   GOTO 420
              END IF
              IF ( SUR%L_SCN == 0 ) THEN
!
! ---------------- At VLBA the first scan should start with azimuth
! ---------------- in range [90.0, 270] to ensure that the azimuth of the
! ---------------- cable wrap is in a predictable position
!
                   IF ( SUR%STA(J2)%NAME == 'BR-VLBA ' .OR. &
     &                  SUR%STA(J2)%NAME == 'FD-VLBA ' .OR. &
     &                  SUR%STA(J2)%NAME == 'HN-VLBA ' .OR. &
     &                  SUR%STA(J2)%NAME == 'KP-VLBA ' .OR. &
     &                  SUR%STA(J2)%NAME == 'LA-VLBA ' .OR. &
     &                  SUR%STA(J2)%NAME == 'MK-VLBA ' .OR. &
     &                  SUR%STA(J2)%NAME == 'NL-VLBA ' .OR. &
     &                  SUR%STA(J2)%NAME == 'OV-VLBA ' .OR. &
     &                  SUR%STA(J2)%NAME == 'PIETOWN ' .OR. &
     &                  SUR%STA(J2)%NAME == 'SC-VLBA ' .OR. &
     &                  SUR%STA(J2)%NAME == 'GBT-VLBA'      ) THEN
                        IF ( AZ_BEG < 0.5D0*PI__NUM + AZ__MARGIN .OR. &
     &                       AZ_BEG > 1.5D0*PI__NUM - AZ__MARGIN .OR. &
     &                       AZ_END < 0.5D0*PI__NUM + AZ__MARGIN .OR. &
     &                       AZ_END > 1.5D0*PI__NUM - AZ__MARGIN      ) THEN
!
                             FL_STA(J2) = .FALSE.
                             GOTO 420
                        END IF
                   END IF
              END IF
!
              FL_STA(J2) = .TRUE.
              K_STA = K_STA + 1
 420       CONTINUE
      END IF
!
      IF ( K_STA < SUR%L_STA  .AND.  K_STA < SUR%TROPO_MIN_STA ) THEN
           SUR_TROPO_GET_SLEW = -22.0D0
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  FUNCTION   SUR_TROPO_GET_SLEW  !#!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE SUR_TROPO_SCAN_UPDATE ( SUR, VTD, I_TYP, IND_SRC, MJD_OBS, &
     &                                   TAI_OBS, TROPO_LEN, FL_STA, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine SUR_TROPO_SCAN_UPDATE
! *                                                                      *
! * # 16-JAN-2007 SUR_TROPO_SCAN_UPDATE v1.3 (c) L. Petrov 22-JAN-2018 # *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'astro_constants.i'
      INCLUDE   'sur_sked.i'
      INCLUDE   'vtd.i'
      INCLUDE   'getpar.i'
      TYPE     ( SUR__TYPE ) :: SUR
      TYPE     ( VTD__TYPE ) :: VTD
      INTEGER*4  MJD_OBS, I_TYP, IND_SRC, IUER
      REAL*8     TAI_OBS, TROPO_LEN, EL_MIN, EL_MAX
      REAL*8     AZ, EL, HA, A, B, ALP, DEL, &
     &           SLEW_EL, SLEW_AZ, SLEW_DEL, SLEW_HA, SLEW_A, SLEW_B, &
     &           DIF_EL,  DIF_AZ, DIF_DEL, DIF_HA, DIF_A, DIF_B, &
     &           TIM_DIF, SLEW_STA(SUR__M_STA), A_LAST, B_LAST
      CHARACTER  STR*128
      LOGICAL*1  FL_STA(SUR__M_STA)
      INTEGER*4  J1, J2, J3, IND_SLEW, UTC_OBS_INT, SPL_STATUS, IER
      CHARACTER, EXTERNAL :: MJDSEC_TO_DATE*30
      LOGICAL*1, EXTERNAL :: SUR_CHECK_VIS
      INTEGER*4, EXTERNAL :: MAX_LIST_R8
!
! --- Compute final azimuth and elevation of the observation, since
! --- now we know the time when it will happen
!
! --- First, rounding
!
      UTC_OBS_INT = IDNINT ( TAI_OBS + SUR%UTC_M_TAI + 0.5000001D0 )
      IF ( MOD(UTC_OBS_INT ,IDNINT(SUR%START_ROUNDING)) .NE. 0 ) THEN
           TAI_OBS = (UTC_OBS_INT/IDNINT(SUR%START_ROUNDING)+1)*IDNINT(SUR%START_ROUNDING) &
     &               - SUR%UTC_M_TAI
          ELSE
           TAI_OBS = UTC_OBS_INT - SUR%UTC_M_TAI
      END IF
      IF ( TAI_OBS > 86400.0D0 ) THEN
           TAI_OBS = TAI_OBS - 86400.0D0
           MJD_OBS = MJD_OBS + 1
      END IF
!
      DO 410 J1=1,SUR%L_STA
         IF ( .NOT. FL_STA(J1) ) GOTO 410
!
         CALL ERR_PASS ( IUER, IER )
         SPL_STATUS = SUR%STATUS_SPL(I_TYP)
         SUR%STATUS_SPL(I_TYP) = 0
         CALL SUR_AZEL ( SUR, VTD, I_TYP, MJD_OBS, TAI_OBS, &
     &                   J1, IND_SRC, AZ, EL, HA, IER )
         SUR%STATUS_SPL(I_TYP) = SPL_STATUS 
         IF ( SUR%STA(J1)%MOUNT_TYPE == MT__ALTAZ ) THEN
!
! ----------- Update accumulative aziumth in order to take into account
! ----------- cable wrap
!
              IF ( SUR%STA(J1)%AZ_ACC_CUR < -100.0D0 ) THEN
!
! ---------------- This means that this was the first scan
!
                   SUR%STA(J1)%AZ_CUR     = AZ
                   SUR%STA(J1)%AZ_ACC_CUR = AZ
                   SUR%STA(J1)%HA_ACC_CUR = HA
              END IF
              DIF_AZ = (AZ - SUR%STA(J1)%AZ_ACC_CUR)
              DIF_AZ = DIF_AZ - PI2*IDNINT(DIF_AZ/PI2)
              DIF_EL = DABS(EL - SUR%STA(J1)%EL_CUR)
              IF ( DIF_AZ > 0.0D0 ) THEN
!
! ---------------- The shortest move is clock-wise
!
                   IF ( SUR%STA(J1)%AZ_ACC_CUR + DIF_AZ < SUR%STA(J1)%AZ_ACC_MAX ) THEN
                        SUR%STA(J1)%AZ_ACC_CUR = SUR%STA(J1)%AZ_ACC_CUR + DIF_AZ
                     ELSE
!
! --------------------- The shortest way is not possible, move the longest way
! --------------------- counter-clock-wise
!
                        SUR%STA(J1)%AZ_ACC_CUR = SUR%STA(J1)%AZ_ACC_CUR + (DIF_AZ - PI2)
                        SUR%STA(J1)%HA_ACC_CUR = HA
                   END IF
                ELSE ! if ( DIF_AZ .LE. 0.0D0 ) then
!
! ---------------- The shortest move is counter-clock-wise
!
                   IF ( SUR%STA(J1)%AZ_ACC_CUR + DIF_AZ > SUR%STA(J1)%AZ_ACC_MIN ) THEN
                        SUR%STA(J1)%AZ_ACC_CUR = SUR%STA(J1)%AZ_ACC_CUR + DIF_AZ
                        SUR%STA(J1)%HA_ACC_CUR = HA
                     ELSE
!
! --------------------- The shortest way is not possible, move the longest way
! --------------------- clock-wise
!
                        SUR%STA(J1)%AZ_ACC_CUR = SUR%STA(J1)%AZ_ACC_CUR + &
     &                                           (DIF_AZ + PI2)
                        SUR%STA(J1)%HA_ACC_CUR = HA
                  END IF
              END IF
!
              IF ( EL < SUR%STA(J1)%EL_MIN .OR. &
     &             EL > SUR%STA(J1)%EL_MAX      ) THEN
!
                   WRITE ( 6, * ) 'K1 Trap of internal control: station '// &
     &                              SUR%STA(J1)%NAME//' Epoch: '// &
     &                              MJDSEC_TO_DATE ( MJD_OBS, TAI_OBS, -2 )// &
     &                            ' Elevation angle: ', EL/DEG__TO__RAD, &
     &                            ' Beyond elevataion range ', &
     &                            SUR%STA(J1)%EL_MIN/DEG__TO__RAD, &
     &                            SUR%STA(J1)%EL_MAX/DEG__TO__RAD
                   CALL FLUSH ( 6 )
                   CALL ERR_LOG ( 1721, IUER, 'SUR_TROPO_SCAN_UPDATE', &
     &                 'Trap of internal control' )
                   RETURN
               END IF
!
               IF ( DABS(DIF_AZ) > SUR%STA(J1)%SLEW_RATE_AZ**2/SUR%STA(J1)%SLEW_ACCL_AZ ) THEN
                    SLEW_AZ = (DABS(DIF_AZ) - SUR%STA(J1)%SLEW_RATE_AZ**2/SUR%STA(J1)%SLEW_ACCL_AZ)/SUR%STA(J1)%SLEW_RATE_AZ + &
     &                        2.0D0*SUR%STA(J1)%SLEW_RATE_AZ/SUR%STA(J1)%SLEW_ACCL_AZ
                   ELSE
                     SLEW_AZ = 2.D0*DSQRT(DABS(DIF_AZ)/SUR%STA(J1)%SLEW_ACCL_AZ)
               END IF
               IF ( DABS(DIF_EL) > SUR%STA(J1)%SLEW_RATE_EL**2/SUR%STA(J1)%SLEW_ACCL_EL ) THEN
                    SLEW_EL = DABS(DIF_EL)/SUR%STA(J1)%SLEW_RATE_EL + &
     &                        SUR%STA(J1)%SLEW_RATE_EL/SUR%STA(J1)%SLEW_ACCL_EL
                  ELSE
                    SLEW_EL = 2.D0*DSQRT(DABS(DIF_EL)/SUR%STA(J1)%SLEW_ACCL_EL)
               END IF
               SUR%SLEW_DUR(J1,SUR%L_SCN+1) = MAX(SLEW_AZ+SUR%STA(J1)%TIME_SETTLE_AZ, &
     &                                            SLEW_EL+SUR%STA(J1)%TIME_SETTLE_EL) + &
     &                                        SUR%STA(J1)%POSTOB
               SUR%STA(J1)%HA_ACC_CUR = HA
             ELSE IF ( SUR%STA(J1)%MOUNT_TYPE == MT__EQUAT ) THEN
               IF ( SUR%STA(J1)%HA_ACC_CUR < -100.0D0 ) THEN
!
! ----------------- This means that this was the first scan
!
                    SUR%STA(J1)%HA_ACC_CUR = HA
                    SUR%STA(J1)%HA_CUR = HA
                  ELSE
                    IF ( SUR%SRC_TYP(SUR%L_SCN) == SUR__TYP_TAG ) THEN
                         ALP = SUR%SOU(SUR%IND_SRC(SUR%L_SCN))%ALPHA
                         DEL = SUR%SOU(SUR%IND_SRC(SUR%L_SCN))%DELTA
                      ELSE IF ( SUR%SRC_TYP(SUR%L_SCN) == SUR__TYP_SEC ) THEN
                         ALP = SUR%SO2(SUR%IND_SRC(SUR%L_SCN))%ALPHA
                         DEL = SUR%SO2(SUR%IND_SRC(SUR%L_SCN))%DELTA
                      ELSE IF ( SUR%SRC_TYP(SUR%L_SCN) == SUR__TYP_CAL ) THEN
                         ALP = SUR%CAL(SUR%IND_SRC(SUR%L_SCN))%ALPHA
                         DEL = SUR%CAL(SUR%IND_SRC(SUR%L_SCN))%DELTA
                      ELSE IF ( SUR%SRC_TYP(SUR%L_SCN) == SUR__TYP_POC ) THEN
                         ALP = SUR%SOP(SUR%IND_SRC(SUR%L_SCN))%ALPHA
                         DEL = SUR%SOP(SUR%IND_SRC(SUR%L_SCN))%DELTA
                      ELSE IF ( SUR%SRC_TYP(SUR%L_SCN) == SUR__TYP_CAL ) THEN
                         ALP = SUR%PLA(SUR%IND_SRC(SUR%L_SCN))%ALPHA
                         DEL = SUR%PLA(SUR%IND_SRC(SUR%L_SCN))%DELTA
                    END IF
                    DIF_DEL = DEL - SUR%STA(J1)%DEL_CUR
               END IF
               DIF_HA = (HA - SUR%STA(J1)%HA_ACC_CUR)
               IF ( DABS(DIF_HA) < P2I ) THEN
!
! ----------------- The shortest move is clock-wise
!
                    SUR%STA(J1)%HA_ACC_CUR = SUR%STA(J1)%HA_ACC_CUR + DIF_HA
                  ELSE
!
! ----------------- The shortest move is counter-clock-wise
!
                    IF ( SUR%STA(J1)%HA_ACC_CUR + DIF_HA > SUR%STA(J1)%AZ_ACC_MIN ) THEN
                         SUR%STA(J1)%HA_ACC_CUR = SUR%STA(J1)%HA_ACC_CUR + DIF_HA
                       ELSE
!
! ---------------------- The shortest way is not possible, move the longest way
! ---------------------- clock-wise
!
                         SUR%STA(J1)%HA_ACC_CUR = SUR%STA(J1)%HA_ACC_CUR + &
     &                                            (DIF_HA + PI2)
                    END IF
               END IF
!
! ------------ Compute the slew time
!
               IF ( DABS(DIF_HA) > SUR%STA(J1)%SLEW_RATE_AZ**2/SUR%STA(J1)%SLEW_ACCL_AZ ) THEN
                    SLEW_HA = (DABS(DIF_HA) - SUR%STA(J1)%SLEW_RATE_AZ**2/SUR%STA(J1)%SLEW_ACCL_AZ)/SUR%STA(J1)%SLEW_RATE_AZ + &
     &                        2.0D0*SUR%STA(J1)%SLEW_RATE_AZ/SUR%STA(J1)%SLEW_ACCL_AZ
                  ELSE
                    SLEW_HA = 2.D0*DSQRT(DABS(DIF_HA)/SUR%STA(J1)%SLEW_ACCL_AZ)
               END IF
               IF ( DABS(DIF_DEL) > SUR%STA(J1)%SLEW_RATE_EL**2/SUR%STA(J1)%SLEW_ACCL_EL ) THEN
                    SLEW_DEL = DABS(DIF_DEL)/SUR%STA(J1)%SLEW_RATE_EL + &
     &                        SUR%STA(J1)%SLEW_RATE_EL/SUR%STA(J1)%SLEW_ACCL_EL
                  ELSE
                    SLEW_DEL = 2.D0*DSQRT(DABS(DIF_DEL)/SUR%STA(J1)%SLEW_ACCL_EL)
               END IF
               SUR%SLEW_DUR(J1,SUR%L_SCN+1) = MAX(SLEW_HA+SUR%STA(J1)%TIME_SETTLE_AZ, &
     &                                            SLEW_DEL+SUR%STA(J1)%TIME_SETTLE_EL) + &
     &                                        SUR%STA(J1)%POSTOB
               SUR%STA(J1)%AZ_ACC_CUR = AZ
             ELSE IF ( SUR%STA(J1)%MOUNT_TYPE == MT__XY_E ) THEN
!
! ------------ XY-E mounting
!
               IF ( DABS(DTAN(SUR%STA(J1)%EL_CUR)) < 1.D-6 ) THEN
                    A_LAST = P2I
                  ELSE
                    A_LAST = DATAN ( DCOS(SUR%STA(J1)%AZ_CUR)/DTAN(SUR%STA(J1)%EL_CUR) )
                    B_LAST = DASIN ( DSIN(SUR%STA(J1)%AZ_CUR)*DCOS(SUR%STA(J1)%EL_CUR) )
               END IF
!
               IF ( DABS(DTAN(EL)) < 1.D-6 ) THEN
                    A = P2I
                  ELSE
                    A = DATAN ( DCOS(AZ)/DTAN(EL) )
                    B = DASIN ( DSIN(AZ)*DCOS(EL) )
               END IF
!
               DIF_A = A - A_LAST
               DIF_B = B - B_LAST
!
               SLEW_A = DABS(DIF_A)/SUR%STA(J1)%SLEW_RATE_AZ + &
     &                   SUR%STA(J1)%SLEW_RATE_AZ/SUR%STA(J1)%SLEW_ACCL_AZ
               SLEW_B = DABS(DIF_B)/SUR%STA(J1)%SLEW_RATE_EL + &
     &                   SUR%STA(J1)%SLEW_RATE_EL/SUR%STA(J1)%SLEW_ACCL_EL
               SUR%SLEW_DUR(J1,SUR%L_SCN+1) = MAX(SLEW_A+SUR%STA(J1)%TIME_SETTLE_AZ, &
     &                                            SLEW_B+SUR%STA(J1)%TIME_SETTLE_EL) + &
     &                                        SUR%STA(J1)%POSTOB
               SUR%STA(J1)%AZ_ACC_CUR = AZ
               SUR%STA(J1)%HA_ACC_CUR = HA
         END IF
!
! ------ Now, let us perform a more stringent check
!
         CALL ERR_PASS ( IUER, IER )
         IF ( .NOT. SUR_CHECK_VIS ( SUR, J1, I_TYP, IND_SRC, AZ, EL, HA, IER ) ) THEN
!
! ----------- Does not see?
!
              STR = MJDSEC_TO_DATE ( MJD_OBS, TAI_OBS, -2 )
              WRITE ( 6, * ) 'K2 Trap of internal control: station '// &
     &                         SUR%STA(J1)%NAME//' Epoch: '//STR(1:19)// &
     &                       ' Elevation angle: ', SNGL(EL/DEG__TO__RAD), &
     &                       ' Azimuth: ', SNGL(AZ/DEG__TO__RAD), &
     &                       ' Do not see!'
              CALL FLUSH ( 6 )
              CALL ERR_LOG ( 1722, IUER, 'SUR_TROPO_SCAN_UPDATE', 'Trap of '// &
     &            'internal control' )
              RETURN
         END IF
!
         SUR%STA(J1)%AZ_CUR = AZ
         SUR%STA(J1)%EL_CUR = EL
         SUR%STA(J1)%HA_CUR = HA
         SUR%EL_OBS(J1,SUR%L_SCN+1) = SUR%STA(J1)%EL_CUR
         SUR%AZ_OBS(J1,SUR%L_SCN+1) = SUR%STA(J1)%AZ_CUR
         SUR%HA_OBS(J1,SUR%L_SCN+1) = SUR%STA(J1)%HA_CUR
         SUR%AZ_ACC_OBS(J1,SUR%L_SCN+1) = SUR%STA(J1)%AZ_ACC_CUR
         SUR%HA_ACC_OBS(J1,SUR%L_SCN+1) = SUR%STA(J1)%HA_ACC_CUR
 410  CONTINUE
!
! --- Update scan counter
!
      IF ( I_TYP == SUR__TYP_CAL ) THEN
           SUR%L_SCN_CAL = SUR%L_SCN_CAL + 1
         ELSE IF ( I_TYP == SUR__TYP_POC ) THEN
           SUR%L_SCN_POC = SUR%L_SCN_POC + 1
         ELSE IF ( I_TYP == SUR__TYP_PLA ) THEN
           SUR%L_SCN_PLA = SUR%L_SCN_PLA + 1
      END IF
      SUR%L_SCN     = SUR%L_SCN + 1
!
! --- Check: is it time to change the tape?
!
      IF ( (MJD_OBS - SUR%MJD_TAPE_START_CUR)*86400.0D0 + &
     &     (TAI_OBS - SUR%TAI_TAPE_START_CUR) + MAX(0.0D0, SUR%PREOBS_LONG) + &
     &     TROPO_LEN > SUR%TAPE_LENGTH ) THEN
!
! -------- Yes, it is just time. 
!
! -------- Increment the tape counter
!
           SUR%L_TAP = SUR%L_TAP + 1
!
! -------- Set the tape start date
!
           SUR%MJD_TAPE_START_CUR = MJD_OBS 
           SUR%TAI_TAPE_START_CUR = TAI_OBS 
           SUR%SCAN_TYPE(SUR%L_SCN) = SUR__TAPE
         ELSE 
           SUR%SCAN_TYPE(SUR%L_SCN) = SUR__LONG
      END IF
!
      SUR%IND_SRC(SUR%L_SCN) = IND_SRC
      SUR%SRC_TYP(SUR%L_SCN) = I_TYP
      SUR%IND_TAP(SUR%L_SCN) = SUR%L_TAP
      SUR%MJD_OBS_BEG(SUR%L_SCN) = MJD_OBS
      SUR%MJD_OBS_END(SUR%L_SCN) = MJD_OBS
      IF ( I_TYP == SUR__TYP_POC .OR. I_TYP == SUR__TYP_PLA  ) THEN
           SUR%TAI_OBS_BEG(SUR%L_SCN) = TAI_OBS 
           SUR%TAI_OBS_END(SUR%L_SCN) = TAI_OBS + TROPO_LEN 
         ELSE 
           SUR%TAI_OBS_BEG(SUR%L_SCN) = TAI_OBS + MAX(0.0D0, SUR%PREOBS_LONG)
           SUR%TAI_OBS_END(SUR%L_SCN) = TAI_OBS + MAX(0.0D0, SUR%PREOBS_LONG) + TROPO_LEN 
      END IF
      IF ( SUR%TAI_OBS_END(SUR%L_SCN) > 86400.0D0 ) THEN
           SUR%TAI_OBS_END(SUR%L_SCN) = SUR%TAI_OBS_END(SUR%L_SCN) - 86400.0D0
           SUR%MJD_OBS_END(SUR%L_SCN) = SUR%MJD_OBS_END(SUR%L_SCN) + 1
      END IF
      SUR%MJD_CUR = SUR%MJD_OBS_END(SUR%L_SCN)
      SUR%TAI_CUR = SUR%TAI_OBS_END(SUR%L_SCN)
      DO 420 J2=1,SUR%L_STA
         IF ( FL_STA(J2) ) THEN
              SUR%OBS_STA(J2,SUR%L_SCN) = SUR__USED
            ELSE
              SUR%OBS_STA(J2,SUR%L_SCN) = SUR__UND
         END IF
         IF ( SUR%L_SCN > 1 ) THEN
              DO 430 J3=SUR%L_SCN-1,1,-1
                 IF ( SUR%OBS_STA(J2,J3) == SUR__USED ) THEN
                      SUR%SCA_PREV(J2,SUR%L_SCN) = J3
                      GOTO 830
                 END IF
 430          CONTINUE 
 830          CONTINUE 
        END IF
 420  CONTINUE
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  SUR_TROPO_SCAN_UPDATE  !#!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE SUR_CUR_SAVE ( SUR, SUR_OBS )
! ************************************************************************
! *                                                                      *
! *   Routine SUR_CUR_SAVE
! *                                                                      *
! *  ### 17-JAN-2007  SUR_CUR_SAVE  v1.0 (c)  L. Petrov 17-JAN-2007 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'astro_constants.i'
      INCLUDE   'sur_sked.i'
      INCLUDE   'vtd.i'
      TYPE     ( SUR__TYPE     ) :: SUR
      TYPE     ( SUR__OBS_TYPE ) :: SUR_OBS
      INTEGER*4  J1
!
      SUR_OBS%L_OBS_TAG = SUR%L_OBS_TAG
      SUR_OBS%L_SCN     = SUR%L_SCN
      SUR_OBS%L_SCN_CAL = SUR%L_SCN_CAL
      SUR_OBS%MJD_CUR   = SUR%MJD_CUR
      SUR_OBS%TAI_CUR   = SUR%TAI_CUR
      SUR_OBS%L_TAP     = SUR%L_TAP 
      SUR_OBS%MJD_TAPE_START_CUR = SUR%MJD_TAPE_START_CUR
      SUR_OBS%TAI_TAPE_START_CUR = SUR%TAI_TAPE_START_CUR
      DO 410 J1=1,SUR%L_STA
!!         SUR_OBS%USAGE(J1) = SUR%STA(J1)%USAGE
         IF ( SUR%STA(J1)%TAGALONE ) GOTO 410
         SUR_OBS%AZ_CUR(J1) = SUR%STA(J1)%AZ_CUR
         SUR_OBS%EL_CUR(J1) = SUR%STA(J1)%EL_CUR
         SUR_OBS%HA_CUR(J1) = SUR%STA(J1)%HA_CUR
         SUR_OBS%AZ_ACC_CUR(J1)  = SUR%STA(J1)%AZ_ACC_CUR
         SUR_OBS%HA_ACC_CUR(J1)  = SUR%STA(J1)%HA_ACC_CUR
 410  CONTINUE
      SUR_OBS%L_STA = SUR%L_STA
      RETURN
      END  SUBROUTINE SUR_CUR_SAVE  !#!
!
! ------------------------------------------------------------------------
!
      SUBROUTINE SUR_CUR_RESTORE ( SUR, SUR_OBS )
! ************************************************************************
! *                                                                      *
! *   Routine SUR_CUR_RESTORE
! *                                                                      *
! * ## 17-JAN-2007   SUR_CUR_RESTORE  v1.1 (c)  L. Petrov 12-DEC-2016 ## *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'astro_constants.i'
      INCLUDE   'sur_sked.i'
      INCLUDE   'vtd.i'
      TYPE     ( SUR__TYPE     ) :: SUR
      TYPE     ( SUR__OBS_TYPE ) :: SUR_OBS
      INTEGER*4  J1
!
      SUR%L_OBS_TAG = SUR_OBS%L_OBS_TAG
      SUR%L_SCN     = SUR_OBS%L_SCN
      SUR%L_SCN_CAL = SUR_OBS%L_SCN_CAL
      SUR%MJD_CUR   = SUR_OBS%MJD_CUR
      SUR%TAI_CUR   = SUR_OBS%TAI_CUR
      SUR%L_STA     = SUR_OBS%L_STA
      SUR%L_TAP     = SUR_OBS%L_TAP 
      SUR%MJD_TAPE_START_CUR = SUR_OBS%MJD_TAPE_START_CUR 
      SUR%TAI_TAPE_START_CUR = SUR_OBS%TAI_TAPE_START_CUR
      DO 410 J1=1,SUR%L_STA
!!         SUR%STA(J1)%USAGE = SUR_OBS%USAGE(J1)
         IF ( SUR%STA(J1)%TAGALONE ) GOTO 410
         SUR%STA(J1)%AZ_CUR      = SUR_OBS%AZ_CUR(J1)
         SUR%STA(J1)%EL_CUR      = SUR_OBS%EL_CUR(J1)
         SUR%STA(J1)%HA_CUR      = SUR_OBS%HA_CUR(J1)
         SUR%STA(J1)%AZ_ACC_CUR  = SUR_OBS%AZ_ACC_CUR(J1)
         SUR%STA(J1)%HA_ACC_CUR  = SUR_OBS%HA_ACC_CUR(J1)
 410  CONTINUE
      RETURN
      END  SUBROUTINE SUR_CUR_RESTORE  !#!
!
! ------------------------------------------------------------------------
!
      SUBROUTINE SUR_POCAL_GBT_4HR ( SUR, VTD, IVRB, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine SUR_POCAL_GBT_4HR
! *                                                                      *
! * ## 15-JAN-2007  SUR_POCAL_GBT_4HR v2.3 (c) L. Petrov 27-NOV-2010 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'astro_constants.i'
      INCLUDE   'sur_sked.i'
      INCLUDE   'vtd.i'
      INCLUDE   'getpar.i'
      TYPE     ( SUR__TYPE ) :: SUR
      TYPE     ( VTD__TYPE ) :: VTD
      TYPE     ( SUR__TYPE ) :: SUR_OBS
      INTEGER*4  IVRB, IUER
      INTEGER*4  MP
      PARAMETER  ( MP = 8192 )
      TYPE     ( SOURCE_CAT__TYPE ) :: SOUCAT(MP)
      CHARACTER  POCAL_FIL*128, BUF(MP)*256, C_SOU(MP)*8
      REAL*8     TIM_DIF, DIST_COS, HA, SLEW_TIME, SLEW_TIME_PRE, &
     &           SLEW_TIME_POST, SLEW_TIME_MIN, SLEW_PRE_MIN, &
     &           SLEW_POST_MIN, DIF_AZ, DIF_HA, DIST_AZ, DIST_HA
      INTEGER*4  MIN_SOU
      PARAMETER  ( MIN_SOU = 2 )
      REAL*8     POCAL_LEN, EL_MIN, EL_MAX
      PARAMETER  ( POCAL_LEN = POCAL_DUR__GBT_4HR )
      PARAMETER  ( EL_MIN    = 25.0D0*DEG__TO__RAD  )
      PARAMETER  ( EL_MAX    = 65.0D0*DEG__TO__RAD  )
      REAL*8     AZ__MARGIN, EL__MARGIN, AZ__LONG_MARGIN, MIN_SOU_TIM
      PARAMETER  ( AZ__MARGIN = 0.25D0*DEG__TO__RAD )
      PARAMETER  ( EL__MARGIN = 0.25D0*DEG__TO__RAD  )
      PARAMETER  ( AZ__LONG_MARGIN = 5.0D0*DEG__TO__RAD )
      PARAMETER  ( MIN_SOU_TIM = 420.0D0 )
      LOGICAL*1  FL_STA(SUR__M_STA)
      INTEGER*4  IND_SCA_PRE, IND_SCA_POL, L_SOU, MODE, J1, J2, J3, J4, &
     &           K_STA, IND_POC, IND_SRC_POL, IND_SCA_POC, SUR_TYP_POL, &
     &           IND_SRC_PRE, SUR_TYP_PRE, IND_SRC_POC, SUR_TYP_POC, &
     &           ITURN, IER
      CHARACTER  MJDSEC_TO_DATE*30
      REAL*8,    EXTERNAL :: SUR_TROPO_GET_SLEW, DP_VV_V
!
      IF ( SUR%L_SCN .LE. 6 ) THEN
           IND_SCA_PRE = 1
           IND_SCA_POC = 1
           IND_SCA_POL = 4
           TIM_DIF = (SUR%MJD_OBS_BEG(IND_SCA_POL) - SUR%MJD_START)*86440.0D0 + &
     &               (SUR%TAI_OBS_BEG(IND_SCA_POL) - SUR%TAI_START)
         ELSE
           TIM_DIF = (SUR%MJD_OBS_BEG(SUR%L_SCN) - SUR%MJD_OBS_BEG(SUR%LAST_POCAL_SCAN))*86440.0D0 + &
     &               (SUR%TAI_OBS_BEG(SUR%L_SCN) - SUR%TAI_OBS_BEG(SUR%LAST_POCAL_SCAN))
           IF ( TIM_DIF < 3.75*3600.0D0 ) THEN
                SUR%L_SCN = SUR%L_SCN - 1
                SUR%L_SCN_CAL = SUR%L_SCN_CAL - 1
                CALL ERR_LOG ( 0, IUER )
                RETURN
           END IF
           TIM_DIF = (SUR%MJD_STOP - SUR%MJD_OBS_BEG(SUR%L_SCN) )*86440.0D0 + &
     &               (SUR%TAI_STOP - (SUR%POSTSES_INTERVAL - SUR%TAI_OBS_BEG(SUR%L_SCN)) )
           IF ( TIM_DIF < 0.75*SUR%TROPO_BURST_INTERVAL ) THEN
                SUR%L_SCN = SUR%L_SCN - 1
                SUR%L_SCN_CAL = SUR%L_SCN_CAL - 1
                SUR%SCAN_TYPE(SUR%L_SCN) = SUR__LAST
                CALL ERR_LOG ( 0, IUER )
                RETURN
           END IF
           IND_SCA_PRE = SUR%L_SCN - 5 ! Last scan before the tropo burst
           IND_SCA_POC = SUR%L_SCN - 4 ! The scan that will be used for pointing
           IND_SCA_POL = SUR%L_SCN - 1 ! The last scan that will be used for pointing
      END IF
 910  CONTINUE
!
      IND_SRC_PRE = SUR%IND_SRC(IND_SCA_PRE)
      SUR_TYP_PRE = SUR%SRC_TYP(IND_SCA_PRE)
!
      IND_SRC_POC = SUR%IND_SRC(IND_SCA_POC)
      SUR_TYP_POC = SUR%SRC_TYP(IND_SCA_POC)
!
      IND_SRC_POL = SUR%IND_SRC(IND_SCA_POL)
      SUR_TYP_POL = SUR%SRC_TYP(IND_SCA_POL)
!
      CALL SUR_CUR_SAVE ( SUR, SUR_OBS )
      SUR%L_STA = 1
      SUR%L_SCN = IND_SCA_PRE
      SUR%IND_SRC(SUR%L_SCN) = SUR%IND_SRC(IND_SCA_PRE)
      SUR%SRC_TYP(SUR%L_SCN) = SUR%SRC_TYP(IND_SCA_PRE)
!
      SLEW_TIME_MIN = 1.D8
      IND_POC = 0
      DO 410 J1=IND_SCA_PRE,1,-1
         IF ( SUR%OBS_STA(1,J1) == SUR__USED ) THEN
              CALL SUR_AZEL ( SUR, VTD, SUR%SRC_TYP(J1), &
     &                        SUR%MJD_OBS_END(J1), SUR%TAI_OBS_END(J1), &
     &                        1, SUR%IND_SRC(J1), SUR%STA(1)%AZ_CUR, &
     &                        SUR%STA(1)%EL_CUR, SUR%STA(1)%HA_CUR, &
     &                        IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 2711, IUER, 'SUR_POCAL_GBT_4HR', 'Error in '// &
     &                 'computing elevation angle and azimuth' )
                   RETURN
              END IF
              IF ( SUR%STA(1)%AZ_CUR < PI2 ) THEN
                   SUR%STA(1)%AZ_ACC_CUR = SUR%STA(1)%AZ_CUR
                 ELSE
                   ITURN = IDINT ( (SUR%STA(1)%AZ_ACC_CUR  - SUR%STA(1)%AZ_CUR)/PI2 )
                   SUR%STA(1)%AZ_ACC_CUR = SUR%STA(1)%AZ_CUR + &
     &                                     PI2*ITURN
              END IF
!
              IF ( SUR%STA(1)%HA_CUR < PI2 ) THEN
                   SUR%STA(1)%HA_ACC_CUR = SUR%STA(1)%HA_CUR
                 ELSE
                   ITURN = IDINT ( (SUR%STA(1)%HA_ACC_CUR  - SUR%STA(1)%HA_CUR)/PI2 )
                   SUR%STA(1)%HA_ACC_CUR = SUR%STA(1)%HA_CUR + PI2*ITURN
              END IF
              IF ( SUR%SRC_TYP(J1) == SUR__TYP_TAG ) THEN
                   SUR%STA(1)%ALP_CUR = SUR%SOU(SUR%SRC_TYP(J1))%ALPHA
                   SUR%STA(1)%DEL_CUR = SUR%SOU(SUR%SRC_TYP(J1))%DELTA
                ELSE IF ( SUR%SRC_TYP(J1) == SUR__TYP_SEC ) THEN
                   SUR%STA(1)%ALP_CUR = SUR%SO2(SUR%SRC_TYP(J1))%ALPHA
                   SUR%STA(1)%DEL_CUR = SUR%SO2(SUR%SRC_TYP(J1))%DELTA
                ELSE IF ( SUR%SRC_TYP(J1) == SUR__TYP_CAL ) THEN
                   SUR%STA(1)%ALP_CUR = SUR%CAL(SUR%SRC_TYP(J1))%ALPHA
                   SUR%STA(1)%DEL_CUR = SUR%CAL(SUR%SRC_TYP(J1))%DELTA
                ELSE IF ( SUR%SRC_TYP(J1) == SUR__TYP_POC ) THEN
                   SUR%STA(1)%ALP_CUR = SUR%SOP(SUR%SRC_TYP(J1))%ALPHA
                   SUR%STA(1)%DEL_CUR = SUR%SOP(SUR%SRC_TYP(J1))%DELTA
              END IF
!
              SUR%EL_OBS(1,J1) = SUR%STA(1)%EL_CUR
              SUR%AZ_OBS(1,J1) = SUR%STA(1)%AZ_CUR
              SUR%HA_OBS(1,J1) = SUR%STA(1)%HA_CUR
!
              GOTO 810
         END IF
 410  CONTINUE
 810  CONTINUE
!
      DO 420 J2=1,SUR%L_SOP
         IF ( IND_SCA_PRE > 1 ) THEN
!
! ----------- Compute the slewing time from the observation preceeding
! ----------- the first observation of the troposphere caliburators
! ----------- burst
!
              SUR%IND_SRC(SUR%L_SCN) = IND_SRC_PRE
              SUR%SRC_TYP(SUR%L_SCN) = SUR_TYP_PRE
              SUR%MJD_CUR = SUR%MJD_OBS_END(IND_SCA_PRE)
              SUR%TAI_CUR = SUR%TAI_OBS_END(IND_SCA_PRE)
!
              CALL ERR_PASS ( IUER, IER )
              SLEW_TIME_PRE = SUR_TROPO_GET_SLEW ( SUR, VTD, SUR%MJD_CUR, &
     &                            SUR%TAI_CUR, POCAL_LEN, SUR__TYP_POC, &
     &                            J2, SUR_TYP_POC, EL_MIN, EL_MAX, &
     &                            SUR__FINE, K_STA, FL_STA, IER )
              IF ( K_STA .NE. 1 ) GOTO 420
            ELSE
              SLEW_TIME_PRE = 0.0D0
         END IF
!
! ------ Compute the slewing time from the current observation to the observation
! ------ in the middle of the the troposphere caliburators burst
! ------ which GBT can catch up
!
         SUR%IND_SRC(SUR%L_SCN) = IND_SRC_POL
         SUR%SRC_TYP(SUR%L_SCN) = SUR_TYP_POL
         SUR%MJD_CUR = SUR%MJD_OBS_BEG(IND_SCA_POL)
         SUR%TAI_CUR = SUR%TAI_OBS_BEG(IND_SCA_POL)
!
         CALL ERR_PASS ( IUER, IER )
         SLEW_TIME_POST =  SUR_TROPO_GET_SLEW ( SUR, VTD, SUR%MJD_CUR, &
     &                                    SUR%TAI_CUR+POCAL_LEN, &
     &                                    POCAL_LEN, SUR__TYP_POC, &
     &                                    J2, SUR_TYP_POL, EL_MIN, EL_MAX, &
     &                                    SUR__FINE, K_STA, FL_STA, IER )
         IF ( K_STA .NE. 1 ) GOTO 420
!
         DIST_COS = DP_VV_V ( 3, SUR%CAL(IND_SRC_POL)%S_VEC, SUR%SOP(J2)%S_VEC )
         IF ( DIST_COS > 1.D0 - 1.D-8 ) THEN
!
! ----------- Aga! The last observed source and the pointing source are the same.
! ----------- No need to slew after point is done.
!
              SLEW_TIME_POST = 0.0D0
         END IF
!
         SLEW_TIME = SLEW_TIME_PRE + SLEW_TIME_POST
!
! ------ A special trick to force to observe first two sources
!
         IF ( J2 .GE. 1 .AND. J2 .LE. MIN_SOU .AND. &
     &        SLEW_TIME < MIN_SOU_TIM ) THEN
              IND_POC = J2
         END IF
         IF ( ( IND_POC .LE. 0 .OR. IND_POC .GT. MIN_SOU ) .AND. &
     &        IND_POC .NE. 2 .AND. K_STA == 1  .AND.  &
     &        SLEW_TIME < SLEW_TIME_MIN ) THEN

              IND_POC = J2 ! Index of the source in the SOP list
              SLEW_TIME_MIN = SLEW_TIME
              SLEW_PRE_MIN  = SLEW_TIME_PRE
              SLEW_POST_MIN = SLEW_TIME_POST
         END IF
 420  CONTINUE
!
      IF ( SUR%LAST_POCAL_SCAN > 0 ) THEN
          TIM_DIF = (SUR%MJD_OBS_BEG(IND_SCA_POL) - SUR%MJD_OBS_END(IND_SCA_PRE))*86440.0D0 + &
     &              (SUR%TAI_OBS_BEG(IND_SCA_POL) - SUR%TAI_OBS_END(IND_SCA_PRE))
      END IF
      IF ( SLEW_TIME_MIN + POCAL_LEN > TIM_DIF  .AND. &
     &     IND_SCA_POL < SUR%L_SCN                    ) THEN
!
           CALL SUR_CUR_RESTORE ( SUR, SUR_OBS )
           IND_SCA_POL = IND_SCA_POL + 1
           TIM_DIF = (SUR%MJD_OBS_BEG(IND_SCA_POL) - SUR%MJD_START)*86440.0D0 + &
     &               (SUR%TAI_OBS_BEG(IND_SCA_POL) - SUR%TAI_START)
           GOTO 910
      END IF
      CALL SUR_CUR_RESTORE ( SUR, SUR_OBS )
!
      IF ( IVRB .GE. 4 ) THEN
           WRITE ( 6, 230 ) SLEW_TIME_MIN + POCAL_LEN, TIM_DIF, &
     &                      SUR%SOP(IND_POC)%J2000_NAME
 230       FORMAT ( 'POCAL: Tim: ', F8.1, ' Rem: ', F8.1, &
     &              ' Sou_poc: ', A )
      END IF
!
      CALL ERR_PASS( IUER, IER )
      CALL SUR_AZEL ( SUR, VTD, SUR__TYP_POC, SUR%MJD_OBS_END(IND_SCA_PRE), &
     &                SUR%TAI_OBS_END(IND_SCA_PRE) + SLEW_PRE_MIN, &
     &                1, IND_POC, &
     &                SUR%STA(1)%AZ_CUR, &
     &                SUR%STA(1)%EL_CUR, &
     &                SUR%STA(1)%HA_CUR, IER )
!
      SUR%AZ_OBS(1,IND_SCA_POC) = SUR%STA(1)%AZ_CUR
      SUR%HA_OBS(1,IND_SCA_POC) = SUR%STA(1)%HA_CUR
      SUR%EL_OBS(1,IND_SCA_POC) = SUR%STA(1)%EL_CUR
      SUR%HA_ACC_OBS(1,IND_SCA_POC) = SUR%STA(1)%HA_CUR ! Simplification!
      IF ( SUR%STA(1)%MOUNT_TYPE == MT__ALTAZ ) THEN
!
! -------- Update accumulative aziumth in order to take into account
! -------- cable wrap
!
           IF ( SUR%STA(1)%AZ_ACC_CUR < -100.0D0 ) THEN
!
! ------------- This means that this was the first scan
!
                SUR%STA(1)%AZ_ACC_CUR = SUR%AZ_OBS(1,IND_SCA_POC)
                SUR%STA(1)%HA_ACC_CUR = SUR%HA_OBS(1,IND_SCA_POC)
                SUR%AZ_ACC_OBS(1,IND_SCA_POC) = SUR%STA(1)%AZ_ACC_CUR
              ELSE
                DIF_AZ = (SUR%STA(1)%AZ_CUR - SUR%AZ_OBS(1,IND_SCA_PRE) )
                DIF_AZ = DIF_AZ - PI2*IDNINT(DIF_AZ/PI2)
!
! ------------- Check whether the shortest azimuthal move is possible
!
                IF ( DIF_AZ > 0.0D0 ) THEN
                     IF ( SUR%AZ_OBS(1,IND_SCA_PRE) + DIF_AZ < SUR%STA(1)%AZ_ACC_MAX - AZ__MARGIN  ) THEN
                          DIST_AZ = DIF_AZ
                       ELSE
                          DIST_AZ = DIF_AZ - PI2
                     END IF
                   ELSE ! if ( DIF_AZ .LE. 0.0D0 ) then
                     IF ( SUR%AZ_OBS(1,IND_SCA_PRE) + DIF_AZ > SUR%STA(1)%AZ_ACC_MIN + AZ__MARGIN ) THEN
                          DIST_AZ = DIF_AZ
                        ELSE
                          DIST_AZ = DIF_AZ + PI2
                     END IF
                END IF
!
                DIF_HA = (SUR%STA(1)%HA_CUR - SUR%HA_ACC_OBS(1,IND_SCA_PRE) )
                DIF_HA = DIF_HA - PI2*IDNINT(DIF_HA/PI2)
!
! ------------- Check whether the shortest move through hour angle is possible
!
                IF ( DIF_HA > 0.0D0 ) THEN
                     IF ( SUR%HA_ACC_OBS(1,IND_SCA_PRE) + DIF_HA < SUR%STA(1)%AZ_ACC_MAX - AZ__MARGIN  ) THEN
                          DIST_HA = DIF_HA
                       ELSE
                          DIST_HA = DIF_HA - PI2
                     END IF
                   ELSE ! if ( DIF_HA .LE. 0.0D0 ) then
                     IF ( SUR%HA_ACC_OBS(1,IND_SCA_PRE) + DIF_HA > SUR%STA(1)%AZ_ACC_MIN + AZ__MARGIN ) THEN
                          DIST_HA = DIF_HA
                        ELSE
                          DIST_HA = DIF_HA + PI2
                     END IF
                END IF
!
                IF ( SUR%AZ_OBS(1,IND_SCA_PRE) < -100.0D0 ) THEN
!
! ------------------- Special case of the first observation in the experiment
!
                      SUR%STA(1)%AZ_ACC_CUR = SUR%STA(1)%AZ_CUR
                      SUR%STA(1)%HA_ACC_CUR = SUR%STA(1)%HA_CUR
                    ELSE
                      SUR%STA(1)%AZ_ACC_CUR = SUR%AZ_ACC_OBS(1,IND_SCA_PRE) + DIST_AZ
                      SUR%STA(1)%HA_ACC_CUR = SUR%HA_ACC_OBS(1,IND_SCA_PRE) + DIST_HA
                END IF
!
                SUR%EL_OBS(1,IND_SCA_POC)     = SUR%STA(1)%EL_CUR
                SUR%AZ_OBS(1,IND_SCA_POC)     = SUR%STA(1)%AZ_CUR
                SUR%HA_OBS(1,IND_SCA_POC)     = SUR%STA(1)%HA_CUR
                SUR%AZ_ACC_OBS(1,IND_SCA_POC) = SUR%STA(1)%AZ_ACC_CUR
                SUR%HA_ACC_OBS(1,IND_SCA_POC) = SUR%STA(1)%HA_ACC_CUR
           END IF
         ELSE
           SUR%STA(1)%AZ_ACC_CUR = SUR%AZ_OBS(1,IND_SCA_POC)
           SUR%AZ_ACC_OBS(1,IND_SCA_POC) = SUR%STA(1)%AZ_ACC_CUR
      END IF
!
! --- Mark skipped scans
!
      DO 430 J3=IND_SCA_POC,IND_SCA_POL
         SUR%OBS_STA(1,J3) = SUR__UND
 430  CONTINUE
!
      IF ( IND_SCA_POC == 1 ) THEN
           SUR%MJD_POCAL(IND_SCA_POC) = SUR%MJD_START
           SUR%TAI_POCAL(IND_SCA_POC) = SUR%TAI_START
         ELSE
           SUR%MJD_POCAL(IND_SCA_POC) = SUR%MJD_OBS_END(IND_SCA_PRE)
           SUR%TAI_POCAL(IND_SCA_POC) = SUR%TAI_OBS_END(IND_SCA_PRE) + SLEW_PRE_MIN
           IF ( SUR%TAI_POCAL(IND_SCA_POC) > 86400.0D0 ) THEN
                SUR%MJD_POCAL(IND_SCA_POC) = SUR%MJD_POCAL(IND_SCA_POC) + 1
                SUR%TAI_POCAL(IND_SCA_POC) = SUR%TAI_POCAL(IND_SCA_POC) - 86400.0D0
           END IF
      END IF
!
      SUR%SOU_POCAL(IND_SCA_POC) = IND_POC
      SUR%LAST_POCAL_SCAN = IND_SCA_POC
!
! --- Restore
!
      SUR%IND_SRC(IND_SCA_POC) = IND_SRC_POC
      SUR%SRC_TYP(IND_SCA_POC) = SUR_TYP_POC
!
      SUR%L_OBS_POC = SUR%L_OBS_POC  + 1
      IF ( SUR%L_OBS_POC > 3  .AND. IND_SCA_POL < SUR%L_SCN ) THEN
           SUR%L_SCN = SUR%L_SCN - 1
           SUR%L_SCN_CAL = SUR%L_SCN_CAL - 1
           CALL ERR_LOG ( 0, IUER )
           RETURN
      END IF
!!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  SUR_POCAL_GBT_4HR  !#!
!
! ------------------------------------------------------------------------
!
      SUBROUTINE SUR_POCAL_KVN ( SUR, VTD, IVRB, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine SUR_POCAL_KVN
! *                                                                      *
! * ### 23-NOV-2012   SUR_POCAL_KVN   v1.0 (c) L. Petrov 27-NOV-2012 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'astro_constants.i'
      INCLUDE   'sur_sked.i'
      INCLUDE   'vtd.i'
      INCLUDE   'getpar.i'
      TYPE     ( SUR__TYPE ) :: SUR
      TYPE     ( VTD__TYPE ) :: VTD
      TYPE     ( SUR__TYPE ) :: SUR_OBS
      INTEGER*4  IVRB, IUER
      REAL*8     EL_MIN, EL_MAX, POCAL_LEN, PLA_LEN
      PARAMETER  ( EL_MIN = 25.0D0*DEG__TO__RAD )
      PARAMETER  ( EL_MAX = 90.0D0*DEG__TO__RAD )
      REAL*8     SLEW_TIME, SLEW_TIME_MIN
      LOGICAL*1  FL_STA(SUR__M_STA), FL_STA_MIN(SUR__M_STA), FL_SKIP_POCAL 
      INTEGER*4  J1, J2, J3, J4, K_STA, KPOC_MIN, KPLA_MIN, IER
      REAL*8,    EXTERNAL :: SUR_TROPO_GET_SLEW 
!
      KPOC_MIN = 0
      SLEW_TIME_MIN = 1.D10
      FL_STA_MIN = .FALSE.
      FL_SKIP_POCAL = .FALSE.
!
      IF ( SUR%L_SCN_POC > 0 ) THEN
!
! -------- Check when did we observe pointing scan
!
           DO 410 J1=SUR%L_SCN,1,-1
              IF ( SUR%SRC_TYP(J1) == SUR__TYP_POC  ) THEN
                   IF ( (SUR%MJD_CUR - SUR%MJD_OBS_BEG(J1))*86400.0D0 + &
     &                  (SUR%TAI_CUR - SUR%TAI_OBS_BEG(J1)) < &
     &                   SUR__POCAL_RAT*SUR%TROPO_BURST_INTERVAL ) THEN
                        FL_SKIP_POCAL = .TRUE.
                   END IF
              END IF
 410       CONTINUE 
      END IF
      IF ( FL_SKIP_POCAL ) THEN
           CALL ERR_LOG ( 0, IUER )
           RETURN
      END IF
!
      DO 420 J2=1,SUR%L_SOP
         CALL ERR_PASS ( IUER, IER ) 
         SLEW_TIME = SUR_TROPO_GET_SLEW ( SUR, VTD, SUR%MJD_CUR, SUR%TAI_CUR, &
     &                                    SUR__POCAL_LEN, SUR__TYP_POC, &
     &                                    J2, EL_MIN, EL_MAX, SUR__FINE, &
     &                                    K_STA, FL_STA, IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 2511, IUER, 'SUR_POCAL_KVN', 'Error in an attempt '// &
     &            'to compute slewing time' )
              RETURN 
         END IF
         IF ( SLEW_TIME > 0.D0 ) THEN
              IF ( SLEW_TIME < SLEW_TIME_MIN ) THEN
                   KPOC_MIN = J2
                   SLEW_TIME_MIN = SLEW_TIME
                   FL_STA_MIN = FL_STA
              END IF
         END IF
 420  CONTINUE 
!
      IF ( KPOC_MIN > 0 ) THEN
           SUR%TAI_CUR = SUR%TAI_CUR + SLEW_TIME_MIN
           IF ( SUR%TAI_CUR > 86400.0D0 ) THEN
                SUR%TAI_CUR = SUR%TAI_CUR - 86400.0D0 
                SUR%MJD_CUR = SUR%MJD_CUR + 1
           END IF
           CALL ERR_PASS ( IUER, IER ) 
           CALL SUR_TROPO_SCAN_UPDATE ( SUR, VTD, SUR__TYP_POC, KPOC_MIN, &
     &          SUR%MJD_CUR, SUR%TAI_CUR, SUR__POCAL_LEN, FL_STA_MIN, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 2511, IUER, 'SUR_POCAL_KVN', 'Error in an attempt '// &
     &              'to update pointing calibration scan' )
                RETURN 
           END IF
      END IF
!
      KPLA_MIN = 0
      SLEW_TIME_MIN = 1.D10
      FL_STA_MIN = .FALSE.
!
      DO 430 J3=1,SUR%L_PLA
         CALL ERR_PASS ( IUER, IER ) 
         SLEW_TIME = SUR_TROPO_GET_SLEW ( SUR, VTD, SUR%MJD_CUR, SUR%TAI_CUR, &
     &                                    SUR__PLA_LEN, SUR__TYP_PLA, &
     &                                    J3, EL_MIN, EL_MAX, SUR__FINE, &
     &                                    K_STA, FL_STA, IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 2513, IUER, 'SUR_POCAL_KVN', 'Error in an attempt '// &
     &            'to compute slewing time' )
              RETURN 
         END IF
         IF ( SLEW_TIME > 0.D0 ) THEN
              IF ( SLEW_TIME < SLEW_TIME_MIN ) THEN
                   KPLA_MIN = J3
                   SLEW_TIME_MIN = SLEW_TIME
                   FL_STA_MIN = FL_STA
              END IF
         END IF
 430  CONTINUE 
!
      IF ( KPLA_MIN > 0 ) THEN
           SUR%TAI_CUR = SUR%TAI_CUR + SLEW_TIME_MIN
           IF ( SUR%TAI_CUR > 86400.0D0 ) THEN
                SUR%TAI_CUR = SUR%TAI_CUR - 86400.0D0 
                SUR%MJD_CUR = SUR%MJD_CUR + 1
           END IF
           CALL ERR_PASS ( IUER, IER ) 
           CALL SUR_TROPO_SCAN_UPDATE ( SUR, VTD, SUR__TYP_PLA, KPLA_MIN, &
     &          SUR%MJD_CUR, SUR%TAI_CUR, SUR__PLA_LEN, FL_STA_MIN, IER )
           IF ( IER .NE. 0 ) THEN
                 CALL ERR_LOG ( 2511, IUER, 'SUR_POCAL_KVN', 'Error in an attempt '// &
     &               'to update planet scan' )
                 RETURN 
           END IF
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  SUR_POCAL_KVN  !#!
!
! ------------------------------------------------------------------------
!
      SUBROUTINE SUR_POCAL_ATCA ( SUR, VTD, IVRB, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine SUR_POCAL_ATCA
! *                                                                      *
! * ### 13-SEP-2014  SUR_POCAL_ATCA   v1.0 (c) L. Petrov 27-NOV-2012 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'astro_constants.i'
      INCLUDE   'sur_sked.i'
      INCLUDE   'vtd.i'
      INCLUDE   'getpar.i'
      TYPE     ( SUR__TYPE ) :: SUR
      TYPE     ( VTD__TYPE ) :: VTD
      TYPE     ( SUR__TYPE ) :: SUR_OBS
      INTEGER*4  IVRB, IUER
      REAL*8     EL_MIN, EL_MAX, POCAL_LEN, PLA_LEN
      PARAMETER  ( EL_MIN = 25.0D0*DEG__TO__RAD )
      PARAMETER  ( EL_MAX = 90.0D0*DEG__TO__RAD )
      REAL*8     SLEW_TIME, SLEW_TIME_MIN
      LOGICAL*1  FL_STA(SUR__M_STA), FL_STA_MIN(SUR__M_STA), FL_SKIP_POCAL 
      INTEGER*4  J1, J2, J3, J4, K_STA, KPOC_MIN, KPLA_MIN, IER
      REAL*8,    EXTERNAL :: SUR_TROPO_GET_SLEW 
!
      KPOC_MIN      = 0
      SLEW_TIME_MIN = 1.D10
      FL_STA_MIN    = .FALSE.
      FL_SKIP_POCAL = .FALSE.
!
!@      IF ( (SUR%MJD_CUR - SUR%MJD_START)*86400.0D0 + &
!@     &     (SUR%TAI_CUR - SUR%TAI_START)           < SUR__POCAL_INT ) THEN
!@           FL_SKIP_POCAL = .TRUE.
!@      END IF
      IF ( SUR%L_SCN_POC > 0 ) THEN
!
! -------- Check when did we observe pointing scan
!
           DO 410 J1=SUR%L_SCN,1,-1
              IF ( SUR%SRC_TYP(J1) == SUR__TYP_POC  ) THEN
                   IF ( (SUR%MJD_CUR - SUR%MJD_OBS_BEG(J1))*86400.0D0 + &
     &                  (SUR%TAI_CUR - SUR%TAI_OBS_BEG(J1)) < &
     &                   SUR__POCAL_INT ) THEN
                        FL_SKIP_POCAL = .TRUE.
                   END IF
              END IF
 410       CONTINUE 
      END IF
      IF ( FL_SKIP_POCAL ) THEN
           CALL ERR_LOG ( 0, IUER )
           RETURN
      END IF
!
      DO 420 J2=1,SUR%L_SOP
         CALL ERR_PASS ( IUER, IER ) 
         SLEW_TIME = SUR_TROPO_GET_SLEW ( SUR, VTD, SUR%MJD_CUR, SUR%TAI_CUR, &
     &                                    SUR__POCAL_LEN, SUR__TYP_POC, &
     &                                    J2, EL_MIN, EL_MAX, SUR__FINE, &
     &                                    K_STA, FL_STA, IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 2511, IUER, 'SUR_POCAL_ATCA', 'Error in an attempt '// &
     &            'to compute slewing time' )
              RETURN 
         END IF
         IF ( SLEW_TIME > 0.D0 ) THEN
              KPOC_MIN = J2
              FL_STA_MIN = FL_STA
              SLEW_TIME_MIN = SLEW_TIME
!
!              IF ( SLEW_TIME < SLEW_TIME_MIN ) THEN
!                   KPOC_MIN = J2
!                   SLEW_TIME_MIN = SLEW_TIME
!                   FL_STA_MIN = FL_STA
!              END IF
         END IF
 420  CONTINUE 
!
      IF ( KPOC_MIN > 0 ) THEN
           SUR%TAI_CUR = SUR%TAI_CUR + SLEW_TIME_MIN
           IF ( SUR%TAI_CUR > 86400.0D0 ) THEN
                SUR%TAI_CUR = SUR%TAI_CUR - 86400.0D0 
                SUR%MJD_CUR = SUR%MJD_CUR + 1
           END IF
           CALL ERR_PASS ( IUER, IER ) 
           CALL SUR_TROPO_SCAN_UPDATE ( SUR, VTD, SUR__TYP_POC, KPOC_MIN, &
     &          SUR%MJD_CUR, SUR%TAI_CUR, SUR__POCAL_LEN, FL_STA_MIN, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 2511, IUER, 'SUR_POCAL_ATCA', 'Error in an attempt '// &
     &              'to update pointing calibration scan' )
                RETURN 
           END IF
      END IF
!
      KPLA_MIN = 0
      SLEW_TIME_MIN = 1.D10
      FL_STA_MIN = .FALSE.
!
      DO 430 J3=1,SUR%L_PLA
         CALL ERR_PASS ( IUER, IER ) 
         SLEW_TIME = SUR_TROPO_GET_SLEW ( SUR, VTD, SUR%MJD_CUR, SUR%TAI_CUR, &
     &                                    SUR__PLA_LEN, SUR__TYP_PLA, &
     &                                    J3, EL_MIN, EL_MAX, SUR__FINE, &
     &                                    K_STA, FL_STA, IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 2513, IUER, 'SUR_POCAL_ATCA', 'Error in an attempt '// &
     &            'to compute slewing time' )
              RETURN 
         END IF
         IF ( SLEW_TIME > 0.D0 ) THEN
              IF ( SLEW_TIME < SLEW_TIME_MIN ) THEN
                   KPLA_MIN = J3
                   SLEW_TIME_MIN = SLEW_TIME
                   FL_STA_MIN = FL_STA
              END IF
         END IF
 430  CONTINUE 
!
      IF ( KPLA_MIN > 0 ) THEN
           SUR%TAI_CUR = SUR%TAI_CUR + SLEW_TIME_MIN
           IF ( SUR%TAI_CUR > 86400.0D0 ) THEN
                SUR%TAI_CUR = SUR%TAI_CUR - 86400.0D0 
                SUR%MJD_CUR = SUR%MJD_CUR + 1
           END IF
           CALL ERR_PASS ( IUER, IER ) 
           CALL SUR_TROPO_SCAN_UPDATE ( SUR, VTD, SUR__TYP_PLA, KPLA_MIN, &
     &          SUR%MJD_CUR, SUR%TAI_CUR, SUR__PLA_LEN, FL_STA_MIN, IER )
           IF ( IER .NE. 0 ) THEN
                 CALL ERR_LOG ( 2511, IUER, 'SUR_POCAL_ATCA', 'Error in an attempt '// &
     &               'to update planet scan' )
                 RETURN 
           END IF
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  SUR_POCAL_ATCA  !#!
!
! ------------------------------------------------------------------------
!
      FUNCTION   COMPAR_R82 ( ARR1, ARR2 )
      IMPLICIT   NONE
      REAL*8     ARR1(2), ARR2(2)
#ifdef GNU
      INTEGER*4  COMPAR_R82
#else
      INTEGER*2  COMPAR_R82
#endif
!
      IF ( ARR1(1) > ARR2(1) ) THEN
           COMPAR_R82 =  1
         ELSE IF ( ARR1(1) < ARR2(1) ) THEN
           COMPAR_R82 = -1
         ELSE
           COMPAR_R82 =  0
      END IF
!
      RETURN
      END  FUNCTION  COMPAR_R82  !#!#
