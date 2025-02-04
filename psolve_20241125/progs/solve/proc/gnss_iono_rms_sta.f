      SUBROUTINE GNSS_IONO_RMS_STA ( MOBS, STA_NAM, FL_PLOT, IS, EL_MIN, EL_MAX, &
     &                               TIM_STEP, MJD_BEG, TAI_BEG, IONO_AVR, &
     &                               IONO_RMS, IONO_FIT_RMS, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine GNSS_IONO_RMS_STA
! *                                                                      *
! * ### 20-DEC-2021 GNSS_IONO_RMS_STA v1.0 (c) L. Petrov 20-DEC-2021 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'vtd.i'
      INCLUDE   'ners_local.i'
      INCLUDE   'astro_constants.i'
      TYPE     ( VTD__TYPE   ) :: VTD
      TYPE     ( NERS__TYPE  ) :: NERS
      TYPE     ( VTD__OBS_TYPE  ) :: OBS_TYP
      CHARACTER  STA_NAM*(*)
      INTEGER*4  MOBS, MJD_BEG, IUER 
      REAL*8     EL_MIN, EL_MAX, TIM_STEP, TAI_BEG, IONO_AVR, IONO_RMS, &
     &           IONO_FIT_RMS
      LOGICAL*1  FL_PLOT
      CHARACTER  FIL_VTD_CONF*128, STR*128, C_STA(VTD__M_STA)*8, C_SOU(VTD__M_SOU)*8
      INTEGER*4  DEG
      PARAMETER  ( DEG = 3 )
      REAL*8     EL(MOBS), AZ(MOBS), IONO_ZEN(MOBS), IONO_FIT(MOBS), &
     &           TAU_GR, RATE_PH, &
     &           DER_DEL(VTD__NDER), DER_RAT(VTD__NDER), RA, DEC, &
     &           AZ_VAL, EL_VAL, HA_VAL, AZ_RATE, EL_RATE, HA_RATE, &
     &           ACC_VAL, ACC_SQR
      REAL*8     SOU_VEC(3,MOBS), RA_VEC(MOBS), DEC_VEC(MOBS), TIM(MOBS), &
     &           ARG_VEC(MOBS), SPL_VEC(1-DEG:MOBS)
      REAL*8     SPL_STEP, SPL_STEP_ORIG, OVR, &
     &           CNS_VAL_SIG, CNS_DER_SIG, CNS_DR2_SIG
      INTEGER*4  L_STA, L_SOU
      INTEGER*4  MJD, J1, J2, J3, J4, J5, J6, J7, NK, IS, IDAY, &
     &           MJD_END, MJD_OBS, IER
      REAL*8     TAI_END, TAI_OBS, TIM_OBS, TIM_TAI
      REAL*8,    EXTERNAL :: RANDOM_NUMB, EBSPL_VAL_R8
      CHARACTER, EXTERNAL :: MJDSEC_TO_DATE*32
!
      SPL_STEP_ORIG = 5000.0
      CNS_VAL_SIG = 0.0
      CNS_DER_SIG = 0.0
      CNS_DR2_SIG = 1.D-7
      OVR = 0.25D0
!
      OBS_TYP%PLRZ    = 'RR'     
      OBS_TYP%FRQ_REF(1) = 2.2D9
      OBS_TYP%FRQ_REF(2) = 8.0D9
      OBS_TYP%N_BND      = 2
      OBS_TYP%DELAY_TYPE = VTD__MH__DTP
      OBS_TYP%FRQ_ION_EFF(1) = 2.0D9
      OBS_TYP%FRQ_ION_EFF(2) = 8.0D9
      OBS_TYP%EXP_NAME   = 'iono_daily'
      OBS_TYP%SCAN_NAME  = 'Scan_0001'
      OBS_TYP%STATUS     = VTD__BND 
!
      MJD_END = MJD_BEG + 1
      TAI_END = TAI_BEG
!
      FIL_VTD_CONF = '/apr/psolve/vtd_getdb.cnf'
!
! --- Initialize VTD object
!
      CALL ERR_PASS ( IUER, IER )
      CALL VTD_INIT ( VTD,  IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 2481, IUER, 'GNSS_IONO_RMS', 'Error in '// &
     &         'initialization of the VTD object' )
           RETURN 
      END IF
!
! --- Read and parse VTD configuration file
!
      CALL ERR_PASS ( IUER, IER )
      CALL VTD_CONF ( FIL_VTD_CONF, VTD, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 2482, IUER, 'GNSS_IONO_RMS', 'Error in '// &
     &         'an attempt to read configuration file '//FIL_VTD_CONF )
           RETURN 
      END IF
      VTD%CONF%IVRB = 0
!
      L_STA = 1
      C_STA(L_STA) = STA_NAM
      L_SOU = 1
      C_SOU(L_SOU) = '1053+815'
!
! --- Load catalogues, ephemerides, EOP series and other data files
!
      CALL ERR_PASS ( IUER, IER )
      CALL VTD_LOAD  ( VTD, L_STA, C_STA, L_SOU, C_SOU, &
     &                 MJD_BEG, TAI_BEG, MJD_END, TAI_END, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 2483, IUER, 'GNSS_IONO_RMS', 'Error in an '// &
     &         'attempt to load the data into VTD data structure' )
           RETURN 
      END IF
!
      VTD%CONF%HZD_MODEL = VTD__SAA 
      VTD%CONF%WZD_MODEL = VTD__NONE
      VTD%CONF%HMF_MODEL = VTD__NMFH  
      VTD%CONF%ATM_PARTIAL_TYPE = VTD__NONE 
      CALL CLRCH ( VTD%CONF%SPD_MODEL )
!
!!   write ( 6, * ) ' mjd_beg/mjd_end = ', mjd_beg, mjd_end ! %%%
!
      DO 410 J1=1,MOBS
         MJD_OBS = MJD_BEG
         TAI_OBS = TAI_BEG + (J1-1)*TIM_STEP
         IDAY = TAI_OBS/86400.D0
         MJD_OBS = MJD_OBS + IDAY
         TAI_OBS = TAI_OBS - IDAY*86400.0D0
!
         CALL ERR_PASS ( IUER, IER )
         CALL VTD_DELAY ( C_SOU(1), C_STA(1), C_STA(1), MJD_OBS,     &
     &                    TAI_OBS, OBS_TYP, VTD, TAU_GR, RATE_PH,        &
     &                    DER_DEL, DER_RAT, IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 2484, IUER, 'GNSS_IONO_RMS', 'Error in '// &
     &            'attempt to compute VLBI time delay ' )
              RETURN 
         END IF
!
         DO 420 J2=1,1024
            RA  =        RANDOM_NUMB ( IS, 0.0D0, PI2 )
            DEC = DASIN( RANDOM_NUMB ( IS, -1.0D0, 1.0D0 ) )
!
            TIM_TAI = (MJD_OBS - J2000__MJD)*86400.0D0 + TAI_OBS
!
            CALL ERR_PASS ( IUER, IER )
            CALL NERS_AZELHA_COMP ( VTD%NERS, TIM_TAI, VTD%STA(1)%COO_TRS, &
     &                              RA, DEC, 'none', AZ_VAL, EL_VAL, HA_VAL, &
     &                              AZ_RATE, EL_RATE, HA_RATE, IER )
            IF ( IER .NE. 0 ) THEN
                 CALL ERR_LOG ( 2485, IUER, 'GNSS_IONO_RMS', 'Error in '// &
     &               'computing azimuth and elevation' )
                 RETURN 
            END IF
!!   write ( 6, * ) ' j1= ', j1, ' RA/DEC= ', RA, DEC, ' AZ/EL = ', AZ_VAL, EL_VAL ! %%%
            IF ( EL_VAL > EL_MIN .AND. EL_VAL < EL_MAX ) GOTO 820
 420     CONTINUE 
         CALL ERR_LOG ( 2486, IUER, 'GNSS_IONO_RMS', 'Trap of internal '// &
     &                 'control' ) 
         RETURN
 820     CONTINUE 
         EL(J1) = EL_VAL
         AZ(J1) = AZ_VAL
         SOU_VEC(1,J1) = DCOS(RA)*DCOS(DEC)
         SOU_VEC(2,J1) = DSIN(RA)*DCOS(DEC)
         SOU_VEC(3,J1) = DSIN(DEC)
         RA_VEC(J1)  = RA
         DEC_VEC(J1) = DEC
 410  CONTINUE 
!
      DO 430 J3=1,MOBS
         VTD%SOU(J3)%S_CRS(1:3) = SOU_VEC(1:3,J3)
         VTD%SOU(J3)%ALPHA      = RA_VEC(J3)
         VTD%SOU(J3)%DELTA      = DEC_VEC(J3)
         VTD%SOU(J3)%IVS_NAME   = 'Sou_0000'
         CALL INCH ( J3, VTD%SOU(J3)%IVS_NAME(5:8) )
         CALL CHASHR ( VTD%SOU(J3)%IVS_NAME(5:8) )
         CALL BLANK_TO_ZERO ( VTD%SOU(J3)%IVS_NAME(5:8) )
         C_SOU(J3) = VTD%SOU(J3)%IVS_NAME
         VTD%SOU(J3)%J2000_NAME = 'J_'//VTD%SOU(J3)%IVS_NAME   
         VTD%SOU(J3)%OBJ_TYPE = VTD__MG
         VTD%SOU(J3)%MJD_REF = MJD_BEG
         VTD%SOU(J3)%TAI_REF = TAI_BEG
 430  CONTINUE 
      VTD%L_SOU = MOBS
!
      DO 440 J4=1,MOBS
         MJD_OBS = MJD_BEG
         TAI_OBS = TAI_BEG + (J4-1)*TIM_STEP
         IDAY = TAI_OBS/86400.D0
         MJD_OBS = MJD_OBS + IDAY
         TAI_OBS = TAI_OBS - IDAY*86400.0D0
!
         CALL ERR_PASS ( IUER, IER )
         CALL VTD_DELAY ( C_SOU(J4), C_STA(1), C_STA(1), MJD_OBS,     &
     &                    TAI_OBS, OBS_TYP, VTD, TAU_GR, RATE_PH,        &
     &                    DER_DEL, DER_RAT, IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 2487, IUER, 'GNSS_IONO_RMS', 'Error in '// &
     &            'attempt to compute VLBI time delay ' )
              RETURN 
         END IF
         IONO_ZEN(J4) = 1.D12*VTD%STA(1)%IONO_DEL/VTD%STA(1)%IONO_MAP
         TIM(J4) = (J4-1)*TIM_STEP
 440  CONTINUE 
!
      NK = TIM(MOBS)/SPL_STEP_ORIG + 1
      IF ( TIM(MOBS) - NK*SPL_STEP_ORIG < OVR*SPL_STEP_ORIG) THEN
           SPL_STEP = TIM(MOBS)/(NK-1)
         ELSE
           NK = NK + 1
           SPL_STEP = SPL_STEP_ORIG
      END IF
      DO 450 J5=1,NK
         ARG_VEC(J5) = (J5-1)*SPL_STEP
 450  CONTINUE 
      ARG_VEC(1)  = ARG_VEC(1)  - 1.0D0
      ARG_VEC(NK) = ARG_VEC(NK) + 1.0D0
!
      CALL ERR_PASS ( IUER, IER )
      CALL EBSPL_LSQ_CNS ( MOBS, TIM, IONO_ZEN, NK, DEG, ARG_VEC, SPL_VEC, &
     &                     CNS_VAL_SIG, CNS_DER_SIG, CNS_DR2_SIG, &
     &                     IONO_FIT_RMS, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 2488, IUER, 'GNSS_IONO_RMS', 'Error in '// &
     &         'attempt to compute VLBI time delay ' )
           RETURN 
      END IF
!      
      ACC_VAL = 0.0D0
      ACC_SQR = 0.0D0
      DO 460 J6=1,MOBS
         IONO_FIT(J6) = EBSPL_VAL_R8 ( NK, DEG, TIM(J6), ARG_VEC, SPL_VEC )
         ACC_VAL = ACC_VAL + IONO_FIT(J6) 
         ACC_SQR = ACC_SQR + IONO_FIT(J6)**2
 460  CONTINUE 
!
      IONO_AVR = ACC_VAL/MOBS
      IONO_RMS = DSQRT (ACC_SQR/MOBS - IONO_AVR**2)
      STR = MJDSEC_TO_DATE ( MJD_BEG, TAI_BEG, IER )
!
      IF ( FL_PLOT ) THEN
           CALL DIAGI_SETDEF ( IER, 'DIAGI_CTIT', 'Zenith ionospheric path '// &
     &                   'delay at '//STA_NAM//' for '//STR(1:10) )
           CALL DIAGI_SETDEF ( IER, 'DIAGI_UNIT', 'Time in sec' )
           CALL DIAGI_2 ( MOBS, TIM, IONO_ZEN, MOBS, TIM, IONO_FIT, IER )
      END IF
!      
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  GNSS_IONO_RMS_STA !#!#
