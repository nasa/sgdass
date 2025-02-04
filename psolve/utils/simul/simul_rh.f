        SUBROUTINE SIMUL_RH ( SIMUL, VEX, STP, VTD, IVRB, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  SIMUL_RH 
! *                                                                      *
! *  ### 12-JUN-2020   SIMUL_RH    v1.1 (c)  L. Petrov  10-SEP-2021 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'solve.i'
      INCLUDE   'simul.i'
      INCLUDE   'stp.i'
      INCLUDE   'vtd.i'
      INCLUDE   'vex.i'
      TYPE     ( SIMUL__TYPE ) :: SIMUL
      TYPE     ( VEX_TYPE    ) :: VEX
      TYPE     ( STP__TYPE   ) :: STP
      TYPE     ( VTD__TYPE   ) :: VTD
      TYPE     ( VTD__OBS_TYPE ) :: OBS_TYP 
      TYPE     ( SIMUL__STAOBS_TYPE ) :: STA_OBS
      REAL*8     SIGMA_DEL, SIGMA_RAT, FREQ, SEFD(2,SIM__MBND), ELEV(2), &
     &           TSYS(2,SIM__MBND), GAIN(2,SIM__MBND), CORR_FD(SIM__MBND), &
     &           CORR_NOI(SIM__MBND), SNR(SIM__MBND)
      REAL*8       SIMUL__EL_MIN, SIMUL__GRDEL_ERR
      PARAMETER  ( SIMUL__EL_MIN = 3.0D0*DEG__TO__RAD )
      INTEGER*4  IVRB, IUER
      CHARACTER  C_SOU*8, C_STA(2)*8, BAND_MODE*8, STR*32, STR_DATE*30
      LOGICAL*1  FL_NEW_SCA
      INTEGER*4  J1, J2, J3, J4, J5, J6, J7, J8, J9, J10, KK, KP, IND_EL, &
     &           IS, ISTA_STP(2), &
     &           USER_SUP, USER_REC, AUTO_SUP, N_BND, N_IFS(SIM__MBND), &
     &           IND_IFS(VEX__MCHA,SIM__MBND), NOS(SIM__MSTA), &
     &           IND_STA_STP(SIM__MSTA), IND_STA_OBS(SIM__MSTA), IND_STA, IER
      REAL*8     CAL_AMP(SIM__MBND), TAI_OBS, TAU_GR, RATE_PH, TAU_GR_ERR(SIM__MBND), &
     &           DER_DEL(VTD__NDER), DER_RAT(VTD__NDER), TIM_VAL
      REAL*8,    ALLOCATABLE :: TIM_ARR(:), SPD_ARR(:,:), ELEV_ARR(:), &
     &                          AZIM_ARR(:), ZPD_DER(:)
      INTEGER*4, ALLOCATABLE :: IARR_TO_OBS(:,:)
      REAL*8     AZ(2), EL(2), HA(2), AZ_RATE(2), EL_RATE(2), HA_RATE(2), TIM_LAST
      REAL*8     DHSEG, DH, CN, H,  VN, VE
      REAL*8,    EXTERNAL :: RGAUSS
      INTEGER*4, EXTERNAL :: IFIND_PL, LTM_DIF, IXMN8
      CHARACTER, EXTERNAL :: MJDSEC_TO_DATE*30
!
      SIMUL%NBND = 1 ! temporary
      DHSEG  = 32.0D0*86400.0D0
      DH     =  200.0D0
      CN     = 2.4D-7
      H      = 2000.0D0
      VN     = 8.0D0
      VE     = 0.0D0
!
      OBS_TYP%PLRZ       = 'RR'             ! Polarization
      OBS_TYP%FRQ_REF(1) = 2.2D9            ! Ref. Freq. per band
      OBS_TYP%FRQ_REF(2) = 8.2D9
      OBS_TYP%N_BND      = 2                ! No. of bands
      OBS_TYP%DELAY_TYPE = VTD__MLMH__DTP   
      OBS_TYP%FRQ_ION_EFF(1) = 2.3D9        ! Effective Ionosphere freq.
      OBS_TYP%FRQ_ION_EFF(2) = 8.5D9
      OBS_TYP%EXP_NAME   = 'Test_01'
      OBS_TYP%SCAN_NAME  = 'Scan_0001'
      OBS_TYP%STATUS     = VTD__BND
!
      ALLOCATE ( TIM_ARR(SIMUL%NOBS),            &
     &           ELEV_ARR(SIMUL%NOBS),           &
     &           AZIM_ARR(SIMUL%NOBS),           &
     &           ZPD_DER(SIMUL%NOBS),            &
     &           SPD_ARR(SIMUL%NOBS,SIMUL%NSTA), &
     &           IARR_TO_OBS(SIMUL%NOBS,2)       )
!
      DO 410 J1=1,SIMUL%NSTA
         IND_STA_STP(J1) = LTM_DIF ( 0, STP%NSTA, STP%C_STA, SIMUL%STA_NAM(J1) )
         IF ( IND_STA_STP(J1) < 1 ) THEN
              CALL ERR_LOG ( 7731, IUER, 'SIMUL_RH', 'Station parameter file '// &
     &             SIMUL%STA_NAM(J1)//' was not found in the directory '// &
     &             TRIM(STP%DIR_NAM)//' . You may need assigne environment '// &
     &            'variable PSOLVE_STP_DIR' )
              RETURN 
         END IF
 410  CONTINUE 
      IF ( SIMUL%CNF%RH_MODE == 'nil'    .OR. &
     &     SIMUL%CNF%RH_MODE == 'cov'    .OR. &
     &     SIMUL%CNF%RH_MODE == 'covest'      ) THEN
!
           NOS = 0
           ALLOCATE ( STA_OBS%IND_SCA(SIMUL%NSCA,SIMUL%NSTA), &
     &                STA_OBS%NOBS_STA(SIMUL%NSTA),           &
     &                STA_OBS%EL(SIMUL%NSCA,SIMUL%NSTA),      &
     &                STA_OBS%AZ(SIMUL%NSCA,SIMUL%NSTA),      &
     &                STA_OBS%MJD(SIMUL%NSCA,SIMUL%NSTA),     &
     &                STA_OBS%UTC(SIMUL%NSCA,SIMUL%NSTA),     &
     &                STA_OBS%ZPD(SIMUL%NSCA,SIMUL%NSTA),     &
     &                STAT=IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 7732, IUER, 'SIMUL_RH', 'Error in allocation '// &
     &              'dynamic memory for STA_OBS object' )
                RETURN 
           END IF
!
           STA_OBS%IND_SCA  = 0
           STA_OBS%NOBS_STA = 0
           DO 420 J2=1,SIMUL%NOBS
!
! ----------- Here we recompute elevation angles since source positions in 
! ----------- vex may differ form source positions VTD uses
!
              C_SOU    = SIMUL%SOU_NAM(SIMUL%SOU_IND(J2))
              C_STA(1) = SIMUL%STA_NAM(SIMUL%STA_IND(1,J2))
              C_STA(2) = SIMUL%STA_NAM(SIMUL%STA_IND(2,J2))
              TAI_OBS  = SIMUL%UTC_OBS(J2) - SIMUL%UTC_MTAI
!
! ----------- Compute the VLBI Time Delay, and UVW derivatives
!
              CALL ERR_PASS  ( IUER, IER )
              CALL VTD_DELAY ( C_SOU, C_STA(1), C_STA(2), SIMUL%MJD_OBS(J2), &
     &                         SIMUL%UTC_OBS(J2), OBS_TYP, VTD, TAU_GR, &
     &                         RATE_PH, DER_DEL, DER_RAT, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 7735, IUER, 'SIMUL_RH', &
     &                 'Error in attempt to compute VLBI time delay ' )
                   RETURN 
              END IF
!
              SIMUL%EL(1,J2) = DER_DEL(VTD__ELEV1)
              SIMUL%EL(2,J2) = DER_DEL(VTD__ELEV2)
!
              IF ( SIMUL%EL(1,J2) > SIMUL__EL_MIN .AND. &
     &             SIMUL%EL(2,J2) > SIMUL__EL_MIN       ) THEN
!
                   DO 430 J3=1,2
                      IND_STA = SIMUL%STA_IND(J3,J2)
                      IF ( STA_OBS%NOBS_STA(IND_STA) == 0  ) THEN
                           FL_NEW_SCA = .TRUE.
                         ELSE IF ( IFIND_PL ( STA_OBS%NOBS_STA(IND_STA),  &
     &                                        STA_OBS%IND_SCA(1,IND_STA), &
     &                                        SIMUL%SCA_IND(J2) ) < 1     ) THEN
                           FL_NEW_SCA = .TRUE.
                         ELSE 
                           FL_NEW_SCA = .FALSE.
                      END IF
!
                      IF ( FL_NEW_SCA ) THEN
                           STA_OBS%NOBS_STA(IND_STA) = STA_OBS%NOBS_STA(IND_STA) + 1
                           STA_OBS%IND_SCA( STA_OBS%NOBS_STA(IND_STA), IND_STA ) = SIMUL%SCA_IND(J2)
                           STA_OBS%MJD    ( STA_OBS%NOBS_STA(IND_STA), IND_STA ) = SIMUL%MJD_OBS(J2)
                           STA_OBS%UTC    ( STA_OBS%NOBS_STA(IND_STA), IND_STA ) = SIMUL%UTC_OBS(J2)
                           STA_OBS%EL     ( STA_OBS%NOBS_STA(IND_STA), IND_STA ) = SIMUL%EL(J3,J2)
                           STA_OBS%AZ     ( STA_OBS%NOBS_STA(IND_STA), IND_STA ) = SIMUL%AZ(J3,J2)
                      END IF
 430               CONTINUE 
              END IF
 420       CONTINUE 
!
           DO 440 J4=1,SIMUL%NSTA
              IF ( STA_OBS%NOBS_STA(J4) == 0 ) THEN
                   CALL ERR_LOG ( 7733, IUER, 'SIMUL_RH', 'No observations at station '// &
     &                  SIMUL%STA_NAM(J4) )
                   RETURN 
              END IF
!
              IF ( SIMUL%CNF%RH_MODE == 'nil' ) THEN
                   CALL ERR_PASS  ( IUER, IER )
                   CALL SIMUL_EZD_NIL ( STA_OBS%MJD(1,J4), STA_OBS%UTC(1,J4), &
     &                              STA_OBS%NOBS_STA(J4), &
     &                              STA_OBS%AZ(1,J4), STA_OBS%EL(1,J4), &
     &                              DHSEG, DH, CN, H,  VN, VE, SIMUL%CNF%ISEED, &
     &                              STA_OBS%ZPD(1,J4), IER )
                   IF ( IER .NE. 0 ) THEN
                        CALL ERR_LOG ( 7733, IUER, 'SIMUL_RH', 'Error in attempt '// &
     &                      'to compute simulated VLBI time delay' )
                        RETURN 
                   END IF
                 ELSE IF ( SIMUL%CNF%RH_MODE == 'cov'    .OR. &
     &                     SIMUL%CNF%RH_MODE == 'covest'      ) THEN
                   CALL ERR_PASS  ( IUER, IER )
                   CALL SIMUL_EZD_COV ( SIMUL, J4, STA_OBS%NOBS_STA(J4),      &
     &                                  STA_OBS%MJD(1,J4), STA_OBS%UTC(1,J4), &
     &                                  STA_OBS%AZ(1,J4), STA_OBS%EL(1,J4),   &
     &                                  SIMUL%CNF%ISEED, STA_OBS%ZPD(1,J4), IER )
                   IF ( IER .NE. 0 ) THEN
                        CALL ERR_LOG ( 7733, IUER, 'SIMUL_RH', 'Error in attempt '// &
     &                      'to compute simulated VLBI time delay' )
                        RETURN 
                   END IF
              END IF
 440       CONTINUE 
      END IF
!
      IF ( SIMUL%ITYP == SIM__VEX ) THEN
           CALL ERR_PASS ( IUER, IER )
           CALL GET_BAND_RANGE ( VEX, 1, N_BND, N_IFS, IND_IFS, BAND_MODE, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 7734, IUER, 'SIMUL_RH', 'Error in dertermining '// &
     &               'the band range' )
                RETURN 
           END IF
!
          SIMUL%N_BNDS = N_BND
          DO 450 J5=1,N_BND
             SIMUL%NUM_IFS(J5) = N_IFS(J5)
             KK = 0
             DO 560 J6=1,N_IFS(J5)
                KK = KK + 1
                SIMUL%FRQ_IF(KK) = VEX%FRQ(1)%SKY_FRQ(KK)
                SIMUL%IND_FRQ_STA(J6,J5,1) = IND_IFS(J6,J5)
 560         CONTINUE 
 450      CONTINUE 
        ELSE
          CALL CLRCH ( BAND_MODE )
      END IF
!
      IF ( IVRB .GE. 2 ) THEN
           STR = MJDSEC_TO_DATE ( SIMUL%MJD_OBS(1), SIMUL%UTC_OBS(1) - SIMUL%UTC_MTAI, IER )
           WRITE ( 6, * ) 'SIMUL_RH First observation: '//STR(1:24)
      END IF
      DO 460 J6=1,SIMUL%NOBS
!
! ------ SIMUL%STA_IND(1,J6) is the index of the J6-station from the VEX station list
!
         C_SOU    = SIMUL%SOU_NAM(SIMUL%SOU_IND(J6))
         C_STA(1) = SIMUL%STA_NAM(SIMUL%STA_IND(1,J6))
         C_STA(2) = SIMUL%STA_NAM(SIMUL%STA_IND(2,J6))
         TAI_OBS  = SIMUL%UTC_OBS(J6) - SIMUL%UTC_MTAI
!
! ------ Compute the VLBI Time Delay, and UVW derivatives
!
         CALL ERR_PASS  ( IUER, IER )
         CALL VTD_DELAY ( C_SOU, C_STA(1), C_STA(2), SIMUL%MJD_OBS(J6), &
     &                    TAI_OBS, OBS_TYP, VTD, TAU_GR, RATE_PH, DER_DEL, &
     &                    DER_RAT, IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 7735, IUER, 'SIMUL_RH', &
     &            'Error in attempt to compute VLBI time delay ' )
              RETURN 
         END IF
!
! ------ We do not want to include source structure delay into simulation
!
         TAU_GR = TAU_GR - DER_DEL(VTD__STRUC) 
         ELEV(1) = DER_DEL(VTD__ELEV1)
         ELEV(2) = DER_DEL(VTD__ELEV2)
         SIMUL%ZPD_DER(1,J6) = DER_DEL(VTD__DER_AT1)
         SIMUL%ZPD_DER(2,J6) = DER_DEL(VTD__DER_AT2)
!
! ------ Grab the Correlated Amplitude from the DER_DEL Derived Type
! ------ NB: -- In the case of wanting to use the lower frequency, 
! ------ grab VTD__COR_FD2
!
         CAL_AMP(1) =  DER_DEL(VTD__COR_FD1)
         CAL_AMP(2) =  DER_DEL(VTD__COR_FD2)
!
         IF ( SIMUL%CNF%RH_MODE == 'wht' ) THEN
              SIGMA_DEL = SIMUL%CNF%WHT_RMS
              SIGMA_RAT = 1.D-14
              SNR = 10.0D0
              FREQ = 8.2D9
!
              SIMUL%GR_DEL(1,J6) = TAU_GR  + RGAUSS ( SIMUL%CNF%ISEED, SIGMA_DEL )
              SIMUL%PH_RAT(1,J6) = RATE_PH + RGAUSS ( SIMUL%CNF%ISEED, SIGMA_RAT )
              SIMUL%GR_DEL_ERR(1,J6) = SIGMA_DEL
              SIMUL%PH_RAT_ERR(1,J6) = SIGMA_RAT
              SIMUL%SNR(1:SIM__MBND,J6) = SNR(1:SIM__MBND)
              SIMUL%EFF_FREQ(1,1,J6) = FREQ
              SIMUL%EFF_FREQ(2,1,J6) = FREQ
              SIMUL%EFF_FREQ(3,1,J6) = FREQ
              SIMUL%REF_FREQ(1,J6) = FREQ
              SEFD(1:2,1)  = 0.1
            ELSE IF ( SIMUL%CNF%RH_MODE == 'ima'    .OR. &
     &                SIMUL%CNF%RH_MODE == 'nil'    .OR. &
     &                SIMUL%CNF%RH_MODE == 'cov'    .OR. &
     &                SIMUL%CNF%RH_MODE == 'covest'      ) THEN
!
! ----------- Compute the Station SEFDs at given freq, and elevation.
!
              IF ( SIMUL%ITYP == SIM__VEX ) THEN
                   ISTA_STP(1) = IND_STA_STP(SIMUL%STA_IND(1,J6))
                   ISTA_STP(2) = IND_STA_STP(SIMUL%STA_IND(2,J6))
                   CALL ERR_PASS( IUER, IER )
                   CALL STP_SNR ( VEX, STP, ISTA_STP, SIMUL%SCA_IND(J6), C_SOU, &
     &                            SIMUL%MJD_OBS(J6), TAI_OBS + SIMUL%UTC_MTAI, &
     &                            SIMUL%SCAN_DUR(J6), N_IFS, IND_IFS, &
     &                            BAND_MODE, ELEV, CAL_AMP(1), CORR_FD, CORR_NOI, &
     &                            TSYS, GAIN, SEFD, SNR, IER )
                   IF ( IER .NE. 0 ) THEN 
                        CALL CLRCH ( STR ) 
                        CALL INCH  ( J6, STR )
                        CALL ERR_LOG ( 7736, IUER, 'SIMUL_RH', &
     &                      'Failed to compute SNR for stations '// &
     &                       STP%STA(IND_STA_STP(SIMUL%STA_IND(1,J6)))%NAME//' '// &
     &                       STP%STA(IND_STA_STP(SIMUL%STA_IND(2,J6)))%NAME// &
     &                      ' while processing observation '//TRIM(STR)// &
     &                      ' scan name '//SIMUL%SCAN_NAME(J6)  )
                        RETURN 
                   END IF
!
                   CALL ERR_PASS( IUER, IER )
                   CALL STP_OBS_ERR ( VEX, STP, IND_STA_STP, C_SOU, SNR, &
     &                                N_IFS, IND_IFS, BAND_MODE, TAU_GR_ERR, IER )
!
                   IF ( IER .NE. 0 ) THEN 
                        CALL ERR_LOG ( 7737, IUER, 'SIMUL_RH', &
     &                      'Failed to compute group delay uncertainty for stations '// &
     &                       STP%STA(IND_STA_STP(SIMUL%STA_IND(1,J6)))%NAME//' '// &
     &                       STP%STA(IND_STA_STP(SIMUL%STA_IND(2,J6)))%NAME )
                        RETURN 
                   END IF
!                 
                   SIMUL%GR_DEL(1,J6) = TAU_GR
                   SIMUL%PH_RAT(1,J6) = RATE_PH
!
                   SIMUL%GR_DEL_ERR(1:2,J6) = TAU_GR_ERR(1:2)
                   SIMUL%SNR(1,J6) = SNR(1)
                   SIMUL%AMP(1:2,J6) = CORR_FD(1:2)
                   SIMUL%NOI(1:2,J6) = CORR_NOI(1:2)
                 ELSE
                   SEFD(1:2,1)  = 0.1
                   TAU_GR_ERR = SIMUL%GR_DEL_ERR(1,J6)
              END IF
         END IF
!
         IF ( SIMUL%CNF%RH_MODE == 'nil' .OR. &
     &        SIMUL%CNF%RH_MODE == 'cov' .OR. &
     &        SIMUL%CNF%RH_MODE == 'covest'   ) THEN
!
! ----------- Compute errors of mapping function
!
              DO 470 J7=1,2
                 IND_STA = SIMUL%STA_IND(J7,J6)
                 IND_STA_OBS(J7) = IFIND_PL ( STA_OBS%NOBS_STA(IND_STA), &
     &                                        STA_OBS%IND_SCA(1,IND_STA), &
     &                                        SIMUL%SCA_IND(J6) )
                 IF ( IND_STA_OBS(J7) < 1 ) THEN
                      CALL ERR_LOG ( 7736, IUER, 'SIMUL_RH', 'Trap of internal '// &
     &                    'control: IND_STA_OBS is zero for station '//SIMUL%STA_NAM(IND_STA) )
                      RETURN 
                 END IF
                 IF ( J7 == 1 ) THEN
                      IF ( ELEV(1) > SIMUL__EL_MIN ) THEN
                           SIMUL%ZPD(J7,J6) = STA_OBS%ZPD(IND_STA_OBS(J7),IND_STA)
                           IND_EL = IXMN8 ( SIMUL%COV(IND_STA)%NE, SIMUL%COV(IND_STA)%EL_MF, ELEV(1) )
                           SIMUL%SPD(J7,J6) = STA_OBS%ZPD(IND_STA_OBS(J7),IND_STA)*DER_DEL(VTD__DER_AT1) + &
     &                                        SIMUL%CNF%DIL_MAP_ERR * RGAUSS ( SIMUL%CNF%ISEED, &
     &                                                                         SIMUL%COV(IND_STA)%MF_RMS(IND_EL) )
                         ELSE
                           SIMUL%ZPD(J7,J6) = 0.D0
                           SIMUL%SPD(J7,J6) = 0.D0
                      END IF
                    ELSE
                      IF ( ELEV(2) > SIMUL__EL_MIN ) THEN
                           SIMUL%ZPD(J7,J6) = STA_OBS%ZPD(IND_STA_OBS(J7),IND_STA)
                           IND_EL = IXMN8 ( SIMUL%COV(IND_STA)%NE, SIMUL%COV(IND_STA)%EL_MF, ELEV(2) )
                           SIMUL%SPD(J7,J6) = STA_OBS%ZPD(IND_STA_OBS(J7),IND_STA)*DER_DEL(VTD__DER_AT2) + &
     &                                        SIMUL%CNF%DIL_MAP_ERR * RGAUSS ( SIMUL%CNF%ISEED, &
     &                                                                         SIMUL%COV(IND_STA)%MF_RMS(IND_EL) )
                         ELSE
                           SIMUL%ZPD(J7,J6) = 0.D0
                           SIMUL%SPD(J7,J6) = 0.D0
                      END IF
                 END IF
 470          CONTINUE 
              SIGMA_RAT = 1.D-14
              SIMUL%GR_DEL(1,J6) = TAU_GR  + RGAUSS ( SIMUL%CNF%ISEED, SIMUL%CNF%WHT_RMS ) + &
     &                                       SIMUL%SPD(1,J6) + SIMUL%SPD(2,J6)
              SIMUL%PH_RAT(1,J6) = RATE_PH + RGAUSS ( SIMUL%CNF%ISEED, SIGMA_RAT )
              SIMUL%GR_DEL_ERR(1,J6) = DSQRT ( SIMUL%CNF%WHT_RMS**2 + &
     &                                         SIMUL%CNF%DIL_OBS_NOISE*TAU_GR_ERR(1)**2 )
              SIMUL%PH_RAT_ERR(1,J6) = SIGMA_RAT
         END IF
!
         SIMUL%USER_SUP(J6) = 0
         SIMUL%USER_REC(J6) = 0
         SIMUL%USER_SUP(J6) = IBSET ( SIMUL%USER_SUP(J6), GOOD__SPS  )
         SIMUL%USER_SUP(J6) = IBSET ( SIMUL%USER_SUP(J6), INIT__SPS  )
         SIMUL%USER_REC(J6) = IBSET ( SIMUL%USER_SUP(J6), INIT__UAS )
         SIMUL%AUTO_SUP(J6) = SIMUL%USER_SUP(J6)
         SIMUL%QUALCODE(J6) = '9'
!
         IF ( IVRB .GE. 2 .AND. SIMUL%CNF%RH_MODE .NE. 'covest') THEN
              CALL ERR_PASS ( IUER, IER )
              STR_DATE = MJDSEC_TO_DATE ( SIMUL%MJD_OBS(J6), TAI_OBS, IER )
              IF ( IER .NE. 0 ) THEN 
                   WRITE ( 6, * ) 'MJD_OBS= ', SIMUL%MJD_OBS(J6), ' TAI= ', TAI_OBS
                   CALL ERR_LOG ( 7738, IUER, 'SIMUL_RH', 'Wrong MJD/TAI ' )
                   RETURN 
              END IF
!
              WRITE ( 6, 110 ) J6, BAND_MODE, C_STA(1), C_STA(2), C_SOU, &
     &                         CAL_AMP(1:2), SEFD(1:2,1), SIMUL%SNR(1,J6), &
     &                         SIMUL%GR_DEL_ERR(1,J6), STR_DATE(1:24), TAU_GR, &
     &                         SIMUL%EL(1,J6)/DEG__TO__RAD, SIMUL%EL(2,J6)/DEG__TO__RAD, &
     &                         1.D12*SIMUL%ZPD(1,J6), 1.D12*SIMUL%ZPD(2,J6), &
     &                         1.D12*SIMUL%SPD(1,J6), 1.D12*SIMUL%SPD(2,J6)
 110          FORMAT ( 'Obs: ', I7, 1X, A, ' Sta: ', A, 1X, A, &
     &                 ' Sou: ', A, ' Cal_amp: ', F7.4, 1X, F7.4 ' Jy  SEFD: ', &
     &                 F8.1, 1X, F8.1, ' Jy SNR= ', F7.1, '  TAU_GR_err= ', 1PD11.4, &
     &                 ' Date: ', A, ' TAU_OBS: ',  1PD19.12, &
     &                 ' El: ', 0PF6.2, 1X, 0PF6.2, ' Zpd: ', 0PF9.1, 1X, 0PF9.1, ' ps ', &
     &                                              ' Spd: ', 0PF9.1, 1X, 0PF9.1, ' ps '  )
         END IF
  460 CONTINUE 
!
      IF ( SIMUL%CNF%RH_MODE == 'covest' ) THEN
           DO 480 J8=1,SIMUL%NSTA
              KP = 0
              DO 490 J9=1,SIMUL%NOBS
                 TIM_VAL = (SIMUL%MJD_OBS(J9) - SIMUL%MJD_OBS(1))*86400.0D0 + &
     &                     (SIMUL%UTC_OBS(J9) - SIMUL%MJD_OBS(1))
                 IF ( SIMUL%STA_IND(1,J9) == J8 ) THEN
                      IF ( KP == 0 ) THEN
                           KP = KP + 1
                           TIM_ARR(KP)    = TIM_VAL
                           SPD_ARR(KP,J8) = SIMUL%SPD(1,J9)
                           ZPD_DER(KP)    = SIMUL%ZPD_DER(1,J9)
                           ELEV_ARR(KP)   = SIMUL%EL(1,J9)
                           AZIM_ARR(KP)   = SIMUL%AZ(1,J9)
                         ELSE
                           IF ( TIM_VAL > TIM_ARR(KP) + 1.D-3 ) THEN
                                KP = KP + 1
                                TIM_ARR(KP)    = TIM_VAL
                                SPD_ARR(KP,J8) = SIMUL%SPD(1,J9)
                                ZPD_DER(KP)    = SIMUL%ZPD_DER(1,J9)
                                ELEV_ARR(KP)   = SIMUL%EL(1,J9)
                                AZIM_ARR(KP)   = SIMUL%AZ(1,J9)
                           END IF
                      END IF
                      IARR_TO_OBS(J9,1) = KP
                    ELSE IF ( SIMUL%STA_IND(2,J9) == J8 ) THEN
                      IF ( KP == 0 ) THEN
                           KP = KP + 1
                           TIM_ARR(KP)    = TIM_VAL
                           SPD_ARR(KP,J8) = -SIMUL%SPD(2,J9)
                           ZPD_DER(KP)    = -SIMUL%ZPD_DER(2,J9)
                           ELEV_ARR(KP)   = SIMUL%EL(2,J9)
                           AZIM_ARR(KP)   = SIMUL%AZ(2,J9)
                         ELSE
                           IF ( TIM_VAL > TIM_ARR(KP) + 1.D-3 ) THEN
                                KP = KP + 1
                                TIM_ARR(KP)    = TIM_VAL
                                SPD_ARR(KP,J8) = -SIMUL%SPD(2,J9)
                                ZPD_DER(KP)    = -SIMUL%ZPD_DER(2,J9)
                                ELEV_ARR(KP)   = SIMUL%EL(2,J9)
                                AZIM_ARR(KP)   = SIMUL%AZ(2,J9)
                           END IF
                      END IF
                      IARR_TO_OBS(J9,2) = KP
                 END IF
 490          CONTINUE 
!
              IF ( KP > 4 ) THEN
                   CALL ERR_PASS ( IUER, IER )
                   CALL SIMUL_ATM_EST_UPDATE ( KP, TIM_ARR, SPD_ARR(1,J8), ZPD_DER, IER )
                   IF ( IER .NE. 0 ) THEN 
                        CALL ERR_LOG ( 7739, IUER, 'SIMUL_RH', 'Error in an attempt to '// &
     &                      'update slant path delay by fitting to a B-spline for '// &
     &                      'station '//SIMUL%STA_NAM(J8) )
                        RETURN 
                   END IF
              END IF
 480       CONTINUE 
!
           DO 4100 J10=1,SIMUL%NOBS
              SIMUL%GR_DEL(1,J10) = SIMUL%GR_DEL(1,J10) - SIMUL%SPD(1,J10) - SIMUL%SPD(2,J10)
              SIMUL%SPD(1,J10) =  SPD_ARR(IARR_TO_OBS(J10,1),SIMUL%OBSTAB(2,J10))
              SIMUL%SPD(2,J10) = -SPD_ARR(IARR_TO_OBS(J10,2),SIMUL%OBSTAB(3,J10))
              SIMUL%GR_DEL(1,J10) = SIMUL%GR_DEL(1,J10) + SIMUL%SPD(1,J10) + SIMUL%SPD(2,J10)
              C_SOU = SIMUL%SOU_NAM(SIMUL%SOU_IND(J10))
              C_STA(1) = SIMUL%STA_NAM(SIMUL%STA_IND(1,J10))
              C_STA(2) = SIMUL%STA_NAM(SIMUL%STA_IND(2,J10))
              TAI_OBS  = SIMUL%UTC_OBS(J10) - SIMUL%UTC_MTAI
              STR_DATE = MJDSEC_TO_DATE ( SIMUL%MJD_OBS(J10), TAI_OBS, IER )
!              
              IF ( IVRB .GE. 2 ) THEN
                   CALL ERR_PASS ( IUER, IER )
                   STR_DATE = MJDSEC_TO_DATE ( SIMUL%MJD_OBS(J10), TAI_OBS, IER )
                   IF ( IER .NE. 0 ) THEN 
                        WRITE ( 6, * ) 'MJD_OBS= ', SIMUL%MJD_OBS(J10), ' TAI= ', TAI_OBS
                        CALL ERR_LOG ( 7738, IUER, 'SIMUL_RH', 'Wrong MJD/TAI ' )
                        RETURN 
                   END IF
!
                   WRITE ( 6, 110 ) J10, BAND_MODE, C_STA(1), C_STA(2), C_SOU, &
     &                              CAL_AMP(1:2), SEFD(1:2,1), SIMUL%SNR(1,J10), &
     &                              SIMUL%GR_DEL_ERR(1,J10), STR_DATE(1:24), TAU_GR, &
     &                              SIMUL%EL(1,J10)/DEG__TO__RAD, SIMUL%EL(2,J10)/DEG__TO__RAD, &
     &                              1.D12*SIMUL%ZPD(1,J10), 1.D12*SIMUL%ZPD(2,J10), &
     &                              1.D12*SIMUL%SPD(1,J10), 1.D12*SIMUL%SPD(2,J10)
              END IF
 4100       CONTINUE 
      END IF
!
      IF ( SIMUL%CNF%RH_MODE == 'nil'    .OR. &
     &     SIMUL%CNF%RH_MODE == 'cov'    .OR. &
     &     SIMUL%CNF%RH_MODE == 'covest'      ) THEN
           DEALLOCATE ( STA_OBS%IND_SCA,  &
     &                  STA_OBS%NOBS_STA, &
     &                  STA_OBS%EL,       &
     &                  STA_OBS%AZ,       &
     &                  STA_OBS%MJD,      &
     &                  STA_OBS%UTC,      &
     &                  STA_OBS%ZPD       )
      END IF
      DEALLOCATE ( TIM_ARR, ELEV_ARR, AZIM_ARR, ZPD_DER, SPD_ARR, IARR_TO_OBS )
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  SIMUL_RH  !#!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE SIMUL_ATM_EST_UPDATE ( NP, TIM_ARR, SPD_ARR, ZPD_DER, &
     &                                  IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  SIMUL_ATM_EST_UPDATE
! *                                                                      *
! * ## 11-SEP-2021 SIMUL_ATM_EST_UPDATE v1.0 (c) L. Petrov 11-SEP-2021 # *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INTEGER*4  NP, IUER
      REAL*8     TIM_ARR(NP), SPD_ARR(NP), ZPD_DER(NP)
      REAL*8     TIM_INT, OVR_SHR
      PARAMETER  ( TIM_INT = 20.0D0*60.0D0 ) 
      PARAMETER  ( OVR_SHR = 0.0002D0 )
      REAL*8,    ALLOCATABLE :: EQU_VEC(:), EST_VEC(:), TIM_NOD(:), &
     &                          NOR_VEC(:), NOR_MAT(:)
      REAL*8     SIG_RAT
      PARAMETER  ( SIG_RAT = 1.0D0/TIM_INT )
      REAL*8     RC, RMS
      REAL*8     X1(8888)
      INTEGER*4  J1, J2, J3, J4, J5, J6, DEG, L_NOD, IER
      REAL*8,    EXTERNAL :: BSPL_VAL, BSPL_DER, DP_VV_V, EBSPL_VAL_R8 
      INTEGER*4, EXTERNAL :: I_LEN
!
      L_NOD = IDINT ( (TIM_ARR(NP) - TIM_ARR(1))/TIM_INT ) + 1
      DEG   = 1
      IF ( TIM_ARR(NP) - L_NOD*TIM_INT > OVR_SHR*TIM_INT ) THEN
           L_NOD = L_NOD + 1
      END IF
      ALLOCATE ( EQU_VEC(1-DEG:L_NOD-1), EST_VEC(1-DEG:L_NOD-1), &
     &           NOR_VEC(L_NOD), NOR_MAT(L_NOD*(L_NOD+1)/2), TIM_NOD(L_NOD) )
      NOR_VEC = 0.0D0
      NOR_MAT = 0.0D0
!
      DO 410 J1=1,L_NOD
         TIM_NOD(J1) = TIM_ARR(1) - 0.001 + (J1-1)*TIM_INT
 410  CONTINUE 
!
      DO 420 J2=1,NP
         EQU_VEC = 0.0D0
!
! ------ Compute observation equation
!
         DO 430 J3=1-DEG,L_NOD-1
            EQU_VEC(J3) = BSPL_VAL ( L_NOD, TIM_NOD, DEG, J3, TIM_ARR(J2) )*ZPD_DER(J2)
 430     CONTINUE 
!
! ------ Compute its contribution to NOR_MAT ( normal matrix ) 
!
         CALL DIAD_CVT_S ( 1.0D0, L_NOD, EQU_VEC, EQU_VEC, NOR_MAT )
!
! ------ ... and NOR_VEC ( normal vector )
!
         DO 440 J4=1,L_NOD
            NOR_VEC(J4) = NOR_VEC(J4) + EQU_VEC(J4-DEG)*SPD_ARR(J2)
 440     CONTINUE 
 420  CONTINUE 
!
! --- Set constrains on rate of change
!
      DO 450 J5=1,L_NOD-1
         EQU_VEC = 0.0D0
         EQU_VEC(J5-1) = -1.0D0/TIM_INT
         EQU_VEC(J5)   =  1.0D0/TIM_INT
         CALL DIAD_CVT_S ( 1.0D0/SIG_RAT**2, L_NOD, EQU_VEC, EQU_VEC, NOR_MAT )
 450  CONTINUE 
!
! --- Invert normal matrix
!
      CALL ERR_PASS ( IUER, IER )
      CALL INVS ( L_NOD, NOR_MAT, RC, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 1414, IUER, 'SIMUL_ATM_EST_UPDATE', 'Failure to invert '// &
     &         'normal matrix' )
           DEALLOCATE ( NOR_VEC, NOR_MAT, EQU_VEC, EST_VEC, TIM_NOD )
           RETURN 
      END IF
!
! --- Compute array of spline coefficients using normal vector and the 
! --- inverse to normnal matrix
!
      CALL MUL_MV_SV_V ( L_NOD, NOR_MAT, L_NOD, NOR_VEC, L_NOD, EST_VEC, -2 )
!
! --- Compute statstics of postfit residuals
!
      RMS = 0.D0
      DO 460 J6=1,NP
!!        x1(j6) = spd_arr(j6) 
         SPD_ARR(J6) = SPD_ARR(J6) - &
     &                 EBSPL_VAL_R8 ( L_NOD, DEG, TIM_ARR(J6), TIM_NOD, EST_VEC )*ZPD_DER(J6)
         RMS = RMS + SPD_ARR(J6)**2
 460  CONTINUE 
      IF ( NP > 1 ) THEN
           RMS = DSQRT ( RMS/NP )
      END IF
!
      DEALLOCATE ( NOR_VEC, NOR_MAT, EQU_VEC, EST_VEC, TIM_NOD )
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  SIMUL_ATM_EST_UPDATE  !#!#
