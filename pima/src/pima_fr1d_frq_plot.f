      SUBROUTINE PIMA_FR1D_FRQ_PLOT ( PIM, IND_OBS, FINAM_PLOT, LTIM, LCHN, &
     &                                LFRQ, DECOR, FREQ_ARR, FREQ_REF, WEI, &
     &                                UV, AC, AC_MEAN, TIME_FRT, GR_DEL, &
     &                                PH_RAT, GR_RAT, PH_ACC, PHAS, AMPL, &
     &                                SB_DEL, SNR, POL_BAS_LABEL, IDEV, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine PIMA_FR1D_FRQ_PLOT 
! *                                                                      *
! * ## 15-JAN-2006  PIMA_FR1D_FRQ_PLOT v4.8 (c) L. Petrov 26-NOV-2020 ## *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'pima.i'
      INCLUDE   'diagi.i'
      CHARACTER  FINAM_PLOT*(*), POL_BAS_LABEL*(*)
      TYPE     ( PIMA__TYPE  ) :: PIM
      TYPE     ( DIAGI_STRU  ) :: DIA(2)
      INTEGER*4  IND_OBS, LTIM, LCHN, LFRQ, IDEV, IUER
      REAL*8     FREQ_ARR(LCHN,LFRQ), FREQ_REF, TIME_FRT, GR_DEL, &
     &           PH_RAT, GR_RAT, PH_ACC, PHAS, AMPL, SB_DEL, SNR, DECOR
      COMPLEX*8  UV(PIM%NCHN,LFRQ,LTIM), AC(PIM%NCHN,LFRQ,LTIM,2)
      REAL*4     WEI(LTIM), AC_MEAN(PIM__MFRQ,2)
      INTEGER*4  MARR, MPB
      PARAMETER  ( MARR = PIM__MCHN )
      PARAMETER  ( MPB  =         4 )
      REAL*4     PHS_MOD, AMPL_FRQ(LCHN,LFRQ)
      REAL*8     ARG(LTIM,LFRQ), VAL(LTIM,LFRQ)
      REAL*8     T8(MARR), X8(MARR), Y8(MARR), W8(MARR), Z8(MARR), GRD_INIT, &
     &           GRD_STEP, &
     &           GRD_VAL, RAT_INIT, RAT_STEP, RAT_VAL, WPOI, T8_MEAN, &
     &           DR_VAL, SH_VAL, DR_SIG, SH_SIG, X8_MIN, X8_MAX, FRQ_PLOT, &
     &           TIM_VAL, RMS, PHS_MIN, PHS_MAX, AMP_MIN, AMP_MAX, &
     &           NRML_AUTC, NRML_AUTC_FRNG, NRML_AUTC_SPLT(2), &
     &           NRML_CROSS_FRNG, NRML_CROSS_SPLT, &
     &           AMPL_AUTO_INTG, BAND_AUTO_INTG, AVER_AUTO, &
     &           AMPL_CROS_INTG, BAND_CROS_INTG, AVER_CROS, &
     &           EXTRA_GDEL, EXTRA_SDEL, EXTRA_PRAT, EXTRA_GRAT
      COMPLEX*8  DRF, DRF_CHN
      INTEGER*4  IBST, ILST, IOST, IPST, IWST, ICL1, ICL2, ICL3, &
     &           IND_STA(2), IND_SOU, KP, KR, J1, J2, J3, J4, J5, J6, J7, J8, &
     &           KSEG, ICODE, NC, NR, IV8(MARR), IP, NP, NN, IDEV_DEF, &
     &           MSEG, UV_IND, FRG_IND, NO, ICHN_1ST, ICHN_LAST, &
     &           KP_BEG, KP_END, IER
      INTEGER*4  I1, I2
      CHARACTER  COMMON_TIT*80, TITS(PIM__MSTA)*80, BUTTON_NAME(MPB)*24, &
     &           BUTTON_LET(MPB)*2, PREF_NAME*128, ZAG*128, UNIT*128, STR*128, &
     &           STR1*32, POL_STR*8, FINAM_TXT_PLOT*128
      LOGICAL*1  FL_NOFRINGE, FL_ONLY_ACPL, FL_SB_DEL, FL_NRML, FL_ACPL, &
     &           FL_SMOOTH, FL_ACPL_PROD, FL_FLAT, FL_LOG
      CHARACTER*128, ALLOCATABLE :: OUT(:)
      DATA      ( BUTTON_LET(NN), BUTTON_NAME(NN), NN=1,MPB ) &
     &          / &
     &          '  ', '    ',  &
     &          'Qq', 'Quit',  &
     &          '  ', '    ',  &
     &          '  ', '    '   &
     &          /
      REAL*8,    EXTERNAL :: PIMA_AUTC_AMPL_NRML, PIMA_BPASS_RENRML
      REAL*4,    EXTERNAL :: PHAS_CMPL_R4 
      LOGICAL*4, EXTERNAL :: IS_R4_NAN 
      INTEGER*4, EXTERNAL :: ILEN, I_LEN
      CHARACTER, EXTERNAL :: GET_CDATE*19
#ifdef NO_PLOT
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  PIMA_FR1D_PLOT  !#!  
#else
      MSEG = PIM%CONF%FRIB_1D_FRQ_MSEG
!
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!  gr_del = 637856.22d-12 + 1.d-9   ! %%%%%%%%%%%%%%%%%%%
!  phas   = phas  + 2.0d0           ! %%%%%%%%%%%%%%%%%%%
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      FL_NOFRINGE = .FALSE. 
      CALL GETENVAR ( 'PIMAVAR_FR1D_NOFRINGE', STR )
      IF ( STR(1:3) == 'YES' .OR. STR(1:3) == 'yes' ) THEN
           FL_NOFRINGE = .TRUE. 
           WRITE ( 6, * ) 'PIMA_FR1D_FRQ_PLOT is made ignoring fringe fitting results'
      END IF
!
      FL_SB_DEL = .FALSE. 
      CALL GETENVAR ( 'PIMAVAR_FR1D_SB_DEL', STR )
      IF ( STR(1:3) == 'YES' .OR. STR(1:3) == 'yes' ) THEN
           FL_SB_DEL = .TRUE. 
      END IF
!
      FL_NRML = .FALSE.
      CALL GETENVAR ( 'PIMAVAR_FR1D_NRML', STR )
      IF ( STR(1:3) == 'YES' .OR. STR(1:3) == 'yes' ) THEN
           FL_NRML = .TRUE. 
      END IF
!
      FL_FLAT = .FALSE.
      CALL GETENVAR ( 'PIMAVAR_FR1D_FLAT', STR )
      CALL TRAN ( 11, STR, STR )
      IF ( STR(1:3) == 'YES' ) THEN
           FL_FLAT = .TRUE.
      END IF
!
      FL_ACPL = .FALSE.
      CALL GETENVAR ( 'PIMAVAR_FR1D_ACPL', STR )
      IF ( STR(1:3) == 'YES' .OR. STR(1:3) == 'yes' ) THEN
           FL_ACPL = .TRUE. 
      END IF
!
      FL_ACPL_PROD = .FALSE.
      CALL GETENVAR ( 'PIMAVAR_FR1D_ACPL_PROD', STR )
      IF ( STR(1:3) == 'YES' .OR. STR(1:3) == 'yes' ) THEN
           FL_ACPL_PROD = .TRUE. 
      END IF
!
      FL_SMOOTH = .FALSE.
      CALL GETENVAR ( 'PIMAVAR_FR1D_SMOOTH', STR )
      IF ( STR(1:3) == 'YES' .OR. STR(1:3) == 'yes' ) THEN
           FL_SMOOTH = .TRUE. 
      END IF
!
      CALL GETENVAR ( 'PIMAVAR_FR1D_EXTRA_GDEL', STR )
      IF ( ILEN(STR) > 0 ) THEN
           READ ( UNIT=STR, FMT=* ) EXTRA_GDEL
           GR_DEL = GR_DEL + EXTRA_GDEL
      END IF
!
      CALL GETENVAR ( 'PIMAVAR_FR1D_EXTRA_SDEL', STR )
      IF ( ILEN(STR) > 0 ) THEN
           READ ( UNIT=STR, FMT=* ) EXTRA_SDEL
           SB_DEL = SB_DEL + EXTRA_SDEL
      END IF
!
      CALL GETENVAR ( 'PIMAVAR_FR1D_EXTRA_PRAT', STR )
      IF ( ILEN(STR) > 0 ) THEN
           READ ( UNIT=STR, FMT=* ) EXTRA_PRAT
           PH_RAT = PH_RAT + EXTRA_PRAT
      END IF
!
      CALL GETENVAR ( 'PIMAVAR_FR1D_EXTRA_GRAT', STR )
      IF ( ILEN(STR) > 0 ) THEN
           READ ( UNIT=STR, FMT=* ) EXTRA_GRAT
           GR_RAT = GR_RAT + EXTRA_GRAT
      END IF
!
      FL_LOG = .FALSE.
      CALL GETENVAR ( 'PIMAVAR_FR1D_LOG', STR )
      IF ( ILEN(STR) > 0 ) THEN
           FL_LOG = .TRUE. 
      END IF
!
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!  write ( 6, * ) 'PIMA_FR1D_FRQ_PLOT: ', ph_rat, gr_del, gr_rat ; call flush ( 6 ) ! %%%%%%%%%%%%%%
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!
! --- Setting defaults values of the plotting parameters
!
      CALL ERR_PASS   ( IUER, IER )
      CALL DIAGI_DEF  ( IBST, ILST, IOST, IPST, IWST, IDEV_DEF, ZAG, UNIT, &
     &                  ICL1, ICL2, ICL3, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 7581, IUER, 'PIMA_FR1D_FRQ_PLOT', 'Error in '// &
     &                    'setting default values for the plot' )
           RETURN
      END IF
      IF ( IDEV ==  0 ) IDEV = IDEV_DEF
      PREF_NAME = '/tmp/'
!
      KP = 0
      KR = 0
      RMS = 0
!
      FRG_IND = PIM%OBS(IND_OBS)%REF_FRG_INDS(PIM%CONF%FRQ_GRP)
      IF ( FRG_IND == 0 ) THEN
           WRITE ( 6, * ) 'No uv data for obs ', IND_OBS, ' frequency group ', &
     &                     PIM%CONF%FRQ_GRP
           CALL ERR_LOG ( 0, IUER )
           RETURN 
      END IF
!
      PHS_MIN =  1.0D30
      PHS_MAX = -1.0D30
      AMP_MIN =  1.0D30
      AMP_MAX = -1.0D30
      DO 410 J1=1,LFRQ
         DRF = ( 0.0, 0.0 )
         WPOI     = 0.0
         FRQ_PLOT = 0.0D0
         IP = 0
         KSEG = 0
         ICHN_1ST = 1
         KP_BEG = KP + 1
!
         DO 420 J2=1,LCHN
            IF ( PIM%BANDPASS_MASK_STYLE .NE. PIMA__NO ) THEN
                 IF ( PIM%BANDPASS_MASK(J2,PIM%CONF%BEG_FRQ+J1-1,PIM%OBS(IND_OBS)%STA_IND(1),PIMA__MASK_FRNG) == 0 ) GOTO 420
                 IF ( PIM%BANDPASS_MASK(J2,PIM%CONF%BEG_FRQ+J1-1,PIM%OBS(IND_OBS)%STA_IND(2),PIMA__MASK_FRNG) == 0 ) GOTO 420
            END IF
            DO 430 J3=1,LTIM
               IF ( J3 == 1 ) THEN
                    KR = KR + 1
                    Z8(KR) = 0.0
               END IF 
               UV_IND  = PIM%OBS(IND_OBS)%UV_IND(J3,FRG_IND)
               IF ( UV_IND == 0 ) THEN
                    CALL CLRCH ( STR  )
                    CALL CLRCH ( STR1 )
                    CALL INCH  ( IND_OBS, STR  )
                    CALL INCH  ( J1,      STR1 )
                    CALL ERR_LOG ( 7583, IUER, 'PIMA_FR1D_FRQ_PLOT', 'Trap of '// &
     &                  'internal control: cannot find UV-index for observation '// &
     &                   STR(1:I_LEN(STR))//' epoch '//STR1 )
                    RETURN
               END IF
!
               TIM_VAL = PIM%TIM_R8(PIM%UV_IND(UV_IND)%TIM_IND) - &
     &                   PIM%OBS(IND_OBS)%TIM_BEG
!
               IF ( IS_R4_NAN ( REAL ( UV(J2,J1,J3) ) ) .OR. &
     &              IS_R4_NAN ( IMAG ( UV(J2,J1,J3) ) )      ) THEN
                    WEI(J3) = 0.0
                    UV(J2,J1,J3) = CMPLX ( 0.0, 0.0 )
               END IF
!
! ------------ Check for insane UV values
!
               IF ( REAL ( UV(J2,J1,J3) ) >  PIMA__AMP_MAX .OR. &
     &              REAL ( UV(J2,J1,J3) ) < -PIMA__AMP_MAX .OR. &
     &              IMAG ( UV(J2,J1,J3) ) >  PIMA__AMP_MAX .OR. &
     &              IMAG ( UV(J2,J1,J3) ) < -PIMA__AMP_MAX      ) THEN
                    WEI(J3) = 0.0
                    UV(J2,J1,J3) = CMPLX ( 0.0, 0.0 )
               END IF
!
! ------------ We apply phase rotation due to the phase delay rate and group
! ------------ delay and conter-rotation due to the total phase. 
! ------------ As a result we will get the residual phase
!
               IF ( GR_RAT < -0.5D0 ) THEN
                    PHS_MOD = - PHAS &
     &                        + PH_RAT*PI2*FREQ_REF*(TIM_VAL - TIME_FRT) &
     &                        + GR_DEL*PI2*(FREQ_ARR(J2,J1)  - FREQ_REF) &
     &                        + PH_ACC*PI2*FREQ_REF*(TIM_VAL - TIME_FRT)**2/2.0D0
                    IF ( FL_SB_DEL ) THEN
                         PHS_MOD = - PHAS &
     &                        + PH_RAT*PI2*FREQ_REF*(TIM_VAL - TIME_FRT) &
     &                        + SB_DEL*PI2*(FREQ_ARR(J2,J1)  - FREQ_REF) &
     &                        + PH_ACC*PI2*FREQ_REF*(TIM_VAL - TIME_FRT)**2/2.0D0
                    END IF
                  ELSE 
                    PHS_MOD = - PHAS &
     &                        + PH_RAT*PI2*FREQ_REF*(TIM_VAL - TIME_FRT) &
     &                        + GR_DEL*PI2*(FREQ_ARR(J2,J1)  - FREQ_REF) &
     &                        + GR_RAT*PI2*(FREQ_ARR(J2,J1)  - FREQ_REF)* &
     &                                     (TIM_VAL - TIME_FRT)
                    IF ( FL_FLAT ) THEN
                         PHS_MOD = PHS_MOD - GR_DEL*PI2*(FREQ_ARR(J2,J1)  - FREQ_ARR(1,J1)) &
     &                                     - GR_RAT*PI2*(FREQ_ARR(J2,J1)  - FREQ_ARR(1,J1))* &
     &                                       (TIM_VAL - TIME_FRT)
                    END IF
                    IF ( FL_SB_DEL ) THEN
                         PHS_MOD = - PHAS &
     &                        + PH_RAT*PI2*FREQ_REF*(TIM_VAL - TIME_FRT) &
     &                        + SB_DEL*PI2*(FREQ_ARR(J2,J1)  - FREQ_REF) &
     &                        + GR_RAT*PI2*(FREQ_ARR(J2,J1)  - FREQ_REF)* &
     &                                     (TIM_VAL - TIME_FRT)
                    END IF
               END IF
               IF ( FL_NOFRINGE ) PHS_MOD = 0.0
               DRF = DRF + CMPLX ( WEI(J3), 0.0 )*UV(J2,J1,J3)* &
     &                     CMPLX ( COS(PHS_MOD), SIN(PHS_MOD) )/DECOR
               IF ( WEI(J3)                 > PIMA__AMP_MIN .AND. &
     &              ABS(REAL(UV(J2,J1,J3))) > PIMA__AMP_MIN .AND. &
     &              ABS(IMAG(UV(J2,J1,J3))) > PIMA__AMP_MIN       ) THEN
                    WPOI = WPOI + WEI(J3)
               END IF
 430        CONTINUE 
!
            KSEG = KSEG + 1
            FRQ_PLOT = FRQ_PLOT + (FREQ_ARR(J2,J1) - FREQ_REF)
            IF ( KSEG < MSEG .AND. J2 < LCHN ) GOTO 420
!
            IF ( WPOI > 1.D-30 ) THEN
                 DRF = DRF/WPOI
                 AMPL_FRQ(KSEG,J1) = ABS(DRF)
            END IF
            KP = KP + 1
            IF ( KP > MARR ) THEN
                 CALL CLRCH ( STR )
                 CALL INCH  ( MARR, STR ) 
                 CALL ERR_LOG ( 7584, IUER, 'PIMA_FR1D_FRQ_PLOT', &
     &               'Parameter MARR is too small: '//STR )
                 RETURN 
            END IF
            T8(KP) = FRQ_PLOT/KSEG
            X8(KP) = PHAS_CMPL_R4 ( DRF )
            IF ( X8(KP) >  PI__NUM ) X8(KP) = X8(KP) - PI2
            IF ( X8(KP) < -PI__NUM ) X8(KP) = X8(KP) + PI2
            Y8(KP) = ABS( DRF )
            IF ( FL_LOG ) THEN
                 Y8(KP) = DLOG10 ( MAX ( PIMA__AMP_MIN, Y8(KP) ) )
            END IF
            RMS = RMS + X8(KP)**2
!
            IF ( IP == 0 ) THEN
                 IP = KP
                 NP = 0
                 T8_MEAN = 0.0D0
            END IF
            NP = NP + 1
            IV8(NP) = 1
            W8(NP)  = (1.D-8 + Y8(KP))
            T8_MEAN = T8_MEAN + T8(KP) 
!
            PHS_MIN = MIN ( PHS_MIN, X8(KP) )
            PHS_MAX = MAX ( PHS_MAX, X8(KP) )
!
            AMP_MIN = MIN ( AMP_MIN, Y8(KP) )
            AMP_MAX = MAX ( AMP_MAX, Y8(KP) )
!
            DRF = ( 0.0, 0.0 )
            WPOI = 0.0
            FRQ_PLOT = 0.0D0
            KSEG = 0
 420     CONTINUE 
         IF ( PIM%CONF%DEBUG_LEVEL .GE. 7                     .AND. &
     &        PIM%BANDPASS_MASK_STYLE      .NE. PIMA__NO      .AND. &
     &        PIM%CONF%FRIB_AUTOCORR_CALIB .NE. PIMA__ACCR_NO       ) THEN
!
              NRML_AUTC_SPLT(1) = PIMA_AUTC_AMPL_NRML ( PIM, IND_OBS, INT(PIM%OBS(IND_OBS)%STA_IND(1),KIND=4), &
     &                                                  J1, LTIM, AC(1,1,1,1), WEI, &
     &                                                  PIM%CONF%FRIB_AUTOCORR_THRESHOLD, IER )
              NRML_AUTC_SPLT(2) = PIMA_AUTC_AMPL_NRML ( PIM, IND_OBS, INT(PIM%OBS(IND_OBS)%STA_IND(2),KIND=4), &
     &                                                  J1, LTIM, AC(1,1,1,2), WEI, &
     &                                                  PIM%CONF%FRIB_AUTOCORR_THRESHOLD, IER )
              WRITE ( 6, 110 ) PIM%CONF%BEG_FRQ+J1-1, NRML_AUTC_SPLT(1:2)
 110          FORMAT ( 'PIMA_FR1D_FRQ_PLOT IF: ', I3, ' NRML_AUTC_SPLT: ', F8.5, 1X, F8.5 )
         END IF
!
         KP_END = KP
         IF ( FL_SMOOTH ) THEN
              CALL ERR_PASS ( IUER, IER )
              CALL PIMA_AMPL_SMOOTH ( PIM, KP_END-KP_BEG+1, T8(KP_BEG), Y8(KP_BEG), .FALSE., IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 7585, IUER, 'PIMA_FR1D_FRQ_PLOT', &
     &                 'Error on cross-correlation amnplitude smoothing' )
                   RETURN 
              END IF
!
              CALL ERR_PASS ( IUER, IER )
              CALL PIMA_AMPL_SMOOTH ( PIM, PIM%NCHN, T8(KR-PIM%NCHN+1), &
     &                                Z8(KR-PIM%NCHN+1), .TRUE., IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 7586, IUER, 'PIMA_FR1D_FRQ_PLOT', &
     &                 'Error on cross-correlation amnplitude smoothing' )
                   RETURN 
              END IF
         END IF
         IF ( FL_NRML ) THEN
              AMPL_CROS_INTG = 0.0D0
              BAND_CROS_INTG = 0.0D0
              DO 440 J4=KP_BEG+1,KP_END
                  IF ( T8(J4) - (FREQ_ARR(1,J1) - FREQ_REF) .GE. PIM%CONF%SPLT_BPASS_NRML_RANGE(1)* &
     &                                              PIM%FRQ(1,PIM%CONF%FRQ_GRP)%BAND_WIDTH        .AND. &
     &                 T8(J4) - (FREQ_ARR(1,J1) - FREQ_REF) .LE. PIM%CONF%SPLT_BPASS_NRML_RANGE(2)* &
     &                                              PIM%FRQ(1,PIM%CONF%FRQ_GRP)%BAND_WIDTH ) THEN
                       AMPL_CROS_INTG = AMPL_CROS_INTG + (Y8(J4) + Y8(J4-1))*(T8(J4) - T8(J4-1))/2.0D0
                       IF ( Y8(J4) > PIMA__AMP_MIN .AND. Y8(J4-1) > PIMA__AMP_MIN ) THEN
                            BAND_CROS_INTG = BAND_CROS_INTG + (T8(J4) - T8(J4-1))
                       END IF
                 END IF
 440          CONTINUE
              AVER_CROS = AMPL_CROS_INTG/BAND_CROS_INTG
!
              AMPL_AUTO_INTG = 0.0D0
              BAND_AUTO_INTG = 0.0D0
              DO 450 J5=KP_BEG+1,KP_END !!! PIM%NCHN
                  IF ( T8(J5) - (FREQ_ARR(1,J1) - FREQ_REF) .GE. PIM%CONF%SPLT_BPASS_NRML_RANGE(1)* &
     &                                              PIM%FRQ(1,PIM%CONF%FRQ_GRP)%BAND_WIDTH .AND. &
     &                 T8(J5) - (FREQ_ARR(1,J1) - FREQ_REF) .LE. PIM%CONF%SPLT_BPASS_NRML_RANGE(2)* &
     &                                              PIM%FRQ(1,PIM%CONF%FRQ_GRP)%BAND_WIDTH ) THEN
                       AMPL_AUTO_INTG = AMPL_AUTO_INTG + (Z8(J5) + Z8(J5-1))*(T8(J5) - T8(J5-1))/2.0D0
                       IF ( Z8(J5) > PIMA__AMP_MIN .AND. Z8(J5-1) > PIMA__AMP_MIN ) THEN
                            BAND_AUTO_INTG = BAND_AUTO_INTG + (T8(J5) - T8(J5-1))
                       END IF
                 END IF
 450          CONTINUE 
              AVER_AUTO = AMPL_AUTO_INTG/BAND_AUTO_INTG
!
              IF ( BAND_AUTO_INTG > PIMA__MIN_FRQ .AND. BAND_CROS_INTG > PIMA__MIN_FRQ ) THEN
                   DO 460 J6=KP_BEG,KP_END
                      Y8(J6) = Y8(J6)*AVER_AUTO/AVER_CROS
 460               CONTINUE 
              END IF
         END IF
!
         IF ( PIM%CONF%DEBUG_LEVEL .GE. 7                   .AND. &
     &        PIM%CONF%BANDPASS_FILE .NE. PIMA__BPASS_NO    .AND. &
     &        PIM%CONF%BANDPASS_USE  .NE. PIMA__BPASS_PHS   .AND. &
     &        PIM%CONF%BANDPASS_USE  .NE. PIMA__BPASS_NO    .AND. &
     &        PIM%CONF%SPLT_BPASS_NRML_METHOD .EQ. PIMA__WEIGHTED ) THEN
!
              CALL ERR_PASS ( IUER, IER )
              NRML_CROSS_FRNG = PIMA_BPASS_RENRML ( PIM, INT(PIM%OBS(IND_OBS)%STA_IND,KIND=4), &
     &                                              J1+PIM%CONF%BEG_FRQ-1, &
     &                                              PIMA__POLAR_RR, PIMA__MASK_FRNG, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 7587, IUER, 'PIMA_FR1D_FRQ_PLOT', 'Error in bandpass '// &
     &                 'renormalization' )
                   RETURN 
              END IF
              IF ( PIM%BPASS(1)%PIMA_VERS .GE. PIMA__BPS_AMP_VERS ) THEN
                   NRML_CROSS_SPLT = &
     &                  PIM%BPASS(PIM%OBS(IND_OBS)%STA_IND(1))%SPLT_NRML_FRQ(J1+PIM%CONF%BEG_FRQ-1)* &
     &                  PIM%BPASS(PIM%OBS(IND_OBS)%STA_IND(2))%SPLT_NRML_FRQ(J1+PIM%CONF%BEG_FRQ-1)
                 ELSE
                   IF ( PIM%BPASS(PIM%OBS(IND_OBS)%STA_IND(1))%L_OBS > 0 .AND. &
     &                  PIM%BPASS(PIM%OBS(IND_OBS)%STA_IND(2))%L_OBS > 0       ) THEN
!
                        NRML_CROSS_SPLT = DSQRT ( &
     &                        PIM%BPASS(PIM%OBS(IND_OBS)%STA_IND(1))%SPLT_NRML_FRQ(J1+PIM%CONF%BEG_FRQ-1)* &
     &                        PIM%BPASS(PIM%OBS(IND_OBS)%STA_IND(2))%SPLT_NRML_FRQ(J1+PIM%CONF%BEG_FRQ-1) &
     &                                          )
                     ELSE IF ( PIM%BPASS(PIM%OBS(IND_OBS)%STA_IND(1))%L_OBS == 0 .AND. &
     &                         PIM%BPASS(PIM%OBS(IND_OBS)%STA_IND(2))%L_OBS >  0       ) THEN
                        NRML_CROSS_SPLT = PIM%BPASS(PIM%OBS(IND_OBS)%STA_IND(2))%SPLT_NRML_FRQ(J1+PIM%CONF%BEG_FRQ-1) 
                     ELSE IF ( PIM%BPASS(PIM%OBS(IND_OBS)%STA_IND(1))%L_OBS >  0 .AND. &
     &                         PIM%BPASS(PIM%OBS(IND_OBS)%STA_IND(2))%L_OBS == 0       ) THEN
                        NRML_CROSS_SPLT = PIM%BPASS(PIM%OBS(IND_OBS)%STA_IND(1))%SPLT_NRML_FRQ(J1+PIM%CONF%BEG_FRQ-1) 
                     ELSE 
                        NRML_CROSS_SPLT = 1.0
                   END IF
              END IF
!
              WRITE ( 6, 120 ) PIM%CONF%BEG_FRQ+J1-1, NRML_CROSS_FRNG, NRML_CROSS_SPLT
 120          FORMAT ( 'PIMA_FR1D_FRQ_PLOT IF: ', I3, ' NRML_CROSS_FRNG: ', F8.5, ' NRML_CROSS_SPL: ', F8.5 )
         END IF
 410  CONTINUE 
!
      IF ( KP > 0 ) THEN
           RMS = DSQRT ( RMS/KP ) 
         ELSE 
            RMS = 0.0D0
      END IF
      IF ( PIM%CONF%DEBUG_LEVEL .GE. 5 ) THEN
           WRITE ( 6, * ) 'PIMA_FR1D_FRQ_PLOT KP = ', KP, ' RMS = ', RMS
      END IF
!
      IND_STA(1) = PIM%OBS(IND_OBS)%STA_IND(1)
      IND_STA(2) = PIM%OBS(IND_OBS)%STA_IND(2)
      IND_SOU    = PIM%OBS(IND_OBS)%SOU_IND
!
      CALL CLRCH ( STR  ) 
      CALL CLRCH ( STR1 ) 
      CALL INCH  ( IND_OBS, STR1 )
      WRITE ( UNIT=STR(1:8), FMT='(F8.1)' ) SNR
      CALL CHASHL ( STR )
      COMMON_TIT = 'Fringe phs/amp for obs #'//STR1(1:I_LEN(STR1))// &
     &             ' Exp '//PIM%OBS_CODE(1:I_LEN(PIM%OBS_CODE))//' SNR='// &
     &             STR(1:ILEN(STR))
!
      X8_MIN = -PI__NUM
      X8_MAX =  PI__NUM
      DO 470 J7=1,2
         CALL NOUT ( SIZEOF(DIA(J7)), DIA(J7) )
         DIA(J7)%IDEV = IDEV
         IF ( J7 == 2 .AND. FL_ACPL ) THEN
              DIA(J7)%NCLR = 2
            ELSE
              DIA(J7)%NCLR = 1
         END IF
         DIA(J7)%NPOI(1)   = KP
         DIA(J7)%ADR_X8(1) = LOC(T8)
         IF ( J7 == 1 ) THEN
              DIA(J7)%ADR_Y8(1) = LOC(X8)
            ELSE IF ( J7 == 2 ) THEN
              DIA(J7)%ADR_Y8(1) = LOC(Y8)
              IF ( FL_ACPL ) THEN
                   DIA(J7)%NPOI(2)   = KR
                   DIA(J7)%ADR_X8(2) = LOC(T8)
                   DIA(J7)%ADR_Y8(2) = LOC(Z8)
              END IF
         END IF
         DIA(J7)%ADR_E8(1) = 0
         DIA(J7)%LER(1)    = .FALSE.
         DIA(J7)%ICOL(1)   = J7
         DIA(J7)%IBST(1)   = 0
         DIA(J7)%ILST(1)   = 2
         DIA(J7)%IOST(1)   = 1
         DIA(J7)%IWST(1)   = 1
         IF ( FL_ACPL ) THEN
              DIA(J7)%ADR_E8(2) = 0
              DIA(J7)%LER(2)    = .FALSE.
              DIA(J7)%ICOL(2)   = 3
              DIA(J7)%IBST(2)   = 0
              DIA(J7)%ILST(2)   = 2
              DIA(J7)%IOST(2)   = 1
              DIA(J7)%IWST(2)   = 2
         END IF 
         DIA(J7)%ICLR      = 1
         IF ( J7 == 1 ) THEN
              DIA(J7)%IPST(1) = 4
              DIA(J7)%ILST(1) = 1
              IF ( KP > 0 ) THEN
                   DIA(J7)%XMIN   =  T8(1)  - (T8(KP) - T8(1))*DIAGI_FIE 
                   DIA(J7)%XMAX   =  T8(KP) + (T8(KP) - T8(1))*DIAGI_FIE 
                   DIA(J7)%YMIN   =  X8_MIN - (X8_MAX - X8_MIN)*DIAGI_FIE 
                   DIA(J7)%YMAX   =  X8_MAX + (X8_MAX - X8_MIN)*DIAGI_FIE 
                 ELSE 
                   DIA(J7)%XMIN   =  0.0
                   DIA(J7)%XMAX   =  1.0
                   DIA(J7)%YMIN   =  0.0
                   DIA(J7)%YMAX   =  1.0
              END IF
            ELSE 
              DIA(J7)%IPST(1) = 4
              DIA(J7)%ILST(1) = 2
              IF ( FL_ACPL ) THEN
                   DIA(J7)%IPST(2) = 2
                   DIA(J7)%ILST(2) = 2
              END IF
              IF ( KP > 0 ) THEN
                   DIA(J7)%XMIN   =  1.0
                   DIA(J7)%XMAX   = -1.0
                   DIA(J7)%YMIN   =  1.0
                   DIA(J7)%YMAX   = -1.0
                 ELSE 
                   DIA(J7)%XMIN   =  0.0
                   DIA(J7)%XMAX   =  1.0
                   DIA(J7)%YMIN   =  0.0
                   DIA(J7)%YMAX   =  1.0
              END IF
         END IF
         CALL CLRCH ( STR ) 
         CALL INCH  ( MSEG, STR )
         DIA(J7)%ARG_UNITS = 'Mseg: '//STR(1:I_LEN(STR))//' Frequency in Hz'
         CALL CLRCH ( STR ) 
         CALL INCH  ( IND_OBS, STR )
         DIA(J7)%NAME   = FINAM_PLOT
         DIA(J7)%ITRM   = 0
         IF ( IDEV == 1  .OR. &
     &        IDEV == 2  .OR. &
     &        IDEV == 3  .OR. &
     &        IDEV == 4  .OR. &
     &        IDEV == 5  .OR. &
     &        IDEV == 6       ) THEN
              DIA(J7)%IBATCH = 0
            ELSE IF ( IDEV == 100 ) THEN
              IP = ILEN(FINAM_PLOT)
              FINAM_PLOT = FINAM_PLOT(1:IP-1)//'.sav'
              DIA%NAME   = FINAM_PLOT
              DIA(J7)%IBATCH = 2
            ELSE 
              DIA(J7)%IBATCH = 1
         END IF
         DIA(J7)%STATUS = DIA__DEF
         IF ( PIM%NPOL == 2 ) THEN 
              POL_STR = ' '//POL_BAS_LABEL//'-pol'
            ELSE 
              CALL CLRCH ( POL_STR )
         END IF
         IF ( J7 == 1 ) THEN
              TITS(J7) = PIM%CONF%BAND//'-band'// &
     &                   POL_STR(1:I_LEN(POL_STR))//' fringe phase for '// &
     &                   PIM%SOU(IND_SOU)%IVS_NAME// &
     &                   ' at '//PIM%STA(IND_STA(1))%IVS_NAME// &
     &                   '/'//PIM%STA(IND_STA(2))%IVS_NAME// &
     &                   ' in '//PIM%OBS_CODE
              DIA(J7)%ZAG  = PIM%CONF%BAND//'-band'// &
     &                       POL_STR(1:I_LEN(POL_STR))//' fringe phase for '// &
     &                       PIM%SOU(IND_SOU)%IVS_NAME// &
     &                       ' at '//PIM%STA(IND_STA(1))%IVS_NAME// &
     &                       '/'//PIM%STA(IND_STA(2))%IVS_NAME
            ELSE IF ( J7 == 2 ) THEN
              TITS(J7) = PIM%CONF%BAND//'-band'// &
     &                   POL_STR(1:I_LEN(POL_STR))//' fringe amplitude for '// &
     &                   PIM%SOU(IND_SOU)%IVS_NAME// &
     &                   ' at '//PIM%STA(IND_STA(1))%IVS_NAME// &
     &                   '/'//PIM%STA(IND_STA(2))%IVS_NAME// &
     &                   ' in '//PIM%OBS_CODE
              DIA(J7)%ZAG =  PIM%CONF%BAND//'-band'// &
     &                      POL_STR(1:I_LEN(POL_STR))//' fringe amplitude for '// &
     &                       PIM%SOU(IND_SOU)%IVS_NAME// &
     &                      ' at '//PIM%STA(IND_STA(1))%IVS_NAME// &
     &                      '/'//PIM%STA(IND_STA(2))%IVS_NAME
         END IF
         IF ( IDEV == -1 .AND. KP > 0 ) THEN
!
! ----------- Plot is written as a text file. In fact, we write two text
! ----------- tables: one for phase (J7=1) and another for amplitude (J7=2)
!
              IF ( J7 == 1 ) THEN
                   FINAM_TXT_PLOT = FINAM_PLOT(1:I_LEN(FINAM_PLOT))//'phs.txt'
                 ELSE IF ( J7 == 2 ) THEN
                   FINAM_TXT_PLOT = FINAM_PLOT(1:I_LEN(FINAM_PLOT))//'amp.txt'
              END IF
!
! ----------- Allocate memory for temporary buffer
!
              ALLOCATE   ( OUT(KP+32), STAT=IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 7588, IUER, 'PIMA_FR1D_FRQ_PLOT', 'Failure '// &
     &                            'to allocate memory for internal buffer '// &
     &                            'of the output text file' )
                   DEALLOCATE ( OUT )
                   RETURN 
              END IF
!
! ----------- Prepare table preamble
!
              OUT(1) = PIMA__TXT1D_LABEL
              OUT(2) = '#'
              OUT(2) = '# Plot of fringe phase and fringe amplitude versus frequency'
              OUT(2) = '#'
              OUT(3) = '# Generated on '//GET_CDATE()
              OUT(4) = '# Generated by '//PIMA__LABEL
              OUT(5) = '#'
!
! ----------- Write into the table auxiliary information
!
              NO = 5
              CALL CLRCH ( STR  ) 
              CALL CLRCH ( STR1 ) 
              CALL INCH  ( IND_OBS, STR1 )
              WRITE ( UNIT=STR(1:8), FMT='(F8.1)' ) SNR
              CALL CHASHL ( STR )
!
              IF ( J7 == 1 ) THEN
                   STR = 'Fringe phase for obs #'//STR1(1:I_LEN(STR1))// &
     &                   ' Exp '//PIM%OBS_CODE(1:I_LEN(PIM%OBS_CODE))//' SNR='// &
     &                   STR(1:ILEN(STR))
                 ELSE IF ( J7 == 2 ) THEN
                   STR = 'Fringe ampl for obs #'//STR1(1:I_LEN(STR1))// &
     &                   ' Exp '//PIM%OBS_CODE(1:I_LEN(PIM%OBS_CODE))//' SNR='// &
     &                   STR(1:ILEN(STR))
              END IF
              NO = NO + 1; OUT(NO) = 'PLOT_TITLE:  '//STR
              CALL INCH  ( MSEG, STR )
              NO = NO + 1; OUT(NO) = 'SUBTITLE:    Mseg: '//STR
              CALL INCH ( KP, STR )
              NO = NO + 1; OUT(NO) = 'NUM_POINTS:  '//STR
              NO = NO + 1; OUT(NO) = 'AXIS1_TITLE: Frequency'
              NO = NO + 1; OUT(NO) = 'AXIS1_UNITS: Hz'
!
              WRITE ( UNIT=STR(1:15), FMT='(F15.1)' ) T8(1)  
              CALL CHASHL ( STR )
              NO = NO + 1; OUT(NO) = 'AXIS1_MIN:   '//STR(1:15)
              WRITE ( UNIT=STR(1:15), FMT='(F15.1)' ) T8(KP)
              CALL CHASHL ( STR )
              NO = NO + 1; OUT(NO) = 'AXIS1_MAX:   '//STR(1:15)
!
              IF ( J7 == 1 ) THEN
                   NO = NO + 1; OUT(NO) = 'AXIS2_NAME:  Fringe phase'
                   NO = NO + 1; OUT(NO) = 'AXIS2_UNITS: rad'
                   WRITE ( UNIT=STR(1:15), FMT='(F10.7)' ) PHS_MIN
                   CALL CHASHL ( STR )
                   NO = NO + 1; OUT(NO) = 'AXIS2_MIN:   '//STR(1:15)
                   WRITE ( UNIT=STR(1:15), FMT='(F10.7)' ) PHS_MAX
                   CALL CHASHL ( STR )
                   NO = NO + 1; OUT(NO) = 'AXIS2_MAX:   '//STR(1:15)
                 ELSE IF ( J7 == 2 ) THEN
                   NO = NO + 1; OUT(NO) = 'AXIS2_NAME:  Normalized fringe ampitude'
                   NO = NO + 1; OUT(NO) = 'AXIS2_UNITS: dimensionless'
                   WRITE ( UNIT=STR(1:15), FMT='(F10.7)' ) AMP_MIN
                   CALL CHASHL ( STR )
                   NO = NO + 1; OUT(NO) = 'AXIS2_MIN:   '//STR(1:15)
                   WRITE ( UNIT=STR(1:15), FMT='(F10.7)' ) AMP_MAX
                   CALL CHASHL ( STR )
                   NO = NO + 1; OUT(NO) = 'AXIS2_MAX:   '//STR(1:15)
              END IF
              NO = NO + 1; OUT(NO) = 'AXIS2_ERRS:  no'
!
! ----------- Write the agrguments and values of plotting variables
!
              DO 480 J8=1,KP
                 NO = NO + 1
                 IF ( J7 == 1 ) THEN
                      WRITE  ( OUT(NO), 130 ) J8, T8(J8), X8(J8)
                    ELSE IF ( J7 == 2 ) THEN
                      WRITE  ( OUT(NO), 130 ) J8, T8(J8), Y8(J8)
                 END IF
 130             FORMAT ( 'POINT: ', I5, 1X, F16.3, 1X, F10.7 )
 480          CONTINUE 
              NO = NO + 1; OUT(NO) = '#'
              NO = NO + 1; OUT(NO) = PIMA__TXT1D_LABEL
!
! ----------- Write the buffer into file
!
              CALL ERR_PASS ( IUER, IER )
              CALL WR_TEXT  ( NO, OUT, FINAM_TXT_PLOT, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 7589, IUER, 'PIMA_FR1D_FRQ_PLOT', 'Failure '// &
     &                            'to write into the output table file '// &
     &                            FINAM_PLOT )
                   DEALLOCATE ( OUT )
                   RETURN 
              END IF
              WRITE ( 6, * ) 'File written '//FINAM_TXT_PLOT(1:I_LEN(FINAM_TXT_PLOT))
!
! ----------- Finally, do not forget to deallocate the buffer
!
              DEALLOCATE ( OUT )
         END IF
 470  CONTINUE 
!
      IF ( IDEV .NE. -1 ) THEN
           NC = 1
           NR = 2
           BUTTON_LET(1)  = 'Xx'
           BUTTON_NAME(1) = 'Exit plot'
!
           ICODE = 0
           CALL ERR_PASS ( IUER, IER )
           CALL MULTI_DIAGI ( COMMON_TIT, 2, NC, NR, TITS, MPB, &
     &                        BUTTON_NAME, BUTTON_LET, PREF_NAME, DIA, &
     &                        ICODE, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 7590, IUER, 'PIMA_FR1D_FRQ_PLOT', 'Failure '// &
     &                         'to make a plot of autocorrelation' )
                RETURN 
           END IF
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  PIMA_FR1D_FRQ_PLOT  !#!#
#endif
!
! ------------------------------------------------------------------------
!
      SUBROUTINE PIMA_AMPL_SMOOTH ( PIM, NP, FREQ, AMPL, FL_PC, IUER )
! ************************************************************************
! *                                                                      *
! *   Auxilliary routine PIMA_AMPL_SMOOTH
! *                                                                      *
! * ### 09-MAR-2017  PIMA_AMPL_SMOOTH v1.0 (c) L. Petrov 09-MAR-2017 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'pima.i'
      TYPE     ( PIMA__TYPE  ) :: PIM
      INTEGER*4  NP, IUER
      REAL*8     FREQ(NP), AMPL(NP)
      LOGICAL*1  FL_PC
      INTEGER*4  DEG
      REAL*8       CNS_VAL_SIG, CNS_DER_SIG, CNS_DR2_SIG, FREQ_EPS
      REAL*8       PIM__PCAL_FREQ, PIM__FRQ_TOL, PIM__WEI_DOWN
      PARAMETER  ( CNS_VAL_SIG  = 1.0D2   )
      PARAMETER  ( CNS_DER_SIG  = 1.0D-5  )
      PARAMETER  ( CNS_DR2_SIG  = 5.0D-12 )
      PARAMETER  ( FREQ_EPS = 1.D0  )
      PARAMETER  ( PIM__PCAL_FREQ = 1.0D6  )
      PARAMETER  ( PIM__FRQ_TOL   = 0.2D0  )
      PARAMETER  ( PIM__WEI_DOWN  = 1.0D-6 )
      PARAMETER  ( DEG = 3 )
      REAL*8     WEI(PIM__MCHN), POL_COEF(0:PIM__MCHN), &
     &           POL_COEF_ERR(0:PIM__MCHN), FREQ_NOD(PIM__MCHN), &
     &           SPL_VEC(1-DEG:PIM__MCHN), POSTFIT_AMP_WRMS
      INTEGER*4  J1, J2, J3, J4, J5, IER
      REAL*8,    EXTERNAL :: LEGENDRE_POL, EBSPL_VAL_R8 
!
      DO 410 J1=1,NP
         WEI(J1) = 1.0D0
         IF ( FL_PC .AND. DABS ( FREQ(J1) - PIM__PCAL_FREQ*IDNINT(FREQ(J1)/PIM__PCAL_FREQ) ) < &
     &                           PIM__FRQ_TOL*PIM%FRQ(1,PIM%CONF%FRQ_GRP)%CHAN_WIDTH ) THEN
              WEI(J1) = PIM__WEI_DOWN
         END IF
 410  CONTINUE 
      IF ( PIM%CONF%BPS_INTRP_METHOD == PIMA__LEGENDRE ) THEN
           CALL ERR_PASS ( IUER, IER )
           CALL LEGENDRE_REGR ( NP, FREQ, AMPL, WEI, PIM%CONF%BPS_DEG_AMP, &
     &                          POL_COEF, POL_COEF_ERR, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 7592, IUER, 'PIMA_AMPL_SMOOTH', 'Failure '// &
     &              'in LEGENDRE_REGR' )
                RETURN
           END IF
           DO 420 J2=1,NP
              AMPL(J2)= 0.0D0
              DO 430 J3=0,PIM%CONF%BPS_DEG_AMP
                 AMPL(J2)= AMPL(J2) + POL_COEF(J3)* &
     &                                LEGENDRE_POL ( J3, FREQ(1), FREQ(NP), FREQ(J2) )
 430          CONTINUE 
 420       CONTINUE 
         ELSE IF ( PIM%CONF%BPS_INTRP_METHOD == PIMA__SPLINE ) THEN
           DO 440 J4=1,PIM%CONF%BPS_DEG_AMP
              FREQ_NOD(J4) = FREQ(1) + (J4-1)*(FREQ(NP) - FREQ(1))/(PIM%CONF%BPS_DEG_AMP-1)
 440       CONTINUE 
           CALL EBSPL_WLSQ_CNS3 ( NP, FREQ, AMPL, WEI, PIM%CONF%BPS_DEG_AMP, DEG, &
     &                            FREQ_NOD, SPL_VEC, &
     &                            CNS_VAL_SIG, CNS_DER_SIG, CNS_DR2_SIG, &
     &                            POSTFIT_AMP_WRMS, IER )
           DO 450 J5=1,NP
              IF ( J5 == 1 ) THEN
                   AMPL(J5)= EBSPL_VAL_R8 ( PIM%CONF%BPS_DEG_AMP, DEG, &
     &                                      FREQ(J5) + FREQ_EPS, FREQ_NOD, SPL_VEC )
                 ELSE IF ( J5 == NP ) THEN
                   AMPL(J5)= EBSPL_VAL_R8 ( PIM%CONF%BPS_DEG_AMP, DEG, &
     &                                      FREQ(J5) - FREQ_EPS, FREQ_NOD, SPL_VEC )
                 ELSE
                   AMPL(J5)= EBSPL_VAL_R8 ( PIM%CONF%BPS_DEG_AMP, DEG, &
     &                                      FREQ(J5),            FREQ_NOD, SPL_VEC )
              END IF
 450       CONTINUE 
         ELSE
           CALL ERR_LOG ( 7593, IUER, 'PIMA_AMPL_SMOOTH', 'Cannot perform '// &
     &         'smoothing  with BPS_INTRP_METHOD: '//PIM%CONF%BPS_INTRP_METHOD )
           RETURN 
      END IF
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  PIMA_AMPL_SMOOTH  !#!  
