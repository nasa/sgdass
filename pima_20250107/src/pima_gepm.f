      SUBROUTINE PIMA_GEPM ( PIM, STA_NAM, T_ACCUM, GEPM_OVR, TIME_THRESH, &
     &                       DIFF_THRESH, MAX_COUNT, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine PIMA_GEPM 
! *                                                                      *
! *  ### 21-JUN-2022  PIMA_GEPM   v1.0 (c)              21-JUN-2022 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'astro_constants.i'
      INCLUDE   'pima.i'
      INCLUDE   'diagi.i'
      TYPE     ( PIMA__TYPE  ) :: PIM
      CHARACTER  STA_NAM*(*)
      LOGICAL*4  GEPM_OVR
      INTEGER*4  IUER
      INTEGER*4  MPB, MP, NN, MDEG, N_KNOT, SIG_NUM, FAIL_AMP, FAIL_CPDIFF, FAIL_Y8DIFF, NUM_SUM, MAX_COUNT
      REAL*8     EPS, T_ACCUM, SIGMA_DETX, SIGMA_DETY, VAR_THRESH, T_SHIFT, TIME_THRESH, DIFF_THRESH
      PARAMETER  ( MPB    =   7 )
      PARAMETER  ( MP     =  128*1024 )
      PARAMETER  ( MDEG   =   3 )
      PARAMETER  ( EPS = 1.D0 )
      PARAMETER  ( N_KNOT = 2 )
      PARAMETER  ( NUM_SUM = 3 ) 
      PARAMETER  ( SIG_NUM = 10 )
      PARAMETER  ( FAIL_AMP = 1 )
      PARAMETER  ( FAIL_CPDIFF = 2 )
      PARAMETER  ( FAIL_Y8DIFF = 3 )
      PARAMETER  ( VAR_THRESH = 5.D-2 )
      PARAMETER  ( SIGMA_DETX = 23.929070929070928 )
      PARAMETER  ( SIGMA_DETY = 19.511488511488512 )
      PARAMETER  ( T_SHIFT = 600 )
      REAL*4     G_DEL
      REAL*8,    ALLOCATABLE :: T8(:,:,:), P8(:,:,:), A8(:,:,:), Y8(:,:,:),  &
     &     FREQ(:), FREQ_SDEL(:,:), AMPL_SDEL(:,:), PHAS_SDEL(:,:), &
     &     TIM(:), PHAS(:,:), PHAS_AMB(:,:), AMPL(:,:), T_NONZERO(:), &
     &     TONE_SORT(:), DIFF_Y8(:)
      REAL*8     FRQ_DIF, FREQS(PIM__MTON,PIM__MFRQ), P8_SPL(PIM__MTON), &
     &     P8_SPL_COEFF(1-MDEG:PIM__MTON), P8_SPL_RMS, KNOT_LOC(N_KNOT), &
     &     A8_SPL_COEFF(1-MDEG:PIM__MTON), A8_SPL_RMS, A8_SPL(PIM__MTON), &
     &     CP_DIFF(PIM__MTON), MEAN_MASK, AMPMAX, FREQ_ABOVE(PIM__MTON),  &
     &     PHASE_ABOVE(PIM__MTON), AMP_ABOVE(PIM__MTON), BPAS_PHASE(PIM__MTON), &
     &     BPAS_AMP(PIM__MTON), DIFF_PHASE(PIM__MTON), AVE_PHASE, AVE_AMP, &
     &     DIFF_AMP(PIM__MTON), FREQ_1ST(PIM__MFRQ), T_ACCUM_AVG, TIM_DIFF, &
     &     FRQ_DIF_MIN, PCAL_FRQ_STEP, PHAS_LAST, SIG, T8_DIFF, TIME_AVG, &
     &     AMP_MED_TONE, DIFF_Y8_THRESHX, DIFF_Y8_THRESHY, SIG_Y8
      CHARACTER  PREF_NAME*128, STR*32, STR1*32, &
     &           MES_SELSTA*80, STA_STR*8
      INTEGER*4  MODE, IND_STA, ISTA_LAST, ICODE, IFRQ, NC, NR, NFRQ, &
     &           J1, J2, J3, J4, J5, J6, J7, J8, J9, NP, IND_POL, IND_PLT, &
     &           I1, I2, I3, I4, I5, K1, K2, K3, K4, K5, K6, K7, K8, K9, &
     &           L1, L2, L3, M1, M2, M3, M4, COUNT_DIFF, &
     &           M_TONES, IND_TONE, IND_FREQ, N_TONES, I_TONE, IND(2,32), I_ABOVE, &
     &           I_NONZERO, LIND, IER, NPT, BEG_STA, END_STA, PREFIX, CP_MAXLOC, RECURSIVE, &
     &           NO_TON, NPCL, IND_MOD, ITYP, KCHN, NT_USED, KP, IND_TONE_FREQ(PIM__MTON), &
     &           NUM_NONZERO, NPOI_ACCUM, NUM_ACCUM, N_AVG, NPOI, N_SHIFT
      LOGICAL*4  INDS_ABOVE(PIM__MTON), LEX
      LOGICAL*4, ALLOCATABLE :: INDS_NONZERO(:)
      INTEGER*4, ALLOCATABLE :: DIFF_MASK(:,:,:), FAIL_ARR(:,:,:), IND_FRQ(:), IND_TON(:)
      INTEGER*4, EXTERNAL :: ILEN, I_LEN, LTM_DIF
      REAL*4,    EXTERNAL :: PIMA_PC_GDEL
      REAL*8,    EXTERNAL :: EBSPL_VAL_R8
      COMPLEX*16  CP_DATA(PIM__MTON), CP_SPL(PIM__MTON)
!
      ALLOCATE ( T8(MAX(1,PIM%NPCT),MP,PIM%NFRQ), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           WRITE ( 6, * ) 'mp, PIM%NPCT, pim%nfrq= ', mp, PIM%NPCT, pim%nfrq ! %%%%
           CALL CLRCH  ( STR )
           CALL IINCH8 ( INT8(MP)*INT8(PIM%NPCT)*INT8(PIM%NFRQ)*INT8(8), STR )
           CALL ERR_LOG ( 8821, IUER, 'PIMA_GEPM', &
     &         'Failure to allocate '//STR(1:I_LEN(STR))// &
     &         ' bytes of dynamic memory for array T8' )
           RETURN
      END IF

      ALLOCATE ( P8(MAX(1,PIM%NPCT),MP,PIM%NFRQ), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( MP*PIM%NPCT*PIM%NFRQ*8, STR )
           CALL ERR_LOG ( 8822, IUER, 'PIMA_GEPM', &
     &         'Failure to allocate '//STR(1:I_LEN(STR))// &
     &         ' bytes of dynamic memory for array P8' )
           RETURN
      END IF

      ALLOCATE ( A8(MAX(1,PIM%NPCT),MP,PIM%NFRQ), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( MP*PIM%NPCT*PIM%NFRQ*8, STR )
           CALL ERR_LOG ( 8823, IUER, 'PIMA_GEPM', &
     &         'Failure to allocate '//STR(1:I_LEN(STR))// &
     &         ' bytes of dynamic memory for array A8' )
           RETURN
      END IF

      ALLOCATE ( Y8(MAX(1,PIM%NPCT),MP,PIM%NFRQ), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( MP*PIM%NPCT*PIM%NFRQ*8, STR )
           CALL ERR_LOG ( 8824, IUER, 'PIMA_PLOT_PCAL', &
     &         'Failure to allocate '//STR(1:I_LEN(STR))// &
     &         ' bytes of dynamic memory for array Y8' )
           RETURN
      END IF

      ALLOCATE ( DIFF_MASK(MAX(1,PIM%NPCT),MP,PIM%NFRQ), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( MP*PIM%NPCT*PIM%NFRQ*4, STR )
           CALL ERR_LOG ( 8825, IUER, 'PIMA_GEPM', &
     &         'Failure to allocate '//STR(1:I_LEN(STR))// &
     &         ' bytes of dynamic memory for array DIFF_MASK' )
           RETURN
      END IF

      ALLOCATE ( FAIL_ARR(PIM__MTON,PIM%NFRQ,PIM%NSTA), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( MP*PIM%NFRQ*PIM%NSTA*4, STR )
           CALL ERR_LOG ( 8826, IUER, 'PIMA_GEPM', &
     &         'Failure to allocate '//STR(1:I_LEN(STR))// &
     &         ' bytes of dynamic memory for array FAIL_ARR' )
           RETURN
      END IF

      CALL CHIN ( PIM%CONF%PHAS_CAL_CODE, IND_TONE ) ! Set IND_TONE
!
!-----Read/allocate pcal mask if desired
!
      IF ( .NOT. ASSOCIATED ( PIM%PCAL_MASK )  ) THEN
           ALLOCATE ( PIM%PCAL_MASK(PIM%NPCT,PIM%NFRQ,PIM%NSTA), STAT=IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 8827, IUER, 'PIMA_GEPM', &
          &                    'Failure to allocate memory for PIM%PCAL_MASK')
                RETURN
           ENDIF
!
! -------- Check whether the pcal mask file exists
!
           INQUIRE ( FILE=PIM%CONF%PCAL_MASK_FILE, EXIST=LEX )
           IF ( LEX ) THEN
!
! ------------- If exists, read it
!
                CALL ERR_PASS ( IUER, IER )
                CALL READ_PCAL_MASK ( PIM, IER )
                IF ( IER .NE. 0 ) THEN
                     CALL ERR_LOG ( 8828, IUER, 'PIMA_GEPM', &
          &                         'Failure to read PIM%PCAL_MASK' )
                     RETURN
                ENDIF
              ELSE
!
! ------------- If does not exist, initialize it with 1 (all tones are good)
!
                PIM%PCAL_MASK = 1
           ENDIF
      END IF      
!
      IND_FREQ = PIM%CONF%BEG_FRQ

      IF ( ( PIM%CONF%POLAR == PIMA__POLAR_RR  .OR. &
     &        PIM%CONF%POLAR == PIMA__POLAR_HH       ) .OR. &
     &      PIM%CONF%ACT_CODE == PIMA__PDPL_CODE            ) THEN
            IND_POL = 1
          ELSE IF ( ( PIM%CONF%POLAR == PIMA__POLAR_LL .OR. &
     &                PIM%CONF%POLAR == PIMA__POLAR_VV      ) .AND. &
     &              PIM%NPOL == 2 ) THEN
            IND_POL = 2
          ELSE IF ( PIM%CONF%POLAR == PIMA__POLAR_LL .AND. &
     &                PIM%NPOL == 1 ) THEN
            IND_POL = 1
          ELSE
            CALL ERR_LOG ( 8829, IUER, 'PIMA_GEPM', 'Polarization code '// &
     &           TRIM(PIM%CONF%POLAR)//' is not supported for plotting pcal. '//&
     &          'Supported codes: RR, LL, HH, VV' )
            RETURN
      END IF

      IF ( STA_NAM == 'ALL' .OR. STA_NAM == 'all' ) THEN
           BEG_STA = 1
           END_STA = PIM%NSTA
        ELSE
           BEG_STA = LTM_DIF ( 0, PIM%NSTA, PIM%C_STA, STA_NAM )
           END_STA = BEG_STA
      ENDIF

      TIME_AVG = 0
      N_AVG = 0
      FAIL_ARR = 0
      IND_TONE_FREQ = 0
! ----Preprocessing run -- unwrap, find group delays, form bandpass
      DO 610 IND_STA = BEG_STA,END_STA
! ------ Free previously allocated memory
!
         IF ( ALLOCATED ( TONE_SORT  ) ) DEALLOCATE ( TONE_SORT  )
         IF ( ALLOCATED ( TIM        ) ) DEALLOCATE ( TIM        )
         IF ( ALLOCATED ( DIFF_Y8    ) ) DEALLOCATE ( DIFF_Y8    )
!
! ------ NB: this logic does not support merged and combined frequency groups!!
!
         IF ( PIM%STA(IND_STA)%PCAL(1)%PCAL_AVAIL .AND. &
     &        PIM%STA(IND_STA)%PCAL(1)%PCAL_USE   .AND. &
     &        PIM%STA(IND_STA)%PCAL(1)%NPOI .GE. 3      ) THEN
!
              NPCL = PIM%STA(IND_STA)%PCAL(1)%NO_TONES * PIM%NFRQ
!
! ----------- Memory allocation
!
              ALLOCATE ( TONE_SORT(PIM%STA(IND_STA)%PCAL(1)%NPOI)    )
              ALLOCATE ( TIM(PIM__MOBS) )

              T8    = 0.0D0
              P8    = 0.0D0
              A8    = 0.0D0
              FREQS = 0.0D0

              NPOI =  PIM%STA(IND_STA)%PCAL(1)%NPOI
              TIM = PIM%STA(IND_STA)%PCAL(1)%TIME_MID_R8(1:NPOI)
              TIM_DIFF = 0.D0
              DO 350 I5 = 1,NPOI
                 IF ( I5 > 1 ) TIM_DIFF = TIM_DIFF + TIM(I5) - TIM(I5-1)
 350          CONTINUE

              T_ACCUM_AVG = TIM_DIFF / (NPOI - 1)

              IF ( T_ACCUM_AVG .LT. T_ACCUM ) THEN
                   NUM_ACCUM = IDNINT ( T_ACCUM / T_ACCUM_AVG )
                   NPOI_ACCUM = CEILING ( NPOI * 1.D0 / NUM_ACCUM )
                 ELSE
                   NUM_ACCUM = 0
                   NPOI_ACCUM = NPOI
              ENDIF
              ALLOCATE ( DIFF_Y8(NPOI_ACCUM-1) )
!
!-------------Set up phase, time, amplitude, differenced phase arrays and average to
!-------------the given time step
!
              CALL ERR_PASS ( IUER, IER )
              CALL PIMA_PCAL_AVG ( PIM, IND_STA, PIMA__AMP_MIN, NUM_ACCUM, &
      &                            NPOI_ACCUM, FREQS, IND_TONE_FREQ, T8, P8, A8, Y8, IER )
              IF  ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 8830, IUER, 'PIMA_GEPM', &
      &                          'Error in setting up P8, T8, A8, Y8 matrices'//&
      &                          ' in PIMA_PCAL_AVG')
                   RETURN
              ENDIF
!
!-------------remove group delay in phase array P8
!
              CALL ERR_PASS ( IUER, IER )
              CALL PIMA_PCAL_CLEAN8 ( PIM, IND_STA, NUM_ACCUM, NPOI_ACCUM, &
      &                               T8, P8, A8, IER )
              IF  ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 8831, IUER, 'PIMA_GEPM', &
      &                          'Error in cleaning P8, T8, A8, Y8 matrices'//&
      &                          ' in PIMA_PCAL_CLEAN8')
                   RETURN
              ENDIF
         ENDIF

         M_TONES = PIM%STA(IND_STA)%PCAL(PIM%CONF%FRQ_GRP)%NO_TONES
         P8_SPL_COEFF = 0.0D0
         A8_SPL_COEFF = 0.0D0
         INDS_ABOVE = .FALSE.
         FREQ_ABOVE = 0.0D0
         PHASE_ABOVE = 0.0D0
         AMP_ABOVE = 0.0D0
         DIFF_MASK = 1

         IFRQ = 0
         DO 410 J1=PIM%CONF%BEG_FRQ,PIM%CONF%END_FRQ
            IFRQ = IFRQ + 1
            NP = 0
            KP = 0
            IF ( PIM%STA(IND_STA)%PCAL(PIM%CONF%FRQ_GRP)%PCAL_AVAIL .AND. &
        &        PIM%STA(IND_STA)%PCAL(PIM%CONF%FRQ_GRP)%PCAL_USE         ) THEN
!
! --- --------Loop over tones within the frequency group
!----------------Remove bandpass shape of phase calibration tones
!
                 CALL ERR_PASS ( IUER, IER )
                 CALL PIMA_PCAL_BPASS ( PIM, IND_STA, J1, IFRQ, PIMA__AMP_MIN, &
        &                                 NUM_ACCUM, NPOI_ACCUM, FREQS, T8, P8, A8, IER )
                 IF  ( IER .NE. 0 ) THEN
                       CALL ERR_LOG ( 8832, IUER, 'PIMA_GEPM', &
         &                          'Error in removing pcal bandpass'//&
         &                          ' in PIMA_PCAL_BPASS')
                       RETURN
                 ENDIF
! ------------Check if phase cal tones are real or noise by checking amplitude (amp<amp_min)
                 DO 440 J4 = 1,M_TONES
                    TONE_SORT(1:NPOI_ACCUM) = A8(J4,1:NPOI_ACCUM,IFRQ)
                    CALL SORT_FAST_R8( INT8 ( NPOI_ACCUM ), TONE_SORT )
                    AMP_MED_TONE = TONE_SORT(NPOI_ACCUM / 2)
                    IF ( AMP_MED_TONE < PIMA__AMP_MIN .AND. FREQS(J4,IFRQ) > PIMA__MIN_FRQ ) THEN
                         DIFF_MASK(J4,1:NPOI_ACCUM,IFRQ) = 0
                         P8(J4,1:NPOI_ACCUM,IFRQ) = 0.0D0
                         A8(J4,1:NPOI_ACCUM,IFRQ) = 0.0D0
                         FAIL_ARR(J4,IFRQ,IND_STA) = FAIL_AMP
                    ENDIF
 440             CONTINUE
   ! ------------Loop over tones within the frequency group
                 NP = 0
                 DO 510 K1=1,NPOI_ACCUM
                    NP = NP+1
                    RECURSIVE = 0

 600                CONTINUE
                    IF ( RECURSIVE == 1 ) THEN ! remove residual group delay and unwrap
                         I_ABOVE = 0
                         INDS_ABOVE(1:M_TONES) = A8(1:M_TONES,NP,IFRQ) > PIMA__AMP_MIN
                         DO 530 K3 = 1,M_TONES
                            IF ( INDS_ABOVE(K3) .EQV. .TRUE. ) THEN
                                 I_ABOVE = I_ABOVE + 1
                                 FREQ_ABOVE(I_ABOVE) = FREQS(K3,IFRQ)
                                 AMP_ABOVE(I_ABOVE) = A8(K3,NP,IFRQ)
                                 PHASE_ABOVE(I_ABOVE) = P8(K3,NP,IFRQ)
                            ENDIF
 530                     CONTINUE
!
                         CALL ERR_PASS ( IUER, IER )
                         G_DEL = PIMA_PC_GDEL( M_TONES, FREQS(1,IFRQ), P8(1,NP,IFRQ), &
        &                                      A8(1,NP,IFRQ), IER )
                         IF  ( IER .NE. 0 ) THEN
                              CALL ERR_LOG ( 8833, IUER, 'PIMA_GEPM', &
        &                                    'Failure in computing residual group delay')
                              RETURN
                         ENDIF
                         PHASE_ABOVE(1:I_ABOVE) = PHASE_ABOVE(1:I_ABOVE) - G_DEL * &
        &                                       PI2 * ( FREQ_ABOVE(1:I_ABOVE) - FREQ_ABOVE(1) )
                         CALL ERR_PASS ( IUER, IER )
                         CALL AMBIG_RESOLVE ( ARA2__PHD, I_ABOVE, &
        &                                     FREQ_ABOVE, PHASE_ABOVE, IER )
                         IF  ( IER .NE. 0 ) THEN
                              CALL ERR_LOG ( 8834, IUER, 'PIMA_GEPM', &
        &                                    'Failure in resolving residual phase ambiguity')
                              RETURN
                         ENDIF
                         I_ABOVE = 0
                         DO 540 K4 = 1,M_TONES
                            IF ( INDS_ABOVE(K4) .EQV. .TRUE. ) THEN
                                 I_ABOVE = I_ABOVE + 1
                                 P8(K4,NP,IFRQ) = PHASE_ABOVE(I_ABOVE)
                            ENDIF
 540                     CONTINUE
                    ENDIF

   ! ---------------Spline remaining trend in phase and amplitude
                    IF ( ALL ( A8(1:M_TONES,NP,IFRQ) >  PIMA__AMP_MIN ) ) THEN
                         IF (M_TONES > 4) THEN
                            KNOT_LOC(1) =  FREQS(1,IFRQ) - EPS
                            KNOT_LOC(2) =  FREQS(M_TONES,IFRQ) + EPS
                            CALL ERR_PASS ( IUER, IER )
                            CALL EBSPL_LSQ_CNS3 ( M_TONES, FREQS(1,IFRQ), P8(1,NP,IFRQ), N_KNOT, 3, KNOT_LOC, &
        &                                         P8_SPL_COEFF, 0.0D0, 5.0D-7, 1.0D-1, P8_SPL_RMS, IER )
                            IF  ( IER .NE. 0 ) THEN
                                 CALL ERR_LOG ( 8835, IUER, 'PIMA_GEPM', &
        &                                       'Failure in determining phase spline coefficients')
                                 RETURN
                            ENDIF
                            CALL ERR_PASS ( IUER, IER )
                            CALL EBSPL_LSQ_CNS3 ( M_TONES, FREQS(1,IFRQ), A8(1,NP,IFRQ), N_KNOT, 3, KNOT_LOC, &
        &                                         A8_SPL_COEFF, 0.0D0, 5.0D-7, 1.0D-1, A8_SPL_RMS, IER )
                            IF  ( IER .NE. 0 ) THEN
                                 CALL ERR_LOG ( 8836, IUER, 'PIMA_GEPM', &
        &                                       'Failure in determining amplitude spline coefficients')
                                 RETURN
                            ENDIF
                         ENDIF
                      ELSE ! Identify which amplitude values are below threshold, exclude
                         I_ABOVE = 0
                         INDS_ABOVE(1:M_TONES) = A8(1:M_TONES,NP,IFRQ) > PIMA__AMP_MIN
                         DO 550 K5 = 1,M_TONES
                            IF ( INDS_ABOVE(K5) .EQV. .TRUE. ) THEN
                                 I_ABOVE = I_ABOVE + 1
                                 FREQ_ABOVE(I_ABOVE) = FREQS(K5,IFRQ)
                                 AMP_ABOVE(I_ABOVE) = A8(K5,NP,IFRQ)
                                 PHASE_ABOVE(I_ABOVE) = P8(K5,NP,IFRQ)
                            ENDIF
 550                     CONTINUE

                         IF ( I_ABOVE > 4) THEN
                             KNOT_LOC(1) =  FREQ_ABOVE(1) - EPS
                             KNOT_LOC(2) =  FREQ_ABOVE(I_ABOVE) + EPS
                             CALL ERR_PASS ( IUER, IER )
                             CALL EBSPL_LSQ_CNS3 ( I_ABOVE, FREQ_ABOVE, PHASE_ABOVE, N_KNOT, 3, KNOT_LOC, &
        &                                          P8_SPL_COEFF, 0.0D0, 5.0D-7, 1.0D-1, P8_SPL_RMS, IER )
                             IF  ( IER .NE. 0 ) THEN
                                  CALL ERR_LOG ( 8837, IUER, 'PIMA_GEPM', &
        &                                        'Failure in determining phase spline coefficients')
                                  RETURN
                             ENDIF
                             CALL ERR_PASS ( IUER, IER )
                             CALL EBSPL_LSQ_CNS3 ( I_ABOVE, FREQ_ABOVE, AMP_ABOVE, N_KNOT, 3, KNOT_LOC, &
        &                                          A8_SPL_COEFF, 0.0D0, 5.0D-7, 1.0D-1, A8_SPL_RMS, IER )
                             IF  ( IER .NE. 0 ) THEN
                                  CALL ERR_LOG ( 8838, IUER, 'PIMA_GEPM', &
        &                                        'Failure in determining amplitude spline coefficients')
                                  RETURN
                             ENDIF
                         ENDIF
                    ENDIF
!-------------------Evaluate splines on complex plane against corresponding data to find
!-------------------spurious signals
                    NUM_NONZERO = COUNT( FREQS(1:M_TONES,IFRQ) > PIMA__MIN_FRQ )
                    IF (NUM_NONZERO > 4) THEN
                         AMPMAX = MAXVAL(A8(1:M_TONES,NP,IFRQ))
                         IF ( AMPMAX == 0 ) THEN
                              DIFF_MASK(1:M_TONES,NP,IFRQ) = 0
                            ELSE IF ( ALL ( A8(1:M_TONES,NP,IFRQ) > PIMA__AMP_MIN ) &
             &                              .OR. I_ABOVE > 3 ) THEN
                              DO 560 K6 = 1,M_TONES
                                 IF ( A8(K6,NP,IFRQ) > PIMA__AMP_MIN ) THEN
                                      CALL ERR_PASS ( IUER, IER )
                                      P8_SPL(K6) = EBSPL_VAL_R8 ( N_KNOT, MDEG, FREQS(K6,IFRQ), KNOT_LOC, &
             &                                                    P8_SPL_COEFF )
                                      A8_SPL(K6) = EBSPL_VAL_R8 ( N_KNOT, MDEG, FREQS(K6,IFRQ), KNOT_LOC, &
             &                                                    A8_SPL_COEFF )
                                      CP_SPL(K6) = A8_SPL(K6)*COMPLEX( COS(P8_SPL(K6)), SIN(P8_SPL(K6)) )
                                      CP_DATA(K6) = A8(K6,NP,IFRQ)*COMPLEX( &
             &                                      COS(P8(K6,NP,IFRQ)), SIN(P8(K6,NP,IFRQ)))
                                      CP_DIFF(K6) = SQRT((REALPART(CP_DATA(K6))-REALPART(CP_SPL(K6)))**2 + &
             &                                        (IMAGPART(CP_DATA(K6))-IMAGPART(CP_SPL(K6)))**2)/AMPMAX
                                    ELSE IF ( FREQS(K6,IFRQ) > PIMA__MIN_FRQ ) THEN
                                      CP_DIFF(K6) = 0
                                      DIFF_MASK(K6,NP,IFRQ) = 0
                                 ENDIF
 560                          CONTINUE
                              CP_MAXLOC = MAXLOC( CP_DIFF(1:NUM_NONZERO), 1 )
                              !IF ( IND_STA == 2 .AND. (IFRQ ==4) ) THEN !.AND. CP_MAXLOC /= 1 .AND. CP_DIFF(CP_MAXLOC) > DIFF_THRESH .AND. .FALSE.) THEN
                              !   CALL DIAGI_2(M_TONES, FREQS(1,IFRQ), P8_SPL, M_TONES, &
                              !                FREQS(1,IFRQ), P8(1,NP,IFRQ), IER)
                              !   CALL DIAGI_2(M_TONES, FREQS(1,IFRQ), A8_SPL, M_TONES, &
                              !                FREQS(1,IFRQ), A8(1,NP,IFRQ), IER)
                              !   CALL DIAGI_1(M_TONES, FREQS(1,IFRQ), CP_DIFF, IER)
                              !ENDIF
                              !IF ( IND_STA == 2 .AND. IFRQ == 1 .AND. CP_MAX /= 1 ) THEN
                              !     WRITE ( 6, * ) 'CP_MAXLOC', CP_MAXLOC
                              !     WRITE ( 6, * ) 'CP_DIFF', CP_DIFF(1:NUM_NONZERO)
                              !     WRITE ( 6, * ) 'P8_SPL', P8_SPL(1:NUM_NONZERO)  
                              !     WRITE ( 6, * ) 'P8', P8(1:NUM_NONZERO,NP,IFRQ)
                              !     WRITE ( 6, * ) 'DIFF_MASK', DIFF_MASK(1:NUM_NONZERO,NP,IFRQ) 
                              !     WRITE ( 6, * ) 'FAIL_ARR', FAIL_ARR(1:NUM_NONZERO,IFRQ,IND_STA)
                              !ENDIF
                              IF ( CP_DIFF(CP_MAXLOC) > DIFF_THRESH ) THEN
                                   DIFF_MASK(CP_MAXLOC,NP,IFRQ) = 0
                                   P8(CP_MAXLOC,NP,IFRQ) = 0.0D0
                                   A8(CP_MAXLOC,NP,IFRQ) = 0.0D0
                                   RECURSIVE = 1
                                   GOTO 600
                              ENDIF
                            ELSE
                               DIFF_MASK(1:M_TONES,NP,IFRQ) = 0
                         ENDIF
                    ENDIF
 510             CONTINUE
            END IF
 410     CONTINUE
         !WRITE ( 6, * ) 'STATION:', PIM%C_STA(IND_STA)
!
!-------- Find if a relative pcal tone is too noisy (jumps too many times)
!
         I_ABOVE = COUNT ( FREQS(1:M_TONES,1) > PIMA__MIN_FRQ )
         IFRQ = 1
         SIG_Y8 = 0.0D0
         DO 570 K7 = PIM%CONF%BEG_FRQ+1,PIM%CONF%END_FRQ
            IFRQ = IFRQ + 1
            I_TONE = 0
            DO 580 K8=1,M_TONES
               I_TONE = I_TONE+1
               COUNT_DIFF = 0
               DO 590 K9 = 1,NPOI_ACCUM-1
                  IF ( ABS ( Y8(I_TONE,K9 + 1,IFRQ) ) > 0.0D0 .AND. ABS ( Y8(I_TONE,K9,IFRQ - 1) ) > 0.0D0 ) THEN
                       DIFF_Y8(K9) =  (Y8(I_TONE,K9 + 1,IFRQ) - Y8(I_TONE,K9,IFRQ)) ** 2
                       IF ( DIFF_Y8(K9) < VAR_THRESH ) THEN
                            ! prevent outliers from infecting variance estimate
                            SIG_Y8 = SIG_Y8 + DIFF_Y8(K9)
                            COUNT_DIFF = COUNT_DIFF + 1
                       ENDIF
                     ELSE
                       DIFF_Y8(K9) = 0
                  ENDIF
 590           CONTINUE
               IF ( COUNT_DIFF > 0 ) SIG_Y8 = SIG_Y8 / COUNT_DIFF
               DIFF_Y8_THRESHX = SIGMA_DETX * SIG_Y8
               N_SHIFT = INT ( T_SHIFT / T_ACCUM )
               DIFF_Y8_THRESHY = 2 * N_SHIFT * SIGMA_DETY * SIG_Y8
               COUNT_DIFF = 0
               DO 591 K9 = 1,NPOI_ACCUM-1
!
!-----------------Find if jump in squared differences violates 10^-6 false alarm chi-squared test
!
                  IF ( ABS ( Y8(I_TONE,K9 + 1,IFRQ - 1) ) > 0.0D0 .AND. ABS ( Y8(I_TONE,K9,IFRQ - 1) ) > 0.0D0 ) THEN
                       ! Check if difference is a single point spur or a pcal jump
                       ! only turn off channel if pcal jump
                       ! spurs will usually infect 2 points therefore average
                       IF ( DIFF_Y8(K9)  > DIFF_Y8_THRESHX .AND. K9 > N_SHIFT + 1 .AND. K9 < NPOI_ACCUM - N_SHIFT - 1 ) THEN
                            IF (  (Y8(I_TONE,K9 + N_SHIFT, IFRQ - 1) - Y8(I_TONE,K9 - N_SHIFT, IFRQ - 1 )) ** 2 > &
                      &           DIFF_Y8_THRESHY ) &
                      &           COUNT_DIFF = COUNT_DIFF + 1
                       ENDIF
                  ENDIF
 591           CONTINUE
               IF ( PIM%CONF%DEBUG_LEVEL >= 5 ) THEN
                    WRITE ( 6, * ) 'STATION:', PIM%C_STA(IND_STA)
                    WRITE ( 6, * ) 'IF:', IFRQ
                    WRITE ( 6, * ) 'TONE:', I_TONE
                    WRITE ( 6, * ) 'COUNT_DIFF:', COUNT_DIFF
                    WRITE ( 6, * ) 'DIFF_Y8_THRESHX:', DIFF_Y8_THRESHX
                    WRITE ( 6, * ) 'DIFF_Y8_THRESHY:', DIFF_Y8_THRESHY
               ENDIF
               !IF ( IND_STA == 8) CALL DIAGI_1(NPOI_ACCUM, T8(1,1:NPOI_ACCUM,1), Y8(1,IFRQ-1), IER)
               !IF ( IND_STA == 8) CALL DIAGI_1(NPOI_ACCUM-1, T8(1,1:NPOI_ACCUM-1,1), DIFF_Y8, IER)
               IF ( COUNT_DIFF  > MAX_COUNT .AND. FAIL_ARR(I_TONE,IFRQ,IND_STA) /= FAIL_AMP) THEN
                    PIM%PCAL_MASK(I_TONE,K7,IND_STA) = 0
                    FAIL_ARR(I_TONE,IFRQ,IND_STA) =  FAIL_Y8DIFF
               ENDIF
               IF ( ALL ( FAIL_ARR(I_TONE,2:IFRQ,IND_STA) > 0 ) .AND. FAIL_ARR(I_TONE,1,IND_STA) /= FAIL_AMP ) THEN
                    PIM%PCAL_MASK(I_TONE,PIM%CONF%BEG_FRQ,IND_STA) = 0
                    FAIL_ARR(I_TONE,1,IND_STA) =  FAIL_Y8DIFF
               ENDIF    
 580        CONTINUE
 570     CONTINUE
         
!--------Translate DIFF_MASK arr to PCAL_MASK by time threshold (max. num. of flagged epochs)
         IF  ( LTM_DIF ( 0, PIM%CONF%L_PUS, PIM%CONF%PCAL_USE_STA, PIM%C_STA(IND_STA) ) == 0 .OR. &
               PIM%CONF%L_PUS == 0 ) THEN
              ! check if pcal is used, if used, write out pcal mask
              IFRQ = 0
              DO 710 L1=PIM%CONF%BEG_FRQ,PIM%CONF%END_FRQ
                 IFRQ = IFRQ + 1
                 IF ( PIM%STA(IND_STA)%PCAL(PIM%CONF%FRQ_GRP)%PCAL_AVAIL .AND. &
             &        PIM%STA(IND_STA)%PCAL(PIM%CONF%FRQ_GRP)%PCAL_USE         ) THEN
                      DO 720 L2=1,M_TONES
                         MEAN_MASK = 0
                         NPT = 0
                         DO 730 L3=1,NPOI_ACCUM
                            MEAN_MASK = MEAN_MASK + DIFF_MASK(L2,L3,IFRQ)
                            NPT = NPT + 1
 730                     CONTINUE
                         !IF ( IND_STA == 2 .AND. ( IFRQ == 4 ) ) THEN
                         !     WRITE ( 6, *) 'IFRQ:', IFRQ
                         !     WRITE (6, *) 'TONE:', L2
                         !     WRITE (6, *) 'MEAN_MASK:', MEAN_MASK/NPT
                         !ENDIF
                         IF ( MEAN_MASK/NPT < 1-TIME_THRESH ) THEN
                              PIM%PCAL_MASK(L2,L1,IND_STA) = 0
                              IF ( FAIL_ARR(L2,IFRQ,IND_STA) == 0 ) THEN
                                 FAIL_ARR(L2,IFRQ,IND_STA) = FAIL_CPDIFF
                              ENDIF
                            ELSE IF ( FAIL_ARR(L2,IFRQ,IND_STA) .NE. FAIL_Y8DIFF) THEN
                              PIM%PCAL_MASK(L2,L1,IND_STA) = 1
                         ENDIF
 720                  CONTINUE
                 ENDIF
 710          CONTINUE
         ENDIF
         IF ( ALL ( FAIL_ARR(1:M_TONES,1:IFRQ,IND_STA) > 0  ) ) THEN
              WRITE ( 6, * ) 'All pcal tones of station ', PIM%C_STA(IND_STA), ' are noise.', &
        &                    ' Recommend adding to  PCAL: not_to_use:  in control file'
         ENDIF
 610  CONTINUE
!
!-----Now calculate phase calibration statistics, jumps
!
      CALL ERR_PASS  ( IUER, IER )
      CALL PIMA_PCST ( PIM, STA_NAM, T_ACCUM, FREQS, T8, P8, A8, Y8, FAIL_ARR, IND_POL, IER )
      IF  ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 8840, IUER, 'PIMA_GEPM', 'Failure in computing phase calibration statistics')
           RETURN
      ENDIF
      
      CALL ERR_PASS  ( IUER, IER )
      CALL WRI_PCAL_MASK ( PIM, PIM%CONF_FILE, PIM%CONF%PCAL_MASK_FILE, IER )
      IF  ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 8841, IUER, 'PIMA_GEPM', 'Failure in writing phase calibration mask')
           RETURN
      ENDIF
!
      CALL ERR_PASS  ( IUER, IER )
      CALL WRI_PCAL_RPT ( PIM, PIM%CONF_FILE, FAIL_ARR, STA_NAM, IER )
      IF  ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 8842, IUER, 'PIMA_GEPM', 'Failure in writing phase calibration report')
           RETURN
      ENDIF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  PIMA_GEPM  !#!
