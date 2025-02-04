      SUBROUTINE PIMA_PCST ( PIM, STA_NAM, T_ACCUM, FREQS, &
     &                       T8, P8, A8, Y8, FAIL_ARR, IND_POL, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine PIMA_PCST 
! *                                                                      *
! *  ### 21-JUN-2022  PIMA_PCST   v1.1 (c)              26-AUG-2022 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'astro_constants.i'
      INCLUDE   'pima.i'
      INCLUDE   'diagi.i'
      TYPE     ( PIMA__TYPE  ) :: PIM
      CHARACTER  STA_NAM*(*)
      INTEGER*4  IUER
      INTEGER*4  MPB, MP, NN, MDEG, N_KNOT, SIG_NUM, FAIL_AMP, FAIL_CPDIFF, &
     &           NUM_SUM, BIN_DIFF
      REAL*8     EPS, T_ACCUM, T_SHIFT, PHASE_MAX, RMS_NSIGMA
      PARAMETER  ( MPB    =   7 )
      PARAMETER  ( MP     =  128*1024 )
      PARAMETER  ( MDEG   =   3 )
      PARAMETER  ( EPS = 1.D0 )
      PARAMETER  ( N_KNOT = 2 )
      PARAMETER  ( NUM_SUM = 10 )
      PARAMETER  ( SIG_NUM = 10 )
      PARAMETER  ( FAIL_AMP = 1 )
      PARAMETER  ( FAIL_CPDIFF = 2 )
      PARAMETER  ( T_SHIFT = 600 )
      PARAMETER  ( PHASE_MAX = 250 )
      PARAMETER  ( RMS_NSIGMA = 2.5 )
      REAL*4     G_DEL
      REAL*8,    ALLOCATABLE :: FREQ_SDEL(:,:), AMPL_SDEL(:,:), PHAS_SDEL(:,:), &
     &     TIM(:), PHAS(:,:), PHAS_AMB(:,:), AMPL(:,:), T_NONZERO(:), &
     &     TONE_SORT(:), RMS_FREQ(:,:), RMS_TIME(:,:,:), ROOT_SQUARE_TIME(:), &
     &     ROOT_SQUARE(:), PHASE_SMOOTH(:), PHASE_UNWRAPPED(:), &
     &     TIME_DET(:), PHASE_CHIX(:), PHASE_CHIY (:), &
     &     DIFF_PHASE_PLOT(:), DEL_PHASE_PLOT(:)
      REAL*8     FRQ_DIF, FREQS(PIM__MTON,PIM__MFRQ), P8_SPL(PIM__MTON), &
     &     P8_SPL_COEFF(1-MDEG:PIM__MTON), P8_SPL_RMS, KNOT_LOC(N_KNOT), &
     &     A8_SPL_COEFF(1-MDEG:PIM__MTON), A8_SPL_RMS, A8_SPL(PIM__MTON), &
     &     CP_DIFF(PIM__MTON), MEAN_MASK, AMPMAX, FREQ_ABOVE(PIM__MTON),  &
     &     PHASE_ABOVE(PIM__MTON), AMP_ABOVE(PIM__MTON), BPAS_PHASE(PIM__MTON), &
     &     BPAS_AMP(PIM__MTON), DIFF_PHASE(PIM__MTON), AVE_PHASE, AVE_AMP, &
     &     DIFF_AMP(PIM__MTON), FREQ_1ST(PIM__MFRQ), T_ACCUM_AVG, TIM_DIFF, &
     &     FRQ_DIF_MIN, PCAL_FRQ_STEP, PHAS_LAST, SIG, T8_DIFF, TIME_AVG, TIME_AVG_STA, &
     &     AMP_MED_TONE, ROOT_SQUARE_FREQ(PIM__MTON), SHIFT, DRIFT, &
     &     ABS_PHASE_DEL_RATE, PHS_DIF, SIGMA_DETX, SIGMA_DETY, SIGMA_DIFF_USE, SIGMA_DIFF_MIN,  &
     &     T8(PIM%NPCT,MP,PIM%NFRQ), P8(PIM%NPCT,MP,PIM%NFRQ), A8(PIM%NPCT,MP,PIM%NFRQ), &
     &     Y8(PIM%NPCT,MP,PIM%NFRQ), DEL_RATE_THRESH, DIFF_RATE_THRESH, AVG_DIFF, AVG_DEL, &
     &     SIGMA_THRESHX, SIGMA_THRESHY, SIGMA_DIFF, DEL_ARG_HIST(MP), DIFF_ARG_HIST(MP), &
     &     PHASE_DEL_HIST(MP), PHASE_DIFF_HIST(MP), NORMAL_DIST_DEL(MP), NORMAL_DIST_DIFF(MP), &
     &     RMS_AVG, RMS_VAR, BPASS_PHASE(PIM__MTON)
      CHARACTER  PREF_NAME*128, STR*32, STR1*32, &
     &           MES_SELSTA*80, STA_STR*8
      INTEGER*4  MODE, IND_STA, ISTA_LAST, ICODE, IFRQ, NC, NR, NFRQ, &
     &           J1, J2, J3, J4, J5, J6, J7, J8, J9, NP, IND_POL, IND_PLT, &
     &           I1, I2, I3, I4, I5, K1, K2, K3, K4, K5, K6, L1, L2, L3, M1, M2, M3, M4, &
     &           M5, M6, M7, M8, N1, N2, N3, N4, N, COUNT, COUNT_SIG, N_SHIFT, COUNT_HIST, &
     &           M_TONES, IND_TONE, IND_FREQ, N_TONES, I_TONE, IND(2,32), I_ABOVE, &
     &           I_NONZERO, LIND, IER, NPT, BEG_STA, END_STA, PREFIX, CP_MAXLOC, RECURSIVE, &
     &           NO_TON, NPCL, IND_MOD, ITYP, KCHN, NT_USED, KP,  &
     &           NUM_NONZERO, NPOI_ACCUM, NUM_ACCUM, N_AVG, N_AVG_STA, NPOI, STA_LIST(PIM%NSTA), &
     &           NUM_STA, IND_TONE_FREQ(PIM__MTON), IFRQ_DIFF, TONE_DIFF, N_SINCE_DETECT, &
     &           FAIL_ARR(PIM__MTON,PIM%NFRQ,PIM%NSTA), RMS_NUMEL
      LOGICAL*4  INDS_ABOVE(PIM__MTON), PREV_EPOCH_DETECT
      LOGICAL*4, ALLOCATABLE :: INDS_NONZERO(:), FIND_IND_FREQ(:)
      COMPLEX*16  CP_DATA(PIM__MTON), CP_SPL(PIM__MTON)
      INTEGER*4, EXTERNAL :: ILEN, I_LEN, LTM_DIF
      REAL*4,    EXTERNAL :: PIMA_PC_GDEL
      REAL*8,    EXTERNAL :: EBSPL_VAL_R8
!
      ALLOCATE ( RMS_TIME(PIM__MTON,PIM%NFRQ,PIM%NSTA), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( PIM__MTON*PIM%NFRQ*PIM%NSTA*7, STR )
           CALL ERR_LOG ( 8842, IUER, 'PIMA_PCST', &
     &         'Failure to allocate '//STR(1:I_LEN(STR))// &
     &         ' bytes of dynamic memory for array RMS_TIME' )
           RETURN 
      END IF

      ALLOCATE ( RMS_FREQ(PIM%NFRQ,PIM%NSTA), STAT=IER )
      IF ( IER .NE. 0 ) THEN 
           CALL CLRCH ( STR )
           CALL IINCH ( PIM__MTON*PIM%NFRQ*PIM%NSTA*8, STR )
           CALL ERR_LOG ( 8844, IUER, 'PIMA_PCST', &
     &         'Failure to allocate '//STR(1:I_LEN(STR))// &
     &         ' bytes of dynamic memory for array RMS_FREQ' )
           RETURN 
      END IF
                 
      CALL CHIN ( PIM%CONF%PHAS_CAL_CODE, IND_TONE ) ! Set IND_TONE

!
      IND_FREQ = PIM%CONF%BEG_FRQ

      IF ( STA_NAM == 'ALL' .OR. STA_NAM == 'all' ) THEN
           BEG_STA = 1
           END_STA = PIM%NSTA
        ELSE
           BEG_STA = LTM_DIF ( 0, PIM%NSTA, PIM%C_STA, STA_NAM )
           END_STA = BEG_STA
      ENDIF
      
      TIME_AVG = 0
      N_AVG = 0 
      NUM_STA = 0
      DO 310 IND_STA = BEG_STA,END_STA
         M_TONES = PIM%STA(IND_STA)%PCAL(PIM%CONF%FRQ_GRP)%NO_TONES
         IF ( ANY (  PIM%PCAL_MASK(1:M_TONES, &
        &            PIM%CONF%BEG_FRQ:PIM%CONF%END_FRQ,IND_STA) == 1 ) ) THEN
              NUM_STA = NUM_STA + 1
              STA_LIST(NUM_STA) = IND_STA
         ENDIF
 310  CONTINUE

! ----Preprocessing run -- unwrap, find group delays, form bandpass
      DO 610 L1 = 1,NUM_STA
         IND_STA = STA_LIST(L1)
         IND_TONE_FREQ = 0
         
         NPOI =  PIM%STA(IND_STA)%PCAL(1)%NPOI
         IF ( NPOI == 1) CYCLE
! ------ Free previously allocated memory
!
         IF ( ALLOCATED ( PHAS_AMB ) ) DEALLOCATE ( PHAS_AMB )
         IF ( ALLOCATED ( TONE_SORT  ) ) DEALLOCATE ( TONE_SORT  )
         IF ( ALLOCATED ( TIM      ) ) DEALLOCATE ( TIM      )
!
! ------ NB: this logic does not support merged and combined frequency groups!!
!

         IF ( PIM%STA(IND_STA)%PCAL(1)%PCAL_AVAIL .AND. &
     &        PIM%STA(IND_STA)%PCAL(1)%PCAL_USE     ) THEN
!
              NPCL = PIM%STA(IND_STA)%PCAL(1)%NO_TONES * PIM%NFRQ
!
! ----------- Memory allocation
!
              ALLOCATE ( PHAS_AMB(NPCL,PIM%STA(IND_STA)%PCAL(1)%NPOI) )
              ALLOCATE ( TONE_SORT(PIM%STA(IND_STA)%PCAL(1)%NPOI)    )
              ALLOCATE ( TIM(PIM__MOBS) )

              T8    = 0.0D0
              P8    = 0.0D0
              A8    = 0.0D0
              FREQS = 0.0D0

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
!
!-------------Set up phase, time, amplitude, differenced phase arrays and average to
!-------------the given time step
!
              CALL ERR_PASS  ( IUER, IER )
              CALL PIMA_PCAL_AVG ( PIM, IND_STA, PIMA__AMP_MIN, NUM_ACCUM, &
      &                            NPOI_ACCUM, FREQS, IND_TONE_FREQ, T8, P8, A8, Y8, IER )
              IF  ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 8845, IUER, 'PIMA_PCST', &
      &                          'Error in setting up P8, T8, A8, Y8 matrices'//&
      &                          ' in PIMA_PCAL_AVG')
                   RETURN
              ENDIF
!
!-------------remove group delay in phase array P8
!
              IF ( M_TONES > 2 ) THEN
                  CALL ERR_PASS  ( IUER, IER )
                  CALL PIMA_PCAL_CLEAN8 ( PIM, IND_STA, NUM_ACCUM, NPOI_ACCUM, &
      &                                   T8, P8, A8, IER )
                  IF  ( IER .NE. 0 ) THEN
                        CALL ERR_LOG ( 8846, IUER, 'PIMA_PCST', &
      &                              'Error in cleaning P8, T8, A8, Y8 matrices'//&
      &                              ' in PIMA_PCAL_CLEAN8')
                       RETURN
                  ENDIF 
              ENDIF
         ENDIF

         M_TONES = PIM%STA(IND_STA)%PCAL(PIM%CONF%FRQ_GRP)%NO_TONES
         P8_SPL_COEFF = 0.0D0
         A8_SPL_COEFF = 0.0D0
         INDS_ABOVE = .FALSE.
         FREQ_ABOVE = 0.0D0
         PHASE_ABOVE = 0.0D0
         AMP_ABOVE = 0.0D0
 
         IFRQ = 0
         DO 410 J1=PIM%CONF%BEG_FRQ,PIM%CONF%END_FRQ
            IFRQ = IFRQ + 1
            NP = 0
            KP = 0
            IF ( PIM%STA(IND_STA)%PCAL(PIM%CONF%FRQ_GRP)%PCAL_AVAIL .AND. &
        &        PIM%STA(IND_STA)%PCAL(PIM%CONF%FRQ_GRP)%PCAL_USE .AND. &
        &        M_TONES .GE. 3 ) THEN
!
! ---------------Loop over tones within the frequency group
!
                 CALL ERR_PASS  ( IUER, IER )
                 CALL PIMA_PCAL_BPASS ( PIM, IND_STA, J1, IFRQ, PIMA__AMP_MIN, &
        &                                 NUM_ACCUM, NPOI_ACCUM, FREQS, &
        &                                  T8, P8, A8, BPASS_PHASE, IER )
                 IF  ( IER .NE. 0 ) THEN
                       CALL ERR_LOG ( 8847, IUER, 'PIMA_PCST', &
         &                          'Error in removing pcal bandpass'//&
         &                          ' in PIMA_PCAL_BPASS')
                       RETURN
                 ENDIF
!
! -------------- Loop over tones within the frequency group
!
                 NP = 0
                 DO 510 K1=1,NPOI_ACCUM
                    NP = NP+1
!
! ------------------Process PCAL_MASK and recompute group delay with values masked
!
                    IF ( ANY ( PIM%PCAL_MASK(1:M_TONES,J1,IND_STA) < 1 ) ) THEN 
                         ! REMOVE RESIDUAL GROUP DELAY
                         I_ABOVE = 0
                         INDS_ABOVE(1:M_TONES) = A8(1:M_TONES,NP,IFRQ) > PIMA__AMP_MIN .AND. &
        &                                        PIM%PCAL_MASK(1:M_TONES,J1,IND_STA) == 1
                         DO 530 K3 = 1,M_TONES
                            IF ( INDS_ABOVE(K3) .EQV. .TRUE. ) THEN
                                 I_ABOVE = I_ABOVE + 1
                                 FREQ_ABOVE(I_ABOVE) = FREQS(K3,IFRQ)
                                 AMP_ABOVE(I_ABOVE) = A8(K3,NP,IFRQ)
                                 PHASE_ABOVE(I_ABOVE) = P8(K3,NP,IFRQ)
                               ELSE
                                 A8(K3,NP,IFRQ) = 0
                                 P8(K3,NP,IFRQ) = 0 
                            ENDIF
 530                     CONTINUE
!
                         CALL ERR_PASS  ( IUER, IER )
                         G_DEL = PIMA_PC_GDEL( M_TONES, FREQS(1,IFRQ), P8(1,NP,IFRQ), &
        &                                      A8(1,NP,IFRQ), IER )
                         IF  ( IER .NE. 0 ) THEN
                              CALL ERR_LOG ( 8848, IUER, 'PIMA_PCST', &
        &                                    'Failure in computing residual group delay')
                              RETURN
                         ENDIF
!
! ---------------------- PIMA_PC_GDEL will work with non-consecutive freqs (but signals an error)
!
                         PHASE_ABOVE(1:I_ABOVE) = PHASE_ABOVE(1:I_ABOVE) - G_DEL * &
        &                                       PI2 * ( FREQ_ABOVE(1:I_ABOVE) - FREQ_ABOVE(1) )
                         CALL ERR_PASS  ( IUER, IER )
                         CALL AMBIG_RESOLVE ( ARA2__PHD, I_ABOVE, &
        &                                     FREQ_ABOVE, PHASE_ABOVE, IER )
                          IF  ( IER .NE. 0 ) THEN
                                CALL ERR_LOG ( 8849, IUER, 'PIMA_PCST', &
        &                                     'Failure in resolving residual phase ambiguity')
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
 510             CONTINUE 
            END IF
 410     CONTINUE
!
! ------ Find the refrence frequencies for each tone
!
         DO 470 J7=1,M_TONES
            IFRQ = 0
!
! --------- Check all the specified IFs and find the first that have
! --------- tone J7 not masked out
!
            DO 480 J8=PIM%CONF%BEG_FRQ,PIM%CONF%END_FRQ
               IFRQ = IFRQ + 1
               IF ( FREQS(J7,IFRQ) > PIMA__MIN_FRQ ) THEN
                    IF ( PIM%PCAL_MASK(J7,J8,IND_STA) == 1 ) THEN
                         IND_TONE_FREQ(J7) = IFRQ
                         GOTO 2080
                    END IF
               END IF
 480        CONTINUE 
 2080        CONTINUE 
 470     CONTINUE         
         IF  ( LTM_DIF ( 0, PIM%CONF%L_PUS, PIM%CONF%PCAL_USE_STA, PIM%C_STA(IND_STA) ) == 0 .OR. &
       &       PIM%CONF%L_PUS == 0  ) THEN
!-------------Find jitter of non-masked pcal tones in the frequency direction
!-------------this is evaluated as sqrt (phase - average phase)^2       
              DO 910 N1 = 1,NP
                 IFRQ = 0
                 IF ( PIM%STA(IND_STA)%PCAL(PIM%CONF%FRQ_GRP)%PCAL_AVAIL .AND. &
             &        PIM%STA(IND_STA)%PCAL(PIM%CONF%FRQ_GRP)%PCAL_USE         ) THEN
                      DO 920 N2=PIM%CONF%BEG_FRQ,PIM%CONF%END_FRQ
                              IFRQ = IFRQ + 1
                              INDS_ABOVE(1:M_TONES) = PIM%PCAL_MASK(1:M_TONES,N2,IND_STA) == 1
                              PHASE_ABOVE = 0.0D0 
                              FREQ_ABOVE = 0.0D0
                              ROOT_SQUARE_FREQ = 0.0D0
                              T8_DIFF = 0.0D0
                              I_ABOVE = 0
                              DO 930 N3 = 1,M_TONES
                                 IF ( INDS_ABOVE(N3) .EQV. .TRUE. ) THEN
                                      I_ABOVE = I_ABOVE + 1
                                      PHASE_ABOVE(I_ABOVE) = P8(N3,N1,IFRQ)
                                      FREQ_ABOVE(I_ABOVE) =  FREQS(N3,IFRQ)
                                 ENDIF
 930                          CONTINUE 
                              IF ( I_ABOVE > 0 ) THEN 
                                   PHASE_ABOVE(1:I_ABOVE) = PHASE_ABOVE(1:I_ABOVE) - &
              &                                        SUM ( PHASE_ABOVE(1:I_ABOVE) ) / I_ABOVE
                                   DO 940 N4=1,I_ABOVE
                                      ROOT_SQUARE_FREQ(N4) = SQRT ( PHASE_ABOVE(N4) ** 2 )  
 940                               CONTINUE
                                   RMS_FREQ(IFRQ,IND_STA) =  SUM ( ROOT_SQUARE_FREQ  ) / I_ABOVE
                                 ELSE
                                   RMS_FREQ(IFRQ,IND_STA) = 0.0D0
                               ENDIF
 920                  CONTINUE
                 ENDIF
 910          CONTINUE
!
!-------------Find jitter of non-masked pcal tones in the time direction wrt a smoothed curve
!------------ redo pcal accumulation b/c phase correction in freq dir. will cause jumps in time dir.
!
              T8    = 0.0D0
              P8    = 0.0D0
              A8    = 0.0D0
              CALL ERR_PASS  ( IUER, IER )
              CALL PIMA_PCAL_AVG ( PIM, IND_STA, PIMA__AMP_MIN, NUM_ACCUM, &
      &                            NPOI_ACCUM, FREQS, IND_TONE_FREQ, T8, P8, A8, Y8, IER )
              IF  ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 8872, IUER, 'PIMA_PCST', &
      &                          'Error in setting up P8, T8, A8, Y8 matrices'// &
      &                          ' in PIMA_PCAL_AVG')
                   RETURN
              ENDIF  
              IF ( ALLOCATED ( PHASE_SMOOTH     ) ) DEALLOCATE ( PHASE_SMOOTH )
              IF ( ALLOCATED ( PHASE_UNWRAPPED     ) ) DEALLOCATE ( PHASE_UNWRAPPED )
              IF ( ALLOCATED ( ROOT_SQUARE_TIME ) ) DEALLOCATE ( ROOT_SQUARE_TIME )
              IF ( ALLOCATED ( T_NONZERO ) ) DEALLOCATE ( T_NONZERO )
              IF ( ALLOCATED ( INDS_NONZERO ) ) DEALLOCATE ( INDS_NONZERO )
              ALLOCATE ( PHASE_SMOOTH(PIM%STA(IND_STA)%PCAL(1)%NPOI), STAT=IER )
              ALLOCATE ( PHASE_UNWRAPPED(PIM%STA(IND_STA)%PCAL(1)%NPOI), STAT=IER )
              ALLOCATE ( ROOT_SQUARE_TIME(PIM%STA(IND_STA)%PCAL(1)%NPOI), STAT=IER )
              ALLOCATE ( T_NONZERO(PIM%STA(IND_STA)%PCAL(1)%NPOI), STAT=IER )
              ALLOCATE ( INDS_NONZERO(PIM%STA(IND_STA)%PCAL(1)%NPOI), STAT=IER )
              IF  ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 8850, IUER, 'PIMA_PCST', &
           &                      'Failure to allocate arrays for clock break investigation')
                    RETURN
              ENDIF
              IFRQ = 0
              NP = NPOI_ACCUM
              TIME_AVG_STA = 0
              N_AVG_STA = 0
              RMS_NUMEL = 0
              RMS_AVG = 0.0D0
              DO 810 M1=PIM%CONF%BEG_FRQ,PIM%CONF%END_FRQ
                 IFRQ = IFRQ + 1
                 IF ( PIM%STA(IND_STA)%PCAL(PIM%CONF%FRQ_GRP)%PCAL_AVAIL .AND. &
             &        PIM%STA(IND_STA)%PCAL(PIM%CONF%FRQ_GRP)%PCAL_USE         ) THEN
                      DO 820 M2=1,M_TONES
!------------------------Analyze only non-problematic tones
                         IF ( PIM%PCAL_MASK(M2,M1,IND_STA) .EQ. 1 .AND. FREQS(M2,IFRQ) .GT. 0  ) THEN
                              INDS_NONZERO = P8(M2,1:NP,IFRQ) .NE. 0.0D0
                              PHASE_SMOOTH = 0.0D0 
                              PHASE_UNWRAPPED = 0.0D0
                              T_NONZERO = 0.0D0
                              ROOT_SQUARE_TIME = 0.0D0
                              T8_DIFF = 0.0D0
                              I_NONZERO = 0
                              DO 830 M3 = 1,NP
                                 IF ( INDS_NONZERO(M3) .EQV. .TRUE.) THEN
                                      I_NONZERO = I_NONZERO + 1
                                      PHASE_UNWRAPPED(I_NONZERO) = P8(M2,M3,IFRQ)
                                      T_NONZERO(I_NONZERO) = T8(M2,M3,IFRQ) 
                                 ENDIF
                                 IF ( M3 > 1 ) THEN
                                    T8_DIFF = T8_DIFF + T8(M2,M3,IFRQ) - T8(M2,M3-1,IFRQ)
                                 ENDIF
 830                          CONTINUE
                              TIME_AVG = TIME_AVG + T8_DIFF
                              TIME_AVG_STA = TIME_AVG_STA + T8_DIFF
                              N_AVG = N_AVG + NP - 1
                              N_AVG_STA = N_AVG_STA + NP - 1
!
!-----------------------------Unwrap phase in the time direction
!
                              CALL ERR_PASS  ( IUER, IER )
                              CALL AMBIG_RESOLVE ( ARA3__PHD, I_NONZERO, &
             &                                     T_NONZERO, PHASE_UNWRAPPED, IER )
                              IF  ( IER .NE. 0 ) THEN
                                   CALL ERR_LOG ( 8851, IUER, 'PIMA_PCST', &
             &                                    'Failure in resolving time unwrapped phase ambiguity')
                                   RETURN
                              ENDIF
!
!-----------------------------Smooth the unwrapped phase with a boxcar filter                                
!

                              CALL BOXCAR_FILTER ( SIG_NUM, I_NONZERO, &
             &                                     T_NONZERO, PHASE_UNWRAPPED, PHASE_SMOOTH )
!
!-----------------------------Evaluate rms in time direction as sqrt( (phase - smoothed phase)^2 )
!
                              DO 840 M4=1,I_NONZERO
                                 ROOT_SQUARE_TIME(M4) = SQRT ( (PHASE_SMOOTH(M4) - PHASE_UNWRAPPED(M4)) ** 2 )      
 840                          CONTINUE
                              IF ( I_NONZERO > 0 ) THEN 
                                  RMS_TIME(M2,IFRQ,IND_STA) = SUM ( ROOT_SQUARE_TIME(1:I_NONZERO) ) / I_NONZERO
                              ELSE 
                                  RMS_TIME(M2,IFRQ,IND_STA) = 0 
                              ENDIF
                              RMS_AVG = RMS_AVG + RMS_TIME(M2,IFRQ,IND_STA)
                              RMS_NUMEL = RMS_NUMEL + 1
                         ENDIF 
 820                  CONTINUE
                 ENDIF
 810          CONTINUE 
!
!-------------Calculate average, variance of time RMS values, turn off tone if RMS too high
!
              RMS_AVG = RMS_AVG / RMS_NUMEL
              RMS_VAR = 0
              IFRQ = 0
              DO 850 M5=PIM%CONF%BEG_FRQ,PIM%CONF%END_FRQ
                 IFRQ = IFRQ + 1
                 IF ( PIM%STA(IND_STA)%PCAL(PIM%CONF%FRQ_GRP)%PCAL_AVAIL .AND. &
             &        PIM%STA(IND_STA)%PCAL(PIM%CONF%FRQ_GRP)%PCAL_USE         ) THEN
                      DO 860 M6=1,M_TONES
                         IF ( PIM%PCAL_MASK(M6,M5,IND_STA) .EQ. 1 .AND. FREQS(M6,IFRQ) .GT. 0  ) THEN
                              RMS_VAR = RMS_VAR + (RMS_TIME(M6,IFRQ,IND_STA) - RMS_AVG) ** 2
                         ENDIF
 860                  CONTINUE
                 ENDIF
 850          CONTINUE
              RMS_VAR = RMS_VAR / (RMS_NUMEL - 1)
              IFRQ = 0
              DO 870 M7=PIM%CONF%BEG_FRQ,PIM%CONF%END_FRQ
                 IFRQ = IFRQ + 1
                 IF ( PIM%STA(IND_STA)%PCAL(PIM%CONF%FRQ_GRP)%PCAL_AVAIL .AND. &
             &        PIM%STA(IND_STA)%PCAL(PIM%CONF%FRQ_GRP)%PCAL_USE         ) THEN
                      DO 880 M8=1,M_TONES
                         IF ( PIM%PCAL_MASK(M8,M7,IND_STA) .EQ. 1 .AND. FREQS(M8,IFRQ) .GT. 0  ) THEN
                              IF ( PIM%CONF%DEBUG_LEVEL >= 8 ) THEN
                                   WRITE ( 6, * ) 'TONE: ', M8
                                   WRITE ( 6, * ) 'IFRQ: ', IFRQ
                                   WRITE ( 6, * ) 'RMS_TIME:', RMS_TIME(M8,IFRQ,IND_STA)
                                   WRITE ( 6, * ) 'RMS_LIMIT', RMS_AVG + SQRT ( RMS_VAR ) * RMS_NSIGMA
                              ENDIF
                              IF ( RMS_TIME(M8,IFRQ,IND_STA) > RMS_AVG + SQRT ( RMS_VAR ) * RMS_NSIGMA ) THEN
                                   PIM%PCAL_MASK(M8,IFRQ,IND_STA) = 0
                                   FAIL_ARR(M8,IFRQ,IND_STA) = FAIL_CPDIFF
                              ENDIF
                         ENDIF
 880                  CONTINUE
                 ENDIF
 870          CONTINUE

         ENDIF         
!
!--------Investigate clock breaks 
!
         M_TONES = PIM%STA(IND_STA)%PCAL(PIM%CONF%FRQ_GRP)%NO_TONES
         IF ( ALLOCATED ( TIME_DET ) ) DEALLOCATE ( TIME_DET )
         IF ( ALLOCATED ( PHASE_CHIX ) ) DEALLOCATE ( PHASE_CHIX )
         IF ( ALLOCATED ( PHASE_CHIY ) ) DEALLOCATE ( PHASE_CHIY )
         IF ( ALLOCATED ( DIFF_PHASE_PLOT ) ) DEALLOCATE ( DIFF_PHASE_PLOT )
         IF ( ALLOCATED ( FIND_IND_FREQ ) ) DEALLOCATE ( FIND_IND_FREQ )
         ALLOCATE ( TIME_DET(NPOI_ACCUM), STAT=IER )
         ALLOCATE ( PHASE_CHIX(NPOI_ACCUM), STAT=IER )
         ALLOCATE ( PHASE_CHIY(NPOI_ACCUM), STAT=IER )
         ALLOCATE ( DIFF_PHASE_PLOT(NPOI_ACCUM - 1), STAT=IER )
         ALLOCATE ( FIND_IND_FREQ(PIM%CONF%END_FRQ - PIM%CONF%BEG_FRQ), STAT=IER ) 
         IF  ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 8853, IUER, 'PIMA_PCST', &
      &                      'Failure to allocate arrays for clock break investigation')
               RETURN
         ENDIF

         TIME_DET = T8(1,1:NPOI_ACCUM,1)
         PHASE_CHIX = 0.0D0
         PHASE_CHIY = 0.0D0
         SIGMA_DIFF = 0.0D0
         SIGMA_DIFF_USE = 0.0D0
         SIGMA_DIFF_MIN = 1000.0D0
         DIFF_PHASE_PLOT = 0.0D0
         IFRQ = 0
         DRIFT = 0.0D0 
         SHIFT = 0.0D0
         COUNT = 0
         COUNT_SIG = 0
         IFRQ_DIFF = 1
         TONE_DIFF = 1
!
!--------Calculate N_SHIFT from average time difference
         TIME_AVG_STA = TIME_AVG_STA / N_AVG_STA
         N_SHIFT = INT ( T_SHIFT / TIME_AVG_STA  )
         IF ( N_SHIFT < 3) N_SHIFT = 3
         
         DO 1010 J1=PIM%CONF%BEG_FRQ,PIM%CONF%END_FRQ
            IFRQ = IFRQ + 1
            INDS_ABOVE(1:M_TONES) = PIM%PCAL_MASK(1:M_TONES,J1,IND_STA) == 1
            DO 1020 J2=1,M_TONES
               IF ( INDS_ABOVE(J2) .EQV. .TRUE. ) THEN 
                    I_TONE = J2
!
                    IND_FREQ = IND_TONE_FREQ(J2)
                    IF ( IND_FREQ == 0 ) THEN
                         GOTO 1020
                    END IF
!                   
!------------------------Calculate chi-squared test statistic by adding epoch-by-epoch differenced
!------------------------relative phase (PHASE_CHI)
!------------------------SIGMA_DIFF is the variance of the noisiest tone --> this is necessary
!------------------------because some tones can be significantly noisier and spoil the chi-squared test
!------------------------In practice, clock breaks will still be detected
!
                    IF (J1 .NE. IND_FREQ .AND. MAXVAL( ABS ( Y8(I_TONE,1:NPOI_ACCUM,IFRQ) ) ) < PHASE_MAX ) THEN
                         COUNT_SIG = 0
                         SIGMA_DIFF = 0.0D0
                         DO 1040 J4=1,NPOI_ACCUM-1
                            PHASE_CHIX(J4) = PHASE_CHIX(J4) + (Y8(I_TONE,J4+1,IFRQ)-Y8(I_TONE,J4,IFRQ)) ** 2 
                            IF ( J4 - N_SHIFT >= 1 .AND. J4 + N_SHIFT <= NPOI_ACCUM ) THEN
                                 PHASE_CHIY(J4) = PHASE_CHIY(J4) + (Y8(I_TONE,J4+N_SHIFT,IFRQ) - Y8(I_TONE,J4-N_SHIFT,IFRQ)) ** 2
                            ENDIF 
                            IF ( (Y8(I_TONE,J4+1,IFRQ) - Y8(I_TONE,J4,IFRQ)) ** 2 > 0 .AND. &
                           &     (Y8(I_TONE,J4+1,IFRQ) - Y8(I_TONE,J4,IFRQ)) ** 2 < PI__NUM / 4) THEN 
                                 COUNT_SIG = COUNT_SIG + 1
                                 SIGMA_DIFF = SIGMA_DIFF + (Y8(I_TONE,J4+1,IFRQ) - Y8(I_TONE,J4,IFRQ)) ** 2
                            ENDIF
 1040                    CONTINUE
                         IF ( COUNT_SIG > 1 ) SIGMA_DIFF = SIGMA_DIFF / (COUNT_SIG - 1)
                         IF ( SIGMA_DIFF > SIGMA_DIFF_USE) THEN
                              SIGMA_DIFF_USE = SIGMA_DIFF
                         ENDIF
                         !SIGMA_DIFF_USE = SIGMA_DIFF_USE + SIGMA_DIFF
                         IF ( SIGMA_DIFF < SIGMA_DIFF_MIN ) THEN
                              SIGMA_DIFF_MIN = SIGMA_DIFF
                              IFRQ_DIFF = IFRQ
                              TONE_DIFF = I_TONE
                         ENDIF
                         
                         ! COUNT tracks the number of IFs & tones that were added to DIFF/DEL rate
                         ! This is the number of degrees of freedom of chi-squared dist.
                         COUNT = COUNT + 1
                    ENDIF
               ENDIF
 1020       CONTINUE
 1010    CONTINUE
         !SIGMA_DIFF_USE = SIGMA_DIFF_USE / COUNT
         ! SIGMA_DIFF_USE is a variance, not a st. dev.
         N_SINCE_DETECT = 5
!--------Find appropriate thresholds for detecting clock breaks 
!--------SIGMA_DET: polynomial fit of chi-squared CDF by degree of freedom for P_A = 10^-6
!--------SIGMA_THRESH: threshold for chi-squared test
         SIGMA_DETX = 22.780665613542475 + 2.692011253135052 * COUNT - 0.03627826253410943 * COUNT ** 2 + &
         &            0.0004955576943092998 * COUNT ** 3 - 0.0000025870752232632106 * COUNT ** 4
         SIGMA_DETY = 18.423071715064864 + 2.55002691257416 * COUNT - 0.03364748902821395 * COUNT ** 2 + &
         &            0.00046147263166883837 * COUNT ** 3 - 0.0000024137416822531333 * COUNT ** 4
         SIGMA_THRESHX = SIGMA_DETX * SIGMA_DIFF_USE
         SIGMA_THRESHY = 2 * N_SHIFT * SIGMA_DETY * SIGMA_DIFF_USE
         COUNT_HIST = 0
         DO 1070 J7=1,NPOI_ACCUM - 1
            IF ( ( PHASE_CHIX(J7) > SIGMA_THRESHX ) &
           &      .AND. (N_SINCE_DETECT >=  5) ) THEN
                 IF ( J7 < NPOI_ACCUM - NUM_SUM - 1 .AND. J7 > NUM_SUM + 1) THEN
                      IF ( PHASE_CHIY(J7)  > SIGMA_THRESHY ) THEN
                           WRITE ( 6,  * ) 'For station ', PIM%C_STA(IND_STA), ' likely clock break near t = ', &
                &                          TIME_DET(J7), ' sec'
!                         ELSE
!                           WRITE ( 6, * ) 'For station ', PIM%C_STA(IND_STA),  &
!                                          ' short-term spurious signal near t = ', &
!                &                          TIME_DET(J7), ' sec'
                      ENDIF
                      N_SINCE_DETECT = 0
                 ENDIF
               ELSE
                 N_SINCE_DETECT = N_SINCE_DETECT + 1
            ENDIF
            IF ( ABS( Y8(TONE_DIFF,J7+1,IFRQ_DIFF) - Y8(TONE_DIFF,J7,IFRQ_DIFF) ) < 6 * SIGMA_DIFF_USE ) THEN
                 COUNT_HIST = COUNT_HIST + 1 
                 DIFF_PHASE_PLOT(COUNT_HIST) = Y8(TONE_DIFF,J7+1,IFRQ_DIFF)-Y8(TONE_DIFF,J7,IFRQ_DIFF)
            ENDIF
 1070    CONTINUE
!--------Develop histogram of relative phase differences to test whether noise is really thermal (Gaussian)
!--------Note that this is includes many tones of potentially different variance, so differences may not 
!--------be Gaussian even if each individual underlying tone is a Gaussian random walk
         DIFF_ARG_HIST = 0.0D0
         PHASE_DIFF_HIST = 0.0D0
         IF ( SIGMA_DIFF_USE > 0 .AND. PIM%CONF%DEBUG_LEVEL >= 8 ) THEN
              ! SCOTT NORMAL REFERENCE RULE
              WRITE ( 6, * ) 'STATION:', PIM%C_STA(IND_STA)
              BIN_DIFF = INT ( 12 * SQRT ( SIGMA_DIFF_MIN ) / (3.49 * SQRT (SIGMA_DIFF_MIN ) / ( COUNT_HIST - 1) ** (1./3)))
              CALL HIST (  COUNT_HIST, DIFF_PHASE_PLOT, BIN_DIFF, -6 * SQRT ( SIGMA_DIFF_MIN ), &
             &             6 * SQRT ( SIGMA_DIFF_MIN ), DIFF_ARG_HIST, PHASE_DIFF_HIST, 1 )
              NORMAL_DIST_DIFF(1:BIN_DIFF) =  1. / (SQRT ( SIGMA_DIFF_MIN ) * SQRT ( 2 * PI__NUM ) ) * EXP ( -1. / 2 &
             &                   * ( DIFF_ARG_HIST(1:BIN_DIFF) / SQRT ( SIGMA_DIFF_MIN ) ) ** 2 ) 
!
              CALL DIAGI_1  ( NPOI_ACCUM, TIME_DET, Y8(TONE_DIFF,1:NPOI_ACCUM,IFRQ_DIFF), IER )
              CALL DIAGI_1  ( COUNT_HIST, TIME_DET, ABS ( DIFF_PHASE_PLOT ), IER )
              CALL DIAGI_2  ( BIN_DIFF, DIFF_ARG_HIST, PHASE_DIFF_HIST, BIN_DIFF, DIFF_ARG_HIST, NORMAL_DIST_DIFF, IER )
              CALL DIAGI_2  ( NPOI_ACCUM-1, TIME_DET, PHASE_CHIX, NPOI_ACCUM, TIME_DET, TIME_DET*0 + SIGMA_THRESHX, IER)
              CALL DIAGI_2  ( NPOI_ACCUM-1, TIME_DET, PHASE_CHIY, NPOI_ACCUM, TIME_DET, TIME_DET*0 + SIGMA_THRESHY, IER)
         ENDIF 
 610  CONTINUE 
      IF ( N_AVG .NE. 0 ) THEN
           TIME_AVG = TIME_AVG / N_AVG ! average time interval over all stations, bands
         ELSE
           TIME_AVG = 0
      ENDIF
!
!-----Write pcal RMS file   
!
      CALL ERR_PASS ( IUER, IER )
      CALL WRI_PCAL_RMS ( PIM, PIM%CONF_FILE, RMS_TIME, RMS_FREQ, TIME_AVG, STA_NAM, IER ) 
      IF  ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 8856, IUER, 'PIMA_PCST','Failure in writing RMS file')
           RETURN
      ENDIF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  PIMA_PCST  !#!  
