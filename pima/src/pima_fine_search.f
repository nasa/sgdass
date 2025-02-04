      SUBROUTINE PIMA_FINE_SEARCH ( PIM, L_MD, L_RT, UV_2D_WEI, WPOI, AP_LEN, &
     &                              K_EPC, MAX_MD, MAX_RT, MD_STEP, RT_STEP,  &
     &                              LTIM, LFRQ, UV, WEI, NOISE_AVR, TIME_FRT, &
     &                              FREQ_ARR, FREQ_REF, TIM_ARR, GRAMBSP,     &
     &                              IND_OBS, GR_DEL, PH_RAT,                  &
     &                              GR_RAT, PH_ACC, PHAS, AMPL, PHAS_ERR,     &
     &                              GR_DEL_ERR, PH_DEL_ERR, PH_RAT_ERR,       &
     &                              GR_RAT_ERR, PH_ACC_ERR, COV_PR_PH,        &
     &                              COV_GR_MD, EFF_DURA, DECOR_TIM, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine PIMA_FINE_SEARCH performs fine search of the fringes       *
! *   within the maxima found with the coarse search. Three algorithms   *
! *   are implemented: parabolic fit, binary convergence or LSQ.         *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *       PIM ( PIMA__TYP ) -- Object with information related to        *
! *                            program PIMA.                             *
! *      L_MD ( INTEGER*4 ) -- The number of samples along group delay   *
! *                            axis .                                    *
! *      L_RT ( INTEGER*4 ) -- The number of samples along phase delay   *
! *                            rate axis.                                *
! * UV_2D_WEI ( COMPLEX*8 ) -- Array of 2D FFT transform of the cross    *
! *                            correlation function.  Dimension:         *
! *                            (L_MD,L_RT).                              *
! *      WPOI ( REAL*8    ) -- Weighted number of points                 *
! *                            ( sum_over(l_md,l_rt) Wei(i,k) )          *
! *    AP_LEN ( REAL*8    ) -- Accumulation period lenth ( sec ).        *
! *                            ( sum_over(l_md,l_rt) Wei(i,k) )          *
! *     K_EPC ( INTEGER*4 ) -- The number of epochs (accumulation        *
! *                            periods ) used for fringe fitting.        *
! *    MAX_MD ( INTEGER*4 ) -- Index at the group delay axis which       *
! *                            corresponds to maximum of DRF found       *
! *                            during coarse search.                     *
! *    MAX_RT ( INTEGER*4 ) -- Index at the phase delay rate axis which  *
! *                            corresponds to maximum of DRF found       *
! *                            during coarse search.                     *
! *   MD_STEP ( INTEGER*4 ) -- Step along the group delay axis in sec.   *
! *   RT_STEP ( INTEGER*4 ) -- Step along phase delay rate axis d/l.     *
! *      LTIM ( INTEGER*4 ) -- Number of accumulation periods.           *
! *      LFRQ ( INTEGER*4 ) -- Number of frequency channels.             *
! *        UV ( COMPLEX*8 ) -- Array of the cross correlation function.  *
! *                            Dimension: (L_MD,L_RT).                   *
! *       WEI ( REAL*4    ) -- Array of weights for each accumulation    *
! *                            period in range [0, 1]. Dimension: LTIM.  *
! * NOISE_AVR ( REAL*8    ) -- Averaged noise over the uv plane, except  *
! *                            the place where the signal is expected.   *
! *  TIME_FRT ( REAL*8    ) -- Fringe reference time from the nominal    *
! *                            start of the observation.                 *
! *  FREQ_ARR ( REAL*8    ) -- Frequency array. Dimension:               *
! *                            (PIM%NCHN,LFRQ).                          *
! *  FREQ_REF ( REAL*8    ) -- Reference frequency.                      *
! *   TIM_ARR ( REAL*8    ) -- Array of time epochs counted from the     *
! *                            1-st epoch (1st epoch is zero).           *
! *                            Units: sec.                               *
! *   GRAMBSP ( REAL*8    ) -- Group delay ambiguity spacing (sec).      *
! *   IND_OBS ( INTEGER*4 ) -- Observation index.                        *
! *                                                                      *
! * _________________________ Output parameters: _______________________ *
! *                                                                      *
! *    GR_DEL ( REAL*8    ) -- Group delay. Dimension: PIM__MFRA.        *
! *    PH_RAT ( REAL*8    ) -- Phase delay rate. Dimension: PIM__MFRA.   *
! *    GR_RAT ( REAL*8    ) -- Group delay rate. Computed only if        *
! *                            PIM%CONF%FRIB_FINE_SEARCH == PIMA__LSQ.   *
! *    PH_ACC ( REAL*8    ) -- Phase delay accelration. Computed only if *
! *                            PIM%CONF%FRIB_FINE_SEARCH == PIMA__ACC.   *
! *      PHAS ( REAL*8    ) -- Fringe phase at reference moment of time  *
! *                            at reference frequency.                   *
! *                            Dimension: PIM__MFRA.                     *
! *      AMPL ( REAL*8    ) -- Amplitude of the cross-correlation        *
! *                            function which corresponds to GR_DEL,     *
! *                            PH_RAT. Dimension: 2. The first value     *
! *                            for the DRF amplitude, the second value   *
! *                            for the LSQ amplitude.                    *
! *   PHAS_ERR ( REAL*8   ) -- Formal uncertainty of phase error (rad).  *
! *                            Dimension: PIM__MFRA.                     *
! * GR_DEL_ERR ( REAL*8   ) -- Formal uncertainty of group delay (sec).  *
! *                            Dimension: PIM__MFRA.                     *
! * PH_DEL_ERR ( REAL*8   ) -- Formal uncertainty of phase delay (sec).  *
! *                            Dimension: PIM__MFRA.                     *
! * PH_RAT_ERR ( REAL*8   ) -- Formal uncertainty of phase delay (d/s).  *
! *                            Dimension: PIM__MFRA.                     *
! * GR_RAT_ERR ( REAL*8   ) -- Formal uncertainty of group delay (d/s).  *
! *                            Computed only if                          *
! *                            PIM%CONF%FRIB_FINE_SEARCH == PIMA__LSQ.   *
! * PH_ACC_ERR ( REAL*8   ) -- Formal uncertainty of phase delay         *
! *                            acceleration (1/s). Computed only if      *
! *                            PIM%CONF%FRIB_FINE_SEARCH == PIMA__ACC.   *
! *  COV_PR_PH ( REAL*8   ) -- Covariance between phase delay rate and   *
! *                            fringe phase. Units: dimensionless.       *
! *  COV_GR_GD ( REAL*8   ) -- Covariance between group delay and group  *
! *                            delay rate. Units: sec.                   *
! *  EFF_DURA  ( REAL*8   ) -- Effective dscan duration, taking into     *
! *                            account weights (sec).                    *
! *  DECOR_TIM ( REAL*8   ) -- Decorrelation factor: the ratio of the    *
! *                            visibility coherently averaged over time  *
! *                            to the visibility incoherently averaged   *
! *                            over time.                                *
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
! *    IUER ( INTEGER*4, OPT ) -- Universal error handler.               *
! *                      Input: IUER=0 -- no error message will be       *
! *                                       printed even in the case       *
! *                                       of error.                      *
! *                             IUER=-1,-2,-3 -- in the case of error    *
! *                                       the message will be put on     *
! *                                       stdout.                        *
! *                             IUER=-3 -- in the case of error after    *
! *                                        printing the error message    *
! *                                        the program will terminate.   *
! *                       Output:                                        *
! *                             if input value of IUER =-2,-3 -- IUER    *
! *                                        is not modified.              *
! *                             otherwise, the output value of IUER is 0 *
! *                             in the case of successful and            *
! *                             positive non-zero in the vase of errors. *
! *                                                                      *
! * ### 15-APR-2006 PIMA_FINE_SEARCH  v6.6 (c) L. Petrov 22-MAR-2022 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'astro_constants.i'
      INCLUDE   'pima.i'
      TYPE     ( PIMA__TYPE  ) :: PIM
      INTEGER*4  L_MD, L_RT, MAX_MD, MAX_RT, LTIM, LFRQ, K_EPC, IND_OBS, IUER
      COMPLEX*8  UV(PIM%NCHN,LFRQ,LTIM), UV_2D_WEI(L_MD,L_RT)
      REAL*8     WPOI, AP_LEN, MD_STEP, RT_STEP, TIME_FRT, &
     &           FREQ_REF, FREQ_ARR(PIM%NCHN,LFRQ), &
     &           TIM_ARR(LTIM), GRAMBSP, GR_DEL(PIM__MFRA), &
     &           PH_RAT(PIM__MFRA), GR_RAT, PHAS(PIM__MFRA), AMPL(PIM__MFRA), &
     &           PHAS_ERR(PIM__MFRA), GR_DEL_ERR(PIM__MFRA), &
     &           PH_DEL_ERR(PIM__MFRA), PH_RAT_ERR(PIM__MFRA), &
     &           GR_RAT_ERR, COV_PR_PH, COV_GR_MD, EFF_DURA, PH_ACC, PH_ACC_ERR
      REAL*8     NOISE_AVR
      REAL*4     WEI(LTIM)
      INTEGER*4  M_PAR, M_PA2
      PARAMETER  ( M_PAR = 4  )
      PARAMETER  ( M_PA2 = (M_PAR*(M_PAR+1))/2 )
      REAL*8     MAX_GD_ERR, MAX_PR_ERR
      PARAMETER  ( MAX_GD_ERR = 4.0D-6 )
      PARAMETER  ( MAX_PR_ERR = 1.D-10 )
      REAL*8     AC_STEP
      REAL*8     NO2_MAT(M_PA2), NOR_MAT(M_PA2), EST(M_PAR), &
     &           NOR_VEC(M_PAR), RC, Q2, Q2_TOT(PIMA__M_SCM), Q2_SIG, SIG, &
     &           SPR_SQ, DELTA_MD, DELTA_RT, SCL_DEF(M_PAR), SCL(M_PAR), &
     &           GRD_SEC_MAX_ARR(PIMA__M_SCM), AMP_SEC_MAX_ARR(PIMA__M_SCM)
      REAL*8     GR_ARR(PIMA__PFS_M_GR), DRF_IF_ARR(PIM__MFRQ,PIMA__PFS_M_GR)
      CHARACTER  NTHR_STR*12
      INTEGER*4  MODE
#ifdef GNU
      INTEGER*4     NTHR, NTHR_OLD, N_PFS_OUT
#else
      ADDRESS__TYPE NTHR, NTHR_OLD, N_PFS_OUT
#endif
      REAL*8,    ALLOCATABLE :: EQU_OBS(:,:), EQU_RH(:), WEI_RH(:), &
     &                          CFRQ_REF(:,:), FREQ_SEG(:,:), &
     &                          TIM_SEG(:), WPOI_SEG(:,:)
      COMPLEX*8, ALLOCATABLE :: UV_SEG(:,:,:)
      DATA       SCL_DEF &
     &               / &
!     &                 1.0D0,  &
!     &                 0.05D0, &
!     &                25.0D0,  &
!     &                 1.0D0   &
!
     &                    0.2D0,   &
     &                    0.001D0, &
     &                   10.0D0,   &
     &                    0.03D0   &
     &               /
      REAL*8       AMP_MIN, PIMA__GR_RAT_CNS_SIG, PIMA__NO_RRAT_CNS_SIG
      PARAMETER  ( AMP_MIN = 1.D-9 )
      PARAMETER  ( PIMA__GR_RAT_CNS_SIG  = 1.D-10 )
      PARAMETER  ( PIMA__NO_RRAT_CNS_SIG = 1.D-3 )
      REAL*8     PHAS_ADD, DECOR_TIM
      REAL*4     PHASE_COARSE, AMP_INCOH
      REAL*8     MD_STEP_BIN, RT_STEP_BIN, MD_BEG, RT_BEG, MD, RT, GR, AC
      COMPLEX*8  UV_USE, DRF, DRF_IF(PIM__MFRQ), DRF_ACC
      REAL*8     A1, A2, A3, B1, B2, B3, C1, C2, C3, AMP_MAX, PHAS_MAX
      REAL*8     PHAS_ADJ, PHRAT_ADJ, GRDEL_ADJ, GRRAT_ADJ, PHACC_ADJ, &
     &           GR_STEP_BIN, GR_BEG, GR_RAT_PREV, GR_DEL_PREV, &
     &           AC_STEP_BIN, AC_BEG, PH_ACC_PREV, AC_DEL_PREV, &
     &           PH_RAT_PREV, PHAS_PREV, &
     &           RES, ACC, WW, WS, WS_ALL, PHS_WRMS, PHS_MOD, SNR, &
     &           REG_MAT(3), PHS_IF, PHS_DIF, ERR_MULT, AMP_SEG, PHS_SEG, &
     &           AMP_PREL, PHS_PREL
      REAL*8     WEI_SQ, SUM_WEI, SUM_FR1, SUM_FR2, SUM_FQ1, SUM_FQ2, &
     &           SUM_FRI, SUM_DFI, FRQ_WEI_CHN, WEI_SCL, SIG_SQ_NEW
      COMPLEX*8  DRF_CHN
      REAL*8     GR_DEL_ARR(PIMA__M_SCM,2),     GR_RAT_ARR(PIMA__M_SCM,2),     &
     &           PHAS_ARR(PIMA__M_SCM,2),       PHAS_DRF_ARR(PIMA__M_SCM,2),   &
     &           PH_ACC_ARR(PIMA__M_SCM,2),     PH_RAT_ARR(PIMA__M_SCM,2),     &
     &           GR_DEL_ERR_ARR(PIMA__M_SCM,2), GR_RAT_ERR_ARR(PIMA__M_SCM,2), &
     &           PHAS_ERR_ARR(PIMA__M_SCM,2),   PH_RAT_ERR_ARR(PIMA__M_SCM,2), &
     &           PH_ACC_ERR_ARR(PIMA__M_SCM,2), AMPL_ARR(PIMA__M_SCM,2), &
     &           WRMS_IF_ARR(PIMA__M_SCM,2),    WRMS_ARR(PIMA__M_SCM,2), &
     &           COV_PR_ARR(PIMA__M_SCM,2),     COV_GR_ARR(PIMA__M_SCM,2)
      REAL*8     SNR_UV_MAX, SNR_UV_OPT, SIG_MIN, PIMA__ACC_PT
      PARAMETER  ( SNR_UV_MAX   = 1.2D0 ) ! rad
      PARAMETER  ( SNR_UV_OPT   = 1.0D0 ) ! rad
      PARAMETER  ( SIG_MIN      = 0.001D0 ) ! rad
      PARAMETER  ( PIMA__ACC_PT = 1.0D0  ) ! How many phase turns due to phase acceleration are allowed
      REAL*8     SNR_UV, SNR_SEG, NOI_SIG_UV, FUN, FUN_MAX
      CHARACTER  STR*80
      INTEGER*4  PIMA__PFS_ITER_NOLSQ, PIMA__PFS_ITER_LSQ, PIMA__PFS_ITER_ACC
      PARAMETER  ( PIMA__PFS_ITER_NOLSQ = 16 )
!!      PARAMETER  ( PIMA__PFS_ITER_LSQ = 8 )
      PARAMETER  ( PIMA__PFS_ITER_LSQ = 24 )
      PARAMETER  ( PIMA__PFS_ITER_ACC = 1 )
      INTEGER*4  MADD_ITER
      PARAMETER  ( MADD_ITER = 7 )
      COMPLEX*8  DRF_ARR(PIMA__PFS_M_GR,PIMA__PFS_M_DL,MAX(PIMA__PFS_M_GR,PIMA__PFS_M_AC,PIMA__PFS_ITER_LSQ))
      LOGICAL*4  FL_END_SEG, FL_ERROR, FL_NO_GRRAT
      LOGICAL*1  FL_FRINGE_DFT, FL_OPT
      INTEGER*4  J1, J2, J3, J4, J5, J6, J7, J8, J9, J10, J11, J12, J13, &
     &           J14, J15, J16, J17, J18, J19, J20, J21, J22, J23, J24, &
     &           J25, J26, J27, J28, J29, J30, J31, IFRA, ITURN, &
     &           M_SCM, KP, IFRQ, I_EQU, IND1, IND2, IND3, &
     &           L_EQU, IND_MD, IND_RT, IND_MAX(3), N_ITER, IAMB, ISUB, &
     &           IND_BEST, IND_FRA, KTIM, PIMA_PFS_M_GR, IER
      INTEGER*4  MUL_CH, MUL_TM, MUL_CH_TM, MUL_CH_MIN, MUL_CH_MAX, &
     &           MUL_TM_MIN, MUL_TM_MAX, LSEG_CH, LSEG_TM, KSEG_TM, &
     &           NUM_CH, NUM_TM, ICH, ITM, N_ITR, KFRQ, &
     &           FIRST_AMPL_FRQ, LAST_AMPL_FRQ
      INTEGER*4, EXTERNAL :: ILEN, I_LEN
#ifdef GNU
      INTEGER*4, EXTERNAL     :: OMP_GET_THREAD_NUM, OMP_GET_NUM_THREADS, OMP_GET_MAX_THREADS
#else
      ADDRESS__TYPE, EXTERNAL :: OMP_GET_THREAD_NUM, OMP_GET_NUM_THREADS, OMP_GET_MAX_THREADS
#endif
      REAL*4,    EXTERNAL :: PHAS_CMPL_R4
      REAL*8,    EXTERNAL :: DP_VV_V, PIMA_AMP_TO_SIGPHS
!
! --- Check for kludge environment variables
!
      CALL GETENVAR ( 'PIMAVAR_FINE_FIRST_AMPL_FRQ', STR )
      IF ( ILEN(STR) > 0 ) THEN
           CALL CHIN ( STR, FIRST_AMPL_FRQ )
         ELSE
           FIRST_AMPL_FRQ = 1
      END IF
!
      CALL GETENVAR ( 'PIMAVAR_FINE_LAST_AMPL_FRQ', STR )
      IF ( ILEN(STR) > 0 ) THEN
           CALL CHIN ( STR, LAST_AMPL_FRQ )
         ELSE
           LAST_AMPL_FRQ = LFRQ
      END IF
!
      CALL GETENVAR ( 'PIMAVAR_FRINGE_DFT', STR )
      IF ( STR == 'YES' .OR. STR == 'yes' ) THEN
           FL_FRINGE_DFT = .TRUE.
           FL_OPT = .FALSE.
         ELSE 
           FL_FRINGE_DFT = .FALSE.
           FL_OPT = .TRUE.
      END IF
!
      CALL GETENVAR ( 'PIMAVAR_NO_GRRAT', STR )
      IF ( STR == 'YES' .OR. STR == 'yes' ) THEN
           FL_NO_GRRAT = .TRUE.
         ELSE 
           FL_NO_GRRAT = .FALSE.
      END IF
!
      IF ( PIM%CONF%DEBUG_LEVEL .EQ. 8 ) THEN
           CALL WALL_TIMER ( %VAL(0) ) 
      END IF           
!
! --- Initialization
!
      PH_ACC     = 0.0D0
      GR_RAT     = 0.0D0
      PHAS_ERR   = 0.0D0
      PH_RAT_ERR = 0.0D0
      PH_ACC_ERR = 0.0D0
      GR_DEL_ERR = 0.0D0
      GR_RAT_ERR = 0.0D0
      COV_PR_PH  = 0.0D0
      COV_GR_MD  = 0.0D0
!
! --- Set the number of threads
!
      NTHR = PIM%CONF%NUM_THREADS
      CALL OMP_SET_NUM_THREADS ( %VAL(NTHR) )
      IF ( PIM%CONF%FRIB_FINE_SEARCH == PIMA__FINE_SEARCH_ACC ) THEN
           MODE = PIMA__ACC  
         ELSE
           MODE = PIMA__GRAT
      END IF
!
! --- Adjust fringe parameters: re-adjst the total fringe phase from FREQ_ARR(1,1) to FREQ_REF 
! --- frequenccy
!
      CALL ERR_PASS ( IUER, IER )
      CALL FRINGE_ADJ ( MODE, LTIM, PIM%NCHN, LFRQ, TIME_FRT, FREQ_ARR, FREQ_REF, &
     &                  WEI, PIM%CONF%FRIB_WEIGHTS_THRESHOLD, TIM_ARR, &
     &                  PHAS, PH_RAT, GR_DEL, PHAS_ADJ, PHRAT_ADJ, GRDEL_ADJ, &
     &                  GRRAT_ADJ, PHACC_ADJ, IER )  
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 7631, IUER, 'PIMA_FINE_SEARCH', 'Error '// &
     &         'in an attempt to adjust results of coarse fringe '// &
     &         'search' )
           RETURN
      END IF
      IF ( PIM%CONF%DEBUG_LEVEL .EQ. 8 ) THEN
           CALL WALL_TIMER ( STR ) 
           WRITE ( 6, '(A)' ) 'PIMA_FINE_SEARCH TIMER_1: '//STR(1:27)
           CALL FLUSH ( 6 )
      END IF
      IF ( PIM%CONF%DEBUG_LEVEL .GE. 6 ) THEN
           WRITE ( 6, 202 ) GR_DEL(1), PH_RAT(1), AMPL(1), GRDEL_ADJ, PHRAT_ADJ
 202       FORMAT ( 'Initial  Gr_del: ', 1PD20.12, ' Ph_rat: ', 1PD20.12, &
     &              ' Ampl: ', 0PF10.7 / &
     &              'Adjusted Gr_del: ', 1PD20.12, ' Ph_rat: ', 1PD20.12 )
      END IF
!
! --- Allocate memory for intermediate frequency arrays needed for
! --- speeding up calculations
!
      ALLOCATE ( CFRQ_REF(PIM%NCHN,LFRQ), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( 8*LTIM, STR )
           CALL ERR_LOG ( 7632, IUER, 'PIMA_FINE_SEARCH', 'Error '// &
     &         'in an attempt to allocate '//STR(1:I_LEN(STR))// &
     &         ' bytes of dynamic memory for array CFRQ_REF' )
           RETURN
      END IF
!
      IFRQ = 0
      WEI_SQ  = 0.0D0
      SUM_WEI = 0.0
      SUM_FR1 = 0.0
      SUM_FR2 = 0.0
      SUM_FQ1 = 0.0
      SUM_FQ2 = 0.0
      SUM_FRI = 0.0
      SUM_DFI = 0.0
      DRF_ACC = 0.0
!
! --- It is worth to remind that the phase of the coarse fringe searach
! --- was computed at the TIM_ARR(1) epoch, while the fine fringe
! --- phase is computed at TIME_FRT epoch
!
      DO 410 J1=PIM%CONF%BEG_FRQ,PIM%CONF%END_FRQ
         IFRQ = IFRQ + 1
         DO 420 J2=1,PIM%NCHN
            DRF_CHN = CMPLX ( 0.0, 0.0 )
            IF ( IFRQ == 1  .AND.  J2 == 1 ) THEN
                 FRQ_WEI_CHN = 0.0D0
                 EFF_DURA = 0.0D0
                 KTIM = 0
            END IF
            DO 430 J3=1,LTIM
!
! ------------ PHAS_ADD -- phase rotation for adjustments to phase, phase delay rate,
! ------------             group delay, and group delay rate or phase acceleration
!
               IF ( MODE == PIMA__GRAT ) THEN
                    PHAS_ADD = - PHAS_ADJ &
     &                         + PHRAT_ADJ*PI2*FREQ_REF* &
     &                                    (TIM_ARR(J3) - TIME_FRT) &
     &                         + GRDEL_ADJ*PI2*(FREQ_ARR(J2,IFRQ)-FREQ_REF) &
     &                         + GRRAT_ADJ*PI2*(FREQ_ARR(J2,IFRQ)-FREQ_REF)* &
     &                                         (TIM_ARR(J3) - TIME_FRT)
                  ELSE
                    PHAS_ADD = - PHAS_ADJ &
     &                         + PHRAT_ADJ*PI2*FREQ_REF* &
     &                                    (TIM_ARR(J3) - TIME_FRT) &
     &                         + GRDEL_ADJ*PI2*(FREQ_ARR(J2,IFRQ)-FREQ_REF) &
     &                         + PHACC_ADJ*PI2*FREQ_REF* &
     &                                    (TIM_ARR(J3) - TIME_FRT)**2/2.0D0
               END IF
!
! ------------ Perform phase rotation
!
               DRF_CHN = DRF_CHN + UV(J2,IFRQ,J3)*WEI(J3)* &
     &                             CMPLX ( COS(PHAS_ADD), SIN(PHAS_ADD) )
               IF ( IFRQ == 1  .AND.  J2 == 1 ) THEN
                    FRQ_WEI_CHN = FRQ_WEI_CHN + WEI(J3)
                    EFF_DURA = EFF_DURA + WEI(J3)*AP_LEN
                    IF ( WEI(J3) > PIM%CONF%FRIB_WEIGHTS_THRESHOLD ) THEN
                         KTIM = KTIM + 1
                    END IF
               END IF
!
! ------------ Accumulate rotated visibilities
!
               DRF_ACC = DRF_ACC + UV(J2,IFRQ,J3)*WEI(J3)* &
     &                             CMPLX ( COS(PHAS_ADD), SIN(PHAS_ADD) )
 430        CONTINUE
            CFRQ_REF(J2,IFRQ) = PI2* ( FREQ_ARR(J2,IFRQ) - FREQ_REF )
!
! --------- Square of weights is proportional to the number of
! --------- processed samples
!
            WEI_SQ = FRQ_WEI_CHN*ABS(DRF_CHN)
            DRF = FREQ_ARR(J2,IFRQ) - FREQ_REF
            SUM_WEI = SUM_WEI + WEI_SQ
            SUM_FR1 = SUM_FR1 + WEI_SQ*DRF
            SUM_FR2 = SUM_FR2 + WEI_SQ*DRF**2
            SUM_FQ1 = SUM_FQ1 + WEI_SQ*FREQ_ARR(J2,IFRQ)
            SUM_FQ2 = SUM_FQ2 + WEI_SQ*FREQ_ARR(J2,IFRQ)**2
            SUM_FRI = SUM_FRI + WEI_SQ/FREQ_ARR(J2,IFRQ)
            SUM_DFI = SUM_DFI + WEI_SQ*DRF/FREQ_ARR(J2,IFRQ)
 420     CONTINUE
 410  CONTINUE
!
! --- Compute preliminary fringe amplitude and phase 
!
      IF ( WPOI > 0.0 ) THEN
           AMP_PREL = ABS(DRF_ACC)/WPOI
           PHS_PREL = PHAS_CMPL_R4(DRF_ACC) + PHAS_ADJ 
         ELSE
           AMP_PREL = 0.0
           PHS_PREL = 0.0
      END IF
!
      IF ( PIM%CONF%FRIB_FINE_SEARCH == PIMA__FINE_SEARCH_NO ) THEN
!
! ======== No fine search.
!
           IF ( MAX_MD < L_MD/2 ) THEN
                GR_DEL(PIMA__DRF) = MD_STEP*(MAX_MD-1)
              ELSE
                GR_DEL(PIMA__DRF) = MD_STEP*(MAX_MD-L_MD-1)
           END IF
           IF ( MAX_RT < L_RT/2 ) THEN
                PH_RAT(PIMA__DRF) = RT_STEP*(MAX_RT-1)
              ELSE
                PH_RAT(PIMA__DRF) = RT_STEP*(MAX_RT-L_RT-1)
           END IF
           GR_RAT  = 0.D0
           IND_FRA = PIMA__DRF
           PHAS    = PHAS_ADJ
           GR_DEL  = GRDEL_ADJ
           PH_RAT  = PHRAT_ADJ
           AMPL    = AMPL(PIMA__DRF)
!
! -------- Compute the SNR, phase error and phase delay error
!
           SNR     = AMPL(PIMA__DRF)/NOISE_AVR
           IF ( SNR > 1.0D0 ) THEN
                PHAS_ERR = 1.D0/SNR
              ELSE 
                PHAS_ERR = PI__NUM/DSQRT(3.0D0)
           END IF
           PH_DEL_ERR = PHAS_ERR(PIMA__DRF)/(PI2*FREQ_REF)
           GR_RAT  = 0.0D0
         ELSE IF ( PIM%CONF%FRIB_FINE_SEARCH == PIMA__FINE_SEARCH_PAR ) THEN
!
! ======== Fine search using parabolic fit through three points of the
! ======== the delay resolution function around the maxima.
!
! -------- The search of the maxima is performed separately
! -------- for delay and delay rate.
! -------- The Lagrange interpolating polynomial is computed, and the linear
! -------- equation which equalizes the derivative to zero is solved
!
           IND1 = MAX_MD-1
           IND2 = MAX_MD
           IND3 = MAX_MD+1
           IF ( IND1 .LE. 0    ) IND1 = IND1 + L_MD
           IF ( IND1 .GT. L_MD ) IND1 = IND1 - L_MD
           IF ( IND2 .LE. 0    ) IND2 = IND2 + L_MD
           IF ( IND2 .GT. L_MD ) IND2 = IND2 - L_MD
           IF ( IND3 .LE. 0    ) IND3 = IND3 + L_MD
           IF ( IND3 .GT. L_MD ) IND3 = IND3 - L_MD
!
! -------- A1, A2, A3 is the ampltitude before, at the maximum, 
! -------- and after the group delay maximum
!
           A1 =  ABS(UV_2D_WEI(IND1,MAX_RT))/WPOI
           A2 =  ABS(UV_2D_WEI(IND2,MAX_RT))/WPOI
           A3 =  ABS(UV_2D_WEI(IND3,MAX_RT))/WPOI
           B1 = -1.0D0
           B2 = -2.0D0
           B3 = -1.0D0
           C1 =  1.0D0
           C2 =  0.0D0
           C3 = -1.0D0
!
! -------- Solution of this equation is the correction to multi-band group delay
! -------- which provides the maxima to the delay resolution function
!
           DELTA_MD = (A1*B1*C1 - A2*B2*C2 + A3*B3*C3)/ &
     &                (A1*B1    - A2*B2    + A3*B3   )/2.0D0
!
! -------- Now the same operation for delay rate
!
           IND1 = MAX_RT-1
           IND2 = MAX_RT
           IND3 = MAX_RT+1
           IF ( IND1 .LE. 0    ) IND1 = IND1 + L_RT
           IF ( IND1 .GT. L_RT ) IND1 = IND1 - L_RT
           IF ( IND2 .LE. 0    ) IND2 = IND2 + L_RT
           IF ( IND2 .GT. L_RT ) IND2 = IND2 - L_RT
           IF ( IND3 .LE. 0    ) IND3 = IND3 + L_RT
           IF ( IND3 .GT. L_RT ) IND3 = IND3 - L_RT
!
! -------- A1, A2, A3 is the ampltitude before, at the maximum, 
! -------- and after the phase delay rate maximum
!
           A1 =  ABS(UV_2D_WEI(MAX_MD,IND1))/WPOI
           A2 =  ABS(UV_2D_WEI(MAX_MD,IND2))/WPOI
           A3 =  ABS(UV_2D_WEI(MAX_MD,IND3))/WPOI
           B1 = -1.0D0
           B2 = -2.0D0
           B3 = -1.0D0
           C1 =  1.0D0
           C2 =  0.0D0
           C3 = -1.0D0
!
! -------- Solution of this equation is a correction to the phase delay rate
! -------- which provided the maxima to the delay resolution function 
! -------- during coarse fringe search
!
           IF ( DABS(A1*B1 - A2*B2 + A3*B3) > 1.D-18 ) THEN
                DELTA_RT = (A1*B1*C1 - A2*B2*C2 + A3*B3*C3)/ &
     &                     (A1*B1    - A2*B2    + A3*B3   )/2.0D0
              ELSE 
                DELTA_RT = 0.0D0
           END IF
!
           IF ( MAX_MD + DELTA_MD < L_MD/2 ) THEN
                GR_DEL(PIMA__DRF) = MD_STEP*(MAX_MD + DELTA_MD - 1)
              ELSE
                GR_DEL(PIMA__DRF) = MD_STEP*(MAX_MD + DELTA_MD - L_MD - 1)
           END IF
           IF ( MAX_RT + DELTA_RT < L_RT/2 ) THEN
                PH_RAT(PIMA__DRF) = RT_STEP*(MAX_RT + DELTA_RT - 1)
              ELSE
                PH_RAT(PIMA__DRF) = RT_STEP*(MAX_RT + DELTA_RT - L_RT - 1)
           END IF
           GR_RAT  = 0.D0
           IND_FRA = PIMA__DRF
           PHAS    = PHAS(IND_FRA)
           GR_DEL  = GR_DEL(IND_FRA)
           PH_RAT  = PH_RAT(IND_FRA)
           GR_RAT  = 0.0D0
           PH_ACC  = 0.0D0
!
! -------- NB: we do not adjust ampltude in this mode. 
! -------- Probably, we shood
!
           AMPL    = AMPL(PIMA__DRF)
!
! -------- Compute the SNR, phase error and phase delay error
!
           SNR     = AMPL(IND_FRA)/NOISE_AVR
           IF ( SNR > 1.0D0 ) THEN
                PHAS_ERR = 1.D0/SNR
              ELSE 
                PHAS_ERR = PI__NUM/DSQRT(3.0D0)
           END IF
           PH_DEL_ERR = PHAS_ERR(PIMA__DRF)/(PI2*FREQ_REF)
         ELSE IF ( PIM%CONF%FRIB_FINE_SEARCH == PIMA__FINE_SEARCH_BIN .OR. &
     &             PIM%CONF%FRIB_FINE_SEARCH == PIMA__FINE_SEARCH_ACC .OR. &
     &             PIM%CONF%FRIB_FINE_SEARCH == PIMA__FINE_SEARCH_LSQ      ) THEN
!
! ======== Refinement with using the binary division method
!
           MD_STEP_BIN = MD_STEP*2.0D0/(PIMA__PFS_M_DL-1)
           RT_STEP_BIN = RT_STEP*2.0D0/(PIMA__PFS_M_RT-1)
           GR_STEP_BIN = PIMA__GR_STEP
           AC_STEP_BIN = PIMA__ACC_PT*8.0D0/((LTIM*AP_LEN)**2*FREQ_ARR(1,1))/((PIMA__PFS_M_AC-1)/2)
!
! -------- Set initial values of the new M_GR x M_DL x M_RT gird.
! -------- Keep the the current maximum at the center of the new grid
!
           GR_DEL_PREV = GRDEL_ADJ
           PH_RAT_PREV = PHRAT_ADJ
           GR_RAT_PREV = 0.0D0
           PH_ACC_PREV = 0.0
!
! -------- Set starting points of the new finer grid
!
           MD_BEG = GR_DEL_PREV - MD_STEP_BIN*(PIMA__PFS_M_DL-1)/2.0D0
           RT_BEG = PH_RAT_PREV - RT_STEP_BIN*(PIMA__PFS_M_RT-1)/2.0D0
           GR_BEG = GR_RAT_PREV - GR_STEP_BIN*(PIMA__PFS_M_GR-1)/2.0D0
           AC_BEG = PH_ACC_PREV - AC_STEP_BIN*(PIMA__PFS_M_AC-1)/2.0D0
!
           IF ( PIM%CONF%DEBUG_LEVEL .GE. 6 ) THEN
                WRITE ( 6, 210 ) 0, GRDEL_ADJ, PHRAT_ADJ, GR_RAT_PREV, &
     &                           PH_ACC_PREV, AMPL(PIMA__DRF), 0, 0, 0
 210            FORMAT ( 'PFS iter: ',I2,' MD= ',1PD20.12, &
     &                   ' RT= ', 1PD20.12, ' GR= ', 1PD14.6, ' AC= ', 1PD14.6, &
     &                   ' AMPL= ', 0PF10.7, ' Max_MRG: ', &
     &                   I3, 1X, I3, 1X, I4 )
           END IF
!
! -------- NB: the number of iterations is different, whether the LSQ
! -------- refinement will be used or not.
!
           IF ( PIM%CONF%FRIB_FINE_SEARCH == PIMA__FINE_SEARCH_BIN ) THEN
                N_ITR = PIMA__PFS_ITER_NOLSQ 
                N_PFS_OUT = PIMA__PFS_M_GR
                IF ( FL_NO_GRRAT ) THEN
                     N_ITR     = 1
                     N_PFS_OUT = 1
                END IF
              ELSE IF ( PIM%CONF%FRIB_FINE_SEARCH == PIMA__FINE_SEARCH_ACC ) THEN
                N_ITR = PIMA__PFS_ITER_ACC
                N_PFS_OUT = PIMA__PFS_M_AC
              ELSE IF ( PIM%CONF%FRIB_FINE_SEARCH == PIMA__FINE_SEARCH_LSQ ) THEN
                N_ITR = PIMA__PFS_ITER_LSQ
                N_PFS_OUT = PIMA__PFS_M_GR
                IF ( FL_NO_GRRAT ) THEN
                     N_ITR     = 1
                     N_PFS_OUT = 1
                END IF
           END IF
           IF ( PIM%CONF%DEBUG_LEVEL .EQ. 8 ) THEN
                CALL WALL_TIMER ( %VAL(0) ) 
           END IF           
!
! -------- A special trick: if the user selected threaded application,
! -------- in order to reach maximum performace, 
! -------- we need to reset the number of threads to PIMA__PFS_M_GR
!
           IF ( NTHR > 1 ) THEN
                NTHR_OLD = OMP_GET_MAX_THREADS() ! Store the current number of threads
                CALL OMP_SET_NUM_THREADS ( %VAL(N_PFS_OUT) )
           END IF
!
           DO 440 J4=1,N_ITR ! Circle over iterations
!
! ----------- Set the current indexes of the maximum
!
              IND_MAX(1) = 1 + (N_PFS_OUT-1)/2
              IND_MAX(2) = 2
              IND_MAX(3) = 2
              AMP_MAX    = -1.0
!
! ----------- Cycle over group delay rate or phase delay accelration
!
!$OMP PARALLEL DO IF ( NTHR > 1 ), &
!$OMP&         PRIVATE ( J5, J6, J7, RT, MD, GR, AC, DRF_IF ), &
!$OMP&         SHARED  ( FL_OPT, FL_NO_GRRAT ), &
!$OMP&         SCHEDULE ( GUIDED )
              DO 450 J5=1,N_PFS_OUT
                 GR = GR_BEG + GR_STEP_BIN*(J5-1)
                 IF ( FL_NO_GRRAT ) THEN
                      GR = 0.0D0
                 END IF
                 AC = AC_BEG + AC_STEP_BIN*(J5-1)
                 DO 460 J6=1,PIMA__PFS_M_DL
!
! ----------------- Cycle over group delay
!
                    MD = MD_BEG + MD_STEP_BIN*(J6-1)
                    DO 470 J7=1,PIMA__PFS_M_RT ! cycle over delay rate
!
! -------------------- Cycle over phase delay rate
!
                       RT = RT_BEG + RT_STEP_BIN*(J7-1)
!
! -------------------- Compute delay resolution function with in the group delay rate 
! -------------------- or acceleration mode
!
                       CALL PIMA_UV_DRF3 ( MODE, FL_OPT, LTIM, PIM%NCHN, LFRQ, WEI, TIM_ARR, &
     &                                     PIM%CONF%FRIB_WEIGHTS_THRESHOLD, &
     &                                     TIME_FRT, RT, MD, GR, AC, FREQ_ARR, &
     &                                     FREQ_REF, CFRQ_REF, UV, &
     &                                     DRF_ARR(J7,J6,J5), DRF_IF )
 470                CONTINUE
 460             CONTINUE
 450          CONTINUE
!$OMP END PARALLEL DO
!
! ----------- Find the indices on the 3D grid that provide the maximum.
! ----------- Store the inideces, phase fringe and fringe amplitude
! ----------- at maximum.
!
              DO 550 J5=1,N_PFS_OUT
                 DO 560 J6=1,PIMA__PFS_M_DL
                    DO 570 J7=1,PIMA__PFS_M_RT 
                       IF ( ABS(DRF_ARR(J7,J6,J5))/WPOI > AMP_MAX ) THEN
                            AMP_MAX = ABS(DRF_ARR(J7,J6,J5))/WPOI
                            PHAS_MAX = PHAS_CMPL_R4 ( DRF_ARR(J7,J6,J5) )
                            IND_MAX(1) = J7
                            IND_MAX(2) = J6
                            IND_MAX(3) = J5
                       END IF
 570                CONTINUE
 560             CONTINUE
 550          CONTINUE
!
              IF ( PIM%CONF%FRIB_FINE_SEARCH == PIMA__FINE_SEARCH_BIN .OR. &
     &             PIM%CONF%FRIB_FINE_SEARCH == PIMA__FINE_SEARCH_LSQ ) THEN
!
! ---------------- Update group delay rate 
!
                   GR_RAT = GR_BEG + GR_STEP_BIN*(IND_MAX(3)-1)
                 ELSE IF ( PIM%CONF%FRIB_FINE_SEARCH == PIMA__FINE_SEARCH_ACC ) THEN
!
! ---------------- or phase accleration
!
                   PH_ACC = AC_BEG + AC_STEP_BIN*(IND_MAX(3)-1)
              END IF
              IF ( FL_NO_GRRAT ) THEN
                   GR_RAT = 0.0D0
              END IF 
              AMPL(PIMA__DRF) = AMP_MAX
              PHAS(PIMA__DRF) = PHAS_MAX
              GR_DEL(PIMA__DRF) = MD_BEG + MD_STEP_BIN*(IND_MAX(2)-1)
              PH_RAT(PIMA__DRF) = RT_BEG + RT_STEP_BIN*(IND_MAX(1)-1)
!
! ----------- Reduce the step of the grid
!
              GR_STEP_BIN = GR_STEP_BIN/(PIMA__PFS_M_GR-1)
              AC_STEP_BIN = AC_STEP_BIN/(PIMA__PFS_M_AC-1)
              MD_STEP_BIN = MD_STEP_BIN/(PIMA__PFS_M_DL-1)
              RT_STEP_BIN = RT_STEP_BIN/(PIMA__PFS_M_RT-1)
!
              AC_BEG = PH_ACC            - (PIMA__PFS_M_AC-1)/2.0D0*AC_STEP_BIN
              GR_BEG = GR_RAT            - (PIMA__PFS_M_GR-1)/2.0D0*GR_STEP_BIN
              MD_BEG = GR_DEL(PIMA__DRF) - (PIMA__PFS_M_DL-1)/2.0D0*MD_STEP_BIN
              RT_BEG = PH_RAT(PIMA__DRF) - (PIMA__PFS_M_RT-1)/2.0D0*RT_STEP_BIN
!
              PHAS_PREV   = PHAS_MAX
              PH_ACC_PREV = PH_ACC
              GR_RAT_PREV = GR_RAT
              GR_DEL_PREV = GR_DEL(PIMA__DRF)
              PH_RAT_PREV = PH_RAT(PIMA__DRF)
!
! ----------- Check whether further iterations are needed
!
              IF ( J4 == N_ITR .OR. &
     &             ( MD_STEP_BIN < PIMA__MD_ERR_LIM  .AND. &
     &               RT_STEP_BIN < PIMA__RT_ERR_LIM        ) ) THEN
!
! ---------------- Well, we have reached the last iteration or found 
! ---------------- the delay and delay rate have converged
!
                   PHAS(PIMA__DRF) = PHAS_CMPL_R4 ( DRF_ARR(IND_MAX(1),IND_MAX(2),IND_MAX(3)) )
                   IF ( PIM%CONF%DEBUG_LEVEL .GE. 15 ) THEN
                        WRITE ( 6, 210 ) J4, GR_DEL(PIMA__DRF), &
     &                                   PH_RAT(PIMA__DRF), GR_RAT, PH_ACC, &
     &                                   AMPL(PIMA__DRF), IND_MAX(2), &
     &                                   IND_MAX(1), IND_MAX(3)
                   END IF
                   GOTO 840
              END IF
              IF ( PIM%CONF%DEBUG_LEVEL .GE. 6 ) THEN
                   WRITE ( 6, 210 ) J4, GR_DEL(PIMA__DRF), &
     &                              PH_RAT(PIMA__DRF), GR_RAT, PH_ACC, &
     &                              AMPL(PIMA__DRF), IND_MAX(2), &
     &                              IND_MAX(1), IND_MAX(3)
              END IF
  440      CONTINUE
  840      CONTINUE
!
           IF ( NTHR > 1 ) THEN
!
! ------------- Restore the number of threads
!
                CALL OMP_SET_NUM_THREADS ( %VAL(NTHR_OLD) )
           END IF
!
! -------- Compute the SNR
!
           SNR = AMPL(PIMA__DRF)/NOISE_AVR
           IND_FRA = PIMA__DRF
           IF ( PIM%CONF%DEBUG_LEVEL .EQ. 8 ) THEN
                CALL WALL_TIMER ( STR ) 
                WRITE ( 6, '(A)' ) 'PIMA_FINE_SEARCH TIMER_2: '//STR(1:27)
                CALL FLUSH ( 6 )
           END IF
      END IF
!
      IF ( ( PIM%CONF%FRIB_FINE_SEARCH == PIMA__FINE_SEARCH_ACC .OR. &
     &       PIM%CONF%FRIB_FINE_SEARCH == PIMA__FINE_SEARCH_LSQ      ) .AND. &
     &       K_EPC > 1 ) THEN
!
! ====== Fine search using the least square method over individual
! ====== fringe phases around the maxima of the delay resolution finction
!
! ------ Compute
! ------ SIG_UV  -- a rough esimates of fringe phase vairance of
! ------            an elementary sample
! ------ SIG_IF  -- a rough esimates of fringe phase vairance averaged
! ------            over time and and over spectral channels within in IF
! ------
! ------ NB! We have to divide SNR by DSQRT(PI__NUM/2.0) = 1.253 in order to convert
! ------ the SNR with respect ot the average ampltude to the SNR with respect
! ------ to variance of noise in real or image part of the cross-correlation
! ------ function
!
         NOI_SIG_UV = NOISE_AVR/DSQRT(P2I)*DSQRT(WPOI)
         SNR= AMPL(PIMA__DRF)/NOISE_AVR
         SNR_UV = DSQRT(P2I)*SNR/DSQRT(WPOI)
         IF ( PIM%CONF%DEBUG_LEVEL .EQ. -6 ) THEN
              WRITE ( 6, * ) 'SSSS SNR= ', SNGL(SNR), ' SNR_UV= ', SNGL(SNR_UV)
         END IF
!
         IF ( SNR_UV > SNR_UV_MAX ) THEN
!
! ----------- The elementary SNR is high enough to use original samples
! ----------- without aggregation
!
              MUL_CH = 1
              MUL_TM = 1
              LSEG_CH = PIM%NCHN
              LSEG_TM = KTIM
            ELSE
!
! ----------- The elemntary SNR is too low. We have to find optimal
! ----------- aggregation to produce segemtns with enough SNR for
! ----------- subsequent LSQ fitting
!
              MUL_CH_MIN = 1
              MUL_CH_MAX = MIN ( PIM%NCHN/2, IDINT ( 1.0D0/SNR_UV**2 ) + 1 )
              MUL_TM_MIN = 1
              MUL_TM_MAX = MIN ( KTIM/2, IDINT ( 1.0D0/SNR_UV**2 ) + 1 )
!
              MUL_CH = PIM%NCHN/2
              MUL_TM = KTIM/2
!
! ----------- Find the optimal aggregation over spectral channels (MUL_CH)
! ----------- and over time (MUL_TM). We try to keep a balance between
! ----------- aggegation over frequency channels and ovrer time
!
              KSEG_TM =  0.0
              FUN_MAX = -1.0
              DO 490 J9=MUL_CH_MIN, MUL_CH_MAX
                 DO 4100 J10=MUL_TM_MIN,MUL_TM_MAX
                    SNR_SEG = SNR_UV*DSQRT(1.0D0*J9*J10)
                    IF ( SNR_SEG > SNR_UV_OPT ) THEN
!
! ---------------------- Aggregation over J9  spectral channels and 
! ----------------------                  J10 accummulation periods acceptable
!
                         FUN = 1.0D0*((PIM%NCHN/J9)*(KTIM/J10))
!
! ---------------------- Set a slite preference for aggregation over frequency
!
                         IF ( (PIM%NCHN/J9) < (KTIM/J10) ) FUN = FUN + 0.4D0
                         IF ( FUN > FUN_MAX ) THEN
                              MUL_CH = J9
                              MUL_TM = J10
                              FUN_MAX = FUN
                         END IF
                    END IF
 4100             CONTINUE
 490           CONTINUE
!
! ------------ Store the number of segments over frequency and over time. 
! ------------ NB: integer rounding
!
               LSEG_CH = PIM%NCHN/MUL_CH
               LSEG_TM = KTIM/MUL_TM
         END IF
         IF ( LSEG_TM < 2 ) THEN
!
! ----------- We got too few time segments? Least square adjustement is not 
! ----------- feasible. Let us store "best" phase apllitude, group delay
! ----------- and and phase delay deterimined by the binary division method
! ----------- and jump out
!
              AMPL(PIMA__LSQ) = AMPL(PIMA__DRF)
              PHAS(PIMA__LSQ) = PHAS(PIMA__DRF)
              GR_DEL(PIMA__LSQ) = GR_DEL(PIMA__DRF)
              PH_RAT(PIMA__LSQ) = PH_RAT(PIMA__DRF)
              GOTO 8280
         END IF
!
! ------ Store the SNR of the elementary segment
!
         SNR_SEG = SNR_UV*DSQRT(1.0D0*MUL_CH*MUL_TM)
!
! ------ Allocate memory for intermediate frequency arrays needed for
! ------ speeding up calculations
!
         ALLOCATE ( FREQ_SEG(LSEG_CH,LFRQ), STAT=IER )
         IF ( IER .NE. 0 ) THEN
              CALL CLRCH ( STR )
              CALL INCH  ( 8*LSEG_CH*LFRQ, STR )
              CALL ERR_LOG ( 7633, IUER, 'PIMA_FINE_SEARCH', 'Error '// &
     &            'in an attempt to allocate '//STR(1:I_LEN(STR))// &
     &            ' bytes of dynamic memory for array FREQ_SEG' )
              RETURN
         END IF
!
         ALLOCATE ( UV_SEG(LSEG_CH,LFRQ,LSEG_TM), STAT=IER  )
         IF ( IER .NE. 0 ) THEN
              CALL CLRCH ( STR )
              CALL INCH  ( 8*LSEG_CH*LFRQ*LSEG_TM, STR )
              CALL ERR_LOG ( 7634, IUER, 'PIMA_FINE_SEARCH', 'Error '// &
     &            'in an attempt to allocate '//STR(1:I_LEN(STR))// &
     &            ' bytes of dynamic memory for array UV_SEG' )
              RETURN
         END IF
         ALLOCATE ( TIM_SEG(LSEG_TM), STAT=IER  )
         IF ( IER .NE. 0 ) THEN
              CALL CLRCH ( STR )
              CALL INCH  ( 8*LSEG_TM, STR )
              CALL ERR_LOG ( 7635, IUER, 'PIMA_FINE_SEARCH', 'Error '// &
     &            'in an attempt to allocate '//STR(1:I_LEN(STR))// &
     &            ' bytes of dynamic memory for array UV_SEG' )
              RETURN
         END IF
         ALLOCATE ( WPOI_SEG(LSEG_CH,LSEG_TM), STAT=IER  )
         IF ( IER .NE. 0 ) THEN
              CALL CLRCH ( STR )
              CALL INCH  ( 8*LSEG_TM, STR )
              CALL ERR_LOG ( 7636, IUER, 'PIMA_FINE_SEARCH', 'Error '// &
     &            'in an attempt to allocate '//STR(1:I_LEN(STR))// &
     &            ' bytes of dynamic memory for array UV_SEG' )
              RETURN
         END IF
!
! ------ Initialization
!
         FREQ_SEG = 0.0
         UV_SEG   = 0.0
         TIM_SEG  = 0.0
         SCL = SCL_DEF/LSEG_TM
!
         ITM = 1
         NUM_TM = 1
         WPOI_SEG = 0.0D0
         FL_END_SEG = .FALSE.
         KTIM = 0
!
! ------ Cycle over time
!
         DO 4110 J11=1,LTIM
!
! --------- ITM    -- segment index over time
! --------- KTIM   -- Counter of processed accumulation periods 
! ---------           (some AP may be skipped because of low weught)  
! --------- NUM_TM -- Index of the AP since the first AP of the current segment
! ---------           Index starts from 1
!
            IF ( WEI(J11) < PIM%CONF%FRIB_WEIGHTS_THRESHOLD ) THEN
                 WEI_SCL = 0.0D0
               ELSE
                 WEI_SCL = 1.0D0
                 KTIM = KTIM + 1
            END IF
!
! --------- Decide whether this accumulation period is the last in the 
! --------- ITM -th segment
!
            IF ( J11 == LTIM ) THEN
                 FL_END_SEG = .TRUE.
               ELSE
                 IF ( SNR_UV*DSQRT(WPOI_SEG(1,ITM) + MUL_CH*WEI_SCL*WEI(J11)) > &
     &                SNR_UV_OPT  .AND.  ITM < LSEG_TM ) THEN
                      FL_END_SEG = .TRUE.
                 END IF
            END IF
!
! --------- Cycle over IFs
!
            IFRQ = 0
            DO 4120 J12=PIM%CONF%BEG_FRQ,PIM%CONF%END_FRQ
               IFRQ = IFRQ + 1
!
! ------------ Cycle over frequency segments
!
               DO 4130 J13=1,LSEG_CH
                  IF ( J13 == LSEG_CH ) THEN
                       NUM_CH = PIM%NCHN - (J13-1)*MUL_CH
                     ELSE
                       NUM_CH = MUL_CH
                  END IF
                  DO 4140 J14=1,NUM_CH
!
! ------------------ ICH -- index of the frequency channel in the J13-th frequency 
! ------------------        segment of the J12-th IF
!
                     ICH = J14 + (J13-1)*MUL_CH
                     IF ( J11 == 1 ) THEN
                          FREQ_SEG(J13,IFRQ) = FREQ_SEG(J13,IFRQ) + FREQ_ARR(ICH,IFRQ)/NUM_CH
                     END IF
!
! ------------------ Apply results of the coarse fringe search to generate
! ------------------ segmented visibility
!
                     IF ( PIM%CONF%FRIB_FINE_SEARCH == PIMA__FINE_SEARCH_LSQ ) THEN
                          PHAS_ADD = - PHAS_PREV &
     &                               + PH_RAT_PREV*PI2*FREQ_REF* &
     &                                                 (TIM_ARR(J11) - TIME_FRT) &
     &                               + GR_DEL_PREV*PI2*(FREQ_ARR(ICH,IFRQ) - FREQ_REF) &
     &                               + GR_RAT_PREV*PI2*(FREQ_ARR(ICH,IFRQ) - FREQ_REF)* &
     &                                                 (TIM_ARR(J11) - TIME_FRT)
                       ELSE IF ( PIM%CONF%FRIB_FINE_SEARCH == PIMA__FINE_SEARCH_ACC ) THEN
                          PHAS_ADD = - PHAS_PREV &
     &                               + PH_RAT_PREV*PI2*FREQ_REF* &
     &                                                 (TIM_ARR(J11) - TIME_FRT) &
     &                               + GR_DEL_PREV*PI2*(FREQ_ARR(ICH,IFRQ) - FREQ_REF) &
     &                               + PH_ACC_PREV*PI2*FREQ_REF* &
     &                                                 (TIM_ARR(J11) - TIME_FRT)**2/2.0D0
                     END IF
!
! ------------------ Update segment residual visibility accumulator
!
                     UV_SEG(J13,IFRQ,ITM) = UV_SEG(J13,IFRQ,ITM) + &
     &                          WEI_SCL*WEI(J11)*UV(ICH,IFRQ,J11)* &
     &                          CMPLX ( COS(PHAS_ADD), SIN(PHAS_ADD) )
                     IF ( IFRQ == 1 ) THEN
                          WPOI_SEG(J13,ITM) = WPOI_SEG(J13,ITM) + WEI_SCL*WEI(J11)
                     END IF
!!   write ( 6, * ) 'PFS-1009 ', int2(J13), int2(IFRQ), int2(ITM), ' uv_seg= ', uv_seg(j13,ifrq,itm), ' wei= ', sngl(wpoi_seg(j13,itm))
 4140             CONTINUE
                  IF ( FL_END_SEG ) THEN
!
! -------------------- Normalize the segemented residual visibility averaged 
! -------------------- over time and frequency
!
                       UV_SEG(J13,IFRQ,ITM) = UV_SEG(J13,IFRQ,ITM)/(NUM_CH*NUM_TM)
                  END IF
 4130          CONTINUE
 4120       CONTINUE
            TIM_SEG(ITM) = TIM_SEG(ITM) + TIM_ARR(J11)
            IF ( FL_END_SEG ) THEN
!
! -------------- Well, we processed the last AP of this segment
!
                 TIM_SEG(ITM) = TIM_SEG(ITM)/NUM_TM
                 IF ( WPOI_SEG(1,ITM) > PIM%CONF%FRIB_WEIGHTS_THRESHOLD ) THEN
!
! ------------------- Udate segment counter unless all APs were discarded 
! ------------------- due to poor weights
!
                      ITM = ITM + 1
                 END IF
!
! -------------- Reset the AP index wrt the segment start
!
                 NUM_TM = 0
                 FL_END_SEG = .FALSE.
            END IF
            NUM_TM = NUM_TM + 1
 4110    CONTINUE
         LSEG_TM = ITM - 1
         IF ( PIM%CONF%DEBUG_LEVEL .GE. 6  ) THEN
              WRITE ( 6, 205 ) PIM%NCHN, MUL_CH, LSEG_CH, &
     &                         KTIM, MUL_TM, LSEG_TM, &
     &                         SNR, SNR_UV, SNR_SEG, LFRQ, WPOI, NOI_SIG_UV
 205          FORMAT ( 'PFS: LCHN: ', I5, ' MUL_CH: ', I5, ' LSEG_CH: ', I5/ &
     &                 '     KTIM: ', I5, ' MUL_TM: ', I5, ' LSEG_TM: ', I5/ &
     &                 '      SNR: ', F10.4, ' SNR_UV: ', F12.5, &
     &                 ' SNR_SEG: ', F12.5/ &
     &                 '     LFRQ: ', I1, ' WPOI: ', 0PF12.1, ' NOISE_UV: ', &
     &                 1PE11.4 )
              CALL FLUSH ( 6 ) 
         END IF
         IF ( LSEG_TM < 2 ) THEN
!
! ----------- Too few time segment.
! ----------- Store phase, amplitude, group delay, phase delay rate
! ----------- deterimined by the binary division method
! ----------- and jump out
!
              AMPL(PIMA__LSQ) = AMPL(PIMA__DRF)
              PHAS(PIMA__LSQ) = PHAS(PIMA__DRF)
              GR_DEL(PIMA__LSQ) = GR_DEL(PIMA__DRF)
              PH_RAT(PIMA__LSQ) = PH_RAT(PIMA__DRF)
!
              PHAS(PIMA__ADD) = PHAS(PIMA__DRF)
              GR_DEL(PIMA__ADD) = GR_DEL(PIMA__DRF)
              PH_RAT(PIMA__ADD) = PH_RAT(PIMA__DRF)
!
! ----------- Deallocate memory and jump out
!
              DEALLOCATE ( FREQ_SEG )
              DEALLOCATE ( UV_SEG   )
              DEALLOCATE ( TIM_SEG  )
              DEALLOCATE ( WPOI_SEG  )
              Q2 = 0.0D0
              GOTO 8280
         END IF
!
         L_EQU = KTIM*(PIM%CONF%END_FRQ-PIM%CONF%BEG_FRQ+1)*PIM%NCHN
!
! ------ Store the current values of the DRF in the maximum
!
         AMP_MAX = ABS(UV_2D_WEI(MAX_MD,MAX_RT))/WPOI
!
! ------ NB: this logic is obsolete by 2020
!
         IF ( AMP_MAX/NOISE_AVR < PIM%CONF%FRIB_SECONDARY_SNR_MIN ) THEN
              N_ITER = PIM%CONF%FRIB_SECONDARY_MAX_TRIES
              IF ( N_ITER < 1 ) N_ITER = 1
!
! ----------- There should be at least two sidelobes in the search window
!
              IF ( L_MD*MD_STEP < 2.01D0*GRAMBSP ) N_ITER = 1
            ELSE
              N_ITER = 1
         END IF
         IF ( N_ITER > 1 ) THEN
              IF ( N_ITER > 3 ) THEN
                   M_SCM = PIMA__M_SCM_LARGE
                 ELSE
                   M_SCM = PIMA__M_SCM_SMALL
              END IF
!
! ----------- If the number of iterations for search of local extrema of
! ----------- DRF is more than 1, then we compute these extrema
!
              CALL ERR_PASS ( IUER, IER )
              CALL PIMA_DRF_MAX_ARR ( PIM%NCHN, LFRQ, LTIM, FREQ_ARR,     &
     &                                GRAMBSP, TIM_ARR,                   &
     &                                PIM%CONF%FRIB_WEIGHTS_THRESHOLD,    &
     &                                TIME_FRT, PH_RAT_PREV, GR_DEL_PREV, &
     &                                GR_RAT_PREV, FREQ_REF, CFRQ_REF,    &
     &                                WEI, UV, M_SCM, GRD_SEC_MAX_ARR,    &
     &                                AMP_SEC_MAX_ARR, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 7637, IUER, 'PIMA_FINE_SEARCH', 'Error '// &
     &                 'in an attempt to compute the array of DRF maxima' )
                   RETURN
              END IF
            ELSE
!
! ----------- The number of iterations is one. We use the main maximum
! ----------- determined by the binary search for for the improvement using
! ----------- the LSQ
!
              GRD_SEC_MAX_ARR(1) = GR_DEL_PREV
         END IF
         Q2_TOT = 0.0D0
         FL_ERROR  = .FALSE.
         IF ( PIM%CONF%DEBUG_LEVEL .EQ. 8 ) THEN
              CALL WALL_TIMER ( %VAL(0) ) 
         END IF           
!
! ------ This is an old logic. Should be revised. This loop should be eliminated,
! ------ since anyway it is executed only once
!
!$OMP PARALLEL DO IF ( NTHR > 1 ), &
!$OMP&         PRIVATE ( J17, J18, J19, J20, J21, J22, J23, J24, J25, J26, &
!$OMP&             I_EQU, IFRQ, IFRA, IER, &
!$OMP&             NOR_MAT, NO2_MAT, NOR_VEC, PHAS_ADD, UV_USE, AMP_SEG, &
!$OMP&             PHS_SEG, SNR_SEG, EQU_OBS, EQU_RH, WEI_RH, Q2, SPR_SQ, &
!$OMP&             RC, EST, ACC, WW, RES, DRF, DRF_IF, PHS_IF, SIG_SQ_NEW, PH_ACC ), &
!$OMP&         SCHEDULE ( STATIC )
         DO 4170 J17=1,N_ITER
            IF ( FL_ERROR ) GOTO 4170
            ALLOCATE ( EQU_OBS(M_PAR,L_EQU), STAT=IER )
            IF ( IER .NE. 0 ) THEN
                 CALL CLRCH ( STR )
                 CALL INCH  ( 8*M_PAR*L_EQU, STR )
!$OMP            CRITICAL
                 CALL ERR_LOG ( 7638, IUER, 'PIMA_FINE_SEARCH', 'Error in an '// &
     &               'attempt to allocate '//STR(1:I_LEN(STR))// &
     &               ' bytes of dynamic memory for array EQU_OBS' )
                 FL_ERROR = .TRUE.
!$OMP            END CRITICAL
                 GOTO 4170
            END IF
!
            ALLOCATE ( EQU_RH(L_EQU), STAT=IER )
            IF ( IER .NE. 0 ) THEN
                 CALL CLRCH ( STR )
                 CALL INCH  ( 8*L_EQU, STR )
!$OMP            CRITICAL
                 CALL ERR_LOG ( 7639, IUER, 'PIMA_FINE_SEARCH', 'Error in an '// &
     &               'attempt to allocate '//STR(1:I_LEN(STR))// &
     &               ' bytes of dynamic memory for array EQU_RH' )
                 FL_ERROR = .TRUE.
                 DEALLOCATE ( EQU_OBS )
!$OMP            END CRITICAL
                 GOTO 4170
            END IF
!
            ALLOCATE ( WEI_RH(L_EQU), STAT=IER )
            IF ( IER .NE. 0 ) THEN
                 CALL CLRCH ( STR )
                 CALL INCH  ( 8*L_EQU, STR )
!$OMP            CRITICAL
                 CALL ERR_LOG ( 7640, IUER, 'PIMA_FINE_SEARCH', 'Error in an '// &
     &               'attempt to allocate '//STR(1:I_LEN(STR))// &
     &               ' bytes of dynamic memory for array WEI_RH' )
                 DEALLOCATE ( EQU_OBS )
                 DEALLOCATE ( EQU_RH  )
                 FL_ERROR = .TRUE.
!$OMP            END CRITICAL
                 GOTO 4170
            END IF
!
! --------- Build equations of conditions
!
            I_EQU = 0
            CALL NOUT_R8 ( M_PA2, NOR_MAT )
            CALL NOUT_R8 ( M_PA2, NO2_MAT )
            CALL NOUT_R8 ( M_PAR, NOR_VEC )
!
! --------- Cycle over time setments
!
            DO 4180 J18=1,LSEG_TM
               IFRQ = 0
!
! ------------ Cycle over IFs
!
               DO 4190 J19=PIM%CONF%BEG_FRQ,PIM%CONF%END_FRQ
                  IFRQ = IFRQ + 1
!
! --------------- Cycle over frequency segments
!
                  DO 4200 J20=1,LSEG_CH
!
! ------------------ Additional phase due to changes in phase delay rate
! ------------------ and group delay found during the coarse search
!
                     PHAS_ADD = (GRD_SEC_MAX_ARR(J17) - GR_DEL_PREV)*PI2* &
     &                          (FREQ_SEG(J20,IFRQ)-FREQ_REF)
!
! ------------------ Apply these phase to the UV datum
!
                     UV_USE = UV_SEG(J20,IFRQ,J18)* &
     &                        CMPLX ( COS(PHAS_ADD), SIN(PHAS_ADD) )
                     IF ( ABS(UV_USE) < PIMA__AMP_MIN ) THEN
                          GOTO 4200
                     END IF
                     PHS_SEG = PHAS_CMPL_R4 ( UV_USE )
                     IF ( PHS_SEG > PI__NUM ) PHS_SEG = PHS_SEG - PI2
!
! ------------------ Build the normal equation and apply scale
!
                     I_EQU = I_EQU + 1
                     EQU_OBS(1,I_EQU) = SCL(1)*1.0D0
                     EQU_OBS(2,I_EQU) = SCL(2)*(TIM_SEG(J18) - TIME_FRT)
                     EQU_OBS(3,I_EQU) = SCL(3)*(FREQ_SEG(J20,IFRQ)/FREQ_REF - 1.0D0)
                     IF ( PIM%CONF%FRIB_FINE_SEARCH == PIMA__FINE_SEARCH_LSQ ) THEN
                          EQU_OBS(4,I_EQU) = SCL(4)*(FREQ_SEG(J20,IFRQ)/FREQ_REF - 1.0D0)* &
     &                                              (TIM_SEG(J18) - TIME_FRT)
                        ELSE IF ( PIM%CONF%FRIB_FINE_SEARCH == PIMA__FINE_SEARCH_ACC ) THEN
                          EQU_OBS(4,I_EQU) = SCL(4)*(TIM_SEG(J18) - TIME_FRT)**2/2.0D0
                     ENDIF
!
! ------------------ ... and the right hand-side
!
                     EQU_RH(I_EQU) = PHS_SEG
!
! ------------------ ... and the weight.
!
                     AMP_SEG = ABS(UV_USE)/( NOI_SIG_UV/DSQRT(WPOI_SEG(J20,J18)) )
                     SNR_SEG = SNR_UV*DSQRT(WPOI_SEG(J20,J18))
                     WEI_RH(I_EQU) = 1.0D0/PIMA_AMP_TO_SIGPHS( SNR_SEG, AMP_SEG )
                     IF ( WEI_RH(I_EQU) < DSQRT(3.0D0)/PI__NUM - 0.001D0 ) THEN
                          WEI_RH(I_EQU) = 0.0D0
                     END IF
!
                     CALL DIAD_CVT_S ( WEI_RH(I_EQU)**2, M_PAR, &
     &                                 EQU_OBS(1,I_EQU), EQU_OBS(1,I_EQU), &
     &                                 NOR_MAT )
!
! ------------------ NB: we also update normal matrix with square of weights
!
                     CALL DIAD_CVT_S ( WEI_RH(I_EQU)**4, M_PAR, &
     &                                 EQU_OBS(1,I_EQU), EQU_OBS(1,I_EQU), &
     &                                 NO2_MAT )
                     DO 4210 J21=1,M_PAR
                        NOR_VEC(J21) =   NOR_VEC(J21) &
     &                                 + WEI_RH(I_EQU)**2*EQU_OBS(J21,I_EQU)* &
     &                                   EQU_RH(I_EQU)
 4210                CONTINUE
 4200             CONTINUE
 4190          CONTINUE
 4180       CONTINUE
            IF ( FL_NO_GRRAT ) THEN
!
! -------------- Effectively eliminate GR_RAT
!
                 NOR_MAT(10) = 1.0D0/PIMA__NO_RRAT_CNS_SIG**2
            END IF
            Q2_TOT(J17) = 0.0D0
!
! --------- Run iterations for update of the additive phase noise 
!
            DO 4220 J22=1,MADD_ITER
               IF ( J22 == 1 ) THEN
                    IFRA = 1
                  ELSE
                    IFRA = 2
               END IF
               IF ( PIM%CONF%FRIB_FINE_SEARCH == PIMA__FINE_SEARCH_LSQ ) THEN
!
! ----------------- Apply constraint on group delay rate or phase acceleration
!
                    NOR_MAT(10) = NOR_MAT(10) + (SCL(4)/(PI2*FREQ_REF)/PIMA__GR_RAT_CNS_SIG)**2
               END IF
!
! ------------ Invert normal matrix
!
               CALL ERR_PASS ( IUER, IER )
!$OMP          CRITICAL
               CALL INVS ( M_PAR, NOR_MAT, RC, IER )
!$OMP          END CRITICAL
               IF ( IER .NE. 0 ) THEN
!$OMP               CRITICAL
                    CALL ERR_LOG ( 7641, IUER, 'PIMA_FINE_SEARCH', 'Failure to '// &
     &                  'invert the normal matrix during an attempt to solve '// &
     &                  'for parameters of fine fringe fitting' )
                    CALL FLUSH ( 6 )
                    FL_ERROR = .TRUE.
                    DEALLOCATE ( EQU_OBS )
                    DEALLOCATE ( EQU_RH  )
                    DEALLOCATE ( WEI_RH  )
!$OMP               END CRITICAL
                    GOTO 4170
               END IF
!
! ------------ Get estimates
!
               CALL MUL_MV_SV_V ( M_PAR, NOR_MAT, M_PAR, NOR_VEC, M_PAR, EST, -2 )
!
! ------------ Apply corrections (with the opposite sign, except the phase)
! ------------ to the resutls of the coarse search and get fine
! ------------ phase, phase delay rate, group delay rate or phase acceleration
!
               PHAS_ARR(J17,IFRA)   = PHAS_PREV            + EST(1)*SCL(1)
               PH_RAT_ARR(J17,IFRA) = PH_RAT_PREV          - EST(2)*SCL(2)/(PI2*FREQ_REF)
               GR_DEL_ARR(J17,IFRA) = GRD_SEC_MAX_ARR(J17) - EST(3)*SCL(3)/(PI2*FREQ_REF)
               IF ( PIM%CONF%FRIB_FINE_SEARCH == PIMA__FINE_SEARCH_LSQ ) THEN
                    GR_RAT_ARR(J17,IFRA) = GR_RAT_PREV     - EST(4)*SCL(4)/(PI2*FREQ_REF)
                  ELSE IF ( PIM%CONF%FRIB_FINE_SEARCH == PIMA__FINE_SEARCH_ACC ) THEN
                    PH_ACC_ARR(J17,IFRA) = PH_ACC_PREV - EST(4)*SCL(4)/(PI2*FREQ_REF)
               END IF
!
! ------------ Get formal errors of estimates
!
               PHAS_ERR_ARR(J17,IFRA)   = DSQRT(NOR_MAT(1) )*SCL(1)
               PH_RAT_ERR_ARR(J17,IFRA) = DSQRT(NOR_MAT(3) )*SCL(2)/(PI2*FREQ_REF)
               GR_DEL_ERR_ARR(J17,IFRA) = DSQRT(NOR_MAT(6) )*SCL(3)/(PI2*FREQ_REF)
               IF ( PIM%CONF%FRIB_FINE_SEARCH == PIMA__FINE_SEARCH_LSQ ) THEN
                    GR_RAT_ERR_ARR(J17,IFRA) = DSQRT(NOR_MAT(10))*SCL(4)/(PI2*FREQ_REF)
                  ELSE IF ( PIM%CONF%FRIB_FINE_SEARCH == PIMA__FINE_SEARCH_ACC ) THEN
                    PH_ACC_ERR_ARR(J17,IFRA) = DSQRT(NOR_MAT(10))*SCL(4)/(PI2*FREQ_REF)
               END IF
!
! ------------ And store some elements of the variance-covariance matrix
!
               COV_PR_ARR(J17,IFRA)     = NOR_MAT(2)*SCL(1)*SCL(2)/(PI2*FREQ_REF)**2
               COV_GR_ARR(J17,IFRA)     = NOR_MAT(9)*SCL(3)*SCL(4)/(PI2*FREQ_REF)**2
!
               IF ( PIM%CONF%FRIB_FINE_SEARCH == PIMA__FINE_SEARCH_BIN .OR. &
     &              PIM%CONF%FRIB_FINE_SEARCH == PIMA__FINE_SEARCH_LSQ      ) THEN
                    MODE = PIMA__GRAT
                  ELSE IF ( PIM%CONF%FRIB_FINE_SEARCH == PIMA__FINE_SEARCH_ACC ) THEN
                    MODE = PIMA__ACC  
               END IF
!
! ------------ Update visibilities for correction in phase delay rate,
! ------------ group delay, and depending on MODE, over either group delay
! ------------ rate of phase acceleration
!
               CALL PIMA_UV_DRF3 ( MODE, FL_OPT, LTIM, PIM%NCHN, LFRQ, WEI, TIM_ARR, &
     &                             PIM%CONF%FRIB_WEIGHTS_THRESHOLD, &
     &                             TIME_FRT, PH_RAT_ARR(J17,IFRA), &
     &                             GR_DEL_ARR(J17,IFRA), GR_RAT_ARR(J17,IFRA), &
     &                             PH_ACC_ARR(J17,IFRA), FREQ_ARR, FREQ_REF, CFRQ_REF, &
     &                             UV, DRF, DRF_IF )
!
! ------------ Compute wrms of phase residuals over frequency
!
               WW  = 0.0D0
               ACC = 0.0D0
               AMPL_ARR(J17,IFRA) = ABS(DRF)/WPOI
               PHAS_DRF_ARR(J17,IFRA) = PHAS_CMPL_R4( DRF )
               DO 4230 J23=1,LFRQ
!
! --------------- PHS_IF is the residual phase over the J23th IF wrt the total phase
!
                  PHS_IF = PHAS_CMPL_R4(DRF_IF(J23)) - PHAS_DRF_ARR(J17,IFRA)
                  IF ( PHS_IF >  PI__NUM ) PHS_IF = PHS_IF - PI2
                  IF ( PHS_IF < -PI__NUM ) PHS_IF = PHS_IF + PI2
                  ACC = ACC + ( PHS_IF * ABS(DRF_IF(J23)) )**2
                  WW  = WW  + ABS(DRF_IF(J23))**2
 4230          CONTINUE
!
! ------------ Computed the total wrms of phase residuals
!
               IF ( WW > 1.D-16 ) THEN
                    WRMS_IF_ARR(J17,IFRA) = DSQRT ( ACC/WW )
                  ELSE
                    WRMS_IF_ARR(J17,IFRA) = 0.0D0
               END IF
               IF ( J22 == MADD_ITER ) GOTO 4220
!
! ------------ Compute wrms of segment residuals
!
               ACC = 0.0D0
               WW  = 0.0D0
               DO 4240 J24=1,I_EQU
                  RES = EQU_RH(J24) - DP_VV_V ( M_PAR, EST, EQU_OBS(1,J24) )
                  ACC = ACC + (RES*WEI_RH(J24))**2
                  WW  = WW  + WEI_RH(J24)**2
 4240          CONTINUE
               WRMS_ARR(J17,IFRA) = DSQRT ( ACC/WW )
               SPR_SQ =   2.0D0*DP_VV_V ( M_PA2, NOR_MAT, NO2_MAT ) &
     &                  - NOR_MAT(1)*NO2_MAT(1)   &
     &                  - NOR_MAT(3)*NO2_MAT(3)   &
     &                  - NOR_MAT(6)*NO2_MAT(6)   &
     &                  - NOR_MAT(10)*NO2_MAT(10)
!
! ------------ Get Q2 -- chi^2/number_of_degrees_of_freedom
!
               Q2 = (ACC - (I_EQU-4))/(WW - SPR_SQ)
               Q2_TOT(J17) = Q2_TOT(J17) + Q2
!
! ------------ Update normal matrix, normal vector and 
! ------------ normal matrix with square of weights
!
               CALL NOUT_R8 ( M_PA2, NOR_MAT )
               CALL NOUT_R8 ( M_PAR, NOR_VEC )
               CALL NOUT_R8 ( M_PA2, NO2_MAT )
!
! ------------ Update segment weights for additive phase errors 
!
               DO 4250 J25=1,I_EQU
                  IF ( WEI_RH(J25) > 0.001 ) THEN
                       SIG_SQ_NEW = 1.D0/WEI_RH(J25)**2 + Q2
                       IF ( SIG_SQ_NEW > SIG_MIN**2 ) THEN
                            WEI_RH(J25) = 1.D0/DSQRT( SIG_SQ_NEW )
                       END IF
                  END IF
                  CALL DIAD_CVT_S ( WEI_RH(J25)**2, M_PAR, &
     &                              EQU_OBS(1,J25), EQU_OBS(1,J25), &
     &                              NOR_MAT )
                  CALL DIAD_CVT_S ( WEI_RH(J25)**4, M_PAR, &
     &                              EQU_OBS(1,J25), EQU_OBS(1,J25), &
     &                              NO2_MAT )
                  DO 4260 J26=1,M_PAR
                     NOR_VEC(J26) =   NOR_VEC(J26) &
     &                              + WEI_RH(J25)**2*EQU_OBS(J26,J25)* &
     &                                EQU_RH(J25)
 4260             CONTINUE
 4250          CONTINUE
 4220       CONTINUE
!
            IF ( PIM%CONF%DEBUG_LEVEL .GE. 16 ) THEN
!$OMP            CRITICAL
                 WRITE ( 6, 220 ) J17, AMPL_ARR(J17,IFRA), &
     &                            WRMS_ARR(J17,IFRA), &
     &                           (GR_DEL_ARR(J17,IFRA) - GR_DEL_PREV)*1.D9
 220             FORMAT ( 'PIMA_FINE_SEA Amb_itr: ', I2,' Amp: ', F9.7, &
     &                    ' Ph_wrms: ', F8.5, 2X, 'Gd_sub: ', F8.3, ' ns' )
                 CALL FLUSH ( 6 )
!$OMP            END CRITICAL
            END IF
            DEALLOCATE ( EQU_OBS )
            DEALLOCATE ( EQU_RH  )
            DEALLOCATE ( WEI_RH  )
 4170    CONTINUE
!$OMP END PARALLEL DO
         IF ( PIM%CONF%DEBUG_LEVEL .EQ. 8 ) THEN
              CALL WALL_TIMER ( STR ) 
              WRITE ( 6, '(A)' ) 'PIMA_FINE_SEARCH TIMER_3: '//STR(1:27)
              CALL FLUSH ( 6 )
         END IF
         IF ( FL_ERROR ) THEN
              IF ( ALLOCATED ( CFRQ_REF ) ) DEALLOCATE ( CFRQ_REF )
              IF ( ALLOCATED ( FREQ_SEG ) ) DEALLOCATE ( FREQ_SEG )
              IF ( ALLOCATED ( TIM_SEG  ) ) DEALLOCATE ( TIM_SEG  )
              IF ( ALLOCATED ( WPOI_SEG ) ) DEALLOCATE ( WPOI_SEG )
              IF ( ALLOCATED ( UV_SEG   ) ) DEALLOCATE ( UV_SEG   )
              CALL ERR_LOG ( 7642, IUER, 'PIMA_FINE_SEARCH', 'Failure in '// &
     &            'an attempt to solve the normal equations for getting '// &
     &            'the LSQ estimate of fringe fit' )
              RETURN
         END IF
!
! ------ Store phase, phase delay rate and group delays for 
! ------ raw LSQ method
!
         PHAS(PIMA__LSQ)   = PHAS_ARR(1,1)
         PH_RAT(PIMA__LSQ) = PH_RAT_ARR(1,1)
         GR_DEL(PIMA__LSQ) = GR_DEL_ARR(1,1)
         PHAS_ERR(PIMA__LSQ)   = PHAS_ERR_ARR(1,1)
         PH_RAT_ERR(PIMA__LSQ) = PH_RAT_ERR_ARR(1,1)
         GR_DEL_ERR(PIMA__LSQ) = GR_DEL_ERR_ARR(1,1)
         IF ( PIM%CONF%FRIB_FINE_SEARCH == PIMA__FINE_SEARCH_LSQ ) THEN
              GR_RAT     = GR_RAT_ARR(1,1)
              GR_RAT_ERR = GR_RAT_ERR_ARR(1,1)
            ELSE IF ( PIM%CONF%FRIB_FINE_SEARCH == PIMA__FINE_SEARCH_ACC ) THEN
              PH_ACC     = PH_ACC_ARR(1,1)
              PH_ACC_ERR = PH_ACC_ERR_ARR(1,1)
         END IF
         AMPL(PIMA__LSQ) = AMPL_ARR(1,1)
         PHS_WRMS   = WRMS_ARR(1,1)
         COV_PR_PH  = COV_PR_ARR(1,1)
         COV_GR_MD  = COV_GR_ARR(1,1)
!
! ------ and for the LSQ with additive re-weighting
!
         PHAS(PIMA__ADD)   = PHAS_ARR(1,2)
         PH_RAT(PIMA__ADD) = PH_RAT_ARR(1,2)
         GR_DEL(PIMA__ADD) = GR_DEL_ARR(1,2)
         PHAS_ERR(PIMA__ADD)   = PHAS_ERR_ARR(1,2)
         PH_RAT_ERR(PIMA__ADD) = PH_RAT_ERR_ARR(1,2)
         GR_DEL_ERR(PIMA__ADD) = GR_DEL_ERR_ARR(1,2)
         PH_DEL_ERR(PIMA__ADD) = PHAS_ERR(PIMA__ADD)/(PI2*FREQ_REF)
         Q2 = Q2_TOT(1)
!
         IND_BEST   = 1
!
         IF ( N_ITER > 1 ) THEN
!
! ----------- Obosolve logic will be removed
!
              DO 4270 J27=2,N_ITER
                 IF ( WRMS_ARR(J27,1) < PHS_WRMS ) THEN
                      PHAS(PIMA__LSQ)   = PHAS_ARR(J27,1)
                      PH_RAT(PIMA__LSQ) = PH_RAT_ARR(J27,1)
                      GR_DEL(PIMA__LSQ) = GR_DEL_ARR(J27,1)
                      PHAS_ERR(PIMA__LSQ)   = PHAS_ERR_ARR(J27,1)
                      PH_RAT_ERR(PIMA__LSQ) = PH_RAT_ERR_ARR(J27,1)
                      GR_DEL_ERR(PIMA__LSQ) = GR_DEL_ERR_ARR(J27,1)
                      GR_RAT = GR_RAT_ARR(J27,1)
                      GR_RAT_ERR = GR_RAT_ERR_ARR(J27,1)
                      PHAS(PIMA__LSQ) = PHAS_DRF_ARR(J27,1)
                      SNR  = AMPL(PIMA__DRF)/NOISE_AVR
                      COV_PR_PH  = COV_PR_ARR(J27,1)
                      COV_GR_MD  = COV_GR_ARR(J27,1)
!
                      AMPL(PIMA__LSQ) = AMPL_ARR(J27,1)
!
                      PHAS(PIMA__ADD)   = PHAS_ARR(J27,2)
                      PH_RAT(PIMA__ADD) = PH_RAT_ARR(J27,2)
                      GR_DEL(PIMA__ADD) = GR_DEL_ARR(J27,2)
                      PHAS_ERR(PIMA__ADD)   = PHAS_ERR_ARR(J27,2)
                      PH_RAT_ERR(PIMA__ADD) = PH_RAT_ERR_ARR(J27,2)
                      GR_DEL_ERR(PIMA__ADD) = GR_DEL_ERR_ARR(J27,2)
                      PHAS(PIMA__ADD) = PHAS_DRF_ARR(J27,2)
                      Q2 = Q2_TOT(J27)
!
                      ITURN = IDNINT(PHAS(PIMA__LSQ)/PI2)
                      PHAS(PIMA__LSQ) = PHAS(PIMA__LSQ) - PI2*ITURN 
!
                      ITURN = IDNINT(PHAS(PIMA__ADD)/PI2)
                      PHAS(PIMA__ADD) = PHAS(PIMA__ADD) - PI2*ITURN 
!
                      PHS_WRMS = WRMS_ARR(J27,1)
                      IND_BEST = J27
                 END IF
 4270         CONTINUE
         END IF
!
! ------ Try to resolve phase delay ambiguity
!
         IAMB = IDNINT ( (GR_DEL(PIMA__LSQ) - GR_DEL_PREV)/GRAMBSP )
         IF ( DABS(GR_DEL(PIMA__LSQ) - IAMB*GRAMBSP - GR_DEL_PREV) .GT. &
     &        GRDEL_SUB_SHR*GRAMBSP ) THEN
              ISUB = 1
         END IF
         IAMB = IDINT ( PHAS(PIMA__ADD)/PI2 )
         PHAS(PIMA__ADD) = PHAS(PIMA__ADD) - IAMB*PI2
!
         SNR  = AMPL(PIMA__DRF)/NOISE_AVR
         IF ( PIM%CONF%DEBUG_LEVEL .GE. 6 ) THEN
              WRITE ( 6, 230 ) IAMB, ISUB, GRAMBSP*1.D9, &
     &                         AMPL(PIMA__LSQ)/NOISE_AVR, IND_BEST
 230          FORMAT ( 'PIMA_FINE_SEARCH AMB: ', I3, ' SUB: ', I2, &
     &                 ' GRAMBSP= ', F10.4, ' ns  SNR= ', F9.3, &
     &                 ' IB= ', I2 )
         END IF
         IND_FRA = PIMA__LSQ
!
         DEALLOCATE ( FREQ_SEG )
         DEALLOCATE ( UV_SEG   )
         DEALLOCATE ( TIM_SEG  )
         DEALLOCATE ( WPOI_SEG  )
       ELSE
         IF ( PIM%CONF%DEBUG_LEVEL .GE. 2 ) THEN
              WRITE ( 6, * ) 'PIMA_FINE_SEARCH: Only ', K_EPC, &
     &                       ' epochs. Fall back to DRF mode'
              SNR  = AMPL(PIMA__DRF)/NOISE_AVR
              Q2 = 1.0
         END IF
!
         GR_DEL(PIMA__LSQ) = GR_DEL(PIMA__DRF)
         PH_RAT(PIMA__LSQ) = PH_RAT(PIMA__DRF)
         PHAS(PIMA__LSQ)   = PHAS(PIMA__DRF)
!
         GR_DEL(PIMA__MUL) = GR_DEL(PIMA__DRF)
         PH_RAT(PIMA__MUL) = PH_RAT(PIMA__DRF)
         PHAS(PIMA__MUL)   = PHAS(PIMA__DRF)
!
         GR_DEL(PIMA__ADD) = GR_DEL(PIMA__DRF)
         PH_RAT(PIMA__ADD) = PH_RAT(PIMA__DRF)
         PHAS(PIMA__ADD)   = PHAS(PIMA__DRF)
!
         IND_FRA =  PIMA__DRF
      END IF ! PIMA__FINE_SEARCH_LSQ
!              =====================
 8280 CONTINUE
      IF ( PIM%CONF%DEBUG_LEVEL .EQ. 8 ) THEN
           CALL WALL_TIMER ( %VAL(0) ) 
      END IF           
!
! --- Compute uncertainties based on SNR
!
      IF ( SNR > 1.0D-8 ) THEN
!
! -------- We store them into the DRF slot
!
           PHAS_ERR(PIMA__DRF)   = 1.D0/SNR
           PH_DEL_ERR(PIMA__DRF) = PHAS_ERR(PIMA__DRF)/(PI2*FREQ_REF)
           PH_RAT_ERR(PIMA__DRF) = DSQRT ( 3.0D0 )/(PI__NUM*FREQ_REF*EFF_DURA*SNR)
           IF ( (SUM_FQ2/SUM_WEI - ( SUM_FQ1/SUM_WEI )**2) > 1.D-20 ) THEN
                GR_DEL_ERR(PIMA__DRF) = 1.D0/(PI2*SNR* &
     &                           DSQRT( SUM_FQ2/SUM_WEI - ( SUM_FQ1/SUM_WEI )**2 ) )
                GR_RAT_ERR = DSQRT ( 6.0D0 )/ &
     &                       DSQRT( SUM_FQ2/SUM_WEI - ( SUM_FQ1/SUM_WEI )**2 )/ &
     &                       (PI__NUM*EFF_DURA*SNR)
             ELSE
!
! ------------- Pathological case. We want to avoid division by zero
!
                GR_DEL_ERR(PIMA__DRF) = MAX_GD_ERR
                GR_RAT_ERR            = MAX_PR_ERR
           END IF
           COV_PR_PH  = 0.0D0
           COV_GR_MD  = 0.0D0
           IF ( IND_FRA .EQ. PIMA__DRF .AND. &
     &          ( PIM%CONF%FRIB_FINE_SEARCH == PIMA__FINE_SEARCH_LSQ .OR. &
     &            PIM%CONF%FRIB_FINE_SEARCH == PIMA__FINE_SEARCH_ACC      ) ) THEN
!
! ------------- Fallback to DRF mode
!
                GR_DEL_ERR(PIMA__LSQ) = GR_DEL_ERR(PIMA__DRF)
                PHAS_ERR(PIMA__LSQ)   = PHAS_ERR(PIMA__DRF)
                PH_DEL_ERR(PIMA__LSQ) = PH_DEL_ERR(PIMA__DRF)
                PH_RAT_ERR(PIMA__LSQ) = PH_RAT_ERR(PIMA__DRF)
                AMPL(PIMA__LSQ)       = AMPL(PIMA__DRF)
!
                GR_DEL_ERR(PIMA__ADD) = GR_DEL_ERR(PIMA__DRF)
                PHAS_ERR(PIMA__ADD)   = PHAS_ERR(PIMA__DRF)
                PH_DEL_ERR(PIMA__ADD) = PH_DEL_ERR(PIMA__DRF)
                PH_RAT_ERR(PIMA__ADD) = PH_RAT_ERR(PIMA__DRF)
!
                GR_DEL_ERR(PIMA__MUL) = GR_DEL_ERR(PIMA__DRF)
                PHAS_ERR(PIMA__MUL)   = PHAS_ERR(PIMA__DRF)
                PH_DEL_ERR(PIMA__MUL) = PH_DEL_ERR(PIMA__DRF)
                PH_RAT_ERR(PIMA__MUL) = PH_RAT_ERR(PIMA__DRF)
           END IF
         ELSE
!
! -------- SNR = 0.0 . Something went wrong. We put some values
!
           GR_DEL_ERR = MAX_GD_ERR
           PHAS_ERR    = PI__NUM/DSQRT(3.0D0)
           PH_RAT_ERR = MAX_PR_ERR
           GR_RAT_ERR = MAX_PR_ERR
           COV_PR_PH  = 0.0D0
           COV_GR_MD  = 0.0D0
      END IF
!
! --- Compute the DRF in order to find the amplitude and phase
!
      IF ( PIM%CONF%FRIB_FINE_SEARCH == PIMA__FINE_SEARCH_ACC ) THEN
           MODE = PIMA__ACC  
         ELSE 
           MODE = PIMA__GRAT
      END IF
!
      CALL PIMA_UV_DRF3 ( MODE, FL_OPT, LTIM, PIM%NCHN, LFRQ, WEI, TIM_ARR, &
     &                    PIM%CONF%FRIB_WEIGHTS_THRESHOLD, &
     &                    TIME_FRT, PH_RAT(IND_FRA), GR_DEL(IND_FRA), &
     &                    GR_RAT, PH_ACC, FREQ_ARR, &
     &                    FREQ_REF, CFRQ_REF, UV, DRF, DRF_IF )
      AMPL(IND_FRA) = ABS(DRF)/WPOI
      SNR = AMPL(IND_FRA)/NOISE_AVR
      PHAS(IND_FRA) = PHAS_CMPL_R4( DRF )
!
! --- Compute formal uncertainties of phase, phase delay rate,
! --- group delay, group delay rate on the basis of the RMS
! --- of residual phases over time
!
! --- Compute AMP_INCOH -- incoherently averged ampltuide
!
      WW        = 0.0D0
      WS        = 0.0D0
      PHS_WRMS  = 0.0D0
      AMP_INCOH = 0.0
      DRF_ACC   = 0.0
!$OMP PARALLEL DO IF ( NTHR > 1 ), &
!$OMP&         PRIVATE ( DRF, DRF_IF, PHS_DIF ) &
!$OMP&         REDUCTION ( +: PHS_WRMS, WW, WS, DRF_ACC, AMP_INCOH ), &
!$OMP&         SHARED ( FL_OPT ), &
!$OMP&         SCHEDULE ( DYNAMIC )
      DO 4280 J28=1,LTIM
         IF ( WEI(J28) > PIM%CONF%FRIB_WEIGHTS_THRESHOLD ) THEN
!
! ----------- Get residuals for the J28th AP
!
              CALL PIMA_UV_DRF3 ( MODE, FL_OPT, 1, PIM%NCHN, LFRQ, WEI(J28), TIM_ARR(J28), &
     &                            PIM%CONF%FRIB_WEIGHTS_THRESHOLD, &
     &                            TIME_FRT, PH_RAT(IND_FRA), GR_DEL(IND_FRA), &
     &                            GR_RAT, PH_ACC, FREQ_ARR, FREQ_REF, CFRQ_REF, &
     &                            UV(1,1,J28), DRF, DRF_IF  )
              PHS_DIF  = PHAS_CMPL_R4( DRF ) - PHAS(IND_FRA)
              IF ( PHS_DIF < -PI__NUM ) PHS_DIF = PHS_DIF + PI2
              IF ( PHS_DIF >  PI__NUM ) PHS_DIF = PHS_DIF - PI2
              PHS_WRMS  = PHS_WRMS + (WEI(J28)*PHS_DIF )**2
              WW        = WW + WEI(J28)**2
              WS        = WS + WEI(J28)
              AMP_INCOH = AMP_INCOH + ABS(DRF)
              DRF_ACC   = DRF_ACC + DRF
         END IF
 4280 CONTINUE
!$OMP END PARALLEL DO
      PH_DEL_ERR(PIMA__DRF) = PHAS_ERR(PIMA__DRF)/(PI2*FREQ_REF)
      PH_DEL_ERR(IND_FRA) = PHAS_ERR(IND_FRA)/(PI2*FREQ_REF)
      IF ( PIM%CONF%FRIB_FINE_SEARCH .NE. PIMA__FINE_SEARCH_LSQ .AND. &
     &     PIM%CONF%FRIB_FINE_SEARCH .NE. PIMA__FINE_SEARCH_ACC       ) THEN
!
! -------- There were not LSQ solution, so chi^2/number_of_degrees_of_freedom 
! -------- is not available
!
           Q2 = 1.0D0
      END IF
!
! --- Compute the time decorrelation factor
!
      IF ( AMP_INCOH > PIMA__AMP_MIN ) THEN
           DECOR_TIM = ABS(DRF)/AMP_INCOH
         ELSE 
           DECOR_TIM = 0.0
      END IF
!
      IF ( WW > 0.0D0 ) THEN
           PHS_WRMS = DSQRT ( PHS_WRMS/WW )
           ERR_MULT  = SNR*PHS_WRMS/DSQRT(WS)
           IF ( Q2 > 0.0D0 ) THEN
                Q2_SIG = DSQRT ( Q2 )
              ELSE
                Q2_SIG = -DSQRT ( -Q2 )
           END IF
           IF ( PIM%CONF%DEBUG_LEVEL .GE. 5 ) THEN
                WRITE ( 6, 240 ) IND_OBS, ERR_MULT, Q2_SIG
 240            FORMAT ( 'PIMA_FINE_SEARCH  Ind_obs: ', I5, &
     &                   ' Err_mult: ', F11.4, ' Err_add:' , F11.4, ' rad' )
           END IF
!
! -------- Scale errors for multiplcative version by square root 
! -------- of chi^/number_of_degrees_of_freedom
!
           IF ( IND_FRA == PIMA__LSQ ) THEN
                PHAS_ERR(PIMA__MUL)   = PHAS_ERR(PIMA__LSQ)*ERR_MULT
                PH_DEL_ERR(PIMA__MUL) = PH_DEL_ERR(PIMA__LSQ)*ERR_MULT
                PH_RAT_ERR(PIMA__MUL) = PH_RAT_ERR(PIMA__LSQ)*ERR_MULT
                GR_DEL_ERR(PIMA__MUL) = GR_DEL_ERR(PIMA__LSQ)*ERR_MULT
                PHAS(PIMA__MUL)   = PHAS(PIMA__LSQ)
                GR_DEL(PIMA__MUL) = GR_DEL(PIMA__LSQ)
                PH_RAT(PIMA__MUL) = PH_RAT(PIMA__LSQ)
             ELSE
                PHAS_ERR(PIMA__MUL)   = PHAS_ERR(PIMA__DRF)*ERR_MULT
                PH_DEL_ERR(PIMA__MUL) = PH_DEL_ERR(PIMA__DRF)*ERR_MULT
                PH_RAT_ERR(PIMA__MUL) = PH_RAT_ERR(PIMA__DRF)*ERR_MULT
                GR_DEL_ERR(PIMA__MUL) = GR_DEL_ERR(PIMA__DRF)*ERR_MULT
                PHAS(PIMA__MUL)   = PHAS(PIMA__DRF)
                GR_DEL(PIMA__MUL) = GR_DEL(PIMA__DRF)
                PH_RAT(PIMA__MUL) = PH_RAT(PIMA__DRF)
           END IF
      END IF
!
      IF ( PIM%CONF%DEBUG_LEVEL .EQ. 8 ) THEN
           CALL WALL_TIMER ( STR ) 
           WRITE ( 6, '(A)' ) 'PIMA_FINE_SEARCH TIMER_4: '//STR(1:27)
           CALL FLUSH ( 6 )
      END IF
!
      IF ( FIRST_AMPL_FRQ .NE. 1 .OR. LAST_AMPL_FRQ .NE. LFRQ ) THEN
!
! -------- Suppot of a special kludge enviroment variables
!
           WS = 0.0D0
           DO 4290 J29=1,LTIM
              IF ( WEI(J29) > PIM%CONF%FRIB_WEIGHTS_THRESHOLD ) THEN
                   DO 4300 J30=FIRST_AMPL_FRQ,LAST_AMPL_FRQ
                       DO 4310 J31=1,PIM%NCHN
                          IF ( ABS(UV(J31,J30,J29)) > 1.0E-8 ) THEN
                               WS = WS + WEI(J29)
                          END IF
 4310                  CONTINUE 
 4300              CONTINUE 
              END IF
 4290      CONTINUE 
           KFRQ = LAST_AMPL_FRQ - FIRST_AMPL_FRQ + 1
           CALL PIMA_UV_DRF3 ( MODE, FL_OPT, LTIM, PIM%NCHN, KFRQ, WEI, TIM_ARR, &
     &                    PIM%CONF%FRIB_WEIGHTS_THRESHOLD, &
     &                    TIME_FRT, PH_RAT(IND_FRA), GR_DEL(IND_FRA), &
     &                    GR_RAT, PH_ACC, FREQ_ARR(1:PIM%NCHN,FIRST_AMPL_FRQ:LAST_AMPL_FRQ), &
     &                    FREQ_REF, CFRQ_REF(1:PIM%NCHN,FIRST_AMPL_FRQ:LAST_AMPL_FRQ), &
     &                    UV(1:PIM%NCHN,FIRST_AMPL_FRQ:LAST_AMPL_FRQ,1:LTIM), DRF, &
     &                    DRF_IF(FIRST_AMPL_FRQ:LAST_AMPL_FRQ) )
           AMPL(IND_FRA) = ABS(DRF)/WS 
      END IF
!
      IF ( PIM%CONF%DEBUG_LEVEL .GE. 6 ) THEN
           WRITE ( 6, 250 ) GR_DEL(1)*1.D12, GR_DEL_ERR(1)*1.D12, PH_RAT(1)*1.D15, PH_RAT_ERR(1)*1.D15, &
     &                      GR_DEL(2)*1.D12, GR_DEL_ERR(2)*1.D12, PH_RAT(2)*1.D15, PH_RAT_ERR(2)*1.D15, &
     &                      GR_DEL(3)*1.D12, GR_DEL_ERR(3)*1.D12, PH_RAT(3)*1.D15, PH_RAT_ERR(3)*1.D15, &
     &                      GR_DEL(4)*1.D12, GR_DEL_ERR(4)*1.D12, PH_RAT(4)*1.D15, PH_RAT_ERR(4)*1.D15, &
     &                      AMPL(PIMA__DRF), AMPL(PIMA__DRF)/NOISE_AVR, &
     &                      AMPL(PIMA__LSQ), AMPL(PIMA__LSQ)/NOISE_AVR, &
     &                      NOISE_AVR, DECOR_TIM, AMP_PREL, PHS_PREL
 250       FORMAT ( 'Grd(DRF)= ', F12.2, ' -+ ', F9.2, ' ps  Phr(DRF)= ', F13.2, ' -+ ', F8.1, ' fs/s'/ &
     &              'Grd(LSQ)= ', F12.2, ' -+ ', F9.2, ' ps  Phr(LSQ)= ', F13.2, ' -+ ', F8.1, ' fs/s'/ &
     &              'Grd(MUL)= ', F12.2, ' -+ ', F9.2, ' ps  Phr(MUL)= ', F13.2, ' -+ ', F8.1, ' fs/s'/ &
     &              'Grd(ADD)= ', F12.2, ' -+ ', F9.2, ' ps  Phr(ADD)= ', F13.2, ' -+ ', F8.1, ' fs/s'/ &
     &              'AMPL(DRF)= ', 0PF10.7, '    SNR= ', 0PF10.3/ &
     &              'AMPL(LSQ)= ', 0PF10.7, '    SNR= ', 0PF10.3/ &
     &              'Noi_avr=   ', 1PD11.4, ' DECOR_TIM = ', 0PF5.3/ &
     &              'Ampl_prel= ', F9.6,' Phas_prel: ', F8.5 )
           IF ( PIM%CONF%FRIB_FINE_SEARCH == PIMA__FINE_SEARCH_ACC ) THEN
                WRITE ( 6, 260 ) PH_ACC, PH_ACC_ERR
 260            FORMAT ( 'PFS Ph_acc= ', 1PD15.7, ' Ph_acc_err: ', 1PD15.7, ' 1/s' )
              ELSE 
                WRITE ( 6, 270 ) GR_RAT, GR_RAT_ERR
 270            FORMAT ( 'PFS GR_RAT= ', 1PD15.7, ' Gr_rat_err: ', 1PD15.7 )
           END IF
      END IF
!
      DEALLOCATE ( CFRQ_REF )
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  PIMA_FINE_SEARCH  !#!#
