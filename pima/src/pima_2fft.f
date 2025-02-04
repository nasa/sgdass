      SUBROUTINE PIMA_2FFT ( PIM, VTD, IND_OBS, LCHN, LFRQ, LTIM, FREQ_ARR,   &
     &                       FREQ_REF, UV, WEI_1D, AP_LEN,       &
     &                       FINAM_2D_PLOT, TIME_FRT, GR_DEL, PH_RAT, GR_RAT, &
     &                       PH_ACC, PHAS, AMPL, SNR, GR_DEL_ERR, PH_DEL_ERR, &
     &                       PH_RAT_ERR, GR_RAT_ERR, PH_ACC_ERR, PHAS_ERR,    &
     &                       GRAMBSP, EFF_FRQ_PHS, EFF_FRQ_GRP,               &
     &                       EFF_FRQ_RAT, COV_PR_PH, COV_GR_MD, EFF_DURA,     &
     &                       TEC, TEC_RATE, TEC_ERR, TEC_RATE_ERR,    &
     &                       DECOR_TIM, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine PIMA_2FFT  performs fringe searching using two-dimensional *
! *   FFT for coarse search and LSQ technique for fine search. It also   *
! *   makes fringe plot.                                                 *
! *                                                                      *
! *   In order to search in the full window use these setttings:         *
! *                                                                      *
! *     FRIB.DELAY_WINDOW_WIDTH 1.0/Chan_with                            *
! *     FRIB.RATE_WINDOW_WIDTH  1.0/Acc_per/Frq                          *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *       PIM ( PIMA__TYP ) -- Object with information related to        *
! *                            program PIMA.                             *
! *       VTD ( VTD__TYP  ) -- Object with information related to        *
! *                            package VTD for computed apriori path     *
! *                            delay.                                    *
! *   IND_OBS ( INTEGER*4 ) -- Observation index.                        *
! *      LCHN ( INTEGER*4 ) -- The number of spectral channels           *
! *                            in one frequency channels.                *
! *      LFRQ ( INTEGER*4 ) -- Number of frequency channels.             *
! *      LTIM ( INTEGER*4 ) -- Number of accumulation periods.           *
! *  FREQ_ARR ( REAL*8    ) -- Frequency array. Dimension: (LCHN,LFRQ).  *
! *  FREQ_REF ( REAL*8    ) -- Reference frequency.                      *
! *        UV ( COMPLEX*8 ) -- Array of the cross correlation function.  *
! *                            Dimension: (LCHN,LFRQ,LTIM).              *
! *                            each accumulation period in range [0, 1]. *
! *                            Dimension: LTIM.                          *
! *    AP_LEN ( REAL*8    ) -- Length of the accumulation period.        *
! * FINAM_2D_PLOT ( CHARACTER ) -- Name of the 2D fringe plot output     *
! *                                file.                                 *
! *                                                                      *
! * _________________________ Output parameters: _______________________ *
! *                                                                      *
! * GR_DEL_ERR_SNR ( REAL*8   ) -- Error of the wide-band group delay    *
! *                                computed on the basis of SNR. Units:  *
! *                                sec.                                  *
! * GR_DEL_ERR_LSQ ( REAL*8   ) -- Error of the wide-band group delay    *
! *                                computed by the LSQ method. Units:    *
! *                                sec.                                  *
! * GR_DEL_ERR_MUL ( REAL*8   ) -- Error of the wide-band group delay    *
! *                                computed by the LSQ method with       *
! *                                applying mulitplicative reweighting.  *
! * GR_DEL_ERR_ADD ( REAL*8   ) -- Error of the wide-band group delay    *
! *                                computed by the LSQ method with       *
! *                                applying additive reweighting.        *
! * PH_DEL_ERR_SNR ( REAL*8   ) -- Error of the phase delay computed     *
! *                                on the basis of SNR. Units: sec.      *
! * PH_DEL_ERR_LSQ ( REAL*8   ) -- Error of the phase delay computed     *
! *                                by the LSQ method. Units: sec.        *
! * PH_DEL_ERR_MUL ( REAL*8   ) -- Error of the phase delay computed     *
! *                                by the LSQ method by the LSQ method   *
! *                                with applying mulitplicative          *
! *                                reweighting.                          *
! * PH_DEL_ERR_ADD ( REAL*8   ) -- Error of the phase delay computed     *
! *                                by the LSQ method by the LSQ method   *
! *                                with applying additive reweighting.   *
! *     GR_DEL ( REAL*8   ) -- Group delay.                              *
! *     PH_RAT ( REAL*8   ) -- Phase delay rate.                         *
! *     GR_RAT ( REAL*8   ) -- Phase delay rate.                         *
! *     PH_ACC ( REAL*8   ) -- Phase acceleration. Computed only in      *
! *                            ACC-mode.                                 *
! *       PHAS ( REAL*8   ) -- Fringe phase at reference moment of time  *
! *                            at reference frequency.                   *
! *      AMPL ( REAL*8    ) -- Amplitude of the cross-correlation        *
! *                            function which corresponds to GR_DEL,     *
! *                            PH_RAT.                                   *
! *       SNR ( REAL*8    ) -- Signal to noise ratio for fringe          *
! *                            amplitude.                                *
! *     PH_RAT_ERR ( REAL*8   ) -- Error of the phase delay rate.        *
! *     GR_RAT_ERR ( REAL*8   ) -- Error of the group delay rate.        *
! *     PH_ACC_ERR ( REAL*8   ) -- Error of Phase acceleration. Computed *
! *                                only in ACC-mode.                     *
! *   PHAS_ERR_SNR ( REAL*8   ) -- Phase error.                          *
! *   PHAS_ERR_LSQ ( REAL*8   ) -- Phase error.                          *
! *        GRAMBSP ( REAL*8   ) -- Group delay ambiguity spacing.        *
! *    EFF_FRQ_PHS ( REAL*8   ) -- Effective ionosphere frequency for    *
! *                                phase delay. Units: Hz.               *
! *    EFF_FRQ_GRP ( REAL*8   ) -- Effective ionosphere frequency for    *
! *                                group delay. Units: Hz.               *
! *    EFF_FRQ_RAT ( REAL*8   ) -- Effective ionosphere frequency for    *
! *                                phase delay date. Units: Hz.          *
! *      COV_PR_PH ( REAL*8   ) -- Covariance between phase delay rate   *
! *                                and fringe phase.                     *
! *                                Units: dimensionless.                 *
! *      COV_GR_GD ( REAL*8   ) -- Covariance between group delay and    *
! *                                group delay rate. Units: sec.         *
! *       ERR_MULT ( REAL*8   ) -- Multiplicative error factor for       *
! *                                making chi/sq of residual phases to   *
! *                                be close to their mathematical        *
! *                                expectation.                          *
! *       EFF_DURA ( REAL*8   ) -- Effective  scan duration, taking into *
! *                                account weights (sec).                *
! *            TEC ( REAL*8   ) -- Estimate of the total electron        *
! *                                contents in TECU.                     *
! *                                Computed only when solution mode is   *
! *                                PIMA__FINE_SEARCH_TEC, otherwise 0.0  *
! *       TEC_RATE ( REAL*8   ) -- Estimate of the rate of change of     *
! *                                the total electron content in TECU/s. *
! *                                Computed only when solution mode is   *
! *                                PIMA__FINE_SEARCH_TEC, otherwise 0.0  *
! *        TEC_ERR ( REAL*8   ) -- Error of the total electron           *
! *                                contents in TECU.                     *
! *                                Computed only when solution mode is   *
! *                                PIMA__FINE_SEARCH_TEC, otherwise 0.0  *
! *   TEC_RATE_ERR ( REAL*8   ) -- Error of the rate of change of        *
! *                                the total electron content in TECU/s. *
! *                                Computed only when solution mode is   *
! *                                PIMA__FINE_SEARCH_TEC, otherwise 0.0  *
! *      DECOR_TIM ( REAL*8   ) -- Decorrelation factor: the ratio of    *
! *                                the visibility coherently averaged    *
! *                                over time to the visibility           *
! *                                incoherently averaged over time.      *
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
! *  TIME_FRT ( REAL*8    ) -- Fringe reference time from the nominal    *
! *                            start of the observation.                 *
! *                            If the input value if PIMA__FRT_UNDF,     *
! *                            then the TIME_FRT is computed as the      *
! *                            weighted mean epoch counted from the      *
! *                            nominal start of the observation.         *
! *                            Otherwise, the input value is used.       *
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
! *                             positive non-zero in the case of errors. *
! *                                                                      *
! * ###  01-APR-2006    PIMA_2FFT   v2.14 (c)  L. Petrov 08-SEP-2024 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'astro_constants.i'
      INCLUDE   'pima.i'
      INCLUDE   'fftw3.f'
      INCLUDE   'vtd.i'
      TYPE     ( PIMA__TYPE  ) :: PIM
      TYPE     ( VTD__TYPE   ) :: VTD
      INTEGER*4  IND_OBS, LCHN, LFRQ, LTIM, IUER
      CHARACTER  FINAM_2D_PLOT*(*)
      REAL*8     FREQ_ARR(LCHN,LFRQ), FREQ_REF, AP_LEN, EFF_DURA, &
     &           TEC, TEC_RATE, TEC_ERR, TEC_RATE_ERR, DECOR_TIM
      REAL*4     WEI_1D(LTIM)
      COMPLEX*8  AC(LCHN,LFRQ,LTIM,2)
      REAL*8     TIME_FRT, GR_DEL(PIM__MFRA), GR_RAT, PH_ACC, &
     &           PH_RAT(PIM__MFRA), PHAS(PIM__MFRA), &
     &           AMPL(PIM__MFRA), SNR, GR_DEL_ERR(PIM__MFRA), &
     &           PH_DEL_ERR(PIM__MFRA), &
     &           PH_RAT_ERR(PIM__MFRA), GR_RAT_ERR, PH_ACC_ERR, &
     &           PHAS_ERR(PIM__MFRA),  GRAMBSP, EFF_FRQ_PHS, &
     &           EFF_FRQ_GRP, EFF_FRQ_RAT, COV_PR_PH, COV_GR_MD
      REAL*8     FRQ_BEG, FRQ_STEP, TOL_MP, FRQ_DIF_LIM, WEI_THR, NOI_FLOOR
      PARAMETER  ( TOL_MP = 0.9 )
      PARAMETER  ( FRQ_DIF_LIM = 0.001 )
      PARAMETER  ( NOI_FLOOR = 1.D-8 )
      INTEGER*4  L_MD, L_RT, L_MD_RAW, L_RT_RAW, L_MD_DEG, L_RT_DEG
      CHARACTER  STR*32, STR1*32
      COMPLEX*8  UV(LCHN,LFRQ,LTIM)
      COMPLEX*8, ALLOCATABLE :: UV_2D_WEI(:,:)
      REAL*4,    ALLOCATABLE :: AMPL_PLO(:,:), AMPL_PLO_RSC(:,:), FRQ_WEI(:,:), &
     &                          WEI_PNT(:), SQR_AMP_ARR(:)
      INTEGER*4, ALLOCATABLE :: IND_MD(:,:), IND_RT(:)
      REAL*8,    ALLOCATABLE :: TIM_ARR(:)
      LOGICAL*1, ALLOCATABLE :: FL_DEL_GRID_ARR(:)
      LOGICAL*1  FL_ITER_COARSE, FL_FRINGE_DFT
      COMPLEX*8  UV_MD3_TAB(LFRQ,-PIM__MBWN:PIM__MBWN), DRF_CHN, DRF_FRQ
      REAL*8     SD3_WIN, RT3_WIN, RT3_STEP, SD3_STEP, MD3_STEP, &
     &           RT3_INIT, SD3_INIT, MD_STEP, RT_STEP
      REAL*8     DEL_ARG(PIM__MFFT), DEL_VAL(PIM__MFFT), DEL_SPL(PIM__MFFT)
      REAL*8     RAT_ARG(PIM__MFFT), RAT_VAL(PIM__MFFT), RAT_SPL(PIM__MFFT)
      REAL*8     WORK_ARR(PIM__MFFT), GR_DEL_COARSE, PH_RAT_COARSE
      REAL*8     SUM_WEI, SUM_FR1, SUM_FR2, SUM_FQ1, SUM_FQ2, SUM_FRI, &
     &           SUM_DFI, DFR, WEI_SQ, FREQ_IF(PIM__MFRQ), TIME_FRT_NEW, &
     &           PH_DEL_APSO, PH_RAT_APSO, PH_ACC_APSO, SUM_WEI_1D
      REAL*8       PIMA__ACC_PT
      PARAMETER  ( PIMA__ACC_PT = 0.33D0  ) ! How many phase turns due to phase acceleration are allowed
      real*8      t1(128*1024), x1(128*1024)
      REAL*8      PIMA__PHS_APSO_LIM, M__SHR_NOI
      INTEGER*4   M_NOI, LTIM_LIM, PIMA__APSO_LTIM_LIM
      PARAMETER ( M_NOI = 32*1024 )
      PARAMETER ( M__SHR_NOI = 0.25D0 )
      PARAMETER ( LTIM_LIM = 3 )
      PARAMETER ( PIMA__PHS_APSO_LIM  = 1.D-5 )
      PARAMETER ( PIMA__APSO_LTIM_LIM = 5 )
      REAL*8     NOISE_RMS, NOISE_AVR, NOI_ARR(M_NOI)
      REAL*4     PHAS_ADD_R4, DP
      COMPLEX*8  UV_VAL
      REAL*8     RT_MAX, MD_MAX, SD_MOD, RT_MOD, AMPL_MAX, PHAS_MAX, &
     &           PH_RAT_GRID, GR_DEL_GRID, WPOI, MASK_CHN, DURA_MAX, &
     &           DURA_MIN, DURA_ACC, PHS_APSO_MAX, PHS_APSO_LIM, &
     &           DEL_MIN, DEL_MAX, DEL_CEN, RAT_MIN, RAT_MAX, RAT_CEN, &
     &           SKIP_BEG_TIME, SQR_AMPL_MAX, PH_ACC_BEG, PH_ACC_STEP, &
     &           PH_ACC_VAL, PH_ACC_DIF, PH_ACC_PREV, PH_ACC_COA, &
     &           AMPL_MAX_GLOB, DEL_QUAD_MAX, TIM_BEG_USED, TIM_END_USED, &
     &           APR_GR_DEL, APR_PH_RAT, APR_PH_ACC
      INTEGER*4  J1, J2, J3, J4, J5, J6, J7, J8, J9, J10, J11, J12, J13, &
     &           J14, J15, J16, J17, J18, J19, J20, J21, J22, J23, J24, &
     &           J25, J26, J27, J28, J29, J30, J31, J32, J33, J34, J35, J36, &
     &           ITURN_DEL, ITURN_RAT, NPOI, MAX_AMP(2), &
     &           K_EPC, ISD3_L, ISD3_H, IRT3_L, IRT3_H, IMD3_L, IMD3_H, IMD3, &
     &           IND_MD_MAX, KP, IP3, IDEV, IPL_DEL, IPL_RAT, IND_1, IND_2, &
     &           IND_NOI, KP_DEL, KP_RAT, KNOT_DEL, KNOT_RAT, IN, IS, IFRQ, &
     &           L_NOI, KTIM, UV_IND, IND_FRA, NTIM, IND_1_ORIG, FRG_IND, &
     &           IND_THR, N_ITER, LEPO, N_GR_DEL, IER
#ifdef GNU
      INTEGER*4     NTHR, NTHR_SAVED, SCHED_THR
#else
      ADDRESS__TYPE NTHR, NTHR_SAVED, SCHED_THR
#endif
      ADDRESS__TYPE :: ADR, SIZ
      LOGICAL*4  FL_RESCALE, FL_ALL_ZERO, FL_WEI_AUTOCORR, FL_MD_ADJUST
      INTEGER*4  IND_MAX_AMP(2), IND_MAX_ACC, MAX_AMP_ARR(2,64)
      INTEGER*8  PLAN_FFTW
#ifdef GNU
      INTEGER*4, EXTERNAL :: PIMA_COMPAR_UV, PIMA_COMPAR_NOISE
#else
      INTEGER*2, EXTERNAL :: PIMA_COMPAR_UV, PIMA_COMPAR_NOISE
#endif
      REAL*4,    EXTERNAL :: PHAS_CMPL_R4
      LOGICAL*4, EXTERNAL :: OMP_IN_PARALLEL
      INTEGER*4, EXTERNAL :: ILEN, I_LEN, OMP_GET_THREAD_NUM, GET_PROC_INFO
#ifdef GNU
      INTEGER*4, EXTERNAL :: OMP_GET_MAX_THREADS, OMP_GET_NUM_THREADS 
      LOGICAL*4, EXTERNAL :: IS_R8_NAN
#else
      ADDRESS__TYPE, EXTERNAL :: OMP_GET_MAX_THREADS, OMP_GET_NUM_THREADS
#endif
      REAL*8,    EXTERNAL :: FSPL8, RANDOM_NUMB, GET_GRPAMBSP
      CHARACTER  MJDSEC_TO_DATE*30
!
      FL_RESCALE = .FALSE.  ! For making fringe plots for publications
      CALL GETENVAR ( 'PIMAVAR_PHS_APSO_LIM', STR )
      IF ( ILEN(STR) > 0 ) THEN
           READ ( UNIT=STR, FMT='(F12.5)', IOSTAT=IER ) PHS_APSO_LIM
           IF ( IER .NE. 0 ) PHS_APSO_LIM = PIMA__PHS_APSO_LIM
         ELSE
           PHS_APSO_LIM = PIMA__PHS_APSO_LIM
      END IF
      CALL GETENVAR ( 'PIMAVAR_SCAN_LEN_SKIP', STR )
      IF ( ILEN(STR) > 0 ) THEN
           READ ( UNIT=STR, FMT='(F12.5)', IOSTAT=IER ) PIM%CONF%SCAN_LEN_SKIP
      END IF
      FL_WEI_AUTOCORR = .FALSE.
      CALL GETENVAR ( 'PIMAVAR_WEI_AUTOCORR', STR )
      IF ( STR(1:3) == 'yes' .OR. STR(1:3) == 'YES' ) FL_WEI_AUTOCORR = .TRUE.
!
      CALL GETENVAR ( 'PIMAVAR_FRINGE_DFT', STR )
      IF ( STR == 'YES' .OR. STR == 'yes' ) THEN
           FL_FRINGE_DFT = .TRUE.
         ELSE 
           FL_FRINGE_DFT = .FALSE.
      END IF
!
      FL_MD_ADJUST = .FALSE. 
      CALL GETENVAR ( 'PIMAVAR_MD_ADJST', STR )
      IF ( STR == 'YES' .OR. STR == 'yes' ) THEN
           FL_MD_ADJUST = .TRUE.
      END IF
!
      APR_GR_DEL = 0.0D0
      CALL GETENVAR ( 'PIMAVAR_APR_GR_DEL', STR )
      IF ( ILEN(STR) > 0.0D0 ) THEN
           READ ( UNIT=STR, FMT=*, IOSTAT=IER ) APR_GR_DEL 
           IF ( PIM%CONF%DEBUG_LEVEL > 0 ) THEN
                WRITE ( 6, * ) 'PIMA_2FFT: APR_GR_DEL= ', APR_GR_DEL
           END IF
      END IF
!
      APR_PH_RAT = 0.0D0
      CALL GETENVAR ( 'PIMAVAR_APR_PH_RAT', STR )
      IF ( ILEN(STR) > 0.0D0 ) THEN
           READ ( UNIT=STR, FMT=*, IOSTAT=IER ) APR_PH_RAT
           IF ( PIM%CONF%DEBUG_LEVEL > 0 ) THEN
                WRITE ( 6, * ) 'PIMA_2FFT: APR_PH_RAT= ', APR_PH_RAT
           END IF
      END IF
!
      APR_PH_ACC = 0.0D0
      CALL GETENVAR ( 'PIMAVAR_APR_PH_ACC', STR )
      IF ( ILEN(STR) > 0.0D0 ) THEN
           READ ( UNIT=STR, FMT=*, IOSTAT=IER ) APR_PH_ACC
           IF ( PIM%CONF%DEBUG_LEVEL > 0 ) THEN
                WRITE ( 6, * ) 'PIMA_2FFT: APR_PH_ACC= ', APR_PH_ACC
           END IF
      END IF
!
      AMPL = 0.0D0
      SNR  = 0.0D0
      TEC  = 0.0D0
      TEC_RATE = 0.0D0
      TEC_ERR  = 0.0D0
      TEC_RATE_ERR = 0.0D0
!
      FRG_IND = PIM%OBS(IND_OBS)%REF_FRG_INDS(PIM%CONF%FRQ_GRP)
      IF ( FRG_IND .LE. 0 ) THEN
           CALL CLRCH ( STR  )
           CALL CLRCH ( STR1 )
           CALL INCH  ( IND_OBS, STR  )
           CALL INCH  ( FRG_IND, STR1 )
           CALL ERR_LOG ( 7611, IUER, 'PIMA_2FFT', 'Trap of internal '// &
     &         'control: observation '//STR(1:I_LEN(STR))//' does not '// &
     &         'have APs with frequency index '//STR1 )
           RETURN 
      END IF
!
      IF ( PIM%CONF%FRIB_DELAY_WINDOW_WIDTH .LE. 0.0D0 ) THEN
!
! -------- Set the natural search window for group delay
!
           IF ( PIM%FRQ(1,FRG_IND)%CHAN_WIDTH > PIMA__MIN_FRQ ) THEN
                PIM%CONF%FRIB_DELAY_WINDOW_WIDTH = 1.D0/PIM%FRQ(1,FRG_IND)%CHAN_WIDTH
              ELSE 
                PIM%CONF%FRIB_DELAY_WINDOW_WIDTH = 1.D0/PIMA__MIN_FRQ 
           END IF
      END IF
!
      IF ( PIM%CONF%FRIB_RATE_WINDOW_WIDTH .LE. 0.0D0 ) THEN
!
! -------- Set the natural search window for delay rate 
!
           IF ( PIM%FRQ(PIM%CONF%BEG_FRQ,FRG_IND)%FREQ > PIMA__MIN_FRQ    .AND. &
     &          PIM%OBS(IND_OBS)%AP_LEN > PIMA__MIN_AP_LEN       ) THEN
                PIM%CONF%FRIB_RATE_WINDOW_WIDTH = 1.D0/(PIM%OBS(IND_OBS)%AP_LEN*PIM%FRQ(PIM%CONF%BEG_FRQ,FRG_IND)%FREQ)
              ELSE 
                PIM%CONF%FRIB_RATE_WINDOW_WIDTH = 1.D0/(PIMA__MIN_AP_LEN*PIMA__MIN_FRQ)
           END IF
      END IF
!
      NTHR = PIM%CONF%NUM_THREADS
      CALL OMP_SET_NUM_THREADS ( %VAL(NTHR) )
!
      IF ( PIM%CONF%DEBUG_LEVEL == 8 ) THEN
           CALL CPU_TIMER ( %VAL(0) )
      END IF
!
! --- FRQ_BEG  -- cyclic lowest frequency
! --- FRQ_STEP -- step in frequency sequence
! --- WEI_THR  -- threshold for using data with poor weights
! --- TIME_FRT -- Fringe reference time
!
      FRQ_BEG  = FREQ_ARR(1,1)
      FRQ_STEP = FREQ_ARR(2,1) - FREQ_ARR(1,1)
      IF ( FRQ_STEP < PIMA__MIN_FRQ ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( PIM%CONF%BEG_FRQ, STR )
           CALL CLRCH ( STR1 )
           CALL INCH  ( FRG_IND, STR1 )
           CALL ERR_LOG ( 7612, IUER, 'PIMA_2FFT', 'Trap of the internal '// &
     &         'control: frequency step for IF '//TRIM(STR)//' from '// &
     &         'frequncy group '//TRIM(STR1)//' is too small. '// &
     &         'You cannot include this IF in fringe fitting' )
           RETURN 
      END IF
      DEL_MAX  =  1.D0/(2.D0*FRQ_STEP)
      DEL_MIN  = -1.D0/(2.D0*FRQ_STEP)
      WEI_THR  = PIM%CONF%FRIB_WEIGHTS_THRESHOLD
      TIME_FRT_NEW = 0.0D0 ! Initialization
!
! --- Compute the number of points in the window over group delay.
! --- Take into account oversampling
! --- The number of points should be a power of 2
!
      L_MD_RAW = IDNINT( (FREQ_ARR(LCHN,LFRQ) - FREQ_ARR(1,1) + (FREQ_ARR(2,1) - FREQ_ARR(1,1)) )/FRQ_STEP)* &
     &            PIM%CONF%FRIB_OVERSAMPLE_MD
      IF ( .NOT. FL_MD_ADJUST ) THEN
           L_MD = L_MD_RAW
           IF ( DABS(L_MD_RAW/5 - L_MD_RAW/5.0D0) < 1.D-10 ) THEN
                L_MD = L_MD_RAW
             ELSE
!
! ------------- Adjusting the FFT size if the number of channel is a power of 2
!
                L_MD_DEG = DLOG(1.0D0*L_MD_RAW*TOL_MP)/DLOG(2.0D0)+1
                L_MD = NINT ( 2.0D0** DFLOAT(L_MD_DEG) )
                IF ( L_MD < L_MD_RAW ) L_MD = 2*L_MD
                IF ( L_MD < L_MD_RAW ) L_MD = 2*L_MD
           END IF
      END IF
!
! --- Compute the number of points in the window over phase delay rate.
! --- Take into account oversampling
! --- It should be a power of 2
!
      RAT_MAX =  1.D0/(2.D0*AP_LEN*FREQ_ARR(1,1))
      RAT_MIN = -1.D0/(2.D0*AP_LEN*FREQ_ARR(1,1))
      KTIM = PIM%OBS(IND_OBS)%NUM_AP_SPAN(PIM%OBS(IND_OBS)%REF_FRG_INDS(PIM%CONF%FRQ_GRP))
      NTIM = (PIM%OBS(IND_OBS)%TIM_END - PIM%OBS(IND_OBS)%TIM_BEG)/AP_LEN + 1
      L_RT_RAW = NTIM*PIM%CONF%FRIB_OVERSAMPLE_RT
      L_RT_DEG = DLOG(1.0D0*L_RT_RAW*TOL_MP)/DLOG(2.0D0)+1
      L_RT = NINT ( 2.0D0** DFLOAT(L_RT_DEG) )
      IF ( L_RT < L_RT_RAW ) L_RT = 2*L_RT
      IF ( L_RT < L_RT_RAW ) L_RT = 2*L_RT
!
      IF ( KTIM > L_RT) L_RT = 2*L_RT
      IF ( NTIM > L_RT) L_RT = 2*L_RT
!
      IF ( KTIM > L_RT) THEN
           write ( 6, * ) ' NTIM = ', NTIM, ' OV_RT = ', PIM%CONF%FRIB_OVERSAMPLE_RT
           write ( 6, * ) ' AP_LEN = ', AP_LEN
           write ( 6, * ) ' TIMS = ', PIM%OBS(IND_OBS)%TIM_BEG, PIM%OBS(IND_OBS)%TIM_END
           write ( 6, * ) ' L_RT_RAW = ', L_RT_RAW, ' L_RT_DEG = ', L_RT_DEG, &
     &                    ' TOL_MP = ', TOL_MP
           CALL CLRCH ( STR )
           CALL INCH  ( KTIM, STR )
           CALL CLRCH ( STR1 )
           CALL INCH  ( L_RT, STR1 )
           CALL ERR_LOG ( 7613, IUER, 'PIMA_2FFT', 'Trap of internal '// &
     &         'control: K_TIM > L_RT -- L_TIM= '//STR(1:I_LEN(STR))// &
     &         ' while L_RT= '//STR1 )
           RETURN
      END IF
!
      IF ( KTIM < LTIM_LIM ) THEN
           IF ( PIM%CONF%DEBUG_LEVEL .GE. 1 ) THEN
                WRITE ( 6, * ) 'PIMA_2FFT Failure: Too small KTIM = ', KTIM, &
     &                         ' LTIM=', LTIM, ' NTIM= ', NTIM, &
     &                         ' NUM_AP_SPAN= ', PIM%OBS(IND_OBS)%NUM_AP_SPAN
           END IF
!
           SNR  = 0.0D0
           AMPL = 0.0D0
           GR_DEL = 0.0D0
           PH_RAT = 0.0D0
           PHAS   = 0.0D0
           TIME_FRT = 0.0D0
           CALL ERR_LOG ( 0, IUER )
           RETURN
      END IF
!
! --- Step of multiband group delay (in sec) and phase delay rate (d/l)
!
      MD_STEP = 1.0D0/(FRQ_STEP*L_MD)
      RT_STEP = 1.0D0/(FRQ_BEG*AP_LEN*L_RT)
!
      IF ( PIM%CONF%DEBUG_LEVEL .GE. 2 ) THEN
           WRITE ( 6, 210 ) L_MD, L_RT, MD_STEP, RT_STEP, &
     &                      DEL_MIN, DEL_MAX, RAT_MIN, RAT_MAX, FREQ_REF
 210       FORMAT ( 'PIMA_2FFT L_MD = ', I7, ' L_RT = ', I6, &
     &              ' MD_STEP = ', 1PD10.4,' RT_STEP = ', 1PD10.4/ &
     &              ' Del_win: [', 1PD11.4, ', ', 1PD11.4,']',4X, &
     &              ' Rat_win: [', 1PD11.4, ', ', 1PD11.4,'] ', &
     &              ' Freq_ref: ', 1PD13.6, ' Hz' )
      END IF
!
! --- Allocate dynamic memory for 2D weighted arrays of UV data for FFT
!
      ALLOCATE ( UV_2D_WEI(L_MD,L_RT), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH  ( STR )
           CALL IINCH8 ( INT8(8)*INT8(L_MD)*INT8(L_RT), STR )
           CALL ERR_LOG ( 7615, IUER, 'PIMA_2FFT', 'Error in an attempt '// &
     &         'to allocate '//STR(1:I_LEN(STR))//' bytes of dynamic '// &
     &         'memory for array UV_2D_WEI' )
           RETURN
      END IF
      UV_2D_WEI = (0.0, 0.0)
!
      ALLOCATE ( SQR_AMP_ARR(L_MD), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH8 ( INT8(8)*INT8(L_MD), STR )
           CALL ERR_LOG ( 7616, IUER, 'PIMA_2FFT', 'Error in an attempt '// &
     &         'to allocate '//STR(1:I_LEN(STR))//' bytes of dynamic '// &
     &         'memory for array SQR_AMP_ARR' )
           RETURN
      END IF
      SQR_AMP_ARR = 0.0D0
!
      ALLOCATE ( FL_DEL_GRID_ARR(L_MD), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH8 ( INT8(8)*INT8(L_MD), STR )
           CALL ERR_LOG ( 7617, IUER, 'PIMA_2FFT', 'Error in an attempt '// &
     &         'to allocate '//STR(1:I_LEN(STR))//' bytes of dynamic '// &
     &         'memory for array FL_DEL_GRID_ARR' )
           RETURN
      END IF
      FL_DEL_GRID_ARR = .FALSE.
!
! --- Alocate dynamic memory for index table: MD --> raw of FFT(UV)
!
      ALLOCATE ( IND_MD(LCHN,PIM%CONF%BEG_FRQ:PIM%CONF%END_FRQ), &
     &           STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH  ( STR )
           CALL IINCH8 ( 8*INT8(LCHN)*INT8(PIM%CONF%END_FRQ-PIM%CONF%BEG_FRQ+1), STR )
           CALL ERR_LOG ( 7618, IUER, 'PIMA_2FFT', 'Error in an attempt '// &
     &         'to allocate '//STR(1:I_LEN(STR))//' bytes of dynamic '// &
     &         'memory for array IND_MD' )
           RETURN
      END IF
      IND_MD = 0
!
      ALLOCATE ( IND_RT(LTIM), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( 4*LTIM, STR )
           CALL ERR_LOG ( 7619, IUER, 'PIMA_2FFT', 'Error in an attempt '// &
     &         'to allocate '//STR(1:I_LEN(STR))//' bytes of dynamic '// &
     &         'memory for array IND_RT' )
           RETURN
      END IF
      IND_RT = 0
!
      ALLOCATE ( FRQ_WEI(LCHN,LFRQ), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH   ( STR )
           CALL IINCH8  ( INT8(4)*INT8(LCHN)*INT8(LFRQ), STR )
           CALL ERR_LOG ( 7620, IUER, 'PIMA_2FFT', 'Error in an attempt '// &
     &         'to allocate '//STR(1:I_LEN(STR))//' bytes of dynamic '// &
     &         'memory for array FRQ_WEI' )
           RETURN
      END IF
      FRQ_WEI = 0.0
!
      ALLOCATE ( TIM_ARR(LTIM), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( 8*LTIM, STR )
           CALL ERR_LOG ( 7621, IUER, 'PIMA_2FFT', 'Error in an attempt '// &
     &         'to allocate '//STR(1:I_LEN(STR))//' bytes of dynamic '// &
     &         'memory for array TIM_ARR' )
           RETURN
      END IF
      TIM_ARR = 0.0D0
!
      ALLOCATE ( WEI_PNT(LTIM), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( 8*KTIM, STR )
           CALL ERR_LOG ( 7622, IUER, 'PIMA_2FFT', 'Error in an attempt '// &
     &         'to allocate '//STR(1:I_LEN(STR))//' bytes of dynamic '// &
     &         'memory for array WEI_PNT' )
           RETURN
      END IF
      WEI_PNT = 0.0
!
! --- Build the index table: IND_CHN,IND_FRQ --> raw of FFT(UV) over group delay
!
      IFRQ = 0
      DO 410 J1=PIM%CONF%BEG_FRQ,PIM%CONF%END_FRQ
         IFRQ = IFRQ + 1
         DO 420 J2=1,LCHN
            IND_MD(J2,J1) = IDNINT( (FREQ_ARR(J2,IFRQ) - FRQ_BEG)/FRQ_STEP ) + 1
            IF ( DABS( ( (IND_MD(J2,J1) - 1)*FRQ_STEP + FRQ_BEG - &
     &                    FREQ_ARR(J2,IFRQ))/FRQ_STEP ) > FRQ_DIF_LIM .AND. &
     &           .NOT. FL_FRINGE_DFT                                        ) THEN
!
                 write ( 6, * ) ' j2=',j2,' ifrq=',ifrq, ' ind_md(j2,j1)= ', ind_md(j2,j1)
                 write ( 6, * ) ' FREQ_ARR(J2,IFRQ) =', FREQ_ARR(J2,IFRQ)
                 write ( 6, * ) ' FF1= ', (IND_MD(J2,J1) - 1)*FRQ_STEP + FRQ_BEG
                 write ( 6, * ) ' FF2= ', FREQ_ARR(J2,IFRQ)
                 write ( 6, * ) ' FRQ_STEP =', FRQ_STEP, ' FRQ_BEG = ', FRQ_BEG
                 write ( 6, * ) ' beg_frq_ind: ', pim%conf%beg_frq, &
     &                          ' end_frq_ind: ', pim%conf%end_frq
                 write ( 6, * ) ' freq_arr(1,1:e-b+1) = ', FREQ_ARR(1,1:PIM%CONF%END_FRQ-PIM%CONF%BEG_FRQ+1)
                 write ( 6, * ) ' freq_arr(1:lchn,1) = ', FREQ_ARR(1:LCHN,IFRQ)
                 write ( 6, * ) ' f1 = ', DABS( ( (IND_MD(J2,J1) - 1)*FRQ_STEP + FRQ_BEG - &
     &                                    FREQ_ARR(J2,IFRQ))/FRQ_STEP ), &
     &                          ' f2 = ', FRQ_DIF_LIM
                 CALL ERR_LOG ( 7623, IUER, 'PIMT_2FFT', 'Frequencies are '// &
     &               'not commensurate' )
                 RETURN
            END IF
 420     CONTINUE
         PIM%OBS(IND_OBS)%EFF_DUR(1) = 0.0
 410  CONTINUE
!
      SKIP_BEG_TIME = 0.0D0
      CALL GETENVAR ( 'PIMAVAR_SKIP_BEG_TIME', STR )
      IF ( ILEN(STR) > 0 ) THEN
           IF ( INDEX( STR, '.' ) == 0 ) STR = STR(1:I_LEN(STR))//'.0'
           READ  ( UNIT=STR, FMT='(F8.2)', IOSTAT=IER ) SKIP_BEG_TIME
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 7624, IUER, 'PIMA_2FFT', 'Failure '// &
     &              'in decoding environment vairable '// &
     &              'PIMAVAR_SKIP_BEG_TIME '//STR )
                RETURN 
           END IF
      END IF
!
      DURA_MIN =  -100.0D0
      CALL GETENVAR ( 'PIMAVAR_DURA_MIN', STR )
      IF ( ILEN(STR) > 0 ) THEN
           READ ( UNIT=STR, FMT='(F10.2)' ) DURA_MIN
      END IF
!
! --- Populate UV matrix and apply weights
!
      K_EPC = 0
      WPOI  = 0.0D0
      DURA_ACC = 0.0D0
      IF ( PIM%CONF%DEBUG_LEVEL == 14 ) THEN
           STR = MJDSEC_TO_DATE ( PIM%MJD_0, PIM%TAI_0 + &
     &                            PIM%TIM_R8(PIM%UV_IND(PIM%OBS(IND_OBS)%UV_IND(1,FRG_IND))%TIM_IND), &
     &                            IER )
           WRITE ( 6, 252 ) IND_OBS, PIM%OBS(IND_OBS)%TIM_BEG, &
     &                      STR(1:24), FRG_IND, &
     &                      PIM%TIM_R8(PIM%UV_IND(PIM%OBS(IND_OBS)%UV_IND(1,FRG_IND))%TIM_IND), &
     &                      PIM%TIM_R8(PIM%UV_IND(PIM%OBS(IND_OBS)%UV_IND(PIM%OBS(IND_OBS)%NUM_EPC(FRG_IND),FRG_IND))%TIM_IND) 
 252       FORMAT ( 'PIMA_2FFT IND_OBS= ', I5, ' Tim_beg: ', F10.3, &
     &              ' Date: ', A, ' FRG_IND: ', I1/ &
     &              '          TIM_BEG: ', F12.5,' TIM_END: ', F12.5 )
      END IF
      SUM_WEI_1D = 0.0
      TIM_BEG_USED = 1.D11
      TIM_END_USED = 1.D11
      DO 430 J3=1,LTIM
         IND_RT(J3) = 0
!
! ------ Update accummulated duration
!
         IF ( PIM%CONF%DEBUG_LEVEL == 14 ) THEN
              WRITE  ( 6, 251 ) J3, WEI_1D(J3), WEI_THR
 251          FORMAT ( 'Ind: 'I5, ' Wei = ', F15.8, ' WEI_THR= ', F15.8 ) 
         END IF
         UV_IND  = PIM%OBS(IND_OBS)%UV_IND(J3,FRG_IND)
         TIM_ARR(J3) = PIM%TIM_R8(PIM%UV_IND(UV_IND)%TIM_IND) - &
     &                 PIM%OBS(IND_OBS)%TIM_BEG
         IF ( WEI_1D(J3) < WEI_THR ) GOTO 430
         IF ( TIM_BEG_USED > 1.D10 )  THEN
              TIM_BEG_USED = TIM_ARR(J3) 
         END IF
         TIM_END_USED = TIM_ARR(J3) + AP_LEN
         SUM_WEI_1D = SUM_WEI_1D + WEI_1D(J3)
         K_EPC = K_EPC + 1
         IF ( TIM_ARR(J3) < PIM%CONF%SCAN_LEN_SKIP ) THEN
              WEI_1D(J3) = 0.0D0
              K_EPC = K_EPC - 1
              GOTO 430
         END IF 
!
         DURA_ACC = TIM_END_USED - TIM_BEG_USED
         IF ( DURA_ACC > PIM%CONF%SCAN_LEN_USED ) THEN
!
! ----------- If scan duration exceeded maximum, discard the data
!
              DURA_ACC = DURA_ACC - AP_LEN
              WEI_1D(J3) = 0.0D0
              K_EPC = K_EPC - 1
              GOTO 430
         END IF
!
         IF ( TIM_ARR(J3) + PIM%OBS(IND_OBS)%AP_LEN/2.0D0 < SKIP_BEG_TIME ) THEN
              WEI_1D(J3) = 0.0D0
              K_EPC = K_EPC - 1
              GOTO 430
         END IF 
!
! ------ Get the index of the time slot. We consider that time tag is uniformly
! ------ sampled with AP_LEN step, but some samples may be lost.
!
         IND_RT(J3) = IDNINT ( TIM_ARR(J3)/PIM%OBS(IND_OBS)%AP_LEN ) + 1
         IF ( IND_RT(J3) < 1 .OR. IND_RT(J3) > L_RT ) THEN
              WRITE ( 6, * ) ' J3= ', J3, ' IND_RT = ', IND_RT(J3), &
     &                       ' LTIM= ', LTIM, ' L_RT= ', L_RT
              CALL ERR_LOG ( 7625, IUER, 'PIMA_2FFT', 'Trap of internal '// &
     &            'control: IND_RT is wrong' )
              RETURN
         END IF
         IFRQ = 0
         FL_ALL_ZERO = .TRUE.
         DO 450 J5=PIM%CONF%BEG_FRQ,PIM%CONF%END_FRQ
            IFRQ = IFRQ + 1
            FREQ_IF(IFRQ) = 0.0D0
            DO 460 J6=1,LCHN
               WEI_PNT(J3) = WEI_1D(J3)
!
! ------------ Apply weight
!
               IF ( UV(J6,IFRQ,J3) .NE. CMPLX ( 0.0, 0.0 ) ) THEN
                    FL_ALL_ZERO = .FALSE.
               END IF
!
               IF ( PIM%BANDPASS_MASK_STYLE .NE. PIMA__NO ) THEN
!
! ----------------- Apply bandpass mask for computing WEI_PNT
!
                    MASK_CHN = PIM%BANDPASS_MASK(J6,J5,PIM%OBS(IND_OBS)%STA_IND(1),PIMA__MASK_FRNG) * &
     &                         PIM%BANDPASS_MASK(J6,J5,PIM%OBS(IND_OBS)%STA_IND(2),PIMA__MASK_FRNG)
!
                    WEI_PNT(J3) = WEI_PNT(J3) * MASK_CHN
                    IF ( FREQ_IF(IFRQ) == 0.0D0 .AND. MASK_CHN > 0.0D0 ) THEN
                         FREQ_IF(IFRQ) = FREQ_ARR(1,IFRQ)
                    END IF
                  ELSE
                    IF ( FREQ_IF(IFRQ) == 0.0D0 ) THEN
                         FREQ_IF(IFRQ) = FREQ_ARR(1,IFRQ)
                    END IF
               END IF
!
               IF ( IND_RT(J3) .GE. 1  .AND.  IND_RT(J3) .LE. NTIM ) THEN
                    UV_2D_WEI(IND_MD(J6,J5),IND_RT(J3)) = UV(J6,IFRQ,J3)*WEI_PNT(J3)
                  ELSE
                    UV_2D_WEI(IND_MD(J6,J5),IND_RT(J3)) = 0.0
               END IF
!
               FRQ_WEI(J6,IFRQ) = FRQ_WEI(J6,IFRQ) + WEI_PNT(J3)
               TIME_FRT_NEW = TIME_FRT_NEW + WEI_PNT(J3)*TIM_ARR(J3)
               WPOI = WPOI + WEI_PNT(J3)
 460        CONTINUE
 450     CONTINUE
         IF ( FL_ALL_ZERO ) THEN
              WEI_PNT(J3) = 0.0D0
            ELSE
              WEI_PNT(J3) = WEI_1D(J3)
         END IF
         IF ( PIM%CONF%DEBUG_LEVEL == 14 ) THEN
              STR = MJDSEC_TO_DATE ( PIM%MJD_0, PIM%TAI_0 + &
     &                               PIM%OBS(IND_OBS)%TIM_BEG + &
     &                               TIM_ARR(J3), STR )
              WRITE  ( 6, 254 ) J3, TIM_ARR(J3), IND_RT(J3), WEI_1D(J3), &
     &                          STR(1:24), UV(1,1,J3), &
     &                          FL_ALL_ZERO
 254          FORMAT ( 'PIMA_2FFT J3= ', I5, ' Tim: ', F9.3, &
     &                 ' Ind_rt: ', I4, ' WEI_PT: ', F8.5, ' Date: ',A, &
     &                 ' UV-1,1= ', F15.7, 1X, F15.7, ' FZ= ', L1 )
         END IF
 430  CONTINUE
!
      IF ( DURA_ACC < DURA_MIN ) THEN
           DEALLOCATE ( FRQ_WEI   )
           DEALLOCATE ( IND_MD    )
           DEALLOCATE ( IND_RT    )
           DEALLOCATE ( UV_2D_WEI )
           DEALLOCATE ( TIM_ARR   )
           SNR = 0.0D0
           CALL ERR_LOG ( 0, IUER )
           RETURN
      END IF
      LEPO = NINT ( (TIM_ARR(LTIM) - TIM_ARR(1))/PIM%OBS(IND_OBS)%AP_LEN ) + 1
      NPOI = LEPO*(PIM%CONF%END_FRQ - PIM%CONF%BEG_FRQ + 1)*LCHN
!
      IF ( WPOI < 1.0D0 ) THEN
           IF ( PIM%CONF%DEBUG_LEVEL .GE. 1 ) THEN
                WRITE ( 6, * ) 'PIMA_2FFT Failure: too small WPOI = ', WPOI
                WRITE ( 6, * ) 'PIMA_2FFT consider to decrease FRIB.WEIGHTS_THRESHOLD ', PIM%CONF%FRIB_WEIGHTS_THRESHOLD
           END IF
!
           SNR      = 0.0D0
           AMPL     = 0.0D0
           GR_DEL   = 0.0D0
           PH_RAT   = 0.0D0
           PHAS     = 0.0D0
           TIME_FRT = 0.0D0
           CALL ERR_LOG ( 7627, IUER, 'PIMA_2FFT', 'Failure: did not '// &
     &                   'collect enough data for fringe fitting' )
           RETURN
      END IF
      GRAMBSP = GET_GRPAMBSP ( LFRQ, FREQ_IF, LCHN*PIM%CHAN_BW, PIM%CHAN_BW )
      IF ( PIM%CONF%DEBUG_LEVEL .GE. 6 ) THEN
           WRITE ( 6, * ) 'PIMA_2FFT IND_OBS: ', IND_OBS,  ' LTIM= ', LTIM, ' LEPO= ', LEPO
      END IF 
!
! --- Fringe reference time is the weighted mean epoch
!
      TIME_FRT_NEW = TIME_FRT_NEW/WPOI
!
      IF ( IS_R8_NAN(TIME_FRT) ) THEN
           TIME_FRT = TIME_FRT_NEW
      END IF
      IF ( DABS(TIME_FRT) > PIMA__FRT_UNDF/2.0D0 ) THEN
!
! -------- The input fringe reference time is "undefined". Let us use
! -------- the fringe reference time which we just have computed
!
           TIME_FRT = TIME_FRT_NEW
      END IF
      PIM%OBS(IND_OBS)%FRT_OFFSET(1) = TIME_FRT
!
      PH_ACC_APSO = 0.0D0
      IF ( K_EPC .GE. PIMA__APSO_LTIM_LIM ) THEN
           CALL ERR_PASS ( IUER, IER )
           CALL PIMA_APSO_FIX ( PIM, VTD, IND_OBS, LFRQ, LTIM, LCHN,        &
     &                          PH_DEL_APSO, PH_RAT_APSO, PH_ACC_APSO, DEL_QUAD_MAX, &
     &                          PHS_APSO_MAX, PHS_APSO_LIM, TIM_ARR,       &
     &                          .TRUE., UV, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 7628, IUER, 'PIMA_2FFT', 'Failure to perform '// &
     &              'correction for quadratic term in fringe phase for '// &
     &              'a significant offset in source positions' )
                RETURN
           END IF
         ELSE
           PH_DEL_APSO  = 0.0D0
           PH_RAT_APSO  = 0.0D0
           PHS_APSO_MAX = 0.0D0
      END IF
      IF ( PIM%CONF%DEBUG_LEVEL .GE. 5 ) THEN
           CALL CLRCH ( STR )
           IF ( PHS_APSO_MAX > PHS_APSO_LIM ) THEN
                STR ='UV are updated'
           END IF
           WRITE ( 6, 110 ) PHS_APSO_MAX, PHS_APSO_LIM, PH_DEL_APSO, &
     &                      PH_RAT_APSO, PH_ACC_APSO, STR(1:I_LEN(STR))
 110       FORMAT ( 'PIMA_2FFT  PHS_APSO_MAX: ',1PD12.5, &
     &               ' rad  PHS_APSO_LIM: ', 1PD11.3/        &
     &               ' PHS_APSO_DEL: ', 1PD12.5, &
     &               ' PHS_APSO_RAT: ', 1PD12.5, &
     &               ' PHS_APSO_ACC: ', 1PD12.5, 2X, A  )

           CALL ERR_PASS ( IUER, IER )
           CALL PIMA_APSO_COMP ( PIM, VTD, IND_OBS, PH_DEL_APSO,             &
     &                           PH_RAT_APSO, PH_ACC_APSO, DEL_QUAD_MAX, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 7629, IUER, 'PIMA_2FFT', 'Failure to perform '// &
     &              'correction for quadratic term in fringe phase for '// &
     &              'a significant offset in source positions' )
                RETURN
           END IF
           WRITE ( 6, 115 ) PH_DEL_APSO, PH_RAT_APSO, PH_ACC_APSO, DEL_QUAD_MAX
 115       FORMAT ( 'PIMA_2FFT  PHS_apso_del: ', 1PD12.5, &
     &               ' PHS_apso_rat: ', 1PD12.5, &
     &               ' PHS_apso_acc: ', 1PD12.5, &
     &               ' DEL_quad_max: ', 1PD12.5  )
      END IF
      IF ( PIM%CONF%PHASE_ACCELERATION .NE. 0.0D0 ) THEN
           CALL ERR_PASS ( IUER, IER )
           CALL PIMA_APPLY_ACCEL ( PIM, LCHN, LFRQ, LTIM, TIME_FRT, &
     &                             TIM_ARR, WEI_PNT, &
     &                             PIM%CONF%PHASE_ACCELERATION, UV, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 7630, IUER, 'PIMA_2FFT', 'Failre to perform '// &
     &              'correction for phase acceleration term in fringe phase' )
                RETURN
           END IF
      END IF
!
      IF ( PIM%CONF%PHASE_ACCEL_MIN .NE. 0.0D0 .OR. &
     &     PIM%CONF%PHASE_ACCEL_MAX .NE. 0.0D0      ) THEN
!
           PH_ACC_BEG = PIM%CONF%PHASE_ACCEL_MIN 
           PH_ACC_STEP = PIMA__ACC_PT*8.0D0/((LTIM*AP_LEN)**2*FREQ_REF)
           N_ITER = INT((PIM%CONF%PHASE_ACCEL_MAX - PIM%CONF%PHASE_ACCEL_MIN)/ &
     &                     PH_ACC_STEP) + 1
           IF ( PIM%CONF%DEBUG_LEVEL .GE. 4 ) THEN
                WRITE ( 6, 228 ) N_ITER, PH_ACC_BEG, PH_ACC_STEP
 228            FORMAT ( 'Start ', I4, ' iterations of coarse fitting with start ', &
     &                   'phase accelertation ',1PD12.4, ' and step ', &
     &                    1PD11.4 , ' 1/s' )
           END IF
           PH_ACC_PREV = 0.0D0
           FL_ITER_COARSE = .TRUE.
         ELSE 
           FL_ITER_COARSE = .FALSE.
           PH_ACC = 0.0D0
           N_ITER = 1
      END IF      
!
      IF ( .NOT. ( APR_GR_DEL == 0.0D0 .AND. APR_PH_RAT == 0.0D0 .AND. APR_PH_ACC == 0.0D0 ) ) THEN
!
! -------- Perform phase rotation for applying apriori 
!
           CALL PIMA_UV_APR_APPLY ( PIM, LCHN, LFRQ, LTIM, TIME_FRT, TIM_ARR, &
     &                              APR_GR_DEL, APR_PH_RAT, APR_PH_ACC, UV )
           CALL PIMA_UV2_APR_APPLY ( PIM, LCHN, LFRQ, LTIM, TIME_FRT, TIM_ARR, &
     &                               L_MD, L_RT, IND_MD, IND_RT, &
     &                               APR_GR_DEL, APR_PH_RAT, APR_PH_ACC, UV_2D_WEI )
      END IF
!
      IND_MAX_AMP(1) = 0
      IND_MAX_AMP(2) = 0
      IND_MAX_ACC    = 0
      AMPL_MAX_GLOB  = -1.0D0
      IF ( PIM%CONF%DEBUG_LEVEL == 8 ) THEN
           CALL CPU_TIMER ( STR ) !
           WRITE ( 6, '(A)' ) 'PIMA_FFT Before coarse fitting iterations timing: '//STR(1:27)
           CALL CPU_TIMER ( %VAL(0) ) !
      END IF
!
      DO 490 J9=1,N_ITER
         IF ( FL_ITER_COARSE ) THEN
              PH_ACC_VAL = PH_ACC_BEG + (J9-1)*PH_ACC_STEP 
              PH_ACC_DIF = PH_ACC_VAL - PH_ACC_PREV
              CALL ERR_PASS ( IUER, IER )
              CALL PIMA_APPLY_ACCEL ( PIM, LCHN, LFRQ, LTIM, TIME_FRT, &
     &                                TIM_ARR, WEI_PNT, PH_ACC_DIF, UV, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 7631, IUER, 'PIMA_2FFT', 'Failre to perform '// &
     &                 'correction for phase acceleration term in fringe phase' )
                   RETURN
              END IF
!
              IF ( PIM%CONF%DEBUG_LEVEL .GE. 3 ) THEN
                   WRITE ( 6, 222 ) J9, PH_ACC_VAL
 222               FORMAT ( 'Coarse fringe fitting, iteration ', I2, &
     &                      ' Phase accel: ', 1PD12.5 )
              END IF
              PH_ACC_PREV = PH_ACC_VAL
         END IF
!
         IF ( FL_ITER_COARSE                         .OR. &
     &        PHS_APSO_MAX > PHS_APSO_LIM            .OR. &
     &        PIM%CONF%PHASE_ACCELERATION .NE. 0.0D0      ) THEN
!
              UV_2D_WEI = 0.0
              DO 4100 J10=1,LTIM
                 IF ( IND_RT(J10) == 0 ) GOTO 4100
                 IFRQ = 0
                 DO 4110 J11=PIM%CONF%BEG_FRQ,PIM%CONF%END_FRQ
                    IFRQ = IFRQ + 1
                    DO 4120 J12=1,LCHN
                       IF ( IND_RT(J10) .GE. 1  .AND.  IND_RT(J10) .LE. NTIM ) THEN
                            UV_2D_WEI(IND_MD(J12,J11),IND_RT(J10)) = UV(J12,IFRQ,J10)*WEI_PNT(J10)
                         ELSE
                            UV_2D_WEI(IND_MD(J12,J11),IND_RT(J10)) = 0.0
                       END IF
 4120               CONTINUE
 4110            CONTINUE
 4100         CONTINUE
         END IF
!
! ------ Perform FFT of the UV data
!
         CALL ERR_PASS ( IUER, IER )
         CALL FFT_2D_C8 ( L_MD, L_RT, -1, UV_2D_WEI, IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 7632, IUER, 'PIMA_2FFT', 'Error in an attempt '// &
     &            'to run 2D FFT transform' )
              RETURN
         END IF
!
         RAT_CEN = PIM%CONF%FRIB_RATE_WINDOW_CENTER
         IF ( PIM%CONF%FRIB_RATE_WINDOW_CENTER  .GE. RAT_MIN .AND. &
     &        PIM%CONF%FRIB_DELAY_WINDOW_CENTER .LE. RAT_MAX       ) THEN
              ITURN_RAT = 0
            ELSE
              ITURN_RAT = IDNINT(RAT_CEN/(RAT_MAX - RAT_MIN))
              RAT_CEN = RAT_CEN - ITURN_RAT*(RAT_MAX-RAT_MIN)
         END IF
         DEL_CEN = PIM%CONF%FRIB_DELAY_WINDOW_CENTER
         IF ( PIM%CONF%FRIB_DELAY_WINDOW_CENTER .GE. DEL_MIN .AND. &
     &        PIM%CONF%FRIB_DELAY_WINDOW_CENTER .LE. DEL_MAX       ) THEN
              ITURN_DEL = 0
            ELSE
              ITURN_DEL = IDNINT(DEL_CEN/(DEL_MAX - DEL_MIN))
              DEL_CEN = DEL_CEN - ITURN_DEL*(DEL_MAX-DEL_MIN)
         END IF
!
! ------ Search for maximum of the DRF
!
         MAX_AMP(1)   = -1
         MAX_AMP(2)   = -1
         AMPL_MAX     = -1.0D0
         MAX_AMP_ARR  = -1
         SQR_AMPL_MAX = -1.0D0
!
! ------ Pre-compute flag whether a given element of the grid over delay
! ------ falls into the window. This is done for acceleration of the 
! ------ unner loop
!
         N_GR_DEL = 0
         DO 4130 J13=1,L_MD
            IF ( J13 < L_MD/2 ) THEN
                 GR_DEL_GRID = MD_STEP*(J13-1)
               ELSE
                 GR_DEL_GRID = MD_STEP*(J13-L_MD-1)
            END IF
            IF ( GR_DEL_GRID < DEL_CEN - PIM%CONF%FRIB_DELAY_WINDOW_WIDTH .OR. &
     &           GR_DEL_GRID > DEL_CEN + PIM%CONF%FRIB_DELAY_WINDOW_WIDTH      ) THEN
                 FL_DEL_GRID_ARR(J13) = .FALSE.
               ELSE
                 FL_DEL_GRID_ARR(J13) = .TRUE.
                 N_GR_DEL = N_GR_DEL + 1
            END IF
 4130    CONTINUE 
         IF ( N_GR_DEL < 2 ) THEN
              CALL CLRCH ( STR )
              WRITE ( UNIT=STR(1:11), FMT='(1PD11.4)' ) PIM%CONF%FRIB_DELAY_WINDOW_WIDTH
              CALL ERR_LOG ( 7633, IUER, 'PIMA_2FFT', 'Too narrow group delay '// &
     &            'window: '//TRIM(STR)//' -- nothing to search. Please either '// &
     &            'narrow the fringe search window or increase FRIB.OVERSAMPLE_MD' )
              RETURN
         END IF
!
! ------ Determine blocking size for the parallel loop
!
         SCHED_THR = MAX ( 1, MIN ( 8*NTHR, (L_MD/4)/(2*NTHR) ) )
!
         DO 4140 J14=1,L_RT
            IF ( J14 < L_RT/2 ) THEN
                 PH_RAT_GRID = RT_STEP*(J14-1)
               ELSE
                 PH_RAT_GRID = RT_STEP*(J14-L_RT-1)
            END IF
!
! --------- Check, whether this point on the phase delay rate grid falls within
! --------- the search window
!
            IF ( PH_RAT_GRID < RAT_CEN - &
     &                         PIM%CONF%FRIB_RATE_WINDOW_WIDTH    ) GOTO 4140
            IF ( PH_RAT_GRID > RAT_CEN + &
     &                         PIM%CONF%FRIB_RATE_WINDOW_WIDTH    ) GOTO 4140
!
! --------- NB: we compare not amplitudes but squares of amplitudes, since
! --------- their computation is faster
!
!$OMP       PARALLEL DO IF    ( NTHR > 1 .AND. L_MD > 64 ), &
!$OMP&                          SCHEDULE ( STATIC, SCHED_THR )
            DO 4150 J15=0,L_MD/4-1
               SQR_AMP_ARR(4*J15+1) = REAL(UV_2D_WEI(4*J15+1,J14))**2 + IMAG(UV_2D_WEI(4*J15+1,J14))**2 
               SQR_AMP_ARR(4*J15+2) = REAL(UV_2D_WEI(4*J15+2,J14))**2 + IMAG(UV_2D_WEI(4*J15+2,J14))**2 
               SQR_AMP_ARR(4*J15+3) = REAL(UV_2D_WEI(4*J15+3,J14))**2 + IMAG(UV_2D_WEI(4*J15+3,J14))**2 
               SQR_AMP_ARR(4*J15+4) = REAL(UV_2D_WEI(4*J15+4,J14))**2 + IMAG(UV_2D_WEI(4*J15+4,J14))**2 
  4150      CONTINUE 
!$OMP       END PARALLEL DO
            DO 4160 J16=1,L_MD
               IF ( FL_DEL_GRID_ARR(J16) ) THEN
                    IF ( SQR_AMP_ARR(J16) > SQR_AMPL_MAX ) THEN
                         MAX_AMP(1) = J16
                         MAX_AMP(2) = J14
                         SQR_AMPL_MAX = SQR_AMP_ARR(J16)
                    END IF
               END IF
 4160       CONTINUE
 4140    CONTINUE
         AMPL_MAX = ABS(UV_2D_WEI(MAX_AMP(1),MAX_AMP(2)))
!
         IF ( IND_MAX_AMP(1) < 1 ) THEN
              IND_MAX_AMP(1) = MAX_AMP(1)
              IND_MAX_AMP(2) = MAX_AMP(2)
              IND_MAX_ACC    = J9
              AMPL_MAX_GLOB  = AMPL_MAX
            ELSE
              IF ( AMPL_MAX > AMPL_MAX_GLOB ) THEN
                   IND_MAX_AMP(1) = MAX_AMP(1)
                   IND_MAX_AMP(2) = MAX_AMP(2)
                   IND_MAX_ACC    = J9
                   AMPL_MAX_GLOB  = AMPL_MAX
              END IF 
         END IF 
         IF ( MAX_AMP(1) .LE. 0  .OR.  MAX_AMP(2) .LE. 0 ) THEN
              WRITE ( 6, * ) ' MAX_AMP= ', MAX_AMP, ' AMPL_MAX= ', AMPL_MAX
              CALL ERR_LOG ( 7634, IUER, 'PIMA_2FFT', 'Trap of internal '// &
     &            'control: UV data are garbage' )
              RETURN
         END IF
         IF ( PIM%CONF%DEBUG_LEVEL == 8 ) THEN
              CALL CPU_TIMER ( STR ) 
              WRITE ( 6, '(A)' ) 'PIMA_FFT TIMER: Coarse fringe fitting timing: '//STR(1:27)
              CALL CPU_TIMER ( %VAL(0) ) !
         END IF
         IF ( PIM%CONF%DEBUG_LEVEL .GE. 5 ) THEN
              WRITE ( 6, 220 ) MAX_AMP(1), MAX_AMP(2), AMPL_MAX/WPOI
 220          FORMAT ( 'PIMA_2FFT: Coarse search: Max_del: ', I8, &
     &                 ' Max_rat: ', I6, ' Ampl_Max: ', F8.6 )
         END IF
 490  CONTINUE 
      DEALLOCATE ( SQR_AMP_ARR )
      DEALLOCATE ( FL_DEL_GRID_ARR )
!
      IF ( FL_ITER_COARSE ) THEN
           PH_ACC_VAL = PH_ACC_BEG + (IND_MAX_ACC-1)*PH_ACC_STEP 
           PH_ACC_DIF = PH_ACC_VAL - PH_ACC_PREV
           IF ( PIM%CONF%DEBUG_LEVEL .GE. 5 ) THEN
                WRITE ( 6, 225 ) MAX_AMP(1), MAX_AMP(2), AMPL_MAX/WPOI, &
     &                           PH_ACC_VAL
 225            FORMAT ( 'PIMA_2FFT: Coarse search: Max_del: ', I8, &
     &                   ' Max_rat: ', I8, ' Ampl_Max: ', F8.6, &
     &                   ' Best ph_acc: ', 1PD12.4 )
           END IF
!
           CALL ERR_PASS ( IUER, IER )
           CALL PIMA_APPLY_ACCEL ( PIM, LCHN, LFRQ, LTIM, TIME_FRT, &
     &                             TIM_ARR, WEI_PNT, PH_ACC_DIF, UV, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 7635, IUER, 'PIMA_2FFT', 'Failure to perform '// &
     &              'correction for phase acceleration term in fringe phase' )
                RETURN
           END IF
           PH_ACC_PREV = PH_ACC_VAL
           UV_2D_WEI = 0.0
           DO 4170 J17=1,LTIM
              IF ( IND_RT(J17) == 0 ) GOTO 4170
              IFRQ = 0
              DO 4180 J18=PIM%CONF%BEG_FRQ,PIM%CONF%END_FRQ
                 IFRQ = IFRQ + 1
                 DO 4190 J19=1,LCHN
                     IF ( IND_RT(J17) .GE. 1  .AND.  IND_RT(J17) .LE. NTIM ) THEN
                          UV_2D_WEI(IND_MD(J19,J18),IND_RT(J17)) = UV(J19,IFRQ,J17)*WEI_PNT(J17)
                       ELSE
                          UV_2D_WEI(IND_MD(J19,J18),IND_RT(J17)) = 0.0
                     END IF
 4190             CONTINUE
 4180         CONTINUE
 4170      CONTINUE
!
           CALL ERR_PASS ( IUER, IER )
           CALL FFT_2D_C8 ( L_MD, L_RT, -1, UV_2D_WEI, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 7636, IUER, 'PIMA_2FFT', 'Error in an attempt '// &
     &              'to run 2D FFT transform' )
                RETURN
           END IF
           IF ( PIM%CONF%DEBUG_LEVEL == 8 ) THEN
                CALL CPU_TIMER ( STR ) 
                WRITE ( 6, '(A)' ) 'PIMA_FFT TIMER: 2D_C8 fft timing: '//STR(1:27)
                CALL CPU_TIMER ( %VAL(0) ) !
           END IF
           PH_ACC_COA = PH_ACC_VAL
        ELSE 
           PH_ACC_COA = PIM%CONF%PHASE_ACCELERATION
      END IF
!
! --- Find group delay, phase delay rate and the amplitude which corresponds
! --- to the maximum of the delay resolution function
!
      IF ( IND_MAX_AMP(1) < L_MD/2 ) THEN
           GR_DEL(PIMA__DRF) =   MD_STEP*(IND_MAX_AMP(1)-1) &
     &                         + ITURN_DEL*(DEL_MAX - DEL_MIN)
         ELSE
           GR_DEL(PIMA__DRF) =   MD_STEP*(IND_MAX_AMP(1)-L_MD-1) &
     &                         + ITURN_DEL*(DEL_MAX - DEL_MIN)
      END IF
      IF ( IND_MAX_AMP(2) < L_RT/2 ) THEN
           PH_RAT(PIMA__DRF) =   RT_STEP*(IND_MAX_AMP(2)-1) &
     &                         + ITURN_RAT*(RAT_MAX - RAT_MIN)
         ELSE
           PH_RAT(PIMA__DRF) =   RT_STEP*(IND_MAX_AMP(2)-L_RT-1) &
     &                         + ITURN_RAT*(RAT_MAX - RAT_MIN)
      END IF
!
! --- Resolve group delay folding if the a priori group delay update
! --- due to the position offset exceeds 1/2 of the window width
!
      IF ( GR_DEL(PIMA__DRF) - PH_DEL_APSO > 0.5D0/PIM%FRQ(PIM%CONF%BEG_FRQ,FRG_IND)%CHAN_WIDTH ) THEN
!
! -------- The differemce between residual group delay and the Delta group
! -------- delay due to poor apriori position is too large -- resolve folding
!
           GR_DEL(PIMA__DRF) = GR_DEL(PIMA__DRF) - 1.D0/PIM%FRQ(PIM%CONF%BEG_FRQ,FRG_IND)%CHAN_WIDTH
         ELSE IF ( GR_DEL(PIMA__DRF) - PH_DEL_APSO < -0.5D0/PIM%FRQ(PIM%CONF%BEG_FRQ,FRG_IND)%CHAN_WIDTH ) THEN
!
! -------- The differemce betweemn residual group delay and the Delta group
! -------- delay due to poor apriori position is too large with negative sign -- resolve folding
!
           GR_DEL(PIMA__DRF) = GR_DEL(PIMA__DRF) + 1.D0/PIM%FRQ(PIM%CONF%BEG_FRQ,FRG_IND)%CHAN_WIDTH
      END IF
!
! --- Resolve phase delay rate folding if the a priori phase delay
! --- due to the position offset exceeds 1/2 of the window width
!
      IF ( PH_RAT(PIMA__DRF) - PH_RAT_APSO > 0.5D0/(PIM%OBS(IND_OBS)%AP_LEN*PIM%FRQ(PIM%CONF%BEG_FRQ,FRG_IND)%FREQ) ) THEN
           PH_RAT(PIMA__DRF) = PH_RAT(PIMA__DRF) - 1.0D0/(PIM%OBS(IND_OBS)%AP_LEN*PIM%FRQ(PIM%CONF%BEG_FRQ,FRG_IND)%FREQ) 
         ELSE IF ( PH_RAT(PIMA__DRF) - PH_RAT_APSO < -0.5D0/(PIM%OBS(IND_OBS)%AP_LEN*PIM%FRQ(PIM%CONF%BEG_FRQ,FRG_IND)%FREQ) ) THEN
           PH_RAT(PIMA__DRF) = PH_RAT(PIMA__DRF) + 1.0D0/(PIM%OBS(IND_OBS)%AP_LEN*PIM%FRQ(PIM%CONF%BEG_FRQ,FRG_IND)%FREQ) 
      END IF
!
      GR_DEL_COARSE = GR_DEL(PIMA__DRF)
      PH_RAT_COARSE = PH_RAT(PIMA__DRF)
!
      AMPL_MAX = ABS(UV_2D_WEI(IND_MAX_AMP(1),IND_MAX_AMP(2)))/WPOI
      PHAS_MAX = PHAS_CMPL_R4 ( UV_2D_WEI(IND_MAX_AMP(1),IND_MAX_AMP(2)) )
!
! --- Compute the rms of the noise level. For that we pick up
! --- the values of the DRF randomly
!
      IS = 189349 + PIM%MJD_0 + 10*PIM%TAI_0
      IN = MIN ( INT8(M_NOI), INT8(L_MD)*INT8(L_RT*M__SHR_NOI) )
!
! --- Build array of randomly taken amplitudes of DRF and compute its
! --- average value and rms
!
      NOISE_RMS = 0.0D0
      NOISE_AVR = 0.0D0
      DO 4200 J20=1,IN
         IND_NOI = 1 + RANDOM_NUMB ( IS, 1.D0, MIN ( (1.D0*L_MD)*(1.D0*L_RT), 2.0D0**31) - 2.0D0 )
#ifdef ADR_32BIT
         CALL LIB$MOVC3 ( 8, %VAL( LOC(UV_2D_WEI) + 8*(IND_NOI-1) ), UV_VAL )
#else
         ADR = LOC(UV_2D_WEI) + INT8(8)*INT8(IND_NOI-1)
         CALL LIB$MOVC3 ( 8, %VAL(ADR), UV_VAL )
#endif
         NOI_ARR(J20) = ABS(UV_VAL)/WPOI
         NOISE_RMS = NOISE_RMS + NOI_ARR(J20)**2
         NOISE_AVR = NOISE_AVR + NOI_ARR(J20)
 4200 CONTINUE
      NOISE_RMS = MAX ( NOI_FLOOR, SQRT ( NOISE_RMS/IN ) )
      NOISE_AVR = MAX ( NOI_FLOOR, NOISE_AVR/IN )
!
! --- Now sort this array of amplitudes in descending order
!
      CALL FOR_QSORT ( NOI_ARR, IN, SIZEOF(NOI_ARR(1)), PIMA_COMPAR_NOISE )
!
! --- Perform an iterative procedure: remove the point in the DRF array which
! --- is greater than N-sigmas. First we consecutively discard 50% of
! --- points in the order of decreasing their amplitudes and synchronously
! --- update the rms
!
      L_NOI = IN
      DO 4210 J21=1,L_NOI/2
         NOISE_RMS = DSQRT ( (NOISE_RMS**2*IN - NOI_ARR(J21)**2)/(IN-1) )
         NOISE_AVR = (NOISE_AVR*IN - NOI_ARR(J21))/(IN-1)
         IN = IN - 1
 4210 CONTINUE
!
! --- Second, we restore the points in ascension of their amplitudes till
! --- we reach the amplitudes which is N-sigmas greater than the average
!
      DO 4220 J22=L_NOI/2,1,-1
         IF ( NOI_ARR(J22) < PIM%CONF%FRIB_NOISE_NSIGMA*NOISE_AVR ) THEN
              NOISE_RMS = SQRT ( (NOISE_RMS**2*IN + NOI_ARR(J22)**2)/(IN+1) )
              NOISE_AVR = (NOISE_AVR*IN + NOI_ARR(J22))/(IN+1)
              IN = IN + 1
            ELSE
              GOTO 8220
         END IF
 4220 CONTINUE
 8220 CONTINUE
      IF ( PIM%CONF%DEBUG_LEVEL == 8 ) THEN
           CALL CPU_TIMER ( STR )
           WRITE ( 6, '(A)' ) 'PIMA_2FFT Coarse fringe search timing:  '//STR(1:27)
           CALL CPU_TIMER ( %VAL(0) )
      END IF
      IF ( NOISE_AVR < 2.0D0*NOI_FLOOR ) THEN
           SNR = 0.0D0
           AMPL     = 0.0D0
           GR_DEL   = 0.0D0
           PH_RAT   = 0.0D0
           PHAS     = 0.0D0
           TIME_FRT = 0.0D0
!
           WRITE  ( 6, 230 ) IND_OBS
 230       FORMAT ( 'PIMA_2FFT: all uv-points are zero. IND_OBS: ', I6 )
!
           DEALLOCATE ( FRQ_WEI   )
           DEALLOCATE ( IND_MD    )
           DEALLOCATE ( IND_RT    )
           DEALLOCATE ( UV_2D_WEI )
           DEALLOCATE ( TIM_ARR   )
!
           CALL ERR_LOG ( 0, IUER )
           RETURN
      END IF
!
      CALL GETENVAR ( 'PIMA_GWIN_TEST_FILE', STR )
      IF ( ILEN(STR) > 0 ) THEN
           J3 = 72
           OPEN ( FILE=STR, UNIT=J3, STATUS='UNKNOWN' )
           DO 510 J1=1,L_RT
              IF ( IABS(J1 - IND_MAX_AMP(2)) < 120 ) GOTO 510
              DO 520 J2=1,L_MD
                 IF ( IABS(J2 - IND_MAX_AMP(1)) < 12 ) GOTO 520
                 WRITE ( UNIT=J3, FMT='(1PE14.7)' ) ABS(UV_2D_WEI(J2,J1))/WPOI
 520          CONTINUE 
 510       CONTINUE 
           CLOSE ( UNIT=J3 )
      END IF
!
! --- Fine search for multiband delay and delay rate
!
      PHAS(PIMA__DRF) = PHAS_MAX
      AMPL = AMPL_MAX
      CALL ERR_PASS ( IUER, IER )
      CALL PIMA_FINE_SEARCH ( PIM, L_MD, L_RT, UV_2D_WEI, WPOI,              &
     &                        PIM%OBS(IND_OBS)%AP_LEN, K_EPC,                &
     &                        IND_MAX_AMP(1), IND_MAX_AMP(2),                &
     &                        MD_STEP, RT_STEP, LTIM, LFRQ, UV, WEI_1D,      &
     &                        NOISE_AVR, TIME_FRT, FREQ_ARR, FREQ_REF,       &
     &                        TIM_ARR, GRAMBSP, IND_OBS,                     &
     &                        GR_DEL, PH_RAT, GR_RAT, PH_ACC, PHAS, AMPL,    &
     &                        PHAS_ERR, GR_DEL_ERR,  PH_DEL_ERR,             &
     &                        PH_RAT_ERR, GR_RAT_ERR, PH_ACC_ERR, COV_PR_PH, &
     &                        COV_GR_MD, EFF_DURA, DECOR_TIM, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 7638, IUER, 'PIMA_2FFT', 'Error in an attempt '// &
     &         'to perform a fine fringe search' )
           RETURN
      END IF
      IF ( PIM%CONF%DEBUG_LEVEL == 8 ) THEN
           CALL CPU_TIMER ( STR )
           WRITE ( 6, '(A)' ) 'PIMA_2FFT Fine fringe search timing:  '//STR(1:27)
           CALL CPU_TIMER ( %VAL(0) )
      END IF
      IF ( PIM%CONF%FRIB_FINE_SEARCH == PIMA__FINE_SEARCH_LSQ .OR. &
     &     PIM%CONF%FRIB_FINE_SEARCH == PIMA__FINE_SEARCH_ACC      ) THEN
           IND_FRA = PIMA__LSQ
         ELSE
           IND_FRA = PIMA__DRF
      END IF
!
      IF ( PIM%CONF%FRIB_FINE_SEARCH == PIMA__FINE_SEARCH_ACC ) THEN
!
! -------- Remove contribution of coarse phase acceleration that was applied to UV data
! -------- during coarse fringe fitting
!
           CALL ERR_PASS ( IUER, IER )
           CALL PIMA_APPLY_ACCEL ( PIM, LCHN, LFRQ, LTIM, TIME_FRT, &
     &                             TIM_ARR, WEI_PNT, -PH_ACC_COA, UV, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 7639, IUER, 'PIMA_2FFT', 'Failre to perform '// &
     &              'correction for phase acceleration term in fringe phase' )
                RETURN
           END IF
!
! -------- Store the total phase acceleration
!
           IF ( FL_ITER_COARSE ) THEN
                PH_ACC = PH_ACC + PH_ACC_VAL 
              ELSE 
                PH_ACC = PH_ACC + PIM%CONF%PHASE_ACCELERATION
           END IF
         ELSE 
           PH_ACC = PIM%CONF%PHASE_ACCELERATION
      END IF
!@      IF ( PHS_APSO_MAX > PHS_APSO_LIM ) THEN
!@           GR_RAT = GR_RAT + PH_RAT_APSO
!@      END IF 
!
      CALL GETENVAR ( 'PIMAVAR_PLOT_DEL', STR )
      IF ( STR(1:3) == 'YES' ) THEN
           IER  = -1
           CALL PIMA_PLOT_DEL ( L_MD, L_RT, UV_2D_WEI, IER )
      END IF
!
      IF ( PIM%CONF%DEBUG_LEVEL .GE. 4 ) THEN
           WRITE ( 6, 240 ) 'Coarse search: AMPL: ', AMPL_MAX, &
     &                      ' PHAS: ', PHAS_MAX, &
     &                      ' GR_DEL = ', GR_DEL_COARSE, &
     &                      ' PH_RAT = ', PH_RAT_COARSE
           IF ( PIM%CONF%FRIB_FINE_SEARCH == PIMA__FINE_SEARCH_ACC ) THEN
                WRITE ( 6, 240 ) 'Fine search:   AMPL: ', AMPL(IND_FRA), &
     &                           ' PHAS: ', PHAS(IND_FRA), &
     &                           ' GR_DEL = ', GR_DEL(IND_FRA), &
     &                           ' PH_RAT = ', PH_RAT(IND_FRA), &
     &                           ' PH_ACC = ', PH_ACC
              ELSE IF ( PIM%CONF%FRIB_FINE_SEARCH == PIMA__FINE_SEARCH_LSQ ) THEN
                WRITE ( 6, 240 ) 'Fine search:   AMPL: ', AMPL(IND_FRA), &
     &                           ' PHAS: ', PHAS(IND_FRA), &
     &                           ' GR_DEL = ', GR_DEL(IND_FRA), &
     &                           ' PH_RAT = ', PH_RAT(IND_FRA), &
     &                           ' GR_RAT = ', GR_RAT
           END IF
           WRITE ( 6, 250 ) FREQ_REF
 240       FORMAT ( A, F9.7, A, F8.5, A, 1PD20.12, A, 1PD18.10, &
     &              A, 1PD18.10 )
 250       FORMAT ( 'Fringe reference frequency: ', 1PD20.12 )
           DP = PIM%OBS(IND_OBS)%FEED_ANG(1) - PIM%OBS(IND_OBS)%FEED_ANG(2)
           IF ( DP < -PI__NUM ) DP = DP + PI2
           IF ( DP >  PI__NUM ) DP = DP - PI2
           WRITE ( 6, 260 ) PIM%C_STA(PIM%OBS(IND_OBS)%STA_IND(1)), &
     &                      PIM%C_STA(PIM%OBS(IND_OBS)%STA_IND(2)),  &
     &                      PIM%OBS(IND_OBS)%FEED_ANG(1), &
     &                      PIM%OBS(IND_OBS)%FEED_ANG(2), DP
 260       FORMAT ( 'Baseline ', A, ' / ', A , ' Paral_ang: ', F6.3, 1X, F6.3, 2X, &
     &              'P1-P2: ', F6.3, ' rad' ) 
      END IF
!
      IF ( PIM%CONF%DEBUG_LEVEL .GE. 5 ) THEN
           WRITE ( 6, * ) 'PIMA_2FFT NOISE_REJECT: IN = ', M_NOI - IN, &
     &                    ' Used: ', IN
           WRITE ( 6, * ) 'PIMA_2FFT NOISE_AVR = ', NOISE_AVR, &
     &                    ' WPOI/NPOI= ', WPOI/MAX(NPOI,1)
           WRITE ( 6, * ) 'PIMA_2FFT NPOI= ', NPOI, ' WPOI= ' , WPOI
      END IF
!
! --- Finally, compute the SNR:
!
      SNR  = AMPL(IND_FRA)/NOISE_AVR
!
! --- Compute effective ionospheric frequencies
!
      SUM_WEI = 0.0
      SUM_FR1 = 0.0
      SUM_FR2 = 0.0
      SUM_FQ1 = 0.0
      SUM_FQ2 = 0.0
      SUM_FRI = 0.0
      SUM_DFI = 0.0
      IFRQ = 0
      DO 4260 J26=PIM%CONF%BEG_FRQ,PIM%CONF%END_FRQ
         IFRQ = IFRQ + 1
         DO 4270 J27=1,LCHN
            DRF_CHN = CMPLX ( 0.0, 0.0 )
            DO 4280 J28=1,LTIM
               IF ( WEI_1D(J28) < WEI_THR ) GOTO 4280
               PHAS_ADD_R4 = PH_RAT(IND_FRA)*PI2*FREQ_REF*((J28-1)*AP_LEN - TIME_FRT) + &
     &                       GR_DEL(IND_FRA)*PI2*(FREQ_ARR(J27,IFRQ)-FREQ_REF) + &
     &                       GR_RAT*PI2*(FREQ_ARR(J27,IFRQ)-FREQ_REF)* &
     &                                  ((J28-1)*AP_LEN - TIME_FRT)
               DRF_CHN = DRF_CHN + UV(J27,IFRQ,J28)*WEI_1D(J28)* &
     &                             CMPLX ( COS(PHAS_ADD_R4), SIN(PHAS_ADD_R4) )
 4280       CONTINUE
!
! --------- Square of weights is proportional to the number of
! --------- processed samples
!
            WEI_SQ = ABS(DRF_CHN)
            DFR = FREQ_ARR(J27,IFRQ) - FREQ_REF
            SUM_WEI = SUM_WEI + WEI_SQ
            SUM_FR1 = SUM_FR1 + WEI_SQ*DFR
            SUM_FR2 = SUM_FR2 + WEI_SQ*DFR**2
            SUM_FQ1 = SUM_FQ1 + WEI_SQ*FREQ_ARR(J27,IFRQ)
            SUM_FQ2 = SUM_FQ2 + WEI_SQ*FREQ_ARR(J27,IFRQ)**2
            SUM_FRI = SUM_FRI + WEI_SQ/FREQ_ARR(J27,IFRQ)
            SUM_DFI = SUM_DFI + WEI_SQ*DFR/FREQ_ARR(J27,IFRQ)
 4270    CONTINUE
 4260 CONTINUE
      EFF_FRQ_PHS = DSQRT ( FREQ_REF*(SUM_WEI*SUM_FR2 - SUM_FR1**2)/ &
     &                               (SUM_FRI*SUM_FR2 - SUM_DFI*SUM_FR1 ) )
      IF ( DABS((SUM_FRI*SUM_FR1 - SUM_DFI*SUM_WEI)) < 1.D-30 ) THEN
           EFF_FRQ_GRP = FREQ_REF
         ELSE
           EFF_FRQ_GRP = DSQRT ( (SUM_WEI*SUM_FR2 - SUM_FR1**2)/ &
     &                           (SUM_FRI*SUM_FR1 - SUM_DFI*SUM_WEI) )
      END IF
      EFF_FRQ_RAT = DSQRT ( SUM_FQ2/SUM_WEI )
!!   write ( 6, * ) 'EFF_FRQ_GRP= ', EFF_FRQ_GRP ; call exit ( 1 ) ! %%%%
!
! --- Compute formal errors of group delay, delay rate, phase, as well
! --- as the group delay ambiguity spacing.
!
      IF ( PIM%CONF%DEBUG_LEVEL == 8 ) THEN
           CALL CPU_TIMER ( STR )
           WRITE ( 6, '(A)' ) 'PIMA_2FFT Noise computation timing:  '//STR(1:27)
           CALL CPU_TIMER ( %VAL(0) )
      END IF
      IF ( PIM%CONF%FRIB_2D_FRINGE_PLOT == PIMA__PLOT_NO ) THEN
!
! -------- No plots? Nothing to do.
!
           IF ( ALLOCATED ( WEI_PNT   ) ) DEALLOCATE ( WEI_PNT   )
           IF ( ALLOCATED ( FRQ_WEI   ) ) DEALLOCATE ( FRQ_WEI   )
           IF ( ALLOCATED ( IND_RT    ) ) DEALLOCATE ( IND_RT    )
           IF ( ALLOCATED ( IND_MD    ) ) DEALLOCATE ( IND_MD    )
           IF ( ALLOCATED ( UV_2D_WEI ) ) DEALLOCATE ( UV_2D_WEI )
           IF ( ALLOCATED ( TIM_ARR   ) ) DEALLOCATE ( TIM_ARR   )
!
           CALL ERR_LOG ( 0, IUER )
           RETURN
      END IF
!
! --- Prepare fringe plot.
! --- first evaluate the plotting area
!
      RT3_WIN  = PIM%CONF%FRIB_PLOT_RATE_WINDOW_WIDTH/2.0
      SD3_WIN  = PIM%CONF%FRIB_PLOT_DELAY_WINDOW_WIDTH/2.0
      SD3_INIT = GR_DEL(IND_FRA)
      RT3_INIT = PH_RAT(IND_FRA)
!
      IPL_DEL = 2*INT( PIM%CONF%FRIB_PLOT_DELAY_WINDOW_WIDTH*(FRQ_STEP*L_MD)* &
     &               PIM%CONF%FRIB_OVERSAMPLE_PLOT_MD ) + 3
      IPL_RAT = 2*INT( PIM%CONF%FRIB_PLOT_RATE_WINDOW_WIDTH*(FRQ_BEG*AP_LEN*L_RT)* &
     &                 PIM%CONF%FRIB_OVERSAMPLE_PLOT_RT ) + 3
!
! --- Allocate memory for the fringe plot
!
      ALLOCATE ( AMPL_PLO(IPL_DEL,IPL_RAT), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH  ( STR )
           CALL IINCH8 ( INT8(4)*INT8(IPL_DEL)*INT8(IPL_RAT), STR )
           WRITE ( 6, * ) ' IPL_DEL = ', IPL_DEL, ' IPL_RAT = ', IPL_RAT
           CALL ERR_LOG ( 7641, IUER, 'PIMA_2FFT', 'Failure '// &
     &                    'to allocate '//STR(1:I_LEN(STR))//' bytes '// &
     &                    'of dynamic memory or plotting' )
           RETURN
      END IF
      CALL NOUT_R4 ( IPL_DEL*IPL_RAT, AMPL_PLO )
!
! --- Compute the 2D array of fringe amplitude for plotting. This is done
! --- by interpolating the previusly computed DRF
!
! --- Compute column of fringe plot by interpolating the amplitude of the DRF
!
      DO 4290 J29=1,IPL_RAT  ! cicle over delay rate
         IF ( MOD(J29-1,PIM%CONF%FRIB_OVERSAMPLE_PLOT_RT) .NE. 0 ) GOTO 4290
         IND_1 = IND_MAX_AMP(2) - &
     &           (IPL_RAT-1)/PIM%CONF%FRIB_OVERSAMPLE_PLOT_RT/2 + &
     &           (J29-1)/PIM%CONF%FRIB_OVERSAMPLE_PLOT_RT - 1
         IND_1_ORIG = IND_1
         IF ( IND_1 < 1    ) IND_1 = IND_1 + L_RT
         IF ( IND_1 > L_RT ) IND_1 = IND_1 - L_RT
         IF ( IND_1 < 1 .OR. IND_1 > L_RT ) THEN
              WRITE ( 6, * ) ' FRQ_BEG = ', FRQ_BEG
              WRITE ( 6, * ) ' IND_MAX_AMP(2) = ', IND_MAX_AMP(2)
              WRITE ( 6, * ) ' PIM%CONF%FRIB_OVERSAMPLE_PLOT_RT = ', PIM%CONF%FRIB_OVERSAMPLE_PLOT_RT
              WRITE ( 6, * ) ' J29     = ', J29
              WRITE ( 6, * ) ' IPL_RAT = ', IPL_RAT
              WRITE ( 6, * ) ' L_RT    = ', L_RT
              WRITE ( 6, * ) ' IND_1   = ', IND_1, ' IND_1_ORIG = ', IND_1_ORIG
              WRITE ( 6, * ) ' IND_10  = ', IND_MAX_AMP(2) - &
     &           (IPL_RAT-1)/PIM%CONF%FRIB_OVERSAMPLE_PLOT_RT/2 + &
     &           (J29-1)/PIM%CONF%FRIB_OVERSAMPLE_PLOT_RT - 1
              WRITE ( 6, * ) ' IND_11  = ', (IPL_RAT-1)/PIM%CONF%FRIB_OVERSAMPLE_PLOT_RT/2
              WRITE ( 6, * ) ' IND_12  = ', (J29-1)/PIM%CONF%FRIB_OVERSAMPLE_PLOT_RT
              CALL ERR_LOG ( 7642, IUER, 'PIMA_2FFT', 'Trap of '// &
     &            'internal control: wrong index IND_1 during computing '// &
     &            'the 2D fringe plot. This may happen if the plotting '// &
     &            'window is greqter than the fringe search window' )
              CALL EXIT ( 1 )
              RETURN
         END IF
         KP_DEL = 0
         DO 4300 J30=1,IPL_DEL ! Cycle over multi-band delay
            IND_2 = IND_MAX_AMP(1) - &
     &              (IPL_DEL-1)/PIM%CONF%FRIB_OVERSAMPLE_PLOT_MD/2  + &
     &              (J30-1)/PIM%CONF%FRIB_OVERSAMPLE_PLOT_MD - 1
            IF ( IND_2 < 1    ) IND_2 = IND_2 + L_MD
            IF ( IND_2 > L_MD ) IND_2 = IND_2 - L_MD
            IF ( IND_2 < 1 .OR. IND_2 > L_MD ) THEN
                 CALL ERR_LOG ( 7643, IUER, 'PIMA_2FFT', 'Trap of '// &
     &               'internal control: wrong index IND_2 during computing '// &
     &               'the 2D fringe plot' )
                 RETURN
            END IF
!
            IF ( MOD(J30-1,PIM%CONF%FRIB_OVERSAMPLE_PLOT_MD) == 0 ) THEN
!
! -------------- Build the column of the amplitude array by gathering the
! -------------- amplitude of the DRF in the knots where the previosly
! -------------- computed DRF coincides with the array of amplitudes for plot
!
                 KP_DEL = KP_DEL + 1
                 DEL_ARG(KP_DEL) = J30
                 DEL_VAL(KP_DEL) = ABS(UV_2D_WEI(IND_2,IND_1))/WPOI
            END IF
 4300    CONTINUE
!
! ------ Compute interpolation spline
!
         IF ( KP_DEL > 3 ) THEN
              CALL ERR_PASS ( IUER, IER )
              CALL MAKE_SPLINE ( 3, KP_DEL, DEL_ARG, DEL_VAL, 0.0D0, 0.0D0, &
     &                           DEL_SPL, WORK_ARR, IER )
              IF ( IER .NE. 0 ) THEN
                   WRITE ( 6, * ) ' IPL_RAT=',IPL_RAT, ' IPL_DEL =', IPL_DEL
                   WRITE ( 6, * ) ' PIM%CONF%FRIB_PLOT_DELAY_WINDOW_WIDTH = ', PIM%CONF%FRIB_PLOT_DELAY_WINDOW_WIDTH
                   WRITE ( 6, * ) ' KP_DEL = ', KP_DEL
                   CALL ERR_LOG ( 7644, IUER, 'PIMA_2FFT', 'Error in an attempt '// &
     &                 'to compute interpolating spline. Check '// &
     &                 'parameter FRIB_PLOT_DELAY_WINDOW_WIDTH'  )
                   RETURN
              END IF
!
              DO 4310 J31=1,IPL_DEL
!
! -------------- ... and fill the column of the plotting array with values of
! -------------- the spline
!
                 KNOT_DEL = (J31-1)/PIM%CONF%FRIB_OVERSAMPLE_PLOT_MD + 1
                 AMPL_PLO(J31,J29) = FSPL8 ( 1.0D0*J31, KP_DEL, DEL_ARG, DEL_VAL, &
     &                                        KNOT_DEL, DEL_SPL )
 4310         CONTINUE
         END IF
 4290 CONTINUE
!
! --- Now computing rows of the plotting array
!
      DO 4320 J32=1,IPL_DEL
         IND_2 = IND_MAX_AMP(1) - &
     &           (IPL_DEL-1)/PIM%CONF%FRIB_OVERSAMPLE_PLOT_MD/2  + &
     &           (J32-1)/PIM%CONF%FRIB_OVERSAMPLE_PLOT_MD - 1
         IF ( IND_2 < 1    ) IND_2 = IND_2 + L_MD
         IF ( IND_2 > L_MD ) IND_2 = IND_2 - L_MD
         KP_RAT = 0
         DO 4330 J33=1,IPL_RAT
            IND_1 = IND_MAX_AMP(2) - &
     &              (IPL_RAT-1)/PIM%CONF%FRIB_OVERSAMPLE_PLOT_RT/2 + &
     &              (J33-1)/PIM%CONF%FRIB_OVERSAMPLE_PLOT_RT - 1
            IF ( IND_1 < 1    ) IND_1 = IND_1 + L_RT
            IF ( IND_1 > L_RT ) IND_1 = IND_1 - L_RT
!
            IF ( MOD(J33-1,PIM%CONF%FRIB_OVERSAMPLE_PLOT_RT) == 0 ) THEN
!
! -------------- Gather array of DRF for interpolating
!
                 KP_RAT = KP_RAT + 1
                 RAT_ARG(KP_RAT) = J33
                 RAT_VAL(KP_RAT) = AMPL_PLO(J32,J33)
            END IF
 4330    CONTINUE
!
! ------ Compute interpolating spline
!
         IF ( KP_RAT > 3 ) THEN
              CALL ERR_PASS ( IUER, IER )
              CALL MAKE_SPLINE ( 3, KP_RAT, RAT_ARG, RAT_VAL, 0.0D0, 0.0D0, &
     &                           RAT_SPL, WORK_ARR, IER )
              IF ( IER .NE. 0 ) THEN
                   WRITE ( 6, * ) ' IPL_RAT=',IPL_RAT, ' IPL_DEL =', IPL_DEL
                   WRITE ( 6, * ) ' KP_RAT = ', KP_RAT
                   WRITE ( 6, * ) ' PIM%CONF%FRIB_PLOT_RATE_WINDOW_WIDTH = ', PIM%CONF%FRIB_PLOT_RATE_WINDOW_WIDTH
                   CALL ERR_LOG ( 7645, IUER, 'PIMA_2FFT', 'Error in an attempt '// &
     &                 'to compute interpolating spline. Check '// &
     &                 'parameter FRIB_PLOT_RATE_WINDOW_WIDTH'  )
                   RETURN
              END IF
              DO 4340 J34=1,IPL_RAT
!
! --------------- ... and build raws of the plotting array by interpolating
!
                 KNOT_RAT = (J34-1)/PIM%CONF%FRIB_OVERSAMPLE_PLOT_RT + 1
                 AMPL_PLO(J32,J34) = FSPL8 ( 1.0D0*J34, KP_RAT, RAT_ARG, RAT_VAL, &
     &                                       KNOT_RAT, RAT_SPL )
 4340         CONTINUE
         END IF
 4320 CONTINUE
!
      IF ( FL_RESCALE ) THEN
!
! -------- Special trick for preparing fringe plots for publication.
! -------- We need to delute it, othersie lines will be too dense
!
           ALLOCATE ( AMPL_PLO_RSC((IPL_DEL-1)/2+1,(IPL_RAT-1)/2+1), STAT=IER )
           DO 4350 J35=1,(IPL_DEL-1)/2+1
              DO 4360 J36=1,(IPL_RAT-1)/2+1
                 AMPL_PLO_RSC(J36,J35) = AMPL_PLO(J36*2-1,J35*2-1)
 4360         CONTINUE
 4350      CONTINUE
           IPL_DEL = (IPL_DEL-1)/2+1
           IPL_RAT = (IPL_RAT-1)/2+1
      END IF
!
      IF ( PIM%CONF%FRIB_2D_FRINGE_PLOT == PIMA__PLOT_XW ) THEN
           IDEV = 1
        ELSE IF ( PIM%CONF%FRIB_2D_FRINGE_PLOT == PIMA__PLOT_GIF ) THEN
           IDEV = 2
        ELSE IF ( PIM%CONF%FRIB_2D_FRINGE_PLOT == PIMA__PLOT_PS  ) THEN
           IDEV = 3
        ELSE IF ( PIM%CONF%FRIB_2D_FRINGE_PLOT == PIMA__PLOT_TXT ) THEN
           IDEV = -1
        ELSE
           IDEV = 0
      END IF
!
      IF ( IDEV .NE. 0 ) THEN
!
! -------- Make the 2D fringe plot
!
           CALL ERR_PASS ( IUER, IER )
           IF ( FL_RESCALE ) THEN
                CALL PIMA_FR2D_PLOT ( FINAM_2D_PLOT, PIM, IND_OBS, &
     &                                1, IPL_DEL, 1, IPL_RAT, &
     &                                AMPL_PLO_RSC, AP_LEN, &
     &                                SD3_INIT, SD3_WIN, RT3_INIT, RT3_WIN, &
     &                                AMPL, SNR, GR_DEL(IND_FRA), &
     &                                PH_RAT(IND_FRA), IDEV, IER )
                DEALLOCATE ( FRQ_WEI )
                DEALLOCATE ( IND_MD  )
                DEALLOCATE ( IND_RT  )
                DEALLOCATE ( AMPL_PLO  )
                DEALLOCATE ( AMPL_PLO_RSC )
              ELSE
                CALL PIMA_FR2D_PLOT ( FINAM_2D_PLOT, PIM, IND_OBS, &
     &                                1, IPL_DEL, 1, IPL_RAT, &
     &                                AMPL_PLO, AP_LEN, &
     &                                SD3_INIT, SD3_WIN, RT3_INIT, RT3_WIN, &
     &                                AMPL, SNR, GR_DEL(IND_FRA), &
     &                                PH_RAT(IND_FRA), IDEV, IER )
           END IF
           IF ( IER .NE. 0 ) THEN
                CALL CLRCH ( STR )
                CALL INCH  ( IND_OBS, STR )
                CALL ERR_LOG ( 7646, IUER, 'PIMA_2FFT', 'Error in an '// &
     &                         'making 2D fringe plot for observation '//STR )
                RETURN
           END IF
      END IF
!
      DEALLOCATE ( AMPL_PLO  )
      IF ( FL_RESCALE ) THEN
           DEALLOCATE ( AMPL_PLO_RSC )
      END IF
      IF ( ALLOCATED ( WEI_PNT   ) ) DEALLOCATE ( WEI_PNT   )
      IF ( ALLOCATED ( FRQ_WEI   ) ) DEALLOCATE ( FRQ_WEI   )
      IF ( ALLOCATED ( IND_RT    ) ) DEALLOCATE ( IND_RT    )
      IF ( ALLOCATED ( IND_MD    ) ) DEALLOCATE ( IND_MD    )
      IF ( ALLOCATED ( UV_2D_WEI ) ) DEALLOCATE ( UV_2D_WEI )
      IF ( ALLOCATED ( TIM_ARR   ) ) DEALLOCATE ( TIM_ARR   )
      IF ( PIM%CONF%DEBUG_LEVEL == 8 ) THEN
           CALL CPU_TIMER ( STR )
           WRITE ( 6, '(A)' ) 'PIMA_2FFT Plot preparation timing:  '//STR(1:27)
           CALL CPU_TIMER ( %VAL(0) )
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  PIMA_2FFT  !#!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE PIMA_PLOT_DEL ( L_MD, L_RT, UV_2D_WEI, IUER )
      IMPLICIT   NONE 
      INTEGER*4  L_MD, L_RT, IUER
      COMPLEX*8  UV_2D_WEI(L_MD,L_RT)
      COMPLEX*8  ACC
      REAL*8     ARG(L_MD), VAL(L_MD), VALS(L_MD)
      INTEGER*4  J1, J2, J3, IND, IER
!
      DO 410 J1=1,L_MD
         ARG(J1) = -L_MD/2 + J1
         ACC = (0.0, 0.0)
         DO 420 J2=1,L_RT
            ACC = ACC + UV_2D_WEI(J1,J2)
 420     CONTINUE 
         IF ( J1 < L_MD/2 ) THEN
              VAL(J1+L_MD/2)   = ABS ( ACC/L_RT  )
            ELSE
              VAL(J1+1-L_MD/2) = ABS ( ACC/L_RT  )
         END IF
 410  CONTINUE 
      CALL GAUSSIAN_FILTER ( 500.0D0, L_MD, ARG, VAL, VALS )
      IER = IUER
      CALL DIAGI_2 ( L_MD, ARG, VAL, L_MD, ARG, VALS, IER )
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  PIMA_PLOT_DEL  !#!#
