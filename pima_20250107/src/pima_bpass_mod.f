      SUBROUTINE PIMA_INIT_PBP_STA ( PIM, MODE, IND_OBS, ISTA, LFRQ, IND_FRQ, &
     &                               IND_REF, IND_REM, CMPL_ARR, AC_AVR, &
     &                               DRF_SEG, FREQ_IN, WEI_IN, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine PIMA_INIT_PBP_STA computes the mathematical model for the  *
! *   complex bandpass and writes them in the output array CMPL_ARR.     *
! *                                                                      *
! * ### 02-MAR-2019 PIMA_INIT_PBP_STA v1.2 (c) L. Petrov 08-DEC-2020 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'astro_constants.i'
      INCLUDE   'pima.i'
      INCLUDE   'vtd.i'
      TYPE     ( PIMA__TYPE         ) :: PIM
      INTEGER*4  IND_OBS, MODE, ISTA, LFRQ, IND_FRQ, IND_REF, IND_REM, IUER 
      REAL*4     AC_AVR(PIM%NCHN,LFRQ,4)
      COMPLEX*8  CMPL_ARR(PIM%NCHN,PIM%NFRQ), &
     &           DRF_SEG(PIM%NCHN/PIM%CONF%BPS_MSEG_ACCUM), DRF_ALL
      CHARACTER  STR*128, STR1*128
      REAL*4     FREQ_IN(PIM__MCHN),  AMPL_RES(PIM__MCHN), PHAS_RES(PIM__MCHN), &
     &           FREQ_OUT(PIM__MCHN), AMPL_MOD(PIM__MCHN), PHAS_MOD(PIM__MCHN), &
     &           PHAS_OUT(PIM__MCHN), AMPL_OUT(PIM__MCHN), &
     &           AMPL_FRQ_NRML(PIM__MFRQ), AMPL_BAND_NRML, &
     &           WEI_IN(PIM__MCHN), PHAS_ALL
      REAL*8     T1(8192), X1(8192), T2(8192), X2(8192)
      LOGICAL*1  FL_BMASK, FL_AMBIG_RES, FL_AMB_IF, FL_PLOT
      INTEGER*1  MASK_CHN
      INTEGER*4  J1, J2, J3, J4, ITURN, MOD_AMB, ICHN, KCHN, JCHN, IND_STA(2), IER
      REAL*4,    EXTERNAL :: PHAS_CMPL_R4
      REAL*8,    EXTERNAL :: LEGENDRE_POL
      LOGICAL*4, EXTERNAL :: BPASS_MOD_POLY, BPASS_MOD_SPLINE
      INTEGER*4, EXTERNAL :: ILEN, I_LEN
!
      FL_PLOT = .FALSE.
!!  if ( ind_obs == 69 .and. ind_frq == 14 ) fl_plot = .true. ! %%%%%%%%%%%%%%%
!
      FL_AMBIG_RES = .FALSE.
      CALL GETENVAR ( 'PIMAVAR_BPAS_AMBIG_RES', STR )
      CALL TRAN ( 11, STR, STR )
      IF ( STR == 'YES' ) FL_AMBIG_RES = .TRUE.
      IF ( STR == 'NO'  ) FL_AMBIG_RES = .FALSE.
      IF ( FL_AMBIG_RES ) THEN
           MOD_AMB = 2
         ELSE
           MOD_AMB = 1
      END IF
!
      LFRQ = PIM%CONF%END_FRQ - PIM%CONF%BEG_FRQ + 1
      KCHN = PIM%NCHN/PIM%CONF%BPS_MSEG_ACCUM
      IND_STA(1) = PIM%OBS(IND_OBS)%STA_IND(1)
      IND_STA(2) = PIM%OBS(IND_OBS)%STA_IND(2)
!
      DRF_ALL = 0.0
      ICHN    = 0
      IF ( PIM%CONF%BANDPASS_MASK_FILE .NE. PIMA__BPASS_NO  .AND. &
     &     ASSOCIATED ( PIM%BANDPASS_MASK )                       ) THEN
           FL_BMASK = .TRUE.
         ELSE
           FL_BMASK = .FALSE.
      END IF
!
! --- Compute segment averaged polarization bandpass
!
      JCHN = 0
      DO 410 J1=1,KCHN ! cycle over spectral segments
         DRF_SEG(J1) = 0.0
         FREQ_IN(J1) = 0.0
         WEI_IN(J1)  = 0.0
         DO 420 J2=1,PIM%CONF%BPS_MSEG_ACCUM ! cycle over the spectral channales of the segment
            ICHN = ICHN + 1
!
! --------- Frequency array FREQ_OUT lists all original frequencies of the IND_FRQ-th IF
!
            FREQ_OUT(ICHN) = PIM%FREQ_ARR(ICHN,IND_FRQ,PIM%CONF%FRQ_GRP) 
            IF ( FL_BMASK ) THEN
                 MASK_CHN = PIM%BANDPASS_MASK(ICHN,IND_FRQ,IND_STA(1),PIMA__MASK_BPAS) * &
     &                      PIM%BANDPASS_MASK(ICHN,IND_FRQ,IND_STA(2),PIMA__MASK_BPAS)
               ELSE
                 MASK_CHN = 1
            END IF
!
! --------- Update the complex bandpass accumulator for the given segment
!
            DRF_SEG(J1) = DRF_SEG(J1) + MASK_CHN*CMPL_ARR(ICHN,IND_FRQ)
            IF ( MASK_CHN*CMPL_ARR(ICHN,IND_FRQ) .NE. CMPLX ( 0.0, 0.0 ) ) THEN
                 FREQ_IN(J1) = FREQ_IN(J1) + PIM%FREQ_ARR(ICHN,IND_FRQ,PIM%CONF%FRQ_GRP)*MASK_CHN
                 WEI_IN(J1)  = WEI_IN(J1)  + MASK_CHN
                 IF ( FL_PLOT ) THEN
                      JCHN = JCHN + 1
                      T1(JCHN) = PIM%FREQ_ARR(ICHN,IND_FRQ,PIM%CONF%FRQ_GRP)*MASK_CHN
                      X1(JCHN) = PHAS_CMPL_R4 ( MASK_CHN*CMPL_ARR(ICHN,IND_FRQ) )
                 END IF
            END IF
 420     CONTINUE
         DRF_ALL = DRF_ALL + DRF_SEG(J1)
!
! ------ Compute the segment averaged frequency, amplitude and phase
! ------ of the residuals
!
         IF ( WEI_IN(J1) > 0.0 ) THEN
              FREQ_IN(J1)  = FREQ_IN(J1)/WEI_IN(J1)  
              AMPL_RES(J1) = ABS ( DRF_SEG(J1) )/WEI_IN(J1)
              PHAS_RES(J1) = PHAS_CMPL_R4 ( DRF_SEG(J1) )
            ELSE
              FREQ_IN(J1)  = PIM%FREQ_ARR(ICHN,IND_FRQ,PIM%CONF%FRQ_GRP)
              WEI_IN(J1)   = 0.0
              AMPL_RES(J1) = 0.0
              PHAS_RES(J1) = 0.0
         END IF
         IF ( FL_PLOT ) THEN
              T2(J1) = FREQ_IN(J1)
              X2(J1) = PHAS_CMPL_R4 ( DRF_SEG(J1) )
         END IF
 410  CONTINUE
      IF ( MODE == 1 ) THEN
           IF ( FL_PLOT ) THEN
                WRITE ( 6, * ) 'PIMA_INIT_PBP_STA Line-119  ind_obs= ', int2(ind_obs), ' IND_FRQ= ', int2(ind_frq) ! %%%%%%%%%
                call diagi_2 ( jchn, t1, x1, kchn, t2, x2, ier ) ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
           END IF
           CALL ERR_LOG ( 0, IUER )
           RETURN 
      END IF
!
! --- Resolve phase ambiguity with respect to the averaged
! --- phase of this IF
!
      PHAS_ALL = PHAS_CMPL_R4 ( DRF_ALL )
      DO 430 J3=1,KCHN
         ITURN = IDNINT( (PHAS_RES(J3) - PHAS_ALL)/PI2 )
         PHAS_RES(J3) = PHAS_RES(J3) - ITURN*PI2
 430  CONTINUE
!
! --- Compute the interpolating polynomial for phases and
! --- amplitudes and replace the actual amplitudes and phases
! --- with those computed using this polynomials
!
      CALL ERR_PASS ( IUER, IER )
      IF ( PIM%CONF%BPS_INTRP_METHOD == PIMA__LEGENDRE ) THEN
!
! -------- ... Using expansion into the Legendre polynomial basis
!
           CALL ERR_PASS ( IUER, IER )
           FL_AMB_IF = BPASS_MOD_POLY ( MOD_AMB, &
     &                                  PIM%CONF%BPS_AMP_MIN,  &
     &                                  PIM%CONF%BPS_DEG_AMP,  &
     &                                  PIM%CONF%BPS_DEG_PHS,  &
     &                                  KCHN,     FREQ_IN,  PHAS_RES, AMPL_RES, &
     &                                                      PHAS_MOD, AMPL_MOD, &
     &                                  PIM%NCHN, FREQ_OUT, PHAS_OUT, AMPL_OUT, &
     &                                  IER )
         ELSE IF ( PIM%CONF%BPS_INTRP_METHOD == PIMA__SPLINE ) THEN
!
! -------- ... Using expansion into the B-spline basis
!
           CALL ERR_PASS ( IUER, IER )
           FL_AMB_IF = BPASS_MOD_SPLINE ( MOD_AMB, &
     &                       PIM%CONF%BPS_AMP_MIN,  &
     &                       PIM%CONF%BPS_DEG_AMP, &
     &                       PIM%CONF%BPS_DEG_PHS, &
     &                       KCHN,     FREQ_IN,  PHAS_RES, AMPL_RES, &
     &                                                     PHAS_MOD, AMPL_MOD, &
     &                       PIM%NCHN, FREQ_OUT, PHAS_OUT, AMPL_OUT, &
     &                       IER )
         ELSE IF ( PIM%CONF%BPS_INTRP_METHOD == PIMA__LINEAR ) THEN
!
! -------- Using linear model
!
           CALL BPASS_MOD_LINEAR ( 2, CMPL_ARR(1,IND_FRQ), &
     &                             AC_AVR(1,IND_FRQ,IND_REF), AC_AVR(1,IND_FRQ,IND_REM), &
     &                             KCHN,     FREQ_IN,  PHAS_RES, AMPL_RES, PHAS_MOD, AMPL_MOD, &
     &                             PIM%NCHN, FREQ_OUT, PHAS_OUT, AMPL_OUT, &
     &                             IER )
      END IF
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR  )
           CALL CLRCH ( STR1 )
           CALL INCH  ( IND_OBS, STR  )
           CALL INCH  ( IND_FRQ,    STR1 )
           CALL ERR_LOG ( 6371, IUER, 'PIMA_INIT_PBP_STA', &
     &         'Failure to compute interpolating '// &
     &         'polynomial for badnpass phases for '// &
     &         'station '//PIM%C_STA(ISTA)// &
     &         ' observation '//STR(1:I_LEN(STR))// &
     &         ' IF# '//STR1 )
           RETURN
      END IF
      DO 440 J4=1,PIM%NCHN ! cycle over spectral channels
         IF ( FL_BMASK ) THEN
              MASK_CHN = PIM%BANDPASS_MASK(J4,IND_FRQ,IND_STA(1),PIMA__MASK_BPAS) * &
     &                   PIM%BANDPASS_MASK(J4,IND_FRQ,IND_STA(2),PIMA__MASK_BPAS)
            ELSE 
              MASK_CHN = 1
         END IF
!
! ------ Write down the value of the model for a given spectral channel
!
         AMPL_OUT(J4) = MASK_CHN*AMPL_OUT(J4)
         CMPL_ARR(J4,IND_FRQ) = CMPLX( AMPL_OUT(J4)*COS(PHAS_OUT(J4)), &
     &                                 AMPL_OUT(J4)*SIN(PHAS_OUT(J4))  )
 440  CONTINUE
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  PIMA_INIT_PBP_STA  !#!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE PG_DEL ( PIM, CMPL, P_AVR, G_DEL, P_RMS )
! ************************************************************************
! *                                                                      *
! *   Auxilliary routine for computing average phase, group delay and    *
! *   rms of residual phases.                                            *
! *                                                                      *
! *  ### 28-FEB-2019     PG_DEL    v1.0 (c)  L. Petrov  28-FEB-2019 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'pima.i'
      INCLUDE   'vtd.i'
      TYPE     ( PIMA__TYPE ) :: PIM
      COMPLEX*8  CMPL(PIM%NCHN,PIM%CONF%END_FRQ-PIM%CONF%BEG_FRQ+1)
      REAL*8     P_AVR, G_DEL, P_RMS 
      INTEGER*4  ME_ITER, MI_ITER
      PARAMETER  ( ME_ITER = 8   )
      PARAMETER  ( MI_ITER = 128 )
      REAL*8       GD_RANGE_MAX 
      PARAMETER  ( GD_RANGE_MAX = 5.0D0 )
      REAL*8     GD_MIN, GD_MAX, DF, AMPL_MAX, G_RAN, G_BEST, P_BEST, F0
      REAL*4     ANG
      INTEGER*4  MP
      PARAMETER  ( MP = 8192 ) 
      REAL*8     T1(MP), X1(MP), X2(MP), X3(MP)
      COMPLEX*8  DRF
      INTEGER*4  J1, J2, J3, J4, J5, J6, IND_MAX, LFRQ, IFRQ, KP
      REAL*4,    EXTERNAL :: PHAS_CMPL_R4
!
      GD_MIN = -GD_RANGE_MAX
      GD_MAX =  GD_RANGE_MAX
      LFRQ   = PIM%CONF%END_FRQ - PIM%CONF%BEG_FRQ + 1
      F0     = PIM%FREQ_ARR(1,PIM%CONF%BEG_FRQ,PIM%CONF%FRQ_GRP) 
      DF = ( PIM%FREQ_ARR(PIM%NCHN,PIM%CONF%END_FRQ,PIM%CONF%FRQ_GRP) - F0 ) + &
     &     ( PIM%FREQ_ARR(2,PIM%CONF%BEG_FRQ,PIM%CONF%FRQ_GRP) - F0 )
!
      DO 410 J1=1,ME_ITER
         AMPL_MAX = -1.0D0
         DO 420 J2=1,MI_ITER
            G_DEL = ( GD_MIN + (J2-1)*(GD_MAX - GD_MIN)/(MI_ITER-1) )/DF
            DRF = 0.0
            IFRQ = 0
            DO 430 J3=PIM%CONF%BEG_FRQ,PIM%CONF%END_FRQ
               IFRQ = IFRQ + 1
               DO 440 J4=1,PIM%NCHN
                  ANG = PI2*(PIM%FREQ_ARR(J4,IFRQ,PIM%CONF%FRQ_GRP) - F0)*G_DEL
                  DRF = DRF + CMPL(J4,IFRQ) * CMPLX(COS(-ANG),SIN(-ANG))
 440           CONTINUE 
 430        CONTINUE 
            IF ( ABS(DRF) > AMPL_MAX ) THEN
                 AMPL_MAX = ABS(DRF)
                 P_BEST = PHAS_CMPL_R4 ( DRF )
                 IND_MAX = J2
            END IF
 420     CONTINUE 
         G_RAN  = GD_MAX - GD_MIN
         G_BEST = GD_MIN + (IND_MAX-1)*(GD_MAX - GD_MIN)/(MI_ITER-1)
         GD_MIN = G_BEST - G_RAN/8
         GD_MAX = G_BEST + G_RAN/8
 410  CONTINUE 
      G_DEL = G_BEST/DF
      P_AVR = P_BEST
!
      IFRQ = 0
      P_RMS = 0.0D0
      KP = 0
      DO 450 J5=PIM%CONF%BEG_FRQ,PIM%CONF%END_FRQ
         IFRQ = IFRQ + 1
         DO 460 J6=1,PIM%NCHN
            ANG = PI2*(PIM%FREQ_ARR(J6,IFRQ,PIM%CONF%FRQ_GRP) - F0)*G_DEL + P_AVR
            P_RMS = P_RMS + ( PHAS_CMPL_R4 ( CMPL(J6,IFRQ)*CMPLX(COS(-ANG),SIN(-ANG)) ) )**2
            KP = KP + 1
            T1(KP) = PIM%FREQ_ARR(J6,IFRQ,PIM%CONF%FRQ_GRP) - F0
            X1(KP) = PHAS_CMPL_R4 ( CMPL(J6,IFRQ) )
            X2(KP) = ANG
            X3(KP) = PHAS_CMPL_R4 ( CMPL(J6,IFRQ)*CMPLX(COS(-ANG),SIN(-ANG)) ) 
 460     CONTINUE 
 450  CONTINUE 
      P_RMS = DSQRT ( P_RMS/(LFRQ*PIM%NCHN) )
      G_DEL = G_DEL*DF
      RETURN
      END  SUBROUTINE  PG_DEL  !#!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE PIMA_PBP_LIN ( PIM, ISTA, IND_OBS, KCHN, LFRQ, FRG_IND, &
     &                          CMPL_ARR, DRF_SEG, FREQ_SEG, WEI_SEG, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine PIMA_PBP_LIN computed the polarization bandpass for the    *
! *   ISTA-th station. It computes the linear trend in the bandpass      *
! *   phase over the band. It computes IF-specific phase offsets and     *
! *   applies then to the bandpass phases.                               *
! *                                                                      *
! *  ### 18-APR-2019  PIMA_PBP_LIN  v1.1 (c)  L. Petrov  04-DEC-2020 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'astro_constants.i'
      INCLUDE   'pima.i'
      TYPE     ( PIMA__TYPE         ) :: PIM
      INTEGER*4  ISTA, IND_OBS, KCHN, LFRQ, FRG_IND, IUER
      COMPLEX*8  CMPL_ARR(PIM%NCHN,PIM%NFRQ), DRF_SEG(KCHN,LFRQ)
      REAL*4     FREQ_SEG(KCHN,LFRQ)
      REAL*4     WEI_SEG(KCHN,LFRQ)
      REAL*8     T1(8192), X1(8192), T2(8192), X2(8192), X3(8192), T4(8192), X4(8192)
      INTEGER*4  M_ITER, M_STEP
      REAL*8     NTUR
      PARAMETER  ( M_ITER =    1  )
      PARAMETER  ( M_STEP = 8192  )
      PARAMETER  ( NTUR   = 8.0D0 ) ! the range -+ phase turns per IF at the edge of the band
      LOGICAL*1  FL_PLOT
      COMPLEX*8  DRF_ACC, DRF_ADD
      REAL*4     GD, AMP_MAX, GD_BEST, PHS_BEST, FRQ_INTR, GD_STEP, PHS_IF
      INTEGER*4  J1, J2, J3, J4, J5, J6, J7, IP, IND_FRQ, ICHN, IER
      REAL*4,    EXTERNAL :: PHAS_CMPL_R4
!
      FL_PLOT = .FALSE. ! Debugging option
!
! --- Get the frequncy interval (cyclic frequency)
!
      FRQ_INTR = FREQ_SEG(KCHN,LFRQ) + (FREQ_SEG(2,1) - FREQ_SEG(1,1)) - FREQ_SEG(1,1)
!
! --- Determine the step of group delay
!
      GD_STEP = 2*NTUR/FRQ_INTR/M_STEP
      DO 410 J1=1,M_STEP
!
! ------ GD -- group delay for the j1-th trial
!
         GD = -NTUR/FRQ_INTR + GD_STEP*(J1-1)
!
! ------ Compute the weighted complex sum of complex residuals
!
         DRF_ACC = 0.0
         ICHN = 0
         DO 420 J2=1,LFRQ
            DO 430 J3=1,KCHN
               DRF_ADD = CMPLX( COS(GD*PI2*(FREQ_SEG(J3,J2) - FREQ_SEG(1,1))), &
     &                          SIN(GD*PI2*(FREQ_SEG(J3,J2) - FREQ_SEG(1,1)))  )
               DRF_ACC = DRF_ACC + DRF_SEG(J3,J2)*DRF_ADD*WEI_SEG(J3,J2)
               IF ( FL_PLOT ) THEN
                    ICHN = ICHN + 1
                    T2(ICHN) = FREQ_SEG(J3,J2) - FREQ_SEG(1,1)
                    X2(ICHN) = PHAS_CMPL_R4 ( DRF_SEG(J3,J2) )
               END IF
 430        CONTINUE 
!
! --------- Check whether this weighted sum provided the maximum ampitude
!
            IF ( J1 == 1 ) THEN
                 AMP_MAX = ABS(DRF_ACC)
                 GD_BEST = GD
                 PHS_BEST = PHAS_CMPL_R4 ( DRF_ACC )
               ELSE
                 IF ( ABS(DRF_ACC) > AMP_MAX ) THEN
                      AMP_MAX  = ABS(DRF_ACC)
                      GD_BEST = GD
                      PHS_BEST = PHAS_CMPL_R4 ( DRF_ACC )
                 END IF
            END IF
            IF ( FL_PLOT ) THEN
                 T1(J1) = GD
                 X1(J1) = ABS(DRF_ACC)
            END IF
 420     CONTINUE 
 410  CONTINUE
      IF ( FL_PLOT ) THEN
           WRITE ( 6, * ) 'GD_BEST = ', GD_BEST, ' PHS_BEST= ', PHS_BEST
           CALL DIAGI_1 ( ICHN, T2, X2, IUER )
           CALL DIAGI_1 ( M_STEP, T1, X1, IUER )
      END IF
!
      IP = 0
      ICHN = 0
      DO 440 J4=1,LFRQ
         IND_FRQ = PIM%CONF%BEG_FRQ + J4 - 1
         DRF_ACC = 0.0
         DO 450 J5=1,KCHN
            ICHN = ICHN + 1
            DRF_ADD = CMPLX( COS(GD_BEST*PI2*(FREQ_SEG(J5,J4) - FREQ_SEG(1,1)) - PHS_BEST), &
     &                       SIN(GD_BEST*PI2*(FREQ_SEG(J5,J4) - FREQ_SEG(1,1)) - PHS_BEST)  )
            DRF_ACC = DRF_ACC + DRF_SEG(J5,J4)*DRF_ADD*WEI_SEG(J5,J4)
 450     CONTINUE 
!
! ------ PHS_IF is the individual phase offset for the IND_FRQ-th IF
!
         PHS_IF = PHAS_CMPL_R4 ( DRF_ACC )
         DO 460 J6=1,KCHN
            IP = IP + 1
            DRF_ADD = CMPLX( COS(GD_BEST*PI2*(FREQ_SEG(J6,J4) - FREQ_SEG(1,1)) - PHS_BEST), &
     &                       SIN(GD_BEST*PI2*(FREQ_SEG(J6,J4) - FREQ_SEG(1,1)) - PHS_BEST)  )
!
! --------- Store the complex polarization bandpass value
!
            IF ( FL_PLOT ) THEN
                 T1(IP) =  FREQ_SEG(J6,J4)
                 X1(IP) =  PHAS_CMPL_R4 ( DRF_SEG(J6,J4) )
                 X2(IP) =  PHAS_CMPL_R4 ( DRF_SEG(J6,J4)*DRF_ADD )
                 X3(IP) = -PHAS_CMPL_R4(DRF_ADD) + PHS_IF
            END IF
 460     CONTINUE 
!
! ------ Write down the output bandpass
!
         DO 470 J7=1,PIM%NCHN
            IF ( FL_PLOT ) THEN
                 T4(J7) = PIM%FREQ_ARR(J7,IND_FRQ,FRG_IND)
                 X4(J7) = PHAS_CMPL_R4 ( CMPL_ARR(J7,IND_FRQ)   )
            END IF
            DRF_ADD = CMPLX( COS(GD_BEST*PI2*(PIM%FREQ_ARR(J7,IND_FRQ,FRG_IND) - FREQ_SEG(1,1)) - PHS_BEST), &
     &                       SIN(GD_BEST*PI2*(PIM%FREQ_ARR(J7,IND_FRQ,FRG_IND) - FREQ_SEG(1,1)) - PHS_BEST)  )
            CMPL_ARR(J7,IND_FRQ) = CONJG(DRF_ADD) * CMPLX(COS(PHS_IF),SIN(PHS_IF))
 470     CONTINUE 
 440  CONTINUE 
!
      IF ( FL_PLOT ) THEN
           WRITE ( 6, * ) 'IND_OBS = ', IND_OBS, ' Sta= ', &
     &                    PIM%C_STA(ISTA), ' PHS_IF=  ', PHS_IF
           CALL DIAGI_SETDEF ( IER, 'DIAGI_CTIT', 'PBP phases for station '//PIM%C_STA(ISTA) )
           CALL DIAGI_3 ( IP, T1, X1, PIM%NCHN, T4, X4, IP, T1, X3, IER )
!!           CALL DIAGI_3 ( IP, T1, X1, IP, T1, X2, IP, T1, X3, IER )
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  PIMA_PBP_LIN  !#!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE PIMA_LIN_PHAOFF ( UV, DQ, PHA_OFF, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine PIMA_LIN_PHAOFF computes phase offset of linear            *
! *   polarization data using special visibility matrix.                 *
! *                                                                      *
! * ### 01-MAY-2019  PIMA_LIN_PHAOFF  v1.1 (c) L. Petrov 06-MAY-2019 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      COMPLEX*8  UV(4)
      REAL*4     DQ, PHA_OFF(2)
      INTEGER*4  IUER
      INTEGER*4  M_ITER
      PARAMETER  ( M_ITER = 1024 )
      REAL*4     ES(2), EL(2), FN(2), DF1, DF2, DA, EPS
      PARAMETER  ( EPS = 1.0E-4 )
      REAL*8     JM(2,2)
      INTEGER*4  J1, IER
      REAL*4,    EXTERNAL :: PHAS_CMPL_R4 
!
      ES = 0.0
      DA = 0.01
!
      DO 410 J1=1,M_ITER
         EL(1) = ES(1)
         EL(2) = ES(2)
         DF1 = (   PHAS_CMPL_R4 ( UV(2)/UV(1) * &
     &                            (1.0 - SIN(DQ)**2*(1.0 - CMPLX(COS(-ES(2)+DA),SIN(-ES(2)+DA))) )/  &
     &                            (1.0 - COS(DQ)**2*(1.0 - CMPLX(COS(-ES(2)+DA),SIN(-ES(2)+DA))) ) ) &
     &           - PHAS_CMPL_R4 ( UV(2)/UV(1) * &
     &                            (1.0 - SIN(DQ)**2*(1.0 - CMPLX(COS(-ES(2)),SIN(-ES(2)))) ) /   &
     &                            (1.0 - COS(DQ)**2*(1.0 - CMPLX(COS(-ES(2)),SIN(-ES(2)))) ) ) &
     &          )/DA
!
         DF2 =  (  PHAS_CMPL_R4 ( UV(4)/UV(3) * &
     &                            (1.0 - SIN(DQ)**2*(1.0 - CMPLX(COS(ES(1)+DA),SIN(ES(1)+DA))) ) / &
     &                            (1.0 - COS(DQ)**2*(1.0 - CMPLX(COS(ES(1)+DA),SIN(ES(1)+DA))) ) ) &
     &           - PHAS_CMPL_R4 ( UV(4)/UV(3) * &
     &                            (1.0 - SIN(DQ)**2*(1.0 - CMPLX(COS(ES(1)),SIN(ES(1))))   ) / &
     &                            (1.0 - COS(DQ)**2*(1.0 - CMPLX(COS(ES(1)),SIN(ES(1))))   ) ) &
     &        )/DA
         JM(1,1) = 1.0D0
         JM(1,2) = -DF1
         JM(2,1) =  DF2
         JM(2,2) = 1.0D0
         CALL ERR_PASS ( IUER, IER )
         CALL INVA ( 2, JM, 1.D0*EPS, IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 6299, IUER, PIMA_LIN_PHAOFF, 'Jacobian is singular' )
              RETURN 
         END IF
!
         FN(1) = ES(1) - PHAS_CMPL_R4 ( UV(2)/UV(1) * &
     &                         (1.0 - SIN(DQ)**2*(1.0 - CMPLX(COS(-ES(2)),SIN(-ES(2)))) )/ &
     &                         (1.0 - COS(DQ)**2*(1.0 - CMPLX(COS(-ES(2)),SIN(-ES(2)))) )  )
         FN(2) = ES(2) + PHAS_CMPL_R4 ( UV(4)/UV(3) * &
     &                         (1.0 - SIN(DQ)**2*(1.0 - CMPLX(COS(ES(1)),SIN(ES(1)))) ) / &
     &                         (1.0 - COS(DQ)**2*(1.0 - CMPLX(COS(ES(1)),SIN(ES(1)))) )  )
         ES(1) = ES(1) - JM(1,1)*FN(1) - JM(1,2)*FN(2) 
         ES(2) = ES(2) - JM(2,1)*FN(1) - JM(2,2)*FN(2) 
!         write ( 6, 210 ) j1, es
! 210     format ( 'Iter: ', I3, ' Es= ', F8.5, 1X, F8.5 )
!!
         IF ( ABS(EL(1) - ES(1)) < EPS  .AND.  ABS(EL(2) - ES(2)) < EPS ) GOTO 810
 410  CONTINUE 
 810  CONTINUE 
      PHA_OFF = ES
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  PIMA_LIN_PHAOFF  !#!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE PIMA_LIN_PHAOFF_ARR ( PIM, IND_OBS, NP, UV_ARR, RES_AVR, &
     &                                 FREQ_ARR_R4, DQ, FREQ_ARR_IF, &
     &                                 AMPL_REF_MOD, AMPL_REM_MOD, &
     &                                 PHAS_REF_MOD, PHAS_REM_MOD, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine PIMA_LIN_PHAOFF_ARR
! *                                                                      *
! * ## 03-MAY-2019 PIMA_LIN_PHAOFF_ARR v1.1 (c) L. Petrov 15-APR-2020 ## *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'pima.i'
      TYPE     ( PIMA__TYPE         ) :: PIM
      INTEGER*4  IND_OBS, NP, IUER
      COMPLEX*8  UV_ARR(PIM__MCHN,PIM__MPLR), RES_AVR(PIM__MPLR)
      REAL*4     FREQ_ARR_R4(PIM__MCHN)
      REAL*8     FREQ_ARR_IF(PIM%NCHN)
      INTEGER*4  DEG
      PARAMETER  ( DEG = 3 )
      REAL*4     DQ, AMPL_REM_MOD(NP), AMPL_REF_MOD(NP), &
     &               PHAS_REM_MOD(NP), PHAS_REF_MOD(NP)
      REAL*8     AMPL_RES(PIM__MCHN,PIM__MPLR), PHAS_RES(PIM__MCHN,PIM__MPLR)
      REAL*8     AMPL_COEF(1-DEG:PIM__MCHN,PIM__MPLR), &
     &           PHAS_COEF(1-DEG:PIM__MCHN,PIM__MPLR), &
     &           AMPL_COEF_ERR(1-DEG:PIM__MCHN,PIM__MPLR), &
     &           PHAS_COEF_ERR(1-DEG:PIM__MCHN,PIM__MPLR), &
     &           WEI(PIM__MCHN), FREQ_ARR(PIM__MCHN), WW, ARG_BEG, ARG_END, ARG_STP
      REAL*8     CNS_VAL, CNS_DER, CNS_DR2, WRMS, PHA_OFF_ARR(1-DEG:PIM__MCHN,2), &
     &           ARGA(PIM__MCHN), ARGP(PIM__MCHN), &
     &           PHAS_WRMS(PIM__MPLR), AMPL_WRMS(PIM__MPLR)
      CHARACTER  STR*128
      COMPLEX*8  UV_MOD(4)
      REAL*4     EPS, OVS, PHA_OFF(2), PHS_MOD_VAL, AMP_MOD_VAL
      PARAMETER  ( EPS = 1.D-5 )
      PARAMETER  ( OVS = 0.25  )
      INTEGER*4  PLOT_MODE
      INTEGER*4  J1, J2, J3, J4, J5, J6, J7, J8, J9, NAB, NAE, NPB, NPE, &
     &           IV(PIM__MCHN), KP, IER
      REAL*8     F1(PIM__MCHN), F2(PIM__MCHN), &
     &           X1(PIM__MCHN), X2(PIM__MCHN), X3(PIM__MCHN), X4(PIM__MCHN), &
     &           Y1(PIM__MCHN), Y2(PIM__MCHN), Y3(PIM__MCHN), Y4(PIM__MCHN) 
      REAL*4,    EXTERNAL :: PHAS_CMPL_R4
      REAL*8,    EXTERNAL :: LEGENDRE_VAL, EBSPL_VAL_R8
!
      PLOT_MODE = 3  ! 0: none, 1: phase; 2: amplitude, 3: pha_off_ref/pha_off_rem
!
      CNS_VAL = 10.0D0
      CNS_DER = 3.0D0/(FREQ_ARR_R4(2) - FREQ_ARR_R4(1))
      CNS_DR2 = 3.0D0/(FREQ_ARR_R4(2) - FREQ_ARR_R4(1))**2
!
      KP = 0
      DO 410 J1=1,NP
         IF ( ABS(UV_ARR(J1,1)) > PIMA__AMP_MIN  .AND. &
              ABS(UV_ARR(J1,2)) > PIMA__AMP_MIN  .AND. &
              ABS(UV_ARR(J1,3)) > PIMA__AMP_MIN  .AND. &
              ABS(UV_ARR(J1,4)) > PIMA__AMP_MIN  .AND. &
              ABS(TAN(DQ))  < 1.0/EPS            .AND. &
              ABS(TAN(DQ))  > EPS                      ) THEN
!
! ----------- Extract phases and aplitudes from the complex residual
!
              DO 420 J2=1,PIM__MPLR
                 AMPL_RES(J1,J2) = ABS ( UV_ARR(J1,J2) ) / ABS ( RES_AVR(J2) )
                 PHAS_RES(J1,J2) = PHAS_CMPL_R4 ( UV_ARR(J1,J2) ) - PHAS_CMPL_R4 ( RES_AVR(J2) ) 
                 IF ( PHAS_RES(J1,J2) >  PI__NUM ) PHAS_RES(J1,J2) = PHAS_RES(J1,J2) - PI2
                 IF ( PHAS_RES(J1,J2) < -PI__NUM ) PHAS_RES(J1,J2) = PHAS_RES(J1,J2) + PI2
 420          CONTINUE 
              WEI(J1) = 1.0
              IV(J1)  = 1
              KP = KP + 1
           ELSE
              IV(J1)  = 0
              WEI(J1) = 0.0
              AMPL_RES(J1,1:PIM__MPLR) = 0.0
              PHAS_RES(J1,1:PIM__MPLR) = 0.0
         END IF
         FREQ_ARR(J1) = FREQ_ARR_R4(J1) - FREQ_ARR_R4(1)
         IF ( PLOT_MODE == 1 ) THEN
              F1(J1) = FREQ_ARR(J1) 
              X1(J1) = PHAS_CMPL_R4 ( UV_ARR(J1,1) )
              X2(J1) = PHAS_CMPL_R4 ( UV_ARR(J1,2) )
              X3(J1) = PHAS_CMPL_R4 ( UV_ARR(J1,3) )
              X4(J1) = PHAS_CMPL_R4 ( UV_ARR(J1,4) )
           ELSE IF ( PLOT_MODE == 2 ) THEN
              F1(J1) = FREQ_ARR(J1) 
              X1(J1) = ABS ( UV_ARR(J1,1) ) / ABS ( RES_AVR(1) )
              X2(J1) = ABS ( UV_ARR(J1,2) ) / ABS ( RES_AVR(2) )
              X3(J1) = ABS ( UV_ARR(J1,3) ) / ABS ( RES_AVR(3) )
              X4(J1) = ABS ( UV_ARR(J1,4) ) / ABS ( RES_AVR(4) )
         END IF
 410  CONTINUE 
!
      IF ( KP == 0 ) THEN
           DO 520 J2=1,NP
              PHAS_REF_MOD(J2) = 0.0D0
              PHAS_REM_MOD(J2) = 0.0D0
              AMPL_REF_MOD(J2) = 1.0D0
              AMPL_REM_MOD(J2) = 1.0D0
 520       CONTINUE 
           CALL ERR_LOG ( 0, IUER )
           RETURN 
      END IF
!
      DO 430 J3=1,PIM__MPLR
!
! ------ Processing phases
!
         IF ( PIM%CONF%BPS_INTRP_METHOD == PIMA__LEGENDRE .AND. &
     &        ( PIM%CONF%BPS_DEG_PHS == 0 .OR. KP == 1 ) ) THEN
              PHAS_COEF(0,J3) = 0.0D0
              WW = 0.0D0
              DO 440 J4=1,NP
                 PHAS_COEF(0,J3) = PHAS_COEF(0,J3) + PHAS_RES(J4,J3)*WEI(J4)
                 AMPL_COEF(0,J3) = AMPL_COEF(0,J3) + AMPL_RES(J4,J3)*WEI(J4)
                 WW = WW + WEI(J4)
 440          CONTINUE 
              PHAS_COEF(0,J3) = PHAS_COEF(0,J3)/WW
              NPB = 0
              NPE = 0
            ELSE IF ( PIM%CONF%BPS_INTRP_METHOD == PIMA__LEGENDRE .AND. &
     &                PIM%CONF%BPS_DEG_PHS == 1                         ) THEN
              CALL ERR_PASS ( IUER, IER )
              CALL REGRW8   ( NP, FREQ_ARR, PHAS_RES(1,J3), WEI, IV, PHAS_COEF(1,J3), &
     &                        PHAS_COEF(0,J3), IER )
              IF ( IER .NE. 0 ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( NP, STR  )
                   CALL ERR_LOG ( 5871, IUER, 'PIMA_LIN_PHAOFF_ARR', 'Failure '// &
     &                 'in REGRW8 for phases NP= '//STR )
                   RETURN
              END IF
              NPB = 0
              NPE = 1
            ELSE IF ( PIM%CONF%BPS_INTRP_METHOD == PIMA__LEGENDRE ) THEN
              CALL ERR_PASS ( IUER, IER )
              CALL LEGENDRE_REGR_CNS ( NP, FREQ_ARR, PHAS_RES(1,J3), WEI, PIM%CONF%BPS_DEG_PHS, &
     &                                 CNS_VAL, CNS_DER, CNS_DR2, &
     &                                 PHAS_COEF(0,J3), PHAS_COEF_ERR(0,J3), IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 5872, IUER, 'PIMA_LIN_PHAOFF_ARR', 'Failure '// &
     &                 'in LEGENDRE_REGR for phases' )
                   RETURN
              END IF
              NPB = 0
              NPE = PIM%CONF%BPS_DEG_PHS
            ELSE IF ( PIM%CONF%BPS_INTRP_METHOD == PIMA__SPLINE ) THEN
              ARG_BEG = FREQ_ARR_IF(1)        - OVS*(FREQ_ARR(1)           - FREQ_ARR_IF(1) )
              ARG_END = FREQ_ARR_IF(PIM%NCHN) + OVS*(FREQ_ARR_IF(PIM%NCHN) - FREQ_ARR(NP)   )
              ARG_STP = (ARG_END - ARG_BEG)/(PIM%CONF%BPS_DEG_PHS-1)
              DO 450 J5=1,PIM%CONF%BPS_DEG_PHS
                 ARGP(J5) = ARG_BEG + (J5-1)*ARG_STP
 450          CONTINUE 
!
              CALL ERR_PASS ( IUER, IER )
              CALL EBSPL_WLSQ_CNS3 ( NP, FREQ_ARR, PHAS_RES(1,J3), WEI, &
          &                          PIM%CONF%BPS_DEG_PHS, DEG, ARGP, PHAS_COEF(1-DEG,J3), &
     &                               CNS_VAL, CNS_DER, CNS_DR2, PHAS_WRMS(J3), IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 5873, IUER, 'PIMA_LIN_PHAOFF_ARR', 'Failure '// &
          &            'in attempt to perform expansion into the spline basis ' )
                   RETURN
              END IF
              NPB = 1-DEG
              NPE = PIM%CONF%BPS_DEG_PHS-1
         END IF 
!
! ------ Processing amplitudes
!
         IF ( PIM%CONF%BPS_INTRP_METHOD == PIMA__LEGENDRE .AND. &
     &        PIM%CONF%BPS_DEG_AMP == 0                         ) THEN
              AMPL_COEF(0,J3) = 0.0D0
              WW = 0.0D0
              DO 460 J6=1,NP
                 AMPL_COEF(0,J3) = AMPL_COEF(0,J3) + AMPL_RES(J6,J3)*WEI(J6)
                 WW = WW + WEI(J6)
 460          CONTINUE 
              AMPL_COEF(0,J3) = AMPL_COEF(0,J3)/WW
              NAB = 0
              NAE = 0
            ELSE IF ( PIM%CONF%BPS_INTRP_METHOD == PIMA__LEGENDRE .AND. &
     &        ( PIM%CONF%BPS_DEG_AMP == 1  .OR.  KP == 1 )               ) THEN
              CALL ERR_PASS ( IUER, IER )
              CALL REGRW8   ( NP, FREQ_ARR, AMPL_RES(1,J3), WEI, %VAL(0), AMPL_COEF(1,J3), &
     &                        AMPL_COEF(0,J3), IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 5874, IUER, 'PIMA_LIN_PHAOFF_ARR', 'Failure '// &
     &                 'in REGRW8 for amplitudes' )
                   RETURN
              END IF
              NAB = 0
              NAE = 1
            ELSE IF ( PIM%CONF%BPS_INTRP_METHOD == PIMA__LEGENDRE ) THEN
              CALL ERR_PASS ( IUER, IER )
              CALL LEGENDRE_REGR_CNS ( NP, FREQ_ARR, AMPL_RES(1,J3), WEI, PIM%CONF%BPS_DEG_AMP, &
     &                                 CNS_VAL, CNS_DER, CNS_DR2, &
     &                                 AMPL_COEF(0,J3), AMPL_COEF_ERR(0,J3), IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 5875, IUER, 'PIMA_LIN_PHAOFF_ARR', 'Failure '// &
     &                 'in LEGENDRE_REGR for amplitude' )
                   RETURN
              END IF
              NAB = 0
              NAE = PIM%CONF%BPS_DEG_AMP
            ELSE IF ( PIM%CONF%BPS_INTRP_METHOD == PIMA__SPLINE ) THEN
              ARG_BEG = FREQ_ARR_IF(1)        - OVS*(FREQ_ARR(1)           - FREQ_ARR_IF(1) )
              ARG_END = FREQ_ARR_IF(PIM%NCHN) + OVS*(FREQ_ARR_IF(PIM%NCHN) - FREQ_ARR(NP)   )
              ARG_STP = (ARG_END - ARG_BEG)/(PIM%CONF%BPS_DEG_AMP-1)
              DO 470 J7=1,PIM%CONF%BPS_DEG_AMP
                 ARGA(J7) = ARG_BEG + (J7-1)*ARG_STP
 470          CONTINUE 
!
              CALL ERR_PASS ( IUER, IER )
              CALL EBSPL_WLSQ_CNS3 ( NP, FREQ_ARR, AMPL_RES(1,J3), WEI, &
          &                          PIM%CONF%BPS_DEG_AMP, DEG, ARGA, AMPL_COEF(1-DEG,J3), &
     &                               CNS_VAL, CNS_DER, CNS_DR2, AMPL_WRMS(J3), IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 5876, IUER, 'PIMA_LIN_PHAOFF_ARR', 'Failure '// &
          &            'in attempt to perform expansion into the spline basis ' )
                   RETURN
              END IF
              NAB = 1-DEG
              NAE = PIM%CONF%BPS_DEG_AMP-1
         END IF 
 430  CONTINUE 
!
      DO 480 J8=1,NP
         DO 490 J9=1,PIM__MPLR
!
! --------- Processing phases
!
            IF ( PIM%CONF%BPS_INTRP_METHOD == PIMA__LEGENDRE .AND. PIM%CONF%BPS_DEG_PHS == 0 ) THEN
                 PHS_MOD_VAL = PHAS_COEF(0,J9)
               ELSE IF ( PIM%CONF%BPS_INTRP_METHOD == PIMA__LEGENDRE .AND. PIM%CONF%BPS_DEG_PHS == 1 ) THEN
                 PHS_MOD_VAL = PHAS_COEF(0,J9) + (FREQ_ARR(J8) - FREQ_ARR(1))*PHAS_COEF(1,J9)
               ELSE IF ( PIM%CONF%BPS_INTRP_METHOD == PIMA__LEGENDRE ) THEN
                 ARG_BEG = FREQ_ARR(1)  - EPS
                 ARG_END = FREQ_ARR(NP) + EPS
                 PHS_MOD_VAL = LEGENDRE_VAL ( PIM%CONF%BPS_DEG_PHS, ARG_BEG, ARG_END, &
     &                                        FREQ_ARR(J8), PHAS_COEF(NPB:NPE,J9) )
               ELSE IF ( PIM%CONF%BPS_INTRP_METHOD == PIMA__SPLINE ) THEN
                 PHS_MOD_VAL = EBSPL_VAL_R8 ( PIM%CONF%BPS_DEG_PHS, DEG, FREQ_ARR(J8), ARGP, &
     &                                        PHAS_COEF(NPB:NPE,J9) )
            END IF
            PHS_MOD_VAL = PHS_MOD_VAL + PHAS_CMPL_R4 ( RES_AVR(J9) ) 
!
! --------- Processing amplitudes
!
            IF ( PIM%CONF%BPS_INTRP_METHOD == PIMA__LEGENDRE .AND. PIM%CONF%BPS_DEG_AMP == 0 ) THEN
                 AMP_MOD_VAL = AMPL_COEF(0,J9)
               ELSE IF ( PIM%CONF%BPS_INTRP_METHOD == PIMA__LEGENDRE .AND. PIM%CONF%BPS_DEG_AMP == 1 ) THEN
                 AMP_MOD_VAL = AMPL_COEF(0,J9) + (FREQ_ARR(J8) - FREQ_ARR(1))*AMPL_COEF(1,J9)
               ELSE IF ( PIM%CONF%BPS_INTRP_METHOD == PIMA__LEGENDRE ) THEN
                 ARG_BEG = FREQ_ARR(1)  - EPS
                 ARG_END = FREQ_ARR(NP) + EPS
                 AMP_MOD_VAL = LEGENDRE_VAL ( PIM%CONF%BPS_DEG_AMP, ARG_BEG, ARG_END, &
     &                                        FREQ_ARR(J8), AMPL_COEF(NAB:NAE,J9) )
               ELSE IF ( PIM%CONF%BPS_INTRP_METHOD == PIMA__SPLINE ) THEN
                 AMP_MOD_VAL = EBSPL_VAL_R8 ( PIM%CONF%BPS_DEG_AMP, DEG, FREQ_ARR(J8), ARGA, &
     &                                        AMPL_COEF(NAB:NAE,J9) )
            END IF
!
            UV_MOD(J9) = AMP_MOD_VAL * CMPLX ( COS(PHS_MOD_VAL), SIN(PHS_MOD_VAL) )
            IF ( PLOT_MODE == 1 .AND. J9 == 1 ) Y1(J8) = PHS_MOD_VAL
            IF ( PLOT_MODE == 1 .AND. J9 == 2 ) Y2(J8) = PHS_MOD_VAL
            IF ( PLOT_MODE == 1 .AND. J9 == 3 ) Y3(J8) = PHS_MOD_VAL
            IF ( PLOT_MODE == 1 .AND. J9 == 4 ) Y4(J8) = PHS_MOD_VAL
            IF ( PLOT_MODE == 2 .AND. J9 == 1 ) Y1(J8) = AMP_MOD_VAL
            IF ( PLOT_MODE == 2 .AND. J9 == 2 ) Y2(J8) = AMP_MOD_VAL
            IF ( PLOT_MODE == 2 .AND. J9 == 3 ) Y3(J8) = AMP_MOD_VAL
            IF ( PLOT_MODE == 2 .AND. J9 == 4 ) Y4(J8) = AMP_MOD_VAL
 490     CONTINUE 
!
         CALL ERR_PASS ( IUER, IER )
         CALL PIMA_LIN_PHAOFF ( UV_MOD, DQ, PHA_OFF, IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 5877, IUER, 'PIMA_LIN_PHAOFF_ARR', 'Failure '// &
          &       'in attempt to compute original phases using expansion coefficients' )
              RETURN
         END IF
         PHAS_REF_MOD(J8) = PHA_OFF(1)
         PHAS_REM_MOD(J8) = PHA_OFF(2)
         AMPL_REF_MOD(J8) = 1.0
         AMPL_REM_MOD(J8) = 1.0
         IF ( PLOT_MODE == 3 ) THEN
              F1(J8) = FREQ_ARR(J8) 
              X1(J8) = PHA_OFF(1)
              X2(J8) = PHA_OFF(2)
         END IF
 480  CONTINUE 
      IF ( PLOT_MODE == 1 ) THEN
           CALL DIAGI_SETDEF ( IER, 'DIAGI_CTIT', 'Phase, Component 1' )
           CALL DIAGI_2 ( NP, F1, X1, NP, F1, Y1, IER )
           CALL DIAGI_SETDEF ( IER, 'DIAGI_CTIT', 'Phase, Component 2' )
           CALL DIAGI_2 ( NP, F1, X2, NP, F1, Y2, IER )
           CALL DIAGI_SETDEF ( IER, 'DIAGI_CTIT', 'Phase, Component 3' )
           CALL DIAGI_2 ( NP, F1, X3, NP, F1, Y3, IER )
           CALL DIAGI_SETDEF ( IER, 'DIAGI_CTIT', 'Phase, Component 4' )
           CALL DIAGI_2 ( NP, F1, X4, NP, F1, Y4, IER )
      END IF
      IF ( PLOT_MODE == 2 ) THEN
           CALL DIAGI_SETDEF ( IER, 'DIAGI_CTIT', 'Amplitude, Component 1' )
           CALL DIAGI_2 ( NP, F1, X1, NP, F1, Y1, IER )
           CALL DIAGI_SETDEF ( IER, 'DIAGI_CTIT', 'Amplitude, Component 2' )
           CALL DIAGI_2 ( NP, F1, X2, NP, F1, Y2, IER )
           CALL DIAGI_SETDEF ( IER, 'DIAGI_CTIT', 'Amplitude, Component 3' )
           CALL DIAGI_2 ( NP, F1, X3, NP, F1, Y3, IER )
           CALL DIAGI_SETDEF ( IER, 'DIAGI_CTIT', 'Amplitude, Component 4' )
           CALL DIAGI_2 ( NP, F1, X4, NP, F1, Y4, IER )
      END IF
      IF ( PLOT_MODE == 3 ) THEN
           CALL DIAGI_SETDEF ( IER, 'DIAGI_CTIT', 'Phase offset reference' )
           CALL DIAGI_1 ( NP, F1, X1, IER )
           CALL DIAGI_SETDEF ( IER, 'DIAGI_CTIT', 'Phase offset remote' )
           CALL DIAGI_1 ( NP, F1, X2, IER )
      END IF
!      
      CALL ERR_LOG ( 0, IUER ) 
      RETURN
      END  SUBROUTINE  PIMA_LIN_PHAOFF_ARR   !#!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE PIMA_PBP_BAND_LIN ( PIM, CMPL_ARR, PHS_OUT, GD_EST, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine PIMA_PBP_BAND_LIN computes
! *                                                                      *
! * ### 05-DEC-2020 PIMA_PBP_BAND_LIN v1.1 (c) L. Petrov 05-DEC-2020 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'astro_constants.i'
      INCLUDE   'pima.i'
      TYPE     ( PIMA__TYPE         ) :: PIM
      INTEGER*4  ISTA, IND_OBS, KCHN, LFRQ, FRG_IND, IUER
      COMPLEX*8  CMPL_ARR(PIM%NCHN,PIM%NFRQ)
      REAL*4     PHS_OUT(PIM%NCHN,PIM%NFRQ), GD_EST
      REAL*4     FREQ_DIF
      INTEGER*4  M_ITER, M_STEP, MP
      REAL*8     NTUR
      PARAMETER  ( M_ITER =    1  )
      PARAMETER  ( M_STEP = 8192  )
      PARAMETER  ( NTUR   = 4.0D0 ) ! the range -+ phase turns per IF at the edge of the band
      LOGICAL*1  FL_PLOT
      COMPLEX*8  DRF_ACC, DRF_ADD, CMPL_EST
      REAL*4     AMP_MAX, PHS_EST, FRQ_INTR, GD_STEP, PHS_IF
      REAL*8     GD, FRQ_DIF
      PARAMETER  ( MP = 8192 ) 
      REAL*8     T1(MP), X1(MP), T2(MP), X2(MP), X3(MP)
      INTEGER*4  J1, J2, J3, J4, J5, J6, J7, IP, IAMB, ICHN, IER
      REAL*4,    EXTERNAL :: PHAS_CMPL_R4
!
      FL_PLOT = .FALSE. ! Debugging option
!
! --- Get the frequency interval (cyclic frequency)
!
      FRQ_INTR = PIM%FREQ_ARR(PIM%NCHN,PIM%CONF%END_FRQ,PIM%CONF%FRQ_GRP) - PIM%FREQ_ARR(1,PIM%CONF%BEG_FRQ,PIM%CONF%FRQ_GRP) + &
     &           PIM%FREQ_ARR(2,PIM%CONF%END_FRQ,PIM%CONF%FRQ_GRP) - PIM%FREQ_ARR(1,PIM%CONF%END_FRQ,PIM%CONF%FRQ_GRP) 
!
! --- Determine the step of group delay
!
      GD_STEP = 2*NTUR/FRQ_INTR/M_STEP
      DO 410 J1=1,M_STEP
!
! ------ GD -- group delay for the j1-th trial
!
         GD = -NTUR/FRQ_INTR + GD_STEP*(J1-1)
!
! ------ Compute the complex sum of complex bandpass
!
         DRF_ACC = 0.0
         ICHN = 0
         DO 420 J2=PIM%CONF%BEG_FRQ,PIM%CONF%END_FRQ
            DO 430 J3=1,PIM%NCHN
               FRQ_DIF = ( PIM%FREQ_ARR(J3,J2,PIM%CONF%FRQ_GRP) - &
     &                     PIM%FREQ_ARR(1,PIM%CONF%BEG_FRQ,PIM%CONF%FRQ_GRP) )
               DRF_ACC = DRF_ACC + CMPLX( COS(PI2*GD*FRQ_DIF), &
     &                                    SIN(PI2*GD*FRQ_DIF)  )*CMPL_ARR(J3,J2)
               IF ( FL_PLOT ) THEN
                    ICHN = ICHN + 1
                    T2(ICHN) = PIM%FREQ_ARR(J3,J2,PIM%CONF%FRQ_GRP)
                    X2(ICHN) = PHAS_CMPL_R4 ( CMPL_ARR(J3,J2) )
               END IF
 430        CONTINUE 
 420     CONTINUE 
!
! ------ Check whether this complex sum provided the maximum ampitude
!
         IF ( J1 == 1 ) THEN
              AMP_MAX = ABS(DRF_ACC)
              GD_EST = GD
              PHS_EST = PHAS_CMPL_R4 ( DRF_ACC )
            ELSE
              IF ( ABS(DRF_ACC) > AMP_MAX ) THEN
                   AMP_MAX  = ABS(DRF_ACC)
                   GD_EST = GD
                   PHS_EST = PHAS_CMPL_R4 ( DRF_ACC )
              END IF
         END IF
         IF ( FL_PLOT ) THEN
              T1(J1) = GD
              X1(J1) = ABS(DRF_ACC)
         END IF
 410  CONTINUE
      IF ( FL_PLOT ) THEN
           WRITE ( 6, * ) 'PIMA_PBP_BAND_LIN-922 GD_EST = ', GD_EST, ' PHS_EST= ', PHS_EST
           CALL DIAGI_1 ( ICHN,   T2, X2, IER )
           CALL DIAGI_1 ( M_STEP, T1, X1, IER )
      END IF
!
      DO 440 J4=PIM%CONF%BEG_FRQ,PIM%CONF%END_FRQ
         DO 450 J5=1,PIM%NCHN
            FRQ_DIF = ( PIM%FREQ_ARR(J5,J4,PIM%CONF%FRQ_GRP) - &
     &                  PIM%FREQ_ARR(1,PIM%CONF%BEG_FRQ,PIM%CONF%FRQ_GRP) )
            IAMB = NINT( (PHAS_CMPL_R4(CMPL_ARR(J5,J4)) + PI2*GD_EST*FRQ_DIF - PHS_EST)/PI2 )
            PHS_OUT(J5,J4) = PHAS_CMPL_R4(CMPL_ARR(J5,J4)) - PI2*IAMB
 450     CONTINUE 
 440  CONTINUE 
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  PIMA_PBP_BAND_LIN  !#!#
