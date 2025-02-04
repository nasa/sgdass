      FUNCTION   BPASS_MOD_SPLINE ( MODE, BPS_AMP_MIN, LNOD_AMP, LNOD_PHS, &
     &                 L_IN,  FRQ_IN,  PHS_IN,  AMP_IN, PHS_MOD, AMP_MOD, &
     &                 L_OUT, FRQ_OUT, PHS_OUT, AMP_OUT, IUER )
! ************************************************************************
! *                                                                      *
! *   Axilliary routine  BPASS_MOD_SPLINE approximates an array of input *
! *   phases and amplitudes using expansion of the basis B-splines of    *
! *   the 3rd degree on the specified number of knots. Constraints on    *
! *   the mean value, the first and second derivatives at each knot are  *
! *   applied during computation of the expansion. The output is written *
! *   into arrays PHS_MOD and AMP_MOD that correspond to the array of    *
! *   input frequencies FRQ_IN and in the arrays PHS_OUT, AMP_OUT that   *
! *   correspond to frequencies FRQ_OUT.                                 *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *        MODE ( INTEGER*4  ) -- Mode: 1 -- not to run phase ambiguity  *
! *                                          resolution procedure.       *
! *                                     2 -- to run one iteration of     *
! *                                          phase ambiguity resolution. *
! * BPS_AMP_MIN ( REAL*8     ) -- Minimally accepted amplitude. Samples  *
! *                               with amplitude less than BPS_AMP_MIN   * 
! *                               are discarded.                         *
! *    LNOD_AMP ( INTEGER*4  ) -- The number of knots of the B-spline    *
! *                               expansion over amplitude.              *
! *    LNOD_PHS ( INTEGER*4  ) -- The number of knots of the B-spline    *
! *                               expansion over phase.                  *
! *        L_IN ( INTEGER*4  ) -- The number of points in the input      *
! *                               arrays.                                *
! *      FRQ_IN ( REAL*4     ) -- Input array of sky frequencies.        *
! *                               Dimension: L_IN, unit: Hz.             *
! *      PHS_IN ( REAL*4     ) -- Input array of phases.                 *
! *                               Dimension: L_IN, unit: rad.            *
! *      AMP_IN ( REAL*4     ) -- Input array of amplitudes.             *
! *                               Dimension: L_IN, unit: dimensionless.  *
! *       L_OUT ( INTEGER*4  ) -- The number of points in the output     *
! *                               arrays.                                *
! *                                                                      *
! * _________________________ Output parameters: _______________________ *
! *                                                                      *
! *     PHS_MOD ( REAL*4     ) -- Modeled phase. Dimension: L_IN.        *
! *                               Unit: rad.                             *
! *     AMP_MOD ( REAL*4     ) -- Modeled amplitude. Dimension: L_IN.    *
! *                               Unit: rad.                             *
! *     FRQ_OUT ( REAL*4     ) -- Output array of sky frequencies.       *
! *                               Dimension: L_IN, unit: Hz.             *
! *     PHS_OUT ( REAL*4     ) -- Modeled phase. Dimension: L_OUT.       *
! *                               Unit: rad.                             *
! *     AMP_OUT ( REAL*4     ) -- Modeled amplitude. Dimension: L_OUT.   *
! *                               Unit: rad.                             *
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
! *     IUER ( INTEGER*4, OPT ) -- Universal error handler.              *
! *                           Input: switch IUER=0 -- no error messages  *
! *                                  will be generated even in the case  *
! *                                  of error. IUER=-1 -- in the case of *
! *                                  error the message will be put on    *
! *                                  stdout.                             *
! *                           Output: 0 in the case of successful        *
! *                                   completion and non-zero in the     *
! *                                   case of error.                     *
! *                                                                      *
! * ### 08-OCT-2012  BPASS_MOD_SPLINE v3.2 (c) L. Petrov 19-FEB-2022 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'astro_constants.i'
      INCLUDE   'pima.i'
      LOGICAL*4  BPASS_MOD_SPLINE
      INTEGER*4  MODE, LNOD_AMP, LNOD_PHS, L_IN, L_OUT, IUER
      REAL*8     BPS_AMP_MIN
      REAL*4     FRQ_IN(L_IN),   PHS_IN(L_IN),   AMP_IN(L_IN), &
     &           PHS_MOD(L_IN), AMP_MOD(L_IN), &
     &           FRQ_OUT(L_OUT), PHS_OUT(L_OUT), AMP_OUT(L_OUT)
      REAL*8     CNS_AMP_VAL, CNS_AMP_DER, CNS_AMP_DR2, &
     &           CNS_PHS_VAL, CNS_PHS_DER, CNS_PHS_DR2, &
     &           NOD_FUDGE, FRQ_EPS
      INTEGER*4  M_DEG, M_ITER
      PARAMETER  ( M_DEG   = 3 )
      PARAMETER  ( M_ITER  = 8 )
      PARAMETER  ( NOD_FUDGE = 1.D-3 )
      PARAMETER  ( FRQ_EPS   = 1.D-5 )
      REAL*8     ARG(PIM__MCHN), ARG_BEG, ARG_END, ARG_SPL, &
     &           VAL_AMP(PIM__MCHN), WEI_AMP(PIM__MCHN), &
     &           VAL_PHS(PIM__MCHN), WEI_PHS(PIM__MCHN), &
     &           FRQ_0, VAL_REG, SH_AMP, DR_AMP, SH_PHS, DR_PHS, &
     &           WEI_FRQ_AVR_AMP, WEI_SUM_AMP, &
     &           WEI_FRQ_AVR_PHS, WEI_SUM_PHS, &
     &           ARG_AMP_NOD(PIM__MCHN+2), ARG_PHS_NOD(PIM__MCHN+2), &
     &           SPL_AMP_VEC(1-M_DEG:PIM__MCHN+2), SPL_PHS_VEC(1-M_DEG:PIM__MCHN+2), &
     &           POSTFIT_AMP_WRMS, POSTFIT_PHS_WRMS, NOD_AMP_STEP, &
     &           NOD_PHS_STEP, CHE_ALT, AMP_DER, PHS_DER, FRQ_BEG, FRQ_END, &
     &           AMPL_MEAN
      CHARACTER  STR*128, STR1*128
      LOGICAL*1  FL_TEST
      INTEGER*4  J1, J2, J3, J4, J5, J6, J7, J8, J9, J10, &
     &           K_CHN, IV(PIM__MCHN), N_ITER, ITURN, NFRQ_IN, IND_BEG, IER
      INTEGER*4  IAMB(PIM__MUV)
      REAL*8,    EXTERNAL :: EBSPL_VAL_R8, EBSPL_DER_R8
      INTEGER*4  I_LEN, ILEN
!
      CNS_AMP_VAL = 100.0D0
      CNS_AMP_DER =   3.0D0/(FRQ_IN(2) - FRQ_IN(1))
      CNS_AMP_DR2 =   1.0D0/(FRQ_IN(2) - FRQ_IN(1))**2
!
      CNS_PHS_VAL = 1.0D2
      CNS_PHS_DER = 3.0D0/(FRQ_IN(2) - FRQ_IN(1))
      CNS_PHS_DR2 = 0.5D0/(FRQ_IN(2) - FRQ_IN(1))**2
!!!!
!      CNS_AMP_VAL = 100.0D0
!      CNS_AMP_DER =  1.0D6/(FRQ_IN(2) - FRQ_IN(1))
!      CNS_AMP_DR2 =  1.0D6/(FRQ_IN(2) - FRQ_IN(1))**2
!!
!      CNS_PHS_VAL = 1.0D2
!      CNS_PHS_DER = 50.0D0/(FRQ_IN(2) - FRQ_IN(1))
!      CNS_PHS_DR2 = 50.0D0/(FRQ_IN(2) - FRQ_IN(1))**2
!
      BPASS_MOD_SPLINE = .FALSE.
      FL_TEST = .FALSE.
      IF ( MODE == 1 ) THEN
           N_ITER = 1
         ELSE IF ( MODE == 2 ) THEN
           N_ITER = M_ITER
         ELSE IF ( MODE == 11 ) THEN
           N_ITER = 1
           FL_TEST = .TRUE.
         ELSE IF ( MODE == 12 ) THEN
           N_ITER = M_ITER
           FL_TEST  = .TRUE.
      END IF
!
      IF ( LNOD_AMP > L_IN-M_DEG-1 ) THEN
           CALL CLRCH ( STR )
           CALL CLRCH ( STR1 )
           CALL INCH  ( LNOD_AMP, STR  )
           CALL INCH  ( L_IN,    STR1 )
           CALL ERR_LOG ( 5381, IUER, 'BPASS_MOD_SPLINE', 'The number of nodes '// &
     &         'for spline model for the amplitude is too high: '//STR(1:I_LEN(STR))// &
     &         ' for fitting into '//STR1(1:I_LEN(STR1))//' channels. Please '// &
     &         'reduce it.' )
           RETURN
      END IF
      IF ( LNOD_PHS > L_IN-1 ) THEN
           CALL CLRCH ( STR  )
           CALL CLRCH ( STR1 )
           CALL INCH  ( LNOD_PHS, STR  )
           CALL INCH  ( L_IN,    STR1 )
           CALL ERR_LOG ( 5381, IUER, 'BPASS_MOD_SPLINE', 'The number of nodes '// &
     &         'for spline model for the phase is too high: '//STR(1:I_LEN(STR))// &
     &         ' for fitting into '//STR1(1:I_LEN(STR1))//' channels. Please '// &
     &         'reduce it.' )
           RETURN
      END IF
!
! --- We can run several iterations for phase ambiguity resolution
!
      NFRQ_IN = 0
      FRQ_BEG = 1.001D20
      AMPL_MEAN = 0.0D0
      DO 410 J1=1,L_IN
         IF ( AMP_IN(J1) > BPS_AMP_MIN ) THEN
              IF ( FRQ_BEG > 1.D20 ) THEN
                   FRQ_BEG = FRQ_IN(J1)  - FRQ_IN(1)
                   IND_BEG = J1
              END IF
              FRQ_END = FRQ_IN(J1)
              NFRQ_IN = NFRQ_IN + 1
              ARG(NFRQ_IN) = FRQ_IN(J1) - FRQ_IN(1)
              AMPL_MEAN = AMPL_MEAN + AMP_IN(J1)
         END IF
 410  CONTINUE 
!
      IF ( NFRQ_IN < 2 ) THEN
          PHS_MOD = 10.0D0*PI2
          PHS_OUT = 10.0D0*PI2
          AMP_MOD = 10.0D0
          AMP_OUT = 10.0D0
          CALL ERR_LOG ( 0, IUER )
          RETURN 
      END IF
      ARG_BEG = ARG(1)
      ARG_END = ARG(NFRQ_IN)
      AMPL_MEAN = AMPL_MEAN/NFRQ_IN
!
      DO 420 J2=1,N_ITER
         WEI_FRQ_AVR_AMP = 0.0
         WEI_SUM_AMP     = 0.0
         WEI_FRQ_AVR_PHS = 0.0
         WEI_SUM_PHS     = 0.0
!
! ------ Collect arguments, values and weights for both amplitude and phase
!
         K_CHN = 0
         IAMB  = 0
         DO 430 J3=1,L_IN
            VAL_PHS(J3) = PHS_IN(J3)
            WEI_PHS(J3) = AMP_IN(J3)
            VAL_AMP(J3) = AMP_IN(J3)
!
! --------- Important: if the amplitude less than some limit, it is considered
! --------- that that spectral channels is masked out and its weight is set to
! --------- zero
!
            IF ( AMP_IN(J3) > BPS_AMP_MIN ) THEN
                 WEI_AMP(J3) = 1.0
                 K_CHN = K_CHN + 1
                 IV(J3) = 1
               ELSE
                 WEI_AMP(J3) = 0.0
                 WEI_PHS(J3) = 0.0
                 IV(J3) = 1
            END IF
            IF ( MODE == 2 ) THEN
                 IF ( J3 > 1 ) THEN
                      ITURN = IDNINT( (VAL_PHS(J3) - VAL_PHS(J3-1))/PI2 )
                      IAMB(J3) = IAMB(J3) + ITURN
                      IF ( ITURN .NE. 0 ) BPASS_MOD_SPLINE = .TRUE.
                      VAL_PHS(J3) = VAL_PHS(J3) - ITURN*PI2
                 END IF
            END IF
            IF ( WEI_PHS(J3) > BPS_AMP_MIN ) THEN
                 WEI_SUM_AMP     = WEI_SUM_AMP + WEI_AMP(J3)
                 WEI_FRQ_AVR_AMP = WEI_FRQ_AVR_AMP + WEI_AMP(J3)**2
                 WEI_SUM_PHS     = WEI_SUM_PHS + WEI_PHS(J3)
                 WEI_FRQ_AVR_PHS = WEI_FRQ_AVR_PHS + WEI_PHS(J3)**2
            END IF
 430     CONTINUE
!
! ------ Compute average weight for both amplitude and phase
!
         IF ( K_CHN > 0 ) THEN
              WEI_FRQ_AVR_AMP = DSQRT ( WEI_FRQ_AVR_AMP/K_CHN )
              WEI_FRQ_AVR_PHS = DSQRT ( WEI_FRQ_AVR_PHS/K_CHN )
            ELSE
              WEI_FRQ_AVR_AMP = 0.0D0
              WEI_FRQ_AVR_PHS = 0.0D0
         END IF
!
         IF ( K_CHN .LE. LNOD_AMP  .OR. &
     &        K_CHN .LE. LNOD_PHS       ) THEN
!
! ----------- Too few channels for computing the spline polynomial
! ----------- either for amplitude or for phase. Set abnormal phase and
! ----------- zero amplitude in order bpass_sta_acc to notice this
! ----------- abnormality and reject the observation
!
              DO 440 J4=1,L_IN
                 PHS_MOD(J4) =  10.0D0*PI2
                 AMP_MOD(J4) = -10.0D0
 440          CONTINUE
              DO 450 J5=1,L_OUT
                 PHS_OUT(J5) =  10.0D0*PI2
                 AMP_OUT(J5) = -10.0D0
 450          CONTINUE
!
              CALL ERR_LOG ( 0, IUER )
              RETURN
         END IF
!
         IF ( J2 == 1 ) THEN
!
! ----------- NB: We have two extra nodes, one arg_beg and another after arg_end
!
              NOD_AMP_STEP = (ARG_END - ARG_BEG)/(LNOD_AMP -1)
              DO 460 J6=1,LNOD_AMP
                 IF ( J6 == 1 ) THEN
                      ARG_AMP_NOD(J6+1) = ARG_BEG + (J6-0.5)*NOD_AMP_STEP
                   ELSE IF ( J6 == LNOD_AMP ) THEN
                      ARG_AMP_NOD(J6+1) = ARG_BEG + (J6-1.5)*NOD_AMP_STEP
                   ELSE
                      ARG_AMP_NOD(J6+1) = ARG_BEG + (J6-1.0)*NOD_AMP_STEP
                 END IF
      460     CONTINUE 
              ARG_AMP_NOD(1)          = FRQ_OUT(1)     - FRQ_IN(1) - NOD_FUDGE*NOD_AMP_STEP
              ARG_AMP_NOD(LNOD_AMP+2) = FRQ_OUT(L_OUT) - FRQ_IN(1) + NOD_FUDGE*NOD_AMP_STEP
!
! ----------- Compute coefficients of the spline approximation for amplitude
!
              IF ( WEI_SUM_AMP > 0.0 ) THEN
                   CALL ERR_PASS ( IUER, IER )
                   CALL EBSPL_WLSQ_CNS3 ( NFRQ_IN, ARG, VAL_AMP(IND_BEG), WEI_AMP(IND_BEG), &
          &                               LNOD_AMP+2, M_DEG, ARG_AMP_NOD, SPL_AMP_VEC, &
     &                                    CNS_AMP_VAL, CNS_AMP_DER, CNS_AMP_DR2, &
     &                                    POSTFIT_AMP_WRMS, IER )
                   IF ( IER .NE. 0 ) THEN
                        CALL ERR_LOG ( 5886, IUER, 'BPASS_MOD_SPLINE', 'Failure '// &
          &                 'in attempt to perform expansion into the spline basis '// &
          &                 'for the amplitude vector' )
                        RETURN
                   END IF
                 ELSE IF ( WEI_SUM_AMP .LE. 0.0 ) THEN
                   SPL_AMP_VEC = 0.0D0
              END IF
         END IF
!
         NOD_PHS_STEP = (ARG_END - ARG_BEG)/(LNOD_PHS -1)
         DO 470 J7=1,LNOD_PHS
            IF ( J7 == 1 ) THEN
                 ARG_PHS_NOD(J7+1) = ARG(1) + (J7-0.5)*NOD_PHS_STEP
               ELSE IF ( J7 == LNOD_PHS ) THEN
                 ARG_PHS_NOD(J7+1) = ARG(1) + (J7-1.5)*NOD_PHS_STEP
               ELSE
                 ARG_PHS_NOD(J7+1) = ARG(1) + (J7-1.0)*NOD_PHS_STEP
            END IF
 470     CONTINUE 
         ARG_PHS_NOD(1)          = FRQ_OUT(1)     - FRQ_IN(1) - NOD_FUDGE*NOD_AMP_STEP
         ARG_PHS_NOD(LNOD_PHS+2) = FRQ_OUT(L_OUT) - FRQ_IN(1) + NOD_FUDGE*NOD_AMP_STEP
         IF ( WEI_SUM_PHS > 0.0 ) THEN
              IF ( FL_TEST ) THEN
                   WRITE ( 6, * ) 'NFRQ_IN= ', NFRQ_IN, ' ARG= ', ARG(1:NFRQ_IN)
                   WRITE ( 6, * ) 'NFRQ_IN= ', NFRQ_IN, ' VAL_PHAS= ', VAL_PHS(1:NFRQ_IN)
                   WRITE ( 6, * ) 'LNOD_PHS+2=', LNOD_PHS+2, ' ARG_PHS_MOD= ', ARG_PHS_NOD(1:LNOD_PHS+2) 
              END IF
              CALL ERR_PASS ( IUER, IER )
              CALL EBSPL_WLSQ_CNS3 ( NFRQ_IN, ARG, VAL_PHS(IND_BEG), WEI_PHS(IND_BEG), &
     &                               LNOD_PHS+2, M_DEG, ARG_PHS_NOD, SPL_PHS_VEC, &
     &                               CNS_PHS_VAL/AMPL_MEAN, CNS_PHS_DER/AMPL_MEAN, &
     &                               CNS_PHS_DR2/AMPL_MEAN, POSTFIT_PHS_WRMS, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 5887, IUER, 'BPASS_MOD_SPLINE', 'Failure '// &
     &                 'in attempt to perform expansion into the spline basis '// &
     &                 'for the phase vector' )
                   RETURN
              END IF
            ELSE IF ( WEI_SUM_PHS .LE. 0.0 ) THEN
              SPL_PHS_VEC = 0.0D0
         END IF
!
         IF ( MODE == 2 ) THEN
!
! ----------- Resolve phase delay ambiguities in phase
!
              DO 480 J8=1,L_IN
                 IF ( FRQ_IN(J8) > ARG_BEG .AND. FRQ_IN(J8) < ARG_END ) THEN
                      IF ( WEI_SUM_PHS > 0.0 ) THEN
                           VAL_REG = EBSPL_VAL_R8 ( LNOD_PHS+2, M_DEG, ARG(J8), &
     &                                              ARG_PHS_NOD, SPL_PHS_VEC )
                         ELSE 
                           VAL_REG = VAL_PHS(J8)
                      END IF
!
                      ITURN = IDNINT( (VAL_PHS(J8) - VAL_REG)/PI2 )
                      IAMB(J8) = IAMB(J8) + ITURN
                      VAL_PHS(J8) = VAL_PHS(J8) - ITURN*PI2
                 END IF
 480         CONTINUE
         END IF
 420  CONTINUE
!
! --- Create the output array of amplitudes and phases in accordance with
! --- with their spline approximation
!
!
! --- Now compute model values that correspond to the input frequency array
!
      DO 490 J9=1,L_IN
          AMP_MOD(J9) = EBSPL_VAL_R8 ( LNOD_AMP+2, M_DEG, DBLE(FRQ_IN(J9) - FRQ_IN(1)), &
     &                                  ARG_AMP_NOD, SPL_AMP_VEC )
!
! ------- ... then phase
!
          PHS_MOD(J9) = EBSPL_VAL_R8 ( LNOD_PHS+2, M_DEG, DBLE(FRQ_IN(J9) - FRQ_IN(1)), &
     &                                  ARG_PHS_NOD, SPL_PHS_VEC )
 490 CONTINUE
!
      DO 4100 J10=1,L_OUT
!
! ------ First, compute the output amplitude
! 
         AMP_OUT(J10) = EBSPL_VAL_R8 ( LNOD_AMP+2, M_DEG, DBLE(FRQ_OUT(J10)- FRQ_IN(1)), &
     &                                ARG_AMP_NOD, SPL_AMP_VEC )
!
! ------ ... then phase
!
         PHS_OUT(J10) = EBSPL_VAL_R8 ( LNOD_PHS+2, M_DEG, DBLE(FRQ_OUT(J10)- FRQ_IN(1)), &
     &                                ARG_PHS_NOD, SPL_PHS_VEC )
 4100 CONTINUE
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  FUNCTION   BPASS_MOD_SPLINE  !#!#
