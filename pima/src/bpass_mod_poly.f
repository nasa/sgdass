      FUNCTION   BPASS_MOD_POLY ( MODE, BPS_AMP_MIN, DEG_AMP, DEG_PHS, &
     &                            L_IN,  FRQ_IN,  PHS_IN,  AMP_IN, &
     &                            PHS_MOD, AMP_MOD, &
     &                            L_OUT, FRQ_OUT, PHS_OUT, AMP_OUT, IUER )
! ************************************************************************
! *                                                                      *
! *   Axilliary routine  BPASS_MOD_POLY approximates an array of input   *
! *   phases and amplitudes using expansion of the Legendre polynomials. *
! *   Constraints on the the mean value, the first and second            *
! *   derivatives are applied during computation of the expansion.       *
! *   The output is written into arrays PHS_MOD and AMP_MOD that         *
! *   correspond to the array of input frequencies FRQ_IN and in the     *
! *   arrays PHS_OUT, AMP_OUT that correspond to frequencies FRQ_OUT.    *
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
! *     DEG_AMP ( INTEGER*4  ) -- degree of the Legendre polynomial      *
! *                               for amplitudes.                        *
! *     DEG_PHS ( INTEGER*4  ) -- degree of the Legendre polynomial      *
! *                               for phases.                            *
! *        L_IN ( INTEGER*4  ) -- The number of points in the input      *
! *                               arrays.                                *
! *      FRQ_IN ( REAL*8     ) -- Input array of sky frequencies.        *
! *                               Dimension: L_IN, unit: Hz.             *
! *      PHS_IN ( REAL*8     ) -- Input array of phases.                 *
! *                               Dimension: L_IN, unit: rad.            *
! *      AMP_IN ( REAL*8     ) -- Input array of amplitudes.             *
! *                               Dimension: L_IN, unit: dimensionless.  *
! *       L_OUT ( INTEGER*4  ) -- The number of points in the output     *
! *                               arrays.                                *
! *                                                                      *
! * _________________________ Output parameters: _______________________ *
! *                                                                      *
! *     PHS_MOD ( REAL*8     ) -- Modeled phase. Dimension: L_IN.        *
! *                               Unit: rad.                             *
! *     AMP_MOD ( REAL*8     ) -- Modeled amplitude. Dimension: L_IN.    *
! *                               Unit: rad.                             *
! *     FRQ_OUT ( REAL*8     ) -- Output array of sky frequencies.       *
! *                               Dimension: L_IN, unit: Hz.             *
! *     PHS_OUT ( REAL*8     ) -- Modeled phase. Dimension: L_OUT.       *
! *                               Unit: rad.                             *
! *     AMP_OUT ( REAL*8     ) -- Modeled amplitude. Dimension: L_OUT.   *
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
! * ### 22-MAY-2006  BPASS_MOD_POLY  v5.1 (c) L. Petrov  2021.07.17  ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'astro_constants.i'
      INCLUDE   'pima.i'
      LOGICAL*4  BPASS_MOD_POLY
      INTEGER*4  MODE, DEG_AMP, DEG_PHS, L_IN, L_OUT, IUER
      REAL*4     FRQ_IN(L_IN),   PHS_IN(L_IN),   AMP_IN(L_IN), &
     &           PHS_MOD(L_IN), AMP_MOD(L_IN), &
     &           FRQ_OUT(L_OUT), PHS_OUT(L_OUT), AMP_OUT(L_OUT)
      REAL*8     BPS_AMP_MIN
      REAL*8     ARG(PIM__MCHN), ARG_BEG, ARG_END, &
     &           VAL_AMP(PIM__MCHN), WEI_AMP(PIM__MCHN), &
     &           VAL_PHS(PIM__MCHN), WEI_PHS(PIM__MCHN), &
     &           FRQ_BEG, FRQ_END, VAL_REG, SH_AMP, DR_AMP, SH_PHS, DR_PHS, &
     &           POL_COEF_AMP(0:PIM__MPOL), &
     &           POL_COEF_PHS(0:PIM__MPOL), &
     &           POL_COEF_ERR(0:PIM__MPOL), &
     &           WEI_FRQ_AVR_AMP, WEI_SUM_AMP, &
     &           WEI_FRQ_AVR_PHS, WEI_SUM_PHS, PH_RATE, PHS_DIF, &
     &           CNS_AMP_VAL, CNS_AMP_DER, CNS_AMP_DR2, &
     &           CNS_PHS_VAL, CNS_PHS_DER, CNS_PHS_DR2, AMPL_MEAN
      CHARACTER  STR*128, STR1*128
      REAL*8       FRQ_EPS   
      INTEGER*4    M_ITER
      PARAMETER  ( M_ITER    = 8 )
      PARAMETER  ( FRQ_EPS   = 1.D-5 )
      INTEGER*4  J1, J2, J3, J4, J5, J6, J7, J8, J9, J10, J11, J12, J13, J14, &
     &           K_CHN, IV(PIM__MCHN), N_ITER, NFRQ_IN, ITURN, IND_BEG, IER
      INTEGER*4  IAMB(PIM__MUV)
      REAL*4,    EXTERNAL :: PHS_AMP_RATE_R4
      REAL*8,    EXTERNAL :: LEGENDRE_POL, LEGENDRE_VAL
      INTEGER*4  I_LEN, ILEN
!
      CNS_AMP_VAL = 30.0D0
      CNS_AMP_DER =  3.0D0/(FRQ_IN(2) - FRQ_IN(1))
      CNS_AMP_DR2 =  1.0D0/(FRQ_IN(2) - FRQ_IN(1))**2
!
      CNS_PHS_VAL = 300.0D0
      CNS_PHS_DER =  30.0D0/(FRQ_IN(2) - FRQ_IN(1))
      CNS_PHS_DR2 =   0.5D0/(FRQ_IN(2) - FRQ_IN(1))**2
!
      BPASS_MOD_POLY = .FALSE.
      IF ( MODE == 1 ) THEN
           N_ITER = 1
         ELSE IF ( MODE == 2 ) THEN
           N_ITER = M_ITER
      END IF
!
      IF ( DEG_AMP > L_IN-1 ) THEN
           CALL CLRCH ( STR )
           CALL CLRCH ( STR1 )
           CALL INCH  ( DEG_AMP, STR  )
           CALL INCH  ( L_IN,    STR1 )
           CALL ERR_LOG ( 5881, IUER, 'BPASS_MOD_POLY', 'Degree of the '// &
               'poynomial for the amplitude is too high: '//STR(1:I_LEN(STR))// &
     &         ' for fitting into '//STR1(1:I_LEN(STR1))//' channels. Please '// &
     &         'reduce it.' )
           RETURN
      END IF
      IF ( DEG_PHS > L_IN-1 ) THEN
           CALL CLRCH ( STR  )
           CALL CLRCH ( STR1 )
           CALL INCH  ( DEG_PHS, STR  )
           CALL INCH  ( L_IN,    STR1 )
           CALL ERR_LOG ( 5882, IUER, 'BPASS_MOD_POLY', 'Degree of the '// &
               'poynomial for the phase is too high: '//STR(1:I_LEN(STR))// &
     &         ' for fitting into '//STR1(1:I_LEN(STR1))//' channels. Please '// &
     &         'reduce it.' )
           RETURN
      END IF
!
! --- We can run several iterations for phase ambiguity resolution
!
      FRQ_BEG = 1.001D20
      NFRQ_IN = 0
      AMPL_MEAN = 0.0D0 
      DO 410 J1=1,L_IN
         IF ( AMP_IN(J1) > BPS_AMP_MIN ) THEN
              IF ( FRQ_BEG > 1.D20 ) THEN
                   FRQ_BEG = FRQ_IN(J1)  - FRQ_IN(1)
                   IND_BEG = J1
              END IF
              FRQ_END      = FRQ_IN(J1)
              NFRQ_IN      = NFRQ_IN + 1
              ARG(NFRQ_IN) = FRQ_IN(J1) - FRQ_IN(1)
              AMPL_MEAN    = AMPL_MEAN + AMP_IN(J1)
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
!
      ARG_BEG = ARG(1)       - FRQ_EPS*(ARG(2) - ARG(1))
      ARG_END = ARG(NFRQ_IN) + FRQ_EPS*(ARG(2) - ARG(1))
      AMPL_MEAN = AMPL_MEAN/NFRQ_IN
!
! --- Compute phase rate
!
      PH_RATE = PHS_AMP_RATE_R4 ( L_IN, FRQ_IN, PHS_IN, AMP_IN, %VAL(0), IER )
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
!
! --------- NB: we subtract phase rate in order to facilitate ambiguity resolution
!
            VAL_PHS(J3) = PHS_IN(J3) - PH_RATE*(FRQ_IN(J3) - FRQ_IN(1))
            WEI_PHS(J3) = AMP_IN(J3)
            VAL_AMP(J3) = AMP_IN(J3)
!
! --------- Important: if the amplitude less than some limit, it is considered
! --------- that that spectral channels is masked out
!
            IF ( AMP_IN(J3) > BPS_AMP_MIN ) THEN
                 WEI_AMP(J3) = 1.0
                 K_CHN = K_CHN + 1
                 IV(J3) = 1
               ELSE
                 WEI_AMP(J3) = 0.0
                 IV(J3) = 0
            END IF
            IF ( MODE == 2 ) THEN
                 IF ( J3 > 1 ) THEN
                      ITURN = IDNINT( (VAL_PHS(J3) - VAL_PHS(J3-1))/PI2 )
                      IAMB(J3) = IAMB(J3) + ITURN
                      IF ( ITURN .NE. 0 ) BPASS_MOD_POLY = .TRUE.
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
         IF ( K_CHN .LE. DEG_AMP  .OR. &
     &        K_CHN .LE. DEG_PHS       ) THEN
!
! ----------- Too few channels for computing the Legendre polynomial
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
! ------ Compute coefficients of the polynomial approximation for amplitude
!
         IF ( ( DEG_AMP == 0 .OR.  DEG_AMP == 1 ) .AND. WEI_SUM_AMP > 0.0 ) THEN
              CALL ERR_PASS ( IUER, IER )
              CALL REGRW8   ( NFRQ_IN, ARG, VAL_AMP(IND_BEG), &
     &                        WEI_AMP(IND_BEG), IV(IND_BEG), DR_AMP, SH_AMP, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 5884, IUER, 'BPASS_MOD_POLY', 'Failure '// &
     &                 'in REGRW8' )
                   RETURN
              END IF
            ELSE IF ( DEG_AMP > 1 .AND. WEI_SUM_AMP > 0.0 ) THEN
              CALL ERR_PASS ( IUER, IER )
              CALL LEGENDRE_REGR_CNS ( NFRQ_IN, ARG, VAL_AMP(IND_BEG), WEI_AMP(IND_BEG), &
     &                                 DEG_AMP, CNS_AMP_VAL, CNS_AMP_DER, CNS_AMP_DR2, &
     &                                 POL_COEF_AMP, POL_COEF_ERR, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( J2, STR )
                   CALL ERR_LOG ( 5885, IUER, 'BPASS_MOD_POLY', 'Failure '// &
     &                 'in LEGENDRE_REGR, iteration '//STR )
                   RETURN
              END IF
            ELSE IF ( WEI_SUM_AMP .LE. 0.0 ) THEN
              CALL NOUT_R8 ( PIM__MPOL+1, POL_COEF_AMP )
         END IF
!
! ------ Compute coefficients of the polynomial approximation for phase
!
         IF ( ( DEG_PHS == 0 .OR.  DEG_PHS == 1 ) .AND. WEI_SUM_PHS > 0.0 ) THEN
              CALL ERR_PASS ( IUER, IER )
              CALL REGRW8   ( NFRQ_IN, ARG, VAL_PHS(IND_BEG), WEI_PHS(IND_BEG), &
     &                        IV(IND_BEG), DR_PHS, SH_PHS, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 5886, IUER, 'BPASS_MOD_POLY', 'Failure '// &
     &                 'in REGRW8' )
                   RETURN
              END IF
            ELSE IF ( DEG_PHS > 1 .AND. WEI_SUM_PHS > 0.0 ) THEN
              CALL ERR_PASS ( IUER, IER )
              CALL LEGENDRE_REGR_CNS ( NFRQ_IN, ARG, VAL_PHS(IND_BEG), WEI_PHS(IND_BEG), &
     &                                 DEG_PHS, CNS_PHS_VAL/AMPL_MEAN, CNS_PHS_DER/AMPL_MEAN, &
     &                                 CNS_PHS_DR2/AMPL_MEAN, POL_COEF_PHS, &
     &                                 POL_COEF_ERR, IER )
              IF ( IER .NE. 0 ) THEN
                   IF ( IER .NE. 1721 ) THEN
                        write ( 6, * ) 'l_in    = ', l_in ! %%%
                        write ( 6, * ) 'arg     = ', arg(1:l_in)  ! %%%
                        write ( 6, * ) 'val_phs = ', val_phs(1:l_in) ! %%%
                        write ( 6, * ) 'wei_phs = ', wei_phs(1:l_in) ! %%%
                   END IF
                   CALL ERR_LOG ( 5887, IUER, 'BPASS_MOD_POLY', 'Failure '// &
     &                 'in LEGENDRE_REGR' )
                   RETURN
              END IF
            ELSE IF ( WEI_SUM_PHS .LE. 0.0 ) THEN
              CALL NOUT_R8 ( PIM__MPOL+1, POL_COEF_PHS )
         END IF
!
         DO 460 J6=1,NFRQ_IN
            IF ( MODE == 2 ) THEN
                 VAL_REG = VAL_PHS(J6)
!
! -------------- Resolve phase ambiguity
!
                 IF ( DEG_PHS == 0 ) THEN
                      VAL_REG = SH_PHS + DR_PHS*(ARG_END+ARG_BEG)/2.0D0
                    ELSE IF ( DEG_PHS == 1 ) THEN
                      VAL_REG = SH_PHS + DR_PHS*(ARG(J6)-ARG_BEG)
                    ELSE IF ( DEG_PHS > 1 ) THEN
                      VAL_REG = 0.0D0
                      DO 470 J7=0,DEG_PHS
                         VAL_REG = VAL_REG + POL_COEF_PHS(J7)* &
     &                             LEGENDRE_POL ( J7, ARG_BEG, ARG_END, ARG(J6) )
 470                  CONTINUE
                 END IF
!
                 ITURN = IDNINT( (VAL_PHS(J6+IND_BEG-1) - VAL_REG)/PI2 )
                 IAMB(J6+IND_BEG-1) = IAMB(J6+IND_BEG-1) + ITURN
                 VAL_PHS(J6+IND_BEG-1) = VAL_PHS(J6+IND_BEG-1) - ITURN*PI2
           END IF
 460    CONTINUE
 420  CONTINUE
!
! --- Create the output array of amplitudes and phases in accordance with
! --- with their polynomal approximation
!
      DO 490 J9=1,L_OUT
!
! ------ First create the output amplitude
!
         IF ( DEG_AMP == 0 ) THEN
              AMP_OUT(J9) = SH_AMP + DR_AMP*(ARG_END+ARG_BEG)/2.0D0
            ELSE IF ( DEG_AMP == 1 ) THEN
              AMP_OUT(J9) = SH_AMP + DR_AMP*(FRQ_OUT(J9)-FRQ_IN(1)-ARG_BEG)
            ELSE IF ( DEG_AMP > 1 ) THEN
              AMP_OUT(J9) = LEGENDRE_VAL ( DEG_AMP, ARG_BEG, ARG_END, &
     &                                     DBLE(FRQ_OUT(J9)- FRQ_IN(1)), POL_COEF_AMP )
         END IF
!
! ----------- ... then phase
!
         IF ( DEG_PHS == 0 ) THEN
              PHS_OUT(J9) = PH_RATE*(FRQ_OUT(J9) - FRQ_IN(1)) + SH_PHS 
            ELSE IF ( DEG_PHS == 1 ) THEN
              PHS_OUT(J9) = PH_RATE*(FRQ_OUT(J9) - FRQ_IN(1)) + &
     &                               SH_PHS + DR_PHS*(FRQ_OUT(J9)-FRQ_IN(1)-ARG_BEG)
            ELSE IF ( DEG_PHS > 1 ) THEN
              PHS_OUT(J9) =  PH_RATE*(FRQ_OUT(J9) - FRQ_IN(1)) + &
     &                                LEGENDRE_VAL ( DEG_PHS, ARG_BEG, ARG_END, &
     &                                               DBLE(FRQ_OUT(J9)- FRQ_IN(1)), POL_COEF_PHS )
         END IF
         ITURN = IDNINT( PHS_OUT(J9)/PI2 )
         PHS_OUT(J9) = PHS_OUT(J9) - PI2*ITURN
 490  CONTINUE
!
! --- Create the model array of amplitudes and phases in accordance with
! --- with their polynomal approximation
!
      DO 4120 J12=1,L_IN
         IF ( FRQ_OUT(J12) - FRQ_IN(1) > ARG_BEG .AND. FRQ_OUT(J12) - FRQ_IN(1) < ARG_END ) THEN
!
! ----------- First create the output amplitude
!
              IF ( DEG_AMP == 0 ) THEN
                   AMP_MOD(J12) = SH_AMP + DR_AMP*(ARG(L_IN)+ARG_BEG)/2.0D0
                 ELSE IF ( DEG_AMP == 1 ) THEN
                   AMP_MOD(J12) = SH_AMP + DR_AMP*(FRQ_OUT(J12)-ARG_BEG)
                 ELSE IF ( DEG_AMP > 1 ) THEN
                   AMP_MOD(J12) = LEGENDRE_VAL ( DEG_AMP, ARG_BEG, ARG_END, &
     &                                           DBLE(FRQ_OUT(J12)- FRQ_IN(1)), POL_COEF_AMP )
              END IF
!
! ----------- ... then phase
!
              IF ( DEG_PHS == 0 ) THEN
                   PHS_MOD(J12) = PH_RATE*(FRQ_IN(J12) - FRQ_IN(1)) + SH_PHS 
                ELSE IF ( DEG_PHS == 1 ) THEN
                  PHS_MOD(J12) = PH_RATE*(FRQ_IN(J12) - FRQ_IN(1)) + &
     &                                    SH_PHS + DR_PHS*(FRQ_OUT(J12)-ARG_BEG)
                ELSE IF ( DEG_PHS > 1 ) THEN
                  PHS_MOD(J12) =  PH_RATE*(FRQ_IN(J12) - FRQ_IN(1)) + &
     &                               LEGENDRE_VAL ( DEG_PHS, ARG_BEG, ARG_END, &
     &                                              DBLE(FRQ_OUT(J12)- FRQ_IN(1)), POL_COEF_PHS )
              END IF
              IF ( WEI_PHS(J12) < BPS_AMP_MIN ) THEN
                   PHS_MOD(J12) = 0.0D0
              END IF
              IF ( WEI_AMP(J12) < BPS_AMP_MIN ) THEN
                   AMP_MOD(J12) = 0.0D0
              END IF
              PHS_DIF = PHS_MOD(J12) - PHS_IN(J12)
              ITURN = IDNINT( PHS_DIF/PI2 )
              PHS_MOD(J12) = PHS_MOD(J12) - PI2*ITURN
            ELSE
              AMP_MOD(J12) = 1.0
              PHS_MOD(J12) = 0.0
         END IF
 4120 CONTINUE
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  FUNCTION   BPASS_MOD_POLY  !#!#
