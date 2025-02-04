      SUBROUTINE ERM_CNST ( L_PAR, C_PAR, ERM, CNSTROBJ, IUER ) 
! ************************************************************************
! *                                                                      *
! *   Routine  ERM_CNST  imposes constraints on                          *
! *   a) values of the ERM, its first and second time derivatives        *
! *      at points of knots.                                             * 
! *   b) mean value of the ERM over the specified date range.            *
! *   c) rate of change of the ERM over the specified date range.        *
! *                                                                      *
! *   Restrictions:                                                      *
! *                                                                      *
! *   -- start and end epochs of the interval for mean and rate          *
! *      constraint should be coinside with spline knots. This is        *
! *      enforced in parse_erm_cnt (BATCH) and is not checked here.      *
! *                                                                      *
! *   -- rate and mean constrainst require fudging if the constraint     *
! *      start epoch is within 0 to DEG knots after the ERM interval.    *
! *      This was tested for equi-disstant knots intervals of 2 days     *
! *      and splines of the 3rd degree. It may not work for a general    *
! *      case.                                                           *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *        L_PAR ( INTEGER*4 ) -- The total number of global parameter.  *
! *        C_PAR ( INTEGER*4 ) -- The list of global parameters.         *
! *          ERM ( ERM__TYPE ) -- Derived object defined in              *
! *                               $PSOLVE_ROOT/include/solve.i which     *
! *                               keeps information about estimation of  *
! *                               the Earth Rotation Model.              *
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
! *    CNSTROBJ  ( RECORD    ) -- Object whcih accumulates information   *
! *                               about constraints: names, coefficient  *
! *                               of constraint equations, etc.          *
! *    IUER ( INTEGER*4, OPT ) -- Universal error handler.               *
! *                           Input: switch IUER=0 -- no error messages  *
! *                                  will be generated even in the case  *
! *                                  of error. IUER=-1 -- in the case of *
! *                                  error the message will be put on    *
! *                                  stdout.                             *
! *                           Output: 0 in the case of successful        *
! *                                   completion and non-zero in the     *
! *                                   case of error.                     *
! *                                                                      *
! *  ### 29-JAN-2006    ERM_CNST   v3.1 (c)  L. Petrov  02-OCT-2023 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'solve.i'
      INCLUDE   'cnstr.i'
      INCLUDE   'erm.i'
      TYPE     ( ERM__TYPE   ) :: ERM
      TYPE     ( CNSTR__STRU ) :: CNSTROBJ
      INTEGER*4  L_PAR, IUER
      CHARACTER  C_PAR(L_PAR)*(*)
      CHARACTER  CNS_ABR*8, CNS_ABR_MEAN*8, CNS_ABR_RATE*8, UNIT*7, &
     &           PAR_NAME*20, STR*128
      REAL*8     SIG_MIN, TIM_EPS
      PARAMETER  ( SIG_MIN = 1.D-30 )
      PARAMETER  ( TIM_EPS = 1.0D-6 )
      REAL*8     COEF_EQU, TIM_ARG_EST, TIM_ARG_REF, MID_EPOCH, &
     &           TIM_CNS_BEG, TIM_CNS_END, &
     &           FINT, FMOM, MEAN_RHS, RATE_RHS, TIM_MID, TIM_RANGE, KNOT_MID, &
     &           MAT_CNS(3), RC, FUDGE_INT, FUDGE_RAT, ERM_VAL, FCT, &
     &           TIM_RAN, S, Q, R, X2(2)
      REAL*8,    ALLOCATABLE :: EQU_CNS_MEAN(:), EQU_CNS_RATE(:)
      INTEGER*4  J1, J2, J3, J4, J5, J6, J7, J8, J9, J10, &
     &           IND_EQU, IND_PAR, KNOT_BEG, KNOT_END, &
     &           IND_PARS(1-ERM__MSPL:ERM__MKNOT,3), NTIM_CNS, &
     &           DEBUG_VAR, IER
      REAL*8     BSPL_VAL, BSPL_DER, BSPL_DR2
      INTEGER*4, EXTERNAL :: LTM_DIF, IXMN8
      REAL*8     BSPL_INT1_FULL, BSPL_MOM1_FULL
      CHARACTER, EXTERNAL :: MJDSEC_TO_DATE*30
!
      DEBUG_VAR = 0 ! 0 for normal runs, 1 or 2 for debugging
      DO 410 J1=1,3
         DO 420 J2=1-ERM%DEGREE(J1),ERM%NKNOTS(J1)-1
!
! --------- Get the parameter name
!
            CALL CLRCH ( PAR_NAME )
            WRITE ( PAR_NAME, '("ERM ",I1,4X,I5,5X)' ) J1, J2
!
! --------- ... and search it in the parameter list
!
            IND_PARS(J2,J1) = LTM_DIF ( 0, L_PAR, C_PAR, PAR_NAME )
 420     CONTINUE 
 410  CONTINUE 
      IF ( DEBUG_VAR .GE. 1 ) THEN
           WRITE ( 6, * ) 'ERM_CNST-91 ERM%NKNOTS(*):       ', ERM%NKNOTS(1:3)
           WRITE ( 6, * ) 'ERM_CNST-92 ERM%TIM(1,*):        ', ERM%TIM(1,1:3)
           WRITE ( 6, * ) 'ERM_CNST-93 ERM%CNS_MEAN_SIGMA = ', ERM%CNS_MEAN_SIGMA
           WRITE ( 6, * ) 'ERM_CNST-94 ERM%CNS_RATE_SIGMA = ', ERM%CNS_RATE_SIGMA 
           WRITE ( 6, * ) 'ERM_CNST-95 ERM%CNS_MEAN_RTP   = ', ERM%CNS_MEAN_RTP
           WRITE ( 6, * ) 'ERM_CNST-96 ERM%CNS_RATE_RTP   = ', ERM%CNS_RATE_RTP
      END IF
!
! --- Inpise constratins on the value, 1st and 2nd derivatives
!
      DO 430 J3=1,3
!
! ------ Cycle over EOP components
!
         MID_EPOCH = ( ERM%TIM(ERM%NKNOTS(J3),J3) + ERM%TIM(1,J3))/2.0D0
         DO 440 J4=0,ERM%DEGREE(J3)
            IF ( ERM%CNS_DER_SIGMA(J4,J3) < SIG_MIN ) GOTO 440
            IF ( J4 > 2 ) THEN
                 CALL ERR_LOG ( 8521, IUER, 'ERM_CNST', 'Trap of internal '// &
     &               'control: request to impose constraints on derivative of '// &
     &               'the EOP modeled with B-spline of a degree higher '// &
     &               'than 2. This case is not supported' )
                 RETURN
            END IF
!
! --------- Insert information about constraint, name, description,
! --------- abbreviation, right hand side, reciprocal weight (sigma), type
!
            CNS_ABR = 'ERM_E%_%' 
            CALL INCH ( J3, CNS_ABR(6:6) )
            CALL INCH ( J4, CNS_ABR(8:8) )
            UNIT = 'rad    '
            IF ( J4 > 0 ) THEN
                 UNIT = 'rad/s^%'
                 CALL INCH ( J4, UNIT(7:7) )
            END IF
!
            DO 450 J5=1,ERM%NKNOTS(J3)-1
               CALL ERR_PASS ( IUER, IER )
               CALL ADDCNS_NAM ( CNS_ABR, J5, 'ERM constraint', UNIT, &
     &                           0.0D0, ERM%CNS_DER_SIGMA(J4,J3), &
     &                          .TRUE., CNSTROBJ, IER )
               IF ( IER .NE. 0 ) THEN
                    CALL ERR_LOG ( 8522, IUER, 'ERM_CNST', 'Error in '// &
     &                  'an attempt to put information about '// &
     &                   CNS_ABR//' constraints into CNSTROBJ' )
                    RETURN
               END IF
!
               CALL ERR_PASS ( IUER, IER ) 
               CALL SET_DYN_CNS ( CNS_ABR, J5, ERM%DEGREE(J3)+1, CNSTROBJ, IER )
               IF ( IER .NE. 0 ) THEN
                    CALL ERR_LOG ( 8523, IUER, 'ERM_CNST', 'Error in '// &
     &                  'an attempt to set status "dynamic" for '// &
     &                   CNS_ABR//' constraints' )
                    RETURN
               END IF
!
               IND_EQU = J5
               TIM_ARG_EST = ERM%TIM(J5,J3)
!
               DO 460 J6=-ERM%DEGREE(J3),0
!
! --------------- Compute constraint equation
!
                  IF ( J4 == 0 ) THEN
                       COEF_EQU = BSPL_VAL ( ERM%NKNOTS(J3), ERM%TIM(1,J3), &
     &                                       ERM%DEGREE(J3), IND_EQU+J6, TIM_ARG_EST ) 
                     ELSE IF ( J4 == 1 ) THEN
                       COEF_EQU = BSPL_DER ( ERM%NKNOTS(J3), ERM%TIM(1,J3), &
     &                                       ERM%DEGREE(J3), IND_EQU+J6, TIM_ARG_EST ) 
                     ELSE IF ( J4 == 2 ) THEN
                       COEF_EQU = BSPL_DR2 ( ERM%NKNOTS(J3), ERM%TIM(1,J3), &
     &                                       ERM%DEGREE(J3), IND_EQU+J6, TIM_ARG_EST ) 
                  END IF
!
! --------------- Search the parameter in the parameter list
!
                  IND_PAR = IND_PARS ( IND_EQU+J6, J3 )
                  IF ( IND_PAR .LE. 0 ) THEN
                       CALL ERR_LOG ( 8524, IUER, 'ERM_CNST', 'Trap of '// &
     &                     'internal control: failure to find paramater '// &
     &                      PAR_NAME )
                       RETURN
                  END IF
!
                  IF ( COEF_EQU .NE. 0.0D0 ) THEN
!
! -------------------- Add constraint equation to the list
!
                       CALL ERR_PASS ( IUER, IER )
                       CALL ADDCNS_EQU ( CNS_ABR, IND_EQU, IND_PAR, COEF_EQU, &
     &                                   .TRUE., CNSTROBJ, IER )
                       IF ( IER .NE. 0 ) THEN
                            WRITE ( 6, * ) 'IND_EQU= ', IND_EQU, ' J5= ', J6, ' J6= ', J6, ' COEF_EQU= ', COEF_EQU
                            CALL ERR_LOG ( 8525, IUER, 'ERM_CNST', 'Failure '// &
     &                          'in putting a coefficient of an equation '// &
     &                          'of '//CNS_ABR//' constraint' )
                            RETURN
                       END IF
                  END IF
 460           CONTINUE 
 450        CONTINUE 
            WRITE ( 23, 110 ) J3, J4, ERM%CNS_DER_SIGMA(J4,J3)
 110        FORMAT ( 'ERMVAL_CNST:  constraint on ERM component ', I1, &
     &               ' degree ', I1, ' with sigma ', 1PD14.7  )
 440     CONTINUE 
!
         IF ( ERM%CNS_MEAN_SIGMA(J3) > 0.0D0 .OR. ERM%CNS_RATE_SIGMA(J3)  > 0.0D0 ) THEN
!
! ----------- Constraints in the mean and the rate of change over the specified
! ----------- interfgal
!
! ----------- Compute time range
!
!
              TIM_RAN  = ((ERM%MJD_END_RANGE_CNS - ERM%MJD_BEG_RANGE_CNS)*86400.0D0 + &
     &                    (ERM%TAI_END_RANGE_CNS - ERM%TAI_BEG_RANGE_CNS))/2.0D0
!
! ----------- Compute the number of time epochs for the mean/rate constraints
!
              NTIM_CNS = ((ERM%MJD_END_RANGE_CNS - ERM%MJD_BEG_RANGE_CNS)*86400.0D0 + &
     &                    (ERM%TAI_END_RANGE_CNS - ERM%TAI_BEG_RANGE_CNS) + TIM_EPS)/ &
     &                     ERM%TIME_CNS_SPAN(J3) + 1
!
! ----------- Compute knots for the first, last, and mid epochs for constraints
!
              KNOT_BEG = IDNINT ( (ERM%MJD_BEG_RANGE_CNS - ERM%MJD_BEG)*86400.0D0 + &
     &                            (ERM%TAI_BEG_RANGE_CNS - ERM%TAI_BEG) )/ &
     &                             ERM%TIME_EST_SPAN(J3) + 1
              KNOT_END = IDNINT ( (ERM%MJD_END_RANGE_CNS - ERM%MJD_BEG)*86400.0D0 + &
     &                            (ERM%TAI_END_RANGE_CNS - ERM%TAI_BEG) )/ &
     &                             ERM%TIME_EST_SPAN(J3) + 1
              KNOT_MID  = ( (ERM%MJD_REF_CNS - ERM%MJD_BEG)*86400.0D0 + &
     &                      (ERM%TAI_REF_CNS - ERM%TAI_BEG) )/ERM%TIME_EST_SPAN(J3)
              IF ( DEBUG_VAR .GE. 1 ) THEN
                   WRITE ( 6, * ) ' ERM_CNST ICMP = ', J3
                   WRITE ( 6, * ) ' ERM_BEG      = ', ERM%MJD_BEG, ERM%TAI_BEG
                   WRITE ( 6, * ) ' ERM_END      = ', ERM%MJD_END, ERM%TAI_END
                   WRITE ( 6, * ) ' BEG_RANGE_CNS= ', ERM%MJD_BEG_RANGE_CNS, ERM%TAI_BEG_RANGE_CNS
                   WRITE ( 6, * ) ' END_RANGE_CNS= ', ERM%MJD_END_RANGE_CNS, ERM%TAI_END_RANGE_CNS
                   WRITE ( 6, * ) ' TIM_RAN      = ', TIM_RAN
                   WRITE ( 6, * ) ' REF_CNS      = ', ERM%MJD_REF_CNS, ERM%TAI_REF_CNS
                   WRITE ( 6, * ) ' KNOT_BEG     = ', KNOT_BEG
                   WRITE ( 6, * ) ' KNOT_END     = ', KNOT_END
                   WRITE ( 6, * ) ' KNOT_MID     = ', KNOT_MID
                   WRITE ( 6, * ) ' NTIM_CNS     = ', NTIM_CNS
                   WRITE ( 6, * ) ' ERM%NKNOTS   = ', ERM%NKNOTS(J3)
                   WRITE ( 6, * ) ' TIM_1ST      = ', ERM%TIM(1,J3)
                   WRITE ( 6, * ) ' TIM_LAST     = ', ERM%TIM(ERM%NKNOTS(J3),J3)
                   WRITE ( 6, * ) ' CNS_MEAN_SIG = ', ERM%CNS_MEAN_SIGMA(J3)
                   WRITE ( 6, * ) ' CNS_RATE_SIG = ', ERM%CNS_RATE_SIGMA(J3)
              END IF
!
! ----------- Compute coefficients of the equations that connect EOP shifts/drifts
! ----------- with constriaint sums
!
              S = 0.0D0
              Q = 0.0D0
              DO 470 J7=1,NTIM_CNS
!
! -------------- Constraint time epoch -- time elapsed from the J7th constraint
! -------------- epoch since the constraint reference epoch
!
                 TIM_ARG_REF = (J7-1)*ERM%TIME_CNS_SPAN(J3) + &
     &                         (ERM%MJD_BEG_RANGE_CNS - ERM%MJD_REF_CNS)*86400.0D0 + &
     &                         (ERM%TAI_BEG_RANGE_CNS - ERM%TAI_REF_CNS)
!
! -------------- S -- The sum of normalized time epochs
! -------------- Q -- The sum of normalized time epoch squares
!
                 S = S +  TIM_ARG_REF/TIM_RAN
                 Q = Q + (TIM_ARG_REF/TIM_RAN)**2
                 IF ( DEBUG_VAR .GE. 1 ) THEN
                      WRITE ( 6, * ) 'ERM_CNST J7= ', J7, ' TIM_ARG_REF= ', TIM_ARG_REF, &
     &                               ' TIM_ARG_REF/TIM_RAN= ', SNGL(TIM_ARG_REF/TIM_RAN)
                 END IF
 470          CONTINUE 
!
! ----------- Define EOP mean constraint
!
              CNS_ABR_MEAN = 'ERM_E%_M' 
              MEAN_RHS = ( NTIM_CNS*ERM%CNS_MEAN_RTP(J3) + S*ERM%CNS_RATE_RTP(J3)*TIM_RAN )
              CALL INCH ( J3, CNS_ABR_MEAN(6:6) )
              CALL ERR_PASS ( IUER, IER )
              CALL ADDCNS_NAM ( CNS_ABR_MEAN, 1, 'ERM mean constraint', 'rad', &
     &                          MEAN_RHS, ERM%CNS_MEAN_SIGMA(J3), &
     &                          .TRUE., CNSTROBJ, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 8526, IUER, 'ERM_CNST', 'Error in '// &
     &                 'an attempt to put information about '// &
     &                  CNS_ABR//' constraints into CNSTROBJ' )
                   RETURN
              END IF
!
! ----------- Define RATE mean constraint
!
              CNS_ABR_RATE = 'ERM_E%_R' 
              RATE_RHS = ( S*ERM%CNS_MEAN_RTP(J3) + Q*ERM%CNS_RATE_RTP(J3)*TIM_RAN )
              CALL INCH ( J3, CNS_ABR_RATE(6:6) )
              CALL ERR_PASS ( IUER, IER )
              CALL ADDCNS_NAM ( CNS_ABR_RATE, 1, 'ERM rate constraint', 'rad/s', &
     &                          RATE_RHS, ERM%CNS_RATE_SIGMA(J3)*TIM_RAN, &
     &                          .TRUE., CNSTROBJ, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 8527, IUER, 'ERM_CNST', 'Error in '// &
     &                 'an attempt to put information about '// &
     &                  CNS_ABR//' constraints into CNSTROBJ' )
                   RETURN
              END IF
              IF ( DEBUG_VAR .GE. 1 ) THEN
                   WRITE ( 6, * ) 'ERM_CNST  J3= ', J3, ' S  = ', S, ' Q= ', Q
                   WRITE ( 6, * ) 'ERM_CNST  J3= ', J3, ' M_S1 = ', NTIM_CNS*ERM%CNS_MEAN_RTP(J3) 
                   WRITE ( 6, * ) 'ERM_CNST  J3= ', J3, ' M_Q1 = ', ERM%CNS_RATE_RTP(J3)*TIM_RAN
                   WRITE ( 6, * ) 'ERM_CNST  J3= ', J3, ' M_Q2 = ', S*ERM%CNS_RATE_RTP(J3)*TIM_RAN
                   WRITE ( 6, * ) 'ERM_CNST  J3= ', J3, ' R_S1 = ', S*ERM%CNS_MEAN_RTP(J3) 
                   WRITE ( 6, * ) 'ERM_CNST  J3= ', J3, ' R_Q1 = ', ERM%CNS_RATE_RTP(J3)*TIM_RAN
                   WRITE ( 6, * ) 'ERM_CNST  J3= ', J3, ' R_Q2 = ', Q*ERM%CNS_RATE_RTP(J3)*TIM_RAN
                   WRITE ( 6, * ) 'ERM_CNST  J3= ', J3, ' MEAN_RHS= ', MEAN_RHS 
                   WRITE ( 6, * ) 'ERM_CNST  J3= ', J3, ' RATE_RHS= ', RATE_RHS 
              END IF
!
! ----------- Allocate memory for the constraint equations on the mean and on the rate 
!
              ALLOCATE ( EQU_CNS_MEAN(1-ERM%DEGREE(J3):ERM%NKNOTS(J3)-1), &
     &                   EQU_CNS_RATE(1-ERM%DEGREE(J3):ERM%NKNOTS(J3)-1), &
     &                   STAT=IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 8528, IUER, 'ERM_CNST', 'Error in '// &
     &                 'an attempt to allocated memory for EQU_CNS_MEAN '// &
     &                 'EQU_CNS_RATE' )
                   RETURN
              END IF
!
! ----------- Initialize the constraint equations
!
              EQU_CNS_MEAN = 0.0
              EQU_CNS_RATE = 0.0
!
! ----------- Build EOP MEAN and EOP RATE constraint equations
!
              DO 480 J8=1,NTIM_CNS
!
! -------------- Time epoch of the EOP wrt the first epoch of the EOP
!
                 TIM_ARG_EST = (J8-1)*ERM%TIME_CNS_SPAN(J3) + &
     &                         (ERM%MJD_BEG_RANGE_CNS - J2000__MJD)*86400.0D0 + &
     &                          ERM%TAI_BEG_RANGE_CNS
!
! -------------- Get the index of the B-spline for the J8th constraint epoch
!
                 IND_EQU = IXMN8 ( ERM%NKNOTS(J3), ERM%TIM(1,J3), TIM_ARG_EST )
                 IF ( IND_EQU == -1 ) THEN
                      TIM_ARG_EST = ERM%TIM(1,J3) - TIM_EPS
                      IND_EQU = IXMN8 ( ERM%NKNOTS(J3), ERM%TIM(1,J3), TIM_ARG_EST )
                   ELSE IF ( IND_EQU == -2 ) THEN
                      TIM_ARG_EST = ERM%TIM(1,J3) + TIM_EPS
                      IND_EQU = IXMN8 ( ERM%NKNOTS(J3), ERM%TIM(1,J3), TIM_ARG_EST )
                 END IF
                 IF ( IND_EQU < 1 ) THEN
                      WRITE ( 6, * ) 'J8= ', J8, ' TIM_ARG_EST = ', TIM_ARG_EST, ' TIM_1ST: ', ERM%TIM(1,J3), &
     &                               ' TIM_LAST= ', ERM%TIM(ERM%NKNOTS(J3),J3)
                      CALL ERR_LOG ( 8529, IUER, 'ERM_CNST', 'Trap of '//  &
     &                    'internal control: failure to find ERM index '// &
     &                    ' while imposing '//CNS_ABR_RATE//' constraint' )
                      RETURN
                 END IF
!
! -------------- Time epoch of the EOP wrt the first epoch of the EOP constraint
!
                 TIM_ARG_REF = (J8-1)*ERM%TIME_CNS_SPAN(J3) + &
     &                     (ERM%MJD_BEG_RANGE_CNS - ERM%MJD_REF_CNS)*86400.0D0 + &
     &                     (ERM%TAI_BEG_RANGE_CNS - ERM%TAI_REF_CNS)
                 IF ( DEBUG_VAR .GE. 2 ) THEN
                      WRITE ( 6, * ) 'ERM_CNST J3= ', INT2(J3), ' J8= ', J8, ' IND_EQU= ', IND_EQU, &
     &                               ' TIM_ARG_EST= ', TIM_ARG_EST, &
     &                               ' TIM_ARG_REF= ', TIM_ARG_REF, &
     &                               ' TIM_ARG_EST/TIM_RAN = ', SNGL(TIM_ARG_EST/TIM_RAN)
                 END IF
!
! -------------- Fill equations for the constraint on the mean and on the rate
!
                 DO 490 J9=IND_EQU-ERM%DEGREE(J3),IND_EQU
                    R = BSPL_VAL ( ERM%NKNOTS(J3), ERM%TIM(1,J3), ERM%DEGREE(J3), &
     &                             J9, TIM_ARG_EST )
                    EQU_CNS_MEAN(J9) = EQU_CNS_MEAN(J9) +                       R
                    EQU_CNS_RATE(J9) = EQU_CNS_RATE(J9) + TIM_ARG_REF/TIM_RAN * R
 490             CONTINUE 
 480          CONTINUE 
!
! ----------- Now apply EOP MEAN and EOP RATE contraints
!
              DO 4100 J10=1-ERM%DEGREE(J3),ERM%NKNOTS(J3)-1
!
! -------------- EOP MEAN constraint
!
                 IF ( DABS(EQU_CNS_MEAN(J10)) > 1.0D-24 ) THEN
!
! ------------------- Search for the J10th parameter in the parameter list
!
                      IND_PAR = IND_PARS ( J10, J3 )
                      IF ( DEBUG_VAR .GE. 2 ) THEN
                           WRITE ( 6, * ) 'ERM_CNST-MEAN J10= ', INT2(J10), &
     &                                    ' IND_PAR= ', IND_PAR, &
     &                                    ' EQU = ', EQU_CNS_MEAN(J10), &
     &                                    ' C_PAR= ', C_PAR(IND_PARS(J10,J3))
                      END IF
                      IF ( IND_PAR .LE. 0 ) THEN
                           CALL CLRCH ( STR )
                           CALL INCH  ( J10, STR )
                           CALL ERR_LOG ( 8530, IUER, 'ERM_CNST', 'Trap of '// &
     &                         'internal control: failure to find paramater '// &
     &                         ' with index '//TRIM(STR)//' while imposing '// &
     &                          CNS_ABR_MEAN//' constraint' )
                           RETURN
                      END IF
!
! ------------------- Put constraint equation to the list
!
                      CALL ERR_PASS ( IUER, IER )
                      CALL ADDCNS_EQU ( CNS_ABR_MEAN, 1, IND_PAR, EQU_CNS_MEAN(J10), &
     &                                  .TRUE., CNSTROBJ, IER )
                      IF ( IER .NE. 0 ) THEN
                           CALL ERR_LOG ( 8531, IUER, 'ERM_CNST', 'Failure '// &
     &                         'in putting a coefficient of an equation '// &
     &                         'of '//CNS_ABR_MEAN//' constraint' )
                           RETURN
                      END IF
                 END IF
!
! -------------- EOP RATE constraint
!
                 IF ( DABS(EQU_CNS_RATE(J10)) > 1.0D-24 ) THEN
!
! ------------------- Search for the J10th parameter in the parameter list
!
                      IND_PAR = IND_PARS ( J10, J3 )
                      IF ( DEBUG_VAR .GE. 2 ) THEN
                           WRITE ( 6, * ) 'ERM_CNST-RATE J10= ', INT2(J10), &
     &                                    ' IND_PAR= ', IND_PAR, &
     &                                    ' EQU = ', EQU_CNS_RATE(J10), &
     &                                    ' C_PAR= ', C_PAR(IND_PARS(J10,J3))
                      END IF
                      IF ( IND_PAR .LE. 0 ) THEN
                           CALL CLRCH ( STR )
                           CALL INCH  ( J10, STR )
                           CALL ERR_LOG ( 8532, IUER, 'ERM_CNST', 'Trap of '// &
     &                         'internal control: failure to find parameter '// &
     &                         ' with index '//TRIM(STR)//' while imposing '// &
     &                          CNS_ABR_RATE//' constraint' )
                           RETURN
                      END IF
!
! ------------------- Put constraint equation to the list
!
                      CALL ERR_PASS ( IUER, IER )
                      CALL ADDCNS_EQU ( CNS_ABR_RATE, 1, IND_PAR, EQU_CNS_RATE(J10), &
     &                                  .TRUE., CNSTROBJ, IER )
                      IF ( IER .NE. 0 ) THEN
                           CALL ERR_LOG ( 8533, IUER, 'ERM_CNST', 'Failure '// &
     &                         'in putting a coefficient of an equation '// &
     &                         'of '//CNS_ABR_RATE//' constraint' )
                           RETURN
                      END IF
                 END IF
 4100         CONTINUE 
              DEALLOCATE ( EQU_CNS_MEAN )
              DEALLOCATE ( EQU_CNS_RATE )
!
              WRITE ( 23, 120 ) J3, ERM%CNS_MEAN_SIGMA(J3) 
              WRITE ( 23, 130 ) J3, ERM%CNS_RATE_SIGMA(J3)  
 120          FORMAT ( 'ERMMEAN_CNS constraint on mean for component ', &
     &                 I1, ' sigma ', 1PD14.7 )            
 130          FORMAT ( 'ERMRATE_CNS constraint on rate for component ', &
     &                 I1, ' sigma ', 1PD14.7 )            
         END IF
 430  CONTINUE 
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  ERM_CNST  !#!#
