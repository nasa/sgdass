      SUBROUTINE EHEO_ERM_CNST ( FAST_MODE, FAST_DBG, L_PAR, C_PAR, &
     &                           ERM, L_EHEO, EHEO, MJD_EHEO_REF, TAI_EHEO_REF, &
     &                           EHES, CNSTROBJ, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  EHEO_ERM_CNST  imposes decorrelation constraints between  *
! *   coefficients of B-spline which model the Earth rotation and        *
! *   coefficients of harmonic expansion of the Earth orientation        *
! *   parameters. These constraint equation requires spline to have zero *
! *   Fourier constituents for those estimated frequencies of the        *
! *   harmonic model, which were specified by the user.                  *
! *                                                                      *
! *   For each frequency omega constraints on the E1 and E2              *
! *   constituents are imposed in this form:                             *
! *                                                                      *
! *   sum_k { E_{1k} * F_c(omega)  +   E_{2k} * F_s(omega) } = 0         *
! *   sum_k { E_{1k} * F_S(omega)  -   E_{2k} * F_c(omega) } = 0         *
! *                                                                      *
! *   Constraints on the E3 constituents is imposed in this form:        *
! *                                                                      *
! *   sum_k { E_{3k} * F_c(omega) } = 0                                  *
! *   sum_k { E_{3k} * F_s(omega) } = 0                                  *
! *                                                                      *
! *   Special case of constraints if omega=0                             *
! *                                                                      *
! *   a) estimates of E1E2:                                              *
! *                                                                      *
! *      COS :: sum_k { E_{1k} * F_0 }                                   *
! *      SIN :: sum_k { E_{2k} * F_0 }                                   *
! *                                                                      *
! *   b) estimates of E1E2 rates:                                        *
! *                                                                      *
! *      COS :: sum_k { E_{1k} * F_1 }                                   *
! *      SIN :: sum_k { E_{2k} * F_1 }                                   *
! *                                                                      *
! *   c) estimates of E3:                                                *
! *                                                                      *
! *      COS :: sum_k { E_{3k} * F_0 }                                   *
! *      SIN :: sum_k { E_{3k} * F_1 }                                   *
! *                                                                      *
! *   s) estimates of E1E2 rates:                                        *
! *                                                                      *
! *      COS :: sum_k { E_{1k} * F_0 }                                   *
! *      SIN :: sum_k { E_{2k} * F_0 }                                   *
! *                                                                      *
! *   Here F_c(omega) and F_s(omega) are cosine and sine part of the     *
! *   Fourier integral over the B-spline basis, F_0 -- the integral      *
! *   of the B-spline basis, F_1 -- the first moment of the B-spline     *
! *   basis.                                                             *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *   FAST_MODE ( INTEGER*4  ) -- Fast mode (algorithms selection).      *
! *    FAST_DBG ( INTEGER*4  ) -- Debugging mode.                        *
! *       L_PAR ( INTEGER*4  ) -- The total number of global parameter.  *
! *       C_PAR ( INTEGER*4  ) -- The list of global parameters.         *
! *         ERM ( ERM__TYPE  ) -- Derived object defined in              *
! *                               $PSOLVE_ROOT/include/solve.i which     *
! *                               keeps information about estimation of  *
! *                               the Earth Rotation Model.              *
! *      L_EHEO ( INTEGER*4  ) -- The number of frequencies for which    *
! *                               harmonic variations in Earth's         *
! *                               rotation are computed.                 *
! *        EHEO ( EHEO__TYPE ) -- Array derived object defined in        *
! *                               $PSOLVE_ROOT/include/solve.i which     *
! *                               keeps information about estimation of  *
! *                               the harmonic variations in the Earth's *
! *                               orientation. Dimension: L_EHEO.        *
! *        EHES ( EHES__TYPE ) -- Derived object defined in              *
! *                               $PSOLVE_ROOT/include/solve.i which     *
! *                               keeps reciprocal weights of            *
! *                               constraints imposed on harmonic        *
! *                               variations in the  Earth's orientation.*
! * MJD_EHEO_REF ( INTEGER*4 ) -- Reference modified Julian date for     *
! *                               estimation of the empirical harmonic   *
! *                               Earth orientation model.               *
! * TAI_EHEO_REF ( REAL*8    ) -- Seconds part of the reference modified *
! *                               Julian date for estimation of the      *
! *                               empirical Harmonic Earth Orientation   *
! *                               model.                                 *
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
! *    CNSTROBJ  ( RECORD    ) -- Object which accumulates information   *
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
! * ### 01-JUN-2006   EHEO_ERM_CNST   v3.1 (c) L. Petrov 14-MAR-2007 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'astro_constants.i'
      INCLUDE   'solve.i'
      INCLUDE   'cnstr.i'
      INCLUDE   'fast.i'
      INCLUDE   'erm.i'
      INTEGER*4  FAST_MODE, FAST_DBG, L_EHEO, L_PAR, MJD_EHEO_REF, IUER
      TYPE     ( EHEO__TYPE  ) :: EHEO(L_EHEO)
      TYPE     ( EHES__TYPE  ) :: EHES
      TYPE     ( ERM__TYPE   ) :: ERM
      TYPE     ( CNSTR__STRU ) :: CNSTROBJ
      CHARACTER  C_PAR(L_PAR)*(*)
      CHARACTER  CNS_ABR(4,2)*8, PAR_NAME*20, STR*32, STR1*32
      REAL*8     TAI_EHEO_REF
      REAL*8     SIG_MIN, TIM_EPS
      PARAMETER  ( SIG_MIN = 1.D-30 )
      PARAMETER  ( TIM_EPS = 0.0D0  )
      REAL*8     CNS_SIGMA
      REAL*8     TIM_SINCE_REF, TIM_INT, FC, FS, FINT, FMOM
      INTEGER*4  J1, J2, J3, J4, J5, J6, J7, J8, IND_EQU, IND_PAR, IND_SBI(4), &
     &           IND_PARS(1-ERM__MSPL:ERM__MKNOT,3), IER
      INTEGER*4   INT4_ARG
      INTEGER*2   INT2_ARG
      INTEGER*4   INT4
      INTEGER*1   INT1
      INT4(INT2_ARG) = INT(INT2_ARG,KIND=4)
      INT1(INT4_ARG) = INT(INT4_ARG,KIND=1)
      REAL*8,    EXTERNAL :: BSPL_INT1_FULL, BSPL_MOM1_FULL
      INTEGER*4, EXTERNAL :: ILEN, I_LEN, LTM_DIF
!
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
!
      TIM_SINCE_REF = ( (MJD_EHEO_REF - J2000__MJD)*86400.0D0 + &
     &                  (TAI_EHEO_REF - 43200.0D0) )
      TIM_INT = (ERM%MJD_END - ERM%MJD_BEG)*86400.0D0 + &
     &          (ERM%TAI_END - ERM%TAI_BEG)
!
! --- Cycle over all frequencies
!
      IND_SBI(HEO__E1E2) = 0  ! for E1E2
      IND_SBI(HEO__E3)   = 0  ! for E3
      IND_SBI(3) = 0  ! for E1E2 rate
      IND_SBI(4) = 0  ! for E3   rate
      DO 430 J3=1,L_EHEO
         IF ( EHEO(J3)%FL_CNS(HEO__E1E2) ) THEN
              IND_SBI(HEO__E1E2) = IND_SBI(HEO__E1E2) + 1
!
! ----------- Define constraints for E1 and E2 components
!
              CNS_ABR(HEO__E1E2,HEO__COS) = 'HEO_ERC1'  ! for cosine component
              IF ( DABS(EHEO(J3)%FREQ) > EHEO__FRQ_MIN ) THEN
                   CNS_SIGMA = EHES%EHEO_ERM_E1E2_HAR
                 ELSE
                   CNS_SIGMA = EHES%EHEO_ERM_E1E2_SHIFT
              END IF
!
              IF ( CNS_SIGMA > 0.0D0 ) THEN
                   CALL ERR_PASS ( IUER, IER )
                   CALL ADDCNS_NAM ( CNS_ABR(HEO__E1E2,HEO__COS), IND_SBI(HEO__E1E2), &
     &                              'HEO_ERM_COS_12 constraint', 'rad', 0.0D0, &
     &                               CNS_SIGMA, .TRUE., CNSTROBJ, IER )
                   IF ( IER .NE. 0 ) THEN
                        CALL ERR_LOG ( 8541, IUER, 'EHEO_ERM_CNST', 'Error in '// &
     &                      'an attempt to put information about '// &
     &                       CNS_ABR(HEO__E1E2,HEO__COS)//' constraints into '// &
     &                      'CNSTROBJ' )
                        RETURN
                   END IF
!
! ---------------- Set status "Dynamic constraint"
!
                   CALL ERR_PASS ( IUER, IER )
                   CALL SET_DYN_CNS ( CNS_ABR(HEO__E1E2,HEO__COS), IND_SBI(HEO__E1E2), &
     &                                2*(ERM%NKNOTS(1)+ERM%DEGREE(1)+1), &
     &                                CNSTROBJ, IER )
                   IF ( IER .NE. 0 ) THEN
                        CALL ERR_LOG ( 8542, IUER, 'EHEO_ERM_CNST', 'Error in '// &
     &                      'an attempt to set status "dynamic" for '// &
     &                       CNS_ABR(HEO__E1E2,HEO__COS)//' constraints' )
                        RETURN
                   END IF
!
                   IF ( FAST_DBG .EQ. F__PRI ) THEN
                        WRITE ( 6, * ) ' HEO_ERC1  fast_mode = ',INT1(FAST_MODE), &
     &                                 ' I_HEO: ', INT2(J3), &
     &                                 ' N_CNSTR = ', CNSTROBJ%N_ECNST
                   END IF
!
                   CNS_ABR(HEO__E1E2,HEO__SIN) = 'HEO_ERS1'  ! for sine component
                   IF ( DABS(EHEO(J3)%FREQ) > EHEO__FRQ_MIN ) THEN
                        CNS_SIGMA = EHES%EHEO_ERM_E1E2_HAR
                      ELSE
                        CNS_SIGMA = EHES%EHEO_ERM_E1E2_SHIFT
                   END IF
                   CALL ERR_PASS ( IUER, IER )
                   CALL ADDCNS_NAM ( CNS_ABR(HEO__E1E2,HEO__SIN), IND_SBI(HEO__E1E2), &
     &                              'HEO_ERM_SIN_12 constraint', 'rad', 0.0D0, &
     &                               CNS_SIGMA, .TRUE., CNSTROBJ, IER )
                   IF ( IER .NE. 0 ) THEN
                        CALL ERR_LOG ( 8543, IUER, 'EHEO_ERM_CNST', 'Error in '// &
     &                      'an attempt to put information about '// &
     &                       CNS_ABR(HEO__E1E2,HEO__SIN)//' constraints into CNSTROBJ' )
                        RETURN
                   END IF
!
! ---------------- Set status "dynamic constraint"
!
                   CALL ERR_PASS ( IUER, IER )
                   CALL SET_DYN_CNS ( CNS_ABR(HEO__E1E2,HEO__SIN), IND_SBI(HEO__E1E2), &
     &                                2*(ERM%NKNOTS(1)+ERM%DEGREE(1)+1), &
     &                                CNSTROBJ, IER )
                   IF ( IER .NE. 0 ) THEN
                        CALL ERR_LOG ( 8544, IUER, 'EHEO_ERM_CNST', 'Error in '// &
     &                      'an attempt to set status "dynamic" for '// &
     &                       CNS_ABR(HEO__E1E2,HEO__SIN)//' constraints' )
                        RETURN
                   END IF
!
                   IF ( FAST_DBG .EQ. F__PRI ) THEN
                        WRITE ( 6, * ) ' HEO_ERS1  fast_mode = ',INT1(FAST_MODE), &
     &                                 ' I_HEO: ', INT2(J3), &
     &                                 ' N_CNSTR = ', CNSTROBJ%N_ECNST
                   END IF
!
! ---------------- Cycle over E1 and E2 components
!
                   DO 440 J4=1,2
!
! ------------------- Cycle over spline coefficints for E(J4) component
!
                      DO 450 J5=1-ERM%DEGREE(J4),ERM%NKNOTS(J4)-1
                         IND_PAR = IND_PARS(J5,J4)
!
                         IF ( IND_PAR .LE. 0 ) THEN
                              CALL ERR_LOG ( 8545, IUER, 'EHEO_ERM_CNST', 'Trap '// &
     &                            'of internal control: cannot find parameter '// &
     &                            PAR_NAME//' in the parameters list' )
                              RETURN
                         END IF
                         IF ( DABS(EHEO(J3)%FREQ) > EHEO__FRQ_MIN ) THEN
!
! --------------------------- Compute Fourier transform of Basis Splines
!
                              CALL BSPL_FOUR ( ERM%NKNOTS(J4), ERM%TIM(1,J4), &
     &                                         ERM%DEGREE(J4), J5, EHEO(J3)%FREQ, FC, FS )
                              IF ( J4 == 1 ) THEN  ! E1
!
! ------------------------------- Insert coefficients for the cosine...
!
                                 CALL ERR_PASS ( IUER, IER )
                                 CALL ADDCNS_EQU ( CNS_ABR(HEO__E1E2,HEO__COS), &
     &                                             IND_SBI(HEO__E1E2), IND_PAR, &
     &                                             FC*DABS(EHEO(J3)%FREQ), &
     &                                             .TRUE., CNSTROBJ, IER )
                                 IF ( IER .NE. 0 ) THEN
                                      CALL ERR_LOG ( 8546, IUER, 'EHEO_ERM_CNST', &
     &                                    'Failure in putting a COS coefficient '// &
     &                                    'of an equation of '// &
     &                                     CNS_ABR(1,HEO__COS)//' constraint' )
                                      RETURN
                                 END IF
!
! ------------------------------ ... and the sine constrain equation
!
                                 CALL ERR_PASS ( IUER, IER )
                                 CALL ADDCNS_EQU ( CNS_ABR(HEO__E1E2,HEO__SIN), &
     &                                             IND_SBI(HEO__E1E2), IND_PAR, &
     &                                             FS*DABS(EHEO(J3)%FREQ), &
     &                                             .TRUE., CNSTROBJ, IER )
                                 IF ( IER .NE. 0 ) THEN
                                      CALL ERR_LOG ( 8547, IUER, 'EHEO_ERM_CNST', &
     &                                    'Failure in putting a SIN coefficient '// &
     &                                    'of an equation of '// &
     &                                     CNS_ABR(1,HEO__SIN)//' constraint' )
                                      RETURN
                                 END IF
                               ELSE IF ( J4 == 2 ) THEN ! E2
!
! ------------------------------ Insert coefficients for the cosine...
!
                                 CALL ERR_PASS ( IUER, IER )
                                 CALL ADDCNS_EQU ( CNS_ABR(HEO__E1E2,HEO__COS), &
     &                                             IND_SBI(HEO__E1E2), IND_PAR, &
     &                                             FS*DABS(EHEO(J3)%FREQ), &
     &                                             .TRUE., CNSTROBJ, IER )
                                 IF ( IER .NE. 0 ) THEN
                                      CALL ERR_LOG ( 8548, IUER, 'EHEO_ERM_CNST', &
     &                                    'Failure in putting a COS coefficient '// &
     &                                    'of an equation of '// &
     &                                     CNS_ABR(J4,HEO__COS)//' constraint' )
                                      RETURN
                                 END IF
!
! ------------------------------ ... and the sine constraint equation
!
                                 CALL ERR_PASS ( IUER, IER )
                                 CALL ADDCNS_EQU ( CNS_ABR(HEO__E1E2,HEO__SIN), &
     &                                             IND_SBI(HEO__E1E2), IND_PAR, &
     &                                             -FC*DABS(EHEO(J3)%FREQ), &
     &                                             .TRUE., CNSTROBJ, IER )
                                 IF ( IER .NE. 0 ) THEN
                                      CALL ERR_LOG ( 8549, IUER, 'EHEO_ERM_CNST', &
     &                                    'Failure in putting a SIN coefficient '// &
     &                                    'of an equation of '// &
     &                                     CNS_ABR(1,HEO__SIN)//' constraint' )
                                      RETURN
                                 END IF
                              END IF
                            ELSE ! Zero frequency
!
! --------------------------- Compute the integral of the B-spline of degree
! --------------------------- ERM%DEGREE(J4), with the pivotal element J5
! --------------------------- in limits from -\infty, to +\infty
!
                              FINT = BSPL_INT1_FULL ( ERM%NKNOTS(J4), ERM%TIM(1,J4), &
     &                                                ERM%DEGREE(J4), J5 )/ &
     &                                                ERM%TIME_EST_SPAN(J4)
!
! --------------------------- Insert coefficients for the shift of J4-th component
!
                              CALL ERR_PASS ( IUER, IER )
                              CALL ADDCNS_EQU ( CNS_ABR(HEO__E1E2,J4), &
     &                                          IND_SBI(HEO__E1E2), IND_PAR, &
     &                                          FINT, .TRUE., CNSTROBJ, IER )
                              IF ( IER .NE. 0 ) THEN
                                   CALL INCH ( J4, STR(1:1) )
                                   CALL ERR_LOG ( 8550, IUER, 'EHEO_ERM_CNST', &
     &                                 'Failure in putting a coefficient '// &
     &                                 'of an equation of constraint in shift of '// &
     &                                 'the E'//STR(1:1)//' comonent' )
                                   RETURN
                              END IF
                         END IF
 450                  CONTINUE
 440               CONTINUE
              END IF ! SIGMA > 0.0D0
         END IF
!
         IF ( EHEO(J3)%FL_CNS(HEO__E3) ) THEN
!
! ----------- Now turn to impose constraints on the E3 component
!
              IND_SBI(HEO__E3) = IND_SBI(HEO__E3) + 1
              CNS_ABR(HEO__E3,HEO__COS) = 'HEO_ERC3'
              IF ( DABS(EHEO(J3)%FREQ) > EHEO__FRQ_MIN ) THEN
                   CNS_SIGMA = EHES%EHEO_ERM_E3_HAR
                 ELSE
                   CNS_SIGMA = EHES%EHEO_ERM_E3_SHIFT
              END IF
              IF ( CNS_SIGMA > 0.0D0 ) THEN
                   CALL ERR_PASS ( IUER, IER )
                   CALL ADDCNS_NAM ( CNS_ABR(HEO__E3,HEO__COS), IND_SBI(HEO__E3), &
     &                              'HEO_ERM_COS_3 constraint', 'rad', 0.0D0, &
     &                               CNS_SIGMA, .TRUE., CNSTROBJ, IER )
                   IF ( IER .NE. 0 ) THEN
                        CALL ERR_LOG ( 8551, IUER, 'EHEO_ERM_CNST', 'Error in '// &
     &                      'an attempt to put information about '// &
     &                       CNS_ABR(HEO__E3,HEO__COS)//' constraints into CNSTROBJ' )
                        RETURN
                   END IF
!
! ---------------- Set status "dynamic constraint"
!
                   CALL ERR_PASS ( IUER, IER )
                   CALL SET_DYN_CNS ( CNS_ABR(HEO__E3,HEO__COS), IND_SBI(HEO__E3), &
     &                                ERM%NKNOTS(3)+ERM%DEGREE(3)+1, CNSTROBJ, IER )
                   IF ( IER .NE. 0 ) THEN
                        CALL ERR_LOG ( 8552, IUER, 'EHEO_ERM_CNST', 'Error in '// &
     &                      'an attempt to set status "dynamic" for '// &
     &                       CNS_ABR(HEO__E3,HEO__COS)//' constraints' )
                        RETURN
                   END IF
!
                   IF ( FAST_DBG .EQ. F__PRI ) THEN
                        WRITE ( 6, * ) ' HEO_ERC3  fast_mode = ',INT1(FAST_MODE), &
     &                                 ' I_HEO: ', INT2(J3), &
     &                                 ' N_CNSTR = ', CNSTROBJ%N_ECNST
                   END IF
!
                   CNS_ABR(HEO__E3,HEO__SIN) = 'HEO_ERS3'
                   IF ( DABS(EHEO(J3)%FREQ) > EHEO__FRQ_MIN ) THEN
                        CNS_SIGMA = EHES%EHEO_ERM_E3_HAR
                      ELSE
                        CNS_SIGMA = EHES%EHEO_ERM_E3_DRIFT
                   END IF
                   CALL ERR_PASS ( IUER, IER )
                   CALL ADDCNS_NAM ( CNS_ABR(HEO__E3,HEO__SIN), IND_SBI(HEO__E3), &
     &                              'HEO_ERM_SIN_3 constraint', 'rad', 0.0D0, &
     &                               CNS_SIGMA, .TRUE., CNSTROBJ, IER )
                   IF ( IER .NE. 0 ) THEN
                        CALL ERR_LOG ( 8553, IUER, 'EHEO_ERM_CNST', 'Error in '// &
     &                      'an attempt to put information about '// &
     &                       CNS_ABR(HEO__E3,HEO__SIN)//' constraints into CNSTROBJ' )
                        RETURN
                   END IF
!
! ---------------- Set status "dynamic constraint"
!
                   CALL ERR_PASS ( IUER, IER )
                   CALL SET_DYN_CNS ( CNS_ABR(HEO__E3,HEO__SIN), IND_SBI(HEO__E3), &
     &                                ERM%NKNOTS(3)+ERM%DEGREE(3)+1, CNSTROBJ, IER )
                   IF ( IER .NE. 0 ) THEN
                        CALL ERR_LOG ( 8554, IUER, 'EHEO_ERM_CNST', 'Error in '// &
     &                      'an attempt to set status "dynamic" for '// &
     &                       CNS_ABR(HEO__E3,HEO__SIN)//' constraints' )
                        RETURN
                   END IF
!
                   IF ( FAST_DBG .EQ. F__PRI ) THEN
                        WRITE ( 6, * ) ' HEO_ERS1  fast_mode = ',INT1(FAST_MODE), &
     &                                 ' I_HEO: ', INT2(J3), &
     &                                 ' N_CNSTR = ', CNSTROBJ%N_ECNST
                   END IF
!
                   DO 460 J6=1-ERM%DEGREE(3),ERM%NKNOTS(3)-1
                      IND_PAR = IND_PARS(J6,3)
!
                      IF ( IND_PAR .LE. 0 ) THEN
                      CALL ERR_LOG ( 8555, IUER, 'EHEO_ERM_CNST', 'Trap '// &
     &                         'of internal control: cannot find parameter '// &
     &                          PAR_NAME//' in the parameters list' )
                           RETURN
                      END IF
!
                      IF ( DABS(EHEO(J3)%FREQ) > EHEO__FRQ_MIN ) THEN
                           CALL BSPL_FOUR ( ERM%NKNOTS(3), ERM%TIM(1,3), &
     &                                      ERM%DEGREE(3), J6, EHEO(J3)%FREQ, FC, FS )
!
                          CALL ERR_PASS   ( IUER, IER )
                          CALL ADDCNS_EQU ( CNS_ABR(HEO__E3,HEO__COS), &
     &                                       IND_SBI(HEO__E3), IND_PAR, &
     &                                       FC*EHEO(J3)%FREQ, .TRUE., &
     &                                       CNSTROBJ, IER )
                           IF ( IER .NE. 0 ) THEN
                                CALL ERR_LOG ( 8556, IUER, 'EHEO_ERM_CNST', &
     &                              'Failure in putting a COS coefficient of '// &
     &                              'an equation of '//CNS_ABR(HEO__E3,HEO__COS)// &
     &                              ' constraint' )
                                RETURN
                           END IF
!
                           CALL ERR_PASS   ( IUER, IER )
                           CALL ADDCNS_EQU ( CNS_ABR(HEO__E3,HEO__SIN), &
     &                                       IND_SBI(HEO__E3), IND_PAR, &
     &                                       FS*EHEO(J3)%FREQ, .TRUE., &
     &                                       CNSTROBJ, IER )
                           IF ( IER .NE. 0 ) THEN
                                CALL ERR_LOG ( 8557, IUER, 'EHEO_ERM_CNST', &
     &                              'Failure in putting a SIN coefficient of '// &
     &                              'an equation of '//CNS_ABR(HEO__E3,HEO__SIN)// &
     &                              ' constraint' )
                                RETURN
                           END IF
                         ELSE ! Zero frequency
!
! ------------------------ Compute the integral and the first moment of the
! ------------------------ B-spline of degree ERM%DEGREE(J4), with the pivotal
! ------------------------ element J5 in limits from -\infty, to +\infty
!
                           FINT = BSPL_INT1_FULL ( ERM%NKNOTS(3), ERM%TIM(1,3), &
     &                                      ERM%DEGREE(3), J6 )/ERM%TIME_EST_SPAN(3)
                           FMOM = ( BSPL_MOM1_FULL ( ERM%NKNOTS(3), ERM%TIM(1,3), &
     &                                               ERM%DEGREE(3), J6 )/ &
     &                                               ERM%TIME_EST_SPAN(3) - &
     &                              TIM_SINCE_REF * FINT )/TIM_INT*2.0D0
!
!------------------------- Insert coefficients for the shift of 3rd component
!
                           CALL ERR_PASS ( IUER, IER )
                           CALL ADDCNS_EQU ( CNS_ABR(HEO__E3,HEO__COS), &
     &                                       IND_SBI(HEO__E3), IND_PAR, FINT, &
     &                                       .TRUE., CNSTROBJ, IER )
                           IF ( IER .NE. 0 ) THEN
                                CALL ERR_LOG ( 8558, IUER, 'EHEO_ERM_CNST', &
     &                              'Failure in putting a coefficient '// &
     &                              'of an equation of constraint in shift of '// &
     &                              'the E3 comonent' )
                                RETURN
                           END IF
!
!------------------------- and on the drift of 3rd component
!
                           CALL ERR_PASS ( IUER, IER )
                           CALL ADDCNS_EQU ( CNS_ABR(HEO__E3,HEO__SIN), &
     &                                       IND_SBI(HEO__E3), IND_PAR, FMOM, &
     &                                       .TRUE., CNSTROBJ, IER )
                           IF ( IER .NE. 0 ) THEN
                                CALL ERR_LOG ( 8559, IUER, 'EHEO_ERM_CNST', &
     &                              'Failure in putting a coefficient '// &
     &                              'of an equation of constraint in shift of '// &
     &                              'the E3 comonent' )
                                RETURN
                           END IF
                      END IF
 460             CONTINUE
              END IF ! CNS_SIGMA
         END IF
!
! ------ Constraints on rate of change of amplitude. Currently only constraint
! ------ on rate of change of E1/E2 are supported
!
         IF ( EHEO(J3)%FL_CNS_VEL(HEO__E1E2) ) THEN
              IF ( DABS(EHEO(J3)%FREQ) > EHEO__FRQ_MIN ) THEN
                   CALL CLRCH ( STR  )
                   CALL CLRCH ( STR1 )
                   CALL INCH  ( J3, STR )
                   WRITE ( UNIT=STR1(1:19), FMT='(1PD19.12)' ) EHEO(J3)%FREQ
                   CALL ERR_LOG ( 8560, IUER, 'EHEO_ERM_CNST', 'Constraint '// &
     &                 'on velocity of harmonic EOP amplitude, frequency '// &
     &                 'index '//STR(1:I_LEN(STR))//', frequency value '// &
     &                 STR(1:19)//' is not supported' )
                   RETURN
                 ELSE
!
! ---------------- Now turn to impose constraints on EOP drift
!
                   IND_SBI(3) = IND_SBI(3) + 1
                   DO 470 J7=1,2
                      CNS_ABR(3,J7)(1:7) = 'HEO_ERD'
                      CNS_SIGMA = EHES%EHEO_ERM_E1E2_DRIFT
                      CALL INCH ( J7, CNS_ABR(3,J7)(8:8) )
!
                      CALL ERR_PASS ( IUER, IER )
                      CALL ADDCNS_NAM ( CNS_ABR(3,J7), IND_SBI(3), &
     &                                 'HEO_ERD constraint', 'rad/s', 0.0D0, &
     &                                  CNS_SIGMA, .TRUE., CNSTROBJ, IER )
                      IF ( IER .NE. 0 ) THEN
                           CALL ERR_LOG ( 8561, IUER, 'EHEO_ERM_CNST', &
     &                         'Error in an attempt to put information '// &
     &                         'about '//CNS_ABR(3,J7)// &
     &                         ' constraints into CNSTROBJ' )
                           RETURN
                      END IF
!
! ------------------- Set status "dynamic constraint"
!
                      CALL ERR_PASS ( IUER, IER )
                      CALL SET_DYN_CNS ( CNS_ABR(3,J7), IND_SBI(3), &
     &                                   ERM%NKNOTS(J7)+ERM%DEGREE(J7)+1, &
     &                                   CNSTROBJ, IER )
                      IF ( IER .NE. 0 ) THEN
                           CALL ERR_LOG ( 8562, IUER, 'EHEO_ERM_CNST', &
     &                         'Error in an attempt to set status '// &
     &                         '"dynamic" for '//CNS_ABR(3,HEO__COS)// &
     &                         ' constraints' )
                           RETURN
                      END IF
!
                      IF ( FAST_DBG .EQ. F__PRI ) THEN
                           WRITE ( 6, * ) ' ',CNS_ABR(3,J7), &
     &                                    '  fast_mode = ',INT1(FAST_MODE), &
     &                                    ' I_HEO: ', INT2(J3), &
     &                                    ' N_CNSTR = ', CNSTROBJ%N_ECNST
                      END IF
!
                      DO 480 J8=1-ERM%DEGREE(J7),ERM%NKNOTS(J7)-1
                         IND_PAR = IND_PARS(J8,J7)
!
                         IF ( IND_PAR .LE. 0 ) THEN
                              CALL ERR_LOG ( 8563, IUER, 'EHEO_ERM_CNST', &
     &                            'Trap of internal control: cannot find '// &
     &                            'parameter '//PAR_NAME//' in the '// &
     &                            'parameters list' )
                              RETURN
                         END IF
!
                         FMOM = ( BSPL_MOM1_FULL ( ERM%NKNOTS(J7), ERM%TIM(1,J7), &
     &                                             ERM%DEGREE(J7), J8 ) - &
     &                            TIM_SINCE_REF * &
     &                            BSPL_INT1_FULL ( ERM%NKNOTS(J7), ERM%TIM(1,J7), &
     &                                             ERM%DEGREE(J7), J8 ) &
     &                          )/TIM_INT/ERM%TIME_EST_SPAN(J7)*2.0D0
                         CALL ERR_PASS ( IUER, IER )
                         CALL ADDCNS_EQU ( CNS_ABR(3,J7), IND_SBI(3), &
     &                                     IND_PAR, FMOM, .TRUE., CNSTROBJ, IER )
                         IF ( IER .NE. 0 ) THEN
                              CALL INCH ( J7, STR )
                              CALL ERR_LOG ( 8564, IUER, 'EHEO_ERM_CNST', &
     &                            'Failure in putting a coefficient '// &
     &                            'of an equation of constraint in drift of '// &
     &                            'the E'//STR(1:1)//' component' )
                              RETURN
                         END IF
 480                  CONTINUE
 470               CONTINUE
              END IF
         END IF
 430  CONTINUE
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE   EHEO_ERM_CNST  !#!#
