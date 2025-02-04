      SUBROUTINE NERS_GET_SPL ( NERS, TIM_TAI, CPAR, L_PAR, PARS, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  NERS_GET_SPL computes the Earth orteintation parameters   *
! *   using local B-spline coefficients computed by the previous         *
! *   call NERS_COMP_SPL that was called by NET_GET_EOP. This is an      *
! *   internal routine. Users are supposed to call external routine      *
! *   NET_GET_EOP.                                                       *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *    NERS ( NERS__TYPE ) -- The data structure that keeps internal     *
! *                           parameters related to the Network Earth    *
! *                           Rotation Service.                          *
! * TIM_TAI ( REAL*8     ) -- Time for which the Earth orientation       *
! *                           parameter(s) is to be computed elapsed     *
! *                           since 2000.01.01_00:00:00.0 TAI. Unit: sec.*
! *                           If TIM_TAI .LE. -1.0D0, then the EOP on    *
! *                           the current moment of time will be         *
! *                           computed.                                  *
! *                                                                      *
! *                           NB: it is common to use tiem tag UTC.      *
! *                           The time tag UTC should be converted to    *
! *                           time in TAI scale prior call to            *
! *                           NERS_GET_EOP.                              *
! *    CPAR ( CHARACTER  ) -- The Earth orientationi parameter name.     *
! *                                                                      *
! * _________________________ Output parameters: _______________________ *
! *                                                                      *
! *   L_PAR ( INTEGER*4  ) -- The number of elements in the output       *
! *                           array of the Earth orientation parameters. *
! *    PARS ( REAL*8     ) -- The array of the Earth orientation         *
! *                           parameters.                                *
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
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
! *  ### 18-JUN-2016  NERS_GET_SPL  v1.2 (c) L. Petrov  21-APR-2018 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'ners.i'
      INTEGER*4  IUER
      TYPE     ( NERS__TYPE ) :: NERS
      INTEGER*4  L_PAR
      CHARACTER  CPAR*(*)
      REAL*8     TIM_TAI, PARS(NERS__MPAR)
      REAL*8     TIM_EPS
      PARAMETER  ( TIM_EPS = 1.D-6 ) 
      REAL*8,    ALLOCATABLE :: TEMP_ARR(:)
      CHARACTER  STR*128
      REAL*8     TIM_BEG, TIM_END, TIM_STEP, TAI, DEPS, DPSI, &
     &           EVEC(3,0:2), HEO_VEC(3,0:2), MAT_ROT(3,3,0:2), &
     &           DER_EOP(2,NERS__MEL), TIM, &
     &           XPOL, YPOL, S_ANG, XPOL_RATE, YPOL_RATE, S_ANG_RATE, &
     &           DZETA, TETA, ZA, EPS_0, DZETA_RATE, &
     &           TETA_RATE, ZA_RATE, EPS_0_RATE, &
     &           E1_NUT, E2_NUT, &
     &           E1_NUT_RATE, E2_NUT_RATE, DPSI_RATE, DEPS_RATE
      REAL*8       LOD__TO__ER3
      PARAMETER  ( LOD__TO__ER3 =  1.00273781191135448E0*OM__EAR**2/PI2 )
      INTEGER*4  J1, J2, J3, J4, J5, J6, J7, IND, MJD, IER
      CHARACTER  MJDSEC_TO_DATE*30
      INTEGER*4, EXTERNAL :: IXMN8
      REAL*8,    EXTERNAL :: EBSPL_VAL_R8, EBSPL_DER_R8, EBSPL_DR2_R8
!
! --- Check whether the spline coefficients have been computed
!
      IF ( NERS%EXP_STATUS .NE. NERS__COMP ) THEN
           CALL ERR_LOG ( 4811, IUER, 'NERS_GET_SPL', 'Trap of '// &
     &         'internal control: NERS spline coefficients have not '// &
     &         'been computed' )
           RETURN 
      END IF
      TIM = TIM_TAI - NERS%TIM_START
      IF ( DABS( TIM_TAI - NERS%TIM_START ) < TIM_EPS ) THEN
           IND = 1
        ELSE IF ( DABS( TIM_TAI - NERS%TIM_STOP ) < TIM_EPS ) THEN
           IND = NERS%EXP%L_NOD
        ELSE IF ( TIM_TAI .GE. NERS%TIM_START - TIM_EPS .AND. &
     &            TIM_TAI .LE. NERS%TIM_STOP  + TIM_EPS       ) THEN
           IND = IXMN8 ( NERS%EXP%L_NOD, NERS%EXP%ARG, TIM_TAI - NERS%TIM_START )
        ELSE
           WRITE ( 6, * ) 'TIM_START/TIM_STOP: ', NERS%TIM_START, NERS%TIM_STOP 
           WRITE ( 6, * ) 'TIM_TAI:            ', TIM_TAI
           CALL ERR_LOG ( 4812, IUER, 'NERS_GET_SPL', 'Trap of '// &
     &         'internal control: time epoch is out of range' )
           RETURN 
      END IF
!
! --- Compute specific Earth orientation parameter
!
      IF ( CPAR == 'mat' .OR. CPAR == 'matr' .OR. CPAR == 'matrr' .OR. CPAR == 'matall' ) THEN
           EVEC(1,0)    = EBSPL_VAL_R8 ( NERS%EXP%L_NOD, NERS__MDEG, TIM, NERS%EXP%ARG, &
     &                                   NERS%EXP%BSPL(1-NERS__MDEG,NERS__E1) )
           EVEC(2,0)    = EBSPL_VAL_R8 ( NERS%EXP%L_NOD, NERS__MDEG, TIM, NERS%EXP%ARG, &
     &                                   NERS%EXP%BSPL(1-NERS__MDEG,NERS__E2) )
           EVEC(3,0)    = EBSPL_VAL_R8 ( NERS%EXP%L_NOD, NERS__MDEG, TIM, NERS%EXP%ARG, &
     &                                   NERS%EXP%BSPL(1-NERS__MDEG,NERS__E3) )
           EVEC(1,1)    = EBSPL_DER_R8 ( NERS%EXP%L_NOD, NERS__MDEG, TIM, NERS%EXP%ARG, &
     &                                   NERS%EXP%BSPL(1-NERS__MDEG,NERS__E1) )
           EVEC(2,1)    = EBSPL_DER_R8 ( NERS%EXP%L_NOD, NERS__MDEG, TIM, NERS%EXP%ARG, &
     &                                   NERS%EXP%BSPL(1-NERS__MDEG,NERS__E2) )
           EVEC(3,1)    = EBSPL_DER_R8 ( NERS%EXP%L_NOD, NERS__MDEG, TIM, NERS%EXP%ARG, &
     &                                   NERS%EXP%BSPL(1-NERS__MDEG,NERS__E3) )
           HEO_VEC(1,0) = EBSPL_VAL_R8 ( NERS%EXP%L_NOD, NERS__MDEG, TIM, NERS%EXP%ARG, &
     &                                   NERS%EXP%BSPL(1-NERS__MDEG,NERS__H1) )
           HEO_VEC(2,0) = EBSPL_VAL_R8 ( NERS%EXP%L_NOD, NERS__MDEG, TIM, NERS%EXP%ARG, &
     &                                   NERS%EXP%BSPL(1-NERS__MDEG,NERS__H2) )
           HEO_VEC(3,0) = EBSPL_VAL_R8 ( NERS%EXP%L_NOD, NERS__MDEG, TIM, NERS%EXP%ARG, &
     &                                   NERS%EXP%BSPL(1-NERS__MDEG,NERS__H3) )
           HEO_VEC(1,1) = EBSPL_DER_R8 ( NERS%EXP%L_NOD, NERS__MDEG, TIM, NERS%EXP%ARG, &
     &                                   NERS%EXP%BSPL(1-NERS__MDEG,NERS__H1) )
           HEO_VEC(2,1) = EBSPL_DER_R8 ( NERS%EXP%L_NOD, NERS__MDEG, TIM, NERS%EXP%ARG, &
     &                                   NERS%EXP%BSPL(1-NERS__MDEG,NERS__H2) )
           HEO_VEC(3,1) = EBSPL_DER_R8 ( NERS%EXP%L_NOD, NERS__MDEG, TIM, NERS%EXP%ARG, &
     &                                   NERS%EXP%BSPL(1-NERS__MDEG,NERS__H3) )
           HEO_VEC(1,2) = EBSPL_DR2_R8 ( NERS%EXP%L_NOD, NERS__MDEG, TIM, NERS%EXP%ARG, &
     &                                   NERS%EXP%BSPL(1-NERS__MDEG,NERS__H1) )
           HEO_VEC(2,2) = EBSPL_DR2_R8 ( NERS%EXP%L_NOD, NERS__MDEG, TIM, NERS%EXP%ARG, &
     &                                   NERS%EXP%BSPL(1-NERS__MDEG,NERS__H2) )
           HEO_VEC(3,2) = EBSPL_DR2_R8 ( NERS%EXP%L_NOD, NERS__MDEG, TIM, NERS%EXP%ARG, &
     &                                   NERS%EXP%BSPL(1-NERS__MDEG,NERS__H3) )
           DPSI         = EBSPL_VAL_R8 ( NERS%EXP%L_NOD, NERS__MDEG, TIM, NERS%EXP%ARG, &
     &                                   NERS%EXP%BSPL(1-NERS__MDEG,NERS__DP) )
           DEPS         = EBSPL_VAL_R8 ( NERS%EXP%L_NOD, NERS__MDEG, TIM, NERS%EXP%ARG, &
     &                                   NERS%EXP%BSPL(1-NERS__MDEG,NERS__DE) )
           DPSI_RATE    = EBSPL_DER_R8 ( NERS%EXP%L_NOD, NERS__MDEG, TIM, NERS%EXP%ARG, &
     &                                   NERS%EXP%BSPL(1-NERS__MDEG,NERS__DP) )
           DEPS_RATE    = EBSPL_DER_R8 ( NERS%EXP%L_NOD, NERS__MDEG, TIM, NERS%EXP%ARG, &
     &                                   NERS%EXP%BSPL(1-NERS__MDEG,NERS__DE) )
           E1_NUT       = EBSPL_VAL_R8 ( NERS%EXP%L_NOD, NERS__MDEG, TIM, NERS%EXP%ARG, &
     &                                   NERS%EXP%BSPL(1-NERS__MDEG,NERS__E1_NUT) )
           E2_NUT       = EBSPL_VAL_R8 ( NERS%EXP%L_NOD, NERS__MDEG, TIM, NERS%EXP%ARG, &
     &                                   NERS%EXP%BSPL(1-NERS__MDEG,NERS__E2_NUT) )
           E1_NUT_RATE  = EBSPL_DER_R8 ( NERS%EXP%L_NOD, NERS__MDEG, TIM, NERS%EXP%ARG, &
     &                                   NERS%EXP%BSPL(1-NERS__MDEG,NERS__E1_NUT) )
           E2_NUT_RATE  = EBSPL_DER_R8 ( NERS%EXP%L_NOD, NERS__MDEG, TIM, NERS%EXP%ARG, &
     &                                   NERS%EXP%BSPL(1-NERS__MDEG,NERS__E2_NUT) )
           DZETA        = EBSPL_VAL_R8 ( NERS%EXP%L_NOD, NERS__MDEG, TIM, NERS%EXP%ARG, &
     &                                   NERS%EXP%BSPL(1-NERS__MDEG,NERS__DZETA) )
           TETA         = EBSPL_VAL_R8 ( NERS%EXP%L_NOD, NERS__MDEG, TIM, NERS%EXP%ARG, &
     &                                   NERS%EXP%BSPL(1-NERS__MDEG,NERS__TETA) )
           ZA           = EBSPL_VAL_R8 ( NERS%EXP%L_NOD, NERS__MDEG, TIM, NERS%EXP%ARG, &
     &                                   NERS%EXP%BSPL(1-NERS__MDEG,NERS__ZA) )
           EPS_0        = EBSPL_VAL_R8 ( NERS%EXP%L_NOD, NERS__MDEG, TIM, NERS%EXP%ARG, &
     &                                   NERS%EXP%BSPL(1-NERS__MDEG,NERS__EPS0) )
           DZETA_RATE   = EBSPL_DER_R8 ( NERS%EXP%L_NOD, NERS__MDEG, TIM, NERS%EXP%ARG, &
     &                                   NERS%EXP%BSPL(1-NERS__MDEG,NERS__DZETA) )
           TETA_RATE    = EBSPL_DER_R8 ( NERS%EXP%L_NOD, NERS__MDEG, TIM, NERS%EXP%ARG, &
     &                                   NERS%EXP%BSPL(1-NERS__MDEG,NERS__TETA) )
           ZA_RATE      = EBSPL_DER_R8 ( NERS%EXP%L_NOD, NERS__MDEG, TIM, NERS%EXP%ARG, &
     &                                   NERS%EXP%BSPL(1-NERS__MDEG,NERS__ZA) )
           EPS_0_RATE   = EBSPL_DER_R8 ( NERS%EXP%L_NOD, NERS__MDEG, TIM, NERS%EXP%ARG, &
     &                                   NERS%EXP%BSPL(1-NERS__MDEG,NERS__EPS0) )
           S_ANG        = EBSPL_VAL_R8 ( NERS%EXP%L_NOD, NERS__MDEG, TIM, NERS%EXP%ARG, &
     &                                   NERS%EXP%BSPL(1-NERS__MDEG,NERS__SANG) )
           S_ANG_RATE   = EBSPL_DER_R8 ( NERS%EXP%L_NOD, NERS__MDEG, TIM, NERS%EXP%ARG, &
     &                                   NERS%EXP%BSPL(1-NERS__MDEG,NERS__SANG) )
           XPOL      = EVEC(2,0)
           YPOL      = EVEC(1,0)
           XPOL_RATE = EVEC(2,1)
           YPOL_RATE = EVEC(1,1)
           CALL NERS_ERM_MATS ( XPOL, YPOL, S_ANG, &
     &                          XPOL_RATE, YPOL_RATE, S_ANG_RATE, &
     &                          DZETA, TETA, ZA, EPS_0, &
     &                          DZETA_RATE, TETA_RATE, ZA_RATE, EPS_0_RATE, &
     &                          DPSI, DEPS, DPSI_RATE, DEPS_RATE, &
     &                          HEO_VEC(1,0), HEO_VEC(1,1), HEO_VEC(1,2), &
     &                          0.0D0, 0.0D0, MAT_ROT, IER )
      END IF
      IF ( CPAR == 'mat' ) THEN
           L_PAR = 9
           CALL COPY_R8 ( 9, MAT_ROT(1,1,0), PARS )
        ELSE IF ( CPAR == 'matr' ) THEN
           L_PAR = 9
           CALL COPY_R8 ( 9, MAT_ROT(1,1,1), PARS )
        ELSE IF ( CPAR == 'matrr' ) THEN
           L_PAR = 9
           CALL COPY_R8 ( 9, MAT_ROT(1,1,2), PARS )
        ELSE IF ( CPAR == 'matall' ) THEN
           L_PAR = 27
           CALL COPY_R8 ( 27, MAT_ROT, PARS )
        ELSE IF ( CPAR == 'e1' ) THEN
           L_PAR = 1
           PARS(L_PAR) = EBSPL_VAL_R8 ( NERS%EXP%L_NOD, NERS__MDEG, TIM, &
     &                                  NERS%EXP%ARG, NERS%EXP%BSPL(1-NERS__MDEG,NERS__E1) )
        ELSE IF ( CPAR == 'e2' ) THEN
           L_PAR = 1
           PARS(L_PAR) = EBSPL_VAL_R8 ( NERS%EXP%L_NOD, NERS__MDEG, TIM, &
     &                                  NERS%EXP%ARG, NERS%EXP%BSPL(1-NERS__MDEG,NERS__E2) )
        ELSE IF ( CPAR == 'e3' ) THEN
           L_PAR = 1
           PARS(L_PAR) = EBSPL_VAL_R8 ( NERS%EXP%L_NOD, NERS__MDEG, TIM, &
     &                                  NERS%EXP%ARG, NERS%EXP%BSPL(1-NERS__MDEG,NERS__E3) )
        ELSE IF ( CPAR == 'e' ) THEN
           L_PAR = 3
           PARS(1) = EBSPL_VAL_R8 ( NERS%EXP%L_NOD, NERS__MDEG, TIM, &
     &                              NERS%EXP%ARG, NERS%EXP%BSPL(1-NERS__MDEG,NERS__E1) )
           PARS(2) = EBSPL_VAL_R8 ( NERS%EXP%L_NOD, NERS__MDEG, TIM, &
     &                              NERS%EXP%ARG, NERS%EXP%BSPL(1-NERS__MDEG,NERS__E2) )
           PARS(3) = EBSPL_VAL_R8 ( NERS%EXP%L_NOD, NERS__MDEG, TIM, &
     &                              NERS%EXP%ARG, NERS%EXP%BSPL(1-NERS__MDEG,NERS__E3) )
        ELSE IF ( CPAR == 'e1r' ) THEN
           L_PAR = 1
           PARS(L_PAR) = EBSPL_DER_R8 ( NERS%EXP%L_NOD, NERS__MDEG, TIM, &
     &                                  NERS%EXP%ARG, NERS%EXP%BSPL(1-NERS__MDEG,NERS__E1) )
        ELSE IF ( CPAR == 'e2r' ) THEN
           L_PAR = 1
           PARS(L_PAR) = EBSPL_DER_R8 ( NERS%EXP%L_NOD, NERS__MDEG, TIM, &
     &                                  NERS%EXP%ARG, NERS%EXP%BSPL(1-NERS__MDEG,NERS__E2) )
        ELSE IF ( CPAR == 'e3r' ) THEN
           L_PAR = 1
           PARS(L_PAR) = EBSPL_DER_R8 ( NERS%EXP%L_NOD, NERS__MDEG, TIM, &
     &                                  NERS%EXP%ARG, NERS%EXP%BSPL(1-NERS__MDEG,NERS__E3) )
        ELSE IF ( CPAR == 'er' ) THEN
           L_PAR = 6
           PARS(1) = EBSPL_VAL_R8 ( NERS%EXP%L_NOD, NERS__MDEG, TIM, &
     &                              NERS%EXP%ARG, NERS%EXP%BSPL(1-NERS__MDEG,NERS__E1) )
           PARS(2) = EBSPL_VAL_R8 ( NERS%EXP%L_NOD, NERS__MDEG, TIM, &
     &                              NERS%EXP%ARG, NERS%EXP%BSPL(1-NERS__MDEG,NERS__E2) )
           PARS(3) = EBSPL_VAL_R8 ( NERS%EXP%L_NOD, NERS__MDEG, TIM, &
     &                              NERS%EXP%ARG, NERS%EXP%BSPL(1-NERS__MDEG,NERS__E3) )
           PARS(4) = EBSPL_DER_R8 ( NERS%EXP%L_NOD, NERS__MDEG, TIM, &
     &                              NERS%EXP%ARG, NERS%EXP%BSPL(1-NERS__MDEG,NERS__E1) )
           PARS(5) = EBSPL_DER_R8 ( NERS%EXP%L_NOD, NERS__MDEG, TIM, &
     &                              NERS%EXP%ARG, NERS%EXP%BSPL(1-NERS__MDEG,NERS__E2) )
           PARS(6) = EBSPL_DER_R8 ( NERS%EXP%L_NOD, NERS__MDEG, TIM, &
     &                              NERS%EXP%ARG, NERS%EXP%BSPL(1-NERS__MDEG,NERS__E3) )
        ELSE IF ( CPAR == 'e1rr' ) THEN
           L_PAR = 1
           PARS(L_PAR) = EBSPL_DR2_R8 ( NERS%EXP%L_NOD, NERS__MDEG, TIM, &
     &                                  NERS%EXP%ARG, NERS%EXP%BSPL(1-NERS__MDEG,NERS__E1) )
        ELSE IF ( CPAR == 'e2rr' ) THEN
           L_PAR = 1
           PARS(L_PAR) = EBSPL_DR2_R8 ( NERS%EXP%L_NOD, NERS__MDEG, TIM, &
     &                                  NERS%EXP%ARG, NERS%EXP%BSPL(1-NERS__MDEG,NERS__E2) )
        ELSE IF ( CPAR == 'e3rr' ) THEN
           L_PAR = 1
           PARS(L_PAR) = EBSPL_DR2_R8 ( NERS%EXP%L_NOD, NERS__MDEG, TIM, &
     &                                  NERS%EXP%ARG, NERS%EXP%BSPL(1-NERS__MDEG,NERS__E3) )
        ELSE IF ( CPAR == 'err' ) THEN
           L_PAR = 9
           PARS(1) = EBSPL_VAL_R8 ( NERS%EXP%L_NOD, NERS__MDEG, TIM, &
     &                              NERS%EXP%ARG, NERS%EXP%BSPL(1-NERS__MDEG,NERS__E1) )
           PARS(2) = EBSPL_VAL_R8 ( NERS%EXP%L_NOD, NERS__MDEG, TIM, &
     &                              NERS%EXP%ARG, NERS%EXP%BSPL(1-NERS__MDEG,NERS__E2) )
           PARS(3) = EBSPL_VAL_R8 ( NERS%EXP%L_NOD, NERS__MDEG, TIM, &
     &                              NERS%EXP%ARG, NERS%EXP%BSPL(1-NERS__MDEG,NERS__E3) )
           PARS(4) = EBSPL_DER_R8 ( NERS%EXP%L_NOD, NERS__MDEG, TIM, &
     &                              NERS%EXP%ARG, NERS%EXP%BSPL(1-NERS__MDEG,NERS__E1) )
           PARS(5) = EBSPL_DER_R8 ( NERS%EXP%L_NOD, NERS__MDEG, TIM, &
     &                              NERS%EXP%ARG, NERS%EXP%BSPL(1-NERS__MDEG,NERS__E2) )
           PARS(6) = EBSPL_DER_R8 ( NERS%EXP%L_NOD, NERS__MDEG, TIM, &
     &                              NERS%EXP%ARG, NERS%EXP%BSPL(1-NERS__MDEG,NERS__E3) )
           PARS(7) = EBSPL_DR2_R8 ( NERS%EXP%L_NOD, NERS__MDEG, TIM, &
     &                              NERS%EXP%ARG, NERS%EXP%BSPL(1-NERS__MDEG,NERS__E1) )
           PARS(8) = EBSPL_DR2_R8 ( NERS%EXP%L_NOD, NERS__MDEG, TIM, &
     &                              NERS%EXP%ARG, NERS%EXP%BSPL(1-NERS__MDEG,NERS__E2) )
           PARS(9) = EBSPL_DR2_R8 ( NERS%EXP%L_NOD, NERS__MDEG, TIM, &
     &                              NERS%EXP%ARG, NERS%EXP%BSPL(1-NERS__MDEG,NERS__E3) )
        ELSE IF ( CPAR == 'h1' ) THEN
           L_PAR = 1
           PARS(L_PAR) = EBSPL_VAL_R8 ( NERS%EXP%L_NOD, NERS__MDEG, TIM, &
     &                                  NERS%EXP%ARG, NERS%EXP%BSPL(1-NERS__MDEG,NERS__H1) )
        ELSE IF ( CPAR == 'h2' ) THEN
           L_PAR = 1
           PARS(L_PAR) = EBSPL_VAL_R8 ( NERS%EXP%L_NOD, NERS__MDEG, TIM, &
     &                                  NERS%EXP%ARG, NERS%EXP%BSPL(1-NERS__MDEG,NERS__H2) )
        ELSE IF ( CPAR == 'h3' ) THEN
           L_PAR = 1
           PARS(L_PAR) = EBSPL_VAL_R8 ( NERS%EXP%L_NOD, NERS__MDEG, TIM, &
     &                                  NERS%EXP%ARG, NERS%EXP%BSPL(1-NERS__MDEG,NERS__H3) )
        ELSE IF ( CPAR == 'heo' ) THEN
           L_PAR = 3
           PARS(1) = EBSPL_VAL_R8 ( NERS%EXP%L_NOD, NERS__MDEG, TIM, &
     &                              NERS%EXP%ARG, NERS%EXP%BSPL(1-NERS__MDEG,NERS__H1) )
           PARS(2) = EBSPL_VAL_R8 ( NERS%EXP%L_NOD, NERS__MDEG, TIM, &
     &                              NERS%EXP%ARG, NERS%EXP%BSPL(1-NERS__MDEG,NERS__H2) )
           PARS(3) = EBSPL_VAL_R8 ( NERS%EXP%L_NOD, NERS__MDEG, TIM, &
     &                              NERS%EXP%ARG, NERS%EXP%BSPL(1-NERS__MDEG,NERS__H3) )
        ELSE IF ( CPAR == 'heor' ) THEN
           L_PAR = 9
           PARS(1) = EBSPL_VAL_R8 ( NERS%EXP%L_NOD, NERS__MDEG, TIM, &
     &                              NERS%EXP%ARG, NERS%EXP%BSPL(1-NERS__MDEG,NERS__H1) )
           PARS(2) = EBSPL_VAL_R8 ( NERS%EXP%L_NOD, NERS__MDEG, TIM, &
     &                              NERS%EXP%ARG, NERS%EXP%BSPL(1-NERS__MDEG,NERS__H2) )
           PARS(3) = EBSPL_VAL_R8 ( NERS%EXP%L_NOD, NERS__MDEG, TIM, &
     &                              NERS%EXP%ARG, NERS%EXP%BSPL(1-NERS__MDEG,NERS__H3) )
           PARS(4) = EBSPL_DER_R8 ( NERS%EXP%L_NOD, NERS__MDEG, TIM, &
     &                              NERS%EXP%ARG, NERS%EXP%BSPL(1-NERS__MDEG,NERS__H1) )
           PARS(5) = EBSPL_VAL_R8 ( NERS%EXP%L_NOD, NERS__MDEG, TIM, &
     &                              NERS%EXP%ARG, NERS%EXP%BSPL(1-NERS__MDEG,NERS__H2) )
           PARS(6) = EBSPL_VAL_R8 ( NERS%EXP%L_NOD, NERS__MDEG, TIM, &
     &                              NERS%EXP%ARG, NERS%EXP%BSPL(1-NERS__MDEG,NERS__H3) )
        ELSE IF ( CPAR == 'heorr' ) THEN
           L_PAR = 9
           PARS(1) = EBSPL_VAL_R8 ( NERS%EXP%L_NOD, NERS__MDEG, TIM, &
     &                              NERS%EXP%ARG, NERS%EXP%BSPL(1-NERS__MDEG,NERS__H1) )
           PARS(2) = EBSPL_VAL_R8 ( NERS%EXP%L_NOD, NERS__MDEG, TIM, &
     &                              NERS%EXP%ARG, NERS%EXP%BSPL(1-NERS__MDEG,NERS__H2) )
           PARS(3) = EBSPL_VAL_R8 ( NERS%EXP%L_NOD, NERS__MDEG, TIM, &
     &                              NERS%EXP%ARG, NERS%EXP%BSPL(1-NERS__MDEG,NERS__H3) )
           PARS(4) = EBSPL_DER_R8 ( NERS%EXP%L_NOD, NERS__MDEG, TIM, &
     &                              NERS%EXP%ARG, NERS%EXP%BSPL(1-NERS__MDEG,NERS__H1) )
           PARS(5) = EBSPL_VAL_R8 ( NERS%EXP%L_NOD, NERS__MDEG, TIM, &
     &                              NERS%EXP%ARG, NERS%EXP%BSPL(1-NERS__MDEG,NERS__H2) )
           PARS(6) = EBSPL_VAL_R8 ( NERS%EXP%L_NOD, NERS__MDEG, TIM, &
     &                              NERS%EXP%ARG, NERS%EXP%BSPL(1-NERS__MDEG,NERS__H3) )
           PARS(7) = EBSPL_DR2_R8 ( NERS%EXP%L_NOD, NERS__MDEG, TIM, &
     &                              NERS%EXP%ARG, NERS%EXP%BSPL(1-NERS__MDEG,NERS__H1) )
           PARS(8) = EBSPL_DR2_R8 ( NERS%EXP%L_NOD, NERS__MDEG, TIM, &
     &                              NERS%EXP%ARG, NERS%EXP%BSPL(1-NERS__MDEG,NERS__H2) )
           PARS(9) = EBSPL_DR2_R8 ( NERS%EXP%L_NOD, NERS__MDEG, TIM, &
     &                              NERS%EXP%ARG, NERS%EXP%BSPL(1-NERS__MDEG,NERS__H3) )
        ELSE IF ( CPAR == 'h1r' ) THEN
           L_PAR = 1
           PARS(L_PAR) = EBSPL_DER_R8 ( NERS%EXP%L_NOD, NERS__MDEG, TIM, &
     &                                  NERS%EXP%ARG, NERS%EXP%BSPL(1-NERS__MDEG,NERS__H1) )
        ELSE IF ( CPAR == 'h2r' ) THEN
           L_PAR = 1
           PARS(L_PAR) = EBSPL_VAL_R8 ( NERS%EXP%L_NOD, NERS__MDEG, TIM, &
     &                                  NERS%EXP%ARG, NERS%EXP%BSPL(1-NERS__MDEG,NERS__H2) )
        ELSE IF ( CPAR == 'h3r' ) THEN
           L_PAR = 1
           PARS(L_PAR) = EBSPL_VAL_R8 ( NERS%EXP%L_NOD, NERS__MDEG, TIM, &
     &                                  NERS%EXP%ARG, NERS%EXP%BSPL(1-NERS__MDEG,NERS__H3) )
        ELSE IF ( CPAR == 'h1rr' ) THEN
           L_PAR = 1
           PARS(L_PAR) = EBSPL_DR2_R8 ( NERS%EXP%L_NOD, NERS__MDEG, TIM, &
     &                                  NERS%EXP%ARG, NERS%EXP%BSPL(1-NERS__MDEG,NERS__H1) )
        ELSE IF ( CPAR == 'h2rr' ) THEN
           L_PAR = 1
           PARS(L_PAR) = EBSPL_DR2_R8 ( NERS%EXP%L_NOD, NERS__MDEG, TIM, &
     &                                  NERS%EXP%ARG, NERS%EXP%BSPL(1-NERS__MDEG,NERS__H2) )
        ELSE IF ( CPAR == 'h3rr' ) THEN
           L_PAR = 1
           PARS(L_PAR) = EBSPL_DR2_R8 ( NERS%EXP%L_NOD, NERS__MDEG, TIM, &
     &                                  NERS%EXP%ARG, NERS%EXP%BSPL(1-NERS__MDEG,NERS__H3) )
        ELSE IF ( CPAR == 'xpol' ) THEN
           L_PAR = 1
           PARS(1) = EBSPL_VAL_R8 ( NERS%EXP%L_NOD, NERS__MDEG, TIM, &
     &                              NERS%EXP%ARG, NERS%EXP%BSPL(1-NERS__MDEG,NERS__E2) )* &
     &                              RAD__TO__ARCSEC
        ELSE IF ( CPAR == 'ypol' ) THEN
           L_PAR = 1
           PARS(1) = EBSPL_VAL_R8 ( NERS%EXP%L_NOD, NERS__MDEG, TIM, &
     &                              NERS%EXP%ARG, NERS%EXP%BSPL(1-NERS__MDEG,NERS__E1) )* &
     &                              RAD__TO__ARCSEC
        ELSE IF ( CPAR == 'xpolr' ) THEN
           L_PAR = 1
           PARS(1) = EBSPL_DER_R8 ( NERS%EXP%L_NOD, NERS__MDEG, TIM, &
     &                              NERS%EXP%ARG, NERS%EXP%BSPL(1-NERS__MDEG,NERS__E2) )* &
     &                              RAD__TO__ARCSEC*86400.0D0
        ELSE IF ( CPAR == 'ypolr' ) THEN
           L_PAR = 1
           PARS(1) = EBSPL_DER_R8 ( NERS%EXP%L_NOD, NERS__MDEG, TIM, &
     &                              NERS%EXP%ARG, NERS%EXP%BSPL(1-NERS__MDEG,NERS__E1) )* &
     &                              RAD__TO__ARCSEC*86400.0D0
        ELSE IF ( CPAR == 'xpolrr' ) THEN
           L_PAR = 1
           PARS(1) = EBSPL_DR2_R8 ( NERS%EXP%L_NOD, NERS__MDEG, TIM, &
     &                              NERS%EXP%ARG, NERS%EXP%BSPL(1-NERS__MDEG,NERS__E2) )* &
     &                              RAD__TO__ARCSEC*86400.0D0**2
        ELSE IF ( CPAR == 'ypolrr' ) THEN
           L_PAR = 1
           PARS(1) = EBSPL_DR2_R8 ( NERS%EXP%L_NOD, NERS__MDEG, TIM, &
     &                              NERS%EXP%ARG, NERS%EXP%BSPL(1-NERS__MDEG,NERS__E1) )* &
     &                              RAD__TO__ARCSEC*86400.0D0**2
        ELSE IF ( CPAR == 'ut1mtai' ) THEN
           L_PAR = 1
           PARS(L_PAR) = EBSPL_VAL_R8 ( NERS%EXP%L_NOD, NERS__MDEG, TIM, &
     &                              NERS%EXP%ARG, NERS%EXP%BSPL(1-NERS__MDEG,NERS__E3) )/ &
     &                              UT1__TO__E3
        ELSE IF ( CPAR == 'ut1rat' ) THEN
           L_PAR = 1
           PARS(L_PAR) = EBSPL_DER_R8 ( NERS%EXP%L_NOD, NERS__MDEG, TIM, &
     &                              NERS%EXP%ARG, NERS%EXP%BSPL(1-NERS__MDEG,NERS__E3) )/ &
     &                              UT1__TO__E3
        ELSE IF ( CPAR == 'ut1rrr' ) THEN
           L_PAR = 1
           PARS(L_PAR) = EBSPL_DR2_R8 ( NERS%EXP%L_NOD, NERS__MDEG, TIM, &
     &                              NERS%EXP%ARG, NERS%EXP%BSPL(1-NERS__MDEG,NERS__E3) )/ &
     &                              UT1__TO__E3
        ELSE IF ( CPAR == 'lod' ) THEN
           L_PAR = 1
           PARS(L_PAR) = EBSPL_DER_R8 ( NERS%EXP%L_NOD, NERS__MDEG, TIM, &
     &                              NERS%EXP%ARG, NERS%EXP%BSPL(1-NERS__MDEG,NERS__E3) )/ &
     &                              LOD__TO__ER3
        ELSE IF ( CPAR == 'lodr' ) THEN
           L_PAR = 1
           PARS(L_PAR) = EBSPL_DR2_R8 ( NERS%EXP%L_NOD, NERS__MDEG, TIM, &
     &                              NERS%EXP%ARG, NERS%EXP%BSPL(1-NERS__MDEG,NERS__E3) )/ &
     &                              LOD__TO__ER3/86400.0D0
        ELSE IF ( CPAR == 'dpsi' ) THEN
           L_PAR = 1
           PARS(L_PAR) = EBSPL_VAL_R8 ( NERS%EXP%L_NOD, NERS__MDEG, TIM, &
     &                              NERS%EXP%ARG, NERS%EXP%BSPL(1-NERS__MDEG,NERS__DP) )
        ELSE IF ( CPAR == 'dpsir' ) THEN
           L_PAR = 1
           PARS(L_PAR) = EBSPL_DER_R8 ( NERS%EXP%L_NOD, NERS__MDEG, TIM, &
     &                              NERS%EXP%ARG, NERS%EXP%BSPL(1-NERS__MDEG,NERS__DP) )
        ELSE IF ( CPAR == 'deps' ) THEN
           L_PAR = 1
           PARS(L_PAR) = EBSPL_VAL_R8 ( NERS%EXP%L_NOD, NERS__MDEG, TIM, &
     &                              NERS%EXP%ARG, NERS%EXP%BSPL(1-NERS__MDEG,NERS__DE) )
        ELSE IF ( CPAR == 'depsr' ) THEN
           L_PAR = 1
           PARS(L_PAR) = EBSPL_DER_R8 ( NERS%EXP%L_NOD, NERS__MDEG, TIM, &
     &                              NERS%EXP%ARG, NERS%EXP%BSPL(1-NERS__MDEG,NERS__DE) )
        ELSE IF ( CPAR == 'nut' ) THEN
           L_PAR = 2
           PARS(1) = EBSPL_VAL_R8 ( NERS%EXP%L_NOD, NERS__MDEG, TIM, &
     &                              NERS%EXP%ARG, NERS%EXP%BSPL(1-NERS__MDEG,NERS__DP) )
           PARS(2) = EBSPL_VAL_R8 ( NERS%EXP%L_NOD, NERS__MDEG, TIM, &
     &                              NERS%EXP%ARG, NERS%EXP%BSPL(1-NERS__MDEG,NERS__DE) )
        ELSE IF ( CPAR == 'nutr' ) THEN
           L_PAR = 4
           PARS(1) = EBSPL_VAL_R8 ( NERS%EXP%L_NOD, NERS__MDEG, TIM, &
     &                              NERS%EXP%ARG, NERS%EXP%BSPL(1-NERS__MDEG,NERS__DP) )
           PARS(2) = EBSPL_VAL_R8 ( NERS%EXP%L_NOD, NERS__MDEG, TIM, &
     &                              NERS%EXP%ARG, NERS%EXP%BSPL(1-NERS__MDEG,NERS__DE) )
           PARS(3) = EBSPL_DER_R8 ( NERS%EXP%L_NOD, NERS__MDEG, TIM, &
     &                              NERS%EXP%ARG, NERS%EXP%BSPL(1-NERS__MDEG,NERS__DP) )
           PARS(4) = EBSPL_DER_R8 ( NERS%EXP%L_NOD, NERS__MDEG, TIM, &
     &                              NERS%EXP%ARG, NERS%EXP%BSPL(1-NERS__MDEG,NERS__DE) )
        ELSE IF ( CPAR == 'eop3' ) THEN
           L_PAR = 3
           PARS(1) = EBSPL_VAL_R8 ( NERS%EXP%L_NOD, NERS__MDEG, TIM, &
     &                              NERS%EXP%ARG, NERS%EXP%BSPL(1-NERS__MDEG,NERS__E2) )*RAD__TO__ARCSEC
           PARS(2) = EBSPL_VAL_R8 ( NERS%EXP%L_NOD, NERS__MDEG, TIM, &
     &                              NERS%EXP%ARG, NERS%EXP%BSPL(1-NERS__MDEG,NERS__E1) )*RAD__TO__ARCSEC
           PARS(3) = EBSPL_VAL_R8 ( NERS%EXP%L_NOD, NERS__MDEG, TIM, &
     &                              NERS%EXP%ARG, NERS%EXP%BSPL(1-NERS__MDEG,NERS__E3) )/ &
     &                              UT1__TO__E3
        ELSE IF ( CPAR == 'eop3r' ) THEN
           L_PAR = 6
           PARS(1) = EBSPL_VAL_R8 ( NERS%EXP%L_NOD, NERS__MDEG, TIM, &
     &                              NERS%EXP%ARG, NERS%EXP%BSPL(1-NERS__MDEG,NERS__E2) )* &
     &                              RAD__TO__ARCSEC
           PARS(2) = EBSPL_VAL_R8 ( NERS%EXP%L_NOD, NERS__MDEG, TIM, &
     &                              NERS%EXP%ARG, NERS%EXP%BSPL(1-NERS__MDEG,NERS__E1) )* &
     &                              RAD__TO__ARCSEC
           PARS(3) = EBSPL_VAL_R8 ( NERS%EXP%L_NOD, NERS__MDEG, TIM, &
     &                              NERS%EXP%ARG, NERS%EXP%BSPL(1-NERS__MDEG,NERS__E3) )/ &
     &                              UT1__TO__E3
           PARS(4) = EBSPL_DER_R8 ( NERS%EXP%L_NOD, NERS__MDEG, TIM, &
     &                              NERS%EXP%ARG, NERS%EXP%BSPL(1-NERS__MDEG,NERS__E2) )* &
     &                              RAD__TO__ARCSEC*86400.0D0
           PARS(5) = EBSPL_DER_R8 ( NERS%EXP%L_NOD, NERS__MDEG, TIM, &
     &                              NERS%EXP%ARG, NERS%EXP%BSPL(1-NERS__MDEG,NERS__E1) )* &
     &                              RAD__TO__ARCSEC*86400.0D0
           PARS(6) = EBSPL_DER_R8 ( NERS%EXP%L_NOD, NERS__MDEG, TIM, &
     &                              NERS%EXP%ARG, NERS%EXP%BSPL(1-NERS__MDEG,NERS__E3) )/ &
     &                              UT1__TO__E3*86400.0D0
        ELSE IF ( CPAR == 'eops' ) THEN
           L_PAR = 8
           PARS(1) = EBSPL_VAL_R8 ( NERS%EXP%L_NOD, NERS__MDEG, TIM, &
     &                              NERS%EXP%ARG, NERS%EXP%BSPL(1-NERS__MDEG,NERS__E2) )* &
     &                              RAD__TO__ARCSEC
           PARS(2) = EBSPL_VAL_R8 ( NERS%EXP%L_NOD, NERS__MDEG, TIM, &
     &                              NERS%EXP%ARG, NERS%EXP%BSPL(1-NERS__MDEG,NERS__E1) )* &
     &                              RAD__TO__ARCSEC
           PARS(3) = EBSPL_VAL_R8 ( NERS%EXP%L_NOD, NERS__MDEG, TIM, &
     &                              NERS%EXP%ARG, NERS%EXP%BSPL(1-NERS__MDEG,NERS__E3) )/ &
     &                              UT1__TO__E3
           PARS(4) = EBSPL_DER_R8 ( NERS%EXP%L_NOD, NERS__MDEG, TIM, &
     &                              NERS%EXP%ARG, NERS%EXP%BSPL(1-NERS__MDEG,NERS__E2) )* &
     &                              RAD__TO__ARCSEC*86400.0D0
           PARS(5) = EBSPL_DER_R8 ( NERS%EXP%L_NOD, NERS__MDEG, TIM, &
     &                              NERS%EXP%ARG, NERS%EXP%BSPL(1-NERS__MDEG,NERS__E1) )* &
     &                              RAD__TO__ARCSEC*86400.0D0
           PARS(6) = EBSPL_DER_R8 ( NERS%EXP%L_NOD, NERS__MDEG, TIM, &
     &                              NERS%EXP%ARG, NERS%EXP%BSPL(1-NERS__MDEG,NERS__E3) )/ &
     &                              UT1__TO__E3
           PARS(7) = EBSPL_VAL_R8 ( NERS%EXP%L_NOD, NERS__MDEG, TIM, &
     &                              NERS%EXP%ARG, NERS%EXP%BSPL(1-NERS__MDEG,NERS__DP) )* &
     &                              RAD__TO__ARCSEC
           PARS(8) = EBSPL_VAL_R8 ( NERS%EXP%L_NOD, NERS__MDEG, TIM, &
     &                              NERS%EXP%ARG, NERS%EXP%BSPL(1-NERS__MDEG,NERS__DE) )* &
     &                              RAD__TO__ARCSEC
        ELSE
           CALL ERR_LOG ( 4813, IUER, 'NERS_GET_SPL', 'Earth orientation '// &
     &         'parameter name '//TRIM(CPAR)//' is not supported' )
           RETURN 
      END IF
!
      CALL ERR_LOG ( 0, IUER ) 
      RETURN
      END  SUBROUTINE  NERS_GET_SPL  !#!  
