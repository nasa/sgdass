      SUBROUTINE LSPLINE_INIT ( N, TIM, IV, IVEL, SPAN, GLO_RATE_CNS, &
     &                          SEG_RATE_CNS, LSPLINE_S, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  LSPLINE_INIT  initilizes internal date structure for      *
! *   computation of linear spline. It is assumed that then coefficients *
! *   of linear spline  will be adjusted usiong LSQ and their rate of    *
! *   change will be constrainted. Sectioning array IV may be used.      *
! *   If the parameter IV is not omitted than the k-th element of the    *
! *   function will be used in calculation if and only if IV(k) = IVEL.  *
! *                                                                      *
! *     Parameters of the function F(t) will be obtained further by      *
! *   LSQ fitting:                                                       *
! *                                                                      *
! *   F(t) = R*(t-Tm)/Td + H(k)*(T(k+1) - t)/Ts + H(k+1)*(t - T(k))/Ts   *
! *                                                                      *
! *     where                                                            *
! *                                                                      *
! *      R -- estimated global rate;                                     *
! *      t -- value of the argument (time);                              *
! *     Tm -- (time) argument at the middle of the entire data set       *
! *     Td -- (time) interval of the entire data set -- value of the     *
! *           last argument minus value of the first argument;           *
! *     Ts -- (time) argument interval between to adjacent segment       *
! *           boundaries. It is assumed that all intervals are the same  *
! *           for entire data set.                                       *
! *   H(k) -- estimated value of the linear spline at the k-th segment   *
! *           boundary. Segment boundaries are numbered starting from 1. *
! *   T(k) -- value of the (time) argument at the k-th segment boundary. *
! *                                                                      *
! *   Rate of changes of the fitting function F is constrained by the    *
! *   value 0 with sigma RATE_CNS.                                       *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *         N ( INTEGER*4 ) -- Overall number of points.                 *
! *       TIM ( REAL*8    ) -- Array of arguments. Dimension: N.         *
! *        IV ( INTEGER*4, OPT ) -- Sectioning array. It contains a code *
! *                                 of the section for each observation. *
! *                                 If IV(k) = IVEL then the k-th        *
! *                                 observation will be used in          *
! *                                 computations. Otherwise it will be   *
! *                                 ignored. If parameter IV is omitted  *
! *                                 then all observations will be used.  *
! *                                 Dimension: N.                        *
! *         IVEL ( INTEGER*4 ) -- Index of the used section. Ignored if  *
! *                               parameter of IV is omitted.            *
! *         SPAN ( REAL*8    ) -- Time span between two boundaries of    *
! *                               linear spline (or time epochs).        *
! * GLO_RATE_CNS ( REAL*8    ) -- Values of sigma of constraint to be    *
! *                               imposed on the global rate of change   *
! *                               of the linear spline.                  *
! *                               0.0 values means no constraint.        *
! * SEG_RATE_CNS ( REAL*8    ) -- Values of sigma of constraint to be    *
! *                               imposed on rate of change at the       *
! *                               segments of the linear spline.         *
! *                               0.0 values means no constraint.        *
! *                                                                      *
! * ________________________ Output parameters: ________________________ *
! *                                                                      *
! * LSPLINE_S ( RECORD    ) -- Data structure used for computation of    *
! *                            linear spline.                            *
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
! *   IUER ( INTEGER*4, OPT ) -- Universal error handler.                *
! *                           Input: switch IUER=0 -- no error messages  *
! *                                  will be generated even in the case  *
! *                                  of error. IUER=-1 -- in the case of *
! *                                  error the message will be put on    *
! *                                  stdout.                             *
! *                           Output: 0 in the case of successful        *
! *                                   completion and non-zero in the     *
! *                                   case of error.                     *
! *                                                                      *
! *  ###  28-OCT-98   LSPLINE_INIT  v2.1 (c)  L. Petrov  11-NOV-98  ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'lspline.i'
      INTEGER*4  N, IUER
      INTEGER*4  IV(N), IVEL
      REAL*8     TIM(N), SPAN, GLO_RATE_CNS, SEG_RATE_CNS
      TYPE ( LSPLINE__STRU ) ::  LSPLINE_S
      CHARACTER  STR*20
      REAL*8     TBEG, TEND
      LOGICAL*4  F_IV, F_FIR
      INTEGER*4  J1, J2, IP
      INTEGER*4, EXTERNAL :: I_LEN
!
! --- Set status: undefined
!
      LSPLINE_S%STATUS = LSP__UND
!
! --- Check: whether the sectioning array IV was omitted
!
      IF ( LOC(IV(1)) .EQ. 0 ) THEN
           F_IV = .FALSE. ! IV should not be used since it ewas omitted
         ELSE
           F_IV = .TRUE.  ! IV should be used
      END IF
!
! --- Calculation the number of boundaries (time epochs)
!
      F_FIR = .FALSE.
      IP = 0
      DO 410 J1=1,N
         IF ( F_IV ) THEN
!
! ----------- Skip the point not belonging to the targeted segnent
!
              IF ( IV(J1) .NE. IVEL ) GOTO 410
         END IF
         IF ( .NOT. F_FIR ) THEN
              TBEG = TIM(J1)
              F_FIR = .TRUE.
         END IF
         TEND = TIM(J1)
         IP = IP + 1
 410  CONTINUE
!
      LSPLINE_S%DURA = TEND - TBEG
      IF ( IP .LE. 2 ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( IP, STR )
           CALL ERR_LOG ( 4411, IUER, 'LSPLINE_INIT', 'The number of '// &
     &         'observations from the targeted segment is less than minimum: '// &
     &          STR )
           RETURN
      END IF
!
      LSPLINE_S%L_LSP = IDINT ( LSPLINE_S%DURA/SPAN +0.001 ) + 2
      IF ( LSPLINE_S%L_LSP  .GT. M_LSP ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( M_LSP, STR )
           WRITE ( 6, * ) ' SPAN = ',SPAN
           CALL ERR_LOG ( 4412, IUER, 'LSPLINE_INIT', 'Attempt to take more '// &
     &         'than maxumal number of segments M_LSP=' //STR(1:I_LEN(STR))// &
     &         '. Maybe parameter SPAN is too short? Parmaeter M_LSP '// &
     &         'is defiend in lspline.i of petools library' )
           RETURN
      END IF
!
! --- Setting boundaries (time epochs)
!
      DO 420 J2=1,LSPLINE_S%L_LSP
         LSPLINE_S%EPOCH(J2) = TBEG + SPAN*(J2-1)
 420  CONTINUE
!
! --- Look can we afford overdraft of the last segment?
!
      IF ( TEND - ( LSPLINE_S%EPOCH(LSPLINE_S%L_LSP) - SPAN ) .LT. &
     &     SPAN*LSP__OVD ) THEN
!
           LSPLINE_S%L_LSP = LSPLINE_S%L_LSP - 1
      END IF
!
! --- Putting appropriate information into the LSPLINE data strucutures
!
      LSPLINE_S%SPAN         = SPAN
      LSPLINE_S%TIM_FIRST    = TBEG
      LSPLINE_S%TIM_LAST     = TEND
      LSPLINE_S%TIM_MIDDLE   = (TEND + TBEG)/2.0D0
      LSPLINE_S%GLO_RATE_CNS = GLO_RATE_CNS
      LSPLINE_S%SEG_RATE_CNS = SEG_RATE_CNS
!
! --- Set status: initialized
!
      LSPLINE_S%STATUS     = LSP__INI
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  LSPLINE_INIT  #!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE LSPLINE_CMP ( N, TIM, VAL, SIG, IV, IVEL, LSPLINE_S, IUER )
! ************************************************************************
! *                                                                      *
! *     Routine  LSPLINE_CMP  computes coefficients of linear spline     *
! *   which approximates a function by LSQ method. The function is       *
! *   specified by array of arguments TIM, array of values VAL and array *
! *   of formal uncertainties SIG. It is allowed to use not all values   *
! *   of the function but only marked as belonging to the certain        *
! *   section. Sectioning array IV may be used. If the parameter IV is   *
! *   not omitted than the k-th element of the function will be used in  *
! *   calculation if and only if IV(k) = IVEL.                           *
! *                                                                      *
! *     It is assumed that the data structure LSPLINE_S has been         *
! *   initialized already by  LSPLINE_INIT  and duration of the time     *
! *   span for one segment of the linear spline, duration of the         *
! *   experiment, value of the constraint have been already put in the   *
! *   LSPLINE_S.                                                         *
! *                                                                      *
! *     Routine  LSPLINE_CMP  only calculates coefficients of the linear *
! *   spline and global rate. There are other routines: LSPLINE_GET and  *
! *   LSPLPINE_GETSEG which provides value of spline for the specified   *
! *   (time) argument.                                                   *
! *                                                                      *
! *     Routine  LSPLINE_CMP  find parameters of regression function     *
! *   F(t) = R*(t-Tm)/Td + H(k)*(T(k+1) - t)/Ts + H(k+1)*(t - T(k))/Ts   *
! *                                                                      *
! *     where                                                            *
! *                                                                      *
! *      R -- estimated global rate;                                     *
! *      t -- value of the argument (time);                              *
! *     Tm -- (time) argument at the middle of the entire data set       *
! *     Td -- (time) interval of the entire data set -- value of the     *
! *           last argument minus value of the first argument;           *
! *     Ts -- (time) argument interval between to adjacent segment       *
! *           boundaries. It is assumed that all intervals are the same  *
! *           for entire data set.                                       *
! *   H(k) -- estimated value of the linear spline at the k-th segment   *
! *           boundary. Segment boundaries are numbered starting from 1. *
! *   T(k) -- value of the (time) argument at the k-th segment boundary. *
! *                                                                      *
! *   Rate of changes of the fitting function F is constrained by the    *
! *   value 0 with sigma RATE_CNS specified in LSPLINE_INIT.             *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *         N ( INTEGER*4 ) -- Overall number of points.                 *
! *       TIM ( REAL*8    ) -- Array of arguments. Dimension: N.         *
! *       VAL ( REAL*8    ) -- Array of values. Dimension: N.            *
! *       SIG ( REAL*8    ) -- Array of uncertainties. Dimension: N.     *
! *        IV ( INTEGER*4, OPT ) -- Sectioning array. It contains a code *
! *                                 of the section for each observation. *
! *                                 If IV(k) = IVEL then the k-th        *
! *                                 observation will be used in          *
! *                                 computations. Otherwise it will be   *
! *                                 ignored. If parameter IV is omitted  *
! *                                 then all observations will be used.  *
! *                                 Dimension: N.                        *
! *      IVEL ( INTEGER*4 ) -- Index of the used section. Ignored if     *
! *                            parameter of IV is omitted.               *
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
! * LSPLINE_S ( RECORD    ) -- Data structure used for computation of    *
! *                            linear spline.                            *
! *      IUER ( INTEGER*4, OPT ) -- Universal error handler.             *
! *                           Input: switch IUER=0 -- no error messages  *
! *                                  will be generated even in the case  *
! *                                  of error. IUER=-1 -- in the case of *
! *                                  error the message will be put on    *
! *                                  stdout.                             *
! *                           Output: 0 in the case of successful        *
! *                                   completion and non-zero in the     *
! *                                   case of error.                     *
! *                                                                      *
! * ###  28-OCT-1998  LSPLINE_CMP   v1.2 (c) L. Petrov  27-AUG-2017 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'lspline.i'
      TYPE ( LSPLINE__STRU ) ::  LSPLINE_S
      INTEGER*4  N, IUER
      REAL*8     TIM(N), VAL(N), SIG(N), CORR
      INTEGER*4  IV(N), IVEL
      REAL*8     DELTA_TIM, FORWARD_FRACTION, BACK_FRACTION, RC, EQU(MPAR_LSP)
      INTEGER*4  IND(MPAR_LSP), LL, N_SEG, N_PAR, J1, J2, J3, IPAR_CURR, &
     &           IPAR_NEXT, KP, IER
      LOGICAL*4  F_IV
      CHARACTER  STR*32, STR1*32
      INTEGER*4  I_LEN, LOCR, I, J
      LOCR(I,J)=min(I,J) +(max(I,J)*(max(I,J)-1))/2
!
! --- Check: whether the sectioning array IV was omitted
!
      IF ( LOC(IV(1)) .EQ. 0 ) THEN
           F_IV = .FALSE. ! IV should not be used since it ewas omitted
         ELSE
           F_IV = .TRUE.  ! IV should be used
      END IF
!
      IF ( LSPLINE_S%STATUS .NE. LSP__INI  .AND. &
     &     LSPLINE_S%STATUS .NE. LSP__CMP        ) THEN
!
           CALL ERR_LOG ( 4421, IUER, 'LSPLINE_CMP', 'LSPLINE_S data '// &
     &         'structures have not been initialized' )
           RETURN
      END IF
!
! --- Initialiszations
!
      CALL NOUT_R8 ( MELM_LSP, LSPLINE_S%COV )
      CALL NOUT_R8 ( MPAR_LSP, LSPLINE_S%DSP )
!
      N_SEG = 1 ! initial value of time epoch counter
      N_PAR = 1+LSPLINE_S%L_LSP ! total number of parameters to be estimated
      KP = 0
!
      DO 410 J1=1,N
!
! ------ Zero the array of equation of conditions
!
         CALL NOUT_R8 ( MPAR_LSP, EQU )
!
! ------ Set partial derivative for global rate
!
         IND(1) = 1
         EQU(1) = ( TIM(J1) - LSPLINE_S%TIM_MIDDLE )/LSPLINE_S%DURA
!
! ------ Check: did we overcome the next time epoch boundary and therefore we
! ------ have to increment a segment counter
!
         IF ( N_SEG .LT. LSPLINE_S%L_LSP ) THEN
              N_SEG = IDINT ( ( TIM(J1) - LSPLINE_S%EPOCH(1) )/ &
     &                        LSPLINE_S%SPAN ) + 1
              IF ( N_SEG .GT. LSPLINE_S%L_LSP ) THEN
                   N_SEG = N_SEG - 1
              END IF
         END IF
!
! ------ Look: should we bypass the J1-th point
!
         IF ( F_IV ) THEN
              IF ( IV(J1) .NE. IVEL ) GOTO 410
         END IF
!
         DELTA_TIM        = TIM(J1) - LSPLINE_S%TIM_FIRST
         FORWARD_FRACTION = DELTA_TIM/LSPLINE_S%SPAN - (N_SEG-1)
         BACK_FRACTION    = 1.D0 - FORWARD_FRACTION
!
! ------ Set partial derivatives for linear spline
!
         IND(2) = 1+N_SEG
         IND(3) = 1+N_SEG+1
         EQU(1+N_SEG)   = BACK_FRACTION
         EQU(1+N_SEG+1) = FORWARD_FRACTION
!
! ------ Update normal matrix and normal vector for the j1-th observation
!
         CALL DIAD_CVT_S ( 1.D0/SIG(J1)**2, N_PAR, EQU, EQU, LSPLINE_S%COV )
         CALL ADDC_VV    ( N_PAR, VAL(J1)/SIG(J1)**2, EQU, 1.D0, LSPLINE_S%DSP, &
     &                     LSPLINE_S%DSP )
         KP = KP + 1
  410 CONTINUE
!
! --- Check: do we have enough parameters to find adjustments?
!
      IF ( KP .LT. N_PAR ) THEN
           CALL CLRCH ( STR     )
           CALL INCH  ( KP, STR )
           CALL CLRCH ( STR1    )
           CALL INCH  ( LSPLINE_S%L_LSP-1, STR1 )
           CALL ERR_LOG ( 4422, IUER, 'LSPLINE_CMP', 'Number of used points: '// &
     &          STR(1:I_LEN(STR))//' is not enough to determine coefficient '// &
     &         'of the linear spline with '//STR1(1:I_LEN(STR1))// &
     &         ' segments. Try to increase segment span' )
           RETURN
      END IF
!
! --- Impose constraints on global rate
!
      IF ( LSPLINE_S%GLO_RATE_CNS .GT. CNST__MIN ) THEN
           CORR = 1.D0/(LSPLINE_S%GLO_RATE_CNS*LSPLINE_S%DURA)**2
           LSPLINE_S%COV(1) = LSPLINE_S%COV(1) + CORR
      END IF
!
! --- Impose constraints on rate between the segments
!
      IF ( LSPLINE_S%SEG_RATE_CNS .GT. CNST__MIN ) THEN
           CORR = 1.D0/(LSPLINE_S%SEG_RATE_CNS*LSPLINE_S%SPAN)**2 ! value to be
!                       added or subtracted from diagonal or near diagonal
!                       terms of normal matrix
           DO 420 J2=1,LSPLINE_S%L_LSP-1
              IPAR_CURR = 1 + J2
              IPAR_NEXT = 1 + J2+1
!
              LSPLINE_S%COV( LOCR(IPAR_CURR,IPAR_CURR) ) = &
     &        LSPLINE_S%COV( LOCR(IPAR_CURR,IPAR_CURR) ) + CORR
!
              LSPLINE_S%COV( LOCR(IPAR_CURR,IPAR_NEXT) ) = &
     &        LSPLINE_S%COV( LOCR(IPAR_CURR,IPAR_NEXT) ) - CORR
!
              LSPLINE_S%COV( LOCR(IPAR_NEXT,IPAR_NEXT) ) = &
     &        LSPLINE_S%COV( LOCR(IPAR_NEXT,IPAR_NEXT) ) + CORR
 420       CONTINUE
      END IF
!
! --- Invert normal matrix
!
      CALL ERR_PASS ( IUER, IER )
      CALL INVS     ( N_PAR, LSPLINE_S%COV, RC, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 4423, IUER, 'LSPLINE_CMP', 'Error during '// &
     &                   'inversion of normal matrix' )
           RETURN
      END IF
!
! --- Find estimates of linear spline coefficients
!
      CALL MUL_MV_SV_V ( N_PAR, LSPLINE_S%COV, N_PAR, LSPLINE_S%DSP, N_PAR, &
     &                   LSPLINE_S%EST, IER )
!
! --- Find formal uncertainties of linear spline coefficients
!
      LL = 1
      DO 430 J3=1,N_PAR
         LSPLINE_S%DSP(J3) = DSQRT ( LSPLINE_S%COV(LL) )
         LL = LL + J3+1
 430  CONTINUE
!
! --- Set status: deal done
!
      LSPLINE_S%STATUS = LSP__CMP
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  LSPLINE_CMP  #!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE LSPLINE_GET ( TIME, LSPLINE_S, VAL, SIG, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  LSPLINE_GET  computes value of approximating linear       *
! *   spline and its formal uncertainty for the argument TIME. It is     *
! *   assumed that the parameters of the linear spline have been         *
! *   obtained earlier by calling firstly LSPLINE_INIT and them          *
! *   LSPLINE_CMP.                                                       *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *      TIME ( REAL*8    ) -- (time) Argument to which the value of     *
! *                            approximating function is obtained.       *
! * LSPLINE_S ( RECORD    ) -- Data structure used for computation of    *
! *                            linear spline.                            *
! *                                                                      *
! * ________________________ Output parameters: ________________________ *
! *                                                                      *
! *       VAL ( REAL*8    ) -- Value of the approximating function at    *
! *                            the (time) argument.                      *
! *       SIG ( REAL*8    ) -- Formal uncertainty of the value of the    *
! *                            approximating function.                   *
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
! *      IUER ( INTEGER*4, OPT ) -- Universal error handler.             *
! *                           Input: switch IUER=0 -- no error messages  *
! *                                  will be generated even in the case  *
! *                                  of error. IUER=-1 -- in the case of *
! *                                  error the message will be put on    *
! *                                  stdout.                             *
! *                           Output: 0 in the case of successful        *
! *                                   completion and non-zero in the     *
! *                                   case of error.                     *
! *                                                                      *
! *  ###  29-OCT-98   LSPLINE_GET  v1.2  (c)  L. Petrov  03-FEB-99 # ##  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'lspline.i'
      TYPE ( LSPLINE__STRU ) ::  LSPLINE_S
      REAL*8     TIME, VAL, SIG
      REAL*8     VEC(3), VEC_TMP(3), MAT(6)
      INTEGER*4  IUER
      INTEGER*4  J1, CURR_SEG, NEXT_SEG, IER
      CHARACTER  STR*32, STR1*32
      REAL*8,    EXTERNAL :: DP_VV_V
      INTEGER*4, EXTERNAL :: I_LEN, ILEN
      INTEGER*4  LOCR, I, J
      LOCR(I,J)=min(I,J) +(max(I,J)*(max(I,J)-1))/2
!
! --- Check: do we have spline coefficients?
!
      IF ( LSPLINE_S%STATUS .NE. LSP__CMP ) THEN
           CALL ERR_LOG ( 4431, IUER, 'LSPLINE_CMP', 'Coefficitents of '// &
     &         'linear spline have not been yet computed' )
           RETURN
      END IF
!
! --- Check: is the number of segments correct
!
      IF ( LSPLINE_S%L_LSP .LE. 1 ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( LSPLINE_S%L_LSP, STR )
           CALL ERR_LOG ( 4432, IUER, 'LSPLINE_CMP', 'The number of segments '// &
     &         'for linear spline is too small: '//STR )
           RETURN
      END IF
!
      IF ( LSPLINE_S%L_LSP .GT. M_LSP ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( LSPLINE_S%L_LSP, STR )
           CALL CLRCH ( STR1 )
           CALL INCH  ( M_LSP, STR1 )
           CALL ERR_LOG ( 4433, IUER, 'LSPLINE_CMP', 'The number of segments '// &
     &         'for linear spline is too big: '//STR(1:I_LEN(STR))//' what '// &
     &         'exceeds the maximal number defined in include block '// &
     &         'lspline.i M_LSP='//STR1 )
           RETURN
      END IF
!
! --- Scan epochs and find appropriate time epoch:
! --- TIME should belong interval [CURR_SEG, NEXT_SEG)
!
      DO 410 J1=1,LSPLINE_S%L_LSP
         IF ( TIME .GE. LSPLINE_S%EPOCH(J1)   .AND. &
     &        TIME .LT. LSPLINE_S%EPOCH(J1+1)       ) THEN
!
              CURR_SEG = J1
              GOTO 810
         END IF
 410  CONTINUE
 810  CONTINUE
      IF ( TIME .LT. LSPLINE_S%EPOCH(1) ) THEN
           CURR_SEG = 1
      END IF
      IF ( TIME .GE. LSPLINE_S%EPOCH(LSPLINE_S%L_LSP) ) THEN
           CURR_SEG = LSPLINE_S%L_LSP-1
      END IF
      NEXT_SEG = CURR_SEG + 1
!
! --- Value of linear spline is a sum of 3 adjustments obtained by LSQ:
! --- 1) global rate; 2) value of spline at CURR_SEG epoch; 3) value
! --- of spline ar NEXT_SEG epoch.
! --- Put coefficients before these adjustments to VEC array
!
      VEC(1) = ( TIME - LSPLINE_S%TIM_MIDDLE )/LSPLINE_S%DURA
      VEC(2) = (LSPLINE_S%EPOCH(NEXT_SEG) - TIME)/LSPLINE_S%SPAN
      VEC(3) = (TIME - LSPLINE_S%EPOCH(CURR_SEG))/LSPLINE_S%SPAN
!
! --- Gather estimates together
!
      VEC_TMP(1) = LSPLINE_S%EST(1)
      VEC_TMP(2) = LSPLINE_S%EST(1+CURR_SEG)
      VEC_TMP(3) = LSPLINE_S%EST(1+NEXT_SEG)
!
! --- Calculation of dot product for producing value of linear spline
!
      VAL = DP_VV_V ( 3, VEC, VEC_TMP )
!
! --- Gather elements of entire covariance matrix to matrix 3 by 3
!
      MAT( LOCR(1,1) ) = LSPLINE_S%COV( LOCR(1,1) )
      MAT( LOCR(1,2) ) = LSPLINE_S%COV( LOCR(1,1+CURR_SEG) )
      MAT( LOCR(1,3) ) = LSPLINE_S%COV( LOCR(1,1+NEXT_SEG) )
      MAT( LOCR(2,2) ) = LSPLINE_S%COV( LOCR(1+CURR_SEG,1+CURR_SEG) )
      MAT( LOCR(2,3) ) = LSPLINE_S%COV( LOCR(1+CURR_SEG,1+NEXT_SEG) )
      MAT( LOCR(3,3) ) = LSPLINE_S%COV( LOCR(1+NEXT_SEG,1+NEXT_SEG) )
!
! --- Calculation of formal uncertainty of plinear spline value:
! --- SIG = DSQRT ( vec(T) * MAT * vec )
! --- We do it by such a manner since we have to take into account correlation
! --- between the estimates of spline coefficients and global rate
!
      CALL MUL_MV_SV_V ( 3, MAT, 3, VEC, 3, VEC_TMP, IER )
      SIG = DSQRT ( DP_VV_V ( 3, VEC, VEC_TMP ) )
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  LSPLINE_GET  #!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE LSPLINE_GETSEG ( M, LSPLINE_S, L, TIM, VAL, SIG, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  LSPLINE_GETSEG  computes values of the linear spline      *
! *   approximating at the boundary points. Approximating function is    *
! *   a continuous piesewise linear function which lays between the      *
! *   boundaries. An array of (time) arguments, values and formal        *
! *   uncertainties of the approximating function is computed.           *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *         M ( INTEGER*4 ) -- Maximal expected number of boundaries.    *
! * LSPLINE_S ( RECORD    ) -- Data structure used for computation of    *
! *                            linear spline.                            *
! *                                                                      *
! * ________________________ Output parameters: ________________________ *
! *                                                                      *
! *         L ( INTEGER*4 ) -- Number of boundaries.                     *
! *       TIM ( REAL*8    ) -- Array of the (time) arguments of the      *
! *                            boundaries for the linear spline.         *
! *                            Input dimension: M. Actiual dimension: L. *
! *       VAL ( REAL*8    ) -- Array of values of the approximating      *
! *                            function at the boundaries of (time)      *
! *                            argument.                                 *
! *                            Input dimension: M. Actiual dimension: L. *
! *       SIG ( REAL*8    ) -- Formal uncertainty of the value of the    *
! *                            approximating function at the boundaries  *
! *                            of (time) argument.                       *
! *                            Input dimension: M. Actiual dimension: L. *
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
! *      IUER ( INTEGER*4, OPT ) -- Universal error handler.             *
! *                           Input: switch IUER=0 -- no error messages  *
! *                                  will be generated even in the case  *
! *                                  of error. IUER=-1 -- in the case of *
! *                                  error the message will be put on    *
! *                                  stdout.                             *
! *                           Output: 0 in the case of successful        *
! *                                   completion and non-zero in the     *
! *                                   case of error.                     *
! *                                                                      *
! *  ###  29-OCT-98  LSPLINE_GETSEG  v1.0 (c) L. Petrov  29-OCT-98  ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'lspline.i'
      TYPE ( LSPLINE__STRU ) ::  LSPLINE_S
      INTEGER*4  M, L, IUER
      REAL*8     TIM(M), VAL(M), SIG(M)
      CHARACTER  STR*32, STR1*32
      INTEGER*4  J1, IER
      INTEGER*4, EXTERNAL :: I_LEN
!
! --- Check: do we have spline coefficients?
!
      IF ( LSPLINE_S%STATUS .NE. LSP__CMP ) THEN
           CALL ERR_LOG ( 4441, IUER, 'LSPLINE_GETSEG', 'Coefficitents of '// &
     &         'linear spline have not been yet computed' )
           RETURN
      END IF
!
! --- Get number of time epochs
!
      L = LSPLINE_S%L_LSP
      IF ( L .GT. M ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( M, STR )
           CALL CLRCH ( STR1 )
           CALL INCH  ( L, STR1 )
           CALL ERR_LOG ( 4442, IUER, 'LSPLINE_GETSEG', 'Parameter M ='// &
     &          STR(1:I_LEN(STR))//' is too small. Actual number of time '// &
     &         'epochs is '//STR1 )
           RETURN
      END IF
!
! --- Cycle over time segments
!
      DO 410 J1=1,L
         TIM(J1) = LSPLINE_S%EPOCH(J1)
!
         CALL ERR_PASS ( IUER, IER )
         CALL LSPLINE_GET ( TIM(J1), LSPLINE_S, VAL(J1), SIG(J1), IER )
         IF ( IER .NE. 0 ) THEN
              CALL CLRCH (     STR )
              CALL INCH  ( J1, STR )
              CALL ERR_LOG ( 4443, IUER, 'LSPLINE_GETSEG', 'Error in '// &
     &            'computation of linear spline for the '//STR(1:I_LEN(STR))// &
     &            '-th segment' )
              RETURN
         END IF
 410  CONTINUE
!
! --- In the case when overdraft of the last time epoch had place we have to
! --- compute linear spline for the last point additionally
!
      IF ( LSPLINE_S%TIM_LAST - LSPLINE_S%EPOCH(L) .GT. &
     &     SHARE__OVD*LSPLINE_S%SPAN ) THEN
!
! -------- Add aditional bounadry
!
           L = L+1
           IF ( L .GT. M ) THEN
                CALL CLRCH ( STR )
                CALL INCH  ( M, STR )
                CALL CLRCH ( STR1 )
                CALL INCH  ( L, STR1 )
                CALL ERR_LOG ( 4444, IUER, 'LSPLINE_GETSEG', 'Parameter M ='// &
     &               STR(1:I_LEN(STR))//' is too small. Actual number time '// &
     &              'arguments is '//STR1 )
                RETURN
           END IF
!
! -------- Compute the value of approximating function at this boundary point
!
           TIM(L) = LSPLINE_S%TIM_LAST
           CALL ERR_PASS ( IUER, IER )
           CALL LSPLINE_GET ( TIM(L), LSPLINE_S, VAL(L), SIG(L), IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 4445, IUER, 'LSPLINE_GETSEG', 'Error in '// &
     &              'computation of linear spline for the last moment of '// &
     &              'time' )
                RETURN
           END IF
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  LSPLINE_GETSEG  #!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE LSPLINE_GETSEGRAT ( M, LSPLINE_S, L, TIM, RAT, SIG, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  LSPLINE_GETSEGRAT  computes values of the rate of linear  *
! *   spline at the boundary points. Approximating function is a         *
! *   continuous piesewise linear function which lays between the        *
! *   boundaries. An array of (time) arguments, values and formal        *
! *   uncertainties of the first derivative of the approximating         *
! *   function is computed.                                              *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *         M ( INTEGER*4 ) -- Maximal expected number of boundaries.    *
! * LSPLINE_S ( RECORD    ) -- Data structure used for computation of    *
! *                            linear spline.                            *
! *                                                                      *
! * ________________________ Output parameters: ________________________ *
! *                                                                      *
! *         L ( INTEGER*4 ) -- Number of boundaries.                     *
! *       TIM ( REAL*8    ) -- Array of the (time) arguments of the      *
! *                            boundaries for the linear spline.         *
! *                            Input dimension: M. Actiual dimension: L. *
! *       RAT ( REAL*8    ) -- Array of the first derivatives (rates) of *
! *                            the approximating function at the interval*
! *                            which follows by the boundary point for   *
! *                            the argument.                             *
! *                            Input dimension: M. Actiual dimension: L. *
! *       SIG ( REAL*8    ) -- Formal uncertainty of the value of the    *
! *                            first derivative of approximating         *
! *                            function at the boundaries of (time)      *
! *                            argument. Cross-correlation between time  *
! *                            epoch is negleceted when its value is     *
! *                            evaluated.                                *
! *                                                                      *
! *                            Input dimension: M. Actiual dimension: L. *
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
! *      IUER ( INTEGER*4, OPT ) -- Universal error handler.             *
! *                           Input: switch IUER=0 -- no error messages  *
! *                                  will be generated even in the case  *
! *                                  of error. IUER=-1 -- in the case of *
! *                                  error the message will be put on    *
! *                                  stdout.                             *
! *                           Output: 0 in the case of successful        *
! *                                   completion and non-zero in the     *
! *                                   case of error.                     *
! *                                                                      *
! * ### 16-NOV-1998 LSPLINE_GETSEGRAT v1.1 (c) L. Petrov 16-AUG-2017 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'lspline.i'
      TYPE ( LSPLINE__STRU ) ::  LSPLINE_S
      INTEGER*4  M, L, IUER
      REAL*8     TIM(M), RAT(M), SIG(M), VALB, VALE, SIGB, SIGE
      CHARACTER  STR*32, STR1*32
      INTEGER*4  J1, IER
      INTEGER*4, EXTERNAL :: I_LEN
!
! --- Check: do we have spline coefficients?
!
      IF ( LSPLINE_S%STATUS .NE. LSP__CMP ) THEN
           CALL ERR_LOG ( 4451, IUER, 'LSPLINE_GETSEGRAT', 'Coefficitents of '// &
     &         'linear spline have not been yet computed' )
           RETURN
      END IF
!
! --- Get number of time epochs
!
      L = LSPLINE_S%L_LSP
      IF ( L .GT. M ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( M, STR )
           CALL CLRCH ( STR1 )
           CALL INCH  ( L, STR1 )
           CALL ERR_LOG ( 4452, IUER, 'LSPLINE_GETSEGRAT', 'Parameter M ='// &
     &          STR(1:I_LEN(STR))//' is too small. Actual number of time '// &
     &         'epochs is '//STR1 )
           RETURN
      END IF
!
! --- Cycle over time segments
!
      DO 410 J1=1,L-1
         TIM(J1)   = LSPLINE_S%EPOCH(J1)
         TIM(J1+1) = LSPLINE_S%EPOCH(J1+1)
!
         CALL ERR_PASS ( IUER, IER )
         CALL LSPLINE_GET ( TIM(J1), LSPLINE_S, VALB, SIGB, IER )
         IF ( IER .NE. 0 ) THEN
              CALL CLRCH (     STR )
              CALL INCH  ( J1, STR )
              CALL ERR_LOG ( 4453, IUER, 'LSPLINE_GETSEGRAT', 'Error in '// &
     &            'computation of linear spline for the '//STR(1:I_LEN(STR))// &
     &            '-th segment' )
              RETURN
         END IF
!
         CALL ERR_PASS ( IUER, IER )
         CALL LSPLINE_GET ( TIM(J1+1), LSPLINE_S, VALE, SIGE, IER )
         IF ( IER .NE. 0 ) THEN
              CALL CLRCH (     STR )
              CALL INCH  ( J1, STR )
              CALL ERR_LOG ( 4453, IUER, 'LSPLINE_GETSEGRAT', 'Error in '// &
     &            'computation of linear spline for the '//STR(1:I_LEN(STR))// &
     &            '-th segment' )
              RETURN
         END IF
!
         RAT(J1) = (VALE - VALB)/LSPLINE_S%SPAN
         SIG(J1) = DSQRT ( SIGB**2 + SIGE**2) /LSPLINE_S%SPAN
 410  CONTINUE
      RAT(L) = RAT(L-1)
      SIG(L) = SIG(L-1)
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  LSPLINE_GETSEGRAT  #!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE LSPLINE_UPD ( IP, TIM, VAL, SIG, LSPLINE_S, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  LSPLINE_UPD  updates coefficients of the linear spline    *
! *   for adding (IP=1) or subtracting (IP=-1) additional point with     *
! *   argument TIME, value VAL and formal uncertainty SIG. It is assumed *
! *   that the coefficients of the linear spline have been already       *
! *   computed by LSPLINE_INIT and LSPLINE_CMP.                          *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *        IP ( INTEGER*4 ) -- Mode switcher.                            *
! *                         IP=-1 -- coeffitiets of the approximating    *
! *                                  function (linear spline) will be    *
! *                                  updated for exclusion of one point  *
! *                                  NB: it is assumed that this point   *
! *                                  was used in computation of the      *
! *                                  linear spline earlier. No checks    *
! *                                  can be done. If it is not true then *
! *                                  result WILL NOT BE CORRECT!!!       *
! *                         IP= 1 -- coeffitiets of the approximating    *
! *                                  function (linear spline) will be    *
! *                                  updated for inclusiuon of the new   *
! *                                  point;                              *
! *      TIME ( REAL*8    ) -- (time) Argument to which the value of     *
! *                            approximating function is obtained.       *
! *                                                                      *
! * ________________________ Output parameters: ________________________ *
! *                                                                      *
! *       VAL ( REAL*8    ) -- Value of the point under consideration.   *
! *       SIG ( REAL*8    ) -- Formal uncertainty of the point under     *
! *                            consideration.                            *
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
! * LSPLINE_S ( RECORD    ) -- Data structure used for computation of    *
! *                            linear spline.                            *
! *      IUER ( INTEGER*4, OPT ) -- Universal error handler.             *
! *                           Input: switch IUER=0 -- no error messages  *
! *                                  will be generated even in the case  *
! *                                  of error. IUER=-1 -- in the case of *
! *                                  error the message will be put on    *
! *                                  stdout.                             *
! *                           Output: 0 in the case of successful        *
! *                                   completion and non-zero in the     *
! *                                   case of error.                     *
! *                                                                      *
! *  ###  29-OCT-1998  LSPLINE_UPD  v1.1  (c) L. Petrov 16-AUG-2017 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'lspline.i'
      TYPE ( LSPLINE__STRU ) ::  LSPLINE_S
      INTEGER*4  IP, IUER
      REAL*8     TIM, VAL, SIG
      REAL*8     EQU_VEC(MPAR_LSP), NA(MPAR_LSP), RPT, CQ, GAIN, UNGAIN, &
     &           UNGAIN_LIMIT
      PARAMETER  ( UNGAIN_LIMIT = 1.D0/1.D9 )
      CHARACTER  STR*32
      INTEGER*4  J1, J2, LL, CURR_SEG, NEXT_SEG, N_PAR, IER
      INTEGER*4, EXTERNAL :: I_LEN
      REAL*8,    EXTERNAL :: DP_VV_V
!
! --- Check: do we have spline coefficients?
!
      IF ( LSPLINE_S%STATUS .NE. LSP__CMP ) THEN
           CALL ERR_LOG ( 4461, IUER, 'LSPLINE_UPD', 'Coefficitents of '// &
     &         'linear spline have not been yet computed' )
           RETURN
      END IF
!
! --- Scan epochs and find appropriate time epoch:
! --- TIM should belong to interval [CURR_SEG, NEXT_SEG)
!
      CURR_SEG = 1
      DO 410 J1=1,LSPLINE_S%L_LSP
         CURR_SEG = J1
         IF ( TIM .GE. LSPLINE_S%EPOCH(J1)    .AND. &
     &        TIM .LT. LSPLINE_S%EPOCH(J1+1)         ) GOTO 810
 410  CONTINUE
 810  CONTINUE
      NEXT_SEG = CURR_SEG + 1
!
      N_PAR = 1+LSPLINE_S%L_LSP ! total number of parameters to be estimated
!
! --- Initiialization of the vector of equation of condition
!
      CALL NOUT_R8 ( N_PAR, EQU_VEC )
!
! --- Forming equation of condition for the point under consideration
!
      EQU_VEC(1)          = ( TIM - LSPLINE_S%TIM_MIDDLE )/LSPLINE_S%DURA/SIG
      EQU_VEC(1+CURR_SEG) = (LSPLINE_S%EPOCH(NEXT_SEG) - TIM)/LSPLINE_S%SPAN/SIG
      EQU_VEC(1+NEXT_SEG) = (TIM - LSPLINE_S%EPOCH(CURR_SEG))/LSPLINE_S%SPAN/SIG
!
! --- Right part
!
      RPT                 = VAL/SIG
!
! --- NA = Cov * Equ_vec , where COV former covarianvce matrix
!
      CALL MUL_MV_SV_V ( N_PAR, LSPLINE_S%COV, N_PAR, EQU_VEC, N_PAR, NA, IER )
!
! --- Cq = Equ_vec(T) * Cov * Equ_vec
!
      CQ = DP_VV_V ( N_PAR, NA, EQU_VEC )
!
! --- Computation GAIN. It depends on the mode. Special care is taken to avoid
! --- dividing zero. The latter case may occur if the rank of the normal matrix
! --- becomes deficient
!
      IF ( IP .EQ. 1 ) THEN
           GAIN = -1.D0/(1.D0 + CQ)
         ELSE IF ( IP .EQ. -1 ) THEN
!
! -------- Such a rare situation is possible: since residuals have not
! -------- been updated after correction solution for the previous
! -------- outlier, gain may appear too large. So large that computational
! -------- problems may occur. To prevent the loss of precision we make
! -------- a test: isn't the gain too large?
!
           UNGAIN = ( 1.D0 - CQ )
           IF ( DABS(UNGAIN) .LT. UNGAIN_LIMIT ) THEN
                CALL CLRCH ( STR )
                WRITE ( UNIT=STR, FMT='(1PE15.7)' ) UNGAIN
                CALL CHASHL ( STR )
                CALL ERR_LOG  ( 4462, IUER, 'LSPLINE_UPD', 'Internal error: '// &
     &                         'GAIN appeared too large: 1/GAIN ='//STR )
                RETURN
           END IF
!
           GAIN = 1.D0/UNGAIN
         ELSE
           CALL CLRCH ( STR )
           CALL INCH  ( IP, STR )
           CALL ERR_LOG ( 4463, IUER, 'LSPLINE_UPD', 'Wrong value of mode '// &
     &                   'switch IP: '//STR(1:I_LEN(STR))//'. Only values '// &
     &                   ' (-1,1) are supported' )
           RETURN
      END IF
!
! --- Solution update: X = X + Cov * Equ_vec * Gain * (Est(T) * Equ_vec - Rpt )
!
      CALL ADDC_VV ( N_PAR, 1.D0, LSPLINE_S%EST, &
     &               GAIN*( DP_VV_V ( N_PAR, LSPLINE_S%EST, EQU_VEC ) - RPT ), &
     &               NA, LSPLINE_S%EST )
!
! --- Update of global covariance matrix:
! --- Cov(new) = Cov + Equ_vec(T) * Cov * Gain * Cov * Equ_vec
!
      CALL DIAD_CVT_S ( GAIN, N_PAR, NA, NA, LSPLINE_S%COV )
!
! --- Update of formal uncertainties of adjustments
!
      LL = 1
      DO 420 J2=1,N_PAR
         IF ( LSPLINE_S%COV(LL) < 0.0D0 ) THEN
              WRITE ( 6, * ) 'N_PAR= ', N_PAR, ' CURR_SEG= ', CURR_SEG, ' SIG= ', SIG
              WRITE ( 6, * ) 'TIM= ', TIM, ' VAL= ', VAL, ' SPAN= ', LSPLINE_S%SPAN
              WRITE ( 6, * ) 'LSPLINE_S%EPOCH = ', LSPLINE_S%EPOCH(1:N_PAR)
              WRITE ( 6, * ) 'EQU_VEC = ', EQU_VEC(1:LSPLINE_S%L_LSP+1)
              WRITE ( 6, * ) 'NA = ', NA(1:N_PAR)
              WRITE ( 6, * ) 'NA(1) = ', NA(1), ' Ga= ', GAIN, ' NA*NA*GAIN= ', NA(1)**2*GAIN
              CALL CLRCH ( STR )
              CALL INCH  ( J2, STR )
              CALL ERR_LOG ( 4464, IUER, 'LSPLINE_UPD', 'Trap of internal '// &
     &            'control: negative element in the covariance matrix '// &
     &            'J2= '//STR )
              RETURN 
         END IF
         LSPLINE_S%DSP(J2) = DSQRT ( LSPLINE_S%COV(LL) )
         LL = LL + J2+1
 420  CONTINUE
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  LSPLINE_UPD  #!#
