      SUBROUTINE REGR_ITE ( IMODE, N, IV, IV_EL, ITARG, TIM, VAL, SIG, COV, &
     &                      SH, DR, SIG_SH, SIG_DR, IUER )
! ************************************************************************
! *                                                                      *
! *     Routine  REGR_ITE  update parameters of linear regression: shift *
! *   for TIM=0 and drift -- parameter of inkline of linear regression   *
! *   adding or subtracting new point.                                   *
! *     If IMODE=1 then  REGR_ITE  updates previous values of parameters *
! *   of linear regression for adding observation with index ITARG.      *
! *     If IMODE=-1 then REGR_ITE  updates previous values of parameters *
! *   of linear regression for subtracting observation with index ITARG. *
! *                                                                      *
! *     If the new number of points participating in makeing regression  *
! *   is 1 than only shift is calcilated. If the number of of points is  *
! *   larger than 4 then incremented(decremented) algorithm are used,    *
! *   otherwise linear regression coeffients are computed anew.          *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *  IMODE ( INTEGER*4 ) -- Mode switcher.                               *
! *                      IMODE=1 -- incremental mode. It is assumed      *
! *                              that coefficients of regression has     *
! *                              been already calculated for points      *
! *                              without ITARG and now point ITARG is    *
! *                              added. Although if the number of used   *
! *                              points is 3 or less then regression     *
! *                              coefficients for all points including   *
! *                              ITARG will be computed anew.            *
! *                      IMODE=-1 -- decremental mode. It is assumed     *
! *                              that coefficients of regression has     *
! *                              been already calculated for all used    *
! *                              points including ITARG and now point    *
! *                              ITARG is excluded. Although if the      *
! *                              number of used after excluding ITARG is *
! *                              3 or less then regression coefficients  *
! *                              for all points excluding ITARG will be  *
! *                              computed anew.                          *
! *      N ( INTEGER*4 ) -- Overall number of points.                    *
! *     IV ( INTEGER*4 ) -- Array of sectioning. All points has value    *
! *                         IV(k) -- index of section. Dimension: N.     *
! *  IV_EL ( INTEGER*4 ) -- Value of used section. Only points which has *
! *                         IV(k) = IV_EL will be used in computation.   *
! *                         All other points will be ignored.            *
! *  ITARG ( INTEGER*4 ) -- Index of targeted point. It should belong    *
! *                         to used section. That means IV(ITARG) should *
! *                         be IV_EL.                                    *
! *    TIM ( REAL*8    ) -- Array of arguments. Dimension: N.            *
! *    VAL ( REAL*8    ) -- Array of values. Dimension: N.               *
! *    SIG ( REAL*8    ) -- Array of formal uncertainties of values.     *
! *                         Dimension: N.                                *
! *                                                                      *
! * ________________________ Output parameters: ________________________ *
! *                                                                      *
! * SIG_SH ( REAL*8    ) -- Formal uncertainty of shift. It is unscaled  *
! *                         formal uncertainty.                          *
! * SIG_DR ( REAL*8    ) -- Formal uncertainty of drift.                 *
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
! *    COV ( REAL*8    ) -- Covariance matrix of regression parameters   *
! *                         Dimension: 3.                                *
! *     SH ( REAL*8    ) -- Regression parametes: shift. Regression is   *
! *                         expressed as  V(TIM) = SH + DR*TIM           *
! *     DR ( REAL*8    ) -- Regression parametes: drift. Regression is   *
! *                         expressed as  V(TIM) = SH + DR*TIM           *
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
! *  ###  27-OCT-98    REGR_ITE    v1.0  (c)  L. Petrov  28-OCT-98  ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INTEGER*4  IMODE, N, IV(N), IV_EL, ITARG, IUER
      REAL*8     TIM(N), VAL(N), SIG(N), COV(3), SH, DR, SIG_SH, SIG_DR
      INTEGER*4  MPL
      PARAMETER   ( MPL = 3 )
      CHARACTER  STR*20
      LOGICAL*4  FTARG_OK
      INTEGER*4  IPL(MPL), NP, J1, IER, I_LEN
!
      NP = 0
      FTARG_OK = .FALSE.
      DO 410 J1=1,N ! scan all points
         IF ( IV(J1) .EQ. IV_EL ) THEN
!
! ----------- This point belongs to used subset
!
              NP = NP + 1
              IF ( NP .LE. MPL  ) IPL(NP) = J1
!
              IF ( J1 .EQ. ITARG ) THEN
!
! ---------------- We reached targeted observation
!
                   FTARG_OK = .TRUE.
                   IF ( IMODE .EQ. 1 ) THEN
                        CONTINUE
                      ELSE IF ( IMODE .EQ. -1 ) THEN
!
! --------------------- In decremention mode we exclude targeted observation
! --------------------- from further analysis
!
                        NP = NP - 1
                        IV(ITARG) = IV_EL + 999
                      ELSE
                        CALL CLRCH ( STR )
                        CALL INCH  ( IMODE, STR )
                        CALL ERR_LOG ( 4301, IUER, 'RECR_REC', 'Wrong value '// &
     &                      'of parameter IMODE: '//STR(1:I_LEN(STR))// &
     &                      ' -- (-1,1) are supported only' )
                        RETURN
                   END IF
              END IF
         END IF
 410  CONTINUE
!
      IF ( .NOT. FTARG_OK ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( ITARG, STR )
           CALL ERR_LOG ( 4302, IUER, 'RECR_REC', 'Targeted observation '// &
     &         '(index '//STR(1:I_LEN(STR))//') is not in the list of used '// &
     &         'for computation observations' )
           RETURN
      END IF
!
      IF ( NP .EQ. 1 ) THEN
!
! -------- Number of used observation is too few for making regresssion
! -------- line, but we formally calculate an "average"
!
           SH     = VAL(IPL(1))
           SIG_SH = SIG(IPL(1))
           DR     = 0.0D0
           SIG_DR = 0.0D0
         ELSE IF ( NP .EQ. 2 ) THEN
!
! -------- Special case: only two used observations
!
           CALL REGR_TWO ( N, IPL, TIM, VAL, SIG, SH, DR, SIG_SH, SIG_DR )
         ELSE IF ( NP .GT. 2  .AND. NP .LE. MPL ) THEN
!
! -------- The number of observations is less than the limit for using
! -------- recursive algorithm. Therefore we calculate anew coefficients
! -------- of linear regression
!
           CALL ERR_PASS  ( IUER, IER )
           CALL REGR_DIRE ( N, IV, IV_EL, TIM, VAL, SIG, COV, SH, DR, &
     &                      SIG_SH, SIG_DR, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 4303, IUER, 'REGR_ITE', 'Error in '// &
     &              'calculation of coefficients of linear regression '// &
     &              'using non-recursive algrorithm' )
                IV(ITARG) = IV_EL
                RETURN
           END IF
         ELSE
!
! -------- Update parameters of linear regression, their sigas and
! -------- covariance matrix using recursive algorithm
!
           CALL ERR_PASS ( IUER, IER )
           CALL REGR_UPD ( IMODE, TIM(ITARG), VAL(ITARG), SIG(ITARG), COV, &
     &                     SH, DR, SIG_SH, SIG_DR, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 4304, IUER, 'REGR_ITE', 'Error in '// &
     &              'calculation of coefficients of linear regression '// &
     &              'using recursive algrorithm' )
                IV(ITARG) = IV_EL
                RETURN
           END IF
      END IF
      IV(ITARG) = IV_EL
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  REGR_ITE  #!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE REGR_TWO ( N, IPL, TIM, VAL, SIG, SH, DR, SIG_SH, SIG_DR )
! ************************************************************************
! *                                                                      *
! *   Auxillary  routine  REGRT_REC2  calculates parameters of linear    *
! *   regression: shit and drift and their formal uncertainties over     *
! *   two points.                                                        *
! *                                                                      *
! *  ###  27-OCT-98   REGR_TWO    v1.0  (c)  L. Petrov  28-OCT-98  ###   *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INTEGER*4  N, IPL(*)
      REAL*8     TIM(N), VAL(N), SIG(N), SH, DR, SIG_SH, SIG_DR
!
      SH = VAL(IPL(1))* TIM(IPL(2)) /( TIM(IPL(2)) - TIM(IPL(1)) ) - &
     &     VAL(IPL(2))* TIM(IPL(1)) /( TIM(IPL(2)) - TIM(IPL(1)) )
      SIG_SH = DSQRT ( &
     &    SIG(IPL(1))**2 *TIM(IPL(2))**2/( TIM(IPL(2))**2 + TIM(IPL(1))**2 ) + &
     &    SIG(IPL(2))**2 *TIM(IPL(1))**2/( TIM(IPL(2))**2 + TIM(IPL(1))**2 ) &
     &               )
      DR = ( VAL(IPL(2)) - VAL(IPL(1)) )/ &
     &     ( TIM(IPL(2)) - TIM(IPL(1)) )
      SIG_DR = DSQRT( SIG(IPL(2))**2 + SIG(IPL(1))**2 ) / &
     &              ( TIM(IPL(2))    - TIM(IPL(1))    )
      RETURN
      END  !#!  REGR_TWO  #!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE REGR_DIRE ( N, IV, IV_EL, TIM, VAL, SIG, COV, SH, DR, SIG_SH, &
     &                       SIG_DR, IUER )
! ************************************************************************
! *                                                                      *
! *   Auxillary routine  REGR_DIRE  calculates parameters of linear      *
! *   regression: shift and drift and their formal uncertainties over    *
! *   N points. Sectioning array IV is used. Not all points are used     *
! *   for computatiuon of regeression coefficients -- only points for    *
! *   IV(k) = IV_EL.                                                     *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *      N ( INTEGER*4 ) -- Overall number of points.                    *
! *     IV ( INTEGER*4 ) -- Array of sectioning. All points has value    *
! *                         IV(k) -- index of section. Dimension: N.     *
! *  IV_EL ( INTEGER*4 ) -- Value of used section. Only points which has *
! *                         IV(k) = IV_EL will be used in computation.   *
! *                         All other points will be ignored.            *
! *    TIM ( REAL*8    ) -- Array of arguments. Dimension: N.            *
! *    VAL ( REAL*8    ) -- Array of values. Dimension: N.               *
! *    SIG ( REAL*8    ) -- Array of formal uncertainties of values.     *
! *                         Dimension: N.                                *
! *                                                                      *
! * ________________________ Output parameters: ________________________ *
! *                                                                      *
! *    COV ( REAL*8    ) -- Covariance matrix of regression parameters   *
! *                         Dimension: 3.                                *
! *     SH ( REAL*8    ) -- Regression parametes: shift. Regression is   *
! *                         expressed as  V(TIM) = SH + DR*TIM           *
! *     DR ( REAL*8    ) -- Regression parametes: drift. Regression is   *
! *                         expressed as  V(TIM) = SH + DR*TIM           *
! * SIG_SH ( REAL*8    ) -- Formal uncertainty of shift. It is unscaled  *
! *                         formal uncertainty.                          *
! * SIG_DR ( REAL*8    ) -- Formal uncertainty of drift.                 *
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
! *   IUER ( INTEGER*4, OPT ) -- Universal error handler.                *
! *                         Input:  switch IUER=0 -- no error messages   *
! *                                 will be generated even in the case   *
! *                                 of error. IUER=-1 -- in the case of  *
! *                                 error the message will be put on     *
! *                                 stdout.                              *
! *                         Output: 0 in the case of successful          *
! *                                 completion and non-zero in the case  *
! *                                 of error.                            *
! *                                                                      *
! *  ###  27-OCT-98   REGR_DIRE    v1.0  (c)  L. Petrov  28-OCT-98  ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INTEGER*4  N, IV(N), IV_EL, IUER
      REAL*8     TIM(N), VAL(N), SIG(N), COV(3), SH, DR, SIG_SH, SIG_DR
      REAL*8     NOR_VEC(2), EQU_VEC(2), RPT, RC
      INTEGER*4  J1, IP, IER
!
! --- Initialization
!
      CALL NOUT_R8 ( 3, COV     )
      CALL NOUT_R8 ( 2, NOR_VEC )
!
      IP = 0
      DO 410 J1=1,N ! Cycle ov all observations
         IF ( IV(J1) .EQ. IV_EL ) THEN
!
! ----------- This observation from used section. Make equations of conditions
!
              EQU_VEC(1) = 1.D0/SIG(J1)
              EQU_VEC(2) = TIM(J1)/SIG(J1)
              RPT        = VAL(J1)/SIG(J1)
!
! ----------- Make normal equations
!
              COV(1) = COV(1) + EQU_VEC(1)**2
              COV(2) = COV(2) + EQU_VEC(1)*EQU_VEC(2)
              COV(3) = COV(3) + EQU_VEC(2)**2
!
! ----------- Make normal equations
!
              NOR_VEC(1) = NOR_VEC(1) + EQU_VEC(1)*RPT
              NOR_VEC(2) = NOR_VEC(2) + EQU_VEC(2)*RPT
              IP = IP + 1
         END IF
 410  CONTINUE
!
! --- Invert normal matrix
!
      CALL ERR_PASS ( IUER, IER )
      CALL INVS     ( 2, COV, RC, IER )
      IF ( IER .NE. 0 ) THEN
           WRITE ( 6, * ) ' ip =',ip
           CALL ERR_LOG ( 4311, IUER, 'REGR_DIRE', 'Error during inversion '// &
     &         'of the normal matrix 2*2' )
           RETURN
      END IF
!
! --- Get adjustmetns ...
!
      SH     = COV(1)*NOR_VEC(1) + COV(2)*NOR_VEC(2)
      DR     = COV(2)*NOR_VEC(1) + COV(3)*NOR_VEC(2)
!
! --- ... and their formal uncertainties
!
      SIG_SH = DSQRT ( COV(1) )
      SIG_DR = DSQRT ( COV(3) )
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  REGR_DIRE  #!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE REGR_UPD ( IMODE, TIM0, VAL0, SIG0, COV, SH, DR, &
     &                      SIG_SH, SIG_DR, IUER )
! ************************************************************************
! *                                                                      *
! *     Auxillary routine  REGR_UPD  updates previous estiamtion of      *
! *   parameters of linear regression: shift and drift and their formal  *
! *   uncertainties over N points for addition (IMODE=1) or subtraction  *
! *   (IMODE=-1) of one observation. Estimates of regression             *
! *   coefficients, their uncertainties and covariance matrix are        *
! *   updatred.                                                          *
! *                                                                      *
! *    Sectioning array IV is used. Not all points are used              *
! *   for computatiuon of regeression coefficients -- only points for    *
! *   IV(k) = IV_EL.                                                     *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *  IMODE ( INTEGER*4 ) -- Overall number of points.                    *
! *      N ( INTEGER*4 ) -- Overall number of points.                    *
! *   TIM0 ( REAL*8    ) -- Argument of point under consideration.       *
! *    VAL ( REAL*8    ) -- Value of point under consideration.          *
! *    SIG ( REAL*8    ) -- Formal uncertainty of the point under        *
! *                         consideration.                               *
! *                                                                      *
! * ________________________ Output parameters: ________________________ *
! *                                                                      *
! *    COV ( REAL*8    ) -- Covariance matrix of regression parameters   *
! *                         Dimension: 3.                                *
! *     SH ( REAL*8    ) -- Regression parametes: shift. Regression is   *
! *                         expressed as  V(TIM) = SH + DR*TIM           *
! *     DR ( REAL*8    ) -- Regression parametes: drift. Regression is   *
! *                         expressed as  V(TIM) = SH + DR*TIM           *
! * SIG_SH ( REAL*8    ) -- Formal uncertainty of shift. It is unscaled  *
! *                         formal uncertainty.                          *
! * SIG_DR ( REAL*8    ) -- Formal uncertainty of drift.                 *
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
! *   IUER ( INTEGER*4, OPT ) -- Universal error handler.                *
! *                         Input:  switch IUER=0 -- no error messages   *
! *                                 will be generated even in the case   *
! *                                 of error. IUER=-1 -- in the case of  *
! *                                 error the message will be put on     *
! *                                 stdout.                              *
! *                         Output: 0 in the case of successful          *
! *                                 completion and non-zero in the case  *
! *                                 of error.                            *
! *                                                                      *
! *  ###  27-OCT-98    REGR_UPD    v1.0  (c)  L. Petrov  28-OCT-98  ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INTEGER*4  IMODE, IUER
      REAL*8     TIM0, VAL0, SIG0, COV(3), SH, DR, SIG_SH, SIG_DR
      REAL*8     EQU_VEC(2), NA(2), RPT, CQ, GAIN, UNGAIN, UNGAIN_LIMIT, EST(2)
      REAL*8     DP_VV_V
      PARAMETER  ( UNGAIN_LIMIT = 1.D0/1.D6 )
      CHARACTER  STR*32
!
! --- Forming equations of conditions for the subjected equation
!
      EQU_VEC(1) = 1.D0/SIG0
      EQU_VEC(2) = TIM0/SIG0
      RPT        = VAL0/SIG0
!
      NA(1) = COV(1)*EQU_VEC(1) + COV(2)*EQU_VEC(2)
      NA(2) = COV(2)*EQU_VEC(1) + COV(3)*EQU_VEC(2)
!
      CQ = DP_VV_V ( 2, NA, EQU_VEC )
      IF ( IMODE .EQ. 1 ) THEN
           GAIN = -1.D0/(1.D0 + CQ)
         ELSE IF ( IMODE .EQ. -1 ) THEN
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
                CALL ERR_LOG  ( 4321, IUER, 'REGR_UPD', 'Internal error: '// &
     &                         'GAIN appeared too large: 1/GAIN ='//STR )
                RETURN
           END IF
!
           GAIN = 1.D0/UNGAIN
      END IF
!
      EST(1) = SH
      EST(2) = DR
!
! --- Solution update
!
      CALL ADDC_VV ( 2, 1.D0, EST, GAIN*( DP_VV_V ( 2, EST, EQU_VEC ) - RPT ), &
     &               NA, EST )
!
      SH = EST(1)
      DR = EST(2)
!
! --- Update of global covariance matrix
!
      CALL DIAD_CVT_S ( GAIN, 2, NA, NA, COV )
!
      SIG_SH = DSQRT ( COV(1) )
      SIG_DR = DSQRT ( COV(3) )
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  REGR_UPD  #!#
