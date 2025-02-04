      SUBROUTINE LSQW ( N, M, A, B, W, X, DISP, COV, RC, SIG, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  LSQW  solves system of linear equation using weighted    *
! *   LSQ method. It returns vector of adjustments and vector of         *
! *   estimates of their uncertainties. Uncertainties are scaled by      *
! *   square root of ratio of chi-square to the number of degrees of     *
! *   freedom.                                                           *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *    N ( INTEGER*4 ) -- Number of parameters.                          *
! *    M ( INTEGER*4 ) -- Number of equations.                           *
! *    A ( REAL*8    ) -- Matrix of equations of conditions as array     *
! *                       N*M. NB: k-th equation located in the k-th     *
! *                       column of the array A.                         *
! *    B ( REAL*8    ) -- Vector of the right parts of equations of      *
! *                       conditions.                                    *
! *    W ( REAL*8, OPT ) -- Vector of weights of equations with M        *
! *                         elements. Weights are reciprocal to          *
! *                         "a priori sigmas". If argument W is omitted  *
! *                         then all weights are considered to be equal  *
! *                         to 1.                                        *
! *                                                                      *
! * ________________________ Output parameters: ________________________ *
! *                                                                      *
! *    X ( REAL*8    ) -- Vector of adjustments (Dimension: N).          *
! * DISP ( REAL*8    ) -- Vector of adjustments (Dimension: N).          *
! *  COV ( REAL*8    ) -- Correlation matrix.                            *
! *   RC ( REAL*8    ) -- Condition number for normal system.            *
! *  SIG ( REAL*8    ) -- Mean error of the unit weight.                 *
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
! *          IUER ( INTEGER*4, OPT ) -- Universal error handler.         *
! *                           Input: switch IUER=0 -- no error messages  *
! *                                  will be generated even in the case  *
! *                                  of error. IUER=-1 -- in the case of *
! *                                  error the message will be put on    *
! *                                  stdout.                             *
! *                           Output: 0 in the case of successful        *
! *                                   completion and non-zero in the     *
! *                                   case of error.                     *
! *                                                                      *
! *  ###  20-MAR-1989     LSQW     v1.2 (c)  L. Petrov  10-AUG-2006 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INTEGER*4  M, N, IUER
      REAL*8     A(N,M), B(M), W(M), X(N), DISP(N), COV(*), RC, SIG
      REAL*8     WEI_MIN, WEI_MAX
      PARAMETER  ( WEI_MIN = 1.D-50 )
      PARAMETER  ( WEI_MAX = 1.D50  )
!
      REAL*8     SUSQ, FREDEG
      INTEGER*4  IER, J1, J2, J3, J4, J5, LL
      REAL*8,    EXTERNAL :: DP_VV_V
!
      IF ( LOC(W(1)) .NE. 0 ) THEN
!
! -------- Weight equations
!
           DO 410 J1=1,M
              IF ( DABS(W(J1)) < WEI_MAX ) THEN
                   DO 420 J2=1,N
                      A(J2,J1) = A(J2,J1)*W(J1)
 420               CONTINUE
                   B(J1) = B(J1)*W(J1)
              END IF
 410       CONTINUE
      END IF
!
! --- Make normal matrix
!
      IER=-1
      CALL MUL_MM_IT_S ( N, M, A, N, M, A, N, COV, IER )
!
! --- Make normal vector
!
      IER=-1
      CALL MUL_MV_IV_V ( N, M, A, M, B, N, DISP, IER )
!
! --- Calculate weighted sum of squares of right parts
!
      SUSQ = DP_VV_V ( M, B, B )
!
      IF ( LOC(W(1)) .NE. 0 ) THEN
!
! -------- Un-weight equations of conditions in order to have them in original
! -------- form
!
           DO 430 J3=1,M
              IF ( DABS(W(J3)) > WEI_MIN  .AND.  DABS(W(J3)) < WEI_MAX ) THEN
                   DO 440 J4=1,N
                      A(J4,J3) = A(J4,J3)/W(J3)
 440               CONTINUE
                   B(J3) = B(J3)/W(J3)
              END IF
 430       CONTINUE
      END IF
!
! --- Invert normal matrix
!
      CALL ERR_PASS ( IUER, IER )
      CALL INVS     ( N, COV, RC, IER )
      IF ( IER > 0 ) THEN
           CALL ERR_LOG ( 12, IUER, 'LSQW', 'Error during inversion '// &
     &         'of the normal matrix' )
           RETURN
      END IF
!
! --- Find vector of the estimates of the parameters
!
      IER=-1
      CALL MUL_MV_SV_V ( N, COV, N, DISP, N, X, IER )
!
! --- Find the number of degrees of freedom
!
      FREDEG = M - N
      IF ( FREDEG .GT. 0.99 ) THEN
!
! -------- Extracting vector of estimates of uncertainties of adjustments
! -------- Transformation covariance matrix to correlation matrix
!
           CALL ERR_PASS ( IUER, IER )
           CALL LSQ_DISP ( N, COV, DISP, SUSQ, X, SIG, FREDEG, IER )
           IF ( IER > 0 ) THEN
                CALL ERR_LOG ( 14, IUER, 'LSQW', 'Error during '// &
     &              'calculation dispersion of the estimates' )
                RETURN
           END IF
         ELSE
!
! -------- Don't calculate mean error of unit weight for special cases
!
           DO 450 J5=1,N
              LL = (J5*(J5+1))/2
              DISP(J5)=DSQRT( COV(LL) )
 450       CONTINUE
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  LSQW  #!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE LSQ1 ( N, M, A, B, X, DISP, COV, RC, SIG, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  LSQ1  solve system of linear equatiuon using LSQ method.  *
! *                                                                      *
! *  ###  20-MAR-89       LSQ1     v1.0  (c)  L. Petrov  02-OCT-98  ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INTEGER*4  M, N, IUER
      REAL*8     A(N,M), B(M), X(N), DISP(N), COV(*), RC, SIG
!
      REAL*8     SUSQ, FREDEG
      INTEGER*4  IER
      REAL*8,    EXTERNAL :: DP_VV_V
!
      IER=-1
      CALL MUL_MM_IT_S ( N, M, A, N, M, A, N, COV, IER )
!
      IER=-1
      CALL MUL_MV_IV_V ( N, M, A, M, B, N, DISP, IER )
!
      SUSQ = DP_VV_V ( M, B, B )
!
      CALL ERR_PASS ( IUER, IER )
      CALL INVS     ( N, COV, RC, IER )
      IF ( IER > 0 ) THEN
           CALL ERR_LOG ( 12, IUER, 'LSQ1', 'Error during inversion '// &
     &         'of the normal matrix' )
           RETURN
      END IF
      IER=-1
      CALL MUL_MV_SV_V ( N, COV, N, DISP, N, X, IER )
!
      FREDEG = M - N
      CALL ERR_PASS ( IUER, IER )
      CALL LSQ_DISP ( N, COV, DISP, SUSQ, X, SIG, FREDEG, IER )
      IF ( IER > 0 ) THEN
           CALL ERR_LOG ( 14, IUER, 'LSQ1', 'Error during '// &
     &         'calculation dispresion of the estimates' )
           RETURN
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  LSQ1  #!#
!
! ------------------------------------------------------------------------
!
        SUBROUTINE LSQ_DISP ( N, A, B, SUSQ, Y, SIG, FREDEG, IUER )
! ************************************************************************
! *                                                                      *
! *     Routine  LSQ_DISP  calculates mean error of unit weight   SIG    *
! *   and vector of scaled formal uncertainties of estimates of LSQ      *
! *   adjustments  B.                                                    *
! *                                                                      *
! * ________________________ Input parameters: _________________________ *
! *                                                                      *
! *       N ( INTEGER*4 ) -- Number of unknowns                          *
! *    SUSQ ( REAL*8    ) -- Weighted sum of squares of right parts of   *
! *                          equations of conditions.                    *
! *       Y ( REAL*8    ) -- Vector of adjustments.                      *
! *  FREDEG ( REAL*8    ) -- Number of degrees of freedom.               *
! *     A   ( REAL*8    ) -- Matrix to be inverse with respect to the    *
! *                          matrix of normal equations.                 *
! *                                                                      *
! * _______________________ Output parameters: _________________________ *
! *                                                                      *
! *     SIG ( REAL*8    ) -- Mean error of unit weight.                  *
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
! *       B ( REAL*8    ) -- Input: normal vector.                       *
! *                          Output: vecotr of scaled formal             *
! *                                  uncertainties.                      *
! *    IUER ( INTEGER*4, OPT ) -- Universal error handler.               *
! *                          Input: switch IUER=0 -- no error messages   *
! *                                 will be generated even in the case   *
! *                                 of error. IUER=-1 -- in the case of  *
! *                                 error the message will be put on     *
! *                                 stdout.                              *
! *                          Output: 0 in the case of successful         *
! *                                  completion and non-zero in the      *
! *                                  case of error.                      *
! *                                                                      *
! *  ###  24-MAR-89    LSQ_DISP   v3.0   (c)  L. Petrov  07-OCT-98  ###  *
! *                                                                      *
! ************************************************************************
        IMPLICIT    NONE
        INTEGER*4   N, IUER
        REAL*8      A(*), B(N), Y(N), SUSQ, S, SIG, FREDEG, EPS, EPS1, EPS2
        PARAMETER ( EPS=1.D-30, EPS1=1.D-13, EPS2=1.D-8  )
        CHARACTER   STR*32
        INTEGER*4   J1, J2, IER, LL, LOC, I, J
        INTEGER*4,  EXTERNAL :: I_LEN
        LOC(I,J)=I+(J*(J-1))/2
!
! ----- Replaced values of vector B to the vector of unscaled sigmas
!
        S=SUSQ
        DO 410 J1=1,N
           S = S-B(J1)*Y(J1) ! Accumulating weighted square of residuals
           LL=LOC(J1,J1)
           IF ( A(LL).LT.EPS ) THEN
                CALL CLRCH ( STR )
                CALL INCH  ( J1, STR(1:10) )
                WRITE ( UNIT=STR(11:32), FMT='(G22.16)' ) A(LL)
                CALL CHASHL  ( STR(11:) )
                CALL ERR_LOG ( 20000+J1, IUER, 'LSQ_DISP', &
     &               'Too small diagonal element of the "covariance" '// &
     &               'was found at ' // &
     &                STR(1:I_LEN(STR(1:10)))//'-th row.  Its value='// &
     &                STR(11:I_LEN(STR)) )
                RETURN
           END IF
           B(J1)=DSQRT( A(LL) )
  410   CONTINUE
!
        IF ( FREDEG .GT. (1.0 - EPS2) ) THEN
             IF ( SUSQ .GT. EPS ) THEN
                  IF ( DABS(S/SUSQ) .LT. EPS1 ) THEN
                       WRITE ( 6,  * ) ' SUSQ=',SUSQ,' B*Y=',SUSQ-S
                       WRITE ( 6,  * ) '  SUSQ-B*Y =',S
                       WRITE ( 6,  * ) ' |SUSQ-B*Y|/SUSQ < EPS1=', EPS1
                       CALL ERR_LOG ( 177, IUER, 'LSQ_DISP', 'Catastrophic '// &
     &                     'loss of precision during calcilation SIGMA' )
                       RETURN
                  END IF
             END IF
!
! ---------- Calculating SIG -- mean error of unit weight
!
             IF ( S .LT. EPS ) THEN
                  SIG=S
                  CALL CLRCH ( STR )
                  WRITE ( UNIT=STR, FMT='(G22.16)' ) S
                  CALL CHASHL  ( STR )
                  CALL ERR_LOG ( 99999, IUER, 'LSQ_DISP', 'Negative '// &
     &                          'sigma. SS='//STR(1:I_LEN(STR))  )
                  RETURN
             END IF
             SIG=DSQRT ( S/FREDEG )
           ELSE IF ( FREDEG .LE. EPS ) THEN
             CALL CLRCH ( STR )
             WRITE ( UNIT=STR, FMT='(1PG15.8)', IOSTAT=IER ) FREDEG
             CALL CHASHL  ( STR )
             CALL ERR_LOG ( 2, IUER, 'LSQ_DISP', 'The number of '// &
     &           'degrees of freedom is too small:  FREDEG='// &
     &            STR(1:I_LEN(STR)) )
             RETURN
           ELSE
             SIG = 1.D0
        END IF
!
! ----- Scaling vector of uncertainties
!
        DO 420 J2=1,N
           B(J2)=B(J2)*SIG
  420   CONTINUE
        CALL ERR_LOG ( 0, IUER )
        RETURN
        END  !#!  LSQ_DISP  #!#
