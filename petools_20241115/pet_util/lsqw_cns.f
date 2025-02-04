      SUBROUTINE LSQW_CNS ( N, M, A, B, W, CNS, X, DISP, COV, RC, SIG, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  LSQW_CNS  solves system of linear equation using weighted *
! *   constrainted LSQ method. It returns vector of adjustments and      *
! *   vector of estimates of their uncertainties. Uncertainties are      *
! *   scaled by square root of ratio of chi-square to the number of      *
! *   degrees of freedom.                                                *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *    N ( INTEGER*4 ) -- Number of parameters.                          *
! *    M ( INTEGER*4 ) -- Number of equations.                           *
! *    A ( REAL*8    ) -- Matrix of equations of conditions as array     *
! *                       N*M. NB: k-th equation located in the k-th     *
! *                       column of the array A.                         *
! *    B ( REAL*8    ) -- Vector of the right parts of equations of      *
! *                       conditions. DimensionL M.                      *
! *    W ( REAL*8, OPT ) -- Vector of weights of equations with M        *
! *                         elements. Weights are reciprocal to          *
! *                         "a priori sigmas". If argument W is omitted  *
! *                         then all weights are considered to be equal  *
! *                         to 1. Dimension: N.                          *
! *  CNS ( REAL*8, OPT ) -- Vector of reciprocal weights of diagonal     *
! *                         constraints. Each parameter is constrainted  *
! *                         to zero with the reciprocal weights CNS.     *
! *                         If CNS(k) = 0.0D0, then no constraint is     *
! *                         applied to the k -th parameter.              *
! *                         Dimension: N.                                *
! *                                                                      *
! * ________________________ Output parameters: ________________________ *
! *                                                                      *
! *    X ( REAL*8    ) -- Vector of adjustments (Dimension: N).          *
! * DISP ( REAL*8    ) -- Vector of adjustments (Dimension: N).          *
! *  COV ( REAL*8    ) -- If weights are specivied, COV is the           *
! *                       covariance matirx. Otherwise, this is          *
! *                       the correlation matrix.                        *
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
! *  ###  20-MAR-1989   LSQW_CNS   v2.0 (c)  L. Petrov  17-NOV-2009 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INTEGER*4  M, N, IUER
      REAL*8     A(N,M), B(M), W(M), CNS(M), X(N), DISP(N), COV(*), RC, SIG
      REAL*8     WEI_MIN, WEI_MAX
      PARAMETER  ( WEI_MIN = 1.D-50 )
      PARAMETER  ( WEI_MAX = 1.D50  )
!
      REAL*8     SUSQ, FREDEG
      INTEGER*4  IER, J1, J2, J3, J4, J5, J6, LL
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
! --- Apply constraints
!
      IF ( LOC(CNS(1)) .NE. 0 ) THEN
           LL = 1
           DO 430 J3=1,N
              IF ( CNS(J3) .NE. 0.0D0 ) THEN
                   COV(LL) = COV(LL) + 1.0D0/CNS(J3)**2
              END IF
              LL = LL + J3+1
 430       CONTINUE 
      END IF
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
           DO 440 J4=1,M
              IF ( DABS(W(J4)) > WEI_MIN  .AND.  DABS(W(J4)) < WEI_MAX ) THEN
                   DO 450 J5=1,N
                      A(J5,J4) = A(J5,J4)/W(J4)
 450               CONTINUE
                   B(J4) = B(J4)/W(J4)
              END IF
 440       CONTINUE
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
      IF ( LOC(W(1)) .EQ. 0.0D0 ) THEN
           FREDEG = M - N
           IF ( FREDEG .GT. 0.99 ) THEN
!
! ------------- Extracting vector of estimates of uncertainties of adjustments
! ------------- Transformation covariance matrix to correlation matrix
!
                CALL ERR_PASS ( IUER, IER )
                CALL LSQ_DISP ( N, COV, DISP, SUSQ, X, SIG, FREDEG, IER )
                IF ( IER > 0 ) THEN
                     CALL ERR_LOG ( 14, IUER, 'LSQW', 'Error during '// &
     &                   'calculation dispersion of the estimates' )
                     RETURN
                END IF
              ELSE
!
! ------------- Don't calculate mean error of unit weight for special cases
!
                DO 460 J6=1,N
                   LL = (J6*(J6+1))/2
                   DISP(J6)=DSQRT( COV(LL) )
 460            CONTINUE
           END IF
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  LSQW_CNS  !#!#
