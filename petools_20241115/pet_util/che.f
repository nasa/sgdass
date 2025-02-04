        SUBROUTINE CHCR ( N, T0, DT, T, Y, CH )
! ************************************************************************
! *                                                                      *
! *    Routine CHCR computes coefficients of the Chebyschev polynomial   *
! *    of degree N for a function defined on the interval [ T0, T0+DT ]  *
! *                                                                      *
! * ______________________ Input parameters ____________________________ *
! *                                                                      *
! *        N  ( INTEGER*4 ) --  The number of values of the function     *
! *                             under consideration.                     *
! *       T0  ( REAL*8    ) --  The left border of the range.            *
! *       DT  ( REAL*8    ) --  The length of the range.                 *
! *        T  ( REAL*8    ) --  Array of the arguments of the function   *
! *                             under consideration.                     *
! *        Y  ( REAL*8    ) --  Array of the values of the function      *
! *                             under consideration.                     *
! *                                                                      *
! * ______________________ Ouput parameters: ___________________________ *
! *                                                                      *
! *       CH  ( REAL*8    ) --  Array with the coefficients.             *
! *                                                                      *
! * *### 25-JAN-1989      CHCR      v1.1 (c) L. Petrov  30-OCT-2003 ###  *
! *                                                                      *
! ************************************************************************
        IMPLICIT   NONE 
        INTEGER*4  N
        REAL*8 T0, DT, T(N), Y(N), CH(N), W
!!        CALL VER$ARG ( 7 )
        REAL*8, ALLOCATABLE :: A (:,:), Y_SAVE(:)
        INTEGER*4  J1, J2
!
        ALLOCATE ( A(N,N) )
        ALLOCATE ( Y_SAVE(N) )
        Y_SAVE = Y
!
! ----- Build the matrix of equation of conditions A
!
        DO 410 J1=1,N
!
! -------- Compute W -- normalized argument reduced to the range [-1,1]
!
           W=-1.D0+2.D0*(T(J1)-T0)/DT
!
! -------- Computation of the Chebyshev polynomual of the 1-st degree
!
           A(J1,1)=1.D0
!
! -------- Computation of the value of the Chebyschev polynomial of the &
! -------- second degree
!
           A(J1,2)=W
           DO 420 J2=3,N
!
! ----------- Computation of the value of Chebyschev polynomial of the &
! ----------- J2-th degree
!
              A(J1,J2)=2.D0*A(J1,J2-1)*W-A(J1,J2-2)
  420      CONTINUE
  410   CONTINUE
!
! ----- Solving of the resulted system of linear equations. The solution vector &
! ----- is just that what we need
!
        CALL LINSYS_SOLVE  ( N, A, Y, CH )
        Y = Y_SAVE 
        DEALLOCATE ( Y_SAVE )
        DEALLOCATE ( A )
!
        RETURN
        END  !#!  CHCR  #!#
!
! ------------------------------------------------------------------------
!
        FUNCTION  CHINT ( N, T0, DT, T, CH, IUER )
! ************************************************************************
! *                                                                      *
! *    Routine  CHINT  computes the value of the function F(t) defined   *
! *    in the range { T0, T0+DT ] at the point T with use of Chebyshev   *
! *    polynomials of degree N. It is assumed that the coefficients have *
! *    been computed beforehand, f.e with program CHCR.                  *
! *                                                                      *
! * ______________________ Input parameters ____________________________ *
! *                                                                      *
! *        N  ( INTEGER*4 ) --  The number of values of the function     *
! *                             under consideration.                     *
! *       T0  ( REAL*8    ) --  The left border of the range.            *
! *       DT  ( REAL*8    ) --  The length of the range.                 *
! *        T  ( REAL*8    ) --  Array of the arguments of the function   *
! *                             under consideration.                     *
! *       CH  ( REAL*8    ) --  Array with Chebyshev coefficients.       *
! *                             Dimension: N.                            *
! *                                                                      *
! * ______________________ Ouput parameters: ___________________________ *
! *                                                                      *
! *    CHINT  ( REAL*8    )  --  The value of function F at the point T. *
! *                                                                      *
! * _________________________ Modified parameters: _____________________ *
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
! * ###  25-JAN-1989      CHINT      v1.2 (c) L. Petrov  25-JAN-2004 ### *
! *                                                                      *
! ************************************************************************
        IMPLICIT   NONE 
        REAL*8 CHINT
        INTEGER*4  N, IUER
        REAL*8 T0, DT, CH(N), T, W, PC, PL1, PL2
        INTEGER*4  J1
!        CALL VER$ARG ( 6 )
        IF ( N .EQ. 0 ) THEN
             CHINT = 0.0D0
             CALL ERR_LOG ( 0, IUER )
             RETURN
        END IF
        W = -1.D0 + 2.D0*(T-T0)/DT
!
! ----- Compute the value of the Chebyschev polymonial of the 1-st degree
!
        CHINT=CH(1)
        IF ( N .EQ. 1 ) RETURN
        PL2=1.D0
!
! ----- Compute the value of the Chebyschev polymonial of the 2-nd degree
!
        PL1=W
        CHINT = CHINT + CH(2)*PL1
        IF ( N .EQ. 2 ) RETURN
!
! ----- PL2 -- the value of the Chebyschev polynomial of degree K-2
! ----- PL1 -- the value of the Chebyschev polynomial of degree K-1
! ----- PC  -- the value of the Chebyschev polynomial of degree K
!
        DO 410 J1=3,N
!
! -------- Compute PC -- teh value of Chebyschev polynomial of the J1-th degree
!
           PC = 2.D0 * W*PL1-PL2
           CHINT = CHINT + CH(J1)*PC
           PL2 = PL1
           PL1 = PC
  410   CONTINUE
!
        CALL ERR_LOG ( 0, IUER )
        RETURN
        END  !#!  CHINT  #!#
!
! ------------------------------------------------------------------------
!
        SUBROUTINE LINSYS_SOLVE ( N, A, B, X )
! ************************************************************************
! *                                                                      *
! *     Very simple program for solving the system of linear equations   *
! *     of the N-th order using Gauss method.                            *
! *                                                                      *
! *          A*B=X                                                       *
! *                                                                      *
! * ______________________ Input parameters ____________________________ *
! *                                                                      *
! *         N  ( INTEGER*4 )  --  Dimension of the system.               *
! *                                                                      *
! * ______________________ Modified parameters: ________________________ *
! *                                                                      *
! *         A  ( REAL*8    )  --  Matris of equations of N*N dimension.  *
! *                               The matrix A is destroyed during work  *
! *                               of the program.                        *
! *         B  ( REAL*8    )  --  Right hand side vector of dimension    *
! *                               of N. The vector B is destroyed during *
! *                               work of the program.                   *
! *                                                                      *
! * ______________________ Ouput parameters: ___________________________ *
! *                                                                      *
! *         X  ( REAL*8    )  --  Solution vector of dimension of N.     *
! *                                                                      *
! *  ### 28-NOV-1987  LINSYS_SOLVE  v1.1 (c) L. Petrov  30-OCT-2003 ###  *
! *                                                                      *
! ************************************************************************
        IMPLICIT   NONE 
        INTEGER*4  N
        REAL*8 A(N,N), B(N), X(N), C, D
        INTEGER*4  J1, J2, J3, J4, J5, JJ, JJJ
!!        CALL VER$ARG ( 4 )
!
! ----- First run: TRansfomraion matrix A infor triagonal matrix.
!
        DO 410 J1=1,N-1
           D=A(J1,J1)
           DO 420 J2=J1+1,N
              C=A(J2,J1)/D
              DO 430 J3=J1,N
                 A(J2,J3)=A(J2,J3)-C*A(J1,J3)
  430         CONTINUE
              B(J2)=B(J2)-C*B(J1)
  420      CONTINUE
  410   CONTINUE
!
! ----- Second run: solving triangular system
!
        X(N)=B(N)/A(N,N)
        DO 440 J4=1,N-1
           JJ=N-J4
           C=0.D0
           DO 450 J5=1,J4
              JJJ=N-J5+1
              C=C+A(JJ,JJJ)*X(JJJ)
  450      CONTINUE
           X(JJ)=(B(JJ)-C)/A(JJ,JJ)
  440   CONTINUE
        RETURN
        END  !#!  LINSYS_SOLVE  #!#
!
! ------------------------------------------------------------------------
!
        SUBROUTINE CHINTI ( N, T0, TL, TR, DT, CH, S, IUER )
! ************************************************************************
! *                                                                      *
! *    Program CHINTI  evaluates the integral in the range [ TL , TR ]   *
! *    from the function defined on the range [ T0 , T0+DT ], using      *
! *    Chebyshev polynomials of degree N. It is assumed that the         *
! *    polynomial coefficients have been computed previously with, f.e.  *
! *    routine CHCR.                                                     *
!  ________________________ Input parameters: __________________________ *
! *                                                                      *
! *        N  ( INTEGER*4 )  --  Degree of the Chebyshev polynomial.     *
! *       T0  ( REAL*8    )  --  Left boundary of the range for which    *
! *                              the polynomial has been computed.       *
! *       TL  ( REAL*8    )  --  Left boundary of the integration range. *
! *       TR  ( REAL*8    )  --  Right boundary of the integration range.*
! *       DT  ( REAL*8    )  --  Length of the integration range.        *
! *       CH  ( REAL*8    )  --  Array with polynomial coefficients.     *
! *                              Dimension: N.                           *
! *                                                                      *
! * _________________________ Modified parameters: _____________________ *
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
! *    The following recurrent relationship is used for evaluation of    *
! *    the integral:                                                     *
! *                                                                      *
! *       S(K) is the integral in the range [ 0, X ] of T(ë),            *
! *            where T(K) -- Chebyshev polynomial of degree K.           *
! *                                                                      *
! *           S(1) = X                                                   *
! *           S(2) = X^2/2                                               *
! *                                                                      *
! *       for ë > 2                                                      *
! *                                                                      *
! *           S(K)= ( ( T(K+1) + KS )/K - ( T(K-1) - KS )/(K-2) )/2      *
! *                                                                      *
! *                     where KS = 0  if  K  is odd;                     *
! *                           KS =-1  if  K  is even  and  K/2 is even.  *
! *                           KS = 1  if  K  is even  and  K/2 is odd.   *
! *                                                                      *
! * _______________________ Output parameters: _________________________ *
! *                                                                      *
! *        S  ( REAL*8    )  -- Value of the integral.                   *
! *                                                                      *
! *  ###  21-SEP-1990    CHINTI   v 2.1 (c)  L. Petrov  25-JAN-2004 ###  *
! *                                                                      *
! ************************************************************************
        IMPLICIT   NONE 
        INTEGER*4  N, IUER
        REAL*8     T0, DT, CH(N), TL, TR, S, SA, RM
        REAL*8     W_R, PC_R, PL1_R, PL2_R, ST_R
        REAL*8     W_L, PC_L, PL1_L, PL2_L, ST_L
        INTEGER*4  I1, J1, KS
!
        RM=2.D0/DT
!
! ----- Compute normalized argument for both ends of the range
!
        W_R=-1.D0+(TR-T0)*RM
        W_L=-1.D0+(TL-T0)*RM
!
! ----- Compute the value of Chebyshev polynomial of degree 1
!
        PL2_R=1.D0
        PL2_L=1.D0
!
        S=CH(1)*( W_R - W_L )
!
! ----- Compute the integral of Chebyshev polynomial of degree 2
!
        ST_R=W_R*W_R/2.D0
        ST_L=W_L*W_L/2.D0
!
        S=S+CH(2)* ( ST_R - ST_L )
!
! ----- Compute the value of Chebyshev polynomial of degree 2
!
        PL1_R=W_R
        PL1_L=W_L
!
! ----- Compute the value of Chebyshev polynomial of degree 3
!
        PC_R=2.D0*W_R*PL1_R-PL2_R
        PC_L=2.D0*W_L*PL1_L-PL2_L
!
        PL2_R=PL1_R
        PL2_L=PL1_L
!
        PL1_R=PC_R
        PL1_L=PC_L
!
! ----- Compute integrals of Chebyshev polynomials of degrees 3 through N
!
        DO 410 J1=3,N
!
! -------- Compute PC --  Chebyshev polymoial of degree J1+1
!
           PC_R=2.D0*W_R*PL1_R-PL2_R
           PC_L=2.D0*W_L*PL1_L-PL2_L
!
! -------- Compute correction KS
!
           IF ( MOD( J1, 2 ) .EQ. 0 )  THEN
                IF ( MOD ( J1/2, 2 ) .EQ. 0 )  THEN
                     KS=-1
                ELSE
                     KS=1
                END IF
            ELSE
                KS=0
            END IF
!
! -------- Compute ST -- Integral of Chebyshev polynomial of degree J1
!
           I1=J1-2
           ST_R=( ( PC_R + KS )/J1 - ( PL2_R - KS )/I1 ) / 2.D0
           ST_L=( ( PC_L + KS )/J1 - ( PL2_L - KS )/I1 ) / 2.D0
!
           S=S+CH(J1)* ( ST_R - ST_L )
!
! -------- PL2  --  Chebyshev polynomial of degree J1-1
!
           PL2_R=PL1_R
           PL2_L=PL1_L
!
! -------- PL1  --  Chebyshev polynomial of degree J1
!
           PL1_R=PC_R
           PL1_L=PC_L
  410   CONTINUE
!
! ----- Normalization of the value of the integral: taking into account 
! ----- the fact that the length of the integration rane is not necessarily 1.
!
        S=S/RM
!
        CALL ERR_LOG ( 0, IUER )
        RETURN
        END  !#!  CHINTI  #!#
!
! ------------------------------------------------------------------------
!
        FUNCTION  CHIV1 ( N, T0, DT, T, CH, IUER )
! ************************************************************************
! *                                                                      *
! *    Routine  CHIV1  computes the first derivatives of the function    *
! *    F(t) defined in the range ( T0, T0+DT ] at the point T with       *
! *    use of Chebyshev polynomials of degree N. It is assumed that      *
! *    the coefficients have been computed beforehand, f.e with          *
! *    program CHCR.                                                     *
! *                                                                      *
! *    The first derivative of the Chebyschev polinomial of degree N     *
! *    is computed with use of the auxiliary polinomual V(N):            *
! *                                                                      *
! *       V(1)  = 1                                                      *
! *       V(2)  = 2X                                                     *
! *       V(N)  = 2 * W * V(N-1) - V(N-2)                                *
! *       ô'(1) = 0                                                      *
! *       For N > 1   ô'(N)= ( N-1) * V(N-1)                             *
! *       where is a normalized argumet: W = -1.0 + 2.0*(t-t0)/DT        *
! *                                                                      *
! * ______________________ Input parameters ____________________________ *
! *                                                                      *
! *        N  ( INTEGER*4 ) --  The number of values of the function     *
! *                             under consideration.                     *
! *       T0  ( REAL*8    ) --  The left border of the range.            *
! *       DT  ( REAL*8    ) --  The length of the range.                 *
! *        T  ( REAL*8    ) --  Array of the arguments of the function   *
! *                             under consideration.                     *
! *       CH  ( REAL*8    ) --  Array with Chebyshev coefficients.       *
! *                             Dimension: N.                            *
! *                                                                      *
! * ______________________ Output parameters: __________________________ *
! *                                                                      *
! *    CHIV1  ( REAL*8    ) --  The value of first derivative of the     *
! *                             function F at the point T.               *
! *                                                                      *
! * _________________________ Modified parameters: _____________________ *
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
! * ###  02-MAR-1993      CHIV1      v1.1 (c) L. Petrov  25-JAN-2004 ### *
! *                                                                      *
! ************************************************************************
        IMPLICIT   NONE 
        REAL*8     CHIV1
        INTEGER*4  N, IUER
        REAL*8 T0, DT, CH(N), T, TT, W, PC, PL1, PL2, VPL1, VPL2, VPC
        INTEGER*4  J1
!        CALL VER$ARG ( 5 )
!
        IF ( N .LE. 1 ) THEN
             CHIV1 = 0.0D0
             CALL ERR_LOG ( 0, IUER )
             RETURN
        END IF
        IF ( DT .LT. 1.D-30 ) THEN
             CALL ERR_LOG ( 1511, IUER, 'CHIV1', 'Argument DT is too small' )
             RETURN
        END IF
        W = -1.D0 + 2.D0*(T-T0)/DT
        IF ( W .LT. -1.0D0 ) THEN
             CALL ERR_LOG ( 1512, IUER, 'CHIV1', 'T < T0 ' )
             RETURN
        END IF
        IF ( W .GT. 1.0D0 ) THEN
             CALL ERR_LOG ( 1513, IUER, 'CHIV1', 'T > T0+DT ' )
             RETURN
        END IF
!
! ----- Computed the first consitiuent of the expansion for the first
! ----- derivative (the second one is equal to zero )
!
        CHIV1 = CH(2)
!
        VPL1 = 2.0D0*W
        VPL2 = 1.0D0
!
        DO 410 J1=3,N
!
! -------- VPC  -- Value of the auxiliary polynomial of degree J2
! -------- VPL1 -- Value of the auxiliary polynomial of degree J2-1
! -------- VPL2 -- Value of the auxiliary polynomial of degree J2-2
! --------  TT  -- Value of the first derivative of the Chebyschev polynomual of degree J2
!
           VPC   = 2.0D0*W*VPL1 - VPL2
           TT    = (J1-1)*VPL1
           CHIV1 = CHIV1 + CH(J1)*TT
           VPL2  = VPL1
           VPL1  = VPC
  410   CONTINUE
!
! ----- Normalizing results for the length of the interpolation range
!
        CHIV1 = CHIV1*2.0D0/DT
!
        CALL ERR_LOG ( 0, IUER )
        RETURN
        END  !#!  CHIIV1  #!#
!
! ------------------------------------------------------------------------
!
        FUNCTION  CHIV2 ( N, T0, DT, T, CH, IUER )
! ************************************************************************
! *                                                                      *
! *    Routine  CHIV2  computes the second derivatives of the function   *
! *    F(t) defined in the range ( T0, T0+DT ] at the point T with       *
! *    use of Chebyshev polynomials of degree N. It is assumed that      *
! *    the coefficients have been computed beforehand, f.e with          *
! *    program CHCR.                                                     *
! *                                                                      *
! *    The second derivative of the Chebyschev polinomial of degree N    *
! *    is computed with use of the auxiliary polinomual V(N):            *
! *                                                                      *
! *       V(1)   = 1                                                     *
! *       V(2)   = 2X                                                    *
! *       V(N)   = 2 * W * V(N-1) - V(N-2)                               *
! *       ô''(1) = 0                                                     *
! *       ô''(2) = 0                                                     *
! *       For N > 2  ô''(N) = (4N-8)*V(N-2) + (2*W*ô''(N-1) - ô''(N-2))  *
! *                                                                      *
! *       where is a normalized argumet: W = -1.0 + 2.0*(t-t0)/DT        *
! *                                                                      *
! * ______________________ Input parameters ____________________________ *
! *                                                                      *
! *        N  ( INTEGER*4 ) --  The number of values of the function     *
! *                             under consideration.                     *
! *       T0  ( REAL*8    ) --  The left border of the range.            *
! *       DT  ( REAL*8    ) --  The length of the range.                 *
! *        T  ( REAL*8    ) --  Array of the arguments of the function   *
! *                             under consideration.                     *
! *       CH  ( REAL*8    ) --  Array with Chebyshev coefficients.       *
! *                             Dimension: N.                            *
! *                                                                      *
! * ______________________ Output parameters: __________________________ *
! *                                                                      *
! *    CHIV2  ( REAL*8    ) --  The value of the second derivative of    *
! *                             function F at the point T.               *
! *                                                                      *
! * _________________________ Modified parameters: _____________________ *
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
! * ###  02-MAR-1993      CHIV2      v1.1 (c) L. Petrov  25-JAN-2004 ### *
! *                                                                      *
! ************************************************************************
        IMPLICIT   NONE 
        REAL*8     CHIV2
        INTEGER*4  N, IUER
        REAL*8     T0, DT, CH(N), T, TTT, W, PC, PL1, PL2, VPL1, VPL2, &
     &             APL1, APL2, VPC
        INTEGER*4  J1
!        CALL VER$ARG ( 5 )
!
        IF ( N .LE. 2 ) THEN
             CHIV2 = 0.0D0
             CALL ERR_LOG ( 0, IUER )
             RETURN
        END IF
        IF ( DT .LT. 1.D-30 ) THEN
             CALL ERR_LOG ( 1521, IUER, 'CHIV2', 'Argument DT is too small' )
             RETURN
        END IF
!
        W = -1.0D0 + 2.0D0*(T-T0)/DT
        IF ( W.LT.-1.D0 ) THEN
             CALL ERR_LOG ( 1522, IUER, 'CHIV2', 'T < T0 ' )
             RETURN
        END IF
        IF ( W .GT. 1.D0 ) THEN
             CALL ERR_LOG ( 1523, IUER, 'CHIV2', 'T > T0+DT ' )
             RETURN
        END IF
!
        CHIV2 = 0.0D0
!
        VPL1 = 2.0D0*W
        VPL2 = 1.0D0
        APL2 = 0.0D0
        APL1 = 0.0D0
!
        DO 410 J1=3,N
!
! -------- VPC  -- Value of the auxiliary polynomial of degree J2
! -------- VPL1 -- Value of the auxiliary polynomial of degree J2-1
! -------- VPL2 -- Value of the auxiliary polynomial of degree J2-2
! --------  TT  -- Value of the second derivative of the Chebyschev polynomual of degree J2
!
           VPC   = 2.0D0*W*VPL1 - VPL2
           TTT   = (4*J1-8) * VPL2 + (2.0D0*W*APL1 - APL2)
           CHIV2 = CHIV2 + CH(J1)*TTT
           VPL2  = VPL1
           VPL1  = VPC
           APL2  = APL1
           APL1  = TTT
  410   CONTINUE
!
! ----- Normalizing results for the length of the interpolation range
!
        CHIV2 = CHIV2*4.0D0/DT**2
!
        CALL ERR_LOG ( 0, IUER )
        RETURN
        END  !#!  CHIIV2  #!#
!
! ------------------------------------------------------------------------
!
        SUBROUTINE  CHMAP ( N, T0, DT, CH, POLY_COEF, IUER )
! ************************************************************************
! *                                                                      *
! *    Routine  CHMAP  computes the standard polynomial coefficients     *
! *    in the range { T0, T0+DT ] from  Chebyshev polynomials of         *
! *    degree N. It is assumed that the coefficients have                *
! *    been computed beforehand, f.e with program CHCR.                  *
! *    This uses the coefficients-to-vector method (Solary, 2018)        *
! *                                                                      *
! * ______________________ Input parameters ____________________________ *
! *                                                                      *
! *        N  ( INTEGER*4 ) --  The number of coefficients               *
! *                             under consideration.                     *
! *       T0  ( REAL*8    ) --  The left border of the range.            *
! *       DT  ( REAL*8    ) --  The length of the range.                 *
! *       CH  ( REAL*8    ) --  Array with Chebyshev coefficients.       *
! *                             Dimension: N.                            *
! *                                                                      *
! * ______________________ Ouput parameters: ___________________________ *
! *                                                                      *
! *    POLY_COEF  ( REAL*8    ) -- The standard polynomial coefficients. *
! *                                                                      *
! * _________________________ Modified parameters: _____________________ *
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
! * ###  12-APR-2024      CHMAP      v1.0 (c) J. Skeens  12-APR-2024 ### *
! *                                                                      *
! ************************************************************************
        IMPLICIT   NONE 
        INTEGER*4  N, IUER
        REAL*8 T0, DT, CH(N), R_MAT(N,N), B_VEC(N), C_VEC(N), POLY_COEF(0:N-1),& 
       &       STEP, VAL(N), CH_PT, TIM(N) 
        INTEGER*4  J1, J2, J3, J4, J5, DEG, IER
        REAL*8      PI
        PARAMETER ( PI=3.141592653589793238D0 ) ! Pi
!
! ----- Compute R matrix
!
        R_MAT = 0.0D0
        DO 410 J1=1,N
           DO 420 J2=J1,N
              IF (J1 == J2) THEN
                  R_MAT(J1,J2) = 1
              ELSE IF ( J1 == 1 .AND. MOD(J2,2) > 0 ) THEN
                  R_MAT(J1,J2) = -R_MAT(J1,J2-2)
              ELSE IF ( J1 == 2 .AND. MOD(J2,2) == 0 ) THEN
                  R_MAT(J1,J2) = -SIGN ( 1.0D0, R_MAT(J1,J2-2) ) * (ABS ( R_MAT(J1,J2-2) ) + 2)
              ELSE IF ( J1 > 2 ) THEN
                  R_MAT(J1,J2) = -SIGN ( 1.0D0, R_MAT(J1,J2-2) ) * &
       &                          (ABS ( R_MAT(J1,J2-2) ) + ABS ( R_MAT(J1-1,J2-1) ))
           ENDIF
  420      CONTINUE
  410   CONTINUE
!
! ----- Compute intermediate coefficients C_VEC
!
        CALL ERR_PASS ( IUER, IER )
        CALL MUL_MV_IV_V ( N, N, R_MAT, N, CH, N, C_VEC, IER )
        IF ( IER .NE. 0 ) THEN
             CALL ERR_LOG ( 1522, IUER, 'CHMAP', 'Failed to calculate'// &
       &     ' intermediate coefficiens of Chebyshev mapping')
             RETURN
        END IF
!
! ----- Compute standard polynomial coefficients defined on [-1, 1]
!
        B_VEC = 0.0D0
        B_VEC(1) = C_VEC(1)
        DO 430 J3=1,N-1
           B_VEC(J3+1) = 2**(J3-1) * C_VEC(J3+1)
  430   CONTINUE
!
! ----- Transform the range of validity to [T0, T0+DT] using Chebyshev nodes
!
        DEG = N - 1
        STEP = DT / DEG
        DO 440 J4=0,DEG
            TIM(J4+1) = T0 + 0.5*DT * (1.0 + COS(PI - (2*J4+1) * PI / (2*DEG+2)))
            CH_PT = -1.0D0 + 2.0D0 * (TIM(J4+1) - T0) / DT  
            VAL(J4+1) = 0.0D0
            DO J5 = 0, DEG
                VAL(J4+1) = VAL(J4+1) + B_VEC(J5+1) * CH_PT**J5
            END DO
  440   CONTINUE
!
! ----- Convert the calculated values back to standard polynomial coefficients
!
        CALL ERR_PASS ( IUER, IER )
        CALL POLY_SOLVE_NDD(DEG, TIM, T0, VAL, POLY_COEF(0:DEG), IER)
        IF ( IER .NE. 0 ) THEN
             CALL ERR_LOG ( 1523, IUER, 'CHMAP', 'Failed to solve'// &
       &     ' for final standard polynomial coefficients')
             RETURN
        END IF
        CALL ERR_LOG ( 0, IUER )
        RETURN
        END  !#!  CHMAP  #!#
