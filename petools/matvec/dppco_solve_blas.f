#include <mk5_preprocessor_directives.inc>
      SUBROUTINE DPPCO_SOLVE_BLAS ( N, A, RCOND, Z, SCAL, B, IUER )
      IMPLICIT NONE
!
! 1.  DPPCO_BLAS PROGRAM SPECIFICATION
!
! 1.1 Cholesky factorization of a upper triangula (SOLVE) format matrix
!     that also estimates the condition of the matrix.  This is a
!     double precision version of the LINPACK SPPCO routine with
!     direct calls to the HP Vector Instruction Set and I*4 indexing.
!
! 1.2 REFERENCES:
!
! 2.  DPPCO INTERFACE
!
! 2.1 Parameter File
!
! 2.2 INPUT Variables:
!
      INTEGER*4 N, IUER
      REAL*8    A(*), SCAL(*), B(*)
!
! A - The SOLVE format matrix
! N - Order of the matrix to be factored
!
! 2.3 OUTPUT Variables:
!
      REAL*8 rcond,z(*)
!
! A - The modified matrix
! RCOND - condition number of the matrix
! Z - work vector
!
! 2.4 COMMON BLOCKS USED
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES:
!       CALLED SUBROUTINES: dscal,dasum,daxpy,ddot
!
! 3.  LOCAL VARIABLES
!
      INTEGER*4 J, K, JM1, KB, KP1, I
      INTEGER*4 KJ, KK, J1, IJ, IER
      REAL*8    S, T, EK, WK, WKM, ANORM, YNORM, SM
      REAL*8    DASUM, DDOT
!
! J,K - Loop indices
! JJ,KJ,KK - Used for indexing matrix
! NBLAS - size of vectors for dot product
! S,T - Intermediate values in matrix element calculations
!
! 4.  HISTORY
!   WHO   WHEN   WHAT
!   AEE  910614  Write 'FAILED TO DECOMPOSE' error message to progress file.
!   PET  961226  Substitute INTEGER*2 by INTEGER*4 thoroughout.
!   PET  961226  Bug corrected at last loop ( was: z(kk) needed: z(k) )
!
! 5.  DPPCO PROGRAM STRUCTURE
!
!  Find norm of A
!
      J1 = 1
      DO J=1,N
         Z(J) = DASUM ( J, A(J1), 1 ) ! Summa modules of A
         IJ = J1
         J1 = J1 + J
         JM1 = J-1
         DO I=1,JM1
            Z(I) = Z(I) + DABS(A(IJ))
            IJ = IJ+1
         ENDDO
      ENDDO
      ANORM = 0.d0
      DO J=1,N
         ANORM = DMAX1 ( ANORM, Z(J) )
      ENDDO
!
! --- Factorization
!
      CALL ERR_PASS ( IUER, IER )
      CALL DPPFA_SOLVE_BLAS  ( N, A, SCAL, B, IER )
      IF ( IER .NE. 0 ) THEN
           RCOND = -1.D0
           CALL ERR_LOG ( IER, IUER, 'DPPCO_BLAS', 'Error duiring '// &
     &                    'factorization' )
           RETURN
      END IF
!
      EK = 1.D0
      DO J=1,N
         Z(J) = 0.D0
      ENDDO
      KK = 0
      DO K=1,N
        KK = KK+K
        IF ( Z(K) .NE. 0.D0 ) EK = DSIGN(EK,-Z(K))
        IF ( DABS(EK-Z(K)) .GT. A(KK) ) THEN
             S = A(KK)/DABS(EK-Z(K))
             CALL DSCAL ( N, S, Z, 1 ) ! Z_i := S * Z_i
             EK = S*EK
        ENDIF
        WK = EK - Z(K)
        WKM = -EK - Z(K)
        S = DABS(WK)
        SM = DABS(WKM)
        WK = WK/A(KK)
        WKM = WKM/A(KK)
        KP1 = K+1
        KJ = KK+K
        IF ( KP1 .LE. N ) THEN
             DO J=KP1,N
                SM = SM + DABS(Z(J)+WKM*A(KJ))
                Z(J) = Z(J) + WK*A(KJ)
                S = S + DABS(Z(J))
                KJ = KJ+J
             ENDDO
             IF ( S .LT. SM ) THEN
                  T = WKM - WK
                  WK = WKM
                  KJ = KK + K
                  DO J=KP1,N
                     Z(J) = Z(J) + T*A(KJ)
                     KJ = KJ+J
                  ENDDO
             ENDIF
        ENDIF
        Z(K) = WK
      ENDDO
      S = 1.D0 / DASUM ( N, Z, 1 )
      CALL DSCAL ( N, S, Z, 1 ) ! Z_i := S * Z_i
!
! --- Solve R*Y = W
!
      DO KB=1,N
         K = N+1-KB
         IF ( DABS(Z(K)) .GT. A(KK) ) THEN
              S = A(KK)/DABS(Z(K))
              CALL DSCAL ( N, S, Z, 1 ) ! Z_i := S * Z_i
         ENDIF
         Z(K) = Z(K)/A(KK)
         KK = KK-K
         T = -Z(K)
         CALL DAXPY ( K-1, T, A(KK+1), 1, Z, 1 ) ! Z_i := Z_i + T * A_kk+1
      ENDDO
      S = 1.D0 / DASUM ( N, Z, 1 )  ! Summa modules of Z
      CALL DSCAL ( N, S, Z, 1 ) ! Z_i := S * Z_i
      YNORM = 1.d0
!
! --- Solve TRANS(R)*V = Y
!
      DO K=1,N
         Z(K) = Z(K) - DDOT ( K-1, A(KK+1), 1, Z, 1 ) ! Z_k := Z_k - (A_kk+1 * Z)
         KK = KK+K
         IF ( DABS(Z(K)) .GT. A(KK) ) THEN
              S = A(KK)/DABS(Z(K))
              CALL DSCAL ( N, S, Z, 1 ) ! Z_i := S * Z_i
              YNORM = S*YNORM
         ENDIF
         Z(K) = Z(K)/A(KK)
      ENDDO
      S = 1.D0 / DASUM ( N, Z, 1 )
      CALL DSCAL ( N, S, Z, 1 ) ! Z_i := S * Z_i
      YNORM = S*YNORM
!
! --- Solve R*Z = V
!
      DO KB=1,N
        K = N+1-KB
        IF ( DABS(Z(K)) .GT. A(KK) ) THEN
             S = A(KK)/DABS(Z(K))
             CALL DSCAL ( N, S, Z, 1 ) ! Z_i := S * Z_i
            YNORM = S*YNORM
        ENDIF
        Z(K) = Z(K)/A(KK)
        KK = KK-K
        T = -Z(K)
        CALL DAXPY ( K-1, T, A(KK+1), 1, Z, 1 ) ! Z_i := Z_i + T * A_kk+1
      ENDDO
!
! --- Make znorm = 1.0
!
      S = 1.D0 / DASUM ( N, Z, 1 )
      CALL DSCAL ( N, S, Z, 1 ) ! Z_i := S * Z_i
      YNORM = S*YNORM
      IF ( ANORM .NE. 0.D0 ) RCOND = YNORM/ANORM
      IF ( ANORM .EQ. 0.D0 ) RCOND = 0.d0
      IF ( RCOND .NE. 0.D0 ) RCOND = 1.d0/RCOND
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  DPPCO_SOLVE_BLAS  #!#
