#include <mk5_preprocessor_directives.inc>
      SUBROUTINE DPPSL ( A, B, N_INT2 )
      IMPLICIT NONE
      INCLUDE   'matvec.i'
!
! 1.  DPPSL PROGRAM SPECIFICATION
!
! 1.1 Solve from a Cholesky factorization of a the matric in upper triangular
!     representation. This is a double precision version of the LINPACK SPPSL 
!     routine with direct calls to DBLAS and unrolling loopes for matrices
!     of dimension < 7.
!
! 1.2 REFERENCES:
!
! 2.  DPPSL INTERFACE
!
! 2.1 Parameter File
!
! 2.2 INPUT Variables:
!
      INTEGER*2 N_INT2
      REAL*8    A(*)
!
! A - Cholesky factorization of SOLVE format matrix
! N - Order of matrix A
!
! 2.3 OUTPUT Variables:
!
      REAL*8 B(*)
!
! B - Solution vector
!
! 2.4 COMMON BLOCKS USED
!
! 2.5 SUBROUTINE INTERFACE
!
!	CALLING SUBROUTINES:
!       CALLED SUBROUTINES: ddot, daxpy
!
! 3.  LOCAL VARIABLES
!
      INTEGER*4 K, KK, N
#if defined (SUN) || defined (GNU)
      INTEGER*4, EXTERNAL :: INT4
#endif
      REAL*8    T
      REAL*8,   EXTERNAL :: DP_VV_V
!      INTEGER*2  INT2_ARG
!      INTEGER*4  INT4
!      INT4(INT2_ARG) = JINT ( FLOATI(INT2_ARG) )
!
! K - Loop index
! KK - Used for indexing matrix elements
!
! 4.  HISTORY
!   WHO   WHEN        WHAT
!   pet   2001.04.12  Totally re-write and optimized
!
! 5.  DPPSL PROGRAM STRUCTURE 
!
      KK=0
      N = INT4(N_INT2)
!
! --- First small dimensions
!
      IF ( N .EQ. 1 ) THEN
           B(1) = B(1)/A(1)
         ELSE IF ( N .EQ. 2 ) THEN
           B(1) = B(1)/A(1)
           T = A(2)*B(1)
           B(2) = ( B(2)-T )/A(3)
           B(2) = B(2)/A(3)
           B(1) = (B(1) - A(2)*B(2))/A(1)
         ELSE IF ( N .EQ. 3 ) THEN
           B(1) = B(1)/A(1)
           T = A(2)*B(1)
           B(2) = ( B(2)-T )/A(3)
           T = A(4)*B(1) + A(5)*B(2) 
           B(3) = ( B(3)-T )/A(6)
!
           B(3) = B(3)/A(6)
           B(2) = ( B(2) - A(5)*B(3) )/A(3)
           B(1) = ( B(1) - A(2)*B(2) - A(4)*B(3) )/A(1)
         ELSE IF ( N .EQ. 4 ) THEN
           B(1) = B(1)/A(1)
           T = A(2)*B(1)
           B(2) = ( B(2)-T )/A(3)
           T = A(4)*B(1) + A(5)*B(2) 
           B(3) = ( B(3)-T )/A(6)
           T = A(7)*B(1) + A(8)*B(2) + A(9)*B(3) 
           B(4) = ( B(4)-T )/A(10)
!
           B(4) = B(4)/A(10)
           B(3) = ( B(3) - A(9)*B(4) )/A(6)
           B(2) = ( B(2) - A(5)*B(3) - A(8)*B(4) )/A(3)
           B(1) = ( B(1) - A(2)*B(2) - A(4)*B(3) - A(7)*B(4) )/A(1)
         ELSE IF ( N .EQ. 5 ) THEN
           B(1) = B(1)/A(1)
           T = A(2)*B(1)
           B(2) = ( B(2)-T )/A(3)
           T = A(4)*B(1) + A(5)*B(2) 
           B(3) = ( B(3)-T )/A(6)
           T = A(7)*B(1) + A(8)*B(2) + A(9)*B(3) 
           B(4) = ( B(4)-T )/A(10)
           T = A(11)*B(1) + A(12)*B(2) + A(13)*B(3) + A(14)*B(4) 
           B(5) = ( B(5)-T )/A(15)
!
           B(5) = B(5)/A(15)
           B(4) = ( B(4) - A(14)*B(5) )/A(10)
           B(3) = ( B(3) - A(9)*B(4) - A(13)*B(5) )/A(6)
           B(2) = ( B(2) - A(5)*B(3) - A(8)*B(4) - A(12)*B(5) )/A(3)
           B(1) = ( B(1) - A(2)*B(2) - A(4)*B(3) - A(7)*B(4) - A(11)*B(5) )/ &
     &              A(1)
         ELSE IF ( N .EQ. 6 ) THEN
           B(1) = B(1)/A(1)
           T = A(2)*B(1)
           B(2) = ( B(2)-T )/A(3)
           T = A(4)*B(1) + A(5)*B(2) 
           B(3) = ( B(3)-T )/A(6)
           T = A(7)*B(1) + A(8)*B(2) + A(9)*B(3) 
           B(4) = ( B(4)-T )/A(10)
           T = A(11)*B(1) + A(12)*B(2) + A(13)*B(3) + A(14)*B(4) 
           B(5) = ( B(5)-T )/A(15)
           T = A(16)*B(1) + A(17)*B(2) + A(18)*B(3) + A(19)*B(4) + A(20)*B(5) 
           B(6) = ( B(6)-T )/A(21)
!
           B(6) = B(6)/A(21)
           B(5) = ( B(5) - A(20)*B(6) )/A(15)
           B(4) = ( B(4) - A(14)*B(5) - A(19)*B(6) )/A(10)
           B(3) = ( B(3) - A(9)*B(4) - A(13)*B(5) - A(18)*B(6) )/A(6)
           B(2) = ( B(2) - A(5)*B(3) - A(8)*B(4) - A(12)*B(5) - A(17)*B(6) )/ &
     &            A(3)
           B(1) = ( B(1) - A(2)*B(2) - A(4)*B(3) - A(7)*B(4) - A(11)*B(5) - &
     &              A(16)*B(6) )/A(1)
         ELSE 
!
! -------- Dimensions > 6
!
           DO K=1,N
              T = DP_VV_V ( K-1, A(KK+1), B )
              KK = KK+K
              B(K) = ( B(K)-T )/A(KK)
           ENDDO
!
           IF ( N .LT. DB__DPPSL ) THEN
                DO K=N,1,-1
                   B(K) = B(K)/A(KK)
                   KK = KK-K
                   T  = -B(K)
#ifdef HPUX
                   CALL VEC_$DMULT_ADD ( B, A(KK+1), K-1, T, B )
#else
                   CALL DAXPY ( K-1, T, A(KK+1), 1, B, 1 )
#endif
                ENDDO
             ELSE 
                DO K=N,1,-1
                   B(K) = B(K)/A(KK)
                   KK = KK-K
                   T  = -B(K)
                   CALL DAXPY ( K-1, T, A(KK+1), 1, B, 1 )
                ENDDO
           END IF
      END IF
!
      RETURN
      END  !#!  DPPSL  #!#
