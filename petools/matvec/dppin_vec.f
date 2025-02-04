      SUBROUTINE DPPIN_VEC ( N, A )
      IMPLICIT NONE
!
! 1.  DPPIN PROGRAM SPECIFICATION
!
! 1.1 Calculate inverse from a Cholesky factorization of a SOLVE
!     format matrix.  This is a double precision version of the
!     LINPACK SPPDI routine with direct calls to the HP Vector
!     Instruction Set and I*4 indexing.  Only the inversion part
!     of the original LINPACK routine is here.
!
! 1.2 REFERENCES:
!
! 2.  DPPIN INTERFACE
!
! 2.1 Parameter File
!
! 2.2 INPUT Variables:
!
      INTEGER*4 N
      REAL*8    A(*)
!
! A - The SOLVE format matrix
! N - Order of the matrix
!
! 2.3 OUTPUT Variables:
!
! A - The inverted matrix
!
! 2.4 COMMON BLOCKS USED
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES:
!       CALLED SUBROUTINES: dscal,daxpy
!
! 3.  LOCAL VARIABLES
!
      INTEGER*4 J, K, K1, KK, J1, KJ, JJ
      REAL*8    T
!
! J,K - Loop indices
! K1,KK,J1,JJ,KJ - Used for indexing matrix elements
! T - Intermediate value in matrix element calculation
!
! 4.  HISTORY
!   WHO   WHEN   WHAT
!
! 5.  DPPIN PROGRAM STRUCTURE
!
      KK=0
      DO K=1,N
         K1=KK+1
         KK=KK+K
         A(KK)=1.0D0/A(KK)
         T=-A(KK)
#ifdef HPUX
         CALL VEC_$DMULT_CONSTANT ( A(K1), K-1, T, A(K1) ) ! A_k1 := T * A_k1
#endif
         J1=KK+1
         KJ=KK+K
         DO J=K+1,N
            T=A(KJ)
            A(KJ)=0.0D0
#ifdef HPUX
            CALL VEC_$DMULT_ADD ( A(J1), A(K1), K, T, A(J1) ) ! A_j1 := A_j1 +
#endif
            J1=J1+J                                           !         T * A_k1
            KJ=KJ+J
         ENDDO
      ENDDO
!
      JJ=0
      DO J=1,N
         J1=JJ+1
         JJ=JJ+J
         K1=1
         KJ=J1
         DO K=1,J-1
            T=A(KJ)
#ifdef HPUX
            CALL VEC_$DMULT_ADD ( A(K1), A(J1), K, T, A(K1) ) ! A_k1 := A_k1 + T * A_j1
#endif
            K1=K1+K
            KJ=KJ+1
         ENDDO
         T=A(JJ)
#ifdef HPUX
         CALL VEC_$DMULT_CONSTANT ( A(J1), J, T, A(J1) ) ! A_j1 := T * A_j1
#endif
      ENDDO
!
      RETURN
      END  !#!  DPPIN_VEC  #!#
