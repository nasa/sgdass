#include <mk5_preprocessor_directives.inc>
      SUBROUTINE DPPFA_SOLVE_BLAS ( N, A, SCAL, B, IUER )
      IMPLICIT   NONE
      INCLUDE   'matvec.i'
!
! 1.  DPPFA PROGRAM SPECIFICATION
!
! 1.1 Cholesky factorization of a SOLVE format matrix.  This is a
!     double precision version of the LINPACK SPPFA routine with
!     direct calls to the HP Vector Instruction Set and I*4 indexing.
!
! 1.2 REFERENCES:
!
! 2.  DPPFA INTERFACE
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
! A - The modified matrix
!
! 2.4 COMMON BLOCKS USED
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES:
!       CALLED SUBROUTINES: ddot
!
! 3.  LOCAL VARIABLES
!
      INTEGER*4 J, K, JJ, KJ, KK, II, I
      REAL*8    S, T
      REAL*8    DDOT
!
! J,K - Loop indices
! JJ,KJ,KK - Used for indexing matrix
! NBLAS - size of vectors for dot product
! S,T - Intermediate values in matrix element calculations
!
! 4.  HISTORY
!   WHO   WHEN   WHAT
!   AEE  910614  Write 'FAILED TO DECOMPOSE' error message to progress file.
!   JMG  960417  Lower test on matrix condition number because small numbers
!                destroy the normal matrix.
!   PET  961226  Added error parameter IUER. Substitute ddot by DP_VV_V
!   PET  2001.04.13  Re-wrote
!
! 5.  DPPFA PROGRAM STRUCTURE
!
      JJ=0
      DO J=1,N
         S=0.0D0
         KJ=JJ
         KK=0
         DO K=1,J-1
            KJ=KJ+1
            T = DDOT ( K-1, A(KK+1), 1, A(JJ+1), 1  )
            T=A(KJ)-T
            KK=KK+K
            T=T/A(KK)
            A(KJ)=T
            S = S + T*T
         ENDDO
         JJ=JJ+J
         S=A(JJ)-S
         IF ( S .LE. 1.D0/COND__MAX ) THEN
              CALL DPPFA_SOLVE_MES ( J, S )
              SCAL(J)=0.0D0
              B(J)=0.0D0
              CALL DCOPY ( J-1, 0.0D0, 0, A(JJ-J+1), 1 )
              II=JJ
              DO I=J+1,N
                 II=II+I-1
                 A(II)=0.0D0
              ENDDO
              S=1.0D0
         ENDIF
         A(JJ)=DSQRT(S)
      ENDDO
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  DPPFA_SOLVE_BLAS  #!#
