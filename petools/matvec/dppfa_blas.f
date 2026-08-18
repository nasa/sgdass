#include <mk5_preprocessor_directives.inc>
      SUBROUTINE DPPFA_BLAS ( N, A, EPS, IUER )
      IMPLICIT   NONE
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
      REAL*8 A(*), EPS
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
      INTEGER*4 J, K, JJ, KJ, KK
      REAL*8    S, T
      CHARACTER STR*20, STR1*12
      INTEGER*4 I_LEN
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
         IF ( S .LE. 1.D0/EPS ) THEN
!
! ----------- Print error message to progress and error files:
!
              CALL CLRCH ( STR  )
              CALL CLRCH ( STR1 )
              CALL INCH  ( J, STR )
              WRITE ( UNIT=STR1(1:12), FMT='(1PG12.4)' ) S
              CALL ERR_LOG ( 2, IUER, 'DPPFA_BLAS', 'Failed to decompose. '// &
     &            'Too small element ( '//STR1//' )  at '//STR(1:I_LEN(STR))// &
     &            '-th step' )
             RETURN
         ENDIF
         A(JJ)=DSQRT(S)
      ENDDO
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  DPPFA_BLAS  #!#
