#include <mk5_preprocessor_directives.inc>
      RECURSIVE SUBROUTINE DREORDER5 ( N, AP, BUF )
! ************************************************************************
! *                                                                      *
! *   Reorders a symmetric, square matrix AP of dimension N from packed  *
! *   upper triangular format to a recursive upper triangular format.    *
! *                                                                      *
! *  ### 2000.02.18    B. Andersen   Original code.                      *
! *  ### 2002.11.28    L. Petrov     massive update. Limited recusrions  *
! *                                  by the order  DB__INVMAT_MIN.       *
! *                                                                      *
! ************************************************************************
      IMPLICIT NONE
      INCLUDE 'matvec.i'
!
      INTEGER*4, INTENT(IN)    :: N
      REAL*8,    INTENT(INOUT) :: AP(*)
      REAL*8,    INTENT(INOUT) :: BUF(*)
!
      INTEGER*4 :: NU, NL
      INTEGER*8 :: P1, P2
!
      IF ( N > DB__INVMAT_MIN ) THEN
           NU = N/2
!@           IF ( NU .LT. DB__INVMAT_MIN ) NU = DB__INVMAT_MIN  
           NL = N - NU
           CALL DREORDER5 ( NU, AP(1), BUF )
           P1 = 1 + INT8(NU)*INT8(NU+1)/2
           P2 = 1 + INT8(N)*INT8(NU) - INT8(NU)*INT8(NU-1)/2
           CALL UPRP      ( N, NU, AP(P1), BUF )
           CALL DREORDER5 ( NL, AP(P2), BUF )
      END IF
!
      END SUBROUTINE DREORDER5
!
! ------------------------------------------------------------------------
!
      RECURSIVE SUBROUTINE DREORDER6 ( N, AP, BUF )
! ************************************************************************
! *                                                                      *
! *   Reorders a symmetric, square matrix AP of dimension N from         *
! *   recursive upper triangular format to packed  upper triangular      *
! *   format.                                                            *
! *                                                                      *
! *  ### 2000.02.18    B. Andersen   Original code.                      *
! *  ### 2002.11.28    L. Petrov     massive update. Limited recusrions  *
! *                                  by the order  DB__INVMAT_MIN.       *
! *                                                                      *
! ************************************************************************
      IMPLICIT NONE
      INCLUDE 'matvec.i'
!
      INTEGER*4, INTENT(IN)    :: N
      REAL*8,    INTENT(INOUT) :: AP(*)
      REAL*8,    INTENT(INOUT) :: BUF(*)
!
!     ...Reorders AP from upper recursive-packed to upper packed format
!
!        UPLO  = 'Upper'
!        TRANS = 'No transpose'
!        TO    = 'Packed'
!
      INTEGER*4  :: NU, NL
      INTEGER*8  :: P1, P2
!
      IF ( N > DB__INVMAT_MIN ) THEN
           NU = N/2
           NL = N - NU
           CALL DREORDER6 ( NU, AP(1), BUF )
           P1 = 1 + INT8(NU)*INT8(NU+1)/2
           P2 = 1 + INT8(N)*INT8(NU) - INT8(NU)*INT8(NU-1)/2
           CALL DREORDER6 ( NL, AP(P2), BUF )
           CALL URPP ( N, NU, AP(P1), BUF )
      END IF
!
      END SUBROUTINE DREORDER6
!
! ------------------------------------------------------------------------
!
      SUBROUTINE UPRP ( M, N, AP, BUF )
      IMPLICIT NONE
!
!     ...Reorders upper trapez AP from packed to recursive-packed format
!
      INTEGER*4, INTENT(IN) :: M, N
      REAL*8,     INTENT(INOUT) :: AP(*)
      REAL*8,     INTENT(INOUT) :: BUF(*)
!
      INTEGER*4   J1, J2, J3, J4
      INTEGER*8   P1, P2, J5
!
      DO 410 J1=1,M-N
         DO 420 J2=1,J1
            P1 = J2 + INT8(J1)*INT8(J1-1)/2
            P2 = J2 + INT8(N)*INT8(J1) + INT8(J1)*INT8(J1-1)/2
            BUF(P1) = AP(P2)
 420     CONTINUE 
 410  CONTINUE 
!
      DO 430 J3=1,M-N
         DO 440 J4=1,N
            P1 = J4 + INT8(N)*INT8(J3-1)
            P2 = J4 + INT8(2*N+J3)*INT8(J3-1)/2
            AP(P1) = AP(P2)
 440     CONTINUE
 430  CONTINUE 
!
      P1 = INT8(M-N)*INT8(M-N+1)/2
!      DO 450 J5=1,P1
!         P2 = INT8(N)*INT8(M-N) + J5
!         AP(P2) = BUF(J5)
! 450  CONTINUE 
      P2 = 1 + INT8(N)*INT8(M-N)
      CALL MEMCPY ( AP(P2), BUF, %VAL(INT8(8)*P1) )
!
      END SUBROUTINE UPRP
!
! ------------------------------------------------------------------------
!
      SUBROUTINE URPP(M,N,AP,BUF)
      IMPLICIT NONE
!
!     ...Reorders upper trapez AP from recursive-packed to packed format
!
      INTEGER*4, INTENT(IN)    :: M,N
      REAL*8,    INTENT(INOUT) :: AP(*)
      REAL*8,    INTENT(INOUT) :: BUF(*)
!
      INTEGER*4   :: J1, J2, J3, J4
      INTEGER*8   P1, P2, K
!
      P1 = INT8(M-N)*INT8(M-N+1)/2
!      DO J=1,P1
!         P2 = N*(M-N)+J
!         BUF(J) = AP(P2)
!      END DO
      P2 = 1 + INT8(N)*INT8(M-N)
      CALL MEMCPY ( BUF, AP(P2), %VAL(INT8(8)*P1) )
!
      DO 410 J1=M-N,1,-1
         DO 420 J2=N,1,-1
            P1 = J2 + INT8(2*N+J1)*INT8(J1-1)/2
            P2 = J2 + INT8(N)*INT8(J1-1)
            AP(P1) = AP(P2)
 420     CONTINUE 
 410  CONTINUE 
      DO 430 J3=1,M-N
         DO 440 J4=1,J3
            P1 = J4 + INT8(N)*INT8(J3) + INT8(J3)*INT8(J3-1)/2
            P2 = J4 + INT8(J3)*INT8(J3-1)/2
            AP(P1) = BUF(P2)
 440     CONTINUE 
 430  CONTINUE 
!
      END SUBROUTINE URPP
!
! ------------------------------------------------------------------------
!
      RECURSIVE  SUBROUTINE DRPPTRF2 ( N, AP, INFO )
! ************************************************************************
! *                                                                      *
! *   Recursive routine for computing Cholesky decomposition of the      *
! *   square symmetric matrix in recursive upper triangular format.      *
! *                                                                      *
! *   ...Factorize AP = U'*U                                             *
! *                                                                      *
! *  ### 2000.02.24    B. Andersen   Original code.                      *
! *  ### 2002.11.28    L. Petrov     massive update. Limited recursions  *
! *                                  by the order  DB__INVMAT_MIN.       *
! *  ### 2006.07.07    L. Petrov     imporved handling singular case.    *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'matvec.i'
      INTEGER*4, INTENT(IN)    :: N
      REAL*8,    INTENT(INOUT) :: AP(*)
      INTEGER*4, INTENT(INOUT) :: INFO
!
      REAL*8,      PARAMETER :: ZERO = 0.0D0, ONE = 1.0D0
      REAL*8    :: EPS
      INTEGER*4 :: NU, NL
      ADDRESS__TYPE :: FUNC_ADDRESS, ARG_LIST(4), FUNC_TRF(DB__INVMAT_MAX)
      ADDRESS__TYPE, EXTERNAL :: DPPTRF_3,  DPPTRF_4
!      EXTERNAL     DPPTRF_5,  DPPTRF_6,  DPPTRF_7,   &
!     &             DPPTRF_8,  DPPTRF_9,  DPPTRF_10, DPPTRF_11, DPPTRF_12,  &
!     &             DPPTRF_13, DPPTRF_14, DPPTRF_15, DPPTRF_16, DPPTRF_17,  &
!     &             DPPTRF_18, DPPTRF_19, DPPTRF_20, DPPTRF_21, DPPTRF_22,  &
!     &             DPPTRF_23, DPPTRF_24
!
      INTEGER*8  P1, P2
      IF ( N == 1 ) THEN
           IF ( AP(1) > ZERO ) THEN
                AP(1) = DSQRT ( AP(1) )
             ELSE
                INFO = 1
           END IF
        ELSE IF ( N == 2 ) THEN
           IF ( AP( 1 ) .LT. DB__INVMAT_EPS ) THEN
                INFO = 1
                RETURN
           END IF
           AP(1) = DSQRT ( AP(1) )
           AP(2) = AP(2)/AP(1)
           IF ( AP( 3  ) .LT. DB__INVMAT_EPS ) THEN
                INFO = 2
                RETURN
           END IF
           AP(3) = AP(3) - AP(2)**2
           IF ( AP( 3  ) .LT. DB__INVMAT_EPS**2 ) THEN
                INFO = 3
                RETURN
           END IF
           AP(3) = DSQRT ( AP(3) )
        ELSE IF ( N .LE. DB__INVMAT_MIN ) THEN
           FUNC_TRF(3) = FUNC_ADDRESS ( DPPTRF_3 )
           FUNC_TRF(4) = FUNC_ADDRESS ( DPPTRF_4 )
!           IF ( N .GT. 4 ) THEN
!                FUNC_TRF(5) = FUNC_ADDRESS ( DPPTRF_5 )
!                FUNC_TRF(6) = FUNC_ADDRESS ( DPPTRF_6 )
!                FUNC_TRF(7) = FUNC_ADDRESS ( DPPTRF_7 )
!                FUNC_TRF(8) = FUNC_ADDRESS ( DPPTRF_8 )
!                FUNC_TRF(9) = FUNC_ADDRESS ( DPPTRF_9 )
!                FUNC_TRF(10) = FUNC_ADDRESS ( DPPTRF_10 )
!                FUNC_TRF(11) = FUNC_ADDRESS ( DPPTRF_11 )
!                FUNC_TRF(12) = FUNC_ADDRESS ( DPPTRF_12 )
!                FUNC_TRF(13) = FUNC_ADDRESS ( DPPTRF_13 )
!                FUNC_TRF(14) = FUNC_ADDRESS ( DPPTRF_14 )
!                FUNC_TRF(15) = FUNC_ADDRESS ( DPPTRF_15 )
!                FUNC_TRF(16) = FUNC_ADDRESS ( DPPTRF_16 )
!                FUNC_TRF(17) = FUNC_ADDRESS ( DPPTRF_17 )
!                FUNC_TRF(18) = FUNC_ADDRESS ( DPPTRF_18 )
!                FUNC_TRF(19) = FUNC_ADDRESS ( DPPTRF_19 )
!                FUNC_TRF(20) = FUNC_ADDRESS ( DPPTRF_20 )
!                FUNC_TRF(21) = FUNC_ADDRESS ( DPPTRF_21 )
!                FUNC_TRF(22) = FUNC_ADDRESS ( DPPTRF_22 )
!                FUNC_TRF(23) = FUNC_ADDRESS ( DPPTRF_23 )
!                FUNC_TRF(24) = FUNC_ADDRESS ( DPPTRF_24 )
!           END IF
!
           EPS = DB__INVMAT_EPS
           ARG_LIST(1) = 3
           ARG_LIST(2) = LOC(AP)
           ARG_LIST(3) = LOC(EPS)
           ARG_LIST(4) = LOC(INFO)
           CALL LIB$CALLG ( ARG_LIST, %VAL(FUNC_TRF(N)) )
        ELSE
           NU = N/2
           NL = N - NU
           CALL DRPPTRF2 ( NU, AP(1), INFO )
           IF ( INFO == 0 ) THEN
                P1 = 1 + INT8(NU)*INT8(NU+1)/2
                P2 = 1 + INT8(N)*INT8(NU) - INT8(NU)*INT8(NU-1)/2
                CALL DRPTRSM4 ( NU, NL, ONE, AP(1), AP(P1), NU )
                CALL DRPSYRK4 ( NL, NU, -ONE, AP(P1), NU, ONE, AP(P2) )
                CALL DRPPTRF2 ( NL, AP(P2), INFO )
                IF ( INFO .NE. 0 ) INFO = NU + INFO
           END IF
      END IF
!
      END SUBROUTINE DRPPTRF2
!
! ------------------------------------------------------------------------
!
      RECURSIVE SUBROUTINE DRPTRTRI2 ( N, AP )
! ************************************************************************
! *                                                                      *
! *   Recursive routine for inversion of a triangular matrix in          *
! *   recursive upper triangular format.                                 *
! *                                                                      *
! *  ### 2000.02.24    B. Andersen   Original code.                      *
! *  ### 2002.11.28    L. Petrov     massive update. Limited recusrions  *
! *                                  by the order  DB__INVMAT_MIN.       *
! *                                                                      *
! ************************************************************************
      IMPLICIT NONE
      INCLUDE 'matvec.i'
!
      INTEGER*4, INTENT(IN)    :: N
      REAL*8,    INTENT(INOUT) :: AP(*)
!
      INTEGER*4 :: NU, NL, IER 
      REAL*8    :: BETA
      REAL*8,      PARAMETER :: ONE=1.0D0
      ADDRESS__TYPE :: FUNC_ADDRESS, ARG_LIST(3), FUNC_TRI(DB__INVMAT_MAX)
      ADDRESS__TYPE, EXTERNAL :: DPPTRI_3,  DPPTRI_4
!      EXTERNAL   DPPTRI_5,  DPPTRI_6,  DPPTRI_7,   &
!     &           DPPTRI_8,  DPPTRI_9,  DPPTRI_10, DPPTRI_11, DPPTRI_12,  &
!     &           DPPTRI_13, DPPTRI_14, DPPTRI_15, DPPTRI_16, DPPTRI_17,  &
!     &           DPPTRI_18, DPPTRI_19, DPPTRI_20, DPPTRI_21, DPPTRI_22,  &
!     &           DPPTRI_23, DPPTRI_24
!
      INTEGER*8  P1, P2
      IF ( N == 1) THEN
           AP(1) = ONE/AP(1)
        ELSE IF ( N == 2 ) THEN
           AP(1) = 1.0D0/AP(1)
           AP(2) = -AP(1)*AP(2)/AP(3)
           AP(3) = 1.0D0/AP(3)
        ELSE IF ( N .LE. DB__INVMAT_MIN ) THEN
           FUNC_TRI(3) = FUNC_ADDRESS ( DPPTRI_3 )
           FUNC_TRI(4) = FUNC_ADDRESS ( DPPTRI_4 )
!           IF ( N .GT. 4 ) THEN
!                FUNC_TRI(5) = FUNC_ADDRESS ( DPPTRI_5 )
!                FUNC_TRI(6) = FUNC_ADDRESS ( DPPTRI_6 )
!                FUNC_TRI(7) = FUNC_ADDRESS ( DPPTRI_7 )
!                FUNC_TRI(8) = FUNC_ADDRESS ( DPPTRI_8 )
!                FUNC_TRI(9) = FUNC_ADDRESS ( DPPTRI_9 )
!                FUNC_TRI(10) = FUNC_ADDRESS ( DPPTRI_10 )
!                FUNC_TRI(11) = FUNC_ADDRESS ( DPPTRI_11 )
!                FUNC_TRI(12) = FUNC_ADDRESS ( DPPTRI_12 )
!                FUNC_TRI(13) = FUNC_ADDRESS ( DPPTRI_13 )
!                FUNC_TRI(14) = FUNC_ADDRESS ( DPPTRI_14 )
!                FUNC_TRI(15) = FUNC_ADDRESS ( DPPTRI_15 )
!                FUNC_TRI(16) = FUNC_ADDRESS ( DPPTRI_16 )
!                FUNC_TRI(17) = FUNC_ADDRESS ( DPPTRI_17 )
!                FUNC_TRI(18) = FUNC_ADDRESS ( DPPTRI_18 )
!                FUNC_TRI(19) = FUNC_ADDRESS ( DPPTRI_19 )
!                FUNC_TRI(20) = FUNC_ADDRESS ( DPPTRI_20 )
!                FUNC_TRI(21) = FUNC_ADDRESS ( DPPTRI_21 )
!                FUNC_TRI(22) = FUNC_ADDRESS ( DPPTRI_22 )
!                FUNC_TRI(23) = FUNC_ADDRESS ( DPPTRI_23 )
!                FUNC_TRI(24) = FUNC_ADDRESS ( DPPTRI_24 )
!           END IF
!
           ARG_LIST(1) = 2
           ARG_LIST(2) = LOC(AP)
           ARG_LIST(3) = LOC(IER)
           CALL LIB$CALLG ( ARG_LIST, %VAL(FUNC_TRI(N)) )
        ELSE
           NU = N/2
!@           IF ( NU .LT. DB__INVMAT_MIN ) NU = DB__INVMAT_MIN  
           NL = N - NU
           P1 = 1 + INT8(NU)*INT8(NU+1)/2
           P2 = 1 + INT8(N)*INT8(NU) - INT8(NU)*INT8(NU-1)/2
           CALL DRPTRTRI2 ( NL, AP(P2) )
           CALL DRPTRMM7  ( NU, NL, -ONE, AP(P2), AP(P1), NU )
           CALL DRPTRSM3  ( NU, NL, ONE, AP(1), AP(P1), NU )
           CALL DRPTRTRI2 ( NU, AP(1) )
      ENDIF
!
      END SUBROUTINE DRPTRTRI2
!
! ------------------------------------------------------------------------
!
      RECURSIVE SUBROUTINE DRPTRRK3 ( N, AP )
! ************************************************************************
! *                                                                      *
! *   Recursive routine for computing A: = A * A(T)  where A is          *
! *   a triangular matrix in recursive upper triangular format.          *
! *                                                                      *
! *  ### 2000.02.24    B. Andersen   Original code.                      *
! *  ### 2002.11.28    L. Petrov     Massive update. Limited recusrions  *
! *                                  by the order  DB__INVMAT_MIN.       *
! *                                                                      *
! ************************************************************************
      IMPLICIT NONE
      INCLUDE 'matvec.i'
!
      INTEGER*4, INTENT(IN)    :: N
      REAL*8,    INTENT(INOUT) :: AP(*)
!
      INTEGER*4 :: NU, NL, IER
      REAL*8,      PARAMETER :: ONE=1.0D0
      ADDRESS__TYPE :: FUNC_ADDRESS, ARG_LIST(2), FUNC_TRK(DB__INVMAT_MAX)
      ADDRESS__TYPE, EXTERNAL :: DPPTRK_3,  DPPTRK_4
!      EXTERNAL     DPPTRK_5,  DPPTRK_6,  DPPTRK_7,   &
!     &             DPPTRK_8,  DPPTRK_9,  DPPTRK_10, DPPTRK_11, DPPTRK_12,  &
!     &             DPPTRK_13, DPPTRK_14, DPPTRK_15, DPPTRK_16, DPPTRK_17,  &
!     &             DPPTRK_18, DPPTRK_19, DPPTRK_20, DPPTRK_21, DPPTRK_22,  &
!     &             DPPTRK_23, DPPTRK_24
!
      INTEGER*8  P1, P2
      IF ( N == 1 ) THEN
           AP(1) = AP(1)**2
        ELSE IF ( N == 2 ) THEN
           AP(1) = AP(1)**2 + AP(2)**2
           AP(2) = AP(2)*AP(3)
           AP(3) = AP(3)**2
        ELSE IF ( N .LE. DB__INVMAT_MIN ) THEN
           FUNC_TRK(3) = FUNC_ADDRESS ( DPPTRK_3 )
           FUNC_TRK(4) = FUNC_ADDRESS ( DPPTRK_4 )
!           IF ( N .GT. 4 ) THEN
!                FUNC_TRK(5) = FUNC_ADDRESS ( DPPTRK_5 )
!                FUNC_TRK(6) = FUNC_ADDRESS ( DPPTRK_6 )
!                FUNC_TRK(7) = FUNC_ADDRESS ( DPPTRK_7 )
!                FUNC_TRK(8) = FUNC_ADDRESS ( DPPTRK_8 )
!                FUNC_TRK(9) = FUNC_ADDRESS ( DPPTRK_9 )
!                FUNC_TRK(10) = FUNC_ADDRESS ( DPPTRK_10 )
!                FUNC_TRK(11) = FUNC_ADDRESS ( DPPTRK_11 )
!                FUNC_TRK(12) = FUNC_ADDRESS ( DPPTRK_12 )
!                FUNC_TRK(13) = FUNC_ADDRESS ( DPPTRK_13 )
!                FUNC_TRK(14) = FUNC_ADDRESS ( DPPTRK_14 )
!                FUNC_TRK(15) = FUNC_ADDRESS ( DPPTRK_15 )
!                FUNC_TRK(16) = FUNC_ADDRESS ( DPPTRK_16 )
!                FUNC_TRK(17) = FUNC_ADDRESS ( DPPTRK_17 )
!                FUNC_TRK(18) = FUNC_ADDRESS ( DPPTRK_18 )
!                FUNC_TRK(19) = FUNC_ADDRESS ( DPPTRK_19 )
!                FUNC_TRK(20) = FUNC_ADDRESS ( DPPTRK_20 )
!                FUNC_TRK(21) = FUNC_ADDRESS ( DPPTRK_21 )
!                FUNC_TRK(22) = FUNC_ADDRESS ( DPPTRK_22 )
!                FUNC_TRK(23) = FUNC_ADDRESS ( DPPTRK_23 )
!                FUNC_TRK(24) = FUNC_ADDRESS ( DPPTRK_24 )
!           END IF
!
           ARG_LIST(1) = 1
           ARG_LIST(2) = LOC(AP)
           CALL LIB$CALLG ( ARG_LIST, %VAL(FUNC_TRK(N)) )
         ELSE
           NU = N/2
!@           IF ( NU .LT. DB__INVMAT_MIN ) NU = DB__INVMAT_MIN  
           NL = N - NU
           CALL DRPTRRK3 ( NU, AP(1) )
           P1 = 1 + INT8(NU)*INT8(NU+1)/2
           P2 = 1 + INT8(N)*INT8(NU) - INT8(NU)*INT8(NU-1)/2
           CALL DRPSYRK3 ( NU, NL, ONE, AP(P1), NU, ONE, AP(1) )
           CALL DRPTRMM8 ( NU, NL, ONE, AP(P2), AP(P1), NU )
           CALL DRPTRRK3 ( NL, AP(P2) )
      ENDIF
!
      END SUBROUTINE DRPTRRK3
!
! ------------------------------------------------------------------------
!
      RECURSIVE SUBROUTINE DRPTRSM3 ( M, N, ALPHA, AP, B, LDB )
! ************************************************************************
! *                                                                      *
! *   Recursive routine for solving alpha * B = AP * X where AP is       *
! *   a triangular matrix in recursive upper triangular format.          *
! *                                                                      *
! *  ### 2000.03.23    B. Andersen   Original code.                      *
! *  ### 2002.11.28    L. Petrov     Massive update. Limited recusrions  *
! *                                  by the order  DB__INVMAT_MIN.       *
! *                                                                      *
! ************************************************************************
      IMPLICIT NONE
      INCLUDE 'matvec.i'
!
      INTEGER*4, INTENT(IN)    :: M, N, LDB
      REAL*8,    INTENT(IN)    :: ALPHA
      REAL*8,    INTENT(IN)    :: AP(*)
      REAL*8,    INTENT(INOUT) :: B(LDB,*)
!
      REAL*8     :: BETA
      REAL*8,       PARAMETER  :: ONE=1.0D0
      INTEGER*4  :: MU, ML, J
      ADDRESS__TYPE :: FUNC_ADDRESS, ARG_LIST(5), FUNC_TRSM3(DB__INVMAT_MAX)
      ADDRESS__TYPE, EXTERNAL :: TRSM3_1,  TRSM3_2,  TRSM3_3,  TRSM3_4
!      EXTERNAL      TRSM3_5,  TRSM3_6,  TRSM3_7,  TRSM3_8,  TRSM3_9,  &
!     &              TRSM3_10, TRSM3_11, TRSM3_12, TRSM3_13, TRSM3_14, &
!     &              TRSM3_15, TRSM3_16, TRSM3_17, TRSM3_18, TRSM3_19, &
!     &              TRSM3_20, TRSM3_21, TRSM3_22, TRSM3_23, TRSM3_24
!
      INTEGER*8  P1, P2
      IF ( M == 1 ) THEN
           BETA=ALPHA/AP(1)
           DO J=1,N
              B(1,J)=BETA*B(1,J)
           END DO
         ELSE IF ( M .LE. DB__INVMAT_MIN ) THEN
           FUNC_TRSM3(2) = FUNC_ADDRESS ( TRSM3_2 )
           FUNC_TRSM3(3) = FUNC_ADDRESS ( TRSM3_3 )
           FUNC_TRSM3(4) = FUNC_ADDRESS ( TRSM3_4 )
!           IF ( M .GT. 4 ) THEN
!                FUNC_TRSM3(5) = FUNC_ADDRESS ( TRSM3_5 )
!                FUNC_TRSM3(6) = FUNC_ADDRESS ( TRSM3_6 )
!                FUNC_TRSM3(7) = FUNC_ADDRESS ( TRSM3_7 )
!                FUNC_TRSM3(8) = FUNC_ADDRESS ( TRSM3_8 )
!                FUNC_TRSM3(9) = FUNC_ADDRESS ( TRSM3_9 )
!                FUNC_TRSM3(10) = FUNC_ADDRESS ( TRSM3_10 )
!                FUNC_TRSM3(11) = FUNC_ADDRESS ( TRSM3_11 )
!                FUNC_TRSM3(12) = FUNC_ADDRESS ( TRSM3_12 )
!                FUNC_TRSM3(13) = FUNC_ADDRESS ( TRSM3_13 )
!                FUNC_TRSM3(14) = FUNC_ADDRESS ( TRSM3_14 )
!                FUNC_TRSM3(15) = FUNC_ADDRESS ( TRSM3_15 )
!                FUNC_TRSM3(16) = FUNC_ADDRESS ( TRSM3_16 )
!                FUNC_TRSM3(17) = FUNC_ADDRESS ( TRSM3_17 )
!                FUNC_TRSM3(18) = FUNC_ADDRESS ( TRSM3_18 )
!                FUNC_TRSM3(19) = FUNC_ADDRESS ( TRSM3_19 )
!                FUNC_TRSM3(20) = FUNC_ADDRESS ( TRSM3_20 )
!                FUNC_TRSM3(21) = FUNC_ADDRESS ( TRSM3_21 )
!                FUNC_TRSM3(22) = FUNC_ADDRESS ( TRSM3_22 )
!                FUNC_TRSM3(23) = FUNC_ADDRESS ( TRSM3_23 )
!                FUNC_TRSM3(24) = FUNC_ADDRESS ( TRSM3_24 )
!           END IF
!
           ARG_LIST(1) = 4
           ARG_LIST(2) = LOC(N)
           ARG_LIST(3) = LOC(AP(1))
           ARG_LIST(4) = LOC(B(1,1))
           ARG_LIST(5) = LOC(LDB)
           CALL LIB$CALLG ( ARG_LIST, %VAL(FUNC_TRSM3(M)) )
        ELSE
           MU = M/2
!@           IF ( MU .LT. DB__INVMAT_MIN ) MU = DB__INVMAT_MIN  
           ML = M - MU
           P1 = 1 + INT8(MU)*INT8(MU+1)/2
           P2 = 1 + INT8(M)*INT8(MU) - INT8(MU)*INT8(MU-1)/2
           CALL DRPTRSM3 ( ML, N, ALPHA, AP(P2), B(MU+1,1), LDB )
           CALL DGEMM ( 'N', 'N', MU, N, ML, -ONE/ALPHA, AP(P1), &
     &                   MU, B(MU+1,1), LDB, ONE, B(1,1), LDB )
           CALL DRPTRSM3 ( MU, N, ALPHA, AP(1), B(1,1), LDB )
      END IF
!
      END SUBROUTINE DRPTRSM3
!
! ------------------------------------------------------------------------
!
      RECURSIVE SUBROUTINE DRPTRSM4 ( M, N, ALPHA, AP, B, LDB )
! ************************************************************************
! *                                                                      *
! *   Recursive routine for solving alpha * B = AP(T) * X  where AP is   *
! *   a triangular matrix in recursive upper triangular format.          *
! *                                                                      *
! *  ### 2000.03.23    B. Andersen   Original code.                      *
! *  ### 2002.11.28    L. Petrov     Massive update. Limited recusrions  *
! *                                  by the order  DB__INVMAT_MIN.       *
! *                                                                      *
! ************************************************************************
      IMPLICIT NONE
      INCLUDE 'matvec.i'
!
      INTEGER*4, INTENT(IN)     :: M,N,LDB
      REAL*8,    INTENT(IN)     :: ALPHA
      REAL*8,    INTENT(IN)     :: AP(*)
      REAL*8,    INTENT(INOUT)  :: B(LDB,*)
!
      REAL*8     :: BETA
      REAL*8,       PARAMETER :: ONE=1.0D0
      INTEGER*4  :: MU, ML, J
      ADDRESS__TYPE :: FUNC_ADDRESS, ARG_LIST(5), FUNC_TRSM4(DB__INVMAT_MAX)
      ADDRESS__TYPE, EXTERNAL :: TRSM4_1,  TRSM4_2,  TRSM4_3,  TRSM4_4
!      EXTERNAL      TRSM4_5,  TRSM4_6,  TRSM4_7,  TRSM4_8,  TRSM4_9,  &
!     &              TRSM4_10, TRSM4_11, TRSM4_12, TRSM4_13, TRSM4_14, &
!     &              TRSM4_15, TRSM4_16, TRSM4_17, TRSM4_18, TRSM4_19, &
!     &              TRSM4_20, TRSM4_21, TRSM4_22, TRSM4_23, TRSM4_24
!
      INTEGER*8  P1, P2
      IF ( M == 1 ) THEN
           BETA=ALPHA/AP(1)
           DO J=1,N
              B(1,J)=BETA*B(1,J)
           END DO
        ELSE IF ( M .LE. DB__INVMAT_MIN ) THEN
           FUNC_TRSM4(2) = FUNC_ADDRESS ( TRSM4_2 )
           FUNC_TRSM4(3) = FUNC_ADDRESS ( TRSM4_3 )
           FUNC_TRSM4(4) = FUNC_ADDRESS ( TRSM4_4 )
!           IF ( M .GT. 4 ) THEN
!                FUNC_TRSM4(5) = FUNC_ADDRESS ( TRSM4_5 )
!                FUNC_TRSM4(6) = FUNC_ADDRESS ( TRSM4_6 )
!                FUNC_TRSM4(7) = FUNC_ADDRESS ( TRSM4_7 )
!                FUNC_TRSM4(8) = FUNC_ADDRESS ( TRSM4_8 )
!                FUNC_TRSM4(9) = FUNC_ADDRESS ( TRSM4_9 )
!                FUNC_TRSM4(10) = FUNC_ADDRESS ( TRSM4_10 )
!                FUNC_TRSM4(11) = FUNC_ADDRESS ( TRSM4_11 )
!                FUNC_TRSM4(12) = FUNC_ADDRESS ( TRSM4_12 )
!                FUNC_TRSM4(13) = FUNC_ADDRESS ( TRSM4_13 )
!                FUNC_TRSM4(14) = FUNC_ADDRESS ( TRSM4_14 )
!                FUNC_TRSM4(15) = FUNC_ADDRESS ( TRSM4_15 )
!                FUNC_TRSM4(16) = FUNC_ADDRESS ( TRSM4_16 )
!                FUNC_TRSM4(17) = FUNC_ADDRESS ( TRSM4_17 )
!                FUNC_TRSM4(18) = FUNC_ADDRESS ( TRSM4_18 )
!                FUNC_TRSM4(19) = FUNC_ADDRESS ( TRSM4_19 )
!                FUNC_TRSM4(20) = FUNC_ADDRESS ( TRSM4_20 )
!                FUNC_TRSM4(21) = FUNC_ADDRESS ( TRSM4_21 )
!                FUNC_TRSM4(22) = FUNC_ADDRESS ( TRSM4_22 )
!                FUNC_TRSM4(23) = FUNC_ADDRESS ( TRSM4_23 )
!                FUNC_TRSM4(24) = FUNC_ADDRESS ( TRSM4_24 )
!           END IF
!
           ARG_LIST(1) = 4
           ARG_LIST(2) = LOC(N)
           ARG_LIST(3) = LOC(AP(1))
           ARG_LIST(4) = LOC(B(1,1))
           ARG_LIST(5) = LOC(LDB)
           CALL LIB$CALLG ( ARG_LIST, %VAL(FUNC_TRSM4(M)) )
        ELSE
           MU = M/2
!@           IF ( MU .LT. DB__INVMAT_MIN ) MU = DB__INVMAT_MIN  
           ML = M - MU
           P1 = 1 + INT8(MU)*INT8(MU+1)/2
           P2 = 1 + INT8(M)*INT8(MU) - INT8(MU)*INT8(MU-1)/2
           CALL DRPTRSM4 ( MU, N, ALPHA, AP(1), B(1,1), LDB )
           CALL DGEMM ( 'T', 'N', ML, N, MU, -ONE/ALPHA, AP(P1), &
     &                   MU, B(1,1), LDB, ONE, B(MU+1,1), LDB )
           CALL DRPTRSM4 ( ML, N, ALPHA, AP(P2), B(MU+1,1), &
     &                     LDB )
      END IF
!
      END SUBROUTINE DRPTRSM4
!
! ------------------------------------------------------------------------
!
      RECURSIVE SUBROUTINE DRPSYRK3 ( M, N, ALPHA, A, LDA, BETA, CP )
! ************************************************************************
! *                                                                      *
! *   Recursive routine for computing CP = alpha*A*A' + beta*CP          *
! *   where A square symmetric matrix in recursive upper triangular      *
! *   format of dimension N ad A is a rectangular matrix M*N             *
! *   dimensioned as LDA*N                                               *
! *                                                                      *
! *  ### 2000.02.18    B. Andersen   Original code.                      *
! *  ### 2002.11.28    L. Petrov     massive update. Limited recusrions  *
! *                                  by the order  DB__INVMAT_MIN.       *
! *                                                                      *
! ************************************************************************
      IMPLICIT NONE
      INCLUDE 'matvec.i'
!
      INTEGER*4, INTENT(IN)    :: M, N, LDA
      REAL*8,    INTENT(IN)    :: ALPHA, BETA
      REAL*8,    INTENT(IN)    :: A(LDA,*)
      REAL*8,    INTENT(INOUT) :: CP(*)
!
      INTEGER*4  :: MU, ML, J
      ADDRESS__TYPE :: FUNC_ADDRESS, ARG_LIST(5), FUNC_SYRK3(DB__INVMAT_MAX)
      ADDRESS__TYPE, EXTERNAL :: SYRK3_1,  SYRK3_2,  SYRK3_3,  SYRK3_4
!      EXTERNAL      SYRK3_5,  SYRK3_6,  SYRK3_7,  SYRK3_8,  SYRK3_9,  &
!     &              SYRK3_10, SYRK3_11, SYRK3_12, SYRK3_13, SYRK3_14, &
!     &              SYRK3_15, SYRK3_16, SYRK3_17, SYRK3_18, SYRK3_19, &
!     &              SYRK3_20, SYRK3_21, SYRK3_22, SYRK3_23, SYRK3_24
!
      INTEGER*8  P1, P2
      IF ( M == 1 ) THEN
           CP(1)=BETA * CP(1)
           DO J=1,N
              CP(1) = CP(1) + ALPHA*A(1,J)**2
           END DO
        ELSE IF ( M .LE. DB__INVMAT_MIN ) THEN
           FUNC_SYRK3(2) = FUNC_ADDRESS ( SYRK3_2 )
           FUNC_SYRK3(3) = FUNC_ADDRESS ( SYRK3_3 )
           FUNC_SYRK3(4) = FUNC_ADDRESS ( SYRK3_4 )
!           IF ( M .GT. 4 ) THEN
!                FUNC_SYRK3(5) = FUNC_ADDRESS ( SYRK3_5 )
!                FUNC_SYRK3(6) = FUNC_ADDRESS ( SYRK3_6 )
!                FUNC_SYRK3(7) = FUNC_ADDRESS ( SYRK3_7 )
!                FUNC_SYRK3(8) = FUNC_ADDRESS ( SYRK3_8 )
!                FUNC_SYRK3(9) = FUNC_ADDRESS ( SYRK3_9 )
!                FUNC_SYRK3(10) = FUNC_ADDRESS ( SYRK3_10 )
!                FUNC_SYRK3(11) = FUNC_ADDRESS ( SYRK3_11 )
!                FUNC_SYRK3(12) = FUNC_ADDRESS ( SYRK3_12 )
!                FUNC_SYRK3(13) = FUNC_ADDRESS ( SYRK3_13 )
!                FUNC_SYRK3(14) = FUNC_ADDRESS ( SYRK3_14 )
!                FUNC_SYRK3(15) = FUNC_ADDRESS ( SYRK3_15 )
!                FUNC_SYRK3(16) = FUNC_ADDRESS ( SYRK3_16 )
!                FUNC_SYRK3(17) = FUNC_ADDRESS ( SYRK3_17 )
!                FUNC_SYRK3(18) = FUNC_ADDRESS ( SYRK3_18 )
!                FUNC_SYRK3(19) = FUNC_ADDRESS ( SYRK3_19 )
!                FUNC_SYRK3(20) = FUNC_ADDRESS ( SYRK3_20 )
!                FUNC_SYRK3(21) = FUNC_ADDRESS ( SYRK3_21 )
!                FUNC_SYRK3(22) = FUNC_ADDRESS ( SYRK3_22 )
!                FUNC_SYRK3(23) = FUNC_ADDRESS ( SYRK3_23 )
!                FUNC_SYRK3(24) = FUNC_ADDRESS ( SYRK3_24 )
!           END IF
!
           ARG_LIST(1) = 4
           ARG_LIST(2) = LOC(N)
           ARG_LIST(3) = LOC(A(1,1))
           ARG_LIST(4) = LOC(LDA)
           ARG_LIST(5) = LOC(CP(1))
           CALL LIB$CALLG ( ARG_LIST, %VAL(FUNC_SYRK3(M)) )
        ELSE
           MU = M/2
!@           IF ( MU .LT. DB__INVMAT_MIN ) MU = DB__INVMAT_MIN  
           ML = M - MU
           CALL DRPSYRK3 ( MU, N, ALPHA, A(1,1), LDA, BETA, CP(1) )
           P1 = 1 + INT8(MU)*INT8(MU+1)/2
           P2 = 1 + INT8(M)*INT8(MU) - INT8(MU)*INT8(MU-1)/2
           CALL DGEMM ( 'N', 'T', MU, ML, N, ALPHA, A(1,1), LDA, A(MU+1,1), &
     &                  LDA, BETA, CP(P1), MU )
           CALL DRPSYRK3 ( ML, N, ALPHA, A(MU+1,1), LDA, BETA, CP(P2) )
      END IF
!
      END SUBROUTINE DRPSYRK3
!
! ------------------------------------------------------------------------
!
      RECURSIVE SUBROUTINE DRPSYRK4 ( M, N, ALPHA, A, LDA, BETA, CP )
! ************************************************************************
! *                                                                      *
! *   Recursive routine for computing CP = alpha*A'*A + beta*CP          *
! *   where CP square symmetric matrix in recursive upper triangular     *
! *   format of dimension N and B is a rectangular matrix M*N            *
! *   dimensioned as LDA*N.                                              *
! *                                                                      *
! *  ### 2000.02.18    B. Andersen   Original code.                      *
! *  ### 2002.11.28    L. Petrov     massive update. Limited recusrions  *
! *                                  by the order  DB__INVMAT_MIN.       *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'matvec.i'
      INTEGER*4, INTENT(IN)    :: M, N, LDA
      REAL*8,    INTENT(IN)    :: ALPHA, BETA
      REAL*8,    INTENT(IN)    :: A(LDA,*)
      REAL*8,    INTENT(INOUT) :: CP(*)
!
      ADDRESS__TYPE :: FUNC_ADDRESS, ARG_LIST(5), FUNC_SYRK4(DB__INVMAT_MAX)
      ADDRESS__TYPE, EXTERNAL :: SYRK4_1,  SYRK4_2,  SYRK4_3,  SYRK4_4
!      EXTERNAL      SYRK4_5,  SYRK4_6,  SYRK4_7,  SYRK4_8,  SYRK4_9,  &
!     &              SYRK4_10, SYRK4_11, SYRK4_12, SYRK4_13, SYRK4_14, &
!     &              SYRK4_15, SYRK4_16, SYRK4_17, SYRK4_18, SYRK4_19, &
!     &              SYRK4_20, SYRK4_21, SYRK4_22, SYRK4_23, SYRK4_24
!
      INTEGER*4 :: MU, ML, I
      INTEGER*8 :: P1, P2
!
      IF ( M == 1 ) THEN
           CP(1)=BETA*CP(1)
           DO I=1,N
              CP(1) = CP(1) + ALPHA*A(I,1)**2
           END DO
        ELSE IF ( M .LE. DB__INVMAT_MIN ) THEN
           FUNC_SYRK4(2) = FUNC_ADDRESS ( SYRK4_2 )
           FUNC_SYRK4(3) = FUNC_ADDRESS ( SYRK4_3 )
           FUNC_SYRK4(4) = FUNC_ADDRESS ( SYRK4_4 )
!           IF ( M .GT. 4 ) THEN
!                FUNC_SYRK4(5) = FUNC_ADDRESS ( SYRK4_5 )
!                FUNC_SYRK4(6) = FUNC_ADDRESS ( SYRK4_6 )
!                FUNC_SYRK4(7) = FUNC_ADDRESS ( SYRK4_7 )
!                FUNC_SYRK4(8) = FUNC_ADDRESS ( SYRK4_8 )
!                FUNC_SYRK4(9) = FUNC_ADDRESS ( SYRK4_9 )
!                FUNC_SYRK4(10) = FUNC_ADDRESS ( SYRK4_10 )
!                FUNC_SYRK4(11) = FUNC_ADDRESS ( SYRK4_11 )
!                FUNC_SYRK4(12) = FUNC_ADDRESS ( SYRK4_12 )
!                FUNC_SYRK4(13) = FUNC_ADDRESS ( SYRK4_13 )
!                FUNC_SYRK4(14) = FUNC_ADDRESS ( SYRK4_14 )
!                FUNC_SYRK4(15) = FUNC_ADDRESS ( SYRK4_15 )
!                FUNC_SYRK4(16) = FUNC_ADDRESS ( SYRK4_16 )
!                FUNC_SYRK4(17) = FUNC_ADDRESS ( SYRK4_17 )
!                FUNC_SYRK4(18) = FUNC_ADDRESS ( SYRK4_18 )
!                FUNC_SYRK4(19) = FUNC_ADDRESS ( SYRK4_19 )
!                FUNC_SYRK4(20) = FUNC_ADDRESS ( SYRK4_20 )
!                FUNC_SYRK4(21) = FUNC_ADDRESS ( SYRK4_21 )
!                FUNC_SYRK4(22) = FUNC_ADDRESS ( SYRK4_22 )
!                FUNC_SYRK4(23) = FUNC_ADDRESS ( SYRK4_23 )
!                FUNC_SYRK4(24) = FUNC_ADDRESS ( SYRK4_24 )
!           END IF
!
           ARG_LIST(1) = 4
           ARG_LIST(2) = LOC(N)
           ARG_LIST(3) = LOC(A(1,1))
           ARG_LIST(4) = LOC(LDA)
           ARG_LIST(5) = LOC(CP(1))
           CALL LIB$CALLG ( ARG_LIST, %VAL(FUNC_SYRK4(M)) )
        ELSE
           MU = M/2
!@           IF ( MU .LT. DB__INVMAT_MIN ) MU = DB__INVMAT_MIN  
           ML = M - MU
           P1 = 1 + INT8(MU)*INT8(MU+1)/2
           P2 = 1 + INT8(M)*INT8(MU) - INT8(MU)*INT8(MU-1)/2
           CALL DRPSYRK4 ( MU, N, ALPHA, A(1,1), LDA, BETA, CP(1) )
           CALL DGEMM ( 'T', 'N', MU, ML, N, ALPHA, A(1,1), LDA, A(1,MU+1), &
     &                   LDA, BETA, CP(P1), MU )
           CALL DRPSYRK4 ( ML, N, ALPHA, A(1,MU+1), LDA, BETA, &
     &                     CP(P2) )
      END IF
!
      END SUBROUTINE DRPSYRK4
!
! ------------------------------------------------------------------------
!
      RECURSIVE SUBROUTINE DRPTRMM7 ( M, N, ALPHA, AP, B, LDB )
! ************************************************************************
! *                                                                      *
! *   Recursive routine for computing B = alpha * B * AP                 *
! *   where AP square symmetric matrix in recursive upper triangular     *
! *   format of dimension N and B is a rectangular matrix M*N            *
! *   dimensioned as LDA*N.                                              *
! *                                                                      *
! *  ### 2000.02.18    B. Andersen   Original code.                      *
! *  ### 2002.11.28    L. Petrov     massive update. Limited recusrions  *
! *                                  by the order  DB__INVMAT_MIN.       *
! *                                                                      *
! ************************************************************************
      IMPLICIT NONE
      INCLUDE 'matvec.i'
!
      INTEGER*4, INTENT(IN)    :: M, N, LDB
      REAL*8,    INTENT(IN)    :: ALPHA
      REAL*8,    INTENT(IN)    :: AP(*)
      REAL*8,    INTENT(INOUT) :: B(LDB,*)
!
      INTEGER*4  :: NU, NL, I
      REAL*8     :: BETA
      REAL*8,       PARAMETER :: ONE=1.0D+0
      ADDRESS__TYPE :: FUNC_ADDRESS, ARG_LIST(5), FUNC_TRMM7(DB__INVMAT_MAX)
      ADDRESS__TYPE, EXTERNAL :: TRMM7_1,  TRMM7_2,  TRMM7_3,  TRMM7_4
!      EXTERNAL      TRMM7_5,  TRMM7_6,  TRMM7_7,  TRMM7_8,  TRMM7_9,  &
!     &              TRMM7_10, TRMM7_11, TRMM7_12, TRMM7_13, TRMM7_14, &
!     &              TRMM7_15, TRMM7_16, TRMM7_17, TRMM7_18, TRMM7_19, &
!     &              TRMM7_20, TRMM7_21, TRMM7_22, TRMM7_23, TRMM7_24
!
      INTEGER*8  :: P1, P2
      IF ( N == 1 ) THEN
           BETA=ALPHA*AP(1)
           DO I=1,M
              B(I,1) = BETA * B(I,1)
           ENDDO
        ELSE IF ( N .LE. DB__INVMAT_MIN ) THEN
           FUNC_TRMM7(2) = FUNC_ADDRESS ( TRMM7_2 )
           FUNC_TRMM7(3) = FUNC_ADDRESS ( TRMM7_3 )
           FUNC_TRMM7(4) = FUNC_ADDRESS ( TRMM7_4 )
!           IF ( N .GT. 4 ) THEN
!                FUNC_TRMM7(5) = FUNC_ADDRESS ( TRMM7_5 )
!                FUNC_TRMM7(6) = FUNC_ADDRESS ( TRMM7_6 )
!                FUNC_TRMM7(7) = FUNC_ADDRESS ( TRMM7_7 )
!                FUNC_TRMM7(8) = FUNC_ADDRESS ( TRMM7_8 )
!                FUNC_TRMM7(9) = FUNC_ADDRESS ( TRMM7_9 )
!                FUNC_TRMM7(10) = FUNC_ADDRESS ( TRMM7_10 )
!                FUNC_TRMM7(11) = FUNC_ADDRESS ( TRMM7_11 )
!                FUNC_TRMM7(12) = FUNC_ADDRESS ( TRMM7_12 )
!                FUNC_TRMM7(13) = FUNC_ADDRESS ( TRMM7_13 )
!                FUNC_TRMM7(14) = FUNC_ADDRESS ( TRMM7_14 )
!                FUNC_TRMM7(15) = FUNC_ADDRESS ( TRMM7_15 )
!                FUNC_TRMM7(16) = FUNC_ADDRESS ( TRMM7_16 )
!                FUNC_TRMM7(17) = FUNC_ADDRESS ( TRMM7_17 )
!                FUNC_TRMM7(18) = FUNC_ADDRESS ( TRMM7_18 )
!                FUNC_TRMM7(19) = FUNC_ADDRESS ( TRMM7_19 )
!                FUNC_TRMM7(20) = FUNC_ADDRESS ( TRMM7_20 )
!                FUNC_TRMM7(21) = FUNC_ADDRESS ( TRMM7_21 )
!                FUNC_TRMM7(22) = FUNC_ADDRESS ( TRMM7_22 )
!                FUNC_TRMM7(23) = FUNC_ADDRESS ( TRMM7_23 )
!                FUNC_TRMM7(24) = FUNC_ADDRESS ( TRMM7_24 )
!           END IF
!
           ARG_LIST(1) = 4
           ARG_LIST(2) = LOC(M)
           ARG_LIST(3) = LOC(AP(1))
           ARG_LIST(4) = LOC(B(1,1))
           ARG_LIST(5) = LOC(LDB)
           CALL LIB$CALLG ( ARG_LIST, %VAL(FUNC_TRMM7(N)) )
        ELSE
           NU = N/2
!@           IF ( NU .LT. DB__INVMAT_MIN ) NU = DB__INVMAT_MIN  
           NL = N - NU
           P1 = 1 + INT8(NU)*INT8(NU+1)/2
           P2 = 1 + INT8(N)*INT8(NU) - INT8(NU)*INT8(NU-1)/2
           CALL DRPTRMM7 ( M, NL, ALPHA, AP(P2), B(1,NU+1), LDB )
           CALL DGEMM ( 'N', 'N', M, NL, NU, ALPHA, B(1,1), LDB, &
     &                   AP(P1), NU, ONE, B(1,NU+1), LDB )
           CALL DRPTRMM7 ( M, NU, ALPHA, AP(1), B(1,1), LDB )
      ENDIF
!
      END SUBROUTINE DRPTRMM7
!
! ------------------------------------------------------------------------
!
      RECURSIVE SUBROUTINE DRPTRMM8 ( M, N, ALPHA, AP, B, LDB )
! ************************************************************************
! *                                                                      *
! *   Recursive routine for computing B = alpha * B * AP(T)              *
! *   where AP square symmetric matrix in recursive upper triangular     *
! *   format of dimension N and B is a rectangular matrix M*N            *
! *   dimensioned as LDA*N.                                              *
! *                                                                      *
! *  ### 2000.02.18    B. Andersen   Original code.                      *
! *  ### 2002.11.28    L. Petrov     massive update. Limited recusrions  *
! *                                  by the order  DB__INVMAT_MIN.       *
! *                                                                      *
! ************************************************************************
      IMPLICIT NONE
      INCLUDE 'matvec.i'
!
      INTEGER*4, INTENT(IN)    :: M, N, LDB
      REAL*8,    INTENT(IN)    :: ALPHA
      REAL*8,    INTENT(IN)    :: AP(*)
      REAL*8,    INTENT(INOUT) :: B(LDB,*)
!
      INTEGER*4  :: NU, NL, I
      REAL*8     :: BETA
      REAL*8,       PARAMETER :: ONE=1.0D+0
      ADDRESS__TYPE :: FUNC_ADDRESS, ARG_LIST(5), FUNC_TRMM8(DB__INVMAT_MAX)
      ADDRESS__TYPE, EXTERNAL :: TRMM8_1,  TRMM8_2,  TRMM8_3,  TRMM8_4
!@      EXTERNAL      TRMM8_5,  TRMM8_6,  TRMM8_7,  TRMM8_8,  TRMM8_9,  &
!@     &              TRMM8_10, TRMM8_11, TRMM8_12, TRMM8_13, TRMM8_14, &
!@     &              TRMM8_15, TRMM8_16, TRMM8_17, TRMM8_18, TRMM8_19, &
!@     &              TRMM8_20, TRMM8_21, TRMM8_22, TRMM8_23, TRMM8_24
!
      INTEGER*8  :: P1, P2
      IF ( N == 1 ) THEN
           BETA=ALPHA*AP(1)
           DO I=1,M
              B(I,1)=BETA*B(I,1)
           ENDDO
        ELSE IF ( N .LE. DB__INVMAT_MIN ) THEN
           FUNC_TRMM8(2) = FUNC_ADDRESS ( TRMM8_2 )
           FUNC_TRMM8(3) = FUNC_ADDRESS ( TRMM8_3 )
           FUNC_TRMM8(4) = FUNC_ADDRESS ( TRMM8_4 )
!           IF ( N .GT. 4 ) THEN
!                FUNC_TRMM8(5) = FUNC_ADDRESS ( TRMM8_5 )
!                FUNC_TRMM8(6) = FUNC_ADDRESS ( TRMM8_6 )
!                FUNC_TRMM8(7) = FUNC_ADDRESS ( TRMM8_7 )
!                FUNC_TRMM8(8) = FUNC_ADDRESS ( TRMM8_8 )
!                FUNC_TRMM8(9) = FUNC_ADDRESS ( TRMM8_9 )
!                FUNC_TRMM8(10) = FUNC_ADDRESS ( TRMM8_10 )
!                FUNC_TRMM8(11) = FUNC_ADDRESS ( TRMM8_11 )
!                FUNC_TRMM8(12) = FUNC_ADDRESS ( TRMM8_12 )
!                FUNC_TRMM8(13) = FUNC_ADDRESS ( TRMM8_13 )
!                FUNC_TRMM8(14) = FUNC_ADDRESS ( TRMM8_14 )
!                FUNC_TRMM8(15) = FUNC_ADDRESS ( TRMM8_15 )
!                FUNC_TRMM8(16) = FUNC_ADDRESS ( TRMM8_16 )
!                FUNC_TRMM8(17) = FUNC_ADDRESS ( TRMM8_17 )
!                FUNC_TRMM8(18) = FUNC_ADDRESS ( TRMM8_18 )
!                FUNC_TRMM8(19) = FUNC_ADDRESS ( TRMM8_19 )
!                FUNC_TRMM8(20) = FUNC_ADDRESS ( TRMM8_20 )
!                FUNC_TRMM8(21) = FUNC_ADDRESS ( TRMM8_21 )
!                FUNC_TRMM8(22) = FUNC_ADDRESS ( TRMM8_22 )
!                FUNC_TRMM8(23) = FUNC_ADDRESS ( TRMM8_23 )
!                FUNC_TRMM8(24) = FUNC_ADDRESS ( TRMM8_24 )
!           END IF
!
           ARG_LIST(1) = 4
           ARG_LIST(2) = LOC(M)
           ARG_LIST(3) = LOC(AP(1))
           ARG_LIST(4) = LOC(B(1,1))
           ARG_LIST(5) = LOC(LDB)
           CALL LIB$CALLG ( ARG_LIST, %VAL(FUNC_TRMM8(N)) )
        ELSE
           NU = N/2
!@           IF ( NU .LT. DB__INVMAT_MIN ) NU = DB__INVMAT_MIN  
           NL = N - NU
           P1 = 1 + INT8(NU)*INT8(NU+1)/2
           P2 = 1 + INT8(N)*INT8(NU) - INT8(NU)*INT8(NU-1)/2
           CALL DRPTRMM8 ( M, NU, ALPHA, AP(1), B(1,1), LDB )
           CALL DGEMM ( 'N', 'T', M, NU, NL, ALPHA, B(1,NU+1), LDB, &
                        AP(P1), NU, ONE, B(1,1), LDB )
           CALL DRPTRMM8 ( M, NL, ALPHA, AP(P2), B(1,NU+1), LDB )
      ENDIF
      RETURN
      END SUBROUTINE DRPTRMM8
!
! ------------------------------------------------------------------------
!
      FUNCTION   FUNC_ADDRESS ( IARG )
! ************************************************************************
! *                                                                      *
! *   This auxilliary function  FUNC_ADDRESS  retrins the address of its *
! *   arguiment. The only reason of existance of this function is a bug  *
! *   in HP f90 compiler: compilers claims that getting the address of   *
! *   external function is an unsupported feature. !!!                   *
! *                                                                      *
! *  ### 21-SEP-2002  FUNC_ADDRESS  v1.0 (c)  L. Petrov  21-SEP-2002 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      ADDRESS__TYPE :: FUNC_ADDRESS
      ADDRESS__TYPE IARG
!
      FUNC_ADDRESS  = LOC(IARG)
!
      RETURN
      END  !#!  FUNC_ADDRESS  #!#
