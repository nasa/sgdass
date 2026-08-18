      SUBROUTINE DRPPTRI(UPLO,M,AP)
      IMPLICIT NONE
!
! --- UNI-C,Lyngby,Denmark. December 6, 1999.
!
      CHARACTER(LEN=1) :: UPLO
      INTEGER, INTENT(IN) :: M
      DOUBLE PRECISION, INTENT(INOUT),    DIMENSION(*) :: AP
!
!     ...Computes the inverse of a recursive packed symmetric,
!        positive definite matrix A, using the upper or lower
!        Cholesky factor computed by PPTRF. The factor is
!        stored in AP.
!
      LOGICAL :: LSAME
!
      IF (LSAME(UPLO,'L')) THEN
!
         CALL DRPTRTRI('L','N',M,AP)
         CALL DRPTRRK('L','T','N',M,AP)
!
      ELSEIF (LSAME(UPLO,'U')) THEN
!
         CALL DRPTRTRI('U','N',M,AP)
         CALL DRPTRRK('U','N','N',M,AP)
!
      ENDIF
!
      END SUBROUTINE DRPPTRI
