      RECURSIVE SUBROUTINE DRPTRTRI(UPLO,DIAG,M,AP)
      IMPLICIT NONE
!
! --- UNI-C,Lyngby,Denmark. December 6, 1999.
!
      CHARACTER(LEN=1) :: UPLO,DIAG
      INTEGER, INTENT(IN) :: M
      DOUBLE PRECISION, INTENT(INOUT),    DIMENSION(*) :: AP
!
!     ...Computes the inverse of a recursive packed triangular
!        matrix AP.
!
      LOGICAL :: LSAME
!
      IF (LSAME(UPLO,'L')) THEN
!
            CALL DRPTRTRI1(M,AP)
!
      ELSEIF (LSAME(UPLO,'U')) THEN
!
            CALL DRPTRTRI2(M,AP)
!
      ENDIF
!
      CONTAINS
!
      RECURSIVE SUBROUTINE DRPTRTRI1(M,AP)
      IMPLICIT NONE
!
      INTEGER, INTENT(IN) :: M
      DOUBLE PRECISION, INTENT(INOUT),    DIMENSION(*) :: AP
!
!     ...Computes the inverse of a recursive packed lower triangular
!        matrix AP.
!
!        UPLO   = 'Lower triangular'
!        DIAG   = 'Non-unit triangular'
!
      INTEGER :: P
      DOUBLE PRECISION :: BETA
      DOUBLE PRECISION, PARAMETER :: ONE=1.0D+0
!
      IF (M == 1) THEN
         AP(1)=ONE/AP(1)
      ELSE
         P=M/2
         CALL DRPTRTRI1(P,AP(1))
         CALL DRPTRMM('R','L','N','N',&
                     M-P,P,-ONE,AP(1),AP(1+P*(P+1)/2),M-P)
         CALL DRPTRSM('L','L','N','N',&
                     M-P,P,ONE,AP(1+M*P-P*(P-1)/2),AP(1+P*(P+1)/2),M-P)
         CALL DRPTRTRI1(M-P,AP(1+M*P-P*(P-1)/2))
      ENDIF
!
      END SUBROUTINE DRPTRTRI1
!
      RECURSIVE SUBROUTINE DRPTRTRI2(M,AP)
      IMPLICIT NONE
!
      INTEGER, INTENT(IN) :: M
      DOUBLE PRECISION, INTENT(INOUT),    DIMENSION(*) :: AP
!
!     ...Computes the inverse of a recursive packed upper triangular
!        matrix AP.
!
!        UPLO   = 'Upper triangular'
!        DIAG   = 'Non-unit triangular'
!
      INTEGER :: P
      DOUBLE PRECISION :: BETA
      DOUBLE PRECISION, PARAMETER :: ONE=1.0D+0
!
      IF (M == 1) THEN
         AP(1)=ONE/AP(1)
      ELSE
         P=M/2
         CALL DRPTRTRI2(M-P,AP(1+M*P-P*(P-1)/2))
         CALL DRPTRMM('R','U','N','N',&
                     P,M-P,-ONE,AP(1+M*P-P*(P-1)/2),AP(1+P*(P+1)/2),P)
         CALL DRPTRSM('L','U','N','N',&
                     P,M-P,ONE,AP(1),AP(1+P*(P+1)/2),P)
         CALL DRPTRTRI2(P,AP(1))
      ENDIF
!
      END SUBROUTINE DRPTRTRI2
!
      END SUBROUTINE DRPTRTRI
