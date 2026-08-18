      RECURSIVE SUBROUTINE DRPTRRK(UPLO,TRANS,DIAG,M,AP)
      IMPLICIT NONE
!
! --- UNI-C,Lyngby,Denmark. December 6, 1999.
!
      CHARACTER(LEN=1) :: UPLO,TRANS,DIAG
      INTEGER, INTENT(IN) :: M
      DOUBLE PRECISION, INTENT(INOUT),    DIMENSION(*) :: AP
!
!     ...Computes the product of a triangular matrix and its transposed.
!        The triangular matrix AP is in recursive packed storage format.
!
      LOGICAL :: LSAME
!
      IF (LSAME(UPLO,'L')) THEN
         IF (LSAME(TRANS,'N')) THEN
!
            CALL DRPTRRK1(M,AP)
!
         ELSEIF (LSAME(TRANS,'T')) THEN
!
            CALL DRPTRRK2(M,AP)
!
         ENDIF
!
      ELSEIF (LSAME(UPLO,'U')) THEN
         IF (LSAME(TRANS,'N')) THEN
!
            CALL DRPTRRK3(M,AP)
!
         ELSEIF (LSAME(TRANS,'T')) THEN
!
            CALL DRPTRRK4(M,AP)
!
         ENDIF
      ENDIF
!
      CONTAINS
!
      RECURSIVE SUBROUTINE DRPTRRK1(M,AP)
      IMPLICIT NONE
!
      INTEGER, INTENT(IN) :: M
      DOUBLE PRECISION, INTENT(INOUT),    DIMENSION(*) :: AP
!
!     ...Performs AP := AP*AP' , AP is recursive packed lower triangular
!
!        UPLO  = 'Lower'
!        TRANS = 'No transpose'
!        DIAG  = 'Non-unit triangular'
!
      INTEGER :: P
      DOUBLE PRECISION, PARAMETER :: ONE=1.0D0
!
      IF (M == 1) THEN
         AP(1)=AP(1)**2
      ELSE
         P=M/2
         CALL DRPTRRK1(M-P,AP(1+M*P-P*(P-1)/2))
         CALL DRPSYRK('L','N',&
                     M-P,P,ONE,AP(1+P*(P+1)/2),M-P,ONE,AP(1+M*P-P*(P-1)/2))
         CALL DRPTRMM('R','L','T','N',&
                     M-P,P,ONE,AP(1),AP(1+P*(P+1)/2),M-P)
         CALL DRPTRRK1(P,AP(1))
      ENDIF
!
      END SUBROUTINE DRPTRRK1
!
      RECURSIVE SUBROUTINE DRPTRRK2(M,AP)
      IMPLICIT NONE
!
      INTEGER, INTENT(IN) :: M
      DOUBLE PRECISION, INTENT(INOUT),    DIMENSION(*) :: AP
!
!     ...Performs AP := AP'*AP , AP is recursive packed lower triangular
!
!        UPLO  = 'Lower'
!        TRANS = 'Transpose'
!        DIAG  = 'Non-unit triangular'
!
      INTEGER :: P
      DOUBLE PRECISION, PARAMETER :: ONE=1.0D0
!
      IF (M == 1) THEN
         AP(1)=AP(1)**2
      ELSE
         P=M/2
         CALL DRPTRRK2(P,AP(1))
         CALL DRPSYRK('L','T',&
                     P,M-P,ONE,AP(1+P*(P+1)/2),M-P,ONE,AP(1))
         CALL DRPTRMM('L','L','T','N',&
                     M-P,P,ONE,AP(1+M*P-P*(P-1)/2),AP(1+P*(P+1)/2),M-P)
         CALL DRPTRRK2(M-P,AP(1+M*P-P*(P-1)/2))
      ENDIF
!
      END SUBROUTINE DRPTRRK2
!
      RECURSIVE SUBROUTINE DRPTRRK3(M,AP)
      IMPLICIT NONE
!
      INTEGER, INTENT(IN) :: M
      DOUBLE PRECISION, INTENT(INOUT),    DIMENSION(*) :: AP
!
!     ...Performs AP := AP*AP' , AP is recursive packed upper triangular
!
!        UPLO  = 'Upper'
!        TRANS = 'No transpose'
!        DIAG  = 'Non-unit triangular'
!
      INTEGER :: P
      DOUBLE PRECISION, PARAMETER :: ONE=1.0D0
!
      IF (M == 1) THEN
         AP(1)=AP(1)**2
      ELSE
         P=M/2
         CALL DRPTRRK3(P,AP(1))
         CALL DRPSYRK('U','N',&
                     P,M-P,ONE,AP(1+P*(P+1)/2),P,ONE,AP(1))
         CALL DRPTRMM('R','U','T','N',&
                     P,M-P,ONE,AP(1+M*P-P*(P-1)/2),AP(1+P*(P+1)/2),P)
         CALL DRPTRRK3(M-P,AP(1+M*P-P*(P-1)/2))
      ENDIF
!
      END SUBROUTINE DRPTRRK3
!
      RECURSIVE SUBROUTINE DRPTRRK4(M,AP)
      IMPLICIT NONE
!
      INTEGER, INTENT(IN) :: M
      DOUBLE PRECISION, INTENT(INOUT),    DIMENSION(*) :: AP
!
!     ...Performs AP := AP'*AP , AP is recursive packed upper triangular
!
!        UPLO  = 'Upper'
!        TRANS = 'Transpose'
!        DIAG  = 'Non-unit triangular'
!
      INTEGER :: P
      DOUBLE PRECISION, PARAMETER :: ONE=1.0D0
!
      IF (M == 1) THEN
         AP(1)=AP(1)**2
      ELSE
         P=M/2
         CALL DRPTRRK4(M-P,AP(1+M*P-P*(P-1)/2))
         CALL DRPSYRK('U','T',&
                     M-P,P,ONE,AP(1+P*(P+1)/2),P,ONE,AP(1+M*P-P*(P-1)/2))
         CALL DRPTRMM('L','U','T','N',&
                     P,M-P,ONE,AP(1),AP(1+P*(P+1)/2),P)
         CALL DRPTRRK4(P,AP(1))
      ENDIF
!
      END SUBROUTINE DRPTRRK4
!
      END SUBROUTINE DRPTRRK
