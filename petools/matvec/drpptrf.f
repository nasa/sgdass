      SUBROUTINE DRPPTRF(UPLO,N,AP,INFO)
      IMPLICIT NONE
!
! --- UNI-C,Lyngby,Denmark. December 6, 1999.
!
!     ...Scalar arguments
      CHARACTER(LEN=1), INTENT(IN) :: UPLO
      INTEGER, INTENT(IN) :: N
      INTEGER, INTENT(OUT) :: INFO
!
!     ...Array arguments
      DOUBLE PRECISION, DIMENSION(*), INTENT(INOUT) :: AP

!     Purpose
!     =======
!
!     DRPPTRF computes the Cholesky factorization of a real symmetric
!     positive definite matrix A stored in recursive-packed format.
!
!     The factorization has the form
!        A = U**T * U, if UPLO = 'U', or
!        A = L * L**T, if UPLO = 'L',
!     where U is an upper triangular matrix and L is lower triangular.
!
!     Arguments
!     =========
!
!     UPLO    (input) CHARACTER(LEN=1)
!             = 'U': Upper triangle of A is stored.
!             = 'L': Lower triangle of A is stored.
!
!     N       (input) INTEGER
!             The order of the matrix A. N >= 0.
!
!     AP      (input/output) DOUBLE PRECISION array, dimension (N*(N+1)/2)
!             On entry, the upper or lower triangle of the symmetric
!             matrix A, recursively packed in a linear array. The
!             recursive-packed format is explained in [].
!
!             On exit, if INFO=0, the triangular factor U or L from the
!             Cholesky factorization A = U**T*U or A = L*L**T, in
!             recursive-packed storage format as A.
!
!     INFO    (output) INTEGER
!              = 0:  successful exit
!              < 0:  if INFO = -i, the i-th argument had an illegal value.
!              > 0:  if INFO =  i, the leading minor of order i is not
!                    positive definite, and the factorization could not
!                    be completed.
!
!     =====================================================================
!
!     ...Local scalars
      INTEGER :: COUNT
!
!     ...External functions
      LOGICAL :: LSAME
      EXTERNAL LSAME
!
!     ...External subroutines
      EXTERNAL DRPTRSM, DRPSYRK, XERBLA
!
!     ...Intrinsic functions
      INTRINSIC SQRT
!
!     ...Executable statements
!
      INFO=0
      COUNT=0
!
!     ...Test the input parameters.
!
      IF (.NOT.LSAME(UPLO,'U') .AND. .NOT.LSAME(UPLO,'L')) THEN
         INFO = -1
      ELSE IF (N < 0) THEN
         INFO = -2
      END IF
      IF (INFO /= 0) THEN
         CALL XERBLA('DRPPTRF',-INFO)
         RETURN
      END IF
!
!     ...Quick return if possible
!
      IF (N == 0) THEN
         RETURN
      END IF
!
      IF (LSAME(UPLO,'L')) THEN
!
         CALL DRPPTRF1(N,AP,INFO)
!
      ELSE IF (LSAME(UPLO,'U')) THEN
!
         CALL DRPPTRF2(N,AP,INFO)
!
      END IF
!   
      CONTAINS
!
      RECURSIVE SUBROUTINE DRPPTRF1(N,AP,INFO)
      IMPLICIT NONE
!
!     ...Factorize AP = L*L'
!
!        UPLO = 'Lower'
!
      INTEGER, INTENT(IN) :: N
      DOUBLE PRECISION, DIMENSION(*), INTENT(INOUT) :: AP
      INTEGER, INTENT(INOUT) :: INFO
!
      INTEGER :: P
!
      DOUBLE PRECISION, PARAMETER :: ZERO=0.0D+0,ONE=1.0D+0
!
      IF (N == 1) THEN
         IF (AP(1) > ZERO) THEN
            AP(1)=SQRT(AP(1))
            COUNT=COUNT+1
         ELSE
            INFO=COUNT+1
         END IF
      ELSE
         P=N/2
         CALL DRPPTRF1(P,AP(1),INFO)
         IF (INFO == 0) THEN
            CALL DRPTRSM('R','L','T','N',&
                        N-P,P,ONE,AP(1),AP(1+P*(P+1)/2),N-P)
            CALL DRPSYRK('L','N',&
                        N-P,P,-ONE,AP(1+P*(P+1)/2),N-P,ONE,AP(1+N*P-P*(P-1)/2))
            CALL DRPPTRF1(N-P,AP(1+N*P-P*(P-1)/2),INFO)
         END IF
      END IF
!   
      END SUBROUTINE DRPPTRF1
!   
      RECURSIVE SUBROUTINE DRPPTRF2(N,AP,INFO)
      IMPLICIT NONE
!
!     ...Factorize AP = U'*U
!
!        UPLO = 'Upper'
!
      INTEGER, INTENT(IN) :: N
      DOUBLE PRECISION, DIMENSION(*), INTENT(INOUT) :: AP
      INTEGER, INTENT(INOUT) :: INFO
!
      INTEGER :: P
!
      DOUBLE PRECISION, PARAMETER :: ZERO=0.0D+0,ONE=1.0D+0
!
      IF (N == 1) THEN
         IF (AP(1) > ZERO) THEN
            AP(1)=SQRT(AP(1))
            COUNT=COUNT+1
         ELSE
            INFO=COUNT+1
         END IF
      ELSE
         P=N/2
         CALL DRPPTRF2(P,AP(1),INFO)
         IF (INFO == 0) THEN
            CALL DRPTRSM('L','U','T','N',&
                        P,N-P,ONE,AP(1),AP(1+P*(P+1)/2),P)
            CALL DRPSYRK('U','T',&
                        N-P,P,-ONE,AP(1+P*(P+1)/2),P,ONE,AP(1+N*P-P*(P-1)/2))
            CALL DRPPTRF2(N-P,AP(1+N*P-P*(P-1)/2),INFO)
         END IF
      END IF
!   
      END SUBROUTINE DRPPTRF2
!   
      END SUBROUTINE DRPPTRF
