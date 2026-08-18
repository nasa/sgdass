      SUBROUTINE DREORDER(UPLO,TRANS,TO,M,AP,INFO)
      IMPLICIT NONE
!
! --- UNI-C,Lyngby,Denmark. December 6, 1999.
!
!     ...Scalar arguments
      CHARACTER(LEN=1), INTENT(IN) :: UPLO,TRANS,TO
      INTEGER, INTENT(IN) :: M
      INTEGER, INTENT(OUT) :: INFO
!
!     ...Array arguments
      DOUBLE PRECISION, DIMENSION(*), INTENT(INOUT) :: AP
!
!     Purpose
!     =======
!
!     DREORDER Reorders AP between packed and recursive-packed format.
!
!     Arguments
!     =========
!
!     UPLO   (input) CHARACTER(LEN=1)
!            ='U': Upper triangle of A is stored in AP
!            ='L': Lower triangle of A is stored in AP
!
!     TRANS  (input) CHARACTER(LEN=1)
!            ='T': A is transposed while AP is reordered
!            ='N': A is not transposed while AP is reordered
!
!     TO     (input) CHARACTER(LEN=1)
!            ='R': AP is reordered from packed TO recursive-packed format.
!            ='P': AP is reordered from recursive-packed TO packed format.
!
!     M      (input) INTEGER
!            The order of matrix A.
!
!     AP     (input/output) DOUBLE PRECISION, dimension (M*(M+1)/2)
!            TO = 'R': input : A in packed storage.
!                      output: A in recursive-packed storage.
!            TO = 'P': input : A in recursive-packed storage.
!                      output: A in packed storage.
!
!     INFO   (output) INTEGER
!            = 0: successful exit.
!            < 0: if INFO = -i, the i-th argument had an illegal value.
!
!     =====================================================================
!
!     ...Local arrays
      DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE :: BUF
!
!     ...External functions
      LOGICAL :: LSAME
      EXTERNAL LSAME
!
!     ...External subroutines
      EXTERNAL XERBLA
!
!     ...Executable statements
!
      INFO=0
!
!     ...Test the input parameters
!
      IF (.NOT.LSAME(UPLO,'L') .AND. .NOT.LSAME(UPLO,'U')) THEN
         INFO=-1
      ELSE IF (.NOT.LSAME(TRANS,'N') .AND. .NOT.LSAME(TRANS,'T')) THEN
         INFO=-2
      ELSE IF (.NOT.LSAME(TO,'R') .AND. .NOT.LSAME(TO,'P')) THEN
         INFO=-3
      ELSE IF (M < 0) THEN
         INFO=-4
      END IF
      IF (INFO /= 0) THEN
         CALL XERBLA('DREORDER',-INFO)
         RETURN
      END IF
!
!     ...Quick return if possible
!
      IF (M == 0) THEN
         RETURN
      END IF
!
      ALLOCATE(BUF((M-M/2)*(M-M/2+1)/2),STAT=INFO)
      IF ( INFO .NE. 0) THEN
           write ( 6, * ) ' mem_elem: ', (m-m/2)*(m-m/2+1)/2  ! %%%%%%%
           PRINT *, 'DREORDER: ALLOCATE ERROR, STAT= ',INFO
           CALL EXIT ( 1 )
      END IF
!
      IF (LSAME(UPLO,'L')) THEN
         IF (LSAME(TRANS,'N')) THEN
            IF (LSAME(TO,'R')) THEN
!
               CALL DREORDER1(M,AP,BUF)
!
            ELSE IF (LSAME(TO,'P')) THEN
!
               CALL DREORDER2(M,AP,BUF)
!
            END IF
         ELSE IF (LSAME(TRANS,'T')) THEN
            IF (LSAME(TO,'R')) THEN
!
               CALL DREORDER3(M,AP,BUF)
!
            ELSE IF (LSAME(TO,'P')) THEN
!
               CALL DREORDER4(M,AP,BUF)
!
            END IF
         END IF
      ELSE IF (LSAME(UPLO,'U')) THEN
         IF (LSAME(TRANS,'N')) THEN
            IF (LSAME(TO,'R')) THEN
!
               CALL DREORDER5(M,AP,BUF)
!
            ELSE IF (LSAME(TO,'P')) THEN
!
               CALL DREORDER6(M,AP,BUF)
!
            END IF
         ELSE IF (LSAME(TRANS,'T')) THEN
            IF (LSAME(TO,'R')) THEN
!
               CALL DREORDER7(M,AP,BUF)
!
            ELSE IF (LSAME(TO,'P')) THEN
!
               CALL DREORDER8(M,AP,BUF)
!
            END IF
         END IF
      END IF
!
      DEALLOCATE(BUF,STAT=INFO)
      IF ( INFO = 0) THEN
           PRINT *, 'DREORDER: DEALLOCATE ERROR, STAT= ',INFO
           CALL EXIT ( 1 )
      END IF
!
      CONTAINS
!
! ------------------------------------------------------------------------
!
      RECURSIVE SUBROUTINE DREORDER1(M,AP,BUF)
      IMPLICIT NONE
!
      INTEGER, INTENT(IN) :: M
      DOUBLE PRECISION, DIMENSION(*), INTENT(INOUT) :: AP
      DOUBLE PRECISION, DIMENSION(*), INTENT(INOUT) :: BUF
!
!     ...Reorders AP from lower packed to lower recursive-packed format.
!
!        UPLO  = 'Lower'
!        TRANS = 'No transpose'
!        TO    = 'Recursive-packed'
!
      INTEGER :: P
!
      IF (M > 1) THEN
         P=M/2
         CALL LPRP(M,P,AP(1),BUF)
         CALL DREORDER1(P,AP(1),BUF)
         CALL DREORDER1(M-P,AP(1+M*P-P*(P-1)/2),BUF)
      END IF
!
      END SUBROUTINE DREORDER1
!
! ------------------------------------------------------------------------
!
      RECURSIVE SUBROUTINE DREORDER2(M,AP,BUF)
      IMPLICIT NONE
!
      INTEGER, INTENT(IN) :: M
      DOUBLE PRECISION, DIMENSION(*), INTENT(INOUT) :: AP
      DOUBLE PRECISION, DIMENSION(*), INTENT(INOUT) :: BUF
!
!     ...Reorders AP from lower recursive-packed to lower packed format.
!
!        UPLO  = 'Lower'
!        TRANS = 'No transpose'
!        TO    = 'Packed'
!
      INTEGER :: P
!
      IF (M > 1) THEN
         P=M/2
         CALL DREORDER2(P,AP(1),BUF)
         CALL LRPP(M,P,AP(1),BUF)
         CALL DREORDER2(M-P,AP(1+M*P-P*(P-1)/2),BUF)
      END IF
!
      END SUBROUTINE DREORDER2
!
! ------------------------------------------------------------------------
!
      RECURSIVE SUBROUTINE DREORDER3(M,AP,BUF)
      IMPLICIT NONE
!
      INTEGER, INTENT(IN) :: M
      DOUBLE PRECISION, DIMENSION(*), INTENT(INOUT) :: AP
      DOUBLE PRECISION, DIMENSION(*), INTENT(INOUT) :: BUF
!
!     ...Reorders AP from lower packed to lower recursive-packed
!        transposed format = upper recursive-packed format.
!
!        UPLO  = 'Lower'
!        TRANS = 'Transposed'
!        TO    = 'Recursive-packed'
!
      INTEGER :: P
!
      IF (M > 1) THEN
         P=M/2
         CALL LPRPT(M,P,AP(1),BUF)
         CALL DREORDER3(P,AP(1),BUF)
         CALL DREORDER3(M-P,AP(1+M*P-P*(P-1)/2),BUF)
      END IF
!
      END SUBROUTINE DREORDER3
!
! ------------------------------------------------------------------------
!
      RECURSIVE SUBROUTINE DREORDER4(M,AP,BUF)
      IMPLICIT NONE
!
      INTEGER, INTENT(IN) :: M
      DOUBLE PRECISION, DIMENSION(*), INTENT(INOUT) :: AP
      DOUBLE PRECISION, DIMENSION(*), INTENT(INOUT) :: BUF
!
!     ...Reorders AP from lower recursive-packed transposed =
!        upper recursive-packed format to lower packed format.
!
!        UPLO  = 'Lower'
!        TRANS = 'Transposed'
!        TO    = 'Packed'
!
      INTEGER :: P
!
      IF (M > 1) THEN
         P=M/2
         CALL DREORDER4(P,AP(1),BUF)
         CALL LRPTP(M,P,AP(1),BUF)
         CALL DREORDER4(M-P,AP(1+M*P-P*(P-1)/2),BUF)
      END IF
!
      END SUBROUTINE DREORDER4
!
! ------------------------------------------------------------------------
!
      RECURSIVE SUBROUTINE DREORDER5(M,AP,BUF)
      IMPLICIT NONE
!
      INTEGER, INTENT(IN) :: M
      DOUBLE PRECISION, DIMENSION(*), INTENT(INOUT) :: AP
      DOUBLE PRECISION, DIMENSION(*), INTENT(INOUT) :: BUF
!
!     ...Reorders AP from upper packed to upper recursive-packed format
!
!        UPLO  = 'Upper'
!        TRANS = 'No transpose'
!        TO    = 'Recursive-packed'
!
      INTEGER :: P
!
      IF (M > 1) THEN
         P=M/2
         CALL DREORDER5(P,AP(1),BUF)
         CALL UPRP(M,P,AP(1+P*(P+1)/2),BUF)
         CALL DREORDER5(M-P,AP(1+M*P-P*(P-1)/2),BUF)
      END IF
!
      END SUBROUTINE DREORDER5
!
! ------------------------------------------------------------------------
!
      RECURSIVE SUBROUTINE DREORDER6(M,AP,BUF)
      IMPLICIT NONE
!
      INTEGER, INTENT(IN) :: M
      DOUBLE PRECISION, DIMENSION(*), INTENT(INOUT) :: AP
      DOUBLE PRECISION, DIMENSION(*), INTENT(INOUT) :: BUF
!
!     ...Reorders AP from upper recursive-packed to upper packed format
!
!        UPLO  = 'Upper'
!        TRANS = 'No transpose'
!        TO    = 'Packed'
!
      INTEGER :: P
!
      IF (M > 1) THEN
         P=M/2
         CALL DREORDER6(P,AP(1),BUF)
         CALL DREORDER6(M-P,AP(1+M*P-P*(P-1)/2),BUF)
         CALL URPP(M,P,AP(1+P*(P+1)/2),BUF)
      END IF
!
      END SUBROUTINE DREORDER6
!
! ------------------------------------------------------------------------
!
      RECURSIVE SUBROUTINE DREORDER7(M,AP,BUF)
      IMPLICIT NONE
!
      INTEGER, INTENT(IN) :: M
      DOUBLE PRECISION, DIMENSION(*), INTENT(INOUT) :: AP
      DOUBLE PRECISION, DIMENSION(*), INTENT(INOUT) :: BUF
!
!     ...Reorders AP from upper packed to upper recursive-packed
!        transposed format = lower recursive-packed format.
!
!        UPLO  = 'Upper'
!        TRANS = 'Transposed'
!        TO    = 'Recursive-packed'
!
      INTEGER :: P
!
      IF (M > 1) THEN
         P=M/2
         CALL DREORDER7(P,AP(1),BUF)
         CALL UPRPT(M,P,AP(1+P*(P+1)/2),BUF)
         CALL DREORDER7(M-P,AP(1+M*P-P*(P-1)/2),BUF)
      END IF
!
      END SUBROUTINE DREORDER7
!
! ------------------------------------------------------------------------
!
      RECURSIVE SUBROUTINE DREORDER8(M,AP,BUF)
      IMPLICIT NONE
!
      INTEGER, INTENT(IN) :: M
      DOUBLE PRECISION, DIMENSION(*), INTENT(INOUT) :: AP
      DOUBLE PRECISION, DIMENSION(*), INTENT(INOUT) :: BUF
!
!     ...Reorders AP from upper recursive-packed transposed format =
!        lower recursive-packed format to upper packed format.
!
!        UPLO  = 'Upper'
!        TRANS = 'Transposed'
!        TO    = 'Packed'
!
      INTEGER :: P
!
      IF (M > 1) THEN
         P=M/2
         CALL DREORDER8(P,AP(1),BUF)
         CALL DREORDER8(M-P,AP(1+M*P-P*(P-1)/2),BUF)
         CALL URPTP(M,P,AP(1+P*(P+1)/2),BUF)
      END IF
!
      END SUBROUTINE DREORDER8
!
! ------------------------------------------------------------------------
!
      SUBROUTINE LPRP(M,N,AP,BUF)
      IMPLICIT NONE
!
!     ...Reorders lower trapez AP from packed to recursive-packed format.
!
      INTEGER, INTENT(IN) :: M,N
      DOUBLE PRECISION, DIMENSION(*), INTENT(INOUT) :: AP
      DOUBLE PRECISION, DIMENSION(*), INTENT(INOUT) :: BUF
!
      INTEGER :: I,J
!
      DO J=1,N
         DO I=J,N
            BUF(I+(2*N-J)*(J-1)/2)=AP(I+(2*M-J)*(J-1)/2)
         END DO
      END DO
      DO J=N,1,-1
         DO I=M,N+1,-1
            AP(I+(M-N)*(J-1)+N*(N-1)/2)=AP(I+(2*M-J)*(J-1)/2)
         END DO
      END DO
      DO J=1,N*(N+1)/2
         AP(J)=BUF(J)
      END DO
!
      END SUBROUTINE LPRP
!
! ------------------------------------------------------------------------
!
      SUBROUTINE LRPP(M,N,AP,BUF)
      IMPLICIT NONE
!
!     ...Reorders lower trapez AP from recursive-packed to packed format.
!
      INTEGER, INTENT(IN) :: M,N
      DOUBLE PRECISION, DIMENSION(*), INTENT(INOUT) :: AP
      DOUBLE PRECISION, DIMENSION(*), INTENT(INOUT) :: BUF
!
      INTEGER :: I,J
!
      DO J=1,N*(N+1)/2
         BUF(J)=AP(J)
      END DO
      DO J=1,N
         DO I=N+1,M
            AP(I+(2*M-J)*(J-1)/2)=AP(I+(M-N)*(J-1)+N*(N-1)/2)
         END DO
      END DO
      DO J=1,N
         DO I=J,N
            AP(I+(2*M-J)*(J-1)/2)=BUF(I+(2*N-J)*(J-1)/2)
         END DO
      END DO
!
      END SUBROUTINE LRPP
!
! ------------------------------------------------------------------------
!
      SUBROUTINE UPRP(M,N,AP,BUF)
      IMPLICIT NONE
!
!     ...Reorders upper trapez AP from packed to recursive-packed format
!
      INTEGER, INTENT(IN) :: M,N
      DOUBLE PRECISION, DIMENSION(*), INTENT(INOUT) :: AP
      DOUBLE PRECISION, DIMENSION(*), INTENT(INOUT) :: BUF
!
      INTEGER :: I,J
!
      DO J=1,M-N
         DO I=1,J
            BUF(I+J*(J-1)/2)=AP(I+N*J+J*(J-1)/2)
         END DO
      END DO
      DO J=1,M-N
         DO I=1,N
            AP(I+N*(J-1))=AP(I+(2*N+J)*(J-1)/2)
         END DO
      END DO
      DO J=1,(M-N)*(M-N+1)/2
         AP(N*(M-N)+J)=BUF(J)
      END DO
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
      INTEGER, INTENT(IN) :: M,N
      DOUBLE PRECISION, DIMENSION(*), INTENT(INOUT) :: AP
      DOUBLE PRECISION, DIMENSION(*), INTENT(INOUT) :: BUF
!
      INTEGER :: I,J
!
      DO J=1,(M-N)*(M-N+1)/2
         BUF(J)=AP(N*(M-N)+J)
      END DO
      DO J=M-N,1,-1
         DO I=N,1,-1
            AP(I+(2*N+J)*(J-1)/2)=AP(I+N*(J-1))
         END DO
      END DO
      DO J=1,M-N
         DO I=1,J
            AP(I+N*J+J*(J-1)/2)=BUF(I+J*(J-1)/2)
         END DO
      END DO
!
      END SUBROUTINE URPP
!
! ------------------------------------------------------------------------
!
      SUBROUTINE LPRPT(M,N,AP,BUF)
      IMPLICIT NONE
!
!     ...Reorders lower trapez AP from packed to recursive-packed format.
!        Transpose the rectangular part.
!
      INTEGER, INTENT(IN) :: M,N
      DOUBLE PRECISION, DIMENSION(*), INTENT(INOUT) :: AP
      DOUBLE PRECISION, DIMENSION(*), INTENT(INOUT) :: BUF
!
      INTEGER :: I,J
!
      DO J=1,N
         DO I=J,N
            BUF(I+(2*N-J)*(J-1)/2)=AP(I+(2*M-J)*(J-1)/2)
         END DO
      END DO
      DO J=N,1,-1
         DO I=M,N+1,-1
            AP(I+(M-N)*(J-1)+N*(N-1)/2)=AP(I+(2*M-J)*(J-1)/2)
         END DO
      END DO
      DO J=1,N*(N+1)/2
         AP(J)=BUF(J)
      END DO
      CALL TRM1(M-N,N,AP(1+N*(N+1)/2),BUF)
!
      END SUBROUTINE LPRPT
!
! ------------------------------------------------------------------------
!
      SUBROUTINE LRPTP(M,N,AP,BUF)
      IMPLICIT NONE
!
!     ...Transpose the rectangular part.
!        Reorders lower trapez AP from recursive-packed to packed format.
!
      INTEGER, INTENT(IN) :: M,N
      DOUBLE PRECISION, DIMENSION(*), INTENT(INOUT) :: AP
      DOUBLE PRECISION, DIMENSION(*), INTENT(INOUT) :: BUF
!
      INTEGER :: I,J
!
      CALL TRM2(N,M-N,AP(1+N*(N+1)/2),BUF)
      DO J=1,N*(N+1)/2
         BUF(J)=AP(J)
      END DO
      DO J=1,N
         DO I=N+1,M
            AP(I+(2*M-J)*(J-1)/2)=AP(I+(M-N)*(J-1)+N*(N-1)/2)
         END DO
      END DO
      DO J=1,N
         DO I=J,N
            AP(I+(2*M-J)*(J-1)/2)=BUF(I+(2*N-J)*(J-1)/2)
         END DO
      END DO
!
      END SUBROUTINE LRPTP
!
! ------------------------------------------------------------------------
!
      SUBROUTINE UPRPT(M,N,AP,BUF)
      IMPLICIT NONE
!
!     ...Reorders upper trapez AP from packed to recursive-packed format
!        Transpose the rectangular part.
!
      INTEGER, INTENT(IN) :: M,N
      DOUBLE PRECISION, DIMENSION(*), INTENT(INOUT) :: AP
      DOUBLE PRECISION, DIMENSION(*), INTENT(INOUT) :: BUF
!
      INTEGER :: I,J
!
      DO J=1,M-N
         DO I=1,J
            BUF(I+J*(J-1)/2)=AP(I+N*J+J*(J-1)/2)
         END DO
      END DO
      DO J=1,M-N
         DO I=1,N
            AP(I+N*(J-1))=AP(I+(2*N+J)*(J-1)/2)
         END DO
      END DO
      DO J=1,(M-N)*(M-N+1)/2
         AP(N*(M-N)+J)=BUF(J)
      END DO
      CALL TRM2(N,M-N,AP,BUF)
!
      END SUBROUTINE UPRPT
!
! ------------------------------------------------------------------------
!
      SUBROUTINE URPTP(M,N,AP,BUF)
      IMPLICIT NONE
!
!     ...Transpose the rectangular part.
!        Reorders upper trapez AP from recursive-packed to packed format
!
      INTEGER, INTENT(IN) :: M,N
      DOUBLE PRECISION, DIMENSION(*), INTENT(INOUT) :: AP
      DOUBLE PRECISION, DIMENSION(*), INTENT(INOUT) :: BUF
!
      INTEGER :: I,J
!
      CALL TRM1(M-N,N,AP,BUF)
      DO J=1,(M-N)*(M-N+1)/2
         BUF(J)=AP(N*(M-N)+J)
      END DO
      DO J=M-N,1,-1
         DO I=N,1,-1
            AP(I+(2*N+J)*(J-1)/2)=AP(I+N*(J-1))
         END DO
      END DO
      DO J=1,M-N
         DO I=1,J
            AP(I+N*J+J*(J-1)/2)=BUF(I+J*(J-1)/2)
         END DO
      END DO
!
      END SUBROUTINE URPTP
!
! ------------------------------------------------------------------------
!
      SUBROUTINE TRM1(M,N,A,BUF)
      IMPLICIT NONE
!
      INTEGER, INTENT(IN) :: M,N
      DOUBLE PRECISION, DIMENSION(M*N), INTENT(INOUT) :: A
      DOUBLE PRECISION, DIMENSION(N), INTENT(INOUT) :: BUF
!
!     ...Transpose a matrix A with number of colums N equal to or
!        one less than the number of rows M.
!
      INTEGER :: I,J
!
      IF (M == N) THEN
         CALL TRM(M,A)
      ELSE IF (M == N+1) THEN
         DO J=1,N
            BUF(J)=A(M*J)
         END DO
         DO J=1,N
            DO I=1,N
               A(I+N*(J-1))=A(I+M*(J-1))
            END DO
         END DO
         CALL TRM(N,A)
         DO J=1,N
            A(N*N+J)=BUF(J)
         END DO
      ELSE
         WRITE ( 6, * ) 'Trap of internal control in trm1 of DREORDER'
         CALL EXIT ( 1 )
      END IF
      END SUBROUTINE TRM1
!
! ------------------------------------------------------------------------
!
      SUBROUTINE TRM2(M,N,A,BUF)
      IMPLICIT NONE
!
      INTEGER, INTENT(IN) :: M,N
      DOUBLE PRECISION, DIMENSION(M*N), INTENT(INOUT) :: A
      DOUBLE PRECISION, DIMENSION(N), INTENT(INOUT) :: BUF
!
!     ...Transpose a matrix A with number of columns N equal to or
!        one greater than the number of rows M.
!
      INTEGER :: I,J
!
      IF (M == N) THEN
         CALL TRM(M,A)
      ELSE IF (M == N-1) THEN
         DO J=1,M
            BUF(J)=A(M*M+J)
         END DO
         CALL TRM(M,A)
         DO J=M,1,-1
            DO I=M,1,-1
               A(I+N*(J-1))=A(I+M*(J-1))
            END DO
         END DO
         DO J=1,M
            A(N*J)=BUF(J)
         END DO
      ELSE
         STOP 2
      END IF
!
! ------------------------------------------------------------------------
!
      END SUBROUTINE TRM2
!
      SUBROUTINE TRM(M,A)
      IMPLICIT NONE
!
      INTEGER, INTENT(IN) :: M
      DOUBLE PRECISION, DIMENSION(M,M), INTENT(INOUT) :: A
!
!     ...Transpose a matrix A with equal number M of rows and columns.
!
      INTEGER :: I,J
      DOUBLE PRECISION :: TMP
!
      DO J=1,M
         DO I=J,M
            TMP=A(I,J)
            A(I,J)=A(J,I)
            A(J,I)=TMP
         END DO
      END DO
!
! ------------------------------------------------------------------------
!
      END SUBROUTINE TRM
!
      END SUBROUTINE DREORDER
