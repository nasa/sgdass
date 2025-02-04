      SUBROUTINE DRPSYRK(UPLO,TRANS,M,N,ALPHA,A,LDA,BETA,CP)
      IMPLICIT NONE
!
! --- UNI-C,Lyngby,Denmark. December 6, 1999.
!
!     ...Scalar Arguments
      CHARACTER(LEN=1), INTENT(IN) :: UPLO,TRANS
      INTEGER, INTENT(IN) :: M,N,LDA
      DOUBLE PRECISION, INTENT(IN) :: ALPHA,BETA
!
!     ...Array arguments
      DOUBLE PRECISION, INTENT(IN), DIMENSION(LDA,*) :: A
      DOUBLE PRECISION, INTENT(INOUT), DIMENSION(*) :: CP
!
!     Purpose
!     =======
!   
!     DRPSYRK performs one of the symmetric rank k operations
!   
!        C := alpha*A*A' + beta*C,
!   
!     or
!   
!        C := alpha*A'*A + beta*C,
!   
!     where alpha and beta are scalars, C is a symmetric matrix stored in
!     upper or lower recursive-packed storage format and A is an N by K 
!     matrix in the first case and a K by N matrix in the second case.
!   
!     Parameters
!     ==========
!   
!     UPLO   - CHARACTER(LEN=1).
!              On  entry, UPLO specifies whether the upper or lower
!              triangular part of the array C is to be referenced as
!              follows:
!   
!                 UPLO = 'U' or 'u'   Only the upper triangular part of C
!                                     is to be referenced.
!                 UPLO = 'L' or 'l'   Only the lower triangular part of C
!                                     is to be referenced.
!   
!              Unchanged on exit.
!   
!     TRANS  - CHARACTER(LEN=1).
!              On entry, TRANS specifies the operation to be performed as
!              follows:
!   
!                 TRANS = 'N' or 'n'   C := alpha*A*A' + beta*C.
!                 TRANS = 'T' or 't'   C := alpha*A'*A + beta*C.
!   
!              Unchanged on exit.
!   
!     M      - INTEGER.
!              On entry, M specifies the order of the matrix C. M must be
!              at least zero.
!              Unchanged on exit.
!   
!     N      - INTEGER.
!              On entry with TRANS = 'N' or 'n', N specifies the number
!              of  columns  of  the  matrix  A; and on entry with
!              TRANS = 'T' or 't' or 'C' or 'c', N specifies the number
!              of rows of the matrix A. N must be at least zero.
!              Unchanged on exit.
!   
!     ALPHA  - DOUBLE PRECISION.
!              On entry, ALPHA specifies the scalar alpha.
!              Unchanged on exit.
!   
!     A      - DOUBLE PRECISION array of DIMENSION ( LDA, k ), where k is
!              N when TRANS = 'N' or 'n', and is M otherwise.
!              Before entry with TRANS = 'N' or 'n', the leading M by N
!              part of the array A must contain the matrix A,otherwise
!              the leading N by M part of the array A must contain the
!              matrix A.
!              Unchanged on exit.
!   
!     LDA    - INTEGER.
!              On entry, LDA specifies the first dimension of A as declared
!              in the calling (sub) program. When TRANS = 'N' or 'n'
!              then LDA must be at least max( 1, M ), otherwise LDA must
!              be at least max( 1, N ).
!              Unchanged on exit.
!   
!     BETA   - DOUBLE PRECISION.
!              On entry, BETA specifies the scalar beta.
!              Unchanged on exit.
!   
!     CP     - DOUBLE PRECISION array of DIMENSION ( M*(M+1)/2 ).
!   
!              For UPLO='U' or 'u':
!              On entry, CP is the upper triangular matrix C stored in
!              recursive-packed format. Before exit CP i updated with the
!              upper triangle of C.
!   
!              For UPLO='L' or 'l':
!              On entry, CP is the lower triangular matrix C stored in
!              recursive-packed format. Before exit CP i updated with the
!              lower triangle of C.
!
!     ======================================================================
!   
!     ...External functions
      LOGICAL :: LSAME
      EXTERNAL LSAME
!   
!     ...External subroutines
      EXTERNAL XERBLA, DGEMM
!   
!     ...Executable statements
!
      IF (LSAME(UPLO,'L')) THEN
         IF (LSAME(TRANS,'N')) THEN
!
            CALL DRPSYRK1(M,N,ALPHA,A,LDA,BETA,CP)
!
         ELSE IF (LSAME(TRANS,'T')) THEN
!
            CALL DRPSYRK2(M,N,ALPHA,A,LDA,BETA,CP)
!
         END IF
      ELSE IF (LSAME(UPLO,'U')) THEN
         IF (LSAME(TRANS,'N')) THEN
!
            CALL DRPSYRK3(M,N,ALPHA,A,LDA,BETA,CP)
!
         ELSE IF (LSAME(TRANS,'T')) THEN
!
            CALL DRPSYRK4(M,N,ALPHA,A,LDA,BETA,CP)
!
         END IF
      END IF
!
      CONTAINS
!
      RECURSIVE SUBROUTINE DRPSYRK1(M,N,ALPHA,A,LDA,BETA,CP)
      IMPLICIT NONE
!
      INTEGER, INTENT(IN) :: M,N,LDA
      DOUBLE PRECISION, INTENT(IN) :: ALPHA,BETA
      DOUBLE PRECISION, INTENT(IN), DIMENSION(LDA,*) :: A
      DOUBLE PRECISION, INTENT(INOUT), DIMENSION(*) :: CP
!
!     ...Calculates CP=alpha*A*A'+beta*CP , CP is recursive-packed
!        lower triangular.
!
!        UPLO  = 'Lower'
!        TRANS = 'No transpose'
!
      INTEGER :: P,J
!
      IF (M == 1) THEN
         CP(1)=BETA*CP(1)
         DO J=1,N
            CP(1)=CP(1)+ALPHA*A(1,J)**2
         END DO
      ELSE
         P=M/2
         CALL DRPSYRK1(P,N,ALPHA,A(1,1),LDA,BETA,CP(1))
         CALL DGEMM('N','T',M-P,P,N,ALPHA,A(P+1,1),LDA,A(1,1),LDA,&
                    BETA,CP(1+P*(P+1)/2),M-P)
         CALL DRPSYRK1(M-P,N,ALPHA,A(P+1,1),LDA,BETA,CP(1+M*P-P*(P-1)/2))
      END IF
!
      END SUBROUTINE DRPSYRK1
!
      RECURSIVE SUBROUTINE DRPSYRK2(M,N,ALPHA,A,LDA,BETA,CP)
      IMPLICIT NONE
!
      INTEGER, INTENT(IN) :: M,N,LDA
      DOUBLE PRECISION, INTENT(IN) :: ALPHA,BETA
      DOUBLE PRECISION, INTENT(IN), DIMENSION(LDA,*) :: A
      DOUBLE PRECISION, INTENT(INOUT), DIMENSION(*) :: CP
!
!     ...Calculates CP=alpha*A'*A+beta*CP , CP is recursive-packed
!        lower triangular.
!
!        UPLO  = 'Lower'
!        TRANS = 'Transpose'
!
      INTEGER :: P,I
!
      IF (M == 1) THEN
         CP(1)=BETA*CP(1)
         DO I=1,N
            CP(1)=CP(1)+ALPHA*A(I,1)**2
         END DO
      ELSE
         P=M/2
         CALL DRPSYRK2(P,N,ALPHA,A(1,1),LDA,BETA,CP(1))
         CALL DGEMM('T','N',M-P,P,N,ALPHA,A(1,P+1),LDA,A(1,1),LDA,&
                    BETA,CP(1+P*(P+1)/2),M-P)
         CALL DRPSYRK2(M-P,N,ALPHA,A(1,P+1),LDA,BETA,CP(1+M*P-P*(P-1)/2))
      END IF
!
      END SUBROUTINE DRPSYRK2
!
      RECURSIVE SUBROUTINE DRPSYRK3(M,N,ALPHA,A,LDA,BETA,CP)
      IMPLICIT NONE
!
      INTEGER, INTENT(IN) :: M,N,LDA
      DOUBLE PRECISION, INTENT(IN) :: ALPHA,BETA
      DOUBLE PRECISION, INTENT(IN), DIMENSION(LDA,*) :: A
      DOUBLE PRECISION, INTENT(INOUT), DIMENSION(*) :: CP
!
!     ...Calculates CP=alpha*A*A'+beta*CP , CP is recursive-packed
!        upper triangular.
!
!        UPLO  = 'Upper'
!        TRANS = 'No transpose'
!
      INTEGER :: P,J
!
      IF (M == 1) THEN
         CP(1)=BETA*CP(1)
         DO J=1,N
            CP(1)=CP(1)+ALPHA*A(1,J)**2
         END DO
      ELSE
         P=M/2
         CALL DRPSYRK3(P,N,ALPHA,A(1,1),LDA,BETA,CP(1))
         CALL DGEMM('N','T',P,M-P,N,ALPHA,A(1,1),LDA,A(P+1,1),LDA,&
                    BETA,CP(1+P*(P+1)/2),P)
         CALL DRPSYRK3(M-P,N,ALPHA,A(P+1,1),LDA,BETA,CP(1+M*P-P*(P-1)/2))
      END IF
!
      END SUBROUTINE DRPSYRK3
!
      RECURSIVE SUBROUTINE DRPSYRK4(M,N,ALPHA,A,LDA,BETA,CP)
      IMPLICIT NONE
!
!     ...Calculates CP=alpha*A'*A+beta*CP , CP is recursive-packed
!        upper triangular.
!
!        UPLO  = 'Upper'
!        TRANS = 'Transpose'
!
      INTEGER, INTENT(IN) :: M,N,LDA
      DOUBLE PRECISION, INTENT(IN) :: ALPHA,BETA
      DOUBLE PRECISION, INTENT(IN), DIMENSION(LDA,*) :: A
      DOUBLE PRECISION, INTENT(INOUT), DIMENSION(*) :: CP
!
      INTEGER :: P,I
!
      IF (M == 1) THEN
         CP(1)=BETA*CP(1)
         DO I=1,N
            CP(1)=CP(1)+ALPHA*A(I,1)**2
         END DO
      ELSE
         P=M/2
         CALL DRPSYRK4(P,N,ALPHA,A(1,1),LDA,BETA,CP(1))
         CALL DGEMM('T','N',P,M-P,N,ALPHA,A(1,1),LDA,A(1,P+1),LDA,&
                    BETA,CP(1+P*(P+1)/2),P)
         CALL DRPSYRK4(M-P,N,ALPHA,A(1,P+1),LDA,BETA,CP(1+M*P-P*(P-1)/2))
      END IF
!
      END SUBROUTINE DRPSYRK4
!
      END SUBROUTINE DRPSYRK
