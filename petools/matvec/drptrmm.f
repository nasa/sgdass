      SUBROUTINE DRPTRMM(SIDE,UPLO,TRANSA,DIAG,M,N,ALPHA,AP,B,LDB)
      IMPLICIT NONE
!
! --- UNI-C,Lyngby,Denmark. December 6, 1999.
!
!     ...Scalar arguments.
      CHARACTER(LEN=1), INTENT(IN) :: SIDE,UPLO,TRANSA,DIAG
      INTEGER, INTENT(IN) :: M,N,LDB
      DOUBLE PRECISION, INTENT(IN) :: ALPHA
!
!     ...Array arguments.
      DOUBLE PRECISION, INTENT(IN), DIMENSION(*) :: AP
      DOUBLE PRECISION, INTENT(INOUT), DIMENSION(LDB,*) :: B
!   
!     Purpose
!     =======
!   
!     DRPTRMM performs one of the matrix-matrix operations
!   
!        B := alpha*op( A )*B,   or   B := alpha*B*op( A ),
!   
!     where alpha is a scalar, B is an m by n matrix, A is a unit, or
!     non-unit, upper or lower triangular matrix and op( A ) is one of
!   
!        op( A ) = A   or   op( A ) = A'.
!   
!     Parameters
!     ==========
!   
!     SIDE   - CHARACTER(LEN=1).
!              On entry, SIDE specifies whether op( A ) multiplies B from
!              the left or right as follows:
!   
!                 SIDE = 'L' or 'l'   B := alpha*op( A )*B.
!                 SIDE = 'R' or 'r'   B := alpha*B*op( A ).
!   
!              Unchanged on exit.
!   
!     UPLO   - CHARACTER(LEN=1).
!              On entry, UPLO specifies whether the matrix A is an upper or
!              lower triangular matrix as follows:
!   
!                 UPLO = 'U' or 'u'   A is an upper triangular matrix.
!                 UPLO = 'L' or 'l'   A is a lower triangular matrix.
!   
!              Unchanged on exit.
!   
!     TRANSA - CHARACTER(LEN=1).
!              On entry, TRANSA specifies the form of op( A ) to be used in
!              the matrix multiplication as follows:
!   
!                 TRANSA = 'N' or 'n'   op( A ) = A.
!                 TRANSA = 'T' or 't'   op( A ) = A'.
!   
!              Unchanged on exit.
!   
!     DIAG   - CHARACTER(LEN=1).
!              On entry, DIAG specifies whether or not A is unit triangular
!              as follows:
!   
!                 DIAG = 'U' or 'u'   A is assumed to be unit triangular.
!                 DIAG = 'N' or 'n'   A is not assumed to be unit
!                                     triangular.
!   
!              Unchanged on exit.
!   
!     M      - INTEGER.
!              On entry, M specifies the number of rows of B. M must be at
!              least zero.
!              Unchanged on exit.
!   
!     N      - INTEGER.
!              On entry, N specifies the number of columns of B. N must be
!              at least zero.
!              Unchanged on exit.
!   
!     ALPHA  - DOUBLE PRECISION.
!              On entry,  ALPHA specifies the scalar alpha. When alpha is
!              zero then A is not referenced and B need not be set before
!              entry.
!              Unchanged on exit.
!   
!     AP     - DOUBLE PRECISION array of DIMENSION ( k*(k+1)/2 ), where k is
!              M when  SIDE = 'L' or 'l'  and is  N  when  SIDE = 'R' or 'r'.
!              On entry, AP is the triangular matrix A stored in upper
!              recursive-packed storage format for UPLO = 'U' or 'u' or
!              lower recursive-packed format for UPLO = 'L' or 'l'.
!              Unchanged on exit.
!   
!   
!     B      - DOUBLE PRECISION array of DIMENSION ( LDB, N ).
!              Before entry, the leading M by N part of the array B must
!              contain the matrix B, and on exit is overwritten by the
!              transformed matrix.
!   
!     LDB    - INTEGER.
!              On entry, LDB specifies the first dimension of B as declared
!              in the calling (sub) program. LDB must be at least
!              max( 1, M ).
!              Unchanged on exit.
!   
!     ==========================================================================
!
!     ...External functions.
      LOGICAL :: LSAME
      EXTERNAL LSAME
!
!     ...External subroutines.
      EXTERNAL XERBLA, DGEMM
!
!     ...Executable statements.
!
      IF (LSAME(SIDE,'L')) THEN
         IF (LSAME(UPLO,'L')) THEN
            IF (LSAME(TRANSA,'N')) THEN
!
               CALL DRPTRMM1(M,N,ALPHA,AP,B,LDB)
!
            ELSEIF (LSAME(TRANSA,'T')) THEN
!
               CALL DRPTRMM2(M,N,ALPHA,AP,B,LDB)
!
            ENDIF
         ELSEIF (LSAME(UPLO,'U')) THEN
            IF (LSAME(TRANSA,'N')) THEN
!
               CALL DRPTRMM3(M,N,ALPHA,AP,B,LDB)
!
            ELSEIF (LSAME(TRANSA,'T')) THEN
!
               CALL DRPTRMM4(M,N,ALPHA,AP,B,LDB)
!
            ENDIF
         ENDIF
      ELSEIF (LSAME(SIDE,'R')) THEN
         IF (LSAME(UPLO,'L')) THEN
            IF (LSAME(TRANSA,'N')) THEN
!
               CALL DRPTRMM5(M,N,ALPHA,AP,B,LDB)
!
            ELSEIF (LSAME(TRANSA,'T')) THEN
!
               CALL DRPTRMM6(M,N,ALPHA,AP,B,LDB)
!
            ENDIF
         ELSEIF (LSAME(UPLO,'U')) THEN
            IF (LSAME(TRANSA,'N')) THEN
!
               CALL DRPTRMM7(M,N,ALPHA,AP,B,LDB)
!
            ELSEIF (LSAME(TRANSA,'T')) THEN
!
               CALL DRPTRMM8(M,N,ALPHA,AP,B,LDB)
!
            ENDIF
         ENDIF
      ENDIF
!
      CONTAINS
!
      RECURSIVE SUBROUTINE DRPTRMM1(M,N,ALPHA,AP,B,LDB)
      IMPLICIT NONE
!
      INTEGER, INTENT(IN) :: M,N,LDB
      DOUBLE PRECISION, INTENT(IN) :: ALPHA
      DOUBLE PRECISION, INTENT(IN),    DIMENSION(*) :: AP
      DOUBLE PRECISION, INTENT(INOUT), DIMENSION(LDB,*) :: B
!
!     ...Performs B := alpha*AP*B , AP is recursive packed lower triangular.
!
!        SIDE   = 'Left'
!        UPLO   = 'Lower triangular'
!        TRANSA = 'No transpose'
!        DIAG   = 'Non-unit triangular'
!
      INTEGER :: P,J
      DOUBLE PRECISION :: BETA
      DOUBLE PRECISION, PARAMETER :: ONE=1.0D+0
!
      IF (M == 1) THEN
         BETA=ALPHA*AP(1)
         DO J=1,N
            B(1,J)=BETA*B(1,J)
         ENDDO
      ELSE
         P=M/2
         CALL DRPTRMM1(M-P,N,ALPHA,AP(1+M*P-P*(P-1)/2),B(P+1,1),LDB)
         CALL DGEMM('N','N',M-P,N,P,ALPHA,AP(1+P*(P+1)/2),M-P,&
                    B(1,1),LDB, ONE,B(P+1,1),LDB)
         CALL DRPTRMM1(P,N,ALPHA,AP(1),B(1,1),LDB)
      ENDIF
!
      END SUBROUTINE DRPTRMM1
!
      RECURSIVE SUBROUTINE DRPTRMM2(M,N,ALPHA,AP,B,LDB)
      IMPLICIT NONE
!
      INTEGER, INTENT(IN) :: M,N,LDB
      DOUBLE PRECISION, INTENT(IN) :: ALPHA
      DOUBLE PRECISION, INTENT(IN),    DIMENSION(*) :: AP
      DOUBLE PRECISION, INTENT(INOUT), DIMENSION(LDB,*) :: B
!
!     ...performs B := alpha*AP'*B , AP is recursive packed lower triangular.
!
!        SIDE   = 'Left'
!        UPLO   = 'Lower triangular'
!        TRANSA = 'Transpose'
!        DIAG   = 'Non-unit triangular'
!
      INTEGER :: P,J
      DOUBLE PRECISION :: BETA
      DOUBLE PRECISION, PARAMETER :: ONE=1.0D+0
!
      IF (M == 1) THEN
         BETA=ALPHA*AP(1)
         DO J=1,N
            B(1,J)=BETA*B(1,J)
         ENDDO
      ELSE
         P=M/2
         CALL DRPTRMM2(P,N,ALPHA,AP(1),B(1,1),LDB)
         CALL DGEMM('T','N',P,N,M-P,ALPHA,AP(1+P*(P+1)/2),M-P,&
                    B(P+1,1),LDB,ONE,B(1,1),LDB)
         CALL DRPTRMM2(M-P,N,ALPHA,AP(1+M*P-P*(P-1)/2),B(P+1,1),LDB)
      ENDIF
!
      END SUBROUTINE DRPTRMM2
!
      RECURSIVE SUBROUTINE DRPTRMM3(M,N,ALPHA,AP,B,LDB)
      IMPLICIT NONE
!
      INTEGER, INTENT(IN) :: M,N,LDB
      DOUBLE PRECISION, INTENT(IN) :: ALPHA
      DOUBLE PRECISION, INTENT(IN),    DIMENSION(*) :: AP
      DOUBLE PRECISION, INTENT(INOUT), DIMENSION(LDB,*) :: B
!
!     ...Performs B := alpha*AP*B , AP is recursive packed upper triangular.
!
!        SIDE   = 'Left'
!        UPLO   = 'Upper'
!        TRANSA = 'No transpose'
!        DIAG   = 'Non-unit triangular'
!
      INTEGER :: P,J
      DOUBLE PRECISION :: BETA
      DOUBLE PRECISION, PARAMETER :: ONE=1.0D+0
!
      IF (M == 1) THEN
         BETA=ALPHA*AP(1)
         DO J=1,N
            B(1,J)=BETA*B(1,J)
         ENDDO
      ELSE
         P=M/2
         CALL DRPTRMM3(P,N,ALPHA,AP(1),B(1,1),LDB)
         CALL DGEMM('N','N',P,N,M-P,ALPHA,AP(1+P*(P+1)/2),P,&
                    B(P+1,1),LDB,ONE,B(1,1),LDB)
         CALL DRPTRMM3(M-P,N,ALPHA,AP(1+M*P-P*(P-1)/2),B(P+1,1),LDB)
      ENDIF
!
      END SUBROUTINE DRPTRMM3
!
      RECURSIVE SUBROUTINE DRPTRMM4(M,N,ALPHA,AP,B,LDB)
      IMPLICIT NONE
!
      INTEGER, INTENT(IN) :: M,N,LDB
      DOUBLE PRECISION, INTENT(IN) :: ALPHA
      DOUBLE PRECISION, INTENT(IN),    DIMENSION(*) :: AP
      DOUBLE PRECISION, INTENT(INOUT), DIMENSION(LDB,*) :: B
!
!     ...Performs B := alpha*AP'*B , AP is recursive packed upper triangular.
!
!        SIDE   = 'Left'
!        UPLO   = 'Upper triangular'
!        TRANSA = 'Transpose'
!        DIAG   = 'Non-unit triangular'
!
      INTEGER :: P,J
      DOUBLE PRECISION :: BETA
      DOUBLE PRECISION, PARAMETER :: ONE=1.0D+0
!
      IF (M == 1) THEN
         BETA=ALPHA*AP(1)
         DO J=1,N
            B(1,J)=BETA*B(1,J)
         ENDDO
      ELSE
         P=M/2
         CALL DRPTRMM4(M-P,N,ALPHA,AP(1+M*P-P*(P-1)/2),B(P+1,1),LDB)
         CALL DGEMM('T','N',M-P,N,P,ALPHA,AP(1+P*(P+1)/2),P,&
                    B(1,1),LDB,ONE,B(P+1,1),LDB)
         CALL DRPTRMM4(P,N,ALPHA,AP(1),B(1,1),LDB)
      ENDIF
!
      END SUBROUTINE DRPTRMM4
!
      RECURSIVE SUBROUTINE DRPTRMM5(M,N,ALPHA,AP,B,LDB)
      IMPLICIT NONE
!
      INTEGER, INTENT(IN) :: M,N,LDB
      DOUBLE PRECISION, INTENT(IN) :: ALPHA
      DOUBLE PRECISION, INTENT(IN),    DIMENSION(*) :: AP
      DOUBLE PRECISION, INTENT(INOUT), DIMENSION(LDB,*) :: B
!
!     ...Performs B := alpha*B*AP , AP is recursive packed lower triangular.
!
!        SIDE   = 'Right'
!        UPLO   = 'Lower triangular'
!        TRANSA = 'No transpose'
!        DIAG   = 'Non-unit triangular'
!
      INTEGER :: P,I
      DOUBLE PRECISION :: BETA
      DOUBLE PRECISION, PARAMETER :: ONE=1.0D+0
!
      IF (N == 1) THEN
         BETA=ALPHA*AP(1)
         DO I=1,M
            B(I,1)=BETA*B(I,1)
         ENDDO
      ELSE
         P=N/2
         CALL DRPTRMM5(M,P,ALPHA,AP(1),B(1,1),LDB)
         CALL DGEMM('N','N',M,P,N-P,ALPHA,B(1,P+1),LDB,&
                    AP(1+P*(P+1)/2),N-P,ONE,B(1,1),LDB)
         CALL DRPTRMM5(M,N-P,ALPHA,AP(1+N*P-P*(P-1)/2),B(1,P+1),LDB)
      ENDIF
!
      END SUBROUTINE DRPTRMM5
!
      RECURSIVE SUBROUTINE DRPTRMM6(M,N,ALPHA,AP,B,LDB)
      IMPLICIT NONE
!
      INTEGER, INTENT(IN) :: M,N,LDB
      DOUBLE PRECISION, INTENT(IN) :: ALPHA
      DOUBLE PRECISION, INTENT(IN),    DIMENSION(*) :: AP
      DOUBLE PRECISION, INTENT(INOUT), DIMENSION(LDB,*) :: B
!
!     ...Performs B := alpha*B*AP' , AP is recursive packed lower triangular.
!
!        SIDE   = 'Right'
!        UPLO   = 'Lower triangular'
!        TRANSA = 'Transpose'
!        DIAG   = 'Non-unit triangular'
!
      INTEGER :: P,I
      DOUBLE PRECISION :: BETA
      DOUBLE PRECISION, PARAMETER :: ONE=1.0D+0
!
      IF (N == 1) THEN
         BETA=ALPHA*AP(1)
         DO I=1,M
            B(I,1)=BETA*B(I,1)
         ENDDO
      ELSE
         P=N/2
         CALL DRPTRMM6(M,N-P,ALPHA,AP(1+N*P-P*(P-1)/2),B(1,P+1),LDB)
         CALL DGEMM('N','T',M,N-P,P,ALPHA,B(1,1),LDB,&
                    AP(1+P*(P+1)/2),N-P,ONE,B(1,P+1),LDB)
         CALL DRPTRMM6(M,P,ALPHA,AP(1),B(1,1),LDB)
      ENDIF
!
      END SUBROUTINE DRPTRMM6
!
      RECURSIVE SUBROUTINE DRPTRMM7(M,N,ALPHA,AP,B,LDB)
      IMPLICIT NONE
!
      INTEGER, INTENT(IN) :: M,N,LDB
      DOUBLE PRECISION, INTENT(IN) :: ALPHA
      DOUBLE PRECISION, INTENT(IN),    DIMENSION(*) :: AP
      DOUBLE PRECISION, INTENT(INOUT), DIMENSION(LDB,*) :: B
!
!     ...Performs B := alpha*B*AP , AP is recursive packed upper triangular.
!
!        SIDE   = 'Right'
!        UPLO   = 'Upper triangular'
!        TRANSA = 'No transpose'
!        DIAG   = 'Non-unit triangular'
!
      INTEGER :: P,I
      DOUBLE PRECISION :: BETA
      DOUBLE PRECISION, PARAMETER :: ONE=1.0D+0
!
      IF (N == 1) THEN
         BETA=ALPHA*AP(1)
         DO I=1,M
            B(I,1)=BETA*B(I,1)
         ENDDO
      ELSE
         P=N/2
         CALL DRPTRMM7(M,N-P,ALPHA,AP(1+N*P-P*(P-1)/2),B(1,P+1),LDB)
         CALL DGEMM('N','N',M,N-P,P,ALPHA,B(1,1),LDB,&
                    AP(1+P*(P+1)/2),P,ONE,B(1,P+1),LDB)
         CALL DRPTRMM7(M,P,ALPHA,AP(1),B(1,1),LDB)
      ENDIF
!
      END SUBROUTINE DRPTRMM7
!
      RECURSIVE SUBROUTINE DRPTRMM8(M,N,ALPHA,AP,B,LDB)
      IMPLICIT NONE
!
      INTEGER, INTENT(IN) :: M,N,LDB
      DOUBLE PRECISION, INTENT(IN) :: ALPHA
      DOUBLE PRECISION, INTENT(IN),    DIMENSION(*) :: AP
      DOUBLE PRECISION, INTENT(INOUT), DIMENSION(LDB,*) :: B
!
!     ...Performs B := alpha*B*AP' , AP is recursive packed upper triangular.
!
!        SIDE   = 'Right'
!        UPLO   = 'Upper triangular'
!        TRANSA = 'Transpose'
!        DIAG   = 'Non-unit triangular'
!
      INTEGER :: P,I
      DOUBLE PRECISION :: BETA
      DOUBLE PRECISION, PARAMETER :: ONE=1.0D+0
!
      IF (N == 1) THEN
         BETA=ALPHA*AP(1)
         DO I=1,M
            B(I,1)=BETA*B(I,1)
         ENDDO
      ELSE
         P=N/2
         CALL DRPTRMM8(M,P,ALPHA,AP(1),B(1,1),LDB)
         CALL DGEMM('N','T',M,P,N-P,ALPHA,B(1,P+1),LDB,&
                    AP(1+P*(P+1)/2),P,ONE,B(1,1),LDB)
         CALL DRPTRMM8(M,N-P,ALPHA,AP(1+N*P-P*(P-1)/2),B(1,P+1),LDB)
      ENDIF
!
      END SUBROUTINE DRPTRMM8
!
      END SUBROUTINE DRPTRMM
