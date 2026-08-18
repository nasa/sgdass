      PROGRAM    LIBBLAS_CHECK
      IMPLICIT   NONE 
      INTEGER*4  N, M
      PARAMETER  ( N = 2 )
      PARAMETER  ( M = 2 )
      REAL*8     VEC(N)
      REAL*4     MAT_B4(8*M)
      INTEGER*4  IPIV(8*M), J1, IER
!
      VEC(1) = 1.0D0
      VEC(2) = 2.0D0
      CALL DDOT ( N, VEC, 1, VEC, 1 )
      DO 410 J1=1,8*M
         MAT_B4(J1) = 1.0
         IPIV(J1) = 1
 410  CONTINUE 
      CALL SGBTRF ( M+2, M+2, 1, 1, MAT_B4, 4, IPIV, IER )
!
      CALL XERBLA( 'LIBBLAS_CHECK', 0 )
      END  !#!  LIBBLAS_CHECK  #!#


