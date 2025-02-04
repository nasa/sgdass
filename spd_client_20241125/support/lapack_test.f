      PROGRAM    LAPACK_TEST
      INTEGER*4  M, N, KL, KU
      PARAMETER  ( M = 4 )
      PARAMETER  ( N = 5 )
      PARAMETER  ( KL = 1 )
      PARAMETER  ( KU = 1 )
      INTEGER*4  IPIV(M,N), INFO
      REAL*4     AB(M*N)
      AB = 1.0
      CALL SGBTRF( M, N, KL, KU, AB, 1, IPIV, INFO )
      END  !#!  
