      PROGRAM    SPC_TEST_03
      IMPLICIT   NONE 
      REAL*8     NOR_MAT(6), RC
      INTEGER*4  IUER
!
      NOR_MAT(1) = 151.00000000000000D0
      NOR_MAT(2) = -4166.1235235391860D0
      NOR_MAT(3) = 4721189.1149851875D0
      NOR_MAT(4) = 2360594.5574925938D0
      NOR_MAT(5) = -213933958.43142876D0
      NOR_MAT(6) = 71664541292.779541D0
!
      IUER = 0
      CALL INVS ( 3, NOR_MAT, RC, IUER )
      END  !#!  
