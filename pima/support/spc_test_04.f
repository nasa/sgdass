      PROGRAM    SPC_TEST_04
      IMPLICIT   NONE 
      REAL*8     MAT1(2,2), MAT2(2,2), MATO(2,2)
      INTEGER*4  IUER
      MAT1 = 1.0D0
      MAT2 = 2.0D0
      CALL DGEMM ( 'N', 'N', 2, 2, 2, 1.D0, MAT1, 2, MAT2, 2, 0.D0, &
     &              MATO, 2 )
      END  !#!  
