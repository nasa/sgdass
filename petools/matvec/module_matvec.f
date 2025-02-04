      MODULE MATVEC
      INTERFACE
!
      FUNCTION DP_VV_V ( N, VEC1, VEC2 )
         IMPLICIT   NONE 
         INTEGER*4  N 
         REAL*8     DP_VV_V
         REAL*8     VEC1(N), VEC2(N)
      END FUNCTION  DP_VV_V
!
      END INTERFACE
      END MODULE MATVEC
