#include <mk5_preprocessor_directives.inc>
      PROGRAM    LIBLAPCK_CHECK
      IMPLICIT   NONE 
      INTEGER*4  N, NB, IPIV, IUER
      PARAMETER  ( N  =  8 )
      PARAMETER  ( NB = 16 )
      REAL*4     MAT_B4(NB)
!
      MAT_B4(1)  =  1.0E0
      MAT_B4(2)  =  2.0E0
      MAT_B4(3)  =  3.0E0
      MAT_B4(4)  =  4.0E0
      MAT_B4(5)  =  5.0E0
      MAT_B4(6)  =  6.0E0
      MAT_B4(7)  =  7.0E0
      MAT_B4(8)  =  8.0E0
      MAT_B4(9)  =  9.0E0
      MAT_B4(10) = 10.0E0
      MAT_B4(11) = 11.0E0
      MAT_B4(12) = 12.0E0
      MAT_B4(13) = 13.0E0
      MAT_B4(14) = 14.0E0
      MAT_B4(15) = 15.0E0
      MAT_B4(16) = 16.0E0
!
      CALL SGBTRF ( N+2, N+2, 1, 1, MAT_B4, 4, IPIV, IUER )
      END  PROGRAM  LIBLAPCK_CHECK  !#!#


