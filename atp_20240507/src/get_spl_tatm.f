      SUBROUTINE GET_SPL_TATM ( ME, LE, ELEV, TATM, SPL_TATM )
! ************************************************************************
! *                                                                      *
! *   Routine GET_SPL_TATM
! *                                                                      *
! *  ### 06-NOV-2021  GET_SPL_TATM  v1.0 (c) L. Petrov  06-NOV-2021 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INTEGER*4  ME, LE
      INTEGER*4  IUER
      INTEGER*4  MED
      PARAMETER  ( MED = 16 )
      REAL*8     ELEV(ME), TATM(ME), SPL_TATM(ME), WORK(MED)
!
      ELEV(1)=   2.999993;  TATM(1)=  23.52000
      ELEV(2)=   3.307868;  TATM(2)=  21.98000
      ELEV(3)=   3.653808;  TATM(3)=  20.44000
      ELEV(4)=   4.046338;  TATM(4)=  18.93000
      ELEV(5)=   4.496807;  TATM(5)=  17.42000
      ELEV(6)=   5.020680;  TATM(6)=  15.93000
      ELEV(7)=   5.639587;  TATM(7)=  14.45000
      ELEV(8)=   6.384756;  TATM(8)=  12.99000
      ELEV(9)=   7.303013;  TATM(9)=  11.53000
      ELEV(10)=  8.467969;  TATM(10)= 10.09   
      ELEV(11)=  10.00   ;  TATM(11)= 8.660000
      ELEV(12)=  12.12883;  TATM(12)= 7.230000
      ELEV(13)=  15.29589;  TATM(13)= 5.810000
      ELEV(14)=  20.57781;  TATM(14)= 4.390000
      ELEV(15)=  31.44779;  TATM(15)= 2.980000
      ELEV(16)=  90.001  ;  TATM(16)= 1.560000
!
      CALL MAKE_SPLINE ( 1, MED, ELEV, TATM, 0.0D0, 0.0D0, SPL_TATM,    &
     &                   WORK, IUER )
      LE = MED
!
      RETURN
      END  SUBROUTINE  GET_SPL_TATM  !#!
