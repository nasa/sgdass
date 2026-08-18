      SUBROUTINE MAKE_SEASONAL ( a,b,t,exp_A)
      implicit none
      REAL*8     a,b
      REAL*8     T
      REAL*8     exp_A(2,2)
! calculate the matrix
!                 |0     1 |
!     Aout= exp t |-a2 -a1 |
!
      REAL*8     omega
      REAL*8     cos_o,sin_o
!
      omega=SQRT(b-a*a/4.d0)
      cos_o=dcos(t*omega)
      sin_o=dsin(t*omega)
      exp_A(1,1)=cos_o+(a/2.d0)*sin_o/omega
      exp_A(2,2)=cos_o-(a/2.d0)*sin_o/omega
      exp_A(1,2)=sin_o/omega
      exp_A(2,1)=-b*sin_o/omega
      exp_A=exp_A*dexp(-a/2.d0)
      RETURN
      END  !#!  MAKE_SEASONAL  #!#
