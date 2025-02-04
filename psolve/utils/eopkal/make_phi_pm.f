      SUBROUTINE MAKE_PHI_PM ( PHI, T )
      implicit none
      INCLUDE 'eopkal_pm.i'
!
      REAL*8     phi(num_pm,num_pm)    !Polar motion transition matrix
      REAL*8      t            !time
! phi is a 6x6 matrix which can most easily thought to be composed of 2x2 matrices:
!
!           |A   B  C |    |exp_tA  [(exp_tA-1)/A]B      G |
! phi=exp t |0   0  0 | =  |0              I             0 |
!           |0   0  D |    |0              0        exp_tD |
!
! For more details on this, see documentation.
! The hardest matrix to construct is G(t).
!
! For the explicit form of A,B,C, see the documentation.
!
      REAL*8     exp_A(2,2), exp_D(2,2),G(2,2)
      REAL*8     cos_st,sin_st    !cos(sigma*t),  sin(sigma*t)
      INTEGER*2 ioff                    !offset
!
! Default is the identity.
      phi=0.
! make the diagonal sub-matrices
!
      cos_st = dcos(t*sigma)
      sin_st = dsin(t*sigma)
      exp_A(1,1)=cos_st
      exp_A(2,2)=cos_st                !             | -gamma  sigma |
      exp_A(1,2)=sin_st                ! exp_A =exp t|               |
      exp_A(2,1)=-sin_st               !             | -sigma -gamma |
      exp_A=exp_A*dexp(-gamma*t)
!
      phi(1:2,1:2)=EXP_A(1:2,1:2)
      phi(3,3)=1.
      phi(4,4)=1.
!     phi(1:2,3:4)=  (exp_A-1) A^-1 B    and  A^-1 B = diag(-1,1)
      phi(1,3)= -(exp_A(1,1)-1.)
      phi(2,3)= - exp_A(2,1)
      phi(1,4)= + exp_A(1,2)
      phi(2,4)= +(exp_A(2,2)-1.)
!
      ioff=4
      IF(kpm_annual) then
! this is seasonal part
         call make_seasonal(a_PA,b_PA,t,exp_D)
         phi(ioff+1:ioff+2,ioff+1:ioff+2)=exp_D(1:2,1:2)
! Now compute G(t).  To understand what is happening, see the documentation.
!     G=LM_real*NR
         call mat_mul2(G,LM_real,exp_D)
         G=-G+cos_st*LM_real-LM_imag*sin_st
         phi(1:2,ioff+1:ioff+2)=G
         ioff=ioff+2
      endif
!
      RETURN
      END  !#!  MAKE_PHI_PM  #!#
