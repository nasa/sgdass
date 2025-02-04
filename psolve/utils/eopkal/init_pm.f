!
      SUBROUTINE INIT_PM()
      implicit none
!
! initialize constants used in PM.
      INCLUDE 'eopkal_pm.i'
!
      REAL*8     twopi/6.283185307179959d0/
!
      REAL*8     decay_time_annual/1000.d0/     !Decay time of seasonal term
      REAL*8     period_annual/365.254d0/       !period of annual term
      REAL*8     chandler_period/433.d0/
      REAL*8     chandler_Q/170.d0/
!
      COMPLEX*16 L(2,2),M(2,2),LM(2,2),lambda,det_M
      COMPLEX*16 I/(0,1)/
!
! initialize Various PM coefficients.
      sigma=twopi/chandler_period
      gamma=sigma/(2.*chandler_Q)  !170=quality factor
!
      a_PA= 2.d0/decay_time_annual
      b_PA= (1./decay_time_annual)**2+(twopi/period_annual)**2
!
! make matrix used in calculating seasonal-PM coupling
      lambda=-gamma+sigma*I
      L(1,1)=sigma-I*gamma
      L(2,1)=gamma+i*sigma
      L(1,2)=0.
      L(2,2)=0.
      M(1,1)=lambda+a_PA
      M(2,1)=b_PA
      M(1,2)=-1.d0
      M(2,2)=lambda
      det_M=lambda*lambda+a_PA*lambda+b_PA
      M=M/det_M                               ! M=(lambda-D)^-1
      call cmplx_mat_mul2(LM,L,M)
      LM_real=REAL(LM)
      LM_imag=-real(LM*I)
!
      RETURN
      END  !#!  INIT_PM  #!#
