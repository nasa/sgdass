      SUBROUTINE MAKE_SEASONAL_NOISE_COVAR ( A, B, T, COV )
      implicit none
! calculate covariance of     exp tA |0 0 | exp tA^T
!                                    |0 1 |
      REAL*8     a,b           !Seasonal parameters
      REAL*8     t               !Time
      REAL*8     cov(4)          ! Return covariance.
!
! temporary internal variables
      REAL*8      w              !angular frequency of seasonal term
      REAL*8     sin_2wt,cos_2wt !As they look
      REAL*8     sin_wt,cos_wt
      REAL*8     exp_at,w_sq,a_sq
      w=SQRT(b-a*a/4.d0)
      w_sq=b-a*a/4.
      a_sq=a*a
!
      sin_2wt=SIN(2.*w*t)
      cos_2wt=COS(2.*w*t)
      sin_wt=SIN(w*t)
      cos_wt=COS(w*t)
!
      exp_at=EXP(-a*t)
!
      cov(1)=1./(a*2.*b) &
     &  + (exp_at/(2.*w_sq))*((a*cos_2wt-2.*w*sin_2wt)/(4.*b)-1./(2.*a))
!
      cov(2)=(exp_at/(4.*w_sq))*(1.-cos_2wt)
      cov(3)=1./(2.d0*a) &
     &       + (exp_at/(8.d0*w_sq))*((a*cos_2wt+2.*w*sin_2wt)-(4.*b/a))
!
      cov(4)=t+(4*w_sq-11.*a_sq)/(2.*a*4.*b) &
     &  +(exp_at/(8.*a*w_sq))*(-4.*b+ &
     &     a*(cos_2wt*a*(a_sq-12.*w_sq)+w*sin_2wt*(8.*w_sq-6.*a_sq))/b) &
     &  + exp(-t*a/2.)*(sin_wt*(a_sq-4.*w_sq)+4.*a*w*cos_wt)/(2.*b*w)
!
      RETURN
      END  !#!  MAKE_SEASONAL_NOISE_COVAR  #!#
