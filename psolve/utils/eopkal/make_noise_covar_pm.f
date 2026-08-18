      SUBROUTINE MAKE_NOISE_COVAR_PM ( NOISE_COVAR, T )
      implicit none
!
!--PM common block
      INCLUDE 'eopkal_pm.i'
!
      REAL*8     noise_covar(num_pm*(num_pm+1)/2)
      REAL*8     t
!
      INTEGER*2 ioff,ioff1,ioff2
!--noise parameters.
      REAL*8     Q_S/4.26d-4/              !amplitude seasonal excitation
      REAL*8     Q_mu/246.6/               !amplitude steady state stuff
!
      REAL*8     cov_seas(4)               !compute the seasonal covariance here.
      REAL*8     tmp1
!
      REAL*8     c_st,s_st,gam_sig_sq ! cos(sigma*t) etc.
      REAL*8     e_gt,e_2gt           ! exp(-gamma*t) etc.
!
      INTEGER*4 indx4
!
      INTEGER*2 i1/1/,i2/2/,i3/3/,i4/4/           !integer*2
!
! initialize.
      noise_covar=0.
!
      gam_sig_sq=gamma**2.+sigma**2.
      c_st=COS(sigma*t)
      s_st=SIN(sigma*t)
      e_gt=EXP(-gamma*t)
      E_2gt=EXP(-2.*gamma*t)
!
!
      tmp1=Q_mu*(t+(1.-e_2gt)/(2.*gamma) &
     &    -2.*(gamma+e_gt*(sigma*s_st-gamma*c_st))/gam_sig_sq)
!
      noise_covar(1)=tmp1                        !1=indx4(1,1)
      noise_covar(3)=tmp1                        !3=indx4(2,2)
!
      tmp1=Q_mu*(t+ e_gt*((gamma*c_st-sigma*s_st)-gamma)/gam_sig_sq)
      noise_covar(indx4(i1,i3))=tmp1
      noise_covar(indx4(i2,i4))=-tmp1
!
      tmp1=Q_mu*(sigma-e_gt*(gamma*s_st+sigma*c_st))/gam_sig_sq
      noise_covar(indx4(i1,i4))=tmp1
      noise_covar(indx4(i2,i3))=tmp1
!
      noise_covar(6)=Q_mu*t                      ! 6=indx4(3,3)
      noise_covar(10)=Q_mu*t                     !10=indx4(4,4)
!
! Start on seasonal terms.
      ioff=4
      IF(kpm_annual) then
       ioff1=ioff+1
       ioff2=ioff+2
       call make_seasonal_noise_covar(a_PA,b_PA,t,cov_seas)
       cov_seas=cov_seas*Q_S
       noise_covar(indx4(ioff1,ioff1))=cov_seas(1)
       noise_covar(indx4(ioff1,ioff2))=cov_seas(2)
       noise_covar(indx4(ioff2,ioff2))=cov_seas(3)
      ENDIF
!
      RETURN
      END  !#!  MAKE_NOISE_COVAR_PM  #!#
