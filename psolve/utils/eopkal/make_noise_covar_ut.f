      SUBROUTINE MAKE_NOISE_COVAR_UT ( NOISE_COVAR, T )
      implicit none
      INCLUDE 'eopkal_ut.i'
!
      REAL*8     T
      REAL*8     noise_covar(num_ut*(num_ut+1)/2)
!
      REAL*8     cov_seas(4)
      REAL*8     omega_L/3600./       !white noise in microseconds^2/Day^3
!
      REAL*8     omega_A/1./          !Noise parameters for annual
      REAL*8     omega_S/1./          ! and semi-annual
      INTEGER*2 ioff,ioff1,ioff2
      INTEGER*4 indx4
!
      noise_covar=0.
!     noise_covar(indx4(1,1))=omega_L*t**3./3.
!     noise_covar(indx4(1,2))=omega_L*t**2./2.
!     noise_covar(indx4(2,2))=omega_L*t
      noise_covar(1)=omega_L*t**3./3.            !1=indx4(1,1)
      noise_covar(2)=omega_L*t**2./2.            !2=indx4(1,2)
      noise_covar(3)=omega_L*t                   !3=indx4(2,2)
!
      ioff=2
      IF(kut_annual) then
        ioff1=ioff+1
        ioff2=ioff+2
        call make_seasonal_noise_covar(A_ua,b_Ua,t,cov_seas)
        cov_seas=cov_seas*omega_A
        noise_covar(indx4(ioff1,ioff1))=cov_seas(1)
        noise_covar(indx4(ioff1,ioff2))=cov_seas(2)
        noise_covar(indx4(ioff2,ioff2))=cov_seas(3)
        noise_covar(1)=cov_seas(4)/(b_UA**2.)       !1=indx4(1,1)
        ioff=ioff+2
      ENDIF
!
      IF(kut_semi) then
        ioff1=ioff+1
        ioff2=ioff+2
        call make_seasonal_noise_covar(A_us,b_US,t,cov_seas)
        cov_seas=cov_seas*omega_S
        noise_covar(indx4(ioff1,ioff1))=cov_seas(1)
        noise_covar(indx4(ioff1,ioff2))=cov_seas(2)
        noise_covar(indx4(ioff2,ioff2))=cov_seas(3)
        noise_covar(1)=cov_seas(4)/(b_Us**2.)      !1=indx4(1,1)
        ioff=ioff+2
      endif
!
      RETURN
      END  !#!  MAKE_NOISE_COVAR_UT  #!#
