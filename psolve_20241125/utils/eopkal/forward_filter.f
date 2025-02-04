      SUBROUTINE FORWARD_FILTER ( fjd,z_meas,cov_meas,num_eop,num_meas, &
     &    x_filt,cov_filt,num_parm,proj,kmon)
! get filter estimates at each epoch, starting from beginning.
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
      integer*2 num_eop                !num_eop parameters measured
!     Updated to specificaly type integers which
!-------------------------------------------------
      INTEGER*4 num_meas               !number of measurements
      REAL*8     fjd(*)          !epoch
      REAL*8     z_meas(num_eop,*)          !EOP measurements
      REAL*8     cov_meas(num_eop*(num_eop+1)/2,*)   !covariance
      INTEGER*2 num_parm               !# Parameters used to model EOP.
      REAL*8     x_filt(num_parm,*)                   !Parameters
      REAL*8     cov_filt(num_parm*(num_parm+1)/2,*)  !Covariance of parameters
      REAL*8     proj(num_parm,num_eop)               !Z_meas --->x_filt
      LOGICAL*2 kmon                     !monitor progress
!
! internal variables
      INTEGER*4 imeas                  !do loop index
      REAL*8     delta_T
      REAL*8     x_filt_pred(num_parm)               !Best guess filter
      REAL*8     cov_filt_pred(num_parm*(num_parm+1)/2)
!
! make initial guess for filter.
      call init_filter(x_filt_pred,cov_filt_pred,num_parm )
!
!
! now propagate forward until we are done.
      write(*,*) "Doing Forward Filter Pass.  Number to do:",num_meas
      do imeas=1,num_meas
        if(kmon .and. mod(imeas,100) .eq. 0) then
            write(*,*) imeas
        endif
! x_filt_pred,cov_filt_pred are current guesses.
! Add data to get current best estimate.
        call add_new_estimate(z_meas(1,imeas),cov_meas(1,imeas), &
     &       num_eop,x_filt_pred,cov_filt_pred,num_parm,proj, &
     &       x_filt(1,imeas),cov_filt(1,imeas) )
! Propagate current best guess to next epoch, if there is one.
        IF(imeas .LT. num_meas) then
          delta_T=fjd(imeas+1)-fjd(imeas)
          call propagate(x_filt(1,imeas),cov_filt(1,imeas),num_parm, &
     &         delta_T,     x_filt_pred,cov_filt_pred )
        endif
      end do
!
      RETURN
      END  !#!  FORWARD_FILTER  #!#
