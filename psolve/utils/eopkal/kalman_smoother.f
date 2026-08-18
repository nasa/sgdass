      SUBROUTINE kalman_smoother(fjd_meas,x_filt_bck,cov_filt_bck, &
     &  x_filt_for,cov_filt_for,num_parm,num_meas, &
     &  time_start,time_end,time_step, &
     & ixref,kut1s,kmon,klod,kplot,time_next)
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
      INTEGER*2 num_parm
      INTEGER*4 num_meas
      REAL*8     fjd_meas(*)               !time tags
!     Updated to specificaly type integers which
!-------------------------------------------------
      REAL*8     x_filt_bck(num_parm,*)    !Backward filter estimates
      REAL*8     cov_filt_bck(num_parm*(num_parm+1)/2,*)   !And covariance.
      REAL*8     x_filt_for(num_parm,*)    !Forward filter estimates
      REAL*8     cov_filt_for(num_parm*(num_parm+1)/2,*)   !And covariance.
!
      REAL*8     time_start                !Start here
      REAL*8     time_end                  !continue to here, or beyond
      REAL*8     time_step                 !With this time step.
      REAL*8     time_next                 !Next time to do.
      INTEGER*2 ixref(3)                         !Correspondence between EOP and filter
      LOGICAL*2 kut1s                              !Do UT1s smoothing
      LOGICAL*2 klod                               !output lod?
      LOGICAL*2 kplot                              !output plot file?
      LOGICAL*2 kmon                               !show progress?
!
! local variables
      REAL*8     time                      !Do variable
      REAL*8     delta_for                 !time interval in forward
      REAL*8     delta_bck                 !time interval in backward
      ADDRESS__TYPE :: iptr,iptr_old                    !Pointer into filter.
      REAL*8     x_filt_for_pred(num_parm) !Forward extrapolation.
      REAL*8     cov_filt_for_pred(num_parm*(num_parm+1)/2)
      REAL*8     x_filt_bck_pred(num_parm) !Backward extrapolation
      REAL*8     cov_filt_bck_pred(num_parm*(num_parm+1)/2)
      REAL*8     x_out(num_parm)           !Combination
      REAL*8     cov_out(num_parm*(num_parm+1)/2)
      INTEGER*2 itype
!
      write(*,*) "Applying Kalman Smoother/Interpolator"
      iptr=1
      iptr_old=1
!
!
      IF(fjd_meas(1) .GT. time_start) then
         WRITE(*, &
     &      *)"kalman_smoother: Starting time is before start of data!"
         stop
      else IF(time_end .GT. fjd_meas(num_meas)) then
         WRITE(*, &
     &      *)"kalman_smoother: Ending time is after end of data!"
         stop
      endif
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
       WRITE ( 6, * ) ' time_start=',time_start,' time_end=',time_end, &   ! %%%%
     &        ' time_step-',time_step  ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
! at this point fjd(iptr) < time_start < fjd(iptr+1)
      do time=time_start,time_end,time_step
! find right place in Kalman series.
        delta_bck=time-fjd_meas(iptr)
        do while(delta_bck .gt. 0)
           iptr=iptr+1
!           if(iptr .gt. num_meas) goto 100
           delta_bck=time-fjd_meas(iptr)
        end do
! at this point    fjd_meas(iptr-1) <= time <=  fjd_meas(iptr)
        delta_for=time-fjd_meas(iptr-1)
!
!
        if(kmon .and. MOD(iptr,100).eq.0 .and. iptr .ne. iptr_old) then
           write(*,*) iptr
           iptr_old=iptr
        endif
!        WRITE(*,*) time,iptr
        call propagate(x_filt_for(1,iptr-1),cov_filt_for(1,iptr-1), &
     &       num_parm,delta_for,x_filt_for_pred,cov_filt_for_pred )
        call propagate(x_filt_bck(1,iptr),cov_filt_bck(1,iptr), &
     &       num_parm,delta_bck,x_filt_bck_pred,cov_filt_bck_pred )
        call add_using_covar(x_filt_for_pred,cov_filt_for_pred, &
     &                       x_filt_bck_pred,cov_filt_bck_pred, &
     &                       x_out,          cov_out,          num_parm )
!
!
        if(delta_for .lt. 1. .or. -delta_bck .lt. 1.0) then
           itype=1
        else
           itype=-1
        endif
        call write_out_mod_line(time,x_out,cov_out,itype,ixref, &
     &       kut1s,klod,kplot )
      end do
!
!
      time_next=time
!
      RETURN
      END  !#!  KALMAN_SMOOTHER  #!#
