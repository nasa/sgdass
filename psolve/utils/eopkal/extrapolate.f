      SUBROUTINE extrapolate(fjd,x_filt,cov_filt,num_parm,imeas, &
     & time_start,time_end,time_step,ixref,kut1s,klod,kplot,time_next)
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
! Extrapolate backward from beginning of data, and write out results.
!     Updated to specificaly type integers which
!-------------------------------------------------
      INTEGER*2 num_parm
      INTEGER*4 imeas
      REAL*8     fjd(*)                              !
      REAL*8     x_filt(num_parm,*)                  !Filter estimates
      REAL*8     cov_filt(num_parm*(num_parm+1)/2,*)   !And covariance.
      REAL*8     time_start                !Start here
      REAL*8     time_end                  !continue to here, or beyond
      REAL*8     time_step                 !With this time step.
      REAL*8     time_next                 !Next time to do.
      INTEGER*2 ixref(3)                         !Correspondence between EOP and filter
      LOGICAL*2 kut1s                              !Do UT1s smoothing
      LOGICAL*2 klod                               !Write out LOD?
      LOGICAL*2 kplot                              !write out plot data?
!
! internal variables
      REAL*8      x_pred(num_parm)                   !Predicted estimates
      REAL*8      cov_pred(num_parm*(num_parm+1))    !And covariance.
      INTEGER*2 itype                            !Is an experiment near here?
      REAL*8     time                      !do loop variable
      REAL*8     delta_time                !time distance to propagate
!
      do time=time_start,time_end,time_step
        delta_time = time-fjd(imeas)
        IF(ABS(delta_time) .LE. 1.) then
          itype=1
        else
          itype=-1
        endif
          call propagate(x_filt(1,imeas),cov_filt(1,imeas),num_parm, &
     &         delta_time, x_pred,cov_pred )
          call write_out_mod_line(time,x_pred,cov_pred,itype,ixref, &
     &         kut1s,klod,kplot )
      end do
      time_next=time
!
      RETURN
      END  !#!  EXTRAPOLATE  #!#
