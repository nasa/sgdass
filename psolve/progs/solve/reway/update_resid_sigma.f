!     Last change:  JG    6 Mar 96    3:57 pm
      SUBROUTINE update_resid_sigma(idbeg,idend, &
     &  nstat,bl_wts_sq,sig_raw_sq,sig_fact_sq)
      implicit none
      integer*4 idbeg,idend
      integer*4 iobs
      integer*2 nstat
      integer*4 icount
      REAL*8 bl_wts_sq(2,*)
      real*8 sig_raw_sq(2,*),  sig_fact_sq(2,*)
! rewrite the residuals to take account of new formal errors.
! Note: After this, the residuals are the not the residuals to a solution.
! sig_fact_sq -- ratio of post fit sigmas to raw
!
!
! input:
!     nobs: number of observations from resfil
!     bl_wts:  bl_wts  (units of sigma sq)
!     sig_sq:  raw sigma square.
!
!
      INCLUDE 'solve.i'
      INCLUDE 'resfl.i'
!
      integer*2 ibase_index
      integer*2 ibl
!
      icount=0
      call acs_resfil('O')
      do iobs=idbeg,idend
        call use_resfil(iobs,'R')
        if(irunw.eq.0) then
          icount=icount+1
          ibl=ibase_index(irsite,nstat)
          rderr=sqrt(sig_raw_sq(1,icount)+bl_wts_sq(1,ibl))/1d3
          rrerr=sqrt(sig_raw_sq(2,icount)+bl_wts_sq(2,ibl))/1d3
        endif
        call use_resfil(iobs,'W')
      enddo
      call acs_resfil('C')
!
      return
      end
