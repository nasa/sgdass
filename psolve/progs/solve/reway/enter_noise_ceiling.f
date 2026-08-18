      SUBROUTINE enter_noise_ceiling(wt_ceiling0)
      implicit none
!
      double precision wt_ceiling0(2)
      double precision wt_ceiling(2)
!
! everything below is used to write out wts.
      character*70  bufstr
      integer*4 i4p0, i4p4, i4p6, i4p50
      integer*4 ierr
      data i4p0, i4p4, i4p6, i4p50 /0,4,6,50/
      integer*2 trimlen
!
!
      call setcr_mn(i4p0,i4p0)
      call clear_mn()
!
10    continue
      call setcr_mn(i4p0,i4p0)
      write(bufstr, &
     & '("Current noise ceilings are : ",i6," ps", i6 " fs/s")') &
     & nint(wt_ceiling0)
      call addstr_f(bufstr)
      call nl_mn()
      call  addstr_f &
     &  ("Enter in new noise ceilings  (20-1e5,  100-1e6): ")
!
      call getstr_f(bufstr)
      if (trimlen(bufstr).eq.0) then
!        User wants to leave things alone for this one
         return
      else
         read(bufstr,*,iostat=ierr,err=110) wt_ceiling
 110     if(ierr.eq.0 .and. &
     &     wt_ceiling(1)  .ge. 20.0 .and. wt_ceiling(1) .le. 1.d5 .and. &
     &     wt_ceiling(2)  .ge. 100.0 .and. wt_ceiling(2) .le. 1.d6) then
             wt_ceiling0=wt_ceiling
             return
           endif
         endif
! something wrong, try again.
      goto 10
      end
