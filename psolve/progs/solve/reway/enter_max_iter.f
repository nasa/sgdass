      SUBROUTINE enter_max_iter(max_iter0)
      implicit none
!
!     created    JMG 971028  to enter in max_iter
      integer*2 max_iter,max_iter0
!
! everything below is used to write out wts.
      character*70 bufstr
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
     & '("Maximum iterations now: ",i6, &
     & " Enter in new maximum (1-20): ")') &
     & max_iter0
      call addstr_f(bufstr)
!      call nl_mn
!      call nl_mn
!
      call getstr_f(bufstr)
      if (trimlen(bufstr).eq.0) then
!        User wants to leave things alone for this one
         return
      else
         read(bufstr,*,iostat=ierr,err=110) max_iter
 110     if (ierr.eq.0) then
            if(max_iter .ge. 1 .and. max_iter .le. 20) then
              max_iter0=max_iter
              return
            endif
         endif
      endif
! something wrong, try again.
      goto 10
      end
