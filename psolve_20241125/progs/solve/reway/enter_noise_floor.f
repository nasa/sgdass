      SUBROUTINE enter_noise_floor(wt_floor0)
      implicit none
!
      double precision wt_floor0(2)
      double precision wt_floor(2)
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
     & '("Current noise floors are : ",f6.1," ps", f6.1 " fs/s")') &
     & wt_floor0
      call addstr_f(bufstr)
      call nl_mn()
      call  addstr_f &
     &  ("Enter in new noise floors  (0-20,  0-300): ")
!
      call getstr_f(bufstr)
      if (trimlen(bufstr).eq.0) then
!        User wants to leave things alone for this one
         return
      else
         read(bufstr,*,iostat=ierr,err=110) wt_floor
 110     if(ierr.eq.0) then
           if(wt_floor(1)  .ge. 0.0 .and. wt_floor(1) .le. 20 .and. &
     &        wt_floor(2)  .ge. 0.0 .and. wt_floor(2) .le. 300) then
              wt_floor0=wt_floor
              return
            endif
         endif
      endif
!
      call setcr_mn(i4p0,i4p0)
      CALL REFRESH_MN()
      call reverse_on_mn()
      call addstr_f( "Something wrong: two numbers spearated by blank " )
      call nl_mn()
      call addstr_f( "the first:   in the range [0, 20]"  )
      call nl_mn()
      call addstr_f( "the second:  in the range [0, 300]   were expected " )
      call nl_mn()
      call reverse_off_mn()
      CALL REFRESH_MN()
      call lib$wait ( 3.0d0 )
      call clear_mn()
!
! something wrong, try again.
!
      goto 10
      end
