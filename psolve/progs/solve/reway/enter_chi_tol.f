      SUBROUTINE enter_chi_tol(chi_tol0)
      implicit none
!
!     created    JMG 971028  to enter in chi_tol
!                pet 980121  increased upper limit for chi_tol up to 1.0 and
!                            allowed to enter tolerance 0.0001
      double precision chi_tol,chi_tol0
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
     & '("Current tolerance is: ",f6.4, &
     & " Enter in new tolerance (0.0001 to 1.0): ")') &
     & chi_tol0
      call addstr_f(bufstr)
!      call nl_mn
!      call nl_mn
!
      call getstr_f(bufstr)
      if (trimlen(bufstr).eq.0) then
!        User wants to leave things alone for this one
         return
      else
         read(bufstr,*,iostat=ierr,err=110) chi_tol
 110     if (ierr.eq.0) then
            if ( chi_tol .ge. 0.0001 .and. chi_tol .le. 1.0 ) then
              chi_tol0=chi_tol
              return
            endif
         endif
      endif
! something wrong, try again.
      goto 10
      end
