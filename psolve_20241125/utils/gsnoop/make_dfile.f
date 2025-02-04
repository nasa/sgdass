      SUBROUTINE make_dfile(ncol, ndata, idate, ydata, qhead, dlu)
!
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!     Updated to specificaly type integers which
!-------------------------------------------------
!
      integer*2      dlu
      integer*2      i, ier, ncol, ndata
!
      integer*2     jdw          !
      parameter     (jdw = 6)
      integer*2     luif         !input data file lu
      integer*2     idate(3,*)
      integer*2     n            !counters
!
!      real*8        rdata(ncol)    !data buffer
      real*8        ydata(ncol,*)
!
      LOGICAL*2 kdebug
!
      character*8    qstat
      character*80    qhead(*)
!
!
!     initialize
      kdebug = .false.
!
!
      do n = 1, 2
        write (dlu, "(a))" ) qhead(n)
      end do
      do n = 1, ndata
        write (dlu, "(i3,x,3(i2,x),12(x,f6.1))") &
     &         n, (idate(i,n),i=1,3), (ydata(i,n),i=1,12)
      end do
!
  999 close(dlu)
      return
      end
