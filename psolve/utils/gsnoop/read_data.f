      SUBROUTINE read_data (n, ncoldat, idate, ydata, qstat, qhead, mlu, &
     &                      jd1, jd2)
!
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
!     Updated to specificaly type integers which
!-------------------------------------------------
      integer*2      len, trimlen
      integer*2      mlu
      integer*2      i, j, ier
      INTEGER*4      IOS
      integer*2      iy, im, id
!
!      integer*2     luif         !input data file lu
      integer*2     idate(3,*)
      integer*2     n, m         !counters
      integer*2     num_err      !counters
      integer*2     ncoldat      !number (total) of columns of data to be read
!
      real*8        rdata(20)    !data buffer
      real*8        ydata(20,*)
      real*8        xjd, jd1, jd2, fjldy
!
      LOGICAL*2 kdebug
!
      character*8    qstat
      character*80   qhead(*), qstr
      character*255  qbuf
!
!
!      initialize
!
      j = 0
      m = 0
      n = 0
      ios = 0
      kdebug = .false.
      rewind (mlu)
!
!
      do while (ios .eq. 0)
        read (mlu, '(a)', iostat=ios, end=999) qbuf
!
!       Look for Station of interest
        if (qbuf(1:8) .eq. qstat) then
          do while (qbuf(6:10) .ne. " 0  0")
            len = trimlen (qbuf)
            m = m + 1
            if (m .le. 2) then
              qhead(m) = qbuf(1:len)
            else
              read (qbuf,*, iostat=ios) (rdata(i),i=1,ncoldat)
              ier = ios
              if (ier .ne. 0) then
                num_err = num_err + 1
                write(qstr, &
     &           '("error ",i5," reading data record # ",i4)')ier, n+1
                call as2nl(qstr )
              else
!
!
                iy =  dint(rdata(2))
                im =  dint(rdata(3))
                id =  dint(rdata(4))
                xjd = fjldy(im, id, iy)
!
                if ((xjd.ge.jd1).and.(xjd.le.jd2)) then
                  n = n + 1
                  idate(1,n)  =  iy
                  idate(2,n)  =  im
                  idate(3,n)  =  id
!
                  ydata(1,n)  =  rdata(5)   !up
                  ydata(2,n)  =  rdata(6)   !up sigma
!                                           !leave space for later
                  ydata(5,n)  =  rdata(7)   !east
                  ydata(6,n)  =  rdata(8)   !east sigma
!                                           !leave space for later
                  ydata(9,n)  =  rdata(9)   !north
                  ydata(10,n) =  rdata(10)  !north sigma
!                                           !leave space for later
                end if
!
              end if
            end if
            read (mlu, '(a)', iostat=ios, end=999) qbuf
          end do
          ios = 1
        end if
      end do
!
 999  if (n.le.2) then
        qstr = "Insufficient number of points for plotting"
        call asnl(qstr )
        call return_to_continue()
      end if
      return
      end
