      SUBROUTINE UTPMINT(T,tbinfo,tbdata,tbout)
      IMPLICIT NONE
!
!     Perform a quadratic interpolation for UT1PM mapping. Copied from
!      program CALC.
!
      REAL*8 T,tt,tbinfo(3),tbdata(6),tbout,y1(2),y2(2),xint(4), &
     &       s,f2
      INTEGER*4 ilast,int,n,nn,nr
!
!     T         - time of observation
!     tbinfo(3) - info on tabular data points: 1) J.D. of first point,
!                 2) interval in days, 3) number of points
!     tbdata(6) - tabular data (X-wobble, Y-wobble, or TAI-UT1)
!     tbout     - value of the desired quantity at time T interpolated
!                 from the data in tbdata.
!
!     HISTORY
!   who   when     what
!   DG    910701   Created for new UT1PM mapping scheme
!
!     Convert time to the units of the interval of the table relative
!        to the first point of the table
!
        tt = (T - tbinfo(1))/tbinfo(2)
        int = IDINT(tt)
        tt = tt - dfloat(int)
        int = int - 1
!
!      Select the 4 tabular points. Verify that the interpolation is not
!        outside the range of the table.
!
        ilast = IDINT(tbinfo(3))
        do n=1,4
        nn = int + n
!***    if((nn.lt.1) .or. (nn.gt.ilast)) go to 1000
        xint(n) = tbdata(nn)
        end do
!
!      Interpolate in the table
!
        do n=1,2
         nr = n+1
         f2 = (xint(nr+1) + xint(nr-1))/6.D0
         y1(n) =  (4.D0/3.D0)*xint(nr) - f2
         y2(n) = -(1.D0/3.D0)*xint(nr) + f2
        end do
!
        s = 1.D0 - tt
        tbout = ((tt * (y1(2) + tt**2 * y2(2)) + s * (y1(1) + &
     &           s**2 * y2(1))))
!
      RETURN
!
 1000   continue
!***    error condition
        return
!
        end
