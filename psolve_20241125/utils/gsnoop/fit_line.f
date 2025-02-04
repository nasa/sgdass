!'''/''''1''''/''''2''''/''''3''''/''''4''''/''''5''''/''''6''''/''''7''''/''''8
      SUBROUTINE fit_line (ncol, ndata, idate, ydata, rate, sigr, lrchi, &
     &                     wrms)
!
!     Subroutine to
!
      IMPLICIT NONE
!
      integer*2       i, j, k, k1, k2
      integer*2       ncol, ndata
      integer*2       idate(3,*)
      integer*2       ierr
!
      real*8          fjldy
      real*8          x0, x(1000), y(1000), sigmy(1000), ydata(ncol,*)
      real*8          rate(3), rt, b(3), b1, r
      real*8          sigb(3), sb, sigr(3), sgr, added_sigmy
      real*8          weight, sumw, chi, lrchi(3), wrms(3)
!
!     Convert from Gregorian date to MJD - MJD of first observation
!
      x0 = -1.0d0
      added_sigmy = 0
      x0 = fjldy(idate(2,1), idate(3,1), idate(1,1))
!
      do i = 1, ndata
        x(i) = fjldy(idate(2,i), idate(3,i), idate(1,i)) -x0
      end do
!
!     get rate for u, e, and n
!
      do j = 1,3
        k = 4*j - 3
        k1 = k + 1
        do i = 1,ndata
          y(i)     = ydata(k,i)
          sigmy(i) = ydata(k1,i)
        end do
        call linft (X,Y,SIGMY,ndata,rt,sgr,b1,sb,r,ADDED_SIGMY)
        rate(j) = rt
        sigr(j) = sgr
        b(j)    = b1
        sigb(j) = sb
      end do
!
!     Now calculate a point on line of best fit for each date in data
!     and calculate the o-c for each date, for u, e, and n
!
      do j = 1,3
        sumw  = 0.0d0
        chi   = 0.0d0
        k2 = 4*j            !observed - calculated
        k1 = k2 - 1         !calculated
        k  = k1 - 2         !observed
        do i = 1,ndata
          ydata(k1,i)  = x(i)*rate(j) + b(j)         !c
          ydata(k2,i) = ydata(k,i) -  ydata(k1,i)    !o-c
          weight = 1.0d0/(sigmy(i)**2)
          sumw   = sumw + weight
          chi    = chi + weight*(ydata(k2,i)**2)
        end do
        lrchi(j)  = chi/(ndata - 2)    !  reduced chi square
        wrms(j)  = dsqrt(chi/sumw)    !  fit to line
      end do
!
!
      return
      end
