      SUBROUTINE EPOC(IM,ID,IY,IHR,IMIN,TIM)
      implicit none
!
!     **************************************************************
!     *                                                            *
!     *  EPOC IS A SUBROUTINE WHICH WILL CONVERT A JULIAN DATE     *
!     *  INTO integer MONTH, DAY, YEAR, HOUR, AND MINUTE.          *
!     *                                                            *
!     **************************************************************
!
!     Written:
!       Brent Archinal     91.02.26  Rewritten.  Fixed round off of
!                                    minutes.
!
!     Subprograms used:
!      cutil:              mdyjl
!
!
      integer*2 im,id,iy,ihr,imin,itime
      real*8 tim,ftim,frac,tmp
!
!   (Add 1/2 minute to tim to get proper round off.)
      ftim = tim + 0.5D0 / 1440.D0
!
      frac = dmod (ftim - 0.5D0, 1.D0)
      ftim = ftim - frac
!
      call mdyjl (im,id,iy,itime,ftim)
!
      tmp = FRAC*24.0D0
      ihr = tmp
!
      tmp = (tmp -ihr) * 60.D0
      imin = tmp
!
      RETURN
      END
