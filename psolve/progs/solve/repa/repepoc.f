      SUBROUTINE REPEPOC ( TIM, IY, IM, ID, IHR, IMIN, SEC )
      IMPLICIT   NONE
!
!     **************************************************************
!     *                                                            *
!     *  REPEPOC IS A SUBROUTINE WHICH WILL CONVERT A JULIAN DATE  *
!     *  INTO integer MONTH, DAY, YEAR, HOUR, MINUTE and SECOND    *
!     *                                                            *
!     **************************************************************
!
!     Written:
!       Brent Archinal     91.02.26  Rewritten.  Fixed round off of
!                                    minutes.
!
!       Gerald Engelhardt  7.6.2000  output of seconds
!                                    output year with 4 digits
!
!       VT                18.6.2002  changed order of parameters
!
!     Subprograms used:
!      cutil:              mdyjl
!
!
      integer*2 im,id,iy,ihr,imin,itime
      real*8 tim,ftim,frac,tmp,sec
!
!   (Add 1/2 minute to tim to get proper round off.)
!     ftim = tim + 0.5D0 / 1440.D0
!
!  (No add 1/2 minute to tim to get proper round off.)
!   Let it be!
      ftim = tim + 0.0D0 / 1440.D0
!
      frac = dmod (ftim - 0.5D0, 1.D0)
!      write(*,100) frac
!100   format(' frac:',f20.12)
      ftim = ftim - frac
!
      call mdyjl (im,id,iy,itime,ftim)
!      write(*,101) itime
!101   format(' itime:',i4)
!
      tmp = FRAC*24.0D0
      ihr = tmp
!
      tmp = (tmp -ihr) * 60.D0
      imin = tmp
!
      tmp = (tmp -imin) * 60.D0
      sec = tmp
!
      iy=iy+((itime+19)*100)
!
      RETURN
      END
