      SUBROUTINE EPOCH_FORM95(JUL_DATE,EPOCH_DISP)
!
!     Reformat a full julian date (see input argument list below)
!     into the proper format for the 1995 IERS site submission (see output
!     argument list below).
!
!     Restrictions:  as of 11/98, when y2k fixes are being made,
!               the next IERS format is unknown.   So a best guess will be
!               made that the IERS will retain the 2 digit format for the
!               year and 00 will represent the year 2000.   This at least
!               retains backwards compatibility with the current format.
!               If the IERS adopts a four digit year, this routine and related
!               code must be changed.
!
!     created by kdb   3/27/95
!
!     modifications
!
!     981120 KDB Y2K changes
!     060418 KDB Add conditional compiler directives to comment out solution
!                archiving code under LINUX, where it is not available yet.
!
      IMPLICIT NONE
!
!     input:
!
!     jul_date - full julian date (24xxxxx) which can be at any time of the
!                day
!
      REAL*8 JUL_DATE
!
!     epoch_disp - input julian date reformatted in proper format, which is:
!
!             yr:doy:scspm, where
!
!                 yr = last 2 digits of year
!                 doy = day of year (e.g., 32 for February 1)
!                (and yr and doy together form a date at midnight)
!                 scspm = seconds past midnight
!
      character*12 epoch_disp
!
!     local variables:
!
      real*8 jul_noon, jul_midnight, jul_part
      integer*2 imonth,iday,iyear,idum,idir,idetail,ext_date(3), &
     &          int_date(2),jerr
      character*255 cdum255
      integer*2 iyr_dig2
!
!     I.
!     The output will be an epoch at midnight (expressed as year and
!     day of year)
!     plus the number of seconds past that epoch.
!     In step 1, calculate the year and day of year at midnight.
!
!     First get the julian date at the midnight preceding the input epoch.
!
      jul_noon = dint(jul_date)
      jul_part = jul_date - jul_noon
      if (jul_part.lt.0.5D0) then !afternoon date
        jul_midnight = jul_noon - 0.5D0
      else !morning or midnight itself
        jul_midnight = jul_noon + 0.5D0
      end if
!
!     Then convert it to a year, month of year and day of the month.
!
      call mdyjl(imonth,iday,iyear,idum,jul_midnight)
      iyr_dig2 = iyear
      call newcents(iyr_dig2,iyear)
!
!     We need day of year, so convert the month of year and day of month
!     to that.  (Day of year is returned in int_date(2)).
!
      idir = -1
      idetail = 3
      ext_date(1) = iyear
      ext_date(2) = imonth
      ext_date(3) = iday
#ifdef LINUX
!cout call dt_int_ext(idir,int_date,idetail,ext_date,cdum255,jerr)
#else
!cout call dt_int_ext(idir,int_date,idetail,ext_date,cdum255,jerr)
#endif
!
!     II.
!     In step 2, calculate the seconds past midnight.
!
!     First get the time past midnight expressed as a fraction of a day.
!
      jul_part = jul_date - jul_midnight
!
!     Now convert this to seconds
!
      jul_part = jul_part * 86400.0D0
!
!     III.
!     In the final step, write everything into the output display buffer.
!
      jul_part = dnint(jul_part)
      write(epoch_disp,"(i2.2,':',i3.3,':',i5.5)") &
     &         iyr_dig2, int_date(2), jidint(jul_part)
!
      return
      end
