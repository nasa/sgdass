      SUBROUTINE decents(iyear_dig4,iyear_dig2)
      implicit none
!
      INCLUDE "param.i"
!
!     given the full year, return an integer representing last two digits.
!        e.g. 1999 converts to 99 and 2000 to  0
!         input values outside the y2k window are errors and convert to -1.
!            The window is  1900+y2k_start_year through
!             2000+y2k_start_year -1.
!
!     restrictions: fails 100 years after the start year in the y2k window
!
!     created by kdb 981008
!
!     input variables:
!      iyear_dig4 - full year
!     output variables:
!      iyear_dig2 - number representing the last two digits of a year
!                   (0 through 99 for 00 through 99)
!
      integer*2 iyear_dig2,iyear_dig4
!
      if (iyear_dig4.lt.1900+y2k_start_year) then
        iyear_dig2 = -1
      else if (iyear_dig4 .lt. 2000) then
        iyear_dig2 = iyear_dig4 - 1900
      else if (iyear_dig4 .lt. 2000+y2k_start_year) then
        iyear_dig2 = iyear_dig4 - 2000
      else
        iyear_dig2 = -1
      endif
!
      return
      end
