      SUBROUTINE newcents(iyear_dig2,iyear_dig4)
      implicit none
!
      INCLUDE "param.i"
!
!     given the last two digits of a year, return the full year.
!      e.g., given 99, return 1999 and given 1, return 2001
!       values outside 0 to 99 range are errors and convert to -1
!
!     restrictions: fails 100 years after the start year in the y2k window
!
!     created by kdb 981005
!
!     input variables:
!      iyear_dig2 - number representing the last two digits of a year
!                   (0 through 99 for 00 through 99)
!     output variables:
!      iyear_dig4 - full year (e.g., 1999,2000)
!
      integer*2 iyear_dig2,iyear_dig4
!
      if (iyear_dig2.lt.0) then
        iyear_dig4 = -1
      else if (iyear_dig2 .lt. y2k_start_year) then
        iyear_dig4 = iyear_dig2 + 2000
      else if (iyear_dig2 .lt. 100) then
        iyear_dig4 = iyear_dig2 + 1900
      else
        iyear_dig4 = -1
      endif
!
      return
      end
