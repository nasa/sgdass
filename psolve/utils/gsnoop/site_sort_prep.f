      SUBROUTINE site_sort_prep(direction,nsites,array_dig2,array_dig4)
!
      implicit none
!
!     written by kdb 11/20/98
!
!     purpose: The internal gsnoop representation of site names in the master
!              site array stores the
!              episodic date (or 000000 for no episodic date) in characters
!              13-18, in the form yrmndy, where yr is a 2 digit year.
!              This makes it impossible to directly sort the array for years
!              past 1999, since 00 (2000) will precede 99 (1999).
!
!              The solution is to temporarily copy the array into an array
!              where the 6 character field YRmndy is expanded to the
!              8 character field (YEARmndy) (with a 4 digit year),
!              sort on that array,
!              and copy  the sorted array back.
!
!              This subroutine copies the array with the 6 character/2 digit
!                 year format to the array with the 8 character/4 digit year
!                 format or vice versa.
!
!     input arguments:
!
!     direction - '24' to convert from 2 digit to 4 digit year
!                 '42' to convert from 4 digit to 2 digit year
!     nsites - number of sites in each array
!
!     input/output arguments:
!
!     array_dig2 - array with 2 digit year (6 character date field)
!          input for direction = 24
!         output for direction = 42
!     array_dig4 - array with 4 digit year (8 character date field)
!          input for direction = 42
!         output for direction = 24
!
      character*2 direction
      integer*2 nsites
      character*23 array_dig2(*)
      character*25 array_dig4(*)
!
!     local variables
!
      integer*2 ict
      integer*2 iyear_sort_dig2,iyear_sort_dig4
!
!     note: In the 2 digit year /6 character format, the episodic date field is
!             in characters 13-18.
!           In the 4 digit year /8 character format, it's in characters 13-20.
!           In either case, a field of all zeroes indicates a non-episodic site.
!
      if (direction .eq. '24') then
        do ict = 1,nsites
          array_dig4(ict)(1:12) = array_dig2(ict)(1:12)
          if (array_dig2(ict)(13:18).eq.'000000') then
!           Non-episodic site: field is all zeroes.  Keep the field all zeroes.
            write(array_dig4(ict)(13:20),"('00000000')")
          else
!           Episodic site:  Convert the year field
            read(array_dig2(ict)(13:14),"(i2)") iyear_sort_dig2
            call newcents(iyear_sort_dig2,iyear_sort_dig4)
            write(array_dig4(ict)(13:16),"(i4)") iyear_sort_dig4
!           Copy the month and day
            array_dig4(ict)(17:20) = array_dig2(ict)(15:18)
          endif
          array_dig4(ict)(21:25) = array_dig2(ict)(19:23)
        enddo
      else if (direction .eq. '42') then
        do ict = 1,nsites
          array_dig2(ict)(1:12) = array_dig4(ict)(1:12)
          if (array_dig4(ict)(13:20).eq.'00000000') then
            write(array_dig2(ict)(13:18),"('000000')")
          else
            read(array_dig4(ict)(13:16),"(i4)") iyear_sort_dig4
            call decents(iyear_sort_dig4,iyear_sort_dig2)
            write(array_dig2(ict)(13:14),"(i2.2)") iyear_sort_dig2
            array_dig2(ict)(15:18) = array_dig4(ict)(17:20)
          endif
          array_dig2(ict)(19:23) = array_dig4(ict)(21:25)
        end do
      endif
!
      return
      end
