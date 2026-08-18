!'''/''''1''''/''''2''''/''''3''''/''''4''''/''''5''''/''''6''''/''''7''''/''''8
      FUNCTION next_nonblank (buffer, i)
!
!     This function searches the string buffer starting at position
!     i and returns the position of the next nonblank character.
!     If buffer(i:i) is nonblank the value of the function will be
!     returned as i.  If the buffer is entirely blank the value of 0
!     will be returned
!
!     94/06  DS Caprette HSTX
!
!
      implicit none
!
      integer*2      i, j, next_nonblank
      integer*2      len, trimlen
!
      character*1    q
      character*(*)  buffer
!
!
      q = buffer(i:i)
      j = i + 1
      len = trimlen (buffer)
!
      do while ((q .eq. " ").and.(j .le. len))
        q = buffer(J:J)
        j = j + 1
      end do
!
      if (j .le. len) then
        next_nonblank = j - 1
      else
        next_nonblank = 0
      end if
!
!
!
      return
      end
