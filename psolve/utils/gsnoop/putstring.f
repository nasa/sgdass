!'''/''''1''''/''''2''''/''''3''''/''''4''''/''''5''''/''''6''''/''''7''''/''''8
      SUBROUTINE putstring(i, j, qstring)
      IMPLICIT NONE                         !Added by IMP/jwr
!
!     This subroutine then writes the string to the window through
!     its last nonblank at location i, j in the window.   It does
!     not add any carriage returns after it.
!
!     Input variables i,j are integer*2 and converted in this
!     routine for programmer's convenience.
!
!     DS Caprette HSTX 94/06/29  Wrote from scratch.
!
      integer*2     trimlen, len, i, j
      integer*4     ix, iy
      character*(*) qstring
!
      ix = jzext(i)
      iy = jzext(j)
      len = trimlen(qstring)
      call setcr_mn(ix, iy)
      call addstr_f(qstring(1:len))
!
      return
      end
