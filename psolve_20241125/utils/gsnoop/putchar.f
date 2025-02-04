!'''/''''1''''/''''2''''/''''3''''/''''4''''/''''5''''/''''6''''/''''7''''/''''8
      SUBROUTINE gsn_putchar(i, j, qch)
      IMPLICIT NONE                         !Added by IMP/jwr
!
!     This subroutine then writes a character to the windo
!     at the location i, j.
!
!     Input variables i,j are integer*2 and converted in this
!     routine for programmer's convenience.
!
!     DS Caprette HSTX 94/07/25  cloned from putstring
!
      integer*2     i, j
      integer*4     ix, iy
      character*1   qch
!
      ix = jzext(i)
      iy = jzext(j)
      call setcr_mn(ix, iy)
      call addstr_f(qch)
      call refresh_mn()
!
      return
      end
