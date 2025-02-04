!'''/''''1''''/''''2''''/''''3''''/''''4''''/''''5''''/''''6''''/''''7''''/''''8
      SUBROUTINE asnl(qstring)
      IMPLICIT NONE                         !Added by IMP/jwr
!
!     This subroutine cls addstr_f, writes the string to the window
!     through its last nonblank character and then puts two linefeed
!     carriage returns after it.
!
!     DS Caprette HSTX 94/06/28  Wrote from scratch.
!     K. Baver         00/10/26  Handle blank lines, which do not work under
!                                fortran range checking.  (Trimlen returns a
!                                length of 0, which causes a range error in
!                                the addstr_f call.)
!
      integer*2     trimlen, len
      character*(*) qstring
!
      len = trimlen(qstring)
!     Check for a blank line
      if (len.eq.0) len = 1
      call addstr_f(qstring(1:len))
      call nl_mn()
      call refresh_mn()
!
      return
      end
