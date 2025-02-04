!'''/''''1''''/''''2''''/''''3''''/''''4''''/''''5''''/''''6''''/''''7''''/''''8
      SUBROUTINE return_to_continue()
!
!
      implicit none
!
      character*80  qstr
!
      qstr = "<RETURN>"
      call blink_on_mn()
      call addstr_f(qstr(1:8))
      call blink_off_mn()
      qstr = " to continue"
      call asnl(qstr)
      call refresh_mn()
      call getstr_f(qstr)
      return
      end
