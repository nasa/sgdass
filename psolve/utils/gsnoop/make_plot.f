!'''/''''1''''/''''2''''/''''3''''/''''4''''/''''5''''/''''6''''/''''7''''/''''8
      SUBROUTINE make_plot (cfname, qdev)
!
!     Subroutine to drive pc8
!
      IMPLICIT NONE
!
      integer*2       ierr, system
      integer*2       trimlen, len, ltn
      integer*4       ierr4
!
      character*10    qdev
      character*50    cfname
      character*100   qstring
!
!
      len = trimlen(qdev)
      ltn = trimlen(cfname)
      write(qstring,'("/usr/local/bin/pc8 ",a," ",a)') &
     &       cfname(1:ltn), qdev(1:len)
      call zterm(qstring,ierr4)
      ierr = system(qstring)
!
!
      return
      end
