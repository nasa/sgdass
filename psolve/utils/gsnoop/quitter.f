      SUBROUTINE quitter(qsub,buffer,ios)
!
      implicit none
      character*(*) buffer
      character*(*) qsub
      character*80  qstr
      integer*2 ios
!
      write(qstr,'("Error "i5," reading the following record" )') &
     &ios
      call asnl(qstr)
      qstr = buffer
      call asnl(qstr)
      qstr = qsub
      call asnl(qstr)
!
      stop
      return
      end
