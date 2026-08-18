      SUBROUTINE write_cpl()
!
!
!
      implicit none
!
      INCLUDE "gsnoop_com.i"
!
      integer*2     i, j, k
      integer*2     trimlen, len
!
!
      character*80  qstr
      character*255 qbuffer
!
!     modifications
!
!     000905 kdb Fix bug: don't write DONE (end of cpl site list)
!                to cpls_uen_<> file.
!     060427 kdb Fix a header that was changed apparently by the software
!                that converts to fortran 90.
!
!
!     Put headers in files.
!
!       write (58,"(//,a8,x,a4,/,
!      &'YY MM DD          X          SIGMA          Y         SIGMA
!      &       Z        SIGMA')") cpls_name(1:8), cpls_name(9:12)
!
 1020 format (//,a8,x,a4,/, &
     &'YY MM DD         U     SIGMA        E     SIGMA        N       SI&
     &GMA')
!
      write (qstr ,'("Writing cpl file.")')
      call asnl(qstr)
!
!
!     write out results
!
      do k = 1, mcpl
        len = trimlen (qcpl_stat(k))
        if (len .gt. 0 .and. qcpl_stat(k)(1:4).ne.'DONE')  then
          write (59,1020) qcpl_stat(k), qmcpl(k)
          write (qbuffer, "('go')")  !  Initialize buffer
          j = 0
          do while (qbuffer(6:7) .ne. ' 0')
            j = j + 1
            write (qbuffer, "(255x)")  !  Initialize buffer
            write (qbuffer(1:), "(i3,x,3(x,i2),3(2x,f9.1,x,f7.1))") &
     &        j, &
     &        (icpls_date(k,i,j), i = 1,3), &
     &        (cpls_uen(k,i,j), cpls_uen_sig(k,i,j),i=1,3)
            len = trimlen (qbuffer)
            write (59, "(a)") qbuffer(1:len)
          end do
        end if
      end do
!
!
!
!
      return
      end
