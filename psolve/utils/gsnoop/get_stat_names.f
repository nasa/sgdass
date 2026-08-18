!'''/''''1''''/''''2''''/''''3''''/''''4''''/''''5''''/''''6''''/''''7''''/''''8
      SUBROUTINE get_stat_names(qstat,j,uenlu)
!
      implicit none
      integer*2      uenlu
      integer*2      j
      INTEGER*4      IOS
      character*8    qstat(*)
      character*255  qbuf(2)
!
!   modifications
!
!   kdb 10/26/00 range checking is being enabled, so convert arguments from (1)
!                to (*).
!
!      initialize
      j = 0
      IOS = 0
!
      do while (ios .eq. 0)
         read ( uenlu, '(a)', iostat=ios ) qbuf(2)
         if ( ios .eq. 0) then
              if ( qbuf(2)(1:2) .eq. "YY") then
                   j = j + 1
                   qstat(j) = qbuf(1)(1:8)
              end if
              qbuf(1) = qbuf(2)
         end if ! (ios
      end do
!
      return
      end
