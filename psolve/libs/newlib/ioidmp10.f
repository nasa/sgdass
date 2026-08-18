      SUBROUTINE IOIDMP10(DUMPVAR,nstart,nstop,iocint,luo)
      Implicit none
!
!     written 9/19/95 by kdb
!
!     purpose: dump selected elements from an integer array as integer or
!              octal numbers, displaying 10 numbers per display row
!
!     restrictions: dumpvar must be single dimensioned
!
!     input:
!
!     dumpvar - integer*2 array to dump
!     nstart,nstop - range of elements to be dumped
!     iocint - dump format - 0 for octal, 1 for integer
!     luo - output lu
!
      integer*4 nstart,nstop
      integer*2 dumpvar(*),iocint,luo
!
!     local variables:
!
      integer*4 ict,jct
      integer*4 i4p10
      data i4p10 /10/
!
!     modifications
!
!     kdb  9/20/95 Precede octal numbers with 'o' to indicate they are octal.
!
      jct = 0
      do ict = nstart,nstop
        jct = jct + 1
!       Dump next element as integer or octal
        if (iocint.eq.1) then
          write(luo,"(I6,$)") dumpvar(ict)
        else
          call ocdump6(luo,dumpvar(ict))
        endif
        if (jct/i4p10 * i4p10 .eq. jct) then
!          Time for a new row
           write(luo,"('')")
        else
!          Still on current row.  Place a blank between numbers
           write(luo,"(1X,$)")
        endif
      end do
!
!     End the last row if it was incomplete (fewer than 10 numbers)
!
      if (jct/i4p10 * i4p10 .ne. jct) write(luo,"(' ')")
!
      return
      end
