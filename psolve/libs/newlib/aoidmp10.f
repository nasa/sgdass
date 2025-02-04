      SUBROUTINE AOIDMP10(DUMPVAR,ibound1,ibound2, &
     &                    ircol,islot,nstart,nstop,iocint,luo)
      Implicit none
!
!     written 9/19/95 by kdb
!
!     purpose: dump a row or column of an integer array as integer or
!              octal numbers, displaying 10 numbers per display row
!
!     restrictions: dumpvar must be 2-dimensional
!
!     input:
!
!     dumpvar - integer*2 array to dump
!     ibound1 - size of array bounds
!     ibound2 - size of array bounds
!     ircol = 1 to dump row
!           = 2 to dump column
!     islot - row or column to be dumped
!     nstart,nstop - range of elements in column/row to be dumped
!     (i.e., ircol = 1 dumps dumpvar(islot,nstart-nstop) and
!            ircol = 2 dumps dumpvar(nstart-nstop,islot)
!     iocint - dump format - 0 for octal, 1 for integer
!     luo - output lu
!
      integer*4 nstart,nstop,islot,ibound1,ibound2
      integer*2 dumpvar(ibound1,ibound2),ircol,iocint,luo
!
!     local variables:
!
      integer*4 ict,jct
      integer*2 dumpint
      integer*4 i4p10
      data i4p10 /10/
!
!     modifications
!
!     kdb  9/20/95 Precede octal numbers with 'o' to indicate they are octal.
!
!
      jct = 0
      do ict = nstart,nstop
        jct = jct + 1
!       Find the next element to be dumped
        if (ircol.eq.1) then
          dumpint = dumpvar(islot,ict)
        else
          dumpint = dumpvar(ict,islot)
        endif
!       Dump it as integer or octal
        if (iocint.eq.1) then
          write(luo,"(I6,$)") dumpint
        else
          call ocdump6(luo,dumpint)
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
