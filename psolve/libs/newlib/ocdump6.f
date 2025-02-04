      SUBROUTINE OCDUMP6(LUO,DUMPINT)
      Implicit none
!
!     written 9/20/95 by kdb
!
!     purpose: dump an integer as an octal, with a preceding 'o' to indicate
!              it's octal
!
!     input:
!
!     luo - output lu
!     DUMPINT - integer*2 number to dump
!
      integer*2 DUMPINT,luo
!
!     local variables:
!
      character*7 buf7
      integer*2 ibl_pt,ict1
!
      buf7 = ' '
      write(buf7(2:7),"(o6)") dumpint
      do ict1 = 1,7
        if (buf7(ict1:ict1).eq.' ') ibl_pt = ict1
      enddo
      buf7(ibl_pt:ibl_pt) = 'o'
      write(luo,"(A7,$)") buf7
!
      return
      end
