      SUBROUTINE movewords(from,to,count)
      implicit none
      integer*2 from(1),to(1),count
!
      integer*2 i
!
      do i=1,count
        to(i)=from(i)
      enddo
!
      return
      end
