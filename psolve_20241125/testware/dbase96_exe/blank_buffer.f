      SUBROUTINE blank_buffer(string,count)
!
!     Simple string blanker
!
      implicit none
      Character*(*) string
      integer*2 count,i
!
      Do i=1,count
        string(i:i) = ' '
      Enddo
!
      return
      end
