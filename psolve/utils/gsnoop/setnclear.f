!
      SUBROUTINE setnclear (ix, iy, luo)
!
!     set cursor & clear below
!
      implicit none
!
      integer*2 ix, iy, luo
!
      call setcr (ix, iy)
      call cldsp (luo)
!
      return
      end
