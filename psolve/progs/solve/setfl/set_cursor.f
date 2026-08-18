      SUBROUTINE set_cursor(ix,iy,xlast,ylast)
!
!     Set_cursor is an alturnate routine to call to set the cursor position
!     when using 'cursus'. It has the added feature that it returns the
!     position set in the variables xlast and ylast.
!
!
      implicit none
!
!     Input:
      integer*4 ix !the screen position to the right - starting from 0.
      integer*4 iy !the screen position down         - starting from 0.
      integer*4 xlast,ylast !The returned coordinates
!
      xlast = ix
      ylast = iy
!
      CALL setcr_mn(ix,iy)
!
      return
      end
