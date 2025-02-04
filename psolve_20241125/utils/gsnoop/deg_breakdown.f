      SUBROUTINE deg_breakdown(single_val,deg_val,min_val,sec_val)
!
!     given a single value in degrees, breaks it down into degrees, hours and
!     minutes
!
!     written 3/24/95 by kdb to support a new file (1995 iers site submission
!                            file (site/id section)
!
      IMPLICIT NONE
!
!     input:
!
      real*8 single_val
!
!     output:
!
      integer*2 deg_val,min_val
      real*8 sec_val
!
      real*8 leftover,use_val
      character*1 lsign
!
      if (single_val.lt.0.0D0) then
        lsign = 'n'
      else
        lsign = 'p'
      end if
      use_val = dabs(single_val)
!
      deg_val = int(use_val) !number of full degrees
      leftover = use_val - deg_val !fraction of a degree left
      leftover = leftover * 60.0D0 !number of minutes
      min_val = int(leftover) !number of full minutes
      leftover = leftover - min_val !fraction of a minute left
      sec_val = leftover * 60.0D0 !number of seconds
!
      if (lsign.eq.'n') deg_val = deg_val * -1
!
      return
      end
