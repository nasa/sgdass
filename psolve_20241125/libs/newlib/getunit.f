        integer*2 function getunit()
      IMPLICIT NONE                         !Added by IMP/jwr
!
!-----The following type specification statements added automatically
!     by imp/jwr July 2002
!
      INTEGER*2 iutot
!-----END of imp added lines.
!
!
! Purpose       Returns a unique unit number for each new call.
!
! Note          It is recommended that all code should call getunit
!               in order to obtain unit numbers.  If this is not
!               desirable, the warning message below should be
!               disabled.
!
! Modified
!   B. Archinal         92.06.19  Rewritten.  Check made if unit
!   A. Myers                      already open.  Possible range
!                                 extended to include 1-4, 8-49 as unit
!                                 numbers.  Check made to see if too
!                                 many units in use.
!
      logical*4 lopen
!
!  Ipoint contains the last unit assigned.  If7 is standard error.
!  Iutot is the total number of units used (including std in, out, err).
!  Iumax is maximum number of units allowed open on this system (this
!    can be changed as a system installation parameter under HP-UX
!    8.0 as "maxfiles" in the /etc/master file).
!
      integer*2 ipoint,if7,iumax
      save ipoint
!
!  Somewhat arbitrarily we start with 51.
!
      data ipoint/50/,if7/7/,iutot/3/,iumax/60/
!
  100 ipoint = ipoint+1
      iutot = iutot + 1
!
!  If 100 or 5, 6, or 7, increment appropriately.
!
      if(ipoint.eq.100) ipoint = 1
      if(ipoint.gt.4.and.ipoint.lt.8) ipoint = 8
!
!  Terminate if out of unit numbers.
!
      if(iutot.gt.iumax) then
        write(if7,"(' *** ERROR *** Getunit: All available unit' &
     &  ,' numbers have been opened.'/ &
     &              '               Execution terminating.'/)")
        stop
      endif
!
!  Issue warning if unit already open, then increment again.
!
      inquire(unit=ipoint,opened=lopen)
      if(lopen) then
!        write(if7,"(' *** WARNING *** getunit has determined that unit' &
!     &  ,' number',I3/ '                 has already been opened' &
!     &  ,' elsewhere.'/'                 Proceeding with next unit' &
!     &  ,' number.'/)") ipoint
        go to 100
       endif
!
!
      getunit = ipoint
      return
      end
