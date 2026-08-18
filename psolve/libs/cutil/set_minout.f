      SUBROUTINE set_minout(state)
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
! 1.  SET_MINOUT PROGRAM SPECIFICATION
!
! 1.1 Set the minimum output control bit and logicals in precm.
!
! 1.2 REFERENCES:
!
! 2.  SET_MINOUT INTERFACE
!
! 2.1 Parameter File
      INCLUDE 'solve.i'
!
! 2.2 INPUT Variables:
!
      logical*2 state
!
! STATE - New value to be set (TRUE to get minimum output to spool file)
!
! 2.3 OUTPUT Variables: None
!
! 2.4 COMMON BLOCKS USED
      INCLUDE 'precm.i'
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES:
!       CALLED SUBROUTINES:
!
! 3.  LOCAL VARIABLES
!
! 4. HISTORY
!   WHO   WHEN   WHAT
!
! 5.  SET_MINOUT PROGRAM STRUCTURE
!
      kminout=state
      kfullout=.not.kminout
      if(kminout) then
        call sbit( pre_ip(3), INT2(2), INT2(1) )
      else
        call sbit( pre_ip(3), INT2(2), INT2(0) )
      endif
      pre_ibatch=pre_ip(3)
!
      return
      end
