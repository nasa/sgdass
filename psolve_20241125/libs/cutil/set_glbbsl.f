      SUBROUTINE set_glbbsl(state)
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!     Updated to specificaly type integers which
!-------------------------------------------------
!
! 1.  SET_GLBBSL PROGRAM SPECIFICATION
!
! 1.1 Set global baseline control bits and logicals in precm.
!
! 1.2 REFERENCES:
!
! 2.  SET_GLBBSL INTERFACE
!
! 2.1 Parameter File
      INCLUDE 'solve.i'
!
! 2.2 INPUT Variables:
!
      LOGICAL*2 STATE
!
! STATE - New state to be set
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
! 4.  HISTORY
!   WHO   WHEN   WHAT
!
! 5.  SET_GLBBSL PROGRAM STRUCTURE
!
      kglbbsl=state
      if(kbatch) then
        call sbit( pre_ip(3), INT2(7), INT2(1) )
      else
        call sbit( pre_ip(3), INT2(7), INT2(0) )
      endif
      pre_ibatch=pre_ip(3)
!
      return
      end
