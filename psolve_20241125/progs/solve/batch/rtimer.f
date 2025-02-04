      SUBROUTINE RTIMER()
      IMPLICIT NONE
!
! 1.  RTIMER PROGRAM SPECIFICATION
!
! 1.1 Reset timer.
!
! 1.2 REFERENCES:
!
! 2.  RTIMER INTERFACE
!
! 2.1 Parameter File
!
! 2.2 INPUT Variables: None
!
! 2.3 OUTPUT Variables: None
!
! 2.4 COMMON BLOCKS USED
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES: ctrls
!       CALLED SUBROUTINES: timenow
!
! 3.  LOCAL VARIABLES
!
      INTEGER*4 RTIMER_TIME0
!
      INTEGER*4 TIMENOW
!
      COMMON / BATCH_TIME / RTIMER_TIME0
!
! 4.  HISTORY
!   WHO   WHEN         WHAT
!   pet   2004.10.27   Replaced name TIME with BATCH_TIME
!
! 5.  RTIMER PROGRAM STRUCTURE
!
! Get the current time
!
      RTIMER_TIME0 = TIMENOW()
!
      RETURN
      END
