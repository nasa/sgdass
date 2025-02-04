      INTEGER*4 FUNCTION ETIMER ( SECS )
      IMPLICIT NONE
!
! 1.  ETIMER PROGRAM SPECIFICATION
!
! 1.1  Determine elapsed time.
!
! 1.2 REFERENCES:
!
! 2.  ETIMER INTERFACE
!
! 2.1 Parameter File
!
! 2.2 INPUT Variables: None
!
! 2.3 OUTPUT Variables:
!
      INTEGER*4 SECS
!
! ETIMER -- Elapsed time (since TIME0), in seconds
! SECS   -- Current time, in seconds
!
! 2.4 COMMON BLOCKS USED
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES: prgres
!       CALLED SUBROUTINES: timenow
!
! 3.  LOCAL VARIABLES
!
      INTEGER*4 ETIMER_TIME0
!
      INTEGER*4 TIMENOW
!
      COMMON / BATCH_TIME / ETIMER_TIME0
!
! 4.  HISTORY
!   WHO   WHEN         WHAT
!   pet   2004.10.27   Replaced name TIME with BATCH_TIME
!
! 5.  ETIMER PROGRAM STRUCTURE
!
      SECS = TIMENOW()
      ETIMER = SECS - ETIMER_TIME0
!
      RETURN
      END
