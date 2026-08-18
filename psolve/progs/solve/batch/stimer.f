      SUBROUTINE STIMER ( SECS )
      IMPLICIT NONE
!
! 1.  STIMER PROGRAM SPECIFICATION
!
! 1.1 Start processing timer.
!
! 1.2 REFERENCES:
!
! 2.  STIMER INTERFACE
!
! 2.1 Parameter File
!
! 2.2 INPUT Variables:
!
      INTEGER*4 SECS
!
! SECS - Time that processing on this arc originally started
!
! 2.3 OUTPUT Variables: None
!
! 2.4 COMMON BLOCKS USED
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES: rstors
!       CALLED SUBROUTINES: None
!
! 3.  LOCAL VARIABLES
!
      INTEGER*4 STIMER_TIME0
!
      COMMON / BATCH_TIME / STIMER_TIME0
!
! 4.  HISTORY
!   WHO   WHEN         WHAT
!   pet   2004.10.27   Replaced name TIME with BATCH_TIME
!
! 5.  STIMER PROGRAM STRUCTURE
!
      STIMER_TIME0 = STIMER_TIME0 - SECS
!
      RETURN
      END
