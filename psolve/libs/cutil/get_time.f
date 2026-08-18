      SUBROUTINE GET_TIME(TIME)
      IMPLICIT NONE
!
! 1.  GET_TIME PROGRAM SPECIFICATION
!
! 1.1 Grab the observation time tag for the active observation.
!     Used by routines that don't have OBORG.
!
! 1.2 REFERENCES:
!
! 2.  GET_TIME INTERFACE
!
! 2.1 Parameter File
      INCLUDE 'solve.i'
!
! 2.2 INPUT Variables: None
!
! 2.3 OUTPUT Variables:
!
      REAL*8 TIME
!
! TIME - Time tag for active observation
!
! 2.4 COMMON BLOCKS USED
      INCLUDE 'oborg.i'
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
!   JWR  890202  Created
!
! 5.  GET_TIME PROGRAM STRUCTURE
!
      TIME = FJD + FRACTC
      RETURN
      END
