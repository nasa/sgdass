      SUBROUTINE HARD_RESET()
      IMPLICIT NONE
!
! 1.  HARD_RESET PROGRAM SPECIFICATION
!
! 1.1 Power off reset of the terminal
!
! 1.2 REFERENCES:
!
! 2.  HARD_RESET INTERFACE
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
!       CALLING SUBROUTINES: mdlpl,menu,screen_pause
!       CALLED SUBROUTINES: utility routines
!
! 3.  LOCAL VARIABLES
!
      INTEGER*4 I4P0
      DATA I4P0 /0/
!
! 4.  HISTORY
!  WHO  WHEN   WHAT
!   JLR  921216  added I4P0 for 0J
!
! 5.  HARD_RESET PROGRAM STRUCTURE
!
!     Put the cursor at the upper left
      CALL SETCR_MN(I4P0,I4P0)
      CALL CLEAR_MN()
!
      RETURN
      END
