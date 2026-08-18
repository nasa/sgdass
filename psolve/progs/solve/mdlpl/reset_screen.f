      SUBROUTINE RESET_SCREEN(ILINE)
      IMPLICIT NONE
!
! 1.  RESET_SCREEN PROGRAM SPECIFICATION
!
! 1.1 Sets the cursor to line ILINE and then clears the screen below it.
!
! 1.2 REFERENCES:
!
! 2.  RESET_SCREEN INTERFACE
!
! 2.1 Parameter File
!
! 2.2 INPUT Variables:
!
      INTEGER*4 ILINE
!
! ILINE - line to set the cursor at
!
! 2.3 OUTPUT Variables: None
!
! 2.4 COMMON BLOCKS USED
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES: menu,screen_pause
!       CALLED SUBROUTINES: utility routines
!
! 3.  LOCAL VARIABLES
      INTEGER*4  I4P0
      DATA I4P0 /0/
!
! 4.  HISTORY
!  WHO  WHEN  WHAT
!   JLR  921216 replaced 0J with I4P0
!
! 5.  RESET_SCREEN PROGRAM STRUCTURE
!
      CALL SETCR_MN(I4P0,ILINE)
      CALL CLRTOBOT_MN()
      RETURN
      END
