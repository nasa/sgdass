      SUBROUTINE SCORF_CENTER (ITPCAL, IHIGHYIND)
      implicit none
!
! 1.  SCORF_CENTER PROGRAM SPECIFICATION
!
! 1.1
!     A really simple sub to place the cursor near the
!     center of the active part of the screen.
!
! 1.2 REFERENCES:
!
! 2.  SCORF_CENTER INTERFACE
!
! 2.1 Parameter File
!
! 2.2 INPUT Variables:
!
      integer*2 ITPCAL,ihighyind
!
! ITPCAL - Number of calibrations on this page
! IHIGHYIND - Y coordinate of the line for the last station on this page
!
! 2.3 OUTPUT Variables: none
!
! 2.4 COMMON BLOCKS USED
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES: selcorf
!       CALLED SUBROUTINES: utility routines
!
! 3.  LOCAL VARIABLES
!
      INTEGER*4 IFLDLENIND,IFRSTCHRXIND,IX,IY
      DATA IFRSTCHRXIND/13/
      DATA IFLDLENIND/5/
!
! 4.  HISTORY
!   WHO   WHEN     WHAT
!   KDB   9/19/91 CREATED
!
! 5.  SCORF_CENTER PROGRAM STRUCTURE
!
      IX = IFRSTCHRXIND + (ITPCAL/2) * IFLDLENIND
      IY = IHIGHYIND + 1
      CALL SETCR_MN(IX,IY)
!
      RETURN
      END
