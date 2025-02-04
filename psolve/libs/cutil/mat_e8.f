      FUNCTION MAT_E8 ( MPAR, NP )
      IMPLICIT NONE
!
! 1.  MAT_E PROGRAM SPECIFICATION
!
! 1.1 Returns the number of elements in NRMFIL through
!     the specified parameter number.
!
! 1.2 REFERENCES:
!
! 2.  MAT_E INTERFACE
!
! 2.1 Parameter File
      INCLUDE 'solve.i'
!
! 2.2 INPUT Variables:
!
      INTEGER*8 MPAR, NP
!
! MPAR - Maximum allowed number of parameters
! NP - Parameter number
!
! 2.3 OUTPUT Variables:
!
      INTEGER*8 MAT_E8
!
! MAT_E - Number of elements in NRMFIL through parameter NP
!
! 2.4 COMMON BLOCKS USED
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
! 5.  MAT_E PROGRAM STRUCTURE
!
      MAT_E8 = 3*MPAR + (INT8(NP)*INT8(NP+1))/2
!
      RETURN
      END
