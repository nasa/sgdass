      FUNCTION MAT_E(MPAR,NP)
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
      INTEGER*2 MPAR,NP
!
! MPAR - Maximum allowed number of parameters
! NP - Parameter number
!
! 2.3 OUTPUT Variables:
!
      INTEGER*4 MAT_E
      INTEGER*4 I4P1, I4P2
      DATA  I4P1, I4P2 / 1, 2 /
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
      MAT_E=3*MPAR+(NP*(NP+I4P1))/I4P2
!
      RETURN
      END
