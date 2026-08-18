      SUBROUTINE GET_PROG_NAM(INAME)
      IMPLICIT NONE
!
! 1.  GET_PROG_NAM PROGRAM SPECIFICATION
!
! 1.1 Uses a system call to get the six character name of
!     the program now running.
!
! 1.2 REFERENCES:
!
! 2.  GET_PROG_NAM INTERFACE
!
! 2.1 Parameter File
!
! 2.2 INPUT Variables: None
!
! 2.3 OUTPUT Variables:
!
      INTEGER*2 INAME(3)
!
! INAME - Name of program now running
!
! 2.4 COMMON BLOCKS USED
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES:
!       CALLED SUBROUTINES: pname
!
! 3.  LOCAL VARIABLES
!
! 4.  HISTORY
!   WHO   WHEN   WHAT
!   JWR  890205  Created
!
! 5.  GET_PROG_NAM PROGRAM STRUCTURE
!
      CALL PNAME(INAME)
      RETURN
      END
