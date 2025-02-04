      LOGICAL*2 FUNCTION CFEOF(IDUM)
      IMPLICIT NONE
!
! 1.  CFEOF PROGRAM SPECIFICATION
!
! 1.1 Low-level I/O utility to test for end of file on the control file
!     input stream.
!
! 1.2 REFERENCES:
!
! 2.  CFEOF INTERFACE
!
! 2.1 Parameter File
!
! 2.2 INPUT Variables:
!
      INTEGER*2 IDUM
!
! IDUM - Dummy input
!
! 2.3 OUTPUT Variables:
!
! CFEOF - True if end of file is found
!
! 2.4 COMMON BLOCKS USED
      INCLUDE 'batcm.i'
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES: Numerous routines
!       CALLED SUBROUTINES: None
!
! 3.  LOCAL VARIABLES
!
! 4.  HISTORY
!   WHO   WHEN   WHAT
!
! 5.  CFEOF PROGRAM STRUCTURE
!
! Simply pick up the end-of-file flag from batcm.i
!
      CFEOF=KEOF
!
      RETURN
      END
