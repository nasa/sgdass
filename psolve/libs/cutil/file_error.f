      SUBROUTINE FILE_ERROR(IERR,FNAME,WHO,WHAT)
      IMPLICIT NONE
!
! 1.  FILE_ERROR PROGRAM SPECIFICATION
!
! 1.1 Generate a file-related error message and pause the program.
!
! 1.2 REFERENCES:
!
! 2.  FILE_ERROR INTERFACE
!
! 2.1 Parameter File
!
! 2.2 INPUT Variables:
!
      INTEGER*4 IERR
      CHARACTER*(*) FNAME,WHO,WHAT
!
! IERR - Error number
! FNAME - Name of file to which error applies
! WHAT - Description of error type
! WHO - Routine in which error was detected
!
! 2.3 OUTPUT Variables: None
!
! 2.4 COMMON BLOCKS USED
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES:
!       CALLED SUBROUTINES: file_mess
!
! 3.  LOCAL VARIABLES
!
! 4.  HISTORY
!   WHO   WHEN   WHAT
!   AEE   910515 Enhanced error messages written to the error file.
!
! 5.  FILE_ERROR PROGRAM STRUCTURE
!
      CALL FILE_MESS(IERR,FNAME,WHO,WHAT)
!     call ferr(171,'in file_error',0,0) ! moved to file_mess.f, aee
!
      RETURN
      END
