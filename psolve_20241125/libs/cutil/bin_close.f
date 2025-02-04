      SUBROUTINE BIN_CLOSE ( FNAME, FILDES )
      IMPLICIT NONE
!
! 1.  BIN_CLOSE PROGRAM SPECIFICATION
!
! 1.1 Binary file closing routine, which calls the C routine fs_close
!
! 1.2 REFERENCES:
!
! 2.  BIN_CLOSE INTERFACE
!
! 2.1 Parameter File
!
! 2.2 INPUT Variables:
!
      INTEGER*4 FILDES
      CHARACTER*(*) FNAME
!
! FILDES - File descriptor of file to be closed
! FNAME - Name of file to be closed
!
! 2.3 OUTPUT Variables: None
!
! 2.4 COMMON BLOCKS USED
      INCLUDE 'fclib.i'
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES:
!       CALLED SUBROUTINES: fc_close,fatal_file
!
! 3.  LOCAL VARIABLES
!
      CHARACTER*9 ME
      INTEGER*4 JERR
      DATA ME/'BIN_CLOSE'/
!
! JERR - (Error) return from fc_close
! ME - Name of this routine
!
! 4.  HISTORY
!   WHO   WHEN   WHAT
!   AEE   920204 Removed hard coded path for fclib.i
!
! 5.  BIN_CLOSE PROGRAM STRUCTURE
!
! Call C routine to close file and handle error, if any
!
      JERR=FC_CLOSE(FILDES)
      CALL FATAL_FILE ( JERR, 'closing', FNAME, ME )
!
      RETURN
      END  !#!  BIN_CLOSE  #!#
