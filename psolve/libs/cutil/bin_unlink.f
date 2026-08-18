      SUBROUTINE BIN_UNLINK(FNAME,IERR)
      IMPLICIT NONE
!
! 1.  BIN_UNLINK PROGRAM SPECIFICATION
!
! 1.1 Delete the specified file
!
! 1.2 REFERENCES:
!
! 2.  BIN_UNLINK INTERFACE
!
! 2.1 Parameter File
!
! 2.2 INPUT Variables:
!
      CHARACTER*(*) FNAME
!
! FNAME - Name of file to delete
!
! 2.3 OUTPUT Variables:
!
      INTEGER*2 IERR
!
! IERR - Error return from fc_unlink
!
! 2.4 COMMON BLOCKS USED
      INCLUDE 'fclib.i'
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES:
!       CALLED SUBROUTINES: fc_unlink
!
! 3.  LOCAL VARIABLES
!
      CHARACTER*10 ME
      INTEGER*4 JERR
      INTEGER*2 TRIMLEN
      DATA ME/'BIN_UNLINK'/
!
! JERR - VAlue returned from fc_unlink
! ME - Name of this routine
!
! 4.  HISTORY
!   WHO   WHEN   WHAT
!   AEE   920204 Removed hard coded path for fclib.i
!
! 5.  BIN_UNLINK PROGRAM STRUCTURE
!
      JERR = FC_UNLINK(PTR_CH(FNAME(:TRIMLEN(FNAME))//char(0)))
      IERR=JERR
!
      RETURN
      END
