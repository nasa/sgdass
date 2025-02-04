      SUBROUTINE BIN_LINK(FNAME1,FNAME2,IERR)
      IMPLICIT NONE
!
! 1.  BIN_LINK PROGRAM SPECIFICATION
!
! 1.1 Establish a new link to an existing file.
!
! 1.2 REFERENCES:
!
! 2.  BIN_LINK INTERFACE
!
! 2.1 Parameter File
!
! 2.2 INPUT Variables:
!
      CHARACTER*(*) FNAME1,FNAME2
!
! FNAME1 - Name of the existing file
! FNAME2 - Name of the new link
!
! 2.3 OUTPUT Variables:
!
      INTEGER*2 IERR
!
! IERR - Value returned from call to fc_link
!
! 2.4 COMMON BLOCKS USED
      INCLUDE 'fclib.i'
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES:
!       CALLED SUBROUTINES: fc_link
!
! 3.  LOCAL VARIABLES
!
      CHARACTER*8 ME
      INTEGER*4 JERR
      DATA ME/'BIN_LINK'/
!
! JERR - Return value from fc_link
! ME - Name of this routine
!
! 4.  HISTORY
!   WHO   WHEN   WHAT
!   AEE   920204 Removed hard coded path for fclib.i
!
! 5.  BIN_LINK PROGRAM STRUCTURE
!
1     CONTINUE
      JERR=FC_LINK(PTR_CH(FNAME1//char(32)//FNAME2//char(0)))
      IERR=JERR
!
      RETURN
      END
