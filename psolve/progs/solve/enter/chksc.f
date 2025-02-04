      LOGICAL*2 FUNCTION CHKSC(FNAME)
      IMPLICIT NONE
!
! 1.  CHKSC PROGRAM SPECIFICATION
!
! 1.1 Check for the existence of a scratch file. An error is
!     printed to the terminal if an error occurs.
!
! 1.2 REFERENCES:
!
! 2.  CHKSC INTERFACE
!
! 2.1 Parameter File
!
! 2.2 INPUT Variables:
!
      CHARACTER*(*) FNAME
!
! FNAME - The name of the scratch file to be checked
!
! 2.3 OUTPUT Variables:
!
! CHKSC - TRUE if file exists; FALSE otherwise
!
! 2.4 COMMON BLOCKS USED
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES: enter
!       CALLED SUBROUTINES: None
!
! 3.  LOCAL VARIABLES
!
      INTEGER*2 IERR,ILETR,ILETX
      INTEGER*4 IOS
      Logical*4 ex
      character*80 bufstr
!
! 4.  HISTORY
!   WHO   WHEN   WHAT
!
! 5.  CHKSC PROGRAM STRUCTURE
!
! Check for existence of file
!
      INQUIRE ( FILE=FNAME, IOSTAT=IOS, EXIST=EX )
      CHKSC = EX
!
! Display error message if file is not found
!
      IF (.NOT.CHKSC) THEN
        WRITE(bufstr,1109) FNAME
        call addstr_f(bufstr)
        call nl_mn()
 1109   FORMAT(" ",A," COULD NOT BE OPENED")
      ENDIF
!
      RETURN
      END
