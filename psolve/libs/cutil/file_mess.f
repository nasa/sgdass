      SUBROUTINE FILE_MESS(IERR,FNAME,WHO,WHAT)
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
! 1.  FILE_MESS PROGRAM SPECIFICATION
!
! 1.1 Format and display file-related error message.
!
! 1.2 REFERENCES:
!
! 2.  FILE_MESS INTERFACE
!
! 2.1 Parameter File
!
! 2.2 INPUT Variables:
!
      INTEGER*4 IERR
      CHARACTER*(*) FNAME,WHO,WHAT
!
! IERR - Error number
! FNAME - File to which error applies
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
!       CALLED SUBROUTINES:
!
! 3.  LOCAL VARIABLES
!
      CHARACTER*6 ERR
      INTEGER*2 IL,TRIMLEN,ILERR
      CHARACTER*150 errstr
      character*80 bufstr
!
! IL - Length of file name
!
! 4.  HISTORY
!   WHO   WHEN   WHAT
!   AEE   910515 Enhanced error messages written to the error file.
!
! 5.  FILE_MESS PROGRAM STRUCTURE
!
! Format the message and display it
!
      ILERR=MAX(TRIMLEN(ERR),INT2(1))
      IL=MAX(TRIMLEN(FNAME),INT2(1))
99    FORMAT('Error ',I5,' from ',A,' while ',A,'ing: ',A)
      WRITE(errstr,99) IERR,WHO,WHAT,FNAME(1:IL)
      call ferr( INT2(171), errstr, INT2(0), INT2(0) )
!
      RETURN
      END
