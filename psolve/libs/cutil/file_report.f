      SUBROUTINE FILE_REPORT(FNAME,WHO,WHAT)
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
! 1.  FILE_REPORT PROGRAM SPECIFICATION
!
! 1.1 Display file-related error message and suspend program execution.
!
! 1.2 REFERENCES:
!
! 2.  FILE_REPORT INTERFACE
!
! 2.1 Parameter File
!
! 2.2 INPUT Variables:
!
      CHARACTER*(*) FNAME,WHO,WHAT
!
! FNAME - Name of file to which error applies
! WHAT - Description of error
! WHO - Routine in which error was detected
!
! 2.3 OUTPUT Variables: None
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
      INTEGER*2 IL, IO, IA, TRIMLEN, INAME(3)
      CHARACTER*5 NAME
      CHARACTER*300 errstr
      character*300 bufstr
      EQUIVALENCE (INAME,NAME)
!
! INAME,NAME - Name of currently executing program
!
! 4.  HISTORY
!   WHO   WHEN   WHAT
!   AEE   910515 Enhanced error messages written to the error file.
!
! 5.  FILE_REPORT PROGRAM STRUCTURE
!
! format and display error message and 'suspended' message and pause
!
      IL=MAX(TRIMLEN(FNAME),INT2(1))
      IA=MAX(TRIMLEN(WHAT),INT2(1))
      IO=MAX(TRIMLEN(WHO),INT2(1))
99    FORMAT('Error: ',A,' from ',A,' while accessing: ',A)
      CALL PNAME(INAME )
      WRITE ( ERRSTR, 99 ) WHAT(1:IA), WHO(1:IO), FNAME(1:IL)
      CALL FERR ( INT2(172), ERRSTR, INT2(0), INT2(0) )
      WRITE(errstr,'(A5," Suspended.")') NAME
      call ferr( INT2(172), errstr, INT2(0), INT2(0) )
!
      RETURN
      END
