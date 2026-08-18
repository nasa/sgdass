      SUBROUTINE FILE_SUSP(FNAME)
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
! 1.  FILE_SUSP PROGRAM SPECIFICATION
!
! 1.1 Suspend program execution for 10 seconds to wait for a locked file.
!
! 1.2 REFERENCES:
!
! 2.  FILE_SUSP INTERFACE
!
! 2.1 Parameter File
!
! 2.2 INPUT Variables:
!
      CHARACTER*(*) FNAME
!
! FNAME - Name of file we are waiting for
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
      INTEGER*2 IL,TRIMLEN
      character*80 bufstr
!
! IL - Length of file name
!
! 4.  HISTORY
!   WHO   WHEN   WHAT
!
! 5.  FILE_SUSP PROGRAM STRUCTURE
!
      IL=MAX(TRIMLEN(FNAME),INT2(1))
99    FORMAT(A,' LOCKED - SUSPENDING FOR 10 SECONDS')
      call ferr( INT2(173), 'file_susp pauses', INT2(0), INT2(0) )
      RETURN
      END
