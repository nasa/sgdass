      SUBROUTINE psfatal(str,nftreat)
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
! 1.  psfatal PROGRAM SPECIFICATION
!
! 1.1 Report an error and, if it's fatal, exit.
!
!     Expanded version of subroutine fatal; the error is possibly fatal
!     (as determined by the input argument, nftreat).
!
! 1.2 REFERENCES:
!
! 2.  psfatal INTERFACE
!
! 2.1 Parameter File
!
! 2.2 INPUT Variables:
!
      character*(*) str
      character*1 nftreat
!
! STR - String with message to be displayed
! nftreat - treat the error as fatal (F) or non-fatal (N)
!
! 2.3 OUTPUT Variables: None
!
! 2.4 COMMON BLOCKS USED
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES:
!       CALLED SUBROUTINES: fc_exit,rcpar
!
! 3.  LOCAL VARIABLES
!
      character*128 name
      integer*2 trimlen
      integer*4 i4
      data i4/-1/
!
! I4 - Argument for call to FC_EXIT (-1)
! NAME - Name of currently executing program
!
! 4.  HISTORY
!   WHO   WHEN   WHAT
!
!   KDB  960207  Created from fatal.
!
! 5.  psfatal PROGRAM STRUCTURE
!
! Get the program name, display the message and
!
!   1. exit if the error's fatal  OR
!   2. return for higher lever error handling if the error is non-fatal.
!
      CALL RCPAR( INT2(0), NAME )
      write(*,'(A,": ",A)') name(:trimlen(name)),str(:trimlen(str))
      if (nftreat.ne.'N') call fc_exit(i4)
      return
      end
