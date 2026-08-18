      SUBROUTINE psfatal_w(str,who,nftreat)
      implicit none
!
! 1.  psfatal_w PROGRAM SPECIFICATION
!
! 1.1 Format possibly fatal error message.
!
!     expanded version of subroutine fatal_w; the error is possibly fatal
!     (as determined by the input argument, nftreat).
!
! 1.2 REFERENCES:
!
! 2.  psfatal_w INTERFACE
!
! 2.1 Parameter File
!
! 2.2 INPUT Variables:
!
      character*(*) str,who
      character*1 nftreat
!
! STR - Main body of message
! WHO - Routine in which error was detected
! nftreat - treat the error as fatal (F) or non-fatal (N)
!
! 2.3 OUTPUT Variables: None
!
! 2.4 COMMON BLOCKS USED
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES:
!       CALLED SUBROUTINES: psfatal
!
! 3.  LOCAL VARIABLES
!
!
      integer*2 trimlen
      character*1024 output
!
! OUTPUT - Formatted message to be sent along to PSFATAL
!
! 4.  HISTORY
!   WHO   WHEN   WHAT
!   KDB  960207  Created from fatal_w.
!
! 5.  psfatal_w PROGRAM STRUCTURE
!
! Format message and send off to PSFATAL
!
      output= &
     & 'error '//str(:trimlen(str))//' in '//who(:trimlen(who))
      call psfatal(output,nftreat)
      return
      end
