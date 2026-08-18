      SUBROUTINE SCOR_BLNKMSG()
      implicit none
!
! 1.  SCOR_BLNKMSG PROGRAM SPECIFICATION
!
! 1.1
!     ERASES MESSAGE AREA SO THAT ANY MESSAGES FROM THE USER'S
!     PREVIOUS COMMAND WILL NOT LINGER TO CONFUSE HIM.
!
! 1.2 REFERENCES:
!
! 2.  SCOR_BLNKMSG INTERFACE
!
! 2.1 Parameter File
      INCLUDE 'solve.i'
!
! 2.2 INPUT Variables: None
!
! 2.3 OUTPUT Variables: None
!
! 2.4 COMMON BLOCKS USED
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES: selcor
!       CALLED SUBROUTINES: utility routines
!
! 3.  LOCAL VARIABLES
!
      integer*4 imsgx,imsglin
      DATA IMSGX/0/, IMSGLIN/30/
!
! 4.  HISTORY
!   WHO   WHEN   WHAT
!   JLR   921215  removed J's from DATA for variables decl I4 already
!   kdb   950810  finish changing for 32 sites
!
! 5.  SCOR_BLNKMSG PROGRAM STRUCTURE
!
      CALL SETCR_MN (IMSGX, IMSGLIN)
      CALL clrtobot_mn()
      RETURN
      END
