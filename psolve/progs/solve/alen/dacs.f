      REAL*8 FUNCTION DACS(X)
      IMPLICIT NONE
!
! 1.  DACS PROGRAM SPECIFICATION
!
! 1.1 Double precision arccosine function.
!
! 1.2 REFERENCES:
!
! 2.  DACS INTERFACE
!
! 2.1 Parameter File
!
! 2.2 INPUT Variables:
!
      REAL*8 X
!
! X - Value for which arccos is required
!
! 2.3 OUTPUT Variables:
!
! DACS - Arccos of X
!
! 2.4 COMMON BLOCKS USED
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES: arcst
!       CALLED SUBROUTINES: None
!
! 3.  LOCAL VARIABLES
!
      REAL*8 COSX,SINX
!
! 4.  HISTORY
!   WHO   WHEN   WHAT
!
! 5.  DACS PROGRAM STRUCTURE
!
      COSX=X
      SINX=DSQRT(1.D0-COSX*COSX)
      DACS=DATAN2(SINX,COSX)
      RETURN
      END
