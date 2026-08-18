      REAL*8 FUNCTION DATAN2Z(Y,X)
      IMPLICIT NONE
!
! 1.  DATAN2Z PROGRAM SPECIFICATION
!
! 1.1 Returns ArcTan of Y/X, unless Y or X is less than 1e-30,
!     in which case zero is returned.
!
! 1.2 REFERENCES:
!
! 2.  DATAN2Z INTERFACE
!
! 2.1 Parameter File
!
! 2.2 INPUT Variables:
!
      REAL*8 Y,X
!
! Y,X - Arguments to be passed on to datan2
!
! 2.3 OUTPUT Variables:
!
! DATAN2Z - Value returned from datan2
!
! 2.4 COMMON BLOCKS USED
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES:
!       CALLED SUBROUTINES: datan2
!
! 3.  LOCAL VARIABLES
!
! 4.  HISTORY
!   WHO   WHEN   WHAT
!
! 5.  DATAN2Z PROGRAM STRUCTURE
!
      DATAN2Z=0.0D0
      IF(DABS(Y).GE.1D-30.AND.DABS(X).GE.1D-30) DATAN2Z=DATAN2(Y,X)
!
      RETURN
      END
