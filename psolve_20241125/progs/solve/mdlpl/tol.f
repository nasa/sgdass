      LOGICAL*2 FUNCTION TOL(A,B,EPS)
      IMPLICIT NONE
!
! 1.  TOL PROGRAM SPECIFICATION
!
! 1.1 Returns True if absolute value of A-B is less than the tolerance EPS,
!     and returns false otherwise
!
! 1.2 REFERENCES:
!
! 2.  TOL INTERFACE
!
! 2.1 Parameter File
!
! 2.2 INPUT Variables:
!
      REAL*8 A,B,EPS
!
! A,B - Parameters to be checked for tolerance
! EPS - Error  tolerance
!
! 2.3 OUTPUT Variables:
!
! TOL -
!
! 2.4 COMMON BLOCKS USED
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES: secnd
!       CALLED SUBROUTINES: None
!
! 3.  LOCAL VARIABLES
!
! 4. HISTORY
!  WHO  WHEN   WHAT
!
! 5.  TOL PROGRAM STRUCTURE
!
      TOL=DABS(A-B).LT.EPS
!
      RETURN
      END
