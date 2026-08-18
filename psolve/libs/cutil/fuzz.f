      LOGICAL*2 FUNCTION FUZZ(A,B)
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!     Updated to specificaly type integers which
!-------------------------------------------------
!
! 1.  FUZZ PROGRAM SPECIFICATION
!
! 1.1 Determine whether two numbers differ by less than some epsilon.
!
! 1.2 REFERENCES:
!
! 2.  FUZZ INTERFACE
!
! 2.1 Parameter File
!
! 2.2 INPUT Variables:
!
      REAL*8 A,B
!
! A,B - Two numbers to be compared (order does not matter)
!
! 2.3 OUTPUT Variables:
!
! FUZZ - True if difference between A and B is less than epsilon
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
      REAL*8 EPS
      PARAMETER (EPS=1.0E-12)
!
! EPS - Epsilon value used as comparison criterion
!
! 4.  HISTORY
!   WHO   WHEN   WHAT
!
! 5.  FUZZ PROGRAM STRUCTURE
!
      FUZZ=ABS(A-B).LT.EPS
!
      RETURN
      END
