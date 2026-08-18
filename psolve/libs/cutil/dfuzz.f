      LOGICAL*2 FUNCTION DFUZZ(A,B)
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!     Updated to specificaly type integers which
!-------------------------------------------------
!
! 1.  DFUZZ PROGRAM SPECIFICATION
!
! 1.1 Tests whether the difference between two double precision
!     numbers is less than an epsilon value. This is the double
!     precision version of function FUZZ.
!
! 1.2 REFERENCES:
!
! 2.  DFUZZ INTERFACE
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
! DFUZZ - True if difference between A and B is less than epsilon
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
      REAL*8 DEPS
      PARAMETER (DEPS=1.0D-12)
!
! DEPS - Epsilon value used as test criterion
!
! 4.  HISTORY
!   WHO   WHEN   WHAT
!
! 5.  DFUZZ PROGRAM STRUCTURE
!
      DFUZZ=DABS(A-B).LT.DEPS
!
      RETURN
      END
