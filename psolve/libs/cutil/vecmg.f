      FUNCTION VECMG ( A )
      IMPLICIT NONE
!
! 1.  VECMG PROGRAM SPECIFICATION
!
! 1.1 Find L2 norm of a vector.
!
! 1.2 REFERENCES:
!
! 2.  VECMG INTERFACE
!
! 2.1 Parameter File
!
! 2.2 INPUT Variables:
!
      REAL*8 A(3)
!
! A - The vector in question
!
! 2.3 OUTPUT Variables:
!
      REAL*8 VECMG
!
! VECMG - The magnitude of the vector A
!
! 2.4 COMMON BLOCKS USED
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES:
!       CALLED SUBROUTINES: dotp
!
! 3.  LOCAL VARIABLES
!
      REAL*8 DOTP
!
! 4.  HISTORY
!   WHO           WHEN   WHAT
! Dale Markham   770119  Created
! Peter Denatale 770718  ???
!
! 5.  VECMG PROGRAM STRUCTURE
!
      VECMG = DSQRT ( DOTP ( A, A ) )
!
      RETURN
      END
