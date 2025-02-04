      SUBROUTINE CROSP ( A, B, C )
      IMPLICIT NONE
!
! 1.  CROSP PROGRAM SPECIFICATION
!
! 1.1 Produce the cross product of two 3-dimensional vectors
!
! 1.2 REFERENCES:
!
! 2.  CROSP INTERFACE
!
! 2.1 Parameter File
!
! 2.2 INPUT Variables:
!
      REAL*8 A(3),B(3)
!
! A - The first vector
! B - The second vector
!
! 2.3 OUTPUT Variables:
!
      REAL*8 C(3)
!
! C - The cross product A x B
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
! 4.  HISTORY
!   WHO         WHEN   WHAT
! Dale Markham 770119  Created
! Peter Denatale 770718  ???
!
! 5.  CROSP PROGRAM STRUCTURE
!
      C(1) = A(2) * B(3)  -  B(2) * A(3)
      C(2) = A(3) * B(1)  -  B(3) * A(1)
      C(3) = A(1) * B(2)  -  B(1) * A(2)
!
      RETURN
      END
