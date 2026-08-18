      SUBROUTINE VECSB ( A, B, C )
      IMPLICIT NONE
!
! 1.  VECSB PROGRAM SPECIFICATION
!
! 1.1 Subtract one vector from another.
!
! 1.2 REFERENCES:
!
! 2.  VECSB INTERFACE
!
! 2.1 Parameter File
!
! 2.2 INPUT Variables:
!
      REAL*8 A(3), B(3)
!
! A - The first vector
! B - The second vector
!
! 2.3 OUTPUT Variables:
!
      REAL*8 C(3)
!
! C - The difference of vectors A and B (A - B).
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
      INTEGER*2 I
!
! I - Loop index
!
! 4.  HISTORY
!   WHO           WHEN   WHAT
! Dale Markham   770119  Created
! Peter Denatale 770718  ????
!
! 5.  VECSB PROGRAM STRUCTURE
!
      DO  I = 1,3
           C(I) = A(I) - B(I)
      ENDDO
!
      RETURN
      END
