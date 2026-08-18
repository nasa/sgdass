      FUNCTION DOTP ( A, B )
      IMPLICIT NONE
!
! 1.  DOTP PROGRAM SPECIFICATION
!
! 1.1 Form the dot product of two 3-dimensional vectors.
!
! 1.2 REFERENCES:
!
! 2.  DOTP INTERFACE
!
! 2.1 Parameter File
!
! 2.2 INPUT Variables:
!
      REAL*8 A(3),B(3)
!
! A - First vector
! C - Second vector
!
! 2.3 OUTPUT Variables:
!
      REAL*8 DOTP
!
! DOTP - The dot product between vectors A and B
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
!     WHO         WHEN   WHAT
! Dale Markham   770119  Created
! Peter Denatale 770718  ??
!
! 5. DOTP PROGRAM STRUCTURE
!
      DOTP = A(1) * B(1) &
     &     + A(2) * B(2) &
     &     + A(3) * B(3)
!
      RETURN
      END
