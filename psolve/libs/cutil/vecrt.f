      SUBROUTINE VECRT ( A, V, RV )
      IMPLICIT NONE
!
! 1.  VECRT PROGRAM SPECIFICATION
!
! 1.1 Rotate a vector.
!
! 1.2 REFERENCES:
!
! 2.  VECRT INTERFACE
!
! 2.1 Parameter File
!
! 2.2 INPUT Variables:
!
      REAL*8 A(3,3),V(3)
!
! A - The rotation matrix for performing the coordinate system rotation
! V - The vector being rotated
!
! 2.3 OUTPUT Variables:
!
      REAL*8 RV(3)
!
! RV - The rotated vector
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
! 4. HISTORY
!   WHO          WHEN   WHAT
! Dale Markham   770119 Created
! Peter Denatale 770718 ????
!
! 5.  VECRT PROGRAM STRUCTURE
!
      DO I = 1,3
           RV(I) = A(I,1) * V(1) &
     &           + A(I,2) * V(2) &
     &           + A(I,3) * V(3)
      ENDDO
!
      RETURN
      END
