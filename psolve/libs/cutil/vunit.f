      SUBROUTINE VUNIT ( A, B )
      IMPLICIT NONE
!
! 1.  VUNIT PROGRAM SPECIFICATION
!
! 1.1 Normalize a vector to make a unit vector.
!
! 1.2 REFERENCES:
!
! 2.  VUNIT INTERFACE
!
! 2.1 Parameter File
!
! 2.2 INPUT Variables:
!
      REAL*8 A(3)
!
! A - The unnormalized vector
!
! 2.3 OUTPUT Variables:
!
      REAL*8 B(3)
!
! B - The normalized unit vector
!
! 2.4 COMMON BLOCKS USED
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES:
!       CALLED SUBROUTINES: vecmg
!
! 3.  LOCAL VARIABLES
!
      INTEGER*2 I
      REAL*8 VECMG,AMAG
!
! AMAG - Magnitude of the vector
! I - Loop index
!
! 5.  VUNIT PROGRAM STRUCTURE
!
      AMAG = VECMG ( A )
      DO I = 1,3
           B(I) = A(I) / AMAG
      ENDDO
!
      RETURN
      END  !#!  VUNIT  #!#
