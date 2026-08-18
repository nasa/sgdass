      SUBROUTINE ROTAT ( THETA, I, R )
      IMPLICIT NONE
!
! 1.  ROTAT PROGRAM SPECIFICATION
!
! 1.1 Form a rotation matrix.
!
! 1.2 REFERENCES:
!
! 2.  ROTAT INTERFACE
!
! 2.1 Parameter File
!
! 2.2 INPUT Variables:
!
      INTEGER*2 I
      REAL*8 THETA
!
! I - The integer flag which determines the rotation axis.
!     (I = 1,2,3 refers respectively to rotation about
!     the X, Y and Z axes).
! THETA - The angle through which the coordinate system rotation
!         is performed (radians).
!
! 2.3 OUTPUT Variables:
!
      REAL*8 R(3,3)
!
! R - The 3X3 rotation matrix which performs the coordinate
!     system rotation (unitless).
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
      REAL*8 C,S
!
! C - Cosine of THETA
! S - Sine of THETA
!
! 4.  HISTORY
!   WHO            WHEN   WHAT
! Dale Markham    770119  Created
! Peter Denatale  770718  ???
!
! 5.  ROTAT PROGRAM STRUCTURE
!
!   1.    COMPUTE THE COSINE AND THE SINE OF THE ROTATION ANGLE THETA.
!
      C = DCOS ( THETA )
      S = DSIN ( THETA )
!
!  2.     DETERMINE THE AXIS OF ROTATION.
!
      IF ( I .EQ. 1 )  GO TO 300
      IF ( I .EQ. 2 )  GO TO 400
      IF ( I .EQ. 3 )  GO TO 500
!
!   3.    ROTATION ABOUT THE X-AXIS.
!
!          ( 1  0  0 )
!   R(X) = ( 0  C  S )
!          ( 0 -S  C )
!
300   CONTINUE
      R(1,1) = 1.D0
      R(2,1) = 0.D0
      R(3,1) = 0.D0
      R(1,2) = 0.D0
      R(2,2) = +C
      R(3,2) = -S
      R(1,3) = 0.D0
      R(2,3) = +S
      R(3,3) = +C
!
      GO TO 700
!
!
!   4.    ROTATION ABOUT THE Y-AXIS.
!
!          ( C  0 -S )
!   R(Y) = ( 0  1  0 )
!          ( S  0  C )
!
400   CONTINUE
      R(1,1) = +C
      R(2,1) = 0.D0
      R(3,1) = +S
      R(1,2) = 0.D0
      R(2,2) = 1.D0
      R(3,2) = 0.D0
      R(1,3) = -S
      R(2,3) = 0.D0
      R(3,3) = +C
!
      GO TO 700
!
!
!   5.    ROTATION ABOUT THE Z-AXIS.
!
!          ( C  S  0 )
!   R(Z) = (-S  C  0 )
!          ( 0  0  1 )
!
500   CONTINUE
      R(1,1) = +C
      R(2,1) = -S
      R(3,1) = 0.D0
      R(1,2) = +S
      R(2,2) = +C
      R(3,2) = 0.D0
      R(1,3) = 0.D0
      R(2,3) = 0.D0
      R(3,3) = 1.D0
!
700   CONTINUE
      RETURN
      END
