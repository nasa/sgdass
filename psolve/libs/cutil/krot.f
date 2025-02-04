      SUBROUTINE KROT(X,Y,Z,VU,VE,VN,VX,VY,VZ)!KJC ECCENT <881007.2023>
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
! 1.  KROT PROGRAM SPECIFICATION
!
! 1.1 Transform a vector in local N-E-U coordinates at a point X,Y,Z
!     into geocentric coordinates.
!
! 1.2 REFERENCES:
!
! 2.  KROT INTERFACE
!
! 2.1 Parameter File
!
! 2.2 INPUT Variables:
!
      REAL*8 X,Y,Z,VU,VE,VN
!
! X,Y,Z - Geocentric coordinates of local N-E-U coordinate system
! VU,VE,VN - Components of vector in N-E-U frame
!
! 2.3 OUTPUT Variables:
!
      REAL*8 VX,VY,VZ
!
! VX,VY,VZ - Components of vector in geocentric frame
!
! 2.4 COMMON BLOCKS USED
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES:
!       CALLED SUBROUTINES: rotat,vecrt
!
! 3.  LOCAL VARIABLES
!
      REAL*8 STAP(3),PHI,ELON,H
      REAL*8 R(3,3),RV(3),V(3),RV2(3)
!
! 4.  HISTORY
!   WHO   WHEN   WHAT
!   KJC  830624  Created
!
! 5.  KROT PROGRAM STRUCTURE
!
      STAP(2) = Y
      STAP(3) = Z
      STAP(1) = X
!
      V(1) = VU
      V(2) = VE
      V(3) = VN
!
!  Convert geocentric coordinates to  geodetic latitude, east longitude
!   and height above the ellipsoidal earth.
!
      CALL PLH(STAP,PHI,ELON,H )
!
!  Form rotation matrix for rotation about Y axis by size of latitude
!   and rotate the NEU vector
!
          CALL ROTAT( PHI, INT2(2), R )
          CALL VECRT(R,V,RV )
!
!  Form rotation matrix for rotation about Z axis by amount of east
!   longitude, and rotate the vector
!
          CALL ROTAT( -ELON, INT2(3), R )
          CALL VECRT(R,RV,RV2 )
!
          VX = RV2(1)
          VY = RV2(2)
          VZ = RV2(3)
!
          RETURN
          END
