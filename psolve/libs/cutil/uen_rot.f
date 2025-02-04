      SUBROUTINE UEN_ROT(XYZ,MAT)
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
! 1.  UEN_ROT PROGRAM SPECIFICATION
!
! 1.1 Make a rotation matrix to convert from XYZ frame
!     to local UEN frame.
!
! 1.2 REFERENCES:
!
! 2.  UEN_ROT INTERFACE
!
! 2.1 Parameter File
!
! 2.2 INPUT Variables:
!
      REAL*8 XYZ(3)
!
! XYZ - Geocentric coordinates of local UEN coordinate
!
! 2.3 OUTPUT Variables:
!
      REAL*8 MAT(3,3)
!
! MAT - Rotation matrix
!
! 2.4 COMMON BLOCKS USED
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES:
!       CALLED SUBROUTINES: plh,rotat
!
! 3.  LOCAL VARIABLES
!
      INTEGER*2 I,J,K
      REAL*8 PHI,ELON,H,TEMP(3,3),TEMP2(3,3)
!
! I,J,K - Loop indices
! ELON - East longitude
! H - Height above ellipsoidal earth
! PHI - Geodetic latitude
! TEMP,TEMP2 - Temporary arrays
!
! 4.  HISTORY
!   WHO   WHEN   WHAT
!
! 5.  UEN_ROT PROGRAM STRUCTURE
!
! Convert X,Y,Z to latitude,longitude and height
!
      CALL PLH(XYZ,PHI,ELON,H )
!
! Form rotation matrices about longitude and latitude axes
!
      CALL ROTAT( ELON, INT2(3), TEMP )
      CALL ROTAT( -PHI, INT2(2), TEMP2 )
!
! Form the combined rotation matrix
!
      DO I=1,3
        DO J=1,3
          MAT(I,J)=0.0D0
          DO K=1,3
            MAT(I,J)=MAT(I,J)+TEMP2(I,K)*TEMP(K,J)
          ENDDO
        ENDDO
      ENDDO
!
      RETURN
      END
