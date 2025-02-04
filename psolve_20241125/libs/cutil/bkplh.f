!$bkplh
!
! NAME             bkplh.f
!
! VERSION          93.01.27
!
! WRITTEN          B. Archinal, USNO, July 20-23, 1990.
!                  Name changed from "borkow" to "bkplh", and arguments
!                  adapted for efficient use by Calc (csitm), Dbcal,
!                  and Solve.  BA, 93.01.27.
!
! PURPOSE          Converts XYZ coordinates to Phi, Lambda, H
!                  ellipsoidal coordinates.
!
! References       Borkowski, K. M. (1989).  "Accurate Algorithms to
!                  transform geocentric to geodetic coordinates"
!                  *Bulletin Geodesique*, v. 63, pp. 50-56.  Also see
!                  Borkowski, K. M. (1987).  "Transformation of
!                  Geocentric to Geodetic Coordinates without
!                  Approximations", *Astrophysics and Space Science*,
!                  v. 139, n. 1, pp. 1-4.  Correction in (1988), v. 146,
!                  n. 1, p. 201.
!
! Note             Will not work for points on the Z axis, i.e. if
!                  if X=Y=0 (Phi = +/- 90 degrees).
!
! Calling sequence CALL bkplh ( XYZ, PLH, PI, A, FL )
!
! ARGUMENT LIST
!
!  PARM       TYPE DESCRIPTION
!
!  XYZ(3)     D    INPUT - XYZ Cartesian coordinates of point.
!                  XYZ(1) and XYZ(2) must not both be zero.  Units are
!                  those of A below.
!  PLH(3)     D    OUTPUT - Ellipsoidal coordinates of point, in
!                  geodetic latitude, longitude, and height.  Units
!                  for latitude and longitude are in radians, units
!                  of height are those of A below.
!  PI         D    INPUT - Ratio of circumference to diameter of circle.
!                  Unitless.
!  A          D    INPUT - Semi-major axis of ellipsoid.  Units are
!                  of distance (meters, kilometers, miles, etc.).
!  FL         D    INPUT - Flattening of ellipsoid.  Unitless.
!
!
! SUBPROGRAMS USED
!  Fortran         DABS      DACOS     DATAN     DATAN2   DCOS
!                  DSIN      DSQRT
!
! COMMON BLOCKS    None.
!
! INPUT            None.
!
! OUTPUT           None, unless diagnostic printout uncommented.
!
! LANGUAGE         Fortran 77.
!
!===================================================================
      SUBROUTINE bkplh(XYZ,PLH,PI,A,FL)
!
      IMPLICIT NONE
      DOUBLE PRECISION A,B,D &
     &                ,DABS,DACOS,DATAN,DATAN2,DCOS,DSIN,DSQRT &
     &                ,E,F,FL,G,P,PI,PLH,Q,R,T,V,X,XYZ,Y,Z,ZLONG
!     DOUBLE PRECISION CHK1, CHK2
!     INTEGER IOUT
      DIMENSION XYZ(3),PLH(3)
!--- XYZ.
      X=XYZ(1)
      Y=XYZ(2)
      Z=XYZ(3)
!--- Semi-minor axis.
      B=A*(1.D0-FL)
!--- Set sign of B to that of Z in order to get sign of Phi correct.
      IF(Z.LT.0.D0) B=-B
!--- Intermediate Values for Latitude.
      R=DSQRT(X*X+Y*Y)
      E=(B*Z-(A*A-B*B))/(A*R)
      F=(B*Z+(A*A-B*B))/(A*R)
      P=4.D0/3.D0 * (E*F+1)
      Q=2.D0 * (E*E - F*F)
      D=P*P*P+Q*Q
      IF(D.GE.0.D0) then
        V=(DSQRT(D)-Q)**(1.D0/3.D0) - (DSQRT(D)+Q)**(1.D0/3.D0)
        else
        V=2.D0 * DSQRT(-P) * DCOS (1.D0/3.D0 * &
     &  DACOS(Q/(P * DSQRT(-P))))
        endif
!   (Improve V - not really necessary except near axes.)
      IF(V*V.LT.DABS(P)) V=-(V*V*V + 2.D0*Q)/(3.D0*P)
      G=(DSQRT(E*E+V)+E)/2.D0
      T=DSQRT( G*G  + (F-V*G)/(2.D0*G-E) ) - G
      PLH(1)=DATAN( (A*(1.D0-T*T))/(2.D0*B*T) )
!--- HEIGHT.
      PLH(3)=(R-A*T)*DCOS(PLH(1)) + (Z-B)*DSIN(PLH(1))
!--- LONGITUDE.
      ZLONG=DATAN2(Y,X)
      IF(ZLONG.LT.0.D0) ZLONG=ZLONG+2.D0*PI
      PLH(2)=ZLONG
!
!   Diagnostic output.
!
!     IOUT=11
!     WRITE(IOUT,901) A,F,B
! 901 FORMAT(' A,F,B:',3D25.16)
!     WRITE(IOUT,902) X, Y, Z
! 902 FORMAT(' X, Y, Z:',3D25.16)
!     WRITE(IOUT,903) R,E,F
! 903 FORMAT(' R, E, F:',3D25.16)
!     WRITE(IOUT,904) P,Q,D
! 904 FORMAT(' P, Q, D:',3D25.16)
!     WRITE(IOUT,905) V,G,T
! 905 FORMAT(' V, G, T:',3D25.16)
!--- Check.
!     CHK1=T*T*T*T + 2.D0 * E *T*T*T + 2.D0 * F *T - 1.D0
!     CHK2=V*V*V + 3.D0*P*V + 2.D0*Q
!     WRITE(IOUT,906) CHK1,CHK2
! 906 FORMAT('Check values (=0):',2D25.16)
      RETURN
      END
