      SUBROUTINE XYZ_TO_HLP ( SPD, INP_VEC, OUT_VEC )
! ************************************************************************
! *                                                                      *
! *   Routine  XYZ_TO_HLP transfroms vectro in carthesian coordinate     *
! *   system XYZ to the vectror in HLP ellipsoidal system: height above  *
! *   the reference ellipsoid, longitude, geodetic latitude.             *
! *                                                                      *
! *  ### 25-FEB-2008   XYZ_TO_HLP  v1.1 (c)  L. Petrov  2014.02.16  ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'spd.i'
      TYPE     ( SPD_3D__TYPE ) :: SPD
      REAL*8     INP_VEC(3), OUT_VEC(3)
      REAL*8     REA, FE, EXC_SQ
!
! --- Constants
!
      PARAMETER  ( REA    = SPD__REA_WGS84  ) ! Earth's equatorial radius
      PARAMETER  ( FE     = SPD__FLAT_WGS84 ) ! Earth's flattening
      PARAMETER  ( EXC_SQ = 2.D0*FE - FE**2 ) ! Earth's eccentricity
      REAL*8     LAMBDA, P, RD, PHI_GCN, MU, PHI_GDT, H_ELL
!
! --- Computation of longitude
! 
      IF ( DABS ( INP_VEC(1) ) .GT. 1.D-8 ) THEN
           LAMBDA= DATAN ( INP_VEC(2)/INP_VEC(1) )
        ELSE
           LAMBDA= P2I
      END IF
      IF ( INP_VEC(1) .LT. 0.D0 ) LAMBDA = LAMBDA + PI__NUM
      IF ( LAMBDA .LT. 0.0D0 ) LAMBDA = LAMBDA + PI2
!
      P = DSQRT ( INP_VEC(1)**2 + INP_VEC(2)**2 )
      IF ( DABS(P) .LT. 1.D-8 ) P=1.D-8
      RD = DSQRT ( P**2    + INP_VEC(3)**2 )
!
! --- Computation of the geocentric latitude
!
      PHI_GCN = DATAN( INP_VEC(3)/P )
!
! --- Computation of the geodetic latitude
!
      MU = DATAN ( INP_VEC(3)/P * ( (1.D0 - FE) + EXC_SQ*REA/RD ) )
!
      PHI_GDT = DATAN( ( (1.D0 - FE)*INP_VEC(3) + EXC_SQ*REA*DSIN(MU)**3 ) / &
     &                 ( (1.D0 - FE)*( P  - EXC_SQ*REA*DCOS(MU)**3 )) )
!
! --- Computation of the height above reference ellipsoid
!
      H_ELL = P*DCOS(PHI_GDT) + INP_VEC(3)*DSIN(PHI_GDT) - &
     &        REA*DSQRT( 1.D0 - EXC_SQ*DSIN(PHI_GDT)**2 )
!      
      OUT_VEC(1) = H_ELL
      OUT_VEC(2) = LAMBDA
      OUT_VEC(3) = PHI_GDT 
!
      RETURN
      END  SUBROUTINE  XYZ_TO_HLP  !#!  
