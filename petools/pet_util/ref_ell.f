        SUBROUTINE REF_ELL ( IPAR, R, PHI_GCN, PHI_GDT, LAMBDA, H_ELL, &
     &                       RD, G_ACC )
! ************************************************************************
! *                                                                      *
! *     Routine  REF_ELL  computes geocentric latitude, geodetic latitude, *
! *   longitude, height above the reference ellipsoid, the distance to   *
! *   the geocenter and local gravity acceleration using Cartesian       *
! *   vector of station coordinates in Crust fixed reference frame.      *
! *                                                                      *
! *     Geodetic latitude is computed with using non-iterative Bowring   *
! *   algorithm ( Bowring, B.R., 1985, "The Accuracy of Geodetic         *
! *   Latitude and Height Equations", Survey Review, Vol. 28, pp.        *
! *   202-206 ) recommended by IERS Standards, 1992 p.18-19. Local       *
! *   gravity acceleration is computed according to the formula of       *
! *   Geodetic Reference System 1967 -- B. Emerson, G.A.Wilkins          *
! *   "Proceeding of IAU Colloquium N9, The IAU system of Astronomical   *
! *   Constants, Heidelberg, 12-14 august 1970" -- IN Celestial          *
! *   Mechanics Vol. 4, N2, 1971, p.128-149. (NB: This formula is        *
! *   adopted by Calc). All constants taken from IERS Standards, 1992 .  *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *    IPAR ( INTEGER*4 ) -- Mode:                                       *
! *           IPAR=0 -- recommended mode.                                *
! *           IPAR=1 -- dependence of gravity acceleration on height is  *
! *                     not taken into account. Therefore, gravity       *
! *                     acceleration is referred to the ellipsoid.       *
! *       R ( REAL*8    ) -- Cartesian geocentric vector of station      *
! *                          coordinates in a crust-fixed reference      *
! *                          system. Units: m.                           *
! *                                                                      *
! * _______________________ Output parameters: _________________________ *
! *                                                                      *
! * PHI_GCN ( REAL*8    ) -- Geocentric latitude positive towards north  *
! *                          in rad in range [-pi/2, pi/2].              *
! * PHI_GDT ( REAL*8    ) -- Geodetic latitude positive towards north    *
! *                          in rad in range [-pi/2, pi/2].              *
! *  LAMBDA ( REAL*8    ) -- Longitude positive towards east in rad,     *
! *                          in range [0, pi2).                          *
! *   H_ELL ( REAL*8    ) -- Height above the reference ellipsoid.       *
! *                          Units: m.                                   *
! *      RD ( REAL*8    ) -- distance to geocenter. Units: m.            *
! *   G_ACC ( REAL*8    ) -- Local gravity acceleration. Units: m/s**2   *
! *                                                                      *
! *  ###   26-NOV-1993   REF_ELL   v 3.1  (c) L. Petrov  22-AUG-2002 ###   *
! *                                                                      *
! ************************************************************************
        IMPLICIT   NONE
        INTEGER*4  IPAR
        REAL*8     R(3), PHI_GCN, PHI_GDT, LAMBDA, H_ELL, RD, G_ACC
        REAL*8     P, MU, PHI, GRV_HEI
!
        REAL*8     REA, FE, EXC_SQ, ACC_EQU, GRV_LAT, GRV_H
!
! ----- Constants
!
        PARAMETER  ( REA     = 6378137.0D0 )       ! Earth's equatorial radius
        PARAMETER  ( FE      = 1.D0/298.257D0 )    ! Earth's flattening
        PARAMETER  ( EXC_SQ  = 2.D0*FE - FE**2 )   ! Earth's eccentricity
        PARAMETER  ( ACC_EQU = 9.7803184558D0 )    ! Equatorial gravity acc.
        PARAMETER  ( GRV_H   = -2.D0*ACC_EQU/REA ) ! D(ACC_EQU)/DH
        PARAMETER  ( GRV_LAT = 0.001931663  )      ! D(ACC_EQU)/D(phi)
        REAL*8      PI, PI2, P2I
        PARAMETER ( PI=3.141592653589793D0, PI2=2.D0*PI, P2I=PI/2D0 ) ! Pi
!
! ----- Computation of longitude
!
        IF ( DABS ( R(1) ) .GT. 1.D-8 ) THEN
             LAMBDA= DATAN ( R(2)/R(1) )
          ELSE
             LAMBDA= P2I
        END IF
        IF ( R(1)   .LT. 0.D0 ) LAMBDA=PI  + LAMBDA
        IF ( LAMBDA .LT. 0.D0 ) LAMBDA=PI2 + LAMBDA
!
        P =DSQRT ( R(1)**2 + R(2)**2 )
        IF ( DABS(P) .LT. 1.D-8 ) P=1.D-8
        RD=DSQRT ( P**2    + R(3)**2 )
!
! ----- Computation of geocentric latitude
!
        PHI_GCN=DATAN( R(3)/P )
!
! ----- Comutation of geodetic latitude
!
        MU=DATAN ( R(3)/P * ( (1.D0 - FE) + EXC_SQ*REA/RD ) )
!
        PHI_GDT=DATAN( ( (1.D0 - FE)*R(3) + EXC_SQ*REA*DSIN(MU)**3 ) / &
     &                 ( (1.D0 - FE)*( P  - EXC_SQ*REA*DCOS(MU)**3 )) )
!
! ----- Computation of height above REF_ELL
!
        H_ELL = P*DCOS(PHI_GDT) + R(3)*DSIN(PHI_GDT) - &
     &          REA*DSQRT( 1.D0 - EXC_SQ*DSIN(PHI_GDT)**2 )
!
! ----- Commputation of local gravity accelrartion
!
        IF ( IPAR.EQ.0  ) THEN
               PHI = PHI_GCN
               GRV_HEI=GRV_H
          ELSE IF ( IPAR.EQ.1  ) THEN
               PHI = PHI_GCN
               GRV_HEI=0.D0
        END IF
!
        G_ACC=ACC_EQU* (1.D0 + GRV_LAT* DSIN(PHI)**2 + GRV_HEI*H_ELL) / &
     &          DSQRT  (1.D0 - EXC_SQ*  DSIN(PHI)**2)
!
        RETURN
        END  !#!  REF_ELL  #!#
