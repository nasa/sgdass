      SUBROUTINE gradrat(AZ,ELEV,LAT,HGT,PI,gradot)
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
      INCLUDE   'solve.i'
      INCLUDE   'glbc4.i'
!-------------------------------------------------
!
! 1.  GRADRAT PROGRAM SPECIFICATION (modified from elrat.f)
!
! 1.1 Calculate the rate gradient mapping functions for a station from the
!     source's elevation,azimuth (and rates) and the station's geodetic
!     latitude.  The azimuth and elevation rates are found from the
!     the following expressions for the site uen components:
!
!     u =  SIN(EL) = SIN(DEC)*SIN(LAT) + COS(DEC)*COS(LAT)*COS(HA)     (1)
!
!     e =  SIN(AZ)*COS(EL)= -SIN(HA)COS(DEC)                           (2)
!
!     n =  cos(az)cos(el)=-sin(lat)cos(dec)cos(ha)+cos(lat)sin(dec)    (3)
!
!     The first equation can be differentiated with respect to time to give:
!
!       COS(EL)*ELDOT = - COS(DEC)*COS(LAT)*SIN(HA)*HADOT           (4)
!
!     which can be solved for ELDOT (elevation rate).  Substituting
!     SIN(HA) into equation 4 and cancelling some terms, the formula for
!     ELDOT is
!
!       ELDOT = COS(LAT)*SIN(AZ)*HADOT                              (5)
!
!     The elevation and azimuth come from the OBSFIL.  The geodetic
!     latitude comes from a table based on the CDP catalog of site
!     information, the Geodetic Survey of Canada, and the 5/8/85 NASA
!     directory of station locations. Source declination is the a priori
!     from prfil.i.
!
!     The rate gradient mapping functions for the north and east components,
!     [cos(az)cot(el)m(el),sin(az)cot(el)m(el)], m(el)=mtt dry mapping fnc.,
!     are calculated for use in partl.f to compute the delay rate gradient
!     partials.
!
! 1.2 REFERENCES:
!
! 2.  ELRAT INTERFACE
!
! 2.1 Parameter File
!
! 2.2 INPUT Variables:
!
      REAL*8 PI,LAT,HGT
      REAL*8 AZ,ELEV
!
! AZ - Source azimuth (rad)
! ELEV - Source elevation (rad)
! DEC - Source declination (rad)
! LAT - Station's geodetic latitude (rad)
! HGT - station's height above ellipsoid (m)
! PI - PI in double precision
!
! 2.3 OUTPUT Variables:
!
      REAL*8 gradot(2)   !north and east components of rate gradient mapping
!function
!
!
!
!
! 2.4 COMMON BLOCKS USED
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES:
!       CALLED SUBROUTINES: dfuzz
!
! 3.  LOCAL VARIABLES
!
      REAL*8 HADOT,eldot,azimd,azim1,azim2
      real*8 mtt,mttr,deftemp
      real*8 azdot1,azdot2,azimdot
      LOGICAL*2 DFUZZ
      CHARACTER*190 errstr
      character*80 bufstr
!
! HADOT - Hour angle rate (2*PI radians / sidereal day)
!
! 4.  HISTORY
!   WHO   WHEN   WHAT
!   KDB  870711  Created
!   AEE  910515  Enhanced error messages written to the error file.
!   DSM  940215  adapted from elrat.f
!
! 5.  ELRAT PROGRAM STRUCTURE
!
!     IF THE ELEVATION IS 90 DEGREES, THE AZIMUTH AND EQUATION WILL BE
!     UNDEFINED.  HOWEVER, ELDOT IS KNOWN TO BE ZERO, AT THAT POINT.
!
      If ( G_WARNING .AND. &
           ( ELEV.lt.0        .or. ELEV.gt.PI/2.   .or. &
     &       AZ.lt.(-2.*PI) .or.   AZ.gt.2.*PI     .or. &
     &      LAT.lt.(-PI/2.) .or.  LAT.gt.PI/2.          )  ) then
       Write(errstr,'("Azimuth or Elevation or Latitude out of range ", &
     &             " AZ   = ",F20.2, &
     &             " ELEV = ",F20.2, &
     &             " LAT  = ",F20.2)') AZ,ELEV,LAT
       call ferr( INT2(170), errstr, INT2(0), INT2(0) )
!        call ferr(170,'Az or el or lat out of range',0,0)
      Endif
!
      IF (DFUZZ(DBLE(ELEV),PI/2.0D0)) THEN  ! ELEV IS 90 DEG (PI/2 RAD)
        ELDOT = 0.0D0
      ELSE
        HADOT = 2.0D0*PI/86164.1D0   ! SECONDS IN A SIDEREAL DAY
        ELDOT = DCOS(LAT)*DSIN(AZ)*HADOT   ! rad/s
      END IF
          deftemp=15.d0
          call mttdry(elev,deftemp,eldot,lat,hgt,mtt,mttr,.FALSE. )
!
! ------- Compute time derivative of north and east gradient partials
! ------- Azimdot (=azimd/cos(el)) was computed from eqn. 2-4
!
          AZIMD = HADOT*(DSIN(LAT)*DCOS(ELEV)-DCOS(LAT)*DCOS(AZ)*DSIN(ELEV))
!
! ------- NORTH Component
!
          gradot(1) = -azimd*dsin(az)/dsin(elev)*mtt- &
     &               eldot*dcos(az)/(dsin(elev))**2*mtt+ &
     &               dcos(az)/dtan(elev)*mttr
!
! ------- EAST Component
!
          gradot(2) = azimd*dcos(az)/dsin(elev)*mtt- &
     &               eldot*dsin(az)/(dsin(elev))**2*mtt+ &
     &               dsin(az)/dtan(elev)*mttr
!
      RETURN
      END
