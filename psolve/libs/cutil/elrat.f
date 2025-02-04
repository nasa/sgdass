      SUBROUTINE ELRAT(AZ,ELEV,LAT,PI,ELDOT)
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
      INCLUDE   'solve.i'
      INCLUDE   'glbc4.i'
!
! 1.  ELRAT PROGRAM SPECIFICATION
!
! 1.1 Calculate the elevation rate of a source for a station from the
!     source's azimuth and hour angle rate, and the station's geodetic
!     latitude.  The calculation is based on the following equations:
!
!       SIN(EL) = SIN(DEC)*SIN(LAT) + COS(DEC)*COS(LAT)*COS(HA)     (1)
!
!       SIN(HA) = - SIN(AZ)*COS(EL)/COS(DEC)                        (2)
!
!     The first equation can be differentiated with respect to time to give:
!
!       COS(EL)*ELDOT = - COS(DEC)*COS(LAT)*SIN(HA)*HADOT           (3)
!
!     which can be solved for ELDOT (elevation rate).  Substituting
!     SIN(HA) into equation 3 and cancelling some terms, the formula for
!     ELDOT is
!
!       ELDOT = COS(LAT)*SIN(AZ)*HADOT                              (4)
!
!     The elevation and azimuth come from the OBSFIL.  The geodetic
!     latitude comes from a table based on the CDP catalog of site
!     information, the Geodetic Survey of Canada, and the 5/8/85 NASA
!     directory of station locations.
!
! 1.2 REFERENCES:
!
! 2.  ELRAT INTERFACE
!
! 2.1 Parameter File
!
! 2.2 INPUT Variables:
!
      REAL*8 PI,LAT
      REAL*8 AZ,ELEV
!
! AZ - Source azimuth
! ELEV - Source elevation
! LAT - Station's geodetic latitude
! PI - PI in double precision
!
! 2.3 OUTPUT Variables:
!
      REAL*8 ELDOT
!
! ELDOT - Elevation rate
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
      REAL*8 HADOT
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
!   BA   950410  Fixed message so it is just printed as a warning.
!
! 5.  ELRAT PROGRAM STRUCTURE
!
!     IF THE ELEVATION IS 90 DEGREES, THE AZIMUTH AND EQUATION WILL BE
!     UNDEFINED.  HOWEVER, ELDOT IS KNOWN TO BE ZERO, AT THAT POINT.
!
      IF ( G_WARNING .AND.                              &
     &     ( ELEV.lt.0        .or. ELEV.gt.PI/2.   .or. &
     &       AZ.lt.(-2.*PI) .or.   AZ.gt.2.*PI     .or. &
     &      LAT.lt.(-PI/2.) .or.  LAT.gt.PI/2.          ) ) then
       Write(6,'("Azimuth or Elevation or Latitude out of range ", &
     &             " AZ   = ",F20.2, &
     &             " ELEV = ",F20.2, &
     &             " LAT  = ",F20.2)') AZ,ELEV,LAT
       write(6,"('*** WARNING *** WARNING *** WARNING ***')")
       write(6,"('(message from cutil/elrat, execution continues)')")
!      call ferr(170,errstr,0,0)
!        call ferr(170,'Az or el or lat out of range',0,0)
      Endif
!
      IF (DFUZZ(DBLE(ELEV),PI/2.0D0)) THEN  ! ELEV IS 90 DEG (PI/2 RAD)
        ELDOT = 0.0D0
      ELSE
        HADOT = 2.0D0*PI/86164.1D0   ! SECONDS IN A SIDEREAL DAY
        ELDOT = DCOS(LAT)*SIN(AZ)*HADOT
      END IF
!
      RETURN
      END
