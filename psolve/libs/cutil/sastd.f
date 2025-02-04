      SUBROUTINE SASTD ( P, PDOT, RLAT, SITEHT, ZD, ZDDOT )
      IMPLICIT NONE
!
! 1.  SASTD PROGRAM SPECIFICATION
!
! 1.1 Calculate zenith delay for "hydrostatic" component of the
!     atmosphere using Saastamoinen formulation and constants from
!     Davis et al.
!
! 1.2 REFERENCES:
!       1) Saastamoinen, J. "The Use of Artificial Satellites for
!          Geodesy", Geophys. Monograph Series, vol. 15, ed. by
!          S.W.Henriksen et al, AGU, Wash,D.C.,pp 247-251,1972.
!       2) Davis, J.L., et al., "Geodesy by radio interferometry:
!          Effects of atmospheric modeling errors on estimates of
!          baseline length", Radio Science,20,1593-1607,1985.
!
! 2.  SASTD INTERFACE
!
! 2.1 Parameter File
!
! 2.2 INPUT Variables:
!
      REAL*8 P,PDOT,RLAT,SITEHT
!
! P,PDOT - Pressure, time rate of change of pressure
! RLAT - Latitude in radians
! SITEHT - Height of station above geoid, in meters
!
! 2.3 OUTPUT Variables:
!
      REAL*8 ZD,ZDDOT
!
! ZD - Zenith delay of "hydrostatic" component, in meters
! ZDDOT - Rate of change of zenith delay, in meters/second
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
      REAL*8 F
!
! F - Intermediate result in zenith delay calculation
!
! 4.  HISTORY
!   WHO   WHEN   WHAT
! JMG,AEN ???    Created
!
! 5.  SASTD PROGRAM STRUCTURE
!
!
!-----Calculate variation of gravity with station position.
! ON ENTRY
!
!     SITEHT = height of station above geoid in METERS.
!     RLAT    = LATITUDE   in radians
!     P,PDOT = PRESSURE, TIME RATE OF CHANGE OF PRESSURE
!
! ON EXIT
!     ZD = ZENITH DELAY OF "HYDROSATIC" COMPONENT IN METERS
!     ZDDOT = RATE OF CHANGE IN METERS/SECOND
!
!
      F = 1.0D0-0.00266D0*COS(2.0D0*RLAT) - 0.00028D0*SITEHT/1000.0D0
!
      ZD = 0.0022768D0*P/F
!
      ZDDOT = ZD*PDOT/P
!
      RETURN
      END  !#!  SASTD  #!#
