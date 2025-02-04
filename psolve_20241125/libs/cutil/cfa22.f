      SUBROUTINE CFA22( &
     & EL,P,RH,TC, &
     & ELDOT,PDOT,RHDOT,TCDOT, &
     & RLAT,SITHT,TROPHT,BETA, &
     & CFAMAP,CFARAT)
!
      IMPLICIT NONE
!
! 1.  CFA22 PROGRAM SPECIFICATION
!
! 1.1 Calculate the CFA mapping function and its time derivative
!
! 1.2 REFERENCES:
!
! 2.  CFA22 INTERFACE
!
! 2.1 Parameter File
!
! 2.2 INPUT Variables:
      real*8 el,p,rh,tc,eldot,pdot,rhdot,tcdot,rlat,sitht,tropht,beta
!
! BETA - Lapse rate
! EL,ELDOT - Elevation in radians, time derivative of elevation in seconds
! ILUIN - Input LU
! P,PDOT - Pressure, time derivative of pressure
! RH,RHDOT - Relative humidity, time derivative of RH
! RLAT - Latitude of site (radians)
! SITHT - Elevation above geod in km
! TC,TCDOT - Temperature in Centigrade, time derivative of temperature
! TROPHT - Height of troposphere, in km
!
! 2.3 OUTPUT Variables:
!
      real*8 cfamap,cfarat
!
! CFAMAP - CFA mapping function
! CFARAT - Time rate of change of mapping function
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
      real*8 temp,esat,esatdot,enot,enotdot,dp,dt,dbeta,dht
      real*8 am,amdot,bm,bmdot,cm,cmdot,sinel,tanel
      real*8 x1,x2,x3,cosel,x1dot,x2dot,x3dot
      REAL*8 A(6),B(6)
      DATA A/ &
     &       0.1185D-2, &
     &       0.6070D-4, &
     &      -0.1471D-3, &
     &       0.3072D-2, &
     &       0.1965D-1, &
     &      -0.5645D-2/
      DATA B/ &
     &       0.1144D-2, &
     &       0.1164D-4, &
     &       0.2795D-3, &
     &       0.3109D-2, &
     &       0.3038D-1, &
     &      -0.1217D-1/
!
! COSEL - Cosine of elevation
! DHT - Height of troposphere relative to 11.231 km
! DP - Pressure relative to 1000 mbar
! DT - Temperature relative to 20 deg C
! ENOT,ENOTDOT -
! SINEL - Sine of elevation
! TANEL - Tangent of elevation
!
! 4.  HISTORY
!   WHO   WHEN   WHAT
!   aen   ????   Created
!   jmg   ????   Added rates
!
! 5.  CFA22 PROGRAM STRUCTURE
!
!-----Calculate mapping coefficients:
!
!     The saturation vapor pressure is calculated using the formula
!     found in MET03 which is much simpler than that found in Methods
!     of Experimental Physics B (1976) p. 187, but agrees to 0.3%
!     over the range 0 - 40 deg. C.
!
      TEMP = TC+237.3D0   !CHANGED FROM 273.3 ON 9-08-87
      ESAT = 6.11D0 * EXP(17.269D0*TC/TEMP)
      ESATDOT = ESAT * (17.269D0/(TEMP) - 17.269D0*TC/(TEMP*TEMP))*TCDOT
!
      ENOT = ESAT*RH
      ENOTDOT = ESATDOT*RH + ESAT*RHDOT
!
      DP = P - 1000.D0
      DT = TC - 20.D0
      DBETA = BETA*1.D3 + 6.50D0
      DHT = TROPHT - 11.231D0
!
      AM =    A(1) * (1.0D0 + A(2)*DP + A(3)*ENOT &
     &        + A(4)*DT + A(5)*DBETA + A(6)*DHT)
!
      AMDOT = A(1) * ( A(2)*PDOT + A(3)*ENOTDOT + A(4)*TCDOT)
!
!
      BM =    B(1) * (1.0D0 + B(2)*DP + B(3)*ENOT &
     &         + B(4)*DT + B(5)*DBETA + B(6)*DHT)
!
      BMDOT = B(1) * ( B(2)*PDOT + B(3)*ENOTDOT + B(4)*TCDOT)
!
      CM = -0.0090D0
      CMDOT = 0.0D0
!
!-----Calculate mapping function.
!
      SINEL = SIN(EL)
      X1 = SINEL + CM
      TANEL = TAN(EL)
      X2 = TANEL + BM/X1
      X3 = SINEL + AM/X2
      CFAMAP = 1.0D0/X3
!
      COSEL = COS(EL)
!
! NOW CALCULATE TIME DERIVATIVE OF MAPPING FUNCITON
!
      X1DOT = COSEL*ELDOT + CMDOT
      X2DOT = ELDOT/(COSEL*COSEL) + BMDOT/X1 - BM*X1DOT/(X1*X1)
      X3DOT = COSEL*ELDOT + AMDOT/X2 - AM*X2DOT/(X2*X2)
!
      CFARAT = -X3DOT/(X3*X3)
!
      RETURN
      END
