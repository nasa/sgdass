      SUBROUTINE SASTW(RH,TC,RHDOT,TCDOT,ZW,ZWDOT)
      IMPLICIT NONE
!
! 1.  SASTW PROGRAM SPECIFICATION
!
! 1.1 Calculate zenith delay due to 'wet' (non-hydrostatic) component
!     of atmosphere using Saastamoinen formula (19a) (Ref. 1).
!     The saturation vapor pressure is calculated using the formula
!     found in MET03 which is much simpler than that found in Methods
!     of Experimental Physics B (1976) p. 187, but agrees to 0.3%
!     over the range 0 - 40 deg. C.
!
! 1.2 REFERENCES:
!       1) Saastamoinen, J. "The Use of Artificial Satellites for
!          Geodesy", Geophys. Monograph Series, vol. 15, ed. by
!          S.W.Henriksen et al, AGU, Wash,D.C.,pp 247-251,1972.
!       2) Davis, J.L., et al., "Geodesy by radio interferometry:
!          Effects of atmospheric modeling errors on estimates of
!          baseline length", Radio Science,20,1593-1607,1985.
!
! 2.  SASTW INTERFACE
!
! 2.1 Parameter File
!
! 2.2 INPUT Variables:
!
      REAL*8 RH,RHDOT,TC,TCDOT
!
! RH - relative humidity (0 <= RH <= 1.0)
! RHDOT - Time derivative of RH
! TC - Temperature (Celsius)
! TCDOT - Time derivative of TC
!
! 2.3 OUTPUT Variables:
!
      REAL*8 ZW,ZWDOT
!
! ZW - Zenith path delay (meters) of wet component/VLIGHT
! ZWDOT - Time derivative of zenith delay/VLIGHT
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
      REAL*8 ESAT,TEMP,ESATDOT
!
! ESAT - Saturation vapor pressure
! ESATDOT - Time derivative of saturation pressure
! TEMP - Used for intermediate values in calculations
!
! 4. HISTORY
!   WHO   WHEN   WHAT
!
! 5.  SASTW PROGRAM STRUCTURE
!
!-----Calculate zenith delay due to'wet' (non-hydrostatic) component
!     of atmosphere using Saastamoinen formula (19a) (Ref 1).
!
!     The saturation vapor pressure is calculated using the formula
!     found in MET03 which is much simpler than that found in Methods
!     of Experimental Physics B (1976) p. 187, but agrees to 0.3%
!     over the range 0 - 40 deg. C.
!
!     Refs:
! ON ENTRY
!     RH = relative humidity  (0 .le. RH .le. 1.0 )
!     RHDOT = TIME DERIVATIVE OF RH
!
!     TC = temperature (Celsius)
!     TCDOT = TIME DERIVATVIE OF TC
! ON EXIT
!     ZW = zenith path delay (meters) of wet component/VLIGHT
!     ZWDOT = TIME DERIVATIVE OF ZENITH DELAY/VLIGHT
!
!-----Calculate saturation vapor pressure, AND TIME DERIVATIVE
!
!     ESAT = 6.11 * 10**(7.5*TC/(TC+237.3))
      TEMP = TC+237.3D0
      ESAT = 6.11D0 * EXP(17.269D0*TC/TEMP)
      ESATDOT = ESAT * (17.269D0/TEMP - 17.269D0*TC/(TEMP*TEMP))*TCDOT
!
!-----Calculate zenith path delay
!
      ZW = 0.002277D0 * (1255.D0/(TC+273.16D0) + 0.05D0) * RH * ESAT
!
! AND RATE OF CHANGE OF ZENITH PATH DELAY
! CHANGED 255. TO 1255. IN TCDOT CONTRIBUTION       12-28-87 WEH
! CHANGED RHDOT CONTRIBUTION FROM ZW*RHDOT/RH       12-28-87  WEH
      TEMP = 273.16D0+TC
      ZWDOT = - 0.002277D0 * (TCDOT*1255.D0/(TEMP*TEMP))*RH*ESAT &
     &  + 0.002277D0 * (1255.D0/(TC+273.16D0) + 0.05D0) * RHDOT * ESAT &
     &  + ZW * ESATDOT/ESAT
!
!
      RETURN
      END
