      SUBROUTINE ifad_wet(pres,temp,rh,elev,elevdot,ifamap,ifarat, &
     &                    met_seas)
!
!  The Ifadis wet global mapping function:
!       wetmap eqn. 8.1 (a-c) pp 98-99
!  from "The Atmospheric Delay of Radio Waves: Modeling the Elevation
!         Dependence on a Global Scale" Ioannis Ifadis, Tech Rep #38L, 1986,
!         Chalmers Univ of Techn. Goteborg Sweden
!
!  Created by C. Kuehn   5/91
!  Modified for SOLVE standard version - D. MacMillan  11/5
!  Added rate mapping function - D. MacMillan   11/5
!
!  input:
!  temp        - Temperature (deg C)
!  pres        - Pressure (mb)
!  rh          - Relative humidity (%)
!  elev,elevdot - elevation and its time derivative  (rad, rad/s)
!
!  internal:
!  a            - mapping function coefficients
!  beta,gamma   - terms in ifadis mapping function
!  wm90         - normalization to ensure that mapping function = 1
!                 at zenith
!  e0           - partial pressure of water vapor (mb)
!
!  output:
!  ifamap   - wet mapping function
!  ifarat   - rate mapping function
!
         IMPLICIT NONE
         real*8 pres,temp,rh ! mbar, deg C, % (i.e. range is 0.0 to 1.0)
         real*8 elev,elevdot,ifamap,ifarat,sinel,cosel
         real*8 a(3),e0,beta,gamma,wm90
         logical*2 met_seas
!
         sinel=dsin(elev)
         cosel=dcos(elev)
!
!  Use Saturation vapor pressure expression from CFA22.f
!
!     The saturation vapor pressure is calculated using the formula
!     found in MET03 which is much simpler than that found in Methods
!     of Experimental Physics B (1976) p. 187, but agrees to 0.3%
!     over the range 0 - 40 deg. C.  CK 5/91 checked values -15 to +50 C
!     against numbers in CRC handbook table D-159, good to 0.5%.
!
         E0 =  rh *(6.11D0 * EXP(17.269D0*Temp/(temp+237.3d0)) )
!
!        Wet Mapping Function
!
         a(1)=.5236d-3+(.2471d-6*(pres-1000.)) &
     &                -(.1724d-6*(temp-15.)) &
     &                +(.1328d-4*(dsqrt(e0)))
         a(2)=.1705d-2+(.7384d-6*(pres-1000.)) &
     &                +(.3767d-6*(temp-15.)) &
     &                +(.2147d-4*(dsqrt(e0)))
         a(3)=.5917d-1
!
         beta = a(2)/(sinel+a(3))
         gamma = a(1)/(sinel+beta)
         wm90 = 1.0d0/(1.0d0+a(1)/(1.0d0+a(2)/(1.0d0+a(3))))
         ifamap = 1.0d0/(sinel+gamma)/wm90
!
!        Rate Mapping Function
         ifarat = -cosel/(sinel+gamma)**2/wm90 * &
     &   (1.0d0-a(1)/(sinel+beta)**2*(1.0d0-a(2)/(sinel+a(3))**2)) * &
     &   elevdot
         return
         end
