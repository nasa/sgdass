      SUBROUTINE ifad_dry(pres,temp,rh,elev,elevdot,ifamap,ifarat, &
     &                    met_seas)
!
!  The Ifadis dry global mapping functions:
!       drymap eqn. 6.10 (a-c) p 67
!  from "The Atmospheric Delay of Radio Waves: Modeling the Elevation
!         Dependence on a Global Scale" Ioannis Ifadis, Tech Rep #38L, 1986,
!         Chalmers Univ of Techn. Goteborg Sweden
!
!  Created by C. Kuehn   5/91
!  Modified for SOLVE standard version - D. MacMillan 11/5/91
!  Added rate mapping function - D. MacMillan  11/5/91
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
!  dm90         - normalization to ensure that mapping function = 1
!                 at zenith
!  e0           - partial pressure of water vapor (mb)
!
!  output:
!  ifamap   - dry mapping function
!  ifarat   - rate mapping function
!
         IMPLICIT NONE
         real*8 pres,temp,rh ! mbar, deg C, % (i.e. range is 0.0 to 1.0)
         real*8 elev,elevdot,ifamap,ifarat,sinel,cosel
         real*8 a(3),e0,beta,gamma,dm90,wm90
         logical*2 met_seas
!
         sinel=dsin(elev)
         cosel=dcos(elev)
!
!  Use Saturation vapor pressure expression in mbar from CFA22.f
!
!     The saturation vapor pressure is calculated using the formula
!     found in MET03 which is much simpler than that found in Methods
!     of Experimental Physics B (1976) p. 187, but agrees to 0.3%
!     over the range 0 - 40 deg. C.  CK 5/91 checked values -15 to +50 C
!     against numbers in CRC handbook table D-159, good to 0.5%.
!
         E0 =  rh *(6.11D0 * EXP(17.269D0*Temp/(temp+237.3d0)) )
!
!        Dry Mapping Function
!
         a(1)=.1237d-2+(.1316d-6*(pres-1000.)) &
     &                +(.1378d-5*(temp-15.)) &
     &                +(.8057d-5*(dsqrt(e0)))
         a(2)=.3333d-2+(.1946d-6*(pres-1000.)) &
     &                +(.1040d-5*(temp-15.)) &
     &                +(.1747d-4*(dsqrt(e0)))
         a(3)=.078d0
!
         beta = a(2)/(sinel+a(3))
         gamma = a(1)/(sinel+beta)
         dm90 = 1.0d0/(1.0d0+a(1)/(1.0d0+a(2)/(1.0d0+a(3))))
         ifamap = 1.0d0/(sinel+gamma)/dm90
!
!        Rate Mapping Function
!
         ifarat = -cosel/(sinel+gamma)**2/dm90 * &
     &   (1.0d0-a(1)/(sinel+beta)**2*(1.0d0-a(2)/(sinel+a(3))**2)) &
     &   *  elevdot
!
         return
         end
