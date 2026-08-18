!TITLE mttdry
!
      SUBROUTINE mttdry(EL,TC,ELDOT,RLAT,SITHT,MTTMAP,MTTRAT,MET_SEAS)
!
!     Routine to compute the new dry_mit mapping function which
!     depends only on temperature (and position)
!     Routine from T. Herring
!     Routine modified for SOLVE by D. MacMillan   91/10/18
!
      implicit none
!
!   Input variables:
!
!   EL,ELDOT    - elevation (rad), time derivative of elevation
!   TC          - observed surface temperature (C)
!   RLAT        - site latitude (rad)
!   SITHT       - site height above the ellipsoid (m)
!   MET_SEAS    - flag to use seasonal met values (T/F)
!
!   Output variables:
!
!   MTTMAP,MTTRAT - delay and rate MTT dry mapping function
!
      real*8  EL,ELDOT,TC,RLAT,SITHT,MTTMAP,MTTRAT
      logical*2 MET_SEAS
!
!   axis_temp   - Temperature at intersection of axes
!   A,B,C,D     - the A,B,C, and D coefficients in mit-2.2
!   beta        - Term in mit-2.2
!   cose        - Cos of elevation angle
!   gamma       - Term in mit-2.2
!   sine        - Sine of elevation angle
!   topcon      - Constant of top of mapping fuinction to ensure
!                 that value is 1.0000 at zenith
!
      real*8 A,B,C, beta, cose, gamma, sine, topcon, axis_temp
!
!    Tc_seas    - Seasonal value of temperature (C)
!    P_seas     - Seasonal Pressure (mbar)
!    rh_seas    - Seasonal relative humidity
!    tb_seas    - Seasonal bias in temperater
!    cosphi     - Cosine of latitude
!    hs_km      - Height of site in kms.
!
      real*8 Tc_seas, P_seas, rh_seas, cosphi, hs_km, tb_seas
!
!
!
      hs_km = SITHT/1000.d0
!
!***      Get the axis temperature
!
      if (MET_SEAS) then
!
!         Use seasonal temperature
          call met_seasonal( Tc_seas, P_seas, rh_seas, tb_seas, &
     &           RLAT, hs_km )
          axis_temp  = Tc_seas
      else
!         Use observed surface temperature
          axis_temp  = TC
      end if
!
!         Now compute the coefficients in the mapping function.
!
          cosphi = dcos(RLAT)
!
          A  = 1.23200d-3 + 0.01391d-3*cosphi - 0.02089d-3*hs_km &
     &                    + 0.002154d-3*(axis_temp - 10.d0)
          B  = 3.16116d-3 - 0.16004d-3*cosphi - 0.03306d-3*hs_km &
     &                    + 0.002064d-3*(axis_temp - 10.d0)
          C  =71.24372d-3 - 4.29342d-3*cosphi - 0.14908d-3*hs_km &
     &                    - 0.002098d-3*(axis_temp - 10.d0)
!
          sine  = dsin( EL )
          cose  = dcos( EL )
          beta  = B/( sine + C )
          gamma = A/( sine + beta)
!
          topcon = (1.d0 + A/(1.d0 + B/(1.d0 + C)))
!
          MTTMAP = topcon / ( sine + gamma )
!
          MTTRAT = -topcon / ( sine + gamma )**2 * &
     &                ( cose - A/( sine + beta)**2 * cose * &
     &                ( 1.d0 - B/( sine + C )**2 ) ) * ELDOT
!                                              !Units (s/s)/s
      return
      end
!
!
!TITLE mttwet
!
      SUBROUTINE mttwet(EL,TC,ELDOT,RLAT,SITHT,MTTMAP,MTTRAT,MET_SEAS)
!
!     Routine to compute the new wet_mit mapping function which
!     depends only on temperature (and position)
!     Obtained from T. Herring
!     Modified for SOLVE by D. MacMillan   91/10/18
!
      implicit none
!
!   Input variables:
!
!   EL,ELDOT    - elevation (rad), time derivative of elevation
!   TC          - observed surface temperature (C)
!   RLAT        - site latitude (rad)
!   SITHT       - site height above the ellipsoid (km)
!   MET_SEAS    - flag to use seasonal met values (T/F)
!
!   Output variables:
!
!   MTTMAP,MTTRAT - delay and rate MTT wet mapping function
!
      real*8  EL,ELDOT,TC,RLAT,SITHT,MTTMAP,MTTRAT
      logical*2 MET_SEAS
!
!   axis_temp   - Temperature at intersection of axes
!   A,B,C,D     - the A,B,C, and D coeffiecents in mit-2.2
!   beta        - Term in mit-2.2
!   cose        - Cos of elevation angle
!   gamma       - Term in mit-2.2
!   sine        - Sine of elevation angle
!   topcon      - Constant of top of mapping function to ensure
!                 that value is 1.0000 at zenith
!
      real*8 A,B,C, beta, cose, gamma, sine, topcon, axis_temp
!
!
!    Tc_seas    - Seasonal value of temperature (C)
!    P_seas     - Seasonal Pressure (mbar)
!    tb_seas    - Seasonal bias in temperature
!    rh_seas    - Seasonal relative humidity
!    cosphi     - Cosine of latitude
!    hs_km      - Height of site in kms.
!
      real*8 Tc_seas, P_seas, rh_seas, cosphi, hs_km, tb_seas
!
      hs_km = SITHT/1000.d0
!
!***      Get the axis temperature
!
      if (MET_SEAS) then
!
!         Use seasonal temperature
          call met_seasonal( Tc_seas, P_seas, rh_seas, tb_seas, &
     &           RLAT, hs_km )
!
          axis_temp  = Tc_seas
      else
!         Use observed surface temperature
          axis_temp  = TC
      end if
!
!         Now compute the coefficients in the mapping function.
          cosphi = dcos(RLAT)
!
!         Compute the coefficients of the mapping function
          A  = 0.58266d-3 - 0.01105d-3*cosphi - 0.05181d-3*hs_km &
     &                    + 0.001442d-3*(axis_temp - 10.d0)
          B  = 1.40218d-3 + 0.10249d-3*cosphi - 0.10128d-3*hs_km &
     &                    + 0.002046d-3*(axis_temp - 10.d0)
          C  =45.85450d-3 - 1.91277d-3*cosphi - 1.28787d-3*hs_km &
     &                    + 0.015136d-3*(axis_temp - 10.d0)
!
!
          sine  = dsin( EL )
          cose  = dcos( EL )
          beta  = B/( sine + C )
          gamma = A/( sine + beta)
!
          topcon = (1.d0 + A/(1.d0 + B/(1.d0 + C)))
!
          MTTMAP = topcon / ( sine + gamma )
!
          MTTRAT = -topcon / ( sine + gamma )**2 * &
     &                ( cose - A/( sine + beta)**2 * cose * &
     &                ( 1.d0 - B/( sine + C )**2 ) ) * ELDOT
!                                              !Units (s/s)/s
!
      return
      end
!
!
!TITLE met_seasonal
!
      SUBROUTINE met_seasonal( t_c, P, rh, tbias, lat, hgt)
!
!     Routine to return an estimate of temperature, pressure, and
!     relative humidity based on time, latitude, and height
!
!     Routine obtained from T. Herring
!     Modified for SOLVE by D. MacMillan  91/10/18
!     AEE 920204 Removed hard coded path for oborg.i
!     pet 1999.11.09  Added including solve.i
!
      implicit none
      INCLUDE 'solve.i'
      INCLUDE 'oborg.i'
!
! PASSED VARIABLES
!
!   t_c     - Temperature in C
!   t_k     - Temperature in K
!   P       - Pressure in mbar
!   rh      - Relative humidity in 0-1
!   lat     - Latitude of site in radians
!   hgt     - Height of site in km.
!   tbias   - Bias in surface temperature (Should not be added to
!             t_c returned from this routine).
!
      real*8 t_c, t_k, P, rh, lat, hgt, tbias
!
!   epoch   - JD of current measurement.
!   dj2000  - JD of year 2000
!   FJD     - JD of previous midnight
!   FRACT   - fraction of day
!   dt1     - Argument for seasonal term
!
      real*8 epoch, dj2000, dt1,  pi
      data dj2000/2451545.d0/, pi/3.14159265/
!
!****
!     Get an estimate of temperature first.  Get seasonal argument
      epoch = FJD + FRACT
      dt1 = mod ((epoch-dj2000)/365.25,1.d0)*2*pi
      t_c   = ( -20.5 + 48.4*cos(lat)  - 3.1*hgt ) + &
     &         (-14.3 + 3.3*hgt )*sin(lat)*cos(dt1)  + &
     &         ( -4.7 + 1.1*hgt )*sin(lat)*sin(dt1)
!
      tbias = (  -3.6 - 0.3*cos(lat)  + 2.3*hgt ) + &
     &         ( -1.7 - 1.0*hgt )*sin(lat)*cos(dt1)  + &
     &         ( -0.2 + 0.8*hgt )*sin(lat)*sin(dt1)
!
!     Now based on standard lapse rate compute the pressure
!
      t_k = t_c + 273.15d0          ! convert to Kelvin
      P = 1013.25d0 * (t_k/(t_k + 6.5*hgt))**5.26d0
!
!     Set the relative humidity
      rh = 0.5
!
!
      return
      end
