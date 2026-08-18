!TITLE nhmf2
!
      SUBROUTINE nhmf2(epoch,latitude,height,elev,hmf)
      IMPLICIT NONE                         !Added by IMP/jwr
!
!     Routine to compute the hydrostatic mapping function nhmf2 which
!     depends on DOY (day of year) and station position (latitude
!     and height above geoid).
!
!     Subroutine modified slightly: input latitude,elev in radians
!     epoch is the julian day.  D. MacMillan   1/27/94
!
!     Added correction from Chris Jacobs to the rate mapping function
!     for the derivative of the height correction wrt to elevation.
!     D. MacMillan  1/17/96
!
!     Corrected the a,b,c coeff for latitude>75deg. Seasonal amplitude
!     term was missing.  D. MacMillan 5/29/96
!
      integer*4 i
!
!   a,b,c       - the a,b,and c coeffiecents in the continued fraction
!                 form of Marini
!   beta        - intermediate term in calculation
!   gamma       - intermediate term in calculation
!   sine        - sine of elevation angle
!   cose        - cos of elevation angle
!   hmf(1)      - delay mapping function
!   hmf(2)      - d_mapping_function/d_elevation (dhmf2/d_el)
!   topcon      - constant of top of mapping fuinction to ensure
!                 that value is 1.0000 at zenith
!
      real*8 a,b,c, beta, cose, hmf(2), gamma, sine, topcon
!
!   height     - height of site above geoid (meters)
!   hs_km      - Height of site in kms.
!   latitude   - latitude (radians)
!   latituded  - latitude (deg)
!   l          - absolute latitude
!   dl         - incremental latitude from last lat_hmf
!   elev       - elevation (radians)
!   epoch      - if Julian date of observation is known for the observation,
!              - then epoch can be used to get day of year.
!              - (if epoch  is passed as argument, then un-comment the
!                 line converting epoch to doy.)
!   doy        - days since Dec 31
!   doy_atm    - doy for atmosphere relative to Jan 28.
!   doyr_atm   - doy_atm in radians;
!   cost       - cosine(day of year)
!   doy2rad    - convert doy to radians
!
      real*8 doy, epoch, latitude,latituded, height, elev
      real*8 hs_km, l, dl, doy_atm, doyr_atm, cost
      real*8 doy2rad, deg2rad
!
!   lat_hmf     - latitudes at which coefficients are defined (5).
!   abc_avg     - continued fraction coefficients at latitudes lat_hmf
!   abc_amp     - amplitude of annual variation of abc_avg
!   daavg, daamp, etc - incremental values for interpolation
!   aavg,  aamp,  etc - average and amplitude at latitude
!
      real*8 lat_hmf(5)
      real*8 abc_avg(5,3), abc_amp(5,3)
      real*8 daavg, daamp, dbavg, dbamp, dcavg, dcamp
      real*8 aavg,  aamp,  bavg,  bamp,  cavg,  camp
!
!   a_ht, b_ht, c_ht - parameters for continued fraction for height corr'n.
!
      real*8 a_ht, b_ht, c_ht, ht_corr_coef, ht_corr
      real*8 dhcc_del,dht_corr_del
!
!   define parameters used for calculating coefficients.
!
      data lat_hmf / 15, 30, 45, 60, 75/
!
      data abc_avg / &
     &1.2769934e-3,1.2683230e-3,1.2465397e-3,1.2196049e-3,1.2045996e-3, &
     &2.9153695e-3,2.9152299e-3,2.9288445e-3,2.9022565e-3,2.9024912e-3, &
     &62.610505e-3,62.837393e-3,63.721774e-3,63.824265e-3,64.258455e-3/
!
      data abc_amp / &
     &  0.0,   1.2709626e-5, 2.6523662e-5, 3.4000452e-5, 4.1202191e-5, &
     &  0.0,   2.1414979e-5, 3.0160779e-5, 7.2562722e-5, 11.723375e-5, &
     &  0.0,   9.0128400e-5, 4.3497037e-5, 84.795348e-5, 170.37206e-5/
!
      data a_ht / 2.53e-5/ &
     &     b_ht / 5.49e-3/ &
     &     c_ht / 1.14e-3/
!
!   conversions:
!
      doy2rad = 2*3.14159265/365.25
      deg2rad = 3.14159265/180.
!
!   convert height in meters to kilometers
!
      hs_km  = height/1000.d0
!
!   If Julian date is used for epoch, then calculate day of year;
!      use 1980 Jan 0 as reference epoch.
!
      doy = epoch - 2444238.5
!
!   to account for the six month difference in seasons between hemispheres,
!   add 365.25/2 days to doy if station is in the southern hemisphere.
!
      latituded = latitude/deg2rad               ! convert to degrees
      l = abs(latituded)
      if (latituded .lt. 0) doy = doy + 365.25/2
!
! mod aen 930517 Use phase of 28 days (winter extremum corresponds to Jan 28)
!                based on least-square fit to
!                raytrace of radiosonde data for DRT, ELP, ALB, CHH, FAI,
!                MUN, and LIH.
!
!
      doy_atm  = doy - 28.
      doyr_atm = doy_atm * doy2rad
!
!aen  debug
!     write(*,'("doy, doy_atm, doyr_atm = ", 3f15.6)') doy, doy_atm, doyr_atm
      cost = cos(doyr_atm)
!
!   Coefficients for the continued fraction expansion for each latitude.
!
!   for latitudes less than 15 degrees:
!
      if (l .le. lat_hmf(1)) then
         a = abc_avg(1,1)
         b = abc_avg(1,2)
         c = abc_avg(1,3)
      endif
!
!   for latitudes between 15 and 75  degrees:
!
      do i = 1,4
          if (l .gt. lat_hmf(i) .and. l .le. lat_hmf(i+1)) then
             dl = (l-lat_hmf(i))/(lat_hmf(i+1)-lat_hmf(i))
             daavg =   abc_avg(i+1,1)-abc_avg(i,1)
             daamp =   abc_amp(i+1,1)-abc_amp(i,1)
             aavg  =   abc_avg(i,1) + dl*daavg
             aamp  =   abc_amp(i,1) + dl*daamp
             a     = aavg - aamp*cost
!     write(*,'(" dl,daavg,daamp,aavg,aamp,a ",6e15.6)')
!    .            dl,daavg,daamp,aavg,aamp,a
!
             dbavg =   abc_avg(i+1,2)-abc_avg(i,2)
             dbamp =   abc_amp(i+1,2)-abc_amp(i,2)
             bavg  =   abc_avg(i,2) + dl*dbavg
             bamp  =   abc_amp(i,2) + dl*dbamp
             b     = bavg - bamp*cost
!     write(*,'(" dl,dbavg,dbamp,bavg,bamp,b ",6e15.6)')
!    .            dl,dbavg,dbamp,bavg,bamp,b
!
             dcavg =   abc_avg(i+1,3)-abc_avg(i,3)
             dcamp =   abc_amp(i+1,3)-abc_amp(i,3)
             cavg  =   abc_avg(i,3) + dl*dcavg
             camp  =   abc_amp(i,3) + dl*dcamp
             c     = cavg - camp*cost
!     write(*,'(" dl,dcavg,dcamp,cavg,camp,c ",6e15.6)')
!    .            dl,dcavg,dcamp,cavg,camp,c
!
          endif
      end do
!
!   for latitudes greater than 75 degrees:
!
      if (l .ge. lat_hmf(5)) then
         a = abc_avg(5,1) - abc_amp(5,1)*cost
         b = abc_avg(5,2) - abc_amp(5,2)*cost
         c = abc_avg(5,3) - abc_amp(5,3)*cost
      endif
!
!   Now the coefficients exist; calculate the mapping function, hmf(1),
!       and the change of mapping function with elevation,
!       dhmf/d_el = hmf(2).
!   To get delay-rate correction d_tau/dt:
!      d_tau/dt = d_tau-zen/dt*hmf(1) + tau-zen*dhmf/d_el*d_el/dt
!
      sine   = sin(elev )
      cose   = cos(elev )
      beta   = b/(sine + c )
      gamma  = a/(sine + beta)
      topcon = (1.d0 + a/(1.d0 + b/(1.d0 + c)))
!
      hmf(1) = topcon / ( sine + gamma )
!
      hmf(2) = -topcon / ( sine + gamma )**2 * &
     &            ( cose - a/( sine + beta)**2 * cose * &
     &            ( 1.d0 - b/( sine + c )**2 ) )
!
!     write(*,'("sine, cose, beta, gamma, topcon = ", 5f10.5)')
!    .           sine, cose, beta, gamma, topcon
!
!     write(*,'("hmf(1), hmf(2) = ", 2f10.4)') hmf(1), hmf(2)
!     write(*,'("hmf(1), hmf(2) = ", 2f10.4)') hmf
!
!   Apply height correction to mapping function and derivative wrt elevation:
!         (not to dmf/d_el since this is a small correction):
!      1) height correction coefficient is
!         1/sine(elev) - continued fraction(a_ht,b_ht,c_ht).
!      2) height correction is ht_corr_coef times height in km.
!      3) height correction to derivative wrt elevation is (derivative of
!         height correction coefficient wrt elevation)*height in km.
!
      beta   = b_ht/( sine + c_ht )
      gamma  = a_ht/( sine + beta)
      topcon = (1.d0 + a_ht/(1.d0 + b_ht/(1.d0 + c_ht)))
      ht_corr_coef = 1/sine - topcon/(sine + gamma)
      ht_corr      = ht_corr_coef * hs_km
      hmf(1)       = hmf(1) + ht_corr
!
!   Add contribution to rate mapping function from the derivative
!   of the height correction wrt to elevation (from Chris Jacobs
!   95/11/29)
!
      dhcc_del = -cose/sine**2 &
     &           +topcon*cose/ ( sine + gamma )**2 * &
     &            ( 1.d0 - a_ht/( sine + beta)**2 * &
     &            ( 1.d0 - b_ht/( sine + c_ht)**2 ) )
!
      dht_corr_del = dhcc_del*hs_km
!
      hmf(2) = hmf(2) + dht_corr_del
!
!     write(*,'("ht_corr_coef, ht_corr, hs_km, hmf(1) = ", 4f15.6)')
!    .           ht_corr_coef, ht_corr, hs_km, hmf(1)
!
      return
      end
!
!
!TITLE nwmf2
!
      SUBROUTINE nwmf2(latitude, elev, wmf)
      IMPLICIT NONE                         !Added by IMP/jwr
!
! new aen 930517 Routine to compute the new wmf2.0 mapping function which
!                depends only on latitude.
! Subroutine slightly modified: input latitude and elev are in rad. dsm 1/27/94
!
      integer*4 i
!
!   a,b,c       - the a,b,and c coefficients in the continued fraction
!                 form of Marini
!   beta        - intermediate term in calculation
!   gamma       - intermediate term in calculation
!   sine        - Sine of elevation angle
!   cose        - Cos of elevation angle
!   wmf(1)      - wet delay mapping function
!   wmf(2)      - d_wet_mapping_function/d_elevation
!   topcon      - Constant of top of mapping fuinction to ensure
!                 that value is 1.0000 at zenith
!
      real*8 a,b,c, beta, cose, wmf(2), gamma, sine, topcon
!
!   latitude   - latitude (radians)
!   latituded   - latitude (degrees)
!   l          - absolute latitude
!   dl         - incremental latitude from last lat_wmf
!   elev       - elevation (radians)
!   dl,da,db,dc  - used for interpolation
!
      real*8 lat_wmf(5), abc_w2p0(5,3)
      real*8 dl, da, db, dc
      real*8 latitude, latituded,l, elev, deg2rad
!
!   define parameters used for calculating coefficients.
!
      data lat_wmf / 15, 30, 45, 60, 75/
!
!   coefficients are from fits to raytraces of the standard atmospheres
!   for July for latitudes 15, 45, 60, and 75 degrees latitude and for
!   January for 30 degrees latitude (930517).
!
      data abc_w2p0 / &
!
     & 5.8021897e-4,5.6794847e-4,5.8118019e-4,5.9727542e-4,6.1641693e-4, &
     & 1.4275268e-3,1.5138625e-3,1.4572752e-3,1.5007428e-3,1.7599082e-3, &
     & 4.3472961e-2,4.6729510e-2,4.3908931e-2,4.4626982e-2,5.4736038e-2/
!
      deg2rad = 3.14159265/180.
!
      latituded = latitude/deg2rad             ! convert to degrees
      l = abs(latituded)
!
!   Coefficients for the continued fraction expansion for each latitude.
!
!   for latitudes less than 15 degrees:
!
      if (l .le. lat_wmf(1)) then
         a = abc_w2p0(1,1)
         b = abc_w2p0(1,2)
         c = abc_w2p0(1,3)
      endif
!
!   for latitudes between 15 and 75  degrees:
!
      do i = 1,4
          if (l .gt. lat_wmf(i) .and. l .le. lat_wmf(i+1)) then
             dl = (l-lat_wmf(i))/(lat_wmf(i+1)-lat_wmf(i))
             da  =   abc_w2p0(i+1,1)-abc_w2p0(i,1)
             a   =   abc_w2p0(i,1) + dl*da
!     write(*,'(" dl,da ,a  ",6e15.6)')
!    .            dl,da ,a
!
             db  =   abc_w2p0(i+1,2)-abc_w2p0(i,2)
             b   =   abc_w2p0(i,2) + dl*db
!     write(*,'(" dl,db ,b ",6e15.6)')
!    .            dl,db ,b
!
             dc  =   abc_w2p0(i+1,3)-abc_w2p0(i,3)
             c   =   abc_w2p0(i,3) + dl*dc
!     write(*,'(" dl,dc ,c ",6e15.6)')
!    .            dl,dc ,c
!
          endif
      end do
!
!   for latitudes greater than 75 degrees:
!
      if (l .ge. lat_wmf(5)) then
         a = abc_w2p0(5,1)
         b = abc_w2p0(5,2)
         c = abc_w2p0(5,3)
      endif
!
!   Now the coefficients exist; calculate the mapping function, wmf(1),
!       and the change of mapping function with elevation,
!       dwmf/d_el =wmf(2).
!   To calculate the delay-rate correction, d_tau/dt:
!       d_tau/dt = d_tau_zen/dt * wmf(1) + tau_zen * dwmf/d_el * d_el/dt
!
      sine  = sin( elev )
      cose  = cos( elev )
      beta  = b/( sine + c )
      gamma = a/( sine + beta)
      topcon = (1.d0 + a/(1.d0 + b/(1.d0 + c)))
!
!     write(*,'("sine, cose, beta, gamma, topcon = ", 5f10.5)')
!    .           sine, cose, beta, gamma, topcon
!
      wmf(1) = topcon / ( sine + gamma )
!
      wmf(2) = -topcon / ( sine + gamma )**2 * &
     &         ( cose - a/( sine + beta)**2 * cose * &
     &         ( 1.d0 - b/( sine + c )**2 ) )
!
!     write(*,'("wmf(1), wmf(2) = ", 2f10.4)') wmf(1), wmf(2)
!     write(*,'("wmf(1), wmf(2) = ", 2f10.4)') wmf
!
!aen   write out diagnostic info.
!     write(*,'("  elev, latitude, wmf2.0, dwmf/del = ",4f15.6)')
!    .             elev, latitude, wmf(1), wmf(2)
!
      return
      end
