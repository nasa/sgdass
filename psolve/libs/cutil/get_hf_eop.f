        SUBROUTINE get_hf_eop(JUL_days,ut, xwob, ywob, &
     &                                utd,xwobd,ywobd,eorc)
        implicit none
! Rewritten August 4, 1994  JMgipson
!    JMGipson
! on entry
!   Jul_days -- julian date
!   eorc     -- estimated or calculated eop values
!
! on exit
!   dut     -- change in ut1 in seconds.
!   dxwob,dywob -- change to x and y wobble in mas
! base on fitting several years of VLBI data.
!
! June 27 1997.  Modified by JMGipson
!  1.) if called sequentially with same time, will return previous answer.
!  2.) A little code clean up.
!     (Got rid of tests to make sure user passed correct time.)
!
      INCLUDE 'solve.i'
      INCLUDE 'glbc4.i'
! tidal terms evaluated at a given epoch.
!
        real*8 ut,xwob,ywob
        real*8 utd,xwobd,ywobd
        integer*2 eorc,iptr
        real*8 jul_days
! the 0 subscripts are calibrations.
        real*8 jul_days0,ut0,xwob0,ywob0,utd0,xwobd0,ywobd0
        save   jul_days0,ut0,xwob0,ywob0,utd0,xwobd0,ywobd0
! the e subscripts are estimations.
        real*8 jul_dayse,ute,xwobe,ywobe,utde,xwobde,ywobde
        save   jul_dayse,ute,xwobe,ywobe,utde,xwobde,ywobde
!
!
      IF(eorc.eq.1) then
! Calculate the EOP values if necessary.
        if(jul_days .ne. jul_days0) then
          iptr=num_sdc_ut1+1
          call get_hf_eop_raw(jul_days,sdc_arg(1,1),sdc_arg(1,iptr), &
     &       sdc_val(1,1),sdc_val(1,iptr),num_sdc_ut1,num_sdc_xy, &
     &       ut0,xwob0,ywob0,utd0,xwobd0,ywobd0)
        endif
! save results for next go round
        jul_days0 =jul_days
! and return the EOP results
        ut=ut0
        xwob=xwob0
        ywob=ywob0
        utd=utd0
        xwobd=xwobd0
        ywobd=ywobd0
      ELSE
! if necessary calculate EOP for this epoch.
        if(jul_dayse.ne.jul_days) then
         iptr=num_sde_ut1+1
         call get_hf_eop_raw(jul_days,sde_arg(1,1),sde_arg(1,iptr), &
     &    sde_val(1,1),sde_val(1,iptr),num_sde_ut1,num_sde_xy, &
     &    ute,xwobe,ywobe,utde,xwobde,ywobde)
        endif
! save results for next go round
        jul_dayse =jul_days
! and return the EOP results
        ut=ute
        xwob=xwobe
        ywob=ywobe
        utd=utde
        xwobd=xwobde
        ywobd=ywobde
      ENDIF
      return
      end
!
!***********************************************************************************
        SUBROUTINE get_hf_eop_raw(fjday, &
     &ut1_arg,xy_arg,ut1_val,xy_val,num_ut1,num_xy, &
     &dut1,dx,dy,dut1d,dxd,dyd)
      IMPLICIT NONE                         !Added by IMP/jwr
!
        integer*2 num_ut1, num_xy,i6
!
!     Routine to compute the diurnal and semidiurnal contributions
!     to the pole position and UT1 using the VLBI derived values
!     for the tidal varitions.
!
        real*8 fjday, dx, dy, dut1,dxd,dyd,dut1d
!
! USAGE:
!     <fjday>    is a full julian date with fractional part
!                   of the day added (REAL*8 INPUT)
!   ut1_arg(6,num_ut1)  - Arguments (Browns + (gst+pi))
!                                 for ut1.  Same ordering as IAU
!                                 nutation series (i.e., l,l',F,D,
!                                 Om)
!   xy_arg(6,num_xy)    - Arguments for xy pole (as above)
!
        integer*2 ut1_arg(6,num_ut1), xy_arg(6,num_xy)
!
!   ut1_val(2,num_ut1)  - Values for cosine and sine for
!                               - UT1 (mas)
!   xy_val(2,num_xy)    - Values for cosine and sine for
!                               - x and y (cosine and sine again)
!                               - (mas)
!
        real*8 ut1_val(2,num_ut1), xy_val(2,num_xy)
!
!
!     <dx>, <dy>, <dut1> are the tidally coherent contributions
!                   to the x and y pole positions and to UT1.
!                   dx and dy are returned in milliarcseconds,
!                   dut1 in microseconds (REAL*8 OUTPUT)
!
! RESTRICTIONS: if <fjday> is less than 2000000.0 this routine
!               assumes an MJD has been passed and the time
!               used will be converted to JD.  A warning
!               message will be printed.
!
!
! DEFINE THE PARAMETERS OF THE SYSTEM
!
!
!
! PHYSICAL CONSTANTS NEEDED FOR SD_COMP
!
!   pi          - Define here to full precision
!   rad_to_deg  - Conversion from radians to degs.
!   DJ2000      - Julian date of J2000
!   sec360      - number of seconds in 360 degreees.
!
        real*8 pi, rad_to_deg, DJ2000, sec360
!
        parameter ( pi            = 3.1415926535897932D0 )
        parameter ( DJ2000        = 2451545.d0           )
        parameter ( sec360        = 1296000.d0           )
!
!     Computed quanities
        parameter ( rad_to_deg    = 180.d0   /pi         )
!
!
!-------------------------------------------------------------------
!
! LOCAL VARIABLES
!
!   i      - Loop counters
!
        integer*4 i
!
!   arg         - Angular argument for the correction (rads)
!   fund_arg(6) - Values of the 5 Brown's arguments (l,l',F,D,
!               - Omega) and gst+pi (rads)
!
! arg,argd -- argument, argument_dot.
        real*8 epoch, arg,argd, fund_arg(6,2)
        real*8 dotarg
!
!**** Get the fundamental arguments at this epoch
!
        call tide_angles(fjday, fund_arg)
!
!     Clear the contributions
        dx   = 0.d0
        dy   = 0.d0
        dut1 = 0.d0
        dxd  = 0.d0
        dyd  = 0.d0
        dut1d =0.d0
!
!
!     Now loop over the UT1 contributions
          i6=6
        do i = 1, num_ut1
!         Get the argument
            arg=dotarg(ut1_arg(1,i), fund_arg(1,1),i6)
!           arg = mod(arg, 2.d0*pi)
            argd=dotarg(ut1_arg(1,i), fund_arg(1,2),i6)
!         Increment the change to UT1
            dut1 = dut1 + &
     &             ut1_val(1,i)*cos(arg)+ut1_val(2,i)*sin(arg)
            dut1d = dut1d + &
     &         argd*(-ut1_val(1,i)*sin(arg)+ut1_val(2,i)*cos(arg))
        end do
! convert from mas to mts
        dut1 = dut1/15.
        dut1d = dut1d/15.
!
!****  Now do polar motion
        do i = 1, num_xy
!         Get the argument
            arg=dotarg(xy_arg(1,i),fund_arg(1,1),i6)
!           arg = mod(arg, 2.d0*pi)
            argd=dotarg(xy_arg(1,i),fund_arg(1,2),i6)
!         Increment the change to the X anf Y positions
            dx = dx  + &
     &            (-xy_val(1,i)* cos(arg) + xy_val(2,i)* sin(arg))
            dxd= dxd + &
     &       argd*(xy_val(1,i)* sin(arg) + xy_val(2,i) * cos(arg))
!
            dy = dy + &
     &            (xy_val(1,i)* sin(arg) + xy_val(2,i) * cos(arg))
            dyd= dyd + &
     &       argd*(xy_val(1,i)* cos(arg) - xy_val(2,i) * sin(arg))
        end do
!***** That is all.
        return
        end
!
!***********************************************************
!TITLE TIDE_ANGLES
        SUBROUTINE tide_angles( epoch, fund_arg )
      IMPLICIT NONE                         !Added by IMP/jwr
!
!-----The following type specification statements added automatically
!     by imp/jwr July 2002
!
      INTEGER*2 i
      REAL*4 gstdot
!-----END of imp added lines.
!
!     Routine to compute the value of the fundamental argument
!     for Brown's arguments.  The sixth entry is returned as GST
!     plus pi.  The additional pi is used for compatability with
!     Doodson's Tide argument.
!
! original routine From Tom Herring.
! modified by JMGipson to compute derivatives of tide angles.
! Tide angles are returned in radians.
! Derivatives in radians/sec.
!
! PHYSICAL CONSTANTS NEEDED FOR SD_COMP
!
!   pi          - Define here to full precision
!   rad_to_deg  - Conversion from radians to degs.
!   DJ2000      - Julian date of J2000
!   sec360      - number of seconds in 360 degreees.
!
        real*8 pi, rad_to_deg, DJ2000, sec360,arcsec2rad,twopi
        real*8 century2sec
!
        parameter ( pi            = 3.1415926535897932D0 )
        parameter ( twopi         = 2.d0*pi)
        parameter ( DJ2000        = 2451545.d0           )
        parameter ( sec360        = 1296000.d0           )
!
!     Computed quanities
        parameter ( rad_to_deg    = 180.d0   /pi         )
! conversion from arcseconds to radians.
        parameter (arcsec2rad= twopi/sec360)
        parameter(century2sec=86400.d0*36525.d0)
!
!-------------------------------------------------------------------
!
! PASSED VARIABLES
!
! INPUT
! epoch  - Julian date for arguments (fjday + fraction of day)
!
! OUTPUT
! fund_arg(6,2) -  Brown's arguments plus GST+pi (rads)
!
        real*8 epoch, fund_arg(6,2)
!
! LOCAL VARIABLES
!      cent             - Julian centuries to DJ2000.
!      el,eld           - Mean longitude of moon minus mean
!                       - longitude of moon's perigee (arcsec)
!      elc(5)           - Coefficients for computing el
!      elp,elpd         - Mean longitude of the sun minus mean
!                       - longitude of sun perigee (arcsec)
!      elpc(5)          - Coeffiecents for computing elp
!      f,fd             - Moon's mean longitude minus omega (sec)
!      fc(5)            - Coefficients for computing f
!      d,dd             - Mean elongation of the moon from the
!                       - sun (arcsec)
!      dc(5)            - coefficients for computing d
!      om,omd           - longitude of the ascending node of the
!                       - moon's mean orbit on the elliptic
!                       - measured from the mean equinox of date
!      omc(5)           - Coefficients for computing om.
!      gst              - Greenwich mean sidereal time (rad)
!
        real*8 cent, el,eld, elc(5), elp, elpd, elpc(5), &
     &    f,fd, fc(5), d,dd, dc(5), om,omd, omc(5), gst
!      fract        - fraction of a day from 0:00 hrs UT.
!      Jd_0hr       - Julian date at zero hours UT
!      t_0hr        - Days since DJ2000 at 0:00 hrs UT
!      gstd         - GMST at 0:00 hrs UT1 of day being evaluated
!      diurnv       - Ratio of solar days to sidreal days on
!                     day of evalution.
!
        real*8 fract, t_0hr, gstd, diurnv, fjday_0hr
!
!****  DATA statements for the fundamental arguments.
!
        data elc    /     0.064d0,    31.310d0,    715922.633d0, &
     &             485866.733d0,    1325.0d0 /
        data elpc   /    -0.012d0,    -0.577d0,   1292581.224d0, &
     &            1287099.804d0,      99.0d0 /
        data fc     /     0.011d0,   -13.257d0,    295263.137d0, &
     &             335778.877d0,    1342.0d0/
        data dc     /     0.019d0,    -6.891d0,    1105601.328d0, &
     &            1072261.307d0,    1236.0d0/
        data omc    /     0.008d0,     7.455d0,    -482890.539d0, &
     &             450160.280d0,      -5.0d0/
!
!****  Get the number of centuries to current time
!
        cent = (epoch-dj2000) / 36525.d0
!
!****  Compute angular arguments
        el = elc(1) * cent**3 + elc(2) * cent**2 + elc(3) * cent &
     &          + elc(4) + dmod( elc(5) * cent, 1.d0 ) * sec360
        el = dmod( el, sec360 )
        eld = 3.d0 * elc(1) * cent**2 + 2.d0 * elc(2) * cent + elc(3) &
     &      + elc(5) * sec360
!
        elp = elpc(1) * cent**3 + elpc(2) * cent**2 + elpc(3) * cent &
     &     + elpc(4) + dmod( elpc(5) * cent, 1.d0 ) * sec360
        elp = dmod( elp, sec360 )
        elpd = 3.d0 * elpc(1) * cent**2 + 2.d0 * elpc(2) * cent + elpc(3) &
     &       + elpc(5) * sec360
!
        f = fc(1) * cent**3 + fc(2) * cent**2 + fc(3) * cent &
     &     + fc(4) + dmod( fc(5) * cent, 1.d0 ) * sec360
        f = dmod( f, sec360 )
        fd = 3.d0 * fc(1) * cent**2 + 2.d0 * fc(2) * cent + fc(3) &
     &     + fc(5) * sec360
!
        d = dc(1) * cent**3 + dc(2) * cent**2 + dc(3) * cent &
     &     + dc(4) + dmod( dc(5) * cent, 1.d0 ) * sec360
        d = dmod( d, sec360 )
        dd = 3.d0 * dc(1) * cent**2 + 2.d0 * dc(2) * cent + dc(3) &
     &     + dc(5) * sec360
!
        om = omc(1) * cent**3 + omc(2) * cent**2 + omc(3) * cent &
     &     + omc(4) + dmod( omc(5) * cent, 1.d0 ) * sec360
        om = dmod( om, sec360 )
        omd = 3.d0 * omc(1) * cent**2 + 2.d0 * omc(2) * cent + omc(3) &
     &      + omc(5) * sec360
!
!
!***** Now compute GMST.  (CALC 7.1 Algorithm)
!     Remove the fractional part of the julian date
!     Get fjday at 0:00 UT
        fjday_0hr = aint(epoch-0.5d0) + 0.5d0
!                         ! Days since J2000.0
        t_0hr = fjday_0hr - dj2000
!                         ! 0:00 hrs at start of day
        cent = t_0hr / 36525.d0
!
!                         ! Fraction of a day
        fract = epoch - fjday_0hr
!
        diurnv = ( 1.002737909350795d0 + 5.9006d-11*cent &
     &                               - 5.9d-15*cent**2 )
!
!**** COMPUTE GST in cycles
        gstd = ( 24110.54841d0  + 8640184.81266d0*cent &
     &                        + 0.093104d0*cent**2 &
     &                        - 6.2d-6*cent**3 ) /86400.d0
!
        gstd = dmod(gstd,1.d0)
!                                             ! Rads
        gst = (gstd + diurnv*fract) * twopi
!
        gstdot=(diurnv/86400.+ &
     &     (8640184.81266d0+2.*0.093104d0*cent - 3.d0*6.2d-6*cent*3 ) &
     &    /86400.d0/century2sec)
!
!
!****  Now save the values.  Convert values from arcseconds to radians
!
        fund_arg(1,1) = el
        fund_arg(2,1) = elp
        fund_arg(3,1) = f
        fund_arg(4,1) = d
        fund_arg(5,1) = om
!
        fund_arg(1,2) = eld
        fund_arg(2,2) = elpd
        fund_arg(3,2) = fd
        fund_arg(4,2) = dd
        fund_arg(5,2) = omd
!
        do i=1,5
          fund_arg(i,1)=fund_arg(i,1)*arcsec2rad
          fund_arg(i,2)=fund_arg(i,2)*arcsec2rad/century2sec
        end do
!
        fund_arg(6,1) = gst + pi
        fund_arg(6,2) = gstdot*twopi
!***** Thats all
        return
        end
!
!********************************************************
         FUNCTION dotarg(a,b,ilen)
      IMPLICIT NONE                         !Added by IMP/jwr
!
!-----The following type specification statements added automatically
!     by imp/jwr July 2002
!
      INTEGER*2 i
!-----END of imp added lines.
!
         integer*2 ilen
         real*8 dotarg
         real*8 b(*)
         integer*2 a(*)
!
         dotarg=0.
         do i=1,ilen
           dotarg=dotarg+a(i)*b(i)
         end do
         return
         end
