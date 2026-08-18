      SUBROUTINE NUTFA (xjd,ct,cent,fa,fad)
!
!  NUTFA computes the number of Julian centuries since J2000 and the fundamental
!  arguments and derivatives to be used in nutation series.
!
!  References: D.McCarthy, IERS Technical Note 13, Paris 1992
!              T.C. van Flandern, Lunar Occult. Work (fundam.argum.)
!
!  Calling sequence:
!    input:
!           xjd   -  The Julian date at zero hours utc of the date in
!                   question.  (days)
!
!           ct    -  The coordinate time fraction of the coordinate time
!                   day.  (days)
!
!    Note:  xjd can also be the complete real number Julian date with
!                   ct equal to zero.
!
!    output:
!           cent  -  the number of Julian centuries elapsed
!                         since the epoch January 1.5 2000 (centuries)
!
!           fa(5) -  fundamental arguments for nutation theory
!                         (arcseconds)
!               1 = mean anomaly of the moon
!                 = mean longitude of the moon minus the
!                   mean longitude of the moon's perigee     (l)
!
!               2 = mean anomaly of the sun
!                 = mean longitude of the sun minus the
!                   mean longitude of the sun's perigee      (l')
!
!               3 = mean longitude of the moon minus omega   (F)
!
!               4 = mean elongation of the moon from the sun (D)
!
!               5 = longitude of the asc.node of the moon's
!                   mean orbit on the ecliptic,
!                   measured from the mean equinox of date   (omega)
!
!           fad(5)     -  the CT time derivatives of the fundam. argum.
!                         (arcsec / century)
!
      implicit none
!
      real*8   xjd,ct,cent,fa(5), fad(5), &
     &         el, elp, f, d, om, sec360, &
     &         elc(5), elpc(5), fc(5), dc(5), omc(5), &
     &         cent2, cent3
      real*8   daysj, centj, dj2000, ec(4), argp(2,6)
      real*8   x(9,120)
      integer*2  not,nop, idp(6)
!
!     local constants:
!          centj   -  the number of coordinate time days per
!                     Julian century  (days/century)
!
!          dj2000  -  the Julian date of the epoch
!                     January 1.5, 2000   (days)
      data  centj  / 36525.D0 /, dj2000 / 2451545.D0 /
!
!       ELC(5)   - coefficients used in the calculation of EL
!       ELPC(5)  - coeficientts used in the calculation of ELP
!       FC(5)    - coefficients used in the calculation of F
!       DC(5)    - coefficients used in the calculation of D
!       OMC(5)   - coefficients used in the calculation of OM
!
!       Coefficients are in the order cubic, quadratic, linear, constant,
!       and integral number of turns.
!
      DATA ELC /0.064D0,31.310D0,715922.633D0,485866.733D0,1325.0D0/
      DATA ELPC /-0.012D0,-0.577D0,1292581.224D0,1287099.804D00,99.0D0/
      DATA FC /0.011D0,-13.257D0,295263.137D0,335778.877D0,1342.0D0/
      DATA DC /0.019D0,-6.891D0,1105601.328D0,1072261.307D0,1236.0D0/
      DATA OMC /0.008D0,7.455D0,-482890.539D0,450160.280D0,-5.0D0/
!
      DATA SEC360 / 1296000.0D0 /       ! arcseconds in one turn
!
!  programmer:
!    930901  Norbert Zacharias: implementation for EQE
!    :93.12.21:jwr: Modified for use in solve. All commons eliminated.
!
!---- the following section is taken from old NUTG subroutine:
!
!  compute the number of Julian days elapsed since the
!  epoch January 1.5, 2000
      daysj = xjd + ct - dj2000
!
!  compute the number of Julian centuries elapsed since the
!  epoch January 1.5, 2000
      cent = daysj / centj
      cent2= cent * cent
      cent3= cent * cent2
!
!---- the following section is taken from old NUTW subroutine:
!
!  computation of fundamental arguments and derivatives
!
      EL = ELC(1) * CENT3 + ELC(2) * CENT2 + ELC(3) * CENT &
     &     + ELC(4) + DMOD( ELC(5) * CENT, 1.D0 ) * SEC360
      fa (1) = DMOD( EL, SEC360 )
      fad(1) = 3.D0 * ELC(1) * CENT2 + 2.D0 * ELC(2) * CENT &
     &         + ELC(3) + ELC(5) * SEC360
!
      ELP = ELPC(1) * CENT3 + ELPC(2) * CENT2 + ELPC(3) * CENT &
     &     + ELPC(4) + DMOD( ELPC(5) * CENT, 1.D0 ) * SEC360
      fa (2) = DMOD( ELP, SEC360 )
      fad(2) = 3.D0 * ELPC(1) * CENT2 + 2.D0 * ELPC(2) * CENT &
     &         + ELPC(3) + ELPC(5) * SEC360
!
      F = FC(1) * CENT3 + FC(2) * CENT2 + FC(3) * CENT &
     &     + FC(4) + DMOD( FC(5) * CENT, 1.D0 ) * SEC360
      fa (3) = DMOD( F, SEC360 )
      fad(3) = 3.D0 * FC(1) * CENT2 + 2.D0 * FC(2) * CENT &
     &         + FC (3) + FC(5) * SEC360
!
      D = DC(1) * CENT3 + DC(2) * CENT2 + DC(3) * CENT &
     &     + DC(4) + DMOD( DC(5) * CENT, 1.D0 ) * SEC360
      fa (4) = DMOD( D, SEC360 )
      fad(4) = 3.D0 * DC(1) * CENT2 + 2.D0 * DC(2) * CENT &
     &         + DC(3) + DC(5) * SEC360
!
      OM = OMC(1) * CENT3 + OMC(2) * CENT2 + OMC(3) * CENT &
     &     + OMC(4) + DMOD( OMC(5) * CENT, 1.D0 ) * SEC360
      fa (5) = DMOD( OM, SEC360 )
      fad(5) = 3.D0 * OMC(1) * CENT2 + 2.D0 * OMC(2) * CENT &
     &         + OMC(3) + OMC(5) * SEC360
!
      return
      END
