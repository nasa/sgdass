!
      SUBROUTINE NUTF96 (xjd,ct,cent,fa,fad)
      IMPLICIT NONE
!
!  NUTF96 computes the number of Julian centuries since J2000 and the
!  fundamental arguments and derivatives to be used in the nutation series.
!
!  NUTF96 is taken from Calc 9, and comforms with the 1996 IERS Conventions.
!   It should be used to duplicate the UT1 => UT1S conversion for Calc 9+
!   databases, and for ALL modfile UT1 => UT1S conversions. Added to SOLVE
!   99.12.14 -DG-
!
!  References: D.McCarthy, IERS Technical Note 13, 'IERS Conventions (1992)',
!                          Paris 1992
!              T.C. van Flandern, Lunar Occult. Work (fundam.argum.)
!              D.McCarthy, IERS Technical Note 21, 'IERS Conventions (1996)',
!                          Paris 1996
!
!  Calling sequence:
!    input:
!           1. xjd  -  THE JULIAN DATE AT ZERO HOURS UTC OF THE DATE IN
!                      QUESTION. (DAYS)
!           2. ct   -  THE COORDINATE TIME FRACTION OF THE COORDINATE TIME DAY.
!                      (DAYS)
!    output:
!           1. cent -  The number of Julian centuries elapsed since the epoch
!                      January 1.5 2000.(centuries)
!           2. fa(5)-  The fundamental arguments for the nutation theory.
!                      (arcseconds)
!               1 = mean anomaly of the moon
!                 = mean longitude of the moon minus the
!                   mean longitude of the moon's perigee     (l)
!               2 = mean anomaly of the sun
!                 = mean longitude of the sun minus the
!                   mean longitude of the sun's perigee      (l')
!               3 = mean longitude of the moon minus omega   (F)
!               4 = mean elongation of the moon from the sun (D)
!               5 = longitude of the asc.node of the moon's
!                   mean orbit on the ecliptic,
!                   measured from the mean equinox of date   (omega)
!           3. fad(5)- The CT time derivatives of the fundamental arguments.
!                      (arcsec/century)
!
      REAL*8 xjd,ct,cent,fa(5), fad(5), el, elp, f, d, om, sec360, &
     &       elc(5), elpc(5), fc(5), dc(5), omc(5), cent2, cent3, cent4, &
     &       daysj
!
      Real*8 CENTJ, DJ2000
      Data  CENTJ  / 36525.D0 /, DJ2000 / 2451545.D0 /
!
!        1. CENTJ   -  the number of coordinate time days per Julian century.
!                      (days/century)
!        2. DJ2000  -  the Julian date of the epoch January 1.5, 2000. (days)
!
!     Constants used -
!       ELC(5)   - COEFFICIENTS USED IN THE CALCULATION OF EL
!       ELPC(5)  - COEFFICIENTS USED IN THE CALCULATION OF ELP
!       FC(5)    - COEFFICIENTS USED IN THE CALCULATION OF F
!       DC(5)    - COEFFICIENTS USED IN THE CALCULATION OF D
!       OMC(5)   - COEFFICIENTS USED IN THE CALCULATION OF OM
!
!     DATA statements for the fundamental arguments.
!     Simons et al., 1994 values
!      -Conforms to IERS Conventions (1996)
      data elc    / -0.00024470d0,       0.051635d0,  31.8792d0, &
     &          1717915923.2178d0,  485868.249036d0/
      data elpc   / -0.00001149d0,      -0.000136d0,  -0.5532d0, &
     &           129596581.0481d0,  1287104.79305d0/
      data fc     /  0.00000417d0,      -0.001037d0, -12.7512d0, &
     &          1739527262.8478d0,  335779.526232d0/
      data dc     / -0.00003169d0,       0.006593d0,  -6.3706d0, &
     &          1602961601.2090d0,  1072260.70369d0/
      data omc    /-0.00005939d0,        0.007702d0,   7.4722d0, &
     &           -6962890.2665d0,   450160.398036d0/
!
      DATA SEC360 / 1296000.0D0 /       ! arcseconds in one turn
!
!  Programmer:
!    93.09.01  Norbert Zacharias - Fundamental arguments computation put into
!              separate subroutine, taken from old NUTG subroutine.
!    98.01.28  David Gordon - Coefficients and computations modified to conform
!              to IERS Conventions (1996).
!
!-------------------------------------------------------------------------------
!  Compute the number of Julian days elapsed since the epoch January 1.5, 2000.
      daysj = xjd + ct - dj2000
!
!  Compute the number of Julian centuries elapsed since the epoch January 1.5,
!   2000.
      CENT  = DAYSJ / CENTJ
      CENT2 = CENT * CENT
      CENT3 = CENT * CENT2
      CENT4 = CENT2 * CENT2
!
!  Computation of the fundamental arguments and derivatives
!
      EL = ELC(1)*CENT4 + ELC(2)*CENT3 + ELC(3)*CENT2 &
     &   + ELC(4)*CENT  + ELC(5)
      fa (1) = DMOD( EL, SEC360 )
      fad(1) = 4.D0*ELC(1)*CENT3 + 3.D0*ELC(2)*CENT2 &
     &       + 2.D0*ELC(3)*CENT  +      ELC(4)
!
      ELP = ELPC(1)*CENT4 + ELPC(2)*CENT3 + ELPC(3)*CENT2 &
     &    + ELPC(4)*CENT  + ELPC(5)
      fa (2) = DMOD( ELP, SEC360 )
      fad(2) = 4.D0*ELPC(1)*CENT3 + 3.D0*ELPC(2)*CENT2 &
     &       + 2.D0*ELPC(3)*CENT  +      ELPC(4)
!
      F = FC(1)*CENT4 + FC(2)*CENT3 + FC(3)*CENT2 &
     &  + FC(4)*CENT  + FC(5)
      fa (3) = DMOD( F, SEC360 )
      fad(3) = 4.D0*FC(1)*CENT3 + 3.D0*FC(2)*CENT2 &
     &       + 2.D0*FC(3)*CENT  +      FC(4)
!
      D = DC(1)*CENT4 + DC(2)*CENT3 + DC(3)*CENT2 &
     &  + DC(4)*CENT  + DC(5)
      fa (4) = DMOD( D, SEC360 )
      fad(4) = 4.D0*DC(1)*CENT3 + 3.D0*DC(2)*CENT2 &
     &       + 2.D0*DC(3)*CENT  +      DC(4)
!
      OM = OMC(1)*CENT4 + OMC(2)*CENT3 + OMC(3)*CENT2 &
     &   + OMC(4)*CENT  + OMC(5)
      fa (5) = DMOD( OM, SEC360 )
      fad(5) = 4.D0*OMC(1)*CENT3 + 3.D0*OMC(2)*CENT2 &
     &       + 2.D0*OMC(3)*CENT  +      OMC(4)
!
      Return
      END
