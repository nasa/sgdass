      SUBROUTINE NUTFA10 (XJD, CT, CENT, FA2K, FAD2K)
      IMPLICIT NONE
!
!    From Calc 10.0, 2005.12.21, DG
!
!  NUTFA computes the number of Julian centuries since J2000 and the fundamental
!  arguments and derivatives to be used in the nutation series.
!
!  References: D.McCarthy, IERS Technical Note 32, IERS Conventions (2003).
!
!  Calling sequence:
!    input:
!           1. XJD  -  THE JULIAN DATE AT ZERO HOURS UTC OF THE DATE IN
!                      QUESTION. (DAYS)
!           2. CT   -  THE COORDINATE TIME FRACTION OF THE COORDINATE TIME DAY.
!                      (DAYS)
!    output:
!           1. CENT -  The number of Julian centuries elapsed since the epoch
!                      January 1.5 2000.(centuries)
!           4. FA2K(14)- The fundamental arguments for the nutation theory,
!                      updated for the IERS Conventions 2003.  (radians)
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
!               6-13 = Planetary longitudes, Mercury through Neptune.
!               14  = General accumulated precession in longitude.
!           3. FAD2K(14)- The CT time derivatives of the fundamental arguments.
!                      (radians/second)
!
      REAL*8 XJD, CT, CENT, EL, ELP, F, D, OM, SEC360, CENT2, CENT3, &
     &       CENT4, DAYSJ, ELC2(5), ELPC2(5), FC2(5), &
     &       DC2(5), OMC2(5), dTdt
      REAL*8 FA2K(14), FAD2K(14)
!
      Real*8 CENTJ, DJ2000
!
      PARAMETER  ( DJ2000 = 2451545.D0 ) ! Julian date at January 1.5, 2000
      PARAMETER  ( CENTJ  = 36525.0D0  ) ! Days per century
!
!     Integer*4 NOT, NOP, IDP(6)
!     Variables from:
!        1. CENTJ   -  The number of coordinate time days per Julian century.
!                      (days/century)
!        2. DJ2000  -  The Julian date of the epoch January 1.5, 2000. (days)
!
      Real*8           PI, TWOPI, HALFPI, CONVD, CONVDS, CONVHS, SECDAY
!              1. TWOPI   -  TWOPI
!              2. SECDAY  -  THE NUMBER OF COORDINATE TIME SECONDS PER
!                            COORDINATE TIME DAY. (SEC/DAY)
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
!      -Conform to IERS Conventions (1996)-
!xx   DATA ELC    / -0.00024470d0,       0.051635d0,  31.8792d0,
!xx  .          1717915923.2178d0,  485868.249036d0/
!xx   DATA ELPC   / -0.00001149d0,      -0.000136d0,  -0.5532d0,
!xx  .           129596581.0481d0,  1287104.79305d0/
!xx   DATA FC     /  0.00000417d0,      -0.001037d0, -12.7512d0,
!xx  .          1739527262.8478d0,  335779.526232d0/
!xx   DATA DC     / -0.00003169d0,       0.006593d0,  -6.3706d0,
!xx  .          1602961601.2090d0,  1072260.70369d0/
!xx   DATA OMC    /-0.00005939d0,        0.007702d0,   7.4722d0,
!xx  .           -6962890.2665d0,   450160.398036d0/
!
      PARAMETER  ( SEC360 = 1296000.0D0 ) ! arcseconds in one turn
!
!     DATA statements for the revised fundamental arguments.
!     Simons et al., 1994 values
!      -Conform to IERS Conventions (2003)-
      DATA ELC2   / -0.00024470d0,       0.051635d0,  31.8792d0, &
     &          1717915923.2178d0,  485868.249036d0/
      DATA ELPC2  / -0.00001149d0,      +0.000136d0,  -0.5532d0, &
     &           129596581.0481d0,  1287104.793048d0/
      DATA FC2    /  0.00000417d0,      -0.001037d0, -12.7512d0, &
     &          1739527262.8478d0,  335779.526232d0/
      DATA DC2    / -0.00003169d0,       0.006593d0,  -6.3706d0, &
     &          1602961601.2090d0,  1072260.703692d0/
      DATA OMC2   /-0.00005939d0,        0.007702d0,   7.4722d0, &
     &           -6962890.5431d0,   450160.398036d0/
      PARAMETER  ( PI = 3.1415926535897932D0 )
      PARAMETER  ( TWOPI  = 6.2831853071795865D0 )
      PARAMETER  ( HALFPI = 1.5707963267948966D0 )
      PARAMETER  ( CONVD  = 1.7453292519943296D-02 )
      PARAMETER  ( CONVDS = 4.8481368110953599D-06 )
      PARAMETER  ( CONVHS = 7.2722052166430399D-05 )
      PARAMETER  ( SECDAY = 86400.0D0 )
!
!  Programmer:
!    93.09.01  Norbert Zacharias - Fundamental arguments computation put into
!              separate subroutine, taken from old NUTG subroutine.
!    98.01.28  David Gordon - Coefficients and computations modified to conform
!              to IERS Conventions (1996).
!                    Jim Ryan 2002.09 Integer*4 conversion.
!    2003.     David Gordon - Revised for IERS Conventions (2003)
!    2006.02.10  pet Fixed a bug: constants CENTJ, CONVD and SECDAY were not
!                    initialized.
!    2006.07.14 DG.  Bug fix, DJ2000 not initialized. Pointed out by Thomas Artz. 
!                    Unused variables removed (EC, ARGP, NOT, NOP, IDP).
!
!-------------------------------------------------------------------------------
!  Compute the number of Julian days elapsed since the epoch January 1.5, 2000.
      DAYSJ = XJD + CT - DJ2000
!
!  Derivative of CENT w.r.t. time (centuries/sec)
      dTdt = 1.D0/(CENTJ*86400.D0)
!
!  Compute the number of Julian centuries elapsed since the epoch January 1.5,
!   2000.
      CENT  = ((XJD - DJ2000) + CT) / CENTJ
      CENT2 = CENT * CENT
      CENT3 = CENT * CENT2
      CENT4 = CENT2 * CENT2
!
!-----------------------------------------------------------
!
!  Revised computation of the fundamental arguments and derivatives.
!   IERS Conventions (2003)
!
      EL = ELC2(1)*CENT4 + ELC2(2)*CENT3 + ELC2(3)*CENT2 &
     &   + ELC2(4)*CENT  + ELC2(5)
      FA2K(1)  = DMOD( EL, SEC360 )
      FAD2K(1) = 4.D0*ELC2(1)*CENT3 + 3.D0*ELC2(2)*CENT2 &
     &       + 2.D0*ELC2(3)*CENT  +      ELC2(4)
!
      ELP = ELPC2(1)*CENT4 + ELPC2(2)*CENT3 + ELPC2(3)*CENT2 &
     &    + ELPC2(4)*CENT  + ELPC2(5)
      FA2K (2) = DMOD( ELP, SEC360 )
      FAD2K(2) = 4.D0*ELPC2(1)*CENT3 + 3.D0*ELPC2(2)*CENT2 &
     &       + 2.D0*ELPC2(3)*CENT  +      ELPC2(4)
!
      F = FC2(1)*CENT4 + FC2(2)*CENT3 + FC2(3)*CENT2 &
     &  + FC2(4)*CENT  + FC2(5)
      FA2K (3) = DMOD( F, SEC360 )
      FAD2K(3) = 4.D0*FC2(1)*CENT3 + 3.D0*FC2(2)*CENT2 &
     &       + 2.D0*FC2(3)*CENT  +      FC2(4)
!
      D = DC2(1)*CENT4 + DC2(2)*CENT3 + DC2(3)*CENT2 &
     &  + DC2(4)*CENT  + DC2(5)
      FA2K (4) = DMOD( D, SEC360 )
      FAD2K(4) = 4.D0*DC2(1)*CENT3 + 3.D0*DC2(2)*CENT2 &
     &       + 2.D0*DC2(3)*CENT  +      DC2(4)
!
      OM = OMC2(1)*CENT4 + OMC2(2)*CENT3 + OMC2(3)*CENT2 &
     &   + OMC2(4)*CENT  + OMC2(5)
      FA2K (5) = DMOD( OM, SEC360 )
      FAD2K(5) = 4.D0*OMC2(1)*CENT3 + 3.D0*OMC2(2)*CENT2 &
     &       + 2.D0*OMC2(3)*CENT  +      OMC2(4)
!  Convert to radians and radians/sec:
      FA2K(1)  =  FA2K(1) * CONVDS
      FA2K(2)  =  FA2K(2) * CONVDS
      FA2K(3)  =  FA2K(3) * CONVDS
      FA2K(4)  =  FA2K(4) * CONVDS
      FA2K(5)  =  FA2K(5) * CONVDS
      FAD2K(1)  =  FAD2K(1) * CONVDS/(SECDAY*CENTJ)
      FAD2K(2)  =  FAD2K(2) * CONVDS/(SECDAY*CENTJ)
      FAD2K(3)  =  FAD2K(3) * CONVDS/(SECDAY*CENTJ)
      FAD2K(4)  =  FAD2K(4) * CONVDS/(SECDAY*CENTJ)
      FAD2K(5)  =  FAD2K(5) * CONVDS/(SECDAY*CENTJ)
!
!  Planetary longitudes, Mercury through Neptune (Souchay et al. 1999).
      FA2K( 6) = ( 4.402608842D0 + 2608.7903141574D0 * CENT )
        FA2K(6) = DMOD(FA2K(6),TWOPI)
      FA2K( 7) = ( 3.176146697D0 + 1021.3285546211D0 * CENT )
        FA2K(7) = DMOD(FA2K(7),TWOPI)
      FA2K( 8) = ( 1.753470314D0 +  628.3075849991D0 * CENT )
        FA2K(8) = DMOD(FA2K(8),TWOPI)
      FA2K( 9) = ( 6.203480913D0 +  334.0612426700D0 * CENT )
        FA2K(9) = DMOD(FA2K(9),TWOPI)
      FA2K(10) = ( 0.599546497D0 +   52.9690962641D0 * CENT )
        FA2K(10) = DMOD(FA2K(10),TWOPI)
      FA2K(11) = ( 0.874016757D0 +   21.3299104960D0 * CENT )
        FA2K(11) = DMOD(FA2K(11),TWOPI)
      FA2K(12) = ( 5.481293872D0 +    7.4781598567D0 * CENT )
        FA2K(12) = DMOD(FA2K(12),TWOPI)
      FA2K(13) = ( 5.311886287D0 +    3.8133035638D0 * CENT )
        FA2K(13) = DMOD(FA2K(13),TWOPI)
!  General accumulated precession in longitude.
      FA2K(14) = ( 0.024381750D0*CENT + 0.00000538691D0*CENT2)
!
      FAD2K( 6) = 2608.7903141574D0 * dTdt
      FAD2K( 7) = 1021.3285546211D0 * dTdt
      FAD2K( 8) = 628.3075849991D0 * dTdt
      FAD2K( 9) = 334.0612426700D0 * dTdt
      FAD2K(10) = 52.9690962641D0 * dTdt
      FAD2K(11) = 21.3299104960D0 * dTdt
      FAD2K(12) = 7.4781598567D0 * dTdt
      FAD2K(13) = 3.8133035638D0 * dTdt
      FAD2K(14) = (0.024381750D0 + 2.D0*0.00000538691D0*CENT) * dTdt
!
!     WRITE (6,'(1X,A)') 'Debug for subroutine NUTFA10'
!     WRITE (6,8) ' CENT,  ', CENT
!   8         FORMAT(A,4D25.16/(7X,5D25.16))
!     Write (6,787) FA2K
!787  Format(' FA2K:',/,3(5E22.15,/))
!     Write (6,788) FAD2K
!788  Format(' FAD2K:',/,3(5E22.15,/))
!
      RETURN
      END
