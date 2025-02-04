      SUBROUTINE NERS_ERM_ANGS ( IPAR, IVRB, PREC_CODE, NUT_CODE, &
     &                           MJD, TAI, XPOL, YPOL, UT1_M_TAI, &
     &                           XPOL_RATE, YPOL_RATE, UT1_RATE,  &
     &                           S_ANG, S_ANG_RATE, &
     &                           DZETA, TETA, ZA, EPS_0, DZETA_RATE, &
     &                           TETA_RATE, ZA_RATE, EPS_0_RATE, &
     &                           E1_NUT, E2_NUT, DPSI, DEPS, &
     &                           DPSI_SEC, DEPS_SEC, DPSIR_SEC, DEPSR_SEC, &
     &                           E1_NUT_RATE, E2_NUT_RATE, DPSI_RATE, DEPS_RATE, &
     &                           E1_GDS, E2_GDS, DPSI_GDS, DEPS_GDS, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine NERS_ERM_ANGS takes as the input three Euler angles of the *
! *   Earth rotation (Xp, Yp, Ut1), their first time derivatives,        *
! *   the set of empirical harmonic variations of the Earth's rotation   *
! *   and computes interemediate angles that describes the Earth         *
! *   rotation. These angles can be used for computing Earth's rotation  *
! *   matrix following the Newcomb-Andoyer formalism.                    *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *        IPAR ( INTEGER*4 ) -- Debugging parameter. Should be 1        *
! *                              for normal work.                        *
! *        IVRB ( INTEGER*4 ) -- Verbosity level, 0 -- silent work, 4 -- *
! *                              verbose printout needed for debugging   *
! *                              only.                                   *
! *   PREC_CODE ( INTEGER*4 ) -- Code of the precession expansion.       *
! *                              Supported codes:                        *
! *                              PREC__LIESKE1977                        *
! *                              PREC__CAPITAINE2003                     *
! *    NUT_CODE ( INTEGER*4 ) -- Code of the nutation expansion.         *
! *                              Supported codes:                        *
! *                              NUT__WAHR1980                           *
! *                              NUT__IERS1996                           *
! *                              NUT__REN2000                            *
! *                              NUT__MHB2000                            *
! *                              NUT__MHB2000_TRANSF                     *
! *                              NUT__MHB2000_ADDON                      *
! *                              NUT__PETA                               *
! *                              NUT__PETB                               *
! *                              NUT__PETC                               *
! *        MJD ( INTEGER*4  ) -- Modified Julian Date. Unit: day.        *
! *        TAI ( INTEGER*4  ) -- Time in TAI since midnight. Unit: sec.  *
! *       XPOL (  REAL*8    ) -- X pole coordinate at the moment MJD,TAI *
! *                              Unit: rad.
! *       YPOL (  REAL*8    ) -- R pole coordinate at the moment MJD,TAI *
! *                              Unit: rad.
! *   UT1_M_TAI ( REAL*8    ) -- Value of the function UT1 minus TAI at  *
! *                              the moment MJD,TAI. Units: sec.         *
! *   XPOL_RATE ( REAL*8    ) -- Rate of change of X pole coordinate at  *
! *                              moment of time MJD,TAI, Units: rad/sec. *
! *   YPOL_RATE ( REAL*8    ) -- Rate of change of Y pole coordinate at  *
! *                              moment of time MJD,TAI, Units: rad/sec. *
! *    UT1_RATE ( REAL*8    ) -- Rate if change of function UT1 minus    *
! *                              TAI at the moment of time MJD,TAU.      *
! *                              Units: sec/sec.                         *
! *                                                                      *
! * _________________________ Output parameters: _______________________ *
! *                                                                      *
! *      S_ANG ( REAL*8     ) -- E3 Euler rotation angle (archaic name:  *
! *                              true Greenwich stellar angle).          *
! *                              Units: rad.                             *
! * S_ANG_RATE ( REAL*8     ) -- Rate of change of S_ANG. Units:         *
! *                              rad/sec.                                *
! * DZETA      ( REAL*8     ) -- Accumulated precession angle in right   *
! *                              ascension dzeta between epochs J2000.0  *
! *                              and MJD,TAI. Unit: rad.                 *
! * TETA       ( REAL*8     ) -- Accumulated precession angle in         *
! *                              declination teta between epochs J2000.0 *
! *                              and MJD,TAI. Unit: rad.                 *
! * ZA         ( REAL*8     ) -- Accumulated precession angle in right   *
! *                              ascension za between epochs J2000.0     *
! *                              and MJD,TAI. Unit: rad.                 *
! * EPS_0      ( REAL*8     ) -- Mean accumulated obliquiry angle eps_0  *
! *                              between epochs J2000.0 and MJD,TAI.     *
! *                              Unit: rad.                              *
! * DZETA_RATE ( REAL*8     ) -- First time derivative of angle dzeta    *
! *                              on epoch MJD,TAI. Unit rad/s.           *
! * TETA_RATE  ( REAL*8     ) -- First time derivative of angle teta     *
! *                              on epoch MJD,TAI. Unit rad/s.           *
! * ZA_RATE    ( REAL*8     ) -- First time derivative of angle za       *
! *                              on epoch MJD,TAI. Unit rad/s.           *
! * EPS_0_RATE ( REAL*8     ) -- First time derivative of angle eps_0    *
! *                              on epoch MJD,TAI. Unit rad/s.           *
! * E1_NUT      ( REAL*8    ) -- Nutation angle descibed as an Euler     *
! *                              angle wrt axis 1 on epoch MJD,TAI.      * 
! *                              Unit: rad.                              *
! * E2_NUT      ( REAL*8    ) -- Nutation angle descibed as an Euler     *
! *                              angle wrt axis 2 on epoch MJD,TAI.      * 
! *                              Unit: rad.                              *
! * DPSI        ( REAL*8    ) -- Nutation in longitude on epoch MJD,TAI  *
! *                              with the secular contribution that is   *
! *                              present in the precession expression    *
! *                              subtracted. Unit. rad.                  *
! * DEPS        ( REAL*8    ) -- Nutation in obliquity on epoch MJD,TAI  *
! *                              with the secular contribution that is   *
! *                              present in the precession expression    *
! *                              subtracted. Unit: rad.                  *
! * DPSI_SEC    ( REAL*8    ) -- Secular contribution in nutation in     *
! *                              longitude that is present in precession *
! *                              expression. Unit: rad.                  *
! * DEPS_SEC    ( REAL*8    ) -- Secular contribution in nutation in     *
! *                              inclination that is present in the      *
! *                              precession expression. Unit: rad.       *
! * DPSIR_SEC   ( REAL*8    ) -- Secular contribution in nutation in     *
! *                              longitude rate that is present in the   *
! *                              precession expression. Unit: rad/sec.   *
! * DEPSR_SEC   ( REAL*8    ) -- Secular contribution in nutation in     *
! *                              inclination rate that is present in the *
! *                              precession expression. Unit: rad/sec.   *
! * E1_NUT_RATE ( REAL*8    ) -- Time derivative of E1_NUT angle on      *
! *                              epoch MJD,TAI. Unit: rad/s.             *
! * E2_NUT_RATE ( REAL*8    ) -- Time derivative of E1_NUT angle on      *
! *                              epoch MJD,TAI. Unit: rad/s.             *
! * DPSI_RATE   ( REAL*8    ) -- Time derivative of DPSI angle on        *
! *                              epoch MJD,TAI. Unit: rad/s.             *
! * DEPS_RATE   ( REAL*8    ) -- Time derivative of DEPS angle on        *
! *                              epoch MJD,TAI. Unit: rad/s.             *
! * E1_GDS   ( REAL*8       ) -- Euler angle of the geodesic nutation    *
! *                              along the axis 1. Unit: rad.            *
! * E2_GDS   ( REAL*8       ) -- Euler angle of the geodesic nutation    *
! *                              along the axis 2. Unit: rad.            *
! * DPSI_GDS ( REAL*8       ) -- Geodesic nutation in longitude.         *
! *                              Unit: rad.                              *
! * DEPS_GDS ( REAL*8       ) -- Geodesic nutation in longitude.         *
! *                              Unit: rad.                              *
! *                                                                      *
! * _________________________ Modified parameters: _____________________ *
! *                                                                      *
! *    IUER ( INTEGER*4, OPT ) -- Universal error handler.               *
! *                           Input: switch IUER=0 -- no error messages  *
! *                                  will be generated even in the case  *
! *                                  of error. IUER=-1 -- in the case of *
! *                                  error the message will be put on    *
! *                                  stdout.                             *
! *                           Output: 0 in the case of successful        *
! *                                   completion and non-zero in the     *
! *                                   case of error.                     *
! *                                                                      *
! *  ### 08-DEC-2003   NERS_ERM_ANGS v4.1 (c) L. Petrov 21-APR-2018 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'astro_constants.i'
      INCLUDE   'heo.i'
      INCLUDE   'eec.i'
      INTEGER*4  IPAR, PREC_CODE, NUT_CODE, MJD, IVRB, IUER
      REAL*8     TAI, UT1_M_TAI, XPOL, YPOL, UT1_RATE, XPOL_RATE, YPOL_RATE
      REAL*8     DPSI_SEC, DEPS_SEC, DPSIR_SEC, DEPSR_SEC
      REAL*8     S0, EPSILON_IERS1996
      PARAMETER ( S0 = 1.75336855923396D0 )
      PARAMETER ( EPSILON_IERS1996 = 0.4090928041D0 )
      CHARACTER  STR*32
      REAL*8     DZETA__LIESKE1977(0:3), TETA__LIESKE1977(0:3), &
     &           ZA__LIESKE1977(0:3),    EPS__LIESKE1977(0:3),  &
     &           OMEGA__LIESKE1977(0:3)
      REAL*8     DZETA__SIMON1994(0:3),  TETA__SIMON1994(0:3),  &
     &           ZA__SIMON1994(0:3),     EPS__SIMON1994(0:3),   &
     &           OMEGA__SIMON1994(0:3)
      REAL*8     DZETA__IERS1996(0:3), TETA__IERS1996(0:3),  ZA__IERS1996(0:3), &
     &           EPS__IERS1996(0:3),   OMEGA__IERS1996(0:3), SOM__IERS1996(2)
      REAL*8     DZETA__IERS2003(0:3), TETA__IERS2003(0:3),  ZA__IERS2003(0:3)
!
! --- Coefficients of expansion of Newcomb-Andoyer variables in low degree
! --- polynomials
!
      DATA       DZETA__LIESKE1977, TETA__LIESKE1977, ZA__LIESKE1977, &
     &           EPS__LIESKE1977,   OMEGA__LIESKE1977                                               &
     &     /                                                                   &
     &              0.0D0,        2306.2181D0,  0.30188D0,  0.017998D0, & ! Dzeta
     &              0.0D0,        2004.3109D0, -0.42665D0, -0.041833D0, & ! Teta
     &              0.0D0,        2306.2181D0,  1.09468D0,  0.018203D0, & ! Za
     &          84381.448D0,       -46.8150D0, -0.00059D0,  0.001813D0, & ! Eps
     &         450160.280D0,  -6962890.539D0,   7.455D0,    0.008D0     & ! Om
     &     /
!
      DATA       DZETA__SIMON1994, TETA__SIMON1994, ZA__SIMON1994,   &
     &           EPS__SIMON1994,   OMEGA__SIMON1994                  &
     &     /                                                         &
     &            0.0D0,   2306.09097D0,   0.302226D0,  0.0180183D0, & ! Dzeta
     &            0.0D0,   2004.20207D0,  -0.426566D0, -0.0418238D0, & ! Teta
     &            0.0D0,   2306.09097D0,   1.095270D0,  0.0182667D0, & ! Za
     &        84381.412D0,  -46.809270D0, -0.000152D0,  0.0019989D0, & ! Eps
     &       450160.39804D0, -6962890.2665D0, 7.47220D0, 0.007702D0  & ! Om
     &     /
      DATA       DZETA__IERS1996, TETA__IERS1996, ZA__IERS1996, EPS__IERS1996, &
     &           OMEGA__IERS1996                                               &
     &     /                                                                   &
     &              0.0D0,        2306.2181D0,  0.30188D0,  0.017998D0, & ! Dzeta
     &              0.0D0,        2004.3109D0, -0.42665D0, -0.041833D0, & ! Teta
     &              0.0D0,        2306.2181D0,  1.09468D0,  0.018203D0, & ! Za
     &          84381.448D0,       -46.8150D0, -0.00059D0,  0.001813D0, & ! Eps
     &       450160.398036D0, -6962890.2665D0,  7.47220D0,  0.007702D0  & ! Om
     &     /
!
      DATA       DZETA__IERS2003, TETA__IERS2003, ZA__IERS2003 &
     &     /                                                   &
     &        2.5976176D0,  2306.0809506D0,  0.3019015D0,  0.0179663D0, & ! Dzeta
     &        0.0D0,        2004.1917476D0, -0.4269353D0, -0.0418251D0, & ! Teta
     &       -2.5976176D0,  2306.0803226D0,  1.0947790D0,  0.0182273D0  & ! Za
     &     /
!
      DATA       SOM__IERS1996  /                          &
     &                            0.00264D0,  0.000063D0   & ! arcsec
     &                          /
      REAL*8     DZETA, TETA, ZA, EPS_0, OMEGA, &
     &           DZETA__ARG(0:3), TETA__ARG(0:3), ZA__ARG(0:3), &
     &           EPS__ARG(0:3), OMEGA__ARG(0:3), SOM__ARG(2),   &
     &           TARG_TAI, SARG_TAI_2PI, TARG_TDB, TDB, S_ANG, DPSI, DEPS, &
     &           DPSI_RATE, DEPS_RATE, &
     &           E1_NUT, E2_NUT, E1_NUT_RATE, E2_NUT_RATE, &
     &           DZETA_RATE, TETA_RATE, ZA_RATE, EPS_0_RATE, OMEGA_RATE, &
     &           S_ANG_RATE, EE_CROSS, EE_CROSS_RATE, EEC_ARG
      REAL*8     RTM_YWOB(3,3),  RTM_XWOB(3,3),  RTM_DIU(3,3),    &
     &           RTM_N1(3,3),    RTM_N2(3,3),    RTM_N3(3,3),     &
     &           RTM_P1(3,3),    RTM_P2(3,3),    RTM_P3(3,3),     &
     &           RTM_TMP(3,3),   RTM_E1(3,3),    RTM_E2(3,3),     &
     &           DRTM_YWOB(3,3), DRTM_XWOB(3,3), DRTM_DIU(3,3),   &
     &           DRTM_N1(3,3),   DRTM_N2(3,3),   DRTM_N3(3,3),    &
     &           DRTM_P1(3,3),   DRTM_P2(3,3),   DRTM_P3(3,3),    &
     &           DRTM_1(3,3),    DRTM_2(3,3),    DRTM_3(3,3),     &
     &           DRTM_4(3,3),    DRTM_5(3,3),    DRTM_6(3,3),     &
     &           DRTM_7(3,3),    DRTM_8(3,3),    DRTM_9(3,3),     &
     &           SRTM_DIU(3,3),  SRTM_1(3,3),    SRTM_2(3,3),     &
     &           SRTM_3(3,3),    SRTM_4(3,3),    SRTM_5(3,3),     &
     &           SRTM_6(3,3),    SRTM_7(3,3),    SRTM_8(3,3)
      REAL*8     PRTM_DIU(3,3),  PRTM_XWOB(3,3), PRTM_YWOB(3,3)
      REAL*8     RTM_1(3,3)
      REAL*8     UT1_M_TDB, E1_GDS, E2_GDS, DPSI_GDS, DEPS_GDS
      REAL*8     EPSILON_0
      REAL*8     PSI_RATE_MHB2000, EPS_RATE_MHB2000
      REAL*8     PSI_OFFS_MHB2000, EPS_OFFS_MHB2000
      PARAMETER  ( PSI_RATE_MHB2000 =  -2.9965D0 ) ! mas/yr
      PARAMETER  ( EPS_RATE_MHB2000 =  -0.2524D0 ) ! mas/yr
      PARAMETER  ( PSI_OFFS_MHB2000 = -41.7750D0 ) ! For Calc-10 compatibility
      PARAMETER  ( EPS_OFFS_MHB2000 =  -6.8192D0 ) ! mas
!
      REAL*8     PSI_RATE_IERS96, EPS_RATE_IERS96
      REAL*8     PSI_OFFS_IERS96, EPS_OFFS_IERS96
!
      PARAMETER  ( PSI_RATE_IERS96 =  -2.957D0 ) ! mas/yr
      PARAMETER  ( EPS_RATE_IERS96 =  -0.227D0 ) ! mas/yr
      PARAMETER  ( PSI_OFFS_IERS96 = -43.1D0   ) ! mas
      PARAMETER  ( EPS_OFFS_IERS96 =  -5.1D0   ) ! mas
!
      PARAMETER  ( EPSILON_0 = 0.4090928041D0 )  ! rad
      REAL*8     CMAT1(3,3), CMAT2(3,3), CMAT3(3,3), CROSS_NUT_SCL
      INTEGER*4  J1, J2, IER
      CHARACTER, EXTERNAL :: MJDSEC_TO_DATE*30, TIM_TO_DATE*23
!
! --- Compute argument TDB at moment of time MJD/TAI
!
      CALL TAI_TO_TDB  ( MJD, TAI, TDB )
      TARG_TAI = (MJD - J2000__MJD - 0.5D0)*86400.0D0 + TAI
      TARG_TDB = (MJD - J2000__MJD - 0.5D0)*86400.0D0 + TDB
!
! --- Produce the quantity UT1 minus TDB
!
      UT1_M_TDB= UT1_M_TAI - (TDB - TAI)
      DPSI = 0.0D0
      DEPS = 0.0D0
      IF ( IVRB .GE. 4 ) THEN
           WRITE ( 6, * ) ' NUT_CODE =',NUT_CODE,' UT1_M_TDB=',UT1_M_TDB
      END IF
      IF ( IPAR .EQ. 1  .OR. IPAR .EQ. 2  .OR.  IPAR .EQ. 5 ) THEN
!
! -------- Transform parameters of Newcomb-Andoyer angles expansion to Si units
!
           IF ( PREC_CODE == PREC__LIESKE1977 ) THEN
                DZETA__ARG(0) = DZETA__LIESKE1977(0)
                DZETA__ARG(1) = DZETA__LIESKE1977(1)*ARCSEC__TO__RAD/CENT__TO__SEC
                DZETA__ARG(2) = DZETA__LIESKE1977(2)*ARCSEC__TO__RAD/CENT__TO__SEC**2
                DZETA__ARG(3) = DZETA__LIESKE1977(3)*ARCSEC__TO__RAD/CENT__TO__SEC**3
!
                TETA__ARG(0)  = TETA__LIESKE1977(0)
                TETA__ARG(1)  = TETA__LIESKE1977(1)*ARCSEC__TO__RAD/CENT__TO__SEC
                TETA__ARG(2)  = TETA__LIESKE1977(2)*ARCSEC__TO__RAD/CENT__TO__SEC**2
                TETA__ARG(3)  = TETA__LIESKE1977(3)*ARCSEC__TO__RAD/CENT__TO__SEC**3
!
                ZA__ARG(0)    = ZA__LIESKE1977(0)
                ZA__ARG(1)    = ZA__LIESKE1977(1)*ARCSEC__TO__RAD/CENT__TO__SEC
                ZA__ARG(2)    = ZA__LIESKE1977(2)*ARCSEC__TO__RAD/CENT__TO__SEC**2
                ZA__ARG(3)    = ZA__LIESKE1977(3)*ARCSEC__TO__RAD/CENT__TO__SEC**3
!
                EPS__ARG(0)   = EPS__LIESKE1977(0)*ARCSEC__TO__RAD
                EPS__ARG(1)   = EPS__LIESKE1977(1)*ARCSEC__TO__RAD/CENT__TO__SEC
                EPS__ARG(2)   = EPS__LIESKE1977(2)*ARCSEC__TO__RAD/CENT__TO__SEC**2
                EPS__ARG(3)   = EPS__LIESKE1977(3)*ARCSEC__TO__RAD/CENT__TO__SEC**3
!
                OMEGA__ARG(0) = OMEGA__LIESKE1977(0)*ARCSEC__TO__RAD
                OMEGA__ARG(1) = OMEGA__LIESKE1977(1)*ARCSEC__TO__RAD/CENT__TO__SEC
                OMEGA__ARG(2) = OMEGA__LIESKE1977(2)*ARCSEC__TO__RAD/CENT__TO__SEC**2
                OMEGA__ARG(3) = OMEGA__LIESKE1977(3)*ARCSEC__TO__RAD/CENT__TO__SEC**3
              ELSE IF ( PREC_CODE == PREC__SIMON1994 ) THEN
                DZETA__ARG(0) = DZETA__SIMON1994(0)
                DZETA__ARG(1) = DZETA__SIMON1994(1)*ARCSEC__TO__RAD/CENT__TO__SEC
                DZETA__ARG(2) = DZETA__SIMON1994(2)*ARCSEC__TO__RAD/CENT__TO__SEC**2
                DZETA__ARG(3) = DZETA__SIMON1994(3)*ARCSEC__TO__RAD/CENT__TO__SEC**3
!
                TETA__ARG(0)  = TETA__SIMON1994(0)
                TETA__ARG(1)  = TETA__SIMON1994(1)*ARCSEC__TO__RAD/CENT__TO__SEC
                TETA__ARG(2)  = TETA__SIMON1994(2)*ARCSEC__TO__RAD/CENT__TO__SEC**2
                TETA__ARG(3)  = TETA__SIMON1994(3)*ARCSEC__TO__RAD/CENT__TO__SEC**3
!
                ZA__ARG(0)    = ZA__SIMON1994(0)
                ZA__ARG(1)    = ZA__SIMON1994(1)*ARCSEC__TO__RAD/CENT__TO__SEC
                ZA__ARG(2)    = ZA__SIMON1994(2)*ARCSEC__TO__RAD/CENT__TO__SEC**2
                ZA__ARG(3)    = ZA__SIMON1994(3)*ARCSEC__TO__RAD/CENT__TO__SEC**3
!
                EPS__ARG(0)   = EPS__SIMON1994(0)*ARCSEC__TO__RAD
                EPS__ARG(1)   = EPS__SIMON1994(1)*ARCSEC__TO__RAD/CENT__TO__SEC
                EPS__ARG(2)   = EPS__SIMON1994(2)*ARCSEC__TO__RAD/CENT__TO__SEC**2
                EPS__ARG(3)   = EPS__SIMON1994(3)*ARCSEC__TO__RAD/CENT__TO__SEC**3
!
                OMEGA__ARG(0) = OMEGA__SIMON1994(0)*ARCSEC__TO__RAD
                OMEGA__ARG(1) = OMEGA__SIMON1994(1)*ARCSEC__TO__RAD/CENT__TO__SEC
                OMEGA__ARG(2) = OMEGA__SIMON1994(2)*ARCSEC__TO__RAD/CENT__TO__SEC**2
                OMEGA__ARG(3) = OMEGA__SIMON1994(3)*ARCSEC__TO__RAD/CENT__TO__SEC**3
              ELSE IF ( PREC_CODE == PREC__IERS1996 ) THEN
                DZETA__ARG(0) = DZETA__IERS1996(0)
                DZETA__ARG(1) = DZETA__IERS1996(1)*ARCSEC__TO__RAD/CENT__TO__SEC
                DZETA__ARG(2) = DZETA__IERS1996(2)*ARCSEC__TO__RAD/CENT__TO__SEC**2
                DZETA__ARG(3) = DZETA__IERS1996(3)*ARCSEC__TO__RAD/CENT__TO__SEC**3
!
                TETA__ARG(0)  = TETA__IERS1996(0)
                TETA__ARG(1)  = TETA__IERS1996(1)*ARCSEC__TO__RAD/CENT__TO__SEC
                TETA__ARG(2)  = TETA__IERS1996(2)*ARCSEC__TO__RAD/CENT__TO__SEC**2
                TETA__ARG(3)  = TETA__IERS1996(3)*ARCSEC__TO__RAD/CENT__TO__SEC**3
!
                ZA__ARG(0)    = ZA__IERS1996(0)
                ZA__ARG(1)    = ZA__IERS1996(1)*ARCSEC__TO__RAD/CENT__TO__SEC
                ZA__ARG(2)    = ZA__IERS1996(2)*ARCSEC__TO__RAD/CENT__TO__SEC**2
                ZA__ARG(3)    = ZA__IERS1996(3)*ARCSEC__TO__RAD/CENT__TO__SEC**3
!
                EPS__ARG(0)   = EPS__IERS1996(0)*ARCSEC__TO__RAD
                EPS__ARG(1)   = EPS__IERS1996(1)*ARCSEC__TO__RAD/CENT__TO__SEC
                EPS__ARG(2)   = EPS__IERS1996(2)*ARCSEC__TO__RAD/CENT__TO__SEC**2
                EPS__ARG(3)   = EPS__IERS1996(3)*ARCSEC__TO__RAD/CENT__TO__SEC**3
!
                OMEGA__ARG(0) = OMEGA__IERS1996(0)*ARCSEC__TO__RAD
                OMEGA__ARG(1) = OMEGA__IERS1996(1)*ARCSEC__TO__RAD/CENT__TO__SEC
                OMEGA__ARG(2) = OMEGA__IERS1996(2)*ARCSEC__TO__RAD/CENT__TO__SEC**2
                OMEGA__ARG(3) = OMEGA__IERS1996(3)*ARCSEC__TO__RAD/CENT__TO__SEC**3
              ELSE IF ( PREC_CODE == PREC__CAPITAINE2003 ) THEN
                DZETA__ARG(0) = DZETA__IERS2003(0)*ARCSEC__TO__RAD
                DZETA__ARG(1) = DZETA__IERS2003(1)*ARCSEC__TO__RAD/CENT__TO__SEC
                DZETA__ARG(2) = DZETA__IERS2003(2)*ARCSEC__TO__RAD/CENT__TO__SEC**2
                DZETA__ARG(3) = DZETA__IERS2003(3)*ARCSEC__TO__RAD/CENT__TO__SEC**3
!
                TETA__ARG(0)  = TETA__IERS2003(0)*ARCSEC__TO__RAD
                TETA__ARG(1)  = TETA__IERS2003(1)*ARCSEC__TO__RAD/CENT__TO__SEC
                TETA__ARG(2)  = TETA__IERS2003(2)*ARCSEC__TO__RAD/CENT__TO__SEC**2
                TETA__ARG(3)  = TETA__IERS2003(3)*ARCSEC__TO__RAD/CENT__TO__SEC**3
!
                ZA__ARG(0)    = ZA__IERS2003(0)*ARCSEC__TO__RAD
                ZA__ARG(1)    = ZA__IERS2003(1)*ARCSEC__TO__RAD/CENT__TO__SEC
                ZA__ARG(2)    = ZA__IERS2003(2)*ARCSEC__TO__RAD/CENT__TO__SEC**2
                ZA__ARG(3)    = ZA__IERS2003(3)*ARCSEC__TO__RAD/CENT__TO__SEC**3
!
                EPS__ARG(0)   = EPS__IERS1996(0)*ARCSEC__TO__RAD
                EPS__ARG(1)   = EPS__IERS1996(1)*ARCSEC__TO__RAD/CENT__TO__SEC
                EPS__ARG(2)   = EPS__IERS1996(2)*ARCSEC__TO__RAD/CENT__TO__SEC**2
                EPS__ARG(3)   = EPS__IERS1996(3)*ARCSEC__TO__RAD/CENT__TO__SEC**3
!
                OMEGA__ARG(0) = OMEGA__IERS1996(0)*ARCSEC__TO__RAD
                OMEGA__ARG(1) = OMEGA__IERS1996(1)*ARCSEC__TO__RAD/CENT__TO__SEC
                OMEGA__ARG(2) = OMEGA__IERS1996(2)*ARCSEC__TO__RAD/CENT__TO__SEC**2
                OMEGA__ARG(3) = OMEGA__IERS1996(3)*ARCSEC__TO__RAD/CENT__TO__SEC**3
           END IF
!
           SOM__ARG(1)   = SOM__IERS1996(1)*ARCSEC__TO__RAD
           SOM__ARG(2)   = SOM__IERS1996(2)*ARCSEC__TO__RAD
!
! -------- Compute nutation anlges
!
           IF ( IPAR .EQ. 1 ) THEN
!
! ------------- Take into account all constituents in the nutation expansion
!
                  IF ( NUT_CODE == NUT__REN2000 ) THEN
                     CALL NERS_HEO_REN2000 ( 4, TARG_TDB, UT1_M_TDB, &
     &                                       E1_NUT, E2_NUT, DPSI, DEPS, &
     &                                       E1_NUT_RATE, E2_NUT_RATE, &
     &                                       DPSI_RATE, DEPS_RATE, CROSS_NUT_SCL )
                  ELSE IF ( NUT_CODE == NUT__MHB2000 ) THEN
                     CALL NERS_HEO_MHB2000 ( 4, TARG_TDB, UT1_M_TDB, &
     &                                       E1_NUT, E2_NUT, DPSI, DEPS, &
     &                                       E1_NUT_RATE, E2_NUT_RATE, &
     &                                       DPSI_RATE, DEPS_RATE, CROSS_NUT_SCL )
                  ELSE
                     CALL CLRCH ( STR )
                     CALL INCH ( NUT_CODE, STR )
                     CALL ERR_LOG ( 2271, IUER, 'NERS_ERM_ANGS', 'Unknown '// &
     &                   'nutation expansion code: '//STR )
                     RETURN
                END IF
              ELSE IF ( IPAR .EQ. 2  .OR.  IPAR .EQ. 5 ) THEN
!
! ------------- Take into account only major constituents
!
                IF ( NUT_CODE == NUT__REN2000 ) THEN
                     CALL NERS_HEO_REN2000 ( 5, TARG_TDB, UT1_M_TDB, &
     &                                       E1_NUT, E2_NUT, DPSI, DEPS, &
     &                                       E1_NUT_RATE, E2_NUT_RATE, &
     &                                       DPSI_RATE, DEPS_RATE, CROSS_NUT_SCL )
                  ELSE IF ( NUT_CODE == NUT__MHB2000 ) THEN
                     CALL NERS_HEO_MHB2000 ( 5, TARG_TDB, UT1_M_TDB, &
     &                                       E1_NUT, E2_NUT, DPSI, DEPS, &
     &                                       E1_NUT_RATE, E2_NUT_RATE, &
     &                                       DPSI_RATE, DEPS_RATE, CROSS_NUT_SCL )
                  ELSE
                     CALL CLRCH ( STR )
                     CALL INCH ( NUT_CODE, STR )
                     CALL ERR_LOG ( 2272, IUER, 'NERS_ERM_ANGS', 'Unknown '// &
     &                   'nutation expansion code: '//STR )
                     RETURN
                END IF
           END IF
!
! -------- Compute contribution due to geodesic nutation to
! -------- E1,E2 and DPSI,DEPS
!
           CALL NERS_NUT_GEODESIC ( MJD, TAI, E1_GDS, E2_GDS, &
     &                              DPSI_GDS, DEPS_GDS )
!
! -------- Compute Newcomb-Andoyer angles for precssion at the moment of time
! -------- of interest
!
           DZETA = ((DZETA__ARG(3)*TARG_TDB  + &
     &               DZETA__ARG(2))*TARG_TDB + &
     &               DZETA__ARG(1))*TARG_TDB + &
     &               DZETA__ARG(0)
           TETA  = ((TETA__ARG(3)*TARG_TDB  + &
     &               TETA__ARG(2))*TARG_TDB + &
     &               TETA__ARG(1))*TARG_TDB + &
     &               TETA__ARG(0)
           ZA    = ((ZA__ARG(3)*TARG_TDB  + &
     &               ZA__ARG(2))*TARG_TDB + &
     &               ZA__ARG(1))*TARG_TDB + &
     &               ZA__ARG(0)
!
! -------- Compute values of time derivatives of Newcomb-Andoyer variables
!
           DZETA_RATE = (3.0D0*DZETA__ARG(3) *TARG_TDB + &
     &                   2.0D0*DZETA__ARG(2))*TARG_TDB + &
     &                   DZETA__ARG(1)
           TETA_RATE  = (3.0D0*TETA__ARG(3) *TARG_TDB  + &
     &                   2.0D0*TETA__ARG(2))*TARG_TDB  + &
     &                   TETA__ARG(1)
           ZA_RATE    = (3.0D0*ZA__ARG(3) *TARG_TDB    + &
     &                   2.0D0*ZA__ARG(2))*TARG_TDB    + &
     &                   ZA__ARG(1)
!
! -------- Compute the angle of mean inclination of ecliptic to the equator
!
           EPS_0 = ((EPS__ARG(3)*TARG_TDB  + &
     &               EPS__ARG(2))*TARG_TDB + &
     &               EPS__ARG(1))*TARG_TDB + &
     &               EPS__ARG(0)
           EPS_0_RATE = (3.0D0*EPS__ARG(3)*TARG_TDB    + &
     &                   2.0D0* EPS__ARG(2))*TARG_TDB  + &
     &                  EPS__ARG(1)
!
! -------- Compute Delauney angles Omega
!
           OMEGA = ((OMEGA__ARG(3)*TARG_TDB  + &
     &               OMEGA__ARG(2))*TARG_TDB + &
     &               OMEGA__ARG(1))*TARG_TDB + &
     &               OMEGA__ARG(0)
           OMEGA_RATE = (3.0D0*OMEGA__ARG(3) *TARG_TDB + &
     &                   2.0D0*OMEGA__ARG(2))*TARG_TDB + &
     &                   OMEGA__ARG(1)
!
           EE_CROSS      = 0.0D0
           EE_CROSS_RATE = 0.0D0
           DO 420 J2=1,N_EEC
              EEC_ARG = EEC_PHS(J2) + EEC_FRQ(J2)*TARG_TDB
              EE_CROSS = EE_CROSS + EEC_COS(J2)*DCOS(EEC_ARG) + &
     &                              EEC_SIN(J2)*DSIN(EEC_ARG)
              EE_CROSS_RATE = EE_CROSS_RATE + &
     &                        EEC_FRQ(J2)* ( - EEC_COS(J2)*DSIN(EEC_ARG) &
     &                                       + EEC_SIN(J2)*DCOS(EEC_ARG) )
 420       CONTINUE
!
           IF ( PREC_CODE == PREC__CAPITAINE2003 ) THEN
!
! ------------- Here is the tricky point: if precession expansion
! ------------- PREC__CAPITAINE2003, then we consider that the nutation
! ------------- expansion does not have secular term (drift)
!
                IF ( NUT_CODE == NUT__MHB2000        .OR. &
                     NUT_CODE == NUT__MHB2000_TRANSF      ) THEN
                     DEPS_SEC  = TARG_TDB*EPS_RATE_MHB2000/(86400.0D0*365.25D0) * &
     &                           MAS__TO__RAD
                     DPSI_SEC  = TARG_TDB*PSI_RATE_MHB2000/(86400.0D0*365.25D0) * &
     &                           MAS__TO__RAD
                     DEPSR_SEC = EPS_RATE_MHB2000/(86400.0D0*365.25D0) * MAS__TO__RAD
                     DPSIR_SEC = PSI_RATE_MHB2000/(86400.0D0*365.25D0) * MAS__TO__RAD
                   ELSE IF ( NUT_CODE == NUT__IERS1996 ) THEN
                     DEPS_SEC  = TARG_TDB*EPS_RATE_IERS96/(86400.0D0*365.25D0) * &
     &                           MAS__TO__RAD
                     DPSI_SEC  = TARG_TDB*PSI_RATE_IERS96/(86400.0D0*365.25D0) * &
     &                           MAS__TO__RAD
                     DEPSR_SEC = EPS_RATE_IERS96/(86400.0D0*365.25D0) * MAS__TO__RAD
                     DPSIR_SEC = PSI_RATE_IERS96/(86400.0D0*365.25D0) * MAS__TO__RAD
                END IF
!
                DPSI = DPSI - DPSI_SEC
                DEPS = DEPS - DEPS_SEC
                DPSI_RATE = DPSI_RATE - DPSIR_SEC
                DEPS_RATE = DEPS_RATE - DEPSR_SEC
           END IF
!
! -------- Compute the hourly angle of the spring equinox
! -------- Straitforward computation
! --------       S_ANG = (PI__NUM + S0) + &
! --------                OM__EAR *(TARG_TAI + UT1_M_TAI) + &
!
! -------- is not suitable due to loss of precision. This trick is done
! -------- for keeping S_ANG smaller: about 1 phase turn per year with
! -------- respect to J2000 epoch
!
           SARG_TAI_2PI = (MJD - J2000__MJD - 0.5D0)*(86400.0D0*OM__EAR - PI2) + &
     &                    TAI*OM__EAR + S0
           S_ANG = SARG_TAI_2PI + &
     &             OM__EAR *UT1_M_TAI                       + &
     &           ( ZA__ARG(1) + DZETA__ARG(1) )*TARG_TDB    + &
     &           ( ZA__ARG(2) + DZETA__ARG(2) )*TARG_TDB**2 + &
     &           ( ZA__ARG(3) + DZETA__ARG(3) - &
     &             ZA__ARG(1)*TETA__ARG(1)**2/6.0D0 )*TARG_TDB**3  &
     &             + DPSI*DCOS(EPS_0) &
     &             + CROSS_NUT_SCL &
     &             + EE_CROSS
!
! -------- ... And its rate of change
!
           S_ANG_RATE = OM__EAR *(1.0D0 + UT1_RATE) &
     &                +        ( ZA__ARG(1) + DZETA__ARG(1) )  &
     &                + 2.0D0*( ZA__ARG(2) + DZETA__ARG(2) )*TARG_TAI  &
     &                + 3.0D0*( ZA__ARG(3) + DZETA__ARG(3)  &
     &                        - ZA__ARG(1)*TETA__ARG(1)**2/6.0D0 )*TARG_TAI**2  &
     &                + DPSI_RATE*DCOS(EPS_0) &
     &                - DPSI*EPS_0_RATE*DSIN(EPS_0) &
     &                + CROSS_NUT_SCL/(1.D-8 + TARG_TDB) &
     &                + EE_CROSS_RATE
           IF ( IVRB .GE. 4 ) THEN
                WRITE ( 6, * ) ' TARG_TDB=', TARG_TDB
                WRITE ( 6, * ) '       S0=', S0
                WRITE ( 6, * ) '      S0A=', SARG_TAI_2PI + &
     &                                       OM__EAR *UT1_M_TAI + 3*PI2
                WRITE ( 6, * ) '      OM1=', OM__EAR + ZA__ARG(1) + DZETA__ARG(1)
                WRITE ( 6, * ) '      UT1=', TAI+UT1_M_TAI
                WRITE ( 6, * ) '       S2=', &
     &                         ( ZA__ARG(2) + DZETA__ARG(2))*TARG_TAI**2
                WRITE ( 6, * ) '       S3=', ( ZA__ARG(3) + DZETA__ARG(3) - &
     &                                         ZA__ARG(1)*TETA__ARG(1)**2/6.0D0 )* &
     &                                         TARG_TAI**3
                WRITE ( 6, * ) '    S_PRC=', &
     &                       ( ZA__ARG(1) + DZETA__ARG(1) )*TARG_TDB    + &
     &                       ( ZA__ARG(2) + DZETA__ARG(2) )*TARG_TDB**2 + &
     &                       ( ZA__ARG(3) + DZETA__ARG(3) - &
     &                         ZA__ARG(1)*TETA__ARG(1)**2/6.0D0 )*TARG_TDB**3 &
     &                       + 14.506d0*mas__to__rad - &
     &                       3.87354D0*MAS__TO__RAD*TARG_TDB/(86400.0D0*36525.0D0)
                WRITE ( 6, * ) '       S4=', DPSI*DCOS(EPS_0)
                WRITE ( 6, * ) '       S5=', EE_CROSS
                WRITE ( 6, * ) '       S6=', 14.506D0*MAS__TO__RAD
                WRITE ( 6, * ) '       S7=', -3.8736D0*MAS__TO__RAD*TARG_TAI/(86400.0D0*36525.0D0)
                WRITE ( 6, * ) '       S8=', 0.047D0*MAS__TO__RAD*TARG_TAI/(86400.0D0*36525.0D0)
                WRITE ( 6, * ) '      TAI=', TAI
                WRITE ( 6, * ) '    EPS_0=', EPS_0
                WRITE ( 6, * ) '    S_ANG=', S_ANG 
                WRITE ( 6, * ) '     DPSI=', DPSI
                WRITE ( 6, * ) '     DEPS=', DEPS
                WRITE ( 6, * ) '     XPOL=', XPOL
                WRITE ( 6, * ) '     YPOL=', YPOL
                WRITE ( 6, * ) '       ZA=', ZA
                WRITE ( 6, * ) '    DZETA=', DZETA
                WRITE ( 6, * ) '    THETA=', TETA
                WRITE ( 6, * ) 'UT1_M_TAI=', UT1_M_TAI
                WRITE ( 6, * ) ' targ_tdb/cent =', targ_tdb/(86400.0D0*36525.d0)
           END IF
        ELSE IF ( IPAR .EQ. 3 ) THEN
!
! -------- Compute Euler angles of the perturbed rotation
!
           IF ( NUT_CODE == NUT__REN2000 ) THEN
                CALL NERS_HEO_REN2000 ( 3, TARG_TDB, UT1_M_TDB, E1_NUT, E2_NUT, &
     &                                  DPSI, DEPS, E1_NUT_RATE, E2_NUT_RATE, &
     &                                  DPSI_RATE, DEPS_RATE, CROSS_NUT_SCL )
              ELSE IF ( NUT_CODE == NUT__MHB2000 ) THEN
                CALL NERS_HEO_MHB2000 ( 3, TARG_TDB, UT1_M_TDB, E1_NUT, E2_NUT, &
     &                                  DPSI, DEPS, E1_NUT_RATE, E2_NUT_RATE, &
     &                                  DPSI_RATE, DEPS_RATE, CROSS_NUT_SCL )
           END IF
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  NERS_ERM_ANGS  !#!#
