      SUBROUTINE VTD_ERM_NA ( IPAR, IVRB, PREC_CODE, NUT_CODE, NUT_GDS, &
     &                        EROT_COMPAT, NERS, MJD, TAI, &
     &                        XPOL,      YPOL,      UT1_M_TAI,  &
     &                        XPOL_RATE, YPOL_RATE, UT1_RATE,   &
     &                        S_ANG, S_ANG_RATE, &
     &                        L_HEO, HEO_EPOCH_SEC, HEO, &
     &                        TRS_TO_CRS, TRS_TO_CRS_DER1, &
     &                        TRS_TO_CRS_DER2, PTRS_TO_CRS_DEOP, &
     &                        DPTRS_TO_CRS_DEOP, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine VTD_ERM_NA computes the matrix of transformation from the  *
! *   terrestrial coordinate system to the celestial coordinate system   *
! *   as well as its first and second time derivatives using             *
! *   Newcomb-Andoyer formalism.                                         *
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
! *                              PREC__CAPITAINE2005                     *
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
! *     NUT_GDS ( INTEGER*4 ) -- Code of usage of geodesic nutation in   *
! *                              computations. Supported codes:          *
! *                              NUT__GDS_YES -- take into account.      *
! *                              NUT__GDS_NO  -- do not take into        *
! *                                              account.                *
! * EROT_COMPAT ( INTEGER*4 ) -- Code of cmpatibility with other         *
! *                              programs. Supported codes:              *
! *                              VTD__NONE   ( recommmended )            *
! *                              VTD__CALC10                             *
! *         MJD ( INTEGER*4 ) -- Modified Julian data on the midnight.   *
! *                              Units: days.                            *
! *         TAI ( REAL*8    ) -- Moment of time. Units: sec.             *
! *        XPOL ( REAL*8    ) -- X pole coordinate at the moment MJD,TAI.*
! *                              Units: rad.                             *
! *        YPOL ( REAL*8    ) -- Y pole coordinate at the moment MJD,TAI.*
! *                              Units: rad.                             *
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
! *      S_ANG      ( REAL*8    ) -- E3 Euler rotation angle (archaic    *
! *                                  name: true Greenwich stellar angle).*
! *                                  Units: rad.                         *
! * S_ANG_RATE      ( REAL*8    ) -- Rate of change of S_ANG. Units:     *
! *                                  rad/sec.                            *
! * TRS_TO_CRS      ( REAL*8    ) -- The matrix of transformation from   *
! *                                  the terrestrial reference system to *
! *                                  the celestial reference system at   *
! *                                  the moment of time MJD,TAI.         *
! *                                  Dimension: 3,3.                     *
! * TRS_TO_CRS_DER1 ( REAL*8    ) -- First time derivative of the        *
! *                                  transformation matrix from the      *
! *                                  terrestrial reference system to     *
! *                                  the celestial reference system at   *
! *                                  the moment of time MJD,TAI.         *
! *                                  Dimension: 3,3.                     *
! * TRS_TO_CRS_DER2 ( REAL*8    ) -- Second time derivative of the       *
! *                                  transformation matrix from the      *
! *                                  terrestrial reference system to     *
! *                                  the celestial reference system at   *
! *                                  the moment of time MJD,TAI.         *
! *                                  Dimension: 3,3.                     *
! * PTRS_TO_CRS_DEOP ( REAL*8   ) -- Arrays of derivatives of the        *
! *                                  transformation matrix from the      *
! *                                  terrestrial reference system to     *
! *                                  the celestial reference system at   *
! *                                  the moment of time MJD,TAI with     *
! *                                  respect to the small vector of      *
! *                                  rotation with Euler angles E1, E2,  *
! *                                  end E3. Dimension: 3,3,3. The last  *
! *                                  dimension runs over E1, E2 and E3   *
! *                                  Euler angles.                       *
! * DPTRS_TO_CRS_DEOP ( REAL*8  ) -- Arrays of mixed derivatives of the  *
! *                                  transformation matrix from the      *
! *                                  terrestrial reference system to     *
! *                                  the celestial reference system at   *
! *                                  the moment of time MJD,TAI with     *
! *                                  respect tim time and to the small   *
! *                                  vector of rotation with Euler       *
! *                                  angles E1, E2, end E3.              *
! *                                  Dimension: 3,3,3. The last          *
! *                                  dimension runs over E1, E2 and E3   *
! *                                  Euler angles.                       *
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
! *  ### 08-DEC-2003   VTD_ERM_NA   v5.2 (c) L. Petrov  27-OCT-2023 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'astro_constants.i'
      INCLUDE   'vtd.i'
      TYPE     ( NERS__TYPE ) :: NERS
      TYPE     ( HEO__STRUC ) HEO(*)
      INTEGER*4  IPAR, PREC_CODE, NUT_CODE, NUT_GDS, EROT_COMPAT, MJD, IVRB, &
     &           L_HEO, IUER
      REAL*8     TAI, UT1_M_TAI, XPOL, YPOL, UT1_RATE, XPOL_RATE, YPOL_RATE, &
     &           HEO_EPOCH_SEC, &
     &           TRS_TO_CRS(3,3), TRS_TO_CRS_DER1(3,3), TRS_TO_CRS_DER2(3,3), &
     &           PTRS_TO_CRS_DEOP(3,3,3), DPTRS_TO_CRS_DEOP(3,3,3)
      REAL*8     S0, EPSILON_IERS1996
      PARAMETER ( S0 = 1.7533685592339632D0 )
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
      REAL*8     DZETA__IERS2003(0:5), TETA__IERS2003(0:5),  ZA__IERS2003(0:5), &
     &           EPS__IERS2003(0:5), OMEGA__IERS2003(0:5)
      REAL*8     DZETA__IERS2010(0:5), TETA__IERS2010(0:5),  ZA__IERS2010(0:5), &
     &           EPS__IERS2010(0:5), OMEGA__IERS2010(0:5)
      REAL*8     SARG__ADDON_CAPITAINE2005(0:4), SARG__ADDON(0:4)
!
! --- Coefficients of expansion of Newcomb-Andoyer variables in low degree
! --- polynomials
!
      DATA       DZETA__LIESKE1977, TETA__LIESKE1977, ZA__LIESKE1977,   &
     &           EPS__LIESKE1977,   OMEGA__LIESKE1977                   &
     &     /                                                            &
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
! --- IERS TN32, section 5.5.1
!
      DATA       DZETA__IERS2003, TETA__IERS2003, ZA__IERS2003, EPS__IERS2003, OMEGA__IERS2003  &
     &     /                                                   &
     &            2.5976176D0,    2306.0809506D0,  0.3019015D0,  0.0179663D0,  -0.0000327D0,   -0.0000002D0, & ! Dzeta
     &            0.0D0,          2004.1917476D0, -0.4269353D0, -0.0418251D0,   0.0000601D0,   -0.0000001D0, & ! Teta
     &           -2.5976176D0,    2306.0803226D0,  1.0947790D0,  0.0182273D0,  -0.0000470D0,   -0.0000003D0, & ! Za
     &        84381.448D0,         -46.84024D0,   -0.00059D0,    0.001813D0,    0.0D0,          0.0D0,       & ! Eps_A
     &       450160.398036D0, -6962890.5431D0,     7.47220D0,    0.007702D0,   -0.00005939D0,   0.0D0        & ! Om
     &     /
!
! --- Table 4 in page 365 in Capitaine, N., P.T. Wallace, and J. Chapront 
! --- "Expression for IAU 2000 precession quantities", A&A, 432, 355-367 (2005).
!
      DATA       DZETA__IERS2010, TETA__IERS2010, ZA__IERS2010, EPS__IERS2010, OMEGA__IERS2010 &
     &     /     &
     &            2.650545D0,     2306.083277D0,   0.2988499D0,  0.01801828D0, -0.000000576D0, -0.0000003173D0, & ! Dzeta
     &            0.0D0,          2004.191903D0,  -0.4294934D0, -0.04182264D0, -0.000007089D0, -0.0000001274D0, & ! Teta
     &           -2.650545D0,     2306.077181D0,   1.0927348D0,  0.01826837D0, -0.000028596D0, -0.0000000368D0, & ! Za
     &        84381.406D0,         -46.836769D0,  -0.0001831D0,  0.00200340D0, -0.000000576D0, -0.0000000434D0, & ! Eps_A
     &       450160.398036D0, -6962890.5431D0,     7.47220D0,    0.007702D0,   -0.00005939D0,   0.0D0           & ! Om
     &     /
!
! --- Add-on terms to S_ang advocated by N. Capitaine. The first term is
! --- the so-called frame bias, the offset in right ascension from 
! --- Chapront 2002 analysis plus the accumulated difference on 2003.01.01 when 
! --- a more rigourous expression for UT1 is used.
! --- The origin of other terms is unclear. Their were produced 
! --- by differencing the theoretircal expression and the expression for GMST in Table 4 in 
! --- "Expression for IAU 2000 precession quantities", A&A, 432, 355-367 (2005).
!
!   SARG(1) =  46.12156534D0 - ( DZETA__IERS2010(1) + ZA__IERS2010(1) )
!   SARG(2) =   1.1395817D0  - ( DZETA__IERS2010(2) + ZA__IERS2010(2) )
!   SARG(3) = -4.4D-7        - ( ZA__ARG(3) + DZETA__ARG(3) - ZA__ARG(1)*TETA__ARG(1)**2/6.0D0 )
!   SARG(4) = -2.9956D-5     - ( ZA__ARG(4) + DZETA__ARG(4) - ZA__ARG(2)*TETA__ARG(1)**2/8.0D0 )
!
      DATA SARG__ADDON_CAPITAINE2005 &
     &     / &
     &         -0.000094D0, -3.924D-3, -3.000D-6, -1.017D-7, 1.211D-5 & ! arcsec and derivatives are in arc/centuries**(degree)
     &     /
!
!!      SARG(1) =  4612.156534D0  - ( 2306.083277D0  + 2306.077181D0  )
!!      SARG(2) =     1.3915817D0 - (    0.2988499D0 +    1.0927348D0 )
!!
!!      G1 =    0.01801828D0*ARCSEC__TO__RAD/CENT__TO__SEC**3
!!      G2 =    0.01826837D0*ARCSEC__TO__RAD/CENT__TO__SEC**3
!!      G3 = 2306.077181D0*ARCSEC__TO__RAD/CENT__TO__SEC
!!      G4 = 2004.191903D0*ARCSEC__TO__RAD/CENT__TO__SEC
!!      SARG(3) =  -4.4D-7 - (G1 + G2 - G3*G4**2/6.0D0 )/(ARCSEC__TO__RAD/CENT__TO__SEC**3)
!!
!!      T1 = -0.000000576D0*ARCSEC__TO__RAD/CENT__TO__SEC**4
!!      T2 = -0.000028596D0*ARCSEC__TO__RAD/CENT__TO__SEC**4
!!      T3 = 1.0927348D0*ARCSEC__TO__RAD/CENT__TO__SEC**2
!!      T4 = 2004.191903D0*ARCSEC__TO__RAD/CENT__TO__SEC
!!      SARG(4) =  -2.9956D-5  - (T1 + T2 - T3*T4**2/8.0D0 )/(ARCSEC__TO__RAD/CENT__TO__SEC**4)
!
!
      DATA       SOM__IERS1996  /                          &
     &                            0.00264D0,  0.000063D0   & ! arcsec
     &                          /
      REAL*8     DZETA, TETA, ZA, EPS_0, OMEGA, &
     &           DZETA__ARG(0:5), TETA__ARG(0:5), ZA__ARG(0:5), &
     &           EPS__ARG(0:5), OMEGA__ARG(0:5), SOM__ARG(2),   &
     &           TARG_TAI, SARG_TAI_2PI, TARG_TDB, TDB, S_ANG, DPSI, DEPS, &
     &           DPSI_RATE, DEPS_RATE, E1, E2, E1_RATE, E2_RATE, &
     &           DZETA_RATE, TETA_RATE, ZA_RATE, EPS_0_RATE, OMEGA_RATE, &
     &           S_ANG_RATE, EE_CROSS, EE_CROSS_RATE, EEC_ARG
      REAL*8     RTM_YWOB(3,3),  RTM_XWOB(3,3),   RTM_DIU(3,3),    &
     &           RTM_N1(3,3),    RTM_N2(3,3),     RTM_N3(3,3),     &
     &           RTM_P1(3,3),    RTM_P2(3,3),     RTM_P3(3,3),     &
     &           RTM_TMP(3,3),   RTM_E1(3,3),     RTM_E2(3,3),     &
     &           DRTM_YWOB(3,3), DRTM_XWOB(3,3),  DRTM_DIU(3,3),   &
     &           DRTM_N1(3,3),   DRTM_N2(3,3),    DRTM_N3(3,3),    &
     &           DRTM_P1(3,3),   DRTM_P2(3,3),    DRTM_P3(3,3),    &
     &           DRTM_1(3,3),    DRTM_2(3,3),     DRTM_3(3,3),     &
     &           DRTM_4(3,3),    DRTM_5(3,3),     DRTM_6(3,3),     &
     &           DRTM_7(3,3),    DRTM_8(3,3),     DRTM_9(3,3),     &
     &           SRTM_DIU(3,3),  SRTM_1(3,3),     SRTM_2(3,3),     &
     &           SRTM_3(3,3),    SRTM_4(3,3),     SRTM_5(3,3),     &
     &           SRTM_6(3,3),    SRTM_7(3,3),     SRTM_8(3,3)
      REAL*8     PRTM_DIU(3,3),  PRTM_XWOB(3,3),  PRTM_YWOB(3,3)
      REAL*8     PPRTM_DIU(3,3), PPRTM_XWOB(3,3), PPRTM_YWOB(3,3)
      REAL*8     DPRTM_DIU(3,3), DPRTM_XWOB(3,3), DPRTM_YWOB(3,3)
      REAL*8     RTM_1(3,3)
      REAL*8     HEO_VEC(3),   HEO_VEC_DER1(3),   HEO_VEC_DER2(3)
      REAL*8     HEO_MAT(3,3), HEO_MAT_DER1(3,3), HEO_MAT_DER2(3,3)
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
      REAL*8     CMAT1(3,3), CMAT2(3,3), CMAT3(3,3), CROSS_NUT_SCL, &
     &           PARS(NERS__MPAR)
      LOGICAL*1  FL_NO_PRECNUT_ROT
      INTEGER*4  J1, J2, L_PAR, IER
      CHARACTER, EXTERNAL :: MJDSEC_TO_DATE*30
!
      FL_NO_PRECNUT_ROT = .FALSE.
      CALL GETENVAR ( 'VTD_NO_PRECNUT_ROT', STR )
      IF ( STR(1:1) == 'Y' .OR. STR(1:1) == 'y' ) THEN
           FL_NO_PRECNUT_ROT = .TRUE.
      END IF
      CALL  NOUT_R8 (  9,   TRS_TO_CRS_DER1 )
      CALL  NOUT_R8 (  9,   TRS_TO_CRS_DER2 )
      CALL  NOUT_R8 ( 27,  PTRS_TO_CRS_DEOP )
      CALL  NOUT_R8 ( 27, DPTRS_TO_CRS_DEOP )
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
           WRITE ( 6, * ) ' NUT_CODE= ',NUT_CODE, ' NUT_CODE= ', PREC_CODE, ' UT1_M_TDB= ', SNGL(UT1_M_TDB)
           STR = MJDSEC_TO_DATE ( MJD, TAI, IER )
           WRITE ( 6, * ) ' Tai_date: '//STR(1:30)
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
                DZETA__ARG(4) = 0.0D0
                DZETA__ARG(5) = 0.0D0
!
                TETA__ARG(0)  = TETA__LIESKE1977(0)
                TETA__ARG(1)  = TETA__LIESKE1977(1)*ARCSEC__TO__RAD/CENT__TO__SEC
                TETA__ARG(2)  = TETA__LIESKE1977(2)*ARCSEC__TO__RAD/CENT__TO__SEC**2
                TETA__ARG(3)  = TETA__LIESKE1977(3)*ARCSEC__TO__RAD/CENT__TO__SEC**3
                TETA__ARG(4)  = 0.0D0
                TETA__ARG(5)  = 0.0D0
!
                ZA__ARG(0)    = ZA__LIESKE1977(0)
                ZA__ARG(1)    = ZA__LIESKE1977(1)*ARCSEC__TO__RAD/CENT__TO__SEC
                ZA__ARG(2)    = ZA__LIESKE1977(2)*ARCSEC__TO__RAD/CENT__TO__SEC**2
                ZA__ARG(3)    = ZA__LIESKE1977(3)*ARCSEC__TO__RAD/CENT__TO__SEC**3
                ZA__ARG(4)    = 0.0D0
                ZA__ARG(5)    = 0.0D0
!
                EPS__ARG(0)   = EPS__LIESKE1977(0)*ARCSEC__TO__RAD
                EPS__ARG(1)   = EPS__LIESKE1977(1)*ARCSEC__TO__RAD/CENT__TO__SEC
                EPS__ARG(2)   = EPS__LIESKE1977(2)*ARCSEC__TO__RAD/CENT__TO__SEC**2
                EPS__ARG(3)   = EPS__LIESKE1977(3)*ARCSEC__TO__RAD/CENT__TO__SEC**3
                EPS__ARG(4)   = 0.0D0
                EPS__ARG(5)   = 0.0D0
!
                OMEGA__ARG(0) = OMEGA__LIESKE1977(0)*ARCSEC__TO__RAD
                OMEGA__ARG(1) = OMEGA__LIESKE1977(1)*ARCSEC__TO__RAD/CENT__TO__SEC
                OMEGA__ARG(2) = OMEGA__LIESKE1977(2)*ARCSEC__TO__RAD/CENT__TO__SEC**2
                OMEGA__ARG(3) = OMEGA__LIESKE1977(3)*ARCSEC__TO__RAD/CENT__TO__SEC**3
                OMEGA__ARG(4) = 0.0D0
                OMEGA__ARG(5) = 0.0D0
              ELSE IF ( PREC_CODE == PREC__SIMON1994 ) THEN
                DZETA__ARG(0) = DZETA__SIMON1994(0)
                DZETA__ARG(1) = DZETA__SIMON1994(1)*ARCSEC__TO__RAD/CENT__TO__SEC
                DZETA__ARG(2) = DZETA__SIMON1994(2)*ARCSEC__TO__RAD/CENT__TO__SEC**2
                DZETA__ARG(3) = DZETA__SIMON1994(3)*ARCSEC__TO__RAD/CENT__TO__SEC**3
                DZETA__ARG(4) = 0.0D0
                DZETA__ARG(5) = 0.0D0
!
                TETA__ARG(0)  = TETA__SIMON1994(0)
                TETA__ARG(1)  = TETA__SIMON1994(1)*ARCSEC__TO__RAD/CENT__TO__SEC
                TETA__ARG(2)  = TETA__SIMON1994(2)*ARCSEC__TO__RAD/CENT__TO__SEC**2
                TETA__ARG(3)  = TETA__SIMON1994(3)*ARCSEC__TO__RAD/CENT__TO__SEC**3
                TETA__ARG(4)  = 0.0D0
                TETA__ARG(5)  = 0.0D0
!
                ZA__ARG(0)    = ZA__SIMON1994(0)
                ZA__ARG(1)    = ZA__SIMON1994(1)*ARCSEC__TO__RAD/CENT__TO__SEC
                ZA__ARG(2)    = ZA__SIMON1994(2)*ARCSEC__TO__RAD/CENT__TO__SEC**2
                ZA__ARG(3)    = ZA__SIMON1994(3)*ARCSEC__TO__RAD/CENT__TO__SEC**3
                ZA__ARG(4)    = 0.0D0
                ZA__ARG(5)    = 0.0D0
!
                EPS__ARG(0)   = EPS__SIMON1994(0)*ARCSEC__TO__RAD
                EPS__ARG(1)   = EPS__SIMON1994(1)*ARCSEC__TO__RAD/CENT__TO__SEC
                EPS__ARG(2)   = EPS__SIMON1994(2)*ARCSEC__TO__RAD/CENT__TO__SEC**2
                EPS__ARG(3)   = EPS__SIMON1994(3)*ARCSEC__TO__RAD/CENT__TO__SEC**3
                EPS__ARG(4)   = 0.0D0
                EPS__ARG(5)   = 0.0D0
!
                OMEGA__ARG(0) = OMEGA__SIMON1994(0)*ARCSEC__TO__RAD
                OMEGA__ARG(1) = OMEGA__SIMON1994(1)*ARCSEC__TO__RAD/CENT__TO__SEC
                OMEGA__ARG(2) = OMEGA__SIMON1994(2)*ARCSEC__TO__RAD/CENT__TO__SEC**2
                OMEGA__ARG(3) = OMEGA__SIMON1994(3)*ARCSEC__TO__RAD/CENT__TO__SEC**3
                OMEGA__ARG(4) = 0.0D0
                OMEGA__ARG(5) = 0.0D0
              ELSE IF ( PREC_CODE == PREC__IERS1996 ) THEN
                DZETA__ARG(0) = DZETA__IERS1996(0)
                DZETA__ARG(1) = DZETA__IERS1996(1)*ARCSEC__TO__RAD/CENT__TO__SEC
                DZETA__ARG(2) = DZETA__IERS1996(2)*ARCSEC__TO__RAD/CENT__TO__SEC**2
                DZETA__ARG(3) = DZETA__IERS1996(3)*ARCSEC__TO__RAD/CENT__TO__SEC**3
                DZETA__ARG(4) = 0.0D0
                DZETA__ARG(5) = 0.0D0
!
                TETA__ARG(0)  = TETA__IERS1996(0)
                TETA__ARG(1)  = TETA__IERS1996(1)*ARCSEC__TO__RAD/CENT__TO__SEC
                TETA__ARG(2)  = TETA__IERS1996(2)*ARCSEC__TO__RAD/CENT__TO__SEC**2
                TETA__ARG(3)  = TETA__IERS1996(3)*ARCSEC__TO__RAD/CENT__TO__SEC**3
                TETA__ARG(4)  = 0.0D0
                TETA__ARG(5)  = 0.0D0
!
                ZA__ARG(0)    = ZA__IERS1996(0)
                ZA__ARG(1)    = ZA__IERS1996(1)*ARCSEC__TO__RAD/CENT__TO__SEC
                ZA__ARG(2)    = ZA__IERS1996(2)*ARCSEC__TO__RAD/CENT__TO__SEC**2
                ZA__ARG(3)    = ZA__IERS1996(3)*ARCSEC__TO__RAD/CENT__TO__SEC**3
                ZA__ARG(4)    = 0.0D0
                ZA__ARG(5)    = 0.0D0
!
                EPS__ARG(0)   = EPS__IERS1996(0)*ARCSEC__TO__RAD
                EPS__ARG(1)   = EPS__IERS1996(1)*ARCSEC__TO__RAD/CENT__TO__SEC
                EPS__ARG(2)   = EPS__IERS1996(2)*ARCSEC__TO__RAD/CENT__TO__SEC**2
                EPS__ARG(3)   = EPS__IERS1996(3)*ARCSEC__TO__RAD/CENT__TO__SEC**3
                EPS__ARG(4)   = 0.0D0
                EPS__ARG(5)   = 0.0D0
!
                OMEGA__ARG(0) = OMEGA__IERS1996(0)*ARCSEC__TO__RAD
                OMEGA__ARG(1) = OMEGA__IERS1996(1)*ARCSEC__TO__RAD/CENT__TO__SEC
                OMEGA__ARG(2) = OMEGA__IERS1996(2)*ARCSEC__TO__RAD/CENT__TO__SEC**2
                OMEGA__ARG(3) = OMEGA__IERS1996(3)*ARCSEC__TO__RAD/CENT__TO__SEC**3
              ELSE IF ( PREC_CODE == PREC__CAPITAINE2003 .OR. PREC_CODE == VTD__NERS ) THEN
                DZETA__ARG(0) = DZETA__IERS2003(0)*ARCSEC__TO__RAD
                DZETA__ARG(1) = DZETA__IERS2003(1)*ARCSEC__TO__RAD/CENT__TO__SEC
                DZETA__ARG(2) = DZETA__IERS2003(2)*ARCSEC__TO__RAD/CENT__TO__SEC**2
                DZETA__ARG(3) = DZETA__IERS2003(3)*ARCSEC__TO__RAD/CENT__TO__SEC**3
                DZETA__ARG(4) = 0.0D0
                DZETA__ARG(5) = 0.0D0
!
                TETA__ARG(0)  = TETA__IERS2003(0)*ARCSEC__TO__RAD
                TETA__ARG(1)  = TETA__IERS2003(1)*ARCSEC__TO__RAD/CENT__TO__SEC
                TETA__ARG(2)  = TETA__IERS2003(2)*ARCSEC__TO__RAD/CENT__TO__SEC**2
                TETA__ARG(3)  = TETA__IERS2003(3)*ARCSEC__TO__RAD/CENT__TO__SEC**3
                TETA__ARG(4)  = 0.0D0
                TETA__ARG(5)  = 0.0D0
!
                ZA__ARG(0)    = ZA__IERS2003(0)*ARCSEC__TO__RAD
                ZA__ARG(1)    = ZA__IERS2003(1)*ARCSEC__TO__RAD/CENT__TO__SEC
                ZA__ARG(2)    = ZA__IERS2003(2)*ARCSEC__TO__RAD/CENT__TO__SEC**2
                ZA__ARG(3)    = ZA__IERS2003(3)*ARCSEC__TO__RAD/CENT__TO__SEC**3
                ZA__ARG(4)    = 0.0D0
                ZA__ARG(5)    = 0.0D0
!
                EPS__ARG(0)   = EPS__IERS2003(0)*ARCSEC__TO__RAD
                EPS__ARG(1)   = EPS__IERS2003(1)*ARCSEC__TO__RAD/CENT__TO__SEC
                EPS__ARG(2)   = EPS__IERS2003(2)*ARCSEC__TO__RAD/CENT__TO__SEC**2
                EPS__ARG(3)   = EPS__IERS2003(3)*ARCSEC__TO__RAD/CENT__TO__SEC**3
                EPS__ARG(4)   = 0.0D0
                EPS__ARG(5)   = 0.0D0
!
                OMEGA__ARG(0) = OMEGA__IERS1996(0)*ARCSEC__TO__RAD
                OMEGA__ARG(1) = OMEGA__IERS1996(1)*ARCSEC__TO__RAD/CENT__TO__SEC
                OMEGA__ARG(2) = OMEGA__IERS1996(2)*ARCSEC__TO__RAD/CENT__TO__SEC**2
                OMEGA__ARG(3) = OMEGA__IERS1996(3)*ARCSEC__TO__RAD/CENT__TO__SEC**3
                OMEGA__ARG(4) = 0.0D0
                OMEGA__ARG(5) = 0.0D0
              ELSE IF ( PREC_CODE == PREC__CAPITAINE2005 ) THEN
                DZETA__ARG(0) = DZETA__IERS2010(0)*ARCSEC__TO__RAD
                DZETA__ARG(1) = DZETA__IERS2010(1)*ARCSEC__TO__RAD/CENT__TO__SEC
                DZETA__ARG(2) = DZETA__IERS2010(2)*ARCSEC__TO__RAD/CENT__TO__SEC**2
                DZETA__ARG(3) = DZETA__IERS2010(3)*ARCSEC__TO__RAD/CENT__TO__SEC**3
                DZETA__ARG(4) = DZETA__IERS2010(4)*ARCSEC__TO__RAD/CENT__TO__SEC**4
                DZETA__ARG(5) = DZETA__IERS2010(5)*ARCSEC__TO__RAD/CENT__TO__SEC**5
!
                TETA__ARG(0)  = TETA__IERS2010(0)*ARCSEC__TO__RAD
                TETA__ARG(1)  = TETA__IERS2010(1)*ARCSEC__TO__RAD/CENT__TO__SEC
                TETA__ARG(2)  = TETA__IERS2010(2)*ARCSEC__TO__RAD/CENT__TO__SEC**2
                TETA__ARG(3)  = TETA__IERS2010(3)*ARCSEC__TO__RAD/CENT__TO__SEC**3
                TETA__ARG(4)  = TETA__IERS2010(4)*ARCSEC__TO__RAD/CENT__TO__SEC**4
                TETA__ARG(5)  = TETA__IERS2010(5)*ARCSEC__TO__RAD/CENT__TO__SEC**5
!
                ZA__ARG(0)    = ZA__IERS2010(0)*ARCSEC__TO__RAD
                ZA__ARG(1)    = ZA__IERS2010(1)*ARCSEC__TO__RAD/CENT__TO__SEC
                ZA__ARG(2)    = ZA__IERS2010(2)*ARCSEC__TO__RAD/CENT__TO__SEC**2
                ZA__ARG(3)    = ZA__IERS2010(3)*ARCSEC__TO__RAD/CENT__TO__SEC**3
                ZA__ARG(4)    = ZA__IERS2010(4)*ARCSEC__TO__RAD/CENT__TO__SEC**4
                ZA__ARG(5)    = ZA__IERS2010(5)*ARCSEC__TO__RAD/CENT__TO__SEC**5
!
                EPS__ARG(0)   = EPS__IERS2010(0)*ARCSEC__TO__RAD
                EPS__ARG(1)   = EPS__IERS2010(1)*ARCSEC__TO__RAD/CENT__TO__SEC
                EPS__ARG(2)   = EPS__IERS2010(2)*ARCSEC__TO__RAD/CENT__TO__SEC**2
                EPS__ARG(3)   = EPS__IERS2010(3)*ARCSEC__TO__RAD/CENT__TO__SEC**3
                EPS__ARG(4)   = EPS__IERS2010(4)*ARCSEC__TO__RAD/CENT__TO__SEC**4
                EPS__ARG(5)   = EPS__IERS2010(5)*ARCSEC__TO__RAD/CENT__TO__SEC**5
!
                OMEGA__ARG(0) = OMEGA__IERS2010(0)*ARCSEC__TO__RAD
                OMEGA__ARG(1) = OMEGA__IERS2010(1)*ARCSEC__TO__RAD/CENT__TO__SEC
                OMEGA__ARG(2) = OMEGA__IERS2010(2)*ARCSEC__TO__RAD/CENT__TO__SEC**2
                OMEGA__ARG(3) = OMEGA__IERS2010(3)*ARCSEC__TO__RAD/CENT__TO__SEC**3
                OMEGA__ARG(4) = OMEGA__IERS2010(4)*ARCSEC__TO__RAD/CENT__TO__SEC**4
                OMEGA__ARG(5) = OMEGA__IERS2010(5)*ARCSEC__TO__RAD/CENT__TO__SEC**5
!
                SARG__ADDON(0) = SARG__ADDON_CAPITAINE2005(0)*ARCSEC__TO__RAD
                SARG__ADDON(1) = SARG__ADDON_CAPITAINE2005(1)*ARCSEC__TO__RAD/CENT__TO__SEC
                SARG__ADDON(2) = SARG__ADDON_CAPITAINE2005(2)*ARCSEC__TO__RAD/CENT__TO__SEC**2
                SARG__ADDON(3) = SARG__ADDON_CAPITAINE2005(3)*ARCSEC__TO__RAD/CENT__TO__SEC**3
                SARG__ADDON(4) = SARG__ADDON_CAPITAINE2005(4)*ARCSEC__TO__RAD/CENT__TO__SEC**4
              ELSE IF ( PREC_CODE == VTD__UNDF ) THEN
                DZETA__ARG  = 0.0D0
                TETA__ARG   = 0.0D0
                ZA__ARG     = 0.0D0
                EPS__ARG    = 0.0D0
                OMEGA__ARG  = 0.0D0
                SARG__ADDON = 0.0D0
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
                IF ( NUT_CODE == NUT__WAHR1980 ) THEN
                     CALL HEO_WAHR1980 ( 4, TARG_TDB, UT1_M_TDB, &
     &                                   E1, E2, DPSI, DEPS, E1_RATE, E2_RATE, &
     &                                   DPSI_RATE, DEPS_RATE, CROSS_NUT_SCL  )
                  ELSE IF ( NUT_CODE == NUT__IERS1996 ) THEN
                     CALL HEO_IERS1996 ( 4, TARG_TDB, UT1_M_TDB, &
     &                                   E1, E2, DPSI, DEPS, E1_RATE, E2_RATE, &
     &                                   DPSI_RATE, DEPS_RATE, CROSS_NUT_SCL  )
                  ELSE IF ( NUT_CODE == NUT__REN2000 ) THEN
                     CALL HEO_REN2000  ( 4, TARG_TDB, UT1_M_TDB, &
     &                                   E1, E2, DPSI, DEPS, E1_RATE, E2_RATE, &
     &                                   DPSI_RATE, DEPS_RATE, CROSS_NUT_SCL )
                  ELSE IF ( NUT_CODE == NUT__MHB2000 ) THEN
                     CALL HEO_MHB2000  ( 4, TARG_TDB, UT1_M_TDB, E1, E2, DPSI, &
     &                                   DEPS, E1_RATE, E2_RATE, &
     &                                   DPSI_RATE, DEPS_RATE, CROSS_NUT_SCL )
                  ELSE IF ( NUT_CODE == NUT__MHB2000_TRANSF ) THEN
                     CALL HEO_MHB2000_TRANSF ( 4, TARG_TDB, UT1_M_TDB, &
     &                                   E1, E2, DPSI, DEPS, E1_RATE, E2_RATE, &
     &                                   DPSI_RATE, DEPS_RATE, CROSS_NUT_SCL )
                  ELSE IF ( NUT_CODE == NUT__PETA ) THEN
                     CALL HEO_EMPI ( 4, NUT__PETA, TARG_TDB, UT1_M_TDB, &
     &                               E1, E2, DPSI, DEPS, E1_RATE, E2_RATE, &
     &                               DPSI_RATE, DEPS_RATE, CROSS_NUT_SCL )
                  ELSE IF ( NUT_CODE == NUT__PETB ) THEN
                     CALL HEO_EMPI ( 4, NUT__PETB, TARG_TDB, UT1_M_TDB, &
     &                               E1, E2, DPSI, DEPS, E1_RATE, E2_RATE, &
     &                               DPSI_RATE, DEPS_RATE, CROSS_NUT_SCL )
                  ELSE IF ( NUT_CODE == NUT__PETC ) THEN
                     CALL HEO_EMPI ( 4, NUT__PETC, TARG_TDB, UT1_M_TDB, &
     &                               E1, E2, DPSI, DEPS, E1_RATE, E2_RATE, &
     &                               DPSI_RATE, DEPS_RATE, CROSS_NUT_SCL )
                  ELSE IF ( NUT_CODE == VTD__NERS ) THEN
                     IF ( NERS%FCS%NUT_APR_MOD == NERS_MHB_2000_STR ) THEN
                          CROSS_NUT_SCL = NERS_CROSS_NUT_RATE_E3_MHB2000 
                        ELSE IF ( NERS%FCS%NUT_APR_MOD == NERS_REN_2000_STR ) THEN
                          CROSS_NUT_SCL = NERS_CROSS_NUT_RATE_E3_REN2000 
                        ELSE
                          CROSS_NUT_SCL = NERS_CROSS_NUT_RATE_E3_MHB2000 
                     END IF
!
                     CALL ERR_PASS     ( IUER, IER ) 
                     CALL NERS_GET_EOP ( NERS, (MJD - J2000__MJD)*86400.0D0 + TAI, &
     &                         'nutr', NERS__MPAR, L_PAR, PARS, IER )
                     IF ( IER .NE. 0 ) THEN
                          CALL ERR_LOG ( 2271, IUER, 'VTD_ERM_MA', 'Error in '// &
     &                        'getting the Earth orientation parameters using NERS' )
                          RETURN
                     END IF
                     DPSI      = PARS(1)
                     DEPS      = PARS(2)
                     DPSI_RATE = PARS(3)
                     DEPS_RATE = PARS(4)
                  ELSE IF ( NUT_CODE == VTD__UNDF ) THEN
                     DPSI      = 0.0D0
                     DEPS      = 0.0D0
                     DPSI_RATE = 0.0D0
                     DEPS_RATE = 0.0D0
                     CROSS_NUT_SCL = 0.0D0
                  ELSE
                     CALL CLRCH ( STR )
                     CALL INCH ( NUT_CODE, STR )
                     CALL ERR_LOG ( 2272, IUER, 'VTD_ERM_NA', 'Unknown '// &
     &                   'nutation expansion code: '//STR )
                     RETURN
                END IF
              ELSE IF ( IPAR .EQ. 2  .OR.  IPAR .EQ. 5 ) THEN
!
! ------------- Take into account only major constituents
!
                IF ( NUT_CODE == NUT__WAHR1980 ) THEN
                     CALL HEO_WAHR1980 ( 5, TARG_TDB, UT1_M_TDB, &
     &                                   E1, E2, DPSI, DEPS, &
     &                                   E1_RATE, E2_RATE, &
     &                                   DPSI_RATE, DEPS_RATE, CROSS_NUT_SCL )
                  ELSE IF ( NUT_CODE == NUT__IERS1996 ) THEN
                     CALL HEO_IERS1996 ( 5, TARG_TDB, UT1_M_TDB, &
     &                                   E1, E2, DPSI, DEPS, E1_RATE, E2_RATE, &
     &                                   DPSI_RATE, DEPS_RATE, CROSS_NUT_SCL )
                  ELSE IF ( NUT_CODE == NUT__REN2000 ) THEN
                     CALL HEO_REN2000  ( 5, TARG_TDB, UT1_M_TDB, &
     &                                   E1, E2, DPSI, DEPS, E1_RATE, E2_RATE, &
     &                                   DPSI_RATE, DEPS_RATE, CROSS_NUT_SCL )
                  ELSE IF ( NUT_CODE == NUT__MHB2000 ) THEN
                     CALL HEO_MHB2000  ( 5, TARG_TDB, UT1_M_TDB, &
     &                                   E1, E2, DPSI, DEPS, E1_RATE, E2_RATE, &
     &                                   DPSI_RATE, DEPS_RATE, CROSS_NUT_SCL )
                  ELSE IF ( NUT_CODE == NUT__MHB2000_TRANSF ) THEN
                     CALL HEO_MHB2000_TRANSF ( 5, TARG_TDB, UT1_M_TDB, &
     &                                   E1, E2, DPSI, DEPS, E1_RATE, E2_RATE, &
     &                                   DPSI_RATE, DEPS_RATE, CROSS_NUT_SCL )
                  ELSE IF ( NUT_CODE == NUT__PETA ) THEN
                     CALL HEO_EMPI ( 5, NUT__PETA, TARG_TDB, UT1_M_TDB, &
     &                               E1, E2, DPSI, DEPS, E1_RATE, E2_RATE, &
     &                               DPSI_RATE, DEPS_RATE, CROSS_NUT_SCL )
                  ELSE IF ( NUT_CODE == NUT__PETB ) THEN
                     CALL HEO_EMPI ( 5, NUT__PETB, TARG_TDB, UT1_M_TDB, &
     &                               E1, E2, DPSI, DEPS, E1_RATE, E2_RATE, &
     &                               DPSI_RATE, DEPS_RATE, CROSS_NUT_SCL )
                  ELSE IF ( NUT_CODE == NUT__PETC ) THEN
                     CALL HEO_EMPI ( 5, NUT__PETC, TARG_TDB, UT1_M_TDB, &
     &                               E1, E2, DPSI, DEPS, E1_RATE, E2_RATE, &
     &                               DPSI_RATE, DEPS_RATE, CROSS_NUT_SCL )
                  ELSE
                     CALL CLRCH ( STR )
                     CALL INCH ( NUT_CODE, STR )
                     CALL ERR_LOG ( 2273, IUER, 'VTD_ERM_NA', 'Unknown '// &
     &                   'nutation expansion code: '//STR )
                     RETURN
                END IF
           END IF
!
           IF ( NUT_GDS .EQ. NUT__GDS_YES ) THEN
!
! -------------- Compute contribution due to geodesic nutation to
! -------------- E1,E2 and DPSI,DEPS
!
                CALL NUT_GEODESIC ( MJD, TAI, E1_GDS, E2_GDS, &
     &                              DPSI_GDS, DEPS_GDS )
                E1 = E1 + E1_GDS
                E2 = E2 + E2_GDS
                DPSI = DPSI + DPSI_GDS
                DEPS = DEPS + DEPS_GDS
           END IF
!
! -------- Compute Newcomb-Andoyer angles for precssion at the moment of time
! -------- of interest
!
           DZETA = (((( DZETA__ARG(5)  *TARG_TDB + &
     &                  DZETA__ARG(4) )*TARG_TDB + &
     &                  DZETA__ARG(3) )*TARG_TDB + &
     &                  DZETA__ARG(2) )*TARG_TDB + &
     &                  DZETA__ARG(1) )*TARG_TDB + &
     &                  DZETA__ARG(0)
           TETA  = (((( TETA__ARG(5)  *TARG_TDB  + &
     &                  TETA__ARG(4) )*TARG_TDB  + &
     &                  TETA__ARG(3) )*TARG_TDB  + &
     &                  TETA__ARG(2) )*TARG_TDB  + &
     &                  TETA__ARG(1) )*TARG_TDB  + &
     &                  TETA__ARG(0)
           ZA    = (((( ZA__ARG(5)  *TARG_TDB + &
     &                  ZA__ARG(4) )*TARG_TDB + &
     &                  ZA__ARG(3) )*TARG_TDB + &
     &                  ZA__ARG(2) )*TARG_TDB + &
     &                  ZA__ARG(1) )*TARG_TDB + &
     &                  ZA__ARG(0)
!
! -------- Compute values of time derivatives of Newcomb-Andoyer variables
!
           DZETA_RATE = ((( 5.0D0*DZETA__ARG(5)  *TARG_TDB + &
     &                      4.0D0*DZETA__ARG(4) )*TARG_TDB + &
     &                      3.0D0*DZETA__ARG(3) )*TARG_TDB + &
     &                      2.0D0*DZETA__ARG(2) )*TARG_TDB + &
     &                            DZETA__ARG(1)
           TETA_RATE  = ((( 5.0D0*TETA__ARG(5)  *TARG_TDB  + &
     &                      4.0D0*TETA__ARG(4) )*TARG_TDB  + &
     &                      3.0D0*TETA__ARG(3) )*TARG_TDB  + &
     &                      2.0D0*TETA__ARG(2) )*TARG_TDB  + &
     &                            TETA__ARG(1  )
           ZA_RATE    = ((( 5.0D0*ZA__ARG(5)  *TARG_TDB    + &
     &                      3.0D0*ZA__ARG(4) )*TARG_TDB    + &
     &                      3.0D0*ZA__ARG(3) )*TARG_TDB    + &
     &                      2.0D0*ZA__ARG(2) )*TARG_TDB    + &
     &                            ZA__ARG(1)
!
! -------- Compute the angle of mean inclination of ecliptic to the equator
!
           EPS_0 = (((( EPS__ARG(5)  *TARG_TDB + &
     &                  EPS__ARG(4) )*TARG_TDB + &
     &                  EPS__ARG(3) )*TARG_TDB + &
     &                  EPS__ARG(2) )*TARG_TDB + &
     &                  EPS__ARG(1) )*TARG_TDB + &
     &                  EPS__ARG(0)
           EPS_0_RATE  = ((( 5.0D0*EPS__ARG(5)  *TARG_TDB  + &
     &                       4.0D0*EPS__ARG(4) )*TARG_TDB  + &
     &                       3.0D0*EPS__ARG(3) )*TARG_TDB  + &
     &                       2.0D0*EPS__ARG(2) )*TARG_TDB  + &
     &                             EPS__ARG(1)
!
! -------- Compute Delauney angle Omega
!
           OMEGA = ((( OMEGA__ARG(4)  *TARG_TDB + &
     &                 OMEGA__ARG(3) )*TARG_TDB + &
     &                 OMEGA__ARG(2) )*TARG_TDB + &
     &                 OMEGA__ARG(1) )*TARG_TDB + &
     &                 OMEGA__ARG(0)
           OMEGA_RATE = (( 4.0D0*OMEGA__ARG(4) *TARG_TDB + &
     &                     3.0D0*OMEGA__ARG(3))*TARG_TDB + &
     &                     2.0D0*OMEGA__ARG(2))*TARG_TDB + &
     &                           OMEGA__ARG(1)
!
! -------- Computation of cross-nutation terms
!
           EE_CROSS      = 0.0D0
           EE_CROSS_RATE = 0.0D0
           IF ( NUT_CODE == NUT__PETA ) THEN
                DO 410 J1=1,N_EEC_PETA
                   EEC_ARG = EEC_PETA_PHS(J1) + EEC_PETA_FRQ(J1)*TARG_TDB
                   EE_CROSS = EE_CROSS + EEC_PETA_COS(J1)*DCOS(EEC_ARG) + &
     &                                   EEC_PETA_SIN(J1)*DSIN(EEC_ARG)
                   EE_CROSS_RATE = EE_CROSS_RATE + EEC_PETA_FRQ(J1)* &
     &                             ( - EEC_PETA_COS(J1)*DSIN(EEC_ARG) &
     &                               + EEC_PETA_SIN(J1)*DCOS(EEC_ARG) )
 410            CONTINUE
              ELSE IF ( NUT_CODE == NUT__PETC ) THEN
                EE_CROSS = 0.0D0
                EE_CROSS_RATE = 0.0D0
              ELSE
                DO 420 J2=1,N_EEC
                   EEC_ARG = EEC_PHS(J2) + EEC_FRQ(J2)*TARG_TDB
                   EE_CROSS = EE_CROSS + EEC_COS(J2)*DCOS(EEC_ARG) + &
     &                                   EEC_SIN(J2)*DSIN(EEC_ARG)
                   EE_CROSS_RATE = EE_CROSS_RATE + &
     &                             EEC_FRQ(J2)* ( - EEC_COS(J2)*DSIN(EEC_ARG) &
     &                                            + EEC_SIN(J2)*DCOS(EEC_ARG) )
 420            CONTINUE
           END IF
!
           IF ( PREC_CODE == PREC__CAPITAINE2003 .OR. &
     &          PREC_CODE == PREC__CAPITAINE2005 .OR. &
     &          PREC_CODE == VTD__NERS                ) THEN
!
! ------------- Here is the tricky point: if precession expansion
! ------------- PREC__CAPITAINE2003 or PREC__CAPITAINE2005, then 
! ------------- we consider that the nutation expansion 
! ------------- does not have secular term (drift)
!
                IF ( NUT_CODE == NUT__MHB2000        .OR. &
                     NUT_CODE == NUT__MHB2000_TRANSF      ) THEN
                     DEPS = DEPS - &
     &                        TARG_TDB*EPS_RATE_MHB2000/(86400.0D0*365.25D0) * &
     &                        MAS__TO__RAD
                     DPSI = DPSI - &
     &                        TARG_TDB*PSI_RATE_MHB2000/(86400.0D0*365.25D0) * &
     &                        MAS__TO__RAD
                   ELSE IF ( NUT_CODE == NUT__IERS1996 ) THEN
                     DEPS = DEPS - &
     &                        TARG_TDB*EPS_RATE_IERS96/(86400.0D0*365.25D0) * &
     &                        MAS__TO__RAD
                     DPSI = DPSI - &
     &                        TARG_TDB*PSI_RATE_IERS96/(86400.0D0*365.25D0) * &
     &                        MAS__TO__RAD
                END IF
           END IF
           IF ( PREC_CODE == PREC__CAPITAINE2005 ) THEN
!
! ------------- "The small corrections that make the IAU 2000A nutation consistent
! ------------- with the IAU2006 precession" from Wallace, P.T. and Capitaine N.
! ------------- Precession nutation procedures consistent with IAU 2006 resolutions",
! ------------- A&A 459, 981-985 (2006).
! -------------- f = \dot(J2)/J - 2.7774D-6 * Time_in_centuries = 2.7774D-6/86400.0D0/36525.D0 = 8.80195D-16
!
                E1 =   (1.0D0            - 8.80195D-16*TARG_TAI ) * E1
                E2 =   (1.0D0 + 4.697D-7 - 8.80195D-16*TARG_TAI ) * E2
                DEPS = (1.0D0            - 8.80195D-16*TARG_TAI ) * DEPS
                DPSI = (1.0D0 + 4.697D-7 - 8.80195D-16*TARG_TAI ) * DPSI
           END IF
!
           IF ( EROT_COMPAT == VTD__CALC10 ) THEN
                DEPS = DEPS - EPS_OFFS_MHB2000 * MAS__TO__RAD
                DPSI = DPSI - PSI_OFFS_MHB2000 * MAS__TO__RAD
                EPS_0 = EPS_0 + ( TARG_TDB*EPS_RATE_MHB2000/(86400.0D0*365.25D0))* &
     &                            MAS__TO__RAD
                CALL VTD_ROTMAT ( 1,  EPS_OFFS_MHB2000* MAS__TO__RAD, CMAT1 )
                CALL VTD_ROTMAT ( 2, -PSI_OFFS_MHB2000*DSIN(EPSILON_0)*MAS__TO__RAD, CMAT2 )
                CALL VTD_ROTMAT ( 3,  14.6D0*MAS__TO__RAD, CMAT3 )
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
           S_ANG =   SARG_TAI_2PI                                        &
     &             + OM__EAR *UT1_M_TAI                                  &
     &             + ( ZA__ARG(1) + DZETA__ARG(1) )*TARG_TDB             &
     &             + ( ZA__ARG(2) + DZETA__ARG(2) )*TARG_TDB**2          &
     &             + ( ZA__ARG(3) + DZETA__ARG(3) -                      &
     &                 ZA__ARG(1)*TETA__ARG(1)**2/6.0D0 )*TARG_TDB**3    &
     &             + ( ZA__ARG(4) + DZETA__ARG(4) -                      &
     &                 ZA__ARG(2)*TETA__ARG(1)**2/8.0D0 )*TARG_TDB**4    &
     &             + DPSI*DCOS(EPS_0)                                    &
     &             + CROSS_NUT_SCL                                       &
     &             + EE_CROSS
            IF ( EROT_COMPAT == VTD__CALC10 ) THEN
!
! ------------- Spurious terms advocated by Capitaine:
! ------------- 1) Offset in right ascension from Chapront 2002 analysis
! ------------- 2) Difference on 2003.01.01 when UT1 is used for accumulated
! -------------    precession in right ascension
! ------------- 3) ???
! ------------- 4) a spurious term in S introducted by Capitaine in
! -------------    order to reach "symmetry" for polar motion
!
                S_ANG = S_ANG &
     &                + 14.600D0*MAS__TO__RAD &
     &                -  0.094D0*MAS__TO__RAD &
     &                +  0.02905D0*MAS__TO__RAD*TARG_TAI/(86400.0D0*36525.0D0) &
     &                -  0.047D0*MAS__TO__RAD*TARG_TAI/(86400.0D0*36525.0D0)
           END IF
           IF ( PREC_CODE == PREC__CAPITAINE2005 ) THEN
!
! ------------- Add-on terms advocated by N. Capitaine
!
                S_ANG =   S_ANG &
     &                  + SARG__ADDON(0)             &
     &                  + SARG__ADDON(1)*TARG_TDB    &
     &                  + SARG__ADDON(2)*TARG_TDB**2 &
     &                  + SARG__ADDON(3)*TARG_TDB**3 &
     &                  + SARG__ADDON(4)*TARG_TDB**4 
           END IF
!!!!!!
!           s_ang =   sarg_tai_2pi + om__ear *ut1_m_tai            &
!     &             - 0.000094d0*arcsec__to__rad                   &
!     &             + 4612.156534d0*arcsec__to__rad/cent__to__sec  *targ_tdb    &
!     &             + 0.0*1.3915817d0*arcsec__to__rad/cent__to__sec**2 *targ_tdb**2 &
!     &             - 0.0*4.4d-7*arcsec__to__rad/cent__to__sec**3      *targ_tdb**3 &
!     &             - 0.0*2.9956d-5*arcsec__to__rad/cent__to__sec**4   *targ_tdb**4 &
!     &             + dpsi*dcos(eps_0) &
!     &             + cross_nut_scl &
!     &             + ee_cross
!!!!!!!!
!
! -------- ... And its rate of change
!
           S_ANG_RATE = OM__EAR *(1.0D0 + UT1_RATE) &
     &                +        ( ZA__ARG(1) + DZETA__ARG(1) )  &
     &                + 2.0D0*( ZA__ARG(2) + DZETA__ARG(2) )*TARG_TAI  &
     &                + 3.0D0*( ZA__ARG(3) + DZETA__ARG(3)  &
     &                        - ZA__ARG(1)*TETA__ARG(1)**2/6.0D0 )*TARG_TAI**2  &
     &                + 4.0D0*( ZA__ARG(4) + DZETA__ARG(4) -                      &
     &                          ZA__ARG(2)*TETA__ARG(1)**2/8.0D0 )*TARG_TDB**3 &
     &                + DPSI_RATE*DCOS(EPS_0) &
     &                - DPSI*EPS_0_RATE*DSIN(EPS_0) &
     &                + CROSS_NUT_SCL/(1.D-8 + TARG_TDB) &
     &                + EE_CROSS_RATE
           IF ( PREC_CODE == PREC__CAPITAINE2005 ) THEN
!
! ------------- Account for add-on terms in Capitaine 2006 precession
!
                S_ANG_RATE =   S_ANG_RATE                       &
     &                       +       SARG__ADDON(1)             &
     &                       + 2.0D0*SARG__ADDON(2)*TARG_TDB    &
     &                       + 3.0D0*SARG__ADDON(3)*TARG_TDB**2 &
     &                       + 4.0D0*SARG__ADDON(4)*TARG_TDB**3 
           END IF
           IF ( EROT_COMPAT == VTD__NO_SARG ) THEN
                S_ANG = 0.0D0
                S_ANG_RATE = 0.0D0
           END IF
           IF ( IVRB .GE. 4 ) THEN
                WRITE ( 6, * ) ' TARG_TDB=', TARG_TDB
                WRITE ( 6, * ) '       S0=', S0
                WRITE ( 6, * ) '      S0A=', SARG_TAI_2PI + &
     &                                       OM__EAR *UT1_M_TAI + 3*PI2
                WRITE ( 6, * ) '      OM1=', OM__EAR + ZA__ARG(1) + DZETA__ARG(1)
                WRITE ( 6, * ) '      UT1=', TAI+UT1_M_TAI
                WRITE ( 6, * ) '       S2=', ( ZA__ARG(2) + DZETA__ARG(2))*TARG_TAI**2
                WRITE ( 6, * ) '       S3=', ( ZA__ARG(3) + DZETA__ARG(3) - &
     &                                         ZA__ARG(1)*TETA__ARG(1)**2/6.0D0 )* &
     &                                         TARG_TAI**3
                WRITE ( 6, * ) '       S4=', ( ZA__ARG(4) + DZETA__ARG(4) - &
     &                                         ZA__ARG(2)*TETA__ARG(1)**2/8.0D0 )*TARG_TDB**4
                WRITE ( 6, * ) '    S_PRC=', &
     &                       ( ZA__ARG(1) + DZETA__ARG(1) )*TARG_TDB    + &
     &                       ( ZA__ARG(2) + DZETA__ARG(2) )*TARG_TDB**2 + &
     &                       ( ZA__ARG(3) + DZETA__ARG(3) - &
     &                         DZETA__ARG(1)*TETA__ARG(1)**2/6.0D0 )*TARG_TDB**3 + &
     &                       ( ZA__ARG(4) + DZETA__ARG(4) - &
     &                         DZETA__ARG(2)*TETA__ARG(1)**2/8.0D0 )*TARG_TDB**4 &
     &                       + 14.506d0*mas__to__rad - &
     &                       3.87354D0*MAS__TO__RAD*TARG_TDB/(86400.0D0*36525.0D0)
                WRITE ( 6, * ) '       S5=', DPSI*DCOS(EPS_0)
                WRITE ( 6, * ) '       S6=', EE_CROSS
                WRITE ( 6, * ) '       S7=', 14.506D0*MAS__TO__RAD
                WRITE ( 6, * ) '       S8=', -3.8736D0*MAS__TO__RAD*TARG_TAI/(86400.0D0*36525.0D0)
                WRITE ( 6, * ) '       S9=', 0.047D0*MAS__TO__RAD*TARG_TAI/(86400.0D0*36525.0D0)
                WRITE ( 6, * ) '       SA=',   SARG__ADDON(0)             &
     &                                       + SARG__ADDON(1)*TARG_TDB    &
     &                                       + SARG__ADDON(2)*TARG_TDB**2 &
     &                                       + SARG__ADDON(3)*TARG_TDB**3 &
     &                                       + SARG__ADDON(4)*TARG_TDB**4 
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
!
! -------- Compute rotation matrices
!
           IF ( FL_NO_PRECNUT_ROT ) THEN
                DZETA = 0.0D0
                TETA  = 0.0D0
                ZA    = 0.0D0
                DPSI  = 0.0D0
                DEPS  = 0.0D0
                XPOL  = 0.0D0
                YPOL  = 0.0D0
                WRITE ( 6, * ) 'VTD_ERM_NA: WARNING!!!  VTD_NO_PRECNUT_ROT environment variable is set up'
           END IF
           CALL VTD_ROTMAT ( 3,  DZETA,        RTM_P1   )
           CALL VTD_ROTMAT ( 2, -TETA,         RTM_P2   )
           CALL VTD_ROTMAT ( 3,  ZA,           RTM_P3   )
           CALL VTD_ROTMAT ( 1, -EPS_0,        RTM_N1   )
           CALL VTD_ROTMAT ( 3,  DPSI,         RTM_N2   )
           CALL VTD_ROTMAT ( 1,  EPS_0 + DEPS, RTM_N3   )
           CALL VTD_ROTMAT ( 3, -S_ANG,        RTM_DIU  )
           CALL VTD_ROTMAT ( 2,  XPOL,         RTM_XWOB )
           CALL VTD_ROTMAT ( 1,  YPOL,         RTM_YWOB )
!
! -------- Now we compute the product of 9 matrices
!
           IF ( EROT_COMPAT == VTD__NONE  .OR.  EROT_COMPAT == VTD__NO_SARG ) THEN
                CALL MULTI_MUL_3 (  9, TRS_TO_CRS, RTM_P1,  RTM_P2,   RTM_P3,  &
     &                                             RTM_N1,  RTM_N2,   RTM_N3,  &
     &                                             RTM_DIU, RTM_XWOB, RTM_YWOB )
             ELSE IF ( EROT_COMPAT == VTD__CALC10 ) THEN
                CALL MULTI_MUL_3 ( 12, TRS_TO_CRS, CMAT1,   CMAT2,    CMAT3,   &
     &                                             RTM_P1,  RTM_P2,   RTM_P3,  &
     &                                             RTM_N1,  RTM_N2,   RTM_N3,  &
     &                                             RTM_DIU, RTM_XWOB, RTM_YWOB )
           END IF
!
! -------- Compute derivatives of the rotation matrices
!
           CALL VTD_ROTMAT_DER ( 3,  DZETA,        DZETA_RATE, DRTM_P1   )
           CALL VTD_ROTMAT_DER ( 2, -TETA,         TETA_RATE,  DRTM_P2   )
           CALL VTD_ROTMAT_DER ( 3,  ZA,           ZA_RATE,    DRTM_P3   )
           CALL VTD_ROTMAT_DER ( 1, -EPS_0,        EPS_0_RATE, DRTM_N1   )
           CALL VTD_ROTMAT_DER ( 3,  DPSI,         DPSI_RATE,  DRTM_N2   )
           CALL VTD_ROTMAT_DER ( 1,  EPS_0 + DEPS, EPS_0_RATE  + DEPS_RATE, &
     &                                                         DRTM_N3   )
!
           CALL VTD_ROTMAT_DER ( 3, -S_ANG,        1.0D0, PRTM_DIU  )
           CALL VTD_ROTMAT_DER ( 2,  XPOL,         1.0D0, PRTM_XWOB )
           CALL VTD_ROTMAT_DER ( 1,  YPOL,         1.0D0, PRTM_YWOB )
!
! -------- Compute time derivatives of matrices RTM_DIU, RTM_XWOB, RTM_YWOB
!
           CALL VEC_MULT_CONSTANT ( PRTM_DIU,  9, S_ANG_RATE, DRTM_DIU  )
           CALL VEC_MULT_CONSTANT ( PRTM_XWOB, 9, XPOL_RATE,  DRTM_XWOB )
           CALL VEC_MULT_CONSTANT ( PRTM_YWOB, 9, YPOL_RATE,  DRTM_YWOB )
!
           CALL VTD_ROTMAT_DER2 ( 3, -S_ANG, 1.0D0, 0.0D0, PPRTM_DIU  )
           CALL VTD_ROTMAT_DER2 ( 2,  XPOL,  1.0D0, 0.0D0, PPRTM_XWOB )
           CALL VTD_ROTMAT_DER2 ( 1,  YPOL,  1.0D0, 0.0D0, PPRTM_YWOB )
!
! -------- Compute mixed derivatives of matrices RTM_DIU, RTM_XWOB, RTM_YWOB over
! -------- time and over the parameter
!
           CALL VEC_MULT_CONSTANT ( PPRTM_XWOB, 9, XPOL_RATE,  DPRTM_XWOB )
           CALL VEC_MULT_CONSTANT ( PPRTM_YWOB, 9, YPOL_RATE,  DPRTM_YWOB )
           CALL VEC_MULT_CONSTANT ( PPRTM_DIU,  9, S_ANG_RATE, DPRTM_DIU  )
!
           IF ( IVRB .GE. 4 ) THEN
                WRITE ( 6, * ) ' DZETA_RATE = ', DZETA_RATE
                WRITE ( 6, * ) ' TETA_RATE  = ', TETA_RATE
                WRITE ( 6, * ) ' ZA_RATE    = ', ZA_RATE
                WRITE ( 6, * ) ' EPS_0_RATE = ', EPS_0_RATE
                WRITE ( 6, * ) ' DPSI_RATE  = ', DPSI_RATE
                WRITE ( 6, * ) ' DEPS_RATE  = ', DEPS_RATE
                WRITE ( 6, * ) ' S_ANG_RATE = ', S_ANG_RATE
                WRITE ( 6, * ) ' XPOL_RATE  = ', XPOL_RATE
                WRITE ( 6, * ) ' YPOL_RATE  = ', YPOL_RATE
           END IF
!
! -------- Now compute first time derivative of TRS_TO_CRS. It is done in
! -------- two steps: first compute 9 terms, each being a product of nine
! -------- matrices, 8 rotation matrices and the 9-th first derivative
!
           CALL MULTI_MUL_3 ( 9, DRTM_1, DRTM_P1,  RTM_P2,  RTM_P3,  RTM_N1,  RTM_N2,  RTM_N3,  RTM_DIU,  RTM_XWOB,  RTM_YWOB )
           CALL MULTI_MUL_3 ( 9, DRTM_2,  RTM_P1, DRTM_P2,  RTM_P3,  RTM_N1,  RTM_N2,  RTM_N3,  RTM_DIU,  RTM_XWOB,  RTM_YWOB )
           CALL MULTI_MUL_3 ( 9, DRTM_3,  RTM_P1,  RTM_P2, DRTM_P3,  RTM_N1,  RTM_N2,  RTM_N3,  RTM_DIU,  RTM_XWOB,  RTM_YWOB )
           CALL MULTI_MUL_3 ( 9, DRTM_4,  RTM_P1,  RTM_P2,  RTM_P3, DRTM_N1,  RTM_N2,  RTM_N3,  RTM_DIU,  RTM_XWOB,  RTM_YWOB )
           CALL MULTI_MUL_3 ( 9, DRTM_5,  RTM_P1,  RTM_P2,  RTM_P3,  RTM_N1, DRTM_N2,  RTM_N3,  RTM_DIU,  RTM_XWOB,  RTM_YWOB )
           CALL MULTI_MUL_3 ( 9, DRTM_6,  RTM_P1,  RTM_P2,  RTM_P3,  RTM_N1,  RTM_N2, DRTM_N3,  RTM_DIU,  RTM_XWOB,  RTM_YWOB )
           CALL MULTI_MUL_3 ( 9, DRTM_7,  RTM_P1,  RTM_P2,  RTM_P3,  RTM_N1,  RTM_N2,  RTM_N3, DRTM_DIU,  RTM_XWOB,  RTM_YWOB )
           CALL MULTI_MUL_3 ( 9, DRTM_8,  RTM_P1,  RTM_P2,  RTM_P3,  RTM_N1,  RTM_N2,  RTM_N3,  RTM_DIU, DRTM_XWOB,  RTM_YWOB )
           CALL MULTI_MUL_3 ( 9, DRTM_9,  RTM_P1,  RTM_P2,  RTM_P3,  RTM_N1,  RTM_N2,  RTM_N3,  RTM_DIU,  RTM_XWOB, DRTM_YWOB )
!
! -------- ... and at the second step we summ up the terms
!
           CALL MULTI_ADD_3 ( 9, TRS_TO_CRS_DER1, &
     &                           1.0D0, DRTM_1, &
     &                          -1.0D0, DRTM_2, &
     &                           1.0D0, DRTM_3, &
     &                          -1.0D0, DRTM_4, &
     &                           1.0D0, DRTM_5, &
     &                           1.0D0, DRTM_6, &
     &                          -1.0D0, DRTM_7, &
     &                           1.0D0, DRTM_8, &
     &                           1.0D0, DRTM_9  )
!
! -------- Compute derivative of TRS_TO_CRS matrix with respect to Eurler
! -------- angles.
!
           CALL MULTI_MUL_3 ( 9, PTRS_TO_CRS_DEOP(1,1,1), RTM_P1, RTM_P2, RTM_P3, RTM_N1, RTM_N2, RTM_N3, RTM_DIU, RTM_XWOB, PRTM_YWOB )
           CALL MULTI_MUL_3 ( 9, PTRS_TO_CRS_DEOP(1,1,2), RTM_P1, RTM_P2, RTM_P3, RTM_N1, RTM_N2, RTM_N3, RTM_DIU, PRTM_XWOB, RTM_YWOB )
           CALL MULTI_MUL_3 ( 9, PTRS_TO_CRS_DEOP(1,1,3), RTM_P1, RTM_P2, RTM_P3, RTM_N1, RTM_N2, RTM_N3, PRTM_DIU, RTM_XWOB, RTM_YWOB )
!
! -------- Compute mixed derivative of TRS_TO_CRS matrix with respect to Eurler
! -------- angles and with respect to time
!
! -------- Mixed derviatve over time and YWOB
!
           CALL MULTI_MUL_3 ( 9, DRTM_1, DRTM_P1,  RTM_P2,  RTM_P3,  RTM_N1,  RTM_N2,  RTM_N3,  RTM_DIU,  RTM_XWOB,  PRTM_YWOB )
           CALL MULTI_MUL_3 ( 9, DRTM_2,  RTM_P1, DRTM_P2,  RTM_P3,  RTM_N1,  RTM_N2,  RTM_N3,  RTM_DIU,  RTM_XWOB,  PRTM_YWOB )
           CALL MULTI_MUL_3 ( 9, DRTM_3,  RTM_P1,  RTM_P2, DRTM_P3,  RTM_N1,  RTM_N2,  RTM_N3,  RTM_DIU,  RTM_XWOB,  PRTM_YWOB )
           CALL MULTI_MUL_3 ( 9, DRTM_4,  RTM_P1,  RTM_P2,  RTM_P3, DRTM_N1,  RTM_N2,  RTM_N3,  RTM_DIU,  RTM_XWOB,  PRTM_YWOB )
           CALL MULTI_MUL_3 ( 9, DRTM_5,  RTM_P1,  RTM_P2,  RTM_P3,  RTM_N1, DRTM_N2,  RTM_N3,  RTM_DIU,  RTM_XWOB,  PRTM_YWOB )
           CALL MULTI_MUL_3 ( 9, DRTM_6,  RTM_P1,  RTM_P2,  RTM_P3,  RTM_N1,  RTM_N2, DRTM_N3,  RTM_DIU,  RTM_XWOB,  PRTM_YWOB )
           CALL MULTI_MUL_3 ( 9, DRTM_7,  RTM_P1,  RTM_P2,  RTM_P3,  RTM_N1,  RTM_N2,  RTM_N3, DRTM_DIU,  RTM_XWOB,  PRTM_YWOB )
           CALL MULTI_MUL_3 ( 9, DRTM_8,  RTM_P1,  RTM_P2,  RTM_P3,  RTM_N1,  RTM_N2,  RTM_N3,  RTM_DIU, DRTM_XWOB,  PRTM_YWOB )
           CALL MULTI_MUL_3 ( 9, DRTM_9,  RTM_P1,  RTM_P2,  RTM_P3,  RTM_N1,  RTM_N2,  RTM_N3,  RTM_DIU,  RTM_XWOB, DPRTM_YWOB )
!
           CALL MULTI_ADD_3 ( 9, DPTRS_TO_CRS_DEOP(1,1,1), &
     &                           1.0D0, DRTM_1, &
     &                          -1.0D0, DRTM_2, &
     &                           1.0D0, DRTM_3, &
     &                          -1.0D0, DRTM_4, &
     &                           1.0D0, DRTM_5, &
     &                           1.0D0, DRTM_6, &
     &                          -1.0D0, DRTM_7, &
     &                           1.0D0, DRTM_8, &
     &                           1.0D0, DRTM_9  )
!
! -------- Mixed derviatve over time and XWOB
!
           CALL MULTI_MUL_3 ( 9, DRTM_1, DRTM_P1,  RTM_P2,  RTM_P3,  RTM_N1,  RTM_N2,  RTM_N3,  RTM_DIU,  PRTM_XWOB,  RTM_YWOB )
           CALL MULTI_MUL_3 ( 9, DRTM_2,  RTM_P1, DRTM_P2,  RTM_P3,  RTM_N1,  RTM_N2,  RTM_N3,  RTM_DIU,  PRTM_XWOB,  RTM_YWOB )
           CALL MULTI_MUL_3 ( 9, DRTM_3,  RTM_P1,  RTM_P2, DRTM_P3,  RTM_N1,  RTM_N2,  RTM_N3,  RTM_DIU,  PRTM_XWOB,  RTM_YWOB )
           CALL MULTI_MUL_3 ( 9, DRTM_4,  RTM_P1,  RTM_P2,  RTM_P3, DRTM_N1,  RTM_N2,  RTM_N3,  RTM_DIU,  PRTM_XWOB,  RTM_YWOB )
           CALL MULTI_MUL_3 ( 9, DRTM_5,  RTM_P1,  RTM_P2,  RTM_P3,  RTM_N1, DRTM_N2,  RTM_N3,  RTM_DIU,  PRTM_XWOB,  RTM_YWOB )
           CALL MULTI_MUL_3 ( 9, DRTM_6,  RTM_P1,  RTM_P2,  RTM_P3,  RTM_N1,  RTM_N2, DRTM_N3,  RTM_DIU,  PRTM_XWOB,  RTM_YWOB )
           CALL MULTI_MUL_3 ( 9, DRTM_7,  RTM_P1,  RTM_P2,  RTM_P3,  RTM_N1,  RTM_N2,  RTM_N3, DRTM_DIU,  PRTM_XWOB,  RTM_YWOB )
           CALL MULTI_MUL_3 ( 9, DRTM_8,  RTM_P1,  RTM_P2,  RTM_P3,  RTM_N1,  RTM_N2,  RTM_N3,  RTM_DIU, DPRTM_XWOB,  RTM_YWOB )
           CALL MULTI_MUL_3 ( 9, DRTM_9,  RTM_P1,  RTM_P2,  RTM_P3,  RTM_N1,  RTM_N2,  RTM_N3,  RTM_DIU,  PRTM_XWOB, DRTM_YWOB )
!
           CALL MULTI_ADD_3 ( 9, DPTRS_TO_CRS_DEOP(1,1,2), &
     &                           1.0D0, DRTM_1, &
     &                          -1.0D0, DRTM_2, &
     &                           1.0D0, DRTM_3, &
     &                          -1.0D0, DRTM_4, &
     &                           1.0D0, DRTM_5, &
     &                           1.0D0, DRTM_6, &
     &                          -1.0D0, DRTM_7, &
     &                           1.0D0, DRTM_8, &
     &                           1.0D0, DRTM_9  )
!
! -------- Mixed derviatve over time and DIU
!
           CALL MULTI_MUL_3 ( 9, DRTM_1, DRTM_P1,  RTM_P2,  RTM_P3,  RTM_N1,  RTM_N2,  RTM_N3,  PRTM_DIU,  RTM_XWOB,  RTM_YWOB )
           CALL MULTI_MUL_3 ( 9, DRTM_2,  RTM_P1, DRTM_P2,  RTM_P3,  RTM_N1,  RTM_N2,  RTM_N3,  PRTM_DIU,  RTM_XWOB,  RTM_YWOB )
           CALL MULTI_MUL_3 ( 9, DRTM_3,  RTM_P1,  RTM_P2, DRTM_P3,  RTM_N1,  RTM_N2,  RTM_N3,  PRTM_DIU,  RTM_XWOB,  RTM_YWOB )
           CALL MULTI_MUL_3 ( 9, DRTM_4,  RTM_P1,  RTM_P2,  RTM_P3, DRTM_N1,  RTM_N2,  RTM_N3,  PRTM_DIU,  RTM_XWOB,  RTM_YWOB )
           CALL MULTI_MUL_3 ( 9, DRTM_5,  RTM_P1,  RTM_P2,  RTM_P3,  RTM_N1, DRTM_N2,  RTM_N3,  PRTM_DIU,  RTM_XWOB,  RTM_YWOB )
           CALL MULTI_MUL_3 ( 9, DRTM_6,  RTM_P1,  RTM_P2,  RTM_P3,  RTM_N1,  RTM_N2, DRTM_N3,  PRTM_DIU,  RTM_XWOB,  RTM_YWOB )
           CALL MULTI_MUL_3 ( 9, DRTM_7,  RTM_P1,  RTM_P2,  RTM_P3,  RTM_N1,  RTM_N2,  RTM_N3, DPRTM_DIU,  RTM_XWOB,  RTM_YWOB )
           CALL MULTI_MUL_3 ( 9, DRTM_8,  RTM_P1,  RTM_P2,  RTM_P3,  RTM_N1,  RTM_N2,  RTM_N3,  PRTM_DIU, DRTM_XWOB,  RTM_YWOB )
           CALL MULTI_MUL_3 ( 9, DRTM_9,  RTM_P1,  RTM_P2,  RTM_P3,  RTM_N1,  RTM_N2,  RTM_N3,  PRTM_DIU,  RTM_XWOB, DRTM_YWOB )
!
           CALL MULTI_ADD_3 ( 9, DPTRS_TO_CRS_DEOP(1,1,3), &
     &                           1.0D0, DRTM_1, &
     &                          -1.0D0, DRTM_2, &
     &                           1.0D0, DRTM_3, &
     &                          -1.0D0, DRTM_4, &
     &                           1.0D0, DRTM_5, &
     &                           1.0D0, DRTM_6, &
     &                          -1.0D0, DRTM_7, &
     &                           1.0D0, DRTM_8, &
     &                           1.0D0, DRTM_9  )
!
! -------- Compute the second derivative of the rotation matrix. Only one
! -------- term is taken into account. Relative error of the expression below
! -------- is only 1.D-5
!
           CALL VTD_ROTMAT_DER2 ( 3, -S_ANG, S_ANG_RATE, 0.0D0, SRTM_DIU  )
!
           CALL MULTI_MUL_3 ( 9, SRTM_1,  RTM_P1,  RTM_P2,  RTM_P3, RTM_N1,  RTM_N2,  RTM_N3, SRTM_DIU,  RTM_XWOB,  RTM_YWOB )
           CALL MULTI_MUL_3 ( 9, SRTM_2, DRTM_P1,  RTM_P2,  RTM_P3, RTM_N1,  RTM_N2,  RTM_N3, DRTM_DIU,  RTM_XWOB,  RTM_YWOB )
           CALL MULTI_MUL_3 ( 9, SRTM_3,  RTM_P1, DRTM_P2,  RTM_P3, RTM_N1,  RTM_N2,  RTM_N3, DRTM_DIU,  RTM_XWOB,  RTM_YWOB )
           CALL MULTI_MUL_3 ( 9, SRTM_4,  RTM_P1,  RTM_P2, DRTM_P3, RTM_N1,  RTM_N2,  RTM_N3, DRTM_DIU,  RTM_XWOB,  RTM_YWOB )
           CALL MULTI_MUL_3 ( 9, SRTM_5,  RTM_P1,  RTM_P2,  RTM_P3, RTM_N1, DRTM_N2,  RTM_N3, DRTM_DIU,  RTM_XWOB,  RTM_YWOB )
           CALL MULTI_MUL_3 ( 9, SRTM_6,  RTM_P1,  RTM_P2,  RTM_P3, RTM_N1,  RTM_N2, DRTM_N3, DRTM_DIU,  RTM_XWOB,  RTM_YWOB )
           CALL MULTI_MUL_3 ( 9, SRTM_7,  RTM_P1,  RTM_P2,  RTM_P3, RTM_N1,  RTM_N2,  RTM_N3, DRTM_DIU, DRTM_XWOB,  RTM_YWOB )
           CALL MULTI_MUL_3 ( 9, SRTM_8,  RTM_P1,  RTM_P2,  RTM_P3, RTM_N1,  RTM_N2,  RTM_N3, DRTM_DIU,  RTM_XWOB, DRTM_YWOB )
!
           CALL MULTI_ADD_3 ( 8, TRS_TO_CRS_DER2, &
     &                           1.0D0, SRTM_1, &
     &                          -2.0D0, SRTM_2, &
     &                           2.0D0, SRTM_3, &
     &                          -2.0D0, SRTM_4, &
     &                          -2.0D0, SRTM_5, &
     &                          -2.0D0, SRTM_6, &
     &                          -2.0D0, SRTM_7, &
     &                          -2.0D0, SRTM_8  )
        ELSE IF ( IPAR .EQ. 3 ) THEN
!
! -------- Compute Euler angles of the perturbed rotation
!
           IF ( NUT_CODE == NUT__WAHR1980 ) THEN
                CALL HEO_WAHR1980 ( 3, TARG_TDB, UT1_M_TDB, E1, E2, &
     &                              DPSI, DEPS, E1_RATE, E2_RATE, &
     &                              DPSI_RATE, DEPS_RATE, CROSS_NUT_SCL )
              ELSE IF ( NUT_CODE == NUT__IERS1996 ) THEN
                CALL HEO_IERS1996 ( 3, TARG_TDB, UT1_M_TDB, E1, E2, &
     &                              DPSI, DEPS, E1_RATE, E2_RATE, &
     &                              DPSI_RATE, DEPS_RATE, CROSS_NUT_SCL )
              ELSE IF ( NUT_CODE == NUT__REN2000 ) THEN
                CALL HEO_REN2000  ( 3, TARG_TDB, UT1_M_TDB, E1, E2, &
     &                              DPSI, DEPS, E1_RATE, E2_RATE, &
     &                              DPSI_RATE, DEPS_RATE, CROSS_NUT_SCL )
              ELSE IF ( NUT_CODE == NUT__MHB2000 ) THEN
                CALL HEO_MHB2000  ( 3, TARG_TDB, UT1_M_TDB, E1, E2, &
     &                              DPSI, DEPS, E1_RATE, E2_RATE, &
     &                              DPSI_RATE, DEPS_RATE, CROSS_NUT_SCL )
              ELSE IF ( NUT_CODE == NUT__MHB2000_TRANSF ) THEN
                CALL HEO_MHB2000_TRANSF ( 3, TARG_TDB, UT1_M_TDB, E1, E2, &
     &                                    DPSI, DEPS, E1_RATE, E2_RATE, &
     &                                    DPSI_RATE, DEPS_RATE, CROSS_NUT_SCL )
           END IF
           IF ( IVRB .GE. 4 ) THEN
                WRITE ( 6, * ) ' E1=',E1,' E2=',E2
           END IF
!
! -------- Compute rotation matrices
!
           CALL VTD_ROTMAT ( 1,  E1, RTM_E1 )
           CALL VTD_ROTMAT ( 2,  E2, RTM_E2 )
!
! -------- Compute their product
!
           CALL MUL_MM_II_I ( 3, 3, RTM_E1, 3, 3, RTM_E2, 3, 3, TRS_TO_CRS, IER )
      END IF
!
      IF ( L_HEO == VTD__NERS ) THEN
           CALL ERR_PASS     ( IUER, IER ) 
           CALL NERS_GET_EOP ( NERS, (MJD - J2000__MJD)*86400.0D0 + TAI, &
     &               'heorr', NERS__MPAR, L_PAR, PARS, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 2274, IUER, 'VTD_ERM_MA', 'Error in '// &
     &              'getting the Earth orientation parameters using NERS' )
                RETURN
           END IF
!
           HEO_VEC(1)      = PARS(1)
           HEO_VEC(2)      = PARS(2)
           HEO_VEC(3)      = PARS(3)
           HEO_VEC_DER1(1) = PARS(4)
           HEO_VEC_DER1(2) = PARS(5)
           HEO_VEC_DER1(3) = PARS(6)
           HEO_VEC_DER2(1) = PARS(7)
           HEO_VEC_DER2(2) = PARS(8)
           HEO_VEC_DER2(3) = PARS(9)
!
           IF ( IVRB .GE. 2 ) THEN
                WRITE ( 6, * ) ' HEO_VEC(1) = ', HEO_VEC(1)
                WRITE ( 6, * ) ' HEO_VEC(2) = ', HEO_VEC(2)
                WRITE ( 6, * ) ' HEO_VEC(3) = ', HEO_VEC(3)
           END IF
        ELSE IF ( L_HEO .GT. 0 ) THEN
!
! -------- Compute vector of perturbation Earth orientation and its the first
! -------- and the second time derivative
!
           CALL ERR_PASS ( IUER, IER )
           CALL VTD_APPLY_HEO ( TARG_TAI, HEO_EPOCH_SEC, L_HEO, HEO, &
     &                          HEO_VEC, HEO_VEC_DER1, HEO_VEC_DER2, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 2275, IUER, 'VTD_ERM_NA', 'Error in '// &
     &              'an attempt to apply harmonic Earth orientation '// &
     &              'variations' )
                RETURN
           END IF
!
           IF ( IVRB .GE. 2 ) THEN
                WRITE ( 6, * ) ' HEO_VEC(1) = ', HEO_VEC(1)
                WRITE ( 6, * ) ' HEO_VEC(2) = ', HEO_VEC(2)
                WRITE ( 6, * ) ' HEO_VEC(3) = ', HEO_VEC(3)
           END IF
      END IF
!
      IF ( L_HEO .GT. 0 ) THEN
!
! -------- Build the rotational matrix and its time derivatives
!
           HEO_MAT(1,1) =  1.0D0
           HEO_MAT(2,1) = -HEO_VEC(3)
           HEO_MAT(3,1) =  HEO_VEC(2)
           HEO_MAT(1,2) =  HEO_VEC(3)
           HEO_MAT(2,2) =  1.0D0
           HEO_MAT(3,2) = -HEO_VEC(1)
           HEO_MAT(1,3) = -HEO_VEC(2)
           HEO_MAT(2,3) =  HEO_VEC(1)
           HEO_MAT(3,3) =  1.0D0
!
           HEO_MAT_DER1(1,1) =  0.0D0
           HEO_MAT_DER1(2,1) = -HEO_VEC_DER1(3)
           HEO_MAT_DER1(3,1) =  HEO_VEC_DER1(2)
           HEO_MAT_DER1(1,2) =  HEO_VEC_DER1(3)
           HEO_MAT_DER1(2,2) =  0.0D0
           HEO_MAT_DER1(3,2) = -HEO_VEC_DER1(1)
           HEO_MAT_DER1(1,3) = -HEO_VEC_DER1(2)
           HEO_MAT_DER1(2,3) =  HEO_VEC_DER1(1)
           HEO_MAT_DER1(3,3) =  0.0D0
!
           HEO_MAT_DER2(1,1) =  0.0D0
           HEO_MAT_DER2(2,1) = -HEO_VEC_DER2(3)
           HEO_MAT_DER2(3,1) =  HEO_VEC_DER2(2)
           HEO_MAT_DER2(1,2) =  HEO_VEC_DER2(3)
           HEO_MAT_DER2(2,2) =  0.0D0
           HEO_MAT_DER2(3,2) = -HEO_VEC_DER2(1)
           HEO_MAT_DER2(1,3) = -HEO_VEC_DER2(2)
           HEO_MAT_DER2(2,3) =  HEO_VEC_DER2(1)
           HEO_MAT_DER2(3,3) =  0.0D0
!
! -------- Multiply TRS_TO_CRS and HEO matrix
!
           CALL MUL_MM_II_I ( 3, 3, TRS_TO_CRS, 3, 3, HEO_MAT, 3, 3, RTM_1, IER )
!
! -------- Compute intermediate matrices for the first derivatives
!
           CALL MUL_MM_II_I ( 3, 3, TRS_TO_CRS_DER1, 3, 3, HEO_MAT, &
     &                        3, 3, DRTM_1, IER )
           CALL MUL_MM_II_I ( 3, 3, TRS_TO_CRS, 3, 3, HEO_MAT_DER1, &
     &                        3, 3, DRTM_2, IER )
!
! -------- ... and second derivative
!
           CALL MUL_MM_II_I ( 3, 3, TRS_TO_CRS_DER2, 3, 3, HEO_MAT, &
     &                        3, 3, SRTM_1, IER )
           CALL MUL_MM_II_I ( 3, 3, TRS_TO_CRS_DER1, 3, 3, HEO_MAT_DER1, &
     &                        3, 3, SRTM_2, IER )
           CALL MUL_MM_II_I ( 3, 3, TRS_TO_CRS,      3, 3, HEO_MAT_DER2, &
     &                        3, 3, SRTM_3, IER )

!
           CALL COPY_R8     ( 9, RTM_1, TRS_TO_CRS )
!
! -------- Compute the updated first and second derivatives of TRS_TO_CRS
!
           CALL MULTI_ADD_3 ( 2, TRS_TO_CRS_DER1, &
     &                           1.0D0, DRTM_1,   &
     &                           1.0D0, DRTM_2    )
           CALL MULTI_ADD_3 ( 3, TRS_TO_CRS_DER2, &
     &                           1.0D0, SRTM_1,   &
     &                           2.0D0, SRTM_2,   &
     &                           1.0D0, SRTM_3    )
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  VTD_ERM_NA  #!#
