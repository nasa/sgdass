!
! >>> Nutation constaints  (c)  L.Petrov  2003.12.11  -- 2020.03.22_18:29:29
!
      REAL*8     PI, PI2, P2I, MAS_TO_RAD, YS, OM_NM, OM_PRC, EPSILON_0
      REAL*8       OM_MHB, PRC_RAD_THO , S0_EARTH
      PARAMETER ( PI=3.141592653589793D0, PI2=2.D0*PI, P2I=PI/2D0 ) !
      PARAMETER ( MAS_TO_RAD = PI/(180.0D0*3600.0D0*1.D3) ) ! mas --> rad
      PARAMETER ( YS = 86400.D0 * 365.25D0 ) ! The number of seconds in Julian
      PARAMETER  ( OM_NM  = 7.292115146706979D-05 ) ! Nominal Earth angular velocity
      PARAMETER  ( OM_PRC = 7.08618327D-12 ) ! Precssion rate (L. Petrov, 2002)
!!!      PARAMETER  ( OM_PRC =    7.08600202E-12 ) ! Precssion rate (Liske, 1976)
      PARAMETER  ( OM_MHB = 7.292115D-05 )   ! MHB2000 Nominal Earth angular velocity
      PARAMETER  ( PRC_RAD_THO = 50287.700D0*PI/(180.D0*3600.D0) ) ! rad per thous
      PARAMETER  ( EPSILON_0 = 0.4090928041D0 )  ! rad
!@      PARAMETER  ( S0_EARTH  = 1.753368559D0  )  ! rad
      PARAMETER  ( S0_EARTH = 1.75336855923396D0 )  ! rad (for Calc compat)
      INTEGER*4  N1__NCN, N2__NCN
      REAL*8     SIM_C(3,15)
!
! --- Fundamental coefficients of Simon for the use of REN-2000
! --- P. Bretagnon, R. Rocher, J.L. Simon, "Theory of the rotation of the
! --- rigid Earth", A&A, 319, p.305--317, 1997.
!
!
        DATA   ( ( SIM_C(N1__NCN,N2__NCN),N1__NCN=1,3 ), N2__NCN=1,5 ) &
     &  / &
     &      2.355555898D0,   83286.914269554D0,  1.545547D-2, & !  Lm   1
     &      6.24006013D0,     6283.01955D0,     -2.681989D-4, & !  Ls   2
     &      1.627905234D0,   84334.66158131D0,  -6.181956D-3, & !  F    3
     &      5.198466741D0,   77713.771468121D0, -3.088554D-3, & !  D    4
     &      2.18243920D0,     -337.57045D0,      3.622625D-3  & !  Om   5
     &  /
!
! --- Simon (1994) expansions
!
	REAL*8     SIM1_C(4,5), SIM1_R(5), SIM2_C(3,11)
!
! ???   Due to bug in Sun complier had to unroll the loop
! 
        DATA   ( SIM1_C(1,N2__NCN), &
     &           SIM1_C(2,N2__NCN), &
     &           SIM1_C(3,N2__NCN), &
     &           SIM1_C(4,N2__NCN), &
     &           SIM1_R(N2__NCN), N2__NCN=1,5 ) &
!        DATA   ( ( SIM1_C(N1__NCN,N2__NCN),N1__NCN=1,4 ),                 &
!     &             SIM1_R(N2__NCN), N2__NCN=1,5 ) &
     &  / &
     &   0.05163D0,  31.8792D0,  715923.2178D0,  485868.24904D0, 1325.D0, &
     &  -0.00014D0,  -0.5532D0, 1292581.0481D0, 1287104.79305D0,   99.D0, &
     &  -0.00104D0, -12.7512D0,  295262.8478D0,  335779.52623D0, 1342.D0, &
     &   0.00659D0,  -6.3706D0, 1105601.2090D0, 1072260.70369D0, 1236.D0, &
     &   0.00770D0,   7.4722D0, -482890.5431D0,  450160.39804D0,   -5.D0  &
     &  /
!
! ----- Fundamental coeeficients for IERS96 nutation expansion
!
        DATA   ( ( SIM2_C(N1__NCN,N2__NCN), N1__NCN=1,3 ), N2__NCN=1,11 ) &
     &  / &
     &     0.01570D0,   83286.91427D0,    2.35555590D0,    & !  LM   1
     &    -0.00026D0,    6283.01955D0,    6.24006013D0,    & !  LS   2
     &    -0.00594D0,   84334.66158D0,    1.62790523D0,    & !  F    3
     &    -0.00284D0,   77713.77147D0,    5.19846674D0,    & !  D    4
     &     0.00362D0,    -337.57045D0,    2.18243920D0,    & !  OM   5
     &     0.0D0,       10213.285546D0,   3.176146697D0,   & !  LV   6
     &     0.0D0,        6283.0758492D0,  1.753470314D0,   & !  EA   7
     &     0.0D0,        3340.6124315D0,  6.203480913D0,   & !  LM   8
     &     0.0D0,         529.6909651D0,  0.599546497D0,   & !  LJ   9
     &     0.0D0,         213.2990954D0,  0.874016757D0,   & !  LS  10
     &     0.0005387D0,     0.2438175D0,  0.0D0            & !  PA  11
     &  /
!
      REAL*8      SIM3_C(3,15)
!
! --- Fundamental coefficients of Simon for the use of REN-2000
!
        DATA   ( ( SIM3_C (N1__NCN,N2__NCN),N1__NCN=1,3 ), N2__NCN=1,15 ) &
     &  / &
     &      2.355555898D0,   83286.914269554D0,  1.545547D-2, & !  Lm   1
     &      6.24006013D0,     6283.01955D0,     -2.681989D-4, & !  Ls   2
     &      1.627905234D0,   84334.66158131D0,  -6.181956D-3, & !  F    3
     &      5.198466741D0,   77713.771468121D0, -3.088554D-3, & !  D    4
     &      2.18243920D0,     -337.57045D0,      3.622625D-3, & !  Om   5
     &      4.402608842D0,   26087.903141574D0,  0.0D0,       & !  Me   6
     &      3.176146697D0,   10213.285546211D0,  0.0D0,       & !  Ve   7
     &      1.753470314D0,    6283.075849991D0,  0.0D0,       & !  Ea   8
     &      6.203480913D0,    3340.612426700D0,  0.0D0,       & !  Ma   9
     &      0.599546497D0,     529.690962641D0,  0.0D0,       & !  Ju  10
     &      0.874016757D0,     213.299104960D0,  0.0D0,       & !  Sa  11
     &      5.481293871D0,      74.781598567D0,  0.0D0,       & !  Ur  12
     &      5.311886287D0,      38.133035638D0,  0.0D0,       & !  Ne  13 ! from Bretagnon
     &      0.0D0,                 PRC_RAD_THO,  0.0D0,       & !  Pa  14
     &      4.894961212D0, 2301216.7526278D0,    0.0D0        & !  Ph  15
     &  /
!
! --- Fundamental coefficients of Simon for the use of MHB-2000
!
	REAL*8     SIM4_C(3,14)
        DATA   ( ( SIM4_C (N1__NCN,N2__NCN),N1__NCN=1,3 ), N2__NCN=1,14 ) &
     &  / &
     &     485868.249036D0, 1717915923.2178d0,  31.8792D0,    & !  Lm   1
     &     1287104.79305D0,  129596581.0481D0,  -0.5532d0,    & !  Ls   2
     &     335779.526232D0, 1739527262.8478D0, -12.7512d0,    & !  F    3
     &     1072260.70369D0, 1602961601.2090D0,  -6.3706d0,    & !  D    4
     &     450160.398036D0,   -6962890.5431D0,   7.4722d0,    & !  Om   5
     &       4.402608842D0,   26087.903141574D0,   0.0D0,     & !  Me   6
     &       3.176146697D0,   10213.285546211D0,   0.0D0,     & !  Ve   7
     &       1.753470314D0,    6283.075849991D0,   0.0D0,     & !  Ea   8
     &       6.203480913D0,    3340.612426700D0,   0.0D0,     & !  Ma   9
     &       0.599546497D0,     529.690962641D0,   0.0D0,     & !  Ju  10
     &       0.874016757D0,     213.299104960D0,   0.0D0,     & !  Sa  11
     &       5.481293871D0,      74.781598567D0,   0.0D0,     & !  Ur  12
     &       5.311886287D0,      38.133035638D0,   0.0D0,     & !  Ne  13 ! from Bretagnon
     &       0.0D0,               0.243817500D0,   5.38691D-6 & !  Pa  14
     &  /
!
! ----- Ranges of frequencies for MHB2000
!
	REAL*8       MHB2000_FRQ_MIN, MHB2000_FRQ_MAX
	PARAMETER  ( MHB2000_FRQ_MIN = -9.2D-5 )
	PARAMETER  ( MHB2000_FRQ_MAX = -5.1D-5 )
!
        REAL*8     PSI_RATE_IERS96, EPS_RATE_IERS96
        REAL*8     PSI_OFFS_IERS96, EPS_OFFS_IERS96
!
! ----- Empirical paramters for precession rate, obliquity rate and CEP 
! ----- coordinates at J2000.0  epoch for IERS1996 expansion
!                                !
        PARAMETER  ( PSI_RATE_IERS96 =  -2.957D0 ) ! mas/yr
        PARAMETER  ( EPS_RATE_IERS96 =  -0.227D0 ) ! mas/yr
        PARAMETER  ( PSI_OFFS_IERS96 = -43.1D0   ) ! mas
        PARAMETER  ( EPS_OFFS_IERS96 =  -5.1D0   ) ! mas
!
! ----- Empirical paramters for precession rate, obliquity rate and CEP 
! ----- coordinates at J2000.0  epoch
!
        REAL*8     PSI_RATE_MHB2000, EPS_RATE_MHB2000
        REAL*8     PSI_OFFS_MHB2000, EPS_OFFS_MHB2000
        PARAMETER  ( PSI_RATE_MHB2000 =  -2.9965D0  ) ! mas/yr
        PARAMETER  ( EPS_RATE_MHB2000 =  -0.2524D0  ) ! mas/yr
        PARAMETER  ( PSI_OFFS_MHB2000 = -41.77467D0 ) ! mas ! -16.6170*sin(eo)
        PARAMETER  ( EPS_OFFS_MHB2000 =  -6.819D0   ) ! mas
!@        PARAMETER  ( PSI_OFFS_MHB2000 = -41.7750D0 ) ! mas For Calc-10 compatibility -0.00033D0 mas
!@        PARAMETER  ( EPS_OFFS_MHB2000 =  -6.8192D0 ) ! mas For Calc-10 compatibility -0.0002D0 mas
