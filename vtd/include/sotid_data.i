!
! >> include-block for estimation of the displacements due to
! >> the Earth's solid tides. Contains data.
! >>
! >> L. Petrov  22-JAN-2001        12-JUL-2002 11:28:31
!
!
      REAL*8      SOTID__PI, SOTID__PI2, SOTID__P2I
      PARAMETER ( SOTID__PI  = 3.141592653589793D0 )
      PARAMETER ( SOTID__PI2 = SOTID__PI*2.D0 )
      PARAMETER ( SOTID__P2I = SOTID__PI/2.D0 )
      REAL*8      SOTID__GEO_CG, SOTID__EARTH_RAD
      PARAMETER ( SOTID__GEO_CG    = 3.986004418D14  ) ! Geocentric constand of
!                           ! Gravity ( m**3/s**2) taken from IERS 1992
      PARAMETER ( SOTID__EARTH_RAD = 6378136.3D0 ) ! Earth radius (m) IERS 1992
!
! --- Low limit frequency. The tides with the frequency lower that that
! --- are considered permanent
!
      REAL*8       SOTID__LOW_FREQ_LIM ! freq. for the period of 40 years
      PARAMETER  ( SOTID__LOW_FREQ_LIM = SOTID__PI2/86400.D0/(365.2422*40.D0) )
      INTEGER*4  SOTID__MJD_J2000
      PARAMETER  ( SOTID__MJD_J2000 = 51544 )
!
! --- Height tolerance. If the the radius vector deviates from the
! --- equatorial Earth radius by more than SOTID_HEIGHT_TOL meters, then
! --- the coordinates of such a station are considered as incorrect and
! --- routine SOTID_PRE will diagnose the error.
!
      REAL*8 SOTID__HEIGHT_TOL
      PARAMETER  ( SOTID__HEIGHT_TOL = 50000.0 )
!
!
      INTEGER*4  SOTID__PRN_ONLY, &
     &           SOTID__PL_ONLY, &
     &           SOTID__GEN_ALL, &
!
     &           SOTID__MDG97EL, &
     &           SOTID__MDG97AN, &
     &           SOTID__DDW99EH, &
     &           SOTID__DDW99IN, &
     &           SOTID__LOVE, &
     &           SOTID__MAT00, &
     &           SOTID__MAT01, &
!
     &           SOTID__2D_NONE, &
     &           SOTID__2D_0ORD, &
     &           SOTID__2D_01ORD, &
     &           SOTID__2D_02ORD, &
     &           SOTID__2D_012ORD, &
     &           SOTID__2D_1ORD, &
     &           SOTID__2D_12ORD, &
     &           SOTID__2D_2ORD, &
!
     &           SOTID__ZF_ZERO, &
     &           SOTID__ZF_LOVE, &
     &           SOTID__ZF_MDG97EL, &
     &           SOTID__ZF_MDG97AN, &
     &           SOTID__ZF_FLUID, &
!
     &           SOTID__3D_NONE, &
     &           SOTID__3D_MDG97
!
      INTEGER*4    SOTID__UNDEFINED, SOTID__MAX_DEF
      PARAMETER  ( SOTID__UNDEFINED = 4746776 )
!
      PARAMETER  ( SOTID__PRN_ONLY   =  1 )
      PARAMETER  ( SOTID__PL_ONLY    =  2 )
      PARAMETER  ( SOTID__GEN_ALL    =  3 )
!
      PARAMETER  ( SOTID__LOVE       =  4 )
      PARAMETER  ( SOTID__MDG97EL    =  5 )
      PARAMETER  ( SOTID__MDG97AN    =  6 )
      PARAMETER  ( SOTID__DDW99EH    =  7 )
      PARAMETER  ( SOTID__DDW99IN    =  8 )
      PARAMETER  ( SOTID__MAT00      =  9 )
      PARAMETER  ( SOTID__MAT01      = 10 )
!
      PARAMETER  ( SOTID__2D_NONE    = 11 )
      PARAMETER  ( SOTID__2D_0ORD    = 12 )
      PARAMETER  ( SOTID__2D_01ORD   = 13 )
      PARAMETER  ( SOTID__2D_02ORD   = 14 )
      PARAMETER  ( SOTID__2D_012ORD  = 15 )
      PARAMETER  ( SOTID__2D_1ORD    = 16 )
      PARAMETER  ( SOTID__2D_12ORD   = 17 )
      PARAMETER  ( SOTID__2D_2ORD    = 18 )
!
      PARAMETER  ( SOTID__ZF_ZERO    = 19 )
      PARAMETER  ( SOTID__ZF_LOVE    = 20 )
      PARAMETER  ( SOTID__ZF_MDG97EL = 21 )
      PARAMETER  ( SOTID__ZF_MDG97AN = 22 )
      PARAMETER  ( SOTID__ZF_FLUID   = 23 )
!
      PARAMETER  ( SOTID__3D_NONE    = 24 )
      PARAMETER  ( SOTID__3D_MDG97   = 25 )
!
      PARAMETER  ( SOTID__MAX_DEF    = SOTID__3D_MDG97 )
!
      INTEGER*4  SOTID__DER0, SOTID__DER1
      PARAMETER  ( SOTID__DER0 = 0 )
      PARAMETER  ( SOTID__DER1 = 1 )
!
! --- Here are the defintision of the description of the parameters.
! --- All descriptions are gatherer in the array SOTID__DSC
!
      INTEGER*4   SOTID__DSC_LEN
      PARAMETER ( SOTID__DSC_LEN = 64 )
      CHARACTER   SOTID__DSC(SOTID__MAX_DEF)*(SOTID__DSC_LEN)
!
      DATA &
!
     & SOTID__DSC(SOTID__PRN_ONLY) &
     &  / 'Only principle Love numbers are  taken into acount              ' /, &
     & SOTID__DSC(SOTID__PL_ONLY) &
     &  / 'Principle and latitude-dependent Love numbers are  used         ' /, &
     & SOTID__DSC(SOTID__GEN_ALL) &
     &  / 'All generalized Love numbers are taken into account             ' /, &
!
     & SOTID__DSC(SOTID__LOVE) &
     &   / 'Frequency independent Love numbers (Love, 1909) are used       ' /, &
     & SOTID__DSC(SOTID__MDG97EL) &
     &   / 'MDG97 elasticity Love numbers model                            ' /, &
     & SOTID__DSC(SOTID__MDG97AN) &
     &   / 'MDG97 anelasticity Love numbers model                          ' /, &
     & SOTID__DSC(SOTID__DDW99EH) &
     &   / 'DDW99 elastiitty, hydrostic Love numbers model                 ' /, &
     & SOTID__DSC(SOTID__DDW99IN) &
     &  / 'DDW99 inelasticity, non-hydrostatic  Love numbers model         ' /, &
     & SOTID__DSC(SOTID__MAT00) &
     &  / 'Mathews, 2000 Love numbers. Resonance formula with corrections  ' /, &
     & SOTID__DSC(SOTID__MAT01) &
     &  / 'Mathews, 2001 Love numbers. Resonance formula with corrections  ' /, &
!
     & SOTID__DSC(SOTID__2D_NONE) &
     &  / 'Diplacements of the 2-nd degree are not computed                ' /, &
     & SOTID__DSC(SOTID__2D_0ORD) &
     &  / 'Diplacements of the 2-nd degree, zonal tides only               ' /, &
     & SOTID__DSC(SOTID__2D_01ORD) &
     &  / 'Diplacements of the 2-nd degree, zonal and diurnal tides only   ' /, &
     & SOTID__DSC(SOTID__2D_01ORD) &
     &  / 'Diplacements of the 2-nd degree, zonal and semi-diurnal tides   ' /, &
     & SOTID__DSC(SOTID__2D_012ORD) &
     &  / 'Diplacements of the 2-nd degree are computed                    ' /, &
     & SOTID__DSC(SOTID__2D_1ORD) &
     &  / 'Diplacements of the 2-nd degree, diurnal tides only             ' /, &
     & SOTID__DSC(SOTID__2D_12ORD) &
     &  / 'Diplacements of the 2-nd degree, diurnal and semi-diurnal tides ' /, &
     & SOTID__DSC(SOTID__2D_2ORD) &
     &  / 'Diplacements of the 2-nd degree, semi-diurnal tides only        ' /, &
!
     & SOTID__DSC(SOTID__ZF_ZERO) &
     &  / 'Love numbers for zero frequency are set to zero                 ' /, &
     & SOTID__DSC(SOTID__ZF_LOVE) &
     &  / 'Love numbers for zero frequency: H2=0.6090, L2=0.0852           ' /, &
     & SOTID__DSC(SOTID__ZF_MDG97EL) &
     &  / 'Love numbers for zero frequency are according to MDG97EL        ' /, &
     & SOTID__DSC(SOTID__ZF_MDG97AN) &
     &  / 'Love numbers for zero frequency are according to MDG97AN        ' /, &
     & SOTID__DSC(SOTID__ZF_FLUID) &
     &  / 'Love numbers for zero frequency are according to the fluid limit' /, &
!
     & SOTID__DSC(SOTID__3D_NONE) &
     &  / 'Diplacements of the 3-rd degree are not computed                ' /, &
     & SOTID__DSC(SOTID__3D_MDG97) &
     &  / 'Love numbers of the 3-rd degree according to MDG97 model        ' /
!
      INTEGER*4  SOTID__REQ_GEN_LOVE, &
     &           SOTID__REQ_MODEL_2D, &
     &           SOTID__REQ_ORDER_2D, &
     &           SOTID__REQ_ZF_LOVE, &
     &           SOTID__REQ_MODEL_3D, &
     &           SOTID__REQ_N_STA, &
     &           SOTID__REQ_NW_D2, &
     &           SOTID__REQ_NW_D3
      PARAMETER  ( SOTID__REQ_GEN_LOVE = 1025 )
      PARAMETER  ( SOTID__REQ_MODEL_2D = 1026 )
      PARAMETER  ( SOTID__REQ_ORDER_2D = 1027 )
      PARAMETER  ( SOTID__REQ_ZF_LOVE  = 1028 )
      PARAMETER  ( SOTID__REQ_MODEL_3D = 1029 )
      PARAMETER  ( SOTID__REQ_N_STA    = 1030 )
      PARAMETER  ( SOTID__REQ_NW_D2    = 1031 )
      PARAMETER  ( SOTID__REQ_NW_D3    = 1032 )
!!
      INCLUDE    'love_const.i'
      INCLUDE    'love_numbers.i'
      INCLUDE    'hw95_2d_0002.i'
      INCLUDE    'hw95_3d_0002.i'
