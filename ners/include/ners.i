!
! >>>>> ners.i   2016.05.11  v 2.13 -- 2021.07.17_11:51:22
!
        CHARACTER  NERS__LABEL*28, NERS__BIN_FMT*64, NERS__LS_FMT*46, &
     &             NERS__CONF*56, NERS__SER_FMT*48, NERS__BIN_FMT_21*64, &
     &             NERS__BIN_FMT_22*64, NERS__BIN_FMT_23*64, &
     &             NERS__IO_LOCK*11, NERS__READ_LOCK*13, NERS__WRITE_LOCK*14
        INTEGER*4  NERS__M_URL, NERS__MDEG, NERS__MPAR, NERS__EDG_NODES, NERS__MJUMP
        PARAMETER  ( NERS__LABEL    = 'NERS 20210824  Version 2.140' )
        PARAMETER  ( NERS__BIN_FMT  = '# NERS EOP model. Format version of 2021.08.24. Little Endian.  ' )
        PARAMETER  ( NERS__LS_FMT   = '# NERS Leap Second file.  Format of 2016.06.17' )
        PARAMETER  ( NERS__CONF     = '# NERS Configuration file.  Format version of 2019.10.20' )
        PARAMETER  ( NERS__SER_FMT  = '# NERS EOP series.  Format version of 2016.06.22' )
        PARAMETER  ( NERS__BIN_FMT_21 = '# NERS EOP model. Format version of 2016.09.01. Little Endian.  ' )
        PARAMETER  ( NERS__BIN_FMT_22 = '# NERS EOP model. Format version of 2016.12.06. Little Endian.  ' )
        PARAMETER  ( NERS__BIN_FMT_23 = '# NERS EOP model. Format version of 2016.12.24. Little Endian.  ' )
        PARAMETER  ( NERS__IO_LOCK    = 'ners_io.lck'    )
        PARAMETER  ( NERS__READ_LOCK  = 'ners_read.lck'  )
        PARAMETER  ( NERS__WRITE_LOCK = 'ners_write.lck' )
        PARAMETER  ( NERS__M_URL   =  8 )
        PARAMETER  ( NERS__MDEG    =  3 )
        PARAMETER  ( NERS__MPAR    = 27 )
        PARAMETER  ( NERS__MJUMP   = 64 )
        PARAMETER  ( NERS__EDG_NODES = 3 )
        INTEGER*4    NERS__MAX_EPHE
        PARAMETER  ( NERS__MAX_EPHE = 32768 )
        REAL*8       NERS__TIMEOUT_MIN, NERS__AGE_MIN, NERS__TIM_STEP, &
     &               NERS__MIN_TIM_STEP, NERS__INTR_MAX, NERS__ARG_EPS, &
     &               NERS__MIN_TIM, NERS__MAX_TIM, NERS__FIL_TIM, NERS__TIM_EPS
        PARAMETER  ( NERS__TIMEOUT_MIN  = 1.0 )
        PARAMETER  ( NERS__AGE_MIN  = 1000.0D0 )
        PARAMETER  ( NERS__TIM_STEP = 3600.0D0 )
        PARAMETER  ( NERS__MIN_TIM_STEP = 30.0D0 )
        PARAMETER  ( NERS__INTR_MAX = 10.0D0*86400.0D0 )
        PARAMETER  ( NERS__ARG_EPS  = 1.0D-4 )
        PARAMETER  ( NERS__MIN_TIM  =  -946684800.000D0 ) ! 1970.01.01_00:00:00.000
        PARAMETER  ( NERS__MAX_TIM  =  1577923199.999D0 ) ! 2049.12.31_23:59:59.999
        PARAMETER  ( NERS__FIL_TIM  =          -1.D14   ) ! Fill time
        PARAMETER  ( NERS__TIM_EPS  =  1.0D-8 )
!
        INTEGER*4  NERS__E1,     NERS__E2,    NERS__E3,  &
     &             NERS__H1,     NERS__H2,    NERS__H3,  &
     &             NERS__DP,     NERS__DE,               &
     &             NERS__E1_NUT, NERS__E2_NUT,           &
     &             NERS__DZETA,  NERS__TETA,             &
     &             NERS__ZA,     NERS__EPS0,             &
     &             NERS__SANG
        INTEGER*4  NERS__MEL
        PARAMETER  ( NERS__E1     =  1 ) ! Eulrer angle, axis 1
        PARAMETER  ( NERS__E2     =  2 ) ! Eulrer angle, axis 2
        PARAMETER  ( NERS__E3     =  3 ) ! Eulrer angle, axis 3
        PARAMETER  ( NERS__H1     =  4 ) ! Harmonic EOP variation, axis 1
        PARAMETER  ( NERS__H2     =  5 ) ! Harmonic EOP variation, axis 2
        PARAMETER  ( NERS__H3     =  6 ) ! Harmonic EOP variation, axis 3
        PARAMETER  ( NERS__DP     =  7 ) ! Nutation angle Delta Psi
        PARAMETER  ( NERS__DE     =  8 ) ! Nutation angle Delta Eps
        PARAMETER  ( NERS__E1_NUT =  9 ) ! Euler angle wrt axis 1 because of nutation
        PARAMETER  ( NERS__E2_NUT = 10 ) ! Euler angle wrt axis 1 because of nutation
        PARAMETER  ( NERS__DZETA  = 11 ) ! Precession angle in right ascension dzeta
        PARAMETER  ( NERS__TETA   = 12 ) ! Precession angle in declination
        PARAMETER  ( NERS__ZA     = 13 ) ! Precession angle in right ascension za
        PARAMETER  ( NERS__EPS0   = 14 ) ! Mean obliquity angle
        PARAMETER  ( NERS__SANG   = 15 ) ! Sideral angle
        PARAMETER  ( NERS__MEL    = NERS__SANG )
!
        TYPE NERS__FCS_TYPE
             CHARACTER  NERS_FMT*64      !! An ascii string with format version
             CHARACTER  EOP_FCS_VERS*32  !! An ascii string with format version 
             CHARACTER  NUT_APR_MOD*16   !! Name of the a priori nutation expansion
             CHARACTER  PRC_APR_MOD*16   !! Name of the a priori precession expansion
             CHARACTER  E3Z_APR_MOD*24   !! Name of the a priori model UT1 varaitions caused by zonal tides
             CHARACTER  HEO_MOD*32       !! Name of the expansion of high-frequency EOP variations, not covered with nutation and precession
             CHARACTER  HEO_ID*24        !! Name of the ad hoc expansion of high-frequency EOP variations in addition to a priori expansions
             CHARACTER  EANG_MOD*32      !! Name of the model for the angular momentum
             CHARACTER  LTP_MOD*32       !! Name of the model for the long-term forecast
             CHARACTER  NERS_URL*128     !! The URL with NERS host
!
             CHARACTER  URL_C*128        !! URL of the C04 EOP series
             CHARACTER  URL_U*128        !! URL of the IGS Ultra-rapid EOP series
             CHARACTER  URL_R*128        !! URL of the IGS Rapid EOP series
             CHARACTER  URL_I*128        !! URL of the IVS Intensive EOP series
             CHARACTER  URL_J*128        !! URL of the IAA Intensive EOP series
             CHARACTER  URL_S*128        !! URL of the 24 hour IVS EOP series
             CHARACTER  URL_F*128        !! URL of the final IGS series
             CHARACTER  URL_A*128        !! URL of the AAM forecast data
             CHARACTER  URL_L*128        !! URL of the the long-term forecast
             CHARACTER  URL_RESERVED*128 !! Reserved
!
             REAL*8     TAI_GEN          !! Time of the moment of the forecast generation
             REAL*8     TAI_LAST_HEO     !! Time of the last epoch for observations used for harmonic EOP expansion generation
             REAL*8     TAI_HEO_EPOCH    !! Time for the reference epoch for the ad hoc harmonic EOP expansion
             REAL*8     TAI_LAST_EOPS_C  !! Time for the last epoch of C04 EOP series                   
             REAL*8     TAI_LAST_EOPS_U  !! Time for the last epoch of IGS Ultra-rapid EOP series       
             REAL*8     TAI_LAST_EOPS_R  !! Time for the last epoch of the IGS Rapid EOP series         
             REAL*8     TAI_LAST_EOPS_I  !! Time for the last epoch of the IVS Intensive EOP series     
             REAL*8     TAI_LAST_EOPS_J  !! Time for the last epoch of the IAA Intensive EOP series     
             REAL*8     TAI_LAST_EOPS_S  !! Time for the last epoch of the 24 hour IVS EOP series       
             REAL*8     TAI_LAST_EOPS_F  !! Time for the last epoch of the final IGS series.            
             REAL*8     TAI_LAST_EOPS_A  !! Time for the last epoch of the AAM forecast data            
             REAL*8     TAI_LAST_EOPS_A_ASS  !! Time for the last epoch of the AAM assimilation data    
             REAL*8     TAI_LAST_EOPS_L  !! Time for the last epoch of the long-term forecast           
!  
             INTEGER*4  NK_12            !! Number of knots for Euler angle 1 and 2 forecast
             INTEGER*4  NK_3             !! Number of knots for Euler angle 3 forecast
             INTEGER*4  NC               !! Number of knots of the past reference EOP series
             INTEGER*4  NL               !! Number of knots of the long-term EOP prediction series
             INTEGER*4  NJ               !! Number of epochs of UTC minus TAI jumps
             INTEGER*4  L_HEO            !! Number of constituents of harmonic EOP variations
             INTEGER*4  L_HEOR           !! Number of constituents of cross-harmonics EOP variations
             INTEGER*4  NERS_STATUS      !! Status of NERS fields
!
! ---------- Dynamic variables
!
             REAL*8,    POINTER :: ARG_12(:)         => NULL() !! Arguments for B-spline expansion of EOP forecast for Euler angle components 1,2. Unit: s since 2000.01.01_00:00:00.0 TAI. Dimension: NK_12
             REAL*8,    POINTER :: ARG_3(:)          => NULL() !! Arguments for B-spline expansion of EOP forecast for Euler angle component  3.   Unit: s since 2000.01.01_00:00:00.0 TAI. Dimension: NK_3
             REAL*8,    POINTER :: ARG_C(:)          => NULL() !! Arguments for B-spline expansion of EOP C04 series. Unit: s since 2000.01.01_00:00:00.0 TAI. Dimension: NC
             REAL*8,    POINTER :: ARG_L(:)          => NULL() !! Arguments for B-spline expansion of EOP long-term forecast. Unit: s since 2000.01.01_00:00:00.0 TAI. Dimension: NC
             REAL*8,    POINTER :: ARG_UTC_M_TAI(:)  => NULL() !! Arguments for UTC-M-TAI function. Units: s since 2000.01.01_00:00:00 in s. Dimension: NJ.
             REAL*8,    POINTER :: HEO_ARG(:,:)      => NULL() !! Arguments for a posteriori harmonic Earth Orientation Parameters expansion. Dimension: [L_HEO,3]. The 2nd dimension runs over phase at epoch 2000.01.01_00:00:00.0 TAI in rad, rate in rad/s, and acceleration in rad/s^2
             REAL*8,    POINTER :: HEOR_ARG(:,:)     => NULL() !! Arguments for a posteriori cross-harmonic Earth Orientation Parameters expansion. Dimension: [L_HEOR,3]. The 2nd dimension runs over phase at epoch 2000.01.01_00:00:00.0 TAI in rad, rate in rad/s, and acceleration in rad/s^2
!
             REAL*8,    POINTER :: BSPL_E12(:,:)     => NULL() !! B-spline coefficients for EOP forecast, components 1,2. Units: rad. Dimension: [-2:NK_12-1]
             REAL*8,    POINTER :: BSPL_E3(:)        => NULL() !! B-spline coefficients for EOP forecast, component 3.    Units: rad. Dimension: [-2:NK_3-1]
             REAL*8,    POINTER :: BSPL_C(:,:)       => NULL() !! B-spline coefficients for EOP C04 series. Units: rad. Dimension: [-2:NC-1,3]. The second dimension runs over Euler angle components.
             REAL*8,    POINTER :: BSPL_L(:,:)       => NULL() !! B-spline coefficients for EOP C04 series. Units: rad. Dimension: [-2:NC-1,3]. The second dimension runs over Euler angle components.
             REAL*8,    POINTER :: BSPL_UTC_M_TAI(:) => NULL() !! Value of UTC_M_TAI function on epochs UTC_JUMP. Units: s. Dimension: NJ.
             REAL*8,    POINTER :: HEO_AMP(:,:,:)    => NULL() !! Coefficients of the harmonic EOP expansion. Units: rad. Dimension: [L_HEO,4]. The 2nd dimension runs over 1) cosine constituent of polar motion; 2) since constituent of polar motion; 3) cosine constituent of the axial component; 4) sine constituent of the axial component.
             REAL*8,    POINTER :: HEOR_AMP(:,:,:)   => NULL() !! Coefficients of the cross-harmonic EOP expansion. Units: rad. Dimension: [L_HEOR,4]. The 2nd dimension runs over 1) cosine constituent of polar motion; 2) since constituent of polar motion; 3) cosine constituent of the axial component; 4) sine consituent of the axial component.
             INTEGER*4  NERS_LAST_VARIABLE !! Placeholder
        END TYPE NERS__FCS_TYPE
!
        TYPE NERS__CNF_TYPE
             CHARACTER  URL(NERS__M_URL)*128
             CHARACTER  FCS_FILE*128
             CHARACTER  LEAPSEC_FILE*128
             REAL*8     CONN_TIMEOUT
             REAL*8     READ_TIMEOUT
             REAL*8     LOCK_TIMEOUT
             REAL*8     AGE_FCS
             REAL*8     AGE_SPL
             CHARACTER  LTP_USAGE*8
             CHARACTER  ON_FAIL_TO_READ*8
             INTEGER*4  N_TRIES
             INTEGER*4  N_URL
!
             INTEGER*4  FD_READ_LOCK
             INTEGER*4  FD_WRITE_LOCK
        END TYPE NERS__CNF_TYPE
!
        TYPE NERS__EXP_TYPE
             INTEGER*4  L_TIM
             INTEGER*4  L_NOD
             REAL*8,    POINTER ::      TIM(:)       => NULL() !!
             REAL*8,    POINTER ::      VAL(:,:)     => NULL() !!
             REAL*8,    POINTER ::      ARG(:)       => NULL() !!
             REAL*8,    POINTER ::      BSPL(:,:)    => NULL() !!
        END TYPE NERS__EXP_TYPE
!
        TYPE NERS__EPH_TYPE
             INTEGER*4  L_TIM
             REAL*8,    POINTER :: TIM(:)             => NULL() !!
             REAL*8,    POINTER :: COO_EARTH_VAL(:,:) => NULL() !!
             REAL*8,    POINTER :: COO_EARTH_SPL(:,:) => NULL() !!
        END TYPE NERS__EPH_TYPE
!
        TYPE NERS__TYPE
             CHARACTER  CONFIG_FILE*128
             TYPE ( NERS__CNF_TYPE ) :: CNF
             TYPE ( NERS__FCS_TYPE ) :: FCS
             TYPE ( NERS__EXP_TYPE ) :: EXP
             TYPE ( NERS__EPH_TYPE ) :: EPH
             REAL*8     TIM_START
             REAL*8     TIM_STOP 
             REAL*8     UTC_LOAD
             REAL*8     TIM_LOAD
             REAL*8     UTC_FILE
             REAL*8     TIM_FILE
             REAL*8     UTC_SPLN
             REAL*8     TIM_SPLN
!
             REAL*8     TIM_MATROT_LAST
             REAL*8     MATROT_LAST(NERS__MPAR)
!
             LOGICAL*4  WARN_LTP
             INTEGER*4  EXP_STATUS
             INTEGER*4  CNF_STATUS
             INTEGER*4  EPH_STATUS
             INTEGER*4  FCS_STATUS
        END TYPE  NERS__TYPE
        INTEGER*4    NERS__UNDF, NERS__INIT, NERS__ALLC, &
     &               NERS__LOAD, NERS__COMP, NERS__OK
        PARAMETER  ( NERS__UNDF = 0          )
        PARAMETER  ( NERS__INIT = 1462390484 )
        PARAMETER  ( NERS__ALLC = 1923894723 )
        PARAMETER  ( NERS__LOAD = 1638239041 )
        PARAMETER  ( NERS__COMP = 1293810423 )
        INTEGER*4    NERS__FCS, NERS__EXP, NERS__ALL
        PARAMETER  ( NERS__FCS  =  820148234 )
        PARAMETER  ( NERS__EXP  =  612907562 )
        PARAMETER  ( NERS__ALL  =  126054782 )
        PARAMETER  ( NERS__OK   =  1         )
        CHARACTER    NERS_CAPITAINE_2003_STR*14, NERS_MHB_2000_STR*8, NERS_REN_2000_STR*8
        PARAMETER  ( NERS_CAPITAINE_2003_STR = 'CAPITAINE_2003' )
        PARAMETER  ( NERS_MHB_2000_STR       = 'MHB_2000' )
        PARAMETER  ( NERS_REN_2000_STR       = 'REN_2000' )
!
        REAL*8       NERS_CROSS_NUT_RATE_E3_MHB2000, NERS_CROSS_NUT_RATE_E3_REN2000
        PARAMETER  ( NERS_CROSS_NUT_RATE_E3_MHB2000 = -5.9232757709D-18 ) ! rad/s  or -3.85559184 mas/Jul_cent
        PARAMETER  ( NERS_CROSS_NUT_RATE_E3_REN2000 = -6.0979186478D-18 ) ! rad/s  or -3.96927077 mas/Jul_cent
        CHARACTER    NERS__STOP*8, NERS__WARNING*8, NERS__SILENT*8, NERS__IGNORE*8
        PARAMETER  ( NERS__STOP    = 'stop    ' )
        PARAMETER  ( NERS__WARNING = 'warning ' )
        PARAMETER  ( NERS__SILENT  = 'silent  ' )
        PARAMETER  ( NERS__IGNORE  = 'ignore  ' )
        INTEGER*4    NERS__SIZE_EXTRA
        PARAMETER  ( NERS__SIZE_EXTRA = 4096 )
        REAL*8       NERS__C, NERS__REA, NERS__FE, NERS__EXC_SQ 
        PARAMETER  ( NERS__C =  299792458.0D0 )
        PARAMETER  ( NERS__REA     = 6378136.7D0 )       ! Earth's radius
        PARAMETER  ( NERS__FE      = 1.D0/298.257D0 )    ! Earth's flattening
        PARAMETER  ( NERS__EXC_SQ  = 2.D0*NERS__FE - NERS__FE**2 )   ! Earth's eccentricity
!
        CHARACTER    NERS__E3Z_D93*24, NERS__E3Z_RE2014*24, NERS__E3Z_NONE*24
        PARAMETER  ( NERS__E3Z_D93    = 'Dickman 1993            ' )
        PARAMETER  ( NERS__E3Z_RE2014 = 'Ray & Erofeeva 2014     ' )
        PARAMETER  ( NERS__E3Z_NONE   = 'None                    ' )
!
        INTEGER*4    NERS__FCS_FIL_LEN_MIN, NERS__LPS_FIL_LEN_MIN
        PARAMETER  ( NERS__FCS_FIL_LEN_MIN = 524288 )
        PARAMETER  ( NERS__LPS_FIL_LEN_MIN = 1541   )
        REAL*8       NERS__LPS_WRITE_TIMEOUT, NERS__LPS_AGE
        PARAMETER  ( NERS__LPS_WRITE_TIMEOUT = 0.01D0 )
        PARAMETER  ( NERS__LPS_AGE           = 20.0D0*86400.0D0 )
!
        REAL*8       NERS__HEIGHT_MIN, NERS__HEIGHT_MAX, NERS__ANG_EPS 
        PARAMETER  ( NERS__HEIGHT_MIN = 0.994D0*NERS__REA )
        PARAMETER  ( NERS__HEIGHT_MAX = 1.006D0*NERS__REA )
        PARAMETER  ( NERS__ANG_EPS    = 1.0D-8 )
        CHARACTER    NERS__EPHE_FIL*22, NERS__EPHE_LABEL*38
        PARAMETER  ( NERS__EPHE_FIL   = 'de403_earth_spline.dat' )
        PARAMETER  ( NERS__EPHE_LABEL = 'Earth orbit from JPL ephemerides DE403' )
        CHARACTER    NERS__REFR_NONE*4, NERS__REFR_OPTIC*5, NERS__REFR_RADIO*5
        PARAMETER  ( NERS__REFR_NONE  = 'none' )
        PARAMETER  ( NERS__REFR_OPTIC = 'optic' )
        PARAMETER  ( NERS__REFR_RADIO = 'radio' )
!
! <<<<  end of ners.i
