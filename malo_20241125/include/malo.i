!
! >>>>> malo.i   2012.10.12 v 2.09 -- 2020.11.03_10:16:16
!
        CHARACTER    MALO__LABEL*22
        PARAMETER  ( MALO__LABEL  = 'MALO  2.09  2020.11.03' )
        INTEGER*4    MALO__ATT, MALO__MLPS, MALO__FIL, MALO__MDIM, &
      &              MALO__MHEI, MALO__MDEG, MALO__NDEG, MALO__MFRQ, &
      &              MALO__MWAV, MALO__MSTA, MALO__CNF, MALO__MFS
        PARAMETER  ( MALO__ATT    =       64 )
        PARAMETER  ( MALO__MLPS   =       64 )
        PARAMETER  ( MALO__CNF    =       21 )
        PARAMETER  ( MALO__FIL    = 512*1024 )
        PARAMETER  ( MALO__MDIM   =    33000 )
        PARAMETER  ( MALO__MHEI   =      125 )
        PARAMETER  ( MALO__MDEG   =        3 )
        PARAMETER  ( MALO__NDEG   =     2800 )
        PARAMETER  ( MALO__MSTA   =     8192 )
        PARAMETER  ( MALO__MFRQ   =       30 )
        PARAMETER  ( MALO__MWAV   =      256 )
        PARAMETER  ( MALO__MFS    =        7 )
!
        INTEGER*4    MALO__STACK_SIZE_IN_GIGABYTES
        PARAMETER  ( MALO__STACK_SIZE_IN_GIGABYTES = 4 )
        INTEGER*4    MALO__MJD_MIN, MALO__MJD_MAX
        PARAMETER  ( MALO__MJD_MIN = 40000 )
        PARAMETER  ( MALO__MJD_MAX = 75000 )
!
        TYPE MALO_LEAPS__TYPE
  	     INTEGER*4  L_LPS
	     CHARACTER  FINAM_LEAPSEC*128
	     INTEGER*4  MJD_LPS(MALO__MLPS)
	     REAL*8     TAI_LPS(MALO__MLPS)
	     REAL*8     UTC_M_TAI(MALO__MLPS)
	     INTEGER*4  STATUS
        END TYPE MALO_LEAPS__TYPE
!
        TYPE MALO_STA__TYPE
    	     REAL*8     COO(3)
	     REAL*8     LAT_GDT
	     REAL*8     LON
	     REAL*8     HEI_ELL
	     REAL*8     HEI_GEOID
	     REAL*8     TAI_BEG
	     REAL*8     TAI_END
	     INTEGER*4  MJD_BEG
	     INTEGER*4  MJD_END
	     CHARACTER  NAME*10
        END TYPE MALO_STA__TYPE
!
	TYPE MALO_CONF__TYPE
             CHARACTER  DEFAULT_DATE*256
             CHARACTER  FINAM_LS_MASK*256
             CHARACTER  FINAM_MODEL*256
             CHARACTER  EPHEDISP_FINAM_FMT*256
             CHARACTER  HARPOS_FINAM_FMT*256
             CHARACTER  SPHE_FINAM_FMT*256
   	     CHARACTER  LOA_FINAM_FMT*256
	     CHARACTER  LOA_FINAM_DESCR*256
	     CHARACTER  LOA_FINAM_COMM*256
	     CHARACTER  STATION_FINAM*256
	     CHARACTER  AGRA_FINAM_DESC*256
	     CHARACTER  AGRA_FINAM_FMT*256
             CHARACTER  FINAM_SPHER_NRM*256
             CHARACTER  UPGRID_LS_MASK*128
             CHARACTER  SC_FILE*128
             CHARACTER  LOVE_FILE*256
	     CHARACTER  SURFACE_TYPE*8 
	     CHARACTER  MODEL_USE*8
!
	     INTEGER*4  MODEL_CODE
	     INTEGER*4  OUTPUT_GRID_DEG
!
	     LOGICAL*1  MONTHLY_EPH
	     LOGICAL*1  BDS_FORMAT
	     LOGICAL*1  KEEP_EPHEDISP_ORIG
	     CHARACTER  CONFIG*128
	END TYPE MALO_CONF__TYPE
!
        INTEGER*4  MALO__CNST, MALO__DRFT, MALO__JMP, MALO__COS, MALO__SIN
        PARAMETER  ( MALO__CNST = 900000001 )
        PARAMETER  ( MALO__DRFT = 900000002 )
        PARAMETER  ( MALO__JMP  = 900000003 )
        PARAMETER  ( MALO__COS  = 900000004 )
        PARAMETER  ( MALO__SIN  = 900000005 )
!
	TYPE      MALO__MODC
             INTEGER*4  TYP
             CHARACTER  WAV*4
             REAL*8     TIM
             REAL*8     PHS
             REAL*8     FRQ
             REAL*8     ACC
        END  TYPE MALO__MODC
!
	TYPE      MALO__TYPE
            INTEGER*4  NLAT
            INTEGER*4  NLON
            INTEGER*4  NTIM
            INTEGER*4  NLEV
            INTEGER*4  NMDC
            INTEGER*4  MJD_BEG
            INTEGER*4  MJD_END
            INTEGER*4  MJD_DEF
            REAL*8     TAI_BEG
            REAL*8     TAI_END
            REAL*8     UTC_BEG
            REAL*8     UTC_END
            REAL*8     TIM_STEP
            REAL*8     TAI_DEF
	    INTEGER*4  N_ACC_TIM
            INTEGER*4  MODC_NHAR
            INTEGER*4  MODC_NHAR_SUBTR
            INTEGER*4  IND_MOD_CNST
!
	    INTEGER*4  NUM_ATT(0:4)
	    INTEGER*4  TYP_ATT(MALO__ATT,0:4)
	    INTEGER*4  LEN_ATT(MALO__ATT,0:4)
	    CHARACTER  NAM_ATT(MALO__ATT,0:4)*32
	    INTEGER*1  VAL_ATT(128,MALO__ATT,0:4)
!
	    INTEGER*4  SPR_STATUS
	    INTEGER*4  SPH_STATUS
	    INTEGER*4  LSM_STATUS
	    INTEGER*4  MDC_STATUS
	    INTEGER*4  PPWTEM_STATUS
	    INTEGER*4  REFRA_4D_STATUS
	    INTEGER*4  NLOVE
	    CHARACTER  DATA_TYPE*32
	    CHARACTER  SPR_NAME*8
	    CHARACTER  SPR_LONG_NAME*128
	    CHARACTER  SPR_UNITS*16
	    REAL*4     SPR_MISSING
	    INTEGER*4  ORD_SPHE
!
	    TYPE     ( MALO_CONF__TYPE )          :: CONF
	    TYPE     ( MALO__MODC      ), POINTER :: MODC(:) => NULL()
!
	    REAL*4,    POINTER :: LEV(:)             => NULL()
	    REAL*4,    POINTER :: LAT(:)             => NULL()
	    REAL*4,    POINTER :: LON(:)             => NULL()
	    REAL*4,    POINTER :: TIM(:)             => NULL()
	    INTEGER*4, POINTER :: MJD_ARR(:)         => NULL()
	    REAL*8,    POINTER :: TAI_ARR(:)         => NULL()
	    REAL*4,    POINTER :: SPR(:,:,:)         => NULL()
	    REAL*8,    POINTER :: SPH(:,:,:,:,:)     => NULL()
            REAL*4,    POINTER :: LSM(:,:)           => NULL()
            REAL*4,    POINTER :: LOVE(:,:)          => NULL()
	    REAL*4,    POINTER :: PPWTEM_4D(:,:,:,:,:) => NULL()
	    REAL*4,    POINTER :: REFRA_4D(:,:,:,:)  => NULL()
	    REAL*4,    POINTER :: PRES_3D(:,:,:,:)   => NULL()
!
	    TYPE ( MALO_LEAPS__TYPE ) :: LEAPSEC
   	    TYPE ( MALO_STA__TYPE   ), POINTER :: STA(:) => NULL()
            INTEGER*4  NSTA
            INTEGER*4  IVRB
            CHARACTER  PLOT_UNIT*16
	    INTEGER*4  STA_STATUS
	    CHARACTER  FILOUT*128
        END TYPE  MALO__TYPE
!
        INTEGER*4    EOPS__J, EOPS__I, EOPS__U, EOPS__R, EOPS__F, EOPS__S, &
     &               EOPS__L, EOPS__C, EOPS__A, M__EOPS, M__EOPA
        PARAMETER  ( EOPS__J = 1 )
        PARAMETER  ( EOPS__I = 2 )
        PARAMETER  ( EOPS__U = 3 )
        PARAMETER  ( EOPS__R = 4 )
        PARAMETER  ( EOPS__F = 5 )
        PARAMETER  ( EOPS__S = 6 )
        PARAMETER  ( EOPS__L = 7 )
        PARAMETER  ( EOPS__C = 8 )
        PARAMETER  ( EOPS__A = 9 )
        PARAMETER  ( M__EOPS = EOPS__C )
        PARAMETER  ( M__EOPA = EOPS__A )
        CHARACTER    EOPS__NAME(M__EOPA)*6, EOPS__CMP(3)*2
        DATA         EOPS__NAME / &
     &                            'EOPS_J', &
     &                            'EOPS_I', &
     &                            'EOPS_U', &
     &                            'EOPS_R', &
     &                            'EOPS_F', &
     &                            'EOPS_S', &
     &                            'EOPS_C', &
     &                            'EOPS_L', &
     &                            'EOPS_A'  &
     &                          /
        DATA         EOPS__CMP  / &
     &                            'E1', &
     &                            'E2', &
     &                            'E3'  &
     &                          /
!
        INTEGER*4  MALO__I31_NIB, MALO__I32_NIB, MALO__I33_NIB, &
     &             MALO__I31_IB,  MALO__I32_IB,  MALO__I33_IB,  &
     &             MALO__H1, MALO__H2, MALO__H3,                &
     &             MALO__XI1_NIB, MALO__XI2_NIB, MALO__XI3_NIB, &
     &             MALO__XI1_IB,  MALO__XI2_IB,  MALO__XI3_IB,  &
     &             M__AAM
        PARAMETER  ( MALO__I31_NIB =  1 )
        PARAMETER  ( MALO__I32_NIB =  2 )
        PARAMETER  ( MALO__I33_NIB =  3 )
        PARAMETER  ( MALO__I31_IB  =  4 )
        PARAMETER  ( MALO__I32_IB  =  5 )
        PARAMETER  ( MALO__I33_IB  =  6 )
        PARAMETER  ( MALO__H1      =  7 )
        PARAMETER  ( MALO__H2      =  8 )
        PARAMETER  ( MALO__H3      =  9 )
        PARAMETER  ( MALO__XI1_NIB = 10 )
        PARAMETER  ( MALO__XI2_NIB = 11 )
        PARAMETER  ( MALO__XI3_NIB = 12 )
        PARAMETER  ( MALO__XI1_IB  = 13 )
        PARAMETER  ( MALO__XI2_IB  = 14 )
        PARAMETER  ( MALO__XI3_IB  = 15 )
        PARAMETER  ( M__AAM        = 15 )
!
        TYPE MALO_EOP_CONF__TYPE
             CHARACTER  FIL_EOP(M__EOPS)*128
             CHARACTER  URL_EOP(M__EOPS)*128
             CHARACTER  FIL_AAM_SER*128
             CHARACTER  URL_AAM_SER*128
             CHARACTER  FIL_LEAPSEC*128
             CHARACTER  FIL_HEO*128
             CHARACTER  EOP_FCS_PREF*128
             CHARACTER  NUT_APR_MOD*16   !! Name of the a priori nutation expansion
             CHARACTER  PRC_APR_MOD*16   !! Name of the a priori precession expansion
             CHARACTER  E3Z_MOD*24       !! Name of the a priori model UT1 varaitions caused by zonal tides
             CHARACTER  HEO_MOD*128      !! Name of the expansion of high-frequency EOP variations, not covered with nutation and precession
             CHARACTER  HEO_ID*24        !! Name of the ad hoc expansion of high-frequency EOP variations in addition to a priori expansions
             CHARACTER  LTP_MOD*32       !! Name of the model for the long-term forecast
             CHARACTER  NERS_URL*128     !! The URL for NERS file
        END TYPE MALO_EOP_CONF__TYPE
!
        TYPE MALO_EOPS__TYPE
             REAL*8     TIM
             REAL*8     E(3)
             REAL*8     ER(3)
             REAL*8     DE(3)
             REAL*8     DER(3)
        END  TYPE MALO_EOPS__TYPE
!
        TYPE EOPS_PTR__TYPE
             INTEGER*4  NP
             CHARACTER  FIL_EOPS*128
             TYPE ( MALO_EOPS__TYPE ), POINTER :: SER(:) => NULL()
        END  TYPE EOPS_PTR__TYPE
!
        TYPE EOPS_AAM__TYPE
             INTEGER*4  NP
             CHARACTER  FIL_AAM*128
             REAL*8,    POINTER :: TIM(:)   => NULL()
             REAL*8,    POINTER :: VAL(:,:) => NULL()
        END  TYPE EOPS_AAM__TYPE
        TYPE MALO__EOP_TYPE
             CHARACTER  CNF_FILE*128
             TYPE ( EOPS_PTR__TYPE ) :: EOPS(M__EOPS)
             TYPE ( EOPS_AAM__TYPE ) :: AAM
             INTEGER*4  LO
   	     TYPE ( MALO_LEAPS__TYPE    ) :: LEAPSEC
   	     TYPE ( MALO_EOP_CONF__TYPE ) :: CONF
             INTEGER*4  IVRB
             INTEGER*4  STATUS
        END  TYPE MALO__EOP_TYPE
!
	INTEGER*4  M__OND
	PARAMETER  ( M__OND = 13 )
	TYPE      MALO_OND__TYPE
	       CHARACTER  HOST_NAME*128
	       CHARACTER  SERVER_GROUP*16
	       CHARACTER  LOAD_TYPE*3
	       CHARACTER  OND_DIR*128
	       CHARACTER  REQ_DIR*128
	       CHARACTER  REQ_HTML_DIR*128
	       CHARACTER  FROM_EMAIL*128
	       CHARACTER  QUEUE_FILE*128
	       CHARACTER  QUEUE_LOCK_FILE*128
	       CHARACTER  OND_LOCK_FILE*128
	       CHARACTER  MALO_ROOT*128
	       CHARACTER  LOG_FILE*128
	       CHARACTER  MALO_BIN*128
	       CHARACTER  MAIL_BIN*128
	       INTEGER*4  MAX_PROC
	END TYPE  MALO_OND__TYPE
!
        TYPE      MALO_QUE__TYPE
  	       CHARACTER  DATE_ORIG*15
  	       CHARACTER  MODEL*20
  	       CHARACTER  MODE*12
  	       CHARACTER  FRAME*2
  	       INTEGER*4  N_STA
               CHARACTER  START_DATE*19
               CHARACTER  STOP_DATE*19
               CHARACTER  IP_ADDR*15
               CHARACTER  EMAIL*40
               CHARACTER  STAT*1
               INTEGER*4  PID
        END TYPE  MALO_QUE__TYPE
!
        TYPE      MOD_INFO__TYPE
             INTEGER*4  RANK
             INTEGER*4  DIMS(4)
             INTEGER*4  IND_HOR_TILE
             INTEGER*4  IND_VER_TILE
             REAL*8     PIXEL_SIZE
             REAL*8     NOR_LAT
             REAL*8     SOU_LAT
             REAL*8     EAS_LON
             REAL*8     WES_LON
             REAL*8     ULP(2)
             REAL*8     LRP(2)
             REAL*8     RAD_PROJ
             CHARACTER  FIL*128
             INTEGER*1  FILL_VALUE
             INTEGER*1  FILLER(3)
             INTEGER*4  STATUS
        END TYPE  MOD_INFO__TYPE
        TYPE       MALO__DAMB_TYPE
             REAL*8     LAT_BEG
             REAL*8     LON_BEG
             REAL*8     LAT_END
             REAL*8     LON_END
             CHARACTER  NAME*32
             INTEGER*1  IND
        END TYPE   MALO__DAMB_TYPE
        TYPE       MALO__LAKE_TYPE
             REAL*8     LAT_GDT
             REAL*8     LON
             CHARACTER  NAME*32
             INTEGER*1  IND
        END TYPE   MALO__LAKE_TYPE
!
	INTEGER*4  MALO__UNDF, MALO__ALLO, MALO__LOAD, MALO__COMP
        PARAMETER  ( MALO__UNDF = 0 )
        PARAMETER  ( MALO__ALLO = 298730128 )
	PARAMETER  ( MALO__LOAD = 482920129 )
        PARAMETER  ( MALO__COMP = 673903026 )
!
	CHARACTER    MALO_INTRVAL_MON_MULTI*13, MALO_INTRVAL_DAY_MULTI*11, &
                     MALO_INTRVAL_3HR_SINGLE*14
	PARAMETER  ( MALO_INTRVAL_MON_MULTI  = 'monthly_multi'  )
	PARAMETER  ( MALO_INTRVAL_DAY_MULTI  = 'dayly_multi'    )
	PARAMETER  ( MALO_INTRVAL_3HR_SINGLE = '3hourly_single' )
        CHARACTER    MALO__LAND*4, MALO__LAKE*4, MALO__OCEAN*5, MALO__ALL*3
        PARAMETER  ( MALO__LAND  = 'LAND'  ) 
	PARAMETER  ( MALO__OCEAN = 'OCEAN' )  
	PARAMETER  ( MALO__LAKE  = 'LAKE'  ) 
	PARAMETER  ( MALO__ALL   = 'ALL'   ) 
        INTEGER*4    MALO__H, MALO__L, MALO__K
        PARAMETER  ( MALO__H = 1 )
        PARAMETER  ( MALO__L = 2 )
	PARAMETER  ( MALO__K = 3 )
        CHARACTER    MALO__SPR*10, MALO__INSP*10
	PARAMETER  ( MALO__SPR  = 'MALO_SPR' )
	PARAMETER  ( MALO__INSP = 'MALO_INSP' )
        INTEGER*1    MALO__WATER_VAL, MALO__LAND_VAL, MALO__SEA_VAL, MALO__MLAC
        PARAMETER  ( MALO__WATER_VAL = 0 )
        PARAMETER  ( MALO__LAND_VAL  = 1 )
        PARAMETER  ( MALO__SEA_VAL   = 2 )
        PARAMETER  ( MALO__MLAC      = 32 )
        INTEGER*4    MALO__MDAMB
        PARAMETER  ( MALO__MDAMB = 1024 )
!
        REAL*8       MALO__DENS, MALO__SW_DENS, MALO__GRAV, MALO__UNI_GRAV, &
     &               MALO__ACC_EQU_WGS84, MALO__GRV_LAT_WGS84, MALO__OMEGA_EGM96, &
     &               MALO__FLAT_WGS84
	PARAMETER  ( MALO__DENS = 5515.0D0  ) ! kg/m^3
	PARAMETER  ( MALO__SW_DENS = 1027.0D0  ) ! kg/m^3
       	PARAMETER  ( MALO__GRAV = 9.80665D0 ) ! kg m/s^2
        PARAMETER  ( MALO__ACC_EQU_WGS84 = 9.7803253359D0     ) ! Equatorial gravity acc.
        PARAMETER  ( MALO__GRV_LAT_WGS84 = 0.00193185265241D0 ) ! D(ACC_EQU)/D(phi)
        PARAMETER  ( MALO__OMEGA_EGM96   = 7.292115D-5 )
        PARAMETER  ( MALO__FLAT_WGS84    = 1.0D0/298.2572235630D0 )
        PARAMETER  ( MALO__UNI_GRAV = 6.67384D-11 ) ! m^3 kg^-1 s^-2
!
	CHARACTER    MALO_CONFIG__LABEL*48, LOVE_NUMBERS__LABEL*36, &
     &               LOVE_NUMBERS__LABEL_01*36, SPHE__LABEL*47, &
     &               LOA__LABEL*47, MALO_STA__LABEL*34, OND__LABEL*64, &
     &               MALO__LAK_LABEL*54, MALO__DAMB_LABEL*54, &
     &               EOP_SER__LABEL*62, EOP_CONF__LABEL*60, MALO_CONFIG__LABEL_V1*48, &
     &               EOP_FCS__LABEL*32
	PARAMETER  ( MALO_CONFIG__LABEL_V1  = '# MALO  Configuration file. Format of 2013.06.03' )
	PARAMETER  ( MALO_CONFIG__LABEL     = '# MALO  Configuration file. Format of 2017.02.22' )
	PARAMETER  ( LOVE_NUMBERS__LABEL    = '# LOVE_NUMBERS  Format of 2005.11.22' )
	PARAMETER  ( LOVE_NUMBERS__LABEL_01 = '# LOVE_NUMBERS  Format of 2005.01.10' )
	PARAMETER  ( SPHE__LABEL      = 'SPHE FORMAT   Version of 2012.11.21, BIG-ENDIAN' )
	PARAMETER  ( LOA__LABEL       = 'LOA FORMAT    Version of 2012.11.21, BIG-ENDIAN' )
	PARAMETER  ( MALO_STA__LABEL  = 'SITLIST Format  Version 2003.07.31' )
	PARAMETER  ( OND__LABEL       = '# MALO_ONDEMAND Configuration file, format version of 2022.11.16' )
        PARAMETER  ( MALO__LAK_LABEL  = '#  Lake definition file.  Format version of 2016.01.07' )
        PARAMETER  ( MALO__DAMB_LABEL = '#  Damb definition file.  Format version of 2016.01.08' )
        PARAMETER  ( EOP_SER__LABEL   = '# Earth orientation time series.  Format version of 2016.03.04' )
        PARAMETER  ( EOP_CONF__LABEL  = '# EOP processing control file.  Format version of 2023.03.03'   )
        PARAMETER  ( EOP_FCS__LABEL   = 'EOP_FCS  Version of 2018.01.06  ' )
	INTEGER*4    MALO__LOA_LTXT, MALO__LOA_LSTR, MALO__SHC_LTXT, MALO__SHC_LSTR, MALO__MEOP
	PARAMETER  ( MALO__LOA_LTXT =  8 )
	PARAMETER  ( MALO__LOA_LSTR = 64 )
	PARAMETER  ( MALO__SHC_LTXT = 64 ) ! Maximum number of lines in spherical
!                                          ! harmonics file text section
	PARAMETER  ( MALO__SHC_LSTR = 80 )
        PARAMETER  ( MALO__MEOP     = 13 ) ! Number of relevant parameters in the eop configuration file
!
	INTEGER*4    MALO__DIR, MALO__INV, MALO__TRA, MALO__SPH1, MALO__SPH2
	PARAMETER  ( MALO__DIR  = 1000001 )
	PARAMETER  ( MALO__INV  = 2000002 )
	PARAMETER  ( MALO__TRA  = 3000003 )
	PARAMETER  ( MALO__SPH1 = 4000001 )
	PARAMETER  ( MALO__SPH2 = 4000002 )
	REAL*8       MALO__HEIGHT_MIN, MALO__HEIGHT_MAX, MALO__RD_AREA, &
     &               MALO__TIME_EPS
	PARAMETER  ( MALO__HEIGHT_MIN = -1000.0D0 ) ! minimal height
	PARAMETER  ( MALO__HEIGHT_MAX =  9500.0D0 ) ! maximal height
        PARAMETER  ( MALO__RD_AREA    =  3000.0D0 ) ! Radius of loading validity
        PARAMETER  ( MALO__TIME_EPS   =  90.0D0   ) ! Tolerance for time epochs
!
        REAL*8     REA__WGS84, FLAT__WGS84, GM__EGM96, OMEGA__EGM96, &
     &             ACC_EQU__WGS84, GRV_LAT__WGS84, W0__IAU2004
        PARAMETER  ( REA__WGS84    = 6378137.0D0 )
        PARAMETER  ( FLAT__WGS84   = 1.0D0/298.2572235630D0 )
        PARAMETER  ( GM__EGM96     = 3.986004418D14 )
        PARAMETER  ( OMEGA__EGM96  = 7.292115D-5 )
        PARAMETER  ( ACC_EQU__WGS84 = 9.7803253359D0     ) ! Equatorial gravity acc.
        PARAMETER  ( GRV_LAT__WGS84 = 0.00193185265241D0 ) ! D(ACC_EQU)/D(phi)
	PARAMETER  ( W0__IAU2004    = 62636856.0D0       ) ! m^2/s^2 Geopotential value defined by
!                                                          ! M. Bursa etal, The geopotential value 
!                                                          ! W0 for specifying the relativistic atomic 
!                                                          ! time scale and a global vertical reference system, 
!                                                          ! JoG, 81(2)2, pp 103-110, 2007
!
! ----- GMAO constants
!
	REAL*8       R__MAPL, MA__MAPL, H2O__MAPL, ACCREF__MAPL, KAPPA__MAPL
        PARAMETER  ( R__MAPL      = 8.3143D0 )    ! Universal gas constant
        PARAMETER  ( MA__MAPL     = 28.97D-3 )    ! Molar mass of dry air
        PARAMETER  ( H2O__MAPL    = 18.01D-3 )    ! Molar mass of wet air
        PARAMETER  ( ACCREF__MAPL = 9.80D0   )    ! Reference gravity acceleration
        PARAMETER  ( KAPPA__MAPL  = 2.0D0/7.0D0 ) ! Adiabatic constant
	REAL*8       GP0, GP1          ! Dependence of gravity acceleration on Pressure according to ISO atmosphere
	PARAMETER  ( GP0 = -2.427388D-02 ) ! g(P) = G_ell*(1.0D + GP0 + GP1*ln(P))
	PARAMETER  ( GP1 =  2.088507D-03 )
!
!  Department of defence World Geodestic System 1984, Department of defence World Geodestic System 1984,
!  NIMA Technical Report TR8350.2
!
!  Milan Bursa, Steve Kenyon, Jan Kouba, Zdislav Sima, Viliam Vatrt, Vojtech Vitek, Marie Vojtiskova,
!  "The geopotential value W 0 for specifying the relativistic atomic time scale and a global vertical reference system",
!  Journal of Geodesy, February 2007, Volume 81, Issue 2, pp 103-110 
!
        INTERFACE
            SUBROUTINE GET_FILE_BUFFER ( FILIN, BUF, LBUF, IUER )
                CHARACTER  FILIN*(*)
                CHARACTER, POINTER :: BUF(:)*1
                INTEGER*4  LBUF, IUER
            END SUBROUTINE GET_FILE_BUFFER 
        END INTERFACE
!
      REAL*8       E0__MALO, LV__MALO, RV__MALO, T0__MALO, ACP__MALO
      PARAMETER  ( E0__MALO  = 611.0D0  )
      PARAMETER  ( LV__MALO  = 2.5D6    )
      PARAMETER  ( RV__MALO  = 461.5D0  )
      PARAMETER  ( T0__MALO  = 273.15D0 )
      PARAMETER  ( ACP__MALO = 2.32D-7  )
      INTEGER*4    MALO__INQ, MALO__REA
      PARAMETER  ( MALO__INQ = 928137 )
      PARAMETER  ( MALO__REA = 231048 )
!
      CHARACTER   MALO__MOD_IGNORE*6,   MALO__MOD_SUBTRACT*8,   &
     &            MALO__MOD_HAR_ONLY*8, MALO__MOD_SUB_NOHAR*13, MALO__NONE*4
      PARAMETER ( MALO__MOD_IGNORE    = 'IGNORE'   )
      PARAMETER ( MALO__MOD_SUBTRACT  = 'SUBTRACT' )
      PARAMETER ( MALO__MOD_HAR_ONLY  = 'HAR_ONLY' )
      PARAMETER ( MALO__MOD_SUB_NOHAR = 'SUB_NOHA' )
      PARAMETER ( MALO__NONE          = 'NONE'     )
!
      INTEGER*4   MALO__P, MALO__PW, MALO__TEM
      PARAMETER  ( MALO__P   = 1 )
      PARAMETER  ( MALO__PW  = 2 )
      PARAMETER  ( MALO__TEM = 3 )
      INTEGER*4  MALO__REFRA_IC1, MALO__REFRA_IC2, MALO__REFRA_RADIO
!
      PARAMETER  ( MALO__REFRA_IC1   = 32001 )
      PARAMETER  ( MALO__REFRA_IC2   = 32002 )
      PARAMETER  ( MALO__REFRA_RADIO = 33003 )
      CHARACTER    MALO__LEAPSEC_FILE*128
      PARAMETER  ( MALO__LEAPSEC_FILE = 'malo_leapsec.dat' )
!
      INTEGER*4  MALO__ACP_GRID, MALO__ACP_STA
      PARAMETER  ( MALO__ACP_GRID = 1 ) 
      PARAMETER  ( MALO__ACP_STA  = 2 ) 
!
      CHARACTER   MALO__FRAME_CM*2, MALO__FRAME_CF*2, MALO__FRAME_D1*2
      PARAMETER ( MALO__FRAME_CM = 'cm' )
      PARAMETER ( MALO__FRAME_CF = 'cf' )
      PARAMETER ( MALO__FRAME_D1 = 'd1' )
      INTEGER*4    MALO__VGEP_DEG
      PARAMETER  ( MALO__VGEP_DEG = 64  )
!
      CHARACTER  AAM__FMT*36, AAM_TAB__LABEL*42, AAM_SER__LABEL*42
      PARAMETER  ( AAM__FMT       = '# AAM  Format  Version of 2015.11.30' )
      PARAMETER  ( AAM_TAB__LABEL = '# AAM_TABLE   Format Version of 2015.11.30' )
      PARAMETER  ( AAM_SER__LABEL = '# AAM_SERIES  Format Version of 2015.11.30' )
      REAL*8     MALO__X1_MAT_COEF, MALO__X1_MOT_COEF, &
     &           MALO__X3_MAT_COEF, MALO__X3_MOT_COEF
      PARAMETER  ( MALO__X1_MAT_COEF = 4.17767D-36 )
      PARAMETER  ( MALO__X3_MAT_COEF = 1.04950D-38 )
      PARAMETER  ( MALO__X1_MOT_COEF = 8.37576D-32 )
      PARAMETER  ( MALO__X3_MOT_COEF = 1.91966D-34 )
!!
       INTEGER*4  MOT
       PARAMETER  ( MOT = 65 )
       INTEGER*4  OTID_ORD(MOT)
       CHARACTER  OTID_WAV(MOT)*4
       REAL*8     OTID_PHS(MOT), OTID_FRQ(MOT), OTID_ACC(MOT), OTID_AMP(MOT)
       INTEGER*4  N1$1
       DATA ( OTID_WAV(N1$1), OTID_PHS(N1$1), OTID_FRQ(N1$1), &
     &        OTID_ACC(N1$1), OTID_AMP(N1$1), OTID_ORD(N1$1), &
     &        N1$1=1,MOT ) &
     &   / &
     &   'NOD ',  4.100746D0, 1.069696236521D-08, -7.28D-24, 0.273592, 0, & !  1
     &   'SA  ',  3.098467D0, 1.990968752920D-07, -5.39D-25, 0.048884, 0, & !  2
     &   'SSA ',  0.365348D0, 3.982127698995D-07,  2.13D-24, 0.303119, 0, & !  3
     &   'MSM ',  4.899785D0, 2.285998575769D-06, -4.34D-23, 0.065922, 0, & !  4
     &   'MM  ',  5.497148D0, 2.639203052741D-06,  3.10D-23, 0.344743, 0, & !  5
     &   'MSF ',  0.972155D0, 4.925201628510D-06, -1.24D-23, 0.057189, 0, & !  6
     &   'MF  ',  4.479096D0, 5.323414398410D-06, -1.03D-23, 0.652632, 0, & !  7
     &   'MF+ ',  2.296657D0, 5.334111360775D-06, -1.76D-23, 0.270591, 0, & !  8
     &   'MTM ',  0.551466D0, 7.962617451151D-06,  2.08D-23, 0.124958, 0, & !  9
     &   'MTM+',  4.652212D0, 7.973314413516D-06,  1.35D-23, 0.051790, 0, & ! 10
     &   'MSQM',  2.309658D0, 1.024861602692D-05, -2.27D-23, 0.019958, 0, & ! 11
     &   '2Q1 ',  0.417245D0, 6.231933804749D-05, -5.07D-23, 0.065101, 1, & ! 12
     &   'SIG1',  1.014608D0, 6.267254252446D-05,  2.37D-23, 0.078504, 1, & ! 13
     &   'Q1- ',  4.955240D0, 6.494784413786D-05, -1.24D-23, 0.092777, 1, & ! 14
     &   'Q1  ',  2.772800D0, 6.495854110023D-05, -1.97D-23, 0.491954, 1, & ! 15
     &   'RHO1',  3.370164D0, 6.531174557720D-05,  5.48D-23, 0.093377, 1, & ! 16
     &   'O1- ',  1.027610D0, 6.758704719061D-05,  1.86D-23, 0.484725, 1, & ! 17
     &   'O1  ',  5.128356D0, 6.759774415297D-05,  1.13D-23, 2.569423, 1, & ! 18
     &   'CHI1',  1.566075D0, 7.063515997561D-05,  4.45D-23, 0.038648, 1, & ! 19
     &   'PI1 ',  3.002044D0, 7.232384890619D-05, -5.24D-25, 0.069785, 1, & ! 20
     &   'P1  ',  2.958919D0, 7.252294578148D-05, -1.06D-24, 1.193312, 1, & ! 21
     &   'S1  ',  1.570796D0, 7.272205216643D-05,  0.0,      0.0,      1, & ! 22
     &   'K1- ',  2.365113D0, 7.291046158901D-05,  8.34D-24, 0.071534, 1, & ! 23
     &   'K1  ',  3.324267D0, 7.292115855138D-05,  1.06D-24, 3.612086, 1, & ! 24
     &   'K1+ ',  1.141827D0, 7.293185551375D-05, -6.21D-24, 0.490128, 1, & ! 25
     &   'PSI1',  3.281141D0, 7.312025542667D-05,  5.24D-25, 0.028656, 1, & ! 26
     &   'THE1',  5.082459D0, 7.520715712715D-05, -4.24D-23, 0.038636, 1, & ! 27
     &   'J1  ',  5.679822D0, 7.556036160412D-05,  3.21D-23, 0.202043, 1, & ! 28
     &   'J1+ ',  3.497383D0, 7.557105856649D-05,  2.48D-23, 0.040048, 1, & ! 29
     &   'EPS2',  1.983319D0, 1.329544980231D-04, -6.23D-24, 0.045770, 2, & ! 30
     &   '2N2 ',  3.741511D0, 1.352404965989D-04, -4.97D-23, 0.156950, 2, & ! 31
     &   'MU2 ',  4.338875D0, 1.355937010758D-04,  2.48D-23, 0.189426, 2, & ! 32
     &   'N2- ',  5.137914D0, 1.378690026892D-04, -1.14D-23, 0.044279, 2, & ! 33
     &   'N2  ',  6.097067D0, 1.378796996516D-04, -1.86D-23, 1.186047, 2, & ! 34
     &   'NU2 ',  0.411245D0, 1.382329041286D-04,  5.58D-23, 0.225298, 2, & ! 35
     &   'M2- ',  1.210284D0, 1.405082057420D-04,  1.97D-23, 0.231130, 2, & ! 36
     &   'M2  ',  2.169437D0, 1.405189027044D-04,  1.24D-23, 6.194554, 2, & ! 37
     &   'LA2 ',  0.786037D0, 1.428049012801D-04, -3.10D-23, 0.045678, 2, & ! 38
     &   'L2  ',  1.383401D0, 1.431581057571D-04,  4.34D-23, 0.175107, 2, & ! 39
     &   '2T2 ',  0.086250D0, 1.450459105823D-04,  1.08D-24, 0.006831, 2, & ! 40
     &   'T2  ',  0.043125D0, 1.452450074576D-04,  5.39D-25, 0.168236, 2, & ! 41
     &   'S2  ',  6.283185D0, 1.454441043329D-04,  0.0D0,    2.881748, 2, & ! 42
     &   'R2  ',  3.098467D0, 1.456432012082D-04, -5.39D-25, 0.024055, 2, & ! 43
     &   'K2  ',  3.506941D0, 1.458423171028D-04,  2.13D-24, 0.783028, 2, & ! 44
     &   'K2+ ',  1.324501D0, 1.458530140651D-04, -5.15D-24, 0.233327, 2, & ! 45
     &   'ETA2',  5.862496D0, 1.484815201555D-04,  3.32D-23, 0.043801, 2, & ! 46
     &   'U3  ',  0.0D0,      2.177679627494D-04,  0.0D0,    0.0,      3, & ! 47
     &   'T3  ',  0.0D0,      2.179670596247D-04,  0.0D0,    0.0,      3, & ! 48
     &   'S3  ',  0.0D0,      2.181661850283D-04,  0.0D0,    0.0,      3, & ! 49
     &   'R3  ',  0.0D0,      2.183652533752D-04,  0.0D0,    0.0,      3, & ! 50
     &   'K3  ',  0.0D0,      2.185643502505D-04,  0.0D0,    0.0,      3, & ! 51
     &   'M3  ',  3.138148D0, 2.108008582030D-04, -1.76D-23, 0.000000, 3, & ! 52
     &   'M4  ',  4.338874D0, 2.810378054088D-04,  2.48D-23, 0.000000, 4, & ! 53
     &   'MKS2',  5.676378D0, 1.409171154743D-04,  1.45D-23, 0.000000, 2, & ! 54
     &   'MN4 ',  1.983319D0, 2.783986023560D-04, -6.20D-24, 0.000000, 4, & ! 55
     &   'MS4 ',  2.169437D0, 2.859630070373D-04,  1.24D-23, 0.000000, 4, & ! 56
     &   'N4  ',  5.910949D0, 2.757593993032D-04, -3.72D-23, 0.000000, 4, & ! 57
     &   'U4  ',  0.0D0,      2.904900529562D-04,  0.0D0,    0.0,      4, & ! 58
     &   'T4  ',  0.0D0,      2.906891498303D-04,  0.0D0,    0.0,      4, & ! 59
     &   'S4  ',  6.283185D0, 2.908882086658D-04,  0.0D0,    0.0,      4, & ! 60
     &   'M6  ',  0.225126D0, 4.215567081132D-04,  3.72D-23, 0.000000, 6, & ! 61
     &   'M8  ',  2.394563D0, 5.620756108176D-04,  4.96D-23, 0.000000, 8, & ! 62
!
     &   'PC01',  0.0D0,      1.64486D-07,         0.0D0,    0.0,      2, & ! 63
     &   'PC02',  0.0D0,      1.68844D-07,         0.0D0,    0.0,      2, & ! 64
     &   'PAW ',  0.0D0,      1.990968752920D-07,  0.0D0,    0.0,      2  & ! 65
     &   /
!
! Per_min: 442.12  430.71 days  RMS_min= 0.027881
! Date_beg: 1980.00.01_00:00:00.0  Date_end: 2017.05.01_00:00:00.0
! Xp=  2.72074D-07 Yp=  2.93247D-16
! Xr=  1.60167D-06 Yr=  3.06977D-16
! Frq_ann=  1.99097D-07   Xac= -2.16997D-07  Xas= -3.69360D-07
! Frq_cw1=  1.64486D-07   X1c= -1.39095D-07  X1s= -7.11497D-08
! Frq_cw2=  1.68844D-07   X2c=  1.56068D-07  X2s=  6.18858D-07
!
!
       REAL*8       POL_C_PAW, POL_C_CH1, POL_C_CH2, POL_S_PAW, POL_S_CH1, POL_S_CH2
       PARAMETER  ( POL_C_PAW = -2.16997D-07 )
       PARAMETER  ( POL_C_CH1 = -1.39095D-07 )
       PARAMETER  ( POL_C_CH2 =  1.56068D-07 )
       PARAMETER  ( POL_S_PAW = -3.69360D-07 )
       PARAMETER  ( POL_S_CH1 = -7.11497D-08 )
       PARAMETER  ( POL_S_CH2 =  6.18858D-07 )
!
      REAL*8     MDC_PHAS(MALO__MFRQ), MDC_FREQ(MALO__MFRQ)
      CHARACTER  MDC_WAVE(MALO__MFRQ)*4
      INTEGER*4  NN__MALO
      DATA     ( MDC_PHAS(NN__MALO), MDC_FREQ(NN__MALO), MDC_WAVE(NN__MALO), NN__MALO=1,MALO__MFRQ ) &
     &         / &
     &           0.0D0,      0.0D0,              'CNST', & !                0  !   1
     &           0.0D0,      0.0D0,              'DRFT', & !                0  !   2
     &           0.0D0,      0.0D0,              'JMP1', & !                0  !   3
     &           0.0D0,      0.0D0,              'JMP2', & !                0  !   4
     &           0.0D0,      0.0D0,              'JMP3', & !                0  !   5
     &           0.0D0,      0.0D0,              'JMP4', & !                0  !   6
     &           0.0D0,      0.0D0,              'JMP5', & !                0  !   7
     &           0.0D0,      0.0D0,              'JMP6', & !                0  !   8
     &           0.0D0,      0.0D0,              'JMP7', & !                0  !   9
     &           0.0D0,      0.0D0,              'JMP8', & !                0  !  10
     &           3.098467D0, 1.990968752920D-07, 'SA  ', & !    194 SA     40  !  11
     &           0.365348D0, 3.982127698995D-07, 'SSA ', & !    358 SSA    14  !  12
     &           3.002044D0, 7.232384890619D-05, 'PI1 ', & !   5525 PI1    34  !  13
     &           2.958919D0, 7.252294578148D-05, 'P1  ', & !   5653 P1      6  !  14
     &           3.367392D0, 7.272206167609D-05, 'S1  ', & !     16 S1     53  !  15
     &           3.324267D0, 7.292115855138D-05, 'K1  ', & ! * 5958 K1      2  !  16
     &           3.281141D0, 7.312025542667D-05, 'PSI1', & !   6117 PSI1   52  !  17
     &           0.086250D0, 1.450459105823D-04, '2T2 ', & !   9874 2T2   114  !  18
     &           0.043125D0, 1.452450074576D-04, 'T2  ', & !   9947 T2     24  !  19
     &           0.0D0,      1.454441043329D-04, 'S2  ', & ! *10068 S2      4  !  20
     &           3.098467D0, 1.456432012082D-04, 'R2  ', & !  10196 R2     57  !  21
     &           3.506941D0, 1.458423171028D-04, 'K2  ', & ! *10303 K2      8  !  22
     &           0.0D0,      2.177679627494D-04, 'U3  ', & !  U3 = S3 - SSa    !  23
     &           0.0D0,      2.179670596247D-04, 'T3  ', & !  T3 = S3 - Sa     !  24
     &           0.0D0,      2.181661850283D-04, 'S3  ', & !  S3               !  25
     &           0.0D0,      2.183652533752D-04, 'R3  ', & !  R3 = S3 + Sa     !  26
     &           0.0D0,      2.185643502505D-04, 'K3  ', & !  K3 = S3 + SSa    !  27
     &           0.0D0,      2.904900529562D-04, 'U4  ', & !  U4 = S4 - SSa    !  28
     &           0.0D0,      2.906891498303D-04, 'T4  ', & !  T4 = S4 - Sa     !  29
     &           0.0D0,      2.908882467044D-04, 'S4  '  & !  S4               !  30
     &        /
      INTEGER*4  MALO__MTIMS
      PARAMETER  ( MALO__MTIMS = 7 )
      CHARACTER  MALO_TIMS(MALO__MTIMS)*19
      DATA       MALO_TIMS &
     &           / &
     &             '1992.01.01_00:00:00', & ! 1
     &             '2001.01.01_00:00:00', & ! 2
     &             '2006.06.01_00:00:00', & ! 3
     &             '2010.12.31_21:00:00', & ! 4
     &             '2011.01.01_00:00:00', & ! 5
     &             '2012.12.31_21:00:00', & ! 6
     &             '2017.01.03_09:00:00'  & ! 7
     &           /
!
      INTEGER*4   MALO_NFS(MALO__MFS), MALO_EFS(MALO__MFS), MALO_HFS(MALO__MFS)
      DATA      ( MALO_NFS(NN__MALO),  MALO_EFS(NN__MALO), MALO_HFS(NN__MALO), NN__MALO=1,MALO__MFS ) &
     &          / &
     &              21, 19, 15, &  ! 1  Probably, MALO_EFS(2) should be 21 
     &              41, 41, 35, &  ! 2
     &              45, 41, 35, &  ! 3
     &              44, 41, 35, &  ! 4
     &               8,  8,  8, &  ! 5
     &              26, 26, 26, &  ! 6
     &               6,  6,  4  &  ! 7
     &          /
!
      CHARACTER   MALO_WFS(MALO__MWAV,MALO__MFS)*8
      DATA      ( MALO_WFS(NN__MALO,1), NN__MALO=1,21 ) / &
     &            'PI1__cos', & !   1
     &            'PI1__sin', & !   2
     &            'P1___cos', & !   3
     &            'P1___sin', & !   4
     &            'S1___cos', & !   5
     &            'S1___sin', & !   6
     &            'K1___cos', & !   7
     &            'K1___sin', & !   8
     &            'PSI1_cos', & !   9
     &            'PSI1_sin', & !  10
     &            '2T2__cos', & !  11
     &            '2T2__sin', & !  12
     &            'T2___cos', & !  13
     &            'T2___sin', & !  14
     &            'S2___cos', & !  15
     &            'SA___cos', & !  16
     &            'SA___sin', & !  17
     &            'SSA__cos', & !  18
     &            'SSA__sin', & !  19
     &            'CNST_tin', & !  20
     &            'DRFT_tin'  & !  21
     &          /
!
      DATA      ( MALO_WFS(NN__MALO,2), NN__MALO=1,41 ) / &
     &            'PI1__cos', & !   1
     &            'PI1__sin', & !   2
     &            'P1___cos', & !   3
     &            'P1___sin', & !   4
     &            'S1___cos', & !   5
     &            'S1___sin', & !   6
     &            'K1___cos', & !   7
     &            'K1___sin', & !   8
     &            'PSI1_cos', & !   9
     &            'PSI1_sin', & !  10
     &            '2T2__cos', & !  11
     &            '2T2__sin', & !  12
     &            'T2___cos', & !  13
     &            'T2___sin', & !  14
     &            'S2___cos', & !  15
     &            'S2___sin', & !  16
     &            'R2___cos', & !  17
     &            'R2___sin', & !  18
     &            'K2___cos', & !  19
     &            'K2___sin', & !  20
     &            'U3___cos', & !  21
     &            'U3___sin', & !  22
     &            'T3___cos', & !  23
     &            'T3___sin', & !  24
     &            'S3___cos', & !  25
     &            'S3___sin', & !  26
     &            'R3___cos', & !  27
     &            'R3___sin', & !  28
     &            'K3___cos', & !  29
     &            'K3___sin', & !  30
     &            'U4___cos', & !  31
     &            'U4___sin', & !  32
     &            'T4___cos', & !  33
     &            'T4___sin', & !  34
     &            'S4___cos', & !  35
     &            'SA___cos', & !  36
     &            'SA___sin', & !  37
     &            'SSA__cos', & !  38
     &            'SSA__sin', & !  39
     &            'CNST_tin', & !  40
     &            'DRFT_tin'  & !  41
     &          /
!
      DATA      ( MALO_WFS(NN__MALO,3), NN__MALO=1,45 ) / &
     &            'PI1__cos', & !   1
     &            'PI1__sin', & !   2
     &            'P1___cos', & !   3
     &            'P1___sin', & !   4
     &            'S1___cos', & !   5
     &            'S1___sin', & !   6
     &            'K1___cos', & !   7
     &            'K1___sin', & !   8
     &            'PSI1_cos', & !   9
     &            'PSI1_sin', & !  10
     &            '2T2__cos', & !  11
     &            '2T2__sin', & !  12
     &            'T2___cos', & !  13
     &            'T2___sin', & !  14
     &            'S2___cos', & !  15
     &            'S2___sin', & !  16
     &            'R2___cos', & !  17
     &            'R2___sin', & !  18
     &            'K2___cos', & !  19
     &            'K2___sin', & !  20
     &            'U3___cos', & !  21
     &            'U3___sin', & !  22
     &            'T3___cos', & !  23
     &            'T3___sin', & !  24
     &            'S3___cos', & !  25
     &            'S3___sin', & !  26
     &            'R3___cos', & !  27
     &            'R3___sin', & !  28
     &            'K3___cos', & !  29
     &            'K3___sin', & !  30
     &            'U4___cos', & !  31
     &            'U4___sin', & !  32
     &            'T4___cos', & !  33
     &            'T4___sin', & !  34
     &            'S4___cos', & !  35
     &            'SA___cos', & !  36
     &            'SA___sin', & !  37
     &            'SSA__cos', & !  38
     &            'SSA__sin', & !  39
     &            'CNST_tin', & !  40
     &            'DRFT_tin', & !  41
     &            'JMP1_t03', & !  42 2006.06.01_00:00:00
     &            'JMP2_t04', & !  43 2010.12.31_21:00:00
     &            'JMP3_t06', & !  44 2012.12.31_21:00:00
     &            'JMP4_t07'  & !  45 2017.01.03_09:00:00
     &          /
!
      DATA      ( MALO_WFS(NN__MALO,4), NN__MALO=1,44 ) / &
     &            'PI1__cos', & !   1
     &            'PI1__sin', & !   2
     &            'P1___cos', & !   3
     &            'P1___sin', & !   4
     &            'S1___cos', & !   5
     &            'S1___sin', & !   6
     &            'K1___cos', & !   7
     &            'K1___sin', & !   8
     &            'PSI1_cos', & !   9
     &            'PSI1_sin', & !  10
     &            '2T2__cos', & !  11
     &            '2T2__sin', & !  12
     &            'T2___cos', & !  13
     &            'T2___sin', & !  14
     &            'S2___cos', & !  15
     &            'S2___sin', & !  16
     &            'R2___cos', & !  17
     &            'R2___sin', & !  18
     &            'K2___cos', & !  19
     &            'K2___sin', & !  20
     &            'U3___cos', & !  21
     &            'U3___sin', & !  22
     &            'T3___cos', & !  23
     &            'T3___sin', & !  24
     &            'S3___cos', & !  25
     &            'S3___sin', & !  26
     &            'R3___cos', & !  27
     &            'R3___sin', & !  28
     &            'K3___cos', & !  29
     &            'K3___sin', & !  30
     &            'U4___cos', & !  31
     &            'U4___sin', & !  32
     &            'T4___cos', & !  33
     &            'T4___sin', & !  34
     &            'S4___cos', & !  35
     &            'SA___cos', & !  36
     &            'SA___sin', & !  37
     &            'SSA__cos', & !  38
     &            'SSA__sin', & !  39
     &            'CNST_tin', & !  40
     &            'DRFT_tin', & !  41
     &            'JMP1_t01', & !  42 1992.01.01_00:00:00
     &            'JMP2_t02', & !  43 2001.01.01_00:00:00
     &            'JMP3_t05'  & !  44 2011.01.01_00:00:00
     &          /
!
      DATA      ( MALO_WFS(NN__MALO,5), NN__MALO=1,8 ) / &
     &            'Pc01_cos',  &  !  1
     &            'Pc01_sin',  &  !  2
     &            'Pc02_cos',  &  !  3
     &            'Pc02_sin',  &  !  4
     &            'Paw__cos',  &  !  5
     &            'Paw__sin',  &  !  6
     &            'Nod__cos',  &  !  9
     &            'Nod__sin'   &  ! 10
     &          /
!
      DATA      ( MALO_WFS(NN__MALO,6), NN__MALO=1,26 ) / &
     &            'Pc01_cos',  &  !  1
     &            'Pc01_sin',  &  !  2
     &            'Pc02_cos',  &  !  3
     &            'Pc02_sin',  &  !  4
     &            'Paw__cos',  &  !  5
     &            'Paw__sin',  &  !  6
     &            'Mtm__cos',  &  !  7
     &            'Mtm__sin',  &  !  8
     &            'Mtm+_cos',  &  !  9
     &            'Mtm+_sin',  &  ! 10
     &            'Mf___cos',  &  ! 11
     &            'Mf___sin',  &  ! 12
     &            'Mf+__cos',  &  ! 13
     &            'Mf+__sin',  &  ! 14
     &            'Mf___cos',  &  ! 15
     &            'Mf___sin',  &  ! 16
     &            'Mm___cos',  &  ! 17
     &            'Mm___sin',  &  ! 18
     &            'Msm__cos',  &  ! 19
     &            'Msm__sin',  &  ! 20
     &            'Ssa__cos',  &  ! 21
     &            'Ssa__sin',  &  ! 22
     &            'Sa___cos',  &  ! 23
     &            'Sa___sin',  &  ! 24
     &            'Nod__cos',  &  ! 25
     &            'Nod__sin'   &  ! 26
     &          /
      DATA      ( MALO_WFS(NN__MALO,7), NN__MALO=1,6 ) / &
     &            'S1___cos', & !   1
     &            'S1___sin', & !   2
     &            'S2___cos', & !   3
     &            'S2___sin', & !   4
     &            'CNST_tin', & !   5
     &            'DRFT_tin'  & !   6
     &          /
!
      INTEGER*4    MALO__KWAV
      PARAMETER  ( MALO__KWAV = 40 )
      CHARACTER   MALO__KWS(MALO__KWAV)*8, MALO__KWN(MALO__KWAV)*8
      INTEGER*4  MALO__KCS(MALO__KWAV)
      DATA      ( MALO__KWS(NN__MALO), MALO__KWN(NN__MALO), &
     &            MALO__KCS(NN__MALO), NN__MALO=1,MALO__KWAV ) / &
!
     &            'PI1__cos', 'PI1 ', MALO__COS, &
     &            'PI1__sin', 'PI1 ', MALO__SIN, &
     &            'P1___cos', 'P1  ', MALO__COS, &
     &            'P1___sin', 'P1  ', MALO__SIN, &
     &            'S1___cos', 'S1  ', MALO__COS, &
     &            'S1___sin', 'S1  ', MALO__SIN, &
     &            'K1___cos', 'K1  ', MALO__COS, &
     &            'K1___sin', 'K1  ', MALO__SIN, &
     &            'PSI1_cos', 'PSI1', MALO__COS, &
     &            'PSI1_sin', 'PSI1', MALO__SIN, &
     &            '2T2__cos', '2T2 ', MALO__COS, &
     &            '2T2__sin', '2T2 ', MALO__SIN, &
     &            'T2___cos', 'T2  ', MALO__COS, &
     &            'T2___sin', 'T2  ', MALO__SIN, &
     &            'S2___cos', 'S2  ', MALO__COS, &
     &            'S2___sin', 'S2  ', MALO__SIN, &
     &            'R2___cos', 'R2  ', MALO__COS, &
     &            'R2___sin', 'R2  ', MALO__SIN, &
     &            'K2___cos', 'K2  ', MALO__COS, &
     &            'K2___sin', 'K2  ', MALO__SIN, &
     &            'U3___cos', 'U3  ', MALO__COS, &
     &            'U3___sin', 'U3  ', MALO__SIN, &
     &            'T3___cos', 'T3  ', MALO__COS, &
     &            'T3___sin', 'T3  ', MALO__SIN, &
     &            'S3___cos', 'S3  ', MALO__COS, &
     &            'S3___sin', 'S3  ', MALO__SIN, &
     &            'R3___cos', 'R3  ', MALO__COS, &
     &            'R3___sin', 'R3  ', MALO__SIN, &
     &            'K3___cos', 'K3  ', MALO__COS, &
     &            'K3___sin', 'K3  ', MALO__SIN, &
     &            'U4___cos', 'U4  ', MALO__COS, &
     &            'U4___sin', 'U4  ', MALO__SIN, &
     &            'T4___cos', 'T4  ', MALO__COS, &
     &            'T4___sin', 'T4  ', MALO__SIN, &
     &            'S4___cos', 'S4  ', MALO__COS, &
     &            'S4___sin', 'S4  ', MALO__SIN, &
     &            'SA___cos', 'SA  ', MALO__COS, &
     &            'SA___sin', 'SA  ', MALO__SIN, &
     &            'SSA__cos', 'SSA ', MALO__COS, &
     &            'SSA__sin', 'SSA ', MALO__SIN  &
     &          /
!
! <<<<  end of malo.i
