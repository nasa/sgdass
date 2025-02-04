!
! >>>>> Include block for package SPD ( Slant Path Delay )
! >>>>> 2007.12.10  (c)  L. Petrov  v 4.8   2024.11.23_20:53:41
! 
      CHARACTER  SPD_3D_CONF__LABEL*42, SPD__ASCII__LABEL_1ST*40, &
     &           SPD__ASCII__LABEL_2ND*40, SPD__ASCII__LABEL*40,  &
     &           AMD_INP__LABEL*32, SIT_INP__LABEL*34, SPD_3D_BIAS__LABEL*42, &
     &           SPD__DAEMON_CONF__LABEL*50
      CHARACTER  THERM_EXP_PROG__LABEL*31
      CHARACTER  SPD_3D_PROG__LABEL*32
      CHARACTER  SPD_RESP_PROG__LABEL*32
      CHARACTER  SPD_3D_BIN__LABEL*40
      CHARACTER  SPD_SHOW__LABEL*40, SOB_SHOW__LABEL*40, SPD_CLI__LABEL*128, &
     &           SPD__RESP_LABEL*60, SPD_CLIENT__LABEL*34, SPD_RES_AZEL__LABEL*50, &
     &           SPD_OPA_TATM_SPECTRUM__LABEL*53, SPD_OPA_TATM_TIPPING__LABEL*53, &
     &           SPD_RES_TS__LABEL*46, SUMMARY_BSPD_FILE*16, BSPD_SUMM__LABEL*48
!
      PARAMETER  ( SPD_3D_PROG__LABEL   = 'spd_3d  20241125   version  4.8 ' )
      PARAMETER  ( SPD_CLIENT__LABEL    = 'spd_client 20241125   version 4.8 ' )
      PARAMETER  ( SPD_RESP_PROG__LABEL = 'spd_resp  20160824  version  2.0' )
      PARAMETER  ( THERM_EXP_PROG__LABEL = 'therm_exp 20131122  version 1.1 ' )
      PARAMETER  ( SPD_3D_BIN__LABEL  = 'spd_3d_bin  1.1 version of 2015.01.05 LE' )
      PARAMETER  ( SPD__ASCII__LABEL_2ND  = 'SPD_ASCII  Format version of 2008.11.30 ' )
      PARAMETER  ( SPD__ASCII__LABEL_1ST  = 'SPD_ASCII  Format version of 2014.09.12 ' )
      PARAMETER  ( SPD__ASCII__LABEL      = 'SPD_ASCII  Format version of 2014.12.30 ' )
      PARAMETER  ( SPD_3D_CONF__LABEL = 'SPD_3D_CONF   Format version of 2024.01.11' )
      PARAMETER  ( SPD__RESP_LABEL = '# Two-point data for delay computation. Format of 2015.01.24' )
      PARAMETER  ( SPD__DAEMON_CONF__LABEL = 'SPD_3D_DAEMON_CONFIG  Format version of 2015.01.06' )
      PARAMETER  ( AMD_INP__LABEL = 'AMD-STA-INPUT Version 2002.08.20' )
      PARAMETER  ( SIT_INP__LABEL = 'SITLIST Format  Version 2003.07.31' )
      PARAMETER  ( SPD_CLI__LABEL = 'SPD_CLIENT  Format version of 2015.01.10' )
      PARAMETER  ( SPD_3D_BIAS__LABEL = 'SPD_3D_BIAS   Format version of 2010.05.18' )
      PARAMETER  ( SPD_SHOW__LABEL = '# SPD SHOW   Format v 2.0  of 2014.08.20' )
      PARAMETER  ( SOB_SHOW__LABEL = '# SOB SHOW   Format v 1.0  of 2014.09.13')
      PARAMETER  ( SPD_RES_AZEL__LABEL = '# SPD_RES_AZEL Format Version  1.00  of 2024.01.11' )
      PARAMETER  ( SPD_OPA_TATM_SPECTRUM__LABEL = '# OPA_TATM_SPECTRUM Format 1.0  version of 2024.01.06' )
      PARAMETER  ( SPD_OPA_TATM_TIPPING__LABEL  = '# OPA_TATM_TIPPING  Format 1.0  version of 2024.01.08' )
      PARAMETER  ( SPD_RES_TS__LABEL = '# SPD_RES_TS Format 1.0  version of 2024.01.09' )
      PARAMETER  ( SUMMARY_BSPD_FILE = 'bspd_summary.txt' )
      PARAMETER  ( BSPD_SUMM__LABEL  = 'BSPD Summary file.  Format version of 2024.11.24' )
!
      INTEGER*4    SPD__MDEG
      PARAMETER  ( SPD__MDEG    =    3 )
      INTEGER*4    SPD__MTYP
      PARAMETER  ( SPD__MTYP    =    2 )
      INTEGER*4    SPD__MWAV
      PARAMETER  ( SPD__MWAV    =    3 )
      INTEGER*4    SPD__MLEV
      PARAMETER  ( SPD__MLEV    =  125 )
      INTEGER*4    SPD__M_ATTR
      PARAMETER  ( SPD__M_ATTR  =   32 )
      INTEGER*4    SPD__M_LEV, SPD__M_MDIM, SPD__M_ATT, SPD__M_ATV, SPD__M_TIM, &
     &             SPD__M_FIL, SPD__M_IP, SPD__M_REQ, SPD__M_LAY, &
     &             SPD__M_RES, SPD__LBOT, SPD__LTOP
      PARAMETER  ( SPD__M_LEV   =  128 )
      PARAMETER  ( SPD__LBOT    =   12 ) ! Extra knots below the bottom
      PARAMETER  ( SPD__LTOP    =    5 ) ! Extra knots below the top
      PARAMETER  ( SPD__M_MDIM  = 8192 )
      PARAMETER  ( SPD__M_ATT   =   24 )
      PARAMETER  ( SPD__M_ATV   =  128 )
      PARAMETER  ( SPD__M_TIM   =   16 ) ! Maximal number of tim epochs
      INTEGER*4    SPD__M_STA, SPD__M_FRQ  
      PARAMETER  ( SPD__M_STA   = 1024*1024 ) ! Maximal number of stations
      PARAMETER  ( SPD__M_FRQ   =       360 ) ! Maximum number of frequencies
      PARAMETER  ( SPD__M_LAY   =        64 ) ! Maximum number of zpd layers
      PARAMETER  ( SPD__M_FIL   =  512*1024 ) ! maximum number of input files
      PARAMETER  ( SPD__M_IP    =       256 ) ! Maximum number of connections
      PARAMETER  ( SPD__M_REQ   =       256 ) ! Maximum nmbner of requests
      PARAMETER  ( SPD__M_RES   =         8 ) ! Number of SPD results
!
      INTEGER*4    SPD__STACK_SIZE_IN_GIGABYTES
      PARAMETER  ( SPD__STACK_SIZE_IN_GIGABYTES = 4 )
!
      REAL*8       SPD__U1_GMAO72, SPD__U2_GMAO72, SPD__U3_GMAO72
      PARAMETER  ( SPD__U1_GMAO72 =    20.25319 )  !  MLEV = 125
      PARAMETER  ( SPD__U2_GMAO72 =  1200.00000 )  !  MLEV = 125
      PARAMETER  ( SPD__U3_GMAO72 =  -169.30782 )  !  MLEV = 125
!@      PARAMETER  ( SPD__U1_GMAO72 =    81.5027466 )  !  MLEV = 500
!@      PARAMETER  ( SPD__U2_GMAO72 =  1200.00000   )  !  MLEV = 500
!@      PARAMETER  ( SPD__U3_GMAO72 =  -680.827454  )  !  MLEV = 500
      REAL*8     SPD__FRQ_MIN, SPD__FRQ_MAX
      PARAMETER  ( SPD__FRQ_MIN = 1.D9   )
      PARAMETER  ( SPD__FRQ_MAX = 1.2D12 )
      REAL*8     SPD__REA, SPD__MAX_ARC
      PARAMETER  ( SPD__REA     = 6378136.7D0 ) ! Earth's radius
      PARAMETER  ( SPD__MAX_ARC = 1500000.0D0 ) ! Max distance from the station to the top of 
!                                               ! of the atmosphere piercing point
      CHARACTER  SPD_RES_PARS(SPD__M_RES)*4
      DATA       SPD_RES_PARS / &
     &                          'opa ', &
     &                          'tatm', &
     &                          'delt', &
     &                          'delw', &
     &                          'deld', &
     &                          'pres', &
     &                          'pwp ', &
     &                          'temp'  &
     &                        /
!
      INTEGER*4    M__SPD_CONF, M__SPD_DAE
      PARAMETER  ( M__SPD_CONF = 15 )
      PARAMETER  ( M__SPD_DAE  =  9 )
!
      TYPE       SPD_CONF__TYPE
	  CHARACTER*20  REFR_EXPR
	  CHARACTER*10  SPD_ALG
	  CHARACTER*12  SOB_ALG
	  CHARACTER*12  COMPR
	  CHARACTER*128 FIL_STA
	  CHARACTER*128 FIL_GEOID
	  CHARACTER*128 FIL_OH
	  CHARACTER*128 FIL_LEAPSEC
	  CHARACTER*128 FIL_DESC
	  CHARACTER*128 FIL_FMT
          CHARACTER*16  BSPL_3WAV
!
	  CHARACTER*128 TITLE
	  CHARACTER*128 INSTITUTION
	  CHARACTER*128 REFERENCE
          CHARACTER*8   TEST_STR
!
          CHARACTER  DAEMON_PID_FILE*128
          CHARACTER  SERVER_PID_FILE*128
          CHARACTER  DAEMON_LOG_FILE*128
          CHARACTER  SERVER_LOG_FILE*128
          CHARACTER  SERVER_SERVER_EXE*128
          CHARACTER  RESP_DIR*128
          INTEGER*4  SERVER_PORT
          INTEGER*4  MAX_NUM_PROC
          INTEGER*4  NUM_IP
          CHARACTER  IP_ALLOW(SPD__M_IP)*16
!
          REAL*8        TIM_INTRV
          REAL*8        FRQ_ARR(SPD__M_FRQ)
          REAL*8        LAY_ARR(SPD__M_LAY)
          REAL*8        LAY_BOT
          REAL*8        LAY_STEP
	  INTEGER*4     N_EL
	  INTEGER*4     N_AZ
	  INTEGER*4     N_FRQ
	  INTEGER*4     N_LAY
      END TYPE   SPD_CONF__TYPE
!
      CHARACTER  SPD__LEAPSEC__LABEL*42
      PARAMETER  ( SPD__LEAPSEC__LABEL = '# LEAP_SECOND file  Version of 2004.01.29 ' )
      INTEGER*4    SPD__M_LPS
      PARAMETER  ( SPD__M_LPS =    64 ) ! Maximum number of entries for leap-sec
!
      TYPE SPD_LEAPSEC__TYPE
	   INTEGER*4  L_LPS
	   INTEGER*4  MJD_LPS(SPD__M_LPS)
	   REAL*8     TAI_LPS(SPD__M_LPS)
	   REAL*8     UTC_M_TAI(SPD__M_LPS)
	   INTEGER*4  STATUS
      END TYPE SPD_LEAPSEC__TYPE
!
      TYPE SPD_DSPL__TYPE
           INTEGER*4  NSTA
	   CHARACTER, POINTER :: NAME(:)*8      => NULL ()
	   REAL*8,    POINTER :: DSPL_XYZ(:,:)  => NULL ()
	   INTEGER*4  STATUS
      END TYPE SPD_DSPL__TYPE
!
      TYPE       SPD_STA__TYPE
          CHARACTER  NAME*8
!
          REAL*8     COO_CFS(3)  ! Coord. in crust fixed frame (m)
          REAL*8     LON         ! Longitude, positive to east [0,pi2]
          REAL*8     LAT_GCN     ! Latitude, pos. to north [-pi/2,pi/2]
          REAL*8     LAT_GDT     ! geodetic latitude, positive to north [-pi/2,pi/2]
          REAL*8     HEI_ELL     ! Height above ellipsoid (m)
          REAL*8     RD          ! Distance to the coordinate system origin
          REAL*8     G_ACC       ! Local gravity acceleration
          REAL*8     HEI_GEOID   ! Height of geoid wrt the reference ellipsoid (m)
!
          INTEGER*4  MJD_BEG     ! Beginning requested time interval
          REAL*8     SEC_BEG     ! for the station
!
          INTEGER*4  MJD_END     ! End of requested time interval for
          REAL*8     SEC_END     ! the station
!
	  REAL*4     SUR_PRS     ! Surface atmospheric pressure
	  REAL*4     SUR_PWP     ! Surface parital pressure of water vapor
	  REAL*4     SUR_TEM     ! Surface air temperature
          REAL*4,    POINTER :: DEL(:,:,:) => NULL() ! Path delays as a function of elevation and azimuth
          REAL*4,    POINTER :: OPA(:,:,:) => NULL() ! Atmosphere opacity as a function of frequency, elevation and azimuth
          REAL*4,    POINTER :: TAT(:,:,:) => NULL() ! Atmosphere radiative temperature as a function of frequency, elevation and azimuth
	  INTEGER*4  STATUS
      END TYPE   SPD_STA__TYPE
!
      TYPE       SPD__ASCII_NLINE
	 CHARACTER   CODE*1
	 CHARACTER   FILL_1*2  
	 CHARACTER   N_MOD*4
	 CHARACTER   FILL_2*2  
	 CHARACTER   N_INP*4
	 CHARACTER   FILL_3*2  
	 CHARACTER   N_STA*7
	 CHARACTER   FILL_4*2  
	 CHARACTER   N_EL*4
	 CHARACTER   FILL_5*2  
	 CHARACTER   N_AZ*4
	 CHARACTER   FILL_6*2  
	 CHARACTER   N_FRQ*4
      END  TYPE  SPD__ASCII_NLINE
!
      TYPE       SPD__ASCII_TEXT_LINE
	 CHARACTER   CODE*1
	 CHARACTER   FILL_1*2  
	 CHARACTER   IND_LINE*4
	 CHARACTER   FILL_2*2  
	 CHARACTER   TEXT*80
      END  TYPE  SPD__ASCII_TEXT_LINE
!
      TYPE       SPD__ASCII_ULINE
	 CHARACTER   CODE*1
	 CHARACTER   FILL_1*2  
	 CHARACTER   DEL1_CODE*8
	 CHARACTER   FILL_2*2  
	 CHARACTER   DEL2_CODE*8
      END  TYPE  SPD__ASCII_ULINE
!
      TYPE       SPD__ASCII_TLINE
	 CHARACTER   CODE*1
	 CHARACTER   FILL_1*2  
	 CHARACTER   DATE_STR*24
	 CHARACTER   FILL_2*2  
	 CHARACTER   UTC_M_TAI_STR*5
      END  TYPE  SPD__ASCII_TLINE
!
      TYPE       SPD__ASCII_SLINE
	 CHARACTER   CODE*1
	 CHARACTER   FILL_1*1
	 CHARACTER   STA_IND_STR*7
	 CHARACTER   FILL_2*2  
	 CHARACTER   STA_NAME*8
	 CHARACTER   FILL_3*2  
	 CHARACTER   X_COOR*12
	 CHARACTER   FILL_4*1  
	 CHARACTER   Y_COOR*12
	 CHARACTER   FILL_5*1
	 CHARACTER   Z_COOR*12
	 CHARACTER   FILL_6*2
	 CHARACTER   LAT_GCN*8
	 CHARACTER   FILL_7*1
	 CHARACTER   LON*8
	 CHARACTER   FILL_8*2
	 CHARACTER   HEI_ELL*6
	 CHARACTER   FILL_9*1
	 CHARACTER   HEI_GEOID*6
      END  TYPE  SPD__ASCII_SLINE
!
      TYPE       SPD__ASCII_PLINE_V2
	 CHARACTER   CODE*1
	 CHARACTER   FILL_1*1
	 CHARACTER   STA_IND*7
	 CHARACTER   FILL_2*2  
	 CHARACTER   PRES*8
	 CHARACTER   FILL_3*2  
	 CHARACTER   TEMP*5
      END  TYPE  SPD__ASCII_PLINE_V2
!
      TYPE       SPD__ASCII_PLINE
	 CHARACTER   CODE*1
	 CHARACTER   FILL_1*1
	 CHARACTER   STA_IND*7
	 CHARACTER   FILL_2*2  
	 CHARACTER   PRES*8
	 CHARACTER   FILL_3*2  
	 CHARACTER   WATER_VAPOR_PRES*8
	 CHARACTER   FILL_4*2  
	 CHARACTER   TEMP*5
      END  TYPE  SPD__ASCII_PLINE
!
      TYPE       SPD__ASCII_FRQ_LINE
	 CHARACTER   CODE*1
	 CHARACTER   FILL_1*2  
	 CHARACTER   FRQ_IND*4
	 CHARACTER   FILL_2*2  
	 CHARACTER   FRQ*15
      END  TYPE  SPD__ASCII_FRQ_LINE
!
      TYPE       SPD__ASCII_ANG_LINE
	 CHARACTER   CODE*1
	 CHARACTER   FILL_1*2  
	 CHARACTER   ANG_IND*4
	 CHARACTER   FILL_2*2  
	 CHARACTER   ANG*10
      END  TYPE  SPD__ASCII_ANG_LINE
!
      TYPE       SPD__ASCII_DEL_LINE
	 CHARACTER   CODE*1
	 CHARACTER   FILL_1*1
	 CHARACTER   STA_IND*7
	 CHARACTER   FILL_2*2  
	 CHARACTER   EL_IND*4
	 CHARACTER   FILL_3*2  
	 CHARACTER   AZ_IND*4
	 CHARACTER   FILL_4*2  
	 CHARACTER   DEL1*12
	 CHARACTER   FILL_5*2  
	 CHARACTER   DEL2*12
      END  TYPE  SPD__ASCII_DEL_LINE
!
      TYPE       SPD__ASCII_OPA_LINE
	 CHARACTER   CODE*1
	 CHARACTER   FILL_1*1
	 CHARACTER   STA_IND*7
	 CHARACTER   FILL_2*2  
	 CHARACTER   EL_IND*4
	 CHARACTER   FILL_3*2  
	 CHARACTER   AZ_IND*4
	 CHARACTER   FILL_4*2  
	 CHARACTER   FRQ_IND*4
	 CHARACTER   FILL_5*2  
	 CHARACTER   OPA*6
	 CHARACTER   FILL_6*2  
	 CHARACTER   TAT*6
      END  TYPE  SPD__ASCII_OPA_LINE
!
      TYPE       SPD__ASCII__TYPE
	 INTEGER*4  NM  ! Number of model lines
	 INTEGER*4  NI
	 INTEGER*4  NS  ! Number of stations
	 INTEGER*4  NE  ! Number of elevations
	 INTEGER*4  NA  ! Number of azimuths for delay
	 INTEGER*4  NFA ! Number of azimuths for frequencies
	 INTEGER*4  NF  ! Number of frequencies
	 CHARACTER  LABEL*80
	 TYPE ( SPD__ASCII_NLINE)               :: NLINE
	 TYPE ( SPD__ASCII_TEXT_LINE ), POINTER :: MLINE(:)       => NULL ()
	 TYPE ( SPD__ASCII_TEXT_LINE ), POINTER :: ILINE(:)       => NULL ()
	 TYPE ( SPD__ASCII_ULINE )              :: ULINE
	 TYPE ( SPD__ASCII_SLINE ),     POINTER :: SLINE(:)       => NULL ()
	 TYPE ( SPD__ASCII_ANG_LINE ),  POINTER :: ELINE(:)       => NULL ()
	 TYPE ( SPD__ASCII_ANG_LINE ),  POINTER :: ALINE(:)       => NULL ()
	 TYPE ( SPD__ASCII_FRQ_LINE ),  POINTER :: FLINE(:)       => NULL ()
	 TYPE ( SPD__ASCII_TLINE )                 TLINE
	 TYPE ( SPD__ASCII_PLINE ),     POINTER :: PLINE(:)       => NULL ()
	 TYPE ( SPD__ASCII_DEL_LINE ),  POINTER :: DLINE(:,:,:)   => NULL ()
	 TYPE ( SPD__ASCII_OPA_LINE ),  POINTER :: OLINE(:,:,:,:) => NULL ()
	 CHARACTER  FILE*256
	 CHARACTER  PROC_O_MODE*4
	 INTEGER*4  O_MODE
	 INTEGER*4  STATUS
      END TYPE   SPD__ASCII__TYPE
!
! --- Introductory record of a datafile with slant path delay
! --- in binary format
!
      TYPE SPD_3D_LAB__TYPE
           CHARACTER  PREF*8      ! record ID:  'LAB_REC '
           INTEGER*8  LEN         ! record length in bytes
           CHARACTER  FMT_LAB*40  ! format name ( SPD_3D_BIN__LABEL )
!
           INTEGER*8  OFF_TIM     ! Offset of TIM record in bytes with respect to the first byte of the file
           INTEGER*8  OFF_STA     ! Offset of STA record in bytes with respect to the first byte of the file
           INTEGER*8  OFF_MOD     ! Offset of MOD record in bytes with respect to the first byte of the file
           INTEGER*8  OFF_MET     ! Offset of MET record in bytes with respect to the first byte of the file
           INTEGER*8  OFF_ELV     ! Offset of ELV record in bytes with respect to the first byte of the file
           INTEGER*8  OFF_AZM     ! Offset of AZM record in bytes with respect to the first byte of the file
           INTEGER*8  OFF_DEL     ! Offset of the firsst DEL record in bytes with respect to the first byte of the file
!
           INTEGER*8  LEN_TIM     ! Length if TIM record in bytes
           INTEGER*8  LEN_STA     ! Length if TIM record in bytes
           INTEGER*8  LEN_MOD     ! Length if TIM record in bytes
           INTEGER*8  LEN_MET     ! Length if TIM record in bytes
           INTEGER*8  LEN_ELV     ! Length if TIM record in bytes
           INTEGER*8  LEN_AZM     ! Length if TIM record in bytes
           INTEGER*8  LEN_DEL     ! Length if TIM record in bytes
!
	   INTEGER*4  TOT_NUM_DEL ! The total number of DEL records
      END  TYPE  SPD_3D_LAB__TYPE
!
! --- TIM record of a datafile with slant path delay
! --- in binary format
!
      TYPE SPD_3D_TIM__TYPE
           CHARACTER  PREF*8      ! record ID:  'TIM_REC '
           INTEGER*8  NREC        ! Number of epochs in one time record. Always 1
	   INTEGER*4  MJD_BEG     ! Modified Juilan date of the first epoch
	   INTEGER*4  MJD_END     ! Modified Juilan date of the last epoch
	   REAL*8     TAI_BEG     ! Time in TAI since midnight of the first epoch
	   REAL*8     TAI_END     ! Time in TAI since midnight of the last epoch
	   REAL*8     TIM_STEP    ! Time step between epochs in seconds
      END  TYPE  SPD_3D_TIM__TYPE
!
! --- STA record of a datafile with slant path delay
! --- in binary format
!
      TYPE SPD_3D_STA__TYPE
           CHARACTER  PREF*8      ! record ID:  'STA_REC '
           CHARACTER  NAME*8      ! IVS station name
	   REAL*8     COO_CFS(3)  ! Station positions in the crust-fixed coordinate system in meters
	   REAL*8     PHI_GCN     ! Station geocentic latitude in rad
	   REAL*8     PHI_GDT     ! Station geodetic  latitude in rad
	   REAL*8     LON         ! Station longitude in rad
	   REAL*8     HEI_ELL     ! Station elevation above the reference ellipsoid in meters
	   REAL*8     HEI_GEOID   ! Station elevation above the geoid in meters
      END  TYPE  SPD_3D_STA__TYPE
!
! --- MOD record of a datafile with slant path delay in binary format. 
! --- It contains description of the model used for slant path delay computation
!
      TYPE SPD_3D_MOD__TYPE
           CHARACTER  PREF*8                 ! record ID:  'MOD_REC '
	   INTEGER*4  N_RFR                  ! Number of path delay types
	   CHARACTER  SPD_TYPE(SPD__MTYP)*8  ! Names of path delay types. One of total, water, dry
           INTEGER*8  N_LINES                ! Number of lines with the model description
           INTEGER*8  LEN_TEXT               ! Lenght of model description in bytes
           CHARACTER*1, POINTER :: TEXT(:) => NULL () ! The model descrption. Consists of byte array. With the array
      END  TYPE  SPD_3D_MOD__TYPE                     ! lines are separater with a binary zero character
!
! --- MET record of a datafile with slant path delay in binary format. 
! --- It contains description of the numerical weather model used for computation
!
      TYPE SPD_3D_MET__TYPE
           CHARACTER  PREF*8                 ! record ID:  'MET_REC '
           INTEGER*8  N_LINES                ! Number of lines with the numerical model description
           INTEGER*8  LEN_TEXT               ! Lenght of model numerical description in bytes
           CHARACTER*1, POINTER :: TEXT(:) => NULL () ! The model descrption. Consists of byte array. With the array
      END  TYPE  SPD_3D_MET__TYPE                     ! lines are separater with a binary zero character
!
! --- ELV record of a datafile with slant path delay in binary format. 
! --- It defines the elevation grid
!
      TYPE SPD_3D_ELV__TYPE
           CHARACTER  PREF*8                       ! record ID:  'ELV_REC '
	   INTEGER*8  N_EL	                   ! Number of elevations
	   REAL*4,    POINTER :: ELEV(:) => NULL() ! Array of elevations above the horizont in rad. Dimension: N_EL
	   REAL*4,    POINTER :: MAP(:)  => NULL() ! Atmospheric path delay mapping function at a given 
      END  TYPE  SPD_3D_ELV__TYPE                  ! elevation according to the ISA standard atmosphere (dimensionless)
!
! --- AZM record of a datafile with slant path delay in binary format. 
! --- It defines the azimth grid
!
      TYPE SPD_3D_AZM__TYPE                        
           CHARACTER  PREF*8                       ! record ID:  'AZM_REC '
	   INTEGER*8  N_AZ                         ! Number of aziuths
	   REAL*4,    POINTER :: AZIM(:) => NULL() ! Array of azimuths in rad. Dimension: N_AZ
      END  TYPE  SPD_3D_AZM__TYPE
!
! --- RES record of a datafile with slant path delay in binary format. 
! --- It contains the results of computation
!
      TYPE SPD_3D_RESDEL__TYPE
           CHARACTER  PREF*8   ! record ID:  'RES_REC '
	   REAL*4     SUR_PRS  ! Surface atmospheric pressure in Pa
	   REAL*4     SUR_PWP  ! Surface partial pressure of water vapur in Pa
	   REAL*4     SUR_TEM  ! Surface atmospheric temperature in K
	   REAL*4,    POINTER :: DEL(:,:,:) => NULL() ! Slant path delay in sec. Axes: elevation, azimuth, delay types
      END  TYPE  SPD_3D_RESDEL__TYPE
!
      TYPE SPD_3D_RES__TYPE
           CHARACTER  PREF*8   ! record ID:  'RES_REC '
           INTEGER*4  NF       ! ??
	   REAL*4     SUR_PRS  ! Surface atmospheric pressure in Pa
	   REAL*4     SUR_PWP  ! Surface partial pressure of water vapur in Pa
	   REAL*4     SUR_TEM  ! Surface atmospheric temperature
	   REAL*4,    POINTER :: DEL(:,:,:) => NULL() ! Slant path delay in sec. Axes: elevation, azimuth, delay types
	   REAL*4,    POINTER :: OPA(:,:,:) => NULL() ! Dimensionless atmospheric opacity. Axes: frequency, elevation, azimuth
	   REAL*4,    POINTER :: TAT(:,:,:) => NULL() ! Atmosperic brightness tempeature in K.  Axes: frequency, elevation, azimuth
           INTEGER*4  STATUS_DEL
           INTEGER*4  STATUS_OPA_TAT
      END  TYPE  SPD_3D_RES__TYPE
!
      TYPE SPD_3D_MF__TYPE
           CHARACTER  MF_NAME*16
           REAL*8     PAR1
           REAL*8     PAR2
           REAL*8,    POINTER :: EL_ARG(:) => NULL()
           REAL*8,    POINTER :: MF_SPL(:) => NULL()
           REAL*8,    POINTER :: MF_ARG(:) => NULL()
           REAL*8,    POINTER :: EL_SPL(:) => NULL()
           INTEGER*4  L_NOD
           INTEGER*4  STATUS
      END  TYPE  SPD_3D_MF__TYPE
!
      TYPE      SPD_3D__TYPE
	  INTEGER*4  NSTA
          INTEGER*4  NFRQ
          INTEGER*4  NTYP
          INTEGER*4  NLEV
          INTEGER*4  NLON
          INTEGER*4  NLAT
          REAL*8     CNS_DR2_SIG(2)
!
	  REAL*8,    POINTER :: LON(:)          => NULL()
	  REAL*8,    POINTER :: LAT(:)          => NULL()
	  REAL*8,    POINTER :: LEV(:)          => NULL()
	  REAL*8,    POINTER :: FRQ(:)          => NULL()
	  REAL*8,    POINTER :: REF_3D(:,:,:,:) => NULL()
          REAL*8,    POINTER :: SPR_3D(:,:,:)   => NULL()
	  REAL*8,    POINTER :: SPW_3D(:,:,:)   => NULL()
	  REAL*8,    POINTER :: STM_3D(:,:,:)   => NULL()
          LOGICAL*1, POINTER :: MASK(:,:)       => NULL()
!
	  TYPE     ( SPD_CONF__TYPE )          :: CONF
	  TYPE     ( SPD_STA__TYPE  ), POINTER :: STA(:)   => NULL()
	  TYPE     ( SPD_LEAPSEC__TYPE )       :: LEAPSEC
          TYPE     ( SPD_3D_ELV__TYPE  ) :: ELV
          TYPE     ( SPD_3D_AZM__TYPE  ) :: AZM
          TYPE     ( SPD_DSPL__TYPE    ) :: DSPL
          REAL*8     TAI
          REAL*8     UTC
          REAL*8     UTC_M_TAI
          INTEGER*4  MJD
	  INTEGER*4  UTC_M_TAI_STATUS
	  CHARACTER  FILSPD*128
          CHARACTER  NWP_TITLE*128
          CHARACTER  NWP_INSTITUTION*128
          CHARACTER  NWP_PROD_NAME*128
          CHARACTER  NWP_REFERENCES*128
	  INTEGER*4  CONF_STATUS
          INTEGER*4  REF_3D_STATUS
      END TYPE  SPD_3D__TYPE 
!
      TYPE      SPD_4D__TYPE
          INTEGER*4  DIMS(5)
          INTEGER*4  MJD_0
          REAL*8     TAI_0
	  REAL*4,    POINTER :: LEV(:)          => NULL()
	  REAL*4,    POINTER :: LON(:)          => NULL()
	  REAL*4,    POINTER :: LAT(:)          => NULL()
	  REAL*4,    POINTER :: TIM(:)          => NULL()
	  REAL*4,    POINTER :: REFR(:,:,:,:,:) => NULL()
          INTEGER*4  STATUS
      END TYPE  SPD_4D__TYPE
!
      CHARACTER    SPD__RADIO_DAVIS_1985*16,  &
     &             SPD__RADIO_RUEGER_2002*17, &
     &             SPD__RADIO_APARICIO_2011*19, &
     &             SPD__W532_CIDDOR_1996*16,  &
     &             SPD__W1064_CIDDOR_1996*17
      PARAMETER  ( SPD__RADIO_DAVIS_1985    = 'Radio_Davis_1985' )      
      PARAMETER  ( SPD__RADIO_RUEGER_2002   = 'Radio_Rueger_2002' )      
      PARAMETER  ( SPD__RADIO_APARICIO_2011 = 'Radio_Aparicio_2011' )
      PARAMETER  ( SPD__W532_CIDDOR_1996    = 'W532_Ciddor_1996' )
      PARAMETER  ( SPD__W1064_CIDDOR_1996   = 'W1064_Ciddor_1996' )
!
      CHARACTER    SPD__COMPR_NO*8,    &
     &             SPD__COMPR_DRY*8,   &
     &             SPD__COMPR_WET*8,   &
     &             SPD__COMPR_TOTAL*8, &
     &             SPD__THERMDEF*8
      PARAMETER  ( SPD__COMPR_NO    = 'no      ' )
      PARAMETER  ( SPD__COMPR_DRY   = 'dry     ' )
      PARAMETER  ( SPD__COMPR_WET   = 'wet     ' )
      PARAMETER  ( SPD__COMPR_TOTAL = 'total   ' )
      PARAMETER  ( SPD__THERMDEF    = 'thermdef' )
!
      CHARACTER    SPD__ALG_LIN_Y*10, SPD__ALG_LIN_YZ*10, &
     &             SPD__ALG_NONLIN_LOC*10, SPD__ALG_NONLOC*10, &
     &             SPD__ALG_Y_NONLOC*10,   SPD__ALG_STRAIGHT*10
!
      PARAMETER  ( SPD__ALG_LIN_Y      = 'lin_y     ' )
      PARAMETER  ( SPD__ALG_LIN_YZ     = 'lin_yz    ' )
      PARAMETER  ( SPD__ALG_NONLIN_LOC = 'nonlin_loc' )
      PARAMETER  ( SPD__ALG_NONLOC     = 'nonloc    ' )
      PARAMETER  ( SPD__ALG_Y_NONLOC   = 'y_nonloc  ' )
      PARAMETER  ( SPD__ALG_STRAIGHT   = 'straight  ' )
!
      CHARACTER    SOB__ALG_NONE*10,         SOB__ALG_RTE_STRA*12, &
     &             SOB__ALG_RTE_STRA_1AZ*12, SOB__ALG_RTE_BENT*12, &
     &             SOB__ALG_RTE_BENT_1AZ*12, SOB__ALG_ZPD*12,      &
     &             SOB__ALG_MZPD*12
      PARAMETER  ( SOB__ALG_NONE         = 'none        ' )
      PARAMETER  ( SOB__ALG_RTE_STRA     = 'rte_straight' )
      PARAMETER  ( SOB__ALG_RTE_STRA_1AZ = 'rte_stra_1az' )
      PARAMETER  ( SOB__ALG_RTE_BENT     = 'rte_bent    ' )
      PARAMETER  ( SOB__ALG_RTE_BENT_1AZ = 'rte_bent_1az' )
      PARAMETER  ( SOB__ALG_ZPD          = 'zpd         ' )
      PARAMETER  ( SOB__ALG_MZPD         = 'multi_zpd   ' )
!
      INTEGER*4    SPD__UNDF, SPD__ALLO, SPD__LOAD, SPD__READ, SPD__COMP, &
     &             SPD__INIT, SPD__INTR
      PARAMETER  ( SPD__UNDF =    0 )
      PARAMETER  ( SPD__ALLO = 8001 )
      PARAMETER  ( SPD__LOAD = 8002 )
      PARAMETER  ( SPD__READ = 8003 )
      PARAMETER  ( SPD__COMP = 8004 )
      PARAMETER  ( SPD__INIT = 8005 )
      PARAMETER  ( SPD__INTR = 8006 )
!
      INTEGER*4    SPD__NONE, SPD__NMFW, SPD__NMFH, SPD__TOTS, SPD__WATS, SPD__GL
      PARAMETER  ( SPD__NONE = 9001 )
      PARAMETER  ( SPD__NMFW = 9002 )
      PARAMETER  ( SPD__NMFH = 9003 )
      PARAMETER  ( SPD__TOTS = 9004 )
      PARAMETER  ( SPD__WATS = 9005 )
      PARAMETER  ( SPD__GL   = 9006 )
      CHARACTER*16  SPD__NONE_STR, SPD__NMFW_STR, SPD__NMFH_STR, &
     &              SPD__TOTS_STR, SPD__WATS_STR, SPD__GL_STR
      PARAMETER  ( SPD__NONE_STR = 'NONE' )
      PARAMETER  ( SPD__NMFW_STR = 'NMFW' )
      PARAMETER  ( SPD__NMFH_STR = 'NMFH' )
      PARAMETER  ( SPD__TOTS_STR = 'TOTAL_SCALE' )
      PARAMETER  ( SPD__WATS_STR = 'WATER_SCALE' )
      PARAMETER  ( SPD__GL_STR   = 'GAUSSIAN_LAYER' )
!
      INTEGER*4  SPD__TYP_HDF4, SPD__TYP_GRDAS, SPD__TYP_NETCDF
      PARAMETER  ( SPD__TYP_HDF4   = 13001 )	
      PARAMETER  ( SPD__TYP_GRDAS  = 13002 )	
      PARAMETER  ( SPD__TYP_NETCDF = 13003 )	
      INTEGER*4    SPD__DS_TAVG3D, SPD__DS_GRDAS, SPD__DS_NCEP_RA, &
     &             SPD__DS_MERRA_NV, SPD__DS_MERRA_NP, SPD__DS_MERRA_CP 
      PARAMETER  ( SPD__DS_TAVG3D   = 14001 )
      PARAMETER  ( SPD__DS_GRDAS    = 14002 )
      PARAMETER  ( SPD__DS_NCEP_RA  = 14003 )
      PARAMETER  ( SPD__DS_MERRA_NV = 14004 )
      PARAMETER  ( SPD__DS_MERRA_NP = 14005 )
      PARAMETER  ( SPD__DS_MERRA_CP = 14006 )
!
      REAL*8       SPD__DEL_MIN_DEG
      PARAMETER  ( SPD__DEL_MIN_DEG = 3.0D0 ) 
!
! --- Thermodynamic constants
!
      REAL*8     SPD__R, SPD__MA, SPD__H2O, SPD__CO2, SPD__NOM_CO2, &
     &           SPD__ABS_TEMP, SPD__MD, SPD__MW, SPD__H2O_DRY_RAT
      PARAMETER  ( SPD__R        = 8.314472D0   ) ! CIPM-2007
      PARAMETER  ( SPD__MA       = 0.02896546D0 ) ! CIPM-2007
      PARAMETER  ( SPD__H2O      = 0.01801528D0 ) ! CIPM-2007
      PARAMETER  ( SPD__CO2      = 0.012011D0   ) ! CIPM-2007
      PARAMETER  ( SPD__NOM_CO2  = 0.004D0      ) ! CIPM-2007
      PARAMETER  ( SPD__MD       = SPD__MA      ) ! CIPM-2007
      PARAMETER  ( SPD__MW       = SPD__H2O     ) ! CIPM-2007
      PARAMETER  ( SPD__ABS_TEMP = 273.15D0     )
      REAL*8     SPD__ENH_ALPHA, SPD__ENH_BETA, SPD__ENH_GAMMA
      PARAMETER  ( SPD__ENH_ALPHA = 1.00062D0   )
      PARAMETER  ( SPD__ENH_BETA  = 3.14D-8     )
      PARAMETER  ( SPD__ENH_GAMMA = 5.6D-7      )
      REAL*8     SPD__PSV_A, SPD__PSV_B, SPD__PSV_C, SPD__PSV_D
      PARAMETER  ( SPD__PSV_A =  1.2378847D-5   ) ! CIPM-2007
      PARAMETER  ( SPD__PSV_B = -1.9121316D-2   ) ! CIPM-2007
      PARAMETER  ( SPD__PSV_C = 33.93711047D0   ) ! CIPM-2007
      PARAMETER  ( SPD__PSV_D = -6.3431645D3    ) ! CIPM-2007
      REAL*8     SPD__COMP_A0, SPD__COMP_A1, SPD__COMP_A2, SPD__COMP_B0, SPD__COMP_B1, &
     &           SPD__COMP_C0, SPD__COMP_C1, SPD__COMP_D0, SPD__COMP_E0
      PARAMETER  ( SPD__COMP_A0 =  1.58123D-6   ) ! CIPM-2007
      PARAMETER  ( SPD__COMP_A1 = -2.9331D-8    ) ! CIPM-2007
      PARAMETER  ( SPD__COMP_A2 =  1.1043D-10   ) ! CIPM-2007
      PARAMETER  ( SPD__COMP_B0 =  5.707D-6     ) ! CIPM-2007
      PARAMETER  ( SPD__COMP_B1 = -2.051D-8     ) ! CIPM-2007
      PARAMETER  ( SPD__COMP_C0 =  1.9898D-4    ) ! CIPM-2007
      PARAMETER  ( SPD__COMP_C1 = -2.376D-6     ) ! CIPM-2007
      PARAMETER  ( SPD__COMP_D0 =  1.83D-11     ) ! CIPM-2007
      PARAMETER  ( SPD__COMP_E0 = -0.765D-8     ) ! CIPM-2007
      REAL*8       SPD__CO2_CONC
      PARAMETER  ( SPD__CO2_CONC = 0.00375D0 ) 
      PARAMETER  ( SPD__H2O_DRY_RAT = SPD__H2O/(SPD__MA + SPD__CO2*(SPD__CO2_CONC - SPD__NOM_CO2)) )
!
      REAL*8     SPD__REF_D0, SPD__REF_D1, SPD__REF_D2, SPD__REF_D3
      REAL*8     SPD__REF_W0, SPD__REF_W1, SPD__REF_W2, SPD__REF_W3, SPD__CF
      PARAMETER  ( SPD__REF_D0 =  2.380185D+14 ) ! d/l
      PARAMETER  ( SPD__REF_D1 =  5.792105D+10 ) ! 1/m^2
      PARAMETER  ( SPD__REF_D2 =  5.7362D+13   ) ! 1/m^2
      PARAMETER  ( SPD__REF_D3 =  1.67917D+09  ) ! 1/m^2
      PARAMETER  ( SPD__CF    =   1.022D0      ) ! 1/m^2
!
      PARAMETER  ( SPD__REF_W0 =  2.95235D-6   ) ! d/l
      PARAMETER  ( SPD__REF_W1 =  2.6422D-20   ) ! 1/m^2
      PARAMETER  ( SPD__REF_W2 = -3.2380D-34   ) ! 1/m^4
      PARAMETER  ( SPD__REF_W3 =  4.028D-47    ) ! 1/m^6
      REAL*8     PD__REF, PW__REF, TEM__D_REF, TEM__W_REF
      PARAMETER  ( PD__REF     = 101325.0D0  )
      PARAMETER  ( PW__REF     =   1333.0D0  )
      PARAMETER  ( TEM__D_REF  =    288.15D0 )
      PARAMETER  ( TEM__W_REF  =    293.15D0 )
!
! --- Parameters of formulae for group refractivity in optical in near infra-red
! --- According to Ciddor 1996
!
      REAL*8       SPD__CIDDOR_W532_ST,  SPD__CIDDOR_W532_SW,  &
     &             SPD__CIDDOR_W1064_ST, SPD__CIDDOR_W1064_SW
      PARAMETER  ( SPD__CIDDOR_W532_ST  =  8.182230D-7 )
      PARAMETER  ( SPD__CIDDOR_W532_SW  = -9.733136D-8 )
      PARAMETER  ( SPD__CIDDOR_W1064_ST =  7.814736D-7 )
      PARAMETER  ( SPD__CIDDOR_W1064_SW = -1.060413D-7 )
!
! --- Refractivity constants
!
! --- Jean Rueger, "Refractivity Index Formulae for Radio Waves",
! --- JS28 Interaction of Techniques and Corrections to Achieve
! --- Accurate Engeneering, FIGS XXII International congress, 
! --- Washington D.C. April 19-26, 2002
!
      REAL*8       SPD__RUEGER_K1R, SPD__RUEGER_K2R, SPD__RUEGER_K3R
      PARAMETER  ( SPD__RUEGER_K1R = 7.76890D-7  ) ! K/Pa
      PARAMETER  ( SPD__RUEGER_K2R = 7.129520D-7 ) ! K/Pa
      PARAMETER  ( SPD__RUEGER_K3R = 3.754630D-3 ) ! K**2/Pa
!
! --- Josep Aparicio and Stephane Laroche, Rueger, "An evaulation of the expression 
! --- of the atmospheric refractivity for GPS signal", JGR, 116, D11104, 2011
!
      REAL*8       SPD__APARICIO_N1D, SPD__APARICIO_N2D, &
     &             SPD__APARICIO_N1W, SPD__APARICIO_N2W
      PARAMETER  ( SPD__APARICIO_N1D = 2.226122D-04 ) ! m^3/kg       2.22682D-4  - 6.98D-8
      PARAMETER  ( SPD__APARICIO_N2D = 1.906587D-05 ) ! m^3/(kg*K)                 6.98D-8*273.15D0
      PARAMETER  ( SPD__APARICIO_N1W = 3.157190D-04 ) ! m^3/kg       6.701605D-3 - 6.385886D-3
      PARAMETER  ( SPD__APARICIO_N2W = 1.744305D+00 ) ! m^3/(kg*K)   6.385886D-3*273.15D0
!
      REAL*8       SPD__OPA_MIN
      PARAMETER  ( SPD__OPA_MIN = 1.D-9 ) ! Minimal atmosphere opacity (optical depth)
!
      REAL*8       SPD__C
      PARAMETER  ( SPD__C = 299792458.0D0 )
      REAL*8     SPD__REA_WGS84, SPD__FLAT_WGS84, SPD__GM_EGM96, SPD__OMEGA_EGM96, &
     &           SPD__ACC_EQU_WGS84, SPD__GRV_LAT_WGS84, SPD__W0_IAU2004
      PARAMETER  ( SPD__REA_WGS84     = 6378137.0D0 )
      PARAMETER  ( SPD__FLAT_WGS84    = 1.0D0/298.2572235630D0 )
      PARAMETER  ( SPD__GM_EGM96      = 3.986004418D14 )
      PARAMETER  ( SPD__OMEGA_EGM96   = 7.292115D-5 )
      PARAMETER  ( SPD__ACC_EQU_WGS84 = 9.7803253359D0     ) ! Equatorial gravity acc.
      PARAMETER  ( SPD__GRV_LAT_WGS84 = 0.00193185265241D0 ) ! D(ACC_EQU)/D(phi)
      PARAMETER  ( SPD__W0_IAU2004    = 62636856.0D0       ) ! m^2/s^2 Geopotential value defined by
!                                                          ! M. Bursa etal, The geopotential value 
!                                                          ! W0 for specifying the relativistic atomic 
!                                                          ! time scale and a global vertical reference system, 
!                                                          ! JoG, 81(2)2, pp 103-110, 2007
! ----- GMAO constants
!
      REAL*8       SPD__R_MAPL, SPD__MA_MAPL, SPD__H2O_MAPL, SPD__ACCREF_MAPL, SPD__KAPPA_MAPL
      PARAMETER  ( SPD__R_MAPL      = 8.3143D0 )    ! Universal gas constant
      PARAMETER  ( SPD__MA_MAPL     = 28.97D-3 )    ! Molar mass of dry air
      PARAMETER  ( SPD__H2O_MAPL    = 18.01D-3 )    ! Molar mass of wet air
      PARAMETER  ( SPD__ACCREF_MAPL = 9.80D0   )    ! Reference gravity acceleration
      PARAMETER  ( SPD__KAPPA_MAPL  = 2.0D0/7.0D0 ) ! Adiabatic constant
      REAL*8       SPD__GP0, SPD__GP1     ! Dependence of gravity acceleration on Pressure according to ISO atmosphere
      PARAMETER  ( SPD__GP0 = -2.427388D-02 ) ! g(P) = G_ell*(1.0D + GP0 + GP1*ln(P))
      PARAMETER  ( SPD__GP1 =  2.088507D-03 )
!
      INTEGER*4  SPD__WRITE_ASC, SPD__WRITE_BIN
      PARAMETER  ( SPD__WRITE_ASC = 17001 )
      PARAMETER  ( SPD__WRITE_BIN = 17002 )
!
      TYPE SPD_2P__TYPE
           REAL*8     TAI
           REAL*8     COO_EMI(3)
           REAL*8     COO_REC(3)
           REAL*8     DEL(3)
           REAL*8     DEL_RDER(3)
           REAL*8     DEL_EDER(3)
           INTEGER*4  MJD
           INTEGER*4  FILLER_1
      END TYPE SPD_2P__TYPE
!
      TYPE SPD_DEL__TYPE
           TYPE ( SPD_3D_LAB__TYPE ) :: LAB
           TYPE ( SPD_3D_TIM__TYPE ) :: TIM
           TYPE ( SPD_3D_STA__TYPE ) :: STA
           TYPE ( SPD_3D_MOD__TYPE ) :: MOD
           TYPE ( SPD_3D_MET__TYPE ) :: MET
           TYPE ( SPD_3D_ELV__TYPE ) :: ELV
           TYPE ( SPD_3D_AZM__TYPE ) :: AZM
           TYPE ( SPD_3D_MF__TYPE  ) :: MF
           TYPE ( SPD_3D_RES__TYPE ),   POINTER :: RES(:) => NULL ()
	   INTEGER*4  NDEL
	   INTEGER*4  IND_FIRST_EPO
!
           REAL*4,    POINTER :: SUR_PRS(:)    => NULL ()
           REAL*4,    POINTER :: SUR_PWP(:)    => NULL ()
           REAL*4,    POINTER :: SUR_TEM(:)    => NULL ()
           REAL*4,    POINTER :: DELS(:,:,:,:) => NULL ()
           REAL*4,    POINTER :: OPA(:,:,:,:)  => NULL ()
           REAL*4,    POINTER :: TAT(:,:,:,:)  => NULL ()
           REAL*4,    POINTER :: MAP_ARR(:)    => NULL ()
           REAL*4,    POINTER :: TIM_ARR(:)    => NULL ()
           REAL*4,    POINTER :: FRQ_ARR(:)    => NULL ()
           REAL*4,    POINTER :: ZEN_DEL(:,:)  => NULL ()
           REAL*8     ZEN_BIAS
           REAL*8     ZEN_SCALE
!
           INTEGER*4  N_FRQ
	   INTEGER*4  N_TIM
	   REAL*8     TIM_BEG
!
           INTEGER*4  MODE_OPA_TAT
           INTEGER*4  MJD_OBS_FIRST
           REAL*8     TAI_OBS_FIRST
	   INTEGER*4  STATUS
      END TYPE SPD_DEL__TYPE
!
      TYPE SPD_STAT_MOD__TYPE
           REAL*4  MEAN
           REAL*4  AMP(2,SPD__M_FRQ)
      END  TYPE SPD_STAT_MOD__TYPE
!
      TYPE SPD_STAT__TYPE
	   INTEGER*4  L_STA
	   CHARACTER  STA_NAM(SPD__M_STA)*8
	   INTEGER*4  L_FRQ
           REAL*4     FRQ(SPD__M_FRQ)
	   TYPE     ( SPD_STAT_MOD__TYPE ), POINTER :: MEAN(:,:)  => NULL ()
	   TYPE     ( SPD_STAT_MOD__TYPE ), POINTER :: RMS(:,:)   => NULL ()
      END  TYPE SPD_STAT__TYPE
!
      TYPE       SPD_COM__TYPE 
          CHARACTER  VERB*8
          INTEGER*4  LEN
      END TYPE   SPD_COM__TYPE 
!
      TYPE       SPD_AZEL__TYPE 
           CHARACTER  STA_NAM*8
           REAL*8     TIM
           REAL*8     AZ
           REAL*8     EL
           INTEGER*4  N_FRQ
           CHARACTER  FILLER*4
!
           REAL*8     PRES
           REAL*8     PWP
           REAL*8     TEMP
           REAL*8     DELS(2)
           REAL*8     OPA(SPD__M_FRQ)
           REAL*8     TATM(SPD__M_FRQ)
           INTEGER*4  STATUS
      END TYPE   SPD_AZEL__TYPE 
!
      TYPE       SPD_AZELS__TYPE 
           INTEGER*4  N_STA
           INTEGER*4  N_FRQ
           INTEGER*4  N_AZEL
           INTEGER*4  MJD_BEG
           INTEGER*4  MJD_END
           REAL*8     TAI_BEG
           REAL*8     TAI_END
           INTEGER*8  FREQ_I8(SPD__M_FRQ)
           CHARACTER  C_STA(SPD__M_STA)*8
           REAL*8     FREQ(SPD__M_FRQ)
           TYPE       ( SPD_AZEL__TYPE ), POINTER :: AZEL(:) => NULL ()
      END TYPE   SPD_AZELS__TYPE 
!
      TYPE       SPD_PDRQ__TYPE 
          INTEGER*4  MJD_BEG
          INTEGER*4  MJD_END
          REAL*8     TAI_BEG
          REAL*8     TAI_END
          INTEGER*4  NUM_POI
          INTEGER*4  TYP_IND(2)
      END TYPE   SPD_PDRQ__TYPE 
!
      INTEGER*4  SPD__MCLI, SPD__SIZE
      PARAMETER  ( SPD__MCLI = 7 )
      PARAMETER  ( SPD__SIZE = 4096 )
      TYPE SPD_CLI__TYPE
           CHARACTER  SERVER_NAME*32
           CHARACTER  SPD_CLI_CONF*128
           CHARACTER  SPD_PATH*128
           CHARACTER  SPD_TYPE_1*8
           CHARACTER  SPD_TYPE_2*8
           REAL*8     READ_TIMEOUT
           REAL*8     CONN_TIMEOUT
!
           INTEGER*4  SERVER_PORT
           INTEGER*4  STATUS
           INTEGER*4  SOCK_FD
           INTEGER*4  REM_FD
           CHARACTER  REM_IP_STR*16
           CHARACTER  CONF_FILE*128
!
           TYPE ( SPD_COM__TYPE ) SEND_COM, RECV_COM
      END  TYPE SPD_CLI__TYPE
      INTEGER*4    CLI__UNDF, CLI__INIT, CLI__WAIT
      PARAMETER  ( CLI__UNDF = 0 )
      PARAMETER  ( CLI__INIT = 1 )
      PARAMETER  ( CLI__WAIT = 2 )
!
      INTEGER*4  TYP__AMD, TYP__SIT
      PARAMETER  ( TYP__AMD = 1 )
      PARAMETER  ( TYP__SIT = 2 )
      REAL*8       HEIGHT_FLOOR, HEIGHT_MIN, HEIGHT_MAX
      PARAMETER  ( HEIGHT_FLOOR = -1200.0D0 ) ! floor height
      PARAMETER  ( HEIGHT_MIN   = -1000.0D0 ) ! minimal height
      PARAMETER  ( HEIGHT_MAX   =  9000.0D0 ) ! maximal height
      REAL*8       SPD__U_FLO, SPD__U_MIN, SPD__U_MAX
      PARAMETER  ( SPD__U_FLO     = -4000.0D0  )
      PARAMETER  ( SPD__U_MIN     = -1000.0D0  )
      PARAMETER  ( SPD__U_MAX     = 90000.0D0  )
!
      INTEGER*4    SPD__TOT, SPD__WAT
      PARAMETER  ( SPD__TOT = 1 )
      PARAMETER  ( SPD__WAT = 2 )
      CHARACTER    SPD__TOT_STR*8, SPD__WAT_STR*8
      PARAMETER  ( SPD__TOT_STR = 'total   ' )
      PARAMETER  ( SPD__WAT_STR = 'water   ' )
      CHARACTER    SPD__IO_LOCK_NAME*10, SPD__READ_LOCK_NAME*12, SPD__WRITE_LOCK_NAME*13
      REAL*8       SPD__LOCK_TIMEOUT
      PARAMETER  ( SPD__IO_LOCK_NAME    = 'spd_io.lck'  )
      PARAMETER  ( SPD__READ_LOCK_NAME  = 'spd_read.lck'  )
      PARAMETER  ( SPD__WRITE_LOCK_NAME = 'spd_write.lck' )
      PARAMETER  ( SPD__LOCK_TIMEOUT    = 8.0D0 )
!
      INTEGER*4    M_SPD__OVR
      PARAMETER  ( M_SPD__OVR  =  2 )
!
      INTEGER*4  SPD__NP, SPD__NW, SPD__NT
      REAL*8     SPD__P_MIN, SPD__P_MAX, SPD__PW_MIN, SPD__PW_MAX, &
     &           SPD__TEM_MIN, SPD__TEM_MAX, SPD__TEM_CMB
      PARAMETER  ( SPD__NP = 256 )
      PARAMETER  ( SPD__NW = 128 )
      PARAMETER  ( SPD__NT =  32 )
      PARAMETER  ( SPD__P_MIN    =  0.05D0     )
      PARAMETER  ( SPD__P_MAX    =  135000.0D0 )
      PARAMETER  ( SPD__PW_MIN   =  1.D-6      )
      PARAMETER  ( SPD__PW_MAX   =  6000.0D0   )
      PARAMETER  ( SPD__TEM_MIN  =  160.0D0    )
      PARAMETER  ( SPD__TEM_MAX  =  340.0D0    )
      PARAMETER  ( SPD__TEM_CMB  =  2.73D0     )
!
      CHARACTER    SPD__YES*3, SPD__NO*2
      PARAMETER  ( SPD__YES = 'YES' )
      PARAMETER  ( SPD__NO  = 'NO'  )
      INTEGER*4    SPD__W532, SPD__W1064, SPD__RADIO, SPD__MREF
      PARAMETER  ( SPD__W532  = 1 )
      PARAMETER  ( SPD__W1064 = 2 )
      PARAMETER  ( SPD__RADIO = 3 )
      CHARACTER    SPD__TYPE_W532*5, SPD__TYPE_W1064*6, SPD__TYPE_RADIO*5, SPD__TYPE_NONE*4
      PARAMETER  ( SPD__TYPE_W532  = '532nm'  )
      PARAMETER  ( SPD__TYPE_W1064 = '1064nm' )
      PARAMETER  ( SPD__TYPE_RADIO = 'radio'  )
      PARAMETER  ( SPD__TYPE_NONE  = 'none'   )
      PARAMETER  ( SPD__MREF  = 3 )
      INTEGER*4    SPD__2P, SPD__SAT
      PARAMETER  ( SPD__2P  = 1284920184 )
      PARAMETER  ( SPD__SAT = 1928492042 )
      INTEGER*4    SPD__ALARM_INT
      PARAMETER  ( SPD__ALARM_INT = 1 )
      INTEGER*4    SPD__NAZ, SPD__1AZ
      PARAMETER  ( SPD__NAZ = 782301467 )
      PARAMETER  ( SPD__1AZ = 573201453 )
      CHARACTER    SPD__NERS*4
      PARAMETER  ( SPD__NERS = 'NERS' )
!
! >>>>> End of include block for package SPD
