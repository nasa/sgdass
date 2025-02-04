! >>>>> Include block for ant (Antenna Telemetry Processing)
! >>>>> 2021.05.07  (c)  L. Petrov  v 0.10  2021.05.07_16:01:27
!
      INCLUDE    'astro_constants.i'
      INTEGER*4  ANC__MBUF, ANC__MSTR, ANC__MPRV, ANC__MPST, ANC__MTPS
      INTEGER*4  ANC__MEPC, ANC__MPCS, ANC__MGPS, ANC__MTGPS, ANC__MFIL
      PARAMETER  ( ANC__MBUF  = 1024*1024 ) !512*1024 )
      PARAMETER  ( ANC__MSTR  =  256 )
      PARAMETER  ( ANC__MPRV  =   16 )
      PARAMETER  ( ANC__MPST  =  128 )
      PARAMETER  ( ANC__MTPS  =    8*1024 )
      PARAMETER  ( ANC__MPCS  =    8*1024 )
      PARAMETER  ( ANC__MGPS  =    8*1024 )
      PARAMETER  ( ANC__MTGPS =   16 )
      PARAMETER  ( ANC__MEPC  = 1024*1024 ) !512*1024 )
      PARAMETER  ( ANC__MFIL  =    2**1024*1024*1024 ) ! 2 Tb
      INTEGER*4  ANC__R_POL, ANC__L_POL, ANC__H_POL
      INTEGER*4  ANC__V_POL, ANC__X_POL, ANC__Y_POL
      INTEGER*4  ANC__MNPOL
      PARAMETER  ( ANC__R_POL = 1 )
      PARAMETER  ( ANC__L_POL = 2 )
      PARAMETER  ( ANC__H_POL = 3 )
      PARAMETER  ( ANC__V_POL = 4 )
      PARAMETER  ( ANC__X_POL = 5 )
      PARAMETER  ( ANC__Y_POL = 6 )
      PARAMETER  ( ANC__MNPOL = 6 )
      INTEGER*4  ANC__USB, ANC__LSB, ANC__NA
      PARAMETER  ( ANC__USB =  1 )
      PARAMETER  ( ANC__LSB = -1 )
      PARAMETER  ( ANC__NA  =  0 )
      REAL*8       ANC__EPS_TIM
      PARAMETER  ( ANC__EPS_TIM = 0.01D0 )
      CHARACTER    ANC__POL(6)
      DATA         ANC__POL / 'R', 'L', 'H', 'V', 'X', 'Y' /
      INTEGER*4   ANC__MAVLS
      PARAMETER  ( ANC__MAVLS = 80 ) ! average string length
      CHARACTER  BNC__LABEL*32 / 'BNC Format version of 2021.10.31' /
      REAL*8     ANC__TSYS_MIN, ANC__TSYS_MAX
      REAL*8     ANC__FRQ_MIN, ANC__FRQ_MAX
      REAL*8     ANC__FILLER_R8, ANC__EL_MIN !@@!, ANC__TIM_DIF_MAX
      REAL*8     ANC__AMP_SCA
      REAL*4     ANC__FILLER_R4
      COMPLEX*8  ANC__PCAL_MIN, ANC__PCAL_MAX
      REAL*8     ANC__AMP_MIN, ANC__AMP_MAX
      INTEGER*4  ANC__FILLER_I4
      PARAMETER  ( ANC__TSYS_MIN =     5.D0 )            ! [K]
      PARAMETER  ( ANC__TSYS_MAX =   6000.D0 )            ! [K]
      PARAMETER  ( ANC__PCAL_MIN =   CMPLX( 0.D0 ) )      ! []
      PARAMETER  ( ANC__PCAL_MAX =   CMPLX( 100.D0 ) )    ! []
      PARAMETER  ( ANC__FRQ_MIN  =  1500.0D0 )            ! [MHz]
      PARAMETER  ( ANC__FRQ_MAX  = 18000.0D0 )            ! [MHz]
      PARAMETER  ( ANC__EL_MIN   = 4.9D0*DEG__TO__RAD )   ! [Deg]
      PARAMETER  ( ANC__FILLER_R8   =  -99.9D0 )
      PARAMETER  ( ANC__FILLER_R4   =  -99.9   )
      PARAMETER  ( ANC__FILLER_I4   = -999 )
!@@!      PARAMETER  ( ANC__TIM_DIF_MAX =   9.5D0 )           ! Duration between scans [sec]
      PARAMETER  ( ANC__AMP_SCA  =  0.0001D0 )
      PARAMETER  ( ANC__AMP_MIN  =  0.1D0*ANC__AMP_SCA  ) ! []
      PARAMETER  ( ANC__AMP_MAX  =  100.D0*ANC__AMP_SCA  ) ! []
      CHARACTER  ANC__FILLER_CH*3
      PARAMETER  ( ANC__FILLER_CH = 'uuu')
      CHARACTER  ANC__PLOTS_DIR*64
      PARAMETER  ( ANC__PLOTS_DIR = '/anc/plots')
      REAL*8     ANC__SEFD_MAX, ANC__SEFD_MIN
      PARAMETER  ( ANC__SEFD_MIN  =  0.D0  )  ! [Jy]
      PARAMETER  ( ANC__SEFD_MAX  =  1.D4  )  ! [Jy] (10kJy)
      CHARACTER  ANTCAL__FMT*44
      PARAMETER  ( ANTCAL__FMT =                                        &
     &              '# ANTCAL Format  Version  0.96 of 2021.05.17' )
							 !
! --- Provenance
!
      TYPE  ANC_PRV__TYP
            CHARACTER  BUF(ANC__MPST)*256
      END TYPE ANC_PRV__TYP
!
! --- Tsys Sensor
!
      TYPE  ANC_TPS__TYP
            CHARACTER  TAG*8
            REAL*8     IF_FRQ           ! [MHz]
            REAL*8     LO_FRQ           ! [MHz]
            REAL*8     SKY_FRQ          ! [MHz]
            REAL*8     BDW              ! [MHz]
            INTEGER*1  POL
            INTEGER*1  SUB
            CHARACTER  ID*4
            INTEGER*4  IF_IND
      END TYPE ANC_TPS__TYP
!    
! --- TSYS
!
      TYPE  ANC_TSYS__TYP
            REAL*8     TIM                                ! [sec]
            REAL*8,    POINTER :: TSYS(:) => NULL()       ! [K]
!##!            REAL*8,    POINTER :: TSYS_SCA(:,:) => NULL() ! Tsys (ave,rms) for each scan
            REAL*8     AZ                                 ! [rad]
            REAL*8     EL                                 ! [rad]
            INTEGER*4  IND_SCA
            INTEGER*4  IND_TPS
            CHARACTER  SOU_NAM*16
            CHARACTER  SCA_NAM*16
      END TYPE  ANC_TSYS__TYP

!     
! --- Data On
!
      TYPE  ANC_DOO__TYP
!No need its the DOO index!            INTEGER*4  IND_SCA     ! NH TO EDIT
            REAL*8     TIM(2)
            CHARACTER  SOU_NAM*16
            CHARACTER  SCA_NAM*16
      END   TYPE  ANC_DOO__TYP
!
! --- METEO
!
      TYPE  ANC_MET__TYP
            REAL*8     TIM              ! [s]
            REAL*8     PRES             ! [Pa]
            REAL*8     TEMP             ! [K]
            REAL*8     HUMID            ! [%]
            CHARACTER  SOU_NAM*16
            CHARACTER  SCA_NAM*16
            INTEGER*4  IND_SCA          ! NH TO EDIT 
      END   TYPE  ANC_MET__TYP
!
! --- Pcal Sensor
!      
      TYPE  ANC_PCS__TYP
            CHARACTER  TAG*8
            REAL*8     SKY_FRQ          ! [MHz]
            INTEGER*1  POL
            CHARACTER  ID*6
            INTEGER*4  IF_IND
            INTEGER*4  SUB
      END TYPE ANC_PCS__TYP
!
! --- PCAL
!
      TYPE  ANC_PCAL__TYP
            REAL*8     TIM                                   ! [sec]
            COMPLEX*8, POINTER :: PCAL_CMPL(:) => NULL()     ! Complex phase cal = |A|*(cos(phi) + isin(phi))
!##!            REAL*8,    POINTER :: PHAS_SCA(:,:) => NULL()    ! Phase (ave, rms) for each scan [rad]
!##!            REAL*8,    POINTER :: AMPL_SCA(:,:) => NULL()    ! Phase amplitude (ave, rms) for each scan []
            INTEGER*4  IND_SCA
            INTEGER*4  IND_PCS
            CHARACTER  SOU_NAM*16
            CHARACTER  SCA_NAM*16
      END TYPE  ANC_PCAL__TYP
!
! --- SEFD
!
      TYPE  ANC_SEFD__TYP
            REAL*8     TIM                              ! [sec]
            REAL*8,    POINTER :: SEFD(:) => NULL()     ! [Jy]
            REAL*8,    POINTER :: TSYS(:) => NULL()     ! [K]
            REAL*8,    POINTER :: TCAL(:) => NULL()     ! [Jy]
            REAL*8,    POINTER :: TRAT(:) => NULL()     ! []
            REAL*8,    POINTER :: GAIN(:) => NULL()     ! 
            REAL*8     AZ                               ! [rad]
            REAL*8     EL                               ! [rad]
            CHARACTER  SOU_NAM*16
      END TYPE  ANC_SEFD__TYP
!
! --- GPS Timer(s)
!
      TYPE  ANC_TGPS__TYP
            CHARACTER  TAG*8
            CHARACTER  BOARD
      END TYPE ANC_TGPS__TYP
!
! --- GPS 
!
      TYPE  ANC_GPS__TYP
            REAL*8     TIM                              ! [sec]
            REAL*8,    POINTER :: FMG(:) => NULL()      ! Formatter minus GPS [sec]
            REAL*8,    POINTER :: FMP(:) => NULL()      ! Formatter minus PPS [sec]
            INTEGER*4  IND_SCA
            INTEGER*4  IND_TGPS
            CHARACTER  SOU_NAM*16
            CHARACTER  SCA_NAM*16
      END TYPE ANC_GPS__TYP
!
! --- ANC Type
!
      TYPE ANC__TYP
           CHARACTER  ANTCAL_FMT*64
           CHARACTER  STA_NAM*8
           CHARACTER  EXP_CODE*16
           REAL*8     UTC_MTAI
           CHARACTER  FILLER_CH*8
           INTEGER*4  FILLER_I4
           REAL*8     FILLER_R8
           CHARACTER  FILIN*128
           INTEGER*4  NUM_PRV
           INTEGER*4  NUM_DOO
           INTEGER*4  NUM_MET
           INTEGER*4  NUM_TPS
           INTEGER*4  NUM_TSYS
           INTEGER*4  NUM_TPI
           INTEGER*4  NUM_PCS
           INTEGER*4  NUM_PCAL
           INTEGER*4  NUM_TGPS
           INTEGER*4  NUM_GPS
           INTEGER*4  NUM_SEFD
           INTEGER*4, POINTER :: NEP_ARR(:) => NULL()  ! Number of TSYS epochs for each TPS
           INTEGER*4  MJD_DOO
           INTEGER*4  MJD_TSYS
           INTEGER*4  MJD_MET
           INTEGER*4  MJD_PCAL
           INTEGER*4  MJD_SEFD
           INTEGER*4  MJD_GPS
           REAL*8     TAI_DOO
           REAL*8     TAI_TSYS
           REAL*8     TAI_MET
           REAL*8     TAI_PCAL
           REAL*8     TAI_SEFD
           REAL*8     TAI_GPS
           CHARACTER  TPS_TAG(ANC__MTPS)*8
           CHARACTER  PCS_TAG(ANC__MTPS)*8
           CHARACTER  TGPS_TAG(ANC__MTPS)*8
           TYPE ( ANC_PRV__TYP ),  POINTER :: PRV(:)  => NULL()
           TYPE ( ANC_DOO__TYP ),  POINTER :: DOO(:)  => NULL()
           TYPE ( ANC_MET__TYP ),  POINTER :: MET(:)  => NULL()
           TYPE ( ANC_TPS__TYP ),  POINTER :: TPS(:)  => NULL()
           TYPE ( ANC_TSYS__TYP ), POINTER :: TSYS(:) => NULL()
           TYPE ( ANC_PCS__TYP ),  POINTER :: PCS(:)  => NULL()
           TYPE ( ANC_PCAL__TYP ), POINTER :: PCAL(:) => NULL()
           TYPE ( ANC_SEFD__TYP ), POINTER :: SEFD(:) => NULL()
           TYPE ( ANC_TGPS__TYP ), POINTER :: TGPS(:) => NULL()
           TYPE ( ANC_GPS__TYP ),  POINTER :: GPS(:)  => NULL()
      END  TYPE ANC__TYP
! ---
      INTEGER*4  ATP__UNDF, ATP__LOAD, ATP__INIT
      PARAMETER  ( ATP__UNDF = 0 )
      PARAMETER  ( ATP__LOAD = 1028931 )
      PARAMETER  ( ATP__INIT = 22831221 )
    
