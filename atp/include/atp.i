! >>>>> Include block for ant (Antenna Telemetry Processing)
! >>>>> 2021.05.07  (c)  L. Petrov  v 0.95  2025.08.17_19:03:18
!
      INCLUDE    'astro_constants.i'
      INTEGER*4  ANC__MBUF, ANC__MSTR, ANC__MPRV, ANC__MPST, ANC__MTPS
      INTEGER*4  ANC__MEPC, ANC__MPCS, ANC__MGPS, ANC__MTGPS, ANC__MFIL
      INTEGER*4  ANC__MVPV, ANC__MSOU
      PARAMETER  ( ANC__MBUF  = 1024*1024 ) !512*1024 )
      PARAMETER  ( ANC__MSTR  =  256 )
      PARAMETER  ( ANC__MVPV  =    8 ) ! Maximum provenance version
      PARAMETER  ( ANC__MPRV  =  256 ) ! Max. no of provenance lines each version
      PARAMETER  ( ANC__MPST  =  128 )
      PARAMETER  ( ANC__MSOU  = 1024 ) 
      PARAMETER  ( ANC__MTPS  =    8*1024 )
      PARAMETER  ( ANC__MPCS  =    8*1024 )
      PARAMETER  ( ANC__MGPS  =    8*1024 )
      PARAMETER  ( ANC__MTGPS =   16 )
      PARAMETER  ( ANC__MEPC  = 1024*1024 )
      PARAMETER  ( ANC__MFIL  =    2*1024*1024*1024 ) ! 2 Gb
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
      PARAMETER  ( ANC__USB =  3 )
      PARAMETER  ( ANC__LSB =  2 )
      PARAMETER  ( ANC__NA  =  1 )
      REAL*8       ANC__EPS_TIM
      PARAMETER  ( ANC__EPS_TIM = 0.01D0 )
      CHARACTER    ANC__POL(6), ANC__SB(3)*6
      DATA         ANC__POL / 'R', 'L', 'H', 'V', 'X', 'Y' /
      DATA         ANC__SB / 'DUMMY', 'LSB', 'USB' /
      INTEGER*4   ANC__MAVLS
      PARAMETER  ( ANC__MAVLS = 80 ) ! average string length
      REAL*8     ANC__TSYS_MIN, ANC__TSYS_MAX
      REAL*8     ANC__FRQ_MIN, ANC__FRQ_MAX
      REAL*8     ANC__FILLER_R8, ANC__EL_MIN !@@!, ANC__TIM_DIF_MAX
      REAL*8     ANC__AMP_SCA
      REAL*4     ANC__FILLER_R4
      COMPLEX*8  ANC__PCAL_MIN, ANC__PCAL_MAX
      REAL*8     ANC__AMP_MIN, ANC__AMP_MAX
      INTEGER*4  ANC__FILLER_I4
      PARAMETER  ( ANC__TSYS_MIN =      5.D0 )            ! [K]
      PARAMETER  ( ANC__TSYS_MAX =   6000.D0 )            ! [K]
      PARAMETER  ( ANC__PCAL_MIN =   CMPLX( 0.D0 ) )      ! []
      PARAMETER  ( ANC__PCAL_MAX =   CMPLX( 100.D0 ) )    ! []
      PARAMETER  ( ANC__FRQ_MIN  =  1.5D9 )               ! [Hz]
      PARAMETER  ( ANC__FRQ_MAX  = 18.0D6 )               ! [Hz]
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
      CHARACTER  BNC__LABEL_1*32, BNC__LABEL_2*32
      PARAMETER  ( BNC__LABEL_1 = 'BNC Format version of 2021.10.31' )
      PARAMETER  ( BNC__LABEL_2 = 'BNC Format version of 2025.09.11' )
      CHARACTER  ANTCAL__FMT_1*64, ANTCAL__FMT_2*64
      CHARACTER  ANTCAL__PLOT_CNF*41
      CHARACTER  ANC_COMPUTE_TATM_OPA__LABEL*29
      PARAMETER  ( ANTCAL__FMT_1      =                                 &
     &               '# ANTCAL Format  Version  0.96 of 2021.05.17' )
      PARAMETER  ( ANTCAL__FMT_2      =                                 &
     &               '# ANTCAL Format  Version  1.0 of 2025.08.22'  )
      PARAMETER  ( ANTCAL__PLOT_CNF =                                   &
     &                'ATP_PLOT_CONFIG File Format of 2025.07.31' )
      PARAMETER  ( ANC_COMPUTE_TATM_OPA__LABEL =                        &
     &                'anc_compute_tatm_opa 20250817' )
!
! --- Configuration file for automated work
!
      TYPE  ANC_CNF__TYP
!
! --------- Plot Configuration
!
            CHARACTER  EXP_TYPE*4
            CHARACTER  INPUT_DIR*256
            CHARACTER  INPUT_FIL*256
            CHARACTER  OUTPUT_DIR*256
            REAL*8     TIM_DIF_MAX
! ------------------------------------------------ METEO
            LOGICAL*1  PLOT_ORD_TEMP
            LOGICAL*1  PLOT_ORD_PRES
            LOGICAL*1  PLOT_ORD_HUMID
!
! ------------------------------------------------ TSYS
!
            LOGICAL*1  PLOT_ORD_TSYS
            INTEGER*4  PLOT_TSYS_FRQ_IND
            REAL*8     PLOT_TSYS_FRQ_VAL ! *32
            INTEGER*4  PLOT_TSYS_TIM_IND ! *16
            INTEGER*1  PLOT_TSYS_POL
            LOGICAL*1  PLOT_TSYS_VAR_TIME
            LOGICAL*1  PLOT_TSYS_VAR_ELEV
            LOGICAL*1  PLOT_TSYS_VAR_AZIM
            LOGICAL*1  PLOT_TSYS_VAR_AZEL
            LOGICAL*1  PLOT_TSYS_VAR_SPECTR
            LOGICAL*1  PLOT_TSYS_RAW
            LOGICAL*1  PLOT_TSYS_AVE
!
! ------------------------------------------------ PHAS
!
            LOGICAL*1  PLOT_ORD_PHAS
            INTEGER*4  PLOT_PHAS_FRQ_IND
            REAL*8     PLOT_PHAS_FRQ_VAL
            INTEGER*4  PLOT_PHAS_TIM_IND
            INTEGER*1  PLOT_PHAS_POL
            LOGICAL*1  PLOT_PHAS_VAR_TIME
            LOGICAL*1  PLOT_PHAS_VAR_SPECTR
            LOGICAL*1  PLOT_PHAS_RAW
            LOGICAL*1  PLOT_PHAS_AVE
! ------------------------------------------------ AMPL
            LOGICAL*1  PLOT_ORD_AMPL
            INTEGER*4  PLOT_AMPL_FRQ_IND
            REAL*8     PLOT_AMPL_FRQ_VAL
            INTEGER*4  PLOT_AMPLPHAS_TIM_IND
            INTEGER*1  PLOT_AMPL_POL
            LOGICAL*1  PLOT_AMPL_VAR_TIME
            LOGICAL*1  PLOT_AMPL_VAR_SPECTR
            LOGICAL*1  PLOT_AMPL_RAW
            LOGICAL*1  PLOT_AMPL_AVE
! ------------------------------------------------ FMGT
            LOGICAL*1  PLOT_ORD_FMGT
            INTEGER*4  PLOT_FMGT_TAG_IND
            CHARACTER  PLOT_FMGT_TAG_VAL*32
            INTEGER*4  PLOT_FMGT_TIM_IND
            LOGICAL*1  PLOT_FMGT_VAR_TIME
            LOGICAL*1  PLOT_FMGT_RAW
            LOGICAL*1  PLOT_FMGT_AVE
! ------------------------------------------------ FMPT
            LOGICAL*1  PLOT_ORD_FMPT
            INTEGER*4  PLOT_FMPT_TAG_IND
            CHARACTER  PLOT_FMPT_TAG_VAL*32
            INTEGER*4  PLOT_FMPT_TIM_IND
            LOGICAL*1  PLOT_FMPT_VAR_TIME
            LOGICAL*1  PLOT_FMPT_RAW
            LOGICAL*1  PLOT_FMPT_AVE
! ------------------------------------------------ SEFD
            LOGICAL*1  PLOT_ORD_SEFD
            INTEGER*4  PLOT_SEFD_FRQ_IND ! *16
            REAL*8  PLOT_SEFD_FRQ_VAL    ! *32
            INTEGER*4  PLOT_SEFD_TIM_IND ! *16
            INTEGER*1  PLOT_SEFD_POL
            LOGICAL*1  PLOT_SEFD_VAR_TIME
            LOGICAL*1  PLOT_SEFD_VAR_ELEV
            LOGICAL*1  PLOT_SEFD_VAR_AZIM
            LOGICAL*1  PLOT_SEFD_VAR_AZEL
            LOGICAL*1  PLOT_SEFD_VAR_SPECTR
            LOGICAL*1  PLOT_SEFD_RAW
            LOGICAL*1  PLOT_SEFD_AVE
! ------------------------------------------------ TPI
            LOGICAL*1  PLOT_ORD_TPI
            INTEGER*4  PLOT_TPI_FRQ_IND
            REAL*8     PLOT_TPI_FRQ_VAL
            INTEGER*4  PLOT_TPI_TIM_IND
            INTEGER*1  PLOT_TPI_POL
            LOGICAL*1  PLOT_TPI_VAR_TIME
            LOGICAL*1  PLOT_TPI_VAR_ELEV
            LOGICAL*1  PLOT_TPI_VAR_AZIM
            LOGICAL*1  PLOT_TPI_VAR_AZEL
            LOGICAL*1  PLOT_TPI_VAR_SPECTR
            LOGICAL*1  PLOT_TPI_RAW
            LOGICAL*1  PLOT_TPI_AVE
      END TYPE ANC_CNF__TYP
!
! --- Provenance
!
      TYPE  ANC_PRV__TYP
            INTEGER*4  N_TEXT
            CHARACTER  TEXT(ANC__MPRV)*256
      END TYPE ANC_PRV__TYP
!
! --- Tsys Sensor
!
      TYPE  ANC_TPS__TYP
            CHARACTER  TAG*8
            REAL*8     IF_FRQ           ! [Hz]
            REAL*8     LO_FRQ           ! [Hz]
            REAL*8     SKY_FRQ          ! [Hz]
            REAL*8     BDW              ! [Hz]
            INTEGER*1  POL
            INTEGER*1  SSB
            INTEGER*1  NSB
            CHARACTER  ID*4
            INTEGER*4  IF_IND
      END TYPE ANC_TPS__TYP
!
! --- TSYS, TATM, OPACITY
!
      TYPE  ANC_TTO__TYP
            REAL*8     TIM                           ! 
            REAL*8     DUR                           ! 
            REAL*8,    POINTER :: TSYS(:) => NULL()  ! System temperture as an array of length ANC%NUM_TPS
            REAL*8,    POINTER :: TATM(:) => NULL()  ! Atmosphere brightness temperature as an array of length ANC%NUM_TPS
            REAL*8,    POINTER :: OPA(:) => NULL()   ! Atmosphere opaicuty as an array of length ANC%NUM_TPS
            REAL*8     AZ                            ! 
            REAL*8     EL                            ! 
            INTEGER*4  IND_SCA     ! scan index as defined in the ANC file
            CHARACTER  SOU_NAM*8   ! source name as defined in the ANC file
            CHARACTER  SCA_NAM*16  ! scan name as defined in the ANC file
      END TYPE  ANC_TTO__TYP
!     
! --- Data On
!
      TYPE  ANC_DOO__TYP
            REAL*8     TIM(2)
            CHARACTER  SOU_NAM*8
            CHARACTER  SCA_NAM*16
      END   TYPE  ANC_DOO__TYP
!
! --- METEO
!
      TYPE  ANC_MET__TYP
            REAL*8     TIM
            REAL*8     PRES             ! [Pa]
            REAL*8     TEMP
            REAL*8     HUMID
            CHARACTER  SOU_NAM*8
            CHARACTER  SCA_NAM*16
            INTEGER*4  IND_SCA
      END   TYPE  ANC_MET__TYP
!
! --- Pcal Sensor
!      
      TYPE  ANC_PCS__TYP
            CHARACTER  TAG*8
            REAL*8     SKY_FRQ
            INTEGER*1  POL
            CHARACTER  ID*6
            INTEGER*4  IF_IND
            INTEGER*1  SSB
            INTEGER*1  NSB
      END TYPE ANC_PCS__TYP
!
! --- PCAL (Phase Calibration)
!
      TYPE  ANC_PCAL__TYP
            REAL*8     TIM
            REAL*8     DUR      
            COMPLEX*8, POINTER :: PCAL_CMPL(:) => NULL()     ! Complex phase cal = |A|*(cos(phi) + isin(phi))
            INTEGER*4  IND_SCA

            CHARACTER  SOU_NAM*8
            CHARACTER  SCA_NAM*16
      END TYPE  ANC_PCAL__TYP
!
! --- SEFD
!
      TYPE  ANC_SEFD__TYP
            REAL*8     TIM
            REAL*8,    POINTER :: SEFD(:) => NULL()
            REAL*8,    POINTER :: TSYS(:) => NULL()
            REAL*8,    POINTER :: TCAL(:) => NULL()
            REAL*8,    POINTER :: TRAT(:) => NULL()
            REAL*8,    POINTER :: GAIN(:) => NULL()
            INTEGER*4  IND_SCA
            REAL*8     AZ
            REAL*8     EL
            CHARACTER  SOU_NAM*8
      END TYPE  ANC_SEFD__TYP
!
! --- GPS Timer(s)
!
      TYPE  ANC_TGPS__TYP
            CHARACTER  TAG*8
            CHARACTER  BOARD
      END TYPE ANC_TGPS__TYP
!
! --- GPS (Global Positioning System)
!
      TYPE  ANC_GPS__TYP
            REAL*8     TIM
            REAL*8,    POINTER :: FMG(:) => NULL()
            REAL*8,    POINTER :: FMP(:) => NULL()
            INTEGER*4  IND_SCA
            CHARACTER  SOU_NAM*8
            CHARACTER  SCA_NAM*16
      END TYPE ANC_GPS__TYP
!
! --- CABLE
!
      TYPE  ANC_CBL__TYP
            REAL*8     TIM
            REAL*8     DELAY
            CHARACTER  SOU_NAM*8
            CHARACTER  SCA_NAM*16
      END TYPE ANC_CBL__TYP
!
! --- TPI (Total Integrated Power)
!
      TYPE  ANC_TPI__TYP
            REAL*8     TIM
            REAL*8     DUR
            INTEGER*4, POINTER :: TPION(:) => NULL()
            INTEGER*4, POINTER :: TPIOFF(:) => NULL()
            INTEGER*4, POINTER :: TPIZERO(:) => NULL()
            REAL*8     AZ
            REAL*8     EL
            INTEGER*4  IND_SCA
            CHARACTER  SOU_NAM*8
            CHARACTER  SCA_NAM*16
      END TYPE  ANC_TPI__TYP
!
! --- ANC Type
!
      TYPE ANC__TYP
           INTEGER*4  FIRST_FIELD
           INTEGER*4  STATUS
           CHARACTER  ANTCAL_FMT*64
           CHARACTER  STA_NAM*8
           CHARACTER  EXP_CODE*16
           CHARACTER  EXP_TYPE*4
           REAL*8     UTC_MTAI
           CHARACTER  FILLER_CH*8
           INTEGER*4  FILLER_I4
           REAL*8     FILLER_R8
           CHARACTER  FILLER_DATE*19
           INTEGER*4  MJD_FILLER
           REAL*8     TAI_FILLER
           CHARACTER  FILIN*128
           INTEGER*4  NUM_PRV           ! The number of provenance blocks == number of versions
           INTEGER*4  NUM_PRV_LINES
           INTEGER*4  NUM_DOO
           INTEGER*4  NUM_MET
           INTEGER*4  NUM_TPS   ! The number of sensors
           INTEGER*4  NUM_TTO   ! The number of records with TTO
           INTEGER*4  NUM_TSYS  ! The number of Tsys values or zero == NUM_TTO
           INTEGER*4  NUM_TATM  ! The number of Tatm values or zero == NUM_TTO
           INTEGER*4  NUM_OPA   ! The number of opacity values or zero == NUM_TTO
           INTEGER*4  NUM_TPI   ! The number of total power integrated values or zero
           INTEGER*4  NUM_PCS
           INTEGER*4  NUM_PCAL
           INTEGER*4  NUM_TGPS
           INTEGER*4  NUM_GPS
           INTEGER*4  NUM_SEFD  ! The number of records of SEFD (SEFD < NUM_EPO*NUM_TPS)
           INTEGER*4  NUM_CBL
           INTEGER*4  NUM_EPO_TTO
           INTEGER*4  NUM_EPO_TPI
           INTEGER*4  NUM_EPO_PCAL
           INTEGER*4  NUM_EPO_GPS
           INTEGER*4  NUM_EPO_SEFD ! SEFD usually only < 10 epochs
           INTEGER*4  MJD_DOO
           INTEGER*4  MJD_TTO
           INTEGER*4  MJD_MET
           INTEGER*4  MJD_PCAL
           INTEGER*4  MJD_SEFD
           INTEGER*4  MJD_GPS
           INTEGER*4  MJD_CBL
           INTEGER*4  MJD_TPI
           REAL*8     TAI_DOO
           REAL*8     TAI_TTO
           REAL*8     TAI_MET
           REAL*8     TAI_PCAL
           REAL*8     TAI_SEFD
           REAL*8     TAI_GPS
           REAL*8     TAI_CBL
           REAL*8     TAI_TPI
           CHARACTER  TPS_TAG(ANC__MTPS)*8
           CHARACTER  PCS_TAG(ANC__MTPS)*8
           CHARACTER  TGPS_TAG(ANC__MTPS)*8
           TYPE ( ANC_CNF__TYP )           :: CNF
           TYPE ( ANC_PRV__TYP )           :: PRV(ANC__MVPV)
           INTEGER*4  LAST_FIELD
!
           INTEGER*4, POINTER :: NEP_ARR(:) => NULL()  ! Number of TSYS epochs for each TPS
           TYPE ( ANC_DOO__TYP ),  POINTER :: DOO(:)  => NULL()
           TYPE ( ANC_MET__TYP ),  POINTER :: MET(:)  => NULL()
           TYPE ( ANC_TPS__TYP ),  POINTER :: TPS(:)  => NULL()
           TYPE ( ANC_TTO__TYP ),  POINTER :: TTO(:) => NULL()
           TYPE ( ANC_PCS__TYP ),  POINTER :: PCS(:)  => NULL()
           TYPE ( ANC_PCAL__TYP ), POINTER :: PCAL(:) => NULL()
           TYPE ( ANC_SEFD__TYP ), POINTER :: SEFD(:) => NULL()
           TYPE ( ANC_TGPS__TYP ), POINTER :: TGPS(:) => NULL()
           TYPE ( ANC_GPS__TYP ),  POINTER :: GPS(:)  => NULL()
           TYPE ( ANC_CBL__TYP ),  POINTER :: CBL(:)  => NULL()
           TYPE ( ANC_TPI__TYP ),  POINTER :: TPI(:)  => NULL()
!%%!          TYPE ( ANC_EPO__TYP ),  POINTER :: EPO(:)  => NULL()
      END  TYPE ANC__TYP
! ---
      INTEGER*4  ATP__UNDF, ATP__LOAD, ATP__INIT
      PARAMETER  ( ATP__UNDF =        0 )
      PARAMETER  ( ATP__LOAD =  1028931 )
      PARAMETER  ( ATP__INIT = 22831221 )
!
      INTEGER*4    ATP__SPD_MARGIN
      PARAMETER  ( ATP__SPD_MARGIN = 3 ) ! Margin in the epochs number at the beginning and the end for a reliable interpolation
