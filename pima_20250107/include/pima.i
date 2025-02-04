!
! >>>>> pima.i   2006.01.06   v 2.44  --  2025.01.07_19:50:37t
!
	INTEGER*4    PIM__MACT, PIM__MOPT, PIM__MFIL, PIM__MHDR, PIM__MKWD, &
     &               PIM__MUVT, PIM__MSOU, PIM__MSTA, PIM__MBAS, PIM__MEPC, &
     &               PIM__MCST, PIM__MCSO, PIM__MSCA, PIM__MUV,  PIM__MFRQ, &
     &               PIM__MCHN, PIM__MBWN, PIM__MFFT, PIM__MFRI, PIM__MOBS, &
     &               PIM__MPOL, PIM__MDPL, PIM__MBND, PIM__MAP,  PIM__MUVS, &
     &               PIM__MTON, PIM__MFRG, PIM__MISO, PIM__MSYN, PIM__MFRA, &
     &               PIM__MGPL, PIM__MPCS, PIM__NSTA, PIM__MNZO, PIM__MSUB, &
     &               PIM__MPLR, PIM__MPIF, PIM__MWVR
	REAL*8       PIM__MSCL
	INTEGER*8    PIMA__STACK_SIZE_IN_GIGABYTES
!
	PARAMETER  ( PIM__MACT =     3 ) ! Number of actions
	PARAMETER  ( PIM__MOPT =  1024 ) ! Maximal number of options
	PARAMETER  ( PIM__MFIL =  1024 ) ! Maximum number of files
	PARAMETER  ( PIM__MHDR =    64 ) ! Maximum number of headers in each file
	PARAMETER  ( PIM__MKWD = 32768 ) ! Maximum number of keywords in each file
	PARAMETER  ( PIM__MUVT =    64 ) ! Maximal number of UV tables in one file
	PARAMETER  ( PIM__MSOU =  1024 ) ! Maximal number of sources  in the experiment
	PARAMETER  ( PIM__MSTA =    32 ) ! Maximal number of stations in the experiment
	PARAMETER  ( PIM__NSTA =   128 ) ! Maximal number of stations in the antenna table
	PARAMETER  ( PIM__MBAS = (PIM__MSTA*(PIM__MSTA+1))/2 ) ! max # baselines
	PARAMETER  ( PIM__MCSO = 65536 ) ! Maximal number of sources  in the cat
	PARAMETER  ( PIM__MCST =   512 ) ! Maximal number of stations in the cat
	PARAMETER  ( PIM__MEPC = 1024*1024 ) ! Maximal number of epochs
	PARAMETER  ( PIM__MSCA = 16*1024   ) ! Maximal number of scans
	PARAMETER  ( PIM__MFRQ =     64 ) ! Maximal number of frequencies
!@	  PARAMETER  ( PIM__MUV  =  512*1024 ) ! Maximal number of UV points in a scan
!@	  PARAMETER  ( PIM__MCHN = 512*1024 ) ! Maximal number of spectral channels
	PARAMETER  ( PIM__MUV  =  65536 ) ! Maximal number of UV points in a scan
	PARAMETER  ( PIM__MCHN = 128*1024 ) ! Maximal number of spectral channels
!
	PARAMETER  ( PIM__MBWN =   1024 ) ! Maximal size of muti-band drf window
	PARAMETER  ( PIM__MFFT = 256*1024 )  ! Maximal dimension of FFT
	PARAMETER  ( PIM__MFRI =      256 )  ! Maximal number of fringe files
	PARAMETER  ( PIM__MOBS = 128*1024 )  ! Maximal number of observations
	PARAMETER  ( PIM__MPOL =  64      )  ! Maximum degree of polynom for
!                                            ! bandpass modeling
	PARAMETER  ( PIM__MDPL =   6      )  ! Maximum degree of polynom for
!                                            ! observation model
	PARAMETER  ( PIM__MBND =   2      )  ! Maximum number of bands
!
        PARAMETER  ( PIM__MAP  =    65536 )  ! Maximum number of accumulation periods in one scan
	PARAMETER  ( PIM__MSCL = 1800.0D0 )  ! Maximum scan length in seconds
	PARAMETER  ( PIM__MTON = 4096     )  ! Maximum number of phase-cal tones
	PARAMETER  ( PIM__MUVS =   8      )  ! Maximum number of spliting uv data between files
	PARAMETER  ( PIM__MFRG =  10      )  ! Maximum number of frequency groups
	PARAMETER  ( PIM__MISO =  32      )  ! Maximum number of in-beam sources
	PARAMETER  ( PIM__MSYN =   8      )  ! Maximum number of synomym names
	PARAMETER  ( PIM__MFRA =   4      )  ! Number of fringe search algorithms
	PARAMETER  ( PIM__MGPL =   8      )  ! Max degree for gain curve polynomial
	PARAMETER  ( PIM__MPCS =   4      )  ! Max number of phase calibrators per target source
        PARAMETER  ( PIM__MNZO = 1024*1024 ) ! Max number of time epochs for near zone object
	PARAMETER  ( PIM__MSUB =  32      )  ! Max number of subarrays
	PARAMETER  ( PIM__MPLR =   4      )  ! Max number of polarization combinations
	PARAMETER  ( PIM__MPIF =   2      )  ! Max number of polarization IFs
	PARAMETER  ( PIM__MWVR =  32      )  ! Max number of WVR files
  	INTEGER*4  PIM__FILL, PIM__CNFL
	PARAMETER  ( PIM__CNFL = 276*1024 )  ! Size in bytes of the configuration section
	PARAMETER  ( PIM__FILL =    10402 )  ! Size in bytes of the filler
	PARAMETER  ( PIMA__STACK_SIZE_IN_GIGABYTES = 8 )
!
	CHARACTER  PIMA__LABEL*28
	CHARACTER  PIMA__FORMAT_LABEL*36, PIMA__BPASS_LABEL*36, &
      &            PIMA__FLAG_LABEL*34, PIMA__CONTROL_LABEL*50, &
      &            PIMA__BPASS_STA_LABEL*40, PIMA__BPASS_STA_LAB_01*40, PIMA__BPASS_STA_LAB_02*40, &
      &            PIMA__POL_BPASS_LABEL*40, PIMA__POL_BPASS_LAB_01*40, &
      &            PIMA__BPASS_MASK_LABEL*40, &
      &            PIMA__PCAL_MASK_LABEL*38, PIMA__BPASS_MASK_GEN*40, &
      &            PIMA__FRIRES_LABEL*60, PIMA__RESID_LABEL*62, &
      &            PIMA__TOTAL_LABEL*62, PIMA__STT_LABEL*56, PIMA__TXT1D_LABEL*46, &
      &            PIMA__TXT2D_LABEL*46, &
      &            PIMA__ACTA_LABEL*44, PIMA__PCAL_MASK_GEN*40, &
      &            PIMA__PCAL_RMS_GEN*39, PIMA__PCAL_RMS_LABEL*38, &
      &            PIMA__PCAL_RPT_GEN*39, PIMA__PCAL_RPT_LABEL*38, &
      &            PIMA__KJCC_V1_LABEL*70, PIMA__GACO_LABEL*60, PIMA__TSOU_LABEL*45, &
      &            PIMA__TSZE_LABEL*50, PIMA__SPLT_TOT_LABEL*72
	CHARACTER  PIMA__FRIRES_LABEL_20100405*60, PIMA__FRIRES_LABEL_20140208*60, &
      &            PIMA__FRIRES_LABEL_20141224*60, PIMA__FRIRES_LABEL_20190224*60, &
      &            PIMA__FRIRES_LABEL_20190420*60, PIMA__FRIRES_LABEL_20221215*60, &
     &             PIMA__TOTAL_LABEL_20180128*62
	PARAMETER  ( PIMA__LABEL= 'PIMA 20250107  version 2.44 ' )
	PARAMETER  ( PIMA__FORMAT_LABEL     = 'PIMA DUMP  Format v 2.40  2022.12.15' )
        PARAMETER  ( PIMA__CONTROL_LABEL    = '# PIMA_CONTROL file.  Format Version of 2020.04.19' )
	PARAMETER  ( PIMA__BPASS_STA_LABEL  = 'PIMA BPASS_STA  Format v 1.1  2020.03.17' )
	PARAMETER  ( PIMA__BPASS_STA_LAB_01 = 'PIMA BPASS_STA  Format v 1.01 2017.03.03' )
	PARAMETER  ( PIMA__BPASS_STA_LAB_02 = 'PIMA BPASS_STA  Format v 0.89 2010.01.01' )
	PARAMETER  ( PIMA__BPASS_LABEL      = 'PIMA BPASS Format v 0.80 2009.01.26 ' )
	PARAMETER  ( PIMA__POL_BPASS_LABEL  = 'PIMA POL_BPASS  Format v 1.00 2017.03.03' )
        PARAMETER  ( PIMA__FLAG_LABEL       = '# PIMA_FLAG  Format of 2008.07.09 ' )
	PARAMETER  ( PIMA__BPASS_MASK_LABEL = '# PIMA BPASS_MASK   Format of 2013.08.15' )
	PARAMETER  ( PIMA__BPASS_MASK_GEN   = '# PIMA BPASS_MASK_GEN  v 0.90 2009.02.05' )
	PARAMETER  ( PIMA__PCAL_MASK_GEN    = '# PIMA PCAL_MASK_GEN  v  1.00 2015.05.10' )
	PARAMETER  ( PIMA__PCAL_MASK_LABEL  = '# PIMA PCAL_MASK  Format of 2014.12.18' )
	PARAMETER  ( PIMA__PCAL_RMS_GEN     = '# PIMA PCAL_RMS_GEN  v  1.00 2022.06.30' )
	PARAMETER  ( PIMA__PCAL_RMS_LABEL   = '# PIMA PCAL_RMS   Format of 2022.06.30' )
	PARAMETER  ( PIMA__PCAL_RPT_GEN     = '# PIMA PCAL_RPT_GEN  v  1.00 2022.07.07' )
	PARAMETER  ( PIMA__PCAL_RPT_LABEL   = '# PIMA PCAL_RPT   Format of 2022.07.07' ) 
        PARAMETER  ( PIMA__FRIRES_LABEL     = '# PIMA Fringe results  v  1.3   Format version of 2022.12.15' )
        PARAMETER  ( PIMA__RESID_LABEL      = '# PIMA Fringe residuals  v 1.00  Format version of  2009.08.22' )
        PARAMETER  ( PIMA__TOTAL_LABEL      = '# PIMA Total observables  v 1.2   Format version of 2019.04.22' )
        PARAMETER  ( PIMA__STT_LABEL        = '#  VLBI session statistics. Format version of 2010.01.30' )
	PARAMETER  ( PIMA__TXT1D_LABEL      = '# 1D text table.  Format version of 2012.12.30' )
	PARAMETER  ( PIMA__TXT2D_LABEL      = '# 2D text table.  Format version of 2012.12.30' )
	PARAMETER  ( PIMA__ACTA_LABEL       = '# ACTA Output.  Format version of 2021.07.15' )
        PARAMETER  ( PIMA__KJCC_V1_LABEL    = '# KJCC   A priori interferometric delay.  Format version of 2015.11.30' )
        PARAMETER  ( PIMA__GACO_LABEL       = '# PIMA Gain correction  v 1.0   Format version of 2016.02.24' )
        PARAMETER  ( PIMA__TSOU_LABEL       = '# PIMA_TSYS_OUT  Format version of 2020.08.04' )
        PARAMETER  ( PIMA__TSZE_LABEL       = '# PIMA_TSYS_ZEN_ELEV  Format version of 2020.08.05' )
        PARAMETER  ( PIMA__SPLT_TOT_LABEL   = '# Total fringe phases and amplitudes. Format  1.00 Version of 2020.04.18' )
!
! ----- Obsolete labels
!
        PARAMETER  ( PIMA__FRIRES_LABEL_20100405 = '# PIMA Fringe results  v  1.00  Format version of 2010.04.05'   )
	PARAMETER  ( PIMA__FRIRES_LABEL_20140208 = '# PIMA Fringe results  v  1.01  Format version of 2014.02.08'   )
	PARAMETER  ( PIMA__FRIRES_LABEL_20141224 = '# PIMA Fringe results  v  1.02  Format version of 2014.12.24'   )
	PARAMETER  ( PIMA__FRIRES_LABEL_20190224 = '# PIMA Fringe results  v  1.1   Format version of 2019.02.24'   )
	PARAMETER  ( PIMA__FRIRES_LABEL_20190420 = '# PIMA Fringe results  v  1.2   Format version of 2019.04.20'   )
	PARAMETER  ( PIMA__FRIRES_LABEL_20221215 = '# PIMA Fringe results  v  1.3   Format version of 2022.12.15'   )
	PARAMETER  ( PIMA__TOTAL_LABEL_20180128  = '# PIMA Total observables  v 1.04  Format version of 2018.01.28' )
!
        CHARACTER  BANDPASS_MASK_TEMPLATE*77
        PARAMETER ( BANDPASS_MASK_TEMPLATE = '         IND_FRQ:       IND_CHN:        IND_ABS_CHN:         MASK:           ' )
        TYPE  BANDPASS_MASK_TEXT__TYPE
              CHARACTER  STA_NAM*8
              CHARACTER  FILLER_1*11
              CHARACTER  IND_FRQ*4
              CHARACTER  FILLER_2*11
              CHARACTER  IND_CHN*5
              CHARACTER  FILLER_3*15
              CHARACTER  IND_ABS_CHN*6
              CHARACTER  FILLER_4*8
              CHARACTER  MASK_AUTC*1
              CHARACTER  FILLER_5*1
              CHARACTER  MASK_BPAS*1
              CHARACTER  FILLER_6*1
              CHARACTER  MASK_FRNG*1
              CHARACTER  FILLER_7*1
              CHARACTER  MASK_SPLT*1
              CHARACTER  FILLER_8*2
        END TYPE  BANDPASS_MASK_TEXT__TYPE
!
        CHARACTER  PCAL_MASK_TEMPLATE*70
        PARAMETER ( PCAL_MASK_TEMPLATE = '         IND_FRQ:       IND_TONE:        IND_ABS_CHN:         MASK:   ' )
        TYPE  PCAL_MASK_TEXT__TYPE
              CHARACTER  STA_NAM*8
              CHARACTER  FILLER_1*11
              CHARACTER  IND_FRQ*4
              CHARACTER  FILLER_2*12
              CHARACTER  IND_TONE*5
              CHARACTER  FILLER_3*15
              CHARACTER  IND_ABS_TONE*6
              CHARACTER  FILLER_4*8
              CHARACTER  MASK*1
        END TYPE  PCAL_MASK_TEXT__TYPE
!
	CHARACTER  PCAL_RMS_TEMPLATE*89
        PARAMETER ( PCAL_RMS_TEMPLATE = '         IND_FRQ:       IND_TONE:        IND_ABS_CHN:         RMS:                       ' )
        TYPE  PCAL_RMS_TEXT__TYPE
              CHARACTER  STA_NAM*8
              CHARACTER  FILLER_1*11
              CHARACTER  IND_FRQ*4
              CHARACTER  FILLER_2*12
              CHARACTER  IND_TONE*5
              CHARACTER  FILLER_3*15
              CHARACTER  IND_ABS_TONE*6
              CHARACTER  FILLER_4*8
              CHARACTER  RMS_VAL*20
        END TYPE  PCAL_RMS_TEXT__TYPE
!
	CHARACTER  PCAL_RMS_FREQ_TEMPLATE*51
        PARAMETER ( PCAL_RMS_FREQ_TEMPLATE = '         IND_FRQ:              RMS:                ' )
        TYPE  PCAL_RMS_FREQ_TEXT__TYPE
              CHARACTER  STA_NAM*8
              CHARACTER  FILLER_1*11
              CHARACTER  IND_FRQ*4
              CHARACTER  FILLER_4*8
              CHARACTER  RMS_VAL*20
        END TYPE  PCAL_RMS_FREQ_TEXT__TYPE
!
	CHARACTER  PCAL_RPT_TEMPLATE*100
	PARAMETER ( PCAL_RPT_TEMPLATE = 'PCAL  STA:           IND_FRQ:           IND_TONE:            OFF !    Problem:                      ' )
        TYPE  PCAL_RPT_TEXT__TYPE
	      CHARACTER  FILLER_1*12
              CHARACTER  STA_NAM*8
              CHARACTER  FILLER_2*11
              CHARACTER  IND_FRQ*6
              CHARACTER  FILLER_3*14
              CHARACTER  IND_TONE*6
              CHARACTER  FILLER_4*23
              CHARACTER  RPT_VAL*20
        END TYPE  PCAL_RPT_TEXT__TYPE
!
	TYPE  PIM_FIL__TYPE
	      CHARACTER  ORIG_NAME*128
	      CHARACTER  NAME*128
	      INTEGER*8  FITS_DESC
	      INTEGER*4  L_HDR
	      INTEGER*4  M_KWD
	      INTEGER*4  L_KWD(PIM__MHDR)
	      INTEGER*4  MJD_REF
	      INTEGER*4  NUM_UV_TAB
	      INTEGER*4  IND_UV_TAB(PIM__MUVT)
	      INTEGER*4  IND_FLUX_KEY(PIM__MUVT)
	      INTEGER*4  IND_WEIGHT_KEY(PIM__MUVT)
	      INTEGER*4  IND_INTTIM_KEY(PIM__MUVT)
	      INTEGER*4  N_SOU
	      CHARACTER  SOU_NAME_ORIG(PIM__MSOU)*16
	      INTEGER*4  N_STA
	      CHARACTER  ANT_STA_REF(PIM__NSTA)
	      REAL*8     REF_FREQ
	      INTEGER*4  NFRQ
	      INTEGER*4  NFRG
!
	      INTEGER*4  REF_FRQ(PIM__MFRQ,PIM__MFRG) ! Cross reference table from
	      INTEGER*4  REF_FRG(PIM__MFRQ,PIM__MFRG) ! the file specific to global frequency table
!
	      INTEGER*4  REV_FRQ(PIM__MFRQ,PIM__MFRG) ! Reverse cross reference table:
	      INTEGER*4  REV_FRG(PIM__MFRQ,PIM__MFRG) ! from the global to the file specific frequency table
!
              INTEGER*4  STATUS
	      CHARACTER*80, POINTER :: KEY(:,:) => NULL()
	END   TYPE  PIM_FIL__TYPE
!
	TYPE  PIM_CONF__TYPE
	      CHARACTER  BAND*1
	      CHARACTER  FILLER_01*7
	      CHARACTER  SESS_CODE*64
	      CHARACTER  FRINGE_ERRORS*8
	      CHARACTER  STA_NAMES_FILE*128
	      CHARACTER  SOU_NAMES_FILE*128
	      CHARACTER  PHAS_CAL_CODE*8
	      CHARACTER  TSYS_CAL_CODE*8
	      CHARACTER  TSYS_EXT_CODE*8
	      CHARACTER  TSYS_EXT_PAR*128
	      CHARACTER  GAIN_CAL_CODE*8
	      CHARACTER  SAMPLER_CAL_CODE*8
	      CHARACTER  UVFILE_NAME(PIM__MFIL)*128
	      CHARACTER  WVR_FILE(PIM__MWVR)*128
	      CHARACTER  WVR_USE*8
	      CHARACTER  INTMOD_TYPE*8
	      CHARACTER  INTMOD_FILE(PIM__MFIL)*128
	      CHARACTER  EXPER_DIR*128
	      CHARACTER  VTD_CONFIG_FILE*128
	      CHARACTER  FFT_CONFIG_FILE*128
	      CHARACTER  EXCLUDE_UV_FINAM*128
	      CHARACTER  BANDPASS_USE*8
	      CHARACTER  BANDPASS_FILE*128
	      CHARACTER  BANDPASS_MASK_FILE*128
	      CHARACTER  PCAL_MASK_FILE*128
	      CHARACTER  POLARCAL_FILE*128
	      INTEGER*4  CORR_FLAG_MIN
	      CHARACTER  TIME_FLAG_FILE*128
	      CHARACTER  TEC_FILE*128
	      CHARACTER  FRINGE_FILE*128
	      CHARACTER  FRIRES_FILE*128
	      CHARACTER  INCLUDE_OBS_FILE*128
	      CHARACTER  EXCLUDE_OBS_FILE*128
	      CHARACTER  STA_REF*8
              CHARACTER  POLAR*8
              CHARACTER  STAGING_DIR*128
	      REAL*8     AP_TOLERANCE
	      REAL*8     MIN_SCAN_LEN
	      REAL*8     MAX_SCAN_LEN
	      REAL*8     MAX_SCAN_GAP
	      REAL*8     SCAN_LEN_USED
	      REAL*8     SCAN_LEN_SKIP
	      REAL*8     FRT_OFFSET
	      REAL*8     PHASE_ACCELERATION
	      REAL*8     PHASE_ACCEL_MIN
	      REAL*8     PHASE_ACCEL_MAX
	      CHARACTER  FRT_USE*8
	      CHARACTER  FRT_FILE*128
              CHARACTER  EPHEMERIDES_FILE*128
              CHARACTER  EPHEMERIDES_USE*8
	      INTEGER*4  FFT_METHOD
	      INTEGER*4  NUM_THREADS
	      INTEGER*4  BEG_FRQ
	      INTEGER*4  END_FRQ
	      INTEGER*4  FRQ_GRP
	      INTEGER*4  FRG_USE
	      INTEGER*4  FRG_LIST(PIM__MFRG)
	      CHARACTER  ONOF_GEN_FLAGS_MODE*8
	      REAL*8     ONOF_AMPL_THRESHOLD
	      REAL*8     ONOF_NSIG_THRESHOLD
	      REAL*8     ONOF_COHERENT_INTERVAL
              INTEGER*4  ONOF_MIN_LOW_AP
	      REAL*8     ONOF_KERNEL_START_SHARE
	      REAL*8     ONOF_KERNEL_END_SHARE
	      REAL*8     WVR_SMOOTHING_INTERVAL
	      REAL*8     WVR_SMOOTHING_SIGMA
!
              REAL*8     FRIB_DELAY_WINDOW_CENTER
              REAL*8     FRIB_RATE_WINDOW_CENTER
              REAL*8     FRIB_DELAY_WINDOW_WIDTH
              REAL*8     FRIB_RATE_WINDOW_WIDTH
	      INTEGER*4  FRIB_OVERSAMPLE_MD
	      INTEGER*4  FRIB_OVERSAMPLE_RT
	      INTEGER*4  FRIB_SECONDARY_MAX_TRIES
	      REAL*8     FRIB_SECONDARY_SNR_MIN
              REAL*8     FRIB_AUTOCORR_THRESHOLD
              REAL*8     FRIB_WEIGHTS_THRESHOLD
              REAL*8     FRIB_DELAY_STOP_SEARCH
              REAL*8     FRIB_RATE_STOP_SEARCH
              REAL*8     FRIB_NOISE_NSIGMA
              REAL*8     FRIB_SNR_DETECTION
              CHARACTER  FRIB_SEARCH_TYPE*8
              CHARACTER  FRIB_FINE_SEARCH*8
              CHARACTER  FRIB_FRQ_TRANSFER_BAND*128
              CHARACTER  FRIB_FRQ_TRANSFER_METHOD*8
              INTEGER*4  FRIB_FRQ_TRANSFER_DEG
              INTEGER*4  FRIB_FRQ_TRANSFER_MSEG
!
	      INTEGER*4  FRIB_OVERSAMPLE_PLOT_MD
	      INTEGER*4  FRIB_OVERSAMPLE_PLOT_RT
              REAL*8     FRIB_PLOT_DELAY_WINDOW_WIDTH
              REAL*8     FRIB_PLOT_RATE_WINDOW_WIDTH
              CHARACTER  FRIB_2D_FRINGE_PLOT*8
              CHARACTER  FRIB_1D_RESTIM_PLOT*8
              CHARACTER  FRIB_1D_RESFRQ_PLOT*8
              CHARACTER  FRIB_1D_DRF_PLOT*8
	      INTEGER*4  FRIB_1D_TIM_MSEG
	      INTEGER*4  FRIB_1D_FRQ_MSEG
	      REAL*8     FRIB_1D_DRF_SPAN
!
	      CHARACTER  FRIB_OBS_COMMAND*128
	      CHARACTER  FRIB_AUTOCORR_CALIB*8
	      CHARACTER  FRIB_AMPL_FUDGE_TYPE*8
	      CHARACTER  FRIB_AMPL_EDGE_WINDOW_COR*8
	      CHARACTER  FRIB_AMPL_EDGE_BEAM_COR*8
	      INTEGER*4  FRIB_NOBS
	      INTEGER*4  FRIB_FIRST_OBS
	      INTEGER*4  FRIB_LAST_OBS
	      CHARACTER  FRIB_OBS_STATUS*8
!
	      CHARACTER  FRIP_SCAN_FILE*128
	      CHARACTER  FRIP_MAP_DIR*128
	      CHARACTER  FRIP_ATM_ZEN_FILE*128
	      CHARACTER  FRIP_STA_INC_FILE*128
	      CHARACTER  FRIP_STA_EXC_FILE*128
	      INTEGER*4  FRIP_N_STA_REF
	      CHARACTER  FRIP_STA_REFS(PIM__MSTA)*8
	      INTEGER*4  FRIP_RESOLUTION
	      INTEGER*4  FRIP_NSCA
	      INTEGER*4  FRIP_FIRST_SCA
	      INTEGER*4  FRIP_LAST_SCA
	      CHARACTER  FRIP_MAP_PLOT*8
	      CHARACTER  FRIP_SCA_STATUS*8
	      REAL*8     FRIP_OVERSAMPLE
	      CHARACTER  FRIP_CAL_PLOT*8
              INTEGER*4  FRIP_CAL_RES
	      CHARACTER  FRIP_TAG_PLOT*8
              INTEGER*4  FRIP_TAG_RES
	      CHARACTER  FRIP_BEAM_PLOT*8
	      INTEGER*4  FRIP_FRQ_MSEG
	      INTEGER*4  FRIP_TIM_MSEG
	      REAL*8     FRIP_RA_CENTER
	      REAL*8     FRIP_DEC_CENTER
	      REAL*8     FRIP_RA_STEP
	      REAL*8     FRIP_DEC_STEP
	      REAL*8     FRIP_RA_RANGE
	      REAL*8     FRIP_DEC_RANGE
!
	      CHARACTER  SPLT_SOU_NAME*16
	      INTEGER*4  SPLT_FRQ_MSEG
	      INTEGER*4  SPLT_TIM_MSEG
	      CHARACTER  SPLT_WEIGHT_TYPE*8
	      CHARACTER  SPLT_POLAR*8
	      CHARACTER  SPLT_STA_BASED*8
              CHARACTER  SPLT_AUTOCORR_NRML_METHOD*8
              CHARACTER  SPLT_BPASS_NRML_METHOD*8
	      CHARACTER  SPLT_GAIN_CORR_FILE*128
	      CHARACTER  SPLT_SUBARRAY_CONSOLIDATION*8
	      REAL*8     SPLT_BPASS_NRML_RANGE(2)
!
	      CHARACTER  MKDB_OUTPUT_TYPE*8
	      CHARACTER  MKDB_SRT_TYPE*8
	      CHARACTER  MKDB_SRT_FILE*128
	      CHARACTER  MKDB_2ND_BAND_FILE*128
              CHARACTER  MKDB_FRINGE_ALGORITHM*8
	      CHARACTER  MKDB_FILTER*8
	      CHARACTER  MKDB_VCAT_CONFIG*128
	      CHARACTER  MKDB_SUFFIX*128
	      CHARACTER  MKDB_OUTPUT_NAME*128
	      CHARACTER  MKDB_DESC_FILE*128
	      REAL*8     MKDB_GD_MAX_ADD_ERROR
	      REAL*8     MKDB_GD_MAX_SCL_ERROR
!
	      REAL*8     BPS_SNR_MIN_ACCUM
	      REAL*8     BPS_SNR_MIN_FINE
	      REAL*8     BPS_AMPL_REJECT
	      REAL*8     BPS_PHAS_REJECT
	      INTEGER*4  BPS_NOBS_ACCUM
	      INTEGER*4  BPS_NOBS_FINE
	      INTEGER*4  BPS_MINOBS_FINE
	      INTEGER*4  BPS_MSEG_ACCUM
	      INTEGER*4  BPS_MSEG_FINE
	      CHARACTER  BPS_INTRP_METHOD*8
	      INTEGER*4  BPS_DEG_AMP
	      INTEGER*4  BPS_DEG_PHS
	      REAL*8     BPS_AMP_MIN
	      CHARACTER  BPS_MODE*8
	      CHARACTER  BPS_NORML*8
	      CHARACTER  BPS_SEFD_USE*8
!
	      CHARACTER  ACT_CODE*8
!
	      INTEGER*4  L_FIL
	      INTEGER*4  L_INM
	      INTEGER*4  L_WVR
	      INTEGER*4  DEBUG_LEVEL
	      INTEGER*4  CHECK_SEVERITY
	      LOGICAL*4  WARNING
	      CHARACTER  FRIP_SOU*10
!
	      CHARACTER  SPLT_TOTAL_UV*8
	      REAL*8     SPLT_SNR_MIN
	      REAL*8     BPS_DECOR_TIM_MIN
	      INTEGER*4  L_PUS
	      INTEGER*4  PUS_TYPE
	      CHARACTER  PCAL_USE_STA(PIM__MSTA)*8
	      CHARACTER  FILLER*(PIM__FILL)
	      INTEGER*4  LAST_FIELD
!
	      INTEGER*4, POINTER :: FRIB_OBS(:) => NULL()
	      INTEGER*4, POINTER :: FRIP_SCA(:,:) => NULL()
	END   TYPE  PIM_CONF__TYPE
!
	TYPE  PIM_PCAL__TYPE
	      LOGICAL*4   PCAL_AVAIL
	      LOGICAL*4   PCAL_USE
	      LOGICAL*4   PCAL_SCA
              INTEGER*4   ISTA
	      INTEGER*4   NO_TONES
	      INTEGER*4   NPOI
	      INTEGER*4   NPOL
	      INTEGER*4   NSCA
	      REAL*4      PCAL_MB_GRDEL
	      REAL*4      PCAL_SB_GRDEL(PIM__MFRQ)
	      INTEGER*4   PCAL_GRDEL_STATUS
	      INTEGER*4   PCAL_MASK_STATUS
	      REAL*4,     POINTER :: PHAS(:,:,:,:)     => NULL()
	      REAL*4,     POINTER :: AMPL(:,:,:,:)     => NULL()
	      REAL*4,     POINTER :: PHAS_RGR(:,:,:,:) => NULL()
	      REAL*4,     POINTER :: AMPL_RGR(:,:,:,:) => NULL()
	      REAL*4,     POINTER :: PHAS_SCA(:,:,:,:) => NULL()
	      REAL*4,     POINTER :: AMPL_SCA(:,:,:,:) => NULL()
	      REAL*4,     POINTER :: PRAT_SCA(:,:,:,:) => NULL()
	      REAL*8,     POINTER :: FREQ(:,:,:)       => NULL()
	      REAL*8,     POINTER :: RATE(:,:,:,:)     => NULL()
	      INTEGER*1,  POINTER :: MASK(:,:,:,:)     => NULL()
	      INTEGER*4,  POINTER :: IAMB(:,:,:)       => NULL()
	      INTEGER*4,  POINTER :: IPOI_SCA(:)       => NULL()
	      INTEGER*4,  POINTER :: ISCA_POI(:)       => NULL()
	      INTEGER*4,  POINTER :: SOU_IND(:)        => NULL()
	      REAL*8,     POINTER :: TIME_MID_R8(:)    => NULL()
	      REAL*8,     POINTER :: TIME_SCA_R8(:)    => NULL()
	      REAL*4,     POINTER :: TIME_SPAN_R4(:)   => NULL()
	END TYPE  PIM_PCAL__TYPE
!
	TYPE  PIM_CABLE__TYPE
	      LOGICAL*4   CAB_AVAIL
	      INTEGER*4   NPOI
	      REAL*8      MEAN_CABLE
	      INTEGER*4   CABLE_SIGN
	      REAL*8,     POINTER :: TIM_CAB(:)       => NULL()
	      REAL*8,     POINTER :: CAB_DEL(:)       => NULL()
	END TYPE  PIM_CABLE__TYPE
!
	TYPE  PIM_TSYS__TYPE
	      LOGICAL*4   AVAIL
	      INTEGER*4   NPOI
	      INTEGER*4   NPOL
	      REAL*8,     POINTER :: TSYS(:,:,:)      => NULL()
	      REAL*8,     POINTER :: TIME_MID_R8(:)   => NULL()
	      INTEGER*4,  POINTER :: SOU_IND(:)       => NULL()
	      REAL*4,     POINTER :: TIME_SPAN_R4(:)  => NULL()
	      REAL*4,     POINTER :: AZ_R4(:)         => NULL()
	      REAL*4,     POINTER :: ELEV_R4(:)       => NULL()
	END TYPE  PIM_TSYS__TYPE
!
	TYPE  PIM_STMO__TYPE
	      INTEGER*4   N_OPA
	      INTEGER*4   N_TAT
	      INTEGER*4   N_TREC
	      INTEGER*4   N_TSPI
	      INTEGER*4   N_TSYS
	      INTEGER*4   N_TTOA
	      INTEGER*4   IF_REF
	      LOGICAL*4   OPA_AVAIL
	      LOGICAL*4   TREC_AVAIL
	      LOGICAL*4   TSPI_AVAIL
	      LOGICAL*4   TSYS_AVAIL
	      LOGICAL*4   TTOA_AVAIL
	      LOGICAL*4   TSRAT_AVAIL
	      INTEGER*4   STATUS
!
	      INTEGER*4,  POINTER :: IND_SCA(:)      => NULL() ! Scan index
	      REAL*8,     POINTER :: TIM(:)          => NULL() ! Time
	      REAL*8,     POINTER :: EL(:)           => NULL() ! Elevatin angle
	      REAL*8,     POINTER :: AZ(:)           => NULL() ! Azimith
	      REAL*8,     POINTER :: OPA(:,:)        => NULL() ! Atmosphere opacity
	      REAL*8,     POINTER :: TAT(:,:)        => NULL() ! Atmosphere radiative temperature
	      REAL*8,     POINTER :: TREC(:,:)       => NULL() ! Receiver temperature
	      REAL*8,     POINTER :: TSPI(:,:)       => NULL() ! Spillover temperature
	      REAL*8,     POINTER :: TSYS_CLN(:,:,:) => NULL() ! Clean system temperature
	      REAL*8,     POINTER :: TSYS_MOD(:,:,:) => NULL() ! Modeled system temperature
	      REAL*8,     POINTER :: TTOA(:,:)       => NULL() ! System temperature at the top of the atmosphere
	      REAL*8      TSRAT(PIM__MFRQ,2)                   ! Ratios of Tsys in IFx wrt the reference IF
	END TYPE  PIM_STMO__TYPE
!
	TYPE  PIM_WEA__TYPE
	      LOGICAL*4   AVAIL
	      INTEGER*4   NPOI
	      REAL*8,     POINTER :: TIME_BEG(:)  => NULL()
	      REAL*8,     POINTER :: TIME_END(:)  => NULL()
	      REAL*8,     POINTER :: PRES(:)      => NULL()
	      REAL*8,     POINTER :: TEMP(:)      => NULL()
	      REAL*8,     POINTER :: HUMID(:)     => NULL()
	END TYPE  PIM_WEA__TYPE
!
	TYPE  PIM_MDC__TYPE
	      REAL*8      CLO_OFFS
	      REAL*8      CLO_RATE
	      REAL*8      CLO_OFFS_ERR
	      REAL*8      CLO_RATE_ERR
	      REAL*8      TAI_REF
	      INTEGER*4   MJD_REF
	      INTEGER*4   CLO_MODEL_STATUS
	      INTEGER*4,  POINTER :: IND_SOU(:)      => NULL()
	      REAL*8,     POINTER :: TIME_CEN(:)     => NULL()
	      REAL*8,     POINTER :: CLOCK_OFFSET(:) => NULL()
	      REAL*8,     POINTER :: CLOCK_RATE(:)   => NULL()
	      REAL*8,     POINTER :: ATMO_DELAY(:)   => NULL()
	      REAL*8,     POINTER :: ATMO_RATE(:)    => NULL()
	      REAL*8,     POINTER :: GDELAY(:)       => NULL()
	      REAL*8,     POINTER :: GRATE(:)        => NULL()
	END TYPE  PIM_MDC__TYPE
!
	TYPE  PIM_GAIN__TYPE
	      LOGICAL*4   AVAIL
	      INTEGER*4   NFRQ
	      INTEGER*4   NPOL
	      INTEGER*4   NTAB
	      INTEGER*4,  POINTER :: TYP  (:,:)    => NULL()
	      INTEGER*4,  POINTER :: NTERM(:,:)    => NULL()
	      INTEGER*4,  POINTER :: X_TYP(:,:)    => NULL()
	      INTEGER*4,  POINTER :: Y_TYP(:,:)    => NULL()
	      REAL*4,     POINTER :: X_VAL(:,:)    => NULL()
	      REAL*4,     POINTER :: Y_VAL(:,:,:)  => NULL()
	      REAL*4,     POINTER :: GAIN (:,:,:)  => NULL()
	      REAL*4,     POINTER :: SENS (:,:)    => NULL()
	END TYPE  PIM_GAIN__TYPE
!
        TYPE       GAIN_KEY__STRU
              REAL*8     FREQ
              REAL*8     DPFU(2)
              REAL*8     POLY(0:PIM__MGPL)
              INTEGER*4  NPOL
              CHARACTER  STA_NAM*8
              CHARACTER  FILLER*2
        END TYPE   GAIN_KEY__STRU
!
	TYPE     PIM__AMPL_SEARCH
	      REAL*4    AMPL
	      INTEGER*4 IND_SD
	      INTEGER*4 IND_RT
	      INTEGER*4 IND_MD
	      INTEGER*4 IND_TRY
	      REAL*8    SD
	      REAL*8    RT
        END TYPE PIM__AMPL_SEARCH
!
	TYPE      PIM_MOD__TYPE
	     REAL*8     TIM_BEG
	     REAL*8     TIM_END
	     REAL*8     GDEL_POL(0:PIM__MDPL,PIM__MFRQ)
	     REAL*8     PDEL_POL(0:PIM__MDPL,PIM__MFRQ)
	     REAL*8     PRAT_POL(0:PIM__MDPL,PIM__MFRQ)
	     INTEGER*4  SOU_IND
	     CHARACTER  SCANNAME*16
	END TYPE  PIM_MOD__TYPE
!
        TYPE  PIM_SCADB__TYPE
	    INTEGER*4  MJD_SRT
            REAL*8     TAI_SRT
            REAL*8     TIM_BEG
            REAL*8     TIM_END
            CHARACTER  NAME*16
            INTEGER*4  NOBS
	    INTEGER*4  SOU_IND
            INTEGER*4, ALLOCATABLE :: OBS_IND(:)
        END TYPE PIM_SCADB__TYPE
!
	TYPE      PIM_WVR__TYPE
	          REAL*8,    POINTER :: TIM_ARR(:) => NULL()
	          REAL*8,    POINTER :: DEL_ARR(:) => NULL()
	          REAL*8,    POINTER :: DEL_ERR(:) => NULL()
	          REAL*8,    POINTER :: EL_ARR(:)  => NULL()
	          REAL*8,    POINTER :: AZ_ARR(:)  => NULL()
		  REAL*8     HEI_WVR
		  INTEGER*4  STATUS
        END TYPE  PIM_WVR__TYPE
!
	TYPE  PIM_STA__TYPE
	      CHARACTER  NAME*8
	      CHARACTER  IVS_NAME*8
	      CHARACTER  ORIG_NAME*16
	      REAL*8     COO(3)
	      REAL*8     COO_ORIG(3)
	      REAL*8     VEL_ORIG(3)
	      REAL*8     ANT_DIAM
	      REAL*8     POL_ANG(PIM__MFRQ)
	      INTEGER*4  L_MOD
	      INTEGER*4  L_MDC
	      INTEGER*4  L_WVR
	      INTEGER*4  IND_ORIG
	      INTEGER*4  POL_TYP(2)
	      INTEGER*4  STATUS
	      TYPE     ( PIM_PCAL__TYPE  ) :: PCAL(PIM__MFRG)
	      TYPE     ( PIM_TSYS__TYPE  ) :: TSYS(PIM__MFRG)
	      TYPE     ( PIM_STMO__TYPE  ) :: STMO(PIM__MFRG)
	      TYPE     ( PIM_GAIN__TYPE  ) :: GAIN(PIM__MFRG)
	      TYPE     ( PIM_CABLE__TYPE ) :: CABLE
	      TYPE     ( PIM_WEA__TYPE   ) :: WEATHER
	      TYPE     ( PIM_MDC__TYPE   ) :: MDC
	      TYPE     ( PIM_WVR__TYPE   ) :: WVR
	      TYPE     ( PIM_MOD__TYPE   ),   POINTER :: MOD(:) => NULL()
	END TYPE  PIM_STA__TYPE
!
	TYPE  PIM_SOU__TYPE
	      REAL*8     ALPHA_INP
	      REAL*8     DELTA_INP
	      REAL*8     ALPHA
	      REAL*8     DELTA
	      REAL*8     S_VEC(3)
	      CHARACTER  NAME*16
	      CHARACTER  IVS_NAME*8
	      CHARACTER  J2000_NAME*10
	      CHARACTER  B1950_NAME*8
	      CHARACTER  DB_NAME*16
	      CHARACTER  FILLER*2
	      INTEGER*4  NISO
	      INTEGER*4  ISO_IND(PIM__MISO)
	      INTEGER*4  NSYN
	      INTEGER*4  SYN_IND(PIM__MSYN)
	      INTEGER*4  IND_SWAP
	      INTEGER*4  IND_LINE
	END TYPE  PIM_SOU__TYPE
!
	TYPE      PIM_NZO__TYPE
              CHARACTER  FILNZO*128
              CHARACTER  NZO_NAME*8
              CHARACTER  OBJ_TYPE*16
              CHARACTER  CENTER_NAME*16
              CHARACTER  REF_NAME*16
              INTEGER*4  TIM_CODE
	      INTEGER*4  COO_CODE
	      INTEGER*4  L_NZO
              INTEGER*4  MJD_ARR(PIM__MNZO)
	      REAL*8     TIM_ARR(PIM__MNZO)
              REAL*8     POS_ARR(3,PIM__MNZO)
              REAL*8     VEL_ARR(3,PIM__MNZO)
	END TYPE  PIM_NZO__TYPE
!
	TYPE      PIM_UVIND__TYPE
             INTEGER*4  TIM_IND
	     INTEGER*4  POI_IND
	     INTEGER*4  POI_AUT_IND(2)
	     INTEGER*2  FIL_IND
	     INTEGER*2  TAB_IND
	     INTEGER*2  STA_IND(2)
	     INTEGER*2  SOU_IND
	     INTEGER*2  SCA_IND
             INTEGER*2  FRG_IND       ! global frequency group index
	     INTEGER*4  OBS_IND       ! observation index
             INTEGER*4  ORIG_IND      ! original UV index
	     INTEGER*4  NEXT_UV_IND   ! Next UV index in the merged chain
	     REAL*4     AP_LEN
	END TYPE  PIM_UVIND__TYPE
!
	TYPE      PIM_OBS__TYPE
	     REAL*8     RES_MB_DEL(PIM__MFRA,PIM__MBND)
	     REAL*8     RES_PH_RAT(PIM__MFRA,PIM__MBND)
	     REAL*8     RES_PHS(PIM__MFRA,PIM__MBND)
	     REAL*8     RES_SB_DEL(PIM__MBND)
	     REAL*8     RES_GR_RAT(PIM__MBND)
	     REAL*8     RES_PH_ACC(PIM__MBND)
	     REAL*4     PCAL_GDEL(2,2)
	     REAL*8     WVR_DEL_AVR
	     REAL*8     TEC
	     REAL*8     TEC_RATE
!
	     REAL*8     TOT_MB_DEL(PIM__MFRA,PIM__MBND)
	     REAL*8     TOT_PH_RAT(PIM__MFRA,PIM__MBND)
	     REAL*8     TOT_SB_DEL(PIM__MBND)
	     REAL*8     TOT_GR_RAT(PIM__MBND)
	     REAL*8     TOT_PH_ACC(PIM__MBND)
!
	     REAL*8     TOT_PHS(PIM__MFRA,PIM__MBND)
	     REAL*8     TOT_PHS_GC(PIM__MFRA,PIM__MBND)
!
	     REAL*8     MB_DEL_ERR(PIM__MFRA,PIM__MBND)
	     REAL*8     PH_DEL_ERR(PIM__MFRA,PIM__MBND)
	     REAL*8     PH_RAT_ERR(PIM__MFRA,PIM__MBND)
	     REAL*8     SB_DEL_ERR(PIM__MBND)
	     REAL*8     GR_RAT_ERR(PIM__MBND)
	     REAL*8     PH_ACC_ERR(PIM__MBND)
	     REAL*8     TEC_ERR
	     REAL*8     TEC_RATE_ERR
!
	     REAL*8     COV_PR_PH(PIM__MBND)
	     REAL*8     COV_GR_GD(PIM__MBND)
	     REAL*8     SCAN_DURA(PIM__MBND)
	     REAL*8     GRAMBSP(PIM__MBND)
	     REAL*8     FEED_ANG(2)
	     REAL*8     FEED_ANG_RATE(2)
!
	     REAL*8     TIM_BEG
	     REAL*8     TIM_END
!
             REAL*8     APR_GC_DEL(2,PIM__MBND)
             REAL*8     APR_GC_RAT(2,PIM__MBND)
             REAL*8     APR_GC_PHS(2,PIM__MBND)
!
             REAL*8     APR_GR_DEL(PIM__MBND)
             REAL*8     APR_RAT(PIM__MBND)
             REAL*8     APR_PHS(PIM__MBND)
             REAL*8     APR_TEC
             REAL*8     APR_TEC_RATE
!
	     REAL*8     CLO_OFFSET_APR(2,PIM__MBND)
	     REAL*8     CLO_RATE_APR(2,PIM__MBND)
!
             REAL*8     THE_GR_DEL
             REAL*8     THE_PH_DEL
             REAL*8     THE_RATE
!
	     REAL*8     DEL_WDT(PIM__MBND)
	     REAL*8     DEL_CEN(PIM__MBND)
	     REAL*8     RATE_WDT(PIM__MBND)
	     REAL*8     RATE_CEN(PIM__MBND)
	     REAL*8     REF_FREQ(PIM__MBND)
	     REAL*8     FRT_OFFSET(PIM__MBND)
	     REAL*8     EFF_FRQ(3,PIM__MBND)
!
	     REAL*4     AMPL(2,PIM__MBND)
	     REAL*4     NOISE(PIM__MBND)
	     INTEGER*4  FRI_STS(PIM__MBND)
	     REAL*4     EFF_DUR(PIM__MBND)
	     INTEGER*4  IND_OBS_2ND
!
	     REAL*8     PRES(2)
	     REAL*8     TEMP(2)
	     REAL*8     HUMID(2)
	     REAL*8     CABLE(2)
!
	     REAL*8     SRT_OFFSET
	     REAL*8     AP_LEN
	     REAL*8     UVW(3)
	     REAL*8     PA_USED
	     REAL*8     DECOR_TIM
	     REAL*4     ELEV(2)
	     REAL*4     AZ(2)
             INTEGER*4  TIM_BEG_IND
	     INTEGER*4  NUM_EPC(PIM__MUVS)
             INTEGER*4  TIM_END_IND
	     INTEGER*4  NUM_AP_SPAN(PIM__MUVS)
	     INTEGER*2  STA_IND(2)
	     INTEGER*2  SOU_IND
	     INTEGER*2  ROOT_SOU_IND
	     INTEGER*2  SCA_IND
	     INTEGER*2  TSYS_IND(2,PIM__MFRG)
	     INTEGER*2  PCAL_IND(4,2,PIM__MFRG)
	     INTEGER*2  STMO_IND(2)
	     CHARACTER  POLARIZ(2)*2
	     CHARACTER  POLAR_USED*2
	     INTEGER*4  MOD_IND_BEG(2)
	     INTEGER*4  MOD_IND_END(2)
	     INTEGER*4  NUVS
	     INTEGER*4  GLO_FRG_INDS(PIM__MUVS)
	     INTEGER*4  REF_FRG_INDS(PIM__MUVS)
!
	     INTEGER*4, POINTER :: UV_IND(:,:)     => NULL() ! UV indices
	     INTEGER*4, POINTER :: CORR_FLAG(:,:)  => NULL() ! Flag set by the correlator
!
! ---------- These parameters are not stored. They are ephemeris
!
	     COMPLEX*8, POINTER :: RES_FRN(:,:)        => NULL() ! Uncalibrated postfit residuals
	     REAL*4,    POINTER :: USER_FLAG(:)        => NULL() ! User-supplied time dependent weights
	     COMPLEX*8, POINTER :: UV(:,:,:,:)         => NULL() ! Visibility data
	     COMPLEX*8, POINTER :: UV_IF(:,:,:)        => NULL() ! Visibility data averaged over the IF
	     COMPLEX*8, POINTER :: UV_BAND(:,:)        => NULL() ! Visibility data averaged over the band
	     COMPLEX*8, POINTER :: AC(:,:,:,:,:)       => NULL() ! Autocorrelation data
	     REAL*4,    POINTER :: AC_AVR_TIM(:,:,:,:) => NULL() ! Autocorrelation data averaged over time PIM%NCHN,LFRQ,2,PIM%NSTK
	     REAL*4,    POINTER :: AC_MEAN(:,:,:)      => NULL() ! Autocorrelation data averaged over time after sampling correction LFRQ,2,PIM%NSTK
	     REAL*4,    POINTER :: WEI_1D(:,:)         => NULL() ! 1-dimension weights
	     REAL*4,    POINTER :: WVR_DELAY(:)        => NULL() ! WVR delay
 	     REAL*4,    POINTER :: TSRF(:,:,:)         => NULL() ! Tsys Renormalization factor to account for discarding spactral channels LFRQ,2,PIM%NSTK
	     INTEGER*4  WVR_FLAG
	END TYPE  PIM_OBS__TYPE
!
	TYPE      PIM_SCA__TYPE
             INTEGER*4  TIM_IND
	     INTEGER*4  NUM_EPC
	     INTEGER*2  SOU_IND
	     INTEGER*4  NSTA
	     INTEGER*4  NBAS
	     INTEGER*4  UNSRT_ID
	     INTEGER*4  IND_ROOT
	     CHARACTER  SCAN_NAME*10
!
	     INTEGER*4, ALLOCATABLE :: OBS_IND(:)
	     INTEGER*4, ALLOCATABLE :: AUT_IND(:)
	END TYPE  PIM_SCA__TYPE
!
	TYPE      PIM_FRQ__TYPE
	     REAL*8     FREQ
	     INTEGER*8  FREQ_I8
	     REAL*8     BAND_WIDTH
	     REAL*8     CHAN_WIDTH
	     INTEGER*4  FRQ_GRP
	     INTEGER*4  SIDE_BAND
	     INTEGER*4  BB_SP_CHAN_IND
	END TYPE  PIM_FRQ__TYPE
!
	TYPE      PIM_BASBPS__TYPE
	     REAL*8     MAX_AMPL
	     REAL*8     INTG_AMPL
	     REAL*8     SNR_MIN
	     REAL*8     AMP_RMS_TOT
	     REAL*8     PHS_RMS_TOT
	     INTEGER*4  L_CHN
	     INTEGER*4  L_FRQ
	     INTEGER*4  L_OBS
	     CHARACTER  STA(2)*8
	     REAL*8,    POINTER :: AMP_RMS_FRQ(:)  => NULL()
	     REAL*8,    POINTER :: PHS_RMS_FRQ(:)  => NULL()
	     REAL*8,    POINTER :: FREQ(:,:)       => NULL()
	     COMPLEX*8, POINTER :: SIGNAL(:,:)     => NULL()
	END TYPE  PIM_BASBPS__TYPE
!
	TYPE      PIM_BPASS__TYPE
	     REAL*8     INTG_AMPL
	     REAL*8     SNR_MIN
	     REAL*8     AMP_RMS_TOT
	     REAL*8     PHS_RMS_TOT
	     REAL*4     BPS_MB_GRDEL
	     INTEGER*4  L_CHN
	     INTEGER*4  L_FRQ
	     INTEGER*4  L_OBS
	     INTEGER*4  IND_STA_REF
	     INTEGER*4  PIMA_VERS
	     CHARACTER  STATUS*8
	     CHARACTER  FINAM*128
	     CHARACTER  STA_NAM*8
	     REAL*4     PCAL_MB_GRDEL
	     REAL*4     PCAL_SB_GRDEL(PIM__MFRQ)
	     REAL*8,    POINTER :: AMP_RMS_FRQ(:)   => NULL()
	     REAL*8,    POINTER :: PHS_RMS_FRQ(:)   => NULL()
	     REAL*8,    POINTER :: FREQ(:,:)        => NULL()
	     COMPLEX*8, POINTER :: BPS(:,:)         => NULL()
	     REAL*4,    POINTER :: PHS_RATE(:)      => NULL()
	     REAL*8,    POINTER :: SPLT_NRML_FRQ(:) => NULL()
	END TYPE  PIM_BPASS__TYPE
!
        TYPE      PIM_BPC__TYPE
	     INTEGER*4  L_BAS
	     INTEGER*4  L_FIL
	     INTEGER*4  L_FRI
	     INTEGER*4  L_OBS_TOT(PIM__MBAS)
	     INTEGER*4  L_OBS_BAS(PIM__MBAS,PIM__MFRI)
             INTEGER*4  L_OBS_FRI(PIM__MBAS,PIM__MFRI)
             INTEGER*4  L_FIL_FRI(PIM__MBAS,PIM__MFRI)
	     CHARACTER  FIL_CNT(PIM__MFRI)*256
	     CHARACTER  FIL_FRI(PIM__MFRI)*256
	     CHARACTER  FIL_PIM(PIM__MFRI)*256
	     REAL*4     SNR(PIM__MBAS,PIM__MFRI)
	     REAL*4     AMPL(PIM__MBAS,PIM__MFRI)
	     REAL*8     GR_DEL(PIM__MBAS,PIM__MFRI)
	     REAL*8     PH_RAT(PIM__MBAS,PIM__MFRI)
	     REAL*8     GR_RAT(PIM__MBAS,PIM__MFRI)
	     REAL*4     AV_AMPL(PIM__MBAS,PIM__MFRI)
	     REAL*4     AV_PHAS(PIM__MBAS,PIM__MFRI)
	     TYPE ( PIM_BASBPS__TYPE ) :: FRINGE(PIM__MBAS,PIM__MFRI)
	     CHARACTER  PHAS_CAL_CODE*8
	     CHARACTER  C_BAS(PIM__MBAS)*17
        END TYPE  PIM_BPC__TYPE
!
        TYPE      PIM_BPS_STA__TYPE
	     INTEGER*4  IND_STA_REF
	     REAL*8     SNR(PIM__MSCA,PIM__MSTA,PIM__MPLR)
	     REAL*8     TIME_FRT(PIM__MSCA,PIM__MSTA,PIM__MPLR)
	     REAL*8     GR_DEL(PIM__MSCA,PIM__MSTA,PIM__MPLR)
	     REAL*8     PH_RAT(PIM__MSCA,PIM__MSTA,PIM__MPLR)
	     REAL*8     GR_RAT(PIM__MSCA,PIM__MSTA,PIM__MPLR)
	     REAL*8     AMPL(PIM__MSCA,PIM__MSTA,PIM__MPLR)
	     REAL*8     PHS(PIM__MSCA,PIM__MSTA,PIM__MPLR)
	     INTEGER*4  PCI(PIM__MSTA)
	     INTEGER*4  NUM_OBS_ACCUM(PIM__MSTA,PIM__MPLR)
	     INTEGER*4  NUM_OBS_FINE(PIM__MSTA,PIM__MPLR)
	     INTEGER*4  IND_OBS_SEL(PIM__MSCA,PIM__MSTA,PIM__MPLR)
	     INTEGER*4  IND_OBS_POL(PIM__MSCA,PIM__MSTA,PIM__MPLR)
	     CHARACTER  FIL_CNT*128
	     REAL*4     AMPL_TOT_RMS(PIM__MSTA)
	     REAL*4     PHAS_TOT_RMS(PIM__MSTA)
	     REAL*4     AMPL_INTEGRAL(PIM__MSTA)
	     CHARACTER  PHAS_CAL_CODE*8
	     INTEGER*4  IFRG
	     INTEGER*4  PIMA_VERS
	     CHARACTER  POLAR*8
	     CHARACTER  FINAM*128
	     REAL*4     PCAL_MB_GRDEL(PIM__MSTA)
	     REAL*4     PCAL_SB_GRDEL(PIM__MFRQ,PIM__MSTA)
!
	     REAL*8     INIT_GR_DEL(PIM__MSTA,PIM__MPLR)
	     REAL*8     INIT_GR_RAT(PIM__MSTA,PIM__MPLR)
	     REAL*8     INIT_PH_RAT(PIM__MSTA,PIM__MPLR)
	     REAL*8     INIT_PHS(PIM__MSTA,PIM__MPLR)
	     INTEGER*4  INIT_PLR(PIM__MSTA)
!
	     CHARACTER  STATUS*8
	     COMPLEX*8, POINTER :: CMPL(:,:,:)        => NULL()
	     REAL*4,    POINTER :: AMPL_FRQ_AVR(:,:)  => NULL()
	     REAL*4,    POINTER :: PHAS_FRQ_AVR(:,:)  => NULL()
	     REAL*4,    POINTER :: AMPL_FRQ_RMS(:,:)  => NULL()
	     REAL*4,    POINTER :: PHAS_FRQ_RMS(:,:)  => NULL()
	     REAL*4,    POINTER :: PHAS_FRQ_RATE(:,:) => NULL()
        END TYPE  PIM_BPS_STA__TYPE
!
        TYPE      PIM_PBP_STA__TYPE
	     INTEGER*4  IND_STA_REF
	     INTEGER*4  PIMA_VERS
	     CHARACTER  TYP*14
	     CHARACTER  FILLER*2
	     CHARACTER  FINAM*128
	     CHARACTER  STATUS*8
             COMPLEX*8, POINTER :: CMPL(:,:)        => NULL()
	     REAL*8,    POINTER :: SPLT_NRML_FRQ(:) => NULL()
        END TYPE  PIM_PBP_STA__TYPE
        TYPE       PIMA_DPC__TYPE
             INTEGER*4  NPC
             INTEGER*4  NFREQ
             CHARACTER  STA_NAM*2
             CHARACTER  FILLER*6
             REAL*8,    POINTER :: FREQ(:,:)      => NULL()
             COMPLEX*8, POINTER :: PCAL(:,:)      => NULL()
             REAL*8,    POINTER :: TIM_DOY_BEG(:) => NULL()
             REAL*8,    POINTER :: TIM_AVR(:)     => NULL()
        END TYPE   PIMA_DPC__TYPE
        TYPE     PIM_FRIP_OBS__TYPE
             INTEGER*4  IND_OBS
             INTEGER*4  NAP
             INTEGER*4  IND_AP_BEG
             INTEGER*4  IND_AP_END
	     REAL*8     UVW_SRT(3)
	     INTEGER*4  VIS_STATUS
             REAL*8,    POINTER :: TIM_AP(:)  => NULL()
	     REAL*4,    POINTER :: UVW(:,:,:) => NULL()
	     REAL*4,    POINTER :: WEI(:,:)   => NULL()
             COMPLEX*8, POINTER :: VIS(:,:)   => NULL()
        END TYPE PIM_FRIP_OBS__TYPE
        TYPE     PIM_FRIP__TYPE
	     INTEGER*4  IND_SCA
             INTEGER*4  NOBS
             INTEGER*4  NFRQ
	     INTEGER*4  NAF
	     INTEGER*4  FRQ_MSEG
	     INTEGER*4  TIM_MSEG
	     INTEGER*4  L_STA
	     CHARACTER  C_STA(PIM__MSTA)*8
	     INTEGER*4  IND_STA_REF
	     REAL*8     TIM_EPC
	     COMPLEX*8  GAIN(PIM__MCHN,PIM__MSTA)
	     CHARACTER  MOD_FILE*128
	     INTEGER*4  AF_STATUS
	     INTEGER*4  MOD_STATUS
	     INTEGER*4  MAP_STATUS
             REAL*8,    POINTER :: FRQ(:)        => NULL()
             REAL*8,    POINTER :: CFRQ_REF(:,:) => NULL()
	     REAL*4,    POINTER :: FREQ_AF(:)    => NULL()
	     REAL*4,    POINTER :: WEI_AF(:,:)   => NULL()
             REAL*4,    POINTER :: UVW_AF(:,:,:) => NULL()
	     INTEGER*4, POINTER :: IND_STA(:,:)  => NULL()
	     INTEGER*4, POINTER :: IAMB_AF(:,:)  => NULL()
             COMPLEX*8, POINTER :: VIS_AF(:,:)   => NULL()
	     LOGICAL*1, POINTER :: USED(:)       => NULL()
             COMPLEX*8, POINTER :: MAP(:,:) => NULL()
             COMPLEX*8, POINTER :: MOD(:,:) => NULL()
             TYPE    ( PIM_FRIP_OBS__TYPE ), POINTER  :: OBS(:) => NULL()
        END TYPE PIM_FRIP__TYPE
!
        TYPE     CODA_IFMOD__HEADER
            CHARACTER  VERSION*4
            CHARACTER  EXP_NAME*32
            INTEGER*4  NPOLY
            INTEGER*4  VERS
            INTEGER*4  NSS
            INTEGER*4  NDAT
            INTEGER*4  IND_ANT
            INTEGER*4  STA_IND
        END TYPE CODA_IFMOD__HEADER
!
        TYPE     CODA_IFMOD__DATA
           REAL*8     MJD_R8
           REAL*8     PP
           CHARACTER  SOU_NAM*32
           INTEGER*4  ANT_NUM
           REAL*8     FAR_ROT
           REAL*8     PDELAY
           REAL*8     GDELAY
           REAL*8     PRATE
           REAL*8     GRATE
           REAL*8     DDELAY
           REAL*8     DRATE
           REAL*8     P2ND
           REAL*8     G2ND
           REAL*8     P3RD
           REAL*8     G3RD
           REAL*8     P4TH
        END TYPE     CODA_IFMOD__DATA
!
        INTEGER*4  SFXC__IM_FILE_HEADER_LEN, &
     &             SFXC__IM_OBS_HEADER_LEN,  &
     &             SFXC__IM_OBS_RECORD_LEN
        PARAMETER  ( SFXC__IM_FILE_HEADER_LEN =  15 )
        PARAMETER  ( SFXC__IM_OBS_HEADER_LEN  = 166 )
        PARAMETER  ( SFXC__IM_OBS_RECORD_LEN  =  56 )
!
        TYPE       SFXC__IM_FILE_HEADER_TYPE
              INTEGER*4  MAGIC_I4(3)
              CHARACTER  STA_NAM*2
        END TYPE   SFXC__IM_FILE_HEADER_TYPE
!
        TYPE       SFXC__IM_OBS_HEADER_TYPE
              CHARACTER  SCA_NAM*80
              CHARACTER  SOU_NAM*80
              INTEGER*4  MJD
        END TYPE   SFXC__IM_OBS_HEADER_TYPE
!
        TYPE       SFXC__IM_OBS_RECORD_TYPE
              REAL*8     UTC
              REAL*8     UVW(3)
              REAL*8     DEL
              REAL*8     FILLER1
              REAL*8     TIM_STEP
        END TYPE   SFXC__IM_OBS_RECORD_TYPE
!
        TYPE     PIMA__SFXC_TYPE 
           CHARACTER  STA_NAM*8
           CHARACTER  SOU_NAM*8
           REAL*8     TAI
           REAL*8     DEL
           INTEGER*4  MJD
           INTEGER*4  FIL_DEL_IND
        END TYPE PIMA__SFXC_TYPE       
!
        TYPE     PIMA__KJCC_TYPE 
           CHARACTER  STA_NAM*8
           CHARACTER  SOU_NAM*8
           REAL*8     TAI
           REAL*8     DEL(0:PIM__MDPL)
           REAL*8     CLO(0:1)
           REAL*8     ATM(0:1)
           INTEGER*4  MJD
        END TYPE PIMA__KJCC_TYPE       
!
        TYPE ANT_NAME_TYPE
             INTEGER*4  N_ARR
             CHARACTER, POINTER :: NAM(:)*8 => NULL()
             INTEGER*4, POINTER :: REF(:)   => NULL()
        END TYPE ANT_NAME_TYPE
!
        TYPE PIM_SUB__TYPE
	     INTEGER*4  IND_SOU
	     INTEGER*4  L_STA(PIM__MSUB)
	     INTEGER*4  LIS_STA(PIM__MSTA,PIM__MSUB)
             INTEGER*4, POINTER :: OBS_IND_SUB(:)  => NULL()
	     REAL*8,    POINTER :: TIM_SRT(:,:)    => NULL()
	     INTEGER*4  STATUS
        END TYPE PIM_SUB__TYPE
!
        TYPE      PIM_GACO__TYPE
	     INTEGER*4  NFRQ
	     REAL*4     GAIN_CORR(PIM__MFRQ)
	     REAL*4     GACO_ERR(PIM__MFRQ)
	     REAL*8     GACO_FRQ(PIM__MFRQ)
	     INTEGER*4  IND_FREQ(PIM__MFRQ)   ! Index in PIM%FRQ table
	     INTEGER*4  NVIS(PIM__MFRQ)
        END  TYPE PIM_GACO__TYPE
!
	TYPE  PIMA__TYPE
	      CHARACTER  CONF_FILE*128
	      INTEGER*4  L_FIL
	      REAL*8     TIM_R8(PIM__MEPC)
	      INTEGER*8  TIM_I8(PIM__MEPC)
	      INTEGER*4  REF_STA(PIM__MSTA,PIM__MFIL)
	      INTEGER*4  REF_SOU(PIM__MSOU,PIM__MFIL)
	      CHARACTER  C_STA(PIM__MSTA)*8
	      CHARACTER  C_SOU(PIM__MSOU)*8
	      REAL*8     REF_FREQ
	      REAL*8     CHAN_BW
	      REAL*8     UTC_MTAI
	      REAL*8     REF_PIXL
	      REAL*8     VIS_SCAL
	      REAL*4     AP_LEN_MIN
	      REAL*4     AP_LEN_MAX
	      INTEGER*4  NSTA
	      INTEGER*4  NSOU
	      INTEGER*4  NFRQ
	      INTEGER*4  NFRG
	      INTEGER*4  NCHN
	      INTEGER*4  NEPC
	      INTEGER*4  NSCA
	      INTEGER*4  NOBS
	      INTEGER*4  NAUT
	      INTEGER*4  NUV
	      INTEGER*4  NMOD
	      INTEGER*4  NBND
	      INTEGER*4  NPOL
	      INTEGER*4  NSTK
	      INTEGER*4  STK_1
	      INTEGER*4  NPCT
!
	      INTEGER*4  MAX_EPC_OBS
	      INTEGER*4  MJD_0
	      REAL*8     TAI_0
	      CHARACTER  OBS_CODE*16
	      INTEGER*4  NUV_EXC
	      CHARACTER  GENERATOR*32
	      CHARACTER  CORR_NAME*8
	      CHARACTER  CORR_VERS*16
              INTEGER*4  TIM_SCL
	      LOGICAL*1  USE_OBS(PIM__MOBS)
	      INTEGER*4  BANDPASS_MASK_STYLE
	      INTEGER*4  PCAL_MASK_STATUS
	      INTEGER*4  NLEV(PIM__MSTA)
	      CHARACTER  TAPER_FN*8
	      INTEGER*4  ZERO_PAD
	      INTEGER*4  OVERSAMPLE
	      INTEGER*4  FFT_SIZE
!
	      INTEGER*4  REF_FRQ(PIM__MFRQ,PIM__MFRG) ! Cross reference frequency table from original to merged
	      INTEGER*4  REV_FRQ(PIM__MFRQ) ! Reverse cross reference frequency table:       from merged to original
	      INTEGER*4  REV_FRG(PIM__MFRQ) ! Reverse cross reference frequency group table: from merged to original
	      INTEGER*4  FRG_USE            ! Usage type of frequency group table
	      INTEGER*4  VIRT_NFRG          ! Number of virtual frequency groups
!
	      INTEGER*4  STATUS
!
	      INTEGER*4  L_MKDB
	      INTEGER*4  L_STA_BPASS
	      INTEGER*4  L_STA_PBP
	      INTEGER*4  L_BASBPS
	      INTEGER*4  REF_BASBPS(PIM__MSTA,PIM__MSTA)
	      INTEGER*4  L_SUB
!
	      REAL*8,    POINTER            :: TSYS(:,:,:)             => NULL()
              TYPE     ( PIM_SCADB__TYPE  ) :: SCADB(PIM__MSCA)
	      TYPE     ( PIM_CONF__TYPE   ) :: CONF
	      TYPE     ( PIM_SOU__TYPE    ) :: SOU(PIM__MSOU)
	      TYPE     ( PIM_STA__TYPE    ) :: STA(PIM__MSTA)
	      TYPE     ( PIM_SCA__TYPE    ) :: SCA(PIM__MSCA)
	      TYPE     ( PIM_FRQ__TYPE    ) :: FRQ(PIM__MFRQ,PIM__MFRG)
!
	      TYPE     ( PIM_FIL__TYPE    ),   POINTER :: FILE(:)      => NULL()
	      TYPE     ( PIM_UVIND__TYPE  ),   POINTER :: UV_IND(:)    => NULL()
	      TYPE     ( PIM_OBS__TYPE    ),   POINTER :: AUT(:)       => NULL()
	      TYPE     ( PIM_OBS__TYPE    ),   POINTER :: OBS(:)       => NULL()
	      TYPE     ( PIM_BPASS__TYPE  ),   POINTER :: BPASS(:)     => NULL()
              TYPE     ( PIM_PBP_STA__TYPE ),  POINTER :: PBP(:)       => NULL()
              TYPE     ( PIM_BPS_STA__TYPE ),  POINTER :: BPS          => NULL()
	      COMPLEX*8,            POINTER :: PBP_INIT_CMPL(:,:,:,:)  => NULL()
	      COMPLEX*8,            POINTER :: PBP_ACCUM_CMPL(:,:,:,:) => NULL()
	      COMPLEX*8,            POINTER :: PBP_FINE_CMPL(:,:,:,:)  => NULL()
	      INTEGER*4, POINTER :: UV_EXC(:)                          => NULL()
	      INTEGER*1, POINTER :: BANDPASS_MASK(:,:,:,:)             => NULL()
	      INTEGER*1, POINTER :: PCAL_MASK(:,:,:)                   => NULL()
	      REAL*8,    POINTER :: FREQ_ARR(:,:,:)                    => NULL()
              TYPE     ( PIM_FRIP__TYPE   ) :: FRIP(2)
	      TYPE     ( PIM_NZO__TYPE    ) :: NZO
	      TYPE     ( PIM_SUB__TYPE    ) :: SUB
	      TYPE     ( PIM_GACO__TYPE   ) :: GACO(PIM__MSTA)
	      INTEGER*4  FRI_STATUS
	      INTEGER*4  WVR_STATUS
	      INTEGER*4  THE_STATUS
	      INTEGER*4  GACO_STATUS
	      INTEGER*4  LAST_FIELD
	END TYPE  PIMA__TYPE
!
	INTEGER*4    PIMA__UNDF, PIMA__ALCT, PIMA__ALL, PIMA__NO, &
     &	             PIMA__COMBINED, PIMA__SEPARATE, PIMA__BIT_INIT, &
                     PIMA__BIT_EXOB
	PARAMETER  ( PIMA__UNDF = 0          )
	PARAMETER  ( PIMA__ALCT = 1712830961 )
	PARAMETER  ( PIMA__ALL  = 1810923984 )
	PARAMETER  ( PIMA__NO   = 1928349051 )
	PARAMETER  ( PIMA__COMBINED = 1203728177 )
	PARAMETER  ( PIMA__SEPARATE = 1492831025 )
	PARAMETER  ( PIMA__BIT_INIT = 0 )
	PARAMETER  ( PIMA__BIT_EXOB = 0 )
	INTEGER*4    PIMA__MKEY, PIMA__FRIB_KEY, PIMA__FRIP_KEY, &
     &               PIMA__MKDB_KEY, PIMA__SPLT_KEY, PIMA__BPS_KEY, &
     &               PIMA__ONOF_KEY
	PARAMETER  ( PIMA__MKEY     = 54 )
	PARAMETER  ( PIMA__FRIB_KEY = 23 )
	PARAMETER  ( PIMA__FRIP_KEY = 22 )
	PARAMETER  ( PIMA__MKDB_KEY = 10 )
	PARAMETER  ( PIMA__SPLT_KEY = 13 )
	PARAMETER  ( PIMA__BPS_KEY  = 17 )
	PARAMETER  ( PIMA__ONOF_KEY =  7 )
        REAL*4       PIMA__BPS_AMP_POLY_MIN, PIMA__BPS_AMP_SPLN_MIN, &
     &               PIMA__BPASS_AMP_MAX, PIMA__AMP_MIN, PIMA__AMP_MAX, &
     &               PIMA__ACC_MIN, PIMA__ACC_MAX
	PARAMETER  ( PIMA__BPS_AMP_POLY_MIN =  0.1    )
	PARAMETER  ( PIMA__BPS_AMP_SPLN_MIN =  0.002  )
	PARAMETER  ( PIMA__BPASS_AMP_MAX    =  9.9999 )
	PARAMETER  ( PIMA__AMP_MIN          =  1.0E-8 )
	PARAMETER  ( PIMA__AMP_MAX          =  1.0E8  )
	PARAMETER  ( PIMA__ACC_MIN          =  1.0E-5 )
	PARAMETER  ( PIMA__ACC_MAX          =  1.0E8  )
!
	INTEGER*4    FFT_MKL
	PARAMETER  ( FFT_MKL = 17223291 )
	INTEGER*4    PIMA__AUTO_1, PIMA__AUTO_2, PIMA__SHORT, PIMA__NO_SCA, &
     &              PIMA__NO_TIM, PIMA__EXCL, PIMA__DFAP, PIMA__RVBS, &
     &              PIMA__NO_SOUID, PIMA__CHAIN
	PARAMETER  ( PIMA__AUTO_1 = -1000000001 )
	PARAMETER  ( PIMA__AUTO_2 = -1000000002 )
	PARAMETER  ( PIMA__SHORT  = -1000000003 )
	PARAMETER  ( PIMA__NO_SCA = -1000000004 )
	PARAMETER  ( PIMA__NO_TIM = -1000000005 )
	PARAMETER  ( PIMA__EXCL   = -1000000006 )
	PARAMETER  ( PIMA__DFAP   = -1000000007 )
	PARAMETER  ( PIMA__RVBS   = -1000000008 )
	PARAMETER  ( PIMA__NO_SOUID = -1000000009 )
	PARAMETER  ( PIMA__CHAIN  = -1000000010 )
!
! ----- Codes algorithms for ambiguity resolution
!
        INTEGER*4    ARA1__PHD, ARA2__PHD, ARA3__PHD
        PARAMETER  ( ARA1__PHD = 801 )
        PARAMETER  ( ARA2__PHD = 802 )
        PARAMETER  ( ARA3__PHD = 803 )
!
! ----- Codes of the bandpass masks
!
	INTEGER*4  PIMA__COARSE, PIMA__FINE, PIMA__BOTH, PIMA__BFS, PIMA__MASK_AUTC, &
     &             PIMA__MASK_BPAS, PIMA__MASK_FRNG, PIMA__MASK_SPLT, PIMA__MASKS
        PARAMETER  ( PIMA__COARSE = 1  ) ! For compatibility 
	PARAMETER  ( PIMA__FINE   = 2  ) ! For compatibility 
	PARAMETER  ( PIMA__BFS    = 32 ) ! bandpass, frine split
	PARAMETER  ( PIMA__BOTH   = 33 ) ! For compatibility 
	PARAMETER  ( PIMA__MASK_AUTC   = 1 )
	PARAMETER  ( PIMA__MASK_BPAS   = 2 )
	PARAMETER  ( PIMA__MASK_FRNG   = 3 )
	PARAMETER  ( PIMA__MASK_SPLT   = 4 )
	PARAMETER  ( PIMA__MASKS       = 4 )
!
	INTEGER*4  PIMA__UTC
	INTEGER*4  PIMA__TAI
	INTEGER*4  PIMA__TDT
	PARAMETER  ( PIMA__UTC = 23101 )
	PARAMETER  ( PIMA__TAI = 23102 )
	PARAMETER  ( PIMA__TDT = 23103 )
!
        CHARACTER    PIMA__PCPL_CODE*8,        PIMA__ACPL_CODE*8, &
       &             PIMA__SPLT_CODE*8,        PIMA__ACTA_CODE*8, &
       &             PIMA__BPLT_CODE*8,        PIMA__BMGE_CODE*8, &
       &             PIMA__MKDB_CODE*8,        PIMA__BPAS_CODE*8, &
       &             PIMA__PMGE_CODE*8,        PIMA__DIPC_CODE*8, &
       &             PIMA__LOAD_CODE*8,        PIMA__FRIB_CODE*8, &
       &             PIMA__GEAN_CODE*8,        PIMA__MOIM_CODE*8, &
       &             PIMA__OPAG_CODE*8,        PIMA__OPAL_CODE*8, &
       &             PIMA__PPLT_CODE*8,        PIMA__PDPL_CODE*8, &
       &             PIMA__MPPL_CODE*8,        PIMA__PRGA_CODE*8, &
       &             PIMA__ONOF_CODE*8,        PIMA__TECG_CODE*8, &
       &             PIMA__FRIP_CODE*8,        PIMA__TSPL_CODE*8, &
       &             PIMA__UPGR_CODE*8,        PIMA__GACO_CODE*8, &
     &               PIMA__FRTR_CODE*8,        PIMA__TSMO_CODE*8, &
     &               PIMA__GENA_CODE*8,        PIMA__GEPM_CODE*8, &
     &               PIMA__CLPC_CODE*8
!
	CHARACTER    PIMA__PCAL_NO*8,          PIMA__PCAL_USE*8,         &
       &             PIMA__PCAL_USE_ONE*8,     PIMA__PCAL_USE_ALL*8,     &
       &             PIMA__TSYS_NO*8,          PIMA__TSYS_INTRP*8,       &
       &             PIMA__TSYS_MEASURED*8,    PIMA__TSYS_CLEANED*8,     &
       &             PIMA__TSYS_MODELED*8,     PIMA__GAIN_NO*8,          &
       &             PIMA__GAIN_USE*8,         PIMA__SMPL_USE*8,         &
       &             PIMA__SMPL_NO*8,          PIMA__FRIB_YES*8,         &
       &             PIMA__FRIB_NO*8,          PIMA__FR2P_YES*8,         &
       &             PIMA__FR2P_NO*8,          PIMA__MD_RT*8,            &
       &             PIMA__SD_RT_MD*8,         PIMA__2FFT*8,             &
       &             PIMA__3FFT*8,             PIMA__BPASS_NO*8,         &
       &             PIMA__BPASS_AMP*8,        PIMA__BPASS_PHS*8,        &
       &             PIMA__BPASS_AMP_PHS*8,    PIMA__LSQ_REF_NO*8,       &
       &             PIMA__LSQ_REF_YES*8,      PIMA__PLOT_NO*8,          &
       &             PIMA__PLOT_GIF*8,         PIMA__PLOT_PS*8,          &
       &             PIMA__PLOT_XW*8,          PIMA__PLOT_TS*8,          &
       &             PIMA__PLOT_PC*8,          PIMA__PLOT_AC*8,          &
       &             PIMA__OBS_NO*8,           PIMA__OBS_ONE*8,          &
       &             PIMA__OBS_LIST*8,         PIMA__OBS_ALL*8,          &
       &             PIMA__SCA_NO*8,           PIMA__SCA_ONE*8,          &
       &             PIMA__SCA_LIST*8,         PIMA__SCA_ALL*8,          &
       &             PIMA__TSYS_EXT_NO*8,      PIMA__TSYS_EXT_VERA*8,    &
       &             PIMA__FINE_SEARCH_NO*8,   PIMA__FINE_SEARCH_PAR*8,  &
       &             PIMA__FINE_SEARCH_BIN*8,  PIMA__FINE_SEARCH_LSQ*8,  &
       &             PIMA__FINE_SEARCH_ACC*8,  PIMA__FINE_SEARCH_TEC*8,  &
       &             PIMA__ACCR_NO*8,          PIMA__ACCR_SQRT_MEA*8,    &
       &             PIMA__ACCR_SQRT_EVR*8,    PIMA__ACCR_SQRT_KOG*8,    &
       &             PIMA__ACCR_INTG*8,        PIMA__ERR_IGNORE*8,       &
       &             PIMA__ACCR_VLBA_CNST1*8,  PIMA__ACCR_VLBA_CNST2*8,  &
       &             PIMA__ERR_CORRECT*8,      PIMA__ERR_STOP*8,         &
       &             PIMA__BPASS_ALLOC*8,      PIMA__BPASS_LIST*8,       &
       &             PIMA__BPASS_INIT*8,       PIMA__BPASS_INSP*8,       &
       &             PIMA__BPASS_ACCUM*8,      PIMA__BPASS_FINE*8,       &
       &             PIMA__MKDB_MID_SCAN*8,    PIMA__MKDB_SRT_VAL*8,     &
       &             PIMA__MKDB_SRT_FRT*8,     PIMA__FRT_VAL*8,          &
       &             PIMA__MKDB_FILE*8,        PIMA__MKDB_TEXT*8,        &
       &             PIMA__MKDB_GVF*8,         PIMA__MKDB_AMPL*8,        &
       &             PIMA__FRA_DRF*8,          PIMA__FRA_MUL*8,          &
       &             PIMA__FRA_ADD*8,          PIMA__FRA_LSQ*8,          &
       &             PIMA__FRA_ACC*8,          PIMA__BPASS_SEFD_YES*8,   &
       &             PIMA__BPASS_SEFD_NO*8,    PIMA__FUDGE_NO*8,         &
       &             PIMA__FUDGE_MITA*8,       PIMA__FUDGE_DWIN*8,       &
       &             PIMA__FUDGE_VLBA*8,       PIMA__FUDGE_DIFX*8,       &
       &             PIMA__FUDGE_KOGAN*8,      PIMA__FUDGE_AIPS*8,       &
       &             PIMA__EXC_NO*8,           PIMA__EXC_AUTO*8,         &
       &             PIMA__POLAR_RR*8,         PIMA__PLOT_TABLE*8,       &
       &             PIMA__POLAR_LL*8,         PIMA__POLAR_RL*8,         &
       &             PIMA__POLAR_LR*8,         PIMA__POLAR_RPL*8,        &
       &             PIMA__POLAR_HH*8,         PIMA__POLAR_HV*8,         &
       &             PIMA__POLAR_VH*8,         PIMA__POLAR_VV*8,         &
       &             PIMA__POLAR_HR*8,         PIMA__POLAR_HL*8,         &
       &             PIMA__POLAR_VR*8,         PIMA__POLAR_VL*8,         &
       &             PIMA__POLAR_RH*8,         PIMA__POLAR_LH*8,         &
       &             PIMA__POLAR_RV*8,         PIMA__POLAR_LV*8,         &
       &             PIMA__POLAR_XX*8,         PIMA__POLAR_YY*8,         &
       &             PIMA__POLAR_XY*8,         PIMA__POLAR_YX*8,         &
       &             PIMA__POLAR_I*8,          PIMA__POLAR_Q*8,          &
       &             PIMA__POLAR_U*8,          PIMA__POLAR_V*8,          &
       &             PIMA__POLAR_PAR*8,        PIMA__POLAR_ALL*8,        &
       &             PIMA__POLAR_ORIG*8,       PIMA__POLAR_1ST*8,        &
       &             PIMA__POLAR_2ND*8,        PIMA__POLARCAL_NO*8,      &
      &              PIMA__FILTER_NO*8,        PIMA__ONLY_DET*8,         &
      &              PIMA__FRT_AUTO*8,         PIMA__FRT_FILE*8,         &
      &              PIMA__EDGE_AMP_USE*8,     PIMA__EDGE_AMP_NO*8,      &
      &              PIMA__FRIB_SUBS*8,        PIMA__NORML_NO*8,         &
      &              PIMA__NORML_BAND*8,       PIMA__NORML_IF*8,         &
      &              PIMA__NORML_IFACC*8,      PIMA__FRIP_NO*8,          &
      &              PIMA__MOD_NO*8,           PIMA__MOD_VERA1000*8,     &
      &              PIMA__MOD_VERA2000*8,     PIMA__MOD_SHANGHAI*8,     &
      &              PIMA__MOD_SFXC*8,         PIMA__MOD_KJCC*8,         &
      &              PIMA__INTERPLA*8,         PIMA__EARTH_OB*8,         &
      &              PIMA__STA_ORB*8,          PIMA__RA_PUSCH*8,         &
      &              PIMA__RA_GBT*8,           PIMA__RA_GB140*8,         &
      &              PIMA__MOIM*8,             PIMA__PLOT_TXT*8,         &
      &              PIMA__STA_BASED_NO*8,     PIMA__STA_BASED_YES*8,    &
      &              PIMA__STA_BASED_ALL*8,    PIMA__BEAM_NO*8,          &
      &              PIMA__BEAM_YES*8,         PIMA__LEGENDRE*8,         &
      &              PIMA__CHEBYSH*8,          PIMA__SPLINE*8,           &
      &              PIMA__LINEAR*8,           PIMA__AVERAGED*8,         &
      &              PIMA__WEIGHTED*8,         PIMA__SPLT_WEI_ONE*8,     &
      &              PIMA__SPLT_WEI_OBS_SNR*8, PIMA__SPLT_WEI_AUTO*8,    &
      &              PIMA__SPLT_WEI_OBS_RMS*8, PIMA__SPLT_WEI_SEG_RMS*8, &
      &              PIMA__PLOT_SAV*8,         PIMA__MEASURED*8,         &
      &              PIMA__NWM*8,              PIMA__OPACITY*8,          &
      &              PIMA__TREC*8,             PIMA__MODELED*8,          &
      &              PIMA__ONOF_CREATE*8,      PIMA__ONOF_UPDATE*8,      &
      &              PIMA__ONOF_NO*8,          PIMA__MPPL_OBS*8,         &
      &              PIMA__MPPL_STA*8,         PIMA__WVR_USE*8,          &
      &              PIMA__WVR_NO*8,           PIMA__WVR_SPLINE_3RD*8,   &
      &              PIMA__WVR_SPLINE_LIN*8,   PIMA__WVR_SPLINE_AVR*8,   &
      &              PIMA__SA_NO*8,            PIMA__SA_MIN*8,           &
      &              PIMA__SA_MAX*8,           PIMA__SA_ALL*8,           &
      &              PIMA__TOTAL_UV_YES*8,     PIMA__TOTAL_UV_NO*8,      &
      &              PIMA__ASIS*8,             PIMA__FRQTRA_NO*8,        &
      &              PIMA__TATM*8,             PIMA__TSMO*8,             &
      &              PIMA__TABL*8,             PIMA__TABMOD*8
!
        PARAMETER  ( PIMA__LOAD_CODE        = 'LOAD    ' )
        PARAMETER  ( PIMA__FRIB_CODE        = 'FRIB    ' )
        PARAMETER  ( PIMA__FRIP_CODE        = 'FRIP    ' )
        PARAMETER  ( PIMA__TSPL_CODE        = 'TSPL    ' )
        PARAMETER  ( PIMA__PCPL_CODE        = 'PCPL    ' )
        PARAMETER  ( PIMA__ACPL_CODE        = 'ACPL    ' )
        PARAMETER  ( PIMA__GEAN_CODE        = 'GEAN    ' )
        PARAMETER  ( PIMA__MOIM_CODE        = 'MOIM    ' )
        PARAMETER  ( PIMA__MKDB_CODE        = 'MKDB    ' )
        PARAMETER  ( PIMA__BPAS_CODE        = 'BPAS    ' )
        PARAMETER  ( PIMA__BPLT_CODE        = 'BPLT    ' )
        PARAMETER  ( PIMA__PPLT_CODE        = 'PPLT    ' )
        PARAMETER  ( PIMA__PDPL_CODE        = 'PDPL    ' )
        PARAMETER  ( PIMA__MPPL_CODE        = 'MPPL    ' )
        PARAMETER  ( PIMA__BMGE_CODE        = 'BMGE    ' )
        PARAMETER  ( PIMA__PMGE_CODE        = 'PMGE    ' )
        PARAMETER  ( PIMA__DIPC_CODE        = 'DIPC    ' )
        PARAMETER  ( PIMA__PRGA_CODE        = 'PRGA    ' )
        PARAMETER  ( PIMA__UPGR_CODE        = 'UPGR    ' )
        PARAMETER  ( PIMA__SPLT_CODE        = 'SPLT    ' )
        PARAMETER  ( PIMA__ACTA_CODE        = 'ACTA    ' )
        PARAMETER  ( PIMA__OPAG_CODE        = 'OPAG    ' )
        PARAMETER  ( PIMA__OPAL_CODE        = 'OPAL    ' )
        PARAMETER  ( PIMA__ONOF_CODE        = 'ONOF    ' )
        PARAMETER  ( PIMA__TECG_CODE        = 'TECG    ' )
        PARAMETER  ( PIMA__GACO_CODE        = 'GACO    ' )
        PARAMETER  ( PIMA__FRTR_CODE        = 'FRTR    ' )
        PARAMETER  ( PIMA__TSMO_CODE        = 'TSMO    ' )
        PARAMETER  ( PIMA__GENA_CODE        = 'GENA    ' )
        PARAMETER  ( PIMA__GEPM_CODE        = 'GEPM    ' )
        PARAMETER  ( PIMA__CLPC_CODE        = 'CLPC    ' )
        PARAMETER  ( PIMA__TSYS_NO          = 'NO      ' )
        PARAMETER  ( PIMA__TSYS_INTRP       = 'INTRP   ' )
        PARAMETER  ( PIMA__TSYS_MEASURED    = 'MEASURED' )
        PARAMETER  ( PIMA__TSYS_CLEANED     = 'CLEANED ' )
        PARAMETER  ( PIMA__TSYS_MODELED     = 'MODELED ' )
        PARAMETER  ( PIMA__GAIN_NO          = 'NO      ' )
        PARAMETER  ( PIMA__GAIN_USE         = 'USE     ' )
        PARAMETER  ( PIMA__PCAL_NO          = 'NO      ' )
        PARAMETER  ( PIMA__PCAL_USE         = 'USE     ' )
        PARAMETER  ( PIMA__PCAL_USE_ONE     = 'USE_ONE ' )
        PARAMETER  ( PIMA__PCAL_USE_ALL     = 'USE_ALL ' )
        PARAMETER  ( PIMA__SMPL_NO          = 'NO      ' )
	PARAMETER  ( PIMA__SMPL_USE         = 'USE     ' )
	PARAMETER  ( PIMA__FRIB_YES         = 'YES     ' )
	PARAMETER  ( PIMA__FRIB_NO          = 'NO      ' )
	PARAMETER  ( PIMA__FRIP_NO          = 'NO      ' )
	PARAMETER  ( PIMA__FR2P_YES         = 'YES     ' )
	PARAMETER  ( PIMA__FR2P_NO          = 'NO      ' )
	PARAMETER  ( PIMA__MD_RT            = 'MD_RT   ' )
	PARAMETER  ( PIMA__SD_RT_MD         = 'SD_RT_MD' )
	PARAMETER  ( PIMA__2FFT             = '2FFT    ' )
	PARAMETER  ( PIMA__3FFT             = '3FFT    ' )
	PARAMETER  ( PIMA__BPASS_NO         = 'NO      ' )
	PARAMETER  ( PIMA__BPASS_AMP        = 'AMP     ' )
	PARAMETER  ( PIMA__BPASS_PHS        = 'PHS     ' )
	PARAMETER  ( PIMA__BPASS_AMP_PHS    = 'AMP_PHS ' )
	PARAMETER  ( PIMA__LSQ_REF_NO       = 'NO      ' )
	PARAMETER  ( PIMA__LSQ_REF_YES      = 'YES     ' )
	PARAMETER  ( PIMA__PLOT_NO          = 'NO      ' )
	PARAMETER  ( PIMA__PLOT_GIF         = 'GIF     ' )
	PARAMETER  ( PIMA__PLOT_PS          = 'PS      ' )
	PARAMETER  ( PIMA__PLOT_XW          = 'XW      ' )
	PARAMETER  ( PIMA__PLOT_TXT         = 'TXT     ' )
	PARAMETER  ( PIMA__PLOT_SAV         = 'SAV     ' )
	PARAMETER  ( PIMA__OBS_NO           = 'NO      ' )
	PARAMETER  ( PIMA__OBS_ONE          = 'ONE     ' )
	PARAMETER  ( PIMA__OBS_LIST         = 'LIST    ' )
	PARAMETER  ( PIMA__OBS_ALL          = 'ALL     ' )
	PARAMETER  ( PIMA__SCA_NO           = 'NO      ' )
	PARAMETER  ( PIMA__SCA_ONE          = 'ONE     ' )
	PARAMETER  ( PIMA__SCA_LIST         = 'LIST    ' )
	PARAMETER  ( PIMA__SCA_ALL          = 'ALL     ' )
	PARAMETER  ( PIMA__PLOT_TS          = 'PLOT_TS ' )
	PARAMETER  ( PIMA__PLOT_PC          = 'PLOT_PC ' )
	PARAMETER  ( PIMA__PLOT_AC          = 'PLOT_AC ' )
	PARAMETER  ( PIMA__TSYS_EXT_NO      = 'NO      ' )
	PARAMETER  ( PIMA__TSYS_EXT_VERA    = 'VERA    ' )
	PARAMETER  ( PIMA__FINE_SEARCH_NO   = 'NO      ' )
	PARAMETER  ( PIMA__FINE_SEARCH_PAR  = 'PAR     ' )
	PARAMETER  ( PIMA__FINE_SEARCH_BIN  = 'BIN     ' )
	PARAMETER  ( PIMA__FINE_SEARCH_LSQ  = 'LSQ     ' )
	PARAMETER  ( PIMA__FINE_SEARCH_ACC  = 'ACC     ' )
	PARAMETER  ( PIMA__FINE_SEARCH_TEC  = 'TEC     ' )
	PARAMETER  ( PIMA__ACCR_NO          = 'NO      ' )
	PARAMETER  ( PIMA__ACCR_SQRT_MEA    = 'SQRT_MEA' )
	PARAMETER  ( PIMA__ACCR_SQRT_EVR    = 'SQRT_EVR' )
	PARAMETER  ( PIMA__ACCR_SQRT_KOG    = 'SQRT_KOG' )
	PARAMETER  ( PIMA__ACCR_INTG        = 'INTG    ' )
	PARAMETER  ( PIMA__ACCR_VLBA_CNST1  = 'CNST1   ' )
	PARAMETER  ( PIMA__ACCR_VLBA_CNST2  = 'CNST2   ' )
	PARAMETER  ( PIMA__ERR_IGNORE       = 'IGNORE  ' )
	PARAMETER  ( PIMA__ERR_CORRECT      = 'CORRECT ' )
	PARAMETER  ( PIMA__ERR_STOP         = 'STOP    ' )
	PARAMETER  ( PIMA__BPASS_ALLOC      = 'ALLOC   ' )
	PARAMETER  ( PIMA__BPASS_LIST       = 'LIST    ' )
	PARAMETER  ( PIMA__BPASS_INIT       = 'INIT    ' )
	PARAMETER  ( PIMA__BPASS_INSP       = 'INSP    ' )
	PARAMETER  ( PIMA__BPASS_ACCUM      = 'ACCUM   ' )
	PARAMETER  ( PIMA__BPASS_FINE       = 'FINE    ' )
	PARAMETER  ( PIMA__BPASS_SEFD_YES   = 'YES     ' )
	PARAMETER  ( PIMA__BPASS_SEFD_NO    = 'NO      ' )
        PARAMETER  ( PIMA__FRT_VAL          = 'FRT_VAL ' )
        PARAMETER  ( PIMA__MKDB_MID_SCAN    = 'MID_SCAN' )
        PARAMETER  ( PIMA__MKDB_SRT_FRT     = 'SRT_FRT ' )
        PARAMETER  ( PIMA__MKDB_SRT_VAL     = 'SRT_VAL ' )
        PARAMETER  ( PIMA__MKDB_FILE        = 'FILE    ' )
        PARAMETER  ( PIMA__MKDB_TEXT        = 'TEXT    ' )
        PARAMETER  ( PIMA__MKDB_GVF         = 'GVF     ' )
        PARAMETER  ( PIMA__MKDB_AMPL        = 'AMPL    ' )
        PARAMETER  ( PIMA__FRA_DRF          = 'DRF     ' )
        PARAMETER  ( PIMA__FRA_LSQ          = 'LSQ     ' )
        PARAMETER  ( PIMA__FRA_ACC          = 'ACC     ' )
        PARAMETER  ( PIMA__FRA_MUL          = 'MUL     ' )
        PARAMETER  ( PIMA__FRA_ADD          = 'ADD     ' )
        PARAMETER  ( PIMA__FUDGE_NO         = 'NO      ' )
        PARAMETER  ( PIMA__FUDGE_MITA       = 'MITA    ' )
        PARAMETER  ( PIMA__FUDGE_DWIN       = 'DWIN    ' )
        PARAMETER  ( PIMA__FUDGE_VLBA       = 'VLBA    ' )
        PARAMETER  ( PIMA__FUDGE_DIFX       = 'DIFX    ' )
        PARAMETER  ( PIMA__FUDGE_KOGAN      = 'KOGAN   ' )
        PARAMETER  ( PIMA__FUDGE_AIPS       = 'AIPS    ' )
        PARAMETER  ( PIMA__EXC_AUTO         = 'AUTO    ' )
        PARAMETER  ( PIMA__EXC_NO           = 'NO      ' )
        PARAMETER  ( PIMA__POLAR_RR         = 'RR      ' )
        PARAMETER  ( PIMA__POLAR_LL         = 'LL      ' )
        PARAMETER  ( PIMA__POLAR_RL         = 'RL      ' )
        PARAMETER  ( PIMA__POLAR_LR         = 'LR      ' )
        PARAMETER  ( PIMA__POLAR_HH         = 'HH      ' )
        PARAMETER  ( PIMA__POLAR_HV         = 'HV      ' )
        PARAMETER  ( PIMA__POLAR_VH         = 'VH      ' )
        PARAMETER  ( PIMA__POLAR_VV         = 'VV      ' )
        PARAMETER  ( PIMA__POLAR_HR         = 'HR      ' )
        PARAMETER  ( PIMA__POLAR_HL         = 'HL      ' )
        PARAMETER  ( PIMA__POLAR_VR         = 'VR      ' )
        PARAMETER  ( PIMA__POLAR_VL         = 'VL      ' )
        PARAMETER  ( PIMA__POLAR_RH         = 'RH      ' )
        PARAMETER  ( PIMA__POLAR_LH         = 'LH      ' )
        PARAMETER  ( PIMA__POLAR_RV         = 'RV      ' )
        PARAMETER  ( PIMA__POLAR_LV         = 'LV      ' )
        PARAMETER  ( PIMA__POLAR_XX         = 'XX      ' )
        PARAMETER  ( PIMA__POLAR_YY         = 'YY      ' )
        PARAMETER  ( PIMA__POLAR_XY         = 'XY      ' )
        PARAMETER  ( PIMA__POLAR_YX         = 'YX      ' )
        PARAMETER  ( PIMA__POLAR_RPL        = 'I       ' )
        PARAMETER  ( PIMA__POLAR_I          = 'I       ' )
        PARAMETER  ( PIMA__POLAR_Q          = 'Q       ' )
        PARAMETER  ( PIMA__POLAR_U          = 'U       ' )
        PARAMETER  ( PIMA__POLAR_V          = 'V       ' )
        PARAMETER  ( PIMA__POLAR_PAR        = 'PAR     ' )
	PARAMETER  ( PIMA__POLAR_ALL        = 'ALL     ' )
	PARAMETER  ( PIMA__POLAR_ORIG       = 'ORIG    ' )
	PARAMETER  ( PIMA__POLAR_1ST        = 'ALL_1ST ' )
	PARAMETER  ( PIMA__POLAR_2ND        = 'ALL_2ND ' )
        PARAMETER  ( PIMA__POLARCAL_NO      = 'NO      ' )
        PARAMETER  ( PIMA__FILTER_NO        = 'NO      ' )
        PARAMETER  ( PIMA__ONLY_DET         = 'ONLY_DET' )
	PARAMETER  ( PIMA__FRT_AUTO         = 'FRT_AUTO' )
	PARAMETER  ( PIMA__FRT_FILE         = 'FRT_FILE' )
        PARAMETER  ( PIMA__EDGE_AMP_USE     = 'USE     ' )
        PARAMETER  ( PIMA__EDGE_AMP_NO      = 'NO      ' )
        PARAMETER  ( PIMA__PLOT_TABLE       = 'TABLE   ' )
	PARAMETER  ( PIMA__FRIB_SUBS        = 'SUBS    ' )
	PARAMETER  ( PIMA__NORML_NO         = 'NO      ' )
	PARAMETER  ( PIMA__NORML_BAND       = 'BAND    ' )
	PARAMETER  ( PIMA__NORML_IF         = 'IF      ' )
	PARAMETER  ( PIMA__NORML_IFACC      = 'IFACC   ' )
	PARAMETER  ( PIMA__MOD_NO           = 'NO      ' )
	PARAMETER  ( PIMA__MOD_VERA1000     = 'VERA1000' )
	PARAMETER  ( PIMA__MOD_VERA2000     = 'VERA2000' )
	PARAMETER  ( PIMA__MOD_SHANGHAI     = 'SHAGHAI ' )
	PARAMETER  ( PIMA__MOD_SFXC         = 'SFXC    ' )
	PARAMETER  ( PIMA__MOD_KJCC         = 'KJCC    ' )
        PARAMETER  ( PIMA__INTERPLA         = 'INTERPLA' )
        PARAMETER  ( PIMA__EARTH_OB         = 'EARTH_OR' )
        PARAMETER  ( PIMA__STA_ORB          = 'STA_ORB ' )
        PARAMETER  ( PIMA__RA_PUSCH         = 'RA_PUSCH' )
        PARAMETER  ( PIMA__RA_GBT           = 'RA_GBT  ' )
        PARAMETER  ( PIMA__RA_GB140         = 'RA_GB140' )
        PARAMETER  ( PIMA__MOIM             = 'MOIM    ' )
        PARAMETER  ( PIMA__STA_BASED_NO     = 'NO      ' )
        PARAMETER  ( PIMA__STA_BASED_YES    = 'YES     ' )
        PARAMETER  ( PIMA__STA_BASED_ALL    = 'ALL     ' )
	PARAMETER  ( PIMA__BEAM_NO          = 'NO      ' )
	PARAMETER  ( PIMA__BEAM_YES         = 'YES     ' )
	PARAMETER  ( PIMA__LEGENDRE         = 'LEGENDRE' )
	PARAMETER  ( PIMA__CHEBYSH          = 'CHEBYSH ' ) 
	PARAMETER  ( PIMA__SPLINE           = 'SPLINE  ' )
	PARAMETER  ( PIMA__LINEAR           = 'LINEAR  ' )
        PARAMETER  ( PIMA__AVERAGED         = 'AVERAGED' )
        PARAMETER  ( PIMA__WEIGHTED         = 'WEIGHTED' )
        PARAMETER  ( PIMA__SPLT_WEI_ONE     = 'ONE     ' )
        PARAMETER  ( PIMA__SPLT_WEI_OBS_SNR = 'OBS_SNR ' )
        PARAMETER  ( PIMA__SPLT_WEI_OBS_RMS = 'OBS_RMS ' )
        PARAMETER  ( PIMA__SPLT_WEI_SEG_RMS = 'SEG_RMS ' )
        PARAMETER  ( PIMA__SPLT_WEI_AUTO    = 'AUTO    ' )
        PARAMETER  ( PIMA__MEASURED         = 'MEASURED' )
        PARAMETER  ( PIMA__NWM              = 'NWM     ' )
        PARAMETER  ( PIMA__OPACITY          = 'OPACITY ' )
        PARAMETER  ( PIMA__TREC             = 'TREC    ' )
        PARAMETER  ( PIMA__MODELED          = 'MODELED ' )
        PARAMETER  ( PIMA__ONOF_CREATE      = 'CREATE  ' )
        PARAMETER  ( PIMA__ONOF_UPDATE      = 'UPDATE  ' )
        PARAMETER  ( PIMA__ONOF_NO          = 'NO      ' )
	PARAMETER  ( PIMA__MPPL_OBS         = 'OBS     ' )
	PARAMETER  ( PIMA__MPPL_STA         = 'STA     ' )
	PARAMETER  ( PIMA__WVR_NO           = 'NO      ' )
	PARAMETER  ( PIMA__TOTAL_UV_YES     = 'YES     ' )
	PARAMETER  ( PIMA__TOTAL_UV_NO      = 'NO      ' )
	PARAMETER  ( PIMA__WVR_SPLINE_3RD   = 'WVR_3SPL' )
	PARAMETER  ( PIMA__WVR_SPLINE_LIN   = 'WVR_LIN ' )
	PARAMETER  ( PIMA__WVR_SPLINE_AVR   = 'WVR_AVR ' )
	PARAMETER  ( PIMA__SA_NO            = 'NO'       )
	PARAMETER  ( PIMA__SA_MIN           = 'MIN'      )
	PARAMETER  ( PIMA__SA_MAX           = 'MAX'      )
	PARAMETER  ( PIMA__SA_ALL           = 'ALL'      )
	PARAMETER  ( PIMA__ASIS             = 'ASIS'     )
	PARAMETER  ( PIMA__FRQTRA_NO        = 'NO'       )
	PARAMETER  ( PIMA__TATM             = 'TATM'     )
	PARAMETER  ( PIMA__TSMO             = 'TSMO'     )
	PARAMETER  ( PIMA__TABL             = 'TABL'     )
	PARAMETER  ( PIMA__TABMOD           = 'TABMOD'   )
!
	INTEGER*4  PIMA__DRF, PIMA__LSQ, PIMA__MUL, PIMA__ADD, PIMA__COA
	PARAMETER  ( PIMA__DRF = 1 )
	PARAMETER  ( PIMA__LSQ = 2 )
	PARAMETER  ( PIMA__MUL = 3 )
	PARAMETER  ( PIMA__ADD = 4 )
	PARAMETER  ( PIMA__COA = 4 )
	INTEGER*4  PIMA__SET_PCAL, PIMA__APPLY_PCAL
	PARAMETER  ( PIMA__SET_PCAL   = 292381234 )
	PARAMETER  ( PIMA__APPLY_PCAL = 192784012 )
!
! ----- Status flags offsets from 1 (!!)
!
	INTEGER*4  FRI__PIM, REA__PIM, FAI__PIM, NDA__PIM, NDT__PIM, &
       &           NPC__PIM, NOC__PIM
	PARAMETER  ( FRI__PIM = 1 ) ! Data were fringed
	PARAMETER  ( REA__PIM = 2 ) ! Results were read
	PARAMETER  ( FAI__PIM = 3 ) ! Failure in fringing
	PARAMETER  ( NDA__PIM = 4 ) ! No Data
	PARAMETER  ( NDT__PIM = 5 ) ! Not detected
	PARAMETER  ( NOC__PIM = 7 ) ! Lost IF channel
	PARAMETER  ( NPC__PIM = 8 ) ! No  phase cal for this observation
!
! ----- Parameters for fine search
!
	INTEGER*4  PIMA__PFS_M_GR, PIMA__PFS_M_DL, PIMA__PFS_M_RT, PIMA__PFS_M_AC
        PARAMETER  ( PIMA__PFS_M_GR  =  7 ) ! Number of grid nodes for group delay rate
        PARAMETER  ( PIMA__PFS_M_DL  =  5 ) ! Number of grid nodes for group delay
        PARAMETER  ( PIMA__PFS_M_RT  =  3 ) ! Number of grid nodes for phase delay rate
        PARAMETER  ( PIMA__PFS_M_AC  =  7 ) ! Number of grid nodes for phase delay acceleration
!
	REAL*8       PIMA__MD_ERR_LIM, PIMA__RT_ERR_LIM, PIMA__GR_STEP
        PARAMETER  ( PIMA__MD_ERR_LIM = 1.0D-12 ) ! Limit for iterations convergence
        PARAMETER  ( PIMA__RT_ERR_LIM = 5.0D-14 ) ! Limit for iterations convergence
        PARAMETER  ( PIMA__GR_STEP    = 2.0D-13 ) ! Initial group delay rate step
!
! ----- Parameter that controls D-factor code
!
        REAL*8       PIM__D_FACT
        PARAMETER  ( PIM__D_FACT = 0.33D0 )
        REAL*8       PIMA__MIN_FRQ, PIMA__MIN_AP_LEN, PIMA__SEFD_MIN
        PARAMETER  ( PIMA__MIN_FRQ = 99.9D0 ) ! Minimal frequency (in Hz)
        PARAMETER  ( PIMA__MIN_AP_LEN = 0.001D0 ) ! Minimum AP len in sec
        PARAMETER  ( PIMA__SEFD_MIN   = 0.1D0 )   ! Minimum SEFD
 	INTEGER*4    PIMA__MAX_AMW
 	PARAMETER  ( PIMA__MAX_AMW = 128  ) ! Maximum ambiguity search window
 	REAL*8       PIMA__FRT_UNDF
 	PARAMETER  ( PIMA__FRT_UNDF = 1.D10 ) ! Placeholder for the indefined FRT offset
!
	REAL*8       PIMA__TIM_AP_TOL, PIMA__TIM_EPS
	PARAMETER  ( PIMA__TIM_AP_TOL = 2.D-6 )   ! Tolerance to the AP time tag for not being
!                                                 ! a multiple of the AP length
	PARAMETER  ( PIMA__TIM_EPS    = 1.D-9   ) ! Time tolerance
!
! ----- Fine search parameters
!
	INTEGER*4    PIMA__M_SCM, PIMA__M_SCM_SMALL, PIMA__M_SCM_LARGE
        PARAMETER  ( PIMA__M_SCM = 16  )  ! Maximum number of secondary maxima to consider
        PARAMETER  ( PIMA__M_SCM_SMALL =  8  )  !
        PARAMETER  ( PIMA__M_SCM_LARGE = 16  )  !
	REAL*8       GRDEL_SUB_SHR
        PARAMETER  ( GRDEL_SUB_SHR     = 0.06D0 ) ! The share of the group delay ambiguity spacing for sub-ambig detection
!
	CHARACTER     PBP__LL_RR_REF*14, PBP__RR_LL_REF*14
	PARAMETER  (  PBP__LL_RR_REF =  'PBP__LL_RR_REF' )
	PARAMETER  (  PBP__RR_LL_REF =  'PBP__RR_LL_REF' )
        REAL*8       PIMA__PCAL_AMP_MIN, PIMA__PCAL_AMP_MAX
        PARAMETER  ( PIMA__PCAL_AMP_MIN = 1.0D-5 )
        PARAMETER  ( PIMA__PCAL_AMP_MAX = 1.0D5 )
!
        REAL*8       PIMA__GEPM_TIME_THRESH__DEF, PIMA__GEPM_DIFF_THRESH__DEF
	INTEGER*4    PIMA__GEPM_MAX_COUNT__DEF
        PARAMETER  ( PIMA__GEPM_TIME_THRESH__DEF = 1.D-1 ) ! percentage of flagged epochs for tone to be turned off
        PARAMETER  ( PIMA__GEPM_DIFF_THRESH__DEF = 1.5D-1) ! distance in complex plane to flag epoch
        PARAMETER  ( PIMA__GEPM_MAX_COUNT__DEF = 50 ) ! maximum number of allowed phase jumps per tone
!
	INTEGER*4  PIMA__GA_ELEV, PIMA__GA_ZEN
	PARAMETER  ( PIMA__GA_ELEV = 1 )
	PARAMETER  ( PIMA__GA_ZEN  = 2 )
!
	TYPE UVO__TYPE
	     COMPLEX*8, POINTER :: SPE(:,:)  => NULL()
	     REAL*4,    POINTER :: WEI(:,:)  => NULL()
	     REAL*8,    POINTER :: SNR(:,:)  => NULL()
	     REAL*8,    POINTER :: SEFD(:,:) => NULL()
	     REAL*8     SNR_FRI
	     REAL*8     SNR_TOT
	     INTEGER*4  STA_IND(2)
	     INTEGER*4  TIM_IND(2)
	     INTEGER*4  IND_OBS
	     INTEGER*4  IND_SEG
	     INTEGER*4  IND_SUB
	     REAL*8     TIM
	     REAL*8     UVW(3)
        END TYPE UVO__TYPE
        TYPE       FITS_PRIM__STRU
             REAL*4,     POINTER :: GRP_ARR(:)     => NULL()
	     REAL*4,     POINTER :: UV_DATA(:,:,:) => NULL()
        END TYPE   FITS_PRIM__STRU
	REAL*8     PIMA__ACCR_CNST1, PIMA__ACCR_CNST2
!!	PARAMETER  ( PIMA__ACCR_CNST1 = 8.69529D0 ) ! If to use original Fudge factor
!!	PARAMETER  ( PIMA__ACCR_CNST1 = 9.24327D0 )
!!
	PARAMETER  ( PIMA__ACCR_CNST1 = 9.23265D0 ) ! FRIB.AMPL_FUDGE_TYPE: VLBA
	PARAMETER  ( PIMA__ACCR_CNST2 = 7.95482D0 ) ! FRIB.AMPL_FUDGE_TYPE: AIPS
	INTEGER*4  PIMA__TAG, PIMA__CAL, PIMA__UC, PIMA__VC, PIMA__WC
	PARAMETER  ( PIMA__TAG = 1 )
	PARAMETER  ( PIMA__CAL = 2 )
	PARAMETER  ( PIMA__UC  = 1 )
	PARAMETER  ( PIMA__VC  = 2 )
	PARAMETER  ( PIMA__WC  = 3 )
	INTEGER*4    PIMA__GRID_PIL, PIMA__GRID_EXS
	PARAMETER  ( PIMA__GRID_PIL = 1723834712 )
	PARAMETER  ( PIMA__GRID_EXS = 1029301238 )
	REAL*4       PIMA__WEI_MIN
	PARAMETER  ( PIMA__WEI_MIN = 1.D-6 )
	REAL*8     PIMA__EXS_EXP_COE, PIMA__EXS_SINC_COE, PIMA__EXS_NRM
	INTEGER*4  PIMA__EXS_NCON
	PARAMETER  ( PIMA__EXS_NCON = 7 )
	PARAMETER  ( PIMA__EXS_EXP_COE  = 1.0D0 )
	PARAMETER  ( PIMA__EXS_SINC_COE = 1.D0/3.141592653589793D0 )
	PARAMETER  ( PIMA__EXS_NRM  = 0.98336673D0 )
	INTEGER*4    PIMA__SC_PHS, PIMA__SC_AMP, PIMA__SC_PHSAMP
	PARAMETER  ( PIMA__SC_PHS    = 259819231 )
	PARAMETER  ( PIMA__SC_AMP    = 283924025 )
	PARAMETER  ( PIMA__SC_PHSAMP = 239285234 )
	INTEGER*4  PIMA__UNDEF, PIMA__ALLOCATED, PIMA__LOADED, PIMA__UPDATED, &
     &             PIMA__INIT, PIMA__PHAS, PIMA__AMPL
	PARAMETER  ( PIMA__UNDEF = 0 )
	PARAMETER  ( PIMA__ALLOCATED = 17001 )
	PARAMETER  ( PIMA__LOADED    = 17002 )
	PARAMETER  ( PIMA__UPDATED   = 17003 )
	PARAMETER  ( PIMA__INIT      = 17004 )
	PARAMETER  ( PIMA__AMPL = 1 )
	PARAMETER  ( PIMA__PHAS = 2 )
	CHARACTER   PIMA__FRIP_SCATYP(2)*3
	DATA        PIMA__FRIP_SCATYP / 'Tag', 'Cal' /
	INTEGER*4   PIMA__NZO_NOT_SCAL
        PARAMETER ( PIMA__NZO_NOT_SCAL = 8 ) ! How many NZO epochs per B-spline segment
!
      INTERFACE
         SUBROUTINE FFITS_GET_KEYP ( FPTR, MHDR, MKEY, MAX_KEY, LHDR, LKEY, &
     &                               KEYS, IUER )
           INTEGER*8  FPTR
           INTEGER*4  MHDR, MKEY, MAX_KEY, LHDR, LKEY(MHDR), IUER
           CHARACTER*80, POINTER :: KEYS(:,:)
         END SUBROUTINE FFITS_GET_KEYP
      END INTERFACE
!
      INTEGER*4    PIMA__POLAR_NC
      PARAMETER  ( PIMA__POLAR_NC = 29 )
      CHARACTER  PIMA__POLAR_CODE(PIMA__POLAR_NC)*8
      DATA       PIMA__POLAR_CODE  / &
     &  	   PIMA__POLAR_RR,   & !  1
     &  	   PIMA__POLAR_LR,   & !  2
     &  	   PIMA__POLAR_LR,   & !  3
     &  	   PIMA__POLAR_LL,   & !  4
     &  	   PIMA__POLAR_HH,   & !  5
     &  	   PIMA__POLAR_VH,   & !  6  
     &  	   PIMA__POLAR_HV,   & !  7 
     &  	   PIMA__POLAR_VV,   & !  8 
     &  	   PIMA__POLAR_HR,   & !  9
     &  	   PIMA__POLAR_VR,   & ! 10  
     &  	   PIMA__POLAR_HL,   & ! 11
     &  	   PIMA__POLAR_VL,   & ! 12
     &  	   PIMA__POLAR_RH,   & ! 13
     &  	   PIMA__POLAR_LH,   & ! 14
     &  	   PIMA__POLAR_RV,   & ! 15
     &  	   PIMA__POLAR_LV,   & ! 16
     &  	   PIMA__POLAR_XX,   & ! 17
     &  	   PIMA__POLAR_YY,   & ! 18
     &  	   PIMA__POLAR_XY,   & ! 19
     &  	   PIMA__POLAR_YX,   & ! 20
     &  	   PIMA__POLAR_I,    & ! 21
     &  	   PIMA__POLAR_Q,    & ! 22
     &  	   PIMA__POLAR_U,    & ! 23
     &  	   PIMA__POLAR_V,    & ! 24
     &  	   PIMA__POLAR_PAR,  & ! 25
     &  	   PIMA__POLAR_ORIG, & ! 26
     &  	   PIMA__POLAR_ALL,  & ! 27
     &  	   PIMA__POLAR_1ST,  & ! 28
     &  	   PIMA__POLAR_2ND   & ! 29
     &                             /
      INTEGER*4    PIMA__POL_R, PIMA__POL_L, PIMA__POL_H, PIMA__POL_V, PIMA__POL_X, PIMA__POL_Y, &
     &             PIMA__POL_H1, PIMA__POL_V1, PIMA__POL_H2, PIMA__POL_V2, &
     &             PIMA__U, PIMA__V, PIMA__W, &
     &             PIMA__STK1__I, PIMA__STK1__RR, PIMA__STK1__HH, PIMA__STK1__MX, &
     &             PIMA__POL_MIN, PIMA__POL_MAX
      PARAMETER  ( PIMA__POL_R = 1 )
      PARAMETER  ( PIMA__POL_L = 2 )
      PARAMETER  ( PIMA__POL_H = 3 )
      PARAMETER  ( PIMA__POL_V = 4 )
      PARAMETER  ( PIMA__POL_X = 5 )
      PARAMETER  ( PIMA__POL_Y = 6 )
      PARAMETER  ( PIMA__POL_H1 = 7 )
      PARAMETER  ( PIMA__POL_V1 = 8 )
      PARAMETER  ( PIMA__POL_H2 = 9 )
      PARAMETER  ( PIMA__POL_V2 = 10 )
      PARAMETER  ( PIMA__POL_MIN =  1 )
      PARAMETER  ( PIMA__POL_MAX = 10 )
      PARAMETER  ( PIMA__U = 1 )
      PARAMETER  ( PIMA__V = 1 )
      PARAMETER  ( PIMA__W = 3 )
      PARAMETER  ( PIMA__STK1__I  =  1 ) ! FITS-IDI I  First Stokes parameter code 
      PARAMETER  ( PIMA__STK1__RR = -1 ) ! FITS-IDI RR First Stokes parameter code 
      PARAMETER  ( PIMA__STK1__HH = -5 ) ! FITS-IDI HH First Stokes parameter code 
      PARAMETER  ( PIMA__STK1__MX = -9 ) ! FITS-IDI MX First Stokes parameter code 
      CHARACTER    PIMA__POL(10)*1, PIMA__POL_STR*10
      DATA         PIMA__POL / 'R', 'L', 'H', 'V', 'X', 'Y', 'F', 'S', 'f', 's' /
      DATA         PIMA__POL_STR / 'RLHVXYFSfs' /
      INTEGER*4    PIMA__CC__RR, PIMA__CC__LR, PIMA__CC__RL, PIMA__CC__LL, &
     &             PIMA__CL__RH, PIMA__CL__LH, PIMA__CL__RV, PIMA__CL__LR, &
     &             PIMA__LC__HR, PIMA__LC__HL, PIMA__LC__VR, PIMA__LC__VL, &
     &             PIMA__LL__HH, PIMA__LL__VH, PIMA__LL__HV, PIMA__LL__VV  
      PARAMETER  ( PIMA__CC__RR = 1 )
      PARAMETER  ( PIMA__CC__LR = 2 )
      PARAMETER  ( PIMA__CC__RL = 3 )
      PARAMETER  ( PIMA__CC__LL = 4 )
!
      PARAMETER  ( PIMA__CL__RH = 1 )
      PARAMETER  ( PIMA__CL__LH = 2 )
      PARAMETER  ( PIMA__CL__RV = 3 )
      PARAMETER  ( PIMA__CL__LR = 4 )
!
      PARAMETER  ( PIMA__LC__HR = 1 )
      PARAMETER  ( PIMA__LC__HL = 2 )
      PARAMETER  ( PIMA__LC__VR = 3 )
      PARAMETER  ( PIMA__LC__VL = 4 )
!
      PARAMETER  ( PIMA__LL__HH = 1 )
      PARAMETER  ( PIMA__LL__VH = 2 )
      PARAMETER  ( PIMA__LL__HV = 3 )
      PARAMETER  ( PIMA__LL__VV = 4 )   
!
      INTEGER*4    PIM__SPLT_SNR_MINSEG
      PARAMETER  ( PIM__SPLT_SNR_MINSEG = 32 )
!
      INTEGER*4   PIMA__ACC, PIMA__GRAT
      PARAMETER ( PIMA__ACC  = 92834102 ) ! Mode: estimate phase acceleration
      PARAMETER ( PIMA__GRAT = 24840216 ) ! Mode: estimate group delay rate
!
      CHARACTER    PIMA__SPD_CNF_TEMPL*34, PIMA__SPD_EXE*6
      PARAMETER  ( PIMA__SPD_CNF_TEMPL = 'share/pima/spd_config_template.txt' )
      PARAMETER  ( PIMA__SPD_EXE       = 'spd_3d' )
      REAL*8       PIMA__SPD_ELMIN, PIMA__SPD_STP
      PARAMETER  ( PIMA__SPD_ELMIN = 0.05236D0 ) ! 3 degrees
      PARAMETER  ( PIMA__SPD_STP   = 3.0D0*3600.0D0 )
      INTEGER*4  PIMA__SINGLE, PIMA__MERGE, PIMA__COMBINE
      PARAMETER  ( PIMA__SINGLE  = 141216894 )
      PARAMETER  ( PIMA__MERGE   = 102348952 )
      PARAMETER  ( PIMA__COMBINE = 120308977 )
      INTEGER*4    PIMA__WVR_AVL
      PARAMETER  ( PIMA__WVR_AVL = 193672302 )
      INTEGER*4    PIMA__BPS_AMP_VERS
      PARAMETER  ( PIMA__BPS_AMP_VERS = 20170303 )
      REAL*8       PIMA__DIG_22, PIMA__DIG_12, PIMA__DIG_11
      PARAMETER  ( PIMA__DIG_22 = 0.8825D0 )
      PARAMETER  ( PIMA__DIG_12 = 0.7495D0 )
      PARAMETER  ( PIMA__DIG_11 = 0.6366D0 )
      CHARACTER  PIMA__CIRPOL_MODE*8, PIMA__LINPOL_MODE*8, PIMA__MIXPOL_MODE*8
      PARAMETER  ( PIMA__CIRPOL_MODE = 'cir-pol ' )
      PARAMETER  ( PIMA__LINPOL_MODE = 'lin-pol ' )
      PARAMETER  ( PIMA__MIXPOL_MODE = 'mix-pol ' )
      INTEGER*4    PIMA__RRCC, PIMA__LLCC, PIMA__RLCC, PIMA__LRCC, &
    &              PIMA__XXCC, PIMA__YYCC, PIMA__XYCC, PIMA__YXCC, &
    &              PIMA__IPCC, PIMA__QPCC, PIMA__UPCC, PIMA__VPCC, &
!
    &              PIMA__HHLL, PIMA__VVLL, PIMA__HVLL, PIMA__VHLL, &
    &              PIMA__XXLL, PIMA__YYLL, PIMA__XYLL, PIMA__YXLL, &
    &              PIMA__RRLL, PIMA__LLLL, PIMA__RLLL, PIMA__LRLL, &
    &              PIMA__IPLL, PIMA__QPLL, PIMA__UPLL, PIMA__VPLL, &
!
    &              PIMA__RHCL, PIMA__RVCL, PIMA__LHCL, PIMA__LVCL, &
    &              PIMA__XXCL, PIMA__YYCL, PIMA__XYCL, PIMA__YXCL, &
    &              PIMA__RRCL, PIMA__LLCL, PIMA__RLCL, PIMA__LRCL, &
    &              PIMA__IPCL, PIMA__QPCL, PIMA__UPCL, PIMA__VPCL, &
!
    &              PIMA__HRLC, PIMA__HLLC, PIMA__VRLC, PIMA__VLLC, &
    &              PIMA__XXLC, PIMA__YYLC, PIMA__XYLC, PIMA__YXLC, &
    &              PIMA__RRLC, PIMA__LLLC, PIMA__RLLC, PIMA__LRLC, &
    &              PIMA__IPLC, PIMA__QPLC, PIMA__UPLC, PIMA__VPLC, &
!
                   PIMA__PAR,  PIMA__PALL_NOR, PIMA__PALL_XY, PIMA__PALL_CIR, &
     &             PIMA__PALL_1ST,  PIMA__PALL_2ND, PIMA__PALL_MIXED
!
      PARAMETER  ( PIMA__RRCC = 1021001 ) ! RR-polarization from dual or single circular polarization
      PARAMETER  ( PIMA__LLCC = 1021002 ) ! LL-polarization from dual or single circular polarization
      PARAMETER  ( PIMA__RLCC = 1021003 ) ! RL-polarization from dual circular polarization
      PARAMETER  ( PIMA__LRCC = 1021004 ) ! LR-polarization from dual circular polarization
      PARAMETER  ( PIMA__XXCC = 1021005 ) ! XX-polarization from dual circular polarization
      PARAMETER  ( PIMA__YYCC = 1021006 ) ! YY-polarization from dual circular polarization
      PARAMETER  ( PIMA__XYCC = 1021007 ) ! XY-polarization from dual circular polarization
      PARAMETER  ( PIMA__YXCC = 1021008 ) ! YX-polarization from dual circular polarization
      PARAMETER  ( PIMA__IPCC = 1021009 ) ! I-Stokes        from dual circular polarization
      PARAMETER  ( PIMA__QPCC = 1021010 ) ! Q-Stokes        from dual circular polarization
      PARAMETER  ( PIMA__UPCC = 1021011 ) ! U-Stokes        from dual circular polarization
      PARAMETER  ( PIMA__VPCC = 1021012 ) ! V-Stokes        from dual circular polarization
!
      PARAMETER  ( PIMA__HHLL = 1022001 ) ! HH-polarization from dual linear  polarization
      PARAMETER  ( PIMA__VVLL = 1022002 ) ! VV-polarization from dual linear  polarization
      PARAMETER  ( PIMA__HVLL = 1022003 ) ! HV-polarization from dual linear  polarization
      PARAMETER  ( PIMA__VHLL = 1022004 ) ! VH-polarization from dual linear  polarization
      PARAMETER  ( PIMA__XXLL = 1022005 ) ! XX-polarization from dual linear  polarization
      PARAMETER  ( PIMA__YYLL = 1022006 ) ! YY-polarization from dual linear  polarization
      PARAMETER  ( PIMA__XYLL = 1022007 ) ! XY-polarization from dual linear  polarization
      PARAMETER  ( PIMA__YXLL = 1022008 ) ! YX-polarization from dual linear  polarization
      PARAMETER  ( PIMA__RRLL = 1022009 ) ! RR-polarization from dual linear  polarization
      PARAMETER  ( PIMA__LLLL = 1022010 ) ! LL-polarization from dual linear  polarization
      PARAMETER  ( PIMA__RLLL = 1022011 ) ! RL-polarization from dual linear  polarization
      PARAMETER  ( PIMA__LRLL = 1022012 ) ! LR-polarization from dual linear  polarization
      PARAMETER  ( PIMA__IPLL = 1022013 ) ! I-Stokes        from dual linear  polarization
      PARAMETER  ( PIMA__QPLL = 1022014 ) ! Q-Stokes        from dual linear  polarization
      PARAMETER  ( PIMA__UPLL = 1022015 ) ! U-Stokes        from dual linear  polarization
      PARAMETER  ( PIMA__VPLL = 1022016 ) ! V-Stokes        from dual linear  polarization
!
      PARAMETER  ( PIMA__RHCL = 1023001 ) ! RH-polarization from mixed circular linear polarization
      PARAMETER  ( PIMA__RVCL = 1023002 ) ! RV-polarization from mixed circular linear polarization
      PARAMETER  ( PIMA__LHCL = 1023003 ) ! LH-polarization from mixed circular linear polarization
      PARAMETER  ( PIMA__LVCL = 1023004 ) ! LV-polarization from mixed circular linear polarization
      PARAMETER  ( PIMA__XXCL = 1023005 ) ! XX-polarization from mixed circular linear polarization
      PARAMETER  ( PIMA__YYCL = 1023006 ) ! YY-polarization from mixed circular linear polarization
      PARAMETER  ( PIMA__XYCL = 1023007 ) ! XY-polarization from mixed circular linear polarization
      PARAMETER  ( PIMA__YXCL = 1023008 ) ! YX-polarization from mixed circular linear polarization
      PARAMETER  ( PIMA__RRCL = 1023009 ) ! RR-polarization from mixed circular linear polarization
      PARAMETER  ( PIMA__LLCL = 1023010 ) ! LL-polarization from mixed circular linear polarization
      PARAMETER  ( PIMA__RLCL = 1023011 ) ! RL-polarization from mixed circular linear polarization
      PARAMETER  ( PIMA__LRCL = 1023012 ) ! LR-polarization from mixed circular linear polarization
      PARAMETER  ( PIMA__IPCL = 1023013 ) ! I-Stokes        from mixed circular linear polarization
      PARAMETER  ( PIMA__QPCL = 1023014 ) ! Q-Stokes        from mixed circular linear polarization
      PARAMETER  ( PIMA__UPCL = 1023015 ) ! U-Stokes        from mixed circular linear polarization
      PARAMETER  ( PIMA__VPCL = 1023016 ) ! V-Stokes        from mixed circular linear polarization
!
      PARAMETER  ( PIMA__HRLC = 1024001 ) ! HR-polarization from mixed linear circular polarization
      PARAMETER  ( PIMA__VRLC = 1024002 ) ! VR-polarization from mixed linear circular polarization
      PARAMETER  ( PIMA__HLLC = 1024003 ) ! HL-polarization from mixed linear circular polarization
      PARAMETER  ( PIMA__VLLC = 1024004 ) ! VL-polarization from mixed linear circular polarization
      PARAMETER  ( PIMA__XXLC = 1024005 ) ! XX-polarization from mixed linear circular polarization
      PARAMETER  ( PIMA__YYLC = 1024006 ) ! YY-polarization from mixed linear circular polarization
      PARAMETER  ( PIMA__XYLC = 1024007 ) ! XY-polarization from mixed linear circular polarization
      PARAMETER  ( PIMA__YXLC = 1024008 ) ! YX-polarization from mixed linear circular polarization
      PARAMETER  ( PIMA__RRLC = 1024009 ) ! RR-polarization from mixed linear circular polarization
      PARAMETER  ( PIMA__LLLC = 1024010 ) ! LL-polarization from mixed linear circular polarization
      PARAMETER  ( PIMA__RLLC = 1024011 ) ! RL-polarization from mixed linear circular polarization
      PARAMETER  ( PIMA__LRLC = 1024012 ) ! LR-polarization from mixed linear circular polarization
      PARAMETER  ( PIMA__IPLC = 1024013 ) ! I-Stokes        from mixed linear circular polarization
      PARAMETER  ( PIMA__QPLC = 1024014 ) ! Q-Stokes        from mixed linear circular polarization
      PARAMETER  ( PIMA__UPLC = 1024015 ) ! U-Stokes        from mixed linear circular polarization
      PARAMETER  ( PIMA__VPLC = 1024016 ) ! V-Stokes        from mixed linear circular polarization
!
      PARAMETER  ( PIMA__PAR      = 1025001 ) ! Use parallel circular polarizations
      PARAMETER  ( PIMA__PALL_NOR = 1025002 ) ! Use all polarizations, no polarization rotation
      PARAMETER  ( PIMA__PALL_XY  = 1025003 ) ! Use all polarizations with applying polarization rotation to XY
      PARAMETER  ( PIMA__PALL_CIR = 1025004 ) ! Use all polarizations with applying polarization rotation to XY
      PARAMETER  ( PIMA__PALL_1ST = 1025005 ) ! Use all polarizations with applying polarization rotation to the 1st station
      PARAMETER  ( PIMA__PALL_2ND = 1025006 ) ! Use all polarizations with applying polarization rotation to the 2nd station
      PARAMETER  ( PIMA__PALL_MIXED = 1025007 ) ! Use all polarizations with applying polarization rotation. &
                                                ! M(1,1) = 1ST(1,1) M(2,1) = 1ST(2,2) ; M(1,2) = 2ND(1,1) M(2,2) = 2ND(2,2)
!
      CHARACTER  PIMA__PC_CC*7, PIMA__PC_LL*7, PIMA__PC_CL*7, PIMA__PC_LC*7
      PARAMETER  ( PIMA__PC_CC = 'cir-cir' )
      PARAMETER  ( PIMA__PC_LL = 'lin-lin' )
      PARAMETER  ( PIMA__PC_CL = 'cir-lin' )
      PARAMETER  ( PIMA__PC_LC = 'lin-cir' )
!
!
      INTEGER*4    PIMA__REM, PIMA__REF
      PARAMETER  ( PIMA__REM = 2 ) ! Index of the remote    station
      PARAMETER  ( PIMA__REF = 1 ) ! Index of the reference station
      INTEGER*4    PIMA__PCI_PAR, PIMA__PCI_CRS, PIMA__PCI_REF, PIMA__PCI_REM
      PARAMETER  ( PIMA__PCI_PAR = 1 ) ! Differitnal bandpass polarizartion combination (involes primary and secondary parallel polaration combination)
      PARAMETER  ( PIMA__PCI_CRS = 2 ) ! Differitnal bandpass polarizartion combination (does not involes neither primary or secondary parallel polaration combination)
      PARAMETER  ( PIMA__PCI_REF = 3 ) ! Reference-based bandpass polarizartion combination     (inolves primary parallel polaration combination)
      PARAMETER  ( PIMA__PCI_REM = 4 ) ! Remote-based bandpass polarizartion combination   (does not innolve primary parallel polaration combination)
      CHARACTER    PIMA__PCI_NAM(PIM__MPLR)*3
      DATA         PIMA__PCI_NAM / 'par', 'crs', 'ref', 'rem' /
!
      INTEGER*4   PIMA__POL_PAR(2), PIMA__POL_CRS(2), PIMA__POL_REF(2), PIMA__POL_REM(2)
      DATA        PIMA__POL_PAR / 1, 4 /
      DATA        PIMA__POL_CRS / 2, 3 /
      DATA        PIMA__POL_REF / 2, 1 /
      DATA        PIMA__POL_REM / 4, 2 /
      INTEGER*4   PIMA__PCC_COD(2,PIM__MPLR)
      DATA        PIMA__PCC_COD / 1, 4, &
     &                            2, 3, &
     &                            2, 1, &
     &                            4, 2  &
     &                          /
!
      REAL*8       PIMA__CAB_THR_SPIKE, PIMA__CAB_THR_SMOOTH
      INTEGER*4    PIMA__CAB_NEP_SPIKE
      PARAMETER  ( PIMA__CAB_THR_SPIKE  = 2.D-10 )
      PARAMETER  ( PIMA__CAB_THR_SMOOTH = 4.D-11 )
      PARAMETER  ( PIMA__CAB_NEP_SPIKE  = 8      )
!
      REAL*8       PIMA__AMP_ATT_MIN 
      PARAMETER  ( PIMA__AMP_ATT_MIN = 0.05 ) ! Minimum combined amplitude attenuation for SPLT
      CHARACTER    PIMA__VCAT_REPO_OBS*3
      PARAMETER  ( PIMA__VCAT_REPO_OBS = 'OBS' )
!
      INTEGER*4    PIMA__USE, PIMA__NOT_USE
      PARAMETER  ( PIMA__USE     = 120281427 )
      PARAMETER  ( PIMA__NOT_USE = 219018154 )
!
      INTEGER*4    PIMA__MDC_SCA_INCLUDED, PIMA__MDC_SCA_EXCLUDED, &
     &             PIMA__MDC_GLO_INCLUDED, PIMA__MDC_GLO_EXCLUDED
      PARAMETER  ( PIMA__MDC_SCA_INCLUDED = 512048515 ) ! Clock delay is included in the reported interferometeric model.     Clock delay is computed for each scan.
      PARAMETER  ( PIMA__MDC_SCA_EXCLUDED = 895293923 ) ! Clock delay is not uncluded in the reported interferometeric model. Clock delay is computed for each scan.
      PARAMETER  ( PIMA__MDC_GLO_INCLUDED = 391924633 ) ! Clock delay is included in the reported interferometeric model.     Clock delay is computed for the entire experiment.
      PARAMETER  ( PIMA__MDC_GLO_EXCLUDED = 294937153 ) ! Clock delay is not included in the reported interferometeric model. Clock delay is computed for the entire experiment.
!
      REAL*8       PIMA__GEPM_TIM_MSEG_DEF
      PARAMETER  ( PIMA__GEPM_TIM_MSEG_DEF = 1.0D0 ) ! Default segment length for GEPM
      INTEGER*4    PIMA__RAW, PIMA__AVR
      PARAMETER  ( PIMA__RAW = 18291553 )
      PARAMETER  ( PIMA__AVR = 33937294 )
!
! <<<<  end of pima.i
