!
! >>>>> Include block for simul
! >>>>> 2020.06.09 (c)  L. Petrov  v 1.16  2021.12.01_15:55:17
!
      INTEGER*4    SIM__MOBS, SIM__MSTA, SIM__MSOU, SIM__MSCA, SIM__MBND, SIM__MIFS
      PARAMETER  ( SIM__MOBS = 2*1024*1024 )
      PARAMETER  ( SIM__MSTA = 256     )
      PARAMETER  ( SIM__MSOU = 4096    )
      PARAMETER  ( SIM__MSCA = 4096    )
      PARAMETER  ( SIM__MBND = 4       )
      PARAMETER  ( SIM__MIFS = 64      )
      CHARACTER   SIMUL__LABEL*36, COVZEN__LABEL*68, SIMPAR__LABEL*64, ACM__LABEL*56
      PARAMETER  ( SIMUL__LABEL  = 'pSolve simul v  1.16  of 2021.12.01' )
      PARAMETER  ( COVZEN__LABEL = '# Covariance of zenith path delay. Format version  1.1 of 2021.11.14' )
      PARAMETER  ( SIMPAR__LABEL = '# VLBI simulation parameters. Format version  1.0  of 2021.12.01' )
      PARAMETER  ( ACM__LABEL    = 'Atmospheric Covariance Matrix Format  2.0  of 2021.11.26' )
      REAL*8       SCAN_DUR_MIN
      PARAMETER  ( SCAN_DUR_MIN = 5.0D0 )
!
      INTEGER*4    M__SMP
      PARAMETER  ( M__SMP = 15 )
      TYPE      SIMUL__CNF_TYPE
          CHARACTER  CONF_FILE*128
!
          CHARACTER  VCAT_CONF*128
          CHARACTER  REPO_IN*128
          CHARACTER  REPO_OUT*128
          CHARACTER  STP_DIR*128
          CHARACTER  VTD_CONF*128
          CHARACTER  NERS_CONF*128
          CHARACTER  SBT_TABLE*128
          CHARACTER  ZEN_COV_DIR*128
          CHARACTER  RH_MODE*16
          CHARACTER  GR_DEL_MODE*16
          REAL*8     WHT_RMS       ! RMS of added white noise
          REAL*8     DIL_ZEN_COV   ! RMS Dilution factor for zenith path delay
          REAL*8     DIL_MAP_ERR   ! RMS Dilution factor for errors in mapping function
          REAL*8     DIL_OBS_NOISE ! Dilution factor for observed noise
          INTEGER*4  ISEED
      END TYPE  SIMUL__CNF_TYPE
!           
      TYPE      SIMUL__STACOV_TYPE
          CHARACTER  FIL_COV*128
          CHARACTER  STA_NAM*8
          CHARACTER  INT_NAM*16
          CHARACTER  MOD_NAM*16
          INTEGER*4  NP
          INTEGER*4  NE
          INTEGER*4  NO
          REAL*8     RMS
          REAL*8     INP_STP
          REAL*8     MOD_STP
          REAL*8     SIG_SQ_DIAG
          REAL*8     COV0
          REAL*8,    POINTER :: DTIM(:)     => NULL()
          REAL*8,    POINTER :: COR(:)      => NULL()
          REAL*8,    POINTER :: EL_MF(:)    => NULL()
          REAL*8,    POINTER :: MF_RMS(:)   => NULL()
          REAL*8,    POINTER :: COVS(:)     => NULL()
          REAL*8,    POINTER :: COR_OBS(:)  => NULL()
      END TYPE  SIMUL__STACOV_TYPE
!
      TYPE      SIMUL__TYPE
          INTEGER*4  NOBS
          INTEGER*4  NSCA
          INTEGER*4  NSTA
          INTEGER*4  NSOU
          INTEGER*4  NBND
          INTEGER*4  ITYP
!
          CHARACTER  SOU_NAM(SIM__MSOU)*8
          CHARACTER  BND_NAM(SIM__MBND)*1
          REAL*8     SOU_COO(2,SIM__MSOU)
!
          INTEGER*4  NOBS_STA(SIM__MSTA)
          CHARACTER  STA_NAM(SIM__MSTA)*8
          REAL*8     STA_COO(3,SIM__MSTA)
          CHARACTER  STA_ID(SIM__MSTA)*2
          REAL*8     SAMPLE_RATE(SIM__MSTA)
          INTEGER*4  BITS_SAMPLE(SIM__MSTA)
!
          INTEGER*4  MJD_BEG
          INTEGER*4  MJD_END
          REAL*8     UTC_BEG
          REAL*8     UTC_END
          REAL*8     UTC_MTAI
!
          CHARACTER  BAND_MODE*8
          INTEGER*4  N_BNDS
          REAL*8     FRQ_IF(SIM__MIFS)
          INTEGER*4  NUM_IFS(SIM__MBND)
          INTEGER*4  IND_FRQ_STA(SIM__MIFS,SIM__MBND,SIM__MSTA)
!
          CHARACTER  SCAN_NAME(SIM__MOBS)*16
          INTEGER*4  OBSTAB(3,SIM__MOBS)
          INTEGER*4  MJD_OBS(SIM__MOBS)
          REAL*8     UTC_OBS(SIM__MOBS)
          INTEGER*4  SOU_IND(SIM__MOBS)
          INTEGER*4  SCA_IND(SIM__MOBS)
          INTEGER*4  STA_IND(2,SIM__MOBS)
          REAL*8     SCAN_DUR(SIM__MOBS)
          REAL*8     GR_DEL(SIM__MBND,SIM__MOBS)
          REAL*8     PH_RAT(SIM__MBND,SIM__MOBS)
          REAL*8     GR_DEL_ERR(SIM__MBND,SIM__MOBS)
          REAL*8     PH_RAT_ERR(SIM__MBND,SIM__MOBS)
          REAL*8     SNR(SIM__MBND,SIM__MOBS)
          REAL*8     AMP(SIM__MBND,SIM__MOBS)
          REAL*8     NOI(SIM__MBND,SIM__MOBS)
          REAL*8     EFF_FREQ(3,SIM__MBND,SIM__MOBS)
          REAL*8     REF_FREQ(SIM__MBND,SIM__MOBS)
          REAL*8     EL(2,SIM__MOBS)
          REAL*8     AZ(2,SIM__MOBS)
          REAL*8     DEL_ERR_SNR_1(SIM__MBND)
          REAL*8     ZPD(2,SIM__MOBS)
          REAL*8     SPD(2,SIM__MOBS)
          REAL*8     ZPD_DER(2,SIM__MOBS)
          INTEGER*4  AUTO_SUP(SIM__MOBS)
          INTEGER*4  USER_SUP(SIM__MOBS)
          INTEGER*4  USER_REC(SIM__MOBS)
          CHARACTER  QUALCODE(SIM__MOBS)*1
!
          CHARACTER  REVISION*8
          CHARACTER  EXPER_NAME*32
          CHARACTER  EXPER_DESCR*128
          CHARACTER  PI_NAME*80
          INTEGER*4  N_COV
          TYPE     ( SIMUL__CNF_TYPE    )  CNF
          TYPE     ( SIMUL__STACOV_TYPE ), POINTER :: COV(:) => NULL()
      END TYPE  SIMUL__TYPE
!
      TYPE      SIMUL__STAOBS_TYPE
          REAL*8,    POINTER :: EL(:,:)      => NULL()
          REAL*8,    POINTER :: AZ(:,:)      => NULL()
          REAL*8,    POINTER :: UTC(:,:)     => NULL()
          REAL*8,    POINTER :: ZPD(:,:)     => NULL()
          INTEGER*4, POINTER :: MJD(:,:)     => NULL()
          INTEGER*4, POINTER :: IND_SCA(:,:) => NULL()
          INTEGER*4, POINTER :: NOBS_STA(:)  => NULL()
      END TYPE  SIMUL__STAOBS_TYPE
!
      TYPE       SIMUL_SORT__TYPE 
           INTEGER*4  OBS_IND
           INTEGER*4  MJD
           REAL*8     UTC
           INTEGER*4  STA_IND(2)
      END  TYPE  SIMUL_SORT__TYPE 
!
      INTEGER*2  SIMUL__DATYP__DEF, SIMUL__SUPMET__DEF
      PARAMETER  ( SIMUL__DATYP__DEF  =  15 )
      PARAMETER  ( SIMUL__SUPMET__DEF = 505 )
      INTEGER*4  SIM__VEX, SIM__GVF, SIM__VDA
      PARAMETER  ( SIM__VEX = 2038941 )
      PARAMETER  ( SIM__GVF = 8392065 )
      PARAMETER  ( SIM__VDA = 7202941 )
