!
! Data structure for ionospheric parameters
!
! Last modifed on 2025.12.14_19:59:51
! 
      CHARACTER    IONO_REGR__LABEL*34, IONO_BIAS__LABEL*34, EST_IONO_MOD__LABEL*38
      PARAMETER  ( IONO_REGR__LABEL = '# IONO_REGR  Format of  2021.12.22' )
      PARAMETER  ( IONO_BIAS__LABEL = '# IONO_BIAS  Format of  2021.12.22' )
      PARAMETER  ( EST_IONO_MOD__LABEL = 'est_iono_mode Version  2.0  2021.12.31' )
      CHARACTER    LABEL__AOC*68, LABEL__ADDW*56, LABEL__EDIT*56, LABEL__DTEC*58
      PARAMETER  ( LABEL__AOC  = '# External apriori observation file.  Format version of 2021.03.16' )
      PARAMETER  ( LABEL__EDIT = '# External suppression flags. Format version of 2020.07.15' )
      PARAMETER  ( LABEL__ADDW = '# External additive weight. Format version of 2022.09.26'   )
      PARAMETER  ( LABEL__DTEC = '# External differential TEC.  Format version of 2022.06.28' )
      INTEGER*4   IONO__AOC, IONO__ADW, IONO__2BND, IONO__3BND, IONO__HBND, IONO__LBND, &
     &            IONO__ABND, IONO__HLBND
      PARAMETER  ( IONO__AOC   = 2028104725 )
      PARAMETER  ( IONO__ADW   = 1902740448 )
      PARAMETER  ( IONO__2BND  =  203938334 )
      PARAMETER  ( IONO__3BND  =  573203424 )
      PARAMETER  ( IONO__HBND  =  854920942 )
      PARAMETER  ( IONO__LBND  =  489292473 )
      PARAMETER  ( IONO__ABND  =  689239023 )
      PARAMETER  ( IONO__HLBND =  590320237 )
      INTEGER*4    M_OBS, M_STA, M_BAS, M_BRK, M_EXP, KOBS_MIN
      PARAMETER  ( M_OBS = 64*1024 )
      PARAMETER  ( M_STA =     256 )
      PARAMETER  ( M_BAS =    2048 )
      PARAMETER  ( M_EXP =    2048 )
      PARAMETER  ( M_BRK =       8 )
      PARAMETER  ( KOBS_MIN =    8 )
      REAL*8       IONO__FREQ_REF, IONO__FREQ_MIN
      PARAMETER  ( IONO__FREQ_REF = 8.0D9 )
      PARAMETER  ( IONO__FREQ_MIN = 0.7D9 )
      TYPE      IONO__TYPE
          LOGICAL*1  USED(4)
          CHARACTER  SOU*8
          CHARACTER  STA(2)*8
          INTEGER*4  MJD
          INTEGER*4  IND_REC
          INTEGER*4  MODE
          INTEGER*4  ISEED
          REAL*8     TAI
          REAL*8     DEL(2)
          REAL*8     DEL_ERR(2)
          REAL*8     FREQ_EFF(2)
          REAL*8     IONO_ZEN(2)
          REAL*8     IONO_MAP(2)
          REAL*8     IONO_DIF_ERR(2)
          REAL*8     IONO_V
          REAL*8     IONO_VERR
          REAL*8     IONO_G
          REAL*8     IONO_D
          REAL*8     EL(2)
          REAL*8     AZ(2)
          REAL*8     ADD_IONO_VAL
          REAL*8     ADD_IONO_SIG
      END TYPE  IONO__TYPE
!      
      TYPE      IONO__EST_TYPE
          INTEGER*4  MRES
          INTEGER*4  MODE
          REAL*8     TIM_STEP
          INTEGER*4  N_TIM
          INTEGER*4  L_STA
          CHARACTER  C_STA(M_STA)*8
          INTEGER*4  MDEG
          REAL*8     IONO_BIAS(M_STA)
          REAL*8     SIG_TIM_VAL
          REAL*8     SIG_TIM_DER
          REAL*8     SIG_TIM_DR2
          REAL*8     SIG_SPC
          REAL*8     EFF_FREQ_AVR(2)
          REAL*8,    POINTER :: TIM(:)
      END TYPE  IONO__EST_TYPE
!
      TYPE      IONO_BAS_STAT__TYPE
           CHARACTER  BAS*17
           INTEGER*4  N_OBS
           REAL*8     RMS_PRE
           REAL*8     RMS_PSF
           REAL*8     RMS_MMP
           REAL*8     AVR_GNSS
           REAL*8     RMS_GNSS
           REAL*8     RMS_STA_GNSS(2)
           REAL*8     CORR_STA_GNSS
           REAL*8     SH_MOD
           REAL*8     DR_MOD
           REAL*8     QD_MOD
           REAL*8     IONO_AVR
           REAL*8     IONO_RMS
           REAL*8     IONO_FIT
           REAL*8     EFF_FREQ_AVR(2)
      END TYPE  IONO_BAS_STAT__TYPE 
!
      TYPE      IONO_STA_STAT__TYPE
           CHARACTER  NAM*8
           REAL*8     CLO_OFF
           REAL*8     BIAS
           REAL*8     RMS
           REAL*8     IONO_AVR
           REAL*8     IONO_RMS
           REAL*8     IONO_FIT
      END TYPE  IONO_STA_STAT__TYPE 
!
      TYPE      IONO_EXP_STAT__TYPE
          CHARACTER  EXP_NAME*10
          INTEGER*4  MJD_EXP
          REAL*8     UTC_EXP
          INTEGER*4  MJD_EXP_END
          REAL*8     UTC_EXP_END
          INTEGER*4  N_BAS
          INTEGER*4  N_STA
          CHARACTER  C_BAS(M_BAS)*17
          CHARACTER  C_STA(M_STA)*8
          TYPE     ( IONO_BAS_STAT__TYPE ), POINTER :: BAS(:) => NULL()
          TYPE     ( IONO_STA_STAT__TYPE ), POINTER :: STA(:) => NULL()
      END TYPE  IONO_EXP_STAT__TYPE
!
      TYPE      IONO_STAT__TYPE
          INTEGER*4  N_EXP
          CHARACTER  C_EXP(M_EXP)*10
          TYPE     ( IONO_EXP_STAT__TYPE ), POINTER :: EXP(:) => NULL()
      END TYPE  IONO_STAT__TYPE
!
