! >>>>> Include block for vex
! >>>>> 2020.01.21 (c)  L. Petrov  v 1.0 2021.07.07_10:19:51
!
      INTEGER*4  VEX__MSTA, VEX__MSOU, VEX__MSCA, VEX__MCHA
      INTEGER*4  VEX__MIF
      PARAMETER  ( VEX__MCHA =  512 )
      PARAMETER  ( VEX__MSTA =  512 )
      PARAMETER  ( VEX__MSOU = 1024 )
      PARAMETER  ( VEX__MSCA = 4096 )
      PARAMETER  ( VEX__MIF  =    8 )   ! Max. no. of if_def in IF
      CHARACTER  VEX_PARSER__LABEL*32
      PARAMETER  ( VEX_PARSER__LABEL = 'vex_parser  1.04   of 2021.07.07' )
!
      TYPE       VEX__STA_TYPE
          CHARACTER  SITE_NAME*16 
          CHARACTER  ANT_NAME*16
          CHARACTER  SITE_ID*2
          CHARACTER  PROCEDURE_NAME_PREFIX*4
          REAL*8     SITE_POS(3)
          REAL*8     SITE_VEL(3)
          INTEGER*4  SITE_POS_EPOCH_MJD
          INTEGER*4  TAPE_CHANGE 
          INTEGER*4  HEADSTACK_MOTION
          INTEGER*4  NEW_SOURCE_COMMAND
          INTEGER*4  NEW_TAPE_SETUP
          INTEGER*4  SETUP_ALWAYS
          INTEGER*4  PARITY_CHECK
          INTEGER*4  TAPE_PREPASS
          INTEGER*4  PREOB_CAL
          INTEGER*4  MIDOB_CAL
          INTEGER*4  POSTOB_CAL
      END TYPE   VEX__STA_TYPE
!
      TYPE       VEX__SOU_TYPE
          CHARACTER  NAME*16
          CHARACTER  IAU_NAME*16 
          REAL*8     RA
          REAL*8     DEC
      END TYPE   VEX__SOU_TYPE
!
      TYPE       VEX__SCA_TYPE 
          CHARACTER  MODE*32    
          CHARACTER  SOU_NAME*16
          CHARACTER  SCAN_NAME*16
          INTEGER*4  IND_SOU
          INTEGER*4  MJD
          REAL*8     UTC 
          INTEGER*4  N_STA
          INTEGER*4  IND_STA(VEX__MSTA)     
          REAL*8     START_OFFSET(VEX__MSTA)
          REAL*8     SCAN_DUR(VEX__MSTA)
          INTEGER*4  WRAP_MODE(VEX__MSTA)
      END TYPE   VEX__SCA_TYPE
!
      TYPE       VEX__FRQ_TYPE 
          CHARACTER FRQ_NAME*32
          CHARACTER MODE*32
          REAL*8    SAMPLE_RATE
          INTEGER*4 BIT_SAMPLE
          INTEGER*4 N_STA
          CHARACTER SITE_ID(VEX__MSTA)*2      !***! Link to Station 
!
          INTEGER*4 N_CHA
          CHARACTER BAND_ID(VEX__MCHA)*2
          REAL*8    SKY_FRQ(VEX__MCHA)        !***! Sky Freq in Hz (astronomical notation)
          CHARACTER NET_SB(VEX__MCHA)*1       
          REAL*8    CHA_BW(VEX__MCHA)         !***! Channel Bandwidth in Hz
          CHARACTER CHAN_ID(VEX__MCHA)*5  
          CHARACTER BBC_ID(VEX__MCHA)*5       !***! Link to BaseBand Converter
          CHARACTER PHASE_CAL_ID(VEX__MCHA)*9 !***! Link to Phase Calibaration Detector
      END TYPE   VEX__FRQ_TYPE
!
      TYPE       VEX__IFS_TYPE
	  CHARACTER IF_NAME*32
	  CHARACTER MODE*32
	  INTEGER*4 N_STA
	  CHARACTER SITE_ID(VEX__MSTA)*2
	  CHARACTER IF_ID(VEX__MIF)*8
          CHARACTER PHY_NAME(VEX__MIF)*2      !***! Physical Name
	  CHARACTER POL(VEX__MIF)*1
	  REAL*8    TOTAL_LO(VEX__MIF)
	  CHARACTER NET_SB(VEX__MIF)*1
	  REAL*8    P_CAL_FREQ_SPACE(VEX__MIF)
	  REAL*8    P_CAL_BASE_FREQ(VEX__MIF)
	  INTEGER*8    N_SX_IF(2)
      END TYPE   VEX__IFS_TYPE
!
      TYPE       VEX__FLG_TYPE 
          LOGICAL*1  GLO
          LOGICAL*1  EXP
          LOGICAL*1  PRO
          LOGICAL*1  STA
          LOGICAL*1  SOU
          LOGICAL*1  SCA
          LOGICAL*1  STE
          LOGICAL*1  FRQ
          LOGICAL*1  MOD
	  LOGICAL*1  IFS
      END TYPE   VEX__FLG_TYPE
!
      TYPE       VEX_TYPE
          CHARACTER  REVISION*8
          CHARACTER  EXPER_NAME*32
          CHARACTER  EXPER_DESCR*128
          CHARACTER  CONTACT_NAME*64
          CHARACTER  SCHEDULER_NAME*64
          CHARACTER  SCHEDULER_EMAIL*64
          INTEGER*4  MJD_START
          INTEGER*4  MJD_STOP
          REAL*8     UTC_START
          REAL*8     UTC_STOP   
          INTEGER*4  N_STA
          INTEGER*4  N_SOU
          INTEGER*4  N_SCA
          INTEGER*4  N_FRQ
          TYPE ( VEX__STA_TYPE ), POINTER :: STA(:) => NULL()
          TYPE ( VEX__SOU_TYPE ), POINTER :: SOU(:) => NULL()
          TYPE ( VEX__SCA_TYPE ), POINTER :: SCA(:) => NULL()
          TYPE ( VEX__FRQ_TYPE ), POINTER :: FRQ(:) => NULL()
          TYPE ( VEX__IFS_TYPE ), POINTER :: IFS(:) => NULL()
          CHARACTER  C_STA(VEX__MSTA)*16
          CHARACTER  C_SOU(VEX__MSOU)*16
          INTEGER*4  STATUS
      END TYPE   VEX_TYPE
!
      INTEGER*4  VEX__UNDF, VEX__LOAD
      PARAMETER  ( VEX__UNDF = 0 )
      PARAMETER  ( VEX__LOAD = 1028931 )
!
      INTEGER*4  VEX__INIT
      PARAMETER  ( VEX__INIT = 22831221 )
!
      REAL*8       VEX__MIN_SAMPLE_RATE, VEX__MIN_INT_TIME
      PARAMETER  ( VEX__MIN_SAMPLE_RATE = 999999.9D0 )
      PARAMETER  ( VEX__MIN_INT_TIME    = 0.00099D0  )
