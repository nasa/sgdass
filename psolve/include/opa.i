!
! >>>>> INCLUDE-BLOCK with description of data structures used by  OPA
!
!       opa.i  14-AUG-2000  v 2.3  Leonid Petrov  2007.09.27_09:27:20
!
      INTEGER*4  M_PAR, M_BUF
      PARAMETER  ( M_PAR =  37 ) ! Number of keywords in OPA configuration file
      PARAMETER  ( M_BUF = 512 )
      CHARACTER  CVRB_RANGE*6
      PARAMETER  ( CVRB_RANGE = '[0, 1]' )
      CHARACTER  OPC__LABEL*13
      PARAMETER  ( OPC__LABEL = 'OPC ver. 2.2 ' )
      CHARACTER  OPC__LABEL_21*13
      PARAMETER  ( OPC__LABEL_21 = 'OPC ver. 2.1 ' )
      REAL*8     OPC__VER_LAST
      PARAMETER  ( OPC__VER_LAST = 2.0 ) ! last compatible version of OPC
      CHARACTER  HELP01__OPA*10, HELP02__OPA*10
      PARAMETER  ( HELP01__OPA = 'opa_01.txt' )
      PARAMETER  ( HELP02__OPA = 'opa_02.txt' )
      CHARACTER  SIG_IERS*44
      PARAMETER  ( SIG_IERS = '# EOP IERS format version 1.0  of 2001.05.25' )
!
! --- This operation indeses should be consecutive integers from 1 to
! --- M_OPA
!
      INTEGER*4  OPA__SUP, OPA__GAL, OPA__BAW, OPA__STW, &
     &           OPA__EOS, OPA__EOM, OPA__STN, OPA__EOK, &
     &           OPA__SNR, OPA__VDB, OPA__SBD, OPA__SBE, &
     &           OPA__SBM, OPA__SNX, OPA__ALL, OPA__UND, &
     &           OPA__CAN
      INTEGER*4  M_OPA
      PARAMETER  ( OPA__SUP =  1  )
      PARAMETER  ( OPA__GAL =  2  )
      PARAMETER  ( OPA__BAW =  3  )
      PARAMETER  ( OPA__STW =  4  )
      PARAMETER  ( OPA__STN =  5  )
      PARAMETER  ( OPA__EOS =  6  )
      PARAMETER  ( OPA__EOM =  7  )
      PARAMETER  ( OPA__EOK =  8  )
      PARAMETER  ( OPA__SNR =  9  )
      PARAMETER  ( OPA__VDB = 10  )
      PARAMETER  ( OPA__SBD = 11  )
      PARAMETER  ( OPA__SBE = 12  )
      PARAMETER  ( OPA__SBM = 13  )
      PARAMETER  ( OPA__SNX = 14  )
      PARAMETER  ( M_OPA = OPA__SNX )
      PARAMETER  ( OPA__ALL = 512 )
      PARAMETER  ( OPA__UND = 513 )
      PARAMETER  ( OPA__CAN = 514 )
!
      TYPE      OPA__STRU
          INTEGER*4  FIRST_FIELD
          CHARACTER  CONFIG_FILE*128
!
          CHARACTER  MASTER_DIR*128
          CHARACTER  URL_IVSCONTROL*128
          CHARACTER  SESSION_DIR*128
          CHARACTER  BAS_WEIGHT_CNT*128
          CHARACTER  SIT_WEIGHT_CNT*128
          CHARACTER  EOPS_CNT*128
          CHARACTER  STANDALONE_CNT*128
          CHARACTER  STANDALONE_ID*128
          CHARACTER  EOPS_CGM*128
          CHARACTER  GEN_INPERP*128
          CHARACTER  BAS_WEIGHT_FILE*128
          CHARACTER  SIT_WEIGHT_FILE*128
          CHARACTER  GLO_STA_FILE*128
          CHARACTER  GLO_SRC_FILE*128
          CHARACTER  GLO_ARC_FILE*128
          CHARACTER  VDB_UPDATE_EXE*128
          CHARACTER  SESSION_TYPE*16
          CHARACTER  EOPB_FILE*128
          CHARACTER  EOPT_FILE*128
          CHARACTER  EOPK_FILE*128
          CHARACTER  EOPS_FILE*128
          CHARACTER  EOPM_FILE*128
          CHARACTER  IVS_DB_URL*256
          CHARACTER  DBS_CONF*256
          CHARACTER  EOS_CONF*256
          CHARACTER  EOM_CONF*256
          CHARACTER  SNX_CONF*256
          CHARACTER  SKED_EXE*256
          LOGICAL*4  SNR_PLOT
          LOGICAL*4  SNR_HIST
          LOGICAL*4  EOPM_ONLY_SINGLE_BASELINE
          CHARACTER  TMP_DIR*256
          CHARACTER  SESUPD_LOG*256
          CHARACTER  WGET_EXE*256
	  INTEGER*4  NUM_USED_MIN
!
          CHARACTER  SESS_CODE*6
          CHARACTER  DB_NAME*10
          CHARACTER  ARC_LINE*256
!
! ------- Status
!
          CHARACTER  STS(M_OPA)*1
!
! ------- Action
!
          CHARACTER  ACT(M_OPA)*1
          INTEGER*4  IACT
          LOGICAL*4  FL_CONFIRM
!
          INTEGER*4  DB_VERSION
          REAL*8     MIN_DURATION
          REAL*8     MAX_DURATION
!
          INTEGER*4  LAST_FIELD
      END TYPE  OPA__STRU  !  OPA__STRU !
!
! <<<<< end of INCLUDE-BLOCK  opa.i
!
