!
! >>>>> INCLUDE-BLOCK with description of data structures used by  GEX
!
!       gex.i  2000.12.29  v 1.3  Leonid Petrov  31-MAY-2001 11:15:41
!
      CHARACTER  GEX__LABEL*20
      PARAMETER  ( GEX__LABEL = 'geo_export ver. 1.3 ' )
      CHARACTER  HELP02__GEX*17
      PARAMETER  ( HELP02__GEX = 'geo_export_01.txt' )
      INTEGER*4  M_PAR
      PARAMETER  ( M_PAR = 11 )
      INTEGER*4  MAX_CR_LEN
      PARAMETER  ( MAX_CR_LEN = 512 ) ! Maximal length of the correlation report
!
      TYPE      GEX__STRU
          INTEGER*4  FIRST_FIELD
          CHARACTER  CONFIG_FILE*256
!
          CHARACTER  MASTER_DIR*256
          CHARACTER  URL_IVSCONTROL*256
          CHARACTER  DBEDIT_TEMPL_CONF*256
          CHARACTER  IVS_DB_URL*256
          CHARACTER  CORREP_EMAIL*256
          LOGICAL*4  CORREP_PROMPT
          CHARACTER  EMAIL_COMMAND*256
          CHARACTER  CORRTYPE*8
          CHARACTER  DBS_CONF*256
          CHARACTER  SCRATCH_DIR*256
          CHARACTER  SCRATCH_FIL*256
          CHARACTER  LOG_FIL*256
          CHARACTER  OUT_FIL*256
          CHARACTER  ERR_FIL*256
          CHARACTER  PRG_FIL*256
          CHARACTER  CORREP_FIL*256
          CHARACTER  WGET_EXE*256
!
          CHARACTER  LOG*256
          CHARACTER  DBE*256
!
          CHARACTER  SESS_CODE*16
          CHARACTER  SESS_DESC*80
          CHARACTER  DATE_TAG*10
          CHARACTER  DB_NAME*10
          CHARACTER  DBNAME_X*10
          CHARACTER  DBNAME_S*10
          INTEGER*4  YEAR
          INTEGER*4  DOY
          INTEGER*4  LAST_FIELD
      END TYPE  GEX__STRU  !  GEX__STRU !
!
! <<<<< end of INCLUDE-BLOCK  gex.i
!
