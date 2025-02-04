!
! >>>>> INCLUDE-BLOCK with description of data structures used by  DB_IMPORT
!
!       db_import.i  2000.10.17  v 1.1  Leonid Petrov  27-MAR-2002 09:47:57
!
      CHARACTER  HELP01__DBI*20
      DATA       HELP01__DBI    / 'db_import_01.txt    '/
      INTEGER*4  YEAR_EARLIEST
      PARAMETER  ( YEAR_EARLIEST = 1979 )
      INTEGER*4  M_DBS
      PARAMETER  ( M_DBS = 32768 )
      CHARACTER    CVRB_RANGE*6
      PARAMETER  ( CVRB_RANGE = '[0, 2]' )
      INTEGER*4  IVRB__MIN, IVRB__MAX
      PARAMETER  ( IVRB__MIN = 0 )
      PARAMETER  ( IVRB__MAX = 2 )
      INTEGER*4  MYEAR_START, MYEAR_END
      PARAMETER  ( MYEAR_START = 1972 )
      PARAMETER  ( MYEAR_END   = 2065 )
      TYPE      DBI__STRU
          INTEGER*4  FIRST_FIELD
          CHARACTER  IVS_DB_URL*256
          CHARACTER  WGET_EXE*256
          CHARACTER  GZIP_EXE*256
          CHARACTER  TMP_DIR*256
          CHARACTER  GET_FILE*256
          CHARACTER  NOGET_FILE*256
          CHARACTER  INCOMING_DIR*256
          CHARACTER  LOG_FILE*256
          CHARACTER  MAIL_COMMAND*256
          CHARACTER  EMAIL_IMPORT*256
          CHARACTER  DATE_START*10
          CHARACTER  DATE_END*10
!
          CHARACTER  CONFIG_FILE*256
          CHARACTER  URL_FILE*256
!
          INTEGER*4  MJD_START
          INTEGER*4  MJD_END
          INTEGER*4  VRB
          INTEGER*4  LAST_FIELD
      END TYPE  DBI__STRU  !  DBI__STRU  !
!
      INTEGER*4    M_PAR
      PARAMETER  ( M_PAR = 12 )
!
! <<<<< end of INCLUDE-BLOCK  db_import.i
!
