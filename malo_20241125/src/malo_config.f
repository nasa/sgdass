      SUBROUTINE MALO_CONFIG ( CONFIG_FILE, MALO, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine MALO_CONFIG parses configration file and fills fields of   *
! *   the  MALO%CONF object.                                             *
! *                                                                      *
! *  ### 18-MAY-2004  MALO_CONFIG  v1.3 (c)  L. Petrov  23-MAY-2017 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'malo.i'
      CHARACTER  CONFIG_FILE*(*)
      TYPE     ( MALO__TYPE ) :: MALO
      INTEGER*4  IUER
      INTEGER*4  MBUF, MIND
      PARAMETER  ( MBUF = 64, MIND = 64 ) 
      CHARACTER  BUF(MBUF)*256, STR*32, STR1*32, REG*7
      PARAMETER  ( REG = CHAR(0)//CHAR(9)//CHAR(32)//':'//'='//'"'//"'" )
      LOGICAL*4  LEX
      INTEGER*4  NBUF, K_CNF, IP, J1, IND(2,MIND), LIND, IFMT_VER, IER
      INTEGER*4, EXTERNAL ::ILEN, I_LEN
!
! --- Initialization
!
      CALL NOUT ( SIZEOF(MALO%CONF), MALO%CONF ) 
!
      CALL ERR_PASS ( IUER, IER )
      CALL MALO_CHECK_SHARE_FILE ( CONFIG_FILE, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 2861, IUER, 'MALO_CONFIG', 'MALO configuration '// &
     &         'file '//CONFIG_FILE(1:I_LEN(CONFIG_FILE))//' was not '// &
     &         'found' )
           RETURN 
      END IF
!
! --- Check, whether configuration file exists
!
      INQUIRE ( FILE=CONFIG_FILE, EXIST=LEX ) 
      IF ( .NOT. LEX ) THEN
           CALL ERR_LOG ( 2862, IUER, 'MALO_CONFIG', 'MALO configuration '// &
     &         'file '//CONFIG_FILE(1:I_LEN(CONFIG_FILE))//' was not '// &
     &         'found' )
           RETURN 
      END IF
!
! --- Read configuration file into buffer BUF
!
      CALL ERR_PASS ( IUER, IER )
      CALL RD_TEXT  ( CONFIG_FILE, MBUF, BUF, NBUF, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 2863, IUER, 'MALO_CONFIG', 'Error in an attempt '// &
     &         'to read MALO configuration file '//CONFIG_FILE )
           RETURN 
      END IF
      IF ( BUF(1) == MALO_CONFIG__LABEL_V1 ) THEN
           IFMT_VER = 1
        ELSE IF ( BUF(1) == MALO_CONFIG__LABEL ) THEN
           IFMT_VER = 0
        ELSE 
           CALL CLRCH ( STR )
           CALL TRAN  ( 13, BUF(1), STR )
           CALL ERR_LOG ( 2864, IUER, 'MALO_CONFIG', 'Unsupported format of '// &
     &         'MALO configuration file '//CONFIG_FILE(1:I_LEN(CONFIG_FILE))// &
     &         ' -- a label '//MALO_CONFIG__LABEL//' was expected but got '// &
     &         STR )
           RETURN 
      END IF
!
! --- Scan the buffer with configuration file line-by-line
!
      K_CNF = 0
      DO 410 J1=1,NBUF
         CALL CLRCH ( STR ) 
         CALL INCH  ( J1, STR )
         IF ( BUF(J1)(1:1)  .EQ. '#' ) GOTO 410
         IF ( ILEN(BUF(J1)) .EQ.  0  ) GOTO 410
!
! ------ Split the J1-th line into words
!
         IP = INDEX ( BUF(J1), '!' ) - 1
         IF ( IP .LE. 0 ) IP = ILEN(BUF(J1))
         CALL ERR_PASS ( IUER, IER )
         CALL EXWORD ( BUF(J1)(1:IP), MIND, LIND, IND, REG, IER ) 
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 2865, IUER, 'MALO_CONFIG', 'Error in parsing '// &
     &            'the '//STR(1:I_LEN(STR))//' of configuration file '// &
     &             CONFIG_FILE(1:I_LEN(CONFIG_FILE))//' into words' )
              RETURN 
         END IF
!
! ------ Check the first word. Make approriate action for each eyword
!
         IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'MALO_DEFAULT_DAT' ) THEN
              MALO%CONF%DEFAULT_DATE = BUF(J1)(IND(1,2):IND(2,LIND)) 
              CALL ERR_PASS ( IUER, IER ) 
              CALL DATE_TO_TIME ( MALO%CONF%DEFAULT_DATE, MALO%MJD_DEF, &
     &                            MALO%TAI_DEF, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 2866, IUER, 'MALO_CONFIG', 'Wrong value '// &
     &                  BUF(J1)(IND(1,2):IND(2,2))//' of the keyword '// &
     &                 'MALO_DEFAULT_DATE was found during parsing '// &
     &                 'configuration file '// &
     &                  CONFIG_FILE(1:I_LEN(CONFIG_FILE))// &
     &                 ' -- wrong date format' )
                   RETURN 
              END IF
           ELSE IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'MALO_SURFACE_TYPE' ) THEN
              MALO%CONF%SURFACE_TYPE = BUF(J1)(IND(1,2):IND(2,LIND)) 
              CALL TRAN ( 11, MALO%CONF%SURFACE_TYPE, MALO%CONF%SURFACE_TYPE )
              IF ( MALO%CONF%SURFACE_TYPE == MALO__LAND ) THEN
                   CONTINUE 
                 ELSE IF ( MALO%CONF%SURFACE_TYPE == MALO__OCEAN ) THEN
                   CONTINUE 
                 ELSE IF ( MALO%CONF%SURFACE_TYPE == 'EVERYTHING' ) THEN
                   MALO%CONF%SURFACE_TYPE = MALO__ALL
                 ELSE IF ( MALO%CONF%SURFACE_TYPE == MALO__ALL ) THEN
                   CONTINUE 
                 ELSE 
                   CALL ERR_LOG ( 2867, IUER, 'MALO_CONFIG', 'Unsupported '// &
     &                 'surface type: '//MALO%CONF%SURFACE_TYPE// &
     &                 ' one of LAND, OCEAN, or ALL were expected' )
                   RETURN 
              END IF
           ELSE IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'MALO_FINAM_LS_MASK' ) THEN
              MALO%CONF%FINAM_LS_MASK = BUF(J1)(IND(1,2):IND(2,LIND)) 
              IF ( BUF(J1)(IND(1,2):IND(2,LIND)) .NE. 'NONE' .AND. &
     &             BUF(J1)(IND(1,2):IND(2,LIND)) .NE. 'None'       ) THEN
                   CALL ERR_PASS ( IUER, IER )
                   CALL MALO_CHECK_MODEL_FILE ( MALO%CONF%FINAM_LS_MASK, IER )
                   IF ( IER .NE. 0 ) THEN
                        CALL ERR_LOG ( 2868, IUER, 'MALO_CONFIG', 'Wrong value '// &
     &                       BUF(J1)(IND(1,2):IND(2,LIND))//' of the keyword '// &
     &                      'MALO_FINAM_LS_MASK was found during parsing '// &
     &                      'configuration file '// &
     &                       CONFIG_FILE(1:I_LEN(CONFIG_FILE))// &
     &                      ' -- File '//TRIM(MALO%CONF%FINAM_LS_MASK)//' was not found' )
                        RETURN 
                   END IF
              END IF
           ELSE IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'MALO_UPGRID_LS_MASK' ) THEN
              MALO%CONF%UPGRID_LS_MASK = BUF(J1)(IND(1,2):IND(2,LIND)) 
              IF ( BUF(J1)(IND(1,2):IND(2,LIND)) .NE. 'NONE' .AND. &
     &             BUF(J1)(IND(1,2):IND(2,LIND)) .NE. 'None'       ) THEN
                   CALL ERR_PASS ( IUER, IER )
                   CALL MALO_CHECK_MODEL_FILE ( MALO%CONF%UPGRID_LS_MASK, IER )
                   IF ( IER .NE. 0 ) THEN
                        CALL ERR_LOG ( 2869, IUER, 'MALO_CONFIG', 'Wrong value '// &
     &                       BUF(J1)(IND(1,2):IND(2,LIND))//' of the keyword '// &
     &                      'MALO_UPGRID_LS_MASK was found during parsing '// &
     &                      'configuration file '// &
     &                       CONFIG_FILE(1:I_LEN(CONFIG_FILE))// &
     &                      ' -- File '//TRIM(MALO%CONF%UPGRID_LS_MASK)//' was not found' )
                        RETURN 
                   END IF
              END IF
           ELSE IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'MALO_SC_FILE' ) THEN
              MALO%CONF%SC_FILE = BUF(J1)(IND(1,2):IND(2,LIND)) 
              IF ( BUF(J1)(IND(1,2):IND(2,LIND)) .NE. 'NONE' .AND. &
     &             BUF(J1)(IND(1,2):IND(2,LIND)) .NE. 'None'       ) THEN
                   CALL ERR_PASS ( IUER, IER )
                   CALL MALO_CHECK_MODEL_FILE ( MALO%CONF%SC_FILE, IER )
                   IF ( IER .NE. 0 ) THEN
                        CALL ERR_LOG ( 2870, IUER, 'MALO_CONFIG', 'Wrong value '// &
     &                       BUF(J1)(IND(1,2):IND(2,LIND))//' of the keyword '// &
     &                      'MALO_SC_FILE was found during parsing '// &
     &                      'configuration file '// &
     &                       CONFIG_FILE(1:I_LEN(CONFIG_FILE))// &
     &                      ' -- File '//TRIM(MALO%CONF%SC_FILE)//' was not found' )
                        RETURN 
                   END IF
              END IF
           ELSE IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'MALO_FINAM_MODEL' ) THEN
              MALO%CONF%FINAM_MODEL = BUF(J1)(IND(1,2):IND(2,LIND)) 
              IF ( BUF(J1)(IND(1,2):IND(2,LIND)) .NE. 'NONE' .AND. &
     &             BUF(J1)(IND(1,2):IND(2,LIND)) .NE. 'None'       ) THEN
!
                   CALL ERR_PASS ( IUER, IER )
                   CALL MALO_CHECK_MODEL_FILE ( MALO%CONF%FINAM_MODEL, IER )
                   IF ( IER .NE. 0 ) THEN
                        CALL ERR_LOG ( 2871, IUER, 'MALO_CONFIG', 'Wrong value '// &
     &                       BUF(J1)(IND(1,2):IND(2,LIND))//' of the keyword '// &
     &                      'MALO_FINAM_MODEL was found during parsing '// &
     &                      'configuration file '// &
     &                       CONFIG_FILE(1:I_LEN(CONFIG_FILE))// &
     &                      ' -- File '//TRIM(MALO%CONF%FINAM_MODEL)//' was not found' )
                       RETURN 
                   END IF
              END IF
           ELSE IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'MALO_MODEL_CODE' ) THEN
              READ ( UNIT=BUF(J1)(IND(1,2):IND(2,2)), FMT='(I2)', IOSTAT=IER ) MALO%CONF%MODEL_CODE
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 2872, IUER, 'MALO_CONFIG', 'Wrong value '// &
     &                  BUF(J1)(IND(1,2):IND(2,LIND))//' of the keyword '// &
     &                 'MALO_MODEL_CODE was found during parsing '// &
     &                 'configuration file '// &
     &                  CONFIG_FILE(1:I_LEN(CONFIG_FILE))// &
     &                 ' -- an integere number was expected' )
                   RETURN 
              END IF
              IF ( MALO%CONF%MODEL_CODE < 0 .OR. MALO%CONF%MODEL_CODE > MALO__MFS ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( MALO__MFS, STR )
                   CALL ERR_LOG ( 2873, IUER, 'MALO_CONFIG', 'Model code '// &
     &                  BUF(J1)(IND(1,2):IND(2,LIND))//' is out of range [0,'// &
     &                  STR(1:I_LEN(STR))//']. Error was detected during '// &
     &                 'parsing configuration file '//CONFIG_FILE  )
                   RETURN 
              END IF
           ELSE IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'MALO_MODEL_USE' ) THEN
              MALO%CONF%MODEL_USE = BUF(J1)(IND(1,2):IND(2,LIND)) 
              CALL TRAN ( 11, MALO%CONF%MODEL_USE, MALO%CONF%MODEL_USE )
              IF ( MALO%CONF%MODEL_USE == MALO__MOD_IGNORE) THEN
                   CONTINUE 
                 ELSE IF ( MALO%CONF%MODEL_USE == MALO__MOD_SUBTRACT ) THEN
                   CONTINUE 
                 ELSE IF ( MALO%CONF%MODEL_USE == MALO__MOD_HAR_ONLY ) THEN
                   CONTINUE 
                 ELSE IF ( MALO%CONF%MODEL_USE == MALO__MOD_SUB_NOHAR ) THEN
                   CONTINUE 
                 ELSE IF ( MALO%CONF%MODEL_USE == MALO__OCEAN ) THEN
                   CONTINUE 
                 ELSE IF ( MALO%CONF%MODEL_USE == MALO__NONE ) THEN
                   MALO%CONF%MODEL_USE = MALO__MOD_IGNORE
                 ELSE 
                   CALL ERR_LOG ( 2874, IUER, 'MALO_CONFIG', 'Unsupported '// &
     &                 'surface type: '//MALO%CONF%MODEL_USE// &
     &                 ' one of Ignore, Subtract, Only_mod, or NONE were expected' )
                   RETURN 
              END IF
            ELSE IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'LOVE_FILE' ) THEN
              MALO%CONF%LOVE_FILE = BUF(J1)(IND(1,2):IND(2,LIND)) 
              CALL ERR_PASS ( IUER, IER )
              CALL MALO_CHECK_SHARE_FILE ( MALO%CONF%LOVE_FILE, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 2875, IUER, 'MALO_CONFIG', 'Wrong value '// &
     &                  BUF(J1)(IND(1,2):IND(2,LIND))//' of the keyword '// &
     &                 'LOVE_FILE  was found during parsing '// &
     &                 'configuration file '// &
     &                  CONFIG_FILE(1:I_LEN(CONFIG_FILE))// &
     &                 ' -- File was not found' )
                   RETURN 
              END IF
            ELSE IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'EPHEDISP_FINAM_FMT' ) THEN
              MALO%CONF%EPHEDISP_FINAM_FMT = BUF(J1)(IND(1,2):IND(2,LIND)) 
              CALL ERR_PASS ( IUER, IER )
              CALL MALO_CHECK_SHARE_FILE ( MALO%CONF%EPHEDISP_FINAM_FMT, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 2876, IUER, 'MALO_CONFIG', 'Wrong value '// &
     &                  BUF(J1)(IND(1,2):IND(2,LIND))//' of the keyword '// &
     &                 'EPHEDISP_FINAM_FMT was found during parsing '// &
     &                 'configuration file '// &
     &                  CONFIG_FILE(1:I_LEN(CONFIG_FILE))// &
     &                 ' -- File was not found' )
                   RETURN 
              END IF
            ELSE IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'HARPOS_FINAM_FMT' ) THEN
              MALO%CONF%HARPOS_FINAM_FMT = BUF(J1)(IND(1,2):IND(2,LIND)) 
              CALL ERR_PASS ( IUER, IER )
              CALL MALO_CHECK_SHARE_FILE ( MALO%CONF%HARPOS_FINAM_FMT, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 2877, IUER, 'MALO_CONFIG', 'Wrong value '// &
     &                  BUF(J1)(IND(1,2):IND(2,LIND))//' of the keyword '// &
     &                 'HARPOS_FINAM_FMT was found during parsing '// &
     &                 'configuration file '// &
     &                  CONFIG_FILE(1:I_LEN(CONFIG_FILE))// &
     &                 ' -- File was not found' )
                   RETURN 
              END IF
            ELSE IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'SPHE_FINAM_FMT' ) THEN
              MALO%CONF%SPHE_FINAM_FMT = BUF(J1)(IND(1,2):IND(2,LIND)) 
              CALL ERR_PASS ( IUER, IER )
              ier = 0
              CALL MALO_CHECK_SHARE_FILE ( MALO%CONF%SPHE_FINAM_FMT, IER )
!@              IF ( IER .NE. 0 ) THEN
!@                   CALL ERR_LOG ( 2878, IUER, 'MALO_CONFIG', 'Wrong value '// &
!@     &                  BUF(J1)(IND(1,2):IND(2,LIND))//' of the keyword '// &
!@     &                 'SPHE_FINAM_FMT was found during parsing '// &
!@     &                 'configuration file '// &
!@     &                  CONFIG_FILE(1:I_LEN(CONFIG_FILE))// &
!@     &                 ' -- File was not found' )
!@                   RETURN 
!@              END IF
            ELSE IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'LOA_FINAM_FMT' ) THEN
              MALO%CONF%LOA_FINAM_FMT = BUF(J1)(IND(1,2):IND(2,LIND)) 
              CALL ERR_PASS ( IUER, IER )
              ier = 0
              CALL MALO_CHECK_SHARE_FILE ( MALO%CONF%LOA_FINAM_FMT, IER )
!@              IF ( IER .NE. 0 ) THEN
!@                   CALL ERR_LOG ( 2879, IUER, 'MALO_CONFIG', 'Wrong value '// &
!@     &                  BUF(J1)(IND(1,2):IND(2,LIND))//' of the keyword '// &
!@     &                 'LOA_FINAM_FMT was found during parsing '// &
!@     &                 'configuration file '// &
!@     &                  CONFIG_FILE(1:I_LEN(CONFIG_FILE))// &
!@     &                 ' -- File was not found' )
!@                   RETURN 
!@              END IF
            ELSE IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'LOA_FINAM_DESCR' ) THEN
              MALO%CONF%LOA_FINAM_DESCR = BUF(J1)(IND(1,2):IND(2,LIND)) 
              CALL ERR_PASS ( IUER, IER )
              CALL MALO_CHECK_SHARE_FILE ( MALO%CONF%LOA_FINAM_DESCR, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 2880, IUER, 'MALO_CONFIG', 'Wrong value '// &
     &                  BUF(J1)(IND(1,2):IND(2,LIND))//' of the keyword '// &
     &                 'LOA_FINAM_DESCR was found during parsing '// &
     &                 'configuration file '// &
     &                  CONFIG_FILE(1:I_LEN(CONFIG_FILE))// &
     &                 ' -- File was not found' )
                   RETURN 
              END IF
            ELSE IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'LOA_FINAM_COMM' ) THEN
              MALO%CONF%LOA_FINAM_COMM = BUF(J1)(IND(1,2):IND(2,LIND)) 
              CALL ERR_PASS ( IUER, IER )
              CALL MALO_CHECK_SHARE_FILE ( MALO%CONF%LOA_FINAM_COMM, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 2881, IUER, 'MALO_CONFIG', 'Wrong value '// &
     &                  BUF(J1)(IND(1,2):IND(2,LIND))//' of the keyword '// &
     &                 'LOA_FINAM_COMM was found during parsing '// &
     &                 'configuration file '// &
     &                  CONFIG_FILE(1:I_LEN(CONFIG_FILE))// &
     &                 ' -- File was not found' )
                   RETURN 
              END IF
            ELSE IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'STATION_FINAM' ) THEN
              MALO%CONF%STATION_FINAM = BUF(J1)(IND(1,2):IND(2,LIND)) 
              IF ( BUF(J1)(IND(1,2):IND(2,LIND)) .NE. 'NONE' .AND. &
     &             BUF(J1)(IND(1,2):IND(2,LIND)) .NE. 'None'       ) THEN
!
                   CALL ERR_PASS ( IUER, IER )
                   CALL MALO_CHECK_SHARE_FILE ( MALO%CONF%STATION_FINAM, IER )
                   IF ( IER .NE. 0 ) THEN
                        CALL ERR_LOG ( 2882, IUER, 'MALO_CONFIG', 'Wrong value '// &
     &                       BUF(J1)(IND(1,2):IND(2,LIND))//' of the keyword '// &
     &                      'STATION_FINAM was found during parsing '// &
     &                      'configuration file '// &
     &                       CONFIG_FILE(1:I_LEN(CONFIG_FILE))// &
     &                      ' -- File was not found' )
                        RETURN 
                   END IF
              END IF
            ELSE IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'AGRA_FINAM_DESC' ) THEN
              MALO%CONF%AGRA_FINAM_DESC = BUF(J1)(IND(1,2):IND(2,LIND)) 
              IF ( BUF(J1)(IND(1,2):IND(2,LIND)) .NE. 'NONE' .AND. &
     &             BUF(J1)(IND(1,2):IND(2,LIND)) .NE. 'None'       ) THEN
!
                   CALL ERR_PASS ( IUER, IER )
                   CALL MALO_CHECK_SHARE_FILE ( MALO%CONF%AGRA_FINAM_DESC, IER )
                   IF ( IER .NE. 0 ) THEN
                        CALL ERR_LOG ( 2883, IUER, 'MALO_CONFIG', 'Wrong value '// &
     &                       BUF(J1)(IND(1,2):IND(2,LIND))//' of the keyword '// &
     &                       'AGRA_FINAM_DESC was found during parsing '// &
     &                       'configuration file '// &
     &                       CONFIG_FILE(1:I_LEN(CONFIG_FILE))// &
     &                       ' -- File was not found' )
                        RETURN 
                   END IF
              END IF
            ELSE IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'AGRA_FINAM_FMT' ) THEN
              MALO%CONF%AGRA_FINAM_FMT = BUF(J1)(IND(1,2):IND(2,LIND)) 
              IF ( BUF(J1)(IND(1,2):IND(2,LIND)) .NE. 'NONE' .AND. &
     &             BUF(J1)(IND(1,2):IND(2,LIND)) .NE. 'None'       ) THEN
                   CALL ERR_PASS ( IUER, IER )
                   CALL MALO_CHECK_SHARE_FILE ( MALO%CONF%AGRA_FINAM_FMT, IER )
                   IF ( IER .NE. 0 ) THEN
                        CALL ERR_LOG ( 2884, IUER, 'MALO_CONFIG', 'Wrong value '// &
     &                       BUF(J1)(IND(1,2):IND(2,LIND))//' of the keyword '// &
     &                      'AGRA_FINAM_FMT was found during parsing '// &
     &                      'configuration file '// &
     &                       CONFIG_FILE(1:I_LEN(CONFIG_FILE))// &
     &                      ' -- File was not found' )
                        RETURN 
                   END IF
              END IF
            ELSE IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'OUTPUT_GRID_DEG' ) THEN
              READ ( UNIT=BUF(J1)(IND(1,2):IND(2,2)), FMT='(I6)', IOSTAT=IER ) MALO%CONF%OUTPUT_GRID_DEG
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 2885, IUER, 'MALO_CONFIG', 'Wrong value '// &
     &                  BUF(J1)(IND(1,2):IND(2,LIND))//' of the keyword '// &
     &                 'OUTPUT_GRID_LON was found during parsing '// &
     &                 'configuration file '// &
     &                  CONFIG_FILE(1:I_LEN(CONFIG_FILE))// &
     &                 ' -- an integere number was expected' )
                   RETURN 
              END IF
            ELSE IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'MONTHLY_EPH' ) THEN
              CALL TRAN ( 12, BUF(J1)(IND(1,2):IND(2,2)), BUF(J1)(IND(1,2):IND(2,2)) )
              IF ( BUF(J1)(IND(1,2):IND(2,2)) == 'yes' ) THEN
                   MALO%CONF%MONTHLY_EPH = .TRUE.
                 ELSE IF ( BUF(J1)(IND(1,2):IND(2,2)) == 'no' ) THEN
                   MALO%CONF%MONTHLY_EPH = .FALSE.
                 ELSE
                   CALL ERR_LOG ( 2886, IUER, 'MALO_CONFIG', 'Wrong value '// &
     &                  BUF(J1)(IND(1,2):IND(2,LIND))//' of the keyword '// &
     &                 'MONTHLY_EPH was found during parsing '// &
     &                 'configuration file '// &
     &                  CONFIG_FILE(1:I_LEN(CONFIG_FILE))// &
     &                 ' -- yes or no were expected' )
                   RETURN 
              END IF
            ELSE IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'BDS_FORMAT' ) THEN
              CALL TRAN ( 12, BUF(J1)(IND(1,2):IND(2,2)), BUF(J1)(IND(1,2):IND(2,2)) )
              IF ( BUF(J1)(IND(1,2):IND(2,2)) == 'yes' ) THEN
                   MALO%CONF%BDS_FORMAT = .TRUE.
                 ELSE IF ( BUF(J1)(IND(1,2):IND(2,2)) == 'no' ) THEN
                   MALO%CONF%BDS_FORMAT = .FALSE.
                 ELSE
                   CALL ERR_LOG ( 2887, IUER, 'MALO_CONFIG', 'Wrong value '// &
     &                  BUF(J1)(IND(1,2):IND(2,LIND))//' of the keyword '// &
     &                 'BDS_FORMAT was found during parsing '// &
     &                 'configuration file '// &
     &                  CONFIG_FILE(1:I_LEN(CONFIG_FILE))// &
     &                 ' -- yes or no were expected' )
                   RETURN 
              END IF
            ELSE IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'KEEP_EPHEDISP_ORIG' ) THEN
              CALL TRAN ( 12, BUF(J1)(IND(1,2):IND(2,2)), BUF(J1)(IND(1,2):IND(2,2)) )
              IF ( BUF(J1)(IND(1,2):IND(2,2)) == 'yes' ) THEN
                   MALO%CONF%KEEP_EPHEDISP_ORIG = .TRUE.
                 ELSE IF ( BUF(J1)(IND(1,2):IND(2,2)) == 'no' ) THEN
                   MALO%CONF%KEEP_EPHEDISP_ORIG = .FALSE.
                 ELSE
                   CALL ERR_LOG ( 2888, IUER, 'MALO_CONFIG', 'Wrong value '// &
     &                  BUF(J1)(IND(1,2):IND(2,LIND))//' of the keyword '// &
     &                 'KEEP_EPHEDISP_ORIG was found during parsing '// &
     &                 'configuration file '// &
     &                  CONFIG_FILE(1:I_LEN(CONFIG_FILE))// &
     &                 ' -- yes or no were expected' )
                   RETURN 
              END IF
           ELSE 
              CALL ERR_LOG ( 2889, IUER, 'MALO_CONFIG', 'Error in parsing '// &
     &            'the '//STR(1:I_LEN(STR))//' of configuration file '// &
     &             CONFIG_FILE(1:I_LEN(CONFIG_FILE))//' -- unknown keyword: '// &
     &             BUF(J1)(IND(1,1):IND(2,1)) )
              RETURN 
         END IF
         K_CNF = K_CNF + 1
 410  CONTINUE 
!
! --- Check, whether all keywords were specified
!
      IF ( K_CNF .NE. MALO__CNF ) THEN
           CALL CLRCH ( STR  )
           CALL CLRCH ( STR1 )
           CALL INCH  ( K_CNF, STR  )
           CALL INCH  ( MALO__CNF, STR1 )
           CALL ERR_LOG ( 2890, IUER, 'MALO_CONFIG', 'Error in parsing '// &
     &         'configuration file '//CONFIG_FILE(1:I_LEN(CONFIG_FILE))// &
     &         ' only '//STR(1:I_LEN(STR))//' keywords were found, while '// &
     &         STR1(1:I_LEN(STR1))//' keywrods are required' )
           RETURN 
      END IF
!
      IF ( MALO%CONF%MODEL_USE == MALO__MOD_SUBTRACT  .OR. &
     &     MALO%CONF%MODEL_USE == MALO__MOD_HAR_ONLY  .OR. &
     &     MALO%CONF%MODEL_USE == MALO__MOD_SUB_NOHAR      ) THEN
!
           CALL ERR_PASS ( IUER, IER )
           CALL MALO_MODC_PARSE ( MALO, IER )
           IF ( IER .NE. 0 ) THEN
                CALL CLRCH ( STR )
                CALL INCH  ( MALO%CONF%MODEL_CODE, STR )
                CALL ERR_LOG ( 2891, IUER, 'MALO_CONFIG', 'Error in setting '// &
     &              'components of the model with code '//STR(1:I_LEN(STR))// &
     &              'specified in the configuration file '//CONFIG_FILE )
                RETURN 
           END IF
         ELSE
           MALO%MDC_STATUS = MALO__UNDF
      END IF
!
! --- Store the name of the configuration file
!
      MALO%CONF%CONFIG = CONFIG_FILE
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  MALO_CONFIG   !#!#
