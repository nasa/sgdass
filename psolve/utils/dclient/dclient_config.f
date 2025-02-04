      SUBROUTINE DCLIENT_CONFIG ( DATA_TYPE, CONFIG_FILE, DCLIENT, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine DCLIENT_CONFIG  is a part of dclient software. It reads    *
! *   CONFIG_FILE with configuration of a dclient utility like           *
! *   database_submit, parses its content and fills field of a record    *
! *   DCLIENT.                                                           *
! *   Syntax of configuration file:                                      *
! *   1) Lines starting with ##  ( Two symbols: <DIES> <DIES>) are       *
! *      recognized as comments and ignored.                             *
! *   2) Lines starting with #  ( Two symbols: <DIES> <BLANK>) are       *
! *      considered as a specifications of dclient configuration.        *
! *      Format: keyword, one or more delimiters, value.                 *
! *      Keywords are case-insensitive, but values are case-sensitive.   *
! *      Delimiter is one of <BLANK> <TAB> <BINARY_ZERO>.                *
! *   No defaults can be used. All keywords are to be specified.         *
! *                                                                      *
! * ________________________ Input parameters: _________________________ *
! *                                                                      *
! *  DATA_TYPE   ( CHARACTER ) -- Type of data to be submitted.          *
! *                               One of DBH or EOPS.                    *
! *  CONFIG_FILE ( CHARACTER ) -- File name of the dserver configuration *
! *                               file.                                  *
! *                                                                      *
! * ________________________ Output parameters: ________________________ *
! *                                                                      *
! * DCLIENT ( RECORD         ) -- Object with data structure for keeping *
! *                               configuration of dclient.              *
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
! *    IUER ( INTEGER*4, OPT ) -- Universal error handler.               *
! *                           Input: switch IUER=0 -- no error messages  *
! *                                  will be generated even in the case  *
! *                                  of error. IUER=-1 -- in the case of *
! *                                  error the message will be put on    *
! *                                  stdout.                             *
! *                           Output: 0 in the case of successful        *
! *                                   completion and non-zero in the     *
! *                                   case of error.                     *
! *                                                                      *
! *  ###  30-SEP-99  DCLIENT_CONFIG  v2.2 (c) L. Petrov 05-JUN-2008 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'dclient.i'
      TYPE ( DCLIENT__STRU ) ::  DCLIENT
      CHARACTER  DATA_TYPE*(*), CONFIG_FILE*(*)
      INTEGER*4  IUER
      INTEGER*4  M_CNF, MIND
      PARAMETER  ( M_CNF = 64, MIND = 32 )
      INTEGER*4  LEN_DSR, LIND, IND(2,MIND), N_BUF, I_PAR, IL, J1, IER
      CHARACTER  BUF(M_CNF)*512, DELIM*3, STR*20, STR1*20
      PARAMETER  ( DELIM = CHAR(0)//CHAR(32)//CHAR(9) )
      LOGICAL*4  FL_EXIST
      INTEGER*4, EXTERNAL :: ILEN, I_LEN
!
! --- Clean record DCLIENT
!
      LEN_DSR = LOC(DCLIENT%LAST_FIELD) - LOC(DCLIENT%FIRST_FIELD) + 4
      CALL NOUT ( LEN_DSR, DCLIENT )
!
! --- Save name of the configuration file
!
      DCLIENT%CONFIG_FILE = CONFIG_FILE
!
! --- Read configuration file into the buffer BUF
!
      CALL ERR_PASS ( IUER, IER )
      CALL RD_TEXT  ( CONFIG_FILE, M_BUF, BUF, N_BUF, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 5121, IUER, 'DCLIENT_CONFIG', 'Error in attempt '// &
     &         'to open dclient configuration file '//CONFIG_FILE )
           RETURN
      END IF
!
! --- Scan the buffer with copy of configuration file
!
      I_PAR = 0
      DO 410 J1=1,N_BUF
!
! ------ Skip comments
!
         IF ( ILEN(BUF(J1)) .EQ.  0   ) GOTO 410
         IF ( BUF(J1)(1:1)  .NE. '#'  ) GOTO 410
         IF ( BUF(J1)(1:2)  .EQ. '#!' ) GOTO 410
         IF ( BUF(J1)(1:2)  .EQ. '##' ) GOTO 410
!
! ------ Split the line onto words
!
         CALL EXWORD ( BUF(J1), MIND, LIND, IND, DELIM, -3 )
         CALL CLRCH ( STR )
         CALL INCH  ( J1, STR )
!
! ------ It should be exactly three words. Let's check it
!
         IF ( LIND .LT. 3 ) THEN
              CALL ERR_LOG ( 5122, IUER, 'DCLIENT_CONFIG', 'Error in '// &
     &            'parsing the '//STR(1:I_LEN(STR))//'-th line in the '// &
     &            'configuration file '//CONFIG_FILE(1:I_LEN(CONFIG_FILE))// &
     &            ': too few words' )
              RETURN
         END IF
!
! ------ Well. Three words. Parse them
!
! ------ Transform the keyword to the letters of upper case
!
         CALL TRAN ( 11, BUF(J1)(IND(1,2):IND(2,2)), &
     &                   BUF(J1)(IND(1,2):IND(2,2))  )
!
         IF ( BUF(J1)(IND(1,2):IND(2,2)) .EQ. 'DATA_CENTER:' ) THEN
              DCLIENT%DATA_CENTER = BUF(J1)(IND(1,3):IND(2,LIND))
            ELSE IF ( BUF(J1)(IND(1,2):IND(2,2)) .EQ. 'DSERVER_EMAIL:' ) THEN
              DCLIENT%DSERVER_EMAIL = BUF(J1)(IND(1,3):IND(2,LIND))
            ELSE IF ( BUF(J1)(IND(1,2):IND(2,2)) .EQ. 'FTP_DIR:' ) THEN
              DCLIENT%FTP_DIR = BUF(J1)(IND(1,3):IND(2,3))
!
! ----------- Add the trailing slash to the directory name if it was omitted
!
              IL = ILEN(DCLIENT%FTP_DIR)
              IF ( DCLIENT%FTP_DIR(IL:IL) .NE. '/' ) THEN
                   DCLIENT%FTP_DIR = DCLIENT%FTP_DIR(1:IL)//'/'
              END IF
            ELSE IF ( BUF(J1)(IND(1,2):IND(2,2)) .EQ. 'URL_PREFIX:' ) THEN
              DCLIENT%URL_PREFIX = BUF(J1)(IND(1,3):IND(2,3))
!
! ----------- Add the trailing slash to the directory name if it was omitted
!
              IL = ILEN(DCLIENT%URL_PREFIX)
              IF ( DCLIENT%URL_PREFIX(IL:IL) .NE. '/' ) THEN
                   DCLIENT%URL_PREFIX = DCLIENT%URL_PREFIX(1:IL)//'/'
              END IF
            ELSE IF ( BUF(J1)(IND(1,2):IND(2,2)) .EQ. 'EOP_SUFFIX:' .AND. &
     &                DATA_TYPE(1:4) .EQ. 'EOPS' ) THEN
              DCLIENT%EOP_SUFFIX = BUF(J1)(IND(1,3):IND(2,LIND))
            ELSE IF ( BUF(J1)(IND(1,2):IND(2,2)) .EQ. 'EOP_SUFFIX:' .AND. &
     &                DATA_TYPE(1:4) .EQ. 'EOPI' ) THEN
              DCLIENT%EOP_SUFFIX = BUF(J1)(IND(1,3):IND(2,LIND))
            ELSE IF ( BUF(J1)(IND(1,2):IND(2,2)) .EQ. 'MAIL_COMMAND:' ) THEN
              DCLIENT%MAIL_COMMAND = BUF(J1)(IND(1,3):IND(2,LIND))
            ELSE IF ( BUF(J1)(IND(1,2):IND(2,2)) .EQ. 'CONFIRM_EMAIL:' ) THEN
              DCLIENT%CONFIRM_EMAIL = BUF(J1)(IND(1,3):IND(2,LIND))
            ELSE IF ( BUF(J1)(IND(1,2):IND(2,2)) .EQ. 'SUCCESS_EMAIL:' ) THEN
              DCLIENT%SUCCESS_EMAIL = BUF(J1)(IND(1,3):IND(2,LIND))
            ELSE IF ( BUF(J1)(IND(1,2):IND(2,2)) .EQ. 'SUBMIT_LOG:' ) THEN
              DCLIENT%SUBMIT_LOG = BUF(J1)(IND(1,3):IND(2,3))
            ELSE IF ( BUF(J1)(IND(1,2):IND(2,2)) .EQ. 'TMP_DIR:'    ) THEN
              DCLIENT%TMP_DIR = BUF(J1)(IND(1,3):IND(2,3))
!
! ----------- Add the trailing slash to the directory name if it was omitted
!
              IL = ILEN(DCLIENT%TMP_DIR)
              IF ( DCLIENT%TMP_DIR(IL:IL) .NE. '/' ) THEN
                   DCLIENT%TMP_DIR = DCLIENT%TMP_DIR(1:IL)//'/'
              END IF
            ELSE IF ( BUF(J1)(IND(1,2):IND(2,2)) .EQ. 'DO_LINK:' ) THEN
              DCLIENT%DO_LINK = BUF(J1)(IND(1,3):IND(2,3))
              CALL TRAN ( 11, DCLIENT%DO_LINK, DCLIENT%DO_LINK )
              IF ( DCLIENT%DO_LINK(1:3) .NE. 'YES' .AND. &
     &             DCLIENT%DO_LINK(1:2) .NE. 'NO'        ) THEN
!
                   CALL ERR_LOG ( 5124, IUER, 'DCLIENT_CONFIG', 'Error in '// &
     &                 'parsing the '//STR(1:I_LEN(STR))//'-th line in the '// &
     &                 'configuration file '// &
     &                  CONFIG_FILE(1:I_LEN(CONFIG_FILE))// &
     &                 ': unsupported values '//BUF(J1)(IND(1,3):IND(2,3))// &
     &                 ' of the keyword DO_LINK: YES or NO expected' )
                   RETURN
              END IF
            ELSE IF ( BUF(J1)(IND(1,2):IND(2,2)) .EQ. 'DO_PURGE:' ) THEN
              DCLIENT%DO_PURGE = BUF(J1)(IND(1,3):IND(2,3))
              CALL TRAN ( 11, DCLIENT%DO_PURGE, DCLIENT%DO_PURGE )
              IF ( DCLIENT%DO_PURGE(1:3) .NE. 'YES' .AND. &
     &             DCLIENT%DO_PURGE(1:2) .NE. 'NO'        ) THEN
!
                   CALL ERR_LOG ( 5125, IUER, 'DCLIENT_CONFIG', 'Error in '// &
     &                 'parsing the '//STR(1:I_LEN(STR))//'-th line in the '// &
     &                 'configuration file '// &
     &                  CONFIG_FILE(1:I_LEN(CONFIG_FILE))// &
     &                 ': unsupported values '//BUF(J1)(IND(1,3):IND(2,3))// &
     &                 ' of the keyword DO_PURGE: YES or NO expected' )
                   RETURN
              END IF
            ELSE IF ( BUF(J1)(IND(1,2):IND(2,2)) .EQ. 'CUSTOM_SCRIPT:' ) THEN
              DCLIENT%CUSTOM_SCRIPT = BUF(J1)(IND(1,3):IND(2,3))
              IF ( DCLIENT%CUSTOM_SCRIPT == 'no'   .OR. &
     &             DCLIENT%CUSTOM_SCRIPT == 'NO'   .OR. &
     &             DCLIENT%CUSTOM_SCRIPT == 'none' .OR. &
     &             DCLIENT%CUSTOM_SCRIPT == 'NONE'      ) THEN
!
                   CALL CLRCH ( DCLIENT%CUSTOM_SCRIPT )
                 ELSE 
                   INQUIRE ( FILE=DCLIENT%CUSTOM_SCRIPT, EXIST=FL_EXIST )
                   IF ( .NOT. FL_EXIST ) THEN
                        CALL ERR_LOG ( 5126, IUER, 'DCLIENT_CONFIG', &
     &                      'Eror in parsing the '//STR(1:I_LEN(STR))// &
     &                      '-th line in the configuration file '// &
     &                       CONFIG_FILE(1:I_LEN(CONFIG_FILE))// &
     &                      ': Cannot find file CUSTOM_SCRIPT '// &
     &                      DCLIENT%CUSTOM_SCRIPT )
                        RETURN
                   END IF
              END IF
            ELSE
              CALL ERR_LOG ( 5127, IUER, 'DCLIENT_CONFIG', 'Error in '// &
     &            'parsing the '//STR(1:I_LEN(STR))//'-th line in the '// &
     &            'configuration file '//CONFIG_FILE(1:I_LEN(CONFIG_FILE))// &
     &            ': unsupported keyword '//BUF(J1)(IND(1,2):IND(2,2)) )
              RETURN
         END IF
         I_PAR = I_PAR + 1
 410  CONTINUE
!
! --- Check: did we define all fields of dserver?
!
      IF ( ( DATA_TYPE(1:3) .EQ. 'DBH'  .AND.  I_PAR .LT. M_PAR_DBS ) .OR. &
     &     ( DATA_TYPE(1:4) .EQ. 'EOPS' .AND.  I_PAR .LT. M_PAR_EOS )     ) THEN
           CALL CLRCH ( STR  )
           CALL INCH  ( I_PAR, STR  )
           CALL CLRCH ( STR1 )
           IF ( DATA_TYPE(1:3) .EQ. 'DBH'  ) CALL INCH  ( M_PAR_DBS, STR1 )
           IF ( DATA_TYPE(1:4) .EQ. 'EOPS' ) CALL INCH  ( M_PAR_EOS, STR1 )
           CALL ERR_LOG ( 5128, IUER, 'DCLIENT_CONFIG', 'Not all parameters '// &
     &         'were found in dserver configuration file '// &
     &          CONFIG_FILE(1:I_LEN(CONFIG_FILE))//' --  '//STR(1:I_LEN(STR))// &
     &         ' instead of '//STR1 )
           RETURN
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!   DCLIENT_CONFIG  #!#
