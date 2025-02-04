      SUBROUTINE DBI_CONFIG ( CONFIG_FILE, DBI, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine DBI_CONFIG reads file CONFIG_FILE with configuration of    *
! *   DB_IMPORT utility, parses its content and fills field of a record  *
! *   DBI. Syntax of configuration file:                                 *
! *   1) Lines starting with ##  ( Two symbols: <DIES> <DIES>) are       *
! *      recognized as comments and ignored.                             *
! *   2) Lines starting with #  ( Two symbols: <DIES> <BLANK>) are       *
! *      considered as a specifications of opa configuration.            *
! *      Format: keyword, one or more delimiters, value.                 *
! *      Keywords are case-insensitive, but values are case-sensitive.   *
! *      Delimiter is one of <BLANK> <TAB> <BINARY_ZERO>.                *
! *   No defaults can be used. All keywords are to be specified.         *
! *                                                                      *
! * ________________________ Input parameters: _________________________ *
! *                                                                      *
! *  CONFIG_FILE ( CHARACTER ) -- File name of the DB_IMPORT             *
! *                               configuration file.                    *
! *                                                                      *
! * ________________________ Output parameters: ________________________ *
! *                                                                      *
! *          DBI ( RECORD    ) -- Object with data structure for keeping *
! *                               configuration of DB_IMPORT utility.    *
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
! *  ###  17-OCT-2000  DBI_CONFIG  v1.3 (c)  L. Petrov  03-SEP-2019 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE    'db_import.i'
      CHARACTER  CONFIG_FILE*(*)
      TYPE ( DBI__STRU ) ::  DBI
      INTEGER*4  IUER, MIND, M_BUF
      PARAMETER  ( MIND= 32, M_BUF = 128 )
      CHARACTER  STR*80, STR1*80, BUF(M_BUF)*128, DELIM*3
      PARAMETER  ( DELIM = CHAR(0)//CHAR(32)//CHAR(9) )
      LOGICAL*4  LEX
      REAL*8     SEC
      CHARACTER  GET_CDATE*19, JD_TO_DATE*23, DATE_NOW*19, DATE_TOMORROW*23
      INTEGER*4  LEN_OPR, N_BUF, I_PAR, LIND, IND(2,MIND), MJD_NOW, IP, IL, &
     &           J1, IER
      ADDRESS__TYPE :: DIR_DESC
      ADDRESS__TYPE, EXTERNAL :: OPENDIR
      INTEGER*4,     EXTERNAL :: ILEN, I_LEN
!
! --- Clean record DBI
!
      LEN_OPR = LOC(DBI%LAST_FIELD) - LOC(DBI%FIRST_FIELD) + 4
      CALL NOUT ( LEN_OPR, DBI )
!
! --- Save name of the configuration file
!
      DBI%CONFIG_FILE = CONFIG_FILE
!
! --- Read configuration file into the buffer BUF
!
      CALL ERR_PASS ( IUER, IER )
      CALL RD_TEXT  ( CONFIG_FILE, M_BUF, BUF, N_BUF, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 5721, IUER, 'DBI_CONFIG', 'Error in attempt '// &
     &         'to open DBI configuration file '//CONFIG_FILE )
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
              CALL ERR_LOG ( 5722, IUER, 'DBI_CONFIG', 'Error in '// &
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
         IF ( BUF(J1)(IND(1,2):IND(2,2)) .EQ. 'IVS_DB_URL:' ) THEN
              DBI%IVS_DB_URL = BUF(J1)(IND(1,3):IND(2,LIND))
            ELSE IF ( BUF(J1)(IND(1,2):IND(2,2)) .EQ. 'WGET_EXE:' ) THEN
              DBI%WGET_EXE = BUF(J1)(IND(1,3):IND(2,LIND))
            ELSE IF ( BUF(J1)(IND(1,2):IND(2,2)) .EQ. 'GZIP_EXE:' ) THEN
              DBI%GZIP_EXE = BUF(J1)(IND(1,3):IND(2,LIND))
            ELSE IF ( BUF(J1)(IND(1,2):IND(2,2)) .EQ. 'TMP_DIR:' ) THEN
              DBI%TMP_DIR = BUF(J1)(IND(1,3):IND(2,LIND))
              IL = I_LEN(DBI%TMP_DIR)
              IF ( DBI%TMP_DIR(IL:IL) .NE. '/' ) THEN
                   DBI%TMP_DIR(IL+1:) = '/'
              END IF
            ELSE IF ( BUF(J1)(IND(1,2):IND(2,2)) .EQ. 'GET_FILE:' ) THEN
              DBI%GET_FILE= BUF(J1)(IND(1,3):IND(2,LIND))
            ELSE IF ( BUF(J1)(IND(1,2):IND(2,2)) .EQ. 'NOGET_FILE:' ) THEN
              DBI%NOGET_FILE= BUF(J1)(IND(1,3):IND(2,LIND))
            ELSE IF ( BUF(J1)(IND(1,2):IND(2,2)) .EQ. 'INCOMING_DIR:' ) THEN
              DBI%INCOMING_DIR = BUF(J1)(IND(1,3):IND(2,LIND))
              IL = I_LEN(DBI%INCOMING_DIR)
              IF ( DBI%INCOMING_DIR(IL:IL) .NE. '/' ) THEN
                   DBI%INCOMING_DIR(IL+1:) = '/'
              END IF
            ELSE IF ( BUF(J1)(IND(1,2):IND(2,2)) .EQ. 'LOG_FILE:' ) THEN
              DBI%LOG_FILE= BUF(J1)(IND(1,3):IND(2,LIND))
            ELSE IF ( BUF(J1)(IND(1,2):IND(2,2)) .EQ. 'DATE_START:' ) THEN
              DBI%DATE_START = BUF(J1)(IND(1,3):IND(2,LIND))
            ELSE IF ( BUF(J1)(IND(1,2):IND(2,2)) .EQ. 'DATE_END:' ) THEN
              DBI%DATE_END = BUF(J1)(IND(1,3):IND(2,LIND))
            ELSE IF ( BUF(J1)(IND(1,2):IND(2,2)) .EQ. 'MAIL_COMMAND:' ) THEN
              DBI%MAIL_COMMAND = BUF(J1)(IND(1,3):IND(2,LIND))
            ELSE IF ( BUF(J1)(IND(1,2):IND(2,2)) .EQ. 'EMAIL_IMPORT:' ) THEN
              DBI%EMAIL_IMPORT = BUF(J1)(IND(1,3):IND(2,LIND))
            ELSE
              CALL ERR_LOG ( 5723, IUER, 'DBI_CONFIG', 'Error in '// &
     &            'parsing the '//STR(1:I_LEN(STR))//'-th line in the '// &
     &            'configuration file '//CONFIG_FILE(1:I_LEN(CONFIG_FILE))// &
     &            ': unsupported keyword '//BUF(J1)(IND(1,2):IND(2,2)) )
              RETURN
         END IF
         I_PAR = I_PAR + 1
 410  CONTINUE
!
! --- Check: did we define all fields of DBI?
!
      IF ( I_PAR .LT. M_PAR ) THEN
           CALL CLRCH ( STR  )
           CALL INCH  ( I_PAR, STR  )
           CALL CLRCH ( STR1 )
           CALL INCH  ( M_PAR, STR1 )
           CALL ERR_LOG ( 5724, IUER, 'DBI_CONFIG', 'Not all parameters '// &
     &         'were found in DBI configuration file '// &
     &          CONFIG_FILE(1:I_LEN(CONFIG_FILE))//' --  '//STR(1:I_LEN(STR))// &
     &         ' instead of '//STR1 )
           RETURN
      END IF
!
! --- Save configuration file
!
      DBI%CONFIG_FILE = CONFIG_FILE
!
! --- Remove enclosing " or '
!
      IF ( DBI%WGET_EXE(1:1) .EQ. '"' .OR. &
     &     DBI%WGET_EXE(1:1) .EQ. "'"      ) DBI%WGET_EXE = DBI%WGET_EXE(2:) 
      IP = I_LEN(DBI%WGET_EXE)
      IF ( DBI%WGET_EXE(IP:IP) .EQ. '"' .OR. &
     &     DBI%WGET_EXE(IP:IP) .EQ. "'"      ) DBI%WGET_EXE = DBI%WGET_EXE(1:IP-1) 
!
! --- Check: whether wget exists?
!
      IP = INDEX ( DBI%WGET_EXE, ' ' ) - 1
      IF ( IP .LE. 0 ) IP = I_LEN(DBI%WGET_EXE)
      INQUIRE ( FILE=DBI%WGET_EXE(1:IP), EXIST=LEX )
      IF ( .NOT. LEX ) THEN
           CALL ERR_LOG ( 5725, IUER, 'DBI_CONFIG', 'Exectuable WGET_EXE '// &
     &          DBI%WGET_EXE(1:IP)//' was not found' )
           RETURN
      END IF
!
! --- Check: whether gzip exists?
!
      INQUIRE ( FILE=DBI%GZIP_EXE, EXIST=LEX )
      IF ( .NOT. LEX ) THEN
           CALL ERR_LOG ( 5726, IUER, 'DBI_CONFIG', 'Exectuable GZIP_EXE '// &
     &          DBI%GZIP_EXE(1:I_LEN(DBI%GZIP_EXE))//' was not found' )
           RETURN
      END IF
!
! --- Check: whether tmp_dir exists?
!
      DIR_DESC = OPENDIR ( DBI%TMP_DIR(1:I_LEN(DBI%TMP_DIR))//CHAR(0) )
      IF ( DIR_DESC == 0 ) THEN
           CALL ERR_LOG ( 5727, IUER, 'DBI_CONFIG', 'Directory TMP_DIR '// &
     &          DBI%TMP_DIR(1:I_LEN(DBI%TMP_DIR))//' was not found' )
           RETURN
         ELSE 
           CALL CLOSEDIR ( %VAL(DIR_DESC) )
      END IF
!
! --- Check: whether file get_list exists?
!
      INQUIRE ( FILE=DBI%GET_FILE, EXIST=LEX )
      IF ( .NOT. LEX ) THEN
           CALL ERR_LOG ( 5728, IUER, 'DBI_CONFIG', 'File GET_FILE '// &
     &          DBI%GET_FILE(1:I_LEN(DBI%GET_FILE))//' was not found' )
           RETURN
      END IF
!
! --- Check: whether file noget_list exists?
!
      INQUIRE ( FILE=DBI%NOGET_FILE, EXIST=LEX )
      IF ( .NOT. LEX ) THEN
           CALL ERR_LOG ( 5729, IUER, 'DBI_CONFIG', 'File NOGET_FILE '// &
     &          DBI%NOGET_FILE(1:I_LEN(DBI%NOGET_FILE))//' was not found' )
           RETURN
      END IF
!
! --- Check: whether directory INCOMING_DIR exists?
!
      DIR_DESC = OPENDIR ( DBI%INCOMING_DIR(1:I_LEN(DBI%INCOMING_DIR))//CHAR(0) )
      IF ( DIR_DESC == 0 ) THEN
           CALL ERR_LOG ( 5730, IUER, 'DBI_CONFIG', 'Directory INCOMING_DIR '// &
     &          DBI%INCOMING_DIR(1:I_LEN(DBI%INCOMING_DIR))//' was not found' )
           RETURN
         ELSE 
           CALL CLOSEDIR ( %VAL(DIR_DESC) )
      END IF
!
! --- Start date transformation to MJD, SEC format
!
      CALL ERR_PASS     ( IUER, IER )
      CALL DATE_TO_TIME ( DBI%DATE_START//'_00:00:00', DBI%MJD_START, SEC, IER)
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 5731, IUER, 'DBI_CONFIG', 'Wrong date format '// &
     &         'DATE_START: '//DBI%DATE_START )
           RETURN
      END IF
!
! --- End date transformation to MJD, SEC format
!
      CALL ERR_PASS     ( IUER, IER )
      CALL DATE_TO_TIME ( DBI%DATE_END//'_00:00:00', DBI%MJD_END, SEC, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 5732, IUER, 'DBI_CONFIG', 'Wrong date format '// &
     &         'DATE_END: '//DBI%DATE_END )
           RETURN
      END IF
!
      DATE_NOW = GET_CDATE ()
      CALL DATE_TO_TIME ( DATE_NOW, MJD_NOW, SEC, 0 )
      IF ( DBI%MJD_END  .GT. MJD_NOW + 1 ) THEN
!
! -------- If the date DATE_END is in the future we set it tomorrow in order
! -------- to avoid attempt to read directories whcih don't exist
!
           DBI%MJD_END = MJD_NOW + 1
           CALL ERR_PASS ( IUER, IER )
           DATE_TOMORROW = JD_TO_DATE ( 2400000.500001D0 + DBI%MJD_END, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 5733, IUER, 'DBI_CONFIG', 'Trap of internal '// &
     &              'control: date transformation error' )
                RETURN
           END IF
           DBI%DATE_END = DATE_TOMORROW
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  DBI_CONFIG  #!#
