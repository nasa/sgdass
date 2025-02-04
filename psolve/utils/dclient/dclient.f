      PROGRAM    DCLIENT_MAIN
! ************************************************************************
! *                                                                      *
! *   Program  DCLIENT_MAIN  is a program which communicate with         *
! *   program dserver which is assumed to run on one of the IVS Data     *
! *   Centers and it transfers data from from the current host to        *
! *   a host of the IVS Data Center.                                     *
! *                                                                      *
! *   Usage: dclient FLAGS -t <data_type> -c <config_file>               *
! *          -f [<database_name> or <eops_name> or <eopi_name>]          *
! *                                                                      *
! *   FLAGS:                                                             *
! *                                                                      *
! *   -q  -- quiet mode. Request for confirmation of the operation is    *
! *          not issued.                                                 *
! *   -qq -- the quietest mode. No request for confirmation of the       *
! *          operation no information about the progress is printed on   *
! *          the screen.                                                 *
! *   -p  -- to send a ping to the Data Center in order to check whether *
! *          it is still running. Arguments data type and database names *
! *          are ignored and can be omitted if this flag is on.          *
! *   -h  -- help information is printed in stdout.                      *
! *                                                                      *
! *   <data_type>   -- The following data types are supported:           *
! *                 DBH   -- database(s) transfer.                       *
! *                 EOPS  -- transfer of EOPS file.                      *
! *                 EOPI  -- transfer of EOPI file.                      *
! *                 DSNX  -- transfer of a listing of a standalone       *
! *                          solution in Sinex format.                   *
! *                 ISNX  -- transfer of a listing of a standalone       *
! *                          solution in Sinex format.                   *
! *   NB: data types "DSNX" and "ISNX" are identical. There is no reason *
! *       to distinguiesh them. The was a caprice of several tech        *
! *       unsavy persons to request it. The contents of these files is   *
! *       the same. The only differen is that the line "DATA_TYPE" in    *
! *       the message to dserver is different.                           *
! *                                                                      *
! *   <config_file> -- file which contains configuration of dclient.     *
! *   <database_name> -- one of the three modes:                         *
! *                      a) If the first symbol is "/" then this         *
! *                         is interpreted as a database file name       *
! *                         with absolute path.                          *
! *                      b) If the argument contains a pattern "_V" then *
! *                         it is interpreted as a database filename     *
! *                         with specific version. File path will be     *
! *                         find in geo-VLBI catalogue system.           *
! *                      c) If the argument contains does not contain    *
! *                         a pattern "_V" then and starts from the      *
! *                         symbol other than "/" than it is interpreted *
! *                         a request for transferring a pair of the     *
! *                         last versions of -X and -S databases. Two    *
! *                         databases will be transferred.               *
! *                                                                      *
! *   <eops_name> -- Earth Orientation file from 24 hours VLBI           *
! *                  experiments.                                        *
! *                                                                      *
! *   <eopi_name> -- Earth Orientation file from NEOS Intensive VLBI     *
! *                  experiments.                                        *
! *                                                                      *
! *   dclient reads configure file, checks its syntax. Further work      *
! *   depends on a data type.                                            *
! *                                                                      *
! *   If the data_type is DBH then dclient                               *
! *      checks a database_name argument and determine its mode. In the  *
! *      case of modes "b" and "c" dclient asks catalogue and finds      *
! *      filename(s) of the database(s) with absolute path(s). Then it   *
! *      creates a temporary file with e-mail message to dserver. Then   *
! *      it creates a temporary file with C-Shell program which copies   *
! *      the database file(s) to a local ftp-directory and gzips them    *
! *      on the fly.                                                     *
! *                                                                      *
! *   If the data type is EOPS or EOPI then dclient                      *
! *      checks EOP filename. Then it creates a temporary file with      *
! *      e-mail message to dserver. After that it creates a temporary    *
! *      file with C-Shell program which copies the EOP file to the      *
! *      local ftp-directory under specific name and gzips it on the fly.*
! *                                                                      *
! *   If the data type is of DSNX or ISNX type if checks whether the     *
! *      file exists. Then it creates a temporary file with              *
! *      e-mail message to dserver. After that it creates a temporary    *
! *      file with C-Shell program which copies the EOP file to the      *
! *      local ftp-directory under specific name and gzips it on the fly.*
! *                                                                      *
! *   Finally, a e-mail message is sent to dserver which should initiate *
! *   retrieving the databases from the host where dclient has been      *
! *   launched (Operating Center) to the host where dserver is running   *
! *   (Data Center). E-mail messages from dserver with confirmation of   *
! *   a request (or denying) and successful termination of retrieving    *
! *   (or error) will be sent to user(s) in according to specifications  *
! *   in config_file.                                                    *
! *                                                                      *
! * ### 30-SEP-99    DCLIENT_MAIN  v2.6 (c)  L. Petrov  21-DEC-2007 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'dclient.i'
      TYPE ( DCLIENT__STRU ) ::  DCLIENT
      INTEGER*4  M_PAIR, M_ARG
      PARAMETER  ( M_PAIR = 5, M_ARG = 7 )
      INTEGER*4  NUMARG, N_ARG, J1, J2, IC, IUER
      CHARACTER  ARG_ARR(M_ARG)*256, SOLVE_HELP_DIR*256, HELP_FILE*256, &
     &           CONFIG_FILE*256, FILIN*256, STR*80, DATA_TYPE*8
      CHARACTER  MESSAGE_FILE*256, COMMAND_FILE*256, FINAM1*256, FINAM2*256
      LOGICAL*4  FL_CONF, FL_SLNT, FL_PING, LEX
      INTEGER*4, EXTERNAL :: IARGC, ILEN, I_LEN
!
      CALL CLRCH ( CONFIG_FILE )
      CALL CLRCH ( FILIN       )
      CALL CLRCH ( DATA_TYPE   )
      FL_PING = .FALSE.
      FL_CONF = .TRUE.
      FL_SLNT = .FALSE.
!
! --- Parse arguments
!
      NUMARG = IARGC ()
      IF ( NUMARG .GE. 1 ) THEN
           N_ARG = MIN(M_ARG,NUMARG)
!
! -------- Get arguments from the command line
!
           N_ARG = MIN(M_ARG,NUMARG)
!
! -------- Copy arguments to the array ARG_ARR for further processing
!
           DO 410 J1=1,N_ARG
              CALL CLRCH  (     ARG_ARR(J1) )
              CALL GETARG ( J1, ARG_ARR(J1) )
!
              IF ( ARG_ARR(J1)(1:2) .EQ. '-h' ) THEN
!
! ---------------- Get help file name
!
                   CALL CLRCH ( SOLVE_HELP_DIR )
                   CALL CLRCH ( HELP_FILE )
                   CALL GETENVAR ( 'PSOLVE_HELP_DIR',  SOLVE_HELP_DIR )
                   IF ( ILEN(SOLVE_HELP_DIR) .EQ. 0 ) THEN
                        CALL ERR_LOG ( 5101, -1, 'DCLIENT_MAIN', &
     &                      'Environment variable SOLVE_HELP_DIR is not set up. '// &
     &                      'Ask system administrator' )
                        WRITE ( 6, * ) 'DCLIENT_MAIN: Error in customization'
                        CALL EXIT ( 1 )
                   END IF
                   IF ( SOLVE_HELP_DIR(ILEN(SOLVE_HELP_DIR):ILEN(SOLVE_HELP_DIR)) .NE. '/' ) THEN
                        SOLVE_HELP_DIR = SOLVE_HELP_DIR(1:ILEN(SOLVE_HELP_DIR))//'/'
                   END IF
                   HELP_FILE = SOLVE_HELP_DIR(1:ILEN(SOLVE_HELP_DIR))//DCLIENT__HELP
!
! ---------------- Check: whether the help file really exists
!
                   INQUIRE ( FILE=HELP_FILE, EXIST = LEX )
                   IF ( .NOT. LEX ) THEN
                        CALL ERR_LOG ( 5102, -1, 'DCLIENT_MAIN', &
     &                      'Help file '//HELP_FILE(1:I_LEN(HELP_FILE))// &
     &                      ' has not been found' )
                        WRITE ( 6, * ) 'DCLIENT_MAIN: Error in customization'
                        CALL EXIT ( 1 )
                   END IF
!
! ---------------- Show content of help file
!
                   IUER = -1
                   CALL SHOW_TEXT_FILE_COL ( HELP_FILE, &
     &                                       DCLIENT__LABEL, 0, IUER )
                   IF ( IUER .NE. 0 ) THEN
                        CALL ERR_LOG ( 5103, -1, 'DCLIENT_MAIN', 'Error '// &
     &                      'in attempt to display help file' )
                        WRITE ( 6, * ) 'DCLIENT_MAIN: Error in customization'
                        CALL EXIT ( 1 )
                   END IF
!
! ---------------- .. and then exit
!
                   CALL EXIT ( 0 )
              END IF
 410       CONTINUE
        ELSE
!
! -------- No arguments were supplied
!
           WRITE ( 6, 110 )
 110       FORMAT ( 'Usage: dclient FLAGS -t <data_type> -c <congif_file> ', &
     &              '-f <file_name> '/ &
     &              'Run dclient -h for help' )
           CALL EXIT ( 1 )
      END IF
!
! --- Parse arguments from the command line
!
      IC=1
      DO 420 J2=1,M_PAIR
         IF ( IC .GT. N_ARG ) GOTO 820
         IF ( ARG_ARR(IC)(1:2) .EQ. '-c' ) THEN
!
! ----------- Configuration file is specified
!
              IF ( N_ARG .LT. IC+1 ) THEN
                   CALL ERR_LOG ( 5104, -1, 'DCLIENT_MAIN', 'Too few '// &
     &                           'arguments')
                   WRITE ( 6, * ) 'DCLIENT_MAIN: Error in command line'
                   CALL EXIT ( 2 )
                ELSE
                   CONFIG_FILE = ARG_ARR(IC+1)
                   IC = IC+2
              END IF
           ELSE IF ( ARG_ARR(IC)(1:2) .EQ. '-f' ) THEN
!
! ----------- Data file for the database(s) is specified
!
              IF ( N_ARG .LT. IC+1 ) THEN
                   CALL ERR_LOG ( 5106, -1, 'DCLIENT_MAIN', 'Too few '// &
     &                           'arguments' )
                   WRITE ( 6, * ) 'DCLIENT_MAIN: Error in command line'
                   CALL EXIT ( 2 )
                ELSE
                   FILIN = ARG_ARR(IC+1)
                   IC = IC+2
              END IF
           ELSE IF ( ARG_ARR(IC)(1:3) .EQ. '-qq' ) THEN
              FL_SLNT = .TRUE.
              FL_CONF = .FALSE.
              IC = IC+1
           ELSE IF ( ARG_ARR(IC)(1:2) .EQ. '-q'  ) THEN
!
! ----------- Quite mode was requested
!
              FL_CONF = .FALSE.
              IC = IC+1
           ELSE IF ( ARG_ARR(IC)(1:2) .EQ. '-p'  ) THEN
!
! ----------- Ping mode is requested
!
              FL_PING = .TRUE.
              IC = IC+1
           ELSE IF ( ARG_ARR(IC)(1:2) .EQ. '-t' ) THEN
!
! ----------- Type file is specified
!
              IF ( N_ARG .LT. IC+1 ) THEN
                   CALL ERR_LOG ( 5105, -1, 'DCLIENT_MAIN', 'Too few '// &
     &                           'arguments' )
                   WRITE ( 6, * ) 'DCLIENT_MAIN: Error in command line'
                   CALL EXIT ( 2 )
                ELSE
                   DATA_TYPE = ARG_ARR(IC+1)
                   IC = IC+2
              END IF
           ELSE
!
! ----------- Unrecognized argument
!
              CALL CLRCH ( STR )
              CALL INCH  ( IC, STR )
              CALL ERR_LOG ( 5107, -1, 'DCLIENT_MAIN', 'Unrecognized '// &
     &             STR(1:I_LEN(STR))//'-th argument: '// &
     &             ARG_ARR(IC)(1:I_LEN(ARG_ARR(IC)))// &
     &            '. One of -c, -f, was expected' )
              WRITE ( 6, * ) 'DCLIENT_MAIN: Error in command line'
              CALL EXIT ( 2 )
         END IF
 420  CONTINUE
 820  CONTINUE
!
      IF ( ILEN(CONFIG_FILE) .EQ. 0 ) THEN
           CALL ERR_LOG ( 5108, -1, 'DCLIENT_MAIN', 'Argument '// &
     &         'configuration_file_name has not been supplied' )
           WRITE ( 6, * ) 'DCLIENT_MAIN: Error in command line'
           CALL EXIT ( 3 )
      END IF
!
      IF ( .NOT. FL_PING  .AND.  ILEN(DATA_TYPE) .EQ. 0 ) THEN
           CALL ERR_LOG ( 5109, -1, 'DCLIENT_MAIN', 'Argument '// &
     &         'data_type has not been supplied' )
           WRITE ( 6, * ) 'DCLIENT_MAIN: Error in command line'
           CALL EXIT ( 3 )
      END IF
!
      IF ( .NOT. FL_PING  .AND.  ILEN(FILIN) .EQ. 0 ) THEN
           CALL ERR_LOG ( 5110, -1, 'DCLIENT_MAIN', 'Argument '// &
     &         'file has not been supplied' )
           WRITE ( 6, * ) 'DCLIENT_MAIN: Error in command line'
           CALL EXIT ( 3 )
      END IF
!
! --- Parse configuration file
!
      IUER = -1
      CALL DCLIENT_CONFIG ( DATA_TYPE, CONFIG_FILE, DCLIENT, IUER )
      IF ( IUER .NE. 0 ) THEN
           WRITE ( 6, * ) 'DCLIENT_MAIN: error in configuration.'
           CALL EXIT ( 4 )
      END IF
!
      IF ( FL_PING ) THEN
!
! -------- Check connection, physical and logical, to dserver
!
           IUER = -1
           CALL DCLIENT_PING ( DCLIENT, IUER )
           IF ( IUER .EQ. 0 ) THEN
                CALL EXIT ( 0 )
              ELSE
                WRITE ( 6, * ) 'DCLIENT_MAIN: error in attempt whether '// &
     &                'program dserver is running at '// &
     &                 DCLIENT%DATA_CENTER(1:I_LEN(DCLIENT%DATA_CENTER))
                CALL EXIT ( 15 )
           END IF
      END IF
!
      IF ( DATA_TYPE(1:3) .EQ. 'DBH' ) THEN
           CALL TRAN ( 11, FILIN, FILIN )
!
! -------- Parse a request and form a filenames for message and a command file
!
           IUER = -1
           CALL DBH_REQUEST ( DCLIENT, FILIN, MESSAGE_FILE, COMMAND_FILE, &
     &                        FINAM1, FINAM2, FL_SLNT, IUER )
           IF ( IUER .NE. 0 ) THEN
                WRITE ( 6, * ) 'DCLIENT_MAIN: error in parsing request'
                CALL EXIT ( 5 )
           END IF
!
! -------- Make a C-shell command file for processing request
!
           IUER = -1
           CALL DBH_COMMAND_FILE ( DCLIENT, FL_CONF, FILIN, FINAM1, FINAM2, &
     &                             COMMAND_FILE, MESSAGE_FILE, IUER )
           IF ( IUER .NE. 0 ) THEN
                WRITE ( 6, * ) 'DCLIENT_MAIN: error in making command file'
                CALL EXIT ( 6 )
           END IF
         ELSE IF ( DATA_TYPE(1:4) .EQ. 'EOPS'  ) THEN
!
! -------- Make a C-shell command file for processing request
!
           IUER = -1
           CALL EOP_COMMAND_FILE ( DCLIENT, 'EOPS', FL_CONF, FILIN, &
     &                             COMMAND_FILE, MESSAGE_FILE, IUER )
           IF ( IUER .NE. 0 ) THEN
                WRITE ( 6, * ) 'DCLIENT_MAIN: error in making command file'
                CALL EXIT ( 7 )
           END IF
         ELSE IF ( DATA_TYPE(1:4) .EQ. 'EOPI'  ) THEN
!
! -------- Make a C-shell command file for processing request
!
           IUER = -1
           CALL EOP_COMMAND_FILE ( DCLIENT, 'EOPI', FL_CONF, FILIN, &
     &                             COMMAND_FILE, MESSAGE_FILE, IUER )
           IF ( IUER .NE. 0 ) THEN
                WRITE ( 6, * ) 'DCLIENT_MAIN: error in making command file'
                CALL EXIT ( 8 )
           END IF
         ELSE IF ( DATA_TYPE(1:4) .EQ. 'DSNX' .OR. &
     &             DATA_TYPE(1:4) .EQ. 'ISNX'      ) THEN
           IUER = -1
           CALL DSNX_COMMAND_FILE ( DCLIENT, FL_CONF, FILIN, COMMAND_FILE, &
     &                              MESSAGE_FILE, DATA_TYPE, IUER )
           IF ( IUER .NE. 0 ) THEN
                WRITE ( 6, * ) 'DCLIENT_MAIN: error in making command file'
                CALL EXIT ( 9 )
           END IF
         ELSE
           CALL ERR_LOG ( 5111, -1, 'DCLIENT_MAIN', 'Data type '//DATA_TYPE// &
     &         ' is not supported' )
           WRITE ( 6, * ) 'DCLIENT_MAIN: Error in command line'
           CALL EXIT ( 10 )
      END IF
!
      IF ( ILEN(COMMAND_FILE) .GT. 0 ) THEN
!
! -------- Execute a C-Shell command file which moves file to a local ftp area
! -------- and send a e-mail message to dserver with a request to retrieve them.
!
           IUER = -1
           CALL EXECUTE_COMMAND ( DCLIENT, DATA_TYPE, FILIN, COMMAND_FILE, &
     &                            MESSAGE_FILE, FL_SLNT, IUER )
           IF ( IUER .NE. 0 ) THEN
                WRITE ( 6, * ) 'DCLIENT_MAIN: error in executing command file'
                CALL EXIT ( 11 )
           END IF
           CALL EXIT ( 0 )
      END IF
      END   !#!  DCLIENT_MAIN  #!#
