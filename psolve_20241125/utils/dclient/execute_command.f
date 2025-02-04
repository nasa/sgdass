      SUBROUTINE EXECUTE_COMMAND ( DCLIENT, DATA_TYPE, FILIN, COMMAND_FILE, &
     &                             MESSAGE_FILE, FL_SLNT, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  EXECUTE_COMMAND  is a part of dclient software.           *
! *   It executes a C-Shell programm kept in the file COMMAND_FILE and   *
! *   it parses completion codes. In the case of successful completion   *
! *   of COMMAND_FILE it adds a line to a submission log file. Then and  *
! *   purges files COMMAND_FILE, MESSAGE_FILE if a parameters DO_PURGE   *
! *   in record DCLIENT is set as .TRUE.                                 *
! *                                                                      *
! * ________________________ Input parameters: _________________________ *
! *                                                                      *
! *      DCLIENT ( RECORD    ) -- Object with data structure for keeping *
! *                               configuration of dclient.              *
! *    DATA_TYPE ( CHARACTER ) -- Type of the data to be submitted.      *
! *        FILIN ( CHARACTER ) -- Input filename argument. May be one of *
! *                               a) database filename with a full path; *
! *                               b) database filename of the specific   *
! *                                  version without path.               *
! *                               c) database name without version       *
! *                                  number and path. It corresponds to  *
! *                                  a X/S pair of databases.            *
! *                               d) database name without version       *
! *                               e) full EOPS file name (with path).    *
! * COMMAND_FILE ( CHARACTER ) -- File name with a C-Shell program for   *
! *                               processing a request for a database    *
! *                               submission.                            *
! * MESSAGE_FILE ( CHARACTER ) -- File name with a message to be sent to *
! *                               dserver.                               *
! *      FL_SLNT ( LOGICAL*4 ) -- Silent mode flag. If .TRUE. then no    *
! *                               information messages are printed on    *
! *                               the screen.                            *
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
! *  ###  01-OCT-99  EXECUTE_COMMAND v1.1 (c) L. Petrov 21-SEP-2000 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'dclient.i'
      TYPE ( DCLIENT__STRU ) ::  DCLIENT
      CHARACTER  DATA_TYPE*(*), FILIN*(*), COMMAND_FILE*(*), MESSAGE_FILE*(*)
      LOGICAL*4  FL_SLNT
      INTEGER*4  IUER
      INTEGER*4  IS, ISIG, ICOD, I11
      CHARACTER  USER_NAME*256, USER_REALNAME*256, USER_E_ADDRESS*256, &
     &           ERROR_FILE*256
      CHARACTER  GET_CDATE*19
      INTEGER*4  I_LEN, SYSTEM
!
      IF ( FL_SLNT ) THEN
           CALL CLRCH ( ERROR_FILE )
           ERROR_FILE = COMMAND_FILE(1:I_LEN(COMMAND_FILE))//'.errs'
           CALL UNLINK ( ERROR_FILE(1:I_LEN(ERROR_FILE))//CHAR(0) )
        ELSE IF ( .NOT. FL_SLNT ) THEN
           IF ( DATA_TYPE(1:3) .EQ. 'DBH' ) THEN
                WRITE ( 6, '(A)' ) '=== Request for submitting the database '// &
     &                              FILIN(1:I_LEN(FILIN))//' is being executed'
              ELSE IF ( DATA_TYPE(1:4) .EQ. 'EOPS' ) THEN
                WRITE ( 6, '(A)' ) '=== Request for submitting EOPS series '// &
     &                              FILIN(1:I_LEN(FILIN))//' is being executed'
           END IF
      END IF
!
! --- Change access mode of the file in order to make command file executable
!
      IS = SYSTEM ( 'chmod +x '//COMMAND_FILE(1:I_LEN(COMMAND_FILE))//CHAR(0) )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 5171, IUER, 'EXECUTE_COMMAND', 'Error in making '// &
     &         'C-Shell command file '//COMMAND_FILE(1:I_LEN(COMMAND_FILE))// &
     &         ' executable' )
           RETURN
      END IF
!
! --- Execute a C-Shell program
!
      IF ( FL_SLNT ) THEN
           IS = SYSTEM ( COMMAND_FILE(1:I_LEN(COMMAND_FILE))//' > '// &
     &                   ERROR_FILE(1:I_LEN(ERROR_FILE))//CHAR(0) )
         ELSE
           IS = SYSTEM ( COMMAND_FILE(1:I_LEN(COMMAND_FILE))//CHAR(0) )
      END IF
      IF ( IS .NE. 0 ) THEN
!
! -------- Completion code is not 0. Extract ISIG -- signal number which
! -------- caused termination of the command and ICOD -- completion code
!
           ISIG = 0
           ICOD = 0
           CALL MVBITS ( IS, 0, 8, ISIG, 0 )
           CALL MVBITS ( IS, 8, 8, ICOD, 0 )
           IF ( ICOD .GE. 128 ) ICOD = ICOD-256
!
! -------- Try to diagnose the reason
!
           IF ( ICOD .EQ. -1 ) THEN
                CALL ERR_LOG ( 5172, IUER, 'EXECUTE_COMMAND', 'Error in '// &
     &              'excuting C-Shell command file '// &
     &               COMMAND_FILE(1:I_LEN(COMMAND_FILE))//' -- gzipping '// &
     &              'and moving result in '// &
     &               DCLIENT%FTP_DIR(1:I_LEN(DCLIENT%FTP_DIR))//' failed' )
                RETURN
           END IF
!
           IF ( ICOD .EQ. -2 ) THEN
                CALL ERR_LOG ( 5173, IUER, 'EXECUTE_COMMAND', 'Error in '// &
     &              'excuting C-Shell command file '// &
     &               COMMAND_FILE(1:I_LEN(COMMAND_FILE))//' -- gzipping '// &
     &              'and moving result in '// &
     &               DCLIENT%FTP_DIR(1:I_LEN(DCLIENT%FTP_DIR))//' failed' )
                RETURN
           END IF
!
           IF ( ICOD .EQ. -3 ) THEN
                CALL ERR_LOG ( 5174, IUER, 'EXECUTE_COMMAND', 'Error in '// &
     &              'excuting C-Shell command file '// &
     &               COMMAND_FILE(1:I_LEN(COMMAND_FILE))//' -- sending '// &
     &              'e-mail to '// &
     &               DCLIENT%DSERVER_EMAIL(1:I_LEN(DCLIENT%DSERVER_EMAIL))// &
     &              ' failed' )
                RETURN
           END IF
!
! -------- We failed to find a reason...
!
           WRITE ( 6, * ) ' is=',is,' isig=',isig,' icod=',icod ! %%%%%%
           CALL ERR_LOG ( 5175, IUER, 'EXECUTE_COMMAND', 'Error in excuting '// &
     &         'C-Shell command file '//COMMAND_FILE(1:I_LEN(COMMAND_FILE)) )
           IF ( FL_SLNT ) THEN
                IS = SYSTEM ( 'cat '//ERROR_FILE(1:I_LEN(ERROR_FILE))//CHAR(0) )
           END IF
           RETURN
      END IF
!
! --- Get information about user
!
      CALL GETINFO_USER ( USER_NAME, USER_REALNAME, USER_E_ADDRESS )
!
! --- Open a file with submission log.
!
      OPEN ( UNIT=11, FILE=DCLIENT%SUBMIT_LOG, STATUS='UNKNOWN', &
     &       ACCESS='APPEND', IOSTAT=I11 )
      IF ( I11 .NE. 0 ) THEN
           WRITE ( 6, * ) ' IOSTAT=',I11
           CALL ERR_LOG ( 5176, IUER, 'MAKE_COMMAND_FILE', 'Error in '// &
     &         'openning output file for a submision log-file '// &
     &          DCLIENT%SUBMIT_LOG )
           RETURN
      END IF
!
! --- Write a record
!
      WRITE ( UNIT=11, FMT='(A)' ) FILIN(1:I_LEN(FILIN))        //' | '// &
     &        USER_REALNAME(1:I_LEN(USER_REALNAME))             //' | '// &
     &        DCLIENT%DATA_CENTER(1:I_LEN(DCLIENT%DATA_CENTER)) //' | '// &
     &        GET_CDATE()                                       //' |'
      CLOSE ( UNIT=11 )
!
      IF ( DCLIENT%DO_PURGE(1:3) .EQ. 'YES' ) THEN
!
! -------- Purge temporal files with a message to dserver and a file with
! -------- C-Shell program
!
           IS = SYSTEM ( 'rm -f '//MESSAGE_FILE(1:I_LEN(MESSAGE_FILE))//CHAR(0))
           IS = SYSTEM ( 'rm -f '//COMMAND_FILE(1:I_LEN(COMMAND_FILE))//CHAR(0))
      END IF
      IF ( FL_SLNT ) THEN
           CALL UNLINK ( ERROR_FILE(1:I_LEN(ERROR_FILE))//CHAR(0) )
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  EXECUTE_COMMAND  #!#
