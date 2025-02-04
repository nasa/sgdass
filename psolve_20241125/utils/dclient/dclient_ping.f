      SUBROUTINE DCLIENT_PING ( DCLIENT, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  DCLIENT_PING  checkcs is there a connection to the IVS    *
! *   Data Center. It first, uses system command "ping" in order to      *
! *   check is there a physical connection. If there is a physical       *
! *   connection then it sends a PING-letter to the Data Center in order *
! *   to check whether dserver is still running. If dserver is running,  *
! *   then a used should receive an e-mail from Data Center confirming   *
! *   such a wonderful thing. Normally, dserver replies within           *
! *   10 seconds. Therefore, the reply should reach user within 1-3      *
! *   minutes. If, say, 10 minutes elapsed and no reply arrived it       *
! *   means that dserver is down :-( . A user should notify data         *
! *   administrator at the IVS Data Center about the problem.            *
! *                                                                      *
! * ________________________ Input parameters: _________________________ *
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
! *  ### 16-JAN-2001  DCLIENT_PING  v1.0 (c) L. Petrov  16-JAN-2001 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'dclient.i'
      TYPE ( DCLIENT__STRU ) ::  DCLIENT
      INTEGER*4  IUER
      CHARACTER  COMSTR*256, PID_STR*5, MESSAGE_FILE*256, STR*32
      CHARACTER  GET_CDATE*19
      INTEGER*4  LUN, IS, IO, IP, PID, ISIG, ICOD
      INTEGER*4  SYSTEM, GETPID, GET_UNIT, I_LEN
!
! --- Prepare the line for checking physical connection to dserver
!
      CALL CLRCH ( COMSTR )
      IP = INDEX ( DCLIENT%DSERVER_EMAIL, '@' ) + 1
      IF ( IP .EQ. 1 ) THEN
           CALL ERR_LOG ( 5181, IUER, 'DCLIENT_PING', 'Wrong e-mail address '// &
     &         'of dserver: '// &
     &          DCLIENT%DSERVER_EMAIL(1:I_LEN(DCLIENT%DSERVER_EMAIL))// &
     &         ' -- no @-character was found' )
           RETURN
      END IF
!
      COMSTR = 'ping - '// &
     &          DCLIENT%DSERVER_EMAIL(IP:I_LEN(DCLIENT%DSERVER_EMAIL))// &
     &         ' -n 1'
      WRITE ( 6, '(A)' ) 'Checking physical connection to '// &
     &                  DCLIENT%DSERVER_EMAIL(IP:I_LEN(DCLIENT%DSERVER_EMAIL))// &
     &                   ' ...'
!
! --- Execute the line
!
      IS = SYSTEM ( COMSTR(1:I_LEN(COMSTR)) )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 5182, IUER, 'DCLIENT_PING', 'No connection to '// &
     &         'dserver now. Try later, ple-e-e-ease' )
           RETURN
      END IF
      WRITE ( 6, '(A)' ) 'Physical connection is OK.'
!
! --- Get PID (Process IDentifier)
!
      PID = GETPID()
!
! --- Transform PID to a text string
!
      CALL CLRCH  ( PID_STR )
      CALL INCH   ( PID,   PID_STR )
      CALL CHASHR (        PID_STR )
      CALL BLANK_TO_ZERO ( PID_STR )
!
! --- Build a message file names. It contain PID in order to avoid
! --- conflicts with simultaneous work of two or more users
!
      MESSAGE_FILE = DCLIENT%TMP_DIR(1:I_LEN(DCLIENT%TMP_DIR))//'dclient_'// &
     &               PID_STR(1:I_LEN(PID_STR))//'.mes'
!
! --- Prepare the file: the body of the message which is to be sent to dserver
!
      LUN = GET_UNIT ()
      OPEN ( UNIT=LUN, FILE=MESSAGE_FILE, STATUS='UNKNOWN', IOSTAT=IO )
      IF ( IO .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL INCH ( IO, STR )
           CALL ERR_LOG ( 5183, IUER, 'DCLIENT_PING', 'Error in attempt '// &
     &         'to open temporary file '//MESSAGE_FILE(1:I_LEN(MESSAGE_FILE))// &
     &         ' IOSTAT='//STR )
           RETURN
      END IF
!
      WRITE ( LUN, '(A)', IOSTAT=IO ) 'DATA_TYPE: PING'
      IF ( IO .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL INCH ( IO, STR )
           CALL ERR_LOG ( 5184, IUER, 'DCLIENT_PING', 'Error in attempt '// &
     &         'to write in temporary file '// &
     &          MESSAGE_FILE(1:I_LEN(MESSAGE_FILE))//' IOSTAT='//STR )
           RETURN
      END IF
      CLOSE ( UNIT=LUN )
      WRITE ( 6, '(A)' ) 'Checking whether dserver is running ...'
!
      CALL CLRCH ( COMSTR )
      COMSTR = DCLIENT%MAIL_COMMAND(1:I_LEN(DCLIENT%MAIL_COMMAND))// &
     &         ' -s "Ping sent at '//GET_CDATE()//' by '//DCLIENT__LABEL//'" '// &
     &         DCLIENT%DSERVER_EMAIL(1:I_LEN(DCLIENT%DSERVER_EMAIL))// &
     &         ' < '//MESSAGE_FILE(1:I_LEN(MESSAGE_FILE))
      IS = SYSTEM ( COMSTR(1:I_LEN(COMSTR)) )
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
           WRITE ( 6, * ) 'DCLIENT_PING: Signal: ',ISIG, &
     &                    '  completion code: ',ICOD
           CALL ERR_LOG ( 5185, IUER, 'DCLIENT_PING', 'Error in attempt '// &
     &         'to execute a command: '//COMSTR )
           RETURN
      END IF
!
! --- Remove temporary file
!
      CALL UNLINK ( MESSAGE_FILE(1:I_LEN(MESSAGE_FILE))//CHAR(0) )
!
! --- Prapare the final messages
!
      WRITE ( 6, '(A)' ) 'Ping to the IVS Data Center '// &
     &                    DCLIENT%DATA_CENTER(1:I_LEN(DCLIENT%DATA_CENTER))// &
     &                   ' is sent at '//GET_CDATE()
      WRITE ( 6, '(A)' ) '     If dserver is running at Data Center you '// &
     &                   'should get an e-mail '
      WRITE ( 6, '(A)' ) '     from there within 1-3 minutes.'
      WRITE ( 6, '(A)' ) 'Please check.'
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  DCLIENT_PING  #!#
