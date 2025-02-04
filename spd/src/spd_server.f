      PROGRAM    SPD_SERVER
! ************************************************************************
! *                                                                      *
! *   Program  SPD_SERVER
! *                                                                      *
! *  ### 06-JAN-2015   SPD_SERVER  v1.0 (c)  L. Petrov  06-JAN-2015 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'spd.i'
      INCLUDE   'spd_local.i'
      INCLUDE   'spd_common.i'
      TYPE     ( SPD_3D__TYPE ) :: SPD
      TYPE     ( SPD_COM__TYPE ) :: COM
      CHARACTER  CONF_FILE*128, TASK*8, STR*128, MESSAGE*256, REQ_ID*6
      LOGICAL*1  LEX, FL_IP_ALLOWED
      REAL*8     SPD__ACCEPT_TIMEOUT, SPD__READ_TIMEOUT
      PARAMETER  ( SPD__ACCEPT_TIMEOUT = 86400.0D0 )
      PARAMETER  ( SPD__READ_TIMEOUT   =  4.0D0 )
      INTEGER*4  IK, IS, J1, J2, ARG_LEN, SIGTERM, SIGCLD, SIGALRM, &
     &           SIG_IGN, NP, SOCK_FD, REM_FD, SERVER_PID, LAST_REQ_NUM, &
     &           PID, IUER
      INTEGER*8  IADR
      INTEGER*4, EXTERNAL :: FORK, ILEN, I_LEN, KILL, SIGNAL, &
     &           SPD_SIGCLD_PROC, SPD_SIGTERM_PROC, SOCK_OPEN_SERVER, &
     &           SOCK_ACCEPT, SOCK_WRITE, SOCK_READ_POLL, &
     &           SPD_GET_LAST_REQ_NUM, SPD_UPDATE_PROC 
      INTEGER*8, EXTERNAL :: INET_NTOA
      CHARACTER, EXTERNAL :: GET_CDATE*19
!
      CALL GET_SYSTEM_CONSTANT ( 'SIGTERM', SIGTERM, ARG_LEN )
      CALL GET_SYSTEM_CONSTANT ( 'SIGALRM', SIGALRM, ARG_LEN )
      CALL GET_SYSTEM_CONSTANT ( 'SIGCLD',  SIGCLD,  ARG_LEN )
      CALL GET_SYSTEM_CONSTANT ( 'SIG_IGN', SIG_IGN, ARG_LEN )
!
      IF ( IARGC() < 1 ) THEN
           WRITE ( 6, * ) 'Usage: spd_server configuration_file'
           CALL EXIT ( 0 )
      END IF
!
      CALL GETARG ( 1, CONF_FILE )
!
! --- Read configuration file
!
      IUER = -1
      CALL SPD_DAEMON_CONFIG ( SPD, CONF_FILE, IUER )
      IF ( IUER .NE. 0 ) CALL EXIT ( 1 )
!
! --- Get the last request number
!
      IUER = - 1
      LAST_REQ_NUM = SPD_GET_LAST_REQ_NUM ( SPD, IUER  )
      IF ( IUER .NE. 0 ) CALL EXIT ( 1 )
!
! --- Check: is there any other server process?
!
      INQUIRE ( FILE=SPD%CONF%SERVER_PID_FILE, EXIST=LEX )
      IF ( LEX ) THEN
!
! -------- Read the file with PID
!
           IUER = 0
           CALL RD_TEXT ( SPD%CONF%SERVER_PID_FILE, 1, STR, NP, IUER )
           IF ( IUER .NE. 0 ) THEN
                WRITE ( 6, '(A,I5,A)' ) 'sdp_daemon: '// &
     &               ' failure to read PID file '// &
     &               SPD%CONF%SERVER_PID_FILE(1:I_LEN(SPD%CONF%SERVER_PID_FILE)) 
                CALL EXIT ( 1 )
           END IF
!
! -------- Send signal 0
!
           CALL CHIN ( STR(21:), SERVER_PID )
           IK = KILL ( %VAL(0), %VAL(SERVER_PID) )
           IF ( IK == 0 ) THEN
!
! ------------- Antoehre SPD server process is running
!
                WRITE ( 6, '(A,I5,A)' ) 'sdp_server: '// &
     &                ' another spd_server with PID ', &
     &                SERVER_PID, ' is running'
                CALL EXIT ( 1 )
           END IF
      END IF
!
! --- Write the PID of the furrenty process in the pid file
!
      WRITE ( UNIT=STR(1:25), FMT='(A,1X,I5)' ) GET_CDATE(), GETPID()
      IUER = -1
      CALL WR_TEXT ( 1, STR, SPD%CONF%SERVER_PID_FILE, IUER )
      IF ( IUER .NE. 0 ) THEN
           WRITE ( 6, '(A)' ) 'spd_daemon: failure to write in daemon PID file '// &
     &                  SPD%CONF%SERVER_PID_FILE(1:I_LEN(SPD%CONF%SERVER_PID_FILE))
           CALL EXIT ( 1 )
      END IF
      FILE_PID = SPD%CONF%SERVER_PID_FILE
!
! --- Set catching TERM and CLD signals
!
      IPAR_HANDLER = 2
      IS = SIGNAL ( %VAL(SIGTERM), SPD_SIGTERM_PROC )
      IS = SIGNAL ( %VAL(SIGCLD),  SPD_SIGCLD_PROC  )
      SPD_NUM_PROC = 0 ! Initialize the number of process counter
!
! --- Open log file
!
      LUN_SER = 11
      OPEN ( UNIT=LUN_SER, FILE=SPD%CONF%SERVER_LOG_FILE, STATUS='UNKNOWN', &
     &       ACCESS='APPEND', IOSTAT=IUER )
      IF ( IUER .NE. 0 ) THEN
           WRITE ( 6, '(A)' ) 'sdp_server: Cannot open log file '// &
     &                         SPD%CONF%SERVER_LOG_FILE(1:I_LEN(SPD%CONF%SERVER_LOG_FILE))
           CALL EXIT ( 1 )
      END IF
      CALL SPD_LOG ( 'MAIN  ', 0, 'I', ' ', 'spd_server started' )
!
! --- Open the socket
!
      IS = SOCK_OPEN_SERVER ( SPD%CONF%SERVER_PORT, SOCK_FD, MESSAGE )
      IF ( IS < 0  .AND. &
     &     INDEX ( MESSAGE, "bind: Address already in use" ) > 0  ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( SPD%CONF%SERVER_PORT, STR )
           CALL SPD_LOG ( 'MAIN  ', 101, 'F', ' ', 'spd_server cannot start '// &
     &         'the port '//STR(1:I_LEN(STR))//' is already in use. '// &
     &         'Please try later' )
         ELSE IF ( IS < 0 ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( SPD%CONF%SERVER_PORT, STR )
           CALL SPD_LOG ( 'MAIN  ', 102, 'F', ' ', 'spd_server failure to '// &
     &         'open the server on port '//STR(1:I_LEN(STR))//'. Reason: '// &
     &          MESSAGE )
           WRITE ( LUN_SER, * ) 'NNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNN'
           WRITE ( LUN_SER, * ) ' '
           CALL SYSTEM ( 'netstat -an '//CHAR(0) )
           WRITE ( LUN_SER, * ) ' '
           WRITE ( LUN_SER, * ) 'PPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPP'
#ifdef LINUX
           CALL SYSTEM  ( 'cat /proc/net/tcp '//CHAR(0) )
#endif
      END IF
      IF ( IS < 0 ) THEN
           CALL CLOSE ( %VAL(SOCK_FD) )
           CALL SOCK_SHUTDOWN ( SOCK_FD )
           CALL EXIT ( 1 )
      END IF
      CALL CLRCH ( STR )
      CALL INCH  ( SPD%CONF%SERVER_PORT, STR )
      CALL SPD_LOG ( 'MAIN  ', 0, 'I', ' ', 'spd_server is '// &
     &              'listening port '//STR )
!
! --- Endless cycle of processing incoming connections
!
      DO 410 J1=1,1024*1024*1024
!
! ------- Accept incopming connections.
!
          IS = SOCK_ACCEPT ( SOCK_FD, REM_FD, SPD__ACCEPT_TIMEOUT, MESSAGE )
          IF ( IS == -1 ) THEN
               IF ( MESSAGE(1:31) == 'select: Interrupted system call' ) THEN
                    GOTO 410
                 ELSE 
                    CALL SPD_LOG ( 'MAIN  ', 103, 'F', ' ', 'Failure in '// &
     &                  'socket accepting. Reason: '//MESSAGE )
                    IF ( SOCK_FD > 0 ) THEN
                         CALL SOCK_SHUTDOWN ( SOCK_FD )
                         CALL CLOSE ( %VAL(SOCK_FD) )
                    END IF
                    CALL EXIT ( 1 )
               END IF
          END IF
!
! ------- Get remote address
!
          IADR = INET_NTOA ( %VAL(IS) )
          CALL CLRCH   ( IP_ADR_STR )
          CALL STRNCPY ( IP_ADR_STR, %VAL(IADR), %VAL(LEN(IP_ADR_STR)-1) )
          CALL SPD_LOG ( 'MAIN  ', 0, 'I', ' ', 'Accepted connection '// &
     &                  'from '//IP_ADR_STR )
!
! ------- Check whether the remote IP is allowed
!
          FL_IP_ALLOWED = .FALSE.
          DO 420 J2=1,SPD%CONF%NUM_IP
             IF ( SPD%CONF%IP_ALLOW(J2)(1:I_LEN(IP_ADR_STR)) == IP_ADR_STR(1:I_LEN(IP_ADR_STR))  ) THEN
                  FL_IP_ALLOWED = .TRUE.
             END IF 
 420      CONTINUE 
          IF ( .NOT. FL_IP_ALLOWED ) THEN
!
! ------------- Not allowed? Close connection
!
                CALL SPD_LOG ( 'MAIN  ', 0, 'I', ' ', 'Closed connection '// &
     &              'from '//IP_ADR_STR//' because this IP is not in the allowed list' )
                CALL CLOSE ( %VAL(REM_FD) )
                GOTO 410
          END IF
!
! ------- Read a command from the remote socket
!
          IS = SOCK_READ_POLL ( REM_FD, SIZEOF(COM), COM, SIZEOF(COM), &
     &                          SPD__READ_TIMEOUT, MESSAGE )
          IF ( INDEX ( MESSAGE, "Time out has expired" ) > 0 ) THEN
               CALL SPD_LOG ( 'MAIN  ', 0, 'I', ' ', 'Closed connection '// &
     &             'from '//IP_ADR_STR//' due to timeout' )
               CALL CLOSE ( %VAL(REM_FD) )
               GOTO 410
             ELSE IF ( IS == -1 ) THEN
               CALL SPD_LOG ( 'MAIN  ', 104, 'E', ' ', 'Failed '// &
     &             'to read a command '//IP_ADR_STR//'. Reason: '// &
     &             MESSAGE(1:I_LEN(MESSAGE))//'. Close connection' )
               CALL CLOSE ( %VAL(REM_FD) )
               GOTO 410
          END IF
!
! ------- Parse a command
!
          IF ( COM%VERB == 'ping    ' ) then
!
! ============ This is ping command
!
               CALL SPD_LOG ( 'MAIN  ', 0, 'I', ' ', 'Received '// &
     &             'ping command from '//IP_ADR_STR  )
!
! ------------ Just write return
!
               COM%VERB = 'ping    '
               COM%LEN    = 0
               IS = SOCK_WRITE ( REM_FD, SIZEOF(COM), COM, MESSAGE )
               IF ( IS == -1 ) THEN
                    CALL SPD_LOG ( 'MAIN  ', 105, 'E', ' ', 'Failure '// &
     &                  'in sending ping response to '//IP_ADR_STR//'. Reason: '// &
     &                  MESSAGE )
               END IF
!
! ------------ ... and close connection
!
               CALL CLOSE ( %VAL(REM_FD) )
               GOTO 410
             ELSE IF ( COM%VERB == 'shutdown' ) THEN
!
! ============ This is shutdown command
!
               CALL SPD_LOG ( 'MAIN  ', 0, 'I', ' ', 'Received '// &
     &             'shutdown command from '//IP_ADR_STR  )
!
! ------------ Write acknowledgment
!
               COM%VERB = 'ack     '
               COM%LEN    = 0
               IS = SOCK_WRITE ( REM_FD, SIZEOF(COM), COM, MESSAGE )
!
! ------------ ... close connection
!
               CALL CLOSE ( %VAL(REM_FD) )
!
! ------------ and the server
!
               CALL SOCK_SHUTDOWN ( SOCK_FD )
               CALL CLOSE ( %VAL(SOCK_FD) )
               CALL SPD_LOG ( 'MAIN  ', 0, 'I', ' ', &
     &                'spd_server was shutdown by peer from IP '//IP_ADR_STR )
               CALL EXIT  ( 0 )
             ELSE IF ( COM%VERB == 'get_2pd ' ) THEN
!
! ------------ Get 2-point path delay
!
               LAST_REQ_NUM = LAST_REQ_NUM + 1
               CALL CLRCH  ( REQ_ID )
               CALL INCH   ( LAST_REQ_NUM, REQ_ID )
               CALL CHASHR ( REQ_ID )
               CALL BLANK_TO_ZERO ( REQ_ID )
               REQ_ID(1:1) = 'R'
               CALL SPD_LOG ( 'MAIN  ', 0, 'I', ' ', 'Received '// &
     &             'get_2pd  request '//REQ_ID//' from '//IP_ADR_STR )
               IUER = -1
               IF ( SPD_NUM_PROC > SPD%CONF%MAX_NUM_PROC ) THEN
                    CALL CLRCH ( STR )
                    CALL INCH  ( SPD_NUM_PROC, STR )
                    STR = 'Server is too busy: limit of '//STR(1:I_LEN(STR))// &
     &                    'processes has reached. Please try later'
                    COM%VERB = 'error'
                    COM%LEN  = ILEN(STR)
                    IS = SOCK_WRITE ( REM_FD, SIZEOF(COM),   COM, MESSAGE )
                    IS = SOCK_WRITE ( REM_FD, ILEN(STR),  %REF(STR), STR )
                    CALL CLOSE ( %VAL(REM_FD) )
                    CALL SPD_LOG ( 'MAIN  ', 106, 'E', ' ', STR )
                    GOTO 410
               END IF
               PID = FORK()
               IF ( PID < 0 ) THEN
!
! ----------------- Failure to fork a process
!
                    CALL CLRCH  ( STR )
                    CALL GERROR ( STR )
                    CALL SPD_LOG ( 'MAIN  ', 107, 'F', ' ', 'Cannot fork '// &
     &                  'a process. Reason: '//STR )
                 ELSE IF ( PID == 0 ) THEN
!
! ----------------- This portion of code is for a child process
!
                    IUER = 0
                    CALL PD_PROC ( SPD, REQ_ID, IP_ADR_STR, SOCK_FD, REM_FD, IUER )
                    CALL CLOSE ( %VAL(REM_FD) )
                    IF ( IUER .NE. 0 ) THEN
                         CALL EXIT ( 1 )
                       ELSE 
                         CALL EXIT ( 0 )
                    END IF
                 ELSE
!
! ----------------- This portion of code for parent. Update process table
!
                    IS = SPD_UPDATE_PROC ( PID, REM_FD )
               END IF
          END IF
 410  CONTINUE 
!!
      END  PROGRAM   SPD_SERVER  !#!#
!
! ------------------------------------------------------------------------
!
      FUNCTION   SPD_GET_LAST_REQ_NUM ( SPD, IUER  )
! ************************************************************************
! *                                                                      *
! *   Routine SPD_GET_LAST_REQ_NUM 
! *                                                                      *
! * ## 23-JAN-2015 SPD_GET_LAST_REQ_NUM v1.0 (c) L. Petrov 23-JAN-2015 # *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INTEGER*4  SPD_GET_LAST_REQ_NUM 
      INCLUDE   'spd.i'
      INCLUDE   'spd_local.i'
      TYPE     ( SPD_3D__TYPE ) :: SPD
      INTEGER*4  IUER
      INTEGER*4  MBUF
      PARAMETER  ( MBUF = 4*1024*1024 )
      LOGICAL*1  LEX
      CHARACTER, ALLOCATABLE :: BUF(:)*128
      INTEGER*4  NBUF, J1, IER
!
      SPD_GET_LAST_REQ_NUM = 0
      INQUIRE ( FILE=SPD%CONF%SERVER_LOG_FILE, EXIST=LEX )
      IF ( .NOT. LEX ) THEN
            CALL ERR_LOG ( 0, IUER )
            RETURN 
      END IF
!
      ALLOCATE ( BUF(MBUF), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 2911, IUER, 'SPD_GET_LAST_REQ_NUM', 'Error '// &
     &         'in an attempt to allocate memory for the bufffer' )
           RETURN 
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL RD_TEXT  ( SPD%CONF%SERVER_LOG_FILE, MBUF, BUF, NBUF, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 2912, IUER, 'SPD_GET_LAST_REQ_NUM', 'Error '// &
     &         'in reading log file '//SPD%CONF%SERVER_LOG_FILE )
           DEALLOCATE ( BUF )
           RETURN 
      END IF
!
      DO 410 J1=1,NBUF  
         IF ( BUF(J1)(39:63) == 'Received get_2pd  request' ) THEN
              READ ( UNIT=BUF(J1)(66:70), FMT='(I5)', IOSTAT=IER ) SPD_GET_LAST_REQ_NUM  
         END IF 
  410 CONTINUE               
      DEALLOCATE ( BUF )
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  FUNCTION  SPD_GET_LAST_REQ_NUM  !#!  
