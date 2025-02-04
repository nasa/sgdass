      PROGRAM    SPD_DAEMON
! ************************************************************************
! *                                                                      *
! *   Program  SPD_DAEMON
! *                                                                      *
! *  ### 06-JAN-2015   SPD_DAEMON  v1.1 (c)  L. Petrov  31-MAR-2015 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'spd.i'
      INCLUDE   'spd_local.i'
      INCLUDE   'spd_common.i'
      TYPE     ( SPD_3D__TYPE ) :: SPD
      INTEGER*4  MBUF
      PARAMETER  ( MBUF = 128 )
      REAL*8     TIMEOUT
      PARAMETER  ( TIMEOUT = 10.0D0 )
      CHARACTER  CONF_FILE*128, TASK*8, STR*128, BUF(MBUF)*128
      CHARACTER, EXTERNAL :: GET_CDATE*19
      LOGICAL*1  LEX
      INTEGER*4  PID, SIGTERM_I4, ARG_LEN, IK, IS, NP, SERVER_PID, &
     &           DAEMON_PID, WNOHANG, SIGTERM, SIGKILL, NBUF, J1, STATUS, IUER 
      INTEGER*4, EXTERNAL :: FORK, ILEN, I_LEN, KILL, SIGNAL, GETPID, &
     &           UNLINK, SPD_SIGTERM_PROC, EXECL, WAITPID 
!
      CALL GET_SYSTEM_CONSTANT ( 'SIGTERM', SIGTERM, ARG_LEN )
      CALL GET_SYSTEM_CONSTANT ( 'SIGKILL', SIGKILL, ARG_LEN )
      CALL GET_SYSTEM_CONSTANT ( 'WNOHANG', WNOHANG, ARG_LEN )
!
      IF ( IARGC() < 2 ) THEN
           WRITE ( 6, * ) 'Usage: spd_daemon configuration_file task'
           CALL EXIT ( 0 )
      END IF
      CALL GETARG ( 1, CONF_FILE )
      CALL GETARG ( 2, TASK      )
!
! --- Parse configuration file
!
      IUER = -1
      CALL SPD_DAEMON_CONFIG ( SPD, CONF_FILE, IUER )
      IF ( IUER .NE. 0 ) CALL EXIT ( 1 )
!
      IF ( TASK == 'start' ) THEN
           CONTINUE 
         ELSE IF ( TASK == 'stop' ) THEN
!
! -------- Stop server process
!
           INQUIRE ( FILE=SPD%CONF%SERVER_PID_FILE, EXIST=LEX )
           IF ( .NOT. LEX ) THEN
                WRITE ( 6, '(A,I5,A)' ) 'sdp_daemon: '// &
     &               ' failure to stop spd_server PID file '// &
     &               SPD%CONF%SERVER_PID_FILE(1:I_LEN(SPD%CONF%SERVER_PID_FILE))// &
     &               ' does not exist' 
                CALL EXIT ( 1 )
           END IF
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
           CALL CHIN ( STR(21:), SERVER_PID )
           IK = KILL ( %VAL(SERVER_PID), %VAL(SIGTERM) )
           IF ( IK .NE. 0 ) THEN
                IF ( IUER .NE. 0 ) THEN
                     CALL GERROR ( STR )
                     WRITE ( 6, '(A,I5,A)' ) 'sdp_daemon: '// &
     &               ' failure to stop spd_server with PID ', &
     &               SERVER_PID, ' : '//STR(1:I_LEN(STR))
                     CALL EXIT ( 1 )
                END IF
           END IF
           IS = WAITPID ( %VAL(SERVER_PID), STATUS, %VAL(WNOHANG) )
!
           IS = UNLINK ( SPD%CONF%SERVER_PID_FILE(1:I_LEN(SPD%CONF%SERVER_PID_FILE))//CHAR(0) )
           IF ( IS .NE. 0 ) THEN
                CALL CLRCH ( STR ) 
                CALL GERROR ( STR )
                WRITE ( 6, '(A,I5,A)' ) 'sdp_daemon: '// &
     &                 ' failed to remove stale PID file '// &
     &                 SPD%CONF%SERVER_PID_FILE(1:I_LEN(SPD%CONF%SERVER_PID_FILE))// &
     &                 ' : '//STR(1:I_LEN(STR))
           END IF
           WRITE ( 6, '(A,I5,A)' ) 'SPD_SERVER process with pid ', SERVER_PID, ' is stopped'
!
! -------- Stop daemon process
!
           INQUIRE ( FILE=SPD%CONF%DAEMON_PID_FILE, EXIST=LEX )
           IF ( .NOT. LEX ) THEN
                WRITE ( 6, '(A,I5,A)' ) 'sdp_daemon: '// &
     &               ' failure to stop spd_server PID file '// &
     &               SPD%CONF%DAEMON_PID_FILE(1:I_LEN(SPD%CONF%DAEMON_PID_FILE))// &
     &               ' does not exist' 
                CALL EXIT ( 1 )
           END IF
           IUER = 0
           CALL RD_TEXT ( SPD%CONF%DAEMON_PID_FILE, 1, STR, NP, IUER )
           IF ( IUER .NE. 0 ) THEN
                WRITE ( 6, '(A,I5,A)' ) 'sdp_daemon: '// &
     &               ' failure to read PID file '// &
     &               SPD%CONF%DAEMON_PID_FILE(1:I_LEN(SPD%CONF%DAEMON_PID_FILE)) 
           END IF
           CALL CHIN ( STR(21:), DAEMON_PID )
           IK = KILL ( %VAL(DAEMON_PID), %VAL(SIGKILL) )
           IF ( IK .NE. 0 ) THEN
                IF ( IUER .NE. 0 ) THEN
                     CALL GERROR ( STR )
                     WRITE ( 6, '(A,I5,A)' ) 'sdp_daemon: '// &
     &               ' failure to stop spd_daemon with PID ', &
     &               DAEMON_PID, ' : '//STR(1:I_LEN(STR))
                END IF
           END IF
           IS = WAITPID ( %VAL(DAEMON_PID), STATUS, %VAL(WNOHANG) )
!
           IS = UNLINK ( SPD%CONF%DAEMON_PID_FILE(1:I_LEN(SPD%CONF%DAEMON_PID_FILE))//CHAR(0) )
           IF ( IS .NE. 0 ) THEN
                CALL CLRCH ( STR ) 
                CALL GERROR ( STR )
                WRITE ( 6, '(A,I5,A)' ) 'sdp_daemon: '// &
     &                 ' failed to remove stale PID file '// &
     &                 SPD%CONF%DAEMON_PID_FILE(1:I_LEN(SPD%CONF%DAEMON_PID_FILE))// &
     &                 ' : '//STR(1:I_LEN(STR))
           END IF
           WRITE ( 6, '(A,I5,A)' ) 'SPD_DAEMON process with pid ', DAEMON_PID, ' is stopped'
           CALL EXIT ( 0 )
         ELSE IF ( TASK == 'ping' ) THEN
           CONTINUE 
           CALL EXIT ( 0 )
      END IF
!
      PID = FORK()
      IF ( PID < 0 ) THEN
           WRITE ( 6, * ) 'Failure to create a process for daemon'
           CALL EXIT ( 1 )
        ELSE IF ( PID > 0 ) THEN
           CALL EXIT ( 0 ) 
      END IF
!
      SERVER_PID = FORK()
      IF ( SERVER_PID < 0 ) THEN
           WRITE ( 6, * ) 'Failure to create a process for server'
           CALL EXIT ( 1 )
        ELSE IF ( SERVER_PID == 0 ) THEN
!
! -------- Child
!
           IS = EXECL ( SPD%CONF%SERVER_SERVER_EXE(1:I_LEN(SPD%CONF%SERVER_SERVER_EXE))//CHAR(0), &
     &                  SPD%CONF%SERVER_SERVER_EXE(1:I_LEN(SPD%CONF%SERVER_SERVER_EXE))//CHAR(0), &
     &                  CONF_FILE(1:I_LEN(CONF_FILE))//CHAR(0), &
     &                  %VAL(0) )
           CALL EXIT ( IS )
      END IF
!
! --- Write the server PID in the pid file
!
      WRITE ( UNIT=STR(1:25), FMT='(A,1X,I5)' ) GET_CDATE(), SERVER_PID
      IUER = -1
      CALL WR_TEXT ( 1, STR, SPD%CONF%DAEMON_PID_FILE, IUER )
      IF ( IUER .NE. 0 ) THEN
           WRITE ( 6, '(A)' ) 'spd_daemon: failure to write in daemon PID file '// &
     &                         SPD%CONF%SERVER_PID_FILE(1:I_LEN(SPD%CONF%SERVER_PID_FILE))
           CALL EXIT ( 1 )
      END IF
!
! --- Change the current directory
!
      CALL CHDIR ( '/tmp'//CHAR(0) )
!
! --- write the PID in the pid file
!
      WRITE ( UNIT=STR(1:25), FMT='(A,1X,I5)' ) GET_CDATE(), GETPID()
      IUER = -1
      CALL WR_TEXT ( 1, STR, SPD%CONF%DAEMON_PID_FILE, IUER )
      IF ( IUER .NE. 0 ) THEN
           WRITE ( 6, '(A)' ) 'spd_daemon: failure to write in daemon PID file '// &
     &                  SPD%CONF%DAEMON_PID_FILE(1:I_LEN(SPD%CONF%DAEMON_PID_FILE))
           CALL EXIT ( 1 )
      END IF
!
! --- Set catching TERM signal
!
      IPAR_HANDLER = 1
      IS = SIGNAL ( %VAL(SIGTERM), SPD_SIGTERM_PROC )
!
      LUN_SER = 11
      OPEN ( UNIT=LUN_SER, FILE=SPD%CONF%DAEMON_LOG_FILE, STATUS='UNKNOWN', &
     &       ACCESS='APPEND', IOSTAT=IUER )
      IF ( IUER .NE. 0 ) THEN
           WRITE ( 6, '(A)' ) 'sdp_daemon: Cannot open log file '// &
     &                         SPD%CONF%DAEMON_LOG_FILE(1:I_LEN(SPD%CONF%DAEMON_LOG_FILE))
           CALL EXIT ( 1 )
      END IF
!@      CLOSE ( UNIT = 5 )
!@      CLOSE ( UNIT = 6 )
!
      WRITE ( UNIT=LUN_SER, FMT='(A)' ) GET_CDATE()//' spd_daemon started'
      WRITE ( UNIT=LUN_SER, FMT='(A,I5,A)' ) GET_CDATE()//' spd_daemon: server process ', &
     &                                                 SERVER_PID, ' was launched'
      CALL FLUSH ( LUN_SER )
      DO 410 J1=1,1000*1000*1000
!
! ------ Waif for TIMEOUT seconds
!
         CALL LIB$WAIT ( TIMEOUT )
!
! ------ Check the server process
!
         IS = WAITPID ( %VAL(SERVER_PID), STATUS, %VAL(WNOHANG) )
         IK = KILL ( %VAL(SERVER_PID), %VAL(0) )
         IF ( IK .NE. 0 ) THEN
              WRITE ( UNIT=LUN_SER, FMT='(A,I5,A)' ) GET_CDATE()//' spd_daemon: server process ', &
     &                                               SERVER_PID, ' has died'
              CALL FLUSH ( LUN_SER )
!
              SERVER_PID = FORK()
              IF ( SERVER_PID < 0 ) THEN
                   WRITE ( UNIT=LUN_SER, FMT='(A)' ) GET_CDATE()//' spd_daemon: '// &
     &                                               'Failure to create a process for server'
                   CALL FLUSH ( LUN_SER )
                   CALL EXIT ( 1 )
                ELSE IF ( SERVER_PID == 0 ) THEN
!
! ---------------- Child
!
                   IS = EXECL ( SPD%CONF%SERVER_SERVER_EXE(1:I_LEN(SPD%CONF%SERVER_SERVER_EXE))//CHAR(0), &
     &                          SPD%CONF%SERVER_SERVER_EXE(1:I_LEN(SPD%CONF%SERVER_SERVER_EXE))//CHAR(0), &
     &                          CONF_FILE(1:I_LEN(CONF_FILE))//CHAR(0), &
     &                          %VAL(0) )
                   CALL EXIT ( IS )
              END IF
              WRITE ( UNIT=LUN_SER, FMT='(A,I5)' ) GET_CDATE()//' spd_daemon: '// &
     &                                         'Launched a new server process with pid ', SERVER_PID 
              CALL FLUSH ( LUN_SER )
            ELSE 
              WRITE ( UNIT=LUN_SER, FMT='(A,I5)' ) GET_CDATE()//' spd_daemon: OK  ik= ', IK
              CALL FLUSH ( LUN_SER )
         END IF
 410  CONTINUE 
      CLOSE ( UNIT=LUN_SER )
      END  PROGRAM    SPD_DAEMON  !#!  
