      SUBROUTINE SPD_CLI_INIT ( SPD_CLI_CONF, SPD_CLI, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  SPD_CLI_INIT  parses configuration file of the SPD client *
! *   and initializes object SPD_CLI for future operations of its        *
! *   interaction with the SPD server.                                   *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *                                                                      *
! * SPD_CLI_CONF ( CHARACTER ) -- name of the SPD Client configuration   *
! *                               file.                                  *
! *                                                                      *
! * _________________________ Output parameters: _______________________ *
! *                                                                      *
! * SPD_CLI ( SPD_CLI__TYPE  ) -- internal SPD Client datastructure that *
! *                               is used in every SPD Client operation. *
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
! *     IUER ( INTEGER*4, OPT ) -- Universal error handler.              *
! *                           Input: switch IUER=0 -- no error messages  *
! *                                  will be generated even in the case  *
! *                                  of error. IUER=-1 -- in the case of *
! *                                  error the message will be put on    *
! *                                  stdout.                             *
! *                           Output: 0 in the case of successful        *
! *                                   completion and non-zero in the     *
! *                                   case of error.                     *
! *                                                                      *
! *  ### 09-JAN-2015  SPD_CLI_INIT  v1.1 (c)  L. Petrov  31-MAR-2015 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'spd.i'
      INCLUDE   'spd_local.i'
      CHARACTER  SPD_CLI_CONF*(*)
      TYPE     ( SPD_CLI__TYPE ) :: SPD_CLI
      INTEGER*4  IUER
      INTEGER*4  MBUF, MIND
      PARAMETER  ( MBUF = 128 )
      PARAMETER  ( MIND = 128 )
      CHARACTER  BUF(MBUF)*128, STR*128, STR1*128
      LOGICAL*1  LEX
      ADDRESS__TYPE DIR_DESC
      INTEGER*4  J1, NS, LIND, IND(2,MIND), N_CNF, IER
      INTEGER*4, EXTERNAL :: ILEN, I_LEN
#ifdef DARWIN
#      define   FUNC_OPENDIR  OPENDIR$INODE64
#else
#      define   FUNC_OPENDIR  OPENDIR
#endif
      ADDRESS__TYPE, EXTERNAL :: FUNC_OPENDIR, CLOSEDIR
!
      INQUIRE ( FILE=SPD_CLI_CONF, EXIST=LEX )
      IF ( .NOT. LEX ) THEN
           CALL ERR_LOG ( 5411, IUER, 'SPD_CLI_INIT', 'Cannot find '// &
     &         'configuration file '//SPD_CLI_CONF )
           RETURN 
      END IF
!
      IER = IUER 
!!      CALL ERR_PASS ( IUER, IER )
      CALL RD_TEXT  ( SPD_CLI_CONF, MBUF, BUF, NS, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 5412, IUER, 'SPD_CLI_INIT', 'Error in reading '// &
     &         'configuration file '//SPD_CLI_CONF )
           RETURN 
      END IF
!
      IF ( BUF(1)(1:LEN(SPD_CLI__LABEL)) .NE. SPD_CLI__LABEL ) THEN
           CALL TRAN ( 12, BUF(1), BUF(1) )
           CALL ERR_LOG ( 5413, IUER, 'SPD_CLI_INIT', 'Unrecogined format '// &
     &         'of SPD client configuration file '// &
     &          SPD_CLI_CONF(1:I_LEN(SPD_CLI_CONF))//' -- found label '// &
     &          BUF(1)(1:LEN(SPD_CLI__LABEL))//' while '//SPD_CLI__LABEL// &
     &         ' was expected' )
           RETURN 
      END IF
      CALL NOUT ( SIZEOF(SPD_CLI), SPD_CLI )
!
      N_CNF = 0
      DO 410 J1=2,NS
         IF ( BUF(J1)(1:1) == '#' ) GOTO 410
         CALL EXWORD ( BUF(J1), MIND, LIND, IND, CHAR(32)//CHAR(9), IER )
         IF ( LIND < 2 ) GOTO 410
         IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'SERVER_NAME:' ) THEN
              N_CNF = N_CNF + 1
              SPD_CLI%SERVER_NAME =  BUF(J1)(IND(1,2):IND(2,2))
            ELSE IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'CONN_TIMEOUT:' ) THEN
              N_CNF = N_CNF + 1
              READ ( UNIT=BUF(J1)(IND(1,2):IND(2,2)), FMT='(F10.2)', IOSTAT=IER ) SPD_CLI%CONN_TIMEOUT
              IF ( IER .NE. 0 .OR.              &
     &             SPD_CLI%CONN_TIMEOUT < 0.0D0 ) THEN
!
                   CALL CLRCH ( STR )
                   CALL INCH  ( J1, STR )
                   CALL ERR_LOG ( 5414, IUER, 'SPD_CLI_INIT', 'Error in '// &
     &                 'parsing the '//STR(1:I_LEN(STR))//' th line of '// &
     &                 'configuration file '//SPD_CLI_CONF(1:I_LEN(SPD_CLI_CONF))// &
     &                 ' -- a non-negatvie real number was expected '// &
     &                 ' but got'//BUF(J1)(IND(2,1):IND(2,2)) )
                   RETURN 
              END IF
            ELSE IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'READ_TIMEOUT:' ) THEN
              N_CNF = N_CNF + 1
              READ ( UNIT=BUF(J1)(IND(1,2):IND(2,2)), FMT='(F10.2)', IOSTAT=IER ) SPD_CLI%READ_TIMEOUT
              IF ( IER .NE. 0 .OR.              &
     &             SPD_CLI%READ_TIMEOUT < 0.0D0 ) THEN
!
                   CALL CLRCH ( STR )
                   CALL INCH  ( J1, STR )
                   CALL ERR_LOG ( 5414, IUER, 'SPD_CLI_INIT', 'Error in '// &
     &                 'parsing the '//STR(1:I_LEN(STR))//' th line of '// &
     &                 'configuration file '//SPD_CLI_CONF(1:I_LEN(SPD_CLI_CONF))// &
     &                 ' -- a non-negatvie real number was expected '// &
     &                 ' but got'//BUF(J1)(IND(2,1):IND(2,2)) )
                   RETURN 
              END IF
            ELSE IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'SERVER_PORT:' ) THEN
              N_CNF = N_CNF + 1
              READ ( UNIT=BUF(J1)(IND(1,2):IND(2,2)), FMT='(I5)', IOSTAT=IER ) SPD_CLI%SERVER_PORT
!
              IF ( IER .NE. 0 .OR.              &
     &             SPD_CLI%SERVER_PORT < 1 .OR. &
     &             SPD_CLI%SERVER_PORT > 65535  ) THEN
!
                   CALL CLRCH ( STR )
                   CALL INCH  ( J1, STR )
                   CALL ERR_LOG ( 5415, IUER, 'SPD_CLI_INIT', 'Error in '// &
     &                 'parsing the '//STR(1:I_LEN(STR))//' th line of '// &
     &                 'configuration file '//SPD_CLI_CONF(1:I_LEN(SPD_CLI_CONF))// &
     &                 ' -- an integer number in range [1, 65535] was expected '// &
     &                 ' but got'//BUF(J1)(IND(1,2):IND(2,2)) )
                   RETURN 
              END IF
            ELSE IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'SPD_TYPE_1:' ) THEN
              N_CNF = N_CNF + 1
              IF ( BUF(J1)(IND(1,2):IND(2,2)) == SPD__TYPE_W532 ) THEN
                   SPD_CLI%SPD_TYPE_1 = BUF(J1)(IND(1,2):IND(2,2))
                 ELSE IF ( BUF(J1)(IND(1,2):IND(2,2)) == SPD__TYPE_W1064 ) THEN
                   SPD_CLI%SPD_TYPE_1 = BUF(J1)(IND(1,2):IND(2,2))
                 ELSE IF ( BUF(J1)(IND(1,2):IND(2,2)) == SPD__TYPE_RADIO ) THEN
                   SPD_CLI%SPD_TYPE_1 = BUF(J1)(IND(1,2):IND(2,2))
                 ELSE 
                   CALL CLRCH ( STR )
                   CALL INCH  ( J1, STR )
                   CALL ERR_LOG ( 5416, IUER, 'SPD_CLI_INIT', 'Error in '// &
     &                 'parsing the '//STR(1:I_LEN(STR))//' th line of the '// &
     &                 'configuration file '//SPD_CLI_CONF(1:I_LEN(SPD_CLI_CONF))// &
     &                 ' -- unsupported value of the key SPD_TYPE_1: '// &
     &                 BUF(J1)(IND(1,2):IND(2,2))//' while one of '// &
     &                 SPD__TYPE_W532//' '//SPD__TYPE_W1064//' '//SPD__TYPE_RADIO// &
     &                 ' was expected' )
                   RETURN 
              END IF
            ELSE IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'SPD_TYPE_2:' ) THEN
              N_CNF = N_CNF + 1
              IF ( BUF(J1)(IND(1,2):IND(2,2)) == SPD__TYPE_W532 ) THEN
                   SPD_CLI%SPD_TYPE_2 = BUF(J1)(IND(1,2):IND(2,2))
                 ELSE IF ( BUF(J1)(IND(1,2):IND(2,2)) == SPD__TYPE_W1064 ) THEN
                   SPD_CLI%SPD_TYPE_2 = BUF(J1)(IND(1,2):IND(2,2))
                 ELSE IF ( BUF(J1)(IND(1,2):IND(2,2)) == SPD__TYPE_RADIO ) THEN
                   SPD_CLI%SPD_TYPE_2 = BUF(J1)(IND(1,2):IND(2,2))
                 ELSE IF ( BUF(J1)(IND(1,2):IND(2,2)) == SPD__TYPE_NONE ) THEN
                   SPD_CLI%SPD_TYPE_2 = BUF(J1)(IND(1,2):IND(2,2))
                 ELSE 
                   CALL CLRCH ( STR )
                   CALL INCH  ( J1, STR )
                   CALL ERR_LOG ( 5417, IUER, 'SPD_CLI_INIT', 'Error in '// &
     &                 'parsing the '//STR(1:I_LEN(STR))//' th line of the '// &
     &                 'configuration file '//SPD_CLI_CONF(1:I_LEN(SPD_CLI_CONF))// &
     &                 ' -- unsupported value of the key SPD_TYPE_2: '// &
     &                 BUF(J1)(IND(1,2):IND(2,2))//' while one of '// &
     &                 SPD__TYPE_W532//' '//SPD__TYPE_W1064//' '//SPD__TYPE_RADIO// &
     &                 SPD__TYPE_NONE//' was expected' )
                   RETURN 
              END IF
            ELSE IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'SPD_PATH:' ) THEN
              N_CNF = N_CNF + 1
              SPD_CLI%SPD_PATH = BUF(J1)(IND(1,2):IND(2,2))
!
              DIR_DESC = FUNC_OPENDIR ( SPD_CLI%SPD_PATH(1:I_LEN(SPD_CLI%SPD_PATH))//CHAR(0) )
              IF ( DIR_DESC .LE. 0 ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( J1, STR )
                   CALL ERR_LOG ( 5418, IUER, 'SPD_CLI_INIT', 'Error in '// &
     &                 'parsing the '//STR(1:I_LEN(STR))//' th line of '// &
     &                 'configuration file '// &
     &                 SPD_CLI_CONF(1:I_LEN(SPD_CLI_CONF))//' -- directory '// &
     &                 SPD_CLI%SPD_PATH(1:I_LEN(SPD_CLI%SPD_PATH))// &
     &                 ' defined in SPD_PATH does not exist' )
                   RETURN 
                 ELSE 
                   IER = CLOSEDIR ( %VAL(DIR_DESC) )
              END IF
         END IF 
 410  CONTINUE 
!
      IF ( N_CNF .NE. SPD__MCLI ) THEN
           CALL CLRCH ( STR  )
           CALL CLRCH ( STR1 )
           CALL INCH  ( N_CNF, STR )
           CALL INCH  ( SPD__MCLI, STR1 )
           CALL ERR_LOG ( 5419, IUER, 'SPD_CLI_INIT', 'Only '// &
     &          STR(1:I_LEN(STR))//' keywords out of '//STR1(1:I_LEN(STR1))// &
     &          ' were found in the configuration file '//SPD_CLI_CONF )
           RETURN 
      END IF
!
      SPD_CLI%STATUS = CLI__INIT
      SPD_CLI%CONF_FILE = SPD_CLI_CONF 
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  SPD_CLI_INIT  !#!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE SPD_CLI_PING ( SPD_CLI, IVRB, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine SPD_CLI_PING  checks whether the SPD server is running.    *
! *   It send ping requests and waits for the reseponse. If no response  *
! *   comes for the period of time specified in the SPD Client           *
! *   configuration file ( READ_TIMEOUT: ), error is returned.           *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *                                                                      *
! * SPD_CLI ( SPD_CLI__TYPE  ) -- internal SPD Client datastructure that *
! *                               is used in every SPD Client operation. *
! *    IVRB ( INTEGER*4      ) -- verbosity level.                       *
! *                               0 -- silent: no messages are printed.  *
! *                               1 -- a message OK will be printed if   *
! *                                    the command is successful.        *
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
! *     IUER ( INTEGER*4, OPT ) -- Universal error handler.              *
! *                           Input: switch IUER=0 -- no error messages  *
! *                                  will be generated even in the case  *
! *                                  of error. IUER=-1 -- in the case of *
! *                                  error the message will be put on    *
! *                                  stdout.                             *
! *                           Output: 0 in the case of successful        *
! *                                   completion and non-zero in the     *
! *                                   case of error.                     *
! *                                                                      *
! *  ### 10-JAN-2015  SPD_CLI_PING  v1.0 (c)  L. Petrov 10-JAN-2015 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'spd.i'
      INCLUDE   'spd_local.i'
      TYPE     ( SPD_CLI__TYPE ) :: SPD_CLI
      INTEGER*4  IVRB, IUER
      CHARACTER  MESSAGE*128
      INTEGER*4  IS
      INTEGER*4, EXTERNAL :: ILEN, I_LEN, SOCK_OPEN_CLIENT, &
     &                       SOCK_READ_POLL, SOCK_WRITE
!
      IF (  SPD_CLI%STATUS .NE. CLI__INIT ) THEN
            WRITE ( 6, '(A,I5,A)' ) 'SPD_CLI object has not been initialized'
            CALL ERR_PASS ( 5431, IUER )
            RETURN 
      END IF
!
! --- Open the socket to the server
!
      IS = SOCK_OPEN_CLIENT ( SPD_CLI%SERVER_NAME(1:I_LEN(SPD_CLI%SERVER_NAME))//CHAR(0), &
     &                        SPD_CLI%SERVER_PORT, SPD_CLI%SOCK_FD, MESSAGE )
      IF ( IS .EQ. -1 ) THEN
           IF ( IUER == -1 ) THEN
                WRITE ( 6, '(A,I5,A)' ) 'Server '//SPD_CLI%SERVER_NAME(1:I_LEN(SPD_CLI%SERVER_NAME))// &
     &                     ' on port ',SPD_CLI%SERVER_PORT, &
     &                     ' does not respond: '//MESSAGE(1:I_LEN(MESSAGE))
           END IF
           CALL ERR_PASS ( 5432, IUER )
           RETURN 
      END IF
      SPD_CLI%REM_IP_STR = MESSAGE(14:)  !  Extract the IP of the server
!
! --- Send command ping to the server
!
      SPD_CLI%SEND_COM%VERB = 'ping    '
      SPD_CLI%SEND_COM%LEN    = 0
      IS = SOCK_WRITE ( SPD_CLI%SOCK_FD, SIZEOF(SPD_CLI%SEND_COM), &
     &                  SPD_CLI%SEND_COM, MESSAGE )
      IF ( IS .EQ. -1 ) THEN
           IF ( IUER == -1 ) THEN
                WRITE ( 6, '(A,I5,A)' ) 'Server '//SPD_CLI%SERVER_NAME(1:I_LEN(SPD_CLI%SERVER_NAME))// &
     &                     ' on port ',SPD_CLI%SERVER_PORT, &
     &                     ' allows to connect but does not respond: '// &
     &                     MESSAGE(1:I_LEN(MESSAGE))
           END IF
           CALL CLOSE ( %VAL(SPD_CLI%SOCK_FD) )
           CALL ERR_PASS ( 5433, IUER )
           RETURN 
      END IF
!
! --- Listen to the server
!
      IS = SOCK_READ_POLL ( SPD_CLI%SOCK_FD, SIZEOF(SPD_CLI%RECV_COM), &
     &                      SPD_CLI%RECV_COM, SIZEOF(SPD_CLI%RECV_COM), &
     &                      SPD_CLI%READ_TIMEOUT, MESSAGE )
      IF ( IS .EQ. -1 ) THEN
           IF ( IUER == -1 ) THEN
                WRITE ( 6, '(A,I5,A)' ) 'Server '//SPD_CLI%SERVER_NAME(1:I_LEN(SPD_CLI%SERVER_NAME))// &
     &                     ' on port ',SPD_CLI%SERVER_PORT, &
     &                     ' allows to connect but does not return ping: '// &
     &                      MESSAGE(1:I_LEN(MESSAGE))
           END IF
           CALL CLOSE ( %VAL(SPD_CLI%SOCK_FD) )
           CALL ERR_PASS ( 5434, IUER )
           RETURN 
      END IF
!
! --- If the operation listen was successful, that means the server is alive
!
      IF ( IVRB > 0 ) THEN
           WRITE ( 6, '(A)' ) 'Server '//SPD_CLI%SERVER_NAME(1:I_LEN(SPD_CLI%SERVER_NAME))// &
     &                     ' ( '//SPD_CLI%REM_IP_STR(1:I_LEN(SPD_CLI%REM_IP_STR))// &
     &                     ' ) is OK'
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  SPD_CLI_PING  !#!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE SPD_CLI_SHUTDOWN ( SPD_CLI, IVRB, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine SPD_CLI_SHUTDOWN sends a command to shutdown the SPD       *
! *   server. Upon receiving, the server obediently will commit suicide. *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *                                                                      *
! * SPD_CLI ( SPD_CLI__TYPE  ) -- internal SPD Client datastructure that *
! *                               is used in every SPD Client operation. *
! *    IVRB ( INTEGER*4      ) -- verbosity level.                       *
! *                               0 -- silent: no messages are printed.  *
! *                               1 -- a message will be printed if      *
! *                                    the command is successful.        *
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
! *     IUER ( INTEGER*4, OPT ) -- Universal error handler.              *
! *                           Input: switch IUER=0 -- no error messages  *
! *                                  will be generated even in the case  *
! *                                  of error. IUER=-1 -- in the case of *
! *                                  error the message will be put on    *
! *                                  stdout.                             *
! *                           Output: 0 in the case of successful        *
! *                                   completion and non-zero in the     *
! *                                   case of error.                     *
! *                                                                      *
! * ## 10-JAN-2015  SPD_CLI_SHUTDOWN  v1.0 (c)  L. Petrov 10-JAN-2015 ## *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'spd.i'
      INCLUDE   'spd_local.i'
      TYPE     ( SPD_CLI__TYPE ) :: SPD_CLI
      INTEGER*4  IVRB, IUER
      CHARACTER  MESSAGE*128
      INTEGER*4  IS
      REAL*8       SPD__READ_TIMEOUT
      PARAMETER  ( SPD__READ_TIMEOUT   =  4.0D0 )
      INTEGER*4, EXTERNAL :: ILEN, I_LEN, SOCK_OPEN_CLIENT, SOCK_READ, SOCK_WRITE
!
      IF (  SPD_CLI%STATUS .NE. CLI__INIT ) THEN
            WRITE ( 6, '(A,I5,A)' ) 'SPD_CLI object has not been initialized'
            CALL ERR_PASS ( 5441, IUER )
            RETURN 
      END IF
!
      IS = SOCK_OPEN_CLIENT ( SPD_CLI%SERVER_NAME(1:I_LEN(SPD_CLI%SERVER_NAME))//CHAR(0), &
     &                        SPD_CLI%SERVER_PORT, SPD_CLI%SOCK_FD, MESSAGE )
      IF ( IS .EQ. -1 ) THEN
           WRITE ( 6, '(A,I5,A)' ) 'Server '//SPD_CLI%SERVER_NAME(1:I_LEN(SPD_CLI%SERVER_NAME))// &
     &                ' on port ',SPD_CLI%SERVER_PORT, &
     &                ' does not respond: '//MESSAGE(1:I_LEN(MESSAGE))
           CALL ERR_PASS ( 1201, IUER )
           RETURN 
      END IF
      SPD_CLI%REM_IP_STR = MESSAGE(14:)
!
      SPD_CLI%SEND_COM%VERB = 'shutdown'
      SPD_CLI%SEND_COM%LEN    = 0
      IS = SOCK_WRITE ( SPD_CLI%SOCK_FD, SIZEOF(SPD_CLI%SEND_COM), &
     &                  SPD_CLI%SEND_COM, MESSAGE )
      IF ( IS .EQ. -1 ) THEN
           WRITE ( 6, '(A,I5,A)' ) 'Server '//SPD_CLI%SERVER_NAME(1:I_LEN(SPD_CLI%SERVER_NAME))// &
     &                        ' on port ',SPD_CLI%SERVER_PORT, &
     &                        ' allows to connect but does not respond: '// &
     &                        MESSAGE(1:I_LEN(MESSAGE))
           CALL ERR_PASS ( 1202, IUER )
           CALL CLOSE ( %VAL(SPD_CLI%SOCK_FD) )
           RETURN 
      END IF
!
      IS = SOCK_READ ( SPD_CLI%SOCK_FD, SIZEOF(SPD_CLI%RECV_COM), SPD_CLI%RECV_COM, &
     &                 SIZEOF(SPD_CLI%RECV_COM), SPD__READ_TIMEOUT, MESSAGE )
      IF ( IS .EQ. -1 ) THEN
           WRITE ( 6, '(A,I5,A)' ) 'Server '//SPD_CLI%SERVER_NAME(1:I_LEN(SPD_CLI%SERVER_NAME))// &
     &                        ' on port ',SPD_CLI%SERVER_PORT, &
     &                        ' allows to connect but does not talk: '// &
     &                        MESSAGE(1:I_LEN(MESSAGE))
           CALL ERR_PASS ( 1203, IUER )
           CALL CLOSE ( %VAL(SPD_CLI%SOCK_FD) )
           RETURN 
      END IF
!
      CALL CLOSE ( %VAL(SPD_CLI%SOCK_FD) )
! *    IVRB ( INTEGER*4      ) -- verbosity level.                       *
! *                               0 -- silent: no messages are printed.  *
! *                               1 -- a message OK will be printed if   *
! *                                    the command is successful.        *
      IF ( IVRB > 0 ) THEN
           WRITE ( 6, '(A)' ) 'Server '//SPD_CLI%SERVER_NAME(1:I_LEN(SPD_CLI%SERVER_NAME))// &
     &                ' ( '//SPD_CLI%REM_IP_STR(1:I_LEN(SPD_CLI%REM_IP_STR))// &
     &                ' ) acknowledged request to shutdown'
      END IF
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  SPD_CLI_SHUTDOWN !#!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE SPD_CLI_GET_2PD ( SPD_CLI, N_DEL, SPD_2P, IVRB, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  SPD_CLI_GET_2PD  sends a request to the SPD server to     *
! *   compute path delays, waits for completion of computation and       *
! *   retrieves the result.                                              *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *                                                                      *
! * SPD_CLI ( SPD_CLI__TYPE  ) -- internal SPD Client datastructure that *
! *                               is used in every SPD Client operation. *
! *                               is used in every SPD Client operation. *
! *   N_DEL ( INTEGER*4      ) -- The number of points.                  *
! *    IVRB ( INTEGER*4      ) -- verbosity level.                       *
! *                               0 -- silent: no messages are printed.  *
! *                               1 -- message will be printed about     *
! *                                    executed operatinos.              *
! *                               2 -- message will be printed about     *
! *                                    executed operatinos and the       *
! *                                    progress counter will be updated  *
! *                                    every second.                     *
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
! *   SPD_2P ( SPD_2P__TYPE   ) -- Array of transport data structures    *
! *                                This array is sent to the server and  *
! *                                is retrieved back.                    *
! *   input fields:                                                      *
! *                                                                      *
! *       SPD_2P%MJD     ( INTEGER*4  ) -- Modified Julian date on       *
! *                                        midnight of the epoch.        *
! *                                        Units: days.                  *
! *       SPD_2P%TAI     ( REAL*8     ) -- TAI time of photom reaching   *
! *                                        the receiver. Units: sec.     *
! *       SPD_2P%COO_EMI ( REAL*8     ) -- Carthezian coordinates of the *
! *                                        emitter. Units: meters.       *
! *       SPD_2P%COO_REC ( REAL*8     ) -- Carthezian coordinates of the *
! *                                        receiver. Units: meters.      *
! *   output fields:                                                     *
! *                                                                      *
! *       SPD_2P%DEL      ( REAL*8     ) -- Path delays for two          *
! *                                         wavelengths. Units: meters.  *
! *       SPD_2P%DEL_RDER ( REAL*8     ) -- Partial derivatives of path  *
! *                                         delays with respect to       *
! *                                         the height of the receiver   *
! *                                         for two wavelegnths.         *
! *                                         Dimesionless.                *
! *       SPD_2P%DEL_EDER ( REAL*8     ) -- Partial derivatives of path  *
! *                                         delays with respect to       *
! *                                         the height of the emitter    *
! *                                         for two wavelegnths.         *
! *                                         Dimesionless.                *
! *   The server reads the input fields and write into the output        *
! *   fields.                                                            *
! *                                                                      *
! *     IUER ( INTEGER*4, OPT ) -- Universal error handler.              *
! *                           Input: switch IUER=0 -- no error messages  *
! *                                  will be generated even in the case  *
! *                                  of error. IUER=-1 -- in the case of *
! *                                  error the message will be put on    *
! *                                  stdout.                             *
! *                           Output: 0 in the case of successful        *
! *                                   completion and non-zero in the     *
! *                                   case of error.                     *
! *                                                                      *
! *                                                                      *
! * ### 22-JAN-2015  SPD_CLI_GET_2PD  v1.0 (c) L. Petrov 22-JAN-2015 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'spd.i'
      INCLUDE   'spd_local.i'
      CHARACTER  SPD_CLI_CONF*128
      INTEGER*4  N_DEL, IVRB, IUER
      TYPE     ( SPD_CLI__TYPE  ) :: SPD_CLI
      TYPE     ( SPD_2P__TYPE   ) :: SPD_2P(N_DEL)
      TYPE     ( SPD_PDRQ__TYPE ) :: PDRQ
      CHARACTER  STR*512, MESSAGE*128
      INTEGER*4  IS, J1, IER
      REAL*8       SPD__READ_TIMEOUT
      PARAMETER  ( SPD__READ_TIMEOUT   =  4.0D0 )
      INTEGER*4, EXTERNAL :: ILEN, I_LEN, SOCK_OPEN_CLIENT, &
     &                       SOCK_READ_POLL, SOCK_WRITE
!
      IF (  SPD_CLI%STATUS .NE. CLI__INIT ) THEN
            WRITE ( 6, '(A,I5,A)' ) 'SPD_CLI object has not been initialized'
            CALL ERR_PASS ( 5441, IUER )
            RETURN 
      END IF
!
! --- Open the socket to the server
!
      IS = SOCK_OPEN_CLIENT ( SPD_CLI%SERVER_NAME(1:I_LEN(SPD_CLI%SERVER_NAME))//CHAR(0), &
     &                        SPD_CLI%SERVER_PORT, SPD_CLI%SOCK_FD, MESSAGE )
      IF ( IS .EQ. -1 ) THEN
           WRITE ( 6, '(A,I5,A)' ) 'Server '//SPD_CLI%SERVER_NAME(1:I_LEN(SPD_CLI%SERVER_NAME))// &
     &                ' on port ',SPD_CLI%SERVER_PORT, &
     &                ' does not respond: '//MESSAGE(1:I_LEN(MESSAGE))
           CALL ERR_PASS ( 1211, IUER )
           RETURN 
      END IF
      SPD_CLI%REM_IP_STR = MESSAGE(14:) ! store the server IP adress
      IF ( IVRB .GE. 1 ) THEN
           WRITE ( 6, '(A)' ) 'spd_cli_get_2pd: connected to '// &
     &                        SPD_CLI%SERVER_NAME(1:I_LEN(SPD_CLI%SERVER_NAME))
      END IF
!
! --- Send command 'get_2pd'
!
      SPD_CLI%SEND_COM%VERB = 'get_2pd '
      SPD_CLI%SEND_COM%LEN  = 0
      IS = SOCK_WRITE ( SPD_CLI%SOCK_FD, SIZEOF(SPD_CLI%SEND_COM), &
     &                  SPD_CLI%SEND_COM, MESSAGE )
      IF ( IS .EQ. -1 ) THEN
           WRITE ( 6, '(A,I5,A)' ) 'Failure in sending get_2pd  request to '// &
     &                 ' server '// &
     &                 SPD_CLI%SERVER_NAME(1:I_LEN(SPD_CLI%SERVER_NAME))// &
     &                 ' : '//MESSAGE(1:I_LEN(MESSAGE))
           CALL ERR_PASS ( 1212, IUER )
           CALL CLOSE ( %VAL(SPD_CLI%SOCK_FD) )
           RETURN 
      END IF
!
! --- Parse the server response
!
      IS = SOCK_READ_POLL ( SPD_CLI%SOCK_FD, SIZEOF(SPD_CLI%RECV_COM), &
     &                      SPD_CLI%RECV_COM, SIZEOF(SPD_CLI%RECV_COM), &
     &                      SPD__READ_TIMEOUT, MESSAGE )
      IF ( IS .EQ. -1 ) THEN
           WRITE ( 6, '(A,I5,A)' ) 'Server '//SPD_CLI%SERVER_NAME(1:I_LEN(SPD_CLI%SERVER_NAME))// &
     &                        ' did not respond to get_2pd  command: '// &
     &                        MESSAGE(1:I_LEN(MESSAGE))
           CALL ERR_PASS ( 1213, IUER )
           CALL CLOSE ( %VAL(SPD_CLI%SOCK_FD) )
           RETURN 
      END IF
      IF ( SPD_CLI%RECV_COM%VERB .EQ. 'error   ' ) THEN
!
! -------- Got response: "error". Now get explanation from the server
! -------- what kind of error has been detected
!
           CALL CLRCH ( STR )
           IS = SOCK_READ_POLL ( SPD_CLI%SOCK_FD, LEN(STR), &
     &                           STR, SPD_CLI%RECV_COM%LEN, &
     &                           SPD__READ_TIMEOUT, MESSAGE )
           WRITE  ( 6, '(A)' ) 'spd_cli_get_2pd: server sent error message: '// &
     &                          STR(1:I_LEN(STR))
!
! -------- Read command bye from the server
!
           IS = SOCK_READ_POLL ( SPD_CLI%SOCK_FD, SIZEOF(SPD_CLI%RECV_COM), &
     &                           SPD_CLI%RECV_COM, SIZEOF(SPD_CLI%RECV_COM), &
     &                           SPD__READ_TIMEOUT, MESSAGE )
           IUER = 1214
           CALL CLOSE ( %VAL(SPD_CLI%SOCK_FD) )
           RETURN 
        ELSE IF ( SPD_CLI%RECV_COM%VERB .NE. 'sendpdrq' ) THEN
!
! -------- Unrecogized server response
!
           WRITE ( 6, '(A)' ) 'Server '//SPD_CLI%SERVER_NAME(1:I_LEN(SPD_CLI%SERVER_NAME))// &
     &             ' sent command '//SPD_CLI%RECV_COM%VERB//' but '// &
     &             'sendpdrq was expected'
           CALL ERR_PASS ( 1215, IUER )
           CALL CLOSE ( %VAL(SPD_CLI%SOCK_FD) )
           RETURN 
      END IF
!
! --- Fille PDRQ data strcuture for the path delay computation request
!
      PDRQ%NUM_POI = N_DEL
      PDRQ%MJD_BEG = SPD_2P(1)%MJD
      PDRQ%TAI_BEG = SPD_2P(1)%TAI
      PDRQ%MJD_END = SPD_2P(N_DEL)%MJD
      PDRQ%TAI_END = SPD_2P(N_DEL)%TAI
!
      IF ( SPD_CLI%SPD_TYPE_1 == SPD__TYPE_W532 ) THEN
            PDRQ%TYP_IND(1) = SPD__W532 
         ELSE IF ( SPD_CLI%SPD_TYPE_1 == SPD__TYPE_W1064 ) THEN
            PDRQ%TYP_IND(1) = SPD__W1064
         ELSE IF ( SPD_CLI%SPD_TYPE_1 == SPD__TYPE_RADIO ) THEN
            PDRQ%TYP_IND(1) = SPD__RADIO
      END IF
!
      IF ( SPD_CLI%SPD_TYPE_2 == SPD__TYPE_W532 ) THEN
            PDRQ%TYP_IND(2) = SPD__W532 
         ELSE IF ( SPD_CLI%SPD_TYPE_2 == SPD__TYPE_W1064 ) THEN
            PDRQ%TYP_IND(2) = SPD__W1064
         ELSE IF ( SPD_CLI%SPD_TYPE_2 == SPD__TYPE_RADIO ) THEN
            PDRQ%TYP_IND(2) = SPD__RADIO
         ELSE IF ( SPD_CLI%SPD_TYPE_2 == SPD__TYPE_NONE ) THEN
            PDRQ%TYP_IND(2) = 0
      END IF
!
! --- Send PDRQ data structure to the server
!
      IS = SOCK_WRITE ( SPD_CLI%SOCK_FD, SIZEOF(PDRQ), PDRQ, MESSAGE )
      IF ( IS .EQ. -1 ) THEN
           WRITE ( 6, '(A,I5,A)' ) 'Failure in sending pdrq header to '// &
     &                 ' server '// &
     &                 SPD_CLI%SERVER_NAME(1:I_LEN(SPD_CLI%SERVER_NAME))// &
     &                 ' : '//MESSAGE(1:I_LEN(MESSAGE))
           CALL ERR_PASS ( 1216, IUER )
           CALL CLOSE ( %VAL(SPD_CLI%SOCK_FD) )
           RETURN 
      END IF
!
! --- Read server's response
!
      IS = SOCK_READ_POLL ( SPD_CLI%SOCK_FD, SIZEOF(SPD_CLI%RECV_COM), &
     &                      SPD_CLI%RECV_COM, SIZEOF(SPD_CLI%RECV_COM), &
     &                      SPD__READ_TIMEOUT, MESSAGE )
      IF ( IS .EQ. -1 ) THEN
           WRITE ( 6, '(A,I5,A)' ) 'Server '// &
     &                  SPD_CLI%SERVER_NAME(1:I_LEN(SPD_CLI%SERVER_NAME))// &
     &                 ' did not respond to get_2pd  command: '// &
     &                  MESSAGE(1:I_LEN(MESSAGE))
           CALL ERR_PASS ( 1217, IUER )
           CALL CLOSE ( %VAL(SPD_CLI%SOCK_FD) )
           RETURN 
      END IF
!
      IF ( SPD_CLI%RECV_COM%VERB .EQ. 'error   ' ) THEN
!
! -------- Got response: "error". Now get explanation from the server
! -------- what kind of error has been detected
!
           CALL CLRCH ( STR )
           IS = SOCK_READ_POLL ( SPD_CLI%SOCK_FD, LEN(STR), &
     &                           STR, SPD_CLI%RECV_COM%LEN, &
     &                           SPD__READ_TIMEOUT, MESSAGE )
           WRITE  ( 6, '(A)' ) 'spd_cli_get_2pd: server sent error message: '// &
     &                          STR(1:I_LEN(STR))
!
! -------- Read bye command
!
           IS = SOCK_READ_POLL ( SPD_CLI%SOCK_FD, SIZEOF(SPD_CLI%RECV_COM), &
     &                           SPD_CLI%RECV_COM, SIZEOF(SPD_CLI%RECV_COM), &
     &                          SPD__READ_TIMEOUT, MESSAGE )
           IUER = 1218
           CALL CLOSE ( %VAL(SPD_CLI%SOCK_FD) )
           RETURN 
        ELSE IF ( SPD_CLI%RECV_COM%VERB .NE. 'sendpdtb' ) THEN
           WRITE ( 6, '(A)' ) 'Server '//SPD_CLI%SERVER_NAME(1:I_LEN(SPD_CLI%SERVER_NAME))// &
     &             ' sent command '//SPD_CLI%RECV_COM%VERB//' but '// &
     &             'sendpdtb was expected'
           CALL ERR_PASS ( 1219, IUER )
           CALL CLOSE ( %VAL(SPD_CLI%SOCK_FD) )
           RETURN 
      END IF
!
! --- Now send the entire SPD_2P to the server
!
      IS = SOCK_WRITE ( SPD_CLI%SOCK_FD, PDRQ%NUM_POI*SIZEOF(SPD_2P(1)), &
     &                  SPD_2P, MESSAGE )
      IF ( IS .EQ. -1 ) THEN
           WRITE ( 6, '(A,I5,A)' ) 'Failure in sending spd_2p table to '// &
     &                 ' server '// &
     &                 SPD_CLI%SERVER_NAME(1:I_LEN(SPD_CLI%SERVER_NAME))// &
     &                 ' : '//MESSAGE(1:I_LEN(MESSAGE))
           CALL ERR_PASS ( 1220, IUER )
           CALL CLOSE ( %VAL(SPD_CLI%SOCK_FD) )
           RETURN 
      END IF
      SPD_CLI%REM_IP_STR = MESSAGE(14:)
      IF ( IVRB .GE. 1 ) THEN
           WRITE ( 6, '(A)' ) 'spd_cli_get_2pd: sent path delay request to '// &
     &                        SPD_CLI%SERVER_NAME(1:I_LEN(SPD_CLI%SERVER_NAME))
      END IF
!
! --- Waits for the server response in a cycle. Server is supposed to send ticks to 
! --- demonstrate that it is dilligently working on request processing
!
      DO 410 J1=1,1024*1024
!
! ------ Read the server response
!
         IS = SOCK_READ_POLL ( SPD_CLI%SOCK_FD, SIZEOF(SPD_CLI%RECV_COM), &
     &                         SPD_CLI%RECV_COM, SIZEOF(SPD_CLI%RECV_COM), &
     &                         SPD__READ_TIMEOUT, MESSAGE )
         IF ( IS .EQ. -1 ) THEN
              WRITE ( 6, '(A,I5,A)' ) 'Server '// &
     &                SPD_CLI%SERVER_NAME(1:I_LEN(SPD_CLI%SERVER_NAME))// &
     &                ' Error in reading command: '// &
     &                MESSAGE(1:I_LEN(MESSAGE))
              CALL ERR_PASS ( 1221, IUER )
              CALL CLOSE ( %VAL(SPD_CLI%SOCK_FD) )
              RETURN 
         END IF
         IF ( SPD_CLI%RECV_COM%VERB == 'wait_1s ' ) THEN
!
! ----------- Server told us: wait a second.
!
              IF ( IVRB .GE. 2 ) THEN
                   WRITE  ( 6, 110 ) J1, CHAR(13)
                   CALL FLUSH ( 6 )
 110               FORMAT ( '  spd_cli_get_2pd: wait for ', I4, ' sec ',A$ )
              END IF
!
! ----------- ... well, let us patiently wait
!
              GOTO 410
            ELSE IF ( SPD_CLI%RECV_COM%VERB == 'error   ' ) THEN
!
! ------------ Server said "error". Now get explanation from the server
! ------------ what kind of error has been detected
!
              CALL CLRCH ( STR )
              IS = SOCK_READ_POLL ( SPD_CLI%SOCK_FD, LEN(STR), &
     &                              STR, SPD_CLI%RECV_COM%LEN, &
     &                              SPD__READ_TIMEOUT, MESSAGE )
              WRITE  ( 6, '(A)' ) 'spd_cli_get_2pd: server sent error message: '// &
     &                            STR(1:I_LEN(STR))
!
! ----------- Read bye command
!
              IS = SOCK_READ_POLL ( SPD_CLI%SOCK_FD, SIZEOF(SPD_CLI%RECV_COM), &
     &                       SPD_CLI%RECV_COM, SIZEOF(SPD_CLI%RECV_COM), &
     &                       SPD__READ_TIMEOUT, MESSAGE )
              IUER = 1222
              CALL CLOSE ( %VAL(SPD_CLI%SOCK_FD) )
              RETURN 
            ELSE IF ( SPD_CLI%RECV_COM%VERB == 'recvpdtb' ) THEN
!
! ----------- Uuu! Finnaly, server said it has computed path delays and asks
! ----------- us kiondly to get the results.
!
              GOTO 810
            ELSE
!
! ----------- Unrecoginzed server respons
!
              WRITE ( 6, '(A,I5,A)' ) 'Server '// &
     &                SPD_CLI%SERVER_NAME(1:I_LEN(SPD_CLI%SERVER_NAME))// &
     &                ' receved unexpeced command '//SPD_CLI%RECV_COM%VERB 
              CALL ERR_PASS ( 1223, IUER )
              CALL CLOSE ( %VAL(SPD_CLI%SOCK_FD) )
              RETURN 
         END IF
 410  CONTINUE 
 810  CONTINUE 
!
! --- Read SPD_2P data structure with results
!
      IS = SOCK_READ_POLL ( SPD_CLI%SOCK_FD, SPD_CLI%RECV_COM%LEN, &
     &                      SPD_2P, SPD_CLI%RECV_COM%LEN, &
     &                     SPD__READ_TIMEOUT, MESSAGE )
      IF ( IS .NE. SPD_CLI%RECV_COM%LEN ) THEN
!
! -------- Error was detected
!
           WRITE ( 6, '(A,I5,A)' ) 'Server '// &
     &             SPD_CLI%SERVER_NAME(1:I_LEN(SPD_CLI%SERVER_NAME))// &
     &             ' error in reading command:  recvpdtb'// &
     &              MESSAGE(1:I_LEN(MESSAGE))
           CALL ERR_PASS ( 1224, IUER )
           CALL CLOSE ( %VAL(SPD_CLI%SOCK_FD) )
           RETURN 
      END IF
!
      IF ( IVRB .GE. 1 ) THEN
           WRITE  ( 6, '(A)' ) 'spd_cli_get_2pd: received path delays'
      END IF
!
! --- Send the server acknowledgment with many thanks
!
      SPD_CLI%SEND_COM%VERB = 'ack     '
      SPD_CLI%SEND_COM%LEN  = 0
      IS = SOCK_WRITE ( SPD_CLI%SOCK_FD, SIZEOF(SPD_CLI%SEND_COM), &
     &                  SPD_CLI%SEND_COM, MESSAGE )
      IF ( IS .EQ. -1 ) THEN
           WRITE ( 6, '(A,I5,A)' ) 'Failure in sending ack request to '// &
     &                 ' server '// &
     &                 SPD_CLI%SERVER_NAME(1:I_LEN(SPD_CLI%SERVER_NAME))// &
     &                 ' : '//MESSAGE(1:I_LEN(MESSAGE))
           CALL ERR_PASS ( 1225, IUER )
           CALL CLOSE ( %VAL(SPD_CLI%SOCK_FD) )
           RETURN 
      END IF
!
! --- Read command bye 
!
      IS = SOCK_READ_POLL ( SPD_CLI%SOCK_FD, SIZEOF(SPD_CLI%RECV_COM), &
     &                      SPD_CLI%RECV_COM, SIZEOF(SPD_CLI%RECV_COM), &
     &                      SPD__READ_TIMEOUT, MESSAGE )
      IF ( IS .EQ. -1 ) THEN
           WRITE ( 6, '(A,I5,A)' ) 'Server '// &
     &                  SPD_CLI%SERVER_NAME(1:I_LEN(SPD_CLI%SERVER_NAME))// &
     &                 ' did not respond to ack command: '// &
     &                  MESSAGE(1:I_LEN(MESSAGE))
           CALL ERR_PASS ( 1226, IUER )
           CALL CLOSE ( %VAL(SPD_CLI%SOCK_FD) )
           RETURN 
      END IF
!
! --- Close the socket
!
      CALL CLOSE ( %VAL(SPD_CLI%SOCK_FD) )
      CALL ERR_LOG ( 0, IUER )
      IF ( IVRB .GE. 3 ) write ( 6, * ) 'SPD_CLI_GET_2PD-ZZ IUER= ', IUER ; call flush ( 6 ) ! %%%%%%%%%%%%%%%
      RETURN
      END  SUBROUTINE  SPD_CLI_GET_2PD  !#!#  
!
! ------------------------------------------------------------------------
!
      FUNCTION   SPD_CLI_LEN ( )
! ************************************************************************
! *                                                                      *
! *   Auxilliary routine SPD_CLI_LEN retursn the length of data          *
! *   structure SPD_CLI_LEN in bytes.                                    *
! *                                                                      *
! *  ### 24-MAR-2015  SPD_CLI_LEN  v1.0 (c)  L. Petrov  24-MAR-2015 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'spd.i'
      INCLUDE   'spd_local.i'
      TYPE     ( SPD_CLI__TYPE ) :: SPD_CLI
      INTEGER*4  SPD_CLI_LEN 
!
      SPD_CLI_LEN = SIZEOF ( SPD_CLI )
      RETURN
      END  FUNCTION  SPD_CLI_LEN  !#!  
!
! ------------------------------------------------------------------------
!
      FUNCTION   GET_SPC_SHARE ( )
      INCLUDE   'spd_local.i'
      CHARACTER  GET_SPC_SHARE*(SIZEOF(SPD__SHARE))
      GET_SPC_SHARE = SPD__SHARE
      RETURN
      END  !#!  
!
! ------------------------------------------------------------------------
!
      SUBROUTINE GET_SPD_CLI_LIB_PATH ( SPD_CLI, SPD_CLI_LIB_PATH )
! ************************************************************************
! *                                                                      *
! *   Auxilliary routine SPD_GET_CLI_LIB returns
! *   structure SPD_CLI_LEN in bytes.                                    *
! *                                                                      *
! * ### 24-MAR-2015  SPD_GET_CLI_LIB v1.0 (c) L. Petrov  24-MAR-2015 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'spd.i'
      INCLUDE   'spd_local.i'
      TYPE     ( SPD_CLI__TYPE ) :: SPD_CLI
      CHARACTER  SPD_CLI_LIB_PATH*(*)
!
      SPD_CLI_LIB_PATH = SPD_CLI%SPD_PATH
      RETURN
      END  SUBROUTINE GET_SPD_CLI_LIB_PATH  !#!#
