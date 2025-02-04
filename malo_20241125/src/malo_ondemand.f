      PROGRAM    MALO_ONDEMAND
! ************************************************************************
! *                                                                      *
! *   Pprogram  MALO_ONDEMAND
! *                                                                      *
! * ### 08-MAY-2013  MALO_ONDEMAND  v1.2 (c)  L. Petrov 17-NOV-2023 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'malo.i'
      TYPE     ( MALO_OND__TYPE ) :: OND
      CHARACTER  CONF_FILE*128, DIR_OND*128, QUE_NAM*128, LOG_NAM*128, &
     &           QUE_FIL*128, LOG_FIL*128, STR*128
      INTEGER*4  OND_SLEEP, NP, J1, IK, OND_PID, IVRB, IUER
      LOGICAL*1  LEX
      PARAMETER  ( OND_SLEEP = 5 )
      CHARACTER, EXTERNAL :: GET_CDATE*19
      INTEGER*4, EXTERNAL :: ILEN, I_LEN, LINDEX, CLOSEDIR, GETPID, KILL
!
      IF ( IARGC() < 1 ) THEN
           WRITE ( 6, '(A)' ) 'Usage: malo_ondemand config_file [verbosity]' 
           CALL EXIT ( 1 )
         ELSE 
           CALL GETARG ( 1, CONF_FILE )
           IF ( IARGC() .GE. 2 ) THEN
                CALL GETARG ( 2, STR )
                CALL CHIN   ( STR, IVRB )
              ELSE 
                IVRB = 0
           END IF
      END IF
!
! --- Set signal handler for ignoring zombies
!
      CALL SET_SIGCLD ()
!
      IUER = 1
      CALL MALO_PARSE_CONF_OND ( CONF_FILE, OND, IUER )
      IF ( IUER .NE. 0 ) THEN
           CALL ERR_LOG ( 7201, IUER, 'MALO_ONDEMAND', 'Failure in '// &
     &         'attempt to parse input configuration file '//CONF_FILE )
           CALL EXIT ( 1 )
      END IF
!
! --- Check lock-file process
!
      INQUIRE ( FILE=OND%OND_LOCK_FILE, EXIST=LEX )
      IF ( LEX ) THEN
           IUER = 0
           CALL RD_TEXT ( OND%OND_LOCK_FILE, 1, STR, NP, IUER )
           IF ( IUER .NE. 0 ) THEN
                CALL ERR_LOG ( 7202, IUER, 'MALO_ONDEMAND', 'Failure in '// &
     &              'attempt to read lock file '//OND%OND_LOCK_FILE )
                CALL EXIT ( 1 )
           END IF
!           
           CALL CHIN ( STR(21:28), OND_PID )
           IK = KILL ( %VAL(OND_PID), %VAL(0) )
           IF ( IK == 0 ) THEN
                WRITE ( 6, '(A)' ) 'Another malo_ondemand process is running '// &
     &                             'with PID '//STR(21:28)
                CALL EXIT ( 0 )
           END IF
      END IF
!
! --- Write the current PID in the lock-file
!
      WRITE ( UNIT=STR(1:28), FMT='(A,1X,I8)' ) GET_CDATE(), GETPID() 
!
      IUER = -1
      CALL WR_TEXT ( 1, STR, OND%OND_LOCK_FILE, IUER )
      IF ( IUER .NE. 0 ) THEN
           CALL ERR_LOG ( 7203, IUER, 'MALO_ONDEMAND', 'Failure in '// &
     &         'an attempt to write malo_ondemand lock file '// &
     &          OND%OND_LOCK_FILE )
           CALL EXIT ( 1 )
      END IF
!
      DO 410 J1=1,1024*1024*1024
         IUER = 1
         CALL MALO_OND_PROC ( OND, IVRB, IUER )
         IF ( IUER .NE. 0 ) THEN
              CALL ERR_LOG ( 7204, IUER, 'MALO_ONDEMAND', 'Trap of internal '// &
     &            'control. MALO_ONDEMAND abnoramlly terminates :-(' )
              CALL UNLINK ( OND%OND_LOCK_FILE(1:I_LEN(OND%OND_LOCK_FILE))//CHAR(0) )
              CALL EXIT ( 1 )
         END IF
         IF ( IVRB .GE. 3 ) THEN
              WRITE ( 6, '(A)' ) GET_CDATE()//' -- go to sleep'
         END IF
         CALL SLEEP ( OND_SLEEP )
 410  CONTINUE 
      END  PROGRAM   MALO_ONDEMAND  !#!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE MALO_PARSE_CONF_OND  ( CONF_FILE, OND, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine MALO_PARSE_CONF_OND 
! *                                                                      *
! * ## 08-MAY-2013 MALO_PARSE_CONF_OND v1.2 (c) L. Petrov 11-NOV-2021 ## *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'malo.i'
      INCLUDE   'malo_local.i'
      TYPE     ( MALO_OND__TYPE ) :: OND
      CHARACTER  CONF_FILE*128
      INTEGER*4  IUER
      LOGICAL*1  LEX
      INTEGER*4  MIND, MBUF
      PARAMETER  ( MIND = 32 )
      PARAMETER  ( MBUF = 128 )
      CHARACTER  BUF(MBUF)*128, STR*128, STR1*128, REG*3
      PARAMETER  ( REG = CHAR(0)//CHAR(32)//CHAR(9) )
      INTEGER*8  DIR_DESC 
      INTEGER*4  J1, IP, IND(2,MIND), N_OND, LIND, NBUF, IER
      INTEGER*4, EXTERNAL :: ILEN, I_LEN, LINDEX, CLOSEDIR 
      INTEGER*8, EXTERNAL :: OPENDIR
!
      N_OND = 0
      INQUIRE ( FILE=CONF_FILE, EXIST=LEX )
      IF ( .NOT. LEX ) THEN
           CALL ERR_LOG ( 7211, IUER, 'MALO_PARSE_CONF_OND', 'Configuration '// &
     &          CONF_FILE(1:I_LEN(CONF_FILE))//' was not found' )
           RETURN 
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL RD_TEXT  ( CONF_FILE, MBUF, BUF, NBUF, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 7212, IUER, 'MALO_PARSE_CONF_OND', 'Failure '// &
     &         'in reading configuration file '//CONF_FILE )
           RETURN 
      END IF
!
      IF ( BUF(1)(1:LEN(OND__LABEL)) == OND__LABEL ) THEN
           CONTINUE 
         ELSE
           CALL CLRCH ( STR )
           CALL TRAN ( 13, BUF(1), STR )
           CALL ERR_LOG ( 7213, IUER, 'MALO_PARSE_CONF_OND', 'Wrong format '// &
     &         'of MALO ondemand configuration file '// &
     &          CONF_FILE(1:I_LEN(CONF_FILE))//' -- the first line is '// &
     &          STR(1:I_LEN(STR))//' while label '//OND__LABEL// &
     &          ' was expected' )
           RETURN 
      END IF
!
      DO 410 J1=2,NBUF
         IF ( BUF(J1)(1:1)  == '#' ) GOTO 410
         IF ( ILEN(BUF(J1)) ==  0  ) GOTO 410
         CALL EXWORD ( BUF(J1), MIND, LIND, IND, REG, IER )
         IF ( LIND .NE. 2 ) THEN
              CALL CLRCH ( STR )
              CALL INCH ( J1, STR )
              CALL ERR_LOG ( 7214, IUER, 'MALO_PARSE_CONF_OND', 'Wrong '// &
     &            'number of words in line '//STR(1:I_LEN(STR))// &
     &            ' of MALO ondemand configuration file '// &
     &            CONF_FILE(1:I_LEN(CONF_FILE))//' -- should be two words' )
              RETURN 
         END IF
         IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'HOST_NAME:'          ) THEN
              OND%HOST_NAME = BUF(J1)(IND(1,2):IND(2,2)) 
              N_OND = N_OND + 1
            ELSE IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'SERVER_GROUP:'    ) THEN
              OND%SERVER_GROUP = BUF(J1)(IND(1,2):IND(2,2)) 
              N_OND = N_OND + 1
            ELSE IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'OND_DIR:'    ) THEN
              OND%OND_DIR = BUF(J1)(IND(1,2):IND(2,2)) 
              DIR_DESC = OPENDIR ( OND%OND_DIR(1:I_LEN(OND%OND_DIR))//CHAR(0) ) 
              IF ( DIR_DESC .LE. 0 ) THEN
                   CALL GERROR ( STR )
                   CALL ERR_LOG ( 7215, IUER, 'MALO_PARSE_CONF_OND', &
     &                 'OND_DIR directory '// &
     &                  OND%OND_DIR(1:I_LEN(OND%OND_DIR))// &
     &                  ' specified in the MALO ondemand control file '// &
     &                  CONF_FILE(1:I_LEN(CONF_FILE))//' '//STR )
                   RETURN 
                 ELSE 
                   IP = CLOSEDIR ( %VAL(DIR_DESC) )
              END IF
              N_OND = N_OND + 1
            ELSE IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'REQ_DIR:'    ) THEN
              OND%REQ_DIR = BUF(J1)(IND(1,2):IND(2,2)) 
              DIR_DESC = OPENDIR ( OND%REQ_DIR(1:I_LEN(OND%REQ_DIR))//CHAR(0) ) 
              IF ( DIR_DESC .LE. 0 ) THEN
                   CALL GERROR ( STR )
                   CALL ERR_LOG ( 7216, IUER, 'MALO_PARSE_CONF_OND', &
     &                 'REQ_DIR directory '// &
     &                  OND%REQ_DIR(1:I_LEN(OND%REQ_DIR))// &
     &                  ' specified in the MALO ondemand control file '// &
     &                  CONF_FILE(1:I_LEN(CONF_FILE))//' '//STR )
                   RETURN 
                 ELSE 
                   IP = CLOSEDIR ( %VAL(DIR_DESC) )
              END IF
              N_OND = N_OND + 1
            ELSE IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'REQ_HTML_DIR:' ) THEN
              OND%REQ_HTML_DIR = BUF(J1)(IND(1,2):IND(2,2)) 
              N_OND = N_OND + 1
            ELSE IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'FROM_EMAIL:'   ) THEN
              OND%FROM_EMAIL = BUF(J1)(IND(1,2):IND(2,2)) 
              N_OND = N_OND + 1
            ELSE IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'MALO_ROOT:'    ) THEN
              OND%MALO_ROOT = BUF(J1)(IND(1,2):IND(2,2)) 
              IF ( OND%MALO_ROOT == 'NONE' .or. OND%MALO_ROOT == 'local' ) THEN
                   OND%MALO_ROOT = MALO_SCRIPT(1:LEN(MALO_SCRIPT)-6)
              END IF
              DIR_DESC = OPENDIR ( OND%MALO_ROOT(1:I_LEN(OND%MALO_ROOT))//CHAR(0) ) 
              IF ( DIR_DESC .LE. 0 ) THEN
                   CALL GERROR ( STR )
                   CALL ERR_LOG ( 7217, IUER, 'MALO_PARSE_CONF_OND', &
     &                 'MALO_ROOT directory '// &
     &                  OND%MALO_ROOT(1:I_LEN(OND%MALO_ROOT))// &
     &                  ' specified in the MALO ondemand control file '// &
     &                  CONF_FILE(1:I_LEN(CONF_FILE))//' '//STR )
                   RETURN 
                 ELSE 
                   IP = CLOSEDIR ( %VAL(DIR_DESC) )
              END IF
              N_OND = N_OND + 1
            ELSE IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'QUEUE_FILE:' ) THEN
              OND%QUEUE_FILE = BUF(J1)(IND(1,2):IND(2,2))
              INQUIRE ( FILE=OND%QUEUE_FILE, EXIST=LEX )
              IF ( .NOT. LEX ) THEN
                   CALL  ERR_LOG ( 7218, IUER, 'MALO_PARSE_CONF_OND', &
     &                  'QUEUE_FILE '//OND%QUEUE_FILE(1:I_LEN(OND%QUEUE_FILE))// &
     &                  ' specified in the MALO ondemand control file '// &
     &                  CONF_FILE(1:I_LEN(CONF_FILE))//' was not found' )
                  RETURN 
              END IF
              N_OND = N_OND + 1
            ELSE IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'QUEUE_LOCK_FILE:' ) THEN
              OND%QUEUE_LOCK_FILE = BUF(J1)(IND(1,2):IND(2,2))
              N_OND = N_OND + 1
            ELSE IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'OND_LOCK_FILE:' ) THEN
              OND%OND_LOCK_FILE = BUF(J1)(IND(1,2):IND(2,2))
              N_OND = N_OND + 1
            ELSE IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'LOG_FILE:' ) THEN
              OND%LOG_FILE = BUF(J1)(IND(1,2):IND(2,2))
              INQUIRE ( FILE=OND%LOG_FILE, EXIST=LEX )
              IF ( .NOT. LEX ) THEN
                   CALL  ERR_LOG ( 7219, IUER, 'MALO_PARSE_CONF_OND', &
     &                  'LOG_FILE '//OND%LOG_FILE(1:I_LEN(OND%LOG_FILE))// &
     &                  ' specified in the MALO ondemand control file '// &
     &                  CONF_FILE(1:I_LEN(CONF_FILE))//' was not found' )
                  RETURN 
              END IF
              N_OND = N_OND + 1
            ELSE IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'MAIL_BIN:' ) THEN
              OND%MAIL_BIN = BUF(J1)(IND(1,2):IND(2,2))
              INQUIRE ( FILE=OND%MAIL_BIN, EXIST=LEX )
              IF ( .NOT. LEX ) THEN
                   CALL  ERR_LOG ( 7220, IUER, 'MALO_PARSE_CONF_OND', &
     &                  'MAIL_BIN '//OND%MAIL_BIN(1:I_LEN(OND%MAIL_BIN))// &
     &                  ' specified in the MALO ondemand control file '// &
     &                  CONF_FILE(1:I_LEN(CONF_FILE))//' was not found' )
                  RETURN 
              END IF
              N_OND = N_OND + 1
            ELSE IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'MAX_PROC:'   ) THEN
              CALL CHIN ( BUF(J1)(IND(1,2):IND(2,2)), OND%MAX_PROC ) 
              IF ( OND%MAX_PROC < 1 .OR. OND%MAX_PROC > 128 ) THEN
                   CALL  ERR_LOG ( 7221, IUER, 'MALO_PARSE_CONF_OND', &
     &                  'Parameter MAX_PROC: '//BUF(J1)(IND(1,2):IND(2,2))// &
     &                  ' specified in the MALO ondemand control file '// &
     &                  CONF_FILE(1:I_LEN(CONF_FILE))//' was not found' )
                   RETURN 
              END IF 
              N_OND = N_OND + 1
            ELSE 
              CALL CLRCH ( STR )
              CALL INCH  ( J1, STR )
              CALL ERR_LOG ( 7222, IUER, 'MALO_PARSE_CONF_OND', 'Unsupported '// &
     &            'keyword in line '//STR(1:I_LEN(STR))// &
     &            ' of MALO ondemand configuration file '// &
     &            CONF_FILE(1:I_LEN(CONF_FILE))//' -- '// &
     &            BUF(J1)(IND(1,1):IND(2,1)) )
              RETURN 
         END IF
 410  CONTINUE 
      CALL GETARG ( 0, STR )
      OND%MALO_BIN = STR(1:ILEN(STR)-14)
!
      IF ( N_OND < M__OND ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( N_OND, STR )
           CALL CLRCH ( STR1 )
           CALL INCH  ( M__OND, STR1 )
           CALL ERR_LOG ( 7223, IUER, 'MALO_PARSE_CONF_OND', 'Not all '// &
     &         'keywords were found in MALO ondemand configuration file '// &
     &          CONF_FILE(1:I_LEN(CONF_FILE))//' -- '//STR(1:I_LEN(STR))// &
     &          ' out if '//STR1 )
           RETURN 
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN 
      END  SUBROUTINE  MALO_PARSE_CONF_OND  !#!#
