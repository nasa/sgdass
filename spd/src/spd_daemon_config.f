#include <mk5_preprocessor_directives.inc>
      SUBROUTINE SPD_DAEMON_CONFIG ( SPD, CONF_FILE, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  SPD_DAEMON_CONFIG 
! *                                                                      *
! * ## 06-JAN-2015 SPD_DAEMON_CONFIG v1.0 (c) L. Petrov  06-JAN-2015 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'spd.i'
      TYPE     ( SPD_3D__TYPE ) :: SPD
      CHARACTER  CONF_FILE*(*)
      INTEGER*4  IUER
      INTEGER*4  MBUF, MIND
      PARAMETER  ( MBUF = 128 )
      PARAMETER  ( MIND =   8 )
      CHARACTER  BUF(MBUF)*128, STR*128, STR1*128, STR_IP*128, REG*3
      LOGICAL*1  LEX
      PARAMETER  ( REG = CHAR(32)//CHAR(0)//CHAR(9) )
      INTEGER*8  DIR_DESC
      INTEGER*4  J1, J2, J3, N_CNF, IB, IE, NBUF, LIND, IND(2,MIND), &
     &           IER
      INTEGER*4, EXTERNAL :: ILEN, I_LEN, MULTI_INDEX
      ADDRESS__TYPE, EXTERNAL :: FUNC_OPENDIR, FUNC_READDIR
!
! --- Read the configuration file
!
      CALL ERR_PASS ( IUER, IER ) 
      CALL RD_TEXT ( CONF_FILE, MBUF, BUF, NBUF, IER ) 
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 5471, IUER, 'SPD_DAEMON_CONFIG', 'Error in an attempt '// &
     &         'to read configuration file '//CONF_FILE )
           RETURN 
      END IF
!
! --- Check the configuration file label
!
      IF ( BUF(1)(1:LEN(SPD__DAEMON_CONF__LABEL)) ==  SPD__DAEMON_CONF__LABEL) THEN
           CONTINUE 
         ELSE 
           CALL CLRCH ( STR )
           CALL TRAN  ( 13, BUF(1), STR )
           CALL ERR_LOG ( 5472, IUER, 'SPD_DAEMON_CONFIG', 'Wrong format label '// &
     &         'at the first line of the configuration file '// &
     &          CONF_FILE(1:I_LEN(CONF_FILE))//' -- '//STR(1:I_LEN(STR))// &
     &         ' while '//SPD__DAEMON_CONF__LABEL//' expected' )
           RETURN 
      END IF
!
! --- Cycle over lines of the file
!
      N_CNF = 0
      SPD%CONF%NUM_IP = 0 
      DO 410 J1=2,NBUF
!
! ------ Split the line into words
!
         IF ( BUF(J1)(1:1)  == '#' ) GOTO 410
         IF ( ILEN(BUF(J1)) ==  0  ) GOTO 410
         CALL EXWORD ( BUF(J1), MIND, LIND, IND, REG, -2 )
         CALL CLRCH  ( STR )
         CALL INCH   ( J1, STR )
         IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'DAEMON_PID_FILE:' ) THEN
              SPD%CONF%DAEMON_PID_FILE = BUF(J1)(IND(1,2):IND(2,2))
              N_CNF = N_CNF + 1
           ELSE IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'SERVER_PID_FILE:' ) THEN
              SPD%CONF%SERVER_PID_FILE = BUF(J1)(IND(1,2):IND(2,2))
              N_CNF = N_CNF + 1
           ELSE IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'DAEMON_LOG_FILE:' ) THEN
              SPD%CONF%DAEMON_LOG_FILE = BUF(J1)(IND(1,2):IND(2,2))
              N_CNF = N_CNF + 1
           ELSE IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'SERVER_LOG_FILE:' ) THEN
              SPD%CONF%SERVER_LOG_FILE = BUF(J1)(IND(1,2):IND(2,2))
              N_CNF = N_CNF + 1
           ELSE IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'RESP_DIR:' ) THEN
              SPD%CONF%RESP_DIR = BUF(J1)(IND(1,2):IND(2,2))
              N_CNF = N_CNF + 1
           ELSE IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'SERVER_EXEC:' ) THEN
              SPD%CONF%SERVER_SERVER_EXE = BUF(J1)(IND(1,2):IND(2,2))
              N_CNF = N_CNF + 1
          ELSE IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'SERVER_PORT:' ) THEN
              READ ( UNIT=BUF(J1)(IND(1,2):IND(2,2)), FMT='(I5)' ) SPD%CONF%SERVER_PORT 
              N_CNF = N_CNF + 1
           ELSE IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'MAX_NUM_PROC:' ) THEN
              READ ( UNIT=BUF(J1)(IND(1,2):IND(2,2)), FMT='(I5)' ) SPD%CONF%MAX_NUM_PROC
              N_CNF = N_CNF + 1
           ELSE IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'IP_ALLOW:' ) THEN
              IF ( SPD%CONF%NUM_IP == 0 ) THEN
                   N_CNF = N_CNF + 1
              END IF
              SPD%CONF%NUM_IP = SPD%CONF%NUM_IP + 1
              SPD%CONF%IP_ALLOW(SPD%CONF%NUM_IP) = BUF(J1)(IND(1,2):IND(2,2))
         END IF
 410  CONTINUE 
!
! --- Check whether all keywords were present in the configuration file
!
      IF ( N_CNF .NE. M__SPD_DAE ) THEN
           CALL CLRCH ( STR  )

           CALL CLRCH ( STR1 )
           CALL INCH  ( N_CNF, STR )
           CALL INCH  ( M__SPD_DAE, STR1 )
           CALL ERR_LOG ( 5473, IUER, 'SPD_DAEMON_CONFIG', 'Only '// &
     &          STR(1:I_LEN(STR))//' keywords out of '//STR1(1:I_LEN(STR1))// &
     &          ' were found in the configuration file '//CONF_FILE )
           RETURN 
      END IF
!
      DIR_DESC = FUNC_OPENDIR ( SPD%CONF%RESP_DIR(1:I_LEN(SPD%CONF%RESP_DIR))//CHAR(0) )
      IF ( DIR_DESC .EQ. 0 ) THEN
           CALL GERROR  ( STR )
           CALL ERR_LOG ( 5474, IUER, 'SPD_DAEMON_CONFIG', 'RESP_DIR directory '// &
     &          SPD%CONF%RESP_DIR(1:I_LEN(SPD%CONF%RESP_DIR))// &
     &          ' does not exist' )
           RETURN
        ELSE
           CALL CLOSEDIR ( %VAL(DIR_DESC) )
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  SPD_DAEMON_CONFIG  !#!  
