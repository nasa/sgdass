      SUBROUTINE MALO_OND_PROC ( OND, IVRB, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine MALO_OND_PROC performs
! *                                                                      *
! * ### 08-MAY-2013  MALO_OND_PROC v1.8 (c)  L. Petrov  17-NOV-2023 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'malo.i'
      TYPE     ( MALO_OND__TYPE ) :: OND
      INTEGER*4  IVRB, IUER
      INTEGER*4  MBUF
      PARAMETER  ( MBUF = 128*1024 )
      CHARACTER  BUF(MBUF)*256, STS*1, STR*128
      TYPE     ( MALO_QUE__TYPE ) :: QUE(MBUF)
      LOGICAL*1  LEX, FL_DUP
      INTEGER*4  NB, NQ, IS, NP, J1, J2, J3, J4, IER
      INTEGER*4, EXTERNAL :: ILEN, I_LEN, CLOSEDIR, LINDEX, UNLINK
!
! --- Check lock file, if exists check it every 0.01 sec for 32.0 seconds.
! --- If does not exist then create the file and write there 
! --- current date and PID
!
      CALL ERR_PASS ( IUER, IER )
      CALL SET_FILE_LOCK ( OND%QUEUE_LOCK_FILE, 0.01D0, 32.0D0, IER )
      IF ( IER .NE. 0 ) THEN
!@           CALL ERR_LOG ( 7221, IUER, 'MALO_OND_PROC', 'Failure to '// &
!@     &         'set lock file for the request queue' )
!@           RETURN 
           INQUIRE ( FILE=OND%QUEUE_LOCK_FILE, EXIST=LEX )
           IF ( LEX ) THEN
                IS = UNLINK ( OND%QUEUE_LOCK_FILE(1:I_LEN(OND%QUEUE_LOCK_FILE))//CHAR(0) )
           END IF
!
           CALL ERR_PASS ( IUER, IER )
           CALL SET_FILE_LOCK ( OND%QUEUE_LOCK_FILE, 0.01D0, 32.0D0, IER )
      END IF
!
! --- Set appropriate permissions for the queue lock file
!
      INQUIRE ( FILE=OND%QUEUE_LOCK_FILE, EXIST=LEX )
      IF ( LEX ) THEN
           CALL SYSTEM ( 'chmod g+wr,o+wr '//TRIM(OND%QUEUE_LOCK_FILE)//CHAR(0) )
           CALL SYSTEM ( 'chgrp '//TRIM(OND%SERVER_GROUP)//' '//TRIM(OND%QUEUE_LOCK_FILE)//CHAR(0) )
      END IF
!
! --- Read queue file
!
      CALL ERR_PASS ( IUER, IER )
      CALL RD_TEXT ( OND%QUEUE_FILE, MBUF, BUF, NB, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 7221, IUER, 'MALO_OND_PROC', 'Failure to '// &
     &         'read queue file '//OND%QUEUE_FILE )
           RETURN 
      END IF
      IS = UNLINK ( OND%QUEUE_LOCK_FILE(1:I_LEN(OND%QUEUE_LOCK_FILE))//CHAR(0) )
!
! --- Parse queue file. Create array of QUE objects that containt "live"
! --- queue element, i.e. process that is not finished
!
      NQ = 0
      DO 410 J1=1,NB
         IF ( BUF(J1)(1:1) == '#' ) GOTO 410
         IF ( BUF(J1)(155:155) == 'U' .OR. &
     &        BUF(J1)(155:155) == 'W' .OR. &
     &        BUF(J1)(155:155) == 'R'      ) THEN
!
! ----------- This request is still active
!
              NQ = NQ + 1
              QUE(NQ)%DATE_ORIG  = BUF(J1)(1:15) 
              QUE(NQ)%MODEL      = BUF(J1)(17:36) 
              QUE(NQ)%MODE       = BUF(J1)(38:49) 
              QUE(NQ)%FRAME      = BUF(J1)(51:52) 
              CALL CHIN ( BUF(J1)(54:58), QUE(NQ)%N_STA )
              QUE(NQ)%START_DATE = BUF(J1)(60:78) 
              QUE(NQ)%STOP_DATE  = BUF(J1)(80:98) 
              QUE(NQ)%IP_ADDR    = BUF(J1)(100:114) 
              CALL CHASHL ( QUE(NQ)%IP_ADDR )
              QUE(NQ)%EMAIL      = BUF(J1)(116:153) 
              QUE(NQ)%STAT       = BUF(J1)(155:155) 
              IF ( BUF(J1)(157:157) .NE. ' ' ) THEN
                   CALL CHIN ( BUF(J1)(157:164), QUE(NQ)%PID )
              END IF 
         END IF
 410  CONTINUE 
      IF ( IVRB .GE. 4 ) THEN
           CALL CLRCH ( STR )
           IF ( NQ .GE. 4 ) THEN
                STR(1:1) = QUE(NQ-3)%STAT 
                STR(3:3) = QUE(NQ-2)%STAT 
                STR(5:5) = QUE(NQ-1)%STAT 
                STR(7:7) = QUE(NQ)%STAT 
              ELSE IF ( NQ == 3 ) THEN
                STR(1:1) = QUE(NQ-2)%STAT 
                STR(3:3) = QUE(NQ-1)%STAT 
                STR(5:5) = QUE(NQ)%STAT 
              ELSE IF ( NQ == 2 ) THEN
                STR(1:1) = QUE(NQ-1)%STAT 
                STR(3:3) = QUE(NQ)%STAT 
              ELSE IF ( NQ == 1 ) THEN
                STR(1:1) = QUE(NQ)%STAT 
              ELSE
                CALL CLRCH ( STR )
           END IF
!@           WRITE ( 6, 110 ) NB, NQ, STR(1:7)
!@ 110       FORMAT ( 'NB = ', I6, ' NQ = ', I3, 2X, A )
      END IF
      IF ( NQ == 0 ) THEN
!
! -------- No active queue elements? Nothing to do!
!
           CALL ERR_LOG ( 0, IUER )
           RETURN 
      END IF
!
! --- Check running processes
!
      NP = 0
      DO 420 J2=1,NQ
         IF ( QUE(J2)%STAT == 'R' ) THEN
              CALL ERR_PASS ( IUER, IER )
              CALL MALO_OND_CHECK ( OND, QUE(J2), STS, IVRB, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 7223, IUER, 'MALO_OND_PROC', 'Failure '// &
     &                 'in an attempt to launch process for requested ID '// &
     &                  QUE(NQ)%DATE_ORIG )
                   RETURN 
              END IF
              IF ( STS == 'R' ) NP = NP + 1
         END IF
 420  CONTINUE 
!
      DO 430 J3=1,NQ
         IF ( QUE(J3)%STAT == 'U' .OR. QUE(J3)%STAT == 'W' ) THEN
!
! ----------- Check requests with undefined or waiting status
!
              FL_DUP = .FALSE.
              IF ( NQ > 1 ) THEN
!
! ---------------- Check whether this is a duplicate request, i.e. there was already
! ---------------- a request with this time tag
!
                   DO 440 J4=1,NQ-1
                      IF ( QUE(J4)%DATE_ORIG == QUE(J3)%DATE_ORIG ) THEN
                           QUE(J3)%STAT = 'D'
                           CALL MALO_SET_STATUS ( OND, QUE(J3), QUE(J3)%STAT, IUER )
                           FL_DUP = .TRUE.
                      END IF
 440               CONTINUE 
              END IF
              IF ( FL_DUP ) GOTO 430
              IF ( NP .LE. OND%MAX_PROC ) THEN
!
! ---------------- Try to launch the processes
!
                   CALL ERR_PASS ( IUER, IER )
                   CALL MALO_OND_LAUNCH ( OND, QUE(J3), IVRB, IER )
                   IF ( IER .NE. 0 ) THEN
                        CALL ERR_LOG ( 7222, IUER, 'MALO_OND_PROC', 'Failure '// &
     &                      'in an attempt to launch process for requested ID '// &
     &                       QUE(NQ)%DATE_ORIG )
                        RETURN 
                   END IF
                   IF ( STS == 'R' ) NP = NP + 1
                 ELSE 
!
! ---------------- Too many process that run concurrently? Set status: waiting
!
                   CALL MALO_SET_STATUS ( OND, QUE(J3), 'W' )
                   CALL MALO_UPDATE_IND ( OND, QUE(J3), 'W' )
              END IF
            ELSE IF ( QUE(NQ)%STAT == 'W' ) THEN
!
! ----------- Update status "waiting"
!
              CALL MALO_UPDATE_IND ( OND, QUE(J3), 'W' )
         END IF
 430  CONTINUE 
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  MALO_OND_PROC  !#!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE MALO_OND_LAUNCH ( OND, QUE, IVRB, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine MALO_OND_LAUNCH 
! *                                                                      *
! * ### 08-MAY-2013  MALO_OND_LAUNCH  v3.3 (c) L. Petrov 17-NOV-2023 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'malo.i'
      INCLUDE   'malo_local.i'
      TYPE     ( MALO_OND__TYPE ) :: OND
      TYPE     ( MALO_QUE__TYPE ) :: QUE
      INTEGER*4  IVRB, IUER
      LOGICAL*1  LEX
      INTEGER*4  MBUF, MIND 
      PARAMETER  ( MBUF = 128*1024 )
      PARAMETER  ( MIND =  32 )
      INTEGER*4  J1, J2, J3, LIND, IND(2,MIND), IP, IR, I11, I19, NBUF, IER
      CHARACTER  BUF(MBUF)*256,  COMSTR*2048, FILCNF*128, FILCNT*128, &
     &           FILLOG*128, FILSTS*128, FILQUE_LOG*128, FILCMD*128, TMPQUE_FILE*128, &
     &           LOAD_SPL_DIR*128, LOAD_D1_SPL_DIR*128, &
     &           LOAD_SPL_PREF*128, LOAD_D1_SPL_PREF*128, SER_SPL_TO_EPH*128, &
     &           HAR_SPL_TO_EPH*128, LOAD_STA_FIL*128, &
     &           LOAD_HAR_SPL_DIR*128,  LOAD_D1_HAR_SPL_DIR*128, &
     &           LOAD_HAR_SPL_FILE*128, LOAD_D1_HAR_SPL_FILE*128, FINAM_SUFFIX*128
      CHARACTER  STR*128, STR1*128, PID_STR*8, MODEL*16, STS*1, REG*3
      PARAMETER  ( REG = CHAR(0)//CHAR(32)//CHAR(9) )
      CHARACTER, EXTERNAL :: GET_CDATE*19
      INTEGER*4, EXTERNAL :: ILEN, I_LEN, FORK, EXECL, GETPID, LINDEX, RENAME 
!
      FILCNF = OND%OND_DIR(1:I_LEN(OND%OND_DIR))//'/control/'// &
     &         QUE%MODEL(1:I_LEN(QUE%MODEL))//'.cnf'
      INQUIRE ( FILE=FILCNF, EXIST=LEX )
      IF ( .NOT. LEX ) THEN
           CALL ERR_LOG ( 7231, IUER, 'MALO_OND_LAUNCH', 'Cannot find '// &
     &         'template control file '//FILCNF )
           RETURN 
      END IF
!
! --- Read template control file
!
      CALL ERR_PASS ( IUER, IER )
      CALL RD_TEXT  ( FILCNF, MBUF, BUF, NBUF, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 7232, IUER, 'MALO_OND_LAUNCH', 'Error in reading '// &
     &         'template control file '//FILCNF )
           RETURN 
      END IF
!
      FILCNF = OND%REQ_DIR(1:I_LEN(OND%REQ_DIR))//'/'//QUE%DATE_ORIG// &
     &         '/control.txt'
      FILSTS = OND%REQ_DIR(1:I_LEN(OND%REQ_DIR))//'/'//QUE%DATE_ORIG// &
     &         '/status.txt'
      FILLOG = OND%REQ_DIR(1:I_LEN(OND%REQ_DIR))//'/'//QUE%DATE_ORIG// &
     &         '/log.txt'
      FILCMD = OND%REQ_DIR(1:I_LEN(OND%REQ_DIR))//'/'//QUE%DATE_ORIG// &
     &         '/cmd.txt'
!
      FILCNT = '???'
      DO 410 J1=1,NBUF
         IF ( BUF(J1)(1:1) == '#' ) GOTO 410
         CALL EXWORD ( BUF(J1), MIND, LIND, IND, REG, IER )
!
         IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'CONTROL:' ) THEN
              FILCNT = MALO_SHARE//'/'//BUF(J1)(IND(1,2):IND(2,2)) 
         END IF
 410  CONTINUE 
      IF ( FILCNT == '????' ) THEN
           CALL ERR_LOG ( 7233, IUER, 'MALO_OND_LAUNCH', 'Trap of internal '// &
     &         'control: did not found control file in '//FILCNF )
           RETURN 
      END IF
!
      INQUIRE ( FILE=FILCNT, EXIST=LEX )
      IF ( .NOT. LEX ) THEN
           CALL ERR_LOG ( 7234, IUER, 'MALO_OND_LAUNCH', 'Trap of internal '// &
     &         'control: MALO control file '//TRIM(FILCNT)// ' was not found' )
           RETURN 
      END IF
!
! --- Read MALO control file
!
      CALL ERR_PASS ( IUER, IER )
      CALL RD_TEXT  ( FILCNT, MBUF, BUF, NBUF, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 7235, IUER, 'MALO_OND_LAUNCH', 'Error in reading '// &
     &         'template control file '//FILCNT )
           RETURN 
      END IF
!
      LOAD_SPL_DIR        = '????'
      LOAD_HAR_SPL_DIR    = '????'
      LOAD_D1_SPL_DIR     = '????'
      LOAD_D1_HAR_SPL_DIR = '????'
      LOAD_SPL_PREF       = '????'
      LOAD_D1_SPL_PREF    = '????'
!
      DO 420 J2=1,NBUF
         IF ( BUF(J2)(1:1) == '#' ) GOTO 420
         CALL EXWORD ( BUF(J2), MIND, LIND, IND, REG, IER )
         IF ( LIND < 2 ) GOTO 420
         IF ( BUF(J2)(IND(1,1):IND(2,1)) == 'load_spl_dir:' ) THEN
              LOAD_SPL_DIR = BUF(J2)(IND(1,2):IND(2,2)) 
            ELSE IF ( BUF(J2)(IND(1,1):IND(2,1)) == 'load_har_spl_dir:' ) THEN
              LOAD_HAR_SPL_DIR = BUF(J2)(IND(1,2):IND(2,2)) 
            ELSE IF ( BUF(J2)(IND(1,1):IND(2,1)) == 'load_d1_spl_dir:' ) THEN
              LOAD_D1_SPL_DIR = BUF(J2)(IND(1,2):IND(2,2)) 
            ELSE IF ( BUF(J2)(IND(1,1):IND(2,1)) == 'load_d1_har_spl_dir:' ) THEN
              LOAD_D1_HAR_SPL_DIR = BUF(J2)(IND(1,2):IND(2,2)) 
            ELSE IF ( BUF(J2)(IND(1,1):IND(2,1)) == 'load_spl_pref:' ) THEN
              LOAD_SPL_PREF = BUF(J2)(IND(1,2):IND(2,2)) 
            ELSE IF ( BUF(J2)(IND(1,1):IND(2,1)) == 'load_d1_spl_pref:' ) THEN
              LOAD_D1_SPL_PREF = BUF(J2)(IND(1,2):IND(2,2)) 
         END IF
 420  CONTINUE 
!
      IF ( LOAD_SPL_DIR == '????' ) THEN
           CALL ERR_LOG ( 7236, IUER, 'MALO_OND_LAUNCH', 'Trap of internal '// &
     &         'control: did not found load_spl_dir keyword file in '//FILCNT )
           RETURN 
      END IF
      IF ( LOAD_HAR_SPL_DIR == '????' ) THEN
           CALL ERR_LOG ( 7237, IUER, 'MALO_OND_LAUNCH', 'Trap of internal '// &
     &         'control: did not found load_har_spl_dir keyword file in '//FILCNT )
           RETURN 
      END IF
      IF ( LOAD_D1_SPL_DIR == '????' ) THEN
           CALL ERR_LOG ( 7238, IUER, 'MALO_OND_LAUNCH', 'Trap of internal '// &
     &         'control: did not found load_d1_spl_dir keyword file in '//FILCNT )
           RETURN 
      END IF
      IF ( LOAD_D1_SPL_DIR == '????' ) THEN
           CALL ERR_LOG ( 7239, IUER, 'MALO_OND_LAUNCH', 'Trap of internal '// &
     &         'control: did not found load_har_d1_spl_dir keyword file in '//FILCNT )
           RETURN 
      END IF
      IF ( LOAD_SPL_PREF == '????' ) THEN
           CALL ERR_LOG ( 7240, IUER, 'MALO_OND_LAUNCH', 'Trap of internal '// &
     &         'control: did not found load_spl_pref keyword file in '//FILCNT )
           RETURN 
      END IF
      IF ( LOAD_D1_SPL_PREF == '????' ) THEN
           CALL ERR_LOG ( 7241, IUER, 'MALO_OND_LAUNCH', 'Trap of internal '// &
     &         'control: did not found load_d1_spl_pref keyword file in '//FILCNT )
           RETURN 
      END IF
!
      IF ( LOAD_HAR_SPL_DIR == 'none' ) THEN
           LOAD_HAR_SPL_DIR  = 'none'
         ELSE
           LOAD_HAR_SPL_FILE = TRIM(LOAD_HAR_SPL_DIR)//'/'//TRIM(LOAD_SPL_PREF)//'harmod.heb'
           INQUIRE ( FILE=LOAD_HAR_SPL_FILE, EXIST=LEX ) 
           IF ( .NOT. LEX ) THEN
                CALL ERR_LOG ( 7242, IUER, 'MALO_OND_LAUNCH', 'Trap of internal '// &
     &              'control: did not found load_har_spl_file '//LOAD_HAR_SPL_FILE )
                RETURN 
           END IF
      END IF
!
      IF ( LOAD_D1_HAR_SPL_DIR == 'none' ) THEN
           LOAD_D1_HAR_SPL_FILE = 'none'
         ELSE
           LOAD_D1_HAR_SPL_FILE = TRIM(LOAD_D1_HAR_SPL_DIR)//'/'//TRIM(LOAD_D1_SPL_PREF)//'harmod.heb'
           INQUIRE ( FILE=LOAD_HAR_SPL_FILE, EXIST=LEX ) 
           IF ( .NOT. LEX ) THEN
                CALL ERR_LOG ( 7243, IUER, 'MALO_OND_LAUNCH', 'Trap of internal '// &
     &              'control: did not found load_d1_har_spl_file '//LOAD_D1_HAR_SPL_FILE )
                RETURN 
           END IF
      END IF
!
      SER_SPL_TO_EPH = MALO_SCRIPT//'/spl_to_eph.py'
      HAR_SPL_TO_EPH = MALO_ROOT//'/bin_static'//'/loading_spl_heb_to_sta'
!
      LOAD_STA_FIL = OND%REQ_DIR(1:I_LEN(OND%REQ_DIR))//'/'//QUE%DATE_ORIG//'/sta_fil.txt'
!
! --- Set command line for MALO program that will compute mass
! --- loading time series
!
      CALL CLRCH ( COMSTR )
      CALL CLRCH ( MODEL )
      CALL TRAN  ( 12, QUE%MODEL(1:I_LEN(QUE%MODEL)), MODEL )
      IF ( QUE%MODE == 'series' ) THEN
           IF ( QUE%FRAME == MALO__FRAME_CM ) THEN
                COMSTR = TRIM(SER_SPL_TO_EPH)//' '// &
     &                   '-s '//TRIM(LOAD_SPL_DIR)//' '// &
     &                   '-f '//TRIM(LOAD_STA_FIL)//' '// &
     &                   '-b '//TRIM(QUE%START_DATE)//' '// &
     &                   '-e '//TRIM(QUE%STOP_DATE)//' '// &
     &                   '-o '//OND%REQ_DIR(1:I_LEN(OND%REQ_DIR))//'/'// &
     &                   QUE%DATE_ORIG(1:I_LEN(QUE%DATE_ORIG))//' '// &
     &                   '-p cm_loading_'//MODEL(1:I_LEN(MODEL))//'_'//' '// &
     &                   '-m'//' '// &
     &                   '-v 1'//' '// &
     &                   ' >& '// &
     &                   FILLOG
              ELSE IF ( QUE%FRAME == MALO__FRAME_D1 ) THEN
                COMSTR = TRIM(SER_SPL_TO_EPH)//' '// &
     &                   '-s '//TRIM(LOAD_D1_SPL_DIR)//' '// &
     &                   '-f '//TRIM(LOAD_STA_FIL)//' '// &
     &                   '-b '//TRIM(QUE%START_DATE)//' '// &
     &                   '-e '//TRIM(QUE%STOP_DATE)//' '// &
     &                   '-o '//OND%REQ_DIR(1:I_LEN(OND%REQ_DIR))//'/'// &
     &                   QUE%DATE_ORIG(1:I_LEN(QUE%DATE_ORIG))//' '// &
     &                   '-p d1_loading_'//MODEL(1:I_LEN(MODEL))//'_'//' '// &
     &                   '-m'//' '// &
     &                   '-v 1'//' '// &
     &                   ' >& '// &
     &                   FILLOG
              ELSE IF ( QUE%FRAME == MALO__FRAME_CF ) THEN
                COMSTR = TRIM(SER_SPL_TO_EPH)//' '// &
     &                   '-s '//TRIM(LOAD_SPL_DIR)//' '// &
     &                   '-d '//TRIM(LOAD_D1_SPL_DIR)//' '// &
     &                   '-f '//TRIM(LOAD_STA_FIL)//' '// &
     &                   '-b '//TRIM(QUE%START_DATE)//' '// &
     &                   '-e '//TRIM(QUE%STOP_DATE)//' '// &
     &                   '-o '//OND%REQ_DIR(1:I_LEN(OND%REQ_DIR))//'/'// &
     &                   QUE%DATE_ORIG(1:I_LEN(QUE%DATE_ORIG))//' '// &
     &                   '-p cf_loading_'//MODEL(1:I_LEN(MODEL))//'_'//' '// &
     &                   '-m'//' '// &
     &                   '-v 1'//' '// &
     &                   ' >& '// &
     &                   FILLOG
           END IF
        ELSE IF ( QUE%MODE == 'harmonics' .OR. QUE%MODE == 's1_harmonics' ) THEN
           IF ( QUE%MODE == 'harmonics' ) THEN
                FINAM_SUFFIX = '_harmod.hps'
              ELSE IF ( QUE%MODE == 's1_harmonics' ) THEN
                FINAM_SUFFIX = '_harmod_s1.hps'
           END IF
!
           IF ( QUE%FRAME == MALO__FRAME_CM ) THEN
                COMSTR = TRIM(HAR_SPL_TO_EPH)//' '// &
     &                   TRIM(LOAD_HAR_SPL_FILE)//' '// &
     &                   TRIM(LOAD_STA_FIL)//' '// &
     &                   OND%REQ_DIR(1:I_LEN(OND%REQ_DIR))//'/'// &
     &                   QUE%DATE_ORIG(1:I_LEN(QUE%DATE_ORIG))//'/'// &
     &                   'cm_loading_'//MODEL(1:I_LEN(MODEL))//TRIM(FINAM_SUFFIX)// &
     &                   ' 2 '// &
     &                   ' >& '// &
     &                   FILLOG
              ELSE IF ( QUE%FRAME == MALO__FRAME_D1 ) THEN
                COMSTR = TRIM(HAR_SPL_TO_EPH)//' '// &
     &                   TRIM(LOAD_D1_HAR_SPL_FILE)//' '// &
     &                   TRIM(LOAD_STA_FIL)//' '// &
     &                   OND%REQ_DIR(1:I_LEN(OND%REQ_DIR))//'/'// &
     &                   QUE%DATE_ORIG(1:I_LEN(QUE%DATE_ORIG))//'/'// &
     &                   'd1_loading_'//MODEL(1:I_LEN(MODEL))//TRIM(FINAM_SUFFIX)// &
     &                   ' 2 '// &
     &                   ' >& '// &
     &                   FILLOG
              ELSE IF ( QUE%FRAME == MALO__FRAME_CF ) THEN
                COMSTR = TRIM(HAR_SPL_TO_EPH)//' '// &
     &                   TRIM(LOAD_HAR_SPL_FILE)//' '// &
     &                   TRIM(LOAD_D1_HAR_SPL_FILE)//' '// &
     &                   TRIM(LOAD_STA_FIL)//' '// &
     &                   OND%REQ_DIR(1:I_LEN(OND%REQ_DIR))//'/'// &
     &                   QUE%DATE_ORIG(1:I_LEN(QUE%DATE_ORIG))//'/'// &
     &                   'cf_loading_'//MODEL(1:I_LEN(MODEL))//TRIM(FINAM_SUFFIX)// &
     &                   ' 2 '// &
     &                   ' >& '// &
     &                   FILLOG
           END IF
      END IF
      IF ( IVRB .GE. 4 ) THEN
           WRITE ( 6, '(A)' ) 'Command: '//COMSTR(1:I_LEN(COMSTR)) 
      END IF
      OPEN  ( UNIT=19, FILE=FILCMD, STATUS='UNKNOWN', IOSTAT=I19 )
      WRITE ( UNIT=19, FMT='(A)' ) COMSTR(1:I_LEN(COMSTR)) 
      CLOSE ( UNIT=19 ) 
!
! --- Create a supbrocess
!
      QUE%PID = FORK ()
      IF ( QUE%PID .EQ. -1 ) THEN
           CALL CLRCH  ( STR )
           CALL GERROR ( STR )
           CALL ERR_LOG ( 7244, IUER, 'MALO_OND_LAUNCH', 'Failure to '// &
     &         'create a subprocess because '//STR )
           RETURN
      END IF
      IF ( QUE%PID .EQ. 0 ) THEN
!
! -------- Execute a csh-script
!
           IP = EXECL ( '/bin/csh'//CHAR(0), 'csh'//CHAR(0), &
     &                  '-cf'//CHAR(0), &
     &                   COMSTR(1:I_LEN(COMSTR))//CHAR(0), %VAL(0) )
           IF ( IP .NE. 0 ) THEN
!
! ------------- Error in attempt to execute a csh-script -- write it
! ------------- down to the log file
!
                CALL CLRCH ( STR )
                CALL GERROR ( STR )
                STR1 = GET_CDATE()
                OPEN ( UNIT=11, FILE=OND%LOG_FILE, STATUS='UNKNOWN', &
     &                 ACCESS='APPEND', IOSTAT=I11 )
                WRITE ( 11, '(A)' ) '$$$ '//STR1(1:19)//' '// &
     &                               COMSTR(1:I_LEN(COMSTR))
                WRITE ( 11, '(A)' ) '$$$ '//STR1(1:19)//' '//STR(1:I_LEN(STR))
                CLOSE ( UNIT=11 )
           END IF
!
! -------- Noramally, we should not get here.
!
           OPEN ( UNIT=11, FILE=FILSTS, STATUS='UNKNOWN', ACCESS='APPEND', IOSTAT=I11 )
           IF ( I11 .NE. 0 ) THEN
                CALL ERR_LOG ( 7245, IUER, 'MALO_OND_LAUNCH', 'Error in '// &
     &              'attempt to open file '//FILSTS )
                RETURN
           END IF
           STR1 = GET_CDATE()
           WRITE ( 11, '(A)' ) STR1(1:I_LEN(STR1))//' Error during '// &
     &                        'computation: '//STR(1:I_LEN(STR))
           CLOSE ( UNIT=11 )
!
           OPEN ( UNIT=11, FILE=OND%LOG_FILE, STATUS='UNKNOWN', ACCESS='APPEND', &
     &            IOSTAT=I11 )
           IF ( I11 .NE. 0 ) THEN
                CALL ERR_LOG ( 7246, IUER, 'MALO_OND_LAUNCH', 'Error in '// &
     &              'attempt to open file '//OND%LOG_FILE )
                RETURN
           END IF
           STR1 = GET_CDATE()
           WRITE ( 11, '(A)' ) STR1(1:I_LEN(STR1))//' Error during '// &
     &                        'computation: '//STR(1:I_LEN(STR))
           CLOSE ( UNIT=11 )
!
! -------- Terminate execution of a child subprocess
!
           CALL EXIT ( IP )
           CALL ERR_LOG ( 0, IUER )
           RETURN 
      END IF
!
! --- Decode QUE%PID of child subprocess
!
      CALL INCH     ( QUE%PID, PID_STR )
      CALL CHASHR   (          PID_STR )
      CALL BLANK_TO_ZERO (     PID_STR )
!
! --- Write down QUE%PID into the queue file
!
      CALL ERR_PASS ( IUER, IER )
      CALL RD_TEXT ( OND%QUEUE_FILE, MBUF, BUF, NBUF, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 7248, IUER, 'MALO_OND_LAUNCH', 'Error in reading '// &
     &         'queue file '//OND%QUEUE_FILE )
           RETURN 
      END IF
!
      DO 430 J3=1,NBUF
         IF ( BUF(J3)(1:15) == QUE%DATE_ORIG ) THEN
              BUF(J3)(157:164) =  PID_STR
              BUF(J3)(155:155) = 'R'
         END IF
 430  CONTINUE 
!
      CALL INCH ( GETPID(), TMPQUE_FILE(1:8) )
      CALL CHASHR         ( TMPQUE_FILE(1:8) )
      CALL BLANK_TO_ZERO  ( TMPQUE_FILE(1:8) )
      TMPQUE_FILE = OND%QUEUE_FILE(1:I_LEN(OND%QUEUE_FILE))//'_'//TMPQUE_FILE(1:8)
!
      CALL ERR_PASS ( IUER, IER )
      CALL WR_TEXT  ( NBUF, BUF, TMPQUE_FILE, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 7249, IUER, 'MALO_OND_LAUNCH', 'Error in writing '// &
     &         'into temporary queue file '//TMPQUE_FILE )
           RETURN 
      END IF
!
      IR = RENAME ( TMPQUE_FILE(1:I_LEN(TMPQUE_FILE))//CHAR(0), &
     &              OND%QUEUE_FILE(1:I_LEN(OND%QUEUE_FILE))//CHAR(0) )
      IF ( IR .NE. 0 ) THEN
           CALL ERR_LOG ( 7250, IUER, 'MALO_OND_LAUNCH', 'Error in writing '// &
     &         'into queue file '//OND%QUEUE_FILE )
           RETURN 
      END IF
!
      CALL SYSTEM ( 'chmod g+wr,o+wr '//TRIM(OND%QUEUE_FILE)//CHAR(0) )
      CALL SYSTEM ( 'chgrp '//TRIM(OND%SERVER_GROUP)//' '//TRIM(OND%QUEUE_FILE)//CHAR(0) )
!
! --- Add a record to log file
!
      OPEN ( UNIT=11, FILE=OND%LOG_FILE, STATUS='UNKNOWN', ACCESS='APPEND', IOSTAT=I11 )
      IF ( I11 .NE. 0 ) THEN
           CALL ERR_LOG ( 7251, IUER, 'MALO_OND_LAUNCH', 'Error in '// &
     &              'attempt to open log file '//OND%LOG_FILE )
           RETURN
      END IF
      STR1 = GET_CDATE()
      WRITE ( 11, '(A)' ) STR1(1:I_LEN(STR1))//' Request '//QUE%DATE_ORIG// &
     &                    ' from IP '//QUE%IP_ADDR//' launched process '// &
     &                    ' with pid '//PID_STR
      CLOSE ( UNIT=11 )
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END   SUBROUTINE  MALO_OND_LAUNCH   !#!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE MALO_OND_CHECK ( OND, QUE, IVRB, STS, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine MALO_OND_CHECK
! *                                                                      *
! * ### 10-MAY-2013  MALO_OND_CHECK  v1.4 (c) L. Petrov  17-NOV-2023 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'malo.i'
      TYPE     ( MALO_OND__TYPE ) :: OND
      TYPE     ( MALO_QUE__TYPE ) :: QUE
      CHARACTER  STS*(*)
      INTEGER*4  IVRB, IUER
      INTEGER*4  MBUF, MMON, MERR
      PARAMETER  ( MBUF = 128*1024 )
      PARAMETER  ( MMON =    8192  )
      PARAMETER  ( MERR =    12    )
      CHARACTER  BUF(MBUF)*256, FILLOG*256, FILIND*256, FILNAM*128, &
     &           STR*128, STR1*128, COM_STR*128, C_FIL(MMON)*128, &
     &           ERR(MBUF)*256, DIR*128, EXT*4
      INTEGER*8  DIR_DESC(16)
      INTEGER*8  SIZE_I8
      INTEGER*4  GROUP_PID, PARENT_PID, IB, ID, IK, IL, IS, NL, I17, I18, &
     &           ISTAT, J1, J2, J3, J4, NBUF, NO, NERR, LEV, L_FIL, &
     &           UNIX_DATE, LOG_UNIT, LOGPROC_UNIT, IER
      CHARACTER, EXTERNAL :: GET_CDATE*19
      INTEGER*4, EXTERNAL :: GETPGRP, GET_FILE_FROM_DIR, ILEN, I_LEN, KILL, &
     &                       LINDEX, FILE_INFO 
!
      FILLOG = OND%REQ_DIR(1:I_LEN(OND%REQ_DIR))//'/'//QUE%DATE_ORIG// &
     &         '/log.txt'
      FILIND = OND%REQ_DIR(1:I_LEN(OND%REQ_DIR))//'/'//QUE%DATE_ORIG// &
     &         '/index.html'
!
! --- Store PID of the group process of malo_ondemand
!
      GROUP_PID = GETPGRP  ( %VAL(0) )
!
! --- Get the parent PID of the checked process
!
      PARENT_PID = GETPGRP ( %VAL(QUE%PID) )
!
! --- Send a signal to the checked process
!
      IK = KILL ( %VAL(QUE%PID), %VAL(0) )
      IF ( IK .EQ. 0  .OR.  PARENT_PID .NE. GROUP_PID ) THEN
!
! -------- Process is alive and running
!
           IS = FILE_INFO ( FILLOG(1:I_LEN(FILLOG))//CHAR(0), UNIX_DATE, SIZE_I8 )
!
           IF ( SIZE_I8 > 0 ) THEN
                CALL ERR_PASS ( IUER, IER )
                CALL RD_TEXT  ( FILLOG, MBUF, BUF, NL, IER )
                IF ( IER .NE. 0 ) THEN
                     CALL ERR_LOG ( 7241, IUER, 'MALO_OND_CHECK', 'Error in '// &
     &                   'an attempt to read log file '//FILLOG )
                     RETURN 
                END IF
                STR = '<PRE>'//TRIM(BUF(NL))//'</PRE>'
              ELSE 
                STR = ' '
           END IF 
!
! -------- Create Web page with progress message
!
           BUF(1) = '<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 3.2//EN">'
           BUF(2) = '<HTML><HEAD> <META HTTP-EQUIV="Content-Type" content="text/html; charset=iso-8859-1">'
           BUF(3) = '<META HTTP-EQUIV="refresh" CONTENT="5">'
           BUF(4) = '<STYLE TYPE="text/css"> PRE {display: inline;} </STYLE>'
           BUF(5) = '</HEAD><BODY>'
           BUF(6) = GET_CDATE()//' &nbsp;&nbsp; Loading displacements are being computed... <BR>'
           BUF(7) = STR
           BUF(8) = '</BODY></HTML>'
!
           CALL ERR_PASS ( IUER, IER )
           CALL WR_TEXT  ( 8, BUF, FILIND, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 7242, IUER, 'MALO_OND_CEHCK', 'Error in writing '// &
     &              'into index file '//FILIND )
                RETURN 
           END IF
           STS = 'R'
         ELSE
!
! -------- Process is stopped. Check whether it terminated correctly
!
! -------- Wait for the child process. Actually there is nothing to wait
! -------- for but this "waiting" removes zomby.
!
           CALL WAIT3 ( ISTAT, 1, %VAL(0) )
!
! -------- Check whether it terminated correctly
!
           IS = FILE_INFO ( FILLOG(1:I_LEN(FILLOG))//CHAR(0), UNIX_DATE, SIZE_I8 )
!
           IF ( SIZE_I8 < 0 ) THEN
                NL = 1
                BUF(NL) = 'ERROR: The process has stopped for unknown reason'
              ELSE IF ( SIZE_I8 == 0 ) THEN
                NL = 1
                BUF(NL) = 'ERROR: Abnormal termination. The process has been lost.'
              ELSE
                CALL ERR_PASS ( IUER, IER )
                CALL RD_TEXT  ( FILLOG, MBUF, BUF, NL, IER )
                IF ( IER .NE. 0 ) THEN
                     CALL ERR_LOG ( 7243, IUER, 'MALO_OND_CHECK', 'Error in an attempt '// &
     &                   'to read log file '//FILLOG )
                     RETURN 
                END IF
           END IF
!
           NERR = MIN ( NL, MERR )
           IB = NL - NERR + 1
           IF ( IB < 1 ) IB = 1
           DO 410 J1=1,NERR
              ERR(J1) = BUF(J1-1+IB)
 410       CONTINUE 
!
! -------- Open the log file. We want to append a record there.
!
           LOG_UNIT = 17
           OPEN ( UNIT=LOG_UNIT, FILE=OND%LOG_FILE, STATUS='UNKNOWN', ACCESS='APPEND', &
     &            IOSTAT=I17 )
           IF ( I17 .NE. 0 ) THEN
                CALL ERR_LOG ( 7244, IUER, 'MALO_OND_CHECK', 'Error in '// &
     &              'attempt to open log file '//OND%LOG_FILE )
                RETURN
           END IF
!
           LOGPROC_UNIT = 18
           OPEN ( UNIT=LOGPROC_UNIT, FILE=FILLOG, STATUS='UNKNOWN', ACCESS='APPEND', &
     &            IOSTAT=I18 )
           IF ( I18 .NE. 0 ) THEN
                CALL ERR_LOG ( 7244, IUER, 'MALO_OND_CHECK', 'Error in '// &
     &              'attempt to open log file '//OND%LOG_FILE )
                RETURN
           END IF
!
           BUF(1) = '<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 3.2//EN">'
           BUF(2) = '<HTML><HEAD> <META HTTP-EQUIV="Content-Type" content="text/html; charset=iso-8859-1">'
           BUF(3) = '</HEAD><BODY>'
           IF ( NERR > 2 .AND. ERR(NERR)(21:41) == 'Successful completion' ) THEN
!
! ------------- Fixing the log for a pathological case when two processess with the same ID ran
!
                IF ( ERR(NERR-1)(32:69) == 'epochs have been successfully computed' ) THEN
                     NERR = NERR - 1
                END IF
           END IF
           NO = 3
           IF ( ERR(NERR)(32:69) == 'epochs have been successfully computed' .OR. &
     &          ERR(NERR)(1:32)  == 'LOADING_SPL_HEB_TO_STA: finished'            ) THEN
!
                IF ( ERR(NERR)(32:69) == 'epochs have been successfully computed' ) THEN
                     BUF(4) = GET_CDATE()//' &nbsp;&nbsp; Loading time series has been computed <BR>'
                     EXT = '.eph'
                   ELSE IF ( ERR(NERR)(1:32) == 'LOADING_SPL_HEB_TO_STA: finished' ) THEN
                     BUF(4) = GET_CDATE()//' &nbsp;&nbsp; Loading harmonics coefficients have been computed <BR>'
                     EXT = '.hps'
                END IF
                BUF(5) = '<FONT COLOR="30A040"><B> Request '//QUE%DATE_ORIG//'</B></FONT> '// &
     &                   'was initiated from IP address <FONT COLOR="A04030"><B>'// &
     &                    QUE%IP_ADDR(1:I_LEN(QUE%IP_ADDR))//'</B></FONT> <BR>'
                BUF(6) = ERR(NERR)(1:I_LEN(ERR(NERR)))//' <BR>'
                NO = 6
                IF ( NERR > 1 ) THEN
                     NO = NO + 1
                     BUF(NO) = ERR(NERR-1)(1:I_LEN(ERR(NERR-1)))//' <BR>'
                END IF
!
! ------------- Now we would like ot get the list of output files
!
                L_FIL = 0
                LEV = 0
                DIR = OND%REQ_DIR(1:I_LEN(OND%REQ_DIR))//'/'//QUE%DATE_ORIG
                DO 420 J1=1,16*MALO__FIL
                   IS = GET_FILE_FROM_DIR ( LEV, DIR_DESC, DIR, FILNAM )
                   IF ( IS .NE. 0 ) THEN
                        CALL ERR_LOG ( 7245, IUER, 'MALO_OND_CHECK', 'Error '// &
     &                      'in reading input directory '//DIR(1:I_LEN(DIR))// &
     &                      '  '//FILNAM )
                        RETURN
                   END IF
                   IF ( LEV == 0 ) GOTO 820 ! End of work
                   IL = ILEN(FILNAM)
!
! ---------------- Various filters...
!
                   IF ( IL < 12                      ) GOTO 420
                   IF ( INDEX ( FILNAM, EXT ) .LE. 0 ) GOTO 420
!
                   L_FIL = L_FIL + 1
                   IF ( L_FIL > MMON ) THEN
                        CALL CLRCH ( STR )
                        CALL INCH  ( MMON, STR )
                        CALL ERR_LOG ( 7246, IUER, 'MALO_OND_CHECK', &
     &                      'Trap of internal control: too many files '// &
     &                      'with displacement time series in input '// &
     &                      'directory '//DIR(1:I_LEN(DIR))//' -- '// &
     &                      STR(1:I_LEN(STR))//' files' )
                        RETURN
                   END IF
                   C_FIL(L_FIL) = FILNAM 
 420            CONTINUE 
 820            CONTINUE 
                IF ( L_FIL == 0 ) THEN
                     NO = NO + 1
                     WRITE ( BUF(NO), '(A)' ) 'Strange... but no files with '// &
     &                                        'displacements has been found'
                     WRITE ( LOG_UNIT, '(A)' ) 'Strange... but no files with '// &
     &                                   'displacements has been found'
                   ELSE 
!
! ------------------ Sort the list of files
!
                     NO = NO + 1
                     BUF(NO) = '<P>'
                     CALL SORT_FAST_CH ( L_FIL, C_FIL )
                     CALL INCH ( L_FIL, STR )
                     NO = NO + 1
                     IF ( EXT == '.eph' ) THEN
                          BUF(NO) = '<B><FONT SIZE=5>Download:</FONT></B> &nbsp; '// &
     &                              STR(1:I_LEN(STR))//' files with time series in'// &
     &                              ' <A HREF="http://astrogeo.org/malo/ephedisp_format.txt">'// &
     &                              'EPHEDISP format</A> are available: <BR>'
                       ELSE IF ( EXT == '.hps' ) THEN
                          BUF(NO) = '<B><FONT SIZE=5>Download:</FONT></B> &nbsp; '// &
     &                              STR(1:I_LEN(STR))//' files with harmonics coefficients '// &
     &                              ' <A HREF="http://astrogeo.org/malo/harpos_format.txt">'// &
     &                              'HARPOS format</A> are available: <BR>'
                     END IF
                     NO = NO + 1
                     BUF(NO) = '<UL>'
!
! ------------------ Print the list of hyperlinks to output files in EPHEDISP
! ------------------ format
!
                     DO 430 J3=1,L_FIL
                        NO = NO + 1
                        ID = LINDEX ( C_FIL(J3), '/' ) + 1
                        WRITE ( UNIT=BUF(NO), FMT='(A)' ) &
     &                         '  <LI>  <A HREF="'// &
     &                         OND%REQ_HTML_DIR(1:I_LEN(OND%REQ_HTML_DIR))// &
     &                         '/'//QUE%DATE_ORIG//'/'// &
     &                         C_FIL(J3)(ID:I_LEN(C_FIL(J3)))// &
     &                         '">'//C_FIL(J3)(ID:I_LEN(C_FIL(J3)))//'</A> <BR></LI>'
 430                 CONTINUE 
                     NO = NO + 1
                     BUF(NO) = '</UL>'
                END IF
                STS = 'S'
                WRITE ( LOG_UNIT, '(A,I8,A)' ) GET_CDATE()//' Request '//QUE%DATE_ORIG// &
     &                    ' with pid ', QUE%PID, ' successfully finished'
                WRITE ( LOGPROC_UNIT, '(A)' ) GET_CDATE()//' Successful completion'
              ELSE IF ( ERR(NERR)(21:41) == 'Successful completion' ) THEN
!
! ------------- Normally, it should not go here
!
                STS = 'S'
                WRITE ( LOG_UNIT, '(A,I8,A)' ) GET_CDATE()//' Request '//QUE%DATE_ORIG// &
     &                    ' with pid ', QUE%PID, ' successfully finished'
                WRITE ( LOGPROC_UNIT, '(A)' ) GET_CDATE()//' Successful completion 2.'
              ELSE 
                BUF(4) = GET_CDATE()//' &nbsp;&nbsp; Computation of time series has not completed due to an error:<BR>'
                BUF(5) = '<FONT COLOR="30A040"><B> Request '//QUE%DATE_ORIG//'</B></FONT> '// &
     &                   'was initiated from IP address <FONT COLOR="A04030"><B>'// &
     &                    QUE%IP_ADDR(1:I_LEN(QUE%IP_ADDR))//'</B></FONT>. '// &
     &                    'Last error messages: <BR>'
                NO = 5
                DO 440 J4=1,NERR
                   NO = NO + 1
                   BUF(NO) = ERR(J4)(1:I_LEN(ERR(J4)))//' <BR>'
 440            CONTINUE 
                STS = 'E'
                WRITE ( LOG_UNIT, '(A,I8,A)' ) GET_CDATE()//' Request '//QUE%DATE_ORIG// &
     &                      ' with pid ', QUE%PID, ' terminated with ERROR'
                WRITE ( LOGPROC_UNIT, '(A)' ) GET_CDATE()//' Abnormal termination'
           END IF
           CLOSE ( UNIT=LOG_UNIT )
           CLOSE ( UNIT=LOGPROC_UNIT )
!
           NO = NO + 1
           BUF(NO) = '</BODY></HTML>'
!
! -------- Write down the index HTML file 
!
           CALL ERR_PASS ( IUER, IER )
           CALL WR_TEXT  ( NO, BUF, FILIND, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 7247, IUER, 'MALO_OND_CHECK', 'Error in writing '// &
     &              'into index file '//FILIND )
                RETURN 
           END IF
!
! -------- Set the final status of the request
!
           CALL ERR_PASS ( IUER, IER )
           CALL MALO_SET_STATUS ( OND, QUE, STS, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 7248, IUER, 'MALO_OND_CHECK', 'Error in an '// &
     &              'attempt to write the status of the request in the '// &
     &              'queue file '//OND%QUEUE_FILE )
                RETURN 
           END IF
!
! -------- Send email
!
           CALL ERR_PASS ( IUER, IER )
           CALL MALO_SEND_EMAIL ( OND, QUE, STS, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 7249, IUER, 'MALO_OND_CHECK', 'Error in an '// &
     &              'attempt to send email to the user whi initiated request' )
                RETURN 
           END IF
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE MALO_OND_CHECK  !#!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE MALO_SET_STATUS ( OND, QUE, STS, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  MALO_SET_STATUS
! *                                                                      *
! * ### 10-MAY-2013 MALO_SET_STATUS v1.4 (c) L. Petrov  20-SEP-2023  ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'malo.i'
      TYPE     ( MALO_OND__TYPE ) :: OND
      TYPE     ( MALO_QUE__TYPE ) :: QUE
      CHARACTER  STS*(*)
      INTEGER*4  IUER
      INTEGER*4  MBUF
      PARAMETER  ( MBUF = 128*1024 )
      CHARACTER  BUF(MBUF)*256, TMPQUE_FILE*128
      INTEGER*4  J1, NBUF, IR, IS, IER
      LOGICAL*1  LEX
      CHARACTER, EXTERNAL :: GET_CDATE*19
      INTEGER*4, EXTERNAL :: ILEN, I_LEN, CLOSEDIR, GETPID, LINDEX, UNLINK
!
! --- Check lock file, if exists check it every 0.01 sec for 32.0 seconds.
! --- If does not exist then create the file and write there 
! --- current date and PID
!
      CALL ERR_PASS ( IUER, IER )
      CALL SET_FILE_LOCK ( OND%QUEUE_LOCK_FILE, 0.01D0, 32.0D0, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 7251, IUER, 'MALO_SET_STATUS', 'Failure to '// &
     &         'set lock file for the request queue' )
           RETURN 
      END IF
!
! --- Set appropriate permissions for the queue lock file
!
      INQUIRE ( FILE=OND%QUEUE_LOCK_FILE, EXIST=LEX )
      IF ( LEX ) THEN
           CALL SYSTEM ( 'chmod g+wr   '//TRIM(OND%QUEUE_LOCK_FILE)//CHAR(0) )
           CALL SYSTEM ( 'chgrp '//TRIM(OND%SERVER_GROUP)//' '//TRIM(OND%QUEUE_LOCK_FILE)//CHAR(0) )
      END IF
!
! --- Read the queue file 
!
      CALL ERR_PASS ( IUER, IER )
      CALL RD_TEXT ( OND%QUEUE_FILE, MBUF, BUF, NBUF, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 7252, IUER, 'MALO_SET_STATUS', 'Error in reading '// &
     &         'queue file '//OND%QUEUE_FILE )
           RETURN 
      END IF
!
! --- Set the status
!
      DO 410 J1=1,NBUF
         IF ( BUF(J1)(1:15) == QUE%DATE_ORIG ) THEN
              BUF(J1)(155:155) = STS
         END IF
 410  CONTINUE 
!
      CALL INCH ( GETPID(), TMPQUE_FILE(1:8) )
      CALL CHASHR         ( TMPQUE_FILE(1:8) )
      CALL BLANK_TO_ZERO  ( TMPQUE_FILE(1:8) )
      TMPQUE_FILE = OND%QUEUE_FILE(1:I_LEN(OND%QUEUE_FILE))//'_'//TMPQUE_FILE(1:8)
!
! --- Write down the status of the request in the temporary queue file
!
      CALL ERR_PASS ( IUER, IER )
      CALL WR_TEXT  ( NBUF, BUF, TMPQUE_FILE, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 7252, IUER, 'MALO_SET_STATUS', 'Error in writing '// &
     &         'into temporary file '//TMPQUE_FILE )
           RETURN 
      END IF
!
      CALL SYSTEM ( 'chmod g+wr   '//TRIM(TMPQUE_FILE)//CHAR(0) )
      CALL SYSTEM ( 'chgrp '//TRIM(OND%SERVER_GROUP)//' '//TRIM(TMPQUE_FILE)//CHAR(0) )
!
! --- ... and then rename the temporary queue file into the permanent queue file
!
      IR = RENAME ( TMPQUE_FILE(1:I_LEN(TMPQUE_FILE))//CHAR(0), &
     &              OND%QUEUE_FILE(1:I_LEN(OND%QUEUE_FILE))//CHAR(0) )
      IF ( IR .NE. 0 ) THEN
           CALL ERR_LOG ( 7253, IUER, 'MALO_SET_STATUS', 'Error in writing '// &
     &         'into queue file '//OND%QUEUE_FILE )
           RETURN 
      END IF
      CALL SYSTEM ( 'chmod g+wr   '//TRIM(OND%QUEUE_FILE)//CHAR(0) )
      CALL SYSTEM ( 'chgrp '//TRIM(OND%SERVER_GROUP)//' '//TRIM(OND%QUEUE_FILE)//CHAR(0) )
!
! --- Remove lock file
!
      IS = UNLINK ( OND%QUEUE_LOCK_FILE(1:I_LEN(OND%QUEUE_LOCK_FILE))//CHAR(0) )
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  MALO_SET_STATUS  !#!  
!
! ------------------------------------------------------------------------
!
      SUBROUTINE MALO_UPDATE_IND ( OND, QUE, STS, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine MALO_UPDATE_IND 
! *                                                                      *
! * ### 10-MAY-2013  MALO_UPDATE_IND  v1.0 (c) L. Petrov 10-MAY-2013 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'malo.i'
      TYPE     ( MALO_OND__TYPE ) :: OND
      TYPE     ( MALO_QUE__TYPE ) :: QUE
      INTEGER*4  IUER
      CHARACTER  STS*(*)
      INTEGER*4  MBUF
      PARAMETER  ( MBUF = 128 )
      CHARACTER  BUF(MBUF)*256, FILIND*128
      INTEGER*4  J1, J2, IER
      CHARACTER, EXTERNAL :: GET_CDATE*19
      INTEGER*4, EXTERNAL :: ILEN, I_LEN
!
      FILIND = OND%REQ_DIR(1:I_LEN(OND%REQ_DIR))//'/'//QUE%DATE_ORIG// &
     &         '/index.html'
!
      IF ( STS == 'W' ) THEN
           BUF(1) = '<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 3.2//EN">'
           BUF(2) = '<HTML><HEAD> <META HTTP-EQUIV="Content-Type" content="text/html; charset=iso-8859-1">'
           BUF(3) = '<META HTTP-EQUIV="refresh" CONTENT="5">'
           BUF(4) = '</HEAD><BODY>'
           BUF(5) = GET_CDATE()//' &nbsp;&nbsp; Process of computing loading '// &
     &             'time series is wating in the queue <BR>'
           BUF(6) = '</BODY></HTML>'
!
           CALL ERR_PASS ( IUER, IER )
           CALL WR_TEXT  ( 6, BUF, FILIND, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 7242, IUER, 'MALO_OND_CEHCK', 'Error in writing '// &
     &              'into index file '//FILIND )
                RETURN 
           END IF
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  MALO_UPDATE_IND  !#!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE MALO_SEND_EMAIL ( OND, QUE, STS, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  MALO_SEND_EMAIL
! *                                                                      *
! * ### 16-MAY-2013 MALO_SEND_EMAIL v1.2 (c) L. Petrov  21-SEP-2023  ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'malo.i'
      TYPE     ( MALO_OND__TYPE ) :: OND
      TYPE     ( MALO_QUE__TYPE ) :: QUE
      CHARACTER  STS*(*)
      INTEGER*4  IUER
      INTEGER*4  J1, NBUF, IR, IS, MB, NB, IER
      PARAMETER  ( MB = 128 )
      CHARACTER  SUBJECT*128, STR*128, COM_STR*1024, TMP_FIL*128, BUF(MB)*256
      CHARACTER, EXTERNAL :: GET_CDATE*19
      INTEGER*4, EXTERNAL :: ILEN, I_LEN, CLOSEDIR, GETPID, LINDEX, UNLINK
!
      IF ( QUE%EMAIL == 'n/a' ) THEN
           CALL ERR_LOG ( 0, IUER )
           RETURN 
      END IF
      IF ( STS == 'S' ) THEN
           SUBJECT = 'Mass loading time series are ready for download'
           BUF(1) = 'Dear colleague,'
           BUF(2) = ' '
           BUF(3) = 'Your request for mass loading time series have been computed.'
           BUF(4) = 'You can download the series at '// &
     &               OND%REQ_HTML_DIR(1:I_LEN(OND%REQ_HTML_DIR))//'/'// &
     &               QUE%DATE_ORIG
           BUF(5) = ' '
           BUF(6) = 'This is an automatically generated message. Do not reply.'
           BUF(7) = GET_CDATE()
           NB = 7
         ELSE IF ( STS == 'E' ) THEN
           SUBJECT = 'Computation of mass loading time series terminated '// &
     &               'with an error' 
           BUF(1) = 'Dear colleague,'
           BUF(2) = ' '
           BUF(3) = 'Unfortunatly, your request for computing mass loading '// &
     &              'time series has terminated due to an error.'
           BUF(4) = 'You may see the error log at '// &
     &               OND%REQ_HTML_DIR(1:I_LEN(OND%REQ_HTML_DIR))//'/'// &
     &               QUE%DATE_ORIG
           BUF(5) = ' '
           BUF(6) = 'Sorry for inconvenience.'
           BUF(7) = 'This is an automatically generated message. Do not reply.'
           BUF(8) = GET_CDATE()
           NB = 8
      END IF
!
      TMP_FIL = OND%REQ_DIR(1:I_LEN(OND%REQ_DIR))//'/'//QUE%DATE_ORIG//'/email.txt'
!
! --- Write down a file with email
!
      CALL ERR_PASS ( IUER, IER )
      CALL WR_TEXT  ( NB, BUF, TMP_FIL, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 7261, IUER, 'MALO_SEND_EMAIL', 'Error in writing '// &
     &         'temporary file with email message '//TMP_FIL )
           RETURN 
      END IF
!
      COM_STR = 'mailx -s  "'//TRIM(SUBJECT)// &
     &          '" -r '//TRIM(OND%FROM_EMAIL)// &
     &          ' '//TRIM(QUE%EMAIL)// &
     &          ' < '//TMP_FIL
      IS = SYSTEM ( TRIM(COM_STR)//CHAR(0) )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 7262, IUER, 'MALO_SEND_EMAIL', 'Error in an '// &
     &         'attempt to send email' )
           RETURN 
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  MALO_SEND_EMAIL  !#!#
