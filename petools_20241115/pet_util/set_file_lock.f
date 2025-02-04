      SUBROUTINE SET_FILE_LOCK ( FILE_LOCK, POLL_INT, MAX_INT, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine SET_FILE_LOCK first check the lock file. If the lock file  *
! *   exist and the time stamp is not older than MAX_INT seconds, then   *
! *   it waits for either file disappearance of for lock expiration.     *
! *   It check the file status every POLL_INT seconds.                   *
! *                                                                      *
! *   Then it creates the lock file. It put there                        *
! *   1) Time stamp in Unis format with milliscond resolution;           *
! *   2) Time in human readable form in ISO format;                      *
! *   3) User name;                                                      *
! *   4) Name of the excetabe without path.                              *
! *                                                                      *
! *   NB: POLL_INT should not be less than 0.001 second, since the       *
! *   timer has millisecond resolution.                                  *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *  FILE_LOCK ( CHARACTER ) -- Name of the lock file.                   *
! *   POLL_INT ( REAL*8    ) -- Interval time between the the lock       *
! *                             file status.                             *
! *     MAX_IT ( REAL*8    ) -- Maximal age of the lock file.            *
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
! * ### 19-JUL-2007  SET_FILE_LOCK  v2.0  (c)  L. Petrov 15-MAY-2013 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      CHARACTER  FILE_LOCK*(*)
      REAL*8     POLL_INT, MAX_INT
      INTEGER*4  IUER
      LOGICAL*4  LEX
      INTEGER*4  IS, UNIX_DATE, NOW_DATE
      INTEGER*8  SIZE_I8
!
      CHARACTER  OUT*256, PROC_NAME*256, STR*256, &
     &           TIM_FIL_STR*23, TIM_NOW_STR*23
      CHARACTER  USER_NAME*128, USER_REALNAME*128, USER_E_ADDRESS*128
      REAL*8     TIM_DIF, SEC_NOW, SEC_FIL
      INTEGER*4  IP, IOS, LUN, PID, IW, J1, MJD_NOW, MJD_FIL, IER
      CHARACTER, EXTERNAL :: GET_CDATE_MS*23
      INTEGER*4, EXTERNAL :: ILEN, I_LEN, FTIME, LINDEX, GET_UNIT, GETPID
!
      LUN = GET_UNIT()
 910  CONTINUE 
!
! --- Check whether the lock file exists
!
      INQUIRE ( FILE=FILE_LOCK, EXIST=LEX )
      IF ( LEX ) THEN
!
! -------- Exists. Open it ...
!
           OPEN ( UNIT=LUN, FILE=FILE_LOCK, STATUS='OLD', IOSTAT=IOS )
           IF ( IOS .NE. 0 ) THEN
                CALL CLRCH ( STR )
                CALL INCH  ( IOS, STR ) 
                CALL ERR_LOG ( 1701, IUER, 'SET_FILE_LOCK', 'Error '// &
     &               STR(1:I_LEN(STR))//' in an attempt to open for read '// &
     &              'lock file '//FILE_LOCK )
                RETURN 
           END IF 
!
! -------- Read it ...
!
           READ ( UNIT=LUN, FMT='(A)', IOSTAT=IOS ) TIM_FIL_STR
           IF ( IOS .NE. 0 ) THEN
                CALL CLRCH ( STR )
                CALL INCH  ( IOS, STR ) 
                CALL ERR_LOG ( 1702, IUER, 'SET_FILE_LOCK', 'Error '// &
     &               STR(1:I_LEN(STR))//' in an attempt to read the '// &
     &              'lock file '//FILE_LOCK )
                RETURN 
           END IF
!
! -------- Close it ...
!
           CLOSE ( LUN ) 
!
! -------- ... and parse the time stamp
!
           IER = -1
           CALL DATE_TO_TIME ( TIM_FIL_STR, MJD_FIL, SEC_FIL, IER )
!
! -------- Infinite loop
!
           IW = 0
           DO 410 J1=1,1024*1024*1024
!
! ----------- Get the current time
!
              TIM_NOW_STR = GET_CDATE_MS()
              CALL DATE_TO_TIME ( TIM_NOW_STR, MJD_NOW, SEC_NOW, IER )
!
! ----------- Check how old the file is
!
              TIM_DIF = (MJD_NOW - MJD_FIL)*86400.0D0 + (SEC_NOW - SEC_FIL) 
              IF ( TIM_DIF > MAX_INT ) THEN
!
! ---------------- Remove stale file
!
                   CALL UNLINK ( FILE_LOCK(1:I_LEN(FILE_LOCK))//CHAR(0) )
                   GOTO 810
              END IF
!
! ----------- fole is not old enough. Wait
!
              IW = IW + 1
              CALL LIB$WAIT ( POLL_INT )
!
! ----------- Check: whether it still exists
!
              INQUIRE ( FILE=FILE_LOCK, EXIST=LEX )
              IF ( .NOT. LEX ) GOTO 820
 410       CONTINUE 
 810       CONTINUE 
!
! -------- OK. File disappered or it is too old. 
!
           IF ( IW > 0 ) GOTO 910 ! since we have waited for mor than one !
!                                 ! poll period, let us check the file once more
 820       CONTINUE 
      END IF
!
! --- Prepare the output record
!
! --- Get the current time
!
      TIM_NOW_STR = GET_CDATE_MS()
!
! --- Get PID
!
      PID = GETPID()
!
! --- Get process name
!
      CALL GETARG ( 0, STR )
      IP = LINDEX ( STR, '/' ) 
      PROC_NAME = STR(IP+1:)
!
! --- Get user name
!
      CALL GETINFO_USER ( USER_NAME, USER_REALNAME, USER_E_ADDRESS )
!
! --- format the output line
!
      WRITE ( UNIT=OUT, FMT=120 ) TIM_NOW_STR, PID, USER_NAME(1:16), &
     &                            PROC_NAME(1:I_LEN(PROC_NAME))
 120  FORMAT ( A23, 1X, I5, 1X, A, 1X, A )
!
! --- Check the file status once again
!
      INQUIRE ( FILE=FILE_LOCK, EXIST=LEX )
      IF ( LEX ) GOTO 910 ! It has appeared again??? Oh, no, let us wait...
!
! --- Open the output lock file
!
      OPEN ( UNIT=LUN, FILE=FILE_LOCK, STATUS='UNKNOWN', IOSTAT=IOS )
      IF ( IOS .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( IOS, STR ) 
           CALL ERR_LOG ( 1703, IUER, 'SET_FILE_LOCK', 'Error '// &
     &          STR(1:I_LEN(STR))//' in an attempt to open for write '// &
     &         'lock file '//FILE_LOCK )
           RETURN 
      END IF
!
! --- Write the output line
!
      WRITE ( UNIT=LUN, FMT='(A)', IOSTAT=IOS ) OUT(1:I_LEN(OUT))
      IF ( IOS .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( IOS, STR ) 
           CALL ERR_LOG ( 1704, IUER, 'SET_FILE_LOCK', 'Error '// &
     &          STR(1:I_LEN(STR))//' in an attempt to write in the '// &
     &         'lock file '//FILE_LOCK )
           RETURN 
      END IF
      CLOSE ( UNIT=LUN )
!
! --- Flush the file
!
      CALL FLUSH ( LUN )
!
! --- Set permissions which allows other users to overwrite this file
!
      CALL SYSTEM ( 'chmod u+rw,g+rw,o+rw '// &
     &               FILE_LOCK(1:I_LEN(FILE_LOCK))//CHAR(0) )
!      
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  SET_FILE_LOCK  !#!  
