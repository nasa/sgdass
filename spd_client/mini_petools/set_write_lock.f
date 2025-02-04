      SUBROUTINE SET_WRITE_LOCK ( IO_LOCK_FILE, READ_LOCK_FILE, &
     &                            WRITE_LOCK_FILE, WAIT_TIME, &
     &                            FD_READ_LOCK, FD_WRITE_LOCK, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine SET_WRITE_LOCK waits for the read lock to be cleared and   *
! *   then sets write lock to allow the process to read data without     *
! *   a danger of being overwritten during reading.                      *
! *                                                                      *
! *   Implementation:                                                    *
! *     a) set exclusive  lock on IO_LOCK_FILE                           *
! *     b) set shared     lock on READ_LOCK_FILE                         *
! *     c) set exclusive  lock on WRITE_LOCK_FILE                        *
! *     d) lift exclusive lock on IO_LOCK_FILE                           *
! *                                                                      *
! * ________________________ Input parameters: _________________________ *
! *                                                                      *
! * IO_LOCK_FILE    ( CHARACTER ) -- Name of the IO lock file.           *
! * READ_LOCK_FILE  ( CHARACTER ) -- Name of the read lock file.         *
! * WRITE_LOCK_FILE ( CHARACTER ) -- Name of the write lock file.        *
! * WAIT_TIME       ( REAL*8    ) -- Waiting time for check the read     *
! *                                  lock be cleared. If the read lock   *
! *                                  is not cleared after that time,     *
! *                                  the read lock is considered         *
! *                                  abandoned. Then the read lock is    *
! *                                  ignored.                            *
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
! *                                                                      *
! *  FD_READ_LOCK   ( INTEGER*4 ) -- File descriptor for the read lock.  *
! *  FD_WRITE_LOCK  ( INTEGER*4 ) -- File descriptor for the write lock. *
! *  IUER ( INTEGER*4, OPT ) -- Universal error handler.                 *
! *                           Input: switch IUER=0 -- no error messages  *
! *                                  will be generated even in the case  *
! *                                  of error. IUER=-1 -- in the case of *
! *                                  error the message will be put on    *
! *                                  stdout.                             *
! *                           Output: 0 in the case of successful        *
! *                                   completion and non-zero in the     *
! *                                   case of error.                     *
! *                                                                      *
! *  ### 16-OCT-2019 SET_WRITE_LOCK v1.1 (c)  L. Petrov  28-APR-2020 ### *
! *                                                                      *
! ************************************************************************
#ifdef ADR_32BIT
#define  FUNC_OPEN OPEN64
#else
#define  FUNC_OPEN OPEN
#endif
      IMPLICIT   NONE 
      CHARACTER  IO_LOCK_FILE*(*), READ_LOCK_FILE*(*), WRITE_LOCK_FILE*(*)
      INTEGER*4  FD_READ_LOCK, FD_WRITE_LOCK, IUER
      REAL*8     WAIT_TIME
      INTEGER*4  DATE_NOW, DATE_WRT_LCK, STAT_BLOCK(16), IS, IK, &
     &           IOS, PID, J1, NUM_READ_FAILURE, IER
      INTEGER*8  IS8
      INTEGER*4  O_RDONLY_FLAG, O_CREAT_FLAG, O_RDWR_FLAG, O_EXCL_FLAG, &
     &           EDQUOT, EEXIST, ENOENT, EINTR, OPEN_READ_FLAGS, OPEN_WRITE_FLAGS, &
     &           MODE_FLAGS, OLD_UMASK, ITIMER_REAL, SIGALRM, LOCK_EX, LOCK_SH, &
     &           LOCK_NB, LOCK_UN, LOCK_SHARED
      INTEGER*4  FD_IO_LOCK, ARG_LEN
      INTEGER*2  MODE_1, MODE_2, MODE_3, MODE_4, MODE_5, MODE_6, MODE_7
      REAL*8     TIM_REMAINED
      CHARACTER  STR*128
      LOGICAL*1  LEX
      INTEGER*4, INTRINSIC :: IERRNO
      TYPE       ITIMERVAL_TYPE
          INTEGER*8  ALARM_SEC
          INTEGER*8  ALARM_MICROSEC
          INTEGER*8  OLD_ALARM_SEC
          INTEGER*8  OLD_ALARM_MICROSEC
      END TYPE   ITIMERVAL_TYPE
      TYPE ( ITIMERVAL_TYPE ) :: TIMER
      ADDRESS__TYPE :: SAVED_ALARM_SIGNAL, OLD_SIG_HANDLER
      INTEGER*4  IPAR
      COMMON   / PETOOLS_SIGNAL_HANDLER / SAVED_ALARM_SIGNAL, IPAR
!
      INTEGER*4, EXTERNAL :: CLOSE, GETPID, GET_UNIT, FLOCK, FOR_STAT, &
     &                       I_LEN, ILEN, KILL, READ, SETITIMER, TIME, &
     &                       UMASK, WRITE
      ADDRESS__TYPE, EXTERNAL :: FUNC_OPEN, PETOOLS_SET_ARARM_HANDLER, &
     &                           PETOOLS_VOID_HANDLER
      CHARACTER, EXTERNAL :: GET_CDATE*19
!
      CALL GET_SYSTEM_CONSTANT ( 'O_RDONLY',    O_RDONLY_FLAG, ARG_LEN )
      CALL GET_SYSTEM_CONSTANT ( 'O_CREAT',     O_CREAT_FLAG,  ARG_LEN )
      CALL GET_SYSTEM_CONSTANT ( 'O_RDWR',      O_RDWR_FLAG,   ARG_LEN )
      CALL GET_SYSTEM_CONSTANT ( 'O_EXCL',      O_EXCL_FLAG,   ARG_LEN )
      CALL GET_SYSTEM_CONSTANT ( 'LOCK_EX',     LOCK_EX,       ARG_LEN )
      CALL GET_SYSTEM_CONSTANT ( 'LOCK_SH',     LOCK_SH,       ARG_LEN )
      CALL GET_SYSTEM_CONSTANT ( 'LOCK_UN',     LOCK_UN,       ARG_LEN )
      CALL GET_SYSTEM_CONSTANT ( 'LOCK_NB',     LOCK_NB,       ARG_LEN )
      CALL GET_SYSTEM_CONSTANT ( 'ITIMER_REAL', ITIMER_REAL,   ARG_LEN )
      CALL GET_SYSTEM_CONSTANT ( 'EEXIST',      EEXIST,        ARG_LEN )
      CALL GET_SYSTEM_CONSTANT ( 'EDQUOT',      EDQUOT,        ARG_LEN )
      CALL GET_SYSTEM_CONSTANT ( 'ENOENT',      ENOENT,        ARG_LEN )
      CALL GET_SYSTEM_CONSTANT ( 'EINTR',       EINTR,         ARG_LEN )
!
      CALL GET_SYSTEM_CONSTANT ( 'S_IRUSR',  MODE_1, ARG_LEN )
      CALL GET_SYSTEM_CONSTANT ( 'S_IWUSR',  MODE_2, ARG_LEN )
      CALL GET_SYSTEM_CONSTANT ( 'S_IRGRP',  MODE_3, ARG_LEN )
      CALL GET_SYSTEM_CONSTANT ( 'S_IWGRP',  MODE_4, ARG_LEN )
      CALL GET_SYSTEM_CONSTANT ( 'S_IROTH',  MODE_5, ARG_LEN )
      CALL GET_SYSTEM_CONSTANT ( 'S_IWOTH',  MODE_6, ARG_LEN )
      CALL GET_SYSTEM_CONSTANT ( 'S_ISGID',  MODE_7, ARG_LEN )
!
      OPEN_READ_FLAGS = O_RDONLY_FLAG
#ifdef GNU
      OPEN_WRITE_FLAGS = O_CREAT_FLAG + O_EXCL_FLAG + O_RDWR_FLAG
      MODE_FLAGS  = MODE_1 + MODE_2 + MODE_3 + MODE_4 + MODE_5 + MODE_6 + MODE_7
      LOCK_SHARED = LOCK_SH + LOCK_NB
#else
      OPEN_WRITE_FLAGS = O_CREAT_FLAG .OR O_EXCL_FLAG .OR. O_RDWR_FLAG
      MODE_FLAGS  = MODE_1 .OR. MODE_2 .OR. MODE_3 .OR. MODE_4 .OR. MODE_5 .OR. MODE_6 .OR. MODE_7
      LOCK_SHARED = LOCK_SH .OR. LOCK_NB
#endif
!
! --- Try to open the io lock file in the excluive creation mode
!     ==========================================================
!
      OLD_UMASK = UMASK ( %VAL(0) )
      FD_IO_LOCK = FUNC_OPEN ( TRIM(IO_LOCK_FILE)//CHAR(0), %VAL(OPEN_WRITE_FLAGS), %VAL(MODE_FLAGS) )
      IS = UMASK ( %VAL(OLD_UMASK) )
!
! --- Copy the system call error code to IOS
!
      IF ( FD_IO_LOCK < 0 ) THEN
           IOS = IERRNO()
         ELSE 
           IOS = 0
      END IF
      IF ( FD_IO_LOCK < 0 .AND. IOS .EQ. EEXIST  ) THEN
!
! -------- Aga, we cannot create the io lock file because it exists. 
! -------- Then open it in core reading
!
           FD_IO_LOCK = FUNC_OPEN ( TRIM(IO_LOCK_FILE)//CHAR(0), %VAL(OPEN_READ_FLAGS), %VAL(MODE_FLAGS) )
           IF ( FD_IO_LOCK < 0 ) THEN
                CALL CLRCH ( STR )
                CALL GERROR ( STR )
                CALL ERR_LOG ( 1011, IUER, 'SET_WRITE_LOCK', 'Error '// &
     &               TRIM(STR)//' in an attempt to open for reading the '// &
     &              'io lock file '//IO_LOCK_FILE )
                IS = FLOCK ( %VAL(FD_IO_LOCK), %VAL(LOCK_UN) )
                IS = CLOSE ( %VAL(FD_IO_LOCK) )
                RETURN
           END IF
        ELSE IF ( FD_IO_LOCK < 0 .AND. IOS .NE. EEXIST  ) THEN
           CALL CLRCH  ( STR )
           CALL GERROR ( STR )
           IS = FLOCK ( %VAL(FD_IO_LOCK), %VAL(LOCK_UN) )
           IS = CLOSE ( %VAL(FD_IO_LOCK) )
           CALL ERR_LOG ( 1012, IUER, 'SET_WRITE_LOCK', 'Error '//TRIM(STR)// &
     &          ' in an attempt to create the io lock file '//IO_LOCK_FILE )
           RETURN
      END IF
!
! --- Set timer that will through ALARM signal after WAIT_TIME
!
      TIMER%ALARM_SEC          = IDINT(WAIT_TIME)
      TIMER%ALARM_MICROSEC     = IDINT(1000000.0D0*(WAIT_TIME - IDINT(WAIT_TIME)))
      TIMER%OLD_ALARM_SEC      = IDINT(WAIT_TIME)
      TIMER%OLD_ALARM_MICROSEC = IDINT(1000000.0D0*(WAIT_TIME - IDINT(WAIT_TIME)))
!
      OLD_SIG_HANDLER = PETOOLS_SET_ARARM_HANDLER( %VAL(0) )
      IS = SETITIMER ( %VAL(ITIMER_REAL), TIMER, TIMER )
!
! --- Set exclusive blocking lock for the io lock file. Though the lock will expire when
! --- the ALARM signal will be triggered
!
      IS = FLOCK   ( %VAL(FD_IO_LOCK), %VAL(LOCK_EX) )
!
! --- Restore the alarm handler to the previous state
!
#ifdef LINUX
      IS8 = PETOOLS_SET_ARARM_HANDLER( %VAL(OLD_SIG_HANDLER) )
#else
      IS8 = PETOOLS_SET_ARARM_HANDLER( PETOOLS_VOID_HANDLER )
      CALL  PETOOLS_DEACTIVATE_ALARM_HANDLER()
#endif
      IS = SETITIMER ( %VAL(ITIMER_REAL), %VAL(0), %VAL(0) )
      IF ( IS < 0 ) THEN
           CALL CLRCH  ( STR )
           CALL GERROR ( STR )
           CALL ERR_LOG ( 1013, IUER, 'SET_WRITE_LOCK', 'Error '//TRIM(STR)// &
     &          ' in an attempt to set exclusive lock to the io lock file '// &
     &          IO_LOCK_FILE )
           IS = FLOCK ( %VAL(FD_IO_LOCK), %VAL(LOCK_UN) )
           IS = CLOSE ( %VAL(FD_IO_LOCK) )
           RETURN
      END IF
!
! --- Try to open the read lock file in the excluive creation mode
!     ============================================================
!
      OLD_UMASK = UMASK ( %VAL(0) )
      FD_READ_LOCK = FUNC_OPEN ( TRIM(READ_LOCK_FILE)//CHAR(0), %VAL(OPEN_WRITE_FLAGS), %VAL(MODE_FLAGS) )
      IS = UMASK ( %VAL(OLD_UMASK) )
!
! --- Copy the system call error code to IOS
!
      IF ( FD_READ_LOCK < 0 ) THEN
           IOS = IERRNO()
         ELSE 
           IOS = 0
      END IF
      IF ( FD_READ_LOCK < 0 .AND. IOS .EQ. EEXIST  ) THEN
!
! -------- Aga, we cannot create the read lock file because it exists. 
! -------- Then open it in core reading
!
           FD_READ_LOCK = FUNC_OPEN ( TRIM(READ_LOCK_FILE)//CHAR(0), %VAL(OPEN_READ_FLAGS), %VAL(MODE_FLAGS) )
           IF ( FD_READ_LOCK < 0 ) THEN
                CALL CLRCH ( STR )
                CALL GERROR ( STR )
                CALL ERR_LOG ( 1014, IUER, 'SET_WRITE_LOCK', 'Error '// &
     &               TRIM(STR)//' in an attempt to open for reading the '// &
     &              'read lock file '//READ_LOCK_FILE )
                IS = FLOCK ( %VAL(FD_IO_LOCK), %VAL(LOCK_UN) )
                IS = CLOSE ( %VAL(FD_IO_LOCK) )
                RETURN
           END IF
        ELSE IF ( FD_READ_LOCK < 0 .AND. IOS .NE. EEXIST  ) THEN
           CALL CLRCH  ( STR )
           CALL GERROR ( STR )
           IS = FLOCK ( %VAL(FD_IO_LOCK), %VAL(LOCK_UN) )
           IS = CLOSE ( %VAL(FD_IO_LOCK) )
           CALL ERR_LOG ( 1015, IUER, 'SET_WRITE_LOCK', 'Error '//TRIM(STR)// &
     &          ' in an attempt to create the read lock file '//READ_LOCK_FILE )
           RETURN
      END IF
!
! --- Set shared non-blocking lock for the read lock file
!
      IS = FLOCK   ( %VAL(FD_READ_LOCK), %VAL(LOCK_SHARED) )
!
! --- Try to open the write lock file in the excluive creation mode
!     =============================================================
!
      OLD_UMASK = UMASK ( %VAL(0) )
      FD_WRITE_LOCK = FUNC_OPEN ( TRIM(WRITE_LOCK_FILE)//CHAR(0), %VAL(OPEN_WRITE_FLAGS), %VAL(MODE_FLAGS) )
      IS = UMASK ( %VAL(OLD_UMASK) )
!
! --- Copy the system call error code to IOS
!
      IF ( FD_WRITE_LOCK < 0 ) THEN
           IOS = IERRNO()
         ELSE 
           IOS = 0
      END IF
      IF ( FD_WRITE_LOCK < 0 .AND. IOS .EQ. EEXIST  ) THEN
!
! -------- Aga, we cannot create the WRITE lock file because it exists. 
! -------- Then open it in core reading
!
           FD_WRITE_LOCK = FUNC_OPEN ( TRIM(WRITE_LOCK_FILE)//CHAR(0), %VAL(OPEN_READ_FLAGS), %VAL(MODE_FLAGS) )
           IF ( FD_WRITE_LOCK < 0 ) THEN
                CALL CLRCH ( STR )
                CALL GERROR ( STR )
                CALL ERR_LOG ( 1016, IUER, 'SET_WRITE_LOCK', 'Error '// &
     &               TRIM(STR)//' in an attempt to open for reading the '// &
     &              'write lock file '//WRITE_LOCK_FILE )
                IS = FLOCK ( %VAL(FD_READ_LOCK), %VAL(LOCK_UN) )
                IS = FLOCK ( %VAL(FD_IO_LOCK),   %VAL(LOCK_UN) )
                IS = CLOSE ( %VAL(FD_READ_LOCK) )
                IS = CLOSE ( %VAL(FD_IO_LOCK) )
                RETURN
           END IF
        ELSE IF ( FD_WRITE_LOCK < 0 .AND. IOS .NE. EEXIST  ) THEN
           CALL CLRCH  ( STR )
           CALL GERROR ( STR )
           IS = FLOCK ( %VAL(FD_READ_LOCK), %VAL(LOCK_UN) )
           IS = FLOCK ( %VAL(FD_IO_LOCK), %VAL(LOCK_UN) )
           IS = CLOSE ( %VAL(FD_READ_LOCK) )
           IS = CLOSE ( %VAL(FD_IO_LOCK) )
           CALL ERR_LOG ( 1017, IUER, 'SET_WRITE_LOCK', 'Error '//TRIM(STR)// &
     &          ' in an attempt to create the WRITE lock file '//WRITE_LOCK_FILE )
           RETURN
      END IF
!
      OLD_SIG_HANDLER = PETOOLS_SET_ARARM_HANDLER( %VAL(0) )
      TIMER%ALARM_SEC          = IDINT(WAIT_TIME)
      TIMER%ALARM_MICROSEC     = IDINT(1000000.0D0*(WAIT_TIME - IDINT(WAIT_TIME)))
      TIMER%OLD_ALARM_SEC      = IDINT(WAIT_TIME)
      TIMER%OLD_ALARM_MICROSEC = IDINT(1000000.0D0*(WAIT_TIME - IDINT(WAIT_TIME)))
      IS = SETITIMER ( %VAL(ITIMER_REAL), TIMER, TIMER )
!
! --- Set exclusive blocking lock for the write lock file. Though the lock will expire when
! --- the ALARM signal will be triggered
!
      IS = FLOCK   ( %VAL(FD_WRITE_LOCK), %VAL(LOCK_EX) )
!
! --- Restore the alarm handler to the previous state
!
#ifdef LINUX
      IS8 = PETOOLS_SET_ARARM_HANDLER( %VAL(OLD_SIG_HANDLER) )
#else
      IS8 = PETOOLS_SET_ARARM_HANDLER( PETOOLS_VOID_HANDLER )
      CALL  PETOOLS_DEACTIVATE_ALARM_HANDLER()
#endif
      IS = SETITIMER ( %VAL(ITIMER_REAL), %VAL(0), %VAL(0) )
      IF ( IS < 0 ) THEN
           CALL CLRCH  ( STR )
           CALL GERROR ( STR )
           CALL ERR_LOG ( 1018, IUER, 'SET_WRITE_LOCK', 'Error '//TRIM(STR)// &
     &          ' in an attempt to set exclusive lock to the write lock file '// &
     &          WRITE_LOCK_FILE )
           IS = FLOCK ( %VAL(FD_READ_LOCK),  %VAL(LOCK_UN) )
           IS = FLOCK ( %VAL(FD_WRITE_LOCK), %VAL(LOCK_UN) )
           IS = FLOCK ( %VAL(FD_IO_LOCK),    %VAL(LOCK_UN) )
           IS = CLOSE ( %VAL(FD_READ_LOCK) )
           IS = CLOSE ( %VAL(FD_WRITE_LOCK) )
           IS = CLOSE ( %VAL(FD_IO_LOCK) )
           RETURN
      END IF
!
! --- Close the master io lock file
!
      IS = CLOSE ( %VAL(FD_IO_LOCK) )
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  SET_WRITE_LOCK  !#!  
!
! ------------------------------------------------------------------------
!
      SUBROUTINE LIFT_READ_WRITE_LOCKS ( FD_READ_LOCK, FD_WRITE_LOCK )
! ************************************************************************
! *                                                                      *
! *   Routine LIFT_READ_WRITE_LOCKS lifts the read and write locks set   *
! *   by either SET_READ_LOCK or SET_WRITE_LOCK.                         *
! *                                                                      *
! * ________________________ Input parameters: _________________________ *
! *                                                                      *
! *  FD_READ_LOCK   ( INTEGER*4 ) -- File descriptor for the read lock.  *
! *  FD_WRITE_LOCK  ( INTEGER*4 ) -- File descriptor for the write lock. *
! *                                                                      *
! * ### 18-OCT-2019 LIFT_WRITE_LOCK  v1.0 (c)  L. Petrov 18-OCT-2019 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INTEGER*4  FD_READ_LOCK, FD_WRITE_LOCK
      INTEGER*4  IS, LOCK_UN, ARG_LEN
      INTEGER*4, EXTERNAL :: CLOSE, FLOCK
!
      CALL GET_SYSTEM_CONSTANT ( 'LOCK_UN', LOCK_UN, ARG_LEN )
      IS = FLOCK ( %VAL(FD_WRITE_LOCK), %VAL(LOCK_UN) )
      IS = FLOCK ( %VAL(FD_READ_LOCK),  %VAL(LOCK_UN) )
      IS = CLOSE ( %VAL(FD_WRITE_LOCK) )
      IS = CLOSE ( %VAL(FD_READ_LOCK)  )
      RETURN
      END  SUBROUTINE  LIFT_READ_WRITE_LOCKS  !#!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE LIFT_READ_LOCK ( FD_READ_LOCK )
! ************************************************************************
! *                                                                      *
! *   Routine LIFT_READ_LOCK lifts the read lock set by SET_READ_LOCK.   *
! *                                                                      *
! * ________________________ Input parameters: _________________________ *
! *                                                                      *
! *  FD_READ_LOCK   ( INTEGER*4 ) -- File descriptor for the read lock.  *
! *                                                                      *
! * ### 18-OCT-2019 LIFT_READ_LOCK  v1.0 (c)  L. Petrov 18-JUL-2021 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INTEGER*4  FD_READ_LOCK
      INTEGER*4  IS, LOCK_UN, ARG_LEN
      INTEGER*4, EXTERNAL :: CLOSE, FLOCK
!
      CALL GET_SYSTEM_CONSTANT ( 'LOCK_UN', LOCK_UN, ARG_LEN )
      IS = FLOCK ( %VAL(FD_READ_LOCK),  %VAL(LOCK_UN) )
      IS = CLOSE ( %VAL(FD_READ_LOCK)  )
      RETURN
      END  SUBROUTINE  LIFT_READ_LOCK  !#!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE LIFT_WRITE_LOCK ( FD_WRITE_LOCK )
! ************************************************************************
! *                                                                      *
! *   Routine LIFT_WRITE_LOCK lifts the read lock set by SET_WRITE_LOCK. *
! *                                                                      *
! * ________________________ Input parameters: _________________________ *
! *                                                                      *
! *  FD_WRITE_LOCK   ( INTEGER*4 ) -- File descriptor for the read lock. *
! *                                                                      *
! * ### 18-OCT-2019 LIFT_WRITE_LOCK  v1.0 (c)  L. Petrov 18-JUL-2021 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INTEGER*4  FD_WRITE_LOCK
      INTEGER*4  IS, LOCK_UN, ARG_LEN
      INTEGER*4, EXTERNAL :: CLOSE, FLOCK
!
      CALL GET_SYSTEM_CONSTANT ( 'LOCK_UN', LOCK_UN, ARG_LEN )
      IS = FLOCK ( %VAL(FD_WRITE_LOCK),  %VAL(LOCK_UN) )
      IS = CLOSE ( %VAL(FD_WRITE_LOCK)  )
      RETURN
      END  SUBROUTINE  LIFT_WRITE_LOCK  !#!#
