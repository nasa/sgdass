      SUBROUTINE SOLVE_LOCK()
! ************************************************************************
! *                                                                      *
! *   Routine SOLVE_LOCK
! *                                                                      *
! *  ### 28-APR-2007   SOLVE_LOCK  v1.0 (c)  L. Petrov  17-MAY-2007 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'solve.i'
      INCLUDE   'precm.i'
      LOGICAL*4  FL_EXIST
      CHARACTER  LOCK_FILE*128
      CHARACTER  USER_NAME*128, USER_REALNAME*128, USER_E_ADDRESS*128, &
     &           PROC_NAME*128, DATE_CHR*19, STR*80
      INTEGER*4  IP, LUN, PID, IOS
      CHARACTER, EXTERNAL :: GET_CDATE*19
      INTEGER*4, EXTERNAL :: GETPID, GET_UNIT, ILEN, I_LEN, LINDEX
!
      LOCK_FILE = PRE_SCR_DIR(1:PRE_SD_LEN)//'LOCK'//PRE_LETRS
!
      INQUIRE ( FILE=LOCK_FILE, EXIST=FL_EXIST )
      IF ( FL_EXIST ) THEN
           CALL UNLINK ( LOCK_FILE(1:I_LEN(LOCK_FILE))//CHAR(0) )
      END IF
!
      CALL GETINFO_USER ( USER_NAME, USER_REALNAME, USER_E_ADDRESS )
      PID = GETPID()
!
      CALL GETARG ( 0, PROC_NAME )
      IP = LINDEX ( PROC_NAME, '/' )
      IF ( IP > 0 ) THEN
           CALL CLRCH  ( PROC_NAME(1:IP) )
           CALL CHASHL ( PROC_NAME       )
      END IF
!
      DATE_CHR = GET_CDATE()
      LUN = GET_UNIT()
      OPEN ( UNIT=LUN, FILE=LOCK_FILE, STATUS='UNKNOWN', IOSTAT=IOS )
      IF ( IOS .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( IOS, STR ) 
           CALL ERR_LOG ( 1701, -2, 'SOLVE_LOCK', 'Error '// &
     &          STR(1:I_LEN(STR))//' in an attempt to open lock file '// &
     &          LOCK_FILE )
           CALL EXIT ( 1 )
      END IF
!
      WRITE ( UNIT=LUN, FMT=110, IOSTAT=IOS ) PROC_NAME(1:I_LEN(PROC_NAME)), &
     &                        USER_REALNAME(1:I_LEN(USER_REALNAME)), &
     &                        PID, DATE_CHR
 110  FORMAT ( A, 2X, 'locked by ', A, 2X, 'proc ID ', I10, '  since ', A )
      IF ( IOS .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( IOS, STR ) 
           CALL ERR_LOG ( 1701, -2, 'SOLVE_LOCK', 'Error '// &
     &          STR(1:I_LEN(STR))//' in an attempt to write lock file '// &
     &          LOCK_FILE )
           CALL EXIT ( 1 )
      END IF
      CLOSE ( UNIT=LUN )
!
! --- Set permissions which allows other users to overwrite this file
!
      CALL SYSTEM ( 'chmod u+rw,g+rw,o+rw '// &
     &               LOCK_FILE(1:I_LEN(LOCK_FILE))//CHAR(0) )
!
      RETURN
      END  SUBROUTINE  SOLVE_LOCK  !#!  
