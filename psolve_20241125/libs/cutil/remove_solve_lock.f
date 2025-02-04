      SUBROUTINE REMOVE_SOLVE_LOCK()
! ************************************************************************
! *                                                                      *
! *   Routine REMOVE_SOLVE_LOCK removves Solve lock file.                *
! *                                                                      *
! * ### 28-APR-2007 REMOVE_SOLVE_LOCK v1.0 (c) L. Petrov 17-MAY-2007 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'solve.i'
      INCLUDE   'precm.i'
      LOGICAL*4  FL_EXIST
      CHARACTER  LOCK_FILE*128 
      INTEGER*4, EXTERNAL :: ILEN, I_LEN 
!
      LOCK_FILE = PRE_SCR_DIR(1:PRE_SD_LEN)//'LOCK'//PRE_LETRS
      INQUIRE ( FILE=LOCK_FILE, EXIST=FL_EXIST )
      IF ( FL_EXIST ) THEN
           CALL UNLINK ( LOCK_FILE(1:I_LEN(LOCK_FILE))//CHAR(0) )
      END IF
      RETURN
      END  SUBROUTINE  REMOVE_SOLVE_LOCK  !#!  
