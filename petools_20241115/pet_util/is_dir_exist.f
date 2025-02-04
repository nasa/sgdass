      FUNCTION IS_DIR_EXIST ( DIRNAM, ERRSTR )
! ************************************************************************
! *                                                                      *
! *   Routine IS_DIR_EXIST checks whether a directory specified in the   *
! *   first argument exist. If it does it recuirds .TRUE. If it does     *
! *   not, it returns .FALSE. and puts the reason in ERRSTR.             *
! *                                                                      *
! *  ### 29-MAR-2023  IS_DIR_EXIST v1.0 (c)  L. Petrov  29-MAR-2023 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      LOGICAL*1  IS_DIR_EXIST 
      CHARACTER  DIRNAM*(*), ERRSTR*(*)
#ifdef DARWIN
#      define   FUNC_OPENDIR  OPENDIR$INODE64
#else
#      define   FUNC_OPENDIR  OPENDIR
#endif
      ADDRESS__TYPE :: DIR_DESC
      INTEGER*4  IP
      INTEGER*4, EXTERNAL :: I_LEN, ILEN, CLOSEDIR
      ADDRESS__TYPE, EXTERNAL :: FUNC_OPENDIR
!
      CALL CLRCH ( ERRSTR ) 
!      
! --- Open directory.
!
      DIR_DESC = FUNC_OPENDIR ( TRIM(DIRNAM)//CHAR(0) )
      IF ( DIR_DESC .EQ. 0 ) THEN
!
! -------- Oh, devil! Error...
!
           CALL GERROR ( ERRSTR )
           ERRSTR = 'opendir: '//ERRSTR
           IS_DIR_EXIST = .FALSE.
         ELSE
!
! -------- Close directory of this level
!
           IP = CLOSEDIR ( %VAL(DIR_DESC) )
           IS_DIR_EXIST = .TRUE.
      END IF
      RETURN
      END  FUNCTION  IS_DIR_EXIST  !#!#
