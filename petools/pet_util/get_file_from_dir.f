#include <mk5_preprocessor_directives.inc>
      FUNCTION   GET_FILE_FROM_DIR ( LEV, DIR_DESC, DIRNAM, FILNAM )
! ************************************************************************
! *                                                                      *
! *     Routine  GET_FILE_FROM_DIR  performs recursive walk around       *
! *   directory tree and returns the full name of the file from the      *
! *   directory. The sequence of consecutive calls of GET_FILE_FROM_DIR  *
! *   allows to get all files located in this directory and all          *
! *   subdirectories.                                                    *
! *                                                                      *
! *     Parameter LEV should be initialized by 0 before the first call   *
! *   of this subroutine. GET_FILE_FROM_DIR returns LEV which keeps the  *
! *   level of the subdirectory for the returned filname. LEV=0 means    *
! *   that no more files in this tree remained. (If GET_FILE_FROM_DIR    *
! *   will be called after that it will start walking through the        *
! *   directory again).                                                  *
! *                                                                      *
! * _________________________ Modified parameters: _____________________ *
! *                                                                      *
! *      LEV ( INTEGER*4 ) -- Input:  LEV should be initialized to zero  *
! *                                   before the first call. A user      *
! *                                   should not modify LEV after that.  *
! *                           Output: Level of subdirectory. 1 means     *
! *                                   directory level requested in the   *
! *                                   first call of this routine.        *
! *                                   0 means that no more files         *
! *                                   remained in the tree.              *
! * DIR_DESC ( INTEGER*4 ) -- Array of file descriptors. Dimension: 16.  *
! *                           This array should not be modified by user. *
! *   DIRNAM ( CHARACTER ) -- Input:  directory name where the files     *
! *                                   are searched. DIRNAM should        *
! *                                   contain the name of this directory *
! *                                   before the first call of DIRNAM.   *
! *                                   Trailing / is allowed.             *
! *                                   User should not change DIRNAM      *
! *                                   after the first and before the     *
! *                                   last call of GET_FILE_FROM_DIR.    *
! *                           Output: the name of the current            *
! *                                   subdirectory. When the output      *
! *                                   value of LEV=0, then DIRNAM is     *
! *                                   the same as before the first call  *
! *                                   of GET_FILE_FROM_DIR.              *
! *                                                                      *
! * _________________________ Output parameters: _______________________ *
! *                                                                      *
! *   FILNAM ( CHARACTER ) -- Full path name, which includes directory   *
! *                           name. If DIRNAM was a relative name, than  *
! *                           FILNAM is relative name as well. If DIRNAM *
! *                           was an absolute name, than FILNAM is       *
! *                           an absolute name too.                      *
! *                           In the case of errors, FILNAM returns      *
! *                           the error message.                         *
! * <GET_FILE_FROM_DIR> ( INTEGER*4 ) -- completion status.              *
! *                                      0 -- normal completion.         *
! *                                     -1 -- error. Then FILNAME        *
! *                                           returns the error message. *
! *                                                                      *
! * ### 20-AUG-2002 GET_FILE_FROM_DIR v2.7 (c) L. Petrov 03-JUN-2019 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INTEGER*4  GET_FILE_FROM_DIR, LEV
      INTEGER*4  MLEV
      PARAMETER  ( MLEV = 64 )
      ADDRESS__TYPE :: DIR_DESC(MLEV)
      CHARACTER  DIRNAM*(*), FILNAM*(*)
      CHARACTER  NAME*256
      INTEGER*4  DIR_BITS, LNK_BITS, SOCK_BITS, J1, K1
      INTEGER*4  STATB(MLEV)
      INTEGER*4  ARG, ARG_LEN
      ADDRESS__TYPE IP
      LOGICAL*1  FL_LNK
#ifdef DARWIN
#      define   FUNC_OPENDIR  OPENDIR$INODE64
#else
#      define   FUNC_OPENDIR  OPENDIR
#endif
      INTEGER*4, EXTERNAL :: I_LEN, ILEN, LINDEX, FOR_STAT, FOR_LSTAT, CLOSEDIR
      ADDRESS__TYPE, EXTERNAL :: FUNC_OPENDIR, FUNC_READDIR
!
! --- Remove the trailing / if present
!
      IF ( DIRNAM(I_LEN(DIRNAM):I_LEN(DIRNAM)) .EQ. '/' ) THEN
           DIRNAM(I_LEN(DIRNAM):I_LEN(DIRNAM)) = ' '
      END IF
!
! --- Check what is the "directory" bit. It is defined in the Unix header
! --- constant S_IFDIR
!
      CALL GET_SYSTEM_CONSTANT ( 'S_IFDIR',  DIR_BITS,  ARG_LEN )
      CALL GET_SYSTEM_CONSTANT ( 'S_IFLNK',  LNK_BITS,  ARG_LEN )
      CALL GET_SYSTEM_CONSTANT ( 'S_IFSOCK', SOCK_BITS, ARG_LEN )
!
      CALL CLRCH ( FILNAM )
      IF ( LEV .EQ. 0 ) THEN
!
! -------- Open directory.
!
           LEV = 1
           DIR_DESC(LEV) = FUNC_OPENDIR ( DIRNAM(1:I_LEN(DIRNAM))//CHAR(0) )
           IF ( DIR_DESC(LEV) .EQ. 0 ) THEN
!
! ------------- Oh, devil! Error...
!
                CALL GERROR ( FILNAM )
                FILNAM = 'opendir: '//FILNAM
                LEV = 0
                GET_FILE_FROM_DIR = -1
                RETURN
           END IF
      END IF
!
      DO 410 J1=1,1024*1024*1024 ! (almost) infinite loop for reading the files
!
! ------ Read the next line of the directory file
!
         IP = FUNC_READDIR ( %VAL(DIR_DESC(LEV)) )
         IF ( IP .EQ. 0 ) THEN
              IF ( LEV .GT. 1 ) THEN
                   IP = LINDEX ( DIRNAM, '/' )
                   IF ( IP > 0 ) CALL CLRCH  ( DIRNAM(IP:) )
              END IF
!
! ----------- Close directory of this level
!
              IP = CLOSEDIR ( %VAL(DIR_DESC(LEV)) )
              LEV = LEV-1  ! decrement the level
              IF ( LEV .EQ. 0 ) THEN
                   GET_FILE_FROM_DIR = 0
                   RETURN   ! no more levels -- nothing to do any more
                ELSE
                   GOTO 410 ! another read
              END IF
         END IF
!
! ------ Extract the name from internal directory data structure
!
         CALL CLRCH ( NAME )
         CALL GET_NAME_FROM_DIR ( %VAL(IP), NAME )
!
! ------ Build the full file name
!
         FILNAM = DIRNAM(1:ILEN(DIRNAM))//'/'//NAME
!
! ------ Learn: whether it is a normal file or directory
!
         IP = FOR_STAT ( FILNAM(1:I_LEN(FILNAM))//CHAR(0), STATB )
         IF ( NAME(1:2) .EQ. '. ' ) THEN
              CONTINUE  ! ingore this special file
            ELSE IF ( NAME(1:3) .EQ. '.. ' ) THEN
              CONTINUE  ! ignore this specual file
!%            ELSE IF ( ( STATB(3) .AND. SOCK_BITS ) == SOCK_BITS ) THEN
            ELSE IF ( IAND ( STATB(3), SOCK_BITS ) == SOCK_BITS ) THEN
              CONTINUE  ! ignore socket file
!%            ELSE IF ( ( STATB(3) .AND. DIR_BITS  ) == DIR_BITS ) THEN
            ELSE IF ( IAND ( STATB(3), DIR_BITS  ) == DIR_BITS ) THEN
!
! ----------- Aga! It is a directory
!
              LEV = LEV + 1
              IF ( LEV .GT. MLEV )  THEN
                   CALL CLRCH ( FILNAM )
                   CALL INCH  ( MLEV, FILNAM(1:4) )
                   FILNAM = 'Too many levels in directory tree: more than '//FILNAM(1:4)
                   GET_FILE_FROM_DIR = -2
                   RETURN
              END IF
              NAME = DIRNAM
              DIRNAM = FILNAM
              CALL CLRCH ( FILNAM )
!
! ----------- Open this directory
!
              DIR_DESC(LEV) = FUNC_OPENDIR ( DIRNAM(1:I_LEN(DIRNAM))//CHAR(0) )
              IF ( DIR_DESC(LEV) .EQ. 0 ) THEN
!
! ---------------- A-a! Error
!
                   CALL GERROR ( FILNAM )
                   IF ( LEV .NE. 0 .AND. &
     &                  ( FILNAM(1:17) .EQ. 'Permission denied'    .OR. &
     &                    FILNAM(1:20) .EQ. 'unknown error number' .OR. &
     &                    FILNAM(1:15) .EQ. 'Not a directory'           ) ) THEN
!
! --------------------- If permission was denied in a subdirectory or
! --------------------- it was an unknown error, go further
!
                        LEV = LEV - 1
                        DIRNAM = NAME
                        CONTINUE
                      ELSE
                        IP = FOR_LSTAT ( DIRNAM(1:I_LEN(DIRNAM))//CHAR(0), STATB )
                        IF ( IAND ( STATB(3), LNK_BITS ) == LNK_BITS ) THEN
                             FL_LNK = .TRUE.
                           ELSE
                             FL_LNK = .FALSE.
                        END IF
                        IF ( FL_LNK .AND. &
     &                       ( FILNAM(1:I_LEN(FILNAM)) == "No such file or directory" .OR. &
     &                         FILNAM(1:I_LEN(FILNAM)) == "Too many levels of symbolic links" ) ) THEN
!
                             LEV = LEV - 1
                             DIRNAM = NAME
                             CONTINUE
                           ELSE
                             LEV = 0
                             FILNAM = 'opendir: '//FILNAM
                             GET_FILE_FROM_DIR = -3
                             RETURN
                        END IF
                   END IF
              END IF
            ELSE
              GET_FILE_FROM_DIR = 0
              RETURN
         END IF
 410  CONTINUE
      FILNAM = 'Too many files'
      GET_FILE_FROM_DIR = -4
 !
      RETURN
      END  FUNCTION  GET_FILE_FROM_DIR  !#!#
!
