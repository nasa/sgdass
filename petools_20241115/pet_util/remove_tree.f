#include <mk5_preprocessor_directives.inc>
      SUBROUTINE REMOVE_TREE ( DIR_TREE, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  REMOVE_TREE  removes all files in the directory tree      *
! *   DIR_TREE and after that it removes the directory file itself.      *
! *   NB: REMOVE_TREE is not a recursive procedure. If the directory     *
! *   DIR_TREE contains at least subdirectories, these subdirectories    *
! *   will not be removed and the DIR_TREE will not be removed either.   *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *    DIR_TREE ( CHARACTER  ) -- The name of the directory tree to be   *
! *                               removed.                               *
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
! *  ### 25-JAN-2001  REMOVE_TREE  v1.3 (c)  L. Petrov  03-JUN-2019 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      CHARACTER  DIR_TREE*(*)
      INTEGER*4  IUER
!
      LOGICAL*4  LEX, FL_SLASH
      INTEGER*4  IS, STATB(12), ARG, ARG_LEN, J1, J2
      ADDRESS__TYPE DIR_DESC, IP
      CHARACTER  NAM*256, FULL_NAME*256, STR*256
      INTEGER*4  DIR_BIT
#ifdef DARWIN
#      define   FUNC_OPENDIR  OPENDIR$INODE64
#else
#      define   FUNC_OPENDIR  OPENDIR
#endif
      ADDRESS__TYPE, EXTERNAL :: FUNC_OPENDIR
      INTEGER*4,     EXTERNAL :: ILEN, I_LEN, FOR_STAT, FUNC_READDIR, &
     &                           CLOSEDIR, RMDIR
!
! --- Check what is the "directory" bit. It is defined in the Unix header
! --- constant S_IFDIR
!
      CALL GET_SYSTEM_CONSTANT ( 'S_IFDIR', ARG, ARG_LEN )
      DIR_BIT = 0
!
! --- Search for
!
      DO 410 J1=1,32
         IF ( BTEST ( ARG, J1 ) ) THEN
              DIR_BIT = J1
              GOTO 810
         END IF
 410  CONTINUE 
 810  CONTINUE 
!
      IF ( ILEN(DIR_TREE) .EQ. 0 ) THEN
           CALL ERR_LOG ( 1911, IUER, 'REMOVE_TREE', 'Empty argumetn DIR_TREE' )
           RETURN
      END IF
!
! --- Learn direcotry file status
!
      IS = FOR_STAT ( DIR_TREE(1:I_LEN(DIR_TREE)), STATB )
      IF ( IS .NE. 0 ) THEN
           CALL GERROR  ( STR )
           CALL ERR_LOG ( 1913, IUER, 'REMOVE_TREE', 'Error in attempt '// &
     &                    'to inquire status of the file '// &
     &                    DIR_TREE(1:I_LEN(DIR_TREE))//' -- '//STR )
           RETURN
      END IF
      IF ( .NOT. BTEST ( STATB(3), DIR_BIT  ) ) THEN
           CALL ERR_LOG ( 1914, IUER, 'REMOVE_TREE', 'File '// &
     &                    DIR_TREE(1:I_LEN(DIR_TREE))//' is not a directory' )
           RETURN
      END IF
!
! --- check whetehr the last character of the name is /
!
      FL_SLASH = .FALSE.
      IF ( DIR_TREE(I_LEN(DIR_TREE):I_LEN(DIR_TREE)) .EQ. '/' ) FL_SLASH =.TRUE.
!
! --- Open directory
!
      DIR_DESC = FUNC_OPENDIR ( DIR_TREE(1:I_LEN(DIR_TREE))//CHAR(0) )
      DO 420 J2=1,1024*1024 ! (almost) infinite loop for reading the files
!
! ------ Read the next line of the direcotory file
!
         IP = FUNC_READDIR ( %VAL(DIR_DESC) )
         IF ( IP .EQ. 0 ) GOTO 820
!
! ------ Extract the filename form the internal data structures
!
         CALL GET_NAME_FROM_DIR ( %VAL(IP), NAM )
         IF ( NAM(1:I_LEN(NAM)) .EQ. '.'  ) GOTO 420
         IF ( NAM(1:I_LEN(NAM)) .EQ. '..' ) GOTO 420
!
! ------ Build the fill file name
!
         CALL CLRCH ( FULL_NAME )
         IF ( FL_SLASH ) THEN
              FULL_NAME = DIR_TREE(1:I_LEN(DIR_TREE))//NAM
            ELSE
              FULL_NAME = DIR_TREE(1:I_LEN(DIR_TREE))//'/'//NAM
         END IF
!
! ------ Remove the file
!
         CALL UNLINK ( FULL_NAME(1:I_LEN(FULL_NAME))//CHAR(0) )
 420  CONTINUE
 820  CONTINUE
!
! --- Close the directory
!
      IS = CLOSEDIR ( %VAL(DIR_DESC) )
!
! --- ... and remove it
!
      IS = RMDIR ( DIR_TREE(1:I_LEN(DIR_TREE))//CHAR(0) )
      IF ( IS .NE. 0 ) THEN
           CALL GERROR  ( STR )
           CALL ERR_LOG ( 1915, IUER, 'REMOVE_TREE', 'Error in attempt to '// &
     &         'remove directory tree '//DIR_TREE(1:I_LEN(DIR_TREE))// &
     &         ' -- error message from rmdir: '//STR )
           RETURN
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  REMOVE_TREE  !#!#
