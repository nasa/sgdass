      SUBROUTINE BSPD_MOVE_LOCK ( L_FIL, FINAM, FIL_POSTFIX, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine BSPD_MOVE_LOCK moves the set of L_FIL filenames FINAM with *
! *   the postfix FIL_POSTFIX to the files without the postfix with      *
! *   checks of lock status. It checks the write lock file. If the lock  *
! *   is set (the file exists and it is not too old), it sets read lock  *
! *   and it waits for lifting the lock. When the write lock is lifted,  *
! *   it renames temporary filenames into the permanent filenames        *
! *   without postfix. Then it removes read lock file.                   *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *       L_FIL ( INTEGER*4 ) -- The number of files in the FINAM list   *
! *                              which are to be renamed.                *
! *       FINAM ( CHARACTER ) -- Array of source files to be renamed.    *
! *                              Dimension: L_FIL.                       *
! * FIL_POSTFIX ( CHARACTER ) -- Postfix which must present in all       *
! *                              filenems from the list FINAM.           *
! *                              File names from FINAM will be changed   *
! *                              to theanems without this postfix.       *
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
! * ### 10-DEC-2002 BSPD_MOVE_LOCK  v2.0 (c)  L. Petrov 18-OCT-2019 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'spd.i'
      INTEGER*4  L_FIL, IUER
      CHARACTER  FINAM(L_FIL)*(*), FIL_POSTFIX*(*)
      CHARACTER  DIRNAM*128, FILE_IO_LOCK*128, FILE_READ_LOCK*128, &
     &           FILE_WRITE_LOCK*128, FILIN*128, FILOUT*128, STR*128
      LOGICAL*4  LEX
      INTEGER*4  LUN, IOS, ID, IP, IS, STAT_BLOCK(16), FD_READ_LOCK, &
     &           FD_WRITE_LOCK, J1, J2, IER
      CHARACTER  GET_CDATE*19
      INTEGER*4, EXTERNAL :: GETPID, GET_UNIT, ILEN, I_LEN, LINDEX, &
     &                       RENAME, STAT, TIME
!
! --- Build directory name
!
      CALL CLRCH ( DIRNAM )
      ID = LINDEX ( FINAM(1), '/' )
      IF ( ID .LE. 0 ) THEN
           DIRNAM = './'
           ID = 2
         ELSE
           DIRNAM = FINAM(1)(1:ID)
      END IF
!
! --- Build the the name of lock files
!
      FILE_IO_LOCK    = DIRNAM(1:ID)//SPD__IO_LOCK_NAME
      FILE_READ_LOCK  = DIRNAM(1:ID)//SPD__READ_LOCK_NAME
      FILE_WRITE_LOCK = DIRNAM(1:ID)//SPD__WRITE_LOCK_NAME
!
      CALL ERR_PASS ( IUER, IER )
      CALL SET_READ_LOCK ( FILE_IO_LOCK, FILE_READ_LOCK, FILE_WRITE_LOCK, &
     &                     SPD__LOCK_TIMEOUT, FD_READ_LOCK, FD_WRITE_LOCK, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 4821, IUER, 'BSPD_MOVE_LOCK', 'Failure to set read '// &
     &         'lock for directory '//DIRNAM(1:ID) )
           RETURN 
      END IF
!
! --- Cycle over the files to be renamed
!
      DO 420 J2=1,L_FIL
         IF ( ILEN(FINAM(J2)) .EQ. 0 ) GOTO 420
         CALL CLRCH ( FILIN  )
         CALL CLRCH ( FILOUT )
!
! ------ Build the source file name
!
         FILIN = FINAM(J2)
         IP = INDEX ( FILIN, FIL_POSTFIX(1:I_LEN(FIL_POSTFIX)) )
         IF ( IP .LE. 1 ) THEN
              CALL CLRCH ( STR )
              CALL INCH  ( J2, STR )
              CALL ERR_LOG ( 4829, IUER, 'BSPD_MOVE_LOCK', 'Trap of unternal '// &
     &            'control: the name of the '//STR(1:I_LEN(STR))//'-th file '// &
     &             FILIN(1:I_LEN(FILIN))//' does not contain prefix '// &
     &             FIL_POSTFIX )
              RETURN
         END IF
!
! ------ and build the target file name
!
         FILOUT = FILIN(1:IP-1)
#ifdef GNU
         IS = RENAME ( FILIN(1:I_LEN(FILIN))//CHAR(0), &
     &                 FILOUT(1:I_LEN(FILOUT))//CHAR(0) ) 
#else
         IS = RENAME ( FILIN, FILOUT ) 
#endif
         IF ( IS .NE. 0 ) THEN
              CALL CLRCH  ( STR )
              CALL GERROR ( STR )
              IF ( J2 > 1 .AND. STR == "No such file or directory" ) THEN
!
! ---------------- This is a known bug in NFS. It may happen that in
! ---------------- an attempt to move a file at NFS mounted disk we
! ---------------- may get a bogus ENOENT error code, while the operation
! ---------------- was successful
!
                   CONTINUE 
                 ELSE
                   CALL ERR_LOG ( 4830, IUER, 'BSPD_MOVE_LOCK', 'Error '// &
     &                  STR(1:I_LEN(STR))//' in an attempt to rename file '// &
     &                  FILIN(1:I_LEN(FILIN))//' into the file '//FILOUT )
                   RETURN
              END IF
         END IF
         FINAM(J2) = FILOUT
 420  CONTINUE
!
! --- Remove read lock file
!
      CALL LIFT_READ_WRITE_LOCKS ( FD_READ_LOCK, FD_WRITE_LOCK )
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  BSPD_MOVE_LOCK  #!#
