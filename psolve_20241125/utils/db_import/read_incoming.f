      SUBROUTINE READ_INCOMING ( DBI, MI_DBS, LI_DBS, CI_DBS, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  READ_INCOMING  reads contents of the directory            *
! *   DBI.INCOMING_DIR and builds the sorted database filename list      *
! *   CI_DBS.                                                            *
! *   Routine READ_INCOMING discards all database filenames which don't  *
! *   have signature "_V".                                               *
! *   Routine READ_INCOMING discards all database filenames which have   *
! *   extension .gz .                                                    *
! *                                                                      *
! * ________________________ Input parameters: _________________________ *
! *                                                                      *
! *   DBI ( RECORD    ) -- Object with data structure for keeping        *
! *                        configuration of DB_IMPORT utility.           *
! * MI_DBS ( INTEGER*4 ) -- Maxinal numbers of the database file names.  *
! *                                                                      *
! * ________________________ Output parameters: ________________________ *
! *                                                                      *
! * LI_DBS ( INTEGER*4 ) -- Numbers of database file names.              *
! * CI_DBS ( CHARACTER ) -- Database filename list. Dimension: LI_DBS.   *
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
! *  ### 18-OCT-2000  READ_INCOMING  v1.1 (c) L. Petrov 03-SEP-2019  ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'db_import.i'
      TYPE ( DBI__STRU ) ::  DBI
      INTEGER*4  MI_DBS, LI_DBS, IUER
      CHARACTER  CI_DBS(MI_DBS)*(*)
      INTEGER*4    M_BUF
      PARAMETER  ( M_BUF = 1024 )
      CHARACTER  STR*128, NAME*256
      INTEGER*4  ID, IV, IG, J1
      ADDRESS__TYPE :: DIR_DESC, IP
      INTEGER*4, EXTERNAL :: I_LEN
      ADDRESS__TYPE, EXTERNAL :: OPENDIR, READDIR
!
! --- Open directory and getting directory file descriptor
!
      ID = I_LEN ( DBI%INCOMING_DIR )
      DIR_DESC = OPENDIR ( DBI%INCOMING_DIR(1:ID)//CHAR(0) )
      IF ( DIR_DESC .LE. 0 ) THEN
           CALL CLRCH  ( STR )
           CALL GERROR ( STR )
           CALL ERR_LOG ( 5751, -1, 'READ_INCOMING', 'Error in attempt to '// &
     &         'read directory '//DBI%INCOMING_DIR(1:ID)//'  OPRENDIR: '//STR )
           RETURN
      END IF
!
! --- Scan directory filenames
!
      LI_DBS = 0
      DO 410 J1=1,1024*1024
!
! ------ Read the next line of the direcotory file
!
         IP = READDIR ( %VAL(DIR_DESC) )
         IF ( IP .EQ. 0 ) GOTO 810
!
! ------ Extract the filename form the internal data structures
!
         CALL GET_NAME_FROM_DIR ( %VAL(IP), NAME )
!
! ------ Discad filename without signature "_V"
!
         IV = INDEX ( NAME, '_V' )
         IF ( IV .LE. 0 ) GOTO 410
!
! ------ Discard fielname with extension .gz
!
         IG = INDEX ( NAME, '.gz' )
         IF ( IG .GT. 0 ) GOTO 410
!
! ------ Add the filename to the list
!
         LI_DBS = LI_DBS + 1
         IF ( LI_DBS .GT. MI_DBS ) THEN
              CALL CLRCH ( STR )
              CALL INCH  ( MI_DBS, STR )
              CALL ERR_LOG ( 5752, IUER, 'READ_INCOMING', 'Too many files '// &
     &            'in incoming directory '//DBI%INCOMING_DIR(1:ID)// &
     &            ' Parameter MT_DBS '//STR(1:I_LEN(STR))// &
     &            ' is too small' )
              RETURN
         END IF
         CI_DBS(LI_DBS) = NAME
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!        type *,' li_dbs=',li_dbs,'  ci_dbs  >>',ci_dbs(li_dbs),'<<  '  ! %%%%
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 410  CONTINUE
 810  CONTINUE
!
! --- Sort database filename list
!
      IF ( LI_DBS .GT. 0 ) THEN
           CALL SORT_CH ( LI_DBS, CI_DBS )
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  READ_INCOMING  #!#
