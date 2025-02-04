      SUBROUTINE OPA_GAL ( OPA, IVRB, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  OPA_GAL  inserts the database name, databases version and *
! *   database-specific arc-line saved in in the object OPA to the       *
! *   global arc file specified in the fields of OPA: OPA.GLO_ARC_FILE . *
! *   This file is overwritten.                                          *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *  IVRB ( INTEGER*4 ) -- Verbosity level. 0 means suppress all         *
! *                        information messages except error messages.   *
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
! *   OPA ( RECORD    ) -- Data structure which keeps internal           *
! *                        information of OPA: configuration parameters, *
! *                        session name, status codes, action codes.     *
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
! *  ### 15-SEP-2000     OPA_GAL   v1.1 (c)  L. Petrov  06-JUN-2002 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE    'opa.i'
      TYPE ( OPA__STRU ) ::  OPA
      INTEGER*4  IVRB, IUER
      CHARACTER  ARC_STR*256, DBN*10, DBV_STR*3, REG*3
      INTEGER*4  MBUF, MIND
      PARAMETER  ( MBUF = 8192 )
      PARAMETER  ( MIND = 32   )
      PARAMETER  ( REG = CHAR(0)//CHAR(32)//CHAR(9) )
      LOGICAL*4  FL_INSERT
      CHARACTER  BUF(MBUF)*256, NEW_ARC_FILE*256, ARC_BACKUP*256, PID_STR*5, &
     &           DB_NM*17, DB_NM_READ*17, DBN_RD*10, STR*32, &
     &           USER_NAME*128, USER_REALNAME*128, USER_E_ADDRESS*128
      CHARACTER  GET_CDATE*19
      INTEGER*4  IS, LIND, IND(2,MIND), LUNO, LUN, PID, IO, J1, NBUF, IER
      INTEGER*4, EXTERNAL :: GET_UNIT, GETPID, ILEN, I_LEN, RENAME, UNLINK
!
      PID = GETPID () ! get process identification of the current process
      CALL CLRCH   (                 PID_STR )
      CALL INCH    ( PID,            PID_STR )
      CALL CHASHR  (                 PID_STR )
      CALL BLANK_TO_ZERO (           PID_STR )
      CALL CLRCH   (            NEW_ARC_FILE )
      CALL CLRCH   (              ARC_BACKUP )
      CALL CLRCH   (                 ARC_STR )
      CALL CLRCH   (                 DBV_STR )
      CALL INCH    ( OPA%DB_VERSION, DBV_STR )
      CALL CHASHR  (                 DBV_STR )
      ARC_STR = '  $'//OPA%DB_NAME//DBV_STR//'  '//OPA%ARC_LINE
      NEW_ARC_FILE = OPA%GLO_ARC_FILE(1:I_LEN(OPA%GLO_ARC_FILE))// &
     &               '.opa_new_arc_'//PID_STR
      ARC_BACKUP   = OPA%GLO_ARC_FILE(1:I_LEN(OPA%GLO_ARC_FILE))// &
     &               '.opa_old_arc_'//PID_STR
!
! --- Read file with global arc-list to the internal buffere BUF
!
      IF ( IVRB .GE. 1 ) WRITE ( 6, '(A)' ) 'Reading arc-list'
      CALL ERR_PASS ( IUER, IER )
      CALL RD_TEXT  ( OPA%GLO_ARC_FILE, MBUF, BUF, NBUF, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 4251, IUER, 'OPA_GAL', 'Error in attempt to '// &
     &         'read global arc-file' )
           RETURN
      END IF
!
! --- Open the new temporary output arc-file. It will contain copy of the old
! --- one plus inserted new arc-line
!
      LUNO = GET_UNIT ()
      OPEN ( UNIT=LUNO, FILE=NEW_ARC_FILE, STATUS='UNKNOWN', IOSTAT=IO )
      IF ( IO .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( IO, STR )
           CALL ERR_LOG ( 4252, IUER, 'OPA_GAL', 'Error '// &
     &          STR(1:I_LEN(STR))//' in attempt to open output arc-file '// &
     &          NEW_ARC_FILE )
           RETURN
      END IF
!
! --- Parse the database name and to transform it to the form
! --- YYYYMMMDDD.SS_VVV
!
      DB_NM = '          .  _   '
      CALL ERR_PASS ( IUER, IER )
      CALL PARSE_DBNAME ( OPA%DB_NAME, DB_NM(1:10), DB_NM(12:13), IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 4253, IUER, 'OPA_GAL', 'Trap of internal control: '// &
     &         'wrong format of the database name which is being processed' )
           RETURN
      END IF
      CALL INCH   ( OPA%DB_VERSION, DB_NM(15:17) )
      CALL CHASHR (                 DB_NM(15:17) )
      CALL BLANK_TO_ZERO (          DB_NM(15:17) )
!
      FL_INSERT = .FALSE.
!
! --- Now scan global arc list
!
      DO 410 J1=1,NBUF
         CALL CLRCH ( DBN_RD )
         IF ( BUF(J1)(1:14) .EQ. '**     Updated' ) THEN
              BUF(J1)(24:42) = GET_CDATE()
         END IF
         IF ( BUF(J1)(1:1) .EQ. '*' ) GOTO 810
!
! ------ Extract words
!
         CALL EXWORD ( BUF(J1), MIND, LIND, IND, REG, IER )
         IF ( LIND .LT. 2 ) GOTO 810
         IF ( IND(1,1) .EQ. IND(2,1) ) GOTO 810
!
         IF ( BUF(J1)(IND(1,1):IND(1,1)) .EQ. '$' ) THEN
              DBN_RD = BUF(J1)(IND(1,1)+1:IND(2,1))
            ELSE
              DBN_RD = BUF(J1)(IND(1,1):IND(2,1))
         END IF
!
         IF ( DBN .EQ. DBN_RD  .AND. .NOT. FL_INSERT ) THEN
!
! ----------- Aga! We found that there is the line with exactly the same
! ----------- database name and version number. We replace it
!
              CALL CLRCH ( BUF(J1) )
              BUF(J1) = ARC_STR
              FL_INSERT = .TRUE.
            ELSE IF ( DBN .EQ. DBN_RD  .AND. FL_INSERT ) THEN
              GOTO 410
            ELSE IF ( .NOT. FL_INSERT ) THEN
              DB_NM_READ = '          .  _   '
!
! ----------- Parse the database file name from the global arc file and
! ----------- build a string DB_NM_READ for comparing
!
              CALL ERR_PASS ( IUER, IER )
              CALL PARSE_DBNAME ( DBN_RD, DB_NM_READ(1:10), &
     &                            DB_NM_READ(12:13), IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 4254, IUER, 'OPA_GAL', 'Trap of internal '// &
     &                 'control: wrong format of the database name '// &
     &                  DBN_RD )
                   RETURN
              END IF
!
              DB_NM_READ(15:17) = BUF(J1)(IND(1,2):IND(2,2))
              CALL CHASHR        ( DB_NM_READ(15:17) )
              CALL BLANK_TO_ZERO ( DB_NM_READ(15:17) )
!
! ----------- Now compare the line
!
              IF ( DB_NM_READ .GE. DB_NM  ) THEN
!
! ---------------- If the line from arc-file appeared greater (older) then
! ---------------- arc-line for the database under consideration, then we insert
! ---------------- it into the global arc file
!
                   WRITE ( LUNO, '(A)' ) ARC_STR(1:I_LEN(ARC_STR))
                   FL_INSERT = .TRUE. ! Set flag for not trying to do it any more
              ENDIF
         END IF
 810     CONTINUE
!
! ------ Write down the line which should not be changed
!
         IF ( DBN_RD .NE. OPA%DB_NAME ) THEN
              WRITE ( LUNO, '(A)' ) BUF(J1)(1:I_LEN(BUF(J1)))
         END IF
 410  CONTINUE
!
      IF ( .NOT. FL_INSERT ) THEN
!
! -------- Not insert? Hm. It means that all arc-lines in the global arc-file
! -------- are less (yonger) then the requested. Well, add it to the end
!
           WRITE ( LUNO, '(A)' ) ARC_STR(1:I_LEN(ARC_STR))
           FL_INSERT = .TRUE.
      END IF
      CLOSE ( UNIT = LUNO )
      IF ( IVRB .GE. 1 ) WRITE ( 6, '(A)' ) 'Writing updated arc-list'
!
! --- Copy  OPA.GLO_ARC_FILE --> ARC_BACKUP for making a backup copy
!
      CALL ERR_PASS ( IUER, IER )
      CALL COPY_ASCII_FILE ( OPA%GLO_ARC_FILE, ARC_BACKUP, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 4255, IUER, 'OPA_GAL', 'Error in attempt '// &
     &          'to make a backup copy of the old global arc-file' )
           RETURN
      END IF
!
! --- Renaming the arc-file. The old arc-file is updated just now
!
      IS = RENAME ( NEW_ARC_FILE, OPA%GLO_ARC_FILE )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 4256, IUER, 'OPA_GAL', 'Serious error '// &
     &          'in attempt to move the new arc file to the old place. '// &
     &          'The old file '// &
     &          OPA%GLO_ARC_FILE(1:I_LEN(OPA%GLO_ARC_FILE))// &
     &         ' is probably spoiled!!! Check the backup copy '//ARC_BACKUP )
           RETURN
      END IF
!
! --- Remove temporary backup of arc-file
!
      IS = UNLINK ( ARC_BACKUP(1:I_LEN(ARC_BACKUP))//CHAR(0) )
!
      LUN = GET_UNIT ()
      OPEN ( UNIT=LUN, FILE=OPA%SESUPD_LOG, STATUS='UNKNOWN', ACCESS='APPEND' )
!
! --- Get user's real name
!
      CALL GETINFO_USER ( USER_NAME, USER_REALNAME, USER_E_ADDRESS )
      IF ( ILEN(USER_REALNAME) .EQ. 0 ) USER_REALNAME = USER_E_ADDRESS
!
! --- ... and write down the current date of update and user's name
!
      WRITE ( LUN, FMT='(A)' ) OPA%DB_NAME//'  '//OPA%SESS_CODE//'  '// &
     &                         GET_CDATE()//'  '// &
     &                         USER_REALNAME(1:I_LEN(USER_REALNAME))
      CLOSE ( UNIT=LUN )
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  OPA_GAL  #!#
