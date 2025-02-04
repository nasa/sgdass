      SUBROUTINE OPA_BAW ( SOLVE_INIT, OPA, IVRB, IUER )
! ************************************************************************
! *                                                                      *
! *   Subroutine  OPA_BAW  executes the action of OPA: it updates        *
! *   baseline weights file.                                             *
! *   OPA_BAW reads template batch control file, inserts arc-line,       *
! *   writes down temporary control file and then runs batch Solve with  *
! *   this temporary control file. Baseline-dependent weights for this   *
! *   session are written in the temporary file. OPA_BAW reads this file *
! *   inserts weights from there into the global baseline-type weights   *
! *   file.                                                              *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! * SOLVE_INIT ( CHARACTER ) -- Solve user initials.                     *
! *        OPA ( RECORD    ) -- Data structure which keeps internal      *
! *                             information of OPA: configuration        *
! *                             parameters, session name, status codes,  *
! *                             action codes.                            *
! *       IVRB ( INTEGER*4 ) -- Verbosity level. 0 means suppress all    *
! *                             information messages except error        *
! *                             messages.                                *
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
! *  ### 07-SEP-2000    OPA_BAW    v1.1 (c)  L. Petrov  15-SEP-2000 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE    'solve.i'
      INCLUDE    'precm.i'
      INCLUDE    'opa.i'
      TYPE ( OPA__STRU ) ::  OPA
      CHARACTER  SOLVE_INIT*2
      INTEGER*4  IVRB, IUER
      INTEGER*4  MBUF
      PARAMETER  ( MBUF = 1024 )
!
      CHARACTER  COMSTR*256, LOCK_FINAM*128, BUF(MBUF)*256, PID_STR*5, &
     &           WEIGHT_FILE*128, CONTROL_FILE*128, ARC_LN*128, DBV_STR*3, &
     &           DB_NM*17, DB_NM_READ*17, STR*512, WEIGHT_BACKUP*128, &
     &           NEW_WEIGHT_FILE*128
      CHARACTER  GET_CDATE*19
      LOGICAL*4  LSUI, CHECK_SOLVE_INITIALS, CHECK_SOLVE_COMPLETE
      INTEGER*4  NBUF, IS, PID, IPW, IPA, LUNI, LUNO, IO, J1, J2, J3, J4, IER
      LOGICAL*4  FL_W, FL_A, FL_INSERT
      INTEGER*4  SYSTEM, UNLINK, GETPID, RENAME, I_LEN, GET_UNIT
!
! --- Read template control file to the buffer (but reserve fitst three lines
! --- for comments)
!
      CALL ERR_PASS ( IUER, IER )
      CALL RD_TEXT  ( OPA%BAS_WEIGHT_CNT, MBUF, BUF(4), NBUF, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG  ( 4242, IUER, 'OPA_ACTION', 'Error in reading '// &
     &         'template control file '//OPA%BAS_WEIGHT_CNT )
           RETURN
      END IF
      NBUF = NBUF + 3
!
! --- Write doesn' comments in to the buffer with control file
!
      BUF(1) = '*  This file was generated automatically by program OPA on '// &
     &             GET_CDATE()
      BUF(2) = '*  using a template file '// &
     &             OPA%BAS_WEIGHT_CNT(1:I_LEN(OPA%BAS_WEIGHT_CNT))
      BUF(3) = '*  '
!
      PID = GETPID () ! get process identification of the current process
      CALL CLRCH   (       PID_STR )
      CALL INCH    ( PID,  PID_STR )
      CALL CHASHR  (       PID_STR )
      CALL BLANK_TO_ZERO ( PID_STR )
      CALL CLRCH   (                 DBV_STR )
      CALL INCH    ( OPA%DB_VERSION, DBV_STR )
      CALL CHASHR  (                 DBV_STR )
!
! --- Build filenames
!
      CALL CLRCH (      WEIGHT_FILE )
      CALL CLRCH (    WEIGHT_BACKUP )
      CALL CLRCH (  NEW_WEIGHT_FILE )
      CALL CLRCH (     CONTROL_FILE )
      CALL CLRCH (           ARC_LN )
!
      WEIGHT_FILE = OPA%TMP_DIR(1:I_LEN(OPA%TMP_DIR))//'opa_baw_'//PID_STR// &
     &              '.wgt'
      CONTROL_FILE = OPA%TMP_DIR(1:I_LEN(OPA%TMP_DIR))//'opa_baw_'//PID_STR// &
     &              '.cnt'
      WEIGHT_BACKUP = OPA%BAS_WEIGHT_FILE(1:I_LEN(OPA%BAS_WEIGHT_FILE))// &
     &                '.opa_'//PID_STR
      NEW_WEIGHT_FILE = OPA%BAS_WEIGHT_FILE(1:I_LEN(OPA%BAS_WEIGHT_FILE))// &
     &                  '.opa_new_baw_'//PID_STR
      ARC_LN = '  $'//OPA%DB_NAME//' '//DBV_STR//' '//OPA%ARC_LINE
!
! --- Scan template files for patterns @weight_file@ and @arc_line@
!
      FL_W = .FALSE.
      FL_A = .FALSE.
!
      DO 410 J1=1,NBUF
         IPW = INDEX ( BUF(J1), '@weight_file@' )
         IF ( IPW .GT. 1 ) THEN
!
! ----------- If found, replave with actual (temnporary) weight file name
!
              BUF(J1) = BUF(J1)(1:IPW-1)//WEIGHT_FILE(1:I_LEN(WEIGHT_FILE))// &
     &                  BUF(J1)(IPW+LEN('@weight_file@'):)
              FL_W = .TRUE.
         END IF
!
         IPA = INDEX ( BUF(J1), '@arc_line@' )
         IF ( IPA .GT. 0 ) THEN
!
! ----------- If found, replace with actual arc-line
!
              BUF(J1) = ARC_LN
              FL_A = .TRUE.
         END IF
 410  CONTINUE
!
      IF ( .NOT. FL_W ) THEN
           CALL ERR_LOG ( 4243, IUER, 'OPA_BAW', 'Signature @weight_file@ '// &
     &         'was not found in a template control file '// &
     &         OPA%BAS_WEIGHT_CNT )
           RETURN
      END IF
!
      IF ( .NOT. FL_A ) THEN
           CALL ERR_LOG ( 4244, IUER, 'OPA_BAW', 'Signature @arc_line@ '// &
     &         'was not found in a template control file '// &
     &         OPA%BAS_WEIGHT_CNT )
           RETURN
      END IF
!
! --- Write down updated control file
!
      CALL ERR_PASS ( IUER, IER )
      CALL WR_TEXT  ( NBUF, BUF, CONTROL_FILE, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 4245, IUER, 'OPA_BAW', 'Error in attempt to write '// &
     &         'temporary control file '//CONTROL_FILE )
           RETURN
      END IF
!
! --- Check solve lock. 
!
      CALL CHECK_SOLVE_LOCK()
!
! --- Build the line for launching Solve
!
      CALL CLRCH ( COMSTR  )
      COMSTR = PRE_SOL_DIR(1:PRE_SOL_LEN)//'solve '//SOLVE_INIT//' '// &
     &         CONTROL_FILE(1:I_LEN(CONTROL_FILE))//' silent'
!
! --- Launch solve and wait
!
      IS = SYSTEM ( COMSTR(1:I_LEN(COMSTR))//CHAR(0) )
!
! --- Remove lock from the current Solve user initials
!
      CALL REMOVE_SOLVE_LOCK()
!
      IF ( IS .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( IS, STR )
           CALL ERR_LOG ( 4246, IUER, 'OPA_BAW', 'Error '//STR(1:I_LEN(STR))// &
     &         ' in executing command line '//COMSTR )
           RETURN
      END IF
!
! --- Block Solve user initials
!
      CALL ERR_PASS ( IUER, IER )
      LSUI = CHECK_SOLVE_INITIALS ( 'W', SOLVE_INIT(1:I_LEN(SOLVE_INIT)), IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 4247, IUER, 'OPA_BAW', 'Trap of internal control: '// &
     &         'error in checking solve initials '//SOLVE_INIT )
           RETURN
      END IF
      IF ( .NOT. LSUI ) THEN
           CALL ERR_LOG ( 4248, IUER, 'OPA_BAW', 'Trap of internal control: '// &
     &         'Solve initials '//SOLVE_INIT//' are in use' )
           RETURN
      END IF
!
! --- Check whether Solve completed successfully
!
      IF ( .NOT. CHECK_SOLVE_COMPLETE ( SOLVE_INIT ) ) THEN
           CALL ERR_LOG ( 4249, IUER, 'OPA_BAW', 'Solve run was not '// &
     &                    'successfull' )
           RETURN
      END IF
!
! --- Remove control files which are not needed any more
!
      IS = UNLINK ( CONTROL_FILE(1:I_LEN(CONTROL_FILE))//CHAR(0) )
      IS = UNLINK ( CONTROL_FILE(1:I_LEN(CONTROL_FILE))//'.XPND'//CHAR(0) )
      IF ( IVRB .GE. 1 ) WRITE ( 6, '(A)' ) 'Insert new weights into the '// &
     &                                      'baseline weight file ...'
!
! --- Open the old input weight file
!
      LUNI = GET_UNIT ()
      OPEN ( UNIT=LUNI, FILE=OPA%BAS_WEIGHT_FILE, STATUS='OLD', IOSTAT=IO )
      IF ( IO .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( IO, STR )
           CALL ERR_LOG ( 4250, IUER, 'OPA_BAW', 'Error '// &
     &          STR(1:I_LEN(STR))//' in attempt to open weight file '// &
     &          OPA%BAS_WEIGHT_FILE )
           RETURN
      END IF
!
! --- Open the new output weight file
!
      LUNO = GET_UNIT ()
      OPEN ( UNIT=LUNO, FILE=NEW_WEIGHT_FILE, STATUS='UNKNOWN', IOSTAT=IO )
      IF ( IO .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( IO, STR )
           CALL ERR_LOG ( 4251, IUER, 'OPA_BAW', 'Error '// &
     &          STR(1:I_LEN(STR))//' in attempt to open output weight file '// &
     &          NEW_WEIGHT_FILE )
           CLOSE ( LUNI )
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
           CALL ERR_LOG ( 4252, IUER, 'OPA_BAW', 'Trap of internal control: '// &
     &         'wrong format of the database name which is being processed' )
           RETURN
      END IF
      CALL INCH   ( OPA%DB_VERSION, DB_NM(15:17) )
      CALL CHASHR (                 DB_NM(15:17) )
      CALL BLANK_TO_ZERO (          DB_NM(15:17) )
!
      FL_INSERT = .FALSE.
!
! --- Read global weight file
!
      DO 420 J2=1,1024*1024*1024
         DB_NM_READ = '          .  _   '
         READ ( LUNI, FMT='(A)', IOSTAT=IO ) STR ! read a line
         IF ( IO .EQ. -1 ) THEN
              GOTO 820
            ELSE IF ( IO .NE. 0 ) THEN
              CALL CLRCH ( STR )
              CALL INCH  ( IO, STR )
              WRITE ( 6, * ) ' line ',J2
              CALL ERR_LOG ( 4253, IUER, 'OPA_BAW', 'Error '// &
     &             STR(1:I_LEN(STR))//' in attempt to read weight file '// &
     &             OPA%BAS_WEIGHT_FILE )
              RETURN
         END IF
!
         IF ( STR(1:1) .NE. '*' ) THEN
!
! ----------- Parse the database file name from the global weight file and
! ----------- build a string DB_NM_READ for comparing
!
              CALL ERR_PASS ( IUER, IER )
              CALL PARSE_DBNAME ( STR(1:10), DB_NM_READ(1:10), &
     &                            DB_NM_READ(12:13), IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 4254, IUER, 'OPA_BAW', 'Trap of internal '// &
     &                 'control: wrong format of the database name '// &
     &                  STR(1:13) )
                   RETURN
              END IF
              DB_NM_READ(15:17) = STR(11:13 )
              CALL BLANK_TO_ZERO ( DB_NM_READ(15:17) )
!
! ----------- Now comapre transformed database filename: the targeted database
! ----------- file name and the database file name taken from global weight
! ----------- file
!
              IF ( DB_NM_READ .GE. DB_NM  .AND. .NOT. FL_INSERT ) THEN
!
! ---------------- It is just the place to insert new weights
! ---------------- Read the new input weight file
!
                   CALL RD_TEXT ( WEIGHT_FILE, MBUF, BUF, NBUF, IER )
                   IF ( IER .NE. 0 ) THEN
                        CALL ERR_LOG ( 4255, IUER, 'OPA_BAW', 'Error in '// &
     &                      'attempt to read the input weight file which '// &
     &                      'just has been created: '//WEIGHT_FILE )
                        CLOSE ( LUNO )
                        CLOSE ( LUNI )
                       RETURN
                   END IF
!
! ---------------- Write it down in the output file
!
                   DO 430 J3=1,NBUF
                      IF ( BUF(J3)(1:1) .EQ. '*'  .AND. &    ! Avoid putting the
     &                     DB_NM_READ .EQ. DB_NM        ) GOTO 430 ! second *
!
                      WRITE ( LUNO, '(A)' ) BUF(J3)(1:I_LEN(BUF(J3)))
 430               CONTINUE
                   FL_INSERT = .TRUE.
              END IF
         END IF
!
         IF ( DB_NM_READ .NE. DB_NM ) THEN
!
! ----------- Copy the input line to the output file unless it is the line
! ----------- for this experiment
!
              WRITE ( LUNO, '(A)' ) STR(1:I_LEN(STR))
         END IF
 420  CONTINUE
 820  CONTINUE
      IF ( .NOT. FL_INSERT ) THEN
!
! -------- Hm. It occurred that alll databases names in the weight file
! -------- turned out to be too young. Read the new input weight file
!
           CALL RD_TEXT ( WEIGHT_FILE, MBUF, BUF, NBUF, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 4256, IUER, 'OPA_BAW', 'Error in attempt '// &
     &              'to read the input weight file which just has been '// &
     &              'created: '//WEIGHT_FILE )
                CLOSE ( LUNI )
                CLOSE ( LUNO )
                RETURN
           END IF
!
! -------- ... and add its contents to the end of the global weight file
!
           DO 440 J4=1,NBUF
              WRITE ( LUNO, '(A)' ) BUF(J4)(1:I_LEN(BUF(J4)))
 440       CONTINUE
      END IF
      CLOSE ( UNIT = LUNI )
      CLOSE ( UNIT = LUNO )
      IF ( IVRB .GE. 1 ) WRITE ( 6, '(A)' ) 'Copying baseline weight files ...'
!
! --- Copy  OPA.BAS_WEIGHT_FILE --> WEIGHT_BACKUP
!
      CALL ERR_PASS ( IUER, IER )
      CALL COPY_ASCII_FILE ( OPA%BAS_WEIGHT_FILE, WEIGHT_BACKUP, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 4257, IUER, 'OPA_BAW', 'Error in attempt '// &
     &          'to make a backup copy of the old baseline weights file' )
           RETURN
      END IF
!
! --- Rename NEW_WEIGHT_FILE --> OPA.BAS_WEIGHT_FILE
! --- This trick with first copying file and then renaming is for minimizing
! --- time of file update. At the same time we are are sure that NEW_WEIGHT_FILE
! --- and OPA.BAS_WEIGHT_FILE are at the same disk system and therefore
! --- operation of renaming is legitimate
!
      IS = RENAME ( NEW_WEIGHT_FILE, OPA%BAS_WEIGHT_FILE )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 4258, IUER, 'OPA_BAW', 'Serious error '// &
     &          'in attempt to move the new weight file to the old place. '// &
     &          'The old file '// &
     &          OPA%BAS_WEIGHT_FILE(1:I_LEN(OPA%BAS_WEIGHT_FILE))// &
     &         ' is probably spoiled!!! Check the backup copy '//WEIGHT_BACKUP )
           RETURN
      END IF
!
! --- Remove temporary weight files
!
      IS = UNLINK ( WEIGHT_BACKUP(1:I_LEN(WEIGHT_BACKUP))//CHAR(0) )
      IS = UNLINK ( WEIGHT_FILE(1:I_LEN(WEIGHT_FILE))//CHAR(0) )
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  OPA_BAW  #!#
