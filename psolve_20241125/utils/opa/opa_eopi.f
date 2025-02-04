      SUBROUTINE OPA_EOPI ( SOLVE_INIT, OPA, IVRB, IUER )
! ************************************************************************
! *                                                                      *
! *   Subroutine  OPA_EOPI  executes the action of OPA: it updates       *
! *   EOPI (EOP submission, conservative style, Intensive experiment)    *
! *   file for contribution of this specific databases OPA.DB_NAME .     *
! *   It makes the Solve batch independent solution:                     *
! *     1) It makes solution with EOP substitution file OPA.GEN_INPERP . *
! *        Then it extracts EOP values for this session and writes them  *
! *        in the EOB format.                                            *
! *                                                                      *
! *        After that it inserts EOP values for this session in the file *
! *        OPA.EOPB_FILE .                                               *
! *                                                                      *
! *        Then OPA.EOPB_FILE is transfored into the file OPA.EOPS_FILE .*
! *                                                                      *
! *        Optional fields in EOPI file are defined as follows:          *
! *        ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~          *
! *                                                                      *
! *        Field 17 -- number of observations used in analysis.          *
! *        Field 18 -- experiment's category. Experiment category is     *
! *                    derived from the databases suffix by the          *
! *                    following way: ICHAR(SUFFIX(2:2)) - 64.           *
! *                    For example, it is 20 for XT, 21 for XU databases.*
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
! *  ### 29-NOV-2000     OPA_EOPI  v1.2 (c)  L. Petrov  06-JUN-2002 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE    'solve.i'
      INCLUDE    'precm.i'
      INCLUDE    'opa.i'
      INCLUDE    'getpar.i'
      TYPE ( OPA__STRU ) ::  OPA
      TYPE ( EOB__CHAR ) ::  EOB
      TYPE ( EOP__STRU ) ::  EOP(M_SES)
      CHARACTER  SOLVE_INIT*2
      INTEGER*4  IVRB, IUER
      INTEGER*4  MBUF, M_HEA
      PARAMETER  ( MBUF = 1024 )
      PARAMETER  ( M_HEA =  512 )
      CHARACTER  CONTROL_FILE*128, SPOOL_FILE*128, SPOOL_DIR_FILE*128, &
     &           EOPB_FILE*128, EOPB_BACKUP*128, NEW_EOPB_FILE*128, &
     &                          EOPI_BACKUP*128, NEW_EOPI_FILE*128, &
     &           COMSTR*256, LOCK_FINAM*128, BUF(MBUF)*272, PID_STR*5, &
     &           ARC_LN*128, DBV_STR*3, DB_NM*13, DB_NM_READ*13, STR*512, &
     &           EOB_STR*512, SOLVE_HELP_DIR_STR*128, EOPS_HELP*128
      CHARACTER  HEA_BUF(M_HEA)*128
      CHARACTER  USER_NAME*128, USER_REALNAME*128, USER_E_ADDRESS*128
      CHARACTER  GET_CDATE*19
      LOGICAL*4  LEX, LSUI, CHECK_SOLVE_INITIALS, CHECK_SOLVE_COMPLETE
      INTEGER*4  NBUF, PID, IPA, IPE, IO, LUNI, LUNO, ISPL, ISIG, ICOD, &
     &           STAT_BLOCK(12), IS, N_HEA, N_HLP, N_SES, LEN_EOB, &
     &           J1, J2, J3, J4, J5, IER
      LOGICAL*4  FL_A, FL_E, FL_INSERT
      INTEGER*4, EXTERNAL :: ILEN, I_LEN, GETPID, GET_UNIT, RENAME, &
     &                       FOR_STAT, SYSTEM, UNLINK
!
! --- Check EOPS file
!
      INQUIRE ( FILE=OPA%EOPS_FILE, EXIST=LEX )
      IF ( .NOT. LEX ) THEN
           CALL ERR_LOG ( 4361, IUER, 'OPA_EOPI', 'The old EOPI file '// &
     &          OPA%EOPS_FILE(1:I_LEN(OPA%EOPS_FILE))//' was not found. '// &
     &          'It should exist before OPA_EOPI will try to update it ' )
           RETURN
      END IF
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
      CALL CLRCH (     EOPB_FILE )
      CALL CLRCH (   EOPB_BACKUP )
      CALL CLRCH ( NEW_EOPB_FILE )
      CALL CLRCH (   EOPI_BACKUP )
      CALL CLRCH ( NEW_EOPI_FILE )
      CALL CLRCH (  CONTROL_FILE )
      CALL CLRCH (    SPOOL_FILE )
      CALL CLRCH (        ARC_LN )
!
      CALL CLRCH ( SPOOL_DIR_FILE )
      CALL GETENVAR ( 'PSOLVE_SPOOL_DIR', SPOOL_DIR_FILE )
      IF ( ILEN(SPOOL_DIR_FILE) .EQ. 0 ) SPOOL_DIR_FILE = SPOOL_DIR
      ISPL = ILEN(SPOOL_DIR_FILE)
      IF ( SPOOL_DIR_FILE(ISPL:ISPL) .NE. '/' ) THEN
           ISPL = ISPL + 1
           SPOOL_DIR_FILE(ISPL:ISPL) = '/'
      ENDIF
!
      CONTROL_FILE  = OPA%TMP_DIR(1:I_LEN(OPA%TMP_DIR))//'opa_eopi_'//PID_STR// &
     &                '.cnt'
      EOPB_FILE     = OPA%TMP_DIR(1:I_LEN(OPA%TMP_DIR))//'opa_eopi_'//PID_STR// &
     &                '.eob'
      EOPB_BACKUP   = OPA%EOPB_FILE(1:I_LEN(OPA%EOPB_FILE))// &
     &                '.opa_eopb_bac_'//PID_STR
      NEW_EOPB_FILE = OPA%EOPB_FILE(1:I_LEN(OPA%EOPB_FILE))// &
     &                '.opa_new_eopb_'//PID_STR
      EOPI_BACKUP   = OPA%EOPB_FILE(1:I_LEN(OPA%EOPB_FILE))// &
     &                '.opa_eopi_bac_'//PID_STR
      NEW_EOPI_FILE = OPA%EOPS_FILE(1:I_LEN(OPA%EOPS_FILE))// &
     &                '.opa_new_eopi_'//PID_STR
      SPOOL_FILE    = SPOOL_DIR_FILE(1:ISPL)//'SPLF'//SOLVE_INIT
!
! --- Check solve lock. 
!
      CALL CHECK_SOLVE_LOCK()
!
! --- Read template control file to the buffer (but reserve first three
! --- lines for comments)
!
      CALL ERR_PASS ( IUER, IER )
      CALL RD_TEXT  ( OPA%EOPS_CNT, MBUF, BUF(4), NBUF, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG  ( 4363, IUER, 'OPA_EOPI', 'Error in reading '// &
     &                    'template control file '//OPA%EOPS_CNT )
           RETURN
      END IF
      NBUF = NBUF + 3
!
! --- Write down comments in to the buffer with control file
!
      BUF(1) = '*  This file was generated automatically by program '// &
     &         'OPA on '//GET_CDATE()
      BUF(2) = '*  using a template file '// &
     &         OPA%EOPS_CNT(1:I_LEN(OPA%EOPS_CNT))
      BUF(3) = '*  '
!
! --- Scan template files for patterns @cgm_file@, @erp_file@ and @arc_line@
!
      FL_E = .FALSE.
      FL_A = .FALSE.
!
      DO 410 J1=1,NBUF
         IPE = INDEX ( BUF(J1), '@erp_file@' )
         IF ( IPE .GT. 1 ) THEN
              BUF(J1) = BUF(J1)(1:IPE-1)// &
     &                  OPA%GEN_INPERP(1:I_LEN(OPA%GEN_INPERP))// &
     &                  '  SPL  UT1S '// &
     &                  BUF(J1)(IPE+LEN('@erp_file@'):)
              FL_E = .TRUE.
         END IF
!
         IPA = INDEX ( BUF(J1), '@arc_line@' )
         IF ( IPA .GT. 0 ) THEN
              ARC_LN = '  $'//OPA%DB_NAME//' '//DBV_STR//' '// &
     &                 OPA%ARC_LINE
!
! ----------- If found, replace with actual arc-line
!
              BUF(J1) = ARC_LN
              FL_A = .TRUE.
         END IF
 410  CONTINUE
!
! --- Check other signatures
!
      IF ( .NOT. FL_E ) THEN
           CALL ERR_LOG ( 4364, IUER, 'OPA_EOPI', 'Signature @erp_file@ '// &
     &         'was not found in a template control file '//OPA%EOPS_CNT )
           RETURN
      END IF
!
      IF ( .NOT. FL_A ) THEN
           CALL ERR_LOG ( 4365, IUER, 'OPA_EOPI', 'Signature @arc_line@ '// &
     &         'was not found in a template control file '//OPA%EOPS_CNT )
           RETURN
      END IF
!
! --- Remove the old control file if it exists
!
      IS = UNLINK ( CONTROL_FILE(1:I_LEN(CONTROL_FILE))//CHAR(0) )
!
! --- Write down updated control file
!
      CALL ERR_PASS ( IUER, IER )
      CALL WR_TEXT  ( NBUF, BUF, CONTROL_FILE, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 4366, IUER, 'OPA_EOPI', 'Error in attempt '// &
     &         'to write temporary control file '//CONTROL_FILE )
           RETURN
      END IF
!
! --- Build the line for launching Solve
!
      CALL CLRCH ( COMSTR  )
      COMSTR = PRE_SOL_DIR(1:PRE_SOL_LEN)//'solve '//SOLVE_INIT//' '// &
     &         CONTROL_FILE(1:I_LEN(CONTROL_FILE))//' silent'
      IF ( IVRB .GE. 1 ) WRITE ( 6, '(A)' ) ' Running Solve ...'
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
!
! -------- Completion code is not 0. Extract ISIG -- signal number which
! -------- caused termination of the command and ICOD -- completion code
!
           ISIG = 0
           ICOD = 0
           CALL MVBITS ( IS, 0, 8, ISIG, 0 )
           CALL MVBITS ( IS, 8, 8, ICOD, 0 )
           IF ( ICOD .GE. 128 ) ICOD = ICOD-256
!
           CALL CLRCH ( STR )
           CALL INCH  ( ICOD, STR )
           CALL ERR_LOG ( 4367, IUER, 'OPA_EOPI', 'Error '//STR(1:I_LEN(STR))// &
     &                      ' in executing command line '//COMSTR )
           RETURN
      END IF
!
! --- Block Solve user initials
!
      CALL ERR_PASS ( IUER, IER )
      LSUI = CHECK_SOLVE_INITIALS ( 'W', SOLVE_INIT(1:I_LEN(SOLVE_INIT)), IER )
!
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 4368, IUER, 'OPA_EOPI', 'Trap of internal '// &
     &         'control: error in checking solve initials '//SOLVE_INIT )
           RETURN
      END IF
!
      IF ( .NOT. LSUI ) THEN
           CALL ERR_LOG ( 4369, IUER, 'OPA_EOPI', 'Trap of internal '// &
     &         'control: Solve initials '//SOLVE_INIT//' are in use' )
           RETURN
      END IF
!
! --- Check, whether Solve completed successfully
!
      IF ( .NOT. CHECK_SOLVE_COMPLETE ( SOLVE_INIT ) ) THEN
           CALL ERR_LOG ( 4370, IUER, 'OPA_EOPI', 'Solve run was not '// &
     &                   'successfull' )
           RETURN
      END IF
!
! --- Remove control files which are not needed any more
!
      IS = UNLINK ( CONTROL_FILE(1:I_LEN(CONTROL_FILE))//CHAR(0) )
      IS = UNLINK ( CONTROL_FILE(1:I_LEN(CONTROL_FILE))//'.XPND'//CHAR(0) )
      IF ( IVRB .GE. 1 ) WRITE ( 6, '(A)' ) ' Parse spool file '// &
     &                              '...                                '
!
! --- Parse spool file and extract EOP values for this specific session
! --- in B-format. They will be written in EOPB_FILE
!
      CALL ERR_PASS ( IUER, IER )
      CALL SPOOL_TO_EOPB ( SPOOL_FILE, EOPB_FILE, OPA%MASTER_DIR, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 4371, IUER, 'OPA_EOPI', 'Error in attempt '// &
     &         'to parse spool file '//SPOOL_FILE(1:I_LEN(SPOOL_FILE))// &
     &         ' and create new EOPB file for this Solve run' )
           RETURN
      END IF
      IF ( IVRB .GE. 1 ) WRITE ( 6, '(A)' ) ' Insert new EOP into '// &
     &                                      'the global EOPB file ...'
!
! --- Check size of the EOPB file
!
      IS = FOR_STAT ( EOPB_FILE, STAT_BLOCK )
      IF ( STAT_BLOCK(8) .LE. 0 ) THEN
           CALL ERR_LOG ( 4372, IUER, 'OPA_EOPI', 'Trap of internal control: '// &
     &                   'EOP were not estimated and file '// &
     &                    EOPB_FILE(1:I_LEN(EOPB_FILE))//' is empty. Check '// &
     &                   'template control file '//OPA%EOPS_CNT )
           RETURN
      END IF
!
! --- Open the old input EOPB file
!
      LUNI = GET_UNIT ()
      OPEN ( UNIT=LUNI, FILE=OPA%EOPB_FILE, STATUS='OLD', IOSTAT=IO )
      IF ( IO .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( IO, STR )
           CALL ERR_LOG ( 4373, IUER, 'OPA_EOPI', 'Error '//STR(1:I_LEN(STR))// &
     &          ' in attempt to open EOPB file '//OPA%EOPB_FILE )
           RETURN
      END IF
!
! --- Open the new output eopb file
!
      LUNO = GET_UNIT ()
      OPEN ( UNIT=LUNO, FILE=NEW_EOPB_FILE, STATUS='UNKNOWN', IOSTAT=IO )
      IF ( IO .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( IO, STR )
           CALL ERR_LOG ( 4374, IUER, 'OPA_EOPI', 'Error '//STR(1:I_LEN(STR))// &
     &         ' in attempt to open output EOPB file '//NEW_EOPB_FILE )
           CLOSE ( LUNI )
           RETURN
      END IF
!
! --- Parse the database name and to transform it to the form
! --- YYYYMMMDDD.SS
!
      DB_NM = '          .  '
      CALL ERR_PASS ( IUER, IER )
      CALL PARSE_DBNAME ( OPA%DB_NAME, DB_NM(1:10), DB_NM(12:13), IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 4375, IUER, 'OPA_EOPI', 'Trap of internal '// &
     &         'control: wrong format of the database name which is '// &
     &         'being processed' )
           RETURN
      END IF
!
      FL_INSERT = .FALSE.
      LEN_EOB = SIZEOF ( EOB )
!
! --- Read global eopb file
!
      DO 420 J2=1,1024*1024*1024
         DB_NM_READ = '          .  '
         READ ( LUNI, FMT='(A)', IOSTAT=IO ) EOB_STR ! read a line
         CALL LIB$MOVC3 ( LEN_EOB, %REF(EOB_STR), EOB )
         IF ( IO .EQ. -1 ) THEN
              GOTO 820
            ELSE IF ( IO .NE. 0 ) THEN
              CALL CLRCH ( STR )
              CALL INCH  ( IO, STR )
              WRITE ( 6, * ) ' line ',J2
              CALL ERR_LOG ( 4376, IUER, 'OPA_EOPI', 'Error '// &
     &             STR(1:I_LEN(STR))//' in attempt to read EOPB file '// &
     &             OPA%EOPB_FILE )
              RETURN
         END IF
!
         IF ( EOB_STR(1:1)    .NE. '#'  .AND. &
     &        EOB%DBNAME(1:1) .EQ. '$'         ) THEN
!
! ----------- Parse the database file name from the EOPS file and
! ----------- build a string DB_NM_READ for comparing
!
              CALL ERR_PASS ( IUER, IER )
              CALL PARSE_DBNAME ( EOB%DBNAME(2:10) , DB_NM_READ(1:10), &
     &                            DB_NM_READ(12:13), IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 4377, IUER, 'OPA_EOPI', 'Trap of '// &
     &                 'internal control: wrong format of the database '// &
     &                 'name '//EOB%DBNAME(1:10) )
                   RETURN
              END IF
!
! ----------- Now comapre transformed database filename: the targeted
! ----------- database name and the database file name taken from EOPS file
!
              IF ( DB_NM_READ .GE. DB_NM  .AND. .NOT. FL_INSERT ) THEN
!
! ---------------- It is just the place to insert new eopb values
! ---------------- Read the new eopb values
!
                   CALL RD_TEXT ( EOPB_FILE, MBUF, BUF, NBUF, IER )
                   IF ( IER .NE. 0 ) THEN
                        CALL ERR_LOG ( 4378, IUER, 'OPA_EOPI', 'Error in '// &
     &                      'attempt to read the input EOPB file which '// &
     &                      'just has been created: '//NEW_EOPB_FILE )
                        CLOSE ( LUNO )
                        CLOSE ( LUNI )
                        RETURN
                   END IF
!
! ---------------- Write it down in the output file
!
                   DO 430 J3=1,NBUF
                      IF ( BUF(J3)(1:1) .NE. '#' ) THEN
                           WRITE ( LUNO, '(A)' ) BUF(J3)(1:I_LEN(BUF(J3)))
                      END IF
 430               CONTINUE
                   FL_INSERT = .TRUE.
              END IF
         END IF ! str is not  comment
         IF ( EOB_STR(1:13) .EQ. '# Spool file:' ) THEN
              EOB_STR = '# Initial spool file: '//EOB_STR(15:)
         END IF
!
         IF ( DB_NM_READ .NE. DB_NM ) THEN
!
! ----------- Copy the input line to the output file unless it is the line
! ----------- for this experiment
!
              CALL LIB$MOVC3 ( LEN_EOB, EOB, %REF(EOB_STR)  )
              WRITE ( LUNO, '(A)' ) EOB_STR(1:I_LEN(EOB_STR))
         END IF
         IF ( EOB_STR(1:10) .EQ. '# Analysis' ) THEN
!
! ----------- Get user's real name
!
              CALL GETINFO_USER ( USER_NAME, USER_REALNAME, USER_E_ADDRESS )
              IF ( ILEN(USER_REALNAME) .EQ. 0 ) USER_REALNAME = USER_E_ADDRESS
!
! ----------- ... and write down the current date of update and user's name
!
              WRITE ( LUNO, '(A)', IOSTAT=IO ) '# Updated by OPA at      '// &
     &                GET_CDATE()//'  by '// &
     &                 USER_REALNAME(1:I_LEN(USER_REALNAME))
         END IF
 420  CONTINUE
 820  CONTINUE
!
      IF ( .NOT. FL_INSERT ) THEN
!
! --------- Data were not inserted. It means that we have to insert them
! --------- after the last line of the input EOPB file.
! --------- Read the new input EOPB file
!
            CALL RD_TEXT ( EOPB_FILE, MBUF, BUF, NBUF, IER )
            IF ( IER .NE. 0 ) THEN
                 CALL ERR_LOG ( 4379, IUER, 'OPA_EOPI', 'Error in attempt '// &
     &               'to read the input EOPB file which just has been '// &
     &               'created: '//EOPB_FILE )
                 CLOSE ( LUNI )
                 CLOSE ( LUNO )
                 RETURN
            END IF
!
! --------- ... and its contents to the end of the global EOPB file
!
            DO 440 J4=1,NBUF
               IF ( BUF(J4)(1:1) .NE. '#' ) THEN
                    WRITE ( LUNO, '(A)' ) BUF(J4)(1:I_LEN(BUF(J4)))
               END IF
 440        CONTINUE
      END IF
!
      CLOSE ( UNIT = LUNI )
      CLOSE ( UNIT = LUNO )
!
      IF ( IVRB .GE. 1 ) WRITE ( 6, '(A)' ) ' Copying EOPB files ...'
!
! --- Copy  OPA.EOPB_FILE --> EOPB_BACKUP
!
      CALL ERR_PASS ( IUER, IER )
      CALL COPY_ASCII_FILE ( OPA%EOPB_FILE, EOPB_BACKUP, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 4380, IUER, 'OPA_EOPI', 'Error in attempt '// &
     &         'to make a backup copy of the old EOPB file' )
           RETURN
      END IF
!
! --- Rename NEW_EOPB_FILE --> OPA.EOPB_FILE
! --- This trick with first copying file and then renaming is for minimizing
! --- time of file update. At the same time we are are sure
! --- that NEW_EOPB_FILE and OPA.EOPB_FILE are at the same disk system and
! --- therefore operation of renaming is legitimate
!
      IS = RENAME ( NEW_EOPB_FILE, OPA%EOPB_FILE )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 4381, IUER, 'OPA_EOPI', 'Serious error '// &
     &         'in attempt to move the new EOPB file to the old place. '// &
     &         'The old file '//OPA%EOPB_FILE(1:I_LEN(OPA%EOPB_FILE))// &
     &         ' is probably spoiled!!! Check the backup copy '// &
     &          EOPB_BACKUP )
           RETURN
      END IF
!
! --- Remove temporary EOPB files
!
      IS = UNLINK ( EOPB_BACKUP(1:I_LEN(EOPB_BACKUP))//CHAR(0) )
      IS = UNLINK ( EOPB_FILE(1:I_LEN(EOPB_FILE))//CHAR(0) )
      IF ( IVRB .GE. 1 ) WRITE ( 6, '(A)' ) ' Copying EOPI files ...'
!
! --- Copy  OPA.EOPS_FILE --> EOPI_BACKUP
!
      CALL ERR_PASS ( IUER, IER )
      CALL COPY_ASCII_FILE ( OPA%EOPS_FILE, EOPI_BACKUP, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 4382, IUER, 'OPA_EOPI', 'Error in attempt '// &
     &         'to make a backup copy of the old EOPI file' )
           RETURN
      END IF
!
! --- Read the old input EOPB file
!
      CALL ERR_PASS ( IUER, IER )
      CALL READ_EOB ( OPA%EOPB_FILE, M_HEA, N_HEA, HEA_BUF, M_SES, N_SES, &
     &                EOP, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 4383, IUER, 'OPA_EOPI', 'Error in an attempt '// &
     &         'to read the eop file '//OPA%EOPB_FILE )
           RETURN
      END IF
!
! --- Get directory for master files
!
      CALL GETENVAR ( 'PSOLVE_HELP_DIR', SOLVE_HELP_DIR_STR )
      IF ( ILEN(SOLVE_HELP_DIR_STR) .LE. 0 ) THEN
           SOLVE_HELP_DIR_STR = SOLVE_HELP_DIR
      END IF
      IF ( SOLVE_HELP_DIR_STR(I_LEN(SOLVE_HELP_DIR_STR):I_LEN(SOLVE_HELP_DIR_STR)) .NE. '/' ) THEN
           SOLVE_HELP_DIR_STR = SOLVE_HELP_DIR_STR(1:I_LEN(SOLVE_HELP_DIR_STR))//'/'
      END IF
!
      EOPS_HELP = SOLVE_HELP_DIR(1:I_LEN(SOLVE_HELP_DIR))//EOPS__HELP_FILE
!
! --- Read help-file with additional information about eops file. It
! --- will be treated as comments
!
      CALL ERR_PASS ( IUER, IER )
      CALL RD_TEXT  ( EOPS_HELP, M_HEA-N_HEA, HEA_BUF(N_HEA+1), N_HLP, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 4384, IUER, 'OPA_EOPI', 'Error in an attempt '// &
     &         'to read the file '//EOPS_HELP(1:I_LEN(EOPS_HELP))// &
     &         'with description of eops format' )
           RETURN
      END IF
!
      N_HEA = N_HEA + N_HLP
      IF ( N_HEA .GT. M_HEA ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( M_HEA, STR )
           CALL ERR_LOG ( 4385, IUER, 'OPA_EOPI', 'Too long header of EOB '// &
     &         'file '//OPA%EOPB_FILE(1:I_LEN(OPA%EOPB_FILE))//' -- '// &
     &        'parameter M_HEA: '//STR(1:I_LEN(STR))//' is not enough' )
           RETURN
      END IF
!
! --- Add comment prefix if needed
!
      DO 450 J5=1,N_HEA
         IF ( HEA_BUF(J5)(1:1) .NE. '#' ) HEA_BUF(J5) = '# '//HEA_BUF(J5)
 450  CONTINUE
!
! --- Write down the file in EOPS format
!
      CALL ERR_PASS ( IUER, IER )
      CALL WRITE_EOPS ( NEW_EOPI_FILE, N_HEA, HEA_BUF, N_SES, EOP, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 4386, IUER, 'OPA_EOPI', 'Error in an attempt '// &
     &         'to wrint in the eops file '//NEW_EOPI_FILE )
           RETURN
      END IF
!
! --- Rename NEW_EOPI_FILE --> OPA.EOPS_FILE
!
      IS = RENAME ( NEW_EOPI_FILE, OPA%EOPS_FILE )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 4387, IUER, 'OPA_EOPI', 'Serious error '// &
     &         'in attempt to move the new EOPI file to the old place. '// &
     &         'The old file '//OPA%EOPS_FILE(1:I_LEN(OPA%EOPS_FILE))// &
     &         ' is probably spoiled!!! Check the backup copy '// &
     &          EOPI_BACKUP )
           RETURN
      END IF
!
! --- Remove temporary EOPS files
!
      IS = UNLINK ( EOPI_BACKUP(1:I_LEN(EOPI_BACKUP))//CHAR(0) )
      IS = UNLINK ( NEW_EOPI_FILE(1:I_LEN(NEW_EOPI_FILE))//CHAR(0) )
!
! --- Remove lock from the current Solve user initials
!
      CALL REMOVE_SOLVE_LOCK()
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  OPA_EOPI  #!#
