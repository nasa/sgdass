      SUBROUTINE OPA_EOPM ( SOLVE_INIT, OPA, OPC_FILE, IVRB, IUER )
! ************************************************************************
! *                                                                      *
! *   Subroutine  OPA_EOPM  executes the action of OPA: it updates       *
! *   EOPM (EOP submission, conservative style, multi-baseline           *
! *   solutions, Intensive experiments)  file for contribution of this   *
! *   specific databases OPA%DB_NAME .                                   *
! *   It makes the Solve batch independent solution:                     *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! * SOLVE_INIT ( CHARACTER ) -- Solve user initials.                     *
! *        OPA ( RECORD    ) -- Data structure which keeps internal      *
! *                             information of OPA: configuration        *
! *                             parameters, session name, status codes,  *
! *                             action codes.                            *
! *   OPC_FILE ( CHARACTER ) -- File name which keeps OPA configuration  *
! *                             for this session.                        *
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
! *  ### 25-SEP-2007     OPA_EOPM  v1.0 (c)  L. Petrov  25-SEP-2007 ###  *
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
      CHARACTER  OPC_FILE*(*)
      INTEGER*4  IVRB, IUER
      INTEGER*4  MBUF, M_HEA, MU_STA, MU_BAS
      PARAMETER  ( MBUF = 1024 )
      PARAMETER  ( M_HEA =  512 )
      PARAMETER  ( MU_STA = MAX_ARC_STA )
      PARAMETER  ( MU_BAS = MAX_ARC_BSL )
      CHARACTER  CONTROL_FILE*128, SPOOL_FILE*128, SPOOL_DIR_FILE*128, &
     &           EOPT_FILE*128, EOPT_BACKUP*128, NEW_EOPT_FILE*128, &
     &                          EOPM_BACKUP*128, NEW_EOPM_FILE*128, &
     &           COMSTR*256, LOCK_FINAM*128, BUF(MBUF)*512, PID_STR*5, &
     &           EXC_LN*512, ARC_LN*512, DBV_STR*3, DB_NM*13, DB_NM_READ*13, &
     &           STR*512, EOB_STR*512, SOLVE_HELP_DIR_STR*128, EOPS_HELP*128, &
     &           SOLVE_INIT*2, SPOOL_STND*128
      CHARACTER  HEA_BUF(M_HEA)*128
      CHARACTER  USER_NAME*128, USER_REALNAME*128, USER_E_ADDRESS*128, &
     &           C_STA(MU_STA)*8, C_BAS(MU_BAS)*17
      LOGICAL*4  LEX, LSUI, CHECK_SOLVE_INITIALS, CHECK_SOLVE_COMPLETE
      INTEGER*4  NBUF, PID, IPA, IPE, IO, LUNI, LUNO, ISPL, ISIG, ICOD, &
     &           STAT_BLOCK(12), IS, N_HEA, N_HLP, N_SES, N_EXC, LEN_EOB, &
     &           L_STA, L_BAS, J1, J2, J3, J4, J5, J6, J7, J8, IER
      LOGICAL*4  FL_A, FL_E, FL_INSERT
      CHARACTER, EXTERNAL :: GET_CDATE*19
      INTEGER*4, EXTERNAL :: ILEN, I_LEN, GETPID, GET_UNIT, RENAME, &
     &                       FOR_STAT, SYSTEM, UNLINK
!
! --- Check EOPM file
!
      INQUIRE ( FILE=OPA%EOPM_FILE, EXIST=LEX )
      IF ( .NOT. LEX ) THEN
           CALL ERR_LOG ( 4361, IUER, 'OPA_EOPM', 'The old EOPM file '// &
     &          OPA%EOPM_FILE(1:I_LEN(OPA%EOPM_FILE))//' was not found. '// &
     &          'It should exist before OPA_EOPM will try to update it ' )
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
      CALL CLRCH (     EOPT_FILE )
      CALL CLRCH (   EOPT_BACKUP )
      CALL CLRCH ( NEW_EOPT_FILE )
      CALL CLRCH (   EOPM_BACKUP )
      CALL CLRCH ( NEW_EOPM_FILE )
      CALL CLRCH (  CONTROL_FILE )
      CALL CLRCH (    SPOOL_FILE )
      CALL CLRCH (        ARC_LN )
!
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
      CONTROL_FILE  = OPA%TMP_DIR(1:I_LEN(OPA%TMP_DIR))//'opa_eopm_'//PID_STR// &
     &                '.cnt'
      EOPT_FILE     = OPA%TMP_DIR(1:I_LEN(OPA%TMP_DIR))//'opa_eopt_'//PID_STR// &
     &                '.eob'
      EOPT_BACKUP   = OPA%EOPT_FILE(1:I_LEN(OPA%EOPT_FILE))// &
     &                '.opa_eopt_bac_'//PID_STR
      NEW_EOPT_FILE = OPA%EOPT_FILE(1:I_LEN(OPA%EOPT_FILE))// &
     &                '.opa_new_eopt_'//PID_STR
      EOPM_BACKUP   = OPA%EOPT_FILE(1:I_LEN(OPA%EOPT_FILE))// &
     &                '.OPA_EOPM_bac_'//PID_STR
      NEW_EOPM_FILE = OPA%EOPM_FILE(1:I_LEN(OPA%EOPM_FILE))// &
     &                '.opa_new_EOPM_'//PID_STR
      SPOOL_FILE    = SPOOL_DIR_FILE(1:ISPL)//'SPLF'//SOLVE_INIT
      SPOOL_STND = OPC_FILE(1:ISPL)//'.spl'
!
      INQUIRE ( FILE = SPOOL_STND, EXIST = LEX )
      IF ( .NOT. LEX ) THEN
           CALL ERR_LOG  ( 4381, IUER, 'OPA_EOPM', 'Trap of internal '// &
     &                    'control: cannot find the listing of the '// &
     &                    'standalone solution '// &
     &                     SPOOL_STND(1:I_LEN(SPOOL_STND))//' . Try to run '// &
     &                    'standalone solution once again' )
           RETURN
      END IF
!
      CALL ERR_PASS ( IUER, IER ) 
      CALL LISBAS_FROM_SPOOL ( SPOOL_FILE, OPA%MASTER_DIR, OPA%NUM_USED_MIN, &
     &                         MU_STA, L_STA, C_STA, &
     &                         MU_BAS, L_BAS, C_BAS, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 4382, IUER, 'OPA_EOPM', 'Error in an attempt '// &
     &         'to get the number of baselines, number of stations, '// &
     &         'the list of baselines, list of stations from the spool '// &
     &         'file '//SPOOL_FILE )
           RETURN 
      END IF
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
           CALL ERR_LOG  ( 4363, IUER, 'OPA_EOPM', 'Error in reading '// &
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
              BUF(J1) = '*' 
              FL_A = .TRUE.
         END IF
 410  CONTINUE
!
! --- Check other signatures
!
      IF ( .NOT. FL_E ) THEN
           CALL ERR_LOG ( 4364, IUER, 'OPA_EOPM', 'Signature @erp_file@ '// &
     &         'was not found in a template control file '//OPA%EOPS_CNT )
           RETURN
      END IF
!
      IF ( .NOT. FL_A ) THEN
           CALL ERR_LOG ( 4365, IUER, 'OPA_EOPM', 'Signature @arc_line@ '// &
     &         'was not found in a template control file '//OPA%EOPS_CNT )
           RETURN
      END IF
!
      IF ( .NOT. OPA%EOPM_ONLY_SINGLE_BASELINE ) THEN
!
! -------- Add the session line wihtout station exceptions
!
           NBUF = NBUF + 1
           BUF(NBUF) = '  $'//OPA%DB_NAME//' '//DBV_STR//' '// &
     &               OPA%ARC_LINE
      END IF
!
      IF ( L_BAS > 1 ) THEN
           CALL CLRCH ( EXC_LN )
           EXC_LN = 'STA_EXCLUDE  0 '
           DO 420 J2=1,L_BAS
              N_EXC = 0
              DO 430 J3=1,L_STA
                 IF ( C_STA(J3) .NE. C_BAS(J2)(1:8)   .AND. &
     &                C_STA(J3) .NE. C_BAS(J2)(10:17)       ) THEN
                      N_EXC = N_EXC + 1
                      EXC_LN(16+(N_EXC-1)*10:16+(N_EXC-1)*10+8) = C_STA(J3)
                      CALL INCH   ( N_EXC, EXC_LN(13:14) )
                      CALL CHASHR (        EXC_LN(13:14) )
                 END IF
 430          CONTINUE 
              ARC_LN = '  $'//OPA%DB_NAME//' '//DBV_STR//' '// &
     &               OPA%ARC_LINE
              NBUF = NBUF + 1
              BUF(NBUF) = '  $'//OPA%DB_NAME//' '//DBV_STR//' '// &
     &                    EXC_LN(1:I_LEN(EXC_LN))//' '//OPA%ARC_LINE
 420       CONTINUE 
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
           CALL ERR_LOG ( 4366, IUER, 'OPA_EOPM', 'Error in attempt '// &
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
           CALL ERR_LOG ( 4367, IUER, 'OPA_EOPM', 'Error '//STR(1:I_LEN(STR))// &
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
           CALL ERR_LOG ( 4368, IUER, 'OPA_EOPM', 'Trap of internal '// &
     &         'control: error in checking solve initials '//SOLVE_INIT )
           RETURN
      END IF
!
      IF ( .NOT. LSUI ) THEN
           CALL ERR_LOG ( 4369, IUER, 'OPA_EOPM', 'Trap of internal '// &
     &         'control: Solve initials '//SOLVE_INIT//' are in use' )
           RETURN
      END IF
!
! --- Check, whether Solve completed successfully
!
      IF ( .NOT. CHECK_SOLVE_COMPLETE ( SOLVE_INIT ) ) THEN
           CALL ERR_LOG ( 4370, IUER, 'OPA_EOPM', 'Solve run was not '// &
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
! --- in B-format. They will be written in EOPT_FILE
!
      CALL ERR_PASS ( IUER, IER )
      CALL SPOOL_TO_EOPB ( SPOOL_FILE, EOPT_FILE, OPA%MASTER_DIR, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 4371, IUER, 'OPA_EOPM', 'Error in attempt '// &
     &         'to parse spool file '//SPOOL_FILE(1:I_LEN(SPOOL_FILE))// &
     &         ' and create new EOPT file for this Solve run' )
           RETURN
      END IF
      IF ( IVRB .GE. 1 ) WRITE ( 6, '(A)' ) ' Insert new EOP into '// &
     &                                      'the global EOPT file ...'
!
! --- Check size of the EOPT file
!
      IS = FOR_STAT ( EOPT_FILE, STAT_BLOCK )
      IF ( STAT_BLOCK(8) .LE. 0 ) THEN
           CALL ERR_LOG ( 4372, IUER, 'OPA_EOPM', 'Trap of internal control: '// &
     &                   'EOP were not estimated and file '// &
     &                    EOPT_FILE(1:I_LEN(EOPT_FILE))//' is empty. Check '// &
     &                   'template control file '//OPA%EOPS_CNT )
           RETURN
      END IF
!
! --- Open the old input EOPT file
!
      LUNI = GET_UNIT ()
      OPEN ( UNIT=LUNI, FILE=OPA%EOPT_FILE, STATUS='OLD', IOSTAT=IO )
      IF ( IO .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( IO, STR )
           CALL ERR_LOG ( 4373, IUER, 'OPA_EOPM', 'Error '//STR(1:I_LEN(STR))// &
     &          ' in attempt to open EOPT file '//OPA%EOPT_FILE )
           RETURN
      END IF
!
! --- Open the new output EOPT file
!
      LUNO = GET_UNIT ()
      OPEN ( UNIT=LUNO, FILE=NEW_EOPT_FILE, STATUS='UNKNOWN', IOSTAT=IO )
      IF ( IO .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( IO, STR )
           CALL ERR_LOG ( 4374, IUER, 'OPA_EOPM', 'Error '//STR(1:I_LEN(STR))// &
     &         ' in attempt to open output EOPT file '//NEW_EOPT_FILE )
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
           CALL ERR_LOG ( 4375, IUER, 'OPA_EOPM', 'Trap of internal '// &
     &         'control: wrong format of the database name which is '// &
     &         'being processed' )
           RETURN
      END IF
!
      FL_INSERT = .FALSE.
      LEN_EOB = SIZEOF ( EOB )
!
! --- Read global EOPT file
!
      DO 440 J4=1,1024*1024*1024
         DB_NM_READ = '          .  '
         READ ( LUNI, FMT='(A)', IOSTAT=IO ) EOB_STR ! read a line
         CALL LIB$MOVC3 ( LEN_EOB, %REF(EOB_STR), EOB )
         IF ( IO .EQ. -1 ) THEN
              GOTO 840
            ELSE IF ( IO .NE. 0 ) THEN
              CALL CLRCH ( STR )
              CALL INCH  ( IO, STR )
              WRITE ( 6, * ) ' line ',J4
              CALL ERR_LOG ( 4376, IUER, 'OPA_EOPM', 'Error '// &
     &             STR(1:I_LEN(STR))//' in attempt to read EOPT file '// &
     &             OPA%EOPT_FILE )
              RETURN
         END IF
!
         IF ( EOB_STR(1:1)    .NE. '#'  .AND. &
     &        EOB%DBNAME(1:1) .EQ. '$'         ) THEN
!
! ----------- Parse the database file name from the EOPM file and
! ----------- build a string DB_NM_READ for comparing
!
              CALL ERR_PASS ( IUER, IER )
              CALL PARSE_DBNAME ( EOB%DBNAME(2:10) , DB_NM_READ(1:10), &
     &                            DB_NM_READ(12:13), IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 4377, IUER, 'OPA_EOPM', 'Trap of '// &
     &                 'internal control: wrong format of the database '// &
     &                 'name '//EOB%DBNAME(1:10) )
                   RETURN
              END IF
!
! ----------- Now comapre transformed database filename: the targeted
! ----------- database name and the database file name taken from EOPM file
!
              IF ( DB_NM_READ .GE. DB_NM  .AND. .NOT. FL_INSERT ) THEN
!
! ---------------- It is just the place to insert new EOPT values
! ---------------- Read the new EOPT values
!
                   CALL RD_TEXT ( EOPT_FILE, MBUF, BUF, NBUF, IER )
                   IF ( IER .NE. 0 ) THEN
                        CALL ERR_LOG ( 4378, IUER, 'OPA_EOPM', 'Error in '// &
     &                      'attempt to read the input EOPT file which '// &
     &                      'just has been created: '//NEW_EOPT_FILE )
                        CLOSE ( LUNO )
                        CLOSE ( LUNI )
                        RETURN
                   END IF
!
! ---------------- Write it down in the output file
!
                   DO 450 J5=1,NBUF
                      IF ( BUF(J5)(1:1) .NE. '#' ) THEN
                           WRITE ( LUNO, '(A)' ) BUF(J5)(1:I_LEN(BUF(J5)))
                      END IF
 450               CONTINUE
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
 440  CONTINUE
 840  CONTINUE
!
      IF ( .NOT. FL_INSERT ) THEN
!
! --------- Data were not inserted. It means that we have to insert them
! --------- after the last line of the input EOPT file.
! --------- Read the new input EOPT file
!
            CALL RD_TEXT ( EOPT_FILE, MBUF, BUF, NBUF, IER )
            IF ( IER .NE. 0 ) THEN
                 CALL ERR_LOG ( 4379, IUER, 'OPA_EOPM', 'Error in attempt '// &
     &               'to read the input EOPT file which just has been '// &
     &               'created: '//EOPT_FILE )
                 CLOSE ( LUNI )
                 CLOSE ( LUNO )
                 RETURN
            END IF
!
! --------- ... and its contents to the end of the global EOPT file
!
            DO 460 J6=1,NBUF
               IF ( BUF(J6)(1:1) .NE. '#' ) THEN
                    WRITE ( LUNO, '(A)' ) BUF(J6)(1:I_LEN(BUF(J6)))
               END IF
 460        CONTINUE
      END IF
!
      CLOSE ( UNIT = LUNI )
      CLOSE ( UNIT = LUNO )
!
      IF ( IVRB .GE. 1 ) WRITE ( 6, '(A)' ) ' Copying EOPT files ...'
!
! --- Copy  OPA%EOPT_FILE --> EOPT_BACKUP
!
      CALL ERR_PASS ( IUER, IER )
      CALL COPY_ASCII_FILE ( OPA%EOPT_FILE, EOPT_BACKUP, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 4380, IUER, 'OPA_EOPM', 'Error in attempt '// &
     &         'to make a backup copy of the old EOPT file' )
           RETURN
      END IF
!
! --- Rename NEW_EOPT_FILE --> OPA%EOPT_FILE
! --- This trick with first copying file and then renaming is for minimizing
! --- time of file update. At the same time we are are sure
! --- that NEW_EOPT_FILE and OPA%EOPT_FILE are at the same disk system and
! --- therefore operation of renaming is legitimate
!
      IS = RENAME ( NEW_EOPT_FILE, OPA%EOPT_FILE )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 4381, IUER, 'OPA_EOPM', 'Serious error '// &
     &         'in attempt to move the new EOPT file to the old place. '// &
     &         'The old file '//OPA%EOPT_FILE(1:I_LEN(OPA%EOPT_FILE))// &
     &         ' is probably spoiled!!! Check the backup copy '// &
     &          EOPT_BACKUP )
           RETURN
      END IF
!
! --- Remove temporary EOPT files
!
      IS = UNLINK ( EOPT_BACKUP(1:I_LEN(EOPT_BACKUP))//CHAR(0) )
      IS = UNLINK ( EOPT_FILE(1:I_LEN(EOPT_FILE))//CHAR(0) )
      IF ( IVRB .GE. 1 ) WRITE ( 6, '(A)' ) ' Copying EOPM files ...'
!
! --- Copy  OPA%EOPM_FILE --> EOPM_BACKUP
!
      CALL ERR_PASS ( IUER, IER )
      CALL COPY_ASCII_FILE ( OPA%EOPM_FILE, EOPM_BACKUP, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 4382, IUER, 'OPA_EOPM', 'Error in attempt '// &
     &         'to make a backup copy of the old EOPM file' )
           RETURN
      END IF
!
! --- Read the old input EOPT file
!
      CALL ERR_PASS ( IUER, IER )
      CALL READ_EOB ( OPA%EOPT_FILE, M_HEA, N_HEA, HEA_BUF, M_SES, N_SES, &
     &                EOP, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 4383, IUER, 'OPA_EOPM', 'Error in an attempt '// &
     &         'to read the eop file '//OPA%EOPT_FILE )
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
! --- Read help-file with additional information about eopm file. It
! --- will be treated as comments
!
      CALL ERR_PASS ( IUER, IER )
      CALL RD_TEXT  ( EOPS_HELP, M_HEA-N_HEA, HEA_BUF(N_HEA+1), N_HLP, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 4384, IUER, 'OPA_EOPM', 'Error in an attempt '// &
     &         'to read the file '//EOPS_HELP(1:I_LEN(EOPS_HELP))// &
     &         'with description of eops format' )
           RETURN
      END IF
!
      N_HEA = N_HEA + N_HLP
      IF ( N_HEA .GT. M_HEA ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( M_HEA, STR )
           CALL ERR_LOG ( 4385, IUER, 'OPA_EOPM', 'Too long header of EOB '// &
     &         'file '//OPA%EOPT_FILE(1:I_LEN(OPA%EOPT_FILE))//' -- '// &
     &        'parameter M_HEA: '//STR(1:I_LEN(STR))//' is not enough' )
           RETURN
      END IF
!
! --- Add comment prefix if needed
!
      DO 470 J7=1,N_HEA
         IF ( HEA_BUF(J7)(1:1) .NE. '#' ) HEA_BUF(J7) = '# '//HEA_BUF(J7)
 470  CONTINUE
!
! --- Write down the file in EOPS format
!
      CALL ERR_PASS ( IUER, IER )
      CALL WRITE_EOPS ( NEW_EOPM_FILE, N_HEA, HEA_BUF, N_SES, EOP, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 4386, IUER, 'OPA_EOPM', 'Error in an attempt '// &
     &         'to wrint in the eopm file '//NEW_EOPM_FILE )
           RETURN
      END IF
!
! --- Rename NEW_EOPM_FILE --> OPA.EOPM_FILE
!
      IS = RENAME ( NEW_EOPM_FILE, OPA%EOPM_FILE )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 4387, IUER, 'OPA_EOPM', 'Serious error '// &
     &         'in attempt to move the new EOPM file to the old place. '// &
     &         'The old file '//OPA%EOPM_FILE(1:I_LEN(OPA%EOPM_FILE))// &
     &         ' is probably spoiled!!! Check the backup copy '// &
     &          EOPM_BACKUP )
           RETURN
      END IF
!
! --- Remove temporary EOPM files
!
      IS = UNLINK ( EOPM_BACKUP(1:I_LEN(EOPM_BACKUP))//CHAR(0) )
      IS = UNLINK ( NEW_EOPM_FILE(1:I_LEN(NEW_EOPM_FILE))//CHAR(0) )
!
! --- Remove lock from the current Solve user initials
!
      CALL REMOVE_SOLVE_LOCK()
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  OPA_EOPM  #!#
