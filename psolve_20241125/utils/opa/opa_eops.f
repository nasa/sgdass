      SUBROUTINE OPA_EOPS ( SOLVE_INIT, OPA, IVRB, IUER )
! ************************************************************************
! *                                                                      *
! *   Subroutine  OPA_EOPS  executes the action of OPA: it updates       *
! *   EOPS (EOP submission, conservative style) file for contribution of *
! *   this specific databases OPA.DB_NAME . It makes two Solve batch     *
! *   global solutions:                                                  *
! *     1) It makes the first solution with EOP substitution file used   *
! *        for generic global solution. Solve uses iput cgm produced     *
! *        during generic solution and one arc: OPA_DB_NAME. The same    *
! *        EOP mapping file as in generic solution is used. NO EOP mod   *
! *        filre is epplied to the experiment OPA_DB_NAME.               *
! *                                                                      *
! *        Then it extracts EOP values for this session and writes them  *
! *        in the EOB format.                                            *
! *                                                                      *
! *        After that it insert EOP values for this session in the file  *
! *        last.eopb .                                                   *
! *                                                                      *
! *        Then it runs Kalman filter by using last.eopb as the input    *
! *        file and writes doen reslts in last.erp                       *
! *                                                                      *
! *     2) The second differes from the first only by two points:        *
! *        a) last.erp file produced in the first solution is used as    *
! *           a mod-file.                                                *
! *        b) EOP modfile is applied for the session OPA.DB_NAME also.   *
! *                                                                      *
! *        After completion soltution two, file last.eob is transfored   *
! *        to file last.eops .                                           *
! *                                                                      *
! *        Optional fields in EOPS file are defined as follows:          *
! *        ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~          *
! *                                                                      *
! *        Field 17 -- number of observations used in analysis.          *
! *        Field 18 -- experiment's category. Experiment category is     *
! *                    derived from the databases suffix by the          *
! *                    following way: ICHAR(SUFFIX(2:2)) - 64.           *
! *                    For example, it is 1 for XA, 5 for XE databases.  *
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
! *  ### 18-SEP-2000    OPA_EOPS   v1.3 (c)  L. Petrov  06-JUN-2002 ###  *
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
      PARAMETER  ( MBUF  = 1024 )
      PARAMETER  ( M_HEA =  512 )
!
      CHARACTER  COMSTR*256, LOCK_FINAM*128, BUF(MBUF)*512, PID_STR*5, &
     &           EOPB_FILE*128, CONTROL_FILE*128, ARC_LN*128, DBV_STR*3, &
     &           DB_NM*13, DB_NM_READ*13, STR*512, EOB_STR*512, &
     &           EOPB_BACKUP*128, EOPS_BACKUP*128, NEW_EOPB_FILE*128, &
     &           NEW_EOPK_FILE*128, NEW_EOPS_FILE*128, SPOOL_DIR_FILE*128, &
     &           SPOOL_FILE*128, EOPK_BACKUP*128, EOPK_STS_FILE*128, &
     &           EOPK_PRG_FILE*128, SOLVE_HELP_DIR_STR*128, EOPS_HELP*128
      CHARACTER  HEA_BUF(M_HEA)*128
      CHARACTER  USER_NAME*128, USER_REALNAME*128, USER_E_ADDRESS*128
      CHARACTER  GET_CDATE*19
      LOGICAL*4  LEX, LSUI, CHECK_SOLVE_INITIALS, CHECK_SOLVE_COMPLETE
      INTEGER*4  NBUF, PID, IPA, IPE, IPS, IO, LUNI, LUNO, ISPL, ISIG, ICOD, &
     &           STAT_BLOCK(12), IS, N_HEA, N_HLP, N_SES, LEN_EOB, &
     &           J1, J2, J3, J4, J5, J6, IER
      LOGICAL*4  FL_A, FL_E, FL_S, FL_INSERT
      INTEGER*4, EXTERNAL :: SYSTEM, UNLINK, GETPID, RENAME, I_LEN, ILEN, &
     &                       GET_UNIT, FOR_STAT
!
! --- Check EOPS file
!
      INQUIRE ( FILE=OPA%EOPS_FILE, EXIST=LEX )
      IF ( .NOT. LEX ) THEN
           CALL ERR_LOG ( 4301, IUER, 'OPA_EOPS', 'The old EOPS file '// &
     &          OPA%EOPS_FILE(1:I_LEN(OPA%EOPS_FILE))//' was not found. '// &
     &          'It should exist before OPA_EOPS will try to update it ' )
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
      CALL CLRCH (   EOPK_BACKUP )
      CALL CLRCH ( NEW_EOPB_FILE )
      CALL CLRCH ( NEW_EOPK_FILE )
      CALL CLRCH ( EOPK_STS_FILE )
      CALL CLRCH ( EOPK_PRG_FILE )
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
      EOPB_FILE = OPA%TMP_DIR(1:I_LEN(OPA%TMP_DIR))//'opa_eops_'//PID_STR// &
     &            '.eob'
      CONTROL_FILE = OPA%TMP_DIR(1:I_LEN(OPA%TMP_DIR))//'opa_eops_'//PID_STR// &
     &              '.cnt'
      EOPB_BACKUP = OPA%EOPB_FILE(1:I_LEN(OPA%EOPB_FILE))// &
     &              '.opa_eopb_bac_'//PID_STR
      EOPK_BACKUP = OPA%EOPK_FILE(1:I_LEN(OPA%EOPK_FILE))// &
     &              '.opa_eopk_bac_'//PID_STR
      EOPS_BACKUP = OPA%EOPS_FILE(1:I_LEN(OPA%EOPS_FILE))// &
     &              '.opa_eops_bac_'//PID_STR
      NEW_EOPB_FILE = OPA%EOPB_FILE(1:I_LEN(OPA%EOPB_FILE))// &
     &                  '.opa_new_eopb_'//PID_STR
      NEW_EOPK_FILE = OPA%EOPK_FILE(1:I_LEN(OPA%EOPK_FILE))// &
     &                  '.opa_new_eopk_'//PID_STR
      NEW_EOPS_FILE = OPA%EOPS_FILE(1:I_LEN(OPA%EOPS_FILE))// &
     &                  '.opa_new_eops_'//PID_STR
      SPOOL_FILE = SPOOL_DIR_FILE(1:ISPL)//'SPLF'//SOLVE_INIT
      EOPK_STS_FILE = OPA%TMP_DIR(1:I_LEN(OPA%TMP_DIR))//'eopk_'//PID_STR// &
     &                '.sts'
      EOPK_PRG_FILE = OPA%TMP_DIR(1:I_LEN(OPA%TMP_DIR))//'eopk_'//PID_STR// &
     &                '.prg'
!
! --- We have two runs: one run with the a priori Earth orientation
! --- substitution file NONE (no substituion), the second run with the a priori
! --- Earth orientation substition file produced by the first run.
!
      DO 410 J1=1,2
!
! ------ Read template control file to the buffer (but reserve first three
! ------ lines for comments)
!
         CALL ERR_PASS ( IUER, IER )
         CALL RD_TEXT  ( OPA%EOPS_CNT, MBUF, BUF(4), NBUF, IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG  ( 4303, IUER, 'OPA_EOPS', 'Error in reading '// &
     &            'template control file '//OPA%EOPS_CNT )
              RETURN
         END IF
         NBUF = NBUF + 3
!
! ------ Write down comments in to the buffer with control file
!
         BUF(1) = '*  This file was generated automatically by program '// &
     &            'OPA on '//GET_CDATE()
         BUF(2) = '*  using a template file '// &
     &             OPA%EOPS_CNT(1:I_LEN(OPA%EOPS_CNT))
         BUF(3) = '*  '
!
! ------ Scan template files for patterns @cgm_file@, @erp_file@ and @arc_line@
!
         FL_S = .FALSE.
         FL_E = .FALSE.
         FL_A = .FALSE.
!
         DO 420 J2=1,NBUF
            IPS = INDEX ( BUF(J2), '@cgm_file@' )
            IF ( IPS .GT. 1 ) THEN
!
! -------------- If found, replace with actual CGM
!
                 BUF(J2) = BUF(J2)(1:IPS-1)// &
     &                     OPA%EOPS_CGM(1:I_LEN(OPA%EOPS_CGM))// &
     &                     BUF(J2)(IPS+LEN('@cgm_file@'):)
                 FL_S = .TRUE.
            END IF
!
            IPE = INDEX ( BUF(J2), '@erp_file@' )
            IF ( IPE .GT. 1 ) THEN
!
! -------------- If found, replace with erp file. erp mod-file is diferent
! -------------- or the first run (the same as for generic solution) and
! -------------- for the second run
!
                 IF ( J1 .EQ. 1 ) THEN
                      BUF(J2) = BUF(J2)(1:IPE-1)// &
     &                          OPA%GEN_INPERP(1:I_LEN(OPA%GEN_INPERP))// &
     &                          '  SPL  UT1S '// &
     &                          BUF(J2)(IPE+LEN('@erp_file@'):)
                    ELSE IF ( J1 .EQ. 2 ) THEN
                      BUF(J2) = BUF(J2)(1:IPE-1)// &
     &                          OPA%EOPK_FILE(1:I_LEN(OPA%EOPK_FILE))// &
     &                          '  SPL  UT1S '// &
     &                          BUF(J2)(IPE+LEN('@erp_file@'):)
                 END IF
                 FL_E = .TRUE.
            END IF
!
            IPA = INDEX ( BUF(J2), '@arc_line@' )
            IF ( IPA .GT. 0 ) THEN
                 IF ( J1 .EQ. 1 ) THEN
!
! ------------------- We add keyword NO_EOP_MOD in the first run and don't do
! ------------------- it in the second run
!
                      ARC_LN = '  $'//OPA%DB_NAME//' '//DBV_STR// &
     &                         ' NO_EOP_MOD '//OPA%ARC_LINE
                    ELSE IF ( J1 .EQ. 2 ) THEN
                      ARC_LN = '  $'//OPA%DB_NAME//' '//DBV_STR//' '// &
     &                          OPA%ARC_LINE
                 END IF
!
! -------------- If found, replace with actual arc-line
!
                 BUF(J2) = ARC_LN
                 FL_A = .TRUE.
            END IF
 420     CONTINUE
!
         IF ( FL_S ) THEN
!
! ----------- Check whether CGM exists
!
              INQUIRE ( FILE=OPA%EOPS_CGM, EXIST=LEX )
              IF ( .NOT. LEX ) THEN
                   CALL ERR_LOG ( 4304, IUER, 'OPA_EOPS', 'CGM file '// &
     &                  OPA%EOPS_CGM(1:I_LEN(OPA%EOPS_CGM))//' was not found.'// &
     &                 ' Check OPA configuration file '//OPA%CONFIG_FILE )
                   RETURN
              END IF
         END IF
!
! ------ Check other signatures
!
         IF ( .NOT. FL_E ) THEN
              CALL ERR_LOG ( 4305, IUER, 'OPA_EOPS', 'Signature @erp_file@ '// &
     &            'was not found in a template control file '// &
     &             OPA%EOPS_CNT )
              RETURN
         END IF
!
         IF ( .NOT. FL_A ) THEN
              CALL ERR_LOG ( 4306, IUER, 'OPA_EOPS', 'Signature @arc_line@ '// &
     &            'was not found in a template control file '// &
     &             OPA%EOPS_CNT )
              RETURN
         END IF
!
! ------ Remove the old control file if it exists
!
         IS = UNLINK ( CONTROL_FILE(1:I_LEN(CONTROL_FILE))//CHAR(0) )
!
! ------ Write down updated control file
!
         CALL ERR_PASS ( IUER, IER )
         CALL WR_TEXT  ( NBUF, BUF, CONTROL_FILE, IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 4307, IUER, 'OPA_EOPS', 'Error in attempt '// &
     &            'to write temporary control file '//CONTROL_FILE )
              RETURN
         END IF
!
! ------ Build the line for launching Solve
!
         CALL CLRCH ( COMSTR  )
         COMSTR = PRE_SOL_DIR(1:PRE_SOL_LEN)//'solve '//SOLVE_INIT//' '// &
     &            CONTROL_FILE(1:I_LEN(CONTROL_FILE))//' silent'
         IF ( IVRB .GE. 1 ) WRITE ( 6, '(I1,A)' ) J1,') Running Solve ...'
!
! ------ Check solve lock. 
!
         CALL CHECK_SOLVE_LOCK()
!
! ------ Launch solve and wait
!
         IS = SYSTEM ( COMSTR(1:I_LEN(COMSTR))//CHAR(0) )
!
! ------ Remove lock from the current Solve user initials
!
         CALL REMOVE_SOLVE_LOCK()
!
         IF ( IS .NE. 0 ) THEN
!
! ----------- Completion code is not 0. Extract ISIG -- signal number which
! ----------- caused termination of the command and ICOD -- completion code
!
              ISIG = 0
              ICOD = 0
              CALL MVBITS ( IS, 0, 8, ISIG, 0 )
              CALL MVBITS ( IS, 8, 8, ICOD, 0 )
              IF ( ICOD .GE. 128 ) ICOD = ICOD-256
!
              CALL CLRCH ( STR )
              CALL INCH  ( ICOD, STR )
              CALL ERR_LOG ( 4308, IUER, 'OPA_EOPS', &
     &                      'Error '//STR(1:I_LEN(STR))// &
     &                      ' in executing command line '//COMSTR )
              RETURN
         END IF
!
! ------ Check, whether Solve completed successfully
!
         IF ( .NOT. CHECK_SOLVE_COMPLETE ( SOLVE_INIT ) ) THEN
              CALL ERR_LOG ( 4311, IUER, 'OPA_EOPS', 'Solve run was not '// &
     &                       'successfull' )
              RETURN
         END IF
!
! ------ Remove control files which are not needed any more
!
         IS = UNLINK ( CONTROL_FILE(1:I_LEN(CONTROL_FILE))//CHAR(0) )
         IS = UNLINK ( CONTROL_FILE(1:I_LEN(CONTROL_FILE))//'.XPND'//CHAR(0) )
         IF ( IVRB .GE. 1 ) WRITE ( 6, '(I1,A)' ) J1,') Parse spool file '// &
     &                                 '...                                '
!
! ------ Parse spool file and extract EOP values for this specific session
! ------ in B-format. They will be written in EOPB_FILE
!
         CALL ERR_PASS ( IUER, IER )
         CALL SPOOL_TO_EOPB ( SPOOL_FILE, EOPB_FILE, OPA%MASTER_DIR, IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 4312, IUER, 'OPA_EOPS', 'Error in attempt '// &
     &            'to parse spool file '//SPOOL_FILE(1:I_LEN(SPOOL_FILE))// &
     &            ' and create new EOPB file for this Solve run' )
              RETURN
         END IF
         IF ( IVRB .GE. 1 ) WRITE ( 6, '(I1,A)' ) J1,') Insert new EOP into '// &
     &                                         'the global EOPB file ...'
!
! ------ Check size of the EOPB file
!
         IS = FOR_STAT ( EOPB_FILE, STAT_BLOCK )
         IF ( STAT_BLOCK(8) .LE. 0 ) THEN
              CALL ERR_LOG ( 4313, IUER, 'OPA_EOPS', 'Trap of internal '// &
     &            'control: EOP were not estimated and file '// &
     &             EOPB_FILE(1:I_LEN(EOPB_FILE))//' is empty. Check template '// &
     &            'control file '//OPA%EOPS_CNT )
              RETURN
         END IF
!
! ------ Open the old input EOPB file
!
         LUNI = GET_UNIT ()
         OPEN ( UNIT=LUNI, FILE=OPA%EOPB_FILE, STATUS='OLD', IOSTAT=IO )
         IF ( IO .NE. 0 ) THEN
              CALL CLRCH ( STR )
              CALL INCH  ( IO, STR )
              CALL ERR_LOG ( 4314, IUER, 'OPA_EOPS', 'Error '// &
     &             STR(1:I_LEN(STR))//' in attempt to open EOPB file '// &
     &             OPA%EOPB_FILE )
              RETURN
         END IF
!
! ------ Open the new output eopb file
!
         LUNO = GET_UNIT ()
         OPEN ( UNIT=LUNO, FILE=NEW_EOPB_FILE, STATUS='UNKNOWN', IOSTAT=IO )
         IF ( IO .NE. 0 ) THEN
              CALL CLRCH ( STR )
              CALL INCH  ( IO, STR )
              CALL ERR_LOG ( 4315, IUER, 'OPA_EOPS', 'Error '// &
     &             STR(1:I_LEN(STR))//' in attempt to open output EOPB file '// &
     &             NEW_EOPB_FILE )
              CLOSE ( LUNI )
              RETURN
         END IF
!
! ------ Parse the database name and to transform it to the form
! ------ YYYYMMMDDD.SS
!
         DB_NM = '          .  '
         CALL ERR_PASS ( IUER, IER )
         CALL PARSE_DBNAME ( OPA%DB_NAME, DB_NM(1:10), DB_NM(12:13), IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 4316, IUER, 'OPA_EOPS', 'Trap of internal '// &
     &            'control: wrong format of the database name which is '// &
     &            'being processed' )
              RETURN
         END IF
!
         FL_INSERT = .FALSE.
         LEN_EOB = SIZEOF ( EOB )
!
! ------ Read global eopb file
!
         DO 430 J3=1,1024*1024*1024
            DB_NM_READ = '          .  '
            READ ( LUNI, FMT='(A)', IOSTAT=IO ) EOB_STR ! read a line
            CALL LIB$MOVC3 ( LEN_EOB, %REF(EOB_STR), EOB )
            IF ( IO .EQ. -1 ) THEN
                 GOTO 830
               ELSE IF ( IO .NE. 0 ) THEN
                 CALL CLRCH ( STR )
                 CALL INCH  ( IO, STR )
                 WRITE ( 6, * ) ' line ',J3
                 CALL ERR_LOG ( 4317, IUER, 'OPA_EOPS', 'Error '// &
     &                STR(1:I_LEN(STR))//' in attempt to read EOPB file '// &
     &                OPA%EOPB_FILE )
                 RETURN
            END IF
!
            IF ( EOB_STR(1:1)    .NE. '#'  .AND. &
     &           EOB%DBNAME(1:1) .EQ. '$'         ) THEN
!
! -------------- Parse the database file name from the EOPS file and
! -------------- build a string DB_NM_READ for comparing
!
                 CALL ERR_PASS ( IUER, IER )
                 CALL PARSE_DBNAME ( EOB%DBNAME(2:10) , DB_NM_READ(1:10), &
     &                               DB_NM_READ(12:13), IER )
                 IF ( IER .NE. 0 ) THEN
                      CALL ERR_LOG ( 4318, IUER, 'OPA_EOPS', 'Trap of '// &
     &                    'internal control: wrong format of the database '// &
     &                    'name '//EOB%DBNAME(1:10)  )
                      RETURN
                 END IF
!
! -------------- Now comapre transformed database filename: the targeted
! -------------- database name and the database file name taken from EOPS file
!
                 IF ( DB_NM_READ .GE. DB_NM  .AND. .NOT. FL_INSERT ) THEN
!
! ------------------- It is just the place to insert new eopb values
! ------------------- Read the new eopb values
!
                      CALL RD_TEXT ( EOPB_FILE, MBUF, BUF, NBUF, IER )
                      IF ( IER .NE. 0 ) THEN
                           CALL ERR_LOG ( 4319, IUER, 'OPA_EOPS', 'Error in '// &
     &                         'attempt to read the input EOPB file which '// &
     &                         'just has been created: '//NEW_EOPB_FILE )
                           CLOSE ( LUNO )
                           CLOSE ( LUNI )
                           RETURN
                      END IF
!
! ------------------- Write it down in the output file
!
                      DO 440 J4=1,NBUF
                         IF ( BUF(J4)(1:1) .NE. '#' ) THEN
                              WRITE ( LUNO, '(A)' ) BUF(J4)(1:I_LEN(BUF(J4)))
                         END IF
 440                  CONTINUE
                      FL_INSERT = .TRUE.
                 END IF
            END IF ! str is not  comment
            IF ( EOB_STR(1:13) .EQ. '# Spool file:' ) THEN
                 EOB_STR= '# Initial spool file: '//EOB_STR(15:)
            END IF
!
            IF ( DB_NM_READ .NE. DB_NM ) THEN
!
! -------------- Copy the input line to the output file unless it is the line
! -------------- for this experiment
!
                CALL LIB$MOVC3 ( LEN_EOB, EOB, %REF(EOB_STR)  )
                WRITE ( LUNO, '(A)' ) EOB_STR(1:I_LEN(EOB_STR))
            END IF
            IF ( J1 .EQ. 2  .AND.  EOB_STR(1:10) .EQ. '# Analysis' ) THEN
!
! -------------- Get user's real name
!
                 CALL GETINFO_USER ( USER_NAME, USER_REALNAME, USER_E_ADDRESS )
                 IF ( ILEN(USER_REALNAME) .EQ. 0 ) THEN
                      USER_REALNAME = USER_E_ADDRESS
                 END IF
!
! -------------- ... and write down the current date of update and user's name
!
                 WRITE ( LUNO, '(A)', IOSTAT=IO ) '# Updated by OPA at      '// &
     &                   GET_CDATE()//'  by '// &
     &                   USER_REALNAME(1:I_LEN(USER_REALNAME))
            END IF
 430     CONTINUE
 830     CONTINUE
!
         IF ( .NOT. FL_INSERT ) THEN
!
! ----------- Date were not inserted. It means that we have to insert them
! ----------- after the last line of the input EOPB file.
! ----------- Read the new input EOPB file
!
              CALL RD_TEXT ( EOPB_FILE, MBUF, BUF, NBUF, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 4320, IUER, 'OPA_EOPS', 'Error in attempt '// &
     &                 'to read the input EOPS file which just has been '// &
     &                 'created: '//EOPB_FILE )
                   CLOSE ( LUNI )
                   CLOSE ( LUNO )
                   RETURN
              END IF
!
! ----------- ... and its contents to the end of the global EOPB file
!
              DO 450 J5=1,NBUF
                 IF ( BUF(J5)(1:1) .NE. '#' ) THEN
                      WRITE ( LUNO, '(A)' ) BUF(J5)(1:I_LEN(BUF(J5)))
                 END IF
 450          CONTINUE
         END IF
!
         CLOSE ( UNIT = LUNI )
         CLOSE ( UNIT = LUNO )
!
         IF ( IVRB .GE. 1 ) WRITE ( 6, '(I1,A)' ) J1,') Copying EOPB files ...'
!
! ------ Copy  OPA.EOPB_FILE --> EOPB_BACKUP
!
         CALL ERR_PASS ( IUER, IER )
         CALL COPY_ASCII_FILE ( OPA%EOPB_FILE, EOPB_BACKUP, IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 4321, IUER, 'OPA_EOPS', 'Error in attempt '// &
     &            'to make a backup copy of the old EOPB file' )
              RETURN
         END IF
!
! ------ Rename NEW_EOPB_FILE --> OPA.EOPB_FILE
! ------ This trick with first copying file and then renaming is for minimizing
! ------ time of file update. At the same time we are are sure
! ------ that NEW_EOPB_FILE and OPA.EOPB_FILE are at the same disk system and
! ------ therefore operation of renaming is legitimate
!
         IS = RENAME ( NEW_EOPB_FILE, OPA%EOPB_FILE )
         IF ( IS .NE. 0 ) THEN
              CALL ERR_LOG ( 4322, IUER, 'OPA_EOPS', 'Serious error '// &
     &            'in attempt to move the new EOPB file to the old place. '// &
     &            'The old file '// &
     &             OPA%EOPB_FILE(1:I_LEN(OPA%EOPB_FILE))// &
     &            ' is probably spoiled!!! Check the backup copy '// &
     &             EOPB_BACKUP )
              RETURN
         END IF
!
! ------ Remove temporary EOPB files
!
         IS = UNLINK ( EOPB_BACKUP(1:I_LEN(EOPB_BACKUP))//CHAR(0) )
         IS = UNLINK ( EOPB_FILE(1:I_LEN(EOPB_FILE))//CHAR(0) )
         IF ( IVRB .GE. 1 ) WRITE ( 6, '(I1,A)' ) J1,') Copying EOPS files ...'
!
! ------ Copy  OPA.EOPB_FILE --> EOPB_BACKUP
!
         CALL ERR_PASS ( IUER, IER )
         CALL COPY_ASCII_FILE ( OPA%EOPS_FILE, EOPS_BACKUP, IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 4323, IUER, 'OPA_EOPS', 'Error in attempt '// &
     &            'to make a backup copy of the old EOPS file' )
              RETURN
         END IF
! ------ Read the old input EOPB file
!
         CALL ERR_PASS ( IUER, IER )
         CALL READ_EOB ( OPA%EOPB_FILE, M_HEA, N_HEA, HEA_BUF, M_SES, N_SES, &
     &                   EOP, IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 4324, IUER, 'OPA_EOPS', 'Error in an attempt '// &
     &            'to read the eop file '//OPA%EOPB_FILE )
              RETURN
         END IF
!
! ------ Get directory for master files
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
! ------ Read help-file with additional information about eops file. It
! ------ will be treated as comments
!
         CALL ERR_PASS ( IUER, IER )
         CALL RD_TEXT  ( EOPS_HELP, M_HEA-N_HEA, HEA_BUF(N_HEA+1), N_HLP, IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 4325, IUER, 'OPA_EOPS', 'Error in an attempt '// &
     &            'to read the file '//EOPS_HELP(1:I_LEN(EOPS_HELP))// &
     &            'with description of eops format' )
              RETURN
         END IF
!
         N_HEA = N_HEA + N_HLP
         IF ( N_HEA .GT. M_HEA ) THEN
              CALL CLRCH ( STR )
              CALL INCH  ( M_HEA, STR )
              CALL ERR_LOG ( 4326, IUER, 'OPA_EOPS', 'Too long header of EOB '// &
     &            'file '//OPA%EOPB_FILE(1:I_LEN(OPA%EOPB_FILE))//' -- '// &
     &            'parameter M_HEA: '//STR(1:I_LEN(STR))//' is not enough' )
              RETURN
         END IF
!
! ------ Add comment prefix if needed
!
         DO 460 J6=1,N_HEA
            IF ( HEA_BUF(J6)(1:1) .NE. '#' ) HEA_BUF(J6) = '# '//HEA_BUF(J6)
 460     CONTINUE
!
! ------ Write down the file in EOPS format
!
         CALL ERR_PASS ( IUER, IER )
         CALL WRITE_EOPS ( NEW_EOPS_FILE, N_HEA, HEA_BUF, N_SES, EOP, IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 4327, IUER, 'OPA_EOPS', 'Error in an attempt '// &
     &            'to write in the eops file '//NEW_EOPS_FILE )
              RETURN
         END IF
!
! ------ Rename NEW_EOPS_FILE --> OPA.EOPS_FILE
!
         IS = RENAME ( NEW_EOPS_FILE, OPA%EOPS_FILE )
         IF ( IS .NE. 0 ) THEN
              CALL ERR_LOG ( 4328, IUER, 'OPA_EOPS', 'Serious error '// &
     &            'in attempt to move the new EOPS file to the old place. '// &
     &            'The old file '// &
     &             OPA%EOPB_FILE(1:I_LEN(OPA%EOPS_FILE))// &
     &            ' is probably spoiled!!! Check the backup copy '// &
     &             EOPS_BACKUP )
              RETURN
         END IF
!
! ------ Remove temporary EOPS files
!
         IS = UNLINK ( EOPS_BACKUP(1:I_LEN(EOPS_BACKUP))//CHAR(0) )
         IS = UNLINK ( NEW_EOPS_FILE(1:I_LEN(NEW_EOPS_FILE))//CHAR(0) )
!
! ------ Build the line for launching eopkal
!
         CALL CLRCH ( COMSTR  )
         COMSTR = PRE_SOL_DIR(1:PRE_SOL_LEN)//'eopkal '// &
     &           ' -i '//OPA%EOPB_FILE(1:I_LEN(OPA%EOPB_FILE))// &
     &           ' -o '//NEW_EOPK_FILE(1:I_LEN(NEW_EOPK_FILE))// &
     &           ' -s '//EOPK_STS_FILE(1:I_LEN(EOPK_STS_FILE))// &
     &           ' >  '//EOPK_PRG_FILE(1:I_LEN(EOPK_PRG_FILE))
         IF ( IVRB .GE. 1 ) WRITE ( 6, '(I1,A)' ) J1,') Running eopkal ...'
!
! ------ Launch eopkal and wait
!
         IS = SYSTEM ( COMSTR(1:I_LEN(COMSTR))//CHAR(0) )
!
! ------ Remove lock from the current Solve user initials
!
         CALL REMOVE_SOLVE_LOCK()
!
         IF ( IS .NE. 0 ) THEN
!
! ----------- Completion code is not 0. Extract ISIG -- signal number which
! ----------- caused termination of the command and ICOD -- completion code
!
              ISIG = 0
              ICOD = 0
              CALL MVBITS ( IS, 0, 8, ISIG, 0 )
              CALL MVBITS ( IS, 8, 8, ICOD, 0 )
              IF ( ICOD .GE. 128 ) ICOD = ICOD-256
!
              CALL CLRCH ( STR )
              CALL INCH  ( ICOD, STR )
              CALL ERR_LOG ( 4329, IUER, 'OPA_EOPS', &
     &                      'Error '//STR(1:I_LEN(STR))// &
     &                      ' in executing command line '//COMSTR )
              RETURN
         END IF
!
! ------ Read eopkal status file
!
         CALL RD_TEXT ( EOPK_STS_FILE, MBUF, BUF, NBUF, IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 4330, IUER, 'OPA_EOPS', 'Error in attempt '// &
     &            'to read the eopkal status file: '//EOPK_STS_FILE )
              RETURN
         END IF
!
         IF ( BUF(1)(1:16) .NE. 'EOPKAL: finished' ) THEN
              CALL ERR_LOG ( 4331, IUER, 'OPA_EOPS', 'Program eopkal '// &
     &            'launched by comand '//COMSTR(1:I_LEN(COMSTR))// &
     &            'terminated abnormally. Please investigate file '// &
     &             EOPK_PRG_FILE )
              RETURN
         END IF
         IF ( IVRB .GE. 1 ) WRITE ( 6, '(I1,A)' ) J1,') Copying EOPK-file ...'
!
! ------ Copy  OPA.EOPK_FILE --> EOPK_BACKUP
!
         CALL ERR_PASS ( IUER, IER )
         CALL COPY_ASCII_FILE ( OPA%EOPK_FILE, EOPK_BACKUP, IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 4332, IUER, 'OPA_EOPS', 'Error in attempt '// &
     &            'to make a backup copy of the old EOPK file' )
              RETURN
         END IF
!
! ------ Rename NEW_EOPK_FILE --> OPA.EOPK_FILE
!
         IS = RENAME ( NEW_EOPK_FILE, OPA%EOPK_FILE )
         IF ( IS .NE. 0 ) THEN
              CALL ERR_LOG ( 4333, IUER, 'OPA_EOPS', 'Serious error '// &
     &            'in attempt to move the new EOPK file to the old place. '// &
     &            'The old file '// &
     &             OPA%EOPK_FILE(1:I_LEN(OPA%EOPK_FILE))// &
     &            ' is probably spoiled!!! Check the backup copy '// &
     &             EOPK_BACKUP )
              RETURN
         END IF
!
! ------ Remove temporary EOPK files
!
         IS = UNLINK ( EOPK_BACKUP(1:I_LEN(EOPK_BACKUP))//CHAR(0) )
         IS = UNLINK ( NEW_EOPK_FILE(1:I_LEN(NEW_EOPK_FILE))//CHAR(0) )
         IS = UNLINK ( EOPK_STS_FILE(1:I_LEN(EOPK_STS_FILE))//CHAR(0) )
         IS = UNLINK ( EOPK_PRG_FILE(1:I_LEN(EOPK_PRG_FILE))//CHAR(0) )
 410  CONTINUE
!
! --- Remove lock from the current Solve user initials
!
      CALL REMOVE_SOLVE_LOCK()
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  OPA_EOPS  #!#
