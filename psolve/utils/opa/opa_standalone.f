      SUBROUTINE OPA_STANDALONE ( SOLVE_INIT, OPA, OPC_FILE, IVRB, IUER )
! ************************************************************************
! *                                                                      *
! *   Subroutine  OPA_STANDALONE  executes the action of OPA: it         *
! *   computes the new stanalone solution and generates listing in SINEX *
! *   format.                                                            *
! *                                                                      *
! *   OPA_STANDALONE reads template batch control file, updates it in    *
! *   according with parameters of configuration file saved in the data  *
! *   structure OPA, write down temporary control file and then runs     *
! *   batch Solve with this temporary control file.                      *
! *                                                                      *
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
! * ### 20-SEP-2000   OPA_STANDALONE  v2.0 (c) L. Petrov 06-JUN-2002 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE    'solve.i'
      INCLUDE    'precm.i'
      INCLUDE    'opa.i'
      TYPE ( OPA__STRU ) ::  OPA
      CHARACTER  SOLVE_INIT*2, OPC_FILE*(*)
      INTEGER*4  IVRB, IUER
      INTEGER*4  MBUF
      PARAMETER  ( MBUF = 1024 )
!
      CHARACTER  COMSTR*256, LOCK_FINAM*128, BUF(MBUF)*256, PID_STR*5, &
     &           CONTROL_FILE*128, ARC_LN*128, DBV_STR*3, STR*256, &
     &           SPOOL_DIR_FILE*128, SPOOL_FILE*128, OUT_SPOOL*128
      CHARACTER  GET_CDATE*19
      LOGICAL*4  LSUI, CHECK_SOLVE_INITIALS, CHECK_SOLVE_COMPLETE
      INTEGER*4  NBUF, IS, PID, IPA, IPE, IPS, ISPL, LUNI, LUNO, IOS, &
     &           J1, J2, IER
      LOGICAL*4  FL_A, FL_E, FL_S
      INTEGER*4, EXTERNAL :: GETPID, GET_UNIT, ILEN, I_LEN, SYSTEM, UNLINK
!
! --- Read template control file to the buffer (but reserve fitst three lines
! --- for comments)
!
      CALL ERR_PASS ( IUER, IER )
      CALL RD_TEXT  ( OPA%STANDALONE_CNT, MBUF, BUF(4), NBUF, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG  ( 4352, IUER, 'OPA_ACTION', 'Error in reading '// &
     &         'template control file '//OPA%STANDALONE_CNT )
           RETURN
      END IF
      NBUF = NBUF + 3
!
! --- Write comments in to the buffer with control file
!
      BUF(1) = '*  This file was generated automatically by program OPA on '// &
     &             GET_CDATE()
      BUF(2) = '*  using a template file '// &
     &             OPA%STANDALONE_CNT(1:I_LEN(OPA%STANDALONE_CNT))
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
      CALL CLRCH   (  CONTROL_FILE )
      CALL CLRCH   (        ARC_LN )
      CALL CLRCH   (     OUT_SPOOL )
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
      CONTROL_FILE = OPA%TMP_DIR(1:I_LEN(OPA%TMP_DIR))// &
     &               'opa_standalone_'//PID_STR//'.cnt'
      ARC_LN = '  $'//OPA%DB_NAME//' '//DBV_STR//' '//OPA%ARC_LINE
      SPOOL_FILE = SPOOL_DIR_FILE(1:ISPL)//'SPLF'//SOLVE_INIT
      ISPL = ILEN(OPC_FILE) - 4
      OUT_SPOOL  = OPC_FILE(1:ISPL)//'.spl'
!
! --- Scan template files for pattern @arc_line@
!
      FL_A = .FALSE.
      FL_E = .FALSE.
      FL_S = .FALSE.
!
      DO 410 J1=1,NBUF
         IPA = INDEX ( BUF(J1), '@arc_line@' )
         IF ( IPA .GT. 0 ) THEN
!
! ----------- If found, replace with actual arc-line
!
              BUF(J1) = ARC_LN
              FL_A = .TRUE.
         END IF
!
         IPE = INDEX ( BUF(J1), '@erp_file@' )
         IF ( IPE .GT. 0 ) THEN
              BUF(J1) = BUF(J1)(1:IPE-1)// &
     &                  OPA%GEN_INPERP(1:I_LEN(OPA%GEN_INPERP))// &
     &                  '  SPL  UT1S '// &
     &                  BUF(J1)(IPE+LEN('@erp_file@'):)
              FL_E = .TRUE.
         END IF
!
         IPS = INDEX ( BUF(J1), '@session_dir@' )
         IF ( IPS .GT. 0 ) THEN
!
! ----------- If found, replace with actual directory where output listing is
! ----------- to be put
!
              BUF(J1) = BUF(J1)(1:IPS-1)// &
     &                  OPA%SESSION_DIR(1:I_LEN(OPA%SESSION_DIR))// &
     &                  BUF(J1)(IPS+LEN('@session_dir@'):)
              FL_S = .TRUE.
         END IF
 410  CONTINUE
!
      IF ( .NOT. FL_A ) THEN
           CALL ERR_LOG ( 4353, IUER, 'OPA_STANDALONE', 'Signature @arc_line@ '// &
     &         'was not found in a template control file '//OPA%STANDALONE_CNT )
           RETURN
      END IF
!
      IF ( .NOT. FL_S ) THEN
           CALL ERR_LOG ( 4354, IUER, 'OPA_STANDALONE', 'Signature @session_dir@ '// &
     &         'was not found in a template control file '//OPA%STANDALONE_CNT )
           RETURN
      END IF
!
      IF ( .NOT. FL_S ) THEN
           CALL ERR_LOG ( 4354, IUER, 'OPA_STANDALONE', 'Signature @erp_fil@ '// &
     &         'was not found in a template control file '//OPA%STANDALONE_CNT )
           RETURN
      END IF
!
! --- Write down updated control file
!
      CALL ERR_PASS ( IUER, IER )
      CALL WR_TEXT  ( NBUF, BUF, CONTROL_FILE, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 4355, IUER, 'OPA_STANDALONE', 'Error in attempt to '// &
     &         'write temporary control file '//CONTROL_FILE )
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
      IF ( IS .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( IS, STR )
           CALL ERR_LOG ( 4356, IUER, 'OPA_STANDALONE', 'Error '//STR(1:I_LEN(STR))// &
     &         ' in executing command line '//COMSTR )
           RETURN
      END IF
!
! --- Remove lock from the current Solve user initials
!
      CALL REMOVE_SOLVE_LOCK()
!
! --- Check whetehr Solve run was complete
!
      IF ( .NOT. CHECK_SOLVE_COMPLETE ( SOLVE_INIT ) ) THEN
           CALL ERR_LOG ( 4359, IUER, 'OPA_STANDALONE', 'Solve run was not '// &
     &                    'successfull' )
           RETURN
      END IF
!
! --- Remove control files which are not needed any more
!
      IS = UNLINK ( CONTROL_FILE(1:I_LEN(CONTROL_FILE))//CHAR(0) )
      IS = UNLINK ( CONTROL_FILE(1:I_LEN(CONTROL_FILE))//'.XPND'//CHAR(0) )
!
      LUNI = GET_UNIT()
      OPEN ( UNIT=LUNI, FILE=SPOOL_FILE, STATUS='OLD', IOSTAT=IOS )
      IF ( IOS .NE. 0 ) THEN
           CALL ERR_LOG ( 4360, IUER, 'OPA_STANDALONE', 'Error '// &
     &          STR(1:I_LEN(STR))//' in an attempt to open spool file '// &
     &          SPOOL_FILE )
           RETURN
      END IF
!
      LUNO = GET_UNIT()
      OPEN ( UNIT=LUNO, FILE=OUT_SPOOL, STATUS='UNKNOWN', IOSTAT=IOS )
      IF ( IOS .NE. 0 ) THEN
           CALL ERR_LOG ( 4361, IUER, 'OPA_STANDALONE', 'Error '// &
     &          STR(1:I_LEN(STR))//' in an attempt to open output file '// &
     &          SPOOL_FILE )
           RETURN
      END IF
!
      DO 420 J2=1,1028*1024*1024
         READ ( UNIT=LUNI, FMT='(A)', IOSTAT=IOS ) STR
         IF ( IOS .EQ. -1 ) THEN
              GOTO 820
            ELSE IF ( IOS .NE. 0 ) THEN
              CALL CLRCH ( STR )
              CALL ERR_LOG ( 4362, IUER, 'OPA_STANDALONE', 'Error '// &
     &             STR(1:I_LEN(STR))//' in reading spool file '// &
     &             SPOOL_FILE )
              RETURN
         END IF
!
! ------ Look: is this the line with the end of interesting portion of the
! ------ listing?
!
         IF ( STR(1:17) .EQ. 'PROGRAM VERSIONS:' ) GOTO 820
!
         WRITE ( UNIT=LUNO, FMT='(A)', IOSTAT=IOS ) STR(1:I_LEN(STR))
         IF ( IOS .NE. 0 ) THEN
              CALL CLRCH ( STR )
              CALL ERR_LOG ( 4363, IUER, 'OPS_STANDALONE', 'Error '// &
     &             STR(1:I_LEN(STR))//' in an attempt to write in the '// &
     &            'outpu spool file '//OUT_SPOOL )
              RETURN
         END IF
 420  CONTINUE
 820  CONTINUE
      CLOSE ( UNIT = LUNI )
      CLOSE ( UNIT = LUNO )
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  OPA_STANDALONE  #!#
