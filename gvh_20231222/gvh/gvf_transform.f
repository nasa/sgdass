      PROGRAM    GVF_TRANSFORM_MAIN
! ************************************************************************
! *                                                                      *
! *   Program GVF_TRANSFORM_MAIN
! *                                                                      *
! * ## 31-JUL-2007  GVF_TRANSFORM_MAIN v3.7 (c) L. Petrov 18-MAR-2024 ## *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
#ifdef GVH_STANDALONE
      INCLUDE   'gvh_solve.i'
#else
      INCLUDE   'solve.i'
#endif
      INCLUDE   'vcat.i'
      INTEGER*8    STACK_SIZE_IN_GIGABYTES, STACK_SIZE_IN_BYTES, GB
      PARAMETER  ( STACK_SIZE_IN_GIGABYTES = 1 )
      PARAMETER  ( GB = 1024*1024*1024 )
      PARAMETER  ( STACK_SIZE_IN_BYTES = STACK_SIZE_IN_GIGABYTES * GB )
!
      INTEGER*4    M_FIL, MBUF
      PARAMETER  ( M_FIL =  32 ) 
      PARAMETER  ( MBUF  = 256 ) 
      CHARACTER  REPO__DEF*3
      PARAMETER  ( REPO__DEF = 'OBS' ) 
      CHARACTER  FIL_ENV_IN*128, DB_FILE(M_FIL)*128, FIL_ENV_OUT*128, FIL_ENV_ORIG*128
      TYPE     ( VCAT__TYPE ) :: VCAT
!
      CHARACTER  VCAT_CONF_FILE*128, GVF_DB_DIR*128, REPO*3, ENV_FILE*128, &
     &           GVF_ENV_DIR*128, BUF(MBUF)*128, COMS(2)*128, FIL_COM*128, &
     &           FIL_TMP*128, EXP_NAME*128, MODE_STR*128, STR*128, FILNAM*128, &
     &           FIL_TRY*128, PID_STR*8
      INTEGER*4  J1, J2, IS, IB, IE, IL, ID, NC, L_FIL, IND_REP, IUER
      LOGICAL*1  FL_DEBUG
      INTEGER*4, EXTERNAL :: GETPID, ILEN, I_LEN, LINDEX, LTM_DIF, SYSTEM
      INTEGER*8, EXTERNAL :: SET_STACKSIZE 
!
! --- Set stacksize
!
      IS = SET_STACKSIZE ( %VAL(STACK_SIZE_IN_BYTES) )
      CALL INCH8    ( STACK_SIZE_IN_BYTES/INT8(1024), STR )
      CALL SETENV   ( 'GOMP_STACKSIZE'//CHAR(0), TRIM(STR)//CHAR(0), %VAL(1) )
!
      IF ( IARGC() < 3 ) THEN
           WRITE ( 6, '(A)' ) 'Usage: gvf_transform -to_ascii|-to_binary '// &
     &                        '{input_file} {output_file} [repo]'
           CALL EXIT ( 1 )
      END IF
!
      CALL GETENVAR  ( 'VCAT_CONF', STR )
      IF ( ILEN(STR) > 0 ) THEN
           VCAT_CONF_FILE = STR
        ELSE
#ifdef GVH_STANDALONE
           CALL GETENVAR  ( 'PSOLVE_SAVE_DIR', STR )
           IF ( ILEN(STR) == 0 ) THEN
!
! ------------- Get solve SAVE_DIR via solve_inq mechanism
!
                CALL CLRCH ( PID_STR ) 
                WRITE ( UNIT=PID_STR, FMT='(I8)' ) GETPID()
                CALL BLANK_TO_ZERO (  PID_STR )
                FIL_COM = '/tmp/solve_inq__'//PID_STR//'.csh'
                FIL_TMP = '/tmp/solve_inq__'//PID_STR//'.log'
!
! ------------- Make a short command file
!
                COMS(1) = '#!/bin/csh -f'
                COMS(2) = 'solve_inq --save >& '//FIL_TMP
                IUER = -1
                CALL WR_TEXT ( 2, COMS, FIL_COM, IUER )
                IF ( IUER .NE. 0 ) THEN
                     IUER = -1
                     CALL ERR_LOG ( 1801, IUER, 'GVF_TRANSFORM_MAIN', &
     &                   'trap of internal control: failure in writing '// &
     &                   'into a temporary file '//FIL_COM )
                     CALL EXIT ( 1 )
                END IF
!
! ------------- Make it executable
!
                IS = SYSTEM ( 'chmod 777 '//TRIM(FIL_COM)//CHAR(0) )
!
! ------------- Exewcute
!
                IS = SYSTEM ( TRIM(FIL_COM)//CHAR(0)  )
!
! ------------- Remove the command file
!
                CALL UNLINK ( TRIM(FIL_COM)//CHAR(0) )
                IF ( IUER .NE. 0 ) THEN
                     IUER = -1
                     CALL ERR_LOG ( 1802, IUER, 'GVF_TRANSFORM_MAIN', &
     &                   'trap of internal control: failure in execution of '// &
     &                   'command '//COMS(2) )
                     CALL EXIT ( 1 )
                END IF
!
! ------------- Read the output
!
                IUER = -1
                CALL RD_TEXT ( FIL_TMP, MBUF, BUF, NC, IUER )
                IF ( IUER .NE. 0 ) THEN
                     IUER = -1
                     CALL ERR_LOG ( 1803, IUER, 'GVF_TRANSFORM_MAIN', &
     &                   'trap of internal control: cannot read file '// &
     &                    TRIM(FIL_TMP)//' created by command '//COMS(2) )
                     CALL EXIT ( 1 )
                END IF
!
! ------------- ... and remove the temporary output file
!
                CALL UNLINK ( TRIM(FIL_TMP)//CHAR(0) )
                IF ( IS .NE. 0 ) THEN
                     IUER = -1
                     CALL ERR_LOG ( 1804, IUER, 'GVF_TRANSFORM_MAIN', &
     &                   'trap of internal control: error in an attempt '// &
     &                   'to delete file '//TRIM(FIL_TMP)// &
     &                   ' created by command '//COMS(2) )
                     CALL EXIT ( 1 )
                END IF                     
                STR = BUF(1)
           END IF
#else 
           STR = SOLVE_SAVE_DIR
#endif
           IF ( STR(I_LEN(STR):I_LEN(STR)) .NE. '/' ) THEN
                STR = STR(1:I_LEN(STR))//'/'
           END IF
!
           VCAT_CONF_FILE = STR(1:I_LEN(STR))//'vcat.conf'
      END IF
!
      IUER = -1
      CALL VCAT_GET_CONF ( VCAT_CONF_FILE, VCAT, IUER )
      IF ( IUER .NE. 0 ) THEN
           CALL EXIT ( 1 ) 
      END IF
!
      CALL GETARG ( 1, MODE_STR     )
      CALL GETARG ( 2, FIL_ENV_ORIG )
      CALL GETARG ( 3, FIL_ENV_OUT  )
      FIL_ENV_IN = FIL_ENV_ORIG
!
      IF ( IARGC() .GE. 4 ) THEN
           CALL GETARG ( 4, REPO  )
         ELSE
           CALL GETENVAR ( 'VCAT_REPO', REPO )
           IF ( ILEN(REPO) == 0 ) THEN
                REPO = REPO__DEF
           END IF
      END IF
      CALL TRAN ( 12, MODE_STR, MODE_STR )
      IF ( MODE_STR == '-to_ascii' ) THEN
           IL = ILEN(FIL_ENV_IN)
           ID = LINDEX ( FIL_ENV_IN, '/' )
           IF ( ID == 2 ) THEN
                FIL_ENV_IN = FIL_ENV_IN(3:)
                CALL CLRCH ( GVF_ENV_DIR )
             ELSE IF ( ID > 0 ) THEN
                GVF_ENV_DIR = FIL_ENV_IN(1:ID-1)
                FIL_ENV_IN = FIL_ENV_IN(ID+1:)
           END IF
!
           IND_REP = 0
           DO 410 J1=1,VCAT%NREPS
              IF ( ILEN(GVF_ENV_DIR) == 0 ) THEN
                   IF ( VCAT%GVF_REP_NAME(J1) == REPO ) THEN
                        IND_REP = J1
                   END IF
                ELSE
                   IF ( GVF_ENV_DIR == VCAT%GVF_ENV_DIR(J1) ) THEN
                        IND_REP = J1
                        REPO = VCAT%GVF_REP_NAME(J1)
                   END IF
              END IF
 410       CONTINUE 
           IF ( IND_REP == 0 ) THEN
                IUER = -1
                CALL ERR_LOG ( 1805, IUER, 'GVF_TRANSFORM_MAIN', &
     &              'Repository '//TRIM(REPO)//' specified in the '// &
     &              'environment variable VCAT_REPO is not '// &
     &              'defined in the VCAT configuration file '// &
     &              VCAT%CONF_FILE )
                CALL EXIT ( 1 )
           END IF
!
! -------- Resolve database name
!
           IUER = -1
           CALL VCAT_RESOLVE_DBNAME ( VCAT, FIL_ENV_IN, REPO, ENV_FILE, M_FIL, &
     &                                L_FIL, DB_FILE, IUER )
           IF ( IUER .NE. 0 ) THEN
                IUER = -1
                CALL ERR_LOG ( 1806, IUER, 'GVF_TRANSFORM_MAIN', 'Error in an '// &
     &              'attempt to resolve database name '//FIL_ENV_IN )
                CALL EXIT ( 1 )
           END IF
           ID = LINDEX ( ENV_FILE, '/' )
           GVF_ENV_DIR = ENV_FILE(1:ID-1)
           ID = LINDEX ( DB_FILE(1), '/' )
           GVF_DB_DIR  = DB_FILE(1)(1:ID-1)
         ELSE IF ( MODE_STR == '-to_binary' ) THEN
!
! -------- Determine the repository index
!
           IF ( ILEN(REPO) == 0 ) THEN
                IND_REP = 1
              ELSE
                IND_REP = 0
                DO 420 J2=1,VCAT%NREPS
                   IF ( REPO == VCAT%GVF_REP_NAME(J2) ) THEN
                        IND_REP = J2
                   END IF
 420            CONTINUE 
                IF ( IND_REP == 0 ) THEN
                     IUER = -1
                     CALL ERR_LOG ( 1807, IUER, 'GVF_TRANSFORM_MAIN', &
     &                   'Repository '//TRIM(REPO)//' specified in the '// &
     &                   'environment variable VCAT_REPO is not '// &
     &                   'defined in the VCAT configuration file '// &
     &                    VCAT%CONF_FILE )
                     CALL EXIT ( 1 )
                END IF
           END IF
           L_FIL = 1
           DB_FILE(1)  = FIL_ENV_IN
           GVF_ENV_DIR = VCAT%GVF_ENV_DIR(IND_REP) 
           GVF_DB_DIR  = VCAT%GVF_DB_DIR(IND_REP) 
         ELSE 
           IUER = -1
           CALL ERR_LOG ( 1808, IUER, 'GVF_TRANSFORM_MAIN', 'Unsupported '// &
     &         'first argument: '//MODE_STR )
           CALL EXIT ( 1 )
      END IF
!      
      IUER = -1
      CALL GVF_TRANSFORM ( MODE_STR, ENV_FILE, L_FIL, DB_FILE, &
     &                     FIL_ENV_OUT, GVF_DB_DIR, GVF_ENV_DIR, IUER )
      IF ( IUER .NE. 0 ) CALL EXIT ( 1 )
!
      END  PROGRAM   GVF_TRANSFORM_MAIN  !#!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE GVF_TRANSFORM ( MODE_STR, ENV_FILE, LIN_FIL, FILIN_GVF, &
     &                           FIL_ENV_OUT, GVF_DB_DIR, GVF_ENV_DIR, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine GVF_TRANSFORM
! *                                                                      *
! * ### 31-JUL-2007  GVF_TRANSFORM  v3.4 (c) L. Petrov  29-NOV-2019 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
#ifdef GVH_STANDALONE
      INCLUDE   'gvh_solve.i'
#else
      INCLUDE   'solve.i'
#endif
      INCLUDE   'astro_constants.i'
      INCLUDE   'gvh.i'
      TYPE     ( GVH__STRU ) :: GVH, GVH_NS
      INTEGER*4  LIN_FIL, IUER
      CHARACTER  MODE_STR*(*), ENV_FILE*(*), FILIN_GVF(LIN_FIL)*(*), &
     &           FIL_ENV_OUT*(*), GVF_DB_DIR*(*), GVF_ENV_DIR*(*)
      INTEGER*4    M_ENV
      PARAMETER  ( M_ENV = 32 )
      LOGICAL*1  LEX, FL_COMPR, FL_SOL
      CHARACTER  STR*128, OUTPUT_NAME*128, KEYWORD_STR*128, VALUE_STR*128, &
     &           DB_GEN*32, FILE_ENV_EMBEDDED*128, COMPR_COM*256, SESS_NAME*10, &
     &           EXP_NAME*16, BGV_DISP*3
      CHARACTER  STR_DAT*8, STR_SUF*1, STR_DEL*2, STR_VER*3, BUF_ENV(M_ENV)*32
      INTEGER*4  J1, J2, J3, J4, J5, J6, REMAINED_BYTES, OPCODE, IVAL, L_ENV, &
     &           IL, IL1, ID, IP, IS, MODE_OUT, IER
      LOGICAL*1  FL_DEBUG
      INTEGER*4, EXTERNAL :: ILEN, I_LEN, LINDEX, LTM_DIF, SYSTEM
!
      FL_DEBUG = .TRUE.
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_INIT ( GVH,  IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 1811, IUER, 'GVF_TRANSFORM', 'Error in an atttempt to '// &
     &         'initialize GVH' )
           RETURN
      END IF
!
      IF ( MODE_STR =='-to_ascii' ) THEN
           IF ( FL_DEBUG ) THEN 
                WRITE ( 6, * ) 'Using envelop file '//TRIM(ENV_FILE)
           END IF
           DO 410 J1=1,LIN_FIL
              IF ( FL_DEBUG ) THEN
                   WRITE ( 6, * ) 'Reading file '//TRIM(FILIN_GVF(J1))
              END IF
              CALL ERR_PASS ( IUER, IER )
              CALL GVH_READ_BGV ( GVH, 1, FILIN_GVF(J1), REMAINED_BYTES, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 1812, IUER, 'GVF_TRANSFORM', 'Error in '// &
     &                 'an atttempt to read input database file '// &
     &                  FILIN_GVF(J1) )
                   RETURN 
              END IF
              IF ( REMAINED_BYTES .NE. 0 ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( REMAINED_BYTES, STR )
                   CALL ERR_LOG ( 1813, IUER, 'GVF_TRANSFORM', 'The number '// &
     &                 'of remaining bytes after reading input databae file '// &
     &                  FILIN_GVF(J1)(1:I_LEN(FILIN_GVF(J1)))// &
     &                  ' is not 0, but '//STR )
                   RETURN 
              END IF
 410       CONTINUE 
           CALL CLRCH ( GVH%GENERATOR ) 
           IF ( GVH%PREA(1)%NKWD .GE. 1 ) THEN
                DO 420 J2=1,GVH%PREA(1)%NKWD
                   CALL GVH_GPREA ( GVH, 1, J2, KEYWORD_STR, VALUE_STR, IER )
                   IF ( INDEX( KEYWORD_STR, 'GENERATOR:' ) > 0 ) THEN
                        GVH%GENERATOR = VALUE_STR
                   END IF
 420            CONTINUE 
           END IF
           IF ( GVH%GENERATOR(1:12) == 'mark3_to_gvf' ) THEN
                CALL ERR_PASS ( IUER, IER )
                CALL GVH_FROM_MARK3 ( GVH, IER )
                IF ( IER .NE. 0 ) THEN
                     CALL ERR_LOG ( 1814, IUER, 'GVF_TRANSFORM', 'Error in an '// &
     &                   'atttempt to correct GVH object created b mark3_to_gvf' )
                     RETURN
                END IF
              ELSE
                CALL ERR_PASS ( IUER, IER )
                CALL GVH_PREGET ( GVH, IER )
                IF ( IER .NE. 0 ) THEN
                     CALL ERR_LOG ( 1815, IUER, 'GVF_TRANSFORM', 'Error in an '// &
     &                   'attempt to execute GVH_PREGET' )
                     RETURN 
                END IF
           END IF
!
! -------- Determine the name of the output ascii file in vgosda format
!
           IP = INDEX ( FIL_ENV_OUT, '@' ) 
           IF ( IP > 0 ) THEN
!
! ------------- Special case when we determine the output name from the session name
!
                ID = LINDEX ( FILIN_GVF(1), '/' )
!
! ------------- Get session name
!
                SESS_NAME = FILIN_GVF(1)(ID+1:ID+10)
!
! ------------- Build the output name
!
                OUTPUT_NAME = FIL_ENV_OUT(1:IP-1)//SESS_NAME//'.vda'
!
! ------------- Determine whether we need run compression
!
                IP = INDEX ( FIL_ENV_OUT, '.bz2' )
                IF ( IP > 0 ) THEN
                     FL_COMPR = .TRUE.
                   ELSE
                     FL_COMPR = .FALSE.
                END IF
              ELSE
!
! ------------- Determine whether compression with lbzip2 is requested
!
                IP = LINDEX ( FIL_ENV_OUT, '.bz2' )
                IF ( IP > 0 ) THEN
!
! ------------------ Yes. Remove .bz2 from the file name
!
                     FL_COMPR = .TRUE.
                     OUTPUT_NAME = FIL_ENV_OUT(1:IP-1)
                   ELSE
                     FL_COMPR = .FALSE.
                     OUTPUT_NAME = FIL_ENV_OUT
                END IF
           END IF
!
           DO 430 J3=1,LIN_FIL
              IF ( J3 == 1 ) THEN
                   OPCODE = GVH__CRT
                ELSE
                   OPCODE = GVH__APP
              END IF
!
              CALL ERR_PASS ( IUER, IER )
              CALL GVH_WRITE_AGV ( GVH, J3, OPCODE, OUTPUT_NAME, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 1816, IUER, 'GVF_TRANSFORM', 'Error in '// &
     &                 'attempt to write output database file '//OUTPUT_NAME )
                   RETURN 
              END IF
 430       CONTINUE 
           IF ( FL_COMPR ) THEN
!
! ------------- Run compression of the output file with lbzip2
!
                COMPR_COM = 'lbzip2 -f -n1 '//OUTPUT_NAME
                IS = SYSTEM ( TRIM(COMPR_COM)//CHAR(0) )
                IF ( IS .NE. 0 ) THEN
                     CALL ERR_LOG ( 1817, IUER, 'GVF_TRANSFORM', 'Error in '// &
     &                   'attempt to compress the output database file with '// &
     &                   'command '//COMPR_COM )
                     RETURN 
                END IF
                OUTPUT_NAME = TRIM(OUTPUT_NAME)//'.bz2'
           END IF
!
           WRITE ( 6, '(A)' ) 'Output file: '//TRIM(OUTPUT_NAME)
        ELSE IF ( MODE_STR =='-to_binary' ) THEN
           L_ENV = -1
           IF ( FIL_ENV_OUT == '@' ) THEN
                MODE_OUT = 1
              ELSE IF ( FIL_ENV_OUT(1:1) == '@'                 .AND. &
     &                  ICHAR(FIL_ENV_OUT(2:2)) .GE. ICHAR('a') .AND. &
     &                  ICHAR(FIL_ENV_OUT(2:2)) .LE. ICHAR('z')       ) THEN
                MODE_OUT = 2
              ELSE
                MODE_OUT = 3
                IF ( ILEN(FIL_ENV_OUT) == 19 ) THEN
                     STR_DAT = FIL_ENV_OUT(1:8)
                     STR_SUF = FIL_ENV_OUT(10:10)
                     STR_DEL = FIL_ENV_OUT(11:12)
                     STR_VER = FIL_ENV_OUT(13:15)
                   ELSE IF ( ILEN(FIL_ENV_OUT) == 15 ) THEN
                     STR_DAT = FIL_ENV_OUT(1:8)
                     STR_SUF = FIL_ENV_OUT(10:10)
                     STR_DEL = FIL_ENV_OUT(11:12)
                     STR_VER = FIL_ENV_OUT(13:15)
                   ELSE IF ( ILEN(FIL_ENV_OUT) == 10 ) THEN
                     STR_DAT = FIL_ENV_OUT(1:8)
                     STR_SUF = FIL_ENV_OUT(10:10)
                     STR_DEL = '_v'
                     STR_VER = '001'
                   ELSE
                     CALL ERR_LOG ( 1818, IUER, 'GVF_TRANSFORM', 'Malformed '// &
     &                   'name of the output database '//FIL_ENV_OUT )
                     RETURN 
                END IF
                CALL CHIN ( STR_DAT, IVAL )
                IF ( IVAL < 19700101 .OR. IVAL > 20991231 ) THEN
                     CALL ERR_LOG ( 1819, IUER, 'GVF_TRANSFORM', 'Malformed '// &
     &                   'name of the output database '//TRIM(FIL_ENV_OUT)// &
     &                   ' -- the first 8 characters should be the date in yyyymmdd'// &
     &                   ' format' )
                     RETURN 
                END IF
!
                IF ( ( ICHAR(STR_SUF) .GE. ICHAR('0') .AND.        &
     &                 ICHAR(STR_SUF) .LE. ICHAR('9')       ) .OR. &
     &               ( ICHAR(STR_SUF) .GE. ICHAR('a') .AND.        &
     &                 ICHAR(STR_SUF) .LE. ICHAR('z')       )      ) THEN
                     CONTINUE 
                   ELSE
                     CALL ERR_LOG ( 1820, IUER, 'GVF_TRANSFORM', 'Malformed '// &
     &                   'name of the output database '//TRIM(FIL_ENV_OUT)//    &
     &                   ' -- the 10th character should be either a digit or a letter'// &
     &                   ' format' )
                     RETURN 
                END IF
                IF ( STR_DEL .NE. '_v' ) THEN
                     CALL ERR_LOG ( 1821, IUER, 'GVF_TRANSFORM', 'Malformed '// &
     &                   'name of the output database '//TRIM(FIL_ENV_OUT)// &
     &                   ' -- the characters 10:12 should be _v' )
                     RETURN 
                END IF
                CALL CHIN ( STR_VER, IVAL )
                IF ( IVAL < 1.OR. IVAL > 999 ) THEN
                     CALL ERR_LOG ( 1822, IUER, 'GVF_TRANSFORM', 'Malformed '// &
     &                   'name of the output database '//TRIM(FIL_ENV_OUT)// &
     &                   ' -- the version counter should be in a range of 1 to 999' )
                     RETURN 
                END IF
                FIL_ENV_OUT = STR_DAT//'_'//STR_SUF//STR_DEL//STR_VER
           END IF
!
           CALL ERR_PASS ( IUER, IER )
           CALL GVH_READ_AGV ( GVH, 0, FILIN_GVF, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 1823, IUER, 'GVF_TRANSFORM', 'Error in '// &
     &              'an atttempt to read input database file '// &
     &               FILIN_GVF(1) )
                RETURN 
           END IF
           DB_GEN = GVH%GENERATOR 
           CALL CLRCH ( FILE_ENV_EMBEDDED )
!
           IF ( GVH%GENERATOR(1:7) == 'nuSolve' ) THEN
                IF ( FIL_ENV_OUT == '@' ) THEN
                     CALL ERR_LOG ( 1824, IUER, 'GVF_TRANSFORM', 'Malformed '// &
     &                   'name of the output file: @ is not allowed for vgosda '// &
     &                   'files generated by nuSolve' )
                     RETURN
                END IF
!
! ------------- Copy the current GVH object to GVH_NS (nuSolve style)
!
                GVH_NS = GVH
!
                CALL ERR_PASS ( IUER, IER )
                CALL GVH_INIT ( GVH, IER )
                IF ( IER .NE. 0 ) THEN
                     CALL ERR_LOG ( 1825, IUER, 'GVF_TRANSFORM', 'Error in an '// &
     &                   'atttempt to initialize GVH memory' )
                     RETURN
                END IF
!
                CALL ERR_PASS ( IUER, IER )
                CALL GVH_FROM_NUSOLVE ( GVH_NS, GVH, GVF_DB_DIR, FIL_ENV_OUT, &
     &                                  M_ENV, L_ENV, BUF_ENV, IER )
                IF ( IER .NE. 0 ) THEN
                     CALL ERR_LOG ( 1826, IUER, 'GVF_TRANSFORM', 'Error in an '// &
     &                   'attempt to convert nuSolve GVH object while processing '// &
     &                   'input file '//FILIN_GVF(1) )
                     RETURN
                END IF
              ELSE IF ( GVH%GENERATOR(1:4)  == 'PIMA'               .OR. &
     &                  GVH%GENERATOR(1:12) == 'mark3_to_gvf'       .OR. &
     &                  GVH%GENERATOR(1:18) == 'mark3_to_gvh_proto' .OR. &
     &                  GVH%GENERATOR(1:13) == 'gvf_transform'           ) THEN
                DO 440 J4=1,GVH%NSEG
                   ID = LINDEX ( GVH%OLD_FILENAME(J4), '/' )
                   CALL CLRCH ( STR )
!
! ---------------- STR is the base file name now
!
                   CALL CLRCH ( EXP_NAME )
                   EXP_NAME = GVH%OLD_FILENAME(J4)(ID+12:)
                   ID = INDEX ( EXP_NAME, '_' )
                   IF ( ID > 0 ) THEN
                        CALL CLRCH ( EXP_NAME(ID:) )
                   END IF                   
!
                   STR = GVH%OLD_FILENAME(J4)(ID+10:)
                   IL = ILEN(STR)
                   ID = LINDEX ( GVH%OLD_FILENAME(J4), '/' )
!
! ---------------- Build the envelop file from the old file name of binary files stored 
! ---------------- in the database FILE section
!
                   IF ( MODE_OUT == 2 ) THEN
                        FILE_ENV_EMBEDDED = GVH%OLD_FILENAME(J4)(ID+1:ID+9)// &
     &                                      FIL_ENV_OUT(2:2)//STR(IL-8:IL-4)
                      ELSE
                        FILE_ENV_EMBEDDED = GVH%OLD_FILENAME(J4)(ID+1:ID+9)// &
     &                                      FIL_ENV_OUT(2:2)//STR(IL-8:IL-4)
                   END IF
                   IF ( GVH%DB_VERS > 0 .AND. GVH%DB_VERS < 1000 ) THEN
                        IL1 = ILEN(FILE_ENV_EMBEDDED)
                        WRITE ( UNIT=FILE_ENV_EMBEDDED(IL1-2:IL1), FMT='(I3.3)' ) GVH%DB_VERS 
                   END IF
                   IF ( STR(IL-11:IL-9) == 'th1' ) THEN
                        BGV_DISP = 'OPT'
                      ELSE 
                        BGV_DISP = 'MAN'
                   END IF
                   IF ( MODE_OUT == 2 ) THEN
                        BUF_ENV(J4) = 'SYS '//BGV_DISP//' '//STR(IL-11:IL-9)//' '// &
     &                                 STR(IL-6:IL-4)//' bgv '// &
     &                                 FIL_ENV_OUT(2:2)//' '//TRIM(EXP_NAME)
                      ELSE
                        BUF_ENV(J4) = 'SYS '//BGV_DISP//' '//STR(IL-11:IL-9)//' '// &
     &                                 STR(IL-6:IL-4)//' bgv '// &
     &                                 GVH%OLD_FILENAME(J4)(ID+10:ID+10)//' '//TRIM(EXP_NAME)
                   END IF
 440            CONTINUE 
                L_ENV = GVH%NSEG
           END IF
           IF ( L_ENV < 1 ) THEN
                CALL ERR_LOG ( 1827, IUER, 'GVF_TRANSFORM', 'Trap of '// &
     &              'internal control: no GVH section were found in '// &
     &              'database file '//TRIM(FILIN_GVF(1))// &
     &              '. Generator: '//GVH%GENERATOR )
                RETURN 
           END IF
           GVH%GENERATOR = 'gvf_transform '
           GVH%VERSION   =  GVH__LABEL
!
           CALL ERR_PASS ( IUER, IER )
           CALL GVH_PREGET ( GVH, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 1828, IUER, 'GVF_TRANSFORM', 'Error in an '// &
     &              'attempt to execute GVH_PREGET' )
                RETURN 
           END IF
!
           FL_SOL = .FALSE.
           DO 450 J5=1,GVH%NSEG
              OPCODE = GVH__CRT
              ID = LINDEX ( GVH%OLD_FILENAME(J5), '/' )
              IF ( LINDEX ( GVH%OLD_FILENAME(J5), '_sl1_' ) > ID ) THEN
                   FL_SOL = .TRUE.
              END IF 
              IF ( MODE_OUT == 2 .AND. FIL_ENV_OUT(1:1) == '@' ) THEN
                   GVH%OLD_FILENAME(J5)(ID+10:ID+10) = FIL_ENV_OUT(2:2)
              END IF
              OUTPUT_NAME = TRIM(GVF_DB_DIR)//GVH%OLD_FILENAME(J5)(ID:)
              CALL ERR_PASS ( IUER, IER ) 
              CALL GVH_WRITE_BGV ( GVH, J5, OPCODE, OUTPUT_NAME, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 1829, IUER, 'GVF_TRANSFORM', 'Error in '// &
     &                 'attempt to write output database file '//OUTPUT_NAME )
                   RETURN 
              END IF
 450       CONTINUE 
           IF ( FIL_ENV_OUT(1:1) .EQ. '@' ) THEN
!
! ------------- A special output file name.
! ------------- Let us check, whether the enevolope file really exists
!
                INQUIRE ( FILE=TRIM(GVF_ENV_DIR)//'/'//TRIM(FILE_ENV_EMBEDDED)//'.env', EXIST=LEX )
                IF ( .NOT. LEX ) THEN
!
! ------------------ Does not exist. Let is build it form GVH%OLD_FILENAME
!
                     ID = LINDEX ( GVH%OLD_FILENAME(1), '/' )
                     IF ( GVH%DB_VERS > 0 ) THEN
!
! ----------------------- If the database version was stored in the LCODE DB_VERS, 
! ----------------------- let us use it
!
                          CALL CLRCH ( STR )
                          WRITE ( UNIT=STR(1:3), FMT='(I3)' ) GVH%DB_VERS
                          CALL CHASHR ( STR(1:3) )
                          CALL BLANK_TO_ZERO ( STR(1:3) )
                          FIL_ENV_OUT = GVH%OLD_FILENAME(1)(ID+1:ID+10)//'_v'//STR(1:3)
                       ELSE
!
! ----------------------- Database version was not stored
!
                          IF ( FL_SOL ) THEN
                               FIL_ENV_OUT = GVH%OLD_FILENAME(1)(ID+1:ID+10)//'_v002'
                             ELSE
                               FIL_ENV_OUT = GVH%OLD_FILENAME(1)(ID+1:ID+10)//'_v001'
                          END IF
                     END IF
                END IF
           END IF
!
           ID = LINDEX ( GVH%OLD_FILENAME(1), '/' )
           IF ( FIL_ENV_OUT(1:1) .NE. '@' ) THEN
!
! ------------- Write down the envelope file
!
                FIL_ENV_OUT = TRIM(GVF_ENV_DIR)//'/'//TRIM(FIL_ENV_OUT)//'.env'
                CALL ERR_PASS ( IUER, IER )
                CALL WR_TEXT  ( L_ENV, BUF_ENV, FIL_ENV_OUT, IER )
                IF ( IER .NE. 0 ) THEN
                     CALL ERR_LOG ( 1830, IUER, 'GVF_TRANSFORM', 'Error in '// &
     &                   'attempt to write output database file '//FIL_ENV_OUT )
                     RETURN 
                END IF
                WRITE ( 6, '(A)' ) 'Created output env-file '//TRIM(FIL_ENV_OUT)
              ELSE
!
! ------------- We do not need write the envelope file
!
                DO 460 J6=1,GVH%NSEG
                   OUTPUT_NAME = GVH%OLD_FILENAME(J6)
                   ID = LINDEX ( OUTPUT_NAME, '/' )
                   OUTPUT_NAME = TRIM(GVF_DB_DIR)//OUTPUT_NAME(ID:)
                   WRITE ( 6, '(A)' ) 'Created output bgf-file '//TRIM(OUTPUT_NAME)
 460            CONTINUE 
                WRITE ( 6, '(A)' ) 'Updated bgv files for '//TRIM(GVF_ENV_DIR)//'/'//TRIM(FILE_ENV_EMBEDDED)//'.env'
           END IF
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  GVF_TRANSFORM  !#!#
