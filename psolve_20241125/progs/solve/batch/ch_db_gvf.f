      SUBROUTINE CH_DB_GVF ( VCAT, STRING, N_SES, N_MIS, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine CH_DB_GVF 
! *                                                                      *
! *  ### 04-MAY-2007   CH_DB_GVF   v2.3 (c)  L. Petrov  22-SEP-2023 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'solve.i'
      INCLUDE   'precm.i'
      INCLUDE   'vcat.i'
      TYPE     ( VCAT__TYPE ) ::  VCAT
      CHARACTER  STRING*(*)
      INTEGER*4  N_SES, N_MIS, IUER 
      CHARACTER  VCAT_CONF_FILE*128, DB_NAME*128, &
     &           DB_VERS_STR*8, ARCFILE*128, SESS_FILE*128, &
     &           ENV_FILE*128, STRING_ORIG*2048, REPO_NAME*128, &
     &           STR*128, STR1*4096, SAVE_DIR*128
      INTEGER*4    M_FIL, M_EXP
      PARAMETER  ( M_FIL = 32 )
      PARAMETER  ( M_EXP =  8 )
      CHARACTER    DB_FILE(M_FIL)*128
      CHARACTER*128, ALLOCATABLE :: BUF_ENV(:,:)
      ADDRESS__TYPE :: DIR_DESC(16)
      INTEGER*4  J1, J2, J3, J4, J5, J6, LEV, IS, N_FIL(VCAT__MDIRS), &
     &           N_DB, IP, ILN, L_FIL, IB, IC, IE, IL, IR, IM, IND_SES, &
     &           LUN_SES, ILINE, IND_REP, IER
      LOGICAL*4  FL_WILD_CARD, FL_SESS_FILE
      INTEGER*2  INT2_ARG
      INTEGER*4  INT4
      INT4(INT2_ARG) = INT(INT2_ARG,KIND=4)
#ifdef GNU
      INTEGER*4, EXTERNAL :: STR_COMPAR 
#else
      INTEGER*2, EXTERNAL :: STR_COMPAR 
#endif
      INTEGER*4, EXTERNAL :: GET_FILE_FROM_DIR, I_LEN, ILEN, LINDEX, GET_UNIT
!
      N_SES = 0
      CALL GETENVAR  ( 'VCAT_CONF', VCAT_CONF_FILE )
      IF ( ILEN(VCAT_CONF_FILE) == 0 ) THEN
           CALL GETENVAR ( 'PSOLVE_SAVE_DIR', SAVE_DIR )
           IF ( ILEN(SAVE_DIR) == 0 ) THEN
                VCAT_CONF_FILE = SOLVE_SAVE_DIR//'/vcat.conf'
              ELSE
                VCAT_CONF_FILE = TRIM(SAVE_DIR)//'/vcat.conf'
           END IF
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL VCAT_GET_CONF ( VCAT_CONF_FILE, VCAT, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 8561, IUER, 'CH_DB_GVF', 'Error in an attempt '// &
     &         'to parse VCAT configuration file' )
           RETURN 
      END IF
!
      ALLOCATE ( BUF_ENV(MAX_SUP,VCAT%NREPS), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( INT4(MAX_SUP)*VCAT%NREPS*128, STR )
           CALL ERR_LOG ( 8562, IUER, 'CH_DB_GVF', 'Error in an attempt '// &
     &         'to allocate '//STR(1:I_LEN(STR))//' bytes of dynamic '// &
     &         'memory' )
           RETURN 
      END IF
!
      N_FIL = 0
      DO 410 J1=1,VCAT%NREPS
         LEV = 0
         DO 420 J2=1,MAX_SUP
            IS = GET_FILE_FROM_DIR ( LEV, DIR_DESC, VCAT%GVF_ENV_DIR(J1), &
     &                               BUF_ENV(N_FIL(J1)+1,J1) )
            IF ( LEV == 0 ) GOTO 810
            IF ( IS .NE. 0 ) THEN
                 CALL ERR_LOG ( 8563, IUER, 'CH_DB_GVF', 'Error in reading '// &
     &                TRIM(VCAT%GVF_ENV_DIR(J1))//' envelop directory' )
                 RETURN 
            END IF
            N_FIL(J1) = N_FIL(J1) + 1
 420     CONTINUE 
         CALL CLRCH ( STR )
         CALL INCH  ( INT4(MAX_SUP), STR )
         WRITE ( 6, * ) 'N_FIL = ', N_FIL(J1)
         CALL ERR_LOG ( 8564, IUER, 'CH_DB_GVF', 'The number of databases '// &
     &       'in directory '//TRIM(VCAT%GVF_ENV_DIR(J1))//' has '// &
     &       'exceeded the limit of '//STR )
         RETURN 
 810     CONTINUE 
!
         CALL FOR_QSORT ( %REF(BUF_ENV(1,J1)), N_FIL(J1), LEN(BUF_ENV(1,1)), &
     &                     STR_COMPAR )
 410  CONTINUE 
!
      N_MIS = 0
      FL_SESS_FILE = .FALSE.
      SESS_FILE = 'Batch control file'
      ILINE = 0
      DO 430 J2=1,MAX_SUP
         CALL CHASHL ( STRING )
         DO 440 J4=1,M_EXP
            IB = INDEX ( STRING, '${' )
            IF ( IB > 1 ) THEN
                 IE = INDEX ( STRING(IB+1:), '}' ) + IB
                 IF ( IE < IB+1 ) THEN
                      CALL ERR_LOG ( 8565, -2, 'CH_DB_GVF', 'Error in processing line '// &
     &                     TRIM(STRING)//' -- there is no matching }' )
                 END IF
!
                 CALL GETENVAR ( STRING(IB+2:IE-1), STR1 )
                 IF ( ILEN(STR1) < 1 ) THEN
                      IUER = -1
                      CALL ERR_LOG ( 8566, -2, 'CH_DB_GVF', 'Error in processing line '// &
     &                     TRIM(STRING)//' -- environment variable '//STRING(IB+2:IE-1)// &
     &                    ' is not defined' )
                      CALL EXIT ( 1 )
                 END IF
                 IF ( IB == 1 ) THEN
                      STRING = TRIM(STR1)//STRING(IE+1:)
                    ELSE
                      STRING = STRING(1:IB-1)//TRIM(STR1)//STRING(IE+1:)
                 END IF
            END IF
 440     CONTINUE 
         IF ( STRING(1:1) == '!' ) GOTO 830
         IF ( STRING(1:1) == '#' ) GOTO 830
         IF ( STRING(1:1) == '*' ) GOTO 830
         IF ( STRING(1:1) == ' ' ) THEN
              IF ( FL_SESS_FILE ) THEN
                   CLOSE ( UNIT=LUN_SES )
                   CALL CLRCH ( STRING )
                   READ ( UNIT=30, FMT='(A)', IOSTAT=IER ) STRING
                   STRING_ORIG = STRING
                   IF ( IER == -1 ) GOTO 730
                   FL_SESS_FILE = .FALSE.
                   GOTO 430
                 ELSE 
                   GOTO 730
              END IF
         END IF
!
         IF ( .NOT. FL_SESS_FILE  .AND.  STRING(1:7) == 'ARCFILE' ) THEN
              SESS_FILE = STRING(8:)
              CALL CHASHL ( SESS_FILE )
              IF ( SESS_FILE(1:5) == 'NONE ' ) GOTO 430
!
              FL_SESS_FILE = .TRUE.
              LUN_SES = GET_UNIT()
              OPEN ( UNIT=LUN_SES, FILE=SESS_FILE, STATUS='OLD', IOSTAT=IER )
              IF ( IER .NE. 0 ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( IER, STR )
                   CALL ERR_LOG ( 8567, IUER, 'CH_DB_GVH', 'Error in '// &
     &                 'opening session file '// &
     &                  SESS_FILE(1:I_LEN(SESS_FILE))//' ERR: '//STR )
                   RETURN 
              END IF
              CALL CLRCH ( STRING )
              READ ( UNIT=LUN_SES, FMT='(A)', IOSTAT=IER ) STRING
              STRING_ORIG = STRING
              ILINE = 1
              GOTO 430
         END IF
!
         CALL SPLITSTRING ( STRING, REPO_NAME,   STRING )
         CALL SPLITSTRING ( STRING, DB_NAME,     STRING )
         CALL SPLITSTRING ( STRING, DB_VERS_STR, STRING )
         IF ( ILEN(DB_VERS_STR) == 0 ) DB_VERS_STR = '0'
         IF ( DB_VERS_STR(1:2) == '0 ' ) THEN
              CALL CLRCH ( DB_NAME(11:) )
              FL_WILD_CARD = .TRUE.
            ELSE IF ( ILEN(DB_VERS_STR) == 1 ) THEN
              DB_NAME = DB_NAME(1:I_LEN(DB_NAME))//'_v00'// &
     &                  DB_VERS_STR(1:1)//'.env'
              FL_WILD_CARD = .FALSE.
            ELSE IF ( ILEN(DB_VERS_STR) == 2 ) THEN
              DB_NAME = DB_NAME(1:I_LEN(DB_NAME))//'_v0'// &
     &                  DB_VERS_STR(1:2)//'.env'
              FL_WILD_CARD = .FALSE.
            ELSE IF ( ILEN(DB_VERS_STR) == 3 ) THEN
              DB_NAME = DB_NAME(1:I_LEN(DB_NAME))//'_v'// &
     &                  DB_VERS_STR(1:3)//'.env'
              FL_WILD_CARD = .FALSE.
         END IF
         ILN = ILEN(DB_NAME)
         IND_REP = 0
         DO 450 J5=1,VCAT%NREPS
            IF ( REPO_NAME == VCAT%GVF_REP_NAME(J5) ) THEN
                 IND_REP = J5
            END IF
 450     CONTINUE 

         IF ( IND_REP > 0 ) THEN
!
! ----------- Binary search
!
              IL = 1
              IR = N_FIL(IND_REP)
              IND_SES = 0
 950          CONTINUE 
                 IM = IL + (IR - IL)/2
                 IF ( IL .GT. IR ) THEN
                      IND_SES = 0
                      GOTO 850
                 END IF
                 STR = BUF_ENV(IM,IND_REP)
                 IP = LINDEX ( STR, '/' )
                 IF ( IP > 0 ) THEN
                      CALL CLRCH  ( STR(1:IP) )
                      CALL CHASHL ( STR       )
                 END IF
                 IF ( FL_WILD_CARD ) THEN
                      IC = STR_COMPAR ( DB_NAME(1:ILN), STR(1:ILN) )
                    ELSE 
                      IC = STR_COMPAR ( DB_NAME, STR )
                 END IF
                 IF ( IC < 0 ) THEN
                      IR = IM - 1
                      GOTO 950
                   ELSE IF ( IC > 0 ) THEN
                      IL = IM + 1
                      GOTO 950
                   ELSE IF ( IC == 0 ) THEN
                      IND_SES = IM
                      GOTO 850
                 END IF
              GOTO 950
 850          CONTINUE 
!
              IF ( FL_WILD_CARD ) THEN
                   IB = IM - 32
                   IE = IM + 32
                   IF ( IB < 1     ) IB = 1
                   IF ( IE > N_FIL(IND_REP) ) IE = N_FIL(IND_REP)
                   DO 460 J6=IB,IE
                      STR = BUF_ENV(J6,IND_REP)
                      IP = LINDEX ( STR, '/' )
                      IF ( IP > 0 ) THEN
                           CALL CLRCH  ( STR(1:IP) )
                           CALL CHASHL ( STR       )
                      END IF
                      IF ( STR(1:ILN) == DB_NAME(1:ILN) ) THEN
                           IND_SES = J6
                      END IF
 460               CONTINUE 
              END IF
!
! ----------- Check, whether we have found the database 
!
              IF ( IND_SES > 0 ) THEN
                   STR = BUF_ENV(IND_SES,IND_REP)
                   IP = LINDEX ( STR, '/' )
                   IF ( IP > 0 ) THEN
                        CALL CLRCH  ( STR(1:IP) )
                        CALL CHASHL ( STR       )
                   END IF
                   IF ( FL_WILD_CARD ) THEN
                        CALL CLRCH ( STR(ILN+1:) )
                   END IF
                 ELSE 
                   CALL CLRCH ( STR )
              END IF
              IF ( STR(1:ILN) == DB_NAME(1:ILN) ) THEN
                   N_SES = N_SES + 1
                   WRITE ( 35, '(A)' ) 'GVF '//TRIM(BUF_ENV(IND_SES,IND_REP))//'  '// &
     &                                  STRING(1:I_LEN(STRING))
                 ELSE 
                   N_MIS = N_MIS + 1
                   WRITE ( 6, '(A,I6,A)' ) 'Not found database '// &
     &                                 DB_NAME(1:I_LEN(DB_NAME))//'  '// &
     &                                 DB_VERS_STR(1:I_LEN(DB_VERS_STR))// &
     &                                 ' Line ', ILINE,' File: '// &
     &                                 SESS_FILE(1:I_LEN(SESS_FILE))// &
     &                                 ' String: "'// &
     &                                 STRING_ORIG(1:I_LEN(STRING_ORIG))//'"  '
              END IF
            ELSE
!
! ----------- Repository was not found
!
              N_MIS = N_MIS + 1
              WRITE ( 6, '(A,I6,A)' ) 'Not found database '//TRIM(DB_NAME)//'  '// &
     &                                 ' Line ', ILINE,' File: '//TRIM(SESS_FILE)// &
     &                                 ' String: "'//TRIM(STRING_ORIG)//'" '// &
     &                                 'because repository '//TRIM(REPO_NAME)// &
     &                                 ' is not defined in the VCAT configuration '// &
     &                                 ' file '//TRIM(VCAT%CONF_FILE)
         END IF
 830     CONTINUE 
         IF ( FL_SESS_FILE ) THEN
              READ ( UNIT=LUN_SES, FMT='(A)', IOSTAT=IER ) STRING
              IF ( IER .NE. 0  .AND.  IER .NE. -1 ) THEN
                   CALL ERR_LOG ( 8568, IUER, 'CH_DB_GVF', 'Error in '// &
     &                 'reading input session file '//SESS_FILE )
                   RETURN 
              END IF
            ELSE 
              READ ( UNIT=30, FMT='(A)', IOSTAT=IER ) STRING
              IF ( IER .NE. 0  .AND.  IER .NE. -1 ) THEN
                   CALL ERR_LOG ( 8569, IUER, 'CH_DB_GVF', 'Error in '// &
     &                 'reading input batch control file' )
                   RETURN 
              END IF
         END IF
!         
         IF ( IER == -1 .AND. FL_SESS_FILE ) THEN
              STRING = ' '
              FL_SESS_FILE = .FALSE.
              GOTO 830
         END IF
         IF ( IER == -1 .AND. .NOT. FL_SESS_FILE ) GOTO 730
         ILINE = ILINE + 1
         STRING_ORIG = STRING
 430  CONTINUE 
 730  CONTINUE 
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  CH_DB_GVF  !#!#
!
! ------------------------------------------------------------------------
!
      FUNCTION   STR_COMPAR ( STR1, STR2 )
! ************************************************************************
! *                                                                      *
! *   Auxilliary finction STR_COMPAR 
! *                                                                      *
! *  ### 04-MAY-2007  STR_COMPAR   v1.0 (c)  L. Petrov  04-MAY-2007 ###  *
! *                                                                      *
! ************************************************************************
#ifdef GNU
      INTEGER*4  STR_COMPAR 
#else
      INTEGER*2  STR_COMPAR 
#endif
      CHARACTER  STR1(128)*1, STR2(128)*1
      INTEGER*4  J1
!
      DO 410 J1=1,128
         IF ( STR1(J1) > STR2(J1) ) THEN
              STR_COMPAR = 1
              RETURN
            ELSE IF ( STR1(J1) < STR2(J1) ) THEN
              STR_COMPAR = -1
              RETURN
         END IF
 410  CONTINUE 
      STR_COMPAR = 0
      RETURN
      END   FUNCTION   STR_COMPAR  !#!  
