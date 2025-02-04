      PROGRAM   OPA_HEAD
! ************************************************************************
! *                                                                      *
! *   This routine is the header module of the program OPA (Operational  *
! *   Post-solve Analysis). It gathers run-string parameters, checks     *
! *   their syntax, calls interactive menu model and then execute        *
! *   a sequence of OPA operations.                                      *
! *                                                                      *
! *  ### 14-AUG-2000   OPA_HEAD    v1.4 (c)  L. Petrov  09-MAY-2020 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE    'solve.i'
      INCLUDE    'precm.i'
      INCLUDE    'opa.i'
      INTEGER*4  M_ARG, M_PAIR, IVRB__MIN, IVRB__MAX
      PARAMETER  ( M_ARG  = 10 )
      PARAMETER  ( M_PAIR =  5 )
      PARAMETER  ( IVRB__MIN = 0 )
      PARAMETER  ( IVRB__MAX = 1 )
      CHARACTER  ARG_ARR(M_ARG)*128, HELP_FILE*128, DB_NAME*10, SOLVE_INIT*2, &
     &           CONFIG_FILE*128, SOLVE_HELP_DIR_STR*128, STR*80, SESS_CODE*6, &
     &           DIR_NAME*128, OPC_FILE*128, DBFILE*128, OUT*4
      INTEGER*4  MASK
      DATA MASK / O'775' / ! Protection mask: read-write-execute for owner and
!                          ! group, read execute for others
      LOGICAL*4  LEX, LSUI, FL_GETMASTER, FL_NOCONFIRM, CHECK_SOLVE_INITIALS
      INTEGER*4  NUMARG, IVRB, N_ARG, IFSIZE, IYEAR, IC, IS, J1, J2, IUER
      INTEGER*2  IP_I2(5)
      TYPE ( OPA__STRU ) ::  OPA
      ADDRESS__TYPE :: DIR_DESC
#ifdef INTEL
      INTEGER*4, EXTERNAL :: IARGC
#endif
      INTEGER*4, EXTERNAL :: I_LEN, ILEN, MKDIR
      ADDRESS__TYPE, EXTERNAL :: OPENDIR
!
      INCLUDE 'opa_version.i' ! Set revision date of the current version
!
! --- Set RMPAR bits and names of some Solve directories
!
      CALL NOUT ( 10, IP_I2 )
      CALL SBIT ( IP_I2(1), INT2(1), INT2(0) )  ! no test version
      CALL SBIT ( IP_I2(2), INT2(1), INT2(1) )  ! spooling on
      CALL SBIT ( IP_I2(3), INT2(1), INT2(1) )  ! Batch
      CALL SBIT ( IP_I2(3), INT2(1), INT2(2) )  ! Minout on
      CALL UNPACK_RMPAR ( IP_I2 )
!
! --- Get numbers of command-string arguments
!
      NUMARG = IARGC ()
!
      CALL CLRCH ( DB_NAME     )
      CALL CLRCH ( HELP_FILE   )
      CALL CLRCH ( SOLVE_INIT  )
      CALL CLRCH ( CONFIG_FILE )
      CALL CLRCH ( SESS_CODE   )
      CALL CLRCH ( DIR_NAME    )
      CALL CLRCH ( OPC_FILE    )
      FL_GETMASTER = .FALSE.
      FL_NOCONFIRM = .FALSE.
      IVRB = 1
!
      CALL SET_SIGNAL_CTRLC ( 2 )
      IF ( NUMARG .EQ. 0 ) THEN
           WRITE ( 6, 110 )
 110       FORMAT ( 'Usage: opa [-h] -c <config_file> -d <database_name> ', &
     &              '-i <solve_initials> '/ &
     &              '           [-v <verbosity_level>] [-m] [-noconfirm]' )
           CALL EXIT ( 1 )
         ELSE
!
! -------- Set values of
! -------- a) SOLVE_HELP_DIR
! -------- We first check environment variables. If they are not set up we
! -------- get system-wide default
!
           CALL CLRCH  ( SOLVE_HELP_DIR_STR )
           CALL GETENVAR ( 'PSOLVE_HELP_DIR', SOLVE_HELP_DIR_STR )
           IF ( ILEN(SOLVE_HELP_DIR_STR) .LE. 0 ) THEN
                SOLVE_HELP_DIR_STR = SOLVE_HELP_DIR
           END IF
           IF ( SOLVE_HELP_DIR_STR(I_LEN(SOLVE_HELP_DIR_STR):I_LEN(SOLVE_HELP_DIR_STR)) .NE. '/' ) THEN
                SOLVE_HELP_DIR_STR = SOLVE_HELP_DIR_STR(1:I_LEN(SOLVE_HELP_DIR_STR))//'/'
           END IF
!
           DIR_DESC = OPENDIR ( SOLVE_HELP_DIR(1:I_LEN(SOLVE_HELP_DIR))//CHAR(0) )
           IF ( DIR_DESC .EQ. 0 ) THEN
                CALL ERR_LOG ( 4001, -2, 'OPA_HEAD', 'Wrong value of '// &
     &              'environment variable SOLVE_HELP_DIR (or system-wide default '// &
     &              'SOLVE_HELP_DIR: '//SOLVE_HELP_DIR(1:I_LEN(SOLVE_HELP_DIR))// &
     &              ' -- directory does not exist' )
                CALL REMOVE_SOLVE_LOCK()  
                CALL EXIT ( 2 )
              ELSE 
                CALL CLOSEDIR ( %VAL(DIR_DESC) )
           END IF
!
! -------- Get argeuments from the command line
!
           N_ARG = MIN(M_ARG,NUMARG)
           DO 410 J1=1,N_ARG
              CALL CLRCH  (     ARG_ARR(J1) )
              CALL GETARG ( J1, ARG_ARR(J1) )
!
              IF ( ARG_ARR(J1)(1:2) .EQ. '-h'  .OR. &
     &             ARG_ARR(J1)(1:2) .EQ. '-?'        ) THEN
!
! ---------------- Get help file name
!
                   CALL CLRCH ( HELP_FILE )
                   HELP_FILE = SOLVE_HELP_DIR(1:ILEN(SOLVE_HELP_DIR))//HELP01__OPA
!
! ---------------- Show help file
!
                   IUER = -1
                   CALL SHOW_TEXT_FILE_COL ( HELP_FILE, 'OPA', 0, IUER )
                   IF ( IUER .NE. 0 ) THEN
                        CALL ERR_LOG ( 4002, -2, 'OPA_HEAD', 'Error '// &
     &                      'in attempt to display help file' )
                        CALL REMOVE_SOLVE_LOCK()  
                        CALL EXIT ( 2 )
                   END IF
                   CALL REMOVE_SOLVE_LOCK()  
                   CALL EXIT ( 0 )
              END IF
 410       CONTINUE
      END IF
!
! --- Parse arguments from the command line
!
      IC=1
      DO 420 J2=1,M_PAIR
         IF ( IC .GT. N_ARG ) GOTO 820
         IF ( ARG_ARR(IC)(1:2) .EQ. '-c' ) THEN
!
! ----------- File name of the database is specified
!
              IF ( N_ARG .LT. IC+1 ) THEN
                   CALL ERR_LOG ( 4003, -2, 'OPA_HEAD', 'No arguments in '// &
     &                 'command line after -c')
                   CALL REMOVE_SOLVE_LOCK()  
                   CALL EXIT ( 2 )
                ELSE
                   CONFIG_FILE = ARG_ARR(IC+1)
                   IC = IC+2
              END IF
           ELSE IF ( ARG_ARR(IC)(1:2) .EQ. '-d' ) THEN
!
! ----------- Database name is specified
!
              IF ( N_ARG .LT. IC+1 ) THEN
                   CALL ERR_LOG ( 4004, -2, 'OPA_HEAD', 'No arguments in '// &
     &                 'command line after -d')
                   CALL REMOVE_SOLVE_LOCK()  
                   CALL EXIT ( 2 )
                ELSE
                   DB_NAME = ARG_ARR(IC+1)
                   CALL TRAN ( 11, DB_NAME, DB_NAME ) ! transfrom to upper regis
                   IC = IC+2
              END IF
           ELSE IF ( ARG_ARR(IC)(1:2) .EQ. '-i' ) THEN
!
! ----------- Solve user initials are specified
!
              IF ( N_ARG .LT. IC+1 ) THEN
                   CALL ERR_LOG ( 4005, -2, 'OPA_HEAD', 'No arguments in '// &
     &                 'command line after -i')
                   CALL REMOVE_SOLVE_LOCK()  
                   CALL EXIT ( 2 )
                ELSE
                   SOLVE_INIT = ARG_ARR(IC+1)
                   IC = IC+2
              END IF
           ELSE IF ( ARG_ARR(IC)(1:2) .EQ. '-v' ) THEN
!
! ----------- Verbosity level is specified
!
              IF ( N_ARG .LT. IC+1 ) THEN
                   CALL ERR_LOG ( 4006, -2, 'OPA_HEAD', 'No arguments in '// &
     &                 'command line after -v')
                   CALL REMOVE_SOLVE_LOCK()  
                   CALL EXIT ( 2 )
                ELSE
                   CALL CHIN ( ARG_ARR(IC+1), IVRB )
                   IF ( IVRB .LT. IVRB__MIN  .OR.  IVRB .GT. IVRB__MAX ) THEN
                        CALL ERR_LOG ( 4007, -2, 'OPA_HEAD', 'Wrong '// &
     &                      'value of -v argument: '// &
     &                       ARG_ARR(IC+1)(1:I_LEN(ARG_ARR(IC+1)))// &
     &                      '. An integer in the range '//CVRB_RANGE// &
     &                      ' was expected' )
                        CALL REMOVE_SOLVE_LOCK()  
                        CALL EXIT ( 2 )
                   END IF
                   IC = IC+2
              END IF
           ELSE IF ( ARG_ARR(IC)(1:2) .EQ. '-m' ) THEN
              FL_GETMASTER = .TRUE.
              IC = IC+1
           ELSE IF ( ARG_ARR(IC)(1:2) .EQ. '-n' ) THEN
              FL_NOCONFIRM = .TRUE.
              IC = IC+1
           ELSE
!
! ----------- Unrecognized argument
!
              CALL CLRCH ( STR )
              CALL INCH  ( IC, STR )
              CALL ERR_LOG ( 4008, -2, 'OPA_HEAD', 'Unrecognized '// &
     &             STR(1:I_LEN(STR))//'-th argument: '// &
     &             ARG_ARR(IC)(1:I_LEN(ARG_ARR(IC)))// &
     &            '. -c, -d, -i, -v -m were expected' )
              CALL REMOVE_SOLVE_LOCK()  
              CALL EXIT ( 2 )
         END IF
 420  CONTINUE
 820  CONTINUE
!
! --- Check whether Solve initials were defined
!
      IF ( ILEN(SOLVE_INIT) .EQ. 0 ) THEN
           CALL ERR_LOG ( 4009, -2, 'OPA_HEAD', 'Solve user initials were '// &
     &         'omitted in command line' )
           CALL REMOVE_SOLVE_LOCK()  
           CALL EXIT ( 9 )
      END IF
!
! --- Check whether solve user initials are OK
!
      CALL TRAN ( 11, SOLVE_INIT, SOLVE_INIT )
!@      IUER = -1
!@      LSUI = CHECK_SOLVE_INITIALS ( 'W', SOLVE_INIT(1:I_LEN(SOLVE_INIT)), IUER )
!@      IF ( .NOT. LSUI ) THEN
!@           CALL ERR_LOG ( 4010, -1, 'OPA_HEAD', 'Solve user initials '// &
!@     &          SOLVE_INIT(1:I_LEN(SOLVE_INIT))//' are wrong or in use' )
!@           CALL REMOVE_SOLVE_LOCK()  
!@           CALL EXIT ( 10 )
!@      END IF
!
      PRE_LETRS = SOLVE_INIT 
!
! --- Check, where Solve user initials are in use
!
      CALL CHECK_SOLVE_LOCK()
!
! --- Check whether configuration file was defined, if not get default
!
      IF ( ILEN(CONFIG_FILE) .EQ. 0 ) THEN
           CALL ERR_LOG ( 4011, -2, 'OPA_HEAD', 'argument with OPA '// &
     &         'configuration file is missed' )
           CALL REMOVE_SOLVE_LOCK()  
           CALL EXIT ( 11 )
      END IF
!
! --- ... and it exists
!
      INQUIRE ( FILE=CONFIG_FILE, EXIST=LEX )
      IF ( .NOT. LEX ) THEN
           CALL ERR_LOG ( 4012, -2, 'OPA_HEAD', 'Configuration file '// &
     &          CONFIG_FILE(1:I_LEN(CONFIG_FILE))//' was not found' )
           CALL REMOVE_SOLVE_LOCK()  
           CALL EXIT ( 12 )
      END IF
!
! --- Check whether database file is defined
!
      IF ( ILEN(DB_NAME) .EQ. 0 ) THEN
           CALL ERR_LOG ( 4013, -2, 'OPA_HEAD', 'Database name was omitted '// &
     &         'in command line' )
           CALL REMOVE_SOLVE_LOCK()  
           CALL EXIT ( 13 )
      END IF
!
! --- ... and has correct year field
!
      IF ( DB_NAME(1:1) .EQ. '$' ) DB_NAME = DB_NAME(2:)
      CALL CHIN ( DB_NAME(1:2), IYEAR )
      IF ( IYEAR .LT. 0  .OR.  IYEAR .GT. 99 ) THEN
           CALL ERR_LOG ( 4014, -2, 'OPA_HEAD', 'Wrong field of year in the '// &
     &         'database name '//DB_NAME )
           CALL REMOVE_SOLVE_LOCK()  
           CALL EXIT ( 14 )
      END IF
!
! --- Read configuration file
!
      IUER = -1
      CALL OPA_CONFIG ( CONFIG_FILE, OPA, IUER )
      IF ( IUER .NE. 0 ) THEN
           CALL ERR_LOG ( 4015, -2, 'OPA_HEAD', 'Error in attempt to set '// &
     &         'configuration for OPA' )
           CALL REMOVE_SOLVE_LOCK()  
           CALL EXIT ( 15 )
      END IF
!
! --- Set confirm flag
!
      IF ( FL_NOCONFIRM ) THEN
           OPA%FL_CONFIRM = .FALSE.
         ELSE
           OPA%FL_CONFIRM = .TRUE.
      END IF
!
      IF ( FL_GETMASTER ) THEN
!
! -------- Retrieve master fiels from IVS Data Center.
!
           IF ( IVRB .GE. 1 ) WRITE ( 6, '(A)' ) 'Retrieving master files... '
           IUER = -1
           CALL GET_MASTER ( OPA%WGET_EXE, OPA%URL_IVSCONTROL, OPA%MASTER_DIR, &
     &                       IUER )
           IF ( IUER .NE. 0 ) THEN
                CALL ERR_LOG ( 4016, -2, 'OPA_HEAD', 'Error in attempt to '// &
     &              'retrieve the set of master files from the IVS Data Center')
                CALL REMOVE_SOLVE_LOCK()  
                CALL EXIT ( 16 )
           END IF
      END IF
!
! --- Resolve database name and get session code SESS_CODE
!
      IUER = -1
      CALL RESOLVE_DBNAME ( OPA%MASTER_DIR, DB_NAME, SESS_CODE, IUER )
      IF ( IUER .NE. 0  ) THEN
           IF ( FL_GETMASTER ) THEN
                CALL ERR_LOG ( 4017, -2, 'OPA_HEAD', 'Database '// &
     &               DB_NAME(1:I_LEN(DB_NAME))//' was not found in the '// &
     &              'master file. Please, check database name' )
                CALL REMOVE_SOLVE_LOCK()  
                CALL EXIT ( 17 )
              ELSE
                CALL ERR_LOG ( 4018, -2, 'OPA_HEAD', 'Database '// &
     &               DB_NAME(1:I_LEN(DB_NAME))//' was not found in the '// &
     &              'master file. Try to update your local copy of master '// &
     &              'files and browse them from the IVS Data Center '// &
     &              'by calling opa once more with using option -m' )
                CALL REMOVE_SOLVE_LOCK()  
                CALL EXIT ( 18 )
           END IF
      END IF
!
! --- Transform session code to the lstters of lower register
!
      CALL TRAN ( 12, SESS_CODE, SESS_CODE )
!
      IF ( IYEAR .GT. 70 ) THEN
           DIR_NAME = OPA%SESSION_DIR(1:I_LEN(OPA%SESSION_DIR))//'19'// &
     &                DB_NAME(1:2)//'/'//SESS_CODE(1:I_LEN(SESS_CODE))//'/'
         ELSE
           DIR_NAME = OPA%SESSION_DIR(1:I_LEN(OPA%SESSION_DIR))//'20'// &
     &                DB_NAME(1:2)//'/'//SESS_CODE(1:I_LEN(SESS_CODE))//'/'
      END IF
!
! --- Resolve database file name
!
      IF ( IVRB .GE. 1 ) WRITE ( 6, '(A)' ) 'Resolving database file name... '
      IUER = -1
!@      CALL GET_DBNAME_FILE ( DB_NAME, DBFILE, IFSIZE, IUER )
!@      IF ( IUER .NE. 0 ) THEN
!@           CALL ERR_LOG ( 4019, -2, 'OPA_HEAD', 'Database '//DB_NAME// &
!@     &         'was not found in the catalogue system' )
!@           CALL REMOVE_SOLVE_LOCK()  
!@           CALL EXIT ( 19 )
!@#      END IF
      IF ( ILEN(DBFILE) .LE. 4 ) THEN
           CALL ERR_LOG ( 4020, -2, 'OPA_HEAD', 'Database file name '// &
     &         'is too short '//DBFILE )
           CALL REMOVE_SOLVE_LOCK()  
           CALL EXIT ( 20 )
      END IF
!
! --- Decode database version number
!
      OUT = DBFILE(ILEN(DBFILE)-2:ILEN(DBFILE))
      IF ( OUT(1:1) .EQ. '0' ) OUT=OUT(2:)
      IF ( OUT(1:1) .EQ. '0' ) OUT=OUT(2:)
      IF ( OUT(1:1) .EQ. '0' ) OUT=OUT(2:)
      CALL CHIN ( OUT, OPA%DB_VERSION )
      IF ( OPA%DB_VERSION .LT. 0  .OR.  OPA%DB_VERSION .GT. 999 ) THEN
           CALL ERR_LOG ( 4021, -2, 'OPA_HEAD', 'Database file name '// &
     &          DBFILE(1:I_LEN(DBFILE))//' which corresponds to the last '// &
     &          'version of database '//DB_NAME//' contains wrong version '// &
     &          'number' )
           CALL REMOVE_SOLVE_LOCK()  
           CALL EXIT ( 21 )
      END IF
      IF ( IVRB .GE. 1 ) WRITE ( 6, '(A)' ) 'Done '
!
! --- Does directory name for this experiment exist?
!
      DIR_DESC = OPENDIR ( DIR_NAME(1:I_LEN(DIR_NAME))//CHAR(0) )
      IF ( DIR_DESC == 0 ) THEN
!
! -------- Create directory
!
           IS = MKDIR ( DIR_NAME(1:I_LEN(DIR_NAME))//CHAR(0), %VAL(MASK) )
           IF ( IS .NE. 0 ) THEN
                CALL GERROR ( STR )
                CALL ERR_LOG ( 4022, -2, 'OPA_HEAD', 'Directory '// &
     &               DIR_NAME(1:I_LEN(DIR_NAME))//' for experiment for '// &
     &              'database '//DB_NAME(1:I_LEN(DB_NAME))// &
     &              ' does not exist. Attempt to create it failed: '// &
     &              STR )
                CALL REMOVE_SOLVE_LOCK()  
                CALL EXIT ( 22 )
           END IF
         ELSE
           CALL CLOSEDIR ( %VAL(DIR_DESC) )
      END IF
!
! --- Build the file name of the operational control file
!
      OPC_FILE = DIR_NAME(1:I_LEN(DIR_NAME))//SESS_CODE(1:I_LEN(SESS_CODE))// &
     &           '.opc'
!
! --- Does this file exist?
!
      INQUIRE ( FILE=OPC_FILE, EXIST=LEX )
      IF ( .NOT. LEX ) THEN
           IUER = -1
           CALL CREATE_OPC ( OPC_FILE, DB_NAME, SESS_CODE, IUER )
           IF ( IUER .NE. 0 ) THEN
                CALL ERR_LOG ( 4023, -2, 'OPA_HEAD', 'Error in attempt '// &
     &              'to create a new file of operational control for '// &
     &              'database '//DB_NAME(1:I_LEN(DB_NAME))//' -- '//OPC_FILE )
                CALL REMOVE_SOLVE_LOCK()  
                CALL EXIT ( 23 )
           END IF
      END IF
!
! --- Read operational control file
!
      IUER = -1
      CALL READ_OPC ( OPC_FILE, OPA, DB_NAME, SESS_CODE, IUER )
      IF ( IUER .NE. 0 ) THEN
           CALL ERR_LOG ( 4024, -2, 'OPA_HEAD', 'Error in attempt to read '// &
     &         'Operational control file for session '// &
     &          SESS_CODE(1:I_LEN(SESS_CODE))//' -- '//OPC_FILE )
           CALL REMOVE_SOLVE_LOCK()  
           CALL EXIT ( 24 )
      END IF
!
! --- Calling menu
!
 910  CONTINUE
      IUER = -1
      CALL OPA_MENU ( IVRB, SOLVE_INIT, OPC_FILE, OPA, IUER )
      IF ( IUER .NE. 0 ) THEN
           CALL ERR_LOG ( 4025, -2, 'OPA_HEAD', 'Error in an attempt to '// &
     &         'invoke OPA menu :-(' )
           CALL REMOVE_SOLVE_LOCK()  
           CALL EXIT ( 25 )
      END IF
      IF ( OPA%IACT .NE. OPA__UND ) THEN
!
! -------- Making an action
!
           IUER = -1
           CALL OPA_ACTION ( IVRB, SOLVE_INIT, OPA, OPC_FILE, IUER )
           IF ( IUER .NE. 0 ) THEN
                CALL ERR_LOG ( 4026, -2, 'OPA_HEAD', 'Error in an attempt '// &
     &              'to execute OPA action :-(' )
                CALL REMOVE_SOLVE_LOCK()  
                CALL EXIT ( 26 )
           END IF
           IF ( OPA%IACT .NE. OPA__ALL ) THEN
                CALL HIT_CONT ( %VAL(0), %VAL(0) )
                GOTO 910
           END IF
      END IF
!
! --- Remove lock from Solve user initiaqls
!
      CALL REMOVE_SOLVE_LOCK()  
!
      END  !#!  OPA_HEAD  #!#
