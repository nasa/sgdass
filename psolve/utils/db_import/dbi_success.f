      SUBROUTINE DBI_SUCCESS ( DBI, L_DBS, C_DBS, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  DBI_SUCCESS  removes temporary files and sends a message  *
! *   to user(s) about successful retrieving L_DBS database filenames.   *
! *                                                                      *
! * ________________________ Input parameters: _________________________ *
! *                                                                      *
! *   DBI ( RECORD    ) -- Object with data structure for keeping        *
! *                        configuration of DB_IMPORT utility.           *
! * L_DBS ( INTEGER*4 ) -- Numbers of database file names.               *
! * C_DBS ( CHARACTER ) -- Database filename list. Dimension: L_DBS.     *
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
! *  ### 18-OCT-2000    DBI_SUCCESS  v1.0 (c) L. Petrov 18-OCT-2000 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'solve.i'
      INCLUDE   'db_import.i'
      TYPE ( DBI__STRU ) ::  DBI
      INTEGER*4  L_DBS, IUER
      CHARACTER  C_DBS(L_DBS)*(*)
      CHARACTER  STR*80, PID_STR*5, MAIL_FILE*128, SENDSTR*256, SUBJECT*128
      CHARACTER  GET_CDATE*19, GET_VERSION*54
      INTEGER*4  IS, ISIG, ICOD, LUN, IO, PID, IG, J1
      INTEGER*4  SYSTEM, I_LEN, GET_UNIT, GETPID
!
      CALL UNLINK ( DBI%URL_FILE(1:I_LEN(DBI%URL_FILE))//CHAR(0) )
      IF ( DBI%EMAIL_IMPORT(1:2) .EQ. 'NO' ) THEN
!
! -------- No mail should be sent
!
           CALL ERR_LOG ( 0, IUER )
           RETURN
      END IF
!
      PID = GETPID () ! get process identification of the current process
      CALL CLRCH   (       PID_STR )
      CALL INCH    ( PID,  PID_STR )
      CALL CHASHR  (       PID_STR )
      CALL BLANK_TO_ZERO ( PID_STR )
!
! --- Build the temporary filename for the mail message
!
      CALL CLRCH ( MAIL_FILE )
      MAIL_FILE = DBI%TMP_DIR(1:I_LEN(DBI%TMP_DIR))//'dbi_'//PID_STR//'.mai'
!
! --- Open the file with the body of e-mail message
!
      LUN = GET_UNIT ()
      OPEN ( UNIT=LUN, FILE=MAIL_FILE, STATUS='UNKNOWN', IOSTAT=IO )
      IF ( IO .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL INCH   ( IO, STR )
           CALL ERR_LOG ( 5781, IUER, 'DBI_SUCCESS', 'Error in attempt '// &
     &         'to open file '//MAIL_FILE(1:I_LEN(MAIL_FILE))// &
     &         ' IO='//STR )
           RETURN
      END IF
!
! --- Create the body of the message
!
      WRITE ( LUN, '(A)', IOSTAT=IO ) 'Dear VLBI-friend at '// &
     &                                 CENTER_FULL_NAME//':'
      WRITE ( LUN, '(A)', IOSTAT=IO ) ' '
      WRITE ( LUN, '(A)', IOSTAT=IO ) '    New VLBI data appeared at the '// &
     &       'IVS Data Center!'
      WRITE ( LUN, '(I5,A)', IOSTAT=IO ) L_DBS,' database files '// &
     &       'were successfully downloaded in the incoming directory '
      WRITE ( LUN, '(A)', IOSTAT=IO) DBI%INCOMING_DIR(1:I_LEN(DBI%INCOMING_DIR))
      WRITE ( LUN, '(A)', IOSTAT=IO ) ' '
!
! --- Enumerate the databasee filename list
!
      DO 410 J1=1,L_DBS
         IG = INDEX ( C_DBS(J1), '.gz' ) - 1
         IF ( IG .LE. 0 ) IG = I_LEN(C_DBS(J1))
         WRITE ( LUN, '(I6,A)', IOSTAT=IO ) J1,') '//C_DBS(J1)(1:IG)
 410  CONTINUE
!
! --- Attached the name of the program, version and the current date
!
      WRITE ( LUN, '(A)', IOSTAT=IO ) ' '
      WRITE ( LUN, '(A)', IOSTAT=IO ) GET_VERSION()
      WRITE ( LUN, '(A)', IOSTAT=IO ) GET_CDATE()
      CLOSE ( UNIT=LUN )
!
! --- Generate a subject of the e-mail message
!
      SUBJECT = 'NEW DATABASES ARRIVED (automatically generated message)'
!
! --- Build the line for invoking mailer
!
      SENDSTR = DBI%MAIL_COMMAND(1:I_LEN(DBI%MAIL_COMMAND))// &
     &          ' -s "'//SUBJECT(1:I_LEN(SUBJECT))//'" '// &
     &          DBI%EMAIL_IMPORT(1:I_LEN(DBI%EMAIL_IMPORT))// &
     &          ' < '//MAIL_FILE
!
! --- Execute a command: sending e-mail message to the user(s)
!
      IS = SYSTEM ( SENDSTR(1:I_LEN(SENDSTR))//CHAR(0) )
      IF ( IS .NE. 0 ) THEN
           ISIG = 0
           ICOD = 0
           CALL MVBITS ( IS, 0, 8, ISIG, 0 )
           CALL MVBITS ( IS, 8, 8, ICOD, 0 )
           IF ( ICOD .GE. 128 ) ICOD = ICOD-256
           CALL CLRCH ( STR )
           CALL INCH  ( ICOD, STR )
           CALL ERR_LOG ( 5782, IUER, 'DBI_SUCCESS', 'Error in '// &
     &         'attempt to send an e-mail: to execute command '// &
     &          SENDSTR(1:I_LEN(SENDSTR))//' Error code ='//STR )
           RETURN
      END IF
!
! --- Remove mail file
!
      CALL UNLINK ( MAIL_FILE(1:I_LEN(MAIL_FILE))//CHAR(0) )
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  DBI_SUCCESS  #!#
