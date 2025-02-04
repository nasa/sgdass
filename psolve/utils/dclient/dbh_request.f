      SUBROUTINE DBH_REQUEST ( DCLIENT, DBNAME, MESSAGE_FILE, COMMAND_FILE, &
     &                         FINAM1, FINAM2, FL_SLNT, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  DBH_REQUEST  parses a request to transfer database(s)     *
! *   from the current host (Operating Center) to a dserver host         *
! *   (Data Center). It checks validity of the argument DBNAME,          *
! *   determines which mode this argument corresponds to, forms a name   *
! *   of temporary files MESSAGE_FILE, COMMAND_FILE, resolve full        *
! *   name(s) with path which correspond to a database_name DBNAME.      *
! *   In the case when only one file corresponds to DBNAME, then FINAM2  *
! *   is an empty string.                                                *
! *                                                                      *
! * ________________________ Input parameters: _________________________ *
! *                                                                      *
! *      DCLIENT ( RECORD    ) -- Object with data structure for keeping *
! *                               configuration of dclient.              *
! *       DBNAME ( CHARACTER ) -- database_name argument. May be one of  *
! *                               a) database filename with a full path; *
! *                               b) database filename of the specific   *
! *                                  version without path.               *
! *                               c) database name without version       *
! *                                  number and path. It corresponds to  *
! *                                  a X/S pair of databases.            *
! *      FL_SLNT ( LOGICAL*4 ) -- Silent mode flag. If .TRUE. then no    *
! *                               information messages are printed on    *
! *                               the screen.                            *
! *                                                                      *
! * ________________________ Output parameters: ________________________ *
! *                                                                      *
! * MESSAGE_FILE ( CHARACTER ) -- File name with a message to be sent to *
! *                               dserver.                               *
! * COMMAND_FILE ( CHARACTER ) -- File name with a C-Shell program for   *
! *                               processing a request for a database    *
! *                               submission.                            *
! *       FINAM1 ( CHARACTER ) -- Full path name which corresponds to    *
! *                               the database.                          *
! *       FINAM2 ( CHARACTER ) -- Full path name which corresponds to    *
! *                               the database at the opposite band.     *
! *                               It is an empty line in modes "a" and   *
! *                               "b".                                   *
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
! *  ### 30-SEP-99  DBH_REQUEST   v1.0 (c)  L. Petrov  10-JAN-2001  ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'dclient.i'
      TYPE ( DCLIENT__STRU ) ::  DCLIENT
      CHARACTER  DBNAME*(*), MESSAGE_FILE*(*), COMMAND_FILE*(*), &
     &           FINAM1*(*),  FINAM2*(*)
      INTEGER*4  IUER
      LOGICAL*4  FL_SLNT
      CHARACTER  USER_NAME*80, USER_REALNAME*80, USER_E_ADDRESS*256, &
     &           PID_STR*5
      INTEGER*4  PID, ISIZ1, ISIZ2, IER
      INTEGER*4, EXTERNAL :: ILEN, I_LEN, GETPID
!
! --- Initialization
!
      CALL CLRCH ( MESSAGE_FILE )
      CALL CLRCH ( COMMAND_FILE )
      CALL CLRCH ( FINAM1       )
      CALL CLRCH ( FINAM2       )
      ISIZ1 = 0
      ISIZ2 = 0
!
! --- Add a dollar sign in order to help boosting American economy
!
      IF ( DBNAME(1:1) .EQ. '$' ) DBNAME = DBNAME(2:)
!
! --- Get user identifier
!
      CALL GETINFO_USER ( USER_NAME, USER_REALNAME, USER_E_ADDRESS )
!
! --- Get PID (Process IDentifier)
!
      PID = GETPID()
!
! --- Transform PID to a text string
!
      CALL CLRCH  ( PID_STR )
      CALL INCH   ( PID,   PID_STR )
      CALL CHASHR (        PID_STR )
      CALL BLANK_TO_ZERO ( PID_STR )
!
! --- Form a message file names. They contain PID in order to avoid
! --- conflicts with simultaneous work of two or more users
!
      MESSAGE_FILE = DCLIENT%TMP_DIR(1:I_LEN(DCLIENT%TMP_DIR))//'dclient_'// &
     &               PID_STR(1:I_LEN(PID_STR))//'.mes'
      COMMAND_FILE = DCLIENT%TMP_DIR(1:I_LEN(DCLIENT%TMP_DIR))//'dclient_'// &
     &               PID_STR(1:I_LEN(PID_STR))//'.cnt'
!
! --- Determine the mode of DBNAME:
!
      IF ( DBNAME(1:1) .EQ. '/' ) THEN
!
! -------- a) absolute path
!
           FINAM1 = DBNAME
         ELSE IF ( ILEN(DBNAME) .EQ. 14 ) THEN
           IF ( .NOT. FL_SLNT ) WRITE ( 6, '(A)' ) 'Resolving filename for '// &
     &                          'database '//DBNAME(1:I_LEN(DBNAME))//' ...'
!
! -------- b) database file name with version. Try to resolve a full file name
! -------- by asling catalogue system.
!
           CALL ERR_PASS ( IUER, IER )
           CALL GET_DBNAME_FILE ( DBNAME, FINAM1, ISIZ1, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 5141, IUER, 'DBH_REQUEST', 'Error in '// &
     &              'attempt to get a file name from the geo-VLBI database '// &
     &              'catalogue for the database '//DBNAME )
                RETURN
           END IF
         ELSE
!
! -------- c) database name without version. Try to Resolve a filename
! -------- for the last version by asking catalogue system
!
           IF ( .NOT. FL_SLNT ) WRITE ( 6, '(A)' ) 'Resolving filename for '// &
     &                          'database '//DBNAME(1:10)//' ...'
           CALL ERR_PASS ( IUER, IER )
           CALL GET_DBNAME_FILE ( DBNAME(1:10), FINAM1, ISIZ1, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 5142, IUER, 'DBH_REQUEST', 'Error in '// &
     &              'attempt to get a file name from the geo-VLBI database '// &
     &              'catalogue for the database '//DBNAME )
                RETURN
           END IF
!
! -------- Try to resolve a filename for the last version of the opposite
! -------- version by asking catalogue system
!
           IF ( DBNAME(8:8) .EQ. 'X' ) THEN
               IF ( .NOT. FL_SLNT ) WRITE ( 6, '(A)' ) 'Resolving filename '// &
     &              'for database '//DBNAME(1:7)//'S'//DBNAME(9:10)//' ...'
                CALL ERR_PASS ( IUER, IER )
                CALL GET_DBNAME_FILE ( DBNAME(1:7)//'S'//DBNAME(9:10), FINAM2, &
     &                                 ISIZ2, IER )
                IF ( IER .NE. 0 ) THEN
                     CALL ERR_LOG ( 5143, IUER, 'DBH_REQUEST', 'Error in '// &
     &                   'attempt to get a file name from the geo-VLBI '// &
     &                   'database catalogue for the database '// &
     &                    DBNAME(1:7)//'X'//DBNAME(9:) )
                     RETURN
                END IF
              ELSE IF ( DBNAME(8:8) .EQ. 'S' ) THEN
               IF ( .NOT. FL_SLNT ) WRITE ( 6, '(A)' ) 'Resolving filename '// &
     &              'for database '//DBNAME(1:7)//'X'//DBNAME(9:10)//' ...'
                CALL ERR_PASS ( IUER, IER )
                CALL GET_DBNAME_FILE ( DBNAME(1:7)//'X'//DBNAME(9:10), FINAM2, &
     &                                 ISIZ2, IER )
                IF ( IER .NE. 0 ) THEN
                     CALL ERR_LOG ( 5144, IUER, 'DBH_REQUEST', 'Error in '// &
     &                   'attempt to get a file name from the geo-VLBI '// &
     &                   'database catalogue for the database '// &
     &                    DBNAME(1:7)//'X'//DBNAME(9:) )
                     RETURN
                END IF
           END IF
      END IF
!
!      type *,' pid_str >>',pid_str,'<<'
!      type *,' message_file >>',message_file(1:i_len(message_file)),'<<'
!      type *,' command_file >>',command_file(1:i_len(command_file)),'<<'
!      type *,' finam1 >>',finam1(1:i_len(finam1)),'<<   isiz1=',isiz1
!      type *,' finam2 >>',finam2(1:i_len(finam2)),'<<   isiz2=',isiz2
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END   !#!  DBH_REQUEST  #!#
