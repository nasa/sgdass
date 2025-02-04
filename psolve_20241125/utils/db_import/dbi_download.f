      SUBROUTINE DBI_DOWNLOAD ( DBI, L_DBS, C_DBS, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  DBI_DOWNLOAD  downloads the set of database files from    *
! *   the IVS Data Center to the incoming directory DBI.INCOMING_DIR .   *
! *   The list of the database URLs which are to be retrieved is taken   *
! *   from the file DBI.URL_FILE . Program wget is used for remote files *
! *   downloading. If the files have extension .gz they are uncompressed *
! *   by gzip. Entries to the file DBI.LOG_FILE are added.               *
! *                                                                      *
! * ________________________ Input parameters: _________________________ *
! *                                                                      *
! *    DBI ( RECORD    ) -- Object with data structure for keeping       *
! *                         configuration of DB_IMPORT utility.          *
! *  L_DBS ( INTEGER*4 ) -- Numbers of database file names.              *
! *  C_DBS ( CHARACTER ) -- Database filename list. Dimension: L_DBS.    *
! *                         NB: L_DBS, C_DBS are used for generating     *
! *                         messages and for uncompressing reterieved    *
! *                         files. URLs of the files to be downloaded    *
! *                         is taken from the fiel DBI.URL_FILE .        *
! *                                                                      *
! * ________________________ Output parameters: ________________________ *
! *                                                                      *
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
! *  ### 18-OCT-2000   DBI_DOWNLOAD  v1.1 (c) L. Petrov 27-APR-2001 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'db_import.i'
      TYPE ( DBI__STRU ) ::  DBI
      INTEGER*4  L_DBS, IUER
      CHARACTER  C_DBS(L_DBS)*(*)
      CHARACTER  REMOTE_COMSTR*256, STR*80, DB_NAME*32
      CHARACTER  GET_CDATE*19
      INTEGER*4  IS, ISIG, ICOD, LUN, IO, IG, J1
      INTEGER*4  SYSTEM, I_LEN, GET_UNIT
!
! --- Open file of DB_IMPOIRT log
!
      LUN = GET_UNIT () ! Get any free fortran I/O logical unit
      OPEN ( UNIT=LUN, FILE=DBI%LOG_FILE, STATUS='UNKNOWN', ACCESS='APPEND', &
     &       IOSTAT=IO )
      IF ( IO .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL INCH   ( IO, STR )
           CALL ERR_LOG ( 5771, IUER, 'DBI_DOWNLOAD', 'Error in attempt '// &
     &         'to open file '//DBI%LOG_FILE(1:I_LEN(DBI%LOG_FILE))// &
     &         ' IO='//STR )
           RETURN
      END IF
!
! --- Write information messages in the log about starting database file
! --- retrieving
!
      WRITE ( LUN, '(A,I6,A)', IOSTAT=IO ) 'DB_IMPORT: Started downloading ', &
     &                                      L_DBS, ' files at '//GET_CDATE()
      WRITE ( LUN, '(A)' ) '~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~'// &
     &                     '~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~'
      CLOSE ( UNIT=LUN )
!
! --- Build the command for launching wget
!
      CALL CLRCH ( REMOTE_COMSTR )
      REMOTE_COMSTR = 'cd '// &
     &                DBI%INCOMING_DIR(1:I_LEN(DBI%INCOMING_DIR))//'; '// &
     &                DBI%WGET_EXE(1:I_LEN(DBI%WGET_EXE))//' -c -nv -i '// &
     &                DBI%URL_FILE(1:I_LEN(DBI%URL_FILE))//' -a '// &
     &                DBI%LOG_FILE(1:I_LEN(DBI%LOG_FILE))
!
! --- Launch wget for file retrieving and wait
!
      IS = SYSTEM ( REMOTE_COMSTR(1:I_LEN(REMOTE_COMSTR))//CHAR(0) )
      IF ( IS .NE. 0 ) THEN
           ISIG = 0
           ICOD = 0
           CALL MVBITS ( IS, 0, 8, ISIG, 0 )
           CALL MVBITS ( IS, 8, 8, ICOD, 0 )
           IF ( ICOD .GE. 128 ) ICOD = ICOD-256
           CALL CLRCH ( STR )
           CALL INCH  ( ICOD, STR )
           CALL ERR_LOG ( 5772, IUER, 'DBI_DOWNLOAD', 'Error in '// &
     &            'attempt to execute a command "'// &
     &             REMOTE_COMSTR(1:I_LEN(REMOTE_COMSTR))// &
     &            '" for database(s) downloading ICOD='//STR )
           RETURN
      END IF
!
! --- Remove index.html file which we don't need
!
      CALL UNLINK ( DBI%INCOMING_DIR(1:I_LEN(DBI%INCOMING_DIR))//'index.html'// &
     &              CHAR(0) )
!
! --- Ura! We retrieved all files. Now scan filenames and if we find files with
! --- extension .gz then we uncompress it
!
      DO 410 J1=1,L_DBS
!
! ------ Check extension
!
         IG = INDEX ( C_DBS(J1), '.gz' )
         IF ( IG .LE. 0 ) GOTO 410 ! Extension is not .gz -- nothing to do
         DB_NAME = C_DBS(J1)
         IF( DB_NAME(9:9) .EQ. ' ' ) DB_NAME(9:9) = '_'
!
! ------ Build the command for uncomressing and mode changes
!
         CALL CLRCH ( REMOTE_COMSTR )
         REMOTE_COMSTR = 'chmod 664 '// &
     &                          DBI%INCOMING_DIR(1:I_LEN(DBI%INCOMING_DIR))// &
     &                          DB_NAME(1:I_LEN(DB_NAME))//'; '// &
     &                    DBI%GZIP_EXE(1:I_LEN(DBI%GZIP_EXE))//' -d '// &
     &                          DBI%INCOMING_DIR(1:I_LEN(DBI%INCOMING_DIR))// &
     &                          DB_NAME(1:I_LEN(DB_NAME))
!
! ------ Launching gzip for uncompression and waiting for results
!
         IS = SYSTEM ( REMOTE_COMSTR(1:I_LEN(REMOTE_COMSTR))//CHAR(0) )
         IF ( IS .NE. 0 ) THEN
              ISIG = 0
              ICOD = 0
              CALL MVBITS ( IS, 0, 8, ISIG, 0 )
              CALL MVBITS ( IS, 8, 8, ICOD, 0 )
              IF ( ICOD .GE. 128 ) ICOD = ICOD-256
              CALL CLRCH ( STR )
              CALL INCH  ( ICOD, STR )
              CALL ERR_LOG ( 5773, IUER, 'DBI_DOWNLOAD', 'Error in '// &
     &            'attempt to uncompress database file: '// &
     &             REMOTE_COMSTR(1:I_LEN(REMOTE_COMSTR))// &
     &            ' Error code ='//STR )
              RETURN
         END IF
 410  CONTINUE
!
! --- Open DB_IMPORT log-file again
!
      LUN = GET_UNIT ()
      OPEN ( UNIT=LUN, FILE=DBI%LOG_FILE, STATUS='UNKNOWN', ACCESS='APPEND', &
     &       IOSTAT=IO )
      IF ( IO .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL INCH   ( IO, STR )
           CALL ERR_LOG ( 5774, IUER, 'DBI_DOWNLOAD', 'Error in attempt '// &
     &         'to open file '//DBI%LOG_FILE(1:I_LEN(DBI%LOG_FILE))// &
     &         ' IO='//STR )
           RETURN
      END IF
!
! --- Add there informational messages
!
      WRITE ( LUN, '(A)' ) '~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~'// &
     &                     '~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~'
      WRITE ( LUN, '(A,I6,A)', IOSTAT=IO ) 'DB_IMPORT: Completed downloading ', &
     &                                      L_DBS, ' files at '//GET_CDATE()
      WRITE ( LUN, '(A)' ) ' '
      CLOSE ( UNIT=LUN )
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  DBI_DOWNLOAD  #!#
