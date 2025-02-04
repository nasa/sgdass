      SUBROUTINE GET_DBNAME_FILE ( DBNAME, DBFILE, IFSIZE, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  GET_DBNAME_FILE  asks catalogue system to resolve         *
! *   a database name DBNAME and to provide a full filename with path.   *
! *   If the database name DBNAME contains version number then this      *
! *   information about this specific version of the database is         *
! *   retrieved. Filesize in bytes IFSIZE is also returned.              *
! *                                                                      *
! * ________________________ Input parameters: _________________________ *
! *                                                                      *
! *  DBNAME ( CHARACTER ) -- database_name argument. May be one of       *
! *                          1) database filename of a specific version; *
! *                          2) database name without version number.    *
! *                             The last version is assumed.             *
! *                                                                      *
! *                          Dollar sign should NOT precede the database *
! *                          name!                                       *
! *                                                                      *
! * ________________________ Output parameters: ________________________ *
! *                                                                      *
! *  DBFILE ( CHARACTER ) -- filename with path which corresponds to     *
! *                          a database DBNAME.                          *
! *   ISIZE ( INTEGER*4 ) -- size in bytes of the file DBFILE.           *
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
! *  ###  30-SEP-99  GET_DBNAME_FILE v1.1 (c)  L. Petrov 21-JAN-2002 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'catcm.i'
      CHARACTER  DBNAME*(*), DBFILE*(*)
      INTEGER*4  IFSIZE, IUER
!
      INTEGER*2  MENT
      PARAMETER  ( MENT=32767 )
      INTEGER*2  INT2_ARG, LDISP(40), IBIN(2), IHIGH(2), IASC(2), ICR, &
     &           LCIFILE(7), IDATE(5), ITIME(5), IHIGHOLD(2), LFMGR(3), &
     &           ID_AREA, IFLOP, IPEND, IVER, ISEC, IMCH, XDBL, KERR, KERR_ANY
      CHARACTER  ENTRY_NAME*10, HUNG*10, DESC*256, FPATH*157, STR*32
      LOGICAL*4  LEX
      INTEGER*4  J1, IS, IP, IVER_REQ, STAT_BLOCK(12)
      INTEGER*4, EXTERNAL :: I_LEN, FOR_STAT
!
      CALL CLRCH ( DBFILE )
!
! --- Openning MK3 catalogue
!
      CALL OPEN_MK3_CAT ( %REF('XB'), KERR )
      IF ( KERR .NE. 0 ) THEN
           WRITE ( 6, * ) ' OPEN_MK3_CAT: kerr=',kerr
           CALL ERR_LOG ( 5151, IUER, 'GET_DBNAME_FILE', 'Failure in '// &
     &         'openning of ther catalogue' )
           RETURN
      END IF
!
! --- Form the entry name for catalogue inquiry
!
! --- Add a dollar sign in order as a processing fee.
!
      IF ( DBNAME(1:1) .NE. '$' ) ENTRY_NAME = '$'//DBNAME(1:9)
      IF ( ENTRY_NAME(10:10) .EQ. '_' ) ENTRY_NAME(10:10) = ' '
!
      IF ( INDEX ( DBNAME, '_V' ) .GT. 0 ) THEN
!
! -------- Version was explicitely specified
!
           IP = INDEX ( DBNAME, '_V' )
           CALL CHIN ( DBNAME(IP+2:), IVER_REQ )
!
! -------- Access to the first available version of the database
!
           CALL RUN_ANY_KEY ( %REF(ENTRY_NAME), INT2(1), %REF(HUNG), IVER, &
     &                        %REF(DESC), KERR_ANY )
           IF ( IVER .EQ. INT2(IVER_REQ) ) GOTO 810 ! OK, we sought it
           IF ( KERR_ANY .LT. INT2(0)    ) GOTO 810 ! Error
!
! -------- The first available version of the database is not the requested
!
           IF ( KERR_ANY .NE. INT2(2) .AND. IVER_REQ .NE. IVER ) THEN
!
! ------------- Look at the sequence of the next versions
!
                DO 410 J1=1,1024
!
! ---------------- Access to the next version
!
                   CALL RUN_ANY_KEY ( %REF(ENTRY_NAME), INT2(0), %REF(HUNG), &
     &                                IVER, %REF(DESC), KERR_ANY )
                   IF ( KERR_ANY .LT. INT2(0)    ) GOTO 810 ! Error
                   IF ( IVER .EQ. INT2(IVER_REQ) ) GOTO 810 ! OK, we sought it
!
                   IF ( KERR_ANY .EQ. INT2(2) ) THEN
!
! --------------------- It turned out the last verrsion
!
                        CALL CLRCH ( STR )
                        CALL INCH  ( INT4(IVER), STR )
                        CALL ERR_LOG ( 5152, IUER, 'GET_DBNAME_FILE', &
     &                                'Spesific version: '// &
     &                                 DBNAME(1:I_LEN(DBNAME))//' is not '// &
     &                                'found in the catalogue. The last '// &
     &                                'version is '//STR )
                        CALL CLOSE_CAT()
                        RETURN
                   END IF
 410            CONTINUE
           END IF
         ELSE
!
! -------- The last version has been requested
!
           CALL RUN_ANY_KEY ( %REF(ENTRY_NAME), INT2(2), %REF(HUNG), IVER, &
     &                        %REF(DESC), KERR_ANY )
      END IF
!
 810  CONTINUE
!
      IF ( KERR_ANY .NE. INT2(0)  .AND. &         ! 0 - normal return
     &     KERR_ANY .NE. INT2(1)  .AND. &         ! 1 - first entry
     &     KERR_ANY .NE. INT2(2)       ) THEN  ! 2 - last entry
!
           CALL ERR_LOG ( 5153, IUER, 'GET_DBNAME_FILE', 'Experiment '// &
     &          ENTRY_NAME//' has not been found in the catalogue' )
           CALL CLOSE_CAT()
           RETURN
      END IF
!
!---- Get information about the specific version of the database
!
      CALL GET_EXPERIMENT_INFO ( %REF(ENTRY_NAME), LDISP, ID_AREA, IBIN, &
     &     IHIGH, IASC, LFMGR, ISEC, LCIFILE, IFLOP, IPEND, IVER, &
     &     XDBL, IDATE, ITIME, IHIGHOLD, IMCH, KERR )
      IF ( KERR .NE. INT2(0) ) THEN
           CALL ERR_LOG ( 5154, IUER, 'GET_DBNAME_FILE', 'Error in getting '// &
     &         'experiment info for entry '//ENTRY_NAME )
           CALL CLOSE_CAT()
           RETURN
      END IF
!
! --- Get file name of the specific version of the database
!
      CALL NAME_TO_PATH ( %REF(ENTRY_NAME), IVER, LCIFILE, ICR, FPATH, KERR )
      DBFILE = FPATH
      IF ( KERR .EQ. INT2(-3) ) THEN
           CALL ERR_LOG ( 5155, IUER, 'GET_DBNAME_FILE', 'This specific '// &
     &         'database version '//DBNAME(1:I_LEN(DBNAME))//' was not on '// &
     &         'disk ("active")' )
           CALL CLOSE_CAT()
           RETURN
         ELSE IF ( KERR .NE. INT2(0) ) THEN
           WRITE ( 6, * ) ' kerr=',kerr
           CALL ERR_LOG ( 5156, IUER, 'GET_DBNAME_FILE', 'Error in getting '// &
     &         'database file name '//DBNAME )
           CALL CLOSE_CAT()
           RETURN
      END IF
!
! --- Now close the catalogue
!
      CALL CLOSE_CAT()
!
! --- Check: does file really exist
!
      INQUIRE ( FILE=DBFILE, EXIST=LEX )
      IF ( .NOT. LEX ) THEN
           CALL ERR_LOG ( 5157, IUER, 'GET_DBNAME_FILE', 'The database file '// &
     &         DBFILE(1:I_LEN(DBFILE))//' is not found' )
           RETURN
      END IF
!
! --- Inqure status of the file
!
      IS = FOR_STAT ( DBFILE, STAT_BLOCK )
      IF ( IS .NE. 0 ) THEN
           WRITE ( 6, * ) ' IS =',IS
           CALL ERR_LOG ( 5158, IUER, 'GET_DBNAME_FILE', 'Error in attempt '// &
     &         'to get information about the file database file '//DBFILE )
           RETURN
      END IF
!
! --- Get a file size in bytes
!
      IFSIZE = STAT_BLOCK(8)
      IF ( IFSIZE .LE. 0 ) THEN
           CALL ERR_LOG ( 5159, IUER, 'GET_DBNAME_FILE', 'Database file '// &
     &          DBFILE(1:I_LEN(DBFILE))//' has zero length' )
           RETURN
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  GET_DBNAME_FILE  #!#
