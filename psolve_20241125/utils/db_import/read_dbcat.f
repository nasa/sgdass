      SUBROUTINE READ_DBCAT ( DBI, MH_DBS, LH_DBS, CH_DBS, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  READ_DBCAT  reads local VLBI database catalogue and       *
! *   returns database filenames with full path which are within the     *
! *   date range [DBI.DATE_START, DBI.DATE_END] inclusive.               *
! *                                                                      *
! * ________________________ Input parameters: _________________________ *
! *                                                                      *
! *    DBI ( RECORD    ) -- Object with data structure for keeping       *
! *                         configuration of DB_IMPORT utility.          *
! * MH_DBS ( INTEGER*4 ) -- Maximal number of database names which       *
! *                         can be returned.                             *
! *                                                                      *
! * ________________________ Output parameters: ________________________ *
! *                                                                      *
! * LH_DBS ( INTEGER*4 ) -- Numbers of database file names.              *
! * CH_DBS ( CHARACTER ) -- Database filename list. Dimension: LH_DBS.   *
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
! * By-effect:  READ_DBCAT set environment variable LOCK_IGNORE=YES      *
! *             what allows to proceed with catalogue operations even    *
! *             if the database is locked. The process which calls       *
! *             READ_DBCAT should unset LCOK_IGNODE if it is going to    *
! *             update catalogue entries directly or indirectly.         *
! *                                                                      *
! * ### 18-OCT-2000    READ_DBCAT   v1.3 (c) L. Petrov 19-NOV-2003  ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'db_import.i'
      INCLUDE   'catcm.i'
      TYPE ( DBI__STRU ) ::  DBI
      INTEGER*4  MH_DBS, LH_DBS, IUER
      CHARACTER  CH_DBS(MH_DBS)*(*)
!
      INTEGER*4  MENT, KENT
      PARAMETER  ( MENT=32767 )
      CHARACTER  ENT(MENT)*10, ENT_GET*10, ENTRY_NAME*10, HUNG*10, &
     &           DESC*80, STR*80
!
      INTEGER*2  LDISP(40), IBIN(2), IHIGH(2), IASC(2), ICR, &
     &           LCIFILE(7), IDATE(5), ITIME(5), IHIGHOLD(2), LFMGR(3), &
     &           ID_AREA, IFLOP, IPEND, IVER, ISEC, IMCH, LEXKY(5), &
     &           LEXDISP(40), XDBL
      CHARACTER  TEXT*80, TEXTA*80, FINAM*14, FPATH*157, DB_NAME*10, CDATE*10, &
     &           SUFFIX*2, ENV_LINE*16
      SAVE       ENV_LINE
      EQUIVALENCE ( LEXDISP(1), TEXTA )
      EQUIVALENCE ( LDISP(1), TEXT )
      EQUIVALENCE ( LEXKY(5), HUNG )
      EQUIVALENCE ( LCIFILE(1), FINAM )
!
      CHARACTER  MON(12)*3
      DATA       MON &
     &         / &
     &           'JAN', 'FEB', 'MAR', 'APR', 'MAY', 'JUN', &
     &           'JUL', 'AUG', 'SEP', 'OCT', 'NOV', 'DEC' &
     &         /
      INTEGER*2  KERR, KERR_ANY, ICONT
      INTEGER*4  IYEAR, IDAY, J1, J2, IER
!
      INTEGER*2  INT2_ARG
      INTEGER*4  I_LEN, LTM_DIF
!
! --- Set LOCK_IGNORE environment variable
!
      ENV_LINE = 'LOCK_IGNORE=YES'//CHAR(0)
      CALL PUTENV ( ENV_LINE )
      IF ( DBI%VRB .GE. 3 ) THEN
           WRITE ( 6, * ) 'READ_DBCAT: Try to open MK3 catalogue.1'
      END IF
!
! --- Openning MK3 catalogue
!
      CALL OPEN_MK3_CAT ( %REF('SB'), KERR )
      IF ( KERR .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( INT4(KERR), STR )
           CALL ERR_LOG ( 5741, IUER, 'READ_DBCAT', 'Failure in openning '// &
     &         'database catalogue KERR='//STR )
           RETURN
      END IF
!
! --- Scan all entries of the catalogue and create the list of entries ENT
!
      IF ( DBI%VRB .GE. 3 ) THEN
           WRITE ( 6, * ) 'READ_DBCAT: start scanning catalogue file. Phase 1'
      END IF
      KENT = 0
      DO 410 J1=1,MENT
!
! ------ Read the next key
!
         IF ( J1 .EQ. 1 ) ICONT = INT2(-3)
         IF ( J1 .GT. 1 ) ICONT = INT2(0)
         CALL RUN_SPECIAL_KEYS ( %REF('EX'), ICONT, %REF(ENT_GET), %REF(DESC), &
     &                           KERR )
         IF ( KERR .LT. INT2(0) ) THEN
              CALL CLOSE_CAT()
              CALL CLRCH ( STR )
              CALL INCH  ( INT4(KERR), STR )
              CALL ERR_LOG ( 5742, IUER, 'READ_DBCAT', 'Error in reading '// &
     &            'a key KERR='//STR(1:I_LEN(STR))//' Database catalaogue '// &
     &            'is probably corrupt' )
              RETURN
         END IF
         IF ( DBI%VRB .GE. 3 ) THEN
              WRITE ( 6, * ) 'READ_DBCAT.  J1=',J1,' Entry: ', &
     &                        ENT_GET(1:I_LEN(ENT_GET))
         END IF
!
! ------ Extract Year and Day from the entry. Check: they should be valid
!
         CALL CHIN ( ENT_GET(2:3), IYEAR )
         CALL CHIN ( ENT_GET(7:8), IDAY  )
!
         IF ( ENT_GET(1:1) .EQ. '$'                        .AND. &
     &        LTM_DIF ( 0, 12, MON, ENT_GET(4:6) ) .GT. 0  .AND. &
     &        IYEAR .GE. 0 .AND. IYEAR .LE. 99             .AND. &
     &        IDAY  .GE. 1 .AND. IDAY  .LE. 31                   ) THEN
!
! ----------- ... yes they are valid. Increment entry counter
!
              KENT = KENT + 1
              ENT(KENT) = ENT_GET
         END IF
!
         IF ( KERR .EQ. INT2(2) ) GOTO 810
 410  CONTINUE
 810  CONTINUE
      LH_DBS = 0
      IF ( DBI%VRB .GE. 3 ) THEN
           WRITE ( 6, * ) 'READ_DBCAT: Continue scanning catalogue file. '// &
     &                    'Phase 2'
      END IF
!
! --- Check all entries
!
      DO 420 J2=1,KENT
         DB_NAME = ENT(J2)(2:)
!
! ------ Now we parse database file name. We extracts database suffix and
! ------ experiment date. The experiment date is decoded in Solve format
!
         IER = 0
         CALL PARSE_DBNAME ( DB_NAME, CDATE, SUFFIX, IER )
!
! ------ Check whether we should proces this key?
!
         IF ( IER .NE. 0 ) GOTO 420  ! It is not a database name
         IF ( CDATE .LT. DBI%DATE_START ) GOTO 420 ! Database is too early
         IF ( CDATE .GT. DBI%DATE_END   ) GOTO 420 ! Database is too old
!
! ------ All checks are done. Contiune
!
         ENTRY_NAME = ENT(J2)
!
! ------ Get the last chain of the entry
!
         CALL RUN_ANY_KEY ( %REF(ENTRY_NAME), INT2(2), %REF(HUNG), IVER, &
     &                      %REF(DESC), KERR_ANY )
!
! ------ Get information for this entry. Frnakly speaking we are interesting
! ------ in only the full database name
!
         CALL GET_EXPERIMENT_INFO ( %REF(ENT), LDISP, ID_AREA, IBIN, &
     &        IHIGH, IASC, LFMGR, ISEC, LCIFILE, IFLOP, IPEND, IVER, &
     &        XDBL, IDATE, ITIME, IHIGHOLD, IMCH, KERR )
!
! ------ Get file name
!
         CALL CLRCH ( FPATH )
         CALL NAME_TO_PATH ( %REF(ENT(J2)), IVER, LCIFILE, ICR, FPATH, KERR )
!
! ------ If the file is not active (not in disk) go to the next entry
! ------ active then go to the next entry
!
         IF ( INT4(KERR) .EQ. -1 ) GOTO 420 ! Couldn't find the experiment key
         IF ( INT4(KERR) .EQ. -2 ) GOTO 420 ! Couldn't find the specified entry
         IF ( INT4(KERR) .EQ. -3 ) GOTO 420 ! The data base is inactive
!
         IF ( INT4(KERR) .NE. 0 ) THEN
              CALL CLOSE_CAT()
              CALL CLRCH ( STR )
              CALL INCH  ( INT4(KERR), STR )
              CALL ERR_LOG ( 5743, IUER, 'READ_DBCAT', 'Error in getting '// &
     &            'database file KERR='//STR(1:I_LEN(STR))//'  Entry '// &
     &             ENT(J2) )
              RETURN
         END IF
!
         LH_DBS = LH_DBS + 1
         IF ( LH_DBS .GT. MH_DBS ) THEN
              CALL CLRCH ( STR )
              CALL INCH  ( MH_DBS, STR )
              CALL ERR_LOG ( 5744, IUER, 'READ_DBCAT', 'Too many databases '// &
     &            'in the current catalogue system. Parameter MH_DBS '// &
     &             STR(1:I_LEN(STR))//' is too small' )
              RETURN
         END IF
         CH_DBS(LH_DBS) = FPATH
         IF ( DBI%VRB .GE. 3 ) THEN
              WRITE ( 6, '(A,I6,A,I6,A,A)' ) 'READ_DBCAT.  J2=', J2, &
     &                ' LH_DBS =', LH_DBS, &
     &                ' File: ',CH_DBS(LH_DBS)(1:I_LEN(CH_DBS(LH_DBS)))
         END IF
 420  CONTINUE
      IF ( DBI%VRB .GE. 3 ) THEN
           WRITE ( 6, * ) 'READ_DBCAT: Ended scanning catalogue file. '
      END IF
!
! --- closing catalgoue file
!
      CALL CLOSE_CAT()
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  READ_DBCAT  #!#
