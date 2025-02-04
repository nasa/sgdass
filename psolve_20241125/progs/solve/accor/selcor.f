      SUBROUTINE SELCOR (JCAVAL, JCAPPL, QDCAL, QSITN, JNSTA, &
     &                   JNCAL, ISTAND, PROGCOM, LDBNAM, NNVER, PHC_STR )
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
!     Updated to specificaly type integers which
!-------------------------------------------------
! 1.  SELCOR PROGRAM SPECIFICATION
!
! 1.1
!     THIS SUBROUTINE ENABLES THE USER TO SELECT WHICH CALIBRATIONS
!     WILL BE USED FOR GIVEN STATIONS.  THE CALIBRATIONS SELECTED
!     WILL BE STORED IN BIT ARRAY JCAPPL.
!
!
! 1.2 REFERENCES:
!
! 2.  SELCOR INTERFACE
!
! 2.1 Parameter File
      INCLUDE 'solve.i'
!
! 2.2 INPUT Variables:
!
      integer*2 JCAVAL(MAX_ARC_STA), JCAPPL(MAX_ARC_STA)
      integer*2 LDBNAM(5),icalibhdx,icalibhdy
      INTEGER*4 IERRX, IERRLIN
      integer*2 ifirststatline,ifldlenall,ifldlenind,ifrstallline
      integer*2 ifrstchrxind,ifrsthixall,ifrsthixind,ifrstloxall
      integer*2 ifrstloxind,ihixchgallgol
      integer*2 ihixmorstat,ihixndb,ihixpdb,ihixret,iloxchgallgol
      integer*2 iloxmorstat,iloxndb,iloxpdb,iloxret,ipdblin,ixtitle
      integer*2 icalibhdlen,istationno,icalibno,ihighxind
      integer*2 ihighxall,ihighyall,i,iytitle,j,k,ifirststation
      integer*2 ihighyind,indchangestat,indchangecalib,iallchangecalib
      CHARACTER*1 PROGCOM
      CHARACTER*8 QDCAL(15)
      CHARACTER*8 QSITN(MAX_ARC_STA)
      integer*2 jnsta, jncal, istand, nnver
      INTEGER*4 IBOT_LINE
!
!            ISTAND        INTEGER           FOR USE WHEN SELCOR WAS
!                                            CALLED BY SDBH.  IF 1,
!                                            INDICATES THAT STANDARD
!                                            CALIBRATIONS WERE PASSED
!                                            TO SELCOR.  OTHERWISE, 0.
!            JCAVAL        INTEGER(32)       STORES WHICH CALIBRATIONS
!                                            ARE AVAILABLE FOR A GIVEN
!                                            STATION
!            JCAPPL        INTEGER(32)       STORES WHICH CALIBRATIONS
!                                            WERE ORIGINALLY APPLIED
!                                            TO A GIVEN STATION
!            JNCAL         INTEGER           SELCOR CAN HANDLE UP TO
!                                            15 CALIBRATIONS.  NUMBER
!                                            SELCOR WILL ACTUALLY DEAL
!                                            WITH.
!            JNSTA         INTEGER           SELCOR CAN HANDLE UP TO
!                                            32 STATIONS.  NUMBER
!                                            SELCOR WILL ACTUALLY DEAL
!                                            WITH.
!            LDBNAM        INTEGER (5)       NAME OF DATA BASE BEING
!                                            PROCESSED.
!            NNVER         INTEGER           VERSION OF DATA BASE
!                                            BEING PROCESSED.
!            PROGCOM       CHARACTER*1       INDICATES WHETHER SDBH
!                                            OR ACCOR CALLED SELCOR.
!                                            NEEDED TO INDICATE
!                                            WHETHER OR NOT MULTIPLE
!                                            DATA BASE PROCESSING IS
!                                            ALLOWED (ALLOWED FOR
!                                            ACCOR, NOT ALLOWED FOR
!                                            SDBH)
!            QDCAL         CHARACTER*8 (15)  CALIBRATION OPTIONS
!            QSITN         CHARACTER*8 (32)  STATIONS
!
! 2.3 OUTPUT Variables:
!
!            JCAPPL        INTEGER(32)       STORES WHICH CALIBRATIONS
!                                            WILL NOW BE USED
!            ISTAND         INTEGER          IF 1, UPON RETURN TO
!                                            SDBH, INDICATES THAT THE
!                                            USER DID NOT CHANGE ANY
!                                            CALIBRATIONS, AND THE
!                                            STANDARD, DEFAULT CALI-
!                                            BRATIONS WILL BE USED.
!                                            IF 0, INDICATES THAT
!                                            THE USER MADE AT LEAST
!                                            ONE CHANGE TO THE DEFAULT
!                                            CALIBRATIONS, AND THE
!                                            CALIBRATIONS ARE NO
!                                            LONGER NECESSARILY STAN-
!                                            DARD.  THEY MIGHT STILL
!                                            BE, IF THE USER REVERSED
!                                            ALL HIS CHANGES, BUT
!                                            BUT DON'T COUNT ON IT.
!            PROGCOM       CHARACTER*1       INDICATES WHETHER THE
!                                            USER WANTS TO SEE THE
!                                            SCREEN FOR THE NEXT DATA
!                                            BASE OR RETURN TO THE
!                                            OPTIN MENU (APPLIES ONLY
!                                            IF SELCOR WAS CALLED BY
!                                            ACCOR)
!
! 2.4 COMMON BLOCKS USED
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES: accor
!       CALLED SUBROUTINES: scor_statdat,scor_chall,scor_lkeyall,
!                     scor_blnkmsg,scor_chind,scor_allgoal
!
! 3.  LOCAL VARIABLES
!
!            ABNORMALMODE   LOGICAL          IF TRUE, OVERRIDES NORMAL
!                                            COMMAND PROCESSING TO
!                                            LEAVE SELCOR OR RETURN TO
!                                            ANOTHER SCREEN.  USED FOR
!                                            SCREEN WITH NO STATION
!                                            DATA TO CHANGE.
!            BUFR           CHARACTER*61     BUFFER TO WRITE TO SCREEN
!            CHANGEALLGOAL  CHARACTER*1      THE USER HAS THE OPTION
!                                            OF CHANGING THE DATA FOR
!                                            FOR A CALIBRATION, FOR
!                                            ALL STATIONS FOR WHICH
!                                            THE CALIBRATION IS
!                                            AVAILABLE, TO APPLIED OR
!                                            TO JUST AVAILABLE.
!                                            CHANGEALLGOAL SPECIFIES
!                                            WHETHER THE DATA WILL BE
!                                            CHANGED TO AVAILABLE OR
!                                            APPLIED.
!            DONE           LOGICAL          USED TO PREMATURELY BUT
!                                            CLEANLY EXIT LOOPS
!            I,J,K          INTEGER          FOR LOOP CONTROL
!            IALLCHANGECALIB INTEGER         CALIBRATION TO BE CHANGED
!                                            FOR ALL STATIONS
!            ICALIBHDLEN    INTEGER          CALIBRATIONS ARE WRITTEN
!                                            VERTICALLY, SEPARATED BY
!                                            COLUMNS OF '|'.
!                                            ICALIBHDLEN MEASURES THE
!                                            DISTANCE BETWEEN THE '|'S
!                                            PLUS 1, TO FACILITATE THE
!                                            PRINTING OF THE HEADINGS
!
!            ICALIBHDX,      INTEGER         X, Y COORDS. FOR BEGIN-NG
!             ICALIBHDY                      NING CALIBRATION HEADINGS
!            ICALIBNO       INTEGER          UP TO 15 CALIBRATIONS MAY
!                                            BE USED.  ACTUAL NUMBER.
!            IERRLIN        INTEGER          LINE OF SCREEN ON WHICH
!                                            ERROR MESSAGE IS PRINTED
!            IERRX          INTEGER          X COORDINATE OF FIRST
!                                            CHARACTER OF FIELD USED
!                                            TO PRINT ERROR MESSAGE
!            IFIRSTSTATION  INTEGER          UP TO 32 STATIONS MAY BE
!                                            USED, BUT A SCREEN CAN
!                                            ONLY HANDLE HALF AT A TIME.
!                                            INDEX TO FIRST SITE IN THE
!                                            HALF CURRENTLY BEING PRINTED.
!            IFIRSTSTATLINE INTEGER          SCREEN LINE ON WHICH
!                                            FIRST ROW OF DATA WILL BE
!                                            PRINTED
!            IFLDLENALL     INTEGER          FIELD LENGTH OF FIELDS
!                                            FOR CHANGING ALL STATIONS
!            IFLDLENIND     INTEGER          FIELD LENGTH OF FIELDS
!                                            FOR CHANGING AN
!                                            INDIVIDUAL STATION
!            IFRSTALLLINE   INTEGER          SCREEN LINE ON WHICH
!                                            FIRST LINE OF INSTRUC-
!                                            TIONS FOR CHANGING ALL
!                                            STATIONS WILL BE PRINTED
!            IFRSTCHRXIND   INTEGER          X COORD. WHERE TOP LEFT
!                                            MOST STATION/CALIBRATION
!                                            STATISTIC WILL BE PRINTED
!            IFRSTHIXALL    INTEGER          HIGHEST X COORD. OF FIRST
!                                            FIELD FOR CHANGING ALL
!                                            STATIONS
!            IFRSTHIXIND    INTEGER          HIGHEST X COORD. OF FIRST
!                                            FIELD FOR CHANGING AN
!                                            INDIVIDUAL STATION.
!            IFRSTLOXALL    INTEGER          LOWEST X COORD. OF FIRST
!                                            FIELD FOR CHANGING ALL
!                                            STATIONS FOR A GIVEN
!                                            CALIBRATION.  USED TO
!                                            FIND OTHER "CHANGE ALL"
!                                            FIELDS IN A LOOP.
!            IFRSTLOXIND    INTEGER          LOWEST X COORD. OF FIRST
!                                            FIELD FOR CHANGING AN
!                                            INDIVIDUAL STATION.
!            IHIGHXALL      INTEGER          DEPENDING ON HOW MANY
!                                            CALIBRATIONS ARE USED,
!                                            CERTAIN FIELDS MAY OR MAY
!                                            NOT BE VALID INPUT FIELDS
!                                            CONTAINS HIGHEST X COORD.
!                                            OF BOTTOM RIGHTMOST VALID
!                                            FIELD FOR CHANGING ALL
!                                            STATIONS.
!            IHIGHYALL      INTEGER          HIGHEST Y COORD. OF
!                                            BOTTOM RIGHTMOST VALID
!                                            FIELD FOR CHANGING ALL
!                                            STATIONS.
!            IHIGHXIND      INTEGER          CONTAINS HIGHEST X COORD.
!                                            OF RIGHTMOST VALID FIELD
!                                            FOR CHANGING INDIVIDUAL
!                                            STATIONS.
!            IHIGHYIND      INTEGER          Y COORD. OF BOTTOM VALID
!                                            FIELD FOR CHANGING
!                                            INDIVIDUAL STATIONS
!            IHIXCHGALLGOL  INTEGER          HIGHEST X COORD. OF FIELD
!                                            FOR SPECIFYING WHETHER
!                                            THE STATUS OF A CALIBRA-
!                                            TION SHOULD BE CHANGED TO
!                                            APPLIED OR NOT APPLIED,
!                                            FOR ALL APPROPRIATE
!                                            STATIONS
!            IHIXMORSTAT    INTEGER          HIGHEST X COORD. OF FIELD
!                                            FOR VIEWING ADDITIONAL
!                                            STATIONS
!            IHIXNDB        INTEGER          HIGHEST X COORD. OF FIELD
!                                            ALLOWING THE USER TO SEE
!                                            THE NEXT DATA BASE
!            IHIXPDB        INTEGER          HIGHEST X COORD. OF FIELD
!                                            ALLOWING THE USER TO SEE
!                                            THE PREVIOUS DATA BASE
!            IHIXRET        INTEGER          HIGHEST X COORD. OF FIELD
!                                            FOR RETURNING TO CALLING
!                                            PROGRAM
!            ILIM1,ILIM2    INTEGER          INDICATE WHERE INFO. WILL
!                                            BE PLACED IN THE BUFFER
!            ILOXCHGALLGOL  INTEGER          LOWEST X COORD. OF FIELD
!                                            FOR CHANGING OPTION OF
!                                            WHETHER ALL APPROPRIATE
!                                            STATIONS SHOULD HAVE THE
!                                            STATUS OF A GIVEN CALI-
!                                            BRATION CHANGED TO
!                                            APPLIED OR NOT APPLIED
!            ILOXMORSTAT    INTEGER          LOWEST X COORD. OF FIELD
!                                            FOR VIEWING ADDITIONAL
!                                            STATIONS
!            ILOXNDB        INTEGER          LOWEST X COORD. OF FIELD
!                                            ALLOWING THE USER TO SEE
!                                            THE NEXT DATA BASE
!            ILOXPDB        INTEGER          LOWEST X COORD. OF FIELD
!                                            ALLOWING THE USER TO SEE
!                                            THE PREVIOUS DATABASE
!            ILOXRET        INTEGER          LOWEST X COORD. OF FIELD
!                                            FOR RETURNING TO CALLING
!                                            PROGRAM
!            INDCHANGECALIB INTEGER          CALIBRATION TO BE CHANGED
!                                            FOR INDIVIDUAL STATION.
!            INDCHANGESTAT  INTEGER          INDIVIDUAL STATION FOR
!                                            WHICH CALIBRATION IS TO
!                                            BE CHANGED.
!            IPDBLIN        INTEGER          Y COORD. FOR LINE CONTAINING
!                                            FIELD TO SELECT PREVIOUS
!                                            DATABASE
!            IPRINTCOORDX   INTEGER          X COORD. OF FIELD WHICH
!                                            GIVES THE STATUS OF A
!                                            CALIBRATION FOR A GIVEN
!                                            STATION.
!            IPRINTCOORDY   INTEGER          Y COORD. OF FIELD WHICH
!                                            GIVES THE STATUS OF A
!                                            CALIBRATION FOR A GIVEN
!                                            STATION.
!            ISCRCHNGLIN    INTEGER          SCREEN LINE CONTAINING
!                                            FIELDS WHICH ALLOW THE
!                                            USER TO CHANGE A SCREEN
!            ISTATIONNO     INTEGER          UP TO 32 STATIONS MAY BE
!                                            USED.  ACTUAL NUMBER.
!            IXTITLE        INTEGER          X COORD. OF BEGINNING
!                                            POSITION OF SCREEN TITLE
!            IYTITLE        INTEGER          LINE ON WHICH SCREEN
!                                            TITLE WILL BE WRITTEN
!            JCSPCL         INTEGER          IF BIT I IS ON, CALIB I IS
!                                            UNAVAILABLE BUT CAN BE
!                                            APPLIED ANYWAY
!            JPRINT         CHARACTER*15(32) USED TO STORE AND PRINT
!                                            DATA ABOUT WHETHER A
!                                            CALIBRATION IS UNAVAIL-
!                                            ABLE, AVAILABLE BUT NOT
!                                            APPLIED, OR APPLIED TO A
!                                            STATION
!            NUM_PER_SCREEN INTEGER*2        NUMBER OF SITES THAT CAN BE
!                                            DISPLAYED ON A SCREEN
!            NUM_SPCL       INTEGER          NUMBER OF SPECIAL (UNAVAILABLE
!                                            BUT APPLICABLE) CALIBRATIONS
!            SPCL_NAMES     CHARACTER*8(2)   LIST OF SPECIAL (UNAVAILABLE
!                                            BUT APPLICABLE) CALIBRATIONS
!
!
      LOGICAL*2 ABNORMALMODE, DONE
      INTEGER*2 JCSPCL, NUM_SPCL, ILIM1, ILIM2
      integer*4 ix,iy,ich,iprintcoordx,iprintcoordy,iscrchnglin
      integer*4 togind
      INTEGER*4 I4P0, I4P2, I4P13, I4P14, I4P18, I4P22, I4P24, I4P40, I4P62
      INTEGER*4 IX_SAVE, IY_SAVE
      character*4 cch
      equivalence (ich,cch)
      CHARACTER   BLANKVAR*9, CHANGEALLGOAL*1, JPRINT(MAX_ARC_STA)*15, &
     &            SPCL_NAMES(2)*8, BUFR*61, BUFSTR*79, PHASE_CAL_STR*8, &
     &            PHC_STR(MAX_ARC_STA)*2, STR*79, GET_VERSION*54
      INTEGER*2   NUM_PER_SCREEN
      DATA I4P0, I4P2, I4P13, I4P14, I4P18, I4P22, I4P24, I4P40, &
     &    I4P62/   0,    2,    13,    14,    18,    22,    24,    40,    62  /
      DATA PHASE_CAL_STR / 'PHAS CAL' /
      DATA BLANKVAR/'         '/
      DATA ICALIBHDX/10/, ICALIBHDY/2/
      DATA IERRX/0/, IERRLIN/23/
      DATA IFIRSTSTATLINE/10/
      DATA IFLDLENALL/13/, IFLDLENIND/5/
      DATA IFRSTALLLINE/21/
      DATA IFRSTCHRXIND/13/
      DATA IFRSTHIXALL/21/, IFRSTHIXIND/14/
      DATA IFRSTLOXALL/16/, IFRSTLOXIND/11/
      DATA IHIXCHGALLGOL/15/
      DATA IHIXMORSTAT/32/
      DATA IHIXNDB/49/
      DATA IHIXPDB/70/
      DATA IHIXRET/15/
      DATA ILOXCHGALLGOL/0/
      DATA ILOXMORSTAT/18/
      DATA ILOXNDB/35/
      DATA ILOXPDB/52/
      DATA ILOXRET/0/
      DATA IPDBLIN/23/
      DATA ISCRCHNGLIN/23/
      DATA IXTITLE/0/
      DATA IYTITLE/0/
      DATA NUM_SPCL /0/
      DATA SPCL_NAMES /'        ','        '/
!
      INTEGER*4 I_LEN
      INTEGER*2 INT2_ARG
!
! 4.  HISTORY
!   WHO   WHEN   WHAT
!   KDB  860115  Created
!   KDB  860203  Modified to receive number of stations and calibrations
!                directly from SDBH, rather than calculate them.
!   KDB  860417  Modified to handle multiple data bases
!   KDB  870807  Allow user to apply a calibration which the NAMFIL
!                says is unavailable (possible even though the database
!                has no LCODE for the calibration, because SOCAL will
!                generate the calib)
!   KDB  870824  Go to CRES directly from SELCOR screen; go to page 2-4
!                directly from OPTIN menu; go to previous database
!   JWR  890317  Allow the use of '/' as well as '*' for toggle
!                include or delete
!   MWH  900416  Add Lanyi wet and dry to list of special cases
!   AEE  920522  Added code to switch between flyby and partial menus.
!   JLR  921215  Added I4Pn variable to replace nJ constants
!   KDB  950810  Finishing changes for 32 sites.
!   PET  970828  Forced it work for 80x24 screen
!   PET  2000.07.04   Added support of priniting phase cal status
!   pet  2000.07.04   Added printing a version label at the first line and
!                     the session name at the second line
!
! 5.  SELCOR PROGRAM STRUCTURE
!
      NUM_PER_SCREEN = 10 !number of sites per screen
      ABNORMALMODE   = .FALSE.
      CHANGEALLGOAL  = 'P'
      ICALIBHDLEN    = IFLDLENIND
      ISTATIONNO     = JNSTA
      ICALIBNO       = JNCAL
!
      IHIGHXIND      = IFRSTHIXIND + (ICALIBNO - 1) * IFLDLENIND
!
      IF (ICALIBNO .LE. 5) THEN
        IHIGHXALL = IFRSTHIXALL + (ICALIBNO - 1) * IFLDLENALL
        IHIGHYALL = IFRSTALLLINE
      ELSE
        IHIGHXALL = IFRSTHIXALL + (ICALIBNO - 6) * IFLDLENALL
        IHIGHYALL = IFRSTALLLINE + 1
      END IF
!
!     SET UP VARIABLE WHICH INDICATES WHICH CALIBRATIONS ARE SPECIAL
!     CASES (I.E., CAN BE APPLIED, EVEN THOUGH THE NAMFIL SAYS THEY'RE
!     UNAVAILABLE).
!
!     Note: this code is now obsolete, but will be left in, at least until
!     the new namfil scheme works well a while in practice, because
!     not harming anything and may make a comeback
      JCSPCL = 0
      DO 20 I = 1, NUM_SPCL
        DO J = 1, ICALIBNO
          IF (QDCAL(J) .EQ. SPCL_NAMES(I)) THEN
            CALL SBIT( JCSPCL, J, INT2(1) )
            GO TO 20
          END IF
        END DO
 20   CONTINUE
      IFIRSTSTATION = 1
!
!     BUILD INITIAL SCREEN.
!
!     CLEAR SCREEN AND WRITE TITLE
!
      IX_SAVE = 11
      IY_SAVE = -1
 75   CONTINUE
      CALL SETCR_MN (I4P0,I4P0 )
      CALL CLEAR_MN()
      IX = IXTITLE
      IY = IYTITLE
      CALL SETCR_MN (IX, IY )
      BUFSTR = 'Database Calibrations Status'
      CALL REVERSE_ON_MN()
      CALL ADDSTR_F ( BUFSTR(1:8) )
      CALL REVERSE_OFF_MN()
      CALL ADDSTR_F ( BUFSTR(9:)  )
!
      STR = GET_VERSION()
      CALL SETCR_MN ( 79-I_LEN(STR), 0 )
      CALL REVERSE_ON_MN()
      CALL ADDSTR_F ( STR(1:I_LEN(STR)) )
      CALL REVERSE_OFF_MN()
!
! --- Then list calibration headings.  calibration headings are
! --- written vertically, with '|' s surrounding them.  Each heading
! --- is up to 8 characters long.  The headings are written row by row
!
      CALL SETCR_MN ( I4P0, IY+1 )
      WRITE ( BUFSTR, '(10X,10I5)') (I, I=1, ICALIBNO )
      CALL ADDSTR_F ( BUFSTR(1:I_LEN(BUFSTR)) )
!
      CALL SETCR_MN ( 58, 1 )
      WRITE ( BUFSTR, 1010 ) (LDBNAM(J), J = 1, 5), NNVER
 1010 FORMAT ( 5A2, '  Vers.', I4 )
      CALL ADDSTR_F ( BUFSTR(1:21) )
!
      CALL NL_MN()
      IX = 0
      DO 80 I = 1, 8
        DO K = 1, 61
           BUFR(K:K) = ' '
        END DO
        DO 85 J = 1, ICALIBNO
          ILIM1 = ICALIBHDX + 1 + (J - 1) * ICALIBHDLEN
          ILIM2 = ILIM1 + (ICALIBHDLEN - 1)
          BUFR(ILIM1:ILIM2) = '| '//QDCAL(J)(I:I)//'  '
 85     CONTINUE
        ILIM1 = ILIM2+1
        ILIM2 = ILIM1+5
        BUFR(ILIM1:ILIM2) = '| '//PHASE_CAL_STR(I:I)//'  |'
        IY = ICALIBHDY + (I - 1)
        CALL SETCR_MN ( IX, IY )
        CALL ADDSTR_F ( BUFR   )
        CALL NL_MN()
 80   CONTINUE
!
! --- Then print list of stations and station/calibration application
! --- data.  also print key for data and instructions for changing a
! --- calibration for all stations at once.
!
      CALL SCOR_STATDAT ( IFIRSTSTATION, ISTATIONNO, ICALIBNO, QSITN, JCAVAL, &
     &                    JCAPPL, JPRINT, IHIGHYIND, PHC_STR )
      CALL SCOR_LKEYALL ( ICALIBNO, QDCAL, CHANGEALLGOAL, IHIGHYIND )
!
! --- Then build line of screen changing instructions
!
 89   CONTINUE
      TOGIND    = IHIGHYIND+1
      IBOT_LINE = IHIGHYIND+2
      CALL SETCR_MN ( I4P0, IBOT_LINE )
      CALL CLRTOEOL_MN()
      IF ( PROGCOM .EQ. 'A' ) THEN
           CALL ADDSTR_F ( '(O)ptions (F)lyby par(T)ial more_(S)tations '// &
     &                     '(N)ext_database (P)revious_database' )
         ELSE
           CALL ADDSTR_F ( '(O)ptions (F)lyby par(T)ial more_(S)tations' )
      END IF
      IF ( ISTATIONNO .GE. NUM_PER_SCREEN ) THEN
           CALL SETCR_MN ( I4P62, IBOT_LINE-4 )
           CALL ADDSTR_F ( 'Part of stations' )
           CALL SETCR_MN ( I4P62, IBOT_LINE-3 )
           CALL ADDSTR_F ( 'is displayed' )
           CALL REFRESH_MN()
      END IF
      CALL REFRESH_MN()
!
! --- Accept, interpret user's command.  Erase any messages from the
! --- USER's previous command.
!
 90   CONTINUE
      CALL SETCR_MN     ( I4P0, TOGIND )
      CALL CLRTOEOL_MN()
!
      CALL SETCR_MN     ( I4P40, TOGIND )
      CALL SCOR_ALLGOAL ( CHANGEALLGOAL, ICALIBNO )
!
      CALL SETCR_MN ( I4P0, TOGIND )
      CALL ADDSTR_F ( 'Toggle >> ' )
      IF ( IY_SAVE < 0 ) IY_SAVE = TOGIND 
      CALL SETCR_MN ( IX_SAVE, IY_SAVE )
 91   CONTINUE
      CALL SENKR_MN ( IX, IY, ICH )
      IX_SAVE = IX 
      IY_SAVE = IY
!
!     ABNORMAL COMMAND PROCESSING:  IF THE USER TRIED TO VIEW INFO.
!     FOR NON-EXISTENT STATIONS DURING HIS PREVIOUS COMMAND, SELCOR
!     ENTERED AN ABNORMAL MODE.  IN THIS MODE, NO MATTER WHAT KEY THE
!     USER TYPES, ONLY 6 COMMANDS ARE RECOGNIZED:  RETURNING TO OPTIN
!     OR SDBH, GOING TO THE NEXT DATA BASE, GOING TO THE PREVIOUS DATABASE,
!     STARTING PROC OR CRES, AND RETURNING TO THE SCREEN WITH
!     EXISTING STATIONS, FOR THIS DATA BASE.  IF NONE OF THE FIRST 5
!     COMMANDS ARE CHOSEN, SELCOR DEFAULTS TO RETURNING TO THE SCREEN
!     WITH THE EXISTING STATIONS.
!
      IF ( IY .EQ. IBOT_LINE .AND. CCH(4:4) .EQ. ' ' ) THEN
           IF ( IX .GE.  0 .AND. IX .LE.  8 ) CCH(4:4)='O'
           IF ( IX .GE. 10 .AND. IX .LE. 16 ) CCH(4:4)='F'
           IF ( IX .GE. 18 .AND. IX .LE. 26 ) CCH(4:4)='T'
           IF ( IX .GE. 28 .AND. IX .LE. 42 ) CCH(4:4)='S'
           IF ( IX .GE. 44 .AND. IX .LE. 58 ) CCH(4:4)='N'
           IF ( IX .GE. 60 .AND. IX .LE. 78 ) CCH(4:4)='P'
      END IF
!
      IF (CCH(4:4) .EQ. ' ') GO TO 100
!
! --- Normal command processing:  selcor will recognize each command
! --- as a pair of coordinates, so convert letter and number commands
! --- to the proper coordinates.  (0, 0) will be invalid coordinates,
! --- generating an invalid message for an invalid key command.
!
 101  CONTINUE
      IF (CCH(4:4) .EQ. 'O') THEN
        IX = 0
        IY = ISCRCHNGLIN
      ELSE IF (CCH(4:4) .EQ. 'F') THEN  ! (F)lyby
        progcom = 'F'
        goto 150
      ELSE IF (CCH(4:4) .EQ. 'T') THEN  ! par(T)ial
        progcom = 'T'
        goto 150
      ELSE IF (CCH(4:4) .EQ. 'S') THEN
        IX = 18
        IY = ISCRCHNGLIN+1
        IY_SAVE = -1
      ELSE IF (CCH(4:4) .EQ. '*'.or.CCH(4:4).eq.'/') THEN
        IX = ILOXCHGALLGOL + 3
        IY = IFRSTALLLINE
      ELSE IF (CCH(4:4) .EQ. 'N') THEN   !Next database
        IX = 35
        IY = ISCRCHNGLIN+1
      ELSE IF (CCH(4:4) .EQ. 'P') THEN   !Previous database
        IX = ILOXPDB
        IY = IPDBLIN
      ELSE IF (CCH(4:4) .EQ. 'Q' .OR. CCH(4:4) .EQ. '@') THEN !Hidden option
        IF (PROGCOM .EQ. 'A') THEN !In OPTIN, so can do it
          PROGCOM = 'Q'                        !Least squares (PROC)
          IF (CCH(4:4) .EQ. '@') PROGCOM = '@'      !Calculate residuals (CRES)
          GO TO 150
        ELSE  !In SDBH
          IF (ABNORMALMODE) THEN !Default to change screen
            IX = ILOXMORSTAT
            IY = ISCRCHNGLIN+1
            GO TO 170
          ELSE  !or flag as error
            GO TO 130
          END IF
        END IF
      ELSE IF (CCH(4:4) .EQ. 'R') THEN !Hidden option (refresh screen)
        GO TO 75
      ELSE IF (CCH(4:4) .EQ. '1' .AND. ICALIBNO .GE. 1) THEN
        IX = 13
        IY = TOGIND
      ELSE IF (CCH(4:4) .EQ. '2' .AND. ICALIBNO .GE. 2) THEN
        IX = 18
        IY = TOGIND
      ELSE IF (CCH(4:4) .EQ. '3' .AND. ICALIBNO .GE. 3) THEN
        IX = 23
        IY = TOGIND
      ELSE IF (CCH(4:4) .EQ. '4' .AND. ICALIBNO .GE. 4) THEN
        IX = 28
        IY = TOGIND
      ELSE IF (CCH(4:4) .EQ. '5' .AND. ICALIBNO .GE. 5) THEN
        IX = 33
        IY = TOGIND
      ELSE IF (CCH(4:4) .EQ. '6' .AND. ICALIBNO .GE. 6) THEN
        IX = 38
        IY = TOGIND
      ELSE IF (CCH(4:4) .EQ. '7' .AND. ICALIBNO .GE. 7) THEN
        IX = 43
        IY = TOGIND
      ELSE IF (CCH(4:4) .EQ. '8' .AND. ICALIBNO .GE. 8) THEN
        IX = 48
        IY = TOGIND
      ELSE IF (CCH(4:4) .EQ. '9' .AND. ICALIBNO .GE. 9) THEN
        IX = 53
        IY = TOGIND
      ELSE IF (CCH(4:4) .EQ. '0' .AND. ICALIBNO .GE. 10) THEN
        IX = 58
        IY = TOGIND
      ELSE
        IX = 0
        IY = 0
      END IF
!
!     THE USER'S INPUT IS NOW EXPRESSED AS COORDINATES.  PERFORM THE
!     THE USER'S COMMAND.
!
!     IF THE COMMAND WAS TO CHANGE AN INDIVIDUAL STATION:
!          IF THE USER CHOSE COORDINATES THAT REPRESENT A VALID
!          CALIBRATION, DETERMINE THE PRECISE STATION AND CALIBRATION
!          CHOSEN, PROCESS AND RETURN TO 90 FOR MORE INPUT.
!          OTHERWISE, GO TO 130 FOR ERROR PROCESSING.
!
 100  CONTINUE
!
! --- DETERMINE IF THE USER WANTS TO CHANGE AN INDIVIDUAL STATION.
!
      IF ( IY .GE. IFIRSTSTATLINE .AND. &
     &     IY .LE. (IFIRSTSTATLINE + NUM_PER_SCREEN) ) THEN
!
! ------ Up to 10 calibrations may be permitted.  Depending on the actual
! ------ number permitted, some coordinates of the screen may represent
! ------ valid or invalid input.  Make sure that the input coordinates
! ------ are valid.
!
        IF ( IX .LE. IHIGHXIND  .AND.  IY .LE. IHIGHYIND ) THEN
             IF ( IFIRSTSTATION .EQ. 1) THEN
                  INDCHANGESTAT = IY - 9
               ELSE
                  INDCHANGESTAT = IY - 9 + NUM_PER_SCREEN
             END IF
             DONE = .FALSE.
             I = 1
!
! --------- Make sure that the user input his command within an actual field
! --------- and not on a boundary or station name. If the input was valid,
! --------- determine the specific calibration requested.
!
            DO WHILE ( I .LE. ICALIBNO .AND. ( DONE .EQV. .FALSE.) )
               IF ( IX .GE. (IFRSTLOXIND + (I - 1) * IFLDLENIND) .AND. &
     &              IX .LE. (IFRSTHIXIND + (I - 1) * IFLDLENIND)      ) THEN
                  INDCHANGECALIB = I
                  IPRINTCOORDX = IFRSTCHRXIND + (I - 1) * IFLDLENIND
                  IPRINTCOORDY = IY
                  CALL SCOR_CHIND ( INDCHANGESTAT, INDCHANGECALIB, JCAVAL, &
     &                 JCAPPL, JPRINT, IPRINTCOORDX, IPRINTCOORDY, ISTAND, &
     &                 JCSPCL )
                  DONE = .TRUE.
               END IF
               I = I + 1
            END DO
            IF ( DONE ) THEN
                 GOTO 75  ! not 91
              ELSE
                 GOTO 130
            END IF
        ELSE IF ( IY.EQ.TOGIND ) THEN
!
! ------- First consider the 'toggle' case
!
          IALLCHANGECALIB = (IX-5) / 5
          IF ( IALLCHANGECALIB .GE. 1 .AND. IALLCHANGECALIB .LE. ICALIBNO ) THEN
!
! -------------- Version commented out made tuning bit calibration regardless
! -------------- of CHANGEALLGOAL. I removed this feature since it contradicts
! -------------- printed status of CHANGEALLGOAL. (pet 28-AUG-97)
!
!#               CALL SCOR_CHALL ( IALLCHANGECALIB, IFIRSTSTATION, JCAVAL,
!#     #                         JCAPPL, JPRINT, ISTATIONNO, ISTAND, 'T', JCSPCL )
               CALL SCOR_CHALL ( IALLCHANGECALIB, IFIRSTSTATION, JCAVAL, &
     &              JCAPPL, JPRINT, ISTATIONNO, ISTAND, CHANGEALLGOAL, JCSPCL )
!
!
          ENDIF
          GOTO 75  ! not 90 due to curses problems
        ELSE
          GOTO 130
       END IF
      END IF
!
!     IF THE USER WANTED TO CHANGE THE STATUS TO WHICH A GIVEN
!     CALIBRATION WILL BE CHANGED FOR ALL STATIONS:
!           NOTE THE CHANGE AND CHANGE THE SCREEN TO REMIND THE
!           USER THAT HE NOW WANTS TO CHANGE ALL STATIONS TO THE
!           NEW STATUS.
!
      IF ( (IX .GE. ILOXCHGALLGOL .AND. IX .LE. IHIXCHGALLGOL) .AND. &
     &     (IY .EQ. IFRSTALLLINE .OR. IY .EQ. IFRSTALLLINE +1)      ) THEN
           IF ( CHANGEALLGOAL .EQ. 'P') THEN
                CHANGEALLGOAL = 'V'
             ELSE
                CHANGEALLGOAL = 'P'
            END IF
            GOTO 90
      END IF
!
!     IF THE COMMAND WAS TO CHANGE A CALIBRATION FOR ALL STATIONS:
!          IF THE USER CHOSE COORDINATES THAT REPRESENT A VALID
!          CALIBRATION, DETERMINE THE PRECISE CALIBRATION, PROCESS
!          AND RETURN TO 90 FOR MORE INPUT.  OTHERWISE, GO TO 130 FOR
!          ERROR PROCESSING.
!
!     SEE IF THE USER WANTED TO CHANGE A CALIBRATION FOR ALL STATIONS.
!
!
!
      IF (IY .EQ. IFRSTALLLINE .OR. IY .EQ. (IFRSTALLLINE + 1)) THEN
        IF (IY .LT. IHIGHYALL .OR. &
     &      (IX .LE. IHIGHXALL .AND. IY .EQ. IHIGHYALL)) THEN
          J = 1
          DONE = .FALSE.
          DO WHILE (J .LE. 2 .AND. (DONE .EQV. .FALSE.))
            I = 1
            DO WHILE (I .LE. 5 .AND. (DONE .EQV. .FALSE.))
              IF (IX .GE. IFRSTLOXALL + (I - 1) * IFLDLENALL .AND. &
     &            IX .LE. IFRSTHIXALL + (I - 1) * IFLDLENALL .AND. &
     &            IY .EQ. IFRSTALLLINE + (J - 1)) THEN
                IALLCHANGECALIB = I + (J - 1) * 5
                CALL SCOR_CHALL (IALLCHANGECALIB, IFIRSTSTATION, &
     &                                 JCAVAL, JCAPPL, JPRINT, &
     &                                 ISTATIONNO, ISTAND, &
     &                                 CHANGEALLGOAL,JCSPCL )
                DONE = .TRUE.
              END IF
              I = I + 1
            END DO
            J = J + 1
          END DO
          IF (DONE) THEN
            GO TO 90
          ELSE
            GO TO 130
          END IF
        ELSE
          GO TO 130
        END IF
      END IF
!
!     IF THE COMMAND WAS TO SEE THE SCREEN FOR THE NEXT DATA BASE,
!     AND SELCOR WAS CALLED BY ACCOR, RETURN TO ACCOR SO THAT IT
!     CAN RECORD ANY CHANGES MADE BY SELCOR AND SET UP FOR THE
!     NEXT DATA BASE.  IF SELCOR WAS CALLED BY SDBH, TREAT THE
!     COMMAND AS INVALID, OR IF THERE ARE NO STATIONS ON THIS
!     SCREEN, GO TO THE OTHER SCREEN.
!
  120 CONTINUE
      IF (IX .GE. ILOXNDB .AND. IX .LE. IHIXNDB .AND. &
     &    IY .EQ. ISCRCHNGLIN+1) THEN
        IF (PROGCOM .EQ. 'A') THEN
          PROGCOM = 'N'
          GO TO 150
        ELSE
          IF (ABNORMALMODE) THEN
            IX = ILOXMORSTAT
            IY = ISCRCHNGLIN+1
            GO TO 170
          ELSE
            GO TO 130
          END IF
        END IF
      END IF
!
!     IF THE COMMAND WAS TO SEE THE SCREEN FOR THE PREVIOUS DATABASE,
!     AND SELCOR WAS CALLED BY ACCOR, RETURN TO ACCOR SO THAT IT
!     CAN RECORD ANY CHANGES MADE BY SELCOR AND SET UP FOR THE
!     PREVIOUS DATABASE.  IF SELCOR WAS CALLED BY SDBH, TREAT THE
!     COMMAND AS INVALID, OR IF THERE ARE NO STATIONS ON THIS
!     SCREEN, GO TO THE OTHER SCREEN.
!
  125 CONTINUE
      IF (IX .GE. ILOXPDB .AND. IX .LE. IHIXPDB .AND. &
     &    IY .EQ. IPDBLIN) THEN
        IF (PROGCOM .EQ. 'A') THEN
          PROGCOM = 'P'
          GO TO 150
        ELSE
          IF (ABNORMALMODE) THEN
            IX = ILOXMORSTAT
            IY = ISCRCHNGLIN+1
            GO TO 170
          ELSE
            GO TO 130
          END IF
        END IF
      END IF
!
!     IF THE COMMAND WAS TO RETURN TO THE CALLING PROGRAM, RETURN
!
      IF (IX .GE. ILOXRET .AND. IX .LE. IHIXRET .AND. &
     &    IY .EQ. ISCRCHNGLIN) THEN
        PROGCOM = 'R'
        GO TO 150
      END IF
!
!    IF THE COMMAND WAS TO VIEW ANY ADDITIONAL STATIONS:
!         3 CASES OCCUR:
!         1.  IF THE PREVIOUS SCREEN LISTED STATIONS BUT NO STATIONS
!             EXIST FOR THE NEXT SCREEN, THE STATION LIST, STATION/
!             CALIBRATION DATA, KEY AND LISTS OF COMMANDS FOR CHANGING
!             ALL STATIONS WILL BE ERASED AND REPLACED BY A MESSAGE THAT
!             NO ADDITIONAL STATIONS EXIST.  THE ABNORMAL MODE WILL BE
!             SET TO QUICKLY RETURN THE USER TO A USEFUL SCREEN.
!         2.  IF STATIONS EXIST FOR BOTH SCREENS, SIMPLY CHANGE THE
!             LIST OF STATIONS AND THE STATION/CALIBRATION DATA
!         3.  IF NO STATIONS EXISTED FOR THE PREVIOUS SCREEN, BUT
!             STATIONS WILL EXIST FOR THE NEXT SCREEN, MUCH OF THE
!             SCREEN MUST BE RE-BUILT.  BUILD THE LIST OF STATIONS,
!             THE STATION/CALIBRATION DATA, THE KEY TO THE DATA AND
!             THE LINE OF COMMANDS FOR CHANGING ALL STATIONS.
!             RETURN TO NORMAL MODE.
!         IN ALL CASES, IFIRSTSTATION MUST BE CHANGED FROM
!           1 TO NUM_PER_SCREEN OR NUM_PER_SCREEN TO 1
!           SINCE THE NEW SCREEN WILL DEAL WITH THE OTHER
!           HALF OF THE LIST.
!
 170  CONTINUE
      IF ( IX .GE. ILOXMORSTAT  .AND.  IX .LE. IHIXMORSTAT  .AND. &
     &     IY .EQ. ISCRCHNGLIN+1 ) THEN
           IF ( IFIRSTSTATION .EQ. 1) THEN
                IFIRSTSTATION = NUM_PER_SCREEN + 1
                IF ( ISTATIONNO .LE. NUM_PER_SCREEN ) THEN
                     ABNORMALMODE = .TRUE.
                   IX = 0
!
! ---------------- Clean up
!
                   DO 160 I = IFIRSTSTATLINE, IFRSTALLLINE + 1
                      IY = I
                      CALL SETCR_MN (IX, IY )
                      WRITE ( BUFSTR, 1130 )
 1130                 FORMAT ( 80X )
                      CALL ADDSTR_F ( BUFSTR )
 160               CONTINUE
!
                   CALL SETCR_MN ( IERRX, IERRLIN )
                   CALL ADDSTR_F ( "No additional stations are available " )
                   CALL SENKR_MN ( IX, IY, ICH )
                   GOTO 75  ! not 90
                 ELSE
                   CALL SCOR_STATDAT ( IFIRSTSTATION, ISTATIONNO, ICALIBNO, &
     &                                 QSITN, JCAVAL, JCAPPL, JPRINT, &
     &                                 IHIGHYIND, PHC_STR )
                   GOTO 89
           END IF
        ELSE
          IFIRSTSTATION = 1
          IF (ABNORMALMODE) THEN
            ABNORMALMODE = .FALSE.
!
            DO 175 I = 1, 2
              IX = 22
              IY = 13 + (I - 1)
              CALL SETCR_MN (IX, IY )
              call addstr_f("                          " )
 175        CONTINUE
!
            CALL SCOR_STATDAT (IFIRSTSTATION, ISTATIONNO, ICALIBNO, &
     &                           QSITN, JCAVAL, JCAPPL, JPRINT, &
     &                           IHIGHYIND, PHC_STR )
            CALL SCOR_LKEYALL (ICALIBNO, QDCAL, CHANGEALLGOAL, &
     &                           IHIGHYIND )
            GOTO 89
          ELSE
            CALL SCOR_STATDAT (IFIRSTSTATION, ISTATIONNO, ICALIBNO, &
     &                           QSITN, JCAVAL, JCAPPL, JPRINT, &
     &                           IHIGHYIND, PHC_STR )
            GOTO 89
          END IF
        END IF
      END IF
!
!     THE USER GAVE INVALID INPUT.  PRINT AN ERROR MESSAGE.
!
 130  CONTINUE
      CALL SETCR_MN ( IERRX, IERRLIN )
      CALL ADDSTR_F ( "ERROR: invalid command" )
      GOTO 90
!
 150  CONTINUE
      RETURN
      END  !#!  SELCOR  #!#
