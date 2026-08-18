      SUBROUTINE SELCORF ( JCAFFL, QDFCAL, QSITN, JNSTA, &
     &                   NFCAL,  PROGCOM, LDBNAM, NNVER)
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
!     Updated to specificaly type integers which
!-------------------------------------------------
      INCLUDE 'solve.i'
!
! 1.  SELCORF PROGRAM SPECIFICATION
!
! 1.1
!     THIS SUBROUTINE ENABLES THE USER TO SELECT WHICH CALIBRATIONS
!     WILL BE USED FOR GIVEN STATIONS.  THE CALIBRATIONS SELECTED
!     WILL BE STORED IN BIT ARRAY JCAFFL.
!     USED FOR FLYBY  CALIBRATIONS ONLY.
!
!
! 1.2 REFERENCES:
!
! 2.  SELCORF INTERFACE
!
! 2.1 Parameter File
!
! 2.2 INPUT Variables:
!
      INTEGER*2  JCAFFL(7,MAX_ARC_STA)
      INTEGER*2 LDBNAM(5)
      CHARACTER*1 PROGCOM
      CHARACTER*8 QDFCAL(112)
      CHARACTER*8 QSITN(MAX_ARC_STA)
      integer*2 jnsta, NFCAL,  nnver
      INTEGER*4 I4P0, I4P2, I4P13, I4P14, I4P18, I4P22, I4P24, I4P40, I4P62
      INTEGER*4  IBOT_LINE, TOGIND
!
!            JCAFFL        INTEGER(32)       STORES WHICH CALIBRATIONS
!                                            WERE ORIGINALLY APPLIED
!                                            TO A GIVEN STATION
!            NFCAL         INTEGER           SELCORF CAN HANDLE UP TO
!                                            112 CALIBRATIONS.  NUMBER
!                                            SELCORF WILL ACTUALLY DEAL
!                                            WITH.
!            JNSTA         INTEGER           SELCORF CAN HANDLE UP TO
!                                            32 STATIONS.  NUMBER
!                                            SELCORF WILL ACTUALLY DEAL
!                                            WITH.
!            LDBNAM        INTEGER (5)       NAME OF DATA BASE BEING
!                                            PROCESSED.
!            NNVER         INTEGER           VERSION OF DATA BASE
!                                            BEING PROCESSED.
!            PROGCOM       CHARACTER*1       INDICATES WHETHER SDBH
!                                            OR ACCOR CALLED SELCORF.
!                                            NEEDED TO INDICATE
!                                            WHETHER OR NOT MULTIPLE
!                                            DATA BASE PROCESSING IS
!                                            ALLOWED (ALLOWED FOR
!                                            ACCOR, NOT ALLOWED FOR
!                                            SDBH)
!            QDFCAL         CHARACTER*8 (10)  CALIBRATION OPTIONS
!            QSITN         CHARACTER*8 (32)  STATIONS
!
! 2.3 OUTPUT Variables:
!
!            JCAFFL        INTEGER(32)       STORES WHICH CALIBRATIONS
!                                            WILL NOW BE USED
!            PROGCOM       CHARACTER*1       INDICATES WHETHER THE
!                                            USER WANTS TO SEE THE
!                                            SCREEN FOR THE NEXT DATA
!                                            BASE OR RETURN TO THE
!                                            OPTIN MENU (APPLIES ONLY
!                                            IF SELCORF WAS CALLED BY
!                                            ACCOR)
!            QDFCAL                          MAY RECEIVE NEW CALIBS
!            NFCAL                           MAY BE RAISED TO SHOW NEW #
!                                            OF CALIBS
!
! 2.4 COMMON BLOCKS USED
      INCLUDE 'precm.i'
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES: accor
!       CALLED SUBROUTINES: scorf_stadat,scorf_chall,scorf_lkyall,
!                     scor_blnkmsg,scorf_chind,scor_allgoal
!
! 3.  LOCAL VARIABLES
!
!            ABNORMALMODE   LOGICAL          IF TRUE, OVERRIDES NORMAL
!                                            COMMAND PROCESSING TO
!                                            LEAVE SELCORF OR RETURN TO
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
!            IERRLIN        INTEGER          LINE OF SCREEN ON WHICH
!                                            ERROR MESSAGE IS PRINTED
!            IERRX          INTEGER          X COORDINATE OF FIRST
!                                            CHARACTER OF FIELD USED
!                                            TO PRINT ERROR MESSAGE
!            IFIRSTSTATION  INTEGER          UP TO 32 STATIONS MAY BE
!                                            USED, BUT A SCREEN CAN
!                                            ONLY HANDLE HALF AT A TIME.
!                                            IDENTIFIES INDEX OF FIRST SITE IN
!                                            HALF CURRENTLY BEING LISTED.
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
!            JPRINT       CHARACTER*112(32)  USED TO STORE AND PRINT
!                                            DATA ABOUT WHETHER A
!                                            CALIBRATION IS UNAVAIL-
!                                            ABLE, AVAILABLE BUT NOT
!                                            APPLIED, OR APPLIED TO A
!                                            STATION
!            NUM_PER_SCREEN INTEGER*2        NUMBER OF SITES THAT CAN BE
!                                            DISPLAYED ON A SCREEN
!
!
      LOGICAL*2 ABNORMALMODE, DONE
      LOGICAL*2   FLAVOK
      INTEGER*2   ILIM1, ILIM2, NUM_FLAV
      CHARACTER   CHANGEALLGOAL*1, RETVAR*1, JPRINT(MAX_ARC_STA)*112, &
     &            FLAV_NAMES(112)*8, BUFR*61, NEW_CAL*8
      INTEGER*4   ICH, IX, IY, IERRX, IERRLIN, ISCRCHNGLIN, IMSGX, IMSGLIN
      CHARACTER*4 CCH
      EQUIVALENCE ( ICH, CCH )
      CHARACTER   BUFSTR*80, STR*79, GET_VERSION*54
      INTEGER*2 I,IALLCHANGECALIB,ICALIBHDLEN,ICALIBHDX,ICALIBHDY, &
     &  IFIRSTSTATION,IFIRSTSTATLINE,IFLDLENALL, &
     &  IFLDLENIND,IFRSTALLLINE,IFRSTCHRXIND,IFRSTHIXALL,IFRSTHIXIND, &
     &  IFRSTLOXALL,IFRSTLOXIND, &
     &  IHIGHXALL,IHIGHXIND,IHIGHYALL,IHIGHYIND,IHIXADDCAL,IHIXBCKCAL, &
     &  IHIXCHGALLGOL,IHIXFWDCAL,IHIXMORSTAT,IHIXNDB,IHIXPDB,IHIXRET, &
     &  ILOXADDCAL,ILOXBCKCAL,ILOXCHGALLGOL,ILOXFWDCAL,ILOXMORSTAT, &
     &  ILOXNDB,ILOXPDB,ILOXRET,INDCHANGECALIB, &
     &  INDCHANGESTAT,IPAGE,IPAGE_SAVE,IPAGE_TOT,IPDBLIN, &
     &  IP_NUM,IP_RANGE(2), &
     &  ISTATIONNO,IXTITLE,IYADDCAL,IYBCKCAL,IYFWDCAL,IYTITLE, &
     &  J,K,IERR
      integer*4 iprintcoordx,iprintcoordy
      integer*2 num_per_screen
!
      DATA ICALIBHDX/10/, ICALIBHDY/2/
      DATA IERRX/0/, IERRLIN/23/
      DATA IFIRSTSTATLINE/10/
      DATA IFLDLENALL/13/, IFLDLENIND/5/
      DATA IFRSTALLLINE/21/
      DATA IFRSTCHRXIND/13/
      DATA IFRSTHIXALL/21/, IFRSTHIXIND/14/
      DATA IFRSTLOXALL/16/, IFRSTLOXIND/11/
      DATA IHIXADDCAL /75/, IHIXBCKCAL /73/, IHIXCHGALLGOL/15/
      DATA IHIXFWDCAL /76/, IHIXMORSTAT/32/, IHIXNDB/49/
      DATA IHIXPDB/70/
      DATA IHIXRET/15/
      DATA ILOXADDCAL /63/
      DATA ILOXBCKCAL /63/
      DATA ILOXCHGALLGOL/0/
      DATA ILOXFWDCAL /63/
      DATA ILOXMORSTAT/18/
      DATA ILOXNDB/35/
      DATA ILOXPDB/52/
      DATA ILOXRET/0/
      DATA IPDBLIN/29/
      DATA ISCRCHNGLIN/23/
      DATA IXTITLE/0/
      DATA IYADDCAL /9/
      DATA IYBCKCAL /7/
      DATA IYFWDCAL /5/
      DATA IYTITLE/0/
      DATA I4P0, I4P2, I4P13, I4P14, I4P18, I4P22, I4P24, I4P40, &
     &    I4P62/   0,    2,    13,    14,    18,    22,    24,    40,    62  /
!
      INTEGER*4 I_LEN
      INTEGER*2 INT2_ARG
!
! 4.  HISTORY
!   WHO   WHEN   WHAT
!   KDB  910718  Created, based on selcor
!   AEE  920522  Added code to switch between non-flyby and partial menus.
!   JLR  921215  replaced nJ constants with I4Pn variables
!   KDB  950810  Finishing changes for 32 sites.
!   PET  970828  Forced it work for 80x24 screen
!   pet  2000.07.04   Added printing a version label at the first line and
!                     the session name at the second line
!
! 5.  SELCORF PROGRAM STRUCTURE
!
!     subroutine initialization
!
      NUM_PER_SCREEN = 10 !number of sites per screen
      ABNORMALMODE = .FALSE.
      CHANGEALLGOAL = 'P'
      ICALIBHDLEN = IFLDLENIND
      IMSGLIN = IERRLIN
      IMSGX = IERRX
      ISTATIONNO = JNSTA
          CALL FLYAVL_N( INT2(301), INT2(112), FLAV_NAMES, NUM_FLAV, IERR, &
     &         jcaffl )
          IF (IERR.NE.0) THEN
            CALL SETCR_MN(IERRX,IERRLIN )
            CALL SCOR_BLNKMSG()
            WRITE(bufstr,"('FLYAVL_N ERR ',I5,' <ret> to cont')") IERR
            call addstr_f(bufstr )
            call nl_mn()
            call getstr_f(bufstr )
            READ(bufstr,'(A1)') RETVAR
!
            CALL SETCR_MN(IERRX,IERRLIN )
            CALL SCOR_BLNKMSG()
            call &
     &           addstr_f("COULD NOT CHECK IF NEW CAL AVAIL, SO COULD NOT ADD" )
            call nl_mn()
            CALL SCORF_CENTER(IP_NUM,IHIGHYIND )
            GO TO 90
          END IF
          IF (NUM_FLAV.EQ.0) THEN
            CALL SETCR_MN(IERRX,IERRLIN )
            CALL SCOR_BLNKMSG()
            call &
     &           addstr_f("FLYCAL AVAIL FILE EMPTY - SEEMS TRASHED - <ret> to cont" )
            call getstr_f(bufstr )
            READ(bufstr,'(A1)') RETVAR
!
            CALL SETCR_MN(IERRX,IERRLIN )
            CALL SCOR_BLNKMSG()
            call &
     &           addstr_f("COULD NOT CHECK IF NEW CAL AVAIL, SO COULD NOT ADD" )
            CALL SCORF_CENTER(IP_NUM,IHIGHYIND )
            GO TO 90
          END IF
        nfcal = num_flav
        do i=1,nfcal
          qdfcal(i) = flav_names(i)
        enddo
      IPAGE = 1
      IF (NFCAL/10 * 10 .EQ. NFCAL) THEN
        IPAGE_TOT = NFCAL / 10
      else
        IPAGE_TOT = NFCAL / 10 + 1
      end if
!
!     set/reset information that depends on the current page
!
  65  CONTINUE
      IP_RANGE(1) = (IPAGE - 1) * 10 + 1
      IP_RANGE(2) = IPAGE * 10
      IF (IP_RANGE(2) .GT. NFCAL) IP_RANGE(2) = NFCAL
      IP_NUM = IP_RANGE(2) - IP_RANGE(1) + 1
!
      IHIGHXIND  = IFRSTHIXIND + (IP_NUM - 1) * IFLDLENIND
!
      IF (IP_NUM .LE. 5) THEN
        IHIGHXALL = IFRSTHIXALL + (IP_NUM - 1) * IFLDLENALL
        IHIGHYALL = IFRSTALLLINE
      ELSE
        IHIGHXALL = IFRSTHIXALL + (IP_NUM - 6) * IFLDLENALL
        IHIGHYALL = IFRSTALLLINE + 1
      END IF
!
!     BUILD/REBUILD SCREEN.
!
!     CLEAR SCREEN AND WRITE TITLE
!
 75   CONTINUE
      CALL CLEAR_MN()
      IX = IXTITLE
      IY = IYTITLE
      CALL SETCR_MN (IX, IY )
      BUFSTR = 'Flyby Station-Dependent Calibrations Status: '
      CALL REVERSE_ON_MN()
      CALL ADDSTR_F ( BUFSTR(1:5) )
      CALL REVERSE_OFF_MN()
      CALL ADDSTR_F ( BUFSTR(6:)  )
!
      STR = GET_VERSION()
      CALL SETCR_MN ( 79-I_LEN(STR), 0 )
      CALL REVERSE_ON_MN()
      CALL ADDSTR_F ( STR(1:I_LEN(STR)) )
      CALL REVERSE_OFF_MN()
!
! --- Then list calibration headings. Calibration headings are
! --- written vertically, with '|' s surrounding them.  Each heading
! --- is up to 8 characters long.  The headings are written row by row
!
      CALL SETCR_MN ( I4P0, IY+1 )
      WRITE ( BUFSTR, '(9X,10I5)') (I,I=1,IP_NUM)
      CALL ADDSTR_F ( BUFSTR(1:I_LEN(BUFSTR)) )
!
      CALL SETCR_MN ( 58, 1 )
      WRITE ( BUFSTR, 1008 ) (LDBNAM(J), J = 1, 5), NNVER
 1008 FORMAT ( 5A2, '  Vers.', I4 )
      CALL ADDSTR_F ( BUFSTR(1:21) )
!
      CALL NL_MN()
      IX = 0
      DO 80 I = 1, 8
        DO K = 1, 61
          BUFR(K:K) = ' '
        END DO
        DO 85 J = 1, IP_NUM
          ILIM1 = ICALIBHDX + 1 + (J - 1) * ICALIBHDLEN
          ILIM2 = ILIM1 + (ICALIBHDLEN - 1)
          WRITE (BUFR(ILIM1:ILIM2), &
     &      1010)QDFCAL(IP_RANGE(1)-1 + J)(I:I)
 1010     FORMAT ("| ", A1, 2X)
 85     CONTINUE
        ILIM2 = ILIM2 + 1
        ILIM1 = ILIM2
        WRITE (BUFR(ILIM1:ILIM2), 1015)
 1015   FORMAT ("|")
        IY = ICALIBHDY + (I - 1)
        CALL SETCR_MN (IX, IY )
        call addstr_f(bufr )
 80   CONTINUE
!
!     THEN PRINT LIST OF STATIONS AND STATION/CALIBRATION APPLICATION
!     DATA.  ALSO PRINT KEY FOR DATA AND INSTRUCTIONS FOR CHANGING A
!     CALIBRATION FOR ALL STATIONS AT ONCE.  AS A NEW FEATURE PRINTING
!     THREE NEW COMMANDS IN THE RIGHT MARGIN: ADDING A FLYBY CALIBRATION
!     GOING FORWARD TO THE NEXT SUBSET OF CALIBRATIONS (FOR THIS DATA BASE)
!     AND GOING BACKWARDS TO THE PREVIOUS CALIBRATIONS.
!
      IFIRSTSTATION = 1
      CALL SCORF_STADAT ( IFIRSTSTATION, ISTATIONNO, IP_NUM,IP_RANGE(1), &
     &                    QSITN,  JCAFFL, JPRINT, IHIGHYIND )
      CALL SCORF_LKYALL ( IP_NUM, QDFCAL, IP_RANGE(1), CHANGEALLGOAL, &
     &                    IHIGHYIND )
!
! --- Then build line of screen changing instructions
!
 89   CONTINUE
      IBOT_LINE = IHIGHYIND+2
      TOGIND    = IHIGHYIND+1
      CALL SETCR_MN ( I4P0, IBOT_LINE )
      CALL CLRTOEOL_MN()
      IF ( PROGCOM .EQ. 'A' ) THEN
           CALL ADDSTR_F ( '(O)ptions (D)atabase par(T)ial more_(S)tations '// &
     &                     '(N)ext_database (P)revious_datab' )
         ELSE
           CALL ADDSTR_F ( '(O)ptions more_(S)tations' )
      END IF
      IF ( ISTATIONNO .GE. NUM_PER_SCREEN ) THEN
           CALL SETCR_MN ( I4P62, IBOT_LINE-4 )
           CALL ADDSTR_F ( 'Part of stations' )
           CALL SETCR_MN ( I4P62, IBOT_LINE-3 )
           CALL ADDSTR_F ( 'is displayed' )
           CALL REFRESH_MN()
      END IF
!
! --- Accept, interpret user's command.  erase any messages from the
! --- user's previous command.
!
 90   CONTINUE
      CALL SETCR_MN ( I4P0, TOGIND )
      CALL CLRTOEOL_MN()
!
      CALL SETCR_MN ( I4P40, TOGIND )
      CALL SCOR_ALLGOAL ( CHANGEALLGOAL, IP_NUM )
!
      CALL SETCR_MN ( I4P0, TOGIND )
      CALL ADDSTR_F ( 'Toggle >> ' )
!
      CALL SENKR_MN ( IX, IY, ICH )
      CALL SETCR_MN ( IERRX, IERRLIN )
      CALL ADDSTR_F ( "                              " )
      CALL SETCR_MN ( IX, IY )
!
!     ABNORMAL COMMAND PROCESSING:  IF THE USER TRIED TO VIEW INFO.
!     FOR NON-EXISTENT STATIONS (ie the second page of stations, when
!     only one page exists) DURING HIS PREVIOUS COMMAND, SELCORF
!     ENTERED AN ABNORMAL MODE.  IN THIS MODE, NO MATTER WHAT KEY THE
!     USER TYPES, ONLY 6 COMMANDS ARE RECOGNIZED:  RETURNING TO OPTIN
!     OR SDBH, GOING TO THE NEXT DATA BASE, GOING TO THE PREVIOUS DATABASE,
!     STARTING PROC OR CRES, AND RETURNING TO THE SCREEN WITH
!     EXISTING STATIONS, FOR THIS DATA BASE.  IF NONE OF THESE COMMANDS
!     ARE CHOSEN, SELCORF GIVES AN ERROR MESSAGE AND MAKES THE USER TRY
!     AGAIN.
!
      IF (ABNORMALMODE) THEN
        IF (IX .GE. 0 .AND. IX .LE. 15 .AND. IY .EQ. ISCRCHNGLIN .AND. &
     &      CCH(4:4) .EQ. ' ') THEN
          GO TO 150
        ELSE IF (CCH(4:4) .EQ. 'O') THEN
          GO TO 150
        ELSE IF (CCH(4:4) .EQ. 'N') THEN
          IX = ILOXNDB
          IY = ISCRCHNGLIN
          GO TO 120
        ELSE IF (IX .GE. ILOXNDB .AND. IX .LE. IHIXNDB .AND. &
     &        IY .EQ. ISCRCHNGLIN .AND. CCH(4:4) .EQ. ' ') THEN
          GO TO 120
        ELSE IF (CCH(4:4) .EQ. 'P') THEN
          IX = ILOXPDB
          IY = IPDBLIN
          GO TO 125
        ELSE IF (IX .GE. ILOXPDB .AND. IX .LE. IHIXPDB .AND. &
     &        IY .EQ. IPDBLIN .AND. CCH(4:4) .EQ. ' ') THEN
          GO TO 125
        ELSE IF (CCH(4:4) .EQ. 'Q' .OR. CCH(4:4) .EQ. '@') THEN
          GO TO 101
        ELSE IF (CCH(4:4) .EQ. 'S') THEN
          IX = ILOXMORSTAT
          IY = ISCRCHNGLIN
          GO TO 170
        ELSE IF (IX .GE. ILOXMORSTAT .AND. IX .LE. IHIXMORSTAT .AND. &
     &            IY .EQ. ISCRCHNGLIN .AND. CCH(4:4) .EQ. ' ') THEN
          IX = ILOXMORSTAT
          IY = ISCRCHNGLIN
          GO TO 170
        ELSE
          GO TO 130
        END IF
      END IF
!
!     NORMAL COMMAND PROCESSING:  SELCORF WILL RECOGNIZE EACH COMMAND
!     AS A PAIR OF COORDINATES, SO CONVERT LETTER AND NUMBER COMMANDS
!     TO THE PROPER COORDINATES.  (0, 0) WILL BE INVALID COORDINATES,
!     GENERATING AN INVALID MESSAGE FOR AN INVALID KEY COMMAND.
!
 101  CONTINUE
      IF ( IY .EQ. IBOT_LINE .AND. CCH(4:4) .EQ. ' ' ) THEN
           IF ( IX .GE.  0 .AND. IX .LE.  8 ) CCH(4:4)='O'
           IF ( IX .GE. 10 .AND. IX .LE. 19 ) CCH(4:4)='D'
           IF ( IX .GE. 21 .AND. IX .LE. 29 ) CCH(4:4)='T'
           IF ( IX .GE. 31 .AND. IX .LE. 45 ) CCH(4:4)='S'
           IF ( IX .GE. 47 .AND. IX .LE. 61 ) CCH(4:4)='N'
           IF ( IX .GE. 63 .AND. IX .LE. 78 ) CCH(4:4)='P'
      END IF
      IF (CCH(4:4) .EQ. ' ') GO TO 100
!
      IF (CCH(4:4) .EQ. 'T') THEN  !  par(T)ial
        progcom = 'T'
        goto 150
      ELSE IF (CCH(4:4) .EQ. 'D') THEN  !  (D)atabase
        progcom = 'D'
        goto 150
      ELSE IF (CCH(4:4) .EQ. 'O') THEN
        IX = 0
        IY = ISCRCHNGLIN
      ELSE IF (CCH(4:4) .EQ. 'S') THEN
        IX = ILOXMORSTAT
        IY = ISCRCHNGLIN
      ELSE IF (CCH(4:4) .EQ. 'C') THEN
        IX = ILOXFWDCAL
        IY = IYFWDCAL
      ELSE IF (CCH(4:4) .EQ. 'K') THEN
        IX = ILOXBCKCAL
        IY = IYBCKCAL
      ELSE IF (CCH(4:4) .EQ. 'A') THEN
        IX = ILOXADDCAL
        IY = IYADDCAL
      ELSE IF (CCH(4:4) .EQ. '*'.or.CCH(4:4).eq.'/') THEN
        IX = ILOXCHGALLGOL + 3
        IY = IFRSTALLLINE
      ELSE IF (CCH(4:4) .EQ. 'N') THEN   !Next database
        IX = ILOXNDB
        IY = ISCRCHNGLIN
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
            IY = ISCRCHNGLIN
            GO TO 170
          ELSE  !or flag as error
            GO TO 130
          END IF
        END IF
      ELSE IF (CCH(4:4) .EQ. 'R') THEN ! Hidden option (refresh screen)
        GO TO 75
      ELSE IF (CCH(4:4) .EQ. '1' .AND. IP_NUM .GE. 1 ) THEN
        IX = 13
        IY = TOGIND
      ELSE IF (CCH(4:4) .EQ. '2' .AND. IP_NUM .GE. 2 ) THEN
        IX = 18
        IY = TOGIND
      ELSE IF (CCH(4:4) .EQ. '3' .AND. IP_NUM .GE. 3 ) THEN
        IX = 23
        IY = TOGIND
      ELSE IF (CCH(4:4) .EQ. '4' .AND. IP_NUM .GE. 4 ) THEN
        IX = 28
        IY = TOGIND
      ELSE IF (CCH(4:4) .EQ. '5' .AND. IP_NUM .GE. 5 ) THEN
        IX = 33
        IY = TOGIND
      ELSE IF (CCH(4:4) .EQ. '6' .AND. IP_NUM .GE. 6 ) THEN
        IX = 38
        IY = TOGIND
      ELSE IF (CCH(4:4) .EQ. '7' .AND. IP_NUM .GE. 7 ) THEN
        IX = 43
        IY = TOGIND
      ELSE IF (CCH(4:4) .EQ. '8' .AND. IP_NUM .GE. 8 ) THEN
        IX = 48
        IY = TOGIND
      ELSE IF (CCH(4:4) .EQ. '9' .AND. IP_NUM .GE. 9 ) THEN
        IX = 53
        IY = TOGIND
      ELSE IF (CCH(4:4) .EQ. '0' .AND. IP_NUM .GE. 10 ) THEN
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
!     DETERMINE IF THE USER WANTS TO CHANGE AN INDIVIDUAL STATION.
!
      IF ( IY .GE. IFIRSTSTATLINE .AND. &
     &     IY .LE. (IFIRSTSTATLINE + NUM_PER_SCREEN) ) THEN
!
! -------- Up to 10 calibrations may be permitted. Depending on the actual
! -------- number permitted, some coordinates of the screen may represent
! -------- valid or invalid input. make sure that the input coordinates
! -------- are valid.
!
           IF ( IX .LE. IHIGHXIND .AND. IY .LE. IHIGHYIND ) THEN
                IF ( IFIRSTSTATION .EQ. 1) THEN
                     INDCHANGESTAT = IY - 9
                   ELSE
                     INDCHANGESTAT = IY - 9 + NUM_PER_SCREEN
                END IF
                DONE = .FALSE.
                I = 1
!
! ------------- Make sure that the user input his command within an actual field
! ------------- and not on a boundary or station name. If the input was valid,
! ------------- determine the specific calibration requested.
!
                DO WHILE (I .LE. IP_NUM .AND. (DONE .EQV. .FALSE.))
                   IF ( IX .GE. (IFRSTLOXIND + (I - 1) * IFLDLENIND) .AND. &
     &                  IX .LE. (IFRSTHIXIND + (I - 1) * IFLDLENIND)      ) THEN
                       INDCHANGECALIB = IP_RANGE(1) - 1 + I
                       IPRINTCOORDX = IFRSTCHRXIND + (I - 1) * IFLDLENIND
                       IPRINTCOORDY = IY
                       CALL SCORF_CHIND ( INDCHANGESTAT, INDCHANGECALIB, &
     &                      JCAFFL, JPRINT, IPRINTCOORDX, IPRINTCOORDY )
                       DONE = .TRUE.
                   END IF
                   I = I + 1
                END DO
                IF ( DONE ) THEN
                     GOTO 90
                  ELSE
                     GOTO 130
                END IF
            ELSE IF ( IY .EQ. TOGIND ) THEN
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!          type *,' iy=',iy,' ix=', ix,' togind = ',togind ! %%
!          type *,' ihighxall = ',ihighxall ,' IP_RANGE(1) =',IP_RANGE(1)
!          call sleep ( i4p2 )
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!
! ------- If the command was to change a calibration for all stations:
! ------- if the user chose coordinates that represent a valid
! ------- calibration, determine the precise calibration, process
! ------- and return to 90 for more input.  Otherwise, go to 130 for
! ------- error processing.
!
! ------- See if the user wanted to change a calibration for all stations.
!
          IF ( IX .LE. IHIGHXALL+2 ) THEN
               DONE = .FALSE.
               I = 1
               DO WHILE (I .LE. IP_NUM .AND. (DONE .EQV. .FALSE.))
                  IF ( IX .GE. (IFRSTLOXIND + (I - 1) * IFLDLENIND) .AND. &
     &                 IX .LE. (IFRSTHIXIND + (I - 1) * IFLDLENIND)      ) THEN
!
                       IALLCHANGECALIB = IP_RANGE(1) - 1 + I
                       CALL SCORF_CHALL ( IALLCHANGECALIB, I, IFIRSTSTATION, &
     &                           JCAFFL, JPRINT, ISTATIONNO, CHANGEALLGOAL )
                       DONE = .TRUE.
                  END IF
                  I = I + 1
               END DO
!
               IF ( DONE ) THEN
                    GOTO 90
                 ELSE
                    GOTO 130
               END IF
            ELSE !  ( IX .GT. IHIGHXALL )
               GOTO 130
          END IF
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
     &     (IY .EQ. IFRSTALLLINE .OR. IY .EQ. IFRSTALLLINE +1)       ) THEN
         IF ( CHANGEALLGOAL .EQ. 'P') THEN
              CHANGEALLGOAL = 'V'
            ELSE
              CHANGEALLGOAL = 'P'
          END IF
          CALL SETCR_MN ( I4P40, TOGIND )
          CALL SCOR_ALLGOAL ( CHANGEALLGOAL, IP_NUM )
          GOTO 90
      END IF
!
!     IF THE COMMAND WAS TO SEE THE SCREEN FOR THE NEXT DATA BASE,
!     AND SELCORF WAS CALLED BY ACCOR, RETURN TO ACCOR SO THAT IT
!     CAN RECORD ANY CHANGES MADE BY SELCORF AND SET UP FOR THE
!     NEXT DATA BASE.  IF SELCORF WAS CALLED BY SDBH, TREAT THE
!     COMMAND AS INVALID, OR IF THERE ARE NO STATIONS ON THIS
!     SCREEN, GO TO THE OTHER SCREEN.
!
  120 CONTINUE
      IF (IX .GE. ILOXNDB .AND. IX .LE. IHIXNDB .AND. &
     &    IY .EQ. ISCRCHNGLIN) THEN
        IF (PROGCOM .EQ. 'A') THEN
          PROGCOM = 'N'
          GO TO 150
        ELSE
          IF (ABNORMALMODE) THEN
            IX = ILOXMORSTAT
            IY = ISCRCHNGLIN
            GO TO 170
          ELSE
            GO TO 130
          END IF
        END IF
      END IF
!
!     IF THE COMMAND WAS TO SEE THE SCREEN FOR THE PREVIOUS DATABASE,
!     AND SELCORF WAS CALLED BY ACCOR, RETURN TO ACCOR SO THAT IT
!     CAN RECORD ANY CHANGES MADE BY SELCORF AND SET UP FOR THE
!     PREVIOUS DATABASE.  IF SELCORF WAS CALLED BY SDBH, TREAT THE
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
            IY = ISCRCHNGLIN
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
!             CALIBRATION DATA, KEY, LISTS OF COMMANDS FOR CHANGING
!             ALL STATIONS AND COMMANDS FOR SEEING OTHER CAL SCREENS
!             AND ADDING CALS WILL BE ERASED AND REPLACED BY A MESSAGE THAT
!             NO ADDITIONAL STATIONS EXIST.  THE ABNORMAL MODE WILL BE
!             SET TO QUICKLY RETURN THE USER TO A USEFUL SCREEN.
!         2.  IF STATIONS EXIST FOR BOTH SCREENS, SIMPLY CHANGE THE
!             LIST OF STATIONS AND THE STATION/CALIBRATION DATA
!         3.  IF NO STATIONS EXISTED FOR THE PREVIOUS SCREEN, BUT
!             STATIONS WILL EXIST FOR THE NEXT SCREEN, BUILD THAT
!             SCREEN AND RETURN TO NORMAL MODE.
!         IN ALL CASES, IFIRSTSTATION MUST BE CHANGED FROM
!           1 TO NUM_PER_SCREEN OR NUM_PER_SCREEN TO 1
!           SINCE THE NEW SCREEN WILL DEAL WITH THE OTHER
!           HALF OF THE LIST.
!
 170  CONTINUE
      IF (IX .GE. ILOXMORSTAT .AND. IX .LE. &
     &       IHIXMORSTAT.AND. IY .EQ. ISCRCHNGLIN) THEN
        IF (IFIRSTSTATION .EQ. 1) THEN
          IFIRSTSTATION = NUM_PER_SCREEN + 1
          IF (ISTATIONNO .LE. NUM_PER_SCREEN) THEN
            ABNORMALMODE = .TRUE.
!
!           blank out illegal commands
!
            IX = ILOXFWDCAL
            IY = IYFWDCAL
            CALL SETCR_MN(IX,IY )
            call reverse_off_mn()
            call addstr_f("              " )
            IX = ILOXBCKCAL
            IY = IYBCKCAL
            CALL SETCR_MN(IX,IY )
            call addstr_f("           " )
            IX = ILOXADDCAL
            IY = IYADDCAL
            CALL SETCR_MN(IX,IY )
            call addstr_f("             " )
!
            IX = 0
            DO 160 I = IFIRSTSTATLINE, IFRSTALLLINE + 1
              IY = I
              CALL SETCR_MN (IX, IY )
              call clrtoeol_mn()
 160        CONTINUE
            CALL SETCR_MN ( IERRX, IERRLIN )
            CALL ADDSTR_F ( "No additional stations are available " )
            CALL SENKR_MN ( IX, IY, ICH )
            GOTO 75 ! not 90
          ELSE
            CALL SCORF_STADAT (IFIRSTSTATION, ISTATIONNO, IP_NUM, &
     &                           IP_RANGE(1), &
     &                           QSITN, JCAFFL, JPRINT, &
     &                           IHIGHYIND )
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
            CALL SCORF_STADAT ( IFIRSTSTATION, ISTATIONNO, IP_NUM, IP_RANGE(1), &
     &                          QSITN,  JCAFFL, JPRINT, IHIGHYIND )
            CALL SCORF_LKYALL (IP_NUM, QDFCAL, IP_RANGE(1), &
     &                         CHANGEALLGOAL, IHIGHYIND )
            GOTO 89
          ELSE
            CALL SCORF_STADAT ( IFIRSTSTATION, ISTATIONNO, IP_NUM, IP_RANGE(1), &
     &                          QSITN,  JCAFFL, JPRINT, IHIGHYIND )
            GOTO 89
          END IF
        END IF
      END IF
!
!     User wants to go backwards in the list of calibrations.
!
      IF (IX .GE. ILOXBCKCAL .AND. IX .LE. IHIXBCKCAL .AND. &
     &    IY .EQ. IYBCKCAL) THEN
        IPAGE_SAVE = IPAGE
        IPAGE = IPAGE - 1
        IF (IPAGE .LT. 1) IPAGE = IPAGE_TOT
        IF (IPAGE_SAVE .EQ. IPAGE) THEN
          CALL SETCR_MN (IERRX, IERRLIN )
          call addstr_f("SORRY, ONLY ONE PAGE OF CALIBRATIONS" )
          call nl_mn()
          GO TO 90
        ELSE
         GO TO 65
        END IF
      END IF
!
!     User wants to go forward in the list of calibrations.
!
      IF (IX .GE. ILOXFWDCAL .AND. IX .LE. IHIXFWDCAL .AND. &
     &    IY .EQ. IYFWDCAL) THEN
        IPAGE_SAVE = IPAGE
        IPAGE = IPAGE + 1
        IF (IPAGE .GT. IPAGE_TOT) IPAGE = 1
        IF (IPAGE_SAVE .EQ. IPAGE) THEN
          CALL SETCR_MN (IERRX, IERRLIN )
          call addstr_f("SORRY, ONLY ONE PAGE OF CALIBRATIONS" )
          call nl_mn()
          GO TO 90
        ELSE !new page
         GO TO 65
        END IF
      END IF
!
!     User wants to add a calibration.
!
      IF (IX .GE. ILOXADDCAL .AND. IX .LE. IHIXADDCAL .AND. &
     &    IY .EQ. IYADDCAL) THEN
        IF (NFCAL .EQ. 112) THEN
          CALL SETCR_MN (IERRX, IERRLIN )
          call addstr_f("SORRY, ALREADY AT MAX CALIBRATIONS" )
          call nl_mn()
          CALL SCORF_CENTER(IP_NUM,IHIGHYIND )
          GO TO 90
        ELSE
          CALL SETCR_MN (IMSGX, IMSGLIN )
          call addstr_f("GIVE ME THE NEW CAL NAME (:: to cancel) " )
          call getstr_f(bufstr )
          READ (bufstr,"(A8)") NEW_CAL
          IF (NEW_CAL(1:2) .EQ. '::') THEN
            CALL SETCR_MN(IMSGX,IMSGLIN )
            CALL SCOR_BLNKMSG()
            call addstr_f("(D)atabase  par(T)ial" )
            call nl_mn()
            CALL SCORF_CENTER(IP_NUM,IHIGHYIND )
            GO TO 90
          END IF
          DO I = 1, NFCAL
            IF (NEW_CAL .EQ. QDFCAL(I)) THEN
              CALL SETCR_MN(IERRX,IERRLIN )
              CALL SCOR_BLNKMSG()
             WRITE(bufstr, &
     &          "('SORRY, ',A8,' IS ALREADY IN YOUR NAMFIL')")NEW_CAL
              call addstr_f(bufstr )
              call nl_mn()
              CALL SCORF_CENTER(IP_NUM,IHIGHYIND )
              GO TO 90
            END IF
          END DO
          FLAVOK = .FALSE.
          DO I = 1,NUM_FLAV
            IF (NEW_CAL .EQ. FLAV_NAMES(I)) FLAVOK = .TRUE.
          END DO
          IF (.NOT. FLAVOK) THEN
            CALL SETCR_MN(IERRX,IERRLIN )
            CALL SCOR_BLNKMSG()
            WRITE(bufstr, &
     &        "(A8,' IS UNAVAILABLE <return to continue>')")NEW_CAL
            call addstr_f(bufstr )
            call nl_mn()
            call getstr_f(bufstr )
            READ (bufstr,"(A1)") RETVAR
!
            CALL SETCR_MN(IERRX,IERRLIN )
            CALL SCOR_BLNKMSG()
            call &
     &           addstr_f("TO MAKE IT AVAILABLE, ENTER IT IN SOCAL.F <return to continue>" )
            call getstr_f(bufstr )
            READ (bufstr,"(A1)") RETVAR
!
            CALL SETCR_MN(IERRX,IERRLIN )
            CALL SCOR_BLNKMSG()
            WRITE(bufstr, &
     &        "('AND IN ',A)")PRE_SAV_DIR(:PRE_SV_LEN)//AVAL_FCAL_FILE
            call addstr_f(bufstr )
            call nl_mn()
            CALL SCORF_CENTER(IP_NUM,IHIGHYIND )
            GO TO 90
          END IF
!
!         Update namfil variables
!
!         Since the user may just want to apply this at one station,
!         don't make any assumptions about where to apply this calibration.
!         Leave it off everywhere, and let the user worry about it.
!
          NFCAL = NFCAL + 1
          QDFCAL(NFCAL) = NEW_CAL
!
!         Update local variables that depend on the number of calibrations
!         in the user's namfil.
!
          IF (NFCAL/10 * 10 .EQ. NFCAL) THEN
            IPAGE_TOT = NFCAL / 10
          else
            IPAGE_TOT = NFCAL / 10 + 1
          end if
!
!         Go to the page where this calibration is
!
          IPAGE = IPAGE_TOT
          GO TO 65
        END IF
      END IF
!
!     THE USER GAVE INVALID INPUT.  PRINT AN ERROR MESSAGE.
!
 130  CONTINUE
      CALL SETCR_MN ( IERRX, IERRLIN )
      CALL ADDSTR_F ( "Error: invalid command" )
      GO TO 90
!
!
 150  CONTINUE
      RETURN
      END
