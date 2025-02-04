      SUBROUTINE SET_ACM()
! ************************************************************************
! *                                                                      *
! *   Routine  SET_ACM   displays, sets or changes parameters of         *
! *   a priori clock model to be applied to theoretical dalys and        *
! *   delay rates before making equations of conditions.                 *
! *                                                                      *
! *  ###  25-MAR-98    SET_ACM     v1.7  (c)  L. Petrov  09-JUL-98  ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
      INCLUDE    'solve.i'
      INCLUDE    'socom.i'
      INCLUDE    'help.i'
      INCLUDE    'prfil.i'
      INCLUDE    'oborg.i'
      INTEGER*2   LDBNAM(5,15), IDBV(15)
!
      CHARACTER     CC4*4, SIM*1, OUT*79, STR*32, STR1*32, STANAM*8
      CHARACTER CDBNAM(15)*10, DBNAME*16, JBUF*120, FINAM*255
      EQUIVALENCE ( CDBNAM, LDBNAM(1,1) )
      INTEGER*2    ICONT_I2, IDBX, IERR_I2
      INTEGER*4    IDBE(15)
      INTEGER*2    INT2_ARG
      INTEGER*4  IX, IY, IST, IO, J1, J2, J3, J4, J5, J6, J7, J8, J9, &
     &           IDB_BEG, IDB_END, IP, IP1, IP2, IM
      LOGICAL*4  WAS_CHANGE
      REAL*8     FJDOBS, LJDOBS, TT, TT2, TAU_ACM_NEW, RATE_ACM_NEW
      INTEGER*4  L_ACM, STAIND_ACM(M_ACM), STAIND_ACM_NEW(M_ACM)
      REAL*8     CLOOF_ACM(M_ACM),     CLODR_ACM(M_ACM)
      REAL*8     CLOOF_ACM_NEW(M_ACM), CLODR_ACM_NEW(M_ACM)
      REAL*8     OFF_LIM, DRF_LIM
      PARAMETER  ( OFF_LIM = 100.0 )
      PARAMETER  ( DRF_LIM = 1.D-3 )
      INTEGER*4  NC, NR, NL, MAX_ARC_STA_I4
      PARAMETER  ( NL = 20 )
      PARAMETER  ( NC = 5  )
      PARAMETER  ( MAX_ARC_STA_I4 = MAX_ARC_STA )
      PARAMETER  ( NR = MAX_ARC_STA/NC + MIN ( 1, MOD(MAX_ARC_STA_I4,NC) ) )
      CHARACTER    SETACM_VER*19, STALET*32
      PARAMETER  ( SETACM_VER  = 'SET_ACM    06/16/98' )
      DATA       STALET / '123456789ABCDEFGHIJKLMNOPQRSTUVW' /
      INTEGER*4   INT4
      INT4(INT2_ARG) = INT(INT2_ARG,KIND=4)
      INTEGER*4, EXTERNAL :: ILEN, I_LEN, IFIND_PL, LTM_DIF, MAKE_HELP_FINAM
!
      CALL END_MN()
!
      CALL DBPOX ( NUMDB, LDBNAM, IDBV, IDBE )
!
! --- Forming database name and version
!
      CALL CLRCH ( DBNAME )
      DBNAME = CDBNAM(1)
      DBNAME(12:) = '<'
      CALL INCH ( INT4(IDBV(1)), DBNAME(13:) )
      DBNAME( I_LEN(DBNAME)+1: ) = '>'
      IF ( NUMDB .GT. 2 ) THEN
           CALL ERR_LOG ( 6801, -1, 'SET_ACM', 'More than 2 databases are '// &
     &         'in scratch area. SET_ACM cannot work properly in such a '// &
     &         'case. Hint: reduce number of databases to 1 or 2 (but only '// &
     &         'it was the same session but for different bands) ' )
           CALL HIT_CONT ( %VAL(0), %VAL(0) )
           RETURN
         ELSE IF ( NUMDB .EQ. 2 ) THEN
           IF ( CDBNAM(1)(1:8) .EQ. CDBNAM(2)(1:8) ) THEN
                IF ( CDBNAM(1)(9:9) .EQ. 'S' ) THEN
                     IDBX = 2
                   ELSE
                     IDBX = 1
                END IF
              ELSE
                CALL ERR_LOG ( 6802, -1, 'SET_ACM', '2 databases are '// &
     &              'in scratch area but they are not databases for '// &
     &              'different bands for the same experiment. SET_ACM '// &
     &              'cannot work properly in such a case. Hint: reduce '// &
     &              'number of databases to 1 ' )
                CALL HIT_CONT ( %VAL(0), %VAL(0) )
                RETURN
           END IF
         ELSE IF ( NUMDB .EQ. 1 ) THEN
           IDBX = 1
      END IF
!
! --- Initialization
!
      WAS_CHANGE = .FALSE.
      L_ACM = 0
!
! --- Reading NAMFIL, ACM cards
!
      CALL OPENNAMFIL()
      ICONT_I2 = INT2(1)
      DO 410 J1=1,M_ACM
!
! ------ Reading ACM card from NAMFIL
!
         CALL CLRCH ( JBUF )
         CALL GETCARD ( IDBX, 'ACM ', ICONT_I2, JBUF, IERR_I2 )
         ICONT_I2 = INT2(0)
         IF  ( IERR_I2 .EQ. INT2(-3) ) THEN
!
! ------------ There were not such a card in NAMFIL.
!
               STAIND_ACM(J1) = 0
               CLOOF_ACM(J1)  = 0.0
               CLODR_ACM(J1)  = 0.0
               ICONT_I2 = INT2(1)
           ELSE IF  ( IERR_I2 .NE. INT2(0) ) THEN
!
! ------------- Error in reading NAMFIL
!
                WRITE ( 6, * ) ' J1=',J1,' IERR_I2=',IERR_I2
                CALL ERR_LOG ( 6803, -1, 'SET_ACM', 'Error in reading of '// &
     &              'ACM card' )
                CALL HIT_CONT ( %VAL(0), %VAL(0) )
                RETURN
           ELSE
!
! ----------- Decoding a card
!
              READ ( JBUF, &
     &          '(5X,I2,1X,I2,1X,I2,1X,A8,1X,D23.15,1X,D23.15)') &
     &             IP1, L_ACM, IP2, STANAM, CLOOF_ACM(J1), CLODR_ACM(J1)
              IF ( ILEN(STANAM) .EQ. 0 ) THEN
!
! ---------------- Card is empty
!
                   STAIND_ACM(J1) = 0
                   CLOOF_ACM(J1)  = 0.0
                   CLODR_ACM(J1)  = 0.0
                ELSE
!
! ---------------- Search of the station name in the list
!
                   IP = LTM_DIF ( 0, INT4(NUMSTA), ISITN_CHR, STANAM )
                   IF ( IP .LE. 0  .OR.  IP .GT. INT4(NUMSTA) ) THEN
                        CALL ERR_LOG ( 6804, -1, 'SET_ACM', 'Iternal error: '// &
     &                      'ACM card contains station "'//STANAM//'" which '// &
     &                      ' is out of list of stations participated in '// &
     &                      ' the session '//DBNAME )
                        CALL HIT_CONT ( %VAL(0), %VAL(0) )
                        RETURN
                   END IF
                   STAIND_ACM(J1) = IP
              END IF
         END IF
!
! ------ Make local copy of a priori clock model
!
         STAIND_ACM_NEW(J1) = STAIND_ACM(J1)
         CLOOF_ACM_NEW(J1)  = CLOOF_ACM(J1)
         CLODR_ACM_NEW(J1)  = CLODR_ACM(J1)
 410  CONTINUE
!
! --- Start curser
!
      CALL START_MN()
 910  CONTINUE
!
! --- Printing the first line: title of the program
!
      CALL CLEAR_MN()
      CALL SETCR_MN (  0, 0 )
      CALL ADDSTR_F ( 'Set a priori clock model' )
      CALL SETCR_MN (  79-ILEN(SETACM_VER), 0 )
      CALL REVERSE_ON_MN()
      CALL ADDSTR_F ( SETACM_VER )
      CALL REVERSE_OFF_MN()
      CALL NL_MN()
      CALL ADDSTR_F ( '같같같같같같같같같같같같' )
      CALL NL_MN()
      CALL NL_MN()
!
! --- Printing the header of the table of ACM
!
      CALL CLRCH ( OUT )
      OUT = '   Station  |   Clock offset (sec)   |  Clock rate (sec/sec)  |'
      CALL ADDSTR_F ( OUT )
      CALL NL_MN()
!
      CALL CLRCH ( OUT )
      OUT = '------------|------------------------|------------------------|'
      CALL ADDSTR_F ( OUT )
      CALL NL_MN()
!
! --- Printing values of currently applied a priori clock model
!
      DO 430 J3=1,M_ACM
         CALL CLRCH ( OUT )
!         CALL INCH  ( J3, OUT(1:1) )
!         OUT(2:2) = ')'
         IF ( STAIND_ACM_NEW(J3) .GE. 1        .AND. &
     &        STAIND_ACM_NEW(J3) .LE. NUMSTA   .AND. &
     &        J3 .LE. L_ACM                            ) THEN
!
! ----------- J3-th value of ACM table is not empty and corresponds to one
! ----------- really used
!
! ----------- Printing station name
!
              OUT(4:11)  = ISITN_CHR ( STAIND_ACM_NEW(J3) )
              OUT(13:13) = '|'
!
! ----------- printing value of clock shift
!
              IF ( CLOOF_ACM_NEW(J3) .EQ. 0.0 ) THEN
                   OUT(15:17) = '0.0'
                ELSE
                   WRITE ( OUT(15:36), FMT='(1PE22.15,0P)' ) CLOOF_ACM_NEW(J3)
                   CALL CHASHL ( OUT(15:36) )
              END IF
              OUT(38:38) = '|'
!
! ----------- Printing value of clock drift
!
              IF ( CLODR_ACM_NEW(J3) .EQ. 0.0 ) THEN
                   OUT(40:42) = '0.0'
                 ELSE
                   WRITE ( OUT(40:61), FMT='(1PE22.15,0P)' ) CLODR_ACM_NEW(J3)
                   CALL CHASHL ( OUT(40:61) )
              END IF
              OUT(63:63) = '|'
         END IF
         CALL ADDSTR_F ( OUT )
         CALL NL_MN()
 430  CONTINUE
!
! --- Printing the header of participated station table
!
      CALL CLRCH ( OUT )
      OUT = '---------------------------------------------------------------'
      CALL ADDSTR_F ( OUT )
      CALL NL_MN()
      CALL CLRCH ( OUT )
      OUT = '              Stations participated in session '//DBNAME
      CALL ADDSTR_F ( OUT )
      CALL NL_MN()
      CALL NL_MN()
!
! --- Printing the names of the participated stations and their code-letter.
! --- Station names are in two-dimensional table with NC columns and NR rows.
! --- Stations are put firstly by columns then by rows
!
      IST = 0
      DO 440 J4=1,NR
         CALL CLRCH ( OUT )
         IP = 1
         DO 450 J5=1,NC
            IST = IST + 1
            IF ( IST .GT. NUMSTA ) GOTO 450
            OUT(IP:IP+2) = '('//STALET(IST:IST)//')'
            IP = IP+4
            OUT(IP:IP+7) = ISITN_CHR(IST)
            IP = IP+11
 450     CONTINUE
         CALL ADDSTR_F ( OUT )
         CALL NL_MN()
 440  CONTINUE
!
! --- Printng a list of supported commands
!
      CALL CLRCH ( OUT )
      OUT = '---------------------------------------------------------------'
      CALL ADDSTR_F ( OUT )
      CALL NL_MN()
!
      CALL CLRCH ( OUT )
      OUT = '(A)Add a priori clock model; (D)Delete a priori clock model; '// &
     &      '(H)On-line help'
      CALL ADDSTR_F ( OUT )
      CALL NL_MN()
!
      CALL CLRCH ( OUT )
      OUT = '(O)Go back without saving;   (S)Save results and go back     '// &
     &      '(R)Refresh screen'
      CALL ADDSTR_F ( OUT )
      CALL NL_MN()
!
! --- Soliciting for user input
!
      CALL SETCR_MN ( 1, NL+1 )
      CALL SENKR_MN ( IX, IY, CC4 )
!
      SIM = CC4(4:4)
      IF ( SIM .EQ. ' '  .OR.  SIM .EQ. CHAR(13) ) THEN
!
! -------- Decoding input as poiting to the command
!
           IF ( IY .EQ. NL   .AND.   IX .GE. 1    .AND.  IX .LE. 28 ) SIM = 'A'
           IF ( IY .EQ. NL   .AND.   IX .GE. 29   .AND.  IX .LE. 60 ) SIM = 'D'
           IF ( IY .EQ. NL   .AND.   IX .GE. 61   .AND.  IX .LE. 78 ) SIM = 'H'
           IF ( IY .EQ. NL+1 .AND.   IX .GE. 1    .AND.  IX .LE. 28 ) SIM = 'O'
           IF ( IY .EQ. NL+1 .AND.   IX .GE. 29   .AND.  IX .LE. 60 ) SIM = 'S'
           IF ( IY .EQ. NL+1 .AND.   IX .GE. 61   .AND.  IX .LE. 78 ) SIM = 'R'
      END IF
!
! --- Parsing code of the command and execution
!
      IF ( SIM .EQ. 'A' ) THEN
!
! ======== Add a priori mode for a new station
!
           CALL CLRCH    ( OUT   )
           CALL SETCR_MN ( 0, NL )
           CALL ADDSTR_F ( OUT   )
!
           CALL SETCR_MN ( 0, NL+1 )
           CALL ADDSTR_F ( OUT     )
!
! -------- Check: do we have free room for model for a new station?
!
           IF ( L_ACM .EQ. M_ACM ) THEN
                CALL SETCR_MN ( 1, NL )
                CALL REVERSE_ON_MN()
                CALL ADDSTR_F ( '$$$ Too much stations with applied a piori '// &
     &                          'clock model. $$$' )
                CALL NL_MN()
                CALL REVERSE_OFF_MN()
                CALL SENKR_MN ( IX, IY, CC4 )
                GOTO 910
           END IF
 920       CONTINUE
!
! -------- Clean the line at the secreen
!
           CALL SETCR_MN ( 0, NL )
           CALL ADDSTR_F ( '                                      '// &
     &                     '                                      ' )
!
! -------- Solicit user to enter the station letter-code
!
           CALL SETCR_MN ( 0, NL )
           CALL ADDSTR_F ( 'Station (enter a letter code) >> ' )
           CALL CLRCH ( STR )
           CALL SENKR_MN ( IX, IY, CC4 )
           STR = CC4(4:4)
           IF ( ICHAR(STR(1:1)) .GE. 0  .AND. ICHAR(STR(1:1)) .LE. 32 ) THEN
                CALL CLRCH ( STR )
           END IF
!
! -------- Parsing letter code
!
           IF ( ILEN(STR) .EQ. 0 ) GOTO 910
           IST = INDEX ( STALET, STR(1:1) )
!
! -------- Was letter code valid?
!
           IF ( IST .LE. 0  .OR.  IST .GT. NUMSTA ) THEN
!
! ------------- Alas, no!
!
                CALL SETCR_MN ( 0, NL )
                CALL REVERSE_ON_MN()
                CALL ADDSTR_F ( '$$$ Wrong letter has been entered: "'// &
     &                           STR(1:1)//'"  $$$' )
                CALL REVERSE_OFF_MN()
                CALL ADDSTR_F ( '   ' )
                CALL SENKR_MN ( IX, IY, CC4 )
                GOTO 920
             ELSE
!
! ------------- Yes it was valid code, but was a priori model for that station
! ------------- already applied
!
                IF ( L_ACM .GE. 1 ) THEN
                     IP = IFIND_PL ( L_ACM, STAIND_ACM_NEW, IST )
                     IF ( IP .GT. 0 ) THEN
!
! ----------------------- Alas, a priori model has been already applied
!
                          CALL SETCR_MN ( 0, NL )
                          CALL REVERSE_ON_MN()
                          CALL ADDSTR_F ( '$$$ A priori clock model for '// &
     &                                     ISITN_CHR(STAIND_ACM_NEW(IP))// &
     &                                    ' has been already applied. $$$' )
                          CALL NL_MN()
                          CALL ADDSTR_F ( '$$$ Delete it first '// &
     &                                  'if you would like to modify it. $$$' )
                          CALL REVERSE_OFF_MN()
                          CALL ADDSTR_F ( '   ' )
                          CALL SENKR_MN ( IX, IY, CC4 )
                          GOTO 910
                     ENDIF
                END IF
!
! ------------- Increment of the counter, and putting index of the station to
! ------------- the table
!
                L_ACM = L_ACM + 1
                STAIND_ACM_NEW(L_ACM)=IST
           END IF
!
 930       CONTINUE
!
! -------- Force user to entere the value of clock offset (in sec!)
!
           CALL SETCR_MN ( 0, NL )
           CALL ADDSTR_F ( 'Clock offset for '// &
     &                      ISITN_CHR(STAIND_ACM_NEW(L_ACM))//' in sec  >> ' )
!
! -------- Reading input vaule for clock offset
!
           CALL CLRCH    ( STR )
           CALL GETSTR_F ( STR )
           IF ( ILEN(STR) .EQ. 0 ) THEN
                L_ACM = L_ACM - 1
                GOTO 910
           END IF
!
! -------- Parsing it
!
           IF ( INDEX ( STR, '.' ) .EQ. 0 ) STR = STR(1:I_LEN(STR))//'.'
           READ ( UNIT=STR(1:I_LEN(STR)), FMT='(F16.8)', IOSTAT = IO) &
     &            CLOOF_ACM_NEW(L_ACM)
!
! -------- Checks of correctness of parsing
!
           IF ( IO .NE. 0 ) THEN
                CALL CLRCH    ( OUT )
                CALL SETCR_MN ( 0, NL )
                CALL ADDSTR_F ( OUT )
!
                CALL SETCR_MN ( 0, NL )
                CALL REVERSE_ON_MN()
                CALL ADDSTR_F ( '$$$ Error of parsing '//STR(1:I_LEN(STR))// &
     &                          '  $$$' )
                CALL ADDSTR_F ( '   ' )
                CALL REVERSE_OFF_MN()
                CALL SENKR_MN ( IX, IY, CC4 )
                GOTO 930
           END IF
           IF ( DABS(CLOOF_ACM_NEW(L_ACM)) .GT. OFF_LIM ) THEN
                CALL    CLRCH ( STR1 )
                WRITE ( UNIT=STR1, FMT='(1PE10.4,0P)' ) OFF_LIM
                CALL CHASHL ( STR1 )
                IF ( STR1(1:1) .EQ. '.' ) STR1='0'//STR1
!
                CALL CLRCH    ( OUT )
                CALL SETCR_MN ( 0, NL )
                CALL ADDSTR_F ( OUT )
!
                CALL SETCR_MN ( 0, NL )
                CALL REVERSE_ON_MN()
                CALL ADDSTR_F ( '$$$ Clock offset '//STR(1:I_LEN(STR))// &
     &                          ' appeared to exceed the limit: '// &
     &                          STR1(1:I_LEN(STR1))//'  $$$' )
                CALL REVERSE_OFF_MN()
                CALL ADDSTR_F ( '   ' )
                CALL SENKR_MN ( IX, IY, CC4 )
                GOTO 930
           END IF
!C
 940       CONTINUE
!
! -------- Force user to enter clock drift
!
           CALL SETCR_MN ( 0, NL )
           CALL ADDSTR_F ( 'Clock drift for '// &
     &          ISITN_CHR(STAIND_ACM_NEW(L_ACM))//' in sec/sec  >> ' )
!
! -------- Reading input vaule for clock drift
!
           CALL CLRCH    ( STR )
           CALL GETSTR_F ( STR )
           IF ( ILEN(STR) .EQ. 0 ) THEN
                L_ACM = L_ACM - 1
                GOTO 910
           END IF
!
! -------- Parsing it
!
           IF ( INDEX ( STR, '.' ) .EQ. 0 ) STR = STR(1:I_LEN(STR))//'.'
           READ ( UNIT=STR(1:I_LEN(STR)), FMT='(F16.8)', IOSTAT = IO) &
     &            CLODR_ACM_NEW(L_ACM)
!
! -------- Checks of correctness of parsing
!
           IF ( IO .NE. 0 ) THEN
                CALL CLRCH    ( OUT )
                CALL SETCR_MN ( 0, NL )
                CALL ADDSTR_F ( OUT )
!
                CALL SETCR_MN ( 0, NL )
                CALL REVERSE_ON_MN()
                CALL ADDSTR_F ( '$$$ Error of parsing '//STR(1:I_LEN(STR))// &
     &                          '  $$$' )
                CALL ADDSTR_F ( '   ' )
                CALL REVERSE_OFF_MN()
                CALL SENKR_MN ( IX, IY, CC4 )
                GOTO 940
           END IF
           IF ( DABS(CLODR_ACM_NEW(L_ACM)) .GT. DRF_LIM ) THEN
                CALL    CLRCH ( STR1 )
                WRITE ( UNIT=STR1, FMT='(1PE10.4,0P)' ) DRF_LIM
                CALL CHASHL ( STR1 )
                IF ( STR1(1:1) .EQ. '.' ) STR1='0'//STR1
!
                CALL CLRCH    ( OUT )
                CALL SETCR_MN ( 0, NL )
                CALL ADDSTR_F ( OUT )
!
                CALL SETCR_MN ( 0, NL )
                CALL REVERSE_ON_MN()
                CALL ADDSTR_F ( '$$$ Clock drift '//STR(1:I_LEN(STR))// &
     &                          ' appeared to exceed the limit: '// &
     &                          STR1(1:I_LEN(STR1))//'  $$$' )
                CALL REVERSE_OFF_MN()
                CALL ADDSTR_F ( '   ' )
                CALL SENKR_MN ( IX, IY, CC4 )
                GOTO 940
           END IF
           WAS_CHANGE = .TRUE.
         ELSE IF ( SIM .EQ. 'D' ) THEN
!
! ======== Delete the station from table
!
           CALL CLRCH    ( OUT   )
           CALL SETCR_MN ( 0, NL )
           CALL ADDSTR_F ( OUT   )
!
           CALL SETCR_MN ( 0, NL+1 )
           CALL ADDSTR_F ( OUT     )
!
! -------- Check: was at least onbe station with a priori model applied?
!
           IF ( L_ACM .EQ. 0 ) THEN
                CALL CLRCH    ( OUT )
                CALL SETCR_MN ( 0, NL )
                CALL ADDSTR_F ( OUT )
!
                CALL SETCR_MN ( 0, NL )
                CALL REVERSE_ON_MN()
                CALL ADDSTR_F ( '$$$  A piori clock model has not been '// &
     &                          'applied for any station  $$$' )
                CALL NL_MN()
                CALL REVERSE_OFF_MN()
                CALL SENKR_MN ( IX, IY, CC4 )
                GOTO 910
           END IF
!
! -------- Solicit user to enter letter code of the station to be deleted
!
 950       CONTINUE
!
! -------- Clean the line at the secreen
!
           CALL SETCR_MN ( 0, NL )
           CALL ADDSTR_F ( '                                      '// &
     &                     '                                      ' )
!
           CALL SETCR_MN ( 0, NL )
           CALL ADDSTR_F ( 'Enter letter-code of the station to be deleted >> ' )
!
! -------- Reading user input
!
           CALL CLRCH ( STR )
           CALL SENKR_MN ( IX, IY, CC4 )
           STR = CC4(4:4)
           IF ( ICHAR(STR(1:1)) .GE. 0  .AND. ICHAR(STR(1:1)) .LE. 32 ) THEN
                CALL CLRCH ( STR )
           END IF
           IF ( ILEN(STR) .EQ. 0 ) GOTO 910
!
! -------- Parsing it
!
           IST = INDEX ( STALET, STR(1:1) )
!
! -------- Was the letter code correct?
!
           IF ( IST .LE. 0   .OR.  IST .GT. MAX_ARC_STA ) THEN
                CALL CLRCH    ( OUT )
                CALL SETCR_MN ( 0, NL )
                CALL ADDSTR_F ( OUT )
!
                CALL SETCR_MN ( 0, NL )
                CALL REVERSE_ON_MN()
                CALL ADDSTR_F ( '$$$ Wrong letter has been entered: "'// &
     &                           STR(1:1)//'"               $$$' )
                CALL REVERSE_OFF_MN()
                CALL ADDSTR_F ( '   ' )
                CALL SENKR_MN ( IX, IY, CC4 )
                GOTO 950
           END IF
!
! -------- Does a priori model for that station already applied?
!
           IP = IFIND_PL ( L_ACM, STAIND_ACM_NEW, IST )
           IF ( IP .GT. 0 ) THEN
!
! ------------- Yes! Then ask user to confirm operation
!
                CALL SETCR_MN ( 0, NL )
                CALL ADDSTR_F ( 'Please confirm deletion a priori model for '// &
     &                           ISITN_CHR ( STAIND_ACM_NEW(IP) )// &
     &                          ' Y/N [N]  >> ' )
                CALL SENKR_MN ( IX, IY, CC4 )
                IF ( CC4(4:4) .EQ. 'Y' ) THEN
!
! ------------------ User confirmed it. Delete stration form the list
!
                     DO 460 J6=1,L_ACM-1
                        IF ( J6 .GE. IP ) THEN
                             STAIND_ACM_NEW(J6) = STAIND_ACM_NEW(J6+1)
                             CLOOF_ACM_NEW(J6)  = CLOOF_ACM_NEW(J6+1)
                             CLODR_ACM_NEW(J6)  = CLODR_ACM_NEW(J6+1)
                        END IF
 460                 CONTINUE
!
! ------------------ .... and decrement the counter of the list
!
                     L_ACM = L_ACM - 1
               END IF
             ELSE
!
! ------------- No, model was not applied for that station
!
                CALL CLRCH    ( OUT )
                CALL SETCR_MN ( 0, NL )
                CALL ADDSTR_F ( OUT )
!
                CALL SETCR_MN ( 0, NL )
                CALL REVERSE_ON_MN()
                CALL ADDSTR_F ( '$$$ A priori clock model was not applied '// &
     &                          'for '//ISITN_CHR(IST)//'   $$$' )
                CALL REVERSE_OFF_MN()
                CALL ADDSTR_F ( '   ' )
                CALL SENKR_MN ( IX, IY, CC4 )
                GOTO 950
           END IF
           WAS_CHANGE = .TRUE.
         ELSE IF ( SIM .EQ. 'H' ) THEN
!
! -------- Stopping curses
!
           CALL END_MN()
           CALL UN_CURSES ( )
!
! -------- Making file name with HELP information
!
           IM = MAKE_HELP_FINAM ( SETACM_HELP, FINAM )
           IF ( IM.NE.0 ) THEN
                CALL ERR_LOG ( 6805, -1, 'SET_ACM', 'Help file '// &
     &               SETACM_HELP//' is not found. Check directory '// &
     &               SOLVE_HELP_DIR//' and environment variable SOLVE_HELP_DIR' )
               CALL HIT_CONT ( '--- Hit any key to proceed --- '//CHAR(1), &
     &                         %VAL(0) )
               GOTO 910
           END IF
!
! -------- Displaying the buffer at the screen
!
           CALL SHOW_TEXT_FILE_COL ( FINAM, 'Set a priori clock model', 0, -3 )
!
! -------- Re-starting curses again
!
           CALL START_MN()
         ELSE IF ( SIM .EQ. 'O' ) THEN
!
! ========= Quit without saving
!
            IF ( WAS_CHANGE ) THEN
!
! -------------- Confirfm: do we really want to do it?
!
                 CALL CLRCH     (    OUT   )
                 CALL SETCR_MN  ( 0, NL    )
                 CALL ADDSTR_F  (    OUT   )
                 CALL SETCR_MN  ( 0, NL+1  )
                 CALL ADDSTR_F  (    OUT   )
!
                 CALL SETCR_MN  ( 0, NL    )
                 CALL ADDSTR_F ( 'Are you really going to discard your '// &
     &               'changes. Please answer  Y/N [N]  >> ' )
                 CALL SENKR_MN ( IX, IY, CC4 )
                 IF ( CC4(4:4) .NE. 'Y' ) GOTO 910
            END IF
            GOTO 810
         ELSE IF ( SIM .EQ. 'S' ) THEN
!
! ========= Quit with saving results
!
            IF ( WAS_CHANGE ) THEN
                 CALL CLRCH     (    OUT   )
                 CALL SETCR_MN  ( 0, NL    )
                 CALL ADDSTR_F  (    OUT   )
                 CALL SETCR_MN  ( 0, NL+1  )
                 CALL ADDSTR_F  (    OUT   )
!
                 CALL SETCR_MN  ( 0, NL    )
                 CALL ADDSTR_F  ( 'Scratch files are being updated... ' )
                 CALL SETCR_MN  ( 0, NL+1  )
                 CALL REFRESH_MN()
               DO 470 J7=1,NUMDB
!
! --------------- There were changes. Update ACM cards in NAMFIL
!
                  DO 480 J8=1,M_ACM
!
! ------------------ Putting stuff from local copy back
!
                     IF ( J8 .LE. L_ACM ) THEN
                          STAIND_ACM(J8) = STAIND_ACM_NEW(J8)
                          CLOOF_ACM(J8)  = CLOOF_ACM_NEW(J8)
                          CLODR_ACM(J8)  = CLODR_ACM_NEW(J8)
                         ELSE
                          STAIND_ACM(J8) = 0
                          CLOOF_ACM(J8)  = 0.0D0
                          CLODR_ACM(J8)  = 0.0D0
                     END IF
!
! ------------------ Preparing a card for PUTCARD
!
                     CALL CLRCH ( JBUF )
                     IF ( J8 .LE. L_ACM ) THEN
                          WRITE ( JBUF, &
     &                   '("ACM  ",I2,1X,I2,1X,I2,1X,A8,1X,D23.15,1X,D23.15)' ) &
     &                   M_ACM, L_ACM, J8, ISITN_CHR(STAIND_ACM(J8)), &
     &                                   CLOOF_ACM(J8), CLODR_ACM(J8)
                      ELSE
                          WRITE ( JBUF, &
     &                   '("ACM  ",I2,1X,I2,1X,I2,1X,A8,1X,D23.15,1X,D23.15)' ) &
     &                    M_ACM, L_ACM, J8, '        ', 0.0D0, 0.0D0
                     END IF
!
                     IF ( J8 .EQ. 1 ) THEN
                          ICONT_I2 = INT2(1)
                        ELSE
                          ICONT_I2 = INT2(0)
                     END IF
!
! ------------------ Putting a ACM card in NAMFIL
!
                     CALL PUTCARD ( INT2(J7), 'ACM ', ICONT_I2, JBUF, IERR_I2 )
                     IF ( IERR_I2  .EQ. INT2(-3)  .OR. &
     &                    IERR_I2  .EQ. INT2(-8)       ) THEN
!
! ----------------------- If status "Card type not found" return while we were
! ----------------------- writing the card then we try once more with
! ----------------------- anpther function controller
!
                          ICONT_I2 = INT2(2)
                          CALL PUTCARD ( INT2(J7), 'ACM ', ICONT_I2, JBUF, &
     &                                   IERR_I2 )
                     END IF
                     IF ( IERR_I2 .NE. INT2(0) ) THEN
                          WRITE ( 6, * ) ' j8=',j8,' icont_i2 = ',icont_i2, &
     &                                     ' ierr_i2=',ierr_i2
                          CALL FERR ( INT2(179), 'Error in attempt to '// &
     &                        'write ACM '//'card', INT2(0), INT2(0) )
                          CALL ERR_LOG ( 6806, -1, 'SET_ACM', 'Error in '// &
     &                                  'attempt to write ACM card in NAMFIL' )
                          CALL PAUSE ( 'SET_ACM' )
                     END IF
 480              CONTINUE
!
                CALL CLOSENAMFIL()
!
! ------------- Update of oborg area
!
                CALL ACS_OBSFIL ( 'O' )
                CALL OBSTM ( FJDOBS, LJDOBS )
                IF ( J7 .EQ. 1 ) THEN
                    IDB_BEG = 1
                  ELSE
                    IDB_BEG = IDBE(J7-1) + 1
               END IF
               IDB_END = IDBE(J7)
!
               DO 490 J9=IDB_BEG,IDB_END
!
! --------------- Reading oborg record
!
                  CALL USE_OBSFIL ( IOBSFIL, J9, 'R' )
!
! --------------- Time in seconds elapsed form reference time epoch of
! --------------- the first observation written in NAMFIL. Tricky point:
! --------------- we should add theoretical delay to the elapsed time when we
! --------------- handle the second station of the baseline, since time epoch
! --------------- for the second station differs from the time epoch of the
! --------------- first statoin by DT (NB: DT is in microseconds,
! --------------- TAU_ACM is in seconds )
!
                  TT  = ((FJD - FJDOBS) + FRACTC)*86400.0D0
                  TT2 = TT + (DT*1.D-6 - TAU_ACM)
!
! --------------- Initialiszation of the contributon of ACM to the current
! --------------- observation
!
                  TAU_ACM_NEW  = 0.0D0
                  RATE_ACM_NEW = 0.0D0
                  IF ( L_ACM .GT. 0 ) THEN
!
! -------------------- IP1 -- index of the fiest station of the baseine in
! -------------------- the list of stations for which a priori clock model
! -------------------- has been applied
!
                       IP1 = IFIND_PL ( L_ACM, STAIND_ACM_NEW, INT4(ISITE(1)) )
                       IF ( IP1 .GT. 0 ) THEN
!
! ------------------------- New correction of theoreticals due to up to date
! ------------------------- ACM
!
                            TAU_ACM_NEW  = TAU_ACM_NEW  - CLOOF_ACM_NEW(IP1) - &
     &                                                    CLODR_ACM_NEW(IP1)*TT
                            RATE_ACM_NEW = RATE_ACM_NEW - CLODR_ACM_NEW(IP1)
                       END IF
!
! -------------------- The same for the second station. But NB sign!
!
                       IP2 = IFIND_PL ( L_ACM, STAIND_ACM_NEW, INT4(ISITE(2)) )
                       IF ( IP2 .GT. 0 ) THEN
                            TAU_ACM_NEW  = TAU_ACM_NEW  + CLOOF_ACM_NEW(IP2) + &
     &                                                    CLODR_ACM_NEW(IP2)*TT2
                            RATE_ACM_NEW = RATE_ACM_NEW + CLODR_ACM_NEW(IP2)
                       END IF
                  END IF
!
! --------------- Correction for theoreticals. NB units for DT!
!
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!      type *,' j9=',j9,' tau_acm = ',tau_acm,' tau_acm_new = ',   ! %%%
!     #         tau_acm_new,' tt=',tt                              ! %%%
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
                  DT = ( DT*1.D-6 - TAU_ACM  + TAU_ACM_NEW  ) *1.D6
                  RT = ( RT       - RATE_ACM + RATE_ACM_NEW )
!
! --------------- Saving new contribution due to ACM in oborg area
!
                  TAU_ACM  = TAU_ACM_NEW
                  RATE_ACM = RATE_ACM_NEW
!
! --------------- Writing updated oborg record back to scratch file
!
                  CALL USE_OBSFIL ( IOBSFIL, J9, 'W' )
 490           CONTINUE
 470        CONTINUE
            CALL ACS_OBSFIL ( 'C' )
           END IF
!
           GOTO 810
         ELSE IF ( SIM .EQ. 'R' ) THEN
           GOTO 910
      END IF
      GOTO 910
!
 810  CONTINUE
      CALL CLOSENAMFIL()
!
      RETURN
      END  !#!  SET_ACM  #!#
