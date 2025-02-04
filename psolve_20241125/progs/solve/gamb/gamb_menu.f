      SUBROUTINE GAMB_MENU ( VER_GAMB, F_OPP_BAND, F_XBAND, F_SBAND, F_ION, &
     &           F_PREUSE, F_SAVE, CUTOFF, MINOBS, GAMB_CONST, IT, &
     &           QUALCODE_LIM, F_OPTIN )
! ************************************************************************
! *                                                                      *
! *   Routine  GAMB_MENU  gets parameters of group delay ambiguity       *
! *   resolution algorithm in interactive mode.                          *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *    VER_GAMB ( CHARACTER ) -- String with GAMB-iddentifier and number *
! *                              of the current version.                 *
! *  F_OPP_BAND ( LOGICAL*4 ) -- Flag: .TRUE. means that there is        *
! *                              information available about S-band      *
! *                              observables.                            *
! *                                                                      *
! * ________________________ Output parameters: ________________________ *
! *                                                                      *
! *     F_OPTIN ( LOGICAL*4 ) -- Flag: .TRUE. means not make ambiguity   *
! *                              resoltion, but go to OPTIN outright.    *
! *                                                                      *
! * _______________________ Modified parameters: _______________________ *
! *                                                                      *
! *     Input valiues will be put at screen form at the beginning.       *
! *     User can, of course change their values.                         *
! *                                                                      *
! *     F_XBAND ( LOGICAL*4 ) -- Flag: whether to analyze X-band?        *
! *     F_SBAND ( LOGICAL*4 ) -- Flag: whether to analyze S-band?        *
! *       F_ION ( LOGICAL*4 ) -- Flag: whether to calculate ionosphere   *
! *                              correction? It has sence only of        *
! *                              F_XBAND and F_SBAND are both .TRUE.,    *
! *                              otherwise it is ignored.                *
! *    F_PREUSE ( LOGICAL*4 ) -- Flag:  .TRUE. means that only           *
! *                              observations marked as "good" has been  *
! *                              put in GAMB data structures. .FALSE.    *
! *                              means that all observations has been    *
! *                              put in GAMB data structures.            *
! *      F_SAVE ( LOGICAL*4 ) -- Flag: whether to save values group delay*
! *                              ambiguities and outliers.               *
! *      CUTOFF ( REAL*8    ) -- Cutoff limit (in sec) for outliers      *
! *                              detection.                              *
! *      MINOBS ( INTEGER*4 ) -- Minimal acceptable numer of not         *
! *                              rejected observations aat one baseline  *
! *                              in order to count that baseline as      *
! *                              "good".                                 *
! *  GAMB_CONST ( REAL*8    ) -- Default for group delay ambiguity       *
! *                              spacing. If zero then the values from   *
! *                              database will be used. If non zero then *
! *                              it will superceed database constant.    *
! *         IT ( INTEGER*4 ) --  Verbosity level. 0 -- silient mode,     *
! *                              2 -- recommended level, 5 -- debugging  *
! *                              level.                                  *
! * QUALCODE_LIM ( INTEGER*4 ) - Mininal value of quality code when the  *
! *                              observation consider as a good one.     *
! *                              Letter-like quality codes ( "A", "B",   *
! *                              "C", "D", "E" are considered as         *
! *                              negative and therfore bad ).            *
! *                                                                      *
! *  ###  06-AUG-97    GAMB_MENU    v1.2 (c)  L. Petrov  09-MAR-98  ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT    NONE
      INCLUDE    'solve.i'
      LOGICAL*4   F_OPP_BAND, F_XBAND, F_SBAND, F_ION, F_PREUSE, F_SAVE, F_OPTIN
      REAL*8      CUTOFF, GAMB_CONST
      INTEGER*4   MINOBS, IT, QUALCODE_LIM
      CHARACTER   VER_GAMB*(*), CC4*4, SIM*1
      INTEGER*2   NUMDB, LDBNAM(5,15), IDBV(15)
      INTEGER*4   IDBE(15)
      CHARACTER   CDBNAM(15)*10
      EQUIVALENCE ( CDBNAM, LDBNAM(1,1) )
      INTEGER*4   IX, IY, NN, I_B, I_I, I_G, I_S, I5, IP, IVAL, &
     &            IT_MIN, IT_MAX, IL_SAVE
      REAL*8      VAL
      CHARACTER   DBASE*80, STR*80
      INTEGER*4,  EXTERNAL :: ILEN, I_LEN
      CHARACTER   BAND(3)*22, ION(2)*3, GOOD(2)*4, SAVE(2)*3
      DATA       ( BAND(NN), NN=1,3 ) &
     &          / &
     &          'X-band                ', &
     &          'S-band                ', &
     &          'X-band and S-band both' &
     &          /
      DATA      ( ION(NN), NN=1,2 ) &
     &          / &
     &          'Yes', &
     &          'No ' &
     &          /
      DATA      ( GOOD(NN), NN=1,2 ) &
     &          / &
     &          'Good', &
     &          'All ' &
     &          /
      DATA      ( SAVE(NN), NN=1,2 ) &
     &          / &
     &          'Yes', &
     &          'No ' &
     &          /
      PARAMETER  ( IT_MIN = 0, IT_MAX = 6 )
      INTEGER*2    INT2_ARG
      INTEGER*4    INT4
      INT4(INT2_ARG) = INT(INT2_ARG,KIND=4)
!
      F_OPTIN = .FALSE.
!
! --- Learn how many database are in oborg area and extracting their names
!
      CALL DBPOX ( NUMDB, LDBNAM, IDBV, IDBE )
      I_B = 1
      IF ( NUMDB .EQ. 0 ) THEN
           CALL ERR_LOG ( 7712, -1, 'GAMB_MENU', 'No database has been '// &
     &         'read into oborg area' )
           CALL HIT_CONT ( %VAL(0), %VAL(0) )
           RETURN
        ELSE IF ( NUMDB .EQ. 1 ) THEN
           CALL CLRCH ( DBASE )
!
           DBASE = 'Database in use: '//CDBNAM(1)(1:10)//' <'
           CALL INCH ( INT4(IDBV(1)), DBASE(ILEN(DBASE)+1:) )
           DBASE(ILEN(DBASE)+1:) = '>'
           IF ( CDBNAM(1)(9:9) .EQ. 'X' ) I_B = 1
           IF ( CDBNAM(1)(9:9) .EQ. 'S' ) I_B = 2
           IF ( F_XBAND  .AND.  F_SBAND  .AND.  F_OPP_BAND ) I_B = 3
        ELSE IF ( NUMDB .GE. 2 ) THEN
           CALL CLRCH ( DBASE )
           DBASE = 'Databases in use: '//CDBNAM(1)(1:10)//' <'
           CALL INCH ( INT4(IDBV(1)), DBASE(ILEN(DBASE)+1:) )
           DBASE(ILEN(DBASE)+1:) = '>,'
           DBASE(ILEN(DBASE)+2:) = CDBNAM(2)(1:10)//' <'
           CALL INCH ( INT4(IDBV(2)), DBASE(ILEN(DBASE)+1:) )
           DBASE(ILEN(DBASE)+1:) = '>'
           IF (       F_XBAND  .AND.  .NOT. F_SBAND  .AND. &
     &          .NOT. F_OPP_BAND                           ) I_B = 1
           IF ( .NOT. F_XBAND  .AND.        F_SBAND        ) I_B = 2
           IF (       F_XBAND  .AND.        F_SBAND        ) I_B = 3
      END IF
!
! --- Setting default values if integer switches.
!
      IF (       F_ION    ) I_I = 1
      IF ( .NOT. F_ION    ) I_I = 2
!
      IF (       F_PREUSE ) I_G = 1
      IF ( .NOT. F_PREUSE ) I_G = 2
!
      IF (       F_SAVE   ) I_S = 1
      IF ( .NOT. F_SAVE   ) I_S = 2
!CCCCC
!
! --- Start curser
!
!@         write( 6, * ) ' numdb=',numdb,' I_B=',I_B ! %%%
!@         call pause ( 'aa' ) ! %%
      CALL START_MN()
 910  CONTINUE
!
! --- Printing the first line: title of the program
!
      CALL CLEAR_MN()
      CALL SETCR_MN (  0, 0 )
      CALL ADDSTR_F ( 'Group delay ambiguities resolution' )
      CALL SETCR_MN (  79-ILEN(VER_GAMB), 0 )
      CALL REVERSE_ON_MN()
      CALL ADDSTR_F ( VER_GAMB )
      CALL REVERSE_OFF_MN()
      CALL NL_MN()
      CALL NL_MN()
!
! --- And then printing the menu
!
      CALL ADDSTR_F ( DBASE(1:I_LEN(DBASE)) )
      IF ( NUMDB .EQ. 1  .AND.  F_OPP_BAND ) THEN
           CALL ADDSTR_F ( '  there is information about S-band available' )
      END IF
      CALL NL_MN()
      CALL NL_MN()
!
      CALL ADDSTR_F ( '(H) On-line help' )
      CALL NL_MN()
      CALL NL_MN()
!
      CALL ADDSTR_F ( '(B) Which band ? ' )
      CALL REVERSE_ON_MN()
      CALL ADDSTR_F ( BAND(I_B)(1:I_LEN(BAND(I_B))) )
      CALL REVERSE_OFF_MN()
      CALL NL_MN()
      CALL NL_MN()
!
      CALL ADDSTR_F ( '(G) Use only good or all observations ? ' )
      CALL REVERSE_ON_MN()
      CALL ADDSTR_F ( GOOD(I_G)(1:I_LEN(GOOD(I_G))) )
      CALL REVERSE_OFF_MN()
      CALL NL_MN()
      CALL NL_MN()
!
      CALL ADDSTR_F ( '(S) Save information about ambiguities and outliers? ' )
      CALL REVERSE_ON_MN()
      CALL ADDSTR_F ( SAVE(I_S)(1:I_LEN(SAVE(I_S))) )
      CALL REVERSE_OFF_MN()
      CALL NL_MN()
      CALL NL_MN()
!
      IF ( I_B .EQ. 3 ) THEN
           CALL ADDSTR_F ( '(I) Calculate group delay ionosphere correction '// &
     &                'after ? ' )
           CALL REVERSE_ON_MN()
           CALL ADDSTR_F ( ION(I_I)(1:I_LEN(ION(I_I))) )
           CALL REVERSE_OFF_MN()
      END IF
      CALL NL_MN()
      CALL NL_MN()
!
      CALL ADDSTR_F ( '(M) Minimal acceptable number of good observations '// &
     &                'at one baseline: ' )
      CALL CLRCH ( STR )
      CALL INCH  ( MINOBS, STR )
      CALL REVERSE_ON_MN()
      CALL ADDSTR_F ( STR(1:I_LEN(STR)) )
      CALL REVERSE_OFF_MN()
      CALL NL_MN()
      CALL NL_MN()
!
      CALL ADDSTR_F ( '(C) Ambiguity spacing constant: ' )
      CALL CLRCH ( STR )
      IF ( GAMB_CONST .GT. 1.D-9 ) THEN
           WRITE ( UNIT=STR, FMT='(F18.5)' ) GAMB_CONST*1.D9
           IF ( STR(I_LEN(STR):I_LEN(STR)) .EQ. '0' ) &
     &          STR(I_LEN(STR):I_LEN(STR)) = ' '
           CALL CHASHL ( STR )
           STR = STR(1:I_LEN(STR))//' nsec'
         ELSE
           STR = 'as saved in database'
      END IF
      CALL REVERSE_ON_MN()
      CALL ADDSTR_F ( STR(1:I_LEN(STR)) )
      CALL REVERSE_OFF_MN()
      CALL NL_MN()
      CALL NL_MN()
!
      CALL ADDSTR_F ( '(L) Cutoff limit: ' )
      CALL CLRCH ( STR )
      WRITE ( UNIT=STR, FMT='(F15.2)' ) CUTOFF*1.D9
      CALL CHASHL ( STR )
      IF ( STR(1:1) .EQ. '.' ) STR='0'//STR
      IF ( STR(I_LEN(STR):I_LEN(STR)) .EQ. '0' ) STR(I_LEN(STR):I_LEN(STR))=' '
      CALL REVERSE_ON_MN()
      CALL ADDSTR_F ( STR(1:I_LEN(STR))//' nsec' )
      CALL REVERSE_OFF_MN()
      IL_SAVE = I_LEN(STR)
      CALL CLRCH ( STR )
      CALL ADDSTR_F ( STR(1:15-IL_SAVE)//'  (Q) Quality code limit: ' )
      CALL INCH  ( QUALCODE_LIM, STR )
      CALL REVERSE_ON_MN()
      CALL ADDSTR_F ( STR(1:I_LEN(STR)) )
      CALL REVERSE_OFF_MN()
      CALL NL_MN()
      CALL NL_MN()
!
      CALL ADDSTR_F ( '(A) Do group ambiguities resolution' )
      CALL ADDSTR_F ( '     ' )
      CALL ADDSTR_F ( '(V) Verbosity mode: ' )
      CALL CLRCH ( STR )
      CALL INCH  ( IT, STR )
      CALL REVERSE_ON_MN()
      CALL ADDSTR_F ( STR(1:I_LEN(STR)) )
      CALL REVERSE_OFF_MN()
      CALL NL_MN()
      CALL NL_MN()
!
      CALL ADDSTR_F ( '(O) return to Optin ' )
      CALL ADDSTR_F ( '                    ' )
      CALL ADDSTR_F ( '(R) Refresh screen' )
      CALL SETCR_MN ( 1, 20 )
!CCCCC
        write ( 6, * ) ' a2' ! %%%%%%%
!
! --- Awaiting for entering information
!
      CALL SENKR_MN ( IX, IY, CC4 )
      SIM = CC4(4:4)
      CALL TRAN ( 11, SIM, SIM )
!
! --- TRansforming cursor coordinamtes in letters
!
      IF ( SIM .EQ. ' '  .OR.  SIM .EQ. CHAR(13) ) THEN
           IF ( IY .EQ.  4 ) SIM = 'H'
           IF ( IY .EQ.  6 ) SIM = 'B'
           IF ( IY .EQ.  8 ) SIM = 'G'
           IF ( IY .EQ. 10 ) SIM = 'S'
           IF ( IY .EQ. 12 ) SIM = 'I'
           IF ( IY .EQ. 14 ) SIM = 'M'
           IF ( IY .EQ. 16 ) SIM = 'C'
           IF ( IY .EQ. 18  .AND.  IX .LT. 40 ) SIM = 'L'
           IF ( IY .EQ. 18  .AND.  IX .GE. 40 ) SIM = 'Q'
           IF ( IY .EQ. 20  .AND.  IX .LT. 40 ) SIM = 'A'
           IF ( IY .EQ. 20  .AND.  IX .GE. 40 ) SIM = 'V'
           IF ( IY .EQ. 22  .AND.  IX .LT. 40 ) SIM = 'O'
           IF ( IY .EQ. 22  .AND.  IX .GE. 40 ) SIM = 'R'
      END IF
!
! --- Now reacting on user's input
!
      IF ( SIM .EQ. 'H' ) THEN
           CALL GAMB_HELP ( VER_GAMB )
           CALL REFRESH_MN()
           GOTO 910
        ELSE IF ( SIM .EQ. 'B' ) THEN
!
! -------- Changing band
!
           IF ( NUMDB .EQ. 2 ) THEN
                IF ( I_B .EQ. 1        ) THEN
                     I_B = 2
                  ELSE IF ( I_B .EQ. 2 ) THEN
                     I_B = 3
                  ELSE IF ( I_B .EQ. 3 ) THEN
                     I_B = 1
                END IF
             ELSE IF ( NUMDB .EQ. 1  .AND.  F_OPP_BAND ) THEN
                IF ( I_B .EQ. 1        ) THEN
                     I_B = 2
                  ELSE IF ( I_B .EQ. 2 ) THEN
                     I_B = 3
                  ELSE IF ( I_B .EQ. 3 ) THEN
                     I_B = 1
                END IF
           END IF
        ELSE IF ( SIM .EQ. 'G' ) THEN
!
! -------- Changing "good" <---> "all" observation selection
!
           IF ( I_G .EQ. 1 ) THEN
                I_G = 2
             ELSE IF ( I_G .EQ. 2 ) THEN
                I_G = 1
           END IF
        ELSE IF ( SIM .EQ. 'S' ) THEN
!
! -------- Changing "save" <---> "not to save" post-action
!
           IF ( I_S .EQ. 1 ) THEN
                I_S = 2
             ELSE IF ( I_S .EQ. 2 ) THEN
                I_S = 1
           END IF
        ELSE IF ( SIM .EQ. 'I' ) THEN
!
! -------- Switching option to make ionospere calibration
!
           IF ( I_I .EQ. 1 ) THEN
                I_I = 2
             ELSE IF ( I_I .EQ. 2 ) THEN
                I_I = 1
           END IF
        ELSE IF ( SIM .EQ. 'M' ) THEN
!
! -------- Resetting minimal number of observations at one baseline
!
           IP = 14
 920       CONTINUE
           CALL SETCR_MN ( 0, IP )
!
! -------- Clearing the line
!
           CALL ADDSTR_F ( "                                        "// &
     &                     "                                        " )
!
! -------- Printout of prompt
!
           CALL SETCR_MN ( 0, IP )
           CALL ADDSTR_F ( 'Enter min acceptable number of '// &
     &                'observations at one baseline: [4, 1000]: ' )
!
! -------- Entering the string STR
!
           CALL CLRCH ( STR )
           CALL GETSTR_F ( STR )
           IF ( ILEN(STR) .EQ. 0 ) GOTO 910
!
! -------- Type transformation CHARACTER  -->  INTEGER*4
!
           READ ( STR, FMT=*, IOSTAT=I5 ) IVAL
           IF ( I5 .NE. 0 ) THEN
!
! ------------- Error in type transformation
!
                CALL SETCR_MN ( 0, IP )
                CALL ADDSTR_F ( "                                        "// &
     &                          "                                       " )
                CALL SETCR_MN ( 0, IP )
                CALL ADDSTR_F ( '$$$   "'//STR(1:I_LEN(STR))//'" is not the '// &
     &              'variable of INTEGER type. Hit any key to proceed. ' )
                CALL SENKR_MN ( IX, IY, CC4 )
                GOTO 920
           END IF
           IF ( IVAL .LT. 1  .OR.  IVAL .GT. 1000 ) THEN
!
! ------------- Value out of range
!
                CALL SETCR_MN ( 0, IP )
                CALL ADDSTR_F ( "                                        "// &
     &                          "                                       " )
                CALL SETCR_MN ( 0, IP )
                CALL ADDSTR_F ( '$$$   '//STR(1:I_LEN(STR))// &
     &                          ' is out of range [4, 1000]. '// &
     &                          'Hit any key to proceed. ' )
                CALL SENKR_MN ( IX, IY, CC4 )
                GOTO 920
           END IF
!
! -------- Substitution to the new value
!
           MINOBS = IVAL
        ELSE IF ( SIM .EQ. 'C' ) THEN
!
! -------- Resetting group delay ambiguty spacing
!
           IP = 16
 930       CONTINUE
           CALL SETCR_MN ( 0, IP )
!
! -------- Clearing the line
!
           CALL ADDSTR_F ( "                                        "// &
     &                     "                                        " )
!
! -------- Printout of prompt
!
           CALL SETCR_MN ( 0, IP )
           CALL ADDSTR_F ( 'Enter ambiguities spacing in range [1,1000] nsec '// &
     &                'or 0 (default) : ' )
!
! -------- Entering the string STR
!
           CALL CLRCH ( STR )
           CALL GETSTR_F ( STR )
           IF ( ILEN(STR) .EQ. 0 ) GOTO 910
!
! -------- Type transformation CHARACTER  -->  REAL*8
!
           READ ( STR, FMT='(F12.6)', IOSTAT=I5 ) VAL
           IF ( I5 .NE. 0 ) THEN
!
! ------------- Error in type transformation
!
                CALL SETCR_MN ( 0, IP )
                CALL ADDSTR_F ( "                                        "// &
     &                          "                                       " )
                CALL SETCR_MN ( 0, IP )
                CALL ADDSTR_F ( '$$$   "'//STR(1:I_LEN(STR))//'" is not the '// &
     &              'variable of REAL type. Hit any key to proceed. ' )
                CALL SENKR_MN ( IX, IY, CC4 )
                GOTO 930
           END IF
           IF ( ( VAL .LT. 1.D0  .OR.  VAL .GT. 1000.D0 ) .AND. &
     &            DABS(VAL) .GT. 1.D-8                          ) THEN
!
! ------------- Value out of range
!
                CALL SETCR_MN ( 0, IP )
                CALL ADDSTR_F ( "                                        "// &
     &                          "                                       " )
                CALL SETCR_MN ( 0, IP )
                CALL ADDSTR_F ( '$$$   '//STR(1:I_LEN(STR))// &
     &                          ' is out of range [1.0, 1000.0]. '// &
     &                          'Hit any key to proceed. ' )
                CALL SENKR_MN ( IX, IY, CC4 )
                GOTO 930
           END IF
!
! -------- Substitution to the new value
!
           GAMB_CONST = VAL*1.D-9
        ELSE IF ( SIM .EQ. 'L' ) THEN
!
! -------- Resetting cutoff limit
!
           IP = 18
 940       CONTINUE
           CALL SETCR_MN ( 0, IP )
!
! -------- Clearing the line
!
           CALL ADDSTR_F ( "                                        "// &
     &                     "                                        " )
!
! -------- Printout of prompt
!
           CALL SETCR_MN ( 0, IP )
           CALL ADDSTR_F ( "Enter cutoff limit (in nanoseconds) "// &
     &                     "in range [0.1, 1000]: " )
!
! -------- Entering the string STR
!
           CALL CLRCH ( STR )
           CALL GETSTR_F ( STR )
           IF ( ILEN(STR) .EQ. 0 ) GOTO 910
!
! -------- Type transformation CHARACTER  -->  REAL*8
!
           READ ( STR, FMT=*, IOSTAT=I5 ) VAL
           IF ( I5 .NE. 0 ) THEN
!
! ------------- Error in type transformation
!
                CALL SETCR_MN ( 0, IP )
                CALL ADDSTR_F ( "                                        "// &
     &                          "                                       " )
                CALL SETCR_MN ( 0, IP )
                CALL ADDSTR_F ( '$$$   "'//STR(1:I_LEN(STR))//'" is not the '// &
     &              'variable of REAL type. Hit any key to proceed. ' )
                CALL SENKR_MN ( IX, IY, CC4 )
                GOTO 940
           END IF
           IF ( VAL .LT. 0.1D0  .OR.  VAL .GT. 1000.D0 ) THEN
!
! ------------- Value out of range
!
                CALL SETCR_MN ( 0, IP )
                CALL ADDSTR_F ( "                                        "// &
     &                          "                                       " )
                CALL SETCR_MN ( 0, IP )
                CALL ADDSTR_F ( '$$$   '//STR(1:I_LEN(STR))// &
     &                          ' is out of range [0.1, 1000.0]. '// &
     &                          'Hit any key to proceed. ' )
                CALL SENKR_MN ( IX, IY, CC4 )
                GOTO 940
           END IF
!
! -------- Substitution to the new value
!
           CUTOFF = VAL*1.D-9
        ELSE IF ( SIM .EQ. 'Q' ) THEN
!
! -------- Resetting minimal number of observations at one baseline
!
           IP = 18
 950       CONTINUE
           CALL SETCR_MN ( 0, IP )
!
! -------- Clearing the line
!
           CALL ADDSTR_F ( "                                        "// &
     &                     "                                        " )
!
! -------- Printout of prompt
!
           CALL SETCR_MN ( 0, IP )
           CALL ADDSTR_F ( 'Enter min quality good which allow to count '// &
     &                'an observation as good: [1, 9]: ' )
!
! -------- Entering the string STR
!
           CALL CLRCH ( STR )
           CALL GETSTR_F ( STR )
           IF ( ILEN(STR) .EQ. 0 ) GOTO 910
!
! -------- Type transformation CHARACTER  -->  INTEGER*4
!
           READ ( STR, FMT=*, IOSTAT=I5 ) IVAL
           IF ( I5 .NE. 0 ) THEN
!
! ------------- Error in type transformation
!
                CALL SETCR_MN ( 0, IP )
                CALL ADDSTR_F ( "                                        "// &
     &                          "                                       " )
                CALL SETCR_MN ( 0, IP )
                CALL ADDSTR_F ( '$$$   "'//STR(1:I_LEN(STR))//'" is not the '// &
     &              'variable of INTEGER type. Hit any key to proceed. ' )
                CALL SENKR_MN ( IX, IY, CC4 )
                GOTO 950
           END IF
           IF ( IVAL .LT. 1  .OR.  IVAL .GT. 9 ) THEN
!
! ------------- Value out of range
!
                CALL SETCR_MN ( 0, IP )
                CALL ADDSTR_F ( "                                        "// &
     &                          "                                       " )
                CALL SETCR_MN ( 0, IP )
                CALL ADDSTR_F ( '$$$   '//STR(1:I_LEN(STR))// &
     &                          ' is out of range [1, 9]. '// &
     &                          'Hit any key to proceed. ' )
                CALL SENKR_MN ( IX, IY, CC4 )
                GOTO 950
           END IF
!
! -------- Substitution to the new value
!
           QUALCODE_LIM = IVAL
        ELSE IF ( SIM .EQ. 'A' ) THEN
!
! -------- Exit from the routine to "Arbeit" (make (A)mbiguity resolition)
!
           GOTO 810
        ELSE IF ( SIM .EQ. 'V' ) THEN
!
! -------- New version of cycling verbosity mode switching
!
           IF ( IT .GE. IT_MAX ) THEN
                IT = IT_MIN
              ELSE
                IT = IT + 1
           END IF
        ELSE IF ( SIM .EQ. 'v' ) THEN ! specially done to disavle this branch
!
! -------- Resetting minimal number of observations at one baseline
!
           IP = 20
 960       CONTINUE
           CALL SETCR_MN ( 0, IP )
!
! -------- Clearing the line
!
           CALL ADDSTR_F ( "                                        "// &
     &                     "                                        " )
!
! -------- Printout of prompt
!
           CALL SETCR_MN ( 0, IP )
           CALL ADDSTR_F ( 'Enter verbosity mode in range [0, 6]: ' )
!
! -------- Entering the string STR
!
           CALL CLRCH ( STR )
           CALL GETSTR_F ( STR )
           IF ( ILEN(STR) .EQ. 0 ) GOTO 910
!
! -------- Type transformation CHARACTER  -->  INTEGER*4
!
           READ ( STR, FMT=*, IOSTAT=I5 ) IVAL
           IF ( I5 .NE. 0 ) THEN
!
! ------------- Error in type transformation
!
                CALL SETCR_MN ( 0, IP )
                CALL ADDSTR_F ( "                                        "// &
     &                          "                                       " )
                CALL SETCR_MN ( 0, IP )
                CALL ADDSTR_F ( '$$$   "'//STR(1:I_LEN(STR))//'" is not the '// &
     &              'variable of INTEGER type. Hit any key to proceed. ' )
                CALL SENKR_MN ( IX, IY, CC4 )
                GOTO 960
           END IF
           IF ( IVAL .LT. 0  .OR.  IVAL .GT. IT_MAX ) THEN
!
! ------------- Value out of range
!
                CALL SETCR_MN ( 0, IP )
                CALL ADDSTR_F ( "                                        "// &
     &                          "                                       " )
                CALL SETCR_MN ( 0, IP )
                CALL ADDSTR_F ( '$$$   '//STR(1:I_LEN(STR))// &
     &                          ' is out of range [0, 6]. '// &
     &                          'Hit any key to proceed. ' )
                CALL SENKR_MN ( IX, IY, CC4 )
                GOTO 960
           END IF
!
! -------- Substitution to the new value
!
           IT = IVAL
        ELSE IF ( SIM .EQ. 'R' ) THEN
!
! -------- Refresh screen
!
           GOTO 910
        ELSE IF ( SIM .EQ. 'O' ) THEN
!
! -------- exit from the routin to "Otdyhat'" -- go to OPTIN without group delay
! -------- ambiguities resolution.
!
           F_OPTIN = .TRUE.
           GOTO 810
      END IF
      GOTO 910
!
 810  CONTINUE
      CALL CLEAR_MN ()
      CALL END_MN()
      CALL UN_CURSES() ! Elimination of the influence of curses
      CALL CLEAR ( 0, 0 )
!
! --- Setting oputput values on the vase of entered values
!
      F_XBAND = .FALSE.
      F_SBAND = .FALSE.
      IF ( I_B .EQ. 1 .OR. I_B .EQ. 3 ) F_XBAND = .TRUE.
      IF ( I_B .EQ. 2 .OR. I_B .EQ. 3 ) F_SBAND = .TRUE.
!
      IF ( I_I .EQ. 1 ) F_ION = .TRUE.
      IF ( I_I .EQ. 2 ) F_ION = .FALSE.
!
      IF ( I_G .EQ. 1 ) F_PREUSE = .TRUE.
      IF ( I_G .EQ. 2 ) F_PREUSE = .FALSE.
!
      IF ( I_S .EQ. 1 ) F_SAVE = .TRUE.
      IF ( I_S .EQ. 2 ) F_SAVE = .FALSE.
!
! --- Saving menu values in common area.
!
      CALL GAMB_MENU_SAVE ( F_XBAND, F_SBAND, F_ION, F_PREUSE, F_SAVE, &
     &                      CUTOFF, MINOBS, GAMB_CONST, IT, QUALCODE_LIM )
!
      RETURN
      END  !#!  GAMB_MENU  #!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE GAMB_MENU_SAVE ( F_XBAND, F_SBAND, F_ION, F_PREUSE, F_SAVE, &
     &           CUTOFF, MINOBS, GAMB_CONST, IT, QUALCODE_LIM )
! ************************************************************************
! *                                                                      *
! *   Auxiliary routine for copying and saving parameters in glbc4       *
! *   common area.                                                       *
! *                                                                      *
! *  ###  07-AUG-97 GAMB_MENU_SAVE v1.0  (c)  L. Petrov  18-AUG-97  ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'solve.i'
      INCLUDE   'glbc4.i'
      LOGICAL*4  F_XBAND, F_SBAND, F_ION, F_PREUSE, F_SAVE
      INTEGER*4  MINOBS, IT, QUALCODE_LIM
      REAL*8     CUTOFF, GAMB_CONST
!
      GAMB_F_X_BAND      = F_XBAND
      GAMB_F_S_BAND      = F_SBAND
      GAMB_F_ION         = F_ION
      GAMB_F_PREUSE      = F_PREUSE
      GAMB_F_SAVE        = F_SAVE
      GAMB_CUTOFF        = CUTOFF
      GAMB_MINOBS        = MINOBS
      GAMB_SPACING_CONST = GAMB_CONST
      GAMB_IT            = IT
      QUALCODE_GOOD_LIM  = QUALCODE_LIM
      CALL USE_GLBFIL_4 ( 'OWC' )
!
      RETURN
      END  !#!  GAMB_MENU_SAVE  #!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE GAMB_HELP ( VER_GAMB )
! ************************************************************************
! *                                                                      *
! *   Ancillary siutine GAMB_HELP print on the screen on-line help       *
! *   information about GAMB routine.                                    *
! *                                                                      *
! *  ###  20-AUG-97    GAMB_HELP   v1.1  (c)  L. Petrov  24-AUG-97  ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT    NONE
      INCLUDE   'solve.i'
      INCLUDE   'help.i'
      INCLUDE   'gamb.i'
      INCLUDE   'glbc4.i'
      INTEGER*4  IUER, MBUF, NBUF, ISIM, J1, IMEN, ICOL, ILIN
      PARAMETER  ( MBUF = 1024 )
      CHARACTER  VER_GAMB*(*), BUF(MBUF)*160, ASIM*1, FINAM*255, &
     &           SOLVE_PS_VIEWER_USE*128
      CHARACTER  PRE_INIT*32, POST_INIT*32, PREF*32, ESC*1
      INTEGER*4  IT, IG, IP, IST, IM
      INTEGER*4  ILEN, I_LEN, SYSTEM, MAKE_HELP_FINAM
!
      CALL GETENVAR ( 'SOLVE_PS_VIEWER', SOLVE_PS_VIEWER_USE )
      IF ( ILEN(SOLVE_PS_VIEWER_USE) == 0 ) THEN
           SOLVE_PS_VIEWER_USE = SOLVE_PS_VIEWER
      END IF
!
      ESC = CHAR(27)
!
! --- Stopping curses
!
      CALL END_MN()
!
! --- And elimination of the influence of curses
!
      CALL UN_CURSES() ! Elimination of the influence of curses
      CALL CLEAR ( 0, 0 )
!
! --- Make filename with help menu
!
      IM = MAKE_HELP_FINAM ( GAMB_HELP_00, FINAM )
      IF ( IM.NE.0 ) THEN
           CALL ERR_LOG ( 7201, -1, 'GAMB_HELP', 'Help file '// &
     &          GAMB_HELP_00//' is not found. Check directory '// &
     &          SOLVE_HELP_DIR//' and environment variable SOLVE_HELP_DIR' )
           CALL HIT_CONT ( '--- Hit any key to proceed --- '//CHAR(1), %VAL(0) )
           GOTO 810
      END IF
!
! --- Reading file with help menu
!
      IUER = -1
      CALL RD_TEXT ( FINAM, MBUF, BUF, NBUF, IUER )
      IF ( IUER .NE. 0 ) THEN
           CALL ERR_LOG ( 7202, -1, 'GAMB_HELP', 'Error during openning '// &
     &         'file '//FINAM(1:I_LEN(FINAM))//' with help information' )
           CALL HIT_CONT ( '--- Hit any key to proceed --- '//CHAR(1), %VAL(0) )
           GOTO 810
      END IF
!
! --- Getting terminal size
!
      CALL TERM_SIZE ( ILIN, ICOL )
!
! --- Setting esc-sequences changing the colour of the terminal (in hpterm mode)
!
      CALL CLRCH ( PRE_INIT  )
      CALL CLRCH ( POST_INIT )
      CALL CLRCH ( PREF      )
      PRE_INIT  = ESC//'&v0m0.41x0.76y0.39z2I'
      POST_INIT = ESC//'&v0m1b1x1y1z2I'
      PREF = ESC//'&v2S'
      CALL SHOW_IO ( IT, IG, IP, IST, %VAL(0) )
      IF ( IT .EQ. 6 ) CALL PRCH ( PRE_INIT )
!
! --- Dusplaying help-menu
!
      DO 410 J1=1,NBUF
         IF ( J1 .EQ. 1 ) THEN
!
! ----------- Adding version date
!
              BUF(1)(ICOL-ILEN(VER_GAMB):) = VER_GAMB
            ELSE
         END IF
         IF ( IT.EQ.6 ) THEN
              CALL ADR_CURSOR  ( J1, 1 )
              CALL PRCH ( PREF )
              CALL PRCH ( BUF(J1)(1:ICOL-1)//CHAR(13) )
           ELSE
              WRITE ( 6, FMT='(A)' ) BUF(J1)(1:I_LEN(BUF(J1)))
         END IF
 410  CONTINUE
      IF ( IT .EQ. 6 ) CALL PRCH ( PREF(1:ILEN(PREF))//CHAR(13)//CHAR(10) )
!
! --- Awaiting user action
!
      CALL INSIM ( ASIM, ISIM )
!
! --- Unsetting clour changes
!
      IF ( IT .EQ. 6 ) CALL PRCH ( POST_INIT )
      IMEN = 1
      IF ( ASIM .EQ. '2' ) IMEN = 2
      IF ( ASIM .EQ. '3' ) IMEN = 3
      IF ( ASIM .EQ. '4' ) IMEN = 4
!
! --- Clearing display ...
!
      CALL CLEAR ( 0, 0 )
!
! --- And different actions
!
      IF ( IMEN .EQ. 1 ) THEN
!
! -------- Displaying 1-st menu item
!
           IM = MAKE_HELP_FINAM ( GAMB_HELP_01, FINAM )
           IF ( IM.NE.0 ) THEN
                CALL ERR_LOG ( 7203, -1, 'GAMB_HELP', 'Help file '// &
     &               GAMB_HELP_01//' is not found. Check directory '// &
     &               SOLVE_HELP_DIR//' and environment variable SOLVE_HELP_DIR' )
                CALL HIT_CONT ( '--- Hit any key to proceed --- '//CHAR(1), &
     &                          %VAL(0) )
                GOTO 810
           END IF
!
           IUER = -1
           CALL SHOW_TEXT_FILE_COL ( FINAM, 'description of GAMB items menu', &
     &                               1, IUER )
!
           IF ( IUER .NE. 0 ) THEN
                CALL ERR_LOG ( 7204, -1, 'GAMB_HELP', 'Error during openning '// &
     &              'file '//FINAM(1:I_LEN(FINAM))//' with help information' )
                CALL HIT_CONT ( '--- Hit any key to proceed --- '//CHAR(1), &
     &                           %VAL(0) )
                GOTO 810
           END IF
        ELSE IF ( IMEN .EQ. 2 ) THEN
!
! -------- Displaying 2-nd menu item
!
           IM = MAKE_HELP_FINAM ( GAMB_HELP_02, FINAM )
           IF ( IM.NE.0 ) THEN
                CALL ERR_LOG ( 7205, -1, 'GAMB_HELP', 'Help file '// &
     &               GAMB_HELP_02//' is not found. Check directory '// &
     &               SOLVE_HELP_DIR//' and environment variable SOLVE_HELP_DIR' )
                CALL HIT_CONT ( '--- Hit any key to proceed --- '//CHAR(1), &
     &                          %VAL(0) )
                GOTO 810
           END IF
!
           IUER = -1
           CALL SHOW_TEXT_FILE_COL ( FINAM, 'brief of description of GAMB '// &
     &                              'algorithm', 1, IUER )
           IF ( IUER .NE. 0 ) THEN
                CALL ERR_LOG ( 7206, -1, 'GAMB_HELP', 'Error during openning '// &
     &              'file '//FINAM(1:I_LEN(FINAM))//' with help information' )
                CALL HIT_CONT ( '--- Hit any key to proceed --- '//CHAR(1), &
     &                          %VAL(0) )
                GOTO 810
           END IF
         ELSE IF ( IMEN .EQ. 3 ) THEN
!
! -------- Displaying 3-rd menu item (in PostScript mode)
!
           IM = MAKE_HELP_FINAM ( GAMB_HELP_03, FINAM )
           IF ( IM.NE.0 ) THEN
                CALL ERR_LOG ( 7207, -1, 'GAMB_HELP', 'Help file '// &
     &               GAMB_HELP_03//' is not found. Check directory '// &
     &               SOLVE_HELP_DIR//' and environment variable SOLVE_HELP_DIR' )
                CALL HIT_CONT ( '--- Hit any key to proceed --- '//CHAR(1), &
     &                           %VAL(0) )
                GOTO 810
           END IF
!
! -------- Launching Postscript previewer
!
           WRITE ( 6, FMT='(A)' ) 'Scheduling '// &
     &            SOLVE_PS_VIEWER_USE(1:I_LEN(SOLVE_PS_VIEWER_USE))//' '// &
     &            FINAM(1:I_LEN(FINAM))//' ...'
           IP = SYSTEM ( SOLVE_PS_VIEWER_USE(1:I_LEN(SOLVE_PS_VIEWER_USE))//' '// &
     &                   FINAM(1:I_LEN(FINAM))//CHAR(0) )
           IF ( IP .EQ. 32512 ) THEN
                CALL ERR_LOG ( 7208, -1, 'GAMB_HELP', 'Environment '// &
     &              'variable SHELL has wrong value. Error in running Shell '// &
     &              'command: '//SOLVE_PS_VIEWER_USE(1:I_LEN(SOLVE_PS_VIEWER_USE))// &
     &              ' '//FINAM )
             ELSE IF ( IP .NE. 0 ) THEN
                CALL ERR_LOG ( 7209, -1, 'GAMB_HELP', 'Error in running Shell'// &
     &              ' command: '//SOLVE_PS_VIEWER_USE(1:I_LEN(SOLVE_PS_VIEWER_USE))// &
     &              ' '//FINAM )
           END IF
           CALL HIT_CONT ( %VAL(0), %VAL(0) )
        ELSE IF ( IMEN .EQ. 4 ) THEN
!
! -------- Displaying 4-th menu item
!
           IM = MAKE_HELP_FINAM ( GAMB_HELP_04, FINAM )
           IF ( IM.NE.0 ) THEN
                CALL ERR_LOG ( 7210, -1, 'GAMB_HELP', 'Help file '// &
     &               GAMB_HELP_04//' is not found. Check directory '// &
     &               SOLVE_HELP_DIR//' and environment variable SOLVE_HELP_DIR' )
                CALL HIT_CONT ( '--- Hit any key to proceed --- '//CHAR(1), &
     &                           %VAL(0) )
                GOTO 810
           END IF
!
           IUER = -1
           CALL SHOW_TEXT_FILE_COL ( FINAM, 'GAMB release message', 1, IUER )
           IF ( IUER .NE. 0 ) THEN
                CALL ERR_LOG ( 7211, -1, 'GAMB_HELP', 'Error during openning '// &
     &              'file '//FINAM(1:I_LEN(FINAM))//' with help information' )
                CALL HIT_CONT ( '--- Hit any key to proceed --- '//CHAR(1), &
     &                           %VAL(0) )
                GOTO 810
           END IF
      END IF
!
! --- Good bye
!
 810  CONTINUE
!
! --- Clearing display
!
      CALL CLEAR ( 0, 0 )
!
! --- Starting curses again
!
      CALL START_MN()
      RETURN
      END  !#!  GAMB_HELP  #!#
