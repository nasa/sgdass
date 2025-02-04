      SUBROUTINE PAMB_MENU ( VER_PAMB, DBNAME, IDATYP, OPP_STATUS, IACT, &
     &                       PAMB_VER, F_OPTIN )
! ************************************************************************
! *                                                                      *
! *     Routine  PAMB_MENU  gets parameters of phase delay ambiguity     *
! *   resolution algorithm in interactive mode, get code of the  PAMB    *
! *   procedure which is assumed to be run after leaving menu.           *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *    VER_PAMB ( CHARACTER ) -- String with PAMB-identifier and number  *
! *                              of the current version.                 *
! *      DBNAME ( CHARACTER ) -- Database name + version number.         *
! *      IDATYP ( INTEGER*2 ) -- Code of solution type. Codes are in     *
! *                              solve.i                                 *
! *  OPP_STATUS ( INTEGER*2 ) -- Code of the status of the opposite band.*
! *                              Codes are in socom.i                    *
! *                                                                      *
! * ________________________ Output parameters: ________________________ *
! *                                                                      *
! *        IACT ( INTEGER*4 ) -- Code of the procedure to be run after   *
! *                              leaving menu.                           *
! *    PAMB_VER ( INTEGER*4 ) -- Verbosity level for interactive PAMB    *
! *                              procedures.                             *
! *     F_OPTIN ( LOGICAL*4 ) -- Flag: .TRUE. means not make ambiguity   *
! *                              resolution, but go to OPTIN outright.   *
! *                                                                      *
! *  ###  07-NOV-1997  PAMB_MENU  v4.2 (c)  L. Petrov  19-DEC-1998  ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT    NONE
      INCLUDE    'solve.i'
      INCLUDE    'pamb.i'
      INTEGER*4   IACT, PAMB_VER, PAMB_PLOT_BAND, PAMB_PLOT_TYPE
      INTEGER*2   IDATYP, OPP_STATUS
      CHARACTER   VER_PAMB*(*), DBNAME*(*), CC4*4, SIM*1
      LOGICAL*4   F_OPTIN
!
      INTEGER*4   IX, IY, IL
      CHARACTER   STR*80, SOLTYP*32
      LOGICAL*2,  EXTERNAL :: KBIT
      INTEGER*4,  EXTERNAL :: ILEN, I_LEN
!
      IACT = 0
      F_OPTIN = .FALSE.
!CCCCC
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
      CALL ADDSTR_F ( 'Phase delay ambiguities resolution' )
      CALL SETCR_MN (  79-ILEN(VER_PAMB), 0 )
      CALL REVERSE_ON_MN()
      CALL ADDSTR_F ( VER_PAMB )
      CALL REVERSE_OFF_MN()
      CALL NL_MN()
!
! --- Decoding solution type
!
      CALL CLRCH ( SOLTYP )
      CALL DATYP_SHOW ( IDATYP, SOLTYP )
      CALL ADDSTR_F   ( 'Solution type for producing residuals:  '//SOLTYP )
      CALL NL_MN()
!
! --- And then printing the menu
!
      CALL ADDSTR_F ( 'Database in use: '//DBNAME )
!
      IF ( KBIT ( OPP_STATUS, OPP_SET1__BIT )  .AND. &
     &     KBIT ( OPP_STATUS, OPP_SET2__BIT )         ) THEN
           CALL ADDSTR_F ( '       There is full information about S-band' )
         ELSE
           CALL ADDSTR_F ( '       No S-band information is available' )
      END IF
      CALL NL_MN()
      CALL NL_MN()
!
      CALL ADDSTR_F ( '(H) On-line help    ' )
      CALL ADDSTR_F ( '                    ' )
!
      CALL ADDSTR_F ( '(V) Verbosity mode: ' )
      CALL CLRCH ( STR )
      CALL INCH  ( PAMB_VER, STR )
      CALL REVERSE_ON_MN()
      CALL ADDSTR_F ( STR(1:I_LEN(STR)) )
      CALL REVERSE_OFF_MN()
      CALL NL_MN()
      CALL NL_MN()
!
      CALL ADDSTR_F ( '(I) Initialize phase delays             ' )
      CALL ADDSTR_F ( '(R) Read AOB-file' )
      CALL NL_MN()
!
      CALL ADDSTR_F ( '(K) Keep and save ambiguities           ' )
      CALL ADDSTR_F ( '(Z) Freeze/unfreeze suppressionn' )
      CALL NL_MN()
      CALL NL_MN()
!
      CALL ADDSTR_F ( '(A) Resolve phase delay ambiguities in automatic mode' )
      CALL NL_MN()
      CALL NL_MN()
!
      CALL ADDSTR_F ( '(1) Use MIXOB algorithm                 ' )
      CALL ADDSTR_F ( '(5) Use SCADAM algorithm' )
      CALL NL_MN()
!
      CALL ADDSTR_F ( '(2) Use OSCRO algorithm                 ' )
      CALL NL_MN()
!
      CALL ADDSTR_F ( '(3) Use CLOPA algorithm                 ' )
      CALL NL_MN()
!
      CALL ADDSTR_F ( '(4) Use SCATIE algortihm                ' )
      CALL NL_MN()
      CALL NL_MN()
!
      CALL ADDSTR_F ( '(Y) Set new solution type (Z)           ' )
      CALL ADDSTR_F ( '(-) Set singularity check' )
      CALL NL_MN()
      CALL NL_MN()
!
      CALL ADDSTR_F ( '(\) Outlier elimination                 ' )
      CALL ADDSTR_F ( '(/) Restoration suppresed observations'   )
      CALL NL_MN()
!
      CALL ADDSTR_F ( '(W) Weights update                      ' )
      CALL ADDSTR_F ( '(P) Draw plot' )
      CALL NL_MN()
      CALL NL_MN()
      CALL NL_MN()
!
      CALL ADDSTR_F ( '(O) Return to Optin                     ' )
      CALL ADDSTR_F ( '(F) Refresh screen' )
!CCCCC
      CALL SETCR_MN ( 1, 22 )
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
           IF ( IY .EQ.  4  .AND.  IX .LE. 40 ) SIM = 'H'
           IF ( IY .EQ.  4  .AND.  IX .GT. 40 ) SIM = 'V'
           IF ( IY .EQ.  6  .AND.  IX .LE. 40 ) SIM = 'I'
           IF ( IY .EQ.  6  .AND.  IX .GT. 40 ) SIM = 'R'
           IF ( IY .EQ.  7  .AND.  IX .LE. 40 ) SIM = 'K'
           IF ( IY .EQ.  7  .AND.  IX .GT. 40 ) SIM = 'Z'
           IF ( IY .EQ.  9                    ) SIM = 'A'
           IF ( IY .EQ. 11  .AND.  IX .LE. 40 ) SIM = '1'
           IF ( IY .EQ. 11  .AND.  IX .GT. 40 ) SIM = '5'
           IF ( IY .EQ. 12  .AND.  IX .LE. 40 ) SIM = '2'
           IF ( IY .EQ. 12  .AND.  IX .GT. 40 ) SIM = ' '
           IF ( IY .EQ. 13  .AND.  IX .LE. 40 ) SIM = '3'
           IF ( IY .EQ. 13  .AND.  IX .GT. 40 ) SIM = ' '
           IF ( IY .EQ. 14  .AND.  IX .LE. 40 ) SIM = '5'
           IF ( IY .EQ. 14  .AND.  IX .GT. 40 ) SIM = ' '
           IF ( IY .EQ. 16  .AND.  IX .LE. 20 ) SIM = 'Y'
           IF ( IY .EQ. 16  .AND.  IX .GT. 20 ) SIM = 'Z'
           IF ( IY .EQ. 16  .AND.  IX .GT. 40 ) SIM = '-'
           IF ( IY .EQ. 18  .AND.  IX .LE. 40 ) SIM = '\'
           IF ( IY .EQ. 18  .AND.  IX .GT. 40 ) SIM = '/'
           IF ( IY .EQ. 19  .AND.  IX .LE. 40 ) SIM = 'W'
           IF ( IY .EQ. 19  .AND.  IX .GT. 35 ) SIM = 'P'
           IF ( IY .EQ. 22  .AND.  IX .LE. 40 ) SIM = 'O'
           IF ( IY .EQ. 22  .AND.  IX .GT. 40 ) SIM = 'F'
      END IF
!
! --- Now reacting on user's input
!
      IF ( SIM .EQ. 'H' ) THEN
!           CALL PAMB_HELP ( VER_PAMB )
!           CALL REFRESH_MN
           GOTO 910
        ELSE IF ( SIM .EQ. 'V' ) THEN
!
! -------- New version of cycling verbosity mode switching
!
           IF ( PAMB_VER .GE. PAMB_VRB_MAX ) THEN
                PAMB_VER = PAMB_VRB_MIN
              ELSE
                PAMB_VER = PAMB_VER + 1
           END IF
        ELSE IF ( SIM .EQ. 'I' ) THEN
           IF ( KBIT ( OPP_STATUS, OPP_SET1__BIT )  .AND. &
     &          KBIT ( OPP_STATUS, OPP_SET2__BIT )         ) THEN
                IACT = 11  ! Go to initiliaze
              ELSE
                IACT = 12  ! Go to read AOB file
                IF ( DBNAME(9:9) == 'K' .OR. DBNAME(10:10) == 'r' ) IACT = 11
           END IF
           GOTO 810
        ELSE IF ( SIM .EQ. 'R' ) THEN
           IACT = 12
           GOTO 810
        ELSE IF ( SIM .EQ. 'K' ) THEN
           IACT = 13
           GOTO 810
        ELSE IF ( SIM .EQ. 'Z' ) THEN
           IACT = 14
           GOTO 810
        ELSE IF ( SIM .EQ. '1' ) THEN
           IACT = 101
           GOTO 810
        ELSE IF ( SIM .EQ. '2' ) THEN
           IACT = 102
           GOTO 810
        ELSE IF ( SIM .EQ. '3' ) THEN
           IACT = 103
           GOTO 810
        ELSE IF ( SIM .EQ. '4' ) THEN
           IACT = 104
           GOTO 810
        ELSE IF ( SIM .EQ. '5' ) THEN
           IACT = 105
           GOTO 810
        ELSE IF ( SIM .EQ. '6' ) THEN
           IACT = 106
           GOTO 810
        ELSE IF ( SIM .EQ. '7' ) THEN
           IACT = 107
           GOTO 810
        ELSE IF ( SIM .EQ. '8' ) THEN
           IACT = 108
           GOTO 810
        ELSE IF ( SIM .EQ. 'Y' ) THEN
           IACT = 111
           GOTO 810
        ELSE IF ( SIM .EQ. 'Z' ) THEN
           IACT = 211
           GOTO 810
        ELSE IF ( SIM .EQ. '-' ) THEN
           IACT = 112
           GOTO 810
        ELSE IF ( SIM .EQ. 'A' ) THEN
           IACT = 121
           GOTO 810
        ELSE IF ( SIM .EQ. '\' ) THEN
           IACT = 131
           GOTO 810
        ELSE IF ( SIM .EQ. '/' ) THEN
           IACT = 132
           GOTO 810
        ELSE IF ( SIM .EQ. 'W' ) THEN
           IACT = 133
           GOTO 810
        ELSE IF ( SIM .EQ. 'P' ) THEN
           IACT = 141
           GOTO 810
        ELSE IF ( SIM .EQ. 'F' ) THEN
!
! -------- Refresh screen
!
           GOTO 910
        ELSE IF ( SIM .EQ. 'O' ) THEN
!
! -------- exit from the routine to "Otdyhat'" -- go to OPTIN without group
! -------- delay ambiguities resolution.
!
           IACT = 0
           F_OPTIN = .TRUE.
           GOTO 810
      END IF
      GOTO 910
!
 810  CONTINUE
      IF ( IACT .NE. 0                               .AND. &
     &     IACT .NE. 11                              .AND. &
     &     IACT .NE. 12                              .AND. &
     &     IACT .NE. 141                             .AND. &
     &     .NOT. KBIT ( OPP_STATUS, OPP_SET1__BIT )  .AND. &
     &     .NOT. KBIT ( OPP_STATUS, OPP_SET2__BIT )         ) THEN
!
! -------- Check have phase delays and ambiguities been initialized
!
           CALL SETCR_MN ( 1, 23 )
           CALL REVERSE_ON_MN()
           CALL ADDSTR_F ( 'Ambiguity should be firstly initialized!   ' )
           CALL REVERSE_OFF_MN()
           CALL SENKR_MN ( IX, IY, CC4 )
           IACT = 0
           GOTO 910
      END IF
!
      CALL CLEAR_MN ()
      CALL END_MN()
      CALL UN_CURSES() ! Elimination of the influence of curses
      CALL CLEAR ( 0, 0 )
!
      RETURN
      END  !#!  PAMB_MENU  #!#
