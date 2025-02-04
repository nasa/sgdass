      SUBROUTINE PAMB_INIT_MENU ( VER_PAMB, F_AMB, F_SUP, F_NWT, INIT_WEI, &
     &           IACT )
! ************************************************************************
! *                                                                      *
! *   Routine  PAMB_INIT_MENU  takes parameters of initialization of     *
! *   phase delays in PAMB in menu mode.                                 *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *  VER_PAMB ( CHARACTER ) -- String with PAMB-identifier and number    *
! *                            of the current version.                   *
! *                                                                      *
! * _________________________ Modified parameters: _____________________ *
! *                                                                      *
! *    F_AMB ( LOGICAL*4 ) -- Flag: if .TRUE. then ambiguities will be   *
! *                           initialized.                               *
! *    F_SUP ( LOGICAL*4 ) -- Flag: if .TRUE. then suppression status of *
! *                           phase delay observables will be            *
! *                           initialized.                               *
! *    F_NWT ( LOGICAL*4 ) -- Flag: if .TRUE. then new phase delay       *
! *                           baseline-dependent reweight constants will *
! *                           be set up.                                 *
! * INIT_WEI ( LOGICAL*4 ) -- Values of baseline reweight constant       *
! *                           (in seconds). INIT_WEI is reweight         *
! *                           constant common for all baselines. It      *
! *                           modifies weights as                        *
! *                           NEW_WEI = 1./SQRT ( SIGMA_OBS**2 +         *
! *                                               INIT_WEI**2    )       *
! *                           where SIGMA_OBS -- formal error of used    *
! *                           observable.                                *
! *                           NB: baseline dependent constant us set up  *
! *                           ONLY IF F_NWT is .TRUE. Otherwise INIT_WEI *
! *                           is ignored and old value of reweight       *
! *                           constant is in use.                        *
! *                                                                      *
! * _________________________ Output parameters: _______________________ *
! *                                                                      *
! *     IACT ( INTEGER*4  ) -- User action after finsihing menu:         *
! *                            IACT=0 -- not to make initialization;     *
! *                            IACT=1 -- proceed phase delay             *
! *                                      initialization routine in       *
! *                                      according with specified flags. *
! *                                                                      *
! *  ### 04-NOV-1998 PAMB_INIT_MENU  v1.0 (c) L. Petrov 04-NOV-1998 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      CHARACTER  VER_PAMB*(*)
      LOGICAL*4  F_AMB, F_SUP, F_NWT
      REAL*8     INIT_WEI
      INTEGER*4  IACT
      CHARACTER  SIM*1, CC4*4, STR*32
      REAL*8     VAL_NEW, VAL_LIM
      PARAMETER  ( VAL_LIM = 0.1D0 )
      INTEGER*4  IX, IY, IO
      INTEGER*2  INT2_ARG
      INTEGER*4  INT4
      INT4(INT2_ARG) = INT(INT2_ARG,KIND=4)
      INTEGER*4, EXTERNAL :: ILEN, I_LEN, FSTREAM
!
! --- Start curser
!
      IACT = 0
      CALL START_MN()
 910  CONTINUE
!
! ----- Printing various menu items
!
        CALL CLEAR_MN()
        CALL SETCR_MN (  0, 0 )
!
! ----- Printing the first line: title of the program
!
        CALL ADDSTR_F ( 'Phase delay initialization routine PAMB_INIT' )
        CALL SETCR_MN (  79-ILEN(VER_PAMB), 0 )
        CALL ADDSTR_F ( VER_PAMB )
        CALL NL_MN()
        CALL ADDSTR_F ( '같같같같같같같같같같같같같같같같같같같같'// &
     &                  '같같같같같같같같같같같같같같같같같같같�'    )
        CALL NL_MN()
        CALL NL_MN()
!
        CALL ADDSTR_F ( '(A) Initialize phase delay ambiguities ' )
        CALL REVERSE_ON_MN()
        IF ( F_AMB ) THEN
             CALL ADDSTR_F ( 'yes' )
          ELSE
             CALL ADDSTR_F ( ' no' )
        END IF
        CALL REVERSE_OFF_MN()
        CALL NL_MN()
        CALL NL_MN()
!
        CALL ADDSTR_F ( '(S) Initialize suppression flag ' )
        CALL REVERSE_ON_MN()
        IF ( F_SUP ) THEN
             CALL ADDSTR_F ( 'yes' )
          ELSE
             CALL ADDSTR_F ( ' no' )
        END IF
        CALL REVERSE_OFF_MN()
        CALL NL_MN()
        CALL NL_MN()
!
        CALL ADDSTR_F ( '(W) Initialize phase baseline-dependent weights ' )
        CALL REVERSE_ON_MN()
        IF ( F_NWT ) THEN
             CALL ADDSTR_F ( 'yes' )
          ELSE
             CALL ADDSTR_F ( ' no' )
        END IF
        CALL REVERSE_OFF_MN()
        CALL NL_MN()
        CALL NL_MN()
!C
        CALL ADDSTR_F ( '(C) Change constant for baseline-dependent weights ' )
        WRITE ( UNIT=STR, FMT='(F6.1)' ) INIT_WEI*1.D12
        CALL CHASHL ( STR )
        IF ( STR(1:1) .EQ. '.' ) STR = '0'//STR
        CALL REVERSE_ON_MN()
        CALL ADDSTR_F (STR(1:I_LEN(STR)) )
        CALL REVERSE_OFF_MN()
        CALL ADDSTR_F ( ' ps' )
        CALL NL_MN()
        CALL NL_MN()
!C
        CALL SETCR_MN ( 0, 21 )
        CALL REFRESH_MN()
        CALL ADDSTR_F ( '같같같같같같같같같같같같같같같같같같같같'// &
     &                  '같같같같같같같같같같같같같같같같같같같�'    )
        CALL SETCR_MN ( 0, 22 )
        CALL ADDSTR_F ( '(*) Go ahead;   (B) Go back to the previous menu' )
!
! ----- Awaiting for user input
!
        CALL SETCR_MN ( 1, 22 )
        CALL SENKR_MN ( IX, IY, CC4 )
        SIM = CC4(4:4)
        CALL TRAN ( 11, SIM, SIM )
!
! ----- Decoding user input
!
        IF ( SIM .EQ. CHAR(13) ) SIM = ' '
        IF ( SIM .EQ. ' ' ) THEN
             IF ( IY .EQ. 3 ) THEN
                  SIM = 'A'
                ELSE IF ( IY .EQ. 5 ) THEN
                  SIM = 'S'
                ELSE IF ( IY .EQ. 7 ) THEN
                  SIM = 'W'
                ELSE IF ( IY .EQ. 9 ) THEN
                  SIM = 'C'
                ELSE IF ( IY .EQ. 22  .AND.  IX .LE. 15 ) THEN
                  SIM = '*'
                ELSE IF ( IY .EQ. 22  .AND.  IX .GT. 15 ) THEN
                  SIM = 'B'
             END IF
        END IF
!
! ----- Make actions in according with user input
!
        IF ( SIM .EQ. 'A' ) THEN
             F_AMB = .NOT. F_AMB
           ELSE IF ( SIM .EQ. 'S' ) THEN
             F_SUP = .NOT. F_SUP
           ELSE IF ( SIM .EQ. 'W' ) THEN
             F_NWT = .NOT. F_NWT
           ELSE IF ( SIM .EQ. 'C' ) THEN
 920         CONTINUE
             CALL SETCR_MN ( 0, 9 )
!
! ---------- Print explanation and old value
!
             WRITE ( UNIT=STR, FMT='(F6.1)' ) INIT_WEI*1.D12
             CALL CHASHL ( STR )
             IF ( STR(1:1) .EQ. '.' ) STR = '0'//STR
             CALL ADDSTR_F ( 'Old value: ' )
             CALL ADDSTR_F ( STR(1:I_LEN(STR))//' ps' )
!
! ---------- Print a prompt
!
             CALL ADDSTR_F ( '  NEW VALUE: >> ' )
!
! ---------- Getting a string which user input
!
             CALL CLRCH    ( STR )
             CALL GETSTR_F ( STR )
             IF ( ILEN(STR) .EQ. 0 ) GOTO 910 ! Empty line -- nothing to do
!
! ---------- Decoding
!
             IF ( INDEX ( STR, '.' ) .LE. 0 ) STR=STR(1:I_LEN(STR))//'.'
             READ ( UNIT=STR(1:I_LEN(STR)), FMT='(F8.4)', IOSTAT=IO ) VAL_NEW
             IF ( IO .NE. 0   .OR.   VAL_NEW .LT. VAL_LIM ) THEN
!
! --------------- An error in decoding was detected. Clear the line
!
                  CALL SETCR_MN ( 0, 9 )
                  CALL ADDSTR_F ( '                                        '// &
     &                            '                                       ' )
                  CALL SETCR_MN ( 0, 9 )
!
! --------------- Print an explanation
!
                  CALL REVERSE_ON_MN()
                  CALL ADDSTR_F ( 'WRONG INPUT: '//STR(1:I_LEN(STR)) )
                  CALL REVERSE_OFF_MN()
!
! --------------- ... and awaiting for user to hit a line
!
                  CALL SETCR_MN ( 50, 9 )
                  CALL SENKR_MN ( IX, IY, CC4 )
                  GOTO 920
             END IF
             INIT_WEI = VAL_NEW/1.D12
           ELSE IF ( SIM .EQ. '*' ) THEN
             IACT = 1
             GOTO 810
           ELSE IF ( SIM .EQ. 'B' ) THEN
             IACT = 0
             GOTO 810
        END IF
      GOTO 910
!
 810  CONTINUE
!
! --- Leave curses
!
      CALL CLEAR_MN ()
      CALL END_MN()
      CALL UN_CURSES() ! Elimination of the influence of curses
      CALL CLEAR ( 0, 0 )
!
      RETURN
      END  !#!  PAMB_INIT_MENU  #!#
