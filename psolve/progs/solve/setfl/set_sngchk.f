      SUBROUTINE SET_SNGCHK ( SNGCHK_ACTION, SNGCHK_SOUMIN, SNGCHK_STAMIN, &
     &                        SNGCHK_BASMIN )
! ************************************************************************
! *                                                                      *
! *   Routine  SET_SNGCHK   displays, sets or changes parameters of      *
! *   singularity check.                                                 *
! *                                                                      *
! *  ###  09-JUL-98   SET_SNGCHK   v1.1  (c)  L. Petrov  11-JUL-98  ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
      INCLUDE    'solve.i'
      INCLUDE    'help.i'
      INTEGER*4    SNGCHK_ACTION, SNGCHK_SOUMIN, SNGCHK_STAMIN, SNGCHK_BASMIN
      INTEGER*4    MVAL_ACT
      PARAMETER  ( MVAL_ACT = SNGCHK_ACT__LSIN )
      CHARACTER    VAL_ACT(0:MVAL_ACT)*16, MIN_VAL(3)*27
      CHARACTER    CC4*4, SIM*1
      CHARACTER    SET_SNGCHK_VER*19, OUT*79, STR*80, FINAM*255
      PARAMETER  ( SET_SNGCHK_VER  = 'SET_SNGCHK 07/11/98' )
      INTEGER*4    J1, IX, IY, NL_MIN(3), NM, NL, IVAL(3), IVAL_NEW, IACT, IM
      PARAMETER  ( NL =20 )
      INTEGER*4    ILEN, I_LEN, MAKE_HELP_FINAM
      DATA         VAL_ACT /  'Undefined       ', &   ! 0
     &                        'None            ', &   ! 1
     &                        'Warning         ', &   ! 2
     &                        'Reparameterize  ', &   ! 3
     &                        'Stop            ' / ! 4
      DATA         MIN_VAL  / ' min_obs for one source:   ', &    ! 1
     &                        ' min_obs for one station:  ', &    ! 2
     &                        ' min_obs for one baseline: '  / ! 3
      DATA         NL_MIN / 10, 12, 14 /
!
      IVAL(1) = SNGCHK_SOUMIN
      IVAL(2) = SNGCHK_STAMIN
      IVAL(3) = SNGCHK_BASMIN
      IACT    = SNGCHK_ACTION
      IF ( IACT .LT. 0  .OR.  IACT .GT. MVAL_ACT ) THEN
!
! -------- If input value was unsupported then set it: undefined
!
           IACT = SNGCHK_ACT__UNDF
      END IF
!
! --- Clear display
!
 910  CONTINUE
      CALL CLEAR_MN()
      CALL SETCR_MN (  0, 0 )
!
! --- Printing the first line: title of the program
!
!
      CALL ADDSTR_F ( 'Set singularity check' )
      CALL SETCR_MN (  79-ILEN(SET_SNGCHK_VER), 0 )
      CALL REVERSE_ON_MN()
      CALL ADDSTR_F ( SET_SNGCHK_VER )
      CALL REVERSE_OFF_MN()
      CALL NL_MN()
      CALL ADDSTR_F ( '같같같같같같같같같같�' )
      CALL NL_MN()
      CALL NL_MN()
!
! --- Printng a list of supported commands
!
      CALL NL_MN()
!
! --- Action
!
      CALL ADDSTR_F ( '(A) Singularity check action: ' )
      CALL REVERSE_ON_MN()
      CALL ADDSTR_F ( VAL_ACT(IACT)(1:I_LEN(VAL_ACT(IACT)))  )
      CALL REVERSE_OFF_MN()
      CALL NL_MN()
      CALL NL_MN()
      CALL NL_MN()
      CALL ADDSTR_F ( '                   Singularity check criterion:' )
      CALL NL_MN()
      CALL NL_MN()
      CALL NL_MN()
!
! --- Criterions
!
      DO 410 J1=1,3
         CALL CLRCH    ( STR )
         CALL INCH     ( J1, STR )
         CALL ADDSTR_F ( '('//STR(1:I_LEN(STR))//')'//MIN_VAL(J1) )
         CALL CLRCH    ( STR )
         CALL INCH     ( IVAL(J1), STR )
         CALL ADDSTR_F ( STR(1:I_LEN(STR)) )
         CALL NL_MN()
         CALL NL_MN()
 410  CONTINUE
!
! --- Printing bottom line
!
      CALL SETCR_MN ( 0, NL )
      CALL CLRCH ( OUT )
      OUT = '________________________________________________________'// &
     &      '_______________________'
      CALL ADDSTR_F ( OUT )
      CALL NL_MN()
!
! --- ... and options at the bottom line
!
      CALL CLRCH ( OUT )
      OUT = '(O)Go back without saving    (S)Save results and go back     '// &
     &      '(H)On-line help'
      CALL ADDSTR_F ( OUT )
!
! --- Awaiting for user response
!
      CALL SETCR_MN ( 1, NL+1 )
      CALL SENKR_MN ( IX, IY, CC4 )
!
      SIM = CC4(4:4)
      IF ( SIM .EQ. ' '  .OR.  SIM .EQ. CHAR(13) ) THEN
!
! -------- Decoding input as poiting to the command
!
           IF ( IY .EQ.  4                                          ) SIM = 'A'
           IF ( IY .EQ.  9                                          ) SIM = '1'
           IF ( IY .EQ. 11                                          ) SIM = '2'
           IF ( IY .EQ. 13                                          ) SIM = '3'
           IF ( IY .EQ. NL+1 .AND.   IX .GE. 1    .AND.  IX .LE. 28 ) SIM = 'O'
           IF ( IY .EQ. NL+1 .AND.   IX .GE. 29   .AND.  IX .LE. 60 ) SIM = 'S'
           IF ( IY .EQ. NL+1 .AND.   IX .GE. 61   .AND.  IX .LE. 78 ) SIM = 'H'
      END IF
!
! --- Parsing code of the command and execution
!
      IF ( SIM .EQ. 'A' ) THEN
!
! -------- Action
!
           IACT = IACT + 1
           IF ( IACT .GT. MVAL_ACT ) IACT = 1
         ELSE IF ( SIM .EQ. '1'  .OR.  SIM .EQ. '2'  .OR.  SIM .EQ. '3' ) THEN
!
! -------- Criterion
!
 920       CONTINUE
!
! -------- Make prompt line
!
           CALL CHIN ( SIM, NM )
           CALL CLRCH ( OUT )
           OUT = MIN_VAL(NM)
           CALL INCH ( IVAL(NM), OUT(I_LEN(OUT)+2:) )
           OUT(I_LEN(OUT)+3:) = 'Enter new value >> '
!
! -------- ... and print it to the screen
!
           CALL SETCR_MN ( 0, NL_MIN(NM) )
           CALL ADDSTR_F ( OUT(1:I_LEN(OUT))//' ' )
!
! -------- Awaiting for user response
!
           CALL CLRCH    ( STR )
           CALL GETSTR_F ( STR )
!
! -------- Parsing the line
!
           IF ( ILEN(STR) .EQ. 0 ) GOTO 910
           CALL TRAN ( 11, STR, STR )
           IF ( STR(1:2) .EQ. 'NO' ) STR='0'
!
           CALL CHIN ( STR, IVAL_NEW )
           IF ( IVAL_NEW .LT. 0 ) THEN
!
! ------------- Error during parsing
!
                CALL SETCR_MN ( 0, NL_MIN(NM) )
                CALL REVERSE_ON_MN()
                CALL ADDSTR_F ( '$$$ Error of parsing '//STR(1:I_LEN(STR))// &
     &                          ' -- Non-negative integer was expected.  $$$' )
                CALL REVERSE_OFF_MN()
                CALL SENKR_MN ( IX, IY, CC4 )
                GOTO 920
           END IF
!
           IVAL(NM) = IVAL_NEW
         ELSE IF ( SIM .EQ. 'O' ) THEN
!
! -------- Quit
!
           GOTO 810
           RETURN
         ELSE IF ( SIM .EQ. 'S' ) THEN
!
! -------- Save results
!
           SNGCHK_ACTION = IACT
           SNGCHK_SOUMIN = IVAL(1)
           SNGCHK_STAMIN = IVAL(2)
           SNGCHK_BASMIN = IVAL(3)
!
! -------- ... and quit
!
           GOTO 810
           RETURN
         ELSE IF ( SIM .EQ. 'H' ) THEN
!
! -------- Stopping curses
!
           CALL END_MN()
           CALL UN_CURSES ( )
!
! -------- Making file name with HELP information
!
           IM = MAKE_HELP_FINAM ( SNGCHK_HELP, FINAM )
           IF ( IM.NE.0 ) THEN
                CALL ERR_LOG ( 6821, -1, 'SET_SNGCHK', 'Help file '// &
     &               SNGCHK_HELP//' is not found. Check directory '// &
     &               SOLVE_HELP_DIR//' and environment variable SOLVE_HELP_DIR' )
                CALL HIT_CONT ( '--- Hit any key to proceed --- '//CHAR(1), &
     &                          %VAL(0) )
                GOTO 910
           END IF
!
! -------- Displaying the buffer at the screen
!
           CALL SHOW_TEXT_FILE_COL ( FINAM, 'Set singularity check', 0, -3 )
!
! -------- Re-starting curses again
!
           CALL START_MN()
      END IF
      GOTO 910
!
 810  CONTINUE
!
      RETURN
      END  !#!  SET_CNGCHK  #!#
