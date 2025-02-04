      SUBROUTINE PARU_MENU ( VER_PAMB, DBNAME, PARU_DIR, PARU_PRG, &
     &                       PARU_SCRATCH, PARU_FIL, PAMB_VER, F_QUIT )
! ************************************************************************
! *                                                                      *
! *   Routine  PARU_MENU  gets parameters of PARU (Phase Ambiguity       *
! *   Resolution Utility).                                               *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *      VER_PAMB ( CHARACTER ) -- String with PAMB-identifier and       *
! *                                number of the current version.        *
! *        DBNAME ( CHARACTER ) -- Database name + version number.       *
! *      PARU_DIR ( CHARACTER ) -- Directory where PARU programs assumed *
! *                                to reside.                            *
! *      PARU_PRG ( CHARACTER ) -- Arrays with PARU_MPRG file names of   *
! *                                default PARU programs.                *
! *  PARU_SCRATCH ( CHARACTER ) -- File name of scratch PARU-program.    *
! *                                                                      *
! * ________________________ Output parameters: ________________________ *
! *                                                                      *
! *    PARU_FIL ( CHARACTER ) -- File name of the PARU-program which     *
! *                              assumed to be used further.             *
! *    PAMB_VER ( INTEGER*4 ) -- Verbosity level for PARU-compiler and   *
! *                              executor. NB: it is not parameter of    *
! *                              verbosity level for the certain         *
! *                              procedures to be run within             *
! *                              PARU-program.                           *
! *      F_QUIT ( LOGICAL*4 ) -- Flag. If it is set as .TRUE. that means *
! *                              user decided to quit PARU-menu quietly  *
! *                              and don't compile PARU-program after    *
! *                              that. If F_QUIT is .FALSE. PARU-program *
! *                              is compiled and executed after.         *
! *                                                                      *
! *  ###  16-MAR-98     PARU_MENU    v1.0 (c)  L. Petrov  19-MAR-98 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'solve.i'
      INCLUDE   'glbc4.i'
      INCLUDE   'pamb.i'
      INTEGER*4  PAMB_VER
      CHARACTER  VER_PAMB*(*), DBNAME*(*), PARU_DIR*(*), &
     &           PARU_PRG(PARU_MPRG)*(*), PARU_SCRATCH*(*), PARU_FIL*(*)
      LOGICAL*4  F_QUIT
!
      CHARACTER  CC4*4, SIM*1, STR*255
      INTEGER*4  IX, IY, J1, IP, IER
      INTEGER*4, EXTERNAL :: ILEN, I_LEN, SYSTEM
!CCCCC
      CALL USE_GLBFIL_4 ( 'ORC' )
      F_QUIT = .FALSE.
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
      CALL ADDSTR_F ( 'PARU (Phase Ambiguities Resolution Utility)' )
      CALL SETCR_MN (  79-ILEN(VER_PAMB), 0 )
      CALL REVERSE_ON_MN()
      CALL ADDSTR_F ( VER_PAMB )
      CALL REVERSE_OFF_MN()
      CALL NL_MN()
      CALL ADDSTR_F ( '같같같같같같같같같같같같같같같같같같같같같�' )
      CALL NL_MN()
!
! --- And then printing the menu
!
      CALL ADDSTR_F ( 'Database in use: '//DBNAME )
      CALL NL_MN()
      CALL NL_MN()
      STR = 'File with PARU-program: '//PARU_FIL
      CALL ADDSTR_F ( STR(1:79) )
      CALL NL_MN()
      CALL NL_MN()
!
      CALL ADDSTR_F ( '(H) On-line help          ' )
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
      DO 410 J1=1,PARU_MPRG
         CALL CLRCH ( STR )
         CALL INCH  ( J1, STR )
         STR = '('//STR(1:I_LEN(STR))//'). Template file: '//PARU_PRG(J1)
         CALL ADDSTR_F ( STR(1:79) )
         CALL NL_MN()
 410  CONTINUE
      STR = '(S)  Scratch file:  '//PARU_SCRATCH
      CALL ADDSTR_F ( STR(1:79) )
      CALL NL_MN()
      CALL NL_MN()
      CALL ADDSTR_F ( '(E) Edit PARU-file' )
      CALL NL_MN()
      CALL NL_MN()
      CALL ADDSTR_F ( '(X) Execute PARU-program' )
!
      CALL ADDSTR_F ( '                      '   )
      CALL ADDSTR_F ( '(Q) Leave PARU quitely'   )
      CALL NL_MN()
      CALL NL_MN()
!
!CCCCC
      CALL SETCR_MN ( 1, 12+PARU_MPRG )
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
           IF ( IY .EQ.  6                              ) SIM = 'H'
           IF ( IY .EQ.  6  .AND.  IX .LT. 40           ) SIM = 'V'
           IF ( IY .EQ.  8                              ) SIM = '1'
           IF ( IY .EQ.  9  .AND.  PARU_MPRG .LE. 2     ) SIM = '2'
           IF ( IY .EQ. 10  .AND.  PARU_MPRG .LE. 3     ) SIM = '3'
           IF ( IY .EQ. 11  .AND.  PARU_MPRG .LE. 4     ) SIM = '4'
           IF ( IY .EQ. 12  .AND.  PARU_MPRG .LE. 5     ) SIM = '5'
           IF ( IY .EQ. 13  .AND.  PARU_MPRG .LE. 6     ) SIM = '6'
           IF ( IY .EQ. 14  .AND.  PARU_MPRG .LE. 7     ) SIM = '7'
           IF ( IY .EQ. 15  .AND.  PARU_MPRG .LE. 8     ) SIM = '8'
           IF ( IY .EQ. 16  .AND.  PARU_MPRG .LE. 9     ) SIM = '9'
           IF ( IY .EQ. 10+PARU_MPRG  .AND.  IX .LT. 40 ) SIM = 'E'
           IF ( IY .EQ. 12+PARU_MPRG  .AND.  IX .LT. 40 ) SIM = 'X'
           IF ( IY .EQ. 12+PARU_MPRG  .AND.  IX .GE. 40 ) SIM = 'Q'
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
        ELSE IF ( SIM .EQ. 'E' .AND. ILEN(PARU_FIL) .GT. 0 ) THEN
           IF ( ILEN(PARU_FIL) .GT. 0 ) THEN
!
! ------------- Stop curser
!
                CALL END_MN()
                CALL UN_CURSES() ! Elimination of the influence of curses
                CALL CLEAR ( 0, 0 )
                WRITE ( 6, * )  'Editor '//SOLVE_EDITOR(1:I_LEN(SOLVE_EDITOR))// &
     &                          ' is scheduling...    '
!
! ------------- Scedling SOLVE-editor
!
                IER = SYSTEM ( SOLVE_EDITOR(1:I_LEN(SOLVE_EDITOR))//' '// &
     &                          PARU_FIL(1:I_LEN(PARU_FIL))//CHAR(0) )
                IF ( IER .NE. 0 ) THEN
!
! ---------------- Printing error message
!
                   WRITE ( 6, * ) ' IER =',IER
                   IF ( IER .EQ. 32512 ) WRITE ( 6, FMT='(1X,A)' ) &
     &                 'Environment variable SHELL probably has a wrong value'
                   WRITE ( 6, FMT='(1X,A)' ) 'Error in running Shell '// &
     &                 'command: "'// &
     &                  SOLVE_EDITOR(1:I_LEN(SOLVE_EDITOR))//' '// &
     &                          PARU_FIL(1:I_LEN(PARU_FIL))
                   STOP 'PAMB'
                ENDIF
!
! ------------- Start curser again
!
                CALL START_MN()
           ENDIF
        ELSE IF ( SIM .EQ. 'S' ) THEN
           PARU_FIL = PARU_SCRATCH
        ELSE IF ( SIM .EQ. '1' .AND. PARU_MPRG .GE. 1 ) THEN
           IP=1
           IF ( ILEN(PARU_PRG(IP)) .GT. 0 ) THEN
                PARU_FIL = PARU_DIR(1:I_LEN(PARU_DIR))//PARU_PRG(IP)
                CALL CHASHL ( PARU_FIL )
           END IF
        ELSE IF ( SIM .EQ. '2' .AND. PARU_MPRG .GE. 2 ) THEN
           IP=2
           IF ( ILEN(PARU_PRG(IP)) .GT. 0 ) THEN
                PARU_FIL = PARU_DIR(1:I_LEN(PARU_DIR))//PARU_PRG(IP)
                CALL CHASHL ( PARU_FIL )
           END IF
        ELSE IF ( SIM .EQ. '3' .AND. PARU_MPRG .GE. 3 ) THEN
           IP=3
           IF ( ILEN(PARU_PRG(IP)) .GT. 0 ) THEN
                PARU_FIL = PARU_DIR(1:I_LEN(PARU_DIR))//PARU_PRG(IP)
                CALL CHASHL ( PARU_FIL )
           END IF
        ELSE IF ( SIM .EQ. '4' .AND. PARU_MPRG .GE. 4 ) THEN
           IP=4
           IF ( ILEN(PARU_PRG(IP)) .GT. 0 ) THEN
                PARU_FIL = PARU_DIR(1:I_LEN(PARU_DIR))//PARU_PRG(IP)
                CALL CHASHL ( PARU_FIL )
           END IF
        ELSE IF ( SIM .EQ. '5' .AND. PARU_MPRG .GE. 5 ) THEN
           IP=5
           IF ( ILEN(PARU_PRG(IP)) .GT. 0 ) THEN
                PARU_FIL = PARU_DIR(1:I_LEN(PARU_DIR))//PARU_PRG(IP)
                CALL CHASHL ( PARU_FIL )
           END IF
        ELSE IF ( SIM .EQ. '6' .AND. PARU_MPRG .GE. 6 ) THEN
           IP=6
           IF ( ILEN(PARU_PRG(IP)) .GT. 0 ) THEN
                PARU_FIL = PARU_DIR(1:I_LEN(PARU_DIR))//PARU_PRG(IP)
                CALL CHASHL ( PARU_FIL )
           END IF
        ELSE IF ( SIM .EQ. '7' .AND. PARU_MPRG .GE. 7 ) THEN
           IP=7
           IF ( ILEN(PARU_PRG(IP)) .GT. 0 ) THEN
                PARU_FIL = PARU_DIR(1:I_LEN(PARU_DIR))//PARU_PRG(IP)
                CALL CHASHL ( PARU_FIL )
           END IF
        ELSE IF ( SIM .EQ. '8' .AND. PARU_MPRG .GE. 8 ) THEN
           IP=8
           IF ( ILEN(PARU_PRG(IP)) .GT. 0 ) THEN
                PARU_FIL = PARU_DIR(1:I_LEN(PARU_DIR))//PARU_PRG(IP)
                CALL CHASHL ( PARU_FIL )
           END IF
        ELSE IF ( SIM .EQ. '9' .AND. PARU_MPRG .GE. 9 ) THEN
           IP=9
           IF ( ILEN(PARU_PRG(IP)) .GT. 0 ) THEN
                PARU_FIL = PARU_DIR(1:I_LEN(PARU_DIR))//PARU_PRG(IP)
                CALL CHASHL ( PARU_FIL )
           END IF
        ELSE IF ( SIM .EQ. 'Q' ) THEN
!
           F_QUIT = .TRUE.
           GOTO 810
        ELSE IF ( SIM .EQ. 'X' ) THEN
           IF ( ILEN(PARU_FIL) .EQ. 0 ) THEN
                CALL SETCR_MN ( 1, 23 )
                CALL REVERSE_ON_MN()
                CALL ADDSTR_F ( '$$$  File with PARU source code has not '// &
     &                          'been specified   ' )
                CALL REVERSE_OFF_MN()
                CALL SENKR_MN ( IX, IY, CC4 )
              ELSE
                GOTO 810
           END IF
      END IF
      GOTO 910
!
 810  CONTINUE
      CALL END_MN()
      CALL UN_CURSES() ! Elimination of the influence of curses
      CALL CLEAR ( 0, 0 )
!
      RETURN
      END  !#!  PARU_MENU  #!#
