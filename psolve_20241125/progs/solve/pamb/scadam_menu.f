      SUBROUTINE SCADAM_MENU ( DBOBJ, SCAINF, ICOND, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  SCADAM_MENU  gets parameters of the algorithm SCADAM for  *
! *   phase delay ambiguity resolution in interactive mode and puts them *
! *   in data structure  SCAINF.                                         *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *  DBOBJ ( RECORD    ) -- Data structure which keeps general           *
! *                         information about the database such as lists *
! *                         of the objects.                              *
! *                                                                      *
! * _________________________ Output parameters: _______________________ *
! *                                                                      *
! *  ICOND ( INTEGER*4 ) -- Condition code. It specifies the further     *
! *                         action. Possible values:                     *
! *                         ICOND = 0 -- return to the previous menu.    *
! *                         ICOND = 1 -- execute phase delay ambiguity   *
! *                                      resolution using SCADAM         *
! *                                      algorithm.                      *
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
! * SCAINF ( RECORD    ) -- Data structure which keeps a) values of      *
! *                         parameters which control work of algorithm   *
! *                         SCADAM; b) result of work of algorithm       *
! *                         SCADAM.                                      *
! *   IUER ( INTEGER*4, OPT ) -- Universal error handler.                *
! *                           Input: switch IUER=0 -- no error messages  *
! *                                  will be generated even in the case  *
! *                                  of error. IUER=-1 -- in the case of *
! *                                  error the message will be put on    *
! *                                  stdout.                             *
! *                           Output: 0 in the case of successful        *
! *                                   completion and non-zero in the     *
! *                                   case of error.                     *
! *                                                                      *
! *  ###  19-OCT-98  SCADAM_MENU   v1.2  (c)  L. Petrov  22-DEC-98  ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'solve.i'
      INCLUDE   'obser.i'
      INCLUDE   'pamb.i'
      TYPE ( DBOBJ_O__STRU ) ::  DBOBJ
      TYPE ( SCAINF__STRU ) ::  SCAINF
      INTEGER*4  ICOND, IUER
      CHARACTER  CC4*4, SIM*1, STR*80, STR1*32
      INTEGER*4  M_MN, IY_STA, IY_BOT
      PARAMETER  ( M_MN=10 )
      PARAMETER   ( IY_STA = 13, IY_BOT = 21 )
      INTEGER*4  IX, IY, IMN, J1, J2, IO, IP
      CHARACTER  C_MN(M_MN)*39, UNIT_MN(M_MN)*8, LET*(M_MN)
      REAL*8     SCALE_MN(M_MN), VAL_MN(M_MN), VAL_NEW, VAL_LIM, HR_INV
      INTEGER*4  LF_STA
      LOGICAL*4  FFID
      PARAMETER  ( VAL_LIM = 0.0D0         )
      PARAMETER  ( HR_INV  = 1.D0/3600.0D0 ) ! Number of hours in one second
      INTEGER*4  IX_MN(M_MN), IY_MN(M_MN)
      DATA       ( C_MN(IX), UNIT_MN(IX), IX=1,M_MN ) &
     &           / &
     &             '(A) XGR misclosure limit               ', 'ps      ', &
     &             '(B) SGR misclosure limit               ', 'ps      ', &
     &             '(C) XPH misclosure limit               ', 'ps      ', &
     &             '(D) SPH misclosure limit               ', 'ps      ', &
     &             '(E) Defragmentation limit              ', 'turns   ', &
     &             '(F) max ARF sigma limit                ', 'turns   ', &
     &             '(G) Frozen transition limit            ', 'hours   ', &
     &             '(H) ARF floor                          ', 'turns   ', &
     &             '(I) Spline time span                   ', 'hours   ', &
     &             '(J) Spline constraint                  ', 'tr/hr   ' &
     &           /
      DATA       LET / 'ABCDEFGHIJ' /
      DATA       ( IX_MN(IX), IY_MN(IX), SCALE_MN(IX), IX=1,M_MN ) &
     &           / &
     &              0,  2,  1.D12, &     ! A
     &             40,  2,  1.D12, &     ! B
     &              0,  3,  1.D12, &     ! C
     &             40,  3,  1.D12, &     ! D
     &              0,  5,  1.D0, &      ! E
     &             40,  5,  1.D0, &      ! F
     &              0,  6,  HR_INV, &    ! G
     &             40,  6,  1.D0, &      ! H
     &              0,  7,  HR_INV, &    ! I
     &             40,  7,  3600.D0 &    ! K
     &           /
      INTEGER*2  INT2_ARG
      INTEGER*4  INT4
      INT4(INT2_ARG) = INT(INT2_ARG,KIND=4)
      INTEGER*4, EXTERNAL :: I_LEN, ILEN
!CCCCC
      FFID = .FALSE.
      LF_STA = 1
!
! ---- Set flags of participation station for SCADAM algorithm
!
      DO 410 J1=1,MG_STA
         IF ( J1 .LE.  DBOBJ%L_STA ) THEN
!
              IF ( SCAINF%P_STA(J1) .AND. SCAINF%FID_STA .EQ. J1 ) THEN
                   FFID = .TRUE.
              END IF
              IF ( SCAINF%P_STA(J1) ) LF_STA = J1
         END IF
 410  CONTINUE
!
! --- If fiducial station is not among participating then we assign the last
! --- participating station from the list as a fiducial one
!
      IF ( .NOT. FFID ) THEN
           SCAINF%FID_STA = LF_STA
      END IF
!
! --- Keep some SCADAM parameters in intermediary array (for facilitating
! --- coding)
!
      VAL_MN(1)  = SCAINF%XGR_LIM
      VAL_MN(2)  = SCAINF%SGR_LIM
      VAL_MN(3)  = SCAINF%XPH_LIM
      VAL_MN(4)  = SCAINF%SPH_LIM
      VAL_MN(5)  = SCAINF%DEFRG
      VAL_MN(6)  = SCAINF%ARFMS
      VAL_MN(7)  = SCAINF%FRZTR
      VAL_MN(8)  = SCAINF%ARFFLO
      VAL_MN(9)  = SCAINF%SPL_SPAN
      VAL_MN(10) = SCAINF%SPL_CNST
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
      CALL ADDSTR_F ( 'SCADAM (phase delay ambiguity resolution algorithm)' )
      CALL NL_MN()
      CALL ADDSTR_F ( '같같같같같같같같같같같같같같같같같같같같'// &
     &                '같같같같같같같같같같같같같같같같같같같�' )
      CALL NL_MN()
!
! --- Print at the screen values of real*8 type parameters and their values
!
      DO 420 J2=1,M_MN
!
! ------ Position cursor
!
         CALL SETCR_MN ( IX_MN(J2), IY_MN(J2) )
         CALL CLRCH ( STR )
!
! ------ Put explanatory text
!
         STR = C_MN(J2)(1:I_LEN(C_MN(J2))+1)
!
! ------ Put the value...
!
         WRITE ( UNIT=STR1, FMT='(F10.3)' ) VAL_MN(J2)*SCALE_MN(J2)
         CALL CHASHL   ( STR1 )
         IF ( STR1(1:1) .EQ. '.' ) STR1='0'//STR1
!
! ------ Special formatting for esthetic purposes
!
         IP = INDEX ( STR1, '.000' )
         IF ( IP .GT. 0 ) CALL CLRCH ( STR1(IP+2:) )
!
         IF ( INDEX ( STR1, '.' ) .GT. 0  .AND. &
     &        STR1(I_LEN(STR1):I_LEN(STR1)) .EQ. '0' ) STR1(I_LEN(STR1):)= ' '
         IF ( INDEX ( STR1, '.' ) .GT. 0  .AND. &
     &        STR1(I_LEN(STR1):I_LEN(STR1)) .EQ. '0' ) STR1(I_LEN(STR1):)= ' '
!
         IF ( STR1(I_LEN(STR1):I_LEN(STR1)) .EQ. '.' ) STR1(I_LEN(STR1)+1:)= '0'
!
         STR = STR(1:I_LEN(STR)+1)//STR1
!
! ------ ... and units
!
         STR = STR(1:I_LEN(STR))//' '//UNIT_MN(J2)(1:I_LEN(UNIT_MN(J2)) )
!
! ------ ... and then -- put it at the screen
!
         CALL ADDSTR_F ( STR(1:39) )
 420  CONTINUE
!
! --- Misclosure control
!
      CALL SETCR_MN ( 0, 8 )
      CALL ADDSTR_F ( '(!) Misclosure control  ' )
      CALL REVERSE_ON_MN()
      IF ( SCAINF%MSC_CONTROL ) THEN
           CALL ADDSTR_F ( ' on' )
         ELSE
           CALL ADDSTR_F ( 'off' )
      END IF
      CALL REVERSE_OFF_MN()
!
! --- ARF type
!
      CALL SETCR_MN ( 40, 8 )
      CALL ADDSTR_F ( '(T) ARF type: ' )
      CALL INCH     ( SCAINF%ARF_TYPE, STR )
      CALL CHASHL   ( STR )
      CALL REVERSE_ON_MN()
      CALL ADDSTR_F ( STR(1:1) )
      CALL REVERSE_OFF_MN()
!
! --- ARF Plotting
!
      CALL SETCR_MN (  0, 9 )
      CALL ADDSTR_F ( '(K) Plot ARF before filtering ' )
      CALL REVERSE_ON_MN()
      IF ( SCAINF%PLOT_INI .EQ. 0 ) THEN
           CALL ADDSTR_F ( ' no' )
         ELSE IF ( SCAINF%PLOT_INI .EQ. 1 ) THEN
           CALL ADDSTR_F ( 'yes' )
      END IF
      CALL REVERSE_OFF_MN()
!
      CALL SETCR_MN ( 40, 9 )
      CALL ADDSTR_F ( '(L) Plot ARF after filtering ' )
      CALL REVERSE_ON_MN()
      IF ( SCAINF%PLOT_FIN .EQ. 0 ) THEN
           CALL ADDSTR_F ( ' no' )
         ELSE IF ( SCAINF%PLOT_FIN .EQ. 1 ) THEN
           CALL ADDSTR_F ( 'yes' )
      END IF
      CALL REVERSE_OFF_MN()
!
! --- Print station status
!
      CALL SETCR_MN ( 0, IY_STA-2 )
      CALL ADDSTR_F ( '           Station status:' )
      CALL SCADAM_MENU_STAPRI ( IY_STA, SCAINF, DBOBJ )
!
! --- Print bottom line
!
      CALL SETCR_MN ( 0, IY_BOT )
      CALL ADDSTR_F ( '같같같같같같같같같같같같같같같같같같같같'// &
     &                '같같같같같같같같같같같같같같같같같같같�' )
      CALL NL_MN()
!
! --- Print proposed actions
!
      CALL ADDSTR_F ( '(*) Go ahead;  (-) Go back;  (S) Change station status' )
      CALL NL_MN()
      CALL SETCR_MN ( 1, IY_BOT+1 )
!
! --- Awaiting for entering information
!
      CALL SENKR_MN ( IX, IY, CC4 )
      SIM = CC4(4:4)
      CALL TRAN ( 11, SIM, SIM )
!
! --- Transforming cursor coordinamtes in letters
!
      IF ( SIM .EQ. ' '  .OR.  SIM .EQ. CHAR(13) ) THEN
!
! -------- Read cursor positioning and get a letter code
!
           IF        ( IY .EQ.  2  .AND.  IX .LT. 40 ) THEN
                SIM = 'A'
             ELSE IF ( IY .EQ.  2  .AND.  IX .GE. 40 ) THEN
                SIM = 'B'
             ELSE IF ( IY .EQ.  3  .AND.  IX .LT. 40 ) THEN
                SIM = 'C'
             ELSE IF ( IY .EQ.  3  .AND.  IX .GE. 40 ) THEN
                SIM = 'D'
             ELSE IF ( IY .EQ.  5  .AND.  IX .LT. 40 ) THEN
                SIM = 'E'
             ELSE IF ( IY .EQ.  5  .AND.  IX .GE. 40 ) THEN
                SIM = 'F'
             ELSE IF ( IY .EQ.  6  .AND.  IX .LT. 40 ) THEN
                SIM = 'G'
             ELSE IF ( IY .EQ.  7  .AND.  IX .LT. 40 ) THEN
                SIM = 'H'
             ELSE IF ( IY .EQ.  7  .AND.  IX .GE. 40 ) THEN
                SIM = 'I'
             ELSE IF ( IY .EQ.  8  .AND.  IX .LT. 40 ) THEN
                SIM = '!'
             ELSE IF ( IY .EQ.  8  .AND.  IX .GE. 40 ) THEN
                SIM = 'T'
             ELSE IF ( IY .EQ.  9  .AND.  IX .LT. 40 ) THEN
                SIM = 'K'
             ELSE IF ( IY .EQ.  9  .AND.  IX .GE. 40 ) THEN
                SIM = 'L'
           END IF
           IF ( IY .EQ. 22  .AND.  IX .LE. 14                    ) SIM = '*'
           IF ( IY .EQ. 22  .AND.  IX .GE. 15  .AND.  IX .LE. 28 ) SIM = '-'
           IF ( IY .EQ. 22  .AND.  IX .GE. 29  .AND.  IX .LE. 53 ) SIM = 'S'
      END IF
!
! --- IMN -- index of real*8 type value
!
      IMN = INDEX ( LET, SIM )
!
      IF ( IMN .GE. 1  .AND.  IMN .LE. M_MN ) THEN
!
! -------- We have to change the value of real*8 type parameter
!
 920       CONTINUE
!
! -------- Clear a botoom line
!
           CALL SETCR_MN ( 0, IY_BOT+1 )
           CALL ADDSTR_F ( '                                        '// &
     &                     '                                       ' )
           CALL SETCR_MN ( 0, IY_BOT+1 )
!
! -------- Print explanation and old value
!
           CALL ADDSTR_F ( 'Old value: ' )
           CALL ADDSTR_F ( C_MN(IMN)(1:I_LEN(C_MN(IMN))+1) )
           WRITE ( UNIT=STR, FMT='(F10.3)' ) VAL_MN(IMN)*SCALE_MN(IMN)
           CALL CHASHL   ( STR )
           IF ( STR(1:1) .EQ. '.' ) STR='0'//STR
           CALL ADDSTR_F ( STR(1:I_LEN(STR))//' '// &
     &                     UNIT_MN(IMN)(1:I_LEN(UNIT_MN(IMN)) ) )
!
! -------- Print a prompt
!
           CALL ADDSTR_F ( '  NEW VALUE: >> ' )
!
! -------- Getting a string which user input
!
           CALL CLRCH    ( STR )
           CALL GETSTR_F ( STR )
           IF ( ILEN(STR) .EQ. 0 ) GOTO 910 ! Empty line -- nothing to do
!
! -------- Decoding
!
           IF ( INDEX ( STR, '.' ) .LE. 0 ) STR=STR(1:I_LEN(STR))//'.'
           READ ( UNIT=STR(1:I_LEN(STR)), FMT='(F8.4)', IOSTAT=IO ) VAL_NEW
           IF ( IO .NE. 0   .OR.   VAL_NEW .LT. VAL_LIM ) THEN
!
! ------------- An error in decoding was detected. Clear the line
!
                CALL SETCR_MN ( 0, IY_BOT+1 )
                CALL ADDSTR_F ( '                                        '// &
     &                          '                                       ' )
                CALL SETCR_MN ( 0, IY_BOT+1 )
!
! ------------- Print an explanation
!
                CALL REVERSE_ON_MN()
                CALL ADDSTR_F ( 'WRONG INPUT: '//STR(1:I_LEN(STR)) )
                CALL REVERSE_OFF_MN()
!
! ------------- ... and awaiting for user to hit a line
!
                CALL SETCR_MN ( 50, IY_BOT+1 )
                CALL SENKR_MN ( IX, IY, CC4 )
                GOTO 920
           END IF
           VAL_MN(IMN) = VAL_NEW/SCALE_MN(IMN)
!
         ELSE IF ( SIM .EQ. '*' ) THEN
!
! -------- Action: go ahead
!
           ICOND = 1
           GOTO 810
         ELSE IF ( SIM .EQ. '-' ) THEN
!
! -------- Action: go back
!
           ICOND = 0
           GOTO 810
         ELSE IF ( SIM .EQ. '!' ) THEN
!
! -------- Misclosure control
!
           SCAINF%MSC_CONTROL = .NOT.  SCAINF%MSC_CONTROL
         ELSE IF ( SIM .EQ. 'T' ) THEN
!
! -------- ARF type
!
           SCAINF%ARF_TYPE = SCAINF%ARF_TYPE + 1
           IF ( SCAINF%ARF_TYPE .GT. ARFTYPE__MAX ) THEN
                SCAINF%ARF_TYPE = ARFTYPE__MIN
           END IF
         ELSE IF ( SIM .EQ. 'S' ) THEN
!
! -------- Change status of the stations for SCADAM algorithm
!
           CALL SCADAM_MENU_STACHA ( IY_STA, IY_BOT, SCAINF, DBOBJ )
         ELSE IF ( SIM .EQ. 'K' ) THEN
!
! -------- Change "before filtering" plotting status
!
           SCAINF%PLOT_INI = SCAINF%PLOT_INI + 1
           IF ( SCAINF%PLOT_INI .GT. SCAINF_PLOT_MAX ) THEN
                SCAINF%PLOT_INI = SCAINF_PLOT_MIN
           END IF
         ELSE IF ( SIM .EQ. 'L' ) THEN
!
! -------- Change "after filtering" plotting status
!
           SCAINF%PLOT_FIN = SCAINF%PLOT_FIN + 1
           IF ( SCAINF%PLOT_FIN .GT. SCAINF_PLOT_MAX ) THEN
                SCAINF%PLOT_FIN = SCAINF_PLOT_MIN
           END IF
      END IF
      GOTO 910
!
 810  CONTINUE
!
      CALL CLEAR_MN ()
      CALL END_MN()
      CALL UN_CURSES() ! Elimination of the influence of curses
      CALL CLEAR ( 0, 0 )
!
! --- Putting real*8 type values back to SCAINF data structures
!
      SCAINF%XGR_LIM  = VAL_MN(1)
      SCAINF%SGR_LIM  = VAL_MN(2)
      SCAINF%XPH_LIM  = VAL_MN(3)
      SCAINF%SPH_LIM  = VAL_MN(4)
      SCAINF%DEFRG    = VAL_MN(5)
      SCAINF%ARFMS    = VAL_MN(6)
      SCAINF%FRZTR    = VAL_MN(7)
      SCAINF%ARFFLO   = VAL_MN(8)
      SCAINF%SPL_SPAN = VAL_MN(9)
      SCAINF%SPL_CNST = VAL_MN(10)
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  SCADAM_MENU  #!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE SCADAM_MENU_STAPRI ( IY_STA, SCAINF, DBOBJ )
! ************************************************************************
! *                                                                      *
! *   Auxiliary  riutine  SCADAM_MENU_STAPRI  prints at the screen the   *
! *   list of stations to be processed (or not processed) by SCADAM      *
! *   algorithm for phase delay ambiguity resolution. The list of        *
! *   stations starts from the IY_STA-th lines of the screen (numbered   *
! *   from zero).                                                        *
! *                                                                      *
! *   It is assumed that curses is already running.                      *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *  IY_STA ( INTEGER*4 ) -- Number of the initial line at the screen.   *
! *   DBOBJ ( RECORD    ) -- Data structure which keeps general          *
! *                          information about the database such as      *
! *                          lists of the objects.                       *
! *  SCAINF ( RECORD    ) -- Data structure which keeps a) values of     *
! *                          parameters which control work of algorithm  *
! *                          SCADAM; b) result of work of algorithm      *
! *                          SCADAM.                                     *
! *                                                                      *
! * ### 22-OCT-98  SCADAM_MENU_STAPRI  v1.0 (c) L. Petrov 22-OCT-98  ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'solve.i'
      INCLUDE   'obser.i'
      INCLUDE   'pamb.i'
      TYPE ( DBOBJ_O__STRU ) ::  DBOBJ
      TYPE ( SCAINF__STRU ) ::  SCAINF
      INTEGER*4  IY_STA
      INTEGER*4  J1, J2, IS
      CHARACTER  SIM*1, STR*18, LET*32
      DATA  LET / '1234567890ABCDEFGHIJKLMNOPQRSTUV' /
!
      IS = 0
!
! --- Cycle on columns
!
      DO 410 J1=1,4
!
! ------ Cycles on raws
!
         DO 420 J2=1,8
            IS = IS + 1 ! Increment station counter
            IF ( IS .GT. DBOBJ%L_STA ) GOTO 810
            CALL CLRCH ( STR )
            IF ( SCAINF%P_STA(IS) ) THEN
                 SIM = 'x' ! station on
               ELSE
                 SIM = '-' ! station off
            END IF
!
! --------- Set symbol of fidutail station if eligible
!
            IF ( IS .EQ. SCAINF%FID_STA  .AND.  SCAINF%P_STA(IS) ) SIM = 'o'
!
! --------- Printin a line at the screen
!
            STR = '('//LET(IS:IS)//') '//DBOBJ%C_STA(IS)//' '//SIM
            CALL SETCR_MN ( (J1-1)*LEN(STR), IY_STA+J2-1 )
            CALL ADDSTR_F ( STR )
 420     CONTINUE
 410  CONTINUE
 810  CONTINUE
!
      RETURN
      END  !#!  SCADAM_MENU_STAPRI  #!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE SCADAM_MENU_STACHA ( IY_STA, IY_BOT, SCAINF, DBOBJ )
! ************************************************************************
! *                                                                      *
! *   Auxilary routine SCADAM_MENU_STACHA wroks as a part of SCADAM_MENU *
! *   and alow user to change in interactice mode a) participation       *
! *   status of stations; b) fiducial station.                           *
! *                                                                      *
! *   It is assumed that curser is already running. Its status is not    *
! *   changed.                                                           *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *  IY_STA ( INTEGER*4 ) -- Number of the initial line at the screen.   *
! *  IY_BOT ( INTEGER*4 ) -- Number of the bottom line at the screen.    *
! *   DBOBJ ( RECORD    ) -- Data structure which keeps general          *
! *                          information about the database such as      *
! *                          lists of the objects.                       *
! *  SCAINF ( RECORD    ) -- Data structure which keeps a) values of     *
! *                          parameters which control work of algorithm  *
! *                          SCADAM; b) result of work of algorithm      *
! *                          SCADAM.                                     *
! *                                                                      *
! * ###  22-OCT-98  SCADAM_MENU_STACHA  v1.0 (c) L. Petrov 22-OCT-98 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'solve.i'
      INCLUDE   'obser.i'
      INCLUDE   'pamb.i'
      TYPE ( DBOBJ_O__STRU ) ::  DBOBJ
      TYPE ( SCAINF__STRU ) ::  SCAINF
      INTEGER*4  IY_STA, IY_BOT
      INTEGER*4  J1, J2, J3, IS, ICOL, IMODE, IX, IY
      CHARACTER  STR*18, LET*32, CC4*4, SIM*1
      DATA  LET / '1234567890ABCDEFGHIJKLMNOPQRSTUV' /
!
! --- 3 modes can be used:
! --- IMODE = 1 -- genereal menu is bid
! --- IMODE = 2 -- user changes participating station
! --- IMODE = 3 -- user changes fiducial station
!
      IMODE = 1
 910  CONTINUE
         CALL CLEAR_MN()
         CALL SETCR_MN ( 0, IY_STA-2 )
         CALL ADDSTR_F ( '           Station status:' )
!
! ------ Print station status on the screen
!
         CALL SCADAM_MENU_STAPRI ( IY_STA, SCAINF, DBOBJ )
!
         CALL SETCR_MN ( 0, IY_BOT )
         CALL ADDSTR_F ( '같같같같같같같같같같같같같같같같같같같같'// &
     &                '같같같같같같같같같같같같같같같같같같같�' )
         CALL NL_MN()
!
! ------ Printing a prompt for possible actions
!
         IF ( IMODE .EQ. 1 ) THEN
              CALL SETCR_MN ( 0, IY_BOT+1 )
              CALL ADDSTR_F ( '(-) Go back; (P) Change participation status; '// &
     &                        '(F) Change fiducial station' )
              CALL SETCR_MN ( 1, IY_BOT+1 )
            ELSE IF ( IMODE .EQ. 2 ) THEN
              CALL SETCR_MN ( 0, IY_BOT+1 )
              CALL ADDSTR_F ( 'Change station participation status' )
              CALL SETCR_MN ( 0, IY_BOT+2 )
              CALL ADDSTR_F ( '(-) Go back; (Z) Deselect all; '// &
     &                        '(W) Select all; <letter_code> Toggle status' )
              CALL SETCR_MN ( 1, IY_BOT+2 )
            ELSE IF ( IMODE .EQ. 3 ) THEN
              CALL SETCR_MN ( 0, IY_BOT+1 )
              CALL ADDSTR_F ( '(-) Go back; <letter_code> Change fiducial '// &
     &                        'station' )
              CALL SETCR_MN ( 1, IY_BOT+1 )
         END IF
!
! ------ Awaiting for entering information
!
         CALL SENKR_MN ( IX, IY, CC4 )
         SIM = CC4(4:4)
!
! ------ TRansform it to large registr
!
         CALL TRAN ( 11, SIM, SIM )
         IF ( SIM .EQ. CHAR(13) ) SIM = ' '
         IF ( IMODE .EQ. 1 ) THEN
!
! ----------- Set one of the modes
!
              IF ( SIM .EQ. ' ' ) THEN
                   IF ( IX .GE. 0   .AND.  IX .LE. 12 ) SIM = '-'
                   IF ( IX .GE. 13  .AND.  IX .LE. 45 ) SIM = 'P'
                   IF ( IX .GE. 46  .AND.  IX .LE. 79 ) SIM = 'F'
              END IF
!
              IF ( SIM .EQ. '-' ) THEN
                   RETURN
                 ELSE IF ( SIM .EQ. 'P' ) THEN
                   IMODE = 2
                 ELSE IF ( SIM .EQ. 'F' ) THEN
                   IMODE = 3
              END IF
           ELSE IF ( IMODE .EQ. 2 ) THEN
!
! ----------- Change participation status
!
              IF ( SIM .EQ. ' ' ) THEN
!
! ---------------- Check, where cursor was positioned
!
                   IF ( IY .EQ. IY_BOT+2 ) THEN
!
! --------------------- ... to some field at the bottom line
!
                        IF ( IX .GE. 0   .AND.  IX .LE. 12 ) SIM = '-'
                        IF ( IX .GE. 13  .AND.  IX .LE. 30 ) SIM = 'Z'
                        IF ( IX .GE. 31  .AND.  IX .LE. 46 ) SIM = 'W'
                     ELSE IF ( IY .GE. IY_STA  .AND.  IY .LE. IY_BOT-1 ) THEN
!
! --------------------- ... to station
!
                        ICOL = IX/LEN(STR) + 1
                        IF ( MOD(IX+LEN(STR),LEN(STR)) .EQ. 0 ) ICOL = ICOL-1
                        IS = (ICOL-1)*8 + (IY - IY_STA + 1)
                        IF ( IS .GE. 1  .AND.  IS .LE. DBOBJ%L_STA ) THEN
                             SIM = LET(IS:IS)
                        END IF
                   END IF
              END IF
!
              IF ( SIM .EQ. '-' ) THEN
                   IMODE = 1
                 ELSE IF ( SIM .EQ. 'Z' ) THEN
!
! ---------------- Lift participation flag
!
                   DO 410 J1=1,DBOBJ%L_STA
                      SCAINF%P_STA(J1) = .FALSE.
 410               CONTINUE
                 ELSE IF ( SIM .EQ. 'W' ) THEN
!
! ---------------- Set participation flag
!
                   DO 420 J2=1,DBOBJ%L_STA
                      SCAINF%P_STA(J2) = .TRUE.
 420               CONTINUE
                 ELSE
                   IS = INDEX ( LET, SIM )
                   IF ( IS .GE. 1  .AND.  IS .LE. DBOBJ%L_STA ) THEN
!
! --------------------- Toggle status
!
                        SCAINF%P_STA(IS) = .NOT. SCAINF%P_STA(IS)
                   END IF
              END IF
              IF ( .NOT. SCAINF%P_STA(SCAINF%FID_STA) ) THEN
                   DO 430 J3=1,DBOBJ%L_STA
                      IF ( SCAINF%P_STA(J3) ) THEN
                           SCAINF%FID_STA = J3
                           GOTO 810
                      END IF
 430               CONTINUE
 810               CONTINUE
              END IF
            ELSE IF ( IMODE .EQ. 3 ) THEN
!
! ----------- Change fiducial station
!
              IF ( SIM .EQ. ' ' ) THEN
!
! ---------------- Check, where cursor was positioned
!
                   IF ( IY .EQ. IY_BOT+1 ) THEN
!
! --------------------- ... to some field at the bottom line
!
                        IF ( IX .GE. 0   .AND.  IX .LE. 12 ) SIM = '-'
                     ELSE IF ( IY .GE. IY_STA  .AND.  IY .LE. IY_BOT-1 ) THEN
!
! --------------------- ... to station
!
                        ICOL = IX/LEN(STR) + 1
                        IF ( MOD(IX+LEN(STR),LEN(STR)) .EQ. 0 ) ICOL = ICOL-1
                        IS = (ICOL-1)*8 + (IY - IY_STA + 1)
                        IF ( IS .GE. 1  .AND.  IS .LE. DBOBJ%L_STA ) THEN
                             SIM = LET(IS:IS)
                        END IF
                   END IF
              END IF
!
              IF ( SIM .EQ. '-' ) THEN
                   IMODE = 1
                ELSE
                   IS = INDEX ( LET, SIM )
                   IF ( IS .GE. 1  .AND.  IS .LE. DBOBJ%L_STA ) THEN
!
! --------------------- Set new fiducial statiuon
!
                        SCAINF%FID_STA = IS
                   END IF
              END IF
         END IF
      GOTO 910
!
      END  !#!  SCADAM_MENU_STACHA  #!#
