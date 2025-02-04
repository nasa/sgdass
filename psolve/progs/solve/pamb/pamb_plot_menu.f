      SUBROUTINE PAMB_PLOT_MENU ( VER_PAMB, M_SOU, ISR, ISS_SOU, LSEL_SOU, &
     &                            PAMB_PLOT_BAND, PAMB_PLOT_TYPE, &
     &                            PAMB_PSL_TYPE, IACT )
! ************************************************************************
! *                                                                      *
! *   Routine  PAMB_PLOT_MENU  takes parameters of the plot for PAMB in  *
! *   menu mode.                                                         *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *  VER_PAMB ( CHARACTER ) -- String with PAMB-identifier and number    *
! *                            of the current version.                   *
! *                                                                      *
! * ________________________ Output parameters: ________________________ *
! *                                                                      *
! * PAMB_PLOT_BAND ( INTEGER*4 ) -- Code of the band to be used in       *
! *                                 plotting. Codes are in pamb.i        *
! * PAMB_PLOT_TYPE ( INTEGER*4 ) -- Type of the plot. Codes are in       *
! *                                 pamb.i                               *
! *  PAMB_PSL_TYPE ( INTEGER*4 ) -- Type of suppression criteria. Codes  *
! *                                 are in pamb.i                        *
! *           IACT ( INTEGER*4 ) -- Action code. Following codes are     *
! *                                 supported:                           *
! *                              IACT =  0 -- nothing to do;             *
! *                              IACT = 11 -- make plot;                 *
! *                              IACT = 2  -- select one source;         *
! *                              IACT = 3  -- select many sources;       *
! *                                                                      *
! *  ###  27-MAY-98  PAMB_PLOT_MENU  v2.1  (c) L. Petrov 02-NOV-98  ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT    NONE
      INCLUDE   'solve.i'
      INCLUDE   'pamb.i'
      INTEGER*4  PAMB_PLOT_BAND, PAMB_PLOT_TYPE, PAMB_PSL_TYPE, &
     &           M_SOU, ISR, ISS_SOU(M_SOU)
      LOGICAL*4  LSEL_SOU(M_SOU)
      CHARACTER  VER_PAMB*(*), STR*80, CC4*4, SIM*1, LET*26
      INTEGER*4  J1, IX, IY, IACT
      INTEGER*4, EXTERNAL :: I_LEN, ILEN
      DATA LET  / 'ABCDEFGHIJKLMNOPQRSTUWXYZ' /
!
! --- Start curser
!
      IACT = 0
      CALL START_MN()
 910  CONTINUE
!
! --- Printing the first line: title of the program
!
      CALL CLEAR_MN()
      CALL SETCR_MN (  0, 0 )
      CALL ADDSTR_F ( 'Plot type selection for PAMB' )
      CALL SETCR_MN (  79-ILEN(VER_PAMB), 0 )
      CALL ADDSTR_F ( VER_PAMB )
      CALL NL_MN()
      CALL ADDSTR_F ( '같같같같같같같같같같같같같같같같같같같같'// &
     &                '같같같같같같같같같같같같같같같같같같같�'    )
      CALL NL_MN()
!
      CALL ADDSTR_F ( '(?) On-line help' )
      CALL NL_MN()
      CALL NL_MN()
!
      CALL ADDSTR_F ( '(^) Band for plotting: ' )
      CALL REVERSE_ON_MN()
      CALL ADDSTR_F ( BAND_STR(PAMB_PLOT_BAND) )
      CALL REVERSE_OFF_MN()
      CALL NL_MN()
      CALL NL_MN()
!
      CALL ADDSTR_F ( '(0) ' )
      IF ( ISR .EQ. 0 ) CALL REVERSE_ON_MN
      CALL ADDSTR_F ( 'Select all sources' )
      IF ( ISR .EQ. 0 ) CALL REVERSE_OFF_MN
      CALL NL_MN()
      CALL NL_MN()
!
      CALL ADDSTR_F ( '(1) ' )
      IF ( ISR .GT. 0 ) CALL REVERSE_ON_MN
      CALL ADDSTR_F ( 'Select one source' )
      IF ( ISR .GT. 0 ) CALL REVERSE_OFF_MN
      CALL NL_MN()
      CALL NL_MN()
!
      CALL ADDSTR_F ( '(#) ' )
      IF ( ISR .LT. 0 ) CALL REVERSE_ON_MN
      CALL ADDSTR_F ( 'Select multiple sources' )
      IF ( ISR .LT. 0 ) THEN
           CALL CLRCH ( STR )
           CALL INCH ( -ISR, STR )
           STR = ' ('//STR(1:I_LEN(STR))//')'
           CALL ADDSTR_F ( STR(1:I_LEN(STR)) )
           CALL REVERSE_OFF_MN()
      END IF
      CALL NL_MN()
      CALL NL_MN()
!
      CALL ADDSTR_F ( '(\) ' )
      CALL REVERSE_ON_MN()
      CALL ADDSTR_F ( PSL_STR(PAMB_PSL_TYPE)(1:I_LEN(PSL_STR(PAMB_PSL_TYPE))) )
      CALL REVERSE_OFF_MN()
      CALL NL_MN()
      CALL NL_MN()
!
      DO 410 J1=1,PAMB_PTP_MAX
         CALL SETCR_MN ( 50, J1+1 )
         CALL CLRCH    ( STR )
         STR = LET(J1:J1)//') '//PTP_STR(J1)
         IF ( J1 .EQ. PAMB_PLOT_TYPE ) CALL REVERSE_ON_MN
         CALL ADDSTR_F ( STR(1:I_LEN(STR)) )
         IF ( J1 .EQ. PAMB_PLOT_TYPE ) CALL REVERSE_OFF_MN
 410  CONTINUE
!
      CALL SETCR_MN ( 0, 21 )
      CALL ADDSTR_F ( '________________________________________'// &
     &                '_______________________________________'   )
      CALL NL_MN()
!
      CALL ADDSTR_F ( '(-) Go back to main menu' )
      CALL ADDSTR_F ( '                         ' )
      CALL ADDSTR_F ( '(>) Draw the plot' )
!
! --- Awaiting for entering information
!
      CALL SETCR_MN ( 50, 22 )
      CALL REFRESH_MN()
      CALL SENKR_MN ( IX, IY, CC4 )
      SIM = CC4(4:4)
      CALL TRAN ( 11, SIM, SIM )
!
      IF ( SIM .EQ. ' '  .OR.  SIM .EQ. CHAR(13) ) THEN
           IF ( IX .LT. 32 ) THEN
                IF ( IY .EQ.  2 ) SIM = '?'
                IF ( IY .EQ.  4 ) SIM = '^'
                IF ( IY .EQ.  6 ) SIM = '0'
                IF ( IY .EQ.  8 ) SIM = '1'
                IF ( IY .EQ. 10 ) SIM = '#'
                IF ( IY .EQ. 12 ) SIM = '\'
                IF ( IY .EQ. 22 ) SIM = '-'
              ELSE
                IF ( IY-1 .GE. 1  .AND. IY-1 .LE. PAMB_PTP_MAX ) THEN
                     SIM = LET(IY-1:)
                END IF
                IF ( IY .EQ. 22 ) SIM = '>'
           END IF
      END IF
!
      IF ( SIM .EQ. '?' ) THEN
        ELSE IF ( SIM .EQ. '^' ) THEN
!
! -------- Change of band used
!
           IF ( PAMB_PLOT_BAND .GE. PAMB_BAND_MAX ) THEN
                PAMB_PLOT_BAND = PAMB_BAND_MIN
              ELSE
                PAMB_PLOT_BAND = PAMB_PLOT_BAND + 1
           END IF
        ELSE IF ( SIM .EQ. '0' ) THEN
           ISR = 0
        ELSE IF ( SIM .EQ. '1' ) THEN
           IACT = 1
           GOTO 810
        ELSE IF ( SIM .EQ. '#' ) THEN
           IACT = 2
           GOTO 810
        ELSE IF ( SIM .EQ. '\' ) THEN
           PAMB_PSL_TYPE = PAMB_PSL_TYPE + 1
           IF ( PAMB_PSL_TYPE .GT. PSL__MAX ) PAMB_PSL_TYPE = PSL__MIN
        ELSE IF ( SIM .EQ. '-' ) THEN
           IACT = 0
           GOTO 810
        ELSE IF ( SIM .EQ. '>' ) THEN
           IACT = 11
           IF ( PTP_ABR(PAMB_PLOT_TYPE) .EQ. 'Gdc'  .OR. &
     &          PTP_ABR(PAMB_PLOT_TYPE) .EQ. 'Pdc'        ) THEN
!
                IACT=3
           END IF
           GOTO 810
        ELSE IF ( INDEX ( LET(1:PAMB_PTP_MAX), SIM ) .GT. 0 ) THEN
           PAMB_PLOT_TYPE = INDEX ( LET(1:PAMB_PTP_MAX), SIM )
      END IF
      GOTO 910
!
 810  CONTINUE
      CALL CLEAR_MN ()
      CALL END_MN()
      CALL UN_CURSES() ! Elimination of the influence of curses
      CALL CLEAR ( 0, 0 )
!
      RETURN
      END  !#!  PAMB_PLOT_MENU  #!#
