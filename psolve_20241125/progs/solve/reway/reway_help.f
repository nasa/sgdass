      SUBROUTINE REWAY_HELP ( VER_REWAY )
! ************************************************************************
! *                                                                      *
! *   Ancillary siutine REWAY_HELP prints on the screen on-line help     *
! *   information about REWAY routine.                                   *
! *                                                                      *
! *  ###  16-FEB-99   REWAY_HELP   v1.0  (c)  L. Petrov  16-FEB-99  ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'solve.i'
      INCLUDE   'help.i'
      INCLUDE   'glbc4.i'
      INTEGER*4  IUER, MBUF, NBUF, ISIM, J1, IMEN, ICOL, ILIN
      PARAMETER  ( MBUF = 1024 )
      CHARACTER  VER_REWAY*(*), BUF(MBUF)*160, ASIM*1, FINAM*255
      CHARACTER  PRE_INIT*32, POST_INIT*32, PREF*32, ESC*1, &
     &           SOLVE_PS_VIEWER_USE*128
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
      IM = MAKE_HELP_FINAM ( REWAY_HELP_00, FINAM )
      IF ( IM.NE.0 ) THEN
           CALL ERR_LOG ( 6791, -1, 'REWAY_HELP', 'Help file '// &
     &          REWAY_HELP_00//' is not found. Check directory '// &
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
           CALL ERR_LOG ( 6792, -1, 'REWAY_HELP', 'Error during openning '// &
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
              BUF(1)(ICOL-ILEN(VER_REWAY):) = VER_REWAY
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
           IM = MAKE_HELP_FINAM ( REWAY_HELP_01, FINAM )
           IF ( IM.NE.0 ) THEN
                CALL ERR_LOG ( 6793, -1, 'REWAY_HELP', 'Help file '// &
     &               REWAY_HELP_01//' is not found. Check directory '// &
     &               SOLVE_HELP_DIR//' and environment variable SOLVE_HELP_DIR' )
                CALL HIT_CONT ( '--- Hit any key to proceed --- '//CHAR(1), &
     &                          %VAL(0) )
                GOTO 810
           END IF
!
           IUER = -1
           CALL SHOW_TEXT_FILE_COL ( FINAM, 'Description of REWAY menu items', &
     &                               1, IUER )
!
           IF ( IUER .NE. 0 ) THEN
                CALL ERR_LOG ( 6794, -1, 'REWAY_HELP', 'Error during openning '// &
     &              'file '//FINAM(1:I_LEN(FINAM))//' with help information' )
                CALL HIT_CONT ( '--- Hit any key to proceed --- '//CHAR(1), &
     &                          %VAL(0) )
                GOTO 810
           END IF
         ELSE IF ( IMEN .EQ. 2 ) THEN
!
! -------- Displaying 2-nd menu item (in PostScript mode)
!
           IM = MAKE_HELP_FINAM ( REWAY_HELP_02, FINAM )
           IF ( IM.NE.0 ) THEN
                CALL ERR_LOG ( 6795, -1, 'REWAY_HELP', 'Help file '// &
     &               REWAY_HELP_02//' is not found. Check directory '// &
     &               SOLVE_HELP_DIR//' and environment variable SOLVE_HELP_DIR' )
                CALL HIT_CONT ( '--- Hit any key to proceed --- '//CHAR(1), &
     &                          %VAL(0) )
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
                CALL ERR_LOG ( 6796, -1, 'REWAY_HELP', 'Environment '// &
     &              'variable SHELL has wrong value. Error in running Shell '// &
     &              'command: '//SOLVE_PS_VIEWER_USE(1:I_LEN(SOLVE_PS_VIEWER_USE))// &
     &              ' '//FINAM )
             ELSE IF ( IP .NE. 0 ) THEN
                CALL ERR_LOG ( 6797, -1, 'REWAY_HELP', 'Error in running Shell'// &
     &              ' command: '//SOLVE_PS_VIEWER_USE(1:I_LEN(SOLVE_PS_VIEWER_USE))// &
     &              ' '//FINAM )
           END IF
           CALL HIT_CONT ( %VAL(0), %VAL(0) )
         ELSE IF ( IMEN .EQ. 3 ) THEN
!
! -------- Displaying 3-rd menu item (in PostScript mode)
!
           IM = MAKE_HELP_FINAM ( REWAY_HELP_03, FINAM )
           IF ( IM.NE.0 ) THEN
                CALL ERR_LOG ( 6798, -1, 'REWAY_HELP', 'Help file '// &
     &               REWAY_HELP_02//' is not found. Check directory '// &
     &               SOLVE_HELP_DIR//' and environment variable SOLVE_HELP_DIR' )
                CALL HIT_CONT ( '--- Hit any key to proceed --- '//CHAR(1), &
     &                          %VAL(0) )
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
                CALL ERR_LOG ( 6799, -1, 'REWAY_HELP', 'Environment '// &
     &              'variable SHELL has wrong value. Error in running Shell '// &
     &              'command: '//SOLVE_PS_VIEWER_USE(1:I_LEN(SOLVE_PS_VIEWER_USE))// &
     &              ' '//FINAM )
             ELSE IF ( IP .NE. 0 ) THEN
                CALL ERR_LOG ( 6800, -1, 'REWAY_HELP', 'Error in running Shell'// &
     &              ' command: '//SOLVE_PS_VIEWER_USE(1:I_LEN(SOLVE_PS_VIEWER_USE))// &
     &              ' '//FINAM )
           END IF
           CALL HIT_CONT ( %VAL(0), %VAL(0) )
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
      END  !#!  REWAY_HELP  #!#
