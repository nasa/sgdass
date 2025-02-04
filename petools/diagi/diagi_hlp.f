      SUBROUTINE DIAGI_HLP ( FINAM, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  DIAGI_HLP  prints at the current device PGPLOT the        *
! *   text of the helper for DiaGI.                                      *
! *                                                                      *
! * ________________________ Input parameters: _________________________ *
! *                                                                      *
! *   FINAM ( CHARACTER ) -- File name which will be displayed. Blank    *
! *                          means default (HELP_FIL defined in diagi.i) *
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
! *   IUER ( INTEGER*4, OPT ) -- Universal error habdler.                *
! *                           Input: swicth IUER=0 -- no error messages  *
! *                                  will be generated even in the case  *
! *                                  of error. IUER=-1 -- in the case of *
! *                                  error the message will pe put on    *
! *                                  stdout.                             *
! *                           Output: 0 in the case of successfull       *
! *                                   completion and non-zero in the     *
! *                                   case of error.                     *
! *                                                                      *
! *  ###  17-OCT-97    DIAGI_HLP   v1.1  (c)  L. Petrov  21-JUL-99  ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'diagi.i'
      INCLUDE   'diagi_local.i'
      INTEGER*4  IUER, IER, NHLP, IADD_HLP
      CHARACTER  FINAM*(*)
      CHARACTER  BUF(MHLP)*80, HELP_FIL*255, HELP_TIT*32, HELP_DIR_STR*255, &
     &           CH*1, STR*80
      REAL*4     XC, YC, XCT, YCT
      INTEGER*4, EXTERNAL :: ILEN
!
      CALL CLRCH ( HELP_DIR_STR )
      CALL GETENVAR ( 'HELP_DIR', HELP_DIR_STR )
      IF ( ILEN(HELP_DIR_STR) .GT. 0 ) THEN
!
! -------- If the last symbol of the value of environment variable is not
! -------- "/" then adding it to the end
!
           IF ( HELP_DIR_STR(ILEN(HELP_DIR_STR):ILEN(HELP_DIR_STR)) .NE. '/' ) &
     &     THEN
                HELP_DIR_STR(ILEN(HELP_DIR_STR)+1:) = '/'
           END IF
         ELSE 
           HELP_DIR_STR = PETOOLS_PREFIX//'/doc'
      END IF
!
! --- Setting file name for help file and the line of its title
!
      CALL CLRCH ( HELP_FIL )
      CALL CLRCH ( HELP_TIT )
      HELP_FIL = DIAGI_HLP_FIL0
      IF ( ILEN(HELP_DIR_STR) .GT. 0  .AND. INDEX(HELP_FIL, '/') .EQ. 0 ) THEN
!
! -------- Adding the directory name before the name of help-file
!
           IF ( FINAM(1:1) .EQ. ' ' ) THEN
                HELP_FIL = HELP_DIR_STR(1:ILEN(HELP_DIR_STR))//HELP_FIL
              ELSE IF ( FINAM(1:1) .EQ. '/' ) THEN
                HELP_FIL = FINAM
              ELSE
                HELP_FIL = HELP_DIR_STR(1:ILEN(HELP_DIR_STR))//FINAM
           END IF
      END IF
      HELP_TIT = DIAGI_HLP_TIT0
!
! --- Reading the file with help information
!
      CALL ERR_PASS ( IUER, IER )
      CALL RD_TEXT  ( HELP_FIL, MHLP, BUF, NHLP, IER )
      IF ( IER .NE.0 ) THEN
           CALL ERR_LOG ( 4141, IUER, 'DIAGI_HLP', 'Error during the '// &
     &         'attempt to read help file' )
           RETURN
      END IF
      IF ( FINAM(1:1) .NE. ' ' ) THEN
           CALL CLRCH ( HELP_TIT )
           CALL CLRCH ( STR      )
           STR = BUF(1)
           CALL CHASHL ( STR     )
           HELP_TIT = STR
      END IF
      IADD_HLP = 0  ! Setting attribute: no additional help file has been read
 910  CONTINUE
!
! --- Deleting previous window
!
      CALL PGERAS
!
      CALL PGSAVE ! 1
!
! --- Setting new world coodrinates
!
      CALL PGSVP  ( 0.0, 1.0, 0.0, 1.0 )
      CALL PGSWIN ( 0.0, 1.0, 0.0, 1.0 )
!
! --- Printing by large letters the title of help information
!
      CALL PGSLW  ( ISLW_HLT )
      CALL PGSCF  ( ISCF_HLT )
      CALL PGSCH  (  SCH_HLT )
      XCT = 0.50
      YCT = 0.96
      CALL PGPTXT ( XCT, YCT, 0.0, 0.5, HELP_TIT )
!
! --- Shrinking a bit the viewing surface to leave the title unchanged when
! --- the text buffer will be printed
!
      XC = XCT
      YC = YCT
      CALL PGSVP  ( 0.0, 1.0, 0.0, 0.948 )
!
! --- Printing the content of help buffer at the graphic window.
!
      CALL PGSLW  ( ISLW_HLP )
      CALL PGSCF  ( ISCF_HLP )
      CALL PGSCH  (  SCH_HLP )
      CALL PGTBF  ( NHLP-2, BUF(3), CH ) ! Skipping first two lines
!
! --- Restoring the current colour index and other PGPLOT current parameters
!
      CALL PGSCI  ( 1   )
      CALL PGUNSA !
!
      IF ( ( CH .EQ. '1'  .OR.  CH .EQ. '2' )  .AND.  IADD_HLP .EQ. 0 ) THEN
!
! -------- Setting file name with additional help file
!
           CALL CLRCH ( HELP_FIL )
           CALL CLRCH ( HELP_TIT )
           IF ( CH .EQ. '1' ) THEN
                HELP_FIL = DIAGI_HLP_FIL1
                HELP_TIT = DIAGI_HLP_TIT1
             ELSE IF ( CH .EQ. '2' ) THEN
                HELP_FIL = DIAGI_HLP_FIL2
                HELP_TIT = DIAGI_HLP_TIT2
           END IF
           IF ( ILEN(HELP_DIR_STR) .GT. 0  .AND. INDEX(HELP_FIL, '/') .EQ. 0 ) &
     &     THEN
!
! ------------- Adding the directory name before the name of help-file
!
                HELP_FIL = HELP_DIR_STR(1:ILEN(HELP_DIR_STR))//HELP_FIL
           END IF
!
! -------- Reading the file with help information
!
           CALL ERR_PASS ( IUER, IER )
           CALL RD_TEXT  ( HELP_FIL, MHLP, BUF, NHLP, IER )
           IF ( IER .NE.0 ) THEN
                CALL ERR_LOG ( 4142, IUER, 'DIAGI_HLP', 'Error during the '// &
     &              'attempt to read additional help file' )
                RETURN
           END IF
           IADD_HLP = 1  ! Setting attribute: additional help file has been read
           GOTO 910
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  DIAGI_HLP  #!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE PGTBF ( NBUF, BUF, CH )
! ************************************************************************
! *                                                                      *
! *   Routine PGTBF clears the graphic screen and prints the content of  *
! *   the text buffer at the currently opened interactive PGPLOT device. *
! *   It leaves the text at the graphic screen after the end of its      *
! *   work. Text is printed in accordance to the current font, font      *
! *   size and line width. If there is not space enough to put all lines *
! *   of the text at the screen then button "Down" and button "Up" will  *
! *   be pictured at the bottom of the graphic screen. Displayed text    *
! *   will be scrolled down when 1) the middle mouse button was hit or   *
! *   2) any key was hit when the cursor is pointed to the button        *
! *   "down". Displayed text will be scrolled up when 1) the left        *
! *   mouse button was hit or 2) any symbol was hit when the cursor is   *
! *   pointed to the button "up". The subroutine will terminate when any *
! *   other symbol will be hit.                                          *
! *                                                                      *
! * _______________________ Input parameters: __________________________ *
! *                                                                      *
! *   NBUF ( INTEGER*4 ) -- the number of lines in the buffer to be      *
! *                         displayed.                                   *
! *    BUF ( CHARACTER ) -- The buffer to be displayed. Dimension: NBUF. *
! *                                                                      *
! * _______________________ Output parameters: _________________________ *
! *                                                                      *
! *     CH ( CHARACTER ) -- The last symbol which has been entered by    *
! *                         user just before termination of work of the  *
! *                         subroutine.                                  *
! *                                                                      *
! *  ###  20-OCT-97     PGTBF      v1.0  (c)  L. Petrov  20-OCT-97  ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INTEGER*4  NBUF
      CHARACTER  BUF(NBUF)*(*), CH*1, LIN_STR*32, STR*20
      REAL*4     XL, YL, XC, YC, X1W, X2W, Y1W, Y2W
      INTEGER*4  J1, IXL, IYL, ILB, IB, IE, ILN_COU
      REAL*4     XLB_DOWN, XTU_DOWN, YLB_DOWN, YTU_DOWN, &
     &           XLB_UP,   XTU_UP,   YLB_UP,   YTU_UP, &
     &           YBB, YBT
      INTEGER*4, EXTERNAL :: ILEN, I_LEN
!
! --- World coordinates of the rectangular "Down" button
!
      PARAMETER  ( XLB_DOWN = 0.75  )  !  "x" left bottom
      PARAMETER  ( XTU_DOWN = 0.90  )  !  "x" top up
      PARAMETER  ( YLB_DOWN = 0.001 )  !  "y" left bottom
      PARAMETER  ( YTU_DOWN = 0.041 )  !  "y" top up
!
! --- World coordinates of the rectangular "Up" button
!
      PARAMETER  ( XLB_UP   = 0.10  )  !  "x" left bottom
      PARAMETER  ( XTU_UP   = 0.25  )  !  "x" top  up
      PARAMETER  ( YLB_UP   = 0.001 )  !  "y" left up
      PARAMETER  ( YTU_UP   = 0.041 )  !  "y" top  up
!
! --- World coordinates of the bar
!
      PARAMETER  ( YBT      = 0.048 )  !  top    "y" coordinate
      PARAMETER  ( YBB      = 0.046 )  !  bottom "y" coordinate
!
! --- Remember window world coordinates
!
      CALL PGQWIN ( X1W, X2W, Y1W, Y2W )
!
! --- Save current PGPLOT parameters
!
      CALL PGSAVE ! 1
!
! --- Set new window coordinates
!
      CALL PGSWIN ( 0.0, 1.0, 0.0, 1.0 )
!
! --- Inquire the length (in x and y coordinates) of the letter K
!
      CALL PGLEN  ( 4, 'K', XL, YL )
!
! --- Calculateion IXL -- the number of symbols per line
! ---              IYL -- the number of lines per screen
!
      IXL = 1.0/(XL)
      IYL = 0.95/(1.5*YL)
!
! --- Set initial index of the first displayed line (IB),
! --- and the last displayed line (IE)
!
      IB  = 1
      IE  = IYL
      IF ( NBUF .LE. IE ) IE=NBUF
 910  CONTINUE
      CALL PGBBUF  !  Beginning of bufferization
!
! --- Erasing the content of the screen
!
      CALL PGSAVE ! 2
      CALL PGSFS  ( 1 )
      CALL PGSCI  ( 0 )
      CALL PGRECT ( 0.0, 1.0, 0.0, 1.0 )
      CALL PGUNSA ! 2
!
! --- Displaying lines from IB-th till IE-th
!
      ILN_COU = 0
      DO 410 J1=IB,IE
         ILN_COU = ILN_COU + 1
         XC = 0.0
         YC = 1.0 - YL*1.5*ILN_COU
         ILB = I_LEN(BUF(J1))
         CALL PGPTXT  ( XC, YC, 0.0, 0.0, BUF(J1)(1:ILB) )
 410  CONTINUE
!
! --- Printing thick line at the bottom
!
      CALL PGSFS  ( 1 )
      CALL PGSCI  ( 1 )
      CALL PGRECT ( 0.0, 1.0, YBB, YBT )
!
      IF ( NBUF .GT. IYL ) THEN
!
! -------- If it is not full content of the text -- then ...
!
           CALL PGSAVE ! 3
!
! -------- Printing the rectungular box near the left bottom corner
!
           CALL PGSCI  ( 3   )
           CALL PGSFS  ( 1   )
           CALL PGSCH  ( 1.5 )
           CALL PGSLW  ( 1 )
           CALL PGRECT ( XLB_DOWN, XTU_DOWN, YLB_DOWN, YTU_DOWN )
           CALL PGSCI  ( 1   )
           CALL PGSFS  ( 2   )
           CALL PGSCH  ( 1.5 )
           CALL PGRECT ( XLB_DOWN, XTU_DOWN, YLB_DOWN, YTU_DOWN )
           CALL PGSLW  ( 6 )
           CALL PGSCI  ( 2 )
!
! -------- And printing the text "Down" within
!
           CALL PGPTXT ( (XLB_DOWN+XTU_DOWN)/2., &
     &                   YLB_DOWN + 0.2*(YTU_DOWN-YLB_DOWN), 0.0, 0.5, &
     &                   'Down' )
           CALL PGUNSA ! 3
!
! -------- Printing outlined rectangular near the right bottom corner
!
           CALL PGSAVE ! 4
           CALL PGSCI  ( 3   )
           CALL PGSFS  ( 1   )
           CALL PGSCH  ( 1.5 )
           CALL PGSLW  ( 1 )
           CALL PGRECT ( XLB_UP, XTU_UP, YLB_UP, YTU_UP )
           CALL PGSCI  ( 1   )
           CALL PGSFS  ( 2   )
           CALL PGSCH  ( 1.5 )
           CALL PGRECT ( XLB_UP, XTU_UP, YLB_UP, YTU_UP )
!
! -------- And printing the text "Up" within
!
           CALL PGSLW  ( 6 )
           CALL PGSCI  ( 2 )
           CALL PGPTXT ( (XLB_UP+XTU_UP)/2., YLB_UP + 0.2*(YTU_UP-YLB_UP), &
     &                   0.0, 0.5, 'Up' )
           CALL PGUNSA ! 4
!
! -------- Prepearing the text about the displayed lines range and the number
! -------- of lines in the buffer
!
           CALL CLRCH ( LIN_STR )
           CALL INCH  ( IB, STR )
           LIN_STR = 'Lines: '//STR
           CALL INCH  ( IE, STR )
           LIN_STR(ILEN(LIN_STR)+1:)='-'//STR
           CALL INCH  ( NBUF, STR )
           LIN_STR(ILEN(LIN_STR)+1:)='('//STR(1:I_LEN(STR))//')'
!
! -------- Printing this message
!
           CALL PGSAVE ! 5
           CALL PGSLW  ( 6 )
           CALL PGSCH  ( 1.5 )
           CALL PGPTXT ( 0.5, YLB_DOWN + 0.2*(YTU_DOWN-YLB_DOWN), 0.0, 0.5, &
     &              LIN_STR(1:I_LEN(LIN_STR)) )
           CALL PGUNSA ! 5
      END IF
      CALL PGEBUF  !  End of bufferization
      CALL PGUPDT  !  Updating of the screen
!
! --- calculation the coordinates of the cursor
!
      IF ( IB .GT. 1    ) XC = XLB_UP   + 0.1*(XTU_UP - XLB_UP)
      IF ( IE .LT. NBUF ) XC = XLB_DOWN + 0.1*(XTU_DOWN - XLB_DOWN)
      YC = (YLB_DOWN + YTU_DOWN)/2.
!
! --- Reading the cursor position and entered symbol
!
 920  CONTINUE
      CALL PGBAND ( 0, 1, XC, YC, XC, YC, CH )
!
! --- Check: was cursor position at the rectangulaer "Down" or "Up"
!
      IF ( XC .GE. XLB_DOWN  .AND.  XC .LE. XTU_DOWN  .AND. &
     &     YC .GE. YLB_DOWN  .AND.  YC .LE. YTU_DOWN        ) THEN
           CH = 'D'
      END IF
!
      IF ( XC .GE. XLB_UP .AND.  XC .LE. XTU_UP .AND. &
     &     YC .GE. YLB_UP .AND.  YC .LE. YTU_UP       ) THEN
           CH = 'U'
      END IF
!
      IF ( NBUF .GT. IYL ) THEN
!
! ------ If the number of lines in buffer exceeds the number of lines at the
! ------ screen we analyse hit he input and make one of hte action:
!
         IF ( CH .EQ. 'D'   .OR.   CH .EQ. 'd' ) THEN
!
! ----------- ... 2) Show the portion of the text "Down" with the respect to
! ----------- the current point
!
              IF ( IE .LT. NBUF ) THEN
                   IB = IE + 1
                   IE = IB + IYL-1
                   IF ( IE .GT. NBUF ) IE = NBUF
                   GOTO 910
                ELSE
!
! ---------------- Command "Down" was issued but the last line is already
! ---------------- shown
!
                   GOTO 920
              END IF
           ELSE IF ( CH .EQ. 'A'   .OR.   CH .EQ. 'U'  .OR.  CH .EQ. 'u' ) THEN
!
! ----------- ... 1) Show the portion of the text "Up" with the respect to
! ----------- the current point
!
              IF ( IB .GT. 1 ) THEN
                   IB = IB - IYL
                   IF ( IB .LT. 1 ) IB = 1
                   IE = IB + (IYL-1)
                   GOTO 910
                ELSE
!
! ---------------- Command "Up" was issued but the first line is already
! ---------------- shown
!
                   GOTO 920
              END IF
           ELSE
!
! ----------- ... 3) or to leave the program.
!
              CONTINUE
         END IF
      END IF
!
! --- Restoring world coordinate space
!
      CALL PGSWIN  ( X1W, X2W, Y1W, Y2W )
      CALL PGUNSA  ! 1
!
      RETURN
      END  !#!  PGTBF  #!#
