      SUBROUTINE SHOW_TEXT_FILE_COL ( FINAM, TITLE, ICL, IUER )
! ************************************************************************
! *                                                                      *
! *   Ancillary subroutine SHOW_TEXT_FILE_COL provides simplified        *
! *   interfase for SHOW_TEXT_FILE . It displays the content of the      *
! *   file in window with changed color.                                 *
! *                                                                      *
! *   If an environment variable TERM_COLOR is set to NO then no colors  *
! *   are changed.                                                       *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *   FINAM ( CHARACTER ) -- File name to be displayed.                  *
! *   TITLE ( CHARACTER ) -- Title which be printed in status line.      *
! *                          No more than 40 symbols can be displayed at *
! *     ICL ( INTEGER*4 ) -- Color code.                                 *
! *                       ICL = 0 -- usual colors (the same as current   *
! *                                  color of the screen).               *
! *                       ICL = 1 -- background color is green.          *
! *                       ICL = 2 -- background color is blue.           *
! *                       ICL = 3 -- background color is red.            *
! *                       ICL = 4 -- background color is yellow.         *
! *                                                                      *
! * _______________________ Modified parameters: _______________________ *
! *                                                                      *
! *   IUER ( INTEGER*4, OPT ) -- Universal error handler.                *
! *                           Input: switch IUER=0 -- no error messages  *
! *                                  will be generated even in the case  *
! *                                  of error. IUER=-1 -- in the case of *
! *                                  error the message will pe put on    *
! *                                  stdout.                             *
! *                           Output: 0 in the case of successful        *
! *                                   completion and non-zero in the     *
! *                                   case of error.                     *
! *                                                                      *
! *   For other information see comments to SHOW_TEXT_FILE.              *
! *                                                                      *
! * ###  21-AUG-97 SHOW_TEXT_FILE_COL v1.2 (c) L. Petrov 24-MAR-2000 ### *
! *                                                                      *
! ************************************************************************
      CHARACTER  FINAM*(*), TITLE*(*)
      INTEGER*4  ICL, IUER
      CHARACTER  PRE_INIT*32, PREF*32, POST*32, POST_INIT*32, ESC*1, STR*20
      INTEGER*4  IT, IG, IP, IST, IRUS
      INTEGER*4, EXTERNAL :: I_LEN
      LOGICAL*4, EXTERNAL :: USE_TERM_COLOR
!
      ESC = CHAR(27)
      CALL CLRCH ( PRE_INIT  )
      CALL CLRCH ( POST_INIT )
      CALL CLRCH ( PREF      )
      CALL CLRCH ( POST      )
      CALL SHOW_IO ( IT, IG, IP, IST, IRUS )
!
      IF ( ICL .EQ. 0 ) THEN
         ELSE IF ( ICL .EQ. 1 ) THEN
!
! -------- Green background
!
           IF ( IT .EQ. 6  .AND.  USE_TERM_COLOR ()  ) THEN
                PRE_INIT  = ESC//'&v0m0.41x0.76y0.39z2I'
                POST_INIT = ESC//'&v0m1b1x1y1z2I'
                PREF = ESC//'&v2S'
           END IF
        ELSE IF ( ICL .EQ. 2 ) THEN
!
! -------- Blue background
!
           IF ( IT .EQ. 6 .AND.  USE_TERM_COLOR () ) THEN
                PRE_INIT  = ESC//'&v0m0.38x0.62y0.99z2I'
                POST_INIT = ESC//'&v0m1b1x1y1z2I'
                PREF = ESC//'&v2S'
           END IF
        ELSE IF ( ICL .EQ. 3 ) THEN
!
! -------- Red background
!
           IF ( IT .EQ. 6  .AND.  USE_TERM_COLOR () ) THEN
                PRE_INIT  = ESC//'&v0m0.99x0.39y0.05z2I'
                POST_INIT = ESC//'&v0m1b1x1y1z2I'
                PREF = ESC//'&v2S'
           END IF
        ELSE IF ( ICL .EQ. 4 ) THEN
!
! -------- Yellow background
!
           IF ( IT .EQ. 6  .AND.  USE_TERM_COLOR () ) THEN
                PRE_INIT  = ESC//'&v0m0.99x0.85y0.01z2I'
                POST_INIT = ESC//'&v0m1b1x1y1z2I'
                PREF = ESC//'&v2S'
           END IF
        ELSE
           CALL CLRCH ( STR )
           CALL INCH  ( ICL, STR )
           CALL ERR_LOG ( 4111, IUER, 'SHOW_TEXT_FILE_COL', 'Parameter ICL '// &
     &         'has wrong values: '//STR(1:I_LEN(STR))//': out of range '// &
     &         '[0,4]' )
           RETURN
      END IF
      CALL PRCH ( PRE_INIT  )
      CALL SHOW_TEXT_FILE ( FINAM, TITLE, PREF, POST, -1, IUER )
      CALL PRCH ( POST_INIT )
      CALL CLEAR ( 1, 1 )
      RETURN
      END  !#!  SHOW_TEXT_FILE_COL  #!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE SHOW_TEXT_FILE ( FINAM, TITLE, PREF, POST, LN, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine SHOW_TEXT_FILE is callable interface for displaying at the *
! *   screen content of text file. For non-hpterm it merely calls UNIX   *
! *   command more. But if it recoginize that the terminal is hpterm     *
! *   (by examining environment variable TERM) it provides more flexible *
! *   interface for displaying textual information. It allows to move in *
! *   forward and backward diretion by lines (with smooth scrolling )    *
! *   or by pages. It adjustable. If user change the size of the window  *
! *   SHOW_TEXT_FILE will trace these changes. SHOW_TEXT_FILE provides   *
! *   the possibility to display file with some visual effects. For      *
! *   example display file wich changed color. Example of such a usage   *
! *   of SHOW_TEXT_FILE can be found in SHOW_TEXT_FILE_COL               *
! *                                                                      *
! *   If an environment variable TERM_COLOR is set to NO then no colors  *
! *   are changed.                                                       *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *   FINAM ( CHARACTER ) -- File name to be displayed.                  *
! *   TITLE ( CHARACTER ) -- Title which be printed in status line.      *
! *                          No more than 40 symbols can be displayed at *
! *                          the standard screen 24x80.                  *
! *    PREF ( CHARACTER ) -- Prefix which is printed before each line.   *
! *                          It is assumed that it contains              *
! *                          ESC-sequences which don't take place.       *
! *                          Should be empty stting "" if we don't like  *
! *                          to use it.                                  *
! *    POST ( CHARACTER ) -- Postfix which is printed after each line.   *
! *                          It is assumed that it contains              *
! *                          ESC-sequences which don't take place.       *
! *                          Should be empty stting "" if we don't like  *
! *                          to use it.                                  *
! *      LN ( INTEGER*4 ) -- lentgh os lines. 0 means that all symbols   *
! *                          of the lines will be printed; -1 means      *
! *                          that all symbols except trailing blanks     *
! *                          will be printed. Values LN > 0 allows us to *
! *                          restrit the length of printed lines.        *
! *                                                                      *
! * _______________________ Modified parameters: _______________________ *
! *                                                                      *
! *   IUER ( INTEGER*4, OPT ) -- Universal error handler.                *
! *                           Input: switch IUER=0 -- no error messages  *
! *                                  will be generated even in the case  *
! *                                  of error. IUER=-1 -- in the case of *
! *                                  error the message will pe put on    *
! *                                  stdout.                             *
! *                           Output: 0 in the case of successful        *
! *                                   completion and non-zero in the     *
! *                                   case of error.                     *
! *                                                                      *
! *  How to use it.                                                      *
! *                                                                      *
! *    SHOW_TEXT display one page of the file and waits for the command. *
! *    STATUS line displays TITLE, range of displayed lines and (in      *
! *    brackets) total number of lines. Comment: at very narrow window   *
! *    STATUS line can be restricted.                                    *
! *    Commands may be one of the                                        *
! *    1) Move one page down (unless it is not the last page):           *
! *       <PgDn> or <SHIFT_Arrow_down>.                                  *
! *    2) Move one page up (unless it is not the first page):            *
! *       <PgUp> or <SHIFT_Arrow_up> .                                   *
! *    3) Move one line down (unless it is not the last line):           *
! *       <Arrow_down> or <Return>.                                      *
! *    4) Move one line up (unless it is not the first line):            *
! *       <Arrow_up>.                                                    *
! *    5) Refresh the screen: due to the bug in hpterm during fast work  *
! *       display sometime is corrupted. Display should be refreshed     *
! *       also after changing window size.                               *
! *       <R> or <W>.                                                    *
! *    6) Exit from subroutine. <E> <Q> or any CNTRL key.                *
! *    7) Print at the screen brief help. All other not mentioned keys.  *
! *                                                                      *
! *  Restrictions:                                                       *
! *                                                                      *
! *    1) File should contain no more than 4096 lines;                   *
! *    2) Each line should contain no more than 255 symbols;             *
! *    3) Width of the display shoud be in range  [3,512];               *
! *    4) Length of the display shoud be in range [3,512].               *
! *                                                                      *
! *  ###  21-AUG-97 SHOW_TEXT_FILE v1.1  (c)  L. Petrov 24-MAR-2000 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      CHARACTER  FINAM*(*), TITLE*(*), PREF*(*), POST*(*)
      INTEGER*4  LN, IUER
      INTEGER*4  ICOL_DEF, ILIN_DEF, MBUF, ICOL, ILIN, L, J1, J2, NBUF, &
     &           NB, NE, ISIM, ICODE, MHLP, IT, IG, IP, IST, NSTR, NCOL, &
     &           IS, IRUS
      INTEGER*4, EXTERNAL :: ILEN, I_LEN, INSIM, SYSTEM
      PARAMETER  ( MBUF = 4096 )
      PARAMETER  ( ICOL_DEF = 80 )
      PARAMETER  ( ILIN_DEF = 80 )
      PARAMETER  ( MHLP     = 10 )
      CHARACTER  BUF(MBUF)*255, STR*255, STATUS*255, ASIM
      LOGICAL*4  F_EXIST
      LOGICAL*4  USE_TERM_COLOR
      CHARACTER  HELP_BUF(MHLP)*57
#ifdef SUN
      DATA ( HELP_BUF(J1), J1=1,MHLP ) &
     &    / &
     &    '|-------------------------------------------------------|', &
     &    '|  SHOW_TEXT_FILE  Command synopsis:                    |', &
     &    '|~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~|', &
     &    '| <Return> or <Arror_Down>       -- Move one line down. |', &
     &    '| <Arrow_Up>                     -- Move one line up.   |', &
     &    '| <PgDn> or <SHIHT_Arrow_Down>   -- Move one page down. |', &
     &    '| <PgUp> or <SHIHT_Arrow_Up>     -- Move one page up.   |', &
     &    '| <R>    or <W>                  -- Refresh screen      |', &
     &    '| <E> or <Q> or <CNTRL/<any_key> -- Exit                |', &
     &    '|_______________________________________________________|' &
     &    /
#else
      DATA ( HELP_BUF(J1), J1=1,MHLP ) &
     &    / &
     &    '/-------------------------------------------------------\', &
     &    '|  SHOW_TEXT_FILE  Command synopsis:                    |', &
     &    '|~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~|', &
     &    '| <Return> or <Arror_Down>       -- Move one line down. |', &
     &    '| <Arrow_Up>                     -- Move one line up.   |', &
     &    '| <PgDn> or <SHIHT_Arrow_Down>   -- Move one page down. |', &
     &    '| <PgUp> or <SHIHT_Arrow_Up>     -- Move one page up.   |', &
     &    '| <R>    or <W>                  -- Refresh screen      |', &
     &    '| <E> or <Q> or <CNTRL/<any_key> -- Exit                |', &
     &    '\_______________________________________________________/' &
     &    /
#endif
!
! --- Learn terminal type
!
      CALL SHOW_IO ( IT, IG, IP, IST, IRUS )
      IF ( IT .NE. 6  ) THEN
!
! -------- ALas, not hpterm. Then we call "less". If less is not found then
! -------- we call more.
!
! -------- For the first: test is file available?
!
           INQUIRE ( FILE=FINAM, EXIST=F_EXIST )
           IF ( .NOT. F_EXIST ) THEN
                CALL ERR_LOG ( 4101, IUER, 'SHOW_TEXT_FILE', 'File '// &
     &               FINAM(1:I_LEN(FINAM))//' is not found' )
                RETURN
           END IF
           CALL CLEAR ( 1, 1 )
           CALL PRCH ( 'loading file '//FINAM(1:I_LEN(FINAM)) )
           CALL PRCH ( ' ' ) 
!
! -------- Call less
!
           IP = SYSTEM ( 'less '//FINAM(1:I_LEN(FINAM))//CHAR(0) )
           IF ( IP .EQ. 32512 ) THEN
                CALL ERR_LOG ( 4102, IUER, 'SHOW_TEXT_FILE', 'Error '// &
     &              'launching subshell. Check your SHELL environment vaiable' )
                RETURN
           END IF
           IF ( IP .NE. 0 ) then
!
! ------------- If less was not found -- try more
!
                IP = SYSTEM ( 'more '//FINAM(1:I_LEN(FINAM))//CHAR(0) )
                IF ( IP .NE. 0 ) THEN
                     WRITE ( 6, * ) ' ip =',ip
                     CALL ERR_LOG ( 4103, IUER, 'SHOW_TEXT_FILE', 'Error '// &
     &                   'during launching shell command more' )
                     RETURN
                END IF
           END IF
!
! -------- And -- farwelling
!
           CALL HIT_CONT ( %VAL(0), %VAL(0) )
           CALL ERR_LOG ( 0, IUER )
           RETURN
      END IF
!
! --- hpterm mode.
!
! --- First -- read file in buffer BUF.
!
      IUER = -1
      CALL RD_TEXT ( FINAM, MBUF, BUF, NBUF, IUER )
      IF ( IUER .NE. 0 ) THEN
           CALL ERR_LOG ( 4104, IUER, 'SHOW_TEXT_FILE', 'Error during '// &
     &         'openning file '//FINAM )
           CALL HIT_CONT ( '--- Hit any key to proceed --- '//char(1), 0 )
           RETURN
      END IF
!
! --- Cursor -- at left upper corner
!
      CALL ADR_CURSOR ( 1, 1 )
      NB=1
!
! --- Now -- loop of commands
!
 910  CONTINUE
!
! ------- Learn window size
!
         CALL TERM_SIZE ( ILIN, ICOL )
         IF ( ILIN .GT. 512 ) ILIN = 512
         IF ( ILIN .LT. 3   ) ILIN = 3
         IF ( ICOL .GT. 216 ) ICOL = 216
         IF ( ICOL .LT. 3   ) ICOL = 3
!
! ------ Calculating NB and NE -- beginning and the end of printed page
!
         NE=NB+(ILIN-2)
         IF ( NE .LT. ILIN-1 ) NE=ILIN-1
         IF ( NE .GT. NBUF   ) NE=NBUF
!
! ------ Printing the current page
!
         DO 410 J1=NB,NB+(ILIN-2)
            CALL CLRCH ( STR )
            IF ( J1 .LE. NE ) THEN
!
! -------------- Adjust the width of the line
!
                 L=LN
                 IF ( L .EQ. 0                      ) L = I_LEN(BUF(J1))
                 IF ( L .LT. 0  .OR.  L .GT. ICOL-1 ) L = ICOL-1
                 IF ( ILEN(PREF) .GT. 0 ) THEN
                      STR = PREF(1:ILEN(PREF))//BUF(J1)(1:L)
                    ELSE
                      STR = BUF(J1)(1:L)
                 END IF
                 IF ( ILEN(POST) .GT. 0 ) STR(L+ILEN(PREF)+1:) = POST
                 STR(L+ILEN(PREF)+ILEN(POST)+1:) = CHAR(13)
                 IF ( J1 .EQ. NB ) THEN
!
! ------------------ Clearing the current temnial line
!
                     IF ( IT .EQ. 6 ) THEN
                          STR=CHAR(27)//'K'//STR
                        ELSE IF ( IT .EQ. 7 ) THEN
                          STR=CHAR(27)//'[2K'//STR
                     END IF
                   ELSE
!
! ------------------ Moving line down and clearing that line
!
                     IF ( IT .EQ. 6 ) THEN
                          STR=CHAR(10)//CHAR(27)//'K'//STR
                        ELSE IF ( IT .EQ. 7 ) THEN
                          STR=CHAR(27)//'[2K'//STR
                     END IF
                 END IF
              ELSE
!
! -------------- Empty space ahter the last line of the buffer. We merely
! -------------- clearn terminal
!
                 IF ( IT .EQ. 6 ) THEN
                      CALL PRCH ( CHAR(10)//CHAR(13)//CHAR(27)//'K' )
                    ELSE IF ( IT .EQ. 7 ) THEN
                      CALL PRCH ( CHAR(10)//CHAR(13)//CHAR(27)//'[2K' )
                 END IF
            END IF
            CALL PRCH ( STR )
 410     CONTINUE
 920     CONTINUE
!
! ------ Making status line
!
         CALL CLRCH ( STATUS )
         IF ( ICOL .GE. 40 ) THEN
              IS=I_LEN(TITLE)
              IF ( IS .GT. ICOL-38 ) IS = ICOL-38
              STATUS = TITLE(1:IS)
              STATUS(ICOL-36:) = '|'
         END IF
         CALL CLRCH ( STR )
         CALL INCH  ( NB,   STR(1:)           )
         STR(ILEN(STR)+1:)='-'
         CALL INCH  ( NE,   STR(ILEN(STR)+1:) )
         STR(ILEN(STR)+1:)='('
         CALL INCH  ( NBUF, STR(ILEN(STR)+1:) )
         STR(ILEN(STR)+1:)=')'
         IF ( ICOL .GE. 40 ) THEN
              STATUS(ICOL-34:) = STR(1:I_LEN(STR))
              STATUS(ICOL-19:) = '| Enter command >>'
           ELSE IF ( ICOL .GE. 15 ) THEN
              STATUS = STR
           ELSE
              STATUS = TITLE
         END IF
!
! ------ Cursor -- jump to the beginning of the last line
!
         CALL ADR_CURSOR ( ILIN, 1 )
!
! ------ Clearing it
!
         IF ( IT .EQ. 6 ) THEN
              CALL PRCH ( CHAR(13)//CHAR(27)//'K' )
            ELSE IF ( IT .EQ. 7 ) THEN
              CALL PRCH ( CHAR(13)//CHAR(27)//'[2K' )
         END IF
!
! ------ Switching NEGative mode on
!
         CALL NEG
!
! ------ Printing status line
!
         CALL PRCH ( STATUS(1:(ICOL-1)) )
!
! ------ Switching NEGative mode off
!
         CALL UN_NEG
!
! ------ Awaiting hitting key
!        ~~~~~~~~~~~~~~~~~~~~
!
         ICODE = INSIM ( ASIM, ISIM )
!
! ------ Clearing status line
!
         CALL ADR_CURSOR  ( ILIN, 1 )
         IF ( IT .EQ. 6 ) THEN
              CALL PRCH ( CHAR(27)//'K' )
            ELSE IF ( IT .EQ. 7 ) THEN
              CALL PRCH ( CHAR(27)//'[2K' )
         END IF
!
! ------ Parsing commands
!
         IF ( ICODE .EQ. ICHAR('D') .OR. ICODE .EQ. ICHAR('d') .OR. &
     &        ICODE .EQ. 529 .OR. ICODE .EQ. 528 ) THEN
!
! ----------- PgDn
!
              IF ( NE .LT. NBUF ) THEN
                   NE=NE + (ILIN-2)
                   IF ( NE .GT. NBUF ) NE=NBUF
                   NB=NE - (ILIN-2)
                   CALL ADR_CURSOR ( 1, 1 )
                   GOTO 910
              END IF
           ELSE IF ( ICODE .EQ. ICHAR('U') .OR. ICODE .EQ. ICHAR('u') .OR. &
     &               ICODE .EQ. 530 .OR. ICODE .EQ. 527 ) THEN
!
! ----------- PgUp
!
              IF ( NB .GT. 1 ) THEN
                   NB=NB - (ILIN-1)
                   IF ( NB .LT. 1 ) NB=1
                   NE=NB + (ILIN-1)-1
                   IF ( NE .GT. NBUF ) NE=NBUF
                   CALL ADR_CURSOR ( 1, 1 )
                   GOTO 910
              END IF
           ELSE IF ( ICODE .EQ. 13  .OR. ICODE .EQ. 10  .OR. &
     &               ICODE .EQ. 516 ) THEN
!
! ----------- Arrow_Down or <Return>
!
              IF ( NE .LT. NBUF ) THEN
                   NB=NB+1
                   NE=NE+1
                   L=LN
!
! ---------------- Printing the next line
!
                   IF ( L .EQ. 0                      ) L = I_LEN(BUF(NE))
                   IF ( L .LT. 0  .OR.  L .GT. ICOL-1 ) L = ICOL-1
                   CALL CLRCH ( STR )
                   IF ( ILEN(PREF) .GT. 0 ) THEN
                        STR = PREF(1:ILEN(PREF))//BUF(NE)(1:L)
                      ELSE
                        STR = BUF(NE)(1:L)
                   END IF
                   IF ( ILEN(POST) .GT. 0 ) STR(L+ILEN(PREF)+1:) = POST
                   STR(L+ILEN(PREF)+ILEN(POST)+1:) = CHAR(13)//CHAR(10)
                   CALL PRCH ( STR )
              END IF
              GOTO 920
           ELSE IF ( ICODE .EQ. 515 ) THEN
!
! ----------- Arrow_Up
!
              IF ( NB .GT. 1 ) THEN
                   NB=NB-1
                   NE=NE-1
                   L=LN
                   IF ( L .EQ. 0                      ) L = I_LEN(BUF(NB))
                   IF ( L .LT. 0  .OR.  L .GT. ICOL-1 ) L = ICOL-1
                   CALL CLRCH ( STR )
                   IF ( ILEN(PREF) .GT. 0 ) THEN
                        STR = PREF(1:ILEN(PREF))//BUF(NB)(1:L)
                      ELSE
                        STR = BUF(NB)(1:L)
                   END IF
                   IF ( ILEN(POST) .GT. 0 ) STR(L+ILEN(PREF)+1:) = POST
                   STR(L+ILEN(PREF)+ILEN(POST)+1:) = CHAR(13)
!
! ---------------- Scrolling down one line
!
                   IF ( IT .EQ. 6 ) THEN
                        CALL PRCH ( CHAR(27)//'T' )
                      ELSE IF ( IT .EQ. 7 ) THEN
                        CALL WHERE_CURSOR ( NSTR, NCOL )
                        CALL ADR_CURSOR  ( 1, NCOL )
                        CALL CURU ( 1 )
                        CALL ADR_CURSOR  ( NSTR, NCOL )
                   END IF
!
! ---------------- Clearing the first line
!
                   CALL ADR_CURSOR  ( 1, 1 )
                   IF ( IT .EQ. 6 ) THEN
                        CALL PRCH ( CHAR(27)//'K' )
                     ELSE IF ( IT .EQ. 7 ) THEN
                        CALL PRCH ( CHAR(27)//'[2K' )
                   END IF
!
! ---------------- Printing the first line
!
                   CALL PRCH ( STR )
                   GOTO 920
                ELSE
                   CALL CURU ( 1 )
                   GOTO 920
              END IF
           ELSE IF ( ICODE .EQ. ICHAR('R') .OR. ICODE .EQ. ICHAR('r') .OR. &
     &               ICODE .EQ. ICHAR('W') .OR. ICODE .EQ. ICHAR('w')     ) THEN
                CALL ADR_CURSOR  ( 1, 1 )
                IF ( IT .EQ. 6 ) THEN
                     CALL PRCH ( CHAR(27)//'K' )
                  ELSE IF ( IT .EQ. 7 ) THEN
                     CALL PRCH ( CHAR(27)//'[2K' )
                END IF
                GOTO 910
           ELSE IF ( ICODE .EQ. ICHAR('E') .OR. ICODE .EQ. ICHAR('e') .OR. &
     &               ICODE .EQ. ICHAR('Q') .OR. ICODE .EQ. ICHAR('q') .OR. &
     &               ICODE .LT. 32 ) THEN
              GOTO 810
           ELSE
!
! ------------- Dispaying brief help'er. Cursor -- to the position of the
! ------------- beginning of the help-text
!
                CALL ADR_CURSOR ( ILIN-MHLP+1, 1 )
!
! ------------- Defining yellow color
!
                IF ( IT .EQ. 6  .AND.  USE_TERM_COLOR () ) THEN
                     CALL PRCH ( CHAR(27)//'&v0m0.99x0.75y0.12z3I' )
                END IF
                DO 420 J2=1,MHLP
                   CALL ADR_CURSOR ( ILIN-MHLP+J2, 1 )
                   IF ( LEN(HELP_BUF(1)) .LT. ICOL-1 ) THEN
!
! --------------------- Adust helkper to the right edge
!
                        CALL CURR ( ICOL - LEN(HELP_BUF(1)) - 1 )
                   END IF
                   L=LEN(HELP_BUF(1))
                   IF ( L .GT. ICOL ) L=ICOL-1
!
! ---------------- Printing j2-th line of the help'er
!
                   IF ( IT .EQ. 6  .AND.  USE_TERM_COLOR ()  ) THEN
                        CALL PRCH ( CHAR(27)//'&v3S'//HELP_BUF(J2)(1:L)// &
     &                              CHAR(13) )
                      ELSE IF ( IT .EQ. 7 ) THEN
                        CALL PRCH ( HELP_BUF(J2)(1:L)//CHAR(13) )
                   END IF
 420            CONTINUE
!
! ------------- Awaiting any key
!
                IS = INSIM ( ASIM, ISIM )
                CALL ADR_CURSOR   ( 1, 1 )
!
! ------------- Undefining color definition
!
                IF ( IT .EQ. 6  .AND.  USE_TERM_COLOR () ) THEN
                     CALL PRCH  ( CHAR(27)//'&v0m1b1x1y1z3I' )
                END IF
                GOTO 910
         END IF
         GOTO 920
 810  CONTINUE
!
! --- Last good bye
!
      IF ( ILEN(PREF) .GT. 0 ) THEN
           CALL PRCH ( PREF(1:ILEN(PREF))//CHAR(13)//CHAR(10) )
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  SHOW_TEXT_FILE  #!#
