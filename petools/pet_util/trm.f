#include <mk5_preprocessor_directives.inc>
      FUNCTION  INSIM ( ASIM, ISIM )
! *************************************************************************
! *                                                                       *
! *   Function  INSIM  reads a symbol from the screen. If control key     *
! *   were hit  INSIM returns their 10-bit representation.                *
! *                                                                       *
! *   ###  26-DEC-89    INSIM    v 4.4  (c)  L. Petrov 17-MAR-2004  ###   *
! *                                                                       *
! *************************************************************************
      IMPLICIT  NONE
      INTEGER*4      ITERMINAL_TYPE, IGRAPHIC_TYPE, ICOLOR_TYPE, &
     &                 IPRINTER_TYPE, ISTATUS, IDEF, &
     &                 ITCH_CHAN, ITCH_ST, OTCH_CHAN, OTCH_ST, RUS_STAT, &
     &                 N_LINES, N_COLUMNS
      COMMON / OPTION_IO / ITERMINAL_TYPE, IGRAPHIC_TYPE, ICOLOR_TYPE, &
     &                       IPRINTER_TYPE, ISTATUS, IDEF, &
     &                       ITCH_CHAN, ITCH_ST, OTCH_CHAN, OTCH_ST, &
     &                       RUS_STAT, N_LINES, N_COLUMNS
      EXTERNAL BLKDAT_OPTION_IO
      CHARACTER  ASIM*1
!!      INTEGER*2  M_GETCH
      INTEGER*4  INSIM, ISIM, ITTI
      INTEGER*4  IT, IG, IP, IST, IRUS, ICODE
!
      CALL SHOW_IO ( IT, IG, IP, IST, IRUS )
      ICODE = ITTI ()
      INSIM = ICODE
!
      IF ( ICODE .EQ. 27 ) THEN
           ICODE = ITTI ()
           IF ( ICODE .EQ. 91 ) THEN
                ICODE = ITTI () ! 49
                IF ( ICODE .EQ. 49 ) THEN
                     ICODE = ITTI () 
                     IF ( ICODE .EQ. 59 ) THEN
                          ICODE = ITTI () 
                          IF ( ICODE .EQ. 50 ) THEN
                               ICODE = ITTI () 
                               IF ( ICODE .EQ. 65 ) THEN
                                    INSIM = 527 ! SHIFT__ARROW_UP
                                    GOTO 810
                                 ELSE IF ( ICODE .EQ. 66 ) THEN
                                    INSIM = 528 ! SHIFT__ARROW_DOWN
                                    GOTO 810
                                 ELSE IF ( ICODE .EQ. 67 ) THEN
                                    INSIM = 514 ! SHIFT__ARROW_RIGHT
                                    GOTO 810
                                 ELSE IF ( ICODE .EQ. 68 ) THEN
                                    INSIM = 513 ! SHIFT__ARROW_LEFT
                                    GOTO 810
                               END IF
                           END IF
                        ELSE IF ( ICODE .EQ. 53 ) THEN
                           INSIM = 536 ! UNDEFINED_F5
                           ICODE = ITTI () 
                           GOTO 810
                        ELSE IF ( ICODE .EQ. 55 ) THEN
                           INSIM = 537 ! UNDEFINED_F6
                           ICODE = ITTI () 
                           GOTO 810
                        ELSE IF ( ICODE .EQ. 56 ) THEN
                           INSIM = 538 ! UNDEFINED_F7
                           ICODE = ITTI () 
                           GOTO 810
                        ELSE IF ( ICODE .EQ. 57 ) THEN
                           INSIM = 539 ! UNDEFINED_F8
                           ICODE = ITTI () 
                           GOTO 810
                     END IF
                   ELSE IF ( ICODE .EQ. 50 ) THEN
                     ICODE = ITTI () 
                     IF ( ICODE .EQ. 65 ) THEN
                          INSIM = 527 ! SHIFT__ARROW_UP
                          GOTO 810
                       ELSE IF ( ICODE .EQ. 66 ) THEN
                          INSIM = 528 ! SHIFT__ARROW_DOWN
                          GOTO 810
                       ELSE IF ( ICODE .EQ. 67 ) THEN
                          INSIM = 514 ! SHIFT__ARROW_RIGHT
                          GOTO 810
                       ELSE IF ( ICODE .EQ. 68 ) THEN
                          INSIM = 513 ! SHIFT__ARROW_LEFT
                          GOTO 810
                       ELSE IF ( ICODE .EQ. 126 ) THEN
                          INSIM = 523 ! INSERT_LINE
                          GOTO 810
                       ELSE IF ( ICODE .EQ. 48 ) THEN
                          INSIM = 540 ! UNDEFINED_F9
                          ICODE = ITTI () 
                          GOTO 810
                       ELSE IF ( ICODE .EQ. 49 ) THEN
                          INSIM = 541 ! UNDEFINED_F10
                          ICODE = ITTI () 
                          GOTO 810
                       ELSE IF ( ICODE .EQ. 51 ) THEN
                          INSIM = 542 ! UNDEFINED_F11
                          ICODE = ITTI () 
                          GOTO 810
                       ELSE IF ( ICODE .EQ. 52 ) THEN
                          INSIM = 543 ! UNDEFINED_F12
                          ICODE = ITTI () 
                          GOTO 810
                     END IF
                   ELSE IF ( ICODE .EQ. 51 ) THEN
                     ICODE = ITTI () ! 126
                     INSIM = 524 ! DELETE_LINE
                     GOTO 810
                   ELSE IF ( ICODE .EQ. 65 ) THEN
                     INSIM = 515 ! ARROW_UP
                     GOTO 810
                   ELSE IF ( ICODE .EQ. 66 ) THEN
                     INSIM = 516 ! ARROW_DOWN
                     GOTO 810
                   ELSE IF ( ICODE .EQ. 67 ) THEN
                     INSIM = 517 ! ARROW_RIGHT
                     GOTO 810
                   ELSE IF ( ICODE .EQ. 68 ) THEN
                     INSIM = 518 ! ARROW_LEFT
                     GOTO 810
                   ELSE IF ( ICODE .EQ. 72 ) THEN
                     INSIM = 531 ! ARROW_HOME
                   ELSE IF ( ICODE .EQ. 53 ) THEN
                     ICODE = ITTI () ! 126
                     INSIM = 529 ! NEXT/PAGE_UP
                   ELSE IF ( ICODE .EQ. 54 ) THEN
                     ICODE = ITTI () ! 126
                     INSIM = 530 ! PREV/PAGE_DOWN
                END IF
             ELSE IF ( ICODE .EQ. 79 ) THEN  ! second
                ICODE = ITTI ()
                IF ( ICODE .EQ. 0 ) THEN
                     INSIM = 531 ! ARROW_HOME
                     GOTO 810
                  ELSE IF ( ICODE .EQ. 80 ) THEN
                     INSIM = 532 ! UNDEFINED_F1
                     GOTO 810
                  ELSE IF ( ICODE .EQ. 81 ) THEN
                     INSIM = 533 ! UNDEFINED_F2
                     GOTO 810
                  ELSE IF ( ICODE .EQ. 82 ) THEN
                     INSIM = 534 ! UNDEFINED_F3
                     GOTO 810
                  ELSE IF ( ICODE .EQ. 83 ) THEN
                    INSIM = 535 ! UNDEFINED_F4
                    GOTO 810
                  ELSE IF ( ICODE .EQ. 84 ) THEN
                    INSIM = 536 ! UNDEFINED_F5
                    GOTO 810
                END IF
              ELSE IF ( ICODE .EQ. 38 ) THEN
                ICODE = ITTI () ! 114
                ICODE = ITTI () !  49
                ICODE = ITTI ()
                IF ( ICODE .EQ. 76 ) THEN
                     INSIM = 513 ! SHIFT__ARROW_LEFT
                     GOTO 810
                  ELSE IF ( ICODE .EQ. 82 ) THEN
                     INSIM = 514 ! SHIFT__ARROW_RIGHT
                     GOTO 810
                END IF
             ELSE IF ( ICODE .EQ. 53 ) THEN
                INSIM = 530 ! PREV/PAGE_DOWN
                ICODE = ITTI () ! 126
                GOTO 810
             ELSE IF ( ICODE .EQ. 54 ) THEN
                INSIM = 529 ! NEXT/PAGE_UP
                ICODE = ITTI () ! 126
                GOTO 810
             ELSE IF ( ICODE .EQ. 65 ) THEN
                INSIM = 515 ! ARROW_UP
                GOTO 810
             ELSE IF ( ICODE .EQ. 66 ) THEN
                INSIM = 516 ! ARROW_DOWN
                GOTO 810
             ELSE IF ( ICODE .EQ. 67 ) THEN
                INSIM = 517 ! ARROW_RIGHT
                GOTO 810
             ELSE IF ( ICODE .EQ. 68 ) THEN
                INSIM = 518 ! ARROW_LEFT
                GOTO 810
             ELSE IF ( ICODE .EQ. 70 ) THEN
                INSIM = 519 ! SHIFT__ARROW_HOME
                GOTO 810
             ELSE IF ( ICODE .EQ. 71 ) THEN
                ICODE = ITTI () ! 27
                ICODE = ITTI () ! 75
                INSIM = 520 ! SHIFT__CLEAR_LINE
                GOTO 810
             ELSE IF ( ICODE .EQ. 74 ) THEN
                INSIM = 521 ! CLEAR_DISPLAY
                GOTO 810
             ELSE IF ( ICODE .EQ. 75 ) THEN
                INSIM = 522 ! CLEAR_LINE
                GOTO 810
             ELSE IF ( ICODE .EQ. 76 ) THEN
                INSIM = 523 ! INSERT_LINE
                GOTO 810
             ELSE IF ( ICODE .EQ. 77 ) THEN
                INSIM = 524 ! DELETE_LINE
                GOTO 810
             ELSE IF ( ICODE .EQ. 80 ) THEN
                INSIM = 525 ! DELETE_CHAR
                GOTO 810
             ELSE IF ( ICODE .EQ. 81 ) THEN
                INSIM = 526 ! INSERT_LINE
                GOTO 810
             ELSE IF ( ICODE .EQ. 83 ) THEN
                INSIM = 527 ! SHIFT__ARROW_UP
                GOTO 810
             ELSE IF ( ICODE .EQ. 84 ) THEN
                INSIM = 528 ! SHIFT__ARROW_DOWN
                GOTO 810
             ELSE IF ( ICODE .EQ. 85 ) THEN
                INSIM = 529 ! NEXT
                GOTO 810
             ELSE IF ( ICODE .EQ. 86 ) THEN
                INSIM = 530 ! PREV
                GOTO 810
             ELSE IF ( ICODE .EQ. 104 ) THEN
                INSIM = 531 ! ARROW_HOME
                GOTO 810
             ELSE IF ( ICODE .EQ. 112 ) THEN
                ICODE = ITTI () ! <RETURN>
                INSIM = 532 ! UNDEFINED_F1
                GOTO 810
             ELSE IF ( ICODE .EQ. 113 ) THEN
                ICODE = ITTI () ! <RETURN>
                INSIM = 533 ! UNDEFINED_F2
                GOTO 810
             ELSE IF ( ICODE .EQ. 114 ) THEN
                ICODE = ITTI () ! <RETURN>
                INSIM = 534 ! UNDEFINED_F3
                GOTO 810
             ELSE IF ( ICODE .EQ. 115 ) THEN
                ICODE = ITTI () ! <RETURN>
                INSIM = 535 ! UNDEFINED_F4
                GOTO 810
             ELSE IF ( ICODE .EQ. 116 ) THEN
                ICODE = ITTI () ! <RETURN>
                INSIM = 536 ! UNDEFINED_F5
                GOTO 810
             ELSE IF ( ICODE .EQ. 117 ) THEN
                ICODE = ITTI () ! <RETURN>
                INSIM = 537 ! UNDEFINED_F6
                GOTO 810
             ELSE IF ( ICODE .EQ. 118 ) THEN
                ICODE = ITTI () ! <RETURN>
                INSIM = 538 ! UNDEFINED_F7
                GOTO 810
             ELSE IF ( ICODE .EQ. 119 ) THEN
                ICODE = ITTI () ! <RETURN>
                INSIM = 539 ! UNDEFINED_F8
                GOTO 810
           END IF
      END IF
!
 810  CONTINUE
      ASIM  =  CHAR ( ICODE )
      ISIM  = ICHAR ( ASIM )
!
      RETURN
      END  !#!  INSIM  #!#
!
! ------------------------------------------------------------------------
!
        SUBROUTINE SHOW_IO ( IT, IG, IP, IST, IRUS )
! ************************************************************************
! *                                                                      *
! *     ποδπςοηςαννα  SHOW_IO  ώιταετ οβύυΰ οβμαστψ  OPTION_IO  ι        *
! *     χοϊχςαύαετ παςανετςω σοστορξιρ χχοδα-χωχοδα: IT, IG, IP, IST,    *
! *     IRUS.                                                            *
! *                                                                      *
! * ________________________ χωθοδξωε παςανετςω: _______________________ *
! *                                                                      *
! *     IT  ( CHARACTER, OPT ) --  τιπ τεςνιξαμα:                        *
! *             1  --   σν 7238,   VT200_8BIT                            *
! *             2  --   σν 7238.1, VT200_8BIT                            *
! *             3  --   FALCO,     VT300_8BIT                            *
! *             4  --   Microterm 5530, VT300_8BIT                       *
! *             5  --   DecWindows, DecTerm ( VAXstation 3100 ).         *
! *     IG  ( CHARACTER, OPT ) --  τιπ ηςαζιλι υ τεςνιξαμα:              *
! *             0  --   ηςαζιλι ξετ.                                     *
! *             1  --   ηςαζιλα εστψ. ςεφιν TEKTROIX 4010/4014           *
! *             2  --   ηςαζιλα εστψ. ςεφιν SIXEL_GRAPHIC.               *
! *             3  --   ηςαζιλα εστψ. ςεφιν REGIS.                       *
! *     IP  ( CHARACTER, OPT ) --  τιπ ισπομψϊυενοηο πςιξτεςα:           *
! *             1  --   σν 6361 .                                        *
! *             2  --   LA 75 .                                          *
! *     IST ( CHARACTER, OPT ) --  τελυύεε σοστορξιε υ τεςνιξαμα.        *
! *             1  --   χ ξαστορύεε χςενρ ςεφιν 80  λομοξολ.             *
! *             2  --   χ ξαστορύεε χςενρ ςεφιν 132 λομοξλι.             *
! *             3  --   χ ξαστορύεε χςενρ ςεφιν TEKTRONIX 4010/4014.     *
! *             4  --   χ ξαστορύεε χςενρ ςεφιν REGIS.                   *
! *             5  --   χ ξαστορύεε χςενρ ςεφιν SIXEL_GRAPHIC.           *
! *             9  --   χ ξαστορύεε χςενρ ςεφιν LA75.                    *
! *    IRUS ( CHARACTER, OPT ) --  τελυύικ ςεφιν πεςελοδιςοχλι σινχομοχ. *
! *             0  --   ξετ πεςελοδιςοχλι.                               *
! *             1  --   ματιξσλιε βυλχω πεςελοδιςυΰτσρ χ ςυσσλιε.        *
! *                                                                      *
! *  ###  12-APR-90    SHOW_IO    V2.1  (c) πΕΤÒΟΧ μ.ΰ.  10-JAN-93  ###  *
! *                                                                      *
! ************************************************************************
        IMPLICIT NONE
        INTEGER*4 ITERMINAL_TYPE, IGRAPHIC_TYPE, IPRINTER_TYPE, ISTATUS, &
     &            IDEF, ITCH_CHAN, ITCH_ST, OTCH_CHAN, OTCH_ST, RUS_STAT, &
     &            N_LINES, N_COLUMNS, ICOLOR_TYPE
        COMMON / OPTION_IO / ITERMINAL_TYPE, IGRAPHIC_TYPE, ICOLOR_TYPE, &
     &                       IPRINTER_TYPE, ISTATUS, IDEF, &
     &                       ITCH_CHAN, ITCH_ST, OTCH_CHAN, OTCH_ST, &
     &                       RUS_STAT, N_LINES, N_COLUMNS
        EXTERNAL BLKDAT_OPTION_IO
        INTEGER*4 IT, IG, IP, IST, IRUS
!        LOGICAL PROBE_W
!
!        NA=NUM$ARG()
!
! ----- εσμι παςανετςω χχοδ-χχοδα ξε βωμ υσταξοχμεξω  --  υσταξοχχλα ιθ
! ----- ϊξαώεξικ "πο υνομώαξιΰ"
!
        IF ( IDEF.EQ.0 ) CALL SETDEF_IO
!
!        IF ( PROBE_W ( 1, 4, IT   ) .AND. NA.GE.1 ) IT=ITERMINAL_TYPE
!        IF ( PROBE_W ( 1, 4, IG   ) .AND. NA.GE.2 ) IG=IGRAPHIC_TYPE
!        IF ( PROBE_W ( 1, 4, IP   ) .AND. NA.GE.3 ) IP=IPRINTER_TYPE
!        IF ( PROBE_W ( 1, 4, IST  ) .AND. NA.GE.4 ) IST=ISTATUS
!        IF ( PROBE_W ( 1, 4, IRUS ) .AND. NA.GE.5 ) IRUS=RUS_STAT
        IF ( LOC(IT)   .NE. 0 ) IT   = ITERMINAL_TYPE
        IF ( LOC(IG)   .NE. 0 ) IG   = IGRAPHIC_TYPE
        IF ( LOC(IP)   .NE. 0 ) IP   = IPRINTER_TYPE
        IF ( LOC(IST)  .NE. 0 ) IST  = ISTATUS
        IF ( LOC(IRUS) .NE. 0 ) IRUS = RUS_STAT
!
        RETURN
        END  !#!  SHOW_IO  #!#
!
! ------------------------------------------------------------------------
!
        SUBROUTINE SETDEF_IO ()
! ************************************************************************
! *                                                                      *
! *     ποδπςοηςαννα  SETDEF_IO  υσταξαχμιχαετ ϊξαώεξιρ παςανετςοχ       *
! *     χχοδα-χωχοδα "πο υνομώαξιΰ".                                     *
! *                                                                      *
! *  ###  14-JUL-93  SETDEF_IO   v 2.3  (c) L. Petrov  08-AUG-2001  ###  *
! *                                                                      *
! ************************************************************************
!        INCLUDE    '($IODEF)'
!        INCLUDE    '($DVIDEF)'
        CHARACTER    LOG_PRT*10, GST*20, PST*20, STR*80
!!        CHARACTER    BUF*8, STR*80
        CHARACTER    STR_LINES*4, STR_COLUMNS*4
!!        INTEGER*4    IDVI, IT, IS, SYS$QIOW
!!        INTEGER*2    OTCH
        INTEGER*4    ITERMINAL_TYPE, IGRAPHIC_TYPE, ICOLOR_TYPE, &
     &               IPRINTER_TYPE, ISTATUS, IDEF, &
     &               ITCH_CHAN, ITCH_ST, OTCH_CHAN, OTCH_ST, RUS_STAT, &
     &               N_LINES, N_COLUMNS
        COMMON / OPTION_IO / ITERMINAL_TYPE, IGRAPHIC_TYPE, ICOLOR_TYPE, &
     &                       IPRINTER_TYPE, ISTATUS, IDEF, &
     &                       ITCH_CHAN, ITCH_ST, OTCH_CHAN, OTCH_ST, &
     &                       RUS_STAT, N_LINES, N_COLUMNS
        CHARACTER  BACKSLASH*1
        PARAMETER  ( BACKSLASH = CHAR(92) )
        EXTERNAL BLKDAT_OPTION_IO
        DATA LOG_PRT / 'IODEF_PRT' /
        DATA GST     / 'TERM_TYPE           ' /
        DATA PST     / 'PRINTER_TYPE        ' /
!
! ----- υϊξα³ν: ρχρμετσρ μι υστςοκστχο SYS$OUTPUT τεςνιξαμον?
!
!        CALL LIB$GETDVI ( DVI$_TRM, , 'SYS$OUTPUT', IDVI, , )
!        IF ( IDVI.NE.1 ) RETURN  ! ξετ -- λοξεγ ςαβοτω
!
! ----- ξαϊξαώεξιε λαξαμα λ τεςνιξαμυ
!
!        CALL CHAN_TERM ( , OTCH, -1 )
!
! ----- ώτεξιε θαςαλτεςιστιλ τεςνιξαμα
!
!        CALL CLRCH ( BUF )
!        IS = SYS$QIOW ( ,%VAL(OTCH), %VAL(IO$_SENSEMODE ),,,,
!     #                   %REF(BUF),  %VAL(8),,,, )
!
! ----- υϊξα³ν τελυύυΰ ϋιςιξυ όλςαξα ( IT -- λομιώεστχο λομοξολ )
!
!        IT=ICHAR(BUF(3:3))
!
! ----- υσταξοχλα παςανετςοχ χχοδα-χωχοδα "υνομώαξιρ υνομώαξιρ"
!
!        ITERMINAL_TYPE=2  !  σν 7238.1
!        IGRAPHIC_TYPE =0  !  ηςαζιλι ξετ
!        IPRINTER_TYPE =1  !  τιπ πςιξτεςα σν 6361
!        ISTATUS       =0  !  τελυύεε σοστορξιε ξεοπςεδεμ³ξξοε
!        IF ( IT.EQ.80  ) ISTATUS =1  !  τελυύεε σοστορξιε 80  λομοξολ
!        IF ( IT.EQ.132 ) ISTATUS =2  !  τελυύεε σοστορξιε 132 λομοξλι
!        RUS_STAT      =0  !  σινχομω ξε πεςελοδιςυΰτσρ
!
! ----- υσταξοχλα παςανετςα τιπα πςιξτεςα
!
!        CALL LOGNAME ( LOG_PRT, STR, IZAV )
!        CALL CHIN    ( STR, IP, IER )
!        IF ( IER.EQ.0 ) IPRINTER_TYPE=IP
!
! ----- α τεπεςψ πςοχεςλα: ρχμρετσρ μι δαξξωκ τεςνιξαμ ηςαζιώεσλιν
!
!        CALL CLRCH ( STR )  !  οώιστλα στςολι
!
! ----- οπςεδεμεξιε ϊξαώεξιρ ημοβαμψξοηο σινχομα TERM_TYPE, υλαϊωχαΰύεηο ξα
! ----- τιπ δισπμερ
!
!        IL=LIB$GET_SYMBOL ( GST, STR )
!
! ----- ιϊχμεώεξιε τιπα τεςνιξαμα
!
!        CALL CHIN ( STR(1:1), IT, IER )
!        IF ( IER.EQ.0 ) ITERMINAL_TYPE=IT
!
! ----- ιϊχμεώεξιε τιπα ηςαζιλι
!
!        CALL CHIN ( STR(2:2), IG, IER )
!        IF ( IER.EQ.0 ) IGRAPHIC_TYPE=IG
!
! ----- οπςεδεμεξιε ϊξαώεξιρ ημοβαμψξοηο σινχομα PRINTER_TYPE, οπςεδεμρΰύεηο
! ----- τιπ πςιξτεςα
!
!        IL=LIB$GET_SYMBOL ( PST, STR )
!
! ----- ιϊχμεώεξιε τιπα πςιξτεςα
!
!        CALL CHIN ( STR, IP, IER )
!        IF ( IER.EQ.0 ) IPRINTER_TYPE=IP
!
! ----- HP-UX
!
        CALL GETENVAR ( 'TERM', STR )
        IF ( STR(1:5) .EQ. 'xterm'  .OR. &
     &       STR(1:5) .EQ. 'vt100'  .OR. &
     &       STR(1:4) .EQ. 'vt52'         ) THEN
             ITERMINAL_TYPE = 7
           ELSE
             IF ( STR(1:6) .EQ. 'hpterm' ) THEN
!                  CALL PRCH ( CHAR(27)//'&k0'//BACKSLASH ) ! Set HP-mode for hpterm
             END IF
!
! ---------- Default is hpterm
!
             ITERMINAL_TYPE = 6
        END IF
!
        IF ( STR(1:5) .EQ. 'xterm'  .OR. STR(1:6) .EQ. 'hpterm' ) THEN
             ICOLOR_TYPE = 1
             CALL GETENVAR ( 'TERM_COLOR', STR )
             CALL TRAN ( 11, STR, STR )
             IF ( STR(1:2) .EQ. 'NO' .OR. STR(1:3) .EQ. 'OFF' ) THEN
                  ICOLOR_TYPE = 0
             END IF
        END IF
        IGRAPHIC_TYPE  = 0
        IPRINTER_TYPE  = 4
        ISTATUS        = 6
        RUS_STAT       = 0
!
        CALL GETENVAR ( 'LINES',   STR_LINES            )
        CALL CHIN     (            STR_LINES,   N_LINES )
        CALL GETENVAR ( 'COLUMNS', STR_COLUMNS          )
        CALL CHIN     (            STR_COLUMNS, N_COLUMNS )
!
!        TYPE *,' setdef:  N_LINES=',N_LINES,' N_COLUMNS=',N_COLUMNS
!
        IDEF=1
        RETURN
        END  !#!  SETDEF_IO  #!#
!
! ------------------------------------------------------------------------
!
      FUNCTION  USE_TERM_COLOR ()
! ************************************************************************
! *                                                                      *
! *   This routine returns  .TRUE.  if this terminal supports            *
! *   color escape sequences for changing colors and user allowed to     *
! *   use them. (If user set environment variable TERM_COLOR = 'NO',     *
! *   then the terminal is considered as not supporting color excape     *
! *   sequences).                                                        *
! *
! *  ### 30-MAR-2000  USE_TERM_COLOR v1.0 (c) L. Petrov 30-MAR-2000 ###  *
! *                                                                      *
! ************************************************************************
      LOGICAL*4 USE_TERM_COLOR
      INTEGER*4 ITERMINAL_TYPE, IGRAPHIC_TYPE, ICOLOR_TYPE, &
     &          IPRINTER_TYPE, ISTATUS, IDEF, &
     &          ITCH_CHAN, ITCH_ST, OTCH_CHAN, OTCH_ST, RUS_STAT, &
     &          N_LINES, N_COLUMNS
        COMMON / OPTION_IO / ITERMINAL_TYPE, IGRAPHIC_TYPE, ICOLOR_TYPE, &
     &                       IPRINTER_TYPE, ISTATUS, IDEF, &
     &                       ITCH_CHAN, ITCH_ST, OTCH_CHAN, OTCH_ST, &
     &                       RUS_STAT, N_LINES, N_COLUMNS
      EXTERNAL BLKDAT_OPTION_IO
!
      IF ( ICOLOR_TYPE .EQ. 0 ) THEN
           USE_TERM_COLOR = .FALSE.
         ELSE
           USE_TERM_COLOR = .TRUE.
      END IF
      RETURN
      END  !#!
!
! ------------------------------------------------------------------------
!
        BLOCK DATA BLKDAT_OPTION_IO
! ************************************************************************
! *                                                                      *
! *     βμολ DATA, λοτοςωκ υσταξαχμιχαετ ϊξαώεξιρ πο υνομώαξιΰ χ βυζεςε  *
! *     σοστορξιρ χχοδ-χωχοδα OPTION_IO.                                 *
! *                                                                      *
! ************************************************************************
        INTEGER*4      ITERMINAL_TYPE, IGRAPHIC_TYPE, ICOLOR_TYPE, &
     &                 IPRINTER_TYPE, ISTATUS, IDEF, &
     &                 ITCH_CHAN, ITCH_ST, OTCH_CHAN, OTCH_ST, RUS_STAT, &
     &                 N_LINES, N_COLUMNS
        COMMON / OPTION_IO / ITERMINAL_TYPE, IGRAPHIC_TYPE, ICOLOR_TYPE, &
     &                       IPRINTER_TYPE, ISTATUS, IDEF, &
     &                       ITCH_CHAN, ITCH_ST, OTCH_CHAN, OTCH_ST, &
     &                       RUS_STAT, N_LINES, N_COLUMNS
!
        DATA ITERMINAL_TYPE / 0  /  !  τιπ τεςνιξαμα
        DATA IGRAPHIC_TYPE  / 0  /  !  τιπ ηςαζιλι
        DATA ICOLOR_TYPE    / 0  /  !  Does the terminal support colors
        DATA IPRINTER_TYPE  / 0  /  !  τιπ πςιξτεςα
        DATA ISTATUS        / 0  /  !  τελυύεε σοστορξιε τεςνιξαμα
        DATA IDEF           / 0  /  !  πςιϊξαλ τοηο, ώτο δαξξοε σοστορξιε
!          υσταξοχμεξο πο υνομώαξιΰ
        DATA ITCH_CHAN      / -1 /  !
        DATA ITCH_ST        / 0  /  !  πςιϊξαλ τοηο, ώτο λαξαμ τεςνιξαμψξοηο
!          χχοδα  ξε οτλςωτ
        DATA OTCH_CHAN      / -1 /  !
        DATA OTCH_ST        /  0 /  !  πςιϊξαλ τοηο, ώτο λαξαμ τεςνιξαμψξοηο
!          χωχοδα ξε οτλςωτ
        DATA RUS_STAT       /  0 /  !  πςιϊξαλ τοηο, ώτο υσταξοχμεξ ματιξσλικ
!          ϋςιζτ
        DATA N_LINES        / -1 /  !
        DATA N_COLUMNS      / -1 /  !
!
        END  !#!  BLKDAT_OPTION_IO  #!#
!
! ------------------------------------------------------------------------
!
        SUBROUTINE CLSTR()
! ************************************************************************
! *                                                                      *
! *     οώιστλα στςολι. λυςσος σταξοχιτσρ χ ξαώαμο στςολι.               *
! *                                                                      *
! ************************************************************************
        CHARACTER STR*256
!!        INTEGER*4 NSTR, NCOL
        CALL SHOW_IO ( IT, IG, IP, IST, IRUS )
        IF ( ( IT.GE.1 .AND. IT.LE.5 ) .OR. IT .EQ. 7 ) THEN
            STR=CHAR(13)//CHAR(27)//'[2K'
            CALL PRCH ( STR(1:5) )
          ELSE IF ( IT .EQ. 6 ) THEN
            STR=CHAR(13)//CHAR(27)//'K'
            CALL PRCH ( STR(1:3) )
!!            CALL WHERE_CURSOR ( NSTR, NCOL )
!!            STR=CHAR(27)//']'
!!            IE=2
!!            IF ( NCOL .NE. 1 ) THEN
!!                 IB=IE+1
!!                 DO 410 J1=NCOL-1,1,-1
!!                    IE=IB+7
!!                    STR(IB:IE)=CHAR(27)//'&a-1C'//CHAR(27)//'P'
!!                    IB=IE+1
!! 410             CONTINUE
!!            END IF
!!            CALL PRCH ( STR(1:IE) )
        END IF
        RETURN
        END  !#!  CLSTR  #!#
!
! ------------------------------------------------------------------------
!
        SUBROUTINE ITTOUT ( C )
! ************************************************************************
! *                                                                      *
! *     χωχοδ ξα τεςνιξαμ σινχομα C ( τιπ BYTE , γεμοε σο ϊξαλον )       *
! *                                                                      *
! ************************************************************************
        CHARACTER  C*1
        WRITE ( 6, 110 ) C
 110    FORMAT(A1$)
        RETURN
        END  !#!  ITTOUT  #!#
!
! ------------------------------------------------------------------------
!
        SUBROUTINE ADR_CURSOR ( NSTR_I, NSTL_I )
! ************************************************************************
! *                                                                      *
! *     Direct addressin the  cursor at the point at the display with    *
! *   coordinates NSTR, NSTRL. The coordinate NSTR is counted from top   *
! *   to down from 1 to 24. The coordinate NSTL is counted from left to  *
! *   right from 1 to 80 or 132.                                         *
! *                                                                      *
! ************************************************************************
        CHARACTER STR_NSTR*2, STR_NSTL*3, STR*12
!        LOGICAL   PRESENT, PROBE_R
!        INTEGER*2 NSTR_I2, NSTL_I2
!
!        NSTR=1
!        IF ( PRESENT ( NSTR_I, 1 ) .AND. PROBE_R ( 1, 4, NSTR_I ) )
!     $       NSTR=NSTR_I
!        NSTL=1
!        IF ( PRESENT ( NSTL_I, 2 ) .AND. PROBE_R ( 1, 4, NSTL_I ) )
!     $       NSTL=NSTL_I
        NSTR=1
        NSTL=1
        IF ( LOC(NSTR_I) .NE. 0 ) NSTR=NSTR_I
        IF ( LOC(NSTL_I) .NE. 0 ) NSTL=NSTL_I
!
        CALL SHOW_IO( IT, IG, IP, IST, IRUS )
        IF ( ( IT.GE.1 .AND. IT.LE.5 ) .OR. IT .EQ. 7 ) THEN
             CALL CLRCH ( STR_NSTR )
             CALL CLRCH ( STR_NSTL )
             CALL CLRCH ( STR )
             CALL POS_TRM ( NSTR, STR_NSTR, N1 )
             IF ( N1.EQ.0 .OR. N1.GT.2 ) RETURN
             CALL POS_TRM ( NSTL, STR_NSTL, N2 )
             IF ( N2.EQ.0 .OR. N2.GT.3 ) RETURN
             STR=CHAR(27)//'['//STR_NSTR(1:N1)//';' &
     &                        //STR_NSTL(1:N2)//'H'
             CALL PRCH ( STR )
          ELSE IF ( IT .EQ. 6 ) THEN
             CALL CLRCH ( STR_NSTR )
             CALL CLRCH ( STR_NSTL )
             CALL CLRCH ( STR )
             CALL POS_TRM ( NSTR-1, STR_NSTR, N1 )
             CALL POS_TRM ( NSTL-1, STR_NSTL, N2 )
             STR=CHAR(27)//'&a'//STR_NSTR(1:N1)//'y' &
     &                         //STR_NSTL(1:N2)//'C'
             CALL PRCH ( STR )
        END IF
!
        RETURN
        END  !#!  ADR_CURSOR  #!#
!
! ------------------------------------------------------------------------
!
        SUBROUTINE POS_TRM ( NUM, STR, NS )
! ************************************************************************
! *                                                                      *
! *     Auxiliary program POS_TRM returns the string STR adjusted to the *
! *     left with the number of NUM, which takes NS position.            *
! *     If NUM < 1,   then NUM=1 .                                       *
! *     If NUM > 132, then NUM=132 .                                     *
! *                                                                      *
! ************************************************************************
        INTEGER*4  , EXTERNAL :: ILEN
        CHARACTER STR*(*)
        NS=0
        CALL CLRCH( STR )
        MUM=NUM
        IF( MUM.LT.0   ) MUM=0
        IF( MUM.GT.132 ) MUM=132
        CALL INCH ( MUM, STR )
        NS=ILEN( STR )
        RETURN
        END  !#!  POS_TRM  #!#
!
! ------------------------------------------------------------------------
!
        SUBROUTINE CURL ( N )
! ************************************************************************
! *                                                                      *
! *     πεςεδχιφεξιε λυςσοςα χμεχο ξα N ποϊιγικ.                         *
! *                                                                      *
! ************************************************************************
        CHARACTER STR*8, STR1*3
        NN=1
!        IF ( NUM$ARG().EQ.1 ) NN=N
        IF ( LOC(N) .NE. 0 ) NN=N
        IF ( NN.LE.0 ) NN=1
        CALL SHOW_IO ( IT, IG, IP, IST, IRUS )
        IF ( ( IT.GE.1 .AND. IT.LE.5 ) .OR. IT .EQ. 7 ) THEN
            CALL POS_TRM( NN, STR1, N1 )
            IF( N1.EQ.0 ) RETURN
            STR=CHAR(27)//'['//STR1(1:N1)//'D'
          ELSE IF ( IT.EQ.6 ) THEN
            CALL POS_TRM( NN, STR1, N1 )
            IF( N1.EQ.0 ) RETURN
            STR=CHAR(27)//'&a-'//STR1(1:N1)//'C'
        END IF
        CALL PRCH ( STR )
        RETURN
        END  !#!  CURL  #!#
!
! ------------------------------------------------------------------------
!
        SUBROUTINE CURR ( N )
! ************************************************************************
! *                                                                      *
! *     πεςεδχιφεξιε λυςσοςα χπςαχο ξα N ποϊιγικ.                        *
! *                                                                      *
! ************************************************************************
        CHARACTER STR*8, STR1*3
        NN=1
!        IF ( NUM$ARG().EQ.1 ) NN=N
        IF ( LOC(N) .NE. 0 ) NN=N
        IF ( NN.LE.0 ) NN=1
        CALL SHOW_IO( IT, IG, IP, IST, IRUS )
        IF ( ( IT.GE.1 .AND. IT.LE.5 ) .OR. IT .EQ. 7 ) THEN
             CALL POS_TRM ( NN, STR1, N1 )
             IF( N1.EQ.0 ) RETURN
             STR=CHAR(27)//'['//STR1(1:N1)//'C'
          ELSE IF ( IT.EQ.6 ) THEN
            CALL POS_TRM( NN, STR1, N1 )
            IF( N1.EQ.0 ) RETURN
            STR=CHAR(27)//'&a+'//STR1(1:N1)//'C'
        END IF
        CALL PRCH ( STR )
        RETURN
        END  !#!  CURR  #!#
!
! ------------------------------------------------------------------------
!
        SUBROUTINE CURU ( N )
! ************************************************************************
! *                                                                      *
! *     πεςεδχιφεξιε λυςσοςα χχεςθ ξα N στςολ.                           *
! *                                                                      *
! ************************************************************************
        CHARACTER STR*8, STR1*3  !! , STR_LINES*4
        INTEGER*4 N, NN, NSTR, NCOL !! , N_LINES
        NN=1
!        IF ( NUM$ARG().EQ.1 ) NN=N
        IF ( LOC(N) .NE. 0 ) NN=N
        IF ( NN.LE.0 ) NN=1
        CALL SHOW_IO( IT, IG, IP, IST, IRUS )
        IF ( ( IT.GE.1 .AND. IT.LE.5 ) .OR. IT .EQ. 7 ) THEN
            CALL POS_TRM ( NN, STR1, N1 )
            IF( N1.EQ.0 ) RETURN
            STR=CHAR(27)//'['//STR1(1:N1)//'A'
          ELSE IF ( IT.EQ.6 ) THEN
            CALL WHERE_CURSOR ( NSTR, NCOL )
            IF ( NN .GT. NSTR-1 ) NN=NSTR-1
            IF ( NN.LE.0 ) RETURN
            CALL POS_TRM( NN, STR1, N1 )
            IF( N1.EQ.0 ) RETURN
            STR=CHAR(27)//'&a-'//STR1(1:N1)//'R'
        END IF
        CALL PRCH ( STR )
        RETURN
        END  !#!  CURU  #!#
!
! ------------------------------------------------------------------------
!
        SUBROUTINE CURD ( N )
! ************************************************************************
! *                                                                      *
! *     πεςεδχιφεξιε λυςσοςα χξιϊ ξα N στςολ.                            *
! *                                                                      *
! ************************************************************************
        CHARACTER STR*8, STR1*3, STR_LINES*4
        INTEGER*4 N, NN, NSTR, NCOL, N_LINES
        NN=1
!        IF ( NUM$ARG().EQ.1 ) NN=N
        IF ( LOC(N) .NE. 0 ) NN=N
        IF ( NN.LE.0 ) NN=1
        CALL SHOW_IO( IT, IG, IP, IST, IRUS )
        IF ( ( IT.GE.1 .AND. IT.LE.5 ) .OR. IT .EQ. 7 ) THEN
            CALL POS_TRM ( NN, STR1, N1 )
            IF( N1.EQ.0 ) RETURN
            STR=CHAR(27)//'['//STR1(1:N1)//'B'
            CALL PRCH ( STR )
         ELSE IF ( IT.EQ.6 ) THEN
            CALL WHERE_CURSOR ( NSTR, NCOL )
            CALL GETENVAR ( 'LINES',   STR_LINES            )
            CALL CHIN     (            STR_LINES,   N_LINES )
            N_LINES=N_LINES
!
            NSTR=NSTR+NN
            IF ( NSTR .GT. N_LINES ) NSTR=N_LINES
            CALL ADR_CURSOR ( NSTR, NCOL )
        END IF
        RETURN
        END  !#!  CURD  #!#
!
! ------------------------------------------------------------------------
!
        SUBROUTINE BELL ( N )
! ************************************************************************
! *                                                                      *
! *        ποδαώα  N  ϊχυλοχωθ σιηξαμοχ.                                 *
! *                                                                      *
! ************************************************************************
        INTEGER*1  B
        B=7
!        IF ( NUM$ARG().EQ.1 ) THEN
!             IF ( N.LE.1 ) GOTO 810
!             DO 410 J1=1,N
!                TYPE 110,B
!                CALL LIB$WAIT(0.4D0)  !   ϊαδεςφλα ξα 0.4 σελυξδω.
!  410        CONTINUE
!  110        FORMAT('+',A1$)
!             RETURN
!          ELSE
!
! ------- εσμι N=1 ιμι N<1 ιμι ζοςναμψξωκ παςανετς οπυύεξ,
! ------- το ποδαώα οδξοηο ηυδλα
!
!  810        TYPE 110,B
!        END IF
!
        RETURN
        END  !#!  BELL  #!#
!
! ------------------------------------------------------------------------
!
        SUBROUTINE PRCHDL ( STR )
! ************************************************************************
! *                                                                      *
! *     ποδπςοηςαννα  PRCHDL  χωχοδιτ ξα όλςαξ σοδεςφινοε δμιξξοκ στςολι *
! *     STR . πςι ότον τελστ χωχοδιτσρ ξαώιξαρ οτ τελυύεηο πομοφεξιρ     *
! *     λυςσοςα, α ποσμε χωχοδα στςολι λυςσος οσταξαχμιχαετσρ σπςαχα οτ  *
! *     ποσμεδξεηο σινχομα, οτμιώξοηο οτ πςοβεμα.                        *
! *     χ οτμιώιε οτ ποδπςοηςαννω PRCH, εσμι στςολα STR ινεετ            *
! *     όζζελτιχξυΰ δμιξυ βομεε 80 σινχομοχ, το  PRCHDL  χωχοδιτ ε³ ταλ, *
! *     ώτοβω ιϊβεφατψ WRAPPING'α .                                      *
! *     πςινεώαξιε: εσμι στςολα σοδεςφιτ τομψλο πςοβεμω, το ξιώεηο ξε    *
! *     χωχεδετσρ.                                                       *
! *                                                                      *
! ************************************************************************
        CHARACTER STR*(*)
!        LOGICAL LR$STR
!        IF ( .NOT. LR$STR ( STR )  )  RETURN
!        L=ILEN(STR)
!        IF ( L.GT.132 ) L=132
!        IF ( L.LT.80 ) THEN
!             CALL PRCH ( STR )
!          ELSE
!
! ---------- χ σμυώαε, εσμι στςολα δμιξεε 80 σινχομοχ, το δμρ τοηο ώτοβω
! ---------- ιϊβεφατψ  WRAPPING'A πο 80-κ λομοξλε, δεμαετσρ οβθοδξωκ ναξ³χς
!
!             ILB=I_LEN(STR(1:79))  !  όζζελτιχξαρ δμιξα μεχοκ ώαστι στςολι
!             CALL CURR ( 79 )  !  σδχιη χπςαχο ξα 79 λομοξολ
!             CALL PRCH ( STR(80:L) )   !  πεώατψ πςαχοκ πομοχιξω τελστα
!             CALL CURL ( L )   !  σδχιη χμεχο
!             CALL PRCH ( STR(1:ILB) )  ! πεώατψ μεχοκ πομοχιξω τελστα
!             CALL CURR ( L-ILB ) ! εύ³ σδχιη χπςαχο
!        END IF
             CALL PRCH ( STR )
        RETURN
        END  !#!  PRCHDL  #!#
!
! ------------------------------------------------------------------------
!
        SUBROUTINE ABOX ( KSTR_LD, KSTOL_LD, KSTR_RH, KSTOL_RH, IUER )
! ************************************************************************
! *                                                                      *
! *     ποδπςοηςαςαννα  ABOX  χωώεςώιχαετ πςρνουηομψξιλ χ ςεφινε         *
! *     πσεχδοηςαζιλι.                                                   *
! *                                                                      *
! * _________________________ χθοδξωε παςαντςω: ________________________ *
! *                                                                      *
! *     KSTR_LD   ( INTEGER*4 ) --  ξονες στςολι  μεχοηο ξιφξεηο υημα.   *
! *     KSTOL_LD  ( INTEGER*4 ) --  ξονες στομβγα μεχοηο ξιφξεηο υημα.   *
! *     KSTR_RH   ( INTEGER*4 ) --  ξονες στςολι  πςαχοηο χεςθξεηο υημα. *
! *     KSTOL_RH  ( INTEGER*4 ) --  ξονες στομβγα πςαχοηο χεςθξεηο υημα. *
! *                                                                      *
! *         ξονες στομβγα οτώσιτωχαετσρ οτ μεχοηο λςαρ τεςνιξαμα ξαπςαχο *
! *     οτ 1 δο 80 ( χ ςεφινε "80 λομοξολ" ) ιμι δο 132 ( χ ςεφινε 132"  *
! *     λομοξλι" ), ξονες στςολι  --  σχεςθυ χξιϊ οτ 1 δο 24 .           *
! *         ξονες στομβγα χσεηδα ρχμρετσρ αβσομΰτξων. ξονες στςολι βυδετ *
! *     ιξτεςπςετιςοχατψσρ λαλ αβσομΰτξωκ, εσμι KSTR_RH>0. εσμι φε       *
! *     KSTR_RH=0, το χεςθξρρ ηςαξψ πςρνουηομψξιλα βυδετ χωχεδεξα χ      *
! *     τελυύεκ στςολε, α ξιφξρρ στςολα βυδετ χωχεδεξα ξα  KSTR_LD στςολ *
! *     ξιφε, πςι ώεν, εσμι οξα ξε βυδετ πονεύατψσρ ξα όλςαξε, πςοιϊοκδετ*
! *     ςομμιξη χξιϊ.                                                    *
! *         ποσμε χωώεςώιχαξιρ ςανλι λυςσος πςωηαετ χ ξαώαμο  KSTR_RH    *
! *     στςολι.                                                          *
! *                                                                      *
! * ___________________ νοδιζιγιςυενωε παςανετςω: ______________________ *
! *                                                                      *
! *  IUER  ( INTEGER*4, OPT )  -- παςανετς οϋιβλι:                       *
! *             χθοδξοε ϊξαώεξιε  --  ςεφιν οβςαβοτλι οϋιβλι:            *
! *             ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~             *
! *      IUER>-1 -- χοϊχςαύεξιε λοδα οϋιβλι.                             *
! *      IUER=-1 -- χοϊχςαύεξιε λοδα IUER=0 χ σμυώαε ξοςναμψξοηο         *
! *                 ϊαχεςϋεξιρ ι χωχοδ διαηξοστιώεσλοηο σοοβύεξιρ        *
! *                 χ σμυώαε χοϊξιλξοχεξιρ οϋιβλι.                       *
! *      IUER<-1 -- χοϊχςαύεξιε λοδα IUER=0 χ σμυώαε ξοςναμψξοηο         *
! *                 ϊαχεςϋεξιρ, χωχοδ διαηξοστιώεσλοηο σοοβύεξιρ ι       *
! *                 ϊαχεςϋεξιε οβςαϊα χ σμυώαε χοϊξιλξοχεξιρ οϋιβλι.     *
! *      εσμι IUER οπυύεξ, το χθοδξοε ϊξαώεξιε πςιξιναετσρ ςαχξων -1     *
! *             χωθοδξοε ϊξαώεξιε  --  λοδ οϋιβλι ( εσμι IUER            *
! *             ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~                        *
! *             δοστυπεξ δμρ ϊαπισι ):                                   *
! *      IUER=0  --  ξοςναμψξοε ϊαχεςϋεξιε.                              *
! *      IERR>0  --  ξεχεςξο ϊαδαξω λοοςδιξατω πςρνουηομψξιλα.           *
! *                  πςρνουηομψξιλ ξε χωώεςώεξ.                          *
! *                                                                      *
! *   ###  ποδπςοηςαννυ   ABOX   ξαπισαμ  πετςοχ μ.ΰ.  04-JUL-91   ###   *
! *   ###                 ABOX            χεςσιρ 3.1               ###   *
! *                                                                      *
! ************************************************************************
        INTEGER*4    LXLD, LYLD, LXRH, LYRH
        CHARACTER    BACKSLASH*1
        PARAMETER  ( BACKSLASH = CHAR(92) )
        LXLD = KSTOL_LD
        LYLD = KSTR_LD
        LXRH = KSTOL_RH
        LYRH = KSTR_RH
!        IF ( NUM$ARG().LT.4 ) CALL VER$ARG ( 5 )
        CALL ERR_LOG ( 0, IUER )
!
! ----- πςεοβςαϊςχαξιε λοοςδιξατ υημοχ
!
        IF ( LXLD.GE.LXRH ) THEN
             L=LXLD
             LXLD=LXRH
             LXRH=L
        END IF
!
        IF ( LYRH.GE.LYLD ) THEN
            L=LYLD
            LYLD=LYRH
            LYRH=L
        END IF
!
! ----- πςοχεςλα ξα λοςςελτξοστψ λοοςδιξατ υημοχ
!
        IF( LXLD.LE.0 .OR. LYLD.LE.0 .OR. LXRH.LE.0 .OR. LYRH.LT.0 )THEN
            CALL ERR_LOG ( 1, IUER, 'ABOX', ' ' )
            RETURN
        END IF
!
        IF( LXLD.EQ.LXRH .OR. LYLD.EQ.LYRH ) THEN
            CALL ERR_LOG ( 2, IUER, 'ABOX', ' ' )
            RETURN
        END IF
!
        IF ( LXLD.GT.132 .OR. LXRH.GT.132 ) THEN
            CALL ERR_LOG ( 3, IUER, 'ABOX', ' ' )
            RETURN
        END IF
!
        IF ( LYLD.GT.24 .OR. LYRH.GT.24 ) THEN
            CALL ERR_LOG ( 4, IUER, 'ABOX', ' '  )
            RETURN
        END IF
!
        CALL SHOW_IO ( IT, IG, IP, IST, IRUS )
        IF ( LXRH.GE.80 .AND. IST.EQ.1 ) LXRH=80
!
! ----- ξαώαμο χωςισοχωχαξιρ ςανλι
!       """"""""""""""""""""""""""
        IF ( LYRH.EQ.0 ) THEN
             CALL CURL ( 132 )
             CALL CURR ( LXLD-1 )
          ELSE
             CALL ADR_CURSOR( LYRH, LXLD )
        END IF
        I=IPG('L') ! χωςισοχωχαξιε σινχομα μεχοηο χεςθξεηο υηομλα
        IF ( IT.EQ.6 ) CALL PRCH ( '/' )
!
! ----- χωςισοχωχαξιε χεςθξεκ ηοςιϊοξταμψξοκ ώεςτω
!
        IF( (LXRH-LXLD).GT.1 )  CALL LINHOR ( 'Q', LXLD+1, LXRH-1 )
        I=IPG('K') ! χωςισοχωχαξιε σινχομα πςαχοηο χεςθξεηο υηομλα
        IF ( IT.EQ.6 ) CALL PRCH ( BACKSLASH )
!
! ----- πεςεθοδ ξα ξιφξΰΰ ηςαξψ
!
        IF ( LYRH.EQ.0 ) THEN
             DO 410 J1=1,LYLD
                CALL PRCH ( CHAR(13)//CHAR(10) )
  410        CONTINUE
             CALL CURR ( LXLD-1 )
          ELSE
             CALL ADR_CURSOR ( LYLD, LXLD )
        END IF
        I=IPG('M') ! χωςισοχωχαξιε σινχομα μεχοηο ξιφξεηο υηομλα
        IF ( IT.EQ.6 .OR. IT .EQ. 7 ) CALL PRCH ( BACKSLASH )
!
! ----- χωςισοχωχαξιε ξιφξεκ ηοςιϊοξταμψξοκ ώεςτω
!
        IF( (LXRH-LXLD).GT.1 )  CALL LINHOR ( 'Q', LXLD+1, LXRH-1 )
        I=IPG('J') ! χωςισοχωχαξιε σινχομα πςαχοηο ξιφξεηο υηομλα
        IF ( IT.EQ.6 .OR. IT .EQ. 7) CALL PRCH ( '/' )
!
        LV=LYLD-LYRH-1
        IF ( LV.LT.1 ) RETURN
!
! ----- χωςισοχωχαξιε χεςτιλαμψξωθ ςανολ
!
        CALL CURU ( LV )
        CALL CURL ( 132 )
        DO 420 J2=1,LV
           CALL CURR ( LXLD-1 )
           I=IPG('X') ! χωςισοχωχαξιε σινχομα μεχοκ χεςτιλαμψξοκ ςανλι
           IF ( IT.EQ.6 .OR. IT .EQ. 7 ) CALL ITTOUT ( '|' )
           CALL CURR ( LXRH-LXLD-1 )
           I=IPG('X') ! χωςισοχωχαξιε σινχομα πςαχοκ χεςτιλαμψξοκ ςανλι
           IF ( IT.EQ.6 .OR. IT .EQ. 7 ) CALL ITTOUT ( '|' )
           CALL PRCH ( CHAR(13)//CHAR(10) )
  420   CONTINUE
        CALL CURU ( LV+1 )
        CALL CURL ( 132 )
        RETURN
        END  !#!  ABOX  #!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE WHERE_CURSOR ( NSTR, NCOL )
! ************************************************************************
! *                                                                      *
! *   Subroutune  WHERE_CURSOR  returns current position of the cursor.  *
! *                                                                      *
! *   ###  17-DEC-96   WHERE_CURSOR   v3.0  L. Petrov  24-MAR-2000 ###   *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
#ifdef LAHEY
ML_EXTERNAL  itti_chr, itti_chr_del
#endif
      INTEGER*4  NSTR, NCOL
      INTEGER*4  IT, IG, IP, IST, IRUS, IP1, IP2
      CHARACTER  STR*12, STA*12
!
      CALL CLRCH ( STA )
      CALL CLRCH ( STR )
      CALL SHOW_IO ( IT, IG, IP, IST, IRUS )
      IF ( IT .EQ. 7 ) THEN
           CALL ITTI_CHR_DEL ( CHAR(27)//'[6n', STA, 'R' )
           IP1 = INDEX ( STA, ';' )
           IP2 = INDEX ( STA, 'R' )
           CALL CHIN ( STA(3:IP1-1),     NSTR )
           CALL CHIN ( STA(IP1+1:IP2-1), NCOL )
         ELSE IF ( IT .EQ. 6 ) THEN
!
! -------- Learn absolute cursor position
!
           CALL ITTI_CHR ( CHAR(27)//'a',            STA )
!
! -------- Learn relative cursor position
!
           CALL ITTI_CHR ( STA(1:11)//CHAR(27)//'`', STR )
           CALL CHIN ( STR(4:6),  NCOL )
           CALL CHIN ( STR(8:10), NSTR )
           NCOL=NCOL+1
           NSTR=NSTR+1
!
! -------- Restore cursor position
!
           CALL PRCH ( STA(1:11) )
      END IF
!
      RETURN
      END  !#!  WHERE_CURSOR  #!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE CLEAR ( NSTR, NCOL )
      CALL ADR_CURSOR( NSTR, NCOL )
      CALL SHOW_IO ( IT, IG, IP, IST, IRUS )
      IF ( IT .GE. 1  .AND.  IT .LE. 5 ) THEN
           CALL PRCH ( CHAR(27)//'J' )
         ELSE IF ( IT .EQ. 6 ) THEN
           CALL PRCH ( CHAR(27)//'J' )
         ELSE IF ( IT .EQ. 7 ) THEN
           CALL PRCH ( CHAR(27)//'[2J' )
      END IF
      RETURN
      END  !#!  CLEAR  #!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE TERM_SIZE ( ILIN, ICOL )
! ************************************************************************
! *                                                                      *
! *   Routine  TERM_SIZE  gets the actual number of lines and columns of *
! *   the screen, and re-setting up environment variables LINES and      *
! *   COLUMNS for the current SHELL.  NB: this variable are not          *
! *   automatically heritated by child processes after termination.      *
! *                                                                      *
! * ________________________ OUTPUT PARAMETERS _________________________ *
! *                                                                      *
! *   ILIN (INTEGER*4 ) -- Number of lines of the screen.                *
! *   ICOL (INTEGER*4 ) -- Number of columns of the screen.              *
! *                                                                      *
! *  ###  14-MAR-97    TERM_SIZE   v1.1  (c)  L. Petrov 04-MAY-2004 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INTEGER*4  ILIN, ICOL
      CHARACTER  LINES_STR*16, COLUMNS_STR*80
      SAVE       LINES_STR, COLUMNS_STR  ! Tricky thing!
      INTEGER*4  ILN
      LOGICAL*4  FL_OUT_TERM 
#ifdef GNU
      LOGICAL*4, INTRINSIC :: ISATTY
#else
      LOGICAL*4, EXTERNAL :: FUNC_ISATTY
#endif
      INTEGER*4, EXTERNAL ::  ILEN, LOC__SUN$$_STR
!
! --- Getting aactual number of lines and columns
!
      ILIN = 0
      ICOL = 0
      CALL GET_TERMSIZE ( ILIN, ICOL )
#ifdef SUN
      FL_OUT_TERM = FUNC_ISATTY ( 0 ) ! Flag whether the unit 6 is a terminal
#else
#ifdef GNU
      FL_OUT_TERM = ISATTY ( 6 ) ! Flag whether the unit 6 is a terminal
#else
      FL_OUT_TERM = FUNC_ISATTY ( 6 ) ! Flag whether the unit 6 is a terminal
#endif
#endif
      IF ( .NOT. FL_OUT_TERM  .OR.  ILIN .LE. 0  .OR. ICOL .LE. 0 ) THEN
!
! -------- Set default if GET_TERMSIZE failed to return correct number
!
           ILIN = 24 
           ICOL = 80
      END IF
!
! --- Setting environment variable LINES
!
      IF ( ILIN .LE. 0   .OR.  ICOL .LE. 0 ) RETURN
      LINES_STR(1:6) = 'LINES='
      CALL INCH ( ILIN, LINES_STR(7:) )
      ILN = ILEN(LINES_STR) + 1
      LINES_STR(ILN:ILN) = CHAR(0)
#ifdef SUN
      CALL PUTENV ( %VAL(LOC__SUN$$_STR(LINES_STR)) )
#else
      CALL PUTENV ( %REF( LINES_STR) )
#endif
!
! --- Setting environment variable COLUMNS
!
      COLUMNS_STR(1:8) = 'COLUMNS='
      CALL INCH ( ICOL, COLUMNS_STR(9:) )
      ILN = ILEN(COLUMNS_STR) + 1
      COLUMNS_STR(ILN:ILN) = CHAR(0)
      CALL PUTENV ( COLUMNS_STR(1:ILN) )
!
      RETURN
      END  !#!  TERM_SIZE  #!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE UN_CURSES ()
! ************************************************************************
! *                                                                      *
! *   Procedure  UN_CURSES  eliminates the harmful consequencies of the  *
! *   curses.                                                            *
! *                                                                      *
! *  ###  21-SEP-97    UN_CURSES   v1.1  (c)  L. Petrov  04-MAY-2004 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      CHARACTER  BACKSLASH*1
      PARAMETER  ( BACKSLASH = CHAR(92) )
      INTEGER*4  IT, IG, IP, IST, IRUS
      CALL SHOW_IO ( IT, IG, IP, IST, IRUS )
      CALL SYSTEM ( 'reset -Q'//CHAR(0) )  ! Elimination of the influence of curses
!
! --- Terminal reset. Sending ESC-sequence. It looks like a socery.
!
      IF ( IT .EQ. 6 ) THEN
           CALL PRCH ( CHAR(27)//'g'    // &   ! Soft ITE reset
     &                 CHAR(27)//'E'    // &   ! Hard ITE reset
     &                 CHAR(27)//'&k1L' // &   ! Local echo On
     &                 CHAR(27)//'&s1A' )  ! Transmit mode On
!
! -------- Commented out since it force to stranve behavour
!
!     #                CHAR(27)//'&k0'\\BACKSLASH )   ! Set HP-mode for hpterm
      END IF
!
      RETURN
      END  !#!  UN_CURSES  #!#
