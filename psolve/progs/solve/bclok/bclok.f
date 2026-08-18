      PROGRAM BCLOK
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!     Updated to specificaly type integers which
!-------------------------------------------------
!
! 1.  BCLOK PROGRAM SPECIFICATION
!
! 1.1 BCLOK is modeled after program SLVEB, but is used to
!      choose baseline-dependent clock offsets.
!
! 1.2 REFERENCES:
!
! 2.  BCLOK INTERFACE
!
! 2.1 Parameter File
      INCLUDE 'solve.i'
      INCLUDE 'fclib.i'
!
! 2.2 INPUT Variables: None
!
! 2.3 OUTPUT Variables: None
!
! 2.4 COMMON BLOCKS USED
      INCLUDE 'socom.i'
      INCLUDE 'precm.i'
      INCLUDE 'prfil.i'
!
! 2.5 SUBROUTINE INTERFACE
!
!     CALLING SUBROUTINES: None
!       CALLED SUBROUTINES: utility routines
!
! 3.  LOCAL VARIABLES
!
      INTEGER*2   I, IOPT, IP, IP1, ITEST, J, JM, JM1, K, M, &
     &            MM100, NBL, NMSTAT, NDX, &
     &            OUTBUF(ARC_STA_BIT_WORDS,MAX_ARC_STA+1)
      INTEGER*4   ICHR, IX, IY, I4, L, IYY, PAGEWID, PAGELEN, CTLN, IOFF, L1, &
     &            IX4_SAVE, IY4_SAVE
      CHARACTER*21 BLBUF(MAX_ARC_BSL)
      CHARACTER*4 CCHAR
      EQUIVALENCE (ICHR,CCHAR)
      CHARACTER*1 C1
      INTEGER*2 NUMB(16), IPAGE, NUMPG, ISTART, IEND, LISTLEN, ICONT
      CHARACTER  NUMB_CH(16)*2
      EQUIVALENCE ( NUMB, NUMB_CH )
!
      CHARACTER   SYMBX*45, BUFSTR*80, LET_STA*32, STR*54, GET_VERSION*54
      LOGICAL*2 KBIT
      LOGICAL*2 CHOSB(MAX_ARC_BSL), CHOSS(MAX_ARC_STA), STATN, &
     &            SELSB(MAX_ARC_BSL), SELSS(MAX_ARC_STA)
!
      DATA LET_STA / '1234567890ABCDEFGHIJKLMNOPQRSTUV' /
      DATA SYMBX  /'ABCDEFGHIJKLMNOPQRSTUVWXYZ!#$%&()=-[]+;*:.<>?'/
      DATA NUMB_CH   / &
     &                   '1 ', &
     &                   '2 ', &
     &                   '3 ', &
     &                   '4 ', &
     &                   '5 ', &
     &                   '6 ', &
     &                   '7 ', &
     &                   '8 ', &
     &                   '9 ', &
     &                   '0 ', &
     &                   '! ', &
     &                   '# ', &
     &                   '$ ', &
     &                   '% ', &
     &                   '& ', &
     &                   '* ' &
     &              /
      INTEGER*4  IUER
      DATA ITEST / 0 /
      LOGICAL*4  DATYP_INQ, CHECK_STABIT
      INTEGER*2  INT2_ARG
      INTEGER*4   I_LEN
!
!      NMSTAT     - NUMBER OF STATIONS
!      JM (JM1)   - NUMBER OF STATIONS MINUS 1, UPPER LIMIT OF "I"
!      I          - COUNTER FOR STATION #1 OF A BASELINE
!                   (ALSO USED AS A STATION COUNTER ALONE)
!      J          - COUNTER FOR STATION #2 OF A BASELINE
!      IP         - VARIABLE "I" PLUS 1, LOWER LIMIT OF "J"
!      CHOSB      - ARRAY FOR CHOSEN BASELINES
!      CHOSS      - ARRAY FOR CHOSEN STATIONS
!
! 4.  HISTORY
!       WHEN    WHO    WHAT
!
!       7/84  Mallama  created
!       6/26/86 KDB MADE HANDLING OF ICLOCK CONSISTENT -- BEFORE,
!                   MASTER STATION OPTION FAILED TO AFFECT SOME BITS
!                   ACCESSED BY OTHER OPTIONS
!       8/14/86 KDB NOW HANDLES UP TO 10 STATIONS, INSTEAD OF 7
!       8/15/86 KDB REPOSITION CURSOR UPON ENTRY
!       :92.03.24:jwr: Header improved slightly.
!       :92.04.15:jwr: Master station logic inverted to make useful.
!       :04.01.04:jwr: Selection of baselines by letter removed because
!                      it could not pleasingly be made to work with
!                      more than 26 baselines and to end the confusion
!                      with the options on the control lines.
!       :95.03.21:jwr: c1 introduced to replace cchar(4:4).
!
!       960416  KDB  Convert hardcoded date to sccs-based date.
!                    Add sccsid parameter for version tracking.
!
!       971204  PET  Virtually rewrote the program. Added support deslected
!                    stations and baselines. Change the logic of treating
!                    master station.
!
!       980508  PET  Made bahaviour of SLVEB more predictable. Now change of
!                    baseline status influences on station status. Added
!                    printing station number and allow to select/deselect
!                    baseline-dependent clock for all baselines where the
!                    station participated by entering station number
!
!       980720  PET  Added an option "M" -- set maximal number of baseline
!                    dependent clocks which still doesn't make normal matrix
!                    singular.
!
!    1999.11.11 pet  Replaced option "E" with option "Z" in order to handle
!                    that case when we have more than 14 stations
!
!    1999.12.30 pet  Fixed a bug: if the number of stations was too large
!                    (more than 17) the stations list was screwed up. Forced to
!                    make the second column of stations if the list of stations
!                    doesn't fit one page. Corrected a bug: the previous
!                    version tried to process the command to flip the usage
!                    flag for the baseline after the last line. Added a line
!                    <more baselines> in order to inform user about other pages.
!                    Removed relicts of the code which allowed to address the
!                    baseline by code. Improved logic
!   pet  2003.12.09   Replaced fc_gwinw, fc_gwinsz  calls with GET_TERMSIZE
!
! 5.  BCLOK PROGRAM STRUCTURE
!
      CALL PRE_PROG()
      INCLUDE 'bclok_version.i' ! Set revision date of the current version
      CALL USE_BUFFER  ( NMSTAT, INT2(1), 'ORC' )
      CALL SET_SIGNAL_CTRLC ( 3 )
!
! --- Get ICLOCK out of common file
!
      CALL USE_COMMON ( 'ORC' )
!
! --- 1. OPEN PARFIL, AND READ STATION NAMES
!
      CALL USE_PARFIL('ORC' )
!
! --- 2. Blank the screen and write the version number
!
      IOFF = 1
      CALL START_MN()
 910  CONTINUE
      CALL SETCR_MN ( 0, 0 )
      CALL CLEAR_MN()
      WRITE ( BUFSTR, '("Select baseline dependent clocks ", 20X )'  )
      STR = GET_VERSION()
      CALL SETCR_MN ( 79-I_LEN(STR), 0 )
      CALL REVERSE_ON_MN()
      CALL ADDSTR_F ( STR(1:I_LEN(STR)) )
      CALL REVERSE_OFF_MN()
!
! --- Determine old status of baseline flags
!
      DO I = 1,NMSTAT
         CHOSS(I) = .FALSE.
      END DO
      JM  = NMSTAT - 1
      NDX = 0
      DO I = 1,JM   ! station 1
         IP = I + 1
         IF ( CHECK_STABIT ( I )  .AND.  I .NE. BM_REF_CL ) THEN
              SELSS(I) = .TRUE.
            ELSE
              SELSS(I) = .FALSE.
         END IF
         DO J = IP,NMSTAT   ! station 2
            IF ( CHECK_STABIT ( J )  .AND.  J .NE. BM_REF_CL ) THEN
                 SELSS(J) = .TRUE.
               ELSE
                 SELSS(J) = .FALSE.
            END IF
            NDX=NDX+1
            CHOSB(NDX) = .FALSE.
            IF ( DATYP_INQ ( IDATYP, PHASE__DTP ) ) THEN
!
! ------------- Phase delay solution type
!
                IF ( KBIT (IBLSEL_P(1,I),J) .AND. SELSS(I) .AND. SELSS(J) ) THEN
                     IF ( KBIT(ICLOCK(1,I),J) ) THEN
                          CHOSB(NDX) = .TRUE.
                          CHOSS(I)   = .TRUE.
                          CHOSS(J)   = .TRUE.
                     END IF
                     SELSB(NDX) = .TRUE.
                   ELSE
                     SELSB(NDX) = .FALSE.
                     CALL SBIT ( ICLOCK(1,J), I, INT2(0) )
                     CALL SBIT ( ICLOCK(1,I), J, INT2(0) )
                END IF
                IF ( KBIT (IBLSEL_P(1,J),I) .AND. SELSS(I) .AND. SELSS(J) ) THEN
                     IF ( KBIT(ICLOCK(1,J),I) ) THEN
                          CHOSB(NDX) = .TRUE.
                          CHOSS(I)   = .TRUE.
                          CHOSS(J)   = .TRUE.
                     END IF
                     SELSB(NDX) = .TRUE.
                   ELSE
                     SELSB(NDX) = .FALSE.
                     CALL SBIT ( ICLOCK(1,J), I, INT2(0) )
                     CALL SBIT ( ICLOCK(1,I), J, INT2(0) )
                END IF
              ELSE
!
! ------------- Group delay solution type
!
                IF ( KBIT (IBLSEL_G(1,I),J) .AND. SELSS(I) .AND. SELSS(J) ) THEN
                     IF ( KBIT(ICLOCK(1,I),J) ) THEN
                          CHOSB(NDX) = .TRUE.
                          CHOSS(I)   = .TRUE.
                          CHOSS(J)   = .TRUE.
                     END IF
                     SELSB(NDX) = .TRUE.
                   ELSE
                     SELSB(NDX) = .FALSE.
                     CALL SBIT ( ICLOCK(1,J), I, INT2(0) )
                     CALL SBIT ( ICLOCK(1,I), J, INT2(0) )
                END IF
                IF ( KBIT (IBLSEL_G(1,J),I) .AND. SELSS(I) .AND. SELSS(J) ) THEN
                     IF ( KBIT(ICLOCK(1,J),I) ) THEN
                          CHOSB(NDX) = .TRUE.
                          CHOSS(I)   = .TRUE.
                          CHOSS(J)   = .TRUE.
                     END IF
                     SELSB(NDX) = .TRUE.
                   ELSE
                     SELSB(NDX) = .FALSE.
                     CALL SBIT ( ICLOCK(1,J), I, INT2(0) )
                     CALL SBIT ( ICLOCK(1,I), J, INT2(0) )
                END IF
            END IF
         END DO  ! station 2
      END DO  ! station 1
!
      NDX=0
      DO I=1,NMSTAT-1
         DO J=I+1,NMSTAT
            NDX=NDX+1
            WRITE ( BLBUF(NDX), 109 ) ISITN_CHR(I), ISITN_CHR(J)
  109       FORMAT ( 4X, A8, "-", A8 )
         ENDDO
      ENDDO
!
! --- List baselines (with letters and x's)
!
      CALL GET_TERMSIZE ( PAGELEN, PAGEWID )
      IF ( PAGELEN .EQ. 0 ) PAGELEN = 24
      CTLN = PAGELEN-3
      LISTLEN = PAGELEN - 8
      NBL   = (NMSTAT*(NMSTAT-1))/2
      NUMPG = (NBL-1)/LISTLEN + 1
      IPAGE = 1
100   CONTINUE
      CALL SETCR_MN ( 0, 2 )
      CALL CLRTOBOT_MN()
      IY = IOFF
      ISTART = (IPAGE-1)*LISTLEN+1
      IEND   = MIN ( NBL, ISTART+LISTLEN-INT2(1) )
!
! --- Printing baselines
!
      DO I = ISTART,IEND
         IY = IY + 1
         CALL SETCR_MN ( 7, IY   )
         CALL ADDSTR_F ( BLBUF(I)(:21) )
         CALL SETCR_MN ( 30, IY  )
         IF ( SELSB(I) .AND. CHOSB(I) ) THEN
              CALL ADDSTR_F ( ' X' )
           ELSE IF ( SELSB(I) .AND. .NOT. CHOSB(I) ) THEN
              CALL ADDSTR_F ( '  ' )
           ELSE IF ( .NOT. SELSB(I) ) THEN
              CALL ADDSTR_F ( ' -' )
         END IF
      END DO
      IF ( IPAGE .GT. 1 ) THEN
           CALL SETCR_MN ( 0, 1 )
           CALL ADDSTR_F ( '           <more baselines>' )
         ELSE
           CALL SETCR_MN ( 0, 1 )
           CALL ADDSTR_F ( '                           ' )
      END IF
      IF ( IPAGE .LT. NUMPG ) THEN
           CALL SETCR_MN ( 0, PAGELEN-6 )
           CALL ADDSTR_F ( '           <more baselines>' )
         ELSE
           CALL SETCR_MN ( 0, PAGELEN-6 )
           CALL ADDSTR_F ( '                           ' )
      END IF
!
! --- List stations (with letters and x's)
!
      DO I = 1,NMSTAT   ! by station
         IF ( I .LE. PAGELEN-8 ) THEN
              I4 = I+IOFF
              CALL SETCR_MN ( 42, I4 )
            ELSE IF ( I .GT. (PAGELEN-8)  .AND.  I .LE. 2*(PAGELEN-8) ) THEN
              I4 = I + IOFF -(PAGELEN-8)
              CALL SETCR_MN ( 62, I4 )
            ELSE IF ( I .GT. 2*(PAGELEN-8) ) THEN
              WRITE ( 6, * ) ' PAGELEN =',PAGELEN,' NMSTAT=',NMSTAT
              CALL FERR ( INT2(2725), 'BCLOK(bclok): too many pages!', &
     &             INT2(0), INT2(0) )
              STOP 'BCLOK(bclok): -- too many stataions or too short page'
         END IF
!
         WRITE  ( BUFSTR, 129 ) ( ISITN(K,I),K=1,4 )
  129    FORMAT ( 3X,   4A2)
         CALL ADDSTR_F ( BUFSTR(:11) )
!
         IF ( I .LE. (PAGELEN-8) ) THEN
              CALL SETCR_MN ( 54, I4 )
            ELSE IF ( I .GT. (PAGELEN-8) ) THEN
              CALL SETCR_MN ( 74, I4 )
         END IF
!
         IF ( SELSS(I) .AND. CHOSS(I) ) THEN
              CALL ADDSTR_F ( ' X' )
           ELSE IF ( SELSS(I) .AND. .NOT. CHOSS(I) ) THEN
              CALL ADDSTR_F ( '  ' )
           ELSE IF ( .NOT. SELSS(I) ) THEN
              CALL ADDSTR_F ( ' -' )
         END IF
!
! ------ Printing a letter code of a station
!
         IF ( I .LE. (PAGELEN-8) ) THEN
              CALL SETCR_MN ( 43, I4 )
            ELSE IF ( I .GT. (PAGELEN-8) ) THEN
              CALL SETCR_MN ( 63, I4 )
         END IF
         CALL ADDSTR_F ( LET_STA(I:I) )
      END DO  ! by station
!
! --- Write control information
!
      CALL SETCR_MN ( 0, CTLN-1 )
      CALL ADDSTR_F ( "NOTE: Baseline selections override station selections." )
      CALL NL_MN()
      CALL ADDSTR_F ( "Master Station  (W)All baselines (Z)No baselines " )
      CALL ADDSTR_F ( "(M)ax setup   (N)ext   (P)rev" )
      CALL NL_MN()
      CALL ADDSTR_F ( "(O)ptin   (L)ast Page   (T)erminate   Least s(Q)ares" )
!
! --- Read and interpret user input
!
  200 CONTINUE
      CALL SETCR_MN ( 0, CTLN )
  201 CONTINUE
      CALL SENKR_MN ( IX, IY, ICHR )
  210 CONTINUE
      C1 = CCHAR(4:4)
      IOPT = 0
!
! --- First handle all the 'blank' cases for the control lines
!
      IF ( C1 .EQ. CHAR(13) ) C1 = ' '
      IF ( C1.EQ.' ') THEN
          IF ( IY .EQ. CTLN ) THEN
              IF ( IX.GE.16 .AND. IX.LE.33 ) C1 = 'W'
              IF ( IX.GE.34 .AND. IX.LE.49 ) C1 = 'Z'
              IF ( IX.GE.49 .AND. IX.LE.62 ) C1 = 'M'
              IF ( IX.GE.63 .AND. IX.LE.71 ) C1 = 'N'
              IF ( IX.GE.72 .AND. IX.LE.77 ) C1 = 'P'
            ELSE IF ( IY .EQ. CTLN+1 ) THEN
              IF ( IX.LE.6                 ) C1 = 'O'
              IF ( IX.GE.10 .AND. IX.LE.20 ) C1 = 'L'
              IF ( IX.GE.24 .AND. IX.LE.34 ) C1 = 'T'
              IF ( IX.GE.38 .AND. IX.LE.52 ) C1 = 'Q'
            ELSE IF ( IY .EQ. CTLN-3 ) THEN
              IF ( IX .GE. 11 .AND. IX .LE. 26 ) C1 = 'N'
            ELSE IF ( IY .EQ. 1 ) THEN
              IF ( IX .GE. 11 .AND. IX .LE. 26 ) C1 = 'P'
          ENDIF
      ENDIF
!
! --- Parse the command code
!
      IF ( C1 .EQ. 'W' ) THEN
           IOPT = 1
           GOTO 300
        ELSE IF ( C1 .EQ. 'Z' ) THEN
           IOPT = 5
           GOTO 300
        ELSE IF ( C1   .EQ. 'M' ) THEN
           CALL END_MN()
!
! -------- Compute such a maximal combination of baselines dependent clocks
! -------- which doesn't make normal matrix singular.
!
           IUER = -1
           CALL BCLOCK_MAX ( IUER )
           IF ( IUER .NE. 0 ) THEN
                CALL HIT_CONT ( %VAL(0), %VAL(0) )
           END IF
           CALL START_MN()
           GOTO 910
        ELSE IF ( C1 .EQ. 'N' ) THEN
           IF ( IPAGE .LT. NUMPG ) IPAGE = IPAGE+1
           GOTO 100
        ELSE IF ( C1.EQ.'P' ) THEN
           IF ( IPAGE.GT.1 ) IPAGE = IPAGE-1
           GOTO 100
        ELSE IF ( C1 .EQ. 'T' .OR. &
     &            C1 .EQ. 'O' .OR. &
     &            C1 .EQ. 'L' .OR. &
     &            C1 .EQ. 'Q'      ) THEN
!
          IOPT = 2
          GOTO 400
      END IF
!
! --- If no legitimate choice try to interperete the code as a station key
!
      GOTO 600
!
! === 'WHOLE SET' option
!
  300 CONTINUE
!
! --- Turn on/off flags for baseline and station selections and write x's
!
      JM1 = NMSTAT - 1
      NDX = 0
      DO I = 1,JM1  ! station 1
         IP1 = I + 1
         DO J = IP1,NMSTAT   ! station 2
            NDX = NDX+1
            IF ( IOPT .EQ. 1  .AND. SELSB(NDX) ) THEN
                 CHOSB(NDX) = .TRUE.
                 CALL SBIT ( ICLOCK(1,I), J, INT2(1) )
                 CALL SBIT ( ICLOCK(1,J), I, INT2(1) )
                 IF ( NDX .GE. ISTART  .AND.  NDX .LE. IEND ) THEN
                      IY = IOFF + NDX - ISTART + 1
                      CALL SETCR_MN ( 30, IY )
                      CALL ADDSTR_F ( ' X' )
                 ENDIF
               ELSE IF ( IOPT .NE. 1  .AND.  SELSB(NDX) ) THEN
                 CHOSB(NDX) = .FALSE.
                 CALL SBIT ( ICLOCK(1,I), J, INT2(0) )
                 CALL SBIT ( ICLOCK(1,J), I, INT2(0) )
                 IF ( NDX .GE. ISTART  .AND.  NDX .LE. IEND ) THEN
                      IY = IOFF + NDX - ISTART + 1
                      CALL SETCR_MN ( 30, IY )
                      CALL ADDSTR_F ( '  ' )
                 ENDIF
            ENDIF
         END DO  ! station 2
      END DO   ! station 1
!
      DO I = 1,NMSTAT   ! by station
         IF ( IOPT .EQ. 1  .AND.  SELSS(I) ) THEN
              CHOSS(I) = .TRUE.
!
              IF ( I .LE. (PAGELEN-8) ) THEN
                   I4 = I+IOFF
                   CALL SETCR_MN ( 54, I4 )
                 ELSE IF ( I .GT. (PAGELEN-8) ) THEN
                   I4 = I+IOFF-(PAGELEN-8)
                   CALL SETCR_MN ( 74, I4 )
              END IF
              CALL ADDSTR_F ( ' X' )
           ELSE IF ( SELSS(I) ) THEN
              CHOSS(I) = .FALSE.
!
              IF ( I .LE. (PAGELEN-8) ) THEN
                   I4 = I+IOFF
                   CALL SETCR_MN ( 54, I4 )
                 ELSE IF ( I .GT. (PAGELEN-8) ) THEN
                   I4 = I+IOFF-(PAGELEN-8)
                   CALL SETCR_MN ( 74, I4 )
              END IF
              CALL ADDSTR_F ( '  ' )
         ENDIF
      END DO  ! by station
!
! --- Set cursor to 'ALL DONE'
!
      CALL SETCR_MN ( 6, CTLN )
      GOTO 201
!
! --- 'NO MORE CHANGES' option
!
  400 CONTINUE
      GOTO 900
!
! --- 'MASTER STATION' option
!
  500 CONTINUE
!
! --- Determine a station number
!
      M = 0
      DO I = 1,NMSTAT   ! by station
         IF ( C1 .EQ. LET_STA(I:I) ) M = I
      END DO            ! by station
      IF ( M .LE. 0   .OR.   M .GT. NMSTAT ) GOTO 200
!
! --- Apply changes to station
!
      DO I = 1,NMSTAT
         IF ( SELSS(I) .AND. I .EQ. M ) THEN
              CHOSS(I) = .NOT. CHOSS(I)
!
              IF ( I .LE. (PAGELEN-8) ) THEN
                   I4 = I + IOFF
                   CALL SETCR_MN ( 54, I4 )
                 ELSE IF ( I .GT. (PAGELEN-8) ) THEN
                   I4 = I + IOFF - (PAGELEN-8)
                   CALL SETCR_MN ( 74, I4 )
              END IF
!
              IF ( CHOSS(I) ) THEN
                   CALL ADDSTR_F ( " X" )
                ELSE
                   CALL ADDSTR_F ( "  " )
              ENDIF
         ENDIF
      END DO
!
! --- Apply changes to baseline and station displays
!
      JM1 = NMSTAT - 1
      IY  = IOFF
      NDX = 0
      DO I = 1,JM1   ! station 1
         ICLOCK(1,I) = 0
         IP1 = I + 1
         DO J = IP1,NMSTAT   ! station 2
            NDX=NDX+1
            IY = IY + 1
            IF ( SELSB(NDX) ) THEN
                 IF ( (I.EQ.M .OR. J.EQ.M)  .AND. SELSS(M) .AND. &
     &                CHOSS(M) ) THEN
                      CHOSB(NDX) = .TRUE.
                      CALL SBIT ( ICLOCK(1,I), J, INT2(1) )
                      CALL SBIT ( ICLOCK(1,J), I, INT2(1) )
                 ENDIF
!
                 IF ( ( I.EQ.M .OR. J.EQ.M  ) .AND. SELSS(M) .AND. &
     &                .NOT. CHOSS(M) )THEN
                      CHOSB(NDX) = .FALSE.
                      CALL SBIT ( ICLOCK(1,I), J, INT2(0) )
                      CALL SBIT ( ICLOCK(1,J), I, INT2(0) )
                 ENDIF
!
                 IF ( NDX .GE. ISTART   .AND.   NDX .LE. IEND ) THEN
                      L1 = IY - (IPAGE-1)*LISTLEN
                      CALL SETCR_MN ( 30, l1 )
                      IF (       CHOSB(NDX) ) CALL ADDSTR_F ( ' X' )
                      IF ( .NOT. CHOSB(NDX) ) CALL ADDSTR_F ( '  ' )
                 END IF
            ENDIF
         ENDDO   ! Station 2
      ENDDO    ! Station 1
!
! --- Set cursor to 'ALL DONE'
!
      CALL SETCR_MN ( 6, CTLN )
      GOTO 201
!
! --- 'TOGGLE POSITION' option
!
  600 CONTINUE
!
! --- Decode station or baseline to toggle
!
      M = 0
      IF ( IY .EQ. CTLN  .OR.  CCHAR(4:4) .NE. ' ' ) THEN
!
! -------- By number or letter (add 100 to letter position)
!
           DO I = 1,NMSTAT   ! by station
              IF ( C1 .EQ. LET_STA(I:I) ) M = I
           END DO  ! by station
         ELSE
!
! -------- By position
!
           IF ( IY .GE. 2  .AND. IY .LT. PAGELEN-6 ) THEN
                IF ( IX .LE. 31 ) THEN
                     M = IY-1 + 100  ! baseline
                  ELSE IF ( IX .GE. 43  .AND. IX .LE. 55 ) THEN
                     M = IY- 1  ! station form the middle column
                  ELSE IF ( IX .GE. 63  .AND. IX .LE. 75 ) THEN
                     M = IY-1 + PAGELEN-8 ! station from the utmost right column
                  ELSE
                     M = 0 ! nothing!
                END IF
           END IF
      END IF
!
! --- If input is not legitimate, read again
!
      IF ( M .LE. 0 ) THEN
           CALL SENKR_MN ( IX, IY, ICHR )
           GOTO 210
      END IF
      IF ( M .GT. NMSTAT  .AND.  M .LT. 101 ) THEN
           CALL SENKR_MN ( IX, IY, ICHR )
           GOTO 210
      END IF
      IF ( M .GT. 100+NBL ) THEN
           CALL SETCR_MN ( 6, CTLN )
           GOTO 201
      END IF
!
! --- Identify whether it is a station
!
      STATN = .FALSE.
      IF ( M .LT. 101 ) STATN = .TRUE.
!
! --- Toggle the appropriate station or baseline for display
!
      IF ( STATN ) THEN  ! station
!
! -------- It was a station
!
           DO I = 1, NMSTAT
              IF ( SELSS(I) .AND. I .EQ. M ) THEN
                   CHOSS(I) = .NOT. CHOSS(I)
!
                   IF ( I .LE. (PAGELEN-8) ) THEN
                        I4 = I+IOFF
                        CALL SETCR_MN ( 54, I4 )
                        IX4_SAVE = 55
                        IY4_SAVE = I4
                     ELSE IF ( I .GT. (PAGELEN-8) ) THEN
                        I4 = I+IOFF-(PAGELEN-8)
                        CALL SETCR_MN ( 74, I4 )
                        IX4_SAVE = 75
                        IY4_SAVE = I4
                   END IF
                   IF (       CHOSS(I) ) CALL ADDSTR_F ( ' X' )
                   IF ( .NOT. CHOSS(I) ) CALL ADDSTR_F ( '  ' )
               END IF
           ENDDO
!
! -------- If station, then toggle the appropriate baselines, too
!
           JM1 = NMSTAT - 1
           L = IOFF
           NDX=0
           DO I = 1,JM1  ! station 1
              IP1 = I + 1
              DO J = IP1,NMSTAT  ! station 2
                 NDX=NDX+1
                 L = L + 1
                 IF ( SELSB(NDX) ) THEN
                      IF ( M .EQ. I  .OR.  M .EQ. J ) THEN
                           CHOSB(NDX) = CHOSS(M)
                      ENDIF
                      IF ( NDX.GE.ISTART .AND. NDX.LE.IEND ) THEN
                           L1 = L-(IPAGE-1)*LISTLEN
                           CALL SETCR_MN ( 30, L1 )
                           IF (       CHOSB(NDX) ) CALL ADDSTR_F ( ' X' )
                           IF ( .NOT. CHOSB(NDX) ) CALL ADDSTR_F ( '  ' )
                     ENDIF
                 ENDIF
              END DO  ! station 2
           END DO  ! station 1
!
           GOTO 810
        ELSE
!
! -------- Baseline only
!
           IX4_SAVE = IX
           IY4_SAVE = IY
           MM100 = M - 100
           L = 0
           JM1 = NMSTAT - 1
           NDX=0
           DO I = 1,JM1  ! station 1
              IP1 = I + 1
              DO J = IP1,NMSTAT   ! station 2
                 NDX=NDX+1
                 L = L + 1
                 IF ( L .EQ. MM100+ISTART-1  .AND.  SELSB(NDX) ) THEN
                      CHOSB(NDX) = .NOT. CHOSB(NDX)
                      IF ( NDX .GE. ISTART  .AND. NDX.LE.IEND ) THEN
                         L1 = L-(IPAGE-1)*LISTLEN
                         IX4_SAVE = 30
                         IY4_SAVE = L1+IOFF
                         CALL SETCR_MN ( IX4_SAVE, IY4_SAVE )
                         IF (       CHOSB(NDX) ) CALL ADDSTR_F ( ' X' )
                         IF ( .NOT. CHOSB(NDX) ) CALL ADDSTR_F ( '  ' )
                      END IF
                 ENDIF
              END DO  ! station 2
           END DO  ! station 1
      END IF
!
! --- Update station flags
!
      DO I = 1,NMSTAT
         IF ( SELSS(I) ) THEN
              CHOSS(I) = .FALSE.
         END IF
      END DO
      NDX = 0
      DO I = 1,NMSTAT-1
         DO J = I+1,NMSTAT
            NDX = NDX+1
            IF ( SELSS(I) .AND. SELSS(J) .AND. &
     &           SELSB(NDX) .AND. CHOSB(NDX) ) THEN
!
                 CHOSS(I) = .TRUE.
                 CHOSS(J) = .TRUE.
            END IF
         END DO
      END DO
!
! --- Now display station flags
!
      DO I=1,NMSTAT
         IF ( I .LE. (PAGELEN-8) ) THEN
              I4 = I+IOFF
              CALL SETCR_MN ( 54, I4 )
            ELSE IF ( I .GT. (PAGELEN-8) ) THEN
              I4 = I+IOFF-(PAGELEN-8)
              CALL SETCR_MN ( 74, I4 )
         END IF
!
         IF ( SELSS(I) ) THEN
              IF ( CHOSS(I) ) THEN
                   CALL ADDSTR_F ( " X" )
                ELSE
                   CALL ADDSTR_F ( "  " )
              ENDIF
         ENDIF
      END DO
!
! --- Restroing cursor position
!
 810  CONTINUE
      CALL SETCR_MN ( IX4_SAVE, IY4_SAVE )
!
! --- Set flags, according to baseline display array
!
      DO I = 1,MAX_ARC_STA
         DO J = 1,ARC_STA_BIT_WORDS
            ICLOCK(J,I) = 0
         ENDDO
      ENDDO
      JM1 = NMSTAT - 1
      NDX=0
      DO I = 1,JM1  ! station 1
         IP1 = I + 1
         DO J = IP1,NMSTAT   ! station 2
            NDX=NDX+1
            IF ( SELSB(NDX) .AND.  CHOSB(NDX) ) THEN  ! Chosen
                 CALL SBIT ( ICLOCK(1,I), J, INT2(1) )
                 CALL SBIT ( ICLOCK(1,J), I, INT2(1) )
               ELSE
                 CALL SBIT ( ICLOCK(1,I), J, INT2(0) )
                 CALL SBIT ( ICLOCK(1,J), I, INT2(0) )
            END IF  ! chosen
         END DO  ! station 2
      END DO  ! station 1
!
! --- Set cursor to `TOGGLE POSITION`
!
      IF ( IY .EQ. CTLN ) THEN
           CALL SETCR_MN ( 6, CTLN )
        ELSE
           IYY = IY + 1
           IF ( IYY .GT. CTLN-3 .OR. STATN ) then
                CALL SETCR_MN ( IX, IY )
             ELSE
                CALL SETCR_MN ( 31, IYY )
           END IF
      END IF
      GOTO 201
!
! --- Return flag array to SETFL
!
 900  CONTINUE
      ICONT=0
      IF ( C1 .EQ. 'O' ) ICONT = -7
      IF ( C1 .EQ. 'Q' ) ICONT = -3
      IF ( C1 .EQ. 'T' ) ICONT = -15
      CALL SETCR_MN ( 0, 0 )
      CALL CLEAR_MN()
      CALL END_MN()
!
      DO I=1,MAX_ARC_STA
         DO J=1,ARC_STA_BIT_WORDS
            OUTBUF(J,I) = ICLOCK(J,I)
         ENDDO
      ENDDO
      OUTBUF ( 1, MAX_ARC_STA+1 ) = ICONT
      CALL SOCOM_EXT ()
      CALL USE_BUFFER ( OUTBUF, INT2(MAX_ARC_STA*ARC_STA_BIT_WORDS+1), 'OWC' )
      CALL END_PROG()
!
      END  !#!  BCLOK  #!#
