      PROGRAM SLVEB
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
      INCLUDE 'solve.i'
      INCLUDE 'socom.i'
      INCLUDE 'precm.i'
      INCLUDE 'fclib.i'
!
!   VARIABLES
!
!      NMSTA     - NUMBER OF STATIONS
!      JM (JM1)   - NUMBER OF STATIONS MINUS 1, UPPER LIMIT OF "I"
!      I          - COUNTER FOR STATION #1 OF A BASELINE
!                   (ALSO USED AS A STATION COUNTER ALONE)
!      J          - COUNTER FOR STATION #2 OF A BASELINE
!      IP         - VARIABLE "I" PLUS 1, LOWER LIMIT OF "J"
!      CHOSB      - ARRAY FOR CHOSEN BASELINES
!      CHOSS      - ARRAY FOR CHOSEN STATIONS
!      ioff       - lines of offset before start of site information.
!
      INCLUDE 'prfil.i'
!
      INTEGER*2 IBLED(2,60)
      INTEGER*2 LET(MAX_ARC_BSL)
      INTEGER*2 KBUF(MAX_ARC_STA*ARC_STA_BIT_WORDS+4), NDX, ISTART, IEND, IPAGE, NUMPG
      LOGICAL*2 CHOSB(MAX_ARC_BSL)
      LOGICAL*2 CHOSS(MAX_ARC_STA)
      LOGICAL*2 STATN
      LOGICAL*4 DATYP_INQ
      INTEGER*2 I,IBLFLG,ICONT,IFIRST,II,IOPT,IP1,IPROG,ITEST, &
     &   J,JJ,K,M,MM100,NBL,NBLSEL, NMSTA, &
     &   NUMBL,JM1,listlen, imod
      INTEGER*4   ICHR, IX, IY, IOFF, L, IY4, L1, PAGELEN, PAGEWID, CTLN, KK
      INTEGER*4   IX4_SAVE, IY4_SAVE
      CHARACTER   BLBUF(MAX_ARC_BSL)*21, CCHAR*4, BUFSTR*80, NUMB9*512, &
     &            CLET*45, LET_C2(45)*2, STR*54, GET_VERSION*54
      EQUIVALENCE (LET, LET_C2)
      INTEGER*4   I_LEN
      EQUIVALENCE (ICHR,CCHAR)
      DATA CLET  / 'ABCDEFGHIJKLMNOPQRSTUVWXYZ!#$%&()=-[]+;*:.<>?' /
      DATA ( LET_C2(I), I=1,45) &
     &       / 'A ', 'B ', 'C ', 'D ', 'E ', 'F ', 'G ', 'H ', 'I ', 'J ', &
     &         'K ', 'L ', 'M ', 'N ', 'O ', 'P ', 'Q ', 'R ', 'S ', 'T ', &
     &         'U ', 'V ', 'W ', 'X ', 'Y ', 'Z ', '! ', '# ', '$ ', '% ', &
     &         '& ', '( ', ') ', '= ', '- ', '[ ', '] ', '+ ', '; ', '* ', &
     &         ': ', '. ', '< ', '> ', &
     &       '? '/
      DATA NUMB9  / '1234567890ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz' /
      DATA CHOSB  / MAX_ARC_BSL * .TRUE.  /, &
     &     CHOSS  / MAX_ARC_STA * .FALSE. /
      INTEGER*2  INT2_ARG
      INTEGER*4  INT4
      INT4(INT2_ARG) = INT(INT2_ARG,KIND=4)
!
! --- UNABLE TO INITIALIZE DATA THAT IS NOW EQUIVALENCED TO COMMON.
! --- MALLAMA. DECEMBER 13, 1983.
!     DATA ICONT /0/
      DATA ITEST /0/
!
!     Modifications:
!
!     :91.03.24:jwr: Header added to menu, plus program name and version.
!
!     :04.01.04:jwr: Selection of baselines by letter removed because
!                    it could not pleasingly be made to work with
!                    more than 26 baselines and to end the confusion
!                    with the options on the control lines.
!
!     :96.04.04:kdb: Fix 32 site solve errors:
!                     - Numb9 only had room for 30 sites.
!                     - The screen couldn't handle more than 16 sites.
!
!     KDB  960416   Convert hardcoded date to sccs-based date.
!                   Add sccsid parameter for version tracking.
!
!     PET  971203   Rewrote comments and made text readable. Change logic:
!                   Mater station toggle status of the station. If a master
!                   station WAS deselected then all baselines connected with
!                   it BECOME selected, And vice versa
!
!     PET  971204   Add logic for support IBLSEL_G and IBLSEL_P in dependence
!                   of solution type
!
!     PET  980203   Substituted hard-coded test of solution type by DATYP_INQ
!
!     PET  980508   Made bahaviour of SLVEB more predictable. Now change of
!                   baseline status influences on station status
!
!    1999.11.11 pet  Replaced option "E" with option "Z" in order to handle
!                    the case when we have more than 14 stations. Made some
!                    code cleanup.
!
!    1999.12.30 pet  Fixed a bug: if the number of stations was too large
!                    (more than 17) the stations list was screwed up. Forced to
!                    make the second column of stations if the list of stations
!                    doesn't fit one page. Corrected a bug: the previous
!                    version tried to process the command to flip the usage
!                    flag for the baseline after the last line. Added a line
!                    <more baselines> in order to inform user about other pages.
!                    Removed relicts of the code which allowed to address the
!                    baseline by code.
!    2003.12.09 pet  Replaced fc_gwinw, fc_gwinsz  calls with GET_TERMSIZE
!    2020.10.01 pet  Replaced hardeworded constants with parameters. Updated to 
!                    supoprt MAX_ARC_STA > 32.
!
!CC
      CALL PRE_PROG()
      INCLUDE 'slveb_version.i' ! Set revision date of the current version
      CALL USE_BUFFER  ( KBUF, INT2(MAX_ARC_STA*ARC_STA_BIT_WORDS+4), 'ORC' )
      CALL SET_SIGNAL_CTRLC ( 3 )
!
      IOFF = 1
      IPROG = KBUF(MAX_ARC_STA*ARC_STA_BIT_WORDS+4)
      IF ( IPROG.EQ.1 ) THEN
           IFIRST = KBUF(MAX_ARC_STA*ARC_STA_BIT_WORDS+3)
           IBLFLG = KBUF(MAX_ARC_STA*ARC_STA_BIT_WORDS+2)
           NMSTA  = KBUF(MAX_ARC_STA*ARC_STA_BIT_WORDS+1)
        ELSE
!
! -------- Load socom if it is called from SETFL
!
           CALL USE_COMMON ( 'ORC' )
           NMSTA = NUMSTA
      END IF
!
! --- BLANK THE SCREEN
!
      CALL START_MN()
 910  CONTINUE
      CALL SETCR_MN ( 0, 0 )
      CALL CLEAR_MN()
!
! --- If called by sdbh: if called for the first time, set flag
! --- indicating that all baselines will be used, as the default
!
      IF ( IPROG .EQ. 1  .AND.  IFIRST .EQ. 1 ) IBLFLG = 0
!
! --- Open parfil, and read station names, if slveb was called by setfl
!
      IF ( IPROG .EQ. 2 ) THEN
           CALL USE_PARFIL ( 'ORC' )
!
! -------- Determine old status of baseline flags
!
           CALL GET_STATUS ( CHOSB, CHOSS )
      END IF
      NDX=0
      IMOD = 0
      DO I=1,NMSTA-1
         DO J=I+1,NMSTA
            NDX=NDX+1
            IMOD = IMOD+1
            IF ( IMOD .EQ. 46 ) IMOD = 1
            WRITE ( BLBUF(NDX), 109 ) ISITN_CHR(I), ISITN_CHR(J)
  109       FORMAT ( 4X, A8, "-", A8 )
         ENDDO
      ENDDO
!
! === List baselines and X'S
!
      IF ( DATYP_INQ ( IDATYP, PHASE__DTP ) ) THEN
          WRITE ( BUFSTR, &
     &         '("Select baselines for inclusion in PHASE DELAY solution:", &
     &           7X)' )
         ELSE
          WRITE ( BUFSTR, &
     &         '("Select baselines for inclusion in GROUP DELAY solution:", &
     &           7X)' )
      END IF
!
      STR = GET_VERSION()
      CALL SETCR_MN ( 79-I_LEN(STR), 0 )
      CALL REVERSE_ON_MN()
      CALL ADDSTR_F ( STR(1:I_LEN(STR)) )
      CALL REVERSE_OFF_MN()
!
      CALL GET_TERMSIZE ( PAGELEN, PAGEWID ) 
      IF ( PAGELEN .EQ. 0 ) PAGELEN = 24
      CTLN = PAGELEN-3
      LISTLEN = PAGELEN - 8
      NBL = (NMSTA*(NMSTA-1))/2
      NUMPG=(NBL-1)/LISTLEN + 1
      IPAGE = 1
!
100   CONTINUE
      CALL SETCR_MN ( 0, 2 )
      CALL CLRTOBOT_MN()
      IY = IOFF
      ISTART = (IPAGE-1)*LISTLEN+1
      IEND   = MIN0 ( INT4(NBL), ISTART+LISTLEN-1 )
      DO I = ISTART,IEND
         IY = IY + 1
         CALL SETCR_MN ( 7, IY )
         CALL ADDSTR_F ( BLBUF(I)(:21) )
         IF ( CHOSB(I) ) THEN
              CALL SETCR_MN ( 29, IY )
              WRITE  ( BUFSTR, 119 ) LET(24)
  119         FORMAT ( A2 )
              CALL ADDSTR_F ( BUFSTR(:2) )
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
! === LIST STATIONS (WITH LETTERS AND X'S)
!
      DO I = 1,NUMSTA
         IF ( I .LE. (PAGELEN-8) ) THEN
              IY4 = I + IOFF
              CALL SETCR_MN ( 42, IY4 )
            ELSE IF ( I .GT. (PAGELEN-8)  .AND.  I .LE. 2*(PAGELEN-8) ) THEN
              IY4 = I + IOFF - (PAGELEN-8)
              CALL SETCR_MN ( 62, IY4 )
            ELSE IF ( I .GT. 2*(PAGELEN-8) ) THEN
              WRITE ( 6, * ) ' PAGELEN =',PAGELEN,' NMSTA=',NMSTA
              CALL FERR ( INT2(2725), 'SLVEB(slveb): too many pages!', &
     &             INT2(0), INT2(0) )
              STOP 'SLVEB(slveb) -- too many stataions or too short page'
         END IF
!
         WRITE ( BUFSTR, 129 ) NUMB9(I:I), (ISITN(K,I),K=1,4)
  129    FORMAT ( A1, 1X, 4A2 )
         CALL ADDSTR_F ( BUFSTR(:10) )
         IF ( CHOSS(I) ) THEN
              IF ( I .LE. (PAGELEN-8) ) THEN
                   CALL SETCR_MN ( 53, IY4 )
                 ELSE IF ( I .GT. (PAGELEN-8) ) THEN
                   CALL SETCR_MN ( 73, IY4 )
              END IF
              WRITE ( BUFSTR, 139 ) LET(24)
  139         FORMAT(A2)
              CALL ADDSTR_F ( BUFSTR(:2) )
         END IF
      END DO
!
! --- Write control information
!
      CALL SETCR_MN ( 0, CTLN-1 )
      CALL ADDSTR_F ( "Note: Baseline selection overrides station selection." )
      CALL NL_MN()
      CALL ADDSTR_F ( "  (W)All baselines  (Z)No baselines  (N)ext  "// &
     &                "(P)rev  (R)efresh" )
      CALL NL_MN()
      CALL ADDSTR_F ( "  (O)ptin  (L)ast page  (Q)Least squares  (T)erminate" )
!
! --- Read and interpret user input
!
  200 CONTINUE
      CALL SETCR_MN ( 0, CTLN )
  201 CONTINUE
      CALL SENKR_MN ( IX, IY, ICHR )
  210 IOPT = 0
      IF ( CCHAR(4:4) .EQ. CHAR(13) ) CCHAR(4:4) = ' '
      IF ( CCHAR(4:4) .EQ. ' ' ) THEN
!
! -------- Set command names to the command pointed by a position
!
           IF ( IY .EQ. CTLN ) THEN
                IF ( IX.GE. 2 .AND. IX.LE.17 ) CCHAR(4:4) = 'W'
                IF ( IX.GE.20 .AND. IX.LE.34 ) CCHAR(4:4) = 'Z'
                IF ( IX.GE.37 .AND. IX.LE.42 ) CCHAR(4:4) = 'N'
                IF ( IX.GE.45 .AND. IX.LE.50 ) CCHAR(4:4) = 'P'
                IF ( IX.GE.53 .AND. IX.LE.61 ) CCHAR(4:4) = 'R'
             ELSE IF ( IY .EQ. CTLN+1 ) THEN
                IF ( IX.LE. 2 .AND. IX.LE. 8 ) CCHAR(4:4) = 'O'
                IF ( IX.GE.11 .AND. IX.LE.21 ) CCHAR(4:4) = 'L'
                IF ( IX.GE.24 .AND. IX.LE.39 ) CCHAR(4:4) = 'Q'
                IF ( IX.GE.42 .AND. IX.LE.52 ) CCHAR(4:4) = 'T'
             ELSE IF ( IY .EQ. CTLN-3 ) THEN
                IF ( IX .GE. 11 .AND. IX .LE. 26 ) CCHAR(4:4) = 'N'
             ELSE IF ( IY .EQ. 1 ) THEN
                IF ( IX .GE. 11 .AND. IX .LE. 26 ) CCHAR(4:4) = 'P'
           ENDIF
      ENDIF
!
! --- Look for 'WHOLE SET' or 'NO MORE CHANGES' option
!
      IF ( CCHAR(4:4) .EQ. 'W' ) THEN
           IOPT = 1
           GOTO 300
         ELSE IF ( CCHAR(4:4) .EQ. 'Z' ) THEN
           IOPT = 5
           GOTO 350
        ELSE IF ( CCHAR(4:4) .EQ. 'R' ) THEN
           GOTO 910
        ELSE IF ( CCHAR(4:4) .EQ. 'N' ) THEN
           IF ( IPAGE .LT. NUMPG ) IPAGE = IPAGE+1
           GOTO 100
        ELSE IF ( CCHAR(4:4) .EQ. 'P' )THEN
           IF ( IPAGE .GT. 1 ) IPAGE = IPAGE-1
           GOTO 100
        ELSE IF ( CCHAR(4:4) .EQ. 'O'  .OR. &
     &            CCHAR(4:4) .EQ. 'L'  .OR. &
     &            CCHAR(4:4) .EQ. 'Q'  .OR. &
     &            CCHAR(4:4) .EQ. 'T'        ) THEN
!
           IOPT = 2
           GOTO 400
      END IF
!
! --- Look for quick exit (only available if called by SETFL)
!
      IF ( IPROG .EQ. 2 ) THEN
           NUMBL = NMSTA*(NMSTA-1)
           IF ( ( CCHAR(4:4).EQ.'O' .AND. NUMBL.LT.15  )  .OR. &
     &          ( CCHAR(4:4).EQ.'Q' .AND. NUMBL.LT.17  )  .OR. &
     &          ( CCHAR(4:4).EQ.'X' .AND. NUMBL.LT.24  )        ) GOTO 2000
      END IF
!
! --- If no legitimate choice try to interperete the code as a station key
!
      GOTO 600
!
! === 'WHOLE SET' OPTION
!
  300 CONTINUE
!
! --- Set baseline counter
!
      IF ( IPROG .EQ. 1 ) THEN
           IBLFLG = 0
        ELSE
           NBLSEL = NMSTA
      END IF
!
! --- Turn on flags for baseline and station selections and write x's
!
      JM1 = NMSTA - 1
      IY  = IOFF
      NDX = 0
      DO I = 1,JM1
         IP1 = I + 1
         DO J = IP1,NMSTA
            NDX = NDX+1
            CHOSB(NDX) = .TRUE.
            IF ( NDX .GE. ISTART   .AND.  NDX .LE. IEND ) THEN
                 IY = IY + 1
                 CALL SETCR_MN ( 29, IY )
                 WRITE ( BUFSTR, 119 ) LET(24)
                 CALL ADDSTR_F ( BUFSTR(:2) )
            ENDIF
         END DO
      END DO
!
      DO I = 1,NMSTA
         CHOSS(I) = .TRUE.
         IF ( I .LE. (PAGELEN-8) ) THEN
              IY4 = I+1
              CALL SETCR_MN ( 53, IY4 )
            ELSE IF ( I .GT. (PAGELEN-8) ) THEN
              IY4 = I+1-(PAGELEN-8)
              CALL SETCR_MN ( 73, IY4 )
         END IF
         WRITE ( BUFSTR, 139 ) LET(24)
         CALL ADDSTR_F ( BUFSTR(:2) )
      END DO
!
! --- Set all flags (if called by SETFL)
!
      IF ( IPROG .EQ. 2 ) THEN
           DO  I=1,ARC_STA_BIT_WORDS
               DO K = 1,MAX_ARC_STA
                  IF ( DATYP_INQ ( IDATYP, PHASE__DTP ) ) THEN
                      IBLSEL_P(I,K) = -1 ! all 16 bits are on
                    ELSE
                      IBLSEL_G(I,K) = -1 ! all 16 bits are on
                  END IF
               ENDDO
           ENDDO
      END IF
!
! --- Set cursor to 'NO MORE CHANGES'
!
      CALL SETCR_MN ( 0, CTLN )
      CALL SENKR_MN ( IX, IY, ICHR )
      GOTO 210
!
  350 CONTINUE
!
! --- Set baseline counter
!
      IF ( IPROG .EQ. 1 ) THEN
           IBLFLG = 0
         ELSE
           NBLSEL = NMSTA
      END IF
!
! --- Turn on flags for baseline and station selections and write x's
!
      JM1 = NMSTA - 1
      IY  = IOFF
      NDX = 0
      DO I = 1,JM1
         IP1 = I + 1
         DO J = IP1,NMSTA
            NDX = NDX+1
            CHOSB ( NDX ) = .FALSE.
            IF ( NDX .GE. ISTART  .AND.  NDX .LE. IEND)  THEN
                 IY = IY + 1
                 CALL SETCR_MN ( 29, IY )
                 WRITE ( BUFSTR, 119 ) '  '
                 CALL ADDSTR_F ( BUFSTR(:2) )
            ENDIF
         END DO
      END DO
!
      DO I = 1,NMSTA
         CHOSS(I) = .FALSE.
         IF ( I .LE. PAGELEN-8 ) THEN
              IY4 = I+1
              CALL SETCR_MN ( 53, IY4 )
            ELSE IF ( I .GT. PAGELEN-8 ) THEN
              IY4 = I+1 - (PAGELEN-8)
              CALL SETCR_MN ( 73, IY4 )
         END IF
         WRITE ( BUFSTR, 139 ) '  '
         CALL ADDSTR_F ( BUFSTR(:2) )
      END DO
!
! --- Clear all flags (if called by SETFL)
!
      IF ( IPROG .EQ. 2 ) THEN
           DO 317 I=1,ARC_STA_BIT_WORDS
              DO 318 K = 1,MAX_ARC_STA
                 IF ( DATYP_INQ ( IDATYP, PHASE__DTP ) ) THEN
                      IBLSEL_P(I,K) = 0
                   ELSE
                      IBLSEL_G(I,K) = 0
                 END IF
 318          CONTINUE
 317       CONTINUE
      END IF
!
! --- Set cursor to 'NO MORE CHANGES'
!
      CALL SETCR_MN ( 0, CTLN )
      CALL SENKR_MN ( IX, IY, ICHR )
      GOTO 210
!
! --- 'NO MORE CHANGES' OPTION
!
  400 CONTINUE
      GOTO 900
!
! --- 'MASTER STATION' option
!
  500 CONTINUE
!
! --- Determine station number
!
      M = 0
      DO I = 1,NMSTA
         IF ( CCHAR(4:4) .EQ. NUMB9(I:I) ) M = I
      END DO
      WRITE ( BUFSTR, 119 ) CCHAR(3:4)
      CALL ADDSTR_F ( BUFSTR(:2) )
      IF ( M.LE.0 .OR. M.GT.NMSTA ) GOTO 200
!
! --- Apply changes to station
!
      DO I = 1,NMSTA
         IF ( I .EQ. M ) CHOSS(I) = .NOT. CHOSS(I)
         IF ( I .LE. PAGELEN-8 ) THEN
              IY4 = I + IOFF
              CALL SETCR_MN ( 53, IY4 )
            ELSE IF ( I .GT. PAGELEN-8 ) THEN
              IY4 = I + IOFF - (PAGELEN-8)
              CALL SETCR_MN ( 73, IY4 )
         END IF
!
         IF ( CHOSS(I) ) THEN
              WRITE ( BUFSTR, 139 ) LET(24)
              CALL ADDSTR_F ( BUFSTR(:2) )
            ELSE
              CALL ADDSTR_F ( "  " )
         ENDIF
      END DO
!
! --- Apply changes to baseline and station displays
!
      JM1 = NMSTA - 1
      IY  = IOFF
      NDX = 0
      DO I = 1,JM1
         IP1 = I + 1
         DO J = IP1,NMSTA
            NDX = NDX+1
            IY  = IY + 1
            IF ( ( I.EQ.M  .OR. J.EQ.M )  .AND.  CHOSS(M) ) THEN
                 CHOSB(NDX) = .TRUE.
               ELSE IF ( ( I.EQ.M  .OR. J.EQ.M )  .AND.  .NOT. CHOSS(M) ) THEN
                 CHOSB(NDX) = .FALSE.
            END IF
            IF ( NDX.GE.ISTART .AND. NDX.LE.IEND ) THEN
                 L1 = IY-(IPAGE-1)*LISTLEN
                 CALL SETCR_MN ( 29, l1 )
                 IF ( CHOSB(NDX) ) THEN
                      WRITE ( BUFSTR, 119 ) LET(24)
                      CALL ADDSTR_F ( BUFSTR(:2) )
                   ELSE
                      CALL ADDSTR_F ( "  " )
                 ENDIF
            ENDIF
         END DO
      END DO
!
! --- Set baseline counter and flags
!
      IF ( IPROG .EQ. 1 ) THEN
           IBLFLG = 0
           JM1 = NMSTA - 1
           NDX = 0
           DO I = 1, JM1
              IP1 = I + 1
              DO J = IP1, NMSTA
                 NDX=NDX+1
                 IF ( CHOSB(NDX) ) THEN
                      IBLFLG = IBLFLG + 1
                      IBLED ( 1, IBLFLG ) = I
                      IBLED ( 2, IBLFLG ) = J
                 END IF
              END DO
           END DO
         ELSE
           NBLSEL = NMSTA - 1
           DO 530 K=1,ARC_STA_BIT_WORDS
              DO 540 I = 1,MAX_ARC_STA
                 IF ( DATYP_INQ ( IDATYP, PHASE__DTP ) ) THEN
                      IBLSEL_P(K,I) = 0
                   ELSE
                      IBLSEL_G(K,I) = 0
                 END IF
  540         CONTINUE
  530      CONTINUE
!
! -------- Toggle status bits for deselected baseline
!
           JM1 = NMSTA - 1
           NDX=0
           DO I = 1,JM1
              IP1 = I + 1
              DO J = IP1,NMSTA
                 NDX=NDX+1
                 IF ( CHOSB(NDX) ) THEN
                      NBLSEL = NBLSEL + 1
                      IF ( DATYP_INQ ( IDATYP, PHASE__DTP ) ) THEN
                           CALL SBIT ( IBLSEL_P(1,I), J, INT2(1) )
                           CALL SBIT ( IBLSEL_P(1,J), I, INT2(1) )
                         ELSE
                           CALL SBIT ( IBLSEL_G(1,I), J, INT2(1) )
                           CALL SBIT ( IBLSEL_G(1,J), I, INT2(1) )
                      END IF
                 END IF
              END DO
           END DO
!
      END IF
!
! --- Set cursor to 'NO MORE CHANGES'
!
      CALL SETCR_MN ( 0, CTLN )
      CALL SENKR_MN ( IX, IY, ICHR )
      GOTO 210
!
! --- 'TOGGLE POSITION' option
!
  600 CONTINUE
!
! --- Decode station or baseline to toggle
!
      NBL = ((NMSTA*(NMSTA-1)) / 2 ) + 0.0001
      M = 0
      IF ( IY .EQ. CTLN  .OR.  CCHAR(4:4) .NE. ' ' ) THEN
!
! -------- By number or letter (add 100 to letter position)
!
           DO I = 1,NMSTA
              IF ( CCHAR(4:4) .EQ. NUMB9(I:I) ) M = I
           END DO
         ELSE
!
! -------- By position
!
           IF ( IY .GE. 2  .AND. IY .LT. PAGELEN-6 ) THEN
                IF ( IX .LE. 29 ) THEN
                     M = IY-IOFF + (IPAGE-1)*LISTLEN + 100  ! baseline
                  ELSE IF ( IX .GE. 42  .AND. IX .LE. 53 ) THEN
                     M = IY-IOFF    ! station form the middle column
                  ELSE IF ( IX .GE. 62  .AND. IX .LE. 73 ) THEN
                     M = IY-IOFF + PAGELEN-8 ! station from the utmost right column
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
      IF ( M .GT. NMSTA  .AND.  M .LT. 101 ) THEN
           CALL SENKR_MN ( IX, IY, ICHR )
           GOTO 210
      END IF
!
      IF ( M .GT. 100+NBL ) THEN
           CALL SETCR_MN ( 0, CTLN )
           CALL SENKR_MN ( IX, IY, ICHR )
           GOTO 210
      END IF
!
! --- Identify whether it is a station
!
      STATN = .FALSE.
      IF ( M .LT. 101 ) STATN = .TRUE.
!
! --- Toggle the appropriate station or baseline for display
!
      IF ( STATN ) THEN
!
! -------- It was station
!
           CHOSS(M) = .NOT. CHOSS(M)
           IF ( M .LE. PAGELEN-8 ) THEN
                IY4 = M+IOFF
                CALL SETCR_MN ( 53, IY4 )
                IX4_SAVE = 53
                IY4_SAVE = IY4
              ELSE IF ( M .GT. PAGELEN-8 ) THEN
                IY4 = M+IOFF-(PAGELEN-8)
                CALL SETCR_MN ( 73, IY4 )
                IX4_SAVE = 73
                IY4_SAVE = IY4
           END IF
!
           IF ( CHOSS(M) ) THEN
                WRITE ( BUFSTR, 139 ) LET(24)
                CALL ADDSTR_F ( BUFSTR(:2) )
             ELSE
                CALL ADDSTR_F ( "  " )
           ENDIF
!
! -------- If station, then toggle the appropriate baselines, too
!
           JM1 = NMSTA - 1
           L   = IOFF
           NDX = 0
           DO I = 1,JM1
              IP1 = I + 1
              DO J = IP1,NMSTA
                 NDX = NDX+1
                 L = L + 1
                 IF ( M .EQ. I  .OR.  M .EQ. J ) THEN
                      CHOSB(NDX) = CHOSS(M)
                 ENDIF
                 IF ( NDX .GE. ISTART   .AND.   NDX .LE. IEND ) THEN
                      L1 = L-(IPAGE-1)*LISTLEN
                      CALL SETCR_MN ( 29, L1 )
                      IF ( CHOSB(NDX) ) THEN
                           WRITE ( BUFSTR, 119 ) LET(24)
                           CALL ADDSTR_F ( BUFSTR(:2) )
                        ELSE
                           CALL ADDSTR_F ( "  " )
                      ENDIF
                  ENDIF
              END DO
           END DO
           GOTO 810
         ELSE
!
! -------- If it was a baseline
!
           IX4_SAVE = IX
           IY4_SAVE = IY
!
! -------- It was baseline
!
           MM100 = M - 100
           L = 0
           JM1 = NMSTA - 1
           NDX = 0
           DO I = 1,JM1
              IP1 = I + 1
              DO J = IP1,NMSTA
                 NDX=NDX+1
                 L = L + 1
                 IF ( L .EQ. MM100 ) THEN
                      CHOSB(NDX) = .NOT. CHOSB(NDX)
                      L1 = L-(IPAGE-1)*LISTLEN
                      IX4_SAVE = 29
                      IY4_SAVE = L1+IOFF
                      CALL SETCR_MN ( IX4_SAVE, IY4_SAVE )
                      IF ( CHOSB(NDX) ) THEN
                           WRITE ( BUFSTR, 119 ) LET(24)
                           CALL ADDSTR_F ( BUFSTR(:2) )
                        ELSE
                           CALL ADDSTR_F ( "  " )
                      ENDIF
                 END IF
              END DO
           END DO
      END IF
!
! --- Update station flags
!
      DO I = 1,NMSTA
         CHOSS(I) = .FALSE.
      END DO
      NDX = 0
      DO I = 1,NMSTA-1
         DO J = I+1,NMSTA
            NDX = NDX+1
            IF ( CHOSB(NDX) ) THEN
                 CHOSS(I) = .TRUE.
                 CHOSS(J) = .TRUE.
            END IF
         END DO
      END DO
!
! --- Now display station flags
!
      DO I=1,NMSTA
         IF ( I .LE. (PAGELEN-8) ) THEN
              IY4 = I+IOFF
              CALL SETCR_MN ( 53, IY4 )
            ELSE
              IY4 = I+IOFF-(PAGELEN-8)
              CALL SETCR_MN ( 73, IY4 )
         END IF
!
         IF ( CHOSS(I) ) THEN
              WRITE ( BUFSTR, 119 ) LET(24)
              CALL ADDSTR_F ( BUFSTR(:2) )
            ELSE
              CALL ADDSTR_F ( "  " )
         ENDIF
      END DO
 810  CONTINUE
!
! --- Restroing cursor position
!
      CALL SETCR_MN ( IX4_SAVE, IY4_SAVE )
!
! --- Set baseline counter and flags, according to baseline display array
!
      IF ( IPROG .EQ. 1 ) THEN
           IBLFLG = 0
           JM1 = NMSTA - 1
           NDX = 0
           DO I = 1, JM1
              IP1 = I + 1
              DO J = IP1, NMSTA
                 NDX=NDX+1
                 IF ( CHOSB(NDX) ) THEN
                      IBLFLG = IBLFLG + 1
                      IBLED ( 1, IBLFLG ) = I
                      IBLED ( 2, IBLFLG ) = J
                 END IF
              END DO
           END DO
         ELSE
           NBLSEL = 0
           DO 650 K=1,ARC_STA_BIT_WORDS
              DO 660 I = 1,MAX_ARC_STA
                 IF ( DATYP_INQ ( IDATYP, PHASE__DTP ) ) THEN
                      IBLSEL_P(K,I) = 0
                    ELSE
                      IBLSEL_G(K,I) = 0
                 END IF
  660         CONTINUE
  650      CONTINUE
!
           JM1 = NMSTA - 1
           NDX=0
           DO I = 1,JM1
              IP1 = I + 1
              DO J = IP1,NMSTA
                 NDX=NDX+1
                 IF ( CHOSB(NDX) ) THEN
                      NBLSEL = NBLSEL + 1
                      IF ( DATYP_INQ ( IDATYP, PHASE__DTP ) ) THEN
                           CALL SBIT ( IBLSEL_P(1,I), J, INT2(1) )
                           CALL SBIT ( IBLSEL_P(1,J), I, INT2(1) )
                        ELSE
                           CALL SBIT ( IBLSEL_G(1,I), J, INT2(1) )
                           CALL SBIT ( IBLSEL_G(1,J), I, INT2(1) )
                      END IF
                 END IF
              END DO
           END DO
      END IF
!
! --- Set cursor to `TOGGLE POSITION`
!
      IF ( IY .EQ. CTLN ) THEN
           CALL SETCR_MN ( 0, CTLN )
      END IF
      CALL SENKR_MN ( IX, IY, ICHR )
      GOTO 210
!
! --- Return flag array to sdbh or setfl
!
 900  CONTINUE
2000  CONTINUE
      IF ( IPROG .EQ. 1) THEN
           KBUF(MAX_ARC_STA*ARC_STA_BIT_WORDS+2) = IBLFLG
           DO II = 1,60
              DO JJ = 1,2
                  KBUF((II-1)*2 + JJ + 64) = IBLED(JJ,II)
              END DO
           END DO
!
           CALL USE_BUFFER ( KBUF, INT2(187), 'OW' )
         ELSE
!
! ------ Initialize icont here. Mallama. December 13, 1983.
!
         ICONT =  0
         IF ( CCHAR(4:4) .EQ. 'Q' ) ICONT = -3
         IF ( CCHAR(4:4) .EQ. 'O' ) ICONT = -7
         IF ( CCHAR(4:4) .EQ. 'T' ) ICONT = -15
!
! ------ Provide route to SLDB (Select baseline). Mallama, February 14, 1984.
!
         IF ( CCHAR(4:4) .EQ. 'X' ) ICONT =-31
         DO K=1,ARC_STA_BIT_WORDS
            DO I=1,MAX_ARC_STA
               KK=(I-1)*ARC_STA_BIT_WORDS+K
               IF ( DATYP_INQ ( IDATYP, PHASE__DTP ) ) THEN
                    KBUF(KK) = IBLSEL_P(K,I)
                 ELSE
                    KBUF(KK) = IBLSEL_G(K,I)
               END IF
            ENDDO
         ENDDO
         KBUF(MAX_ARC_STA*ARC_STA_BIT_WORDS+1)=ICONT
         CALL USE_BUFFER ( KBUF, INT2(MAX_ARC_STA*ARC_STA_BIT_WORDS+4), 'OW' )
      END IF
      CALL END_MN()
      CALL END_PROG()
!
      END  !#!  SLVEB  #!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE GET_STATUS ( CHOSB, CHOSS )
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
      INCLUDE    'solve.i'
      INCLUDE    'socom.i'
      LOGICAL*2  CHOSB(*)
      LOGICAL*2  CHOSS(*)
      INTEGER*2  J1, J2, NBS
      LOGICAL*2  KBIT
      LOGICAL*4  DATYP_INQ
!
! --- Determine old status of baseline flags
!
      NBS  = 0
      DO 410 J1=1,NUMSTA-1
         DO 420 J2=J1+1,NUMSTA
            NBS = NBS+1
            CHOSB(NBS) = .FALSE.
            IF ( DATYP_INQ ( IDATYP, PHASE__DTP ) ) THEN
                 IF ( KBIT(IBLSEL_P(1,J1), J2) ) THEN
                      CHOSB(NBS) = .TRUE.
                      CHOSS(J1)  = .TRUE.
                      CHOSS(J2)  = .TRUE.
                 END IF
                 IF ( KBIT(IBLSEL_P(1,J2), J1) ) THEN
                      CHOSB(NBS) = .TRUE.
                      CHOSS(J1)  = .TRUE.
                      CHOSS(J2)  = .TRUE.
                 END IF
               ELSE
                 IF ( KBIT(IBLSEL_G(1,J1), J2) ) THEN
                      CHOSB(NBS) = .TRUE.
                      CHOSS(J1)  = .TRUE.
                      CHOSS(J2)  = .TRUE.
                 END IF
                 IF ( KBIT(IBLSEL_G(1,J2), J1) ) THEN
                      CHOSB(NBS) = .TRUE.
                      CHOSS(J1)  = .TRUE.
                      CHOSS(J2)  = .TRUE.
                 END IF
            END IF
 420     CONTINUE
 410  CONTINUE
      RETURN
      END  !#!  GET_STATUS  #!#
