      SUBROUTINE SRFLG ( IKONT )
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
! 1.  SRFLG PROGRAM SPECIFICATION
!
! 1.1 Display and set the star parameter flags.
!
! 1.2 REFERENCES:
!
! 2.  SRFLG INTERFACE
!
! 2.1 Parameter File
      INCLUDE 'solve.i'
      INCLUDE 'fclib.i'
!
! 2.3 OUTPUT Variables:
!
      INTEGER*2 IKONT
!
! IKONT - Option flag (to run least squares or OPTIN from here)
!
! 2.4 COMMON BLOCKS USED
      INCLUDE 'socom.i'
      INCLUDE 'glbcm.i'
      INCLUDE 'glbc4.i'
      INCLUDE 'prfil.i'
      INCLUDE 'precm.i'
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES: setfl
!       CALLED SUBROUTINES: None
!
! 3.  LOCAL VARIABLES
!
      INTEGER*2 IDSP(16), I2_CHAR, J, I, IPOS, IPAGE, NUMPG, ISTART, IEND, &
     &          NLINE, ISNUM, KBITN, ILAST ,ICOD, IALPHA, L, LISTLEN
      LOGICAL*2 KBIT
      CHARACTER   ALENS*3, INFLY*1, BUFSTR*79, STR*54, GET_VERSION*54
      INTEGER*4   IX, IY, ICH, NCNT, PAGELEN, PAGEWID
      CHARACTER*4 CCH
      CHARACTER*2 CCHAR
      EQUIVALENCE (ICH,CCH)
      EQUIVALENCE (I2_CHAR,CCHAR)
      CHARACTER*26     ALPHABET
      DATA ALPHABET / 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'/
      INTEGER*2    INT2_HA
      PARAMETER  ( INT2_HA = 1HA )
      INTEGER*2    INT2_ARG
      INTEGER*4    INT4
      INT4(INT2_ARG) = INT(INT2_ARG,KIND=4)
      INTEGER*4  I_LEN
!
! 4.  HISTORY
!   WHO   WHEN   WHAT
!   JWR  830319  Implemented 'X' and 'B' characters from SENCR
!   JWR  831031  Proper motion flag setting removed
!   PET  1999.10.12  Changed the way of treating versions
!   PET  1999.12.07  Corected a bug with wring cursor addresation.
!                    Added printing the current page number, source number,
!                    total number of sources, totatl number of pages. Improved
!                    comments
!   PET  2000.01.10  Small fix: ther previous version didn't allow to set source
!                    estimation flag for the last source
!   pet  2003.12.09  Replaced fc_gwinw, fc_gwinsz  calls with GET_TERMSIZE
!
! 5.  SRFLG PROGRAM STRUCTURE
!
  50  CONTINUE
!
! --- Set up the crt display
!
      IPAGE = 1
      CALL GET_TERMSIZE ( PAGELEN, PAGEWID ) 
      IF ( PAGELEN .EQ. 0 ) PAGELEN = 24
      LISTLEN = PAGELEN-9
      NUMPG = 1 + (NUMSTR-1)/LISTLEN
  75  CONTINUE
!
! --- Cursor goes to the left upper corner
!
      CALL SETCR_MN ( 0, 0 )
!
! --- Clear display
!
      CALL CLEAR_MN()
!
! --- Print the header line
!
      WRITE ( BUFSTR, 25 ) IPAGE, NUMPG, PRE_LETRS
   25 FORMAT ( " Source coordinate flags     Page  ",I2,"(",I2,")", 8X, &
     &         "User ", A2 )
      CALL ADDSTR_F ( BUFSTR )
      CALL NL_MN()
!
      STR = GET_VERSION()
      CALL SETCR_MN ( 79-I_LEN(STR), 0 )
      CALL REVERSE_ON_MN()
      CALL ADDSTR_F ( STR(1:I_LEN(STR)) )
      CALL REVERSE_OFF_MN()
      CALL NL_MN()
!
      NLINE = 1
      ISTART = (IPAGE-1)*LISTLEN+1
      IEND = MIN0 ( INT4(NUMSTR), ISTART+LISTLEN-1 )
!
      IF ( ISTART .GT. 1 ) THEN
           CALL ADDSTR_F ( '(-More-)' )
      ENDIF
!
! --- Print the portion of sources
!
      DO 200 I=ISTART,IEND
         DO 100 J=1,2
            IDSP(J)=KBITN(LSTAR(1,J),I)
  100    CONTINUE
!
         INFLY = '%'
         IF ( STRDIF(1,I).NE.0 .OR. STRDIF(2,I).NE.0 ) INFLY=' '
         IDSP(3)=KBITN(ISRSEL,I)
         IALPHA = MOD(I-1,26)+1
         WRITE ( BUFSTR, 125 ) (ISTRN(L,I),L=1,4),(IDSP(L),L=1,3), &
     &           ALPHABET(IALPHA:IALPHA), INFLY, I, NUMSTR
  125    FORMAT ( 4A2," RA, Dec  ",2I2,2X,I2,2X,A1,1X,A1,3X,I3,"(",I3,")" )
         CALL NL_MN()
         CALL ADDSTR_F ( BUFSTR )
         NLINE = NLINE + 1
  200 CONTINUE
      NCNT = NLINE + 4
!
      CALL NL_MN()
      IF ( IEND .LT. NUMSTR ) THEN
           CALL ADDSTR_F ( "(-More-)" )
           CALL NL_MN()
           NCNT=NCNT+1
      ENDIF
!
! --- Set up the control line(s).
!
      ALENS='NO'
      IF ( KBIT( IPRES, INT2(2) ) ) ALENS='YES'
      CALL NL_MN()
      WRITE ( BUFSTR, 325 ) ALENS
  325 FORMAT( 'All flags (*)ON  All flags OF(F)   Print (A)rc Lengths: ',A )
      CALL ADDSTR_F ( BUFSTR )
      CALL NL_MN()
      CALL ADDSTR_F ( "Page: (N)ext  (P)revious" )
      CALL NL_MN()
      CALL ADDSTR_F ( "(O)PTIN   (L)ast page   (T)erminate   Least s(Q)uares" )
      CALL NL_MN()
      CALL ADDSTR_F ( "'%' in last column means source is not in flyby list" )
!
! --- Reset the cursor to the top.
!
      CALL SETCR_MN ( 19, 2 )
!
  350 CONTINUE
      ICOD = INT2_HA
  360 CONTINUE
!
! --- Ask user for input
!
      CALL SENKR_MN ( IX, IY, ICH )
      CCHAR(1:1) = CCH(4:4)
      IF ( CCHAR(1:1) .EQ. ' ' ) THEN
           IF ( IY .EQ. 0  .OR.  IY .EQ. 1 ) THEN
                CCHAR(1:1) = 'P'
             ELSE IF ( ( IEND .LT. NUMSTR ) .AND. &
     &                 ( IY .EQ. NCNT-4 .OR. IY .EQ. NCNT-3 ) ) THEN
                CCHAR(1:1) = 'N'
             ELSE IF ( IY .EQ. NCNT-2 ) THEN
                IF ( IX .LE. 14 ) CCHAR(1:1) = '*'
                IF ( IX .GE. 17 .AND. IX .LE. 31 ) CCHAR(1:1) = 'F'
                IF ( IX .GE. 35 .AND. IX .LE. 58 ) CCHAR(1:1) = 'A'
             ELSE IF ( IY .EQ. NCNT-1 ) THEN
                IF ( IX .GE. 6  .AND. IX .LE. 11 ) CCHAR(1:1) = 'N'
                IF ( IX .GE. 14 .AND. IX .LE. 23 ) CCHAR(1:1) = 'P'
             ELSE IF ( IY .EQ. NCNT ) THEN
                IF ( IX .LE. 6 ) CCHAR(1:1) = 'O'
                IF ( IX .GE. 10  .AND.  IX .LE. 20 ) CCHAR(1:1) = 'L'
                IF ( IX .GE. 24  .AND.  IX .LE. 34 ) CCHAR(1:1) = 'T'
                IF ( IX .GE. 38  .AND.  IX .LE. 52 ) CCHAR(1:1) = 'Q'
           ENDIF
      ENDIF
      ILAST = 0
      IF ( CCHAR(1:1) .EQ. 'O') THEN  ! setup to run optin
           IKONT = -7
           RETURN
      END IF  !setup to run optin
!
      IF ( CCHAR(1:1) .EQ. 'T' ) THEN  !terminate SOLVE
!
! -------- Count the parameter flags
!
           ICLMAX = 0
           CALL PARCN()
           CALL USE_COMMON ( 'OWC' )
           CALL RUN_PROG   ( 'SLEND', 'PASS', INT2(0) )
      END IF  !terminate SOLVE
!
      IF ( CCHAR(1:1) .EQ. 'Q' .OR. CCHAR(1:1) .EQ. 'A' ) GOTO 361
      IF ( CCHAR(1:1) .EQ. '*' ) GOTO 550
      IF ( CCHAR(1:1) .EQ. 'F' ) GOTO 525
      IF ( CCHAR(1:1) .EQ. 'N' .OR. &
     &     CCHAR(1:1) .EQ. 'P' .OR. &
     &     CCHAR(1:1) .EQ. 'I' .OR. &
     &     CCHAR(1:1) .EQ. 'L' .OR. &
     &     CCHAR(1:1) .EQ. CHAR(82) .OR. &
     &     CCHAR(1:1) .EQ. CHAR(83)      ) GOTO 362
      IF ( CCHAR(1:1) .EQ. 'B' .OR. CCHAR(1:1) .EQ. 'X'      ) GOTO 585
      GOTO 363
  362 CONTINUE
      ILAST = -1
      GOTO 364
  363 CONTINUE
      IF ( CCHAR(1:1) .NE. ' ' ) GOTO 360
  361 CONTINUE
      IF ( CCHAR(1:1) .EQ. 'Q' ) THEN
!
! ------- skip out to run least squares
!
          CALL SETCR_MN ( 0, 0 )
          CALL CLEAR_MN()
          IKONT = -3
          RETURN
      END IF  !skip out to run least squares
!
      IF ( CCHAR(1:1) .EQ. 'A' ) THEN
           CALL SWBIT( IPRES, INT2(2) )
           IF ( ALENS .EQ. 'YES' ) THEN
                ALENS='NO'
              ELSE
                ALENS='YES'
           ENDIF
!
           CALL SETCR_MN ( 56, NCNT-2 )
           WRITE ( BUFSTR, "(A)" ) ALENS
           CALL ADDSTR_F ( BUFSTR(:3) )
           CALL SETCR_MN ( IX, IY )
           GOTO 350
      ENDIF
!
 364  CONTINUE
      IF ( IY .EQ. NCNT .OR. ILAST .EQ. -1 ) THEN
!
! -------- Selecting new page
!
           IKONT = -2
           IF ( CCHAR(1:1) .EQ. 'N'  .OR.  CCHAR(1:1) == CHAR(82) ) THEN
                IF ( IPAGE .LT. NUMPG ) THEN
                     IPAGE = IPAGE+1
                     GOTO 75
                  ELSE
                     IKONT = -2
                ENDIF
           ENDIF
           IF ( CCHAR(1:1) .EQ. 'P' .OR.  CCHAR(1:1) == CHAR(83) ) THEN
                IF ( IPAGE .GT. 1 ) THEN
                     IPAGE=IPAGE-1
                     GOTO 75
                  ELSE
                     IKONT = NUMSTA
                ENDIF
          ENDIF
          RETURN
      END IF  ! selecting new page
!
      IPOS = 0
      IF ( IX .EQ. 19  ) IPOS = 1
      IF ( IX .EQ. 21  ) IPOS = 2
      IF ( IX .EQ. 25  ) IPOS = 3
      IF ( IPOS .EQ. 0 ) GOTO 350
!
!---- Process star flags
!
      ISNUM = (IPAGE-1)*LISTLEN + IY - 1
      IF ( ISNUM .LT. 1  .OR.  ISNUM .GT. NUMSTR ) GOTO 350
      IF ( IPOS .NE. 3 ) THEN
           CALL SWBIT ( LSTAR(1,IPOS), ISNUM )
           IDSP(1) = KBITN ( LSTAR(1,IPOS), ISNUM )
        ELSE
           CALL SWBIT ( ISRSEL, ISNUM )
           IDSP(1) = KBITN ( ISRSEL, ISNUM )
      ENDIF
!
! --- Position the cursor
!
      CALL SETCR_MN(IX,IY )
      WRITE ( BUFSTR, '(I1)' ) IDSP(1)
      CALL ADDSTR_F ( BUFSTR(:1) )
      IF ( IPOS .EQ. 1 ) IX = IX + 2
      IF ( IPOS .EQ. 1  .OR.  IY .GT. NLINE  .OR.  IPOS .EQ. 3 ) GOTO 406
      IY = IY + 1
      IX = IX - 2
  406 CONTINUE
      CALL SETCR_MN ( IX, IY )
      GOTO 350
!
! --- Process the control line
!
  500 CONTINUE
      IPOS = IX/15 + 1
      GOTO ( 600, 525, 550, 575 ) IPOS
!
! --- Turn all star flags off
!
  525 CONTINUE
      DO 540 I = 1,SRC_BIT_WORDS
         DO 535 J = 1,2
            LSTAR(I,J)=0
  535    CONTINUE
  540 CONTINUE
      GOTO 50
!
! --- Turn all star flags on
!
  550 CONTINUE
      DO 570 I = 1,SRC_BIT_WORDS
         DO 565 J = 1,2
            LSTAR(I,J) = -1 ! All 16 bits on
  565    CONTINUE
  570 CONTINUE
      GOTO 50
!
  575 CONTINUE
!
! --- Processing hidden commands to select baselines or data bases
!
  585 CONTINUE
      IF ( CCHAR(1:1) .EQ. 'B' ) IKONT = -5
      IF ( CCHAR(1:1) .EQ. 'X' ) IKONT = -6
!
  600 CONTINUE
!
      RETURN
      END  !#!  SRFLG  #!#
