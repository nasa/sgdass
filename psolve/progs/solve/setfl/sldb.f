      SUBROUTINE SLDB(IKONT,CHR)
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!     Updated to specificaly type integers which
!-------------------------------------------------
!
! 1.  SLDB PROGRAM SPECIFICATION
!
! 1.1.   SLDB retrieves data from NAMFIL, a scratch file containing data
!  base information, and passes OBSFIL pointer information to SOLVE
!  common.  If the information is already in common, then SLDB displays
!  the names of data bases, and allows the user to select a subset of
!  them for inclusion in subsequent least-squares solutions.
!
!  RESTRICTIONS: OBSFIL must contain no more than 15 data bases.
!
!  CALLING SEQUENCE:  CALL SLDB(IKONT,CHR)
!
! 1.2 REFERENCES:
!
! 2.  SLDB INTERFACE
!
! 2.1 Parameter File
      INCLUDE 'solve.i'
!
! 2.2 INPUT Variables:
!
      CHARACTER*70 JBUF
!
! JBUF - String buffer from NAMFIL scratch file
!
! 2.3 OUTPUT Variables:
!
       INTEGER*2 IKONT
       character*1 CHR
!
! ICHAR - One letter from SENCR
! IKONT - Option flag (to run least squares or OPTIN from here)
!
! 2.4 COMMON BLOCKS USED
      INCLUDE 'socom.i'
      INCLUDE 'glbcm.i'
      INCLUDE 'precm.i'
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES: setfl,rmflg
!       CALLED SUBROUTINES: None
!
! 3.  LOCAL VARIABLES
!
      LOGICAL*2 KBIT
!
      INTEGER*2 LDBNAM(5,15),IDBVER(15),INAMDC(144),IERR(2), &
     &IXORS(15), IONBUF(12), NYLABL(2,2), LCF(2) , IWAT, JERR, &
     &IOFF, IWF, N, K, NYFLAG, IFLAG, IDB, KBITN, ICF, J, &
     &ILOG,  ITBIT, MTBIT, NSEC,  I,ichar ,numdb
      integer*4 ios, ifrst, ilong
      integer*4 ix,iy,ich,numdb4,n4
      integer*4 inc_col, res_col !columns for output status (yes,no)
      DATA inc_col /41/
      DATA res_col /54/
      character*4 cch
      character*2 cchar
      equivalence (ich,cch)
      equivalence (ichar,cchar)
      character*79 bufstr
!
      DATA NYLABL /2HNO,2H  ,2HYE,2HS /, LCF /1H.,1H /
      INTEGER*4    I_LEN
!
!                        DEFINITION                            TO/FROM
!
!        IERR(2)      = Error flag array
!        NUMDB        = Number of data bases
!        LDBNAM(5,15) = Data base names                         NAMFIL
!        IDBVER(15)   = Data base version numbers               NAMFIL
!        IDBEND(15)   = File pointer to end of data base        NAMFIL
!                         (logical record counter)
!        IXORS(15)    = Frequency band flag (X- or S-)          NAMFIL
!        MTBIT        =  Number of first unused bit of Include Flag
!        IDBSEL       =  Data base selection bit flag
!                        BIT N is on if Nth data base is "selected"
!                        BIT 16 tells SLDB to pass info to common
!        IY           =  Line count of cursor, from top of page
!        ICHAR        =  User-input control character
!         IWF         =  NO DATA warning flag
!         IFRST       =  Logical record in OBSFIL of data base
!         NYFLAG      =  YES/NO flag:  1 no, 2 yes
!         IDB         =  Table entry number of data base
!     LSLDB (3)    6-character name of this routine
!     LASTED(6)   12-character date of last edit
!     NNAMF(3)     6-char. name of scratch file with data base names
!
! 4.  HISTORY
!   WHO   WHEN   WHAT
!   MAP  820319  Modified to 'RUN LEAST SQUARES' from here
!   CMA  820413  Option to suppress CRES by database
!   JWR  830816  Clean up suppress CRES by database
!   kdb  951207  Integer*4 number of observations.
!   kdb  960412  Fix up formatting problems.
!   jwr  2005:05:05: i_len added to buffer read.
!
! 5.  SLDB PROGRAM STRUCTURE
!
!     Get database names from NANFIL (from SDBH segment GETOB)
!
      NUMDB = 0
      JERR  = 0
      DO WHILE (NUMDB.LT.15 .AND. JERR.EQ.0)
        NUMDB = NUMDB + 1
        CALL GETCARD( NUMDB, 'INIT', INT2(1), JBUF, JERR )
        IF (JERR.LT.0 .AND. JERR.NE.-6) GO TO 990
        IF (JERR.EQ.0) THEN !Another card found
          READ (JBUF,910,IOSTAT=ios) NSEC, (LDBNAM(J,NUMDB),J=1,5), &
     &         IDBVER(NUMDB), IDBEND(NUMDB), IXORS(NUMDB)
          CALL FERR( INT2(IOS), "Reading NAMFIL INIT card", INT2(0), INT2(0) )
  910     FORMAT(5X,I4,1X,5A2,I4,11X,I11,1X,A2)
        END IF !Another card found
!
        IF (JERR.NE.0) THEN !No more cards
        NUMDB = NUMDB - 1
        END IF !No more cards
      END DO
      CALL CLOSENAMFIL()
!
!  IF CALL IS TO STORE NEW DATA BASE INFO. IN COMMON, ZERO UNUSED FLAG BITS.
!
      IF (KBIT( IDBSEL, INT2(16)))THEN  !zero unused flag bits
        MTBIT = NUMDB + 1
        DO  ITBIT = MTBIT,15
          CALL SBIT( IDBSEL, ITBIT, INT2(0) )
        END DO
!
!  IF CALL IS FOR DATA BASE SELECTION OPTION, THEN:
!
      ELSE  !select specific data bases for solution
        IY = 0
        CCHAR = '  '
!
!    DISPLAY DATA BASE TABLE INITIALLY, OR REPEAT DISPLAY IF REQUESTED
!
!       INITIALIZE WARNING FLAG
        IWF = 0
        DO WHILE (CCHAR(1:1).NE.'L' .AND. CCHAR(1:1).NE.'Q' .AND. &
     &         CCHAR(1:1).NE.'O' .AND. CCHAR(1:1).NE.'B')
!
!     display data base table
!
          CALL setcr_mn( 0, 0 )
          CALL clear_mn()
          call addstr_f("Databases in OBSFIL" )
        call nl_mn()
        call nl_mn()
        call &
     &       addstr_f("Name        Vers         # of   Freq   Include in   Generate" )
        call nl_mn()
        call &
     &       addstr_f("                         obs    band   solution?    residuals?" )
          call nl_mn()
          call nl_mn()
          IFRST = 0
          DO 100 J = 1, NUMDB
            ILONG = IDBEND(J) - IFRST
            IFRST = IDBEND(J)
            IFLAG = KBITN(IDBSEL,J)
            NYFLAG = IFLAG+1
!
!           Get CRES suppression flag
!
            IFLAG = KBITN(IDCSEL,J)
            ICF=IFLAG+1
            WRITE(bufstr,60) (LDBNAM(I,J),I=1,5),IDBVER(J),ILONG, &
     &                   IXORS(J),(NYLABL(K,NYFLAG),K=1,2), &
     &                   (NYLABL(K,ICF),K=1,2)
  60        FORMAT(5A2,I5,I14,2X,A2,"BAND",4X,2A2,9X,2A2)
            call addstr_f(bufstr )
            call nl_mn()
  100     CONTINUE
          call nl_mn()
          call &
     &         addstr_f("(A)ll databases in solution   (N)o databases in solution " )
          call nl_mn()
          write(bufstr,101)
  101     &
     &FORMAT('(L)ast Page    Run least-s(Q)uares   Run (O)PTIN' &
     &      ,'   (B)aselines')
          call addstr_f(bufstr )
          call nl_mn()
!
! *** Add instructions for toggling solution,residuals on/off ***
! ***  MWH 910417  ***
!
          call nl_mn()
          call &
     &         addstr_f("With cursor set on line of desired database:" )
          call nl_mn()
          call &
     &         addstr_f("Space bar toggles inclusion; (C) toggles residuals" )
          call nl_mn()
!
!    READ CURSOR AND FOLLOW ORDERS
!
          IOFF = 6
          IY  = NUMDB + IOFF
          CALL setcr_mn ( 0, IY )
          CALL SENKR_MN(IX,IY,ICH )
          cchar(1:1) = cch(4:4)
          IF  (CCHAR(1:1).EQ. &
     &      ' ')THEN  !set ICHAR to letter command
            IF (IY.LT.4) CCHAR(1:1) = 'R'
            if (iy.eq.numdb+ioff)then
              if (ix.le.27) cchar(1:1) = 'A'
              if (ix.ge.31.and.ix.le.56) cchar(1:1) = 'N'
            else if (iy.eq.numdb+ioff+1) then
              IF (IX.LT.10) CCHAR(1:1) = 'L'
              IF (ix.ge.15.and.ix.le.33) CCHAR(1:1) = 'Q'
              IF (ix.ge.37.and.ix.le.47) CCHAR(1:1) = 'O'
              IF (ix.ge.51.and.ix.le.61) CCHAR(1:1) = 'B'
            endif
          END IF  !set ICHAR to letter command
          chr = cchar(1:1)
!
!  EXECUTE USER'S INSTRUCTION AND PROMPT FOR NEXT INSTRUCTION.  REPEAT
!  UNTIL 'END' IS REQUESTED AND AT LEAST ONE DATA BASE FLAG IS SET.
!
          DO WHILE ( CCHAR(1:1).NE.'R' .AND. CCHAR(1:1).NE.'L' .AND. &
     &            CCHAR(1:1).NE.'Q' .AND. CCHAR(1:1).NE.'O' .AND. &
     &            CCHAR(1:1).NE.'B')
!
!     follow orders and ask for more
!
!         SWITCH A SINGLE FLAG
!
            IF ((IY.GT.4).AND.(IY.LE.NUMDB+4).AND.(CCHAR(1:1).EQ. &
     &      ' ').AND.(IX.LE. &
     &        42))THEN  !switch 'put in solution' flag
              IDB = IY - 4
              CALL SWBIT(IDBSEL,IDB )
              IFLAG = KBITN(IDBSEL,IDB)
              NYFLAG = IFLAG + 1
              CALL setcr_mn(inc_col,IY )
              WRITE(bufstr,550) (NYLABL(K,NYFLAG),K=1,2)
              call addstr_f(bufstr(1:4) )
  550         FORMAT(2A2)
              CALL setcr_mn(inc_col,IY )
            END IF  !switch 'put in the solution' flag
!
! ****. CMA 820413 ACCEPT 'C' TO SUPPRESS CRES FOR DATA BASE
!
            IF  (IY.GT.4.AND.IY.LE.NUMDB+4.AND.(CCHAR(1:1).EQ. &
     &      'C'.OR.IX.GT. &
     &        42))THEN  !switch CRES flag
                IDB=IY-4
                CALL SWBIT(IDCSEL,IDB )
                IFLAG = KBITN(IDCSEL,IDB)
                NYFLAG = IFLAG + 1
                CALL setcr_mn(res_col,IY )
                WRITE(bufstr,550) (NYLABL(K,NYFLAG),K=1,2)
                call addstr_f(bufstr(1:4) )
                CALL setcr_mn(res_col,IY )
                END IF  !switch CRES flag
!
!         LIMBO BETWEEN MENU AND LETTER OPTIONS:  MOVE CURSOR TO (E)ND LINE
!
            numdb4 = numdb
            IF  ( (IY.GT.NUMDB+4) .AND. &
     &            (IY.LE.NUMDB+6) .AND. &
     &            (CCHAR(1:1).EQ. ' ')  )CALL SETCR_MN ( 1, NUMDB4+8)
!
!           SET ALL FLAGS
!
            IF ( CCHAR(1:1) .EQ. 'A' ) THEN  !set all flags
              DO 750 N = 1,NUMDB
                CALL SBIT( IDBSEL, N, INT2(1) )
                n4 = n
                CALL setcr_mn(inc_col,N4+4 )
                WRITE(bufstr,550) NYLABL(1,2),NYLABL(2,2)
                call addstr_f(bufstr(1:4) )
  750         CONTINUE
              CALL setcr_mn(1,5 )
            END IF  !set all flags
!
!           ZERO ALL FLAGS
!
            IF (CCHAR(1:1).EQ. &
     &        'N')THEN  !zero all flags
              DO 850 N = 1,NUMDB
                CALL SBIT( IDBSEL, N, INT2(0) )
                n4 = n
                CALL setcr_mn(inc_col,N4+4 )
                WRITE(bufstr,550) NYLABL(1,1),NYLABL(2,1)
                call addstr_f(bufstr(1:4) )
  850         CONTINUE
              CALL setcr_mn ( 1, 4 )
            END IF  !zero all flags
!
!           TERMINATION REQUESTED, WITH NO DATA BASE FLAGS SET: ISSUE WARNIN
!
            numdb4 = numdb
            IF (IDBSEL.EQ. &
     &        0)THEN  !warning: no data for solution
              IWF = 1
              CALL setcr_mn ( 1, NUMDB4+12 )
              call &
     &             addstr_f("WARNING: NO DATA BASES FLAGGED FOR LEAST SQUARES" )
              CALL setcr_mn ( 1, 4 )
            ELSE  ! turn warning off if its on
              IF (IWF.EQ. &
     &          1)THEN  !
                IWF = 0
                CALL setcr_mn ( 1, NUMDB4+12 )
                call &
     &               addstr_f("                                                " )
                CALL setcr_mn ( 1, 5 )
              END IF  !
            END IF  ! turn warning off if its on
!
            CALL SENKR_MN(IX,IY,ICH )
            cchar(1:1) = cch(4:4)
          IF  (CCHAR(1:1).EQ. &
     &      ' ')THEN  !set ICHAR to letter command
            IF (IY.LT.4) CCHAR(1:1) = 'R'
            if (iy.eq.numdb+ioff)then
              if (ix.le.27) cchar(1:1) = 'A'
              if (ix.ge.31.and.ix.le.56) cchar(1:1) = 'N'
            else if (iy.eq.numdb+ioff+1) then
              IF (IX.LT.10) CCHAR(1:1) = 'L'
              IF (ix.ge.15.and.ix.le.33) CCHAR(1:1) = 'Q'
              IF (ix.ge.37.and.ix.le.47) CCHAR(1:1) = 'O'
              IF (ix.ge.51.and.ix.le.61) CCHAR(1:1) = 'B'
            endif
          END IF  !set ICHAR to letter command
          chr = cchar(1:1)
!
          END DO  !follow orders and ask for more
        END DO  !display data base table
!
      END IF  !select specific data bases for solution
!
!
      NDB = NUMDB
!
!  NORMAL TERMINATION
!
!   Fix CALL CLOSE(INAMDC) to CALL CLOSENAMFIL
!
!     CALL CLOSE(INAMDC)
      CALL CLOSENAMFIL()
      IF(CCHAR(1:1).EQ.'Q') IKONT = -3
      IF(CCHAR(1:1).EQ.'O') IKONT = -7
      IF(CCHAR(1:1).EQ.'B') IKONT =-15
      CALL setcr_mn ( 0, 0 )
      CALL clear_mn()
      RETURN
!
!  ABNORMAL TERMINATION
!
  990 CONTINUE
      CALL NL_MN()
      CALL NL_MN()
      CALL NL_MN()
      CALL ADDSTR_F ( " ERROR accessing data base information on NAMFIL:" )
      CALL NL_MN()
      WRITE ( bufstr, 995 ) JERR
  995 FORMAT(' error codes is:',I6,' from GETCARD.')
      CALL ADDSTR_F ( BUFSTR )
      CALL NL_MN()
      CALL ADDSTR_F ( " Hit RETURN key to continue:" )
      CALL GETSTR_F ( BUFSTR )
      READ ( BUFSTR(1:I_LEN(BUFSTR)), 996 ) IWAT
  996 FORMAT(A2)
      CALL CLOSENAMFIL()
      RETURN
      END
