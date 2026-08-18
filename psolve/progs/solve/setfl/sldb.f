      SUBROUTINE SLDB ( IKONT, CHR )
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
      INCLUDE 'oborg.i'
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
     &          IXORS(15), IONBUF(12), NYLABL(2,2), LCF(2) , IWAT, JERR, &
     &          IOFF, IWF, N, K, NYFLAG, IFLAG, IDB, KBITN, ICF, J, &
     &          ILOG,  ITBIT, MTBIT, NSEC,  I
      INTEGER*4 IOS, IFRST, ILONG, IX, IY, ICH, NUMDB4, N4, &
     &          N_BND, J1, J2, J3
      REAL*8    FREQ, REFFREQ_MEAN, REFFREQ_S_MEAN
      INTEGER*4 INC_COL, RES_COL !C OLUMNS FOR OUTPUT STATUS (YES,NO)
      DATA inc_col /41/
      DATA res_col /54/
      CHARACTER CCH*4, CCHAR*2, BUFSTR*79 
      EQUIVALENCE (ICH,CCH)
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
!        CCHAR        =  User-input control character
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
!   pet  2025.04.29  Rewrote
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
 990  CONTINUE 
      CALL CLOSENAMFIL()
!
! --- If call is to store new data base info. in common, zero unused flag bits.
!
      IF ( KBIT( IDBSEL, INT2(16)) )THEN  ! zero unused flag bits
           MTBIT = NUMDB + 1
           DO ITBIT = MTBIT,15
              CALL SBIT( IDBSEL, ITBIT, INT2(0) )
           END DO
!
! ------- If call is for data base selection option, then:
!
      ELSE  !select specific data bases for solution
        IY = 0
        CCHAR = '  '
!
! ----- Display data base table initially, or repeat display if requested
!
! ----- Initialize warning flag
!
        N_BND = 0
        DO 410 J1=1,MAX_BND
           IF ( .NOT. ( BAND_NAM(J1) == ' ' .OR. BAND_NAM(J1) == CHAR(0) ) ) THEN
                 N_BND = N_BND + 1
           END IF
 410    CONTINUE 
!
        CALL ACS_OBSFIL ( 'O' )
        DO 420 J2=1,NUMOBS
           CALL USE_OBSFIL ( IOBSFIL, J2, 'R' )
           IF ( N_BND == 1 ) THEN
                IF ( FREQ_SKY > MIN__FRQ .AND. FREQ_SKY < MAX__FRQ ) THEN
                     REFFREQ_MEAN = FREQ_SKY
                END IF
              ELSE
                IF ( REFFREQ > MIN__FRQ .AND. REFFREQ < MAX__FRQ ) THEN
                     REFFREQ_MEAN = FREQ_SKY ! REFFREQ 
                END IF
                IF ( REFFREQ_S > MIN__FRQ .AND. REFFREQ_S < MAX__FRQ ) THEN
                     REFFREQ_S_MEAN = REFFREQ_S
                END IF
           END IF
 420    CONTINUE 
        CALL ACS_OBSFIL ( 'C' )
!
        IWF = 0
        DO WHILE ( CCHAR(1:1).NE.'L' .AND. CCHAR(1:1).NE.'Q' .AND. &
     &             CCHAR(1:1).NE.'O' .AND. CCHAR(1:1).NE.'B' .AND. &
     &             CCHAR(1:1).NE.'E' .AND. CCHAR(1:1).NE.'S'       )
!
! ------- Display data base table
!
          CALL SETCR_MN( 0, 0 )
          CALL CLEAR_MN()
          CALL ADDSTR_F ( "Information about the database loaded in OBSFIL" )
          CALL NL_MN()
          CALL NL_MN()
          CALL ADDSTR_F ( "Name       Vers   Exp_code  # bnd  #sca #sta #src   # obs  " )
          CALL NL_MN()
          CALL NL_MN()
!
          IFRST = 0
          ILONG = IDBEND(1) - IFRST
          WRITE ( BUFSTR, 60 ) (LDBNAM(I,J),I=1,5), IDBVER(J), EXP_CODE, N_BND, &
     &                         NUMSCA, NUMSTA, NUMSTR, NUMOBS
  60      FORMAT ( 5A2, I5, 3X, A8, 6X, I1, 1X, I5, 3X, I2, 1X, I4, 1X, I7 )
          CALL ADDSTR_F(BUFSTR )
          CALL NL_MN()
          CALL NL_MN()
          DO 430 J3=1,N_BND
             IF ( J3 == 1 ) THEN
                  FREQ = REFFREQ_MEAN
               ELSE IF ( J3 == 2 ) THEN
                  FREQ = REFFREQ_S_MEAN
               ELSE
                  FREQ = 0.0D0
             END IF
             WRITE ( BUFSTR, 110 ) BAND_NAM(J3), FREQ
 110         FORMAT ( 'Band  ', A, 4X, 'Freq:  ', F9.2, ' MHz' ) 
             CALL ADDSTR_F(BUFSTR )
             CALL NL_MN()
 430      CONTINUE 
          CALL SETCR_MN( 0, 16 )
          WRITE(BUFSTR,101) 
  101     FORMAT ( '  (L)ast Page  least-s(Q)uares  (O)PTIN', &
     &           '  (E)sites  (S)sources  (B)aselines')
          CALL ADDSTR_F( BUFSTR )
          CALL NL_MN ()
!
          IY = 16
          CALL SETCR_MN ( 0, IY )
          CALL SENKR_MN ( IX, IY, ICH )
          CCHAR(1:1) = CCH(4:4)
          IF ( ICHAR(CCHAR(1:1)) == 13 ) CCHAR(1:1) = 'L'
          CHR = CCHAR(1:1)
        END DO
      ENDIF
!!
      CALL CLOSENAMFIL()
      IF ( CCHAR(1:1) .EQ. 'Q' ) THEN
           IKONT =  -3
        ELSE IF ( CCHAR(1:1) .EQ. 'O' ) THEN
           IKONT =  -7
        ELSE IF ( CCHAR(1:1) .EQ. 'B' ) THEN
           IKONT = -15
        ELSE IF ( CCHAR(1:1) .EQ. 'E' ) THEN
           IKONT = -14
           CHR = 'I'
        ELSE IF ( CCHAR(1:1) .EQ. 'S' ) THEN
           IKONT = -14
           CHR = 'S'
      END IF
!
      CALL SETCR_MN ( 0, 0 )
      CALL CLEAR_MN ()
      RETURN
      END  SUBROUTINE  SLDB  !#!#
