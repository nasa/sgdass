      PROGRAM BASFE_HEAD
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
! 1.  BASFE PROGRAM SPECIFICATION
!
! 1.1
!    ****************************************************************
!    *                                                              *
!    * BASFE IS THE PROGRAM WHICH WILL CALCULATE THE BASELINE       *
!    * COMPONENTS AND FORMAL ERRORS, AND THE BASELINE ERROR         *
!    * ELLIPSOIDS                                                   *
!    *                                                              *
!    ****************************************************************
!
! 1.2 REFERENCES:
!
! 2.  BASFE INTERFACE
!
! 2.1 Parameter File
      INCLUDE 'solve.i'
!
! 2.2 INPUT Variables: None
!
! 2.3 OUTPUT Variables: None
!
! 2.4 COMMON BLOCKS USED
      INCLUDE 'precm.i'
      INCLUDE 'buff4.i'
!
! ARRAYS FOR MONUMENT INFORMATION
      REAL*8 XOFFST(3,MAX_STA)
      INTEGER*2 MOTYPE(MAX_STA),MONAM(5,MAX_STA),LSCRD(3,MAX_STA)
      COMMON/BASEMA/XOFFST,MOTYPE,MONAM,LSCRD
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES: None
!       CALLED SUBROUTINES: reada,bwork
!
! 3.  LOCAL VARIABLES
      LOGICAL*2 KBIT
      INTEGER*2 I
!
      INTEGER*4   IDUM, IPTR, PAGELEN, PAGEWID, IX, IY, IUER
      ADDRESS__TYPE :: ADR_ARR
      INTEGER*4   MBUF
      PARAMETER  ( MBUF = 1024*1024 )
      CHARACTER*4 CDUM, BUFSTR*120, STR*32
      CHARACTER,  ALLOCATABLE :: LBUF(:)*120
      EQUIVALENCE (IDUM,CDUM)
!
      INTEGER*2  LAST_LINE, FIRST_LINE, LMINF, N, ICONT
      INTEGER*2  INT2_ARG
      INTEGER*4  INT4
      INTEGER*4  FC_GWINSZ, FC_GWINW
      INT4(INT2_ARG) = INT(INT2_ARG,KIND=4)
!
! 4.  HISTORY
!   WHO  WHEN    WHAT
!   MWH  940201  Implement dynamic memory allocation for large matrices
!   PET  990404  Changed the structure of the program for support of NO TRAIN
!                mode
!   pet  2003.12.09   Replaced fc_gwinw, fc_gwinsz  calls with GET_TERMSIZE
!   pet  2005.02.04   Made BASFE to call OPTIN adter REPA ends
!   pet  2021.07.16   Made LBUF allocatable 
!
! 5.  BASFE PROGRAM STRUCTURE
!
      CALL PRE_PROG()
!
      INCLUDE 'basfe_version.i' ! Set revision date of the current version
!
      IF ( ( KSCREEN .AND. KBIT( PRE_IP(2), INT2(6))) .OR. .NOT.KBATCH ) THEN
           CALL START_MN
      END IF
      CALL SET_SIGNAL_CTRLC ( 3 )
!
! --- Open and position SPOOL file using info from SOCOM
!
      IF ( KSPOOL ) CALL USE_SPOOL ( 'O' )
!
! --- Open archive scratch file and read record counter
!
      CALL READA_BASFE ( XOFFST, MONAM, MOTYPE, ADR_ARR )
!
! --- Set up LSCRD for CVMML
!
      IF ( NSCNT1 .NE. 0 ) CALL LVECT ( LSCRD, ISCNT, ABS(NSCNT1) )
      ALLOCATE ( LBUF(MBUF), STAT=IUER )
      IF ( IUER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( MBUF*128, STR )
           IUER = -1
           CALL ERR_LOG ( 6101, IUER, 'BASFE_HEAD', 'Failire to allocate '// &
     &          TRIM(STR)//' bytes of dynamic memory for array LBUF' )
           CALL EXIT ( 1 )
      END IF
!
! --- Compute baselines
!
      CALL BWORK    ( LSCRD, XOFFST, MONAM, MOTYPE, %VAL(ADR_ARR), LBUF, IPTR )
      CALL FREE_MEM ( ADR_ARR )
!
      IF ( KSPOOL ) CALL USE_SPOOL ( 'C' )
!
! --- After actions
!
      IF ( KBATCH ) THEN
!
! -------- Nothing in batch mode
!
           CONTINUE
         ELSE
!
! -------- Printing a memu and asking user which program to schedule further
!
           CALL GET_TERMSIZE ( PAGELEN, PAGEWID )
           PAGEWID = PAGEWID - 1
           IF ( PAGELEN .EQ.   0 ) PAGELEN=24
           IF ( PAGEWID .LT.  79 ) PAGEWID=79
           IF ( PAGEWID .GT. 120 ) PAGEWID=120
           LMINF = PAGELEN-6
           ICONT = 0
!
! -------- Dialogue with user
!
           LAST_LINE  = IPTR
           FIRST_LINE = LAST_LINE-LMINF
           IF ( FIRST_LINE .LT. 1 ) FIRST_LINE=1
           CALL SETCR_MN ( 0, 0 )
           CALL CLRCH ( BUFSTR )
           BUFSTR = "  Next: 'P'-Plot, 'O'-OPTIN, 'T'-Terminate"// &
     &              "  '* /' Atm/Clock plots"
           CALL ADDSTR_F ( BUFSTR(:PAGEWID) )
           CALL NL_MN()
           BUFSTR = "  Reset: 'E'-Sites, 'S'-Sources, 'L'-Last page,"// &
     &              "  'B'-Baselines 'X'-Databases"
           CALL ADDSTR_F ( BUFSTR(:PAGEWID) )
           CALL NL_MN()
!
           CALL REVERSE_ON_MN()
           CALL ADDSTR_F ( " '-','+':Line up/down  '<','>':Page up/down  " )
           CALL ADDSTR_F ( "'0'-'8' : Start n/8 of way through" )
           CALL REVERSE_OFF_MN()
!
           CALL NL_MN()
           CALL NL_MN()
           IY=PAGELEN-1
           CALL SETSCRREG_MN ( 4, IY )
           GOTO 810
 910       CONTINUE
           CALL SENKR_MN ( IX, IY, IDUM )
!
           IF ( CDUM(4:4) .EQ. '+' ) THEN
                IF ( LAST_LINE .LT. IPTR ) THEN
                     LAST_LINE=LAST_LINE+1
                     CALL ADDSTR_F ( LBUF(LAST_LINE)(:PAGEWID) )
                     CALL NL_MN()
                ENDIF
                GOTO 910
             ELSE IF ( CDUM(4:4) .EQ. '-' ) THEN
                IF ( FIRST_LINE .GT. 1 ) LAST_LINE=LAST_LINE-1
                GOTO 810
             ELSE IF ( CDUM(4:4) .EQ. '>' ) THEN
                LAST_LINE = LAST_LINE+LMINF
                IF ( LAST_LINE .GT. IPTR ) LAST_LINE=IPTR
                GOTO 810
             ELSE IF ( CDUM(4:4) .EQ. '<' ) THEN
                LAST_LINE=LAST_LINE-LMINF
                IF ( LAST_LINE-LMINF .LT. 1 ) LAST_LINE=LMINF+1
                GOTO 810
             ELSE IF ( CDUM(4:4) .GE. '0'  .AND.  CDUM(4:4) .LE. '8' ) THEN
                READ(CDUM(4:4),'(i1)') N
                LAST_LINE=LMINF+(N*(IPTR-(LMINF+1)))/8+1
                GOTO 810
             ELSE
                GOTO 820
           ENDIF  ! CDUM
 810       CONTINUE
!
           FIRST_LINE=LAST_LINE-LMINF
           CALL SETCR_MN ( 0, 4 )
           CALL CLRTOBOT_MN()
!
           DO I=FIRST_LINE,LAST_LINE
              CALL ADDSTR_F ( LBUF(I)(:PAGEWID) )
              CALL NL_MN()
          ENDDO
!
          GOTO 910
 820      CONTINUE
!
          IF  ( CDUM(4:4) .NE.'O'  .AND. &
     &          CDUM(4:4) .NE.'P'  .AND. &
     &          CDUM(4:4) .NE.'E'  .AND. &
     &          CDUM(4:4) .NE.'S'  .AND. &
     &          CDUM(4:4) .NE.'L'  .AND. &
     &          CDUM(4:4) .NE.'B'  .AND. &
     &          CDUM(4:4) .NE.'T'  .AND. &
     &          CDUM(4:4) .NE.'*'  .AND. &
     &          CDUM(4:4) .NE.'/'  .AND. &
     &          CDUM(4:4) .NE.'X'        )   GOTO 910
!
          CALL END_MN()
          ICONT=0
!
          IF ( CDUM(4:4) .EQ. 'E') THEN
               ICONT=1
             ELSE IF ( CDUM(4:4) .EQ. 'S') THEN
               ICONT=-1
             ELSE IF ( CDUM(4:4) .EQ. 'L') THEN
               ICONT=-2
             ELSE IF ( CDUM(4:4) .EQ. 'B') THEN
               ICONT=-5
             ELSE IF ( CDUM(4:4) .EQ. 'X') THEN
               ICONT=-6
          END IF
!
          IF ( ICONT .NE. 0 ) THEN
               CALL USE_BUFFER ( ICONT, INT2(1), 'OWC' )
               CALL RUN_PROG   ( 'SETFL', 'PASS', INT2(0) )
          ENDIF
!
          IF ( CDUM(4:4) .EQ. '*' ) THEN
               CALL USE_BUFFER ( INT2(0), INT2(1), 'OWC' )
               CALL RUN_PROG ( 'MDLPL', 'WAIT', INT2(0) )
               CALL RUN_PROG ( 'OPTIN', 'PASS', INT2(0) )
            ELSE IF ( CDUM(4:4) .EQ. '/' ) THEN
               CALL USE_BUFFER ( INT2(1), INT2(1), 'OWC' )
               CALL RUN_PROG ( 'MDLPL', 'WAIT', INT2(0) )
               CALL RUN_PROG ( 'OPTIN', 'PASS', INT2(0) )
            ELSE IF ( CDUM(4:4) .EQ. 'O' ) THEN
               CALL RUN_PROG ( 'OPTIN', 'PASS', INT2(0) )
            ELSE IF ( CDUM(4:4) .EQ. 'P' ) THEN
               CALL RUN_PROG ( 'REPA',  'WAIT', INT2(0) )
               CALL RUN_PROG ( 'OPTIN', 'PASS', INT2(0) )
            ELSE IF ( CDUM(4:4) .EQ. 'T') THEN
               CALL RUN_PROG ( 'SLEND', 'PASS', INT2(0) )
          ENDIF
      END IF
      DEALLOCATE ( LBUF )
      CALL END_PROG()
!
      END  !#!  BASFE_HEAD  #!#
