      SUBROUTINE CONPG ( IKONT )
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
! 1.  CONPG PROGRAM SPECIFICATION
!
! 1.1 Build and then use the constraints menu to allow the user
!     to control the application of constraints.
!
! 1.2 REFERENCES:
!
! 2.  CONPG INTERFACE
!
! 2.1 Parameter File
      INCLUDE 'solve.i'
!
! 2.3 OUTPUT Variables: None
      INTEGER*2 IKONT
!
! 2.4 COMMON BLOCKS USED
      INCLUDE 'glbcm.i'
      INCLUDE 'prfil.i'
      INCLUDE 'socom.i'
      INCLUDE 'precm.i'
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES: rmflag
!       CALLED SUBROUTINES: utility routines
!
! 3.  LOCAL VARIABLES
!
      REAL*8      SACNST_OLD, SCCNST_OLD
      CHARACTER   ATM_RATES*3, EOP*3, CLKS*3, NUT*3, GRAD*3
      CHARACTER   BUFSTR*100, STR*54, GET_VERSION*54
      INTEGER*2   I, IDUM, OFFSET, J, N
      INTEGER*4   IX, IY, ICH, IYY, IOS
      CHARACTER   CCH*4, CCHAR*2
      EQUIVALENCE (ICH,CCH)
      LOGICAL*2   KBIT
      INTEGER*4 I4P0,I4P1,I4P5,I4P7,I4P9,I4P21,I4P32,I4P43,I4P69
      DATA  I4P0,I4P1,I4P5,I4P7,I4P9,I4P21,I4P32,I4P43, &
     &     I4P69/   0,   1,   5,   7,   9,   21,   32,  43 ,69/
      LOGICAL*4   CHECK_STABIT, ISTA_WAS
      INTEGER*4   SC_LEN, SC_WID
      INTEGER*2   IPAGE, NPAGE, ISTA_BEG, ISTA_END
      INTEGER*4   I_LEN, ILEN
      INTEGER*2   INT2_ARG
!
! 4.  HISTORY
!   WHO   WHEN       WHAT
!   pet   17-JUL-97  Changed interface. Made it fork at display 80x24. Changed
!                    the order of items.
!   pet   07-AUG-97  Minor bug in setting site-dependent constants fixed
!   pet   03-DEC-97  Added logic for bypassing deselected station
!   pet   08-MAY-98  Changed a bit logic: now changing status from
!                    site-dependent to global doesn't lead to constraint
!                    initialization: values of constraints from the first
!                    station are propagated to all stations
!   pet   20-SEP-98  Added option "M"  -- change sigmas of constraints for
!                    more constraints.
!   pet   1999.12.30 Fixed a bug related with processing more than 16 stations
!                    experiment: the previous version was putting all station
!                    dependent information on one page and it screwed up.
!                    The new versions splits the menu onto two pages if it
!                    is not enough to put all information in one page. Support
!                    of commands N (next page) and P (previous station is added)
!   pet   2000.02.11 Fixed a minor bug: option L was not activated
!   pet   2003.12.09 Replaced fc_gwinw, fc_gwinsz  calls with GET_TERMSIZE
!   pet   2004.03.18 Fixed a bug. The true is that HP Fortran compiler 
!                    refuses recognize a string with binary NULL character 
!                    as a valid input string in format transformations. &
!                    It returns error message number 915
!
! 5.  CONPG PROGRAM STRUCTURE
!
!CCCC
!
! --- Initial settings
!
      EOP  = 'NO'
      NUT  = 'NO'
      GRAD = 'NO'
      IF ( KBIT( CONSTRAINT_BITS, INT2(1) ) ) EOP  = 'YES'
      IF ( KBIT( CONSTRAINT_BITS, INT2(7)) ) NUT  = 'YES'
      IF ( KBIT( CONSTRAINT_BITS, INT2(8)) ) GRAD = 'YES'
!
      ATM_RATES = 'NO'
      IF ( KBIT( CONSTRAINT_BITS, INT2(2) ) ) ATM_RATES = 'YES'
!
      CLKS = 'NO'
      IF ( KBIT( CONSTRAINT_BITS, INT2(3) ) ) CLKS = 'YES'
!
      IX = 9
      IY = 5
!
! --- Learn the page size (number of columns) and decide whether we have
! --- to have more than one page to list all stations
!
      CALL GET_TERMSIZE ( SC_LEN, SC_WID )
      IF ( NUMSTA .GT. SC_LEN-8 ) THEN
           NPAGE = 2  ! Two page mode
           IPAGE = 1  ! The first page is 1
         ELSE
           NPAGE = 1
           IPAGE = 1
      END IF
    1 CONTINUE
      CALL SETCR_MN ( I4P0, I4P0 )
      CALL CLEAR_MN()
!
! --- Write out the menu header
!
      CALL ADDSTR_F ( "Constraints" )
      STR = GET_VERSION()
      CALL SETCR_MN ( 79-I_LEN(STR), 0 )
      CALL REVERSE_ON_MN()
      CALL ADDSTR_F ( STR(1:I_LEN(STR)) )
      CALL REVERSE_OFF_MN()
      CALL NL_MN()
      CALL NL_MN()
      WRITE ( BUFSTR,'( &
     &       "Atmosphere Rates  EOP   Nutation   Clock Rates    ", &
     &       " Gradients")' )
      CALL ADDSTR_F ( BUFSTR(1:79) )
      CALL NL_MN()
      WRITE(BUFSTR,'( &
     &       "    (ps/hr)                      (parts in 1.E14) ", &
     &       "     offset   rate")')
      CALL ADDSTR_F ( BUFSTR(1:79) )
      CALL NL_MN()
!
      IF ( NPAGE .EQ. 2  .AND.  IPAGE .EQ. 2 ) THEN
           CALL ADDSTR_F ( "                                        "// &
     &                     "                        <more stations>"    )
      END IF
      CALL NL_MN()
!
      IF ( SITE_DEP_CONST ) THEN
!
! ------ Find the index of the fist station to be displayed and the last one
!
         IF ( IPAGE .EQ. 1 ) THEN
              ISTA_BEG = 1
              ISTA_END = MIN ( NUMSTA, INT2(SC_LEN-8) )
            ELSE IF ( IPAGE .EQ. 2 ) THEN
              ISTA_BEG = 1 + INT2(SC_LEN-8)
              ISTA_END = MIN ( NUMSTA, INT2(2*(SC_LEN-8)) )
            ELSE IF ( IPAGE .EQ. 3 ) THEN
              WRITE ( 6, * ) ' SC_LEN =',SC_LEN,' NUMSTA=',NUMSTA
              CALL FERR ( INT2(2725), 'SETFL(conpg): too many pages!', &
     &             INT2(0), INT2(0) )
              STOP 'SETFL(conpg)Too many pages!'
         END IF
!
! ------ Display all stations if site-dependent constraints are desired
!
         OFFSET = ISTA_END-1
         ISTA_WAS = .FALSE.
         DO I = ISTA_BEG, ISTA_END
!
! --------- Check: was the I-th station in solution?
!
            IF ( .NOT. CHECK_STABIT ( I ) ) THEN
                 WRITE ( BUFSTR, 1010 ) "--- Deselected from solution ---", &
     &                                  ( ISITN(J,I), J=1,4 )
 1010            FORMAT ( A, 40X, 4A2 )
                 CALL ADDSTR_F ( BUFSTR(1:79) )
               ELSE
                 IF ( .NOT. ISTA_WAS ) THEN
                      WRITE ( BUFSTR, 1011 ) ATM_RATES, SACNST(I), EOP, NUT, &
     &                        CLKS, SCCNST(I), GRAD, GRADCONS(1), &
     &                        GRADCONS(2),( ISITN(J,I), J=1,4 )
                      CALL ADDSTR_F ( BUFSTR(1:79) )
                      ISTA_WAS = .TRUE.
                  ELSE
                      WRITE ( BUFSTR, 1011 ) "   ", SACNST(I), "   ", "   ", &
     &                        "   ", SCCNST(I), "   ", GRADCONS(1), &
     &                        GRADCONS(2),(ISITN(J,I),J=1,4)
 1011                 FORMAT ( A3, 3X, F10.3, 2X, A3, 3X, A3, 7X, A3, 2X, &
     &                         F10.3, 2X, A3, 2X, F5.2, 2X, F5.2, 3X, 4A2 )
                     CALL ADDSTR_F ( BUFSTR(1:79) )
               ENDIF
            END IF
            CALL NL_MN()
         ENDDO
!
         IF ( NPAGE .EQ. 2  .AND.  IPAGE .EQ. 1 ) THEN
              CALL ADDSTR_F ( "                                        "// &
     &                        "                        <more stations>"    )
         END IF
         CALL NL_MN()
       ELSE
!
! ------ Otherwise display a single (global) constraint
!
         OFFSET = 0
         WRITE ( BUFSTR, 1001 ) ATM_RATES, SACNST(1), EOP, NUT, CLKS, &
     &           SCCNST(1), GRAD, GRADCONS(1), GRADCONS(2)
 1001    FORMAT ( A3, 3X, F10.3, 2X, A3, 3X, A3, 7X, A3, 2X, F10.3, 2X, A3, &
     &            2X, F5.2, 2X, F5.2 )
         CALL ADDSTR_F ( BUFSTR(1:79) )
         CALL NL_MN()
         CALL NL_MN()
      ENDIF
!
      IF ( SITE_DEP_CONST ) THEN
           CALL ADDSTR_F ( "(*) Constraint status: Site dependent" )
           IF ( NPAGE .EQ. 1 ) THEN
                CALL ADDSTR_F ( "         " )
              ELSE
                IF ( IPAGE .EQ. 1 ) THEN
                     CALL ADDSTR_F ( "  (N)ext station page" )
                   ELSE
                     CALL ADDSTR_F ( "  (P)revious station page" )
                END IF
           END IF
         ELSE
           CALL ADDSTR_F ( "(*) Constraint status: One value for all sites" )
      ENDIF
      CALL NL_MN()
      CALL ADDSTR_F ( "(O)ptin   (M)ore constraints   (L)ast Page   "// &
     &                "(T)erminate   Least s(Q)uares" )
!
! --- See what the user wants to do
!
      CALL SETCR_MN ( IX, IY )
   2  CONTINUE
      CALL SENKR_MN ( IX, IY, ICH )
      CCHAR(1:1) = CCH(4:4)
      IF ( CCHAR(1:1) .EQ. CHAR(13) ) CCHAR(1:1) = ' '
      IF (CCHAR(1:1).EQ.' ') THEN
         IF ( IY.EQ.OFFSET+7 ) THEN
              IF ( IX .LE. 37 ) CCHAR(1:1) = '*'
              IF ( IX .GE. 39  .AND.  IX .LE. 57  .AND. &
     &             IPAGE .EQ. 1                         ) CCHAR(1:1) = 'N'
              IF ( IX .GE. 39  .AND.  IX .LE. 61  .AND. &
     &             IPAGE .EQ. 2                         ) CCHAR(1:1) = 'P'
           ELSE IF ( IY .EQ. OFFSET+8 ) THEN
              IF ( IX.LE.9                 ) CCHAR(1:1) = 'O'
              IF ( IX.GE.10 .AND. IX.LE.30 ) CCHAR(1:1) = 'M'
              IF ( IX.GE.31 .AND. IX.LE.44 ) CCHAR(1:1) = 'L'
              IF ( IX.GE.45 .AND. IX.LE.58 ) CCHAR(1:1) = 'T'
              IF ( IX.GE.59 .AND. IX.LE.73 ) CCHAR(1:1) = 'Q'
         ENDIF
      ENDIF
!
      IF(CCHAR(1:1).EQ.'R') GO TO 1
!
! --- See if the user wants PROC or OPTIN
!
      IF ( CCHAR(1:1) .EQ. 'Q'  .OR.  CCHAR(1:1).EQ.'O'  .OR. &
     &     CCHAR(1:1) .EQ. 'T' ) THEN
!
           ICLMAX = 0
           CALL PARCN()
           IF ( NPARAM .LE. NRMFL_PARMS ) GO TO 800
           CALL USE_COMMON ( 'OWC' )
           WRITE ( BUFSTR, 700 ) NPARAM, PRE_ILETRS
           CALL ADDSTR_F ( BUFSTR )
           CALL NL_MN()
           WRITE ( BUFSTR, 701 ) NRMFL_PARMS
           CALL ADDSTR_F ( BUFSTR )
           CALL NL_MN()
  700      FORMAT(I5," Parameters have been selected, but your ", &
     &               "normal equations file NRMF", A2 )
  701      FORMAT ( ' was only dimensioned to ', I5 )
           CALL ADDSTR_F ( " Return to continue" )
           CALL SENKR_MN ( IX, IY, IDUM )
           GOTO 1
!
! -------- Number of parameters ok,so write out COMMON and close the file.
!
  800      CONTINUE
           CALL USE_COMMON ( 'OWC' )
!
! -------- Turn on and schedule PROC or OPTIN and terminate
!
           IF ( CCHAR(1:1) .EQ. 'Q' ) THEN
                CALL RUN_PROG ( 'GLOBL', 'PASS', INT2(0) )
             ELSE IF ( CCHAR(1:1) .EQ. 'O' ) THEN
                CALL RUN_PROG ( 'OPTIN', 'PASS', INT2(0) )
             ELSE
                IKONT=-15
                RETURN
           ENDIF
           STOP
        ELSE IF ( CCHAR(1:1) .EQ. 'L') THEN
!
! --- See if it is time to skip out.
!
           CALL USE_COMMON ( 'OWC' )
           IKONT = 0
           RETURN
        ELSE IF ( CCHAR(1:1) .EQ. 'N' ) THEN
!
! -------- The next page
!
           IF ( NPAGE .GT. 1 ) IPAGE = IPAGE+1
           IF ( IPAGE .GT. NPAGE ) IPAGE = 1
           GOTO 1
        ELSE IF ( CCHAR(1:1) .EQ. 'P' ) THEN
!
! -------- The previous page
!
           IF ( NPAGE .GT. 1 ) IPAGE = IPAGE-1
           IF ( IPAGE .LT. 1 ) IPAGE = NPAGE
           GOTO 1
        ELSE IF ( CCHAR(1:1) .EQ. 'M' ) THEN
!
! -------- Set sigmas for other constraints
!
           CALL MORE_CONPG ( )
           IY = OFFSET+8
           IX = 1
           GOTO 1
       ELSE IF ( CCHAR(1:1) .eq. '*'  .OR. &
     &           (IY .EQ. 7+OFFSET  .AND. IX .GT. 26) ) THEN ! flip status
!
! -------- Handle flipping constraint status
!
           IF ( SITE_DEP_CONST ) THEN ! Flip to global constraints
                SITE_DEP_CONST = .FALSE.
!
                SCCNST_OLD = SCCNST(1)
                SACNST_OLD = SACNST(1)
                DO I = 1,NUMSTA
                   SCCNST(I) = SCCNST_OLD
                   SACNST(I) = SACNST_OLD
                ENDDO
             ELSE ! Flip to site dependent constraints
                SITE_DEP_CONST = .TRUE.
                CALL SBIT ( CONSTRAINT_BITS, INT2(2), INT2(1) )
                CALL SBIT ( CONSTRAINT_BITS, INT2(3), INT2(1) )
                ATM_RATES = 'YES'
                CLKS      = 'YES'
           ENDIF
           GOTO 1
        ELSE IF ( IY.EQ.5 .AND. IX.GE.0 .AND. IX.LE.2 ) THEN
!
! ------- Setting general constraints on atmosphere
!
           CALL SWBIT ( CONSTRAINT_BITS, INT2(2) )
           IF ( ATM_RATES .EQ. 'NO' ) THEN
                ATM_RATES='YES'
             ELSE
               ATM_RATES='NO'
           ENDIF
           CALL SETCR_MN ( 0, 5 )
           WRITE ( BUFSTR, "(A)" ) ATM_RATES
           CALL ADDSTR_F ( BUFSTR(:3) )
           CALL SETCR_MN ( IX, IY )
        ELSE IF ( .NOT. SITE_DEP_CONST .AND. IY.EQ.5 .AND. &
     &            IX.GE.3 .AND. IX.LE.13 ) THEN
!
! -------- Setting site-independent atmopsphere constraints
!
  120     CONTINUE
          IYY=OFFSET+6
          CALL SETCR_MN ( I4P0, IYY )
          CALL ADDSTR_F ( "Input constraint on atmosphere rate (ps/hr)  ?" )
          CALL CLRCH ( BUFSTR )
          CALL GETSTR_F ( BUFSTR )
          IF ( BUFSTR(1:1) .EQ. ' ' ) GOTO 1
          IF ( BUFSTR(1:1) .EQ. CHAR(0) ) GOTO 1
          READ ( BUFSTR(1:I_LEN(BUFSTR)), FMT=*, ERR=120 ) SACNST(1)
          DO I=2,NUMSTA
             SACNST(I)=SACNST(1)
          ENDDO
          GOTO 1
        ELSE IF ( SITE_DEP_CONST .AND. ( IY.GE.5 .AND. IY.LT.(5+ISTA_END)) &
     &            .AND. IX.GE.3 .AND. IX.LE.15 ) THEN
!
! ------- Setting site-dependent atmopsphere constraints
!
          IF ( IPAGE .EQ. 1 ) THEN
               N=IY-4
             ELSE IF ( IPAGE .EQ. 2 ) THEN
               N=IY-4 + SC_LEN-8
          END IF
  122     CONTINUE
          IYY=OFFSET+6
          CALL SETCR_MN ( 0, IYY )
          CALL ADDSTR_F ( "Input constraint on atmosphere rate (ps/hr)  ?" )
          CALL CLRCH ( BUFSTR )
          CALL GETSTR_F ( BUFSTR )
          IF ( BUFSTR(1:1) .EQ. ' '     ) GOTO 1
          IF ( BUFSTR(1:1) .EQ. CHAR(0) ) GOTO 1
          READ ( BUFSTR(1:I_LEN(BUFSTR)), FMT=*, ERR=122 ) SACNST(N)
          GOTO 1
        ELSE IF ( IY.EQ.5 .AND. IX.GE.18 .AND. IX.LE.20 ) THEN
!
! ------- Setting EOP constraints
!
          CALL SWBIT ( CONSTRAINT_BITS, INT2(1) )
          IF ( EOP .EQ. 'NO' ) THEN
             EOP='YES'
           ELSE
             EOP='NO'
          ENDIF
          CALL SETCR_MN ( 18, 5 )
          WRITE ( BUFSTR, "(A)" ) EOP
          CALL ADDSTR_F ( BUFSTR(:3) )
          CALL SETCR_MN ( IX, IY )
        ELSE IF ( IY.EQ.5 .AND. IX.GE.24 .AND. IX.LE.31 ) THEN
!
! ------- Setting nutation constraints
!
          CALL SWBIT ( CONSTRAINT_BITS, INT2(7) )
          IF ( NUT .EQ. 'NO' ) THEN
               NUT='YES'
            ELSE
               NUT='NO'
          ENDIF
          CALL SETCR_MN ( 24, 5 )
          WRITE ( BUFSTR, "(A)" ) NUT
          CALL ADDSTR_F ( BUFSTR(:3) )
          CALL SETCR_MN ( IX, IY )
        ELSE IF ( IY.EQ.5 .AND. IX.GE.34 .AND. IX.LE.36 ) THEN
!
! ------- Setting general constgraints on clocks
!
          CALL SWBIT ( CONSTRAINT_BITS, INT2(3) )
          IF ( CLKS .EQ. 'NO' ) THEN
               CLKS = 'YES'
            ELSE
               CLKS = 'NO '
          ENDIF
          CALL SETCR_MN ( 34, 5 )
          WRITE ( BUFSTR, "(A)" ) CLKS
          CALL ADDSTR_F ( BUFSTR(:3) )
          CALL SETCR_MN ( IX, IY )
        ELSE IF ( .NOT. SITE_DEP_CONST .AND. IY.EQ.5 .AND. &
     &             IX.GE.37 .AND. IX.LE.48                  ) THEN
!
! ------- Setting Genereal clock constrints
!
  121     CONTINUE
          IYY = OFFSET + 6
          CALL SETCR_MN ( 0, IYY )
          CALL ADDSTR_F ( "Input constraint on clock rate (parts in 1.E14) ?" )
          CALL CLRCH ( BUFSTR )
          CALL GETSTR_F ( BUFSTR )
          IF ( BUFSTR(1:1) .EQ. ' ' ) GOTO 1
          IF ( BUFSTR(1:1) .EQ. CHAR(0) ) GOTO 1
          IF ( INDEX( BUFSTR, '.' ) .EQ. 0 ) THEN
               BUFSTR = BUFSTR(1:I_LEN(BUFSTR))//'.0'
          END IF
          READ ( UNIT=BUFSTR(1:I_LEN(BUFSTR)), FMT='(F8.4)', IOSTAT=IOS ) SCCNST(1)
          DO I=1,NUMSTA
             SCCNST(I)=SCCNST(1)
          ENDDO
          GOTO 1
        ELSE IF ( SITE_DEP_CONST .AND. ( IY.GE.5 .AND. IY.LT.(5+ISTA_END)) &
     &            .AND.  IX .GE. 37  .AND.  IX .LE. 48   ) THEN
!
! ------- Setting site-dependant clock constrints
!
          IF ( IPAGE .EQ. 1 ) THEN
               N=IY-4
             ELSE IF ( IPAGE .EQ. 2 ) THEN
               N=IY-4 + SC_LEN-8
          END IF
  123     CONTINUE
          IYY = OFFSET + 6
          CALL SETCR_MN ( 0, IYY )
          CALL ADDSTR_F ( "Input constraint on clock rate (parts in 1.E14) ?" )
          CALL CLRCH ( BUFSTR )
          CALL GETSTR_F ( BUFSTR )
          IF ( BUFSTR(1:1) .EQ. ' '     ) GOTO 1
          IF ( BUFSTR(1:1) .EQ. CHAR(0) ) GOTO 1
          READ ( BUFSTR(1:I_LEN(BUFSTR)), FMT=*, ERR=123 ) SCCNST(N)
          GOTO 1
        ELSE IF ( IY.EQ.5  .AND. IX.GE.51 .AND. IX.LE.53 ) THEN
!
! ------- Set general gradient flag
!
          CALL SWBIT ( CONSTRAINT_BITS, INT2(8) )
          IF ( GRAD .EQ. 'NO' ) THEN
               GRAD='YES'
             ELSE
               GRAD='NO'
          ENDIF
          CALL SETCR_MN ( 51, 5 )
          WRITE ( BUFSTR, "(A)" ) GRAD
          CALL ADDSTR_F ( BUFSTR(:3) )
          CALL SETCR_MN ( IX, IY )
        ELSE IF ( IY.GE.5  .AND.           &
     &            IX.GE.54 .AND. IX.LE.60  ) THEN
!
! ------- Setting general constraints on atmosphere gradient offset
!
  124     CONTINUE
          IYY = OFFSET + 6
          CALL SETCR_MN ( 0, IYY )
          CALL ADDSTR_F ( "Input constraint on atmosphere gradient offset: ?" )
          CALL CLRCH ( BUFSTR )
          CALL GETSTR_F ( BUFSTR )
          IF ( BUFSTR(1:1) .EQ. ' '     ) GOTO 1
          IF ( BUFSTR(1:1) .EQ. CHAR(0) ) GOTO 1
          READ ( BUFSTR(1:I_LEN(BUFSTR)), FMT=*, ERR=124 ) GRADCONS(1)
          GOTO 1
        ELSE IF ( IY.GE.5  .AND.           &
     &            IX.GE.61 .AND. IX.LE.67  ) THEN
!
! ------- Setting general constraints on atmosphere gradient rate
!
  125     CONTINUE
          IYY = OFFSET + 6
          CALL SETCR_MN ( 0, IYY )
          CALL ADDSTR_F ( "Input constraint on atmosphere gradient rate "// &
     &                    "(mm/d): ?" )
          CALL CLRCH ( BUFSTR )
          CALL GETSTR_F ( BUFSTR )
          IF ( BUFSTR(1:1) .EQ. ' '     ) GOTO 1
          IF ( BUFSTR(1:1) .EQ. CHAR(0) ) GOTO 1
          READ ( BUFSTR(1:I_LEN(BUFSTR)), FMT=*, ERR=125 ) GRADCONS(2)
          GOTO 1
      ENDIF
      GOTO 2
!
      END  !#!  CONPG  #!#
