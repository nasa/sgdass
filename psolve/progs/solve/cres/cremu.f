      SUBROUTINE CREMU ( IRESTYP )
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
      INCLUDE 'solve.i'
      INCLUDE 'help.i'
      INCLUDE 'glbc4.i'
      INCLUDE 'glbcm.i'
      INCLUDE 'socom.i'
!
! 1.  CREMU PROGRAM SPECIFICATION
!
! 1.1 Write out a special options menu for CRES and CNPLT when the
!     appropriate selection is made in OPTIN.
!
! 1.2 REFERENCES:
!
! 2.  CREMU INTERFACE
!
! 2.1 Parameter File
!
! 2.2 INPUT Variables: None
!
! 2.3 OUTPUT Variables:
!
      INTEGER*2 IRESTYP
!
! IRESTYP - User-selected method for residuals computation:
!           0 = conventional; 1 = X-phase minus S-phase;
!           2 = X-phase minus S-group
!
! 2.4 COMMON BLOCKS USED
!
! 2.5 SUBROUTINE INTERFACE
!
!     CALLING SUBROUTINES: secnd
!       CALLED SUBROUTINES: utility routines
!
! 3.  LOCAL VARIABLES
!
      INTEGER*4     IX, IY, IM
      CHARACTER     CC4*4, SIM*1, FINAM*255, STR*80
      CHARACTER     GET_VERSION*54
      INTEGER*2     IRESTYP_MIN, IRESTYP_MAX
      PARAMETER   ( IRESTYP_MIN = 0 )
      PARAMETER   ( IRESTYP_MAX = 2 )
      INTEGER*2     NUMDB, LDBNAM(5,15), IDBV(15), J1
      INTEGER*4     IDBE(15), IL
      CHARACTER     CDBNAM(15)*10
      EQUIVALENCE ( CDBNAM, LDBNAM(1,1) )
      LOGICAL*4     EST_DONE
      LOGICAL*2,    EXTERNAL :: KBIT
      INTEGER*4,    EXTERNAL :: I_LEN, ILEN, MAKE_HELP_FINAM
!
! 4.  HISTORY
!     95.07.20  jwr  Phase delay flaging logic fixed.
!     98.05.05  pet  Rewrote anew
!     98.05.06  pet  Added check: was the database analyzed. If not, then
!                    estimation procedure will be called
!     1999.10.11 pet Got rid from the argument DATE
!     2003.08.12 pet Made changes realted to using CRES_STYLE variable
!                    instead of TITLE_ANON, TITLE_PRE98, CRES_PRE98
!     2005.01.27 pet Added support of changing variable FRINGE_ROOT_DIR
!
! 5.  CREMU PROGRAM STRUCTURE
!
!     Set up some default values.
!
      IRESTYP = 0
!
! --- Write the program title, menu page info, etc. at the top
! --- of the user terminal screen.
!
 910  CONTINUE
!
      CALL SETCR_MN ( 0, 0 )
      CALL CLEAR_MN()
      CALL ADDSTR_F ( "Special options menu" )
      STR = GET_VERSION()
      CALL SETCR_MN ( 79-I_LEN(STR), 0 )
      CALL REVERSE_ON_MN()
      CALL ADDSTR_F ( STR(1:I_LEN(STR)) )
      CALL REVERSE_OFF_MN()
      CALL ADDSTR_F ( "~~~~~~~~~~~~~~~~~~~~~" )
!
! --- Write out menu of user options.
!     ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      CALL SETCR_MN ( 0, 2 )
      CALL ADDSTR_F ( "                    CRES options: " )
      CALL SETCR_MN ( 0, 3 )
      CALL ADDSTR_F ( "                    ~~~~~~~~~~~~"  )
!
      CALL SETCR_MN ( 0, 4 )
      CALL ADDSTR_F ( "(C) CRES Compatibility with " )
      CALL REVERSE_ON_MN()
      IF ( CRES_STYLE == CRES__CURRENT ) THEN
           CALL ADDSTR_F ( "current" )
         ELSE IF ( CRES_STYLE == CRES__PRE03 ) THEN
           CALL ADDSTR_F ( "pre03" )
         ELSE IF ( CRES_STYLE == CRES__PRE98 ) THEN
           CALL ADDSTR_F ( "pre98" )
      END IF
      CALL REVERSE_OFF_MN()
!
! --- Residuals computations
!
      CALL SETCR_MN ( 0, 6 )
      CALL ADDSTR_F ( "(R) Residual computation: " )
      CALL REVERSE_ON_MN()
      IF ( IRESTYP .EQ. 0 ) THEN
           CALL ADDSTR_F ( "conventional method" )
         ELSE IF ( IRESTYP .EQ. 1 ) THEN
           CALL ADDSTR_F ( "X-phase minus S-phase (S-phase minus X-phase)" )
         ELSE IF ( IRESTYP .EQ. 2 ) THEN
           CALL ADDSTR_F ( "X-phase minus X-group (S-phase minus S-group)" )
      END IF
      CALL REVERSE_OFF_MN()
      CALL NL_MN()
!
      CALL SETCR_MN ( 0,  9 )
      CALL ADDSTR_F ( "                    REPA options: " )
      CALL SETCR_MN ( 0, 10 )
      CALL ADDSTR_F ( "                    ------------- "  )
!
! --- Sigmas
!
      CALL SETCR_MN ( 0, 12 )
      CALL ADDSTR_F ( "(S) CNPLT sigmas: " )
      CALL REVERSE_ON_MN()
      IF ( SIGMA_TYPE .EQ. 'PR' ) THEN
           CALL ADDSTR_F ( "pre-fit" )
        ELSE
           CALL ADDSTR_F ( "post-fit" )
      END IF
      CALL REVERSE_OFF_MN()
!
! --- Suppression
!
      CALL SETCR_MN ( 0, 14 )
      CALL ADDSTR_F ( "(\) Suppression action scheme: " )
      CALL REVERSE_ON_MN()
      IF ( CNPLT_SUPR_PRE98 ) THEN
           CALL ADDSTR_F ( "PRE98" )
        ELSE
           CALL ADDSTR_F ( "POST98" )
      END IF
      CALL REVERSE_OFF_MN()
!
! --- Unrecoverable observations
!
      CALL SETCR_MN ( 0, 16 )
      CALL ADDSTR_F ( "(U) To show unrecoverable observations: " )
      CALL REVERSE_ON_MN()
      IF ( CNPLT_SHOW_UNRC ) THEN
           CALL ADDSTR_F ( "yes" )
        ELSE
           CALL ADDSTR_F ( "no" )
      END IF
      CALL REVERSE_OFF_MN()
!
! --- Bad observations
!
      CALL SETCR_MN ( 0, 18 )
      CALL ADDSTR_F ( "(B) To show conditionally bad observations: " )
      CALL REVERSE_ON_MN()
      IF ( CNPLT_SHOW_CBAD ) THEN
           CALL ADDSTR_F ( "yes" )
        ELSE
           CALL ADDSTR_F ( "no" )
      END IF
      CALL REVERSE_OFF_MN()
!
! --- Cross band option
!
      CALL SETCR_MN ( 0, 19 )
      CALL ADDSTR_F ( "(-) makes phase delay residuals for band " )
      CALL REVERSE_ON_MN()
      IF ( CNPLT_PHASE_S ) THEN
           CALL ADDSTR_F ( "S" )
        ELSE
           CALL ADDSTR_F ( "X" )
      END IF
      CALL REVERSE_OFF_MN()
!
! --- Cross band option
!
      CALL SETCR_MN ( 0, 20 )
      CALL ADDSTR_F ( "(F) Fringe root_dir: " )
      IF ( ILEN(FRINGE_ROOT_DIR) .GT. 0 ) THEN
           CALL REVERSE_ON_MN()
           CALL ADDSTR_F ( FRINGE_ROOT_DIR(1:ILEN(FRINGE_ROOT_DIR)) )
           CALL REVERSE_OFF_MN()
         ELSE 
           CALL ADDSTR_F ( 'not defined' )
      END IF
!
! --- Bottom line
!
      CALL SETCR_MN ( 0, 21 )
      CALL ADDSTR_F ( "----------------------------------------"// &
     &                "---------------------------------------"    )
      CALL SETCR_MN ( 0, 22 )
      CALL ADDSTR_F ( "(O) Go back to OPTIN                  "// &
     &                "(>) Proceed with CRES" )
      CALL SETCR_MN ( 0, 23 )
      CALL ADDSTR_F ( "(H) On-line help                      "// &
     &                "(Q) Least square solution" )
      CALL REFRESH_MN()
!
! --- Awaiting for user response
!
      CALL SETCR_MN ( 1, 22 )
      CALL SENKR_MN ( IX, IY, CC4 )
      SIM = CC4(4:4)
      IF ( SIM .EQ. ' ' .OR. SIM .EQ. CHAR(13) ) THEN
           IF ( IY .EQ.  5  .AND.  IX .LT. 43 ) SIM='H'
           IF ( IY .EQ.  4  .AND.  IX .LT. 43 ) SIM='C'
           IF ( IY .EQ.  6  .AND.  IX .LT. 43 ) SIM='R'
           IF ( IY .EQ. 12  .AND.  IX .LT. 43 ) SIM='S'
           IF ( IY .EQ. 14  .AND.  IX .LT. 43 ) SIM='\'
           IF ( IY .EQ. 16  .AND.  IX .LT. 43 ) SIM='U'
           IF ( IY .EQ. 18  .AND.  IX .LT. 43 ) SIM='B'
           IF ( IY .EQ. 19  .AND.  IX .LT. 43 ) SIM='-'
           IF ( IY .EQ. 20  .AND.  IX .LT. 43 ) SIM='F'
           IF ( IY .EQ. 22  .AND.  IX .LE. 38 ) SIM='O'
           IF ( IY .EQ. 22  .AND.  IX .GT. 38 ) SIM='>'
           IF ( IY .EQ. 23  .AND.  IX .LE. 38 ) SIM='H'
           IF ( IY .EQ. 23  .AND.  IX .GT. 38 ) SIM='Q'
      END IF
!
      IF ( SIM .EQ. 'H' ) THEN
!
! -------- Stopping curses
!
           CALL END_MN()
!
! -------- And elimination of the influence of curses
!
           CALL UN_CURSES() ! Elimination of the influence of curses
           CALL CLEAR ( 0, 0 )
!
! -------- Displaying 1-st menu item
!
           IM = MAKE_HELP_FINAM ( CRES_HELP, FINAM )
           IF ( IM .NE. 0 ) THEN
                CALL CLRCH ( STR )
                STR = CRES_HELP
                CALL ERR_LOG ( 6783, -1, 'CREMU', 'Help file '// &
     &               STR(1:I_LEN(STR))//' is not found. Check directory '//SOLVE_HELP_DIR// &
     &              ' and environment variable SOLVE_HELP_DIR' )
                CALL HIT_CONT ( '--- Hit any key to proceed --- '//CHAR(1), &
     &                          %VAL(0) )
                CALL START_MN()
                GOTO 910
           END IF
!
! -------- Show file FINAM
!
           CALL SHOW_TEXT_FILE_COL ( FINAM, 'Description of CRES menu options', &
     &                               1, -3 )
!
! -------- ... and then start again curses
!
           CALL START_MN()
        ELSE IF ( SIM .EQ. 'C' ) THEN
           IF ( CRES_STYLE == CRES__CURRENT ) THEN
                CRES_STYLE = CRES__PRE03
              ELSE IF ( CRES_STYLE == CRES__PRE03 ) THEN
                CRES_STYLE = CRES__PRE98
              ELSE IF ( CRES_STYLE == CRES__PRE98 ) THEN
                CRES_STYLE = CRES__CURRENT
           END IF
        ELSE IF ( SIM .EQ. 'R' ) THEN
           IRESTYP = IRESTYP + 1
           IF ( IRESTYP .GT. IRESTYP_MAX ) IRESTYP = IRESTYP_MIN
        ELSE IF ( SIM .EQ. 'S' ) THEN
           IF ( SIGMA_TYPE .EQ. 'PR' ) THEN
                SIGMA_TYPE = 'PS'
             ELSE
                SIGMA_TYPE = 'PR'
           ENDIF
        ELSE IF ( SIM .EQ. '\' ) THEN
           CNPLT_SUPR_PRE98 = .NOT. CNPLT_SUPR_PRE98
        ELSE IF ( SIM .EQ. 'U' ) THEN
           CNPLT_SHOW_UNRC = .NOT. CNPLT_SHOW_UNRC
        ELSE IF ( SIM .EQ. 'B' ) THEN
           CNPLT_SHOW_CBAD = .NOT. CNPLT_SHOW_CBAD
        ELSE IF ( SIM .EQ. '-' ) THEN
           CNPLT_PHASE_S = .NOT. CNPLT_PHASE_S
        ELSE IF ( SIM .EQ. 'O' ) THEN
!
! ------- Scheduling OPTIN
!
          CALL END_MN()
          CALL USE_GLBFIL   ( 'W'  )  ! Write GLBFIL
          CALL USE_GLBFIL_4 ( 'WC' )  ! ... and GLBFIL_4
          CALL RUN_PROG ( 'OPTIN', 'PASS', INT2(0) )
        ELSE IF ( SIM .EQ. '>' ) THEN
!
! -------- Go further
!
           GOTO 810
        ELSE IF ( SIM .EQ. 'Q' ) THEN
!
! -------- Make estimation
!
           CALL RUN_PROG ( 'GLOBL', 'PASS', INT2(0) )
        ELSE IF ( SIM .EQ. 'F' ) THEN
           CALL SETCR_MN ( 0, 22 )
           CALL ADDSTR_F ( "                                       "// &
     &                     "                                       "   )
           CALL SETCR_MN ( 0, 23 )
           CALL ADDSTR_F ( "                                       "// &
     &                     "                                       "   )
           CALL SETCR_MN ( 0, 22 )
           CALL ADDSTR_F ( "Enter fringe root_dir: " )
           CALL GETSTR_F ( FRINGE_ROOT_DIR )
           IF ( ILEN(FRINGE_ROOT_DIR) .GT. 0 ) THEN
                IL = ILEN(FRINGE_ROOT_DIR) 
                IF ( FRINGE_ROOT_DIR(IL:IL) .NE. '/' ) THEN
                     FRINGE_ROOT_DIR(IL+1:IL+1) = '/'
                END IF
           END IF
      END IF
      GOTO 910
 810  CONTINUE
!CCCCC
!
! --- Clear off screen before returning to normal CRES operations.
!
      CALL SETCR_MN ( 0, 0 )
      CALL CLEAR_MN()
!
! --- Now we should decide what is to do further.
! --- If all databases marked "make solution" has been already analyzed then
! --- then we execute CRES further. Otherwise we make estimation
!
      CALL USE_COMMON ( 'OWC' )
      CALL DBPOX ( NUMDB, LDBNAM, IDBV, IDBE )
      EST_DONE = .TRUE.
!
! --- Scan all loaded databases
!
      DO 410 J1=1,NUMDB
         IF ( KBIT(IDBSEL,J1)  .AND.  .NOT. KBIT(IDBEST,J1) ) THEN
!
! ----------- J1-th database has not been estimated
!
              EST_DONE = .FALSE.
         END IF
 410  CONTINUE
!
      IF ( .NOT. EST_DONE ) CALL RUN_PROG ( 'GLOBL', 'PASS', INT2(0) )
!
! --- That's all.
!
      RETURN
      END  !#!  CREMU  #!#
