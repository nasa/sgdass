      SUBROUTINE OPA_MENU ( IVRB, SOLVE_INIT, OPC_FILE, OPA, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  OPA_MENU  displays the screen menu and asks for user      *
! *   input. User is able to change OPA actions and to set the action    *
! *   code. The current action code is written on disk in the            *
! *   Operational Analysis file successful after parsing user input.     *
! *                                                                      *
! * ________________________ Input parameters: _________________________ *
! *                                                                      *
! *       IVRB ( INTEGER*4 ) -- Verbosity level. 0 means suppress all    *
! *                             information messages except error        *
! *                             messages.                                *
! * SOLVE_INIT ( CHARACTER ) -- Solve user initials.                     *
! *   OPC_FILE ( CHARACTER ) -- Operational analysis control file.       *
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
! *        OPA ( RECORD    ) -- Data structure which keeps settings      *
! *                             of OPA and current information related   *
! *                             to processing this session.              *
! *    IUER ( INTEGER*4, OPT ) -- Universal error handler.               *
! *                           Input: switch IUER=0 -- no error messages  *
! *                                  will be generated even in the case  *
! *                                  of error. IUER=-1 -- in the case of *
! *                                  error the message will be put on    *
! *                                  stdout.                             *
! *                           Output: 0 in the case of successful        *
! *                                   completion and non-zero in the     *
! *                                   case of error.                     *
! *                                                                      *
! *  ### 15-AUG-2000    OPA_MENU   v1.6 (c)  L. Petrov  24-SEP-2007 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE    'solve.i'
      INCLUDE    'opa.i'
      TYPE ( OPA__STRU ) ::  OPA
      CHARACTER  SOLVE_INIT*2, OPC_FILE*(*)
      INTEGER*4  IVRB, IUER
      CHARACTER  STR*79, STRG*79, CH*4, CHR*1
      CHARACTER  GET_VERSION*54, SOLVE_HELP_DIR_STR*128, HELP_FILE*128, LINE_STSACT*3
      LOGICAL*4  LEX
      INTEGER*4  ICH, IX, IY, NN, IP, IX_NEW, IY_NEW, J1, J2, J3, J4, J5, J6, &
     &           J7, J8, IER
      INTEGER*4  M_ITEM, M_QUES, IY_BLN, IX_STS
      EQUIVALENCE  ( ICH, CH )
      PARAMETER  ( M_ITEM = M_OPA )
      PARAMETER  ( M_QUES =  8 )
      PARAMETER  ( IY_BLN = 21 )
      PARAMETER  ( IX_STS = 52 )
      CHARACTER  CH_ITEM(M_ITEM)*50, CH_QUES(M_QUES)*50
      CHARACTER  CH_ITEM_D(M_ITEM)*50, CH_ITEM_I(M_ITEM)*50
      INTEGER*4  KQX_B(M_QUES), KQX_E(M_QUES), KQY(M_QUES), KIY(M_ITEM), &
     &           KIY_D(M_ITEM), KIY_I(M_ITEM)
!
! --- definition of actions
!
      DATA     ( CH_ITEM_D(NN), KIY_D(NN), NN=1,M_ITEM ) &
     &         / &  !N  action name                                  ! row
!
     &           '1. Make superfile                                 ',  5, & !  1
     &           '2. Update global arc-list                         ',  6, & !  2
     &           '3. Update baseline-dependent weights              ',  7, & !  3
     &           '4. Update site-dependent weights                  ',  8, & !  4
     &           '5. Make a standalone solution and keep listings   ',  9, & !  5
     &           '6. Make EOPS solution for IVS submission          ', 10, & !  6
     &           '                                                  ', 11, & !  7
     &           '8. Update EOPK time series                        ', 12, & !  8
     &           '9. Make SNR analysis                              ', 13, & !  9
     &           'A. Insert a solution listing into VDB             ', 14, & ! 10
     &           'B. Submit a pair of databases to IVS              ', 15, & ! 11
     &           'C. Submit EOPS series to IVS                      ', 16, & ! 12
     &           '                                                  ', 17, & ! 13
     &           'E. Submit Sinex listing to IVS                    ', 18  & ! 14
     &         /
!
! --- definition of actions
!
! --- Items names for intensive sessions
!
      DATA     ( CH_ITEM_I(NN), KIY_I(NN), NN=1,M_ITEM ) &
     &         / &  !N  action name                                  ! row
!
     &           '1. Make superfile                                 ',  5, & !  1
     &           '2. Update arc-list for intensives                 ',  6, & !  2
     &           '3. Update baseline-dependent weights              ',  7, & !  3
     &           '                                                  ',  8, & !  4
     &           '5. Make a standalone solution and keep listings   ',  9, & !  5
     &           '6. Make EOPI solution for IVS submission          ', 10, & !  6
     &           '7. Make Multi-EOP solution for IVS submission     ', 11, & !  7
     &           '8. Update EOPK time series                        ', 12, & !  8
     &           '                                                  ', 13, & !  9
     &           'A. Insert a solution listing into VDB             ', 14, & ! 10
     &           'B. Submit a pair of databases to IVS              ', 15, & ! 11
     &           'C. Submit EOPI series to IVS                      ', 16, & ! 12
     &           'D. Submit Multi-EOP series to IVS                 ', 17, & ! 13
     &           'E. Submit Sinex listing to IVS                    ', 18  & ! 14
     &         /
!
! --- Item names for
!
      DATA     ( CH_QUES(NN), KQY(NN), KQX_B(NN), KQX_E(NN), NN=1,M_QUES ) &
     &         / &  !code name                        ! row, [column range]
     &           '? -- Help,                      ', 22,  0,  8, &
     &           '$ -- Inquire status,            ', 22, 11, 29, &
     &           'M -- Modify arc-line,           ', 22, 32, 51, &
     &           'Q -- Quit                       ', 22, 55, 63, &
     &           'W -- Set all,                   ', 23,  0, 11, &
     &           'Z -- Clear all,                 ', 23, 14, 27, &
     &           '@ -- Do all set actions,        ', 23, 30, 52, &
     &           '* -- Execute this action        ', 23, 55, 78 &
     &         /
      INTEGER*4, EXTERNAL :: ILEN, I_LEN, IFIND_PL
!
      OPA%IACT = OPA__UND
      IF ( OPA%SESSION_TYPE(1:7) .EQ. 'DIURNAL' ) THEN
!
! -------- Setting item codes for 24 hours sessions
!
           DO 410 J1=1,M_ITEM
              CH_ITEM(J1) = CH_ITEM_D(J1)
              KIY(J1)     = KIY_D(J1)
              IF ( OPA%VDB_UPDATE_EXE == 'NO'        .AND.  &
     &             INDEX ( CH_ITEM(J1), 'VDB' ) > 0         ) THEN
!
                   CALL CLRCH ( CH_ITEM(J1) )
              END IF
 410       CONTINUE
         ELSE IF ( OPA%SESSION_TYPE(1:9) .EQ. 'INTENSIVE' ) THEN
!
! -------- Setting item codes for intensive sessions
!
           DO 420 J2=1,M_ITEM
              CH_ITEM(J2) = CH_ITEM_I(J2)
              KIY(J2)     = KIY_I(J2)
 420       CONTINUE
      END IF
      CALL START_MN()
      IX_NEW = 0
      IY_NEW = IY_BLN
 910  CONTINUE
!
! --- Make a menu header
!
      CALL CLEAR_MN()
      CALL CLRCH ( STR )
      STR = 'Operational Analysis Utility'
      STR(31:32) = SOLVE_INIT
      STR(35:35) = '$'
      STR(36:44) = OPA%DB_NAME
      STR(46:46) = '<'
      CALL INCH ( OPA%DB_VERSION, STR(47:49) )
      CALL CHASHL ( STR(47:49) )
      STR(ILEN(STR)+1:ILEN(STR)+1) = '>'
!
      STR(52:57) = OPA%SESS_CODE
      CALL ADDSTR_F ( STR(1:I_LEN(STR)) )
!
      STRG = GET_VERSION()
      CALL SETCR_MN ( 79-I_LEN(STRG), 0 )
      CALL REVERSE_ON_MN()
      CALL ADDSTR_F ( STRG(1:I_LEN(STRG)) )
      CALL REVERSE_OFF_MN()
      CALL REFRESH_MN ()
!
! --- Print config file name
!
      CALL SETCR_MN ( 0, 1 )
      CALL ADDSTR_F ( 'Config_file: '// &
     &                 OPA%CONFIG_FILE(1:I_LEN(OPA%CONFIG_FILE)) )
!
! --- ... and print the current arc-line
!
      CALL SETCR_MN ( 0, 2 )
      CALL ADDSTR_F ( 'Arc_line:    '//OPA%ARC_LINE(1:I_LEN(OPA%ARC_LINE)) )
!
! --- Now print a delimiter row
!
      CALL REPEAT   ( '-', 79, STR )
      CALL SETCR_MN ( 0, 3 )
      CALL ADDSTR_F ( STR(1:79) )
!
      DO 430 J3=1,M_ITEM
!
! ------ Print the item name
!
         CALL SETCR_MN ( 0, KIY(J3) )
         CALL ADDSTR_F ( CH_ITEM(J3)(1:I_LEN(CH_ITEM(J3))) )
!
! ------ Print status code and action code
!
         IF ( ILEN(CH_ITEM(J3)) .GT. 0 ) THEN
              CALL SETCR_MN ( IX_STS, KIY(J3) )
              CALL ADDSTR_F ( LINE_STSACT ( OPA%STS(J3), OPA%ACT(J3) ) )
         END IF
 430  CONTINUE
!
! --- Print the delimiter line
!
      CALL SETCR_MN ( 0, IY_BLN )
      CALL ADDSTR_F ( STR(1:79) )
!
! --- Now print a set of solicited actions
!
      DO 440 J4=1,M_QUES
         CALL SETCR_MN ( KQX_B(J4), KQY(J4) )
         CALL ADDSTR_F ( CH_QUES(J4)(1:I_LEN(CH_QUES(J4))) )
 440  CONTINUE
!
      CALL SETCR_MN ( IX_NEW, IY_NEW )
      CALL REFRESH_MN()
!
! --- Wait for user response
!
      CALL SENKR_MN ( IX, IY, ICH )
      CHR = CH(4:4)
      IF ( CHR .EQ. CHAR(13)  .OR.  CHR .EQ. ' ' ) THEN
!
! -------- User enter <blank> or <return>. Try to determine action code using
! -------- cursor coordinates
!
           DO 450 J5=1,M_QUES
              IF ( IY .EQ. KQY(J5) .AND. &
     &             ( IX .GE. KQX_B(J5)  .AND.  IX .LE. KQX_E(J5) ) ) THEN
                   CHR = CH_QUES(J5)(1:1)
              END IF
 450       CONTINUE
        ELSE
!
! -------- Tranform to the letter of upper register
!
           CALL TRAN ( 11, CHR, CHR )
      END IF
!
! --- Remove two bottom lines
!
      CALL SETCR_MN ( 0, IY_BLN+1 )
      CALL CLRCH    ( STR )
      CALL ADDSTR_F ( STR )
      CALL NL_MN()
      CALL ADDSTR_F ( STR )
      CALL SETCR_MN ( 0, IY_BLN+1 )
      CALL REFRESH_MN ()
!
! --- Now try to execute the user action
!
      IF ( CHR .EQ. '?' ) THEN
!
! -------- Help
!
           CALL CLRCH  ( SOLVE_HELP_DIR )
           CALL GETENVAR ( 'PSOLVE_HELP_DIR', SOLVE_HELP_DIR_STR )
           IF ( ILEN(SOLVE_HELP_DIR_STR) .LE. 0 ) THEN
                SOLVE_HELP_DIR_STR = SOLVE_HELP_DIR
           END IF
           IF ( SOLVE_HELP_DIR_STR(I_LEN(SOLVE_HELP_DIR_STR):I_LEN(SOLVE_HELP_DIR_STR)) .NE. '/' ) THEN
                SOLVE_HELP_DIR_STR = SOLVE_HELP_DIR_STR(1:I_LEN(SOLVE_HELP_DIR_STR))//'/'
           END IF
!
           INQUIRE ( FILE=SOLVE_HELP_DIR, EXIST=LEX )
           IF ( .NOT. LEX ) THEN
                CALL ERR_LOG ( 4161, IUER, 'OPA_MENU', 'Wrong value of '// &
     &              'environment variable SOLVE_HELP_DIR (or system-wide default '// &
     &              'SOLVE_HELP_DIR: '//SOLVE_HELP_DIR(1:I_LEN(SOLVE_HELP_DIR))// &
     &              ' -- directory does not exist' )
                RETURN
           END IF
           CALL CLRCH ( HELP_FILE )
           HELP_FILE = SOLVE_HELP_DIR(1:ILEN(SOLVE_HELP_DIR))//HELP02__OPA
!
! -------- Show help file
!
           CALL ERR_PASS ( IUER, IER )
           CALL SHOW_TEXT_FILE_COL ( HELP_FILE, 'OPA', 0, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 4162, IUER, 'OPA_MENU', 'Error '// &
     &              'in attempt to display help file' )
                RETURN
           END IF
         ELSE IF ( CHR .EQ. 'M' ) THEN
!
! -------- New arc-line
!
           CALL ADDSTR_F ( 'New arc-line: ' )
           CALL NL_MN()
           CALL REFRESH_MN()
           CALL GETSTR_F ( OPA%ARC_LINE )
         ELSE IF ( CHR .EQ. '$' ) THEN
!
! -------- Status inquiry
!
           CALL END_MN()
           CALL CURU ( 1 )
           IF ( IVRB .GE. 1 ) WRITE ( 6, '(A)' ) 'Inquiry of status of '// &
     &                        'session '// &
     &                        OPA%SESS_CODE(1:I_LEN(OPA%SESS_CODE))// &
     &                        ' is in progress...'
!
! -------- Inquire status of the session
!
           CALL ERR_PASS   ( IUER, IER )
           CALL OPA_STSINQ ( IVRB, OPA, OPC_FILE, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 4163, IUER, 'OPA_MENU', 'Error occurred '// &
     &              'during attempt to inquire status of session '// &
     &               OPA%SESS_CODE )
                RETURN
           END IF
           CALL START_MN()
           IX_NEW = 0
           IY_NEW = IY_BLN
         ELSE IF ( CHR .EQ. 'W' ) THEN
!
! -------- Set all actions
!
           DO 460 J6=1,M_ITEM
              IF ( OPA%ACT(J6) .NE. 'N' ) OPA%ACT(J6) = '+'
 460       CONTINUE
           IX_NEW = 0
           IY_NEW = IY_BLN
         ELSE IF ( CHR .EQ. 'Z' ) THEN
!
! -------- Unset all actions
!
           DO 470 J7=1,M_ITEM
              IF ( OPA%ACT(J7) .NE. 'N' ) OPA%ACT(J7) = '-'
 470       CONTINUE
         ELSE IF ( CHR .EQ. '@' ) THEN
!
! -------- Execute all actions with action code "+"
!
           OPA%IACT = OPA__ALL
           GOTO 810
         ELSE IF ( CHR .EQ. 'Q' ) THEN
!
! -------- Quitely Quit OPA
!
           GOTO 810
         ELSE IF ( CHR .EQ. ' ' .OR. CHR .EQ. CHAR(13) ) THEN
!
! -------- Check whether user pointed out at the action
!
           IP = IFIND_PL ( M_ITEM, KIY, IY )
           IF ( IP .GT. 0 ) THEN
!
! ------------- Yes. Then set a flag "+" if it is a valid action
!
                IF ( OPA%ACT(IP) .NE. 'N'  .AND. &
     &               OPA%ACT(IP) .NE. '+'        ) THEN
                     OPA%ACT(IP) = '+'
                  ELSE IF ( OPA%ACT(IP) .EQ. '+' ) THEN
                     OPA%ACT(IP) = '-'
                END IF
           END IF
           IX_NEW = IX
           IY_NEW = IY
         ELSE IF ( CHR .EQ. '*' ) THEN
!
! -------- Execute this action. Check whether user pointed out at the action
!
           IP = IFIND_PL ( M_ITEM, KIY, IY )
           IF ( IP .GT. 0 ) THEN
                IF ( OPA%ACT(IP) .NE. 'N' .AND. CH_ITEM(IP)(1:1) .NE. ' ' ) THEN
!
! ------------------ Yes, user pointed out at the valid action.
!
                     OPA%ACT(IP) = '+'
                     OPA%IACT = IP
                     GOTO 810
                END IF
           END IF
         ELSE
!
! -------- Try to interpret the letter as the action code
!
           DO 480 J8=1,M_ITEM
              IF ( CHR .EQ. CH_ITEM(J8)(1:1)  .AND.  &
     &             CH_ITEM(J8)(1:1) .NE. ' '         ) THEN
!
! ---------------- Yes, there is such an action. If it is not forbidden,
! ---------------- let's execute it.
!
                   IF ( OPA%ACT(J8) .NE. 'N' ) THEN
                        OPA%ACT(J8) = '+'
                        OPA%IACT = J8
                        GOTO 810
                   END IF
              END IF
 480       CONTINUE
      END IF
      GOTO 910
 810  CONTINUE
      CALL END_MN ()
      CALL CURU ( 1 )
!
! --- Write OPC file on disk once more
!
      CALL ERR_PASS ( IUER, IER )
      CALL WRITE_OPC ( OPC_FILE, OPA, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 4164, IUER, 'OPA_MENU', 'Error in attempt '// &
     &         'to write the current status of experiment '//OPA%DB_NAME// &
     &         ' in file '//OPC_FILE )
           RETURN
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  OPA_MENU  #!#
