      SUBROUTINE OBCOR ( OBCAVL, OBCAPL, QDOBCAL, NOBCAL, COMMAND, LDBNAM, &
     &                   NNVER )
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
      INCLUDE  'precm.i'
!     Updated to specificaly type integers which
!-------------------------------------------------
!
! 1.  OBCOR PROGRAM SPECIFICATION
!
! 1.1
!     This subroutine enables the user to select which observation
!     dependent calibrations will be applied to a data base.  This
!     data will be stored in bit array OBCAPL.
!
!
! 1.2 REFERENCES:
!
! 2.  OBCOR INTERFACE
!
! 2.1 Parameter File
!
! 2.2 INPUT Variables:
!
      INTEGER*2 LDBNAM(5)
      INTEGER*2 OBCAVL, OBCAPL
      CHARACTER*8 QDOBCAL(*)
      INTEGER*2 NNVER,NOBCAL
      INTEGER*4 I4P0
!
!            Ldbnam(5)     Integer           Name of data base being
!                                            processed.
!            Nnver         Integer           Version of data base
!                                            being processed.
!            Nobcal        Integer           OBCOR can handle up to
!                                            15 calibrations.  Number
!                                            OBCOR will actually deal
!                                            with.
!            Obcavl        Integer           Stores which observation
!                                            dependent calibrations
!                                            are available for a given
!                                            data base.
!            Obcapl        Integer           Stores which observation
!                                            dependent calibrations
!                                            were originally applied
!                                            to the data base.
!            Qdobcal       Character*8 (15)  Calibration names.
!
! 2.3 OUTPUT Variables:
!
      CHARACTER*1 COMMAND
!
!            Command       Character*1       Tells ACCOR whether the
!                                            user wants to see the
!                                            screen for the next data
!                                            base or return to the
!                                            OPTIN menu.
!            Obcapl        Integer           Stores which calibrations
!                                            will now be applied.
!
! 2.4 COMMON BLOCKS USED
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES: accor
!       CALLED SUBROUTINES: utility routines
!
! 3.  LOCAL VARIABLES
!
!            Abnormalmode   Logical          True, if an abnormal
!                                            situation occurs and
!                                            forces a premature exit.
!            Changeconfirm  Character*11     New application status.
!                                            Printed for each calibra-
!                                            tion, when the user
!                                            changes all calibrations
!                                            at once.
!            Changetarget   Integer          0 or 1.  Value which
!                                            every bit of OBCAPL will
!                                            assume when user chooses
!                                            to change all calibra-
!                                            tions, making all cali-
!                                            brations unapplied (0) or
!                                            applied (1).
!            Ichangecalib   Integer          Calibration to be changed
!            Ifrstycalib    Integer          Line of screen on which
!                                            first calibration is
!                                            listed.
!            Ifrstxkey,     Integer          X, Y coords. of the first
!             Ifrstykey                      character of the key to
!                                            the statuses.
!            Ifrstystatus   Integer          Line on which first cali-
!                                            bration status is printed
!            Ihivalystat    Integer          OBCOR can handle up to 6
!                                            calibrations.  Depending
!                                            on how many are actually
!                                            used, certain lines of
!                                            screen may either list
!                                            calibrations and statuses
!                                            and accept input, or be
!                                            blank and not accept
!                                            input.  Contains lowest
!                                            screen line (highest y
!                                            coord.) that is a valid
!                                            status display/input line
!                                            given the actual number
!                                            of calibrations.
!            Ihixapply      Integer          Highest X coord. of the
!                                            field for applying all
!                                            calibrations to the data
!                                            base.
!            Ihixdapply     Integer          Highest X coord. of the
!                                            field for stopping the
!                                            application of all cali-
!                                            brations to the data base
!            Ihixndb        Integer          Highest X coord. of the
!                                            field which allows the
!                                            user to see the screen
!                                            for the next data base.
!            Ihixpdb        Integer          Highest X coord. of the
!                                            field which allow the
!                                            user to see the screen
!                                            for the previous database.
!            Ihixret        Integer          Highest X coord. of the
!                                            field for returning to
!                                            the options menu.
!            Ihixstatus     Integer          Highest X coord. of the
!                                            fields for displaying
!                                            the calibration statuses.
!            Iloxapply      Integer          Lowest X coord. of the
!                                            field for applying all
!                                            calibrations to the data
!                                            base.
!            Iloxcalib      Integer          Lowest X coord. of the
!                                            fields which display the
!                                            calibrations.
!            Iloxdapply     Integer          Lowest X coord. of the
!                                            field for stopping the
!                                            application of all cali-
!                                            brations to the data base
!            Iloxndb        Integer          Lowest X coord. of the
!                                            field which allows the
!                                            user to see the screen
!                                            for the next data base.
!            Iloxpdb        Integer          Lowest X coord. of the
!                                            field which allows the
!                                            user to see the screen
!                                            for the previous database.
!            Iloxret        Integer          Lowest X coord. of the
!                                            field for returning to
!                                            the options menu.
!            Iloxstatus     Integer          Lowest X coord. of the
!                                            fields for displaying the
!                                            calibrations statuses.
!            Ixabnorm,      Integer          X, Y coords. of the first
!             Iyabnorm                       character of the message
!                                            printed when an abnormal
!                                            situation occurs and
!                                            forces a premature exit.
!            Ixcolhd,       Integer          X, Y coords. of the first
!             Iycolhd                        character of the column
!                                            heading.
!            Ixcomment,     Integer          X, Y coords. of the
!             Iycomment                      applications explanation.
!            Ixmsg,         Integer          X, Y coords. of the
!             Iymsg                          first character of the
!                                            message field.
!            Ixtitle,       Integer          X, Y coords. of the first
!             Iytitle                        character of the screen
!                                            title.
!            Iyapply        Integer          Line for the field which
!                                            lets the user apply all
!                                            calibrations to the data
!                                            base.
!            Iydapply       Integer          Line for the field which
!                                            lets the user stop all
!                                            calibrations from being
!                                            applied to the data base.
!            Iyndb          Integer          Line for the field which
!                                            allows the user to see
!                                            the screen for the next
!                                            data base.
!            Iypdb          Integer          Line for the field which
!                                            allows the user to see
!                                            the screen for the previous
!                                            database.
!            Iyret          Integer          Line for the field which
!                                            lets the user return to
!                                            the options menu.
!            Unaffected     Logical          Notes whether the user
!                                            tried to apply an
!                                            unavailable calibration,
!                                            in the case where the
!                                            user tried to apply all
!                                            calibrations or tried to
!                                            stop the application of
!                                            all calibrations.  Used
!                                            to print or suppress
!                                            a message.
      LOGICAL*2 ABNORMALMODE
      CHARACTER CHANGECONFIRM*11, LABEL*15, BUFSTR*79
      INTEGER*2 CHANGETARGET
      LOGICAL*2 KBIT
      LOGICAL*2 UNAFFECTED
      INTEGER*2 I,ICHANGECALIB,IFRSTXKEY,IFRSTYCALIB,IFRSTYKEY, &
     &   IFRSTYSTATUS,IHIVALYSTAT,IHIXAPPLY,IHIXDAPPLY,IHIXNDB, &
     &   IHIXPDB,IHIXRET,IHIXSTATUS,ILOXAPPLY,ILOXCALIB,ILOXDAPPLY, &
     &   ILOXNDB,ILOXPDB,ILOXRET,ILOXSTATUS,IXABNORM,IXCOLHD, &
     &   IXCOMMENT,IXTITLE,IYABNORM,IYAPPLY,IYCOLHD, &
     &   IYCOMMENT,IYDAPPLY,IYNDB,IYPDB,IYRET,IYTITLE,J
      INTEGER*2 ILINIT, IHINIT, IYINIT, ILMODE, IHMODE, IYMODE
      INTEGER*2 J1, J2, J3, LUN
      INTEGER*4 IERR
      INTEGER*2 LCTYP, LCNUM, LDISP(8), LCORC(7), LCSTA(7), LCFAC(2)
      integer*4 ich,ix,iy,ixmsg,iymsg
      CHARACTER*4 CCH
      EQUIVALENCE (ICH,CCH)
      CHARACTER    STR*128, GET_VERSION*54, LCORF*128, CBUF*80
!
      DATA label /'123456789#$%^&*'/
      DATA IFRSTYCALIB/3/
      DATA IFRSTXKEY/40/, IFRSTYKEY/6/ ! 50, 6
      DATA IHIXAPPLY/17/
      DATA IHIXDAPPLY/42/
      DATA IHIXNDB/15/
      DATA IHIXPDB/36/
      DATA IHIXRET/15/
      DATA ILOXAPPLY/0/
      DATA ILOXCALIB/3/
      DATA ILOXDAPPLY/20/
      DATA ILOXNDB/0/
      DATA ILOXPDB/17/
      DATA ILOXRET/0/
      DATA IXCOLHD/3/, IYCOLHD/2/
      DATA IXCOMMENT/0/
      DATA IXMSG  / 40 /, IYMSG/18/
      DATA IXTITLE/0/
      DATA IXABNORM/20/, IYABNORM/8/
      DATA ILINIT  / 45 /,  IHINIT  / 56 /
      DATA ILMODE  / 59 /,  IHMODE  / 77 /
!
      DATA IYAPPLY/19/
      DATA IYCOMMENT/22/
      DATA IYDAPPLY/19/
      DATA IYINIT  / 19 /
      DATA IYMODE  / 19 /
      DATA IYNDB/20/
      DATA IYPDB/20/
      DATA IYRET/18/
      DATA IYTITLE/0/
      DATA I4P0 /0/
      INTEGER*4, EXTERNAL :: ILEN, I_LEN
!
! 4.  HISTORY
!   WHO   WHEN   WHAT
!   KDB  860219  Created
!   KDB  860417  Handle multiple data bases
!   KDB  860505  Refresh screen and initiate least squares
!   KDB  870826  Add previous data base option; go to CRES directly
!                from menu; go pages 2-4 of menu (databases 2-4)
!                directly from OPTIN
!   JLR  921215  added I4P0 variable to replace 0J constant
!   pet  1999.11.10   Added printing a version label at the first line and
!                     the session name at the second line
!   pet  1999.11.10  Added support of a new option: Initialize: Calibrations
!                    specified in the 41-th section of CORFIL are set as
!                    applied if they are are available. Calibrations not listed
!                    in the 41-th section of CORFIL are not applied
!
! 5.  OBCOR PROGRAM STRUCTURE
!
      ILOXSTATUS = ILOXCALIB + 19
      IHIXSTATUS = ILOXSTATUS + 10
      IFRSTYSTATUS = IFRSTYCALIB
      ABNORMALMODE = .FALSE.
!
      IHIVALYSTAT = IFRSTYSTATUS + (NOBCAL - 1)
!
!     Check for abnormal cases.  Either no contributions exist,
!     indicating that CORFIL contained no contribution (40) recs, or
!     no contributions were available, indicating that no LCODES
!     existed for the contributions listed in CORFIL.  This
!     probably indicates that the data base is a calc version less
!     than 6.0.
!     Return to the options menu when the user enters his first
!     command whatever that command is.
!
      IF ( NOBCAL .LE. 0 .OR. OBCAVL .EQ. 0 ) THEN
           CALL SETCR_MN (I4P0,I4P0 )
           CALL CLEAR_MN()
           IX = IXABNORM
           IY = IYABNORM
           CALL SETCR_MN ( IX, IY )
           IF ( OBCAVL .EQ. 0) THEN
                CALL ADDSTR_F ( "NO CALIBS AVAILABLE--RETURN TO OPTIONS" )
                IY = IY + 1
                CALL SETCR_MN ( IX, IY )
                CALL ADDSTR_F ( "DATA BASE VERSION PROBABLY LESS THAN 6.0" )
                CALL NL_MN()
              ELSE
                CALL ADDSTR_F ( "NO CALIBRATIONS EXIST-RETURN TO OPTIONS" )
                IY = IY + 1
                CALL SETCR_MN ( IX, IY )
                CALL ADDSTR_F ( "CORFIL NEEDS A 40 REC FOR EACH CALIB" )
                CALL NL_MN()
          END IF
!@          ABNORMALMODE = .TRUE.
!@          GOTO 90
      END IF
!
! --- Build initial screen.
!
! --- Clear screen and write title and column headings.
!
  50  CONTINUE
      CALL SETCR_MN (I4P0,I4P0 )
      CALL CLEAR_MN()
      IX = IXTITLE
      IY = IYTITLE
      CALL SETCR_MN ( IX, IY )
      BUFSTR  = 'Observation Dependent Calibration Status'
      CALL ADDSTR_F ( BUFSTR )
!
      STR = GET_VERSION()
      CALL SETCR_MN ( 79-I_LEN(STR), 0 )
      CALL REVERSE_ON_MN()
      CALL ADDSTR_F ( STR(1:I_LEN(STR)) )
      CALL REVERSE_OFF_MN()
!
      CALL SETCR_MN ( 58, 1 )
      WRITE ( BUFSTR, 1010 ) (LDBNAM(J), J = 1, 5), NNVER
 1010 FORMAT ( 5A2, '  Vers.', I4 )
      CALL ADDSTR_F ( BUFSTR(1:21) )
!
      IX = IXCOLHD
      IY = IYCOLHD
      CALL SETCR_MN (IX, IY )
      CALL ADDSTR_F ( "Code  Calibration    Status      |   Legend:" )
!
! --- List the calibrations and their statuses.
!
      IX = ILOXCALIB
      IY = IFRSTYCALIB
!
      DO I = 1, NOBCAL
         CALL SETCR_MN (IX, IY )
         WRITE ( BUFSTR, "('(', A1, ')   ', A8)") label(I:I), QDOBCAL(I)
         CALL ADDSTR_F ( BUFSTR )
         IY = IY + 1
      END DO
!
      IX = ILOXSTATUS
      IY = IFRSTYSTATUS
      DO I = 1, NOBCAL
         CALL SETCR_MN (IX, IY )
         IF ( KBIT ( OBCAVL, I) ) THEN
              IF ( KBIT (OBCAPL, I)) THEN
                   CALL ADDSTR_F ( "  Applied     |" )
                 ELSE
                   CALL ADDSTR_F ( "NOT Applied   |" )
              END IF
            ELSE
              CALL ADDSTR_F ( "-----------   |" )
         END IF
         IY = IY + 1
      END DO
!
! --- Then write key, explaining the statuses.
!
      IX = IFRSTXKEY
      IY = IFRSTYKEY
      CALL SETCR_MN (IX, IY )
      CALL ADDSTR_F ( "-----------  :" )
      CALL ADDSTR_F ( " not available" )
      IY = IY + 2
      CALL SETCR_MN (IX, IY )
      CALL ADDSTR_F ( "NOT APPLIED  :" )
      CALL ADDSTR_F ( " available but not applied" )
      IY = IY + 2
      CALL SETCR_MN (IX, IY )
      CALL ADDSTR_F ( "APPLIED      :" )
      CALL ADDSTR_F ( " available and applied" )
!
! --- Explain how calibrations are applied to the data base.
!
      IX = IXCOMMENT
      IY = IYCOMMENT
      CALL SETCR_MN (IX, IY )
      BUFSTR = '=== A calibration is either applied to all observations in '// &
     &         'the data base or ==='
      CALL ADDSTR_F ( BUFSTR )
      IX = IXCOMMENT
      IY = IYCOMMENT+1
      CALL SETCR_MN (IX, IY )
      BUFSTR = '=== else not applied to any observation in the data base   '// &
     &         '                 ==='
      CALL ADDSTR_F ( BUFSTR )
!
! --- Write field allowing the user to return to the options menu.
!
      IX = ILOXRET
      IY = IYRET
      CALL SETCR_MN (IX, IY )
      CALL ADDSTR_F ( "Return (O)ptions" )
!
! --- Write fields allowing the user to apply all calibrations at once
! --- or stop all calibrations from being applied, at once.
!
      IX = ILOXAPPLY
      IY = IYAPPLY
      CALL SETCR_MN (IX, IY )
      CALL ADDSTR_F ( "(A)pply all calibs" )
!
      IX = ILOXDAPPLY
      IY = IYDAPPLY
      CALL SETCR_MN (IX, IY )
      CALL ADDSTR_F ( "(D)on't apply any calib" )
!
      IX = ILINIT
      IY = IYINIT
      CALL SETCR_MN ( IX, IY )
      CALL ADDSTR_F ( "(I)nitialize" )
!
      IX = ILMODE
      IY = IYMODE
      CALL SETCR_MN ( IX, IY )
      CALL ADDSTR_F ( "(M)ode calibrations" )
!
! --- Write field allowing user to see the screen for the next data
! --- base.
!
      IX = ILOXNDB
      IY = IYNDB
      CALL SETCR_MN (IX, IY )
      call addstr_f("(N)ext data base" )
!
!     Write field allowing user to see the screen for the next previous
!     base.
!
      IX = ILOXPDB
      IY = IYPDB
      CALL SETCR_MN (IX, IY )
      call addstr_f("(P)revious data base" )
!
!     Place cursor where user can conveniently use a blank to enter
!     input.
!     Accept, interpret user's command.  Erase any messages left over
!     from the previous command, to avoid confusion.
!
 90   CONTINUE
      IX = ILOXSTATUS
      IY = IYAPPLY - 1
      CALL SETCR_MN (IX, IY )
 91   CONTINUE
      CALL SENKR_MN (IX, IY,  ICH )
!
!     Abnormal processing.
!
      IF (ABNORMALMODE) GO TO 130
!
!     Process command.  Except for the 'hidden' commands to refresh
!     the screen (R), perform least squares (Q) and calculate residuals
!     (@), OBCOR will recognize each command
!     as a pair of coordinates, so convert letter and number commands
!     to the proper coordinates.  (0, 0) will be invalid coordinates,
!     generating an invalid message for an invalid key command.
!
      IF (CCH(4:4) .EQ. ' ') GO TO 100
!
      IF (CCH(4:4) .EQ. 'A') THEN
        IX = ILOXAPPLY
        IY = IYAPPLY
      ELSE IF (CCH(4:4) .EQ. 'D') THEN
        IX = ILOXDAPPLY
        IY = IYDAPPLY
      ELSE IF (CCH(4:4) .EQ. 'N') THEN
        IX = ILOXNDB
        IY = IYNDB
      ELSE IF (CCH(4:4) .EQ. 'P') THEN
        IX = ILOXPDB
        IY = IYPDB
      ELSE IF (CCH(4:4) .EQ. 'O') THEN
        IX = ILOXRET
        IY = IYRET
      ELSE IF ( CCH(4:4) .EQ. 'I' ) THEN
        IX = ILINIT
        IY = IYINIT
      ELSE IF ( CCH(4:4) .EQ. 'M' ) THEN
        IX = ILMODE
        IY = IYMODE
      ELSE IF (CCH(4:4) .EQ. 'Q') THEN !Hidden option (least squares)
        COMMAND = 'Q'
        GO TO 130
      ELSE IF (CCH(4:4) .EQ. '@') THEN !Hidden option (calculate residuals)
        COMMAND = '@'
        GO TO 130
      ELSE IF (CCH(4:4) .EQ. 'R') THEN  !Hidden option (refresh screen)
        GO TO 50
      ELSE IF (CCH(4:4) .EQ. '1' .AND. NOBCAL .GE. 1) THEN
        IX = ILOXCALIB
        IY = IFRSTYCALIB
      ELSE IF (CCH(4:4) .EQ. '2' .AND. NOBCAL .GE. 2) THEN
        IX = ILOXCALIB
        IY = IFRSTYCALIB + 1
      ELSE IF (CCH(4:4) .EQ. '3' .AND. NOBCAL .GE. 3) THEN
        IX = ILOXCALIB
        IY = IFRSTYCALIB + 2
      ELSE IF (CCH(4:4) .EQ. '4' .AND. NOBCAL .GE. 4) THEN
        IX = ILOXCALIB
        IY = IFRSTYCALIB + 3
      ELSE IF (CCH(4:4) .EQ. '5' .AND. NOBCAL .GE. 5) THEN
        IX = ILOXCALIB
        IY = IFRSTYCALIB + 4
      ELSE IF (CCH(4:4) .EQ. '6' .AND. NOBCAL .GE. 6) THEN
        IX = ILOXCALIB
        IY = IFRSTYCALIB + 5
      ELSE IF (CCH(4:4) .EQ. '7' .AND. NOBCAL .GE. 7) THEN
        IX = ILOXCALIB
        IY = IFRSTYCALIB + 6
      ELSE IF (CCH(4:4) .EQ. '8' .AND. NOBCAL .GE. 8) THEN
        IX = ILOXCALIB
        IY = IFRSTYCALIB + 7
      ELSE IF (CCH(4:4) .EQ. '9' .AND. NOBCAL .GE. 9) THEN
        IX = ILOXCALIB
        IY = IFRSTYCALIB + 8
      ELSE IF (CCH(4:4) .EQ. '#' .AND. NOBCAL .GE. 10) THEN
        IX = ILOXCALIB
        IY = IFRSTYCALIB + 9
      ELSE IF (CCH(4:4) .EQ. '$' .AND. NOBCAL .GE. 11) THEN
        IX = ILOXCALIB
        IY = IFRSTYCALIB + 10
      ELSE IF (CCH(4:4) .EQ. '%' .AND. NOBCAL .GE. 12) THEN
        IX = ILOXCALIB
        IY = IFRSTYCALIB + 11
      ELSE IF (CCH(4:4) .EQ. '^' .AND. NOBCAL .GE. 13) THEN
        IX = ILOXCALIB
        IY = IFRSTYCALIB + 12
      ELSE IF (CCH(4:4) .EQ. '&' .AND. NOBCAL .GE. 14) THEN
        IX = ILOXCALIB
        IY = IFRSTYCALIB + 13
      ELSE IF (CCH(4:4) .EQ. '*' .AND. NOBCAL .GE. 15) THEN
        IX = ILOXCALIB
        IY = IFRSTYCALIB + 14
      ELSE
        IX = 0
        IY = 0
      END IF
!
! --- The user's input is now expressed as coordinates.  Perform the
! --- the user's command.
!
! --- If the command was to apply a single calibration to the data
! --- base, or stop a single calibration from being applied to the
! --- data base,
!
 100  CONTINUE
      IF (IY .GE. IFRSTYSTATUS .AND. IY .LE. (IFRSTYSTATUS +14)) THEN
!
!     Up to 15 calibrations may be used.  Depending on the actual
!     number used, some Y coordinates of the screen may represent
!     valid or invalid input.  Make sure that the input Y coordinate
!     is valid for the actual number of calibrations.
!
        IF (IY .LE. IHIVALYSTAT) THEN
!
!    Make sure that the user input his command within the actual field
!
          IF (IX .GE. ILOXCALIB .AND. IX .LE. IHIXSTATUS) THEN
            ICHANGECALIB = IY - 2
            IF (KBIT (OBCAVL, ICHANGECALIB)) THEN
              IX = ILOXSTATUS
              CALL SETCR_MN (IX, IY )
              IF (KBIT (OBCAPL, ICHANGECALIB)) THEN
                CALL SBIT ( OBCAPL, ICHANGECALIB, INT2(0) )
                call addstr_f("NOT Applied" )
              ELSE
                CALL SBIT ( OBCAPL, ICHANGECALIB, INT2(1) )
                call addstr_f("  Applied  " )
              END IF
            ELSE
              IX = IXMSG
              IY = IYMSG
              CALL SETCR_MN (IX, IY )
              call addstr_f("Can't apply" )
            END IF
            CALL SETCR_MN (IX, IY )
            GO TO 91
          ELSE
            GO TO 110
          END IF
        ELSE
          GO TO 110
        END IF
      END IF
!
!     If the command was to apply all calibrations to the data base...
!
      IF ((IX .GE. ILOXAPPLY .AND. IX .LE. IHIXAPPLY) .AND. &
     &     IY .EQ. IYAPPLY) THEN
        CHANGECONFIRM = '  Applied'
        CHANGETARGET = 1
        GO TO 120
      END IF
!
!     Or if the command was to stop the application of all
!     calibrations to the data base, go to code to perform the
!     request change.
!
      IF ((IX .GE. ILOXDAPPLY .AND. IX .LE. IHIXDAPPLY) .AND. &
     &     IY .EQ. IYDAPPLY) THEN
        CHANGECONFIRM = 'NOT Applied'
        CHANGETARGET = 0
        GO TO 120
      END IF
!
!     If the command was to return to the options menu, return.
!
      IF (IX .GE. ILOXRET .AND. IX .LE. IHIXRET .AND. &
     &    IY .EQ. IYRET) THEN
        COMMAND = 'R'
        GO TO 130
      END IF
!
! --- If the command was to see the screen for the next data base,
! --- return to ACCOR so that ACCOR can record any changes and set
! --- up the data for the next data base.
!
      IF ( IX .GE. ILOXNDB  .AND.  IX .LE. IHIXNDB  .AND.  IY .EQ. IYNDB ) THEN
           COMMAND = 'N'
           GOTO 130
      END IF
!
! --- If the command was to see the screen for the previous data base,
! --- return to ACCOR so that ACCOR can record any changes and set
! --- up the data for the previous data base.
!
      IF ( IX .GE. ILOXPDB .AND. IX .LE. IHIXPDB .AND. &
     &     IY .EQ. IYPDB ) THEN
           COMMAND = 'P'
           GOTO 130
      END IF
!
      IF ( IX .EQ. ILINIT  .AND.  IX .LE. IHINIT  .AND.  IY .EQ. IYINIT ) THEN
!
! -------- Operation: setting initial observation calibration setup.
! -------- First clear off all calibrations
!
           DO 410 J1=1,NOBCAL
              CALL SBIT ( OBCAPL, J1, INT2(0) )
 410       CONTINUE
!
! -------- Form the name of a corfil file
!
           LCORF = PRE_SCR_DIR(1:PRE_SD_LEN)//'CORF'//PRE_LETRS
!
! -------- Look: is the environment variable CORFIL specified?
!
           CALL GETENVAR ( 'CORFIL', STR )
           IF ( ILEN(STR) .NE. 0 ) LCORF = STR
!
! -------- Open CORFIL file
!
           LUN = 301
           OPEN ( UNIT=LUN, FILE=LCORF, IOSTAT=IERR, STATUS='OLD' )
           IF ( IERR .NE. 0 ) THEN
                IX = IXMSG
                IY = IYMSG
                CALL SETCR_MN ( IX, IY )
                CALL ADDSTR_F ( "ERROR in openning CORFIL" )
           END IF
!
! -------- Read CORFIL file. We are looking for the section 41
!
           DO 420 J2=1,1024
              CALL REACO ( LUN, LCTYP, LCNUM, LDISP, LCORC, LCSTA, LCFAC, CBUF )
              IF ( LCTYP .EQ. 41 ) THEN
!
! ---------------- Well. Check does tha calibration specified in this line of
! ---------------- the section 41 of the CORFIL available
!
                   DO 430 J3=1,NOBCAL
!
! ------------------- Replace underscores with blanks
!
                      CALL UNDSCR ( CBUF(8:15) )
                      IF ( CBUF(8:15) .EQ. QDOBCAL(J3)  .AND. &
     &                     KBIT ( OBCAVL, J3 )                ) THEN
!
! ------------------------ ... if available -- apply it!
!
                           CALL SBIT ( OBCAPL, J3, INT2(1) )
                      END IF
 430               CONTINUE
                ELSE IF ( LCTYP .EQ. 99 ) THEN
!
! ---------------- End of CORFIL has been detected
!
                   GOTO 820
              END IF
 420       CONTINUE
 820       CONTINUE
!
! -------- Close CORFIL file
!
           CLOSE ( UNIT=LUN )
           GOTO 50
      END IF
!
      IF ( IX .EQ. ILMODE  .AND.  IX .LE. IHMODE  .AND.  IY .EQ. IYMODE ) THEN
           COMMAND = 'M'
           GOTO 130
      END IF
!
! --- The user gave invalid input.  Print an error message.
!
 110  CONTINUE
      IX = IXMSG
      IY = IYMSG
      CALL SETCR_MN (IX, IY )
      call addstr_f ( "ERROR: Invalid command            " )
      GO TO 90
!
! --- Change the application status of all calibrations to applied
! --- or not applied, as requested.
!
 120  CONTINUE
      IX = ILOXSTATUS
      IY = IFRSTYSTATUS
      UNAFFECTED = .FALSE.
      DO ICHANGECALIB = 1, NOBCAL
         IF ( KBIT (OBCAVL, ICHANGECALIB)) THEN
              CALL SBIT (OBCAPL, ICHANGECALIB, CHANGETARGET )
              CALL SETCR_MN (IX, IY )
              WRITE ( BUFSTR, "(A11)" ) CHANGECONFIRM
              CALL ADDSTR_F ( BUFSTR(:11) )
           ELSE
              UNAFFECTED = .TRUE.
         END IF
         IY = IY + 1
      END DO
!
      IF ( UNAFFECTED ) THEN
           IX = IXMSG
           IY = IYMSG
           CALL SETCR_MN (IX, IY )
           CALL ADDSTR_F ( "Some calibs unaffected" )
      END IF
!
      GOTO 90
!
 130  CONTINUE
      RETURN
      END   !#!  OBCOR  #!#
