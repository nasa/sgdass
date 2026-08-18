      SUBROUTINE FLEDIT ( COMMAND, FILENAMES, NUMFLOPTS, DEFAULTS, &
     &                    PLFIX )
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
!     Updated to specificaly type integers which
!-------------------------------------------------
! 1.  FLEDIT PROGRAM SPECIFICATION
!
! 1.1
!     This subroutine enables the user to select which files will
!     supply the values for the flyby options (a priori stations,
!     nutation daily offset, etc.).
!
!
! 1.2 REFERENCES:
!
! 2.  FLEDIT INTERFACE
!
! 2.1 Parameter File
      INCLUDE 'solve.i'
!
! 2.2 INPUT Variables:
!
      CHARACTER*(NAME_SIZE) FILENAMES(11),defaults(11),plfix
      INTEGER*2 NUMFLOPTS
!
! FILENAMES - FILENAMES(I) names the file whose walues will be applied
!             to flyby option I, in the least sqares processing
! NUMFLOPTS - Number of flyby options
!
! 2.3 OUTPUT Variables:
!
      CHARACTER*1 COMMAND
!
! COMMAND - Tells FLOPT whether the user wants to:
!              1. Return to OPTIN, saving his changes
!              2. Return to OPTIN, cancelling his changes
!              3. go directly to least squares (saving changes)
!
! 2.4 COMMON BLOCKS USED
      INCLUDE 'precm.i'
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES: flopt
!       CALLED SUBROUTINES: calc_bounds
!
! 3.  LOCAL VARIABLES
!
      CHARACTER*82 BUFR          ! Buffer for writing initial screen
      LOGICAL*4 EX                 ! Logical variable for INQUIRE statement
! EX=.TRUE. if inquired file exists.
      CHARACTER*(NAME_SIZE) FILER
      LOGICAL*2 FIRSTADD           ! For aesthetics
      CHARACTER*16 FLOPTDISPS(11) ! Codes representing flyby options
!  on screen
      INTEGER*2 I,J,n              ! Loop control, array pointer
      INTEGER*4 ICH              ! Receives user input from senkr
      CHARACTER*4 CCH            ! Receives user input from senkr
      EQUIVALENCE (ICH,CCH)
      INTEGER*2 IL               ! Leftmost buffer position to be written
! to
      INTEGER*2 IOPT             ! Number of option user wants to update
      INTEGER*2 IPOS             ! Counter for setting up the menu
      INTEGER*2 IR               ! Rightmost buffer postion to be written
! to
      INTEGER*2 IRV(11)           ! Indicates which file is currently
! selected for a given flyby option
      INTEGER*2 IRV1             ! Holds the new file selection until it
! can be placed in IRV
      INTEGER*4 IX               ! Tells SETCR where to put cursor (x coord)
      INTEGER*2 IXDESIG          ! Coordinate assigned to command for
! designating a file not listed
      INTEGER*4 IXN              ! Receives x coord of cursor when user
!   gives input
      INTEGER*4 IXASKFILE        ! X coord for printing message which
!   solicits a file name
      INTEGER*2 IXBOUNDHI(5)     ! Indicate boundaries of fields for
      INTEGER*2 IXBOUNDLO(5)     ! selecting files via cursor positioning
      INTEGER*4 IXCANCELHI       ! Highest x coord of field to cancel
! changes
      INTEGER*4 IXCANCELLO       ! Lowest x coord of field to cancel changes
      INTEGER*4 IXREFRESHHI      ! Highest x coord of field to refresh screen
      INTEGER*4 IXREFRESHLO      ! Lowest  x coord of field to refresh screen
      INTEGER*4 IXDASH1          ! Lowest x coord of field to mark off
!  flyby options list
      INTEGER*4 IXDASH2          ! Lowest x coord of field to mark off
!  list of file names
      INTEGER*2 IXFILE           ! Low x coord of first field in list
!   of file names to be used for flyby
!   options
      INTEGER*4 IXFRESHHI        ! High x coord of field to refresh screen
      INTEGER*4 IXFRESHLO        ! Low x coord of field to refresh screen
      INTEGER*4 IXHEAD1          ! Low x coord of flyby options heading
      INTEGER*4 IXHEAD2          ! Low x coord of files heading
      INTEGER*2 IXOPT            ! Low x coord of first field in list of
! flyby options
      INTEGER*4 IXRECORDHI       ! High x coord
      INTEGER*4 IXRECORDLO       ! Low x coord of field to exit, recording
!   changes
      INTEGER*4 IXRESPONSE       ! Low x coord of field where program will
!   write messages
      INTEGER*4 IXTITLE          ! Low x coord of screen title
      INTEGER*4 IXVERSION        ! x coordinate of the FLOPT version message
      INTEGER*4 IXWAITCOM        ! Coordinate for awaiting a command
      INTEGER*4 IXWRITCOM        ! Coordinate for writing command
      INTEGER*4 IXWAITFILE       ! Low x coord of field where program will
! await a file name, once the user has
! asked to change an option
      INTEGER*4 IY               ! Tells SETCR_MN where to put cursor (y coord)
      INTEGER*4 IYN              ! Receives y coord of cursor when user
!   gives input
      INTEGER*4 IYASKFILE        ! Y coordinate for printing message which
!   solicits file name
      INTEGER*4 IYCANCEL         ! Line of field to exit, cancelling changes
      INTEGER*4 IYREFRESH        ! Line of field to refresh screen
      INTEGER*4 IYDASH           ! Line of fields to mark off flyby options
!  list and list of file names
      INTEGER*4 IYFRESH          ! Line of field to refresh screen
      INTEGER*4 IYHEAD           ! Line of option and file headings
      INTEGER*2 IYOPT            ! Line where first file and option are
!   displayed
      INTEGER*4 IYRECORD         ! Line of field to exit, recording changes
      INTEGER*4 IYRESPONSE       ! Line of field where program writes
!   messages
      INTEGER*4 IYTITLE          ! Line of screen title
      INTEGER*4 IYVERSION        ! y coordinate of the FLOPT version message
      INTEGER*4 IYWAITCOM        ! Coordinate for awaiting a command
      INTEGER*4 IYWRITCOM        ! Coordinate for writing command
      INTEGER*4 IYWAITFILE       ! Line of field where program will wait for
!   a file name
      INTEGER*2 LNG,TRIMLEN,ILD
      CHARACTER*10 LISTFILES(11,5) ! Array of possible files displayed to
!  user
      INTEGER*2 NNONE            ! Line position for using no file for flyby
      INTEGER*2 NUSER            ! Line position of user designated file
      CHARACTER*50 REQFILE       ! User's latest request
      CHARACTER*10 STANDARDS(11,3) ! Files available for flyby
      LOGICAL*2 USER               ! Indicates whether or not the selected
! file is a user designated one
      LOGICAL*2 FORCED_CHOICE(11)!true if user forced to choose a file
! (cannot choose none)
      CHARACTER*79 ERRSTR, FLYBY_BUF, BUFSTR
      INTEGER*2    IM1
      INTEGER*4    IOS
      character*11 ilab
      character*(name_size) def_check
      logical*2 def_okay
      character*1 cdum1
      character*18 sccsid
      character*8  sccsid_g
      PARAMETER (sccsid = '03/05/97  21:19:09')
      PARAMETER (sccsid_g = '30/08/97')
      DATA ilab /'1234567890A'/
!
      DATA FLOPTDISPS /'APRIORI STATIONS','APRIORI SOURCES ', &
     &                 'NUTATION MODEL  ','NUTATION OFFSET ', &
     &                 'EARTH ROTATION  ','TECTONIC PLATE  ', &
     &                 'APRIORI VELOCITY','HF_EOP CALIB    ', &
     &                 'PRESS_LOAD CALIB','AXIS OFFSET     ', &
     &                 'SITPL           '/
!
      DATA FORCED_CHOICE /.FALSE.,.FALSE., &
     &                    .FALSE.,.FALSE., &
     &                    .FALSE.,.FALSE., &
     &                    .FALSE.,.FALSE., &
     &                    .FALSE.,.FALSE., &
     &                    .TRUE./
!
      DATA IXASKFILE   / 0/
      DATA IXCANCELHI  /40/
      DATA IXCANCELLO  /25/
      DATA IXREFRESHHI /78/
      DATA IXREFRESHLO /70/
      DATA IXDASH1     / 3/
      DATA IXDASH2     /25/
      DATA IXDESIG     / 0/
      DATA IXFILE      /25/
      DATA IXFRESHHI   /67/
      DATA IXFRESHLO   /49/
      DATA IXHEAD1     / 3/
      DATA IXHEAD2     /25/
      DATA IXOPT       / 3/
      DATA IXRECORDHI  /19/
      DATA IXRECORDLO  / 0/
      DATA IXRESPONSE  / 0/
      DATA IXTITLE     / 0/
      DATA IXVERSION   /60/
      DATA IXWRITCOM   / 0/
      DATA IXWAITFILE  /33/
      DATA IYASKFILE   /19/
      DATA IYCANCEL    /21/
      DATA IYREFRESH   /21/
      DATA IYDASH      / 3/
      DATA IYFRESH     /21/
      DATA IYHEAD      / 2/
      DATA IYOPT       / 5/
      DATA IYRECORD    /21/
      DATA IYRESPONSE  /18/
      DATA IYTITLE     / 0/
      DATA IYVERSION   / 0/
      DATA IYWRITCOM   /19/
      DATA IYWAITFILE  /19/
!
      INTEGER*4 I4P0, I4P22
      DATA I4P0 /0/, I4P22 /22/
! 4.  HISTORY
!   WHO   WHEN   WHAT
!   KDB  870507  Created
!   KDB  870915  Display standard list of files which can be selectd
!                through cursor positioning
!   MWH  900608  am0-2 changed to AM0-2
!   MWH  910402  Added NUVEL as option for plate model
!   AEE  910814  Read flyby options from FLYBY_FILES instead of having
!                them hard coded.
!   MWH  941004  Added NUVEL-1A as option for plate model
!   KDB  951012  Designate options (e.g., sitpl) for which the user must
!                select a file (i.e., cannot select none).
!   KDB  951107  Change misleading error message about opening flyby_files file
!   KDB  960416  Convert hardcoded date to sccs-based date.
!                Add sccsid parameter for version tracking.
!   PET  970830  Made some small cosmetic changes to force FLOPT to work
!                prperly at 80x24 screen
!   pet  2001.01.11  Allowed user to specife full path of the eop-mod file
!
! 5.  FLOPT PROGRAM STRUCTURE
!
! Open user's flyby defaults scratch file:
!
      OPEN ( UNIT=45, FILE=PRE_SCR_DIR(:PRE_SD_LEN)//'FDEF'//PRE_LETRS, &
     &       IOSTAT=IOS, STATUS='OLD' )
      IF (IOS .NE. 0) then
        write(errstr, &
     &                "('FLEDIT: error ',I6,' opening FDEF',A2)") IOS,PRE_LETRS
        call ferr( INT2(304), errstr, INT2(0), INT2(0) )
      endif
!
      flyby_buf = ' '
      do while (flyby_buf(1:14).ne.'FLYBY_OPTIONS:')
         read(45,'(A)',IOSTAT=ios) flyby_buf
         CALL FERR ( INT2(IOS), "Reading flyby options", INT2(0), INT2(0) )
      enddo
      do I=1,11
        read(45,'(A)',IOSTAT=IOS) flyby_buf
        call ferr(  INT2(ios), "Reading flyby options", INT2(0), INT2(0) )
        standards(I,1)= flyby_buf(20:29)
        standards(I,2)= flyby_buf(31:40)
        standards(I,3)= flyby_buf(42:51)
      end do
!
      close(45)
!
!     Initialize variables.
!
!      IXWAITCOM = 28
      IXWAITCOM = 62
      IYWAITCOM = IYOPT
      FIRSTADD = .TRUE.
!
!     Display the headings.
!
 50   CONTINUE
      CALL SETCR_MN (I4P0,I4P0 )
      CALL CLEAR_MN()
!
      CALL SETCR_MN ( IXTITLE, IYTITLE )
      BUFSTR = 'FLYBY Options Menu      '
      CALL ADDSTR_F ( BUFSTR )
      WRITE ( BUFSTR, FMT="('FLOPT Ver. ',A,' ' )" ) SCCSID_G
      CALL SETCR_MN ( IXVERSION, IYVERSION )
      CALL REVERSE_ON_MN()
      CALL ADDSTR_F ( BUFSTR(1:19) )
      CALL REVERSE_OFF_MN()
      CALL SETCR_MN (IXHEAD1,IYHEAD )
!
      CALL ADDSTR_F ( "OPTION" )
      CALL SETCR_MN (IXHEAD2,IYHEAD )
      WRITE(bufstr, "('File to be used: ',A)") PRE_SAV_DIR(:PRE_SV_LEN)
      call addstr_f(bufstr )
      call nl_mn()
      CALL SETCR_MN (IXDASH1,IYDASH )
      call addstr_f("--------------------" )
      call nl_mn()
      CALL SETCR_MN (IXDASH2,IYDASH )
      call addstr_f("----------------------------------------" )
      IF (.not.FIRSTADD) THEN
        call addstr_f("------------" )
      endif
      call nl_mn()
      call refresh_mn()
!
!     Display the data.
!
      DO 35 I = 1, NUMFLOPTS
!
!       See if currently selected file is a user designated file.
!
        USER = .TRUE.
        DO J = 1,3
          IF (FILENAMES(I) .EQ. STANDARDS(I,J)) USER = .FALSE.
        END DO
        IF (FILENAMES(I).EQ.' ' .OR. FILENAMES(I).EQ. &
     &    'NONE')USER = .FALSE.
!
        IPOS = 0
        IRV(I) = 0
!
!       Load buffer to be written to screen.
!       First, write out fly by option for this line.
!
        IX = IXOPT
        IY = IYOPT - 1 + I
        CALL SETCR_MN (IX,IY )
        IL = 1
        IR = 22
        IM1 = i
        WRITE ( BUFR(IL:IR), "('(', A1, ') ',A16,2X)" ) ILAB(IM1:IM1), &
     &                                                 FLOPTDISPS(I)
        CALL ADDSTR_F(BUFR(IL:IR) )
!
!       Next write out standard options and see if any of these is the
!       currently selected file.
!
        DO J = 1,3     !Standard files
          IPOS = IPOS + 1
          IF (STANDARDS(I,J).EQ.FILENAMES(I) .AND. &
     &        STANDARDS(I,J).NE.' ') IRV(I) = IPOS
          IF (IRV(I) .EQ. IPOS) THEN
            call reverse_on_mn()
          END IF
          WRITE (BUFR,"(A10)") STANDARDS(I,J)
          call addstr_f(bufr(:10) )
          LISTFILES(I,IPOS) = STANDARDS(I,J)
          IF (IRV(I) .EQ. IPOS) THEN
            call reverse_off_mn()
          END IF
          call addstr_f("  " )
        END DO  !Standard files
!
!       Write out the no file chosen option and indicate if selected.
!       There are two cases - flyby options for which this is valid and
!       options for which a file must be chosen.  In the first case write
!       none, and in the second case write blanks to discourage selection
!       of this option.  (A true safeguard against selection of it will
!       be used in the part of the code that actually selects files.)
!       Ideally "none" should never be chosen for options for which it's
!       invalid.  However, highlight the blank field in reverse video if
!       the user has somehow managed to set the option to none or blank
!       to highlight the pathological condition.
!
        IPOS = IPOS + 1
        NNONE = IPOS
        IF (FILENAMES(I).EQ.' ' .OR. FILENAMES(I).EQ. &
     &    'NONE')IRV(I) = IPOS
        IF (IRV(I) .EQ. IPOS) THEN
          call reverse_on_mn()
        END IF
        IF (FORCED_CHOICE(I)) THEN
          call addstr_f("    " )
          LISTFILES(I,IPOS) = '    '
        ELSE
          call addstr_f("NONE" )
          LISTFILES(I,IPOS) = 'NONE'
        END IF
        IF (IRV(I) .EQ. IPOS) THEN
          call reverse_off_mn()
        END IF
        call addstr_f("  " )
!
!       Write out a user designated file, if any.
!
        IPOS = IPOS + 1
        NUSER = IPOS
        IF (USER) IRV(I) = IPOS
!
        IF (IRV(I) .EQ. IPOS) THEN
          call reverse_on_mn()
          WRITE (BUFR,"(A10)") FILENAMES(I)
          call addstr_f(bufr(:10) )
          LISTFILES(I,IPOS) = FILENAMES(I)
          call reverse_off_mn()
        ELSE
          call addstr_f("          " )
          LISTFILES(I,IPOS) = ' '
        END IF
        call nl_mn()
 35   CONTINUE
!
!     Find boundaries for the fields for displaying possible files and
!     choosing them, via cursor positioning.
!
      CALL CALC_BOUNDS(NNONE,IXFILE,IXBOUNDLO,IXBOUNDHI )
!
!     Display the instructions.
!
      CALL SETCR_MN (IXRECORDLO,IYRECORD )
      call addstr_f ( "Rec(O)rd all changes" )
!
      CALL SETCR_MN (IXCANCELLO,IYCANCEL )
      call addstr_f ( "(C)ancel changes" )
!
      CALL SETCR_MN (IXFRESHLO,IYFRESH )
      call addstr_f ( "Restore (D)efaults" )
!
      CALL SETCR_MN ( IXREFRESHLO, IYREFRESH )
      CALL ADDSTR_F ( "(R)efresh" )
      CALL NL_MN()
!
! --- Place cursor where user can conveniently use a blank to enter input.
! --- Accept, interpret user's command.  Erase any messages left over
! --- from the previous command, to avoid confusion.
!
 90   CONTINUE
      CALL SETCR_MN (IXWRITCOM, IYWRITCOM )
      call addstr_f ( "Cursor position or choose number to add your own file " )
      CALL SETCR_MN (IXWAITCOM, IYWAITCOM )
      CALL SENKR_MN (IXN, IYN,  ICH )
      IXWAITCOM = IXN
      IYWAITCOM = IYN
!
      CALL SETCR_MN (IXRESPONSE,IYRESPONSE )
      call nl_mn()
!
!     Process command.  Except for the 'hidden' command
!     to perform least squares (Q), OBCOR will
!     recognize each command
!     as a pair of coordinates, so convert letter and number commands
!     to the proper coordinates.  (0, 0) will be invalid coordinates,
!     generating an invalid message for an invalid key command.
!
      IF (CCH(4:4) .EQ. ' ') GO TO 100
!
      IF ( CCH(4:4) .EQ. 'R') THEN
           IXN = IXREFRESHLO
           IYN = IYREFRESH
         ELSE IF (CCH(4:4) .EQ. 'C') THEN
           IXN = IXCANCELLO
           IYN = IYCANCEL
         ELSE IF (CCH(4:4) .EQ. 'O') THEN
           IXN = IXRECORDLO
           IYN = IYRECORD
         ELSE IF (CCH(4:4) .EQ. 'Q') THEN !Hidden option (least squares)
           COMMAND = 'Q'
           GO TO 130
         ELSE IF (CCH(4:4) .EQ. 'D') THEN
           IXN = IXFRESHLO
           IYN = IYFRESH
         ELSE IF (CCH(4:4) .EQ. '1' ) THEN
           IXN = IXDESIG
           IYN = IYOPT
         ELSE IF (CCH(4:4) .EQ. '2') THEN
           IXN = IXDESIG
           IYN = IYOPT + 1
         ELSE IF (CCH(4:4) .EQ. '3') THEN
           IXN = IXDESIG
           IYN = IYOPT + 2
         ELSE IF (CCH(4:4) .EQ. '4') THEN
           IXN = IXDESIG
           IYN = IYOPT + 3
         ELSE IF (CCH(4:4) .EQ. '5') THEN
           IXN = IXDESIG
           IYN = IYOPT + 4
         ELSE IF (CCH(4:4) .EQ. '6') THEN
           IXN = IXDESIG
           IYN = IYOPT + 5
         ELSE IF (CCH(4:4) .EQ. '7') THEN
           IXN = IXDESIG
           IYN = IYOPT + 6
         ELSE IF (CCH(4:4) .EQ. '8') THEN
           IXN = IXDESIG
           IYN = IYOPT + 7
         ELSE IF (CCH(4:4) .EQ. '9') THEN
           IXN = IXDESIG
           IYN = IYOPT + 8
         ELSE IF (CCH(4:4) .EQ. '0') THEN
           IXN = IXDESIG
           IYN = IYOPT + 9
         ELSE IF ( CCH(4:4) .EQ. 'A'  .OR.  CCH(4:4) .EQ. 'a' ) THEN
           IXN = IXDESIG
           IYN = IYOPT + 10
         ELSE
           IXN = 0
           IYN = 0
      END IF
!
!     The user's input is now expressed as coordinates.  Perform the
!     the user's command.
!
 100  CONTINUE
!
!     If the command was to choose a new file to be used from the current
!     list, for one of the flyby options, process the command.
!
!
      IF (IYN.GE.IYOPT.AND.IYN.LE.(IYOPT+10).AND. IXN.NE.IXDESIG) THEN
!
!       Make sure the user has selected a valid field.
!
        IRV1 = 0
        DO I = 1,5
          IF (IXN.GE.IXBOUNDLO(I) .AND. IXN.LE.IXBOUNDHI(I)) IRV1 = I
        END DO
        IF (IRV1 .EQ. 0) THEN        !Invalid choice
          CALL SETCR_MN (IXRESPONSE,IYRESPONSE )
          call addstr_f("Invalid command" )
          call nl_mn()
          GO TO 90
        END IF
        IOPT = IYN - IYOPT + 1
        IF (IRV1 .EQ. IRV(IOPT)) THEN  !File already selected
          CALL SETCR_MN (IXRESPONSE,IYRESPONSE )
          call addstr_f("Already selected" )
          call nl_mn()
          GO TO 90
        END IF
        IF (LISTFILES(IOPT,IRV1) .EQ. ' ') THEN !Not really a file
          CALL SETCR_MN (IXRESPONSE,IYRESPONSE )
          call addstr_f("Invalid command" )
          call nl_mn()
          GO TO 90
        END IF
!
!       Turn off old field.
!
        IX = IXBOUNDLO(IRV(IOPT))
        IY = IYN
        CALL SETCR_MN (IX,IY )
        call reverse_off_mn()
        IX = IXBOUNDLO(IRV(IOPT))
        IY = IYN
        CALL SETCR_MN (IX,IY )
        IF (IRV(IOPT) .EQ. NNONE) THEN
          WRITE(bufstr,"(A4)") LISTFILES(IOPT,IRV(IOPT))(1:4)
          call addstr_f(bufstr(:4) )
        ELSE
          WRITE(bufstr,"(A10)") LISTFILES(IOPT,IRV(IOPT))
          call addstr_f(bufstr(:10) )
        END IF
        call refresh_mn()
!
!       Save information identfying current file.
!
        IRV(IOPT) = IRV1
        FILENAMES(IOPT) = LISTFILES(IOPT,IRV(IOPT))
!
!       Turn on new field.
!
        IX = IXBOUNDLO(IRV(IOPT))
        IY = IYN
        CALL SETCR_MN (IX,IY )
        call reverse_on_mn()
        IF (IRV(IOPT) .EQ. NNONE) THEN
          WRITE(bufstr,1051) LISTFILES(IOPT,IRV(IOPT)) (1:4)
          call addstr_f(bufstr(:4) )
        ELSE
          WRITE(bufstr,1050) LISTFILES (IOPT,IRV(IOPT))
          call addstr_f(bufstr(:10) )
          if (iopt.eq.6) then
            filenames(7) = 'NONE'
          else if (iopt.eq.7) then
            filenames(6) = 'NONE'
          endif
        END IF
 1051   FORMAT (A4)
        call reverse_off_mn()
        GO TO 50
      END IF
!
!     If the command was to add a new file to the list, do so.  (This
!     new file will automatically be selected for flyby).
!
      IF (IXN.EQ.IXDESIG .AND. IYN.GE.IYOPT.AND.IYN.LE.IYOPT+10) THEN
        IOPT = IYN - IYOPT + 1
!
!       Solicit a file to be used.
!
        CALL SETCR_MN (IXWRITCOM,IYWRITCOM )
        call deleteln_mn()
        CALL SETCR_MN (IXASKFILE,IYASKFILE )
        call deleteln_mn()
        call addstr_f ( "Type filename (:: to cancel)  >> " )
        CALL SETCR_MN (IXWAITFILE,IYWAITFILE )
        call reverse_on_mn()
        call addstr_f ( "                                     " )
        call reverse_off_mn()
        CALL SETCR_MN (IXWAITFILE,IYWAITFILE )
        REQFILE = ' '
        CALL GETSTR_F ( BUFSTR )
        READ ( BUFSTR, '(A)') REQFILE
        IF ( REQFILE .EQ. ' ' ) REQFILE='NONE'
!
!     In the initial implementation of flyby, the only acceptable choices
!     for the plate motion model are AM0-2 or NUVEL or no file, which are
!     already in the standard list.
!
        IF (FLOPTDISPS(IOPT) .EQ. 'TECTONIC PLATE  ') THEN
          if(reqfile.ne.'AM0-2'.and.reqfile.ne.'NUVEL'.and. &
     &       reqfile.ne.'NONE'.and.reqfile.ne.' '.and. &
     &       reqfile.ne.'NUVEL-1A') then
            CALL SETCR_MN (IXRESPONSE,IYRESPONSE )
            call &
     &           addstr_f("Only AM0-2 or NUVEL may be used for the plate motion model" )
            call nl_mn()
            GO TO 90
          endif
          if(reqfile.eq.'NUVEL'.or.reqfile.eq.'NUVEL-1A')then
            CALL SETCR_MN (IXWRITCOM,IYWRITCOM )
            call deleteln_mn()
            CALL SETCR_MN (IXASKFILE,IYASKFILE )
            call addstr_f("Which plate to fix? (Return for NONE):   " )
            call getstr_f(bufstr )
            plfix = bufstr(1:4)
          endif
          filenames(iopt) = reqfile
          goto 50
        END IF
        if (reqfile.eq.'NONE'.and. (.not.(forced_choice(iopt)))) then
          filenames(iopt) = reqfile
          goto 50
        endif
!       Blank out file solicitation
!
        CALL SETCR_MN (IXASKFILE, IYASKFILE )
        call addstr_f("                            " )
        IX = IXWAITFILE -1
        IY = IYWAITFILE
        CALL SETCR_MN (IX,IY )
        call addstr_f("K" )
!
!       Reject attempt to turn off option which must be left on
!
        if (reqfile.eq.'NONE'.and. (forced_choice(iopt))) then
          CALL SETCR_MN (IXRESPONSE,IYRESPONSE )
          WRITE(bufstr, 1012)
 1012     FORMAT(" A file must be selected for this option ")
          call addstr_f(bufstr )
          call nl_mn()
          GO TO 90
        endif
!
!     Cancel if requested.
!
        IF (REQFILE(1:2) .EQ. '::') GO TO 90
!
!    Check requested filename valid or not
!
        IF ( REQFILE(1:1) .EQ. '/' ) THEN
             FILER=REQFILE
           ELSE
             FILER=PRE_SAV_DIR(:PRE_SV_LEN)//REQFILE
        END IF
        INQUIRE(FILE=FILER,EXIST=EX)
        IF ( .NOT. EX ) THEN
             CALL SETCR_MN (IXRESPONSE,IYRESPONSE )
             WRITE(bufstr, 1011 ) REQFILE(1:trimlen(REQFILE))
 1011        FORMAT ( '$$$ File "',A,'" not found $$$' )
             CALL ADDSTR_F(BUFSTR )
             CALL NL_MN()
             GOTO 90
        END IF
!
!       Valid request.
!
        IF (FIRSTADD) THEN
          FIRSTADD = .FALSE.
          CALL SETCR_MN (IXDASH2,IYDASH )
          call &
     &         addstr_f("----------------------------------------------------" )
        END IF
!       Turn off reverse video for currently selected file, to
!       indicate that the user no longer wants it.
!
        IX = IXBOUNDLO(IRV(IOPT))
        IY = IYN
        CALL SETCR_MN (IX,IY )
        call reverse_off_mn()
        IF (IRV(IOPT) .EQ. NNONE) THEN
          WRITE(bufstr,"(A4)") LISTFILES(IOPT,IRV(IOPT))(1:4)
          call addstr_f(bufstr(:4) )
        ELSE
          WRITE(bufstr,"(A10)") LISTFILES(IOPT,IRV(IOPT))
          call addstr_f(bufstr(:10) )
        END IF
        call refresh_mn()
!
!       Update information identifying the current file.
!
        IRV(IOPT) = NUSER
        FILENAMES(IOPT) = REQFILE
        if (iopt.eq.7) filenames(6) = 'NONE'
!
!       Write the new file into the display of filenames, using reverse
!       video to indicate that this file is selected.
!
        LISTFILES(IOPT,NUSER) = REQFILE
        IX = IXBOUNDLO(NUSER)
        IY = IYN
        CALL SETCR_MN (IX,IY )
        call reverse_on_mn()
        WRITE(bufstr,1050) LISTFILES(IOPT,NUSER)
 1050   FORMAT (A10)
        call addstr_f(bufstr(:10) )
        call reverse_off_mn()
        call refresh_mn()
!
!       Echo the option and file chosen on the message lines.
!
        CALL SETCR_MN(IXRESPONSE,IYRESPONSE )
        LNG=TRIMLEN(FILENAMES(IOPT))
        WRITE(bufstr, &
     &    "(A, ' has been chosen for ', A16)")FILENAMES(IOPT)(1:LNG),FLOPTDISPS(IOPT)
        call addstr_f(bufstr )
        call refresh_mn()
        GO TO 50
      END IF
!
!
!     If the command was to return to the options menu, recording
!     the new files, do it.
!
      IF (IXN .GE. IXRECORDLO .AND. IXN .LE. IXRECORDHI .AND. &
     &    IYN .EQ. IYRECORD) THEN
        COMMAND = 'O'
        CALL SETCR_MN(I4P0,I4P22 )
        GO TO 130
      END IF
!
!     If the command was to return to the options menu, cancelling
!     all changes, do it.
!
      IF (IXN .GE. IXCANCELLO .AND. IXN .LE. IXCANCELHI .AND. &
     &    IYN .EQ. IYCANCEL) THEN
        COMMAND = 'C'
        GO TO 130
      END IF
!
      IF ( IXN .GE. IXREFRESHLO .AND. IXN .LE. IXREFRESHHI .AND. &
     &     IYN .EQ. IYREFRESH ) THEN
           COMMAND = 'R'
           GOTO 50
      END IF
!
!     If the command was to restore defaults, do it.
!
        do i=1,11
          def_okay = .true.
          if (forced_choice(i)) then
            def_check = defaults(i)
            call casefold (def_check )
            if (def_check.eq.'NONE'.or.def_check.eq. &
     &          ' ')def_okay = .false.
          endif
          if (def_okay) then
            filenames(i) = defaults(i)
          else
            CALL SETCR_MN (IXRESPONSE,IYRESPONSE )
            call reverse_on_mn()
            call addstr_f("Some defaults were rejected " )
            call addstr_f("as invalid choice of none. " )
            call addstr_f("Return to continue." )
            call reverse_off_mn()
            call nl_mn()
            call getstr_f(cdum1 )
          endif
        enddo
      IF (IXN .GE. IXFRESHLO .AND. IXN .LE. IXFRESHHI .AND. &
     &    IYN .EQ. IYFRESH) THEN
        GO TO 50
      END IF
!
!     Any other command is invalid, and ignored.
!
      CALL SETCR_MN (IXRESPONSE,IYRESPONSE )
      call addstr_f("Invalid command" )
      call nl_mn()
      GO TO 90
!
!
 130  CONTINUE
      RETURN
      END  !#!  FLEDIT  #!#
