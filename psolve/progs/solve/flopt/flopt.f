      PROGRAM FLOPT
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!     Updated to specificaly type integers which
!-------------------------------------------------
!
! 1.  FLOPT PROGRAM SPECIFICATION
!
! 1.1
!     FLOPT gathers the flyby NAMFIL data needed for FLEDIT, the subroutine
!     which allows the user to view and change the data via a terminal.
!     After the call to FLEDIT, FLOPT stores the user's changes and writes
!     the changes to the spool file.
!
!     Files:
!
!        NAMFIL               Contains flyby data.  To process a flyby
!                             option (a priori stations, nutation daily
!                             offset, etc.) SOLVE needs values from a file.
!                             For each option, NAMFIL contains a record
!                             which lists the file whose values SOLVE will
!                             use for that option.
!
!
! 1.2 REFERENCES:
!
! 2.  FLOPT INTERFACE
!
! 2.1 Parameter File
      INCLUDE 'astro_constants.i'
      INCLUDE 'solve.i'
!
! 2.2 INPUT Variables: None
!
! 2.3 OUTPUT Variables: None
!
! 2.4 COMMON BLOCKS USED
      INCLUDE 'precm.i'
      INCLUDE 'glbcm.i'
      INCLUDE 'glbc4.i'
      INCLUDE 'socom.i'
      INCLUDE 'bindisp.i'
      INCLUDE 'flyby.i'
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES: None
!       CALLED SUBROUTINES: fledit,utility routines
!
! 3.  LOCAL VARIABLES
!
      LOGICAL*2 CHANGEMADE             !True if user changed at least one file
!  name.
      CHARACTER*1 CHFLAG             !Symbol used to flag changed file names
!  in the spool file printout
      CHARACTER*1 COMMAND            !Used by FLEDIT to tell FLOPT whether
!  or not to save any changes and start
!  least squares
      CHARACTER*(NAME_SIZE) DEFAULTS(11)
      CHARACTER*1 ERRSTAT            !Identifies statements that may generate
!  fatal errors
      CHARACTER*(NAME_SIZE) FILENAMES(11)
      CHARACTER*4 FLOPTC             !Handles individual FLOPTCODES values in
!  a loop
      INTEGER*2 I                    !Loop control, pointer.
      INTEGER*2 IDEFAULT             !Upon entry,
! 2, if OPTIN is just calling, to see
!    if the current NAMFIL setup is the
!    default setup
! 1, if the user wants to turn on the
!    default setup
! 0, for normal mode (letting the user
!    view and change the NAMFIL setup
!    via a menu
      INTEGER*2 IERR                 !Receives read, write errors.
      INTEGER*2 IPAR(5)              !Receives RMPAR values
      INTEGER*2 IREPLY(2)            ! IREPLY(1) tells OPTIN whether or not
!           to start least squares
! IREPLY(2) tells OPTIN whether or not
!           the final NAMFIL setup is
!           the default setup
      INTEGER*2 IBUFF                !ONE WORD PACKED VERISION OF IREPLY
      INTEGER*2 J                    !Loop control, pointer.
      INTEGER*2 NUMDB                !Number of data bases in user's scratch
! files
      INTEGER*2 NUMFLOPTS            !Number of flyby options
      CHARACTER*(NAME_SIZE) OFILENAMES(11),fname
      CHARACTER*80 errstr,fb_def_buf,bufstr
      character*63 cdum
      real*8 ddum
      character*4 ofix
      INTEGER*4 IOS
!
      DATA NUMFLOPTS /11/
!
! 4.  HISTORY
!   WHO   WHEN   WHAT
!   KDB  870507  Created
!   MK   870708  Check flyby substitution files exists or not.
!   KDB  870917  Allow user to turn on default flyby setup from OPTIN.
!                Allow OPTIN to just see if the current NAMFIL setup
!                is the default.
!   AEE   910515 Enhanced error messages written to the error file.
!   AEE   910814 Read flyby defaults from  FLYBY_DEFAULTS file instead of being
!                hard wired.
!   AEE   920204 Use "FLYBY_DEFAULTS" from solve.i to open FLYBY_DEFAULTS file.
!   jwr   921229 ut1_rs_flyby put into socom, so statement added to update socom.
!   mwh   950228 Use FDEFxx instead of FLYBY_DEFAULTS to ge default mod file names
!   pet   1999.10.15  Added addional parameter to the argument list of FLYBY_INIT
!   pet   2000.09.22  Added once more addional parameter to the argument
!                     list of FLYBY_INIT
!
! 5.  FLOPT PROGRAM STRUCTURE
!
!     Initialize.
!
      CALL PRE_PROG()
      INCLUDE 'flopt_version.i' ! Set revision date of the current version
!
      CALL USE_BUFFER( IDEFAULT, INT2(1), 'OR' )
      CALL USE_GLBFIL   ( 'OR' )
      CALL USE_GLBFIL_4 (  'R' )
      TIME0 = 2444529.5D0/JYEAR__DAYS
!
! --- Set flag indicating that substitution files has not been initialized
! --- This trick will cause programs which read substitutio files to really
! --- read substition files instead of getting them from cache in flyby.i
!
      SRC_SUBSTITUTE_INIT = .FALSE.
      SIT_SUBSTITUTE_INIT = .FALSE.
      VEL_SUBSTITUTE_INIT = .FALSE.
!
      call start_mn()
      call setcr_mn ( 0, 0 )
!
! --- Open FLYBY_DEFAULT file and read the defaults:
!
      fname = PRE_SCR_DIR(:PRE_SD_LEN)//'FDEF'//PRE_LETRS
      open ( UNIT=45, FILE=FNAME, IOSTAT=IOS, STATUS='OLD' )
      IF ( IOS .NE. 0 ) THEN
           WRITE ( ERRSTR, "('FLOPT: error ',I6,' opening ',A)" ) IOS, &
     &             FNAME(1:50)
           CALL FERR ( INT2(300), ERRSTR, INT2(0), INT2(0) )
      endif
!
      FB_DEF_BUF = ' '
      do while (fb_def_buf(1:15).ne.'FLYBY_DEFAULTS:')
         read(45,'(A)',IOSTAT=ios) fb_def_buf
         CALL FERR ( INT2(IOS), "Reading flyby defaults", INT2(0), INT2(0) )
      enddo
!
      DO i=1,11
         FILENAMES(I) = ' '
         READ ( 45, '(A)', IOSTAT=IOS ) FB_DEF_BUF
         CALL FERR ( INT2(IOS), "Reading flyby defaults", INT2(0), INT2(0) )
         DEFAULTS(I)= fb_def_buf(15:28)
      END DO
!
      CLOSE ( 45 )
!
      COMMAND = ' '
!
!     Obtain the flyby data from NAMFIL.  Each data base will have the
!     same flyby data, so just take the data from the first data base.
!     The data are the names of the files which the user currently wants
!     to use for flyby.
!
      FILENAMES(1)  = STASUB_CHR
      FILENAMES(2)  = SRCSUB_CHR
      FILENAMES(3)  = NUTSRS_CHR
      FILENAMES(4)  = NUTDLY_CHR
      FILENAMES(5)  = EOPDLY_CHR
      FILENAMES(6)  = PLTMOD_CHR
      FILENAMES(7)  = VELSUB_CHR
      FILENAMES(8)  = HFEOPF_CHR
      FILENAMES(9)  = PLCALF_CHR
      FILENAMES(10) = AXOSUB_CHR
!
      FILENAMES(11)=SITPL_FIL
      IF ( FILENAMES(6) .EQ. 'NUVEL' .OR. &
     &     FILENAMES(6) .EQ. 'AM0-2' .OR. &
     &     FILENAMES(6) .EQ. 'NUVEL-1A'   ) THEN
           FILENAMES(7) = 'NONE'
        ELSE
           FILENAMES(6) = 'NONE'
      ENDIF
!
!     If OPTIN is only calling to see whether or not the NAMFIL setup
!     is the default setup, skip down to the code which checks this.
!
      IF (IDEFAULT .EQ. 2) GO TO 90
!
!
!     Save filenames for comparison to see if user makes changes.
!
      DO I = 1, NUMFLOPTS
         OFILENAMES(I) = FILENAMES(I)
      END DO
      ofix = nuvel_fixed
!
!     Depending on the user's choice, either
!       call FLEDIT, the subroutine which presents the user with a
!       screen of flyby data and allows him to change it
!     or
!       turn on the default flyby setup.
!
      IF ( IDEFAULT .EQ. 0) THEN
           CALL FLEDIT(COMMAND, FILENAMES, NUMFLOPTS,defaults,nuvel_fixed )
         ELSE
           DO I = 1, NUMFLOPTS
              FILENAMES(I) = DEFAULTS(I)
           END DO
      END IF
!
!     Record any changes to the flyby data, which the user made in
!     FLEDIT.
!
!     Determine whether a change was made.
!
      IF (COMMAND .EQ. 'C') GO TO 90
      CHANGEMADE = .FALSE.
!
      DO I = 1, NUMFLOPTS
        IF (FILENAMES(I) .NE. OFILENAMES(I)) CHANGEMADE = .TRUE.
      END DO
      if (nuvel_fixed.ne.ofix) changemade = .TRUE.
!
      IF ( .NOT. CHANGEMADE ) GOTO 90
!
      CALL USE_COMMON ( 'ORC' )
      CALL USE_PARFIL ( 'ORC' )
      CDUM = ' '
      DDUM = 1.0D0
      CALL FLYBY_INIT ( FILENAMES(1), FILENAMES(2), FILENAMES(3), &
     &                  FILENAMES(4), FILENAMES(5), FILENAMES(6), &
     &                  TIME0, FILENAMES(7), CDUM, CDUM, CDUM, CDUM, DDUM, &
     &                  FILENAMES(10), CDUM, CDUM, CDUM )
      CALL SHFEOP_INT( FILENAMES(8) )
      CALL CHAR2HOL  ( FILENAMES(8), HFEOPF, INT2(1), NAME_SIZE )
      CALL PLOD_CORR ( FILENAMES(9) )
      CALL CHAR2HOL  ( FILENAMES(9), PLCALF, INT2(1), NAME_SIZE )
!
      STASUB_CHR = FILENAMES(1)  
      SRCSUB_CHR = FILENAMES(2)  
      NUTSRS_CHR = FILENAMES(3)  
      NUTDLY_CHR = FILENAMES(4)  
      EOPDLY_CHR = FILENAMES(5)  
      PLTMOD_CHR = FILENAMES(6)  
      VELSUB_CHR = FILENAMES(7)  
      HFEOPF_CHR = FILENAMES(8)  
      PLCALF_CHR = FILENAMES(9)  
      AXOSUB_CHR = FILENAMES(10) 
      SITPL_FIL  = FILENAMES(11)
      FL_NOFLYBY = .TRUE.
!
      IF ( STASUB_CHR(1:4) .NE. 'NONE' .AND. STASUB_CHR(1:1) .NE. ' ' ) FL_NOFLYBY = .FALSE.
      IF ( SRCSUB_CHR(1:4) .NE. 'NONE' .AND. SRCSUB_CHR(1:1) .NE. ' ' ) FL_NOFLYBY = .FALSE.
      IF ( NUTSRS_CHR(1:4) .NE. 'NONE' .AND. NUTSRS_CHR(1:1) .NE. ' ' ) FL_NOFLYBY = .FALSE.
      IF ( NUTDLY_CHR(1:4) .NE. 'NONE' .AND. NUTDLY_CHR(1:1) .NE. ' ' ) FL_NOFLYBY = .FALSE.
      IF ( EOPDLY_CHR(1:4) .NE. 'NONE' .AND. EOPDLY_CHR(1:1) .NE. ' ' ) FL_NOFLYBY = .FALSE.
      IF ( PLTMOD_CHR(1:4) .NE. 'NONE' .AND. PLTMOD_CHR(1:1) .NE. ' ' ) FL_NOFLYBY = .FALSE.
      IF ( VELSUB_CHR(1:4) .NE. 'NONE' .AND. VELSUB_CHR(1:1) .NE. ' ' ) FL_NOFLYBY = .FALSE.
      IF ( HFEOPF_CHR(1:4) .NE. 'NONE' .AND. HFEOPF_CHR(1:1) .NE. ' ' ) FL_NOFLYBY = .FALSE.
      IF ( PLCALF_CHR(1:4) .NE. 'NONE' .AND. PLCALF_CHR(1:1) .NE. ' ' ) FL_NOFLYBY = .FALSE.
      IF ( AXOSUB_CHR(1:4) .NE. 'NONE' .AND. AXOSUB_CHR(1:1) .NE. ' ' ) FL_NOFLYBY = .FALSE.
!
      CALL USE_GLBFIL('W' )
      CALL USE_GLBFIL_4('W' )
!
  90  CONTINUE
!
!     See if the final NAMFIL setup is the default setup. If so, set
!     IREPLY(2) to 1.  Otherwise, set it to 0.
!
        IREPLY(2) = 1
!
!       If the user edited but cancelled changes (C), compare the default
!       files to OFILENAMES, the old file names.  Otherwise, the user
!       wants to record his changes or OPTIN wants to display the NAMFIL;
!       either way, FILENAMES contains the current set up and should be
!       compared to the default files.
!
        IF (COMMAND .EQ. 'C') THEN
          DO I = 1, NUMFLOPTS
            IF (OFILENAMES(I) .NE. DEFAULTS(I)) THEN
              IF (.NOT. ((OFILENAMES(I).EQ.' '.AND.DEFAULTS(I).EQ. 'NONE') .OR. &
     &                   (OFILENAMES(I).EQ.'NONE'.AND.DEFAULTS(I).EQ. ' '))) &
     &                   IREPLY(2) = 0
              END IF
          END DO
        ELSE
          DO I = 1, NUMFLOPTS
            IF (FILENAMES(I) .NE. DEFAULTS(I)) THEN
              IF ( .NOT. ( ( FILENAMES(I) .EQ. ' ' .AND. &
     &                       DEFAULTS(I)  .EQ. 'NONE' ) .OR. &
     &                     ( FILENAMES(I) .EQ. 'NONE' .AND. &
     &                       DEFAULTS(I).EQ. ' ') )          )IREPLY(2) = 0
              END IF
          END DO
        END IF
      call use_glbfil('C' )
!
 50   CONTINUE
      call end_mn()
      IF (COMMAND .EQ. 'Q') THEN
        IREPLY(1) = 1
      ELSE
        IREPLY(1) = 0
      END IF
      IBUFF=0
      if (changemade) CALL USE_COMMON('OWC')
      CALL SBIT( IBUFF, INT2(1), IREPLY(1) )
      CALL SBIT( IBUFF, INT2(2), IREPLY(2) )
      CALL USE_BUFFER( IBUFF, INT2(1), 'WC' )
      CALL END_PROG()
      END
