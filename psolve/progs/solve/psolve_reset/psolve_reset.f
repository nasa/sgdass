      PROGRAM SOLVE_RESET
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
! 1.  SOLVE_RESET PROGRAM SPECIFICATION
!
! 1.1
!   The function of the SOLVE_RESET program is simply to recreate
!   the SOLVE scratch files non-interactively and set some initial values.
!   The program is to be run after the scratch files have
!   been deleted (or before they were ever created) for the
!   set of initials involved.  Running sequence is
!        solve <initials> <max # obs> <max # parameters>
!
!   RESTRICTIONS - The following are dimension limitations which apply
!   to all of the SOLVE programs:
!
!   Number of stations-total                512
!   Number of stations/database              32
!   Number of stars-total                  8192
!   Number of stars/database                512
!   Number of clock polynomials             100 ??
!   Number of atmosphere epochs             160 ??
!   Number of polar motion and UTI epochs    12 ??
!   Number of adjusted parameters         20000
!   Number of observations               100000 
!
! 1.2 REFERENCES:
!
! 2.  SOLVE_RESET INTERFACE
!
! 2.1 Parameter File
      INCLUDE 'solve.i'
      INCLUDE 'fclib.i'
!
! 2.2 INPUT Variables:
!
!
! 2.3 OUTPUT Variables: None
!
! 2.4 COMMON BLOCKS USED
!
      INCLUDE 'glbcm.i'
      INCLUDE 'glbc4.i'
      INCLUDE 'socom.i'
      INCLUDE 'prfil.i'
      INCLUDE 'precm.i'
      INCLUDE 'sareq.i'
      INCLUDE 'diagi.i'
      INCLUDE 'repa.i'
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES: None
!       CALLED SUBROUTINES: psnew,creaq,copy_corfil, chksc
!
! 3.  LOCAL VARIABLES
!
      INTEGER*2 CRT
      PARAMETER (CRT=6)
!
      LOGICAL*4 EX, NRSIZ(2), NSSIZ(2)
      INTEGER*4 NOBS_MAX, NPARMS, IOS, I4
      INTEGER*4 MAT_E, JRND_BLOCKS, FDES, MAKE_SEMA, VBLOCKS
      INTEGER*2 IBUF(JNAMREC_WORDS), I, J, IERR
      CHARACTER CH_BUF(JNAMREC_WORDS)*2
      EQUIVALENCE ( IBUF, CH_BUF )
      INTEGER*2 IUNIT, LIMIT, NUM_PARTIAL, IPARTDEF, KERR
      PARAMETER ( VBLOCKS=2 )
      LOGICAL*4 LEX
!
      CHARACTER FNAME*(NAME_SIZE), CORFI*(NAME_SIZE), CORFO*(NAME_SIZE), &
     &          BLANK*(NAME_SIZE)
      CHARACTER OBS_STRING*12, ERRSTR*80, CSIZE(2)*6, BLANK2*2, CLETRS*2
      CHARACTER DIR_PART*128, FILE_PART*128, ERRSTR128*128
      INTEGER*2 TRM(9), LETRS, ICT, TRIMLEN
      LOGICAL*2 LETOK, POST_EXIST
      INTEGER*8 LEN8_BLOCKS, LEN8_BYTES
      EQUIVALENCE (LETRS,CLETRS)
      CHARACTER  STR*80, GET_VERSION*54
!
      CHARACTER*4 NOBSFL, NSPLFL, NNAMFL, NPARFL, NCOMMN
      CHARACTER*4 NNRMFL, NCGMFL, NCGBFL, NCOVFL, NRESFL
      CHARACTER*4 NPLTFL, NGLBFL, NSARFL, NVERFL, NCONFL
      CHARACTER*8 PARTIAL(112)
      INTEGER*4   NBUF, MBUF, LUN, IER
      PARAMETER  ( MBUF = 256 )
      CHARACTER    BUF(MBUF)*256
!
! Names of scratch files
!
      DATA NOBSFL /'OBSF'/
      DATA NSPLFL /'SPLF'/
      DATA NNAMFL /'NAMF'/
      DATA NPARFL /'PARF'/
      DATA NCOMMN /'COMM'/
      DATA NNRMFL /'NRMF'/
      DATA NCGMFL /'CGMF'/
      DATA NCGBFL /'CGMB'/
      DATA NCOVFL /'COVF'/
      DATA NRESFL /'RESF'/
      DATA NPLTFL /'PLTF'/
      DATA NGLBFL /'GLBF'/
      DATA NSARFL /'SARF'/
      DATA NVERFL /'VERS'/
      DATA NCONFL /'CONF'/
!
      DATA BLANK(1:NAME_SIZE) / ' ' /
      DATA CSIZE(1)  / '      '     /
      DATA CSIZE(2)  / '      '     /
      DATA BLANK2    / '  '         /
!!      DATA NRSIZ     / 0, 35        /
!!      DATA NSSIZ     / 240, 21      /
      DATA CH_BUF    / 35*'  '      /
      CHARACTER  ARG2*64, ARG3*64
      CHARACTER, EXTERNAL :: GET_CDATE*19
      INTEGER*4, EXTERNAL :: ILEN, GET_UNIT, I_LEN
!
! 4.  HISTORY
!
!   WHO   WHEN   WHAT
!   AEE  910611  Added bground=1 since we can only be interactive.
!   MWH  911212  Created for special purpose of recreating scratch files
!   KDB  951005  Set sitpl to default.
!   KDB  951030  Turn off no_net_rotation.  Also turn off input and output cgm
!                variables previously missed.
!   KDB  951108  Parameterize flyby master mod file.
!   kdb  951207  Integer*4 number of observations.
!   kdb  960326  Show the maximum number of parameters if the user exceeds
!                them.
!   kdb  960418  Initialize resoutfile.
!   kdb  960422  Fix error: wouldn't display error msg if initials in use.
!   kdb  960506  Make sure that solve code isn't being installed and
!                that it's okay to run the solve programs.
!   kdb  960524  Fix bug: raise num_part from 6 to 7.
!   kdb  960528  Fix bug: set reqref to true so that reway iterate to unity
!                feature won't iterate to horrendously large value.
!   kdb  960530  Fix typo: was setting kstap to false twice, but not setting
!                kstam.
!   kdb  960530  Make sure to initialize some glbfil variables that are not
!                currently set at all appropriate places (e.g., when resetting
!                the data files via sskedh, reading in a new database,
!                reading in a new superfile interactively).
!   kdb  961112  Remove hard coding of partial list.
!                Initialize part_applied.
!   pet  980617  Made some minor changes to force it to work under HP-10.20
!   pet  1999.11.02  Fixed a bug related with new structure of pvser.i
!   pet  2000.04.26  Imroved comments. Removed unused variables. Changed
!                    MIN_ET(1)  from 15 psec to 10 psec
!   pet  2000.10.06  Added creation of CONFxx -- configuration file. It will
!                    keep date of creation and parameters of scratch fiels:
!                    maximal number of observations and maximal number of
!                    parameters.
!   pet  2000.10.23  Added checks of existence of 6 files: CORFIL,
!                    FLYBY_DEFAULTS, SOLMOD, ECCDAT.ECC, flyby_calibrations,
!                    partial_calibrations. If they don't exist, then
!                    solve_reset would refuse to continue its work
!   pet  2000.11.22  Replaced DEFVELCNST, ALTVELCNST with
!                    STAXYZ_CNST, STAUEN_CNST, VELXYZ_CNST, VELUEN_CNST
!   pet  2000.12.28  A rediculous bug was wixed: some INQUIRE statements were
!                    lost (noticed by C. Klatt).  Added call w_trm for saving
!                    terminal properties.
!   pet  2003.07.24  Added creation of REPAxx file
!   pet  2004.10.27  Got rid of curses and replace RMPAR with GETARG, else
!                    it was not possible correctly to set the masimal number
!                    of observations more than 32767
!   pet  2004.12.15  Added initialization or repa and repab configuration files
!
! 5.  SOLVE_RESET PROGRAM STRUCTURE
!
!
! --- Get user initials, #obs, # parms from RMPAR
!
      INCLUDE 'psolve_reset_version.i' ! Set revision date of the current version
      CLETRS = '  '
      CALL GETARG ( 1, CLETRS )
      CALL TRAN   ( 11, CLETRS, CLETRS )
      CALL GETARG ( 2, ARG2 )
      CALL GETARG ( 3, ARG3 )
      CALL CHIN   ( ARG2,  NOBS_MAX )
      CALL CHIN   ( ARG3,  NPARMS   )
      STR = GET_VERSION()
      WRITE ( 6, '(A)' ) STR(1:I_LEN(STR))//'  Create a new set of '// &
     &              'SOLVE scratch files'
      IF ( CLETRS .EQ. '  '  .OR.  &
     &     NOBS_MAX .LE. 0   .OR.  &
     &     NPARMS   .LE. 0         ) THEN
!
           WRITE ( 6, 110 )
 110       FORMAT(/"Usage:  psolve_reset xx max_obs max_parms"// &
     &             "   Where        xx = user's initials"/ &
     &             "           max_obs = maximum number of observations"/ &
     &             "         max_parms = maximum number of parameters"/ &
     &             "   Enter solve_reset xx to learn your previous settings" )
           IF ( ILEN(CLETRS) .GE. 2 ) THEN
                CALL CLRCH ( FNAME )
                CALL SETUP_PRELUDE ( CLETRS )
                FNAME=PRE_SCR_DIR(1:PRE_SD_LEN)//'CONF'//PRE_LETRS
                INQUIRE ( FILE=FNAME, EXIST=LEX )
                IF ( LEX ) THEN
                     WRITE ( UNIT=6, FMT='(A)' ) ' '
                     WRITE ( UNIT=6, FMT='(A)' ) 'Your previous settings:'
                     WRITE ( UNIT=6, FMT='(A)' ) '~~~~~~~~~~~~~~~~~~~~~~~'
                     CALL SYSTEM ( 'cat '//FNAME(1:I_LEN(FNAME))//CHAR(0) )
                END IF
           END IF
           WRITE ( UNIT=6, FMT='(A)' ) ' '
           WRITE ( UNIT=6, FMT='(A)' ) 'Scratch files WERE NOT updated'
           CALL FLUSH ( 6 )
           CALL EXIT  ( 0 )
      ENDIF
!
! --- Get the 2nd parameter in the run string - the initails
!
      CALL CASEFOLD ( CLETRS )
      IF ( .NOT.  LETOK ( CLETRS ) ) THEN
           WRITE ( 6, '(A)' ) 'SOLVE_RESET: Invalid initials "'//CLETRS// &
     &                        '" -- try again'
           CALL EXIT ( 1 )
      END IF
!
! --- Set up prelude common by hand for now
!
      CALL SETUP_PRELUDE ( LETRS )
      CALL SET_TESTV ( .FALSE. )
      CALL SET_SPOOL ( .TRUE.  )
!
      CALL MAKE_PIPES()
!
! --- Try to put a lock on user initials; if already locked, display message
! --- and stop
!
!@      FDES = MAKE_SEMA ( PRE_SCR_DIR(:PRE_SD_LEN)//'LOCK'//PRE_LETRS, 'W', 'N' )
!@      IF ( FDES .EQ. -1 ) THEN
!@           WRITE ( ERRSTR, '("Initials are in use.") ' )
!@           CALL FERR ( INT2(697), ERRSTR, INT2(0), INT2(0) )
!@      ENDIF
!
! --- Save terminal properties
!
      CALL SAVE_TERM ( TRM )
      CALL W_TRM ( PTR_CH ( PRE_SCR_DIR(:PRE_SD_LEN)//'term'//PRE_LETRS// &
     &                      CHAR(0) ), TRM )
!
      IF ( NOBS_MAX .GT. MAX_OBS ) THEN
           WRITE ( 6, * ) ' NOBS_MAX = ',NOBS_MAX
           WRITE ( ERRSTR, '("Exceeded maximum of ",I10," obs")') MAX_OBS
           CALL FERR ( INT2(698), ERRSTR, INT2(0), INT2(0) )
      ENDIF
      IF ( NOBS_MAX .LE. 0 ) NOBS_MAX = 1000
!
! --- Be sure obsfil and resfil are gone
!
      CALL BIN_UNLINK ( PRE_SCR_DIR(1:PRE_SD_LEN)//NOBSFL//CLETRS, IERR )
      CALL BIN_UNLINK ( PRE_SCR_DIR(1:PRE_SD_LEN)//NRESFL//CLETRS, IERR )
!
      IF ( NPARMS .GT. M_GPA ) THEN
           WRITE ( ERRSTR, '("Exceeded maximum of ",I7," parms")' ) M_GPA
           CALL FERR ( INT2(699), ERRSTR, INT2(0), INT2(0) )
      ENDIF
!
      IF ( NPARMS .LE. 0 ) NPARMS = 512
!
      WRITE ( 6, 120 ) NOBS_MAX, NPARMS, CHAR(13)
 120  FORMAT ( 'solve_reset:  started creating scratch files for'/ &
     &         '              max_obs: ',I7 / &
     &         '              max_par: ',I7 / &
               '  working ... ',A$ )
      CALL FLUSH ( 6 ) 
!
      CALL BIN_UNLINK ( PRE_SCR_DIR(1:PRE_SD_LEN)//NNRMFL//CLETRS, IERR )
      CALL BIN_UNLINK ( PRE_SCR_DIR(1:PRE_SD_LEN)//NCGMFL//CLETRS, IERR )
      CALL BIN_UNLINK ( PRE_SCR_DIR(1:PRE_SD_LEN)//NCGBFL//CLETRS, IERR )
      CALL BIN_UNLINK ( PRE_SCR_DIR(1:PRE_SD_LEN)//NCOVFL//CLETRS, IERR )
      CALL BIN_UNLINK ( PRE_SCR_DIR(1:PRE_SD_LEN)//NGLBFL//CLETRS, IERR )
      CALL BIN_UNLINK ( PRE_SCR_DIR(1:PRE_SD_LEN)//NVERFL//CLETRS, IERR )
      CALL BIN_UNLINK ( PRE_SCR_DIR(1:PRE_SD_LEN)//NCONFL//CLETRS, IERR )
!
      IUNIT = 303
      LIMIT = 112
      DIR_PART  = PRE_SAV_DIR(:PRE_SV_LEN)
      FILE_PART = AVAL_PART_FILE//CHAR(0)
      CALL ATMAVL_N ( IUNIT, LIMIT, DIR_PART, FILE_PART, PARTIAL, NUM_PARTIAL, &
     &                IPARTDEF, KERR )
!
      IF ( KERR.NE.0 .OR. NUM_PARTIAL .EQ. 0) THEN
          IF ( KERR .NE. 0) THEN
               IF ( KERR .EQ. -1 ) THEN
                    ERRSTR128 = 'Error in openning file '// &
     &                           DIR_PART(1:TRIMLEN(DIR_PART))//FILE_PART
                    CALL FERR ( KERR, ERRSTR128, INT2(0), INT2(0) )
                 ELSE
                    ERRSTR128 = 'solve_reset error:  from atmavl_n'
                    CALL FERR ( KERR, ERRSTR128, INT2(0), INT2(0) )
               END IF
             ELSE
               ERRSTR128 = 'solve_reset error: part cal avail file empty = '// &
     &                      DIR_PART(1:TRIMLEN(DIR_PART))//'/'//FILE_PART
               CALL FERR ( INT2(401), ERRSTR128, INT2(0), INT2(0) )
          END IF
      ENDIF
!
! --- Create the scratch files required by the remainder of the
! --- SOLVE programs. Of the common block type files, the length parameter
! --- (fourth in the calling sequence) indicates the number of blocks
! --- of 128 words required to hold the common block.  In file OBSFIL
! --- the length specified is four times the number of observations.
!
      CALL CREAQ ( NCOMMN, IERR, INT8(JSOCOM_BLOCKS) )
!
! --- Create and Initialize file VERSxx
!
      CALL CREAQ       ( NVERFL, IERR, INT8(JPVERS_BLOCKS) )
      CALL SET_VERSION ( '?????', '??????????', '??' )
!
! --- Create a new CORFIL from a copy of the standard
!
      CALL CREAQ ( 'CORF', IERR, INT8(0) )
      IF ( IERR .EQ. 0 ) THEN
           CORFI=PRE_SAV_DIR(:PRE_SV_LEN)//'CORFIL'
           CORFO=PRE_SCR_DIR(1:PRE_SD_LEN)//'CORF'//pre_letrs
           INQUIRE ( FILE=CORFI, EXIST=LEX )
           IF ( .NOT. LEX ) THEN
                CALL ERR_LOG ( 701, 1, 'SOLVE_RESET', 'Generic file '// &
     &               CORFI(1:I_LEN(CORFI))//' was not found. Refer to '// &
     &               CORFI(1:I_LEN(CORFI))//'.template ' )
                CALL EXIT ( 2 )
           END IF
           CALL COPY_CORFIL ( CORFI, CORFO )
      END IF
!
! --- Create a new flyby modfile/default file from a copy of the standard
!
      CALL CREAQ ( 'FDEF', IERR, INT8(0) )
      IF ( IERR .EQ. 0 ) THEN
           CORFI=PRE_SAV_DIR(:PRE_SV_LEN)//FLYBY_DEFAULTS
           CORFO=PRE_SCR_DIR(1:PRE_SD_LEN)//'FDEF'//PRE_LETRS
           INQUIRE ( FILE=CORFI, EXIST=LEX )
           IF ( .NOT. LEX ) THEN
                CALL ERR_LOG ( 702, 1, 'SOLVE_RESET', 'Generic file '// &
     &               CORFI(1:I_LEN(CORFI))//' was not found. Refer to '// &
     &               CORFI(1:I_LEN(CORFI))//'.template ' )
                CALL EXIT ( 2 )
           END IF
           CALL COPY_FDEF ( CORFI, CORFO )
      END IF
!
! --- Create a new repa configuration file if it does not yet exist
!
      CALL CREAQ ( 'REPA', IERR, INT8(0) )
      IF ( IERR .EQ. -1 ) THEN
!
! -------- Check, whether the control file for REPA is obsolete
!
           FNAME = PRE_SCR_DIR(1:PRE_SD_LEN)//'REPA'//PRE_LETRS
           CALL CLRCH   ( BUF(1) )
           IER = 0
           CALL RD_TEXT ( FNAME, MBUF, BUF, NBUF, IER )
           IF ( BUF(1)(1:LEN(REPA__CNF_2004)) == REPA__CNF_2004 ) THEN
                IERR = 0
           END IF
      END IF
!
      IF ( IERR .EQ. 0 ) THEN
           CORFI=PRE_SAV_DIR(:PRE_SV_LEN)//'repa_template'
           CORFO=PRE_SCR_DIR(1:PRE_SD_LEN)//'REPA'//PRE_LETRS
           INQUIRE ( FILE=CORFI, EXIST=LEX )
           IF ( .NOT. LEX ) THEN
                CALL ERR_LOG ( 703, 1, 'SOLVE_RESET', 'Generic file '// &
     &               CORFI(1:I_LEN(CORFI))//' was not found' )
                CALL EXIT ( 2 )
           END IF
           CALL COPY_FDEF ( CORFI, CORFO )
      END IF
!
! --- Create a new repa status file if it does not yet exist
!
      CALL CREAQ ( 'RPST', IERR, INT8(0) )
      IF ( IERR .EQ. -1 ) THEN
!
! -------- Check, whether the control file for REPA is obsolete
!
           FNAME = PRE_SCR_DIR(1:PRE_SD_LEN)//'RPST'//PRE_LETRS
           CALL CLRCH   ( BUF(1) )
           IER = 0
           CALL RD_TEXT ( FNAME, MBUF, BUF, NBUF, IER )
           IF ( BUF(1)(1:LEN(REPA__STS_2004)) == REPA__STS_2004 ) THEN
                IERR = 0
           END IF
      END IF
      IF ( IERR .EQ. 0 ) THEN
           CORFI=PRE_SAV_DIR(:PRE_SV_LEN)//'repa_stat_template'
           CORFO=PRE_SCR_DIR(1:PRE_SD_LEN)//'RPST'//PRE_LETRS
           INQUIRE ( FILE=CORFI, EXIST=LEX )
           IF ( .NOT. LEX ) THEN
                CALL ERR_LOG ( 704, 1, 'SOLVE_RESET', 'Generic file '// &
     &               CORFI(1:I_LEN(CORFI))//' was not found' )
                CALL EXIT ( 2 )
           END IF
           CALL COPY_FDEF ( CORFI, CORFO )
      END IF
!
! --- Create a new repab configuration file if it does not yet exist
!
      CALL CREAQ ( 'REPAB', IERR, INT8(0) )
      IF ( IERR .EQ. 0 ) THEN
           CORFI=PRE_SAV_DIR(:PRE_SV_LEN)//'REPAB_template'
           CORFO=PRE_SCR_DIR(1:PRE_SD_LEN)//'REPAB'//PRE_LETRS
           INQUIRE ( FILE=CORFI, EXIST=LEX )
           IF ( .NOT. LEX ) THEN
                CALL ERR_LOG ( 705, 1, 'SOLVE_RESET', 'Generic file '// &
     &               CORFI(1:I_LEN(CORFI))//' was not found' )
                CALL EXIT ( 2 )
           END IF
           CALL COPY_FDEF ( CORFI, CORFO )
      END IF
!
! --- Check availability of eccentricity file
!
      CALL CLRCH ( CORFI )
      CORFI = PRE_SAV_DIR(:PRE_SV_LEN)//ECC_DATA_FILE
      INQUIRE ( FILE=CORFI, EXIST=LEX )
      IF ( .NOT. LEX ) THEN
           CALL ERR_LOG ( 706, -1, 'SOLVE_RESET', 'Generic file '// &
     &          CORFI(1:I_LEN(CORFI))//' was not found' )
           CALL EXIT ( 2 )
      END IF
!
! --- Check availability of flyby calibration file
!
      CALL CLRCH ( CORFI )
      CORFI = PRE_SAV_DIR(:PRE_SV_LEN)//AVAL_FCAL_FILE
      INQUIRE ( FILE=CORFI, EXIST=LEX )
      IF ( .NOT. LEX ) THEN
           CALL ERR_LOG ( 707, -1, 'SOLVE_RESET', 'Generic file '// &
     &          CORFI(1:I_LEN(CORFI))//' was not found. Refer to '// &
     &          CORFI(1:I_LEN(CORFI))//'.template ' )
           CALL EXIT ( 2 )
      END IF
!
! --- Check availability of atmosphere partials file
!
      CALL CLRCH ( CORFI )
      CORFI = PRE_SAV_DIR(:PRE_SV_LEN)//AVAL_PART_FILE
      INQUIRE ( FILE=CORFI, EXIST=LEX )
      IF ( .NOT. LEX ) THEN
           CALL ERR_LOG ( 708, -1, 'SOLVE_RESET', 'Generic file '// &
     &          CORFI(1:I_LEN(CORFI))//' was not found. Refer to '// &
     &          CORFI(1:I_LEN(CORFI))//'.template ' )
           CALL EXIT ( 2 )
      END IF
!
! --- Check availability of SOLMOD file
!
      CALL CLRCH ( CORFI )
      CORFI = PRE_SAV_DIR(:PRE_SV_LEN)//'SOLMOD'
      INQUIRE ( FILE=CORFI, EXIST=LEX )
      IF ( .NOT. LEX ) THEN
           CALL ERR_LOG ( 709, -1, 'SOLVE_RESET', 'Generic file '// &
     &          CORFI(1:I_LEN(CORFI))//' was not found. Refer to '// &
     &          CORFI(1:I_LEN(CORFI))//'.template ' )
           CALL EXIT ( 2 )
      END IF
!
! --- Create a new Solve configuration file:
!
      CALL CREAQ ( NCONFL, IERR, INT8(0) )
      LUN = GET_UNIT()
      OPEN ( UNIT=LUN, FILE=PRE_SCR_DIR(1:PRE_SD_LEN)//NCONFL//CLETRS, &
     &       STATUS='UNKNOWN' )
!
! --- Write down configuration
!
      STR = GET_CDATE()
      WRITE ( LUN, '(A)'    ) 'Creation_date: '//STR(1:19)
      WRITE ( LUN, '(A,I7)' ) 'Max_obs:       ', NOBS_MAX
      WRITE ( LUN, '(A,I7)' ) 'Max_par:       ', NPARMS
      CLOSE ( UNIT=LUN )
!
      CALL CREAQ ( NPARFL, IERR, INT8(JPARFIL_BLOCKS) )
!
      LEN8_BYTES  = 8*(3*M_GPA + INT8(NPARMS)*INT8(NPARMS+1)/2)
      LEN8_BLOCKS = (LEN8_BYTES + INT8(255))/INT8(256)
      CALL CREAQ ( NNRMFL, IERR, LEN8_BLOCKS )
      CALL CREAQ ( NPLTFL, IERR, INT8(JPLTFIL_BLOCKS) )
      CALL CREAQ ( NRESFL, IERR, INT8(JRND_BLOCKS(JRESREC_WORDS*NOBS_MAX)) )
      CALL CREAQ ( NOBSFL, IERR, INT8(JRND_BLOCKS(JOBSREC_WORDS*NOBS_MAX)) )
!
      INQUIRE ( FILE=PRE_SCR_DIR(1:PRE_SD_LEN)//'SARF'//PRE_LETRS, &
     &          EXIST=EX)
      CALL CREAQ ( NSARFL, IERR, INT8(24) )
      IF ( .NOT. EX ) CALL ACS_SARFIL ( 'OISC' )
!
      INQUIRE ( FILE=PRE_SCR_DIR(1:PRE_SD_LEN)//'STAT'//PRE_LETRS, &
     &          EXIST=EX)
      IF ( .NOT. EX ) THEN
           LUN = GET_UNIT()
           OPEN ( FILE=PRE_SCR_DIR(1:PRE_SD_LEN)//'STAT'//PRE_LETRS, &
     &            UNIT=LUN, STATUS='NEW', IOSTAT=IER )
           CALL CLRCH ( STR   ) 
           WRITE ( LUN, '(A)' ) STR
           CLOSE ( UNIT=LUN   )
      END IF
!
      CALL SOLVE_LOCK()  
!
! --- Create the post-APR1986 style NAMFIL
!
      FNAME = PRE_SCR_DIR(1:PRE_SD_LEN)//NNAMFL//CLETRS
      INQUIRE ( FILE=FNAME, IOSTAT=IOS, EXIST=EX )
      CALL FERR ( INT2(IOS), 'inquiring '//FNAME, INT2(0), INT2(0) )
!
! --- Blank out the 1st record in NAMFIL
!
      IF ( .NOT. EX ) THEN
           OPEN ( UNITNAM, FILE=FNAME, IOSTAT=IOS, ACCESS='DIRECT', &
     &            RECL=JNAMREC_WORDS*2 )
           CALL FERR ( INT2(IOS), 'opening '//FNAME, INT2(0), INT2(0) )
           WRITE ( UNITNAM, IOSTAT=IOS, REC=1 ) (IBUF(J),J=1,JNAMREC_WORDS)
           CALL FERR ( INT2(IOS), 'writing '//FNAME, INT2(0), INT2(0) )
           CLOSE ( UNITNAM, IOSTAT=IOS )
           CALL FERR ( INT2(IOS), 'closing '//FNAME, INT2(0), INT2(0) )
      ENDIF
!
      CALL CREAQ ( NGLBFL, IERR, INT8(JGLBFIL_BLOCKS) )
!
      CALL USE_GLBFIL('OR' )
      CALL CHAR2HOL ( 'NONE', STASUB, INT2(1), NAME_SIZE )
      CALL CHAR2HOL ( 'NONE', SRCSUB, INT2(1), NAME_SIZE )
      CALL CHAR2HOL ( 'NONE', NUTSRS, INT2(1), NAME_SIZE )
      CALL CHAR2HOL ( 'NONE', NUTDLY, INT2(1), NAME_SIZE )
      CALL CHAR2HOL ( 'NONE', EOPDLY, INT2(1), NAME_SIZE )
      CALL CHAR2HOL ( 'NONE', VELSUB, INT2(1), NAME_SIZE )
      CALL CHAR2HOL ( 'NONE', AXOSUB, INT2(1), NAME_SIZE )
      KSTAP = .FALSE.
      KSOUC = .FALSE.
      KNUTS = .FALSE.
      KNUTD = .FALSE.
      KEROT = .FALSE.
      KSVEL = .FALSE.
      KSTAM = .FALSE.
      SITPL_FIL = SITPL_FILE
      META_SUP = .FALSE.
      CALL USE_GLBFIL_4('W' )
      DO I=1,JGLBCM_WORDS
         IGLBCM(I)=0
      ENDDO
      LMNAM_CH(1:2) = '::'
      NRMFL_PARMS=NPARMS
      NRECD=NOBS_MAX
      INAMCG=BLANK
      ONAMCG=BLANK
      DO I=1,3
        ARCDIR(I)=BLANK
      ENDDO
      IOCGM = 0
      ICONT = 0
      ISOLU = 0
      REQREF = .TRUE.
!
      CALL USE_GLBFIL_4 ( 'R' )
      DO I= 1,NUM_PARTIAL
         PART_ARRAY(I)= PARTIAL(I)
      END DO
      NUM_PART     = NUM_PARTIAL
      PART_APPLIED = IPARTDEF
      KIONO        = .TRUE.
      KBEEP        = .FALSE.
      KELDEP_NOISE = .FALSE.
      RESOUTFILE   = 'NONE'
      KCENTERMASS  = .FALSE.
      ELDEP_FILE   = ' '
      SOURCE_WEIGHT_FILE = ' '
!
! --- Initialize default mimimun reweights
!
      MIN_ET(1) =  10.D-12          ! Group delay 10 ps
      MIN_ET(2) = 100.D-15          ! Phase delay rate 100 fs/s
      MIN_ET(3) =   5.D-12          ! Phase delay  5 ps
      MIN_ET(4) =   0.D0            ! Not currently used.
!
! --- Turn off constraints on station positions and velocitites
!
      DO I = 1,3
         STAXYZ_CNST(I) = 0.0D0
         STAUEN_CNST(I) = 0.0D0
         VELXYZ_CNST(I) = 0.0D0
         VELUEN_CNST(I) = 0.0D0
      ENDDO
      DO ICT = 1,STA_BIT_WORDS
         STAXYZ_CNSB(ICT) = 0
         STAUEN_CNSB(ICT) = 0
         VELUEN_CNSB(ICT) = 0
         VELUEN_CNSB(ICT) = 0
      END DO
      STAXYZ_CNFL = .FALSE.
      STAUEN_CNFL = .FALSE.
      VELXYZ_CNFL = .FALSE.
      VELUEN_CNFL = .FALSE.
!
! --- Turn off the pressure loading feature.
!
      CALL CHAR2HOL ( 'NONE', PLCALF, INT2(1), NAME_SIZE )
      KPLODCAL = .FALSE.
!
! --- This will turn off the high frequency earth orientation calibrations
! --- a little later.  (The decision to turn these on or off is arbitrary,
! --- because the paths that need calibrations (e.g., reading in a database)
! --- will turn them on.  So the calibrations will be turned off here,
! --- because that is easiest.)
!
      CALL CHAR2HOL ( 'NONE', HFEOPF, INT2(1), NAME_SIZE )
      CALL USE_GLBFIL_4 ( 'W' )
!
      KUSER_PART      = .FALSE.
      USER_PART_PROG  = ' '
      KUSER_CONST     = .FALSE.
      USER_CONST_PROG = ' '
      KSRC_CONST      = .FALSE.
      ALL_SIM_FLG     = .FALSE.
      NUVEL_FIXED     = 'NONE'
      CALL USE_GLBFIL ( 'WC' )
!
! --- Do a final pass to make sure all glbfil variables important to the
! --- interactive mode are set properly.  (This may reset some previously
! --- set variables, because this is an effort to set problem variables
! --- that have been set in some places, but not in others.)
!
      CALL INIT_GLB_CM4 ( HFEOPF_CHR )
!
! --- New scratch files
!
      CALL USE_COMMON('O' )
      DO I4=1,JSOCOM_WORDS
         ISOCOM(I4) = 0
      ENDDO
      NUMOBS = 0
      NUMSTA = 0
      NUMSTR = 0
!
! --- Write socom
!
      CALL USE_COMMON('WC' )
!
      CALL USE_COMMON('OR' )
      BGROUND = 1 ! using foreground only when interactive. 6/11/91, AEE
      CALL USE_COMMON('WC' )
!
! --- New PARFIL -- must zero out NUMSEL, VSITED
!
      NUMSEL = 0
      DO I=1,MAX_STA
         VSITED(I) = 0.D0
      ENDDO
      CALL USE_PARFIL ( 'OWC' )
!
      CALL REMOVE_SOLVE_LOCK()  
      WRITE ( 6, '(A)' ) "New SOLVE scratch files have been created"
      END  !#!  SOLVE_RESET  #!#
