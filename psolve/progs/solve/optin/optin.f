       PROGRAM OPTIN
!
!      ******************************************************************
!      *                                                                *
!      * OPTIN IS THE PROGRAM, WHICH WILL ALLOW THE USER TO SELECT WHAT *
!      * TO DO FROM VARIOUS OPTIONS                                     *
!      *                                                                *
!      ******************************************************************
!
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
      INCLUDE  'solve.i'
      INCLUDE  'socom.i'
      INCLUDE  'precm.i'
      INCLUDE  'glbcm.i'
      INCLUDE  'buff2.i'
      INCLUDE  'prfil.i'
      INCLUDE  'glbc3.i'
      INCLUDE  'glbc4.i'
      INCLUDE  'fast.i'
      INCLUDE  'vtd.i'
      INCLUDE  'fclib.i'
      INCLUDE  'flyby.i'
!
      INTEGER*2  IFLYREP(2), KBITN, IACCOR(2), I, J, ICONTI, ICRX, IFLYDEF, &
     &           IFLYDEF_SAVE, ISPOOL, ISQR, ITERM, IX, IY, &
     &           KPAGE, NUMDB, CMENU, KERR, LETTER_I
      LOGICAL*2  KBIT, KDEFAULT, KEXIST, NEED_2, KSTON, ltest
!
      CHARACTER  SITE_FLYBY*14,   SOURCE_FLYBY*14, NUTSRS_FLYBY*14, &
     &           NUTDLY_FLYBY*14, EOPDLY_FLYBY*14, STAVEL_FLYBY*14, &
     &           PLATE_MODEL*14,  HFEOP_FLYBY*14,  PLOD_FLYBY*14, &
     &           AXOF_FLYBY*14
      CHARACTER  FB_DEF_BUF*80, ERRSTR*80, BUFSTR*79, VLBI_DIR*128, SPL_FILE*128, &
     &           LTMP*1, STR*128, STR1*10, STR2*10
      CHARACTER  LINES_STR*16, COLUMNS_STR*16
      SAVE       LINES_STR, COLUMNS_STR  !  Very important trick. Else putenv
!                                        !  will not work properly!
      INTEGER*2  IPASS(64)
      INTEGER*4  IXX, IYY, ICHAR4, SCLEN, SCWID
      INTEGER*2  EVSTARTX, TRIMLEN, JCHAR
      CHARACTER  LNAME*128, CCHAR*4, PARXC_FLAG*1
      LOGICAL*2  TRUE_L2
      EQUIVALENCE ( ICHAR4, CCHAR )
      DATA ISPOOL / 0 /
      DATA KSTON  / .FALSE. /
      CHARACTER  FIL_PREF(4)*4
      DATA       &
     &           FIL_PREF/ &
     &             'CNQL', &
     &             'CNQG', &
     &             'ULCC', &
     &             'UGLC' &
     &           /
      PARAMETER  ( TRUE_L2 = .TRUE. )
      INTEGER*4    J1, J2, J3, J4, J5, J6, J7, IP, NC, IOS, ILN, IER, SIZEOF_VTD
      INTEGER*8        MEM_LEN
      ADDRESS__TYPE :: MEM_ADR
      INTEGER*4     MIN_SCLEN, MAX_SCLEN, MIN_WIDTH, SCLEN__OLD, SCWID__OLD
      INTEGER*4     FM_POI, FD_POI
      PARAMETER   ( MIN_SCLEN = 23, MAX_SCLEN = 41, MIN_WIDTH = 80 )
      REAL*8        RL, DDUM, VAL_R8
      INTEGER*2     LDBNAM(5,15), IDBV(15)
      INTEGER*4     MC, MIND
      PARAMETER  ( MC = 512, MIND = 16 )
      INTEGER*4     IDBE(15), LIND, IND(2,MIND)
      CHARACTER     CDBNAM(15)*10, FNAME*128, CDUM*128, VTD_CONF_SES_SAVE*256, &
     &              BUF(MC)*128, SAVE_DIR*128
      EQUIVALENCE ( CDBNAM, LDBNAM(1,1) )
      LOGICAL*4  LEX
      LOGICAL*4  BAD_OBS, DATYP_INQ
      CHARACTER  GET_VERSION*54
      INTEGER*2  ICRES, IBUFF(64)
      TYPE   ( VTD__TYPE          ), POINTER :: VTD_PTR
      INTEGER*8    STACK_SIZE_IN_BYTES, GB, IS
      PARAMETER  ( GB = 1024*1024*1024 )
      PARAMETER  ( STACK_SIZE_IN_BYTES = PSOLVE__STACK_SIZE_IN_GIGABYTES * GB )
      INTEGER*4  IUER
      INTEGER*8, EXTERNAL :: SET_STACKSIZE 
      INTEGER*4, EXTERNAL :: ILEN, I_LEN
!CCCCC
!
!     WHO  WHEN     WHAT
!     AEE  920108   Removed K option (Herring's Kalman filter); never used
!                   and caused bus error.
!     AEE  920204   Use "FLYBY_DEFAULTS" from solve.i to open "FLYBY_DEFAULTS"
!                   file.
!     AEE  920521   Added "/partials" to the "%" option line of optin menu.
!     AEE  920628   added default (chwtpart) in case no partial is selected.
!     MWH  920706   Added data decimation option and display current work dir
!     MWH  920916   Implement curses for keyboard/screen I/O
!     MWH  931216   Rearrange menu format; use single page if window is large
!                   enough; remove dialog for spool, eop plot file setting
!     :94.01.24:jwr:Active intials listed.
!     KDB  960416   Convert hardcoded date to sccs-based date.
!                   Add sccsid parameter for version tracking.
!     KDB  960510   < will turn on the site xyz parameters.
!     KDB  960529   Switch symbols for user partials toggling (!)
!                   and site position parameter setting (<).
!     KDB  960529   Disable option D, which accesses differencing (the quikd
!                   program).  Add program flow notes.
!     PET  970128   Add FAST_MODE (|) and FAST_DBG (") options. Alligned the
!                   second screen form
!     KDB  970207   Disable old weigh call (option A), which became obsolete
!                   due to site weighting
!     PET  970314   Add new automatic ambiguity resolution call (A).
!                   Rearranged screen. Provided feature of proper work while
!                   screen is resized
!     PET  970522   Add new option ({) -- full listing mode as Jim Ryan
!                   proposed.
!     PET  970604   Changed logic for B3D initialization
!     kdb  970613   bug fixed: GLFBxx should be saved before REWAY and read
!                   after it.
!     pet  970711   Initiailzation of CHI_TOL and TIGHTNESS added
!     pet  970807   Rearranged menu items. Added item D (phase delay resolution)
!                   and (\) -- automatic outliers elimination/restoration
!                   Added block for GAMB global variable initialiization
!     pet  970808   Added special routines FAST_DEFAULT, REWAY_DEFAULT,
!                   GAMB_DEFAULT frosetting default values for manu items for
!                   OPTIN (FAST), REWAY and GAMB
!     pet  970810   Added special initialisation routine IONO_DEFAULT
!     pet  971810   Added special initialisation routine SOLVE_DEFAULT
!     pet  971810   Added special initialisation routine SETFL_DEFAULT
!     pet  970917   Added special initialisation routine ELIM_DEFAULT
!     pet  970925   Added automatic scheduling CRES if ELIM changed the solution
!     pet  970926   Fixed bug for option M (No net translation constraint)
!     pet  971029   Added test before calling REWAY: was the database to be
!                   processed by REWAY previouly processed by
!                   PROC-NORML-CRES-ADJST and whether it produced residuals
!     pet  971031   Added analogous test before calling MDLPL
!     pet  980218   Found erroneous mixture of menu items for flyby warning and
!                   default flyby options. Fixed it.
!     KDB  980220   Move ksinex from socom.i to glbcm.i.
!     KDB  980302   Convert > option (sinex setting) so that it simply toggles
!                   the sinex output flag, rather than also starting a
!                   solution.  This standardizes the handling of the sinex flag
!                   as well as correcting gaps in the turning off of the flag.
!     pet  980331   Removed option (!) toggle site positions and put option
!                   (>) Sinex output to that place.
!     pet  980401   Forced to switch off no_net_translation constrains when
!                   SINEX-output is on and vice versus: to switch off
!                   SINEX-output when no_net_translation constrains is
!                   switched on.
!     pet  980505   Added calls of CRES_DEFAULT, CNPLT_DEFAULT
!     pet  980710   Added call  of PROC_DEFAULT
!     pet  990406   Added flipping G_WARNING synchronously with flipping
!                   FLYBY_WARNING
!     pet  1999.07.23  Added initialization of WEB_DIR, MDLPL_IPS_PAG,
!                      MDLPL_IPC_PAG, MDLPL_FL_EOPMOD, MDLPL_FL_CLF
!     pet  2000.03.05  Added check flag NORATE_FLAG before calling REWAY:
!                      REWAY requires NORATE_FLAG NO only.
!     pet  2000.03.09  Added support of the case when FC_GWINSZ, FC_GWINW are
!                      unable to determine actual length and with of the screen:
!                      environment variables LINES, COLUMNS are read. If they
!                      are not specified then defauls (80x23) are assumed.
!     pet  2000.03.28  Added writing a flag in buffer for ELIM 3.0
!     pet  2001.05.02  Added ability to search of user programs in the current
!                      directory AND in the directory SOLVE_DIR.
!     pet  2002.09.25  Forced removal of stale files with constraint equations
!     jwr  2003.03.12  test introduced to logical expression in call.
!     pet  2003.09.29  Corrected logic of setting user partial/user program
!     pet  2005.03.16  Added initialization L_HPE, L_SPE, L_BSP, ADR_HPE, 
!                      ADR_SPE, ADR_BSP
!     pet  2006.01.18  Added initialization of parameters related to ERM &
!                      estimation
!     pet  2010.01.24  Added support of "Update theoretical option"
!     pet  2022.08.21  Added support of ionosphere scale paratemeter estimation and constraint
!
!CCCCC
!
!     Program flow notes:
!     as of 5/29/96, the sections labelled with the following numbers do
!     the following tasks.
!
!     50   write page 1
!     1011 write page 2
!         (if the two pages are combined on a big terminal, optin calls 50,
!          then 1011)
!     200 cursor sensing for page 1 (including "page 2" functions on a
!         combined page)
!     1110 cursor sensing for page 2
!     222  character sensing for both pages. (All cursor sensing is converted
!          to character input and processed in this section.)
!
! --- Set stacksize
!
      IS = SET_STACKSIZE ( %VAL(STACK_SIZE_IN_BYTES) )
      CALL INCH8    ( STACK_SIZE_IN_BYTES/INT8(1024), STR )
      CALL SETENV   ( 'GOMP_STACKSIZE'//CHAR(0), TRIM(STR)//CHAR(0), %VAL(1) )
!
      IDBE = 1
      CALL PRE_PROG()
      CALL SET_VERSION ( 'OPTIN', '????.??.??', '??' ) ! Initialization
      INCLUDE 'optin_version.i' ! Set revision date of the current version
      CALL SET_SIGNAL_CTRLC ( 3 )
      CALL ERR_MODE ( 'NO_PROBE' ) 
!
      VLBI_DIR = '/vlbi'
!
! --- See if the user's NAMFIL is currently set up to use the default
! --- flyby setup.
!
      CALL USE_GLBFIL   ( 'OR'  )  ! Load glbcm
      CALL USE_GLBFIL_3 (  'R'  )  ! Load glbc3
      CALL USE_GLBFIL_4 (  'R'  )  ! Load glbc4
      L_HPE    = 0
      L_SPE    = 0
      L_BSP    = 0
      L_EERM   = 0
      L_MERM   = 0
      L_EHEO   = 0
      L_EHEC   = 0
      L_AEM    = 0
      ADR_HPE  = 0
      ADR_SPE  = 0
      ADR_BSP  = 0
      ADR_EERM = 0
      ADR_MERM = 0
      ADR_EHEO = 0
      ADR_EHEC = 0
      ADR_EHES = 0
      ADR_AEM  = 0
!
      CALL USE_GLBFIL_4 (  'WC' )
!
! --- Open socom
!
      CALL USE_COMMON ( 'ORC' )    ! Load socom
      CGM_TYPE = .FALSE.  !  Set once more session-type of socom
      CALL SOCOM_EXT()
!
      KDEFAULT=.TRUE.
!
      IF ( INIT_INTERACTIVE .NE. 2 ) THEN
!
! -------- Setup default global parameters from glbc4 if a new database was
! -------- loaded in scratch area
!
! -------- Initialization of FAST parameters
!
           IUER = -1
           CALL FAST_DEFAULT ( IUER )
           IF ( IUER .NE. 0 ) THEN
                CALL ERR_LOG ( 6901, -1, 'OPTIN', 'Errors in '// &
     &              'setting default values variables for FAST SOLVE' )
                STOP 'OPTIN  abnormal termination'
           END IF
!
! -------- Initialization of REWAY parameters
!
           IUER = -1
           CALL REWAY_DEFAULT ( IUER )
           IF ( IUER .NE. 0 ) THEN
                CALL ERR_LOG ( 6902, -1, 'OPTIN', 'Errors in '// &
     &              'setting default values variables for REWAY' )
                STOP 'OPTIN  abnormal termination'
           END IF
!
! -------- Initialization of GAMB parameters
!
           IUER = -1
           CALL GAMB_DEFAULT ( IUER )
           IF ( IUER .NE. 0 ) THEN
                CALL ERR_LOG ( 6903, -1, 'OPTIN', 'Errors in '// &
     &              'setting default values variables for GAMB' )
                STOP 'OPTIN  abnormal termination'
           END IF
!
! -------- Initialization of IONO parameters
!
           IUER = -1
           CALL IONO_DEFAULT ( IUER )
           IF ( IUER .NE. 0 ) THEN
                CALL ERR_LOG ( 6904, -1, 'OPTIN', 'Errors in '// &
     &              'setting default values variables for IONO' )
                STOP 'OPTIN  abnormal termination'
           END IF
!
! -------- Initialization of SETFL parameters
!
           IUER = -1
           CALL SETFL_DEFAULT ( IUER )
           IF ( IUER .NE. 0 ) THEN
                CALL ERR_LOG ( 6905, -1, 'OPTIN', 'Errors in '// &
     &              'setting default values variables for SETFL' )
                STOP 'OPTIN  abnormal termination'
           END IF
!
! -------- Initialization of common SOLVE parameters for different programs
!
           VTD_CONF_SES_SAVE = VTD_CONF_SES
           IUER = -1
           CALL SOLVE_DEFAULT ( IUER )
           IF ( IUER .NE. 0 ) THEN
                CALL ERR_LOG ( 6906, -1, 'OPTIN', 'Errors in '// &
     &              'setting default values variables for SOLVE' )
                STOP 'OPTIN  abnormal termination'
           END IF
           VTD_CONF_SES = VTD_CONF_SES_SAVE 
!
! -------- Initialization of ELIM parameters
!
           IUER = -1
           CALL ELIM_DEFAULT ( IUER )
           IF ( IUER .NE. 0 ) THEN
                CALL ERR_LOG ( 6907, -1, 'OPTIN', 'Errors in '// &
     &              'setting default values variables for ELIM' )
                STOP 'OPTIN  abnormal termination'
           END IF
!
! -------- Initialization of CRES parameters
!
           IUER = -1
           CALL CRES_DEFAULT ( IUER )
           IF ( IUER .NE. 0 ) THEN
                CALL ERR_LOG ( 6908, -1, 'OPTIN', 'Errors in '// &
     &              'setting default values variables for CRES' )
                STOP 'OPTIN  abnormal termination'
           END IF
!
! -------- Initialization of CNPLT parameters
!
           IUER = -1
           CALL CNPLT_DEFAULT ( IUER )
           IF ( IUER .NE. 0 ) THEN
                CALL ERR_LOG ( 6909, -1, 'OPTIN', 'Errors in '// &
     &              'setting default values variables for CNPLT' )
                STOP 'OPTIN  abnormal termination'
           END IF
!
! -------- Initialization of PROC parameters
!
           IUER = -1
           CALL PROC_DEFAULT ( IUER )
           IF ( IUER .NE. 0 ) THEN
                CALL ERR_LOG ( 6910, -1, 'OPTIN', 'Errors in '// &
     &              'setting default values variables for PROC' )
                STOP 'OPTIN  abnormal termination'
           END IF
!
! -------- Initialization of PAMB parameters
!
           IUER = -1
           CALL PAMB_DEFAULT ( IUER )
           IF ( IUER .NE. 0 ) THEN
                CALL ERR_LOG ( 6911, -1, 'OPTIN', 'Errors in '// &
     &              'setting default values variables for PAMB' )
                STOP 'OPTIN  abnormal termination'
           END IF
!
           IF ( INIT_INTERACTIVE .NE. 2 ) THEN
                CALL DBPOX ( NUMDB, LDBNAM, IDBV, IDBE )
                IF ( NUMDB .EQ. 1 ) IDBEST = 0
                IF ( NUMDB .GT. 0  .AND. NUMDB .LE. 15 ) THEN
                     CALL SBIT  ( IDBEST, NUMDB, INT2(0) )
                END IF
           END IF
!
! -------- Clear WEB_DIR name and set default
!
           CALL CLRCH ( WEB_DIR )
           WEB_DIR         = WEB_DIR__DEF
           MDLPL_IPS_PAG   = MDLPL_IPS_PAG__DEF
           MDLPL_IPC_PAG   = MDLPL_IPC_PAG__DEF
           MDLPL_FL_EOPMOD = MDLPL_FL_EOPMOD__DEF
           MDLPL_FL_CLF    = MDLPL_FL_CLF__DEF
           MDLPL_FL_FRAME  = MDLPL_FL_FRAME__DEF
!
! -------- Set correlation flags off
!
           COR_FLAG        = .FALSE.
           COR_GG_FLAG     = .FALSE.
           COR_GL_FLAG     = .FALSE.
           COR_LL_FLAG     = .FALSE.
           COR_CL_FLAG     = .FALSE.
!
! -------- Set flags for making sinex output to off
!
           FL_SINEX_MAKE = .FALSE.
           FL_SINEX_GLO  = .FALSE.
           FL_SINEX_LOC  = .FALSE.
           FL_SINEX_SEG  = .FALSE.
           FL_SINEX_EST  = .FALSE.
           FL_SINEX_COV  = .FALSE.
           FL_SINEX_CNS  = .FALSE.
           FL_SINEX_DCM  = .FALSE.
           CALL CLRCH ( SINEX_OUTFILE )
           CALL CLRCH ( SINEX_INCLUDE )
           CALL CLRCH ( SINEX_EXCLUDE )
           CALL CLRCH ( SINEX_ACKFIL  )
           CALL CLRCH ( SINEX_COMFIL  )
           CALL CLRCH ( SINEX_VERS    )
!
           KECC = .FALSE. ! No mapping eccentricity file
           KMGR = .FALSE. ! No mapping mean gradient file
           KMET = .FALSE. ! No mapping metric tensor
!
! -------- Check: norate_flag should by .FALSE. if observalbe type is the type
! -------- which uses delay rate
!
           IF ( DATYP_INQ ( IDATYP, RATE__DTP ) ) THEN
                IF ( NORATE_FLAG ) THEN
                     NORATE_FLAG = .FALSE.
                END IF
           END IF
!
! -------- Writing the current status of global parameters into GFLB-file
!
           NARCS = 1 ! only one arc is analized
           N_POSVAR  = 0 ! No mapping position variation external files
           STAT_HEO  = HEO__UNDF
           CALL CLRCH ( NAME_HEO )
           HEO_EPOCH_SEC = 0.0D0
           CALL CLRCH ( SUPPRESS_FILE )
           CALL USE_GLBFIL   ( 'OW' )
           CALL USE_GLBFIL_4 ( 'WC' )
           INIT_INTERACTIVE = 2
           CALL USE_COMMON ( 'OWC' )
      END IF
!
! --- Calculation pointer to the default value
!
      FM_POI = 1
      DO 410 J1=1,FMI_VAR
         IF ( FM_VAL(J1) .EQ. FAST_MODE ) FM_POI = J1
 410  CONTINUE
!
      FD_POI = 1
      DO 420 J2=1,FDI_VAR
         IF ( FD_VAL(J2) .EQ. FAST_DBG  ) FD_POI = J2
 420  CONTINUE
!
! --- Set VCAT configuation file
!
      CALL USE_GLBFIL_4 ( 'OR' )
      CALL GETENVAR ( 'VCAT_CONF', VCAT_CONF_FILE )
      IF ( ILEN(VCAT_CONF_FILE) == 0 ) THEN
           CALL GETENVAR ( 'PSOLVE_SAVE_DIR', SAVE_DIR )
           IF ( ILEN(SAVE_DIR) == 0 ) THEN
                VCAT_CONF_FILE = SOLVE_SAVE_DIR//'/vcat.conf'
              ELSE
                VCAT_CONF_FILE = TRIM(SAVE_DIR)//'/vcat.conf'
           END IF
      END IF
      CALL USE_GLBFIL_4 ( 'WC' )
      IF ( ILEN(VCAT_REPO) == 0 ) THEN
!
! -------- VCAT repo is not defined. Let us extrat if from the VCAT 
! -------- configuration file
!
           CALL RD_TEXT ( VCAT_CONF_FILE, MC, BUF, NC, IUER )
           IF ( IUER == 0 ) THEN
                DO 430 J3=1,NC
                   IF ( BUF(J3)(1:14) == 'GVF_REP_NAMES:' ) THEN
!
! --------------------- set VCAT_REPO to the secnd word of GVF_REP_NAMES
!
                        STR = BUF(J3)(15:)
                        CALL CHASHL ( STR )
                        IP = INDEX ( STR, ' ' )
                        IF ( IP == 0 .OR. IP > 4 ) IP = 4
                        VCAT_REPO = STR(1:IP) 
                   END IF
 430            CONTINUE 
              ELSE
                VCAT_REPO = 'UNKN'
           END IF
      END IF
!
! --- Open FLYBY_DEFAULT file and read the defaults: added 081491, aee
!
      INQUIRE ( FILE=PRE_SCR_DIR(:PRE_SD_LEN)//'FDEF'//PRE_LETRS, EXIST=LEX )
      IF ( .NOT. LEX ) THEN
           CALL FERR ( INT2(385), &
     &          'File '//PRE_SCR_DIR(:PRE_SD_LEN)//'FDEF'//PRE_LETRS// &
     &         ' was not found', INT2(0), INT2(0) )
           CALL EXIT ( 1 )
      END IF
!
      OPEN ( 45, FILE=PRE_SCR_DIR(:PRE_SD_LEN)//'FDEF'//PRE_LETRS, &
     &           IOSTAT=IOS, STATUS='OLD')
      IF ( IOS .NE. 0 ) then
           WRITE ( ERRSTR, "('Error ',I6,' during opening FDEF file: ',A)") &
     &             IOS, PRE_SCR_DIR(:PRE_SD_LEN)//'FDEF'//PRE_LETRS
           CALL FERR ( INT2(301), ERRSTR, INT2(0), INT2(0) )
      ENDIF
!
      FB_DEF_BUF=' '
      DO WHILE ( FB_DEF_BUF(1:15) .NE. 'FLYBY_DEFAULTS:' )
         READ ( 45, '(A)', IOSTAT=IOS ) FB_DEF_BUF
         CALL FERR ( INT2(IOS), "1. Error during reading flyby "// &
     &              "defaults. File: "//PRE_SCR_DIR(:PRE_SD_LEN)//'FDEF'// &
     &               PRE_LETRS, INT2(0), INT2(0) )
      ENDDO
!
      READ ( 45, '(A)', IOSTAT=IOS ) FB_DEF_BUF
      CALL FERR ( INT2(IOS), "2. Error during reading flyby defaults. File: "// &
     &     PRE_SCR_DIR(:PRE_SD_LEN)//'FDEF'//PRE_LETRS, INT2(0), INT2(0) )
      SITE_FLYBY = FB_DEF_BUF(15:28)
      READ ( 45, '(A)', IOSTAT = IOS ) FB_DEF_BUF
      CALL FERR ( INT2(IOS), "3. Error during reading flyby defaults. File: "// &
     &     PRE_SCR_DIR(:PRE_SD_LEN)//'FDEF'//PRE_LETRS, INT2(0), INT2(0) )
      SOURCE_FLYBY = FB_DEF_BUF(15:28)
      READ ( 45, '(A)', IOSTAT = IOS ) FB_DEF_BUF
      CALL FERR ( INT2(IOS), "4. Error during reading flyby defaults. File: "// &
     &     PRE_SCR_DIR(:PRE_SD_LEN)//'FDEF'//PRE_LETRS, INT2(0), INT2(0) )
      NUTSRS_FLYBY= fb_def_buf(15:28)
      READ ( 45, '(A)', IOSTAT=IOS ) FB_DEF_BUF
      CALL FERR ( INT2(IOS), "5. Error during reading flyby defaults. File: "// &
     &     PRE_SCR_DIR(:PRE_SD_LEN)//'FDEF'//PRE_LETRS, INT2(0), INT2(0) )
      NUTDLY_FLYBY= fb_def_buf(15:28)
      READ ( 45, '(A)', IOSTAT=IOS ) FB_DEF_BUF
      CALL FERR ( INT2(IOS), "6. Error during reading flyby defaults. File: "// &
     &     PRE_SCR_DIR(:PRE_SD_LEN)//'FDEF'//PRE_LETRS, INT2(0), INT2(0) )
      EOPDLY_FLYBY= fb_def_buf(15:28)
      READ ( 45, '(A)', IOSTAT=IOS ) FB_DEF_BUF
      CALL FERR ( INT2(IOS), "7. Error during reading flyby defaults. File: "// &
     &     PRE_SCR_DIR(:PRE_SD_LEN)//'FDEF'//PRE_LETRS, INT2(0), INT2(0) )
      PLATE_MODEL= fb_def_buf(15:28)
      READ ( 45, '(A)', IOSTAT=IOS ) FB_DEF_BUF
      CALL FERR ( INT2(IOS), "8. Error during reading flyby defaults. File: "// &
     &     PRE_SCR_DIR(:PRE_SD_LEN)//'FDEF'//PRE_LETRS, INT2(0), INT2(0) )
      STAVEL_FLYBY= fb_def_buf(15:28)
      READ ( 45, '(a)', IOSTAT=IOS ) FB_DEF_BUF
      CALL FERR ( INT2(IOS), "9. Error during reading flyby defaults. File: "// &
     &     PRE_SCR_DIR(:PRE_SD_LEN)//'FDEF'//PRE_LETRS, INT2(0), INT2(0) )
      HFEOP_FLYBY= fb_def_buf(15:28)
      READ(45,'(A)',IOSTAT=IOS) FB_DEF_BUF
      CALL FERR ( INT2(IOS), "10. Error during reading flyby defaults. File: "// &
     &     PRE_SCR_DIR(:PRE_SD_LEN)//'FDEF'//PRE_LETRS, INT2(0), INT2(0) )
      PLOD_FLYBY= FB_DEF_BUF(15:28)
      READ(45,'(A)',IOSTAT=IOS) FB_DEF_BUF
      CALL FERR ( INT2(IOS), "11. Error during reading flyby defaults. File: "// &
     &     PRE_SCR_DIR(:PRE_SD_LEN)//'FDEF'//PRE_LETRS, INT2(0), INT2(0) )
      AXOF_FLYBY= FB_DEF_BUF(15:28)
!
      CLOSE ( 45 )
!
      CALL HOL2CHAR( STASUB, INT2(1), NAME_SIZE, LNAME )
      KDEFAULT=KDEFAULT.AND.LNAME.EQ.SITE_FLYBY
      CALL HOL2CHAR( SRCSUB, INT2(1), NAME_SIZE, LNAME )
      KDEFAULT=KDEFAULT.AND.LNAME.EQ.SOURCE_FLYBY
      CALL HOL2CHAR( NUTSRS, INT2(1), NAME_SIZE, LNAME )
      KDEFAULT=KDEFAULT.AND.LNAME.EQ.NUTSRS_FLYBY
      CALL HOL2CHAR( NUTDLY, INT2(1), NAME_SIZE, LNAME )
      KDEFAULT=KDEFAULT.AND.LNAME.EQ.NUTDLY_FLYBY
      CALL HOL2CHAR( EOPDLY, INT2(1), NAME_SIZE, LNAME )
      KDEFAULT=KDEFAULT.AND.LNAME.EQ.EOPDLY_FLYBY
      CALL HOL2CHAR( PLTMOD, INT2(1), NAME_SIZE, LNAME )
      KDEFAULT=KDEFAULT.AND.LNAME.EQ.PLATE_MODEL
      CALL HOL2CHAR( VELSUB, INT2(1), NAME_SIZE, LNAME )
      KDEFAULT=KDEFAULT.AND.LNAME.EQ.STAVEL_FLYBY
      CALL HOL2CHAR( HFEOPF, INT2(1), NAME_SIZE, LNAME )
      KDEFAULT=KDEFAULT.AND.LNAME.EQ.HFEOP_FLYBY
      CALL HOL2CHAR( PLCALF, INT2(1), NAME_SIZE, LNAME )
      KDEFAULT=KDEFAULT.AND.LNAME.EQ.PLOD_FLYBY
!
! --- Ignore IFLYREP(1), which is used later on to see if the user
! --- wants least squares, after changing flyby.  IFLYREP(2) will
! --- be 1 if flyby default is set up, and 0 otherwise.
!
      IF(KDEFAULT) THEN
        IFLYDEF = 1
      ELSE
        IFLYDEF= 0
      ENDIF
      IXX = 0
      IYY = 0
!
      KPAGE = 1
      SCLEN__OLD = -1
      SCWID__OLD = -1
   50 CONTINUE
!
! --- Check length of window to determine whether we need one or two pages
!
      CALL GET_TERMSIZE ( SCLEN, SCWID ) 
      IF ( SCLEN .LE. 0 ) THEN
           CALL GETENVAR ( 'LINES', STR )
           READ ( UNIT=STR, IOSTAT=IOS, FMT='(I4)' ) SCLEN
           IF ( SCLEN .LE. 0 ) SCLEN = MIN_SCLEN
      END IF
!
! --- Test on validity screen length
!
      IF ( SCLEN .LT. MIN_SCLEN ) THEN
           CALL START_MN()
           CALL SETCR_MN ( 0, 0 )
           CALL CLEAR_MN()
           STR1(1:10)='            '
           WRITE ( UNIT=STR1, IOSTAT=IOS, FMT='(I10)' ) SCLEN
           STR2(1:10)='            '
           WRITE ( UNIT=STR2, IOSTAT=IOS, FMT='(I10)' ) MIN_SCLEN
           CALL ADDSTR_F ( 'The length of the screen now is            '//STR1 )
           CALL NL_MN()
           CALL ADDSTR_F ( 'Please, using mouse increase the length to '//STR2 )
           CALL NL_MN()
           CALL ADDSTR_F ( 'since OPTIN cannot work properly on ' )
           CALL ADDSTR_F ( 'too short screen' )
           CALL NL_MN()
           CALL NL_MN()
           CALL SENKR_MN ( IXX, IYY, ICHAR4 )
!
! -------- Restart MN
!
           IXX = 0
           IYY = 0
           GOTO 7000
      END IF
!
! --- Check size of window
!
      IF ( SCWID .LE. 0 ) THEN
           CALL GETENVAR ( 'COLUMNS', STR )
           READ ( UNIT=STR, IOSTAT=IOS, FMT='(I4)' ) SCWID
           IF ( SCWID .LE. 0 ) SCWID = MIN_WIDTH
      END IF
!
! --- Test on validity screen width
!
      IF ( SCWID .LT. MIN_WIDTH ) THEN
           CALL START_MN()
           CALL SETCR_MN ( 0, 0 )
           CALL CLEAR_MN()
           STR1(1:10)='            '
           WRITE ( UNIT=STR1, IOSTAT=IOS, FMT='(I10)' ) SCWID
           STR2(1:10)='            '
           WRITE ( UNIT=STR2, IOSTAT=IOS, FMT='(I10)' ) MIN_WIDTH
           CALL ADDSTR_F ( 'The width of the screen now is            '//STR1 )
           CALL NL_MN()
           CALL ADDSTR_F ( 'Please, using mouse increase the width to '//STR2 )
           CALL NL_MN()
           CALL ADDSTR_F ( 'since OPTIN cannot work properly on ' )
           CALL ADDSTR_F ( 'too thin screen' )
           CALL NL_MN()
           CALL NL_MN()
           CALL ADDSTR_F ( "Hit any key when ready" )
           CALL SENKR_MN ( IXX, IYY, ICHAR4 )
!
! -------- Restart MN
!
           IXX = 0
           IYY = 0
           GOTO 7000
      END IF
      IF ( SCLEN__OLD .NE. -1  .AND.  SCLEN__OLD .NE. SCLEN ) GOTO 7000
      IF ( SCWID__OLD .NE. -1  .AND.  SCWID__OLD .NE. SCWID ) GOTO 7000
!
      NEED_2 = .TRUE.
      IF ( SCLEN .GE. MAX_SCLEN ) NEED_2 = .FALSE.
      IF ( KPAGE.EQ.2 .AND. NEED_2 ) GOTO 1001
      CALL START_MN()
      CALL CLEAR_MN()
      CALL SETCR_MN ( 0, 0 )
      CALL ADDSTR_F ( "Active initials: "//PRE_LETRS )
!
! --- Write version
!
      STR = GET_VERSION()
      CALL SETCR_MN ( 79-I_LEN(STR), 0 )
      CALL REVERSE_ON_MN()
      CALL ADDSTR_F ( STR(1:I_LEN(STR)) )
      CALL REVERSE_OFF_MN()
!
      CALL NL_MN()
      CALL NL_MN()
      CALL ADDSTR_F ( "Current work file directory: " )
      CALL ADDSTR_F ( pre_scr_dir(1:48) )
!
      CALL NL_MN()
      CALL NL_MN()
      CALL ADDSTR_F ( 'group delay (A)mbiguity resolution ' )
      CALL ADDSTR_F ( '(D)phase delay ambiguity resolution ' )
      CALL NL_MN()
      CALL NL_MN()
      CALL ADDSTR_F ( "(G)et data base                    (U)pdate data base  Change (R)epository " )
      CALL REVERSE_ON_MN()
      CALL ADDSTR_F ( TRIM(VCAT_REPO) )
      CALL REVERSE_OFF_MN()
!
      CALL NL_MN()
      CALL NL_MN()
      CALL ADDSTR_F ( "(F)Default flyby setup " )
!
      CALL REVERSE_ON_MN()
      IF ( IFLYDEF .EQ. 1 ) THEN
           CALL ADDSTR_F ( " on" )
        ELSE
           CALL ADDSTR_F ( "off" )
      ENDIF
      CALL REVERSE_OFF_MN()
      CALL ADDSTR_F ( "         (#)Change flyby setup " )
      CALL ADDSTR_F ( " (W)Flyby warnings " )
!
      CALL REVERSE_ON_MN()
      IF ( FLYBY_WARNING ) THEN
           CALL ADDSTR_F ( " on" )
        ELSE
           CALL ADDSTR_F ( "off" )
      ENDIF
      CALL REVERSE_OFF_MN()
!
      call nl_mn()
      call nl_mn()
      call addstr_f ( "(P)lot the residuals               "// &
     &                "   Plot clock, atmosphere & EOP estimates(/)" )
      call nl_mn()
      call nl_mn()
      call addstr_f("(C)hange Spooling current: " )
      call reverse_on_mn()
      if (kspool) then
        call addstr_f("on " )
      else
        call addstr_f("off" )
      endif
      call reverse_off_mn()
      call addstr_f("     (;)Rewind Spool File" )
      CALL ADDSTR_F ( "  (~)Update theoreticals" )
!#      call addstr_f("  (~)Beeps at prompt " )
!#      call reverse_on_mn()
!#      if (kbeep) then
!#        call addstr_f(" on" )
!#        call reverse_off_mn()
!#      else
!#        call addstr_f("off" )
!#        call reverse_off_mn()
!#      endif
      call nl_mn()
      call nl_mn()
      call addstr_f("(<)User Partials " )
      call reverse_on_mn()
      if (kuser_part) then
        call addstr_f(" on" )
        call reverse_off_mn()
        write(bufstr,'(": ",A20)') user_part_prog
        call addstr_f(bufstr(:trimlen(bufstr)) )
      else
        call addstr_f("off" )
        call reverse_off_mn()
        call addstr_f("      " )
      endif
      call addstr_f("         (&)User Program " )
      call reverse_on_mn()
      if (user_pro.ne.' ') then
        call addstr_f(" on" )
        call reverse_off_mn()
        write(bufstr,'(": ",A20)') user_pro
        call addstr_f(bufstr(:trimlen(bufstr)) )
      else
        call addstr_f("off" )
        call reverse_off_mn()
        call addstr_f(" " )
      endif
      call nl_mn()
      call nl_mn()
      call addstr_f('(+)Obs Dependent Contributions' )
      call addstr_f('     (%)Station Dependent Calibrations/Partials' )
      call nl_mn()
      call nl_mn()
      call addstr_f ( '(M)No Net Translation Constraint ' )
      call reverse_on_mn()
      if ( kcentermass )  then
           call addstr_f("on:" )
           call reverse_off_mn()
           write(bufstr,'("  ",A48)') sta_wt_fil
           call addstr_f(bufstr(:trimlen(bufstr)) )
        else
           call addstr_f("off" )
           call reverse_off_mn()
      endif
      CALL SETCR_MN ( 58, 18 )
      CALL ADDSTR_F ( '(H) Reweigh the data' )
!
      CALL NL_MN()
      CALL NL_MN()
      call addstr_f ( "(R)ewrite Screen                   (T)erminate SOLVE"// &
     &                "      Run least-s(Q)uares" )
!
      if ( need_2) then
           call nl_mn()
           call nl_mn()
           call addstr_f ( "(N)ext page options                (CTRL/A) Update full residual listing" )
        else
           goto 1011
      endif
  200 CONTINUE
      CALL SETCR_MN ( IXX, IYY )
!
! --- Reading from keyboard one symbol
!
      CALL SENKR_MN ( IXX, IYY, ICHAR4 )
      IF ( CCHAR(4:4) .EQ. CHAR(13) ) CCHAR(4:4)=' '
      IX = INT2(IXX)
      IY = INT2(IYY)
      if (cchar(4:4).eq.' ') then
        if       (iy.eq.4) then
            if ( ix.le.31 ) cchar(4:4) = 'A'
         else if (iy.eq.6) then
            if( ix .le. 32 ) cchar(4:4) = 'G'
            if ( ix.ge.35 .and. ix.le.54 ) cchar(4:4) = 'U'
            if ( ix.ge.55 .and. ix.le.78 ) cchar(4:4) = 'R'
         else if (iy.eq.8) then
            if ( ix.ge.1  .and. ix.le.25 ) cchar(4:4) = 'F'
            if ( ix.ge.35 .and. ix.le.56 ) cchar(4:4) = '#'
            if ( ix.ge.60 .and. ix.le.78 ) cchar(4:4) = 'W'
         else if (iy.eq.10) then
            if ( ix.le.19                ) cchar(4:4) = 'P'
            if ( ix.ge.35 .and. ix.le.76 ) cchar(4:4) = '*'
            if ( ix.ge.35 .and. ix.ge.77 ) cchar(4:4) = '/'
         else if (iy.eq.12) then
            if ( ix.le.29                ) cchar(4:4) = 'C'
            if ( ix.ge.35 .and. ix.le.54 ) cchar(4:4) = ';'
            if ( ix.ge.57 .and. ix.le.78 ) cchar(4:4) = '~'
         else if (iy.eq.14) then
            if ( ix.le.20                ) cchar(4:4) = '<'
            if ( ix.ge.35 .and. ix.le.54 ) cchar(4:4) = '&'
         else if (iy.eq.16) then
            if ( ix.ge.1  .and. ix.le.29 ) cchar(4:4) = '+'
            if ( ix.ge.35 .and. ix.le.76 ) cchar(4:4) = '%'
         else if (iy.eq.18) then
            if ( ix.ge.1  .and. ix.le.57 ) cchar(4:4) = 'M'
            if ( ix.ge.58 .and. ix.le.77 ) cchar(4:4) = 'H'
         else if (iy.eq.20) then
            if ( ix.ge.1  .and. ix.le.15 ) cchar(4:4) = 'R'
            if ( ix.ge.35 .and. ix.le.51 ) cchar(4:4) = 'T'
            if ( ix.ge.58 .and. ix.le.76 ) cchar(4:4) = 'Q'
         else if (iy.eq.22) then
            if ( ix.ge.36 .and. ix.le.72 ) cchar(4:4) = '_'
         else if (iy.ge.22 .and. need_2 ) then
            cchar(4:4) = 'N'
            ixx = 1
            iyy = 20
         else if (iy.eq.22 .and. .not. need_2 ) then
            if ( ix.le.18  ) cchar(4:4) = '='
            if ( ix.ge.33  ) cchar(4:4) = 'Y'
         else if (iy.eq.24 .and. .not. need_2 ) then
            if ( ix.le.26  ) cchar(4:4) = 'I'
            if ( ix.ge.33  ) cchar(4:4) = '$'
         else if (iy.eq.26 ) then
            if ( ix.le.20  ) cchar(4:4) = '@'
            if ( ix.ge.33  ) cchar(4:4) = '?'
         else if (iy.eq.28 ) then
            if ( ix.le.16  ) cchar(4:4) = '^'
            if ( ix.ge.33  ) cchar(4:4) = 'Z'
         else if (iy.eq.30 ) then
            if ( ix.le.21  ) cchar(4:4) = '['
            if ( ix.ge.33  ) cchar(4:4) = 'V'
         else if (iy.eq.32 ) then
            if ( ix.le.29  ) cchar(4:4) = '>'
            if ( ix.ge.33  ) cchar(4:4) = 'K'
         else if (iy.eq.34 ) then
            if ( ix.ge.1  .and. ix.le.18 ) cchar(4:4) = ':'
            if ( ix.ge.33 .and. ix.le.60 ) cchar(4:4) = 'J'
         else if (iy.eq.36 ) then
            if ( ix.le.32 ) cchar(4:4) = '|'
            if ( ix.gt.33 ) cchar(4:4) = '"'
         else if (iy.eq.38 ) then
            if ( ix.le.32 ) cchar(4:4) = '{'
         else if (iy.eq.40 ) then
            if ( ix.ge.7  .and. ix.le.13 ) cchar(4:4) = 'E'
            if ( ix.ge.17 .and. ix.le.25 ) cchar(4:4) = 'S'
            if ( ix.ge.29 .and. ix.le.39 ) cchar(4:4) = 'L'
            if ( ix.ge.43 .and. ix.le.53 ) cchar(4:4) = 'B'
            if ( ix.ge.57 .and. ix.le.69 ) cchar(4:4) = 'X'
         else
            goto 200
        endif
      endif
 222  continue
      IF (CCHAR(4:4) .EQ. 'T') GO TO 2000
      IF (CCHAR(4:4) .EQ. 'A') GO TO 500
      IF (CCHAR(4:4) .EQ. 'D') GO TO 510
      IF (CCHAR(4:4) .EQ. '*') GO TO 550
      IF (CCHAR(4:4) .EQ. '/') GO TO 555
      IF (CCHAR(4:4) .EQ. 'G') GO TO 620
      IF (CCHAR(4:4) .EQ. CHAR(7) ) GO TO 620
      IF (CCHAR(4:4) .EQ. 'H') GO TO 650
      IF (CCHAR(4:4) .EQ. '@') GO TO 3850
      IF (CCHAR(4:4) .EQ. '?') GO TO 3850
      IF (CCHAR(4:4) .EQ. 'E') GO TO 700
      IF (CCHAR(4:4) .EQ. 'S') GO TO 700
      IF (CCHAR(4:4) .EQ. 'L') GO TO 700
      IF (CCHAR(4:4) .EQ. 'B') GO TO 700
      IF (CCHAR(4:4) .EQ. 'X') GO TO 700
      IF (CCHAR(4:4) .EQ. 'Q') GO TO 801
      IF (CCHAR(4:4) .EQ. CHAR(18) ) GO TO 801 ! Ctrl/Q
      IF (CCHAR(4:4) .EQ. '$') GO TO 800
      IF (CCHAR(4:4) .EQ. 'P') GO TO 900
      IF (CCHAR(4:4) .EQ. '=') GO TO 901
      IF (CCHAR(4:4) .EQ. 'Y') GO TO 903
      IF (CCHAR(4:4) .EQ. '#') GO TO 951
      IF (CCHAR(4:4) .EQ. 'F') GO TO 951
      IF (CCHAR(4:4) .EQ. 'W') GO TO 950
      IF (CCHAR(4:4) .EQ. 'N'.and.need_2) GO TO 1001
      IF (CCHAR(4:4) .EQ. '<') GO TO 1100
      IF (CCHAR(4:4) .EQ. '&') GO TO 1100
      IF (CCHAR(4:4) .EQ. 'M') GO TO 970
      IF (CCHAR(4:4) .EQ. 'C') GO TO 975
      IF (CCHAR(4:4) .EQ. ':') GO TO 977
      IF (CCHAR(4:4) .EQ. ';') GO TO 978
      IF (CCHAR(4:4) .EQ. '~') GO TO 979
      IF (CCHAR(4:4) .EQ. '`') GO TO 979
      IF (CCHAR(4:4) .EQ. '%') GO TO 926
      IF (CCHAR(4:4) .EQ. '^') GO TO 1940
      IF (CCHAR(4:4) .EQ. 'Z') GO TO 1945
      IF (CCHAR(4:4) .EQ. '.') GO TO 1950
      IF (CCHAR(4:4) .EQ. '[') GO TO 1000
      IF (CCHAR(4:4) .EQ. ']') GO TO 1024
      IF (CCHAR(4:4) .EQ. 'V') GO TO 1050
      IF (CCHAR(4:4) .EQ. 'J') GO TO 1502
      IF (CCHAR(4:4) .EQ. 'K') GO TO 1506
      IF (CCHAR(4:4) .EQ. 'R') GO TO 1870
      IF (CCHAR(4:4) .EQ. '+') GO TO 1700
      IF (CCHAR(4:4) .EQ. 'I') GO TO 1600
      IF (CCHAR(4:4) .EQ. 'U') GO TO 1850
      IF (CCHAR(4:4) .EQ. CHAR(21) ) GO TO 1850
      IF (CCHAR(4:4) .EQ. '|') GO TO 1900
      IF (CCHAR(4:4) .EQ. '"') GO TO 1910
      IF (CCHAR(4:4) .EQ. '{') GO TO 1920
      IF (CCHAR(4:4) .EQ. '\') GO TO 1930
      IF (CCHAR(4:4) .EQ. '-') GO TO 1960
      IF (CCHAR(4:4) .EQ. '_') GO TO 1970
      IF (CCHAR(4:4) .EQ. CHAR(1) ) GO TO 1970
!
!**** Special logic (added 930823 by MWH) to allow user to choose a
!****  particular site page to process
!
      IF ( CCHAR(4:4) .GE. '1'  .AND.  CCHAR(4:4) .LE. '9' ) THEN
           READ ( UNIT=CCHAR(4:4), FMT='(I1)' ) ICONTI
!C           iconti = jchar(ichar4,4)-48 ! Bad practice of programming!
           CALL USE_BUFFER ( ICONTI, INT2(1), 'OWC' )
           CALL RUN_PROG   ( 'SETFL', 'PASS', INT2(0) )
      ENDIF
!
      IF ( KPAGE .EQ. 1  .OR.  .NOT. NEED_2 ) THEN
           GOTO 200
         ELSE
           GOTO 1110
      ENDIF
  500 CONTINUE
!
! --- Make automatic preliminary estimatiuon of the polinomial clock model,
! --- group delay ambiguity resolution and finding oulliers
!
        CALL CLEAR_MN ( )
        CALL END_MN()
        CALL UN_CURSES ( )
        CALL CLEAR ( 0, 0 )
        CALL RUN_PROG ( 'GAMB', 'WAIT', INT2(0) )
!
        GOTO 6000
  510 CONTINUE
!
! ----- Make manual or semi-mannual or automatic phase delay ambiguity
! ----- resolution
!
        CALL CLEAR_MN ( )
        CALL END_MN()
        CALL UN_CURSES ( )
        CALL CLEAR ( 0, 0 )
!
        CALL END_MN()
        CALL UN_CURSES ( )
        CALL NOUT ( 128, IBUFF )
        CALL USE_BUFFER ( IBUFF, INT2(64), 'OWC' )
        CALL RUN_PROG  ( 'PAMB', 'WAIT', INT2(0) )
        CALL START_MN()
!
        GOTO 6000
  550 CONTINUE
!
      CALL USE_BUFFER ( INT2(0), INT2(1), 'OWC' )
      GOTO 558
 555  CONTINUE
      CALL USE_BUFFER ( INT2(1), INT2(1), 'OWC' )
!
 558  CONTINUE
!
      CALL END_MN()
      CALL DBPOX ( NUMDB, LDBNAM, IDBV, IDBE )
      DO 440 J4=1,NUMDB
         IF ( KBIT(IDBSEL,INT2(J4))  .AND.  .NOT. KBIT(IDBEST,INT2(J4)) ) THEN
              CALL CLRCH ( STR )
              CALL INCH ( J4, STR )
              CALL ERR_LOG ( 6912, -1, 'OPTIN', CDBNAM(J4)//' , '// &
     &             STR(1:I_LEN(STR))//'-th database in the list '// &
     &            'of the databases has not been analazyed and '// &
     &            'therefore estimates are not calculated yet. MDLPL '// &
     &            ' will be unable to work. Please make solution firstly '// &
     &            '(using option Q) and then look at the plots' )
              CALL HIT_CONT ( %VAL(0), %VAL(0) )
              CALL USE_BUFFER ( ICRX, INT2(1), 'ORC' )
              GOTO  50
         END IF
  440 CONTINUE
!
      CALL RUN_PROG   ( 'MDLPL', 'PASS', INT2(0) )
  600 CONTINUE
!
! ----- Get another data set from the data base
! ----- Pack 1 with identifier indicating sdbh was called by OPTIN
!
        ICRX = 6 + 100
        CALL USE_BUFFER ( ICRX, INT2(7), 'OWC' )
        CALL RUN_PROG   ( 'SDBH', 'PASS', INT2(0) )
        GOTO 3000
!
  620   CONTINUE
        CALL NOUT ( 128, IPASS )
        IPASS(1) = 1
        CALL USE_BUFFER ( IPASS, INT2(64), 'OWC' )
        CALL RUN_PROG   ( 'GETDB', 'PASS', INT2(0) )
        CALL USE_GLBFIL_4 ( 'ORC' )
        CALL USE_COMMON   ( 'ORC' )
        INIT_INTERACTIVE = 1
        CALL USE_COMMON   ( 'WC' )
        GOTO 3000
  650  CONTINUE
!
! ----- Reway
!
        CALL END_MN()
        CALL DBPOX ( NUMDB, LDBNAM, IDBV, IDBE )
        DO 450 J5=1,NUMDB
           IF ( KBIT(IDBSEL,INT2(J5))  .AND.  KBIT(IDCSEL,INT2(J5)) .AND. &
     &          .NOT. KBIT(IDBEST,INT2(J5)) ) THEN
!
                CALL CLRCH ( STR )
                CALL INCH ( J5, STR )
                CALL ERR_LOG ( 6913, -1, 'OPTIN', CDBNAM(J5)//' , '// &
     &               STR(1:I_LEN(STR))//'-th database in the list '// &
     &              'of the databases has not been analazyed and '// &
     &              'therefore residuals are not calculated yet. REWAY '// &
     &              ' will be unable to work correctly. Please make '// &
     &              'solution firstly (using option Q) and then reweight '//'it' )
                CALL HIT_CONT ( %VAL(0), %VAL(0) )
                GOTO  50
           END IF
  450   CONTINUE
!
        IF ( NORATE_FLAG ) THEN
             CALL ERR_LOG ( 6914, -1, 'OPTIN', ' NORATE_FLAG '// &
     &           ' is set YES and it is incompatible with REWAY since '// &
     &           'REWAY must use delay rate. Please, set "(R)Use rate" to '// &
     &           'YES in the menu of the last OPTIN page.' )
             CALL HIT_CONT ( %VAL(0), %VAL(0) )
             GOTO  50
        END IF
!
        CALL USE_GLBFIL ( 'OWC' )
        CALL USE_COMMON ( 'OWC' )    ! write socom
        CALL RUN_PROG   ( 'REWAY', 'WAIT', INT2(0) )
        CALL USE_GLBFIL ( 'ORC' )
        GOTO 50
  700  CONTINUE
!
! ---- Re-set the parameter flags
!
      IF ( FAST_MODE .NE. F__NONE .AND. SOLVE_EMULATION .NE. 0 ) THEN
!
! -------- Test eligibility of the SOLVE_EMULATION mode
!
           CALL CLRCH ( STR )
           CALL INCH  ( SOLVE_EMULATION, STR )
           call clear_mn()  !  clear screen
           call nl_mn()  !  new line
           call nl_mn()  !  new line
           call addstr_f ( "                          " )
           call reverse_on_mn()
           call addstr_f ( "Conflicting options:" )
           call reverse_off_mn()
           call nl_mn()
           call nl_mn()
           WRITE ( 6, FMT='(A)' ) CHAR(27)//'&v1S' ! set red color
           call addstr_f ( 'Fast mode '//FM_STR(FM_POI)(1:FM_LEN(FM_POI))// &
     &                   ' cannot be in force when archaic SOLVE is emulated' )
           call nl_mn()
           call addstr_f ( 'SOLVE_EMULATION = '//STR(1:I_LEN(STR)) )
           call nl_mn()
           call nl_mn()
           call addstr_f ( 'Hint: either set SOLVE_EMULATION environment '// &
     &                     'variable to zero ' )
           call nl_mn()
           call addstr_f ( 'and then reread database or turn off B3D mode.' )
           WRITE ( 6, FMT='(A)' ) CHAR(27)//'&v0S' ! turn off red color
           call nl_mn()
           call senkr_mn(ixx,iyy,ichar4 )
           call clear_mn()  !  clear screen
           GOTO 6000
      END IF
      CALL SOCOM_EXT()
!
      ICONTI = 1
      IF ( CCHAR(4:4) .EQ. 'S' ) ICONTI = -1
      IF ( CCHAR(4:4) .EQ. 'L' ) ICONTI = -2
      IF ( CCHAR(4:4) .EQ. 'B' ) ICONTI = -5
      IF ( CCHAR(4:4) .EQ. 'X' ) ICONTI = -6
!
      CALL USE_BUFFER ( ICONTI, INT2(1), 'OWC' )
      CALL RUN_PROG   ( 'SETFL', 'PASS', INT2(0) )
!
!  Toggle ion correction flag
!
  800 CONTINUE
        KIONO = .NOT. KIONO
        CALL USE_GLBFIL ( 'OWC' )
        GOTO 50
!
! --- RE-RUN THE SOLUTION
!
 801  CONTINUE
!
      IF ( FAST_MODE .NE. F__NONE .AND. SOLVE_EMULATION .NE. 0 ) THEN
!
! -------- Test eligibility of the SOLVE_EMULATION mode
!
           CALL CLRCH ( STR )
           CALL INCH  ( SOLVE_EMULATION, STR )
           call clear_mn()  !  clear screen
           call nl_mn()  !  new line
           call nl_mn()  !  new line
           call addstr_f ( "                          " )
           call reverse_on_mn()
           call addstr_f ( "Conflicting options:" )
           call reverse_off_mn()
           call nl_mn()
           call nl_mn()
           WRITE ( 6, FMT='(A)' ) CHAR(27)//'&v1S' ! set red color
           call addstr_f ( 'Fast mode '//FM_STR(FM_POI)(1:FM_LEN(FM_POI))// &
     &                   ' cannot be in force when archaic SOLVE is emulated' )
           call nl_mn()
           call addstr_f ( 'SOLVE_EMULATION = '//STR(1:I_LEN(STR)) )
           call nl_mn()
           call nl_mn()
           call addstr_f ( 'Hint: either set SOLVE_EMULATION environment '// &
     &                     'variable to zero ' )
           call nl_mn()
           call addstr_f ( 'and then reread database or turn off B3D mode.' )
           WRITE ( 6, FMT='(A)' ) CHAR(27)//'&v0S' ! turn off red color
           call nl_mn()
           call senkr_mn(ixx,iyy,ichar4 )
           call clear_mn()  !  clear screen
           GOTO 6000
      END IF
!
      IF ( FAST_MODE .EQ. F__B3D .AND. CLOCK_INTERVAL .GT. 0  .AND. &
     &     ATMOS_INTERVAL .GT. 0                                     ) THEN
!
! -------- Test eligibility of the fast mode
!
           RL = MAX(CLOCK_INTERVAL,ATMOS_INTERVAL) / &
     &          MIN(CLOCK_INTERVAL,ATMOS_INTERVAL)
           IF ( ABS( RL - INT(RL+0.49) ) .GT. 0.001 ) THEN
                call clear_mn()  !  clear screen
                call nl_mn()  !  new line
                call nl_mn()  !  new line
                call addstr_f ( "                          " )
                call reverse_on_mn()
                call addstr_f ( "Conflicting options:" )
                call reverse_off_mn()
                call nl_mn()
                call nl_mn()
                WRITE ( 6, FMT='(A)' ) CHAR(27)//'&v1S' ! set red color
                call addstr_f ( "The time span of the atmosphere and "// &
     &              "clock parameters is not a multiple each" )
                call nl_mn()
                call addstr_f ( "other. This case cannot be handled "// &
     &                          "by B3D algorithm." )
                call nl_mn()
                call nl_mn()
                call addstr_f ( "Hint: either change the time span for "// &
     &               "the atmosphere or for the clock " )
                call nl_mn()
                call addstr_f ( "or turn off B3D mode." )
                WRITE ( 6, FMT='(A)' ) CHAR(27)//'&v0S' ! turn off red color
                call nl_mn()
                call senkr_mn(ixx,iyy,ichar4 )
                call clear_mn()  !  clear screen
                GOTO 6000
           END IF
      END IF
!
      CALL END_MN()
      CALL SOCOM_EXT()
!
! --- Remove stale files with constraints, buil-in and user, local and global.
! --- This files will be re-created if necessary, but the stale versions may
! --- inflict harm
!
      DO 460 J6=1,4
         FNAME = PRE_SCR_DIR(1:PRE_SD_LEN)//FIL_PREF(J6)//PRE_LETRS
         INQUIRE ( FILE=FNAME, EXIST=LEX )
         IF ( LEX ) THEN
              CALL UNLINK ( FNAME(1:I_LEN(FNAME))//CHAR(0) )
         END IF
 460  CONTINUE
!
      IF ( CCHAR(4:4) == CHAR(18) ) THEN
           CALL USE_GLBFIL_4 ( 'ORC' )
           IF ( VTD_ADR .NE. 0 ) THEN
                IUER = -1
                CALL VTD_QUIT ( %VAL(VTD_ADR), IUER )
                CALL FREE ( VTD_ADR )
                VTD_ADR = 0
                CALL USE_GLBFIL_4 ( 'OWC' )
           END IF
      END IF
!
! --- Set status
!
      CALL STATUS_SET  ( 'OPTIN', STA__INT )
      CALL SAVE_CFNAME ( '<interactive>' )
!
! --- Make LSQ solution
!
      CALL RUN_PROG( 'GLOBL', 'PASS', INT2(0) )
!
! === Plot the residuals
!
  900 CONTINUE
!
! ----- Scheduling REPA
!
        CALL CLEAR_MN ( )
        CALL END_MN()
        CALL UN_CURSES ( )
        CALL CLEAR ( 0, 0 )
!
! ----- Call the program for outliers elimination or restoring observations 
! ----- which have been rejected earlier as outliers.
!
        CALL USE_BUFFER ( INT2(0), INT2(1), 'OWC' )
        CALL CLRCH ( STR )
        CALL GETENVAR   ( 'USE_REPAB', STR )
        CALL DBPOX ( NUMDB, LDBNAM, IDBV, IDBEND )
        IF ( STR(1:1) .EQ. 'Y' .OR. STR(1:1) .EQ. 'y' ) THEN
             CALL RUN_PROG   ( 'REPAB', 'WAIT', INT2(0) )
           ELSE 
             CALL RUN_PROG   ( 'REPA', 'WAIT', INT2(0) )
        END IF
        CALL USE_BUFFER ( ICRES, INT2(1), 'ORC' )
!
! ----- Reread socom, since REPA may change it
!
        CALL USE_COMMON ( 'ORC' )
        CGM_TYPE = .FALSE.  !  Set once more session-type of socom
        CALL SOCOM_EXT()
!?        IF ( ICRES .EQ. 0 ) GOTO 6000  !  Displaying  OPTIN menu again
!?        IF ( ICRES .EQ. 1 ) GOTO 3850  !  Scheduling  CRES
!?        CALL RUN_PROG( 'REPA', 'PASS', INT2(0) )
        GOTO 50
901   CONTINUE
        CALL SETCR_MN ( 0, 0 )
        CALL CLEAR_MN ()
902     CONTINUE
        EVSTARTX = EVSTART
        IF ( EVSTARTX .EQ. 0  .AND.  EVINT .NE. 0 ) EVSTARTX = EVINT
        WRITE ( BUFSTR, '("Current values: Every ",I2," Start ",I2)' ) &
     &          EVINT, EVSTARTX
        CALL ADDSTR_F(BUFSTR )
        CALL NL_MN()
        CALL NL_MN()
        CALL ADDSTR_F ( "Input values (every,start): " )
        CALL GETSTR_F ( BUFSTR )
        READ ( BUFSTR, *, ERR=902 ) EVINT, EVSTART
        IF ( EVINT .NE. 0 ) EVSTART = MOD ( EVSTART, EVINT )
        CALL NL_MN()
        CALL NL_MN()
        CALL ADDSTR_F ( "Return to continue" )
        CALL GETSTR_F ( BUFSTR )
        CALL END_MN()
        CALL USE_GLBFIL_4 ( 'OWC' )
        GOTO 50
903     CONTINUE
      CALL END_MN()
      CALL START_MN()
      CALL SETCR_MN( 0, 0 )
      CALL ADDSTR_F ( "Enter name of user constraints program (Return if none): " )
      CALL GETSTR_F ( USER_CONST_PROG )
      if ( USER_CONST_PROG .NE. ' ' ) then
           KUSER_CONST = .TRUE.
           LNAME = USER_CONST_PROG(:TRIMLEN(USER_CONST_PROG))
           CALL BIN_EXIST ( LNAME, KEXIST )
           IF ( .NOT. KEXIST ) THEN
                CALL ADDSTR_F ( 'Program not found: Type Return to continue' )
                KUSER_CONST = .FALSE.
                USER_CONST_PROG=' '
                CALL GETSTR_F ( LTMP )
           ENDIF
        ELSE
           KUSER_CONST = .FALSE.
      ENDIF
      CALL END_MN()
      CALL USE_GLBFIL ( 'OWC' )
      goto 6000
!
! === Flyby branch
!
 950  CONTINUE
!
! --- Handle FLYBY warning toggle logic
!
        IF ( FLYBY_WARNING ) THEN
             FLYBY_WARNING = .FALSE.
             G_WARNING     = .FALSE.
           ELSE
             G_WARNING     = .TRUE.
             FLYBY_WARNING = .TRUE.
        ENDIF
        CALL USE_COMMON   ( 'OWC' )
        CALL USE_GLBFIL_4 ( 'OWC' )
      GOTO 50
!
!
 951  CONTINUE
      IFLYDEF_SAVE = 0
      IFLYDEF = 0
      IF ( CCHAR(4:4) .EQ. 'F') THEN
           IFLYDEF = 1                !Turn on flyby default setup
           IFLYDEF_SAVE = 1
      END IF
      CALL END_MN()
      CALL USE_BUFFER ( IFLYDEF, INT2(1), 'OWC' )
      CALL RUN_PROG   ( 'FLOPT', 'WAIT', INT2(0) )
      CALL USE_BUFFER ( IFLYREP, INT2(1), 'ORC' )
!
! --- Reload socom.i since FLOPT might have changed it
!
      CALL USE_COMMON ( 'ORC' )
!
      CDUM = 'NONE'
      DDUM = 1.0D0
      SRC_SUBSTITUTE_INIT = .FALSE. 
      SIT_SUBSTITUTE_INIT = .FALSE.
      VEL_SUBSTITUTE_INIT = .FALSE.
      CALL FLYBY_INIT ( STASUB_CHR, SRCSUB_CHR, NUTSRS_CHR, NUTDLY_CHR, &
     &                  EOPDLY_CHR, PLTMOD_CHR, TIME0, &
     &                  VELSUB_CHR, CDUM, CDUM, CDUM, CDUM, DDUM, &
     &                  AXOSUB_CHR, CDUM, CDUM, CDUM )
!
      CALL START_MN()
!
! --- Decode reply
!
      IFLYREP(2)=KBITN( IFLYREP, INT2(2) )
      IF ( KBIT( IFLYREP, INT2(1) ) ) THEN
           IFLYREP(1)=1
        ELSE
           IFLYREP(1)=0
      ENDIF
!
      IFLYDEF= IFLYREP(2)
      IF (IFLYREP(1) .EQ. 1) GO TO 801
      GO TO 6000
!
!     TERMINATE OR PROCESS STATION DEPENDENT CALIBRATIONS
!
  926 CONTINUE
!
!     SEE IF USER WANTS TO SPECIFY WHICH DATA BASE HE SEES FIRST
!
      call end_mn()
      NUMDB = 0
!
      IACCOR(1)=NUMDB
      IACCOR(2)=1      !SELCOR
      CALL USE_BUFFER( IACCOR, INT2(2), 'OWC' )
      CALL RUN_PROG( 'ACCOR', 'WAIT', INT2(0) )
      CALL USE_BUFFER( ISQR, INT2(1), 'ORC' )
!
!     If ISQR is 1, user chose to perform least squares, while he
!     was in SELCOR.  If ISQR is 2, the CORFIL had a discrepancy
!     which generated a fatal error.  If ISQR is 3, the user chose CRES.
!
      IF (ISQR .EQ. 1) GO TO 801
      IF (ISQR .EQ. 2) GO TO 2000
      IF (ISQR .EQ. 3) THEN
        CCHAR(4:4)='@'
        GO TO 3850
      ENDIF
      GO TO 6000
!
!**** CHANGE THE PRINT OR CRT LU    ***OR****
!     change plotting of earth orientation adjustments
!
  970 CONTINUE
!
      kcentermass = .not.kcentermass
      if ( kcentermass) then
           call end_mn()
           call start_mn()
           call setcr_mn( 0, 0 )
           call &
     &          addstr_f("Enter name of station weight file (Return for default): " )
           call getstr_f(bufstr )
           IF ( bufstr.ne.' ' ) THEN
                lname = PRE_SAV_DIR(:PRE_SV_LEN)//'/'//BUFSTR
                call bin_exist(lname,kexist )
                if (.not.kexist) then
                    call addstr_f('File not found: Type Return to continue' )
                    call getstr_f(ltmp )
                  else
                    sta_wt_fil = bufstr
                endif
             else
               sta_wt_fil = STATION_WEIGHT_FILE
           ENDIF
!
! -------- Turn on all stations when no net translation option is selected
!
           DO I=1,NUMSTA
              DO J=1,3
                 CALL SBIT ( LSITEC(1,J), I, INT2(1) )
              ENDDO
           ENDDO
           CALL USE_COMMON ( 'OWC' )
        ELSE
!
! -------- Restoring master stations ( whose coordinates are not estimated )
!
           CALL USE_PARFIL ( 'ORC' )
           PARXC_FLAG = '!'
           KSTON = .TRUE.
           CALL PARXC ( PARXC_FLAG, KERR )
           CALL PARCN()
           CALL USE_COMMON ( 'OWC' )
      ENDIF
      CALL USE_GLBFIL_4 ( 'OWC' )
      GOTO 6000
!
  975 CONTINUE
!
      ltest = .not.kspool
      call set_spool(ltest )
      iprnt=1
      if (kspool) iprnt=23
      CALL USE_GLBFIL('OWC' )
      GO TO 6000
!
 978  continue
      CALL USE_SPOOL('OIC' )
      GO TO 6000
 979  continue
      CALL END_MN()
      IUER = -1
      CALL USE_GLBFIL_4 ( 'ORC' )
!
! --- Releaase VTD_ADR it it held dynamic memory
!
      IF ( VTD_ADR .NE. 0 ) THEN
           IUER = -1
           CALL VTD_QUIT ( %VAL(VTD_ADR), IUER )
           CALL FREE ( VTD_ADR )
           VTD_ADR = 0
           CALL USE_GLBFIL_4 ( 'OWC' )
      END IF
!
! --- Grab dynamic memory for VTD object
!
      SIZEOF_VTD = SIZEOF(VTD_PTR)
      IUER = -1
      CALL GRAB_MEM ( IUER, MEM_LEN, MEM_ADR, 1, INT8(SIZEOF_VTD), VTD_ADR )
      IF ( IUER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( SIZEOF_VTD, STR )
           CALL ERR_LOG ( 8869, -2, 'OPTIN', 'Failure to allocate '// &
     &          STR(1:I_LEN(STR))//' bytes of dynamic memory' )
           CALL EXIT ( 1 )
      END IF
!
! --- Update theoretical model for delay, delay rate and partial derivatives
!
      IUER = -1
      CALL UPDATE_THEOR_DELAY ( %VAL(VTD_ADR), IUER )
!
! --- Free dynamic memoory grabbed for the VTD object
!
      CALL FREE_MEM ( MEM_ADR )
      IF ( IUER .NE. 0 ) CALL EXIT ( 1 )
      VTD_ADR = 0
!
! --- Restart curses
!
      CALL START_MN()
      GOTO 6000
  976 continue
      IF(IEOPL .eq. 0) then
        IEOPL = 2
        CALL USE_SPLLK(PRE_SCR_DIR,EOPL_BASE,EOPL_LU,'OSC' )
      ELSE
        IEOPL = 0
      ENDIF
      CALL USE_COMMON('OWC' )
      GO TO 6000
977   continue
      if(ieopl.ne.0) IEOPL = 1
      CALL USE_SPLLK(PRE_SCR_DIR,EOPL_BASE,EOPL_LU,'OIC' )
      CALL USE_COMMON('OWC' )
      GO TO 6000
1100  continue
      IF ( CCHAR(4:4) .EQ. '&' ) goto 1150
      CALL END_MN()
      CALL START_MN()
      CALL SETCR_MN ( 0, 0 )
      CALL ADDSTR_F ( "Enter name of user partial program (Return if none) >> " )
      CALL GETSTR_F ( USER_PART_PROG )
      CALL CHASHL   ( USER_PART_PROG )
      IF ( USER_PART_PROG(1:1) .NE. ' ' ) THEN
           KUSER_PART = .TRUE.
           LNAME = USER_PART_PROG(:TRIMLEN(USER_PART_PROG))
           CALL BIN_EXIST ( LNAME, KEXIST )
           IF ( .NOT. KEXIST ) THEN
                LNAME = PRE_SOL_DIR(1:PRE_SOL_LEN)// &
     &                  USER_PART_PROG(1:TRIMLEN(USER_PART_PROG))
                CALL BIN_EXIST ( LNAME, KEXIST )
           END IF
           IF ( .NOT. KEXIST ) THEN
                CALL ADDSTR_F ( 'Program not found: Type Return to continue' )
                KUSER_PART = .FALSE.
                USER_PART_PROG = ' '
                CALL GETSTR_F ( LTMP )
           ENDIF
         ELSE
           ARC_USER_PART  = 0
           NUM_USER_PART  = 0
           USER_PART_PROG = ' '
           KUSER_PART     = .FALSE.
      ENDIF
      CALL END_MN()
      CALL USE_GLBFIL ( 'OWC' )
      GOTO 6000
1150  CONTINUE
!
      CALL END_MN()
      CALL START_MN()
      CALL SETCR_MN ( 0, 0 )
      CALL ADDSTR_F ( "Enter name of user program (Return if none) >> " )
      CALL GETSTR_F ( USER_PRO )
      CALL CHASHL   ( USER_PRO )
      IF ( USER_PRO(1:1) .NE. ' ' ) THEN
           LNAME = USER_PRO(1:TRIMLEN(USER_PRO) )
           CALL BIN_EXIST ( LNAME, KEXIST )
           IF ( .NOT. KEXIST ) THEN
                LNAME = PRE_SOL_DIR(1:PRE_SOL_LEN)// &
     &                  USER_PRO(1:TRIMLEN(USER_PRO))
                CALL BIN_EXIST ( LNAME, KEXIST )
           END IF
           IF ( .NOT. KEXIST ) THEN
                LNAME = PRE_SOL_DIR(1:PRE_SOL_LEN)// &
     &                  USER_PRO(1:TRIMLEN(USER_PRO))
                CALL BIN_EXIST ( LNAME, KEXIST )
           END IF
           IF ( KEXIST ) THEN
                CALL NL_MN()
                CALL ADDSTR_F ( "Enter name of user buffer (Return if none)>> " )
                USER_BUF=' '
                CALL GETSTR_F(USER_BUF )
             ELSE
                CALL ADDSTR_F ( "Program not found; type Return to continue" )
                USER_PRO=' '
                USER_BUF=' '
                CALL GETSTR_F(LTMP )
           ENDIF
        ELSE
           USER_BUF = ' '
      ENDIF
      CALL END_MN()
      CALL USE_COMMON ( 'OWC' )
      GOTO 6000
!
! === SECOND PAGE ===
!
 1001 continue
      KPAGE = 2
      call end_mn()
      call start_mn()
      CALL SETCR_MN ( 0, 0 )
!
!**** WRITE OUT OPTIONS
1011  continue
      call nl_mn()
      call nl_mn()
      call addstr_f("(=) Data Decimation" )
      call addstr_f("              (Y) User Constraints " )
      call reverse_on_mn()
      if (kuser_const) then
        call addstr_f(" on" )
        call reverse_off_mn()
        write(bufstr,'(": ",A20)') user_const_prog
        call addstr_f(bufstr(:trimlen(bufstr)) )
      else
        call addstr_f("off" )
        call reverse_off_mn()
        call addstr_f("      " )
      endif
      call nl_mn()
      call nl_mn()
      call addstr_f("(I) Ionosphere calibrations" )
      call addstr_f("      (V) Source weights " )
      call reverse_on_mn()
      if (source_weight_file.ne.' ') then
        call addstr_f(" on" )
        call reverse_off_mn()
        write(bufstr,'(": ",A)') source_weight_file
        call addstr_f(bufstr(:trimlen(bufstr)) )
      else
        call addstr_f("off" )
        call reverse_off_mn()
      endif
      call nl_mn()
      call nl_mn()
      call &
     &     addstr_f("(@) Compute residuals            (?) Special CRES options" )
      call nl_mn()
      call nl_mn()
      call addstr_f("(^) List solution" )
      call addstr_f("                (Z) Source Constraint " )
      call reverse_on_mn()
      if (ksrc_const) then
        call addstr_f(" on" )
      else
        call addstr_f("off" )
      endif
      call reverse_off_mn()
      call nl_mn()
      call nl_mn()
      call addstr_f("([) IOS estimation " )
      CALL REVERSE_ON_MN()
      IF ( IOS_EST == IOS__SES ) THEN
           CALL ADDSTR_F ( "ses" )
        ELSE IF ( IOS_EST == IOS__STA ) THEN
           CALL ADDSTR_F ( "sta" )
        ELSE IF ( IOS_EST == IOS__BAS ) THEN
           CALL ADDSTR_F ( "bas" )
        ELSE
           CALL ADDSTR_F ( "off" )
      END IF
      CALL REVERSE_OFF_MN()
      call addstr_f("           (]) IOS constraint " )
      CALL REVERSE_ON_MN()
      WRITE ( UNIT=STR(1:7), FMT='(F7.5)' ) IOS_SIG
      IF ( STR(7:7) == '0' ) THEN
           IF ( STR(6:6) == '0' ) THEN
                IF ( STR(5:5) == '0' ) THEN
                     IF ( STR(4:4) == '0' ) THEN
                          CALL ADDSTR_F ( STR(1:3) )
                        ELSE
                          CALL ADDSTR_F ( STR(1:4) )
                     END IF
                   ELSE
                     CALL ADDSTR_F ( STR(1:5) )
                END IF
              ELSE
                CALL ADDSTR_F ( STR(1:6) )
           END IF
         ELSE
           CALL ADDSTR_F ( STR(1:7) )
      END IF
      CALL REVERSE_OFF_MN()
!
      CALL NL_MN()
      CALL NL_MN()
      CALL ADDSTR_F ( "(J) Minimum SNR " )
      WRITE ( UNIT=BUFSTR(1:15), FMT='("X: ", F4.1, " S: ", F4.1)' ) &
     &        SNR_MIN_X, SNR_MIN_S
      CALL REVERSE_ON_MN()
      CALL ADDSTR_F ( BUFSTR(1:15) )
      CALL REVERSE_OFF_MN()
!
      CALL ADDSTR_F ( "  (K) Elevation-dependent Noise " )
      CALL REVERSE_ON_MN()
      IF ( KELDEP_NOISE ) THEN
           CALL ADDSTR_F("ON " )
           CALL REVERSE_OFF_MN()
           WRITE(BUFSTR,'(": ",a)') ELDEP_FILE
           CALL ADDSTR_F(BUFSTR(:TRIMLEN(BUFSTR)) )
        ELSE
           CALL ADDSTR_F ( "off" )
           CALL REVERSE_OFF_MN()
      ENDIF
!
      call nl_mn()
      call nl_mn()
      call addstr_f("(:) Rewind EOP Plot File" )
      call addstr_f("         ( ) Earth Orientation Plot File " )
      call reverse_on_mn()
      if (ieopl.ne.0) then
        call addstr_f("on " )
      else
        call addstr_f("off" )
      endif
      call reverse_off_mn()
      call nl_mn()
      call nl_mn()
      call reverse_off_mn()
      call addstr_f ( "(|) Fast mode switch " )
      call reverse_on_mn()
      call addstr_f ( FM_STR(FM_POI)(1:FM_LEN(FM_POI)) )
      call reverse_off_mn()
      call addstr_f ( FM_STR(FM_POI)(FM_LEN(FM_POI)+1:) )
!
      call addstr_f ( '(") Fast debug mode switch  ' )
      call reverse_on_mn()
      call addstr_f ( FD_STR(FD_POI)(1:FD_LEN(FD_POI)) )
      call reverse_off_mn()
      call addstr_f ( FD_STR(FD_POI)(FD_LEN(FD_POI)+1:) )
!
      call nl_mn()
      call nl_mn()
!C
      call reverse_off_mn()
      call addstr_f ( "({) Full adjustment list " )
      call reverse_on_mn()
      IF ( SEG_OUTPUT ) THEN
           CALL ADDSTR_F ( " on" )
        ELSE
           CALL ADDSTR_F ( "off" )
      ENDIF
      call reverse_off_mn()
      call addstr_f ( "     (\) Automatic outliers elimination\restoration" )
!
      call nl_mn()
      call nl_mn()
!
      WRITE ( BUFSTR,130 )
  130 FORMAT( "Reset: Sit(E)s   (S)ources   (L)ast page   (B)aselines", &
     &        "   (X)data bases")
      CALL ADDSTR_F ( BUFSTR )
!
      IF ( NEED_2 ) THEN
           CALL NL_MN()
           CALL NL_MN()
           IF ( SIMULATION_TEST ) THEN
                CALL ADDSTR_F ( "(.) Use all data (simulation only): " )
                CALL REVERSE_ON_MN()
                IF ( ALL_SIM_FLG ) THEN
                     CALL ADDSTR_F ( "ON " )
                   ELSE
                     CALL ADDSTR_F ( "OFF" )
                ENDIF
                CALL REVERSE_OFF_MN()
                CALL ADDSTR_F ( "    " )
           ENDIF
           CALL ADDSTR_F ( "(N) Return to first page of options" )
!
! -------- Writing active initials at the screen
!
           CALL SETCR_MN ( 0, 0 )
           CALL ADDSTR_F ( "Active initials: "//PRE_LETRS )
!
! -------- Writing version number at the screen
!
           STR = GET_VERSION()
           CALL SETCR_MN ( 79-I_LEN(STR), 0 )
           CALL REVERSE_ON_MN()
           CALL ADDSTR_F ( STR(1:I_LEN(STR)) )
           CALL REVERSE_OFF_MN()
      ENDIF
      IF ( .NOT. NEED_2 ) GOTO 200
!
!**** SEE WHAT THE USER WANTS TO DO
 1110 CONTINUE
      CALL SETCR_MN   ( IXX,  IYY )
!
! --- Reading from keyboard one symbol
!
      CALL SENKR_MN ( IXX, IYY, ICHAR4 )
      IF ( ICHAR4 .EQ. 13 ) CCHAR=' '
      IX = IXX
      IY = IYY
      IF ( CCHAR(4:4) .EQ. 'N' ) THEN
           KPAGE=1
           CALL END_MN()
           GOTO 50
      ENDIF
!
      if (cchar(4:4).eq.' ') then
        if ( iy.eq. 2 ) then
             if ( ix.le.18  ) cchar(4:4) = '='
             if ( ix.ge.33  ) cchar(4:4) = 'Y'
          else if ( iy.eq.4 ) then
             if ( ix.le.26  ) cchar(4:4) = 'I'
             if ( ix.ge.33  ) cchar(4:4) = 'V'
          else if ( iy.eq.6 ) then
             if ( ix.le.20  ) cchar(4:4) = '@'
             if ( ix.ge.33  ) cchar(4:4) = '?'
          else if ( iy.eq.8 ) then
             if ( ix.le.16  ) cchar(4:4) = '^'
             if ( ix.ge.33  ) cchar(4:4) = 'Z'
          else if ( iy.eq.10 ) then
             if ( ix.le.21  ) cchar(4:4) = '['
             if ( ix.ge.33  ) cchar(4:4) = ']'
          else if ( iy.eq.12 .and. ix.le.18 ) then
             IF ( IX .GE. 34 ) CCHAR(4:4) = 'K'
             IF ( IX .LE. 31 ) CCHAR(4:4) = 'J'
          else if ( iy.eq.14 ) then
             if ( ix.ge.1  .and. ix.le.18 ) cchar(4:4) = ':'
          else if ( iy.eq.16 ) then
             if ( ix .le. 32 ) cchar(4:4) = '|'
             if ( ix .gt. 33 ) cchar(4:4) = '"'
          else if ( iy.eq.18 ) then
             if ( ix .le. 32 ) cchar(4:4) = '{'
             if ( ix .gt. 33 ) cchar(4:4) = '\'
          else if ( iy.eq.20 ) then
             if ( ix.ge.7  .and. ix.le.13 ) cchar(4:4) = 'E'
             if ( ix.ge.17 .and. ix.le.25 ) cchar(4:4) = 'S'
             if ( ix.ge.29 .and. ix.le.39 ) cchar(4:4) = 'L'
             if ( ix.ge.43 .and. ix.le.53 ) cchar(4:4) = 'B'
             if ( ix.ge.57 .and. ix.le.69 ) cchar(4:4) = 'X'
          else if ( iy.gt.20 ) then
             kpage=1
             ixx = 1
             iyy = 22
             GOTO 50
        endif
      endif
      goto 222
 1502 CONTINUE
      CALL END_MN()
      CALL START_MN()
 1504 CONTINUE 
      CALL SETCR_MN (  0, 0 )
      WRITE ( UNIT=BUFSTR(1:70), &
     &        FMT='("Current X: ", F4.1, " S: ", F4.1, &
     &        "  Enter Minimal SNR for X band and S-band: " )' ) &
     &        SNR_MIN_X, SNR_MIN_S
      CALL ADDSTR_F ( BUFSTR(1:I_LEN(BUFSTR))//' ' )
      CALL GETSTR_F ( STR )
      IF ( ILEN(STR) == 0 ) GOTO 6000
      CALL EXWORD ( STR, MIND, LIND, IND, CHAR(32)//CHAR(9)//',', IUER )
      STR1 = STR(IND(1,1):IND(2,1))
      IF ( INDEX( STR1, '.' ) == 0 ) STR1 = STR1(1:I_LEN(STR1))//'.'
      READ ( UNIT=STR1, FMT='(F5.2)', IOSTAT=IUER ) SNR_MIN_X
      IF ( IUER .NE. 0 ) THEN
           CALL NL_MN ( )
           CALL NL_MN ( )
           CALL ADDSTR_F ( "Error in parsing the input "//STR1 )
           GOTO 1504
      END IF
      IF ( LIND .GE. 2 ) THEN
           STR2 = STR(IND(1,2):IND(2,2))
           IF ( INDEX(STR2, '.' ) == 0 ) STR2 = STR2(1:I_LEN(STR2))//'.'
           READ ( UNIT=STR2, FMT='(F5.2)', IOSTAT=IUER ) SNR_MIN_S
           IF ( IUER .NE. 0 ) THEN
                CALL NL_MN ( )
                CALL NL_MN ( )
                CALL ADDSTR_F ( "Error in parsing the input "//STR2 )
                GOTO 1504
           END IF
      END IF
!
      CALL USE_GLBFIL_4( 'OWC' )
      CALL UPDATE_SUPSTAT ( )
      GOTO 6000
 1506 CONTINUE
!
! --- Toggle elevation-dependent noise
!
      keldep_noise = .not.keldep_noise
      if (keldep_noise) then
      call end_mn()
      call start_mn()
      call setcr_mn (  0, 0 )
      call &
     &     addstr_f("Enter name of el dependent noise file(Return if none): " )
      call getstr_f(eldep_file )
      IF(eldep_file.ne.' ') THEN
        lname = eldep_file(:trimlen(eldep_file))
        call bin_exist(lname,kexist )
        if (.not.kexist) then
          call addstr_f('File not found: Type Return to continue' )
          keldep_noise = .FALSE.
          eldep_file=' '
          call getstr_f(ltmp )
        endif
      endif
      call end_mn()
      endif
      call use_glbfil_4('OWC' )
      goto 6000
!
!**** SCHEDULE CORRN WITH WAIT
 1600 CONTINUE
      call end_mn()
      CALL RUN_PROG( 'CORRN', 'WAIT', INT2(0) )
      CALL USE_BUFFER( ISQR, INT2(1), 'ORC' )
!
!     See if the user wants to do least squares (ISQR = 1)
!
      IF (ISQR .EQ. 1) GO TO 801
      GO TO 6000
!
!**** SCHEDULE ACCOR WITH WAIT
 1700 CONTINUE
      call end_mn()
      NUMDB = 0
      IF (CCHAR .EQ. '1') NUMDB = 1
      IF (CCHAR .EQ. '2') NUMDB = 2
      IF (CCHAR .EQ. '3') NUMDB = 3
      IF (CCHAR .EQ. '4') NUMDB = 4
!
      IACCOR(1)=NUMDB
      IACCOR(2)=2         !OBCOR
      CALL USE_BUFFER( IACCOR, INT2(2), 'OWC' )
      CALL RUN_PROG( 'ACCOR', 'WAIT', INT2(0) )
      CALL USE_BUFFER( ISQR, INT2(1), 'ORC' )
!
!     If ISQR is 1, user decided to perform least squares, while
!     he was in OBCOR. If ISQR is 2, the CORFIL had a discrepancy,
!     which generated a fatal error. If ISQR is 3, the user decided to
!     calculate residuals.
!
      IF (ISQR .EQ. 1) GO TO 801
      IF (ISQR .EQ. 2) GO TO 2000
      IF (ISQR .EQ. 3) THEN
        CCHAR(1:1)='@'
        GO TO 3850
      ENDIF
      GO TO 6000
!
!**** MODIFY THE DATA BASE OBSERVATIONS
!
 1800 CONTINUE
      CALL END_MN()
      CALL RUN_PROG  ( 'NEWDB', 'WAIT', INT2(0) )
      CALL USE_BUFFER( ITERM, INT2(1), 'ORC' )
      IF (ITERM .EQ. 1) GO TO 802
      GO TO 6000
!
 1850 CONTINUE
      CALL END_MN()
      CALL NOUT ( 128, IPASS )
      IPASS(1) = 1
      CALL USE_BUFFER( IPASS, INT2(64), 'OWC' )
      CALL RUN_PROG  ( 'UPTDB', 'PASS', INT2(0) )
      GO TO 6000
 1870 CONTINUE
      CALL SETCR_MN (  0, 6 )
      WRITE ( UNIT=BUFSTR(1:70), &
     &        FMT='("Current repostitory: ", A, &
     &        "  Enter new repository: " )' ) &
     &        VCAT_REPO
      CALL ADDSTR_F ( BUFSTR(1:I_LEN(BUFSTR))//' ' )
      CALL GETSTR_F ( STR )
      IF ( ILEN(STR) == 0 ) GOTO 6000
      CALL TRAN ( 11, STR, STR )
      CALL USE_GLBFIL_4 ( 'OR'  )
      VCAT_REPO = STR
      CALL USE_GLBFIL_4 (  'WC' )
      GO TO 6000
 1900 CONTINUE
!
! --- Fast mode switch
!
      FM_POI = FM_POI + 1
      IF ( FM_POI .GT. FMI_VAR ) FM_POI = 1
      CALL USE_GLBFIL_4 ( 'OR'  )
      FAST_MODE     = FM_VAL(FM_POI)
      FAST_MODE_GLO = FAST_MODE
      CALL USE_GLBFIL_4 (  'WC' )
      GO TO 6000
 1910 CONTINUE
!
! --- Fast debug mode switch
!
      FD_POI = FD_POI + 1
      IF ( FD_POI .GT. FDI_VAR ) FD_POI = 1
      CALL USE_GLBFIL_4 ( 'OR'  )
      FAST_DBG = FD_VAL(FD_POI)
      FAST_DBG_GLO = FAST_DBG
      CALL USE_GLBFIL_4 (  'WC' )
      GO TO 6000
!
! --- Toggle the ADJUST list control
!
 1920 CONTINUE
      IF ( SEG_OUTPUT ) THEN
           SEG_OUTPUT = .FALSE.
        ELSE
           SEG_OUTPUT = .TRUE.
      ENDIF
      CALL USE_GLBFIL_4 ( 'OWC' )
      GOTO 50
 1930 CONTINUE
!
! ----- Automatic outlier's elimination and re-weighting using UPWEI algorithm
!
        CALL CLEAR_MN ( )
        CALL END_MN()
        CALL UN_CURSES ( )
        CALL CLEAR ( 0, 0 )
!
! ----- Call the program for outliers rejection or restoring observations which
! ----- have been rejected earlier as outliers.
!
        CALL NOUT ( 128, IBUFF )
        CALL USE_BUFFER ( IBUFF, INT2(64), 'OWC' )
        CALL RUN_PROG   ( 'ELIM', 'WAIT', INT2(0) )
        CALL USE_BUFFER ( ICRES, INT2(1), 'ORC' )
!
! ----- Reread socom, since ELIM may change it
!
        CALL USE_COMMON ( 'ORC' )
        CGM_TYPE = .FALSE.  !  Set once more session-type of socom
        CALL SOCOM_EXT()
        IF ( ICRES .EQ. 0 ) GOTO 6000  !  Displaying  OPTIN menu again
        IF ( ICRES .EQ. 1 ) GOTO 3850  !  Scheduling  CRES
  802 CONTINUE
!
! --- Code for termination from NEWDB
!
      CALL RUN_PROG ( 'SLEND', 'PASS', INT2(0) )
!
! --- Reschedule ADJST
!
 1940 CONTINUE
      CALL END_MN()
      BUFF2_WORDS = (   LOC(BUFF2_I2_FILLER) - LOC(LCHAO) &
     &                + SIZEOF(BUFF2_I2_FILLER) )/2
      CALL USE_BUFFER ( LCHAO, BUFF2_WORDS, 'OWC' )
      CALL RUN_PROG   ( 'ADJST', 'PASS', INT2(0) )
!
! --- Toggle weak source constraint
!
 1945 CONTINUE
      KSRC_CONST = .NOT. KSRC_CONST
      CALL USE_GLBFIL ( 'OWC' )
      GOTO 6000
 1950 CONTINUE
      ALL_SIM_FLG = .NOT. ALL_SIM_FLG
      CALL USE_GLBFIL ( 'OWC' )
      GOTO 6000
 1960 CONTINUE
!
! ----- Scheduling Alternative graphic utililty.
! ----- Check whetehr its name is defined
!
        CALL GETENVAR  ( 'SOLVE_ALTGRAPHIC', STR )
        IF ( ILEN(STR) .GT. 0 ) THEN
!
! ---------- Executable file exists? Good. Lets remove curses spell ...
!
             CALL CLEAR ( 0, 0 )
             CALL END_MN()
             CALL UN_CURSES ( )
!
! ---------- ... and  launch this alternative program!
!
             WRITE ( 6, * ) 'Launching alternative graphic program  >>', &
     &                       STR(1:I_LEN(STR)),'<< '
             CALL RUN_PROG   ( STR(1:I_LEN(STR)), 'PASS', INT2(1) )
        END IF
        GOTO 50
 1970 CONTINUE
!
! --- Action: run Solve, compute fiel residuals and write them into a file
!
      CALL END_MN()
      CALL UN_CURSES ( )
!
! --- Rewind spool file
!
      CALL SET_SPOOL( TRUE_L2 )
      CALL USE_GLBFIL('OWC' )
      IF ( KSPOOL ) IPRNT=23
      CALL USE_SPOOL('OIC' )
      CALL SBIT ( IPRES, INT2(1), INT2(1) )
      CALL USE_COMMON ( 'OWC' )
      CALL SOCOM_EXT()
!
! --- Remove stale files with constraints, buil-in and user, local and global.
! --- This files will be re-created if necessary, but the old versions may
! --- inflict harm
!
      DO 470 J7=1,4
         FNAME = PRE_SCR_DIR(1:PRE_SD_LEN)//FIL_PREF(J7)//PRE_LETRS
         INQUIRE ( FILE=FNAME, EXIST=LEX )
         IF ( LEX ) THEN
              CALL UNLINK ( FNAME(1:I_LEN(FNAME))//CHAR(0) )
         END IF
 470  CONTINUE
!
! --- Set status of the solution
!
      CALL STATUS_SET  ( 'OPTIN', STA__INT )
      CALL SAVE_CFNAME ( '<interactive>' )
!
! --- Make LSQ solution
!
      CALL NOUT ( 128, IBUFF )
      CALL SBIT ( PRE_IP(3), INT2(4), INT2(1) ) ! Lift "screen" mode
      WRITE ( 6, * ) 'Running PROC'
      CALL RUN_PROG ( 'PROC', 'WAIT', INT2(0) )
      CALL USE_BUFFER ( IBUFF, INT2(1), 'ORC' )
      IF ( IBUFF(1) .EQ. 1 ) THEN
           WRITE ( 6, * ) 'Failure in PROC'
           CALL EXIT ( 1 )
      END IF
!
      WRITE ( 6, * ) 'Running NORML'
      CALL RUN_PROG ( 'NORML', 'WAIT', INT2(0) )
!
      CALL USE_BUFFER ( INT2(0), INT2(1), 'OWC' )
      WRITE ( 6, * ) 'Running CRES'
      CALL RUN_PROG ( 'CRES',  'WAIT', INT2(0) )
      CALL SBIT ( IPRES, INT2(1), INT2(0) )
      CALL USE_COMMON ( 'OWC' )
!
! --- Generate the name of the output file
!
      CALL CLRCH ( SPL_FILE )
      IF ( IDATYP == GX__DTP  ) THEN
           CALL TRAN ( 12, BAND_NAM(1), STR(1:1) )
           IF ( ILEN(BAND_NAM(1)) > 0 ) THEN
                SPL_FILE = TRIM(VLBI_DIR)//'/'//TRIM(EXP_CODE)//'/'//TRIM(EXP_CODE)//'_'//STR(1:1)//'_init.spl'
           END IF
         ELSE IF ( IDATYP == GS__DTP  ) THEN
           CALL TRAN ( 12, BAND_NAM(2), STR(1:1) )
           IF ( ILEN(BAND_NAM(1)) > 0 ) THEN
                SPL_FILE = TRIM(VLBI_DIR)//'/'//TRIM(EXP_CODE)//'/'//TRIM(EXP_CODE)//'_'//STR(1:1)//'_init.spl'
           END IF
      END IF
!
      IF ( ILEN(SPL_FILE) == 0 ) THEN
           WRITE ( 6, '(A)' ) ' Solution type: '//DATYP__ABR(1+6*IDATYP:6*(IDATYP+1))
           WRITE ( 6, '(A)' ) ' Spool file:    '//PRE_SPL_NAM(1:PRE_SPL_LEN)
         ELSE 
!
! -------- If the solution is in a single-band mode, copy the spool file
!
           STR = 'cp '//PRE_SPL_NAM(1:PRE_SPL_LEN)//' '//SPL_FILE
           IS = SYSTEM ( TRIM(STR)//CHAR(0) )
           IF ( IS .EQ. 0 ) THEN
                WRITE ( 6, '(A)' ) ' Solution listing with residuals is written in file '//TRIM(SPL_FILE)
              ELSE
                WRITE ( 6, '(A)' ) ' Error in an attempt to execute command '//TRIM(STR)
           END IF
      END IF
      CALL HIT_CONT ( %VAL(0), %VAL(0) )
      CALL SBIT ( PRE_IP(3), INT2(4), INT2(0) ) ! Set "screen" mode
      ISCREEN = 0
      GOTO 6000
 3850 CONTINUE
!
!     SCHEDULE CRES
!
      call end_mn()
      IF(IX.LE.37.OR.CCHAR(4:4).EQ.'@') CMENU=0
      IF(IX.GT.37.OR.CCHAR(4:4).EQ.'?') CMENU=1
      CALL USE_BUFFER( CMENU, INT2(1), 'OWC' )
      CALL RUN_PROG( 'CRES', 'PASS', INT2(0) )
!
!     DO S/X ION CORRECTION JWR FEB 3, 1981
!
 1000 CONTINUE
      IF ( IOS_EST == IOS__UNDF ) THEN
           IOS_EST = IOS__SES
        ELSE IF ( IOS_EST == IOS__SES ) THEN
           IOS_EST = IOS__STA
        ELSE IF ( IOS_EST == IOS__STA ) THEN
           IOS_EST = IOS__BAS
        ELSE IF ( IOS_EST == IOS__BAS ) THEN
           IOS_EST = IOS__UNDF
      END IF
      CALL USE_COMMON ( 'OWC' )
      GOTO 6000
 1024 CONTINUE
      CALL END_MN()
      CALL START_MN()
      CALL SETCR_MN ( 0, 0 )
 1025 CONTINUE 
      CALL ADDSTR_F ( "Enter IOS reciprical weigth constraint: " )
      CALL GETSTR_F ( STR )
      IF ( INDEX ( STR, '.' ) < 1 ) STR = TRIM(STR)//'.'
      READ ( UNIT=STR, FMT=*, IOSTAT=IER ) VAL_R8
      IF ( IER .NE. 0 .OR. VAL_R8 < 0.0 ) THEN
           CALL ADDSTR_F ( 'Error in conversion of '//TRIM(STR)//' -- a positie number was expeced' )
           CALL NL_MN ( )
           CALL NL_MN ( )
           GOTO 1025
      END IF
      IOS_SIG = VAL_R8
      CALL USE_COMMON ( 'OWC' )
      GOTO 6000
 1050 CONTINUE
      CALL END_MN()
      CALL START_MN()
      CALL SETCR_MN ( 0, 0 )
      call addstr_f("Enter name of source weight file (Return if none): " )
      call getstr_f(source_weight_file )
      IF(source_weight_file.ne.' ') THEN
        lname = source_weight_file(:trimlen(source_weight_file))
        call bin_exist(lname,kexist )
        if (.not.kexist) then
          call addstr_f('File not found: Type Return to continue' )
          source_weight_file=' '
          call getstr_f(ltmp )
        endif
      endif
      call end_mn()
      call use_glbfil_4('OWC' )
      goto 6000
!
!
!     Since the options are now all available on either page, use
!     KPAGE to return to the proper page when the option is finished.
!
 6000 CONTINUE
      goto 50
 7000 CONTINUE
!
! --- Restart MN
!
      CALL END_MN()
!
! --- Rather akward way to re-establish einvironment varaiable for the number
! --- of lines on the screen
!
      CALL GET_TERMSIZE ( SCLEN, SCWID ) 
      LINES_STR(1:6) = 'LINES='
      IF ( SCLEN .LT. 10  ) THEN
           WRITE ( UNIT=LINES_STR(7:7), FMT='(I1)' ) SCLEN
           LINES_STR(8:8)=CHAR(0)
           ILN = 8
        ELSE IF ( SCLEN .LT. 100 ) THEN
           WRITE ( UNIT=LINES_STR(7:8), FMT='(I2)' ) SCLEN
           LINES_STR(9:9)=CHAR(0)
           ILN = 9
        ELSE IF ( SCLEN .GE. 100 ) THEN
           WRITE ( UNIT=LINES_STR(7:9), FMT='(I3)' ) SCLEN
           LINES_STR(10:10)=CHAR(0)
           ILN = 10
      END IF
      CALL PUTENV ( LINES_STR(1:ILN) )
!
! --- Rather akward way to re-establish einvironment varaiable for the number
! --- of columns on the screen
!
      COLUMNS_STR = 'COLUMNS='
      IF ( SCWID .LT. 10 ) THEN
           WRITE ( UNIT=COLUMNS_STR(9:9), FMT='(I1)'  ) SCWID
           COLUMNS_STR(10:10) = CHAR(0)
           ILN = 10
        ELSE IF ( SCWID .LT. 100 ) THEN
           WRITE ( UNIT=COLUMNS_STR(9:10), FMT='(I2)' ) SCWID
           COLUMNS_STR(11:11) = CHAR(0)
           ILN = 11
        ELSE IF ( SCWID .GE. 100 ) THEN
           WRITE ( UNIT=COLUMNS_STR(9:11), FMT='(I3)' ) SCWID
           COLUMNS_STR(12:12) = CHAR(0)
           ILN = 12
      END IF
      CALL PUTENV ( COLUMNS_STR(1:ILN) )
      SCLEN__OLD = SCLEN
      SCWID__OLD = SCWID
!
! --- Hard restarting curser. Curser will read environment variables LINES and
! --- COLUMNS and its behaviour will depend on it.
!
      CALL START_MN()
      CALL SETCR_MN ( 0, 0 )
      GOTO 50
!
!**** TERMINATE THE PROGRAM
!
 2000 CONTINUE
      CALL USE_GLBFIL ( 'OWC' )
      CALL END_MN()
      CALL RUN_PROG   ( 'SLEND', 'PASS', INT2(0) )
 3000 CONTINUE
      END  !#!  OPTIN  #!#
