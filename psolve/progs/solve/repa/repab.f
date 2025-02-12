      PROGRAM REPAB
!
!     REsidual Plotting and Ambiguity resolution                                                                             |
!                                                                                                                            |
!     RUN STRING:   REPA xx  (xx is the user id for the workfiles) - in standalone mode                                      |
!     RUN FROM OPTIN MENU: type "P"                                                                                          |
!                                                                                                                            |
!     INPUT:               COMMxx, NAMFxx, OBSFxx, PARFxx, RESFxx, REPAxx (workfiles in $REPA_WORK resp. in $WORK_DIR)       |
!                          REPAxx (parameter file in  $REPA_WORK resp. in $WORK_DIR)                                         |
!                          repa.hlp (helptext in $SOLVE_HELP_DIR)                                                                  |
!                                                                                                                            |
!     OUTPUT:              OBSFxx and RESFxx (observation and residual workfiles)                                            |
!                                                                                                                            |
!     ENVIRONMENT:         if running from OPTIN:   SOLVE environment ( $WORK_DIR and $SOLVE_HELP_DIR )                            |
!                          in standalone mode:      two environmental variables ( $REPA_WORK, $SOLVE_HELP_DIR  )                   |
!                                                   $REPA_WORK: directory with input files                                   |
!                                                   $SOLVE_HELP_DIR:  directory with helpfile repa.hlp                             |
!                                                                                                                            |
!     TERMS:               G -- good -- good observations                                                                    |
!                          M -- recoverable -- manually suppressed -- manual deleted observations                            |
!                          B -- bad -- unrecoverable -- bad observations                                                     |
!                                                                                                                            |
!     SUBROUTINES AND FUNCTIONS:                                                                                             |
!                                                                                                                            |
!     REPCORD              reads COMMxx file   SOLVE workfile                                                                |
!     REPNARD              reads NAMFxx file   SOLVE workfile                                                                |
!     REPOBRD              reads OBSFxx file   SOLVE workfile  (observations)                                                |
!     REPPARD              reads PARFxx file   SOLVE workfile                                                                |
!     REPRERD              reads RESFxx file   SOLVE workfile  (residuals)                                                   |
!     REPPARM              reads REPAxx file   REPA user parameter file                                                      |
!     REPOBWT              writes OBSFxx file                                                                                |
!     REPREWT              writes RESFxx file                                                                                |
!     REPAMB8              calculate number of ambiguity steps                                                               |
!                                                                                                                            |
!     REPINFO              user function: puts information about the points into the bottom line of the current DiaGi plot   |
!     REPPTSH              user function: single ambiguity shifting                                                          |
!     REPGRSH              user function: group ambiguity shifting                                                           |
!     REPGRRS              user function: ambiguity reset for all baselines to the original state                            |
!     REPPTSU              user function: single suppression/recover                                                         |
!     REPGRSU              user function: group suppression/recover                                                          |
!     REPCONN              user function: set and delete of connecting lines                                                 |
!     REPBASL              user function: set DiaGi flags                                                                    |
!     REPGOOD              user function: set plotting area for good points only                                             |
!                                                                                                                            |
!     SET_VERSION          set the date of the version                                                                       |
!     GET_VERSION          set the date of the version                                                                       |
!     SUPR_INQ             function: get status of observations                                                              |
!                                                                                                                            |
!     REPCABL              reads the calibration status                                                                      |
!     REPEPOC              converts Julian Date into integer MONTH, DAY, YEAR, HOUR, MINUTE and SECOND                       |
!     REPSIGM              statisctics for sigma lines in DiaGi plots                                                        |
!     REPSCAN              find best pseudo click values for ambiguity shifting                                              |
!                                                                                                                            |
!     DIAGI_DEF            set DIAGI defaults (written by Leonid Petrov)                                                     |
!     MULTI_DIAGI          Multi Dialogue Graphics Interface (written by Leonid Petrov)                                      |
!     NOUT                 Clear DIAGI_S object (written by Leonid Petrov)                                                   |
!                                                                                                                            |
!     REPA_INT             fix numerical problem                                                                             |
!     GETARG               get argument from run string (FORTRAN routine)                                                    |
!     GETENV               get env. variable (FORTRAN routine)                                                               |
!     CHASHL               string --> left (written by Leonid Petrov)                                                        |
!     CLRCH                clear string  (written by Leonid Petrov)                                                          |
!     ILEN                 length of a character string (written by Leonid Petrov)                                           |
!     ERR_LOG              error routine (written by Leonid Petrov)                                                          |
!     PRE_PROG             interface to SOLVE                                                                                |
!     END_PROG             interface to SOLVE                                                                                |
!                                                                                                                            |
!     created by Volkmar Thorandt (VT) and Gerald Engelhardt (GE)                                                            |
!     Federal Agency for Cartography and Geodesy, Leipzig, Germany                                                           |
!                                                                                                                            |
!     HISTORY:                                                                                                               |
!     2002-09-18   VT & GE:   created                                                                                        |
!     2003-02-07   VT & GE:   1st release                                                                                    |
!     2003.05.14   pet        Added support of user function: set plotting                                                   |
!                             area for good observations only (CNTRL/M).                                                     |
!     2003.06.05   VT         Ambiguity resolution for the whole set of databases added.                                     |
!                                                                                                                            |
      IMPLICIT   NONE
!
! --- do not change the order of the following includes
!
      INCLUDE 'solve.i'                               ! CALC/SOLVE - include
      INCLUDE 'resfl.i'                               ! RESFxx     - include
      INCLUDE 'prfil.i'                               ! PARFxx     - include
      INCLUDE 'socom.i'                               ! COMMxx     - include
      INCLUDE 'precm.i'                               ! PRECxx     - include
      INCLUDE 'oborg.i'                               ! OBSFxx     - include
      INCLUDE 'obors.i'                               ! OBSFxx     - include
      INCLUDE 'diagi.i'                               ! DIAGI      - include
      INCLUDE 'repab.i'                               ! REPA       - include
!
      INTEGER*4  J1, J2, J3                           ! loop indices
      INTEGER*4  GET_UNIT                             ! find unit number (function)
      INTEGER*4  ILEN                                 ! length of a character string (function)
      INTEGER*4  IDEV                                 ! DiaGI device type
      INTEGER*4  IER, IUER                            ! universal error handler
      CHARACTER  STR*80                               ! string variable
      INTEGER*4  INT_TMP
      CHARACTER  STR_TMP*3                            ! string variable
!
      INTEGER*4  IBST, ILST, IOST, IPST, IWST         ! DiaGi parameter
      INTEGER*4  ICL1, ICL2, ICL3                     ! DiaGi parameter
      INTEGER*4  IBATCH                               ! DiaGi batch control parameter
      PARAMETER  ( IBATCH = 0 )                       ! DiaGi batch control parameter
      INTEGER*4  MCLRV                                ! copy of parameter M_CLR
!
      INTEGER*4  ICODE                                ! MultiDiaGi return code
      CHARACTER  BUT(MPB_NUM)*80                      ! button names
      CHARACTER  BUT_LET(MPB_NUM)*1                   ! button short cuts
      INTEGER*4  MPL                                  ! # of plots
      INTEGER*4  MPB                                  ! # of buttons
      INTEGER*4  NC                                   ! # of columns
      INTEGER*4  NR                                   ! # of lines
      CHARACTER  TITLE*80                             ! headlines of plots (MULTI_DIAGI)
      CHARACTER  TITS(PPPL_MAX)*32                    ! headlines of plots (DIAGI)
      TYPE ( DIAGI_STRU ) ::  DIAGI_S(PPPL_MAX)       ! record of DIAGI structure (see diagi.i)
      INTEGER*4  DIAGI_LEN                            ! length of DIAGI record
      INTEGER*4  IPAGE                                ! property page index
      CHARACTER  PROP*6                               ! current property code
      LOGICAL*4  FL_SCALE(PPPL_MAX)                   ! scale flag
      LOGICAL*4  JUMP                                 ! flag for changing the band
!
      CHARACTER  USER_ID*2                            ! user id for workfiles (e.g. VT in NAMFVT)
      INTEGER*4  IUSER_ID                             ! decimal ASCII values of USER_ID elements
      CHARACTER  WORK_DIR*200                         ! work directory
      INTEGER*4  WD_LEN                               ! length of WORK_DIR
      LOGICAL*4  LEX                                  ! aux. parameter for inquire
      CHARACTER  FILE_NAME_1*100                      ! file name variable
      CHARACTER  FILE_NAME_2*100                      ! file name variable
      INTEGER*2  REC_OBSF( JOBSREC_WORDS )            ! OBSFxx record (JOBSREC_WORDS s. solve.i)
      EQUIVALENCE ( REC_OBSF, FJD )
!
      INTEGER*4  REC_NUM(MAX_DBS+2)                   ! REC_NUM(1) # of loaded bands, REC_NUM(2) total # of records in NAMFxx,
!                                                       REC_NUM(n>2)  # of first record of bands in NAMFxx [s. REPNARD]
      INTEGER*4  RNB(MAX_DBS)                         ! # of last record in res. file RESFxx for databases [s. REPNARD]
      CHARACTER  DBFB(MAX_DBS)*9                      ! database names [s. REPNARD]
      CHARACTER  DBFBV(MAX_DBS)*4                     ! version numbers [s. REPNARD]
      CHARACTER  DBFBB(MAX_DBS)*1                     ! 'X' for X band, 'S' for S band
!
      INTEGER*4  BASL_INDEX(MAX_OBS,3)                ! baseline information (rec.#, index of 1st stat., index of 2nd stat.)
      INTEGER*4  BASL_NUM                             ! total # of useful baselines in IBAS
      CHARACTER  BASL_NUM_CH*3                        ! total # of useful baselines in IBAS (char)
      INTEGER*4  IBAS(BASL_MAX,4)                     ! array with useful baseline station pairs (1st & 2nd stat.#, bl.page #,
!                                               flag for reverse station order
      INTEGER*4  IPBL_MAX                             ! max. # of baseline pages (calculated from PPPL_MAX and BASL_MAX)
      INTEGER*4  BPAG_NUM                             ! # of baseline pages (max. index of BASL_PAG)
      CHARACTER  BPAG_NUM_CHR*2                       ! # of baseline pages (max. index of BASL_PAG) (character)
      INTEGER*4  BASL_PAG(BASL_MAX)                   ! # of baselines of baseline pages
      INTEGER*4  IBPAG                                ! index of current bl page in BASL_PAG
      CHARACTER  IBPAG_CHR*2                          ! index of current bl page in BASL_PAG (character)
      INTEGER*4  BASL_F                               ! 1st bl index in IBAS for current bl page IBPAG
      INTEGER*4  BASL_L                               ! last bl index in IBAS for current bl page IBPAG
      INTEGER*4  NUM_ALL(BASL_MAX)                    ! total #s of observations of baselines
      CHARACTER  NUM_ALL_CH*5                         ! total # of observations (char. string)
      INTEGER*4  NUM_G(BASL_MAX)                      ! #s of "good" observations in baselines (G, good)
      INTEGER*4  NUM_M(BASL_MAX)                      ! #s of "manually deleted" observations in baselines (M,recoverable)
      INTEGER*4  NUM_B(BASL_MAX)                      ! #s of "bad" observations in baselines (B, bad)
      INTEGER*4  EXP_INDEX                            ! index of current experiment (currently always EXP_INDEX=1)
      INTEGER*4  BAND_INDEX                           ! index of current band in BAND_NAME & BAND_RECS
      INTEGER*4  INEXT                                ! index of next band in BAND_NAME & BAND_RECS
      INTEGER*4  BAND_REC_F                           ! 1st record # (oservation #) of current band BAND_INDEX
      INTEGER*4  BAND_REC_L                           ! last record # (oservation #) of current band BAND_INDEX
      INTEGER*4  IRUNX                                ! flag for obs. status (good=0,recoverable=1,bad=2)
      LOGICAL*4  SUPR_INQ                             ! function: get suppression status
      CHARACTER  PS_NS(BASL_MAX)*2                    ! unit (ps/ns)
      CHARACTER  MEAN_CH*11                           ! WTD. Mean of baseline (character string)
      CHARACTER  VAR_CH*11                            ! RMS W.Res (character string)
      INTEGER*4  IBOBSM                               ! copy of parameter BOBS_MAX (repa.i) for delivery to user functions
!
      TYPE ( REPA_EXP ) ::  REPA_E(MAX_DBS)           ! record of REPA structure (see repa.i)
!
      REAL*8     SCAL_MIN(PPPL_MAX),                   & ! min. scale value - obs. minimum (delays,rates,etc.)
     &           SCAL_MAX(PPPL_MAX),                   & ! max. scale value - obs. maximum (delays,rates,etc.)
     &           FVAL8_G(BOBS_MAX,PPPL_MAX),           & ! values (residuals etc.) (good, G)
     &           FVAL8_M(BOBS_MAX,PPPL_MAX),           & ! values (residuals etc.) (recoverable, M)
     &           FVAL8_B(BOBS_MAX,PPPL_MAX),           & ! values (residuals etc.) (bad, B)
     &           FVAL8_A(BOBS_MAX,PPPL_MAX),           & ! values (residuals etc.) (all, A)
     &           FVAL8_S(BOBS_MAX,PPPL_MAX),           & ! values (residuals etc.) (special case)
     &           FERR8_G(BOBS_MAX,PPPL_MAX),           & ! errors (residuals etc.) (good, G)
     &           FERR8_M(BOBS_MAX,PPPL_MAX),           & ! errors (residuals etc.) (recoverable, M)
     &           FERR8_B(BOBS_MAX,PPPL_MAX),           & ! errors (residuals etc.) (bad, B)
     &           FERR8_A(BOBS_MAX,PPPL_MAX),           & ! errors (residuals etc.) (all, A)
     &           FERR8_S(BOBS_MAX,PPPL_MAX),           & ! errors (residuals etc.) (special case)
     &           AMB_SP(PPPL_MAX),                     & ! size of ambiguity (ps)
     &           AMB_SP_N(PPPL_MAX),                   & ! size of ambiguity (ns)
     &           TIME_G(BOBS_MAX,PPPL_MAX),            & ! time funktion (good)
     &           TIME_M(BOBS_MAX,PPPL_MAX),            & ! time funktion (recoverable)
     &           TIME_B(BOBS_MAX,PPPL_MAX),            & ! time funktion (bad)
     &           TIME_S(BOBS_MAX,PPPL_MAX),            & ! time funktion (special case - connecting lines)
     &           TIME(BOBS_MAX,PPPL_MAX),              & ! time funktion (all)
     &           TIM,                                  & ! Julian Date
     &           TIM1(PPPL_MAX),                       & ! fraction of TIM preceeding dot for NUM=1
     &           SEC,                                  & ! second
     &           MEAN(BOBS_MAX),                       & ! weighted mean residual
     &           VAR(BOBS_MAX),                        & ! rms weighted residual
     &           SIGD_LINE1(2,PPPL_MAX),               & ! sigma lines in plots
     &           SIGD_LINE2(2,PPPL_MAX),               & ! sigma lines in plots
     &           ZERO_TIME(2,PPPL_MAX),                & ! start end end time of zero line and sigma lines
     &           ZERO_LINE(2),                         & ! zero line in plots
     &           FTEMP(BOBS_MAX),                      & ! temporary array
     &           ETEMP(BOBS_MAX),                      & ! temporary array
     &           PI_VAL                                  ! pi
!
      INTEGER*2  IM, ID, IY, IHR, IMIN                ! year, month, day, hour, minute
      CHARACTER  TIME_CHR*16                          ! epoch of observation
      CHARACTER  TIME_CHR_1(2,PPPL_MAX)*16            ! epoch of 1st and last observation
      CHARACTER  INFO_CHR_G(BOBS_MAX,PPPL_MAX)*87     ! information to send to DiaGi via REPINFO - (G)
      CHARACTER  INFO_CHR_M(BOBS_MAX,PPPL_MAX)*87     ! information to send to DiaGi via REPINFO - (M)
      CHARACTER  INFO_CHR_B(BOBS_MAX,PPPL_MAX)*87     ! information to send to DiaGi via REPINFO - (B)
      CHARACTER  GMB(BOBS_MAX,PPPL_MAX)*1             ! flags for good(G), recoverable(M) or bad(B) observations
      INTEGER*4  REC_OBS(BOBS_MAX,PPPL_MAX)           ! record #s of all baseline observations
      INTEGER*4  REC_OBS_G(BOBS_MAX,PPPL_MAX)         ! record #s of "good" baseline observations
      INTEGER*4  REC_OBS_M(BOBS_MAX,PPPL_MAX)         ! record #s of "recoverable" baseline observations
      INTEGER*4  REC_OBS_B(BOBS_MAX,PPPL_MAX)         ! record #s of "bad" baseline observations
      CHARACTER  SUPKEY_S*1                           ! suppression flag for user function REPGRSU
      CHARACTER  SUPKEY_R*1                           ! recover flag for user function REPGRSU
      INTEGER*4  NUM_AMB(BOBS_MAX,PPPL_MAX)           ! tracks of shifting
      INTEGER*4  IWAY_DOWN                            ! # of ambiguity steps (down)
      INTEGER*4  IWAY_UP                              ! # of ambiguity steps (up)
      LOGICAL*4  FL_STANDALONE                        ! flag for run mode
      INTEGER*4  N_COL                                ! # of observation functions resp. colour in terms of DiaGi
      INTEGER*4  COL_S_IDX                            ! colour index for special case (connect points)
      CHARACTER  CONCON*1                             ! flag for setting of connecting lines (if 'C')
      CHARACTER  CONDEL*1                             ! flag for deleting of connecting lines (if 'D')
      CHARACTER  CONINP*1                             ! flag for user input for connecting lines (if 'I')
      CHARACTER  CONINI*1                             ! flag for minimum action of REPCONN for met. data
      CHARACTER  CH1_TMP*80                           ! temporary string
      CHARACTER  CH2_TMP*80                           ! temporary string
      CHARACTER  CH3_TMP*80                           ! temporary string
      CHARACTER  CH4_TMP*80                           ! temporary string
      INTEGER*4  I_TMP                                ! temporary integer
      INTEGER*4  FUNC_N                               ! # of user functions for current property
      CHARACTER  FUNC_B(FUNC_NUM)*8                   ! user function buttons for current property
      CHARACTER  FUNC_K(FUNC_NUM)*1                   ! keys for user functions
      INTEGER*4  FUNC_NNN                             ! copy of parameter FUNC_NUM (s. repa.i)
      CHARACTER  RESAMB *1                            ! flag for ambuguity reset for all baselines
      INTEGER*4  PPPL_MAX1                            ! copy of parameter in repa.i
!
      EXTERNAL   REPPTSU                              ! DiaGi user function - suppress/recover single points
      EXTERNAL   REPPTSH                              ! DiaGi user function - shift single point
      EXTERNAL   REPGRSH                              ! DiaGi user function - shift point group
      EXTERNAL   REPINFO                              ! DiaGi user function - information in bottom line
      EXTERNAL   REPGRSU                              ! DiaGi user function - shift point groups
      EXTERNAL   REPGRRS                              ! DiaGi user function - reset ambiguities
      EXTERNAL   REPCONN                              ! DiaGi user function - connect points
      EXTERNAL   REPBASL                              ! DiaGi user function - set DiaGi flags
      EXTERNAL   REPGOOD                              ! DiaGi user function - sets plotting area for good points only
      EXTERNAL   ILEN
!
      INTEGER*4  REPA_PAR, REPA_FLAG, BAS_IN
      INTEGER*4  REPA_NEXT_VAR, REPA_PREV_VAR
      INTEGER*4  IND_CAB                              ! index of cable calibration
      LOGICAL*4  AVL_ARR(MAX_ARC_STA)
      INTEGER*4  REPA_INT
      INTEGER*4  MD_IN_FLAG                           ! flag for changing baseline page by PgUp/PgDn
      INTEGER*4  I_LEN
      CHARACTER  GET_VERSION*54
!
      LOGICAL*4  ASHIFT                               ! flag for ambiguity resolving
      LOGICAL*4  IDP                                  ! flag for independent baselines
      REAL*8     FCLICK                               ! pseudo mouse click
      INTEGER*4  FCLICK_C                             ! correction factor for FCLICK ("independent" baselines)
      INTEGER*4  ISTEPS                               ! # of ambiguity steps
      INTEGER*4  ISTEPS_G(BOBS_MAX,PPPL_MAX)          ! # of ambiguity steps (good)
      INTEGER*4  ISTEPS_M(BOBS_MAX,PPPL_MAX)          ! # of ambiguity steps (recoverable)
      INTEGER*4  ISTEPS_B(BOBS_MAX,PPPL_MAX)          ! # of ambiguity steps (bad)
      REAL*8     TARGET(MAX_STA)                      ! vector of independent target values
      INTEGER*4  TARGET_F(MAX_STA)                    ! flag vector for TARGET ( -1 --> baseline empty)
      INTEGER*4  THEO_I                               ! theoretical mean values
      INTEGER*4  CALC_I                               ! calculated values after amb. shifting
      CHARACTER  WARN_CH*14                           ! warning message if difference to great
      INTEGER*4  LOC_EXT                              ! function (see bottom of repa source code)
      INTEGER*4  IARGC
!
      CHARACTER  REVORD*2                             ! message for reverse station order
      REAL*8     SCALE_MIN                            ! scaling value for met., cable, and ionosph. values
      REAL*8     SCALE_MAX                            ! scaling value for met., cable, and ionosph. values
      REAL*8     SCALE_NUL                            ! scaling value for met. and cable data
      CHARACTER  OUT1*32, OUT2*32, OUT3*32
      CHARACTER  IDA_DTP*2                            ! datatype group flag ('GX', 'GS', 'PX', 'PS')
      INTEGER*4  REPA__NO, REPA__PREV, REPA__NEXT
      PARAMETER ( REPA__NO   = 2001 )
      PARAMETER ( REPA__PREV = 2002 )         ! previous baseline
      PARAMETER ( REPA__NEXT = 2003 )         ! next baseline
!
      INCLUDE 'repa_version.i' ! Set revision date of the current version
      STR = GET_VERSION()
      WRITE ( 6, '(A)' ) STR(1:I_LEN(STR))
!
! --- check in which mode REPA is called: standalone or in Solve mode?
!
      IF ( IARGC () .LE. 1 ) THEN
           FL_STANDALONE = .TRUE.
!
! -------- get user id
!
           CALL GETARG ( 1, USER_ID )
           IF ( USER_ID .EQ. '  ' ) STOP 'no user id in command line'
           DO J1=1,2
              IUSER_ID = ICHAR( USER_ID(J1:J1) )
              IF ( IUSER_ID .GT. 96 .AND. IUSER_ID .LT. 122 ) &
     &        USER_ID(J1:J1) = CHAR( IUSER_ID - 32 )
           END DO
!
! -------- get REPA work directory
!
           CALL GETENVAR ( 'PSOLVE_WORK_DIR', WORK_DIR )
           CALL CHASHL   (  WORK_DIR )
           WD_LEN = ILEN( WORK_DIR )
           IF ( WD_LEN .EQ. 0 ) THEN
                STOP 'environment variable WORK_DIR not defined (solve workfiles)'
           END IF
           IF ( WORK_DIR( WD_LEN:WD_LEN ) .NE. '/' ) THEN
                WORK_DIR = WORK_DIR(1:WD_LEN)//'/'
                WD_LEN = WD_LEN + 1
           END IF
           INQUIRE ( FILE = WORK_DIR, EXIST = LEX )
           IF ( .NOT. LEX ) THEN
                CALL ERR_LOG ( 1001, -1, 'REPA', 'wrong value of '// &
     &              'environment variable WORK_DIR or '// &
     &               WORK_DIR(1:WD_LEN)//' -- directory does not exist' )
                STOP 'REPA: Wrong environment variable'
           END IF
!
! -------- copy synonymous names
!
           PRE_SCR_DIR = WORK_DIR
           PRE_SD_LEN  = WD_LEN
           PRE_LETRS   = USER_ID
         ELSE
           FL_STANDALONE = .FALSE.
           CALL PRE_PROG()
!
! -------- Copy synonymous names
!
           WORK_DIR = PRE_SCR_DIR
           WD_LEN   = PRE_SD_LEN
           USER_ID  = PRE_LETRS
      END IF
!
! --- Initialize variables
!
      PI_VAL = DACOS(-1D0)
      TIME_CHR = '    :  :     :  '
      ZERO_LINE(1) = 0.0D0
      ZERO_LINE(2) = 0.0D0
      IBOBSM = BOBS_MAX                          ! copy parameter for delivery to user functions
      MCLRV = M_CLR                              ! copy parameter for delivery to user function
      PPPL_MAX1 = PPPL_MAX                       ! copy of parameter in repa.i
      REPA_NEXT_VAR = REPA__NEXT
      REPA_PREV_VAR = REPA__PREV
      SUPKEY_S = '0'                             ! suppression flag for user function REPGRSU
      SUPKEY_R = '1'                             ! recover flag for user function REPGRSU
      CONCON = 'C'                               ! flag for setting of connecting lines
      CONDEL = 'D'                               ! flag for deleting of connecting lines
      CONINP = 'I'                               ! flag for user input for connecting lines
      CONINI = 'M'                               ! flag for minimum action of REPCONN for met. data
      ASHIFT = .FALSE.                           ! flag for ambiguity resolving
!
! --- Build the name of the configuration file for repa
!
      CALL CLRCH ( FILE_NAME_1 )
      FILE_NAME_1 = WORK_DIR( 1:WD_LEN )//'REPA'//USER_ID
!
! --- Check whether this file exists
!
      INQUIRE ( FILE=FILE_NAME_1, EXIST=LEX )
      IF ( .NOT. LEX ) THEN
           IUER = -1
           CALL ERR_LOG ( 7801, IUER, 'REPA', 'Configuration file for '// &
     &         'REPA: '//FILE_NAME_1(1:I_LEN(FILE_NAME_1))//' was not '// &
     &         'found. If you do not know where it is gone, you may need '// &
     &         'to create it by running solve_reset' )
           CALL EXIT ( 1 )
      END IF
!
! --- read parameter file REPAxx
!
      CALL CHASHL ( FILE_NAME_1 )
      IUER = -1
      CALL REPPARM ( FILE_NAME_1, MCLRV, COL_KEY,  COL_ATR, COL_NAM, &
     &               STY_KEY, PNT_STY, BAD_KEY, SHOW_BAD, NUM_KEY, &
     &               PPPL_M, PPPL_MAX1, PAG_KEY, PAGE_UPDN, REPAB__LABEL, IUER )
      IF ( IUER .NE. 0 ) THEN
           CALL ERR_LOG ( 7802, IUER, 'REPA', 'Error in processing REPA '// &
     &         'configuration file '//FILE_NAME_1 )
           CALL EXIT ( 1 )
      END IF
!
      DO J1=1,M_CLR
         CALL CHASHL ( COL_NAM(J1) )             ! colour names
      END DO
!
! --- Read workfile NAMFxx
!
      CALL OPENNAMFIL()
      CALL REPNARD ( REC_NUM, DBFB, DBFBV, DBFBB, RNB )
!
! --- read file COMMxx
!
      CALL USE_COMMON ( 'ORC' )
      CALL SOCOM_EXT ()
      CALL DATYP_SHOW ( IDATYP, OUT1 )
      WRITE ( 6, '( A, I2, A )' ) ' REPA: IDATYP=',IDATYP,' ('//OUT1(1:I_LEN(OUT1))//')'
!%%%%%%%%%%%%%%%%%
      if ( 2 < 1 ) then                                                !%%%%%%% for test: jump over Leonid P. block
      IF ( IDATYP .EQ. GRPONL__DTP .OR.  IDATYP .EQ. GRPRAT__DTP ) THEN
           CONTINUE
      ELSE
         CALL DATYP_SHOW ( GRPONL__DTP, OUT2 )
         CALL DATYP_SHOW ( GRPRAT__DTP, OUT3 )
         CALL ERR_LOG ( 4001, -1, 'REPA', 'You are trying to use REPA '// &
     &        'with solution type '//OUT1(1:I_LEN(OUT1))//' Unfortunatly, '// &
     &        'the current version of REPA supports only solution types "'// &
     &         OUT2(1:I_LEN(OUT2))//'" and "'//OUT3(1:I_LEN(OUT3))//'"' )
         CALL HIT_CONT ( %VAL(0), %VAL(0) )
         IF ( .NOT. FL_STANDALONE ) THEN
            CALL END_PROG ()
         ELSE
            CALL EXIT ( 1 )
         END IF
      END IF
      end if
!%%%%%%%%%%%%%%%%%
      EXP_INDEX = 1
      WRITE (6,*) 'REPA: EXP_CODE = ', EXP_CODE                                 ! code of the experiment                            (s. socom.i)
      WRITE (6,*) 'REPA: EXP_DESC = ', EXP_DESC( 1:10 )                         ! experiment description                            (s. socom.i)
!
! --- read file PARFxx
!
      CALL USE_PARFIL ( 'ORC' )
!
! --- preparation for MULTI_DIAGI BAND page
!
  910 CONTINUE
      IPAGE = 1                                             ! current property page index
      MPL =   0                                             ! # plots
      NC  =   0                                             ! # columns
      NR  =   0                                             ! # lines
      MPB =   REC_NUM(1) + 1                                ! # of buttons of band page
      DO J1=1,MPB-1
         BUT(J1) = DBFB(J1)//DBFBV(J1)
         BUT_LET(J1)  = BUT_SHORT(J1,IPAGE)
      END DO
      BUT(MPB)  = 'Return to OPTIN (Exit)'
      BUT_LET(MPB)  = 'E'
      FUNC_NNN = FUNC_NUM                                   ! copy of parameter FUNC_NUM
!
      CALL CLRCH ( TITLE )
      TITLE = 'REPA BAND PAGE - CHOOSE BAND!'
!
      JUMP = .FALSE.                                        ! flag for changing the band
!
! --- initialize DIAGI defaults
!
      IER = -1
      CALL DIAGI_DEF ( IBST, ILST, IOST, IPST, IWST, IDEV, STR, STR, &
     &                 ICL1, ICL2, ICL3, IER )
      IF ( IER .NE. 0 ) STOP 'REPA: error in intialization'
!
! --- call MULTI_DIAGI
!
      DO J1=1,PPPL_MAX
         DIAGI_S(J1)%IDEV = IDEV
      END DO
      ICODE = 1 ! %%% no MULTI_DIAGI call !!!
      IF ( ICODE .EQ. MPB .OR. ICODE .EQ. 0 ) GOTO 1000                            ! exit
!
! --- fill REPA structure record for current experiment
!
      EXP_INDEX = 1                                                                ! experiment index (currently always =1)
      REPA_E( EXP_INDEX )%IVS_CODE = EXP_CODE                                      ! IVS code of current experiment [from COMMxx]
      REPA_E( EXP_INDEX )%BAND_NUM = REC_NUM( 1 )                                  ! number of loaded bands [from NAMFxx]
      DO J1=1,REC_NUM(1)
         REPA_E( EXP_INDEX )%BAND_NAME( J1 ) = DBFB( J1 )//DBFBV( J1 )             ! db names + versions [from NAMFxx]
         REPA_E( EXP_INDEX )%BAND_KIND( J1 ) = DBFBB( J1 )                         ! kind (X or S) of bands [from NAMFxx]
      END DO
!
      REPA_E( EXP_INDEX )%BAND_RECS( 1, 1 ) = 1                                    ! # of first record of band #1 in RESFxx [from NAMFxx]
      IF ( REC_NUM(1) .EQ. 1 ) THEN
         WRITE ( 6, * ) 'REPA: AVAILABLE DATABASE BAND'
        ELSE
         WRITE ( 6, * ) 'REPA: AVAILABLE DATABASE BANDS'
      END IF
!
      DO J1 = 1 , REC_NUM( 1 )
         IF ( J1 .GT. 1) REPA_E(EXP_INDEX)%BAND_RECS( J1, 1 ) = RNB( J1-1 ) + 1  ! # of first record of band #J1 in RESFxx [from NAMFxx]
         REPA_E( EXP_INDEX )%BAND_RECS( J1, 2 ) = RNB( J1 )                        ! # of last record of band #J1 in RESFxx [from NAMFxx]
         INT_TMP = RNB( J1 )
         IF ( J1 .GT. 1) INT_TMP = INT_TMP - RNB( J1-1 )
         STR_TMP = DBFBV( J1 )(2:4)
         DO J2=1,3
            IF ( STR_TMP(J2:J2) .EQ. ' ' ) STR_TMP(J2:J2) = '0'
         END DO
         WRITE  ( 6, * ) '      '//DBFB( J1 )//'_V'//STR_TMP//' (',INT_TMP,' OBSERVATIONS )'  ! available database bands
      END DO
      WRITE ( 6, * ) 'REPA: NUMBER OF OBSERVATIONS (NUMOBS):', NUMOBS              ! total number of observations in the scratch files (s. socom.i)
!
! --- test prints
!
      BAND_INDEX = ICODE                                             ! current band index
!
! --- read index of cable calibrations
!
  915 CONTINUE
      IF ( REPA_E(EXP_INDEX)%BAND_KIND(BAND_INDEX) .EQ. 'X' ) THEN
         IUER = -1
         CALL REPCABL ( BAND_INDEX, IND_CAB, AVL_ARR, IUER )
         IF ( IUER .NE. 0 ) THEN
              CALL ERR_LOG ( 7803, -1, 'REPA', 'Error in REPCABL to find '// &
     &            'cable calibration in the database' )
              CALL EXIT ( 2 )
         END IF
      END IF
!
      BAND_REC_F = REPA_E(EXP_INDEX)%BAND_RECS( BAND_INDEX, 1 )    ! 1st RESFxx record (observation) # of current band BAND_INDEX
      BAND_REC_L = REPA_E(EXP_INDEX)%BAND_RECS( BAND_INDEX, 2 )    ! last RESFxx record (observation) # of current band BAND_INDEX
!
      IF ( JUMP ) THEN                                               ! band change
         JUMP = .FALSE.
         GOTO 950
      END IF
!
! --- find useful baselines pairs (initializing of field IBAS)
!
      DO J1=1,BASL_MAX
         IF ( J1*PPPL_M .GT. BASL_MAX ) THEN
            IPBL_MAX = J1                                            ! max. # of baseline pages
            GOTO 920
         END IF
      END DO
  920 CONTINUE
      WRITE (6,*) 'REPA: NUMBER OF STATIONS (NUMSTA): ',NUMSTA       ! total number of stations SOLVE knows about
      WRITE (6,*) 'REPA: NUMBER OF SOURCES (NUMSTR): ', NUMSTR       ! total number of sources SOLVE knows about
      BASL_NUM = 0
      DO J1=1,NUMSTA
         DO J2=1,NUMSTA
            IF ( J2 .GT. J1 ) THEN
               BASL_NUM = BASL_NUM + 1                               ! total # of useful baselines in IBAS
               IBAS( BASL_NUM, 1 ) = J1                              ! 1st station #
               IBAS( BASL_NUM, 2 ) = J2                              ! 2nd station #
               IBAS( BASL_NUM, 4 ) = 0                               ! flag for reverse station order
            END IF
         END DO
      END DO
!
      WRITE ( 6, * ) 'REPA: NUMBER OF BASELINES (BASL_NUM): ',BASL_NUM
      WRITE ( BASL_NUM_CH, '( I3 )' ) BASL_NUM
      CALL CHASHL ( BASL_NUM_CH )
!
      J2 = 0
      BPAG_NUM = 0
  930 CONTINUE
      J2 = J2 + PPPL_M
      BPAG_NUM = BPAG_NUM + 1                 ! # of baseline pages (max. index of BASL_PAG)
      J3 = 0
      DO J1=J2-PPPL_M+1,J2
            IF ( J1 .GT. BASL_NUM ) GOTO 935
            J3 = J3 + 1
            IBAS(J1,3) = BPAG_NUM             ! bl. page # of current baseline
      END DO
      BASL_PAG(BPAG_NUM) = J3                 ! # of bls. in bl page BPAG_NUM
      IF ( J2 .EQ. BASL_NUM ) THEN
         GOTO 935
      ELSE
         GOTO 930
      END IF
  935 CONTINUE
      BASL_PAG(BPAG_NUM) = J3
!
! --- find station numbers and baseline numbers (array BASL_INDEX)
!
      CALL CLRCH ( FILE_NAME_1 )
      FILE_NAME_1 = WORK_DIR(1:WD_LEN)//'RESF'//USER_ID
      CALL CHASHL ( FILE_NAME_1 )
!
      DO 940 J1=BAND_REC_F,BAND_REC_L                                               ! loop over records (observations) of RESFxx
         CALL REPRERD ( FILE_NAME_1, J1 )                                           ! read residual file RESFxx
         BASL_INDEX(J1,1) = IRSITE(1)                                               ! station #1
         BASL_INDEX(J1,2) = IRSITE(2)                                               ! station #2
         DO J2=1,BASL_NUM
            IF ( IBAS(J2,1) .EQ. IRSITE(1) .AND. IBAS(J2,2) .EQ. IRSITE(2) ) THEN
               BASL_INDEX(J1,3) = J2                                                ! baseline #
               GOTO 940
            END IF
         END DO
  940 CONTINUE
!
! --- preparation for MULTI_DIAGI property page 1
!
      IPAGE = 1                                                               ! current property page index
  945 CONTINUE                                                                ! >---> jump from 'next page' button
      CALL CLRCH ( TITLE )
      TITLE = 'REPA PROPERTY PAGE( / ) - '//REPA_E(EXP_INDEX)%BAND_NAME(BAND_INDEX)   ! title for property page
      WRITE ( TITLE(22:22), '(I1)' ) PAGES_M
      MPL =   0                                                               ! # plots
      WRITE ( TITLE(20:20), '(I1)' ) IPAGE
      NC  =   0                                                               ! # columns
      NR  =   0                                                               ! # lines
      MPB = PAG_NUM(IPAGE)                                                    ! # of buttons of current property page
!
! --- multi_diagi button names - property page
!
      DO J1=1,MPB
         BUT_LET(J1)  = BUT_SHORT(J1,IPAGE)                                   ! button short cuts - property page
         BUT(J1) = BUT_MAIN(J1,IPAGE)                                         ! buttons of current property page
      END DO
!
! --- call MULTI_DIAGI
!
      IUER  = -1                                                              ! error handler
      CALL MULTI_DIAGI ( TITLE, MPL, NC, NR, TITS, MPB, BUT, BUT_LET, &
     &                           '/tmp/mumu_', DIAGI_S, ICODE, IUER )
      IF ( IUER .NE. 0 ) STOP 'REPA_TEST: error in MULTI_DIAGI'
!
! --- set current property code PROP
!
      IF ( ICODE .NE. 0 ) THEN
         PROP = PROPER(ICODE,IPAGE)
      ELSE
         PROP = 'EXIT  '
      END IF
!
! --- Analyze return code of MULTI_DIAGI (ICODE = last index of BUT_LET)
!
! --- Analyze property code PROP
!
      IF ( PROP .EQ. 'CHBAND' ) THEN                                    ! change band
         GOTO 910
      ELSE IF ( PROP .EQ. 'NEXTPG' ) THEN                               ! next property page
         IF ( IPAGE .EQ. PAGES_M ) THEN                                 ! set property page index
            IPAGE = 1
         ELSE
            IPAGE = IPAGE + 1
         END IF
         GOTO 945
      ELSE IF ( PROP .EQ. 'EXIT  ' ) THEN                               ! exit
         GOTO 1000
      END IF
!
! --- Read RESFxx and OBSFxx
! --- initialize some values
!
      IBPAG = 1                                                         ! index of current bl.page in BASL_PAG
      WRITE ( BPAG_NUM_CHR, '(I2)' ) BPAG_NUM
      CALL CHASHL ( BPAG_NUM_CHR )
  950 CONTINUE
      WRITE ( IBPAG_CHR, '(I2)' ) IBPAG
      CALL CHASHL ( IBPAG_CHR )
      DO J1=1,BASL_MAX
         NUM_ALL(J1) = 0                                                ! total #s of observations of baselines
         NUM_G(J1) = 0                                                  ! #s of "good" observations in baselines (G, good)
         NUM_M(J1) = 0                                                  ! #s of "manual downweighted" observations in baselines (M, recoverable)
         NUM_B(J1) = 0                                                  ! #s of "bad" observations in baselines (B, bad)
      END DO
      DO J1=1,PPPL_M
         SCAL_MIN(J1) = .0                                              ! min. scale value
         SCAL_MAX(J1) = .0                                              ! max. scale value
         FL_SCALE(J1) = .TRUE.                                          ! scale flag
         PS_NS(J1) = 'ps'                                               ! unit (ps/ns)
      END DO
      IWAY_DOWN = -1                                                    ! # of ambiguity steps (down)
      IWAY_UP = 1                                                       ! # of ambiguity steps (up)
!
      CALL CLRCH ( FILE_NAME_1 )
      CALL CLRCH ( FILE_NAME_2 )
      FILE_NAME_1 = WORK_DIR( 1:WD_LEN )//'RESF'//USER_ID               ! residual filename
      FILE_NAME_2 = WORK_DIR( 1:WD_LEN )//'OBSF'//USER_ID               ! observation filename
      CALL CHASHL ( FILE_NAME_1 )
      CALL CHASHL ( FILE_NAME_2 )
!
! --- start and stop index for baseline loop for current bl.page IBPAG in IBAS(.,.)
!
      IF ( ASHIFT ) THEN                                     ! ambiguity resolving
         BASL_F = 1                                          ! start index for baseline loop in IBAS
         BASL_L = BASL_NUM                                   ! stop index for baseline loop in IBAS
      ELSE
         BASL_L = 0
         DO J1=1,IBPAG
            BASL_L = BASL_L + BASL_PAG(J1)                   ! stop index for baseline loop in IBAS
         END DO
         BASL_F = BASL_L - BASL_PAG(IBPAG) +1                ! start index for baseline loop in IBAS
      END IF
!
!CC   write (6,*) 'REPA: BASL_F BASL_L=',BASL_F,' ',BASL_L
!
! --- Terminal display
!
      IF ( RESAMB .EQ. 'Y' ) THEN
           WRITE ( 6, '(/A)' ) ' AMBIGUITY RESET FOR ALL BASELINES OF '// &
     &                           REPA_E( EXP_INDEX )%BAND_KIND(BAND_INDEX)// &
     &                           '-BAND'
      END IF
      WRITE ( 6, * ) 'REPA is loading residuals... '
      DO J1=BAND_REC_F,BAND_REC_L                            ! start loop over RESFxx & OBSFxx records (observation index J1)
         IF ( MOD(J1,100) .EQ. 0  .OR. J1 .EQ. BAND_REC_L ) THEN
              WRITE ( 6, '("    Observation ",I5," ( ",I5," )  ",A$)' ) J1, &
     &                (BAND_REC_L-BAND_REC_F+1), CHAR(13)
              CALL FLUSH ( 6 )
         END IF
         CALL REPRERD ( FILE_NAME_1, J1 )                                      ! read residual file RESFxx
         CALL REPOBRD ( FILE_NAME_2, J1, REC_OBSF, JOBSREC_WORDS )             ! read observation file OBSFxx
!
! ------ datatype group flag for datatypes
!
         IF ( IDATYP == 1 .OR. IDATYP == 4 .OR. IDATYP == 8 .OR.    &
     &        IDATYP == 10 .OR. IDATYP == 11 .OR. IDATYP == 14 .OR. &
     &        IDATYP == 17 ) THEN
            IDA_DTP = 'PX'    ! X phase data type
            PS_NS = 'tp'      ! flag (t)urns if (p)hases for phase data types ( to be used for amb. shifting)
         ELSE IF ( IDATYP == 9 .OR. IDATYP == 12 .OR. IDATYP == 13 .OR. IDATYP == 18 ) THEN
            IDA_DTP = 'PS'    ! S phase data type
            PS_NS = 'tp'      ! flag (t)urns if (p)hases for phase data types ( to be used for amb. shifting)
         ELSE IF ( IDATYP == 3 .OR. IDATYP == 7 .OR. IDATYP == 15 ) THEN
            IDA_DTP = 'GX'    ! X group delay data type
         ELSE IF ( IDATYP == 16 ) THEN
            IDA_DTP = 'GS'    ! S group delay data type
         END IF
         IF ( IDA_DTP == 'PX' ) THEN
            RDOC = ( RDOC / PHAMI8 ) / 1000                                   ! turns of phase (residual)
            RDERR = ( RDERR / PHAMI8 ) / 1000                                 ! turns of phase (error)
         ELSE IF ( IDA_DTP == 'PS' ) THEN
            RDOC = ( RDOC / PHAMI8_S ) / 1000                                 ! turns of phase (residual)
            RDERR = ( RDERR / PHAMI8_S ) / 1000                               ! turns of phase (error)
         END IF
!
! ------ case: ambiguity reset for all baselines (only for group delay datatypes)
!
         IF ( RESAMB .EQ. 'Y' ) THEN
            RDOC = RDOC - (NAMB * 1.0D9 * RFAMB)
            DOBS = DOBS - (NAMB * 1.0D6 * RFAMB)
            NAMB = 0
            NUMAMB = 0
            CALL REPREWT ( FILE_NAME_1, J1 )
            CALL REPOBWT ( FILE_NAME_2, J1, REC_OBSF, JOBSREC_WORDS )
            IF ( J1 .EQ. BAND_REC_L ) WRITE ( 6, * ) 'AMBIGUITY RESET FINISHED (',J1-BAND_REC_F+1,' OBSERVATIONS)'
         END IF
!
! ------ find baseline
!
         DO J2=1,BASL_L-BASL_F+1 ! start loop over baseline indices (J2) of current baseline page
!
! --------- current baseline (station combination) available in residual file:
!
            IF ( IBAS(J2+BASL_F-1,1) .EQ. IRSITE(1) .AND. &
     &           IBAS(J2+BASL_F-1,2) .EQ. IRSITE(2) .OR.  & ! default order
     &           IBAS(J2+BASL_F-1,1) .EQ. IRSITE(2) .AND. &
     &           IBAS(J2+BASL_F-1,2) .EQ. IRSITE(1)       ) THEN  ! reverse order
               IF ( IBAS(J2+BASL_F-1,1) .EQ. IRSITE(2) .AND. IBAS(J2+BASL_F-1,2) .EQ. IRSITE(1)) THEN
                    IBAS(J2+BASL_F-1,4) = 1                                                              ! flag for reverse station order
               END IF
!
! ------------ initialize obs. status flag
!
               IF ( SUPR_INQ ( SUPSTAT_RES, UACSUP_RES, USED__SPS ) ) THEN          ! USED__SPS=129?
                  IRUNX = 0                                                         ! good
                 ELSE IF ( SUPR_INQ ( SUPSTAT_RES, UACSUP_RES, RECO__SPS ) ) THEN   ! RECO__SPS=130?
                  IRUNX = 1                                                         ! recoverable
                 ELSE
                  IRUNX = 2                                                         ! bad
               END IF
!
! ------------ baseline found
!
               NUM_ALL(J2) = NUM_ALL(J2) + 1                       ! total observations # of baseline J2
!
! ------------ Convert JULIAN DATE to "calendar" date
!
               TIM= RFJD + RFRCT
               CALL REPEPOC ( TIM, IY, IM, ID, IHR, IMIN, SEC )
               WRITE ( TIME_CHR( 1:4 ), '( I4 )' ) IY              ! year
               WRITE ( TIME_CHR( 6:7 ), '( I2.2 )' ) IM            ! month
               WRITE ( TIME_CHR( 9:10 ), '( I2.2 )' ) ID           ! day
               WRITE ( TIME_CHR( 12:13 ), '( I2.2 )' ) IHR         ! hour
               WRITE ( TIME_CHR( 15:16 ), '( I2.2 )' ) IMIN        ! minute
!
! ------------ set time reduced to the first epoch
!
               TIM = 24.0D0 * TIM
!
               IF ( NUM_ALL(J2) .EQ. 1 ) THEN
                  TIM1(J2) = TIM                                   ! time of 1st epoch
                  TIME_CHR_1(1,J2) = TIME_CHR                      ! time of 1st epoch (character)
               END IF
               TIME_CHR_1(2,J2) = TIME_CHR                         ! at the end of J2-loop: time of last epoch (character)
!
! ------------ use TIME arrays for arguments in case properties 'GDELE1', 'GDELE1', 'SNELE1', 'SNELE2', 'GDELA1', 'GDELA2' !!
!
               IF ( PROP .EQ. 'SNELE1' .OR. PROP .EQ. 'GDELE1' ) THEN
                  TIME(NUM_ALL(J2),J2) = RELEV(1)
               ELSE IF ( PROP .EQ. 'SNELE2' .OR. PROP .EQ. 'GDELE2' ) THEN
                  TIME(NUM_ALL(J2),J2) = RELEV(2)
               ELSE IF ( PROP .EQ. 'GDELA1' ) THEN
                  TIME(NUM_ALL(J2),J2) = (AZ(1) * 180.0) / PI_VAL
               ELSE IF ( PROP .EQ. 'GDELA2' ) THEN
                  TIME(NUM_ALL(J2),J2) = (AZ(2) * 180.0) / PI_VAL
               ELSE
                  TIME(NUM_ALL(J2),J2) = TIM - TIM1(J2)            ! time (all)
               END IF
!
! ----------- take observations to pieces (good, recoverable, bad)
!
              REC_OBS(NUM_ALL(J2),J2) = J1                                   ! record #s of baseline observations
              IF ( PROP .NE. 'TEMPER' .AND. &
     &             PROP .NE. 'PRESSU' .AND. &
     &             PROP .NE. 'HUMIDI' ) THEN
                 IF ( IRUNX .EQ. 0 ) THEN
                     NUM_G(J2) = NUM_G(J2) + 1                               ! # of "good" observations in current baseline
!
                     IF ( PROP .EQ. 'SNELE1' .OR. PROP .EQ. 'GDELE1' ) THEN      ! use TIME arrays for properties 'GDELE1', 'GDELE1',
                        TIME_G(NUM_G(J2),J2) = RELEV(1)                          !
                     ELSE IF ( PROP .EQ. 'GDELA1' ) THEN
                        TIME_G(NUM_G(J2),J2) = (AZ(1) * 180.0) / PI_VAL
                     ELSE IF ( PROP .EQ. 'SNELE2' .OR. PROP .EQ. 'GDELE2' ) THEN ! 'SNELE1', 'SNELE2'
                        TIME_G(NUM_G(J2),J2) = RELEV(2)                          !
                     ELSE IF ( PROP .EQ. 'GDELA2' ) THEN
                        TIME_G(NUM_G(J2),J2) = (AZ(2) * 180.0) / PI_VAL
                     ELSE
                        TIME_G(NUM_G(J2),J2) = TIM - TIM1(J2)                ! time information (good)
                     END IF
!
                     GMB(NUM_ALL(J2),J2) = 'G'                               ! flag for good(G), recoverable(M) or bad(B) observation
                     INFO_CHR_G(NUM_G(J2),J2) = ' '
                     WRITE( INFO_CHR_G(NUM_G(J2),J2)(1:5), '(I5)' ) J1       ! info array (record #)
                     WRITE( INFO_CHR_G(NUM_G(J2),J2)(7:9), '(I3)' ) IRUNW    ! info array (status)
                     INFO_CHR_G(NUM_G(J2),J2)(11:18) = ISTRN_CHR(IRSTAR)     ! info array (source)
                     INFO_CHR_G(NUM_G(J2),J2)(20:33) = TIME_CHR(3:16)        ! info array (time)
                     I_TMP = REPA_INT( SNR )
                     WRITE( INFO_CHR_G(NUM_G(J2),J2)(35:38), '(I4)' ) I_TMP  ! info array (signal to noise ratio)
                     WRITE( INFO_CHR_G(NUM_G(J2),J2)(40:40), '(I1)' ) LQUAL - 10 * INT( LQUAL/10 ) ! info array (quality code X-band)
                     INFO_CHR_G(NUM_G(J2),J2)(41:41) = '/'
                     IF ( REPA_E( EXP_INDEX )%BAND_KIND(BAND_INDEX) .EQ. 'X' ) THEN
                        WRITE( INFO_CHR_G(NUM_G(J2),J2)(42:42), '(I1)' ) LQUAL_S - 10 * INT( LQUAL_S/10 ) ! info array (quality code S-band)
                     ELSE
                        INFO_CHR_G(NUM_G(J2),J2)(42:42) = '-'
                     END IF
                     WRITE( INFO_CHR_G(NUM_G(J2),J2)(44:49), '(O6)' ) ICORR  ! info array (ion flag)
                     INFO_CHR_G(NUM_G(J2),J2)(60:64) = 'xxxxx'               ! info array (full sigma)
                     INFO_CHR_G(NUM_G(J2),J2)(66:70) = 'xxxxx'               ! info array (correlator sigma)
                     I_TMP = REPA_INT( (AZ(1) * 180.0) / PI_VAL )
                     WRITE( INFO_CHR_G(NUM_G(J2),J2)(72:74), '(I3)' ) I_TMP  ! info array (azimut 1)
                     I_TMP = REPA_INT( (AZ(2) * 180.0) / PI_VAL )
                     WRITE( INFO_CHR_G(NUM_G(J2),J2)(76:78), '(I3)' ) I_TMP  ! info array (azimut 2)
                     I_TMP = REPA_INT( RELEV(1) )
                     WRITE( INFO_CHR_G(NUM_G(J2),J2)(80:81), '(I2)' ) I_TMP  ! info array (elavation -source 1)
                     I_TMP = REPA_INT( RELEV(2) )
                     WRITE( INFO_CHR_G(NUM_G(J2),J2)(83:84), '(I2)' ) I_TMP  ! info array (elavation -source 2)
                     INFO_CHR_G(NUM_G(J2),J2)(86:87) = 'GP'
                     REC_OBS_G(NUM_G(J2),J2) = J1                            ! record # of "good" observation
                  ELSE IF (IRUNX .EQ. 1 ) THEN
                     NUM_M(J2) = NUM_M(J2) + 1                               ! # of "recoverable" observations in current baseline
                     IF ( PROP .EQ. 'SNELE1' .OR. PROP .EQ. 'GDELE1' ) THEN      ! use TIME arrays for properties 'GDELE1', 'GDELE1',
                        TIME_M(NUM_M(J2),J2) = RELEV(1)                          !
                     ELSE IF ( PROP .EQ. 'GDELA1' ) THEN
                        TIME_M(NUM_M(J2),J2) = (AZ(1) * 180.0) / PI_VAL
                     ELSE IF ( PROP .EQ. 'SNELE2' .OR. PROP .EQ. 'GDELE2' ) THEN ! 'SNELE1', 'SNELE2'
                        TIME_M(NUM_M(J2),J2) = RELEV(2)                          !
                     ELSE IF ( PROP .EQ. 'GDELA2' ) THEN
                        TIME_M(NUM_M(J2),J2) = (AZ(2) * 180.0) / PI_VAL
                     ELSE
                        TIME_M(NUM_M(J2),J2) = TIM - TIM1(J2)                ! time information (recoverable)
                     END IF
                     GMB(NUM_ALL(J2),J2) = 'M'                               ! flag for good(G), recoverable(M) or bad(B) observation
                     INFO_CHR_M(NUM_M(J2),J2) = ' '
                     WRITE( INFO_CHR_M(NUM_M(J2),J2)(1:5), '(I5)' ) J1       ! info array (record #)
                     WRITE( INFO_CHR_M(NUM_M(J2),J2)(7:9), '(I3)' ) IRUNW    ! info array (status)
                     INFO_CHR_M(NUM_M(J2),J2)(11:18) = ISTRN_CHR(IRSTAR)     ! info array (source)
                     INFO_CHR_M(NUM_M(J2),J2)(20:33) = TIME_CHR(3:16)        ! info array (time)
                     I_TMP = REPA_INT( SNR )
                     WRITE( INFO_CHR_M(NUM_M(J2),J2)(35:38), '(I4)' ) I_TMP  ! info array (signal to noise ratio)
                     WRITE( INFO_CHR_M(NUM_M(J2),J2)(40:40), '(I1)' ) LQUAL - 10 * INT( LQUAL/10 ) ! info array (quality code current band)
                     INFO_CHR_M(NUM_M(J2),J2)(41:41) = '/'
                     IF ( REPA_E( EXP_INDEX )%BAND_KIND(BAND_INDEX) .EQ. 'X' ) THEN
                        WRITE( INFO_CHR_M(NUM_M(J2),J2)(42:42), '(I1)' ) LQUAL_S - 10 * INT( LQUAL_S/10 ) ! info array (quality code S-band)
                     ELSE
                        INFO_CHR_M(NUM_M(J2),J2)(42:42) = '-'
                     END IF
                     WRITE( INFO_CHR_M(NUM_M(J2),J2)(44:49), '(O6)' ) ICORR  ! info array (ion flag)
                     INFO_CHR_M(NUM_M(J2),J2)(60:64) = 'xxxxx'               ! info array (full sigma)
                     INFO_CHR_M(NUM_M(J2),J2)(66:70) = 'xxxxx'               ! info array (correlator sigma)
                     I_TMP = REPA_INT( (AZ(1) * 180.0) / PI_VAL )
                     WRITE( INFO_CHR_M(NUM_M(J2),J2)(72:74), '(I3)' ) I_TMP  ! info array (azimut 1)
                     I_TMP = REPA_INT( (AZ(2) * 180.0) / PI_VAL )
                     WRITE( INFO_CHR_M(NUM_M(J2),J2)(76:78), '(I3)' ) I_TMP  ! info array (azimut 2)
                     I_TMP = REPA_INT( RELEV(1) )
                     WRITE( INFO_CHR_M(NUM_M(J2),J2)(80:81), '(I2)' ) I_TMP  ! info array (elavation -source 1)
                     I_TMP = REPA_INT( RELEV(2) )
                     WRITE( INFO_CHR_M(NUM_M(J2),J2)(83:84), '(I2)' ) I_TMP  ! info array (elavation -source 2)
                     INFO_CHR_M(NUM_M(J2),J2)(86:87) = 'MD'
                     REC_OBS_M(NUM_M(J2),J2) = J1                            ! record # of "recoverable" observation
                  ELSE IF ( IRUNX .NE. 0 .AND. IRUNX .NE. 1 ) THEN
                     NUM_B(J2) = NUM_B(J2) + 1                               ! # of "bad" observations in current baseline
                     IF ( PROP .EQ. 'SNELE1' .OR. PROP .EQ. 'GDELE1' ) THEN      ! use TIME arrays for properties 'GDELE1', 'GDELE1',
                        TIME_B( NUM_B(J2), J2 ) = RELEV(1)                       !
                     ELSE IF ( PROP .EQ. 'GDELA1' ) THEN
                        TIME_B( NUM_B(J2), J2 ) = (AZ(1) * 180.0) / PI_VAL
                     ELSE IF ( PROP .EQ. 'SNELE2' .OR. PROP .EQ. 'GDELE2' ) THEN ! 'SNELE1', 'SNELE2'
                        TIME_B(NUM_B(J2),J2) = RELEV(2)                          !
                     ELSE IF ( PROP .EQ. 'GDELA2' ) THEN
                        TIME_B(NUM_B(J2),J2) = (AZ(2) * 180.0) / PI_VAL
                     ELSE
                        TIME_B( NUM_B(J2), J2 ) = TIM - TIM1(J2)             ! time information (bad)
                     END IF
                     GMB(NUM_ALL(J2),J2) = 'B'                               ! flag for good(G), recoverable(M) or bad(B) observation
                     INFO_CHR_B(NUM_B(J2),J2) = ' '
                     WRITE( INFO_CHR_B(NUM_B(J2),J2)(1:5), '(I5)' ) J1       ! info array (record #)
                     WRITE( INFO_CHR_B(NUM_B(J2),J2)(7:9), '(I3)' ) IRUNW    ! info array (status)
                     INFO_CHR_B(NUM_B(J2),J2)(11:18) = ISTRN_CHR(IRSTAR)     ! info array (source)
                     INFO_CHR_B(NUM_B(J2),J2)(20:33) = TIME_CHR(3:16)        ! info array (time)
                     I_TMP = REPA_INT( SNR )
                     WRITE( INFO_CHR_B(NUM_B(J2),J2)(35:38), '(I4)' ) I_TMP  ! info array (signal to noise ratio)
                     WRITE( INFO_CHR_B(NUM_B(J2),J2)(40:40), '(I1)' ) LQUAL - 10 * INT( LQUAL/10 ) ! info array (quality code X-band)
                     INFO_CHR_B(NUM_B(J2),J2)(41:41) = '/'
                     IF ( REPA_E( EXP_INDEX )%BAND_KIND(BAND_INDEX) .EQ. 'X' ) THEN
                        WRITE( INFO_CHR_B(NUM_B(J2),J2)(42:42), '(I1)' ) LQUAL_S -  10 * INT( LQUAL_S/10 ) ! info array (quality code S-band)
                     ELSE
                        INFO_CHR_B(NUM_B(J2),J2)(42:42) = '-'
                     END IF
                     WRITE( INFO_CHR_B(NUM_B(J2),J2)(44:49), '(O6)' ) ICORR  ! info array (ion flag)
                     INFO_CHR_B(NUM_B(J2),J2)(60:64) = 'xxxxx'               ! info array (full sigma)
                     INFO_CHR_B(NUM_B(J2),J2)(66:70) = 'xxxxx'               ! info array (correlator sigma)
                     I_TMP = REPA_INT( (AZ(1) * 180.0) / PI_VAL )
                     WRITE( INFO_CHR_B(NUM_B(J2),J2)(72:74), '(I3)' ) I_TMP  ! info array (azimut 1)
                     I_TMP = REPA_INT( (AZ(2) * 180.0) / PI_VAL )
                     WRITE( INFO_CHR_B(NUM_B(J2),J2)(76:78), '(I3)' ) I_TMP  ! info array (azimut 2)
                     I_TMP = REPA_INT( RELEV(1) )
                     WRITE( INFO_CHR_B(NUM_B(J2),J2)(80:81), '(I2)' ) I_TMP  ! info array (elavation -source 1)
                     I_TMP = REPA_INT( RELEV(2) )
                     WRITE( INFO_CHR_B(NUM_B(J2),J2)(83:84), '(I2)' ) I_TMP  ! info array (elavation -source 2)
                     INFO_CHR_B(NUM_B(J2),J2)(86:87) = 'BP'
                     REC_OBS_B(NUM_B(J2),J2) = J1                            ! record # of "bad" observation
                  END IF
               END IF
!
! ------------ Collect diagi input values - all observations of current MultiDiaGi page
!
! ------------ ambiguities for properties 'DELCOR' and 'DELFUL' (hier evtl. Amb.-Felder fuellen, fuer jede Beob. eigener Wert)
!
               IF ( PROP .EQ. 'DELCOR' .OR. PROP .EQ. 'DELFUL' ) THEN
                  NUM_AMB(NUM_ALL(J2),J2) = 0                               ! initialize NUM_AMB field (factors for ambiguity shifting)
!
                  IF ( IDA_DTP == 'PX' .AND. PHAMI8 < 0.5  ) THEN           ! X-phase data type
                     AMB_SP(J2) = PHAMI8                                    ! ambiguity step size (microsec)
                  ELSE IF ( IDA_DTP == 'PS' .AND. PHAMI8_S < 0.5 ) THEN     ! S-phase data type
                     AMB_SP(J2) = PHAMI8_S                                  ! ambiguity step size (microsec)
                  ELSE IF ( IDA_DTP == 'GX' .AND. FAMB < 0.5  ) THEN        ! group delay data type GX
                     AMB_SP(J2) = 1D12 * FAMB                               ! ambiguity step size (ps)
                     AMB_SP_N(J2) = 1D9 * FAMB                              ! ambiguity step size (ns)
                  ELSE IF ( IDA_DTP == 'GS' .AND. FAMB_S < 0.5 ) THEN       ! group delay data type GS
                     AMB_SP(J2) = 1D12 * FAMB_S                             ! ambiguity step size (ps)
                     AMB_SP_N(J2) = 1D9 * FAMB_S                            ! ambiguity step size 
                  END IF
               END IF
!
! ------------ values for properties 'DELCOR', 'DELFUL', 'GDELE1', 'GDELE2', 'GDELA1', 'GDELA2'
!
               IF ( PROP .EQ. 'DELCOR' .OR. PROP .EQ. 'DELFUL' .OR. &
     &              PROP .EQ. 'GDELE1' .OR. PROP .EQ. 'GDELE2' .OR. &
     &              PROP .EQ. 'GDELA1' .OR. PROP .EQ. 'GDELA2' ) THEN
!
                  IF ( IDA_DTP == 'PX' .OR. IDA_DTP == 'PS' ) THEN
                     FVAL8_A(NUM_ALL(J2),J2) = RDOC                         ! value (phase delays, all)
                  ELSE
                     FVAL8_A(NUM_ALL(J2),J2) = RDOC * 1000                  ! value (group delays, all)
                  END IF
!
                  IF ( IRUNX .EQ. 0 ) THEN                                  ! good observation
!
                     IF ( IDA_DTP == 'PX' .OR. IDA_DTP == 'PS' ) THEN
                        FVAL8_G( NUM_G(J2), J2 ) = RDOC                     ! delay residual (phase delays, good)
                     ELSE
                        FVAL8_G( NUM_G(J2), J2 ) = RDOC * 1000              ! delay residual (group delays, good)
                     END IF
!
                     I_TMP = REPA_INT( FVAL8_G(NUM_G(J2),J2) )
                     WRITE( INFO_CHR_G(NUM_G(J2),J2)(51:58), '(I8)' ) I_TMP ! info array (value)
! ------------------ minima and maxima of "good" observations
                     IF ( FL_SCALE(J2) ) THEN
                        IF ( IDA_DTP == 'PX' .OR. IDA_DTP == 'PS' ) THEN
                           SCAL_MIN(J2) = ( RDOC - RDERR )                  ! initial value (phase delays)
                           SCAL_MAX(J2) = ( RDOC + RDERR )                  ! initial value (phase delays)
                        ELSE
                           SCAL_MIN(J2) = 1000 * ( RDOC - RDERR )           ! initial value (group delays)
                           SCAL_MAX(J2) = 1000 * ( RDOC + RDERR )           ! initial value (group delays)
                        END IF
                        FL_SCALE(J2) = .FALSE.                              ! set flag for "initialized"
                     ELSE
                        IF ( IDA_DTP == 'PX' .OR. IDA_DTP == 'PS' ) THEN
                           IF ( ( RDOC - RDERR ) .LT. SCAL_MIN(J2) ) SCAL_MIN(J2) = ( RDOC - RDERR )
                           IF ( ( RDOC + RDERR ) .GT. SCAL_MAX(J2) ) SCAL_MAX(J2) = ( RDOC + RDERR )
                        ELSE
                           IF ( 1000 * ( RDOC - RDERR ) .LT. SCAL_MIN(J2) ) SCAL_MIN(J2) = 1000 * ( RDOC - RDERR )
                           IF ( 1000 * ( RDOC + RDERR ) .GT. SCAL_MAX(J2) ) SCAL_MAX(J2) = 1000 * ( RDOC + RDERR )
                        END IF
                     END IF
                  ELSE IF (IRUNX .EQ. 1 ) THEN                              ! recoverable observation
!
                     IF ( IDA_DTP == 'PX' .OR. IDA_DTP == 'PS' ) THEN
                        FVAL8_M( NUM_M(J2), J2 ) = RDOC                     ! phase delay residual (recoverable)
                     ELSE
                        FVAL8_M( NUM_M(J2), J2 ) = RDOC * 1000              ! group delay residual (recoverable)
                     END IF
!
                     I_TMP = REPA_INT( FVAL8_M(NUM_M(J2),J2) )
                     WRITE( INFO_CHR_M(NUM_M(J2),J2)(51:58), '(I8)' ) I_TMP ! info array (value)
                  ELSE IF ( IRUNX .NE. 0 .AND. IRUNX .NE. 1 ) THEN          ! bad observation
!
                     IF ( IDA_DTP == 'PX' .OR. IDA_DTP == 'PS' ) THEN
                          FVAL8_B( NUM_B(J2), J2 ) = RDOC                     ! phase delay residual (bad)
                       ELSE
                          FVAL8_B( NUM_B(J2), J2 ) = RDOC * 1000              ! group delay residual (bad)
                     END IF
!
                     I_TMP = REPA_INT( FVAL8_B(NUM_B(J2),J2) )
                     WRITE( INFO_CHR_B(NUM_B(J2),J2)(51:58), '(I8)' ) I_TMP ! info array (value)
                  END IF
               END IF
!
! ------------ values for properties 'RATCOR' and 'RATFUL'
!
               IF ( PROP .EQ. 'RATCOR' .OR. PROP .EQ. 'RATFUL' ) THEN
!
! --------------- collect diagi input values - all observations of current MultiDiaGi page
!
                  FVAL8_A(NUM_ALL(J2),J2) = RROC * 1000                     ! value (all)
!C                write (6,*) 'REPA: FVAL8_A(',NUM_ALL(J2),',',J2,')=', FVAL8_A(NUM_ALL(J2),J2)
                  IF ( IRUNX .EQ. 0 ) THEN                                  ! good observation
                     FVAL8_G( NUM_G(J2), J2 ) = RROC * 1000                 ! rate residual (good)
                     I_TMP = REPA_INT( FVAL8_G(NUM_G(J2),J2) )
                     WRITE( INFO_CHR_G(NUM_G(J2),J2)(51:58), '(I8)' ) I_TMP ! info array (value)
!
! ------------------ minima and maxima of "good" observations
!
                     IF ( FL_SCALE(J2) ) THEN
                        SCAL_MIN(J2) = 1000 * ( RROC - RRERR )              ! initial value
                        SCAL_MAX(J2) = 1000 * ( RROC + RRERR )              ! initial value
                        FL_SCALE(J2) = .FALSE.                              ! set flag for "initialized"
                     ELSE
                        IF ( 1000 * ( RROC - RRERR ) .LT. SCAL_MIN(J2) ) SCAL_MIN(J2) = 1000 * ( RROC - RRERR )
                        IF ( 1000 * ( RROC + RRERR ) .GT. SCAL_MAX(J2) ) SCAL_MAX(J2) = 1000 * ( RROC + RRERR )
                     END IF
                  ELSE IF (IRUNX .EQ. 1 ) THEN                              ! recoverable observation
                     FVAL8_M( NUM_M(J2), J2 ) = RROC * 1000                 ! rate residual (recoverable)
                     I_TMP = REPA_INT( FVAL8_M(NUM_M(J2),J2) )
                     WRITE( INFO_CHR_M(NUM_M(J2),J2)(51:58), '(I8)' ) I_TMP ! info array (value)
                  ELSE IF ( IRUNX .NE. 0 .AND. IRUNX .NE. 1 ) THEN          ! bad observation
                     FVAL8_B( NUM_B(J2), J2 ) = RROC * 1000                 ! rate residual (bad)
                     I_TMP = REPA_INT( FVAL8_B(NUM_B(J2),J2) )
                     WRITE( INFO_CHR_B(NUM_B(J2),J2)(51:58), '(I8)' ) I_TMP ! info array (value)
                  END IF
               END IF
!
! ------------ values for properties 'SNELE1' and 'SNELE2'
!
               IF ( PROP .EQ. 'SNELE1' .OR. PROP .EQ. 'SNELE2' ) THEN
                  FVAL8_A(NUM_ALL(J2),J2) = SNR                             ! signal to noise ratio
                  IF ( IRUNX .EQ. 0 ) THEN                                  ! good observation
                     FVAL8_G( NUM_G(J2), J2 ) = SNR
                     I_TMP = REPA_INT( FVAL8_G(NUM_G(J2),J2) )
                     WRITE( INFO_CHR_G(NUM_G(J2),J2)(51:58), '(I8)' ) I_TMP ! info array (value)
                  ELSE IF (IRUNX .EQ. 1 ) THEN                              ! recoverable observation
                     FVAL8_M( NUM_M(J2), J2 ) = SNR
                     I_TMP = REPA_INT( FVAL8_M(NUM_M(J2),J2) )
                     WRITE( INFO_CHR_M(NUM_M(J2),J2)(51:58), '(I8)' ) I_TMP ! info array (value)
                  ELSE IF ( IRUNX .NE. 0 .AND. IRUNX .NE. 1 ) THEN          ! bad observation
                     FVAL8_B( NUM_B(J2), J2 ) = SNR
                     I_TMP = REPA_INT( FVAL8_B(NUM_B(J2),J2) )
                     WRITE( INFO_CHR_B(NUM_B(J2),J2)(51:58), '(I8)' ) I_TMP ! info array (value)
                  END IF
               END IF
!
! ------------ values for property 'PHADEL'
!
               IF ( PROP .EQ. 'PHADEL' ) THEN
                  FVAL8_A(NUM_ALL(J2),J2) = DPH                             ! phase delay
                  IF ( IRUNX .EQ. 0 ) THEN                                  ! good observation
                     FVAL8_G( NUM_G(J2), J2 ) = DPH
                     I_TMP = REPA_INT( FVAL8_G(NUM_G(J2),J2) )
                     WRITE( INFO_CHR_G(NUM_G(J2),J2)(51:58), '(I8)' ) I_TMP ! info array (value)
                  ELSE IF (IRUNX .EQ. 1 ) THEN                              ! recoverable observation
                     FVAL8_M( NUM_M(J2), J2 ) = DPH
                     I_TMP = REPA_INT( FVAL8_M(NUM_M(J2),J2) )
                     WRITE( INFO_CHR_M(NUM_M(J2),J2)(51:58), '(I8)' ) I_TMP ! info array (value)
                  ELSE IF ( IRUNX .NE. 0 .AND. IRUNX .NE. 1 ) THEN          ! bad observation
                     FVAL8_B( NUM_B(J2), J2 ) = DPH
                     I_TMP = REPA_INT( FVAL8_B(NUM_B(J2),J2) )
                     WRITE( INFO_CHR_B(NUM_B(J2),J2)(51:58), '(I8)' ) I_TMP ! info array (value)
                  END IF
               END IF
!
! ------------ values for property 'GRIONO'
!
               IF ( PROP .EQ. 'GRIONO' ) THEN
                 IF ( IRUNX .EQ. 0 ) THEN                                   ! good observation
                     FVAL8_G( NUM_G(J2), J2 ) = GION(1)*1.D3                 ! delay residual (good)
                  ELSE IF (IRUNX .EQ. 1 ) THEN                               ! recoverable observation
                     FVAL8_M( NUM_M(J2), J2 ) = GION(1)*1.D3
                  ELSE IF ( IRUNX .NE. 0 .AND. IRUNX .NE. 1 ) THEN           ! bad observation
                     FVAL8_B( NUM_B(J2), J2 ) = GION(1)*1.D3
                  END IF
               END IF
!
! ------------ values for property 'CABLED'
!
               IF ( PROP .EQ. 'CABLED' ) THEN
                  NUM_G(J2) = NUM_ALL(J2)
                  TIME_G(NUM_G(J2),J2) = TIM - TIM1(J2)
                  FVAL8_G(NUM_G(J2),J2)   = 0.0D0
                  FVAL8_M(NUM_G(J2),J2)   = 0.0D0
                  IF ( IND_CAB .GT. 0 .AND. AVL_ARR(ISITE(1)) ) THEN         ! 1st station
                     FVAL8_G(NUM_G(J2),J2)   = CALIBS(1,1,IND_CAB) * 1.0D9   ! nsec
                  END IF
                  IF ( IND_CAB .GT. 0 .AND. AVL_ARR(ISITE(2)) ) THEN         ! 2nd station
                     FVAL8_M(NUM_G(J2),J2)   = CALIBS(2,1,IND_CAB) * 1.0D9   ! nsec
                  END IF
               END IF
!
! ------------ values for property 'TEMPER'
!
               IF ( PROP .EQ. 'TEMPER' ) THEN
                  NUM_G(J2) = NUM_ALL(J2)
                  TIME_G(NUM_G(J2),J2) = TIM - TIM1(J2)
                  FVAL8_G(NUM_G(J2),J2)   = TEMPC(1)
                  FVAL8_M(NUM_G(J2),J2)   = TEMPC(2)
               END IF
!
! ------------ values for property 'PRESSU'
!
               IF ( PROP .EQ. 'PRESSU' ) THEN
                  NUM_G(J2) = NUM_ALL(J2)
                  TIME_G(NUM_G(J2),J2) = TIM - TIM1(J2)
                  FVAL8_G(NUM_G(J2),J2)   = ATMPR(1)
                  FVAL8_M(NUM_G(J2),J2)   = ATMPR(2)
               END IF
!
! ------------ values for property 'HUMIDI'
!
               IF ( PROP .EQ. 'HUMIDI' ) THEN
                  NUM_G(J2) = NUM_ALL(J2)
                  TIME_G(NUM_G(J2),J2) = TIM - TIM1(J2)
                  FVAL8_G(NUM_G(J2),J2)   = 1D2 * RELHU(1)
                  FVAL8_M(NUM_G(J2),J2)   = 1D2 * RELHU(2)
               END IF
!
! ------------ errors
!
               IF ( PROP .EQ. 'DELCOR' ) THEN                                ! gr. delays  vs. time (corr. sigma)
                  FERR8_A(NUM_ALL(J2),J2) = DERR * 1E12                      ! error (all)
                  IF ( IRUNX .EQ. 0 ) THEN                                   ! good observation
                     FERR8_G( NUM_G(J2), J2 ) = DERR * 1E12                  ! delay error (corsig) (good)
                     I_TMP = REPA_INT( RDERR * 1E3 )
                     WRITE ( INFO_CHR_G(NUM_G(J2),J2)(60:64), '(I5)' ) I_TMP ! info array (fulsig)
                     I_TMP = REPA_INT( DERR * 1E12 )
                     WRITE ( INFO_CHR_G(NUM_G(J2),J2)(66:70), '(I5)' ) I_TMP ! info array (corsig)
                  ELSE IF (IRUNX .EQ. 1 ) THEN                               ! recoverable observation
                     FERR8_M( NUM_M(J2), J2 ) = DERR * 1E12                  ! delay error (corsig) (recoverable)
                     I_TMP = REPA_INT( RDERR * 1E3 )
                     WRITE ( INFO_CHR_M(NUM_M(J2),J2)(60:64), '(I5)' ) I_TMP ! info array (fulsig)
                     I_TMP = REPA_INT( DERR * 1E12 )
                     WRITE ( INFO_CHR_M(NUM_M(J2),J2)(66:70), '(I5)' ) I_TMP ! info array (corsig)
                  ELSE IF ( IRUNX .NE. 0 .AND. IRUNX .NE. 1 ) THEN           ! bad observation
                     FERR8_B( NUM_B(J2), J2 ) = DERR * 1E12                  ! delay error (corsig) (bad)
                     I_TMP = REPA_INT( RDERR * 1E3 )
                     WRITE ( INFO_CHR_B(NUM_B(J2),J2)(60:64), '(I5)' ) I_TMP ! info array (fulsig)
                     I_TMP = REPA_INT( DERR * 1E12 )
                     WRITE ( INFO_CHR_B(NUM_B(J2),J2)(66:70), '(I5)' ) I_TMP ! info array (corsig)
                  END IF
               ELSE IF ( PROP .EQ. 'DELFUL'  .OR. &                          ! gr. delays vs. time (full sigma)
     &                   PROP .EQ. 'GDELE1' .OR. PROP .EQ. 'GDELE2' .OR. &   ! gr. delays vs. elevation (full sigma)
     &                   PROP .EQ. 'GDELA1' .OR. PROP .EQ. 'GDELA2' ) THEN   !
                  IF ( IDA_DTP == 'PX' .OR. IDA_DTP == 'PS' ) THEN
                     FERR8_A(NUM_ALL(J2),J2) = RDERR                         ! error (phase delays, all)
                  ELSE
                     FERR8_A(NUM_ALL(J2),J2) = RDERR * 1E3                   ! error (group delays, all)
                  END IF
                  IF ( IRUNX .EQ. 0 ) THEN                                   ! good observation
                     IF ( IDA_DTP == 'PX' .OR. IDA_DTP == 'PS' ) THEN
                        FERR8_G( NUM_G(J2), J2 ) = RDERR                     ! phase delay error (fulsig) (good)
                     ELSE
                        FERR8_G( NUM_G(J2), J2 ) = RDERR * 1000              ! group delay error (fulsig) (good)
                     END IF
                     I_TMP = REPA_INT( FERR8_G( NUM_G(J2), J2 ) )
                     WRITE ( INFO_CHR_G(NUM_G(J2),J2)(60:64), '(I5)' ) I_TMP ! info array (fulsig)
                     I_TMP = REPA_INT( DERR * 1E12 )
                     WRITE ( INFO_CHR_G(NUM_G(J2),J2)(66:70), '(I5)' ) I_TMP ! info array (corsig)
                  ELSE IF (IRUNX .EQ. 1 ) THEN                               ! recoverable observation
                     IF ( IDA_DTP == 'PX' .OR. IDA_DTP == 'PS' ) THEN
                        FERR8_M( NUM_M(J2), J2 ) = RDERR                     ! phase delay error (fulsig) (recoverable)
                     ELSE
                        FERR8_M( NUM_M(J2), J2 ) = RDERR * 1000              ! group delay error (fulsig) (recoverable)
                     END IF
                     I_TMP = REPA_INT( FERR8_M( NUM_M(J2), J2 ) )
                     WRITE ( INFO_CHR_M(NUM_M(J2),J2)(60:64), '(I5)' ) I_TMP ! info array (fulsig)
                     I_TMP = REPA_INT( DERR * 1E12 )
                     WRITE ( INFO_CHR_M(NUM_M(J2),J2)(66:70), '(I5)' ) I_TMP ! info array (corsig)
                  ELSE IF ( IRUNX .NE. 0 .AND. IRUNX .NE. 1 ) THEN           ! bad observation
                     IF ( IDA_DTP == 'PX' .OR. IDA_DTP == 'PS' ) THEN
                        FERR8_B( NUM_B(J2), J2 ) = RDERR                     ! phase delay error (fulsig) (bad)
                     ELSE
                        FERR8_B( NUM_B(J2), J2 ) = RDERR * 1000              ! group delay error (fulsig) (bad)
                     END IF
                     I_TMP = REPA_INT( FERR8_B( NUM_B(J2), J2 ) )
                     WRITE ( INFO_CHR_B(NUM_B(J2),J2)(60:64), '(I5)' ) I_TMP ! info array (fulsig)
                     I_TMP = REPA_INT( DERR * 1E12 )
                     WRITE ( INFO_CHR_B(NUM_B(J2),J2)(66:70), '(I5)' ) I_TMP ! info array (corsig)
                  END IF
               ELSE IF ( PROP .EQ. 'RATCOR' ) THEN
                  FERR8_A(NUM_ALL(J2),J2) = RERR * 1E15                      ! error (all)
                  IF ( IRUNX .EQ. 0 ) THEN                                   ! good observation
                     FERR8_G( NUM_G(J2), J2 ) = RERR * 1E15                  ! rate error (corsig) (good)
                     I_TMP = REPA_INT( RRERR * 1E3 )
                     WRITE ( INFO_CHR_G(NUM_G(J2),J2)(60:64), '(I5)' ) I_TMP ! info array (fulsig)
                     I_TMP = REPA_INT( RERR * 1E15 )
                     WRITE ( INFO_CHR_G(NUM_G(J2),J2)(66:70), '(I5)' ) I_TMP ! info array (corsig)
                  ELSE IF (IRUNX .EQ. 1 ) THEN                               ! recoverable observation
                     FERR8_M( NUM_M(J2), J2 ) = RERR * 1E15                  ! rate error (corsig) (recoverable)
                     I_TMP = REPA_INT( RRERR * 1E3 )
                     WRITE ( INFO_CHR_M(NUM_M(J2),J2)(60:64), '(I5)' ) I_TMP ! info array (fulsig)
                     I_TMP = REPA_INT( RERR * 1E15 )
                     WRITE ( INFO_CHR_M(NUM_M(J2),J2)(66:70), '(I5)' ) I_TMP ! info array (corsig)
                  ELSE IF ( IRUNX .NE. 0 .AND. IRUNX .NE. 1 ) THEN           ! bad observation
                     FERR8_B( NUM_B(J2), J2 ) = RERR * 1E15                  ! rate error (corsig) (bad)
                     I_TMP = REPA_INT( RRERR * 1E3 )
                     WRITE ( INFO_CHR_B(NUM_B(J2),J2)(60:64), '(I5)' ) I_TMP ! info array (fulsig)
                     I_TMP = REPA_INT( RERR * 1E15 )
                     WRITE ( INFO_CHR_B(NUM_B(J2),J2)(66:70), '(I5)' ) I_TMP ! info array (corsig)
                  END IF
               ELSE IF ( PROP .EQ. 'RATFUL' ) THEN
                  FERR8_A(NUM_ALL(J2),J2) = RRERR * 1000                     ! error (all)
                  IF ( IRUNX .EQ. 0 ) THEN                                   ! good observation
                     FERR8_G( NUM_G(J2), J2 ) = RRERR * 1000                 ! rate error (fulsig) (good)
                     I_TMP = REPA_INT( RRERR * 1E3 )
                     WRITE ( INFO_CHR_G(NUM_G(J2),J2)(60:64), '(I5)' ) I_TMP ! info array (fulsig)
                     I_TMP = REPA_INT( RERR * 1E15 )
                     WRITE ( INFO_CHR_G(NUM_G(J2),J2)(66:70), '(I5)' ) I_TMP ! info array (corsig)
                  ELSE IF (IRUNX .EQ. 1 ) THEN                               ! recoverable observation
                     FERR8_M( NUM_M(J2), J2 ) = RRERR * 1000                 ! rate error (fulsig) (recoverable)
                     I_TMP = REPA_INT( RRERR * 1E3 )
                     WRITE ( INFO_CHR_M(NUM_M(J2),J2)(60:64), '(I5)' ) I_TMP ! info array (fulsig)
                     I_TMP = REPA_INT( RERR * 1E15 )
                     WRITE ( INFO_CHR_M(NUM_M(J2),J2)(66:70), '(I5)' ) I_TMP ! info array (corsig)
                  ELSE IF ( IRUNX .NE. 0 .AND. IRUNX .NE. 1 ) THEN           ! bad observation
                     FERR8_B( NUM_B(J2), J2 ) = RRERR * 1000                 ! rate error (fulsig) (bad)
                     I_TMP = REPA_INT( RRERR * 1E3 )
                     WRITE ( INFO_CHR_B(NUM_B(J2),J2)(60:64), '(I5)' ) I_TMP ! info array (fulsig)
                     I_TMP = REPA_INT( RERR * 1E15 )
                     WRITE ( INFO_CHR_B(NUM_B(J2),J2)(66:70), '(I5)' ) I_TMP ! info array (corsig)
                  END IF
               ELSE IF ( PROP .EQ. 'PHADEL' ) THEN
                  IF ( IRUNX .EQ. 0 ) THEN                                    ! good observation
                     FERR8_G( NUM_G(J2), J2 ) = DPHER * 1E15                  ! phase error (corsig) (good)
                     I_TMP = REPA_INT( DPHER * 1E15 )
                     WRITE ( INFO_CHR_G(NUM_G(J2),J2)(66:70), '(I5)' ) I_TMP  ! info array (corsig)
                     INFO_CHR_G(NUM_G(J2),J2)(60:64) = 'ps ->'
                  ELSE IF (IRUNX .EQ. 1 ) THEN                                ! recoverable observation
                     FERR8_M( NUM_M(J2), J2 ) = DPHER * 1E15                  ! phase error (corsig) (recoverable)
                     I_TMP = REPA_INT( DPHER * 1E15 )
                     WRITE ( INFO_CHR_M(NUM_M(J2),J2)(66:70), '(I5)' ) I_TMP  ! info array (corsig)
                     INFO_CHR_M(NUM_M(J2),J2)(60:64) = 'ps ->'
                  ELSE IF ( IRUNX .NE. 0 .AND. IRUNX .NE. 1 ) THEN            ! bad observation
                     FERR8_B( NUM_B(J2), J2 ) = DPHER * 1E15                  ! phase error (corsig) (bad)
                     I_TMP = REPA_INT( DPHER * 1E15 )
                     WRITE ( INFO_CHR_B(NUM_B(J2),J2)(66:70), '(I5)' ) I_TMP  ! info array (fulsig)
                     INFO_CHR_B(NUM_B(J2),J2)(60:64) = 'ps ->'
                  END IF
               ELSE IF ( PROP .EQ. 'GRIONO' ) THEN
                  IF ( IRUNX .EQ. 0 ) THEN                                    ! good observation
                     FERR8_G( NUM_G(J2), J2 ) = GIONSG(1)*1.D9                ! error (good)                  ! phase error (corsig) (good)
                     I_TMP = REPA_INT( GIONSG(1)*1.D12 )
                     WRITE ( INFO_CHR_G(NUM_G(J2),J2)(66:70), '(I5)' ) I_TMP  ! info array
                     INFO_CHR_G(NUM_G(J2),J2)(60:64) = '?? ->'
                  ELSE IF (IRUNX .EQ. 1 ) THEN                                ! recoverable observation
                     FERR8_M( NUM_M(J2), J2 ) = GIONSG(1)*1.D9                ! error (recoverable)
                     I_TMP = REPA_INT( GIONSG(1)*1.D12 )
                     WRITE ( INFO_CHR_M(NUM_M(J2),J2)(66:70), '(I5)' ) I_TMP  ! info array (corsig)
                     INFO_CHR_M(NUM_M(J2),J2)(60:64) = '?? ->'
                  ELSE IF ( IRUNX .NE. 0 .AND. IRUNX .NE. 1 ) THEN            ! bad observation
                     FERR8_B( NUM_B(J2), J2 ) = GIONSG(1)*1.D9                ! error (bad)
                     I_TMP = REPA_INT( GIONSG(1)*1.D12 )
                     WRITE ( INFO_CHR_B(NUM_B(J2),J2)(66:70), '(I5)' ) I_TMP  ! info array (fulsig)
                     INFO_CHR_B(NUM_B(J2),J2)(60:64) = '?? ->'
                  END IF
               END IF
            END IF
         END DO  ! end loop over baselines (J2)
      END DO     ! end loop over observations (J1)
      WRITE ( 6, * ) 'REPA has loaded ',J1-1, ' observations              '
!
! --- solve ambiguities for all baselines in one step (only for group delay datatypes)
!
      IF ( ASHIFT ) THEN
!
! ------ initialize ambiguity step arrays
!
         DO J1=1,BASL_NUM                                                                 ! loop over baselines (J1)
            DO J2=1,NUM_G(J1)                                                             ! "good"
               ISTEPS_G(J2,J1) = 0
            END DO
            DO J2=1,NUM_M(J1)                                                             ! "recoverable"
               ISTEPS_M(J2,J1) = 0
            END DO
            DO J2=1,NUM_B(J1)                                                             ! "bad"
               ISTEPS_B(J2,J1) = 0
            END DO
         END DO
!
!------- initialize flag vector for TARGET
!
         DO J1=1,NUMSTA
            TARGET_F(J1) = -1                                                             ! "independent" baselines apriori empty
         END DO
         IDP = .FALSE.
!
         WRITE(6,*) ' '                                                                   ! headlines to terminal
         WRITE(6,*) ' STATISTICS FOR AMBIGUITIES OF '// &
     &              REPA_E( EXP_INDEX )%BAND_KIND(BAND_INDEX)//'-BAND'
         WRITE(6,*) ' Ambiguity Step and Weighted Mean Residuals (WM) in nanoseconds '
         WRITE(6,*) ' ( R: reverse station order, IDP: independent baselines )'
         WRITE(6,*) '----------------------------------------------------------------'
         WRITE(6,*) '  #     BASELINE          AMB   CALCULATED THEORETICALLY'
         WRITE(6,*) '----------------------------------------------------------------'
         DO J1=1,BASL_NUM                                                                 ! loop over baselines (J1)
            DO J2=1,NUM_G(J1)
               ETEMP(J2) = FERR8_G(J2,J1)                                                 ! fill temp. array for WM calculation
            END DO
!
! --------- find pseudo mouse click FCLICK for the (first) "independent" NUMSTA-1 baselines
!
            IF ( J1 .LT. NUMSTA ) THEN
               FCLICK = 0.0                                                               ! initialize
               IF ( NUM_G(J1) .GT. 0 ) THEN                                               ! "good"
                  DO J2=1,NUM_G(J1)                                                       ! fill temp. area
                     FTEMP(J2) = FVAL8_G(J2,J1)
                  END DO
                  CALL REPSCAN ( IBOBSM, NUM_G(J1), FTEMP, AMB_SP(J1), 0, FCLICK )        ! get FCLICK (pseudo mouse click)
                  TARGET_F(IBAS(J1,2)) = 0                                                ! set flag vector (baseline not empty)
                  IDP = .TRUE.
               ELSE IF ( NUM_M(J1) .GT. 0 ) THEN                                          ! "recoverable"
                  DO J2=1,NUM_M(J1)                                                       ! fill temp. area
                     FTEMP(J2) = FVAL8_M(J2,J1)
                  END DO
                  CALL REPSCAN ( IBOBSM, NUM_M(J1), FTEMP, AMB_SP(J1), 0, FCLICK )        ! get FCLICK (pseudo mouse click)
                  TARGET_F(IBAS(J1,2)) = 0                                                ! set flag vector (baseline not empty)
                  IDP = .TRUE.
               END IF
               IF ( DABS(FCLICK) .GT. AMB_SP(J1)/2.0D0 ) THEN                             ! shift close to zero
                  CALL REPAMB8 ( FCLICK, 0.0D0, AMB_SP(J1), FCLICK_C )                    ! get correction factor FCLICK_C
                  FCLICK = FCLICK + (FCLICK_C * AMB_SP(J1))                               ! correct FCLICK
               END IF
            END IF
!
! --------- set FCLICK for current baseline (target of ambiguity shifting)
!
            IF ( J1 .LT. NUMSTA ) THEN                                                    ! first NUMSTA-1 "independent" baselines
               TARGET(IBAS(J1,2)) = FCLICK                                                ! target value
            ELSE                                                                          ! baselines NUMSTA...BASL_NUM
               FCLICK = TARGET(IBAS(J1,2)) - TARGET(IBAS(J1,1))                           ! theoretical value in triangle
!
! ------------ Set artificial TARGET values for empty "independent" baselines
!
               IF ( TARGET_F(IBAS(J1,2)) .EQ. -1 .AND. TARGET_F(IBAS(J1,1)) .EQ. -1 ) THEN  ! two empty "independent" baselines
                  TARGET(IBAS(J1,1)) = 0.0D0                                              ! set artificial value zero
                  TARGET_F(IBAS(J1,1)) = 0                                                ! set artificial flag vector value
                  IDP = .TRUE.
               END IF
!
! ------------ The same procedure as for the first "independent" baselines!!
!
               IF ( TARGET_F(IBAS(J1,2)) .EQ. -1 .OR. TARGET_F(IBAS(J1,1)) .EQ. -1 ) THEN   ! no TARGET values available
                  FCLICK = 0.0                                                            ! initialize
                  IF ( NUM_G(J1) .GT. 0 ) THEN                                            ! "good"
                     DO J2=1,NUM_G(J1)                                                    ! fill temp. area
                        FTEMP(J2) = FVAL8_G(J2,J1)
                     END DO
                     CALL REPSCAN ( IBOBSM, NUM_G(J1), FTEMP, AMB_SP(J1), 0, FCLICK )     ! get FCLICK (pseudo mouse click)
                  ELSE IF ( NUM_M(J1) .GT. 0 ) THEN                                       ! "recoverable"
                     DO J2=1,NUM_M(J1)                                                    ! fill temp. area
                        FTEMP(J2) = FVAL8_M(J2,J1)
                     END DO
                     CALL REPSCAN ( IBOBSM, NUM_M(J1), FTEMP, AMB_SP(J1), 0, FCLICK )     ! get FCLICK (pseudo mouse click)
                  END IF
                  IF ( DABS(FCLICK) .GT. AMB_SP(J1)/2.0D0 ) THEN                          ! shift close to zero
                     CALL REPAMB8 ( FCLICK, 0.0D0, AMB_SP(J1), FCLICK_C )                 ! get correction factor FCLICK_C
                     FCLICK = FCLICK + (FCLICK_C * AMB_SP(J1))                            ! correct FCLICK
                  END IF
                  IF ( TARGET_F(IBAS(J1,2)) .EQ. -1 ) THEN                                ! empty "independent" baseline
                     TARGET(IBAS(J1,2)) = TARGET(IBAS(J1,1)) + FCLICK                     ! set pseudo value for TARGET
                     TARGET_F(IBAS(J1,2)) = 0                                             ! set flag vector value(baseline not empty)
                     IDP = .TRUE.
                  ELSE IF ( TARGET_F(IBAS(J1,1)) .EQ. -1 ) THEN                           ! empty "independent" baseline
                     TARGET(IBAS(J1,1)) = TARGET(IBAS(J1,2)) - FCLICK                     ! set pseudo value for TARGET
                     TARGET_F(IBAS(J1,1)) = 0                                             ! set flag vector value(baseline not empty)
                     IDP = .TRUE.
                  END IF
               END IF
!
! ------------ correct the theoretical FCLICK value [scan in FCLICK-(20/100)*AMB,FCLICK+(20/100)*AMB]
! ------------ 5th parameter in REPSCAN is 20!! --> can be varied ( 10....40 ??)
!
               IF ( NUM_G(J1) .GT. 0 ) THEN
                  DO J2=1,NUM_G(J1)                                                       ! fill temp. array
                     FTEMP(J2) = FVAL8_G(J2,J1)
                  END DO
                  CALL REPSCAN ( IBOBSM, NUM_G(J1), FTEMP, AMB_SP(J1), 20, FCLICK )
               ELSE IF ( NUM_M(J1) .GT. 0 ) THEN
                  DO J2=1,NUM_M(J1)                                                       ! fill temp. array
                     FTEMP(J2) = FVAL8_M(J2,J1)
                  END DO
                  CALL REPSCAN ( IBOBSM, NUM_M(J1), FTEMP, AMB_SP(J1), 20, FCLICK )
               END IF
            END IF
!
! --------- find # of ambiguity steps, add ambiguities, write RESFxx & OBSFxx workfiles
!
            DO J2=1,NUM_G(J1)                                                             ! "good"
               CALL REPAMB8 ( FVAL8_G(J2,J1), FCLICK, AMB_SP(J1), ISTEPS )                ! find # of ambiguity steps
               ISTEPS_G(J2,J1) = ISTEPS_G(J2,J1) + ISTEPS                                 ! add ambiguity steps
               IF ( ISTEPS_G(J2,J1) .NE. 0 ) THEN                                         ! reset observation record
                  CALL REPRERD ( FILE_NAME_1, REC_OBS_G(J2,J1) )                          ! read residual file RESFxx
                  CALL REPOBRD ( FILE_NAME_2, REC_OBS_G(J2,J1), REC_OBSF, JOBSREC_WORDS ) ! read observation file OBSFxx
                  RDOC = RDOC + (ISTEPS_G(J2,J1) * 1D9 * RFAMB)                           ! add ambiguities
                  DOBS = DOBS + (ISTEPS_G(J2,J1) * 1D6 * RFAMB)                           ! add ambiguities
                  NAMB = NAMB + ISTEPS_G(J2,J1)                                           ! add ambiguities
                  NUMAMB = NUMAMB + ISTEPS_G(J2,J1)                                       ! add ambiguities
                  CALL REPREWT ( FILE_NAME_1, REC_OBS_G(J2,J1) )                          ! write residual file RESFxx
                  CALL REPOBWT ( FILE_NAME_2, REC_OBS_G(J2,J1), REC_OBSF, JOBSREC_WORDS ) ! write observation file OBSFxx
                  FTEMP(J2) = RDOC * 1000.0
                  ETEMP(J2) = FERR8_G(J2,J1)
               END IF
            END DO
!
            IF ( NUM_G(J1) .GT. 0 ) THEN
               CALL REPSIGM ( MEAN(J1), VAR(J1), FTEMP, ETEMP, NUM_G(J1) )                !calculate "Weighted Mean Residual"
            ELSE
               MEAN(J1) = 0.0D0
            END IF
            CALC_I = NINT(MEAN(J1)/1D3)                                                   ! integer of calculated values
            THEO_I = NINT((TARGET(IBAS(J1,2))-TARGET(IBAS(J1,1)))/1D3)
!
! --------- fill warning message
!
            WARN_CH = ' '
            IF ( IDP ) THEN
               WARN_CH(2:4) = 'IDP'
               IDP = .FALSE.
            END IF
!
            IF ( DABS(TARGET(IBAS(J1,2))-TARGET(IBAS(J1,1)) - MEAN(J1)) &
     &           .GT. AMB_SP(J1)/2 .AND. NUM_G(J1) .GT. 0 ) THEN                          ! great difference
                 WARN_CH = '     WARNING!!'
              ELSE IF ( NUM_G(J1) .EQ. 0 ) THEN
               WARN_CH = ' no good obs.!'                                                 ! no good observations in baseline
            END IF
!
            IF ( IBAS(J1,4) .NE. 0 ) THEN                                                 ! reverse station order
               REVORD = ' R'
              ELSE
                REVORD = '  '
            END IF
            WRITE ( 6, '(I4,A18,A2,I6,A4,I6,A6,I6,A14)' ) J1, &
     &              ' '//ISITN_CHR(IBAS(J1,1))(1:8)//' '// &
     &                   ISITN_CHR(IBAS(J1,2))(1:8), &
     &                   REVORD,IDNINT(AMB_SP(J1)/1000.0), '    ', &
     &                   CALC_I, '      ', THEO_I, WARN_CH
!
            DO J2=1,NUM_M(J1)                                                             ! "recoverable"
               CALL REPAMB8 ( FVAL8_M(J2,J1), FCLICK, AMB_SP(J1), ISTEPS )                ! find # of ambiguity steps
               ISTEPS_M(J2,J1) = ISTEPS_M(J2,J1) + ISTEPS                                 ! add ambiguity steps
               IF ( ISTEPS_M(J2,J1) .NE. 0 ) THEN                                         ! reset observation record
                  CALL REPRERD ( FILE_NAME_1, REC_OBS_M(J2,J1) )                          ! read residual file RESFxx
                  CALL REPOBRD ( FILE_NAME_2, REC_OBS_M(J2,J1), REC_OBSF, JOBSREC_WORDS ) ! read observation file OBSFxx
                  RDOC = RDOC + (ISTEPS_M(J2,J1) * 1D9 * RFAMB)                           ! add ambiguities
                  DOBS = DOBS + (ISTEPS_M(J2,J1) * 1D6 * RFAMB)                           ! add ambiguities
                  NAMB = NAMB + ISTEPS_M(J2,J1)                                           ! add ambiguities
                  NUMAMB = NUMAMB + ISTEPS_M(J2,J1)                                       ! add ambiguities
                  CALL REPREWT ( FILE_NAME_1, REC_OBS_M(J2,J1) )                          ! write residual file RESFxx
                  CALL REPOBWT ( FILE_NAME_2, REC_OBS_M(J2,J1), REC_OBSF, JOBSREC_WORDS ) ! write observation file OBSFxx
                  END IF
            END DO
!
            DO J2=1,NUM_B(J1)                                                             ! "bad"
               CALL REPAMB8 ( FVAL8_B(J2,J1), FCLICK, AMB_SP(J1), ISTEPS )                ! find # of ambiguity steps
               ISTEPS_B(J2,J1) = ISTEPS_B(J2,J1) + ISTEPS                                 ! add ambiguity steps
               IF ( ISTEPS_B(J2,J1) .NE. 0 ) THEN                                         ! reset observation record
                  CALL REPRERD ( FILE_NAME_1, REC_OBS_B(J2,J1) )                          ! read residual file RESFxx
                  CALL REPOBRD ( FILE_NAME_2, REC_OBS_B(J2,J1), REC_OBSF, JOBSREC_WORDS ) ! read observation file OBSFxx
                  RDOC = RDOC + (ISTEPS_B(J2,J1) * 1D9 * RFAMB)                           ! add ambiguities
                  DOBS = DOBS + (ISTEPS_B(J2,J1) * 1D6 * RFAMB)                           ! add ambiguities
                  NAMB = NAMB + ISTEPS_B(J2,J1)                                           ! add ambiguities
                  NUMAMB = NUMAMB + ISTEPS_B(J2,J1)                                       ! add ambiguities
                  CALL REPREWT ( FILE_NAME_1, REC_OBS_B(J2,J1) )                          ! write residual file RESFxx
                  CALL REPOBWT ( FILE_NAME_2, REC_OBS_B(J2,J1), REC_OBSF, JOBSREC_WORDS ) ! write observation file OBSFxx
                  END IF
            END DO
         END DO    ! J1
         WRITE(6,*) '----------------------------------------------------------'
         ASHIFT = .FALSE.                                                                 ! set ambiguity resolve flag back
         GOTO 950                                                                         ! jump to reading of RESFxx & OBSFxx
      END IF
!
! --- Fill DIAGI structure for baseline page
!
      MPL =   BASL_L - BASL_F + 1
      DO J1=1,30
         IF ( J1**2 .GE. BASL_PAG(IBPAG) ) THEN
            NC = J1                                                           ! # columns
            NR = J1                                                           ! # lines
            GOTO 960
         END IF
      END DO
!
  960 CONTINUE
      MPB =   8                                                               ! # of buttons
      IF ( BAND_INDEX .EQ. REPA_E( EXP_INDEX )%BAND_NUM ) THEN                ! find index of next band
         INEXT = 1
      ELSE
         INEXT = BAND_INDEX + 1
      END IF
      IF ( PROP .EQ. 'DELCOR' .OR. PROP .EQ. 'DELFUL' ) THEN                  ! buttons for 'DELCOR', 'DELFUL'
         BUT(1) = 'prev. bl. page'
         BUT(2) = 'next bl. page'
         BUT(3) = 'reset ambiguities'
         BUT(4) = 'solve ambiguities'
         BUT(5) = 'property pages'
         BUT(6) = 'reload '//REPA_E( EXP_INDEX )%BAND_KIND(BAND_INDEX)//'-BAND'
         BUT(7) = 'load '//REPA_E( EXP_INDEX )%BAND_KIND(INEXT)//'-BAND'
         BUT(8) = 'OPTIN (Exit)'
         BUT_LET(1) = 'P'
         BUT_LET(2) = 'N'
         BUT_LET(3) = 'R'
         BUT_LET(4) = 'A'
         BUT_LET(5) = '#'
         BUT_LET(6) = 'L'
         BUT_LET(7) = 'B'
         BUT_LET(8) = 'E'
      ELSE                                                                     ! buttons for other properties
         MPB =   6
         BUT(1) = 'prev. bl. page'
         BUT(2) = 'next bl. page'
         BUT(3) = 'property pages'
         BUT(4) = 'reload '//REPA_E( EXP_INDEX )%BAND_KIND(BAND_INDEX)//'-BAND'
         BUT(5) = 'load '//REPA_E( EXP_INDEX )%BAND_KIND(INEXT)//'-BAND'
         BUT(6) = 'OPTIN (Exit)'
         BUT_LET(1) = 'P'
         BUT_LET(2) = 'N'
         BUT_LET(3) = '#'
         BUT_LET(4) = 'L'
         BUT_LET(5) = 'B'
         BUT_LET(6) = 'E'
      END IF
      CALL CLRCH ( TITLE )
!
! --- Clear DIAGI_S object. We always must do it! -------------------
!
      DIAGI_LEN = LOC(DIAGI_S%STATUS) - LOC(DIAGI_S%IFIRST_FIELD) + 4          ! learn length
      CALL NOUT ( DIAGI_LEN, DIAGI_S )
!
! --- ask for current property and fill DIAGI structure
!
! --- convert ps values to ns values for big ps range (group delay data types)
!
      IF ( IDA_DTP /= 'PX' .AND. IDA_DTP /= 'PS' ) THEN                    ! no scaling for 'PX' and 'PS' (later to be programmed)
         IF ( PROP .EQ. 'DELCOR' .OR. PROP .EQ. 'DELFUL' .OR. &
     &        PROP .EQ. 'GDELE1' .OR. PROP .EQ. 'GDELE2' .OR. &
     &        PROP .EQ. 'GDELA1' .OR. PROP .EQ. 'GDELA2' ) THEN
            DO J1=1,BASL_L-BASL_F+1                                        ! start loop over baseline(plot) indices (J1)
               IF ( SCAL_MIN(J1) .LT. -1.0D4 .OR. SCAL_MAX(J1) .GT. 1.0D4 ) THEN
                  SCAL_MIN(J1) = SCAL_MIN(J1) / 1000
                  SCAL_MAX(J1) = SCAL_MAX(J1) / 1000
                  AMB_SP(J1) = AMB_SP_N(J1)                                ! AMB_SP in ns
                  PS_NS(J1) = 'ns'
                  DO J2 = 1, NUM_ALL(J1)
                     FVAL8_A(J2,J1) = FVAL8_A(J2,J1) / 1000
                     FERR8_A(J2,J1) = FERR8_A(J2,J1) / 1000
                  END DO
                  DO J2 = 1, NUM_G(J1)
                     FVAL8_G(J2,J1) = FVAL8_G(J2,J1) / 1000
                     FERR8_G(J2,J1) = FERR8_G(J2,J1) / 1000
                     I_TMP = REPA_INT( FVAL8_G(J2,J1) )
                     WRITE( INFO_CHR_G(J2,J1)(51:58), '(I8)' ) I_TMP
                  END DO
                  DO J2 = 1, NUM_M(J1)
                     FVAL8_M(J2,J1) = FVAL8_M(J2,J1) / 1000
                     FERR8_M(J2,J1) = FERR8_M(J2,J1) / 1000
                     I_TMP = REPA_INT( FVAL8_M(J2,J1) )
                     WRITE( INFO_CHR_M(J2,J1)(51:58), '(I8)' ) I_TMP
                  END DO
                  DO J2 = 1, NUM_B(J1)
                     FVAL8_B(J2,J1) = FVAL8_B(J2,J1) / 1000
                     FERR8_B(J2,J1) = FERR8_B(J2,J1) / 1000
                     I_TMP = REPA_INT( FVAL8_B(J2,J1) )
                     WRITE( INFO_CHR_B(J2,J1)(51:58), '(I8)' ) I_TMP
                  END DO
               ENDIF
            END DO
         END IF
      END IF
!
      IF ( PROP .EQ. 'DELCOR' .OR. PROP .EQ. 'DELFUL' .OR. &
     &     PROP .EQ. 'RATCOR' .OR. PROP .EQ. 'RATFUL' .OR. &
     &     PROP .EQ. 'GDELE1' .OR. PROP .EQ. 'GDELE2' .OR. &
     &     PROP .EQ. 'GDELA1' .OR. PROP .EQ. 'GDELA2' .OR. &
     &     PROP .EQ. 'GRIONO' ) THEN                                       ! note! more definitions for 'GRIONO below
!
         DO J1=1,BASL_L-BASL_F+1                                           ! start loop over baseline(plot) indices
!
! --------- initialize sigma lines (delays)
!
            DO J2=1,NUM_G(J1)                                              ! fill temporary arrays
               FTEMP(J2) = FVAL8_G(J2,J1)
               ETEMP(J2) = FERR8_G(J2,J1)
            END DO
            IF ( NUM_G(J1) .GT. 0 .AND. PROP .NE. 'GRIONO' ) THEN
               CALL REPSIGM ( MEAN(J1), VAR(J1), FTEMP, ETEMP, NUM_G(J1) )
            ELSE
               MEAN(J1) = 0.0D0
               VAR(J1)  = 0.0D0
            END IF
            SIGD_LINE1(1,J1) = 3 * VAR(J1)                                 ! sigma line 1
            SIGD_LINE1(2,J1) = SIGD_LINE1(1,J1)
            SIGD_LINE2(1,J1) = - SIGD_LINE1(1,J1)                          ! sigma line 2
            SIGD_LINE2(2,J1) = - SIGD_LINE1(2,J1)
!
            DIAGI_S(J1)%XMIN = -0.1D0                                      ! initialize plot coordinates
            IF ( NUM_ALL(J1) .GT. 0 ) THEN
               IF ( PROP .EQ. 'DELCOR' .OR. PROP .EQ. 'DELFUL' .OR. &
     &              PROP .EQ. 'RATCOR' .OR. PROP .EQ. 'RATFUL' .OR. PROP .EQ. 'GRIONO') THEN
                  DIAGI_S(J1)%XMAX = TIME(NUM_ALL(J1),J1) + 0.1D0
               ELSE IF ( PROP .EQ. 'GDELE1' .OR. PROP .EQ. 'GDELE2' ) THEN
                  DIAGI_S(J1)%XMAX = 90.1D0
               ELSE IF ( PROP .EQ. 'GDELA1' .OR. PROP .EQ. 'GDELA2' ) THEN
                  DIAGI_S(J1)%XMAX = 360.1D0
               END IF
            ELSE
               DIAGI_S(J1)%XMAX = 0.0
            END IF
            ZERO_TIME(1,J1) = -0.1D0                                       ! time values for zero- and sigma lines
            IF ( NUM_ALL(J1) .GT. 0 ) THEN
               ZERO_TIME(2,J1) = DIAGI_S(J1)%XMAX
            ELSE
               ZERO_TIME(2,J1) = 0.0D0
            END IF
            DIAGI_S(J1)%YMIN = SCAL_MIN(J1) - ( SCAL_MAX(J1) - SCAL_MIN(J1) ) / 20
            DIAGI_S(J1)%YMAX = SCAL_MAX(J1) + ( SCAL_MAX(J1) - SCAL_MIN(J1) ) / 20
 
            IF ( PROP .NE. 'GRIONO' .AND. ABS(DIAGI_S(J1)%YMAX - DIAGI_S(J1)%YMIN) .LT. 10E-32 &
     &          .AND. DIAGI_S(J1)%YMIN .LT. 0.0 .AND. DIAGI_S(J1)%YMAX .GT. 0.0 ) THEN
               DIAGI_S(J1)%YMIN = -0.1
               DIAGI_S(J1)%YMAX = 0.1
            END IF
 
            IF ( ABS(DIAGI_S(J1)%YMAX - DIAGI_S(J1)%YMIN) .LT. 10E-32 ) THEN
               DIAGI_S(J1)%YMIN = -0.1
               DIAGI_S(J1)%YMAX = 0.1
            END IF
            IF ( DIAGI_S(J1)%YMIN .LT. 0.0  .AND. DIAGI_S(J1)%YMAX .GT. 0.0) THEN      ! make the plot area symmetric
               DIAGI_S(J1)%YMIN = 0.0 - MAX( ABS( DIAGI_S(J1)%YMIN ), ABS(DIAGI_S(J1)%YMAX ) )
               DIAGI_S(J1)%YMAX = 0.0 - DIAGI_S(J1)%YMIN
            ENDIF
            IF ( NUM_ALL(J1) .EQ. 0 ) THEN
               DIAGI_S(J1)%XMIN = 0.0
               DIAGI_S(J1)%XMAX = 1.0
               DIAGI_S(J1)%YMIN = 0.0
               DIAGI_S(J1)%YMAX = 1.0
            END IF
!
            IF ( IBATCH .EQ. 0 ) THEN
               DIAGI_S(J1)%IDEV      = IDEV
            ELSE
               DIAGI_S(J1)%IDEV      = 7
            END IF
!
            N_COL = 3                                                      ! # of observation colours
!
            DIAGI_S(J1)%STATUS = DIA__DEF                                  ! DiaGi status
!
            DIAGI_S( J1 )%ICLR = 1                                         ! current colour ( main function)
            DIAGI_S( J1 )%NCLR = 7                                         ! number of the functions to be plotted
            DIAGI_S( J1 )%ITRM = 0                                         ! code action for termination
            DIAGI_S( J1 )%IBATCH = 0                                       ! code of batch mode. (0 for interactive)
!
            IF ( PROP .EQ. 'DELCOR' .OR. PROP .EQ. 'DELFUL' .OR. PROP .EQ. 'RATCOR' .OR. PROP .EQ. 'RATFUL' ) THEN
               DIAGI_S( J1 )%LER(1) = .TRUE.                               ! flag array for using function 1 errors
               DIAGI_S( J1 )%LER(2) = .TRUE.                               ! flag array for using function 2 errors
               DIAGI_S( J1 )%LER(3) = .TRUE.                               ! flag array for using function 3 errors
               DIAGI_S( J1 )%LER(7) = .TRUE.                               ! flag array for using function 7 errors
            ELSE IF ( PROP .EQ. 'GDELE1' .OR. PROP .EQ. 'GDELE2' .OR. PROP .EQ. 'GDELA1' .OR. PROP .EQ. 'GDELA2' ) THEN
               DIAGI_S( J1 )%LER(1) = .FALSE.                              ! flag array for using function 1 errors
               DIAGI_S( J1 )%LER(2) = .FALSE.                              ! flag array for using function 2 errors
               DIAGI_S( J1 )%LER(3) = .FALSE.                              ! flag array for using function 3 errors
               DIAGI_S( J1 )%LER(7) = .FALSE.                              ! flag array for using function 7 errors
            END IF
!
            DIAGI_S( J1 )%LER(4) = .FALSE.                                 ! flag array for using function 4 errors
            DIAGI_S( J1 )%LER(5) = .FALSE.                                 ! flag array for using function 5 errors
            DIAGI_S( J1 )%LER(6) = .FALSE.                                 ! flag array for using function 6 errors
!
            IF ( PROP .EQ. 'DELCOR' .OR. PROP .EQ. 'DELFUL' .OR. PROP .EQ. 'RATCOR' .OR. PROP .EQ. 'RATFUL' ) THEN
               DIAGI_S( J1 )%IBST(1) = 2                                   ! error bar style for using function 1
               DIAGI_S( J1 )%IBST(2) = 2                                   ! error bar style for using function 2
               DIAGI_S( J1 )%IBST(3) = 2                                   ! error bar style for using function 3
               DIAGI_S( J1 )%IBST(7) = 0                                   ! error bar style for using function 7
            ELSE IF (PROP .EQ. 'GDELE1' .OR. PROP .EQ. 'GDELE2' .OR. PROP .EQ. 'GDELA1' .OR. PROP .EQ. 'GDELA2' ) THEN
               DIAGI_S( J1 )%IBST(1) = 0                                   ! error bar style for using function 1
               DIAGI_S( J1 )%IBST(2) = 0                                   ! error bar style for using function 2
               DIAGI_S( J1 )%IBST(3) = 0                                   ! error bar style for using function 3
               DIAGI_S( J1 )%IBST(7) = 0                                   ! error bar style for using function 7
            END IF
!
            DIAGI_S( J1 )%IBST(4) = 0                                      ! error bar style for using function 4
            DIAGI_S( J1 )%IBST(5) = 0                                      ! error bar style for using function 5
            DIAGI_S( J1 )%IBST(6) = 0                                      ! error bar style for using function 6
!
            DIAGI_S(J1)%IOST(1) = 1                                        ! overplot style on
            DIAGI_S(J1)%IOST(2) = 1                                        ! overplot style on
            DIAGI_S(J1)%IOST(3) = 1                                        ! overplot style on
            DIAGI_S(J1)%IOST(4) = 0                                        ! overplot style off
            DIAGI_S(J1)%IOST(5) = 0                                        ! overplot style off
            DIAGI_S(J1)%IOST(6) = 0                                        ! overplot style off
            DIAGI_S(J1)%IOST(7) = 1                                        ! overplot style on
!
            DIAGI_S(J1)%IWST(1)= 1                                         ! line width
            DIAGI_S(J1)%IWST(2)= 1                                         ! line width
            DIAGI_S(J1)%IWST(3)= 1                                         ! line width
            DIAGI_S(J1)%IWST(4)= 1                                         ! line width
            DIAGI_S(J1)%IWST(5)= 1                                         ! line width
            DIAGI_S(J1)%IWST(6)= 1                                         ! line width
            DIAGI_S(J1)%IWST(7)= 1                                         ! line width
!
            DIAGI_S(J1)%ICOL(1) = COL_ATR(1)                               ! colour attribute for good obs.(G)
            DIAGI_S(J1)%ICOL(2) = COL_ATR(2)                               ! colour attribute for recoverable obs.(M)
            DIAGI_S(J1)%ICOL(3) = COL_ATR(3)                               ! colour attribute for bad obs.(B)
            DIAGI_S(J1)%ICOL(4) = COL_ATR(4)                               ! colour attribute for zero line
            DIAGI_S(J1)%ICOL(5) = COL_ATR(5)                               ! colour attribute for sigma line
            DIAGI_S(J1)%ICOL(6) = COL_ATR(6)                               ! colour attribute for sigma line
            DIAGI_S(J1)%ICOL(7) = COL_ATR(7)                               ! variable colour index for point connection
!
            DIAGI_S(J1)%ILST(1) = 1                                        ! line style for good obs.
            DIAGI_S(J1)%ILST(2) = 1                                        ! line style for recoverable obs.
            DIAGI_S(J1)%ILST(3) = 1                                        ! line style for bad obs.
            DIAGI_S(J1)%ILST(4) = 2                                        ! line style for zero line
            DIAGI_S(J1)%ILST(5) = 2                                        ! line style for sigma line
            DIAGI_S(J1)%ILST(6) = 2                                        ! line style for sigma line
            DIAGI_S(J1)%ILST(7) = 2                                        ! line style for special case
!
            DIAGI_S(J1)%IPST(1) = PNT_STY(1)                               ! point style for good obs.
            DIAGI_S(J1)%IPST(2) = PNT_STY(2)                               ! point style for recoverable obs
            DIAGI_S(J1)%IPST(3) = PNT_STY(3)                               ! point style for bad obs.
!
            DIAGI_S(J1)%IPST(4) = PNT_STY(4)                               ! point style for zero line
            DIAGI_S(J1)%IPST(5) = PNT_STY(5)                               ! point style for sigma line
            DIAGI_S(J1)%IPST(6) = PNT_STY(6)                               ! point style for sigma line
            DIAGI_S(J1)%IPST(7) = PNT_STY(7)                               ! point style for special case
!
            DIAGI_S(J1)%NPOI(1) =  NUM_G(J1)                               ! # of points for good obs.
            DIAGI_S(J1)%NPOI(2) =  NUM_M(J1)                               ! # of points for recoverable obs.
!
            IF ( SHOW_BAD ) THEN                                           ! # of points for bad obs.
               DIAGI_S(J1)%NPOI(3) =  NUM_B(J1)
            ELSE
               DIAGI_S(J1)%NPOI(3) =  0
            END IF
!
            DIAGI_S(J1)%NPOI(4) =  2                                       ! # of points zero_line
            DIAGI_S(J1)%NPOI(5) =  2                                       ! # of points sigma line
            DIAGI_S(J1)%NPOI(6) =  2                                       ! # of points sigma line
            DIAGI_S(J1)%NPOI(7) =  0                                       ! # of points special case
!
            DIAGI_S(J1)%ADR_X8(1) = LOC( TIME_G(1,J1))                    ! address of the 1st element of TIME_G(.,J1)
            DIAGI_S(J1)%ADR_X8(2) = LOC( TIME_M(1,J1))                    ! address of the 1st element of TIME_M(.,J1)
            DIAGI_S(J1)%ADR_X8(3) = LOC( TIME_B(1,J1))                    ! address of the 1st element of TIME_B(.,J1)
            DIAGI_S(J1)%ADR_X8(4) = LOC( ZERO_TIME(1,J1))                 ! address of the 1st element of ZERO_TIME(.,J1)
            DIAGI_S(J1)%ADR_X8(5) = LOC( ZERO_TIME(1,J1))                 ! address of the 1st element of ZERO_TIME(.,J1)
            DIAGI_S(J1)%ADR_X8(6) = LOC( ZERO_TIME(1,J1))                 ! address of the 1st element of ZERO_TIME(.,J1)
            DIAGI_S(J1)%ADR_X8(7) = LOC( TIME_S(1,J1) )                   ! address of the 1st element of TIME_S(.,J1)
!
            DIAGI_S(J1)%ADR_Y8(1) = LOC( FVAL8_G(1,J1))                   ! address of the 1st element of FVAL8_G(.,J1)
            DIAGI_S(J1)%ADR_Y8(2) = LOC( FVAL8_M(1,J1))                   ! address of the 1st element of FVAL8_M(.,J1)
            DIAGI_S(J1)%ADR_Y8(3) = LOC( FVAL8_B(1,J1))                   ! address of the 1st element of FVAL8_B(.,J1)
            DIAGI_S(J1)%ADR_Y8(4) = LOC( ZERO_LINE(1))                    ! address of the 1st element of ZERO_LINE(.,J1)
            DIAGI_S(J1)%ADR_Y8(5) = LOC( SIGD_LINE1(1,J1))                ! address of the 1st element of SIGD_LINE1(.,J1))
            DIAGI_S(J1)%ADR_Y8(6) = LOC( SIGD_LINE2(1,J1))                ! address of the 1st element of SIGD_LINE2(.,J1))
            DIAGI_S(J1)%ADR_Y8(7) = LOC( FVAL8_S(1,J1))                   ! address of the 1st element of FVAL8_S(.,J1)
!
            DIAGI_S(J1)%ADR_E8(1) = LOC( FERR8_G(1,J1))                   ! address of the 1st element of FERR8_G(.,J1)
            DIAGI_S(J1)%ADR_E8(2) = LOC( FERR8_M(1,J1))                   ! address of the 1st element of FERR8_M(.,J1)
            DIAGI_S(J1)%ADR_E8(3) = LOC( FERR8_B(1,J1))                   ! address of the 1st element of FERR8_B(.,J1)
            DIAGI_S(J1)%ADR_E8(7) = LOC( FERR8_S(1,J1))                   ! address of the 1st element of FERR8_S(.,J1)
!
! --------- titles of small plots in MultiDiaGi
!
! --------- normal station order
!
            TITS(J1) = ISITN_CHR(IBAS(J1+BASL_F-1,1))(1:ILEN(ISITN_CHR(IBAS(J1+BASL_F-1,1))))//' '// &
     &                 ISITN_CHR(IBAS(J1+BASL_F-1,2))(1:ILEN(ISITN_CHR(IBAS(J1+BASL_F-1,2))))
!
! --------- reverse station order
!
!
! --------- statistic information for titles of big DiaGi plots
!
            CALL CLRCH ( NUM_ALL_CH )
            CALL CLRCH ( MEAN_CH )
            CALL CLRCH ( VAR_CH )
            WRITE ( NUM_ALL_CH, '(i3)' ) NUM_G(J1) + NUM_M(J1) + NUM_B(J1)         ! total # of obs. --> character value
            WRITE ( MEAN_CH, '(F9.1)' ) MEAN(J1)                                   ! WTD. Mean of baseline --> character value
            WRITE ( VAR_CH, '(F9.1)' ) VAR(J1)                                     ! RMS W.Res
            CALL CHASHL ( NUM_ALL_CH )
            CALL CHASHL ( MEAN_CH )
            CALL CHASHL ( VAR_CH )
!
! --------- titles of big DiaGi plots: properties 'DELCOR','DELFUL','GDELE1','GDELE2','GDELA1','GDELA2',
!
            CALL CLRCH ( CH1_TMP )
            CALL CLRCH ( CH2_TMP )
            CALL CLRCH ( CH3_TMP )
            CALL CLRCH ( CH4_TMP )
!
            IF ( PROP .EQ. 'DELCOR' .OR. PROP .EQ. 'DELFUL' .OR. &
     &         PROP .EQ. 'RATCOR' .OR. PROP .EQ. 'RATFUL' ) THEN
!
               CH1_TMP = IDATYP_ABBR(IDATYP+1)     ! note: IDATYP numbers start with 0
!
               CH2_TMP = NUM_ALL_CH(1:ILEN(NUM_ALL_CH))//' WM='// &
     &                   MEAN_CH(1:ILEN(MEAN_CH))//' RWR='//VAR_CH(1:ILEN(VAR_CH))
! ------------ normal station order
               IF ( IBAS(J1+BASL_F-1,4) .EQ. 0 ) THEN
                  CH3_TMP = ISITN_CHR(IBAS(J1+BASL_F-1,1))(1:ILEN(ISITN_CHR(IBAS(J1+BASL_F-1,1))))
                  CH4_TMP = ISITN_CHR(IBAS(J1+BASL_F-1,2))(1:ILEN(ISITN_CHR(IBAS(J1+BASL_F-1,2))))
! ------------ reverse station order
               ELSE
                  CH3_TMP = ISITN_CHR(IBAS(J1+BASL_F-1,2))(1:ILEN(ISITN_CHR(IBAS(J1+BASL_F-1,2))))
                  CH4_TMP = ISITN_CHR(IBAS(J1+BASL_F-1,1))(1:ILEN(ISITN_CHR(IBAS(J1+BASL_F-1,1))))
               END IF
            ELSE IF ( PROP .EQ. 'GRIONO' ) THEN
               CH1_TMP = ' Del. GION contribution #'
               CH2_TMP = NUM_ALL_CH(1:ILEN(NUM_ALL_CH))//'   '
!
               IF ( IBAS(J1+BASL_F-1,4) .EQ. 0 ) THEN
!
! --------------- normal station order
!
                  CH3_TMP = ISITN_CHR(IBAS(J1+BASL_F-1,1))(1:ILEN(ISITN_CHR(IBAS(J1+BASL_F-1,1))))
                  CH4_TMP = ISITN_CHR(IBAS(J1+BASL_F-1,2))(1:ILEN(ISITN_CHR(IBAS(J1+BASL_F-1,2))))
               ELSE
!
! --------------- reverse station order
!
                  CH3_TMP = ISITN_CHR(IBAS(J1+BASL_F-1,2))(1:ILEN(ISITN_CHR(IBAS(J1+BASL_F-1,2))))
                  CH4_TMP = ISITN_CHR(IBAS(J1+BASL_F-1,1))(1:ILEN(ISITN_CHR(IBAS(J1+BASL_F-1,1))))
               END IF
            ELSE IF ( PROP .EQ. 'GDELE1' .OR. PROP .EQ. 'GDELE2' ) THEN
               CH1_TMP = ' del.res vs. el. #'
               CH2_TMP = NUM_ALL_CH(1:ILEN(NUM_ALL_CH))//' WM='// &
     &                   MEAN_CH(1:ILEN(MEAN_CH))//' RWR='//VAR_CH(1:ILEN(VAR_CH))
            ELSE IF ( PROP .EQ. 'GDELA1' .OR. PROP .EQ. 'GDELA2' ) THEN
               CH1_TMP = ' del.res vs. az. #'
               CH2_TMP = NUM_ALL_CH(1:ILEN(NUM_ALL_CH))//' WM='// &
     &                   MEAN_CH(1:ILEN(MEAN_CH))//' RWR='//VAR_CH(1:ILEN(VAR_CH))
            END IF
!
            IF ( PROP .EQ. 'GDELE1' .OR. PROP .EQ. 'GDELA1' ) THEN
!
! ------------ normal station order
!
               IF ( IBAS(J1+BASL_F-1,4) .EQ. 0 ) THEN
                  CH3_TMP = '<'//ISITN_CHR(IBAS(J1+BASL_F-1,1))(1:ILEN(ISITN_CHR(IBAS(J1+BASL_F-1,1))))//'>'
                  CH4_TMP = ISITN_CHR(IBAS(J1+BASL_F-1,2))(1:ILEN(ISITN_CHR(IBAS(J1+BASL_F-1,2))))
               ELSE
!
! --------------- reverse station order
!
                  CH3_TMP = '<'//ISITN_CHR(IBAS(J1+BASL_F-1,2))(1:ILEN(ISITN_CHR(IBAS(J1+BASL_F-1,2))))//'>'
                  CH4_TMP = ISITN_CHR(IBAS(J1+BASL_F-1,1))(1:ILEN(ISITN_CHR(IBAS(J1+BASL_F-1,1))))
               END IF
            ELSE IF ( PROP .EQ. 'GDELE2' .OR. PROP .EQ. 'GDELA2' ) THEN
!
! ------------ normal station order
!
               IF ( IBAS(J1+BASL_F-1,4) .EQ. 0 ) THEN
                  CH3_TMP = ISITN_CHR(IBAS(J1+BASL_F-1,1))(1:ILEN(ISITN_CHR(IBAS(J1+BASL_F-1,1))))
                  CH4_TMP = '<'//ISITN_CHR(IBAS(J1+BASL_F-1,2))(1:ILEN(ISITN_CHR(IBAS(J1+BASL_F-1,2))))//'>'
               ELSE
!
! --------------- reverse station order
!
                  CH3_TMP = ISITN_CHR(IBAS(J1+BASL_F-1,2))(1:ILEN(ISITN_CHR(IBAS(J1+BASL_F-1,2))))
                  CH4_TMP = '<'//ISITN_CHR(IBAS(J1+BASL_F-1,1))(1:ILEN(ISITN_CHR(IBAS(J1+BASL_F-1,1))))//'>'
               END IF
            END IF
!
            DIAGI_S(J1)%ZAG =  REPA_E(EXP_INDEX)%BAND_NAME(BAND_INDEX)(1:9)// &
     &                         REPA_E(EXP_INDEX)%BAND_NAME(BAND_INDEX)(12:13)// &
     &                         CH1_TMP(1:ILEN(CH1_TMP))// &
     &                         CH2_TMP(1:ILEN(CH2_TMP))//' '// &
     &                         CH3_TMP(1:ILEN(CH3_TMP))//' '// &
     &                         CH4_TMP(1:ILEN(CH4_TMP))
!
         END DO
      END IF
!
! --- user functions
!
      IF ( PROP .EQ. 'DELCOR' .OR. PROP .EQ. 'DELFUL' ) THEN
!
! ------ user function buttons in DiaGi headline (s. repa.i)
!
         FUNC_N = 10                                                       ! # of user functions
         FUNC_B(1)  = FUNC_BUTT(5)                                         ! button name - info user function
         FUNC_B(2)  = FUNC_BUTT(1)                                         ! button name - single ambiguity down
         FUNC_B(3)  = FUNC_BUTT(2)                                         ! button name - single suppress/recover
         FUNC_B(4)  = FUNC_BUTT(3)                                         ! button name - single ambiguity up
         FUNC_B(5)  = FUNC_BUTT(6)                                         ! button name - group suppress
         FUNC_B(6)  = FUNC_BUTT(7)                                         ! button name - group recover
         FUNC_B(7)  = FUNC_BUTT(8)                                         ! button name - group shift
         FUNC_B(8)  = FUNC_BUTT(12)                                        ! button name - reset ambiguities
         FUNC_B(9)  = FUNC_BUTT(9)                                         ! button name - connect points with the same source
         FUNC_B(10) = FUNC_BUTT(10)                                        ! button name - initialize
!
! ------ user function keys in DiaGi headline (s. repa.i)
!
         FUNC_K(1)  = FUNC_KEY(5)                                          ! key - info user function
         FUNC_K(2)  = FUNC_KEY(1)                                          ! key - single ambiguity down
         FUNC_K(3)  = FUNC_KEY(2)                                          ! key - single suppress/recover
         FUNC_K(4)  = FUNC_KEY(3)                                          ! key - single ambiguity up
         FUNC_K(5)  = FUNC_KEY(6)                                          ! key - group suppress
         FUNC_K(6)  = FUNC_KEY(7)                                          ! key - group recover
         FUNC_K(7)  = FUNC_KEY(8)                                          ! key - group shift
         FUNC_K(8)  = FUNC_KEY(12)                                         ! key - reset ambiguities
         FUNC_K(9)  = FUNC_KEY(9)                                          ! key - connect points with the same source
         FUNC_K(10) = FUNC_KEY(10)                                         ! key - initialize
!
         COL_S_IDX = 7                                                     ! colour index for connecting lines
!
         DO J1=1,BASL_L-BASL_F+1                                           ! start loop over baseline(plot) indices (J1)
!C       DO J1=1,BASL_NUM
!
            DIAGI_S(J1)%NUSER_FUNC = 14                                    ! # of DiaGi user functions
!
            DIAGI_S(J1)%USER_FUNC(1)   = LOC_EXT(REPINFO)   ! address of the entry point REPINFO
            DIAGI_S(J1)%USER_FUNC(2)   = LOC_EXT(REPPTSH)   ! address of the entry point REPPTSH
            DIAGI_S(J1)%USER_FUNC(3)   = LOC_EXT(REPPTSU)   ! address of the entry point REPPTSU
            DIAGI_S(J1)%USER_FUNC(4)   = LOC_EXT(REPPTSH)   ! address of the entry point REPPTSH
            DIAGI_S(J1)%USER_FUNC(5)   = LOC_EXT(REPGRSU)   ! address of the entry point REPGRSU
            DIAGI_S(J1)%USER_FUNC(6)   = LOC_EXT(REPGRSU)   ! address of the entry point REPGRSU
            DIAGI_S(J1)%USER_FUNC(7)   = LOC_EXT(REPGRSH)   ! address of the entry point REPGRSH
            DIAGI_S(J1)%USER_FUNC(8)   = LOC_EXT(REPGRRS)   ! address of the entry point REPGRRS
            DIAGI_S(J1)%USER_FUNC(9)   = LOC_EXT(REPCONN)   ! address of the entry point REPCONN
            DIAGI_S(J1)%USER_FUNC(10)  = LOC_EXT(REPCONN)   ! address of the entry point REPCONN
            DIAGI_S(J1)%USER_FUNC(11)  = LOC_EXT(REPCONN)   ! address of the entry point REPCONN
            DIAGI_S(J1)%USER_FUNC(12)  = LOC_EXT(REPBASL)   ! address of the entry point REPBASL
            DIAGI_S(J1)%USER_FUNC(13)  = LOC_EXT(REPBASL)   ! address of the entry point REPBASL
            DIAGI_S(J1)%USER_FUNC(14)  = LOC_EXT(REPGOOD)   ! address of the entry point REPGOOD
!
            DIAGI_S(J1)%USER_ARG(0,1) = 10                                 ! # of arguments for REPINFO
            DIAGI_S(J1)%USER_ARG(1,1) = LOC(DIAGI_S(J1))                   ! DIAGI record
            DIAGI_S(J1)%USER_ARG(2,1) = LOC(IBOBSM)                        ! total # of parameters (G+M+B)
            DIAGI_S(J1)%USER_ARG(3,1) = LOC(INFO_CHR_G(1,J1))              ! information for bottom line (good)
            DIAGI_S(J1)%USER_ARG(4,1) = LOC(INFO_CHR_M(1,J1))              ! information for bottom line (recoverable)
            DIAGI_S(J1)%USER_ARG(5,1) = LOC(INFO_CHR_B(1,J1))              ! information for bottom line (bad)
            DIAGI_S(J1)%USER_ARG(6,1) = LOC(GMB(1,J1))                     ! flags for good(G), recoverable(M) or bad(B) observation
            DIAGI_S(J1)%USER_ARG(7,1) = LOC(FUNC_N)                        ! # of user functions
            DIAGI_S(J1)%USER_ARG(8,1) = LOC(FUNC_B)                        ! button names for user functions
            DIAGI_S(J1)%USER_ARG(9,1) = LOC(FUNC_K)                        ! user functions keys
            DIAGI_S(J1)%USER_ARG(10,1) = LOC(IUER)                         ! error handler
!
            DIAGI_S(J1)%USER_ARG(0,2)  = 23                                ! # of arguments for REPPTSH
            DIAGI_S(J1)%USER_ARG(1,2)  = LOC(DIAGI_S(J1))                  ! argument list for REPPTSH
            DIAGI_S(J1)%USER_ARG(2,2)  = LOC(NUM_ALL(J1))                  ! total # of parameters (G+M+B)
            DIAGI_S(J1)%USER_ARG(3,2)  = LOC(TIME(1,J1))                   ! all arguments (G+M+B)
            DIAGI_S(J1)%USER_ARG(4,2)  = LOC(FVAL8_A(1,J1))                ! all values (G+M+B)
            DIAGI_S(J1)%USER_ARG(5,2)  = LOC(FVAL8_G(1,J1))                ! good values (G)
            DIAGI_S(J1)%USER_ARG(6,2)  = LOC(FERR8_G(1,J1))                ! errors (G)
            DIAGI_S(J1)%USER_ARG(7,2)  = LOC(FVAL8_M(1,J1))                ! recoverable values (M)
            DIAGI_S(J1)%USER_ARG(8,2)  = LOC(FVAL8_B(1,J1))                ! bad values (B)
            DIAGI_S(J1)%USER_ARG(9,2)  = LOC(NUM_AMB(1,J1))                ! # of ambiguity steps
            DIAGI_S(J1)%USER_ARG(10,2) = LOC(AMB_SP(J1))                   ! ambiguity step size
!%%%        print*,'AMB_SP(',J1,')=',AMB_SP(J1)
            DIAGI_S(J1)%USER_ARG(11,2) = LOC(IWAY_DOWN)                    ! # of ambiguity steps (down)
            DIAGI_S(J1)%USER_ARG(12,2) = LOC(REC_OBS(1,J1))                ! record #s of baseline observations
            DIAGI_S(J1)%USER_ARG(13,2) = LOC(FUNC_N)                       ! # of user functions
            DIAGI_S(J1)%USER_ARG(14,2) = LOC(FUNC_B)                       ! button names for user functions
            DIAGI_S(J1)%USER_ARG(15,2) = LOC(FUNC_K)                       ! user functions keys
            DIAGI_S(J1)%USER_ARG(16,2) = LOC(FILE_NAME_1)                  ! residual filename
            DIAGI_S(J1)%USER_ARG(17,2) = LOC(FILE_NAME_2)                  ! observation filename
            DIAGI_S(J1)%USER_ARG(18,2) = LOC(PS_NS(J1))                    ! unit (ps/ns)
            DIAGI_S(J1)%USER_ARG(19,2) = LOC(INFO_CHR_G(1,J1))             ! information for bottom line (good)
            DIAGI_S(J1)%USER_ARG(20,2) = LOC(INFO_CHR_M(1,J1))             ! information for bottom line (recoverable)
            DIAGI_S(J1)%USER_ARG(21,2) = LOC(INFO_CHR_B(1,J1))             ! information for bottom line (bad)
!
            DIAGI_S(J1)%USER_ARG(22,2) = LOC(IDA_DTP)                      ! data type ('GX','GS','PX','PS')
!
            DIAGI_S(J1)%USER_ARG(23,2) = LOC(IUER)                         ! error handler 
!
            DIAGI_S(J1)%USER_ARG(0,3)  = 23                                ! # of arguments for REPPTSU
            DIAGI_S(J1)%USER_ARG(1,3)  = LOC(DIAGI_S(J1))                  ! DIAGI record
            DIAGI_S(J1)%USER_ARG(2,3)  = LOC(NUM_ALL(J1))                  ! total # of parameters (G+M+B)
            DIAGI_S(J1)%USER_ARG(3,3)  = LOC(GMB(1,J1))                    ! flag for good(G), man.suppr.(M) or bad(B) observation
            DIAGI_S(J1)%USER_ARG(4,3)  = LOC(TIME(1,J1))                   ! all arguments (G+M+B)
            DIAGI_S(J1)%USER_ARG(5,3)  = LOC(FVAL8_A(1,J1))                ! all values (G+M+B)
            DIAGI_S(J1)%USER_ARG(6,3)  = LOC(FERR8_A(1,J1))                ! all errors (G+M+B)
            DIAGI_S(J1)%USER_ARG(7,3)  = LOC(TIME_G(1,J1))                 ! arguments (G)
            DIAGI_S(J1)%USER_ARG(8,3)  = LOC(FVAL8_G(1,J1))                ! values (G)
            DIAGI_S(J1)%USER_ARG(9,3)  = LOC(FERR8_G(1,J1))                ! errors (G)
            DIAGI_S(J1)%USER_ARG(10,3) = LOC(TIME_M(1,J1))                 ! arguments (M)
            DIAGI_S(J1)%USER_ARG(11,3) = LOC(FVAL8_M(1,J1))                ! values (M)
            DIAGI_S(J1)%USER_ARG(12,3) = LOC(FERR8_M(1,J1))                ! errors (M)
            DIAGI_S(J1)%USER_ARG(13,3) = LOC(INFO_CHR_G(1,J1))             ! information for bottom line (good)
            DIAGI_S(J1)%USER_ARG(14,3) = LOC(INFO_CHR_M(1,J1))             ! information for bottom line (recoverable)
            DIAGI_S(J1)%USER_ARG(15,3) = LOC(REC_OBS(1,J1))                ! record #s of baseline observations
            DIAGI_S(J1)%USER_ARG(16,3) = LOC(REC_OBS_G(1,J1))              ! residual filename
            DIAGI_S(J1)%USER_ARG(17,3) = LOC(REC_OBS_M(1,J1))              ! observation filename
            DIAGI_S(J1)%USER_ARG(18,3) = LOC(FILE_NAME_1)                  ! residual filename
            DIAGI_S(J1)%USER_ARG(19,3) = LOC(FILE_NAME_2)                  ! observation filename
            DIAGI_S(J1)%USER_ARG(20,3) = LOC(FUNC_N)                       ! # of user functions
            DIAGI_S(J1)%USER_ARG(21,3) = LOC(FUNC_B)                       ! button names for user functions
            DIAGI_S(J1)%USER_ARG(22,3) = LOC(FUNC_K)                       ! user functions keys
            DIAGI_S(J1)%USER_ARG(23,3) = LOC(IUER)                         ! error handler
!
            DIAGI_S(J1)%USER_ARG(0,4)  = 23                                ! # of arguments for REPPTSH
            DIAGI_S(J1)%USER_ARG(1,4)  = LOC(DIAGI_S(J1))                  ! DIAGI record
            DIAGI_S(J1)%USER_ARG(2,4)  = LOC(NUM_ALL(J1))                  ! total # of parameters (G+M+B)
            DIAGI_S(J1)%USER_ARG(3,4)  = LOC(TIME(1,J1))                   ! all arguments (G+M+B)
            DIAGI_S(J1)%USER_ARG(4,4)  = LOC(FVAL8_A(1,J1))                ! all values (G+M+B)
            DIAGI_S(J1)%USER_ARG(5,4)  = LOC(FVAL8_G(1,J1))                ! values (G)
            DIAGI_S(J1)%USER_ARG(6,4)  = LOC(FERR8_G(1,J1))                ! errors (G)
            DIAGI_S(J1)%USER_ARG(7,4)  = LOC(FVAL8_M(1,J1))                ! values (M)
            DIAGI_S(J1)%USER_ARG(8,4)  = LOC(FVAL8_B(1,J1))                ! values (B)
            DIAGI_S(J1)%USER_ARG(9,4)  = LOC(NUM_AMB(1,J1))                ! # of ambiguity steps
            DIAGI_S(J1)%USER_ARG(10,4) = LOC(AMB_SP(J1))                   ! ambiguity step size
            DIAGI_S(J1)%USER_ARG(11,4) = LOC(IWAY_UP)                      ! # of ambiguity steps (up)
            DIAGI_S(J1)%USER_ARG(12,4) = LOC(REC_OBS(1,J1))                ! record #s of baseline observations
            DIAGI_S(J1)%USER_ARG(13,4) = LOC(FUNC_N)                       ! # of user functions
            DIAGI_S(J1)%USER_ARG(14,4) = LOC(FUNC_B)                       ! button names for user functions
            DIAGI_S(J1)%USER_ARG(15,4) = LOC(FUNC_K)                       ! user functions keys
            DIAGI_S(J1)%USER_ARG(16,4) = LOC(FILE_NAME_1)                  ! residual filename
            DIAGI_S(J1)%USER_ARG(17,4) = LOC(FILE_NAME_2)                  ! observation filename
            DIAGI_S(J1)%USER_ARG(18,4) = LOC(PS_NS(J1))                    ! unit (ps/ns)
            DIAGI_S(J1)%USER_ARG(19,4) = LOC(INFO_CHR_G(1,J1))             ! information for bottom line (good)
            DIAGI_S(J1)%USER_ARG(20,4) = LOC(INFO_CHR_M(1,J1))             ! information for bottom line (recoverable)
            DIAGI_S(J1)%USER_ARG(21,4) = LOC(INFO_CHR_B(1,J1))             ! information for bottom line (bad)
!
            DIAGI_S(J1)%USER_ARG(22,4) = LOC(IDA_DTP)                      ! data type ('GX','GS','PX','PS')
!
            DIAGI_S(J1)%USER_ARG(23,4) = LOC(IUER)                         ! error handler

!
            DIAGI_S(J1)%USER_ARG(0,5)  = 23                                ! # of arguments for REPGRSU
            DIAGI_S(J1)%USER_ARG(1,5)  = LOC(SUPKEY_S)                     ! suppression key
            DIAGI_S(J1)%USER_ARG(2,5)  = LOC(DIAGI_S(J1))                  ! DIAGI record
            DIAGI_S(J1)%USER_ARG(3,5)  = LOC(NUM_ALL(J1))                  ! total # of parameters (G+M+B)
            DIAGI_S(J1)%USER_ARG(4,5)  = LOC(GMB(1,J1))                    ! flag for good(G), recoverable(M) or bad(B) observation
            DIAGI_S(J1)%USER_ARG(5,5)  = LOC(TIME(1,J1))                   ! all arguments (G+M+B)
            DIAGI_S(J1)%USER_ARG(6,5)  = LOC(FVAL8_A(1,J1))                ! all values (G+M+B)
            DIAGI_S(J1)%USER_ARG(7,5)  = LOC(TIME_G(1,J1))                 ! arguments (G)
            DIAGI_S(J1)%USER_ARG(8,5)  = LOC(FVAL8_G(1,J1))                ! values (G)
            DIAGI_S(J1)%USER_ARG(9,5)  = LOC(FERR8_G(1,J1))                ! errors (G)
            DIAGI_S(J1)%USER_ARG(10,5) = LOC(TIME_M(1,J1))                 ! arguments (M)
            DIAGI_S(J1)%USER_ARG(11,5) = LOC(FVAL8_M(1,J1))                ! values (M)
            DIAGI_S(J1)%USER_ARG(12,5) = LOC(FERR8_M(1,J1))                ! errors (M)
            DIAGI_S(J1)%USER_ARG(13,5) = LOC(INFO_CHR_G(1,J1))             ! information for bottom line (good)
            DIAGI_S(J1)%USER_ARG(14,5) = LOC(INFO_CHR_M(1,J1))             ! information for bottom line (recoverable)
            DIAGI_S(J1)%USER_ARG(15,5) = LOC(REC_OBS(1,J1))                ! record #s of baseline observations
            DIAGI_S(J1)%USER_ARG(16,5) = LOC(REC_OBS_G(1,J1))              ! residual filename
            DIAGI_S(J1)%USER_ARG(17,5) = LOC(REC_OBS_M(1,J1))              ! observation filename
            DIAGI_S(J1)%USER_ARG(18,5) = LOC(FILE_NAME_1)                  ! residual filename
            DIAGI_S(J1)%USER_ARG(19,5) = LOC(FILE_NAME_2)                  ! observation filename
            DIAGI_S(J1)%USER_ARG(20,5) = LOC(FUNC_N)                       ! # of user functions
            DIAGI_S(J1)%USER_ARG(21,5) = LOC(FUNC_B)                       ! button names for user functions
            DIAGI_S(J1)%USER_ARG(22,5) = LOC(FUNC_K)                       ! user functions keys
            DIAGI_S(J1)%USER_ARG(23,5) = LOC(IUER)                         ! error handler
!
            DIAGI_S(J1)%USER_ARG(0,6)  = 23                                ! # of arguments for REPGRSU
            DIAGI_S(J1)%USER_ARG(1,6)  = LOC(SUPKEY_R)                     ! recover key
            DIAGI_S(J1)%USER_ARG(2,6)  = LOC(DIAGI_S(J1))                  ! DIAGI record
            DIAGI_S(J1)%USER_ARG(3,6)  = LOC(NUM_ALL(J1))                  ! total # of parameters (G+M+B)
            DIAGI_S(J1)%USER_ARG(4,6)  = LOC(GMB(1,J1))                    ! flag for good(G), recoverable(M) or bad(B) observation
            DIAGI_S(J1)%USER_ARG(5,6)  = LOC(TIME(1,J1))                   ! all arguments (G+M+B)
            DIAGI_S(J1)%USER_ARG(6,6)  = LOC(FVAL8_A(1,J1))                ! all values (G+M+B)
            DIAGI_S(J1)%USER_ARG(7,6)  = LOC(TIME_G(1,J1))                 ! arguments (G)
            DIAGI_S(J1)%USER_ARG(8,6)  = LOC(FVAL8_G(1,J1))                ! values (G)
            DIAGI_S(J1)%USER_ARG(9,6)  = LOC(FERR8_G(1,J1))                ! errors (G)
            DIAGI_S(J1)%USER_ARG(10,6) = LOC(TIME_M(1,J1))                 ! arguments (M)
            DIAGI_S(J1)%USER_ARG(11,6) = LOC(FVAL8_M(1,J1))                ! values (M)
            DIAGI_S(J1)%USER_ARG(12,6) = LOC(FERR8_M(1,J1))                ! errors (M)
            DIAGI_S(J1)%USER_ARG(13,6) = LOC(INFO_CHR_G(1,J1))             ! information for bottom line (good)
            DIAGI_S(J1)%USER_ARG(14,6) = LOC(INFO_CHR_M(1,J1))             ! information for bottom line (recoverable)
            DIAGI_S(J1)%USER_ARG(15,6) = LOC(REC_OBS(1,J1))                ! record #s of baseline observations
            DIAGI_S(J1)%USER_ARG(16,6) = LOC(REC_OBS_G(1,J1))              ! residual filename
            DIAGI_S(J1)%USER_ARG(17,6) = LOC(REC_OBS_M(1,J1))              ! observation filename
            DIAGI_S(J1)%USER_ARG(18,6) = LOC(FILE_NAME_1)                  ! residual filename
            DIAGI_S(J1)%USER_ARG(19,6) = LOC(FILE_NAME_2)                  ! observation filename
            DIAGI_S(J1)%USER_ARG(20,6) = LOC(FUNC_N)                       ! # of user functions
            DIAGI_S(J1)%USER_ARG(21,6) = LOC(FUNC_B)                       ! button names for user functions
            DIAGI_S(J1)%USER_ARG(22,6) = LOC(FUNC_K)                       ! user functions keys
            DIAGI_S(J1)%USER_ARG(23,6) = LOC(IUER)                         ! error handler
!
            DIAGI_S(J1)%USER_ARG(0,7)  = 22                                ! # of arguments for REPGRSH
            DIAGI_S(J1)%USER_ARG(1,7)  = LOC(DIAGI_S(J1))                  ! DIAGI record
            DIAGI_S(J1)%USER_ARG(2,7)  = LOC(NUM_ALL(J1))                  ! total # of parameters (G+M+B)
            DIAGI_S(J1)%USER_ARG(3,7)  = LOC(N_COL)                        ! # of observation functions
            DIAGI_S(J1)%USER_ARG(4,7)  = LOC(TIME(1,J1))                   ! all arguments (G+M+B)
            DIAGI_S(J1)%USER_ARG(5,7)  = LOC(FVAL8_A(1,J1))                ! all values (G+M+B)
            DIAGI_S(J1)%USER_ARG(6,7)  = LOC(FVAL8_G(1,J1))                ! values (G)
            DIAGI_S(J1)%USER_ARG(7,7)  = LOC(FERR8_G(1,J1))                ! errors (G)
            DIAGI_S(J1)%USER_ARG(8,7)  = LOC(FVAL8_M(1,J1))                ! values (M)
            DIAGI_S(J1)%USER_ARG(9,7)  = LOC(FVAL8_B(1,J1))                ! values (B)
            DIAGI_S(J1)%USER_ARG(10,7) = LOC(NUM_AMB(1,J1))                ! # of ambiguity steps
            DIAGI_S(J1)%USER_ARG(11,7) = LOC(AMB_SP(J1))                   ! ambiguity step size
            DIAGI_S(J1)%USER_ARG(12,7) = LOC(REC_OBS(1,J1))                ! record #s of baseline observations
            DIAGI_S(J1)%USER_ARG(13,7) = LOC(FUNC_N)                       ! # of user functions
            DIAGI_S(J1)%USER_ARG(14,7) = LOC(FUNC_B)                       ! button names for user functions
            DIAGI_S(J1)%USER_ARG(15,7) = LOC(FUNC_K)                       ! user functions keys
            DIAGI_S(J1)%USER_ARG(16,7) = LOC(FILE_NAME_1)                  ! residual filename
            DIAGI_S(J1)%USER_ARG(17,7) = LOC(FILE_NAME_2)                  ! observation filename
            DIAGI_S(J1)%USER_ARG(18,7) = LOC(PS_NS(J1))                    ! unit (ps/ns)
            DIAGI_S(J1)%USER_ARG(19,7) = LOC(INFO_CHR_G(1,J1))             ! information for bottom line (good)
            DIAGI_S(J1)%USER_ARG(20,7) = LOC(INFO_CHR_M(1,J1))             ! information for bottom line (recoverable)
            DIAGI_S(J1)%USER_ARG(21,7) = LOC(INFO_CHR_B(1,J1))             ! information for bottom line (bad)
            DIAGI_S(J1)%USER_ARG(22,7) = LOC(IUER)                         ! error handler
!
            DIAGI_S(J1)%USER_ARG(0,8)  = 27                                ! # of arguments for REPGRRS
            DIAGI_S(J1)%USER_ARG(1,8)  = LOC(DIAGI_S(J1))                  ! DIAGI record
            DIAGI_S(J1)%USER_ARG(2,8)  = LOC(NUM_ALL(J1))                  ! total # of parameters (G+M+B)
            DIAGI_S(J1)%USER_ARG(3,8)  = LOC(N_COL)                        ! # of observation functions
            DIAGI_S(J1)%USER_ARG(4,8)  = LOC(TIME(1,J1))                   ! all arguments (G+M+B)
            DIAGI_S(J1)%USER_ARG(5,8)  = LOC(FVAL8_A(1,J1))                ! all values (G+M+B)
            DIAGI_S(J1)%USER_ARG(6,8)  = LOC(FVAL8_G(1,J1))                ! values (G)
            DIAGI_S(J1)%USER_ARG(7,8)  = LOC(FERR8_G(1,J1))                ! errors (G)
            DIAGI_S(J1)%USER_ARG(8,8)  = LOC(FVAL8_M(1,J1))                ! values (M)
            DIAGI_S(J1)%USER_ARG(9,8)  = LOC(FVAL8_B(1,J1))                ! values (B)
            DIAGI_S(J1)%USER_ARG(10,8) = LOC(NUM_AMB(1,J1))                ! # of ambiguity steps
            DIAGI_S(J1)%USER_ARG(11,8) = LOC(AMB_SP(J1))                   ! ambiguity step size
            DIAGI_S(J1)%USER_ARG(12,8) = LOC(REC_OBS(1,J1))                ! record #s of baseline observations (all)
            DIAGI_S(J1)%USER_ARG(13,8) = LOC(REC_OBS_G(1,J1))              ! record #s (G)
            DIAGI_S(J1)%USER_ARG(14,8) = LOC(REC_OBS_M(1,J1))              ! record #s (M)
            DIAGI_S(J1)%USER_ARG(15,8) = LOC(REC_OBS_B(1,J1))              ! record #s (B)
            DIAGI_S(J1)%USER_ARG(16,8) = LOC(FUNC_N)                       ! # of user functions
            DIAGI_S(J1)%USER_ARG(17,8) = LOC(FUNC_B)                       ! button names for user functions
            DIAGI_S(J1)%USER_ARG(18,8) = LOC(FUNC_K)                       ! user functions keys
            DIAGI_S(J1)%USER_ARG(19,8) = LOC(FUNC_NNN)                     ! max. # of user functions (repa.i)
            DIAGI_S(J1)%USER_ARG(20,8) = LOC(FUNC_BUTT)                    ! all avail. button names for user functions (repa.i)
            DIAGI_S(J1)%USER_ARG(21,8) = LOC(FILE_NAME_1)                  ! residual filename
            DIAGI_S(J1)%USER_ARG(22,8) = LOC(FILE_NAME_2)                  ! observation filename
            DIAGI_S(J1)%USER_ARG(23,8) = LOC(PS_NS(J1))                    ! unit (ps/ns)
            DIAGI_S(J1)%USER_ARG(24,8) = LOC(INFO_CHR_G(1,J1))             ! information for bottom line (good)
            DIAGI_S(J1)%USER_ARG(25,8) = LOC(INFO_CHR_M(1,J1))             ! information for bottom line (recoverable)
            DIAGI_S(J1)%USER_ARG(26,8) = LOC(INFO_CHR_B(1,J1))             ! information for bottom line (bad)
            DIAGI_S(J1)%USER_ARG(27,8) = LOC(IUER)                         ! error handler
!
            DIAGI_S(J1)%USER_ARG(0,9)  = 25                                ! # of arguments for REPCONN
            DIAGI_S(J1)%USER_ARG(1,9)  = LOC(CONCON)                       ! flag for connecting lines
            DIAGI_S(J1)%USER_ARG(2,9)  = LOC(NUM_ALL(J1))                  ! dimension of baseline arrays
            DIAGI_S(J1)%USER_ARG(3,9)  = LOC(DIAGI_S(J1))                  ! DIAGI record
            DIAGI_S(J1)%USER_ARG(4,9)  = LOC(COL_S_IDX)                    ! colour index for connecting  points
            DIAGI_S(J1)%USER_ARG(5,9)  = LOC(INFO_CHR_G(1,J1))             ! information for bottom line (good)
            DIAGI_S(J1)%USER_ARG(6,9)  = LOC(INFO_CHR_M(1,J1))             ! information for bottom line (recoverable)
            DIAGI_S(J1)%USER_ARG(7,9)  = LOC(INFO_CHR_B(1,J1))             ! information for bottom line (bad)
            DIAGI_S(J1)%USER_ARG(8,9)  = LOC(TIME_G(1,J1))                 ! arguments (G)
            DIAGI_S(J1)%USER_ARG(9,9)  = LOC(FVAL8_G(1,J1))                ! values (G)
            DIAGI_S(J1)%USER_ARG(10,9) = LOC(FERR8_G(1,J1))                ! errors (G)
            DIAGI_S(J1)%USER_ARG(11,9) = LOC(TIME_M(1,J1))                 ! arguments (M)
            DIAGI_S(J1)%USER_ARG(12,9) = LOC(FVAL8_M(1,J1))                ! values (M)
            DIAGI_S(J1)%USER_ARG(13,9) = LOC(FERR8_M(1,J1))                ! errors (M)
            DIAGI_S(J1)%USER_ARG(14,9) = LOC(TIME_B(1,J1))                 ! arguments (B)
            DIAGI_S(J1)%USER_ARG(15,9) = LOC(FVAL8_B(1,J1))                ! values (B)
            DIAGI_S(J1)%USER_ARG(16,9) = LOC(FERR8_B(1,J1))                ! errors (B)
            DIAGI_S(J1)%USER_ARG(17,9) = LOC(TIME_S(1,J1))                 ! arguments (connecting lines)
            DIAGI_S(J1)%USER_ARG(18,9) = LOC(FVAL8_S(1,J1))                ! values (connecting lines)
            DIAGI_S(J1)%USER_ARG(19,9) = LOC(FERR8_S(1,J1))                ! errors (connecting lines)
            DIAGI_S(J1)%USER_ARG(20,9) = LOC(FUNC_N)                       ! # of user functions
            DIAGI_S(J1)%USER_ARG(21,9) = LOC(FUNC_B)                       ! button names for user functions
            DIAGI_S(J1)%USER_ARG(22,9) = LOC(FUNC_K)                       ! user functions keys
            DIAGI_S(J1)%USER_ARG(23,9) = LOC(FUNC_NNN)                     ! max. # of user functions (repa.i)
            DIAGI_S(J1)%USER_ARG(24,9) = LOC(FUNC_BUTT)                    ! all avail. button names for user functions (repa.i)
            DIAGI_S(J1)%USER_ARG(25,9) = LOC(IUER)                         ! error handler
!
            DIAGI_S(J1)%USER_ARG(0,10)  = 25                               ! # of arguments for REPCONN
            DIAGI_S(J1)%USER_ARG(1,10)  = LOC(CONDEL)                      ! flag for user input of source name for connecting lines
            DIAGI_S(J1)%USER_ARG(2,10)  = LOC(NUM_ALL(J1))                 ! dimension of baseline arrays
            DIAGI_S(J1)%USER_ARG(3,10)  = LOC(DIAGI_S(J1))                 ! DIAGI record
            DIAGI_S(J1)%USER_ARG(4,10)  = LOC(COL_S_IDX)                   ! colour index for connecting  points
            DIAGI_S(J1)%USER_ARG(5,10)  = LOC(INFO_CHR_G(1,J1))            ! information for bottom line (good)
            DIAGI_S(J1)%USER_ARG(6,10)  = LOC(INFO_CHR_M(1,J1))            ! information for bottom line (recoverable)
            DIAGI_S(J1)%USER_ARG(7,10)  = LOC(INFO_CHR_B(1,J1))            ! information for bottom line (bad)
            DIAGI_S(J1)%USER_ARG(8,10)  = LOC(TIME_G(1,J1))                ! arguments (G)
            DIAGI_S(J1)%USER_ARG(9,10)  = LOC(FVAL8_G(1,J1))               ! values (G)
            DIAGI_S(J1)%USER_ARG(10,10) = LOC(FERR8_G(1,J1))               ! errors (G)
            DIAGI_S(J1)%USER_ARG(11,10) = LOC(TIME_M(1,J1))                ! arguments (M)
            DIAGI_S(J1)%USER_ARG(12,10) = LOC(FVAL8_M(1,J1))               ! values (M)
            DIAGI_S(J1)%USER_ARG(13,10) = LOC(FERR8_M(1,J1))               ! errors (M)
            DIAGI_S(J1)%USER_ARG(14,10) = LOC(TIME_B(1,J1))                ! arguments (B)
            DIAGI_S(J1)%USER_ARG(15,10) = LOC(FVAL8_B(1,J1))               ! values (B)
            DIAGI_S(J1)%USER_ARG(16,10) = LOC(FERR8_B(1,J1))               ! errors (B)
            DIAGI_S(J1)%USER_ARG(17,10) = LOC(TIME_S(1,J1))                ! arguments (connecting lines)
            DIAGI_S(J1)%USER_ARG(18,10) = LOC(FVAL8_S(1,J1))               ! values (connecting lines)
            DIAGI_S(J1)%USER_ARG(19,10) = LOC(FERR8_S(1,J1))               ! errors (connecting lines)
            DIAGI_S(J1)%USER_ARG(20,10) = LOC(FUNC_N)                      ! # of user functions
            DIAGI_S(J1)%USER_ARG(21,10) = LOC(FUNC_B)                      ! button names for user functions
            DIAGI_S(J1)%USER_ARG(22,10) = LOC(FUNC_K)                      ! user functions keys
            DIAGI_S(J1)%USER_ARG(23,10) = LOC(FUNC_NNN)                    ! max. # of user functions (repa.i)
            DIAGI_S(J1)%USER_ARG(24,10) = LOC(FUNC_BUTT)                   ! all avail. button names for user functions (repa.i)
            DIAGI_S(J1)%USER_ARG(25,10) = LOC(IUER)                        ! error handler
            DIAGI_S(J1)%INIT_USER_FUNC = 10                                ! initialize user function
!
            DIAGI_S(J1)%USER_ARG(0,11)  = 25                               ! # of arguments for REPCONN
            DIAGI_S(J1)%USER_ARG(1,11)  = LOC(CONINP)                      ! flag for deleting of connecting lines
            DIAGI_S(J1)%USER_ARG(2,11)  = LOC(NUM_ALL(J1))                 ! dimension of baseline arrays
            DIAGI_S(J1)%USER_ARG(3,11)  = LOC(DIAGI_S(J1))                 ! DIAGI record
            DIAGI_S(J1)%USER_ARG(4,11)  = LOC(COL_S_IDX)                   ! colour index for connecting  points
            DIAGI_S(J1)%USER_ARG(5,11)  = LOC(INFO_CHR_G(1,J1))            ! information for bottom line (good)
            DIAGI_S(J1)%USER_ARG(6,11)  = LOC(INFO_CHR_M(1,J1))            ! information for bottom line (recoverable)
            DIAGI_S(J1)%USER_ARG(7,11)  = LOC(INFO_CHR_B(1,J1))            ! information for bottom line (bad)
            DIAGI_S(J1)%USER_ARG(8,11)  = LOC(TIME_G(1,J1))                ! arguments (G)
            DIAGI_S(J1)%USER_ARG(9,11)  = LOC(FVAL8_G(1,J1))               ! values (G)
            DIAGI_S(J1)%USER_ARG(10,11) = LOC(FERR8_G(1,J1))               ! errors (G)
            DIAGI_S(J1)%USER_ARG(11,11) = LOC(TIME_M(1,J1))                ! arguments (M)
            DIAGI_S(J1)%USER_ARG(12,11) = LOC(FVAL8_M(1,J1))               ! values (M)
            DIAGI_S(J1)%USER_ARG(13,11) = LOC(FERR8_M(1,J1))               ! errors (M)
            DIAGI_S(J1)%USER_ARG(14,11) = LOC(TIME_B(1,J1))                ! arguments (B)
            DIAGI_S(J1)%USER_ARG(15,11) = LOC(FVAL8_B(1,J1))               ! values (B)
            DIAGI_S(J1)%USER_ARG(16,11) = LOC(FERR8_B(1,J1))               ! errors (B)
            DIAGI_S(J1)%USER_ARG(17,11) = LOC(TIME_S(1,J1))                ! arguments (connecting lines)
            DIAGI_S(J1)%USER_ARG(18,11) = LOC(FVAL8_S(1,J1))               ! values (connecting lines)
            DIAGI_S(J1)%USER_ARG(19,11) = LOC(FERR8_S(1,J1))               ! errors (connecting lines)
            DIAGI_S(J1)%USER_ARG(20,11) = LOC(FUNC_N)                      ! # of user functions
            DIAGI_S(J1)%USER_ARG(21,11) = LOC(FUNC_B)                      ! button names for user functions
            DIAGI_S(J1)%USER_ARG(22,11) = LOC(FUNC_K)                      ! user functions keys
            DIAGI_S(J1)%USER_ARG(23,11) = LOC(FUNC_NNN)                    ! max. # of user functions (repa.i)
            DIAGI_S(J1)%USER_ARG(24,11) = LOC(FUNC_BUTT)                   ! all avail. button names for user functions (repa.i)
            DIAGI_S(J1)%USER_ARG(25,11) = LOC(IUER)                        ! error handler
!
            DIAGI_S(J1)%USER_ARG(0,12)  = 5                                ! # of arguments for REPBASL
            DIAGI_S(J1)%USER_ARG(1,12)  = LOC(REPA_PAR)
            DIAGI_S(J1)%USER_ARG(2,12)  = LOC(DIAGI_S(J1))                 ! DIAGI record
            DIAGI_S(J1)%USER_ARG(3,12)  = LOC(REPA_FLAG)
            DIAGI_S(J1)%USER_ARG(4,12)  = LOC(REPA_PREV_VAR)
            DIAGI_S(J1)%USER_ARG(5,12)  = LOC(DIAGI_S(J1)%MD_OUT)
!
            DIAGI_S(J1)%USER_ARG(0,13)  = 5                                ! # of arguments for REPBASL
            DIAGI_S(J1)%USER_ARG(1,13)  = LOC(REPA_PAR)
            DIAGI_S(J1)%USER_ARG(2,13)  = LOC(DIAGI_S(J1))                 ! DIAGI record
            DIAGI_S(J1)%USER_ARG(3,13)  = LOC(REPA_FLAG)
            DIAGI_S(J1)%USER_ARG(4,13)  = LOC(REPA_NEXT_VAR)
            DIAGI_S(J1)%USER_ARG(5,13)  = LOC(DIAGI_S(J1)%MD_OUT)
!
            DIAGI_S(J1)%USER_ARG(0,14)  = 1                                 ! # of arguments for REPGOOD
            DIAGI_S(J1)%USER_ARG(1,14)  = LOC(DIAGI_S(J1))                  ! DIAGI record
!
! --------- user function keys to be delivered to DiaGi (s. repa.i)
!
            DIAGI_S(J1)%USER_CHR(1)  = FUNC_KEY(5)                         ! info
            DIAGI_S(J1)%USER_CHR(2)  = FUNC_KEY(1)                         ! single shift down
            DIAGI_S(J1)%USER_CHR(3)  = FUNC_KEY(2)                         ! single suppress/recover
            DIAGI_S(J1)%USER_CHR(4)  = FUNC_KEY(3)                         ! single shift up
            DIAGI_S(J1)%USER_CHR(5)  = FUNC_KEY(6)                         ! group suppress
            DIAGI_S(J1)%USER_CHR(6)  = FUNC_KEY(7)                         ! group recover
            DIAGI_S(J1)%USER_CHR(7)  = FUNC_KEY(8)                         ! group shift
            DIAGI_S(J1)%USER_CHR(8)  = FUNC_KEY(12)                        ! ambiguity reset
            DIAGI_S(J1)%USER_CHR(9)  = FUNC_KEY(9)                         ! connect
            DIAGI_S(J1)%USER_CHR(10) = 'A'                                 ! disconnect observations with the same source
            DIAGI_S(J1)%USER_CHR(11) = FUNC_KEY(11)                        ! connect (user input)
            DIAGI_S(J1)%USER_CHR(12) = CHAR(PAGE_UPDN(1))                  ! decimal code for PgUp keybord key
            DIAGI_S(J1)%USER_CHR(13) = CHAR(PAGE_UPDN(2))                  ! decimal code for PgDn keybord key
            DIAGI_S(J1)%USER_CHR(14) = CHAR(13)                            ! decimal code for PgDn keybord key
!
! --------- bottom message and units
!
            DIAGI_S(J1)%MESS_BOT = 'left mouse click to initialize --> then choose user function in the headline'
!
            IF ( IDA_DTP == 'PX' .OR. IDA_DTP == 'PS' ) THEN
               DIAGI_S(J1)%ARG_UNITS = 'turns of phase'
            ELSE
               DIAGI_S(J1)%ARG_UNITS = PS_NS(J1)//' vs. hours'
            END IF
         END DO                                                            ! end loop over baseline(plot) indices (J1)
      END IF
!
! --- properties 'RATCOR' and 'RATFUL'
!
      IF ( PROP .EQ. 'RATCOR' .OR. PROP .EQ. 'RATFUL' ) THEN
!
         FUNC_N = 3                                                        ! # of user functions
         FUNC_B(1) = FUNC_BUTT(5)                                          ! button name - info user function
         FUNC_B(2) = FUNC_BUTT(9)                                          ! button name - connect user function
         FUNC_B(3) = FUNC_BUTT(10)                                         ! button name - initialize
!
         FUNC_K(1) = FUNC_KEY(5)                                           ! key - info user function
         FUNC_K(2) = FUNC_KEY(9)                                           ! key - connect user function
         FUNC_K(3) = FUNC_KEY(10)                                          ! key - initialize
!
         COL_S_IDX = 7                                                     ! colour index for connecting  lines
!
         DO J1=1,BASL_L-BASL_F+1                                           ! start loop over baseline(plot) indices (
!
            DIAGI_S(J1 )%ARG_UNITS  = 'fs vs. hours'                       ! units
!
            DIAGI_S(J1)%NUSER_FUNC     = 7                                 ! # of DiaGi user functions
            DIAGI_S(J1)%USER_FUNC(1)   = LOC_EXT(REPINFO)                      ! address of the entry point REPINFO
            DIAGI_S(J1)%USER_FUNC(2)   = LOC_EXT(REPCONN)                      ! address of the entry point REPCONN
            DIAGI_S(J1)%USER_FUNC(3)   = LOC_EXT(REPCONN)                      ! address of the entry point REPCONN
            DIAGI_S(J1)%USER_FUNC(4)   = LOC_EXT(REPCONN)                      ! address of the entry point REPCONN
            DIAGI_S(J1)%USER_FUNC(5)   = LOC_EXT(REPBASL)                      ! address of the entry point REPBASL
            DIAGI_S(J1)%USER_FUNC(6)   = LOC_EXT(REPBASL)                      ! address of the entry point REPBASL
            DIAGI_S(J1)%USER_FUNC(7)   = LOC_EXT(REPGOOD)                      ! address of the entry point REPGOOD
!
            DIAGI_S(J1)%USER_ARG(0,1)  = 10                                ! # of arguments for REPINFO
            DIAGI_S(J1)%USER_ARG(1,1)  = LOC(DIAGI_S(J1))                  ! DIAGI record
            DIAGI_S(J1)%USER_ARG(2,1)  = LOC(IBOBSM)                       ! total # of parameters (G+M+B)
            DIAGI_S(J1)%USER_ARG(3,1)  = LOC(INFO_CHR_G(1,J1))             ! information for bottom line (good)
            DIAGI_S(J1)%USER_ARG(4,1)  = LOC(INFO_CHR_M(1,J1))             ! information for bottom line (recoverable)
            DIAGI_S(J1)%USER_ARG(5,1)  = LOC(INFO_CHR_B(1,J1))             ! information for bottom line (bad)
            DIAGI_S(J1)%USER_ARG(6,1)  = LOC(GMB(1,J1))                    ! flags for good(G), recoverable(M) or bad(B) observation
            DIAGI_S(J1)%USER_ARG(7,1)  = LOC(FUNC_N)                       ! # of user functions
            DIAGI_S(J1)%USER_ARG(8,1)  = LOC(FUNC_B)                       ! button names for user functions
            DIAGI_S(J1)%USER_ARG(9,1)  = LOC(FUNC_K)                       ! user functions keys
            DIAGI_S(J1)%USER_ARG(10,1) = LOC(IUER)                         ! error handler
!
            DIAGI_S(J1)%USER_ARG(0,2)  = 25                                ! # of arguments for REPCONN
            DIAGI_S(J1)%USER_ARG(1,2)  = LOC(CONCON)                       ! flag for connecting lines
            DIAGI_S(J1)%USER_ARG(2,2)  = LOC(NUM_ALL(J1))                  ! dimension of baseline arrays
            DIAGI_S(J1)%USER_ARG(3,2)  = LOC(DIAGI_S(J1))                  ! DIAGI record
            DIAGI_S(J1)%USER_ARG(4,2)  = LOC(COL_S_IDX)                    ! colour index for connecting  points
            DIAGI_S(J1)%USER_ARG(5,2)  = LOC(INFO_CHR_G(1,J1))             ! information for bottom line (good)
            DIAGI_S(J1)%USER_ARG(6,2)  = LOC(INFO_CHR_M(1,J1))             ! information for bottom line (recoverable)
            DIAGI_S(J1)%USER_ARG(7,2)  = LOC(INFO_CHR_B(1,J1))             ! information for bottom line (bad)
            DIAGI_S(J1)%USER_ARG(8,2)  = LOC(TIME_G(1,J1))                 ! arguments (G)
            DIAGI_S(J1)%USER_ARG(9,2)  = LOC(FVAL8_G(1,J1))                ! values (G)
            DIAGI_S(J1)%USER_ARG(10,2) = LOC(FERR8_G(1,J1))                ! errors (G)
            DIAGI_S(J1)%USER_ARG(11,2) = LOC(TIME_M(1,J1))                 ! arguments (M)
            DIAGI_S(J1)%USER_ARG(12,2) = LOC(FVAL8_M(1,J1))                ! values (M)
            DIAGI_S(J1)%USER_ARG(13,2) = LOC(FERR8_M(1,J1))                ! errors (M)
            DIAGI_S(J1)%USER_ARG(14,2) = LOC(TIME_B(1,J1))                 ! arguments (B)
            DIAGI_S(J1)%USER_ARG(15,2) = LOC(FVAL8_B(1,J1))                ! values (B)
            DIAGI_S(J1)%USER_ARG(16,2) = LOC(FERR8_B(1,J1))                ! errors (B)
            DIAGI_S(J1)%USER_ARG(17,2) = LOC(TIME_S(1,J1))                 ! arguments (connecting lines)
            DIAGI_S(J1)%USER_ARG(18,2) = LOC(FVAL8_S(1,J1))                ! values (connecting lines)
            DIAGI_S(J1)%USER_ARG(19,2) = LOC(FERR8_S(1,J1))                ! errors (connecting lines)
            DIAGI_S(J1)%USER_ARG(20,2) = LOC(FUNC_N)                       ! # of user functions
            DIAGI_S(J1)%USER_ARG(21,2) = LOC(FUNC_B)                       ! button names for user functions
            DIAGI_S(J1)%USER_ARG(22,2) = LOC(FUNC_K)                       ! user functions keys
            DIAGI_S(J1)%USER_ARG(23,2) = LOC(FUNC_NNN)                     ! all avail. button names for user functions (repa.i)
            DIAGI_S(J1)%USER_ARG(24,2) = LOC(FUNC_BUTT)                    ! all avail. button names for user functions (repa.i)
            DIAGI_S(J1)%USER_ARG(25,2) = LOC(IUER)                         ! error handler
!
            DIAGI_S(J1)%USER_ARG(0,3)  = 25                                ! # of arguments for REPCONN
            DIAGI_S(J1)%USER_ARG(1,3)  = LOC(CONDEL)                       ! flag for user input of source name for connecting lines
            DIAGI_S(J1)%USER_ARG(2,3)  = LOC(NUM_ALL(J1))                  ! dimension of baseline arrays
            DIAGI_S(J1)%USER_ARG(3,3)  = LOC(DIAGI_S(J1))                  ! DIAGI record
            DIAGI_S(J1)%USER_ARG(4,3)  = LOC(COL_S_IDX)                    ! colour index for connecting  points
            DIAGI_S(J1)%USER_ARG(5,3)  = LOC(INFO_CHR_G(1,J1))             ! information for bottom line (good)
            DIAGI_S(J1)%USER_ARG(6,3)  = LOC(INFO_CHR_M(1,J1))             ! information for bottom line (recoverable)
            DIAGI_S(J1)%USER_ARG(7,3)  = LOC(INFO_CHR_B(1,J1))             ! information for bottom line (bad)
            DIAGI_S(J1)%USER_ARG(8,3)  = LOC(TIME_G(1,J1))                 ! arguments (G)
            DIAGI_S(J1)%USER_ARG(9,3)  = LOC(FVAL8_G(1,J1))                ! values (G)
            DIAGI_S(J1)%USER_ARG(10,3) = LOC(FERR8_G(1,J1))                ! errors (G)
            DIAGI_S(J1)%USER_ARG(11,3) = LOC(TIME_M(1,J1))                 ! arguments (M)
            DIAGI_S(J1)%USER_ARG(12,3) = LOC(FVAL8_M(1,J1))                ! values (M)
            DIAGI_S(J1)%USER_ARG(13,3) = LOC(FERR8_M(1,J1))                ! errors (M)
            DIAGI_S(J1)%USER_ARG(14,3) = LOC(TIME_B(1,J1))                 ! arguments (B)
            DIAGI_S(J1)%USER_ARG(15,3) = LOC(FVAL8_B(1,J1))                ! values (B)
            DIAGI_S(J1)%USER_ARG(16,3) = LOC(FERR8_B(1,J1))                ! errors (B)
            DIAGI_S(J1)%USER_ARG(17,3) = LOC(TIME_S(1,J1))                 ! arguments (connecting lines)
            DIAGI_S(J1)%USER_ARG(18,3) = LOC(FVAL8_S(1,J1))                ! values (connecting lines)
            DIAGI_S(J1)%USER_ARG(19,3) = LOC(FERR8_S(1,J1))                ! errors (connecting lines)
            DIAGI_S(J1)%USER_ARG(20,3) = LOC(FUNC_N)                       ! # of user functions
            DIAGI_S(J1)%USER_ARG(21,3) = LOC(FUNC_B)                       ! button names for user functions
            DIAGI_S(J1)%USER_ARG(22,3) = LOC(FUNC_K)                       ! user functions keys
            DIAGI_S(J1)%USER_ARG(23,3) = LOC(FUNC_NNN)                     ! all avail. button names for user functions (repa.i)
            DIAGI_S(J1)%USER_ARG(24,3) = LOC(FUNC_BUTT)                    ! all avail. button names for user functions (repa.i)
            DIAGI_S(J1)%USER_ARG(25,3) = LOC(IUER)                         ! error handler
            DIAGI_S(J1)%INIT_USER_FUNC = 3                                 ! initialize user function
!
            DIAGI_S(J1)%USER_ARG(0,4)  = 25                                ! # of arguments for REPCONN
            DIAGI_S(J1)%USER_ARG(1,4)  = LOC(CONINP)                       ! flag for deleting of connecting lines
            DIAGI_S(J1)%USER_ARG(2,4)  = LOC(NUM_ALL(J1))                  ! dimension of baseline arrays
            DIAGI_S(J1)%USER_ARG(3,4)  = LOC(DIAGI_S(J1))                  ! DIAGI record
            DIAGI_S(J1)%USER_ARG(4,4)  = LOC(COL_S_IDX)                    ! colour index for connecting  points
            DIAGI_S(J1)%USER_ARG(5,4)  = LOC(INFO_CHR_G(1,J1))             ! information for bottom line (good)
            DIAGI_S(J1)%USER_ARG(6,4)  = LOC(INFO_CHR_M(1,J1))             ! information for bottom line (recoverable)
            DIAGI_S(J1)%USER_ARG(7,4)  = LOC(INFO_CHR_B(1,J1))             ! information for bottom line (bad)
            DIAGI_S(J1)%USER_ARG(8,4)  = LOC(TIME_G(1,J1))                 ! arguments (G)
            DIAGI_S(J1)%USER_ARG(9,4)  = LOC(FVAL8_G(1,J1))                ! values (G)
            DIAGI_S(J1)%USER_ARG(10,4) = LOC(FERR8_G(1,J1))                ! errors (G)
            DIAGI_S(J1)%USER_ARG(11,4) = LOC(TIME_M(1,J1))                 ! arguments (M)
            DIAGI_S(J1)%USER_ARG(12,4) = LOC(FVAL8_M(1,J1))                ! values (M)
            DIAGI_S(J1)%USER_ARG(13,4) = LOC(FERR8_M(1,J1))                ! errors (M)
            DIAGI_S(J1)%USER_ARG(14,4) = LOC(TIME_B(1,J1))                 ! arguments (B)
            DIAGI_S(J1)%USER_ARG(15,4) = LOC(FVAL8_B(1,J1))                ! values (B)
            DIAGI_S(J1)%USER_ARG(16,4) = LOC(FERR8_B(1,J1))                ! errors (B)
            DIAGI_S(J1)%USER_ARG(17,4) = LOC(TIME_S(1,J1))                 ! arguments (connecting lines)
            DIAGI_S(J1)%USER_ARG(18,4) = LOC(FVAL8_S(1,J1))                ! values (connecting lines)
            DIAGI_S(J1)%USER_ARG(19,4) = LOC(FERR8_S(1,J1))                ! errors (connecting lines)
            DIAGI_S(J1)%USER_ARG(20,4) = LOC(FUNC_N)                       ! # of user functions
            DIAGI_S(J1)%USER_ARG(21,4) = LOC(FUNC_B)                       ! button names for user functions
            DIAGI_S(J1)%USER_ARG(22,4) = LOC(FUNC_K)                       ! user functions keys
            DIAGI_S(J1)%USER_ARG(23,4) = LOC(FUNC_NNN)                     ! all avail. button names for user functions (repa.i)
            DIAGI_S(J1)%USER_ARG(24,4) = LOC(FUNC_BUTT)                    ! all avail. button names for user functions (repa.i)
            DIAGI_S(J1)%USER_ARG(25,4) = LOC(IUER)                         ! error handler
!
            DIAGI_S(J1)%USER_ARG(0,5)  = 5                                 ! # of arguments for REPBASL
            DIAGI_S(J1)%USER_ARG(1,5)  = LOC(REPA_PAR)
            DIAGI_S(J1)%USER_ARG(2,5)  = LOC(DIAGI_S(J1))                  ! DIAGI record
            DIAGI_S(J1)%USER_ARG(3,5)  = LOC(REPA_FLAG)
            DIAGI_S(J1)%USER_ARG(4,5)  = LOC(REPA_PREV_VAR)
            DIAGI_S(J1)%USER_ARG(5,5)  = LOC(DIAGI_S(J1)%MD_OUT)
!
            DIAGI_S(J1)%USER_ARG(0,6)  = 5                                 ! # of arguments for REPBASL
            DIAGI_S(J1)%USER_ARG(1,6)  = LOC(REPA_PAR)
            DIAGI_S(J1)%USER_ARG(2,6)  = LOC(DIAGI_S(J1))                  ! DIAGI record
            DIAGI_S(J1)%USER_ARG(3,6)  = LOC(REPA_FLAG)
            DIAGI_S(J1)%USER_ARG(4,6)  = LOC(REPA_NEXT_VAR)
            DIAGI_S(J1)%USER_ARG(5,6)  = LOC(DIAGI_S(J1)%MD_OUT)
!
            DIAGI_S(J1)%USER_ARG(0,7)  = 1                                 ! # of arguments for REPGOOD
            DIAGI_S(J1)%USER_ARG(1,7)  = LOC(DIAGI_S(J1))                  ! DIAGI record
!
            DIAGI_S(J1)%USER_CHR(1)   = FUNC_KEY(5)                        ! info key
            DIAGI_S(J1)%USER_CHR(2)   = FUNC_KEY(9)                        ! connect observations with the same source
            DIAGI_S(J1)%USER_CHR(3)   = 'A'                                ! disconnect observations with the same source
            DIAGI_S(J1)%USER_CHR(4)   = FUNC_KEY(11)                       ! user input of source name to connect points
            DIAGI_S(J1)%USER_CHR(5)   = CHAR(PAGE_UPDN(1))                 ! decimal code for PgUp keybord key
            DIAGI_S(J1)%USER_CHR(6)   = CHAR(PAGE_UPDN(2))                 ! decimal code for PgDn keybord key
            DIAGI_S(J1)%USER_CHR(7)   = CHAR(13) ! decimal code for
!
! --------- bottom message
!
            DIAGI_S(J1)%MESS_BOT =  'left mouse click to initialize --> then choose user function in the headline'
         END DO
      END IF
!
! --- Properties 'GDELE1' and 'GDELE2'
!
      IF ( PROP .EQ. 'GDELE1' .OR.  PROP .EQ. 'GDELE2' .OR. PROP .EQ. 'GDELA1' .OR.  PROP .EQ. 'GDELA2' ) THEN
!
         FUNC_N = 3                                                        ! # of user functions
         FUNC_B(1) = FUNC_BUTT(5)                                          ! button name - info user function
         FUNC_B(2) = FUNC_BUTT(9)                                          ! button name - connect user function
         FUNC_B(3) = FUNC_BUTT(10)                                         ! button name - initialize
!
         FUNC_K(1) = FUNC_KEY(5)                                           ! key - info user function
         FUNC_K(2) = FUNC_KEY(9)                                           ! key - connect user function
         FUNC_K(3) = FUNC_KEY(10)                                          ! key - initialize
!
         COL_S_IDX = 7                                                     ! colour index for connecting  lines
!
         DO J1=1,BASL_L-BASL_F+1                                           ! start loop over baseline(plot) indices
!
            DIAGI_S(J1)%ARG_UNITS = PS_NS(J1)//' vs. deg.'                 ! units
!
            DIAGI_S(J1)%NUSER_FUNC = 7                                     ! # of DiaGi user functions
!
            DIAGI_S(J1)%USER_FUNC(1)   = LOC_EXT(REPINFO)                      ! address of the entry point REPINFO
            DIAGI_S(J1)%USER_FUNC(2)   = LOC_EXT(REPCONN)                      ! address of the entry point REPCONN
            DIAGI_S(J1)%USER_FUNC(3)   = LOC_EXT(REPCONN)                      ! address of the entry point REPCONN
            DIAGI_S(J1)%USER_FUNC(4)   = LOC_EXT(REPCONN)                      ! address of the entry point REPCONN
            DIAGI_S(J1)%USER_FUNC(5)   = LOC_EXT(REPBASL)                      ! address of the entry point REPBASL
            DIAGI_S(J1)%USER_FUNC(6)   = LOC_EXT(REPBASL)                      ! address of the entry point REPBASL
            DIAGI_S(J1)%USER_FUNC(7)   = LOC_EXT(REPGOOD)                      ! address of the entry point REPGOOD
!
            DIAGI_S(J1)%USER_ARG(0,1)  = 10                                ! # of arguments for REPINFO
            DIAGI_S(J1)%USER_ARG(1,1)  = LOC(DIAGI_S(J1))                  ! DIAGI record
            DIAGI_S(J1)%USER_ARG(2,1)  = LOC(IBOBSM)                       ! total # of parameters (G+M+B)
            DIAGI_S(J1)%USER_ARG(3,1)  = LOC(INFO_CHR_G(1,J1))             ! information for bottom line (good)
            DIAGI_S(J1)%USER_ARG(4,1)  = LOC(INFO_CHR_M(1,J1))             ! information for bottom line (recoverable)
            DIAGI_S(J1)%USER_ARG(5,1)  = LOC(INFO_CHR_B(1,J1))             ! information for bottom line (bad)
            DIAGI_S(J1)%USER_ARG(6,1)  = LOC(GMB(1,J1))                    ! flags for good(G), recoverable(M) or bad(B) observation
            DIAGI_S(J1)%USER_ARG(7,1)  = LOC(FUNC_N)                       ! # of user functions
            DIAGI_S(J1)%USER_ARG(8,1)  = LOC(FUNC_B)                       ! button names for user functions
            DIAGI_S(J1)%USER_ARG(9,1)  = LOC(FUNC_K)                       ! user functions keys
            DIAGI_S(J1)%USER_ARG(10,1) = LOC(IUER)                         ! error handler
!
            DIAGI_S(J1)%USER_ARG(0,2)  = 25                                ! # of arguments for REPCONN
            DIAGI_S(J1)%USER_ARG(1,2)  = LOC(CONCON)                       ! flag for connecting lines
            DIAGI_S(J1)%USER_ARG(2,2)  = LOC(NUM_ALL(J1))                  ! dimension of baseline arrays
            DIAGI_S(J1)%USER_ARG(3,2)  = LOC(DIAGI_S(J1))                  ! DIAGI record
            DIAGI_S(J1)%USER_ARG(4,2)  = LOC(COL_S_IDX)                    ! colour index for connecting  points
            DIAGI_S(J1)%USER_ARG(5,2)  = LOC(INFO_CHR_G(1,J1))             ! information for bottom line (good)
            DIAGI_S(J1)%USER_ARG(6,2)  = LOC(INFO_CHR_M(1,J1))             ! information for bottom line (recoverable)
            DIAGI_S(J1)%USER_ARG(7,2)  = LOC(INFO_CHR_B(1,J1))             ! information for bottom line (bad)
            DIAGI_S(J1)%USER_ARG(8,2)  = LOC(TIME_G(1,J1))                 ! arguments (G)
            DIAGI_S(J1)%USER_ARG(9,2)  = LOC(FVAL8_G(1,J1))                ! values (G)
            DIAGI_S(J1)%USER_ARG(10,2) = LOC(FERR8_G(1,J1))                ! errors (G)
            DIAGI_S(J1)%USER_ARG(11,2) = LOC(TIME_M(1,J1))                 ! arguments (M)
            DIAGI_S(J1)%USER_ARG(12,2) = LOC(FVAL8_M(1,J1))                ! values (M)
            DIAGI_S(J1)%USER_ARG(13,2) = LOC(FERR8_M(1,J1))                ! errors (M)
            DIAGI_S(J1)%USER_ARG(14,2) = LOC(TIME_B(1,J1))                 ! arguments (B)
            DIAGI_S(J1)%USER_ARG(15,2) = LOC(FVAL8_B(1,J1))                ! values (B)
            DIAGI_S(J1)%USER_ARG(16,2) = LOC(FERR8_B(1,J1))                ! errors (B)
            DIAGI_S(J1)%USER_ARG(17,2) = LOC(TIME_S(1,J1))                 ! arguments (connecting lines)
            DIAGI_S(J1)%USER_ARG(18,2) = LOC(FVAL8_S(1,J1))                ! values (connecting lines)
            DIAGI_S(J1)%USER_ARG(19,2) = LOC(FERR8_S(1,J1))                ! errors (connecting lines)
            DIAGI_S(J1)%USER_ARG(20,2) = LOC(FUNC_N)                       ! # of user functions
            DIAGI_S(J1)%USER_ARG(21,2) = LOC(FUNC_B)                       ! button names for user functions
            DIAGI_S(J1)%USER_ARG(22,2) = LOC(FUNC_K)                       ! user functions keys
            DIAGI_S(J1)%USER_ARG(23,2) = LOC(FUNC_NNN)                     ! all avail. button names for user functions (repa.i)
            DIAGI_S(J1)%USER_ARG(24,2) = LOC(FUNC_BUTT)                    ! all avail. button names for user functions (repa.i)
            DIAGI_S(J1)%USER_ARG(25,2) = LOC(IUER)                         ! error handler
!
            DIAGI_S(J1)%USER_ARG(0,3)  = 25                                ! # of arguments for REPCONN
            DIAGI_S(J1)%USER_ARG(1,3)  = LOC(CONDEL)                       ! flag for user input of source name for connecting lines
            DIAGI_S(J1)%USER_ARG(2,3)  = LOC(NUM_ALL(J1))                  ! dimension of baseline arrays
            DIAGI_S(J1)%USER_ARG(3,3)  = LOC(DIAGI_S(J1))                  ! DIAGI record
            DIAGI_S(J1)%USER_ARG(4,3)  = LOC(COL_S_IDX)                    ! colour index for connecting  points
            DIAGI_S(J1)%USER_ARG(5,3)  = LOC(INFO_CHR_G(1,J1))             ! information for bottom line (good)
            DIAGI_S(J1)%USER_ARG(6,3)  = LOC(INFO_CHR_M(1,J1))             ! information for bottom line (recoverable)
            DIAGI_S(J1)%USER_ARG(7,3)  = LOC(INFO_CHR_B(1,J1))             ! information for bottom line (bad)
            DIAGI_S(J1)%USER_ARG(8,3)  = LOC(TIME_G(1,J1))                 ! arguments (G)
            DIAGI_S(J1)%USER_ARG(9,3)  = LOC(FVAL8_G(1,J1))                ! values (G)
            DIAGI_S(J1)%USER_ARG(10,3) = LOC(FERR8_G(1,J1))                ! errors (G)
            DIAGI_S(J1)%USER_ARG(11,3) = LOC(TIME_M(1,J1))                 ! arguments (M)
            DIAGI_S(J1)%USER_ARG(12,3) = LOC(FVAL8_M(1,J1))                ! values (M)
            DIAGI_S(J1)%USER_ARG(13,3) = LOC(FERR8_M(1,J1))                ! errors (M)
            DIAGI_S(J1)%USER_ARG(14,3) = LOC(TIME_B(1,J1))                 ! arguments (B)
            DIAGI_S(J1)%USER_ARG(15,3) = LOC(FVAL8_B(1,J1))                ! values (B)
            DIAGI_S(J1)%USER_ARG(16,3) = LOC(FERR8_B(1,J1))                ! errors (B)
            DIAGI_S(J1)%USER_ARG(17,3) = LOC(TIME_S(1,J1))                 ! arguments (connecting lines)
            DIAGI_S(J1)%USER_ARG(18,3) = LOC(FVAL8_S(1,J1))                ! values (connecting lines)
            DIAGI_S(J1)%USER_ARG(19,3) = LOC(FERR8_S(1,J1))                ! errors (connecting lines)
            DIAGI_S(J1)%USER_ARG(20,3) = LOC(FUNC_N)                       ! # of user functions
            DIAGI_S(J1)%USER_ARG(21,3) = LOC(FUNC_B)                       ! button names for user functions
            DIAGI_S(J1)%USER_ARG(22,3) = LOC(FUNC_K)                       ! user functions keys
            DIAGI_S(J1)%USER_ARG(23,3) = LOC(FUNC_NNN)                     ! all avail. button names for user functions (repa.i)
            DIAGI_S(J1)%USER_ARG(24,3) = LOC(FUNC_BUTT)                    ! all avail. button names for user functions (repa.i)
            DIAGI_S(J1)%USER_ARG(25,3) = LOC(IUER)                         ! error handler
            DIAGI_S(J1)%INIT_USER_FUNC = 3                                 ! initialization function
!
            DIAGI_S(J1)%USER_ARG(0,4)  = 25                                ! # of arguments for REPCONN
            DIAGI_S(J1)%USER_ARG(1,4)  = LOC(CONINP)                       ! flag for deleting of connecting lines
            DIAGI_S(J1)%USER_ARG(2,4)  = LOC(NUM_ALL(J1))                  ! dimension of baseline arrays
            DIAGI_S(J1)%USER_ARG(3,4)  = LOC(DIAGI_S(J1))                  ! DIAGI record
            DIAGI_S(J1)%USER_ARG(4,4)  = LOC(COL_S_IDX)                    ! colour index for connecting  points
            DIAGI_S(J1)%USER_ARG(5,4)  = LOC(INFO_CHR_G(1,J1))             ! information for bottom line (good)
            DIAGI_S(J1)%USER_ARG(6,4)  = LOC(INFO_CHR_M(1,J1))             ! information for bottom line (recoverable)
            DIAGI_S(J1)%USER_ARG(7,4)  = LOC(INFO_CHR_B(1,J1))             ! information for bottom line (bad)
            DIAGI_S(J1)%USER_ARG(8,4)  = LOC(TIME_G(1,J1))                 ! arguments (G)
            DIAGI_S(J1)%USER_ARG(9,4)  = LOC(FVAL8_G(1,J1))                ! values (G)
            DIAGI_S(J1)%USER_ARG(10,4) = LOC(FERR8_G(1,J1))                ! errors (G)
            DIAGI_S(J1)%USER_ARG(11,4) = LOC(TIME_M(1,J1))                 ! arguments (M)
            DIAGI_S(J1)%USER_ARG(12,4) = LOC(FVAL8_M(1,J1))                ! values (M)
            DIAGI_S(J1)%USER_ARG(13,4) = LOC(FERR8_M(1,J1))                ! errors (M)
            DIAGI_S(J1)%USER_ARG(14,4) = LOC(TIME_B(1,J1))                 ! arguments (B)
            DIAGI_S(J1)%USER_ARG(15,4) = LOC(FVAL8_B(1,J1))                ! values (B)
            DIAGI_S(J1)%USER_ARG(16,4) = LOC(FERR8_B(1,J1))                ! errors (B)
            DIAGI_S(J1)%USER_ARG(17,4) = LOC(TIME_S(1,J1))                 ! arguments (connecting lines)
            DIAGI_S(J1)%USER_ARG(18,4) = LOC(FVAL8_S(1,J1))                ! values (connecting lines)
            DIAGI_S(J1)%USER_ARG(19,4) = LOC(FERR8_S(1,J1))                ! errors (connecting lines)
            DIAGI_S(J1)%USER_ARG(20,4) = LOC(FUNC_N)                       ! # of user functions
            DIAGI_S(J1)%USER_ARG(21,4) = LOC(FUNC_B)                       ! button names for user functions
            DIAGI_S(J1)%USER_ARG(22,4) = LOC(FUNC_K)                       ! user functions keys
            DIAGI_S(J1)%USER_ARG(23,4) = LOC(FUNC_NNN)                     ! all avail. button names for user functions (repa.i)
            DIAGI_S(J1)%USER_ARG(24,4) = LOC(FUNC_BUTT)                    ! all avail. button names for user functions (repa.i)
            DIAGI_S(J1)%USER_ARG(25,4) = LOC(IUER)                         ! error handler
!
            DIAGI_S(J1)%USER_ARG(0,5)  = 5                                 ! # of arguments for REPBASL
            DIAGI_S(J1)%USER_ARG(1,5)  = LOC(REPA_PAR)
            DIAGI_S(J1)%USER_ARG(2,5)  = LOC(DIAGI_S(J1))                  ! DIAGI record
            DIAGI_S(J1)%USER_ARG(3,5)  = LOC(REPA_FLAG)
            DIAGI_S(J1)%USER_ARG(4,5)  = LOC(REPA_PREV_VAR)
            DIAGI_S(J1)%USER_ARG(5,5)  = LOC(DIAGI_S(J1)%MD_OUT)
!
            DIAGI_S(J1)%USER_ARG(0,6)  = 5                                 ! # of arguments for REPBASL
            DIAGI_S(J1)%USER_ARG(1,6)  = LOC(REPA_PAR)
            DIAGI_S(J1)%USER_ARG(2,6)  = LOC(DIAGI_S(J1))                  ! DIAGI record
            DIAGI_S(J1)%USER_ARG(3,6)  = LOC(REPA_FLAG)
            DIAGI_S(J1)%USER_ARG(4,6)  = LOC(REPA_NEXT_VAR)
            DIAGI_S(J1)%USER_ARG(5,6)  = LOC(DIAGI_S(J1)%MD_OUT)
!
            DIAGI_S(J1)%USER_ARG(0,7)  = 1                                 ! # of arguments for REPGOOD
            DIAGI_S(J1)%USER_ARG(1,7)  = LOC(DIAGI_S(J1))                  ! DIAGI record
!
            DIAGI_S(J1)%USER_CHR(1)    = FUNC_KEY(5)                       ! info key
            DIAGI_S(J1)%USER_CHR(2)    = FUNC_KEY(9)                       ! connect observations with the same source
            DIAGI_S(J1)%USER_CHR(3)    = 'A'                               ! disconnect observations with the same source
            DIAGI_S(J1)%USER_CHR(4)    = FUNC_KEY(11)                      ! user input of source name to connect points
            DIAGI_S(J1)%USER_CHR(5)    = CHAR(PAGE_UPDN(1))                ! decimal code for PgUp keybord key
            DIAGI_S(J1)%USER_CHR(6)    = CHAR(PAGE_UPDN(2))                ! decimal code for PgDn keybord key
            DIAGI_S(J1)%USER_CHR(7)    = CHAR(13)                          ! decimal code for PgDn keybord key
!
! --------- bottom message
!
            DIAGI_S(J1)%MESS_BOT =  'left mouse click to initialize --> then choose user function in the headline'
         END DO
      END IF
!
! --- Properties 'PHADEL', 'SNELE1', 'SNELE2'
!
      IF ( PROP .EQ. 'PHADEL' .OR. PROP .EQ. 'SNELE1' .OR. PROP .EQ. 'SNELE2') THEN
!
         FUNC_N = 3                                                        ! # of user functions
         FUNC_B(1) = FUNC_BUTT(5)                                          ! button name - info user function
         FUNC_B(2) = FUNC_BUTT(9)                                          ! button name - connect user function
         FUNC_B(3) = FUNC_BUTT(10)                                         ! button name - initialize
!
         FUNC_K(1) = FUNC_KEY(5)                                           ! key - info user function
         FUNC_K(2) = FUNC_KEY(9)                                           ! key - connect user function
         FUNC_K(3) = FUNC_KEY(10)                                          ! key - initialize
!
         COL_S_IDX = 7                                                     ! colour index for connecting  lines
!
         DO J1=1,BASL_L-BASL_F+1                                           ! start loop over baseline(plot) indices
!
            DIAGI_S(J1)%ARG_UNITS = 'microsec'                             ! units
!
            DIAGI_S(J1)%NUSER_FUNC = 7                                     ! # of DiaGi user functions
!
            DIAGI_S(J1)%USER_FUNC(1)   = LOC_EXT(REPINFO)                      ! address of the entry point REPINFO
            DIAGI_S(J1)%USER_FUNC(2)   = LOC_EXT(REPCONN)                      ! address of the entry point REPCONN
            DIAGI_S(J1)%USER_FUNC(3)   = LOC_EXT(REPCONN)                      ! address of the entry point REPCONN
            DIAGI_S(J1)%USER_FUNC(4)   = LOC_EXT(REPCONN)                      ! address of the entry point REPCONN
            DIAGI_S(J1)%USER_FUNC(5)   = LOC_EXT(REPBASL)                      ! address of the entry point REPBASL
            DIAGI_S(J1)%USER_FUNC(6)   = LOC_EXT(REPBASL)                      ! address of the entry point REPBASL
            DIAGI_S(J1)%USER_FUNC(7)   = LOC_EXT(REPGOOD)                      ! address of the entry point REPGOOD
!
            DIAGI_S(J1)%USER_ARG(0,1)  = 10                                ! # of arguments for REPINFO
            DIAGI_S(J1)%USER_ARG(1,1)  = LOC(DIAGI_S(J1))                  ! DIAGI record
            DIAGI_S(J1)%USER_ARG(2,1)  = LOC(IBOBSM)                       ! total # of parameters (G+M+B)
            DIAGI_S(J1)%USER_ARG(3,1)  = LOC(INFO_CHR_G(1,J1))             ! information for bottom line (good)
            DIAGI_S(J1)%USER_ARG(4,1)  = LOC(INFO_CHR_M(1,J1))             ! information for bottom line (recoverable)
            DIAGI_S(J1)%USER_ARG(5,1)  = LOC(INFO_CHR_B(1,J1))             ! information for bottom line (bad)
            DIAGI_S(J1)%USER_ARG(6,1)  = LOC(GMB(1,J1))                    ! flags for good(G), recoverable(M) or bad(B) observation
            DIAGI_S(J1)%USER_ARG(7,1)  = LOC(FUNC_N)                       ! # of user functions
            DIAGI_S(J1)%USER_ARG(8,1)  = LOC(FUNC_B)                       ! button names for user functions
            DIAGI_S(J1)%USER_ARG(9,1)  = LOC(FUNC_K)                       ! user functions keys
            DIAGI_S(J1)%USER_ARG(10,1) = LOC(IUER)                         ! error handler
!
            DIAGI_S(J1)%USER_ARG(0,2)  = 25                                ! # of arguments for REPCONN
            DIAGI_S(J1)%USER_ARG(1,2)  = LOC(CONCON)                       ! flag for connecting lines
            DIAGI_S(J1)%USER_ARG(2,2)  = LOC(NUM_ALL(J1))                  ! dimension of baseline arrays
            DIAGI_S(J1)%USER_ARG(3,2)  = LOC(DIAGI_S(J1))                  ! DIAGI record
            DIAGI_S(J1)%USER_ARG(4,2)  = LOC(COL_S_IDX)                    ! colour index for connecting  points
            DIAGI_S(J1)%USER_ARG(5,2)  = LOC(INFO_CHR_G(1,J1))             ! information for bottom line (good)
            DIAGI_S(J1)%USER_ARG(6,2)  = LOC(INFO_CHR_M(1,J1))             ! information for bottom line (recoverable)
            DIAGI_S(J1)%USER_ARG(7,2)  = LOC(INFO_CHR_B(1,J1))             ! information for bottom line (bad)
            DIAGI_S(J1)%USER_ARG(8,2)  = LOC(TIME_G(1,J1))                 ! arguments (G)
            DIAGI_S(J1)%USER_ARG(9,2)  = LOC(FVAL8_G(1,J1))                ! values (G)
            DIAGI_S(J1)%USER_ARG(10,2) = LOC(FERR8_G(1,J1))                ! errors (G)
            DIAGI_S(J1)%USER_ARG(11,2) = LOC(TIME_M(1,J1))                 ! arguments (M)
            DIAGI_S(J1)%USER_ARG(12,2) = LOC(FVAL8_M(1,J1))                ! values (M)
            DIAGI_S(J1)%USER_ARG(13,2) = LOC(FERR8_M(1,J1))                ! errors (M)
            DIAGI_S(J1)%USER_ARG(14,2) = LOC(TIME_B(1,J1))                 ! arguments (B)
            DIAGI_S(J1)%USER_ARG(15,2) = LOC(FVAL8_B(1,J1))                ! values (B)
            DIAGI_S(J1)%USER_ARG(16,2) = LOC(FERR8_B(1,J1))                ! errors (B)
            DIAGI_S(J1)%USER_ARG(17,2) = LOC(TIME_S(1,J1))                 ! arguments (connecting lines)
            DIAGI_S(J1)%USER_ARG(18,2) = LOC(FVAL8_S(1,J1))                ! values (connecting lines)
            DIAGI_S(J1)%USER_ARG(19,2) = LOC(FERR8_S(1,J1))                ! errors (connecting lines)
            DIAGI_S(J1)%USER_ARG(20,2) = LOC(FUNC_N)                       ! # of user functions
            DIAGI_S(J1)%USER_ARG(21,2) = LOC(FUNC_B)                       ! button names for user functions
            DIAGI_S(J1)%USER_ARG(22,2) = LOC(FUNC_K)                       ! user functions keys
            DIAGI_S(J1)%USER_ARG(23,2) = LOC(FUNC_NNN)                     ! all avail. button names for user functions (repa.i)
            DIAGI_S(J1)%USER_ARG(24,2) = LOC(FUNC_BUTT)                    ! all avail. button names for user functions (repa.i)
            DIAGI_S(J1)%USER_ARG(25,2) = LOC(IUER)                         ! error handler
!
            DIAGI_S(J1)%USER_ARG(0,3)  = 25                                ! # of arguments for REPCONN
            DIAGI_S(J1)%USER_ARG(1,3)  = LOC(CONDEL)                       ! flag for user input of source name for connecting lines
            DIAGI_S(J1)%USER_ARG(2,3)  = LOC(NUM_ALL(J1))                  ! dimension of baseline arrays
            DIAGI_S(J1)%USER_ARG(3,3)  = LOC(DIAGI_S(J1))                  ! DIAGI record
            DIAGI_S(J1)%USER_ARG(4,3)  = LOC(COL_S_IDX)                    ! colour index for connecting  points
            DIAGI_S(J1)%USER_ARG(5,3)  = LOC(INFO_CHR_G(1,J1))             ! information for bottom line (good)
            DIAGI_S(J1)%USER_ARG(6,3)  = LOC(INFO_CHR_M(1,J1))             ! information for bottom line (recoverable)
            DIAGI_S(J1)%USER_ARG(7,3)  = LOC(INFO_CHR_B(1,J1))             ! information for bottom line (bad)
            DIAGI_S(J1)%USER_ARG(8,3)  = LOC(TIME_G(1,J1))                 ! arguments (G)
            DIAGI_S(J1)%USER_ARG(9,3)  = LOC(FVAL8_G(1,J1))                ! values (G)
            DIAGI_S(J1)%USER_ARG(10,3) = LOC(FERR8_G(1,J1))                ! errors (G)
            DIAGI_S(J1)%USER_ARG(11,3) = LOC(TIME_M(1,J1))                 ! arguments (M)
            DIAGI_S(J1)%USER_ARG(12,3) = LOC(FVAL8_M(1,J1))                ! values (M)
            DIAGI_S(J1)%USER_ARG(13,3) = LOC(FERR8_M(1,J1))                ! errors (M)
            DIAGI_S(J1)%USER_ARG(14,3) = LOC(TIME_B(1,J1))                 ! arguments (B)
            DIAGI_S(J1)%USER_ARG(15,3) = LOC(FVAL8_B(1,J1))                ! values (B)
            DIAGI_S(J1)%USER_ARG(16,3) = LOC(FERR8_B(1,J1))                ! errors (B)
            DIAGI_S(J1)%USER_ARG(17,3) = LOC(TIME_S(1,J1))                 ! arguments (connecting lines)
            DIAGI_S(J1)%USER_ARG(18,3) = LOC(FVAL8_S(1,J1))                ! values (connecting lines)
            DIAGI_S(J1)%USER_ARG(19,3) = LOC(FERR8_S(1,J1))                ! errors (connecting lines)
            DIAGI_S(J1)%USER_ARG(20,3) = LOC(FUNC_N)                       ! # of user functions
            DIAGI_S(J1)%USER_ARG(21,3) = LOC(FUNC_B)                       ! button names for user functions
            DIAGI_S(J1)%USER_ARG(22,3) = LOC(FUNC_K)                       ! user functions keys
            DIAGI_S(J1)%USER_ARG(23,3) = LOC(FUNC_NNN)                     ! all avail. button names for user functions (repa.i)
            DIAGI_S(J1)%USER_ARG(24,3) = LOC(FUNC_BUTT)                    ! all avail. button names for user functions (repa.i)
            DIAGI_S(J1)%USER_ARG(25,3) = LOC(IUER)                         ! error handler
            DIAGI_S(J1)%INIT_USER_FUNC = 3                                 ! initialize user function
!
            DIAGI_S(J1)%USER_ARG(0,4)  = 25                                ! # of arguments for REPCONN
            DIAGI_S(J1)%USER_ARG(1,4)  = LOC(CONINP)                       ! flag for deleting of connecting lines
            DIAGI_S(J1)%USER_ARG(2,4)  = LOC(NUM_ALL(J1))                  ! dimension of baseline arrays
            DIAGI_S(J1)%USER_ARG(3,4)  = LOC(DIAGI_S(J1))                  ! DIAGI record
            DIAGI_S(J1)%USER_ARG(4,4)  = LOC(COL_S_IDX)                    ! colour index for connecting  points
            DIAGI_S(J1)%USER_ARG(5,4)  = LOC(INFO_CHR_G(1,J1))             ! information for bottom line (good)
            DIAGI_S(J1)%USER_ARG(6,4)  = LOC(INFO_CHR_M(1,J1))             ! information for bottom line (recoverable)
            DIAGI_S(J1)%USER_ARG(7,4)  = LOC(INFO_CHR_B(1,J1))             ! information for bottom line (bad)
            DIAGI_S(J1)%USER_ARG(8,4)  = LOC(TIME_G(1,J1))                 ! arguments (G)
            DIAGI_S(J1)%USER_ARG(9,4)  = LOC(FVAL8_G(1,J1))                ! values (G)
            DIAGI_S(J1)%USER_ARG(10,4) = LOC(FERR8_G(1,J1))                ! errors (G)
            DIAGI_S(J1)%USER_ARG(11,4) = LOC(TIME_M(1,J1))                 ! arguments (M)
            DIAGI_S(J1)%USER_ARG(12,4) = LOC(FVAL8_M(1,J1))                ! values (M)
            DIAGI_S(J1)%USER_ARG(13,4) = LOC(FERR8_M(1,J1))                ! errors (M)
            DIAGI_S(J1)%USER_ARG(14,4) = LOC(TIME_B(1,J1))                 ! arguments (B)
            DIAGI_S(J1)%USER_ARG(15,4) = LOC(FVAL8_B(1,J1))                ! values (B)
            DIAGI_S(J1)%USER_ARG(16,4) = LOC(FERR8_B(1,J1))                ! errors (B)
            DIAGI_S(J1)%USER_ARG(17,4) = LOC(TIME_S(1,J1))                 ! arguments (connecting lines)
            DIAGI_S(J1)%USER_ARG(18,4) = LOC(FVAL8_S(1,J1))                ! values (connecting lines)
            DIAGI_S(J1)%USER_ARG(19,4) = LOC(FERR8_S(1,J1))                ! errors (connecting lines)
            DIAGI_S(J1)%USER_ARG(20,4) = LOC(FUNC_N)                       ! # of user functions
            DIAGI_S(J1)%USER_ARG(21,4) = LOC(FUNC_B)                       ! button names for user functions
            DIAGI_S(J1)%USER_ARG(22,4) = LOC(FUNC_K)                       ! user functions keys
            DIAGI_S(J1)%USER_ARG(23,4) = LOC(FUNC_NNN)                     ! all avail. button names for user functions (repa.i)
            DIAGI_S(J1)%USER_ARG(24,4) = LOC(FUNC_BUTT)                    ! all avail. button names for user functions (repa.i)
            DIAGI_S(J1)%USER_ARG(25,4) = LOC(IUER)                         ! error handler
!
            DIAGI_S(J1)%USER_ARG(0,5)  = 5                                 ! # of arguments for REPBASL
            DIAGI_S(J1)%USER_ARG(1,5)  = LOC(REPA_PAR)
            DIAGI_S(J1)%USER_ARG(2,5)  = LOC(DIAGI_S(J1))                  ! DIAGI record
            DIAGI_S(J1)%USER_ARG(3,5)  = LOC(REPA_FLAG)
            DIAGI_S(J1)%USER_ARG(4,5)  = LOC(REPA_PREV_VAR)
            DIAGI_S(J1)%USER_ARG(5,5)  = LOC(DIAGI_S(J1)%MD_OUT)
!
            DIAGI_S(J1)%USER_ARG(0,6)  = 5                                 ! # of arguments for REPBASL
            DIAGI_S(J1)%USER_ARG(1,6)  = LOC(REPA_PAR)
            DIAGI_S(J1)%USER_ARG(2,6)  = LOC(DIAGI_S(J1))                  ! DIAGI record
            DIAGI_S(J1)%USER_ARG(3,6)  = LOC(REPA_FLAG)
            DIAGI_S(J1)%USER_ARG(4,6)  = LOC(REPA_NEXT_VAR)
            DIAGI_S(J1)%USER_ARG(5,6)  = LOC(DIAGI_S(J1)%MD_OUT)
!
            DIAGI_S(J1)%USER_ARG(0,7)  = 1                                 ! # of arguments for REPGOOD
            DIAGI_S(J1)%USER_ARG(1,7)  = LOC(DIAGI_S(J1))
!
            DIAGI_S(J1)%USER_CHR(1)    = FUNC_KEY(5)         ! info key
            DIAGI_S(J1)%USER_CHR(2)    = FUNC_KEY(9)         ! connect observations with the same source
            DIAGI_S(J1)%USER_CHR(3)    = 'A'                 ! disconnect observations with the same source
            DIAGI_S(J1)%USER_CHR(4)    = FUNC_KEY(11)        ! user input of source name to connect points
            DIAGI_S(J1)%USER_CHR(5)    = CHAR(PAGE_UPDN(1))  ! decimal code for PgUp keybord key
            DIAGI_S(J1)%USER_CHR(6)    = CHAR(PAGE_UPDN(2))  ! decimal code for PgDn keybord key
            DIAGI_S(J1)%USER_CHR(7)    = CHAR(13)            !
!
! --------- bottom message
!
            DIAGI_S(J1)%MESS_BOT =  'left mouse click to initialize --> then choose user function in the headline'
!
            DIAGI_S(J1)%XMIN = -0.1                                        ! initialize plot coordinates
            IF ( NUM_ALL(J1) .GT. 0 ) THEN
                 DIAGI_S(J1)%XMAX = TIME(NUM_ALL(J1),J1) + 0.1
              ELSE
                 DIAGI_S(J1)%XMAX = 0.0
            END IF
!
            DIAGI_S(J1)%YMIN = 1.0                                         ! initialize plot coordinates
            DIAGI_S(J1)%YMAX = 0.0                                         ! initialize plot coordinates
!
! --------- zero line
!
            IF ( PROP .EQ. 'PHADEL' ) THEN
               ZERO_TIME(1,J1) = DIAGI_S(J1)%XMIN - 0.1D0                  ! time values for zero line
               IF ( NUM_ALL(J1) .GT. 0 ) THEN
                  ZERO_TIME(2,J1) = DIAGI_S(J1)%XMAX + 0.1D0
               ELSE
                  ZERO_TIME(2,J1) = 0.0D0
               END IF
            END IF
!
            IF ( IBATCH .EQ. 0 ) THEN
               DIAGI_S(J1)%IDEV      = IDEV
            ELSE
               DIAGI_S(J1)%IDEV      = 7
            END IF
!
            N_COL = 3                                                      ! # of observation colours
!
            DIAGI_S(J1)%STATUS = DIA__DEF                                  ! DiaGi status
!
            DIAGI_S( J1 )%ICLR = 1                                         ! current colour ( main function)
            DIAGI_S( J1 )%NCLR = 7                                         ! number of the functions to be plotted
            DIAGI_S( J1 )%ITRM = 0                                         ! code action for termination
            DIAGI_S( J1 )%IBATCH = 0                                       ! code of batch mode. (0 for interactive)
!
            DIAGI_S( J1 )%LER(1) = .FALSE.                                 ! flag array for using function 1 errors
            DIAGI_S( J1 )%LER(2) = .FALSE.                                 ! flag array for using function 2 errors
            DIAGI_S( J1 )%LER(3) = .FALSE.                                 ! flag array for using function 3 errors
            DIAGI_S( J1 )%LER(4) = .FALSE.                                 ! flag array for using function 4 errors
            DIAGI_S( J1 )%LER(7) = .FALSE.                                 ! flag array for using function 7 errors
!
            DIAGI_S( J1 )%IBST(1) = 0                                      ! error bar style for using function 1 errors
            DIAGI_S( J1 )%IBST(2) = 0                                      ! error bar style for using function 2 errors
            DIAGI_S( J1 )%IBST(3) = 0                                      ! error bar style for using function 3 errors
            DIAGI_S( J1 )%IBST(4) = 0                                      ! error bar style for using function 4 errors
            DIAGI_S( J1 )%IBST(7) = 0                                      ! error bar style for using function 7 errors
!
            DIAGI_S(J1)%IOST(1) = 1                                        ! overplot style on
            DIAGI_S(J1)%IOST(2) = 1                                        ! overplot style on
            DIAGI_S(J1)%IOST(3) = 1                                        ! overplot style on
            DIAGI_S(J1)%IOST(4) = 0                                        ! overplot style on
            DIAGI_S(J1)%IOST(7) = 1                                        ! overplot style on
!
            DIAGI_S(J1)%IWST(1)= 1                                         ! line width
            DIAGI_S(J1)%IWST(2)= 1                                         ! line width
            DIAGI_S(J1)%IWST(3)= 1                                         ! line width
            DIAGI_S(J1)%IWST(4)= 1                                         ! line width
            DIAGI_S(J1)%IWST(7)= 1                                         ! line width
!
            DIAGI_S(J1)%ICOL(1) = COL_ATR(1)                               ! colour attribute for good obs.(G)
            DIAGI_S(J1)%ICOL(2) = COL_ATR(2)                               ! colour attribute for recoverable obs.(M)
            DIAGI_S(J1)%ICOL(3) = COL_ATR(3)                               ! colour attribute for bad obs.(B)
            DIAGI_S(J1)%ICOL(4) = COL_ATR(4)                               ! colour attribute for zero line
            DIAGI_S(J1)%ICOL(7) = COL_ATR(7)                               ! colour attribute for special case
!
            DIAGI_S(J1)%ILST(1) = 1                                        ! line style for good obs.
            DIAGI_S(J1)%ILST(2) = 1                                        ! line style for recoverable obs.
            DIAGI_S(J1)%ILST(3) = 1                                        ! line style for bad obs.
            DIAGI_S(J1)%ILST(4) = 2                                        ! line style for zero line
            DIAGI_S(J1)%ILST(7) = 2                                        ! line style for special case
!
            DIAGI_S(J1)%IPST(1) = PNT_STY(1)                               ! point style for good obs.
            DIAGI_S(J1)%IPST(2) = PNT_STY(2)                               ! point style for recoverable obs
            DIAGI_S(J1)%IPST(3) = PNT_STY(3)                               ! point style for bad obs.
            DIAGI_S(J1)%IPST(4) = PNT_STY(4)                               ! point style for zero line
            DIAGI_S(J1)%IPST(7) = PNT_STY(7)                               ! point style for special case
!
            DIAGI_S(J1)%NPOI(1) =  NUM_G(J1)                               ! # of points for good obs.
            DIAGI_S(J1)%NPOI(2) =  NUM_M(J1)                               ! # of points for mandown obs.
            IF ( SHOW_BAD ) THEN                                           ! # of points for bad obs.
               DIAGI_S(J1)%NPOI(3) =  NUM_B(J1)
            ELSE
               DIAGI_S(J1)%NPOI(3) =  0
            END IF
            IF ( PROP .EQ. 'PHADEL' ) THEN
               DIAGI_S(J1)%NPOI(4) =  2
            ELSE
               DIAGI_S(J1)%NPOI(4) =  0
            END IF
            DIAGI_S(J1)%NPOI(5) =  0
            DIAGI_S(J1)%NPOI(6) =  0
            DIAGI_S(J1)%NPOI(7) =  0
!
            DIAGI_S(J1)%ADR_X8(1) = LOC( TIME_G(1,J1))                    ! address of the 1st element of TIME_G(.)
            DIAGI_S(J1)%ADR_X8(2) = LOC( TIME_M(1,J1))                    ! address of the 1st element of TIME_M(.)
            DIAGI_S(J1)%ADR_X8(3) = LOC( TIME_B(1,J1))                    ! address of the 1st element of TIME_B(.)
            DIAGI_S(J1)%ADR_X8(4) = LOC( ZERO_TIME(1,J1))                 ! address of the 1st element of ZERO_TIME(.)
!
            DIAGI_S(J1)%ADR_Y8(1) = LOC( FVAL8_G(1,J1))                   ! address of the 1st element of FVAL8_G(.,J1)
            DIAGI_S(J1)%ADR_Y8(2) = LOC( FVAL8_M(1,J1))                   ! address of the 1st element of FVAL8_M(.,J1)
            DIAGI_S(J1)%ADR_Y8(3) = LOC( FVAL8_B(1,J1))                   ! address of the 1st element of FVAL8_B(.,J1)
            DIAGI_S(J1)%ADR_Y8(4) = LOC( ZERO_LINE(1))                    ! address of the 1st element of ZERO_LINE(.)
!
            DIAGI_S(J1)%ADR_E8( 1 ) = LOC( FERR8_G(1,J1))               ! address of the 1st element of FERR8_G(.,J1)
            DIAGI_S(J1)%ADR_E8( 2 ) = LOC( FERR8_M(1,J1))               ! address of the 1st element of FERR8_M(.,J1)
            DIAGI_S(J1)%ADR_E8( 3 ) = LOC( FERR8_B(1,J1))               ! address of the 1st element of FERR8_B(.,J1)
!
! --------- titles of small plots in MultiDiaGi
!
! --------- normal station order
!
            TITS(J1) = ISITN_CHR(IBAS(J1+BASL_F-1,1))(1:ILEN(ISITN_CHR(IBAS(J1+BASL_F-1,1))))//' '// &
     &                 ISITN_CHR(IBAS(J1+BASL_F-1,2))(1:ILEN(ISITN_CHR(IBAS(J1+BASL_F-1,2))))
!
! --------- statistic information for titles of big DiaGi plots
!
            CALL CLRCH ( NUM_ALL_CH )
            WRITE ( NUM_ALL_CH, '(i3)' ) NUM_G(J1) + NUM_M(J1) + NUM_B(J1)         ! total # of obs. --> character value
            CALL CHASHL ( NUM_ALL_CH )
!
! --------- titles of big DiaGi plots
!
            CALL CLRCH ( CH1_TMP )
            CALL CLRCH ( CH2_TMP )
            CALL CLRCH ( CH3_TMP )
            CALL CLRCH ( CH4_TMP )
!
            IF ( PROP .EQ. 'PHADEL' ) THEN
               CH1_TMP = '    Phase Delays    #'
               CH2_TMP = NUM_ALL_CH(1:ILEN(NUM_ALL_CH))
! ------------ normal station order
               IF ( IBAS(J1+BASL_F-1,4) .EQ. 0 ) THEN
                  CH3_TMP = ISITN_CHR(IBAS(J1+BASL_F-1,1))(1:ILEN(ISITN_CHR(IBAS(J1+BASL_F-1,1))))
                  CH4_TMP = ISITN_CHR(IBAS(J1+BASL_F-1,2))(1:ILEN(ISITN_CHR(IBAS(J1+BASL_F-1,2))))
! ------------ reverse station order
               ELSE
                  CH3_TMP = ISITN_CHR(IBAS(J1+BASL_F-1,2))(1:ILEN(ISITN_CHR(IBAS(J1+BASL_F-1,2))))
                  CH4_TMP = ISITN_CHR(IBAS(J1+BASL_F-1,1))(1:ILEN(ISITN_CHR(IBAS(J1+BASL_F-1,1))))
               END IF
            ELSE
               CH1_TMP = '    SNR vs. ELEVATION    #'
               CH2_TMP = NUM_ALL_CH(1:ILEN(NUM_ALL_CH))
!
               IF ( PROP .EQ. 'SNELE1' ) THEN
! --------------- normal station order
                  IF ( IBAS(J1+BASL_F-1,4) .EQ. 0 ) THEN
                     CH3_TMP = '<'//ISITN_CHR(IBAS(J1+BASL_F-1,1))(1:ILEN(ISITN_CHR(IBAS(J1+BASL_F-1,1))))//'>'
                     CH4_TMP = ISITN_CHR(IBAS(J1+BASL_F-1,2))(1:ILEN(ISITN_CHR(IBAS(J1+BASL_F-1,2))))
! --------------- reverse station order
                  ELSE
                     CH3_TMP = '<'//ISITN_CHR(IBAS(J1+BASL_F-1,2))(1:ILEN(ISITN_CHR(IBAS(J1+BASL_F-1,2))))//'>'
                     CH4_TMP = ISITN_CHR(IBAS(J1+BASL_F-1,1))(1:ILEN(ISITN_CHR(IBAS(J1+BASL_F-1,1))))
                  END IF
               ELSE IF ( PROP .EQ. 'SNELE2' ) THEN
! --------------- normal station order
                  IF ( IBAS(J1+BASL_F-1,4) .EQ. 0 ) THEN
                     CH3_TMP = ISITN_CHR(IBAS(J1+BASL_F-1,1))(1:ILEN(ISITN_CHR(IBAS(J1+BASL_F-1,1))))
                     CH4_TMP = '<'//ISITN_CHR(IBAS(J1+BASL_F-1,2))(1:ILEN(ISITN_CHR(IBAS(J1+BASL_F-1,2))))//'>'
! --------------- reverse station order
                  ELSE
                     CH3_TMP = ISITN_CHR(IBAS(J1+BASL_F-1,2))(1:ILEN(ISITN_CHR(IBAS(J1+BASL_F-1,2))))
                     CH4_TMP = '<'//ISITN_CHR(IBAS(J1+BASL_F-1,1))(1:ILEN(ISITN_CHR(IBAS(J1+BASL_F-1,1))))//'>'
                  END IF
               END IF
            END IF
!
           DIAGI_S(J1)%ZAG =  REPA_E(EXP_INDEX)%BAND_NAME(BAND_INDEX)(1:9)// &
     &                        REPA_E(EXP_INDEX)%BAND_NAME(BAND_INDEX)(12:13)// &
     &                        CH1_TMP(1:ILEN(CH1_TMP))// &
     &                        CH2_TMP(1:ILEN(CH2_TMP))//' '// &
     &                        CH3_TMP(1:ILEN(CH3_TMP))//' '// &
     &                        CH4_TMP(1:ILEN(CH4_TMP))
         END DO
      ENDIF
!
! --- property 'GRIONO' (note! more definitions above)
!
      IF ( PROP .EQ. 'GRIONO' ) THEN
         FUNC_N = 1                                                        ! # of user functions
         FUNC_B(1) = FUNC_BUTT(10)                                         ! button name - initialize
         FUNC_K(1) = FUNC_KEY(10)                                          ! key - initialize
!
         COL_S_IDX = 7
!
         DO J1=1,BASL_L-BASL_F+1                                           ! start loop over baseline(plot) indices
!
            DIAGI_S(J1)%NUSER_FUNC = 4                                     ! # of DiaGi user functions
!
            DIAGI_S(J1)%USER_FUNC(1)   = LOC_EXT(REPCONN)                      ! address of the entry point REPCONN (only for help function!!)
            DIAGI_S(J1)%USER_FUNC(2)   = LOC_EXT(REPBASL)                      ! address of the entry point REPBASL
            DIAGI_S(J1)%USER_FUNC(3)   = LOC_EXT(REPBASL)                      ! address of the entry point REPBASL
            DIAGI_S(J1)%USER_FUNC(4)   = LOC_EXT(REPGOOD)                      ! address of the entry point REPGOOD
!
            DIAGI_S(J1)%USER_ARG(0,1)  = 25                                ! # of arguments for REPCONN
            DIAGI_S(J1)%USER_ARG(1,1)  = LOC(CONINI)                       ! flag for
!C          DIAGI_S(J1).USER_ARG(1,1)  = LOC(CONDEL)                       ! flag for deleting of connecting lines and initialization
            DIAGI_S(J1)%USER_ARG(2,1)  = LOC(NUM_ALL(J1))                  ! dimension of baseline arrays
            DIAGI_S(J1)%USER_ARG(3,1)  = LOC(DIAGI_S(J1))                  ! DIAGI record
            DIAGI_S(J1)%USER_ARG(4,1)  = LOC(COL_S_IDX)                    ! colour index for connecting  points
            DIAGI_S(J1)%USER_ARG(5,1)  = LOC(INFO_CHR_G(1,J1))             ! information for bottom line (good)
            DIAGI_S(J1)%USER_ARG(6,1)  = LOC(INFO_CHR_M(1,J1))             ! information for bottom line (recoverable)
            DIAGI_S(J1)%USER_ARG(7,1)  = LOC(INFO_CHR_B(1,J1))             ! information for bottom line (bad)
            DIAGI_S(J1)%USER_ARG(8,1)  = LOC(TIME_G(1,J1))                 ! arguments (G)
            DIAGI_S(J1)%USER_ARG(9,1)  = LOC(FVAL8_G(1,J1))                ! values (G)
            DIAGI_S(J1)%USER_ARG(10,1) = LOC(FERR8_G(1,J1))                ! errors (G)
            DIAGI_S(J1)%USER_ARG(11,1) = LOC(TIME_M(1,J1))                 ! arguments (M)
            DIAGI_S(J1)%USER_ARG(12,1) = LOC(FVAL8_M(1,J1))                ! values (M)
            DIAGI_S(J1)%USER_ARG(13,1) = LOC(FERR8_M(1,J1))                ! errors (M)
            DIAGI_S(J1)%USER_ARG(14,1) = LOC(TIME_B(1,J1))                 ! arguments (B)
            DIAGI_S(J1)%USER_ARG(15,1) = LOC(FVAL8_B(1,J1))                ! values (B)
            DIAGI_S(J1)%USER_ARG(16,1) = LOC(FERR8_B(1,J1))                ! errors (B)
            DIAGI_S(J1)%USER_ARG(17,1) = LOC(TIME_S(1,J1))                 ! arguments (connecting lines)
            DIAGI_S(J1)%USER_ARG(18,1) = LOC(FVAL8_S(1,J1))                ! values (connecting lines)
            DIAGI_S(J1)%USER_ARG(19,1) = LOC(FERR8_S(1,J1))                ! errors (connecting lines)
            DIAGI_S(J1)%USER_ARG(20,1) = LOC(FUNC_N)                       ! # of user functions
            DIAGI_S(J1)%USER_ARG(21,1) = LOC(FUNC_B)                       ! button names for user functions
            DIAGI_S(J1)%USER_ARG(22,1) = LOC(FUNC_K)                       ! user functions keys
            DIAGI_S(J1)%USER_ARG(23,1) = LOC(FUNC_NNN)                     ! all avail. button names for user functions (repa.i)
            DIAGI_S(J1)%USER_ARG(24,1) = LOC(FUNC_BUTT)                    ! all avail. button names for user functions (repa.i)
            DIAGI_S(J1)%USER_ARG(25,1) = LOC(IUER)                         ! error handler
!
!C          DIAGI_S(J1).INIT_USER_FUNC = 1                                 ! initialize user function
!
            DIAGI_S(J1)%USER_ARG(0,2)  = 5                                 ! # of arguments for REPBASL
            DIAGI_S(J1)%USER_ARG(1,2)  = LOC(REPA_PAR)
            DIAGI_S(J1)%USER_ARG(2,2)  = LOC(DIAGI_S(J1))                  ! DIAGI record
            DIAGI_S(J1)%USER_ARG(3,2)  = LOC(REPA_FLAG)
            DIAGI_S(J1)%USER_ARG(4,2)  = LOC(REPA_PREV_VAR)
            DIAGI_S(J1)%USER_ARG(5,2)  = LOC(DIAGI_S(J1)%MD_OUT)
!
            DIAGI_S(J1)%USER_ARG(0,3)  = 5                                 ! # of arguments for REPBASL
            DIAGI_S(J1)%USER_ARG(1,3)  = LOC(REPA_PAR)
            DIAGI_S(J1)%USER_ARG(2,3)  = LOC(DIAGI_S(J1))                  ! DIAGI record
            DIAGI_S(J1)%USER_ARG(3,3)  = LOC(REPA_FLAG)
            DIAGI_S(J1)%USER_ARG(4,3)  = LOC(REPA_NEXT_VAR)
            DIAGI_S(J1)%USER_ARG(5,3)  = LOC(DIAGI_S(J1)%MD_OUT)
!
            DIAGI_S(J1)%USER_ARG(0,4)  = 1                                 ! # of arguments for REPGOOD
            DIAGI_S(J1)%USER_ARG(1,4)  = LOC(DIAGI_S(J1))                  ! DIAGI record
!
            DIAGI_S(J1)%USER_CHR(1)    = 'A'                               ! disconnect observations with the same source
            DIAGI_S(J1)%USER_CHR(2)    = CHAR(PAGE_UPDN(1))                ! decimal code for PgUp keybord key
            DIAGI_S(J1)%USER_CHR(3)    = CHAR(PAGE_UPDN(2))                ! decimal code for PgDn keybord key
            DIAGI_S(J1)%USER_CHR(4)    = CHAR(13)
!
! --------- units and bottom lines
!
            IF ( REPA_E(EXP_INDEX)%BAND_KIND(BAND_INDEX) .EQ. 'X' ) THEN
               DIAGI_S(J1)%MESS_BOT =  'DELAYS GION CORR.  -  '//' TIME RANGE: '//TIME_CHR_1(1,J1)//' - '//TIME_CHR_1(2,J1)
               DIAGI_S(J1)%ARG_UNITS = 'nsec'
            ELSE
               DIAGI_S(J1)%MESS_BOT =  'GION NOT AVAILABLE IN S-BAND --> USE X-BAND'
            END IF
!
            DIAGI_S( J1 )%NCLR = 7
            DIAGI_S( J1 )%LER(1) = .TRUE.
            DIAGI_S( J1 )%LER(2) = .TRUE.
            DIAGI_S( J1 )%LER(3) = .TRUE.
            DIAGI_S( J1 )%LER(4) = .FALSE.
            DIAGI_S( J1 )%LER(5) = .FALSE.
            DIAGI_S( J1 )%LER(6) = .FALSE.
            DIAGI_S( J1 )%LER(7) = .FALSE.
            DIAGI_S( J1 )%IBST(1) = 2                                   ! error bar style for using function 1
            DIAGI_S( J1 )%IBST(2) = 2                                   ! error bar style for using function 2
            DIAGI_S( J1 )%IBST(3) = 2                                   ! error bar style for using function 3
            DIAGI_S( J1 )%IBST(4) = 0                                   ! error bar style for using function 4
            DIAGI_S( J1 )%IBST(5) = 0                                   ! error bar style for using function 5
            DIAGI_S( J1 )%IBST(6) = 0                                   ! error bar style for using function 6
            DIAGI_S( J1 )%IBST(7) = 0                                   ! error bar style for using function 7
            DIAGI_S(J1)%NPOI(1) =  NUM_G(J1)                            ! # of points for good obs.
            DIAGI_S(J1)%NPOI(2) =  NUM_M(J1)                            ! # of points for recoverable obs.
            IF ( SHOW_BAD ) THEN                                        ! # of points for bad obs.
               DIAGI_S(J1)%NPOI(3) =  NUM_B(J1)
            ELSE
               DIAGI_S(J1)%NPOI(3) =  0
            END IF
            IF ( REPA_E(EXP_INDEX)%BAND_KIND(BAND_INDEX) .EQ. 'S' ) THEN
               DIAGI_S(J1)%NPOI(1) =  0
               DIAGI_S(J1)%NPOI(2) =  0
               DIAGI_S(J1)%NPOI(3) =  0
            END IF
            DIAGI_S(J1)%NPOI(5) =  0                                    ! # of points
            DIAGI_S(J1)%NPOI(6) =  0                                    ! # of points
            DIAGI_S(J1)%NPOI(7) =  0                                    ! # of points
            DIAGI_S(J1)%ILST(1) = 1                                     ! line style for good obs.
            DIAGI_S(J1)%ILST(2) = 1                                     ! line style for recoverable obs.
            DIAGI_S(J1)%ILST(3) = 1                                     ! line style for bad obs.
            DIAGI_S(J1)%ILST(4) = 2                                     ! line style for zero line
!
! --------- find proper scaling
!
            DIAGI_S(J1)%XMIN = TIME(1,J1)
            IF ( NUM_ALL(J1) .GT. 1 ) THEN
               DIAGI_S(J1)%XMAX = TIME(NUM_ALL(J1),J1)
            ELSE IF (NUM_ALL(J1) .EQ. 1 ) THEN
               DIAGI_S(J1)%XMIN = -0.1
               DIAGI_S(J1)%XMAX = 0.1
            ELSE
               DIAGI_S(J1)%XMAX = 0.0
            END IF
            IF ( NUM_ALL(J1) .GT. 0 ) THEN
               IF ( NUM_G(J1) .GT. 0 ) THEN
                  SCALE_MIN = FVAL8_G(1,J1)
                  SCALE_MAX = FVAL8_G(1,J1)
               ELSE IF ( NUM_M(J1) .GT. 0 ) THEN
                  SCALE_MIN = FVAL8_M(1,J1)
                  SCALE_MAX = FVAL8_M(1,J1)
               ELSE IF ( NUM_B(J1) .GT. 0 ) THEN
                  SCALE_MIN = FVAL8_B(1,J1)
                  SCALE_MAX = FVAL8_B(1,J1)
               END IF
               DO J2=1,NUM_G(J1)
                  IF ( FVAL8_G(J2,J1) .LT. SCALE_MIN ) SCALE_MIN = FVAL8_G(J2,J1)
                  IF ( FVAL8_G(J2,J1) .GT. SCALE_MAX ) SCALE_MAX = FVAL8_G(J2,J1)
               END DO
               DO J2=1,NUM_M(J1)
                  IF ( FVAL8_M(J2,J1) .LT. SCALE_MIN ) SCALE_MIN = FVAL8_M(J2,J1)
                  IF ( FVAL8_M(J2,J1) .GT. SCALE_MAX ) SCALE_MAX = FVAL8_M(J2,J1)
               END DO
               DO J2=1,NUM_B(J1)
                  IF ( FVAL8_B(J2,J1) .LT. SCALE_MIN ) SCALE_MIN = FVAL8_B(J2,J1)
                  IF ( FVAL8_B(J2,J1) .GT. SCALE_MAX ) SCALE_MAX = FVAL8_B(J2,J1)
               END DO
               DIAGI_S(J1)%YMIN = SCALE_MIN - (SCALE_MAX-SCALE_MIN) / 10.0
               DIAGI_S(J1)%YMAX = SCALE_MAX + (SCALE_MAX-SCALE_MIN) / 10.0
               IF ( DABS(SCALE_MIN) .LT. 10D-32 .AND. DABS(SCALE_MAX) .LT. 10D-32 ) THEN
                  DIAGI_S(J1)%YMIN = -1.0
                  DIAGI_S(J1)%YMAX = 1.0
               END IF
            END IF
         END DO
      END IF
!
! --- properties 'TEMPER', 'PRESSU', 'HUMIDI', 'CABLED'
!
      IF ( PROP .EQ. 'TEMPER' .OR. PROP .EQ. 'PRESSU' .OR. PROP .EQ. 'HUMIDI' .OR. &
     &     PROP .EQ. 'CABLED' ) THEN
!
! ------ In this case the user function is called only for the minimum task of displaying the help text.
! ------ Only the parameter CONINI is important!!
!
         FUNC_N = 1                                                        ! # of user functions
         FUNC_B(1) = FUNC_BUTT(10)                                         ! button name - initialize
         FUNC_K(1) = FUNC_KEY(10)                                          ! key - initialize
!
         COL_S_IDX = 7
!
         DO J1=1,BASL_L-BASL_F+1                                           ! start loop over baseline(plot) indices
!
            DIAGI_S(J1)%NUSER_FUNC = 3                                     ! # of DiaGi user functions
!
            DIAGI_S(J1)%USER_FUNC(1)  = LOC_EXT(REPCONN)                       ! address of the entry point REPCONN (only for help function!!)
            DIAGI_S(J1)%USER_FUNC(2)  = LOC_EXT(REPBASL)                      ! address of the entry point REPBASL
            DIAGI_S(J1)%USER_FUNC(3)  = LOC_EXT(REPBASL)                      ! address of the entry point REPBASL
!
            DIAGI_S(J1)%INIT_USER_FUNC = 0                                 ! delete this value from previous property
!
            DIAGI_S(J1)%USER_ARG(0,1)  = 25                                ! # of arguments for REPCONN
            DIAGI_S(J1)%USER_ARG(1,1)  = LOC(CONINI)                       ! flag for user input of source name for connecting lines
            DIAGI_S(J1)%USER_ARG(2,1)  = LOC(NUM_ALL(J1))                  ! dimension of baseline arrays
            DIAGI_S(J1)%USER_ARG(3,1)  = LOC(DIAGI_S(J1))                  ! DIAGI record
            DIAGI_S(J1)%USER_ARG(4,1)  = LOC(COL_S_IDX)                    ! colour index for connecting  points
            DIAGI_S(J1)%USER_ARG(5,1)  = LOC(INFO_CHR_G(1,J1))             ! information for bottom line (good)
            DIAGI_S(J1)%USER_ARG(6,1)  = LOC(INFO_CHR_M(1,J1))             ! information for bottom line (recoverable)
            DIAGI_S(J1)%USER_ARG(7,1)  = LOC(INFO_CHR_B(1,J1))             ! information for bottom line (bad)
            DIAGI_S(J1)%USER_ARG(8,1)  = LOC(TIME_G(1,J1))                 ! arguments (G)
            DIAGI_S(J1)%USER_ARG(9,1)  = LOC(FVAL8_G(1,J1))                ! values (G)
            DIAGI_S(J1)%USER_ARG(10,1) = LOC(FERR8_G(1,J1))                ! errors (G)
            DIAGI_S(J1)%USER_ARG(11,1) = LOC(TIME_M(1,J1))                 ! arguments (M)
            DIAGI_S(J1)%USER_ARG(12,1) = LOC(FVAL8_M(1,J1))                ! values (M)
            DIAGI_S(J1)%USER_ARG(13,1) = LOC(FERR8_M(1,J1))                ! errors (M)
            DIAGI_S(J1)%USER_ARG(14,1) = LOC(TIME_B(1,J1))                 ! arguments (B)
            DIAGI_S(J1)%USER_ARG(15,1) = LOC(FVAL8_B(1,J1))                ! values (B)
            DIAGI_S(J1)%USER_ARG(16,1) = LOC(FERR8_B(1,J1))                ! errors (B)
            DIAGI_S(J1)%USER_ARG(17,1) = LOC(TIME_S(1,J1))                 ! arguments (connecting lines)
            DIAGI_S(J1)%USER_ARG(18,1) = LOC(FVAL8_S(1,J1))                ! values (connecting lines)
            DIAGI_S(J1)%USER_ARG(19,1) = LOC(FERR8_S(1,J1))                ! errors (connecting lines)
            DIAGI_S(J1)%USER_ARG(20,1) = LOC(FUNC_N)                       ! # of user functions
            DIAGI_S(J1)%USER_ARG(21,1) = LOC(FUNC_B)                       ! button names for user functions
            DIAGI_S(J1)%USER_ARG(22,1) = LOC(FUNC_K)                       ! user functions keys
            DIAGI_S(J1)%USER_ARG(23,1) = LOC(FUNC_NNN)                     ! all avail. button names for user functions (repa.i)
            DIAGI_S(J1)%USER_ARG(24,1) = LOC(FUNC_BUTT)                    ! all avail. button names for user functions (repa.i)
            DIAGI_S(J1)%USER_ARG(25,1) = LOC(IUER)                         ! error handler
!
            DIAGI_S(J1)%USER_ARG(0,2)  = 5                                 ! # of arguments for REPBASL
            DIAGI_S(J1)%USER_ARG(1,2)  = LOC(REPA_PAR)
            DIAGI_S(J1)%USER_ARG(2,2)  = LOC(DIAGI_S(J1))                  ! DIAGI record
            DIAGI_S(J1)%USER_ARG(3,2)  = LOC(REPA_FLAG)
            DIAGI_S(J1)%USER_ARG(4,2)  = LOC(REPA_PREV_VAR)
            DIAGI_S(J1)%USER_ARG(5,2)  = LOC(DIAGI_S(J1)%MD_OUT)
!
            DIAGI_S(J1)%USER_ARG(0,3)  = 5                                 ! # of arguments for REPBASL
            DIAGI_S(J1)%USER_ARG(1,3)  = LOC(REPA_PAR)
            DIAGI_S(J1)%USER_ARG(2,3)  = LOC(DIAGI_S(J1))                  ! DIAGI record
            DIAGI_S(J1)%USER_ARG(3,3)  = LOC(REPA_FLAG)
            DIAGI_S(J1)%USER_ARG(4,3)  = LOC(REPA_NEXT_VAR)
            DIAGI_S(J1)%USER_ARG(5,3)  = LOC(DIAGI_S(J1)%MD_OUT)
!
            DIAGI_S(J1)%USER_CHR(1)    = 'A'                               ! disconnect observations with the same source
            DIAGI_S(J1)%USER_CHR(2)    = CHAR(PAGE_UPDN(1))                ! decimal code for PgUp keybord key
            DIAGI_S(J1)%USER_CHR(3)    = CHAR(PAGE_UPDN(2))                ! decimal code for PgDn keybord key
!
! --------- units and bottom lines
!
            IF ( REPA_E(EXP_INDEX)%BAND_KIND(BAND_INDEX) .EQ. 'X' ) THEN
               IF ( PROP .EQ. 'TEMPER' ) THEN
                  DIAGI_S(J1)%MESS_BOT =  'TEMPERATURES  -  '//' TIME RANGE: '//TIME_CHR_1(1,J1)//' - '//TIME_CHR_1(2,J1)
                  DIAGI_S(J1)%ARG_UNITS = 'degrees Celsius'
               ELSE IF ( PROP .EQ. 'PRESSU' ) THEN
                  DIAGI_S(J1)%MESS_BOT =  'PRESSURES  -  '//' TIME RANGE: '//TIME_CHR_1(1,J1)//' - '//TIME_CHR_1(2,J1)
                  DIAGI_S(J1)%ARG_UNITS = 'millibar'
               ELSE IF ( PROP .EQ. 'HUMIDI' ) THEN
                  DIAGI_S(J1)%MESS_BOT =  'HUMIDITIES  -  '//' TIME RANGE: '//TIME_CHR_1(1,J1)//' - '//TIME_CHR_1(2,J1)
                  DIAGI_S(J1)%ARG_UNITS = '%'
               ELSE IF ( PROP .EQ. 'CABLED' ) THEN
                  DIAGI_S(J1)%MESS_BOT =  'CABLE CALIBRATIONS  -  '//' TIME RANGE: '//TIME_CHR_1(1,J1)//' - '//TIME_CHR_1(2,J1)
                  DIAGI_S(J1)%ARG_UNITS = 'nsec'
               END IF
            ELSE
               IF ( PROP .EQ. 'TEMPER' ) THEN
                  DIAGI_S(J1)%MESS_BOT =  'TEMPERATURES NOT AVAILABLE IN S-BAND --> USE X-BAND'
               ELSE IF ( PROP .EQ. 'PRESSU' ) THEN
                  DIAGI_S(J1)%MESS_BOT =  'PRESSURES NOT AVAILABLE IN S-BAND --> USE X-BAND'
               ELSE IF ( PROP .EQ. 'HUMIDI' ) THEN
                  DIAGI_S(J1)%MESS_BOT =  'HUMIDITIES NOT AVAILABLE IN S-BAND --> USE X-BAND'
               ELSE IF ( PROP .EQ. 'CABLED' ) THEN
                  DIAGI_S(J1)%MESS_BOT =  'CABLE CALIBRATIONS NOT AVAILABLE IN S-BAND --> USE X-BAND'
               END IF
            END IF
!
! --------- find proper scaling
!
            DIAGI_S(J1)%XMIN = TIME(1,J1)
            IF ( NUM_ALL(J1) .GT. 1 ) THEN
               DIAGI_S(J1)%XMAX = TIME(NUM_ALL(J1),J1)
            ELSE IF (NUM_ALL(J1) .EQ. 1 ) THEN
               DIAGI_S(J1)%XMIN = -0.5
               DIAGI_S(J1)%XMAX = 0.5
            ELSE
               DIAGI_S(J1)%XMAX = 0.0
            END IF
            IF ( PROP .EQ. 'HUMIDI' ) THEN
               SCALE_NUL = -99899.0D0
            ELSE IF (PROP .EQ. 'CABLED' ) THEN
               SCALE_NUL = - 1.0D0
            ELSE
               SCALE_NUL = -998.0D0
            END IF
            IF ( FVAL8_G(1,J1) .GT. SCALE_NUL ) THEN
               SCALE_MIN = FVAL8_G(1,J1)
               SCALE_MAX = FVAL8_G(1,J1)
            ELSE IF ( FVAL8_M(1,J1) .GT. SCALE_NUL ) THEN
               SCALE_MIN = FVAL8_M(1,J1)
               SCALE_MAX = FVAL8_M(1,J1)
            ELSE
               SCALE_MIN = 0.0D0
               SCALE_MAX = 0.0D0
            END IF
            DO J2=1,NUM_ALL(J1)
               IF ( FVAL8_G(J2,J1) .LT. SCALE_MIN .AND. FVAL8_G(J2,J1) .GT. SCALE_NUL ) SCALE_MIN = FVAL8_G(J2,J1)
               IF ( FVAL8_M(J2,J1) .LT. SCALE_MIN .AND. FVAL8_M(J2,J1) .GT. SCALE_NUL ) SCALE_MIN = FVAL8_M(J2,J1)
               IF ( FVAL8_G(J2,J1) .GT. SCALE_MAX .AND. FVAL8_G(J2,J1) .GT. SCALE_NUL ) SCALE_MAX = FVAL8_G(J2,J1)
               IF ( FVAL8_M(J2,J1) .GT. SCALE_MAX .AND. FVAL8_M(J2,J1) .GT. SCALE_NUL ) SCALE_MAX = FVAL8_M(J2,J1)
            END DO
            DIAGI_S(J1)%YMIN = SCALE_MIN - (SCALE_MAX-SCALE_MIN) / 10.0
            DIAGI_S(J1)%YMAX = SCALE_MAX + (SCALE_MAX-SCALE_MIN) / 10.0
            IF ( DABS(SCALE_MIN) .LT. 10D-32 .AND. DABS(SCALE_MAX) .LT. 10D-32 ) THEN
               DIAGI_S(J1)%YMIN = -1.0
               DIAGI_S(J1)%YMAX = 1.0
            END IF
!
! --------- zero line
!
            IF ( PROP .EQ. 'TEMPER' .OR. PROP .EQ. 'CABLED' ) THEN
               ZERO_TIME(1,J1) = DIAGI_S(J1)%XMIN                          ! time values for zero line
               IF ( NUM_ALL(J1) .GT. 0 ) THEN
                  ZERO_TIME(2,J1) = DIAGI_S(J1)%XMAX
               ELSE
                  ZERO_TIME(2,J1) = 0.0D0
               END IF
            END IF
!
            IF ( IBATCH .EQ. 0 ) THEN
               DIAGI_S(J1)%IDEV      = IDEV
            ELSE
               DIAGI_S(J1)%IDEV      = 7
            END IF
!
            N_COL = 3                                                      ! # of observation colours
!
            DIAGI_S(J1)%STATUS = DIA__DEF                                  ! DiaGi status
!
            DIAGI_S( J1 )%ICLR = 1                                         ! current colour ( main function)
            DIAGI_S( J1 )%NCLR = 7                                         ! number of the functions to be plotted
            DIAGI_S( J1 )%ITRM = 0                                         ! code action for termination
            DIAGI_S( J1 )%IBATCH = 0                                       ! code of batch mode. (0 for interactive)
            DIAGI_S( J1 )%LER(1) = .FALSE.                                 ! flag array for using function 1 errors
            DIAGI_S( J1 )%LER(2) = .FALSE.                                 ! flag array for using function 2 errors
            DIAGI_S( J1 )%LER(3) = .FALSE.                                 ! flag array for using function 3 errors
            DIAGI_S( J1 )%LER(4) = .FALSE.                                 ! flag array for using function 4 errors
            DIAGI_S( J1 )%LER(5) = .FALSE.                                 ! flag array for using function 5 errors
            DIAGI_S( J1 )%LER(6) = .FALSE.                                 ! flag array for using function 6 errors
            DIAGI_S( J1 )%LER(7) = .FALSE.                                 ! flag array for using function 7 errors
!
            DIAGI_S( J1 )%IBST(1) = 0                                      ! error bar style for using function 1 errors
            DIAGI_S( J1 )%IBST(2) = 0                                      ! error bar style for using function 2 errors
            DIAGI_S( J1 )%IBST(3) = 0                                      ! error bar style for using function 3 errors
            DIAGI_S( J1 )%IBST(4) = 0                                      ! error bar style for using function 4 errors
            DIAGI_S( J1 )%IBST(5) = 0                                      ! error bar style for using function 5 errors
            DIAGI_S( J1 )%IBST(6) = 0                                      ! error bar style for using function 6 errors
            DIAGI_S( J1 )%IBST(7) = 0                                      ! error bar style for using function 7 errors
!
            DIAGI_S(J1)%IOST(1) = 1                                        ! overplot style on
            DIAGI_S(J1)%IOST(2) = 1                                        ! overplot style on
            DIAGI_S(J1)%IOST(3) = 1                                        ! overplot style on
            DIAGI_S(J1)%IOST(4) = 0                                        ! overplot style on
            DIAGI_S(J1)%IOST(7) = 0                                        ! overplot style on
!
            DIAGI_S(J1)%IWST(1)= 1                                         ! line width
            DIAGI_S(J1)%IWST(2)= 1                                         ! line width
            DIAGI_S(J1)%IWST(3)= 1                                         ! line width
            DIAGI_S(J1)%IWST(4)= 1                                         ! line width
            DIAGI_S(J1)%IWST(7)= 1                                         ! line width
!
            DIAGI_S(J1)%ICOL(1) = COL_ATR(8)                               ! colour attribute for good obs.(G)
            DIAGI_S(J1)%ICOL(2) = COL_ATR(9)                               ! colour attribute for recoverable obs.(M)
            DIAGI_S(J1)%ICOL(3) = COL_ATR(3)                               ! colour attribute for bad obs.(B)
            DIAGI_S(J1)%ICOL(4) = COL_ATR(4)                               ! colour attribute for zero line
            DIAGI_S(J1)%ICOL(7) = COL_ATR(7)                               ! colour attribute for special case
!
            IF ( PROP .EQ. 'CABLED' ) THEN
               DIAGI_S(J1)%ILST(1) = 2                                     ! line style for 1st station
               DIAGI_S(J1)%ILST(2) = 2                                     ! line style for 2nd station
            ELSE
               IF ( NUM_ALL(J1) .GT. 3 ) THEN
                  DIAGI_S(J1)%ILST(1) = 3                                  ! line style for 1st station
                  DIAGI_S(J1)%ILST(2) = 3                                  ! line style for 2nd station
               ELSE IF ( NUM_ALL(J1) .GT. 1 ) THEN                            ! no spline interpolation!!
                  DIAGI_S(J1)%ILST(1) = 2                                  ! line style for 1st station
                  DIAGI_S(J1)%ILST(2) = 2                                  ! line style for 2nd station
               ELSE                                                           ! only one point!!
                  DIAGI_S(J1)%ILST(1) = 1                                  ! line style for 1st station
                  DIAGI_S(J1)%ILST(2) = 1                                  ! line style for 2nd station
               END IF
            END IF
            DIAGI_S(J1)%ILST(3) = 1                                        ! line style for nothing
            DIAGI_S(J1)%ILST(4) = 2                                        ! line style for zero line
            DIAGI_S(J1)%ILST(7) = 2                                        ! line style for special case
!
            DIAGI_S(J1)%IPST(1) = PNT_STY(1)                               ! point style for 1st station
            DIAGI_S(J1)%IPST(2) = PNT_STY(2)                               ! point style for 2nd station
            DIAGI_S(J1)%IPST(3) = PNT_STY(3)                               ! point style for nothing
            DIAGI_S(J1)%IPST(4) = PNT_STY(4)                               ! point style for zero line
            DIAGI_S(J1)%IPST(7) = PNT_STY(7)                               ! point style for special case
!
            DIAGI_S(J1)%NPOI(1) =  NUM_G(J1)                               ! # of points 1st station
            DIAGI_S(J1)%NPOI(2) =  NUM_G(J1)                               ! # of points 2nd station
            DIAGI_S(J1)%NPOI(3) =  0                                       ! # of points nothing
            IF ( PROP .EQ. 'TEMPER' .OR. PROP .EQ. 'CABLED' ) THEN
               DIAGI_S(J1)%NPOI(4) =  2                                    ! # of points zero line
            ELSE
               DIAGI_S(J1)%NPOI(4) =  0                                    ! # of points zero line
            END IF
            DIAGI_S(J1)%NPOI(5) =  0                                       ! # of points
            DIAGI_S(J1)%NPOI(6) =  0                                       ! # of points
            DIAGI_S(J1)%NPOI(7) =  0                                       ! # of points
            IF ( REPA_E(EXP_INDEX)%BAND_KIND(BAND_INDEX) .EQ. 'S' ) THEN
               DIAGI_S(J1)%NPOI(1) =  0
               DIAGI_S(J1)%NPOI(2) =  0
               DIAGI_S(J1)%NPOI(4) =  0
            END IF
!
            DIAGI_S(J1)%ADR_X8(1) = LOC( TIME_G(1,J1))                    ! address of the 1st element of TIME_G(.)
            DIAGI_S(J1)%ADR_X8(2) = LOC( TIME_G(1,J1))                    ! address of the 1st element of TIME_M(.)
            DIAGI_S(J1)%ADR_X8(4) = LOC( ZERO_TIME(1,J1))                 ! address of the 1st element of ZERO_TIME(.)
!
            DIAGI_S(J1)%ADR_Y8(1) = LOC( FVAL8_G(1,J1))                   ! address of the 1st element of FVAL8_G(.,J1)
            DIAGI_S(J1)%ADR_Y8(2) = LOC( FVAL8_M(1,J1))                   ! address of the 1st element of FVAL8_M(.,J1)
            DIAGI_S(J1)%ADR_Y8(4) = LOC( ZERO_LINE(1))                    ! address of the 1st element of ZERO_LINE(.)
!
! --------- titles of small plots in MultiDiaGi
!
! --------- normal station order
!
            TITS(J1) = ISITN_CHR(IBAS(J1+BASL_F-1,1))(1:ILEN(ISITN_CHR(IBAS(J1+BASL_F-1,1))))//' '// &
     &                 ISITN_CHR(IBAS(J1+BASL_F-1,2))(1:ILEN(ISITN_CHR(IBAS(J1+BASL_F-1,2))))
!
! --------- statistic information for titles of big DiaGi plots
!
            CALL CLRCH ( NUM_ALL_CH )
            WRITE ( NUM_ALL_CH, '(i3)' ) NUM_G(J1) + NUM_M(J1) + NUM_B(J1)         ! total # of obs. --> character value
            CALL CHASHL ( NUM_ALL_CH )
!
! --------- titles of big DiaGi plots
!
            CALL CLRCH ( CH1_TMP )
            CALL CLRCH ( CH2_TMP )
            CALL CLRCH ( CH3_TMP )
            CALL CLRCH ( CH4_TMP )
!
            IF ( PROP .EQ. 'TEMPER' ) THEN
               CH1_TMP = '    Temperatures    #'
            ELSE IF  ( PROP .EQ. 'PRESSU' ) THEN
               CH1_TMP = '    Pressures    #'
            ELSE IF  ( PROP .EQ. 'HUMIDI' ) THEN
               CH1_TMP = '    Humidities    #'
            ELSE IF  ( PROP .EQ. 'CABLED' ) THEN
               CH1_TMP = ' Cable Calibrations #'
            END IF
            CH2_TMP = NUM_ALL_CH(1:ILEN(NUM_ALL_CH))
! --------- normal station order
            IF ( IBAS(J1+BASL_F-1,4) .EQ. 0 ) THEN
               CH3_TMP = ISITN_CHR(IBAS(J1+BASL_F-1,1))(1:ILEN(ISITN_CHR(IBAS(J1+BASL_F-1,1))))// &
     &                                                     ' ('//COL_NAM(8)(1:ILEN(COL_NAM(8)))//')'
               CH4_TMP = ISITN_CHR(IBAS(J1+BASL_F-1,2))(1:ILEN(ISITN_CHR(IBAS(J1+BASL_F-1,2))))// &
     &                                                     ' ('//COL_NAM(9)(1:ILEN(COL_NAM(9)))//')'
! --------- reverse station order
            ELSE
               CH3_TMP = ISITN_CHR(IBAS(J1+BASL_F-1,2))(1:ILEN(ISITN_CHR(IBAS(J1+BASL_F-1,2))))// &
     &                                                     ' ('//COL_NAM(9)(1:ILEN(COL_NAM(9)))//')'
               CH4_TMP = ISITN_CHR(IBAS(J1+BASL_F-1,1))(1:ILEN(ISITN_CHR(IBAS(J1+BASL_F-1,1))))// &
     &                                                     ' ('//COL_NAM(8)(1:ILEN(COL_NAM(8)))//')'
            END IF
!
           DIAGI_S(J1)%ZAG =  REPA_E(EXP_INDEX)%BAND_NAME(BAND_INDEX)(1:9)// &
     &                        REPA_E(EXP_INDEX)%BAND_NAME(BAND_INDEX)(12:13)// &
     &                        CH1_TMP(1:ILEN(CH1_TMP))// &
     &                        CH2_TMP(1:ILEN(CH2_TMP))//' '// &
     &                        CH3_TMP(1:ILEN(CH3_TMP))//' '// &
     &                        CH4_TMP(1:ILEN(CH4_TMP))
         END DO
      END IF
!
! --- titles of MultiDiaGi plots
!
      IF ( PROP .EQ. 'DELCOR' ) THEN
         TITLE = 'Bl.Page('//IBPAG_CHR(1:ILEN(IBPAG_CHR))//'/'//BPAG_NUM_CHR(1:ILEN(BPAG_NUM_CHR))// &
     &            ') - '//OUT1(1:ILEN(OUT1))//' (csig) '// REPA_E(EXP_INDEX)%BAND_NAME(BAND_INDEX)
      ELSE IF ( PROP .EQ. 'DELFUL' ) THEN
         DO J1=1,BASL_L-BASL_F+1
            DIAGI_S(J1)%ZAG(22:22) = 'f'
         END DO
         TITLE = 'Bl.Page('//IBPAG_CHR(1:ILEN(IBPAG_CHR))//'/'//BPAG_NUM_CHR(1:ILEN(BPAG_NUM_CHR))// &
     &            ') - '//OUT1(1:ILEN(OUT1))//' (fsig) '// REPA_E(EXP_INDEX)%BAND_NAME(BAND_INDEX)
      ELSE IF ( PROP .EQ. 'RATCOR' ) THEN
         DO J1=1,BASL_L-BASL_F+1
            DIAGI_S(J1)%ZAG(13:15) = 'rat'
         END DO
         TITLE = 'Bl.Page('//IBPAG_CHR(1:ILEN(IBPAG_CHR))//'/'//BPAG_NUM_CHR(1:ILEN(BPAG_NUM_CHR))// &
     &            ') - Rate Residuals (csig) '// REPA_E(EXP_INDEX)%BAND_NAME(BAND_INDEX)
      ELSE IF ( PROP .EQ. 'RATFUL' ) THEN
         DO J1=1,BASL_L-BASL_F+1
            DIAGI_S(J1)%ZAG(13:22) = 'rat.res.(f'
         END DO
         TITLE = 'Bl.Page('//IBPAG_CHR(1:ILEN(IBPAG_CHR))//'/'//BPAG_NUM_CHR(1:ILEN(BPAG_NUM_CHR))// &
     &            ') - Rate Residuals (fsig) '// REPA_E(EXP_INDEX)%BAND_NAME(BAND_INDEX)
      ELSE IF ( PROP .EQ. 'ELEVAT' ) THEN
         goto 1000
      ELSE IF ( PROP .EQ. 'SIGNOI' ) THEN
         write (6,*) 'REPA: SNR(1,1)=',FVAL8_G(1,1)
         goto 1000
      ELSE IF ( PROP .EQ. 'QUALCO' ) THEN
         goto 1000
      ELSE IF ( PROP .EQ. 'PHADEL' ) THEN
         TITLE = 'Bl.Page('//IBPAG_CHR(1:ILEN(IBPAG_CHR))//'/'//BPAG_NUM_CHR(1:ILEN(BPAG_NUM_CHR))// &
     &            ') - Phase Delays  '// REPA_E(EXP_INDEX)%BAND_NAME(BAND_INDEX)
      ELSE IF ( PROP .EQ. 'GDELE1' ) THEN
         TITLE = 'Bl.Page('//IBPAG_CHR(1:ILEN(IBPAG_CHR))//'/'//BPAG_NUM_CHR(1:ILEN(BPAG_NUM_CHR))// &
     &            ') - RES. vs. Elev. St.1  '// REPA_E(EXP_INDEX)%BAND_NAME(BAND_INDEX)
      ELSE IF ( PROP .EQ. 'GDELE2' ) THEN
         TITLE = 'Bl.Page('//IBPAG_CHR(1:ILEN(IBPAG_CHR))//'/'//BPAG_NUM_CHR(1:ILEN(BPAG_NUM_CHR))// &
     &            ') - RES. vs. Elev. St.2  '// REPA_E(EXP_INDEX)%BAND_NAME(BAND_INDEX)
      ELSE IF ( PROP .EQ. 'GDELA1' ) THEN
         TITLE = 'Bl.Page('//IBPAG_CHR(1:ILEN(IBPAG_CHR))//'/'//BPAG_NUM_CHR(1:ILEN(BPAG_NUM_CHR))// &
     &            ') - RES. vs. Azimut St.1  '// REPA_E(EXP_INDEX)%BAND_NAME(BAND_INDEX)
      ELSE IF ( PROP .EQ. 'GDELA2' ) THEN
         TITLE = 'Bl.Page('//IBPAG_CHR(1:ILEN(IBPAG_CHR))//'/'//BPAG_NUM_CHR(1:ILEN(BPAG_NUM_CHR))// &
     &            ') - RES. vs. Azimut St.2  '// REPA_E(EXP_INDEX)%BAND_NAME(BAND_INDEX)
      ELSE IF ( PROP .EQ. 'SNELE1' ) THEN
         TITLE = 'Bl.Page('//IBPAG_CHR(1:ILEN(IBPAG_CHR))//'/'//BPAG_NUM_CHR(1:ILEN(BPAG_NUM_CHR))// &
     &            ') - SNR vs. Elev. St.1  '// REPA_E(EXP_INDEX)%BAND_NAME(BAND_INDEX)
      ELSE IF ( PROP .EQ. 'SNELE2' ) THEN
         TITLE = 'Bl.Page('//IBPAG_CHR(1:ILEN(IBPAG_CHR))//'/'//BPAG_NUM_CHR(1:ILEN(BPAG_NUM_CHR))// &
     &            ') - SNR vs. Elev. St.2  '// REPA_E(EXP_INDEX)%BAND_NAME(BAND_INDEX)
      ELSE IF ( PROP .EQ. 'TEMPER' ) THEN
         TITLE = 'Bl.Page('//IBPAG_CHR(1:ILEN(IBPAG_CHR))//'/'//BPAG_NUM_CHR(1:ILEN(BPAG_NUM_CHR))// &
     &            ') - Temperatures  '// REPA_E(EXP_INDEX)%BAND_NAME(BAND_INDEX)
      ELSE IF ( PROP .EQ. 'PRESSU' ) THEN
         TITLE = 'Bl.Page('//IBPAG_CHR(1:ILEN(IBPAG_CHR))//'/'//BPAG_NUM_CHR(1:ILEN(BPAG_NUM_CHR))// &
     &            ') - Pressures  '// REPA_E(EXP_INDEX)%BAND_NAME(BAND_INDEX)
      ELSE IF ( PROP .EQ. 'HUMIDI' ) THEN
         TITLE = 'Bl.Page('//IBPAG_CHR(1:ILEN(IBPAG_CHR))//'/'//BPAG_NUM_CHR(1:ILEN(BPAG_NUM_CHR))// &
     &            ') - Humidities  '// REPA_E(EXP_INDEX)%BAND_NAME(BAND_INDEX)
      ELSE IF ( PROP .EQ. 'CABLED' ) THEN
         TITLE = 'Bl.Page('//IBPAG_CHR(1:ILEN(IBPAG_CHR))//'/'//BPAG_NUM_CHR(1:ILEN(BPAG_NUM_CHR))// &
     &            ') - Cable Calibrations  '// REPA_E(EXP_INDEX)%BAND_NAME(BAND_INDEX)
      ELSE IF ( PROP .EQ. 'GRIONO' ) THEN
         TITLE = 'Bl.Page('//IBPAG_CHR(1:ILEN(IBPAG_CHR))//'/'//BPAG_NUM_CHR(1:ILEN(BPAG_NUM_CHR))// &
     &            ') - Del. GION corr.  '// REPA_E(EXP_INDEX)%BAND_NAME(BAND_INDEX)
      END IF
!
! --- Call MULTI_DIAGI
!
      IF ( MD_IN_FLAG .EQ. 1 ) THEN
           DIAGI_S(1)%MD_IN = 1
        ELSE IF ( MD_IN_FLAG .EQ. 2 ) THEN
           DIAGI_S(1)%MD_IN = BASL_PAG(IBPAG)
        ELSE
         DIAGI_S(1)%MD_IN = 0
      END IF
!
      REPA_PAR = 0
      REPA_FLAG = REPA__NO
      ASHIFT = .FALSE.                                                     ! flag for ambiguity resolving
  970 CONTINUE
      IUER  = -1                                                           ! error handler                                                    ! jump from non amb.reset
      CALL MULTI_DIAGI ( TITLE, MPL, NC, NR, TITS, MPB, BUT, BUT_LET, &
     &                           '/tmp/mumu_', DIAGI_S, ICODE, IUER )
      IF ( IUER .NE. 0 ) STOP 'REPA: error in MULTI_DIAGI'
!
! --- browse through baselines by PgUp/PgDn
!
      DIAGI_S(1)%MD_IN = 0                                                  ! first bl. to display (0=MultiDiaGi plot)
      MD_IN_FLAG = 0                                                        ! flag for changing bl. by PgUp/PgDn
      IF ( ICODE .EQ. 0 .AND. REPA_FLAG .NE. REPA__NO ) THEN
         BAS_IN = 0
         DO 420 J2=1,BASL_L-BASL_F+1
            IF ( REPA_PAR .EQ. LOC(DIAGI_S(J2)) ) THEN
               BAS_IN = J2
            END IF
            DIAGI_S(J2)%MD_OUT = 0
420      CONTINUE
         IF ( BAS_IN .EQ. 0 ) THEN
            WRITE (6,*) 'REPA 3726 trap of internal control: baseline not found. REPA_PAR: ', REPA_PAR
            CALL EXIT (7)
         END IF
         IF ( REPA_FLAG .EQ. REPA__PREV ) THEN                              ! previous baseline plot
            BAS_IN = BAS_IN - 1                                             ! counter for current bl.page
            IF ( BAS_IN .LT. 1 ) THEN
               IF ( BPAG_NUM .EQ. 1 ) THEN                                  ! total # of bl.pages=1 -> no need to read new bl.
                  BAS_IN = BASL_L                                           ! set counter to last index
                  DIAGI_S(1)%MD_IN = BAS_IN                                 ! DiaGi bl. start index
                  GOTO 970
               ELSE
                  IF ( IBPAG .EQ. 1 ) THEN                                  ! jump to last bl.page
                     IBPAG = BPAG_NUM
                  ELSE                                                      ! jump to previous bl.page
                     IBPAG = IBPAG - 1
                  END IF
                  MD_IN_FLAG = 2                                            ! flag for change to prev. bl.page
                  GOTO 950
               END IF
            END IF
         ELSE IF ( REPA_FLAG .EQ. REPA__NEXT ) THEN                         ! next baseline plot
            BAS_IN = BAS_IN + 1                                             ! counter for current bl.page
            IF ( BAS_IN .GT. BASL_PAG(IBPAG) ) THEN
               IF ( BPAG_NUM .EQ. 1 ) THEN                                  ! total # of bl.pages=1 -> no need to read new bl.
                  BAS_IN = BASL_F                                           ! set counter to first index
                  DIAGI_S(1)%MD_IN = BAS_IN                                 ! DiaGi bl. start index
                  GOTO 970
               ELSE
                  IF ( IBPAG .EQ. BPAG_NUM ) THEN                           ! jump to first bl.page
                     IBPAG = 1
                  ELSE                                                      ! jump to previous bl.page
                     IBPAG = IBPAG + 1
                  END IF
                  MD_IN_FLAG = 1                                            ! flag for change to next. bl.page
                  GOTO 950
               END IF
            END IF
         ELSE
            WRITE (6,*) 'REPA 3727 trap of internal control: wrong REPA_FLAG: ', REPA_FLAG
            CALL EXIT (7)
         END IF
         DIAGI_S(1)%MD_IN = BAS_IN                                          ! DiaGi bl. start index
         GOTO 970                                                           ! jump to MultiGiaGi
      END IF
!
! --- analyze return code of MULTI_DIAGI (ICODE = last index of BUT_LET)
!
      RESAMB = 'N'                                                             ! flag for ambiguity reset
      IF ( PROP .EQ. 'DELCOR' .OR. PROP .EQ. 'DELFUL' ) THEN       ! this part with ambiguity reset (one button more)
         IF ( ICODE .EQ. 1 ) THEN                                              ! previous baseline page
            IF ( BPAG_NUM .EQ. 1 ) GOTO 970                                    ! if only one baseline page exists
            IF ( IBPAG .EQ. 1 ) THEN
               IBPAG = BPAG_NUM
            ELSE
               IBPAG = IBPAG - 1
            END IF
            GOTO 950
         ELSE IF ( ICODE .EQ. 2 ) THEN                                         ! next baseline page
            IF ( BPAG_NUM .EQ. 1 ) GOTO 970                                    ! if only one baseline page exists
            IF ( IBPAG .EQ. BPAG_NUM ) THEN
               IBPAG = 1
            ELSE
               IBPAG = IBPAG + 1
            END IF
            GOTO 950
         ELSE IF ( ICODE .EQ. 3 ) THEN                                         ! reset ambiguities for all baselines
!
            IF ( IDA_DTP == 'GX' .OR. IDA_DTP == 'GS' ) THEN                   ! group delay data types
               WRITE ( 6, '(A$)' ) ' DO YOU REALLY WANT TO RESET ALL AMBIGUITIES OF '// &
     &                             REPA_E( EXP_INDEX )%BAND_KIND(BAND_INDEX)//'-BAND? (Y/N) '
               READ ( *, '(A1)' ) RESAMB
               IF ( RESAMB .EQ. 'Y' .OR. RESAMB .EQ. 'y' ) THEN
                  RESAMB = 'Y'
                  GOTO 950
               ELSE                                                            ! no amb. reset
                  WRITE ( 6, * ) 'O.K. --> BACK TO CURRENT BASELINE PAGE'
                  GOTO 970
               END IF
            ELSE                                                               ! phase delay data types
               WRITE ( 6, '( /A )' ) 'RESETTING OF AMBIGUITIES FOR ALL BASELINES IN ONE STEP'
               WRITE ( 6, '( A )' )  'NOT YET FOR PHASE DELAY DATATYPES!'
               GOTO 970                                                        ! return baseline page
            END IF
         ELSE IF ( ICODE .EQ. 4 ) THEN                                         ! resolve ambiguities
            IF ( IDA_DTP == 'GX' .OR. IDA_DTP == 'GS' ) THEN                   ! group delay data types
               IF ( BASL_NUM .GT. PPPL_MAX1 ) THEN
                  WRITE ( 6, * ) ' '
                  WRITE ( 6, * ) 'ATTENTION!! NUMBER OF BASELINES EXCEEDS PARAMETER PPPL_MAX IN repa.i'
                  WRITE ( 6, * ) '            NUMBER OF BASELINES = ',BASL_NUM
                  WRITE ( 6, * ) '            PPPL_MAX = ',PPPL_MAX
                  WRITE ( 6, * ) 'RECOMMENDATION: EITHER INCREASE PPPL_MAX AND RECOMPILE/RELINK REPA'
                  WRITE ( 6, * ) '                OR SOLVE AMBIGUITIES STEP BY STEP IN THE BASELINES'
                  WRITE ( 6, '( A$ )' ) ' <ENTER> TO PROCEED '
                  READ ( *, '( A1 )' ) STR_TMP(1:1)
                  GOTO 970
               END IF
               WRITE ( 6, '(/A)' ) ' SOLVE AMBIGUITIES FOR ALL BASELINES OF '// &
     &                             REPA_E( EXP_INDEX )%BAND_KIND(BAND_INDEX)//'-BAND'
               IBPAG = 1                                                       ! jump to 1st baseline page
               ASHIFT = .TRUE.                                                 ! ambiguity flag
               GOTO 950
            ELSE                                                               ! phase delay data types
               WRITE ( 6, '( /A )' ) 'RESOLVING OF AMBIGUITIES FOR ALL BASELINES IN ONE STEP'
               WRITE ( 6, '( A )' )  'NOT YET FOR PHASE GROUP DELAY DATATYPES!'
               GOTO 970                                                        ! return baseline page
            END IF
!
         ELSE IF ( ICODE .EQ. 5 ) THEN                                         ! return to property page
            GOTO 945
         ELSE IF ( ICODE .EQ. 6 ) THEN                                         ! reload current band
            JUMP = .TRUE.
            GOTO 915
         ELSE IF ( ICODE .EQ. 7 ) THEN                                         ! change band
            IF ( BAND_INDEX .EQ. REPA_E( EXP_INDEX )%BAND_NUM ) THEN
               BAND_INDEX = 1
            ELSE
               BAND_INDEX = BAND_INDEX + 1
            END IF
            JUMP = .TRUE.
            GOTO 915
         ELSE IF ( ICODE .EQ. 8 .OR. ICODE .EQ. 0 ) THEN                       ! exit
            GOTO 1000
         ENDIF
       ELSE                                                         ! this part without ambiguity reset
         IF ( ICODE .EQ. 1 ) THEN                                              ! previous baseline page
            IF ( BPAG_NUM .EQ. 1 ) GOTO 970                                    ! if only one baseline page exists
            IF ( IBPAG .EQ. 1 ) THEN
               IBPAG = BPAG_NUM
            ELSE
               IBPAG = IBPAG - 1
            END IF
            GOTO 950
         ELSE IF ( ICODE .EQ. 2 ) THEN                                         ! next baseline page
            IF ( BPAG_NUM .EQ. 1 ) GOTO 970                                    ! if only one baseline page exists
            IF ( IBPAG .EQ. BPAG_NUM ) THEN
               IBPAG = 1
            ELSE
               IBPAG = IBPAG + 1
            END IF
            GOTO 950
         ELSE IF ( ICODE .EQ. 3 ) THEN                                         ! return to property page
            GOTO 945
         ELSE IF ( ICODE .EQ. 4 ) THEN                                         ! reload current band
            JUMP = .TRUE.
            GOTO 915
         ELSE IF ( ICODE .EQ. 5 ) THEN                                         ! change band
            IF ( BAND_INDEX .EQ. REPA_E( EXP_INDEX )%BAND_NUM ) THEN
               BAND_INDEX = 1
            ELSE
               BAND_INDEX = BAND_INDEX + 1
            END IF
            JUMP = .TRUE.
            GOTO 915
         ELSE IF ( ICODE .EQ. 6 .OR. ICODE .EQ. 0 ) THEN                                         ! exit
            GOTO 1000
         ENDIF
      END IF
 1000 CONTINUE
      IF ( .NOT. FL_STANDALONE ) THEN
           CALL END_PROG()
      END IF
      END  PROGRAM    REPAB
