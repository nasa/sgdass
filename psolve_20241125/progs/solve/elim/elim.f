      PROGRAM    ELIM_LAUNCH
      IMPLICIT   NONE
      INCLUDE   'solve.i'
      CHARACTER    STR*32
      INTEGER*8    STACK_SIZE_IN_BYTES, GB, IS
      PARAMETER  ( GB = 1024*1024*1024 )
      PARAMETER  ( STACK_SIZE_IN_BYTES = PSOLVE__STACK_SIZE_IN_GIGABYTES * GB )
      INTEGER*8, EXTERNAL :: SET_STACKSIZE 
!
! --- Set stacksize
!
      IS = SET_STACKSIZE ( %VAL(STACK_SIZE_IN_BYTES) )
      CALL INCH8    ( STACK_SIZE_IN_BYTES/INT8(1024), STR )
      CALL SETENV   ( 'GOMP_STACKSIZE'//CHAR(0), TRIM(STR)//CHAR(0), %VAL(1) )
      CALL ELIM()
      END  PROGRAM  ELIM_LAUNCH
!
! ------------------------------------------------------------------------
!
      SUBROUTINE ELIM()
! ************************************************************************
! *                                                                      *
! *     Program  ELIM  is the part of SOLVE software for geodetic VLBI   *
! *   data analysis. ELIM can be called in batch and interactive mode.   *
! *   When ELIM is called in interactive mode it makes iteratively       *
! *   outliers elimination or restores observations previously           *
! *   suppressed and not used in solution.                               *
! *                                                                      *
! *     ELIM prints menu screen form and inquire user parameters of the  *
! *   work and mode: elimination and restoration. Then it reads scratch  *
! *   file, put some subset of the data from there in temporary data     *
! *   structures, builds normal equations, solve them, calculates post   *
! *   fit residuals and their statistics.                                *
! *                                                                      *
! *     ELIM scans in elimination mode all used observations and finds   *
! *   the most substantial outlier. If postfit residuals of that outlier *
! *   exceeds the specified limit then the downweight flag for this      *
! *   observation is set up (that means that this observation will not   *
! *   participate further in solution) and ELIM makes update of the      *
! *   estimates, their covariance matrix, postfit residuals and their    *
! *   statistics for elimination of that observation. Then the process   *
! *   is repeated until the last outlier will be eliminated or user      *
! *   terminate the process manually.                                    *
! *                                                                      *
! *     ELIM/MILE scans in restoration mode all not used but potentially *
! *   recoverable observations and finds the most obvious candidate in   *
! *   restoration: observations with minimal (in some sense) postfit     *
! *   residual. If postfit residuals of the candidate does not exceed    *
! *   the specified limit then the downweight flag for this observation  *
! *   is cleared (that means that this observation will participate      *
! *   further in solution) and ELIM/MILE makes update of the estimates,  *
! *   their covariance matrix, postfit residuals and their statistics    *
! *   for including that previously suppressed observation in solution.  *
! *   Then the process is repeated until the last candidate will be      *
! *   restored or user terminate the process manually.                   *
! *                                                                      *
! *     After completion the work ELIM updates downweight flags in       *
! *   scratch files.                                                     *
! *                                                                      *
! *     ELIM calls routine UPWEI for compuation of quadratic corrections *
! *   to formal uncertainties in order to make the ratio of a weighted   *
! *   sum of post-fit residuals (or chi-square) to its mathematical      *
! *   expectation (chi/ndg) near to unity. ELIM may work in batch mode   *
! *   for execution this operation.                                      *
! *                                                                      *
! *     Restrictions:                                                    *
! *                                                                      *
! *   1) ELIM/MILE works only in B3D mode.                               *
! *   2) It doesn't support observation types with delay rate.           *
! *   3) Estimation of user-defined variables is not supported.          *
! *                                                                      *
! *   WHEN       WHO  VERS.  WHAT                                        *
! *                                                                      *
! *   12-SEP-97  pet  v 0.0  Beginning of the developing.                *
! *                                                                      *
! *   08-OCT-97  pet  v 1.0  Release of the first version.               *
! *                                                                      *
! *   29-OCT-97  pet  v 1.1  Fixed bug: ELIM worked incorrectly when the *
! *                          last observation in the database had bad    *
! *                          ionosphere.                                 *
! *                                                                      *
! *   31-OCT-97  pet  v 2.0  Option " Acceleration factor" is added in   *
! *                          menu. Support of environment variable       *
! *                          ELIM_UPD is added. Residuals updated not    *
! *                          after each observation but only after each  *
! *                          N observations, where N is acceleration     *
! *                          factor. Computational expenses are          *
! *                          approximately reciprocal to N. But deferred *
! *                          updates of the residuals may worsen results *
! *                          since not the best candidates in            *
! *                          elimination/restoration will be taken.      *
! *                          For this reason it is recommended to set N  *
! *                          always 1, except the cases when low speed   *
! *                          of elimination/restoration process become   *
! *                          intolerable.                                *
! *                                                                      *
! *   12-DEC-97  pet  v 2.1  Logic of detection best candidate for       *
! *                          rejection and for restoration is changed    *
! *                          a bit for the case when both upper          *
! *                          threshold and cutoff criterion are applied. *
! *                                                                      *
! *   30-JAN-98  pet  v 2.2  Added routine for weights update.           *
! *                                                                      *
! *   02-FEB-98  pet  v 2.3  "Double-suppressed observation" bug fixed.  *
! *                          Previous version didn't work correctly when *
! *                          the observation has quality codes less than *
! *                          8 at both bands.                            *
! *                                                                      *
! *   28-FEB-98  pet  v 2.4  Support of phase-delay solution types was   *
! *                          added.                                      *
! *                                                                      *
! *   23-MAR-98  pet  v 2.5  Support of mode "restoration with resolving *
! *                          ambiguities and update of ionosphere        *
! *                          correction due to changes in ambiguities"   *
! *                          was added for solution type G_GXS or P_PXS. *
! *                                                                      *
! *   24-MAR-98  pet  v 2.6  Changed the logic when both threshold and   *
! *                          cutoff criterion are used in elimination:   *
! *                          all observation are examined in the order   *
! *                          of descending their normalized residual.    *
! *                          The observation is eliminated if it has     *
! *                          normalized residual exceeding cutoff        *
! *                          criterion OR it has residual exceeding      *
! *                          threshold criterion. Observations are       *
! *                          examined in the order of ascending their    *
! *                          normalized residuals in restoration mode.   *
! *                          Observation is restored if it has normalized*
! *                          residual less than cutoff criterion OR it   *
! *                          has residual less than threshold criterion. *
! *                                                                      *
! *   03-APR-98  pet  v 2.61 Minor bug fixed in elim.f and manor_init.f: *
! *                          variable idbe was defined as INTEGER*2, but *
! *                          should be defined as INTEGER*4  .           *
! *                                                                      *
! *   28-APR-98  pet  v 2.62 Minor bug fixed in elim.f: mode             *
! *                          "restoration ambiguities and update of      *
! *                          ionosphere correction due to changes in     *
! *                          ambiguities" was set in previous versions   *
! *                          in all solution types when the first        *
! *                          iteration was running. Now it is set only   *
! *                          in combination modes.                       *
! *                                                                      *
! *   01-MAY-98  pet  v 2.7  Added support of different suppression      *
! *                          methods. Added new option in elim_menu:     *
! *                          "Change suppression method". Fixed bug:     *
! *                          suppression status of observations was not  *
! *                          traced when quality code limit was changed. *
! *                          Changed title lines at the header of menu.  *
! *                          Added new section in on-line help.          *
! *                                                                      *
! *   15-JUN-98  pet  v 2.71 Fixed a bug in io_obser.f: previous version *
! *                          didn't work correctly when axis offset was  *
! *                          estimated.                                  *
! *                                                                      *
! *   12-JUL-98  pet  v 2.8  Changed logic of singularity check. Made it *
! *                          compatible with logic used in PROC, ARCPE,  *
! *                          BATCH. Removed ELIM_MSR, ELIM_MBS global    *
! *                          variables. Added parameterization test:     *
! *                          a check: doesn't solution's set up contain  *
! *                          parameters unsupported by ELIM.             *
! *                                                                      *
! *   02-AUG-98  pet  v 2.81 Added support of verbosity level 2. It may  *
! *                          appeared useful for processing large        *
! *                          sessions (more than 5000 observations).     *
! *                                                                      *
! *   03-AUG-98  pet  v 2.9  Changed logic to force ELIM to work faster  *
! *                          and not to make unnecessary calculations    *
! *                          when threshold criteria is in use.          *
! *                          Eliminated a bug connected with processing  *
! *                          sessions with a priori clock model.         *
! *                                                                      *
! *   03-NOV-98  pet v. 2.10 Changed the logic of support resolving      *
! *                          ambiguities on the fly in restoration mode. *
! *                          New logic conserves misclosures excess. It  *
! *                          is assumed that misclosure excess for       *
! *                          observables doesn't excess one ambiguity    *
! *                          jump.                                       *
! *                                                                      *
! *   03-NOV-98  pet v. 2.101 Fixed a bug in support of ambiguity        *
! *                           resolution on the fly.                     *
! *                                                                      *
! *   22-NOV-98  pet v. 2.11 Added support of parameter ELIM_MSR --      *
! *                          Maximum uncertainty. Observations with      *
! *                          formal uncertainty exceeding ELIM_MSR       *
! *                          are marked as outliers in ELIM mode and     *
! *                          are not eligible for restoration in MILE    *
! *                          mode. If ELIM_MSR < 1.D-12 sec then it is   *
! *                          ignored.                                    *
! *                                                                      *
! *   29-JAN-99  pet v. 2.111 Corrected a bug: the previous version died *
! *                           when effective frequency of the            *
! *                           observation had value NaN.                 *
! *                                                                      *
! *   07-APR-99  pet v. 2.112 The previous version may die in weights    *
! *                           updating when considerable number of       *
! *                           baselines were deselected.                 *
! *                                                                      *
! *   28-APR-99  pet v. 2.113 The previous version didn't work correctly *
! *                           when there were stations in the database   *
! *                           without observations.                      *
! *                                                                      *
! *   07-MAY-99  pet v. 2.114 Corrected a bug: the previous version      *
! *                           didn't work in supression mode             *
! *                           SUPMET_PRE98 when the last observation had *
! *                           ionosphere contribution zero.              *
! *                                                                      *
! *   12-OCT-99  pet v. 2.115 Set post OCT99 version control.            *
! *                                                                      *
! *   1999.11.19 pet v. 2.116 Updated for using Calc 9.1 (it supplies    *
! *                           troposphere gradient derivative).          *
! *                                                                      *
! *   2000.01.30 pet v. 2.117 Fixed a bug: variable AMP_SP in RESID_ST   *
! *                           was not always initialized and its check   *
! *                           for "Not-A-Number" resulted sometimes to   *
! *                           premature end of iterations.               *
! *                                                                      *
! *   2000.03.29 pet v. 3.0   a) Added support of EQUMEM_FLAG --         *
! *                              if EQUMEM_FLAG = .TRUE. then equations  *
! *                              of conditions are stored in memory what *
! *                              speeds up computation by 1.5 times.     *
! *                           b) Added support of batch mode for         *
! *                              updating weights. ELIM reads            *
! *                              pipe-buffer. If the first INTEGER*2     *
! *                              word is 0 then ELIM works in            *
! *                              interactive mode, if that word is 1     *
! *                              then ELIM updates weights in            *
! *                              non-interactive mode.                   *
! *                           c) added support of an environment         *
! *                              variable TERM_COLOR. If TERM_COLOR is   *
! *                              set NO, then no color escape sequences  *
! *                              are generated. (The window with SOLVE   *
! *                              may dissappear in attempt to change     *
! *                              colors if no free colors are avaialbe   *
! *                              due to bug in hpterm).                  *
! *                                                                      *
! *   2000.04.04 pet v. 3.1   a) Moved definitions of chi/ndg tolerance  *
! *                              factor and floor for batch UPWEI to     *
! *                              glbc4.i . Changed their numerical       *
! *                              values: tolerance 0.02 and floor for    *
! *                              group delay solutions to 8 psec.        *
! *                           b) Fixed a bug in UPWEI_INFO -- the        *
! *                              previous version of UPWEI(ELIM) tried   *
! *                              to compute statistics for deslected     *
! *                              baselines/sources incorrrectly what     *
! *                              sometimes resulted to abnormal          *
! *                              termination.                            *
! *                                                                      *
! *   2000.04.18 pet v. 3.11     Prevented an attempt to resolve group   *
! *                              delay ambiguities on the fly if         *
! *                              ambiguities exceed by modulo 32766      *
! *                              since SOLVE internally keeps number of  *
! *                              ambiguities as INTEGER*2.               *
! *                                                                      *
! *   2000.07.18 pet v. 3.12     Fixed a bug: the previous version       *
! *                              didn't work when Solve started from     *
! *                              detached terminal in Batch mode.        *
! *                                                                      *
! *   2002.07.30 pet v. 3.13     Added better error control when ELIM    *
! *                              is invoked in batch mode. The previous  *
! *                              version issued the error message, but   *
! *                              terminated normally and wrote some      *
! *                              values in weights file. The new version *
! *                              terminates abnoramlly with error code 1 *
! *                              and creates no new records in weight    *
! *                              file.                                   *
! *                                                                      *
! *   2002.07.30 pet v. 3.14     Added support of ELIM_ERROR_IGNORE      *
! *                              kludge environement variable. If set it *
! *                              causes ELIM to return error code 0 when *
! *                              the error occurred.                     *
! *                                                                      *
! *   2007.07.01 pet v. 3.15     Added an ability to pass PARU-file from *
! *                              batch Solve.                            *
! *                                                                      *
! *   2020.10.01 pet v. 3.16     Added setting stacksize.                *
! *                                                                      *
! *  ###  12-SEP-97      ELIM    v3.16  (c) L. Petrov  01-JUN-2007 ###   *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'solve.i'
      INCLUDE   'socom.i'
      INCLUDE   'prfil.i'
      INCLUDE   'precm.i'
      INCLUDE   'glbcm.i'
      INCLUDE   'glbc2.i'
      INCLUDE   'glbc3.i'
      INCLUDE   'glbc4.i'
      INCLUDE   'oborg.i'
      INCLUDE   'obser.i'
      INCLUDE   'fast.i'
      INCLUDE   'ncrec.i'
      INCLUDE   'equmem.i'
      LOGICAL*2   KBIT
      INTEGER*2   LDBNAM(5,15), IDBV(15), IDB2
      INTEGER*4   IDBE(15)
      CHARACTER   CDBNAM(15)*10, DBNAME*16
      EQUIVALENCE ( CDBNAM, LDBNAM(1,1) )
      CHARACTER   VER_ELIM*21, VER_MILE*21
      INTEGER*4   J1, IP, N_OBS, IDBF, IER, IUER
      INTEGER*4   ISIM, K_RJC, K_RST, KG_RJC, KG_RST
      INTEGER*8        ML_OBSER
      ADDRESS__TYPE :: MA_OBSER, IADR_OBSSCA, IADR_OBSSTA, IADR_OBSBAS, IADR_RES
      INTEGER*4   FAST_COV__SAV, SNGCHK_CMP, ICMPL, L_SCA, L_STA
      INTEGER*4   R_SOU, RIS_SOU(MO_SOU), R_STA, RIS_STA(MO_STA), R_BAS, &
     &            RIS_BAS(MO_BAS)
      CHARACTER   STR*80, STR1*80, STR2*80, ASIM*1, HOSTNAME*32, &
     &            DATE_RAW*24, DATE*18, GET_VERSION*54
      LOGICAL*4   F_OPTIN, F_SAVE, F_UPDATE, F_UPWEI, F_SUPSTAT, FL_ERROR, &
     &            F_CHI, F_SUP, UPWEI_WAS, EQUMEM_INIT_WAS, UPWEI_BATCH
      LOGICAL*4   DATYP_INQ, SUP_UPDATE
      TYPE ( PLACE__STRU   ) ::  PLACE
      TYPE ( B3D__STRU     ) ::  B3DOBJ
      TYPE ( B1B3D__STRU   ) ::  B1B3DOBJ
      TYPE ( NCREC__STRU   ) ::  NCREC
      TYPE ( DBOBJ_O__STRU ) ::  DBOBJ
      TYPE ( HLD_O__STRU   ) ::  OBSHLD
      TYPE ( RST_O__STRU   ) ::  RST
      TYPE ( CHIACC__STRU  ) ::  CHIOBJ
      TYPE ( EQUMEM__STRU  ) ::  EQUMEM
!
      INTEGER*2     IBUFF(64)
      CHARACTER     CBUFF*128, PARU_FILE*128
      EQUIVALENCE ( IBUFF, CBUFF )
      INTEGER*2  INT2_ARG
      INTEGER*4  INT4
      INT4(INT2_ARG) = INT(INT2_ARG,KIND=4)
      INTEGER*4, EXTERNAL :: IFIND_PL, ILEN, I_LEN
      INTEGER*8    STACK_SIZE_IN_BYTES, GB, IS
      PARAMETER  ( GB = 1024*1024*1024 )
      PARAMETER  ( STACK_SIZE_IN_BYTES = PSOLVE__STACK_SIZE_IN_GIGABYTES * GB )
      INTEGER*8, EXTERNAL :: SET_STACKSIZE 
!
! --- Set stacksize
!
      IS = SET_STACKSIZE ( %VAL(STACK_SIZE_IN_BYTES) )
      CALL INCH8    ( STACK_SIZE_IN_BYTES/INT8(1024), STR )
      CALL SETENV   ( 'GOMP_STACKSIZE'//CHAR(0), TRIM(STR)//CHAR(0), %VAL(1) )
!
      CALL PRE_PROG()
      INCLUDE 'elim_version.i' ! Set revision date of the current version
      CALL SET_PATHS() ! Setting environment variables to PGPLOT and SOLVE_HELP_DIR
      CALL ERR_MODE ( 'NO_PROBE' )
!
! --- Reading buffer in order to learn the mode
!
      CALL USE_BUFFER ( IBUFF, INT2(64), 'ORC' )
      IF ( IBUFF(1) .EQ. INT2(1) ) THEN
           UPWEI_BATCH = .TRUE.
         ELSE IF ( IBUFF(1) .EQ. INT2(2) ) THEN
           UPWEI_BATCH = .TRUE.
           CALL CLRCH ( PARU_FILE )
           PARU_FILE = CBUFF(3:)
         ELSE
           UPWEI_BATCH = .FALSE.
           CALL UN_CURSES () ! Eliminate the influence of the curses
      END IF
!
      VER_ELIM = GET_VERSION ()
      VER_MILE = 'MILE'//VER_ELIM(5:)
      DBOBJ%STATUS = DBOBJ__UNF
!
! --- Open files needed for MANOR
!
      CALL MANOR_OPEN()
      FAST_COV__SAV = FAST_COV  !  Saving covariance mode flag
!
      IUER     = -1
      KG_RJC   =  0
      KG_RST   =  0
      ML_OBSER = -1
      B3DOBJ%MEM_STAT = F__UND
      EQUMEM_INIT_WAS = .FALSE.
      FL_ERROR = .FALSE.
!
! --- Learn NUMDB  -- the number of database treated by LOOP now
! ---       LDBNAM -- data base name
! ---       IDBV   -- data base version (in 1-st element of array)
! ---       IDBE   -- number of observations (in 1-st element of array)
!
      CALL DBPOX ( NUMDB, LDBNAM, IDBV, IDBE )
!
      N_OBS = 0
      CALL CLRCH ( DBNAME )
!
      IF ( NUMDB .EQ. 0 ) THEN
!
! -------- No databases chosen
!
           CALL ERR_LOG ( 6601, IUER, 'ELIM', 'No database has been '// &
     &         'read from database or scratch file' )
           FL_ERROR = .TRUE.
           GOTO 810
         ELSE
!
! -------- Scan databases in scratch file
!
           DO 410 J1=1,NUMDB
              IF ( KBIT ( IDBSEL, INT2(J1) ) ) THEN
                   IF ( N_OBS .GT. 0 ) THEN
                        CALL ERR_LOG ( 6602, IUER, 'ELIM', 'More than one '// &
     &                      'database have been included in solution. Please '// &
     &                      'reduce the number of databases included in '// &
     &                      'solution  to 1' )
                        FL_ERROR = .TRUE.
                        GOTO 810
                   END IF
!
! ---------------- Determine IDBF -- index of the first observation for this
! ---------------- database in SCRATCH file, N_OBS -- the number of observations
! ---------------- in scratch file, IDB2 -- the index of the database in the
! ---------------- list of databases in scratch file
!
                   IF ( J1 .EQ. 1 ) THEN
                        IDBF  = 1
                      ELSE
                        IDBF  = IDBE(J1-1)
                   END IF
                   N_OBS = IDBE(J1) - IDBF + 1
                   IDB2 = J1
!
! ---------------- Form the name of the analyzed database
!
                   DBNAME(1:12) = CDBNAM(J1)(1:10)//' <'
                   CALL INCH ( INT4(IDBV(J1)), DBNAME(13:) )
                   DBNAME( ILEN(DBNAME)+1: ) = '>'
              END IF
 410       CONTINUE
      END IF
!
      IF ( N_OBS .EQ. 0 ) THEN
           CALL ERR_LOG ( 6603, IUER, 'ELIM', 'No one database have been '// &
     &         'included in solution' )
           FL_ERROR = .TRUE.
           GOTO 810
      END IF
!
! --- Read SOCOM and extend it
!
      CALL USE_COMMON ( 'ORC' )
      CALL SOCOM_EXT()
      F_UPDATE  = .FALSE.
      UPWEI_WAS = .FALSE.
!
      CALL USE_GLBFIL   ( 'OR' )
      CALL USE_GLBFIL_2 ( 'R'  )
      CALL USE_GLBFIL_3 ( 'R'  )
      CALL USE_GLBFIL_4 ( 'R' )
      VTD_ADR = 0
      CALL USE_GLBFIL_4 ( 'WC' )
!
! --- Checking fast mode
!
      IF ( FAST_MODE .NE. F__B3D   .AND.   FAST_MODE .NE. F__B1B3D ) THEN
           CALL CLRCH ( STR )
           IP = IFIND_PL ( FM_VAR, FM_VAL, FAST_MODE )
           IF ( IP .LE. 0 ) THEN
                STR = 'Undefined'
              ELSE
                STR = FM_STR ( IP )
           END IF
           CALL ERR_LOG ( 6604, IUER, 'ELIM', 'FAST mode "'//STR(1:I_LEN(STR))// &
     &         '" is not supported by ELIM. Only B3D mode is supported. '// &
     &         'Please change the fast mode for processing the database '// &
     &          DBNAME )
           FL_ERROR = .TRUE.
           GOTO 810
      END IF
!
! --- Checking solution type
!
      IF ( DATYP_INQ ( IDATYP, RATE__DTP ) ) THEN
           CALL CLRCH      ( STR )
           CALL DATYP_SHOW ( IDATYP, STR )
           CALL ERR_LOG    ( 6605, IUER, 'ELIM', 'Solution type "'// &
     &          STR(1:I_LEN(STR))//'" is not supported by ELIM since it '// &
     &         'assumes using daly rates. Please change solution type' )
           FL_ERROR = .TRUE.
           GOTO 810
      END IF
!
! --- Getting current date and time
!
      CALL FDATE ( DATE_RAW )
!
! --- ... and convert it to the international DEC format
!
      CALL TRAN  ( 11, DATE_RAW, DATE_RAW )
      CALL CLRCH ( DATE )
      IF ( DATE_RAW(9:9) .EQ. ' ' ) DATE_RAW(9:9) = '0'
      DATE = DATE_RAW(12:19)//' '// &
     &       DATE_RAW(9:10)//'-'//DATE_RAW(5:7)//'-'//DATE_RAW(23:24)
!
! --- Getting host name
!
      CALL CLRCH       ( HOSTNAME )
      CALL GETHOSTNAME ( HOSTNAME, %VAL(LEN(HOSTNAME)) )
      CALL CHASHL      ( HOSTNAME )
!
! --- Printing in spooll file information message
!
      WRITE ( 23, FMT='(A)' ) VER_ELIM//'   Utility for automatic '// &
     &                        'outliers elimination/restoration'
      WRITE ( 23, FMT='(A)' ) 'ELIM  ran on '//HOSTNAME(1:I_LEN(HOSTNAME))// &
     &                        '  at '//DATE//' by '//PRE_LETRS
!
! --- Some initializations
!
      F_OPTIN     = .FALSE.
      F_CHI       = .FALSE.
      DBOBJ%NAME  = DBNAME
      DBOBJ%L_OBS = N_OBS
      KG_RJC = 0
      KG_RST = 0
!
 910  CONTINUE
      DO WHILE ( .NOT. F_OPTIN )
         IF ( ( DATYP_INQ ( IDATYP, G_GXS__DTP ) .OR. &
     &          DATYP_INQ ( IDATYP, P_PXS__DTP ) .OR. &
     &          DATYP_INQ ( IDATYP, PX_GS__DTP ) .OR. &
     &          DATYP_INQ ( IDATYP, PS_GS__DTP )      ) ) THEN
              CONTINUE
            ELSE
!
! ------------- Suppress flag of automatic attempt to resolve ambiguity in
! ------------- restoration mode
!
                ELIM_AMB = .FALSE.
                ELIM_ION = .FALSE.
         END IF
         IF ( UPWEI_BATCH ) THEN
              F_OPTIN  = .FALSE.
              F_SAVE   = .FALSE.
              F_UPDATE = .FALSE.
              F_CHI    = .FALSE.
              F_UPWEI  = .TRUE.
!
! ----------- Setting some parameters which are relevant only for batch UPWEI
!
              ELIM_VRB = 0
              REWAY_VERBOSE = 0
              REWAY_CHITOL = UPWEI_CHITOL__BATCH
              IF ( DATYP_INQ ( IDATYP, PHASE__DTP ) ) THEN
                   REWAY_FLODEL = UPWEI_FLOOR_PHASE__BATCH
                 ELSE
                   REWAY_FLODEL = UPWEI_FLOOR_GROUP__BATCH
              END IF
            ELSE
!
! ----------- Calling menu. Parameters of the ELIM (variables with prefix ELIM)
! ----------- may change their values.
! ----------- Flags F_OPTIN, F_SAVE, F_UPDATE may bve set up
! ----------- F_OPTIN -- exit from the program
! ----------- F_SAVE  -- suboption of the exit:
! -----------            F_SAVE = .TRUE. --  downweight flag if scratch file
! -----------                                will be updated,
! -----------            F_SAVE = .FALSE. -- not to do it. Results of the work
! -----------                                of ELIM/MILE will be lost in
! -----------                                that case
! ----------- F_UPDATE -- make only update of the statistics but not
! -----------             elimination/restoration
!
              CALL ERR_PASS  ( IUER, IER )
              CALL ELIM_MENU ( VER_ELIM, VER_MILE, N_OBS, DBOBJ, &
     &                    %VAL(IADR_OBSSCA), %VAL(IADR_OBSSCA), &
     &                    %VAL(IADR_OBSBAS), %VAL(IADR_RES), RST, CHIOBJ, &
     &                    ELIM_MOD, ELIM_THR, ELIM_CUT, ELIM_MSR, ELIM_TYP, &
     &                    ELIM_VRB, ELIM_CNF, ELIM_UPD, ELIM_AMB, ELIM_ION, &
     &                    QUALCODE_GOOD_LIM, SUPMET, EQUMEM_FLAG, &
     &                    SNGCHK_ACTION, SNGCHK_SOUMIN, SNGCHK_STAMIN, &
     &                    SNGCHK_BASMIN, F_CHI, F_OPTIN, F_SAVE, F_UPDATE, &
     &                    F_UPWEI, F_SUPSTAT, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 6606, IUER, 'ELIM', 'Error during work '// &
     &                 'of ELIM_MENU detected while database '//DBNAME// &
     &                 ' was processing' )
                   FL_ERROR = .TRUE.
                   GOTO 810
              END IF
         END IF
!
         CALL USE_COMMON   ( 'OWC' )   !  Storing the results of menu changes
         CALL USE_GLBFIL_4 ( 'OWC' )   !  Storing the results of menu changes
         IF ( F_OPTIN ) GOTO 810       !  End of work
!
         IF ( EQUMEM_FLAG ) THEN
              IF ( .NOT. EQUMEM_INIT_WAS ) THEN
!
! ---------------- Initialize EQUMEM data structure if EQUMEM_FLAG is set and
! ---------------- we have not initialized this data structure before
!
                   CALL ERR_PASS    ( IUER, IER )
                   CALL EQUMEM_INIT ( EQUMEM, IER )
                   IF ( IER .NE. 0 ) THEN
                        CALL ERR_LOG ( 6607, IUER, 'ELIM', 'Error during '// &
     &                      'attempt to initiialize EQUMEM data strucutre' )
                        FL_ERROR = .TRUE.
                        GOTO 810
                   END IF
                   EQUMEM_INIT_WAS = .TRUE.
              END IF
            ELSE
              EQUMEM%USE_FLAG = .FALSE.
         END IF
!
         IF ( .NOT. F_SUPSTAT                      .AND. &
     &              DBOBJ%STATUS .NE. DBOBJ__DON          ) THEN
              IF ( ELIM_VRB .GE. 1 ) THEN
                   WRITE ( 6, FMT='(A)' ) 'Database '//DBNAME//' is being read'
              END IF
!
! ----------- Switching mode of calculation of the covariance matrix to
! ----------- "FULL"
!
              FAST_COV = F__FUL
              CALL USE_GLBFIL_4 ( 'OWC' )
!
! ----------- Scanning of the observations and calculation some statistics
! ----------- of the sessions, building the lists of the objects. This
! ----------- information will be used for calculation amount of dynamic
! ----------- memory needed for temorary bypass data structures
!
              CALL ERR_PASS ( IUER, IER )
              CALL DB_SCAN  ( DBNAME, IDB2, IDBF, N_OBS, DBOBJ, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 6608, IUER, 'ELIM', 'Error during '// &
     &                 'initialization of data structure detected while '// &
     &                 'database '//DBNAME//' was processing' )
                   FL_ERROR = .TRUE.
                   GOTO 810
              END IF
              IF ( DBOBJ%L_SCA .LE. 2 ) THEN
                   CALL ERR_LOG ( 6609, IUER, 'ELIM', 'Trap of internal '// &
     &                 'control: too few scans were found while database '// &
     &                 DBNAME//' was processing' )
                   FL_ERROR = .TRUE.
                   GOTO 810
              END IF
              IF ( DBOBJ%L_STA .LT. 2 ) THEN
                   WRITE ( 6, * ) ' DBOBJ%L_STA = ', DBOBJ%L_STA
                   CALL ERR_LOG ( 6610, IUER, 'ELIM', 'Trap of internal '// &
     &                 'control: too few stations were found while database '// &
     &                 DBNAME//' was processing' )
                   FL_ERROR = .TRUE.
                   GOTO 810
              END IF
!
              IF ( ELIM_VRB .GE. 1 ) THEN
                   WRITE ( 6, FMT='(A)' ) 'Database '//DBNAME//' has been read'
              END IF
!
              IF ( DBOBJ%U_OBS .LE. 0 ) THEN
                   CALL ERR_LOG ( 6611, IUER, 'ELIM', 'No one observation '// &
     &                 'which can be used in solution were found in database '// &
     &                 DBNAME//' . Check whether ionosphere correction has '// &
     &                 'been calculated and applied' )
                   FL_ERROR = .TRUE.
                   GOTO 810
              END IF
!
! ----------- Make singularity check test
!
              CALL ERR_PASS   ( IUER, IER )
              CALL ELIM_PARAM ( DBOBJ, ICMPL, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 6612, IUER, 'ELIM', 'Error during '// &
     &                 'parameterization check' )
                   FL_ERROR = .TRUE.
                   GOTO 810
              END IF
              IF ( ICMPL .NE. 0 ) GOTO 810
!
! ----------- Make parameterization check
!
              CALL ERR_PASS ( IUER, IER )
              CALL SNGCHK ( DBOBJ, -1, R_SOU, RIS_SOU, R_STA, RIS_STA, R_BAS, &
     &                      RIS_BAS, SNGCHK_CMP, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 6613, IUER, 'ELIM', 'Error during '// &
     &                 'singularity check' )
                   FL_ERROR = .TRUE.
                   GOTO 810
              END IF
!
! ----------- Grabbing dynamic memory
!
              CALL ERR_PASS ( IUER, IER )
              CALL GRAB_MEM ( IER,  ML_OBSER,                       MA_OBSER,    4, &
     &                        INT8(ML_SCA*DBOBJ%L_SCA),             IADR_OBSSCA, &
     &                        INT8(ML_STA*DBOBJ%L_STA*DBOBJ%L_SCA), IADR_OBSSTA, &
     &                        INT8(ML_BAS*N_OBS),                   IADR_OBSBAS, &
     &                        INT8( ML_RES*N_OBS),                  IADR_RES     )
!
              IF ( IER .NE. 0 ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( ML_SCA*DBOBJ%L_SCA             + &
     &                          ML_STA*DBOBJ%L_STA*DBOBJ%L_SCA + &
     &                          ML_BAS*N_OBS, &
     &                          STR )
                   CALL ERR_LOG ( 6614, IUER, 'ELIM', 'Error during the '// &
     &                 'attempt to grab '//STR(1:I_LEN(STR))//' bytes of '// &
     &                 'dynamic memory for allocation internal data '// &
     &                 'structures while database '//B3DOBJ%DBNAME_MES// &
     &                 ' was processing' )
                   FL_ERROR = .TRUE.
                   GOTO 810
              END IF
!
! ----------- Zeroing entire pool of grabbed dynamic memory
!
              CALL NOUT ( ML_OBSER, %VAL(MA_OBSER) )
!
! ----------- Unprotecting all observations (protecting means that some
! ----------- observations may be barred from elimination or restoration)
!
              CALL UNPROT_RES ( N_OBS, %VAL(IADR_RES) )
!
              DBOBJ%F_AMB = ELIM_AMB
              DBOBJ%F_ION = ELIM_ION
              DBOBJ%F_AMB_CHANGED = .FALSE.
!
! ----------- Making initial LSQ solution. Estimates and FULL covariance matrix
! ----------- are calculated. Postfit residuals and their statistics are also
! ----------- calculated and stored in temporary data structures
!
              CALL ERR_PASS ( IUER, IER )
              CALL SOL_INIT ( ELIM_VRB, 0, 1, N_OBS, DBOBJ%L_STA, DBOBJ%L_SCA, &
     &             IDB2, IDBF, PLACE, B3DOBJ, B1B3DOBJ, OBSHLD, DBOBJ, NCREC, &
     &             %VAL(IADR_OBSSCA), %VAL(IADR_OBSSTA), %VAL(IADR_OBSBAS), &
     &             %VAL(IADR_RES), RST, CHIOBJ, EQUMEM, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 6615, IUER, 'ELIM', 'Error during the '// &
     &                 'attempt to obtain initial solution while database '// &
     &                 B3DOBJ%DBNAME_MES//' was processing' )
                   FL_ERROR = .TRUE.
                   GOTO 810
              END IF
              F_CHI = .TRUE.    ! Flag: chi/ndg has been calculated
          END IF ! DBNAME.STATUS .NE. DBNAME__DON
!
          IF ( .NOT. F_UPDATE  .AND. &
     &         .NOT. ELIM_MOD  .AND. &
     &         .NOT. F_UPWEI   .AND. &
     &         .NOT. F_SUPSTAT        ) THEN
!
! ------------ Preparing messages which will be put into scratch file
!
               CALL CLRCH ( STR )
               WRITE ( UNIT=STR, FMT='(F9.2)' ) ELIM_CUT
               IF ( STR(I_LEN(STR):I_LEN(STR)) .EQ. '0' ) &
     &         STR(I_LEN(STR):I_LEN(STR)) = ' '
               CALL CHASHL ( STR )
               STR = STR(1:I_LEN(STR))//' sigma'
!
               CALL CLRCH ( STR1 )
               IF ( ELIM_THR .GT. 1.D-13 ) THEN
                    WRITE ( UNIT=STR1, FMT='(F8.1)' ) ELIM_THR*1.D12
                    IF ( STR1(I_LEN(STR1):I_LEN(STR1)) .EQ. '0' ) &
     &              STR1(I_LEN(STR1):I_LEN(STR1)) = ' '
                    CALL CHASHL ( STR1 )
                    STR1 = STR1(1:I_LEN(STR1))//' psec'
                ELSE
                    STR1 = '"not specified"'
               END IF
!
               CALL CLRCH ( STR2 )
               IF ( ELIM_TYP .EQ. 'GL' ) STR2='"Global"'
               IF ( ELIM_TYP .EQ. 'BA' ) STR2='"Baseline"'
!
               WRITE ( 23, FMT='(A)' ) 'ELIM  Threshold = '// &
     &                                  STR1(1:I_LEN(STR1))// &
     &                                 ',  Cutoff = '//STR(1:I_LEN(STR))// &
     &                                 ',  Type = '//STR2(1:I_LEN(STR2))
!
               CALL CLRCH ( STR )
               CALL INCH  ( QUALCODE_GOOD_LIM, STR )
               WRITE ( 23, FMT='(A)' ) 'ELIM  Qualcode_good_lim = '// &
     &                                 STR(1:I_LEN(STR))
          END IF
!
          IF ( .NOT. F_UPDATE  .AND. &
     &               ELIM_MOD  .AND. &
     &         .NOT. F_UPWEI   .AND. &
     &         .NOT. F_SUPSTAT         ) THEN
!
               WRITE ( 23, FMT='(A)' ) 'ELIM  ran in "elimination" mode'
!
! ------------ Executing iterative outliers elimination
!
               CALL ERR_PASS ( IUER, IER )
               L_SCA = DBOBJ%L_SCA 
               L_STA = DBOBJ%L_STA
               CALL ELIM_DO  ( IDB2, IDBF, N_OBS, L_SCA, L_STA, OBSHLD, &
     &              DBOBJ, NCREC, %VAL(IADR_OBSSCA), %VAL(IADR_OBSSTA), &
     &              %VAL(IADR_OBSBAS), %VAL(IADR_RES), RST, &
     &              PLACE, B3DOBJ, B1B3DOBJ, CHIOBJ, EQUMEM, &
     &              ELIM_MOD, ELIM_TYP, ELIM_CNF, ELIM_VRB, ELIM_THR, ELIM_CUT, &
     &              ELIM_MSR, ELIM_UPD, K_RJC, IER )
               IF ( IER .NE. 0 ) THEN
                    CALL ERR_LOG ( 6616, IUER, 'ELIM', 'Error during '// &
     &                 'elimination of the outliers' )
                    FL_ERROR = .TRUE.
                    GOTO 810
               END IF
!
               CALL PRCH ( 'Hit any key to proceed '//CHAR(1) )
!
! ------------ Awaiting for the reaction of the user
!
               CALL INSIM ( ASIM, ISIM )
!
               KG_RJC = KG_RJC + K_RJC
          END IF
!
          IF ( .NOT. F_UPDATE  .AND. &
     &         .NOT. ELIM_MOD  .AND. &
     &         .NOT. F_UPWEI   .AND. &
     &         .NOT. F_SUPSTAT        ) THEN
!
               WRITE ( 23, FMT='(A)' ) 'ELIM  ran in "restoration" mode'
!
! ------------ Executing iterative restoration of the observations prevously
! ------------ eliminated from the solution
!
               CALL ERR_PASS ( IUER, IER )
               CALL MILE_DO  ( IDB2, IDBF, N_OBS, DBOBJ%L_SCA, DBOBJ%L_STA, &
     &              OBSHLD, DBOBJ, NCREC, %VAL(IADR_OBSSCA), %VAL(IADR_OBSSTA), &
     &              %VAL(IADR_OBSBAS), %VAL(IADR_RES), RST, &
     &              PLACE, B3DOBJ, B1B3DOBJ, CHIOBJ, EQUMEM, &
     &              ELIM_MOD, ELIM_TYP, ELIM_CNF, ELIM_VRB, ELIM_THR, ELIM_CUT, &
     &              ELIM_MSR, ELIM_UPD, ELIM_AMB, ELIM_ION, K_RST, IER )
               IF ( IER .NE. 0 ) THEN
                    CALL ERR_LOG ( 6617, IUER, 'ELIM', 'Error during '// &
     &                 'restoration of the observations' )
                    FL_ERROR = .TRUE.
                    GOTO 810
               END IF
               CALL PRCH ( 'Hit any key to proceed '//CHAR(1) )
!
! ------------ Awaiting for the reaction of the user
!
               CALL INSIM ( ASIM, ISIM )
               KG_RST = KG_RST + K_RST
          END IF
          IF ( F_UPWEI ) THEN
!
! ------------ Weights update
!
               UPWEI_WAS = .TRUE.
               CALL ERR_PASS ( IUER, IER )
               CALL UPWEI ( IDB2, IDBF, N_OBS, DBOBJ%L_SCA, DBOBJ%L_STA, &
     &              OBSHLD, DBOBJ, NCREC, %VAL(IADR_OBSSCA), %VAL(IADR_OBSSTA), &
     &              %VAL(IADR_OBSBAS), %VAL(IADR_RES), RST, &
     &              PLACE, B3DOBJ, B1B3DOBJ, CHIOBJ, EQUMEM, F_CHI, &
     &              UPWEI_BATCH, IER )
               IF ( IER .NE. 0 ) THEN
                    CALL ERR_LOG ( 6618, IUER, 'ELIM', 'Error during '// &
     &                 'updating weights' )
                    FL_ERROR = .TRUE.
                    GOTO 810
               END IF
               IF ( UPWEI_BATCH ) GOTO 810
          END IF
          IF ( F_SUPSTAT ) THEN
               IF ( ELIM_VRB .GE. 1 ) THEN
                    WRITE ( 6, FMT='(A)' ) 'Suppression status for '// &
     &                    'observations from '//DBNAME//' is being updated'
               END IF
!
! ------------ Update of suppression status for the changes of suppression
! ------------ method or quality code limit
!
               CALL ERR_PASS ( IUER, IER )
               F_SUP = SUP_UPDATE ( N_OBS, DBOBJ, %VAL(IADR_OBSSCA), &
     &                              %VAL(IADR_OBSBAS), QUALCODE_GOOD_LIM, IER )
               IF ( IER .NE. 0 ) THEN
                    CALL ERR_LOG ( 6619, IUER, 'ELIM', 'Error during '// &
     &                 'update of suppression status' )
                    FL_ERROR = .TRUE.
                    GOTO 810
               END IF
!
               IF ( F_SUP ) THEN
!
! ----------------- Status of observation: used or not used in solution was
! ----------------- changed.
!
! ----------------- Making initial LSQ solution. Estimates and FULL covariance
! ----------------- matrix are calculaterd. Postfit residuals and their
! ----------------- statistics are also calculated and stored in temporary
! ----------------- data structures
!
                    CALL ERR_PASS ( IUER, IER )
                    CALL SOL_INIT ( ELIM_VRB, 1, 1, N_OBS, DBOBJ%L_STA, &
     &                   DBOBJ%L_SCA, IDB2, IDBF, PLACE, B3DOBJ, B1B3DOBJ, &
     &                   OBSHLD, DBOBJ, NCREC, %VAL(IADR_OBSSCA), &
     &                   %VAL(IADR_OBSSTA), %VAL(IADR_OBSBAS), %VAL(IADR_RES), &
     &                   RST, CHIOBJ, EQUMEM, IER )
                    IF ( IER .NE. 0 ) THEN
                         CALL ERR_LOG ( 6620, IUER, 'ELIM', 'Error during '// &
     &                       'the attempt to obtain initial solution while '// &
     &                       'database '//B3DOBJ%DBNAME_MES//' was processing' )
                         FL_ERROR = .TRUE.
                         GOTO 810
                    END IF
                    F_CHI = .TRUE.  ! Flag: chi/ndg has been calculated
                  ELSE
!
! ----------------- If we don't need to update solution then we should at least
! ----------------- update residuals
!
                    CALL ERR_PASS ( IUER, IER )
                    CALL RESID_ST ( .FALSE., .FALSE., ELIM_THR, ELIM_CUT, &
     &                              ELIM_MSR, 0, N_OBS, DBOBJ, &
     &                              %VAL(IADR_OBSSCA), %VAL(IADR_OBSBAS), &
     &                              %VAL(IADR_RES), RST, IER )
                    IF ( IER .NE. 0 ) THEN
                         CALL ERR_LOG ( 6621, IUER, 'ELIM', 'Error during '// &
     &                       'calculation statisics for the postfit '// &
     &                       'residuals while database '//B3DOBJ%DBNAME_MES// &
     &                       ' was processing' )
                         FL_ERROR = .TRUE.
                         GOTO 810
                    END IF
               END IF
          END IF
      END DO
!
! --- END OF CYCLE...
!
 810  CONTINUE
      IF ( FL_ERROR .AND. UPWEI_BATCH ) THEN
           CALL ERR_LOG ( 6622, IUER, 'ELIM', 'Execution of ELIM while '// &
     &         'for processing the database '//B3DOBJ%DBNAME_MES//' was '// &
     &         'abnormally terminatated due to the error. No records were '// &
     &         'added to the weights file' )
           CALL GETENVAR ( 'ELIM_ERROR_IGNORE', STR )
           IF ( STR(1:1) .EQ. 'Y' .OR. STR(1:1) .EQ. 'y' ) THEN
                CALL END_PROG()
                CALL EXIT ( 0 )
              ELSE
                CALL EXIT ( 1 )
           END IF
      END IF
!
! --- Do we need the results of our work?
!
      IF ( F_SAVE   .AND.   ( KG_RJC .NE. 0  .OR.  KG_RST .NE. 0 ) ) THEN
!
! -------- Updating the scratch file in order to save results of our work
!
           CALL PRCH ( 'Updating scratch file is in progress ...'//CHAR(1) )
           CALL ELIM_SAVE ( N_OBS, IDB2, IDBF, DBOBJ, %VAL(IADR_RES), &
     &                      %VAL(IADR_OBSBAS) )
         ELSE IF ( ( KG_RJC .NE. 0  .OR.  KG_RST .NE. 0 ) .AND. &
     &             .NOT. UPWEI_BATCH ) THEN
           CALL PRCH ( 'Unweight flag for some observations has been changed' )
           CALL PRCH ( CHAR(13)//CHAR(10) )
           CALL PRCH ( 'Are you really going to discard these changes '// &
     &                 ' (Y/N) [Y]  ? '//CHAR(1) )
!
! -------- Awaiting for the user's reply
!
           CALL INSIM ( ASIM, ISIM )
           CALL TRAN  ( 11, ASIM, ASIM )
           IF ( ASIM .EQ. 'N' ) THEN
                F_OPTIN = .FALSE.
                GOTO 910
           END IF
      END IF
!
      IF ( F_SAVE   .AND.   ( KG_RJC .NE. 0  .OR.  KG_RST .NE. 0 ) ) THEN
!
! -------- If the number of parameters has been changed then we should
! -------- prepare B3D data structures for calling CRES which will be called
! -------- from OPTIN after termination ELIM
!
           CALL ERR_PASS ( IUER, IER )
           CALL ELIM_WRIEST ( B3DOBJ, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 6623, IUER, 'ELIM', 'Error during attempt '// &
     &              'to write down updated covarianve matrix and vector '// &
     &              'of the estiamtes for allocation full normal matrix '// &
     &              'while database '//B3DOBJ%DBNAME_MES//' was processing' )
                CALL HIT_CONT ( %VAL(0), %VAL(0) )
           END IF
           WRITE ( 23, FMT='(A)' ) 'ELIM  Unweight flags are updated in '// &
     &                             'scratch area'
         ELSE
           WRITE ( 23, FMT='(A)' ) 'ELIM  Unweight flags have not been '// &
     &                             'updated in scratch area'
      END IF
!
      IF ( EQUMEM_INIT_WAS ) THEN
           CALL ERR_PASS   ( IUER, IER )
           CALL EQUMEM_END ( EQUMEM, IER )
           IF ( IER .NE. 0 ) THEN
                 CALL ERR_LOG ( 6624, IUER, 'ELIM', 'Error during '// &
     &               'attempt to free dynamic memory occupied by EQUMEM '// &
     &               'data strucutre' )
                 CALL HIT_CONT ( %VAL(0), %VAL(0) )
           END IF
      END IF
!
! --- Freeing dynamic memory allocated for fields of B3DOBJ
!
      IF ( B3DOBJ%MEM_STAT .EQ. F__MSL  .OR. &
     &     B3DOBJ%MEM_STAT .EQ. F__MFL       ) THEN
           CALL B3D_FREEMEM ( B3DOBJ, IER )
      END IF
      IF ( ML_OBSER .GT. 0 ) THEN
           CALL FREE_MEM ( MA_OBSER )
           ML_OBSER = 0
      END IF
!
      FAST_COV = FAST_COV__SAV
      CALL USE_GLBFIL_4 ( 'OWC' )
!
      IF ( UPWEI_BATCH ) THEN
           CONTINUE
         ELSE
           IF ( UPWEI_WAS .OR. ( F_SAVE .AND. &
     &                         ( KG_RJC .NE. 0  .OR.  KG_RST .NE. 0 ) ) ) THEN
!
! ------------- Some changes occured either with the status of points
! ------------- (unweight flag) or with weights correction
!
                CALL USE_COMMON   ( 'ORC' )
!
! ------------- Set flag: "database IDB2 has not been analyzed" to prevent
! ------------- failure of REWAY if it is called just after ELIM
!
                CALL SBIT ( IDBEST, IDB2, INT2(0) )
                CALL USE_COMMON   ( 'OWC' )
!
! ------------- Setting flag in inter-program buffer: "Call CRES after ELIM"
!
                CALL USE_BUFFER ( INT2(1), INT2(1), 'OWC'  )
             ELSE
!
! ------------- No changes occured. Setting flag in inter-program buffer:
! ------------ "Not to call CRES after ELIM"
!
               CALL USE_BUFFER ( INT2(0), INT2(1), 'OWC'  )
           END IF
!
! -------- Good bye!
!
           IF ( IUER .NE. -1 ) THEN
                CALL HIT_CONT ( %VAL(0), %VAL(0) )
           END IF
      END IF
      CALL END_PROG()
!
      END  SUBROUTINE  ELIM  !#!#
