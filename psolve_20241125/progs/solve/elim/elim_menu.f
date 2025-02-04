      SUBROUTINE ELIM_MENU ( VER_ELIM, VER_MILE, N_OBS, &
     &                       DBOBJ, OBSSCA, OBSSTA, OBSBAS, RES, RST, CHIOBJ, &
     &                       ELIM_MOD, ELIM_THR, ELIM_CUT, ELIM_MSR, ELIM_TYP, &
     &                       ELIM_VRB, ELIM_CNF, ELIM_UPD, ELIM_AMB, ELIM_ION, &
     &                       QUALCODE_GOOD_LIM, SUPMET, EQUMEM_FLAG, &
     &                       SNGCHK_ACTION, SNGCHK_SOUMIN, SNGCHK_STAMIN, &
     &                       SNGCHK_BASMIN, F_CHI, &
     &                       F_OPTIN, F_SAVE, F_UPDATE, F_UPWEI, F_SUPSTAT, &
     &                       IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  ELIM_MENU  gets parameters for outliers elimination/      *
! *   restoration procedure in interactive mode.                         *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *  VER_ELIM ( CHARACTER ) -- String with ELIM-identifier and number    *
! *                            of the current version.                   *
! *  VER_MILE ( CHARACTER ) -- String with MILE-identifier and number    *
! *                            of the current version.                   *
! *     N_OBS ( INTEGER*4 ) -- Total number of observations in the       *
! *                            session.                                  *
! *     DBOBJ ( RECORD    ) -- Data structure which keeps general        *
! *                            information about the database such as    *
! *                            lists of the objects.                     *
! *    OBSSCA ( RECORD    ) -- Array of data structures which keeps      *
! *                            scan-dependent information about the      *
! *                            session.                                  *
! *    OBSSTA ( RECORD    ) -- Array of data structures which keeps      *
! *                            station dependent information about the   *
! *                            session.                                  *
! *    OBSBAS ( RECORD    ) -- Array of data structures which keeps      *
! *                            baseline dependent information about the  *
! *                            session.                                  *
! *       RES ( RECORD    ) -- Array of data structures keeping          *
! *                            information about residuals.              *
! *       RST ( RECORD    ) -- Data structure keeping the statistics of  *
! *                            postfit residuals.                        *
! *    CHIOBJ ( RECORD    ) -- Object with data structure for keeping    *
! *                            accumulators of the chi-squares and their *
! *                            mathematical expectations.                *
! *     F_CHI ( LOGICAL*4 ) -- Flag: .TRUE. means that the ratio of      *
! *                            chi-square to its mathematical            *
! *                            expectation has been calculated and       *
! *                            available.                                *
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
! *  ELIM_MOD ( LOGICAL*4 ) -- mode switch. .TRUE. if ELIM is called in  *
! *                            elimination mode, .FALSE. if called in    *
! *                            restoration mode.                         *
! *  ELIM_THR ( REAL*8    ) -- Postfit threshold (sec). ELIM mode:       *
! *                            If used observation with postfit          *
! *                            residual exceeds ELIM_THR it marked as    *
! *                            outlier. MILE mode: if recoverable        *
! *                            observation has postfit residual less     *
! *                            than ELIM_THR it marked as candidate in   *
! *                            restoration.                              *
! *  ELIM_CUT ( REAL*8    ) -- n-sigmas cutoff criterion. ELIM mode:     *
! *                            If used observation has normalized        *
! *                            postfit residual larger than ELIM_CUT it  *
! *                            marked as outlier. MILE mode: if          *
! *                            recoverable observation has normalized    *
! *                            postfit residual less than ELIM_THR it    *
! *                            marked as candidate in restoration.       *
! *  ELIM_MSR ( REAL*8    ) -- Maximum uncertainty. Observations with    *
! *                            formal uncertainty exceeding ELIM_MSR     *
! *                            are marked as outliers in ELIM mode and   *
! *                            are not eligible for restoration in MILE  *
! *                            mode. If ELIM_MSR < 1.D-13 then it is     *
! *                            ignored.                                  *
! *  ELIM_TYP ( CHARACTER ) -- Mode of normalization. Normalization of   *
! *                            "normalized postfit residuals" may be     *
! *                            done in two modes: 'global' mode when     *
! *                            dispersion of postfit residuals used for  *
! *                            normalization is calculated for all used  *
! *                            observations of the database or in        *
! *                            'baseline' mode when this dispersion is   *
! *                            calculated for used observations made at  *
! *                            the same baseline as the considered       *
! *                            observation. Two values are allowed:      *
! *                            'BA' or 'GL'.                             *
! *  ELIM_VRB ( REAL*8    ) -- Verbosity level. 0 -- silent mode. 1 --   *
! *                            verbose mode.                             *
! *  ELIM_CNF ( LOGICAL*4 ) -- Confirmation mode switch. If .TRUE. then  *
! *                            confirmation before each elimination/     *
! *                            restoration will be inquired.             *
! *  ELIM_UPD ( INTEGER*4 ) -- Parameter, showing how frequently         *
! *                            residuals will be updated. Residuals are  *
! *                            updated after ELIM_UPD operation of       *
! *                            elimination/restoration.                  *
! *  ELIM_AMB ( LOGICAL*4 ) -- If .TRUE. MILE when search of the best    *
! *                            candidate to restoration will resolve     *
! *                            ambiguity (group or phase in according    *
! *                            with solution type) and try to recover    *
! *                            observation with resolved ambiguity.      *
! *  ELIM_ION ( LOGICAL*4 ) -- If .TRUE. and ELIM_AMB is also .TRUE.     *
! *                            then MILE when search of the best         *
! *                            candidate to restoration will resolve     *
! *                            ambiguity (group or phase in according    *
! *                            with solution type) and take into account *
! *                            change of residuals for changes           *
! *                            ionosphere correction occurred because    *
! *                            change of ambiguity.                      *
! *  QUALCODE_GOOD_LIM ( INTEGER*4 ) -- Minimal value of quality code    *
! *                            when the observation consider as a good   *
! *                            one. Letter-like quality codes ( "A",     *
! *                            "B", "C", "D", "E" are considered as      *
! *                            negative and therefore bad ).             *
! *    SUPMET ( INTEGER*2 ) -- Code of strategy applied for deciding     *
! *                            which observation should be treated as    *
! *                            unrecoverable, conditionally bad,         *
! *                            conditionally good.                       *
! *   EQUMEM_FLAG ( LOGICAL*4 ) -- Flag whether to save equations of     *
! *                                conditions in memory (.TRUE.) or      *
! *                                to recompute them a new each time     *
! *                                (.FALSE.)                             *
! * SNGCHK_ACTION ( INTEGER*4 ) -- Code of the action to be done when    *
! *                            sessions fails singularity test. Possible *
! *                            actions:                                  *
! *                            SNGCHK_ACT__NONE -- do nothing;           *
! *                            SNGCHK_ACT__WARN -- issue warning;        *
! *                                             execution will continue; *
! *                            SNGCHK_ACT__REPR -- make                  *
! *                                   reparameterization and then        *
! *                                   execution will continue;           *
! *                            SNGCHK_ACT__STOP -- stop execution;       *
! *                            SNGCHK_ACT__SKIP -- skip session;         *
! *                                   (stop execution in ELIM).          *
! * SNGCHK_SOUMIN ( INTEGER*4 ) -- Criteria for singularity check.       *
! *                            if the number of used in estimation       *
! *                            observations of the source which          *
! *                            coordinates are adjusted is less than     *
! *                            this value, then singularity test is      *
! *                            considered as failed for this source.     *
! * SNGCHK_STAMIN ( INTEGER*4 ) -- Criteria for singularity check.       *
! *                            if the number of used in estimation       *
! *                            observations at the station is less than  *
! *                            this value, then singularity test is      *
! *                            considered as failed for this station.    *
! * SNGCHK_BASMIN ( INTEGER*4 ) -- Criteria for singularity check.       *
! *                            if the number of used in estimation       *
! *                            observations at the baseline is less than *
! *                            this value, then singularity test is      *
! *                            considered as failed for this baseline.   *
! *                                                                      *
! * ________________________ Output parameters: ________________________ *
! *                                                                      *
! *   F_OPTIN ( LOGICAL*4 ) -- Flag: .TRUE. means not to make elimination*
! *                            restoration but go back to OPTIN outright *
! *                            without saving results.                   *
! *    F_SAVE ( LOGICAL*4 ) -- Flag: .TRUE. means not to make elimination*
! *                            restoration but go back to OPTIN outright *
! *                            and to save results by updating           *
! *                            downweight flag in scratch file.          *
! *  F_UPDATE ( LOGICAL*4 ) -- Flag: .TRUE. means not to make elimination*
! *                            restoration but only update residuals.    *
! *   F_UPWEI ( LOGICAL*4 ) -- Flag: .TRUE. means to call upweight       *
! *                            routine, not to make                      *
! *                            elimination/restoration.                  *
! * F_SUPSTAT ( LOGICAL*4 ) -- Flag: .TRUE. means to call routine for    *
! *                            change of suppression status since        *
! *                            suppression method has been modified or   *
! *                            quality code limit has been changed.      *
! *                                                                      *
! * _________________________ Modified parameters: _____________________ *
! *                                                                      *
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
! *  ###  17-SEP-97    ELIM_MENU   v2.12 (c)  L. Petrov 18-JUL-2007 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'solve.i'
      INCLUDE   'obser.i'
      INTEGER*4  N_OBS
      TYPE ( DBOBJ_O__STRU ) ::  DBOBJ
      TYPE ( SCA_O__STRU ) ::  OBSSCA(*)
      TYPE ( STA_O__STRU ) ::  OBSSTA(*)
      TYPE ( BAS_O__STRU ) ::  OBSBAS(N_OBS)
      TYPE ( RES_O__STRU ) ::  RES(N_OBS)
      TYPE ( RST_O__STRU ) ::  RST
      TYPE ( CHIACC__STRU ) ::  CHIOBJ
      CHARACTER  VER_ELIM*(*), VER_MILE*(*)
      LOGICAL*4  ELIM_MOD, ELIM_CNF, ELIM_AMB, ELIM_ION, EQUMEM_FLAG
      REAL*8     ELIM_THR, ELIM_CUT, ELIM_MSR
      INTEGER*4  ELIM_VRB, QUALCODE_GOOD_LIM, ELIM_UPD, &
     &           SNGCHK_ACTION, SNGCHK_SOUMIN, SNGCHK_STAMIN, SNGCHK_BASMIN, &
     &           IUER
      INTEGER*2  SUPMET
      CHARACTER  ELIM_TYP*2
      LOGICAL*4  F_CHI, F_OPTIN, F_SAVE, F_UPDATE, F_UPWEI, F_SUPSTAT
      CHARACTER  STR*120, OUT*120, STR1*80, CC4*4, SIM*1, SOLTYP*32, &
     &           SUPMET_OUT*16
      REAL*8     VAL, MIN_MSR
      PARAMETER  ( MIN_MSR = 1.D-12 ) ! min acceptable valuye of ELIM_MSR
      INTEGER*4  IX, IY, IP, I5, IS, IER, IVAL, LEN_RST, ELIM_VRB_MIN, ELIM_VRB_MAX
      PARAMETER  ( ELIM_VRB_MIN = 0, ELIM_VRB_MAX = 2 )
      INTEGER*2  INT2_ARG
      INTEGER*4  INT4
      INT4(INT2_ARG) = INT(INT2_ARG,KIND=4)
      LOGICAL*4, EXTERNAL :: DATYP_INQ
      INTEGER*4, EXTERNAL :: I_LEN, ILEN, OTL_LINE
!
! --- Initialization
!
      F_OPTIN   = .FALSE.
      F_SAVE    = .FALSE.
      F_UPDATE  = .FALSE.
      F_UPWEI   = .FALSE.
      F_SUPSTAT = .FALSE.
!
      IF ( DBOBJ%STATUS .EQ. DBOBJ__DON ) THEN
           IF ( .NOT. ( DATYP_INQ ( DBOBJ%IDATYP, G_GXS__DTP ) .OR. &
     &                  DATYP_INQ ( DBOBJ%IDATYP, P_PXS__DTP ) .OR. &
     &                  DATYP_INQ ( DBOBJ%IDATYP, PX_GS__DTP ) .OR. &
     &                  DATYP_INQ ( DBOBJ%IDATYP, PS_GS__DTP )      ) ) THEN
!
! ------------- Disabling forcibly possibility to resolve on the fly ambiguities
! ------------- if solution type is not appropriate.
!
                ELIM_AMB = .FALSE.
                ELIM_ION = .FALSE.
           END IF
      END IF
!
! --- Test of the length of RST data structure
!
      LEN_RST = (LOC(RST%LAST_FIELD) - LOC(RST%FIRST_FIELD)) + 4
      IF ( LEN_RST .NE. ML_RST ) THEN
           CALL CLRCH ( STR  )
           CALL INCH  ( ML_RST, STR )
           CALL CLRCH ( STR1 )
           CALL INCH  ( LEN_RST, STR1 )
           CALL ERR_LOG ( 6771, IUER, 'ELIM_MENU', 'Internal error: '// &
     &         'Declared size of RST data structure (obser.i) '// &
     &          STR(1:I_LEN(STR))//' doesn''t coincide with the '// &
     &         'actual size: '//STR1(1:I_LEN(STR1)) )
           RETURN
      END IF
!
! --- Start curser
!
      CALL START_MN()
 910  CONTINUE
!
! --- Printing the screen form
!     ~~~~~~~~~~~~~~~~~~~~~~~~
!
! --- Printing the first line: title of the program
!
      CALL CLEAR_MN()
      CALL SETCR_MN (  0, 0 )
      IF ( ELIM_MOD ) THEN
           CALL ADDSTR_F ( 'Automatic outliers elimination utility' )
           CALL SETCR_MN (  79-ILEN(VER_ELIM), 0 )
           CALL REVERSE_ON_MN()
           CALL ADDSTR_F ( VER_ELIM )
           CALL REVERSE_OFF_MN()
        ELSE
           CALL ADDSTR_F ( 'Automatic outliers restoration utility' )
           CALL SETCR_MN (  79-ILEN(VER_MILE), 0 )
           CALL REVERSE_ON_MN()
           CALL ADDSTR_F ( VER_MILE )
           CALL REVERSE_OFF_MN()
      END IF
      CALL NL_MN()
!
! --- Drawing the undescoring line
!
      CALL CLRCH ( STR )
      STR(1:1)  = CHAR(176)
      STR(2:5)  = STR(1:1)//STR(1:1)//STR(1:1)//STR(1:1)
      STR(6:11) = STR(1:1)//STR(1:5)
      CALL ADDSTR_F ( '                   '//STR(1:11) )
      CALL NL_MN()
!
! --- And then printing the statistics of the session
!
      CALL CLRCH    ( OUT )
      OUT = DBOBJ%NAME(1:I_LEN(DBOBJ%NAME))
      IF ( DBOBJ%STATUS .EQ. DBOBJ__DON ) THEN
           CALL CLRCH    ( STR )
           CALL INCH     ( DBOBJ%L_SOU, STR )
           OUT(I_LEN(OUT)+1:) = '  SOU='//STR(1:I_LEN(STR))//'('
           CALL INCH     ( DBOBJ%U_SOU, STR )
           OUT(I_LEN(OUT)+1:) = STR(1:I_LEN(STR))//')'
!
           CALL CLRCH    ( STR )
           CALL INCH     ( DBOBJ%L_STA, STR )
           OUT(I_LEN(OUT)+1:) = '  STA='//STR(1:I_LEN(STR))//'('
           CALL INCH     ( DBOBJ%U_STA, STR )
           OUT(I_LEN(OUT)+1:) = STR(1:I_LEN(STR))//')'
!
           CALL CLRCH    ( STR )
           CALL INCH     ( DBOBJ%L_BAS, STR )
           OUT(I_LEN(OUT)+1:) = '  BAS='//STR(1:I_LEN(STR))//'('
           CALL INCH     ( DBOBJ%U_BAS, STR )
           OUT(I_LEN(OUT)+1:) = STR(1:I_LEN(STR))//')'
!
           CALL CLRCH    ( STR )
           CALL INCH     ( DBOBJ%CG_OBS, STR )
           OUT(I_LEN(OUT)+1:) = '  OBS='//STR(1:I_LEN(STR))//'('
!
           CALL CLRCH    ( STR )
           CALL INCH     ( DBOBJ%U_OBS, STR )
           OUT(I_LEN(OUT)+1:) = STR(1:I_LEN(STR))//'){'
!
           CALL CLRCH    ( STR )
           CALL INCH     ( DBOBJ%R_OBS, STR )
           OUT(I_LEN(OUT)+1:) = STR(1:I_LEN(STR))//'}'
           CALL ADDSTR_F ( OUT(1:79) )
           CALL NL_MN()
         ELSE
           CALL ADDSTR_F ( OUT(1:79) )
           CALL NL_MN()
      END IF
!
! --- Get suppression method strategy label
!
      CALL CLRCH ( SUPMET_OUT )
      CALL SUPMET_SHOW ( SUPMET, SUPMET_OUT )
      IF ( DBOBJ%STATUS .EQ. DBOBJ__DON ) THEN
           CALL CLRCH ( SOLTYP )
!
! -------- Decoding solution type
!
           CALL DATYP_SHOW ( DBOBJ%IDATYP, SOLTYP )
!
! -------- Decoding w.r.m.s. for entire solution
!
           CALL CLRCH ( STR )
           WRITE ( UNIT=STR, FMT='(F8.1)' ) RST%WRMS_G*1.D12
           CALL CHASHL ( STR )
           IF ( STR(1:1) .EQ. '.' ) STR='0'//STR
!
! -------- Ratio of Chi-square to its mathematical expectation
!
           CALL CLRCH ( STR1 )
           IF ( F_CHI ) THEN
                WRITE ( UNIT=STR1, FMT='(F8.3)' ) CHIOBJ%CHI_GLO/ &
     &                                      (CHIOBJ%NEQU_GLO-CHIOBJ%CHIMAT_GLO)
                CALL CHASHL ( STR1 )
                IF ( STR1(1:1) .EQ. '.' ) STR1='0'//STR1
             ELSE
                STR1 = 'N/A'
           END IF
!
! -------- ... and printing them at the screen
!
           CALL CLRCH ( OUT )
           OUT = ''''//SUPMET_OUT(1:I_LEN(SUPMET_OUT))//'''  "'// &
     &                 SOLTYP(1:I_LEN(SOLTYP))//'"  '// &
     &                'wrms = '//STR(1:I_LEN(STR))//' psec  '// &
     &                'chi/ndg = '//STR1(1:I_LEN(STR1))
           CALL ADDSTR_F ( OUT(1:79) )
           CALL NL_MN()
           CALL CLRCH ( STR )
           IF ( ELIM_MOD ) THEN
                STR(1:2) = '<='
              ELSE
                STR(1:2) = '=>'
           END IF
!
! -------- Build the line with the strongest outlier for elimination or
! -------- restoration
!
           IS = OTL_LINE ( ELIM_MOD, ELIM_TYP, ELIM_THR, N_OBS, DBOBJ, &
     &                     OBSSCA, OBSSTA, OBSBAS, RES, RST, STR(4:) )
!
! -------- ... and putting the line at the screen
!
           CALL ADDSTR_F ( STR(1:79) )
           CALL NL_MN()
           CALL NL_MN()
        ELSE
!
! -------- Printing supprssion method and solution type at the screen
!
           CALL CLRCH ( OUT )
           OUT = ''''//SUPMET_OUT(1:I_LEN(SUPMET_OUT))//''''
           CALL ADDSTR_F ( OUT(1:79) )
           CALL NL_MN()
           CALL ADDSTR_F ( 'Information about residuals is not available yet' )
           CALL NL_MN()
           CALL NL_MN()
      END IF
!
! --- Printing value of maximum uncertainty
!
      CALL CLRCH ( STR )
      WRITE ( UNIT=STR, FMT='(F10.0)', IOSTAT=I5 ) ELIM_MSR*1.0D12
      IF ( ELIM_MSR .LT. MIN_MSR ) THEN
           ELIM_MSR = 0.0D0
           CALL CLRCH ( STR )
           STR = 'not specified'
         ELSE
           CALL CHASHL ( STR )
           IF ( STR(1:1) .EQ. '.' ) STR = '0'//STR
           STR(I_LEN(STR)+2:) = 'psec'
      END IF
      IP = I_LEN(STR)
      CALL ADDSTR_F ( '(X) Maximum uncertainty: ' )
      CALL REVERSE_ON_MN()
      CALL ADDSTR_F ( STR(1:IP) )
      CALL REVERSE_OFF_MN()
      CALL CLRCH ( STR )
      CALL ADDSTR_F ( STR(1:(18-IP)) )
!
      CALL CLRCH ( STR )
      CALL INCH ( ELIM_UPD, STR )
      CALL ADDSTR_F ( '(A) Acceleration factor: '//STR(1:I_LEN(STR)) )
      CALL NL_MN()
      CALL NL_MN()
!
      CALL ADDSTR_F ( '(U) Upper threshold for outlier detection: ' )
      CALL CLRCH ( STR )
      IF ( DABS(ELIM_THR) .GT. 1.D-13 ) THEN
           WRITE ( UNIT=STR, FMT='(F8.1)' ) ELIM_THR*1.D12
           IF ( STR(I_LEN(STR):I_LEN(STR)) .EQ. '0' ) &
     &          STR(I_LEN(STR):I_LEN(STR)) = ' '
           CALL CHASHL ( STR )
           STR = STR(1:I_LEN(STR))//' psec'
         ELSE
           STR = 'not specified'
      END IF
      CALL REVERSE_ON_MN()
      CALL ADDSTR_F ( STR(1:I_LEN(STR)) )
      CALL REVERSE_OFF_MN()
!
      IP = 15 - ILEN(STR)
      CALL CLRCH ( STR )
      IF ( IP .GT. 0 ) THEN
           CALL ADDSTR_F ( STR(1:IP) )
      END IF
!
      IF ( DBOBJ%STATUS .EQ. DBOBJ__DON ) THEN
           IF ( EQUMEM_FLAG ) THEN
                CALL ADDSTR_F ( '    EQM speed-up: yes' )
              ELSE
                CALL ADDSTR_F ( '    EQM speed-up: no ' )
           END IF
         ELSE
           CALL ADDSTR_F ( '(E) EQM speed-up: ' )
           IF ( EQUMEM_FLAG ) THEN
                CALL REVERSE_ON_MN()
                CALL ADDSTR_F ( 'Yes' )
                CALL REVERSE_OFF_MN()
              ELSE
                CALL REVERSE_ON_MN()
                CALL ADDSTR_F ( 'No' )
                CALL REVERSE_OFF_MN()
                CALL ADDSTR_F ( ' ' )
           END IF
      END IF
      CALL NL_MN()
      CALL NL_MN()
!
      CALL ADDSTR_F ( '(C) Cutoff limit for outlier detection:    ' )
      CALL CLRCH ( STR )
      IF ( ELIM_CUT .GT. 0.01 ) THEN
           WRITE ( UNIT=STR, FMT='(F9.2)' ) ELIM_CUT
           IF ( STR(I_LEN(STR):I_LEN(STR)) .EQ. '0' ) &
     &          STR(I_LEN(STR):I_LEN(STR)) = ' '
           CALL CHASHL ( STR )
           STR = STR(1:I_LEN(STR))//' sigma'
           CALL REVERSE_ON_MN()
           CALL ADDSTR_F ( STR(1:I_LEN(STR)) )
           CALL REVERSE_OFF_MN()
           CALL ADDSTR_F ( '    ' )
         ELSE
           CALL REVERSE_ON_MN()
           STR = 'not specified'
           CALL ADDSTR_F ( STR(1:I_LEN(STR)) )
           CALL REVERSE_OFF_MN()
      END IF
!
      CALL ADDSTR_F ( '  (Y) Type: ' )
      CALL CLRCH ( STR )
      IF ( ELIM_TYP .EQ. 'BA' ) STR='baseline'
      IF ( ELIM_TYP .EQ. 'GL' ) STR='global'
      CALL REVERSE_ON_MN()
      CALL ADDSTR_F ( STR(1:I_LEN(STR)) )
      CALL REVERSE_OFF_MN()
      CALL NL_MN()
      CALL NL_MN()
!
      CALL ADDSTR_F ( '(Q) Quality code limit: ' )
      CALL CLRCH ( STR )
      CALL INCH  ( QUALCODE_GOOD_LIM, STR )
      CALL REVERSE_ON_MN()
      CALL ADDSTR_F ( STR(1:I_LEN(STR)) )
      CALL REVERSE_OFF_MN()
      CALL ADDSTR_F ( '                  ' )
      IF ( DBOBJ%STATUS .NE. DBOBJ__DON ) THEN
           CALL ADDSTR_F ( '(D) Update residuals' )
        ELSE
           CALL ADDSTR_F ( '(R) Refresh screen   ' )
      END IF
      CALL NL_MN()
      CALL NL_MN()
!
      IF ( ELIM_MOD ) THEN
           CALL ADDSTR_F ( '(-) Singularity check'  )
!
           CALL ADDSTR_F ( '                      ' )
           CALL ADDSTR_F ( '('') Change suppression method' )
        ELSE IF ( .NOT. ELIM_MOD                             .AND. &
     &                  DBOBJ%STATUS .EQ. DBOBJ__DON         .AND. &
     &                ( DATYP_INQ ( DBOBJ%IDATYP, G_GXS__DTP ) .OR. &
     &                  DATYP_INQ ( DBOBJ%IDATYP, P_PXS__DTP ) .OR. &
     &                  DATYP_INQ ( DBOBJ%IDATYP, PX_GX__DTP ) .OR. &
     &                  DATYP_INQ ( DBOBJ%IDATYP, PX_GS__DTP ) .OR. &
     &                  DATYP_INQ ( DBOBJ%IDATYP, PS_GS__DTP )      ) ) THEN
           CALL ADDSTR_F ( '(M) Try to resolve ambuguity: ' )
           CALL REVERSE_ON_MN()
           IF ( ELIM_AMB ) THEN
                CALL ADDSTR_F ( 'yes' )
             ELSE
                CALL ADDSTR_F ( 'no ' )
           END IF
           CALL REVERSE_OFF_MN()
!
           CALL ADDSTR_F ( '          ' )
!
           CALL ADDSTR_F ( '(I) Ionosphere for ambiguities: ' )
           CALL REVERSE_ON_MN()
           IF ( ELIM_ION ) THEN
                CALL ADDSTR_F ( 'yes' )
             ELSE
                CALL ADDSTR_F ( 'no ' )
           END IF
           CALL REVERSE_OFF_MN()
      END IF
      CALL NL_MN()
      CALL NL_MN()
!
      CALL ADDSTR_F ( '(V) Verbosity level:    ' )
      CALL CLRCH ( STR )
      CALL INCH  ( ELIM_VRB, STR )
      CALL REVERSE_ON_MN()
      CALL ADDSTR_F ( STR(1:I_LEN(STR)) )
      CALL REVERSE_OFF_MN()
!
      CALL ADDSTR_F ( '                  ' )
      CALL ADDSTR_F ( '(N) Confirm each action: ' )
      CALL CLRCH    ( STR )
      IF ( ELIM_CNF ) THEN
           STR='yes'
           ELIM_VRB = 1
        ELSE
           STR='no'
      END IF
      CALL REVERSE_ON_MN()
      CALL ADDSTR_F ( STR(1:I_LEN(STR)) )
      CALL REVERSE_OFF_MN()
      CALL NL_MN()
      CALL NL_MN()
!
      CALL ADDSTR_F ( '(S) Return to Optin and save results ' )
      CALL ADDSTR_F ( '      ')
      CALL ADDSTR_F ( '(O) Return to Optin without saving' )
      CALL NL_MN()
      CALL NL_MN()
!
      IF ( ELIM_MOD ) THEN
           CALL ADDSTR_F ( '(P) Proceed for outliers elimination       ' )
         ELSE
           CALL ADDSTR_F ( '(P) Proceed for observations restoration   ' )
      END IF
!
      CALL ADDSTR_F ( '(T) Toggle elimination/restoration' )
      CALL NL_MN()
      CALL NL_MN()
      CALL ADDSTR_F ( '(W) Weights update'// &
     &                '                         ' )
      CALL ADDSTR_F ( '(H) On-line help' )
      CALL SETCR_MN ( 1, 20 )
!
! --- Awaiting for entering information
!
 920  CONTINUE
!
! --- Awaiting for user response
!
      CALL SENKR_MN ( IX, IY, CC4 )
      SIM = CC4(4:4)
!
! --- Transforming cursor coordinates in to the letter of the item to which it
! --- points
!
      IF ( SIM .EQ. ' ' .OR. SIM .EQ. CHAR(13) ) THEN
           IF ( IY .EQ.  6  .AND.  IX .LT. 43 ) SIM='X'
           IF ( IY .EQ.  6  .AND.  IX .GE. 43 ) SIM='A'
           IF ( IY .EQ.  8  .AND.  IX .LT. 58 ) SIM='U'
           IF ( IY .EQ.  8  .AND.  IX .GE. 58 ) SIM='E'
           IF ( IY .EQ. 10  .AND.  IX .LT. 58 ) SIM='C'
           IF ( IY .EQ. 10  .AND.  IX .GE. 58 ) SIM='Y'
           IF ( IY .EQ. 12  .AND.  IX .LT. 43 ) SIM='Q'
           IF ( IY .EQ. 12  .AND.  DBOBJ%STATUS .NE. DBOBJ__DON  .AND. &
     &                                                  IX .GE. 43 ) SIM='D'
           IF ( IY .EQ. 12  .AND.  DBOBJ%STATUS .EQ. DBOBJ__DON  .AND. &
     &                                                  IX .GE. 43 ) SIM='R'
           IF ( IY .EQ. 14  .AND.  ELIM_MOD       .AND. IX .LT. 43 ) SIM='-'
           IF ( IY .EQ. 14  .AND.  ELIM_MOD       .AND. IX .GE. 43 ) SIM="'"
!
           IF ( DATYP_INQ ( DBOBJ%IDATYP, G_GXS__DTP ) .OR. &
     &          DATYP_INQ ( DBOBJ%IDATYP, P_PXS__DTP ) .OR. &
     &          DATYP_INQ ( DBOBJ%IDATYP, PX_GS__DTP ) .OR. &
     &          DATYP_INQ ( DBOBJ%IDATYP, PS_GS__DTP )      ) THEN
              IF ( IY .EQ. 14  .AND.  .NOT. ELIM_MOD .AND. IX .GE. 43 ) SIM='I'
           END IF
           IF ( IY .EQ. 16  .AND.  IX .LT. 43 ) SIM='V'
           IF ( IY .EQ. 16  .AND.  IX .GE. 43 ) SIM='N'
           IF ( IY .EQ. 18  .AND.  IX .LT. 43 ) SIM='S'
           IF ( IY .EQ. 18  .AND.  IX .GE. 43 ) SIM='O'
           IF ( IY .EQ. 20  .AND.  IX .LT. 43 ) SIM='P'
           IF ( IY .EQ. 20  .AND.  IX .GE. 43 ) SIM='T'
           IF ( IY .EQ. 22  .AND.  IX .LT. 43 ) SIM='W'
           IF ( IY .EQ. 22  .AND.  IX .GE. 43 ) SIM='H'
      END IF
!
! --- Making actions
!     ~~~~~~~~~~~~~~
!
      IF ( SIM .EQ. 'H' ) THEN
!
! -------- Help
!
           CALL ELIM_HELP ( VER_ELIM )
           GOTO 910
        ELSE IF ( SIM .EQ. 'A' ) THEN
           IP = 6
 915       CONTINUE
           CALL SETCR_MN ( 0, IP )
!
! -------- Clearing the line
!
           CALL ADDSTR_F ( "                                        "// &
     &                     "                                        " )
!
! -------- Printout of prompt
!
           CALL SETCR_MN ( 0, IP )
           CALL ADDSTR_F ( 'Enter acceleration factor ( > 0 ): ' )
!
! -------- Entering the string STR
!
           CALL CLRCH ( STR )
           CALL GETSTR_F ( STR )
           IF ( ILEN(STR) .EQ. 0 ) GOTO 910
!
! -------- Type transformation CHARACTER  -->  INTEGER*4
!
           READ ( STR(1:I_LEN(STR)), FMT=*, IOSTAT=I5 ) IVAL
           IF ( I5 .NE. 0 ) THEN
!
! ------------- Error in type transformation
!
                CALL SETCR_MN ( 0, IP )
                CALL ADDSTR_F ( "                                        "// &
     &                          "                                       " )
                CALL SETCR_MN ( 0, IP )
                CALL ADDSTR_F ( '$$$   "'//STR(1:I_LEN(STR))//'" is not the '// &
     &              'variable of INTEGER type. Hit any key to proceed. ' )
                CALL SENKR_MN ( IX, IY, CC4 )
                GOTO 915
           END IF
           IF ( IVAL .LT. 1  ) THEN
!
! ------------- Value out of range
!
                CALL SETCR_MN ( 0, IP )
                CALL ADDSTR_F ( "                                        "// &
     &                          "                                       " )
                CALL SETCR_MN ( 0, IP )
                CALL ADDSTR_F ( '$$$   '//STR(1:I_LEN(STR))// &
     &                          ' is not > 0.  Hit any key to proceed. ' )
                CALL SENKR_MN ( IX, IY, CC4 )
                GOTO 915
           END IF
!
! -------- Substitution to the new value
!
           ELIM_UPD = IVAL
           GOTO 910
        ELSE IF ( SIM .EQ. 'U' ) THEN
!
! -------- Resetting upper threshold for outlier detection
!
           IP = 8
 930       CONTINUE
           CALL SETCR_MN ( 0, IP )
!
! -------- Clearing the line
!
           CALL ADDSTR_F ( "                                        "// &
     &                     "                                        " )
!
! -------- Printout of prompt
!
           CALL SETCR_MN ( 0, IP )
           CALL ADDSTR_F ( 'Enter upper threshold for outlier detection '// &
     &                'in psec or 0 (default) : ' )
!
! -------- Entering the string STR
!
           CALL CLRCH ( STR )
           CALL GETSTR_F ( STR )
           IF ( ILEN(STR) .EQ. 0 ) THEN
                ELIM_THR = 0.0D0
                GOTO 910
           END IF
!
! -------- Type transformation CHARACTER  -->  REAL*8
!
           READ ( STR(1:I_LEN(STR)), FMT=*, IOSTAT=I5 ) VAL
           IF ( I5 .NE. 0 ) THEN
!
! ------------- Error in type transformation
!
                CALL SETCR_MN ( 0, IP )
                CALL ADDSTR_F ( "                                        "// &
     &                          "                                       " )
                CALL SETCR_MN ( 0, IP )
                CALL ADDSTR_F ( '$$$   "'//STR(1:I_LEN(STR))//'" is not the '// &
     &              'variable of REAL type. Hit any key to proceed. ' )
                CALL SENKR_MN ( IX, IY, CC4 )
                GOTO 930
           END IF
           ELIM_THR = VAL*1.D-12
           GOTO 910
        ELSE IF ( SIM .EQ. 'C' ) THEN
!
! -------- Resetting cutoff limit for outlier detection
!
           IP = 10
 940       CONTINUE
           CALL SETCR_MN ( 0, IP )
!
! -------- Clearing the line
!
           CALL ADDSTR_F ( "                                        "// &
     &                     "                                        " )
!
! -------- Printout of prompt
!
           CALL SETCR_MN ( 0, IP )
           CALL ADDSTR_F ( 'Enter cutoff limit for outlier detection '// &
     &                'in sigmas or 0 (default) : ' )
!
! -------- Entering the string STR
!
           CALL CLRCH ( STR )
           CALL GETSTR_F ( STR )
           IF ( ILEN(STR) .EQ. 0 ) THEN
                ELIM_CUT = 0.0D0
                GOTO 910
           END IF
!
! -------- Type transformation CHARACTER  -->  REAL*8
!
           READ ( STR(1:I_LEN(STR)), FMT=*, IOSTAT=I5 ) VAL
           IF ( I5 .NE. 0 ) THEN
!
! ------------- Error in type transformation
!
                CALL SETCR_MN ( 0, IP )
                CALL ADDSTR_F ( "                                        "// &
     &                          "                                       " )
                CALL SETCR_MN ( 0, IP )
                CALL ADDSTR_F ( '$$$   "'//STR(1:I_LEN(STR))//'" is not the '// &
     &              'variable of REAL type. Hit any key to proceed. ' )
                CALL SENKR_MN ( IX, IY, CC4 )
                GOTO 940
           END IF
           ELIM_CUT = VAL
           IF ( VAL .LT. 0.0D0 ) ELIM_CUT = 0.0D0
           GOTO 910
        ELSE IF ( SIM .EQ. 'E' ) THEN
!
! -------- Toggling flag EQUMEM speed up if the database has not yet been read
!
           IP = 8
           IF ( DBOBJ%STATUS .NE. DBOBJ__DON ) THEN
                EQUMEM_FLAG = .NOT. EQUMEM_FLAG
           END IF
           GOTO 910
        ELSE IF ( SIM .EQ. 'Y' ) THEN
!
! -------- Resetting cutoff type for outliers detection
!
           IP = 10
           CALL SETCR_MN ( 0, IP )
           IF ( ELIM_TYP .EQ. 'BA' ) THEN
                ELIM_TYP = 'GL'
             ELSE IF ( ELIM_TYP .EQ. 'GL' ) THEN
                ELIM_TYP = 'BA'
           END IF
           GOTO 910
        ELSE IF ( SIM .EQ. 'Q' ) THEN
!
! -------- Resetting the quality code
!
           IP = 12
 950       CONTINUE
           CALL SETCR_MN ( 0, IP )
!
! -------- Clearing the line
!
           CALL ADDSTR_F ( "                                        "// &
     &                     "                                        " )
!
! -------- Printout of prompt
!
           CALL SETCR_MN ( 0, IP )
           CALL ADDSTR_F ( 'Enter min quality code which allow to count '// &
     &                'an observation as good: [1, 9]: ' )
!
! -------- Entering the string STR
!
           CALL CLRCH ( STR )
           CALL GETSTR_F ( STR )
           IF ( ILEN(STR) .EQ. 0 ) GOTO 910
!
! -------- Type transformation CHARACTER  -->  INTEGER*4
!
           READ ( STR(1:I_LEN(STR)), FMT=*, IOSTAT=I5 ) IVAL
           IF ( I5 .NE. 0 ) THEN
!
! ------------- Error in type transformation
!
                CALL SETCR_MN ( 0, IP )
                CALL ADDSTR_F ( "                                        "// &
     &                          "                                       " )
                CALL SETCR_MN ( 0, IP )
                CALL ADDSTR_F ( '$$$   "'//STR(1:I_LEN(STR))//'" is not the '// &
     &              'variable of INTEGER type. Hit any key to proceed. ' )
                CALL SENKR_MN ( IX, IY, CC4 )
                GOTO 950
           END IF
           IF ( IVAL .LT. 1  .OR.  IVAL .GT. 9 ) THEN
!
! ------------- Value out of range
!
                CALL SETCR_MN ( 0, IP )
                CALL ADDSTR_F ( "                                        "// &
     &                          "                                       " )
                CALL SETCR_MN ( 0, IP )
                CALL ADDSTR_F ( '$$$   '//STR(1:I_LEN(STR))// &
     &                          ' is out of range [1, 9]. '// &
     &                          'Hit any key to proceed. ' )
                CALL SENKR_MN ( IX, IY, CC4 )
                GOTO 950
           END IF
!
! -------- Substitution to the new value
!
           IF ( IVAL .NE. QUALCODE_GOOD_LIM ) F_SUPSTAT = .TRUE.
           QUALCODE_GOOD_LIM = IVAL
!
! -------- Going out from ELIOM_MENU if quality code limit was actually changed.
! -------- We update suppression status and then come back to ELIUM_MENU again
!
           IF ( F_SUPSTAT ) GOTO 810
           GOTO 910
        ELSE IF ( SIM .EQ. 'D' ) THEN
!
! -------- Settin update flag
!
           F_UPDATE = .TRUE.
           GOTO 810
        ELSE IF ( SIM .EQ. '-'  .AND. ELIM_MOD ) THEN
!
! -------- Resetting singularity control
!
           CALL SET_SNGCHK ( SNGCHK_ACTION, SNGCHK_SOUMIN, SNGCHK_STAMIN, &
     &                       SNGCHK_BASMIN )
           GOTO 910
        ELSE IF ( SIM .EQ. "'" .AND. ELIM_MOD ) THEN
!
! -------- Change suppression method
!
           CALL CHANGE_SUPMET ( F_SUPSTAT, .FALSE. )
           IF ( F_SUPSTAT ) THEN
!
! ------------- Save SUPMET updated in socom area
!
                CALL USE_COMMON ( 'OWC' )
           END IF
!
! -------- Suppression method has been really changed: go out from ELIM_MENU
! -------- to change status of observations. Then we go to ELIM_MENU again.
! -------- But we need do it only if DBOBJ  data structure has been set up
! -------- already
!
           IF ( DBOBJ%STATUS .EQ. DBOBJ__DON   .AND.  F_SUPSTAT ) GOTO 810
           F_SUPSTAT = .FALSE.
           GOTO 910
        ELSE IF ( SIM .EQ. 'M'  .AND. &
     &           .NOT. ELIM_MOD .AND. &
     &                 DBOBJ%STATUS .EQ. DBOBJ__DON  ) THEN
           IF ( ( DATYP_INQ ( DBOBJ%IDATYP, G_GXS__DTP ) .OR. &
     &            DATYP_INQ ( DBOBJ%IDATYP, P_PXS__DTP ) .OR. &
     &            DATYP_INQ ( DBOBJ%IDATYP, PX_GS__DTP ) .OR. &
     &            DATYP_INQ ( DBOBJ%IDATYP, PS_GS__DTP )      ) ) THEN
              ELIM_AMB = .NOT. ELIM_AMB
              DBOBJ%F_AMB = ELIM_AMB
              DBOBJ%F_ION = ELIM_ION
!
! ----------- Recalculation of the statistics
!
              CALL ERR_PASS ( IUER, IER )
              CALL RESID_ST ( .FALSE., .FALSE., ELIM_THR, ELIM_CUT, ELIM_MSR, 0, &
     &                        N_OBS, DBOBJ, OBSSCA, OBSBAS, RES, RST, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 6772, IUER, 'ELIM_MENU', 'Error during '// &
     &                 'calculation statisics for the postfit residuals '// &
     &                 'while database '//DBOBJ%NAME//' was processing' )
                   RETURN
              END IF
              GOTO 910
           END IF
        ELSE IF ( SIM .EQ. 'I'  .AND. &
     &           .NOT. ELIM_MOD .AND. &
     &                 DBOBJ%STATUS .EQ. DBOBJ__DON  ) THEN
           IF ( ( DATYP_INQ ( DBOBJ%IDATYP, G_GXS__DTP ) .OR. &
     &            DATYP_INQ ( DBOBJ%IDATYP, P_PXS__DTP ) .OR. &
     &            DATYP_INQ ( DBOBJ%IDATYP, PX_GS__DTP ) .OR. &
     &            DATYP_INQ ( DBOBJ%IDATYP, PS_GS__DTP )      ) ) THEN
              ELIM_ION = .NOT. ELIM_ION
              DBOBJ%F_AMB = ELIM_AMB
              DBOBJ%F_ION = ELIM_ION
!
! ----------- Recalculation of the statistics
!
              CALL ERR_PASS ( IUER, IER )
              CALL RESID_ST ( .FALSE., .FALSE., ELIM_THR, ELIM_CUT, ELIM_MSR, 0, &
     &                        N_OBS, DBOBJ, OBSSCA, OBSBAS, RES, RST, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 6773, IUER, 'ELIM_MENU', 'Error during '// &
     &                 'calculation statisics for the postfit residuals '// &
     &                 'while database '//DBOBJ%NAME//' was processing' )
                   RETURN
              END IF
              GOTO 910
           END IF
        ELSE IF ( SIM .EQ. 'V' ) THEN
!
! -------- New version of cycling verbosity mode switching
!
           IF ( ELIM_VRB .GE. ELIM_VRB_MAX ) THEN
                ELIM_VRB = ELIM_VRB_MIN
              ELSE
                ELIM_VRB = ELIM_VRB + 1
           END IF
           GOTO 910
        ELSE IF ( SIM .EQ. 'N' ) THEN
!
! -------- Toggling confirmation flag
!
           ELIM_CNF = .NOT. ELIM_CNF
           GOTO 910
        ELSE IF ( SIM .EQ. 'S' ) THEN
!
! -------- Setting Save and Optin flag
!
           F_SAVE  = .TRUE.
           F_OPTIN = .TRUE.
           GOTO 810
        ELSE IF ( SIM .EQ. 'O' ) THEN
!
! -------- Setting Optin flag
!
           F_SAVE  = .FALSE.
           F_OPTIN = .TRUE.
           GOTO 810
        ELSE IF ( SIM .EQ. 'R' ) THEN
!
! -------- Refreshing screen
!
           GOTO 910
        ELSE IF ( SIM .EQ. 'P' ) THEN
!
! -------- Exiting from the ELIM_MENU with cleared Optin, Save and Update flags
!
           GOTO 810
        ELSE IF ( SIM .EQ. 'T' ) THEN
!
! -------- Toggling ELIM/MILE mode switch
!
           ELIM_MOD = .NOT. ELIM_MOD
           GOTO 910
        ELSE IF ( SIM .EQ. 'W' ) THEN
!
! -------- Setting Optin flag
!
           F_SAVE  = .FALSE.
           F_OPTIN = .FALSE.
           F_UPWEI = .TRUE.
           GOTO 810
        ELSE IF ( SIM .EQ. 'X' ) THEN
!
! -------- Resetting maximum uncertainty
!
           IP = 6
 960       CONTINUE
           CALL SETCR_MN ( 0, IP )
!
! -------- Clearing the line
!
           CALL ADDSTR_F ( "                                        "// &
     &                     "                                        " )
!
! -------- Printout of prompt
!
           CALL SETCR_MN ( 0, IP )
           CALL ADDSTR_F ( 'Enter maximum uncertainty of the obseration '// &
     &         '(in psec) or 0 (default) : ' )
!
! -------- Entering the string STR
!
           CALL CLRCH ( STR )
           CALL GETSTR_F ( STR )
           IF ( ILEN(STR) .EQ. 0 ) THEN
                ELIM_MSR = 0.0D0
                GOTO 910
           END IF
!
! -------- Type transformation CHARACTER  -->  REAL*8
!
           READ ( STR(1:I_LEN(STR)), FMT=*, IOSTAT=I5 ) VAL
           IF ( I5 .NE. 0 ) THEN
!
! ------------- Error in type transformation
!
                CALL SETCR_MN ( 0, IP )
                CALL ADDSTR_F ( "                                        "// &
     &                          "                                       " )
                CALL SETCR_MN ( 0, IP )
                CALL ADDSTR_F ( '$$$   "'//STR(1:I_LEN(STR))//'" is not the '// &
     &              'variable of REAL type. Hit any key to proceed. ' )
                CALL SENKR_MN ( IX, IY, CC4 )
                GOTO 960
           END IF
           ELIM_MSR = VAL*1.D-12
           IF ( ELIM_MSR .LT. MIN_MSR ) ELIM_MSR = 0.0D0
           GOTO 910
        ELSE
           GOTO 920
      END IF
!
 810  CONTINUE
!
! --- Terminating curses and clearing the cscreen before leaving
!
      CALL CLEAR_MN ()
      CALL END_MN()
      CALL UN_CURSES ()    !  Elimination of the influence of curses
      CALL CLEAR ( 0, 0 )
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  ELIM_MENU  #!#
!
! ------------------------------------------------------------------------
!
      FUNCTION  OTL_LINE ( ELIM_MOD, ELIM_TYP, ELIM_THR, N_OBS, DBOBJ, &
     &                     OBSSCA, OBSSTA, OBSBAS, RES, RST, OUT )
! ************************************************************************
! *                                                                      *
! *   Auxilary routine  OTL_LINE  formats the line with information      *
! *   about eliminating or recovering observation.                       *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *  ELIM_MOD ( LOGICAL*4 ) -- mode switch. If .TRUE. than main outlier  *
! *                            will be sought. If .FALSE. tyhan the main *
! *                            candidate in restoration will be sought.  *
! *  ELIM_TYP ( CHARACTER ) -- Mode of normalization.                    *
! *     N_OBS ( INTEGER*4 ) -- Total number of observations in the       *
! *                            session.                                  *
! *     DBOBJ ( RECORD    ) -- Data structure which keeps general        *
! *                            information about the database such as    *
! *                            lists of the objects.                     *
! *    OBSSCA ( RECORD    ) -- Array of data structures which keeps      *
! *                            scan-dependent information about the      *
! *                            session.                                  *
! *    OBSSTA ( RECORD    ) -- Array of data structures which keeps      *
! *                            station dependent information about the   *
! *                            session.                                  *
! *    OBSBAS ( RECORD    ) -- Array of data structures which keeps      *
! *                            baseline dependent information about the  *
! *                            session.                                  *
! *       RES ( RECORD    ) -- Array of data structures keeping          *
! *                            information about residuals.              *
! *       RST ( RECORD    ) -- Data structure keeping the statisitcs of  *
! *                            postfit residuals.                        *
! *                                                                      *
! * ________________________ Output parameters: ________________________ *
! *                                                                      *
! *       OUT ( CHARACTER ) -- Output line.                              *
! *                                                                      *
! *  ###  19-SEP-97    OTL_LINE    v1.1  (c)  L. Petrov  20-MAR-98  ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'solve.i'
      INCLUDE   'obser.i'
      INTEGER*4  OTL_LINE, N_OBS
      LOGICAL*4  ELIM_MOD
      TYPE ( DBOBJ_O__STRU ) ::  DBOBJ
      TYPE ( SCA_O__STRU ) ::  OBSSCA(*)
      TYPE ( STA_O__STRU ) ::  OBSSTA(*)
      TYPE ( BAS_O__STRU ) ::  OBSBAS(N_OBS)
      TYPE ( RES_O__STRU ) ::  RES(N_OBS)
      TYPE ( RST_O__STRU ) ::  RST
      CHARACTER  ELIM_TYP*2, OUT*(*)
!
      INTEGER*4  IP, ISC, IOBS, IP1, IP2, IP3, IPP, NAMC
      INTEGER*2  IM, ID, IYR, IHR, IMN
      REAL*8     ELIM_THR, PSF, WNPR, AMBS
      CHARACTER  STR1*40
      INTEGER*2  INT2_ARG
      INTEGER*4  INT4
      INT4(INT2_ARG) = INT(INT2_ARG,KIND=4)
      INTEGER*4, EXTERNAL :: ILEN, I_LEN, IFIND_PL, NSTBA, MAIN_OUTOBS
!
      CALL CLRCH ( OUT )
      IOBS = MAIN_OUTOBS ( ELIM_MOD, ELIM_TYP, ELIM_THR, N_OBS, RES, RST, &
     &                     PSF, WNPR, NAMC, AMBS )
      IF ( IOBS .GT. 0 ) THEN
!
! -------- Getting information about the main outlier/cnadidate in restoration
!
! -------- Getting the index of the baseline in the baselines list
!
           IP = IFIND_PL ( DBOBJ%L_BAS, DBOBJ%LIS_BAS, &
     &                     NSTBA ( INT4(OBSBAS(IOBS)%ISITE(1)), &
     &                     INT4(OBSBAS(IOBS)%ISITE(2))  ) )
           IF ( IP .GT. 0 ) THEN
                OUT(1:) = DBOBJ%C_BAS(IP)
              ELSE
                OUT(1:) = 'Unknown baseline'
           END IF
!
! -------- Getting ISC the number of the scan
!
           ISC = OBSBAS(IOBS)%IND_SCA
!
! -------- Getting the index of the baseline in the sources list
!
           IP = IFIND_PL ( DBOBJ%L_SOU, DBOBJ%LIS_SOU, INT4(OBSSCA(ISC)%ISTAR) )
           IF ( IP .GT. 0 ) THEN
                OUT(19:) = DBOBJ%C_SOU(IP)
              ELSE
                OUT(19:) = '????_SOU'
           END IF
!
! -------- Formating time tag
!
           CALL EPOC (IM, ID, IYR, IHR, IMN, OBSSCA(ISC)%FJD+OBSSCA(ISC)%FRACT )
!
! -------- Decoding date
!
           WRITE ( UNIT=OUT(28:35), FMT='(I2,".",I2,".",I2)' ) IYR, IM, ID
!
! -------- Putting zeroes instead of blanks
!
           CALL BLANK_TO_ZERO ( OUT(I_LEN(OUT)-7:I_LEN(OUT) ) )
!
! -------- Decoding UTC tag
!
           WRITE ( UNIT=OUT(I_LEN(OUT)+2:I_LEN(OUT)+6), &
     &              FMT='(I2,":",I2)' ) IHR, IMN
!
! -------- Putting zeroes instead of blanks
!
           CALL BLANK_TO_ZERO ( OUT(I_LEN(OUT)-4:I_LEN(OUT)) )
!
! -------- Putting quality code information
!
           OUT(I_LEN(OUT)+2:)= 'Q='//OBSBAS(IOBS)%LQUAL_CHR
           CALL CHASHL ( OUT(I_LEN(OUT)-1:) )
!
! -------- Putting reciprocal weight in psec
!
           IP1=I_LEN(OUT)+2
           WRITE ( UNIT=STR1, FMT='(F7.1)' ) 1.D12/RES(IOBS)%WEI_DEL
           CALL CHASHL ( STR1 )
           IF ( STR1(1:1) .EQ. '.' ) STR1='0'//STR1
           OUT(IP1:)='w='//STR1
!
! -------- Putting post residual in psec
!
           IP2=IP1+9
           IF ( I_LEN(OUT)+3 .GT. IP2 ) IP2 = I_LEN(OUT)+3
           OUT(IP2:)='p='
           CALL CLRCH ( STR1 )
           WRITE ( UNIT=STR1, FMT='(F9.1)' ) PSF*1.D12
           CALL CHASHL ( STR1 )
           IF ( STR1(1:1) .EQ. '.'  ) STR1='0'//STR1
           IF ( STR1(1:2) .EQ. '-.' ) STR1='-0'//STR1(2:)
           IPP = ILEN(STR1)
           OUT(I_LEN(OUT)+1:)=STR1
!
           IF ( NAMC .NE. 0 ) THEN
                CALL CLRCH ( STR1 )
                CALL INCH  ( NAMC, STR1 )
                STR1='('//STR1(1:I_LEN(STR1))//')'
                IF ( IPP + ILEN(STR1) .LT. 12 ) THEN
                     OUT(I_LEN(OUT)+1:) = STR1
                  ELSE
                     OUT(I_LEN(OUT)+1:) = '#'
                END IF
           END IF
!
! -------- Putting normalized residual
!
           IP3=IP2+12
           IF ( I_LEN(OUT)+2 .GT. IP3 ) IP3 = I_LEN(OUT)+2
           OUT(IP3:)='d='
           WRITE ( UNIT=STR1, FMT='(F9.1)' ) WNPR
           CALL CHASHL ( STR1 )
           IF ( STR1(1:1) .EQ. '.'  ) STR1='0'//STR1
           IF ( STR1(1:2) .EQ. '-.' ) STR1='-0'//STR1(2:)
           OUT(I_LEN(OUT)+1:)=STR1
         ELSE
           IF ( ELIM_MOD ) THEN
                OUT(I_LEN(OUT)+2:)= 'There is no any observation with '// &
     &                              'residual large 1.d-20 ???'
              ELSE
                OUT(I_LEN(OUT)+2:)= 'There is no any recoverable rejected '// &
     &                              'observations'
           END IF
      END IF
!
      OTL_LINE = IOBS
      RETURN
      END  !#!  OTL_LINE   #!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE ELIM_HELP ( VER_ELIM )
! ************************************************************************
! *                                                                      *
! *   Ancillary siutine ELIM_HELP print on the screen on-line help       *
! *   information about ELIM routine.                                    *
! *                                                                      *
! *  ###  29-SEP-97    ELIM_HELP   v1.2  (c)  L. Petrov 30-MAR-2000 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT    NONE
      INCLUDE   'solve.i'
      INCLUDE   'help.i'
      INCLUDE   'glbc4.i'
      INTEGER*4  IUER, MBUF, NBUF, ISIM, J1, IMEN, ICOL, ILIN
      PARAMETER  ( MBUF = 1024 )
      CHARACTER  VER_ELIM*(*), BUF(MBUF)*160, ASIM*1, FINAM*255
      CHARACTER  PRE_INIT*32, POST_INIT*32, PREF*32, &
     &           SOLVE_PS_VIEWER_USE*128, ESC*1
      INTEGER*4  IT, IG, IP, IST, IM
      LOGICAL*4  USE_TERM_COLOR
      INTEGER*4, EXTERNAL :: ILEN, I_LEN, SYSTEM, MAKE_HELP_FINAM
!
      CALL GETENVAR ( 'SOLVE_PS_VIEWER', SOLVE_PS_VIEWER_USE )
      IF ( ILEN(SOLVE_PS_VIEWER_USE) == 0 ) THEN
           SOLVE_PS_VIEWER_USE = SOLVE_PS_VIEWER
      END IF
      ESC = CHAR(27)
!
! --- Stopping curses
!
      CALL END_MN()
!
! --- And elimination of the influence of curses
!
      CALL UN_CURSES() ! Elimination of the influence of curses
      CALL CLEAR ( 0, 0 )
!
! --- Make filename with help menu
!
      IM = MAKE_HELP_FINAM ( ELIM_HELP_00, FINAM )
      IF ( IM.NE.0 ) THEN
           CALL ERR_LOG ( 6781, -1, 'ELIM_HELP', 'Help file '// &
     &          ELIM_HELP_00//' is not found. Check directory '// &
     &          SOLVE_HELP_DIR//' and environment variable SOLVE_HELP_DIR' )
           CALL HIT_CONT ( '--- Hit any key to proceed --- '//CHAR(1), %VAL(0) )
           GOTO 810
      END IF
!
! --- Reading file with help menu
!
      IUER = -1
      CALL RD_TEXT ( FINAM, MBUF, BUF, NBUF, IUER )
      IF ( IUER .NE. 0 ) THEN
           CALL ERR_LOG ( 6782, -1, 'ELIM_HELP', 'Error during openning '// &
     &         'file '//FINAM(1:I_LEN(FINAM))//' with help information' )
           CALL HIT_CONT ( '--- Hit any key to proceed --- '//CHAR(1), %VAL(0) )
           GOTO 810
      END IF
!
! --- Getting terminal size
!
      CALL TERM_SIZE ( ILIN, ICOL )
!
! --- Setting esc-sequences changing the colour of the terminal (in hpterm mode)
!
      CALL CLRCH ( PRE_INIT  )
      CALL CLRCH ( POST_INIT )
      CALL CLRCH ( PREF      )
!@      CALL SHOW_IO ( IT, IG, IP, IST, %VAL(0) )
      IT = 6
!
      IF ( IT .EQ. 6  .AND.  USE_TERM_COLOR() ) THEN
           PRE_INIT  = ESC//'&v0m0.41x0.76y0.39z2I'
           POST_INIT = ESC//'&v0m1b1x1y1z2I'
           PREF = ESC//'&v2S'
      END IF
      IF ( USE_TERM_COLOR() ) THEN
           CALL PRCH ( PRE_INIT )
      END IF
!
! --- Dusplaying help-menu
!
      DO 410 J1=1,NBUF
         IF ( J1 .EQ. 1 ) THEN
!
! ----------- Adding version date
!
              BUF(1)(ICOL-ILEN(VER_ELIM):) = VER_ELIM
            ELSE
         END IF
         IF ( IT.EQ.6 .OR. IT.EQ.7 ) THEN
              CALL ADR_CURSOR ( J1, 1 )
              IF ( USE_TERM_COLOR() ) THEN
                   CALL PRCH  ( PREF )
              END IF
              CALL PRCH ( BUF(J1)(1:ICOL-1)//CHAR(13) )
           ELSE
              WRITE ( 6, FMT='(A)' ) BUF(J1)(1:I_LEN(BUF(J1)))
         END IF
 410  CONTINUE
      IF ( IT .EQ. 6  .AND.  USE_TERM_COLOR() ) THEN
           CALL PRCH ( PREF(1:ILEN(PREF))//CHAR(13)//CHAR(10) )
      END IF
!
! --- Awaiting user action
!
      CALL INSIM ( ASIM, ISIM )
!
! --- Unsetting clour changes
!
      IF ( USE_TERM_COLOR() ) THEN
           CALL PRCH ( POST_INIT )
      END IF
      IMEN = 1
      IF ( ASIM .EQ. '2' ) IMEN = 2
      IF ( ASIM .EQ. '3' ) IMEN = 3
      IF ( ASIM .EQ. '4' ) IMEN = 4
      IF ( ASIM .EQ. '5' ) IMEN = 5
!
! --- Clearing display ...
!
      CALL CLEAR ( 0, 0 )
!
! --- And different actions
!
      IF ( IMEN .EQ. 1 ) THEN
!
! -------- Displaying 1-st menu item
!
           IM = MAKE_HELP_FINAM ( ELIM_HELP_01, FINAM )
           IF ( IM.NE.0 ) THEN
                CALL ERR_LOG ( 6783, -1, 'ELIM_HELP', 'Help file '// &
     &               ELIM_HELP_01//' is not found. Check directory '// &
     &               SOLVE_HELP_DIR//' and environment variable SOLVE_HELP_DIR' )
                CALL HIT_CONT ( '--- Hit any key to proceed --- '//CHAR(1), &
     &                          %VAL(0) )
                GOTO 810
           END IF
!
           IUER = -1
           CALL SHOW_TEXT_FILE_COL ( FINAM, 'Description of ELIM menu items', &
     &                               1, IUER )
!
           IF ( IUER .NE. 0 ) THEN
                CALL ERR_LOG ( 6784, -1, 'ELIM_HELP', 'Error during openning '// &
     &              'file '//FINAM(1:I_LEN(FINAM))//' with help information' )
                CALL HIT_CONT ( '--- Hit any key to proceed --- '//CHAR(1), &
     &                          %VAL(0) )
                GOTO 810
           END IF
         ELSE IF ( IMEN .EQ. 2 ) THEN
!
! -------- Displaying 2-nd menu item (in PostScript mode)
!
           IM = MAKE_HELP_FINAM ( ELIM_HELP_02, FINAM )
           IF ( IM.NE.0 ) THEN
                CALL ERR_LOG ( 6785, -1, 'ELIM_HELP', 'Help file '// &
     &               ELIM_HELP_02//' is not found. Check directory '// &
     &               SOLVE_HELP_DIR//' and environment variable SOLVE_HELP_DIR' )
                CALL HIT_CONT ( '--- Hit any key to proceed --- '//CHAR(1), &
     &                          %VAL(0) )
                GOTO 810
           END IF
!
! -------- Launching Postscript previewer
!
           WRITE ( 6, FMT='(A)' ) 'Scheduling '// &
     &            SOLVE_PS_VIEWER_USE(1:I_LEN(SOLVE_PS_VIEWER_USE))//' '// &
     &            FINAM(1:I_LEN(FINAM))//' ...'
           IP = SYSTEM ( SOLVE_PS_VIEWER_USE(1:I_LEN(SOLVE_PS_VIEWER_USE))//' '// &
     &                   FINAM(1:I_LEN(FINAM))//CHAR(0) )
           IF ( IP .EQ. 32512 ) THEN
                CALL ERR_LOG ( 6786, -1, 'ELIM_HELP', 'Environment '// &
     &              'variable SHELL has wrong value. Error in running Shell '// &
     &              'command: '//SOLVE_PS_VIEWER_USE(1:I_LEN(SOLVE_PS_VIEWER_USE))// &
     &              ' '//FINAM )
             ELSE IF ( IP .NE. 0 ) THEN
                CALL ERR_LOG ( 6787, -1, 'ELIM_HELP', 'Error in running Shell'// &
     &              ' command: '//SOLVE_PS_VIEWER_USE(1:I_LEN(SOLVE_PS_VIEWER_USE))// &
     &              ' '//FINAM )
           END IF
           CALL HIT_CONT ( %VAL(0), %VAL(0) )
        ELSE IF ( IMEN .EQ. 3 ) THEN
!
! -------- Displaying 3-rd menu item
!
           IM = MAKE_HELP_FINAM ( ELIM_HELP_03, FINAM )
           IF ( IM.NE.0 ) THEN
                CALL ERR_LOG ( 6788, -1, 'ELIM_HELP', 'Help file '// &
     &               ELIM_HELP_03//' is not found. Check directory '// &
     &               SOLVE_HELP_DIR//' and environment variable SOLVE_HELP_DIR' )
                CALL HIT_CONT ( '--- Hit any key to proceed --- '//CHAR(1), &
     &                          %VAL(0) )
                GOTO 810
           END IF
!
           IUER = -1
           CALL SHOW_TEXT_FILE_COL ( FINAM, 'ELIM release note', 1, IUER )
           IF ( IUER .NE. 0 ) THEN
                CALL ERR_LOG ( 6789, -1, 'ELIM_HELP', 'Error during openning '// &
     &              'file '//FINAM(1:I_LEN(FINAM))//' with help information' )
                CALL HIT_CONT ( '--- Hit any key to proceed --- '//CHAR(1), &
     &                          %VAL(0) )
                GOTO 810
           END IF
        ELSE IF ( IMEN .EQ. 4 ) THEN
!
! -------- Displaying the 4-th menu item
!
           IM = MAKE_HELP_FINAM ( ELIM_HELP_04, FINAM )
           IF ( IM.NE.0 ) THEN
                CALL ERR_LOG ( 6790, -1, 'ELIM_HELP', 'Help file '// &
     &               ELIM_HELP_04//' is not found. Check directory '// &
     &               SOLVE_HELP_DIR//' and environment variable SOLVE_HELP_DIR' )
                CALL HIT_CONT ( '--- Hit any key to proceed --- '//CHAR(1), &
     &                          %VAL(0) )
                GOTO 810
           END IF
!
           IUER = -1
           CALL SHOW_TEXT_FILE_COL ( FINAM, 'Hints of using ELIM ', 1, IUER )
           IF ( IUER .NE. 0 ) THEN
                CALL ERR_LOG ( 6791, -1, 'ELIM_HELP', 'Error during openning '// &
     &              'file '//FINAM(1:I_LEN(FINAM))//' with help information' )
                CALL HIT_CONT ( '--- Hit any key to proceed --- '//CHAR(1), &
     &                          %VAL(0) )
                GOTO 810
           END IF
        ELSE IF ( IMEN .EQ. 5 ) THEN
!
! -------- Displaying the 5-th menu item
!
           IM = MAKE_HELP_FINAM ( ELIM_HELP_05, FINAM )
           IF ( IM.NE.0 ) THEN
                CALL ERR_LOG ( 6792, -1, 'ELIM_HELP', 'Help file '// &
     &               ELIM_HELP_05//' is not found. Check directory '// &
     &               SOLVE_HELP_DIR//' and environment variable SOLVE_HELP_DIR' )
                CALL HIT_CONT ( '--- Hit any key to proceed --- '//CHAR(1), &
     &                          %VAL(0) )
                GOTO 810
           END IF
!
           IUER = -1
           CALL SHOW_TEXT_FILE_COL ( FINAM, 'Description of suppression '// &
     &                                      'strategy', 1, IUER )
           IF ( IUER .NE. 0 ) THEN
                CALL ERR_LOG ( 6793, -1, 'ELIM_HELP', 'Error during openning '// &
     &              'file '//FINAM(1:I_LEN(FINAM))//' with help information' )
                CALL HIT_CONT ( '--- Hit any key to proceed --- '//CHAR(1), &
     &                          %VAL(0) )
                GOTO 810
           END IF
      END IF
!
! --- Good bye
!
 810  CONTINUE
!
! --- Clearing display
!
      CALL CLEAR ( 0, 0 )
!
! --- Starting curses again
!
      CALL START_MN()
      RETURN
      END  !#!  ELIM_HELP  #!#
