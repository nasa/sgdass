      SUBROUTINE GAMB_GET ( OBS, GAMB, CALX, CALS, IUER )
! ************************************************************************
! *                                                                      *
! *     Routine  GAMB_GET  gets data from oborg area for further         *
! *   resolving group delay ambiguity. GAMB_GET provides the interface   *
! *   between SOLVE and algorithm for group delay ambiguity resolution.  *
! *   Data (time of the observations, o-c for delay and rate, lists of   *
! *   the stations, baselines etc. ) are being extracted from internal   *
! *   data structure of the SOLVE and are being put in data structure    *
! *   GAMB for one or two bands. Only the first of 2 oborg area are      *
! *   examined. It is assumed that oborg area contains either 1          *
! *   database or 2 databases of the same experiment but for two bands:  *
! *   X- and S-. It doesn't matter in what order they are located in     *
! *   oborg area. First object GAMB will contain always X-band. The      *
! *   second will be either empty or contain S-band. If only one         *
! *   database was read but user is going to resolve group delay         *
! *   ambiguities for both of them then OPP_STATUS code (from socom.i)   *
! *   is analyzed. If it is zero, then error message is generated. If it *
! *   is not zero it means that X-band database contains observables     *
! *   from S-band. These stuff is taken from X-band in that case.        *
! *                                                                      *
! * ________________________ Output parameters: ________________________ *
! *                                                                      *
! *      OBS ( RECORD    ) -- Data structure which contains              *
! *                           band-independent information: time of      *
! *                           observation, baseline, lists of objects,   *
! *                           status flags etc.                          *
! *     GAMB ( RECORD    ) -- Array of data structures for group delay   *
! *                           ambiguity resolution software, which       *
! *                           contains two elements: the first for       *
! *                           X-band, the second for S-band.             *
! *     CALX ( RECORD    ) -- Calibration/contribution status for        *
! *                           database for X-band.                       *
! *     CALS ( RECORD    ) -- Calibration/contribution status for        *
! *                           database for S-band.                       *
! *                                                                      *
! * _______________________ Modified parameters: _______________________ *
! *                                                                      *
! *   IUER ( INTEGER*4, OPT ) -- Universal error handler.                *
! *                           Input: switch IUER=0 -- no error messages  *
! *                                  will be generated even in the case  *
! *                                  of error. IUER=-1 -- in the case of *
! *                                  error the message will be put on    *
! *                                  stdout.                             *
! *                           Output: 0 in the case of successful        *
! *                                   completion and non-zero in the     *
! *                                   case of error.                     *
! *                                                                      *
! *   Comment:                                                           *
! *         1) Global variable GAMB_IT from glbc4.i affects on the       *
! *           verbosity of informational messages of GAMB_GET.           *
! *         2) Global variables GAMB_F_X_BAND and GAMB_F_S_BAND from     *
! *            glbc4.i affect on the work of GAMB_GET.                   *
! *            If GAMB_F_X_BAND is .TRUE. and data for X-band are not be *
! *            found then error message will be generated.               *
! *            If GAMB_F_S_BAND is .TRUE. and data for S-band are not be *
! *            found then error message will be generated.               *
! *                                                                      *
! * Who  When     What                                                   *
! * pet  971124   Accommodated changes in the set of formal parameters   *
! *               for NCORT.                                             *
! * pet  980302   Added support of the deselected lists for baselines    *
! *               and sources: deselected observations are not taken     *
! *               into account.                                          *
! * pet  980306   Added capacity to read S-band stuff from the X-band    *
! *               database in the case when it really contains all       *
! *               needed stuff.                                          *
! *                                                                      *
! * pet  980309   Fixed a bug: observations without matching counterpart *
! *               at S-band were allowed to go into solution.            *
! *                                                                      *
! * pet  980403   Fixed a bug: IDBE was defined as INTEGER*2, but should *
! *               be defined as INTEGER*4!! Renamed IDBE to IDBE_I4.     *
! *               Fixed minor bug in message 7728.                       *
! *                                                                      *
! * pet  980501   Changed logic for determination suppression status of  *
! *               observations: call of SUPR_INQ instead of examining    *
! *               IUNW variable.                                         *
! *                                                                      *
! * pet  980507   Changed logic for getting bad observations and setting *
! *               "not used" flag.                                       *
! *                                                                      *
! * pet  980508   Fixed bug: previous version didn't take S-database if  *
! *               X-database had ionosphere calibration, but S-database  *
! *               -- not.                                                *
! *                                                                      *
! * pet  990129   Added check: whether the group delay ambiguity is in   *
! *               the range of acceptable values. If not then the        *
! *               observation is marked as bad and rejected before group *
! *               delay ambiguity resolution process.                    *
! *                                                                      *
! * pet  990407   Changed a bit logic of support of recoverable and      *
! *               unrecoverable observations.                            *
! *                                                                      *
! * pet  990420   Added call FLYBY_MAP_INIT before call NCORT.           *
! *                                                                      *
! * pet  990428   Added lifting (temporarily) bits of availability of    *
! *               ionosphere calibration in order to prevent deselection *
! *               of the observations due to bad ionosphere calibration. *
! *                                                                      *
! * pet  1999.11.17  Changed the list of actual parameters fro NCORT     *
! *                                                                      *
! * pet  2000.03.22  fixed a bug: the previous version set ionosphere    *
! *                  flag "undefined" if GAMB_GET didn't find a matching *
! *                  observation at the opposite band for the last       *
! *                  observation. The new version sets this flag if it   *
! *                  didn't find any matching observation.               *
! *                                                                      *
! * pet  2001.07.17  Relaxed restrictions: nonstandard database suffixes *
! *                  DX/DS are supported.                                *
! *                                                                      *
! * pet  2005.11.30  Support for meta-Solve is added.                    *
! * pet  2007.06.08  Added support of SUPMET_META
! *                                                                      *
! *  ###  29-JUL-97    GAMB_GET   v3.13  (c)  L. Petrov 08-JUN-2007 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT      NONE
      INCLUDE       'solve.i'
      INCLUDE       'glbcm.i'
      INCLUDE       'socom.i'
      INCLUDE       'oborg.i'
      INCLUDE       'prfil.i'
      INCLUDE       'precm.i'
      INCLUDE       'glbc4.i'
      INCLUDE       'cals.i'
      INCLUDE       'gamb.i'
!
      INTEGER*2    JSITN(4,MAX_ARC_STA),  ITTB(MAX_ARC_BSL)
      INTEGER*2    ITT_1(MAX_ARC_STA),    ITT_2(MAX_ARC_STA)
      INTEGER*2    JSITI_1(MAX_ARC_STA),  JSITI_2(MAX_ARC_STA)
      INTEGER*2    JCAPPL(MAX_ARC_STA),   JCAVAL(MAX_ARC_STA)
      INTEGER*2    AX_TYPES(MAX_ARC_STA)
      INTEGER*2    JCAFFL(7,MAX_ARC_STA),  NFCAL, NAMSTA, OBCAPL, MCAPL
      REAL*8       ET(2,MAX_ARC_BSL),      AX_OFFS(MAX_ARC_STA)
      REAL*8       SE(MAX_ARC_STA),        SS(MAX_ARC_SRC)
      REAL*8       BARO_CALS(MAX_ARC_STA), BARO_HEIGHTS(MAX_ARC_STA)
      REAL*8       LATS(MAX_ARC_STA),      HEIGHTS(MAX_ARC_STA)
      CHARACTER    FCAL_NAMES(112)*8 ! why 112?
      INTEGER*2    LDBNAM(5,15), IDBV(15)
      INTEGER*4    IDBE_I4(15)
      CHARACTER    CDBNAM(15)*10
      EQUIVALENCE  ( CDBNAM, LDBNAM(1,1) )
      INTEGER*2    NOGOOD
      LOGICAL*2    KBIT
      REAL*8       DERR_RAW, RERR_RAW, DPHER_RAW
      REAL*8       APP(2,2), AVG_ATM(4,MAX_ARC_STA)
!
      CHARACTER    CBAST*17, STR*20, STR1*20
      INTEGER*4    IUER, LEN_OBS, LEN_GAMB, IER
      INTEGER*4    J0, J1, J2, J3, J4, J5, J6, J7, J8, J9, J11, J12, J13
      INTEGER*4    OBK_BAS(MG_BAS)
      INTEGER*4    IP_SOU, IP_STA, IP_ST1, IP_ST2, IP_BAS, IP
      INTEGER*4    K1, K2, K3, K4, IFIND_PL
!
      INTEGER*2    OBS_IND(2)
      INTEGER*4    NF, NF_LIM, NS, NS_LIM, NS_NEW, N_DB, NN_DB, NOBS
      INTEGER*4    JSITE1_1, JSITE2_1, JSITE1_2, JSITE2_2, JSTAR_1, JSTAR_2
      REAL*8       FFJD(2), FFRACTC(2), TT_LAST(2), EPS_SEC, EPS_FAM
      PARAMETER  ( EPS_SEC = 0.1    ) ! max acceptable diff. in order of observ.
      PARAMETER  ( EPS_FAM = 1.D-14 ) ! max acceptable diff. in values of FAMB
!                                     ! to treat them separately
      LOGICAL*4    WAS_FIRST(2), USE(2), FION_CALIB(2), MATCH, PRE_USE
!
      REAL*8       TT(2), DEL_T
      REAL*8       OCT(2), TAUOB(2), OCF(2), FREOB(2), GION_TAU(2), GION_FRE(2), &
     &             F_AMB(2), P_AMB(2), FREQ_GR(2), FREQ_PH(2), FREQ_RF(2), &
     &             GION1_SAVE(2), GION2_SAVE(2), TAU_TH(2)
      REAL*8       TEMPC_SAV(2), ATMPR_SAV(2), RELHU_SAV(2)
      REAL*8       DOBS_TAU_XS, DOBS_FRE_XS, SPAC
      REAL*8       FAMB_MIN, FAMB_MAX
      PARAMETER  ( FAMB_MIN = 2.D-9 ) ! Min allowed value of gamb spacing
      PARAMETER  ( FAMB_MAX = 1.D10 ) ! Max allowed value of gamb spacing
      INTEGER*4    IAM
      REAL*8       CALIBB_XBAND(2,15),  CALIBS_XBAND(2,2,15)
      INTEGER*2    INT2_ARG
      INTEGER*4    INT4
      INT4(INT2_ARG) = INT(INT2_ARG,KIND=4)
!
      LOGICAL*4    BAD_OBS
      LOGICAL*4,   EXTERNAL :: SUPR_INQ, META_SUPR_INQ
      INTEGER*4,   EXTERNAL :: I_LEN, ILEN, ADC_LIS, ADD_LIS, NSTBA
      TYPE ( OBS__STRU  ) ::  OBS
      TYPE ( GAMB__STRU ) ::  GAMB(2)
      TYPE ( CALS_STRU  ) ::  CALX, CALS
!
! --- Important tests on equality actual and declared lentghs of data structures
!
      LEN_OBS = (LOC(OBS%LAST_FIELD) - LOC(OBS%FIRST_FIELD)) + 4
      IF ( LEN_OBS .NE. OBS__SIZE ) THEN
           CALL CLRCH ( STR  )
           CALL INCH  ( OBS__SIZE, STR )
           CALL CLRCH ( STR1 )
           CALL INCH  ( LEN_OBS, STR1 )
           CALL ERR_LOG ( 7711, IUER, 'GAMB_GET', 'Internal error: Declared '// &
     &         'size of OBS data structure (oba%i) '//STR(1:I_LEN(STR))// &
     &         ' doesn''t coincide with actual size: '//STR1(1:I_LEN(STR1)) )
           RETURN
      END IF
!
      LEN_GAMB = (LOC(GAMB(1)%LAST_FIELD) - LOC(GAMB(1)%FIRST_FIELD)) + 4
      IF ( LEN_GAMB .NE. GAMB__SIZE ) THEN
           CALL CLRCH ( STR  )
           CALL INCH  ( GAMB__SIZE, STR )
           CALL CLRCH ( STR1 )
           CALL INCH  ( LEN_GAMB, STR1 )
           CALL ERR_LOG ( 7712, IUER, 'GAMB_GET', 'Internal error: Declared '// &
     &         'size of GAMB data structure (gamb%i) '//STR(1:I_LEN(STR))// &
     &         ' doesn''t coincide with actual size: '//STR1(1:I_LEN(STR1)) )
           RETURN
      END IF
!
! --- Setting status
!
      OBS%FIRST_FIELD     = OBS__IDE
      GAMB(1)%FIRST_FIELD = GAMB__IDE
      GAMB(1)%FIRST_FIELD = GAMB__IDE
      GAMB(2)%FIRST_FIELD = GAMB__IDE
      GAMB(2)%FIRST_FIELD = GAMB__IDE
!
      OBS%STATUS_GET_X  = GAMB__UNF
      OBS%STATUS_GET_S  = GAMB__UNF
      OBS%STATUS_GAMB_X = GAMB__UNF
      OBS%STATUS_GAMB_S = GAMB__UNF
!
      CALX%STATUS = CAL__UNF
      CALS%STATUS = CAL__UNF
!
! --- Learn NUMDB     -- the number of database treated by LOOP now
! ---       LDBNAM    -- data base name
! ---       IDBV      -- data base version (in 1-st element of array)
! ---       IDBE_I4   -- number of observations (in 1-st element of array)
!
      CALL DBPOX ( NUMDB, LDBNAM, IDBV, IDBE_I4 )
      IF ( NUMDB .EQ. 0 ) THEN
           CALL ERR_LOG ( 7713, IUER, 'GAMB_GET', 'No database has been '// &
     &         'read into oborg area' )
           RETURN
      END IF
!
! --- Inititalization before determining 1) elibility databses which are already
! --- in oborg 2) their order
!
      CALL CLRCH ( GAMB(1)%DBASE )
      CALL CLRCH ( GAMB(2)%DBASE )
!
      DO 400 J0=1,2
         CALL NOUT_R8 ( MG_OBS, GAMB(J0)%JMP ) ! initialise jumps
         CALL NOUT_R8 ( MG_OBS, GAMB(J0)%OCT ) ! initialise o-c for delay
         CALL NOUT_R8 ( MG_OBS, GAMB(J0)%OCF ) ! initialise o-c for delay rate
         CALL NOUT_R8 ( MG_OBS, GAMB(J0)%GION_TAU  ) ! initialise gion for group delay
         CALL NOUT_R8 ( MG_OBS, GAMB(J0)%GION_FRE  ) ! initialise gion for delay rate
         CALL NOUT    ( 2*MG_OBS, GAMB(J0)%OBS_IND ) ! initialise obs. index
 400  CONTINUE
!
      N_DB  = MIN ( INT4(NUMDB), 2 ) ! number of database (no more than 2)
      NN_DB = N_DB                   ! number of used bands
      OBS%IDB_X =  0
      OBS%IDB_S =  0
      OBS%NB_X  = -1
      OBS%NE_X  = -1
      OBS%NB_S  = -1
      OBS%NE_S  = -1
      NS        = -1
      NS_LIM    = -1
      IF ( N_DB .EQ. 1 ) THEN
!
! -------- Only one database loaded. Fill field of session identifiers
!
           GAMB(1)%DBASE(1:12) = CDBNAM(1)(1:10)//' <'
           CALL INCH ( INT4(IDBV(1)), GAMB(1)%DBASE(13:) )
           GAMB(1)%DBASE( ILEN(GAMB(1)%DBASE)+1: ) = '>'
           IF ( CDBNAM(1)(9:9) == 'X'  .OR.  &
     &          CDBNAM(1)(9:9) == 'K'  .OR.  &
     &          ( CALCV .GE. -200.0D0  .AND.  CALCV .LE. -100.0D0 ) ) THEN
                OBS%NB_X  = 1          !  Index of the first observation
                OBS%NE_X  = IDBE_I4(1) !  Index of the last  observation
                OBS%IDB_X = 1          !  First database is X-band database
                NF        = OBS%NB_X   !  First observation of first database
                NF_LIM    = OBS%NE_X   !  Last observation of first database
           END IF
           IF ( CDBNAM(1)(9:9) .EQ. 'S' ) THEN
                OBS%NB_S  = 1          !  Index of the first observation
                OBS%NE_S  = IDBE_I4(1) !  Index of the last  observation
                OBS%IDB_S = 1          !  First database is S-band database
                NF        = OBS%NB_S   !  First observation of first database
                NF_LIM    = OBS%NE_S   !  Last observation of first database
           END IF
         ELSE IF ( N_DB .EQ. 2 ) THEN
!
! -------- Two databases loaded.
!
! -------- Do they correspond to the same session?
!
           IF ( CDBNAM(1)(1:8) .NE. CDBNAM(2)(1:8) ) THEN
                CALL ERR_LOG ( 7714, IUER, 'GAMB_GET', 'Databases '// &
     &               CDBNAM(1)(1:10)//' and '//CDBNAM(2)(1:10)//' which are '// &
     &              'the first and the second database in oborg scratch '// &
     &              'area are NOT the databases for the same experiment' )
                RETURN
           END IF
           IF (  ( CDBNAM(1)(9:9)  .EQ. 'X'   .AND. &
     &             CDBNAM(2)(9:9)  .EQ. 'S'         ) .OR. &
     &           ( CDBNAM(1)(9:10) .EQ. 'DX'  .AND. &
     &             CDBNAM(2)(9:10) .EQ. 'DS'        )       ) THEN
!
! -------------- First one is for X-band, and second for S-band.
! -------------- Fill field of session identifiers
!
                 GAMB(1)%DBASE(1:12) = CDBNAM(1)(1:10)//' <'
                 CALL INCH ( INT4(IDBV(1)), GAMB(1)%DBASE(13:) )
                 GAMB(1)%DBASE( ILEN(GAMB(1)%DBASE)+1: ) = '>'
!
                 GAMB(2)%DBASE(1:12) = CDBNAM(2)(1:10)//' <'
                 CALL INCH ( INT4(IDBV(2)), GAMB(2)%DBASE(13:) )
                 GAMB(2)%DBASE( ILEN(GAMB(2)%DBASE)+1: ) = '>'
!
                 OBS%NB_X  = 1            ! Index of the first X-band observation
                 OBS%NE_X  = IDBE_I4(1)   ! Index of the last  X-band observation
                 OBS%NB_S  = IDBE_I4(1)+1 ! Index of the first S-band observation
                 OBS%NE_S  = IDBE_I4(2)   ! Index of the last  S-band observation
                 OBS%IDB_X = 1            !  First database  was X-band database
                 OBS%IDB_S = 2            !  Second database was S-band database
              ELSE IF (  ( CDBNAM(1)(9:9)  .EQ. 'S'  .AND. &
     &                     CDBNAM(2)(9:9)  .EQ. 'X'         )  .OR. &
     &                   ( CDBNAM(1)(9:10) .EQ. 'DS'  .AND. &
     &                     CDBNAM(2)(9:10) .EQ. 'DX'        )       ) THEN
!
! -------------- First one is for S-band, and second for X-band.
! -------------- Fill field of session identifier.
!
                 GAMB(1)%DBASE(1:12) = CDBNAM(2)(1:10)//' <'
                 CALL INCH ( INT4(IDBV(2)), GAMB(1)%DBASE(13:) )
                 GAMB(1)%DBASE( ILEN(GAMB(1)%DBASE)+1: ) = '>'
!
                 GAMB(2)%DBASE(1:12) = CDBNAM(1)(1:10)//' <'
                 CALL INCH ( INT4(IDBV(1)), GAMB(2)%DBASE(13:) )
                 GAMB(2)%DBASE( ILEN(GAMB(2)%DBASE)+1: ) = '>'
!
                 OBS%NB_S  = 1             !  Index of the first S-band observation
                 OBS%NE_S  = IDBE_I4(1)    !  Index of the last  S-band observation
                 OBS%NB_X  = IDBE_I4(1)+1  !  Index of the first X-band observation
                 OBS%NE_X  = IDBE_I4(2)    !  Index of the last  X-band observation
                 OBS%IDB_S = 1          !  First database  was S-band database
                 OBS%IDB_X = 2          !  First database  was X-band database
              ELSE
!
! ------------- Another situation...
!
                CALL ERR_LOG ( 7715, IUER, 'GAMB_GET', 'Databases '// &
     &               CDBNAM(1)(1:10)//' and '//CDBNAM(2)(1:10)//' which are '// &
     &              'the first and the second database in oborg scratch '// &
     &              'area are NOT the databases for the same experiment: one '// &
     &              'for the X band and one for the S band' )
                RETURN
           END IF
!
           IF ( GAMB_F_X_BAND .AND. .NOT. GAMB_F_S_BAND ) THEN
                NF        = OBS%NB_X
                NF_LIM    = OBS%NE_X
                OBS%IDB_S = -1
             ELSE IF ( .NOT. GAMB_F_X_BAND .AND. GAMB_F_S_BAND ) THEN
                NF        = OBS%NB_S
                NF_LIM    = OBS%NE_S
                OBS%IDB_X = -1
             ELSE IF ( GAMB_F_X_BAND .AND. GAMB_F_S_BAND ) THEN
                NF        = OBS%NB_X
                NF_LIM    = OBS%NE_X
                NS        = OBS%NB_S
                NS_LIM    = OBS%NE_S
           END IF
      END IF
!
! --- Generate error message if necessary
!
      IF ( GAMB_F_X_BAND  .AND.  OBS%IDB_X .LE. 0 ) THEN
           CALL ERR_LOG ( 7716, IUER, 'GAMB_GET', 'Database for X-band '// &
     &         'has not been read into oborg area. ' )
           RETURN
      END IF
!
      IF (       GAMB_F_S_BAND      .AND. &
     &           OBS%IDB_S .LE. 0   .AND. &
     &     .NOT. KBIT ( OPP_STATUS, OPP_SET2__BIT )  ) THEN
!
           CALL ERR_LOG ( 7717, IUER, 'GAMB_GET', 'Database for S-band '// &
     &         'has not been read into oborg area. ' )
           RETURN
      END IF
      IF (  GAMB_F_S_BAND      .AND. &
     &      OBS%IDB_S .LE. 0   .AND. &
     &      KBIT ( OPP_STATUS, OPP_SET2__BIT )  ) THEN
!
            NN_DB = 2 ! The number of GAMB data strutures (for X- and S- bands)
      END IF
!
! --- Initialization
!
      OBS%NOBS  = 0
      OBS%L_SOU = 0
      OBS%L_STA = 0
      OBS%L_BAS = 0
!
      GAMB(1)%L_SOU = 0
      GAMB(1)%L_STA = 0
      GAMB(1)%L_BAS = 0
      GAMB(1)%UOBS  = 0
      GAMB(1)%L_FAM = 0
!
      GAMB(2)%L_SOU = 0
      GAMB(2)%L_STA = 0
      GAMB(2)%L_BAS = 0
      GAMB(2)%UOBS  = 0
      GAMB(2)%L_FAM = 0
      CALL FLYBY_MAP_INIT()
!
! --- Read station names, status array, eccentricity data, monument
! --- names, and set up a correspondence table between the stations
! --- in NAMFIL (JSIT's) and those in PARFIL (ISIT's).
!
      CALL NCORT ( JSITN, JSITI_1, JCAPPL, NUMSTA, ITT_1, INT2(1), &
     &             IDATYP, ITTB, ET, SE, SS, OBCAPL, MCAPL, JCAVAL, &
     &             LATS, HEIGHTS, AX_TYPES, AX_OFFS, BARO_CALS, &
     &             BARO_HEIGHTS, JCAFFL, FCAL_NAMES, NFCAL, NAMSTA, CALCV )
!
! --- Lift bits of availability of ionsphere calibration. Why? In order to
! --- prevent deselection of observations due to bad ionsphere calibration
!
      DO IP=1,NUMSTA
         CALL SBIT ( JSITI_1(IP), INT2(1), INT2(0) )
         CALL SBIT ( JSITI_1(IP), INT2(2), INT2(0) )
         CALL SBIT ( JSITI_1(IP), INT2(4), INT2(0) )
         CALL SBIT ( JSITI_1(IP), INT2(5), INT2(0) )
      END DO
      IF ( N_DB .EQ. 2 ) THEN
           CALL FLYBY_MAP_INIT()
!
! -------- The same but for the second database
!
           CALL NCORT ( JSITN, JSITI_2, JCAPPL, NUMSTA, ITT_2, INT2(2), &
     &                  IDATYP, ITTB, ET, SE, SS, OBCAPL, MCAPL, JCAVAL, &
     &                  LATS, HEIGHTS, AX_TYPES, AX_OFFS, BARO_CALS, &
     &                  BARO_HEIGHTS, JCAFFL, FCAL_NAMES, NFCAL, NAMSTA, CALCV )
!
! -------- Lift bits of availability of ionsphere calibration
!
           DO IP=1,NUMSTA
              CALL SBIT ( JSITI_2(IP), INT2(1), INT2(0) )
              CALL SBIT ( JSITI_2(IP), INT2(2), INT2(0) )
              CALL SBIT ( JSITI_2(IP), INT2(4), INT2(0) )
              CALL SBIT ( JSITI_2(IP), INT2(5), INT2(0) )
           END DO
      END IF
!
! --- Calculation coefficients of cubic spline for interpolation high
! --- frequency EOP
!
      CALL ERR_PASS ( IUER, IER )
      CALL HFINT_INIT ( IER )
      IF ( IER .NE. 0 ) THEN
          CALL ERR_LOG ( 7718, IUER, 'GAMB_GET', 'Error during '// &
     &         'attempt to build coefficients of cubic spline for '// &
     &         'intepolation of high frequency EOP' )
           STOP 'GAMB: Abnormal termination'
      END IF
!
      IF ( GAMB_IT .GT. 1 ) THEN
           IF ( NUMDB .EQ. 1 ) THEN
                WRITE (  6, FMT='(A)' ) ' &&&  Oborg area of database '// &
     &                   GAMB(1)%DBASE//' is being read now... '
              ELSE IF ( NUMDB .GE. 2 ) THEN
                WRITE (  6, FMT='(A)' ) ' &&&  Oborg area of databases '// &
     &                   GAMB(1)%DBASE//' '//GAMB(2)%DBASE// &
     &                   ' is being read'
           END IF
      END IF
!
      WAS_FIRST(1) = .FALSE. ! set the flag: no observations has not been read
      WAS_FIRST(2) = .FALSE. ! set the flag: no observations has not been read
      GAMB(1)%STATUS_ION  = GAMB__UNF ! set flag: no ionosphere information
      GAMB(2)%STATUS_ION  = GAMB__UNF ! set flag: no ionosphere information
!
      DO 410 J1=NF,NF_LIM
         MATCH = .FALSE.
!
! ------ Read the next record for the first database X-BAND from OBSFIL
!
         CALL USE_OBSFIL ( IOBSFIL, J1, 'R' )
!
! ------ We don't analyse observation with
! ------    a) no fringes
! ------    b) made at the deselected baseline
! ------    c) of the deselected source
!
         IF ( BAD_OBS ( LQUAL_CHR )                         ) GOTO 410
         IF ( .NOT. KBIT ( IBLSEL_G(1,ISITE(1)), ISITE(2) ) ) GOTO 410
         IF ( .NOT. KBIT ( ISRSEL(1),ISTAR)                 ) GOTO 410
!
! ------ Saving meteo parameters. S-band databases sometimes goes without
! ------ this informations, so we can borrow meteop from X-band and supply it
! ------ to S-band. Cunning?
!
         TEMPC_SAV(1) = TEMPC(1)
         TEMPC_SAV(2) = TEMPC(2)
         ATMPR_SAV(1) = ATMPR(1)
         ATMPR_SAV(2) = ATMPR(2)
         RELHU_SAV(1) = RELHU(1)
         RELHU_SAV(2) = RELHU(2)
!
! ------ Do the flyby mapping of DT and RT
!
         IF ( ISITE(1) .LE. 0   .OR.  ISITE(2) .LE. 0 ) THEN
               WRITE ( 6, * ) ' before flyby isite(1)=',isite(1),' isite(2)=',isite(2)
               CALL ERR_LOG ( 7719, IUER, 'GAMB_GET', 'Error during '// &
     &             'reading of '//GAMB(1)%DBASE//' : some scratch '// &
     &             'files should be updated' )
               STOP 'GAMB: Abnormal termination'
         END IF
!
! ------ Grabbing "raw" observables before any calibration
!
         TAUOB(1)    = DOBS*1.D-6 - NUMAMB*FAMB
         FREOB(1)    = ROBS
!
! ------ Making flyby calibration: adding to DT (theoretical time delay)
! ------ and to RT (thoretical delay rate) some corrections:
! ------
! ------ 1) station substitution
! ------ 2) source substitution
! ------ 3) precession-nutation substitution (7 terms)
! ------ 4) substitution nutation daily offsets
! ------ 5) UT1/PM substitution
! ------ 6) High-frequency EOP parameters
!
         CALL FLYBY_MAP()
         TAU_TH(1)   = DT*1.D-6
!
! ------ Calculation different mapping functions for using them in partials
! ------ on troposphere delay in zenith direction
!
         CALL ATMPART ( ITT_1, ISITE, ISITN, ISTAR, VSTARC, &
     &                  AZ, ELEV, ATMPR, RELHU, TEMPC, LATS, HEIGHTS, &
     &                  AX_OFFS, AX_TYPES, BARO_CALS, BARO_HEIGHTS, INT2(1) )
!
! ------ ... and make other calibrations for the j1-th observation
!
         IF ( GAMB_F_ION ) THEN
!
! ----------- Saving SOLVE-supplied ionosphere calibration and then zerioing it
!
              GION1_SAVE(1) = GION(1)
              GION1_SAVE(2) = GION(2)
              GION(1) = 0.D0
              GION(2) = 0.D0
         END IF
!
! ------ Copying arrays for X-band in special repositary. We will apply just
! ------ these calibrations for S-band observations too.
!
         CALL COPY_V ( 2*15,   CALIBB,   CALIBB_XBAND   )
         CALL COPY_V ( 2*2*15, CALIBS,   CALIBS_XBAND   )
!
! ------ Making calibration: adding to DT (theoretical time delay)
! ------ and to RT (thoretical delay rate) some corrections:
!
! -----  1) observation dependent contributions where requested;
! -----  2) non-flyby calibrations;
! -----  3) Apply the selected flyby calibrations:
! -----  4) Searching over stations and across the calibration bits in JCAFFL,
! -----     and apply the calibrations where requested.
! -----     Signs have been selected in SDBH
! -----  5) Add troposphere noise based on average atmosphere delay
! -----     (roughly elevation dependent)
! -----  6) add ionosphere calibration and modify errors;
! -----  7) setting flag of goodness of the observation due to ionosphere status
! -----  8) Apply reweighting constants
!
         CALL SOCAL ( JCAPPL, JSITI_1, ITT_1, NOGOOD, ISITE, DT, RT, &
     &                CALIBS, ICORR, GION, GIONSG, PHION, PHIONS, &
     &                DERR, RERR, DPHER, ITTB, ET, CALIBB, OBCAPL, &
     &                ISITN, ISTAR, VSTARC, &
     &                AZ, ELEV, ATMPR, RELHU, TEMPC, &
     &                DERR_RAW, RERR_RAW, DPHER_RAW, LATS, HEIGHTS, &
     &                AX_OFFS, AX_TYPES, BARO_CALS, BARO_HEIGHTS, &
     &                APP, JCAFFL, NFCAL, FCAL_NAMES, NAMSTA, INT2(1), &
     &                EFFREQ, PHEFFREQ, REFFREQ, EFFREQ_XS, PHEFFREQ_XS, &
     &                AXDIF, ISTRN_CHR(ISTAR), SOURCE_WEIGHT_FILE, &
     &                SOURCE_WEIGHTS, AVG_ATM, KELDEP_NOISE )
!
! ------- Test of the order of the observations
!
          IF ( WAS_FIRST(1) ) THEN
               TT(1) = ( (FJD - FFJD(1)) + (FRACTC - FFRACTC(1)) ) * 86400.D0
               IF ( (TT_LAST(1) - TT(1)) .GT. EPS_SEC ) THEN
                     CALL CLRCH ( STR  )
                     CALL INCH  ( J1, STR )
                     CALL CLRCH ( STR1 )
                     WRITE ( UNIT=STR1, FMT='(F20.6)' ) (TT_LAST(1) - TT(1))* &
     &                                                  86400.D0
                     CALL CHASHL  ( STR1 )
                     CALL ERR_LOG ( 7720, IUER, 'GAMB_GET', 'Wrong '// &
     &                   'order of observations detected in the session '// &
     &                    GAMB(1)%DBASE//' : '//STR(1:I_LEN(STR))// &
     &                   '-th observation occured BEFORE the previous '// &
     &                   'one at '//STR1(1:I_LEN(STR1))// &
     &                   ' sec. Database file should be cured!!!' )
                     STOP 'GAMB: Abnormal termination'
               END IF
               TT_LAST = TT(1)
             ELSE
!
! ------------ It is the first analyzed observation
!
               FFJD(1)      = FJD
               FFRACTC(1)   = FRACTC
               TT(1)        = 0.D0
               WAS_FIRST(1) = .TRUE.
          END IF
!
! ------- Grabbing infromation to the temporary arrays
!
          TT_LAST(1)  = TT(1)
          JSTAR_1     = INT4(ISTAR)
          JSITE1_1    = INT4(ISITE(1))
          JSITE2_1    = INT4(ISITE(2))
          OCT(1)      = ( DOBS - DT )*1.D-6 - NUMAMB*FAMB
          OCF(1)      = ( ROBS - RT )
          GION_TAU(1) = GION(1)*1.D-6
          GION_FRE(1) = GION(2)
          FREQ_GR(1)  = EFFREQ
          FREQ_PH(1)  = PHEFFREQ
          FREQ_RF(1)  = REFFREQ
          F_AMB(1)    = FAMB
          P_AMB(1)    = PHAMI8
          OBS_IND(1)  = INT2(J1)
!
! ------- Now we check: is FAMB (SOLVE supplied group delay ambiguity spacing)
! ------- for the current  observation coincides with old FAMB? If not we add
! ------- the current FAMB to the list of FAMB values
!
          PRE_USE = .TRUE.
          IF ( GAMB(1)%L_FAM .EQ. 0 ) THEN
!
! ------------ First observation -- put FAMB at the beginning of the list
!
               GAMB(1)%L_FAM = 1
               GAMB(1)%FAM_LIS(1) = FAMB
               GAMB(1)%FAMBAS_LIS(1) = NSTBA ( JSITE1_1, JSITE2_1 )
            ELSE
!
! ------------ Scan list of previous values of FAMB
!
               DO 4110 J11=1,GAMB(1)%L_FAM
                  IF ( DABS(FAMB - GAMB(1)%FAM_LIS(J11)) .LT. EPS_FAM) GOTO 8110
 4110          CONTINUE
               IF ( GAMB(1)%L_FAM .LT. M_FAM ) THEN
                    IF ( FAMB .LT. FAMB_MIN  .OR.  FAMB .GT. FAMB_MAX ) THEN
                         PRE_USE = .FALSE.
                       ELSE
!
! ---------------------- Adding current FAMB to the end of list
!
                         GAMB(1)%L_FAM = GAMB(1)%L_FAM + 1
                         IP = GAMB(1)%L_FAM
                         GAMB(1)%FAM_LIS(IP) = FAMB
                         GAMB(1)%FAMBAS_LIS(IP) = NSTBA ( JSITE1_1, JSITE2_1 )
                    END IF
               END IF
 8110          CONTINUE
          END IF
!
! ------- Test bits ionosphere calibrations
!
          IF (       KBIT ( JSITI_1(ITT_1(ISITE(1))), INT2(4) ) .AND. &
     &         .NOT. KBIT ( JSITI_1(ITT_1(ISITE(1))), INT2(5) ) .AND. &
     &               KBIT ( JSITI_1(ITT_1(ISITE(2))), INT2(4) ) .AND. &
     &         .NOT. KBIT ( JSITI_1(ITT_1(ISITE(2))), INT2(5) )       ) THEN
              FION_CALIB(1) = .TRUE.
            ELSE
              FION_CALIB(1) = .FALSE.
          END IF
!
! ------- Setting flags of suppression status
!
          IF ( .NOT. SUPMET == SUPMET__META ) THEN
               CALL SUPSTAT_SET ( IUNW, IUNWP, LQUAL, LQUALXS, ICORR, GIONSG, &
     &                            PHIONS, IWVBIT1, ISITE, JSITI_1, ITT_1, &
     &                            ISTAR, ELEV, KIONO, SNR, SNR_S, &
     &                            SUPSTAT, UACSUP )
          END IF
!
          IF ( GAMB_F_PREUSE ) THEN
               IF ( SUPMET == SUPMET__META ) THEN
                    USE(1) = META_SUPR_INQ ( AUTO_SUP, USER_SUP, USER_REC, &
     &                                       USED__SPS )
                  ELSE 
                    USE(1) = SUPR_INQ ( SUPSTAT, UACSUP, USED__SPS )
               END IF
             ELSE
               IF ( SUPMET == SUPMET__META ) THEN
                    USE(1) = META_SUPR_INQ ( AUTO_SUP, USER_SUP, &
     &                                       USER_REC, USED__SPS )
                  ELSE
                    USE(1) = SUPR_INQ ( SUPSTAT, UACSUP, RECO__SPS )
               END IF
          END IF
          IF ( .NOT. PRE_USE ) USE(1) = .FALSE.
!!
          NS_NEW = 0
          IF ( NS .GT. 0 ) THEN
!
! -------- Ogo! We need read the second databases
!
           DO 420 J2=NS,NS_LIM
!
! ---------- Well. Let's do it. Read the next record of the second databases
! ---------- from OBSFIL, superseding in oborg area what we had there already
!
             CALL USE_OBSFIL ( IOBSFIL, J2, 'R' )
!
             IF ( BAD_OBS ( LQUAL_CHR )                         ) GOTO 420
             IF ( .NOT. KBIT ( IBLSEL_G(1,ISITE(1)), ISITE(2) ) ) GOTO 420
             IF ( .NOT. KBIT ( ISRSEL(1),ISTAR)                 ) GOTO 420
!
             IF ( ISITE(1) .LE. 0   .OR.  ISITE(2) .LE. 0 ) THEN
                  WRITE ( 6, * ) ' before flyby isite(1)=',isite(1), &
     &                   ' isite(2)=',isite(2)
                  CALL ERR_LOG ( 7721, IUER, 'GAMB_GET', 'Error during '// &
     &                'reading of '//GAMB(2)%DBASE//' : some scratch '// &
     &                'files should be updated' )
                  STOP 'GAMB: Abnormal termination'
             END IF
!
! ---------- Test of the order of the observations
!
             IF ( WAS_FIRST(2) ) THEN
                  TT(2)= ( (FJD - FFJD(2)) + (FRACTC - FFRACTC(2)) ) * 86400.D0
                  IF ( (TT_LAST(2)- TT(2)) .GT. EPS_SEC ) THEN
                     CALL CLRCH ( STR  )
                     CALL INCH  ( J2, STR )
                     CALL CLRCH ( STR1 )
                     WRITE ( UNIT=STR1, FMT='(F20.6)' ) (TT_LAST(2) - TT(2))* &
     &                                                  86400.D0
                     CALL CHASHL  ( STR1 )
                     CALL ERR_LOG ( 7722, IUER, 'GAMB_GET', 'Wrong '// &
     &                   'order of observations detected in the session '// &
     &                    GAMB(2)%DBASE//' : '//STR(1:I_LEN(STR))// &
     &                   '-th observation occured BEFORE the previous '// &
     &                   'one at '//STR1(1:I_LEN(STR1))// &
     &                   ' sec. Database file should be cured!!!' )
                     STOP 'GAMB: Abnormal termination'
                  END IF
                  TT_LAST(2) = TT(2)
               ELSE
!
! --------------- It is the first analyzed observation
!
                  FFJD(2)      = FJD
                  FFRACTC(2)   = FRACTC
                  TT(2)        = 0.D0
             END IF
!
! ---------- Indeces of the objects for J2-th observation
!
             JSTAR_2     = INT4(ISTAR)
             JSITE1_2    = INT4(ISITE(1))
             JSITE2_2    = INT4(ISITE(2))
!
! ---------- Let's calculate time difference between observations
!
             DEL_T =   ( TT(2) - TT(1)  ) + &
     &               ( (FFJD(2) -FFJD(1)) + (FFRACTC(2) -FFRACTC(1)) )*86400.D0
!
! ---------- If the observation at S-band occured after the observation under
! ---------- investigation for the X-band then there is no matching observation
! ---------- for it
!
             IF ( DEL_T .GT. EPS_SEC ) GOTO 830 ! Not found... Go to the next...
!
             IF ( DABS(DEL_T) .LT. 0.001D0  .AND. &
     &            JSTAR_1  .EQ. JSTAR_2     .AND. &
     &            JSITE1_1 .EQ. JSITE1_2    .AND. &
     &            JSITE2_1 .EQ. JSITE2_2           ) THEN
!
! --------------- Ura! We matched observations for both bands.
!
                  MATCH = .TRUE.
!
                  TEMPC(1) = TEMPC_SAV(1)
                  TEMPC(2) = TEMPC_SAV(2)
                  ATMPR(1) = ATMPR_SAV(1)
                  ATMPR(2) = ATMPR_SAV(2)
                  RELHU(1) = RELHU_SAV(1)
                  RELHU(2) = RELHU_SAV(2)
!
! --------------- Copying arrays X-band calibrations from special repositary.
! --------------- We will apply just these calibrations for S-band observations
!
                  CALL COPY_V ( 2*15,   CALIBB_XBAND, CALIBB )
                  CALL COPY_V ( 2*2*15, CALIBS_XBAND, CALIBS )
!
! --------------- Writing data back to OBORG
!
!                  CALL USE_OBSFIL ( IOBSFIL, J2, 'W' )
!
! --------------- Grabbing "raw" observable BEFORE any calibration
!
                  TAUOB(2)    = DOBS*1.D-6 - NUMAMB*FAMB
                  FREOB(2)    = ROBS
!
! --------------- Making flyby calibration
!
                  CALL FLYBY_MAP()
                  TAU_TH(2)   = DT*1.D-6
!
! --------------- Apply the appropriate calibrations
!
                  CALL ATMPART ( ITT_2, ISITE, ISITN, ISTAR, VSTARC, &
     &                 AZ, ELEV, ATMPR, RELHU, TEMPC, LATS, HEIGHTS, &
     &                 AX_OFFS, AX_TYPES, BARO_CALS, BARO_HEIGHTS, INT2(2) )
!
                  IF ( GAMB_F_ION ) THEN
!
! -------------------- Saving SOLVE-supplied ionosphere calibration and then
! -------------------- ... zerioing it because wil'll be calculating it ourself
!
                       GION2_SAVE(1) = GION(1)
                       GION2_SAVE(2) = GION(2)
                       GION(1) = 0.D0
                       GION(2) = 0.D0
                  END IF
!
! --------------- And now -- applying calibration for S-band observations
!
                  CALL SOCAL ( JCAPPL, JSITI_2, ITT_2, NOGOOD, ISITE, DT, RT, &
     &                         CALIBS, ICORR, GION, GIONSG, PHION, PHIONS, &
     &                         DERR, RERR, DPHER, ITTB, ET, CALIBB, OBCAPL, &
     &                         ISITN, ISTAR, VSTARC, &
     &                         AZ, ELEV, ATMPR, RELHU, TEMPC, &
     &                         DERR_RAW, RERR_RAW, DPHER_RAW, LATS, HEIGHTS, &
     &                         AX_OFFS, AX_TYPES, BARO_CALS, BARO_HEIGHTS, &
     &                         APP, JCAFFL, NFCAL, FCAL_NAMES, NAMSTA, INT2(2), &
     &                         EFFREQ, PHEFFREQ, REFFREQ, EFFREQ_XS, &
     &                         PHEFFREQ_XS, AXDIF, ISTRN_CHR(ISTAR), &
     &                         SOURCE_WEIGHT_FILE, SOURCE_WEIGHTS, AVG_ATM, &
     &                         KELDEP_NOISE )
                  OCT(2)      = ( DOBS - DT )*1.D-6 - NUMAMB*FAMB
                  OCF(2)      = ( ROBS - RT )
                  GION_TAU(2) = GION(1)*1.D-6
                  GION_FRE(2) = GION(2)
                  FREQ_GR(2)  = EFFREQ
                  FREQ_PH(2)  = PHEFFREQ
                  FREQ_RF(2)  = REFFREQ
                  F_AMB(2)    = FAMB
                  P_AMB(2)    = PHAMI8
                  OBS_IND(2)  = INT2(J2)
!
! --------------- Now we check: is FAMB (SOLVE supplied group delay ambiguity
! --------------- spacing) for the current  observation coincides with old FAMB?
! --------------- If not we add the current FAMB to the list of FAMB values
!
                  PRE_USE = .TRUE.
                  IF ( GAMB(2)%L_FAM .EQ. 0 ) THEN
!
! -------------------- First observation -- put FAMB at the beginning of the list
!
                       GAMB(2)%L_FAM = 1
                       GAMB(2)%FAM_LIS(1) = FAMB
                       GAMB(2)%FAMBAS_LIS(1) = NSTBA ( JSITE1_2, JSITE2_2 )
                    ELSE
!
! -------------------- Scan list of previous values of FAMB
!
                       DO 4120 J12=1,GAMB(2)%L_FAM
                          IF ( DABS(FAMB - GAMB(2)%FAM_LIS(J12)) .LT. EPS_FAM ) &
     &                    GOTO 8120
 4120                  CONTINUE
                       IF ( GAMB(2)%L_FAM .LT. M_FAM ) THEN
                            IF ( FAMB .LT. FAMB_MIN  .OR. &
     &                           FAMB .GT. FAMB_MAX        ) THEN
                                 PRE_USE = .FALSE.
                              ELSE
!
! ------------------------------ Adding current FAMB to the end of list
!
                                 GAMB(2)%L_FAM = GAMB(2)%L_FAM + 1
                                 IP = GAMB(2)%L_FAM
                                 GAMB(2)%FAM_LIS(IP) = FAMB
                                 GAMB(2)%FAMBAS_LIS(IP) = &
     &                                   NSTBA (JSITE1_2, JSITE2_2)
                             END IF
                       END IF
 8120                  CONTINUE
                  END IF
!
! --------------- Test bits ionosphere calibrations
!
                  IF (       KBIT ( JSITI_2(ITT_2(ISITE(1))), INT2(4) ) .AND. &
     &                 .NOT. KBIT ( JSITI_2(ITT_2(ISITE(1))), INT2(5) ) .AND. &
     &                       KBIT ( JSITI_2(ITT_2(ISITE(2))), INT2(4) ) .AND. &
     &                 .NOT. KBIT ( JSITI_2(ITT_2(ISITE(2))), INT2(5) ) ) THEN
                      FION_CALIB(2) = .TRUE.
                    ELSE
                      FION_CALIB(2) = .FALSE.
                  END IF
!
! --------------- Setting flags of suppression status
!
                  IF ( .NOT. SUPMET == SUPMET__META ) THEN
                       CALL SUPSTAT_SET ( IUNW, IUNWP, LQUAL, LQUALXS, &
     &                                    ICORR, GIONSG, PHIONS, IWVBIT1, &
     &                                    ISITE, JSITI_2, ITT_2, ISTAR, &
     &                                    ELEV, KIONO, SUPSTAT, UACSUP )
!
                  END IF
                  IF ( GAMB_F_PREUSE ) THEN
                       IF ( SUPMET == SUPMET__META ) THEN
                            USE(2) = META_SUPR_INQ ( AUTO_SUP, USER_SUP, &
     &                                               USER_REC, USED__SPS )
                         ELSE 
                            USE(2) = SUPR_INQ ( SUPSTAT, UACSUP, USED__SPS )
                       END IF
                    ELSE
                       IF ( SUPMET == SUPMET__META ) THEN
                            USE(2) = META_SUPR_INQ ( AUTO_SUP, USER_SUP, &
     &                                               USER_REC, USED__SPS )
                          ELSE
                            USE(2) = SUPR_INQ ( SUPSTAT, UACSUP, RECO__SPS )
                       END IF
                  END IF
!
                  IF ( .NOT. PRE_USE ) USE(2) = .FALSE.
!
                  NS_NEW = J2
                  GOTO 820
              END IF
 420       CONTINUE
           GOTO 830 ! We reached the end but didn't find matching
!
! -------- Well we successfully found matching observation
!
 820       CONTINUE
!
! -------- If no matching observations found
!
           IF ( NS_NEW .EQ. 0 ) GOTO 830
           NS=NS_NEW ! update pointer to the first not-scanned observation
!                    ! in the second database.
          ELSE IF ( N_DB .EQ. 1  .AND.  KBIT (OPP_STATUS, OPP_SET2__BIT) .AND. &
     &              GAMB_F_X_BAND  .AND. &
     &              GAMB_F_S_BAND         ) THEN
!
! -------- We don't take into account observations without matching at S-band
!
           IF ( SUPMET == SUPMET__META ) THEN
                IF ( BTEST ( AUTO_SUP, INT4(NOFS__SPS) ) ) GOTO 830
              ELSE 
                IF ( SUPR_INQ ( SUPSTAT, UACSUP, NOFS__SPS ) ) GOTO 830
           END IF
           MATCH = .TRUE.
!
! -------- Case when there is no S-band database in oborg-area but there X-band
! -------- database contains all needed information in its slots
!
           TAUOB(2)    = DOBSXS*1.D-6 - NUMAMB_S*FAMB_S
           FREOB(2)    = ROBSXS
           TAU_TH(2)   = TAU_TH(1)
           OCT(2)      = ( DOBSXS - DT )*1.D-6 - NUMAMB_S*FAMB_S
           OCF(2)      = ( ROBSXS - RT )
!
! -------- Recalculate group delay inosphere correction for the S-band
!
           GION(1)     =-(DOBS       - DOBSXS   )*EFFREQ**2/ &
     &                   (EFFREQ**2  - EFFREQ_XS**2 )
           GION(2)     =-(ROBS       - ROBSXS   )*REFFREQ**2/ &
     &                   (REFFREQ**2 - REFFREQ_XS**2)
!
           GION_TAU(2) = GION(1)*1.D-6
           GION_FRE(2) = GION(2)
!
           IF ( GAMB_F_ION ) THEN
!
! ------------- Saving SOLVE-supplied ionosphere calibration and then
! ------------- ... zerioing it because wi'll be calculating it ourself
!
                GION2_SAVE(1) = GION(1)
                GION2_SAVE(2) = GION(2)
                GION(1) = 0.D0
                GION(2) = 0.D0
           END IF
!
           FREQ_GR(2)  = EFFREQ_XS
           FREQ_PH(2)  = PHEFFREQ_XS
           FREQ_RF(2)  = REFFREQ_XS
           F_AMB(2)    = FAMB_S
           P_AMB(2)    = PHAMI8_S
           USE(2)      = USE(1)
           OBS_IND(2)  = INT2(J1)
!
! -------- Now we check: is FAMB_S (SOLVE supplied group delay ambiguity
! -------- spacing for S-band) for the current  observation coincides with
! -------- the old FAMB_S?
! -------- If not we add the current FAMB_S to the list of FAMB_S values
!
           IF ( GAMB(2)%L_FAM .EQ. 0 ) THEN
!
! ------------- First observation -- put FAMB_S at the beginning of the list
!
                GAMB(2)%L_FAM = 1
                GAMB(2)%FAM_LIS(1) = FAMB_S
                GAMB(2)%FAMBAS_LIS(1) = NSTBA ( JSITE1_1, JSITE2_1 )
              ELSE
!
! ------------- Scan list of previous values of FAMB_S
!
                DO 4130 J13=1,GAMB(2)%L_FAM
                   IF ( DABS(FAMB_S - GAMB(2)%FAM_LIS(J13)) .LT. EPS_FAM ) &
     &             GOTO 8130
 4130           CONTINUE
                IF ( GAMB(2)%L_FAM .LT. M_FAM ) THEN
!
! ------------------ Adding current FAMB_S to the end of list
!
                     GAMB(2)%L_FAM = GAMB(2)%L_FAM + 1
                     IP = GAMB(2)%L_FAM
                     GAMB(2)%FAM_LIS(IP) = FAMB_S
                     GAMB(2)%FAMBAS_LIS(IP) = NSTBA (JSITE1_2, JSITE2_2)
                END IF
 8130           CONTINUE
            END IF
          END IF  ! N_DB
 830      CONTINUE
!
          OBS%NOBS = OBS%NOBS + 1
          NOBS     = OBS%NOBS
!
! ------- Setting baseline code
!
          OBS%IBA(NOBS) = NSTBA ( JSITE1_1, JSITE2_1 )
          OBS%ISO(NOBS) = JSTAR_1
          OBS%TT (NOBS) = TT(1)
!
! ------- Now merging data together
!
          DO 430 J3=1,NN_DB
!
! ---------- Bypass if there is no matching observation
!
             IF ( J3 .GT. 1 .AND. .NOT. MATCH ) GOTO 430
!
! ---------- Put data extracted from oborg area to GAMB data structure
!
             GAMB(J3)%OCT(NOBS)      = OCT(J3)
             GAMB(J3)%OCF(NOBS)      = OCF(J3)
             IF ( MATCH ) THEN
                  GAMB(J3)%GION_TAU(NOBS) = GION_TAU(J3)
                  GAMB(J3)%GION_FRE(NOBS) = GION_FRE(J3)
                ELSE
!
! --------------- No match -- no GION
!
                  GAMB(J3)%GION_TAU(NOBS) = 0.0D0
                  GAMB(J3)%GION_FRE(NOBS) = 0.0D0
             END IF
             GAMB(J3)%USE(NOBS)      = USE(J3)
             IF ( GAMB(J3)%USE(NOBS) ) THEN
                  GAMB(J3)%UOBS = GAMB(J3)%UOBS + 1
             END IF
             GAMB(J3)%OBS_IND(NOBS) = OBS_IND(J3)
!
             GAMB(J3)%GAMB_SP = F_AMB(J3)
             GAMB(J3)%PAMB_SP = P_AMB(J3)
             GAMB(J3)%FREQ_GR = FREQ_GR(J3)
             GAMB(J3)%FREQ_PH = FREQ_PH(J3)
             GAMB(J3)%FREQ_RF = FREQ_RF(J3)
!
             IF ( MATCH .AND. FION_CALIB(J3) ) THEN
                  GAMB(J3)%STATUS_ION  = GAMB__IONO_0
             END IF
             GAMB(J3)%STATUS_GAMB = GAMB__UNF
             GAMB(J3)%JMP(NOBS)   = 0.D0
 430      CONTINUE
!
          IF ( MATCH  .AND.  NN_DB .EQ. 2  .AND.  GAMB_F_ION ) THEN
!
! ---------- Recalculation ionosphere correction and applying it to o-c.
! ---------- Remeber: we zeroed SOLVE-supplied ionosphere calibration already
!
             DOBS_TAU_XS = TAUOB(1) - TAUOB(2) ! raw difference observed X- - S-
             DOBS_FRE_XS = FREOB(1) - FREOB(2) ! raw difference X- - S-
!
! ---------- Minimal ambiguity
!
             SPAC = MIN ( F_AMB(1), F_AMB(2) )
!
! ---------- Number of different ambiguties for both bands for these
! ---------- observation
!
             IAM  = NINT ( DOBS_TAU_XS/SPAC )
!
! ---------- Correction difference for ambiguities
!
             DOBS_TAU_XS = DOBS_TAU_XS - IAM*SPAC
             GAMB(1)%OCT(NOBS) = GAMB(1)%OCT(NOBS) - IAM*SPAC
             GAMB(1)%JMP(NOBS) = GAMB(1)%JMP(NOBS) - IAM*SPAC
!
             GAMB(1)%GION_TAU(NOBS) = -DOBS_TAU_XS  * &
     &                        FREQ_GR(2)**2/(FREQ_GR(1)**2 - FREQ_GR(2)**2)
             GAMB(2)%GION_TAU(NOBS) = -DOBS_TAU_XS  * &
     &                        FREQ_GR(1)**2/(FREQ_GR(1)**2 - FREQ_GR(2)**2)
             GAMB(1)%GION_FRE(NOBS) = -DOBS_FRE_XS  * &
     &                        FREQ_RF(2)**2/(FREQ_RF(1)**2 - FREQ_RF(2)**2)
             GAMB(2)%GION_FRE(NOBS) = -DOBS_FRE_XS  * &
     &                        FREQ_RF(1)**2/(FREQ_RF(1)**2 - FREQ_RF(2)**2)
!
             GAMB(1)%OCT(NOBS) = GAMB(1)%OCT(NOBS) - GAMB(1)%GION_TAU(NOBS)
             GAMB(2)%OCT(NOBS) = GAMB(2)%OCT(NOBS) - GAMB(2)%GION_TAU(NOBS)
!
             GAMB(1)%OCF(NOBS) = GAMB(1)%OCF(NOBS) - GAMB(1)%GION_FRE(NOBS)
             GAMB(2)%OCF(NOBS) = GAMB(2)%OCF(NOBS) - GAMB(2)%GION_FRE(NOBS)
             GAMB(1)%STATUS_ION = GAMB__IONO_1
             GAMB(2)%STATUS_ION = GAMB__IONO_1
          END IF
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!             type 210, nobs,                                           ! %%%%%%
!     #                       gion1_save(1)*1.d-6,                      ! %%%%%%
!     #                       gamb(1).gion_tau(nobs),                   ! %%%%%%
!     #                       gion2_save(1)*1.d-6,                      ! %%%%%%
!     #                       gamb(2).gion_tau(nobs)                    ! %%%%%%
! 210         format ( 1x,i4,' dif = ',4(1pd14.6,1x) )                  ! %%%%%%
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!
! ------- Now tenuous business with maintaining lists
!
! ------- Add the observed source to the list of sources
!
          CALL ERR_PASS ( IUER, IER )
          IP_SOU = ADD_LIS ( MG_SOU, OBS%L_SOU, OBS%LIS_SOU, JSTAR_1, IER )
          IF ( IER .NE. 0 ) THEN
               CALL ERR_LOG ( 7723, IUER, 'GAMB_GET', 'Error in adding a '// &
     &             'source to the list of sources' )
               RETURN
          END IF
!
! ------- Now updateing list of observed sources among ued observations at
! ------- X-band
!
          IF (  GAMB(1)%USE(NOBS) ) THEN
                IP_SOU = ADC_LIS ( MG_SOU, GAMB(1)%L_SOU, GAMB(1)%LIS_SOU, &
     &                            GAMB(1)%K_SOU, JSTAR_1, IER )
          END IF
!
! ------- Now updating list of observed sources among ued observations at
! ------- S-band
!
          IF ( MATCH  .AND.  NN_DB .EQ. 2 ) THEN
             IF ( GAMB(2)%USE(NOBS) ) THEN
                IP_SOU = ADC_LIS ( MG_SOU, GAMB(2)%L_SOU, GAMB(2)%LIS_SOU, &
     &                             GAMB(2)%K_SOU, JSTAR_1, IER )
             END IF
          END IF
!
! ------- Add the first station to the list of stations
!
          CALL ERR_PASS ( IUER, IER )
          IP_ST1 = ADD_LIS ( MG_STA, OBS%L_STA, OBS%LIS_STA, JSITE1_1, IER )
          IF ( IER .NE. 0 ) THEN
               CALL ERR_LOG ( 7724, IUER, 'GAMB_GET', 'Error in adding a '// &
     &             'station to the list of stations' )
               RETURN
          END IF
          IF (  GAMB(1)%USE(NOBS) ) THEN
                IP_ST1 = ADC_LIS ( MG_STA, GAMB(1)%L_STA, GAMB(1)%LIS_STA, &
     &                             GAMB(1)%K_STA, JSITE1_1, IER )
          END IF
          IF ( MATCH  .AND.  NN_DB .EQ. 2 ) THEN
             IF ( GAMB(2)%USE(NOBS) ) THEN
                IP_ST1 = ADC_LIS ( MG_STA, GAMB(2)%L_STA, GAMB(2)%LIS_STA, &
     &                             GAMB(2)%K_STA, JSITE1_1, IER )
             END IF
          END IF
!
! ------- Add the second station to the list of stations
!
          CALL ERR_PASS ( IUER, IER )
          IP_ST2 = ADD_LIS ( MG_STA, OBS%L_STA, OBS%LIS_STA, JSITE2_1, IER )
          IF ( IER .NE. 0 ) THEN
               CALL ERR_LOG ( 7725, IUER, 'GAMB_GET', 'Error in adding a '// &
     &             'station to the list of stations' )
               RETURN
          END IF
          IF (  GAMB(1)%USE(NOBS) ) THEN
                IP_ST2 = ADC_LIS ( MG_STA, GAMB(1)%L_STA, GAMB(1)%LIS_STA, &
     &                             GAMB(1)%K_STA, JSITE2_1, IER )
          END IF
          IF ( MATCH  .AND.  NN_DB .EQ. 2 ) THEN
             IF ( GAMB(2)%USE(NOBS) ) THEN
                IP_ST2 = ADC_LIS ( MG_STA, GAMB(2)%L_STA, GAMB(2)%LIS_STA, &
     &                             GAMB(2)%K_STA, JSITE2_1, IER )
             END IF
          END IF
!
! ------- Add the baseline code to the list of baselines
!
          CALL ERR_PASS ( IUER, IER )
          IP_BAS = ADC_LIS ( MG_BAS, OBS%L_BAS, OBS%LIS_BAS, OBK_BAS, &
     &                       NSTBA ( JSITE1_1, JSITE2_1 ), IER )
          IF ( IER .NE. 0 ) THEN
               CALL ERR_LOG ( 7726, IUER, 'GAMB_GET', 'Error in adding a '// &
     &             'baseline to the list of baselines' )
               RETURN
          END IF
          IF (  GAMB(1)%USE(NOBS) ) THEN
                IP_BAS = ADC_LIS ( MG_BAS, GAMB(1)%L_BAS, GAMB(1)%LIS_BAS, &
     &                   GAMB(1)%K_BAS, NSTBA ( JSITE1_1, JSITE2_1 ), IER )
          END IF
          IF ( MATCH  .AND.  NN_DB .EQ. 2 ) THEN
             IF ( GAMB(2)%USE(NOBS) ) THEN
                IP_BAS = ADC_LIS ( MG_BAS, GAMB(2)%L_BAS, GAMB(2)%LIS_BAS, &
     &                   GAMB(2)%K_BAS, NSTBA ( JSITE1_1, JSITE2_1 ), IER )
             END IF
          END IF
 410  CONTINUE
!
! --- The last observation has been read. What do we have?
!
      IF ( N_DB .LE. 1 ) THEN
           IF ( GAMB(1)%UOBS .EQ. 0 ) THEN
                CALL ERR_LOG ( 7727, IUER, 'GAMB_GET', 'No one used '// &
     &              'observation found in database '//GAMB(1)%DBASE )
                RETURN
           END IF
        ELSE IF ( N_DB .EQ. 2   .AND.  NS .GT. 0 ) THEN
           IF ( GAMB(2)%UOBS .EQ. 0 ) THEN
                CALL ERR_LOG ( 7728, IUER, 'GAMB_GET', 'No one used '// &
     &              'observation found in database '//GAMB(2)%DBASE )
                RETURN
           END IF
      END IF
!
! --- Sorting and updating lists
!
! --- Let's start from common lists (OBS-lists) regardless of useness
! --- observations at both bands. We do it first since OBS data structure may be
! --- used to genereate error messages for further step
!
! --- Sort lists
!
      CALL SORT_I ( OBS%L_STA, OBS%LIS_STA )
      CALL SORT_I ( OBS%L_SOU, OBS%LIS_SOU )
      DO 440 J4=1,OBS%L_SOU
!
! ------ Putting names of the sourcres to OBS data structure
!
         IP_SOU = OBS%LIS_SOU(J4)
         CALL CLRCH ( OBS%C_SOU(IP_SOU) )
         OBS%C_SOU(J4) = ISTRN_CHR( IP_SOU ) ! ISTRN_CHR from prfil.i
 440  CONTINUE
!
! --- ... stations list
!
      DO 450 J5=1,OBS%L_STA
!
! ------ Putting names of the stations to OBS data structure
!
         IP_STA = OBS%LIS_STA(J5)
         CALL CLRCH ( OBS%C_STA(IP_STA) )
         OBS%C_STA(J5) = ISITN_CHR( IP_STA ) ! ISITN_CHR from prfil.i
 450  CONTINUE
!
! --- Now time came to handle band-dependent lists.
!
      DO 460 J6=1,NN_DB
!
! ------ Let's check: whether were the observations at the same baseline but
! ------ in opposite order (for, example: NYALES-ONSALA and ONSASA-NYALES)
!
         DO 470 J7=1,GAMB(J6)%L_BAS
            IP = GAMB(J6)%LIS_BAS(J7)
            IF ( IFIND_PL ( GAMB(J6)%L_BAS, GAMB(J6)%LIS_BAS(1), -IP ) .GT.0 &
     &         ) THEN
                 CALL ERR_LOG ( 7729, IUER, 'GAMB_GET', 'Database '// &
     &                GAMB(J6)%DBASE//' contains observations made at the'// &
     &               ' baseline '//CBAST ( OBS, IP )//' but in various order' )
                 RETURN
            END IF
!
! --------- Prepareing for sorting formed lists
!
! --------- Baseline list will be sorted in according woth increasing modules
! --------- of baseline codes (since baseline code may be negative). To do it
! --------- array GAMB(J6).K_BAS will be spoiled temorarily: the oldest
! --------- decimal digits will be occupied by cmodule of baseline code
! --------- (but 5 youngest digits remained intact).
!
            GAMB(J6)%K_BAS(J7) = 100000*ABS(GAMB(J6)%LIS_BAS(J7)) + &
     &                                      GAMB(J6)%K_BAS(J7)
  470    CONTINUE
!
! ------ After that we sort (in increasong order) a pair of tied arrays:
! ------ GAMB(J6).K_BAS and GAMB(J6).LIS_BAS in according increasing
! ------ "spoiled" array GAMB(J6).K_BAS
!
         CALL SORT_I2 ( GAMB(J6)%L_BAS, GAMB(J6)%K_BAS, GAMB(J6)%LIS_BAS )
!
! ------ And now -- removing "spoliage" from the array GAMB(J6).K_BAS
!
         DO 480 J8=1,GAMB(J6)%L_BAS
            GAMB(J6)%K_BAS(J8) = GAMB(J6)%K_BAS(J8) - &
     &                           100000*ABS(GAMB(J6)%LIS_BAS(J8))
  480    CONTINUE
!
! ------ Then we sort lists of stations and sources. It may be easily done
! ------ without problems:
!
         CALL SORT_I2 ( GAMB(J6)%L_STA, GAMB(J6)%LIS_STA, GAMB(J6)%K_STA )
         CALL SORT_I2 ( GAMB(J6)%L_SOU, GAMB(J6)%LIS_SOU, GAMB(J6)%K_SOU )
!
! ------ Calculation the number of closed trinagles
!
         GAMB(J6)%L_TRI = (GAMB(J6)%L_STA - 2) * (GAMB(J6)%L_STA - 1) / 2
         IF ( GAMB(J6)%L_STA .LT. 3 ) GAMB(J6)%L_TRI=0
!
! ------ Creating the list of closed triangles (if there are any)
!
         IF ( GAMB(J6)%L_TRI .NE. 0 ) THEN
              CALL TRI_GRP ( GAMB(J6)%L_STA, GAMB(J6)%LIS_STA, &
     &                       GAMB(J6)%L_BAS, GAMB(J6)%LIS_BAS, &
     &                       MG_TRI, GAMB(J6)%L_TRI, GAMB(J6)%LIS_TRI, -3 )
         END IF
  460 CONTINUE
!
! --- At last common list of baselines
!
      DO 490 J9=1,OBS%L_BAS
         IP = OBS%LIS_BAS(J9)
         IF ( IFIND_PL ( OBS%L_BAS, OBS%LIS_BAS(1), -IP ) .GT.0 ) THEN
              CALL ERR_LOG ( 7730, IUER, 'GAMB_GET', 'Database '// &
     &            'contains observations made at the '// &
     &            'baseline '//CBAST ( OBS, IP )//' but in various order' )
              RETURN
         END IF
         OBK_BAS(J9) = 100000*ABS(OBS%LIS_BAS(J9))
 490  CONTINUE
!
! --- After that we sort (in increasong order) a pair of tied arrays
!
      CALL SORT_I2 ( OBS%L_BAS, OBK_BAS, OBS%LIS_BAS )
!
! --- Deal done.
!
      IF ( OBS%IDB_X .GT. 0 ) THEN
           OBS%STATUS_GET_X    = GAMB__GET
           GAMB(1)%STATUS_GAMB = GAMB__GET
           GAMB(1)%STATUS_NZ   = GAMB__UNF
           GAMB(1)%STATUS_BAS  = GAMB__UNF
           GAMB(1)%NEW_NZ      = -1
           GAMB(1)%NEW_L_BAS   = -1
           CALL CALS_R ( INT2(OBS%IDB_X), 0, 1, CALX, -3 )
      END IF
      IF ( OBS%IDB_S .GT. 0  .OR.  NN_DB .EQ. 2 ) THEN
           OBS%STATUS_GET_S    = GAMB__GET
           GAMB(2)%STATUS_GAMB = GAMB__GET
           GAMB(2)%STATUS_NZ   = GAMB__UNF
           GAMB(2)%STATUS_BAS  = GAMB__UNF
           GAMB(2)%NEW_NZ      = -1
           GAMB(2)%NEW_L_BAS   = -1
           IF ( OBS%IDB_S .GT. 0 ) THEN
                CALL CALS_R ( INT2(OBS%IDB_S), 0, 1, CALS, -3 )
           END IF
      END IF
!
      IF ( GAMB_IT .GT. 1 ) THEN
           IF ( NUMDB .EQ. 1  .AND. NN_DB .EQ. 1 ) THEN
                WRITE (  6, FMT='(A)' ) ' &&&  Oborg area of database '// &
     &                   GAMB(1)%DBASE//' has been read.'
              ELSE IF ( NUMDB .EQ. 1  .AND. NN_DB .EQ. 2 ) THEN
                WRITE (  6, FMT='(A)' ) ' &&&  Oborg area of database '// &
     &                   GAMB(1)%DBASE//' has been read. (S-band also)'
              ELSE IF ( NUMDB .GE. 2 ) THEN
                WRITE (  6, FMT='(A)' ) ' &&&  Oborg area of databases '// &
     &                   GAMB(1)%DBASE//' '//GAMB(2)%DBASE// &
     &                   ' has been read.'
           END IF
      END IF
!
      IF ( GAMB_IT .GT. 4 ) THEN
!
! ------ Debugging printout
!
         WRITE ( 6, * ) ' GAMB(1)%L_SOU = ',GAMB(1)%L_SOU, &
     &          ' GAMB(1)%L_STA = ',GAMB(1)%L_STA
         WRITE ( 6, * ) ' OBS%NOBS = ',OBS%NOBS, ' GAMB(1)%UOBS = ',GAMB(1)%UOBS
         WRITE ( 6, * ) ' GAMB(1)%L_TRI = ', GAMB(1)%L_TRI, &
     &          ' GAMB(1)%GAMBC = ', GAMB(1)%GAMBC
!
         DO 510 K1=1,GAMB(1)%L_SOU
            WRITE ( 6, 151 )  K1, GAMB(1)%LIS_SOU(K1), GAMB(1)%K_SOU(K1), OBS%C_SOU(K1)
  151       FORMAT ( 1X,'SOU ==> I=',I3,' LIS=',I3,' K=',I4,' NAME >>',A,'<<' )
  510    CONTINUE
         DO 520 K2=1,GAMB(1)%L_STA
            WRITE ( 6, 152 )  K2, GAMB(1)%LIS_STA(K2), GAMB(1)%K_STA(K2), OBS%C_STA(K2)
 152        FORMAT ( 1X,'STA ==> I=',I3,' LIS=',I3,' K=',I4,' NAME >>',A,'<<' )
 520     CONTINUE
         DO 530 K3=1,GAMB(1)%L_BAS
            WRITE ( 6, 153 )  K3, GAMB(1)%LIS_BAS(K3), GAMB(1)%K_BAS(K3), CBAST ( OBS, &
     &                GAMB(1)%LIS_BAS(K3) )
 153        FORMAT ( 1X,'BAS ==> I=',I3,' LIS=',I8,' K=',I4,' Baseline >>', &
     &                A,'<<' )
 530     CONTINUE
!
         IF ( GAMB_IT .GT. 5 ) THEN
            K3=0
            DO 540 K4=1,OBS%NOBS
               IF ( IFIND_PL( GAMB(1)%L_BAS, GAMB(1)%LIS_BAS, OBS%IBA(K4)) &
     &             .EQ. 1 ) THEN
                  K3=K3+1
                  WRITE ( 6, 154 )  K4, K3, CBAST ( OBS, GAMB(1)%LIS_BAS(1) ), &
     &                      OBS%TT(K4), GAMB(1)%OCT(K4)
  154             FORMAT ( 1X,' I=',I4,'/',I4,2X,A,2X,' TT = ',1PE15.7, &
     &                       ' OCT = ',1PE15.7 )
               END IF
 540        CONTINUE
         END IF
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  GAMB_GET  #!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE PRCH_23 ( IP, ILIM, STR )
      IMPLICIT   NONE
      CHARACTER  STR*(*)
      INTEGER*4  IP, ILIM, I_LEN
!
      IF ( IP .GE. ILIM ) WRITE ( 6, FMT='(A)' ) STR(1:I_LEN(STR))
      WRITE ( 23, FMT='(A)' ) STR(1:I_LEN(STR))
      RETURN
      END  !#!  PRCH_23  #!#
