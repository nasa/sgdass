      SUBROUTINE VTD_SET_BINDISP ( MJD_BEG, TAI_BEG, MJD_END, TAI_END,     &
     &                             L_STA, STA_NAM, STA_TRS_COO,            &
     &                             POSVAR_RD_AREA, N_SIT, NAMSIT, STACOO,  &
     &                             BDSFIL, BDSSAM, BDSFMJ, BDSFSC, BDSLMJ, &
     &                             BDSLSC, BDSNSA, BDS_ENDIAN, LEN_NAM,    &
     &                             LEN_FIL, POSVAR_FIL, USE_MOD, USE_INT,  &
     &                             TIM_BEG_PSV, TIM_PSV, VAL_BDS, FL_WARN, &
     &                             IUER )
! ************************************************************************
! *                                                                      *
! *   Routine VTD_SET_BINDISP  computes an arrays of site position       *
! *   variations for the set of time epochss within the time range of    *
! *   the session for all stations which participated in the session.    *
! *   Site displacement are taken from the external file with time       *
! *   series of site positin varaitions in BINDISP format. These         *
! *   external time series are re-sampled using linear or spline         *
! *   interpolation. VTD_SET_BINDISP returns vectors of 3-D              *
! *   displacements.                                                     *
! *                                                                      *
! * ________________________ Input parameters: _________________________ *
! *                                                                      *
! *      MJD_BEG ( REAL*8    ) -- Modified Julian date for the first     *
! *                               observation of the experiment.         *
! *      TAI_BEG ( REAL*8    ) -- Time at TAI elapsed from midnight for  *
! *                               the first observation of the experiment*
! *      MJD_END ( REAL*8    ) -- Modified Julian date for the last      *
! *                               observation of the experiment.         *
! *      TAI_END ( REAL*8    ) -- Time at TAI elapsed from midnight for  *
! *                               the last observation of the experiment.*
! *        L_STA ( INTEGER*4 ) -- The total number of stations for which *
! *                               displacements are to be computed.      *
! *      STA_NAM ( CHARACTER ) -- The list of station names for which    *
! *                               displacements are to be computed.      *
! *  STA_TRS_COO ( REAL*8    ) -- Arrays of coordinates of the stations  *
! *                               for which displacements are to be      *
! *                               computed. Reference system: TRS.       *
! *                               Dimensions: (3,L_STA). Units: meters.  *
! *  POSVAR_RD_AREA ( REAL*8 ) -- The radius of the area for which       *
! *                               the displacement is applicable.        *
! *        N_SIT ( INTEGER*4 ) -- The number of sites in the model of    *
! *                               site position  variations.             *
! *       NAMSIT ( CHARACTER ) -- Array of site names defined in the     *
! *                               site position variatios file. NB:      *
! *                               these names do not necessarily         *
! *                               coincide with the IVS station names.   *         *
! *                               Dimension: N_SIT.                      *
! *       STACOO ( REAL*8    ) -- Arrays of site coordinates in a crust  *
! *                               reference frame. Dimension: (3,N_SIT). *
! *       BDSFIL ( CHARACTER ) -- Array of full names including path of  *
! *                               the files with site position           *
! *                               variations time seriesin BINDISP       *
! *                               format. Dimension: N_STA.              *
! *       BDSSAM ( REAL*8    ) -- Array of sampling intervals for each   *
! *                               site position variations time series   *
! *                               files. Units: sec. Dimension: N_STA.   *
! *       BDSFMJ ( INTEGER*4 ) -- Array of the MJD at the midnight of    *
! *                               the first epoch of the site            *
! *                               displacement in the file of site       *
! *                               position variations. Dimension: N_STA. *
! *       BDSFSC ( REAL*8    ) -- Array of time elaped from the midnight *
! *                               in seconds of the first epoch of the   *
! *                               site displacement in the file of site  *
! *                               position variations. Dimension: N_STA. *
! *       BDSLMJ ( INTEGER*4 ) -- Array of the MJD at the midnight of    *
! *                               the last epoch of the site             *
! *                               displacement in the file of site       *
! *                               position variations. Dimension: N_STA. *
! *       BDSLSC ( REAL*8    ) -- Array of time elaped from the midnight *
! *                               in seconds of the first epoch of the   *
! *                               site displacement in the file of site  *
! *                                position variations. Dimension: N_STA.*
! *       BDSNSA ( INTEGER*4 ) -- Array of the number of samples in each *
! *                               file of site displacements time series.*
! *                               Dimension: N_STA.                      *
! *   BDS_ENDIAN ( CHARACTER ) -- Specifier endian binary format of the  *
! *                               files of site position variations:     *
! *                               B for Big-Endian, L for Little-endian. *
! *                               It is assumed that all files have the  *
! *                               same flags.                            *
! *      LEN_NAM ( INTEGER*4 ) -- The length of the site name string     *
! *                               in bytes.                              *
! *      LEN_FIL ( INTEGER*4 ) -- The length of the file name string     *
! *                               in bytes.                              *
! *   POSVAR_FIL ( CHARACTER ) -- The name of the harmonic site position *
! *                               variations file. Used for generating   *
! *                               error messages only.                   *
! *      USE_MOD ( INTEGER*4 ) -- Usage mode. If a station was not find  *
! *                               in the site list for harmonic site     *
! *                               position variations model, then        *
! *                               depending on USE_MOD the following     *
! *                               will be done:                          *
! *                               1) USE_MOD = PSV__REQ -- then the      *
! *                                  error message will be issued and    *
! *                                  the process will be terminated.     *
! *                               2) USE_MOD = PSV__IFA -- then          *
! *                                  a warning message will be put in    *
! *                                  screen and in spool file if         *
! *                                  G_WARNING is true and then the      *
! *                                  process will continue with setting  *
! *                                  harmonic position variations for    *
! *                                  this site to zero.                  *
! *      USE_INT ( INTEGER*4 ) -- Interpolation mode. Two modes are      *
! *                               supported: PSV__LIN -- then linear     *
! *                               interpolation is used for computing    *
! *                               position variations on the nodes of    *
! *                               the output array TIM_PSV. If PSV__SPL  *
! *                               then spline interpolation is in use.   *
! *     FL_WARN ( LOGICAL*4  ) -- Flag: if .TRUE., then a warning will   *
! *                               be set in the screen and out in the    *
! *                               spool file if needed. If .FALSE. then  *
! *                               no warning message will be generated.  *
! *                                                                      *
! * ________________________ Output parameters: ________________________ *
! *                                                                      *
! *   TIM_BEG_PSV ( REAL*8    ) -- Date in TDB time scale in seconds     *
! *                                elapsed from the J2000.0 of the       *
! *                                first time epoch of the output array. *
! *                                It is set to the nominal session      *
! *                                start minus some amount of the        *
! *                                overhead time defined in OVH__PSV     *
! *                                variable from vtd.i                   *
! *       TIM_PSV ( REAL*8    ) -- Array of time epochs for the site     *
! *                                position variations as the time       *
! *                                elapsed from TIM_BEG_PSV.             *
! *                                Dimension: M__PSV.                    *
! *       VAL_PSV ( REAL*8    ) -- Three-dimensional array of position   *
! *                                variations of all stations            *
! *                                participated in this session          *
! *                                on the epochs defined in array        *
! *                                TIM_PSV. Dimensions:                  *
! *                                (VTD__M_SDI,3,VTD__M_STA).            *
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
! *  IUER ( INTEGER*4, OPT ) -- Universal error handler.                 *
! *                           Input: switch IUER=0 -- no error messages  *
! *                                  will be generated even in the case  *
! *                                  of error. IUER=-1 -- in the case of *
! *                                  error the message will be put on    *
! *                                  stdout.                             *
! *                           Output: 0 in the case of successful        *
! *                                   completion and non-zero in the     *
! *                                   case of error.                     *
! *                                                                      *
! * ### 18-DEC-2002  VTD_SET_BINDISP  v1.4 (c) L. Petrov 25-NOV-2004 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'vtd.i'
      TYPE      ( BINDISP_DATA ) ::  BDS
      INTEGER*4  L_STA, MJD_BEG, MJD_END, N_SIT, BDSFMJ(N_SIT), BDSLMJ(N_SIT), &
     &           BDSNSA(N_SIT), LEN_NAM, LEN_FIL, USE_MOD, USE_INT, IUER
      CHARACTER  STA_NAM(L_STA)*(LEN_NAM), NAMSIT(N_SIT)*(LEN_NAM), &
     &           BDSFIL(N_SIT)*(LEN_FIL), POSVAR_FIL*128, BDS_ENDIAN*1, &
     &           STR*80, STR1*80
      REAL*8     TAI_BEG, TAI_END, POSVAR_RD_AREA, STACOO(3,N_SIT), &
     &           BDSSAM(N_SIT), BDSFSC(N_SIT), BDSLSC(N_SIT), TIM_BEG_PSV, &
     &           TIM_PSV(M__PSV), VAL_BDS(M__PSV,3,VTD__M_STA), &
     &           STA_TRS_COO(3,L_STA)
      LOGICAL*4  FL_WARN
      REAL*8     TIM_BEG_SEC, TIM_END_SEC, TIM_STEP_SEC, TIM_STEP_DAY
      REAL*8     TIM_ARG, ARG_BDS_DAY(M__BDS), ARG_BDS_SEC(M__BDS), &
     &           XYZ_VEC(M__BDS,3), SPL_BDS(M__BDS,3), WORK_SPL(M__BDS), &
     &           UEN_TO_XYZ(3,3,VTD__M_STA), DIST_SQ_MIN
      INTEGER*2  KX, KY, KZ
      INTEGER*4  I_STA(VTD__M_STA), IREC_BEG, IREC_END, IOS, NODE, I_BDS, &
     &           L_BDS, LUN, J1, J2, J3, J4, J5, IER
      CHARACTER  ENDIAN__FMT*1
#ifdef BIG_ENDIAN
      PARAMETER  ( ENDIAN__FMT = 'B' ) 
#else
      PARAMETER  ( ENDIAN__FMT = 'L' ) 
#endif
!
      CHARACTER, EXTERNAL :: MJDSEC_TO_DATE*30
      REAL*8,    EXTERNAL :: FLIN8, FSPL8, MJD_SEC_TO_JD
      INTEGER*4, EXTERNAL :: GET_UNIT, I_LEN, IXMN8, IXMN8_S
!
! --- Initialization
!
      CALL NOUT_R8 ( VTD__M_SDI,              TIM_PSV     )
      CALL NOUT_R8 ( VTD__M_SDI*3*VTD__M_STA, VAL_BDS     )
      CALL NOUT_R8 ( M__BDS,                  ARG_BDS_DAY )
      CALL NOUT_R8 ( M__BDS,                  ARG_BDS_SEC )
      CALL NOUT_R8 ( M__BDS*3,                XYZ_VEC     )
      CALL NOUT_R8 ( M__BDS*3,                SPL_BDS     )
      CALL NOUT_R8 ( M__BDS,                  WORK_SPL    )
      CALL NOUT_R8 ( VTD__M_STA*3*3,          UEN_TO_XYZ  )
!
      IF ( L_STA .GT. VTD__M_STA ) THEN
           CALL CLRCH ( STR  )
           CALL CLRCH ( STR1 )
           CALL INCH  ( L_STA, STR )
           CALL INCH  ( VTD__M_STA, STR1 )
           CALL ERR_LOG ( 2821, IUER, 'VTD_SET_BINDISP', 'Error of internal '// &
     &         'control: parameter L_STA: '//STR(1:I_LEN(STR))//' is too '// &
     &         'large: larger than the constant VTD__M_STA '// &
     &         STR1(1:I_LEN(STR1))//' defined in vtd.i' ) 
           RETURN 
      END IF
!
! --- Get nominal start ( FJD_BEG ) and nominal stop ( FJD_END ) time epoch
!
      TIM_BEG_SEC = (MJD_BEG - J2000__MJD)*86400.0D0 + (TAI_BEG - 43200.0D0)
      TIM_END_SEC = (MJD_END - J2000__MJD)*86400.0D0 + (TAI_END - 43200.0D0)
!
! --- Compute the first epoch of the time interval for which harmonic
! --- displacement is to be computed
!
      TIM_BEG_PSV = TIM_BEG_SEC - OVH__PSV
!
! --- Get the step of the the array of time epochs
!
      TIM_STEP_SEC = ( (TIM_END_SEC - TIM_BEG_SEC) + 2.0D0*OVH__PSV )/ &
     &               (VTD__M_SDI-1)
!
      DO 410 J1=1,M__PSV
!
! ------ TIM_PSV -- time in seconds elapsed since beginning the
! ------ interpolation interval
!
         TIM_PSV(J1) = (J1-1)*TIM_STEP_SEC + 1.D-3
 410  CONTINUE
!
! --- Initialization
!
      CALL NOUT_R8 ( M__PSV*3*VTD__M_STA, VAL_BDS )
!
      DO 420 J2=1,L_STA ! Cycle over the participated stations
!
! ------ Bypass a "GEOCENTR" station
!
         IF ( DSQRT ( STA_TRS_COO(1,J2)**2 + STA_TRS_COO(2,J2)**2 + &
     &                STA_TRS_COO(3,J2)**2 ) < VTD__REA/2.0D0 ) GOTO 420
!
! ------ Compute the matrix of transformation from the local
! ------ topocentric reference system (UEN) to the crust fixed
! ------ reference system (XYZ)
!
         CALL MAKE_UEN_TO_XYZ ( STA_TRS_COO(1,J2), UEN_TO_XYZ(1,1,J2) )
!
! ------ Search for the site which is the closest to the J2-th station
!
         DIST_SQ_MIN = VTD__REA**2
         I_STA(J2) = 0
         DO 430 J3=1,N_SIT
            IF ( (STACOO(1,J3)-STA_TRS_COO(1,J2))**2 + &
     &           (STACOO(2,J3)-STA_TRS_COO(2,J2))**2 + &
     &           (STACOO(3,J3)-STA_TRS_COO(3,J2))**2   .LT. DIST_SQ_MIN ) THEN
!
                 I_STA(J2) = J3
                 DIST_SQ_MIN = (STACOO(1,J3)-STA_TRS_COO(1,J2))**2 + &
     &                         (STACOO(2,J3)-STA_TRS_COO(2,J2))**2 + &
     &                         (STACOO(3,J3)-STA_TRS_COO(3,J2))**2
            END IF
 430     CONTINUE
!
         IF ( I_STA(J2) .EQ. 0  .OR.  DIST_SQ_MIN .GT. POSVAR_RD_AREA**2 ) THEN 
!
! ----------- The station was not been found in the list. Well, let's issue 
! ----------- a warning and go ahead
!
              IF ( FL_WARN ) THEN
                   IF ( ( USE_MOD .EQ. PSV__REQ  .OR. &
     &                  ( USE_MOD .EQ. PSV__AVL  .AND.  FL_WARN ) ) ) THEN
                        WRITE ( 6, '(A)' ) 'WARNING (VTD_SET_BINDISP) '// &
     &                        'Station '//STA_NAM(J2)//' was not found '// &
     &                        'in the position variation file '// &
     &                         POSVAR_FIL(1:I_LEN(POSVAR_FIL))
                        IF ( USE_MOD .EQ. PSV__REQ ) THEN
                             CALL ERR_LOG ( 2831, IUER, 'VTD_SET_BINDISP', &
     &                           'Station '//STA_NAM(J2)//' was not found '// &
     &                           'in the position variation file '//POSVAR_FIL )
                             RETURN
                        END IF
                   END IF
              END IF
              GOTO 420  ! Go to the next station
         END IF
!
! ------ Get the index of the data record in the biniary site position time
! ------ series file for the first epoch of TIM_PSV (IREC_BEG) and for the
! ------ last epoch of this array (IREC_END)
! ------ Epoch of IREC_BEG is coincides with TIM_PSV(1) node or just before
!
         IREC_BEG = ( TIM_BEG_PSV -                                  &
     &                ( BDSFMJ(I_STA(J2)) - J2000__MJD )*86400.0D0 - &
     &                ( BDSFSC(I_STA(J2)) - 43200.0D0  )             &
     &              )                                                &
     &              / BDSSAM(I_STA(J2)) + 1
         IREC_END = ( TIM_BEG_PSV + M__PSV*TIM_STEP_SEC -            &
     &                ( BDSFMJ(I_STA(J2)) - J2000__MJD )*86400.0D0 - &
     &                ( BDSFSC(I_STA(J2)) - 43200.0D0  )             &
     &              )                                                &
     &              / BDSSAM(I_STA(J2)) + 2
!
! ------ Check whether the index of the first record is OK
!
         IF ( IREC_BEG .LT. 1 ) THEN
              IF ( FL_WARN ) THEN
                   STR  = MJDSEC_TO_DATE ( MJD_BEG, TAI_BEG, IER )  
                   STR1 = MJDSEC_TO_DATE ( BDSFMJ(I_STA(J2)), &
     &                                     BDSFSC(I_STA(J2)), IER )
                   IF ( USE_MOD .EQ. PSV__REQ ) THEN
                        CALL ERR_LOG ( 2832, IUER, 'VTD_SET_BINDISP', 'Station '// &
     &                       STA_NAM(J2)//' has position variation data '// &
     &                       'only after '//STR1(1:I_LEN(STR1))// &
     &                       ' in the position variation file '// &
     &                        POSVAR_FIL(1:I_LEN(POSVAR_FIL))// &
     &                       ' what is not enough for processing a session '// &
     &                       'which has nominal start at '//STR )
                        RETURN
                   END IF
!
                   WRITE ( 6, '(A)' ) 'WARNING (map_bindisp)  Station '// &
     &                     STA_NAM(J2)//' has position variation data '
                   WRITE ( 6, '(A)' ) ' in the position variation file '// &
     &                                POSVAR_FIL(1:I_LEN(POSVAR_FIL))
                   WRITE ( 6, '(A)' ) 'only after '//STR1(1:I_LEN(STR1))// &
     &                                ' what is not enough '
                   WRITE ( 6, '(A)' ) 'for processing '// &
     &                                'a session which has nominal '
                   WRITE ( 6, '(A)' ) 'start at '//STR(1:I_LEN(STR))
!
              END IF
!
! ----------- Not enough data... Nothing to do. Arrays VAL_BDS has already 
! ----------- been filled with zeroes. No data -- get zeroes.
!
              GOTO 420  ! Go to the next station
         END IF
!
! ------ Check whether the index of the last record is OK
!
         IF ( IREC_END .GT. BDSNSA(I_STA(J2)) ) THEN
              STR  = MJDSEC_TO_DATE ( MJD_BEG, TAI_BEG, IER )
              STR1 = MJDSEC_TO_DATE ( BDSLMJ(I_STA(J2)), BDSLSC(I_STA(J2)), IER )
              IF ( FL_WARN ) THEN
                   WRITE ( 6, '(A)' ) 'WARNING (map_bindisp)  Station '// &
     &                     STA_NAM(J2)//' has position variation data '
                   WRITE ( 6, '(A)' ) ' in the position variation file '// &
     &                                POSVAR_FIL(1:I_LEN(POSVAR_FIL))
                   WRITE ( 6, '(A)' ) 'only before '//STR1(1:I_LEN(STR1))// &
     &                                ' what is not enough '
                   WRITE ( 6, '(A)' ) 'for processing '// &
     &                                'a session which has nominal '
                   WRITE ( 6, '(A)' ) 'start at '//STR(1:I_LEN(STR))
              END IF
              IF ( USE_MOD .EQ. PSV__REQ ) THEN
                   CALL ERR_LOG ( 2833, IUER, 'VTD_SET_BINDISP', 'Station '// &
     &                  STA_NAM(J2)//' has position variation data '// &
     &                 'only before '//STR1(1:I_LEN(STR1))//' in the '// &
     &                 'position variation file '// &
     &                 POSVAR_FIL(1:I_LEN(POSVAR_FIL))//' '// &
     &                 'what is not enough for processing session '// &
     &                 ' which has nominal stop at '//STR )
                   RETURN
              END IF
!
! ----------- Not enough data... Nothing to do. Arrays VAL_BDS has already 
! ----------- been filled with zeroes. No data -- get zeroes.
!
              GOTO 420  ! Go to the next station
         END IF
!
! ------ Take 3 nodes more before the first epoch and after the last epoch
! ------ for better interpolation if data permit
!
         IF ( IREC_BEG .GE. 4 ) THEN
              IREC_BEG = IREC_BEG - 3
            ELSE
              IREC_BEG = 1
         END IF
!
         IF ( IREC_END .LE. BDSNSA(I_STA(J2)) - 3 ) THEN
              IREC_END = IREC_END + 3
            ELSE
              IREC_END = BDSNSA(I_STA(J2))
         END IF
!
! ------ L_BDS -- the number of records in the BINDISP file which cover
! ------          the range of interpolation epochs for this session
!
         L_BDS = IREC_END - IREC_BEG + 1
!
! ------ Check whether this range isn't too small
!
         IF ( L_BDS .LT. 2  .AND.  USE_INT .EQ. PSV__LIN ) THEN
!
! ----------- Not enough points for linear interpolation
!
              IF ( IREC_END .LT. BDSNSA(I_STA(J2)) ) THEN
                   IREC_END = IREC_END + 1
                   L_BDS = L_BDS + 1
                 ELSE IF ( IREC_BEG .GT. 1 ) THEN
                   IREC_BEG = IREC_BEG - 1
                   L_BDS = L_BDS + 1
                 ELSE
                   CALL ERR_LOG ( 2834, IUER, 'VTD_SET_BINDISP', 'Trap of '// &
     &                 'internal control' )
                   RETURN
              END IF
           ELSE IF ( L_BDS .LT. 4  .AND.  USE_INT .EQ. PSV__SPL ) THEN
!
! ----------- Not enough points for spline interpolation
!
              IF ( (BDSNSA(I_STA(J2))-IREC_END) .GT. 4-L_BDS ) THEN
                   IREC_END = IREC_END + L_BDS-4
                 ELSE
                   IREC_END = BDSNSA(I_STA(J2))
              END IF
!
              L_BDS = IREC_END - IREC_BEG + 1
              IF ( IREC_BEG-1 .GT. 4-L_BDS ) THEN
                   IREC_BEG = IREC_BEG - (4-L_BDS)
                 ELSE
                   IREC_BEG = 1
              END IF
!
              L_BDS = IREC_END - IREC_BEG + 1
              IF ( L_BDS    .LT. 4                 .OR. &
     &             IREC_BEG .LT. 1                 .OR. &
     &             IREC_END .GT. BDSNSA(I_STA(J2))      ) THEN
!
                   WRITE ( 6, * ) ' IREC_BEG = ',IREC_BEG
                   WRITE ( 6, * ) ' IREC_END = ',IREC_END
                   WRITE ( 6, * ) ' J2=',J2,' I_STA(J2)=',I_STA(J2), &
     &                            ' BDSNSA(I_STA(J2)) = ',BDSNSA(I_STA(J2))
                   CALL ERR_LOG ( 2835, IUER, 'VTD_SET_BINDISP', 'Trap of '// &
     &                 'internal control' )
                   RETURN
              END IF
         END IF
!
         IF ( L_BDS .GT. M__BDS ) THEN
!
! ----------- Too many points
!
              CALL CLRCH ( STR  )
              CALL CLRCH ( STR1 )
              CALL INCH  ( L_BDS,  STR  )
              CALL INCH  ( M__BDS, STR1 )
              CALL ERR_LOG ( 2836, IUER, 'VTD_SET_BINDISP', 'Sampling rate of '// &
     &             BDSFIL(I_STA(J2))(1:I_LEN(BDSFIL(I_STA(J2))))// &
     &            ' is too small, so the session interval covers '// &
     &             STR(1:I_LEN(STR))//' points, what is '// &
     &            'greater than the constant M__BDS: '//STR1 )
              RETURN
         END IF
!
! ------ Open the file with site displacements
!
         LUN = GET_UNIT ()
         OPEN ( UNIT=LUN, FILE=BDSFIL(I_STA(J2)), STATUS='UNKNOWN', &
     &          ACCESS='DIRECT', FORM='UNFORMATTED', RECL=LEN__BDS, &
     &          IOSTAT=IOS )
         IF ( IOS .NE. 0 ) THEN
              CALL CLRCH ( STR )
              CALL INCH  ( IOS, STR )
              CALL ERR_LOG ( 2837, IUER, 'VTD_SET_BINDISP', 'Error '// &
     &             STR(1:I_LEN(STR))//' in opening file '//BDSFIL(I_STA(J2)) )
              RETURN
         END IF
!
! ------ Read the data records from this BINDISP file
!
         I_BDS = 0
         DO 440 J4=IREC_BEG,IREC_END
            READ ( UNIT=LUN, REC=M__HDR+J4 ) BDS
            I_BDS = I_BDS + 1
!
! --------- ARG_BDS -- Time tag of the record as a Julian day (in days)
!
            ARG_BDS_DAY(I_BDS) = MJD_SEC_TO_JD ( BDSFMJ(I_STA(J2)), &
     &                                           (J4-1)*BDSSAM(I_STA(J2)) )
!
! --------- ARG_BDS_SEC -- Time tag of the record in sec elapsed from J2000.0
!
            ARG_BDS_SEC(I_BDS) = ( BDSFMJ(I_STA(J2)) - J2000__MJD )*86400.0D0 + &
     &                           ( BDSFSC(I_STA(J2)) - 43200.0D0  ) +           &
     &                           (J4-1)*BDSSAM(I_STA(J2)) 
            IF ( BDS_ENDIAN .EQ. 'L'  .AND.  ENDIAN__FMT .EQ. 'B' ) THEN
                 CALL ENDIAN_CNV_I2 ( BDS%X_DSP )
                 CALL ENDIAN_CNV_I2 ( BDS%Y_DSP )
                 CALL ENDIAN_CNV_I2 ( BDS%Z_DSP )
               ELSE IF ( BDS_ENDIAN .EQ. 'B'  .AND.  ENDIAN__FMT .EQ. 'L' ) THEN
                 CALL ENDIAN_CNV_I2 ( BDS%X_DSP )
                 CALL ENDIAN_CNV_I2 ( BDS%Y_DSP )
                 CALL ENDIAN_CNV_I2 ( BDS%Z_DSP )
               ELSE IF ( BDS_ENDIAN .NE. ENDIAN__FMT ) THEN
                 CALL ERR_LOG ( 2838, IUER, 'VTD_SET_BINDISP', 'Unsupported '// &
     &               'ENDIAN flag: '//BDS_ENDIAN//' in file position '// &
     &               'variation file '//BDSFIL(I_STA(J2)) )
                 RETURN
            END IF
!
! --------- Extract displacement extension. The extenion sign 
! --------- is in bits 1-3. Extenion occupies bits 4-7, 8-11, 12-15
!
            IF ( BTEST ( BDS%EXT_DSP, 1 ) ) THEN
                 KX = -1
               ELSE
                 KX =  0
            END IF
            IF ( BTEST ( BDS%EXT_DSP, 2 ) ) THEN
                 KY = -1
              ELSE
                 KY =  0
            END IF
            IF ( BTEST ( BDS%EXT_DSP, 3 ) ) THEN
                 KZ = -1
              ELSE
                 KZ =  0
            END IF
!
            CALL MVBITS ( BDS%EXT_DSP,  4, 4, KX,  0 )
            CALL MVBITS ( BDS%EXT_DSP,  8, 4, KY,  0 )
            CALL MVBITS ( BDS%EXT_DSP, 12, 4, KZ,  0 )
!
            XYZ_VEC(I_BDS,1) = BDS%X_DSP*1.D-5 + KX*VTD__BDS_MAX
            XYZ_VEC(I_BDS,2) = BDS%Y_DSP*1.D-5 + KY*VTD__BDS_MAX
            XYZ_VEC(I_BDS,3) = BDS%Z_DSP*1.D-5 + KZ*VTD__BDS_MAX
 440     CONTINUE
!
         CLOSE ( UNIT= LUN )
!
         IF ( USE_INT .EQ. PSV__SPL ) THEN
!
! ----------- Spline interpolation was requested. Let's compute coefficients
! ----------- of interpolation polynomials for X, Y and Z components
!
              CALL ERR_PASS ( IUER, IER )
              CALL MAKE_SPLINE ( 3, L_BDS, ARG_BDS_SEC, XYZ_VEC(1,1), &
     &                           0.0D0, 0.0D0, SPL_BDS(1,1), WORK_SPL, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 2838, IUER, 'VTD_SET_BINDISP', 'Error in '// &
     &                 'interpolating site position variations from file '// &
     &                  BDSFIL(I_STA(J2)) )
                   RETURN
              END IF
!
              CALL MAKE_SPLINE ( 3, L_BDS, ARG_BDS_SEC, XYZ_VEC(1,2), &
     &                           0.0D0, 0.0D0, SPL_BDS(1,2), WORK_SPL, IER )
              CALL MAKE_SPLINE ( 3, L_BDS, ARG_BDS_SEC, XYZ_VEC(1,3), &
     &                           0.0D0, 0.0D0, SPL_BDS(1,3), WORK_SPL, IER )
         END IF
!
! ------ Now we have to compute site displacemetns in M__PSV nodes within
! ------ the session range
!
         DO 450 J5=1,M__PSV
!
! --------- TIM_ARG -- Julian date at the J5-th interpolation node in days
!
            TIM_ARG = TIM_BEG_PSV + TIM_PSV(J5)
!
! --------- Find the leading node
!
            IF ( J5 .EQ. 1 ) THEN
                 NODE = IXMN8   ( L_BDS, ARG_BDS_SEC, TIM_ARG )
               ELSE
                 NODE = IXMN8_S ( NODE, L_BDS, ARG_BDS_SEC, TIM_ARG )
            END IF
            IF ( NODE .LT. 1 ) THEN
                 WRITE ( 6, * ) ' J5=',J5, ' J2=',J2,' NODE=',NODE, &
     &                          ' L_BDS=',L_BDS
                 WRITE ( 6, * ) ' TIM_PSV(J5) = ', TIM_PSV(J5)
                 WRITE ( 6, * ) ' TIM_ARG = ', TIM_ARG
                 WRITE ( 6, * ) ' IREC_BEG=',IREC_BEG,' IREC_END=',IREC_END
                 WRITE ( 6, * ) ' BDSFMJ(I_STA(J2)) = ',BDSFMJ(I_STA(J2)), &
     &                          ' BDSFSC(I_STA(J2)) = ',BDSFSC(I_STA(J2))
                 WRITE ( 6, * ) ' BDSLMJ(I_STA(J2)) = ',BDSLMJ(I_STA(J2)), &
     &                          ' BDSLSC(I_STA(J2)) = ',BDSLSC(I_STA(J2))
                 WRITE ( 6, * ) ' BDSSAM(I_STA(J2)) = ',BDSSAM(I_STA(J2)), &
     &                          ' BDSNSA(I_STA(J2)) = ',BDSNSA(I_STA(J2))
                 WRITE ( 6, * ) ' L_BDS = ',L_BDS
                 WRITE ( 6, * ) ' ARG_BDS_SEC(1) = ',ARG_BDS_SEC(1)
                 WRITE ( 6, * ) ' ARG_BDS_SEC(L_BDS) = ',ARG_BDS_SEC(L_BDS)
                 WRITE ( 6, * ) ' MJD_BEG = ',MJD_BEG, ' MJD_END = ',MJD_END
                 WRITE ( 6, * ) ' TAI_BEG = ',TAI_BEG, ' TAI_END = ',TAI_END
                 WRITE ( 6, * ) ' TIM_BEG_PSV = ', TIM_BEG_PSV
                 WRITE ( 6, * ) ' M__PSV*TIM_STEP_DAY = ', M__PSV*TIM_STEP_DAY
                 WRITE ( 6, * ) ' BDS_BEG = ', &
     &                            MJDSEC_TO_DATE ( BDSFMJ(I_STA(J2)), &
     &                                             BDSFSC(I_STA(J2)) )
                 CALL ERR_LOG ( 2839, IUER, 'VTD_SET_BINDISP', 'Trap of '// &
     &               'internal control. Station '//STA_NAM(J2) )
                 RETURN
            END IF
!
            IF ( USE_INT .EQ. PSV__LIN  .OR.  USE_INT .EQ. PSV__CLS ) THEN
!
! -------------- Compute displacements in the J5-th node with using linear
! -------------- interpolation
!
                 VAL_BDS(J5,1,J2) = FLIN8 ( TIM_ARG, L_BDS, ARG_BDS_SEC, &
     &                                      XYZ_VEC(1,1), NODE )
                 VAL_BDS(J5,2,J2) = FLIN8 ( TIM_ARG, L_BDS, ARG_BDS_SEC, &
     &                                      XYZ_VEC(1,2), NODE )
                 VAL_BDS(J5,3,J2) = FLIN8 ( TIM_ARG, L_BDS, ARG_BDS_SEC, &
     &                                      XYZ_VEC(1,3), NODE )
               ELSE IF ( USE_INT .EQ. PSV__SPL ) THEN
!
! -------------- Compute displacements in the J5-th node with using spline
! -------------- interpolation
!
                 VAL_BDS(J5,1,J2) = FSPL8 ( TIM_ARG, L_BDS, ARG_BDS_SEC, &
     &                                      XYZ_VEC(1,1), NODE, SPL_BDS(1,1) )
                 VAL_BDS(J5,2,J2) = FSPL8 ( TIM_ARG, L_BDS, ARG_BDS_SEC, &
     &                                      XYZ_VEC(1,2), NODE, SPL_BDS(1,2) )
                 VAL_BDS(J5,3,J2) = FSPL8 ( TIM_ARG, L_BDS, ARG_BDS_SEC, &
     &                                      XYZ_VEC(1,3), NODE, SPL_BDS(1,3) )
            END IF
 450     CONTINUE
 420  CONTINUE
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  VTD_SET_BINDISP   #!#
