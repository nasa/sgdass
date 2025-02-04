      SUBROUTINE MAP_POSVAR ( I_PSV, FJDCT_BEG_PSV, TIM_PSV, VAL_PSV, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  MAP_POSVAR  computes array of site position variations    *
! *   according to the I_PSV -th model specified in the batch control    *
! *   file for the time range around the nominal start and nominal stop  *
! *   of the session. It returns the time epochs of the displacements    *
! *   and 3-D displacement for all stations participated in the session. *
! *                                                                      *
! * ________________________ Input parameters: _________________________ *
! *                                                                      *
! *         I_PSV ( INTEGER*4 ) -- Index of the position variation file  *
! *                                in the array POSVAR_FIL (which is     *
! *                                defined in glbc4 block).              *
! *                                                                      *
! * ________________________ Output parameters: ________________________ *
! *                                                                      *
! * FJDCT_BEG_PSV ( REAL*8    ) -- Julian date in TDB time scale of the  *
! *                                first time epoch of the output array. *
! *                                It is set to the nominal session      *
! *                                start minus some amount of the        *
! *                                overhead time defined in OVH__PSV     *
! *                                variable from gfscb.i                 *
! *       TIM_PSV ( REAL*8    ) -- Array of time epochs for the site     *
! *                                position variations as the time       *
! *                                elapsed from FJDCT_BEG_PSV.           *
! *                                Dimension: M__PSV.                    *
! *       VAL_PSV ( REAL*8    ) -- Three-dimensional array of position   *
! *                                variations of all stations            *
! *                                participated in this session          *
! *                                according to the I_PSV -th model of   *
! *                                site position variations defined in   *
! *                                the batch control file. Dimensions:   *
! *                                (M__PSV,3,MAX_ARC_STA).               *
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
! *  ### 17-DEC-2002   MAP_POSVAR  v1.2 (c)  L. Petrov  25-MAR-2005 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
      INCLUDE   'astro_constants.i'
      INCLUDE   'solve.i'
      INCLUDE   'glbc4.i'
      INCLUDE   'bindisp.i'
      INCLUDE   'prfil.i'
      INTEGER*4  I_PSV, IUER
      REAL*8     FJDCT_BEG_PSV, TIM_PSV(M__PSV), VAL_PSV(M__PSV,3,MAX_ARC_STA)
      CHARACTER  FILSUM*128
      LOGICAL*2  FL_WARN_L2
      INTEGER*4  IS, STAT_BLOCK(16), IER
      CHARACTER  STR*32
      INTEGER*2  INT2_ARG
      INTEGER*4  INT4
      INTEGER*4, EXTERNAL :: I_LEN, FOR_STAT
      INT4(INT2_ARG) = INT(INT2_ARG,KIND=4)
!
      IF ( I_PSV .LT. 0  .OR.  I_PSV .GT. M__PSV ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( I_PSV, STR )
           CALL ERR_LOG ( 2811, IUER, 'MAP_POSVAR', 'Wrong value of '// &
     &         'parameter I_PSV: '//STR )
           RETURN
      END IF
!
      IF ( POSVAR_MOD(I_PSV) .EQ. PSV__HMD ) THEN
!
! -------- Compute displacements of all sites in this sessions using the
! -------- model of harminc displacements. Displacements will be computed at
! -------- the M__PSV epochs equally distribited over the range which is
! -------- of the slightly wider than the nominal time interval of the session.
!
           CALL ERR_PASS ( IUER, IER )
           CALL MAP_HARPOS ( N_PSVHAR(I_PSV), N_PSVSTA(I_PSV),    &
     &          FL_WARN_POSVAR(I_PSV),   POSVAR_RD_AREA(I_PSV),   &
     &          %VAL(ADR_NAMSIT(I_PSV)), %VAL(ADR_STACOO(I_PSV)), &
     &          %VAL(ADR_HARVAL(I_PSV)), %VAL(ADR_HARDSP(I_PSV)), &
     &          8, POSVAR_USE(I_PSV), POSVAR_FIL(I_PSV), &
     &          FJDCT_BEG_PSV, TIM_PSV, VAL_PSV, IER )
         ELSE IF ( POSVAR_MOD(I_PSV) .EQ. PSV__TSR ) THEN
!
! -------- Build the name of the summary file
!
           FILSUM = POSVAR_FIL(I_PSV)(1:I_LEN(POSVAR_FIL(I_PSV)))// &
     &              SUMMARY_BDS_FILE
!
! -------- Learn information about the summary file, including date of last
! -------- modification
!
           IS = FOR_STAT ( FILSUM, STAT_BLOCK )
           IF ( IS .NE. 0 ) THEN
                CALL GERROR ( STR )
                CALL ERR_LOG ( 2812, IUER, 'MAP_POSVAR', 'Error '// &
     &               STR(1:I_LEN(STR))//' in STAT of the file '//FILSUM )
                RETURN
           END IF
!
! -------- Check whether the summary file has been modifed since the time it
! -------- was read the first time?
!
           IF ( STAT_BLOCK(10) .GT. TIM_PSVFIL(I_PSV) ) THEN
!
! ------------- We learned that it was changed. Them we free dymanic memory
! ------------- allocated for the data structures used for keeping information
! ------------- from the summary file
!
                CALL FREE ( ADR_NAMSIT(I_PSV) )
!
! ------------- And then re-load the summary file once again.
!
                CALL ERR_PASS ( IUER, IER )
                CALL LOAD_BINDISP ( I_PSV, IER )
                IF ( IER .NE. 0 ) THEN
                     CALL ERR_LOG ( 2813, IUER, 'MAP_POSVAR', 'Error '// &
     &                   'In an attempt to re-read and parse summary file '// &
     &                    FILSUM )
                     RETURN
                END IF
           END IF
!
! -------- Compute displacements of all sites in this sessions using the
! -------- time series of displacements. Displacements will be computed at
! -------- the M__PSV epochs equally disstribited over the range which is
! -------- of the slightly wider than the nominal time interval of the session.
! -------- Displacements are computed at this grid using either linear or
! -------- spline interpolation depending on  POSVAR_INT(I_PSV)
!
           CALL ERR_PASS ( IUER, IER )
           CALL MAP_BINDISP ( N_PSVSTA(I_PSV), FL_WARN_POSVAR(I_PSV), &
     &          POSVAR_RD_AREA(I_PSV),   %VAL(ADR_NAMSIT(I_PSV)), &
     &          %VAL(ADR_STACOO(I_PSV)), %VAL(ADR_BDSFIL(I_PSV)), &
     &          %VAL(ADR_BDSSAM(I_PSV)), %VAL(ADR_BDSFMJ(I_PSV)), &
     &          %VAL(ADR_BDSFSC(I_PSV)), %VAL(ADR_BDSLMJ(I_PSV)), &
     &          %VAL(ADR_BDSLSC(I_PSV)), %VAL(ADR_BDSNSA(I_PSV)), &
     &          BDS_ENDIAN(I_PSV), 8, 128, POSVAR_FIL(I_PSV), &
     &          POSVAR_USE(I_PSV), POSVAR_INT(I_PSV), &
     &          FJDCT_BEG_PSV, TIM_PSV, VAL_PSV, IER )
         ELSE
           CALL CLRCH ( STR )
           CALL INCH  ( I_PSV, STR )
           CALL ERR_LOG ( 2814, IUER, 'MAP_POSVAR', 'Wrong value of '// &
     &         'parameter I_PSV: '//STR )
           RETURN
      END IF
!
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 2815, IUER, 'MAP_POSVAR', 'Error in an attempt to '// &
     &         'compute position variations at the nodes of the '// &
     &         'interpolation range' )
           RETURN
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  MAP_POSVAR  #!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE MAP_HARPOS ( N_HAR, N_SIT, FL_WARNING, POSVAR_RD_AREA, &
     &                        NAMSIT, STACOO, HARVAL, HARDSP, LEN_NAM, &
     &                        USE_MOD, POSVAR_FIL, FJDCT_BEG_PSV, TIM_PSV, &
     &                        VAL_PSV, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  MAP_HARPOS  computes site position variations according   *
! *   to the model of harmonic position variations for the set of time   *
! *   epochs around the time range of the session for all stations which *
! *   participated in the session. It returns vectors of 3-D             *
! *   displacements.                                                     *
! *                                                                      *
! * ________________________ Input parameters: _________________________ *
! *                                                                      *
! *        N_HAR ( INTEGER*4 ) -- The number of harmonics in the model   *
! *                               of site position variations.           *
! *        N_SIT ( INTEGER*4 ) -- The number of sites in the harmonic    *
! *                               model of site position  variations.    *
! *   FL_WARNING ( LOGICAL*4 ) -- Flag: if .TRUE., then a warning will   *
! *                               be set in the screen and out in the    *
! *                               spool file if needed. If .FALSE. then  *
! *                               no warning message will be generated.  *
! *  POSVAR_RD_AREA ( REAL*8 ) -- The radius of the area for which       *
! *                               the displacement is applicable.        *
! *       NAMSIT ( CHARACTER ) -- Array of site names defined in the     *
! *                               harmonic site position file. NB: these *
! *                               names do not necessarily coincides     *
! *                               with the IVS station names.            *
! *                               Dimension: N_SIT.                      *
! *       STACOO ( REAL*8    ) -- Arrays of site coordinates in a crust  *
! *                               reference frame. Dimension: (3,N_SIT). *
! *       HARVAL ( REAL*8    ) -- Two-dimensional array of phases,       *
! *                               frequencies and accelerations for each *
! *                               harmonic. Dimension: (3,N_HAR).        *
! *       HARDSP ( REAL*8    ) -- Four-dimensional array of site         *
! *                               position displacements. The first      *
! *                               index runs through Up, East, North     *
! *                               component of a displacement vector.    *
! *                               The second index runs through cosine   *
! *                               (1) and sine (2) mode, the third index *
! *                               runs through harmonics, and the fourth *
! *                               index runs through stations.           *
! *                               Dimension: 3,2,N_HAR,N_SIT             *
! *      LEN_NAM ( INTEGER*4 ) -- The length of the site name string     *
! *                               in bytes.                              *
! *      USE_MOD ( INTEGER*4 ) -- Flag of usage mode. If a station was   *
! *                               not find in the site list for harmonic *
! *                               site position variations model, then   *
! *                               depending on USE_MOD the following     *
! *                               will be done:                          *
! *                               1) USE_MOD = PSV__REQ -- then the      *
! *                                  error message will be issued and    *
! *                                  the process will be terminated.     *
! *                               2) USE_MOD = PSV__IFA -- then          *
! *                                  a warning message will be put in    *
! *                                  screen and in spool file if         *
! *                                  FL_WARNING is true and then the     *
! *                                  process will continue with setting  *
! *                                  harmonic position variations for    *
! *                                  this site to zero.                  *
! *   POSVAR_FIL ( CHARACTER ) -- The name of the harmonic site position *
! *                               variations file. Used for generating   *
! *                               error messages only.                   *
! *                                                                      *
! * ________________________ Output parameters: ________________________ *
! *                                                                      *
! * FJDCT_BEG_PSV ( REAL*8    ) -- Julian date in TDB time scale of the  *
! *                                first time epoch of the output array. *
! *                                It is set to the nominal session      *
! *                                start minus some amount of the        *
! *                                overhead time defined in OVH__PSV     *
! *                                variable from gfscb.i                 *
! *       TIM_PSV ( REAL*8    ) -- Array of time epochs for the site     *
! *                                position variations as the time       *
! *                                elapsed from FJDCT_BEG_PSV.           *
! *                                Dimension: M__PSV.                    *
! *       VAL_PSV ( REAL*8    ) -- Three-dimensional array of position   *
! *                                variations of all stations            *
! *                                participated in this session          *
! *                                on the epochs defined in array        *
! *                                TIM_PSV.                              *
! *                                Dimensions: (M__PSV,3,MAX_ARC_STA).   *
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
! *  ### 17-DEC-2002   MAP_HARPOS  v1.3 (c)  L. Petrov  28-MAR-2005 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
      INCLUDE   'astro_constants.i'
      INCLUDE   'solve.i'
      INCLUDE   'socom.i'
      INCLUDE   'bindisp.i'
      INCLUDE   'prfil.i'
      INTEGER*4  N_HAR, N_SIT, LEN_NAM, USE_MOD, IUER
      CHARACTER  NAMSIT(N_SIT)*(LEN_NAM), POSVAR_FIL*128
      REAL*8     POSVAR_RD_AREA, STACOO(3,N_SIT), HARVAL(3,N_HAR), &
     &           HARDSP(3,2,N_HAR,N_SIT), FJDCT_BEG_PSV, TIM_PSV(M__PSV), &
     &           VAL_PSV(M__PSV,3,MAX_ARC_STA)
      LOGICAL*4  FL_WARNING
      REAL*8     FJD_BEG, FJD_END, TIM_STEP_SEC, TIM_STEP_DAY
      REAL*8     TIM_ARG, HAR_ARG, UEN_TO_XYZ(3,3,MAX_ARC_STA), UEN_VEC_IN(3), &
     &           UEN_VEC_OUT(3), XYZ_VEC_IN(3), XYZ_VEC_OUT(3), DIST_SQ_MIN
      INTEGER*4  I_STA(MAX_ARC_STA), J1, J2, J3, J4, J5, IER
!
      INTEGER*2  INT2_ARG
      INTEGER*4  INT4
      LOGICAL*4, EXTERNAL :: CHECK_STABIT
      INTEGER*4, EXTERNAL :: I_LEN
      INT4(INT2_ARG) = INT(INT2_ARG,KIND=4)
!
! --- Get nominal start ( FJD_BEG ) and nominal stop ( FJD_END ) time epoch
!
      CALL OBSTM ( FJD_BEG, FJD_END )
!
! --- Compute the first epoch of the time interval for which harmonic
! --- displacement is to be computed
!
      FJDCT_BEG_PSV = FJD_BEG - OVH__PSV/86400.0D0
!
! --- Get the step of the the array of time epochs
!
      TIM_STEP_SEC = ( (FJD_END - FJD_BEG)*86400.0 + 2.0D0*OVH__PSV )/(M__PSV-1)
      TIM_STEP_DAY = TIM_STEP_SEC/86400.0D0
!
      DO 410 J1=1,INT4(NUMSTA) ! Cycle over the participated stations
!
! ------ Compute the matrix of transformation from the local
! ------ topocentric reference system (UEN) to the crust fixed
! ------ reference system (XYZ)
!
         CALL MAKE_UEN_TO_XYZ ( VSITEC(1,J1), UEN_TO_XYZ(1,1,J1) )
!
! ------ Search for the site which is the closest to the J1-th station
!
         DIST_SQ_MIN = (6378136.7D0)**2
         I_STA(J1) = 0
         DO 420 J2=1,N_SIT
            IF ( (STACOO(1,J2)-VSITEC(1,J1))**2 + &
     &           (STACOO(2,J2)-VSITEC(2,J1))**2 + &
     &           (STACOO(3,J2)-VSITEC(3,J1))**2    .LT. DIST_SQ_MIN ) THEN
!
                 I_STA(J1) = J2
                 DIST_SQ_MIN = (STACOO(1,J2)-VSITEC(1,J1))**2 + &
     &                         (STACOO(2,J2)-VSITEC(2,J1))**2 + &
     &                         (STACOO(3,J2)-VSITEC(3,J1))**2
            END IF
 420     CONTINUE
!
         IF ( DIST_SQ_MIN .LT. POSVAR_RD_AREA**2 ) THEN
!
! ----------- Store I_STA -- the index of the J2-th site in HARPOS
! ----------- site array which is the closest to the J1-th station
! ----------- participatingin this VLBI experiment
!
              CONTINUE
            ELSE
!
! ----------- Mmmm. No close station was found. If this station is seclected,
! ----------- it is terrible.
!
              IF ( CHECK_STABIT ( INT2(J1) ) ) THEN
                   IF ( ( USE_MOD .EQ. PSV__REQ  .OR. &
     &                  ( USE_MOD .EQ. PSV__AVL  .AND.  FL_WARNING ) ) ) THEN
                        WRITE ( 23, '(A)' ) 'WARNING (map_harpos)  Station '// &
     &                         ISITN_CHR(J1)//' was not found in the '// &
     &                        'position variation file '// &
     &                         POSVAR_FIL(1:I_LEN(POSVAR_FIL))
                        WRITE ( 6, '(A)' ) 'WARNING (map_harpos)  Station '// &
     &                         ISITN_CHR(J1)//' was not found in the '// &
     &                        'position variation file '// &
     &                         POSVAR_FIL(1:I_LEN(POSVAR_FIL))
                        IF ( USE_MOD .EQ. PSV__REQ ) THEN
                             CALL ERR_LOG ( 2821, IUER, 'MAP_HARPOS', &
     &                           'Station '//ISITN_CHR(J1)//' was not found '// &
     &                           'in the position variation file '//POSVAR_FIL )
                             RETURN
                        END IF
                   END IF
              END IF
              I_STA(J1) = 0
         END IF
 410  CONTINUE
!
! --- Now cycle over the epochs
!
      DO 430 J3=1,M__PSV
!
! ------ Get time argument TIM_PSV -- time in seconds elapsed from the
! ------ beginning the interval
!
         TIM_PSV(J3) = (J3-1)*TIM_STEP_SEC
!
! ------ Get the time argument for computing harmonic site position variations:
! ------ time in seconds elapsed from the J2000.0 epoch
!
         TIM_ARG = ( FJDCT_BEG_PSV - J2000__JD )*86400.0D0 + TIM_PSV(J3)
         DO 440 J4=1,N_HAR
!
! --------- Compute the argument for the harmonic function
!
            HAR_ARG = ( HARVAL(3,J4)*TIM_ARG/2.0D0 + HARVAL(2,J4) )*TIM_ARG + &
     &                  HARVAL(1,J4)
            DO 450 J5=1,INT4(NUMSTA)
               IF ( I_STA(J5) > 0 ) THEN
!
! ----------------- For each station get the cosine and sine component of the
! ----------------- displacement vector
!
                    UEN_VEC_IN(1)  = HARDSP(1,1,J4,I_STA(J5))*DCOS(HAR_ARG)
                    UEN_VEC_IN(2)  = HARDSP(2,1,J4,I_STA(J5))*DCOS(HAR_ARG)
                    UEN_VEC_IN(3)  = HARDSP(3,1,J4,I_STA(J5))*DCOS(HAR_ARG)
!
                    UEN_VEC_OUT(1) = HARDSP(1,2,J4,I_STA(J5))*DSIN(HAR_ARG)
                    UEN_VEC_OUT(2) = HARDSP(2,2,J4,I_STA(J5))*DSIN(HAR_ARG)
                    UEN_VEC_OUT(3) = HARDSP(3,2,J4,I_STA(J5))*DSIN(HAR_ARG)
!
! ----------------- Transform the displacement vector from the local topocentric
! ----------------- coordinate system to the crust-fixed coordinate system
!
                    CALL MUL_MV_IV_V ( 3, 3, UEN_TO_XYZ(1,1,J5), &
     &                                 3, UEN_VEC_IN, 3, XYZ_VEC_IN, IER )
                    CALL MUL_MV_IV_V ( 3, 3, UEN_TO_XYZ(1,1,J5), &
     &                                 3, UEN_VEC_OUT, 3, XYZ_VEC_OUT, IER )
!
! ----------------- Add the displacement to displacements computed before this
! ----------------- call of MAP_HARPOS
!
                    VAL_PSV(J3,1,J5) = VAL_PSV(J3,1,J5) + XYZ_VEC_IN(1) + &
     &                                                    XYZ_VEC_OUT(1)
                    VAL_PSV(J3,2,J5) = VAL_PSV(J3,2,J5) + XYZ_VEC_IN(2) + &
     &                                                    XYZ_VEC_OUT(2)
                    VAL_PSV(J3,3,J5) = VAL_PSV(J3,3,J5) + XYZ_VEC_IN(3) + &
     &                                                    XYZ_VEC_OUT(3)
               END IF
 450        CONTINUE
 440     CONTINUE
 430  CONTINUE
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  MAP_HARPOS  #!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE MAP_BINDISP ( N_SIT, FL_WARNING, POSVAR_RD_AREA, NAMSIT, &
     &                         STACOO, BDSFIL, BDSSAM, BDSFMJ, BDSFSC, &
     &                         BDSLMJ, BDSLSC, BDSNSA, BDS_ENDIAN, LEN_NAM, &
     &                         LEN_FIL, POSVAR_FIL, USE_MOD, USE_INT, &
     &                         FJDCT_BEG_PSV, TIM_PSV, DSP_BDS, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine MAP_BINDISP  computes an arrays of site position           *
! *   variations for the set of time epochss within the time range of    *
! *   the session for all stations which participated in the session.    *
! *   Site displacement are taken from the external file with time       *
! *   series of site positin varaitions in BINDISP format. These         *
! *   external time series are re-sampled using linear or spline         *
! *   interpolation. MAP_BINDISP returns vectors of 3-D displacements.   *
! *                                                                      *
! * ________________________ Input parameters: _________________________ *
! *                                                                      *
! *        N_SIT ( INTEGER*4 ) -- The number of sites in the model of    *
! *                               site position  variations.             *
! *   FL_WARNING ( LOGICAL*4 ) -- Flag: if .TRUE., then a warning will   *
! *                               be set in the screen and out in the    *
! *                               spool file if needed. If .FALSE. then  *
! *                               no warning message will be generated.  *
! *  POSVAR_RD_AREA ( REAL*8 ) -- The radius of the area for which       *
! *                               the displacement is applicable.        *
! *       NAMSIT ( CHARACTER ) -- Array of site names defined in the     *
! *                               harmonic site position file. NB: these *
! *                               names do not necessarily coincides     *
! *                               with the IVS station names.            *
! *                               Dimension: N_SIT.                      *
! *       STACOO ( REAL*8    ) -- Arrays of site coordinates in a crust  *
! *                               reference frame. Dimension: (3,N_SIT). *
! *       BDSFIL ( CHARACTER ) -- Array of full names including path of  *
! *                               the files with site position           *
! *                               variations time seriesin BINDISP       *
! *                               format. Dimension: N_STA.              *
! *       BDSSAM ( REAL*8    ) -- Array of sampling intervals for each   *
! *                               site position variations time series   *
! *                               files. Dimension: N_STA.               *
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
! *                                  FL_WARNING is true and then the     *
! *                                  process will continue with setting  *
! *                                  harmonic position variations for    *
! *                                  this site to zero.                  *
! *      USE_INT ( INTEGER*4 ) -- Interpolation mode. Two modes are      *
! *                               supported: PSV__LIN -- then linear     *
! *                               interpolation is used for computing    *
! *                               position variations on the nodes of    *
! *                               the output array TIM_PSV. If PSV__SPL  *
! *                               then spline interpolation is in use.   *
! *                                                                      *
! * ________________________ Output parameters: ________________________ *
! *                                                                      *
! * FJDCT_BEG_PSV ( REAL*8    ) -- Julian date in TDB time scale of the  *
! *                                first time epoch of the output array. *
! *                                It is set to the nominal session      *
! *                                start minus some amount of the        *
! *                                overhead time defined in OVH__PSV     *
! *                                variable from gfscb.i                 *
! *       TIM_PSV ( REAL*8    ) -- Array of time epochs for the site     *
! *                                position variations as the time       *
! *                                elapsed from FJDCT_BEG_PSV.           *
! *                                Dimension: M__PSV.                    *
! *       VAL_PSV ( REAL*8    ) -- Three-dimensional array of position   *
! *                                variations of all stations            *
! *                                participated in this session          *
! *                                on the epochs defined in array        *
! *                                TIM_PSV.                              *
! *                                Dimensions: (M__PSV,3,MAX_ARC_STA).   *
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
! *  ### 18-DEC-2002   MAP_BINDISP  v1.3 (c) L. Petrov  28-MAR-2005 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
      INCLUDE   'solve.i'
      INCLUDE   'socom.i'
      INCLUDE   'prfil.i'
      INCLUDE   'bindisp.i'
      TYPE ( BINDISP_DATA ) ::  BDS
      INTEGER*4  N_SIT, BDSFMJ(N_SIT), BDSLMJ(N_SIT), BDSNSA(N_SIT), LEN_NAM, &
     &           LEN_FIL, USE_MOD, USE_INT, IUER
      CHARACTER  NAMSIT(N_SIT)*(LEN_NAM), BDSFIL(N_SIT)*(LEN_FIL), &
     &           POSVAR_FIL*128, BDS_ENDIAN*1
      REAL*8     POSVAR_RD_AREA, STACOO(3,N_SIT), BDSSAM(N_SIT), &
     &           BDSFSC(N_SIT), BDSLSC(N_SIT), FJDCT_BEG_PSV, &
     &           TIM_PSV(M__PSV), DSP_BDS(M__PSV,3,MAX_ARC_STA)
      LOGICAL*4  FL_WARNING
      REAL*8     FJD_BEG, FJD_END, TIM_STEP_SEC, TIM_STEP_DAY
      REAL*8     TIM_ARG, ARG_BDS(M__BDS), XYZ_VEC(M__BDS,3), &
     &           SPL_BDS(M__BDS,3), WORK_SPL(M__BDS), DIST_SQ_MIN
      INTEGER*2  IDBV(15), LDBNAM(5,15), NUMD_I2
      CHARACTER  CDBNAM(15)*10, STR*128, STR1*128
      EQUIVALENCE ( LDBNAM, CDBNAM )
      INTEGER*4  IDBE(15)
      INTEGER*4  I_STA(MAX_ARC_STA), IREC_BEG, IREC_END, IOS, NODE, I_BDS, &
     &           L_BDS, LUN, J1, J2, J3, J4, J5, IER
      CHARACTER  ENDIAN__FMT*1
#ifdef BIG_ENDIAN
      PARAMETER  ( ENDIAN__FMT = 'B' ) 
#else
      PARAMETER  ( ENDIAN__FMT = 'L' ) 
#endif
!
      INTEGER*2  INT2_ARG
      INTEGER*4  INT4
      CHARACTER, EXTERNAL :: JD_TO_DATE*23
      REAL*8,    EXTERNAL :: FLIN8, FSPL8, MJD_SEC_TO_JD
      INTEGER*4, EXTERNAL :: GET_UNIT, I_LEN, IXMN8, IXMN8_S
      INT4(INT2_ARG) = INT(INT2_ARG,KIND=4)
!
! --- Get the Julian date of the nominal start and nominal stop of the session
!
      CALL OBSTM ( FJD_BEG, FJD_END )
!
! --- Compute the first epoch of the time interval for which site
! --- displacement is to be computed
!
      FJDCT_BEG_PSV = FJD_BEG - OVH__PSV/86400.0D0
!
! --- Get the step of the the array of time epochs
!
      TIM_STEP_SEC = ( (FJD_END - FJD_BEG)*86400.0 + 2.0D0*OVH__PSV )/(M__PSV-1)
      TIM_STEP_DAY = TIM_STEP_SEC/86400.0D0
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
      CALL NOUT_R8 ( M__PSV*3*MAX_ARC_STA, DSP_BDS )
!
      DO 420 J2=1,INT4(NUMSTA)
!
! ------ Search for the site which is the closest to the J2-th station
!
         DIST_SQ_MIN = (6378136.7D0)**2
         I_STA(J2) = 0
         DO 430 J3=1,N_SIT
            IF ( (STACOO(1,J3)-VSITEC(1,J2))**2 + &
     &           (STACOO(2,J3)-VSITEC(2,J2))**2 + &
     &           (STACOO(3,J3)-VSITEC(3,J2))**2    .LT. DIST_SQ_MIN ) THEN
!
                 I_STA(J2) = J3
                 DIST_SQ_MIN = (STACOO(1,J3)-VSITEC(1,J2))**2 + &
     &                         (STACOO(2,J3)-VSITEC(2,J2))**2 + &
     &                         (STACOO(3,J3)-VSITEC(3,J2))**2
            END IF
 430     CONTINUE
!
         IF ( I_STA(J2) .EQ. 0  .OR.  DIST_SQ_MIN .GT. POSVAR_RD_AREA**2 ) THEN
!
! ----------- The station was not been found in the list. Well, let's issue
! ----------- a warning and go ahead
!
              IF ( USE_MOD .EQ. PSV__REQ  .OR. &
     &             ( USE_MOD .EQ. PSV__AVL  .AND.  FL_WARNING ) ) THEN
!
                   NUMD_I2 = 1
                   CALL DBPOX ( NUMD_I2, LDBNAM, IDBV, IDBE )
                   WRITE ( 23, '(A)' ) 'WARNING (map_bindisp)  No position '// &
     &                   'variations for station '//ISITN_CHR(J2)// &
     &                   ' was found'
                   WRITE ( 23, '(A,I4,A)' ) '                       in a '// &
     &                   'in the position variation file '//   &
     &                    POSVAR_FIL(1:I_LEN(POSVAR_FIL))//' of ', N_SIT, &
     &                   ' sites'
                   WRITE ( 23, '(A)' ) '                      when a '//      &
     &                                 'session '//CDBNAM(1)//' was being '// &
     &                                 'processed'
!
                   IF ( USE_MOD .EQ. PSV__REQ ) THEN
                        CALL ERR_LOG ( 2831, IUER, 'MAP_BINDISP', 'Station '// &
     &                       ISITN_CHR(J2)//' was not found in the '//         &
     &                       ' position variation file '//                     &
     &                        POSVAR_FIL(1:I_LEN(POSVAR_FIL))//' when '//      &
     &                       'a session '//CDBNAM(1)//' was being processed' )
                        RETURN
                      ELSE IF ( FL_WARNING ) THEN
                        WRITE ( 6, '(A)' ) 'WARNING (map_bindisp)  No '// &
     &                   'position variation for station '//ISITN_CHR(J2)// &
     &                   ' was found'
                        WRITE ( 6, '(A,I4,A)' ) '                       '// &
     &                   'in the position variation file '//   &
     &                    POSVAR_FIL(1:I_LEN(POSVAR_FIL))//' of ', N_SIT, &
     &                   ' sites'
                        WRITE ( 6, '(A)' ) '                      when a '// &
     &                    'session '//CDBNAM(1)//' was being '// &
     &                    'processed'
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
         IREC_BEG = ( FJDCT_BEG_PSV - &
     &                MJD_SEC_TO_JD ( BDSFMJ(I_STA(J2)), BDSFSC(I_STA(J2)))) &
     &              / BDSSAM(I_STA(J2)) + 1
         IREC_END = ( FJDCT_BEG_PSV + M__PSV*TIM_STEP_DAY - &
     &                MJD_SEC_TO_JD ( BDSFMJ(I_STA(J2)), BDSFSC(I_STA(J2)))) &
     &              / BDSSAM(I_STA(J2)) + 2
!
! ------ Check whether the index of the first record is OK
!
         IF ( IREC_BEG .LT. 1 ) THEN
              IF ( USE_MOD .EQ. PSV__REQ  .OR. &
     &             ( USE_MOD .EQ. PSV__AVL  .AND.  FL_WARNING ) ) THEN
!
                   NUMD_I2 = 1
                   CALL DBPOX ( NUMD_I2, LDBNAM, IDBV, IDBE )
                   STR  = JD_TO_DATE ( FJD_BEG, 0 )
                   STR1 = JD_TO_DATE ( MJD_SEC_TO_JD ( BDSFMJ(I_STA(J2)), &
     &             BDSFSC(I_STA(J2)) ), 0 )
!
                   WRITE ( 23, '(A)' ) 'WARNING (map_bindisp)  Station '// &
     &                     ISITN_CHR(J2)//' has position variation data '
                   WRITE ( 23, '(A)' ) '                      in the '// &
     &                    'position variation file '// &
     &                    POSVAR_FIL(1:I_LEN(POSVAR_FIL))
                   WRITE ( 23, '(A)' ) '                      only after '// &
     &                                 STR1(1:I_LEN(STR1))// &
     &                                 ' what is not enough '
                   WRITE ( 23, '(A)' ) '                      for processing '// &
     &                                 'session '//CDBNAM(1)//' which has '// &
     &                                 'nominal '
                   WRITE ( 23, '(A)' ) '                      start at '// &
     &                                 STR(1:I_LEN(STR))
!
                   IF ( USE_MOD .EQ. PSV__REQ ) THEN
                        CALL ERR_LOG ( 2832, IUER, 'MAP_BINDISP', 'Station '// &
     &                       ISITN_CHR(J2)//' has position variation data '// &
     &                       'only after '//STR1(1:I_LEN(STR1))// &
     &                       ' in the position variation file '// &
     &                        POSVAR_FIL(1:I_LEN(POSVAR_FIL))// &
     &                       ' what is not enough for processing session '// &
     &                       CDBNAM(1)//' which has nominal start at '//STR )
                        RETURN
                      ELSE IF ( FL_WARNING ) THEN
                        WRITE ( 6, '(A)' ) 'WARNING (map_bindisp)  Station '// &
     &                                      ISITN_CHR(J2)// &
     &                                     ' has position variation data '
                        WRITE ( 6, '(A)' ) '                      in the '// &
     &                                     'position variation file '// &
     &                                     POSVAR_FIL(1:I_LEN(POSVAR_FIL))
                        WRITE ( 6, '(A)' ) '                      only after '// &
     &                                     STR1(1:I_LEN(STR1))// &
     &                                     ' what is not enough '
                        WRITE ( 6, '(A)' ) '                      for '// &
     &                                     'processing session '//CDBNAM(1)// &
     &                                     ' which has nominal '
                        WRITE ( 6, '(A)' ) '                      start at '// &
     &                                     STR(1:I_LEN(STR))
                   END IF
              END IF
!
! ----------- Not enough data... Nothing to do. Arrays DSP_BDS has already
! ----------- been filled with zeroes. No data -- get zeroes.
!
              GOTO 420  ! Go to the next station
         END IF
!
! ------ Check whether the index of the last record is OK
!
         IF ( IREC_END .GT. BDSNSA(I_STA(J2)) ) THEN
              IF ( USE_MOD .EQ. PSV__REQ  .OR. &
     &             ( USE_MOD .EQ. PSV__AVL  .AND.  FL_WARNING ) ) THEN
                   NUMD_I2 = 1
                   CALL DBPOX ( NUMD_I2, LDBNAM, IDBV, IDBE )
                   STR  = JD_TO_DATE ( FJD_END, 0 )
                   STR1 = JD_TO_DATE ( MJD_SEC_TO_JD ( BDSLMJ(I_STA(J2)), &
     &             BDSLSC(I_STA(J2)) ), 0 )
!
                   WRITE ( 23, '(A)' ) 'WARNING (map_bindisp)  Station '// &
     &                     ISITN_CHR(J2)//' has position variation data '
                   WRITE ( 23, '(A)' ) '                      in the '// &
     &                    'position variation file '// &
     &                    POSVAR_FIL(1:I_LEN(POSVAR_FIL))
                   WRITE ( 23, '(A)' ) '                      only before '// &
     &                                 STR1(1:I_LEN(STR1))// &
     &                                 ' what is not enough '
                   WRITE ( 23, '(A)' ) '                      for processing '// &
     &                                 'session '//CDBNAM(1)//' which has '// &
     &                                 'nominal '
                   WRITE ( 23, '(A)' ) '                      stop at '// &
     &                                 STR(1:I_LEN(STR))
!
                   IF ( USE_MOD .EQ. PSV__REQ ) THEN
                        CALL ERR_LOG ( 2833, IUER, 'MAP_BINDISP', 'Station '// &
     &                       ISITN_CHR(J2)//' has position variation data '// &
     &                       'only before '//STR1(1:I_LEN(STR1))//' in the '// &
     &                       'position variation file '// &
     &                        POSVAR_FIL(1:I_LEN(POSVAR_FIL))//' '// &
     &                       'what is not enough for processing session '// &
     &                       CDBNAM(1)//' which has nominal stop at '//STR )
                        RETURN
                      ELSE IF ( FL_WARNING ) THEN
                        WRITE ( 6, '(A)' ) 'WARNING (map_bindisp)  Station '// &
     &                     ISITN_CHR(J2)//' has position variation data '
                        WRITE ( 6, '(A)' ) '                      in the '// &
     &                                     'position variation file '// &
     &                                      POSVAR_FIL(1:I_LEN(POSVAR_FIL))
                        WRITE ( 6, '(A)' ) '                      '// &
     &                                     'only before '//STR1(1:I_LEN(STR1))// &
     &                                     ' what is not enough '
                        WRITE ( 6, '(A)' ) '                      for '// &
     &                                 'processing session '//CDBNAM(1)// &
     &                                 ' which has nominal '
                        WRITE ( 6, '(A)' ) '                      stop at '// &
     &                                 STR(1:I_LEN(STR))
                   END IF
              END IF
!
! ----------- Not enough data... Nothing to do. Arrays DSP_BDS has already
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
                   CALL ERR_LOG ( 2834, IUER, 'MAP_BINDISP', 'Trap of '// &
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
                   CALL ERR_LOG ( 2835, IUER, 'MAP_BINDISP', 'Trap of '// &
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
              CALL ERR_LOG ( 2836, IUER, 'MAP_BINDISP', 'Sampling rate of '// &
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
              CALL ERR_LOG ( 2837, IUER, 'MAP_BINDISP', 'Error '// &
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
            ARG_BDS(I_BDS) = MJD_SEC_TO_JD ( BDSFMJ(I_STA(J2)), &
     &                                      (J4-1)*BDSSAM(I_STA(J2))*86400.0D0 )
!
            IF ( BDS_ENDIAN .EQ. 'L'  .AND.  ENDIAN__FMT .EQ. 'B' ) THEN
                 CALL ENDIAN_CNV_I2 ( BDS%X_DSP )
                 CALL ENDIAN_CNV_I2 ( BDS%Y_DSP )
                 CALL ENDIAN_CNV_I2 ( BDS%Z_DSP )
               ELSE IF ( BDS_ENDIAN .EQ. 'B'  .AND.  ENDIAN__FMT .EQ. 'L' ) THEN
                 CALL ENDIAN_CNV_I2 ( BDS%X_DSP )
                 CALL ENDIAN_CNV_I2 ( BDS%Y_DSP )
                 CALL ENDIAN_CNV_I2 ( BDS%Z_DSP )
               ELSE IF ( BDS_ENDIAN .NE. ENDIAN__FMT ) THEN
                 CALL ERR_LOG ( 2838, IUER, 'MAP_BINDISP', 'Unsupported '// &
     &               'ENDIAN flag: '//BDS_ENDIAN//' in file position '// &
     &               'variation file '//BDSFIL(I_STA(J2)) )
                 RETURN
            END IF
            XYZ_VEC(I_BDS,1) = BDS%X_DSP*1.D-5
            XYZ_VEC(I_BDS,2) = BDS%Y_DSP*1.D-5
            XYZ_VEC(I_BDS,3) = BDS%Z_DSP*1.D-5
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
              CALL MAKE_SPLINE ( 3, L_BDS, ARG_BDS, XYZ_VEC(1,1), 0.0D0, 0.0D0, &
     &                           SPL_BDS(1,1), WORK_SPL, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 2838, IUER, 'MAP_BINDISP', 'Error in '// &
     &                 'interpolating site position variations from file '// &
     &                  BDSFIL(I_STA(J2)) )
                   RETURN
              END IF
!
              CALL MAKE_SPLINE ( 3, L_BDS, ARG_BDS, XYZ_VEC(1,2), 0.0D0, 0.0D0, &
     &                           SPL_BDS(1,2), WORK_SPL, IER )
              CALL MAKE_SPLINE ( 3, L_BDS, ARG_BDS, XYZ_VEC(1,3), 0.0D0, 0.0D0, &
     &                           SPL_BDS(1,3), WORK_SPL, IER )
         END IF
!
! ------ Now we have to compute site displacemetns in M__PSV nodes within
! ------ the session range
!
         DO 450 J5=1,M__PSV
!
! --------- TIM_ARG -- Julian date at the J5-th interpolation node in days
!
            TIM_ARG = FJDCT_BEG_PSV + TIM_PSV(J5)/86400.0D0
!
! --------- Find the leading node
!
            IF ( J5 .EQ. 1 ) THEN
                 NODE = IXMN8   ( L_BDS, ARG_BDS, TIM_ARG )
               ELSE
                 NODE = IXMN8_S ( NODE, L_BDS, ARG_BDS, TIM_ARG )
            END IF
            IF ( NODE .LT. 1 ) THEN
                 NUMD_I2 = 1
                 CALL DBPOX ( NUMD_I2, LDBNAM, IDBV, IDBE )
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
                 WRITE ( 6, * ) ' ARG_BDS(1) = ',ARG_BDS(1)
                 WRITE ( 6, * ) ' ARG_BDS(L_BDS) = ',ARG_BDS(L_BDS)
                 WRITE ( 6, * ) ' FJD_BEG = ',FJD_BEG,' FJD_END = ',FJD_END
                 WRITE ( 6, * ) ' FJDCT_BEG_PSV = ', FJDCT_BEG_PSV
                 WRITE ( 6, * ) ' M__PSV*TIM_STEP_DAY = ', M__PSV*TIM_STEP_DAY
                 WRITE ( 6, * ) ' BDS_BEG = ', &
     &                            MJD_SEC_TO_JD ( BDSFMJ(I_STA(J2)), &
     &                                            BDSFSC(I_STA(J2)) )
                 CALL ERR_LOG ( 2839, IUER, 'MAP_BINDISP', 'Trap of '// &
     &               'internal control. Station '//ISITN_CHR(J2)// &
     &               ' session '//CDBNAM(1) )
                 RETURN
            END IF
!
            IF ( USE_INT .EQ. PSV__LIN ) THEN
!
! -------------- Compute displacements in the J5-th node by using linear
! -------------- interpolation
!
                 DSP_BDS(J5,1,J2) = FLIN8 ( TIM_ARG, L_BDS, ARG_BDS, &
     &                                      XYZ_VEC(1,1), NODE )
                 DSP_BDS(J5,2,J2) = FLIN8 ( TIM_ARG, L_BDS, ARG_BDS, &
     &                                      XYZ_VEC(1,2), NODE )
                 DSP_BDS(J5,3,J2) = FLIN8 ( TIM_ARG, L_BDS, ARG_BDS, &
     &                                      XYZ_VEC(1,3), NODE )
               ELSE IF ( USE_INT .EQ. PSV__SPL ) THEN
!
! -------------- Compute displacements in the J5-th node by using spline
! -------------- interpolation
!
                 DSP_BDS(J5,1,J2) = FSPL8 ( TIM_ARG, L_BDS, ARG_BDS, &
     &                                      XYZ_VEC(1,1), NODE, SPL_BDS(1,1) )
                 DSP_BDS(J5,2,J2) = FSPL8 ( TIM_ARG, L_BDS, ARG_BDS, &
     &                                      XYZ_VEC(1,2), NODE, SPL_BDS(1,2) )
                 DSP_BDS(J5,3,J2) = FSPL8 ( TIM_ARG, L_BDS, ARG_BDS, &
     &                                      XYZ_VEC(1,3), NODE, SPL_BDS(1,3) )
            END IF
 450     CONTINUE
 420  CONTINUE
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  MAP_BINDISP   #!#
