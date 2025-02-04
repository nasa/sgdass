      SUBROUTINE VTD_SET_BSPPOS ( MJD_BEG, TAI_BEG, MJD_END, TAI_END, &
     &                            L_STA, STA_NAM, STA_TRS_COO,        &
     &                            POSVAR_RD_AREA, N_SIT, NAMSIT,      &
     &                            LEN_NAM, STACOO, BSP, USE_MOD,      &
     &                            LEN_POSVAR_FIL, POSVAR_FIL,         &
     &                            TIM_BEG_PSV, TIM_PSV, VAL_PSV,      &
     &                            FL_WARN, IUER )
! ************************************************************************
! *                                                                      *
! *     Routine  VTD_SET_BSPPOS  computes site position variations       *
! *   according to the model of position variations modeled with         *
! *   B-spline for the stations for a set of time epochs around the time *
! *   range of the session for all participated in the session. It       *
! *   returns vectors of 3-D displacements.                              *
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
! *        N_SIT ( INTEGER*4 ) -- The number of sites in the harmonic    *
! *                               model of site position  variations.    *
! *       NAMSIT ( CHARACTER ) -- Array of site names defined in the     *
! *                               harmonic site position file. NB: these *
! *                               names do not necessarily coincides     *
! *                               with the IVS station names.            *
! *                               Dimension: N_SIT.                      *
! *      LEN_NAM ( INTEGER*4 ) -- The length of the site name string     *
! *                               in bytes.                              *
! *       STACOO ( REAL*8    ) -- Arrays of site coordinates in a crust  *
! *                               reference frame. Dimension: (3,N_SIT). *
! *      BSP  ( BSPPOS__TYPE ) -- Object with parameters of B-spline     *
! *                               mathematical model of site             *
! *                               displacements.                         *
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
! *                                  FL_WARN is true and then the        *
! *                                  process will continue with setting  *
! *                                  harmonic position variations for    *
! *                                  this site to zero.                  *
! * LEN_POSVAR_FIL ( INTEGER*4 ) -- Length of the name of the position   *
! *                                 variation file.                      *
! *  POSVAR_FIL ( CHARACTER  ) -- Name of the position variation file.   *
! *                               Used for error messages only.          *
! *     FL_WARN ( LOGICAL*4  ) -- Flag: if .TRUE., then a warning will   *
! *                               be printed in the screen, if needed.   *
! *                               If .FALSE. then no warning message     *
! *                               will be generated.                     *
! *                                                                      *
! * ________________________ Output parameters: ________________________ *
! *                                                                      *
! *   TIM_BEG_PSV ( REAL*8    ) -- Date in TAI in seconds elapsed from   *
! *                                the J2000.0 of the first time epoch   *
! *                                of the output array.                  *      
! *                                It is set to the nominal session      *
! *                                start minus some amount of the        *
! *                                overhead time defined in OVH__PSV     *
! *                                variable from vtd.i                   *
! *       TIM_PSV ( REAL*8    ) -- Array of time epochs for the site     *
! *                                position variations as the time       *
! *                                elapsed from TIM_BEG_PSV.             *
! *                                Dimension: VTD__M_SDI.                *
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
! * ### 30-OCT-2007  VTD_SET_BSPPOS  v1.1 (c) L.  Petrov 16-NOV-2011 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'vtd.i'
      TYPE     ( BSPPOS__TYPE ) :: BSP(N_SIT)
      INTEGER*4  N_SIT, LEN_NAM, USE_MOD, L_STA, MJD_BEG, MJD_END, &
     &           LEN_POSVAR_FIL, IUER
      CHARACTER  STA_NAM(L_STA)*(*), NAMSIT(N_SIT)*(LEN_NAM), &
     &           POSVAR_FIL*(LEN_POSVAR_FIL)
      REAL*8     TAI_BEG, TAI_END, STA_TRS_COO(3,L_STA), STACOO(3,N_SIT), &
     &           POSVAR_RD_AREA, TIM_BEG_PSV, TIM_PSV(VTD__M_SDI),  &
     &           VAL_PSV(VTD__M_SDI,3,VTD__M_STA)
      LOGICAL*4  FL_WARN
      CHARACTER  STR*32, STR1*32
      REAL*8     TIM_SEC_BEG, TIM_SEC_END, TIM_STEP_SEC, &
     &           TIM_ARG, HAR_ARG, UEN_TO_XYZ(3,3,VTD__M_STA), &
     &           UEN_VEC_IN(3), UEN_VEC_OUT(3), XYZ_VEC_IN(3),    &
     &           XYZ_VEC_OUT(3), DIST_SQ_MIN, TAI, B_SPLINE 
      INTEGER*4  I_STA(VTD__M_STA), J1, J2, J3, J4, J5, J6, J7, MJD, MAR, IER
!
      REAL*8,    EXTERNAL :: BSPL_VAL
      CHARACTER, EXTERNAL :: MJDSEC_TO_DATE*30
      INTEGER*4, EXTERNAL :: I_LEN
!
! --- Initialization
!
      CALL NOUT_R8 ( VTD__M_SDI,              TIM_PSV     )
      CALL NOUT_R8 ( VTD__M_SDI*3*VTD__M_STA, VAL_PSV     )
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
      TIM_SEC_BEG = (MJD_BEG - J2000__MJD)*86400.0D0 + (TAI_BEG - 43200.0D0)
      TIM_SEC_END = (MJD_END - J2000__MJD)*86400.0D0 + (TAI_END - 43200.0D0)
!
! --- Compute the first epoch of the time interval for which harmonic
! --- displacement is to be computed
!
      TIM_BEG_PSV = TIM_SEC_BEG - OVH__PSV
!
! --- Get the step of the the array of time epochs
!
      TIM_STEP_SEC = ( (TIM_SEC_END - TIM_SEC_BEG) + 2.0D0*OVH__PSV )/ &
     &               (VTD__M_SDI-1)
!
      DO 410 J1=1,L_STA ! Cycle over the participated stations
!
! ------ Bypass a "GEOCENTR" station
!
         IF ( DSQRT ( STA_TRS_COO(1,J1)**2 + STA_TRS_COO(2,J1)**2 + &
     &                STA_TRS_COO(3,J1)**2 ) < VTD__REA/2.0D0 ) GOTO 410
!
! ------ Compute the matrix of transformation from the local
! ------ topocentric reference system (UEN) to the crust fixed
! ------ reference system (XYZ)
!
         CALL MAKE_UEN_TO_XYZ ( STA_TRS_COO(1,J1), UEN_TO_XYZ(1,1,J1) )
!
! ------ Search for the site which is the closest to the J1-th station
!
         DIST_SQ_MIN = VTD__REA**2
         I_STA(J1) = 0
         DO 420 J2=1,N_SIT
            IF ( (STACOO(1,J2)-STA_TRS_COO(1,J1))**2 + &
     &           (STACOO(2,J2)-STA_TRS_COO(2,J1))**2 + &
     &           (STACOO(3,J2)-STA_TRS_COO(3,J1))**2   .LT. DIST_SQ_MIN ) THEN
!
                 I_STA(J1) = J2
                 DIST_SQ_MIN = (STACOO(1,J2)-STA_TRS_COO(1,J1))**2 + &
     &                         (STACOO(2,J2)-STA_TRS_COO(2,J1))**2 + &
     &                         (STACOO(3,J2)-STA_TRS_COO(3,J1))**2
            END IF
 420     CONTINUE
!
         IF ( DIST_SQ_MIN .LT. POSVAR_RD_AREA**2 ) THEN  
!
! ----------- Store I_STA -- the index of the J2-th site in the
! ----------- site array which is the closest to the J1-th station
! ----------- participating this VLBI experiment
!
              IF ( BSP(I_STA(J1))%TIM(1-BSP(I_STA(J1))%L_DEG) > &
     &             TIM_SEC_BEG ) THEN
!
! ---------------- First epoch is too late
!
                   IF ( FL_WARN ) THEN
                        IF ( ( USE_MOD .EQ. PSV__REQ  .OR. &
     &                       ( USE_MOD .EQ. PSV__AVL  .AND.  FL_WARN ) ) ) THEN
!
                             CALL TIM_TO_MJDSEC ( BSP(I_STA(J1))%TIM(BSP(I_STA(J1))%L_NOD), &
     &                                            MJD, TAI )
                             IER = 0
                             STR = MJDSEC_TO_DATE ( MJD, TAI, IER )
                             IER = 0
                             STR1 = MJDSEC_TO_DATE ( MJD_BEG, TAI_BEG, IER )
!
                             WRITE ( 6, '(A)' ) 'WARNING (VTD_SET_PASPOS) '// &
     &                          'Station '//STA_NAM(J1)//' has the last '// &
     &                          'epoch for B-spline model specified '// &
     &                          'in the position variation file '// &
     &                           POSVAR_FIL(1:I_LEN(POSVAR_FIL))// &
     &                          ' set to '//STR(1:21)//', which is too '// &
     &                          'early for processing an experiment '// &
     &                          'with the last epoch '//STR1(1:21)
                             IF ( USE_MOD .EQ. PSV__REQ ) THEN
                                  CALL ERR_LOG ( 2822, IUER, 'VTD_SET_BSPPOS', &
     &                                'Station '//STA_NAM(J1)//' has the '// &
     &                                'last epoch for B-spline model '// &
     &                                'specified in the position variation '// &
     &                                'file '//POSVAR_FIL(1:I_LEN(POSVAR_FIL))// &
     &                                ' set to '//STR(1:21)//', which is '// &
     &                                'too late for processing an '// &
     &                                'experiment with the first epoch '// &
     &                                STR1(1:21) )
                                  RETURN
                             END IF
                        END IF
                   END IF
                   I_STA(J1) = 0
                   GOTO 410
              END IF ! time_first
!
              IF ( BSP(I_STA(J1))%TIM(BSP(I_STA(J1))%L_NOD) < &
     &             TIM_SEC_END ) THEN
!
! ---------------- Last epoch is too early
!
                   IF ( FL_WARN ) THEN
                        IF ( ( USE_MOD .EQ. PSV__REQ  .OR. &
     &                       ( USE_MOD .EQ. PSV__AVL  .AND.  FL_WARN ) ) ) THEN
!
                             CALL TIM_TO_MJDSEC ( BSP(I_STA(J1))%TIM(BSP(I_STA(J1))%L_NOD), &
     &                                            MJD, TAI )
                             IER = 0
                             STR = MJDSEC_TO_DATE ( MJD, TAI, IER )
                             IER = 0
                             STR1 = MJDSEC_TO_DATE ( MJD_END, TAI_END, IER )
!
                             WRITE ( 6, '(A)' ) 'WARNING (VTD_SET_PASPOS) '// &
     &                          'Station '//STA_NAM(J1)//' has the last '// &
     &                          'epoch for B-spline model specified '// &
     &                          'in the position variation file '// &
     &                           POSVAR_FIL(1:I_LEN(POSVAR_FIL))// &
     &                          ' set to '//STR(1:21)//', which is too '// &
     &                          'early for processing an experiment '// &
     &                          'with the last epoch '//STR1(1:21)
                             IF ( USE_MOD .EQ. PSV__REQ ) THEN
                                  CALL ERR_LOG ( 2822, IUER, 'VTD_SET_BSPPOS', &
     &                                'Station '//STA_NAM(J1)//' has the '// &
     &                                'last epoch for B-spline model '// &
     &                                'specified in the position variation '// &
     &                                'file '//POSVAR_FIL(1:I_LEN(POSVAR_FIL))// &
     &                                ' set to '//STR(1:21)//', which is '// &
     &                                'too early  for processing an '// &
     &                                'experiment with the last epoch '// &
     &                                STR1(1:21) )
                                  RETURN
                             END IF
                        END IF
                   END IF
                   I_STA(J1) = 0
              END IF ! time_last
            ELSE
              I_STA(J1) = 0
         END IF
 410  CONTINUE
!
! --- Now cycle over the epochs
!
      DO 430 J3=1,VTD__M_SDI
!
! ------ Get time argument TIM_PSV -- time in seconds elapsed from the
! ------ beginning the interval
!
         TIM_PSV(J3) = (J3-1)*TIM_STEP_SEC
!
! ------ Get the time argument for computing harmonic site position variations:
! ------ time in seconds elapsed from the J2000.0 epoch
!
         TIM_ARG = TIM_BEG_PSV + TIM_PSV(J3)
!
         DO 440 J4=1,L_STA
            IF ( I_STA(J4) > 0 ) THEN
!
! -------------- Cycle over knots
!
                 DO 450 J5=1-BSP(I_STA(J4))%L_DEG,BSP(I_STA(J4))%L_NOD
                    B_SPLINE = BSPL_VAL ( BSP(I_STA(J4))%L_NOD, &
     &                                    BSP(I_STA(J4))%TIM(1), &
     &                                    BSP(I_STA(J4))%L_DEG, J5, &
     &                                    TIM_ARG )
                    DO 460 J6=1,3
                       VAL_PSV(J3,J6,J4) = VAL_PSV(J3,J6,J4) + &
     &                                     B_SPLINE*BSP(I_STA(J4))%SPL(J5,J6)
 460                CONTINUE 
 450             CONTINUE 
               ELSE 
!
! -------------- Station was not found. Well, set zeroes
!
                 DO 470 J7=1,3
                    VAL_PSV(J3,J7,J4) = 0.0D0
 470             CONTINUE 
            END IF
 440     CONTINUE
 430  CONTINUE
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  VTD_SET_BSPPOS  !#!#
