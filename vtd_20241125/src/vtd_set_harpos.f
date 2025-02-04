      SUBROUTINE VTD_SET_HARPOS ( MJD_BEG, TAI_BEG, MJD_END, TAI_END,        &
     &                            L_STA, STA_NAM, STA_TRS_COO,               &
     &                            POSVAR_RD_AREA, N_HAR, N_SIT, NAMSIT,      &
     &                            STACOO, HARVAL, HARDSP, LEN_NAM, USE_MOD,  &
     &                            LEN_POSVAR_FIL, POSVAR_FIL, TIM_BEG_PSV,   &
     &                            TIM_PSV, VAL_PSV, FL_WARN, IUER )
! ************************************************************************
! *                                                                      *
! *     Routine  VTD_SET_HARPOS  computes site position variations       *
! *   according to the model of harmonic position variations for the     *
! *   stations which set of time epochs around the time range of the     *
! *   session for all participated in the session. It returns vectors    *
! *   of 3-D displacements.                                              *
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
! *        N_HAR ( INTEGER*4 ) -- The number of harmonics in the model   *
! *                               of site position variations.           *
! *        N_SIT ( INTEGER*4 ) -- The number of sites in the harmonic    *
! *                               model of site position  variations.    *
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
! *                                  FL_WARN is true and then the        *
! *                                  process will continue with setting  *
! *                                  harmonic position variations for    *
! *                                  this site to zero.                  *
! * LEN_POSVAR_FIL ( INTEGER*4 ) -- Length of the name of the position   *
! *                                 variation file.                      *
! *   POSVAR_FIL ( CHARACTER ) -- The name of the harmonic site position *
! *                               variations file. Used for generating   *
! *                               error messages only.                   *
! *     FL_WARN ( LOGICAL*4  ) -- Flag: if .TRUE., then a warning will   *
! *                               be printed in the screen, if needed.   *
! *                               If .FALSE. then no warning message     *
! *                               will be generated.                     *
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
! * ### 17-DEC-2002  VTD_SET_HARPOS  v2.3 (c) L. Petrov  01-NOV-2007 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'vtd.i'
      INTEGER*4  N_HAR, N_SIT, LEN_NAM, USE_MOD, L_STA, MJD_BEG, MJD_END, &
     &           LEN_POSVAR_FIL, IUER
      CHARACTER  STA_NAM(L_STA)*(*)
      CHARACTER  NAMSIT(N_SIT)*(LEN_NAM), POSVAR_FIL*(LEN_POSVAR_FIL)
      REAL*8     TAI_BEG, TAI_END, STA_TRS_COO(3,L_STA), STACOO(3,N_SIT), &
     &           POSVAR_RD_AREA, HARVAL(3,N_HAR),  HARDSP(3,2,N_HAR,N_SIT), &
     &           TIM_BEG_PSV, TIM_PSV(VTD__M_SDI),  &
     &           VAL_PSV(VTD__M_SDI,3,VTD__M_STA)
      LOGICAL*4  FL_WARN
      CHARACTER  STR*16, STR1*16
      REAL*8     TIM_SEC_BEG, TIM_SEC_END, TIM_STEP_SEC, &
     &           TIM_ARG, HAR_ARG, UEN_TO_XYZ(3,3,VTD__M_STA), &
     &           UEN_VEC_IN(3), UEN_VEC_OUT(3), XYZ_VEC_IN(3),    &
     &           XYZ_VEC_OUT(3), DIST_SQ_MIN
      INTEGER*4  I_STA(VTD__M_STA), J1, J2, J3, J4, J5, IER
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
           CALL ERR_LOG ( 2821, IUER, 'VTD_SET_HARPOS', 'Error of internal '// &
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
         I_STA(J1) = 0
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
! ----------- Store I_STA -- the index of the J2-th site in HARPOS
! ----------- site array which is the closest to the J1-th station
! ----------- participating this VLBI experiment
!
              CONTINUE
            ELSE
!
! ----------- Mmmm. No close station was found. If this station is selected,
! ----------- it is terrible.
!
              IF ( FL_WARN ) THEN
                   IF ( ( USE_MOD .EQ. PSV__REQ  .OR. &
     &                  ( USE_MOD .EQ. PSV__AVL  .AND.  FL_WARN ) ) ) THEN
                        WRITE ( 6, '(A)' ) 'WARNING (VTD_SET_HARPOS)  Station '// &
     &                         STA_NAM(J1)//' was not found in the '// &
     &                        'position variation file '// &
     &                         POSVAR_FIL(1:I_LEN(POSVAR_FIL))
                        IF ( USE_MOD .EQ. PSV__REQ ) THEN
                             CALL ERR_LOG ( 2822, IUER, 'VTD_SET_HARPOS', &
     &                           'Station '//STA_NAM(J1)//' was not found '// &
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
         DO 440 J4=1,N_HAR
!
! --------- Compute the argument for the harmonic function
!
            HAR_ARG = ( HARVAL(3,J4)*TIM_ARG/2.0D0 + HARVAL(2,J4) )*TIM_ARG + &
     &                  HARVAL(1,J4)
            DO 450 J5=1,L_STA
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
! ----------------- reference system to the crust-fixed reference system
!
                    CALL MUL_MV_IV_V ( 3, 3, UEN_TO_XYZ(1,1,J5), 3, &
     &                                 UEN_VEC_IN, 3, XYZ_VEC_IN, IER )
                    CALL MUL_MV_IV_V ( 3, 3, UEN_TO_XYZ(1,1,J5), 3, &
     &                                 UEN_VEC_OUT, 3, XYZ_VEC_OUT, IER )
!
! ----------------- Add the displacement to displacements computed before this
! ----------------- call of VTD_SET_HARPOS
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
      END  SUBROUTINE  VTD_SET_HARPOS  !#!#
