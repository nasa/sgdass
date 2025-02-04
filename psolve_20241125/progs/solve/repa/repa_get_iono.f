      SUBROUTINE REPA_GET_IONO ( CH_VAL, IND_OBS, REP, IONDEL_VAL, IONDEL_ERR )
! ************************************************************************
! *                                                                      *
! *   Auxilliary routine REPA_GET_ION returns the contribution of        *
! *   ionosphere ot the group or phase delay at X-band and its           *
! *   unreweighted formal uncertainty.                                   *
! *                                                                      *
! * ________________________ Input parameters: _________________________ *
! *                                                                      *
! *     CH_VAL ( CHARACTER ) -- The flavor of the ambiguity. One of twos:*
! *                         'GrIon_Dl' -- group delay ambiguity;         *
! *                         'PhIon_Dl' -- phase delay ambiguity.         *
! *    IND_OBS ( INTEGER*4 ) -- Index of the observation of interest     *
! *                             in the experiment.                       *
! *        REP ( RECORD    ) -- Object which keeps internal parameters   *
! *                             for program REPA (REsiduals Plots and    *
! *                             Ambiguities).                            *
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
! * IONDEL_VAL ( REAL*8    ) -- Contribution of ionosphere to group      *
! *                             or phase delay according to CH_VAL.      *
! * IONDEL_ERR ( REAL*8    ) -- Unreweighted formal uncertainty of the   *
! *                             contribution of ionosphere to group      *
! *                             or phase delay according to CH_VAL.      *
! *                                                                      *
! *  ### 14-DEC-2004  REPA_GET_IONO  v1.2 (c)  L. Petrov 11-NOV-2018 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i' 
      INCLUDE   'solve.i'
      INCLUDE   'diagi.i' 
      INCLUDE   'repa.i' 
      TYPE     ( REP__TYPE ) :: REP
      CHARACTER  CH_VAL*(*)
      CHARACTER  STR*128
      LOGICAL*1  FL_PH_DIF, FL_GR_DIF, FL_GR_ALP
      INTEGER*4  IND_OBS
      REAL*8     IONDEL_VAL, IONDEL_ERR, FREQ_MIN 
      PARAMETER  ( FREQ_MIN = 1.D7 )
!
      FL_PH_DIF = .FALSE.
      CALL GETENVAR ( 'REPA_PH_DIF', STR )
      IF ( STR == 'YES' .OR. STR == 'yes' ) FL_PH_DIF = .TRUE.
!
      FL_GR_DIF = .FALSE.
      CALL GETENVAR ( 'REPA_GR_DIF', STR )
      IF ( STR == 'YES' .OR. STR == 'yes' ) FL_GR_DIF = .TRUE.
!
      FL_GR_ALP = .FALSE.
      CALL GETENVAR ( 'REPA_GR_ALP', STR )
      IF ( STR == 'YES' .OR. STR == 'yes' ) FL_GR_ALP = .TRUE.
!
      IF ( CH_VAL == 'GrIon_Dl' ) THEN
!
! -------- Compute the contribution of ionosphere to the group delay
!
!!  write ( 6, * ) 'ind_obs = ', ind_obs, ' tau= ', rep%obs(ind_obs)%tau_gr_x, rep%obs(ind_obs)%tau_gr_s, ' dt= ', sngl(rep%obs(ind_obs)%tau_gr_x - rep%obs(ind_obs)%tau_gr_s), ' fef= ', sngl(rep%obs(ind_obs)%frqeff_gr_x), sngl(rep%obs(ind_obs)%frqeff_gr_s) ! %%%%%%%%%
           IF ( REP%OBS(IND_OBS)%FRQEFF_GR_X .GT. FREQ_MIN .AND. &
     &          REP%OBS(IND_OBS)%FRQEFF_GR_S .GT. FREQ_MIN ) THEN
                IONDEL_VAL = &
     &                (REP%OBS(IND_OBS)%TAU_GR_X - REP%OBS(IND_OBS)%TAU_GR_S)* &
     &                 REP%OBS(IND_OBS)%FRQEFF_GR_S**2/ &
     &               ( REP%OBS(IND_OBS)%FRQEFF_GR_X**2 - REP%OBS(IND_OBS)%FRQEFF_GR_S**2 )
                IONDEL_ERR = DSQRT ( REP%OBS(IND_OBS)%ERR_GR_X**2 + &
     &                               REP%OBS(IND_OBS)%ERR_GR_S**2 )* &
     &                 REP%OBS(IND_OBS)%FRQEFF_GR_S**2/ &
     &               ( REP%OBS(IND_OBS)%FRQEFF_GR_X**2 - REP%OBS(IND_OBS)%FRQEFF_GR_S**2 )
                IF ( FL_GR_DIF )  THEN
                     IONDEL_VAL = REP%OBS(IND_OBS)%TAU_GR_X - REP%OBS(IND_OBS)%TAU_GR_S
                     IONDEL_ERR = DSQRT( REP%OBS(IND_OBS)%ERR_GR_X**2 + REP%OBS(IND_OBS)%ERR_GR_S**2)
                END IF
                IF ( FL_GR_ALP )  THEN
                     IONDEL_VAL = 1.D-9*REP%OBS(IND_OBS)%FRQEFF_GR_S**2/ &
     &               ( REP%OBS(IND_OBS)%FRQEFF_GR_X**2 - REP%OBS(IND_OBS)%FRQEFF_GR_S**2 )
                     IONDEL_ERR = 1.D-12*0.001
                END IF
              ELSE
                IONDEL_VAL = 0.0D0
                IONDEL_ERR = 1.D-13
           END IF
         ELSE IF ( CH_VAL == 'PhIon_Dl' ) THEN
!
! -------- Compute the contribution of ionosphere to the phase delay
!
           IONDEL_VAL = &
     &           (REP%OBS(IND_OBS)%TAU_PH_X - REP%OBS(IND_OBS)%TAU_PH_S)* &
     &            REP%OBS(IND_OBS)%FRQEFF_PH_S**2/ &
     &      ( REP%OBS(IND_OBS)%FRQEFF_PH_X**2 - REP%OBS(IND_OBS)%FRQEFF_PH_S**2 )
           IONDEL_ERR = DSQRT ( REP%OBS(IND_OBS)%ERR_PH_X**2 + &
     &                          REP%OBS(IND_OBS)%ERR_PH_S**2 )* &
     &            REP%OBS(IND_OBS)%FRQEFF_PH_S**2/ &
     &      ( REP%OBS(IND_OBS)%FRQEFF_PH_X**2 - REP%OBS(IND_OBS)%FRQEFF_PH_S**2 )
           IF ( REP%OBS(IND_OBS)%FRQEFF_PH_S .LT. FREQ_MIN ) THEN
                IONDEL_VAL = 0.0D0
                IONDEL_ERR = 1.D-13
           END IF
           IF ( FL_PH_DIF ) IONDEL_VAL = (REP%OBS(IND_OBS)%TAU_PH_X - REP%OBS(IND_OBS)%TAU_PH_S)
      END IF
!
      RETURN
      END  SUBROUTINE  REPA_GET_IONO
