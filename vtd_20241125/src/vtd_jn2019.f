      SUBROUTINE VTD_JN2019 ( VTD, ISTA1, ISTA2, ISOU, OBS_TYP, TAU_GEOM, RATE_GEOM, &
     &                        TAU_DER_EOP,  TAU_DER_STA1, TAU_DER_STA2, &
     &                        TAU_DER_POS,  TAU_DER_VEL, &
     &                        RATE_DER_EOP, RATE_DER_STA1, RATE_DER_STA2, &
     &                        RATE_DER_POS, RATE_DER_VEL, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine VTD_JN2019 computes VLBI path delay from an Earth orbiting *
! *   satellite using Jaron & Nothnagel (2019) analytical expression.    *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *     ISTA1 ( INTEGER*4 ) -- Index in the list of station of the first *
! *                            station of the baseline.                  *
! *     ISTA2 ( INTEGER*4 ) -- Index in the list of station of the       *
! *                            second station of the baseline.           *
! *      ISOU ( INTEGER*4 ) -- Index in the list of sources of the       *
! *                            source under consideration.               *
! *                                                                      *
! * _________________________ Output parameters: _______________________ *
! *                                                                      *
! *  TAU_GEOM ( REAL*8    ) -- Geometric delay. Units: seconds.          *
! * RATE_GEOM ( REAL*8    ) -- First time derivative of geometric time   *
! *                            delay. Units: dimensionless.              *
! * TAU_DER_STA1 ( REAL*8    ) -- Partial derivative of time delay with  *
! *                               respect to coordinates vector of       *
! *                               station #1. Units: sec/m.              *
! * TAU_DER_STA2 ( REAL*8    ) -- Partial derivative of time delay with  *
! *                               respect to coordinates vector of       *
! *                               station #2. Units: sec/m.              *
! * TAU_DER_POS ( REAL*8    ) -- Partial derivative of time delay with   *
! *                               respect to position vector of          *
! *                               satellite. Units: sec/m.               *
! * TAU_DER_VEL ( REAL*8    ) -- Partial derivative of time delay with   *
! *                               respect to velocity vector of          *
! *                               satellite. Units: sec/m.               *
! * RATE_DER_STA1 ( REAL*8   ) -- Partial derivative of delay rate with  *
! *                               respect to coordinates vector of       *
! *                               station #1. Units: 1/m.                *
! * RATE_DER_STA2 ( REAL*8   ) -- Partial derivative of delay rate with  *
! *                               respect to coordinates vector of       *
! *                               station #2. Units: 1/m.                *
! * RATE_DER_POS ( REAL*8    ) -- Partial derivative of delay rate with  *
! *                               respect to position vector of          *
! *                               satellite. Units: sec/m.               *
! * RATE_DER_VEL ( REAL*8    ) -- Partial derivative of delay rate with  *
! *                               respect to velocity vector of          *
! *                               satellite. Units: sec/m.               *
! *                                                                      *
! * _________________________ Modified parameters: _____________________ *
! *                                                                      *
! *       VTD ( RECORD    ) -- Object which keeps configuration and data *
! *                            related to VLBI Theoretical Delay (VTD)   *
! *                            package.                                  *
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
! *  ###  18-APR-2024  VTD_JN2019  v2.0 (c)  L. Petrov  19-OCT-2023 ###  *
! *                                          J. Skeens                   *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'astro_constants.i'
      INCLUDE   'vtd.i'
      INCLUDE   'ners_local.i'
      TYPE     ( VTD__TYPE ) :: VTD
      TYPE     ( VTD__OBS_TYPE  ) :: OBS_TYP
      INTEGER*4  ISTA1, ISTA2, ISOU, IUER, CYCLE_ADJ
      CHARACTER  STR*80
      REAL*8     TAU_GEOM, RATE_GEOM, TAU_DER_EOP, TAU_DER_STA1(3), &
     &           TAU_DER_STA2(3), TAU_DER_POS(3),  TAU_DER_VEL(3), &
     &           RATE_DER_EOP, RATE_DER_STA1(3), RATE_DER_STA2(3), &
     &           RATE_DER_POS(3), RATE_DER_VEL(3)
      REAL*8     COO1_BRS(3), COO2_BRS(3), COO2_BRS0(3), SAT_POS_TRS(3), &
     &           SAT_VEL_TRS(3), B_CRS(3), V_CRS(3), TIM_ARG, TIM_TR1, &
     &           TIM_TR2, TARG, R0J, R1A(3), R2A(3), V1A(3), V2A(3), &
     &           R1A_LEN, R2A_LEN, COO_PLAN(3), VEL_PLAN(3), &
     &           TAU_GRAV1, TAU_GRAV2, TAU_RETR1, TAU_RETR2, &
     &           SOU_STA1(3), SOU_STA2(3), S_HAT(3), TAU_COMP, PAR0, PAR1, PAR2, PAR3, &
     &           TAU_GEOM_BAR, DIST_SUN_EARTH, VE_SQ, VEC_TMP(3)
      REAL*8     G_T0, G_T2, DELTA_1, T0_EXP, T2_EXP, & 
     &           DELTA_2, DD_12, TG_01, DELTA_T0, TG_02, DELTA_T2, &
     &           DELTA_1_DOT, DELTA_2_DOT, DT0_DT, DT2_DT, W_Z, & 
     &           D_DELTADOT1_DX1_IDX, D_DELTADOT2_DX2_IDX
      REAL*8     X1(3), X2(3), X0(3), V0(3), V1(3), V2(3), A0(3), A1(3), &
     &           A2(3), X0_DT1(3), V0_DT1(3), A0_DT1(3), X_01(3), X_02(3), &
     &           X2_DD(3), V2_DD(3), A2_DD(3), X0_BAR(3), X2_BAR(3), &
     &           DT0_DX1(3), DT2_DX1(3), DT2_DX2(3), DT0_DX0(3), DT2_DX0(3), &
     &           DT0_DV0(3), DT2_DV0(3), DT0DOT_DX0(3), DT2DOT_DX0(3), &
     &           DT0DOT_DV0(3), DT2DOT_DV0(3), DX01_DT(3), DX02_DT(3), DT0DOT_DX1(3), &
     &           D_DELTA1_DX1(3), D_DELTA2_DX2(3), POS_COMP_VEL(3), &
     &           DX01_DX1_IDX(3), DX01DOT_DX1_IDX(3), DX02_DX1_IDX(3), &
     &           DX02_DX2_IDX(3), DX02DOT_DX1_IDX(3), DX02DOT_DX2_IDX(3), &
     &           DT2DOT_DX1(3), DT2DOT_DX2(3), X2_TRS(3), DV1_DX1_IDX(3), &
     &           DV2_DX2_IDX(3), DA2_DX2_IDX(3), D_DELTA1_DX0(3), D_DELTA2_DX0(3), &
     &           D_TAU_DX0(3), D_DELTA1DOT_DX0(3), D_DELTA2DOT_DX0(3), D_TAUDOT_DX0(3), &
     &           D_DELTA1DOT_DV0(3), D_DELTA2DOT_DV0(3), D_TAUDOT_DV0(3), DG0_DV0(3), &
     &           DX01_DX0_IDX(3), DX02_DX0_IDX(3), DX01_DV0_IDX(3), DX02_DV0_IDX(3), &
     &           DX01DOT_DX0_IDX(3), DX02DOT_DX0_IDX(3), DX01DOT_DV0_IDX(3), &
     &           DX02DOT_DV0_IDX(3), MATR(3,3,3), X0_ACC(3), X0_DT1_ACC(3)
      REAL*8     IDEN_MAT3(3,3), X0_OP(3,3), DV1_DX1(3,3), DV2_DX2(3,3), DA2_DX2(3,3), &
     &           DA0_DX0(3,3), CRS_TO_TRS(3,3), RX2SAT_STA1(3), RX2SAT_STA2(3), &
     &           PCO_STA1(3), PCO_STA2(3), SAT_PCO_TOT(3), SAT_PCO_XYZ(3), TAU_PCV, &
     &           SATBODY_TO_ECEF(3,3), SAT_PCV_STA1, SAT_PCV_STA2, X1_TRS(3), &
     &           PHASE_WINDUP_STA1, PHASE_WINDUP_STA2, TAU_PHASE_WINDUP, PHASE_WINDUP_DIFF
      REAL*8     TARG_T0, T0, T2, X01(3), X02(3), DST_X01, DST_X02, VEC_INC(3), DX, DT
      REAL*8     AZ, EL, HA, DST, AZ_RATE, EL_RATE, HA_RATE, DST_RATE, RN, SAT_TRS_SAVE(3) 
      INTEGER*4  J1, J2, J3, J4, KNOT, L_PAR, MP, IDX, IER, GPS2TAI
      CHARACTER, EXTERNAL :: MJDSEC_TO_DATE*30
      INTEGER*4, EXTERNAL :: ILEN, I_LEN, IXMN8
      REAL*8,    EXTERNAL :: BSPL_VAL, BSPL_DER, DP_VV_V
!
      PARAMETER ( MP = NERS__MPAR )
      TAU_GEOM      = 0.0D0
      RATE_GEOM     = 0.0D0
!
      TAU_DER_EOP   = 0.0D0
      TAU_DER_STA1  = 0.0D0
      TAU_DER_STA2  = 0.0D0
      TAU_DER_POS   = 0.0D0
      TAU_DER_VEL   = 0.0D0
      RATE_DER_EOP  = 0.0D0
      RATE_DER_STA1 = 0.0D0
      RATE_DER_STA2 = 0.0D0
      RATE_DER_POS  = 0.0D0
      RATE_DER_VEL  = 0.0D0
!
      TARG_T0 = (VTD%MOM%MJD - VTD%NZO(VTD%SOU(ISOU)%IND_NZO)%MJD_BEG)*86400.0D0 + &
     &          (VTD%MOM%TAI - VTD%NZO(VTD%SOU(ISOU)%IND_NZO)%TIM_BEG)
      KNOT = IXMN8 ( VTD%NZO(VTD%SOU(ISOU)%IND_NZO)%NOD_SPL, &
     &               VTD%NZO(VTD%SOU(ISOU)%IND_NZO)%TIM_ARR(1), TARG_T0 )
      IF ( KNOT .EQ. -1 ) THEN
           STR(1:28)  = MJDSEC_TO_DATE ( VTD%MOM%MJD, VTD%MOM%TAI, IER )
           STR(31:58) = MJDSEC_TO_DATE ( VTD%NZO(VTD%SOU(ISOU)%IND_NZO)%MJD_BEG, &
     &                                   VTD%NZO(VTD%SOU(ISOU)%IND_NZO)%TIM_BEG, IER )
           CALL ERR_LOG ( 2371, IUER, 'VTD_JN2019', 'Moment of time '// &
     &          STR(1:28)//' at TAI precedes the first epoch for the '// &
     &         'near zone object '//VTD%SOU(ISOU)%NZO_NAME//' -- '// &
     &          STR(31:58) )
           RETURN
      END IF
      IF ( KNOT .EQ. -2 ) THEN
           STR(1:28)  = MJDSEC_TO_DATE ( VTD%MOM%MJD, VTD%MOM%TAI, IER )
           STR(31:53) = MJDSEC_TO_DATE ( VTD%NZO(VTD%SOU(ISOU)%IND_NZO)%MJD_BEG, &
     &                                   VTD%NZO(VTD%SOU(ISOU)%IND_NZO)%TIM_BEG + &
     &                  VTD%NZO(VTD%SOU(ISOU)%IND_NZO)%TIM_ARR(VTD%NZO(VTD%SOU(ISOU)%IND_NZO)%NOD_SPL), IER )
           CALL ERR_LOG ( 2372, IUER, 'VTD_JN2019', 'Moment of time '// &
     &          STR(1:28)//' at TAI is after the last epoch for the '// &
     &         'near zone object '//VTD%SOU(ISOU)%NZO_NAME//' -- '// &
     &          STR(31:58) )
           RETURN
      END IF
!
      DO 410 J1=1,3
         SAT_POS_TRS(J1) = 0.0D0
         SAT_VEL_TRS(J1) = 0.0D0
         DO 420 J2=KNOT-VTD%NZO(VTD%SOU(ISOU)%IND_NZO)%DEG_SPL,KNOT
            SAT_POS_TRS(J1) = SAT_POS_TRS(J1) + &
     &          VTD%NZO(VTD%SOU(ISOU)%IND_NZO)%SPL_ARR(J2,J1)* &
     &          BSPL_VAL ( VTD%NZO(VTD%SOU(ISOU)%IND_NZO)%NOD_SPL, &
     &                     VTD%NZO(VTD%SOU(ISOU)%IND_NZO)%TIM_ARR(1), &
     &                     VTD%NZO(VTD%SOU(ISOU)%IND_NZO)%DEG_SPL, J2, TARG_T0 )
            SAT_VEL_TRS(J1) = SAT_VEL_TRS(J1) + &
     &          VTD%NZO(VTD%SOU(ISOU)%IND_NZO)%SPL_ARR(J2,J1)* &
     &          BSPL_DER ( VTD%NZO(VTD%SOU(ISOU)%IND_NZO)%NOD_SPL, &
     &                     VTD%NZO(VTD%SOU(ISOU)%IND_NZO)%TIM_ARR(1), &
     &                     VTD%NZO(VTD%SOU(ISOU)%IND_NZO)%DEG_SPL, J2, TARG_T0 )
 420     CONTINUE
 410  CONTINUE

!
!-----Get PCO/PCV data for antennas/satellite
!
      RX2SAT_STA1 = SAT_POS_TRS - VTD%STA(ISTA1)%MOM_COO_TRS
      CALL NORM_VEC ( 3, RX2SAT_STA1, RN )
      IF ( VTD%STA(ISTA1)%MOUNT_TYPE  == 'GNSS' ) THEN
          CALL ERR_PASS  ( IUER, IER )
          CALL VTD_RX_PCO ( VTD, PCO_STA1, RX2SAT_STA1, OBS_TYP, ISTA1, IER )
          X1_TRS = VTD%STA(ISTA1)%MOM_COO_TRS + PCO_STA1

          CALL MUL_MV_IV_V ( 3, 3, VTD%MOM%TRS_TO_CRS, &
     &                       3, X1_TRS, &
     &                       3, X1, IER )
          CALL MUL_MV_IV_V ( 3, 3, VTD%MOM%TRS_TO_CRS_DER1, &
     &                       3, X1_TRS, &
     &                       3, V1, IER )
          CALL MUL_MV_IV_V ( 3, 3, VTD%MOM%TRS_TO_CRS_DER2, &
     &                       3, X1_TRS, &
     &                       3, A1, IER )
          IF ( IER .NE. 0 ) THEN
               CALL ERR_LOG ( 2373, IUER, 'VTD_JN2019', 'Error in an attempt '// &
     &             'to compensate for station 1 phase center offset/variation')
               RETURN 
          END IF

      ELSE
          PCO_STA1 = 0.0D0
          X1 = VTD%STA(ISTA1)%COO_CRS
          V1 = VTD%STA(ISTA1)%VEL_CRS
          A1 = VTD%STA(ISTA1)%ACC_CRS
      END IF

      RX2SAT_STA2 = SAT_POS_TRS - VTD%STA(ISTA2)%MOM_COO_TRS
      CALL NORM_VEC ( 3, RX2SAT_STA2, RN )
      IF ( VTD%STA(ISTA2)%MOUNT_TYPE  == 'GNSS' ) THEN
          CALL ERR_PASS  ( IUER, IER )
          CALL VTD_RX_PCO ( VTD, PCO_STA2, RX2SAT_STA2, OBS_TYP, ISTA2, IER )

          X2_TRS = VTD%STA(ISTA2)%MOM_COO_TRS + PCO_STA2

          CALL MUL_MV_IV_V ( 3, 3, VTD%MOM%TRS_TO_CRS, &
     &                       3, X2_TRS, &
     &                       3, X2, IER )
          CALL MUL_MV_IV_V ( 3, 3, VTD%MOM%TRS_TO_CRS_DER1, &
     &                       3, X2_TRS, &
     &                       3, V2, IER )
          CALL MUL_MV_IV_V ( 3, 3, VTD%MOM%TRS_TO_CRS_DER2, &
     &                       3, X2_TRS, &
     &                       3, A2, IER )
          IF ( IER .NE. 0 ) THEN
               CALL ERR_LOG ( 2374, IUER, 'VTD_JN2019', 'Error in an attempt '// &
     &             'to compensate for station 2 phase center offset/variation')
               RETURN 
          END IF
      ELSE
          PCO_STA2 = 0.0D0
          X2 = VTD%STA(ISTA2)%COO_CRS 
          V2 = VTD%STA(ISTA2)%VEL_CRS
          A2 = VTD%STA(ISTA2)%ACC_CRS
      END IF 
!
!---- Compute satellite PCO (by freq) and PCV (by antenna)
!
!
!---- First need to compute transformation from sat body frame to ECEF
!
      CALL ERR_PASS  ( IUER, IER )
      CALL VTD_CALC_SATBODY_TRANSF ( VTD, SAT_POS_TRS, SATBODY_TO_ECEF, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 2375, IUER, 'VTD_JN2019', 'Error in an attempt '// &
     &         'to compute transformation from satellite body frame to ECEF')
           RETURN 
      END IF
!
!---- Get satellite PCO (treated as position offset)
!
      CALL ERR_PASS  ( IUER, IER )
      CALL VTD_SAT_PCO ( VTD, SAT_PCO_TOT, OBS_TYP, ISOU, IER )
      CALL MUL_MV_IV_V ( 3, 3, SATBODY_TO_ECEF, &
     &                   3, SAT_PCO_TOT, 3, SAT_PCO_XYZ, IER )
!
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 2376, IUER, 'VTD_JN2019', 'Error in an attempt '// &
     &         'to find satellite phase center offset')
           RETURN 
      END IF 
!
!---- Get satellite PCV (treated as delay)
!
      IF ( VTD%STA(ISTA1)%STA_TYP /= VTD__GC .AND. &
     &     VTD%STA(ISTA2)%STA_TYP /= VTD__GC ) THEN
          CALL VTD_SAT_STA_PCV ( VTD, SAT_PCV_STA1, OBS_TYP, ISTA1, ISOU, &
     &                           SATBODY_TO_ECEF, RX2SAT_STA1, IER )
          CALL VTD_SAT_STA_PCV ( VTD, SAT_PCV_STA2, OBS_TYP, ISTA2, ISOU, &
     &                           SATBODY_TO_ECEF, RX2SAT_STA2, IER )
          IF ( IER .NE. 0 ) THEN
               CALL ERR_LOG ( 2377, IUER, 'VTD_JN2019', 'Error in an attempt '// &
     &             'to find satellite phase center variations')
               RETURN 
          END IF 
          TAU_PCV = -(SAT_PCV_STA2 - SAT_PCV_STA1) / VTD__C ! negative b/c +PCV means lower delay
      ELSE
          TAU_PCV = 0.0D0
      END IF 
!
!---- Get phase windup (treated as delay)
!
      IF ( VTD%STA(ISTA1)%STA_TYP /= VTD__GC .AND. &
     &     VTD%STA(ISTA2)%STA_TYP /= VTD__GC .AND. &
           OBS_TYP%DELAY_TYPE == VTD__PH__DTP ) THEN
          CALL ERR_PASS  ( IUER, IER )
          CALL VTD_CALC_PHASE_WINDUP ( VTD, RX2SAT_STA1, SATBODY_TO_ECEF, &
     &           OBS_TYP, ISTA1, ISOU, PHASE_WINDUP_STA1, IER )
          CALL VTD_CALC_PHASE_WINDUP ( VTD, RX2SAT_STA2, SATBODY_TO_ECEF, &
     &           OBS_TYP, ISTA2, ISOU, PHASE_WINDUP_STA2, IER )
          IF ( IER .NE. 0 ) THEN
               CALL ERR_LOG ( 2378, IUER, 'VTD_JN2019', 'Error in an attempt '// &
     &             'to find satellite phase center variations')
               RETURN 
          END IF 
          TAU_PHASE_WINDUP = PHASE_WINDUP_STA2 - PHASE_WINDUP_STA1 
          PHASE_WINDUP_DIFF = VTD%NZO(VTD%SOU(ISOU)%IND_NZO)%PHASE_WINDUP(ISTA1,ISTA2) - &
     &                        (PHASE_WINDUP_STA2 - PHASE_WINDUP_STA1)
!
! ------- Adjust for 2 pi ambiguity for phase continuity (not really
! ------- necessary b/c ambiguity is adjusted point by point, but this makes
! ------- initial ambiguity resolution easier by keeping them closely grouped)
! 
          CYCLE_ADJ = NINT ( PHASE_WINDUP_DIFF )
          TAU_PHASE_WINDUP = TAU_PHASE_WINDUP + CYCLE_ADJ
!
! ------- Save the phase windup and convert to a delay
! 
          VTD%NZO(VTD%SOU(ISOU)%IND_NZO)%PHASE_WINDUP(ISTA1,ISTA2) = TAU_PHASE_WINDUP
          TAU_PHASE_WINDUP = TAU_PHASE_WINDUP / OBS_TYP%FRQ_REF(1)
      ELSE
          TAU_PHASE_WINDUP = 0.0D0             
      ENDIF
!
!---- Convert satellite position to CRF
!
      CALL MUL_MV_IV_V ( 3, 3, VTD%MOM%TRS_TO_CRS, &
     &                   3, SAT_POS_TRS, 3, X0_ACC, IER )
      SAT_POS_TRS = SAT_POS_TRS + SAT_PCO_XYZ
      SAT_TRS_SAVE = SAT_POS_TRS + 0.0D0
      CALL MUL_MV_IV_V ( 3, 3, VTD%MOM%TRS_TO_CRS, &
     &                   3, SAT_POS_TRS, 3, X0, IER )

      CALL MUL_MV_IV_V ( 3, 3, VTD%MOM%TRS_TO_CRS, &
     &                   3, SAT_VEL_TRS, 3, V0, IER )

      CALL MUL_MV_IV_V ( 3, 3, VTD%MOM%TRS_TO_CRS_DER1, &
     &                   3, SAT_POS_TRS, 3, POS_COMP_VEL, IER )
      V0 = V0 + POS_COMP_VEL 
! 
      G_T0 = 1.0D0/SQRT ( 1.0D0 - NORM2 ( V0 )**2/VTD__C**2 )
      G_T2 = 1.0D0/SQRT ( 1.0D0 - NORM2 ( V2 )**2/VTD__C**2 )
      DELTA_1 = NORM2 ( X1 - X0 )/VTD__C
      DELTA_2 = NORM2 ( X2 - X0 )/VTD__C
      DD_12 = DELTA_2 - DELTA_1
!
!-----Regenerate satellite position/velocity accounting for light time
! 
      DO 430 J3=1,3
         SAT_POS_TRS(J3) = 0.0D0
         SAT_VEL_TRS(J3) = 0.0D0
         DO 440 J4=KNOT-VTD%NZO(VTD%SOU(ISOU)%IND_NZO)%DEG_SPL,KNOT
            SAT_POS_TRS(J3) = SAT_POS_TRS(J3) + &
     &          VTD%NZO(VTD%SOU(ISOU)%IND_NZO)%SPL_ARR(J4,J3)* &
     &          BSPL_VAL ( VTD%NZO(VTD%SOU(ISOU)%IND_NZO)%NOD_SPL, &
     &                     VTD%NZO(VTD%SOU(ISOU)%IND_NZO)%TIM_ARR(1), &
     &                     VTD%NZO(VTD%SOU(ISOU)%IND_NZO)%DEG_SPL, J4, TARG_T0 - DELTA_1 )
            SAT_VEL_TRS(J3) = SAT_VEL_TRS(J3) + &
     &          VTD%NZO(VTD%SOU(ISOU)%IND_NZO)%SPL_ARR(J4,J3)* &
     &          BSPL_DER ( VTD%NZO(VTD%SOU(ISOU)%IND_NZO)%NOD_SPL, &
     &                     VTD%NZO(VTD%SOU(ISOU)%IND_NZO)%TIM_ARR(1), &
     &                     VTD%NZO(VTD%SOU(ISOU)%IND_NZO)%DEG_SPL, J4, TARG_T0 - DELTA_1 )
 440     CONTINUE
 430  CONTINUE
!
!-----Convert satellite position to CRS
! 
      CALL ERR_PASS  ( IUER, IER )
      CALL NERS_INIT ( NERS__CONFIG, VTD%NERS, &
     &              (VTD%MJD_BEG - J2000__MJD)*86400.0D0 + VTD%TAI_BEG - VTD__EOP_TIM_MAR, &
     &              (VTD%MJD_END - J2000__MJD)*86400.0D0 + VTD%TAI_END + VTD__EOP_TIM_MAR, &
     &               IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 2379, IUER, 'VTD_JN2019', 'Error in an attempt '// &
     &         'to initialize NERS object' )
           RETURN 
      END IF

      MATR = 0.0D0
      CALL ERR_PASS ( IUER, IER )
      CALL NERS_GET_EOP ( VTD%NERS, (VTD%MOM%MJD - J2000__MJD)*86400.0D0 &
     &                    + VTD%MOM%TAI - DELTA_1, 'matall', MP, L_PAR, MATR, IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           STR = MJDSEC_TO_DATE ( VTD%MOM%MJD, VTD%MOM%TAI - DELTA_1, IER )
           CALL ERR_LOG ( 2380, IUER, 'VTD_JN2019', 'Error in an attempt '// &
     &         'to compute the transformation matrix from the terrestrial '// &
     &         'to the terrestrial coordinate system on '//STR(1:24) )
           RETURN 
      END IF
!
      CALL MUL_MV_IV_V ( 3, 3, MATR(1,1,1), &
     &                      3, SAT_POS_TRS, &
     &                      3, X0_DT1_ACC, IER )

      SAT_POS_TRS = SAT_POS_TRS + SAT_PCO_XYZ
      CALL MUL_MV_IV_V ( 3, 3, MATR(1,1,1), &
     &                      3, SAT_POS_TRS, &
     &                      3, X0_DT1, IER )
      CALL MUL_MV_IV_V ( 3, 3, MATR(1,1,1), &
     &                      3, SAT_VEL_TRS, &
     &                      3, V0_DT1, IER )
      CALL MUL_MV_IV_V ( 3, 3, MATR(1,1,2), &
     &                      3, SAT_POS_TRS, &
     &                      3, POS_COMP_VEL, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 2381, IUER, 'VTD_JN2019', 'Error in an attempt '// &
     &         'to convert ECEF coords to ECI')
           RETURN 
      END IF
      V0_DT1 = V0_DT1 + POS_COMP_VEL
!
!---- Get ECI position/velocity/acceleration of station 2 at T+DD_12
!     
      MATR = 0.0D0
      CALL ERR_PASS ( IUER, IER )
      CALL NERS_GET_EOP ( VTD%NERS, (VTD%MOM%MJD - J2000__MJD)*86400.0D0 &
     &                    + VTD%MOM%TAI - DD_12, 'matall', MP, L_PAR, MATR, IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           STR = MJDSEC_TO_DATE ( VTD%MOM%MJD, VTD%MOM%TAI - DD_12, IER )
           CALL ERR_LOG ( 2382, IUER, 'VTD_JN2019', 'Error in an attempt '// &
     &         'to compute the transformation matrix from the terrestrial '// &
     &         'to the inertial coordinate system on '//STR(1:24) )
           RETURN 
      END IF
!
      X2_DD = 0.0D0
      V2_DD = 0.0D0
      A2_DD = 0.0D0
      X2_TRS = VTD%STA(ISTA2)%MOM_COO_TRS + PCO_STA2
      
      CALL MUL_MV_IV_V ( 3, 3, MATR(1,1,1), &
     &                      3, X2_TRS, &
     &                      3, X2_DD, IER )
      CALL MUL_MV_IV_V ( 3, 3, MATR(1,1,2), &
     &                      3, X2_TRS, &
     &                      3, V2_DD, IER )
      CALL MUL_MV_IV_V ( 3, 3, MATR(1,1,3), &
     &                      3, X2_TRS, &
     &                      3, A2_DD, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 2383, IUER, 'VTD_JN2019', 'Error in an attempt '// &
     &         'to convert station 2 ECEF coords to ECI')
           RETURN 
      END IF
      X0_BAR = V0_DT1 * DELTA_1 + X0_DT1
      X2_BAR = V2_DD * (-DD_12) + X2_DD
!
! --- Delay
!
      X_01 = X0_BAR - X1 
      TG_01 = 2.0D0*SOTID__GEO_CG/VTD__C**3 * LOG ( (NORM2 ( X0 ) + NORM2 ( X1 ) &
     &        + NORM2 ( X_01 )) / ABS ( (NORM2 ( X0 ) + NORM2 ( X1 ) - NORM2 ( X_01 )) ))
      DELTA_T0 = 1.0D0/VTD__C**2 * DOT_PRODUCT ( X_01, V0 ) - &
     &           1.0D0/VTD__C * G_T0 * SQRT ( DOT_PRODUCT ( X_01, X_01) &
     &           + DOT_PRODUCT ( X_01, V0 )**2 / VTD__C**2 ) - TG_01
      
      X_02 = X0_BAR - X2_BAR + (V0 - V2)*DELTA_T0
      TG_02 = 2.0D0*SOTID__GEO_CG/VTD__C**3 * LOG ( (NORM2 ( X0 ) + NORM2 ( X2 ) + NORM2 ( X_02 ))/&
     &        ABS (NORM2 ( X0 ) + NORM2 ( X2 ) - NORM2 ( X_02 )) )
      DELTA_T2 = -1.0D0/VTD__C**2 * DOT_PRODUCT ( X_02, V2 ) + &
     &           1.0D0/VTD__C * G_T2 * SQRT ( DOT_PRODUCT ( X_02, X_02 ) &
     &           + DOT_PRODUCT ( X_02, V2 )**2 / VTD__C**2 ) + TG_02
      TAU_GEOM = DELTA_T2 + DELTA_T0 + TAU_PCV + TAU_PHASE_WINDUP
!
!-----Calculation of  Delay Partials
!
      IDEN_MAT3(1,1) = 1.0D0 ; IDEN_MAT3(1,2) = 0.0D0 ; IDEN_MAT3(1,3) = 0.0D0
      IDEN_MAT3(2,1) = 0.0D0 ; IDEN_MAT3(2,2) = 1.0D0 ; IDEN_MAT3(2,3) = 0.0D0
      IDEN_MAT3(3,1) = 0.0D0 ; IDEN_MAT3(3,2) = 0.0D0 ; IDEN_MAT3(3,3) = 1.0D0
!
      D_DELTA1_DX1 = 1.0D0/VTD__C * (X1 - X0) / NORM2 ( X1 - X0 )
      D_DELTA2_DX2 = 1.0D0/VTD__C * (X2 - X0) / NORM2 ( X2 - X0 )
      DT0_DX1 = 0.0D0
      DT2_DX1 = 0.0D0
      DT2_DX2 = 0.0D0
!      
      T0_EXP = DOT_PRODUCT ( X_01, X_01 ) + &
     &         DOT_PRODUCT ( X_01, V0 )**2/VTD__C**2
      T2_EXP = DOT_PRODUCT ( X_02, X_02 ) + &
     &         DOT_PRODUCT ( X_02, V2 )**2/VTD__C**2
      DO 450 IDX = 1, 3
          DX01_DX1_IDX(1:3) = D_DELTA1_DX1(IDX) * V0_DT1(1:3) - IDEN_MAT3(1:3,IDX)
          DT0_DX1(IDX) = 1.0D0/VTD__C**2 * DOT_PRODUCT ( DX01_DX1_IDX, V0 ) &
         &               - G_T0/VTD__C * 1 / SQRT ( T0_EXP ) * &
         &               (DOT_PRODUCT ( DX01_DX1_IDX, X_01 ) + 1/VTD__C**2 * &
         &               DOT_PRODUCT ( X_01, V0 ) * DOT_PRODUCT ( DX01_DX1_IDX, V0 ))

          DX02_DX1_IDX = V0_DT1 * D_DELTA1_DX1(IDX) - V2_DD * D_DELTA1_DX1(IDX) &
         &               + (V0 - V2) * DT0_DX1(IDX)
          DT2_DX1(IDX) = -1.0D0/VTD__C**2 * DOT_PRODUCT ( DX02_DX1_IDX, V2 ) &
         &               + G_T2/VTD__C * 1 / SQRT ( T2_EXP )  &
         &               * (DOT_PRODUCT ( DX02_DX1_IDX, X_02 ) + 1/VTD__C**2 * &
         &               DOT_PRODUCT ( X_02, V2 ) * DOT_PRODUCT ( DX02_DX1_IDX, V2))
          
          DX02_DX2_IDX = V2_DD * D_DELTA2_DX2(IDX) - IDEN_MAT3(:,IDX)
          DT2_DX2(IDX) = -1.0D0/VTD__C**2 * DOT_PRODUCT ( DX02_DX2_IDX, V2 ) &
         &               + G_T2/VTD__C * 1 / SQRT ( T2_EXP ) & 
         &               * (DOT_PRODUCT ( DX02_DX2_IDX, X_02 ) + 1/VTD__C**2 &
         &               * DOT_PRODUCT ( X_02, V2 ) * DOT_PRODUCT ( DX02_DX2_IDX , V2 ))
450   CONTINUE

!
!-----Set MATR to current epoch to convert CRS partials to TRS
!
      MATR = 0.0D0  
      CALL ERR_PASS ( IUER, IER )
      CALL NERS_GET_EOP ( VTD%NERS, (VTD%MOM%MJD - J2000__MJD)*86400.0D0 &
     &                    + VTD%MOM%TAI, 'matall', MP, L_PAR, MATR, IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           STR = MJDSEC_TO_DATE ( VTD%MOM%MJD, VTD%MOM%TAI, IER )
           CALL ERR_LOG ( 2384, IUER, 'VTD_JN2019', 'Error in an attempt '// &
     &         'to compute the transformation matrix from the terrestrial '// &
     &         'to the terrestrical coordinate system on '//STR(1:24) )
           RETURN 
      END IF
!
      CRS_TO_TRS = TRANSPOSE ( MATR(1:3,1:3,1) )
      CALL MUL_MV_IV_V ( 3, 3, CRS_TO_TRS, &
     &                      3, DT2_DX1 + DT0_DX1, &
     &                      3, TAU_DER_STA1, IER )

      CALL MUL_MV_IV_V ( 3, 3, CRS_TO_TRS, &
     &                      3, DT2_DX2, &
     &                      3, TAU_DER_STA2, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 2385, IUER, 'VTD_JN2019', 'Error in an attempt '// &
     &         'to convert ECI delay derivatives to ECEF')
           RETURN 
      END IF
!      TAU_DER_STA1 = DT2_DX1 + DT0_DX1
!      TAU_DER_STA2 = DT2_DX2
!      
!-----Calculation of Delay Rate
!
      A0 = -SOTID__GEO_CG / NORM2 ( X0_ACC )**3 * X0_ACC
      A0_DT1 = -SOTID__GEO_CG / NORM2 ( X0_DT1_ACC )**3 * X0_DT1_ACC
      
      DELTA_1_DOT = 1.0D0/VTD__C * DOT_PRODUCT ( V1-V0, X1-X0 ) / NORM2 ( X1 - X0 )
      DX01_DT = A0_DT1*DELTA_1 + V0_DT1*DELTA_1_DOT + V0_DT1 - V1
      
      DT0_DT = (DOT_PRODUCT ( DX01_DT, V0 ) + DOT_PRODUCT ( X_01, A0 )) &
     &         / VTD__C**2 - 1.0D0/VTD__C*G_T0 * 1 / SQRT ( T0_EXP ) &
     &         * (DOT_PRODUCT ( X_01, DX01_DT ) + DOT_PRODUCT (X_01, V0 ) &
     &         / VTD__C**2 * (DOT_PRODUCT ( DX01_DT, V0 ) + DOT_PRODUCT ( X_01, A0 )))
      
      DELTA_2_DOT = 1.0D0/VTD__C * DOT_PRODUCT ( V2-V0, X2-X0 ) / NORM2 ( X2-X0 )
      DX02_DT = A0*DELTA_1 + V0_DT1*DELTA_1_DOT + V0_DT1 +  &
     &          A2_DD * (DELTA_2 - DELTA_1) + V2_DD*(DELTA_2_DOT - DELTA_1_DOT) &
     &          - V2_DD + (A0 - A2)*DELTA_T0 + (V0 - V2)*DT0_DT
      
      DT2_DT = -(DOT_PRODUCT ( DX02_DT, V2 ) + DOT_PRODUCT ( X_02, A2 )) & 
     &         / VTD__C**2 + 1.0D0/VTD__C*G_T2 * 1 / SQRT ( T2_EXP ) &
     &         * (DOT_PRODUCT ( X_02, DX02_DT ) + DOT_PRODUCT ( X_02, V2 ) &
     &         / VTD__C**2 * (DOT_PRODUCT ( DX02_DT, V2 ) + DOT_PRODUCT ( X_02, A2 )))
      
      RATE_GEOM = DT2_DT + DT0_DT
      W_Z = 2.0D0*PI__NUM/(23.0D0*3600.0D0+56.0D0*60.0D0+4.0916)
!
! ----Form approximate DV1_DX1, DV2_DX2, DA2_DX2 matrices
!
      DV1_DX1(1,1) = 0.0D0 ; DV1_DX1(1,2) = W_Z ;   DV1_DX1(1,3) = 0.0D0
      DV1_DX1(2,1) = -W_Z  ; DV1_DX1(2,2) = 0.0D0 ; DV1_DX1(2,3) = 0.0D0
      DV1_DX1(3,1) = 0.0D0 ; DV1_DX1(3,2) = 0.0D0 ; DV1_DX1(3,3) = 0.0D0
      
      DV2_DX2(1,1) = 0.0D0 ; DV2_DX2(1,2) = W_Z   ; DV2_DX2(1,3) = 0.0D0
      DV2_DX2(2,1) = -W_Z  ; DV2_DX2(2,2) = 0.0D0 ; DV2_DX2(2,3) = 0.0D0
      DV2_DX2(3,1) = 0.0D0 ; DV2_DX2(3,2) = 0.0D0 ; DV2_DX2(3,3) = 0.0D0
      
      DA2_DX2(1,1) = 2.0D0*W_Z**2 ; DA2_DX2(1,2) = 0.0D0         ; DA2_DX2(1,3) = 0.0D0
      DA2_DX2(2,1) = 0.0D0        ; DA2_DX2(2,2) = 2.0D0*W_Z**2  ; DA2_DX2(2,3) = 0.0D0
      DA2_DX2(3,1) = 0.0D0        ; DA2_DX2(3,2) = 0.0D0         ; DA2_DX2(3,3) = 0.0D0

      DT0DOT_DX1 = 0.0D0
      DT2DOT_DX1 = 0.0D0
      DT2DOT_DX2 = 0.0D0       
!
!-----Calculation of Delay Rate Partials      
!
      DO 460 IDX = 1, 3
          DV1_DX1_IDX = DV1_DX1(:,IDX)
          DV2_DX2_IDX = DV2_DX2(:,IDX)
          DA2_DX2_IDX = DA2_DX2(:,IDX)
      
          D_DELTADOT1_DX1_IDX = 1.0D0/VTD__C * DOT_PRODUCT ( DV1_DX1_IDX, X1-X0 ) &
         &                      / NORM2 ( X1-X0 ) + 1.0D0/VTD__C * (V1(IDX) - V0(IDX)) &
         &                      / NORM2 ( X1-X0 ) - 1.0D0/VTD__C**2 * &
         &                      DOT_PRODUCT ( V1-V0, X1-X0 ) / NORM2 ( X1-X0 )**3 &
         &                      * (X1(IDX) - X0(IDX))
          DX01_DX1_IDX = V0_DT1 * D_DELTA1_DX1(IDX) - IDEN_MAT3(:,IDX)

          DX01DOT_DX1_IDX = A0_DT1 * D_DELTA1_DX1(IDX) + V0_DT1 * &
         &                  D_DELTADOT1_DX1_IDX - DV1_DX1_IDX
      
          DT0DOT_DX1(IDX) = 1/VTD__C**2 * (DOT_PRODUCT ( DX01DOT_DX1_IDX, V0) + &
         &                  DOT_PRODUCT ( DX01_DX1_IDX, A0 )) &
         &                  + G_T0/VTD__C * 1 / T0_EXP**1.5D0 &
         &                  * (DOT_PRODUCT ( DX01_DX1_IDX, X_01 ) + 1.0D0/VTD__C**2 &
         &                  * DOT_PRODUCT ( X_01, V0 ) * DOT_PRODUCT ( DX01_DX1_IDX, V0 )) &
         &                  * (DOT_PRODUCT ( DX01_DT, X_01 ) + 1.0D0/VTD__C**2 &
         &                  * DOT_PRODUCT ( X_01, V0 ) * (DOT_PRODUCT ( DX01_DT, V0 ) &
         &                  + DOT_PRODUCT ( X_01, A0 ))) - G_T0/VTD__C * 1 / SQRT( T0_EXP ) &
         &                  * (DOT_PRODUCT ( DX01DOT_DX1_IDX, X_01 ) + &
         &                  DOT_PRODUCT ( DX01_DT, DX01_DX1_IDX ) + 1.0D0/VTD__C**2 &
         &                  * DOT_PRODUCT ( DX01_DX1_IDX, V0 ) * (DOT_PRODUCT ( DX01_DT, V0 ) &
         &                  + DOT_PRODUCT ( X_01, A0 )) + 1.0D0/VTD__C**2 &
         &                  * DOT_PRODUCT ( X_01, V0 ) * (DOT_PRODUCT ( DX01DOT_DX1_IDX, V0 ) &
         &                  + DOT_PRODUCT ( DX01_DX1_IDX, A0 )))
          D_DELTADOT2_DX2_IDX = 1.0D0/VTD__C * DOT_PRODUCT ( DV2_DX2(:, IDX), X2-X0 ) &
         &                      / NORM2 ( X2-X0 ) + 1.0D0/VTD__C * (V2(IDX) - V0(IDX)) &
         &                      / NORM2 ( X2-X0 ) - 1.0D0/VTD__C * DOT_PRODUCT ( V2-V0, X2-X0 ) &
         &                      / NORM2 ( X2-X0 )**3 * (X2(IDX) - X0(IDX))
          DX02_DX1_IDX = V0_DT1 * D_DELTA1_DX1(IDX) - V2_DD * D_DELTA1_DX1(IDX) &
         &               + (V0 - V2) * DT0_DX1(IDX)
          DX02_DX2_IDX = V2_DD * D_DELTA2_DX2(IDX) - IDEN_MAT3(:, IDX)
          DX02DOT_DX1_IDX = A0_DT1 * D_DELTA1_DX1(IDX) + V0_DT1 * D_DELTADOT1_DX1_IDX &
         &                  - A2_DD * D_DELTA1_DX1(IDX) - V2_DD * D_DELTADOT1_DX1_IDX &
         &                  + (A0 - A2) * DT0_DX1(IDX) + (V0 - V2) * DT0DOT_DX1(IDX)
          DX02DOT_DX2_IDX = DA2_DX2(:, IDX) * DD_12 + A2_DD * D_DELTA2_DX2(IDX) &
         &                  + DV2_DX2(:, IDX) * (DELTA_2_DOT - DELTA_1_DOT) &
         &                  + V2_DD * D_DELTADOT2_DX2_IDX - DV2_DX2(:, IDX) &
         &                  - DA2_DX2(:, IDX) * DELTA_T0 - DV2_DX2(:, IDX) * DT0_DT
!          
          DT2DOT_DX1(IDX) = -1/VTD__C**2*(DOT_PRODUCT ( DX02DOT_DX1_IDX, V2 ) +  &
         &                  DOT_PRODUCT ( DX02_DX1_IDX, A2 ) ) &
         &                  - G_T2/VTD__C* 1 / T2_EXP**1.5D0  &
         &                  * (DOT_PRODUCT ( DX02_DX1_IDX, X_02 ) + 1/VTD__C**2 &
         &                  * DOT_PRODUCT ( X_02, V2 ) * DOT_PRODUCT ( DX02_DX1_IDX, V2 )) &
         &                  * (DOT_PRODUCT ( X_02, DX02_DT ) + 1/VTD__C**2 &
         &                  * DOT_PRODUCT ( X_02, V2) * (DOT_PRODUCT ( DX02_DT, V2 ) &
         &                  + DOT_PRODUCT ( X_02, A2 ))) + G_T2/VTD__C &
         &                  * 1 / SQRT ( T2_EXP ) * (DOT_PRODUCT ( DX02_DX1_IDX, DX02_DT ) &
         &                  + DOT_PRODUCT ( X_02, DX02DOT_DX1_IDX ) + 1/VTD__C**2 &
         &                  * DOT_PRODUCT ( DX02_DX1_IDX, V2 ) * (DOT_PRODUCT ( DX02_DT, V2 ) &
         &                  + DOT_PRODUCT ( X_02, A2)) + 1/VTD__C**2 * DOT_PRODUCT ( X_02, V2 ) &
         &                  * (DOT_PRODUCT ( DX02DOT_DX1_IDX, V2 ) + DOT_PRODUCT ( DX02_DX1_IDX, A2 )))
!          
          DT2DOT_DX2(IDX) =  -1/VTD__C**2*( DOT_PRODUCT ( DX02DOT_DX2_IDX, V2 ) +  &
         &                  DOT_PRODUCT ( DX02_DT, DV2_DX2_IDX ) + DOT_PRODUCT ( &
         &                  DX02_DX2_IDX, A2 ) + DOT_PRODUCT ( X_02, DA2_DX2_IDX )) & 
         &                  + G_T2/(VTD__C * SQRT ( T2_EXP ))  &
         &                  * (-(DOT_PRODUCT(DX02_DX2_IDX, X_02) + 1/VTD__C**2 & 
         &                  * DOT_PRODUCT(X_02, V2) * (DOT_PRODUCT ( DX02_DX2_IDX, V2 ) & 
         &                  + DOT_PRODUCT(X_02, DV2_DX2_IDX)) ) * (DOT_PRODUCT ( DX02_DT, X_02 ) &
         &                  + 1/VTD__C**2 * DOT_PRODUCT ( X_02, V2 ) &
         &                  * (DOT_PRODUCT ( DX02_DT, V2 ) + DOT_PRODUCT ( X_02, A2 ))) &
         &                  / T2_EXP + DOT_PRODUCT ( DX02DOT_DX2_IDX, X_02 ) & 
         &                  + DOT_PRODUCT ( DX02_DT, DX02_DX2_IDX ) + 1/VTD__C**2 &
         &                  * (DOT_PRODUCT ( DX02_DX2_IDX, V2 ) + DOT_PRODUCT ( X_02, DV2_DX2_IDX )) &
         &                  * (DOT_PRODUCT ( DX02_DT, V2 ) + DOT_PRODUCT (X_02, A2) ) &
         &                  + 1/VTD__C**2 * DOT_PRODUCT ( X_02, V2 ) &
         &                  * (DOT_PRODUCT ( DX02DOT_DX2_IDX, V2 ) + DOT_PRODUCT ( DX02_DT, DV2_DX2_IDX )&
         &                  + DOT_PRODUCT ( DX02_DX2_IDX, A2 ) + DOT_PRODUCT ( X_02, DA2_DX2_IDX )))
              
460   CONTINUE
!      
      CALL MUL_MV_IV_V ( 3, 3, CRS_TO_TRS, &
     &                      3, DT2DOT_DX1 + DT0DOT_DX1, &
     &                      3, RATE_DER_STA1, IER )
      CALL MUL_MV_IV_V ( 3, 3, CRS_TO_TRS, &
     &                      3, DT2DOT_DX2, &
     &                      3, RATE_DER_STA2, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 2386, IUER, 'VTD_JN2019', 'Error in an attempt '// &
     &         'to convert ECI rate derivatives to ECEF')
           RETURN 
      END IF
!
!-----Satellite Partial Derivatives 
!
      DT0_DX0 = 0.0
      DT2_DX0 = 0.0
      DT0_DV0 = 0.0
      DT2_DV0 = 0.0
      DT0DOT_DX0 = 0.0
      DT2DOT_DX0 = 0.0
      DT0DOT_DV0 = 0.0
      DT2DOT_DV0 = 0.0
!
! ---- Create matrix of X0 x X0 outer/dyadic product -- X0_OP
!
      X0_OP(1,1) = X0(1)**2    ; X0_OP(1,2) = X0(1)*X0(2) ; X0_OP(1,3) = X0(1)*X0(3)
      X0_OP(2,1) = X0(1)*X0(2) ; X0_OP(2,2) = X0(2)**2    ; X0_OP(2,3) = X0(2)*X0(3)
      X0_OP(3,1) = X0(1)*X0(3) ; X0_OP(3,2) = X0(2)*X0(3) ; X0_OP(3,3) = X0(3)**2
      
      DA0_DX0 = -SOTID__GEO_CG/NORM2 ( X0 )**3 * IDEN_MAT3 + 3*SOTID__GEO_CG/NORM2 ( X0 )**5 & 
     &          * X0_OP
      D_DELTA1_DX0 = -(X1-X0) / (VTD__C*NORM2 ( X1 - X0 ))
      D_DELTA2_DX0 = -(X2-X0) / (VTD__C*NORM2 ( X2 - X0 ))
      D_TAU_DX0 = D_DELTA2_DX0 - D_DELTA1_DX0
      D_DELTA1DOT_DX0 = - (V1 - V0) / (VTD__C*NORM2 ( X1-X0 )) + DOT_PRODUCT ( V1-V0, X1-X0 ) &
     &                  * (X1 - X0) / (VTD__C*NORM2 ( X1-X0 )**3)
      D_DELTA2DOT_DX0 = - (V2 - V0) / (VTD__C*NORM2 ( X2-X0 )) + DOT_PRODUCT ( V2-V0, X2-X0 ) &
     &                  * (X2 - X0) / (VTD__C*NORM2 ( X2-X0 )**3)
      D_TAUDOT_DX0 = D_DELTA2DOT_DX0 - D_DELTA1DOT_DX0
      D_DELTA1DOT_DV0 = -(X1 - X0) / (VTD__C*NORM2 ( X1-X0 ))
      D_DELTA2DOT_DV0 = -(X2 - X0) / (VTD__C*NORM2 ( X2-X0 ))
      D_TAUDOT_DV0 = D_DELTA2DOT_DV0 - D_DELTA1DOT_DV0
      DG0_DV0 = V0 / (VTD__C**2 * (1-NORM2 ( V0 )**2/VTD__C**2)**(1.5D0))

      DO 470 IDX = 1, 3
!
!-------- delay position partials
!
          DX01_DX0_IDX = V0_DT1*D_DELTA1_DX0(IDX) + IDEN_MAT3(:,IDX)
          DT0_DX0(IDX) = 1/VTD__C**2*DOT_PRODUCT ( DX01_DX0_IDX, V0 ) - G_T0/VTD__C &
     &                   * (DOT_PRODUCT ( DX01_DX0_IDX, X_01 ) + DOT_PRODUCT ( X_01, V0 )/VTD__C**2 &
     &                   * DOT_PRODUCT ( DX01_DX0_IDX, V0 )) / SQRT ( T0_EXP )
!    
          DX02_DX0_IDX = V0_DT1*D_DELTA1_DX0(IDX) + IDEN_MAT3(:,IDX) &
     &                   + V2_DD*D_TAU_DX0(IDX) + (V0-V2)*DT0_DX0(IDX)
          DT2_DX0(IDX) = -1/VTD__C**2*DOT_PRODUCT ( DX02_DX0_IDX, V2 ) + G_T2/VTD__C&
     &                   *(DOT_PRODUCT ( DX02_DX0_IDX, X_02 ) + DOT_PRODUCT ( X_02, V2 )/VTD__C**2 &
     &                   * DOT_PRODUCT ( DX02_DX0_IDX, V2 )) / SQRT ( T2_EXP )
!
!--------delay velocity partials
!
         DX01_DV0_IDX = 0.0*IDEN_MAT3(:,IDX)
         DT0_DV0(IDX) = 1/VTD__C**2*(DOT_PRODUCT ( DX01_DV0_IDX, V0 ) + X_01(IDX)) &
     &                  - DG0_DV0(IDX)/VTD__C * SQRT ( T0_EXP ) &
     &                  - G_T0/VTD__C*(DOT_PRODUCT(DX01_DV0_IDX,X_01) + DOT_PRODUCT(X_01,V0)/VTD__C**2 &
     &                  *(DOT_PRODUCT(DX01_DV0_IDX,V0)+X_01(IDX))) / SQRT( T0_EXP )
         DX02_DV0_IDX = DELTA_T0*IDEN_MAT3(:,IDX) + (V0-V2)*DT0_DV0(IDX)
         DT2_DV0(IDX) = -1/VTD__C**2*DOT_PRODUCT ( DX02_DV0_IDX, V2 ) + G_T2/VTD__C &
     &                  *(DOT_PRODUCT ( DX02_DV0_IDX, X_02 ) + DOT_PRODUCT ( X_02, V2 )/VTD__C**2 &
     &                  * DOT_PRODUCT ( DX02_DV0_IDX, V2 )) / SQRT ( T2_EXP )
 
!   
!--------rate position partials
!
         DX01DOT_DX0_IDX = DA0_DX0(:,IDX)*DELTA_1 + A0_DT1*D_DELTA1_DX0(IDX) + V0_DT1*D_DELTA1DOT_DX0(IDX)
         DT0DOT_DX0(IDX) = 1/VTD__C**2*(DOT_PRODUCT ( DX01DOT_DX0_IDX, V0 ) + DOT_PRODUCT ( DX01_DX0_IDX, A0 ) &
     &                     + DOT_PRODUCT ( X_01, DA0_DX0(:,IDX) )) + G_T0/(VTD__C * SQRT ( T0_EXP )) &
     &                     *((DOT_PRODUCT ( DX01_DX0_IDX, X_01 ) + DOT_PRODUCT ( X_01, V0 )/VTD__C**2 &
     &                     * DOT_PRODUCT ( DX01_DX0_IDX, V0 )) * (DOT_PRODUCT ( DX01_DT, X_01 ) &
     &                     + DOT_PRODUCT ( X_01, V0 )/VTD__C**2 * (DOT_PRODUCT ( DX01_DT, V0 ) &
     &                     + DOT_PRODUCT ( X_01, A0 ))) / T0_EXP - DOT_PRODUCT ( DX01DOT_DX0_IDX, X_01 ) &
     &                     - DOT_PRODUCT ( DX01_DT, DX01_DX0_IDX ) - 1/VTD__C**2*DOT_PRODUCT ( DX01_DX0_IDX, V0 ) &
     &                     *(DOT_PRODUCT ( DX01_DT, V0 ) + DOT_PRODUCT ( X_01, A0 )) - DOT_PRODUCT ( X_01, V0 )/VTD__C**2 &
     &                     *(DOT_PRODUCT ( DX01DOT_DX0_IDX, V0 ) + DOT_PRODUCT ( DX01_DX0_IDX, A0 ) &
     &                     + DOT_PRODUCT ( X_01, DA0_DX0(:,IDX) )))
         DX02DOT_DX0_IDX = DA0_DX0(:,IDX)*DELTA_1 + A0_DT1*D_DELTA1_DX0(IDX) + V0_DT1*D_DELTA1DOT_DX0(IDX) &
     &                     + A2_DD*D_TAU_DX0(IDX) + V2_DD*D_TAUDOT_DX0(IDX) + DA0_DX0(:,IDX)*DELTA_T0 &
     &                     + (A0-A2)*DT0_DX0(IDX) + (V0-V2)*DT0DOT_DX0(IDX)
         DT2DOT_DX0(IDX) = -1/VTD__C**2*(DOT_PRODUCT ( DX02DOT_DX0_IDX, V2 ) + DOT_PRODUCT ( DX02_DX0_IDX, A2 )) &
     &                     - G_T2/(VTD__C * SQRT ( T2_EXP )) * ((DOT_PRODUCT ( DX02_DX0_IDX, X_02 ) &
     &                     + DOT_PRODUCT ( X_02, V2 )/VTD__C**2 * DOT_PRODUCT ( DX02_DX0_IDX, V2 )) &
     &                     *(DOT_PRODUCT ( DX02_DT, X_02 ) + DOT_PRODUCT ( X_02, V2 )/VTD__C**2 &
     &                     *(DOT_PRODUCT ( DX02_DT, V2 ) + DOT_PRODUCT ( X_02, A2 ))) / T2_EXP &
     &                     - DOT_PRODUCT ( DX02DOT_DX0_IDX, X_02 ) - DOT_PRODUCT ( DX02_DT, DX02_DX0_IDX ) &
     &                     - 1/VTD__C**2*DOT_PRODUCT ( DX02_DX0_IDX, V2 ) * (DOT_PRODUCT ( DX02_DT, V2 ) &
     &                     + DOT_PRODUCT ( X_02, A2 )) - DOT_PRODUCT ( X_02, V2 )/VTD__C**2 &
     &                     * (DOT_PRODUCT ( DX02DOT_DX0_IDX, V2 ) + DOT_PRODUCT ( DX02_DX0_IDX, A2 )))
!
!--------rate velocity partials
!
         DX01DOT_DV0_IDX = (1+DELTA_1_DOT)*IDEN_MAT3(:,IDX) + V0_DT1*D_DELTA1DOT_DV0(IDX)
         DT0DOT_DV0(IDX) = 1/VTD__C**2*(DOT_PRODUCT ( DX01DOT_DV0_IDX, V0 ) + DOT_PRODUCT ( DX01_DV0_IDX, A0 ) + DX01_DT(IDX)) &
     &                     - DG0_DV0(IDX)/VTD__C * (DOT_PRODUCT ( DX01_DT, X_01 ) + DOT_PRODUCT ( X_01, V0 )/VTD__C**2 &
     &                     *(DOT_PRODUCT ( DX01_DT, V0 ) + DOT_PRODUCT ( X_01, A0 ))) / SQRT ( T0_EXP ) &
     &                     + G_T0/(VTD__C * SQRT ( T0_EXP )) * ((DOT_PRODUCT ( DX01_DV0_IDX, X_01 ) &
     &                     + DOT_PRODUCT ( X_01, V0 )/VTD__C**2 * (DOT_PRODUCT ( DX01_DV0_IDX, V0 ) + X_01(IDX))) &
     &                     *(DOT_PRODUCT ( DX01_DT, X_01 ) + DOT_PRODUCT ( X_01, V0 )/VTD__C**2 &
     &                     *(DOT_PRODUCT ( DX01_DT, V0 ) + DOT_PRODUCT ( X_01, A0 ))) / T0_EXP &
     &                     - DOT_PRODUCT ( DX01DOT_DV0_IDX, X_01 ) - DOT_PRODUCT ( DX01_DT, DX01_DV0_IDX ) & 
     &                     - 1/VTD__C**2*(DOT_PRODUCT ( DX01_DV0_IDX, V0 ) + X_01(IDX)) * (DOT_PRODUCT ( DX01_DT, V0 ) &
     &                     + DOT_PRODUCT ( X_01, A0 )) - DOT_PRODUCT ( X_01, V0 )/VTD__C**2 &
     &                     *(DOT_PRODUCT ( DX01DOT_DV0_IDX, V0 ) + DOT_PRODUCT ( DX01_DV0_IDX, A0 ) + DX01_DT(IDX)))
!    
         DX02DOT_DV0_IDX = (1+DELTA_1_DOT+DT0_DT)*IDEN_MAT3(:,IDX) + V0_DT1*D_DELTA1DOT_DV0(IDX) &
     &                     + V2_DD*D_TAUDOT_DV0(IDX) + (A0-A2)*DT0_DV0(IDX) + (V0-V2)*DT0DOT_DV0(IDX)
         DT2DOT_DV0(IDX) = -1/VTD__C**2*(DOT_PRODUCT ( DX02DOT_DV0_IDX, V2 ) + DOT_PRODUCT ( DX02_DV0_IDX, A2 )) &
     &                     - G_T2/(VTD__C*SQRT ( T2_EXP )) * ((DOT_PRODUCT ( DX02_DV0_IDX, X_02 ) &
     &                     + DOT_PRODUCT ( X_02, V2 )/VTD__C**2 * DOT_PRODUCT ( DX02_DV0_IDX, V2 )) &
     &                     *(DOT_PRODUCT ( DX02_DT, X_02 ) + DOT_PRODUCT ( X_02, V2 )/VTD__C**2 &
     &                     *(DOT_PRODUCT ( DX02_DT, V2 ) + DOT_PRODUCT ( X_02, A2 ))) / T2_EXP &
     &                     - DOT_PRODUCT ( DX02DOT_DV0_IDX, X_02 ) - DOT_PRODUCT ( DX02_DT, DX02_DV0_IDX ) &
     &                     - 1/VTD__C**2*(DOT_PRODUCT ( DX02_DV0_IDX, V2 )) * (DOT_PRODUCT ( DX02_DT, V2 ) &
     &                     + DOT_PRODUCT ( X_02, A2 )) - DOT_PRODUCT ( X_02, V2 )/VTD__C**2 &
     &                     *(DOT_PRODUCT ( DX02DOT_DV0_IDX, V2 ) + DOT_PRODUCT ( DX02_DV0_IDX, A2 )))
470   CONTINUE
!
      TAU_DER_POS = DT0_DX0 + DT2_DX0
      TAU_DER_VEL = DT0_DV0 + DT2_DV0
      RATE_DER_POS = DT0DOT_DX0 + DT2DOT_DX0
      RATE_DER_VEL = DT0DOT_DV0 + DT2DOT_DV0
!      
      CALL ERR_LOG ( 0, IUER )
      RETURN

      END  SUBROUTINE  VTD_JN2019  !#!  
!
! ------------------------------------------------------------------------
!
      SUBROUTINE NERS_AZELHA_SAT ( NERS, TIM_TAI, STA_TRS, SAT_POS_CRS, &
     &                             SAT_VEL_CRS, &
     &                             REFR_MODE, AZ, EL, HA, DST, &
     &                             AZ_RATE, EL_RATE, HA_RATE, DST_RATE, &
     &                             IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  NERS_AZELHA_COMP computes azimuth, elevation, hour angle, *
! *   and their time derivatives at moment of time TIM_TAI for station   *
! *   with coordinates in the terrestrial coordinate system STA_TRS that *
! *   observes a source with coordinates in the barycentric celestial    *
! *   coordinate system RA, DEC. Aberration is NOT taken into account.   *
! *   Elevation is computed with respect to the normal to the reference  *
! *   ellipsoid. Vertical deflection is ignored. Optionally the          *
! *   elevation can be corrected for refraction.                         *
! *                                                                      *
! *   This routine can be used only for computation of azimuth and       *
! *   elevations of an object beyond the Solar system, such a as a star  *
! *   or a galaxy.                                                       *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! * TIM_TAI   ( REAL*8    ) -- Time for which the Earth orientation      *
! *                            parameter(s) is to be computed elapsed    *
! *                            since 2000.01.01_00:00:00.0 TAI. Unit:    *
! *                            sec. If TIM_TAI .LE. -1.0D14, then the    *
! *                            azimuth and elevation at the current      *
! *                            moment of time will be computed.          *
! * STA_TRS   ( REAL*8    ) -- Vector of station coordinates in the      *
! *                            rotating crust-fixed coordinate system.   *
! *                            Units: m.                                 *
! * SAT_POS_CRS ( REAL*8  ) -- Vector of sattellite position in the      *
! *                            celestial coordinate system.              *
! *                            Units: meters.                            *
! * SAT_VEL_CRS ( REAL*8  ) -- Vector of sattellite velocity in the      *
! *                            celestial coordinate system.              *
! *                            Units: meter/sec.                         *
! * REFR_MODE ( CHARACTER ) -- Refraction mode. Supported values:        *
! *                            none  -- refractivity is not accounted.   *
! *                            optic -- formula Bennett for optic range  *
! *                                     is used ( Bennett, G.G. (1982).  *
! *                                     "The Calculation of Astronomical *
! *                                     Refraction in Marine Navigation".*
! *                                     Journal of Navigation, 35(2),    *
! *                                     255-259.                         *
! *                            radio -- 3.13D-4/tg(el) expression        *
! *                                     suitable for radio waves is used.*
! *                            both formulae have a floor of 3 deg, i.e. *
! *                            refraction at elevations below 3 deg is   *
! *                            considered to be equal to refraction at   *
! *                            3 deg.                                    *
! *                                                                      *
! * _________________________ Output parameters: _______________________ *
! *                                                                      *
! * AZ        ( REAL*8    ) -- Azimuth in rad.                           *
! * EL        ( REAL*8    ) -- Elevation in rad.                         *
! * HA        ( REAL*8    ) -- Hour angle in rad.                        *
! * AZ_RATE   ( REAL*8    ) -- Time derivative of azimuth   in rad/s.    *
! * EL_RATE   ( REAL*8    ) -- Time derivative of elevation in rad/s.    *
! * HA_RATE   ( REAL*8    ) -- Time derivative of hour angle in rad/s.   *
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
! *    NERS ( NERS__TYPE ) -- The data structure that keeps internal     *
! *                           parameters related to the Network Earth    *
! *                           Rotation Service.                          *
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
! * ### 19-APR-2023  NERS_AZELHA_SAT  v1.0 (c) L. Petrov 19-APR-2023 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'astro_constants.i'
      INCLUDE   'ners.i'
      REAL*8     TIM_TAI, STA_TRS(3), SAT_POS_CRS(3), SAT_VEL_CRS(3), &
     &           AZ, EL, HA, AZ_RATE, EL_RATE, HA_RATE, DST, DST_RATE
      CHARACTER  REFR_MODE*(*)
      INTEGER*4  IUER 
      TYPE     ( NERS__TYPE ) :: NERS
      INTEGER*4  MP
      PARAMETER  ( MP = 18 ) 
      CHARACTER  STR*128, STR1*128, STR2*128
      INTEGER*4  L_PAR, IK, J1, IER 
      REAL*8     MATR(3,3,3), LONG, PP, RAD, LAT_GCN, LAT_GDT, MU, HEI_ELL, &
     &           STA_CRS(3), UEN_TO_TRS(3,3), UP_UEN(3), UP_TRS(3), UP_CRS(3), &
     &           UP_CRS_RATE(3), NORTH_TRS(3), NORTH_CRS(3), NORTH_CRS_RATE(3), &
     &           VEL_CRS(3), VEC_PROJ_EN(3), VAL, VEC1(3), VEC2(3), &
     &           EAST_CRS(3), EAST_CRS_RATE(3), N_PROJ, E_PROJ, N_PROJ_RATE, &
     &           E_PROJ_RATE, CF, SF, CL, SL, NORTH_UEN(3), VEC_PE_RATE(3), &
     &           VEL_EA(3), ACC_EA(3), SOU_CRS(3), S_APP(3), S_APP_RATE(3), &
     &           ACC_CRS(3), SV, SA, VEL_BAR_CRS(3), ACC_BAR_CRS(3), S_APP_MAG, &
     &           EL_USE, REFR_ANG, DST_APP, RA, DEC, S_TPC(3), TMP_VEC1(3), TMP_VEC2(3)
      REAL*8     VTD__C
      PARAMETER  ( VTD__C = 299792458.D0 )
      CHARACTER, EXTERNAL :: TIM_TO_DATE*23
      LOGICAL*4, EXTERNAL :: IS_R8_NAN
      REAL*8,    EXTERNAL :: ATAN_CS, DP_VV_V, DSPL8, D2SPL8
      INTEGER*4, EXTERNAL :: IXMN8 
!
      IF ( REFR_MODE == NERS__REFR_NONE ) THEN
           CONTINUE 
         ELSE IF ( REFR_MODE == NERS__REFR_OPTIC ) THEN
           CONTINUE 
         ELSE IF ( REFR_MODE == NERS__REFR_RADIO ) THEN
           CONTINUE 
         ELSE
           CALL ERR_LOG ( 4381, IUER, 'NERS_AZELHA_SAT', 'Wrong site '// &
    &          'refractivity mode: '//REFR_MODE//' while none, optic, '// &
    &          'or radio were expected' )
           RETURN
      END IF 
      IF ( IS_R8_NAN(TIM_TAI) ) THEN
           CALL ERR_LOG ( 4382, IUER, 'NERS_AZELHA_SAT', 'Argument TIM_TAI '// &
     &         'is not a number' )
           RETURN 
      END IF
      IF ( IS_R8_NAN(STA_TRS(1)) ) THEN
           CALL ERR_LOG ( 4383, IUER, 'NERS_AZELHA_SAT', 'Argument STA_TRS(1) '// &
     &         'is not a number' )
           RETURN 
      END IF
      IF ( IS_R8_NAN(STA_TRS(2)) ) THEN
           CALL ERR_LOG ( 4384, IUER, 'NERS_AZELHA_SAT', 'Argument STA_TRS(2) '// &
     &         'is not a number' )
           RETURN 
      END IF
      IF ( IS_R8_NAN(STA_TRS(3)) ) THEN
           CALL ERR_LOG ( 4385, IUER, 'NERS_AZELHA_SAT', 'Argument STA_TRS(3) '// &
     &         'is not a number' )
           RETURN 
      END IF
      IF ( IS_R8_NAN(SAT_POS_CRS(1)) ) THEN
           CALL ERR_LOG ( 4386, IUER, 'NERS_AZELHA_SAT', 'Argument SAT_POS_CRS(1) '// &
     &         'is not a number' )
           RETURN 
      END IF
      IF ( IS_R8_NAN(SAT_POS_CRS(2)) ) THEN
           CALL ERR_LOG ( 4387, IUER, 'NERS_AZELHA_SAT', 'Argument SAT_POS_CRS(2) '// &
     &         'is not a number' )
           RETURN 
      END IF
      IF ( IS_R8_NAN(SAT_POS_CRS(3)) ) THEN
           CALL ERR_LOG ( 4388, IUER, 'NERS_AZELHA_SAT', 'Argument SAT_POS_CRS(3) '// &
     &         'is not a number' )
           RETURN 
      END IF
!
! --- Compute longitude
!
      LONG = ATAN_CS ( STA_TRS(1), STA_TRS(2)  )
      IF ( LONG .LT. 0.0D0 ) THEN
           LONG = PI2 + LONG
      END IF
!
! --- Compute geodetic latitude
!
      PP  = DSQRT ( STA_TRS(1)**2 + STA_TRS(2)**2 )
      IF ( DABS(PP) .LT. NERS__ANG_EPS ) PP = NERS__ANG_EPS 
      RAD = DSQRT ( STA_TRS(3)**2 + PP**2 )
      IF ( DABS ( RAD - NERS__REA ) .GT. NERS__HEIGHT_MIN .AND. &
           DABS ( RAD - NERS__REA ) .LT. NERS__HEIGHT_MAX       ) THEN
!
           CALL CHASHL ( STR  )
           WRITE ( UNIT=STR, FMT='(3(F15.3,2X))' ) STA_TRS
           CALL ERR_LOG ( 4390, IUER, 'NERS_AZELHA_SAT', 'Wrong site '// &
     &                   'positions  -- '//TRIM(STR)//' -- they are '// &
     &                   'not on the surface of our planet' )
           RETURN
      END IF
!
      LAT_GCN = DATAN( STA_TRS(3)/PP )
!
! --- Computation of geodetic latitude
!
      MU = DATAN ( STA_TRS(3)/PP * ( (1.D0 - NERS__FE) + NERS__EXC_SQ*NERS__REA/RAD ) )  
!
      LAT_GDT = DATAN( ( (1.D0 - NERS__FE)*STA_TRS(3) + &
     &                    NERS__EXC_SQ*NERS__REA*DSIN(MU)**3 ) / &
     &                   ( (1.D0 - NERS__FE)* &
     &                   ( PP  - NERS__EXC_SQ*NERS__REA*DCOS(MU)**3 )) )
!
! ---- Computation of height above the ellipsoid
!
      HEI_ELL =   PP*DCOS(LAT_GDT)         &
     &          + STA_TRS(3)*DSIN(LAT_GDT) &
     &          - NERS__REA* DSQRT( 1.D0 - NERS__EXC_SQ*DSIN(LAT_GDT)**2 )
!
! --- Calculation matrix of transformation from UEN (local topocentric,
! --- (Up,East,North) ) to the CFS (crust-fixed (X,Y,Z) ) system
!
      CF = DCOS(LAT_GDT)
      SF = DSIN(LAT_GDT)
      CL = DCOS(LONG)
      SL = DSIN(LONG)
!
      UEN_TO_TRS(1,1) = CF*CL
      UEN_TO_TRS(2,1) = CF*SL
      UEN_TO_TRS(3,1) = SF
!
      UEN_TO_TRS(1,2) = -SL
      UEN_TO_TRS(2,2) =  CL
      UEN_TO_TRS(3,2) =  0.D0
!
      UEN_TO_TRS(1,3) = -SF*CL
      UEN_TO_TRS(2,3) = -SF*SL
      UEN_TO_TRS(3,3) =  CF
!
      CALL ERR_PASS ( IUER, IER )       
      CALL NERS_GET_EOP ( NERS, TIM_TAI, 'matall', MP, L_PAR, MATR, IER )
      IF ( IER .NE. 0 ) THEN
           STR = TIM_TO_DATE ( TIM_TAI, IER )
           CALL ERR_LOG ( 4391, IUER, 'NERS_AZELHA_SAT', 'Error in '//&
     &                   'computing the Earth rotation matrtix on epoch '// &
     &                    STR )
           RETURN
      END IF
!
! --- Compute the vector of local zenith in CRS and then
! --- rate of its change
!
      UP_UEN(1) = 1.0D0
      UP_UEN(2) = 0.0D0
      UP_UEN(3) = 0.0D0
      CALL MUL_MV_IV_V ( 3, 3, UEN_TO_TRS,  3, UP_UEN, 3, UP_TRS, IER )
      CALL MUL_MV_IV_V ( 3, 3, MATR(1,1,1), 3, UP_TRS, 3, UP_CRS, IER )
      CALL MUL_MV_IV_V ( 3, 3, MATR(1,1,2), 3, UP_TRS, 3, UP_CRS_RATE, IER )
!
      NORTH_UEN(1) = 0.0D0
      NORTH_UEN(2) = 0.0D0
      NORTH_UEN(3) = 1.0D0
      CALL MUL_MV_IV_V ( 3, 3, UEN_TO_TRS,  3, NORTH_UEN, 3, NORTH_TRS, IER )
      CALL MUL_MV_IV_V ( 3, 3, MATR(1,1,1), 3, NORTH_TRS, 3, NORTH_CRS, IER )
      CALL MUL_MV_IV_V ( 3, 3, MATR(1,1,2), 3, NORTH_TRS, 3, NORTH_CRS_RATE, IER )
!
! --- Compute velocity of the station
! --- First: TRS_TO_CRS' * R_VEC
!
      CALL MUL_MV_IV_V ( 3, 3, MATR(1,1,1), &
     &                      3, STA_TRS, &
     &                      3, STA_CRS, IER )
      CALL MUL_MV_IV_V ( 3, 3, MATR(1,1,2), &
     &                      3, STA_TRS, &
     &                      3, VEL_CRS, IER )
      CALL MUL_MV_IV_V ( 3, 3, MATR(1,1,3), &
     &                      3, STA_TRS, &
     &                      3, ACC_CRS, IER )
!
! --- Compute the true topocentic satellite vector and 
! --- DST  -- its magnitude, i.e. topocentric distance.
!
      S_TPC = SAT_POS_CRS - STA_CRS
      CALL NORM_VEC ( 3, S_TPC, DST )
!
! --- Compute S_APP -- apparent satellite position in the celestial
! --- coordinate system taking into account the averration
!
      CALL VM83 ( SAT_POS_CRS, (SAT_VEL_CRS - VEL_CRS), TMP_VEC1 )
      CALL VM83 ( SAT_POS_CRS, TMP_VEC1, TMP_VEC2 )
      S_APP = SAT_POS_CRS + TMP_VEC2/VTD__C/DSQRT(SAT_POS_CRS(1)**2 + SAT_POS_CRS(2)**2 + SAT_POS_CRS(3)**2)
!      
      S_TPC = SAT_POS_CRS - STA_CRS
      CALL NORM_VEC ( 3, S_TPC, DST )
      CALL NORM_VEC ( 3, S_APP, S_APP_MAG )
!
! --- Compute geometric elevation angle (no refraction, just aberration)
!
      EL = DASIN ( DP_VV_V ( 3, UP_CRS, S_APP ) )
!
! ----------- ... and its rate
!
      SA = 0.0 ! we neglect satellite acceleration
      CALL ADDC_VV ( 3, 1.0D0/NERS__C/S_APP_MAG, &
     &                  ACC_BAR_CRS, -SA/NERS__C/S_APP_MAG, &
     &                  SAT_POS_CRS, S_APP_RATE )
      EL_RATE =  ( DP_VV_V( 3, UP_CRS_RATE, S_APP) + DP_VV_V( 3, UP_CRS, S_APP_RATE) )/ &
     &             DSQRT ( 1.0D0 - DP_VV_V ( 3, UP_CRS, S_APP )**2 )
!
! --- Now let us compute azimuth. In order to do it, first compute
! --- the projection of the source vector to the horizontal plane
!
      CALL ADDC_VV  ( 3, 1.0D0, S_APP, -DP_VV_V( 3, UP_CRS, S_APP ), &
     &                UP_CRS, VEC_PROJ_EN )
      CALL NORM_VEC ( 3, VEC_PROJ_EN, VAL )
      VEC_PE_RATE =    S_APP_RATE &
     &               - DP_VV_V ( 3, S_APP,      UP_CRS      )*UP_CRS_RATE &
     &               - DP_VV_V ( 3, S_APP_RATE, UP_CRS      )*UP_CRS &
     &               - DP_VV_V ( 3, S_APP,      UP_CRS_RATE )*UP_CRS
      VEC_PE_RATE = (  VEC_PE_RATE &
     &               - DP_VV_V ( 3, VEC_PROJ_EN, VEC_PE_RATE )* &
     &                 VEC_PROJ_EN )/VAL
!
! --- Then compute the north projection of that projection ...
!
      N_PROJ = DP_VV_V ( 3, VEC_PROJ_EN, NORTH_CRS )
!
! --- ... and east projection of that projection
!
      CALL VM83 ( NORTH_CRS, UP_CRS, EAST_CRS )
      E_PROJ = DP_VV_V ( 3, VEC_PROJ_EN, EAST_CRS  )
!
! --- ... and its time derivative.
!
      N_PROJ_RATE = DP_VV_V ( 3, VEC_PE_RATE, NORTH_CRS ) + &
     &              DP_VV_V ( 3, VEC_PROJ_EN, NORTH_CRS_RATE )
      CALL VM83 ( NORTH_CRS_RATE, UP_CRS, VEC1 )
      CALL VM83 ( NORTH_CRS, UP_CRS_RATE, VEC2 )
      EAST_CRS_RATE = VEC1 + VEC2
      E_PROJ_RATE = DP_VV_V ( 3, VEC_PE_RATE, EAST_CRS ) + &
     &              DP_VV_V ( 3, VEC_PROJ_EN, EAST_CRS_RATE )
!
! --- From these two projections we get the azimuth. Ugh!
!
      AZ = ATAN_CS ( N_PROJ, E_PROJ )
      AZ_RATE  = ( E_PROJ_RATE*N_PROJ - E_PROJ*N_PROJ_RATE   )/ &
     &           ( E_PROJ**2 + N_PROJ**2 )
!
      IF ( REFR_MODE == NERS__REFR_OPTIC ) THEN
           EL_USE = MAX ( 3.0D0*DEG__TO__RAD, EL )
           REFR_ANG = 2.909D-4/DTAN(EL_USE + 2.126D-3/(EL_USE + 0.0768))
           EL = EL + REFR_ANG
         ELSE IF ( REFR_MODE == NERS__REFR_RADIO ) THEN
           EL_USE = MAX ( 3.0D0*DEG__TO__RAD, EL )
           REFR_ANG = 3.13D-4/DTAN(EL_USE)
           EL = EL + REFR_ANG
      END IF
!
      CALL DECPOL ( 3, S_APP, DST_APP, RA, DEC, IER )
!
      HA = ATAN_CS ( STA_CRS(1), STA_CRS(2) ) - RA
      IF ( HA >  PI__NUM ) HA = HA - PI2
      IF ( HA < -PI__NUM ) HA = HA + PI2
      HA_RATE = ( STA_CRS(1)*VEL_CRS(2) - VEL_CRS(1)*STA_CRS(2) )/ &
     &          ( STA_CRS(1)**2 + STA_CRS(2)**2 )
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE NERS_AZELHA_SAT !#!#
