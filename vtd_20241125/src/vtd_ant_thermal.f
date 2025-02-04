      SUBROUTINE VTD_ANT_THERMAL ( VTD, ISOU, ISTA, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  VTD_ANT_THERMAL  computes the contribution of the antenna *
! *   thermal expansion to the path delay. It is assumed that the        *
! *   information about dimension of antenna parts and their thermal     *
! *   expansion was loaded to VTD.                                       *
! *                                                                      *
! * ________________________ Input parameters: _________________________ *
! *                                                                      *
! *      ISTA ( INTEGER*4 ) -- Station index in internal VTD data        *
! *                            structures.                               *
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
! *       VTD ( RECORD    ) -- Object which keeps configuration and data *
! *                            related to VLBI Theoretical Delay (VTD)   *
! *                            package.                                  *
! * IUER ( INTEGER*4, OPT ) -- Universal error handler.                  *
! *                           Input: switch IUER=0 -- no error messages  *
! *                                  will be generated even in the case  *
! *                                  of error. IUER=-1 -- in the case of *
! *                                  error the message will be put on    *
! *                                  stdout.                             *
! *                           Output: 0 in the case of successful        *
! *                                   completion and non-zero in the     *
! *                                   case of error.                     *
! *                                                                      *
! * ### 23-APR-2008  VTD_ANT_THERMAL  v1.1 (c) L. Petrov 23-JUL-2014 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'vtd.i'
      TYPE     ( VTD__TYPE      ) :: VTD
      INTEGER*4  ISOU, ISTA, IUER
      REAL*8     HEI_VAR, OFF_VAR, LOS_VAR, HEI_REN(3), HEI_TRS(3), HEI_CRS(3)
      REAL*8     TEMP_LOW, TEMP_HIGH, AIR_TEMP 
      PARAMETER  ( TEMP_LOW  =   213.0D0 ) ! -60C
      PARAMETER  ( TEMP_HIGH =   333.0D0 ) ! +60C
      REAL*4     ARG
      REAL*4     EPS_MAR
      PARAMETER  ( EPS_MAR = 1.0E-5 )
      INTEGER*4  IND_STA, IND_AHM_STA, IND, IER
      REAL*8,    EXTERNAL :: DP_VV_V 
      REAL*4,    EXTERNAL :: VAL_1D_BSPL4
      INTEGER*4, EXTERNAL :: I_LEN, LTM_DIF
!
      IF ( VTD%STATUS_ANTI .NE. VTD__YES ) THEN
           CALL ERR_LOG ( 2481, IUER, 'VTD_ANT_THERMAL', 'Trap of internal '// &
     &         'control: Antenna information file has not been loaded' )
           RETURN 
      END IF
!
      VTD%STA(ISTA)%ANT_THERMAL_DEL = 0.0D0
      IND_STA = 0
      IND_AHM_STA = 0
      IF ( VTD%ANTI%N_ANT > 0 ) THEN
           IND_STA = LTM_DIF ( 1, VTD%ANTI%N_ANT, VTD%ANTI%STA_NAM, &
                               VTD%STA(ISTA)%IVS_NAME )
      END IF
      IF ( VTD%ANTI%N_AHM > 0 ) THEN
           IND_AHM_STA = LTM_DIF ( 1, VTD%ANTI%N_AHM, VTD%ANTI%AHM_STA_NAM, & 
                                   VTD%STA(ISTA)%IVS_NAME )
      END IF
      IF ( IND_AHM_STA > 0 ) IND_STA = 0
      IF ( IND_STA == 0 ) THEN
!
! -------- Nothing to do
!
           CALL ERR_LOG ( 0, IUER )
           RETURN 
      END IF
!
      IF ( VTD%CONF%ATHD_DATA_FROM  == VTD__METEO ) THEN
!
! -------- Surface temperature is tsaken from meteorological in situ measurements
!
           IF ( VTD%STA(ISTA)%AIR_TEMP_EFF  .LT. TEMP_LOW   .OR. &
     &          VTD%STA(ISTA)%AIR_TEMP_EFF  .GT. TEMP_HIGH       ) THEN
!
! ------------- Use IMA standard atmosphere instead of that.
!
                AIR_TEMP = 288.15D0 - 0.0065D0*VTD%STA(ISTA)%HEI_ELL
              ELSE 
                AIR_TEMP = VTD%STA(ISTA)%AIR_TEMP_EFF  
           END IF
         ELSE IF ( VTD%CONF%ATHD_DATA_FROM  == VTD__SPD ) THEN
           ARG = ( VTD%MOM%MJD*86400.0D0 + VTD%MOM%TAI ) - &
     &             VTD%SPD_3D(ISTA)%TIM_BEG - VTD%CONF%ATHD_LAG
!
           IF ( .NOT. ASSOCIATED ( VTD%SPD_3D(ISTA)%TIM_ARR ) ) THEN
                CALL ERR_LOG ( 2482, IUER, 'VTD_ANT_THERMAL', 'Trap of internal '// &
     &              'control: No SPD data were found for station '//VTD%SPD_3D(ISTA)%STA%NAME )
                RETURN 
           END IF
           IF ( ABS(ARG - VTD%SPD_3D(ISTA)%TIM_ARR(1) ) < &
     &             EPS_MAR*(VTD%SPD_3D(ISTA)%TIM_ARR(2) - VTD%SPD_3D(ISTA)%TIM_ARR(1)) ) THEN
                   IND = 1
                ELSE IF ( ABS(ARG - VTD%SPD_3D(ISTA)%TIM_ARR(VTD%SPD_3D(ISTA)%N_TIM)) < &
     &                    EPS_MAR*(VTD%SPD_3D(ISTA)%TIM_ARR(2) - VTD%SPD_3D(ISTA)%TIM_ARR(1)) ) THEN
                   IND = VTD%SPD_3D(ISTA)%N_TIM - 1
                ELSE 
                   IND = INT ( (ARG - VTD%SPD_3D(ISTA)%TIM_ARR(1))/ &
     &                            (VTD%SPD_3D(ISTA)%TIM_ARR(2) - VTD%SPD_3D(ISTA)%TIM_ARR(1)) ) &
     &                       + 1
           END IF
           AIR_TEMP = VAL_1D_BSPL4 ( ARG, VTD%SPD_3D(ISTA)%N_TIM, &
     &                               SPD__MDEG, IND, &
     &                               VTD%SPD_3D(ISTA)%TIM_ARR, &
     &                               VTD%SPD_3D(ISTA)%SUR_TEM )
      END IF
!
! --- Compute the height variation
!
      HEI_VAR = VTD%ANTI%INFO(IND_STA)%FOUNDATION_HEIGHT* &
     &          VTD%ANTI%INFO(IND_STA)%FOUNDATION_TE* &
     &          (AIR_TEMP - VTD%ANTI%INFO(IND_STA)%REF_TEMP) + &
!
     &          VTD%ANTI%INFO(IND_STA)%PILLAR_HEIGHT* &
     &          VTD%ANTI%INFO(IND_STA)%PILLAR_TE* &
     &          (AIR_TEMP - VTD%ANTI%INFO(IND_STA)%REF_TEMP)
!
! --- Compute the axis offset variation
!
      OFF_VAR = VTD%ANTI%INFO(IND_STA)%AXOF_LEN* &
     &          VTD%ANTI%INFO(IND_STA)%AXOF_TE*  &
     &          (AIR_TEMP - VTD%ANTI%INFO(IND_STA)%REF_TEMP)
!
! --- Compute the line-of-sight variation
!
      IF ( VTD%ANTI%INFO(IND_STA)%FOCUS_TYPE == ANTI__FO_PRIM ) THEN
           LOS_VAR = VTD%ANTI%INFO(IND_STA)%VERTEX_LEN* &
     &               VTD%ANTI%INFO(IND_STA)%VERTEX_TE*  &
     &              (AIR_TEMP - VTD%ANTI%INFO(IND_STA)%REF_TEMP)  &
!
     &             - VTD%ANTI%INFO(IND_STA)%SUBREFL_HEIGHT*ANTI__FO_ADM* &
     &               VTD%ANTI%INFO(IND_STA)%SUBREFL_TE* &
     &              (AIR_TEMP - VTD%ANTI%INFO(IND_STA)%REF_TEMP)
         ELSE IF ( VTD%ANTI%INFO(IND_STA)%FOCUS_TYPE == ANTI__FO_SECN ) THEN
           LOS_VAR = VTD%ANTI%INFO(IND_STA)%VERTEX_LEN* &
     &               VTD%ANTI%INFO(IND_STA)%VERTEX_TE*  &
     &              (AIR_TEMP - VTD%ANTI%INFO(IND_STA)%REF_TEMP) &
!
     &             - VTD%ANTI%INFO(IND_STA)%SUBREFL_HEIGHT*2.0D0*ANTI__FO_ADM* &
     &               VTD%ANTI%INFO(IND_STA)%SUBREFL_TE* &
     &              (AIR_TEMP - VTD%ANTI%INFO(IND_STA)%REF_TEMP)
      END IF
!
! --- Transform the vertical variation to the vector in the celecstial
! --- coordiante system, by applying two rotatios: UEN-->TRS and TRS-->CRS
!
      HEI_REN(1) = HEI_VAR
      HEI_REN(2) = 0.0D0
      HEI_REN(3) = 0.0D0
      CALL MUL_MV_IV_V ( 3, 3, VTD%STA(ISTA)%UEN_TO_TRS, &
     &                  3, HEI_REN, 3, HEI_TRS, IER )
      CALL MUL_MV_IV_V ( 3, 3, VTD%MOM%TRS_TO_CRS, 3, HEI_TRS, 3, HEI_CRS, IER )
!
! --- Finally, compute the thermal displacement contribution
!
      VTD%STA(ISTA)%ANT_THERMAL_DEL = &
     &          DP_VV_V ( 3, VTD%SOU(ISOU)%S_CRS, HEI_CRS )/VTD__C &
     &        + OFF_VAR*VTD%STA(ISTA)%AXIS_DELAY_DER &
     &        + LOS_VAR/VTD__C
!
      IF ( VTD%CONF%IVRB .GE. 2 ) THEN
           WRITE ( 6, 110 ) VTD%STA(ISTA)%IVS_NAME, &
     &                      AIR_TEMP, &
     &                      VTD%ANTI%INFO(IND_STA)%REF_TEMP, &
     &                      HEI_VAR*1.D3, OFF_VAR*1.D3, LOS_VAR*1.D3, &
     &                      VTD%STA(ISTA)%ANT_THERMAL_DEL*1.D12
 110       FORMAT ( 'ANT_THERMAL: ',A, ' Temp: ',F7.2,' ref: ',F7.2, &
     &              ' Hei_var: ', F6.2,' mm  Off_var: ', F6.2, &
     &              ' Los_var: ', F6.2,' mm  Delay: ', F6.2,' ps ' )
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  !#!#  VTD_ANT_THERMAL  #!#
