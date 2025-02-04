      SUBROUTINE SUR_AZEL ( SUR, VTD, SOU_TYP, MJD, TAI, I_STA, I_SOU, &
     &                      AZ, ELEV, HA, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine SUR_AZEL computes azimuth and elevation with accuracy      *
! *   2.D-4 rad.                                                         *
! *                                                                      *
! *  ### 11-OCT-2005   SUR_AZEL    v4.0 (c)  L. Petrov  10-JAN-2022 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'vtd.i'
      INCLUDE   'sur_sked.i'
      TYPE     ( SUR__TYPE ) :: SUR
      TYPE     ( VTD__TYPE ) :: VTD
      INTEGER*4  MJD, I_STA, I_SOU, SOU_TYP, IUER
      REAL*8     TAI, AZ, ELEV, HA
!
      INTEGER*4  L_PAR
      REAL*8     TIM_MOM, S_ANG, S_ANG_RATE
      REAL*8     TRS_TO_CRS(3,3), TRS_TO_CRS_DER1(3,3), &
     &           TRS_TO_CRS_DER2(3,3), DTRS_TO_CRS_DEOP(3,3,3)
      REAL*8     AZ_RATE, ELEV_RATE, HA_RATE, UP_UEN(3), UP_CRS(3), UP_TRS(3)
      REAL*8     NORTH_UEN(3), NORTH_CRS(3), NORTH_TRS(3), COO_CRS(3)
      REAL*8     VEC_PROJ_EN(3), EAST_CRS(3)
      REAL*8     N_PROJ, E_PROJ, VAL, S_VEC(3), ALPHA, DELTA, DST, &
     &           TIM_TAI, TIM_NOW, ELEV_REFR
      REAL*8     ZEN_VEN(3), S_VEN(3), RD
      REAL*8     TIM_EPS, MAX__REFR
      PARAMETER  ( TIM_EPS = 1.D-3 )
      PARAMETER  ( MAX__REFR = 35.0D0/60.0D0*DEG__TO__RAD ) ! Maximum refraction
      REAL*8     AZ_TMP, EL_TMP, HA_TMP
      INTEGER*4  IPAR, KNOT, ITURN, IER
      INTEGER*4  PREC_CODE, NUT_CODE, NUT_GDS, EROT_COMPAT
      PARAMETER  ( PREC_CODE = PREC__IERS1996 )
      PARAMETER  ( NUT_CODE = NUT__PETA )
      PARAMETER  ( NUT_GDS     = NUT__GDS_NO  )
      PARAMETER  ( EROT_COMPAT = VTD__NONE    )
      CHARACTER, EXTERNAL :: MJDSEC_TO_DATE*30
      REAL*8,    EXTERNAL :: FSPL8, DP_VV_V, ATAN_CS
      INTEGER*4, EXTERNAL :: IXMN8, ILEN, I_LEN
!
! --- Store the date of the observation
!
      IF ( SUR%STATUS_SPL(SOU_TYP) == SUR__SPL ) THEN
           TIM_NOW = (MJD - SUR%MJD_START)*86400.0D0 + &
     &               (TAI - SUR%TAI_START)
           IF ( (TIM_NOW - SUR%TIM_SPL(1))          <  TIM_EPS ) TIM_NOW = TIM_EPS
           IF ( (TIM_NOW - SUR%TIM_SPL(SUR__M_SPL)) > -TIM_EPS ) TIM_NOW = SUR%TIM_SPL(SUR__M_SPL) - TIM_EPS
           KNOT = IXMN8 ( SUR__M_SPL, SUR%TIM_SPL, TIM_NOW )
           IF ( SOU_TYP == SUR__TYP_TAG ) THEN
                AZ   = FSPL8 ( TIM_NOW, SUR__M_SPL, SUR%TIM_SPL, &
     &                         SUR%VAL_SOU(1,I_STA,I_SOU,SUR__AZ), KNOT, &
     &                         SUR%SPL_SOU(1,I_STA,I_SOU,SUR__AZ) )
                ELEV = FSPL8 ( TIM_NOW, SUR__M_SPL, SUR%TIM_SPL, &
     &                         SUR%VAL_SOU(1,I_STA,I_SOU,SUR__EL), KNOT, &
     &                         SUR%SPL_SOU(1,I_STA,I_SOU,SUR__EL) )
                HA   = FSPL8 ( TIM_NOW, SUR__M_SPL, SUR%TIM_SPL, &
     &                         SUR%VAL_SOU(1,I_STA,I_SOU,SUR__HA), KNOT, &
     &                         SUR%SPL_SOU(1,I_STA,I_SOU,SUR__HA) )
             ELSE IF ( SOU_TYP == SUR__TYP_SEC ) THEN
                AZ   = FSPL8 ( TIM_NOW, SUR__M_SPL, SUR%TIM_SPL, &
     &                         SUR%VAL_SO2(1,I_STA,I_SOU,SUR__AZ), KNOT, &
     &                         SUR%SPL_SO2(1,I_STA,I_SOU,SUR__AZ) )
                ELEV = FSPL8 ( TIM_NOW, SUR__M_SPL, SUR%TIM_SPL, &
     &                         SUR%VAL_SO2(1,I_STA,I_SOU,SUR__EL), KNOT, &
     &                         SUR%SPL_SO2(1,I_STA,I_SOU,SUR__EL) )
                HA   = FSPL8 ( TIM_NOW, SUR__M_SPL, SUR%TIM_SPL, &
     &                         SUR%VAL_SO2(1,I_STA,I_SOU,SUR__HA), KNOT, &
     &                         SUR%SPL_SO2(1,I_STA,I_SOU,SUR__HA) )
             ELSE IF ( SOU_TYP == SUR__TYP_CAL ) THEN
                AZ   = FSPL8 ( TIM_NOW, SUR__M_SPL, SUR%TIM_SPL, &
     &                         SUR%VAL_CAL(1,I_STA,I_SOU,SUR__AZ), KNOT, &
     &                         SUR%SPL_CAL(1,I_STA,I_SOU,SUR__AZ) )
                ELEV = FSPL8 ( TIM_NOW, SUR__M_SPL, SUR%TIM_SPL, &
     &                         SUR%VAL_CAL(1,I_STA,I_SOU,SUR__EL), KNOT, &
     &                         SUR%SPL_CAL(1,I_STA,I_SOU,SUR__EL) )
                HA   = FSPL8 ( TIM_NOW, SUR__M_SPL, SUR%TIM_SPL, &
     &                         SUR%VAL_CAL(1,I_STA,I_SOU,SUR__HA), KNOT, &
     &                         SUR%SPL_CAL(1,I_STA,I_SOU,SUR__HA) )
             ELSE IF ( SOU_TYP == SUR__TYP_POC ) THEN
                AZ   = FSPL8 ( TIM_NOW, SUR__M_SPL, SUR%TIM_SPL, &
     &                         SUR%VAL_SOP(1,I_STA,I_SOU,SUR__AZ), KNOT, &
     &                         SUR%SPL_SOP(1,I_STA,I_SOU,SUR__AZ) )
                ELEV = FSPL8 ( TIM_NOW, SUR__M_SPL, SUR%TIM_SPL, &
     &                         SUR%VAL_SOP(1,I_STA,I_SOU,SUR__EL), KNOT, &
     &                         SUR%SPL_SOP(1,I_STA,I_SOU,SUR__EL) )
                HA   = FSPL8 ( TIM_NOW, SUR__M_SPL, SUR%TIM_SPL, &
     &                         SUR%VAL_SOP(1,I_STA,I_SOU,SUR__HA), KNOT, &
     &                         SUR%SPL_SOP(1,I_STA,I_SOU,SUR__HA) )
             ELSE IF ( SOU_TYP == SUR__TYP_PLA ) THEN
                AZ   = FSPL8 ( TIM_NOW, SUR__M_SPL, SUR%TIM_SPL, &
     &                         SUR%VAL_PLA(1,I_STA,I_SOU,SUR__AZ), KNOT, &
     &                         SUR%SPL_PLA(1,I_STA,I_SOU,SUR__AZ) )
                ELEV = FSPL8 ( TIM_NOW, SUR__M_SPL, SUR%TIM_SPL, &
     &                         SUR%VAL_PLA(1,I_STA,I_SOU,SUR__EL), KNOT, &
     &                         SUR%SPL_PLA(1,I_STA,I_SOU,SUR__EL) )
                HA   = FSPL8 ( TIM_NOW, SUR__M_SPL, SUR%TIM_SPL, &
     &                         SUR%VAL_PLA(1,I_STA,I_SOU,SUR__HA), KNOT, &
     &                         SUR%SPL_PLA(1,I_STA,I_SOU,SUR__HA) )
          END IF
!
          ITURN = IDINT ( AZ/PI2 )
          AZ = AZ - PI2*ITURN
!
          ITURN = IDINT ( HA/PI2 )
          HA = HA - PI2*ITURN
          IF ( HA > PI__NUM ) HA = HA - PI2
!
          CALL ERR_LOG ( 0, IUER )
          RETURN 
      END IF
!
      TIM_TAI = (MJD - J2000__MJD)*86400.0D0 + TAI
      IF ( SOU_TYP == SUR__TYP_TAG ) THEN
           CALL COPY_V ( 3, SUR%SOU(I_SOU)%S_VEC, S_VEC ) 
           ALPHA = SUR%SOU(I_SOU)%ALPHA
           DELTA = SUR%SOU(I_SOU)%DELTA
           DST   = SUR%SOU(I_SOU)%DST
         ELSE IF ( SOU_TYP == SUR__TYP_SEC ) THEN
           CALL COPY_V ( 3, SUR%SO2(I_SOU)%S_VEC, S_VEC ) 
           ALPHA = SUR%SO2(I_SOU)%ALPHA
           DELTA = SUR%SO2(I_SOU)%DELTA
           DST   = SUR%SO2(I_SOU)%DST
         ELSE IF ( SOU_TYP == SUR__TYP_CAL ) THEN
           CALL COPY_V ( 3, SUR%CAL(I_SOU)%S_VEC, S_VEC ) 
           ALPHA = SUR%CAL(I_SOU)%ALPHA
           DELTA = SUR%CAL(I_SOU)%DELTA
           DST   = SUR%CAL(I_SOU)%DST
         ELSE IF ( SOU_TYP == SUR__TYP_POC ) THEN
           CALL COPY_V ( 3, SUR%SOP(I_SOU)%S_VEC, S_VEC ) 
           ALPHA = SUR%SOP(I_SOU)%ALPHA
           DELTA = SUR%SOP(I_SOU)%DELTA
           DST   = SUR%SOP(I_SOU)%DST
         ELSE IF ( SOU_TYP == SUR__TYP_PLA ) THEN
           CALL COPY_V ( 3, SUR%PLA(I_SOU)%S_VEC, S_VEC ) 
           ALPHA = SUR%PLA(I_SOU)%ALPHA
           DELTA = SUR%PLA(I_SOU)%DELTA
           DST   = SUR%PLA(I_SOU)%DST
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL NERS_AZELHA_COMP ( VTD%NERS, TIM_TAI, SUR%STA(I_STA)%COO_TRS, ALPHA, DELTA, &
     &                        'radio', AZ, ELEV, HA, AZ_RATE, ELEV_RATE, HA_RATE, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 1664, IUER, 'SUR_AZEL', 'Error in computing '// &
     &         'azimth, elevation, and hour angle on epochs '// &
     &          MJDSEC_TO_DATE( MJD, TAI, IER ) )
           AZ = 0.0D0
           ELEV = 0.0D0
           HA = 0.0D0
           AZ_RATE = 0.0D0
           ELEV_RATE = 0.0D0
           HA_RATE = 0.0D0
           RETURN 
      END IF
!
      IF ( DST > 1.0D0 ) THEN
           ZEN_VEN(1) = 0.0D0
           ZEN_VEN(2) = 0.0D0
           ZEN_VEN(3) = SUR%STA(I_STA)%RAD
           S_VEN(1) = DCOS(ELEV)*DCOS(AZ)
           S_VEN(2) = DCOS(ELEV)*DSIN(AZ)
           S_VEN(3) = DSIN(ELEV)
           S_VEN = DST*S_VEN - ZEN_VEN
!!             write ( 6, * ) '1: az/el= ', sngl(az/deg__to__rad), sngl(elev/deg__to__rad) ! %%%
           CALL DECPOL ( 3, S_VEN, RD, AZ, ELEV, IER )
!!             write ( 6, * ) '2: az/el= ', sngl(az/deg__to__rad), sngl(elev/deg__to__rad) ! %%%
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  SUR_AZEL  !#!  
