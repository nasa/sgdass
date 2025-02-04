      SUBROUTINE ERM_TO_EOP ( ERM, TIM_STEP, UZT_MODEL, UZT_USE, &
     &                        MP, NP, EOP, L_HEO, HEO, &
     &                        HEO_EPOCH_SEC, IVRB, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine ERM_TO_EOP
! *                                                                      *
! *  ### 01-JUL-2021   ERM_TO_EOP  v2.1 (c)  L. Petrov  20-OCT-2023 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'solve.i'
      INCLUDE   'getpar.i'
      INCLUDE   'vtd.i'
      INCLUDE   'erm.i'
      INTEGER*4  UZT_MODEL, UZT_USE, MP, NP, L_HEO, IVRB, IUER
      REAL*8     TIM_STEP, HEO_EPOCH_SEC
      TYPE     ( ERM__TYPE   ) :: ERM
      TYPE     ( EOP__STRU  ) :: EOP(MP)
      TYPE     ( HEO__STRUC ) :: HEO(MAX(1,L_HEO))
      INTEGER*4  MDEG
      PARAMETER  ( MDEG  = 3 )
      REAL*8     ERM_TIM_MIN, ERM_TIM_MAX
      REAL*8     TIM_EOP, TAI, AVR, RMS, E3, E3_DOT, E3_DT2, &
     &           U1_UZT, U1_UZT_RATE, U1_UZT_ACCL
      REAL*8     T1(MP), X1(MP), T2(MP), X2(MP), E1(MP)
      REAL*8     HEO_VEC(3), HEO_VEC_DER1(3), HEO_VEC_DER2(3)
      REAL*8       ERR_MIN, ERR_DEFAULT, OM_PRC, TIM_EPS
      PARAMETER  ( ERR_MIN     = 1.0D-15  )
      PARAMETER  ( ERR_DEFAULT = 1.0D-8   )
      PARAMETER  ( TIM_EPS     = 1.D-3    )
      PARAMETER  ( OM_PRC = 7.08618327D-12 ) ! Precession rate (L. Petrov, 2002)
      REAL*8     T_SEC, ARG, VEL, ACC, DCOS_ARG, DSIN_ARG, PMC_AMP, PMS_AMP, &
     &           E3C_AMP, E3S_AMP, TEMP, SCL_APR, SCL_VAL, SCL_HEO
      REAL*8     EPS_KA, EPSILON_0 
      PARAMETER  ( EPS_KA = 1.D-10 )
      PARAMETER  ( EPSILON_0 = 0.4090928041D0 )  ! rad
      CHARACTER  STR*128, STR1*128
      LOGICAL*1  FL_NO_APR
      INTEGER*4  J1, J2, J3, J4, IND_KNOT(3), KA, MJD, IER
      INTEGER*4, EXTERNAL :: IXMN8 
      REAL*8,    EXTERNAL :: VAL_1D_BSPL, DER_1D_BSPL, DR2_1D_BSPL
      CHARACTER, EXTERNAL :: TIM_TO_DATE*23, MJDSEC_TO_DATE*30, JD_TO_DATE*23
!
      ERM_TIM_MIN = MAX ( ERM%TIM(1,1),             ERM%TIM(1,2),             ERM%TIM(1,3)             )
      ERM_TIM_MAX = MIN ( ERM%TIM(ERM%NKNOTS(1),1), ERM%TIM(ERM%NKNOTS(2),2), ERM%TIM(ERM%NKNOTS(3),3) )
      NP = IDNINT( (ERM_TIM_MAX - ERM_TIM_MIN)/TIM_STEP ) + 1
!
      CALL GETENVAR ( 'ERM_NO_APR', STR )
      IF ( STR(1:1) == 'y' .OR. STR(1:1) == 'Y' ) THEN
           FL_NO_APR = .TRUE.
           WRITE ( 6, '(A)' ) 'ERM apriori model will is set to zero' 
           SCL_APR = 0.0D0
         ELSE
           SCL_APR = 1.0D0
           FL_NO_APR = .FALSE.
      END IF
!
      CALL GETENVAR ( 'ERM_NO_VAL', STR )
      IF ( STR(1:1) == 'y' .OR. STR(1:1) == 'Y' ) THEN
           WRITE ( 6, '(A)' ) 'ERM value will is set to zero' 
           SCL_VAL = 0.0D0
         ELSE
           SCL_VAL = 1.0D0
      END IF
!
      CALL GETENVAR ( 'ERM_NO_HEO', STR )
      IF ( STR(1:1) == 'y' .OR. STR(1:1) == 'Y' ) THEN
           WRITE ( 6, '(A)' ) 'HEO value will is set to zero' 
           SCL_HEO = 0.0D0
         ELSE
           SCL_HEO = 1.0D0
      END IF
!
      IF ( NP > MP ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( NP, STR )
           CALL CLRCH ( STR1 )
           CALL INCH  ( MP, STR1 )
           CALL ERR_LOG ( 3281, IUER, 'ERM_TO_EOP', 'Parameter NP '//TRIM(STR)// &
     &         ' is too small. It needs to be at least '//STR1 )
           RETURN 
      END IF
      ERM%UZM = UZT_MODEL
      ERM%UZU = UZT_USE
!
      DO 410 J1=1,3
         CALL ERR_PASS ( IUER, IER )
         CALL BSPL_1D_CMP ( MDEG, 0, ERM%NKNOTS(J1), ERM%TIM(1,J1), ERM%APR(1-MDEG,J1), IER )
         IF ( IER .NE. 0 ) THEN
              CALL CLRCH ( STR )
              CALL INCH  ( J1, STR )
              CALL ERR_LOG ( 3282, IUER, 'ERM_TO_EOP', 'Error in computing '// &
     &            'spline coefficients of the apriori EOP for component '//STR )
              RETURN 
         END IF
 410  CONTINUE 
!
      IF ( L_HEO > 0 ) THEN
           DO 420 J2=1,L_HEO
              IF ( HEO(J2)%FREQ < 1.D-16 .OR. HEO(J2)%FREQ > 2.0D-5 ) THEN
                   HEO(J2)%ROTANG(3,1) = 0.0D0
                   HEO(J2)%ROTANG(4,1) = 0.0D0
              END IF
 420       CONTINUE 
      END IF
!
! --- NB: units for XPL_V, YPL_V, and U1_V are radians
!
      KA  = 0
      AVR = 0.0D0
      RMS = 0.0D0
      DO 430 J3=1,NP
         TIM_EOP = ERM_TIM_MIN + (J3-1)*TIM_STEP !!! - 32.184D0 ! ??
         T_SEC   = TIM_EOP - 43200.0D0 - 32.184D0
!
         IF ( L_HEO > 0 ) THEN
              CALL ERR_PASS ( IUER, IER )
              CALL VTD_APPLY_HEO ( T_SEC, HEO_EPOCH_SEC, L_HEO, HEO, &
     &                             HEO_VEC, HEO_VEC_DER1, HEO_VEC_DER2, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( J3, STR )
                   CALL ERR_LOG ( 3282, IUER, 'ERM_TO_EOP', 'Error in computing '// &
     &                 'HEO vector and its time derivatives' )
                   RETURN 
              END IF
!
              HEO_VEC(1:2)      = 0.0D0
              HEO_VEC_DER1(1:2) = 0.0D0
              HEO_VEC_DER2(1:2) = 0.0D0
!
! ----------- Compute nutation angles from HEO expansion
!
              DO 440 J4=1,L_HEO
!
! -------------- Here we define nutation as harmonic variations in EOP around 
! -------------- Euler angles 1 and 2 shifted by OM__EAR + OM_PRC frequency i[
!
                 IF ( HEO(J4)%FREQ > -1.5D0*OM__EAR .AND. HEO(J4)%FREQ < -0.5D0*OM__EAR ) THEN
                      ARG = (0.5D0*HEO(J4)%ACCL*T_SEC + HEO(J4)%FREQ + OM__EAR + OM_PRC)*T_SEC + &
     &                      HEO(J4)%PHAS
                      VEL = HEO(J4)%ACCL*T_SEC + HEO(J4)%FREQ
                      ACC = HEO(J4)%ACCL
!
                      DCOS_ARG = DCOS(ARG)
                      DSIN_ARG = DSIN(ARG)
!
                      IF ( HEO(J4)%USE_VEL ) THEN
                           PMC_AMP = HEO(J4)%ROTANG(HEO__PMC,HEO__ANG) + &
     &                               HEO(J4)%ROTANG(HEO__PMC,HEO__VEL)*(T_SEC-HEO_EPOCH_SEC)
                           PMS_AMP = HEO(J4)%ROTANG(HEO__PMS,HEO__ANG) + &
     &                               HEO(J4)%ROTANG(HEO__PMS,HEO__VEL)*(T_SEC-HEO_EPOCH_SEC)
                           E3C_AMP = HEO(J4)%ROTANG(HEO__E3C,HEO__ANG) + &
     &                               HEO(J4)%ROTANG(HEO__E3C,HEO__VEL)*(T_SEC-HEO_EPOCH_SEC)
                           E3S_AMP = HEO(J4)%ROTANG(HEO__E3S,HEO__ANG) + &
     &                               HEO(J4)%ROTANG(HEO__E3S,HEO__VEL)*(T_SEC-HEO_EPOCH_SEC)
                         ELSE
                           PMC_AMP = HEO(J4)%ROTANG(HEO__PMC,HEO__ANG)
                           PMS_AMP = HEO(J4)%ROTANG(HEO__PMS,HEO__ANG)
                           E3C_AMP = HEO(J4)%ROTANG(HEO__E3C,HEO__ANG)
                           E3S_AMP = HEO(J4)%ROTANG(HEO__E3S,HEO__ANG)
                      END IF
!
                      HEO_VEC(1) = HEO_VEC(1) + PMC_AMP*DCOS_ARG + PMS_AMP*DSIN_ARG
                      HEO_VEC(2) = HEO_VEC(2) + PMC_AMP*DSIN_ARG - PMS_AMP*DCOS_ARG
!
                      HEO_VEC_DER1(1) = HEO_VEC_DER1(1) + &
     &                                  VEL*(-PMC_AMP*DSIN_ARG + PMS_AMP*DCOS_ARG)
                      HEO_VEC_DER1(2) = HEO_VEC_DER1(2) + &
     &                                  VEL*( PMC_AMP*DCOS_ARG + PMS_AMP*DSIN_ARG)
!
                      HEO_VEC_DER2(1) = HEO_VEC_DER2(1) + &
     &                                  ACC*(-PMC_AMP*DCOS_ARG - PMS_AMP*DSIN_ARG)
                      HEO_VEC_DER2(2) = HEO_VEC_DER2(2) + &
     &                                  ACC*(-PMC_AMP*DSIN_ARG + PMS_AMP*DCOS_ARG)
                 END IF
 440          CONTINUE 
            ELSE
              HEO_VEC      = 0.0D0
              HEO_VEC_DER1 = 0.0D0
              HEO_VEC_DER2 = 0.0D0
         END IF
!
         IF ( J3 < NP ) THEN
              IND_KNOT(1) = IXMN8 ( ERM%NKNOTS(1), ERM%TIM(1,1), TIM_EOP + TIM_EPS )
              IND_KNOT(2) = IXMN8 ( ERM%NKNOTS(2), ERM%TIM(1,2), TIM_EOP + TIM_EPS )
              IND_KNOT(3) = IXMN8 ( ERM%NKNOTS(3), ERM%TIM(1,3), TIM_EOP + TIM_EPS )
            ELSE
              IND_KNOT(1) = IXMN8 ( ERM%NKNOTS(1), ERM%TIM(1,1), TIM_EOP - TIM_EPS )
              IND_KNOT(2) = IXMN8 ( ERM%NKNOTS(2), ERM%TIM(1,2), TIM_EOP - TIM_EPS )
              IND_KNOT(3) = IXMN8 ( ERM%NKNOTS(3), ERM%TIM(1,3), TIM_EOP - TIM_EPS )
         END IF
!
         EOP(J3)%YPL_V   = SCL_APR*VAL_1D_BSPL ( TIM_EOP, ERM%NKNOTS(1), 3, IND_KNOT(1), ERM%TIM(1,1), ERM%APR(1-MDEG,1) ) + &
     &                     SCL_VAL*VAL_1D_BSPL ( TIM_EOP, ERM%NKNOTS(1), 3, IND_KNOT(1), ERM%TIM(1,1), ERM%VAL(1-MDEG,1) )
         EOP(J3)%XPL_V   = SCL_APR*VAL_1D_BSPL ( TIM_EOP, ERM%NKNOTS(2), 3, IND_KNOT(2), ERM%TIM(1,2), ERM%APR(1-MDEG,2) ) + &
     &                     SCL_VAL*VAL_1D_BSPL ( TIM_EOP, ERM%NKNOTS(2), 3, IND_KNOT(2), ERM%TIM(1,2), ERM%VAL(1-MDEG,2) )
!
         EOP(J3)%YPR_V   = SCL_APR*DER_1D_BSPL ( TIM_EOP, ERM%NKNOTS(1), 3, IND_KNOT(1), ERM%TIM(1,1), ERM%APR(1-MDEG,1) ) + &
     &                     SCL_VAL*DER_1D_BSPL ( TIM_EOP, ERM%NKNOTS(1), 3, IND_KNOT(1), ERM%TIM(1,1), ERM%VAL(1-MDEG,1) )
         EOP(J3)%XPR_V   = SCL_APR*DER_1D_BSPL ( TIM_EOP, ERM%NKNOTS(2), 3, IND_KNOT(2), ERM%TIM(1,2), ERM%APR(1-MDEG,2) ) + &
     &                     SCL_VAL*DER_1D_BSPL ( TIM_EOP, ERM%NKNOTS(2), 3, IND_KNOT(2), ERM%TIM(1,2), ERM%VAL(1-MDEG,2) )
!
         EOP(J3)%YPQ_V   = SCL_APR*DR2_1D_BSPL ( TIM_EOP, ERM%NKNOTS(1), 3, IND_KNOT(1), ERM%TIM(1,1), ERM%APR(1-MDEG,1) ) + &
     &                     SCL_VAL*DR2_1D_BSPL ( TIM_EOP, ERM%NKNOTS(1), 3, IND_KNOT(1), ERM%TIM(1,1), ERM%VAL(1-MDEG,1) )
         EOP(J3)%XPQ_V   = SCL_APR*DR2_1D_BSPL ( TIM_EOP, ERM%NKNOTS(2), 3, IND_KNOT(2), ERM%TIM(1,2), ERM%APR(1-MDEG,2) ) + &
     &                     SCL_VAL*DR2_1D_BSPL ( TIM_EOP, ERM%NKNOTS(2), 3, IND_KNOT(2), ERM%TIM(1,2), ERM%VAL(1-MDEG,2) )
!
         IF ( ERM%UZU .EQ. UZT__ADD .OR. ERM%UZU .EQ. UZT__SUBTRACT ) THEN
              MJD = IDINT ( (TIM_EOP + TIM_EPS)/86400.0D0 ) + J2000__MJD
              TAI = TIM_EOP - 86400.0D0*(MJD-J2000__MJD) + 32.184D0
              IF ( ERM%UZM == UZT__DICKMAN1993 ) THEN
                   CALL E3ZT_DICKMAN1993  (  0, MJD, TAI, E3, E3_DOT, E3_DT2 )
                 ELSE IF ( ERM%UZM == UZT__DICKMAN_PRINCIPLE ) THEN
                   CALL E3ZT_DICKMAN1993  ( 13, MJD, TAI, E3, E3_DOT, E3_DT2 )
                 ELSE IF ( ERM%UZM == UZT__DICKMAN_SHORT ) THEN
                   CALL E3ZT_DICKMAN1993  ( 12, MJD, TAI, E3, E3_DOT, E3_DT2 )
                 ELSE IF ( ERM%UZM == UZT__RE2014 ) THEN
                   CALL E3ZT_RE2014       ( 0, MJD, TAI, E3, E3_DOT, E3_DT2 )
                 ELSE IF ( ERM%UZM == UZT__RE2014_SHORT ) THEN
                   CALL E3ZT_RE2014       ( 1, MJD, TAI, E3, E3_DOT, E3_DT2 )
              END IF
              IF ( ERM%UZU == UZT__ADD ) THEN
                   U1_UZT      =  E3/UT1__TO__E3*SEC__TO__RAD
                   U1_UZT_RATE =  E3_DOT/UT1__TO__E3*SEC__TO__RAD
                   U1_UZT_ACCL =  E3_DT2/UT1__TO__E3*SEC__TO__RAD
                 ELSE IF ( ERM%UZU == UZT__SUBTRACT ) THEN
                   U1_UZT      = -E3/UT1__TO__E3*SEC__TO__RAD
                   U1_UZT_RATE = -E3_DOT/UT1__TO__E3*SEC__TO__RAD
                   U1_UZT_ACCL = -E3_DT2/UT1__TO__E3*SEC__TO__RAD
                   u1_uzt      = 0 ! %%%%%%%%%%
                   u1_uzt_rate = 0 ! %%%%%%%%%%
                   u1_uzt_accl = 0 ! %%%%%%%%%%
              END IF
            ELSE
              U1_UZT      = 0.0D0
              U1_UZT_RATE = 0.0D0
              U1_UZT_ACCL = 0.0D0
         END IF
!
         EOP(J3)%U1_V    =  (   SCL_APR*VAL_1D_BSPL ( TIM_EOP, ERM%NKNOTS(3), 3, IND_KNOT(3), ERM%TIM(1,3), ERM%APR(1-MDEG,3) ) &
     &                        + SCL_VAL*VAL_1D_BSPL ( TIM_EOP, ERM%NKNOTS(3), 3, IND_KNOT(3), ERM%TIM(1,3), ERM%VAL(1-MDEG,3) ) &
     &                        + SCL_HEO*HEO_VEC(3) &
     &                      ) * SEC__TO__RAD/UT1__TO__E3 + U1_UZT
         EOP(J3)%UTR_V   =  (   SCL_APR*DER_1D_BSPL ( TIM_EOP, ERM%NKNOTS(3), 3, IND_KNOT(3), ERM%TIM(1,3), ERM%APR(1-MDEG,3) ) &
     &                        + SCL_VAL*DER_1D_BSPL ( TIM_EOP, ERM%NKNOTS(3), 3, IND_KNOT(3), ERM%TIM(1,3), ERM%VAL(1-MDEG,3) ) &
     &                        + SCL_HEO*HEO_VEC_DER1(3) &
     &                      ) * SEC__TO__RAD/UT1__TO__E3 + U1_UZT_RATE
         EOP(J3)%UTQ_V   =  (   SCL_APR*DR2_1D_BSPL ( TIM_EOP, ERM%NKNOTS(3), 3, IND_KNOT(3), ERM%TIM(1,3), ERM%APR(1-MDEG,3) ) &
     &                        + SCL_VAL*DR2_1D_BSPL ( TIM_EOP, ERM%NKNOTS(3), 3, IND_KNOT(3), ERM%TIM(1,3), ERM%VAL(1-MDEG,3) ) &
     &                        + SCL_HEO*HEO_VEC_DER2(3) &
     &                      ) * SEC__TO__RAD/UT1__TO__E3 + U1_UZT_ACCL
!
! ------ Transform harmonic variations HEO_VEC 1,2 to Dpsi/Deps
!
         EOP(J3)%DPSI_V  = HEO_VEC(1)/DSIN(EPSILON_0)
         EOP(J3)%DEPS_V  = HEO_VEC(2)
!
         EOP(J3)%DPSI_E  = 30.0D-12/DSIN(EPSILON_0)
         EOP(J3)%DEPS_E  = 30.0D-12
!
         IF ( ERM%ERR(ERM%DEGREE(1)+IND_KNOT(1),1) < ERR_MIN ) ERM%ERR(ERM%DEGREE(1)+IND_KNOT(1),1) = ERR_DEFAULT
         IF ( ERM%ERR(ERM%DEGREE(2)+IND_KNOT(2),2) < ERR_MIN ) ERM%ERR(ERM%DEGREE(2)+IND_KNOT(2),2) = ERR_DEFAULT
         IF ( ERM%ERR(ERM%DEGREE(3)+IND_KNOT(3),3) < ERR_MIN ) ERM%ERR(ERM%DEGREE(3)+IND_KNOT(3),3) = ERR_DEFAULT
!
         EOP(J3)%YPL_E   = ERM%ERR(ERM%DEGREE(1)+IND_KNOT(1),1)
         EOP(J3)%XPL_E   = ERM%ERR(ERM%DEGREE(2)+IND_KNOT(2),2)
         EOP(J3)%U1_E    = ERM%ERR(ERM%DEGREE(3)+IND_KNOT(3),3)
!
         EOP(J3)%MJD_EOP = J2000__MJD + TIM_EOP/86400.0D0
         EOP(J3)%MJD_NUT = J2000__MJD + TIM_EOP/86400.0D0
         EOP(J3)%STATUS  = 0
         EOP(J3)%STATUS  = IBSET ( EOP(J3)%STATUS, XPL__GTP )
         EOP(J3)%STATUS  = IBSET ( EOP(J3)%STATUS, YPL__GTP )
         EOP(J3)%STATUS  = IBSET ( EOP(J3)%STATUS, U1__GTP )
!
         IF ( IVRB .GE. 2 ) THEN
              T1(J3) = TIM_EOP/YEAR__TO__SEC
              X1(J3) = DR2_1D_BSPL ( TIM_EOP, ERM%NKNOTS(3), 3, IND_KNOT(3), ERM%TIM(1,3), ERM%VAL(1-MDEG,3) )
              X1(J3) = (HEO_VEC(3) - U1_UZT)/SEC__TO__RAD
              X1(J3) = HEO_VEC(3)
              IF ( J3 < NP -10 ) THEN
                   AVR = AVR + X1(J3)
                   RMS = RMS + X1(J3)**2
                   KA  = KA  + 1
              END IF
         END IF
 430  CONTINUE 
      IF ( IVRB .GE. 12 ) THEN
           IF ( KA > 2 ) THEN
	        AVR = AVR/KA
	        RMS = DSQRT ( RMS/KA - (1.0D0 - EPS_KA)*AVR**2  )
                WRITE ( 6, * ) 'KA= ', KA, ' AVR = ', SNGL(AVR), ' RMS= ', SNGL(RMS)
           END IF
           CALL DIAGI_SETDEF ( IER, 'DIAGI_CTIT', 'E3 2nd derivative (rad)' )
           CALL DIAGI_SETDEF ( IER, 'DIAGI_UNIT', 'Time in years' )
           CALL DIAGI_1 ( NP, T1, X1, IER )
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  ERM_TO_EOP  !#!#
