      SUBROUTINE VTD_AEM ( AEM, MJD, TAI, ERM_VAL, ERM_DER, ERM_DR2, L_HEO, &
     &                     HEO_EPOCH_SEC, HEO, TRS_TO_CRS, TRS_TO_CRS_DER1, &
     &                     TRS_TO_CRS_DER2, DTRS_TO_CRS_DEOP, UT1_M_TAI, &
     &                     SANG, IVRB, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine VTD_AEM  computes the matrix of transformation from the    *
! *   terrestrial coordinate system to the celestial coordinate system   *
! *   as well as its first and second time derivatives using             *
! *   the a priori Earth rotation matrix approach.                       *
! *                                                                      *
! *  ### 01-NOV-2006    VTD_AEM    v1.0 (c)  L. Petrov  01-NOV-2006 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'aem.i'
      INCLUDE   'heo.i'
      INTEGER*4  MJD, L_HEO, IVRB, IUER
      REAL*8     TAI, HEO_EPOCH_SEC, ERM_VAL(3), ERM_DER(3), ERM_DR2(3), &
     &           TRS_TO_CRS(3,3), TRS_TO_CRS_DER1(3,3), TRS_TO_CRS_DER2(3,3), &
     &           DTRS_TO_CRS_DEOP(3,3,3), UT1_M_TAI, SANG
      TYPE     ( AEM__TYPE  ) AEM
      TYPE     ( HEO__STRUC ) HEO(*)
      REAL*8     TARG_TAI, DZETA, TETA, ZA, EPS0, DZETA_RATE, TETA_RATE, &
     &           ZA_RATE, EPS0_RATE, CROSS_NUT_SCL_E3, PSI, EPS, PSI_RATE, &
     &           EPS_RATE, SANG_RATE
      REAL*8     RTM_DIU(3,3),   RTM_N1(3,3),    RTM_N2(3,3),    &
     &           RTM_N3(3,3),    RTM_P1(3,3),    RTM_P2(3,3),    &
     &           RTM_P3(3,3),    RTM_E1(3,3),    RTM_E2(3,3),    &
     &           DRTM_DIU(3,3),  DRTM_N1(3,3),   DRTM_N2(3,3),   &
     &           DRTM_N3(3,3),   DRTM_P1(3,3),   DRTM_P2(3,3),   &
     &           DRTM_P3(3,3),   DRTM_1(3,3),    DRTM_2(3,3),    &
     &           DRTM_3(3,3),    DRTM_4(3,3),    DRTM_5(3,3),    &
     &           DRTM_6(3,3),    DRTM_7(3,3),    SRTM_DIU(3,3),  &
     &           SRTM_1(3,3),    SRTM_2(3,3),    SRTM_3(3,3),    &
     &           SRTM_4(3,3),    SRTM_5(3,3),    SRTM_6(3,3),    &
     &           PRTM_DIU(3,3),  RTM_1(3,3),     &
     &           PRTM_E1(3,3),   PRTM_E2(3,3) 
      REAL*8     HEO_VEC(3),   HEO_VEC_DER1(3),   HEO_VEC_DER2(3)
      REAL*8     E3_ANG, E3_ANG_RATE
      REAL*8     ERM_MAT(3,3), ERM_MAT_DER1(3,3), ERM_MAT_DER2(3,3)
      INTEGER*4  J1, J2, IER
!
      TARG_TAI = (MJD - J2000__MJD - 0.5D0)*86400.0D0 + TAI 
!
      DZETA = ((AEM%DZETA(3)*TARG_TAI + AEM%DZETA(2))*TARG_TAI + &
     &          AEM%DZETA(1))*TARG_TAI + AEM%DZETA(0)
      TETA  = ((AEM%TETA(3)*TARG_TAI + AEM%TETA(2))*TARG_TAI + &
     &          AEM%TETA(1))*TARG_TAI + AEM%TETA(0)
      ZA    = ((AEM%Z(3)*TARG_TAI + AEM%Z(2))*TARG_TAI + &
     &          AEM%Z(1))*TARG_TAI + AEM%Z(0)
!
! --- Compute values of time derivatives of Newcomb-Andoyer variables
!
      DZETA_RATE = (3.0D0*AEM%DZETA(3)*TARG_TAI + 2.0D0*AEM%DZETA(2))*TARG_TAI + &
     &                    AEM%DZETA(1)
      TETA_RATE  = (3.0D0*AEM%TETA(3)*TARG_TAI + 2.0D0*AEM%TETA(2))*TARG_TAI + &
     &                    AEM%TETA(1)
      ZA_RATE    = (3.0D0*AEM%Z(3)*TARG_TAI + 2.0D0*AEM%Z(2))*TARG_TAI + &
     &                    AEM%Z(1)
!
! --- Compute the angle of mean inclination of ecliptic to the equator
!
      EPS0 = ((AEM%EPS0(3)*TARG_TAI + AEM%EPS0(2))*TARG_TAI + &
     &         AEM%EPS0(1))*TARG_TAI + AEM%EPS0(0)
      EPS0_RATE = (3.0D0*AEM%EPS0(3)*TARG_TAI + 2.0D0*AEM%EPS0(2))*TARG_TAI + &
     &                   AEM%EPS0(1)
!
!@      CROSS_NUT_SCL_E3 = &
!@     &     -(AEM%NUT_FRQ(1)*AEM%NUT_PSI_IN(1)*AEM%NUT_EPS_IN(1) + &
!@     &       AEM%NUT_FRQ(2)*AEM%NUT_PSI_IN(2)*AEM%NUT_EPS_IN(2) )* &
!@     &       DSIN(AEM%EPS0(0))/2.0D0
!
      PSI = 0.0D0
      EPS = 0.0D0
      PSI_RATE = 0.0D0
      EPS_RATE = 0.0D0
      CROSS_NUT_SCL_E3 = 0.0D0
      DO 410 J1=1,AEM%N_NUT
         IF ( AEM%NUT_PSI_IN(J1) > 1.D-8 .AND. &
     &        AEM%NUT_EPS_IN(J1) > 1.D-8       ) THEN
!
              CROSS_NUT_SCL_E3 = CROSS_NUT_SCL_E3 &
     &            -(AEM%NUT_FRQ(J1)*AEM%NUT_PSI_IN(J1)*AEM%NUT_EPS_IN(J1))* &
     &              DSIN(AEM%EPS0(0))/2.0D0
         END IF
!
         PSI = PSI + (AEM%NUT_PSI_IN(J1) + AEM%NUT_PSI_IN_RATE(J1)*TARG_TAI) * &
     &                DSIN( AEM%NUT_PHS(J1) + &
     &                      AEM%NUT_FRQ(J1)*TARG_TAI + &
     &                      AEM%NUT_ACC(J1)*TARG_TAI**2/2.0D0 ) + &
     &               (AEM%NUT_PSI_OUT(J1) + AEM%NUT_PSI_OUT_RATE(J1)*TARG_TAI) * &
     &                DCOS( AEM%NUT_PHS(J1) + &
     &                      AEM%NUT_FRQ(J1)*TARG_TAI + &
     &                      AEM%NUT_ACC(J1)*TARG_TAI**2/2.0D0 )
         EPS = EPS + (AEM%NUT_EPS_IN(J1) + AEM%NUT_EPS_IN_RATE(J1)*TARG_TAI) * &
     &               DCOS( AEM%NUT_PHS(J1) + &
     &                     AEM%NUT_FRQ(J1)*TARG_TAI + &
     &                     AEM%NUT_ACC(J1)*TARG_TAI**2/2.0D0 ) + &
     &               (AEM%NUT_EPS_OUT(J1) + AEM%NUT_EPS_OUT_RATE(J1)*TARG_TAI) * &
     &               DSIN( AEM%NUT_PHS(J1) + &
     &                     AEM%NUT_FRQ(J1)*TARG_TAI + &
     &                     AEM%NUT_ACC(J1)*TARG_TAI**2/2.0D0 )
!
         PSI_RATE = PSI_RATE + AEM%NUT_PSI_IN(J1)* &
     &                         ( AEM%NUT_FRQ(J1) + AEM%NUT_ACC(J1)*TARG_TAI )* &
     &                         DCOS( AEM%NUT_PHS(J1) + &
     &                               AEM%NUT_FRQ(J1)*TARG_TAI + &
     &                               AEM%NUT_ACC(J1)*TARG_TAI**2/2.0D0 ) - &
     &                         AEM%NUT_PSI_OUT(J1)* &
     &                         ( AEM%NUT_FRQ(J1) + AEM%NUT_ACC(J1)*TARG_TAI )* &
     &                         DSIN( AEM%NUT_PHS(J1) + &
     &                               AEM%NUT_FRQ(J1)*TARG_TAI + &
     &                               AEM%NUT_ACC(J1)*TARG_TAI**2/2.0D0 )
         EPS_RATE = EPS_RATE - AEM%NUT_EPS_IN(J1)* &
     &                         ( AEM%NUT_FRQ(J1) + AEM%NUT_ACC(J1)*TARG_TAI )* &
     &                         DSIN( AEM%NUT_PHS(J1) + &
     &                               AEM%NUT_FRQ(J1)*TARG_TAI + &
     &                               AEM%NUT_ACC(J1)*TARG_TAI**2/2.0D0 ) + &
     &                         AEM%NUT_EPS_OUT(J1)* &
     &                         ( AEM%NUT_FRQ(J1) + AEM%NUT_ACC(J1)*TARG_TAI )* &
     &                         DSIN( AEM%NUT_PHS(J1) + &
     &                               AEM%NUT_FRQ(J1)*TARG_TAI + &
     &                               AEM%NUT_ACC(J1)*TARG_TAI**2/2.0D0 )
 410  CONTINUE 
      E3_ANG      =   AEM%E3_POL(0)                &
     &              + AEM%E3_POL(1)*TARG_TAI       &
     &              + AEM%E3_POL(2)*TARG_TAI**2
      E3_ANG_RATE =   AEM%E3_POL(1)                &
     &              + 2.0D0*AEM%E3_POL(2)*TARG_TAI
      DO 420 J2=1,AEM%N_E3H
         E3_ANG =   E3_ANG &
     &            + AEM%E3_COS(J2)*DCOS(AEM%E3_FRQ(J2)*TARG_TAI) &
     &            + AEM%E3_SIN(J2)*DSIN(AEM%E3_FRQ(J2)*TARG_TAI) 
         E3_ANG_RATE =    E3_ANG_RATE &
     &           - AEM%E3_FRQ(J2)*AEM%E3_COS(J2)*DSIN(AEM%E3_FRQ(J2)*TARG_TAI) &
     &           + AEM%E3_FRQ(J2)*AEM%E3_SIN(J2)*DCOS(AEM%E3_FRQ(J2)*TARG_TAI) 
 420  CONTINUE 
!
! ---  Compute the hourly angle of the spring equinox
! ---  Straitforward computation
! ---  SANG = ( AEM%S0 - AEM%E3_POL(0) + PI ) + &
! ----        ( AEM%OMEGA_N + AEM%DZETA(1) + AEM%Z(1) - AEM%E3_POL(1) )*TARG_TAI + &
! ----        ( AEM%DZETA(2) + AEM%Z(2) - AEM%E3_POL(2) )*TARG_TAI**2 + &
! ----        PSI*DCOS(EPS0)
!
! --- is not suitable due to loss of precision. This trick is done
! --- for keeping S_ANG smaller: about 1 phase turn per year with
! --- respect to J2000 epoch
!
      SANG =  (MJD - J2000__MJD - 0.5D0)*(86400.0D0*AEM%OMEGA_N - PI2) &
     &      + TAI*AEM%OMEGA_N &
     &      + AEM%S0          &
     &      + ( AEM%DZETA(0) + AEM%Z(0) )             &
     &      + ( AEM%DZETA(1) + AEM%Z(1) )*TARG_TAI    &
     &      + ( AEM%DZETA(2) + AEM%Z(2) )*TARG_TAI**2 &
     &      + PSI*DCOS(EPS0) &
     &      - E3_ANG
!
      SANG_RATE =         ( AEM%OMEGA_N + AEM%DZETA(1) + AEM%Z(1) ) &
     &            + 2.0D0*( AEM%DZETA(2) + AEM%Z(2) )*TARG_TAI &
     &            + PSI_RATE*DCOS(EPS0) &
     &            - PSI*DSIN(EPS0)*EPS0_RATE &
     &            - E3_ANG_RATE
!
      IF ( IVRB .GE. 4 ) THEN
           WRITE ( 6, * ) '      TAI = ', TAI
           WRITE ( 6, * ) ' TARG_TAI = ', TARG_TAI
           WRITE ( 6, * ) '     EPS0 = ', EPS0
           WRITE ( 6, * ) '       S0 = ', AEM%S0
           WRITE ( 6, * ) '       E0 = ', AEM%E3_POL(0)
           WRITE ( 6, * ) '   CNS_E3 = ', CROSS_NUT_SCL_E3 
           WRITE ( 6, * ) '     SANG = ', SANG
           WRITE ( 6, * ) '      PSI = ', PSI
           WRITE ( 6, * ) '      EPS = ', EPS
           WRITE ( 6, * ) '    DZETA = ', DZETA
           WRITE ( 6, * ) '    THETA = ', TETA
           WRITE ( 6, * ) '       ZA = ', ZA
!
           WRITE ( 6, * ) ' DZETA_RATE = ', DZETA_RATE
           WRITE ( 6, * ) ' TETA_RATE  = ', TETA_RATE
           WRITE ( 6, * ) ' ZA_RATE    = ', ZA_RATE
           WRITE ( 6, * ) ' EPS0_RATE  = ', EPS0_RATE
           WRITE ( 6, * ) ' PSI_RATE   = ', PSI_RATE
           WRITE ( 6, * ) ' EPS_RATE   = ', EPS_RATE
           WRITE ( 6, * ) '         E1 = ', AEM%E3_POL(1)
           WRITE ( 6, * ) ' SANG_RATE  = ', SANG_RATE
      END IF
!
! --- Compute rotation matrices
!
      CALL VTD_ROTMAT ( 3,  DZETA,      RTM_P1   )
      CALL VTD_ROTMAT ( 2, -TETA,       RTM_P2   )
      CALL VTD_ROTMAT ( 3,  ZA,         RTM_P3   )
      CALL VTD_ROTMAT ( 1, -EPS0,       RTM_N1   )
      CALL VTD_ROTMAT ( 3,  PSI,        RTM_N2   )
      CALL VTD_ROTMAT ( 1,  EPS0 + EPS, RTM_N3   )
      CALL VTD_ROTMAT ( 3, -SANG,       RTM_DIU  )
!
      CALL MULTI_MUL_3 ( 7, TRS_TO_CRS, RTM_P1, RTM_P2, RTM_P3, &
     &                                  RTM_N1, RTM_N2, RTM_N3, RTM_DIU )
!
! --- Compute derivatives of the rotation matrices
!
      CALL VTD_ROTMAT_DER ( 3,  DZETA,       DZETA_RATE, DRTM_P1   )
      CALL VTD_ROTMAT_DER ( 2, -TETA,        TETA_RATE,  DRTM_P2   )
      CALL VTD_ROTMAT_DER ( 3,  ZA,          ZA_RATE,    DRTM_P3   )
      CALL VTD_ROTMAT_DER ( 1, -EPS0,        EPS0_RATE,  DRTM_N1   )
      CALL VTD_ROTMAT_DER ( 3,  PSI,         PSI_RATE,   DRTM_N2   )
      CALL VTD_ROTMAT_DER ( 1,  EPS0 + EPS,  EPS0_RATE + EPS_RATE, &
     &                                                   DRTM_N3   )
!
      CALL VTD_ROTMAT_DER ( 3, -SANG,        1.0D0,      PRTM_DIU  )
      CALL VEC_MULT_CONSTANT ( PRTM_DIU,  9, SANG_RATE,  DRTM_DIU  )
!
      PRTM_E1(1,1) =  0.0D0;  PRTM_E1(1,2) =  0.0D0;  PRTM_E1(1,3) =  0.0D0
      PRTM_E1(2,1) =  0.0D0;  PRTM_E1(2,2) =  0.0D0;  PRTM_E1(2,3) =  1.0D0
      PRTM_E1(3,1) =  0.0D0;  PRTM_E1(3,2) = -1.0D0;  PRTM_E1(3,3) =  0.0D0
!
      PRTM_E2(1,1) =  0.0D0;  PRTM_E2(1,2) =  0.0D0;  PRTM_E2(1,3) = -1.0D0
      PRTM_E2(2,1) =  0.0D0;  PRTM_E2(2,2) =  0.0D0;  PRTM_E2(2,3) =  0.0D0
      PRTM_E2(3,1) =  1.0D0;  PRTM_E2(3,2) =  0.0D0;  PRTM_E2(3,3) =  0.0D0
!
! --- Now compute first time derivative of TRS_TO_CRS. It is done in
! --- two steps: first compute 7 terms, each being a product of nine
! --- matrices, 7 rotation matrices and the 7-th first derivative
!
      CALL MULTI_MUL_3 ( 7, DRTM_1, DRTM_P1, RTM_P2,  RTM_P3, &
     &                              RTM_N1,  RTM_N2,  RTM_N3, RTM_DIU )
      CALL MULTI_MUL_3 ( 7, DRTM_2, RTM_P1,  DRTM_P2, RTM_P3, &
     &                              RTM_N1,  RTM_N2,  RTM_N3, RTM_DIU )
      CALL MULTI_MUL_3 ( 7, DRTM_3, RTM_P1,  RTM_P2,  DRTM_P3, &
     &                              RTM_N1,  RTM_N2,  RTM_N3, RTM_DIU )
      CALL MULTI_MUL_3 ( 7, DRTM_4, RTM_P1,  RTM_P2,  RTM_P3, &
     &                              DRTM_N1, RTM_N2,  RTM_N3, RTM_DIU )
      CALL MULTI_MUL_3 ( 7, DRTM_5, RTM_P1,  RTM_P2,  RTM_P3, &
     &                              RTM_N1,  DRTM_N2, RTM_N3, RTM_DIU )
      CALL MULTI_MUL_3 ( 7, DRTM_6, RTM_P1,  RTM_P2,  RTM_P3, &
     &                              RTM_N1,  RTM_N2, DRTM_N3, RTM_DIU )
      CALL MULTI_MUL_3 ( 7, DRTM_7, RTM_P1,  RTM_P2,  RTM_P3, &
     &                              RTM_N1,  RTM_N2,  RTM_N3, DRTM_DIU )
!
! -------- ... and at the second step we summ up the terms
!
      CALL MULTI_ADD_3 ( 7, TRS_TO_CRS_DER1, &
     &                       1.0D0, DRTM_1, &
     &                      -1.0D0, DRTM_2, &
     &                       1.0D0, DRTM_3, &
     &                      -1.0D0, DRTM_4, &
     &                       1.0D0, DRTM_5, &
     &                       1.0D0, DRTM_6, &
     &                      -1.0D0, DRTM_7  )
!
! --- Compute derivative of TRS_TO_CRS matrix with respect to Eurler
! --- angles.
!
      CALL MULTI_MUL_3 ( 8, DTRS_TO_CRS_DEOP(1,1,1), RTM_P1, RTM_P2,  &
     &                      RTM_P3, RTM_N1, RTM_N2,  RTM_N3, RTM_DIU, PRTM_E1 )
      CALL MULTI_MUL_3 ( 8, DTRS_TO_CRS_DEOP(1,1,2), RTM_P1, RTM_P2,  &
     &                      RTM_P3, RTM_N1, RTM_N2, RTM_N3,  RTM_DIU, PRTM_E2 )
      CALL MULTI_MUL_3 ( 7, DTRS_TO_CRS_DEOP(1,1,3), RTM_P1, RTM_P2,  &
     &                      RTM_P3, RTM_N1, RTM_N2, RTM_N3,  PRTM_DIU )
!
! --- Compute the second derivative of the rotation matrix. Only one
! --- term is taken into account. Relative error of the expression below
! --- is only 1.D-5
!
      CALL VTD_ROTMAT_DER2 ( 3, -SANG, SANG_RATE, 0.0D0, SRTM_DIU  )
!
      CALL MULTI_MUL_3 ( 7, SRTM_1,  RTM_P1,  RTM_P2,  RTM_P3,  &
     &                      RTM_N1,  RTM_N2,  RTM_N3, SRTM_DIU  )
      CALL MULTI_MUL_3 ( 7, SRTM_2, DRTM_P1,  RTM_P2,  RTM_P3,  &
     &                      RTM_N1,  RTM_N2,  RTM_N3,  DRTM_DIU )
      CALL MULTI_MUL_3 ( 7, SRTM_3,  RTM_P1, DRTM_P2,  RTM_P3,  &
     &                      RTM_N1,  RTM_N2,  RTM_N3, DRTM_DIU  )
      CALL MULTI_MUL_3 ( 7, SRTM_4,  RTM_P1,  RTM_P2, DRTM_P3,  &
     &                      RTM_N1,  RTM_N2,  RTM_N3, DRTM_DIU  )
      CALL MULTI_MUL_3 ( 7, SRTM_5,  RTM_P1,  RTM_P2,  RTM_P3,  &
     &                      RTM_N1, DRTM_N2,  RTM_N3, DRTM_DIU  )
      CALL MULTI_MUL_3 ( 7, SRTM_6,  RTM_P1,  RTM_P2,  RTM_P3,  &
     &                      RTM_N1,  RTM_N2, DRTM_N3, DRTM_DIU  )
!
      CALL MULTI_ADD_3 ( 6, TRS_TO_CRS_DER2, &
     &                       1.0D0,  SRTM_1, &
     &                      -2.0D0,  SRTM_2, &
     &                       2.0D0,  SRTM_3, &
     &                      -2.0D0,  SRTM_4, &
     &                      -2.0D0,  SRTM_5, &
     &                      -2.0D0,  SRTM_6  )
!
      IF ( L_HEO .GT. 0 ) THEN
!
! -------- Compute vector of perturbation Earth orientation and its the first
! -------- and the second time derivative
!
           CALL ERR_PASS ( IUER, IER )
           CALL VTD_APPLY_HEO ( TARG_TAI, HEO_EPOCH_SEC, L_HEO, HEO, &
     &                          HEO_VEC, HEO_VEC_DER1, HEO_VEC_DER2, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 2281, IUER, 'VTD_AEM', 'Error in '// &
     &              'an attempt to apply harmonic Earth orientation '// &
     &              'variations' )
                RETURN
           END IF
!
           IF ( IVRB .GE. 2 ) THEN
                WRITE ( 6, * ) ' HEO_VEC(1) = ', HEO_VEC(1)
                WRITE ( 6, * ) ' HEO_VEC(2) = ', HEO_VEC(2)
                WRITE ( 6, * ) ' HEO_VEC(3) = ', HEO_VEC(3)
           END IF
         ELSE
           CALL NOUT_R8 ( 3, HEO_VEC )
           CALL NOUT_R8 ( 3, HEO_VEC_DER1 )
           CALL NOUT_R8 ( 3, HEO_VEC_DER2 )
      END IF
!
      IF ( L_HEO > 0 .OR. &
     &     (ERM_VAL(1)**2 + ERM_VAL(2)**2 + ERM_VAL(3)**2) > 0.0D0 ) THEN
!
! -------- Build the rotational matrix and its time derivatives
!
           ERM_MAT(1,1) =  1.0D0
           ERM_MAT(2,1) = -HEO_VEC(3) - ERM_VAL(3)
           ERM_MAT(3,1) =  HEO_VEC(2) + ERM_VAL(2)
           ERM_MAT(1,2) =  HEO_VEC(3) + ERM_VAL(3)
           ERM_MAT(2,2) =  1.0D0
           ERM_MAT(3,2) = -HEO_VEC(1) - ERM_VAL(1)
           ERM_MAT(1,3) = -HEO_VEC(2) - ERM_VAL(2)
           ERM_MAT(2,3) =  HEO_VEC(1) + ERM_VAL(1)
           ERM_MAT(3,3) =  1.0D0
!
           ERM_MAT_DER1(1,1) =  0.0D0
           ERM_MAT_DER1(2,1) = -HEO_VEC_DER1(3) - ERM_DER(3)
           ERM_MAT_DER1(3,1) =  HEO_VEC_DER1(2) + ERM_DER(2)
           ERM_MAT_DER1(1,2) =  HEO_VEC_DER1(3) + ERM_DER(3)
           ERM_MAT_DER1(2,2) =  0.0D0
           ERM_MAT_DER1(3,2) = -HEO_VEC_DER1(1) - ERM_DER(1)
           ERM_MAT_DER1(1,3) = -HEO_VEC_DER1(2) - ERM_DER(2)
           ERM_MAT_DER1(2,3) =  HEO_VEC_DER1(1) + ERM_DER(1)
           ERM_MAT_DER1(3,3) =  0.0D0
!
           ERM_MAT_DER2(1,1) =  0.0D0
           ERM_MAT_DER2(2,1) = -HEO_VEC_DER2(3) - ERM_DR2(3)
           ERM_MAT_DER2(3,1) =  HEO_VEC_DER2(2) + ERM_DR2(2)
           ERM_MAT_DER2(1,2) =  HEO_VEC_DER2(3) + ERM_DR2(3)
           ERM_MAT_DER2(2,2) =  0.0D0
           ERM_MAT_DER2(3,2) = -HEO_VEC_DER2(1) - ERM_DR2(1)
           ERM_MAT_DER2(1,3) = -HEO_VEC_DER2(2) - ERM_DR2(2)
           ERM_MAT_DER2(2,3) =  HEO_VEC_DER2(1) + ERM_DR2(1)
           ERM_MAT_DER2(3,3) =  0.0D0
!
! -------- Multiply TRS_TO_CRS and HEO matrix
!
           CALL MUL_MM_II_I ( 3, 3, TRS_TO_CRS, 3, 3, ERM_MAT, 3, 3, RTM_1, IER )
!
! -------- Compute intermediate matrices for the first derivatives
!
           CALL MUL_MM_II_I ( 3, 3, TRS_TO_CRS_DER1, 3, 3, ERM_MAT, &
     &                        3, 3, DRTM_1, IER )
           CALL MUL_MM_II_I ( 3, 3, TRS_TO_CRS, 3, 3, ERM_MAT_DER1, &
     &                        3, 3, DRTM_2, IER )
!
! -------- ... and second derivative
!
           CALL MUL_MM_II_I ( 3, 3, TRS_TO_CRS_DER2, 3, 3, ERM_MAT, &
     &                        3, 3, SRTM_1, IER )
           CALL MUL_MM_II_I ( 3, 3, TRS_TO_CRS_DER1, 3, 3, ERM_MAT_DER1, &
     &                        3, 3, SRTM_2, IER )
           CALL MUL_MM_II_I ( 3, 3, TRS_TO_CRS,      3, 3, ERM_MAT_DER2, &
     &                        3, 3, SRTM_3, IER )

!
           CALL COPY_R8     ( 9, RTM_1, TRS_TO_CRS )
!
! -------- Compute the updated first and second derivatives of TRS_TO_CRS
!
           CALL MULTI_ADD_3 ( 2, TRS_TO_CRS_DER1, &
     &                           1.0D0, DRTM_1,   &
     &                           1.0D0, DRTM_2    )
           CALL MULTI_ADD_3 ( 3, TRS_TO_CRS_DER2, &
     &                           1.0D0, SRTM_1,   &
     &                           2.0D0, SRTM_2,   &
     &                           1.0D0, SRTM_3    )
!
           E3_ANG = E3_ANG + HEO_VEC(3) + ERM_VAL(3)
      END IF
!
      UT1_M_TAI = E3_ANG/UT1__TO__E3
      IF ( IVRB .GE. 4 ) THEN
           WRITE ( 6, * ) ' UT1_M_TAI  = ', UT1_M_TAI
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  VTD_AEM  !#!  
