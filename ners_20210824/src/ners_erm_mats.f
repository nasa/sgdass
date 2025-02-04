      SUBROUTINE NERS_ERM_MATS ( XPOL, YPOL, S_ANG, &
     &                    XPOL_RATE, YPOL_RATE, S_ANG_RATE, &
     &                    DZETA, TETA, ZA, EPS_0, DZETA_RATE, &
     &                    TETA_RATE, ZA_RATE, EPS_0_RATE, &
     &                    DPSI, DEPS, DPSI_RATE, DEPS_RATE, &
     &                    HEO_VEC, HEO_VEC_DER1, HEO_VEC_DER2, &
     &                    E1_GDS, E2_GDS, TRS_TO_CRS, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine NERS_ERM_MATS computes the matrix of transformation from   *
! *   the terrestrial coordinate system to the celestial coordinate      *
! *   system as well as its first and second time derivatives using      *
! *   Newcomb-Andoyer formalism. The routine uses angles computed by     *
! *   NERS_ERM_ANGS subprogram.                                          *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *       XPOL (  REAL*8    ) -- X pole coordinate at the moment MJD,TAI *
! *                              Unit: rad.
! *       YPOL (  REAL*8    ) -- R pole coordinate at the moment MJD,TAI *
! *                              Unit: rad.
! *      S_ANG ( REAL*8     ) -- E3 Euler rotation angle (archaic name:  *
! *                              true Greenwich stellar angle).          *
! *                              Units: rad.                             *
! *   XPOL_RATE ( REAL*8    ) -- Rate of change of X pole coordinate at  *
! *                              moment of time MJD,TAI, Units: rad/sec. *
! *   YPOL_RATE ( REAL*8    ) -- Rate of change of Y pole coordinate at  *
! *                              moment of time MJD,TAI, Units: rad/sec. *
! * S_ANG_RATE ( REAL*8     ) -- Rate of change of S_ANG. Units:         *
! *                              rad/sec.                                *
! * DZETA      ( REAL*8     ) -- Accumulated precession angle in right   *
! *                              ascension dzeta between epochs J2000.0  *
! *                              and MJD,TAI. Unit: rad.                 *
! * TETA       ( REAL*8     ) -- Accumulated precession angle in         *
! *                              declination teta between epochs J2000.0 *
! *                              and MJD,TAI. Unit: rad.                 *
! * ZA         ( REAL*8     ) -- Accumulated precession angle in right   *
! *                              ascension za between epochs J2000.0     *
! *                              and MJD,TAI. Unit: rad.                 *
! * EPS_0      ( REAL*8     ) -- Mean accumulated obliquiry angle eps_0  *
! *                              between epochs J2000.0 and MJD,TAI.     *
! *                              Unit: rad.                              *
! * DZETA_RATE ( REAL*8     ) -- First time derivative of angle dzeta    *
! *                              on epoch MJD,TAI. Unit rad/s.           *
! * TETA_RATE  ( REAL*8     ) -- First time derivative of angle teta     *
! *                              on epoch MJD,TAI. Unit rad/s.           *
! * ZA_RATE    ( REAL*8     ) -- First time derivative of angle za       *
! *                              on epoch MJD,TAI. Unit rad/s.           *
! * EPS_0_RATE ( REAL*8     ) -- First time derivative of angle eps_0    *
! *                              on epoch MJD,TAI. Unit rad/s.           *
! * DPSI        ( REAL*8    ) -- Nutation in longitude on epoch MJD,TAI. *
! *                              Unit: rad.                              *
! * DEPS        ( REAL*8    ) -- Nutation in obliquity on epoch MJD,TAI. *
! *                              Unit: rad.                              *
! * DPSI_RATE   ( REAL*8    ) -- Time derivative of DPSI angle on        *
! *                              epoch MJD,TAI. Unit: rad/s.             *
! * DEPS_RATE   ( REAL*8    ) -- Time derivative of DEPS angle on        *
! *                              epoch MJD,TAI. Unit: rad/s.             *
! * HEO_VEC    ( REAL*8     ) -- The vector of the perturbation Earth    *
! *                              rotation due to harmonic variations.    *
! *                              Dimension: 3. Units: radians.           *
! *                              Components: E1, E2, E3.                 *
! * HEO_VEC_DER1 ( REAL*8   ) -- The first time derivative of the        *
! *                              vector of the perturbation Earth        *
! *                              rotation due to harmonic variations.    *
! *                              Dimension: 3. Units: rad/sec.           *
! *                              Components: E1, E2, E3.                 *
! * HEO_VEC_DER2 ( REAL*8   ) -- The second time derivative of the       *
! *                              vector of the perturbation Earth        *
! *                              rotation due to harmonic variations.    *
! *                              Dimension: 3. Units: rad/sec^2.         *
! *                              Components: E1, E2, E3.                 *
! * E1_GDS   ( REAL*8       ) -- Euler angle of the geodesic nutation    *
! *                              along the axis 1. Unit: rad.            *
! * E2_GDS   ( REAL*8       ) -- Euler angle of the geodesic nutation    *
! *                              along the axis 2. Unit: rad.            *
! *                                                                      *
! * _________________________ Output parameters: _______________________ *
! *                                                                      *
! * TRS_TO_CRS      ( REAL*8    ) -- The matrix of transformation from   *
! *                                  the terrestrial reference system to *
! *                                  the celestial reference system at   *
! *                                  the moment of time MJD,TAI and its  *
! *                                  the frst and second time derivative.*
! *                                  Dimension: 3,3,0:2.                 *
! *                                                                      *
! * _________________________ Modified parameters: _____________________ *
! *                                                                      *
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
! *  ### 02-JAN-2018  NERS_ERM_MATS  v1.0 (c) L. Petrov 03-JAN-2018 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'astro_constants.i'
      INCLUDE   'heo.i'
      INCLUDE   'eec.i'
      INTEGER*4  IUER
      REAL*8     TRS_TO_CRS(3,3,0:2)
      REAL*8     XPOL, YPOL, S_ANG, DZETA, TETA, ZA, EPS_0, DPSI, DEPS, &
     &           DPSI_RATE, DEPS_RATE, &
     &           DZETA_RATE, TETA_RATE, ZA_RATE, EPS_0_RATE, OMEGA_RATE, &
     &           XPOL_RATE, YPOL_RATE, S_ANG_RATE, E1_GDS, E2_GDS, &
     &           DPSI_GDS, DEPS_GDS
      REAL*8     RTM_YWOB(3,3),  RTM_XWOB(3,3),  RTM_DIU(3,3),    &
     &           RTM_N1(3,3),    RTM_N2(3,3),    RTM_N3(3,3),     &
     &           RTM_P1(3,3),    RTM_P2(3,3),    RTM_P3(3,3),     &
     &           RTM_TMP(3,3),   RTM_E1(3,3),    RTM_E2(3,3),     &
     &           DRTM_YWOB(3,3), DRTM_XWOB(3,3), DRTM_DIU(3,3),   &
     &           DRTM_N1(3,3),   DRTM_N2(3,3),   DRTM_N3(3,3),    &
     &           DRTM_P1(3,3),   DRTM_P2(3,3),   DRTM_P3(3,3),    &
     &           DRTM_1(3,3),    DRTM_2(3,3),    DRTM_3(3,3),     &
     &           DRTM_4(3,3),    DRTM_5(3,3),    DRTM_6(3,3),     &
     &           DRTM_7(3,3),    DRTM_8(3,3),    DRTM_9(3,3),     &
     &           SRTM_DIU(3,3),  SRTM_1(3,3),    SRTM_2(3,3),     &
     &           SRTM_3(3,3),    SRTM_4(3,3),    SRTM_5(3,3),     &
     &           SRTM_6(3,3),    SRTM_7(3,3),    SRTM_8(3,3)
      REAL*8     PRTM_DIU(3,3),  PRTM_XWOB(3,3), PRTM_YWOB(3,3)
      REAL*8     RTM_1(3,3)
      REAL*8     HEO_VEC(3),   HEO_VEC_DER1(3),   HEO_VEC_DER2(3)
      REAL*8     HEO_MAT(3,3), HEO_MAT_DER1(3,3), HEO_MAT_DER2(3,3)
      INTEGER*4  J1, J2, IER
!
       CALL  NOUT_R8 ( 27, TRS_TO_CRS )
!
       DPSI_GDS = E2_GDS/DSIN(EPS_0)
       DEPS_GDS = E1_GDS
!
! ---- Compute rotation matrices
!
       CALL NERS_ROTMAT ( 3,  DZETA,                   RTM_P1   )
       CALL NERS_ROTMAT ( 2, -TETA,                    RTM_P2   )
       CALL NERS_ROTMAT ( 3,  ZA,                      RTM_P3   )
       CALL NERS_ROTMAT ( 1, -EPS_0,                   RTM_N1   )
       CALL NERS_ROTMAT ( 3,  DPSI + DPSI_GDS,         RTM_N2   )
       CALL NERS_ROTMAT ( 1,  EPS_0 + DEPS + DEPS_GDS, RTM_N3   )
       CALL NERS_ROTMAT ( 3, -S_ANG,                   RTM_DIU  )
       CALL NERS_ROTMAT ( 2,  XPOL,                    RTM_XWOB )
       CALL NERS_ROTMAT ( 1,  YPOL,                    RTM_YWOB )
!
! ---- Now we compute the product of 9 matrices
!
       CALL MULTI_MUL_3 (  9, TRS_TO_CRS(1,1,0), RTM_P1,  RTM_P2,   RTM_P3,  &
     &                        RTM_N1,  RTM_N2,   RTM_N3,  &
     &                        RTM_DIU, RTM_XWOB, RTM_YWOB )
!
! ---- Compute derivatives of the rotation matrices
!
       CALL NERS_ROTMAT_DER ( 3,  DZETA,        DZETA_RATE, DRTM_P1   )
       CALL NERS_ROTMAT_DER ( 2, -TETA,         TETA_RATE,  DRTM_P2   )
       CALL NERS_ROTMAT_DER ( 3,  ZA,           ZA_RATE,    DRTM_P3   )
       CALL NERS_ROTMAT_DER ( 1, -EPS_0,        EPS_0_RATE, DRTM_N1   )
       CALL NERS_ROTMAT_DER ( 3,  DPSI,         DPSI_RATE,  DRTM_N2   )
       CALL NERS_ROTMAT_DER ( 1,  EPS_0 + DEPS, EPS_0_RATE  + DEPS_RATE, &
     &                                                        DRTM_N3   )
!
       CALL NERS_ROTMAT_DER ( 3, -S_ANG,        1.0D0, PRTM_DIU  )
       CALL NERS_ROTMAT_DER ( 2,  XPOL,         1.0D0, PRTM_XWOB )
       CALL NERS_ROTMAT_DER ( 1,  YPOL,         1.0D0, PRTM_YWOB )
!
       CALL VEC_MULT_CONSTANT ( PRTM_DIU,  9, S_ANG_RATE, DRTM_DIU  )
       CALL VEC_MULT_CONSTANT ( PRTM_XWOB, 9, XPOL_RATE,  DRTM_XWOB )
       CALL VEC_MULT_CONSTANT ( PRTM_YWOB, 9, YPOL_RATE,  DRTM_YWOB )
!
! ---- Now compute first time derivative of TRS_TO_CRS. It is done in
! ---- two steps: first compute 9 terms, each being a product of nine
! ---- matrices, 8 rotation matrices and the 9-th first derivative
!
       CALL MULTI_MUL_3 ( 9, DRTM_1, DRTM_P1,  RTM_P2,  RTM_P3, &
     &       RTM_N1,  RTM_N2,  RTM_N3,  RTM_DIU,  RTM_XWOB,  RTM_YWOB )
       CALL MULTI_MUL_3 ( 9, DRTM_2,  RTM_P1, DRTM_P2,  RTM_P3, &
     &       RTM_N1,  RTM_N2,  RTM_N3,  RTM_DIU,  RTM_XWOB,  RTM_YWOB )
       CALL MULTI_MUL_3 ( 9, DRTM_3,  RTM_P1,  RTM_P2, DRTM_P3, &
     &       RTM_N1,  RTM_N2,  RTM_N3,  RTM_DIU,  RTM_XWOB,  RTM_YWOB )
       CALL MULTI_MUL_3 ( 9, DRTM_4,  RTM_P1,  RTM_P2, RTM_P3, &
     &      DRTM_N1,  RTM_N2,  RTM_N3,  RTM_DIU,  RTM_XWOB,  RTM_YWOB )
       CALL MULTI_MUL_3 ( 9, DRTM_5,  RTM_P1,  RTM_P2,  RTM_P3, &
     &       RTM_N1, DRTM_N2,  RTM_N3,  RTM_DIU,  RTM_XWOB,  RTM_YWOB )
       CALL MULTI_MUL_3 ( 9, DRTM_6,  RTM_P1,  RTM_P2,  RTM_P3, &
     &       RTM_N1,  RTM_N2, DRTM_N3,  RTM_DIU,  RTM_XWOB,  RTM_YWOB )
       CALL MULTI_MUL_3 ( 9, DRTM_7,  RTM_P1,  RTM_P2,  RTM_P3, &
     &       RTM_N1,  RTM_N2,  RTM_N3, DRTM_DIU,  RTM_XWOB,  RTM_YWOB )
       CALL MULTI_MUL_3 ( 9, DRTM_8,  RTM_P1,  RTM_P2,  RTM_P3, &
     &       RTM_N1,  RTM_N2,  RTM_N3,  RTM_DIU, DRTM_XWOB,  RTM_YWOB )
       CALL MULTI_MUL_3 ( 9, DRTM_9,  RTM_P1,  RTM_P2,  RTM_P3, &
     &       RTM_N1,  RTM_N2,  RTM_N3,  RTM_DIU,  RTM_XWOB, DRTM_YWOB )
!
! --- ... and at the second step we summ up the terms
!
       CALL MULTI_ADD_3 ( 9, TRS_TO_CRS(1,1,1), &
     &                     1.0D0, DRTM_1, &
     &                    -1.0D0, DRTM_2, &
     &                     1.0D0, DRTM_3, &
     &                    -1.0D0, DRTM_4, &
     &                     1.0D0, DRTM_5, &
     &                     1.0D0, DRTM_6, &
     &                    -1.0D0, DRTM_7, &
     &                     1.0D0, DRTM_8, &
     &                     1.0D0, DRTM_9  )
!
! -------- Compute the second derivative of the rotation matrix. Only one
! -------- term is taken into account. Relative error of the expression below
! -------- is only 1.D-5
!
       CALL NERS_ROTMAT_DER2 ( 3, -S_ANG, S_ANG_RATE, 0.0D0, SRTM_DIU  )
!
       CALL MULTI_MUL_3 ( 9, SRTM_1,  RTM_P1,  RTM_P2,  RTM_P3, &
     &       RTM_N1,  RTM_N2,  RTM_N3, SRTM_DIU,  RTM_XWOB,  RTM_YWOB )
       CALL MULTI_MUL_3 ( 9, SRTM_2, DRTM_P1, RTM_P2, RTM_P3, &
     &       RTM_N1,  RTM_N2,  RTM_N3, DRTM_DIU,  RTM_XWOB,  RTM_YWOB )
       CALL MULTI_MUL_3 ( 9, SRTM_3, RTM_P1, DRTM_P2, RTM_P3, &
     &       RTM_N1,  RTM_N2,  RTM_N3, DRTM_DIU,  RTM_XWOB,  RTM_YWOB )
       CALL MULTI_MUL_3 ( 9, SRTM_4, RTM_P1, RTM_P2, DRTM_P3, &
     &       RTM_N1,  RTM_N2,  RTM_N3, DRTM_DIU,  RTM_XWOB,  RTM_YWOB )
       CALL MULTI_MUL_3 ( 9, SRTM_5, RTM_P1, RTM_P2, RTM_P3, &
     &       RTM_N1, DRTM_N2,  RTM_N3, DRTM_DIU,  RTM_XWOB,  RTM_YWOB )
       CALL MULTI_MUL_3 ( 9, SRTM_6, RTM_P1, RTM_P2, RTM_P3, &
     &       RTM_N1,  RTM_N2, DRTM_N3, DRTM_DIU,  RTM_XWOB,  RTM_YWOB )
       CALL MULTI_MUL_3 ( 9, SRTM_7, RTM_P1, RTM_P2, RTM_P3, &
     &       RTM_N1,  RTM_N2,  RTM_N3, DRTM_DIU, DRTM_XWOB,  RTM_YWOB )
       CALL MULTI_MUL_3 ( 9, SRTM_8, RTM_P1, RTM_P2, RTM_P3, &
     &       RTM_N1,  RTM_N2,  RTM_N3, DRTM_DIU,  RTM_XWOB, DRTM_YWOB )
!
       CALL MULTI_ADD_3 ( 8, TRS_TO_CRS(1,1,2), &
     &                       1.0D0, SRTM_1, &
     &                      -2.0D0, SRTM_2, &
     &                       2.0D0, SRTM_3, &
     &                      -2.0D0, SRTM_4, &
     &                      -2.0D0, SRTM_5, &
     &                      -2.0D0, SRTM_6, &
     &                      -2.0D0, SRTM_7, &
     &                      -2.0D0, SRTM_8  )
!
! ---- Build the rotational matrix and its time derivatives
!
       HEO_MAT(1,1) =  1.0D0
       HEO_MAT(2,1) = -HEO_VEC(3)
       HEO_MAT(3,1) =  HEO_VEC(2)
       HEO_MAT(1,2) =  HEO_VEC(3)
       HEO_MAT(2,2) =  1.0D0
       HEO_MAT(3,2) = -HEO_VEC(1)
       HEO_MAT(1,3) = -HEO_VEC(2)
       HEO_MAT(2,3) =  HEO_VEC(1)
       HEO_MAT(3,3) =  1.0D0
!
       HEO_MAT_DER1(1,1) =  0.0D0
       HEO_MAT_DER1(2,1) = -HEO_VEC_DER1(3)
       HEO_MAT_DER1(3,1) =  HEO_VEC_DER1(2)
       HEO_MAT_DER1(1,2) =  HEO_VEC_DER1(3)
       HEO_MAT_DER1(2,2) =  0.0D0
       HEO_MAT_DER1(3,2) = -HEO_VEC_DER1(1)
       HEO_MAT_DER1(1,3) = -HEO_VEC_DER1(2)
       HEO_MAT_DER1(2,3) =  HEO_VEC_DER1(1)
       HEO_MAT_DER1(3,3) =  0.0D0
!
       HEO_MAT_DER2(1,1) =  0.0D0
       HEO_MAT_DER2(2,1) = -HEO_VEC_DER2(3)
       HEO_MAT_DER2(3,1) =  HEO_VEC_DER2(2)
       HEO_MAT_DER2(1,2) =  HEO_VEC_DER2(3)
       HEO_MAT_DER2(2,2) =  0.0D0
       HEO_MAT_DER2(3,2) = -HEO_VEC_DER2(1)
       HEO_MAT_DER2(1,3) = -HEO_VEC_DER2(2)
       HEO_MAT_DER2(2,3) =  HEO_VEC_DER2(1)
       HEO_MAT_DER2(3,3) =  0.0D0
!
! ---- Multiply TRS_TO_CRS and HEO matrix
!
       CALL MUL_MM_II_I ( 3, 3, TRS_TO_CRS(1,1,0), 3, 3, HEO_MAT, 3, 3, RTM_1, IER )
!
! ---- Compute intermediate matrices for the first derivatives
!
       CALL MUL_MM_II_I ( 3, 3, TRS_TO_CRS(1,1,1), 3, 3, HEO_MAT, &
     &                    3, 3, DRTM_1, IER )
       CALL MUL_MM_II_I ( 3, 3, TRS_TO_CRS(1,1,0), 3, 3, HEO_MAT_DER1, &
     &                    3, 3, DRTM_2, IER )
!
! ---- ... and second derivative
!
       CALL MUL_MM_II_I ( 3, 3, TRS_TO_CRS(1,1,2), 3, 3, HEO_MAT, &
     &                    3, 3, SRTM_1, IER )
       CALL MUL_MM_II_I ( 3, 3, TRS_TO_CRS(1,1,1), 3, 3, HEO_MAT_DER1, &
     &                    3, 3, SRTM_2, IER )
       CALL MUL_MM_II_I ( 3, 3, TRS_TO_CRS(1,1,0), 3, 3, HEO_MAT_DER2, &
     &                    3, 3, SRTM_3, IER )
!
       CALL COPY_R8     ( 9, RTM_1, TRS_TO_CRS(1,1,0) )
!
! ---- Compute the updated first and second derivatives of TRS_TO_CRS
!
       CALL MULTI_ADD_3 ( 2, TRS_TO_CRS(1,1,1), &
     &                       1.0D0, DRTM_1,     &
     &                       1.0D0, DRTM_2      )
       CALL MULTI_ADD_3 ( 3, TRS_TO_CRS(1,1,2), &
     &                       1.0D0, SRTM_1,     &
     &                       2.0D0, SRTM_2,     &
     &                       1.0D0, SRTM_3      )
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  NERS_ERM_MATS  !#!#
