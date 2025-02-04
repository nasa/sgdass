      SUBROUTINE VTD_PK2001 ( VTD, ISTA1, ISTA2, ISOU, &
     &                        TRS_TO_CRS, TRS_TO_CRS_DER1, &
     &                        DTRS_TO_CRS_DEOP, DTRS_TO_CRS_DER1_DEOP, &
     &                        NZO_RLT, TAU_GEOM, RATE_GEOM, ACLR_GEOM, &
     &                        TAU_DER_EOP, TAU_DER_STA1, TAU_DER_STA2, &
     &                        TAU_DER_RA, TAU_DER_DL, TAU_DER_GAMMA, &
     &                        RATE_DER_EOP, RATE_DER_STA1, RATE_DER_STA2, &
     &                        RATE_DER_RA, RATE_DER_DL, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine VTD_PK2001  computes VLBI time delay, its time derivative  *
! *   and derivatives with respect to some parameters according to an    *
! *   algorithm presented in memo Petrov and Kopeikin 2001. Precision of *
! *   the expression is better than 5.D-13 seconds. It is assumed that   *
! *   intermediate quantities, such as instantaneous site positions,     *
! *   coordinates of celestial bodies etc. have been computed beforehand *
! *   and stored in the internal fields of the object VTD.               *
! *                                                                      *
! *   VLBI Time delay is defined as the difference of two intervals of   *
! *   proper time: 1) the interval of proper time of station #2 between  *
! *   events: coming the wave front to the reference point on the moving *
! *   axis and clock synchronization; 2) the interval of proper time of  *
! *   station #1 between events: coming the wave front to the reference  *
! *   point on the moving axis and clock synchronization. The time delay *
! *   is referred to the moment of coming the wave front to the          *
! *   reference point on the moving axis of the first antenna at time    *
! *   measured by the timescale TAI. The reference point of the station  *
! *   for which modeling is done is defined as the point on the moving   *
! *   axis which has the minimal distance to the fixed axis. In the      *
! *   case if axes intersect, this is the point of intersection.         *
! *                                                                      *
! *   References:                                                        *
! *   1) S.M. Kopeikin, G. Schaefer, Lorenz covariant theory of light    *
! *      propagation in gravitational field of arbitrary-moving bodies,  *
! *      Physical Review D, vol. 60, 12402, 1999.                        *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *     ISTA1 ( INTEGER*4 ) -- Index in the list of station of the first *
! *                            station of the baseline.                  *
! *     ISTA2 ( INTEGER*4 ) -- Index in the list of station of the       *
! *                            second station of the baseline.           *
! *      ISOU ( INTEGER*4 ) -- Index in the list of sources of the       *
! *                            source under consideration.               *
! * TRS_TO_CRS      ( REAL*8    ) -- The matrix of transformation from   *
! *                                  the terrestrial reference system to *
! *                                  the celestial reference system at   *
! *                                  the moment of time MJD,TAI.         *
! *                                  Dimension: 3,3.                     *
! * TRS_TO_CRS_DER1 ( REAL*8    ) -- The time derivative of the matrix   *
! *                                  of transformation from              *
! *                                  the terrestrial reference system to *
! *                                  the celestial reference system at   *
! *                                  the moment of time MJD,TAI.         *
! *                                  Dimension: 3,3.                     *
! * DTRS_TO_CRS_DEOP ( REAL*8   ) -- Array of time derivatives of the    *
! *                                  transformation matrix from the      *
! *                                  terrestrial reference system to     *
! *                                  the celestial reference system at   *
! *                                  the moment of time MJD,TAI with     *
! *                                  respect to the small vector of      *
! *                                  rotation with Euler angles E1, E2,  *
! *                                  and E3. Dimension: 3,3,3. The last  *
! *                                  dimension runs over E1, E2 and E3   *
! *                                  Euler angles.                       *
! * DTRS_TO_CRS_DER1_DEOP ( REAL*8   ) -- Array of mixed derivatives     *
! *                                  of the transformation matrix from   *
! *                                  the terrestrial reference system to *
! *                                  the celestial reference system at   *
! *                                  the moment of time MJD,TAI with     *
! *                                  respect to time and the small       *
! *                                  vector of rotation with Euler       *
! *                                  angles E1, E2, and E3. Dimension:   *
! *                                  3,3,3. The last dimension runs over *
! *                                  E1, E2, and E3 Euler angles.        *
! *         NZO_RLT  ( REAL*8   ) --                                     *
! *                                                                      *
! * _________________________ Output parameters: _______________________ *
! *                                                                      *
! *  TAU_GEOM ( REAL*8    ) -- Geometric delay. Units: seconds.          *
! * RATE_GEOM ( REAL*8    ) -- First time derivative of geometric time   *
! *                            delay. Units: dimensionless.              *
! * ACLR_GEOM ( REAL*8    ) -- Second time derivative of geometric       *
! *                            time delay. Units: 1/seconds.             *
! *                            NB: accuracy of ACLR_GEOM is low: 10^{-6} *
! * TAU_DER_EOP  ( REAL*8    ) -- Partial derivative of time delay with  *
! *                               respect to the vector of a small       *
! *                               perturbing Earth's rotation.           *
! *                               Dimension: 3. Indexes run over Euler   *
! *                               angles of the Earth orientation.       *
! *                               Units: sec.                            *
! * TAU_DER_STA1 ( REAL*8    ) -- Partial derivative of time delay with  *
! *                               respect to coordinates vector of       *
! *                               station #1. Units: sec/m.              *
! * TAU_DER_STA2 ( REAL*8    ) -- Partial derivative of time delay with  *
! *                               respect to coordinates vector of       *
! *                               station #2. Units: sec/m.              *
! *   TAU_DER_RA ( REAL*8    ) -- Partial derivative of time delay with  *
! *                               respect to right ascension of the      *
! *                               observed source. Units: sec.           *
! *   TAU_DER_RA ( REAL*8    ) -- Partial derivative of time delay with  *
! *                               respect to declination of the observed *
! *                               source. Units: sec.                    *
! * TAU_DER_GAMMA ( REAL*8   ) -- Partial derivative of group delay over *
! *                               the Parametersized Post-Newtonian      *
! *                               formalism parameter gamma.             *
! * RAT_DER_EOP  ( REAL*8    ) -- Partial derivative of delay rate with  *
! *                               respect to the vector of a small       *
! *                               perturbing Earth's rotation.           *
! *                               Dimension: 3. Indexes run over Euler   *
! *                               angles of the Earth orientation.       *
! *                               Units: dimensionless.                  *
! * RATE_DER_STA1 ( REAL*8   ) -- Partial derivative of delay rate with  *
! *                               respect to coordinates vector of       *
! *                               station #1. Units: 1/m.                *
! * RATE_DER_STA2 ( REAL*8   ) -- Partial derivative of delay rate with  *
! *                               respect to coordinates vector of       *
! *                               station #2. Units: 1/m.                *
! *  RATE_DER_RA ( REAL*8    ) -- Partial derivative of delay rate with  *
! *                               respect to right ascension of the      *
! *                               observed source. Units: dimensionless. *
! *  RATE_DER_RA ( REAL*8    ) -- Partial derivative of delay rate with  *
! *                               respect to declination of the observed *
! *                               source. Units: dimensionless.          *
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
! *  ### 29-JAN-2004   VTD_PK2001  v6.5 (c)  L. Petrov  13-MAR-2023 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'vtd.i'
      INCLUDE   'astro_constants.i'
      INTEGER*4  ISTA1, ISTA2, ISOU, IUER
      REAL*8     TRS_TO_CRS(3,3), TRS_TO_CRS_DER1(3,3), &
     &           DTRS_TO_CRS_DEOP(3,3,3), DTRS_TO_CRS_DER1_DEOP(3,3,3), &
     &           NZO_RLT, TAU_GEOM, RATE_GEOM, ACLR_GEOM, TAU_DER_EOP(3), &
     &           TAU_DER_STA1(3), TAU_DER_STA2(3), &
     &           TAU_DER_RA,TAU_DER_DL, &
     &           RATE_DER_EOP(3), RATE_DER_STA1(3), RATE_DER_STA2(3), &
     &           RATE_DER_RA, RATE_DER_DL, TAU_DER_GAMMA
      TYPE     ( VTD__TYPE ) :: VTD
      INTEGER*4  J1, J2, IER
      REAL*8     R1A(3), R2A(3), V1A(3), V2A(3), &
     &           R1A_NOR(3), R2A_NOR(3), R1A_LEN, R2A_LEN, &
     &           TAU_GRAV, TRS_METRIC, B_CRS(3), V_CRS(3), A_CRS(3), &
     &           COO1_BRS(3), COO2_BRS(3), VEL1_BRS(3), VEL2_BRS(3), &
     &           COO_PLAN(3), VEL_PLAN(3),  ACC_PLAN(3), &
     &           DIST_SUN_EARTH, TAU_RETR1, TAU_RETR2, RAT_GRAV, &
     &           DS_DALPHA(3), DS_DDELTA(3), C_FACTOR, B_TRS(3), &
     &           VEC_TMP1(3), VEC_TMP2(3), RDIST, TIM_SRC
      REAL*8,    EXTERNAL :: DP_VV_V
!
      TAU_GRAV = 0.0D0
      RAT_GRAV = 0.0D0
      DO 410 J1=1,VTD__M_PLA
!
! ------ Compute COO1_BRS and COO2_BRS -- baricentric vectors of the
! ------ the first and the second station at the time of arrival of
! ------ the photon at the first station
!
         CALL ADD_VV_V ( 3, VTD%STA(ISTA1)%COO_CRS, &
     &                      VTD%MOM%PLAN(1,VTD__COO,VTD__EART), COO1_BRS )
         CALL ADD_VV_V ( 3, VTD%STA(ISTA2)%COO_CRS, &
     &                      VTD%MOM%PLAN(1,VTD__COO,VTD__EART), COO2_BRS )
!
! ------ Get coordinates of the planet at the moment of time of arrival of
! ------ the photon at the station
!
         CALL COPY_R8  ( 3, VTD%MOM%PLAN(1,VTD__COO,J1), COO_PLAN )
         CALL COPY_R8  ( 3, VTD%MOM%PLAN(1,VTD__VEL,J1), VEL_PLAN )
!
! ------ Solve light cone equation by iteration
!
         CALL SUB_VV_V ( 3, COO1_BRS, COO_PLAN, R1A )
         CALL SUB_VV_V ( 3, COO2_BRS, COO_PLAN, R2A )
         CALL COPY_R8  ( 3, R1A, R1A_NOR )
         CALL COPY_R8  ( 3, R2A, R2A_NOR )
         CALL NORM_VEC ( 3, R1A_NOR, R1A_LEN )
         CALL NORM_VEC ( 3, R2A_NOR, R2A_LEN )
         TAU_RETR1 = -R1A_LEN/VTD__C ! Get time retardation
         TAU_RETR2 = -R2A_LEN/VTD__C
!
! ------ Compute position of the gravitating body at retarded moment of time
!
         CALL ADDC_VV  ( 3, 1.0D0, VTD%MOM%PLAN(1,VTD__COO,J1), TAU_RETR1, &
     &                   VEL_PLAN, COO_PLAN )
         CALL SUB_VV_V ( 3, COO1_BRS, COO_PLAN, R1A )
         CALL COPY_R8  ( 3, R1A,     R1A_NOR )
         CALL NORM_VEC ( 3, R1A_NOR, R1A_LEN )
!
         CALL ADDC_VV  ( 3, 1.0D0, VTD%MOM%PLAN(1,VTD__COO,J1), TAU_RETR2, &
     &                   VEL_PLAN, COO_PLAN )
         CALL SUB_VV_V ( 3, COO2_BRS, COO_PLAN, R2A )
         CALL COPY_R8  ( 3, R2A,     R2A_NOR )
         CALL NORM_VEC ( 3, R2A_NOR, R2A_LEN )
!
! ------ Special cases of station "GEOCENTR"
!
         IF ( R1A_LEN < VTD__REA/2.0D0 ) THEN
              R1A     = VTD%STA(ISTA1)%COO_CRS
              CALL COPY_R8  ( 3, R1A, R1A_NOR )
              CALL NORM_VEC ( 3, R1A_NOR, R1A_LEN )
              R1A_LEN = VTD__REA
         END IF
         IF ( R2A_LEN < VTD__REA/2.0D0 ) THEN
              R2A     = VTD%STA(ISTA2)%COO_CRS
              CALL COPY_R8  ( 3, R2A, R2A_NOR )
              CALL NORM_VEC ( 3, R2A_NOR, R2A_LEN )
              R2A_LEN = VTD__REA
         END IF
!
         TAU_GRAV = TAU_GRAV + &
     &           2.D0*VTD__GM(J1)/VTD__C**3*(1.0D0 + &
     &           DP_VV_V ( 3, VTD%SOU(ISOU)%S_CRS, VTD%MOM%PLAN(1,VTD__VEL,J1) &
     &                   )/ VTD__C)* &
     &      DLOG ( (R1A_LEN*(1.0D0 + DP_VV_V ( 3, VTD%SOU(ISOU)%S_CRS, R1A_NOR)))/ &
     &             (R2A_LEN*(1.0D0 + DP_VV_V ( 3, VTD%SOU(ISOU)%S_CRS, R2A_NOR)))  )
         IF ( VTD%CONF%FL_RATE ) THEN
              CALL COPY_R8  ( 3, VTD%MOM%PLAN(1,VTD__ACC,J1), ACC_PLAN )
!
! ----------- Compute VEL1_BRS and VEL2_BRS -- baricentric vectors of the
! ----------- the first and the second station at the time of arrival of
! ----------- the photon at the first station
!
              CALL ADD_VV_V ( 3, VTD%STA(ISTA1)%VEL_CRS, &
     &                        VTD%MOM%PLAN(1,VTD__VEL,VTD__EART), VEL1_BRS )
              CALL ADD_VV_V ( 3, VTD%STA(ISTA2)%VEL_CRS, &
     &                        VTD%MOM%PLAN(1,VTD__VEL,VTD__EART), VEL2_BRS )
!
! ----------- Compute velocity of the gravitating body at retarded
! ----------- moment of time
!
              CALL ADDC_VV  ( 3, 1.0D0, VTD%MOM%PLAN(1,VTD__VEL,J1), &
     &                        TAU_RETR1, ACC_PLAN, VEL_PLAN )
              CALL SUB_VV_V ( 3, VEL1_BRS, VEL_PLAN, V1A )
!
              CALL ADDC_VV  ( 3, 1.0D0, VTD%MOM%PLAN(1,VTD__VEL,J1), &
     &                        TAU_RETR2, ACC_PLAN, VEL_PLAN )
              CALL SUB_VV_V ( 3, VEL2_BRS, VEL_PLAN, V2A )
!
              RAT_GRAV = RAT_GRAV + &
     &          2.D0*VTD__GM(J1)/VTD__C**3* &
     &          ( DP_VV_V ( 3, VTD%SOU(ISOU)%S_CRS, VTD%MOM%PLAN(1,VTD__ACC,J1) &
     &                    )/ VTD__C * &
     &            DLOG ( (R1A_LEN*(1.0D0 + DP_VV_V( 3, VTD%SOU(ISOU)%S_CRS, R1A_NOR)))/ &
     &                   (R2A_LEN*(1.0D0 + DP_VV_V( 3, VTD%SOU(ISOU)%S_CRS, R2A_NOR))) &
     &                 ) &
     &            + &
#define comment here
     &            (1.0D0 + DP_VV_V ( 3, VEL_PLAN, VTD%SOU(ISOU)%S_CRS )/ VTD__C &
     &            )* &
     &            ( (   DP_VV_V ( 3, V1A, VTD%SOU(ISOU)%S_CRS ) &
     &                + (  DP_VV_V ( 3, COO1_BRS, VEL1_BRS )    &
     &                   - DP_VV_V ( 3, COO1_BRS, VEL_PLAN )    &
     &                   - DP_VV_V ( 3, VEL1_BRS, COO_PLAN )    &
     &                   + DP_VV_V ( 3, COO_PLAN, VEL_PLAN )    &
     &                  )/R1A_LEN &
     &              )/(R1A_LEN + DP_VV_V( 3, R1A, VTD%SOU(ISOU)%S_CRS ) ) &
     &              - &
     &              ( &
     &                  DP_VV_V ( 3, V2A, VTD%SOU(ISOU)%S_CRS ) &
     &                + (  DP_VV_V ( 3, COO2_BRS, VEL2_BRS )    &
     &                   - DP_VV_V ( 3, COO2_BRS, VEL_PLAN )    &
     &                   - DP_VV_V ( 3, VEL2_BRS, COO_PLAN )    &
     &                   + DP_VV_V ( 3, COO_PLAN, VEL_PLAN )    &
     &                  )/R2A_LEN &
     &              )/(R2A_LEN + DP_VV_V( 3, R2A, VTD%SOU(ISOU)%S_CRS ) ) &
     &            ) &
     &          )
         END IF
 410  CONTINUE
!
      CALL SUB_VV_V ( 3, VTD%STA(ISTA2)%COO_CRS, VTD%STA(ISTA1)%COO_CRS, B_CRS )
      CALL SUB_VV_V ( 3, VTD%STA(ISTA2)%VEL_CRS, VTD%STA(ISTA1)%VEL_CRS, V_CRS )
      CALL SUB_VV_V ( 3, VTD%STA(ISTA2)%ACC_CRS, VTD%STA(ISTA1)%ACC_CRS, A_CRS )
      DIST_SUN_EARTH = DSQRT (   &
     &    (VTD%MOM%PLAN(1,VTD__COO,VTD__EART)-VTD%MOM%PLAN(1,VTD__COO,VTD__SUN))**2 &
     &  + (VTD%MOM%PLAN(2,VTD__COO,VTD__EART)-VTD%MOM%PLAN(2,VTD__COO,VTD__SUN))**2 &
     &  + (VTD%MOM%PLAN(3,VTD__COO,VTD__EART)-VTD%MOM%PLAN(3,VTD__COO,VTD__SUN))**2 &
     &                       )
      IF ( VTD%CONF%GRS_METRIC == VTD__METRIC_ITRF1992 ) THEN
           TRS_METRIC = 0.0D0
         ELSE IF ( VTD%CONF%GRS_METRIC == VTD__METRIC_ITRF1996 ) THEN
           TRS_METRIC = 2.0D0
         ELSE IF ( VTD%CONF%GRS_METRIC == VTD__METRIC_ITRF2000 ) THEN
           TRS_METRIC = 0.0D0
         ELSE IF ( VTD%CONF%GRS_METRIC == VTD__METRIC_IAU2000 ) THEN
           TRS_METRIC = 1.0D0
         ELSE
           CALL ERR_LOG ( 2911, IUER, 'VTD_PK2001', 'Unknown code of the '// &
     &         'GRS metric' )
           RETURN
      END IF
!
! --- Compute geometric path delay
!
      TIM_SRC = (VTD%MOM%MJD - VTD%SOU(ISOU)%MJD_REF)*86400.0D0 + &
     &          (VTD%MOM%TAI - VTD%SOU(ISOU)%TAI_REF)
      TAU_GEOM = ( -DP_VV_V ( 3, B_CRS, VTD%SOU(ISOU)%S_CRS )/VTD__C * &
     &         ( 1.0D0                                                 &
     &          -2.0D0*VTD__GM(VTD__SUN)/DIST_SUN_EARTH/VTD__C**2 &
     &          -TRS_METRIC*VTD__GM(VTD__EART)/VTD__REA/VTD__C**2 &
     &          -DP_VV_V ( 3, VTD%MOM%PLAN(1,VTD__VEL,VTD__EART), &
     &                        VTD%MOM%PLAN(1,VTD__VEL,VTD__EART)  )/2.0D0/VTD__C**2 &
     &          -DP_VV_V ( 3, VTD%MOM%PLAN(1,VTD__VEL,VTD__EART), &
     &                        VTD%STA(ISTA2)%VEL_CRS )/VTD__C**2  &
     &         )                                                  &
     &       - DP_VV_V ( 3, VTD%MOM%PLAN(1,VTD__VEL,VTD__EART), B_CRS )*    &
     &            (1.0D0 + DP_VV_V ( 3, VTD%MOM%PLAN(1,VTD__VEL,VTD__EART), &
     &                                  VTD%SOU(ISOU)%S_CRS)/2.0D0/VTD__C   &
     &                             )/ VTD__C**2                             &
     &       + VTD%SOU(ISOU)%PRLX/DIST_SUN_EARTH/VTD__C*                    &
     &             DP_VV_V ( 3, VTD%MOM%PLAN(1,VTD__COO,VTD__EART), B_CRS ) &
     &       - TIM_SRC * DP_VV_V ( 3, B_CRS, VTD%SOU(ISOU)%S_CRS_RATE )/VTD__C &
     &       + TAU_GRAV )/                                                  &
     &       (1.0D0 + (   DP_VV_V ( 3, VTD%MOM%PLAN(1,VTD__VEL,VTD__EART),  &
     &                                 VTD%SOU(ISOU)%S_CRS               )  &
     &                  + DP_VV_V ( 3, VTD%STA(ISTA2)%VEL_CRS,              &
     &                                 VTD%SOU(ISOU)%S_CRS               )  &
     &                )/VTD__C                                              &
     &       )
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!        write ( 6, * ) 'sou= ', VTD%SOU(ISOU)%IVS_NAME, ' tau_grav= ', tau_grav ! %%%%%
!   if ( VTD%SOU(ISOU)%PRLX .ne. 0.0d0 ) then
!        write ( 6, * ) 'sou= ', VTD%SOU(ISOU)%IVS_NAME, ' prlx = ', VTD%SOU(ISOU)%PRLX, ' del_prl = ', - VTD%SOU(ISOU)%PRLX/DIST_SUN_EARTH/VTD__C*DP_VV_V ( 3, VTD%MOM%PLAN(1,VTD__COO,VTD__EART), B_CRS ),  ' del_prp= ', -tim_src * dp_vv_v ( 3, b_crs, vtd%sou(isou)%s_crs_rate )/vtd__c, ' TIM_SRC= ', TIM_SRC, &
!     &       ' sou_mjd_ref= ', vtd%sou(isou)%mjd_ref  ! %%%%%%%%%%
!!        call pause ( 'sdsd' ) ! %%%%
!   end if
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!
      RATE_GEOM = &
     &     ( - DP_VV_V ( 3, V_CRS, VTD%SOU(ISOU)%S_CRS )/VTD__C * &
     &         ( 1.0D0                                            &
     &          -2.0D0*VTD__GM(VTD__SUN)/DIST_SUN_EARTH/VTD__C**2 &
     &          -TRS_METRIC*VTD__GM(VTD__EART)/VTD__REA/VTD__C**2 &
     &          -DP_VV_V ( 3, VTD%MOM%PLAN(1,VTD__VEL,VTD__EART), &
     &                        VTD%MOM%PLAN(1,VTD__VEL,VTD__EART)  )/2.0D0/VTD__C**2 &
     &          -DP_VV_V ( 3, VTD%MOM%PLAN(1,VTD__VEL,VTD__EART), &
     &                        VTD%STA(ISTA2)%VEL_CRS )/VTD__C**2  &
     &         )                                                  &
     &       - ( DP_VV_V ( 3, VTD%MOM%PLAN(1,VTD__ACC,VTD__EART), B_CRS ) + &
     &           DP_VV_V ( 3, VTD%MOM%PLAN(1,VTD__VEL,VTD__EART), V_CRS )   &
     &         )*                                                           &
     &         (1.0D0 + DP_VV_V ( 3, VTD%MOM%PLAN(1,VTD__VEL,VTD__EART),    &
     &                                  VTD%SOU(ISOU)%S_CRS)/2.0D0/VTD__C   &
     &                             )/ VTD__C**2                             &
     &       - VTD%SOU(ISOU)%PRLX/DIST_SUN_EARTH/VTD__C*                    &
     &          DP_VV_V ( 3, VTD%MOM%PLAN(1,VTD__COO,VTD__EART), V_CRS )    &
     &       - VTD%SOU(ISOU)%PRLX/DIST_SUN_EARTH/VTD__C*                    &
     &          DP_VV_V ( 3, VTD%MOM%PLAN(1,VTD__VEL,VTD__EART), B_CRS )    &
     &       - TIM_SRC * DP_VV_V ( 3, V_CRS, VTD%SOU(ISOU)%S_CRS_RATE )/VTD__C * &
     &       + RAT_GRAV                                                     &
     &     ) /                                                              &
     &       (1.0D0 + (   DP_VV_V ( 3, VTD%MOM%PLAN(1,VTD__VEL,VTD__EART),  &
     &                                 VTD%SOU(ISOU)%S_CRS               )  &
     &                  + DP_VV_V ( 3, VTD%STA(ISTA2)%VEL_CRS,              &
     &                                 VTD%SOU(ISOU)%S_CRS               )  &
     &                )/VTD__C  &
     &       ) &
#define comment here
     &       - &
#define comment here
     &      ( -DP_VV_V ( 3, B_CRS, VTD%SOU(ISOU)%S_CRS )/VTD__C*  &
     &         ( 1.0D0                                            &
     &          -2.0D0*VTD__GM(VTD__SUN)/DIST_SUN_EARTH/VTD__C**2 &
     &          -TRS_METRIC*VTD__GM(VTD__EART)/VTD__REA/VTD__C**2 &
     &          -DP_VV_V ( 3, VTD%MOM%PLAN(1,VTD__VEL,VTD__EART), &
     &                        VTD%MOM%PLAN(1,VTD__VEL,VTD__EART)  )/2.0D0/VTD__C**2 &
     &          -DP_VV_V ( 3, VTD%MOM%PLAN(1,VTD__VEL,VTD__EART), &
     &                        VTD%STA(ISTA2)%VEL_CRS )/VTD__C**2  &
     &         )                                                  &
     &        -DP_VV_V ( 3, VTD%MOM%PLAN(1,VTD__VEL,VTD__EART), B_CRS )*    &
     &            (1.0D0 + DP_VV_V ( 3, VTD%MOM%PLAN(1,VTD__VEL,VTD__EART), &
     &                                  VTD%SOU(ISOU)%S_CRS)/2.0D0/VTD__C   &
     &                             )/ VTD__C**2                             &
     &       + TAU_GRAV )* &
     &       ( DP_VV_V ( 3, VTD%MOM%PLAN(1,VTD__ACC,VTD__EART), VTD%SOU(ISOU)%S_CRS ) + &
     &         DP_VV_V ( 3, VTD%STA(ISTA2)%ACC_CRS, VTD%SOU(ISOU)%S_CRS )   &
     &       )/VTD__C /                                                     &
     &       (1.0D0 + (   DP_VV_V ( 3, VTD%MOM%PLAN(1,VTD__VEL,VTD__EART),  &
     &                                 VTD%SOU(ISOU)%S_CRS               )  &
     &                  + DP_VV_V ( 3, VTD%STA(ISTA2)%VEL_CRS,              &
     &                                 VTD%SOU(ISOU)%S_CRS               )  &
     &                )/VTD__C  &
     &       )**2
!
! --- Second time derivative of VLBI delay
!
      ACLR_GEOM = - DP_VV_V ( 3, A_CRS, VTD%SOU(ISOU)%S_CRS )/VTD__C/       &
     &       (1.0D0 + (   DP_VV_V ( 3, VTD%MOM%PLAN(1,VTD__VEL,VTD__EART),  &
     &                                 VTD%SOU(ISOU)%S_CRS               )  &
     &                  + DP_VV_V ( 3, VTD%STA(ISTA2)%VEL_CRS,              &
     &                                 VTD%SOU(ISOU)%S_CRS               )  &
     &                )/VTD__C  &
     &       ) &
#define comment here
     &       + 2.0D0 * &
     &       ( DP_VV_V ( 3, VTD%MOM%PLAN(1,VTD__ACC,VTD__EART), VTD%SOU(ISOU)%S_CRS ) + &
     &         DP_VV_V ( 3, VTD%STA(ISTA2)%ACC_CRS, VTD%SOU(ISOU)%S_CRS )   &
     &       )/VTD__C /                                                     &
     &       (1.0D0 + (   DP_VV_V ( 3, VTD%MOM%PLAN(1,VTD__VEL,VTD__EART),  &
     &                                 VTD%SOU(ISOU)%S_CRS               )  &
     &                  + DP_VV_V ( 3, VTD%STA(ISTA2)%VEL_CRS,              &
     &                                 VTD%SOU(ISOU)%S_CRS               )  &
     &                )/VTD__C  &
     &       )**2 * &
     &       DP_VV_V ( 3, V_CRS, VTD%SOU(ISOU)%S_CRS )/VTD__C &
#define comment here
     &       - 2.0D0 * &
     &         DP_VV_V ( 3, VTD%MOM%PLAN(1,VTD__ACC,VTD__EART), V_CRS )/VTD__C**2 &
#define comment here
     &       - DP_VV_V ( 3, VTD%MOM%PLAN(1,VTD__VEL,VTD__EART), A_CRS )/VTD__C**2
!
      IF ( VTD%STA(ISTA1)%STA_TYP == VTD__OR ) THEN
!
! -------- In the case if the 1st antenna is on the orbit, we apply several
! -------- terms that are negligbile for ground stations
!
           RDIST = DSQRT ( DP_VV_V ( 3, VTD%STA(ISTA1)%COO_CRS, VTD%STA(ISTA1)%COO_CRS ) )
           TAU_GEOM =   TAU_GEOM &
     &                - VTD__GM(VTD__EART)/VTD__C**3* &
     &                  (1.D0/RDIST - 1.0D0/VTD__REA)* &
     &                  DP_VV_V ( 3, VTD%STA(ISTA1)%COO_CRS, VTD%SOU(ISOU)%S_CRS )
           RATE_GEOM =  RATE_GEOM &
     &                - VTD__GM(VTD__EART)/VTD__C**3* &
     &                  (1.D0/RDIST - 1.0D0/VTD__REA)* &
     &                  DP_VV_V ( 3, VTD%STA(ISTA1)%VEL_CRS, VTD%SOU(ISOU)%S_CRS )
           ACLR_GEOM =  RATE_GEOM &
     &                - VTD__GM(VTD__EART)/VTD__C**3* &
     &                  (1.D0/RDIST - 1.0D0/VTD__REA)* &
     &                  DP_VV_V ( 3, VTD%STA(ISTA1)%ACC_CRS, VTD%SOU(ISOU)%S_CRS )
      END IF
!
      IF ( VTD%STA(ISTA2)%STA_TYP == VTD__OR ) THEN
!
! -------- In the case if the 2nd antenna is on the orbit, we apply several
! -------- terms that are negligbile for ground stations
!
           RDIST = DSQRT ( DP_VV_V ( 3, VTD%STA(ISTA2)%COO_CRS, VTD%STA(ISTA2)%COO_CRS ) )
           TAU_GEOM =   TAU_GEOM &
     &                - DP_VV_V ( 3, B_CRS, VTD%SOU(ISOU)%S_CRS )* &
     &                  DP_VV_V ( 3, VTD%STA(ISTA2)%VEL_CRS, VTD%STA(ISTA2)%VEL_CRS)/ &
     &                  (2.0D0*VTD__C**3) &
     &                + VTD__GM(VTD__EART)/VTD__C**3* &
     &                  (1.D0/RDIST - 1.0D0/VTD__REA)* &
     &                  DP_VV_V ( 3, VTD%STA(ISTA2)%COO_CRS, VTD%SOU(ISOU)%S_CRS )
           RATE_GEOM =  RATE_GEOM &
     &                - DP_VV_V ( 3, V_CRS, VTD%SOU(ISOU)%S_CRS )* &
     &                  DP_VV_V ( 3, VTD%STA(ISTA2)%VEL_CRS, VTD%STA(ISTA2)%VEL_CRS)/ &
     &                  (2.0D0*VTD__C**3) &
     &                + VTD__GM(VTD__EART)/VTD__C**3* &
     &                  (1.D0/RDIST - 1.0D0/VTD__REA)* &
     &                  DP_VV_V ( 3, VTD%STA(ISTA2)%VEL_CRS, VTD%SOU(ISOU)%S_CRS )
           ACLR_GEOM =  ACLR_GEOM &
     &                - DP_VV_V ( 3, A_CRS, VTD%SOU(ISOU)%S_CRS )* &
     &                  DP_VV_V ( 3, VTD%STA(ISTA2)%VEL_CRS, VTD%STA(ISTA2)%VEL_CRS)/ &
     &                  (2.0D0*VTD__C**3) &
     &                + VTD__GM(VTD__EART)/VTD__C**3* &
     &                  (1.D0/RDIST - 1.0D0/VTD__REA)* &
     &                  DP_VV_V ( 3, VTD%STA(ISTA2)%ACC_CRS, VTD%SOU(ISOU)%S_CRS )
!!
           RATE_GEOM =  RATE_GEOM - VTD%MOM%NZO_RLT
      END IF
!
! --- Compute derivatives with respect to EOP
!
      C_FACTOR = 1.0D0/(                                                    &
     &                     DP_VV_V ( 3, VTD%MOM%PLAN(1,VTD__VEL,VTD__EART), &
     &                                  VTD%SOU(ISOU)%S_CRS )               &
     &                   + DP_VV_V ( 3, VTD%STA(ISTA2)%VEL_CRS,             &
     &                                  VTD%SOU(ISOU)%S_CRS )               &
     &                   + VTD__C                                           &
     &                 )
      CALL SUB_VV_V ( 3, VTD%STA(ISTA2)%MOM_COO_TRS, &
     &                   VTD%STA(ISTA1)%MOM_COO_TRS, B_TRS )
      DO 420 J2=1,3
         TAU_DER_STA1(J2) =   C_FACTOR* DP_VV_V ( 3, TRS_TO_CRS(1,J2),       &
     &                        VTD%SOU(ISOU)%S_CRS )                          &
     &                      + 1.D0/VTD__C**2* DP_VV_V ( 3, TRS_TO_CRS(1,J2), &
     &                        VTD%MOM%PLAN(1,VTD__VEL,VTD__EART) )
         TAU_DER_STA2(J2) = - TAU_DER_STA1(J2)
!
         CALL MUL_MV_IV_V ( 3, 3, DTRS_TO_CRS_DEOP(1,1,J2), &
     &                         3, B_TRS,                    &
     &                         3, VEC_TMP1, IER )
         CALL MUL_MV_IV_V ( 3, 3, DTRS_TO_CRS_DER1_DEOP(1,1,J2), &
     &                         3, B_TRS,                         &
     &                         3, VEC_TMP2, IER )
         TAU_DER_EOP(J2)  = - C_FACTOR* DP_VV_V ( 3, VTD%SOU(ISOU)%S_CRS,      &
     &                                               VEC_TMP1 )                &
     &                      + C_FACTOR**2* DP_VV_V ( 3, VTD%SOU(ISOU)%S_CRS,   &
     &                                               VEC_TMP2 )                &
     &                      - 1.D0/VTD__C**2 *                                 &
     &                        DP_VV_V ( 3, VTD%MOM%PLAN(1,VTD__VEL,VTD__EART), &
     &                                     VEC_TMP1 )
!
         RATE_DER_STA1(J2) =   C_FACTOR* DP_VV_V ( 3, TRS_TO_CRS_DER1(1,J2),      &
     &                                                VTD%SOU(ISOU)%S_CRS )       &
     &                      + 1.D0/VTD__C**2* DP_VV_V ( 3, TRS_TO_CRS_DER1(1,J2), &
     &                        VTD%MOM%PLAN(1,VTD__VEL,VTD__EART) )                &
     &                      + 1.D0/VTD__C**2* DP_VV_V ( 3, TRS_TO_CRS(1,J2),      &
     &                        VTD%MOM%PLAN(1,VTD__ACC,VTD__EART) )
         RATE_DER_STA2(J2) = - RATE_DER_STA1(J2) 
!
         RATE_DER_EOP(J2)  = - C_FACTOR* DP_VV_V ( 3, VTD%SOU(ISOU)%S_CRS,      &
     &                                                VEC_TMP2 )                &
     &                       - 1.D0/VTD__C**2 *                                 &                       
     &                         DP_VV_V ( 3, VTD%MOM%PLAN(1,VTD__VEL,VTD__EART), &
     &                                      VEC_TMP2 )
 420  CONTINUE
!
      DS_DALPHA(1) = -VTD%SOU(ISOU)%S_CRS(2)
      DS_DALPHA(2) =  VTD%SOU(ISOU)%S_CRS(1)
      DS_DALPHA(3) =  0.0D0
!
      DS_DDELTA(1) = -VTD%SOU(ISOU)%S_CRS(1)*VTD%SOU(ISOU)%S_CRS(3)/ &
     &                DSQRT( 1.0D0 - VTD%SOU(ISOU)%S_CRS(3)**2 )
      DS_DDELTA(2) = -VTD%SOU(ISOU)%S_CRS(2)*VTD%SOU(ISOU)%S_CRS(3)/ &
     &                DSQRT( 1.0D0 - VTD%SOU(ISOU)%S_CRS(3)**2 )
      DS_DDELTA(3) =  DSQRT( 1.0D0 - VTD%SOU(ISOU)%S_CRS(3)**2 )
!
      TAU_DER_RA = - C_FACTOR    * DP_VV_V ( 3, B_CRS, DS_DALPHA )             &
     &             + C_FACTOR**2 * DP_VV_V ( 3, B_CRS, VTD%SOU(ISOU)%S_CRS ) * &
     &      (                                                                  &
     &          DP_VV_V ( 3, VTD%MOM%PLAN(1,VTD__VEL,VTD__EART), DS_DALPHA )   &
     &        + DP_VV_V ( 3, VTD%STA(ISTA2)%VEL_CRS, DS_DALPHA )               &
     &      )
!
      TAU_DER_DL = - C_FACTOR    * DP_VV_V ( 3, B_CRS, DS_DDELTA )             &
     &             + C_FACTOR**2 * DP_VV_V ( 3, B_CRS, VTD%SOU(ISOU)%S_CRS ) * &
     &      (                                                                  &
     &          DP_VV_V ( 3, VTD%MOM%PLAN(1,VTD__VEL,VTD__EART), DS_DDELTA )   &
     &        + DP_VV_V ( 3, VTD%STA(ISTA2)%VEL_CRS, DS_DDELTA )               &
     &      )
!
      RATE_DER_RA = - C_FACTOR    * DP_VV_V ( 3, V_CRS, DS_DALPHA )             &
     &              + C_FACTOR**2 * DP_VV_V ( 3, V_CRS, VTD%SOU(ISOU)%S_CRS ) * &
     &      (                                                                   &
     &          DP_VV_V ( 3, VTD%MOM%PLAN(1,VTD__ACC,VTD__EART), DS_DALPHA )    &
     &        + DP_VV_V ( 3, VTD%STA(ISTA2)%ACC_CRS, DS_DALPHA )                &
     &      )
!
      RATE_DER_DL = - C_FACTOR    * DP_VV_V ( 3, V_CRS, DS_DDELTA )             &
     &              + C_FACTOR**2 * DP_VV_V ( 3, V_CRS, VTD%SOU(ISOU)%S_CRS ) * &
     &      (                                                                   &
     &          DP_VV_V ( 3, VTD%MOM%PLAN(1,VTD__ACC,VTD__EART), DS_DDELTA )    &
     &        + DP_VV_V ( 3, VTD%STA(ISTA2)%ACC_CRS, DS_DDELTA )                &
     &      )
!
      TAU_DER_GAMMA = -DP_VV_V ( 3, B_CRS, VTD%SOU(ISOU)%S_CRS )/VTD__C *        &
     &                 VTD__GM(VTD__SUN)/DIST_SUN_EARTH/VTD__C**2/               &
     &                 (1.0D0 + DP_VV_V ( 3, VTD%MOM%PLAN(1,VTD__VEL,VTD__EART), &
     &                                  VTD%SOU(ISOU)%S_CRS)/2.0D0/VTD__C        &
     &                                  )/ VTD__C**2                             &
     &                 + TAU_GRAV/2.0D0
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  VTD_PK2001  !#!#
