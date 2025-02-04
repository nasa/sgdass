      SUBROUTINE VTD_KS1999 ( VTD, ISTA1, ISTA2, ISOU, &
     &                        TRS_TO_CRS, DTRS_TO_CRS_DEOP, &
     &                        TAU_GEOM, RATE_GEOM, ACLR_GEOM, &
     &                        TAU_DER_EOP, &
     &                        TAU_DER_STA1, TAU_DER_STA2, TAU_DER_RA, &
     &                        TAU_DER_DL, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine VTD_KS1999 computes VLBI time delay, its first and second  *
! *   time derivatives as well as some partial derivatgies on parameters *
! *   of the model according to expression of Kopeikin, Schaefer, 1999.  *
! *   Precision of the expression is better than 5.D-13 seconds.         *
! *   Relative precision of partial derivaties is better than 1.D-6.     *
! *   It is assumed that intermediate quantities, such as instantaneous  *
! *   site positions, coordinates of celestial bodies etc. have been     *
! *   computed beforehand and stored in the internal fields of the       *
! *   object VTD.                                                        *
! *                                                                      *
! *   VLBI Time delay is defined as the difference of two intervals of   *
! *   proper time: 1) the interval of proper time of station #1 between  *
! *   events: coming the wave front to the reference point on the moving *
! *   axis and clock synchronization; 2) the interval of proper time of  *
! *   station #2 between events: coming the wave front to the reference  *
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
! *   Caveat: currently RATE_GEOM is not computed.                       *
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
! * DTRS_TO_CRS_DEOP ( REAL*8   ) -- Array of time derivatives of the    *
! *                                  transformation matrix from the      *
! *                                  terrestrial reference system to     *
! *                                  the celestial reference system at   *
! *                                  the moment of time MJD,TAI with     *
! *                                  respect to the small vector of      *
! *                                  rotation with Euler angles E1, E2,  *
! *                                  end E3. Dimension: 3,3,3. The last  *
! *                                  dimension runs over E1, E2 and E3   *
! *                                  Euler angles.                       *
! *                                                                      *
! * _________________________ Output parameters: _______________________ *
! *                                                                      *
! *     TAU_GEOM ( REAL*8    ) -- Geometric delay. Units: seconds.       *
! *    RATE_GEOM ( REAL*8    ) -- First time derivative of geometric     *
! *                               time delay. Units: dimensionless.      *
! *    ACLR_GEOM ( REAL*8    ) -- Second time derivative of geometric    *
! *                               time delay. Units: 1/seconds.          *
! *                               NB: accuracy of ACLR_GEOM is low:      *
! *                               10^{-6}                                *
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
! *  ### 01-FEB-2004   VTD_KS1999  v2.0 (c)  L. Petrov  21-SEP-2004 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'vtd.i'
      INTEGER*4  ISTA1, ISTA2, ISOU, IUER
      REAL*8     TRS_TO_CRS(3,3), DTRS_TO_CRS_DEOP(3,3,3), TAU_GEOM, &
     &           RATE_GEOM, ACLR_GEOM, TAU_DER_EOP(3), TAU_DER_STA1(3), &
     &           TAU_DER_STA2(3), TAU_DER_RA, TAU_DER_DL
      TYPE     ( VTD__TYPE ) :: VTD
      INTEGER*4  J1, J2, IER
      REAL*8     R1A(3), R2A(3), R1A_LEN, R2A_LEN, TAU_GRAV, TAU_GRAV_PLAN, &
     &           TRS_METRIC, B_CRS(3), COO1_BRS(3), COO2_BRS(3), DIST1_BRS, &
     &           DIST2_BRS, DIST_SUN_EARTH, DS_DALPHA(3), DS_DDELTA(3), &
     &           C_FACTOR, B_TRS(3), VEC_TMP(3)
      REAL*8,    EXTERNAL :: DP_VV_V
!
      RATE_GEOM = 0.0D0
!
! --- Computation of gravitation delay.
!
      TAU_GRAV = 0.0D0
      DO 410 J1=1,VTD__M_PLA ! Cycle over all planets, Sun and Moon
!
! ------ Compute COO1_BRS and COO2_BRS -- baricentric vectors of the 
! ------ the first and the second station at the time of arrival of
! ------ the photon at the first station
!
         CALL ADD_VV_V ( 3, VTD%STA(ISTA1)%COO_CRS, &
     &                      VTD%MOM%PLAN(1,VTD__COO,VTD__EART), COO1_BRS )
         CALL ADD_VV_V ( 3, VTD%STA(ISTA2)%COO_CRS, &
     &                      VTD%MOM%PLAN(1,VTD__COO,VTD__EART), COO2_BRS )
         CALL SUB_VV_V ( 3, COO1_BRS, VTD%MOM%PLAN(1,VTD__COO,J1), R1A )
         CALL SUB_VV_V ( 3, COO2_BRS, VTD%MOM%PLAN(1,VTD__COO,J1), R2A )
         CALL NORM_VEC ( 3, R1A, R1A_LEN )
         CALL NORM_VEC ( 3, R2A, R2A_LEN )
!
         TAU_GRAV_PLAN = 2.D0*VTD__GM(J1)/VTD__C**3*(1.0D0 + &
     &           DP_VV_V ( 3, VTD%SOU(ISOU)%S_CRS, VTD%MOM%PLAN(1,VTD__VEL,J1) &
     &                   )/VTD__C )* &
     &      DLOG ( (R1A_LEN*(1.0D0 &
     &              + DP_VV_V ( 3, VTD%SOU(ISOU)%S_CRS, R1A )  &
     &              + DP_VV_V ( 3, VTD%MOM%PLAN(1,VTD__VEL,J1), R1A )/VTD__C &
     &              - DP_VV_V ( 3, VTD%SOU(ISOU)%S_CRS, R1A )* &
     &                DP_VV_V ( 3, VTD%MOM%PLAN(1,VTD__VEL,J1), &
     &                             VTD%SOU(ISOU)%S_CRS )/VTD__C ) &
     &             )/ &
     &             ( R2A_LEN*(1.0D0 &
     &              + DP_VV_V ( 3, VTD%SOU(ISOU)%S_CRS, R2A ) &
     &              + DP_VV_V ( 3, VTD%MOM%PLAN(1,VTD__VEL,J1), R2A )/VTD__C &
     &              - DP_VV_V ( 3, VTD%SOU(ISOU)%S_CRS, R2A )* &
     &                DP_VV_V ( 3, VTD%MOM%PLAN(1,VTD__VEL,J1), &
     &                             VTD%SOU(ISOU)%S_CRS )/VTD__C ) &
     &             ) &
     &           )
          TAU_GRAV = TAU_GRAV + TAU_GRAV_PLAN
 410  CONTINUE 
!
      CALL SUB_VV_V ( 3, VTD%STA(ISTA2)%COO_CRS, VTD%STA(ISTA1)%COO_CRS, B_CRS )
      DIST_SUN_EARTH = DSQRT (   VTD%MOM%PLAN(1,VTD__COO,VTD__EART)**2 &
     &                         + VTD%MOM%PLAN(2,VTD__COO,VTD__EART)**2 &
     &                         + VTD%MOM%PLAN(3,VTD__COO,VTD__EART)**2 )
!
! --- Get the coefficient before the term of geocetrnic potential. 
! --- It depends on a boundary condition
!
      IF ( VTD%CONF%GRS_METRIC == VTD__METRIC_IAU2000 ) THEN
           TRS_METRIC = 2.0D0
         ELSE IF ( VTD%CONF%GRS_METRIC == VTD__METRIC_ITRF2000 ) THEN
           TRS_METRIC = 0.0D0
         ELSE 
           CALL ERR_LOG ( 2911, IUER, 'VTD_SK2001', 'Unknown code of the '// &
     &         'GRS metric' ) 
           RETURN 
      END IF
!
! --- Compute geometric path delay
!
      TAU_GEOM = ( -DP_VV_V ( 3, B_CRS, VTD%SOU(ISOU)%S_CRS )/VTD__C*          &
     &               ( 1.0D0                                                   &
     &                 -2.0D0*VTD__GM(VTD__SUN)/DIST_SUN_EARTH/VTD__C**2       &
     &                 -TRS_METRIC*VTD__GM(VTD__EART)/VTD__REA/VTD__C**2       &
     &                 -DP_VV_V ( 3, VTD%MOM%PLAN(1,VTD__VEL,VTD__EART),       &
     &                               VTD%MOM%PLAN(1,VTD__VEL,VTD__EART)        &
     &                          )/2.0D0/VTD__C**2                              &
     &                 -DP_VV_V ( 3, VTD%MOM%PLAN(1,VTD__VEL,VTD__EART),       &
     &                            VTD%STA(ISTA2)%VEL_CRS                       &
     &                          )/VTD__C**2                                    &
     &               )                                                         &
     &             - DP_VV_V ( 3, VTD%MOM%PLAN(1,VTD__VEL,VTD__EART), B_CRS )* &
     &               (1.0D0 + DP_VV_V ( 3, VTD%MOM%PLAN(1,VTD__VEL,VTD__EART), &
     &                                     VTD%SOU(ISOU)%S_CRS                 &
     &                                )/2.0D0/VTD__C                           &
     &               )/VTD__C**2                                               &
     &             + TAU_GRAV                                                  &
     &            )/                                                           &
     &            (1.0D0 + (  DP_VV_V ( 3, VTD%MOM%PLAN(1,VTD__VEL,VTD__EART), &
     &                                     VTD%SOU(ISOU)%S_CRS     )           &
     &                      + DP_VV_V ( 3, VTD%STA(ISTA2)%VEL_CRS,             &
     &                                     VTD%SOU(ISOU)%S_CRS     )           &
     &                     )/VTD__C                                            &
     &            )
!
      C_FACTOR = 1.0D0/(                                                    &
     &                     DP_VV_V ( 3, VTD%MOM%PLAN(1,VTD__VEL,VTD__EART), &
     &                                  VTD%SOU(ISOU)%S_CRS )               &
     &                   + DP_VV_V ( 3, VTD%STA(ISTA2)%VEL_CRS,             &
     &                                  VTD%SOU(ISOU)%S_CRS )               &
     &                   + VTD__C                                           &
     &                 )
!
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
     &                         3, VEC_TMP, IER )
         TAU_DER_EOP(J2)  =   C_FACTOR* DP_VV_V ( 3, VTD%SOU(ISOU)%S_CRS,      &
     &                                            VEC_TMP )                    &
     &                      + C_FACTOR/VTD__C*                                 &
     &                        DP_VV_V ( 3, VTD%MOM%PLAN(1,VTD__VEL,VTD__EART), &
     &                                     VEC_TMP )
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
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  VTD_KS1999
