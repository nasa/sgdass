      SUBROUTINE VTD_SK1999 ( VTD, ISTA1, ISTA2, ISOU, TAU_GEOM, RATE_GEOM, &
     &                        ACCL_GEOM, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine VTD_SK1999
! *                                                                      *
! *  ### 01-FEB-2004   VTD_SK1999  v1.0 (c)  L. Petrov  30-JAN-2004 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'vtd.i'
      INTEGER*4  ISTA1, ISTA2, ISOU, IUER
      REAL*8     TAU_GEOM, RATE_GEOM, ACCL_GEOM
      TYPE     ( VTD__TYPE ) :: VTD
      INTEGER*4  J1
      REAL*8     R1A(3), R2A(3), R1A_LEN, R2A_LEN, TAU_GRAV, TRS_METRIC, &
     &           B_CRS(3), COO1_BRS(3), COO2_BRS(3), DIST1_BRS, DIST2_BRS, &
     &           DIST_SUN_EARTH
                     REAL*8     TAU_GRAV_0, TAU_GRAV_CNS, TAU_GR_JUP_DER
      REAL*8     DP_VV_V
!
      TAU_GRAV = 0.0D0
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
         CALL SUB_VV_V ( 3, COO1_BRS, VTD%MOM%PLAN(1,VTD__COO,J1), R1A )
         CALL SUB_VV_V ( 3, COO2_BRS, VTD%MOM%PLAN(1,VTD__COO,J1), R2A )
         CALL NORM_VEC ( 3, R1A, R1A_LEN )
         CALL NORM_VEC ( 3, R2A, R2A_LEN )
!
         TAU_GRAV = TAU_GRAV + &
     &           2.D0*VTD__GM(J1)/VTD__C**3*(1.0D0 + &
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
!
!@         if ( j1 .eq. 6 ) then
!@              tau_gr_jup_der = vtd__gm(j1)/vtd__c**3*(1.0d0 + &
!@     &           dp_vv_v ( 3, vtd%sou(isou)%s_crs, vtd%mom%plan(1,vtd__vel,j1) &
!@     &                   )/vtd__c )* &
!@     &      dlog ( (r1a_len*(1.0d0 &
!@     &              + dp_vv_v ( 3, vtd%sou(isou)%s_crs, r1a )  &
!@     &              + dp_vv_v ( 3, vtd%mom%plan(1,vtd__vel,j1), r1a )/vtd__c &
!@     &              - dp_vv_v ( 3, vtd%sou(isou)%s_crs, r1a )* &
!@     &                dp_vv_v ( 3, vtd%mom%plan(1,vtd__vel,j1), &
!@     &                             vtd%sou(isou)%s_crs )/vtd__c ) &
!@     &             )/ &
!@     &             ( r2a_len*(1.0d0 &
!@     &              + dp_vv_v ( 3, vtd%sou(isou)%s_crs, r2a ) &
!@     &              + dp_vv_v ( 3, vtd%mom%plan(1,vtd__vel,j1), r2a )/vtd__c &
!@     &              - dp_vv_v ( 3, vtd%sou(isou)%s_crs, r2a )* &
!@     &                dp_vv_v ( 3, vtd%mom%plan(1,vtd__vel,j1), &
!@     &                             vtd%sou(isou)%s_crs )/vtd__c ) &
!@     &             ) &
!@     &           )/ &
!@     &       (1.0d0 + (   dp_vv_v ( 3, vtd%mom%plan(1,vtd__vel,vtd__eart),  &
!@     &                                 vtd%sou(isou)%s_crs               )  &
!@     &                  + dp_vv_v ( 3, vtd%sta(ista2)%vel_crs,              &
!@     &                                 vtd%sou(isou)%s_crs               )  &
!@     &                )/vtd__c  &
!@     &       )
!@         end if
!
! $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
!$        if ( j1 .eq. 6 ) then
!$             tau_grav_0 = 2.d0*vtd__gm(j1)/vtd__c**3* &
!$     &       dlog ( (r1a_len*(1.0d0 + dp_vv_v (3, vtd%sou(isou)%s_crs, r1a)))/ &
!$     &              (r2a_len*(1.0d0 + dp_vv_v (3, vtd%sou(isou)%s_crs, r2a))) )
!$             tau_geom = (tau_grav - tau_grav_0)*1.d12
!$!@             write ( 6, 110 ) (tau_grav - tau_grav_0)*1.d12, tau_grav*1.d12
!$!@ 110         format ( ' Delta_sk1999: =',F18.6, ' psec | ', f18.6 ) 
!$             call err_log ( 0, iuer )
!$             return 
!$        end if
! $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
 410  CONTINUE 
!
      CALL SUB_VV_V ( 3, VTD%STA(ISTA2)%COO_CRS, VTD%STA(ISTA1)%COO_CRS, B_CRS )
      DIST_SUN_EARTH = DSQRT (   VTD%MOM%PLAN(1,VTD__COO,VTD__EART)**2 &
     &                         + VTD%MOM%PLAN(2,VTD__COO,VTD__EART)**2 &
     &                         + VTD%MOM%PLAN(3,VTD__COO,VTD__EART)**2 )
      IF ( VTD%CONF%GRS_METRIC == VTD__METRIC_IAU2000 ) THEN
           TRS_METRIC = 1.0D0
         ELSE IF ( VTD%CONF%GRS_METRIC == VTD__METRIC_ITRF2000 ) THEN
           TRS_METRIC = 2.0D0
         ELSE 
           CALL ERR_LOG ( 2911, IUER, 'VTD_PL2001', 'Unknown code of the '// &
     &         'GRS metric' ) 
           RETURN 
      END IF
!
! --- Compute geometric path delay
!
      TAU_GEOM = ( -DP_VV_V ( 3, B_CRS, VTD%SOU(ISOU)%S_CRS )/VTD__C* &
     &         ( 1.0D0                                                &
     &          -2.0D0*VTD__GM(VTD__SUN)/DIST_SUN_EARTH/VTD__C**2 &
     &          -TRS_METRIC*VTD__GM(VTD__EART)/VTD__REA/VTD__C**2 &
     &          -DP_VV_V ( 3, VTD%MOM%PLAN(1,VTD__VEL,VTD__EART), &
     &                        VTD%MOM%PLAN(1,VTD__VEL,VTD__EART)  )/2.0D0/VTD__C**2 &
     &          -DP_VV_V ( 3, VTD%MOM%PLAN(1,VTD__VEL,VTD__EART), &
     &                        VTD%STA(ISTA2)%VEL_CRS )/VTD__C**2  &
     &         ) &
     &       - DP_VV_V ( 3, VTD%MOM%PLAN(1,VTD__VEL,VTD__EART), B_CRS )*    &
     &            (1.0D0 + DP_VV_V ( 3, VTD%MOM%PLAN(1,VTD__VEL,VTD__EART), &
     &                                  VTD%SOU(ISOU)%S_CRS)/2.0D0/VTD__C   &
     &                             )/ VTD__C**2                             &
     &       + TAU_GRAV )/ &
     &       (1.0D0 + (   DP_VV_V ( 3, VTD%MOM%PLAN(1,VTD__VEL,VTD__EART),  &
     &                                 VTD%SOU(ISOU)%S_CRS               )  &
     &                  + DP_VV_V ( 3, VTD%STA(ISTA2)%VEL_CRS,              &
     &                                 VTD%SOU(ISOU)%S_CRS               )  &
     &                )/VTD__C  &
     &       )
!
!@         tau_geom = tau_gr_jup_der ! %%%
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  VTD_SK1999
