      SUBROUTINE VTD_TROP_GEOM_COUPL ( VTD, ISOU, TROP_DEL, TROP_RAT, &
     &                                 TROP_GEOM_TAU, TROP_GEOM_RAT )
! ************************************************************************
! *                                                                      *
! *   Routine  VTD_TROP_GEOM_COUPL  computes an additional time delay    *
! *   which stems from the coupling between the troposphere path delay   *
! *   and the geometrical delay.                                         *
! *                                                                      *
! * ________________________ Input parameters: _________________________ *
! *                                                                      *
! *      VTD ( RECORD    ) -- Object which keeps configuration and data  *
! *                           related to VLBI Theoretical Delay (VTD)    *
! *                           package.                                   *
! *     ISOU ( INTEGER*4 ) -- Index of the source in the VTD source list.*
! * TROP_DEL ( REAL*8    ) -- Total path delay in the direction to the   *
! *                           source. Units: seconds.                    *
! * TROP_RAT ( REAL*8    ) -- Rate of change of the total path delay in  *
! *                           the direction to the source. Dimensionless.*
! *                                                                      *
! * ________________________ Output parameters: ________________________ *
! *                                                                      *
! *  TROP_GEOM_TAU ( REAL*8    ) -- Additional time delay due to         *
! *                                 coupling between the geometrical     *
! *                                 delay and the troposphere path delay.*
! *                                 Units: seconds.                      *
! *  TROP_GEOM_RAT ( REAL*8    ) -- Additional delay rate due to         *
! *                                 coupling between the geometrical     *
! *                                 delay and the troposphere path delay.*
! *                                 Units: dimensionless,                *
! *                                                                      *
! * ## 25-JUN-2004 VTD_TROP_GEOM_COUPL  v2.0(c) L. Petrov 24-DEC-2005 ## *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'vtd.i'
      TYPE     ( VTD__TYPE ) :: VTD
      REAL*8     TROP_DEL, TROP_RAT, TROP_GEOM_TAU, TROP_GEOM_RAT 
      INTEGER*4  ISOU
      REAL*8     DP_VV_V
!
      TROP_GEOM_TAU = - DP_VV_V ( 3, VTD%MOM%PLAN(1,VTD__VEL,VTD__EART), &
     &                            VTD%SOU(ISOU)%S_CRS )*TROP_DEL/VTD__C
      IF ( VTD%CONF%FL_RATE ) THEN
           TROP_GEOM_RAT = - DP_VV_V ( 3, VTD%MOM%PLAN(1,VTD__VEL,VTD__EART),   &
     &                                    VTD%SOU(ISOU)%S_CRS )*TROP_RAT/VTD__C &
     &                     - DP_VV_V ( 3, VTD%MOM%PLAN(1,VTD__ACC,VTD__EART),   &
     &                                    VTD%SOU(ISOU)%S_CRS )*TROP_DEL/VTD__C
      END IF
!
      RETURN
      END  SUBROUTINE VTD_TROP_GEOM_COUPL 
