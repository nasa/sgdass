      SUBROUTINE VTD_TROP_AXOF_COUPL ( AXOF_UP, AXOF_UP_RATE, TROP_DEL, &
     &                                 TROP_AXOF_TAU, TROP_AXOF_RAT )
! ************************************************************************
! *                                                                      *
! *   Routine  VTD_TROP_AXOF_COUPL  computes an additional time delay    *
! *   which stems from the coupling between troposphere path delay and   *
! *   antenna axis offset delay. It is zero for antennas with azimuthal  *
! *   mounting.                                                          *
! *                                                                      *
! * ________________________ Input parameters: _________________________ *
! *                                                                      *
! *     AXOF_UP ( REAL*8    ) -- Displacement of the point on the moving *
! *                              antenna's axis in vertical direction.   *
! *                              This point is used for modeling         *
! *                              geometric  path delay. Units: meters.   *
! * AXOF_UP_RAT ( REAL*8    ) -- Rate of change of AXOF_UP. Units: m/s   *
! *    TROP_DEL ( REAL*8    ) -- Troposphere path delay for this antenna.*
! *                              Units: seconds.                         *
! *                                                                      *
! * ________________________ Output parameters: ________________________ *
! *                                                                      *
! * TROP_AXOF_TAU ( REAL*8    ) -- Additional time delay due to          *
! *                                coupling between the antenna's axis   *
! *                                offset and troposphere path delay.    *
! *                                Units: seconds.                       *
! * TROP_AXOF_RAT ( REAL*8    ) -- Additional delay rate due to          *
! *                                coupling between the antenna's axis   *
! *                                offset and troposphere path delay     *
! *                                rate. Units: dimensionless.           *
! *                                                                      *
! * ## 25-JUN-2004 VTD_TROP_AXOF_COUPL  v1.0(c) L. Petrov 25-JUN-2004 ## *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'vtd.i'
      REAL*8     AXOF_UP, AXOF_UP_RATE, TROP_DEL, TROP_AXOF_TAU, TROP_AXOF_RAT 
      INTEGER*4  ISOU
      REAL*8     DP_VV_V
!
      TROP_AXOF_TAU = -1.1859D-4 * AXOF_UP * TROP_DEL
      TROP_AXOF_RAT = -1.1859D-4 * AXOF_UP_RATE * TROP_DEL
!
      RETURN
      END  SUBROUTINE VTD_TROP_AXOF_COUPL 

