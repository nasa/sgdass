      SUBROUTINE E3ZT_YODER1981_ALL ( MJD, TAI_SEC, E3, E3_DOT ) 
! ************************************************************************
! *                                                                      *
! *   Routine E3ZT_YODER1981_ALL  computes contribution of zonal solid   *
! *   Earth tides to the Euler angle around the axis 3 as well as its    *
! *   time derivative. Expansion of Yoder 1981 is used. All terms, short *
! *   periodic and long periodic are taken into account.                 *
! *   Yoder, C.F., Williams, J.G., and Parke, M.E., "Tidal variations of *
! *   Earth rotation", J. Geophys. Res., vol. 86, pp. 881-891, 1981.     *
! *                                                                      *
! *   NB: Delta E3 (rad) = -1.002737909 * Delta UT1 * 2*PI/86400.0 (sec) *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *     MJD ( INTEGER*4 ) -- Modified Julian date.                       *
! * TAI_SEC ( REAL*8    ) -- Time in TAI, in seconds.                    *
! *                                                                      *
! * _________________________ Output parameters: _______________________ *
! *                                                                      *
! *      E3 ( REAL*8    ) -- Contribution to the Euler angle around the  *
! *                          axis 3 due to zonal tides. Units: rad.      *
! *  E3_DOT ( REAL*8    ) -- Contribution to the angular velocity along  *
! *                          the axis 3 due to zonal tides.              *
! *                          Units: rad/sec.                             *
! *                                                                      *
! * ### 22-MAR-2004 E3ZT_YODER1981_ALL v1.0 (c) L. Petrov 22-MAR-2004 ## *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'e3zt_yoder1981.i'
      INTEGER*4  MJD
      REAL*8     TAI_SEC, E3, E3_DOT
      INTEGER*4  J2000__MJD  
      REAL*8     J2000__JD   
      PARAMETER  ( J2000__JD   = 2451545.0D0 ) ! 2000.01.01_12:00:00
      PARAMETER  ( J2000__MJD  = 51544       ) ! 2000.01.01_00:00:00
      REAL*8     TDT_ARG, ARG, ARG_DOT
      INTEGER*4  J1
!
      TDT_ARG = ( MJD - J2000__MJD )*86400.0D0 + ( TAI_SEC - 43200.0D0 ) + &
     &          32.184D0
      E3     = 0.0D0
      E3_DOT = 0.0D0
      DO 410 J1=1,N_YODER1981
         ARG = ( 0.5D0*ACCL_YODER1981(J1) *TDT_ARG &
     &               + FREQ_YODER1981(J1))*TDT_ARG &
     &               + PHAS_YODER1981(J1)
         ARG_DOT = ACCL_YODER1981(J1)*TDT_ARG + FREQ_YODER1981(J1)
         E3      = E3     + E3S_YODER1981(J1)*DSIN(ARG)
         E3_DOT  = E3_DOT + ARG_DOT*E3S_YODER1981(J1)*DCOS(ARG)
 410  CONTINUE 
!
      RETURN
      END  SUBROUTINE E3ZT_YODER1981_ALL
