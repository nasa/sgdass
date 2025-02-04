      SUBROUTINE NERS_E3ZT_RE2014  ( MODE, MJD, TAI_SEC, E3, E3_DOT, E3_DT2 ) 
! ************************************************************************
! *                                                                      *
! *   Routine NERS_E3ZT_RE2014 computes contribution of zonal solid      *
! *   Earth tides and ocean tides to the Euler angle around the axis 3   *
! *   as well as its time derivative. Expansion of Ray & Erofeeva is     *
! *   used.                                                              *
! *                                                                      *
! *   Reference:                                                         *
! *                                                                      *
! *   Richard D. Ray, Svetlana Y. Erofeeva "Long-period tidal variations *
! *   in the length of day", (2014) J. Geophys. Res. Solid Earth,        *
! *   119, 1498--1509, doi:10.1002/2013JB010830.                         *
! *                                                                      *
! *   NB: Delta E3 (rad) = -1.002737909 * Delta UT1 * 2*PI/86400.0 (sec) *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *    MODE ( INTEGER*4 ) -- Mode of using expansion:                    *
! *                           0 -- all terms.                            *
! *                           1 -- terms with periods less than 35 days. *
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
! *  E3_DT2 ( REAL*8    ) -- Contribution to the angular acceleration    *
! *                          along the axis 3 due to zonal tides.        *
! *                          Units: rad/sec.                             *
! *                                                                      *
! * ### 24-AUG-2021  NERS_E3ZT_RE2014 v1.0 (c) L. Petrov 24-AUG-2021 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'e3zt_re2014.i'
      INTEGER*4  MODE, MJD
      REAL*8     TAI_SEC, E3, E3_DOT, E3_DT2
      INTEGER*4  J2000__MJD  
      REAL*8     J2000__JD, RATE_AMP_MIN
      PARAMETER  ( J2000__JD    = 2451545.0D0 ) ! 2000.01.01_12:00:00
      PARAMETER  ( J2000__MJD   = 51544       ) ! 2000.01.01_00:00:00
      PARAMETER  ( RATE_AMP_MIN = 1.D-14      ) ! rad/s
      REAL*8     TDT_ARG, ARG, ARG_DOT, RATE_AMP
      INTEGER*4  J1, IBEG, IEND
!
! --- Get time arguments: interval of time since J2000.0 reference epoch
!
      TDT_ARG = ( MJD - J2000__MJD )*86400.0D0 + ( TAI_SEC - 43200.0D0 ) + &
     &          32.184D0
      E3     = 0.0D0
      E3_DOT = 0.0D0
      E3_DT2 = 0.0D0
!
! --- Summing over all consituents
!
      IF ( MODE == 0  ) THEN
           IBEG = 1 
           IEND = N_RE2014
         ELSE IF ( MODE ==  1 ) THEN
           IBEG = 1
           IEND = 58 
      END IF
!
      DO 410 J1=IBEG,IEND
         ARG = (0.5D0*ACCL_RE2014(J1)*TDT_ARG + &
     &          FREQ_RE2014(J1))*TDT_ARG + &
     &          PHAS_RE2014(J1)
         ARG_DOT = ACCL_RE2014(J1)*TDT_ARG + FREQ_RE2014(J1)
         E3      = E3     + E3C_RE2014(J1)*DCOS(ARG) &
     &                    + E3S_RE2014(J1)*DSIN(ARG)
         E3_DOT  = E3_DOT + ARG_DOT* &
     &                   ( - E3C_RE2014(J1)*DSIN(ARG) &
     &                     + E3S_RE2014(J1)*DCOS(ARG) )
         E3_DT2  = E3_DT2 + ARG_DOT**2* &
     &                   ( - E3C_RE2014(J1)*DCOS(ARG) &
     &                     - E3S_RE2014(J1)*DSIN(ARG) )
 410  CONTINUE 
!
      RETURN
      END  SUBROUTINE  NERS_E3ZT_RE2014  !#!#
