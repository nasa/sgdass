      SUBROUTINE NERS_E3ZT_DICKMAN1993 ( MODE, MJD, TAI_SEC, E3, &
     &                                   E3_DOT, E3_DT2 ) 
! ************************************************************************
! *                                                                      *
! *   Routine NERS_E3ZT_DICKMAN1993 computes contribution of zonal solid *
! *   Earth tides and ocean tides to the Euler angle around the axis 3   *
! *   as well as its time derivative. Expansion of Dickman 1993 is used. *
! *   Dickman, S.R, "Dynamic ocean-tide effects on Earth's rotation",    *
! *   Geophys. J. Int., vol. 112, pp. 448-470, 1993.                     *
! *                                                                      *
! *   NB: Delta E3 (rad) = -1.002737909 * Delta UT1 * 2*PI/86400.0 (sec) *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *    MODE ( INTEGER*4 ) -- Mode of using expansion:                    *
! *                           0 -- all terms.                            *
! *                           1 -- only the principal term.              *
! *                           2 -- only the long-periodic terms.         *
! *                          11 -- all, but the principal term.          *
! *                          12 -- all, but the long-periodic terms,     *
! *                                i.e. only short-periodic terms.       *
! *                          13 -- the princial term and 14 terms        *
! *                                with the amplitude of the first       *
! *                                derivatives exceeding 1.d-14 rad/s    *
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
! * # 22-MAR-2004 NERS_E3ZT_DICKMAN1993 v2.1 (c) L. Petrov 21-AUG-2006 # *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'e3zt_dickman1993.i'
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
      IF ( MODE == 0  .OR.  MODE == 13 ) THEN
           IBEG = 1 
           IEND = N_DICKMAN1993
         ELSE IF ( MODE ==  1 ) THEN
           IBEG = E3Z_DICKMAN1993_PRI_BEG 
           IEND = E3Z_DICKMAN1993_PRI_END 
         ELSE IF ( MODE ==  2 ) THEN
           IBEG = E3Z_DICKMAN1993_LNG_BEG 
           IEND = E3Z_DICKMAN1993_LNG_END 
         ELSE IF ( MODE == 11 ) THEN
           IBEG = 1
           IEND = E3Z_DICKMAN1993_PRI_BEG - 1
         ELSE IF ( MODE == 12 ) THEN
           IBEG = E3Z_DICKMAN1993_SHR_BEG 
           IEND = E3Z_DICKMAN1993_SHR_END 
      END IF
      DO 410 J1=IBEG,IEND
         IF ( MODE == 13 ) THEN
              IF ( J1 == E3Z_DICKMAN1993_PRI_BEG ) THEN
                   CONTINUE 
                 ELSE 
                   RATE_AMP = FREQ_DICKMAN1993(J1)* &
     &                        DSQRT ( E3C_DICKMAN1993(J1)**2 + &
     &                                E3S_DICKMAN1993(J1)**2 ) 
                   IF ( RATE_AMP < RATE_AMP_MIN ) GOTO 410
              END IF
         END IF
!
         ARG = (0.5D0*ACCL_DICKMAN1993(J1)*TDT_ARG + &
     &          FREQ_DICKMAN1993(J1))*TDT_ARG + &
     &          PHAS_DICKMAN1993(J1)
         ARG_DOT = ACCL_DICKMAN1993(J1)*TDT_ARG + FREQ_DICKMAN1993(J1)
         E3      = E3     + E3C_DICKMAN1993(J1)*DCOS(ARG) &
     &                    + E3S_DICKMAN1993(J1)*DSIN(ARG)
         E3_DOT  = E3_DOT + ARG_DOT* &
     &                   ( - E3C_DICKMAN1993(J1)*DSIN(ARG) &
     &                     + E3S_DICKMAN1993(J1)*DCOS(ARG) )
         E3_DT2  = E3_DT2 + ARG_DOT**2* &
     &                   ( - E3C_DICKMAN1993(J1)*DCOS(ARG) &
     &                     - E3S_DICKMAN1993(J1)*DSIN(ARG) )
 410  CONTINUE 
!
      RETURN
      END  SUBROUTINE  NERS_E3ZT_DICKMAN1993  !##
