      SUBROUTINE HEO_REN2000 ( IPAR, TARG_TDB, UT1_M_TDB, E1, E2, DPSI, DEPS, &
     &                         E1_RATE, E2_RATE, DPSI_RATE, DEPS_RATE, &
     &                         CROSS_NUT_E3  )
! ************************************************************************
! *                                                                      *
! *   Routine  HEO_REN2000  computes angles of small rotation of the     *
! *   coordinate transformation from a celestial coordinate system to    *
! *   the terrestrial coordinate system: E1, E2, as well as nutation     *
! *   angles DPSI, DEPS according to the HEO_REN2000 nutation expansion. *
! *   This routine computes rate of change of E1,E2 or DEPS,DPSI         *
! *   as well.                                                           *
! *                                                                      *
! *   References:                                                        *
! *                                                                      *
! *   J. Souchay and H. Kinioshita, "Corrections and new developments    *
! *      in rigid Earth nutation theory: Lunisolar influence including   *
! *      indirect planetary effects", Astron. and Astrophys., 1995.      *
! *
! *   Souchay, J., and H. Kinoshita, "Corrections and new developments   *
! *      in rigid Earth nutation theory: I. Lunisolar influence          *
! *      including indirect planetary effects", Astron. Astrophys.,      *
! *      312, 1017--1030, 1996.                                          *
! *   Souchay, J., and H. Kinoshita, "Corrections and new developments   *
! *      in rigid Earth nutation theory: II. Influence of second-order   *
! *      geopotential and direct planetray effect", Astron. Astrophys.,  *
! *      318, 639--652, 1997.                                            *
! *   Souchay, J., B. Loysel, H, Kinoshita, and M. Folgueira,            *
! *      "Corrections and new developments in rigid Earth nutation       *
! *      theory: III. final tables REN-2000 including crossed-nutation   *
! *      and spin-orbit coupling effects", Astron. Astrophys. Suppl.,    *
! *      135, 111-131, 1999.                                             *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *   IPAR     ( INTEGER*4 ) -- Mode of computations:                    *
! *                          IPAR = 1 Angles E1 and E2 are computed      *
! *                                   using full expansion.              *
! *                          IPAR = 2 Angles E1 and E2 are computed      *
! *                                   using terms with the amplitudes    *
! *                                   greater than some limit.           *
! *                          IPAR = 3 Angles E1 and E2 are computed      *
! *                                   using terms with the amplitudes    *
! *                                   less than some limit.              *
! *                          IPAR = 4 Nutation angles DPSI, DEPS are     *
! *                                   computed using full expansion.     *
! *                          IPAR = 5 Nutation angles DPSI, DEPS are     *
! *                                   computed using the terms with      *
! *                                   amplitudes greater than some limit.*
! *                          IPAR = 6 Nutation angles DPSI, DEPS are     *
! *                                   computed using the terms with      *
! *                                   amplitudes less than some limit.   *
! *   TARG_TDB ( REAL*8    ) -- Argument: TDB (Time Dynamic Barycentric) *
! *  UT1_M_TDB ( REAL*8    ) -- The differences UT1 minus TDB in secs.   *
! *                                                                      *
! * _________________________ Output parameters: _______________________ *
! *                                                                      *
! *   E1       ( REAL*8    ) -- Rotation angle with respect to the axis  *
! *                             1 of the transformation from the         *
! *                             celestial frame to terrestrial frame.    *
! *                             Units: rad. This parameter is computed,  *
! *                             if IPAR=1,2 or 3, otherwise 0.           *
! *   E2       ( REAL*8    ) -- Rotation angle with respect to the axis  *
! *                             2 of the transformation from the         *
! *                             celestial frame to terrestrial frame.    *
! *                             Units: rad. This parameter is computed,  *
! *                             if IPAR=1,2 or 3, otherwise 0.           *
! *   DPSI     ( REAL*8    ) -- Angle of nutation in longitude.          *
! *                             Units: rad. This parameter is computed,  *
! *                             if IPAR=4,5 or 6, otherwise 0.           *
! *   DEPS     ( REAL*8    ) -- Angle of nutation in obliquity.          *
! *                             Units: rad. This parameter is computed,  *
! *                             if IPAR=4,5 or 6, otherwise 0.           *
! *   E1_RATE  ( REAL*8    ) -- Rate of change of E1. Units: rad/s.      *
! *                             This parameter is computed, if IPAR=1,2  *
! *                             or 3, otherwise 0.                       *
! *   E2_RATE  ( REAL*8    ) -- Rate of change of E2. Units: rad/s.      *
! *                             This parameter is computed, if IPAR=1,2  *
! *                             or 3, otherwise 0.                       *
! * DPSI_RATE  ( REAL*8    ) -- Rate of change of DPSI. Units: rad/s.    *
! *                             This parameter is computed, if IPAR=4,5  *
! *                             or 6, otherwise 0.                       *
! * DEPS_RATE  ( REAL*8    ) -- Rate of change of DEPS. Units: rad/s.    *
! *                             This parameter is computed, if IPAR=4,5  *
! *                             or 6, otherwise 0.                       *
! * CROSS_NUT_E3 ( REAL*8  ) -- Contribution to Euler angle 3 of the     *
! *                             Earth  rotation due to the secular part  *
! *                             of cross nutation-nutation terms.        *
! *                             Unit: rad.                               *
! *                                                                      *
! *  ###  06-OCT-2003  HEO_REN2000  v3.1 (c)  L. Petrov 05-SEP-2006 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'nut_const.i'
      INCLUDE   'ren2000_heo.i'
      INTEGER*4  IPAR
      REAL*8     TARG_TDB, UT1_M_TDB, E1, E2, DPSI, DEPS, &
     &           E1_RATE, E2_RATE, DPSI_RATE, DEPS_RATE, CROSS_NUT_E3
      REAL*8     AMP_LIM, AMP_LIM_SQ, AMP_TERM_SQ, SIN_EPS_0, EARTH_PHS, ARG, VEL
      PARAMETER  ( AMP_LIM = 1.D-7 ) !  rad
      INTEGER*4  J1
!
      SIN_EPS_0 = DSIN ( EPSILON_0 )
      AMP_LIM_SQ = (1.D12*AMP_LIM)**2
!
      DEPS = 0.0D0
      DPSI = 0.0D0
      E1   = 0.0D0
      E2   = 0.0D0
!
      DEPS_RATE = 0.0D0
      DPSI_RATE = 0.0D0
      E1_RATE   = 0.0D0
      E2_RATE   = 0.0D0
!
! --- Compute the Earth's phase
!
      EARTH_PHS = PI + S0_EARTH + UT1_M_TDB*PI2/86400.0D0
!
! --- Cycle over nutation constituents
!
      DO 410 J1=1,N_NUT
         IF ( IPAR .EQ. 1  .OR.  IPAR .EQ. 2  .OR.  IPAR .EQ. 3 ) THEN
!
! ----------- Compute the argument of perturbed rotation
!
              ARG = (0.5D0*ACCL_NUT(J1)*TARG_TDB + FREQ_NUT(J1))*TARG_TDB + &
     &               PHAS_NUT(J1) - EARTH_PHS
              VEL =  ACCL_NUT(J1)*TARG_TDB + FREQ_NUT(J1)
!
              IF ( IPAR .EQ. 2 .OR. IPAR .EQ. 3 ) THEN
!
! ---------------- Check the square of the amplitude
!
                   AMP_TERM_SQ = PMC_NUT(J1)**2 + PMS_NUT(J1)**2
                   IF ( IPAR .EQ. 2 ) THEN
!
! --------------------- Is the amplitude too small?
!
                        IF ( AMP_TERM_SQ .LE. AMP_LIM_SQ ) GOTO 410
                      ELSE IF ( IPAR .EQ. 3 ) THEN
!
! --------------------- Is the amplpitude too large?
!
                        IF ( AMP_TERM_SQ .GT. AMP_LIM_SQ ) GOTO 410
                   END IF
              END IF
!
              E1 = E1 &
     &           + (PMC_NUT(J1) + 1.D-12*PMC_RATE_NUT(J1)*TARG_TDB)*DCOS(ARG) &
     &           + (PMS_NUT(J1) + 1.D-12*PMS_RATE_NUT(J1)*TARG_TDB)*DSIN(ARG)
              E2 = E2 &
     &           + (PMC_NUT(J1) + 1.D-12*PMC_RATE_NUT(J1)*TARG_TDB)*DSIN(ARG) &
     &           - (PMS_NUT(J1) + 1.D-12*PMS_RATE_NUT(J1)*TARG_TDB)*DCOS(ARG)
!
! ----------- ... and their rate of change
!
              E1_RATE = E1_RATE &
     &          -VEL*(PMC_NUT(J1) + 1.D-12*PMC_RATE_NUT(J1)*TARG_TDB)*DSIN(ARG) &
     &          + 1.D-12*PMC_RATE_NUT(J1)*DCOS(ARG)                             &
     &          +VEL*(PMS_NUT(J1) + 1.D-12*PMS_RATE_NUT(J1)*TARG_TDB)*DCOS(ARG) &
     &          + 1.D-12*PMS_RATE_NUT(J1)*DSIN(ARG)
!
              E2_RATE = E2_RATE &
     &          +VEL*(PMC_NUT(J1) + 1.D-12*PMC_RATE_NUT(J1)*TARG_TDB)*DCOS(ARG) &
     &          + 1.D-12*PMC_RATE_NUT(J1)*DSIN(ARG)                             &
     &          +VEL*(PMS_NUT(J1) + 1.D-12*PMS_RATE_NUT(J1)*TARG_TDB)*DSIN(ARG) &
     &          - 1.D-12*PMS_RATE_NUT(J1)*DCOS(ARG)
            ELSE IF ( IPAR .EQ. 4  .OR.  IPAR .EQ. 5  .OR. IPAR .EQ. 6 ) THEN
!
! ----------- Arugument for nutation
!
!@              ARG = (0.5D0*ACCL_NUT(J1)*TARG_TDB + OM_NM + OM_PRC + &
!@     &               FREQ_NUT(J1))*TARG_TDB + PHAS_NUT(J1)
              ARG = (OM_NM + OM_PRC + FREQ_NUT(J1))*TARG_TDB + PHAS_NUT(J1)
              VEL =  ACCL_NUT(J1)*TARG_TDB + OM_NM + OM_PRC + FREQ_NUT(J1)
              IF ( IPAR .EQ. 5 .OR. IPAR .EQ. 6 ) THEN
                   AMP_TERM_SQ = PMC_NUT(J1)**2 + PMS_NUT(J1)**2
                   IF ( IPAR .EQ. 5 ) THEN
!
! --------------------- Is the amplitude too small?
!
                        IF ( AMP_TERM_SQ .LE. AMP_LIM_SQ ) GOTO 410
                      ELSE IF ( IPAR .EQ. 6 ) THEN
!
! --------------------- Is the amplpitude too large?
!
                        IF ( AMP_TERM_SQ .GT. AMP_LIM_SQ ) GOTO 410
                   END IF
              END IF
!
! ----------- Compute contribution to nutation angles
!
              DEPS = DEPS  &
     &          + (PMC_NUT(J1) + 1.D-12*PMC_RATE_NUT(J1)*TARG_TDB)*DCOS(ARG) &
     &          + (PMS_NUT(J1) + 1.D-12*PMS_RATE_NUT(J1)*TARG_TDB)*DSIN(ARG)
              DPSI = DPSI  &
     &          - (PMC_NUT(J1) + 1.D-12*PMC_RATE_NUT(J1)*TARG_TDB)*DSIN(ARG) &
     &          + (PMS_NUT(J1) + 1.D-12*PMS_RATE_NUT(J1)*TARG_TDB)*DCOS(ARG)
!
! ----------- Compute contribution to nutation angles rate of change
!
              DEPS_RATE = DEPS_RATE  &
     &          -VEL*(PMC_NUT(J1) + 1.D-12*PMC_RATE_NUT(J1)*TARG_TDB)*DSIN(ARG) &
     &          +1.D-12*PMC_RATE_NUT(J1)*DCOS(ARG)                              &
     &          +VEL*(PMS_NUT(J1) + 1.D-12*PMS_RATE_NUT(J1)*TARG_TDB)*DCOS(ARG) &
     &          +1.D-12*PMS_RATE_NUT(J1)*DSIN(ARG)
              DPSI_RATE = DPSI_RATE  &
     &          -VEL*(PMC_NUT(J1) + 1.D-12*PMC_RATE_NUT(J1)*TARG_TDB)*DCOS(ARG) &
     &          -1.D-12*PMC_RATE_NUT(J1)*DSIN(ARG)                              &
     &          -VEL*(PMS_NUT(J1) + 1.D-12*PMS_RATE_NUT(J1)*TARG_TDB)*DSIN(ARG) &
     &          +1.D-12*PMS_RATE_NUT(J1)*DCOS(ARG)
         END IF
 410  CONTINUE
!
! --- convert angles ot SI units
!
      IF ( IPAR .EQ. 1  .OR.  IPAR .EQ. 2  .OR.  IPAR .EQ. 3 ) THEN
           E1 = E1*1.D-12
           E2 = E2*1.D-12
           E1_RATE = E1_RATE*1.D-12
           E2_RATE = E2_RATE*1.D-12
         ELSE IF ( IPAR .EQ. 4  .OR.  IPAR .EQ. 5  .OR.  IPAR .EQ. 6  ) THEN
           DEPS = DEPS*1.D-12
           DPSI = DPSI*1.D-12/SIN_EPS_0
           DEPS_RATE = DEPS_RATE*1.D-12
           DPSI_RATE = DPSI_RATE*1.D-12/SIN_EPS_0
      END IF
!
      CROSS_NUT_E3 = CROSS_NUT_RATE_E3*TARG_TDB
!
      RETURN
      END  SUBROUTINE  HEO_REN2000  !#!#
