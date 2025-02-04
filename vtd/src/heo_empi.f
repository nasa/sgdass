      SUBROUTINE HEO_EMPI ( IPAR, EXP_NAME, TARG_TDB, UT1_M_TDB, &
     &                      E1, E2, DPSI, DEPS, E1_RATE, E2_RATE, &
     &                      DPSI_RATE, DEPS_RATE, CROSS_NUT_E3 )
! ************************************************************************
! *                                                                      *
! *   Routine  HEO_EMPI  computes angles of small rotation of the        *
! *   coordinate transformation from a celestial coordinate system to    *
! *   the terrestrial coordinate system: E1, E2, as well as nutation     *
! *   angles DPSI, DEPS according to the empirical nutation expansion    *
! *   EXP_NAME.                                                          *
! *                                                                      *
! *   This routine computes rate of change of E1,E2 or DEPS,DPSI as well.*
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
! *   EXP_NAME ( INTEGER*4 ) -- Expansion name. Two expansions are       *
! *                             supported now:                           *
! *                             NUT__PETA -- only three terms in         *
! *                                          nutation, pro and retro     *
! *                                          printcipal, retro           *
! *                                          semi-annual.                *
! *                             NUT__PETB,                               *
! *                             NUT__PETC -- REN2000, only three terms,  *
! *                                          angular acceleration        *
! *                                          is switched off.            *
! *   TARG_TDB ( REAL*8    ) -- Argument: TDB (Time Dynamic Barycentric) *
! *  UT1_M_TDB ( REAL*8    ) -- The differences UT1 minus TDB in secs.   *
! *                                                                      *
! * _________________________ Output parameters: _______________________ *
! *                                                                      *
! * <HEO_EMPI> ( REAL*8    ) -- Contribution to Euler angle 3 of the     *
! *                             Earth rotation due to the                *
! *                             secular part of cross                    *
! *                             nutation-nutation terms. Unit: rad.      *
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
! *  ###  06-OCT-2003    HEO_EMPI   v3.2 (c) L. Petrov  15-SEP-2006 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'nut_const.i'
      INCLUDE   'heo.i'
      INCLUDE   'heo_nut_peta.i'
      INCLUDE   'heo_nut_petb.i'
      INTEGER*4  IPAR, EXP_NAME
      REAL*8     TARG_TDB, UT1_M_TDB, E1, E2, DPSI, DEPS, E1_RATE, E2_RATE, &
     &           DPSI_RATE, DEPS_RATE, CROSS_NUT_E3 
      REAL*8     AMP_LIM, AMP_LIM_SQ, SIN_EPS_0, EARTH_PHS, AMP_TERM_SQ, &
     &           ARG, VEL
      PARAMETER  ( AMP_LIM = 1.D-6 ) !  rad
      REAL*8     PRC_IN, PRC_IN_RATE, PRC_OUT, PRC_OUT_RATE
      INTEGER*4  J1, N_NUT
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
      IF ( EXP_NAME == NUT__PETA ) THEN
           N_NUT = N_NUT_PETA
        ELSE IF ( EXP_NAME == NUT__PETB ) THEN
           N_NUT = N_NUT_PETB
        ELSE IF ( EXP_NAME == NUT__PETC ) THEN
           N_NUT = N_NUT_PETC
        ELSE
           WRITE ( 6, * ) 'EXP_NAME = ', EXP_NAME
           CALL ERR_LOG ( 911, -3, 'HEO_EMPI', 'Trap of internal control: '// &
     &         'unsupported value of EXP_NAME' )
           CALL EXIT ( 1 )
      END IF
!
      DO 410 J1=1,N_NUT
         IF ( IPAR .EQ. 1  .OR.  IPAR .EQ. 2  .OR.  IPAR .EQ. 3 ) THEN
!
! ----------- Compute the argument of perturbed rotation
!
              IF ( EXP_NAME == NUT__PETA ) THEN
                   ARG = (0.5D0*ACCL_NUT_PETA(J1)*TARG_TDB + &
     &                    FREQ_NUT_PETA(J1))*TARG_TDB + &
     &                    PHAS_NUT_PETA(J1) - EARTH_PHS
                   VEL =  ACCL_NUT_PETA(J1)*TARG_TDB + FREQ_NUT_PETA(J1)
                ELSE IF ( EXP_NAME == NUT__PETB ) THEN
                   ARG = (0.5D0*ACCL_NUT_PETB(J1)*TARG_TDB + &
     &                    FREQ_NUT_PETB(J1))*TARG_TDB + &
     &                    PHAS_NUT_PETB(J1) - EARTH_PHS
                   VEL =  ACCL_NUT_PETB(J1)*TARG_TDB + FREQ_NUT_PETB(J1)
                ELSE IF ( EXP_NAME == NUT__PETC ) THEN
                   ARG =  FREQ_NUT_PETC(J1)*TARG_TDB + &
     &                    PHAS_NUT_PETC(J1) - EARTH_PHS
                   VEL =  FREQ_NUT_PETC(J1)
              END IF
!
              IF ( IPAR .EQ. 2 .OR. IPAR .EQ. 3 ) THEN
!
! ---------------- Check the square of the amplitude
!
!
                   IF ( EXP_NAME == NUT__PETA ) THEN
                        AMP_TERM_SQ = PMC_NUT_PETA(J1)**2 + PMS_NUT_PETA(J1)**2
                      ELSE IF ( EXP_NAME == NUT__PETB ) THEN
                        AMP_TERM_SQ = PMC_NUT_PETB(J1)**2 + PMS_NUT_PETB(J1)**2
                      ELSE IF ( EXP_NAME == NUT__PETC ) THEN
                        AMP_TERM_SQ = PMC_NUT_PETC(J1)**2 + PMS_NUT_PETC(J1)**2
                   END IF
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
              IF ( EXP_NAME == NUT__PETA ) THEN
                   E1 = E1 &
     &                + PMC_NUT_PETA(J1)*DCOS(ARG) &
     &                + PMS_NUT_PETA(J1)*DSIN(ARG)
                   E2 = E2 &
     &                + PMC_NUT_PETA(J1)*DSIN(ARG) &
     &                - PMS_NUT_PETA(J1)*DCOS(ARG)
!
! ---------------- ... and their rate of change
!
                   E1_RATE =   E1_RATE &
     &                       - VEL*PMC_NUT_PETB(J1)*DSIN(ARG) &
     &                       + VEL*PMS_NUT_PETB(J1)*DCOS(ARG)
!
                   E2_RATE =   E2_RATE &
     &                       + VEL*PMC_NUT_PETB(J1)*DCOS(ARG) &
     &                       + VEL*PMS_NUT_PETB(J1)*DSIN(ARG)
                 ELSE IF ( EXP_NAME == NUT__PETB ) THEN
                   E1 = E1 &
     &                + PMC_NUT_PETB(J1)*DCOS(ARG) &
     &                + PMS_NUT_PETB(J1)*DSIN(ARG)
                   E2 = E2 &
     &                + PMC_NUT_PETB(J1)*DSIN(ARG) &
     &                - PMS_NUT_PETB(J1)*DCOS(ARG)
!
! ---------------- ... and their rate of change
!
                   E1_RATE =   E1_RATE &
     &                       - VEL*PMC_NUT_PETB(J1)*DSIN(ARG) &
     &                       + VEL*PMS_NUT_PETB(J1)*DCOS(ARG)
!
                   E2_RATE =   E2_RATE &
     &                       + VEL*PMC_NUT_PETB(J1)*DCOS(ARG) &
     &                       + VEL*PMS_NUT_PETB(J1)*DSIN(ARG)
                 ELSE IF ( EXP_NAME == NUT__PETC ) THEN
                   E1 = E1 &
     &                + PMC_NUT_PETC(J1)*DCOS(ARG) &
     &                + PMS_NUT_PETC(J1)*DSIN(ARG)
                   E2 = E2 &
     &                + PMC_NUT_PETC(J1)*DSIN(ARG) &
     &                - PMS_NUT_PETC(J1)*DCOS(ARG)
!
! ---------------- ... and their rate of change
!
                   E1_RATE =   E1_RATE &
     &                       - VEL*PMC_NUT_PETC(J1)*DSIN(ARG) &
     &                       + VEL*PMS_NUT_PETC(J1)*DCOS(ARG)
!
                   E2_RATE =   E2_RATE &
     &                       + VEL*PMC_NUT_PETC(J1)*DCOS(ARG) &
     &                       + VEL*PMS_NUT_PETC(J1)*DSIN(ARG)
              END IF
            ELSE IF ( IPAR .EQ. 4  .OR.  IPAR .EQ. 5  .OR. IPAR .EQ. 6 ) THEN
!
! ----------- Arugument for nutation
!
              IF ( EXP_NAME == NUT__PETA ) THEN
                   ARG = (0.5D0*ACCL_NUT_PETA(J1)*TARG_TDB + OM_NM + OM_PRC + &
     &                     FREQ_NUT_PETA(J1))*TARG_TDB + PHAS_NUT_PETA(J1)
                   VEL =  ACCL_NUT_PETA(J1)*TARG_TDB + OM_NM + OM_PRC + &
     &                    FREQ_NUT_PETA(J1)
                 ELSE IF ( EXP_NAME == NUT__PETB ) THEN
                   ARG = (0.5D0*ACCL_NUT_PETB(J1)*TARG_TDB + OM_NM + OM_PRC + &
     &                     FREQ_NUT_PETB(J1))*TARG_TDB + PHAS_NUT_PETB(J1)
                   VEL =  ACCL_NUT_PETB(J1)*TARG_TDB + OM_NM + OM_PRC + &
     &                    FREQ_NUT_PETB(J1)
                 ELSE IF ( EXP_NAME == NUT__PETC ) THEN
                   ARG = (OM_NM + OM_PRC + FREQ_NUT_PETC(J1))*TARG_TDB + &
     &                    PHAS_NUT_PETC(J1)
                   VEL =  OM_NM + OM_PRC + FREQ_NUT_PETC(J1)
              END IF
              IF ( IPAR .EQ. 5 .OR. IPAR .EQ. 6 ) THEN
                   IF ( EXP_NAME == NUT__PETA ) THEN
                        AMP_TERM_SQ = PMC_NUT_PETA(J1)**2 + PMS_NUT_PETA(J1)**2
                     ELSE IF ( EXP_NAME == NUT__PETB ) THEN
                        AMP_TERM_SQ = PMC_NUT_PETB(J1)**2 + PMS_NUT_PETB(J1)**2
                     ELSE IF ( EXP_NAME == NUT__PETC ) THEN
                        AMP_TERM_SQ = PMC_NUT_PETC(J1)**2 + PMS_NUT_PETC(J1)**2
                   END IF
!
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
              IF ( EXP_NAME == NUT__PETA ) THEN
                   DEPS =   DEPS  &
     &                    + PMC_NUT_PETA(J1)*DCOS(ARG) &
     &                    + PMS_NUT_PETA(J1)*DSIN(ARG)
                   DPSI =   DPSI  &
     &                    - PMC_NUT_PETA(J1)*DSIN(ARG) &
     &                    + PMS_NUT_PETA(J1)*DCOS(ARG)
!
! ---------------- Compute contribution to nutation angles rate of change
!
                   DEPS_RATE =   DEPS_RATE  &
     &                         - VEL*PMC_NUT_PETA(J1)*DSIN(ARG) &
     &                         + VEL*PMS_NUT_PETA(J1)*DCOS(ARG)
                   DPSI_RATE =   DPSI_RATE  &
     &                         - VEL*PMC_NUT_PETA(J1)*DCOS(ARG) &
     &                         - VEL*PMS_NUT_PETA(J1)*DSIN(ARG)
                 ELSE IF ( EXP_NAME == NUT__PETB ) THEN
                   DEPS =   DEPS  &
     &                    + PMC_NUT_PETB(J1)*DCOS(ARG) &
     &                    + PMS_NUT_PETB(J1)*DSIN(ARG)
                   DPSI =   DPSI  &
     &                    - PMC_NUT_PETB(J1)*DSIN(ARG) &
     &                    + PMS_NUT_PETB(J1)*DCOS(ARG)
!
! ---------------- Compute contribution to nutation angles rate of change
!
                   DEPS_RATE =   DEPS_RATE  &
     &                         - VEL*PMC_NUT_PETB(J1)*DSIN(ARG) &
     &                         + VEL*PMS_NUT_PETB(J1)*DCOS(ARG)
                   DPSI_RATE =   DPSI_RATE  &
     &                         - VEL*PMC_NUT_PETB(J1)*DCOS(ARG) &
     &                         - VEL*PMS_NUT_PETB(J1)*DSIN(ARG)
                 ELSE IF ( EXP_NAME == NUT__PETC ) THEN
                   DEPS =   DEPS  &
     &                    + PMC_NUT_PETC(J1)*DCOS(ARG) &
     &                    + PMS_NUT_PETC(J1)*DSIN(ARG)
                   DPSI =   DPSI  &
     &                    - PMC_NUT_PETC(J1)*DSIN(ARG) &
     &                    + PMS_NUT_PETC(J1)*DCOS(ARG)
!
! ---------------- Compute contribution to nutation angles rate of change
!
                   DEPS_RATE =   DEPS_RATE  &
     &                         - VEL*PMC_NUT_PETC(J1)*DSIN(ARG) &
     &                         + VEL*PMS_NUT_PETC(J1)*DCOS(ARG)
                   DPSI_RATE =   DPSI_RATE  &
     &                         - VEL*PMC_NUT_PETC(J1)*DCOS(ARG) &
     &                         - VEL*PMS_NUT_PETC(J1)*DSIN(ARG)
              END IF
         END IF
 410  CONTINUE
!
      IF ( EXP_NAME == NUT__PETA ) THEN
!
! -------- Convert angles ot SI units
!
           IF ( IPAR .EQ. 1  .OR.  &
     &          IPAR .EQ. 2  .OR.  &
     &          IPAR .EQ. 3        ) THEN
!
                E1 = E1*1.D-12
                E2 = E2*1.D-12
                E1_RATE = E1_RATE*1.D-12
                E2_RATE = E2_RATE*1.D-12
              ELSE IF ( IPAR .EQ. 4  .OR.  &
     &                  IPAR .EQ. 5  .OR.  &
     &                  IPAR .EQ. 6        ) THEN !
!
                DEPS = DEPS*1.D-12
                DPSI = DPSI*1.D-12/SIN_EPS_0
                DEPS_RATE = DEPS_RATE*1.D-12
                DPSI_RATE = DPSI_RATE*1.D-12/SIN_EPS_0
           END IF
           CROSS_NUT_E3 = CROSS_NUT_PETA_RATE_E3*TARG_TDB
        ELSE IF ( EXP_NAME == NUT__PETB ) THEN
!
! -------- Apply empirical correction due to precession rate, obliquity rate
! -------- and CEP coordinates, We have also to convert previously computed
! -------- angles to SI units
!
           ARG = -(OM_NM+OM_PRC)*TARG_TDB - EARTH_PHS
           VEL = -(OM_NM+OM_PRC)
           IF ( IPAR .EQ. 1  .OR.  IPAR .EQ. 3 ) THEN
!
! ------------- Compute amplitudes of in-phase and out-of-phase offsets and
! ------------- secular changes of nutation angles
!
                PRC_IN       = EPS_OFFS_MHB2000*MAS_TO_RAD
                PRC_IN_RATE  = EPS_RATE_MHB2000/(86400.0D0*365.25D0)*MAS_TO_RAD
                PRC_OUT      = PSI_OFFS_MHB2000*SIN_EPS_0*MAS_TO_RAD
                PRC_OUT_RATE = PSI_RATE_MHB2000*SIN_EPS_0/(86400.0D0*365.25D0)* &
     &                         MAS_TO_RAD
                E1 = E1*1.D-12   &
     &               + ( PRC_IN  + PRC_IN_RATE*TARG_TDB  )*DCOS(ARG)  &
     &               + ( PRC_OUT + PRC_OUT_RATE*TARG_TDB )*DSIN(ARG)
                E2 = E2*1.D-12   &
     &               + ( PRC_IN  + PRC_IN_RATE*TARG_TDB  )*DSIN(ARG)  &
     &               - ( PRC_OUT + PRC_OUT_RATE*TARG_TDB )*DCOS(ARG)
!
                E1_RATE = E1_RATE*1.D-12 &
     &                    - VEL*( PRC_IN  + PRC_IN_RATE*TARG_TDB  )*DSIN(ARG)  &
     &                    + PRC_IN_RATE*TARG_TDB*DCOS(ARG)                     &
     &                    + VEL*( PRC_OUT + PRC_OUT_RATE*TARG_TDB )*DCOS(ARG)  &
     &                    + PRC_OUT_RATE*TARG_TDB*DSIN(ARG)
!
                E2_RATE = E2_RATE*1.D-12 &
     &                    + VEL*( PRC_IN  + PRC_IN_RATE*TARG_TDB  )*DCOS(ARG)  &
     &                    + PRC_IN_RATE*DSIN(ARG)                              &
     &                    + VEL*( PRC_OUT + PRC_OUT_RATE*TARG_TDB )*DSIN(ARG)  &
     &                    - PRC_OUT_RATE*DCOS(ARG)
              ELSE IF ( IPAR .EQ. 2  ) THEN
!
! ------------- No contribution due to precession and obliquity rate is applied
!
                E1 = E1*1.D-12
                E2 = E2*1.D-12
                E1_RATE = E1_RATE*1.D-12
                E2_RATE = E2_RATE*1.D-12
              ELSE IF ( IPAR .EQ. 4  .OR.  IPAR .EQ. 6  ) THEN
                DEPS = DEPS*1.D-12 + &
     &               ( TARG_TDB*EPS_RATE_MHB2000/(86400.0D0*365.25D0) + &
     &                 EPS_OFFS_MHB2000 ) * MAS_TO_RAD
                DPSI = DPSI*1.D-12/SIN_EPS_0  + &
     &               ( TARG_TDB*PSI_RATE_MHB2000/(86400.0D0*365.25D0) + &
     &                 PSI_OFFS_MHB2000 )* MAS_TO_RAD
!
                DEPS_RATE = DEPS_RATE*1.D-12 + &
     &               EPS_RATE_MHB2000/(86400.0D0*365.25D0)*MAS_TO_RAD
                DPSI_RATE = DPSI_RATE*1.D-12/SIN_EPS_0  + &
     &               PSI_RATE_MHB2000/(86400.0D0*365.25D0)*MAS_TO_RAD
              ELSE IF ( IPAR .EQ. 5 ) THEN
!
! ------------- No contribution due to precession and obliquity rate is applied
!
                DEPS = DEPS*1.D-12
                DPSI = DPSI*1.D-12/SIN_EPS_0
                DEPS_RATE = DEPS_RATE*1.D-12
                DPSI_RATE = DPSI_RATE*1.D-12/SIN_EPS_0
           END IF
           CROSS_NUT_E3 = CROSS_NUT_PETA_RATE_E3*TARG_TDB
        ELSE IF ( EXP_NAME == NUT__PETC ) THEN
!
! -------- Convert angles to SI units
!
           IF ( IPAR .EQ. 1  .OR.  &
     &          IPAR .EQ. 2  .OR.  &
     &          IPAR .EQ. 3        ) THEN
!
                E1 = E1*1.D-12
                E2 = E2*1.D-12
                E1_RATE = E1_RATE*1.D-12
                E2_RATE = E2_RATE*1.D-12
              ELSE IF ( IPAR .EQ. 4  .OR.  &
     &                  IPAR .EQ. 5  .OR.  &
     &                  IPAR .EQ. 6        ) THEN !
!
                DEPS = DEPS*1.D-12
                DPSI = DPSI*1.D-12/SIN_EPS_0
                DEPS_RATE = DEPS_RATE*1.D-12
                DPSI_RATE = DPSI_RATE*1.D-12/SIN_EPS_0
           END IF
           CROSS_NUT_E3 = CROSS_NUT_PETC_RATE_E3*TARG_TDB
      END IF
!
      RETURN
      END  SUBROUTINE  HEO_EMPI  !#!#
