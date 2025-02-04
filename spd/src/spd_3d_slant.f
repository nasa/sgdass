      SUBROUTINE SPD_3D_SLANT ( SPD, VEC_GROUND_XYZ, MAT_XI_TO_X3, AZ, EL, &
     &                          NH, XI, RFR_VAL, RFR_DER, DELS, OPA, TAT, &
     &                          IVRB, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  SPD_3D_SLANT  computes the path delay through the 3D      *
! *   heterogeneous refractivity field by integrating the refractivity   *
! *   among the wave trajectory. The wave trajectory is found by solving *
! *   a system of two differential equations of the second order that    *
! *   provides the solution of the variational problem of finding the    *
! *   trajectory that minimizes the propagation time in the form of      *
! *   Euler equations. The coordinate system xi,y,z is set the following *
! *   way: the origin is placed in the receiver. The xi axis is directed *
! *   towards the emitter that is supposed to be at the infinity.        *
! *   The y axis lies in the plane of xi axis and the Earth pole. The    *
! *   z axis is perpendicular to both xi and y: z = xi cross y.          *
! *   The refractivity field is presented in the form of the array       *
! *   RFR_VAL at knots XI and its partial derivatives with respect to    *
! *   xi, y and z.                                                       *
! *                                                                      *
! *   The trajectory is sought in the form of an expansion over the      *
! *   B-spline of the 3rd order. The iterative relaxation scheme is      *
! *   employed: at the first iteration the trajectory is considered to   *
! *   be xi. The preliminary solution is the form of expansion over the  *
! *   B-spline basis is substituted to the system of differential        *
! *   equations, and this substitution transforms the system of          *
! *   differential equations to the system of algebraic equations,       *
! *   coefficients being dependent on the initial solution. Solving      *
! *   the system of algebraic equations, we get a better approximation.  *
! *   In practice two iterations is sufficient to reach the accuracy     *
! *   better than 1.D-6.                                                 *
! *                                                                      *
! *   The procedure SPD_3D_SLANT has 5 options. The rigorous solution    *
! *   gives an excessive accuracy that is much higher than the accuracy  *
! *   of the refractivity field. Simplified algorithms run faster and    *
! *   provides accuracy sufficient for geodetic applications.            *
! *                                                                      *
! *   SPD__ALG_NONLOC      -- the rigorous non-linear 3D equations.      *
! *   SPD__ALG_NONLIN_LOC  -- nonliner 3D equations, but partial         *
! *                           derivatives refractivity of are considered *
! *                           only function of xi, not y and z.          *
! *                           Gives an error up to 5 psec at 5 deg       *
! *                           elevation angle.                           *
! *   SPD__ALG_LIN_YZ      -- linear 3D equations.                       *
! *   SPD__ALG_LIN_Y       -- (fastest) linear 2D equations. Dependence  *
! *                           on z is ignored.                           *
! *                                                                      *
! *   After computing the trajectory, the function under the delay       *
! *   integral is computed: n*sqrt(1 + y'^2 + z'^2) - 1.                 *
! *   The interpolating B-spline is computed for this function in the    *
! *   range of [xi(1), xi(nh)]. Finally the integral over this function  *
! *   in this range is computed using the B-spline coefficients.         *
! *                                                                      *
! *   The array of refractivity along xi axis and its partial            *
! *   derivatives contain two components. The second component may be    *
! *   missing. The meaning of components is determined by parameter      *
! *   SPD%CONF%SPLIT_MODE. Two components of delay that correspond       *
! *   to these two components of the refractivity are computed.          *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *      SPD ( MET_GRID__TYPE ) -- Object with data structure of         *
! *                                     the  4D field of meteorological  *
! *                                     parameters.                      *
! * VEC_GROUND_XYZ ( REAL*8    ) -- XYZ coordinates of the receiver in   *
! *                                 the crust-fixed coordinate system.   *
! *                                 Dimension: 3. Units: meters.         *
! *  MAT_XI_TO_X3 ( REAL*8     ) -- 3x3 matrix of transformation from    *
! *                                 xi-coordinates to XYZ:               *
! *                                 R3(-LAM)*R2(lat_gdt)*R1(PI/2-AZ)*    *
! *                                 R3(EL-PI/2).                         *
! *            AZ ( REAL*8     ) -- azimuth of the line-of-sight from    *
! *                                 the station.                         *
! *            EL ( REAL*8     ) -- elevation (refraction-free) of the   *
! *                                 line-of-sight from the station.      *
! *            NH ( INTEGER*4  ) -- The number of knots of the B-spline  *
! *                                 expansion of refractivity along XI   *
! *                                 axis.                                *
! *            XI ( REAL*8     ) -- Array of knots along the XI axis.    *
! *                                 The first point corresponds to the   *
! *                                 receiver. The last point corresponds *
! *                                 to the top of the atmosphere in      *
! *                                 the direction to the emitter,        *
! *                                 undisturbed by refraction. Units: m. *
! *       RFR_VAL ( REAL*8     ) -- Array of two components of           *
! *                                 refractivity along XI as a function  *
! *                                 of xi. Dimension: NH,3. NB: One has  *
! *                                 been subtracted from refractivity.   *
! *       RFR_DER ( REAL*8     ) -- Array of two components of           *
! *                                 refractivity partial derivatives     *
! *                                 along XI as a function of xi.        *
! *                                 Dimension: NH,3,2. Units: 1/m.       *
! *     IVRB ( INTEGER*4      ) -- Verbosity level. 0 -- silent;         *
! *                                IVRB > 3 is for debugging.            *
! *                                                                      *
! * _________________________ Output parameters: _______________________ *
! *                                                                      *
! *     DELS ( REAL*8         ) -- Excess path delays through the        *
! *                                atmosphere relative to the the case   *
! *                                if the waves between the emitter and  *
! *                                received were propagated in vacuum.   *
! *                                One or two components are returned:   *
! *                                a) total path delay; and/or           *
! *                                b) hydrostatic path delay and/or      *
! *                                c) non-hydrostatic path delay.        *
! *                                The meaning of components is defined  *
! *                                by SPD%CONF%SPLIT_MODE.               *
! *                                Dimension: 2. Units: seconds.         *
! *      OPA ( REAL*8         ) -- Atmospheric opacity, i.e. optical     *
! *                                depth as a function of frequency.     *
! *                                Unitless (i.e. neper).                *
! *                                Dimension: SPD%NFRQ.                  *
! *      TAT ( REAL*8         ) -- Atmosphere brigthness temperature     *
! *                                as a function of frequency.           *
! *                                Units: Kelvin. Dimension: SPD%NFRQ.   *
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
! *     IUER ( INTEGER*4, OPT ) -- Universal error handler.              *
! *                           Input: switch IUER=0 -- no error messages  *
! *                                  will be generated even in the case  *
! *                                  of error. IUER=-1 -- in the case of *
! *                                  error the message will be put on    *
! *                                  stdout.                             *
! *                           Output: 0 in the case of successful        *
! *                                   completion and non-zero in the     *
! *                                   case of error.                     *
! *                                                                      *
! * ### 29-NOV-2008   SPD_3D_SLANT   v4.1 (c)  L. Petrov 01-JAN-2024 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'spd.i'
      TYPE     ( SPD_3D__TYPE ) :: SPD
      INTEGER*4  NH, IVRB, IUER
      REAL*8     XI(NH), VEC_GROUND_XYZ(3), MAT_XI_TO_X3(3,3), &
     &           AZ, EL, DELS(SPD__MTYP), RFR_VAL(SPD__MLEV,SPD__MTYP), &
     &           RFR_DER(SPD__MLEV,3), OPA(SPD__M_FRQ), TAT(SPD__M_FRQ)
      INTEGER*4    M_ITR
      PARAMETER  ( M_ITR = 2 )
      REAL*8     Y(SPD__MLEV),   Z(SPD__MLEV), &
     &           YD(SPD__MLEV),  ZD(SPD__MLEV), &
     &           YDD(SPD__MLEV), ZDD(SPD__MLEV)
      REAL*4     XI_R4(SPD__MLEV), DST_TOT_R4(1-SPD__MDEG:SPD__MLEV), &
     &           DST_WAT_R4(1-SPD__MDEG:SPD__MLEV)
      REAL*8     DST(SPD__MLEV), TMP_ARR(SPD__MLEV)
      REAL*8     NXN, NYN, NZN, D1, DN, R_VAL, RW_VAL, &
     &           RDX_VAL, RDY_VAL, RDZ_VAL, &
     &           MATB_Y((1+SPD__MDEG)*(SPD__MDEG+SPD__MLEV)), &
     &           MATB_Z((1+SPD__MDEG)*(SPD__MDEG+SPD__MLEV)), &
     &           RB_Y(1-SPD__MDEG:SPD__MLEV), RB_Z(1-SPD__MDEG:SPD__MLEV), &
     &           Y_SPL(1-SPD__MDEG:SPD__MLEV), Z_SPL(1-SPD__MDEG:SPD__MLEV), &
     &           EQU(1-SPD__MDEG:0), XI_VAL, VEC_X3(3), &
     &           VEC_XYZ(3), VEC_HLP(3), RDER_HLP(3), R_DER(3), REFR_DER_XI(3), &
     &           ATT(SPD__MLEV,SPD__M_FRQ), TEM(SPD__MLEV)
      REAL*8     XI_EPS, AZ_EPS
      PARAMETER  ( XI_EPS = 1.D-5   )
      PARAMETER  ( AZ_EPS = 0.001D0 )
      CHARACTER  STR*128
      INTEGER*4  J1, J2, J3, J4, J5, J6, J7, J8, IN, IPIV(SPD__MLEV), &
     &           SPD_TYP, IER
      REAL*8     DELTA_ARG, DELTA_R, DELTA_VEC_X3(3), DELTA_VEC_HLP(3)
      LOGICAL*1  FL_OPA_TAT
      PARAMETER  ( DELTA_ARG = 1.0D0 ) 
      REAL*8,    EXTERNAL :: ISPL8, BSPL_VAL, BSPL_DER, BSPL_DR2, &
     &                       SPD_GET_ATT, SPD_GET_TEMP
      LOGICAL*4, EXTERNAL :: IS_R8_NAN 
      REAL*4,    EXTERNAL :: SPL4_INT 
      INTEGER*1, EXTERNAL :: INT1
      INTEGER*4, EXTERNAL :: IXMN8 
      INTEGER*4  I, J, IDEG, LOC_TO_BAND
      LOC_TO_BAND(I,J,IDEG) = I-J+IDEG + (IDEG+1)*(IDEG+J-1)
!
      IF ( SPD%CONF%SPD_ALG == SPD__ALG_STRAIGHT ) THEN
           IF ( IVRB == 5 ) THEN
                CALL WALL_TIMER ( %VAL(0) )
           END IF
!
! -------- "Straight line" algorithm. We neglect bending and consider
! -------- photon trajectory is a straight line
!
           DO 410 J1=1,NH
              VEC_X3(1) = XI(J1) 
              VEC_X3(2) = 0.0D0
              VEC_X3(3) = 0.0D0
!
! ----------- Transform vector coordinate from XI,Y,Z to crust-fixed 
! ----------- carthesizan vector X,Y,Z ( VEC_XYZ ) with respect to 
! ----------- the receiver
!
              CALL MUL_MV_IV_V ( 3, 3, MAT_XI_TO_X3, 3, VEC_X3, 3, &
     &                           VEC_XYZ, -2 )
!
! ----------- Transform XYZ to HLP coordinates that are used for
! ----------- interpolating the global refractivity field
!
              VEC_XYZ(1) = VEC_GROUND_XYZ(1) + VEC_XYZ(1) 
              VEC_XYZ(2) = VEC_GROUND_XYZ(2) + VEC_XYZ(2) 
              VEC_XYZ(3) = VEC_GROUND_XYZ(3) + VEC_XYZ(3) 
!
              CALL XYZ_TO_HLP ( SPD, VEC_XYZ, VEC_HLP )
!
! ----------- Get the total refractivity and its partial derivatives 
! ----------- in HLP coordinate system
!
              CALL ERR_PASS ( IUER, IER )
              CALL GET_REFR_HLP ( SPD, SPD__TOT, VEC_HLP, R_VAL,  IER )
              CALL GET_REFR_HLP ( SPD, SPD__WAT, VEC_HLP, RW_VAL, IER )
              DST_TOT_R4(J1) = R_VAL
              DST_WAT_R4(J1) = RW_VAL
              XI_R4(J1)      = XI(J1)
 410       CONTINUE 
           IF ( IVRB == 5 ) THEN
                CALL WALL_TIMER ( STR )
                WRITE ( 6, * ) 'Slant_bend: ', STR(1:27)
                CALL FLUSH ( 6 )
           END IF
!
! -------- Now compute path delay along the straight line
!
           GOTO 710
      END IF
      IF ( IVRB == 5 ) THEN
           CALL WALL_TIMER ( %VAL(0) )
      END IF
!
! --- Initialiazation: the initial trajectory is the straight line
! --- that coincides with the axis XI, i.e. no bending
!
      CALL NOUT_R8 ( NH, Y  )
      CALL NOUT_R8 ( NH, Z  )
      CALL NOUT_R8 ( NH, YD )
      CALL NOUT_R8 ( NH, ZD )
      CALL NOUT_R8 ( NH, YDD )
      CALL NOUT_R8 ( NH, ZDD )
!
! --- Several relaxation iterations for computing the trajectory
!
      DO 420 J2=1,M_ITR
!
! ------ Build the band system of linear algebraic equations for estimation 
! ------ of the B-spline coefficients that approximate the trajectory
!
         CALL NOUT_R8 ( (1+SPD__MDEG)*SPD__MLEV, MATB_Y )
         CALL NOUT_R8 ( (1+SPD__MDEG)*SPD__MLEV, MATB_Z )
         CALL NOUT_R8 ( (NH+2), RB_Y )
         CALL NOUT_R8 ( (NH+2), RB_Z )
!
         DO 430 J3=1,NH
!
! --------- Compute refractivity at point XI(J3),Y(J3),Z(J3) and its 
! --------- parital derivatives
!
            IF ( SPD%CONF%SPD_ALG == SPD__ALG_NONLOC   .OR. &
     &           SPD%CONF%SPD_ALG == SPD__ALG_Y_NONLOC      ) THEN
!
! -------------- Non-local algorithm: interpolate the global refractivity
! -------------- field to the point XI(J3),Y(J3),Z(J3).
!
                 VEC_X3(1) = XI(J3) 
                 VEC_X3(2) = Y(J3)
                 VEC_X3(3) = Z(J3) 
!               
                 CALL ERR_PASS ( IUER, IER )
                 CALL GET_REFR_DER_XI ( SPD, SPD__TOT, MAT_XI_TO_X3, &
     &                                  VEC_GROUND_XYZ, VEC_X3, AZ, EL, &
     &                                  R_VAL, REFR_DER_XI, IER )
                 IF ( IER .NE. 0 ) THEN
                      CALL CLRCH ( STR )
                      CALL INCH  ( J3, STR )
                      CALL ERR_LOG ( 5531, IUER, 'SPD_3D_SLANT', 'Error in '// &
     &                    'computing refractivity. Node '//STR )
                      RETURN 
                 END IF
                 RDX_VAL = REFR_DER_XI(1)
                 RDY_VAL = REFR_DER_XI(2)
                 RDZ_VAL = REFR_DER_XI(3)
                 RFR_VAL(J3,SPD__TOT) = R_VAL
                 R_VAL = R_VAL + 1.0D0
                 RFR_DER(J3,1) = REFR_DER_XI(1)
                 RFR_DER(J3,2) = REFR_DER_XI(2)
                 RFR_DER(J3,3) = REFR_DER_XI(3)
               ELSE
!
! -------------- Local algorithm. 
! -------------- 1) Consider that parital derivatives depend
! -------------- only on XI, but not in Y and Z. 
! -------------- 2) Consider only the first term of Taylor expansion of 
! --------------    the refractivity over spatial coordinates
!
                 RDX_VAL = RFR_DER(J3,1)
                 RDY_VAL = RFR_DER(J3,2)
                 IF ( SPD%CONF%SPD_ALG == SPD__ALG_LIN_Y ) THEN
                      RDZ_VAL = 0.0D0
                    ELSE 
                      RDZ_VAL = RFR_DER(J3,3)
                 END IF
                 R_VAL = 1.0D0 + RFR_VAL(J3,SPD__TOT) + RDY_VAL*Y(J3) + RDZ_VAL*Z(J3) 
            END IF
!
! --------- Compute three non-zero coefficients in the J2 -th equation
!
            IF ( J3 < NH ) THEN
                  EQU(-2) = R_VAL  *BSPL_DR2 ( NH, XI, SPD__MDEG, J3-3, XI(J3) ) &
     &                    + RDX_VAL*BSPL_DER ( NH, XI, SPD__MDEG, J3-3, XI(J3) )
                  EQU(-1) = R_VAL  *BSPL_DR2 ( NH, XI, SPD__MDEG, J3-2, XI(J3) ) &
     &                    + RDX_VAL*BSPL_DER ( NH, XI, SPD__MDEG, J3-2, XI(J3) )
                  EQU(0)  = R_VAL  *BSPL_DR2 ( NH, XI, SPD__MDEG, J3-1, XI(J3) ) &
     &                    + RDX_VAL*BSPL_DER ( NH, XI, SPD__MDEG, J3-1, XI(J3) )
                ELSE 
                  EQU(-2) = R_VAL  *BSPL_DR2 ( NH, XI, SPD__MDEG, J3-3, XI(J3)-XI_EPS ) &
     &                    + RDX_VAL*BSPL_DER ( NH, XI, SPD__MDEG, J3-3, XI(J3)-XI_EPS )
                  EQU(-1) = R_VAL  *BSPL_DR2 ( NH, XI, SPD__MDEG, J3-2, XI(J3)-XI_EPS ) &
     &                    + RDX_VAL*BSPL_DER ( NH, XI, SPD__MDEG, J3-2, XI(J3)-XI_EPS )
                  EQU(0)  = R_VAL  *BSPL_DR2 ( NH, XI, SPD__MDEG, J3-1, XI(J3)-XI_EPS ) &
     &                    + RDX_VAL*BSPL_DER ( NH, XI, SPD__MDEG, J3-1, XI(J3)-XI_EPS )
            END IF
!
! --------- Put these coefficients into the banded matrix for Y coordinate
! --------- of the trajectory
!
            IF ( SPD%CONF%SPD_ALG == SPD__ALG_LIN_Y      .OR. &
     &           SPD%CONF%SPD_ALG == SPD__ALG_LIN_YZ     .OR. &
     &           SPD%CONF%SPD_ALG == SPD__ALG_NONLIN_LOC .OR. &
     &           SPD%CONF%SPD_ALG == SPD__ALG_NONLOC     .OR. &
     &           SPD%CONF%SPD_ALG == SPD__ALG_Y_NONLOC          ) THEN
!
                 MATB_Y(LOC_TO_BAND(J3-3,J3-2,SPD__MDEG)) = EQU(-2)
                 MATB_Y(LOC_TO_BAND(J3-2,J3-2,SPD__MDEG)) = EQU(-1)
                 MATB_Y(LOC_TO_BAND(J3-1,J3-2,SPD__MDEG)) = EQU( 0)
            END IF
            IF ( SPD%CONF%SPD_ALG == SPD__ALG_LIN_Y  .OR. &
     &           SPD%CONF%SPD_ALG == SPD__ALG_LIN_YZ      ) THEN
!
! --------------- Linear algorithm. The right hand side depends only on 
! --------------- the partial derivative of refractivity
!
                  RB_Y(J3-2) = RDY_VAL
               ELSE IF ( SPD%CONF%SPD_ALG == SPD__ALG_NONLIN_LOC .OR. &
     &                   SPD%CONF%SPD_ALG == SPD__ALG_NONLOC     ) THEN
!
! --------------- Full, non-liner algorithm
!
                  RB_Y(J3-2) = RDY_VAL &
     &                         + 2.D0*RDY_VAL*ZD(J3)**2 &
     &                         - RDZ_VAL*YD(J3)*ZD(J3) &
     &                         + RDY_VAL*YD(J3)**2 &
     &                         - RDX_VAL*YD(J3)*ZD(J3)**2 &
     &                         - RDX_VAL*YD(J3)**3 &
     &                         - RDZ_VAL*YD(J3)*ZD(J3)**3 &
     &                         + RDY_VAL*YD(J3)**2*ZD(J3)**2 &
     &                         - RDZ_VAL*YD(J3)**3*ZD(J3) &
     &                         + RDY_VAL*ZD(J3)**4 &
     &                         + RDZ_VAL*YD(J3)*ZD(J3)*ZDD(J3) 
               ELSE IF ( SPD%CONF%SPD_ALG == SPD__ALG_Y_NONLOC     ) THEN
!
! --------------- Full, non-liner algorithm
!
                  RB_Y(J3-2) = RDY_VAL &
     &                         + RDY_VAL*YD(J3)**2 &
     &                         - RDX_VAL*YD(J3)**3 
            END IF
!
            IF ( SPD%CONF%SPD_ALG == SPD__ALG_LIN_YZ     .OR. &
     &             SPD%CONF%SPD_ALG == SPD__ALG_NONLIN_LOC .OR. &
     &             SPD%CONF%SPD_ALG == SPD__ALG_NONLOC          ) THEN
!
! ---------------- Put these coefficients into the banded matrix for 
! ---------------- Z coordinate of the trajectory
!
                   MATB_Z(LOC_TO_BAND(J3-3,J3-2,SPD__MDEG)) = EQU(-2)
                   MATB_Z(LOC_TO_BAND(J3-2,J3-2,SPD__MDEG)) = EQU(-1)
                   MATB_Z(LOC_TO_BAND(J3-1,J3-2,SPD__MDEG)) = EQU( 0)
            END IF
            IF ( SPD%CONF%SPD_ALG == SPD__ALG_LIN_YZ ) THEN
!
! -------------- Linear algorithm. The right hand side depends only on 
! -------------- the partial derivative of refractivity
!
                 RB_Z(J3-2) = RDZ_VAL
               ELSE IF ( SPD%CONF%SPD_ALG == SPD__ALG_NONLIN_LOC .OR. &
     &                   SPD%CONF%SPD_ALG == SPD__ALG_NONLOC     ) THEN
!
! -------------- Full, non-liner algorithm
!
                 RB_Z(J3-2) = RDZ_VAL &
     &                        + 2.D0*RDZ_VAL*YD(J3)**2 &
     &                        - RDY_VAL*ZD(J3)*YD(J3) &
     &                        + RDZ_VAL*ZD(J3)**2 &
     &                        - RDX_VAL*ZD(J3)*YD(J3)**2 &
     &                        - RDX_VAL*ZD(J3)**3 &
     &                        - RDY_VAL*ZD(J3)*YD(J3)**3 &
     &                        + RDZ_VAL*ZD(J3)**2*YD(J3)**2 &
     &                        - RDY_VAL*ZD(J3)**3*YD(J3) &
     &                        + RDZ_VAL*YD(J3)**4 &
     &                        + RDY_VAL*ZD(J3)*YD(J3)*YDD(J3) 
            END IF
 430     CONTINUE 
!
! ------ Initial conditions at the receiver part of the integration interval
!
         EQU(-2) = BSPL_VAL ( NH, XI, SPD__MDEG, -2, XI(1)+XI_EPS )
         EQU(-1) = BSPL_VAL ( NH, XI, SPD__MDEG, -1, XI(1)+XI_EPS )
         EQU( 0) = BSPL_VAL ( NH, XI, SPD__MDEG,  0, XI(1)+XI_EPS )
!
         IF ( SPD%CONF%SPD_ALG == SPD__ALG_LIN_Y      .OR. &
     &        SPD%CONF%SPD_ALG == SPD__ALG_LIN_YZ     .OR. &
     &        SPD%CONF%SPD_ALG == SPD__ALG_NONLIN_LOC .OR. &
     &        SPD%CONF%SPD_ALG == SPD__ALG_NONLOC     .OR. &
     &        SPD%CONF%SPD_ALG == SPD__ALG_Y_NONLOC        ) THEN
!
              MATB_Y(LOC_TO_BAND(-3,-2,SPD__MDEG)) = EQU(-2)
              MATB_Y(LOC_TO_BAND(-2,-2,SPD__MDEG)) = EQU(-1)
              MATB_Y(LOC_TO_BAND(-1,-2,SPD__MDEG)) = EQU( 0)
              RB_Y(-2) = 0.0D0
         END IF
!
         IF ( SPD%CONF%SPD_ALG == SPD__ALG_LIN_YZ     .OR. &
     &        SPD%CONF%SPD_ALG == SPD__ALG_NONLIN_LOC .OR. &
     &        SPD%CONF%SPD_ALG == SPD__ALG_NONLOC          ) THEN
!
              MATB_Z(LOC_TO_BAND(-3,-2,SPD__MDEG)) = EQU(-2)
              MATB_Z(LOC_TO_BAND(-2,-2,SPD__MDEG)) = EQU(-1)
              MATB_Z(LOC_TO_BAND(-1,-2,SPD__MDEG)) = EQU( 0)
              RB_Z(-2) = 0.0D0
         END IF
!
! ------ Initial conditions at the emitter part of the integration interval
!
         EQU(-2) = BSPL_DER ( NH, XI, SPD__MDEG, NH-3, XI(NH)-XI_EPS )
         EQU(-1) = BSPL_DER ( NH, XI, SPD__MDEG, NH-2, XI(NH)-XI_EPS )
         EQU( 0) = BSPL_DER ( NH, XI, SPD__MDEG, NH-1, XI(NH)-XI_EPS )
!
         IF ( SPD%CONF%SPD_ALG == SPD__ALG_LIN_Y      .OR. &
     &        SPD%CONF%SPD_ALG == SPD__ALG_LIN_YZ     .OR. &
     &        SPD%CONF%SPD_ALG == SPD__ALG_NONLIN_LOC .OR. &
     &        SPD%CONF%SPD_ALG == SPD__ALG_NONLOC     .OR. &
     &        SPD%CONF%SPD_ALG == SPD__ALG_Y_NONLOC        ) THEN
!
              MATB_Y(LOC_TO_BAND(NH-3,NH-1,SPD__MDEG)) = EQU(-2)
              MATB_Y(LOC_TO_BAND(NH-2,NH-1,SPD__MDEG)) = EQU(-1)
              MATB_Y(LOC_TO_BAND(NH-1,NH-1,SPD__MDEG)) = EQU( 0)
              RB_Y(NH-1) = 0.0D0
         END IF
!
         IF ( SPD%CONF%SPD_ALG == SPD__ALG_LIN_YZ     .OR. &
     &        SPD%CONF%SPD_ALG == SPD__ALG_NONLIN_LOC .OR. &
     &        SPD%CONF%SPD_ALG == SPD__ALG_NONLOC          ) THEN
!
              MATB_Z(LOC_TO_BAND(NH-3,NH-1,SPD__MDEG)) = EQU(-2)
              MATB_Z(LOC_TO_BAND(NH-2,NH-1,SPD__MDEG)) = EQU(-1)
              MATB_Z(LOC_TO_BAND(NH-1,NH-1,SPD__MDEG)) = EQU( 0)
              RB_Z(NH-1) = 0.0D0
         END IF
!
! ------ Solve the system for Y variable
!
         IF ( SPD%CONF%SPD_ALG == SPD__ALG_LIN_Y      .OR. &
     &        SPD%CONF%SPD_ALG == SPD__ALG_LIN_YZ     .OR. &
     &        SPD%CONF%SPD_ALG == SPD__ALG_NONLIN_LOC .OR. &
     &        SPD%CONF%SPD_ALG == SPD__ALG_NONLOC     .OR. &
     &        SPD%CONF%SPD_ALG == SPD__ALG_Y_NONLOC        ) THEN
!
! ----------- Decompose the band matrix
!
              CALL DGBTRF ( NH+2, NH+2, 1, 1, MATB_Y, 4, IPIV, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( IER, STR )
!$OMP              CRITICAL  
                   CALL ERR_LOG ( 5535, IUER, 'SPD_3D_SLANT', 'Error in '// &
          &           'DGBTRF INFO= '//STR )
!$OMP              END CRITICAL
                   RETURN 
              END IF
!
! ----------- Solve the linear equation using the decomposed band matrix
!
              CALL DGBTRS ( 'T', NH+2, 1, 1, 1, MATB_Y, 4, IPIV, RB_Y, &
          &                  NH+2, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( IER, STR )
!$OMP              CRITICAL  
                   CALL ERR_LOG ( 5536, IUER, 'SPD_3D_SLANT', 'Error in '// &
          &            'DGBTRS INFO= '//STR )
!$OMP              END CRITICAL
                   RETURN 
              END IF
         END IF
!
! ------ Solve the system for Z variable
!
         IF ( SPD%CONF%SPD_ALG == SPD__ALG_LIN_YZ     .OR. &
     &        SPD%CONF%SPD_ALG == SPD__ALG_NONLIN_LOC .OR. &
     &        SPD%CONF%SPD_ALG == SPD__ALG_NONLOC          ) THEN
!
! ----------- Decompose the band matrix
!
              IER = -1
              CALL DGBTRF ( NH+2, NH+2, 1, 1, MATB_Z, 4, IPIV, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( IER, STR )
!$OMP              CRITICAL  
                   CALL ERR_LOG ( 5537, IUER, 'SPD_3D_SLANT', 'Error in '// &
          &            'DGBTRF INFO= '//STR )
!$OMP              END CRITICAL
                   RETURN 
              END IF
!
! ----------- Solve the linear equation using the decomposed band matrix
!
              IER = -1
              CALL DGBTRS ( 'T', NH+2, 1, 1, 1, MATB_Z, 4, IPIV, RB_Z, &
          &                  NH+2, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( IER, STR )
!$OMP              CRITICAL  
                   CALL ERR_LOG ( 5538, IUER, 'SPD_3D_SLANT', 'Error in '// &
          &            'DGBTRS INFO= '//STR )
!$OMP              END CRITICAL
                   RETURN 
              END IF
         END IF
!
! ------ Get coefficients of the B-spline that approximate the trajectory
!
         IF ( SPD%CONF%SPD_ALG == SPD__ALG_LIN_Y      .OR. &
     &        SPD%CONF%SPD_ALG == SPD__ALG_LIN_YZ     .OR. &
     &        SPD%CONF%SPD_ALG == SPD__ALG_NONLIN_LOC .OR. &
     &        SPD%CONF%SPD_ALG == SPD__ALG_NONLOC     .OR. &
     &        SPD%CONF%SPD_ALG == SPD__ALG_Y_NONLOC        ) THEN
!
              Y_SPL(1-SPD__MDEG:NH-1) = RB_Y(1-SPD__MDEG:NH-1) 
!
! ----------- Y_SPL(NH) was not defined before. Set it to zero in order
! ----------- to avoid an opertation on not-a-number
!
              Y_SPL(NH) = 0.0D0
         END IF
!
         IF ( SPD%CONF%SPD_ALG == SPD__ALG_LIN_YZ     .OR. &
     &        SPD%CONF%SPD_ALG == SPD__ALG_NONLIN_LOC .OR. &
     &        SPD%CONF%SPD_ALG == SPD__ALG_NONLOC          ) THEN
!
              Z_SPL(1-SPD__MDEG:NH-1) = RB_Z(1-SPD__MDEG:NH-1) 
!
! ----------- Z_SPL(NH) was not defined before. Set it to zero in order
! ----------- to avoid an opertation on not-a-number
!
              Z_SPL(NH) = 0.0D0
!!  z_spl = 0.0d0 ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
         END IF
!
! ------ Compute the trajectory using coefficients of B-spline that are 
! ------ solution of linear equations that we have just found
!
         CALL NOUT_R8 ( NH, Y )
         CALL NOUT_R8 ( NH, Z )
         CALL NOUT_R8 ( NH, YD )
         CALL NOUT_R8 ( NH, ZD )
         CALL NOUT_R8 ( NH, YDD )
         CALL NOUT_R8 ( NH, ZDD )
         Z_SPL(NH) = 0.0D0
         DO 440 J4=1,NH
            DO 450 J5=-SPD__MDEG,0
!
! ------------ Y component
!
               IF ( SPD%CONF%SPD_ALG == SPD__ALG_LIN_Y      .OR. &
     &              SPD%CONF%SPD_ALG == SPD__ALG_LIN_YZ     .OR. &
     &              SPD%CONF%SPD_ALG == SPD__ALG_NONLIN_LOC .OR. &
     &              SPD%CONF%SPD_ALG == SPD__ALG_NONLOC     .OR. &
     &              SPD%CONF%SPD_ALG == SPD__ALG_Y_NONLOC        ) THEN
!
                    IF ( J4 < NH ) THEN
                         Y(J4)  = Y(J4)  + Y_SPL(J4+J5)* &
     &                            BSPL_VAL ( NH, XI, SPD__MDEG, J4+J5, XI(J4) )
                         YD(J4) = YD(J4) + Y_SPL(J4+J5)* &
     &                            BSPL_DER ( NH, XI, SPD__MDEG, J4+J5, XI(J4) )
                       ELSE 
                         Y(J4)  = Y(J4) + Y_SPL(J4+J5)* &
     &                            BSPL_VAL ( NH, XI, SPD__MDEG, J4+J5, XI(J4)-XI_EPS )
                         YD(J4) = YD(J4) + Y_SPL(J4+J5)* &
     &                            BSPL_DER ( NH, XI, SPD__MDEG, J4+J5, XI(J4)-XI_EPS )
                    END IF
               END IF
!
! ------------ Z component
!
               IF ( SPD%CONF%SPD_ALG == SPD__ALG_LIN_YZ     .OR. &
     &              SPD%CONF%SPD_ALG == SPD__ALG_NONLIN_LOC .OR. &
     &              SPD%CONF%SPD_ALG == SPD__ALG_NONLOC          ) THEN
!
                    IF ( J4 < NH ) THEN
                         Z(J4)  = Z(J4)  + Z_SPL(J4+J5)* &
     &                            BSPL_VAL ( NH, XI, SPD__MDEG, J4+J5, XI(J4) )
                         ZD(J4) = ZD(J4) + Z_SPL(J4+J5)* &
     &                            BSPL_DER ( NH, XI, SPD__MDEG, J4+J5, XI(J4) )
                       ELSE 
                         Z(J4)  = Z(J4) + Z_SPL(J4+J5)* &
     &                            BSPL_VAL ( NH, XI, SPD__MDEG, J4+J5, XI(J4)-XI_EPS )
                         ZD(J4) = ZD(J4) + Z_SPL(J4+J5)* &
     &                            BSPL_DER ( NH, XI, SPD__MDEG, J4+J5, XI(J4)-XI_EPS )
                    END IF
               END IF
!
! ------------ Compute the second derivatives that is needed for 
! ------------ non-linear terms
!
               IF ( SPD%CONF%SPD_ALG == SPD__ALG_NONLIN_LOC .OR. &
     &              SPD%CONF%SPD_ALG == SPD__ALG_NONLOC          ) THEN
                    IF ( J4 < NH ) THEN
                         YDD(J4) = YDD(J4) + Y_SPL(J4+J5)* &
     &                             BSPL_DR2 ( NH, XI, SPD__MDEG, J4+J5, XI(J4) )
                         ZDD(J4) = ZDD(J4) + Z_SPL(J4+J5)* &
     &                             BSPL_DR2 ( NH, XI, SPD__MDEG, J4+J5, XI(J4) )
                       ELSE 
                         YDD(J4) = YDD(J4) + Y_SPL(J4+J5)* &
     &                             BSPL_DR2 ( NH, XI, SPD__MDEG, J4+J5, XI(J4)-XI_EPS )
                         ZDD(J4) = ZDD(J4) + Z_SPL(J4+J5)* &
     &                             BSPL_DR2 ( NH, XI, SPD__MDEG, J4+J5, XI(J4)-XI_EPS )
                    END IF
                  ELSE IF ( SPD%CONF%SPD_ALG == SPD__ALG_Y_NONLOC ) THEN
                    IF ( J4 < NH ) THEN
                         YDD(J4) = YDD(J4) + Y_SPL(J4+J5)* &
     &                             BSPL_DR2 ( NH, XI, SPD__MDEG, J4+J5, XI(J4) )
                       ELSE 
                         YDD(J4) = YDD(J4) + Y_SPL(J4+J5)* &
     &                             BSPL_DR2 ( NH, XI, SPD__MDEG, J4+J5, XI(J4)-XI_EPS )
                    END IF
               END IF
 450        CONTINUE 
 440     CONTINUE 
 420  CONTINUE 
!
! --- Decide whether we need to compute atmospheric opaicity and brightness temperature
!
      FL_OPA_TAT = .FALSE.
      IF ( SPD%CONF%SOB_ALG == SOB__ALG_RTE_BENT  .AND.  SPD%NFRQ > 0 ) THEN
           FL_OPA_TAT = .TRUE.
      END IF
      IF ( SPD%CONF%SOB_ALG == SOB__ALG_RTE_BENT_1AZ  .AND.  &
     &     SPD%NFRQ > 0 .AND.                                &
     &     DABS(AZ) < AZ_EPS                                 ) THEN
           FL_OPA_TAT = .TRUE.
      END IF
      ATT = 0.0D0
!
! --- Compute the array of the expression under the path delay integral
!
!!   call wall_timer ( %val(0) ) ! %%%%
      DO 460 J6=1,NH
!
! ------ Compute the refractivity at the point (XI(J6),Y(J6),Z(J6)) on the 
! ------ trajectory
!
         IF ( SPD%CONF%SPD_ALG == SPD__ALG_NONLOC   .OR. &
     &        SPD%CONF%SPD_ALG == SPD__ALG_Y_NONLOC      ) THEN
!
! ----------- Non-local algorithm: interpolate the global refractivity
! ----------- field to the point XI(J6),Y(J6),Z(J6).
!
              VEC_X3(1) = XI(J6) 
              VEC_X3(2) = Y(J6)
              VEC_X3(3) = Z(J6) 
!
              CALL ERR_PASS ( IUER, IER )
              CALL GET_REFR_XI ( SPD, SPD__WAT, MAT_XI_TO_X3, &
     &                           VEC_GROUND_XYZ, VEC_X3, RW_VAL, IER )
              CALL GET_REFR_XI ( SPD, SPD__TOT, MAT_XI_TO_X3, &
     &                           VEC_GROUND_XYZ, VEC_X3, R_VAL, IER )
              IF ( FL_OPA_TAT ) THEN
                   DO 470 J7=1,SPD%NFRQ  
                      ATT(J6,J7) = SPD_GET_ATT  ( SPD, J7, MAT_XI_TO_X3, VEC_GROUND_XYZ, VEC_X3, IER )
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!      write ( 6, 220 ) j6, j7, att(j6,j7), R_VAL, RW_VAL                        ! %%%%%%%%%%%%%%%%%%%%%%%%%%
! 220  format ( 'ATT: j6= ', i4,' j7= ', i4, ' att: ', 1pd22.15, ' r: ', 1pd22.15, ' rw: ', 1pd22.15 ) ! %%%%
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
                      IF ( IS_R8_NAN (ATT(J6,J7)) ) THEN
!$OMP                      CRITICAL
                           WRITE ( 6, * ) 'J6,J7= ', J6, J7
                           CALL ERR_LOG ( 5599, IUER, 'SPD_3D_SLANT', 'Trao of '// &
     &                         'internal control: not-a-number in absorbtion' )
!$OMP                      END CRITICAL
                           RETURN 
                      END IF
 470               CONTINUE 
                   TEM(J6)       = SPD_GET_TEMP ( SPD,     MAT_XI_TO_X3, VEC_GROUND_XYZ, VEC_X3, IER )
              END IF
            ELSE 
              RDY_VAL = RFR_DER(J6,2)
              IF ( SPD%CONF%SPD_ALG == SPD__ALG_LIN_YZ     .OR. &
     &             SPD%CONF%SPD_ALG == SPD__ALG_NONLIN_LOC      ) THEN
!
                   RDZ_VAL = RFR_DER(J6,3)
                 ELSE 
                   RDZ_VAL = 0.0D0
              END IF
              R_VAL  = RFR_VAL(J6,SPD__TOT) + RDY_VAL*Y(J6) + RDZ_VAL*Z(J6) 
              RW_VAL = RFR_VAL(J6,SPD__WAT) + RDY_VAL*Y(J6) + RDZ_VAL*Z(J6) 
         END IF
!
         DST_TOT_R4(J6) = (1.0D0 +  R_VAL)* &
     &                     DSQRT( 1.0D0 + YD(J6)**2 + ZD(J6)**2 ) - 1.0D0
         DST_WAT_R4(J6) = (1.0D0 + RW_VAL)* &
     &                     DSQRT( 1.0D0 + YD(J6)**2 + ZD(J6)**2 ) - 1.0D0
         XI_R4(J6)  = XI(J6)
 460  CONTINUE 
!
      IF ( FL_OPA_TAT ) THEN
           DO 480 J8=1,SPD%NFRQ
              CALL ERR_PASS ( IUER, IER )
              CALL SOB_COMP ( NH, XI, ATT(1,J8), TEM, OPA(J8), TAT(J8), IER )
              IF ( IER .NE. 0 ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( J8, STR )
                   WRITE ( 6, * ) 'AZ= ', AZ, ' EL= ', EL
                   CALL ERR_LOG ( 5539, IUER, 'SPD_3D_SLANT', 'Error in '// &
     &                 'an attempt to compute atmospheric opacity and '// &
     &                 'atmosphere brightness temperature for the '//TRIM(STR)// &
     &                 'th frequency' )
                   RETURN 
              END IF
 480       CONTINUE 
         ELSE
           IF ( SPD%NFRQ > 0 ) THEN
                OPA = 0.0D0
                TAT = 0.0D0
           END IF
      END IF
      IF ( IVRB == 5 ) THEN
           CALL WALL_TIMER ( STR )
           WRITE ( 6, * ) 'Slant_Bend: ', STR(1:27)
           CALL FLUSH ( 6 )
      END IF
 710  CONTINUE 
      IF ( IVRB == 5 ) CALL WALL_TIMER ( %VAL(0) )
!
! --- Expand DST_TOT_R4 and DST_WAT_R4 into B-spline basis
!
      CALL ERR_PASS ( IUER, IER )
      CALL BSPL4_1D_CMP ( SPD__MDEG, 0, NH, XI_R4, DST_TOT_R4, IER )
      IF ( IER .NE. 0 ) THEN
!$OMP      CRITICAL
           CALL ERR_LOG ( 5540, IUER, 'SPD_3D_SLANT', 'Error in '// &
     &         'an attempt to expand DST_TOT_R4 into B-spline basis' )
!$OMP      END CRITICAL
           RETURN 
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL BSPL4_1D_CMP ( SPD__MDEG, 0, NH, XI_R4, DST_WAT_R4, IER )
      IF ( IER .NE. 0 ) THEN
!$OMP      CRITICAL
           CALL ERR_LOG ( 5541, IUER, 'SPD_3D_SLANT', 'Error in '// &
     &         'an attempt to expand DST_WAT_R4 into B-spline basis' )
!$OMP      END CRITICAL
           RETURN 
      END IF
!
! --- ... And then compute the integral using spline coefficients
! 
      DELS(SPD__TOT) = SPL4_INT ( NH, XI_R4, SPD__MDEG, DST_TOT_R4, XI_R4(NH) )/SPD__C
      DELS(SPD__WAT) = SPL4_INT ( NH, XI_R4, SPD__MDEG, DST_WAT_R4, XI_R4(NH) )/SPD__C
      IF ( IVRB == 5 ) THEN
           CALL WALL_TIMER ( STR )
           WRITE ( 6, * ) 'Slant_del:  ', STR(1:27)
           CALL FLUSH ( 6 )
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  SPD_3D_SLANT  !#!#
