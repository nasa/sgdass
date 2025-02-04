      SUBROUTINE SPD_3D_DEL ( SPD, IND_STA, EL, AZ, DEL, OPA, TAT, &
     &                        IVRB, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  SPD_3D_DEL  computes the path delay through the 3D        *
! *   heterogeneous refractivity field represented in the form of        *
! *   a 3D section over time variable of 3D array of coefficients of     *
! *   expansion over B-splines of the 3rd degree. The path delay is      *
! *   computed between the ground point of the station with the IND_STA  *
! *   index in the station list and the emitter located at the infinity  *
! *   with mathematical elevation above the horizon EL, undisturbed      *
! *   by refraction and azimuth AZ.                                      *
! *                                                                      *
! *   The atmospheric path delay is computed for one or two components   *
! *   among a) total path delay; b) hydrostatic path delay;              *
! *   c) non-hydrostatic path delay in accordance to                     *
! *   SPD%CONF%SPLIT_MODE parameter.                                     *
! *                                                                      *
! *   Path delay is computed by numeric solving a system of two          *
! *   differential equations of the second order that are Euler          *
! *   equations that solves the variational problem of minimization of   *
! *   propagation time.                                                  *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *  IND_STA ( INTEGER*4      ) -- Station index for which the           *
! *                                computation will be carried out.      *
! *       EL ( REAL*8         ) -- Elevation angle of the the source     *
! *                                above the mathematical horizon,       *
! *                                i.e. plain normal to the reference    *
! *                                ellipsoid. The angle is undisturbed   *
! *                                by refraction. Units: radians.        *
! *       AZ ( REAL*8         ) -- Local azimuth of the source counted   *
! *                                from the north towards east,          *
! *                                undisturbed by refraction. Units:     *
! *                                radians.                              *
! *     IVRB ( INTEGER*4      ) -- Verbosity level. 0 -- silent;         *
! *                                IVRB > 3 is for debugging.            *
! * SPD ( MET_GRID__TYPE ) -- Object with data structure of the          *
! *                                3D field of meteorological parameters.*
! *                                                                      *
! * _________________________ Output parameters: _______________________ *
! *                                                                      *
! *      DEL ( REAL*8         ) -- Excess path delays through the        *
! *                                atmosphere relative to the the case   *
! *                                if the waves between the emitter and  *
! *                                received were propagated in vacuum.   *
! *                                Two components are returned:          *
! *                                1) total path delay;                  *
! *                                2) contribution of water vapor in     *
! *                                   path delay;                        *
! *                                Dimension: 2. Units: seconds.         *
! *      OPA ( REAL*8         ) -- Atmospheric opacity, i.e. optical     *
! *                                depth as a function of frequency.     *
! *                                Unitless (i.e. neper).                *
! *                                Dimension: SPD%NFRQ.                  *
! *      TAT ( REAL*8         ) -- Atmosphere brightness temperature     *
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
! *  ### 28-NOV-2008   SPD_3D_DEL  v4.1 (c)  L. Petrov  01-JAN-2024 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'spd.i'
      INCLUDE   'astro_constants.i'
      TYPE     ( SPD_3D__TYPE ) :: SPD
      REAL*8     EL, AZ, DEL(2), OPA(SPD__M_FRQ), TAT(SPD__M_FRQ)
      INTEGER*4  IND_STA, IVRB, IUER
      REAL*8     VEC_GROUND_C3(3), VEC_GROUND_HLP(3)
      REAL*8       H__EOA, REA, FE, ECC_SQ, XI_EPS, VTD__C
      INTEGER*4  M_ITR
      PARAMETER  ( H__EOA = 80000.0D0     )    ! Height of end of atmpshere &
!                                              ! above the reference ellispoid
      PARAMETER  ( REA     = 6378136.3D0     ) ! Earth's equatorial radius
      PARAMETER  ( FE      = 1.D0/298.257D0  ) ! Earth's flattening
      PARAMETER  ( ECC_SQ  = 2.D0*FE - FE**2 ) ! Earth's eccentricity
      PARAMETER  ( M_ITR   = 4 ) ! Number of iteration for geometric computatins
      PARAMETER  ( XI_EPS  = 1.0D0 ) ! Marings for overshot/undershot
      PARAMETER  ( VTD__C   = 299792458.0D0 )
      INTEGER*4  MAX_NOD
      PARAMETER  ( MAX_NOD = 32*SPD__M_LEV )
      REAL*8     XI_EOA, H_STEP, H_LEV, XI(SPD__MLEV), VEC_XI(3), &
     &           VEC_C3(3), VEC_HLP(3), VEC_EOA_C3(3), &
     &           VEC_EOA_HLP(3), &
     &           RFR_VAL(SPD__MLEV,SPD__MTYP), RFR_DER(SPD__MLEV,3), &
     &           SPL_RFR_VAL(SPD__MLEV), &
     &           TMP_ARR(SPD__MLEV), &
     &           LAT_GCN, LAT_GDT, LON, HEI_GEOID, HEI_ELL, EL_EFF, HEI_COR, &
     &           LAT_GCN_EOA, DH_GROUND, RA, RS, &
     &           LON_EOA, HEI_REFEL_EOA, RD_EOA, G_ACC, &
     &           DH_EOA, LAT_GDT_EOA, L_GRO, L_EOA, HEI_DIF, RLEV, &
     &           RDER_HLP(3), RDER_XI(3), XI_ARR(MAX_NOD), Y_SPL(-2:MAX_NOD)
      REAL*8     MAT_R1(3,3), MAT_R2(3,3), MAT_R3(3,3), MAT_R4(3,3), &
     &           MAT_TMP(3,3), MAT_XI_TO_X3(3,3), ATT(SPD__MLEV,SPD__M_FRQ), &
     &           TEM(SPD__MLEV)
      LOGICAL*1  FL_OPA_TAT
      REAL*8       AZ_EPS
      PARAMETER  ( AZ_EPS = 0.001D0 )
      CHARACTER  STR*128
      REAL*8       EPS
      PARAMETER  ( EPS = 0.05 )
      REAL*8     ARGS(3)
      INTEGER*4  J1, J2, J3, J4, J5, IN, NH, INDZ, IXP, L_NOD, &
     &           DIMS(3), INDS(3), IER
!
      REAL*8       EL_TOL
      PARAMETER  ( EL_TOL = 1.D-4 ) ! Tolerance for elevation angle in rad
      REAL*8,    EXTERNAL :: GET_REFR_H4, VAL_3D_BSPLJ, SPD_GET_ATT, SPD_GET_TEMP
      INTEGER*4, EXTERNAL :: IXMN8, IXMN4
!
      IF ( IVRB == 7 ) CALL WALL_TIMER ( %VAL(0) )
      NH   = SPD%NLEV
!
! --- Get XYZ coordinates of the ground point (antenna)
!
      VEC_GROUND_C3(1) = SPD%STA(IND_STA)%COO_CFS(1)
      VEC_GROUND_C3(2) = SPD%STA(IND_STA)%COO_CFS(2)
      VEC_GROUND_C3(3) = SPD%STA(IND_STA)%COO_CFS(3)
!
! --- Get HLP coordinates for the ground point
! --- Height is counted from the ellipsoid
!
      CALL XYZ_TO_HLP ( SPD, VEC_GROUND_C3, VEC_GROUND_HLP ) ! H -- means height above the ellipsoid
      LAT_GCN   = SPD%STA(IND_STA)%LAT_GCN
      LAT_GDT   = SPD%STA(IND_STA)%LAT_GDT
      LON       = SPD%STA(IND_STA)%LON
      HEI_GEOID = SPD%STA(IND_STA)%HEI_GEOID
      HEI_ELL   = SPD%STA(IND_STA)%HEI_ELL
      RS        = SPD%STA(IND_STA)%RD         ! Distance from station to the geocenter
!     
! --- Compute MAT_XI_TO_X3 -- 3x3 matrix of transformation from
! --- xi-coordinates to XYZ:
! --- Rz(pi - az) * Rx(pi/2) * R2(pi/2 - lat_gdt -el) * R3(-lon)
!
      CALL GET_MAT_XI_TO_X3 ( LAT_GDT, LON, AZ, EL, MAT_XI_TO_X3 )
!
! --- Compute the effective elevation angle: pi/2 - z, where z 
! --- is the angle between the source and the geocentric station
! --- vector. Comutation is performed by iterations
!
      EL_EFF = EL - (LAT_GDT - LAT_GCN) 
!
! --- Initial value of the radius vector of the end-of-atmosphere
!
      RA = (RS - HEI_ELL) + H__EOA
!
      DO 410 J1=1,M_ITR
!@@         XI_EOA = DSQRT ( RA**2 - (RS*DCOS(EL_EFF))**2 ) - RS*DSIN(EL_EFF) ! equivalent
         XI_EOA = RS*( DSQRT(DSIN(EL_EFF)**2 + (RA/RS)**2 - 1.0D0) - DSIN(EL_EFF) )
         IF ( IVRB == 4 ) THEN
              WRITE  ( 6, 215 ) J1, EL/DEG__TO__RAD, XI_EOA, &
     &                          DSQRT ( RA**2 - (RS*DCOS(EL_EFF))**2 ) - RS*DSIN(EL_EFF), &
     &                          (H__EOA - HEI_ELL)/DSIN(EL)
 215          FORMAT ( 'Iter: ', I1, ' El= ', F10.7, ' Xie= ', F10.2, ' Xi1= ', F10.2, ' Xi2= ', F10.0 )
         END IF 
         VEC_XI(1) = XI_EOA 
         VEC_XI(2) = 0.0D0
         VEC_XI(3) = 0.0D0
!
! ------ VEC_C3 = MAT_XI_TO_X3 * VEC_XI 
!
         CALL MUL_MV_IV_V ( 3, 3, MAT_XI_TO_X3, 3, VEC_XI, 3, VEC_C3, -2 )
         VEC_EOA_C3 = VEC_GROUND_C3 + VEC_C3 
!
! ------ Compute geocentric latitude LAT_GCN_EOA, geodetic latitude LAT_GDT_EOA,
! ------ and height above the ellipsoid HEI_REFEL_EOA for the point of 
! ------ "end-of-atmosphere" at the straight line of sight
!
         CALL REF_ELL ( 0, VEC_EOA_C3, LAT_GCN_EOA, LAT_GDT_EOA, LON_EOA, &
     &                  HEI_REFEL_EOA, RD_EOA, G_ACC )
!
! ------ Update the radius-vector of "end-of-atmosphere"
!
         RA = RA + (H__EOA - HEI_REFEL_EOA)/DCOS(LAT_GDT_EOA - LAT_GCN_EOA)
 410  CONTINUE 
      IF ( DABS(HEI_REFEL_EOA - H__EOA) > 1.0D0 ) THEN
           WRITE ( 6, * ) ' HEI_REFEL_EOA = ', HEI_REFEL_EOA 
           WRITE ( 6, * ) ' H__EOA  = ', H__EOA  
           WRITE ( 6, * ) ' EL = ', EL, ' AZ = ', AZ
           CALL ERR_LOG ( 5511, IUER, 'SPD_3D_DEL', 'Trap of internal '// &
     &         'control: iterations for computing XI_EOA did not converge' )
           RETURN 
      END IF
!
! --- Compute HLP coordinates of the end-of-atmosphere
!
      CALL XYZ_TO_HLP ( SPD, VEC_EOA_C3, VEC_EOA_HLP )
!
! --- Compute array XI and air refractivity along vector XI.
! --- NB: The array of XI is not equidistant. We compute equidistant
! --- sequence of H-coordinates and transform them to XI coordiantes 
! --- by scaling using the ratio of XI_EOA/H__EOA
!
      H_STEP = (VEC_EOA_HLP(1) - VEC_GROUND_HLP(1))/(NH-1)
      DO 420 J2=1,NH
!
! ------ Compute XI
!
         RLEV = J2-1 - (SPD__MLEV-1)/2
         H_LEV  = VEC_GROUND_HLP(1) + (VEC_EOA_HLP(1) - VEC_GROUND_HLP(1))/SPD__U_MAX   * &
     &              (DEXP ( (RLEV - SPD__U3_GMAO72)/SPD__U1_GMAO72 ) - SPD__U2_GMAO72 - SPD__U_MIN)
!
! ------ Transform XI to XYZ
!
         XI(J2) = (XI_EOA + XI_EPS) * (H_LEV  - HEI_ELL)/ &
     &                                (H__EOA - HEI_ELL)
         VEC_XI(1) = XI(J2)
         VEC_XI(2) = 0.0D0
         VEC_XI(3) = 0.0D0
!
         IF ( SPD%CONF%SPD_ALG == SPD__ALG_STRAIGHT .OR. &
              SPD%CONF%SPD_ALG == SPD__ALG_NONLOC   .OR. &
     &        SPD%CONF%SPD_ALG == SPD__ALG_Y_NONLOC      ) THEN
              CONTINUE 
            ELSE 
              CALL ERR_PASS ( IUER, IER )
              CALL GET_REFR_DER_XI ( SPD, SPD__WAT, MAT_XI_TO_X3, VEC_GROUND_C3, &
     &                          VEC_XI, AZ, EL, RFR_VAL(J2,SPD__WAT), RDER_XI, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL CLRCH ( STR ) 
                   CALL INCH  ( J2, STR )
                   CALL ERR_LOG ( 5512, IUER , 'SPD_3D_DEL', 'Error in an attempt to get '// &
     &                 'refractivity and its partial derivatives' )
                   RETURN 
              END IF
!
              CALL ERR_PASS ( IUER, IER )
              CALL GET_REFR_DER_XI ( SPD, SPD__TOT, MAT_XI_TO_X3, VEC_GROUND_C3, &
     &                               VEC_XI, AZ, EL, RFR_VAL(J2,SPD__TOT), RDER_XI, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL CLRCH ( STR ) 
                   CALL INCH  ( J2, STR )
                   CALL ERR_LOG ( 5513, IUER , 'SPD_3D_DEL', 'Error in an attempt to get '// &
     &                 'refractivity and its partial derivatives' )
                   RETURN 
              END IF
              RFR_DER(J2,1:3) = RDER_XI(1:3)
              IF ( IVRB == 4 ) THEN
                   WRITE ( 6, 222 ) J2, NH, EL/DEG__TO__RAD, XI(J2), RFR_VAL(J2,SPD__TOT), &
     &                              RFR_DER(J2,1:3), H_LEV
 222               FORMAT ( 'Pnt: ', I3, ' ( ', I3, ' ) Elev= ', F10.6, &
     &                      ' Xi= ', F10.2, ' Ref= ', F12.10, &
     &                      ' Ref_der: ', 3(1PD12.4,1X), &
     &                      ' HLEV= ', 0PF9.2 )
              END IF
         END IF
         IF ( ( SPD%CONF%SOB_ALG == SOB__ALG_RTE_STRA    .OR. &
     &          SPD%CONF%SOB_ALG == SOB__ALG_RTE_STRA_1AZ     ) .AND. &
     &        SPD%NFRQ > 0 ) THEN
!
              DO 430 J3=1,SPD%CONF%N_FRQ
                 ATT(J2,J3) = SPD_GET_ATT  ( SPD, J3, MAT_XI_TO_X3, VEC_GROUND_C3, VEC_XI, IER )
 430          CONTINUE 
              TEM(J2)       = SPD_GET_TEMP ( SPD,     MAT_XI_TO_X3, VEC_GROUND_C3, VEC_XI, IER )
         END IF
 420  CONTINUE 
      IF ( IVRB == 7 ) THEN
           CALL WALL_TIMER ( STR )
           WRITE ( 6, * ) 'Slant_Del_a: ', STR(1:27)
           CALL FLUSH ( 6 )
           CALL WALL_TIMER ( %VAL(0) )
      END IF
      IF ( SPD%NFRQ > 0 ) THEN
           OPA = 0.0D0
           TAT = 0.0D0
      END IF
!
      IF ( SPD%CONF%SPD_ALG == SPD__ALG_LIN_Y      .OR. &
     &     SPD%CONF%SPD_ALG == SPD__ALG_LIN_YZ     .OR. &
     &     SPD%CONF%SPD_ALG == SPD__ALG_NONLOC     .OR. &
     &     SPD%CONF%SPD_ALG == SPD__ALG_Y_NONLOC   .OR. &
     &     SPD%CONF%SPD_ALG == SPD__ALG_NONLIN_LOC .OR. &
     &     SPD%CONF%SPD_ALG == SPD__ALG_STRAIGHT        ) THEN
!
! -------- A case of slanted path delay. We solve numerically the equations of 
! -------- wave propagation and integrate refractivity among the wave trajectory
!
           CALL ERR_PASS ( IUER, IER )
           CALL SPD_3D_SLANT ( SPD, VEC_GROUND_C3, MAT_XI_TO_X3, AZ, EL, &
     &                         NH, XI, RFR_VAL, RFR_DER, DEL, OPA, TAT, &
     &                         IVRB, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 5514, IUER, 'SPD_3D_DEL', 'Error in '// &
     &              'an attempt to compute slanted path delay by '// &
     &              'solving the system of differential equations' )
                RETURN 
           END IF
      END IF
!
! --- Decide whether we need compute atmospheric opaicity and brightness temperatyre
!
      FL_OPA_TAT = .FALSE.
      IF ( SPD%CONF%SOB_ALG == SOB__ALG_RTE_STRA  .AND.  SPD%NFRQ > 0 ) THEN
           FL_OPA_TAT = .TRUE.
      END IF
      IF ( SPD%CONF%SOB_ALG == SOB__ALG_RTE_STRA_1AZ  .AND.  &
     &     SPD%NFRQ > 0                               .AND.  &
     &     DABS(AZ) < AZ_EPS                                 ) THEN
           FL_OPA_TAT = .TRUE.
      END IF
!
      IF ( FL_OPA_TAT ) THEN
           DO 440 J4=1,SPD%NFRQ
              CALL ERR_PASS ( IUER, IER )
              CALL SOB_COMP ( NH, XI, ATT(1,J4), TEM, OPA(J4), TAT(J4), IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 5515, IUER, 'SPD_3D_DEL', 'Error in '// &
     &                 'an attempt to atmosphere opacity and atmosphere '// &
     &                 'brightness' )
                   RETURN 
              END IF
 440       CONTINUE 
      END IF
!
      IF ( IVRB == 7 ) THEN
           CALL WALL_TIMER ( STR )
           WRITE ( 6, * ) 'Slant_Del_b: ', STR(1:27)
           CALL FLUSH ( 6 )
      END IF
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  SPD_3D_DEL  !#!  
