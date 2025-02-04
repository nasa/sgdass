      SUBROUTINE MALO_COMP_AAM ( HEB_D, HEB_T, HEB_Q, HEB_U, HEB_V, HEB_IEH, &
     &                           HEB_OEH, HEB_LS, MAL, IMOM_NOIB, IMOM_IB, &
     &                           HMOM, TEST_STR, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  MALO_COMP_AAM
! *                                                                      *
! * ### 30-JUL-2015  MALO_COMP_AAM  v3.0 (c)  L. Petrov  12-JUN-2017 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'malo.i'
      INCLUDE   'heb.i'
      TYPE     ( HEB__TYPE  ) :: HEB_D, HEB_T, HEB_Q, HEB_U, HEB_V, HEB_IEH, &
     &                           HEB_OEH, HEB_LS
      TYPE     ( MALO__TYPE ) :: MAL
      REAL*8     IMOM_NOIB(3), IMOM_IB(3), HMOM(3)
      CHARACTER  TEST_STR*(*)
      INTEGER*4  IUER
      REAL*4,    ALLOCATABLE  :: U3D(:,:,:), V3D(:,:,:), W3D(:,:,:), &
     &                           LEV_R4(:), LAT_R4(:), LON_R4(:), &
     &                           LLAT_R4(:), LLON_R4(:), &
     &                           HLON_R4(:,:), ILON_NOIB_R4(:,:), ILON_IB_R4(:,:), &
     &                           HLAT_R4(:,:), ILAT_NOIB_R4(:,:), ILAT_IB_R4(:,:), &
     &                           ILON_LW_R4(:,:), ILON_WO_R4(:), ILON_LO_R4(:), &
     &                           ILAT_LW_R4(:,:), ILAT_WO_R4(:), ILAT_LO_R4(:)
      REAL*8     P(0:MALO__MHEI+1), PD(MALO__MHEI), PW(MALO__MHEI), &
     &           TEM(MALO__MHEI), HYPS_VAL(0:MALO__MHEI), HYPS_SPL(MALO__MHEI+1), &
     &           TMP(MALO__MHEI+2), H(0:MALO__MHEI), &
     &           GE, G, PHI, ZM(MALO__MHEI), Q0, Q(MALO__MHEI), &
     &           T_RATE, T_NLEV, LPL, LPH, RLEV, &
     &           LOGP_1D(MALO__MHEI),  LOGPW_1D(MALO__MHEI), &
     &           U_1D(MALO__MHEI),  V_1D(MALO__MHEI), &
     &           TEM_1D(MALO__MHEI),  &
     &           LOGP_SPL(MALO__MHEI), LOGPW_SPL(MALO__MHEI), &
     &           U_SPL(MALO__MHEI), V_SPL(MALO__MHEI), &
     &           TEM_SPL(MALO__MHEI),  &
     &           PRES_R8(MALO__MHEI), &
     &           HEI_R8(MALO__MHEI), PRES_VAL, TEMP, LOG_PRES, DH, &
     &           T1(MALO__MDIM), X1(MALO__MDIM), X2(MALO__MDIM)
      REAL*4     QV_LEV(MALO__MHEI), HEI_R4(MALO__MHEI), LAT_VAL, LON_VAL, &
     &           U_R4(1-MALO__MDEG:MALO__MHEI), V_R4(1-MALO__MDEG:MALO__MHEI), &
     &           W_R4(1-MALO__MDEG:MALO__MHEI), ARGS(3), &
     &           UH_INT, VH_INT, WH_INT, UH_INT_WATER, VH_INT_WATER, WH_INT_WATER
      REAL*8     EPS, PRES_HIGH, HEI_MIN, HEI_MIN_INT, HEI_MAX, &
     &           PRES_LN_0, PRES_LN_RATE, EXC_SQR_WGS84, HEI_TOA, HEI_RAN
      PARAMETER  ( PRES_HIGH    = 25000.0D0 )
      PARAMETER  ( HEI_MIN      =  -500.0D0 )
      PARAMETER  ( HEI_MIN_INT  = -1000.0D0 )
      PARAMETER  ( HEI_MAX      =  9000.0D0 )
      PARAMETER  ( HEI_TOA      = 80000.0D0 )
      PARAMETER  ( HEI_RAN      = 91000.0D0 )
      PARAMETER  ( PRES_LN_0    = 11.5476D0 )
      PARAMETER  ( PRES_LN_RATE = -1.323D-4 )
      PARAMETER  ( EPS = 1.D-4 )
      PARAMETER  ( EXC_SQR_WGS84 = 2.0D0*FLAT__WGS84 - FLAT__WGS84**2 )
      REAL*8     SPD__COMP_A0, SPD__COMP_A1, SPD__COMP_A2, SPD__COMP_B0, SPD__COMP_B1, &
     &           SPD__COMP_C0, SPD__COMP_C1, SPD__COMP_D0, SPD__COMP_E0, SPD__ABS_TEMP
      PARAMETER  ( SPD__COMP_A0 =  1.58123D-6   ) ! CIPM-2007
      PARAMETER  ( SPD__COMP_A1 = -2.9331D-8    ) ! CIPM-2007
      PARAMETER  ( SPD__COMP_A2 =  1.1043D-10   ) ! CIPM-2007
      PARAMETER  ( SPD__COMP_B0 =  5.707D-6     ) ! CIPM-2007
      PARAMETER  ( SPD__COMP_B1 = -2.051D-8     ) ! CIPM-2007
      PARAMETER  ( SPD__COMP_C0 =  1.9898D-4    ) ! CIPM-2007
      PARAMETER  ( SPD__COMP_C1 = -2.376D-6     ) ! CIPM-2007
      PARAMETER  ( SPD__COMP_D0 =  1.83D-11     ) ! CIPM-2007
      PARAMETER  ( SPD__COMP_E0 = -0.765D-8     ) ! CIPM-2007
      PARAMETER  ( SPD__ABS_TEMP = 273.15D0 )
      REAL*8       SPD__U1_GMAO72, SPD__U2_GMAO72, SPD__U3_GMAO72
      PARAMETER  ( SPD__U1_GMAO72 =    20.25319 )  !  MLEV = 125
      PARAMETER  ( SPD__U2_GMAO72 =  1200.00000 )  !  MLEV = 125
      PARAMETER  ( SPD__U3_GMAO72 =  -169.30782 )  !  MLEV = 125
      INTEGER*4    M_LEV
      REAL*8       MD, MW
      PARAMETER  ( M_LEV = MALO__MHEI )
      PARAMETER  ( MD    = MA__MAPL   )
      PARAMETER  ( MW    = H2O__MAPL  )
      CHARACTER  STR*128, INTEGR_LON_MODE*16
      INTEGER*4  L_LON, L_LAT, IND, K_LEV, L_LEV, N_LEV, I_LON, DIMS(3), &
     &           J1, J2, J3, J4, J5, J6, J7, J8, J9, J10, J11, J12, J13, &
     &           J14, J15, J16, J17, J18, NW, &
     &           INDS(3), IT_LON, IT_LAT, IND_LON, IND_LAT, KLON, KLAT, &
     &           JLON, JLAT, IER
      REAL*4,    ALLOCATABLE  :: PRS_SUR(:,:)
      REAL*8     HARR(2), TARR(2), MIN_VAL, MAX_VAL, RELL_HEI, H_ARG, &
     &           P_VAL, PW_VAL, TEM_VAL, RHO_VAL, ZM_VAL, U_VAL, V_VAL, &
     &           GMR_DRY, GMR_WET, TIM, LAT_TEST, LON_TEST, LO, WO, LW(3)
      LOGICAL*1  WATER_SPEEDUP, FL_MASS_CONS, FL_ALL_WATER
      INTEGER*4, EXTERNAL :: I_LEN, ILEN, IXMN8, IXMN4, IXMN4_S
      REAL*4,    EXTERNAL :: VAL_3D_BSPL4, VAL_3D_BSPLE3_R4, VAL_3D_BSPLE3J_R4, SPL4_INT
      REAL*8,    EXTERNAL :: ISPL8, FSPL8, WALL_TIMER
      CHARACTER, EXTERNAL :: MJDSEC_TO_DATE*30
real*4,    external :: val_3d_bsple3j_r4a ! %%%%
!
      FL_MASS_CONS    = .TRUE.
      INTEGR_LON_MODE = 'trapez'
      WATER_SPEEDUP   = .TRUE.
!
      TEST_STR     = 'none'  ! or 'timer'
      IF ( WATER_SPEEDUP ) THEN
           IF ( HEB_OEH%DIMS(1) > 5000 ) THEN
                NW = 9
             ELSE
                NW = 5
           END IF
         ELSE
           NW = 1
      END IF
!
! --- Grid of the input numerical models
!
      L_LON = HEB_D%DIMS(1)
      L_LAT = HEB_D%DIMS(2)
      L_LEV = HEB_D%DIMS(3)
!
      KLAT = 0
      KLON = 0
      JLAT = 0
      JLON = 0
!
! --- Grid of the output surface pressure field
!
      MAL%NLEV = M_LEV
      MAL%NLON = HEB_OEH%DIMS(1)
      MAL%NLAT = HEB_OEH%DIMS(2)
      MAL%NTIM = 1
      IF ( TEST_STR == 'plot' .OR. TEST_STR == 'print' ) THEN
           LAT_TEST = 60.0D0
           LON_TEST = 30.0D0
           KLAT = 1 + IDNINT ( (LAT_TEST*DEG__TO__RAD + P2I)/PI__NUM*(L_LAT-1)  )
           KLON = 1 + IDNINT (  LON_TEST*DEG__TO__RAD/PI2*L_LON )
           JLAT = 1 + IDNINT ( (LAT_TEST*DEG__TO__RAD + P2I)/PI__NUM*(MAL%NLAT-1)  )
           JLON = 1 + IDNINT (  LON_TEST*DEG__TO__RAD/PI2*MAL%NLON )
        ELSE IF ( TEST_STR == 'timer' ) THEN
           TIM = WALL_TIMER ( %VAL(0) )
      END IF
      LPL = PRES_LN_0 + PRES_LN_RATE*HEI_MIN
      LPH = PRES_LN_0 + PRES_LN_RATE*HEI_MAX
      DO 410 J1=1,M_LEV
         LOG_PRES = LPL + (J1-1)*(LPH - LPL)/(M_LEV-1)
         HEI_R8(J1)  = -PRES_LN_0/PRES_LN_RATE + LOG_PRES/PRES_LN_RATE
         HEI_R4(J1)  = HEI_R8(J1)
 410  CONTINUE 
!
! --- Allocate dynamic memory
!
      ALLOCATE ( U3D(1-MALO__MDEG:M_LEV,1-MALO__MDEG:L_LON+1,1-MALO__MDEG:L_LAT), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( 4*(M_LEV+MALO__MDEG)*(L_LON+1+MALO__MDEG)*(L_LAT+MALO__MDEG), STR )
           CALL ERR_LOG ( 4711, IUER, 'MALO_COMP_AAM', 'Failure in an attempt '// &
     &         'to allocate '//STR(1:I_LEN(STR))//' bytes of dynamic memory '// &
     &         'for array U3D' )
           RETURN 
      END IF
!
      ALLOCATE ( V3D(1-MALO__MDEG:M_LEV,1-MALO__MDEG:L_LON+1,1-MALO__MDEG:L_LAT), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( 4*(M_LEV+MALO__MDEG)*(L_LON+1+MALO__MDEG)*(L_LAT+MALO__MDEG), STR )
           CALL ERR_LOG ( 4712, IUER, 'MALO_COMP_AAM', 'Failure in an attempt '// &
     &         'to allocate '//STR(1:I_LEN(STR))//' bytes of dynamic memory '// &
     &         'for array U3D' )
           RETURN 
      END IF
!
      ALLOCATE ( W3D(1-MALO__MDEG:M_LEV,1-MALO__MDEG:L_LON+1,1-MALO__MDEG:L_LAT), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( 4*(M_LEV+MALO__MDEG)*(L_LON+1+MALO__MDEG)*(L_LAT+MALO__MDEG), STR )
           CALL ERR_LOG ( 4713, IUER, 'MALO_COMP_AAM', 'Failure in an attempt '// &
     &         'to allocate '//STR(1:I_LEN(STR))//' bytes of dynamic memory '// &
     &         'for array U3D' )
           RETURN 
      END IF
!
      ALLOCATE ( LAT_R4(1-MALO__MDEG:L_LAT+MALO__MDEG), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( 4*L_LAT, STR )
           CALL ERR_LOG ( 4714, IUER, 'MALO_COMP_AAM', 'Failure in an attempt '// &
     &         'to allocate '//STR(1:I_LEN(STR))//' bytes of dynamic memory '// &
     &         'for array LAT_R4' )
           RETURN 
      END IF
!
      ALLOCATE ( LON_R4(1-MALO__MDEG:L_LON+1+MALO__MDEG), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( 4*L_LON, STR )
           CALL ERR_LOG ( 4715, IUER, 'MALO_COMP_AAM', 'Failure in an attempt '// &
     &         'to allocate '//STR(1:I_LEN(STR))//' bytes of dynamic memory '// &
     &         'for array LON_R4' )
           RETURN 
      END IF
!
      ALLOCATE ( LEV_R4(1-MALO__MDEG:M_LEV+MALO__MDEG), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( 4*L_LON, STR )
           CALL ERR_LOG ( 4716, IUER, 'MALO_COMP_AAM', 'Failure in an attempt '// &
     &         'to allocate '//STR(1:I_LEN(STR))//' bytes of dynamic memory '// &
     &         'for array LEV_R4' )
           RETURN 
      END IF
!
      ALLOCATE ( LLAT_R4(1-MALO__MDEG:MAL%NLAT+MALO__MDEG), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( 4*(MAL%NLAT+1+2*MALO__MDEG), STR )
           CALL ERR_LOG ( 4717, IUER, 'MALO_COMP_AAM', 'Failure in an attempt '// &
     &         'to allocate '//STR(1:I_LEN(STR))//' bytes of dynamic memory '// &
     &         'for array LAT_R4' )
           RETURN 
      END IF
!
      ALLOCATE ( LLON_R4(1-MALO__MDEG:MAL%NLON+1+MALO__MDEG), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( 4*(MAL%NLON+1+2*MALO__MDEG), STR )
           CALL ERR_LOG ( 4718, IUER, 'MALO_COMP_AAM', 'Failure in an attempt '// &
     &         'to allocate '//STR(1:I_LEN(STR))//' bytes of dynamic memory '// &
     &         'for array LON_R4' )
           RETURN 
      END IF
!
      ALLOCATE ( ILON_NOIB_R4(1-MALO__MDEG:MAL%NLON+1+MALO__MDEG,3), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 4719, IUER, 'MALO_COMP_AAM', 'Failure in an attempt '// &
     &         'to allocate memory for array ILON_NOIB_R4' )
           RETURN 
      END IF
      ALLOCATE ( ILON_IB_R4(1-MALO__MDEG:MAL%NLON+1+MALO__MDEG,3), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 4720, IUER, 'MALO_COMP_AAM', 'Failure in an attempt '// &
     &         'to allocate memory for array ILON_IB_R4' )
           RETURN 
      END IF
      ALLOCATE ( ILON_LW_R4(1-MALO__MDEG:MAL%NLON+1+MALO__MDEG,3), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 4720, IUER, 'MALO_COMP_AAM', 'Failure in an attempt '// &
     &         'to allocate memory for array ILON_LW_R4' )
           RETURN 
      END IF
      ALLOCATE ( ILON_WO_R4(1-MALO__MDEG:MAL%NLON+1+MALO__MDEG), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 4720, IUER, 'MALO_COMP_AAM', 'Failure in an attempt '// &
     &         'to allocate memory for array ILON_WO_R4' )
           RETURN 
      END IF
      ALLOCATE ( ILON_LO_R4(1-MALO__MDEG:MAL%NLON+1+MALO__MDEG), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 4720, IUER, 'MALO_COMP_AAM', 'Failure in an attempt '// &
     &         'to allocate memory for array ILON_LO_R4' )
           RETURN 
      END IF
      ALLOCATE ( HLON_R4(1-MALO__MDEG:MAL%NLON+1+MALO__MDEG,3), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 4721, IUER, 'MALO_COMP_AAM', 'Failure in an attempt '// &
     &         'to allocate memory for array HLON_R4' )
           RETURN 
      END IF
!
      ALLOCATE ( ILAT_NOIB_R4(1-MALO__MDEG:MAL%NLAT+1+MALO__MDEG,3), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 4722, IUER, 'MALO_COMP_AAM', 'Failure in an attempt '// &
     &         'to allocate memory for array ILAT_NOIB_R4' )
           RETURN 
      END IF
!
      ALLOCATE ( ILAT_LW_R4(1-MALO__MDEG:MAL%NLAT+1+MALO__MDEG,3), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 4722, IUER, 'MALO_COMP_AAM', 'Failure in an attempt '// &
     &         'to allocate memory for array ILAT_LW_R4' )
           RETURN 
      END IF
!
      ALLOCATE ( ILAT_WO_R4(1-MALO__MDEG:MAL%NLAT+1+MALO__MDEG), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 4722, IUER, 'MALO_COMP_AAM', 'Failure in an attempt '// &
     &         'to allocate memory for array ILAT_WO_R4' )
           RETURN 
      END IF
      ALLOCATE ( ILAT_LO_R4(1-MALO__MDEG:MAL%NLAT+1+MALO__MDEG), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 4722, IUER, 'MALO_COMP_AAM', 'Failure in an attempt '// &
     &         'to allocate memory for array ILAT_LO_R4' )
           RETURN 
      END IF
!
      ALLOCATE ( ILAT_IB_R4(1-MALO__MDEG:MAL%NLAT+1+MALO__MDEG,3), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 4723, IUER, 'MALO_COMP_AAM', 'Failure in an attempt '// &
     &         'to allocate memory for array ILAT_IB_R4' )
           RETURN 
      END IF
      ALLOCATE ( HLAT_R4(1-MALO__MDEG:MAL%NLAT+1+MALO__MDEG,3), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 4724, IUER, 'MALO_COMP_AAM', 'Failure in an attempt '// &
     &         'to allocate memory for array HLAT_R4' )
           RETURN 
      END IF
!
! --- Initialization. Just in case.
!
      U3D = 0.0
      V3D = 0.0
      W3D = 0.0
      ILAT_NOIB_R4 = 0.0
      ILAT_IB_R4   = 0.0
      ILAT_LW_R4   = 0.0
      ILAT_LO_R4   = 0.0
      ILAT_WO_R4   = 0.0
      ILON_NOIB_R4 = 0.0
      ILON_IB_R4   = 0.0
      ILON_LW_R4   = 0.0
      ILON_LO_R4   = 0.0
      ILON_WO_R4   = 0.0
      HLAT_R4 = 0.0
      HLON_R4 = 0.0
      LEV_R4  = 0.0
      LAT_R4  = 0.0
      LON_R4  = 0.0
      LLAT_R4 = 0.0
      LLON_R4 = 0.0
!
! === Compute the state of the atmosphere from the output of the numeric 
! === weather model and compute fuinctions U3D, V3D, V3D
!
      DO 420 J2=1,L_LAT
!
! ------ PHI -- is geodetic latitude
!
         PHI = -P2I + (J2-1)*PI__NUM/(L_LAT-1)
         LAT_R4(J2) = PHI
!
! ------ Compute gravity on the reference ellipsoid for a given geodetic latitude PHI
!
         GE = ACC_EQU__WGS84*(1.D0 + GRV_LAT__WGS84*DSIN(PHI)**2)/ &
     &          DSQRT(1.D0 - (2.D0*FLAT__WGS84 - FLAT__WGS84**2)*DSIN(PHI)**2 )
         RELL_HEI = REA__WGS84 * &
     &             ( DSQRT(1.0D0 - EXC_SQR_WGS84*DSIN(PHI)**2) - 1.0D0 )
         DO 430 J3=1,L_LON
!
! --------- NB: Merra/Geos-FP/Geos-FPIT longitude starts from -180deg
! --------- I_LON -- longitude that starts from -180 deg
! --------- J3    -- longitude that starts from    0 deg
!
            I_LON = J3 + L_LON/2
            IF ( I_LON > L_LON ) I_LON = I_LON - L_LON
            LON_R4(J3) = (J3-1)*PI2/L_LON
!
! --------- Correct specific humidity if we can
!
            QV_LEV(1:L_LEV) = HEB_Q%VAL(I_LON,J2,1:L_LEV,1)
            CALL ERR_PASS ( IUER, IER )
            CALL CORRECT_SPEC_HUM ( L_LEV, QV_LEV, IER )
!
! --------- First run is for computing height above the ellipsoid 
! --------- for a given pressure level, the middle point of a layer
!
! --------- Re-indexing pressure in such a way that the 1-st level of 
! --------- array P starts at the nominal surface
!
            P(L_LEV+1) = 1.0D0
            N_LEV = L_LEV
            DO 440 J4=1,L_LEV
               K_LEV = L_LEV + 1 - J4
               IF ( J4 == 1 ) THEN
                    P(K_LEV) = P(K_LEV+1) + HEB_D%VAL(I_LON,J2,J4,1)/2.0D0
                  ELSE 
                    P(K_LEV) = P(K_LEV+1) + HEB_D%VAL(I_LON,J2,J4-1,1)/2.0D0 + &
     &                                      HEB_D%VAL(I_LON,J2,J4,1)/2.0D0 
               END IF
               IF ( P(K_LEV) < PRES_HIGH ) N_LEV = K_LEV
!
! ------------ Compute approximate gravity acceleration at a given pressure level
!
               G = GE*(1.D0 + GP0 + GP1*DLOG(P(K_LEV)))
!
! ------------ PW -- parital pressure of water vapour
!
               Q(K_LEV)    = QV_LEV(J4)
               PW(K_LEV)   = P(K_LEV) * Q(K_LEV) / &
     &                                ( MW/MD + (1.0D0 - MW/MD)*Q(K_LEV) )
               U_1D(K_LEV) = HEB_U%VAL(I_LON,J2,J4,1)
               V_1D(K_LEV) = HEB_V%VAL(I_LON,J2,J4,1)
!
! ------------ Partial pressure of dry air
!
               PD(K_LEV)   = P(K_LEV) - PW(K_LEV)
               TEM(K_LEV)  = HEB_T%VAL(I_LON,J2,J4,1)
!
! ------------ Compute water vapor compressibility
!
               ZM(K_LEV) = 1.D0/ &
     &              ( 1.0D0 &
     &                - P(K_LEV)/TEM(K_LEV)*  (SPD__COMP_A0 + SPD__COMP_A1*(TEM(K_LEV) - SPD__ABS_TEMP) +  &
     &                                                        SPD__COMP_A2*(TEM(K_LEV) - SPD__ABS_TEMP)**2 ) &
     &                + PW(K_LEV)/TEM(K_LEV)* (SPD__COMP_B0 + SPD__COMP_B1*(TEM(K_LEV) - SPD__ABS_TEMP)) &
     &                + PW(K_LEV)**2/(P(K_LEV)*TEM(K_LEV))* &
     &                                      (SPD__COMP_C0 + SPD__COMP_C1*(TEM(K_LEV) - SPD__ABS_TEMP)) &
     &                + P(K_LEV)**2/TEM(K_LEV)**2*  SPD__COMP_D0 &
     &                + PW(K_LEV)**2/TEM(K_LEV)**2* SPD__COMP_E0 &
     &              )
               HYPS_VAL(K_LEV) = R__MAPL*TEM(K_LEV)/ &
     &                          (G*(MD*PD(K_LEV) + MW*PW(K_LEV))*ZM(K_LEV))
 440        CONTINUE 
            P(0) = P(1) + HEB_D%VAL(I_LON,J2,L_LEV,1)/2.0D0
            HYPS_VAL(0) = HYPS_VAL(1) - (P(1) - P(0))*(HYPS_VAL(2) - HYPS_VAL(1))/(P(2) - P(1))
!
! --------- Special trick for making interpolation: we reverse the sign of pressure.
! --------- Otherwise, MAKE_SPLINE will complain that 
! --------- the array of arguments is not in the rising order
!
            P = -P ! For interpolation
            CALL ERR_PASS ( IUER, IER ) 
            CALL MAKE_SPLINE ( 3, L_LEV+1, P, HYPS_VAL, 0.0D0, 0.0D0, HYPS_SPL, TMP, IER )
            IF ( IER .NE. 0 ) THEN
                 CALL ERR_LOG ( 4725, IUER, 'MALO_COMP_AAM', 'Failure in an attempt '// &
     &                'to compute coefficients of the interpolating spline' )
                 RETURN 
            END IF
!
! --------- Get the height above the reference ellipsoid at the bottom pressure levels 
! --------- NB: HEB_IEH has longitude running from -180 to +180 degrees
!
            H(0) = HEB_IEH%VAL(I_LON,J2,1,1)
            DO 450 J5=1,L_LEV
!
! ------------ NB: index for P, HYPS starts from 0, but ISPL8 expects them
! ------------ to start from 1. Therefore, its 5 and 6th artugments are 
! ------------ augmented by 1.
!
               IF ( J5 < L_LEV ) THEN
                    H(J5) = H(J5-1) + ISPL8 ( P(J5), L_LEV+1, P, HYPS_VAL, &
     &                                        J5, J5+1, HYPS_SPL, IER )
                  ELSE IF ( J5 == L_LEV ) THEN
                    H(J5) = H(J5-1) + ISPL8 ( P(J5)*(1.D0-EPS), L_LEV+1, P, HYPS_VAL, &
     &                                        J5, J5+1, HYPS_SPL, IER )
               END IF
 450        CONTINUE 
!
! --------- Compute the lapse rate between N_LEV/2 and N_LEV
!
            CALL ERR_PASS ( IUER, IER )
            CALL REGR8 ( N_LEV - N_LEV/2 + 1, H(N_LEV/2), TEM(N_LEV/2), &
     &                   T_RATE, T_NLEV, IER )
            IF ( TEST_STR == 'temp'  .AND.  I_LON == IT_LON  .AND.  J2 == IT_LAT ) THEN
                 HARR(1) = H(1)
                 HARR(2) = H(N_LEV)
                 TARR(1) = T_NLEV + T_RATE*(H(1) - H(N_LEV/2))
                 TARR(2) = T_NLEV + T_RATE*(H(N_LEV) - H(N_LEV/2))
                 WRITE ( 6, * ) 'Test: Lat= ', SNGL((-P2I + (IT_LAT-1)*PI__NUM/(L_LAT-1))/DEG__TO__RAD), &
     &                          'Lon= ', SNGL((IT_LON-1)*PI2/L_LON/DEG__TO__RAD)
                 CALL DIAGI_2 ( N_LEV, H, TEM, 2, HARR, TARR, IER )
            END IF
!
! --------- Second round
!
            DO 460 J6=1,L_LEV
!
! ------------ We compute gravity at a given height
!
               G = GE*(1.D0 - 2.D0/REA__WGS84*(1.0D0 + OMEGA__EGM96**2*REA__WGS84**2* &
     &                (1.D0 - FLAT__WGS84)/GM__EGM96 + (1.D0 - 2.D0*FLAT__WGS84)* &
     &                 DSIN(PHI)**2)*H(J6) + 3.D0/REA__WGS84**2*H(J6)**2)
!
! ------------ Compute improved function for integration
!
               HYPS_VAL(J6) = R__MAPL*TEM(J6)/(G*(MD*PD(J6) + MW*PW(J6))*ZM(J6))
 460        CONTINUE 
            HYPS_VAL(0) = HYPS_VAL(1) - (P(1) - P(0))*(HYPS_VAL(2) - HYPS_VAL(1))/(P(2) - P(1))
!
! --------- Compute interpolating spline of H(P) the second time
!
            CALL ERR_PASS ( IUER, IER ) 
            CALL MAKE_SPLINE ( 3, L_LEV+1, P, HYPS_VAL, 0.0D0, 0.0D0, HYPS_SPL, TMP, IER )
            IF ( IER .NE. 0 ) THEN
                 CALL ERR_LOG ( 4726, IUER, 'MALO_COMP_AAM', 'Failure in an attempt '// &
     &                'to compute coefficients of the interpolating spline' )
                 RETURN 
            END IF
!
! --------- Compute height as a function of pressure the second time.
! --------- Also store ln(P)
!
            H(0) = HEB_IEH%VAL(I_LON,J2,1,1) ! NB: HEB_IEH%VAL has longitude running from -180 to +180 degrees
            DO 470 J7=1,L_LEV
               IF ( J7 < L_LEV ) THEN
                    H(J7) = H(J7-1) + ISPL8 ( P(J7), L_LEV+1, P, HYPS_VAL, &
     &                                        J7, J7+1, HYPS_SPL, IER )
                  ELSE IF ( J5 == L_LEV ) THEN
                    H(J7) = H(J7-1) + ISPL8 ( P(J7)*(1.D0-EPS), L_LEV+1, P, HYPS_VAL, &
     &                                        J7, J7+1, HYPS_SPL, IER )
               END IF
               LOGP_1D(J7)  = LOG(-P(J7))
               LOGPW_1D(J7) = DLOG(PW(J7))
               TEM_1D(J7)   = TEM(J7)
 470        CONTINUE 
!
! --------- Now we compute the interpolation spline for the opposite problem:
! --------- we would like to model dependence of ln(P), ln(Pw) and Temp
! --------- as a function of geometrical height above the ellipsoid
!
            CALL ERR_PASS ( IUER, IER ) 
            CALL MAKE_SPLINE ( 3, L_LEV, H(1), LOGP_1D, 0.0D0, 0.0D0, LOGP_SPL, TMP, IER )
            IF ( IER .NE. 0 ) THEN
                 CALL ERR_LOG ( 4727, IUER, 'MALO_COMP_AAM', 'Failure in an attempt '// &
     &                'to compute coefficients of the interpolating spline' )
                 RETURN 
            END IF
!
            CALL ERR_PASS ( IUER, IER ) 
            CALL MAKE_SPLINE ( 3, L_LEV, H(1), LOGPW_1D, 0.0D0, 0.0D0, LOGPW_SPL, TMP, IER )
            IF ( IER .NE. 0 ) THEN
                 CALL ERR_LOG ( 4728, IUER, 'MALO_COMP_AAM', 'Failure in an attempt '// &
     &                'to compute coefficients of the interpolating spline' )
                 RETURN 
            END IF
!
            CALL ERR_PASS ( IUER, IER ) 
            CALL MAKE_SPLINE ( 3, L_LEV, H(1), TEM_1D, 0.0D0, 0.0D0, TEM_SPL, TMP, IER )
            IF ( IER .NE. 0 ) THEN
                 CALL ERR_LOG ( 4729, IUER, 'MALO_COMP_AAM', 'Failure in an attempt '// &
     &                'to compute coefficients of the interpolating spline' )
                 RETURN 
            END IF
!
! --------- Compute interpolation spline for the wind
!
            CALL ERR_PASS ( IUER, IER ) 
            CALL MAKE_SPLINE ( 3, L_LEV, H(1), U_1D, 0.0D0, 0.0D0, U_SPL, TMP, IER )
            IF ( IER .NE. 0 ) THEN
                 CALL ERR_LOG ( 4730, IUER, 'MALO_COMP_AAM', 'Failure in an attempt '// &
     &                'to compute coefficients of the interpolating spline' )
                 RETURN 
            END IF
!
            CALL ERR_PASS ( IUER, IER ) 
            CALL MAKE_SPLINE ( 3, L_LEV, H(1), V_1D, 0.0D0, 0.0D0, V_SPL, TMP, IER )
            IF ( IER .NE. 0 ) THEN
                 CALL ERR_LOG ( 4731, IUER, 'MALO_COMP_AAM', 'Failure in an attempt '// &
     &                'to compute coefficients of the interpolating spline' )
                 RETURN 
            END IF
!
! --------- Now we compute the state of the atmosphere at the global grid
!
            DO 480 J8=1,M_LEV
               RLEV = J8-1 - (MALO__MHEI-1)/2
               HEI_R8(J8) = DEXP ( (RLEV - SPD__U3_GMAO72)/SPD__U1_GMAO72 ) - SPD__U2_GMAO72
               LEV_R4(J8) = HEI_R8(J8) 
               G = GE*(1.D0 - 2.D0/REA__WGS84*(1.0D0 + OMEGA__EGM96**2*REA__WGS84**2* &
     &                (1.D0 - FLAT__WGS84)/GM__EGM96 + (1.D0 - 2.D0*FLAT__WGS84)* &
     &                 DSIN(PHI)**2)*HEI_R8(J8) + 3.D0/REA__WGS84**2*HEI_R8(J8)**2)
               IF ( HEI_R8(J8) < H(1) ) THEN
!
! ---------------- This height level is below the surface. We assume the atmosphere
! ---------------- beneath the surface obeys to the adiabatic gas law with a constant 
! ---------------- lapse rate computed at the previous step
!
                   TEM_VAL =  TEM(1) + T_RATE*(HEI_R8(J8) - H(1))
                   PW_VAL  =  PW(1)*(TEM_VAL/TEM(1))**(-G*MW/(R__MAPL*T_RATE))
                   P_VAL   = -P(1) *(TEM_VAL/TEM(1))**(-G*MD/(R__MAPL*T_RATE))
!
! ---------------- We consider wind below the nominal surface is the same
! ---------------- as at the first model level
!
                   U_VAL   = U_1D(1)
                   V_VAL   = V_1D(1)
                 ELSE IF ( HEI_R8(J8) > H(L_LEV) ) THEN
!
! ---------------- This is above the upper level. We consider the atmosphere isothermal.
!
                   GMR_DRY = G*MD/R__MAPL
                   GMR_WET = G*MW/R__MAPL
                   TEM_VAL =  TEM(L_LEV)
                   PW_VAL  =  PW(L_LEV)*DEXP ( -GMR_WET*(HEI_R8(J8) - H(L_LEV))/TEM_VAL )
                   P_VAL   =  -P(L_LEV)*DEXP ( -GMR_DRY*(HEI_R8(J8) - H(L_LEV))/TEM_VAL )
!
! ---------------- We consider wind above the nominal top of the atmosphere is zero
!
                   U_VAL   = 0.D0
                   V_VAL   = 0.D0
                 ELSE
!
! ---------------- Compute air temperature, U-wind, V-wind, partial pressure of
! ---------------- water vapor, and the total pressure by spline interpolation
!
                   IND = IXMN8 ( L_LEV, H(1), HEI_R8(J8) )
                   TEM_VAL =        FSPL8 ( HEI_R8(J8), L_LEV, H(1), TEM_1D,   IND, TEM_SPL ) 
                   U_VAL   =        FSPL8 ( HEI_R8(J8), L_LEV, H(1), U_1D,     IND, U_SPL   )
                   V_VAL   =        FSPL8 ( HEI_R8(J8), L_LEV, H(1), V_1D,     IND, V_SPL   )
                   PW_VAL  = DEXP ( FSPL8 ( HEI_R8(J8), L_LEV, H(1), LOGPW_1D, IND, LOGPW_SPL ) )
                   P_VAL   = DEXP ( FSPL8 ( HEI_R8(J8), L_LEV, H(1), LOGP_1D,  IND, LOGP_SPL  ) )
               END IF
!
! ------------ Compute inverse compressibility of moist air
!
               ZM_VAL = 1.D0/ &
     &              ( 1.0D0 &
     &                - P_VAL/TEM_VAL*  (SPD__COMP_A0 + SPD__COMP_A1*(TEM_VAL - SPD__ABS_TEMP) +  &
     &                                                      SPD__COMP_A2*(TEM_VAL - SPD__ABS_TEMP)**2 ) &
     &                + PW_VAL/TEM_VAL* (SPD__COMP_B0 + SPD__COMP_B1*(TEM_VAL - SPD__ABS_TEMP)) &
     &                + PW_VAL**2/(P_VAL*TEM_VAL)* &
     &                                  (SPD__COMP_C0 + SPD__COMP_C1*(TEM_VAL - SPD__ABS_TEMP)) &
     &                + P_VAL**2/TEM_VAL**2*  SPD__COMP_D0 &
     &                + PW_VAL**2/TEM_VAL**2* SPD__COMP_E0 &
     &              )
!
! ------------ Get the height argument for the innter integration
!
               H_ARG = (HEI_R8(J8) - RELL_HEI)/REA__WGS84
!
! ------------ Get the density of moist air
!
               RHO_VAL = ((P_VAL - PW_VAL)*MD + PW_VAL*MW)*ZM_VAL/(R__MAPL*TEM_VAL)
!
! ------------ Compute elements of 3D U,V,W functions
!
               U3D(J8,J3,J2) = RHO_VAL*(1.D0 + 3.D0*H_ARG + 3.D0*H_ARG**2)*U_VAL
               V3D(J8,J3,J2) = RHO_VAL*(1.D0 + 3.D0*H_ARG + 3.D0*H_ARG**2)*V_VAL
               W3D(J8,J3,J2) = RHO_VAL*(1.D0 + 4.D0*H_ARG + 6.D0*H_ARG**2)
               IF ( TEST_STR == 'plot' .AND. ( J3 == KLON .AND. J2 == KLAT ) ) THEN
                    TMP(J8) = W3D(J8,J3,J2) 
               END IF
 480        CONTINUE 
            IF ( TEST_STR == 'plot' .AND. ( J3 == KLON .AND. J2 == KLAT ) ) THEN
                 CALL DIAGI_1 ( M_LEV, HEI_R8, TMP, IER )
            END IF
 430     CONTINUE 
!
! ------ We extend longitude coverage to one pixel eastward.
! ------ The pixel with longitude index L_LON+1 is equal to the pixel with index 1
!
         U3D(1-MALO__MDEG:M_LEV,L_LON+1,J2) = U3D(1-MALO__MDEG:M_LEV,1,J2) 
         V3D(1-MALO__MDEG:M_LEV,L_LON+1,J2) = V3D(1-MALO__MDEG:M_LEV,1,J2) 
         W3D(1-MALO__MDEG:M_LEV,L_LON+1,J2) = W3D(1-MALO__MDEG:M_LEV,1,J2) 
 420  CONTINUE 
      LON_R4(L_LON+1) = PI2
!
      IF ( TEST_STR == 'timer' ) THEN
           WRITE ( 6, '("UVW computing    time: ", F8.3)' ) WALL_TIMER ( %VAL(2) )
           TIM = WALL_TIMER ( %VAL(0) )
      END IF
!
      DIMS(1) = M_LEV
      DIMS(2) = L_LON+1
      DIMS(3) = L_LAT
!
! --- Extend array of levels, longitude, and latitudes MALO__MDEG at
! --- the beginning at MALO__MDEG at the end of the range at MALO__MDEG.
! --- This is needed in order to use an efficient B-spline algorithm
!
      DO 4110 J11=1,MALO__MDEG
         LEV_R4(1-J11) = LEV_R4(1) - EPS*(LEV_R4(2)-LEV_R4(1))*J11
         LON_R4(1-J11) = LON_R4(1) - EPS*(LON_R4(2)-LON_R4(1))*J11
         LAT_R4(1-J11) = LAT_R4(1) - EPS*(LAT_R4(2)-LAT_R4(1))*J11
         LEV_R4(M_LEV+J11)   = LEV_R4(M_LEV)   + EPS*(LEV_R4(M_LEV)   - LEV_R4(M_LEV-1))*J11
         LON_R4(L_LON+1+J11) = LON_R4(L_LON+1) + EPS*(LON_R4(L_LON+1) - LON_R4(L_LON))*J11
         LAT_R4(L_LAT+J11)   = LAT_R4(L_LAT)   + EPS*(LAT_R4(L_LAT)   - LAT_R4(L_LAT-1))*J11
 4110 CONTINUE 
!
! === Expand U3D, V3D, and W3D into 3D B-spline basis
!
      CALL ERR_PASS ( IUER, IER )
      CALL BSPL4_3D_CMP ( MALO__MDEG, 0, DIMS, &
     &                    LEV_R4(1), LON_R4(1), LAT_R4(1), &
     &                    U3D, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 4732, IUER, 'MALO_COMP_AAM', 'Failure in '// &
     &         'an attempt to compute coefficients of the 3D '// &
     &         'interpolating spline for U3D' )
           RETURN 
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL BSPL4_3D_CMP ( MALO__MDEG, 0, DIMS, &
     &                    LEV_R4(1), LON_R4(1), LAT_R4(1), &
     &                    V3D, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 4733, IUER, 'MALO_COMP_AAM', 'Failure in '// &
     &         'an attempt to compute coefficients of the 3D '// &
     &         'interpolating spline for V3D' )
           RETURN 
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL BSPL4_3D_CMP ( MALO__MDEG, 0, DIMS, LEV_R4(1), &
     &                    LON_R4(1), LAT_R4(1), &
     &                    W3D, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 4734, IUER, 'MALO_COMP_AAM', 'Failure in '// &
     &         'an attempt to compute coefficients of the 3D '// &
     &         'interpolating spline for W3D' )
           RETURN 
      END IF
!
      IF ( TEST_STR == 'timer' ) THEN
           WRITE ( 6, '("Spline building time:  ", F8.3)' ) WALL_TIMER ( %VAL(2) )
           TIM = WALL_TIMER ( %VAL(0) )
      END IF
!
! === Compute the 3D integrals of functions of U3D, V3D, W3D
!
      DO 4120 J12=1,MAL%NLAT
!
! ------ LLAT_R4 is the GEOCENTRIC latitude of the integration grid
!
         LLAT_R4(J12) = -P2I + (J12-1)*PI__NUM/(MAL%NLAT-1)
!
! ------ ARGS(3) is the GEODETIC latitude that corresponds to LLAT_R4(J12)
!
         IF ( J12 == 1 ) THEN
              ARGS(3) = LLAT_R4(J12) + EPS*PI__NUM/(MAL%NLAT-1)
              INDS(3) = 1
            ELSE IF ( J12 == MAL%NLAT ) THEN
              ARGS(3) = LLAT_R4(J12) - EPS*PI__NUM/(MAL%NLAT-1)
              INDS(3) = DIMS(3) - 1
            ELSE
              ARGS(3) = ATAN ( TAN(LLAT_R4(J12))/(1.0 - MALO__FLAT_WGS84)**2 )
              INDS(3) = IXMN4 ( DIMS(3), LAT_R4(1), ARGS(3) )
         END IF
!
         FL_ALL_WATER = .FALSE.
         DO 4130 J13=1,MAL%NLON+1
!
! --------- LLON_R4 is the longitude of the integration grid
!
            LLON_R4(J13) = (J13-1)*PI2/MAL%NLON
            ARGS(2) = LLON_R4(J13) 
            INDS(2) = IXMN4 ( DIMS(2), LON_R4(1), ARGS(2) )
            IF ( J13 == 1 ) THEN
                 ARGS(2) = LON_R4(1) + EPS*PI2/MAL%NLON
                 INDS(2) = 1
            END IF
            IF ( J13 == MAL%NLON+1 ) THEN
                 ARGS(2) = LLON_R4(MAL%NLON+1) - EPS*PI2/MAL%NLON
                 INDS(2) = DIMS(2) - 1
            END IF
!
! --------- The lowest level of integration is the surface height on the 
! --------- output gird
!
            IF ( J13 == MAL%NLON+1 ) THEN
                 ARGS(1) = HEB_OEH%VAL(1,J12,1,1)
               ELSE
                 ARGS(1) = HEB_OEH%VAL(J13,J12,1,1)
            END IF
            INDS(1) = IXMN4 ( DIMS(1), LEV_R4(1), ARGS(1) )
!
! --------- I. Integrate from the surface height to the top of the atmosphere 
! --------- on the output grid
!
!!            UH_INT = VAL_3D_BSPLE3J_R4 ( ARGS, DIMS, INDS, &
!!     &                                   LEV_R4, LON_R4, LAT_R4, U3D )
!!            VH_INT = VAL_3D_BSPLE3J_R4 ( ARGS, DIMS, INDS, &
!!     &                                   LEV_R4, LON_R4, LAT_R4, V3D )
!!            WH_INT = VAL_3D_BSPLE3J_R4 ( ARGS, DIMS, INDS, &
!!     &                                   LEV_R4, LON_R4, LAT_R4, W3D )
!!
!
! --------- A special trick to speed up computation: if for next NW points over 
! --------- the longitude there is only ocean, we compute the integral 
! --------- at the center point and assign other NW points to this value.
! --------- The height is supposed to be equal over ocean, so the oversampling
! --------- has no effect. This trick speeds up computation by a factor of 4-6
!
            IF ( NW > 1 .AND. MOD(J13,NW) == 0 .AND. J13 < MAL%NLON-NW ) THEN
!
! -------------- check whether this an and all NW-1 next points are all water.
!
                 FL_ALL_WATER = .TRUE.
                 DO 4160 J16=J13,J13+NW-1
                    IF ( HEB_LS%VAL(J16,J12,1,1) > 0.002 ) FL_ALL_WATER = .FALSE.
 4160            CONTINUE 
                 IF ( FL_ALL_WATER ) THEN
!
! ------------------- Yes, it is all water. Then compute the integrals for
! ------------------- the central point of the  NW-point batch
!
                      LLON_R4(J13+(NW-1)/2) = (J13+(NW-1)/2-1)*PI2/MAL%NLON
                      ARGS(2) = LLON_R4(J13+(NW-1)/2) 
                      INDS(2) = IXMN4 ( DIMS(2), LON_R4(1), ARGS(2) )
                      ARGS(1) = HEB_OEH%VAL(J13+(NW-1)/2,J12,1,1)
                      INDS(1) = IXMN4 ( DIMS(1), LEV_R4(1), ARGS(1) )
!
                      CALL VAL_3D_BSPLE3J_R4_V3 ( ARGS, DIMS, INDS, &
     &                                            LEV_R4, LON_R4, LAT_R4, &
     &                                            U3D, V3D, W3D, &
     &                                            UH_INT_WATER, VH_INT_WATER, WH_INT_WATER )
                 END IF
               ELSE IF ( NW > 1 .AND. MOD(J13,NW) == 0 .AND. J13 .GE. MAL%NLON-NW ) THEN
                 FL_ALL_WATER = .FALSE.
            END IF
!
            IF ( NW > 1 .AND. FL_ALL_WATER ) THEN
!
! -------------- All water
!
                 UH_INT = UH_INT_WATER
                 VH_INT = VH_INT_WATER
                 WH_INT = WH_INT_WATER
              ELSE IF ( .NOT. FL_ALL_WATER ) THEN
!
! -------------- Compute three integrals for U3D, V3D, and W3D
!
                 CALL VAL_3D_BSPLE3J_R4_V3 ( ARGS, DIMS, INDS, &
     &                                       LEV_R4, LON_R4, LAT_R4, &
     &                                       U3D, V3D, W3D, &
     &                                       UH_INT, VH_INT, WH_INT )
            END IF
!
! --------- II. Compute the function under integral on the longitude stripe
! --------- We do it for all three components of the mass term and
! --------- the motion term
!
            ILON_NOIB_R4(J13,1) = -WH_INT * COS(LLON_R4(J13)) * &
     &                                      COS(LLAT_R4(J12))**2*SIN(LLAT_R4(J12))
            ILON_NOIB_R4(J13,2) = -WH_INT * SIN(LLON_R4(J13)) * &
     &                                      COS(LLAT_R4(J12))**2*SIN(LLAT_R4(J12))
            ILON_NOIB_R4(J13,3) =  WH_INT * COS(LLAT_R4(J12))**3
!
! --------- In a case of IB model, we multiply the mass term by the land-sea mask
!
            IF ( J13  == MAL%NLON +1 ) THEN
                 IND_LON = 1
               ELSE 
                 IND_LON = J13
            END IF
!
            ILON_IB_R4(J13,1:3) =  HEB_LS%VAL(IND_LON,J12,1,1)*ILON_NOIB_R4(J13,1:3) 
            ILON_WO_R4(J13)   = (1.0 - HEB_LS%VAL(IND_LON,J12,1,1))*WH_INT*COS(LLAT_R4(J12)) 
            ILON_LO_R4(J13)   = (1.0 - HEB_LS%VAL(IND_LON,J12,1,1))*COS(LLAT_R4(J12)) 
            ILON_LW_R4(J13,1) = (1.0 - HEB_LS%VAL(IND_LON,J12,1,1))*COS(LLON_R4(J13)) * &
     &                                 COS(LLAT_R4(J12))**2*SIN(LLAT_R4(J12))
            ILON_LW_R4(J13,2) = (1.0 - HEB_LS%VAL(IND_LON,J12,1,1))*SIN(LLON_R4(J13)) * &
     &                                 COS(LLAT_R4(J12))**2*SIN(LLAT_R4(J12))
            ILON_LW_R4(J13,3) = (1.0 - HEB_LS%VAL(IND_LON,J12,1,1))*COS(LLAT_R4(J12))**3
!
            HLON_R4(J13,1) = ( -UH_INT*SIN(LLAT_R4(J12))*COS(LLON_R4(J13)) &
     &                         +VH_INT*SIN(LLON_R4(J13)) )*COS(LLAT_R4(J12))
            HLON_R4(J13,2) = ( -UH_INT*SIN(LLAT_R4(J12))*SIN(LLON_R4(J13)) &
     &                         -VH_INT*COS(LLON_R4(J13)) )*COS(LLAT_R4(J12))
            HLON_R4(J13,3) =    UH_INT*COS(LLAT_R4(J12))**2
!
            IF ( J13 == JLON .AND. J12 == JLAT ) THEN
                 WRITE ( 6, * ) 'UVW = ', UH_INT, VH_INT, WH_INT
                 WRITE ( 6, * ) 'LS = ', HEB_LS%VAL(J13,J12,1,1)
                 WRITE ( 6, * ) 'ILON_NOIB= ', ILON_NOIB_R4(J13,1:3)
                 WRITE ( 6, * ) 'ILON_IB= ', ILON_IB_R4(J13,1:3)
                 WRITE ( 6, * ) 'HLON= ', HLON_R4(J13,1:3)
            END IF
            IF ( TEST_STR == 'stripe' ) THEN
                 T1(J13) = LLON_R4(J13)/DEG__TO__RAD
                 X1(J13) = ILON_NOIB_R4(J13,3)
                 X2(J13) = ILON_IB_R4(J13,3)
            END IF 
 4130    CONTINUE 
         IF ( TEST_STR == 'stripe' .AND. &
     &        LLAT_R4(J12) > 20.D0*DEG__TO__RAD .AND. &
     &        MOD(J12,8) == 1 ) THEN
!
              CALL CLRCH ( STR )
              WRITE ( UNIT=STR(1:8), FMT='(F8.4)' ) LLAT_R4(J12)/DEG__TO__RAD
              CALL DIAGI_SETDEF ( IER, 'DIAGI_CTIT', 'Lat: '//STR(1:8)  )
              CALL DIAGI_2 ( MAL%NLON+1, T1, X1, MAL%NLON+1, T1, X2, IER )
         END IF
!
         DO 4150 J15=1,3
            IF ( INTEGR_LON_MODE == 'spline' ) THEN
!
! -------------- Expand the ILON_NOIB_R4, ILON_IB_R4, and HLON_R4 into 
! -------------- 1D B-spline basis over longitude
!
                 CALL ERR_PASS ( IUER, IER )
                 CALL BSPL4_1D_CMP ( MALO__MDEG, 0, MAL%NLON+1, LLON_R4(1), &
     &                               ILON_NOIB_R4(1-MALO__MDEG,J15), IER )
                 IF ( IER .NE. 0 ) THEN
                      CALL ERR_LOG ( 4735, IUER, 'MALO_COMP_AAM', 'Failure in '// &
     &                    'an attempt to compute coefficients of the 3D '// &
     &                    'interpolating spline for ILON_NOIB_R4' )
                      RETURN 
                 END IF
!
                 CALL ERR_PASS ( IUER, IER )
                 CALL BSPL4_1D_CMP ( MALO__MDEG, 0, MAL%NLON+1, LLON_R4(1), &
     &                               ILON_IB_R4(1-MALO__MDEG,J15), IER )
                 IF ( IER .NE. 0 ) THEN
                      CALL ERR_LOG ( 4736, IUER, 'MALO_COMP_AAM', 'Failure in '// &
     &                    'an attempt to compute coefficients of the 3D '// &
     &                    'interpolating spline for ILON_IB_R4' )
                      RETURN 
                 END IF
!
                 CALL ERR_PASS ( IUER, IER )
                 CALL BSPL4_1D_CMP ( MALO__MDEG, 0, MAL%NLON+1, LLON_R4(1), &
     &                               HLON_R4(1-MALO__MDEG,J15), IER )
                 IF ( IER .NE. 0 ) THEN
                      CALL ERR_LOG ( 4737, IUER, 'MALO_COMP_AAM', 'Failure in '// &
     &                    'an attempt to compute coefficients of the 3D '// &
     &                    'interpolating spline for HLON_R4' )
                      RETURN 
                 END IF
!
                 CALL ERR_PASS ( IUER, IER )
                 CALL BSPL4_1D_CMP ( MALO__MDEG, 0, MAL%NLON+1, LLON_R4(1), &
     &                               ILON_LW_R4(1-MALO__MDEG,J15), IER )
                 IF ( IER .NE. 0 ) THEN
                      CALL ERR_LOG ( 4737, IUER, 'MALO_COMP_AAM', 'Failure in '// &
     &                    'an attempt to compute coefficients of the 3D '// &
     &                    'interpolating spline for ILON_LW_R4' )
                      RETURN 
                 END IF
!
                 IF ( J15 == 3 ) THEN
                      CALL ERR_PASS ( IUER, IER )
                      CALL BSPL4_1D_CMP ( MALO__MDEG, 0, MAL%NLON+1, LLON_R4(1), &
     &                                    ILON_WO_R4(1-MALO__MDEG), IER )
                      IF ( IER .NE. 0 ) THEN
                           CALL ERR_LOG ( 4737, IUER, 'MALO_COMP_AAM', 'Failure in '// &
     &                         'an attempt to compute coefficients of the 3D '// &
     &                         'interpolating spline for ILON_WO_R4' )
                           RETURN
                      END IF
!
                      CALL ERR_PASS ( IUER, IER )
                      CALL BSPL4_1D_CMP ( MALO__MDEG, 0, MAL%NLON+1, LLON_R4(1), &
     &                                    ILON_LO_R4(1-MALO__MDEG), IER )
                      IF ( IER .NE. 0 ) THEN
                           CALL ERR_LOG ( 4737, IUER, 'MALO_COMP_AAM', 'Failure in '// &
     &                         'an attempt to compute coefficients of the 3D '// &
     &                         'interpolating spline for ILON_LO_R4' )
                           RETURN
                      END IF
                 END IF
!
! -------------- ... II. integrate over longitude from 0 to pi2
!
                 ILAT_NOIB_R4(J12,J15) = SPL4_INT ( MAL%NLON+1, LLON_R4(1), MALO__MDEG, &
     &                                              ILON_NOIB_R4(1-MALO__MDEG,J15), PI2 )
                 ILAT_IB_R4(J12,J15)   = SPL4_INT ( MAL%NLON+1, LLON_R4(1), MALO__MDEG, &
     &                                              ILON_IB_R4(1-MALO__MDEG,J15), PI2 )
                 HLAT_R4(J12,J15)      = SPL4_INT ( MAL%NLON+1, LLON_R4(1), MALO__MDEG, &
     &                                              HLON_R4(1-MALO__MDEG,J15), PI2 )
                 ILAT_LW_R4(J12,J15)   = SPL4_INT ( MAL%NLON+1, LLON_R4(1), MALO__MDEG, &
     &                                              ILON_LW_R4(1-MALO__MDEG,J15), PI2 )
                 IF ( J15 == 3 ) THEN
                      ILAT_WO_R4(J12)  = SPL4_INT ( MAL%NLON+1, LLON_R4(1), MALO__MDEG, &
     &                                              ILON_WO_R4(1-MALO__MDEG), PI2 )
                      ILAT_LO_R4(J12)  = SPL4_INT ( MAL%NLON+1, LLON_R4(1), MALO__MDEG, &
     &                                              ILON_LO_R4(1-MALO__MDEG), PI2 )
                 END IF
              ELSE IF ( INTEGR_LON_MODE == 'trapez' ) THEN
!
! -------------- Trapez algorithm. It is much fast then spline and is sufficient
! -------------- of the model resolution is hight
!
                 ILAT_NOIB_R4(J12,J15) = 0.0
                 ILAT_IB_R4(J12,J15)   = 0.0
                 HLAT_R4(J12,J15)      = 0.0
                 ILAT_LW_R4(J12,J15)   = 0.0
                 IF ( J15 == 3 ) THEN
                      ILAT_WO_R4(J12)  = 0.0
                      ILAT_LO_R4(J12)  = 0.0
                 END IF
!
! -------------- Just summing
!
                 DO 4170 J17=1,MAL%NLON
                    ILAT_NOIB_R4(J12,J15) = ILAT_NOIB_R4(J12,J15) + ILON_NOIB_R4(J17,J15) 
                    ILAT_IB_R4(J12,J15)   = ILAT_IB_R4(J12,J15)   + ILON_IB_R4(J17,J15)
                    HLAT_R4(J12,J15)      = HLAT_R4(J12,J15)      + HLON_R4(J17,J15)
                    ILAT_LW_R4(J12,J15)   = ILAT_LW_R4(J12,J15)   + ILON_LW_R4(J17,J15)
                    IF ( J15 == 3 ) THEN
                         ILAT_WO_R4(J12)  = ILAT_WO_R4(J12) + ILON_WO_R4(J17)
                         ILAT_LO_R4(J12)  = ILAT_LO_R4(J12) + ILON_LO_R4(J17)
                    END IF
 4170            CONTINUE 
!
! -------------- ... and then scaling the sum to get the integral
!
                 ILAT_NOIB_R4(J12,J15) = PI2/MAL%NLON*ILAT_NOIB_R4(J12,J15)
                 ILAT_IB_R4(J12,J15)   = PI2/MAL%NLON*ILAT_IB_R4(J12,J15)
                 HLAT_R4(J12,J15)      = PI2/MAL%NLON*HLAT_R4(J12,J15)
                 ILAT_LW_R4(J12,J15)   = PI2/MAL%NLON*ILAT_LW_R4(J12,J15)
                 IF ( J15 == 3 ) THEN
                      ILAT_WO_R4(J12)  = PI2/MAL%NLON*ILAT_WO_R4(J12)
                      ILAT_LO_R4(J12)  = PI2/MAL%NLON*ILAT_LO_R4(J12)
                 END IF
              ELSE 
                 CALL ERR_LOG ( 4738, IUER, 'MALO_COMP_AAM', 'Trap of internal '// &
     &               'control: unknown parameter INTEGR_LON_MODE = '//INTEGR_LON_MODE )
              RETURN 
            END IF
 4150    CONTINUE
!!         IF ( TEST_STR == 'timer' ) WRITE ( 6, '("2. integration TIME:  ", F11.6)' ) WALL_TIMER ( %VAL(2) )
 4120 CONTINUE 
      IF ( TEST_STR == 'timer' ) THEN
           WRITE ( 6, '("Ver. integration time:  ", F8.3)' ) WALL_TIMER ( %VAL(2) )
           TIM = WALL_TIMER ( %VAL(0) )
      END IF
      DO 4180 J18=1,3
!
! ------ Expand the ILAT_NOIB_R4, ILAT_IB_R4, ILAT_LW_R4, and HLAT_R4 into 1D B-spline 
! ------ basis over latitude
!
         CALL ERR_PASS ( IUER, IER )
         CALL BSPL4_1D_CMP ( MALO__MDEG, 0, MAL%NLAT, LLAT_R4(1), &
     &                       ILAT_NOIB_R4(1-MALO__MDEG,J18), IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 4738, IUER, 'MALO_COMP_AAM', 'Failure in '// &
     &            'an attempt to compute coefficients of the 3D '// &
     &            'interpolating spline for ILAT_NOIB_R4' )
              RETURN 
         END IF
!
         CALL ERR_PASS ( IUER, IER )
         CALL BSPL4_1D_CMP ( MALO__MDEG, 0, MAL%NLAT, LLAT_R4(1), &
     &                       ILAT_IB_R4(1-MALO__MDEG,J18), IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 4739, IUER, 'MALO_COMP_AAM', 'Failure in '// &
     &            'an attempt to compute coefficients of the 3D '// &
     &            'interpolating spline for ILAT_IB_R4' )
              RETURN 
         END IF
!
         CALL ERR_PASS ( IUER, IER )
         CALL BSPL4_1D_CMP ( MALO__MDEG, 0, MAL%NLAT, LLAT_R4(1), &
     &                       HLAT_R4(1-MALO__MDEG,J18), IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 4740, IUER, 'MALO_COMP_AAM', 'Failure in '// &
     &            'an attempt to compute coefficients of the 3D '// &
     &            'interpolating spline for HLAT_R4' )
              RETURN 
         END IF
!
         CALL ERR_PASS ( IUER, IER )
         CALL BSPL4_1D_CMP ( MALO__MDEG, 0, MAL%NLAT, LLAT_R4(1), &
     &                       ILAT_LW_R4(1-MALO__MDEG,J18), IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 4740, IUER, 'MALO_COMP_AAM', 'Failure in '// &
     &            'an attempt to compute coefficients of the 3D '// &
     &            'interpolating spline for ILAT_LW_R4' )
              RETURN 
         END IF
         IF ( J18 == 3 ) THEN
              CALL ERR_PASS ( IUER, IER )
              CALL BSPL4_1D_CMP ( MALO__MDEG, 0, MAL%NLAT, LLAT_R4(1), &
     &                            ILAT_WO_R4(1-MALO__MDEG), IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 4740, IUER, 'MALO_COMP_AAM', 'Failure in '// &
     &                 'an attempt to compute coefficients of the 3D '// &
     &                 'interpolating spline for ILAT_WO_R4' )
                   RETURN 
              END IF
!
              CALL ERR_PASS ( IUER, IER )
              CALL BSPL4_1D_CMP ( MALO__MDEG, 0, MAL%NLAT, LLAT_R4(1), &
     &                            ILAT_LO_R4(1-MALO__MDEG), IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 4740, IUER, 'MALO_COMP_AAM', 'Failure in '// &
     &                 'an attempt to compute coefficients of the 3D '// &
     &                 'interpolating spline for ILAT_LO_R4' )
                   RETURN 
              END IF
         END IF
!
! ------ ... III. integrate over latitude from pi/2 to +pi/2
!
         IMOM_NOIB(J18) = REA__WGS84**4 * &
     &                    SPL4_INT ( MAL%NLAT, LLAT_R4(1), MALO__MDEG, &
     &                               ILAT_NOIB_R4(1-MALO__MDEG,J18), P2I )
         IMOM_IB(J18)   = REA__WGS84**4 * &
     &                    SPL4_INT ( MAL%NLAT, LLAT_R4(1), MALO__MDEG, &
     &                               ILAT_IB_R4(1-MALO__MDEG,J18), P2I )
         HMOM(J18) = REA__WGS84**3 * &
     &               SPL4_INT ( MAL%NLAT, LLAT_R4(1), MALO__MDEG, &
     &                          HLAT_R4(1-MALO__MDEG,J18), P2I )
         LW(J18)   = SPL4_INT ( MAL%NLAT, LLAT_R4(1), MALO__MDEG, &
     &                          ILAT_LW_R4(1-MALO__MDEG,J18), P2I )
 4180 CONTINUE
      WO = SPL4_INT ( MAL%NLAT, LLAT_R4(1), MALO__MDEG, &
     &                ILAT_WO_R4(1-MALO__MDEG), P2I )
      LO = SPL4_INT ( MAL%NLAT, LLAT_R4(1), MALO__MDEG, &
     &                ILAT_LO_R4(1-MALO__MDEG), P2I )
      IMOM_IB(1) = IMOM_IB(1) - REA__WGS84**4 * WO/LO * LW(1)
      IMOM_IB(2) = IMOM_IB(2) - REA__WGS84**4 * WO/LO * LW(2)
      IMOM_IB(3) = IMOM_IB(3) + REA__WGS84**4 * WO/LO * LW(3)
!
      IF ( TEST_STR == 'timer' ) THEN
           WRITE ( 6, '("Hor. integration time:  ", F8.3)' ) WALL_TIMER ( %VAL(2) )
           TIM = WALL_TIMER ( %VAL(0) )
      END IF
      DEALLOCATE ( HLAT_R4 ) 
      DEALLOCATE ( ILAT_IB_R4   ) 
      DEALLOCATE ( ILAT_NOIB_R4 ) 
      DEALLOCATE ( ILAT_LW_R4 ) 
      DEALLOCATE ( ILAT_LO_R4 ) 
      DEALLOCATE ( ILAT_WO_R4 ) 
      DEALLOCATE ( HLON_R4 ) 
      DEALLOCATE ( ILON_LW_R4 ) 
      DEALLOCATE ( ILON_LO_R4 ) 
      DEALLOCATE ( ILON_WO_R4 ) 
      DEALLOCATE ( ILON_IB_R4   ) 
      DEALLOCATE ( ILON_NOIB_R4 ) 
      DEALLOCATE ( LON_R4  ) 
      DEALLOCATE ( LAT_R4  ) 
      DEALLOCATE ( LLON_R4 ) 
      DEALLOCATE ( LLAT_R4 ) 
      DEALLOCATE ( LEV_R4 ) 
      DEALLOCATE ( U3D ) 
      DEALLOCATE ( V3D ) 
      DEALLOCATE ( W3D ) 
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  MALO_COMP_AAM  !#!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE VAL_3D_BSPLE3J_R4_V3 ( ARGS, DIMS, INDS, &
     &                                  ARG_ARR_1, ARG_ARR_2, ARG_ARR_3, &
     &                                  BCF1, BCF2, BCF3, &
     &                                  VAL1, VAL2, VAL3 )
! ************************************************************************
! *                                                                      *
! *   Routine VAL_3D_BSPLE3J_R4  computes the integral over the          *
! *   dimension 1 from ARGS(1) to ARGS(DIMS(1)) at the 3D field of       *
! *   function expanded over B-spline basis of the DEG th degree.        *
! *   at the point with coordinates (x, ARGS(2), ARGS(3)).              *
! *                                                                      *
! *   NB: Vectors ARGS_ARR_1, ARGS_ARR_2, and ARGS_ARR_3 are so-called   *
! *   expanded vectors of knots. They have have three extra knots before *
! *   the first knot and three extra knots after the last knots:         *
! *                                                                      *
! *   -2, -1, 0,    1, 2, 3, ... n-1, n,   n+1, n+2, n+3                 *
! *                                                                      *
! *   Arguments between expanded knots should have a small positive      *
! *   step. Recommended step at the beginning of the sequence of knots:  *
! *   eps*(arg[2] - arg[1]), where eps=1.0e-5. Analogously, the step     *
! *   at the end is recommended to be eps*(arg[n] - arg[n-1]).           *
! *                                                                      *
! *   NB: VAL_3D_BSPLE3J_R4 does not make a check of validity of         *
! *   extended knots. If the step is too big, the result will be wrong.  *
! *   If the step is too short, VAL_3D_BSPLE3J_R4 will crash.            *
! *                                                                      *
! *   It is recommended to use routine BSPLE3_EXTEND_R4 for computing    *
! *   extended knots.                                                    *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *      ARGS ( REAL*4    ) -- Vector of coordinate of the point along   *
! *                            dimensions. Dimension: 3.                 *
! *       DEG ( INTEGER*4 ) -- Degree of the spline.                     *
! *      DIMS ( INTEGER*4 ) -- Array of 3 dimensions.                    *
! *      INDS ( INTEGER*4 ) -- Array of indexes of a pivotal element     *
! *                            along each of dimsions. The pivotal       *
! *                            element is the index of the maximal       *
! *                            argument on the mesh that does not exceed *
! *                            the coordinate along the same dimension.  *
! * ARG_ARR_1 ( REAL*4    ) -- Array of arguments over the first         *
! *                            dimension. Dimension: [-2:DIMS(1)+3].     *
! * ARG_ARR_2 ( REAL*4    ) -- Array of arguments over the first         *
! *                            dimension. Dimension: [-2:DIMS(2)+3].     *
! * ARG_ARR_3 ( REAL*4    ) -- Array of arguments over the first         *
! *                            dimension. Dimension: [-2:DIMS(3)+3].     *
! *       BCF ( REAL*4    ) -- Array of B-spline coefficients. Dimension:*
! *           (1-DEG:DIMS(1),1-DEG:DIMS(2),1-DEG:DIMS(3)).               *
! *                                                                      *
! * ________________________ Output parameters: ________________________ *
! *                                                                      *
! * <VAL_3D_BSPLE3J_R4> ( REAL*4 ) -- Integral among dimension 1         *
! *                                   in limits ARG_ARR_1(1) to          *
! *                                   ARG_ARR_1(DIMS(1)) at point        *
! *                                   (x, ARGS(2), ARGS(3)) of the       *
! *                                   function epxaned into 3D B-spline. *
! *                                                                      *
! * # 12-JUN-2017 VAL_3D_BSPLE3J_R4_V3 v1.0 (c) L. Petrov 12-JUN-2017 ## *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INTEGER*4  INDS(3), DIMS(3)
      INTEGER*4    DEG, MD
      PARAMETER  ( DEG = 3 )
      PARAMETER  ( MD  = 4096 )
      REAL*4     ARGS(3), &
     &           ARG_ARR_1(1-DEG:DIMS(1)+DEG), &
     &           ARG_ARR_2(1-DEG:DIMS(2)+DEG), &
     &           ARG_ARR_3(1-DEG:DIMS(3)+DEG), &
     &           BCF1(1-DEG:DIMS(1),1-DEG:DIMS(2),1-DEG:DIMS(3)), &
     &           BCF2(1-DEG:DIMS(1),1-DEG:DIMS(2),1-DEG:DIMS(3)), &
     &           BCF3(1-DEG:DIMS(1),1-DEG:DIMS(2),1-DEG:DIMS(3)), &
     &           VAL1, VAL2, VAL3
      REAL*4     VAL
      REAL*4     BSPL(-DEG:0,3), BSPLJ(-DEG:MD), DSPL(-DEG:0,3)
      INTEGER*4  J1, J2, J3, J4, J5, J6
      REAL*4,    EXTERNAL :: BSPLE3_VAL_R4, BSPL4_JNT 
!
! --- Check dimensions
!
      IF ( INDS(1) < 1 .OR. &
     &     INDS(2) < 1 .OR. &
     &     INDS(3) < 1      ) THEN
!
           VAL1 = -1.E30
           VAL2 = -1.E30
           VAL3 = -1.E30
           RETURN 
      END IF
!
! --- Compute values of 1D B-splines
!
      DO 410 J1=INDS(1)-DEG,DIMS(1)-1
         BSPLJ(J1) = BSPL4_JNT ( DIMS(1), ARG_ARR_1(1), DEG, J1, ARGS(1) )
 410  CONTINUE 
!
      DO 420 J2=-DEG,0
         BSPL(J2,2) = BSPLE3_VAL_R4 ( DIMS(2), ARG_ARR_2, INDS(2)+J2, ARGS(2) )
 420  CONTINUE 
      DO 430 J3=-DEG,0
         BSPL(J3,3) = BSPLE3_VAL_R4 ( DIMS(3), ARG_ARR_3, INDS(3)+J3, ARGS(3) )
 430  CONTINUE 
!
! --- Compute the tensor product of B-splines
!
      VAL1 = 0.0
      VAL2 = 0.0
      VAL3 = 0.0
      DO 440 J4=-DEG,0
         DO 450 J5=-DEG,0
            DO 460 J6=INDS(1)-DEG,DIMS(1)-1
               VAL1 = VAL1 + &
     &                BCF1(J6,INDS(2)+J5,INDS(3)+J4)* &
     &                BSPLJ(J6)*BSPL(J5,2)*BSPL(J4,3)
               VAL2 = VAL2 + &
     &                BCF2(J6,INDS(2)+J5,INDS(3)+J4)* &
     &                BSPLJ(J6)*BSPL(J5,2)*BSPL(J4,3)
               VAL3 = VAL3 + &
     &                BCF3(J6,INDS(2)+J5,INDS(3)+J4)* &
     &                BSPLJ(J6)*BSPL(J5,2)*BSPL(J4,3)
 460        CONTINUE 
 450     CONTINUE 
 440  CONTINUE 
!
      RETURN
      END  SUBROUTINE  VAL_3D_BSPLE3J_R4_V3  !#!#
