#define NO_SERIAL
      SUBROUTINE GEN_TRG_DENS ( HEB_DELP, HEB_T, HEB_Q, HEB_NAT_OH, HEB_O3, &
     &                          HEB_CO2, HEB_FINE_OH, HEB_TRG_3D, HEB_TRG_2D, &
     &                          DIMO, MODE, REF_HEI, CNS_DR2, IVRB, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  GEN_TRG_DENS computes the coefficients of expansion of    *
! *                                                                      *
! * ### 27-MAR-2018 GEN_TRG_DENS    v2.0 (c) L. Petrov  07-APR-2018  ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'malo.i'
      INCLUDE   'heb.i'
      TYPE     ( HEB__TYPE  ) :: HEB_DELP, HEB_T, HEB_Q, HEB_NAT_OH, HEB_O3, &
     &                           HEB_CO2, HEB_FINE_OH, HEB_TRG_3D, HEB_TRG_2D
      INTEGER*4  DIMO(3), IVRB, IUER
      CHARACTER  MODE*(*)
      REAL*8     REF_HEI, CNS_DR2(2)
!!!!!!
#ifdef GNU
      INTEGER*4  NTHR,    NTHR_SAVED
#else
      ADDRESS__TYPE NTHR, NTHR_SAVED
#endif
      INTEGER*4    SPD__MLEV, DEG, SPD__LBOT, SPD__LTOP, &
     &             M_TYP, M__O3, M__CO2, M__ACP, M__RH, M__DEW, M__TEM, M__O3M, M__CO2M
      PARAMETER  ( SPD__MLEV    =  125 )
      PARAMETER  ( DEG          =    3 )
      PARAMETER  ( M_TYP        =    8 )
      PARAMETER  ( M__O3        =    1 )
      PARAMETER  ( M__CO2       =    2 )
      PARAMETER  ( M__ACP       =    3 )
      PARAMETER  ( M__RH        =    4 )
      PARAMETER  ( M__DEW       =    5 )
      PARAMETER  ( M__TEM       =    6 )
      PARAMETER  ( M__O3M       =    7 )
      PARAMETER  ( M__CO2M      =    8 )
      PARAMETER  ( SPD__LBOT    =   12 ) ! Extra knots below the bottom
      PARAMETER  ( SPD__LTOP    =    5 ) ! Extra knots below the top
      REAL*8       SPD__U1_GMAO72, SPD__U2_GMAO72, SPD__U3_GMAO72
      PARAMETER  ( SPD__U1_GMAO72 =    20.25319 )  !  MLEV = 125
      PARAMETER  ( SPD__U2_GMAO72 =  1200.00000 )  !  MLEV = 125
      PARAMETER  ( SPD__U3_GMAO72 =  -169.30782 )  !  MLEV = 125
!
      REAL*8       SPD__U_FLO, SPD__U_MIN, SPD__U_MAX
      PARAMETER  ( SPD__U_FLO     = -4000.0D0  )
      PARAMETER  ( SPD__U_MIN     = -1000.0D0  )
      PARAMETER  ( SPD__U_MAX     = 90000.0D0  )
!
      REAL*8     SPD__REA_WGS84, SPD__FLAT_WGS84, SPD__GM_EGM96, SPD__OMEGA_EGM96, &
     &           SPD__ACC_EQU_WGS84, SPD__GRV_LAT_WGS84, SPD__W0_IAU2004
      PARAMETER  ( SPD__REA_WGS84     = 6378137.0D0 )
      PARAMETER  ( SPD__FLAT_WGS84    = 1.0D0/298.2572235630D0 )
      PARAMETER  ( SPD__GM_EGM96      = 3.986004418D14 )
      PARAMETER  ( SPD__OMEGA_EGM96   = 7.292115D-5 )
      PARAMETER  ( SPD__ACC_EQU_WGS84 = 9.7803253359D0     ) ! Equatorial gravity acc.
      PARAMETER  ( SPD__GRV_LAT_WGS84 = 0.00193185265241D0 ) ! D(ACC_EQU)/D(phi)
      PARAMETER  ( SPD__W0_IAU2004    = 62636856.0D0       ) ! m^2/s^2 Geopotential value defined by
!                                                          ! M. Bursa etal, The geopotential value 
!                                                          ! W0 for specifying the relativistic atomic 
!                                                          ! time scale and a global vertical reference system, 
!                                                          ! JoG, 81(2)2, pp 103-110, 2007
!
! --- Thermodynamic constants
!
      REAL*8     SPD__R, SPD__MA, SPD__H2O, SPD__CO2, SPD__NOM_CO2, &
     &           SPD__ABS_TEMP, SPD__MD, SPD__MW, SPD__H2O_DRY_RAT
      PARAMETER  ( SPD__R        = 8.314472D0   ) ! CIPM-2007
      PARAMETER  ( SPD__MA       = 0.02896546D0 ) ! CIPM-2007
      PARAMETER  ( SPD__H2O      = 0.01801528D0 ) ! CIPM-2007
      PARAMETER  ( SPD__CO2      = 0.012011D0   ) ! CIPM-2007
      PARAMETER  ( SPD__NOM_CO2  = 0.004D0      ) ! CIPM-2007
      PARAMETER  ( SPD__MD       = SPD__MA      ) ! CIPM-2007
      PARAMETER  ( SPD__MW       = SPD__H2O     ) ! CIPM-2007
      PARAMETER  ( SPD__ABS_TEMP = 273.15D0     )
      REAL*8     SPD__ENH_ALPHA, SPD__ENH_BETA, SPD__ENH_GAMMA
      PARAMETER  ( SPD__ENH_ALPHA = 1.00062D0   )
      PARAMETER  ( SPD__ENH_BETA  = 3.14D-8     )
      PARAMETER  ( SPD__ENH_GAMMA = 5.6D-7      )
      REAL*8     SPD__PSV_A, SPD__PSV_B, SPD__PSV_C, SPD__PSV_D
      PARAMETER  ( SPD__PSV_A =  1.2378847D-5   ) ! CIPM-2007
      PARAMETER  ( SPD__PSV_B = -1.9121316D-2   ) ! CIPM-2007
      PARAMETER  ( SPD__PSV_C = 33.93711047D0   ) ! CIPM-2007
      PARAMETER  ( SPD__PSV_D = -6.3431645D3    ) ! CIPM-2007
      REAL*8     SPD__COMP_A0, SPD__COMP_A1, SPD__COMP_A2, SPD__COMP_B0, SPD__COMP_B1, &
     &           SPD__COMP_C0, SPD__COMP_C1, SPD__COMP_D0, SPD__COMP_E0
      PARAMETER  ( SPD__COMP_A0 =  1.58123D-6   ) ! CIPM-2007
      PARAMETER  ( SPD__COMP_A1 = -2.9331D-8    ) ! CIPM-2007
      PARAMETER  ( SPD__COMP_A2 =  1.1043D-10   ) ! CIPM-2007
      PARAMETER  ( SPD__COMP_B0 =  5.707D-6     ) ! CIPM-2007
      PARAMETER  ( SPD__COMP_B1 = -2.051D-8     ) ! CIPM-2007
      PARAMETER  ( SPD__COMP_C0 =  1.9898D-4    ) ! CIPM-2007
      PARAMETER  ( SPD__COMP_C1 = -2.376D-6     ) ! CIPM-2007
      PARAMETER  ( SPD__COMP_D0 =  1.83D-11     ) ! CIPM-2007
      PARAMETER  ( SPD__COMP_E0 = -0.765D-8     ) ! CIPM-2007
      REAL*8       SPD__CO2_CONC
      PARAMETER  ( SPD__CO2_CONC = 0.00375D0 ) 
      PARAMETER  ( SPD__H2O_DRY_RAT = SPD__H2O/(SPD__MA + SPD__CO2*(SPD__CO2_CONC - SPD__NOM_CO2)) )
!
      REAL*8     SPD__REF_D0, SPD__REF_D1, SPD__REF_D2, SPD__REF_D3
      REAL*8     SPD__REF_W0, SPD__REF_W1, SPD__REF_W2, SPD__REF_W3, SPD__CF
      PARAMETER  ( SPD__REF_D0 =  2.380185D+14 ) ! d/l
      PARAMETER  ( SPD__REF_D1 =  5.792105D+10 ) ! 1/m^2
      PARAMETER  ( SPD__REF_D2 =  5.7362D+13   ) ! 1/m^2
      PARAMETER  ( SPD__REF_D3 =  1.67917D+09  ) ! 1/m^2
      PARAMETER  ( SPD__CF    =   1.022D0      ) ! 1/m^2
!
      PARAMETER  ( SPD__REF_W0 =  2.95235D-6   ) ! d/l
      PARAMETER  ( SPD__REF_W1 =  2.6422D-20   ) ! 1/m^2
      PARAMETER  ( SPD__REF_W2 = -3.2380D-34   ) ! 1/m^4
      PARAMETER  ( SPD__REF_W3 =  4.028D-47    ) ! 1/m^6
      REAL*8     PD__REF, PW__REF, TEM__D_REF, TEM__W_REF
      PARAMETER  ( PD__REF     = 101325.0D0  )
      PARAMETER  ( PW__REF     =   1333.0D0  )
      PARAMETER  ( TEM__D_REF  =    288.15D0 )
      PARAMETER  ( TEM__W_REF  =    293.15D0 )
      REAL*8     TEMP_K, A1_DEW, B1_DEW, C1_DEW, EPS_PW, ACP_MIN, RH_MIN, RH_MAX
      PARAMETER  ( TEMP_K = 273.16D0 )
      PARAMETER  ( A1_DEW = 17.625D0 )
      PARAMETER  ( B1_DEW = 243.04D0 )
      PARAMETER  ( C1_DEW = 610.94D0 )
      PARAMETER  ( EPS_PW = 1.D-10   )
      PARAMETER  ( ACP_MIN = 0.00001 )
      PARAMETER  ( RH_MIN  = 0.0001  )
      PARAMETER  ( RH_MAX  = 1.0000  )
!                                                          ! JoG, 81(2)2, pp 103-110, 2007
! ----- GMAO constants
!
      REAL*8       SPD__R_MAPL, SPD__MA_MAPL, SPD__H2O_MAPL, SPD__ACCREF_MAPL, SPD__KAPPA_MAPL
      PARAMETER  ( SPD__R_MAPL      = 8.3143D0 )    ! Universal gas constant
      PARAMETER  ( SPD__MA_MAPL     = 28.97D-3 )    ! Molar mass of dry air
      PARAMETER  ( SPD__H2O_MAPL    = 18.01D-3 )    ! Molar mass of wet air
      PARAMETER  ( SPD__ACCREF_MAPL = 9.80D0   )    ! Reference gravity acceleration
      PARAMETER  ( SPD__KAPPA_MAPL  = 2.0D0/7.0D0 ) ! Adiabatic constant
      REAL*8       SPD__GP0, SPD__GP1     ! Dependence of gravity acceleration on Pressure according to ISO atmosphere
      PARAMETER  ( SPD__GP0 = -2.427388D-02 ) ! g(P) = G_ell*(1.0D + GP0 + GP1*ln(P))
      PARAMETER  ( SPD__GP1 =  2.088507D-03 )
!!!!!!
      REAL*8     P(0:SPD__MLEV), PD(SPD__MLEV), PW(SPD__MLEV), &
     &           TEM(SPD__MLEV), HYPS(0:SPD__MLEV), &
     &           TMP_R8(SPD__MLEV+DEG), H(0:SPD__MLEV), &
     &           GE, G, LAT_GDT, ZM_VAL, ZM(SPD__MLEV), &
     &           T_RATE, T_NLEV, LPL, LPH, PRES_R8(SPD__MLEV), &
     &           HEI_VAL, HEI_R8(SPD__MLEV), PRES_VAL, TEMP, LOG_PRES, &
     &           Q(SPD__MLEV), LOGP_1D(SPD__MLEV), LOGP_M1D(SPD__MLEV), LOGPW_1D(SPD__MLEV), &
     &           TEM_1D(SPD__MLEV), TDEW_1D(SPD__MLEV), SPL_H(SPD__MLEV+DEG), &
     &           SPL_P(SPD__MLEV+DEG), SPL_PW(SPD__MLEV+DEG), &
     &           SPL_TEM(SPD__MLEV+DEG), SPL_TDEW(SPD__MLEV+DEG), &
     &           PW_LEV, GMR_DRY, GMR_WET, &
     &           O3_DENS(SPD__MLEV), LOG_O3_DENS(SPD__MLEV), O3_SPL(SPD__MLEV+DEG), &
     &           CO2_DENS(SPD__MLEV), LOG_CO2_DENS(SPD__MLEV), CO2_SPL(SPD__MLEV+DEG), &
     &           AIR_DENS, TEM_VAL, PW_VAL, P_VAL, O3_VAL, CO2_VAL, &
     &           LOG_CO2_DENS_LOWLEV, LOG_O3_DENS_LOWLEV, &
     &           LOG_CO2_DENS_RATE, LOG_O3_DENS_RATE, ARGS(3), &
     &           RH1, RH_VAL, TDEW_VAL, ACP_VAL, &
     &           O3M_VAL,  O3M_1D(SPD__MLEV+DEG), O3M_SPL(SPD__MLEV+DEG), &
     &           CO2M_VAL, CO2M_1D(SPD__MLEV+DEG), CO2M_SPL(SPD__MLEV+DEG)
      REAL*4     HEI_R4(SPD__MLEV)
      REAL*8     LAT_VAL, LON_VAL, CNS_DR2_LON
      INTEGER*8  MEL
      REAL*8     EPS, PRES_HIGH, HEI_MIN, HEI_MIN_INT, HEI_MAX, &
     &           PRES_LN_0, PRES_LN_RATE, SPD__APS_TEMP, MD, MW, &
     &           PRES_LEV, LOG_PRES_LEV
      PARAMETER  ( PRES_HIGH    = 25000.0D0 )
      PARAMETER  ( HEI_MIN      =  -500.0D0 )
      PARAMETER  ( HEI_MIN_INT  = -1000.0D0 )
      PARAMETER  ( HEI_MAX      =  9000.0D0 )
      PARAMETER  ( PRES_LN_0    = 11.5476D0 )
      PARAMETER  ( PRES_LN_RATE = -1.323D-4 )
      PARAMETER  ( PRES_LEV     = 25000.0D0 )
      PARAMETER  ( LOG_PRES_LEV = DLOG(PRES_LEV) )
      PARAMETER  ( MD = SPD__MA_MAPL  )
      PARAMETER  ( MW = SPD__H2O_MAPL )
      PARAMETER  ( EPS = 1.D-5 )
      LOGICAL*1  FL_ATT_INTR
      CHARACTER  STR*128, STR1*128, TEST_STR*8
      INTEGER*4  L_LON, L_LAT, L_LEV, K_LON, K_LAT, K_LEV, I_LEV,IND, &
     &           N_LEV, I_LON, DIMS(3), INDS(3), &
     &           J1, J2, J3, J4, J5, J6, J7, J8, J9, J10, J11, &
     &           J12, J13, J14, J15, J16, J17, J18, J19, J20, J21, J22, &
     &           J23, J24, IT_LON, IT_LAT, IND_P, IND_PW, IND_TEM, &
     &           NLEV, NLON, NLAT, IND_THR, IER
      REAL*4     QV_LEV(SPD__MLEV)
      REAL*8     T1(8192), X1(8192), X2(8192), R1, R2
      REAL*8     HARR(2), TARR(2), MIN_VAL, MAX_VAL , LON_TST, LAT_TST, &
     &           RLEV
      REAL*8,    ALLOCATABLE :: LEV(:), LON(:), LAT(:), &
     &           LEV_SMO(:), &
     &           TRG_NAT_VAL(:,:,:,:), TRG_SMO_VAL(:,:,:,:), &
     &           RES_VAL(:,:), RES_SPL(:,:), PRES_LEV_SPL(:,:)
      LOGICAL*1  FL_ERROR
      INTEGER*4, EXTERNAL :: I_LEN, ILEN, IXMN8, IXMN8_S, IXMN4, IXMN4_S
      LOGICAL*4, EXTERNAL :: OMP_IN_PARALLEL, IS_R8_NAN 
      REAL*8,    EXTERNAL :: ISPL8, FSPL8, VAL_3D_BSPL, VAL_2D_BSPL
      LOGICAL*4, EXTERNAL :: IS_R4_NAN
      CHARACTER, EXTERNAL :: MJDSEC_TO_DATE*30
#ifdef GNU
      INTEGER*4, EXTERNAL     :: OMP_GET_MAX_THREADS, OMP_GET_NUM_THREADS, OMP_GET_THREAD_NUM
#else
      ADDRESS__TYPE, EXTERNAL :: OMP_GET_MAX_THREADS, OMP_GET_NUM_THREADS, OMP_GET_THREAD_NUM 
#endif
!
#ifndef SERIAL
      NTHR_SAVED = OMP_GET_NUM_THREADS()
      IF ( OMP_IN_PARALLEL() ) THEN
!
! -------- Do serial if we are already in the parallel region
!
           NTHR = 1
         ELSE
           CALL GETENVAR ( 'OMP_NUM_THREADS', STR )
           IF ( ILEN(STR) == 0 ) THEN
                NTHR = 1
              ELSE 
                CALL CHIN ( STR, NTHR )
           END IF
           CALL OMP_SET_NUM_THREADS ( %VAL(NTHR) )
           NTHR = OMP_GET_MAX_THREADS()
      END IF
#endif
!
! --- Set flag: interpolate attenuation
!
      FL_ATT_INTR = .TRUE.
!
! --- Parameters for tests, when test mode is turned on
!
      LON_TST  =   6*DEG__TO__RAD
      LAT_TST  = 45.0*DEG__TO__RAD
!
! --- Grid of the input numerical models
!
      L_LEV = HEB_DELP%DIMS(3)
      L_LON = HEB_DELP%DIMS(1)
      L_LAT = HEB_DELP%DIMS(2)
!
      K_LEV = SPD__MLEV
      K_LON = L_LON+2
      K_LAT = L_LAT
!
! --- Checks of OH file (orto-height)
!
      IF ( HEB_NAT_OH%SDS_NAME .NE. 'height above the WGS84 geoid' ) THEN
           CALL ERR_LOG ( 5411, IUER, 'GEN_TRG_DENS', 'Trap of internal '// &
     &         'control: the science dataset name of '//TRIM(HEB_NAT_OH%FILE_NAME)// &
     &         ' is '//TRIM(HEB_NAT_OH%SDS_NAME)//' while '// &
     &         'height above the WGS84 geoid was expected' )
           RETURN 
      END IF
!
      IF ( HEB_NAT_OH%DIMS(1) .NE. HEB_DELP%DIMS(1) .OR. &
     &     HEB_NAT_OH%DIMS(2) .NE. HEB_DELP%DIMS(2)      ) THEN
           WRITE ( 6, * ) 'HEB_NAT_OH%DIMS(1:2)   = ', HEB_NAT_OH%DIMS(1:2) 
           WRITE ( 6, * ) 'HEB_DELP%DIMS(1:2) = ', HEB_DELP%DIMS(1:2) 
           CALL ERR_LOG ( 5412, IUER, 'GEN_TRG_DENS', 'Error in configuration: '// &
     &         'Dimension of HEB-file with gridded surface orto-height is '// &
     &         'not the same as dimension of the pressure field' )
           RETURN 
      END IF
!
! --- Grid of the output Total pressure, partial pressure of water vapor and air temperature
! --- L_LEV, L_LON, L_LAT -- native input grid
! --- K_LEV, K_LON, K_LAT -- native output grid
! --- NLEV,  NLON,  NLAT  -- output spline
!
      IF ( DIMO(1) == 0 ) THEN
           NLEV = K_LEV
           NLON = K_LON
           NLAT = K_LAT
         ELSE
           NLEV = DIMO(1)
           NLON = DIMO(2)+2
           NLAT = DIMO(3)
      END IF
      IF ( MODE == 'd3' ) THEN
           HEB_TRG_3D = HEB_O3
           HEB_TRG_3D%DIMS(1) = NLEV + DEG
           HEB_TRG_3D%DIMS(2) = NLON + DEG
           HEB_TRG_3D%DIMS(3) = NLAT + DEG
           HEB_TRG_3D%DIMS(4) = M_TYP
           HEB_TRG_3D%SDS_NAME   = 'Trace gas density'
           HEB_TRG_3D%TITLE      = HEB_TRG_3D%SDS_NAME 
           HEB_TRG_3D%PROD_NAME  = HEB_TRG_3D%SDS_NAME 
           HEB_TRG_3D%INSTITUTION = 'Astrogeo Center'
           HEB_TRG_3D%VERSION_ID  = '1.0'
      END IF
!
! --- Alollocate TRG_NAT_VAL in the extended native dimension 
! --- (extended fo two extra knots at longitude)
!
      ALLOCATE ( TRG_NAT_VAL(1-DEG:K_LEV,1-DEG:K_LON,1-DEG:K_LAT,M_TYP), STAT=IER )
      MEL = INT8(K_LEV+DEG)*INT8(K_LON+DEG)*INT8(K_LAT+DEG)
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH  ( STR )
           CALL IINCH8 ( INT8(8)*MEL, STR )
           CALL ERR_LOG ( 5413, IUER, 'GEN_TRG_DENS', 'Failure in an attempt '// &
     &         'to allocate '//STR(1:I_LEN(STR))//' bytes of dynamic memory '// &
     &         'for array TRG_3D' )
           RETURN 
      END IF
      IF ( DIMO(1) > 0 ) THEN
!
! -------- Smoothing mode: allocate memory for temporary array TRG_SMO_VAL
!
           ALLOCATE ( TRG_SMO_VAL(1-DEG:NLEV,NLON,NLAT,M_TYP), STAT=IER )
           MEL = INT8(NLEV+DEG)*INT8(L_LON+2)*INT8(L_LAT)*INT8(M_TYP)
           IF ( IER .NE. 0 ) THEN
                CALL CLRCH  ( STR )
                CALL IINCH8 ( INT8(4)*MEL, STR )
                CALL ERR_LOG ( 5414, IUER, 'GEN_TRG_DENS', 'Failure in an attempt '// &
     &              'to allocate '//STR(1:I_LEN(STR))//' bytes of dynamic memory '// &
     &              'for array TRG_VAL' )
                RETURN 
           END IF
           ALLOCATE ( LEV_SMO(L_LEV)   )
      END IF
!
! --- Alolocate memory for the output arrays
!
      ALLOCATE ( HEB_TRG_3D%VAL(1-DEG:NLEV,1-DEG:NLON,1-DEG:NLAT,M_TYP), &
     &           STAT=IER )
      MEL = INT8(NLEV+DEG)*INT8(NLON+DEG)*INT8(NLAT+DEG)
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH  ( STR )
           CALL IINCH8 ( INT8(4)*MEL, STR )
           CALL ERR_LOG ( 5415, IUER, 'GEN_TRG_DENS', 'Failure in an attempt '// &
     &         'to allocate '//STR(1:I_LEN(STR))//' bytes of dynamic memory '// &
     &         'for array HEB_TRG_3D%VAL' )
           RETURN 
      END IF
      HEB_TRG_3D%VAL = 0.0D0
!
      IF ( MODE(1:2) == 'h_' ) THEN
           HEB_TRG_2D      = HEB_O3
           HEB_TRG_2D%DIMS = HEB_FINE_OH%DIMS
           HEB_TRG_2D%DIMS(3) = M_TYP
           HEB_TRG_2D%DIMS(4) = 2
           ALLOCATE ( HEB_TRG_2D%VAL(HEB_TRG_2D%DIMS(1),HEB_TRG_2D%DIMS(2),HEB_TRG_2D%DIMS(3),HEB_TRG_2D%DIMS(4)), STAT=IER )
           MEL = HEB_TRG_2D%DIMS(1)*HEB_TRG_2D%DIMS(2)*HEB_TRG_2D%DIMS(3)*HEB_TRG_2D%DIMS(4)
           IF ( IER .NE. 0 ) THEN
                CALL CLRCH  ( STR )
                CALL IINCH8 ( INT8(4)*MEL, STR )
                CALL ERR_LOG ( 5416, IUER, 'GEN_TRG_DENS', 'Failure in an attempt '// &
     &              'to allocate '//STR(1:I_LEN(STR))//' bytes of dynamic memory '// &
     &              'for array HEB_TRG_2D%VAL' )
               RETURN 
          END IF
          HEB_TRG_2D%VAL = 0.0D0
      END IF
!
      ALLOCATE ( PRES_LEV_SPL(1-DEG:NLON,1-DEG:NLAT), STAT=IER )
      MEL = INT8(NLON+DEG)*INT8(NLAT+DEG)
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH  ( STR )
           CALL IINCH8 ( INT8(8)*MEL, STR )
           CALL ERR_LOG ( 5417, IUER, 'GEN_TRG_DENS', 'Failure in an attempt '// &
     &         'to allocate '//STR(1:I_LEN(STR))//' bytes of dynamic memory '// &
     &         'for array PRES_LEV_SPL' )
           RETURN 
      END IF
      PRES_LEV_SPL = 0.0D0
!
      ALLOCATE ( LEV(1-DEG:K_LEV+DEG), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 5418, IUER, 'GEN_TRG_DENS', 'Failure in an attempt '// &
     &         'to allocate dynamic memory for array LEV_R8' )
           RETURN 
      END IF
!
      ALLOCATE ( RES_VAL(K_LEV,M_TYP), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 5419, IUER, 'GEN_TRG_DENS', 'Failure in an attempt '// &
     &         'to allocate dynamic memory for array RES_VAL' )
           RETURN 
      END IF
!
      ALLOCATE ( RES_SPL(1-DEG:NLEV-1,M_TYP), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 5420, IUER, 'GEN_TRG_DENS', 'Failure in an attempt '// &
     &         'to allocate dynamic memory for array RES_SPL' )
           RETURN 
      END IF
!
! --- Compute the dependence of log P on height for the Standard Atmosphere
!
      LPL = PRES_LN_0 + PRES_LN_RATE*HEI_MIN
      LPH = PRES_LN_0 + PRES_LN_RATE*HEI_MAX
      DO 410 J1=1,SPD__MLEV
         LOG_PRES = LPL + (J1-1)*(LPH - LPL)/(SPD__MLEV-1)
         HEI_R8(J1)  = -PRES_LN_0/PRES_LN_RATE + LOG_PRES/PRES_LN_RATE
         HEI_R4(J1)  = HEI_R8(J1)
 410  CONTINUE 
!
      IF( DIMO(1) > 0 ) THEN
          DO 420 J2=1,DIMO(1)
             LEV_SMO(J2) = DEXP (           DLOG(SPD__U_MIN - SPD__U_FLO) + &
     &                           (J2-1)* ( DLOG(SPD__U_MAX - SPD__U_FLO) - &
     &                                     DLOG(SPD__U_MIN - SPD__U_FLO) )/(DIMO(1)-1) &
     &                      ) + SPD__U_FLO
 420      CONTINUE 
      END IF
!
      ALLOCATE ( LAT(1-DEG:NLAT+DEG), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( 4*(NLAT+2*DEG), STR )
           CALL ERR_LOG ( 5421, IUER, 'GEN_TRG_DENS', 'Failure in '// &
     &         'an attempt to allocate '//STR(1:I_LEN(STR))// &
     &         ' bytes of dynamic memory for array LAT' )
           RETURN 
      END IF
!
      ALLOCATE ( LON(1-DEG:NLON+DEG), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( 4*(NLON+2*DEG), STR )
           CALL ERR_LOG ( 5422, IUER, 'GEN_TRG_DENS', 'Failure in '// &
     &         'an attempt to allocate '//STR(1:I_LEN(STR))// &
     &         ' bytes of dynamic memory for array LON' )
           RETURN 
      END IF
!
      IF ( IVRB .GE. 4 ) WRITE ( 6, * ) 'The number of threats: ', NTHR
      IF ( IVRB .GE. 4 ) CALL WALL_TIMER ( %VAL(0) )
      DO 430 J3=1,L_LAT
!
! ------ LAT_GDT -- is geodetic latitude
!
         LAT_GDT = -P2I + (J3-1)*PI__NUM/(L_LAT-1)
!
! ------ Compute gravity on the reference ellipsoid for a given geodetic latitude
!
         GE = SPD__ACC_EQU_WGS84*(1.D0 + SPD__GRV_LAT_WGS84*DSIN(LAT_GDT)**2)/ &
     &        DSQRT(1.D0 - (2.D0*SPD__FLAT_WGS84 - SPD__FLAT_WGS84**2)*DSIN(LAT_GDT)**2 )
         FL_ERROR = .FALSE.
!! 
#ifndef SERIAL
!$OMP    PARALLEL DO IF ( NTHR > 1 ), DEFAULT(NONE), &
!$OMP&   PRIVATE ( J4, J5, J6, J7, J8, J9, J10, IER, G, I_LON, LON_VAL, P, TMP_R8, HYPS, &
!$OMP&             H, PW, PD, TEM, ZM, Q, QV_LEV, SPL_TEM, SPL_TDEW, SPL_H, SPL_PW, SPL_P, &
!$OMP&             LOGP_1D, LOGP_M1D, LOGPW_1D, TEM_1D, TDEW_1D, RLEV, HEI_R4, HEI_R8, &
!$OMP&             T_RATE, T_NLEV, GMR_DRY, GMR_WET, I_LEV, N_LEV, IND, RH_VAL, TDEW_VAL, &
!$OMP&             ACP_VAL, RH1, AIR_DENS, O3_DENS, CO2_DENS, O3_SPL, CO2_SPL, LOG_O3_DENS, &
!$OMP&             LOG_CO2_DENS, LOG_CO2_DENS_LOWLEV, LOG_O3_DENS_LOWLEV, &
!$OMP&             LOG_O3_DENS_RATE, LOG_CO2_DENS_RATE, &
!$OMP&             ZM_VAL, P_VAL, PW_VAL, TEM_VAL, O3_VAL, CO2_VAL, &
!$OMP&             O3M_VAL, O3M_1D, O3M_SPL, CO2M_VAL, CO2M_1D, CO2M_SPL, RES_VAL, RES_SPL ), &
!$OMP&   SHARED   ( FL_ERROR, L_LON, L_LEV, J3, HEB_Q, HEB_DELP, HEB_T, HEB_NAT_OH, HEB_O3, HEB_CO2, &
!$OMP&              GE, LAT_GDT, IT_LON, IT_LAT, LON_TST, LAT_TST, FL_ATT_INTR, &
!$OMP&              IND_TEM, IND_PW, IND_P, DIMS, DIMO, K_LEV, LEV, LEV_SMO, &
!$OMP&              T1, IVRB, NTHR, NLEV, TRG_NAT_VAL, TRG_SMO_VAL, CNS_DR2, PRES_LEV_SPL, IUER ), &
!$OMP&   SCHEDULE ( STATIC, NTHR )
#endif
         DO 440 J4=1,L_LON
            IF ( FL_ERROR ) GOTO 440
!
! --------- NB: Merra/Geos longitude starts from -180deg   to +180 deg
! --------- J4    -- Merra/GEOS indexing from    -180 deg
! --------- I_LON -- SPD indexing from 0deg to   +360 deg
!
            I_LON = J4 + L_LON/2
            IF ( I_LON > L_LON ) I_LON = I_LON - L_LON
            LON_VAL = (I_LON - 1)/(L_LON/PI2)
!
! --------- First run is for computing height above the ellipsoid for a given pressure level
!
            P(L_LEV+1) = 1.0D0
            N_LEV = L_LEV
!
! --------- Correct specific humidity if we can
!
            QV_LEV(1:L_LEV) = HEB_Q%VAL(J4,J3,1:L_LEV,1)
            CALL ERR_PASS ( IUER, IER )
            CALL CORRECT_SPEC_HUM ( L_LEV, QV_LEV, IER )
            IF ( IER .NE. 0 ) THEN
#ifndef SERIAL
!$OMP           CRITICAL
#endif
                WRITE ( 6, * ) 'J4= ', J4, ' J3= ', J3
                CALL ERR_LOG ( 5423, IUER, 'GEN_TRG_DENS', 'Error '// &
     &              'in an attempt to correct specific humidity' )
                FL_ERROR = .TRUE.
#ifndef SERIAL
!$OMP           END CRITICAL
#endif
                GOTO 440
           END IF
!
! -------- Re-indexing pressure in such a way that the 1-st level of 
! -------- array P starts at the nominal surface
!
           DO 450 J5=1,L_LEV
              I_LEV = L_LEV+1-J5
              IF ( J5 == 1 ) THEN
                   P(I_LEV) = P(I_LEV+1) + HEB_DELP%VAL(J4,J3,J5,1)/2.0D0
                 ELSE 
                   P(I_LEV) = P(I_LEV+1) + HEB_DELP%VAL(J4,J3,J5-1,1)/2.0D0 + &
     &                                     HEB_DELP%VAL(J4,J3,J5,1)/2.0D0 
              END IF
              IF ( P(I_LEV) < PRES_HIGH ) N_LEV = I_LEV
!
! ----------- Compute approximate gravity acceleration at a given pressure level
! ----------- using a regression formulae for the US Standard Atmosphere
!
              G = GE*(1.D0 + SPD__GP0 + SPD__GP1*DLOG(P(I_LEV)))
!
! ----------- PW -- partial pressure of water vapour
!
              Q(I_LEV)   = QV_LEV(J5)
              PW(I_LEV)  = P(I_LEV)* Q(I_LEV)/ ( MW/MD + (1.0D0 - MW/MD)* Q(I_LEV) )
!
! ----------- PD -- Partial pressure of dry air
!
              PD(I_LEV)  = P(I_LEV) - PW(I_LEV)
              TEM(I_LEV) = HEB_T%VAL(J4,J3,J5,1)
              ZM(I_LEV)  = 1.D0/ &
     &           ( 1.0D0 &
     &             - P(I_LEV)/TEM(I_LEV)*         (SPD__COMP_A0        + SPD__COMP_A1*(TEM(I_LEV) - SPD__ABS_TEMP) +  &
     &                                                                   SPD__COMP_A2*(TEM(I_LEV) - SPD__ABS_TEMP)**2 ) &
     &             + PW(I_LEV)/TEM(I_LEV)*        (SPD__COMP_B0        + SPD__COMP_B1*(TEM(I_LEV) - SPD__ABS_TEMP)) &
     &             + PW(I_LEV)**2/(P(I_LEV)*TEM(I_LEV))* (SPD__COMP_C0 + SPD__COMP_C1*(TEM(I_LEV) - SPD__ABS_TEMP)) &
     &             + P(I_LEV)**2/TEM(I_LEV)**2*    SPD__COMP_D0 &
     &             + PW(I_LEV)**2/TEM(I_LEV)**2*   SPD__COMP_E0 &
     &           )
              AIR_DENS = (SPD__MD/SPD__R*P(I_LEV)/TEM(I_LEV) - (SPD__MD - SPD__MW)/SPD__R*PW(I_LEV)/TEM(I_LEV) )*ZM(I_LEV)
              O3_DENS(I_LEV)  = HEB_O3%VAL(J4,J3,J5,1)*AIR_DENS 
              CO2_DENS(I_LEV) = HEB_CO2%VAL(J4,J3,J5,1)*AIR_DENS 
              O3M_1D(I_LEV)   = HEB_O3%VAL(J4,J3,J5,1)
              CO2M_1D(I_LEV)  = HEB_CO2%VAL(J4,J3,J5,1)
!
! ----------- Compute function HYPS that is the right hand-side of 
! ----------- the hypsometric equation, while the left hand side is h(P)
!
              HYPS(I_LEV) = SPD__R_MAPL*TEM(I_LEV)/ &
     &                     (G*(MD*PD(I_LEV) + MW*PW(I_LEV))*ZM(I_LEV))
 450       CONTINUE 
!
! -------- Set ground values
!
           P(0) = P(1) + HEB_DELP%VAL(J4,J3,L_LEV,1)/2.0D0
           HYPS(0) = HYPS(1) - (P(1) - P(0))*(HYPS(2) - HYPS(1))/(P(2) - P(1))
!
! -------- Compute relative humidity at the surface
!
           TDEW_1D(1) = TEMP_K + B1_DEW*DLOG((PW(1)+EPS_PW)/C1_DEW)/ &
     &                       (A1_DEW - DLOG((PW(1)+EPS_PW)/C1_DEW))
           RH1 = DEXP ( LV__MALO/RV__MALO*(1.D0/TEM(1) - 1.D0/TDEW_1D(1)) )
!
! -------- Special trick for making interpolation: we reverse the sign of pressure.
! -------- Otherwise, MAKE_SPLINE will complain the argument is not in rising order
!
           P = -P ! For interpolation
           CALL ERR_PASS ( IUER, IER ) 
           CALL MAKE_SPLINE ( 3, L_LEV+1, P, HYPS, 0.0D0, 0.0D0, SPL_H, TMP_R8, IER )
           IF ( IER .NE. 0 ) THEN
#ifndef SERIAL
!$OMP           CRITICAL
#endif
                CALL ERR_LOG ( 5424, IUER, 'GEN_TRG_DENS', 'Failure in an attempt '// &
     &              'to compute coefficients of the interpolating spline' )
                FL_ERROR = .TRUE.
#ifndef SERIAL
!$OMP           END CRITICAL
#endif
                GOTO 440
           END IF
!
! -------- Compute the ellipsoidal height at pressure grid by integrating 
! -------- the spline that interpolates HYPS
!
! -------- Set boundary condition at the surface
!
           CALL ERR_PASS ( IUER, IER )
           H(0) = HEB_NAT_OH%VAL(J4,J3,1,1)
!@           IF ( IER .NE. 0 ) THEN
!@#ifndef SERIAL
!@!$OMP           CRITICAL
!@#endif
!@                CALL ERR_LOG ( 5425, IUER, 'GEN_TRG_DENS', 'Failure in an attempt '// &
!@     &              'to compute geoid undulation' )
!@                FL_ERROR = .TRUE.
!@#ifndef SERIAL
!@!$OMP           END CRITICAL
!@#endif
!@                GOTO 440
!@           END IF
!
           DO 460 J6=1,L_LEV
              CALL ERR_PASS ( IUER, IER )
              IF ( J6 < L_LEV ) THEN
                   H(J6) = H(J6-1) + ISPL8 ( P(J6), L_LEV+1, P(0), HYPS(0), &
     &                                       J6, J6+1, SPL_H, IER )
                 ELSE IF ( J6 == L_LEV ) THEN
                   H(J6) = H(J6-1) + ISPL8 ( P(J6)*(1.D0-EPS), L_LEV+1, P(0), HYPS(0), &
     &                                       J6, J6+1, SPL_H, IER )
              END IF
              IF ( IER .NE. 0 ) THEN
#ifndef SERIAL
!$OMP             CRITICAL
#endif
                  CALL ERR_LOG ( 5426, IUER, 'GEN_TRG_DENS', 'Failure in '// &
     &                'an attempt to integrate hypsometric equation' )
                  FL_ERROR = .TRUE.
#ifndef SERIAL
!$OMP             END CRITICAL
#endif
                  GOTO 440
              END IF
 460       CONTINUE 
!
! -------- Compute the lapse rate between N_LEV/2 and N_LEV
!
           CALL ERR_PASS ( IUER, IER )
           CALL REGR8 ( N_LEV - N_LEV/2 + 1, H(N_LEV/2), TEM(N_LEV/2), &
     &                  T_RATE, T_NLEV, IER )
           IF ( IER .NE. 0 ) THEN
#ifndef SERIAL
!$OMP           CRITICAL
#endif
                write ( 6, * ) 'k1= ', n_lev - n_lev/2 + 1, ' k2= ', n_lev/2, ' l_lev= ', l_lev
                write ( 6, * ) '  tem= ', tem(n_lev/2:n_lev)
                write ( 6, * ) 'h(0) = ', h(0) 
                write ( 6, * ) '    h= ', h(n_lev/2:n_lev)
                write ( 6, * ) '    p= ', p(0:l_lev)      
                write ( 6, * ) ' hyps= ', hyps(0:l_lev)   
                write ( 6, * ) 'spl_h= ', spl_h(1:l_lev+1)
                CALL ERR_LOG ( 5427, IUER, 'GEN_TRG_DENS', 'Failure in '// &
     &              'an attempt to compute lapse rate' )
                FL_ERROR = .TRUE.
#ifndef SERIAL
!$OMP           END CRITICAL
#endif
                GOTO 440
           END IF
!
! -------- Second round
!
           DO 470 J7=1,L_LEV
!
! ----------- We compute gravity at a given height
!
              G = GE*(1.D0 - 2.D0/SPD__REA_WGS84*(1.0D0 + SPD__OMEGA_EGM96**2*SPD__REA_WGS84**3* &
     &                            (1.D0 - SPD__FLAT_WGS84)/SPD__GM_EGM96 + &
     &                            SPD__FLAT_WGS84*(1.D0 - 2.D0* DSIN(LAT_GDT)**2) &
     &                           )*H(J7) &
     &                     + 3.D0/SPD__REA_WGS84**2*H(J7)**2)
!
! ------------ Compute improved function for integration
!
               HYPS(J7) = SPD__R_MAPL*TEM(J7)/(G*(MD*PD(J7) + MW*PW(J7))*ZM(J7))
  470       CONTINUE 
!
! -------- Compute interpolating spline of HYPS(P) the second time
!
           CALL ERR_PASS ( IUER, IER ) 
           CALL MAKE_SPLINE ( 3, L_LEV+1, P, HYPS, 0.0D0, 0.0D0, SPL_H, TMP_R8, IER )
           IF ( IER .NE. 0 ) THEN
#ifndef SERIAL
!$OMP           CRITICAL
#endif
                WRITE ( 6, * ) 'ier= ',ier 
                WRITE ( 6, * ) 'J4(lon) = ', INT2(J4), ' J3(lat) = ', INT2(J3)
                WRITE ( 6, * ) 'L_LEV   = ', L_LEV + 1
                WRITE ( 6, * ) 'P       = ', SNGL(P(1:L_LEV))
                WRITE ( 6, * ) 'hyps    = ', SNGL(HYPS(1:L_LEV))
                CALL ERR_LOG ( 5428, IUER, 'GEN_TRG_DENS', 'Failure in '// &
     &              'an attempt to compute coefficients of the '// &
     &              'interpolating spline' )
                FL_ERROR = .TRUE.
#ifndef SERIAL
!$OMP           END CRITICAL
#endif
                GOTO 440
           END IF
!
! -------- Compute height as a function of pressure the second time
!
           CALL ERR_PASS ( IUER, IER ) 
           H(0) = HEB_NAT_OH%VAL(J4,J3,1,1)
           DO 480 J8=1,L_LEV
              CALL ERR_PASS ( IUER, IER ) 
              IF ( J8 < L_LEV ) THEN
                   H(J8) = H(J8-1) + ISPL8 ( P(J8), L_LEV+1, P(0), HYPS(0), &
     &                                       J8, J8+1, SPL_H, IER )
                 ELSE IF ( J8 == L_LEV ) THEN
                   H(J8) = H(J8-1) + ISPL8 ( P(J8)*(1.D0-EPS), L_LEV+1, P(0), HYPS(0), &
     &                                       J8, J8+1, SPL_H, IER )
              END IF
              IF ( IER .NE. 0 ) THEN
#ifndef SERIAL
!$OMP             CRITICAL
#endif
                  CALL ERR_LOG ( 5429, IUER, 'GEN_TRG_DENS', 'Failure in '// &
     &                'an attempt to integrate hypsometric equation' )
                  FL_ERROR = .TRUE.
#ifndef SERIAL
!$OMP             END CRITICAL
#endif
                  GOTO 440
              END IF
              LOGP_1D(J8)  = DLOG(-P(J8))
              LOGPW_1D(J8) = DLOG(PW(J8))
              TEM_1D(J8)   = TEM(J8)
              LOG_CO2_DENS(J8) = DLOG( MAX(CO2_DENS(J8), 1.D-18 ) )
              LOG_O3_DENS(J8)  = DLOG( MAX(O3_DENS(J8),  1.D-18 ) )
              TDEW_1D(J8) = TEMP_K + B1_DEW*DLOG((PW(J8)+EPS_PW)/C1_DEW)/ &
     &                              (A1_DEW - DLOG((PW(J8)+EPS_PW)/C1_DEW))
 480       CONTINUE 
!
! -------- Compute the rate of change of log CO2 density at low atmosphere
! -------- for future extrapolation
!
           CALL ERR_PASS ( IUER, IER )
           CALL REGR8 ( N_LEV, H(1), LOG_CO2_DENS(1), &
     &                  LOG_CO2_DENS_RATE, LOG_CO2_DENS_LOWLEV, IER )
!
! -------- Compute the rate of change of log O3 density at low atmosphere
! -------- for future extrapolation
!
           CALL ERR_PASS ( IUER, IER )
           CALL REGR8 ( N_LEV, H(1), LOG_O3_DENS(1), &
     &                  LOG_O3_DENS_RATE, LOG_O3_DENS_LOWLEV, IER )
!
! -------- Now we compute interpolation spline of the total pressure logarighm as 
! -------- a function of height
!
           CALL ERR_PASS ( IUER, IER ) 
           CALL MAKE_SPLINE ( 3, L_LEV, H(1), LOGP_1D, 0.0D0, 0.0D0, &
     &                        SPL_P, TMP_R8, IER )
           IF ( IER .NE. 0 ) THEN
#ifndef SERIAL
!$OMP           CRITICAL
#endif
                WRITE ( 6, * ) 'ier= ',ier 
                WRITE ( 6, * ) 'J4(lon) = ', INT2(J4), ' J3(lat) = ', INT2(J3)
                WRITE ( 6, * ) 'L_LEV   = ', L_LEV + 1
                WRITE ( 6, * ) 'H       = ', SNGL(H(1:L_LEV))
                WRITE ( 6, * ) 'logp_1d = ', SNGL(LOGP_1D(1:L_LEV))
                CALL ERR_LOG ( 5430, IUER, 'GEN_TRG_DENS', 'Failure in an attempt '// &
     &              'to compute coefficients of the interpolating spline' )
                FL_ERROR = .TRUE.
#ifndef SERIAL
!$OMP           END CRITICAL
#endif
                GOTO 440
           END IF
!
! -------- Now we compute interpolation spline of height a function 
! -------- of the total pressure logarighm 
!
           LOGP_M1D = -LOGP_1D
           CALL ERR_PASS ( IUER, IER ) 
           CALL MAKE_SPLINE ( 3, L_LEV, LOGP_M1D, H(1), 0.0D0, 0.0D0, &
     &                        SPL_H, TMP_R8, IER )
           IF ( IER .NE. 0 ) THEN
#ifndef SERIAL
!$OMP           CRITICAL
#endif
                WRITE ( 6, * ) 'ier= ',ier 
                WRITE ( 6, * ) 'J4(lon) = ', INT2(J4), ' J3(lat) = ', INT2(J3)
                WRITE ( 6, * ) 'L_LEV   = ', L_LEV + 1
                WRITE ( 6, * ) 'LOGP_M1D = ', SNGL(LOGP_M1D(1:L_LEV))
                WRITE ( 6, * ) 'H       = ', SNGL(H(1:L_LEV))
                CALL ERR_LOG ( 5431, IUER, 'GEN_TRG_DENS', 'Failure in an attempt '// &
     &              'to compute coefficients of the interpolating spline' )
                FL_ERROR = .TRUE.
#ifndef SERIAL
!$OMP           END CRITICAL
#endif
                GOTO 440
           END IF
!
! -------- Now we compute the interpolation spline of water vapor partial 
! -------- pressure as a function of height
!
           CALL ERR_PASS ( IUER, IER ) 
           CALL MAKE_SPLINE ( 3, L_LEV, H(1), LOGPW_1D, 0.0D0, 0.0D0, &
     &                        SPL_PW, TMP_R8, IER )
           IF ( IER .NE. 0 ) THEN
#ifndef SERIAL
!$OMP           CRITICAL
#endif
                WRITE ( 6, * ) 'J4(lon) = ', INT2(J4), ' J3(lat) = ', INT2(J3)
                WRITE ( 6, * ) 'L_LEV   = ', L_LEV + 1
                WRITE ( 6, * ) 'H       = ', SNGL(H(1:L_LEV))
                WRITE ( 6, * ) 'PW_1D   = ', SNGL(LOGPW_1D(1:L_LEV))
                CALL ERR_LOG ( 5432, IUER, 'GEN_TRG_DENS', 'Failure in an attempt '// &
     &              'to compute coefficients of the interpolating spline' )
                FL_ERROR = .TRUE.
#ifndef SERIAL
!$OMP           END CRITICAL
#endif
                GOTO 440
           END IF
!
! --------- Now we compute interpolation spline of dew point temperature as 
! --------- a function of height
!
            CALL ERR_PASS ( IUER, IER ) 
            CALL MAKE_SPLINE ( 3, L_LEV, H(1), TDEW_1D, 0.0D0, 0.0D0, &
     &                         SPL_TDEW, TMP_R8, IER )
            IF ( IER .NE. 0 ) THEN
#ifndef SERIAL
!$OMP           CRITICAL
#endif
                 WRITE ( 6, * ) 'J4(lon) = ', INT2(J4), ' J3(lat) = ', INT2(J3)
                 WRITE ( 6, * ) 'L_LEV= ', L_LEV
                 WRITE ( 6, * ) 'H = ', SNGL(H(1:L_LEV))
                 CALL ERR_LOG ( 5433, IUER, 'MALO_COMP_ACP', 'Failure in an attempt '// &
     &                'to compute coefficients of the interpolating spline' )
#ifndef SERIAL
!$OMP           END CRITICAL
#endif
                GOTO 440
            END IF
!
! -------- Now we compute interpolation spline of air temperature as 
! -------- a function of height
!
           CALL ERR_PASS ( IUER, IER ) 
           CALL MAKE_SPLINE ( 3, L_LEV, H(1), TEM_1D, 0.0D0, 0.0D0, &
     &                        SPL_TEM, TMP_R8, IER )
           IF ( IER .NE. 0 ) THEN
#ifndef SERIAL
!$OMP           CRITICAL
#endif
                WRITE ( 6, * ) 'J4(lon) = ', INT2(J4), ' J3(lat) = ', INT2(J3)
                WRITE ( 6, * ) 'L_LEV   = ', L_LEV + 1
                WRITE ( 6, * ) 'H       = ', SNGL(H(1:L_LEV))
                WRITE ( 6, * ) 'TEM_1D  = ', SNGL(TEM_1D(1:L_LEV))
                CALL ERR_LOG ( 5434, IUER, 'GEN_TRG_DENS', 'Failure in an attempt '// &
     &              'to compute coefficients of the interpolating spline' )
                FL_ERROR = .TRUE.
#ifndef SERIAL
!$OMP           END CRITICAL
#endif
                GOTO 440
           END IF
!
! -------- Now we compute interpolation spline of ozone mixing ratio as a function of height
!
           CALL ERR_PASS ( IUER, IER ) 
           CALL MAKE_SPLINE ( 3, L_LEV, H(1), O3_DENS, 0.0D0, 0.0D0, &
     &                        O3_SPL, TMP_R8, IER )
           IF ( IER .NE. 0 ) THEN
#ifndef SERIAL
!$OMP           CRITICAL
#endif
                WRITE ( 6, * ) 'J4(lon) = ', INT2(J4), ' J3(lat) = ', INT2(J3)
                WRITE ( 6, * ) 'L_LEV   = ', L_LEV + 1
                WRITE ( 6, * ) 'H       = ', SNGL(H(1:L_LEV))
                WRITE ( 6, * ) 'TEM_1D  = ', SNGL(TEM_1D(1:L_LEV))
                CALL ERR_LOG ( 5435, IUER, 'GEN_TRG_DENS', 'Failure in an attempt '// &
     &              'to compute coefficients of the interpolating spline' )
                FL_ERROR = .TRUE.
#ifndef SERIAL
!$OMP           END CRITICAL
#endif
                GOTO 440
           END IF
!
! -------- Now we compute interpolation spline of carbon dixide mixing densoty as a function of height
!
           CALL ERR_PASS ( IUER, IER ) 
           CALL MAKE_SPLINE ( 3, L_LEV, H(1), CO2_DENS, 0.0D0, 0.0D0, &
     &                        CO2_SPL, TMP_R8, IER )
           IF ( IER .NE. 0 ) THEN
#ifndef SERIAL
!$OMP           CRITICAL
#endif
                WRITE ( 6, * ) 'J4(lon) = ', INT2(J4), ' J3(lat) = ', INT2(J3)
                WRITE ( 6, * ) 'L_LEV   = ', L_LEV + 1
                WRITE ( 6, * ) 'H       = ', SNGL(H(1:L_LEV))
                WRITE ( 6, * ) 'TEM_1D  = ', SNGL(TEM_1D(1:L_LEV))
                CALL ERR_LOG ( 5436, IUER, 'GEN_TRG_DENS', 'Failure in an attempt '// &
     &              'to compute coefficients of the interpolating spline' )
                FL_ERROR = .TRUE.
#ifndef SERIAL
!$OMP           END CRITICAL
#endif
                GOTO 440
           END IF
!
! -------- Now we compute interpolation spline of ozone mixing ratio
!
           CALL ERR_PASS ( IUER, IER ) 
           CALL MAKE_SPLINE ( 3, L_LEV, H(1), O3M_1D, 0.0D0, 0.0D0, &
     &                        O3M_SPL, TMP_R8, IER )
           IF ( IER .NE. 0 ) THEN
#ifndef SERIAL
!$OMP           CRITICAL
#endif
                WRITE ( 6, * ) 'J4(lon) = ', INT2(J4), ' J3(lat) = ', INT2(J3)
                WRITE ( 6, * ) 'L_LEV   = ', L_LEV + 1
                WRITE ( 6, * ) 'H       = ', SNGL(H(1:L_LEV))
                WRITE ( 6, * ) 'TEM_1D  = ', SNGL(TEM_1D(1:L_LEV))
                CALL ERR_LOG ( 5437, IUER, 'GEN_TRG_DENS', 'Failure in an attempt '// &
     &              'to compute coefficients of the interpolating spline' )
                FL_ERROR = .TRUE.
#ifndef SERIAL
!$OMP           END CRITICAL
#endif
                GOTO 440
           END IF
!
! -------- Now we compute interpolation spline of carbon dipxide mixing ratio
!
           CALL ERR_PASS ( IUER, IER ) 
           CALL MAKE_SPLINE ( 3, L_LEV, H(1), CO2M_1D, 0.0D0, 0.0D0, &
     &                        CO2M_SPL, TMP_R8, IER )
           IF ( IER .NE. 0 ) THEN
#ifndef SERIAL
!$OMP           CRITICAL
#endif
                WRITE ( 6, * ) 'J4(lon) = ', INT2(J4), ' J3(lat) = ', INT2(J3)
                WRITE ( 6, * ) 'L_LEV   = ', L_LEV + 1
                WRITE ( 6, * ) 'H       = ', SNGL(H(1:L_LEV))
                WRITE ( 6, * ) 'TEM_1D  = ', SNGL(TEM_1D(1:L_LEV))
                CALL ERR_LOG ( 5438, IUER, 'GEN_TRG_DENS', 'Failure in an attempt '// &
     &              'to compute coefficients of the interpolating spline' )
                FL_ERROR = .TRUE.
#ifndef SERIAL
!$OMP           END CRITICAL
#endif
                GOTO 440
           END IF
!
! -------- Now we compute total pressure, water vapour pressure and air temperature
! -------- on a global grid
!
           DO 490 J9=1,K_LEV
              IF ( DIMO(1) == 0 ) THEN
                   RLEV = J9-1 - (SPD__MLEV-1)/2
                   LEV(J9) = DEXP ( (RLEV - SPD__U3_GMAO72)/SPD__U1_GMAO72 ) - SPD__U2_GMAO72
                 ELSE IF ( J9 .LE. SPD__LBOT ) THEN
                   LEV(J9) = DEXP (  DLOG(SPD__U_MIN - SPD__U_FLO) + (J9-1)* &
     &                             ( DLOG(H(1)       - SPD__U_FLO) - &
     &                               DLOG(SPD__U_MIN - SPD__U_FLO) )/SPD__LBOT ) + &
     &                               SPD__U_FLO
                 ELSE IF ( J9 .EQ. SPD__LBOT + 1 ) THEN
                   LEV(J9) = H(1)
                 ELSE IF ( J9 .EQ. SPD__LBOT + L_LEV + 1 ) THEN
                   LEV(J9) = H(L_LEV)
                 ELSE IF ( J9 > SPD__LBOT + L_LEV + 1 ) THEN
                   LEV(J9) = DEXP (  DLOG(H(L_LEV) - SPD__U_FLO) + &
     &                               (J9 - SPD__LBOT - L_LEV)* &
     &                               ( DLOG(SPD__U_MAX - SPD__U_FLO) - &
     &                                 DLOG(H(L_LEV)   - SPD__U_FLO) )/SPD__LTOP ) + &
     &                                 SPD__U_FLO
                 ELSE
                   LEV(J9) = DEXP (  DLOG(H(1) - SPD__U_FLO) + &
     &                                   (J9 - SPD__LBOT)* &
     &                                   ( DLOG(H(L_LEV) - SPD__U_FLO) - &
     &                                     DLOG(H(1)     - SPD__U_FLO) &
     &                                   )/L_LEV &
     &                            ) + SPD__U_FLO
              END IF
              HEI_R4(J9) = LEV(J9) 
              HEI_R8(J9) = LEV(J9) 
              G = GE*(1.D0 - 2.D0/SPD__REA_WGS84*(1.0D0 + SPD__OMEGA_EGM96**2*SPD__REA_WGS84**3* &
     &                           (1.D0 - SPD__FLAT_WGS84)/SPD__GM_EGM96 + SPD__FLAT_WGS84* &
     &                           (1.D0 - DSIN(LAT_GDT)**2))*HEI_R8(J9) &
     &                     + 3.D0/SPD__REA_WGS84**2*HEI_R8(J9)**2)

              IF ( HEI_R8(J9) < H(1) ) THEN
!
! ---------------- This height level is below the surface. We assume the atmosphere
! ---------------- beneath the surface obeys to the adiabatic gas law with a constant 
! ---------------- lapse rate computed at the previous step
!
                   TEM_VAL =  TEM(1) + T_RATE*(HEI_R8(J9) - H(1))
                   PW_VAL    =  PW(1)*(TEM_VAL/TEM(1))**(-G*MW/(SPD__R_MAPL*T_RATE))
                   P_VAL    = -P(1) *(TEM_VAL/TEM(1))**(-G*MD/(SPD__R_MAPL*T_RATE))
                   O3_VAL   =  DEXP ( DLOG(O3_DENS(1) ) + LOG_O3_DENS_RATE* (HEI_R8(J9) - H(1)) )
                   CO2_VAL  =  DEXP ( DLOG(CO2_DENS(1)) + LOG_CO2_DENS_RATE*(HEI_R8(J9) - H(1)) )
                   O3M_VAL  =  O3M_1D(1)
                   CO2M_VAL =  CO2M_1D(1)
!
! ---------------- Fix relative humdity to the value at the 1st level
!
                   RH_VAL = RH1
                   IF ( RH_VAL > RH_MAX ) RH_VAL = RH_MAX
                   IF ( RH_VAL < RH_MIN ) RH_VAL = RH_MIN
                   ACP_VAL  =  ACP__MALO*TEM_VAL**2*LOG(1.0/RH_VAL)
                 ELSE IF ( HEI_R8(J9) > H(L_LEV) ) THEN
!
! ---------------- This is above the upper level. We consider the atmosphere isothermal.
!
                   GMR_DRY  = G*SPD__MA/SPD__R        
                   GMR_WET  = G*SPD__MW/SPD__R        
                   TEM_VAL  =  TEM(L_LEV)
                   PW_VAL   =  PW(L_LEV)*DEXP ( -GMR_WET*(HEI_R8(J9) - H(L_LEV))/TEM_VAL )
                   P_VAL    = -P(L_LEV)*DEXP  ( -GMR_DRY*(HEI_R8(J9) - H(L_LEV))/TEM_VAL )
                   O3_VAL   =  O3_DENS(L_LEV)
                   CO2_VAL  =  CO2_DENS(L_LEV)
                   O3M_VAL  =  O3M_1D(L_LEV)
                   CO2M_VAL =  CO2M_1D(L_LEV)
                   RH_VAL   = 0.0D0
                   TDEW_VAL = TEM_VAL
                   ACP_VAL  = 0.D0
                 ELSE
!
! ---------------- Compute air temperature, partial water vapour pressure, 
! ---------------- and the total pressure by spline interpolation
!
                   IND = IXMN8 ( L_LEV, H(1), HEI_R8(J9) )
                   TEM_VAL  =  FSPL8 ( HEI_R8(J9), L_LEV, H(1), TEM_1D,   IND, SPL_TEM )  
                   PW_VAL   =   DEXP ( FSPL8 ( HEI_R8(J9), L_LEV, H(1), LOGPW_1D, IND, SPL_PW  ) )
                   P_VAL    =   DEXP ( FSPL8 ( HEI_R8(J9), L_LEV, H(1), LOGP_1D,  IND, SPL_P   ) )
                   O3_VAL   =  FSPL8 ( HEI_R8(J9), L_LEV, H(1), O3_DENS,  IND, O3_SPL )  
                   CO2_VAL  =  FSPL8 ( HEI_R8(J9), L_LEV, H(1), CO2_DENS, IND, CO2_SPL )  
                   O3M_VAL  =  FSPL8 ( HEI_R8(J9), L_LEV, H(1), O3M_1D,  IND, O3M_SPL )  
                   CO2M_VAL =  FSPL8 ( HEI_R8(J9), L_LEV, H(1), CO2M_1D, IND, CO2M_SPL )  
                   TDEW_VAL =  FSPL8 ( HEI_R8(J9), L_LEV, H(1), TDEW_1D, IND,  SPL_TDEW )  
                   RH_VAL   =  DEXP ( LV__MALO/RV__MALO*(1.D0/TEM_VAL - 1.D0/TDEW_VAL) )
                   ACP_VAL  =  ACP__MALO*TEM_VAL**2*LOG(1.0/RH_VAL)
               END IF
               ZM_VAL = 1.D0/ &
     &                    ( 1.0D0 &
     &                      - P_VAL/TEM_VAL*             (SPD__COMP_A0 + SPD__COMP_A1*(TEM_VAL+SPD__ABS_TEMP) +  &
     &                                                                   SPD__COMP_A2*(TEM_VAL+SPD__ABS_TEMP)**2 ) &
     &                      + PW_VAL/TEM_VAL*            (SPD__COMP_B0 + SPD__COMP_B1*(TEM_VAL+SPD__ABS_TEMP)) &
     &                      + PW_VAL**2/(P_VAL*TEM_VAL)* (SPD__COMP_C0 + SPD__COMP_C1*(TEM_VAL+SPD__ABS_TEMP)) &
     &                      + P_VAL**2/TEM_VAL**2*        SPD__COMP_D0 &
     &                      + PW_VAL**2/TEM_VAL**2*       SPD__COMP_E0 &
     &                    )
               AIR_DENS = (SPD__MD/SPD__R*P_VAL/TEM_VAL - (SPD__MD - SPD__MW)/SPD__R*PW_VAL/TEM_VAL )*ZM_VAL
               TRG_NAT_VAL(J9,I_LON,J3,M__O3)   = O3_VAL*AIR_DENS
               TRG_NAT_VAL(J9,I_LON,J3,M__CO2)  = CO2_VAL*AIR_DENS 
               TRG_NAT_VAL(J9,I_LON,J3,M__ACP)  = ACP_VAL
               TRG_NAT_VAL(J9,I_LON,J3,M__RH)   = RH_VAL
               TRG_NAT_VAL(J9,I_LON,J3,M__DEW)  = TDEW_VAL
               TRG_NAT_VAL(J9,I_LON,J3,M__TEM)  = TEM_VAL
               TRG_NAT_VAL(J9,I_LON,J3,M__O3M)  = O3M_VAL
               TRG_NAT_VAL(J9,I_LON,J3,M__CO2M) = CO2M_VAL
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!   if ( trg_nat_val(j9,i_lon,j3,m__o3) < 0.0 ) then
!        write ( 6, * ) 'SMALL o3  ', int2(j9), int2(i_lon), int2(j3), ' val= ', trg_nat_val(j9,i_lon,j3,m__o3)  ! %%
!   end if
!   if ( trg_nat_val(j9,i_lon,j3,m__co2) < 0.0 ) then
!        write ( 6, * ) 'SMALL co2 ', int2(j9), int2(i_lon), int2(j3), ' val= ', trg_nat_val(j9,i_lon,j3,m__co2), ' AIR_DENS= ', AIR_DENS, ' co2= ', co2_val  ! %%
!   end if
!   if ( trg_nat_val(j9,i_lon,j3,m__o3) > 0.0001 ) then
!        write ( 6, * ) 'BIG   o3  ', int2(j9), int2(i_lon), int2(j3), ' val= ', trg_nat_val(j9,i_lon,j3,m__o3)  ! %%
!   end if
!   if ( trg_nat_val(j9,i_lon,j3,m__co2) > 0.0001 ) then
!        write ( 6, * ) 'BIG  co2 ', int2(j9), int2(i_lon), int2(j3), ' val= ', trg_nat_val(j9,i_lon,j3,m__co2), ' AIR_DENS= ', AIR_DENS, ' co2= ', co2_val  ! %%
!   end if
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  490      CONTINUE 
!
           IF ( DIMO(1) > 0 ) THEN
!
! ------------- Compute smoothing spline over height
!
                RES_VAL(1:K_LEV,1:M_TYP) = TRG_NAT_VAL(1:K_LEV,I_LON,J3,1:M_TYP)
                CALL ERR_PASS ( IUER, IER )
                CALL EBSPL_LSQ_CNS3_VEC ( K_LEV, M_TYP, HEI_R8(1), RES_VAL, &
     &                     NLEV, DEG, LEV_SMO(1), RES_SPL, &
     &                     0.0D0, 0.0D0, CNS_DR2(1), IER )
                IF ( IER .NE. 0 ) THEN
!$OMP                CRITICAL
                     CALL ERR_LOG ( 5439, IUER, 'GEN_TRG_DENS', 'Failure in '// &
     &                   'an attempt to compute smoothing B-spline over '// &
     &                   'height dimension' )
                     FL_ERROR = .TRUE.
!$OMP                END CRITICAL
                END IF
                TRG_SMO_VAL(1-DEG:NLEV-1,I_LON,J3,1:M_TYP) = RES_SPL(1-DEG:NLEV-1,1:M_TYP)
           END IF
!
           IND = IXMN8 ( L_LEV, LOGP_M1D(1), -LOG_PRES_LEV )
           PRES_LEV_SPL(I_LON,J3) = FSPL8 ( -LOG_PRES_LEV, L_LEV, LOGP_M1D(1), H(1), IND, SPL_H )  
  440    CONTINUE 
#ifndef SERIAL
!$OMP END PARALLEL DO
#endif
         IF ( FL_ERROR ) RETURN
!
! ------ We extend longitude coverage to one pixel eastward.
! ------ The pixel with longitude index L_LON+1,L_LON+2 is equal 
! ------ to pixels with indexes 1 and 2.
!
         IF ( DIMO(1) == 0 ) THEN
              TRG_NAT_VAL(1-DEG:K_LEV,L_LON+1,J3,1:M_TYP) = TRG_NAT_VAL(1-DEG:K_LEV,1,J3,1:M_TYP) 
              TRG_NAT_VAL(1-DEG:K_LEV,L_LON+2,J3,1:M_TYP) = TRG_NAT_VAL(1-DEG:K_LEV,2,J3,1:M_TYP) 
            ELSE
              TRG_SMO_VAL(1-DEG:NLEV,L_LON+1,J3,1:M_TYP)  = TRG_SMO_VAL(1-DEG:NLEV,1,J3,1:M_TYP) 
              TRG_SMO_VAL(1-DEG:NLEV,L_LON+2,J3,1:M_TYP)  = TRG_SMO_VAL(1-DEG:NLEV,2,J3,1:M_TYP) 
         END IF
         IF ( IVRB .GE. 4 ) THEN
              WRITE ( 6, 220 ) J3, L_LAT, CHAR(13)
 220          FORMAT ( '  1st Ilat= ', I4, ' ( ', I4, ' ) ',A$ )
         END IF
 430  CONTINUE 
      IF ( IVRB .GE. 4 ) WRITE ( 6, '(A)' ) ' ' 
      IF ( IVRB .GE. 2 ) THEN
           WRITE ( 6, * ) 'Finished processing the  1st dimension'
      END IF
      IF ( IVRB .GE. 4 ) THEN
           CALL WALL_TIMER ( STR )
           WRITE ( 6, * ) 'Computation of values '//STR(1:I_LEN(STR))
      END IF
!
! --- Build arrays of longitude/latitude
!
      DO 4110 J11=1,NLON
         LON(J11) = (J11-1)*PI2/(NLON-2)
 4110 CONTINUE 
!
      DO 4120 J12=1,NLAT
         LAT(J12) = -P2I + (J12-1)*PI__NUM/(NLAT-1)
 4120 CONTINUE 
!
      CALL ERR_PASS ( IUER, IER )
      CALL BSPL_2D_CMP ( DEG, 0, NLON, NLAT, LON(1), LAT(1), PRES_LEV_SPL, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 5440, IUER, 'GEN_TRG_DENS', 'Failure in '// &
     &         'an attempt to compute coefficients of the 2D '// &
     &         'interpolating for the height of the specified pressure level' )
           RETURN 
      END IF
      IF ( DIMO(1) == 0 ) THEN
!
! -------- Compute coefficients of 3D interpolating B-splines
!          ==================================================
!
           IF ( IVRB .GE. 4 ) CALL WALL_TIMER ( %VAL(0) )
           DIMS(1) = NLEV
           DIMS(2) = NLON
           DIMS(3) = NLAT
           DO 4100 J10=1,K_LEV
              LEV(J10) = DEXP (            DLOG(SPD__U_MIN - SPD__U_FLO) + &
     &                          (J10-1)* ( DLOG(SPD__U_MAX - SPD__U_FLO) - &
     &                                     DLOG(SPD__U_MIN - SPD__U_FLO) )/(K_LEV-1) &
     &                        ) + SPD__U_FLO
 4100      CONTINUE 
           FL_ERROR = .FALSE.
#ifndef    SERIAL
!$OMP      PARALLEL DO & ! IF ( NTHR > 1 ), &
!$OMP&     PRIVATE ( J13, IER  )
#endif
           DO 4130 J13=1,M_TYP
              IF ( FL_ERROR ) GOTO 4130
              CALL ERR_PASS ( IUER, IER )
              CALL BSPL_3D_CMP ( DEG, 0, DIMS, &
     &                           LEV(1), LON(1), LAT(1), &
     &                           TRG_NAT_VAL(1-DEG:NLEV,1-DEG:NLON,1-DEG:NLAT,J13), IER )
              IF ( IER .NE. 0 ) THEN
!$OMP              CRITICAL
!$OMP              FLUSH
                   CALL ERR_LOG ( 5441, IUER, 'GEN_TRG_DENS', 'Failure in '// &
     &                 'an attempt to compute coefficients of the 3D '// &
     &                 'interpolating spline for atmospheric attenuation' )
                   FL_ERROR = .TRUE.
!$OMP              END CRITICAL
              END IF
 4130      CONTINUE 
#ifndef    SERIAL
!$OMP      END PARALLEL DO
#endif
           IF ( FL_ERROR ) THEN
                RETURN 
           END IF
           IF ( MODE == 'd3' ) THEN
                HEB_TRG_3D%VAL = TRG_NAT_VAL
                HEB_TRG_3D%DATA_FORMAT    = HEB__R4
                HEB_TRG_3D%DATA_TRANSFORM = HEB__SCOF
                HEB_TRG_3D%OFFSET         = 0.0
                HEB_TRG_3D%SCALE_FACTOR   = 1.0
                HEB_TRG_3D%VALID_RANGE(1) = 0.0
                HEB_TRG_3D%VALID_RANGE(2) = 0.01
                HEB_TRG_3D%MIN_VALUE      = MAX ( MINVAL(HEB_TRG_3D%VAL), HEB_TRG_3D%VALID_RANGE(1) )
                HEB_TRG_3D%MAX_VALUE      = MIN ( MAXVAL(HEB_TRG_3D%VAL), HEB_TRG_3D%VALID_RANGE(2) )
           END IF
           IF ( IVRB .GE. 4 ) THEN
                CALL WALL_TIMER ( STR )
                WRITE ( 6, * ) 'Bspline expansion took '//STR(1:I_LEN(STR))
           END IF
         ELSE
!
! -------- Compute coefficients of the 3D smoothing spline
!          ===============================================
!
! -------- Allocate temporary arrays
!
!
           DEALLOCATE ( RES_VAL )
           DEALLOCATE ( RES_SPL )
           ALLOCATE   ( RES_VAL(L_LON+2,M_TYP), STAT=IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 5442, IUER, 'GEN_TRG_DENS', 'Failure in an attempt '// &
     &              'to allocate dynamic memory for array RES_VAL' )
                RETURN 
           END IF
!
           ALLOCATE ( RES_SPL(1-DEG:NLON-1,M_TYP), STAT=IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 5443, IUER, 'GEN_TRG_DENS', 'Failure in an attempt '// &
     &              'to allocate dynamic memory for array RES_SPL' )
                RETURN 
           END IF
!
           DO 4140 J14=1,L_LON+2
              LON(J14) = (J14-1)*PI2/L_LON
 4140      CONTINUE 
           FL_ERROR = .FALSE.
!
! ----- Expansion over longitude
!
#ifndef SERIAL
!$OMP      PARALLEL DO IF ( NTHR > 1 ), DEFAULT(NONE), &
!$OMP&     PRIVATE ( J15, J16, J17, RES_VAL, RES_SPL, CNS_DR2_LON, IER ), &
!$OMP&     SHARED  ( FL_ERROR, L_LON, L_LAT, NTHR, &
!$OMP&               TRG_SMO_VAL, LON, CNS_DR2, LAT_GDT, IVRB, NLEV, NLON, NLAT, HEB_TRG_3D, IUER ), &
!$OMP&     SCHEDULE ( STATIC, 1 )
#endif
           DO 4150 J15=1,L_LAT
!
! ----------- LAT_GDT -- is geodetic latitude
!
              LAT_GDT = -P2I + (J15-1)*PI__NUM/(L_LAT-1)
              IF ( J15 == 1 .OR. J15 == L_LAT ) THEN
!
! ---------------- Special case near poles to avoid a situation that COS(LAT) is a very
! ---------------- small number greatr than zero
!
                   CNS_DR2_LON = CNS_DR2(2)*(PI2*SPD__REA_WGS84*DCOS(-P2I + 1*PI__NUM/(L_LAT-1))/(NLON-2))**2
                 ELSE
                   CNS_DR2_LON = CNS_DR2(2)*(PI2*SPD__REA_WGS84*DCOS(LAT_GDT)/(NLON-2))**2
              END IF
!
              IF ( FL_ERROR ) GOTO 4150
              DO 4160 J16=1-DEG,NLEV-1
                 IF ( FL_ERROR ) GOTO 4150
                 DO 4170 J17=1,L_LON+2
                    RES_VAL(J17,1:M_TYP) = TRG_SMO_VAL(J16,J17,J15,1:M_TYP)
 4170            CONTINUE 
                 CALL ERR_PASS ( IUER, IER ) 
                 CALL EBSPL_LSQ_CNS3_VEC ( L_LON+2, M_TYP, LON(1), RES_VAL, &
     &                      NLON, DEG, LON(1), RES_SPL, &
     &                      0.0D0, 0.0D0, CNS_DR2_LON, IER )
                 IF ( IER .NE. 0 ) THEN
#ifndef               SERIAL
!$OMP                 CRITICAL
#endif
                      FL_ERROR = .TRUE.
#ifndef SERIAL
!$OMP                 END CRITICAL
#endif
                 END IF
                 TRG_SMO_VAL(J16,1-DEG:NLON-1,J15,1:M_TYP) = RES_SPL(1-DEG:NLON-1,1:M_TYP)
 4160         CONTINUE 
              IF ( IVRB .GE. 4 ) THEN
                   WRITE ( 6, 230 ) J15, L_LAT, CHAR(13)
 230               FORMAT ( '  2nd: Ilat= ', I4, ' ( ', I4, ' ) ',A$ )
              END IF
 4150      CONTINUE 
#ifndef SERIAL
!$OMP END PARALLEL DO
#endif
           IF ( FL_ERROR ) THEN
                CALL ERR_LOG ( 5444, IUER, 'GEN_TRG_DENS', 'Error in an attempt '// &
      &                        'compute coefficients of the smoothing B-spline '// &
     &                         'over longitude' )
               RETURN
           END IF
!
           IF ( IVRB .GE. 2 ) THEN
                WRITE ( 6, * ) 'Finished processing the  2nd dimension'
           END IF
!
           DEALLOCATE ( RES_VAL)
           DEALLOCATE ( RES_SPL )
!
           ALLOCATE   ( RES_VAL(L_LAT,M_TYP), STAT=IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 5445, IUER, 'GEN_TRG_DENS', 'Failure in an attempt '// &
     &              'to allocate dynamic memory for array RES_VAL' )
                RETURN 
           END IF
!
           ALLOCATE ( RES_SPL(1-DEG:NLAT-1,M_TYP), STAT=IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 5446, IUER, 'GEN_TRG_DENS', 'Failure in an attempt '// &
     &              'to allocate dynamic memory for array RES_SPL' )
                RETURN 
           END IF
!
           DO 4180 J18=1,L_LAT
              LAT(J18) = -P2I + (J18-1)*PI__NUM/(L_LAT-1)
 4180      CONTINUE 
           FL_ERROR = .FALSE.
!
! ----- Expansion over latitude
!
#ifndef SERIAL
!$OMP      PARALLEL DO IF ( NTHR > 1 ), DEFAULT(NONE), &
!$OMP&     PRIVATE ( J19, J20, J21, RES_VAL, RES_SPL, HEB_TRG_3D, IER ), &
!$OMP&     SHARED  ( FL_ERROR, L_LON, L_LAT, NTHR, &
!$OMP&               LAT, CNS_DR2, IVRB, NLEV, NLON, NLAT, TRG_SMO_VAL, IUER ), &
!$OMP&     SCHEDULE ( STATIC, 1 )
#endif
           DO 4190 J19=1-DEG,NLEV-1
              IF ( FL_ERROR ) GOTO 4190
              DO 4200 J20=1-DEG,NLON-1
                 IF ( FL_ERROR ) GOTO 4190
                 DO 4210 J21=1,L_LAT
                    RES_VAL(J21,1:M_TYP) = TRG_SMO_VAL(J19,J20,J21,1:M_TYP)
 4210            CONTINUE 
                 CALL ERR_PASS ( IUER, IER )
                 CALL EBSPL_LSQ_CNS3_VEC ( L_LAT, M_TYP, LAT(1), RES_VAL, &
     &                      NLAT, DEG, LAT(1), RES_SPL, &
     &                      0.0D0, 0.0D0, CNS_DR2(2)*(PI__NUM*SPD__REA_WGS84/(NLAT-1))**2, IER )
                 IF ( IER .NE. 0 ) THEN
#ifndef               SERIAL
!$OMP                 CRITICAL
#endif
                      FL_ERROR = .TRUE.
#ifndef SERIAL
!$OMP                 END CRITICAL
#endif
                 END IF
                 TRG_SMO_VAL(J19,J20,1-DEG:NLAT-1,1:M_TYP) = RES_SPL(1-DEG:NLAT-1,1:M_TYP)
 4200         CONTINUE 
              IF ( IVRB .GE. 4 ) THEN
                   WRITE ( 6, 240 ) J19, NLEV-1, CHAR(13)
 240               FORMAT ( '  3rd: Ilev= ', I4, ' ( ', I4, ' ) ',A$ )
              END IF
 4190      CONTINUE 
#ifndef SERIAL
!$OMP END PARALLEL DO
#endif
           IF ( FL_ERROR ) THEN
                CALL ERR_LOG ( 5449, IUER, 'GEN_TRG_DENS', 'Error in an attempt '// &
     &                        'compute coefficients of the smoothing B-spline '// &
     &                        'over longitude' )
               RETURN
           END IF
           IF ( IVRB .GE. 2 ) THEN
                WRITE ( 6, * ) 'Finished processing the  3rd dimension'
           END IF
!
           HEB_TRG_3D%VAL = TRG_SMO_VAL
      END IF
      IF ( MODE(1:2) == 'h_' ) THEN
           IF ( IVRB .GE. 4 ) CALL WALL_TIMER ( %VAL(0) )
           DIMS(1) = NLEV
           DIMS(2) = NLON
           DIMS(3) = NLAT
           DO 4220 J22=1,HEB_TRG_2D%DIMS(3)
#ifndef SERIAL
!$OMP      PARALLEL DO IF ( NTHR > 1 ), DEFAULT(NONE), &
!$OMP&     PRIVATE ( J23, J24, ARGS, INDS, IND_THR, STR ), &
!$OMP&     SHARED  ( J22, HEB_TRG_2D, TRG_NAT_VAL, HEB_FINE_OH, DIMO, DIMS, NLEV, &
!$OMP&               NLON, NLAT, LEV, LON, LAT, REF_HEI, NTHR, IVRB, PRES_LEV_SPL ), &
!$OMP&     SCHEDULE ( STATIC, NTHR )
#endif
              DO 4230 J23=1,HEB_TRG_2D%DIMS(2) ! over latitude
                 IND_THR = OMP_GET_THREAD_NUM()
                 IF ( J23 == 1 ) THEN
                      ARGS(3) = -P2I + (J23-1)*PI__NUM/(HEB_TRG_2D%DIMS(2)-1) + EPS
                      INDS(3) = IXMN8 ( NLAT, LAT(1), ARGS(3) )
                    ELSE IF ( J23 == HEB_TRG_2D%DIMS(2) ) THEN
                      ARGS(3) = -P2I + (J23-1)*PI__NUM/(HEB_TRG_2D%DIMS(2)-1) - EPS
                      INDS(3) = IXMN8 ( NLAT, LAT(1), ARGS(3) )
                    ELSE
                      ARGS(3) = -P2I + (J23-1)*PI__NUM/(HEB_TRG_2D%DIMS(2)-1)
                      INDS(3) = IXMN8_S ( INDS(3), NLAT, LAT(1), ARGS(3) )
                 END IF
                 DO 4240 J24=1,HEB_TRG_2D%DIMS(1) ! over longigude
                    IF ( J24 == 1 ) THEN
                         ARGS(2) = 0.0D0 + (J24-1)*PI2/HEB_TRG_2D%DIMS(1) + EPS
                         INDS(2) = IXMN8 ( NLON, LON(1), ARGS(2) )
                       ELSE 
                         ARGS(2) = 0.0D0 + (J24-1)*PI2/HEB_TRG_2D%DIMS(1)
                         INDS(2) = IXMN8_S ( INDS(2), NLON, LON(1), ARGS(2) )
                    END IF                 
!
                    ARGS(1) = REF_HEI + HEB_FINE_OH%VAL(J24,J23,1,1)
                    INDS(1) = IXMN8 ( NLEV, LEV(1), ARGS(1) )
                    IF ( DIMO(1) == 0 ) THEN
                         HEB_TRG_2D%VAL(J24,J23,J22,1) = VAL_3D_BSPL ( ARGS, DEG, DIMS, INDS, &
     &                             LEV(1), LON(1), LAT(1), &
     &                             TRG_NAT_VAL(1-DEG:NLEV,1-DEG:NLON,1-DEG:NLAT,J22) )
                    END IF
!
                    ARGS(1) = VAL_2D_BSPL ( ARGS(2), ARGS(3), NLON, NLAT, DEG, &
     &                                      INDS(2), INDS(3), LON(1), LAT(1), PRES_LEV_SPL )
                    INDS(1) = IXMN8 ( NLEV, LEV(1), ARGS(1) )
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!#ifndef serial
!!$omp   critical
!#endif
!                    write ( 6, * ) 'lon/lat = ', sngl(args(2)/deg__to__rad), sngl(args(3)/deg__to__rad), ' hei = ', sngl(args(1)), ' inds= ', inds(1:3), ' pls= ', pres_lev_spl(inds(2),inds(3)) ! %%%%%%%%%
!#ifndef serial
!!$omp   end critical
!#endif
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
                    IF ( DIMO(1) == 0 ) THEN
                         HEB_TRG_2D%VAL(J24,J23,J22,2) = VAL_3D_BSPL ( ARGS, DEG, DIMS, INDS, &
     &                             LEV(1), LON(1), LAT(1), &
     &                             TRG_NAT_VAL(1-DEG:NLEV,1-DEG:NLON,1-DEG:NLAT,J22) )
                    END IF
 4240            CONTINUE 
 4230         CONTINUE 
#ifndef SERIAL
!$OMP END PARALLEL DO
#endif
 4220      CONTINUE 
           IF ( IVRB .GE. 4 ) THEN
                CALL WALL_TIMER ( STR )
                WRITE ( 6, * ) 'Computation of 2D function took '//STR(1:I_LEN(STR))
           END IF
!
           HEB_TRG_2D%SDS_NAME    = 'Trace gas density'
           HEB_TRG_2D%TITLE       = HEB_TRG_2D%SDS_NAME 
           HEB_TRG_2D%PROD_NAME   = HEB_TRG_2D%SDS_NAME 
           HEB_TRG_2D%INSTITUTION = 'Astrogeo Center'
           HEB_TRG_2D%VERSION_ID  = '1.0'
           HEB_TRG_2D%UNITS          = 'kg/m^3'
           HEB_TRG_2D%DATA_FORMAT    = HEB__R4
           HEB_TRG_2D%DATA_TRANSFORM = HEB__SCOF
           HEB_TRG_2D%OFFSET         = 0.0
           HEB_TRG_2D%SCALE_FACTOR   = 1.0
           HEB_TRG_2D%VALID_RANGE(1) = 0.0
           HEB_TRG_2D%VALID_RANGE(2) = 0.01
           HEB_TRG_2D%MIN_VALUE      = MAX ( MINVAL(HEB_TRG_2D%VAL), HEB_TRG_2D%VALID_RANGE(1) )
           HEB_TRG_2D%MAX_VALUE      = MIN ( MAXVAL(HEB_TRG_2D%VAL), HEB_TRG_2D%VALID_RANGE(2) )
      END IF
!
      DEALLOCATE ( LON  )
      DEALLOCATE ( LAT  )
      DEALLOCATE ( LEV  )
      DEALLOCATE ( RES_VAL )
      DEALLOCATE ( RES_SPL )
      DEALLOCATE ( TRG_NAT_VAL )
      IF ( ALLOCATED ( LEV_SMO     ) ) DEALLOCATE ( LEV_SMO     )
      IF ( ALLOCATED ( TRG_SMO_VAL ) ) DEALLOCATE ( TRG_SMO_VAL )
#ifndef SERIAL
#ifdef GNU
      CALL OMP_SET_NUM_THREADS ( %VAL(NTHR_SAVED) )
#else
      CALL OMP_SET_NUM_THREADS (      NTHR_SAVED  )
#endif
#endif
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  GEN_TRG_DENS  !#!#
