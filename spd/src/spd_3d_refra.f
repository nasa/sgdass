      SUBROUTINE SPD_3D_REFRA ( SPD, HEB_DELP, HEB_T, HEB_Q, HEB_OH, &
     &                          HEB_GEOID_BSPL, DIMO, CNS_DR2, IVRB, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  SPD_3D_REFRA  computes the coefficients of expansion of   *
! *   the refractivity of moist air in the 4D B-spline basis.            *
! *                                                                      *
! *   SPD_3D_REFRA honors environment variable OMP_NUM_THREADS and       *
! *   it runs in parallel mode if OMP_NUM_THREADS > 1 and SPD_3D_REFRA   *
! *   is not called from the parallel region. However, the speed-up      *
! *   factor usually is less than the number of threads, since           *
! *   SPD_3D_REFRA thrashes memory extensively and has a high cache      *
! *   miss rate.                                                         *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! * HEB_DELP ( HEB__TYPE ) -- Array of HEB data structures that keep     *
! *                           the field of pressure thickness.           *
! *                           Dimension: NTIM. One element corresponds   *
! *                           to one time epoch.                         *
! * HEB_T    ( HEB__TYPE ) -- Array of HEB data structures that keep     *
! *                           the field of air temperature.              *
! *                           Dimension: NTIM. One element corresponds   *
! *                           to one time epoch.                         *
! * HEB_Q    ( HEB__TYPE ) -- Array of HEB data structures that keep     *
! *                           the field of speficic himdity.             *
! *                           Dimension: NTIM. One element corresponds   *
! *                           to one time epoch.                         *
! * HEB_OH   ( HEB__TYPE ) -- HEB data strcuture that keeps the global   *
! *                           array heights above the geoid of the       *
! *                           nominal surface.                           *
! * HEB_GEOID_BSPL ( HEB__TYPE ) -- HEB data strcuture that keeps the    *
! *                           array of coefficients of the geoid heights *
! *                           expansion into the 2D B-spline basis.      *
! * DIMO     ( INTEGER*4 ) -- Dimensions of the output grid: array of 3  *
! *                           elements. If DIMO=(0,0,0), then            *
! *                           interpolation spline is computed.          *
! * CNS_DR2  ( REAL*8    ) -- Array of constraint on second derivatives  *
! *                           of the refractirivy field over vertical    *
! *                           (1st) and horizonatal components (2nd).    *
! *                           Unit: 1/m^2. If CNS_DR2(i) <= 0.0, then    *
! *                           the constraint is not imposed on the       *
! *                           second derivative over that dimension.     *
! * IVRB     ( INTEGER*4 ) -- Verbosity level.                           *
! *                           0 -- silent                                *
! *                           1 -- terse;                                *
! *                           2 -- terse;                                *
! *                          >2 -- debugging                             *
! *                                                                      *
! * ________________________ Output parameters: ________________________ *
! *                                                                      *
! * SPD ( SPD_3D__TYPE   ) -- The object that keeps information          *
! *                           related to computing slanted path delay.   *
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
! *    IUER ( INTEGER*4, OPT ) -- Universal error handler.               *
! *                           Input: switch IUER=0 -- no error messages  *
! *                                  will be generated even in the case  *
! *                                  of error. IUER=-1 -- in the case of *
! *                                  error the message will be put on    *
! *                                  stdout.                             *
! *                           Output: 0 in the case of successful        *
! *                                   completion and non-zero in the     *
! *                                   case of error.                     *
! *                                                                      *
! * ### 12-JUN-2013   SPD_3D_REFRA  v4.3 (c) L. Petrov  02-JAN-2024  ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'spd.i'
      INCLUDE   'heb.i'
      TYPE     ( SPD_3D__TYPE ) :: SPD
      TYPE     ( HEB__TYPE    ) :: HEB_GEOID_BSPL
      INTEGER*4  MODE, NTIM, DIMO(3), IVRB, IUER
      TYPE     ( HEB__TYPE  ) :: HEB_DELP, HEB_T, HEB_Q, HEB_OH
      REAL*8     CNS_DR2(2)
      REAL*8     P(0:SPD__MLEV), PD(SPD__MLEV), PW(SPD__MLEV), &
     &           TEM(SPD__MLEV), HYPS(0:SPD__MLEV), &
     &           TMP_R8(SPD__MLEV+SPD__MDEG), H(0:SPD__MLEV), &
     &           GE, G, LAT_GDT, LON, ZM(SPD__MLEV), &
     &           T_RATE, T_NLEV, LPL, LPH, PRES_R8(SPD__MLEV), &
     &           HEI_R8(SPD__MLEV), PRES_VAL, TEMP, LOG_PRES, &
     &           Q(SPD__MLEV), LOGP_1D(SPD__MLEV), LOGPW_1D(SPD__MLEV), &
     &           TEM_1D(SPD__MLEV), SPL_H(SPD__MLEV+SPD__MDEG), &
     &           SPL_P(SPD__MLEV+SPD__MDEG), SPL_PW(SPD__MLEV+SPD__MDEG), &
     &           SPL_TEM(SPD__MLEV+SPD__MDEG), &
     &           PRES_LEV, PW_LEV, GMR_DRY, GMR_WET, &
     &           LAY_ARR(SPD__MLEV), ZPD_ARR(SPD__MLEV)
      REAL*4     HEI_R4(SPD__MLEV), ARGS(4)
      REAL*4     EPS_P
      PARAMETER  ( EPS_P = 0.001 )
      REAL*8     LAT_VAL, LON_VAL, CNS_DR2_LON
      INTEGER*8  MEL
      REAL*8     EPS, PRES_HIGH, HEI_MIN, HEI_MIN_INT, HEI_MAX, &
     &           PRES_LN_0, PRES_LN_RATE, SPD__APS_TEMP, MD, MW, &
     &           FILL_VALUE
      PARAMETER  ( PRES_HIGH    = 25000.0D0 )
      PARAMETER  ( HEI_MIN      =  -500.0D0 )
      PARAMETER  ( HEI_MIN_INT  = -1000.0D0 )
      PARAMETER  ( HEI_MAX      =  9000.0D0 )
      PARAMETER  ( PRES_LN_0    = 11.5476D0 )
      PARAMETER  ( PRES_LN_RATE = -1.323D-4 )
      PARAMETER  ( FILL_VALUE   = 1.D15     )
      PARAMETER  ( MD = SPD__MA_MAPL  )
      PARAMETER  ( MW = SPD__H2O_MAPL )
      PARAMETER  ( EPS = 1.D-5 )
      LOGICAL*1  FL_ATT_INTR, FL_PWT
      CHARACTER  STR*128, STR1*128, TEST_STR*8
      INTEGER*4  L_LON, L_LAT, L_LEV, L_TIM, M_LEV, M_TYP, L_TYP, IND, &
     &           K_LEV, N_LEV, I_LON, DIMS(3), INDS(3), &
     &           J1, J2, J3, J4, J5, J6, J7, J8, J9, J10, J11, J12, J13, &
     &           J14, J15, J16, J17, J18, J19, J20, J21, J22, J23, J24,  &
     &           IT_LON, IT_LAT, IND_P, IND_PW, IND_TEM, IER
#ifdef GNU
      INTEGER*4  NTHR,    NTHR_SAVED
#else
      ADDRESS__TYPE NTHR, NTHR_SAVED
#endif
      REAL*4     QV_LEV(SPD__MLEV)
      REAL*8     T1(8192), X1(8192), X2(8192), R1, R2
      REAL*8     HARR(2), TARR(2), MIN_VAL, MAX_VAL , LON_TST, LAT_TST, &
     &           RLEV
      REAL*8,    ALLOCATABLE :: LEV_R8(:), RES_ARR(:,:), RES_SPL(:,:), &
     &                          REF_3D(:,:,:,:), RES_TMP(:,:,:,:), LON_R8(:), LAT_R8(:)
      REAL*4,    ALLOCATABLE :: BSPL_ATT(:,:,:,:)     
      REAL*4     P_ARG(1-SPD__MDEG:SPD__NP+SPD__MDEG), &
     &           PW_ARG(1-SPD__MDEG:SPD__NW+SPD__MDEG), &
     &           TEM_ARG(1-SPD__MDEG:SPD__NT+SPD__MDEG), RMS
      LOGICAL*1  FL_ERROR
      INTEGER*4, EXTERNAL :: I_LEN, ILEN, IXMN8, IXMN8_S, IXMN4, IXMN4_S
      LOGICAL*4, EXTERNAL :: OMP_IN_PARALLEL, IS_R8_NAN 
      REAL*8,    EXTERNAL :: ISPL8, FSPL8, PW_TO_RELH, SPD_REF_CIDDOR96_W532, &
     &                       SPD_REF_CIDDOR96_W1064, SPD_REF_RUEGER02, &
     &                       SPD_REF_APARICIO11, SPD_REF_APARICIO11_WATER, &
     &                       GET_GEOID, ATT_ITU_R13, SPL_INT, VAL_1D_BSPL
      REAL*4,    EXTERNAL :: VAL_3D_BSPL4, VAL_4D_BSPL4, VAL_3D_BSPLE3_R4
      LOGICAL*4, EXTERNAL :: IS_R4_NAN
      CHARACTER, EXTERNAL :: MJDSEC_TO_DATE*30
#ifdef GNU
      INTEGER*4, EXTERNAL     :: OMP_GET_MAX_THREADS, OMP_GET_NUM_THREADS 
#else
      ADDRESS__TYPE, EXTERNAL :: OMP_GET_MAX_THREADS, OMP_GET_NUM_THREADS 
#endif
!
#ifndef SERIAL
      NTHR_SAVED = OMP_GET_MAX_THREADS()
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
      FL_ATT_INTR = .FALSE. ! ??????
      FL_PWT      = .TRUE.
!
! --- Parameters for tests, when test mode is turned on
!
      SPD%CONF%TEST_STR = 'timer'
!      LON_TST  =  90.0*DEG__TO__RAD
!      LAT_TST  = -90.0*DEG__TO__RAD
      LON_TST  =   6*DEG__TO__RAD
      LAT_TST  = 45.0*DEG__TO__RAD
!      SPD%CONF%TEST_STR = 'pl_temp'
!      SPD%CONF%TEST_STR = 'pl_pres'
!      SPD%CONF%TEST_STR = 'pl_pw'
!      SPD%CONF%TEST_STR = 'tab_pres'
!      SPD%CONF%TEST_STR = 'pl_refs'
!
! --- Grid of the input numerical models
!
      L_LON = HEB_DELP%DIMS(1)
      L_LAT = HEB_DELP%DIMS(2)
      L_LEV = HEB_DELP%DIMS(3)
      IF ( SPD%CONF%BSPL_3WAV == SPD__YES ) THEN
           IND_P = SPD__MWAV + 1
           IND_PW = SPD__MWAV + 2
           IND_TEM = SPD__MWAV + 3
        ELSE
           IND_P = SPD__MTYP + SPD%CONF%N_FRQ + 1
           IND_PW = SPD__MTYP + SPD%CONF%N_FRQ + 2
           IND_TEM = SPD__MTYP + SPD%CONF%N_FRQ + 3
      END IF 
      IF ( L_LON > 1024 ) THEN
           FL_PWT = .FALSE.
      END IF
!
! --- Checks of OH file (ortho-height)
!
      IF ( HEB_OH%SDS_NAME .NE. 'height above the WGS84 geoid' ) THEN
           CALL ERR_LOG ( 5411, IUER, 'SPD_3D_REFRA', 'Trap of internal '// &
     &         'control: the science dataset name of '//TRIM(HEB_OH%FILE_NAME)// &
     &         ' is '//TRIM(HEB_OH%SDS_NAME)//' while '// &
     &         'height above the WGS84 geoid was expected' )
           RETURN 
      END IF
!
      IF ( HEB_OH%DIMS(1) .NE. HEB_DELP%DIMS(1) .OR. &
     &     HEB_OH%DIMS(2) .NE. HEB_DELP%DIMS(2)      ) THEN
           WRITE ( 6, * ) 'HEB_OH%DIMS(1:2)   = ', HEB_OH%DIMS(1:2) 
           WRITE ( 6, * ) 'HEB_DELP%DIMS(1:2) = ', HEB_DELP%DIMS(1:2) 
           CALL ERR_LOG ( 5412, IUER, 'SPD_3D_REFRA', 'Error in configuration: '// &
     &         'Dimension of HEB-file with gridded surface orto-height is '// &
     &         'not the same as dimension of the pressure field' )
           RETURN 
      END IF
!
      IT_LON = 1 + L_LON*(LON_TST/PI2)
      IF ( IT_LON > L_LON ) IT_LON = IT_LON - L_LON
      IT_LAT = 1 + (L_LAT-1)*((LAT_TST+P2I)/PI__NUM)
!
! --- Grid of the output Total pressure, partial pressure of water vapor and air temperature
!
      IF ( DIMO(1) == 0 ) THEN
           M_LEV = SPD__MLEV
           SPD%NLEV = M_LEV
           IF ( SPD%CONF%SOB_ALG == SOB__ALG_ZPD .OR. SPD%CONF%SOB_ALG == SOB__ALG_MZPD ) THEN
                SPD%NLON = L_LON
              ELSE 
                SPD%NLON = L_LON+2
           END IF
           SPD%NLAT = L_LAT
         ELSE
           M_LEV = L_LEV + SPD__LBOT + SPD__LTOP
           SPD%NLEV = DIMO(1)
           SPD%NLON = DIMO(2)+2
           SPD%NLAT = DIMO(3)
      END IF
      SPD%NFRQ = SPD%CONF%N_FRQ
      IF ( SPD%CONF%BSPL_3WAV == SPD__YES ) THEN
           SPD%NTYP = SPD__MWAV
         ELSE 
           SPD%NTYP = SPD__MTYP + SPD%NFRQ
      END IF
      L_TYP = SPD%NTYP + 3 
      IF ( FL_PWT ) THEN
           M_TYP = SPD%NTYP + 3 
         ELSE
           M_TYP = SPD%NTYP
      END IF
      IF ( IVRB > 1 ) THEN
           WRITE ( 6, * ) 'SPD_3D_REFRA DIMO   = ', DIMO
           WRITE ( 6, * ) 'SPD_3D_REFRA FL_PWT = ', FL_PWT, ' L_TYP= ', L_TYP, ' M_TYP= ', M_TYP, ' inds= ', IND_TEM, IND_PW, IND_P
           WRITE ( 6, * ) 'SPD_3D_REFRA M_LEV  = ', M_LEV, ' L_LEV= ', L_LEV, ' NLEV = ', SPD%NLEV
      END IF
      IF ( ASSOCIATED ( SPD%REF_3D) ) DEALLOCATE ( SPD%REF_3D )
      IF ( DIMO(1) > 0 ) THEN
!
! -------- Smoothing mode: allocate memory for temporary array RED_3D
!
           ALLOCATE ( REF_3D(1-SPD__MDEG:SPD%NLEV,L_LON+2,L_LAT,M_TYP), &
     &                STAT=IER )
           MEL = INT8(SPD%NLEV+SPD__MDEG)*INT8(L_LON+2)*INT8(L_LAT)
           IF ( IER .NE. 0 ) THEN
                CALL CLRCH  ( STR )
                CALL IINCH8 ( INT8(4)*MEL, STR )
                CALL ERR_LOG ( 5413, IUER, 'SPD_3D_REFRA', 'Failure in an attempt '// &
     &              'to allocate '//STR(1:I_LEN(STR))//' bytes of dynamic memory '// &
     &              'for array REF_3D' )
                RETURN 
           END IF
           ALLOCATE ( LON_R8(L_LON+2) )
           ALLOCATE ( LAT_R8(L_LAT)   )
           SPD%REF_3D_STATUS = SPD__UNDF
         ELSE 
!
! -------- Alolocate memory for the output array
!
           IF ( SPD%CONF%SOB_ALG == SOB__ALG_ZPD ) THEN
                ALLOCATE ( SPD%REF_3D(1,SPD%NLON,SPD%NLAT,3), STAT=IER )
                MEL = INT8(SPD%NLON)*INT8(SPD%NLAT)*INT8(3)
              ELSE IF ( SPD%CONF%SOB_ALG == SOB__ALG_MZPD ) THEN
                ALLOCATE ( SPD%REF_3D(1,SPD%NLON,SPD%NLAT,SPD%CONF%N_LAY+1), STAT=IER )
                MEL = INT8(SPD%NLON)*INT8(SPD%NLAT)*INT8(SPD%CONF%N_LAY+1)
                LAY_ARR(1:SPD%CONF%N_LAY) = SPD%CONF%LAY_ARR(1:SPD%CONF%N_LAY) 
              ELSE
                ALLOCATE ( SPD%REF_3D(1-SPD__MDEG:SPD%NLEV,1-SPD__MDEG:SPD%NLON,1-SPD__MDEG:SPD%NLAT,SPD%NTYP), &
     &                     STAT=IER )
                MEL = INT8(SPD%NLEV+SPD__MDEG)*INT8(SPD%NLON+SPD__MDEG)*INT8(SPD%NLAT+SPD__MDEG)*INT8(SPD%NTYP)
           END IF
!
           IF ( IER .NE. 0 ) THEN
                CALL CLRCH  ( STR )
                CALL IINCH8 ( INT8(4)*MEL, STR )
                CALL ERR_LOG ( 5414, IUER, 'SPD_3D_REFRA', 'Failure in an attempt '// &
     &              'to allocate '//STR(1:I_LEN(STR))//' bytes of dynamic memory '// &
     &              'for array SPD%REF_3D' )
                RETURN 
           END IF
           SPD%REF_3D = 0.0D0
           SPD%REF_3D_STATUS = SPD__ALLO
      END IF
      IF ( IVRB > 2 ) THEN
           WRITE ( 6, * ) 'SPD_3D_REFRA Size: ', SIZEOF(SPD%REF_3D)/1.0E9, ' Gb typs: ', M_TYP, SPD%NTYP
           WRITE ( 6, * ) 'SPD_3D_REFRA dims: ', SPD%NLON, SPD%NLAT, SPD%NTYP
           WRITE ( 6, * ) 'SPD_3D_REFRA Shap: ', SHAPE ( SPD%REF_3D )
      END IF
!
! --- Allocate memory for total pressure, partial water vapour pressure, and temperature
!
      IF ( ASSOCIATED ( SPD%SPR_3D) ) DEALLOCATE ( SPD%SPR_3D )
      IF ( ASSOCIATED ( SPD%SPW_3D) ) DEALLOCATE ( SPD%SPW_3D )
      IF ( ASSOCIATED ( SPD%STM_3D) ) DEALLOCATE ( SPD%STM_3D )
      IF ( FL_PWT ) THEN
           ALLOCATE ( SPD%SPR_3D(1-SPD__MDEG:SPD%NLEV,1-SPD__MDEG:SPD%NLON,1-SPD__MDEG:SPD%NLAT), &
     &                STAT=IER )
           MEL = INT8(SPD%NLEV+SPD__MDEG)*INT8(SPD%NLON+SPD__MDEG)*INT8(SPD%NLAT+SPD__MDEG)
           IF ( IER .NE. 0 ) THEN
                CALL CLRCH  ( STR )
                CALL IINCH8 ( INT8(8)*MEL, STR )
                CALL ERR_LOG ( 5415, IUER, 'SPD_3D_REFRA', 'Failure in an attempt '// &
     &              'to allocate '//STR(1:I_LEN(STR))//' bytes of dynamic memory '// &
     &              'for array SPD%SPR_3D' )
                RETURN 
           END IF
           SPD%SPR_3D = 0.0D0
!
           ALLOCATE ( SPD%SPW_3D(1-SPD__MDEG:SPD%NLEV,1-SPD__MDEG:SPD%NLON,1-SPD__MDEG:SPD%NLAT), &
     &                STAT=IER )
           MEL = INT8(SPD%NLEV+SPD__MDEG)*INT8(SPD%NLON+SPD__MDEG)*INT8(SPD%NLAT+SPD__MDEG)
           IF ( IER .NE. 0 ) THEN
                CALL CLRCH  ( STR )
                CALL IINCH8 ( INT8(8)*MEL, STR )
                CALL ERR_LOG ( 5416, IUER, 'SPD_3D_REFRA', 'Failure in an attempt '// &
     &              'to allocate '//STR(1:I_LEN(STR))//' bytes of dynamic memory '// &
     &              'for array SPD%SPW_3D' )
                RETURN 
           END IF
           SPD%SPW_3D = 0.0D0
!
           ALLOCATE ( SPD%STM_3D(1-SPD__MDEG:SPD%NLEV,1-SPD__MDEG:SPD%NLON,1-SPD__MDEG:SPD%NLAT), &
     &                STAT=IER )
           MEL = INT8(SPD%NLEV+SPD__MDEG)*INT8(SPD%NLON+SPD__MDEG)*INT8(SPD%NLAT+SPD__MDEG)
           IF ( IER .NE. 0 ) THEN
                CALL CLRCH  ( STR )
                CALL IINCH8 ( INT8(8)*MEL, STR )
                CALL ERR_LOG ( 5417, IUER, 'SPD_3D_REFRA', 'Failure in an attempt '// &
     &              'to allocate '//STR(1:I_LEN(STR))//' bytes of dynamic memory '// &
     &              'for array SPD%SPD%STM_3D' )
                RETURN 
           END IF
           SPD%STM_3D = 0.0D0
           IF ( IVRB > 2 ) THEN
                WRITE ( 6, * ) 'SPD_3D_REFRA-2 Mem: ', (3*MEL)/1.0E9, ' Gb'
           END IF
      END IF
      SPD%NWP_TITLE       = HEB_DELP%TITLE
      SPD%NWP_INSTITUTION = HEB_DELP%INSTITUTION 
      SPD%NWP_PROD_NAME   = HEB_DELP%PROD_NAME
      SPD%NWP_REFERENCES  = HEB_DELP%REFERENCES
!
      ALLOCATE ( LEV_R8(1-SPD__MDEG:M_LEV+SPD__MDEG), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 5418, IUER, 'SPD_3D_REFRA', 'Failure in an attempt '// &
     &         'to allocate dynamic memory for array LEV_R8' )
           RETURN 
      END IF
      LEV_R8 = 0.0D0
!
      ALLOCATE ( RES_ARR(M_LEV,L_TYP), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 5419, IUER, 'SPD_3D_REFRA', 'Failure in an attempt '// &
     &         'to allocate dynamic memory for array RES_ARR' )
           RETURN 
      END IF
      RES_ARR = 0.0D0
!
      ALLOCATE ( RES_SPL(1-SPD__MDEG:SPD%NLEV,L_TYP), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 5420, IUER, 'SPD_3D_REFRA', 'Failure in an attempt '// &
     &         'to allocate dynamic memory for array RES_SPL' )
           RETURN 
      END IF
      RES_SPL = 0.0D0
!
      IF ( FL_ATT_INTR .AND. SPD%NFRQ > 0 ) THEN
!
! -------- Allocate memory for specific attenuation array
!
           ALLOCATE ( BSPL_ATT(1-SPD__MDEG:SPD__NP,1-SPD__MDEG:SPD__NW,1-SPD__MDEG:SPD__NT,SPD%NFRQ), STAT=IER )
           MEL = INT8(SPD__NP+SPD__MDEG)*INT8(SPD__NW+SPD__MDEG)*INT8(SPD__NT+SPD__MDEG)*INT8(SPD%NFRQ)
           IF ( IER .NE. 0 ) THEN
                CALL CLRCH  ( STR )
                CALL IINCH8 ( MEL, STR )
                CALL ERR_LOG ( 5421, IUER, 'SPD_3D_REFRA', 'Failure in an attempt '// &
     &              'to allocate '//STR(1:I_LEN(STR))//' bytes of dynamic memory '// &
     &               'for array ATT' )
                RETURN 
           END IF
           IF ( IVRB > 2 ) THEN
                WRITE ( 6, * ) 'SPD_3D_REFRA-3 Mem: ', MEL
           END IF
!
           DIMS(1) = SPD__NP
           DIMS(2) = SPD__NW
           DIMS(3) = SPD__NT
!
           IF ( SPD%CONF%TEST_STR == 'timer' ) THEN
                CALL WALL_TIMER ( %VAL(0) )
           END IF
!
! -------- Compute atmospheric attenuation at the specifited frequencies and put it in BSPL_ATT
!
           CALL ERR_PASS ( IUER, IER )
           CALL SPD_ATT_INTR ( NTHR, SPD, SPD__NP, SPD__NW, SPD__NT, &
     &                         P_ARG, PW_ARG, TEM_ARG, BSPL_ATT, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 5422, IUER, 'SPD_3D_REFRA', 'Failure to expand '// &
     &              'specific absorption of the atmosphere into 3D B-spline basis' )
                RETURN 
           END IF
           IF ( SPD%CONF%TEST_STR == 'timer' ) THEN
                CALL WALL_TIMER ( STR )
                WRITE ( 6, '(A)' ) 'Computing attenuation: '//STR(1:I_LEN(STR)-5)
                CALL WALL_TIMER ( %VAL(0) )
           END IF
      END IF
!
      IF ( SPD%CONF%TEST_STR == 'timer' ) THEN
           WRITE ( 6, '(A,I6,1X,I6,1X,A,1XI3)' ) 'SPD_3D_REFRA NLON/NLAT = ', &
     &                  HEB_DELP%DIMS(1), HEB_DELP%DIMS(2), ' NTHR= ', NTHR
           CALL WALL_TIMER ( %VAL(0) ) 
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
             LEV_R8(J2) = DEXP (           DLOG(SPD__U_MIN - SPD__U_FLO) + &
     &                           (J2-1)* ( DLOG(SPD__U_MAX - SPD__U_FLO) - &
     &                                     DLOG(SPD__U_MIN - SPD__U_FLO) )/(DIMO(1)-1) &
     &                         ) + SPD__U_FLO
 420      CONTINUE 
      END IF
!
      IF ( ASSOCIATED ( SPD%LEV ) ) DEALLOCATE ( SPD%LEV )
      ALLOCATE ( SPD%LEV(1-SPD__MDEG:SPD__MLEV+SPD__MDEG), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( 4*(SPD__MLEV+SPD__MDEG), STR )
           CALL ERR_LOG ( 5423, IUER, 'SPD_3D_REFRA', 'Failure in '// &
     &         'an attempt to allocate '//STR(1:I_LEN(STR))// &
     &         ' bytes of dynamic memory for array SPD%LEV' )
           RETURN 
      END IF
      SPD%LEV = 0.0D0
!
      IF ( ASSOCIATED ( SPD%LAT ) ) DEALLOCATE ( SPD%LAT )
      ALLOCATE ( SPD%LAT(1-SPD__MDEG:SPD%NLAT+SPD__MDEG), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( 4*(SPD%NLAT+SPD__MDEG), STR )
           CALL ERR_LOG ( 5424, IUER, 'SPD_3D_REFRA', 'Failure in '// &
     &         'an attempt to allocate '//STR(1:I_LEN(STR))// &
     &         ' bytes of dynamic memory for array SPD%LAT' )
           RETURN 
      END IF
      SPD%LAT = 0.0D0
!
      IF ( ASSOCIATED ( SPD%LON ) ) DEALLOCATE ( SPD%LON )
      ALLOCATE ( SPD%LON(1-SPD__MDEG:SPD%NLON+SPD__MDEG), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( 4*(SPD%NLON+SPD__MDEG), STR )
           CALL ERR_LOG ( 5425, IUER, 'SPD_3D_REFRA', 'Failure in '// &
     &         'an attempt to allocate '//STR(1:I_LEN(STR))// &
     &         ' bytes of dynamic memory for array SPD%LON' )
           RETURN 
      END IF
      SPD%LON = 0.0D0
!
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
! 
#ifndef SERIAL
!$OMP    PARALLEL DO IF ( NTHR > 1 ), DEFAULT(NONE), &
!$OMP&   FIRSTPRIVATE ( LAY_ARR ), &
!$OMP&   PRIVATE ( J4, J5, J6, J7, J8, J9, J10, J11, J12, IER, G, I_LON, LON, P, TMP_R8, &
!$OMP&             HYPS, H, PW, PD, TEM, ZM, Q, QV_LEV, SPL_TEM, SPL_H, SPL_PW, SPL_P, &
!$OMP&             LOGP_1D, LOGPW_1D, TEM_1D, RLEV, HEI_R4, HEI_R8, T_RATE, T_NLEV, &
!$OMP&             RES_ARR, RES_SPL, GMR_DRY, GMR_WET, ZPD_ARR, K_LEV, N_LEV, IND, &
!$OMP&             ARGS, INDS, r1, r2 ), &
!$OMP&   SHARED   ( FL_ERROR, L_LON, L_LEV, J3, SPD, HEB_Q, HEB_DELP, HEB_T, HEB_OH, &
!$OMP&              GE, LAT_GDT, IT_LON, IT_LAT, LON_TST, LAT_TST, FL_ATT_INTR, &
!$OMP&              M_LEV, IND_TEM, IND_PW, IND_P, P_ARG, PW_ARG, TEM_ARG, &
!$OMP&              DIMS, DIMO, M_TYP, LEV_R8, REF_3D, &
!$OMP&              BSPL_ATT, T1, IVRB, NTHR, IUER, FL_PWT ), &
!$OMP&   SCHEDULE ( STATIC, NTHR )
#endif
         DO 440 J4=1,L_LON
            IF ( SPD%CONF%SOB_ALG .NE. SOB__ALG_ZPD .AND. SPD%CONF%SOB_ALG .NE. SOB__ALG_MZPD ) THEN
                 IF ( .NOT. SPD%MASK(J4,J3) ) GOTO 440
            END IF
            IF ( FL_ERROR ) GOTO 440
!
! --------- NB: Merra/Geos longitude starts from -180deg   to +180 deg
! --------- J4    -- Merra/GEOS indexing from    -180 deg
! --------- I_LON -- SPD indexing from 0deg to   +360 deg
!
            I_LON = J4 + L_LON/2
            IF ( I_LON > L_LON ) I_LON = I_LON - L_LON
            LON = (I_LON - 1)/(L_LON/PI2)
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
                CALL ERR_LOG ( 5426, IUER, 'SPD_3D_REFRA', 'Error '// &
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
              K_LEV = L_LEV+1-J5
              IF ( J5 == 1 ) THEN
                   P(K_LEV) = P(K_LEV+1) + HEB_DELP%VAL(J4,J3,J5,1)/2.0D0
                 ELSE 
                   P(K_LEV) = P(K_LEV+1) + HEB_DELP%VAL(J4,J3,J5-1,1)/2.0D0 + &
     &                                     HEB_DELP%VAL(J4,J3,J5,1)/2.0D0 
              END IF
              IF ( P(K_LEV) < PRES_HIGH ) N_LEV = K_LEV
!
! ----------- Compute approximate gravity acceleration at a given pressure level
! ----------- using a regression formulae for the US Standard Atmosphere
!
              G = GE*(1.D0 + SPD__GP0 + SPD__GP1*DLOG(P(K_LEV)))
!
! ----------- PW -- partial pressure of water vapour
!
              Q(K_LEV)    = QV_LEV(J5)
              PW(K_LEV)   = P(K_LEV)* Q(K_LEV)/ ( MW/MD + (1.0D0 - MW/MD)* Q(K_LEV) )
!
! ----------- PD -- Partial pressure of dry air
!
              PD(K_LEV)  = P(K_LEV) - PW(K_LEV)
              TEM(K_LEV) = HEB_T%VAL(J4,J3,J5,1)
              ZM(K_LEV)  = 1.D0/ &
     &           ( 1.0D0 &
     &             - P(K_LEV)/TEM(K_LEV)*         (SPD__COMP_A0        + SPD__COMP_A1*(TEM(K_LEV) - SPD__ABS_TEMP) +  &
     &                                                                   SPD__COMP_A2*(TEM(K_LEV) - SPD__ABS_TEMP)**2 ) &
     &             + PW(K_LEV)/TEM(K_LEV)*        (SPD__COMP_B0        + SPD__COMP_B1*(TEM(K_LEV) - SPD__ABS_TEMP)) &
     &             + PW(K_LEV)**2/(P(K_LEV)*TEM(K_LEV))* (SPD__COMP_C0 + SPD__COMP_C1*(TEM(K_LEV) - SPD__ABS_TEMP)) &
     &             + P(K_LEV)**2/TEM(K_LEV)**2*    SPD__COMP_D0 &
     &             + PW(K_LEV)**2/TEM(K_LEV)**2*   SPD__COMP_E0 &
     &           )
!
! ----------- Compute function HYPS that is the right hand-side of 
! ----------- the hypsometric equation, while the left hand side is h(P)
!
              HYPS(K_LEV) = SPD__R_MAPL*TEM(K_LEV)/ &
     &                     (G*(MD*PD(K_LEV) + MW*PW(K_LEV))*ZM(K_LEV))
 450       CONTINUE 
!
! -------- Set ground values
!
           P(0) = P(1) + HEB_DELP%VAL(J4,J3,L_LEV,1)/2.0D0
           HYPS(0) = HYPS(1) - (P(1) - P(0))*(HYPS(2) - HYPS(1))/(P(2) - P(1))
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
                CALL ERR_LOG ( 5427, IUER, 'SPD_3D_REFRA', 'Failure in an attempt '// &
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
           H(0) = HEB_OH%VAL(J4,J3,1,1)
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
                  CALL ERR_LOG ( 5429, IUER, 'SPD_3D_REFRA', 'Failure in '// &
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
                CALL ERR_LOG ( 5430, IUER, 'SPD_3D_REFRA', 'Failure in '// &
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
                CALL ERR_LOG ( 5431, IUER, 'SPD_3D_REFRA', 'Failure in '// &
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
           H(0) = HEB_OH%VAL(J4,J3,1,1)
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
                  CALL ERR_LOG ( 5432, IUER, 'SPD_3D_REFRA', 'Failure in '// &
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
              IF ( SPD%CONF%TEST_STR == 'tab_pres' .AND. &
     &             IT_LON == I_LON .AND.                 &
     &             IT_LAT == J3                          ) THEN
                   WRITE ( 6, 210 ) J8, H(J8), -P(J8)
 210               FORMAT ( 'ILEV= ', I2, ' Hei: ', F10.2, ' Pres: ', F10.1 )
              END IF
480        CONTINUE 
!
! -------- Now we compute interpolation spline of the total pressure as 
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
                CALL ERR_LOG ( 5433, IUER, 'SPD_3D_REFRA', 'Failure in an attempt '// &
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
                CALL ERR_LOG ( 5434, IUER, 'SPD_3D_REFRA', 'Failure in an attempt '// &
     &              'to compute coefficients of the interpolating spline' )
                FL_ERROR = .TRUE.
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
                CALL ERR_LOG ( 5435, IUER, 'SPD_3D_REFRA', 'Failure in an attempt '// &
     &              'to compute coefficients of the interpolating spline' )
                FL_ERROR = .TRUE.
#ifndef SERIAL
!$OMP           END CRITICAL
#endif
                GOTO 440
           END IF
!
           IF ( SPD%CONF%TEST_STR == 'pl_temp' .AND. &
     &          IT_LON == I_LON .AND.                &
     &          IT_LAT == J3                         ) THEN
!               
                WRITE ( 6, * ) 'IT_LON/IT_LAT= ', INT2(IT_LON), INT2(IT_LAT), &
     &                         ' LON/LAT= ', SNGL(LON_TST/DEG__TO__RAD), SNGL(LAT_TST/DEG__TO__RAD)
                CALL DIAGI_SETDEF ( IER, 'DIAGI_CTIT', 'Temp(K) Orig' )
                CALL DIAGI_1 ( L_LEV, H(1), TEM_1D, IER )
             ELSE IF ( SPD%CONF%TEST_STR == 'pl_pres' .AND. &
     &          IT_LON == I_LON .AND.                &
     &          IT_LAT == J3                         ) THEN
!               
                write ( 6, * ) 'h(0) = ', sngl(h(0)), ' h(1) = ', sngl(h(1)), ' h(2) = ', sngl(h(2)) ! %%%%
                write ( 6, * ) 'p(0) = ', sngl(p(0)), ' p(1) = ', sngl(p(1)), ' p(2) = ', sngl(p(2)) ! %%%%               
                WRITE ( 6, * ) 'IT_LON/IT_LAT= ', INT2(IT_LON), INT2(IT_LAT), &
     &                         ' LON/LAT= ', SNGL(LON_TST/DEG__TO__RAD), SNGL(LAT_TST/DEG__TO__RAD)
                CALL DIAGI_SETDEF ( IER, 'DIAGI_CTIT', 'Pres(Pa) Orig' )
                CALL DIAGI_1 ( L_LEV, H(1), P(1), IER )
             ELSE IF ( SPD%CONF%TEST_STR == 'pl_pw' .AND. &
     &          IT_LON == I_LON .AND.                &
     &          IT_LAT == J3                         ) THEN
!               
                WRITE ( 6, * ) 'IT_LON/IT_LAT= ', INT2(IT_LON), INT2(IT_LAT), &
     &                         ' LON/LAT= ', SNGL(LON_TST/DEG__TO__RAD), SNGL(LAT_TST/DEG__TO__RAD)
                CALL DIAGI_SETDEF ( IER, 'DIAGI_CTIT', 'Pw(Pa) Orig' )
                CALL DIAGI_1 ( L_LEV, H(1), PW(1), IER )
           END IF
!
! -------- Now we compute total pressure, water vapour pressure and air temperature
! -------- on a global grid
!
           DO 490 J9=1,M_LEV
              IF ( DIMO(1) == 0 ) THEN
                   RLEV = J9-1 - (SPD__MLEV-1)/2
                   SPD%LEV(J9) = DEXP ( (RLEV - SPD__U3_GMAO72)/SPD__U1_GMAO72 ) - SPD__U2_GMAO72
                 ELSE IF ( J9 .LE. SPD__LBOT ) THEN
                   SPD%LEV(J9) = DEXP (  DLOG(SPD__U_MIN - SPD__U_FLO) + (J9-1)* &
     &                                 ( DLOG(H(1)       - SPD__U_FLO) - &
     &                                   DLOG(SPD__U_MIN - SPD__U_FLO) )/SPD__LBOT ) + &
     &                                   SPD__U_FLO
                 ELSE IF ( J9 .EQ. SPD__LBOT + 1 ) THEN
                   SPD%LEV(J9) = H(1)
                 ELSE IF ( J9 .EQ. SPD__LBOT + L_LEV + 1 ) THEN
                   SPD%LEV(J9) = H(L_LEV)
                 ELSE IF ( J9 > SPD__LBOT + L_LEV + 1 ) THEN
                   SPD%LEV(J9) = DEXP (  DLOG(H(L_LEV) - SPD__U_FLO) + &
     &                                 (J9 - SPD__LBOT - L_LEV)* &
     &                                 ( DLOG(SPD__U_MAX - SPD__U_FLO) - &
     &                                   DLOG(H(L_LEV)   - SPD__U_FLO) )/SPD__LTOP ) + &
     &                                   SPD__U_FLO
                 ELSE
                   SPD%LEV(J9) = DEXP (  DLOG(H(1) - SPD__U_FLO) + &
     &                                   (J9 - SPD__LBOT)* &
     &                                   ( DLOG(H(L_LEV) - SPD__U_FLO) - &
     &                                     DLOG(H(1)     - SPD__U_FLO) &
     &                                   )/L_LEV &
     &                                ) + SPD__U_FLO
              END IF
              HEI_R4(J9) = SPD%LEV(J9) 
              HEI_R8(J9) = SPD%LEV(J9) 
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
                   RES_ARR(J9,IND_TEM) =  TEM(1) + T_RATE*(HEI_R8(J9) - H(1))
                   RES_ARR(J9,IND_PW)  =  PW(1)*(RES_ARR(J9,IND_TEM)/TEM(1))**(-G*MW/(SPD__R_MAPL*T_RATE))
                   RES_ARR(J9,IND_P)   = -P(1) *(RES_ARR(J9,IND_TEM)/TEM(1))**(-G*MD/(SPD__R_MAPL*T_RATE))
                 ELSE IF ( HEI_R8(J9) > H(L_LEV) ) THEN
!
! ---------------- This is above the upper level. We consider the atmosphere isothermal.
!
                   GMR_DRY = G*SPD__MA/SPD__R        
                   GMR_WET = G*SPD__MW/SPD__R        
                   RES_ARR(J9,IND_TEM) =  TEM(L_LEV)
                   RES_ARR(J9,IND_PW)  =  PW(L_LEV)*DEXP ( -GMR_WET*(HEI_R8(J9) - H(L_LEV))/RES_ARR(J9,IND_TEM) )
                   RES_ARR(J9,IND_P)   = -P(L_LEV)*DEXP  ( -GMR_DRY*(HEI_R8(J9) - H(L_LEV))/RES_ARR(J9,IND_TEM) )
                 ELSE
!
! ---------------- Compute air temperature, partial water vapour pressure, 
! ---------------- and the total pressure by spline interpolation
!
                   IND = IXMN8 ( L_LEV, H(1), HEI_R8(J9) )
                   RES_ARR(J9,IND_TEM) =        FSPL8 ( HEI_R8(J9), L_LEV, H(1), TEM_1D,   IND, SPL_TEM )  
                   RES_ARR(J9,IND_PW)  = DEXP ( FSPL8 ( HEI_R8(J9), L_LEV, H(1), LOGPW_1D, IND, SPL_PW  ) )
                   RES_ARR(J9,IND_P)   = DEXP ( FSPL8 ( HEI_R8(J9), L_LEV, H(1), LOGP_1D,  IND, SPL_P   ) )
               END IF
!
               IF ( SPD%CONF%BSPL_3WAV == SPD__YES ) THEN
                    RES_ARR(J9,SPD__W532)  = SPD_REF_CIDDOR96_W532  ( RES_ARR(J9,IND_P), RES_ARR(J9,IND_PW), RES_ARR(J9,IND_TEM) )
                    RES_ARR(J9,SPD__W1064) = SPD_REF_CIDDOR96_W1064 ( RES_ARR(J9,IND_P), RES_ARR(J9,IND_PW), RES_ARR(J9,IND_TEM) )
                    RES_ARR(J9,SPD__RADIO) = SPD_REF_APARICIO11     ( RES_ARR(J9,IND_P), RES_ARR(J9,IND_PW), RES_ARR(J9,IND_TEM) )
                  ELSE 
                    IF ( SPD%CONF%REFR_EXPR == SPD__W532_CIDDOR_1996 ) THEN
                         RES_ARR(J9,SPD__TOT) = SPD_REF_CIDDOR96_W532  ( RES_ARR(J9,IND_P), RES_ARR(J9,IND_PW), RES_ARR(J9,IND_TEM) )
                         RES_ARR(J9,SPD__WAT) = SPD%REF_3D(J9,I_LON,J3,SPD__TOT) - &
     &                                                     SPD_REF_CIDDOR96_W532  ( RES_ARR(J9,IND_P), 0.0D0, RES_ARR(J9,IND_TEM) )
                       ELSE IF ( SPD%CONF%REFR_EXPR == SPD__W1064_CIDDOR_1996 ) THEN
                         RES_ARR(J9,SPD__TOT) = SPD_REF_CIDDOR96_W1064 ( RES_ARR(J9,IND_P), RES_ARR(J9,IND_PW), RES_ARR(J9,IND_TEM) )
                         RES_ARR(J9,SPD__WAT) = SPD%REF_3D(J9,I_LON,J3,SPD__TOT) - &
     &                                                     SPD_REF_CIDDOR96_W1064 ( RES_ARR(J9,IND_P), 0.0D0, RES_ARR(J9,IND_TEM) )
                       ELSE IF ( SPD%CONF%REFR_EXPR == SPD__RADIO_RUEGER_2002 ) THEN
                         RES_ARR(J9,SPD__TOT) = SPD_REF_RUEGER02 ( RES_ARR(J9,IND_P), RES_ARR(J9,IND_PW), RES_ARR(J9,IND_TEM) )
                         RES_ARR(J9,SPD__WAT) = SPD%REF_3D(J9,I_LON,J3,SPD__TOT) - &
     &                                                     SPD_REF_RUEGER02 ( RES_ARR(J9,IND_P), 0.0D0, RES_ARR(J9,IND_TEM) )
                       ELSE IF ( SPD%CONF%REFR_EXPR == SPD__RADIO_APARICIO_2011 ) THEN
                         IF ( SPD%CONF%SOB_ALG == SOB__ALG_MZPD ) THEN
                              RES_ARR(J9,SPD__TOT) = SPD_REF_APARICIO11 ( RES_ARR(J9,IND_P), RES_ARR(J9,IND_PW), RES_ARR(J9,IND_TEM) )
                            ELSE
                              RES_ARR(J9,SPD__TOT) = SPD_REF_APARICIO11 ( RES_ARR(J9,IND_P), RES_ARR(J9,IND_PW), RES_ARR(J9,IND_TEM) )
                              RES_ARR(J9,SPD__WAT) = SPD_REF_APARICIO11_WATER ( RES_ARR(J9,IND_P), RES_ARR(J9,IND_PW), RES_ARR(J9,IND_TEM) )
                         END IF
                    END IF
               END IF
               IF ( SPD%NFRQ > 0 ) THEN
!
! ----------------- Compute atmosphere attenuation
!
                    IF ( FL_ATT_INTR ) THEN
                         ARGS(1) = LOG(RES_ARR(J9,IND_P))
                         ARGS(2) = LOG(RES_ARR(J9,IND_PW))
                         ARGS(3) = RES_ARR(J9,IND_TEM)
!
                         INDS(1) = IXMN4 ( DIMS(1), P_ARG(1),   ARGS(1) )
                         INDS(2) = IXMN4 ( DIMS(2), PW_ARG(1),  ARGS(2) )
                         INDS(3) = IXMN4 ( DIMS(3), TEM_ARG(1), ARGS(3) )
                         IF ( INDS(1) == -1 ) THEN
                              ARGS(1) = P_ARG(1) + EPS_P*(P_ARG(2)-P_ARG(1))
                              INDS(1) = 1
                         END IF
                         IF ( INDS(2) == -1 ) THEN
                              ARGS(2) = PW_ARG(1) + EPS_P*(PW_ARG(2)-PW_ARG(1))
                              INDS(2) = 1
                         END IF
                         IF ( INDS(3) == -1 ) THEN
                              ARGS(3) = TEM_ARG(1) + EPS*(TEM_ARG(2)-TEM_ARG(1))
                              INDS(3) = 1
                         END IF
                    END IF
!
! ----------------- Cycle over frequencies
!
                    DO 4100 J10=1,SPD%NFRQ
                       IF ( FL_ATT_INTR .AND. INDS(1) > 0 .AND. INDS(2) > 0 .AND. INDS(3) > 0 ) THEN 
                            RES_ARR(J9,SPD__MTYP+J10) = &
     &                          LOG ( VAL_3D_BSPLE3_R4 ( ARGS, DIMS, INDS, &
     &                                       P_ARG, PW_ARG, TEM_ARG, &
     &                                       BSPL_ATT(1-SPD__MDEG:SPD__NP,1-SPD__MDEG:SPD__NW,1-SPD__MDEG:SPD__NT,J10) ) &
     &                              )
                          ELSE IF ( FL_ATT_INTR ) THEN
                             IF ( INDS(1) == -2 ) THEN
!$OMP                             CRITICAL
                                  WRITE ( 6, '(A,1PD12.5,A,0PF8.1)' ) 'SPD_3D_REFRA Trap of '// &
     &                                   'internal control: atmospheric pressure P_VAL = ', &
     &                                   RES_ARR(J9,IND_P), ' which is greater than SPD__P_MAX = ', &
     &                                   SPD__P_MAX 
                                  WRITE ( 6, * ) 'J9= ', J9
                                  WRITE ( 6, * ) 'H(1) = ', H(1), ' LAT_GDT= ', LAT_GDT/DEG__TO__RAD, ' LON = ', LON/DEG__TO__RAD
                                  WRITE ( 6, * ) 'RES_ARR(1:SPD__MLEV,IND_P) = ', RES_ARR(1:SPD__MLEV,IND_P) 
                                  WRITE ( 6, * ) 'HEI_R8(1:SPD__MLEV,IND_P)  = ', HEI_R8(1:SPD__MLEV) 
                                  CALL EXIT ( 1 )
!$OMP                             END CRITICAL
                             END IF 
                             IF ( INDS(2) == -2 ) THEN
!$OMP                             CRITICAL
                                  WRITE ( 6, '(A,1PD12.5,A,0PF8.1)' ) 'SPD_3D_REFRA Trap of '// &
     &                                   'internal control: atmospheric pressure PW_VAL = ', &
     &                                   RES_ARR(J9,IND_PW), ' which is greater than SPD__PW_MAX = ', &
     &                                   SPD__PW_MAX 
                                  CALL EXIT ( 1 )
!$OMP                             END CRITICAL
                             END IF 
                             RES_ARR(J9,SPD__MTYP+J10) = LOG(1.0E-12)
                          ELSE
!
! ------------------------- Use direct computation
!
                            RES_ARR(J9,SPD__MTYP+J10) = &
     &                                 DLOG ( ATT_ITU_R13 ( RES_ARR(J9,IND_P), RES_ARR(J9,IND_PW), RES_ARR(J9,IND_TEM), &
     &                                                      SPD%CONF%FRQ_ARR(J10) ) )
                       END IF
 4100               CONTINUE 
               END IF
  490      CONTINUE 
!
           IF ( SPD%CONF%SOB_ALG == SOB__ALG_ZPD ) THEN
!
! ------------- Compute zenith path delay
!
                DO 4110 J11=1,SPD%NTYP
                   RES_SPL(1-SPD__MDEG:0,J11) = 0.0
                   RES_SPL(1:SPD%NLEV,J11)  = RES_ARR(1:SPD%NLEV,J11) 
                   CALL BSPL_1D_CMP ( 3, 0, M_LEV, HEI_R8, RES_SPL(1-SPD__MDEG,J11), IER )
                   SPD%REF_3D(1,I_LON,J3,J11) = SPL_INT ( M_LEV, HEI_R8, 3, RES_SPL(1-SPD__MDEG,J11), HEI_R8(M_LEV) ) - &
     &                                          SPL_INT ( M_LEV, HEI_R8, 3, RES_SPL(1-SPD__MDEG,J11), H(0)          )
 4110           CONTINUE 
!
! ------------- Compute refractivity at the surface and put it into the 3rd slot
!
                IND = IXMN8 ( M_LEV, HEI_R8, H(0) )
                SPD%REF_3D(1,I_LON,J3,3) = VAL_1D_BSPL ( H(0), M_LEV, 3, &
     &                                                   IND, HEI_R8, RES_SPL(1-SPD__MDEG,SPD__TOT) )
              ELSE IF ( SPD%CONF%SOB_ALG == SOB__ALG_MZPD ) THEN
!
! ------------- Compute zenith path delay from the bottom of the atmosphere
! ------------- to the layer array SPD%CONF%LAY_ARR
!
                RES_SPL(1-SPD__MDEG:0,SPD__TOT) = 0.0
                RES_SPL(1:M_LEV,SPD__TOT) = RES_ARR(1:M_LEV,SPD__TOT) 
                LAY_ARR(SPD%CONF%N_LAY+1) = H(0)
                LAY_ARR(SPD%CONF%N_LAY+2) = HEI_R8(M_LEV)
!
                CALL BSPL_1D_CMP ( 3, 0, M_LEV, HEI_R8, RES_SPL(1-SPD__MDEG,SPD__TOT), IER )
                CALL MULTI_SPL_INT ( M_LEV, HEI_R8, 3, RES_SPL(1-SPD__MDEG,SPD__TOT), &
     &                               SPD%CONF%N_LAY+2, LAY_ARR, ZPD_ARR )
!
! ------------- Subtract path delay from the top of the atmopshere at the physial surface.
! ------------- Results: for i in 1 to SPD%CONF%N_LAY, SPD%REF_3D will keep path delay
! -------------          between the surface to the i-th layer.
! -------------          The total path delay from the surface to the top of the 
! -------------          atmosphere will be kept in the slot SPD%CONF%N_LAY+1
!
                IND = 0
                DO 4120 J12=2,SPD%CONF%N_LAY
                   IF ( LAY_ARR(J12) < H(0) ) THEN
!
! --------------------- This layer is below the surface -- set path delay to FILL_VALUE
!
                        SPD%REF_3D(1,I_LON,J3,J12) = FILL_VALUE
                      ELSE IF ( J12 < SPD%CONF%N_LAY ) THEN
                        IF ( IND == 0 ) THEN
!
! -------------------------- This is the first layer above the surface 
! -------------------------- Set path delay integration to the interval between the 
! -------------------------- surface and the next upper leverl
!
                             IND = J12
                             SPD%REF_3D(1,I_LON,J3,J12) = ZPD_ARR(J12+1) - ZPD_ARR(SPD%CONF%N_LAY+1) 
                           ELSE
!
! -------------------------- Set path delay integration to the interval between the 
! -------------------------- J12 and J12-th layer
! -------------------------- surface and the next upper leverl
!
                             SPD%REF_3D(1,I_LON,J3,J12) = ZPD_ARR(J12+1) - ZPD_ARR(J12) 
                        END IF
                      ELSE
!
! --------------------- This is the last layer. Set the path delay integation
! --------------------- interfacl from J12-th layer to the top of the atmosphere
!
                        SPD%REF_3D(1,I_LON,J3,J12) = ZPD_ARR(SPD%CONF%N_LAY+2) - ZPD_ARR(J12) 
                   END IF
 4120           CONTINUE 
!
! ------------- Put the zenith path dalay from the surface to the top of the atmosphere
! ------------- in the SPD%CONF%N_LAY+1 slot 
!
                SPD%REF_3D(1,I_LON,J3,1) = ZPD_ARR(SPD%CONF%N_LAY+2) - ZPD_ARR(SPD%CONF%N_LAY+1) 
!
! ------------- Compute refractivity at the surface and put it into SPD%CONF%N_LAY+2 slot
!
                IND = IXMN8 ( M_LEV, HEI_R8, H(0) )
                SPD%REF_3D(1,I_LON,J3,SPD%CONF%N_LAY+1) = VAL_1D_BSPL ( H(0), M_LEV, 3, &
     &                                                                  IND, HEI_R8, RES_SPL(1-SPD__MDEG,SPD__TOT) )
              ELSE
                IF ( DIMO(1) == 0 ) THEN
                     IF ( SPD%CONF%BSPL_3WAV == SPD__YES ) THEN
                          SPD%REF_3D(1:M_LEV,I_LON,J3,1:3)  = RES_ARR(1:M_LEV,1:3)
                        ELSE 
                          SPD%REF_3D(1:M_LEV,I_LON,J3,1:2)  = RES_ARR(1:M_LEV,1:2)
                     END IF 
                     IF ( FL_PWT ) THEN
                          SPD%SPR_3D(1:M_LEV,I_LON,J3) = RES_ARR(1:M_LEV,IND_P)
                          SPD%SPW_3D(1:M_LEV,I_LON,J3) = RES_ARR(1:M_LEV,IND_PW)
                          SPD%STM_3D(1:M_LEV,I_LON,J3) = RES_ARR(1:M_LEV,IND_TEM)
                     END IF
!
                     IF ( SPD%NFRQ > 0 ) THEN
                          SPD%REF_3D(1:M_LEV,I_LON,J3,SPD__MTYP+1:SPD__MTYP+SPD%NFRQ) = RES_ARR(1:M_LEV,SPD__MTYP+1:SPD__MTYP+SPD%NFRQ)
                     END IF
                   ELSE
!
! ------------------ Compute smoothing spline over height
!
                     CALL ERR_PASS ( IUER, IER )
                     CALL EBSPL_LSQ_CNS3_VEC ( M_LEV, M_TYP, HEI_R8, RES_ARR, &
     &                          SPD%NLEV, SPD__MDEG, LEV_R8(1), RES_SPL, &
     &                          0.0D0, 0.0D0, SPD%CNS_DR2_SIG(1), IER )
                     IF ( IER .NE. 0 ) THEN
!$OMP                     CRITICAL
                          CALL ERR_LOG ( 5436, IUER, 'SPD_3D_REFRA', 'Failure in '// &
     &                        'an attempt to compute smoothing B-spline over '// &
     &                        'height dimension' )
                          FL_ERROR = .TRUE.
!$OMP                     END CRITICAL
                     END IF
                     REF_3D(1-SPD__MDEG:SPD%NLEV-1,I_LON,J3,1:M_TYP) = RES_SPL(1-SPD__MDEG:SPD%NLEV-1,1:M_TYP)
                     IF ( SPD%CONF%TEST_STR == 'pl_refs' .AND. &
     &                    IT_LON == I_LON                .AND. &
     &                    IT_LAT == J3                         ) THEN
                          WRITE ( 6, * ) 'IT_LON = ', IT_LON, ' IT_LAT= ', IT_LAT
                          CALL DIAGI_2 ( M_LEV,      HEI_R8(1), RES_ARR(1,SPD%NTYP), &
     &                                   SPD%NLEV-1, LEV_R8(1), RES_SPL(1,SPD%NTYP), IER )
                     END IF
                END IF
!
                IF ( SPD%CONF%TEST_STR == 'pl_temp' .AND. &
     &               IT_LON == I_LON .AND.                &
     &               IT_LAT == J3                         ) THEN
!               
                     T1(1:SPD__MLEV) = SPD%STM_3D(1:SPD__MLEV,IT_LON,IT_LAT)
                     CALL DIAGI_SETDEF ( IER, 'DIAGI_CTIT', 'Temp(K) regrid' )
                     CALL DIAGI_1 ( SPD__MLEV, SPD%LEV(1), T1, IER )
                   ELSE IF ( SPD%CONF%TEST_STR == 'pl_pres' .AND. &
     &                       IT_LON == I_LON .AND.                &
     &                       IT_LAT == J3                         ) THEN
!               
                     T1(1:SPD__MLEV) = SPD%SPR_3D(1:SPD__MLEV,IT_LON,IT_LAT)
                     CALL DIAGI_SETDEF ( IER, 'DIAGI_CTIT', 'Pres(Pa) regrid' )
                     CALL DIAGI_1 ( SPD__MLEV, SPD%LEV(1), T1, IER )
                   ELSE IF ( SPD%CONF%TEST_STR == 'pl_pw' .AND. &
     &                       IT_LON == I_LON .AND.                &
     &                       IT_LAT == J3                         ) THEN
!               
                     T1(1:SPD__MLEV) = SPD%SPW_3D(1:SPD__MLEV,IT_LON,IT_LAT)
                     CALL DIAGI_SETDEF ( IER, 'DIAGI_CTIT', 'Pw(Pa) regrid' )
                     CALL DIAGI_1 ( SPD__MLEV, SPD%LEV(1), T1, IER )
                END IF
            END IF
  440    CONTINUE 
#ifndef SERIAL
!$OMP END PARALLEL DO
#endif
         IF ( FL_ERROR ) RETURN
         IF ( SPD%CONF%SOB_ALG == SOB__ALG_ZPD .OR. SPD%CONF%SOB_ALG == SOB__ALG_MZPD ) THEN
              CONTINUE 
           ELSE
!
! ----------- We extend longitude coverage to one pixel eastward.
! ----------- The pixel with longitude index L_LON+1,L_LON+2 is equal 
! ----------- to pixels with indexes 1 and 2.
!
              IF ( DIMO(1) == 0 ) THEN
                   SPD%REF_3D(1-SPD__MDEG:SPD__MLEV,L_LON+1,J3,1:SPD%NTYP) = SPD%REF_3D(1-SPD__MDEG:SPD__MLEV,1,J3,1:SPD%NTYP) 
                   SPD%REF_3D(1-SPD__MDEG:SPD__MLEV,L_LON+2,J3,1:SPD%NTYP) = SPD%REF_3D(1-SPD__MDEG:SPD__MLEV,2,J3,1:SPD%NTYP)
!
                   IF ( FL_PWT ) THEN
                        SPD%SPR_3D(1-SPD__MDEG:SPD__MLEV,L_LON+1,J3) = SPD%SPR_3D(1-SPD__MDEG:SPD__MLEV,1,J3)
                        SPD%SPR_3D(1-SPD__MDEG:SPD__MLEV,L_LON+2,J3) = SPD%SPR_3D(1-SPD__MDEG:SPD__MLEV,2,J3)
!
                        SPD%SPW_3D(1-SPD__MDEG:SPD__MLEV,L_LON+1,J3) = SPD%SPW_3D(1-SPD__MDEG:SPD__MLEV,1,J3)
                        SPD%SPW_3D(1-SPD__MDEG:SPD__MLEV,L_LON+2,J3) = SPD%SPW_3D(1-SPD__MDEG:SPD__MLEV,2,J3)
!
                        SPD%STM_3D(1-SPD__MDEG:SPD__MLEV,L_LON+1,J3) = SPD%STM_3D(1-SPD__MDEG:SPD__MLEV,1,J3)
                        SPD%STM_3D(1-SPD__MDEG:SPD__MLEV,L_LON+2,J3) = SPD%STM_3D(1-SPD__MDEG:SPD__MLEV,2,J3)
                   END IF
                 ELSE
                   REF_3D(1-SPD__MDEG:SPD%NLEV,L_LON+1,J3,1:M_TYP) = REF_3D(1-SPD__MDEG:SPD%NLEV,1,J3,1:M_TYP) 
                   REF_3D(1-SPD__MDEG:SPD%NLEV,L_LON+2,J3,1:M_TYP) = REF_3D(1-SPD__MDEG:SPD%NLEV,2,J3,1:M_TYP)
              END IF
         END IF
         IF ( IVRB .GE. 4 ) THEN
              WRITE ( 6, 220 ) J3, L_LAT, CHAR(13)
 220          FORMAT ( '  1st Ilat= ', I4, ' ( ', I4, ' ) ',A$ )
              CALL FLUSH ( 6 )
         END IF
 430  CONTINUE 
      IF ( IVRB .GE. 4 ) WRITE ( 6, '(A)' ) ' ' 
      IF ( IVRB .GE. 2 ) THEN
           WRITE ( 6, * ) 'Finished computation of the refractivity field'
      END IF
      SPD%MJD = HEB_DELP%MJD
      SPD%UTC = HEB_DELP%UTC
      SPD%TAI = SPD%UTC 
      CALL FREE_HEB ( HEB_DELP )
      CALL FREE_HEB ( HEB_T    )
      CALL FREE_HEB ( HEB_Q    )
!
! --- Compute the value of UTC function on MJD,TAI time
!
      CALL ERR_PASS ( IUER, IER )
      CALL SPD_UTC_TO_TAI ( SPD, SPD%MJD, SPD%UTC, SPD%TAI, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 5437, IUER, 'SPD_3D_REFRA', 'Failure to '// &
     &         'compute value of TAI time on moment of UTC function' )
           RETURN 
      END IF
!
! --- Build arrays of longitude/latitude
!
      DO 4130 J13=1,SPD%NLON
         SPD%LON(J13) = (J13-1)*PI2/(SPD%NLON-2)
 4130 CONTINUE 
!
      DO 4140 J14=1,SPD%NLAT
         SPD%LAT(J14) = -P2I + (J14-1)*PI__NUM/(SPD%NLAT-1)
 4140 CONTINUE 
      SPD%REF_3D_STATUS = SPD__LOAD
      IF ( SPD%CONF%SOB_ALG == SOB__ALG_ZPD .OR. SPD%CONF%SOB_ALG == SOB__ALG_MZPD ) THEN
           DEALLOCATE ( LEV_R8  )
           DEALLOCATE ( RES_ARR )
           DEALLOCATE ( RES_SPL )
           CALL ERR_LOG ( 0, IUER )
           RETURN 
      END IF
!
      IF ( SPD%CONF%TEST_STR == 'timer' ) THEN
           CALL WALL_TIMER ( STR )
           WRITE ( 6, '(A)' ) 'Collecting functions to expand: '//STR(1:I_LEN(STR)-5)
           CALL WALL_TIMER ( %VAL(0) )
      END IF
#ifndef SERIAL
#ifdef GNU
      CALL OMP_SET_NUM_THREADS ( %VAL(NTHR_SAVED) )
#else
      CALL OMP_SET_NUM_THREADS (      NTHR_SAVED  )
#endif
#endif
      IF ( DIMO(1) == 0 ) THEN
!
! -------- Compute coefficients of 3D interpolating B-splines
!          ==================================================
!
           DIMS(1) = SPD%NLEV
           DIMS(2) = SPD%NLON
           DIMS(3) = SPD%NLAT
           FL_ERROR = .FALSE.
#ifndef    SERIAL
!$OMP      PARALLEL DO IF ( NTHR > 1 ), DEFAULT(NONE), &
!$OMP&     PRIVATE ( J15, IER  ), &
!$OMP&     SHARED  ( DIMS, SPD, FL_ERROR, IUER )
#endif
           DO 4150 J15=1,SPD%NTYP
              IF ( FL_ERROR ) GOTO 4150
              CALL ERR_PASS ( IUER, IER )
              CALL BSPL_3D_CMP ( SPD__MDEG, 0, DIMS, &
     &                           SPD%LEV(1), SPD%LON(1), SPD%LAT(1), &
     &                           SPD%REF_3D(1-SPD__MDEG:SPD%NLEV,1-SPD__MDEG:SPD%NLON,1-SPD__MDEG:SPD%NLAT,J15), &
     &                           IER )
              IF ( IER .NE. 0 ) THEN
!$OMP              CRITICAL
!$OMP              FLUSH
                   CALL ERR_LOG ( 5438, IUER, 'SPD_3D_REFRA', 'Failure in '// &
     &                 'an attempt to compute coefficients of the 3D '// &
     &                 'interpolating spline for atmospheric attenuation' )
                   FL_ERROR = .TRUE.
!$OMP              END CRITICAL
              END IF
 4150      CONTINUE 
#ifndef    SERIAL
!$OMP      END PARALLEL DO
#endif
           IF ( FL_ERROR ) THEN
                RETURN 
           END IF
!
           IF ( FL_PWT ) THEN
                CALL ERR_PASS ( IUER, IER )
                CALL BSPL_3D_CMP ( SPD__MDEG, 0, DIMS, SPD%LEV(1), SPD%LON(1), SPD%LAT(1), &
     &                             SPD%SPR_3D, IER )
                IF ( IER .NE. 0 ) THEN
                     CALL ERR_LOG ( 5439, IUER, 'SPD_3D_REFRA', 'Failure in '// &
     &                   'an attempt to compute coefficients of the 3D '// &
     &                   'interpolating spline for SPD%SPR_3D' )
                     RETURN 
                END IF
!
                CALL ERR_PASS ( IUER, IER )
                CALL BSPL_3D_CMP ( SPD__MDEG, 0, DIMS, SPD%LEV(1), SPD%LON(1), SPD%LAT(1), &
     &                             SPD%SPW_3D, IER )
                IF ( IER .NE. 0 ) THEN
                     CALL ERR_LOG ( 5440, IUER, 'SPD_3D_REFRA', 'Failure in '// &
     &                   'an attempt to compute coefficients of the 3D '// &
     &                   'interpolating spline for SPD%SPW_3D' )
                     RETURN 
                END IF
!
                CALL ERR_PASS ( IUER, IER )
                CALL BSPL_3D_CMP ( SPD__MDEG, 0, DIMS, SPD%LEV(1), SPD%LON(1), SPD%LAT(1), &
     &                             SPD%STM_3D, IER )
                IF ( IER .NE. 0 ) THEN
                     CALL ERR_LOG ( 5441, IUER, 'SPD_3D_REFRA', 'Failure in '// &
     &                   'an attempt to compute coefficients of the 3D '// &
     &                   'interpolating spline for SPD%STM_3D' )
                     RETURN 
                END IF
           END IF
!
           IF ( SPD%CONF%TEST_STR == 'timer' ) THEN
                CALL WALL_TIMER ( STR )
                WRITE ( 6, '(A)' ) 'Computation of 3D B-splines:    '//STR(1:I_LEN(STR)-5)
                CALL WALL_TIMER ( %VAL(0) )
           END IF
           IF ( IVRB .GE. 2 ) THEN
                WRITE ( 6, '(A)' ) 'Finished computation of 3D B-splines' 
           END IF
         ELSE
!
! -------- Compute coefficients of the 3D smoothing spline
!          ===============================================
!
! -------- Allocate temporary arrays
!
           CALL WALL_TIMER ( %VAL(0) )
           ALLOCATE ( RES_TMP(1-SPD__MDEG:SPD%NLEV,1-SPD__MDEG:SPD%NLON,L_LAT,M_TYP), &
     &                STAT=IER )
           IF ( IER .NE. 0 ) THEN
                CALL CLRCH  ( STR )
                CALL IINCH8 ( INT8(4)*MEL, STR )
                CALL ERR_LOG ( 5442, IUER, 'SPD_3D_REFRA', 'Failure in an attempt '// &
     &              'to allocate '//STR(1:I_LEN(STR))//' bytes of dynamic memory '// &
     &              'for array SPD%REF_3D' )
                RETURN 
           END IF
!
           DEALLOCATE ( RES_ARR )
           DEALLOCATE ( RES_SPL )
           ALLOCATE   ( RES_ARR(L_LON+2,M_TYP), STAT=IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 5443, IUER, 'SPD_3D_REFRA', 'Failure in an attempt '// &
     &              'to allocate dynamic memory for array RES_ARR' )
                RETURN 
           END IF
!
           ALLOCATE ( RES_SPL(1-SPD__MDEG:SPD%NLON-1,M_TYP), STAT=IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 5444, IUER, 'SPD_3D_REFRA', 'Failure in an attempt '// &
     &              'to allocate dynamic memory for array RES_SPL' )
                RETURN 
           END IF
           IF ( IVRB > 2 ) THEN
                WRITE ( 6, * ) 'SPD_3D_REFRA-4 Size: ', SIZEOF(RES_TMP)/1.0E9, ' Gb '
                WRITE ( 6, * ) 'SPD_3D_REFRA-5 Size: ', SIZEOF(RES_ARR)/1.0E9, ' Gb '
                WRITE ( 6, * ) 'SPD_3D_REFRA-6 Size: ', SIZEOF(RES_SPL)/1.0E9, ' Gb '
           END IF 
!
           DO 4160 J16=1,L_LON+2
              LON_R8(J16) = (J16-1)*PI2/L_LON
 4160      CONTINUE 
           FL_ERROR = .FALSE.
!
! ----- Expansion over longitude
!
#ifndef SERIAL
!$OMP      PARALLEL DO IF ( NTHR > 1 ), DEFAULT(NONE), &
!$OMP&     PRIVATE ( J17, J18, J19, RES_ARR, RES_SPL, CNS_DR2_LON, IER ), &
!$OMP&     SHARED  ( FL_ERROR, M_TYP, L_LON, L_LAT, NTHR, SPD, &
!$OMP&               REF_3D, RES_TMP, LON_R8, CNS_DR2, LAT_GDT, IVRB, IUER ), &
!$OMP&     SCHEDULE ( STATIC, 1 )
#endif
           DO 4170 J17=1,L_LAT
!
! ----------- LAT_GDT -- is geodetic latitude
!
              LAT_GDT = -P2I + (J17-1)*PI__NUM/(L_LAT-1)
              IF ( J17 == 1 .OR. J17 == L_LAT ) THEN
!
! ---------------- Special case near poles to avoid a situation that COS(LAT) is a very
! ---------------- small number greatr than zero
!
                   CNS_DR2_LON = CNS_DR2(2)*(PI2*SPD__REA_WGS84*DCOS(-P2I + 1*PI__NUM/(L_LAT-1))/(SPD%NLON-2))**2
                 ELSE
                   CNS_DR2_LON = CNS_DR2(2)*(PI2*SPD__REA_WGS84*DCOS(LAT_GDT)/(SPD%NLON-2))**2
              END IF
!
              IF ( FL_ERROR ) GOTO 4170
              DO 4180 J18=1-SPD__MDEG,SPD%NLEV-1
                 IF ( FL_ERROR ) GOTO 4170
                 DO 4190 J19=1,L_LON+2
                    RES_ARR(J19,1:M_TYP) = REF_3D(J18,J19,J17,1:M_TYP)
 4190            CONTINUE 
                 CALL ERR_PASS ( IUER, IER ) 
                 CALL EBSPL_LSQ_CNS3_VEC ( L_LON+2, M_TYP, LON_R8(1), RES_ARR, &
     &                      SPD%NLON, SPD__MDEG, SPD%LON(1), RES_SPL, &
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
                 RES_TMP(J18,1-SPD__MDEG:SPD%NLON-1,J17,1:M_TYP) = RES_SPL(1-SPD__MDEG:SPD%NLON-1,1:M_TYP)
 4180         CONTINUE 
              IF ( IVRB .GE. 4 ) THEN
                   WRITE ( 6, 230 ) J17, L_LAT, CHAR(13)
 230               FORMAT ( '  2nd: Ilat= ', I4, ' ( ', I4, ' ) ',A$ )
              END IF
 4170      CONTINUE 
#ifndef SERIAL
!$OMP END PARALLEL DO
#endif
           IF ( FL_ERROR ) THEN
                CALL ERR_LOG ( 5445, IUER, 'SPD_3D_REFRA', 'Error in an attempt '// &
      &                        'compute coefficients of the smoothing B-spline '// &
     &                         'over longitude' )
                RETURN
           END IF
!
           IF ( SPD%CONF%TEST_STR == 'timer' ) THEN
                CALL WALL_TIMER ( STR )
                WRITE ( 6, '(A)' ) 'Computation of 3D B-splines over the 2nd dim:   '//STR(1:I_LEN(STR)-5)
                CALL WALL_TIMER ( %VAL(0) )
           END IF
           IF ( IVRB .GE. 2 ) THEN
                WRITE ( 6, * ) 'Finished processing the  2nd dimension'
           END IF
!
           DEALLOCATE ( RES_ARR )
           DEALLOCATE ( RES_SPL )
!
           ALLOCATE   ( RES_ARR(L_LAT,M_TYP), STAT=IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 5446, IUER, 'SPD_3D_REFRA', 'Failure in an attempt '// &
     &              'to allocate dynamic memory for array RES_ARR' )
                RETURN 
           END IF
!
           ALLOCATE ( RES_SPL(1-SPD__MDEG:SPD%NLAT-1,M_TYP), STAT=IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 5447, IUER, 'SPD_3D_REFRA', 'Failure in an attempt '// &
     &              'to allocate dynamic memory for array RES_SPL' )
                RETURN 
           END IF
!
! -------- Allocate "extended" REF_3D array. NB: it contains B-spline 
! -------- expansions for P, PW, and TEM in the 4th dimension with indices
! -------- IND_P, INT_PW, and IND_TEM
!
           ALLOCATE ( SPD%REF_3D(1-SPD__MDEG:SPD%NLEV,1-SPD__MDEG:SPD%NLON,1-SPD__MDEG:SPD%NLAT,M_TYP), &
     &                STAT=IER )
           IF ( IER .NE. 0 ) THEN
                MEL = INT8(SPD%NLEV+SPD__MDEG)*INT8(SPD%NLON+SPD__MDEG)*INT8(SPD%NLAT+SPD__MDEG)*INT8(M_TYP)
                CALL CLRCH  ( STR )
                CALL IINCH8 ( MEL, STR ) 
                CALL ERR_LOG ( 5448, IUER, 'SPD_3D_REFRA', 'Failure in an attempt '// &
     &              'to allocate '//TRIM(STR)//' bytes of dynamic memory for '// &
     &              'array REF_3D' )
                RETURN 
           END IF
           SPD%REF_3D = -1.0D0
!
           DO 4200 J20=1,L_LAT
              LAT_R8(J20) = -P2I + (J20-1)*PI__NUM/(L_LAT-1)
 4200      CONTINUE 
           FL_ERROR = .FALSE.
!
! ----- Expansion over latitude
!
#ifndef SERIAL
!$OMP      PARALLEL DO IF ( NTHR > 1 ), DEFAULT(NONE), &
!$OMP&     PRIVATE ( J21, J22, J23, RES_ARR, RES_SPL, IER ), &
!$OMP&     SHARED  ( FL_ERROR, M_TYP, L_LON, L_LAT, NTHR, SPD, &
!$OMP&               RES_TMP, LAT_R8, CNS_DR2, IVRB, IUER ), &
!$OMP&     SCHEDULE ( STATIC, 1 )
#endif
           DO 4210 J21=1-SPD__MDEG,SPD%NLEV-1
              IF ( FL_ERROR ) GOTO 4210
              DO 4220 J22=1-SPD__MDEG,SPD%NLON-1
                 IF ( FL_ERROR ) GOTO 4210
                 DO 4230 J23=1,L_LAT
                    RES_ARR(J23,1:M_TYP) = RES_TMP(J21,J22,J23,1:M_TYP)
 4230            CONTINUE 
                 CALL ERR_PASS ( IUER, IER )
                 CALL EBSPL_LSQ_CNS3_VEC ( L_LAT, M_TYP, LAT_R8(1), RES_ARR, &
     &                      SPD%NLAT, SPD__MDEG, SPD%LAT(1), RES_SPL, &
     &                      0.0D0, 0.0D0, CNS_DR2(2)*(PI__NUM*SPD__REA_WGS84/(SPD%NLAT-1))**2, IER )
                 IF ( IER .NE. 0 ) THEN
#ifndef               SERIAL
!$OMP                 CRITICAL
#endif
                      FL_ERROR = .TRUE.
#ifndef SERIAL
!$OMP                 END CRITICAL
#endif
                 END IF
                 SPD%REF_3D(J21,J22,1-SPD__MDEG:SPD%NLAT-1,1:M_TYP) = RES_SPL(1-SPD__MDEG:SPD%NLAT-1,1:M_TYP)
 4220         CONTINUE 
              IF ( IVRB .GE. 4 ) THEN
                   WRITE ( 6, 240 ) J21, SPD%NLEV-1, CHAR(13)
 240               FORMAT ( '  3rd: Ilev= ', I4, ' ( ', I4, ' ) ',A$ )
              END IF
 4210      CONTINUE 
#ifndef SERIAL
!$OMP END PARALLEL DO
#endif
           IF ( FL_ERROR ) THEN
                CALL ERR_LOG ( 5449, IUER, 'SPD_3D_REFRA', 'Error in an attempt '// &
     &                        'compute coefficients of the smoothing B-spline '// &
     &                        'over longitude' )
               RETURN
           END IF
           IF ( IVRB .GE. 2 ) THEN
                WRITE ( 6, * ) 'Finished processing the  3rd dimension'
           END IF
           IF ( FL_PWT ) THEN
                MEL = INT8(SPD%NLEV+SPD__MDEG)*INT8(SPD%NLON+SPD__MDEG)*INT8(SPD%NLAT+SPD__MDEG)
                CALL MEMCPY ( SPD%SPR_3D, SPD%REF_3D(1-SPD__MDEG,1-SPD__MDEG,1-SPD__MDEG,IND_P), %VAL(INT8(8)*MEL) )
                CALL MEMCPY ( SPD%SPW_3D, SPD%REF_3D(1-SPD__MDEG,1-SPD__MDEG,1-SPD__MDEG,IND_PW), %VAL(INT8(8)*MEL) )
                CALL MEMCPY ( SPD%STM_3D, SPD%REF_3D(1-SPD__MDEG,1-SPD__MDEG,1-SPD__MDEG,IND_TEM), %VAL(INT8(8)*MEL) )
           END IF
           IF ( SPD%CONF%TEST_STR == 'timer' ) THEN
                CALL WALL_TIMER ( STR )
                WRITE ( 6, '(A)' ) 'Computation of 3D B-splines over the 3rd dim:   '//STR(1:I_LEN(STR)-5)
                CALL WALL_TIMER ( %VAL(0) )
           END IF
      END IF
!
      DO 4240 J24=1-SPD__MDEG,0
         SPD%LEV(J24) = SPD%LEV(1)
         SPD%LON(J24) = SPD%LON(1)
         SPD%LAT(J24) = SPD%LAT(1)
 4240 CONTINUE 
      SPD%REF_3D_STATUS = SPD__COMP
#ifndef SERIAL
      CALL OMP_SET_NUM_THREADS ( %VAL(NTHR_SAVED) )
#endif
      IF ( FL_ATT_INTR .AND. SPD%NFRQ > 0 ) THEN
           DEALLOCATE ( BSPL_ATT )
      END IF
      DEALLOCATE ( LEV_R8  )
      DEALLOCATE ( RES_ARR )
      DEALLOCATE ( RES_SPL )
      IF ( DIMO(1) .NE. 0 ) THEN
           DEALLOCATE ( RES_TMP )
      END IF
      IF ( IVRB .GE. 3 ) THEN
           WRITE ( 6, * ) 'Finished SPD_3D_REFRA' 
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  SPD_3D_REFRA  !#!#
