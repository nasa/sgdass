      SUBROUTINE MALO_COMP_ACP ( MODE, HEI_ABOVE_SURFACE, HEB_DELP, HEB_T, &
     &                           HEB_Q, HEB_G, HEB_OH, HEB_ACP, &
     &                           HEB_ST, HEB_RH, COO_HLP, ACP_VAL, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  MALO_COMP_ACP  computes atmospheric chemical potential    *
! *   and temperature at the heihgt above the surface HEI_ABOVE_SURFACE  *
! *   for either a station with specified coordinates COO_HLP or for     *
! *   a regular 2D lat/long grid.                                        *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *     MODE ( INTEGER*4 ) -- Output mode.                               *
! *                           If MALO__ACP_GRID then atmospheric         *
! *                              chemical potential and surface          *
! *                              temperature are computed on a global    *
! *                              grid with dimension defined in          *
! *                              HEB_OH.                                 *
! *                           If MALO__ACP_STA  then these parameters    *
! *                              are compued for station with            *
! *                              coordinates COO_HLP.                    *
! * HEI_ABOVE_SURFACE ( REAL*8   ) -- Height above the surface for       *
! *                                   computation of atmospheric         *
! *                                   chemical potential.                *
! * HEB_DELP ( HEB__TYPE ) -- HEB data structure that keeps the field of *
! *                           pressure thickness.                        *
! *                           Dimension: NTIM. One element corresponds   *
! *                           to one time epoch.                         *
! * HEB_T    ( HEB__TYPE ) -- HEB data structure that keeps the field of *
! *                           air temperature.                           *
! * HEB_Q    ( HEB__TYPE ) -- HEB data structure that keeps the field of *
! *                           speficic himdity.                          *
! * HEB_G    ( HEB__TYPE ) -- HEB data structure that keeps geopotntial  *
! *                           at the nominal surface used by the numeric *
! *                           weather model.                             *
! * HEB_OH   ( HEB__TYPE ) -- HEB data strcuture that keeps the global   *
! *                           array heights above the geoid of the       *
! *                           surface.                                   *
! *                                                                      *
! * ________________________ Output parameters: ________________________ *
! *                                                                      *
! * HEB_ACP  ( HEB__TYPE ) -- HEB data structure that keeps the global   *
! *                           field of atmospheric chemical potential.   *
! *                           Not used if MODE == MALO__ACP_STA.         *
! * HEB_ST   ( HEB__TYPE ) -- HEB data structure that keeps the global   *
! *                           field of air temperature.                  *
! *                           Not used if MODE == MALO__ACP_STA.         *
! * HEB_RH   ( HEB__TYPE ) -- HEB data structure that keeps the global   *
! *                           field of relative humidity.                *
! *                           Not used if MODE == MALO__ACP_STA.         *
! * COO_HLP  ( REAL*8    ) -- Coordinates of a station: Height above     *
! *                           the geoid, Longitude, Geodetic Latitude.   *
! *                           Not used if MODE == MALO__ACP_GRID.        *
! * ACP_HLP  ( REAL*8    ) -- Atmospheric hcemical potential for the     *
! *                           station of interest.                       *
! *                           Not used if MODE == MALO__ACP_GRID.        *
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
! * ### 13-MAR-2013  MALO_COMP_ACP  v3.0(c)   L. Petrov 28-NOV-2017  ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'malo.i'
      INCLUDE   'heb.i'
      REAL*8     HEI_ABOVE_SURFACE
      TYPE     ( HEB__TYPE  ) :: HEB_DELP, HEB_T, HEB_Q, HEB_G, HEB_OH, &
     &                           HEB_ACP, HEB_ST, HEB_RH
      INTEGER*4  MODE, IUER
      REAL*4,    ALLOCATABLE  :: TT_3D(:,:,:), RH_3D(:,:,:), ACP_3D(:,:,:), &
     &           LAT_R4(:), LON_R4(:)
      REAL*8     P(MALO__MHEI), PD(MALO__MHEI), PW(MALO__MHEI), &
     &           T(MALO__MHEI), FUN1(MALO__MHEI), FUN2(MALO__MHEI), &
     &           TMP(MALO__MHEI), H(MALO__MHEI), &
     &           GE, G, PHI, ZM(MALO__MHEI), &
     &           T_RATE, T_NLEV, LPL, LPH, PRES_R8(MALO__MHEI), &
     &           HEI_R8(MALO__MHEI), TPD_VAL, PRES_VAL, TEMP, LOG_PRES, &
     &           Q(MALO__MHEI), LOGQ(MALO__MHEI), TPD_1D(MALO__MHEI), &
     &           TT_1D(MALO__MHEI), SPL_H(MALO__MHEI), PW0, &
     &           SPL_TT(MALO__MHEI), SPL_TD(MALO__MHEI), PRES_LEV, PW_LEV, &
     &           COO_HLP(3), ACP_VAL
      LOGICAL*1  FL_ERROR
      REAL*4     VAL_MIN, VAL_MAX
      REAL*8     SPD__COMP_A0, SPD__COMP_A1, SPD__COMP_A2, SPD__COMP_B0, SPD__COMP_B1, &
     &           SPD__COMP_C0, SPD__COMP_C1, SPD__COMP_D0, SPD__COMP_E0
!
      PARAMETER  ( SPD__COMP_A0 =  1.58123D-6   ) ! CIPM-2007
      PARAMETER  ( SPD__COMP_A1 = -2.9331D-8    ) ! CIPM-2007
      PARAMETER  ( SPD__COMP_A2 =  1.1043D-10   ) ! CIPM-2007
      PARAMETER  ( SPD__COMP_B0 =  5.707D-6     ) ! CIPM-2007
      PARAMETER  ( SPD__COMP_B1 = -2.051D-8     ) ! CIPM-2007
      PARAMETER  ( SPD__COMP_C0 =  1.9898D-4    ) ! CIPM-2007
      PARAMETER  ( SPD__COMP_C1 = -2.376D-6     ) ! CIPM-2007
      PARAMETER  ( SPD__COMP_D0 =  1.83D-11     ) ! CIPM-2007
      PARAMETER  ( SPD__COMP_E0 = -0.765D-8     ) ! CIPM-2007
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
!
      REAL*4     HEI_R4(MALO__MHEI), LAT_VAL, LON_VAL, ARGS(3)
      REAL*8     EPS, PRES_HIGH, HEI_MIN, HEI_MIN_INT, HEI_MAX, PRES_LN_0, PRES_LN_RATE
      PARAMETER  ( PRES_HIGH    = 23000.0D0 )
      PARAMETER  ( HEI_MIN      =  -500.0D0 )
      PARAMETER  ( HEI_MIN_INT  = -1000.0D0 )
      PARAMETER  ( HEI_MAX      =  9000.0D0 )
      PARAMETER  ( PRES_LN_0    = 11.5476D0 )
      PARAMETER  ( PRES_LN_RATE = -1.323D-4 )
      PARAMETER  ( EPS = 1.D-5 )
      INTEGER*4    M_LEV
      REAL*8       MD, MW
      PARAMETER  ( M_LEV = 64        )
      PARAMETER  ( MD    = MA__MAPL  )
      PARAMETER  ( MW    = H2O__MAPL )
      CHARACTER  STR*128, TEST_STR*8
      INTEGER*4  L_LON, L_LAT, L_LEV, M_LON, M_LAT, IND, &
     &           K_LEV, N_LEV, I_LON, DIMS(3), &
     &           J1, J2, J3, J4, J5, J6, J7, J8, J9, J10, J11, J12, &
     &           INDS(3), IT_LON, IT_LAT, NTHR, NTHR_SAVED, IER
      REAL*4,    ALLOCATABLE  :: TPD_LEV(:,:), TT_LEV(:,:)
      REAL*8     HARR(2), TARR(2), MIN_VAL, MAX_VAL , LON_TST, LAT_TST, &
     &           E, ES, RH, RH0, TD0, RH1, TD1
      REAL*8     TEMP_K, A1_DEW, B1_DEW, C1_DEW, EPS_PW, ACP_MIN, RH_MIN, RH_MAX
      PARAMETER  ( TEMP_K = 273.16D0 )
      PARAMETER  ( A1_DEW = 17.625D0 )
      PARAMETER  ( B1_DEW = 243.04D0 )
      PARAMETER  ( C1_DEW = 610.94D0 )
      PARAMETER  ( EPS_PW = 1.D-10   )
      PARAMETER  ( ACP_MIN = 0.00001 )
      PARAMETER  ( RH_MIN  = 0.0001  )
      PARAMETER  ( RH_MAX  = 1.0000  )
      LOGICAL*4, EXTERNAL :: OMP_IN_PARALLEL
      INTEGER*4, EXTERNAL :: I_LEN, ILEN, IXMN8, IXMN4, IXMN4_S, &
     &                       OMP_GET_NUM_THREADS, OMP_GET_MAX_THREADS
      LOGICAL*4, EXTERNAL :: IS_R4_NAN
      REAL*4,    EXTERNAL :: VAL_3D_BSPL4 
      REAL*8,    EXTERNAL :: ISPL8, FSPL8
#ifndef SERIAL
!
! --- Set the number of threads
!
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
      TEST_STR = 'none'  ! Supported values: none, timer, diff, pw, pres, temp, intrp, dewt
      LON_TST  = 270.0*DEG__TO__RAD
      LAT_TST  =   0.0*DEG__TO__RAD
!
! --- Grid of the input numerical models
!
      L_LON = HEB_DELP%DIMS(1)
      L_LAT = HEB_DELP%DIMS(2)
      L_LEV = HEB_DELP%DIMS(3)
!
      IT_LON = 1 + L_LON*(LON_TST/PI2)
      IF ( IT_LON > L_LON ) IT_LON = IT_LON - L_LON
      IT_LAT = 1 + (L_LAT-1)*((LAT_TST+P2I)/PI__NUM)
!
! --- Allocate memory for the output dew point temperature array
!
      HEB_ACP%DIMS = HEB_OH%DIMS
      M_LON = HEB_OH%DIMS(1)
      M_LAT = HEB_OH%DIMS(2)
      IF ( ASSOCIATED ( HEB_ACP%VAL ) ) DEALLOCATE ( HEB_ACP%VAL )
      ALLOCATE ( HEB_ACP%VAL(HEB_ACP%DIMS(1),HEB_ACP%DIMS(2),HEB_ACP%DIMS(3),HEB_ACP%DIMS(4)), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH  ( STR )
           CALL IINCH8 ( INT8(4)*HEB_ACP%DIMS(1),HEB_ACP%DIMS(2),HEB_ACP%DIMS(3),HEB_ACP%DIMS(4), STR )
           CALL ERR_LOG ( 4711, IUER, 'MALO_COMP_ACP', 'Failure in an attempt '// &
     &         'to allocate '//STR(1:I_LEN(STR))//' bytes of dynamic memory '// &
     &         'for array MAL%SPR' )
           RETURN 
      END IF
!
! --- Allocate memory for the output air temperature array
!
      HEB_ST%DIMS = HEB_OH%DIMS
      M_LON = HEB_OH%DIMS(1)
      M_LAT = HEB_OH%DIMS(2)
      IF ( ASSOCIATED ( HEB_ST%VAL ) ) DEALLOCATE ( HEB_ST%VAL )
      ALLOCATE ( HEB_ST%VAL(HEB_ST%DIMS(1),HEB_ST%DIMS(2),HEB_ACP%DIMS(3),HEB_ST%DIMS(4)), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH  ( STR )
           CALL IINCH8 ( INT8(4)*HEB_ST%DIMS(1),HEB_ST%DIMS(2),HEB_ST%DIMS(3),HEB_ST%DIMS(4), STR )
           CALL ERR_LOG ( 4712, IUER, 'MALO_COMP_ACP', 'Failure in an attempt '// &
     &         'to allocate '//STR(1:I_LEN(STR))//' bytes of dynamic memory '// &
     &         'for array MAL%SPR' )
           RETURN 
      END IF
!
! --- Allocate memory for the output relative humidity array
!
      HEB_RH%DIMS = HEB_OH%DIMS
      M_LON = HEB_OH%DIMS(1)
      M_LAT = HEB_OH%DIMS(2)
      IF ( ASSOCIATED ( HEB_RH%VAL ) ) DEALLOCATE ( HEB_RH%VAL )
      ALLOCATE ( HEB_RH%VAL(HEB_RH%DIMS(1),HEB_RH%DIMS(2),HEB_ACP%DIMS(3),HEB_RH%DIMS(4)), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH  ( STR )
           CALL IINCH8 ( INT8(4)*HEB_RH%DIMS(1),HEB_RH%DIMS(2),HEB_RH%DIMS(3),HEB_RH%DIMS(4), STR )
           CALL ERR_LOG ( 4713, IUER, 'MALO_COMP_ACP', 'Failure in an attempt '// &
     &         'to allocate '//STR(1:I_LEN(STR))//' bytes of dynamic memory '// &
     &         'for array MAL%SPR' )
           RETURN 
      END IF
!
      IF ( TEST_STR == 'timer' ) THEN
           WRITE ( 6, '(A,I6,1X,I6)' ) 'NLON/NLAT = ', HEB_RH%DIMS(1), HEB_RH%DIMS(2)
           CALL WALL_TIMER ( %VAL(0) ) 
      END IF
!
! --- Create an initial array of height for mid-level pressure thickness
!
      LPL = PRES_LN_0 + PRES_LN_RATE*HEI_MIN
      LPH = PRES_LN_0 + PRES_LN_RATE*HEI_MAX
      DO 410 J1=1,M_LEV
         LOG_PRES = LPL + (J1-1)*(LPH - LPL)/(M_LEV-1)
         HEI_R8(J1)  = -PRES_LN_0/PRES_LN_RATE + LOG_PRES/PRES_LN_RATE
         HEI_R4(J1)  = HEI_R8(J1)
 410  CONTINUE 
!
! --- Allocate memory for the 3D atmosperic chemical potential
!
      ALLOCATE ( ACP_3D(1-MALO__MDEG:M_LEV,1-MALO__MDEG:L_LON+1,1-MALO__MDEG:L_LAT), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( 4*(M_LEV+MALO__MDEG)*(L_LON+1+MALO__MDEG)*(L_LAT+MALO__MDEG), STR )
           CALL ERR_LOG ( 4714, IUER, 'MALO_COMP_ACP', 'Failure in an attempt '// &
     &         'to allocate '//STR(1:I_LEN(STR))//' bytes of dynamic memory '// &
     &         'for array ACP_3D' )
           RETURN 
      END IF
!
! --- Allocate memory for the 3D air temperature array
!
      ALLOCATE ( TT_3D(1-MALO__MDEG:M_LEV,1-MALO__MDEG:L_LON+1,1-MALO__MDEG:L_LAT), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( 4*(M_LEV+MALO__MDEG)*(L_LON+1+MALO__MDEG)*(L_LAT+MALO__MDEG), STR )
           CALL ERR_LOG ( 4715, IUER, 'MALO_COMP_ACP', 'Failure in an attempt '// &
     &         'to allocate '//STR(1:I_LEN(STR))//' bytes of dynamic memory '// &
     &         'for array TT_3D' )
           RETURN 
      END IF
!
! --- Allocate memory for the 3D air temperature array
!
      ALLOCATE ( RH_3D(1-MALO__MDEG:M_LEV,1-MALO__MDEG:L_LON+1,1-MALO__MDEG:L_LAT), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( 4*(M_LEV+MALO__MDEG)*(L_LON+1+MALO__MDEG)*(L_LAT+MALO__MDEG), STR )
           CALL ERR_LOG ( 4716, IUER, 'MALO_COMP_ACP', 'Failure in an attempt '// &
     &         'to allocate '//STR(1:I_LEN(STR))//' bytes of dynamic memory '// &
     &         'for array RH_3D' )
           RETURN 
      END IF
!
      IF ( TEST_STR == 'diff' .OR. TEST_STR == 'intrp' .OR. TEST_STR == 'gen' ) THEN
           ALLOCATE ( TPD_LEV(L_LON,L_LAT) )
      END IF
!
! --- Allocate mmeory for longitude and latitude arrays
!
      ALLOCATE ( LAT_R4(L_LAT), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( 4*L_LAT, STR )
           CALL ERR_LOG ( 4717, IUER, 'MALO_COMP_ACP', 'Failure in '// &
     &         'an attempt to allocate '//STR(1:I_LEN(STR))// &
     &         ' bytes of dynamic memory for array LAT_R4' )
           RETURN 
      END IF
!
      ALLOCATE ( LON_R4(L_LON+1), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( 4*L_LON, STR )
           CALL ERR_LOG ( 4718, IUER, 'MALO_COMP_ACP', 'Failure in '// &
     &         'an attempt to allocate '//STR(1:I_LEN(STR))// &
     &         ' bytes of dynamic memory for array LON_R4' )
           RETURN 
      END IF
!
      ACP_3D = 0.0
      LAT_R4 = 0.0
      LON_R4 = 0.0
!
! === Now start processing input data
!
      DO 420 J2=1,L_LAT
!
! ------ PHI -- is geodetic latitude
!
         PHI = -P2I + (J2-1)*PI__NUM/(L_LAT-1)
!
! ------ Compute gravity on the reference ellipsoid for a latitude
!
         GE = ACC_EQU__WGS84*(1.D0 + GRV_LAT__WGS84*DSIN(PHI)**2)/ &
     &          DSQRT(1.D0 - (2.D0*FLAT__WGS84 - FLAT__WGS84**2)*DSIN(PHI)**2 )
         FL_ERROR = .FALSE.
#ifndef SERIAL
!$OMP    PARALLEL DO IF ( NTHR > 1 ), &
!$OMP&   PRIVATE ( J3, J4, J5, J6, J7, J8, IER, G, I_LON, P, T, Q, &
!$OMP&             H, PW, PD, ZM, SPL_H, SPL_TD, SPL_TT, FUN1, FUN2, TMP, LOGQ, &
!$OMP&             HEI_R4, T_RATE, T_NLEV, TT_1D, TPD_1D, RH, RH1, TPD_LEV, &
!$OMP&             PRES_LEV, PW_LEV, TPD_VAL, K_LEV, N_LEV, IND ), &
!$OMP&             SCHEDULE ( STATIC )
#endif
         DO 430 J3=1,L_LON
            IF ( FL_ERROR ) GOTO 430
!
! --------- NB: Merra/Geos57 longitude starts from -180deg
!
            I_LON = J3 + L_LON/2
            IF ( I_LON > L_LON ) I_LON = I_LON - L_LON
!
! --------- First run for computing height above the ellipsoid for a given pressure level
!
            P(L_LEV+1) = 0.0D0
            N_LEV = L_LEV
!
! --------- Correct specific humidity ofr voids
!
            CALL ERR_PASS ( IUER, IER )
            CALL CORRECT_SPEC_HUM ( L_LEV, HEB_Q%VAL(J3,J2,1:L_LEV,1), IER )
            IF ( IER .NE. 0 ) THEN
#ifndef SERIAL
!$OMP            CRITICAL
#endif
                 WRITE ( 6, * ) 'J3= ', J3, ' J2= ', J2
                 CALL ERR_LOG ( 4719, IUER, 'MALO_COMP_ACP', 'Error '// &
     &               'in an attempt to correct specific humidity' )
                 FL_ERROR = .TRUE.
#ifndef SERIAL
!$OMP            END CRITICAL
#endif
                 GOTO 430
            END IF
!
! --------- First run over the layers
!
            DO 440 J4=1,L_LEV
               K_LEV = L_LEV+1-J4
!
! ------------ Compute the atmospheric pressure to the mid layer point
!
               IF ( J4 == 1 ) THEN
                    P(K_LEV) = P(K_LEV+1) + HEB_DELP%VAL(J3,J2,J4,1)/2.0D0
                  ELSE 
                    P(K_LEV) = P(K_LEV+1) + HEB_DELP%VAL(J3,J2,J4-1,1)/2.0D0 + &
     &                                      HEB_DELP%VAL(J3,J2,J4,1)/2.0D0 
               END IF
               IF ( P(K_LEV) < PRES_HIGH ) N_LEV = K_LEV
!
! ------------ Compute approximate gravity acceleration at a given pressure level
! ------------ using a regression model
!
               G = GE*(1.D0 + GP0 + GP1*DLOG(P(K_LEV)))
!
! ------------ PW -- parital pressure of water vapour
!
               PW(K_LEV)   = HEB_Q%VAL(J3,J2,J4,1)*P(K_LEV)/ &
     &                      (MW/MD + (1.0D0 - MW/MD)*HEB_Q%VAL(J3,J2,J4,1))
!
! ------------ Partial pressure of dry air
!
               PD(K_LEV)   = P(K_LEV) - PW(K_LEV)
!
! ------------ store air temperature and specific humidity
!
               T(K_LEV)    = HEB_T%VAL(J3,J2,J4,1)
               Q(K_LEV)    = HEB_Q%VAL(J3,J2,J4,1)
!
! ------------ Compute water vapor compressibility
!
               ZM(K_LEV) = 1.D0/ &
     &           ( 1.0D0 &
     &             - P(K_LEV)/T(K_LEV)*  (SPD__COMP_A0 + SPD__COMP_A1*(T(K_LEV) + SPD__ABS_TEMP) +  &
     &                                                   SPD__COMP_A2*(T(K_LEV) + SPD__ABS_TEMP)**2 ) &
     &             + PW(K_LEV)/T(K_LEV)* (SPD__COMP_B0 + SPD__COMP_B1*(T(K_LEV) + SPD__ABS_TEMP)) &
     &             + PW(K_LEV)**2/(P(K_LEV)*T(K_LEV))* &
     &                                   (SPD__COMP_C0 + SPD__COMP_C1*(T(K_LEV) + SPD__ABS_TEMP)) &
     &             + P(K_LEV)**2/T(K_LEV)**2*  SPD__COMP_D0 &
     &             + PW(K_LEV)**2/T(K_LEV)**2* SPD__COMP_E0 &
     &           )
!
! ------------ Compute function HYPS that is the right hand-side of 
! ------------ the hypsometric equation, while the left hand side is h(P)
!
               FUN1(K_LEV) = R__MAPL*T(K_LEV)/ &
     &                      (G*(MD*PD(K_LEV) + MW*PW(K_LEV))*ZM(K_LEV))
 440        CONTINUE 
!
! --------- Special trick for making interpolation: we reverse the sign of pressure.
! --------- Otherwise, MAKE_SPLINE will complain the argument is not in rising order
!
            P = -P ! For interpolation
!
! --------- Compute the spline interpolation polynok for FUN1 as a function of pressure
!
            CALL ERR_PASS ( IUER, IER ) 
            CALL MAKE_SPLINE ( 3, L_LEV, P, FUN1, 0.0D0, 0.0D0, SPL_H, TMP, IER )
            IF ( IER .NE. 0 ) THEN
#ifndef SERIAL
!$OMP           CRITICAL
#endif
                 CALL ERR_LOG ( 4720, IUER, 'MALO_COMP_ACP', 'Failure in an attempt '// &
     &                'to compute coefficients of the interpolating spline' )
                 FL_ERROR = .TRUE.
#ifndef SERIAL
!$OMP           END CRITICAL
#endif
                GOTO 430
            END IF
!
! --------- Compute height at mid pressure layers by integrating the spline
!
            H(1) = HEB_G%VAL(J3,J2,1,1) ! Boundary condition at the surface
            DO 450 J5=2,L_LEV
               IF ( J5 < L_LEV ) THEN
                    H(J5) = H(J5-1) + ISPL8 ( P(J5), L_LEV, P, FUN1, &
     &                                        J5-1, J5, SPL_H, IER )
                  ELSE IF ( J5 == L_LEV ) THEN
                    H(J5) = H(J5-1) + ISPL8 ( P(J5)*(1.D0-EPS), L_LEV, P, FUN1, &
     &                                        J5-1, J5, SPL_H, IER )
               END IF
 450        CONTINUE 
!
! --------- Compute the lapse rate between heights N_LEV/2 and N_LEV
!
            CALL ERR_PASS ( IUER, IER )
            CALL REGR8 ( N_LEV - N_LEV/2 + 1, H(N_LEV/2), T(N_LEV/2), T_RATE, T_NLEV, IER )
            IF ( TEST_STR == 'temp'  .AND.  I_LON == IT_LON  .AND.  J2 == IT_LAT ) THEN
                 HARR(1) = H(1)
                 HARR(2) = H(N_LEV)
                 TARR(1) = T_NLEV + T_RATE*(H(1) - H(N_LEV/2))
                 TARR(2) = T_NLEV + T_RATE*(H(N_LEV) - H(N_LEV/2))
                 WRITE ( 6, * ) 'Test: Lat= ', SNGL((-P2I + (IT_LAT-1)*PI__NUM/(L_LAT-1))/DEG__TO__RAD), &
     &                          'Lon= ', SNGL((IT_LON-1)*PI2/L_LON/DEG__TO__RAD), &
     &                          ' IT_LON= ', IT_LON, ' IT_LAT= ', IT_LAT, &
     &                          ' J2= ', J2, ' J3= ', J3
                 CALL DIAGI_2 ( N_LEV+1, H, T, 2, HARR, TARR, IER )
            END IF
!
! --------- Second round of solving hte hyspometric equation
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
               FUN2(J6) = R__MAPL*T(J6)/(G*(MD*PD(J6) + MW*PW(J6))*ZM(J6))
 460        CONTINUE 
!
! --------- Compute interpolating spline of H(P) the second time
!
            CALL ERR_PASS ( IUER, IER ) 
            CALL MAKE_SPLINE ( 3, L_LEV, P, FUN2, 0.0D0, 0.0D0, SPL_H, TMP, IER )
            IF ( IER .NE. 0 ) THEN
#ifndef SERIAL
!$OMP           CRITICAL
#endif
                 CALL ERR_LOG ( 4721, IUER, 'MALO_COMP_ACP', 'Failure in '// &
     &               'an attempt to compute coefficients of the '// &
     &               'interpolating spline' )
                FL_ERROR = .TRUE.
#ifndef SERIAL
!$OMP           END CRITICAL
#endif
                GOTO 430
            END IF
!
! --------- Compute height as a function of pressure the second time.
!
            LOGQ(1) = DLOG(Q(1))
            TT_1D(1) = T(1)
            TPD_1D(1) = TEMP_K + B1_DEW*DLOG((PW(1)+EPS_PW)/C1_DEW)/ &
     &                          (A1_DEW - DLOG((PW(1)+EPS_PW)/C1_DEW))
!
! --------- Compute relative humidity at the surface
!
            RH1 = DEXP ( LV__MALO/RV__MALO*(1.D0/TT_1D(1) - 1.D0/TPD_1D(1)) )
!
! --------- Compute for mid-layers ogarithm of specific humidity and 
! --------- dew point temperature
!
            DO 470 J7=2,L_LEV
               IF ( J7 == L_LEV ) THEN
                    H(J7) = H(J7-1) + ISPL8 ( P(J7)*(1.D0-EPS), L_LEV, P, FUN1, &
     &                                        J7-1, J7, SPL_H, IER )
                  ELSE IF ( J7 < L_LEV ) THEN
                    H(J7) = H(J7-1) + ISPL8 ( P(J7), L_LEV, P, FUN1, &
     &                                        J7-1, J7, SPL_H, IER )
               END IF
               LOGQ(J7) = LOG(Q(J7))
               TPD_1D(J7) = TEMP_K + B1_DEW*DLOG((PW(J7)+EPS_PW)/C1_DEW)/ &
     &                              (A1_DEW - DLOG((PW(J7)+EPS_PW)/C1_DEW))
               TT_1D(J7) = T(J7)
               IF ( TEST_STR == 'temp'  .AND.  I_LON == IT_LON  .AND.  J2 == IT_LAT ) THEN
                    WRITE ( 6, * ) 'J7= ', INT2(J7), ' H= ', SNGL(H(J7)), ' PW= ', SNGL(PW(J7))
               END IF 
 470        CONTINUE 
!
            IF ( TEST_STR == 'dewt'  .AND.  I_LON == IT_LON  .AND.  J2 == IT_LAT ) THEN
                 WRITE ( 6, * ) 'Test1: Lat= ', SNGL((-P2I + (IT_LAT-1)*PI__NUM/(L_LAT-1))/DEG__TO__RAD), &
     &                          'Lon= ', SNGL((IT_LON-1)*PI2/L_LON/DEG__TO__RAD)
                 CALL DIAGI_1 ( N_LEV+1, H, LOGQ, -2 ) 
                 CALL DIAGI_1 ( N_LEV+1, H, PW, -2 ) 
                 CALL DIAGI_1 ( N_LEV+1, H, TPD_1D, -2 ) 
            END IF
!
! --------- Now we compute interpolation spline of dew point temperature as 
! --------- a function of height
!
            CALL ERR_PASS ( IUER, IER ) 
            CALL MAKE_SPLINE ( 3, L_LEV, H, TPD_1D, 0.0D0, 0.0D0, &
     &                         SPL_TD, TMP, IER )
            IF ( IER .NE. 0 ) THEN
#ifndef SERIAL
!$OMP           CRITICAL
#endif
                 WRITE ( 6, * ) 'J3(lon) = ', INT2(J3), ' J2(lat) = ', INT2(J2)
                 WRITE ( 6, * ) 'L_LEV= ', L_LEV
                 WRITE ( 6, * ) 'H = ', SNGL(H(1:L_LEV))
                 WRITE ( 6, * ) 'Q = ', HEB_Q%VAL(J3,J2,1:L_LEV,1)
                 CALL ERR_LOG ( 4722, IUER, 'MALO_COMP_ACP', 'Failure in an attempt '// &
     &                'to compute coefficients of the interpolating spline' )
#ifndef SERIAL
!$OMP           END CRITICAL
#endif
                GOTO 430
            END IF
!
! --------- Now we compute interpolation spline of air temperature as 
! --------- a function of height
!
            CALL ERR_PASS ( IUER, IER ) 
            CALL MAKE_SPLINE ( 3, L_LEV, H, TT_1D, 0.0D0, 0.0D0, &
     &                         SPL_TT, TMP, IER )
            IF ( IER .NE. 0 ) THEN
#ifndef SERIAL
!$OMP            CRITICAL
#endif
                 WRITE ( 6, * ) 'J3(lon) = ', INT2(J3), ' J2(lat) = ', INT2(J2)
                 WRITE ( 6, * ) 'L_LEV= ', L_LEV
                 WRITE ( 6, * ) 'H = ', SNGL(H(1:L_LEV))
                 WRITE ( 6, * ) 'Q = ', HEB_Q%VAL(J3,J2,1:L_LEV,1)
                 CALL ERR_LOG ( 4723, IUER, 'MALO_COMP_ACP', 'Failure in an attempt '// &
     &                'to compute coefficients of the interpolating spline' )
#ifndef SERIAL
!$OMP            END CRITICAL
#endif
                GOTO 430
            END IF
!
! --------- Now we compute dew point temperature, relative humidity, and 
! --------- atmospheric chemical potential at the output regular grid
!
            DO 480 J8=1,M_LEV
               G = GE*(1.D0 - 2.D0/REA__WGS84*(1.0D0 + OMEGA__EGM96**2*REA__WGS84**2* &
     &                (1.D0 - FLAT__WGS84)/GM__EGM96 + (1.D0 - 2.D0*FLAT__WGS84)* &
     &                 DSIN(PHI)**2)*HEI_R8(J8) + 3.D0/REA__WGS84**2*HEI_R8(J8)**2)
               IF ( HEI_R8(J8) < H(1) ) THEN
!
! ----------------- This height level is below the surface. We assume the atmosphere
! ----------------- beneath the surface objeys to the adiabatic gas law with a constant 
! ----------------- lapse rate computed at the previous step
!
                    TT_3D(J8,I_LON,J2) = T(1) + T_RATE*(HEI_R8(J8) - H(1))
                    PRES_LEV = -P(1)*(TT_3D(J8,I_LON,J2)/T(1))**(-G*MD/(R__MAPL*T_RATE))
                    PW_LEV   = PW(1)*(TT_3D(J8,I_LON,J2)/T(1))**(-G*MW/(R__MAPL*T_RATE))
!
                    TPD_VAL  = TEMP_K + B1_DEW*DLOG((PW_LEV+EPS_PW)/C1_DEW)/ &
     &                                 (A1_DEW - DLOG((PW_LEV+EPS_PW)/C1_DEW))
                    IF ( TEST_STR == 'temp'  .AND.  I_LON == IT_LON  .AND.  J2 == IT_LAT ) THEN
                         write ( 6, * ) ' j8= ', j8, ' temp = ',temp, ' pres_lev= ', pres_lev, ' pw_lev= ', pw_lev, &
     &                                  ' je= ', hei_r8(j8), ' t3 =', TPD_VAL
                    END IF 
                    IND = -123
!
! ----------------- Fix relative humdity to the value at the 1st level
!
                    RH = RH1
                    IF ( RH > RH_MAX ) RH = RH_MAX
                    IF ( RH < RH_MIN ) RH = RH_MIN
                    RH_3D(J8,I_LON,J2)  = RH
                  ELSE
!
! ----------------- Compute the dew point temperature by spline interpolation
!
                    IND = IXMN8 ( L_LEV, H, HEI_R8(J8) )
                    IF ( IND < 1  ) THEN
#ifndef SERIAL
!$OMP                    CRITICAL
#endif
                         WRITE ( 6, * ) 'J8= ', J8, ' HEI_R8= ', HEI_R8(J8)
                         WRITE ( 6, * ) 'H= ', H(1:L_LEV)
                         CALL ERR_LOG ( 4724, IUER, 'MALO_COMP_ACP', 'Trap of '// &
     &                       'internal control: valye of HEI_R8 is outtide of '// &
     &                       'height range ofd array H. Try to decrease PRES_HIGH' )
#ifndef SERIAL
!$OMP                    END CRITICAL
#endif
                         GOTO 430
                    END IF
!
! ----------------- Compute 
! ----------------- 1) TT_3D  air temperature
! ----------------- 2) TPD_1D dew point temperature
! ----------------- 3) RH     relative humidity
!
                    TT_3D(J8,I_LON,J2)  = FSPL8 ( HEI_R8(J8), L_LEV, H, TT_1D,  IND, SPL_TT )
                    TPD_VAL =             FSPL8 ( HEI_R8(J8), L_LEV, H, TPD_1D, IND, SPL_TD )
                    RH = DEXP ( LV__MALO/RV__MALO*(1.D0/TT_3D(J8,I_LON,J2) - 1.D0/TPD_VAL) )
!
                    IF ( RH > RH_MAX ) RH = RH_MAX
                    IF ( RH < RH_MIN ) RH = RH_MIN
                    RH_3D(J8,I_LON,J2)  = RH
               END IF
!
! ------------ Finally, compute the chemical potential
!
               ACP_3D(J8,I_LON,J2) = ACP__MALO*TT_3D(J8,I_LON,J2)**2*LOG(1.0/RH)
               IF ( TEST_STR == 'temp'  .AND.  I_LON == IT_LON  .AND.  J2 == IT_LAT ) THEN
                    ZM(J8) = ACP_3D(J8,I_LON,J2)
               END IF
!
               IF ( IS_R4_NAN(ACP_3D(J8,I_LON,J2)) ) THEN
                    WRITE ( 6, * ) 'ACP is nan! ', j8, i_lon, j2, j3
                    CALL EXIT ( 1 )
               END IF
 480        CONTINUE 
            IF ( TEST_STR == 'temp'  .AND.  I_LON == IT_LON  .AND.  J2 == IT_LAT ) THEN
                 CALL DIAGI_1 ( M_LEV, HEI_R8(1), ZM(1), IER )
            END IF
            IF ( TEST_STR == 'gen' ) TPD_LEV(I_LON,J2)= ACP_3D(4,I_LON,J2)
 430     CONTINUE 
#ifndef SERIAL
!$OMP END PARALLEL DO
#endif
         IF ( FL_ERROR ) RETURN
!
! ------ We extend longitude coverage to one pixel eastward.
! ------ The pixel with longitude index L_LON+1 is equal to the pixel with index 1
!
         TT_3D(1-MALO__MDEG:M_LEV,L_LON+1,J2)  = TT_3D(1-MALO__MDEG:M_LEV,1,J2) 
         RH_3D(1-MALO__MDEG:M_LEV,L_LON+1,J2)  = RH_3D(1-MALO__MDEG:M_LEV,1,J2) 
         ACP_3D(1-MALO__MDEG:M_LEV,L_LON+1,J2) = ACP_3D(1-MALO__MDEG:M_LEV,1,J2) 
 420  CONTINUE 
      VAL_MIN =  1.0
      VAL_MAX = -1.0
      IF ( TEST_STR == 'gen' ) CALL PLOT_GRID_R4 ( 1, 7, 0, 1, L_LON, L_LAT, &
     &                         TPD_LEV, 'Plotik-a', 'B', VAL_MIN, VAL_MAX, 'C', -2 )
!
! --- Build arrays of longitude/latitude
!
      DO 490 J9=1,L_LON+1
         LON_R4(J9) = (J9-1)*PI2/L_LON
 490  CONTINUE 
!
      DO 4100 J10=1,L_LAT
         LAT_R4(J10) = -P2I + (J10-1)*PI__NUM/(L_LAT-1)
 4100 CONTINUE 
!
      IF ( TEST_STR == 'timer' ) THEN
           CALL WALL_TIMER ( STR )
           WRITE ( 6, '(A)' ) 'Collecting surface pressure:      '//STR(1:I_LEN(STR)-5)
           CALL WALL_TIMER ( %VAL(0) )
      END IF
!
! --- Compute coefficients of 3D interpolating B-splines
!
      DIMS(1) = M_LEV
      DIMS(2) = L_LON+1
      DIMS(3) = L_LAT
!
! --- ... for atmospheric chemical potential
!
      CALL ERR_PASS ( IUER, IER )
      CALL BSPL4_3D_CMP ( MALO__MDEG, 0, DIMS, HEI_R4, LON_R4, LAT_R4, &
     &                    ACP_3D, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 4725, IUER, 'MALO_COMP_ACP', 'Failure in an attempt '// &
     &         'to compute coefficients of the 3D interpolating spline '// &
     &         'air temperature' )
           RETURN 
      END IF
!
! --- ... and for air tempertature
!
      CALL ERR_PASS ( IUER, IER )
      CALL BSPL4_3D_CMP ( MALO__MDEG, 0, DIMS, HEI_R4, LON_R4, LAT_R4, &
     &                    TT_3D, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 4726, IUER, 'MALO_COMP_ACP', 'Failure in an attempt '// &
     &         'to compute coefficients of the 3D interpolating spline for '// &
     &         'air temperature' )
           RETURN 
      END IF
!
! --- ... and for relative humidity
!
      CALL ERR_PASS ( IUER, IER )
      CALL BSPL4_3D_CMP ( MALO__MDEG, 0, DIMS, HEI_R4, LON_R4, LAT_R4, &
     &                    RH_3D, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 4727, IUER, 'MALO_COMP_ACP', 'Failure in an attempt '// &
     &         'to compute coefficients of the 3D interpolating spline for '// &
     &         'relative humidity' )
           RETURN 
      END IF
      IF ( TEST_STR == 'timer' ) THEN
           CALL WALL_TIMER ( STR )
           WRITE ( 6, '(A)' ) 'Computation of 3D B-spline:       '//STR(1:I_LEN(STR)-5)
           CALL WALL_TIMER ( %VAL(0) )
      END IF
!
      HEB_ACP%MJD = HEB_DELP%MJD
      HEB_ACP%UTC = HEB_DELP%UTC
      HEB_ACP%TAI = HEB_DELP%TAI
!
      HEB_ST%MJD = HEB_DELP%MJD
      HEB_ST%UTC = HEB_DELP%UTC
      HEB_ST%TAI = HEB_DELP%TAI
!
      HEB_RH%MJD = HEB_DELP%MJD
      HEB_RH%UTC = HEB_DELP%UTC
      HEB_RH%TAI = HEB_DELP%TAI
      INDS(3) = 1
!
      IF ( MODE == MALO__ACP_GRID ) THEN
!
! -------- Now compute ACP and temperature at the output grid at the
! -------- specified height by 3D interpolation 
!
           DO 4110 J11=1,HEB_ACP%DIMS(2)
              LAT_VAL = -P2I + (J11-1)*PI__NUM/(HEB_ACP%DIMS(2)-1)
              INDS(3) = IXMN4_S ( INDS(3), L_LAT, LAT_R4, LAT_VAL )
              IF ( LAT_VAL .LE. LAT_R4(1)     ) INDS(3) = 1
              IF ( LAT_VAL .GE. LAT_R4(L_LAT) ) INDS(3) = L_LAT-1
              INDS(2) = 1
              DO 4120 J12=1,HEB_ACP%DIMS(1)
                 I_LON = J12 + L_LON/2
                 IF ( I_LON > L_LON ) I_LON = I_LON - L_LON
                 LON_VAL = (J12-1)*PI2/HEB_ACP%DIMS(1)
                 INDS(2) = IXMN4_S ( INDS(2), L_LON+1, LON_R4, LON_VAL )
                 IF ( LON_VAL .LE. LON_R4(1)       ) INDS(2) = 1
                 IF ( LON_VAL .GE. LON_R4(L_LON+1) ) INDS(2) = L_LON
                 IF ( TEST_STR == 'intrp' ) THEN
                      HEB_OH%VAL(J12,J11,1,1) = HEB_G%VAL(I_LON,J11,1,1)
                 END IF
                 INDS(1) = IXMN4 ( M_LEV, HEI_R4, HEB_OH%VAL(J12,J11,1,1) + SNGL(HEI_ABOVE_SURFACE) )
                 ARGS(1) = HEB_OH%VAL(J12,J11,1,1) + HEI_ABOVE_SURFACE
                 ARGS(2) = LON_VAL
                 ARGS(3) = LAT_VAL
                 HEB_ACP%VAL(J12,J11,1,1) = VAL_3D_BSPL4 ( ARGS, MALO__MDEG, DIMS, INDS, &
     &                                                     HEI_R4, LON_R4, LAT_R4, ACP_3D )
                 HEB_ST%VAL(J12,J11,1,1)  = VAL_3D_BSPL4 ( ARGS, MALO__MDEG, DIMS, INDS, &
     &                                                     HEI_R4, LON_R4, LAT_R4, TT_3D )
                 HEB_RH%VAL(J12,J11,1,1)  = VAL_3D_BSPL4 ( ARGS, MALO__MDEG, DIMS, INDS, &
     &                                                     HEI_R4, LON_R4, LAT_R4, RH_3D )
                 IF ( HEB_ACP%VAL(J12,J11,1,1) < ACP_MIN ) HEB_ACP%VAL(J12,J11,1,1) = ACP_MIN
 4120         CONTINUE 
 4110      CONTINUE 
         ELSE IF ( MODE == MALO__ACP_STA ) THEN
!
! -------- Now compute ACP and temperature for the list of specified 
! -------- stations at the specified height by 3D interpolation 
!
           ARGS(1:3) = COO_HLP(1:3)
           INDS(1) = IXMN4 ( M_LEV, HEI_R4, ARGS(1) )
           INDS(2) = IXMN4 ( L_LON+1, LON_R4, ARGS(2) )
           IF ( ARGS(2) .LE. LON_R4(1)       ) INDS(2) = 1
           IF ( ARGS(2) .GE. LON_R4(L_LON+1) ) INDS(2) = L_LON
!
           INDS(3) = IXMN4 ( L_LAT, LAT_R4, ARGS(3) )
           IF ( ARGS(3) .LE. LAT_R4(1)     ) INDS(3) = 1
           IF ( ARGS(3) .GE. LAT_R4(L_LAT) ) INDS(3) = L_LAT-1
           ACP_VAL = VAL_3D_BSPL4 ( ARGS, MALO__MDEG, DIMS, INDS, &
     &                              HEI_R4, LON_R4, LAT_R4, ACP_3D )
      END IF
!
      DEALLOCATE ( RH_3D   ) 
      DEALLOCATE ( TT_3D   ) 
      DEALLOCATE ( ACP_3D ) 
      DEALLOCATE ( LAT_R4 ) 
      DEALLOCATE ( LON_R4 ) 
      IF ( TEST_STR == 'timer' ) THEN
           CALL WALL_TIMER ( STR ) 
           WRITE ( 6, '(A)' ) 'Perform 3D B-spline intrpolation: '//STR(1:I_LEN(STR)-5)
      END IF
      IF ( TEST_STR == 'diff' .OR. TEST_STR == 'intrp' .OR. TEST_STR == 'gen' ) THEN
           DEALLOCATE ( TPD_LEV )
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  MALO_COMP_ACP  !#!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE CORRECT_SPEC_HUM ( N, Q, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine CORRECT_SPEC_HUM  checks the specific humidity profile     *
! *   and corrects it for voids.                                         *
! *                                                                      *
! * ### 14-MAR-2013 CORRECT_SPEC_HUM v1.0 (c)  L. Petrov 15-MAR-2013 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INTEGER*4  N, IUER
      REAL*4     Q(N)
      INTEGER*4  M
      PARAMETER  ( M = 256 )
      REAL*8     Q_LIM, QL_MAX_DER, QL_MAX_DR2
      INTEGER*4  N_LIN
      PARAMETER  ( Q_LIM = 1.0D-10 )
      PARAMETER  ( QL_MAX_DER = 2.5D0 )
      PARAMETER  ( QL_MAX_DR2 = 5.0D0 )
      PARAMETER  ( N_LIN = 8 ) ! Number of points for linear extrapolation
      REAL*8     ARG(M), LQ(M), SPL(M), TMP(M), ARG_INTRP, LQ_DR, LQ_SH, &
     &           LQDD, LQDD_MAX 
      LOGICAL*1  FL_Q, FL_DER, FL_POI(M)
      INTEGER*4  J1, J2, J3, J4, J5, J6, J7, K, L, IP, IND_MAX, IER
      REAL*8,    EXTERNAL :: FSPL8, DSPL8, D2SPL8
      INTEGER*4, EXTERNAL :: IXMN8
!
      FL_Q = .FALSE.
      DO 410 J1=1,N
         IF ( Q(J1) < Q_LIM ) THEN
              FL_Q = .TRUE.
         END IF
         IF ( J1 > 1 ) THEN
              IF ( ABS(LOG(Q(J1)) - LOG(Q(J1-1))) > QL_MAX_DER ) THEN
                   FL_Q = .TRUE.
              END IF
         END IF
 410  CONTINUE 
      IF ( FL_Q ) THEN
           FL_POI = .TRUE.
           DO 420 J2=N,1,-1
              L = N+1-J2
              IF ( Q(J2) < Q_LIM ) THEN
                   FL_POI(J2) = .FALSE.
              END IF
 420       CONTINUE 
!
           DO 430 J3=1,N/2
              K = 0
              DO 440 J4=N,1,-1
                 L = N+1-J4
                 IF ( FL_POI(J4) ) THEN
                      K = K + 1
                      ARG(K) = L
                      LQ(K)  = LOG(Q(J4))
                 END IF
 440          CONTINUE 
!
              CALL ERR_PASS ( IUER, IER )
              CALL REGR8 ( N_LIN, ARG, LQ, LQ_DR, LQ_SH, IER  )
!
              CALL ERR_PASS ( IUER, IER )
              CALL MAKE_SPLINE ( 3, K, ARG, LQ, 0.0D0, 0.0D0, SPL, TMP, IER )
              IF ( IER .NE. 0 ) THEN
                   WRITE ( 6, * ) 'K= ', K
                   WRITE ( 6, * ) ' ARG = ', ARG(1:K)
                   CALL ERR_LOG ( 4741, IUER, 'CORRECT_SPEC_HUM', 'Failure '// &
     &                 'in an attempt to compute interpolating B-spline '// &
     &                 'for logarithm of specific humidity' )
                   RETURN 
              END IF
              FL_DER = .FALSE.
              LQDD_MAX = -1.D00
              DO 450 J5=1,N
                 ARG_INTRP = N+1-J5
                 IP = IXMN8 ( K, ARG, ARG_INTRP )
                 IF ( FL_POI(J5) .AND. IP > 0 ) THEN
                      LQDD = D2SPL8 ( ARG_INTRP, K, ARG, LQ, IP, SPL )
                      IF ( DABS(LQDD) > LQDD_MAX ) THEN
                           LQDD_MAX = DABS(LQDD)
                           IND_MAX = J5
                      END IF
                 END IF
 450          CONTINUE 
              IF ( LQDD_MAX < QL_MAX_DR2 ) GOTO 810 
              FL_POI(IND_MAX) = .FALSE.
 430       CONTINUE 
 810       CONTINUE 
!
           DO 460 J6=1,N
              IF ( .NOT. FL_POI(J6) ) THEN
                   ARG_INTRP = N+1-J6
                   IP = IXMN8 ( K, ARG, ARG_INTRP )
                   IF ( IP == -1 ) THEN
                        Q(J6) = EXP ( LQ_SH + LQ_DR*(ARG_INTRP - ARG(1)) )
                     ELSE 
                        IF ( IP == -2 ) IP = K
                        Q(J6) = EXP ( FSPL8 ( ARG_INTRP, K, ARG, LQ, IP, SPL ) )
                   END IF
              END IF
 460       CONTINUE 
      END IF
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE CORRECT_SPEC_HUM  !#!  
