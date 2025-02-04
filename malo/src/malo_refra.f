      SUBROUTINE MALO_REFRA ( MODE, NTIM, HEB_DELP, HEB_T, HEB_Q, HEB_OH, &
     &                        MALO, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  MALO_REFRA  computes the coefficients of expansion of the *
! *   refractivity of moist air in the 4D B-spline basis.                *
! *                                                                      *
! *   MALO_REFRA honors environment variable OMP_NUM_THREADS and         *
! *   it runs in parallel mode if OMP_NUM_THREADS > 1 and MALO_REFRA     *
! *   is not called from the parallel region. However, the speed-up      *
! *   factor usually is less than the number of threads, since           *
! *   MALO_REFRA thrashes memory extensively and has a high cache        *
! *   miss rate.                                                         *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *     MODE ( INTEGER*4 ) -- mode of refractivity computation:          *
! *                           1 -- ICESat1 ( 1064 nm );                  *
! *                           2 -- ICESat2 (  532 nm );                  *
! *     NTIM ( INTEGER*4 ) -- Number of time epochs.                     *
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
! *                           surface.                                   *
! *                                                                      *
! * ________________________ Output parameters: ________________________ *
! *                                                                      *
! *    MALO ( MALO__TYPE ) -- The data structure that on output will     *
! *                           keep the 4D array of the expansion of      *
! *                           the refractivity field into the            *
! *                           B-spline basis in the array                *
! *                           MALO%REFRA_4D.                             *
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
! * ### 12-JUN-2013    MALO_REFRA   v2.0 (c) L. Petrov  24-OCT-2013  ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'malo.i'
      INCLUDE   'heb.i'
      INTEGER*4  MODE, NTIM, IUER
      TYPE     ( MALO__TYPE ) :: MALO
      TYPE     ( HEB__TYPE  ) :: HEB_DELP(NTIM), HEB_T(NTIM), HEB_Q(NTIM), &
     &                           HEB_OH
      REAL*8     P(0:MALO__MHEI), PD(MALO__MHEI), PW(MALO__MHEI), &
     &           T(MALO__MHEI), HYPS(0:MALO__MHEI), &
     &           TMP_R8(MALO__MHEI+MALO__MDEG), H(0:MALO__MHEI), &
     &           GE, G, PHI, ZM(MALO__MHEI), &
     &           T_RATE, T_NLEV, LPL, LPH, PRES_R8(MALO__MHEI), &
     &           HEI_R8(MALO__MHEI), PRES_VAL, TEMP, LOG_PRES, &
     &           Q(MALO__MHEI), LOGP_1D(MALO__MHEI), LOGPW_1D(MALO__MHEI), &
     &           TEM_1D(MALO__MHEI), SPL_H(MALO__MHEI+MALO__MDEG), &
     &           SPL_P(MALO__MHEI+MALO__MDEG), SPL_PW(MALO__MHEI+MALO__MDEG), &
     &           SPL_TEM(MALO__MHEI+MALO__MDEG), PRES_LEV, &
     &           PW_LEV, GMR_DRY, GMR_WET
      REAL*4     HEI_R4(MALO__MHEI), LAT_VAL, LON_VAL, ARGS(4)
      INTEGER*8  MEL
      REAL*8     EPS, PRES_HIGH, HEI_MIN, HEI_MIN_INT, HEI_MAX, PRES_LN_0, PRES_LN_RATE
      PARAMETER  ( PRES_HIGH    = 25000.0D0 )
      PARAMETER  ( HEI_MIN      =  -500.0D0 )
      PARAMETER  ( HEI_MIN_INT  = -1000.0D0 )
      PARAMETER  ( HEI_MAX      =  9000.0D0 )
      PARAMETER  ( PRES_LN_0    = 11.5476D0 )
      PARAMETER  ( PRES_LN_RATE = -1.323D-4 )
      PARAMETER  ( EPS = 1.D-5 )
      REAL*8       SPD__U1_GMAO72, SPD__U2_GMAO72, SPD__U3_GMAO72 
      PARAMETER  ( SPD__U1_GMAO72 =    20.25319 )
      PARAMETER  ( SPD__U2_GMAO72 =  1200.00000 )
      PARAMETER  ( SPD__U3_GMAO72 =  -169.30782 )
      INTEGER*4    M_LEV
      REAL*8       MD, MW
      PARAMETER  ( M_LEV = 64        )
      PARAMETER  ( MD    = MA__MAPL  )
      PARAMETER  ( MW    = H2O__MAPL )
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
      CHARACTER  STR*128, STR1*128, TEST_STR*8
      INTEGER*4  L_LON, L_LAT, L_LEV, L_TIM, IND, IND_LAST, &
     &           K_LEV, N_LEV, I_LON, DIMS(4), INDS(4), &
     &           J1, J2, J3, J4, J5, J6, J7, J8, J9, J10, J11, J12, J13, &
     &           IT_LON, IT_LAT, NTHR, NTHR_SAVED, IER
      REAL*4     QV_LEV(MALO__MHEI)
      REAL*4,    ALLOCATABLE  :: TD_LEV(:,:), TT_LEV(:,:)
      REAL*8     HARR(2), TARR(2), MIN_VAL, MAX_VAL , LON_TST, LAT_TST, &
     &           P_VAL, PW_VAL, TEM_VAL, RLEV
      LOGICAL*1  FL_ERROR
      INTEGER*4, EXTERNAL :: I_LEN, ILEN, IXMN8, IXMN8_S, IXMN4, IXMN4_S, &
     &                       OMP_GET_NUM_THREADS, OMP_GET_MAX_THREADS
      LOGICAL*4, EXTERNAL :: OMP_IN_PARALLEL
      REAL*8,    EXTERNAL :: ISPL8, FSPL8, PW_TO_RELH, REFRA_CIDDOR_IC1, &
     &                       REFRA_CIDDOR_IC2, BSPL_KNT 
      REAL*4,    EXTERNAL :: VAL_3D_BSPL4, VAL_4D_BSPL4
      LOGICAL*4, EXTERNAL :: IS_R4_NAN
      CHARACTER, EXTERNAL :: MJDSEC_TO_DATE*30
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
!!      TEST_STR = 'none'    ! Supported values: none, timer, diff, pw, pres, temp, intrp, dewt
      TEST_STR = 'test_01'  ! Supported values: none, timer, diff, pw, pres, temp, intrp, dewt
      LON_TST  = 270.0*DEG__TO__RAD
      LAT_TST  =   0.0*DEG__TO__RAD
!
! --- Grid of the input numerical models
!
      L_LON = HEB_DELP(1)%DIMS(1)
      L_LAT = HEB_DELP(1)%DIMS(2)
      L_LEV = HEB_DELP(1)%DIMS(3)
      L_TIM = NTIM
!
      IT_LON = 1 + L_LON*(LON_TST/PI2)
      IF ( IT_LON > L_LON ) IT_LON = IT_LON - L_LON
      IT_LAT = 1 + (L_LAT-1)*((LAT_TST+P2I)/PI__NUM)
!
! --- Grid of the output Total pressure, partial pressure of water vapor and air temperature
!
      MALO%NLEV = MALO__MHEI
      MALO%NLON = L_LON+2
      MALO%NLAT = L_LAT
      MALO%NTIM = L_TIM
      IF ( ASSOCIATED ( MALO%REFRA_4D ) ) DEALLOCATE ( MALO%REFRA_4D )
      IF ( L_TIM > 1 ) THEN
           ALLOCATE ( MALO%REFRA_4D(1-MALO__MDEG:MALO%NLEV,1-MALO__MDEG:MALO%NLON,1-MALO__MDEG:MALO%NLAT,1-MALO__MDEG:MALO%NTIM), &
     &                STAT=IER )
           MEL = INT8(MALO%NLEV)*INT8(MALO%NLON)*INT8(MALO%NLAT)*INT8(MALO%NTIM)
         ELSE
           ALLOCATE ( MALO%REFRA_4D(1-MALO__MDEG:MALO%NLEV,1-MALO__MDEG:MALO%NLON,1-MALO__MDEG:MALO%NLAT,1), &
     &                STAT=IER )
           MEL = INT8(MALO%NLEV)*INT8(MALO%NLON)*INT8(MALO%NLAT)
      END IF
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH  ( STR )
           CALL IINCH8 ( INT8(4)*MEL, STR )
           CALL ERR_LOG ( 4811, IUER, 'MALO_REFRA', 'Failure in an attempt '// &
     &         'to allocate '//STR(1:I_LEN(STR))//' bytes of dynamic memory '// &
     &         'for array MALO%REFRA_4D' )
           RETURN 
      END IF
      MALO%REFRA_4D_STATUS = MALO__ALLO
!
      IF ( TEST_STR == 'timer' ) THEN
           WRITE ( 6, '(A,I6,1X,I6)' ) 'MALO_REFRA NLON/NLAT = ', HEB_DELP(1)%DIMS(1), HEB_DELP(1)%DIMS(2)
           CALL WALL_TIMER ( %VAL(0) ) 
      END IF
!
! --- Compute the dependence of log P on height for the Standard Atmosphere
!
      LPL = PRES_LN_0 + PRES_LN_RATE*HEI_MIN
      LPH = PRES_LN_0 + PRES_LN_RATE*HEI_MAX
      DO 410 J1=1,M_LEV
         LOG_PRES = LPL + (J1-1)*(LPH - LPL)/(M_LEV-1)
         HEI_R8(J1)  = -PRES_LN_0/PRES_LN_RATE + LOG_PRES/PRES_LN_RATE
         HEI_R4(J1)  = HEI_R8(J1)
 410  CONTINUE 
!
      ALLOCATE ( MALO%LEV(MALO__MHEI), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( 4*MALO__MHEI, STR )
           CALL ERR_LOG ( 4812, IUER, 'MALO_REFRA', 'Failure in '// &
     &         'an attempt to allocate '//STR(1:I_LEN(STR))// &
     &         ' bytes of dynamic memory for array MALO%LEV' )
           RETURN 
      END IF
!
      ALLOCATE ( MALO%LAT(MALO%NLAT), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( 4*MALO%NLAT, STR )
           CALL ERR_LOG ( 4813, IUER, 'MALO_REFRA', 'Failure in '// &
     &         'an attempt to allocate '//STR(1:I_LEN(STR))// &
     &         ' bytes of dynamic memory for array MALO%LAT' )
           RETURN 
      END IF
!
      ALLOCATE ( MALO%LON(MALO%NLON), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( 4*MALO%NLON, STR )
           CALL ERR_LOG ( 4814, IUER, 'MALO_REFRA', 'Failure in '// &
     &         'an attempt to allocate '//STR(1:I_LEN(STR))// &
     &         ' bytes of dynamic memory for array MALO%LON' )
           RETURN 
      END IF
!
      ALLOCATE ( MALO%TIM(MALO%NTIM), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( 4*MALO%NTIM, STR )
           CALL ERR_LOG ( 4815, IUER, 'MALO_REFRA', 'Failure in '// &
     &         'an attempt to allocate '//STR(1:I_LEN(STR))// &
     &         ' bytes of dynamic memory for array MALO%TIM' )
           RETURN 
      END IF
!
      DO 420 J2=1,L_TIM
         DO 430 J3=1,L_LAT
!
! --------- PHI -- is geodetic latitude
!
            PHI = -P2I + (J3-1)*PI__NUM/(L_LAT-1)
!
! --------- Compute gravity on the reference ellipsoid for a given geodetic latitude
!
            GE = ACC_EQU__WGS84*(1.D0 + GRV_LAT__WGS84*DSIN(PHI)**2)/ &
     &             DSQRT(1.D0 - (2.D0*FLAT__WGS84 - FLAT__WGS84**2)*DSIN(PHI)**2 )
            FL_ERROR = .FALSE.
#ifndef SERIAL
!$OMP    PARALLEL DO IF ( NTHR > 1 ), &
!$OMP&   PRIVATE ( J4, J5, J6, J7, J8, J9, J10, J11, IER, G, I_LON, P, TMP_R8, HYPS, &
!$OMP&             H, PW, PD, T, Q, ZM, QV_LEV, SPL_TEM, SPL_H, SPL_PW, SPL_P, &
!$OMP&             LOGP_1D, LOGPW_1D, TEM_1D, RLEV, HEI_R4, T_RATE, T_NLEV, &
!$OMP&             PW_VAL, P_VAL, TEM_VAL, K_LEV, IND, IND_LAST, N_LEV ), &
!$OMP&             SCHEDULE ( STATIC )
#endif
            DO 440 J4=1,L_LON
!
! ------------ Execution time of the code within the cycle is 80 mks
!
               IF ( FL_ERROR ) GOTO 440
!
! ------------ NB: Merra/Geos longitude starts from -180deg
!
               I_LON = J4 + L_LON/2
               IF ( I_LON > L_LON ) I_LON = I_LON - L_LON
!
! ------------ First run is for computing height above the ellipsoid for a given pressure level
!
               P(L_LEV+1) = 1.0D0
               N_LEV = L_LEV
!
! ------------ Correct specific humidity if we can
!
               QV_LEV(1:L_LEV) = HEB_Q(J2)%VAL(J4,J3,1:L_LEV,1)
               CALL ERR_PASS ( IUER, IER )
               CALL CORRECT_SPEC_HUM ( L_LEV, QV_LEV, IER )
               IF ( IER .NE. 0 ) THEN
#ifndef SERIAL
!$OMP               CRITICAL
#endif
                    WRITE ( 6, * ) 'J4= ', J4, ' J3= ', J3
                    CALL ERR_LOG ( 4816, IUER, 'MALO_REFRA', 'Error '// &
     &                  'in an attempt to correct specific humidity' )
                    FL_ERROR = .TRUE.
#ifndef SERIAL
!$OMP               END CRITICAL
#endif
                    GOTO 440
               END IF
!
! ------------ Re-indexing pressure in such a way that the 1-st level of 
! ------------ array P starts at the nominal surface
!
               DO 450 J5=1,L_LEV
                  K_LEV = L_LEV+1-J5
                  IF ( J5 == 1 ) THEN
                       P(K_LEV) = P(K_LEV+1) + HEB_DELP(J2)%VAL(J4,J3,J5,1)/2.0D0
                     ELSE 
                       P(K_LEV) = P(K_LEV+1) + HEB_DELP(J2)%VAL(J4,J3,J5-1,1)/2.0D0 + &
     &                                         HEB_DELP(J2)%VAL(J4,J3,J5,1)/2.0D0 
                  END IF
                  IF ( P(K_LEV) < PRES_HIGH ) N_LEV = K_LEV
!
! --------------- Compute approximate gravity acceleration at a given pressure level
! --------------- using a regression formulae for the US Standard Atmosphere
!
                  G = GE*(1.D0 + GP0 + GP1*DLOG(P(K_LEV)))
!
! --------------- PW -- partial pressure of water vapour
!
                  Q(K_LEV)    = QV_LEV(J5)
                  PW(K_LEV)   = P(K_LEV) * Q(K_LEV)/ ( MW/MD + (1.0D0 - MW/MD)*Q(K_LEV) )
!
! --------------- PD -- Partial pressure of dry air
!
                  PD(K_LEV)   = P(K_LEV) - PW(K_LEV)
                  T(K_LEV)    = HEB_T(J2)%VAL(J4,J3,J5,1)
!
! --------------- Compute water vapor compressibility
!
                  ZM(K_LEV) = 1.D0/ &
     &              ( 1.0D0 &
     &               - P(K_LEV)/T(K_LEV)*  (SPD__COMP_A0 + SPD__COMP_A1*(T(K_LEV) - SPD__ABS_TEMP) +  &
     &                                                     SPD__COMP_A2*(T(K_LEV) - SPD__ABS_TEMP)**2 ) &
     &               + PW(K_LEV)/T(K_LEV)* (SPD__COMP_B0 + SPD__COMP_B1*(T(K_LEV) - SPD__ABS_TEMP)) &
     &               + PW(K_LEV)**2/(P(K_LEV)*T(K_LEV))* &
     &                                     (SPD__COMP_C0 + SPD__COMP_C1*(T(K_LEV) - SPD__ABS_TEMP)) &
     &               + P(K_LEV)**2/T(K_LEV)**2*  SPD__COMP_D0 &
     &               + PW(K_LEV)**2/T(K_LEV)**2* SPD__COMP_E0 &
     &             )
!
! --------------- Compute function HYPS that is the right hand-side of 
! --------------- the hypsometric equation, while the left hand side is h(P)
!
                  HYPS(K_LEV) = R__MAPL*T(K_LEV)/ &
     &                          (G*(MD*PD(K_LEV) + MW*PW(K_LEV))*ZM(K_LEV))
!
                  IF ( TEST_STR == 'test_01' ) THEN
                       IF ( J2 == 5 .AND. J3 == 5 .AND. I_LON == 560 ) THEN 
                            write  ( 6, 216 ) K_LEV, HEB_DELP(J2)%VAL(J4,J3,J5,1), T(K_LEV), &
     &                                        G, ZM(K_LEV), HYPS(K_LEV)
                            216  format ( 'Layer: ',I3, ' Delp= ', 0PF11.6, ' T= ', 0PF9.5, &
     &                                    ' G = ', 0PF10.8, ' Zm = ', 0PF10.8, ' Y= ', 1PD14.7 )
                       END IF
                  END IF
 450           CONTINUE 
!
               P(0) = P(1) + HEB_DELP(J2)%VAL(J4,J3,L_LEV,1)/2.0D0
               HYPS(0) = HYPS(1) - (P(1) - P(0))*(HYPS(2) - HYPS(1))/(P(2) - P(1))
!
! ------------ Special trick for making interpolation: we reverse the sign of pressure.
! ------------ Otherwise, MAKE_SPLINE will complain the argument is not in rising order
!
               P = -P ! For interpolation
               CALL ERR_PASS ( IUER, IER ) 
               CALL MAKE_SPLINE ( 3, L_LEV+1, P, HYPS, 0.0D0, 0.0D0, SPL_H, TMP_R8, IER )
               IF ( IER .NE. 0 ) THEN
#ifndef SERIAL
!$OMP               CRITICAL
#endif
                    CALL ERR_LOG ( 4817, IUER, 'MALO_REFRA', 'Failure in an attempt '// &
     &                   'to compute coefficients of the interpolating spline' )
                    FL_ERROR = .TRUE.
#ifndef SERIAL
!$OMP               END CRITICAL
#endif
                    GOTO 440
               END IF
!
! ------------ Compute height at pressure grid by integrating the spline that 
! ------------ interpolates HYPS
!
               H(0) = HEB_OH%VAL(J4,J3,1,1) ! Boundary condition at the surface
               DO 460 J6=1,L_LEV
                  IF ( J6 < L_LEV ) THEN
                       H(J6) = H(J6-1) + ISPL8 ( P(J6), L_LEV+1, P, HYPS, &
     &                                           J6, J6+1, SPL_H, IER )
                     ELSE IF ( J6 == L_LEV ) THEN
                       H(J6) = H(J6-1) + ISPL8 ( P(J6)*(1.D0-EPS), L_LEV+1, P, HYPS, &
     &                                           J6, J6+1, SPL_H, IER )
                  END IF
 460           CONTINUE 
!
! ------------ Compute the lapse rate between N_LEV/2 and N_LEV
!
               CALL ERR_PASS ( IUER, IER )
               CALL REGR8 ( N_LEV - N_LEV/2 + 1, H(N_LEV/2), T(N_LEV/2), T_RATE, T_NLEV, IER )
!
! ------------ Second round
!
               DO 480 J8=1,L_LEV
!
! --------------- Compute gravity at a given height
!
                  G = GE*(1.D0 - 2.D0/REA__WGS84* &
     &                          (1.0D0 + OMEGA__EGM96**2*REA__WGS84**3*(1.D0 - FLAT__WGS84)/GM__EGM96 + &
     &                           FLAT__WGS84*(1.D0 - 2.0D0*DSIN(PHI)**2))*H(J8) &
     &                         + 3.D0/REA__WGS84**2*H(J8)**2 )
!
! --------------- Compute improved function for integration
!
                  HYPS(J8) = R__MAPL*T(J8)/(G*(MD*PD(J8) + MW*PW(J8))*ZM(J8))
                  IF ( TEST_STR == 'test_01' ) THEN
                       IF ( J2 == 5 .AND. J3 == 5 .AND. I_LON == 560 ) THEN 
                           WRITE ( 6, 210 ) 73-J8, G, -P(J8), PW(J8), PD(J8), &
     &                                      T(J8), HEB_DELP(J2)%VAL(J4,J3,73-J8,1)
 210   FORMAT              ( ' Layer : ', i3, ' g= ', f20.16, &
     &                       '      Pres= ', f20.12, '      PW= ', 1PD22.15, &
     &                       '    PD = ', 0pf20.13, '      T= ', 0pF20.13, &
     &                       ' Delp: ', 0PF11.6 )
                       END IF
                  END IF
 480           CONTINUE 
               HYPS(0) = HYPS(1) - (P(1) - P(0))*(HYPS(2) - HYPS(1))/(P(2) - P(1))
!
! ------------ Compute interpolating spline of HYPS(P) the second time
!
               CALL ERR_PASS ( IUER, IER ) 
               CALL MAKE_SPLINE ( 3, L_LEV+1, P, HYPS, 0.0D0, 0.0D0, SPL_H, TMP_R8, IER )
               IF ( IER .NE. 0 ) THEN
#ifndef SERIAL
!$OMP               CRITICAL
#endif
                    WRITE ( 6, * ) 'ier= ',ier 
                    WRITE ( 6, * ) 'J4(lon) = ', INT2(J4), ' J3(lat) = ', INT2(J3)
                    WRITE ( 6, * ) 'L_LEV   = ', L_LEV + 1
                    WRITE ( 6, * ) 'P       = ', SNGL(P(1:L_LEV))
                    WRITE ( 6, * ) 'hyps    = ', SNGL(HYPS(1:L_LEV))
                    CALL ERR_LOG ( 4818, IUER, 'MALO_REFRA', 'Failure in '// &
     &                  'an attempt to compute coefficients of the '// &
     &                  'interpolating spline' )
                    FL_ERROR = .TRUE.
#ifndef SERIAL
!$OMP               END CRITICAL
#endif
                    GOTO 440
               END IF
!
! ------------ Compute height as a function of pressure the second time
!
               H(0) = HEB_OH%VAL(J4,J3,1,1) ! Boundary condition
               DO 490 J9=1,L_LEV
                  IF ( J9 < L_LEV ) THEN
                       H(J9) = H(J9-1) + ISPL8 ( P(J9), L_LEV+1, P, HYPS, &
     &                                           J9, J9+1, SPL_H, IER )
                     ELSE IF ( J9 == L_LEV ) THEN
                       H(J9) = H(J9-1) + ISPL8 ( P(J9)*(1.D0-EPS), L_LEV+1, P, HYPS, &
     &                                           J9, J9+1, SPL_H, IER )
                  END IF
                  LOGP_1D(J9)  = DLOG(-P(J9))
                  LOGPW_1D(J9) = DLOG(PW(J9))
                  TEM_1D(J9)   = T(J9)
                  IF ( TEST_STR == 'test_01' ) THEN
                       IF ( J2 == 5 .AND. J3 == 5 .AND. I_LON == 560 ) THEN 
                            G = GE*(1.D0 - 2.D0/REA__WGS84*(1.0D0 + OMEGA__EGM96**2*REA__WGS84**3* &
     &                          (1.D0 - FLAT__WGS84)/GM__EGM96 + FLAT__WGS84*(1.D0 - 2.0D0* &
     &                          DSIN(PHI)**2))*H(J9) + 3.D0/REA__WGS84**2*H(J9)**2)
                            WRITE  ( 6, 218 ) J9, L_LEV+1-J9, G, -P(J9), PW(J9), &
     &                                        -P(J9) - PW(J9), H(J9), T(J9), HYPS(J9)
       218                  FORMAT ( 'LAyer: ',I3, ' Lev: ', I3, ' G: ', 0PF10.8, &
     &                               ' Pres: ', 0PF12.5, &
     &                               ' Pw: ', 1PD12.5, ' Pd: ', 0PF12.5, ' Height: ', 0PF10.3, &
     &                               ' Temp: ', 0PF7.3, ' y= ', 1PD15.7 )
                      END IF
                  END IF
 490           CONTINUE 
               IF ( TEST_STR == 'test_01' ) THEN
                    IF ( J2 == 5 .AND. J3 == 5 .AND. I_LON == 560 ) THEN 
                         WRITE  ( 6, 219 ) H(0), -P(0), HYPS(0), GE
 219                     FORMAT ( 'GROUND:  Height: ', F10.3, ' Pres: ', F10.3, &
     &                            ' Y(0)= ', 1PD15.7, ' GE = ', 0PF12.9 )
                    END IF
               END IF
!
! ------------ Now we compute interpolation spline of the total pressure as 
! ------------ a function of height
!
               CALL ERR_PASS ( IUER, IER ) 
               CALL MAKE_SPLINE ( 3, L_LEV, H(1), LOGP_1D, 0.0D0, 0.0D0, &
     &                            SPL_P, TMP_R8, IER )
               IF ( IER .NE. 0 ) THEN
#ifndef SERIAL
!$OMP               CRITICAL
#endif
                    WRITE ( 6, * ) 'ier= ',ier 
                    WRITE ( 6, * ) 'J4(lon) = ', INT2(J4), ' J3(lat) = ', INT2(J3)
                    WRITE ( 6, * ) 'L_LEV   = ', L_LEV + 1
                    WRITE ( 6, * ) 'H       = ', SNGL(H(1:L_LEV))
                    WRITE ( 6, * ) 'logp_1d = ', SNGL(LOGP_1D(1:L_LEV))
                    CALL ERR_LOG ( 4819, IUER, 'MALO_REFRA', 'Failure in an attempt '// &
     &                   'to compute coefficients of the interpolating spline' )
                    FL_ERROR = .TRUE.
#ifndef SERIAL
!$OMP               END CRITICAL
#endif
                    GOTO 440
               END IF
!
! ------------ Now we compute the interpolation spline of water vapor partial 
! ------------ pressure as a function of height
!
               CALL ERR_PASS ( IUER, IER ) 
               CALL MAKE_SPLINE ( 3, L_LEV, H(1), LOGPW_1D, 0.0D0, 0.0D0, &
     &                            SPL_PW, TMP_R8, IER )
               IF ( IER .NE. 0 ) THEN
#ifndef SERIAL
!$OMP               CRITICAL
#endif
                    WRITE ( 6, * ) 'J4(lon) = ', INT2(J4), ' J3(lat) = ', INT2(J3)
                    WRITE ( 6, * ) 'L_LEV   = ', L_LEV + 1
                    WRITE ( 6, * ) 'H       = ', SNGL(H(1:L_LEV))
                    WRITE ( 6, * ) 'PW_1D   = ', SNGL(LOGPW_1D(1:L_LEV))
                    CALL ERR_LOG ( 4820, IUER, 'MALO_REFRA', 'Failure in an attempt '// &
     &                   'to compute coefficients of the interpolating spline' )
                    FL_ERROR = .TRUE.
#ifndef SERIAL
!$OMP               END CRITICAL
#endif
                    GOTO 440
               END IF
!
! ------------ Now we compute interpolation spline of air temperature as 
! ------------ a function of height
!
               CALL ERR_PASS ( IUER, IER ) 
               CALL MAKE_SPLINE ( 3, L_LEV, H(1), TEM_1D, 0.0D0, 0.0D0, &
     &                            SPL_TEM, TMP_R8, IER )
               IF ( IER .NE. 0 ) THEN
#ifndef SERIAL
!$OMP               CRITICAL
#endif
                    WRITE ( 6, * ) 'J4(lon) = ', INT2(J4), ' J3(lat) = ', INT2(J3)
                    WRITE ( 6, * ) 'L_LEV   = ', L_LEV + 1
                    WRITE ( 6, * ) 'H       = ', SNGL(H(1:L_LEV))
                    WRITE ( 6, * ) 'TEM_1D  = ', SNGL(TEM_1D(1:L_LEV))
                    CALL ERR_LOG ( 4821, IUER, 'MALO_REFRA', 'Failure in an attempt '// &
     &                   'to compute coefficients of the interpolating spline' )
                    FL_ERROR = .TRUE.
#ifndef SERIAL
!$OMP               END CRITICAL
#endif
                    GOTO 440
               END IF
!
! ------------ Now we compute total pressure, water vapour pressure and air temperature
! ------------ on a global grid
!
               IND_LAST = 1
               DO 4110 J11=1,MALO__MHEI
                  RLEV = J11 - 1 - (MALO__MHEI-1)/2
                  MALO%LEV(J11) = DEXP ( (RLEV - SPD__U3_GMAO72)/SPD__U1_GMAO72 ) - SPD__U2_GMAO72
                  HEI_R4(J11)= MALO%LEV(J11) 
                  G = GE*(1.D0 - 2.D0/REA__WGS84*(1.0D0 + OMEGA__EGM96**2*REA__WGS84**2* &
     &                   (1.D0 - FLAT__WGS84)/GM__EGM96 + (1.D0 - 2.D0*FLAT__WGS84)* &
     &                    DSIN(PHI)**2)*HEI_R4(J11) + 3.D0/REA__WGS84**2*HEI_R4(J11)**2)
                  IF ( HEI_R4(J11) < H(1) ) THEN
!
! -------------------- This height level is below the surface. We assume the atmosphere
! -------------------- beneath the surface obeys to the adiabatic gas law with a constant 
! -------------------- lapse rate computed at the previous step
!
                       TEM_VAL =  T(1) + T_RATE*(HEI_R4(J11) - H(1))
                       PW_VAL  =  PW(1)*(TEM_VAL/T(1))**(-G*MW/(R__MAPL*T_RATE))
                       P_VAL   = -P(1) *(TEM_VAL/T(1))**(-G*MD/(R__MAPL*T_RATE))
                     ELSE IF ( HEI_R4(J11) > H(L_LEV) ) THEN
!
! -------------------- This is above the upper level. We consider the atmosphere isothermal.
!
                       GMR_DRY = G*SPD__MA/SPD__R        
                       GMR_WET = G*SPD__MA/SPD__R        
                       TEM_VAL =  T(L_LEV)
                       PW_VAL  =  PW(L_LEV)*DEXP ( -GMR_WET*(HEI_R4(J11) - H(L_LEV))/TEM_VAL )
                       P_VAL   =  -P(L_LEV)*DEXP ( -GMR_DRY*(HEI_R4(J11) - H(L_LEV))/TEM_VAL )
                     ELSE
!
! -------------------- Compute air temperature by spline interpolation
!
                       IND = IXMN8 ( L_LEV, H(1), DBLE(HEI_R4(J11)) )
!!                       IND = IXMN8_S ( IND_LAST, L_LEV, H(1), DBLE(HEI_R4(J11)) )
                       TEM_VAL = FSPL8 ( DBLE(HEI_R4(J11)), L_LEV, H(1), TEM_1D, IND, SPL_TEM )
                       PW_VAL  = DEXP ( FSPL8 ( DBLE(HEI_R4(J11)), L_LEV, H(1), LOGPW_1D, IND, SPL_PW ) )
                       P_VAL   = DEXP ( FSPL8 ( DBLE(HEI_R4(J11)), L_LEV, H(1), LOGP_1D,  IND, SPL_P  ) )
                       IND_LAST = IND
                  END IF
                  IF ( MODE == MALO__REFRA_IC1 ) THEN
                       MALO%REFRA_4D(J11,I_LON,J3,J2) = REFRA_CIDDOR_IC1 ( P_VAL, PW_VAL, TEM_VAL ) - 1.0D0
                   ELSE IF ( MODE == MALO__REFRA_IC2 ) THEN
                       MALO%REFRA_4D(J11,I_LON,J3,J2) = REFRA_CIDDOR_IC2 ( P_VAL, PW_VAL, TEM_VAL ) - 1.0D0
                  END IF
                  IF ( TEST_STR == 'test_01' ) THEN
                       IF ( J2 == 5 .AND. J3 == 5 .AND. I_LON == 560 ) THEN 
                            WRITE ( 6, 220 ) J11, HEI_R4(J11), MALO%REFRA_4D(J11,I_LON,J3,J2), P_VAL, PW_VAL, TEM_VAL
 220                        FORMAT ( 'LayeR= ', I3, ' Hei= ', F10.3, ' Refr= ', F12.10, &
     &                               ' P= ', F12.5, ' PW = ', F12.8, ' Tem= ', F10.5 )
                       END IF
                  END IF
  4110         CONTINUE 
  440       CONTINUE 
#ifndef SERIAL
!$OMP END PARALLEL DO
#endif
            IF ( FL_ERROR ) RETURN
!
! --------- We extend longitude coverage to one pixel eastward.
! --------- The pixel with longitude index L_LON+1,L_LON+2 is equal 
! --------- to pixels with indexes 1 and 2.
!
            MALO%REFRA_4D(1-MALO__MDEG:MALO__MHEI,L_LON+1,J3,J2) = MALO%REFRA_4D(1-MALO__MDEG:MALO__MHEI,1,J3,J2) 
            MALO%REFRA_4D(1-MALO__MDEG:MALO__MHEI,L_LON+2,J3,J2) = MALO%REFRA_4D(1-MALO__MDEG:MALO__MHEI,2,J3,J2) 
 430     CONTINUE 
         IF ( J2 == 1 ) THEN
              MALO%TIM(J2) = 0.0
              MALO%MJD_BEG = HEB_DELP(J2)%MJD
              MALO%UTC_BEG = HEB_DELP(J2)%UTC
            ELSE 
              MALO%TIM(J2) = (HEB_DELP(J2)%MJD - MALO%MJD_BEG)*86400.0 + &
     &                       (HEB_DELP(J2)%UTC - MALO%UTC_BEG)
         END IF
 420  CONTINUE 
!
! --- Build arrays of longitude/latitude
!
      DO 4120 J12=1,MALO%NLON
         MALO%LON(J12) = (J12-1)*PI2/L_LON
 4120 CONTINUE 
!
      DO 4130 J13=1,MALO%NLAT
         MALO%LAT(J13) = -P2I + (J13-1)*PI__NUM/(L_LAT-1)
 4130 CONTINUE 
      MALO%REFRA_4D_STATUS = MALO__LOAD
!
      IF ( TEST_STR == 'timer' ) THEN
           CALL WALL_TIMER ( STR )
           WRITE ( 6, '(A)' ) 'Collecting three functions:    '//STR(1:I_LEN(STR)-5)
           CALL WALL_TIMER ( %VAL(0) )
      END IF
#ifndef SERIAL
      CALL OMP_SET_NUM_THREADS ( %VAL(NTHR_SAVED) )
#endif
!
! --- Compute coefficients of 3D interpolating B-splines
!
      DIMS(1) = MALO%NLEV
      DIMS(2) = MALO%NLON
      DIMS(3) = MALO%NLAT
      DIMS(4) = MALO%NTIM
!
      CALL ERR_PASS ( IUER, IER )
      CALL BSPL4_4D_CMP ( MALO__MDEG, 0, DIMS, MALO%LEV, MALO%LON, MALO%LAT, &
     &                    MALO%TIM, MALO%REFRA_4D, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 4822, IUER, 'MALO_REFRA', 'Failure in '// &
     &         'an attempt to compute coefficients of the 3D '// &
     &         'interpolating spline' )
           RETURN 
      END IF
      IF ( TEST_STR == 'timer' ) THEN
           CALL WALL_TIMER ( STR )
           WRITE ( 6, '(A)' ) 'Computation of 3D B-splines:   '//STR(1:I_LEN(STR)-5)
           CALL WALL_TIMER ( %VAL(0) )
      END IF
      MALO%REFRA_4D_STATUS = MALO__COMP
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  MALO_REFRA  !#!#
!
! ------------------------------------------------------------------------
!
      FUNCTION   REFRA_CIDDOR_IC1 ( P, PW, TEM )
! ************************************************************************
! *                                                                      *
! *   Routine REFRA_CIDDOR_IC1
! *                                                                      *
! * ## 16-SEP-2013 REFRA_CIDDOR_IC1 v1.0 (c)  L. Petrov  16-SEP-2013 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      REAL*8     REFRA_CIDDOR_IC1
      REAL*8     P, PW, TEM
!
! --- Thermodynamic constants
!
      REAL*8     SPD__R, SPD__MA, SPD__H2O, SPD__CO2, SPD__NOM_CO2, &
     &           SPD__ABS_TEMP, SPD__H2O_DRY_RAT
      PARAMETER  ( SPD__R        = 8.314472D0    ) ! CIPM-2007 J/(K mol)
      PARAMETER  ( SPD__MA       = 0.02896546D0  ) ! CIPM-2007 kg/mol
      PARAMETER  ( SPD__H2O      = 0.01801528    ) ! CIPM-2007 kg/mol
      PARAMETER  ( SPD__CO2      = 0.012011D0    ) ! CIPM-2007 kg/mol
      PARAMETER  ( SPD__NOM_CO2  = 0.004D0       ) ! CIPM-2007
      PARAMETER  ( SPD__ABS_TEMP = 273.15D0      )
      REAL*8     SPD__ENH_ALPHA, SPD__ENH_BETA, SPD__ENH_GAMMA
      PARAMETER  ( SPD__ENH_ALPHA = 1.00062D0 )
      PARAMETER  ( SPD__ENH_BETA  = 3.14D-8   )
      PARAMETER  ( SPD__ENH_GAMMA = 5.6D-7    )
      REAL*8     SPD__PSV_A, SPD__PSV_B, SPD__PSV_C, SPD__PSV_D
      PARAMETER  ( SPD__PSV_A =  1.2378847D-5  ) ! CIPM-2007
      PARAMETER  ( SPD__PSV_B = -1.9121316D-2  ) ! CIPM-2007
      PARAMETER  ( SPD__PSV_C = 33.93711047D0  ) ! CIPM-2007
      PARAMETER  ( SPD__PSV_D = -6.3431645D3  ) ! CIPM-2007
      REAL*8     SPD__COMP_A0, SPD__COMP_A1, SPD__COMP_A2, SPD__COMP_B0, SPD__COMP_B1, &
     &           SPD__COMP_C0, SPD__COMP_C1, SPD__COMP_D0, SPD__COMP_E0
      PARAMETER  ( SPD__COMP_A0 =  1.58123D-6 ) ! CIPM-2007
      PARAMETER  ( SPD__COMP_A1 = -2.9331D-8  ) ! CIPM-2007
      PARAMETER  ( SPD__COMP_A2 =  1.1043D-10 ) ! CIPM-2007
      PARAMETER  ( SPD__COMP_B0 =  5.707D-6   ) ! CIPM-2007
      PARAMETER  ( SPD__COMP_B1 = -2.051D-8   ) ! CIPM-2007
      PARAMETER  ( SPD__COMP_C0 =  1.9898D-4  ) ! CIPM-2007
      PARAMETER  ( SPD__COMP_C1 = -2.376D-6   ) ! CIPM-2007
      PARAMETER  ( SPD__COMP_D0 =  1.83D-11   ) ! CIPM-2007
      PARAMETER  ( SPD__COMP_E0 = -0.765D-8   ) ! CIPM-2007
      REAL*8       SPD__CO2_CONC
      PARAMETER  ( SPD__CO2_CONC = 0.00375D0 ) 
      PARAMETER  ( SPD__H2O_DRY_RAT = SPD__H2O/(SPD__MA + SPD__CO2*(SPD__CO2_CONC - SPD__NOM_CO2)) )
!
      REAL*8     SPD__IR_K0, SPD__IR_K1, SPD__IR_K2, SPD__IR_K3
      REAL*8     SPD__IR_W0, SPD__IR_W1, SPD__IR_W2, SPD__IR_W3, SPD__CF
      PARAMETER  ( SPD__IR_K0 =  2.380185D+14 ) ! d/l
      PARAMETER  ( SPD__IR_K1 =  5.792105D+10 ) ! 1/m^2
      PARAMETER  ( SPD__IR_K2 =  5.7362D+13   ) ! 1/m^2
      PARAMETER  ( SPD__IR_K3 =  1.67917D+09  ) ! 1/m^2
      PARAMETER  ( SPD__CF    =  1.022D0      ) ! 1/m^2
!
      PARAMETER  ( SPD__IR_W0 =  2.95235D-6 ) ! d/l
      PARAMETER  ( SPD__IR_W1 =  2.6422D-20 ) ! 1/m^2
      PARAMETER  ( SPD__IR_W2 = -3.2380D-34 ) ! 1/m^4
      PARAMETER  ( SPD__IR_W3 =  4.028D-47  ) ! 1/m^6
      REAL*8     PD__REF, PW__REF, TEM__D_REF, TEM__W_REF
      PARAMETER  ( PD__REF = 101325.0D0 )
      PARAMETER  ( PW__REF =   1333.0D0 )
      PARAMETER  ( TEM__D_REF = 288.15D0 )
      PARAMETER  ( TEM__W_REF = 293.15D0 )
      REAL*8     SVP, F, ZD_REF, ZW_REF, RHOD_REF, RHOW_REF, RHO_D, RHO_W, &
     &           ZM, ZD, WN, NDS, NDW, C1, C2, D1, D2
!
      PARAMETER  ( D1 =  7.814735813D-7 ) ! 1064 nm
      PARAMETER  ( D2 = -1.060412830D-7 ) ! 1064 nm
!
      ZM = 1.D0/ &
     &           ( 1.0D0 &
     &             - P/TEM*         (SPD__COMP_A0 + SPD__COMP_A1*(TEM - SPD__ABS_TEMP) +  &
     &                                              SPD__COMP_A2*(TEM - SPD__ABS_TEMP)**2 ) &
     &             + PW/TEM*        (SPD__COMP_B0 + SPD__COMP_B1*(TEM - SPD__ABS_TEMP)) &
     &             + PW**2/(P*TEM)* (SPD__COMP_C0 + SPD__COMP_C1*(TEM - SPD__ABS_TEMP)) &
     &             + P**2/TEM**2*    SPD__COMP_D0 &
     &             + PW**2/TEM**2*   SPD__COMP_E0 &
     &           )
      REFRA_CIDDOR_IC1 = 1.D0 + (D1*P + D2*PW)/TEM*ZM
!
      RETURN
      END  FUNCTION   REFRA_CIDDOR_IC1  !#!#
!
! ------------------------------------------------------------------------
!
      FUNCTION   REFRA_CIDDOR_IC2 ( P, PW, TEM )
! ************************************************************************
! *                                                                      *
! *   Routine REFRA_CIDDOR_IC2
! *                                                                      *
! * ## 16-SEP-2013 REFRA_CIDDOR_IC2 v1.0 (c)  L. Petrov  16-SEP-2013 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      REAL*8     REFRA_CIDDOR_IC2
      REAL*8     P, PW, TEM
!
! --- Thermodynamic constants
!
      REAL*8     SPD__R, SPD__MA, SPD__H2O, SPD__CO2, SPD__NOM_CO2, &
     &           SPD__ABS_TEMP, SPD__H2O_DRY_RAT
      PARAMETER  ( SPD__R        = 8.314472D0    ) ! CIPM-2007 J/(K mol)
      PARAMETER  ( SPD__MA       = 0.02896546D0  ) ! CIPM-2007 kg/mol
      PARAMETER  ( SPD__H2O      = 0.01801528    ) ! CIPM-2007 kg/mol
      PARAMETER  ( SPD__CO2      = 0.012011D0    ) ! CIPM-2007 kg/mol
      PARAMETER  ( SPD__NOM_CO2  = 0.004D0       ) ! CIPM-2007
      PARAMETER  ( SPD__ABS_TEMP = 273.15D0      )
      REAL*8     SPD__ENH_ALPHA, SPD__ENH_BETA, SPD__ENH_GAMMA
      PARAMETER  ( SPD__ENH_ALPHA = 1.00062D0 )
      PARAMETER  ( SPD__ENH_BETA  = 3.14D-8   )
      PARAMETER  ( SPD__ENH_GAMMA = 5.6D-7    )
      REAL*8     SPD__PSV_A, SPD__PSV_B, SPD__PSV_C, SPD__PSV_D
      PARAMETER  ( SPD__PSV_A =  1.2378847D-5  ) ! CIPM-2007
      PARAMETER  ( SPD__PSV_B = -1.9121316D-2  ) ! CIPM-2007
      PARAMETER  ( SPD__PSV_C = 33.93711047D0  ) ! CIPM-2007
      PARAMETER  ( SPD__PSV_D = -6.3431645D3  ) ! CIPM-2007
      REAL*8     SPD__COMP_A0, SPD__COMP_A1, SPD__COMP_A2, SPD__COMP_B0, SPD__COMP_B1, &
     &           SPD__COMP_C0, SPD__COMP_C1, SPD__COMP_D0, SPD__COMP_E0
      PARAMETER  ( SPD__COMP_A0 =  1.58123D-6 ) ! CIPM-2007
      PARAMETER  ( SPD__COMP_A1 = -2.9331D-8  ) ! CIPM-2007
      PARAMETER  ( SPD__COMP_A2 =  1.1043D-10 ) ! CIPM-2007
      PARAMETER  ( SPD__COMP_B0 =  5.707D-6   ) ! CIPM-2007
      PARAMETER  ( SPD__COMP_B1 = -2.051D-8   ) ! CIPM-2007
      PARAMETER  ( SPD__COMP_C0 =  1.9898D-4  ) ! CIPM-2007
      PARAMETER  ( SPD__COMP_C1 = -2.376D-6   ) ! CIPM-2007
      PARAMETER  ( SPD__COMP_D0 =  1.83D-11   ) ! CIPM-2007
      PARAMETER  ( SPD__COMP_E0 = -0.765D-8   ) ! CIPM-2007
      REAL*8       SPD__CO2_CONC
      PARAMETER  ( SPD__CO2_CONC = 0.00375D0 ) 
      PARAMETER  ( SPD__H2O_DRY_RAT = SPD__H2O/(SPD__MA + SPD__CO2*(SPD__CO2_CONC - SPD__NOM_CO2)) )
!
      REAL*8     SPD__IR_K0, SPD__IR_K1, SPD__IR_K2, SPD__IR_K3
      REAL*8     SPD__IR_W0, SPD__IR_W1, SPD__IR_W2, SPD__IR_W3, SPD__CF
      PARAMETER  ( SPD__IR_K0 =  2.380185D+14 ) ! d/l
      PARAMETER  ( SPD__IR_K1 =  5.792105D+10 ) ! 1/m^2
      PARAMETER  ( SPD__IR_K2 =  5.7362D+13   ) ! 1/m^2
      PARAMETER  ( SPD__IR_K3 =  1.67917D+09  ) ! 1/m^2
      PARAMETER  ( SPD__CF    =  1.022D0      ) ! 1/m^2
!
      PARAMETER  ( SPD__IR_W0 =  2.95235D-6 ) ! d/l
      PARAMETER  ( SPD__IR_W1 =  2.6422D-20 ) ! 1/m^2
      PARAMETER  ( SPD__IR_W2 = -3.2380D-34 ) ! 1/m^4
      PARAMETER  ( SPD__IR_W3 =  4.028D-47  ) ! 1/m^6
      REAL*8     PD__REF, PW__REF, TEM__D_REF, TEM__W_REF
      PARAMETER  ( PD__REF = 101325.0D0 )
      PARAMETER  ( PW__REF =   1333.0D0 )
      PARAMETER  ( TEM__D_REF = 288.15D0 )
      PARAMETER  ( TEM__W_REF = 293.15D0 )
      REAL*8     SVP, F, ZD_REF, ZW_REF, RHOD_REF, RHOW_REF, RHO_D, RHO_W, &
     &           ZM, ZD, WN, NDS, NDW, C1, C2, D1, D2
      PARAMETER  ( D1 =  8.182229570D-7 ) !  532 nmw
      PARAMETER  ( D2 = -9.733136038D-8 ) !  532 nm
!
      ZM = 1.D0/ &
     &           ( 1.0D0 &
     &             - P/TEM*         (SPD__COMP_A0 + SPD__COMP_A1*(TEM - SPD__ABS_TEMP) +  &
     &                                              SPD__COMP_A2*(TEM - SPD__ABS_TEMP)**2 ) &
     &             + PW/TEM*        (SPD__COMP_B0 + SPD__COMP_B1*(TEM - SPD__ABS_TEMP)) &
     &             + PW**2/(P*TEM)* (SPD__COMP_C0 + SPD__COMP_C1*(TEM - SPD__ABS_TEMP)) &
     &             + P**2/TEM**2*    SPD__COMP_D0 &
     &             + PW**2/TEM**2*   SPD__COMP_E0 &
     &           )
      REFRA_CIDDOR_IC2 = 1.D0 + (D1*P + D2*PW)/TEM*ZM
!
      RETURN
      END  FUNCTION   REFRA_CIDDOR_IC2  !#!#
