      SUBROUTINE MALO_COMP_PPWTEM ( NTIM, HEB_DELP, HEB_T, HEB_Q, HEB_OH, &
     &                              MALO, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  MALO_COMP_PPWTEM 
! *                                                                      *
! * ### 12-JUN-2013  MALO_COMP_PPWTEM v1.0 (c) L. Petrov 13-MAR-2013 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'malo.i'
      INCLUDE   'heb.i'
      INTEGER*4  NTIM, IUER
      TYPE     ( MALO__TYPE ) :: MALO
      TYPE     ( HEB__TYPE  ) :: HEB_DELP(NTIM), HEB_T(NTIM), HEB_Q(NTIM), &
     &                           HEB_OH
      REAL*8     P(MALO__MHEI), PD(MALO__MHEI), PW(MALO__MHEI), &
     &           T(MALO__MHEI), HYPS(MALO__MHEI), &
     &           TMP(MALO__MHEI), H(MALO__MHEI), &
     &           GE, G, PHI, ZD(MALO__MHEI), ZW(MALO__MHEI), H0, &
     &           T_RATE, T_NLEV, LPL, LPH, PRES_R8(MALO__MHEI), &
     &           HEI_R8(MALO__MHEI), PRES_VAL, PW_VAL, TEMP, LOG_PRES, &
     &           Q(MALO__MHEI), LOGP_1D(MALO__MHEI), LOGPW_1D(MALO__MHEI), &
     &           TEM_1D(MALO__MHEI), SPL_H(MALO__MHEI), &
     &           SPL_P(MALO__MHEI), SPL_PW(MALO__MHEI), SPL_TEM(MALO__MHEI), &
     &           PRES_LEV, PW_LEV, GMR_DRY, GMR_WET
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
      REAL*8       SPD__R, SPD__MA, SPD__H2O
      PARAMETER  ( SPD__R        = 8.314472D0   ) ! CIPM-2007
      PARAMETER  ( SPD__MA       = 0.02896546D0 ) ! CIPM-2007
      PARAMETER  ( SPD__H2O      = 0.01801528D0 ) ! CIPM-2007
      INTEGER*4    M_LEV
      REAL*8       MD, MW
      PARAMETER  ( M_LEV = 64        )
      PARAMETER  ( MD    = MA__MAPL  )
      PARAMETER  ( MW    = H2O__MAPL )
      CHARACTER  STR*128, STR1*128, TEST_STR*8
      INTEGER*4  L_LON, L_LAT, L_LEV, L_TIM, IND, IND_LAST, &
     &           K_LEV, N_LEV, I_LON, DIMS(4), INDS(4), &
     &           J1, J2, J3, J4, J5, J6, J7, J8, J9, J10, J11, J12, J13, J14, &
     &           IT_LON, IT_LAT, IER
      REAL*4     Q_LEV(MALO__MHEI)
      REAL*4,    ALLOCATABLE  :: TD_LEV(:,:), TT_LEV(:,:)
      REAL*8     HARR(2), TARR(2), MIN_VAL, MAX_VAL , LON_TST, LAT_TST
  integer*4 is, ip ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  real*4, allocatable :: pres(:,:) ! %%%%%%%%%
  real*8  r1_min, r1_max, r2_min, r2_max, temp_val ! %%%%%
      INTEGER*4, EXTERNAL :: I_LEN, ILEN, IXMN8, IXMN8_S, IXMN4, IXMN4_S
      REAL*8,    EXTERNAL :: ISPL8, FSPL8, PW_TO_RELH
      REAL*4,    EXTERNAL :: VAL_3D_BSPL4, VAL_4D_BSPL4
      LOGICAL*4, EXTERNAL :: IS_R4_NAN
      CHARACTER, EXTERNAL :: MJDSEC_TO_DATE*30
!
      TEST_STR = 'none'  ! Supported values: none, timer, diff, pw, pres, temp, intrp, dewt
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
! --- Grid of the output Total pressure, partial pressure of water vapor and air temperatyre
!
      MALO%NLEV = MALO__MHEI
      MALO%NLON = L_LON+2
      MALO%NLAT = L_LAT
      MALO%NTIM = L_TIM
!@   write ( 6, * ) ' MALO%NLEV = ', MALO%NLEV, ' MALO%NLON = ', MALO%NLON, ' MALO%NLAT = ', MALO%NLAT, ' MALO%NTIM = ', MALO%NTIM ! %%%%
!@   write ( 6, * ) ' l_lev= ', l_lev, ' l_lon= ', l_lon, ' l_lat= ', l_lat, ' l_tim= ', l_tim ! %%%%%%%%%%%%%%%
      IF ( ASSOCIATED ( MALO%PPWTEM_4D ) ) DEALLOCATE ( MALO%PPWTEM_4D )
      IF ( L_TIM > 1 ) THEN
           ALLOCATE ( MALO%PPWTEM_4D(1-MALO__MDEG:MALO%NLEV,1-MALO__MDEG:MALO%NLON,1-MALO__MDEG:MALO%NLAT,1-MALO__MDEG:MALO%NTIM,3), &
     &                STAT=IER )
           MEL = INT8(MALO%NLEV+MALO__MDEG)*INT8(MALO%NLON+MALO__MDEG)*INT8(MALO%NLAT+MALO__MDEG)*INT8(MALO%NTIM+MALO__MDEG)*INT8(3)
         ELSE
           ALLOCATE ( MALO%PPWTEM_4D(1-MALO__MDEG:MALO%NLEV,1-MALO__MDEG:MALO%NLON,1-MALO__MDEG:MALO%NLAT,1,3), &
     &                STAT=IER )
           MEL = INT8(MALO%NLEV+MALO__MDEG)*INT8(MALO%NLON+MALO__MDEG)*INT8(MALO%NLAT+MALO__MDEG)*INT8(3)
      END IF
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH  ( STR )
           CALL IINCH8 ( INT8(4)*MEL, STR )
           CALL ERR_LOG ( 4811, IUER, 'MALO_COMP_PPWTEM', 'Failure in an attempt '// &
     &         'to allocate '//STR(1:I_LEN(STR))//' bytes of dynamic memory '// &
     &         'for array MALO%PPWTEM_4D' )
           RETURN 
      END IF
      MALO%PPWTEM_STATUS = MALO__ALLO
!
      IF ( TEST_STR == 'timer' ) THEN
           WRITE ( 6, '(A,I6,1X,I6)' ) 'MALO_COMP_PPWTEM NLON/NLAT = ', HEB_DELP(1)%DIMS(1), HEB_DELP(1)%DIMS(2)
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
      ALLOCATE ( MALO%LEV(MALO%NLEV), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( 4*MALO%NLEV, STR )
           CALL ERR_LOG ( 4812, IUER, 'MALO_COMP_PPWTEM', 'Failure in '// &
     &         'an attempt to allocate '//STR(1:I_LEN(STR))// &
     &         ' bytes of dynamic memory for array MALO%LEV' )
           RETURN 
      END IF
!
      ALLOCATE ( MALO%LAT(MALO%NLAT), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( 4*MALO%NLAT, STR )
           CALL ERR_LOG ( 4813, IUER, 'MALO_COMP_PPWTEM', 'Failure in '// &
     &         'an attempt to allocate '//STR(1:I_LEN(STR))// &
     &         ' bytes of dynamic memory for array MALO%LAT' )
           RETURN 
      END IF
!
      ALLOCATE ( MALO%LON(MALO%NLON), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( 4*MALO%NLON, STR )
           CALL ERR_LOG ( 4814, IUER, 'MALO_COMP_PPWTEM', 'Failure in '// &
     &         'an attempt to allocate '//STR(1:I_LEN(STR))// &
     &         ' bytes of dynamic memory for array MALO%LON' )
           RETURN 
      END IF
!
      ALLOCATE ( MALO%TIM(MALO%NTIM), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( 4*MALO%NTIM, STR )
           CALL ERR_LOG ( 4815, IUER, 'MALO_COMP_PPWTEM', 'Failure in '// &
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
            H0 = 100.0D0
            DO 440 J4=1,L_LON
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
               CALL ERR_PASS ( IUER, IER )
               Q_LEV(1:L_LEV) = HEB_Q(J2)%VAL(J4,J3,1:L_LEV,1)
               CALL CORRECT_SPEC_HUM ( L_LEV, Q_LEV, IER )
               IF ( IER .NE. 0 ) THEN
                    WRITE ( 6, * ) 'J4= ', J4, ' J3= ', J3
                    CALL ERR_LOG ( 4816, IUER, 'MALO_COMP_PPWTEM', 'Error '// &
     &                  'in an attempt to correct specific humidity' )
                    RETURN 
               END IF
               HEB_Q(J2)%VAL(J4,J3,1:L_LEV,1) = Q_LEV(1:L_LEV) 
!
! ------------ Re-indexing pressure to start have 0-th level at the nominal surface
!
               DO 450 J5=1,L_LEV
                  K_LEV = L_LEV+1-J5
                  P(K_LEV) = P(K_LEV+1) + HEB_DELP(J2)%VAL(J4,J3,J5,1)
                  IF ( P(K_LEV) < PRES_HIGH ) N_LEV = K_LEV
!
                  ZD(K_LEV) = 1.0D0
                  ZW(K_LEV) = 1.0D0
!
! --------------- Compute approximate gravity acceleration at a given pressure level
! --------------- using a regression formulae for the US Standard Atmosphere
!
                  G = GE*(1.D0 + GP0 + GP1*DLOG(P(K_LEV)))
!
! --------------- PW -- parital pressure of water vapour
!
                  PW(K_LEV)   = HEB_Q(J2)%VAL(J4,J3,J5,1)/(HEB_Q(J2)%VAL(J4,J3,J5,1) + MW/MD)* &
     &                          P(K_LEV)
!
! --------------- PD -- Partial pressure of dry air
!
                  PD(K_LEV)   = P(K_LEV) - PW(K_LEV)
                  T(K_LEV)    = HEB_T(J2)%VAL(J4,J3,J5,1)
                  Q(K_LEV)    = HEB_Q(J2)%VAL(J4,J3,J5,1)
!
! --------------- Compute function HYPS that js the right hand-side of 
! --------------- the hypsometric equation, while the left handsize is h(P)
!
                  HYPS(K_LEV) = R__MAPL*T(K_LEV)/ &
     &                         (G*(ZD(K_LEV)*MD*PD(K_LEV) + ZW(K_LEV)*MW*PW(K_LEV)))
 450           CONTINUE 
!
! ------------ Special trick for making interpolation: we reverse the sign of pressure.
! ------------ Otherwise, MAKE_SPLINE will complain the argument is not in rising order
!
               P = -P ! For interpolation
               CALL ERR_PASS ( IUER, IER ) 
               CALL MAKE_SPLINE ( 3, L_LEV, P, HYPS, 0.0D0, 0.0D0, SPL_H, TMP, IER )
               IF ( IER .NE. 0 ) THEN
                    CALL ERR_LOG ( 4817, IUER, 'MALO_COMP_PPWTEM', 'Failure in an attempt '// &
     &                   'to compute coefficients of the interpolating spline' )
                    RETURN 
               END IF
!
! ------------ Compute height at pressure grid by integration the spline that 
! ------------ interpolates HYPS
!
               H(1) = HEB_OH%VAL(J4,J3,1,1) ! Boundary condition at the surface
               DO 460 J6=2,L_LEV
                  IF ( J6 < L_LEV ) THEN
                       H(J6) = H(J6-1) + ISPL8 ( P(J6), L_LEV, P, HYPS, &
     &                                           J6-1, J6, SPL_H, IER )
                     ELSE IF ( J6 == L_LEV ) THEN
                       H(J6) = H(J6-1) + ISPL8 ( P(J6)*(1.D0-EPS), L_LEV, P, HYPS, &
     &                                           J6-1, J6, SPL_H, IER )
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
               DO 470 J7=1,L_LEV
!
! --------------- We compute gravity at a given height
!
                  G = GE*(1.D0 - 2.D0/REA__WGS84*(1.0D0 + OMEGA__EGM96**2*REA__WGS84**2* &
     &                   (1.D0 - FLAT__WGS84)/GM__EGM96 + (1.D0 - 2.D0*FLAT__WGS84)* &
     &                    DSIN(PHI)**2)*H(J7) + 3.D0/REA__WGS84**2*H(J7)**2)
!
! --------------- Compute improved function for integration
!
                  HYPS(J7) = R__MAPL*T(J7)/(G*(ZD(J7)*MD*PD(J7) + ZW(J7)*MW*PW(J7)))
 470           CONTINUE 
!
! ------------ Compute interpolating spline of HYPS(P) the second time
!
               CALL ERR_PASS ( IUER, IER ) 
               CALL MAKE_SPLINE ( 3, L_LEV, P, HYPS, 0.0D0, 0.0D0, SPL_H, TMP, IER )
               IF ( IER .NE. 0 ) THEN
                    CALL ERR_LOG ( 4818, IUER, 'MALO_COMP_PPWTEM', 'Failure in '// &
     &                  'an attempt to compute coefficients of the '// &
     &                  'interpolating spline' )
                    RETURN 
               END IF
!
! ------------ Compute height as a function of pressure the second time
!
               H(1) = HEB_OH%VAL(J4,J3,1,1) ! Boundary condition
               LOGP_1D(1)  = DLOG(-P(1))
               LOGPW_1D(1) = DLOG(PW(1))
               TEM_1D(1)   = T(1)
               DO 480 J8=2,L_LEV
                  IF ( J8 < L_LEV ) THEN
                       H(J8) = H(J8-1) + ISPL8 ( P(J8), L_LEV, P, HYPS, &
     &                                           J8-1, J8, SPL_H, IER )
                     ELSE IF ( J8 == L_LEV ) THEN
                       H(J8) = H(J8-1) + ISPL8 ( P(J8)*(1.D0-EPS), L_LEV, P, HYPS, &
     &                                           J8-1, J8, SPL_H, IER )
                  END IF
                  LOGP_1D(J8)  = DLOG(-P(J8))
                  LOGPW_1D(J8) = DLOG(PW(J8))
                  TEM_1D(J8)   = T(J8)
 480           CONTINUE 
!
! ------------ Now we compute interpolation spline of the total pressure as 
! ------------ a function of height
!
               CALL ERR_PASS ( IUER, IER ) 
               CALL MAKE_SPLINE ( 3, L_LEV, H, LOGP_1D, 0.0D0, 0.0D0, &
     &                            SPL_P, TMP, IER )
               IF ( IER .NE. 0 ) THEN
                    WRITE ( 6, * ) 'J4(lon) = ', INT2(J4), ' J3(lat) = ', INT2(J3)
                    WRITE ( 6, * ) 'L_LEV   = ', L_LEV + 1
                    WRITE ( 6, * ) 'H       = ', SNGL(H(1:L_LEV))
                    WRITE ( 6, * ) 'P_1D    = ', SNGL(LOGP_1D(1:L_LEV))
                    CALL ERR_LOG ( 4819, IUER, 'MALO_COMP_PPWTEM', 'Failure in an attempt '// &
     &                   'to compute coefficients of the interpolating spline' )
                    RETURN 
               END IF
!
! ------------ Now we compute the interpolation spline of water vapor partial 
! ------------ pressure as a function of height
!
               CALL ERR_PASS ( IUER, IER ) 
               CALL MAKE_SPLINE ( 3, L_LEV, H, LOGPW_1D, 0.0D0, 0.0D0, &
     &                            SPL_PW, TMP, IER )
               IF ( IER .NE. 0 ) THEN
                    WRITE ( 6, * ) 'J4(lon) = ', INT2(J4), ' J3(lat) = ', INT2(J3)
                    WRITE ( 6, * ) 'L_LEV   = ', L_LEV + 1
                    WRITE ( 6, * ) 'H       = ', SNGL(H(1:L_LEV))
                    WRITE ( 6, * ) 'PW_1D   = ', SNGL(LOGPW_1D(1:L_LEV))
                    CALL ERR_LOG ( 4820, IUER, 'MALO_COMP_PPWTEM', 'Failure in an attempt '// &
     &                   'to compute coefficients of the interpolating spline' )
                    RETURN 
               END IF
!
! ------------ Now we compute interpolation spline of air temperature as 
! ------------ a function of height
!
               CALL ERR_PASS ( IUER, IER ) 
               CALL MAKE_SPLINE ( 3, L_LEV, H, TEM_1D, 0.0D0, 0.0D0, &
     &                            SPL_TEM, TMP, IER )
               IF ( IER .NE. 0 ) THEN
                    WRITE ( 6, * ) 'J4(lon) = ', INT2(J4), ' J3(lat) = ', INT2(J3)
                    WRITE ( 6, * ) 'L_LEV   = ', L_LEV + 1
                    WRITE ( 6, * ) 'H       = ', SNGL(H(1:L_LEV))
                    WRITE ( 6, * ) 'TEM_1D  = ', SNGL(TEM_1D(1:L_LEV))
                    CALL ERR_LOG ( 4821, IUER, 'MALO_COMP_PPWTEM', 'Failure in an attempt '// &
     &                   'to compute coefficients of the interpolating spline' )
                    RETURN 
               END IF
!
! ------------ Now we compute total pressure, water vapour pressure and air temperature
! ------------ on a global grid
!
               IND_LAST = 1
               DO 490 J9=1,MALO__MHEI
                  MALO%LEV(J9) = J9-1 - (MALO__MHEI-1)/2
                  HEI_R4(J9) = DEXP ( (MALO%LEV(J9) - SPD__U3_GMAO72)/SPD__U1_GMAO72 ) - SPD__U2_GMAO72
                  G = GE*(1.D0 - 2.D0/REA__WGS84*(1.0D0 + OMEGA__EGM96**2*REA__WGS84**2* &
     &                   (1.D0 - FLAT__WGS84)/GM__EGM96 + (1.D0 - 2.D0*FLAT__WGS84)* &
     &                    DSIN(PHI)**2)*HEI_R4(J9) + 3.D0/REA__WGS84**2*HEI_R4(J9)**2)
                  IF ( HEI_R4(J9) < H(1) ) THEN
!
! -------------------- This height level is below the surface. We assume the atmosphere
! -------------------- beneath the surface obeys to the adiabatic gas law with a constant 
! -------------------- lapse rate computed at the previous step
!
                       MALO%PPWTEM_4D(J9,I_LON,J3,J2,MALO__TEM) =  T(1) + T_RATE*(HEI_R4(J9) - H(1))
                       MALO%PPWTEM_4D(J9,I_LON,J3,J2,MALO__P)   = -P(1) *(MALO%PPWTEM_4D(J9,I_LON,J3,J2,MALO__TEM)/T(1))**(-G*MD/(R__MAPL*T_RATE))
                       MALO%PPWTEM_4D(J9,I_LON,J3,J2,MALO__PW)  =  PW(1)*(MALO%PPWTEM_4D(J9,I_LON,J3,J2,MALO__TEM)/T(1))**(-G*MW/(R__MAPL*T_RATE))
                     ELSE IF ( HEI_R4(J9) > H(L_LEV) ) THEN
!
! -------------------- This is above the upper level. We consider the atmosphere isothermal.
!
                       GMR_DRY = G*SPD__MA/SPD__R        
                       GMR_WET = G*SPD__MA/SPD__R        
                       MALO%PPWTEM_4D(J9,I_LON,J3,J2,MALO__P)   = -P(L_LEV)*DEXP ( -GMR_DRY*(HEI_R4(J9) - H(L_LEV))/T(L_LEV) )
                       MALO%PPWTEM_4D(J9,I_LON,J3,J2,MALO__PW)  = PW(L_LEV)*DEXP ( -GMR_WET*(HEI_R4(J9) - H(L_LEV))/T(L_LEV) )
                       MALO%PPWTEM_4D(J9,I_LON,J3,J2,MALO__TEM) = MALO%PPWTEM_4D(L_LEV,I_LON,J3,J2,MALO__TEM)
                     ELSE
!
! -------------------- Compute air temperature by spline interpolation
!
                       IND = IXMN8 ( L_LEV, H, DBLE(HEI_R4(J9)) )
                       MALO%PPWTEM_4D(J9,I_LON,J3,J2,MALO__P)   = DEXP ( FSPL8 ( DBLE(HEI_R4(J9)), L_LEV, H, LOGP_1D,   IND, SPL_P  ) )
                       MALO%PPWTEM_4D(J9,I_LON,J3,J2,MALO__PW)  = DEXP ( FSPL8 ( DBLE(HEI_R4(J9)), L_LEV, H, LOGPW_1D,  IND, SPL_PW ) )
                       MALO%PPWTEM_4D(J9,I_LON,J3,J2,MALO__TEM) = FSPL8 ( DBLE(HEI_R4(J9)), L_LEV, H, TEM_1D, IND, SPL_TEM )
                  END IF
 490           CONTINUE 
 440        CONTINUE 
!
! --------- We extend longitude coverage to three pixel eastward.
! --------- The pixel with longitude index L_LON+a is equal to the pixel with index a
!
            DO 4100 J10=1,2
               MALO%PPWTEM_4D(1-MALO__MDEG:MALO__MHEI,L_LON+J10,J3,J2,MALO__P)   = MALO%PPWTEM_4D(1-MALO__MDEG:MALO__MHEI,J10,J3,J2,MALO__P) 
               MALO%PPWTEM_4D(1-MALO__MDEG:MALO__MHEI,L_LON+J10,J3,J2,MALO__PW)  = MALO%PPWTEM_4D(1-MALO__MDEG:MALO__MHEI,J10,J3,J2,MALO__PW) 
               MALO%PPWTEM_4D(1-MALO__MDEG:MALO__MHEI,L_LON+J10,J3,J2,MALO__TEM) = MALO%PPWTEM_4D(1-MALO__MDEG:MALO__MHEI,J10,J3,J2,MALO__TEM) 
 4100       CONTINUE 
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
      DO 4110 J11=1,MALO%NLON
         MALO%LON(J11) = (J11-1)*PI2/L_LON
 4110 CONTINUE 
!
      DO 4120 J12=1,MALO%NLAT
         MALO%LAT(J12) = -P2I + (J12-1)*PI__NUM/(L_LAT-1)
 4120 CONTINUE 
      MALO%PPWTEM_STATUS = MALO__LOAD
!
      IF ( TEST_STR == 'timer' ) THEN
           CALL WALL_TIMER ( STR )
           WRITE ( 6, '(A)' ) 'Collecting three functions:    '//STR(1:I_LEN(STR)-5)
           CALL WALL_TIMER ( %VAL(0) )
      END IF
!
! --- Compute coefficients of 3D interpolating B-splines
!
      DIMS(1) = MALO%NLEV
      DIMS(2) = MALO%NLON
      DIMS(3) = MALO%NLAT
      DIMS(4) = MALO%NTIM
!
      DO 4130 J13=1,3
         IF ( NTIM > 1 ) THEN
              CALL ERR_PASS ( IUER, IER )
              CALL BSPL4_4D_CMP ( MALO__MDEG, 0, DIMS, MALO%LEV, MALO%LON, MALO%LAT, MALO%TIM, &
     &                            MALO%PPWTEM_4D(1-MALO__MDEG,1-MALO__MDEG,1-MALO__MDEG,1-MALO__MDEG,J13), &
     &                            IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 4822, IUER, 'MALO_COMP_PPWTEM', 'Failure in '// &
     &                 'an attempt to compute coefficients of the 3D '// &
     &                 'interpolating spline' )
                   RETURN 
              END IF
            ELSE IF ( NTIM == 1 ) THEN
              CALL ERR_PASS ( IUER, IER )
              CALL BSPL4_3D_CMP ( MALO__MDEG, 0, DIMS, MALO%LEV, MALO%LON, MALO%LAT, &
     &                            MALO%PPWTEM_4D(1-MALO__MDEG,1-MALO__MDEG,1-MALO__MDEG,1,J13), &
     &                            IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 4823, IUER, 'MALO_COMP_PPWTEM', 'Failure in '// &
     &                 'an attempt to compute coefficients of the 3D '// &
     &                 'interpolating spline' )
                   RETURN 
              END IF
         END IF
 4130 CONTINUE 
      IF ( TEST_STR == 'timer' ) THEN
           CALL WALL_TIMER ( STR )
           WRITE ( 6, '(A)' ) 'Computation of 3D B-splines:   '//STR(1:I_LEN(STR)-5)
           CALL WALL_TIMER ( %VAL(0) )
      END IF
      MALO%PPWTEM_STATUS = MALO__COMP
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!@      r2_min = minval ( malo%ppwtem_4d(1-malo__mdeg:malo__mhei, &
!@     &                                               1-malo__mdeg:malo%nlon+1, &
!@     &                                               1-malo__mdeg:malo%nlat,   &
!@     &                                               1-malo__mdeg:malo%ntim,ip) )
!@      r2_max = maxval ( malo%ppwtem_4d(1-malo__mdeg:malo__mhei, &
!@     &                                               1-malo__mdeg:malo%nlon+1, &
!@     &                                               1-malo__mdeg:malo%nlat,   &
!@     &                                               1-malo__mdeg:malo%ntim,ip) )
!@      ip = malo__pw
!@      str1 = mjdsec_to_date ( malo%mjd_beg, malo%tai_beg + malo%tim(4), -2 )
!@      WRITE ( 6, * ) 'Epoch: '//STR1(1:24)
!@      allocate ( pres(l_lon,l_lat) )
!@ 720  continue 
!@      write ( 6, * ) 'PW-spline Enter layer' 
!@      read ( 5, '(i4)', iostat=ier ) is
!@      if ( is < 1 .or. is > dims(1) ) goto 820
!@      do 610 j1=1,malo%ntim
!@         do 620 j2=1,malo%nlat
!@            do 630 j3=1,malo%nlon
!@                do 640 j4=1,malo__mhei
!@!                   if ( malo%ppwtem_4d(j4,j3,j2,j1,ip) > 60.0 ) then
!@!                        write ( 6, * ) 'inds= ', int2(j4), int2(j3), int2(j2), int2(j1), ' val= ', malo%ppwtem_4d(j4,j3,j2,j1,ip) ! %%%%%%%
!@!                   end if
!@                   if ( j1 == 4 .and. j4 == is ) then
!@                        args(1) = malo%lev(j4)
!@                        args(2) = malo%lon(j3) 
!@                        args(3) = malo%lat(j2)
!@                        args(4) = malo%tim(j1)
!@                        inds(1) = j4
!@                        inds(2) = j3
!@                        inds(3) = j2
!@                        inds(4) = j1
!@                        pres_val = val_4d_bspl4 ( args, malo__mdeg, dims, inds, &
!@     &                                         malo%lev, malo%lon, malo%lat, malo%tim, &
!@     &                                         malo%ppwtem_4d(1-malo__mdeg,1-malo__mdeg,1-malo__mdeg,1-malo__mdeg,malo__p) )
!@                        pw_val   = val_4d_bspl4 ( args, malo__mdeg, dims, inds, &
!@     &                                         malo%lev, malo%lon, malo%lat, malo%tim, &
!@     &                                         malo%ppwtem_4d(1-malo__mdeg,1-malo__mdeg,1-malo__mdeg,1-malo__mdeg,malo__pw) )
!@                        temp_val = val_4d_bspl4 ( args, malo__mdeg, dims, inds, &
!@     &                                         malo%lev, malo%lon, malo%lat, malo%tim, &
!@     &                                         malo%ppwtem_4d(1-malo__mdeg,1-malo__mdeg,1-malo__mdeg,1-malo__mdeg,malo__tem) )
!@!!
!@!                        pres(j3,j2) = pres_val
!@!                        pres_val = exp ( pres_val )
!@!                        pres(j3,j2) = log10 ( pres_val/temp_val * spd__h2o/spd__r )
!@!!!
!@                        pres(j3,j2) = log10 ( pw_to_relh ( dble(pw_val), dble(temp_val) ) )
!@                   endif
!@ 640            continue 
!@ 630        continue 
!@ 620     continue 
!@ 610  continue 
!@      call clrch ( str )
!@      write ( unit=str(1:10), fmt='(f10.0)' ) hei_r4(is) 
!@      call chashl ( str )
!@!      call plot_grid_r4 ( 1, 7, 0, 1, malo%nlon, malo%nlat, pres, &
!@!     &                    'Lg(water vapor density kg/m**3) at height '//str(1:i_len(str)), &
!@!     &                    'lg(rho)', '/tmp/boo', ier )
!@      call plot_grid_r4 ( 1, 7, 0, 1, malo%nlon, malo%nlat, pres, &
!@     &                    'Lg(rel humid) at height '//str(1:i_len(str))//' on '//str1(1:16), &
!@     &                    'lg(rhel)', '/tmp/boo', ier )
!@      goto 720
!@ 820  continue 
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  MALO_COMP_PPWTEM  !#!#
!
! ------------------------------------------------------------------------
!
      FUNCTION   PW_TO_RELH ( PW, TEM )
      IMPLICIT   NONE 
      REAL*8     PW_TO_RELH 
      REAL*8     PW, TEM
!
! --- Coefficients of expension of the ln(P_sat_vw) following the 
! --- Smithsonian Tables, 1984, after Goff and Gratch, 1946
!
      INTEGER*4  L_GG, N$1
      PARAMETER  ( L_GG = 12 )
      REAL*8     T_MIN, T_MAX, GG_LEG_COEF(0:L_GG)
      PARAMETER  ( T_MIN = 153.0D0 )
      PARAMETER  ( T_MAX = 353.0D0 )
      DATA (GG_LEG_COEF(N$1), N$1=0,L_GG) &
     & / &
     &    4.6153120D+00, & !  0
     &    8.1286881D+00, & !  1
     &   -2.0759868D+00, & !  2
     &    5.6933927D-01, & !  3
     &   -1.9143749D-01, & !  4
     &    7.6444822D-02, & !  5
     &   -3.1940023D-02, & !  6
     &    1.2845264D-02, & !  7
     &   -4.8023028D-03, & !  8
     &    1.6537891D-03, & !  9
     &   -5.2386559D-04, & ! 10
     &    1.5442392D-04, & ! 11
     &   -4.1882817D-05  & ! 12
     & /
      REAL*8     PRES_SAT
      REAL*8,    EXTERNAL :: LEGENDRE_VAL
      PRES_SAT = DEXP ( LEGENDRE_VAL ( L_GG, T_MIN, T_MAX, TEM, GG_LEG_COEF ) )
      PW_TO_RELH = PW/PRES_SAT
      RETURN
      END  FUNCTION  PW_TO_RELH  !#!#
