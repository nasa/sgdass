      SUBROUTINE MALO_COMP_SPR ( HEB_DELP, HEB_T, HEB_Q, HEB_G, HEB_OH, &
     &                           MAL, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine MALO_COMP_SPR computes surface pressure for the grid       *
! *   of orthoheights specifed in HEB_OH object using parameters of      *
! *   4D numerical models outputs: HEB_DELP -- layer thickness,          *
! *   HEB_T -- air temperature, HEB_Q -- specific humidity,              *
! *   HEB_G -- surface geopotential. Results are written in object MAL.  *
! *                                                                      *
! * ### 15-FEB-2013  MALO_COMP_SPR v2.1 (c)  L. Petrov  20-APR-2014 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'malo.i'
      INCLUDE   'heb.i'
      TYPE     ( HEB__TYPE  ) :: HEB_DELP, HEB_T, HEB_Q, HEB_G, HEB_OH
      TYPE     ( MALO__TYPE ) :: MAL
      INTEGER*4  IUER
      REAL*4,    ALLOCATABLE  :: PRS(:,:,:), LAT_R4(:), LON_R4(:)
      REAL*8     P(0:MALO__MHEI+1), PD(MALO__MHEI), PW(MALO__MHEI), &
     &           T(MALO__MHEI), HYPS_VAL(0:MALO__MHEI), HYPS_SPL(MALO__MHEI+1), &
     &           TMP(MALO__MHEI+2), H(0:MALO__MHEI), &
     &           GE, G, PHI, ZM(MALO__MHEI), Q0, &
     &           T_RATE, T_NLEV, LPL, LPH, LOGP_VAL(MALO__MHEI), &
     &           LOGP_SPL(MALO__MHEI), PRES_R8(MALO__MHEI), &
     &           HEI_R8(MALO__MHEI), PRES_VAL, TEMP, LOG_PRES
      REAL*4     HEI_R4(MALO__MHEI), LAT_VAL, LON_VAL, ARGS(3)
      REAL*8     EPS, PRES_HIGH, HEI_MIN, HEI_MIN_INT, HEI_MAX, PRES_LN_0, PRES_LN_RATE
      PARAMETER  ( PRES_HIGH    = 25000.0D0 )
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
      INTEGER*4  L_LON, L_LAT, IND, K_LEV, L_LEV, N_LEV, I_LON, DIMS(3), &
     &           J1, J2, J3, J4, J5, J6, J7, J8, J9, J10, J11, J12, &
     &           INDS(3), IT_LON, IT_LAT, IND_LON, IND_LAT, IER
      REAL*4,    ALLOCATABLE  :: PRS_SUR(:,:)
      REAL*8     HARR(2), TARR(2), MIN_VAL, MAX_VAL 
      INTEGER*4, EXTERNAL :: I_LEN, ILEN, IXMN8, IXMN4, IXMN4_S
      REAL*4,    EXTERNAL :: VAL_3D_BSPL4 
      REAL*8,    EXTERNAL :: ISPL8, FSPL8
      CHARACTER, EXTERNAL :: MJDSEC_TO_DATE*30
!
      TEST_STR = 'info'  ! Supported values: none, timer, diff, spr, pres, temp, intrp
!!      TEST_STR = 'pres'  ! Supported values: none, timer, diff, spr, pres, temp, intrp
!!      TEST_STR = 'timer'  ! Supported values: none, timer, diff, spr, pres, temp, intrp
!!      TEST_STR = 'p0'  ! Supported values: none, timer, diff, spr, pres, temp, intrp
!!      IT_LON = 46; IT_LAT = 301 ! Sankt-Peterburg for Merra grid
      IT_LON = 415; IT_LAT = 189 ! Sankt-Peterburg for Merra grid
!
! --- Grid of the input numerical models
!
      L_LON = HEB_DELP%DIMS(1)
      L_LAT = HEB_DELP%DIMS(2)
      L_LEV = HEB_DELP%DIMS(3)
!
! --- Grid of the output surface pressure field
!
      MAL%NLON = HEB_OH%DIMS(1)
      MAL%NLAT = HEB_OH%DIMS(2)
      MAL%NTIM = 1
      IF ( ASSOCIATED ( MAL%SPR ) ) THEN
           DEALLOCATE ( MAL%SPR )
      END IF
!
      ALLOCATE ( MAL%SPR(MAL%NLON,MAL%NLAT,1), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( 4*MAL%NLON*MAL%NLAT, STR )
           CALL ERR_LOG ( 4611, IUER, 'MALO_COMP_SPR', 'Failure in an attempt '// &
     &         'to allocate '//STR(1:I_LEN(STR))//' bytes of dynamic memory '// &
     &         'for array MAL%SPR' )
           RETURN 
      END IF
!
      IF ( TEST_STR == 'timer' ) THEN
           WRITE ( 6, * ) ' MAL%NLON/MAL%NLAT = ', MAL%NLON, MAL%NLAT
           CALL WALL_TIMER ( %VAL(0) ) 
      END IF
      IF ( TEST_STR == 'p0' ) THEN
           MAL%SPR = -9999.0
      END IF
      LPL = PRES_LN_0 + PRES_LN_RATE*HEI_MIN
      LPH = PRES_LN_0 + PRES_LN_RATE*HEI_MAX
      DO 410 J1=1,M_LEV
         LOG_PRES = LPL + (J1-1)*(LPH - LPL)/(M_LEV-1)
         HEI_R8(J1)  = -PRES_LN_0/PRES_LN_RATE + LOG_PRES/PRES_LN_RATE
         HEI_R4(J1)  = HEI_R8(J1)
 410  CONTINUE 
!
      ALLOCATE ( PRS(1-MALO__MDEG:M_LEV,1-MALO__MDEG:L_LON+1,1-MALO__MDEG:L_LAT), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( 4*(M_LEV+MALO__MDEG)*(L_LON+1+MALO__MDEG)*(L_LAT+MALO__MDEG), STR )
           CALL ERR_LOG ( 4612, IUER, 'MALO_COMP_SPR', 'Failure in an attempt '// &
     &         'to allocate '//STR(1:I_LEN(STR))//' bytes of dynamic memory '// &
     &         'for array PRS' )
           RETURN 
      END IF
      IF ( TEST_STR == 'diff' .OR. TEST_STR == 'spr' .OR. TEST_STR == 'intrp' ) THEN
           ALLOCATE ( PRS_SUR(L_LON,L_LAT) )
      END IF
!
      ALLOCATE ( LAT_R4(L_LAT), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( 4*L_LAT, STR )
           CALL ERR_LOG ( 4612, IUER, 'MALO_COMP_SPR', 'Failure in an attempt '// &
     &         'to allocate '//STR(1:I_LEN(STR))//' bytes of dynamic memory '// &
     &         'for array LAT_R4' )
           RETURN 
      END IF
!
      ALLOCATE ( LON_R4(L_LON+1), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( 4*L_LON, STR )
           CALL ERR_LOG ( 4613, IUER, 'MALO_COMP_SPR', 'Failure in an attempt '// &
     &         'to allocate '//STR(1:I_LEN(STR))//' bytes of dynamic memory '// &
     &         'for array LON_R4' )
           RETURN 
      END IF
!
      PRS    = 0.0
      LAT_R4 = 0.0
      LON_R4 = 0.0
!
      DO 420 J2=1,L_LAT
!
! ------ PHI -- is geodetic latitude
!
         PHI = -P2I + (J2-1)*PI__NUM/(L_LAT-1)
!
! ------ Compute gravity on the reference ellipsoid for a given geodetic latitude PHI
!
         GE = ACC_EQU__WGS84*(1.D0 + GRV_LAT__WGS84*DSIN(PHI)**2)/ &
     &          DSQRT(1.D0 - (2.D0*FLAT__WGS84 - FLAT__WGS84**2)*DSIN(PHI)**2 )
         DO 430 J3=1,L_LON
!
! --------- NB: Merra/Geos57 longitude starts from -180deg
!
            I_LON = J3 + L_LON/2
            IF ( I_LON > L_LON ) I_LON = I_LON - L_LON
!
! --------- First run for computing height above the ellipsoid 
! --------- for a given pressure level, the middle point of a layer
!
            P(L_LEV+1) = 1.0D0
            N_LEV = L_LEV
            DO 440 J4=1,L_LEV
               K_LEV = L_LEV + 1 - J4
               IF ( J4 == 1 ) THEN
                    P(K_LEV) = P(K_LEV+1) + HEB_DELP%VAL(J3,J2,J4,1)/2.0D0
                  ELSE 
                    P(K_LEV) = P(K_LEV+1) + HEB_DELP%VAL(J3,J2,J4-1,1)/2.0D0 + &
     &                                      HEB_DELP%VAL(J3,J2,J4,1)/2.0D0 
               END IF
               IF ( P(K_LEV) < PRES_HIGH ) N_LEV = K_LEV
!
! ------------ Compute approximate gravity acceleration at a given pressure level
!
               G = GE*(1.D0 + GP0 + GP1*DLOG(P(K_LEV)))
!
! ------------ PW -- parital pressure of water vapour
!
               PW(K_LEV)   = P(K_LEV) * HEB_Q%VAL(J3,J2,J4,1) / &
     &                                ( MW/MD + (1.0D0 - MW/MD)*HEB_Q%VAL(J3,J2,J4,1) )
!
! ------------ Partial pressure of dry air
!
               PD(K_LEV)   = P(K_LEV) - PW(K_LEV)
               T(K_LEV)    = HEB_T%VAL(J3,J2,J4,1)
!
! ------------ Compute water vapor compressibility
!
!@               ZM(K_LEV) = 1.D0/ &
!@     &              ( 1.0D0 &
!@     &                - P(K_LEV)/T(K_LEV)*  (SPD__COMP_A0 + SPD__COMP_A1*(T(K_LEV) - SPD__ABS_TEMP) +  &
!@     &                                                      SPD__COMP_A2*(T(K_LEV) - SPD__ABS_TEMP)**2 ) &
!@     &                + PW(K_LEV)/T(K_LEV)* (SPD__COMP_B0 + SPD__COMP_B1*(T(K_LEV) - SPD__ABS_TEMP)) &
!@     &                + PW(K_LEV)**2/(P(K_LEV)*T(K_LEV))* &
!@     &                                      (SPD__COMP_C0 + SPD__COMP_C1*(T(K_LEV) - SPD__ABS_TEMP)) &
!@     &                + P(K_LEV)**2/T(K_LEV)**2*  SPD__COMP_D0 &
!@     &                + PW(K_LEV)**2/T(K_LEV)**2* SPD__COMP_E0 &
!@     &              )
!
! ------------ ... mmm. no, let it keep equl to 1 as it was in GEOS
!
               ZM(K_LEV) = 1.0D0
               HYPS_VAL(K_LEV) = R__MAPL*T(K_LEV)/ &
     &                          (G*(MD*PD(K_LEV) + MW*PW(K_LEV))*ZM(K_LEV))
 440        CONTINUE 
            P(0) = P(1) + HEB_DELP%VAL(J3,J2,L_LEV,1)/2.0D0
            HYPS_VAL(0) = HYPS_VAL(1) - (P(1) - P(0))*(HYPS_VAL(2) - HYPS_VAL(1))/(P(2) - P(1))
!
            IF ( TEST_STR == 'p0' ) THEN
                 LAT_VAL = PHI
                 LON_VAL = (J3-1)*PI2/L_LON
                 IND_LAT = IDINT( (LAT_VAL + P2I)/PI__NUM*(HEB_OH%DIMS(2)-1) ) + 1
                 IND_LON = IDINT( LON_VAL/PI2*HEB_OH%DIMS(1) ) + 1 + HEB_OH%DIMS(1)/2
                 IF ( IND_LON > HEB_OH%DIMS(1) ) IND_LON = IND_LON - HEB_OH%DIMS(1)
                 MAL%SPR(IND_LON,IND_LAT,1) = P(1)
            END IF 
!
! --------- Special trick for making interpolation: we reverse the sign of pressure.
! --------- Otherwise, MAKE_SPLINE will complain that 
! --------- the array of arguments is not in rising order
!
            P = -P ! For interpolation
            CALL ERR_PASS ( IUER, IER ) 
            CALL MAKE_SPLINE ( 3, L_LEV+1, P, HYPS_VAL, 0.0D0, 0.0D0, HYPS_SPL, TMP, IER )
            IF ( IER .NE. 0 ) THEN
                 CALL ERR_LOG ( 4614, IUER, 'MALO_COMP_SPR', 'Failure in an attempt '// &
     &                'to compute coefficients of the interpolating spline' )
                 RETURN 
            END IF
!
! --------- Compute height at pressure levels by integration of the spline
!
            H(0) = HEB_G%VAL(J3,J2,1,1) ! height of the surface above the geoid
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
            CALL REGR8 ( N_LEV - N_LEV/2 + 1, H(N_LEV/2), T(N_LEV/2), &
     &                   T_RATE, T_NLEV, IER )
            IF ( TEST_STR == 'temp'  .AND.  I_LON == IT_LON  .AND.  J2 == IT_LAT ) THEN
                 HARR(1) = H(1)
                 HARR(2) = H(N_LEV)
                 TARR(1) = T_NLEV + T_RATE*(H(1) - H(N_LEV/2))
                 TARR(2) = T_NLEV + T_RATE*(H(N_LEV) - H(N_LEV/2))
                 WRITE ( 6, * ) 'Test: Lat= ', SNGL((-P2I + (IT_LAT-1)*PI__NUM/(L_LAT-1))/DEG__TO__RAD), &
     &                          'Lon= ', SNGL((IT_LON-1)*PI2/L_LON/DEG__TO__RAD)
                 CALL DIAGI_2 ( N_LEV, H, T, 2, HARR, TARR, IER )
            END IF
!
! --------- Second round
!
            DO 460 J6=1,L_LEV
!
! ------------ We compute gravity at a given height
!
               G = GE*(1.D0 - 2.D0/REA__WGS84*(1.0D0 + OMEGA__EGM96**2*REA__WGS84**3* &
     &                (1.D0 - FLAT__WGS84)/GM__EGM96 + (1.D0 - 2.D0*FLAT__WGS84)* &
     &                 DSIN(PHI)**2)*H(J6) + 3.D0/REA__WGS84**2*H(J6)**2)
!
! ------------ Compute improved function for integration
!
               HYPS_VAL(J6) = R__MAPL*T(J6)/(G*(MD*PD(J6) + MW*PW(J6))*ZM(J6))
 460        CONTINUE 
            HYPS_VAL(0) = HYPS_VAL(1) - (P(1) - P(0))*(HYPS_VAL(2) - HYPS_VAL(1))/(P(2) - P(1))
!
! --------- Compute interpolating spline of H(P) the second time
!
            CALL ERR_PASS ( IUER, IER ) 
            CALL MAKE_SPLINE ( 3, L_LEV+1, P, HYPS_VAL, 0.0D0, 0.0D0, HYPS_SPL, TMP, IER )
            IF ( IER .NE. 0 ) THEN
                 CALL ERR_LOG ( 4615, IUER, 'MALO_COMP_SPR', 'Failure in an attempt '// &
     &                'to compute coefficients of the interpolating spline' )
                 RETURN 
            END IF
!
! --------- Compute height as a function of pressure the second time.
! --------- Also store ln(P)
!
            H(0) = HEB_G%VAL(J3,J2,1,1) ! Height of the surface above the geoid
            DO 470 J7=1,L_LEV
               IF ( J7 < L_LEV ) THEN
                    H(J7) = H(J7-1) + ISPL8 ( P(J7), L_LEV+1, P, HYPS_VAL, &
     &                                        J7, J7+1, HYPS_SPL, IER )
                  ELSE IF ( J5 == L_LEV ) THEN
                    H(J7) = H(J7-1) + ISPL8 ( P(J7)*(1.D0-EPS), L_LEV+1, P, HYPS_VAL, &
     &                                        J7, J7+1, HYPS_SPL, IER )
               END IF
               LOGP_VAL(J7) = LOG(-P(J7))
 470        CONTINUE 
            IF ( TEST_STR == 'pres'  .AND.  I_LON == IT_LON  .AND.  J2 == IT_LAT ) THEN
                 WRITE ( 6, * ) 'Test1: Lat= ', SNGL((-P2I + (IT_LAT-1)*PI__NUM/(L_LAT-1))/DEG__TO__RAD), &
     &                          'Lon= ', SNGL((IT_LON-1)*PI2/L_LON/DEG__TO__RAD)
                 DO 570 J7=1,L_LEV
                    WRITE ( 6, 210 ) J7, T(J7), H(J7), -P(J7), HEB_DELP%VAL(J3,J2,L_LEV+1-J7,1)
 210                FORMAT ( 'J2= ', I3, ' T= ', F10.5, ' H = ', F12.5, ' Pres: ', F10.3, &
     &                       ' DELP= ', F12.5 )
 570             CONTINUE 
                 WRITE ( 6, * ) 'PHIS = ', HEB_G%VAL(J3,J2,1,1), ' PS = ', P(0)
                 CALL DIAGI_1 ( L_LEV+1, H, P, -2 ) 
            END IF
!
! --------- Now we compute interpolation spline for the opposite problem:
! --------- we would like to model dependence of ln(P) as a function of geometrical
! --------- height above the ellipsoid
!
            CALL ERR_PASS ( IUER, IER ) 
            CALL MAKE_SPLINE ( 3, L_LEV, H(1), LOGP_VAL, 0.0D0, 0.0D0, LOGP_SPL, TMP, IER )
            IF ( IER .NE. 0 ) THEN
                 CALL ERR_LOG ( 4616, IUER, 'MALO_COMP_SPR', 'Failure in an attempt '// &
     &                'to compute coefficients of the interpolating spline' )
                 RETURN 
            END IF
!
            IF ( TEST_STR == 'intrp' ) THEN
                 IND = IXMN8 ( N_LEV, H, DBLE(HEB_G%VAL(J3,J2,1,1))  )
                 PRS_SUR(J3,J2) = EXP ( FSPL8 ( DBLE(HEB_G%VAL(J3,J2,1,1)), L_LEV, &
     &                                          H(1), LOGP_VAL, IND, LOGP_SPL ) ) + P(1)
            END IF
!
! --------- Now we compute logarithm of pressure as a function of height on
! --------- another global grid
!
            DO 480 J8=1,M_LEV
               G = GE*(1.D0 - 2.D0/REA__WGS84*(1.0D0 + OMEGA__EGM96**2*REA__WGS84**2* &
     &                (1.D0 - FLAT__WGS84)/GM__EGM96 + (1.D0 - 2.D0*FLAT__WGS84)* &
     &                 DSIN(PHI)**2)*HEI_R8(J8) + 3.D0/REA__WGS84**2*HEI_R8(J8)**2)
               IF ( HEI_R8(J8) < H(1) ) THEN
!
! ----------------- This height level is below the surface. We assume the atmosphere
! ----------------- beneath the surface obeys to the adiabatic gas law with a constant 
! ----------------- lapse rate computed at the previous step
!
                    TEMP = T(1) + T_RATE*(HEI_R8(J8) - H(1))
                    PRS(J8,I_LON,J2) = LOG(-P(1)*(TEMP/T(1))**(-G*MD/(R__MAPL*T_RATE)))
                  ELSE
!
! ----------------- Compute the logarithm of the atmospheric pressure by
! ----------------- spline interpolation
!
                    IND = IXMN8 ( L_LEV, H(1), HEI_R8(J8) )
                    PRS(J8,I_LON,J2) = FSPL8 ( HEI_R8(J8), L_LEV, H(1), LOGP_VAL, IND, LOGP_SPL )
               END IF
               IF ( TEST_STR == 'pres'  .AND.  I_LON == IT_LON  .AND.  J2 == IT_LAT ) THEN
                    PRES_R8(J8) = EXP ( PRS(J8,I_LON,J2) )
                    ZM(J8) = PRS(J8,I_LON,J2)
               END IF
               IF ( TEST_STR == 'intrp' ) THEN
                    PRES_R8(J8) = PRS(J8,I_LON,J2)
               END IF
 480        CONTINUE 
!
            IF ( TEST_STR == 'pres'  .AND.  I_LON == IT_LON  .AND.  J2 == IT_LAT ) THEN
                 WRITE ( 6, * ) 'Test2: Lat= ', SNGL((-P2I + (IT_LAT-1)*PI__NUM/(L_LAT-1))/DEG__TO__RAD), &
     &                          'Lon= ', SNGL((IT_LON-1)*PI2/L_LON/DEG__TO__RAD)
                 CALL DIAGI_2 ( M_LEV, HEI_R8, PRES_R8, N_LEV, H, -P, IER )
                 WRITE ( 6, * ) 'Test3: Lat= ', SNGL((-P2I + (IT_LAT-1)*PI__NUM/(L_LAT-1))/DEG__TO__RAD), &
     &                          'Lon= ', SNGL((IT_LON-1)*PI2/L_LON/DEG__TO__RAD)
                 CALL DIAGI_2 ( M_LEV, HEI_R8, ZM(1), N_LEV, H, LOGP_VAL, IER )
            END IF
 430     CONTINUE 
!
! ------ We extend longitude coverage to one pixel eastward.
! ------ The pixel with longitude index L_LON+1 is equal to the pixel with index 1
!
         PRS(1-MALO__MDEG:M_LEV,L_LON+1,J2) = PRS(1-MALO__MDEG:M_LEV,1,J2) 
 420  CONTINUE 
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
      CALL ERR_PASS ( IUER, IER )
      CALL BSPL4_3D_CMP ( MALO__MDEG, 0, DIMS, HEI_R4, LON_R4, LAT_R4, &
     &                    PRS, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 4617, IUER, 'MALO_COMP_SPR', 'Failure in an attempt '// &
     &         'to compute coefficients of the 3D interpolating spline' )
           RETURN 
      END IF
      IF ( TEST_STR == 'timer' ) THEN
           CALL WALL_TIMER ( STR )
           WRITE ( 6, '(A)' ) 'Computation of 3D B-spline:       '//STR(1:I_LEN(STR)-5)
           CALL WALL_TIMER ( %VAL(0) )
      END IF
!
      MAL%MJD_BEG = HEB_DELP%MJD
      MAL%TAI_BEG = HEB_DELP%TAI
      INDS(3) = 1
!
! --- Now compute surface pressure by 3D interpolation 
!
      DO 4110 J11=1,MAL%NLAT
         LAT_VAL = -P2I + (J11-1)*PI__NUM/(MAL%NLAT-1)
         INDS(3) = IXMN4_S ( INDS(3), L_LAT, LAT_R4, LAT_VAL )
         IF ( LAT_VAL .LE. LAT_R4(1)     ) INDS(3) = 1
         IF ( LAT_VAL .GE. LAT_R4(L_LAT) ) INDS(3) = L_LAT-1
         INDS(2) = 1
         DO 4120 J12=1,MAL%NLON
            IF ( TEST_STR == 'p0' ) GOTO 4110
            I_LON = J12 + L_LON/2
            IF ( I_LON > L_LON ) I_LON = I_LON - L_LON
            LON_VAL = (J12-1)*PI2/MAL%NLON
            INDS(2) = IXMN4_S ( INDS(2), L_LON+1, LON_R4, LON_VAL )
            IF ( LON_VAL .LE. LON_R4(1)       ) INDS(2) = 1
            IF ( LON_VAL .GE. LON_R4(L_LON+1) ) INDS(2) = L_LON
            IF ( TEST_STR == 'intrp' ) THEN
                 HEB_OH%VAL(J12,J11,1,1) = HEB_G%VAL(I_LON,J11,1,1)
            END IF
            INDS(1) = IXMN4   ( M_LEV, HEI_R4, HEB_OH%VAL(J12,J11,1,1) )
            ARGS(1) = HEB_OH%VAL(J12,J11,1,1)
            ARGS(2) = LON_VAL
            ARGS(3) = LAT_VAL
            MAL%SPR(J12,J11,1) = EXP ( VAL_3D_BSPL4 ( ARGS, MALO__MDEG, DIMS, INDS, &
     &                                                HEI_R4, LON_R4, LAT_R4, PRS ) )
            IF ( TEST_STR == 'diff' ) THEN
                 MAL%SPR(J12,J11,1) = MAL%SPR(J12,J11,1) - PRS_SUR(I_LON,J11)
               ELSE IF ( TEST_STR == 'spr' .OR. TEST_STR == 'intrp' ) THEN
                 MAL%SPR(J12,J11,1) = PRS_SUR(I_LON,J11)
            END IF
 4120    CONTINUE 
 4110 CONTINUE 
      DEALLOCATE ( PRS ) 
      DEALLOCATE ( LAT_R4 ) 
      DEALLOCATE ( LON_R4 ) 
      IF ( TEST_STR == 'timer' ) THEN
           CALL WALL_TIMER ( STR ) 
           WRITE ( 6, '(A)' ) 'Perform 3D B-spline intrpolation: '//STR(1:I_LEN(STR)-5)
         ELSE IF ( TEST_STR == 'diff'  .OR.  TEST_STR == 'spr'  .OR.  TEST_STR == 'intrp' ) THEN
           DEALLOCATE ( PRS_SUR )
      END IF
      IF ( TEST_STR .NE. 'none' ) THEN
           STR = MJDSEC_TO_DATE ( HEB_DELP%MJD, HEB_DELP%UTC, -2 )
           WRITE ( 6, '(A)' ) 'Finished processing SPR for '//STR(1:19)
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  MALO_COMP_SPR  !#!#
