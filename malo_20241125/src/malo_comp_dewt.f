      SUBROUTINE MALO_COMP_DEWT ( HEI_DEW_POINT, HEB_DELP, HEB_T, HEB_Q, &
     &                            HEB_G, HEB_OH, HEB_DEWT, &
     &                            HEB_ST, HEB_SPR, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  MALO_COMP_DEWT 
! *                                                                      *
! * ### 13-MAR-2013  MALO_COMP_DEWT  v2.0 (c)  L. Petrov 13-MAR-2013 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'malo.i'
      INCLUDE   'heb.i'
      REAL*8     HEI_DEW_POINT
      TYPE     ( HEB__TYPE  ) :: HEB_DELP, HEB_T, HEB_Q, HEB_G, HEB_OH, &
     &                           HEB_DEWT, HEB_ST, HEB_SPR
      INTEGER*4  IUER
      REAL*4,    ALLOCATABLE  :: TT_3D(:,:,:), TD_3D(:,:,:), LAT_R4(:), LON_R4(:)
      REAL*8     P(0:MALO__MHEI+1), PD(0:MALO__MHEI+1), PW(0:MALO__MHEI+1), &
     &           T(0:MALO__MHEI), FUN1(0:MALO__MHEI), FUN2(0:MALO__MHEI), &
     &           TMP(0:MALO__MHEI), H(0:MALO__MHEI), &
     &           GE, G, PHI, ZD(0:MALO__MHEI), ZW(0:MALO__MHEI), H0, &
     &           T_RATE, T_NLEV, LPL, LPH, PRES_R8(MALO__MHEI), &
     &           HEI_R8(MALO__MHEI), PRES_VAL, TEMP, LOG_PRES, &
     &           Q(0:MALO__MHEI), LOGQ(0:MALO__MHEI), TD_1D(0:MALO__MHEI), &
     &           TT_1D(0:MALO__MHEI), SPL_H(0:MALO__MHEI), &
     &           SPL_TT(0:MALO__MHEI), SPL_TD(0:MALO__MHEI), PRES_LEV, PW_LEV
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
      INTEGER*4  L_LON, L_LAT, L_LEV, M_LON, M_LAT, IND, &
     &           K_LEV, N_LEV, I_LON, DIMS(3), &
     &           J1, J2, J3, J4, J5, J6, J7, J8, J9, J10, J11, J12, &
     &           INDS(3), IT_LON, IT_LAT, IER
      REAL*4,    ALLOCATABLE  :: TD_LEV(:,:), TT_LEV(:,:)
      REAL*4     VAL_MIN, VAL_MAX
      REAL*8     HARR(2), TARR(2), MIN_VAL, MAX_VAL , LON_TST, LAT_TST
      REAL*8     TEMP_K, A1_DEW, B1_DEW, C1_DEW, EPS_PW
      PARAMETER  ( TEMP_K = 271.16D0 )
      PARAMETER  ( A1_DEW = 17.625D0 )
      PARAMETER  ( B1_DEW = 243.04D0 )
      PARAMETER  ( C1_DEW = 610.94D0 )
      PARAMETER  ( EPS_PW = 1.D-10   )
      INTEGER*4, EXTERNAL :: I_LEN, ILEN, IXMN8, IXMN4, IXMN4_S
      REAL*4,    EXTERNAL :: VAL_3D_BSPL4 
      REAL*8,    EXTERNAL :: ISPL8, FSPL8
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
! --- Grid of the output dew point temperature 
!
      HEB_DEWT%DIMS = HEB_OH%DIMS
      M_LON = HEB_OH%DIMS(1)
      M_LAT = HEB_OH%DIMS(2)
      IF ( ASSOCIATED ( HEB_DEWT%VAL ) ) DEALLOCATE ( HEB_DEWT%VAL )
      ALLOCATE ( HEB_DEWT%VAL(HEB_DEWT%DIMS(1),HEB_DEWT%DIMS(2),HEB_DEWT%DIMS(3),HEB_DEWT%DIMS(4)), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH  ( STR )
           CALL IINCH8 ( INT8(4)*HEB_DEWT%DIMS(1),HEB_DEWT%DIMS(2),HEB_DEWT%DIMS(3),HEB_DEWT%DIMS(4), STR )
           CALL ERR_LOG ( 4711, IUER, 'MALO_COMP_DEWT', 'Failure in an attempt '// &
     &         'to allocate '//STR(1:I_LEN(STR))//' bytes of dynamic memory '// &
     &         'for array MAL%SPR' )
           RETURN 
      END IF
!
! --- Grid of the output air temperature 
!
      HEB_ST%DIMS = HEB_OH%DIMS
      M_LON = HEB_OH%DIMS(1)
      M_LAT = HEB_OH%DIMS(2)
      IF ( ASSOCIATED ( HEB_ST%VAL ) ) DEALLOCATE ( HEB_ST%VAL )
      ALLOCATE ( HEB_ST%VAL(HEB_ST%DIMS(1),HEB_ST%DIMS(2),HEB_DEWT%DIMS(3),HEB_ST%DIMS(4)), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH  ( STR )
           CALL IINCH8 ( INT8(4)*HEB_ST%DIMS(1),HEB_ST%DIMS(2),HEB_ST%DIMS(3),HEB_ST%DIMS(4), STR )
           CALL ERR_LOG ( 4712, IUER, 'MALO_COMP_DEWT', 'Failure in an attempt '// &
     &         'to allocate '//STR(1:I_LEN(STR))//' bytes of dynamic memory '// &
     &         'for array MAL%SPR' )
           RETURN 
      END IF
!
      IF ( TEST_STR == 'timer' ) THEN
           WRITE ( 6, '(A,I6,1X,I6)' ) 'NLON/NLAT = ', HEB_ST%DIMS(1), HEB_ST%DIMS(2)
           CALL WALL_TIMER ( %VAL(0) ) 
      END IF
      LPL = PRES_LN_0 + PRES_LN_RATE*HEI_MIN
      LPH = PRES_LN_0 + PRES_LN_RATE*HEI_MAX
      DO 410 J1=1,M_LEV
         LOG_PRES = LPL + (J1-1)*(LPH - LPL)/(M_LEV-1)
         HEI_R8(J1)  = -PRES_LN_0/PRES_LN_RATE + LOG_PRES/PRES_LN_RATE
         HEI_R4(J1)  = HEI_R8(J1)
 410  CONTINUE 
!
      ALLOCATE ( TD_3D(1-MALO__MDEG:M_LEV,1-MALO__MDEG:L_LON+1,1-MALO__MDEG:L_LAT), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( 4*(M_LEV+MALO__MDEG)*(L_LON+1+MALO__MDEG)*(L_LAT+MALO__MDEG), STR )
           CALL ERR_LOG ( 4713, IUER, 'MALO_COMP_DEWT', 'Failure in an attempt '// &
     &         'to allocate '//STR(1:I_LEN(STR))//' bytes of dynamic memory '// &
     &         'for array TD_3D' )
           RETURN 
      END IF
!
      ALLOCATE ( TT_3D(1-MALO__MDEG:M_LEV,1-MALO__MDEG:L_LON+1,1-MALO__MDEG:L_LAT), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( 4*(M_LEV+MALO__MDEG)*(L_LON+1+MALO__MDEG)*(L_LAT+MALO__MDEG), STR )
           CALL ERR_LOG ( 4713, IUER, 'MALO_COMP_DEWT', 'Failure in an attempt '// &
     &         'to allocate '//STR(1:I_LEN(STR))//' bytes of dynamic memory '// &
     &         'for array TT_3D' )
           RETURN 
      END IF
      IF ( TEST_STR == 'diff' .OR. TEST_STR == 'intrp' .OR. TEST_STR == 'gen' ) THEN
           ALLOCATE ( TD_LEV(L_LON,L_LAT) )
      END IF
!
      ALLOCATE ( LAT_R4(L_LAT), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( 4*L_LAT, STR )
           CALL ERR_LOG ( 4714, IUER, 'MALO_COMP_DEWT', 'Failure in '// &
     &         'an attempt to allocate '//STR(1:I_LEN(STR))// &
     &         ' bytes of dynamic memory for array LAT_R4' )
           RETURN 
      END IF
!
      ALLOCATE ( LON_R4(L_LON+1), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( 4*L_LON, STR )
           CALL ERR_LOG ( 4715, IUER, 'MALO_COMP_DEWT', 'Failure in '// &
     &         'an attempt to allocate '//STR(1:I_LEN(STR))// &
     &         ' bytes of dynamic memory for array LON_R4' )
           RETURN 
      END IF
!
      TD_3D  = 0.0
      LAT_R4 = 0.0
      LON_R4 = 0.0
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
         H0 = 100.0D0
         DO 430 J3=1,L_LON
!
! --------- NB: Merra/Geos57 longitude starts from -180deg
!
            I_LON = J3 + L_LON/2
            IF ( I_LON > L_LON ) I_LON = I_LON - L_LON
!
! --------- First run for computing height above the ellipsoid for a given pressure level
!
            P(L_LEV+1) = 1.0D0
            N_LEV = L_LEV
!
            CALL  ERR_PASS ( IUER, IER )
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!  if ( j3 == 221  .and. j2 == 87 ) ier = 666 ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!  if ( j3 == 221  .and. j2 == 87 ) write ( 6, * ) 'GG before ier=', ier ! %%%%%%%%%%%%%%%
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
            CALL CORRECT_SPEC_HUM ( L_LEV, HEB_Q%VAL(J3,J2,1:L_LEV,1), IER )
            IF ( IER .NE. 0 ) THEN
                 WRITE ( 6, * ) 'J3= ', J3, ' J2= ', J2
                 CALL ERR_LOG ( 4716, IUER, 'MALO_COMP_DEWT', 'Error '// &
     &               'in an attempt to correct specific humidity' )
                 RETURN 
            END IF
!
            DO 440 J4=1,L_LEV
               K_LEV = L_LEV+1-J4
               IF ( J4 == 1 ) THEN
                    P(K_LEV) = P(K_LEV+1) + HEB_DELP%VAL(J3,J2,J4,1)/2.D0
                  ELSE 
                    P(K_LEV) = P(K_LEV+1) + HEB_DELP%VAL(J3,J2,J4,1)
               END IF
               IF ( P(K_LEV) < PRES_HIGH ) N_LEV = K_LEV
!
               ZD(K_LEV) = 1.0D0
               ZW(K_LEV) = 1.0D0
!
! ------------ Compute approximate gravity acceleration at a given pressure level
!
               G = GE*(1.D0 + GP0 + GP1*DLOG(P(K_LEV)))
!
! ------------ PW -- parital pressure of water vapour
!
               PW(K_LEV)   = HEB_Q%VAL(J3,J2,J4,1)/(HEB_Q%VAL(J3,J2,J4,1) + MW/MD)* &
     &                       P(K_LEV)
!
! ------------ Partial pressure of dry air
!
               PD(K_LEV)   = P(K_LEV) - PW(K_LEV)
               T(K_LEV)    = HEB_T%VAL(J3,J2,J4,1)
               Q(K_LEV)    = HEB_Q%VAL(J3,J2,J4,1)
               FUN1(K_LEV) = R__MAPL*T(K_LEV)/ &
     &                      (G*(ZD(K_LEV)*MD*PD(K_LEV) + ZW(K_LEV)*MW*PW(K_LEV)))
 440        CONTINUE 
!
! --------- Now we compute pressure and other parameters at the surface
!
            P(0) = P(1) + HEB_DELP%VAL(J3,J2,L_LEV,1)/2.D0
            T(0) =  HEB_T%VAL(J3,J2,L_LEV,1) - &
     &           (HEB_T%VAL(J3,J2,L_LEV-1,1) - HEB_T%VAL(J3,J2,L_LEV,1))/2.0D0
!
            Q(0) = DEXP( DLOG(Q(1)) - ( DLOG(Q(2)) - DLOG(Q(1)) )/2.0D0 )
            Q(0) = Q(1)
!
            ZD(0) = 1.0D0
            ZW(0) = 1.0D0
!
            G = GE*(1.D0 + GP0 + GP1*DLOG(P(0)))
            PW(0) = DEXP( DLOG(PW(1)) - ( DLOG(PW(2)) - DLOG(PW(1)) )/2.0D0 )
            PD(0) = P(0) - PW(0)
            FUN1(0) = R__MAPL*T(0)/(G*(ZD(0)*MD*PD(0) + ZW(0)*MW*PW(0)))
!
! --------- Special trick for making interpolation: we reverse the sign of pressure.
! --------- Otherwise, MAKE_SPLINE will complain the argument is not in rising order
!
            P = -P ! For interpolation
            CALL ERR_PASS ( IUER, IER ) 
            CALL MAKE_SPLINE ( 3, N_LEV+1, P, FUN1, 0.0D0, 0.0D0, SPL_H, TMP, IER )
            IF ( IER .NE. 0 ) THEN
                 CALL ERR_LOG ( 4717, IUER, 'MALO_COMP_DEWT', 'Failure in an attempt '// &
     &                'to compute coefficients of the interpolating spline' )
                 RETURN 
            END IF
!
! --------- Compute height at pressure levels by integration of the spline
!
            H(0) = HEB_G%VAL(J3,J2,1,1) 
            DO 450 J5=0,N_LEV
               IF ( J5 < N_LEV ) THEN
                    H(J5) = HEB_G%VAL(J3,J2,1,1) + ISPL8 ( P(J5), N_LEV+1, P, FUN1, &
     &                                                     1, IND, SPL_H, IER )
                  ELSE 
                    H(J5) = HEB_G%VAL(J3,J2,1,1) + ISPL8 ( P(J5)*(1.D0+EPS), N_LEV+1, P, FUN1, &
     &                                                     1, IND, SPL_H, IER )
               END IF
 450        CONTINUE 
!
! --------- Compute the lapse rate between N_LEV/2 and N_LEV
!
            CALL ERR_PASS ( IUER, IER )
            CALL REGR8 ( N_LEV - N_LEV/2 + 1, H(N_LEV/2), T(N_LEV/2), T_RATE, T_NLEV, IER )
            IF ( TEST_STR == 'temp'  .AND.  I_LON == IT_LON  .AND.  J2 == IT_LAT ) THEN
                 HARR(1) = H(0)
                 HARR(2) = H(N_LEV)
                 TARR(1) = T_NLEV + T_RATE*(H(0) - H(N_LEV/2))
                 TARR(2) = T_NLEV + T_RATE*(H(N_LEV) - H(N_LEV/2))
                 WRITE ( 6, * ) 'Test: Lat= ', SNGL((-P2I + (IT_LAT-1)*PI__NUM/(L_LAT-1))/DEG__TO__RAD), &
     &                          'Lon= ', SNGL((IT_LON-1)*PI2/L_LON/DEG__TO__RAD), &
     &                          ' IT_LON= ', IT_LON, ' IT_LAT= ', IT_LAT, &
     &                          ' J2= ', J2, ' J3= ', J3
                 CALL DIAGI_2 ( N_LEV+1, H, T, 2, HARR, TARR, IER )
            END IF
!
! --------- Second round
!
            DO 460 J6=0,N_LEV
!
! ------------ We compute gravity at a given height
!
               G = GE*(1.D0 - 2.D0/REA__WGS84*(1.0D0 + OMEGA__EGM96**2*REA__WGS84**2* &
     &                (1.D0 - FLAT__WGS84)/GM__EGM96 + (1.D0 - 2.D0*FLAT__WGS84)* &
     &                 DSIN(PHI)**2)*H(J6) + 3.D0/REA__WGS84**2*H(J6)**2)
!
! ------------ Compute improved function for integration
!
               FUN2(J6) = R__MAPL*T(J6)/(G*(ZD(J6)*MD*PD(J6) + ZW(J6)*MW*PW(J6)))
 460        CONTINUE 
!
! --------- Compute interpolating spline of H(P) the second time
!
            CALL ERR_PASS ( IUER, IER ) 
            CALL MAKE_SPLINE ( 3, N_LEV+1, P, FUN2, 0.0D0, 0.0D0, SPL_H, TMP, IER )
            IF ( IER .NE. 0 ) THEN
                 CALL ERR_LOG ( 4718, IUER, 'MALO_COMP_DEWT', 'Failure in '// &
     &               'an attempt to compute coefficients of the '// &
     &               'interpolating spline' )
                 RETURN 
            END IF
!
! --------- Compute height as a function of pressure the second time.
!
            DO 470 J7=0,N_LEV
               IF ( J7 == 0 ) THEN
                    H(0) = HEB_G%VAL(J3,J2,1,1) 
                  ELSE IF ( J7 == N_LEV ) THEN
                    H(J7) = H(J7-1) + ISPL8 ( P(J7)*(1.D0-EPS), N_LEV+1, P, FUN1, &
     &                                        J7, J7+1, SPL_H, IER )
                  ELSE IF ( J7 < N_LEV ) THEN
                    H(J7) = H(J7-1) + ISPL8 ( P(J7), N_LEV+1, P, FUN1, &
     &                                        J7, J7+1, SPL_H, IER )
               END IF
               LOGQ(J7) = LOG(Q(J7))
               TD_1D(J7) = TEMP_K + B1_DEW*DLOG((PW(J7)+EPS_PW)/C1_DEW)/ &
     &                             (A1_DEW - DLOG((PW(J7)+EPS_PW)/C1_DEW))
               TT_1D(J7) = T(J7)
               IF ( TEST_STR == 'temp'  .AND.  I_LON == IT_LON  .AND.  J2 == IT_LAT ) THEN
                    WRITE ( 6, * ) 'J7= ', INT2(J7), ' H= ', SNGL(H(J7)), ' PW= ', SNGL(PW(J7))
               END IF 
 470        CONTINUE 
            IF ( TEST_STR == 'dewt'  .AND.  I_LON == IT_LON  .AND.  J2 == IT_LAT ) THEN
                 WRITE ( 6, * ) 'Test1: Lat= ', SNGL((-P2I + (IT_LAT-1)*PI__NUM/(L_LAT-1))/DEG__TO__RAD), &
     &                          'Lon= ', SNGL((IT_LON-1)*PI2/L_LON/DEG__TO__RAD)
                 CALL DIAGI_1 ( N_LEV+1, H, LOGQ, -2 ) 
                 CALL DIAGI_1 ( N_LEV+1, H, PW, -2 ) 
                 CALL DIAGI_1 ( N_LEV+1, H, TD_1D, -2 ) 
            END IF
!
! --------- Now we compute interpolation spline of dew point temperature as 
! --------- a function of height
!
            CALL ERR_PASS ( IUER, IER ) 
            CALL MAKE_SPLINE ( 3, N_LEV+1, H, TD_1D, 0.0D0, 0.0D0, &
     &                         SPL_TD, TMP, IER )
            IF ( IER .NE. 0 ) THEN
                 WRITE ( 6, * ) 'J3(lon) = ', INT2(J3), ' J2(lat) = ', INT2(J2)
                 WRITE ( 6, * ) 'N_LEV= ', N_LEV + 1
                 WRITE ( 6, * ) 'H = ', SNGL(H(0:N_LEV))
                 WRITE ( 6, * ) 'Q = ', HEB_Q%VAL(J3,J2,1:L_LEV,1)
                 CALL ERR_LOG ( 4719, IUER, 'MALO_COMP_DEWT', 'Failure in an attempt '// &
     &                'to compute coefficients of the interpolating spline' )
                 RETURN 
            END IF
!
! --------- Now we compute interpolation spline of air temperature as 
! --------- a function of height
!
            CALL ERR_PASS ( IUER, IER ) 
            CALL MAKE_SPLINE ( 3, N_LEV+1, H, TT_1D, 0.0D0, 0.0D0, &
     &                         SPL_TT, TMP, IER )
            IF ( IER .NE. 0 ) THEN
                 WRITE ( 6, * ) 'J3(lon) = ', INT2(J3), ' J2(lat) = ', INT2(J2)
                 WRITE ( 6, * ) 'N_LEV= ', N_LEV + 1
                 WRITE ( 6, * ) 'H = ', SNGL(H(0:N_LEV))
                 WRITE ( 6, * ) 'Q = ', HEB_Q%VAL(J3,J2,1:L_LEV,1)
                 CALL ERR_LOG ( 4720, IUER, 'MALO_COMP_DEWT', 'Failure in an attempt '// &
     &                'to compute coefficients of the interpolating spline' )
                 RETURN 
            END IF
!
            IF ( TEST_STR == 'intrp' ) THEN
                 IND = IXMN8 ( N_LEV+1, H, DBLE(HEB_G%VAL(J3,J2,1,1))  )
                 TD_LEV(J3,J2) = EXP ( FSPL8 ( DBLE(HEB_G%VAL(J3,J2,1,1)), N_LEV+1, &
     &                                         H, TD_1D, IND, SPL_TD ) )
            END IF
!
! --------- Now we compute due point temperature at the another global grid
!
            DO 480 J8=1,M_LEV
               G = GE*(1.D0 - 2.D0/REA__WGS84*(1.0D0 + OMEGA__EGM96**2*REA__WGS84**2* &
     &                (1.D0 - FLAT__WGS84)/GM__EGM96 + (1.D0 - 2.D0*FLAT__WGS84)* &
     &                 DSIN(PHI)**2)*HEI_R8(J8) + 3.D0/REA__WGS84**2*HEI_R8(J8)**2)
               IF ( HEI_R8(J8) < H(0) ) THEN
!
! ----------------- This height level is below the surface. We assume the atmosphere
! ----------------- beneath the surface objeys to the adiabatic gas law with a constant 
! ----------------- lapse rate computed at the previous step
!
                    TT_3D(J8,I_LON,J2) = T(0) + T_RATE*(HEI_R8(J8) - H(0))
                    PRES_LEV = -P(0)*(TT_3D(J8,I_LON,J2)/T(0))**(-G*MD/(R__MAPL*T_RATE))
                    PW_LEV = Q(0)/(Q(0) + MW/MD)*PRES_LEV
                    TD_3D(J8,I_LON,J2) = TEMP_K + B1_DEW*DLOG((PW_LEV+EPS_PW)/C1_DEW)/ &
     &                                           (A1_DEW - DLOG((PW_LEV+EPS_PW)/C1_DEW))
                    IF ( TEST_STR == 'temp'  .AND.  I_LON == IT_LON  .AND.  J2 == IT_LAT ) THEN
                         write ( 6, * ) ' j8= ', j8, ' temp = ',temp, ' pres_lev= ', pres_lev, ' pw_lev= ', pw_lev, &
     &                                  ' je= ', hei_r8(j8), ' t3 =', TD_3D(J8,I_LON,J2) 
                    END IF 
                  ELSE
!
! ----------------- Compute the dew point temperature by spline interpolation
!
                    IND = IXMN8 ( N_LEV+1, H, HEI_R8(J8) )
                    TT_3D(J8,I_LON,J2) = FSPL8 ( HEI_R8(J8), N_LEV+1, H, TT_1D, IND, SPL_TT )
                    TD_3D(J8,I_LON,J2) = FSPL8 ( HEI_R8(J8), N_LEV+1, H, TD_1D, IND, SPL_TD )
               END IF
               IF ( TEST_STR == 'temp'  .AND.  I_LON == IT_LON  .AND.  J2 == IT_LAT ) THEN
                    ZD(J8) = TD_3D(J8,I_LON,J2)
               END IF
 480        CONTINUE 
            IF ( TEST_STR == 'temp'  .AND.  I_LON == IT_LON  .AND.  J2 == IT_LAT ) THEN
                 write ( 6 ,* ) 'h(0) = ', h(0) ! %%%%%%
                 CALL DIAGI_1 ( M_LEV, HEI_R8(1), ZD(1), IER )
            END IF
            IF ( TEST_STR == 'gen' ) TD_LEV(I_LON,J2)= TD_3D(4,I_LON,J2)
 430     CONTINUE 
!
! ------ We extend longitude coverage to one pixel eastward.
! ------ The pixel with longitude index L_LON+1 is equal to the pixel with index 1
!
         TT_3D(1-MALO__MDEG:M_LEV,L_LON+1,J2) = TT_3D(1-MALO__MDEG:M_LEV,1,J2) 
         TD_3D(1-MALO__MDEG:M_LEV,L_LON+1,J2) = TD_3D(1-MALO__MDEG:M_LEV,1,J2) 
 420  CONTINUE 
      VAL_MIN =  1.0
      VAL_MAX = -1.0
      IF ( TEST_STR == 'gen' ) CALL PLOT_GRID_R4 ( 1, 7, 0, 1, L_LON, L_LAT, &
     &                         TD_LEV, 'Plotik-a', 'B', VAL_MIN, VAL_MAX, 'C', -2 )
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
     &                    TD_3D, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 4721, IUER, 'MALO_COMP_DEWT', 'Failure in an attempt '// &
     &         'to compute coefficients of the 3D interpolating spline' )
           RETURN 
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL BSPL4_3D_CMP ( MALO__MDEG, 0, DIMS, HEI_R4, LON_R4, LAT_R4, &
     &                    TT_3D, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 4722, IUER, 'MALO_COMP_DEWT', 'Failure in an attempt '// &
     &         'to compute coefficients of the 3D interpolating spline' )
           RETURN 
      END IF
      IF ( TEST_STR == 'timer' ) THEN
           CALL WALL_TIMER ( STR )
           WRITE ( 6, '(A)' ) 'Computation of 3D B-spline:       '//STR(1:I_LEN(STR)-5)
           CALL WALL_TIMER ( %VAL(0) )
      END IF
!
      HEB_DEWT%MJD = HEB_DELP%MJD
      HEB_DEWT%UTC = HEB_DELP%UTC
      HEB_DEWT%TAI = HEB_DELP%TAI
!
      HEB_ST%MJD = HEB_DELP%MJD
      HEB_ST%UTC = HEB_DELP%UTC
      HEB_ST%TAI = HEB_DELP%TAI
      INDS(3) = 1
!
! --- Now compute dew point temperature by 3D interpolation 
!
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 
!      call plot_grid_r4 ( 1, 7, 0, 1, heb_dewt%dims(1), heb_dewt%dims(2), &
!     &                    heb_oh%val, 'a', 'b', 'c', -2 )
!      hei_dew_point = 300.0
!      write ( 6, * ) ' dims = ', dims             ! %%%%%%%%
!      write ( 6, * ) ' hei_r4= ', hei_r4(1:m_lev) ! %%%%%%%%
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 
      DO 4110 J11=1,HEB_DEWT%DIMS(2)
         LAT_VAL = -P2I + (J11-1)*PI__NUM/(HEB_DEWT%DIMS(2)-1)
         INDS(3) = IXMN4_S ( INDS(3), L_LAT, LAT_R4, LAT_VAL )
         IF ( LAT_VAL .LE. LAT_R4(1)     ) INDS(3) = 1
         IF ( LAT_VAL .GE. LAT_R4(L_LAT) ) INDS(3) = L_LAT-1
         INDS(2) = 1
         DO 4120 J12=1,HEB_DEWT%DIMS(1)
            I_LON = J12 + L_LON/2
            IF ( I_LON > L_LON ) I_LON = I_LON - L_LON
            LON_VAL = (J12-1)*PI2/HEB_DEWT%DIMS(1)
            INDS(2) = IXMN4_S ( INDS(2), L_LON+1, LON_R4, LON_VAL )
            IF ( LON_VAL .LE. LON_R4(1)       ) INDS(2) = 1
            IF ( LON_VAL .GE. LON_R4(L_LON+1) ) INDS(2) = L_LON
            IF ( TEST_STR == 'intrp' ) THEN
                 HEB_OH%VAL(J12,J11,1,1) = HEB_G%VAL(I_LON,J11,1,1)
            END IF
            INDS(1) = IXMN4 ( M_LEV, HEI_R4, HEB_OH%VAL(J12,J11,1,1) + SNGL(HEI_DEW_POINT) )
            ARGS(1) = HEB_OH%VAL(J12,J11,1,1) + HEI_DEW_POINT
            ARGS(2) = LON_VAL
            ARGS(3) = LAT_VAL
            HEB_DEWT%VAL(J12,J11,1,1) = VAL_3D_BSPL4 ( ARGS, MALO__MDEG, DIMS, INDS, &
     &                                                 HEI_R4, LON_R4, LAT_R4, TD_3D )
            HEB_ST%VAL(J12,J11,1,1)   = VAL_3D_BSPL4 ( ARGS, MALO__MDEG, DIMS, INDS, &
     &                                                 HEI_R4, LON_R4, LAT_R4, TT_3D )
 4120    CONTINUE 
 4110 CONTINUE 
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 
!      call plot_grid_r4 ( 1, 7, 0, 1, heb_dewt%dims(1), heb_dewt%dims(2), &
!     &                    heb_oh%val, 'Plot-1', 'b', 'c', -2 )
!      call plot_grid_r4 ( 1, 7, 0, 1, heb_dewt%dims(1), heb_dewt%dims(2), &
!     &                    heb_dewt%val, 'Plot-2', 'b', 'c', -2 )
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 
      DEALLOCATE ( TT_3D ) 
      DEALLOCATE ( TD_3D ) 
      DEALLOCATE ( LAT_R4 ) 
      DEALLOCATE ( LON_R4 ) 
      IF ( TEST_STR == 'timer' ) THEN
           CALL WALL_TIMER ( STR ) 
           WRITE ( 6, '(A)' ) 'Perform 3D B-spline intrpolation: '//STR(1:I_LEN(STR)-5)
      END IF
      IF ( TEST_STR == 'diff' .OR. TEST_STR == 'intrp' .OR. TEST_STR == 'gen' ) THEN
           DEALLOCATE ( TD_LEV )
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  MALO_COMP_DEWT  !#!#
