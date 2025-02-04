      SUBROUTINE MALO_INTRP_PPWTEM ( NTIM, HEB_G, HEB_H, HEB_T, HEB_W, &
     &                               LP_LEV, PRES_LEV, MALO, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  MALO_INTRP_PPWTEM 
! *                                                                      *
! * ## 12-JUN-2013  MALO_INTRP_PPWTEM v1.0 (c) L. Petrov 13-MAR-2013 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'malo.i'
      INCLUDE   'heb.i'
      INTEGER*4  NTIM, LP_LEV, IUER
      TYPE     ( MALO__TYPE ) :: MALO
      TYPE     ( HEB__TYPE  ) :: HEB_G(NTIM), HEB_H(NTIM), HEB_T(NTIM), &
     &                           HEB_W(NTIM)
      REAL*8     PRES_LEV(LP_LEV)
      REAL*4     HEI_R4(MALO__MHEI), LAT_VAL, LON_VAL, ARGS(4)
      INTEGER*8  MEL
      INTEGER*4  M_LEV
      PARAMETER  ( M_LEV = MALO__MHEI )
      REAL*8     EPS, PRES_HIGH, HEI_MIN, HEI_MAX, HEI_MIN_INT, HEI_MAX_INT
      PARAMETER  ( PRES_HIGH    = 25000.0D0 )
      PARAMETER  ( HEI_MIN      =  -500.0D0 )
      PARAMETER  ( HEI_MAX      = 12700.0D0 )
      PARAMETER  ( HEI_MIN_INT  = -1000.0D0 )
      PARAMETER  ( HEI_MAX_INT  = 40000.0D0 )
      PARAMETER  ( EPS = 1.D-5 )
      REAL*8       SPD__U1_GMAO72, SPD__U2_GMAO72, SPD__U3_GMAO72 
      PARAMETER  ( SPD__U1_GMAO72 =    20.25319 )
      PARAMETER  ( SPD__U2_GMAO72 =  1200.00000 )
      PARAMETER  ( SPD__U3_GMAO72 =  -169.30782 )
      REAL*8       SPD__R, SPD__MA, SPD__H2O
      PARAMETER  ( SPD__R        = 8.314472D0   ) ! CIPM-2007
      PARAMETER  ( SPD__MA       = 0.02896546D0 ) ! CIPM-2007
      PARAMETER  ( SPD__H2O      = 0.01801528D0 ) ! CIPM-2007
      REAL*8       MD, MW, W_LEV_MIN
      PARAMETER  ( MD    = MA__MAPL  )
      PARAMETER  ( MW    = H2O__MAPL )
      PARAMETER  ( W_LEV_MIN = 0.01 )
      REAL*8     T(M_LEV), H(M_LEV), H_ARR(M_LEV), &
     &           P_ARR(M_LEV), P_SPL(M_LEV), P_LOG(M_LEV), &
     &           W_ARR(M_LEV), W_SPL(M_LEV), W_LOG(M_LEV), &
     &           T_ARR(M_LEV), T_SPL(M_LEV), T_LEV(M_LEV), &
     &           W_LEV(M_LEV), LON_TST, LAT_TST
      CHARACTER  STR*128, STR1*128, TEST_STR*8
      INTEGER*4  L_LON, L_LAT, IND, K_LEV, L_LEV, N_LEV, DIMS(4), &
     &           J1, J2, J3, J4, J5, J6, J7, J8, J9, J10, J11, J12, &
     &           INDS(4), IND_1ST, IND_LAST, IT_LAT, IT_LON, &
     &           L_TIM, IP, IS, IER
!@  real*4, allocatable :: pres(:,:) ! %%%%%%%%% for tests
!@  real*8     r1_min, r1_max, r2_min, r2_max, pres_val, temp_val ! %%%%% for tests
      REAL*8     GE, G, PHI, T_1ST, T_RATE, TEMP
      REAL*8     HARR(2), TARR(2), MIN_VAL, MAX_VAL
      INTEGER*4, EXTERNAL :: I_LEN, ILEN, IXMN8, IXMN4, IXMN4_S
      REAL*4,    EXTERNAL :: VAL_3D_BSPL4, VAL_4D_BSPL4
      REAL*8,    EXTERNAL :: ISPL8, FSPL8, PW_TO_RELH
      CHARACTER, EXTERNAL :: MJDSEC_TO_DATE*30
!
      TEST_STR = 'timer'  ! Supported values: none, timer, diff, pw, pres, temp, intrp, dewt
      LON_TST  = 270.0*DEG__TO__RAD
      LAT_TST  =   0.0*DEG__TO__RAD
!
! --- Grid of the input numerical models
!
      L_LON = HEB_H(1)%DIMS(1)
      L_LAT = HEB_H(1)%DIMS(2)
      L_LEV = HEB_H(1)%DIMS(3)
      L_TIM = NTIM
!
      IT_LON = 1 + L_LON*(LON_TST/PI2)
      IF ( IT_LON > L_LON ) IT_LON = IT_LON - L_LON
      IT_LAT = 1 + (L_LAT-1)*((LAT_TST+P2I)/PI__NUM)
!@!$OMP CRITICAL   
!@            WRITE ( 6, * ) 'R0 ', HEB_H(1)%MJD  ! %%%%
!@!$OMP END CRITICAL   
!
! --- Grid of the output Total pressure, partial pressure of water vapor and air temperatyre
!
      MALO%NLEV = MALO__MHEI
      MALO%NLON = L_LON
      MALO%NLAT = L_LAT
      MALO%NTIM = L_TIM
!
      IF ( ASSOCIATED ( MALO%PPWTEM_4D ) ) DEALLOCATE ( MALO%PPWTEM_4D )
      IF ( ASSOCIATED ( MALO%LEV       ) ) DEALLOCATE ( MALO%LEV       )
      IF ( ASSOCIATED ( MALO%TIM       ) ) DEALLOCATE ( MALO%TIM       )
      IF ( ASSOCIATED ( MALO%LAT       ) ) DEALLOCATE ( MALO%LAT       )
      IF ( ASSOCIATED ( MALO%LON       ) ) DEALLOCATE ( MALO%LON       )
!
      IF ( L_TIM > 1 ) THEN
           ALLOCATE ( MALO%PPWTEM_4D(1-MALO__MDEG:MALO__MHEI,1-MALO__MDEG:L_LON+1,1-MALO__MDEG:L_LAT,1-MALO__MDEG:L_TIM,3), &
     &                STAT=IER )
           MEL = INT8(L_LON+1+MALO__MDEG)*INT8(L_LAT+MALO__MDEG)*INT8(MALO__MHEI+MALO__MDEG)*INT8(L_TIM+MALO__MDEG)*INT8(3)
         ELSE
           ALLOCATE ( MALO%PPWTEM_4D(1-MALO__MDEG:MALO__MHEI,1-MALO__MDEG:L_LON+1,1-MALO__MDEG:L_LAT,1,3), &
     &                STAT=IER )
           MEL = INT8(L_LON+1+MALO__MDEG)*INT8(L_LAT+MALO__MDEG)*INT8(MALO%NTIM+MALO__MDEG)*INT8(3)
      END IF
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH  ( STR )
           CALL IINCH8 ( INT8(4)*MEL, STR )
           CALL ERR_LOG ( 4611, IUER, 'MALO_INTRP_PPWTEM', 'Failure in an attempt '// &
     &         'to allocate '//STR(1:I_LEN(STR))//' bytes of dynamic memory '// &
     &         'for array MALO%PPWTEM_4D' )
           RETURN 
      END IF
!
      MALO%PPWTEM_4D = 0.0
      MALO%PPWTEM_STATUS = MALO__ALLO
!
      ALLOCATE ( MALO%LEV(MALO%NLEV), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( 4*MALO%NLEV, STR )
           CALL ERR_LOG ( 4611, IUER, 'MALO_INTRP_PPWTEM', 'Failure in '// &
     &         'an attempt to allocate '//STR(1:I_LEN(STR))// &
     &         ' bytes of dynamic memory for array MALO%LEV' )
           RETURN 
      END IF
!
      ALLOCATE ( MALO%LAT(L_LAT), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( 4*L_LAT, STR )
           CALL ERR_LOG ( 4611, IUER, 'MALO_INTRP_PPWTEM', 'Failure in '// &
     &         'an attempt to allocate '//STR(1:I_LEN(STR))// &
     &         ' bytes of dynamic memory for array MALO%LAT' )
           RETURN 
      END IF
!
      ALLOCATE ( MALO%LON(L_LON+1), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( 4*(L_LON+1), STR )
           CALL ERR_LOG ( 4611, IUER, 'MALO_INTRP_PPWTEM', 'Failure in '// &
     &         'an attempt to allocate '//STR(1:I_LEN(STR))// &
     &         ' bytes of dynamic memory for array MALO%LON' )
           RETURN 
      END IF
!
      ALLOCATE ( MALO%TIM(L_TIM), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( 4*L_LON, STR )
           CALL ERR_LOG ( 4611, IUER, 'MALO_INTRP_PPWTEM', 'Failure in '// &
     &         'an attempt to allocate '//STR(1:I_LEN(STR))// &
     &         ' bytes of dynamic memory for array MALO%TIM' )
           RETURN 
      END IF
!
!$OMP CRITICAL   
      IF ( TEST_STR == 'timer' ) THEN
           WRITE ( 6, '(A,I6,1X,I6)' ) 'MALO_INTRP_PPWTEM NLON/NLAT = ', HEB_H(1)%DIMS(1), HEB_H(1)%DIMS(2)
           CALL WALL_TIMER ( %VAL(0) ) 
     END IF
!$OMP END CRITICAL   
!
      DO 410 J1=1,L_TIM
         DO 420 J2=1,L_LAT
!
! --------- PHI -- is geodetic latitude
!
            PHI = -P2I + (J2-1)*PI__NUM/(L_LAT-1)
!
! --------- Compute gravity on the reference ellipsoid for a latitude
!
            GE = ACC_EQU__WGS84*(1.D0 + GRV_LAT__WGS84*DSIN(PHI)**2)/ &
     &             DSQRT(1.D0 - (2.D0*FLAT__WGS84 - FLAT__WGS84**2)*DSIN(PHI)**2 )
            DO 430 J3=1,L_LON
               IND_1ST  = 0
               IND_LAST = 0
               DO 440 J4=1,LP_LEV
                  H(J4) = HEB_H(J1)%VAL(J3,J2,J4,1) 
                  T(J4) = HEB_T(J1)%VAL(J3,J2,J4,1) 
                  IF ( H(J4) > HEB_G(J1)%VAL(J3,J2,1,1) ) THEN
                       IF ( IND_1ST == 0 ) IND_1ST = J4
                  END IF
                  IF ( H(J4) < HEI_MAX ) THEN
                       IND_LAST = J4
                  END IF
                  W_LEV(J4) = HEB_W(J1)%VAL(J3,J2,J4,1)
                  P_LOG(J4) = DLOG(PRES_LEV(J4))
                  IF ( W_LEV(J4) < W_LEV_MIN ) W_LEV(J4) = W_LEV_MIN
                  W_LOG(J4) = DLOG(W_LEV(J4))
                  T_LEV(J4) = HEB_T(J1)%VAL(J3,J2,J4,1)
 440           CONTINUE 
               IND_1ST = IND_1ST + 2
!
               CALL ERR_PASS ( IUER, IER )
               CALL REGR8 ( IND_LAST-IND_1ST+1, H(IND_1ST), T(IND_1ST), T_RATE, T_1ST, IER )
               IF ( IER .NE. 0 ) THEN
                    write ( 6,  * ) ' j2= ', j2, ' j1= ', j1, &
     &                              ' ind_1st= ', ind_1st, ' ind_last= ', ind_last
                    write ( 6,  * ) ' T = ', T
                    CALL ERR_LOG ( 4612, IUER, 'MALO_INTRP_PPWTEM', 'Failure '// &
     &                  'in an attempt to compute lapse rate' ) 
                    RETURN 
               END IF
!
               CALL ERR_PASS ( IUER, IER ) 
               CALL MAKE_SPLINE ( 3, LP_LEV, H, P_LOG, 0.0D0, 0.0D0, P_SPL, H_ARR, IER )
               IF ( IER .NE. 0 ) THEN
                    CALL ERR_LOG ( 4613, IUER, 'MALO_INTRP_PPWTEM', 'Failure '// &
     &                  'in an attempt to compute coefficients of the '// &
     &                  'interpolating spline' )
                    RETURN 
               END IF
!
               CALL ERR_PASS ( IUER, IER ) 
               CALL MAKE_SPLINE ( 3, LP_LEV, H, W_LOG, 0.0D0, 0.0D0, W_SPL, H_ARR, IER )
               IF ( IER .NE. 0 ) THEN
                    CALL ERR_LOG ( 4614, IUER, 'MALO_INTRP_PPWTEM', 'Failure '// &
     &                  'in an attempt to compute coefficients of '// &
     &                  'the interpolating spline for array W_LOG' )
                    RETURN 
               END IF
!
               CALL ERR_PASS ( IUER, IER ) 
               CALL MAKE_SPLINE ( 3, LP_LEV, H, T_LEV, 0.0D0, 0.0D0, T_SPL, H_ARR, IER )
               IF ( IER .NE. 0 ) THEN
                    CALL ERR_LOG ( 4615, IUER, 'MALO_INTRP_PPWTEM', 'Failure in '// &
     &                  'an attempt to compute coefficients of the interpolating '// &
     &                  'spline for array T_LEV' )
                    RETURN 
               END IF
!
               DO 450 J5=1,M_LEV
                  MALO%LEV(J5) = J5-1 - (MALO__MHEI-1)/2
                  H_ARR(J5) = DEXP ( (MALO%LEV(J5) - SPD__U3_GMAO72)/SPD__U1_GMAO72 ) - SPD__U2_GMAO72
                  HEI_R4(J5) = H_ARR(J5)
                  IF ( H_ARR(J5) < H(1) ) THEN
                       TEMP = T(1) + T_RATE * (H_ARR(J5) - H(1))
                       G = GE*(1.D0 - 2.D0/REA__WGS84*(1.0D0 + OMEGA__EGM96**2*REA__WGS84**2* &
     &                        (1.D0 - FLAT__WGS84)/GM__EGM96 + (1.D0 - 2.D0*FLAT__WGS84)* &
     &                        DSIN(PHI)**2)*H_ARR(J5) + 3.D0/REA__WGS84**2*H_ARR(J5)**2)
!
                       P_ARR(J5) = PRES_LEV(1)*(TEMP/T(1))**(-G*MD/(R__MAPL*T_RATE))
                       W_ARR(J5) = W_LEV(1)*(TEMP/T(1))**(-G*MW/(R__MAPL*T_RATE))
                       T_ARR(J5) = TEMP
                    ELSE IF ( H_ARR(J5) > H(LP_LEV) ) THEN
                       T_RATE = 2.8D-3
                       TEMP = T(LP_LEV) + T_RATE * (H_ARR(J5) - H(LP_LEV))
                       G = GE*(1.D0 - 2.D0/REA__WGS84*(1.0D0 + OMEGA__EGM96**2*REA__WGS84**2* &
     &                        (1.D0 - FLAT__WGS84)/GM__EGM96 + (1.D0 - 2.D0*FLAT__WGS84)* &
     &                        DSIN(PHI)**2)*H_ARR(J5) + 3.D0/REA__WGS84**2*H_ARR(J5)**2)
!
                       P_ARR(J5) = PRES_LEV(LP_LEV)*(TEMP/T(LP_LEV))**(-G*MD/(R__MAPL*T_RATE))
                       W_ARR(J5) = W_LEV(LP_LEV)*(TEMP/T(LP_LEV))**(-G*MW/(R__MAPL*T_RATE))
                       T_ARR(J5) = TEMP
                    ELSE
                       IND = IXMN8 ( LP_LEV, H, H_ARR(J5) )
                       P_ARR(J5) = DEXP ( FSPL8 ( H_ARR(J5), LP_LEV, H, P_LOG, IND, P_SPL )  )
                       W_ARR(J5) = DEXP ( FSPL8 ( H_ARR(J5), LP_LEV, H, W_LOG, IND, W_SPL )  )
                       T_ARR(J5) =        FSPL8 ( H_ARR(J5), LP_LEV, H, T_LEV, IND, T_SPL )
                  END IF
                  MALO%PPWTEM_4D(J5,J3,J2,J1,MALO__P)   = DLOG( P_ARR(J5) )
                  MALO%PPWTEM_4D(J5,J3,J2,J1,MALO__PW)  = DLOG( W_ARR(J5) )
                  MALO%PPWTEM_4D(J5,J3,J2,J1,MALO__TEM) =       T_ARR(J5)
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!@!!   if ( j5 ==1 ) write ( 6, * ) 'Coo: ', int2(j3), int2(j2), ' P= ', MALO%PPWTEM_4D(J5,J3,J2,J1,MALO__P) !,  p_arr= ', p_arr(j5) ! %%%%%%%%%%%%%%%%%%%%%%%%%
!@                  if ( w_arr(j5) < 1.d-11 .or. w_arr(j5) > 12000.0 ) then
!@                       write ( 6, * ) 'j5/j3/j2= ', int2(j5), int2(j3), int2(j2), ' W= ', w_arr(j5) ! %%%% 
!@                  end if 
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 450           CONTINUE 
 430        CONTINUE 
            MALO%PPWTEM_4D(1:M_LEV,L_LON+1,J2,J1,MALO__P)   = MALO%PPWTEM_4D(1:M_LEV,1,J2,J1,MALO__P) 
            MALO%PPWTEM_4D(1:M_LEV,L_LON+1,J2,J1,MALO__PW)  = MALO%PPWTEM_4D(1:M_LEV,1,J2,J1,MALO__PW) 
            MALO%PPWTEM_4D(1:M_LEV,L_LON+1,J2,J1,MALO__TEM) = MALO%PPWTEM_4D(1:M_LEV,1,J2,J1,MALO__TEM) 
 420     CONTINUE 
         IF ( J1 == 1 ) THEN
              MALO%TIM(J1) = 0.0
              MALO%MJD_BEG = HEB_H(J1)%MJD
              MALO%UTC_BEG = HEB_H(J1)%UTC
            ELSE 
              MALO%TIM(J1) = (HEB_H(J1)%MJD - MALO%MJD_BEG)*86400.0 + &
     &                       (HEB_H(J1)%UTC - MALO%UTC_BEG)
         END IF
 410  CONTINUE 
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!      allocate ( pres(l_lon,l_lat) )
!      do 560 j6=1,l_lat
!         do 570 j7=1,l_lon
!            pres(j7,j6) = exp ( malo%ppwtem_4d(4,j7,j6,2,malo__p) ) - 101300.
!!  if ( pres(j7,j6) .ne. 0 ) then
!!       write ( 6,  * ) 'PG: ', int2(j7), int2(j6), ' pres= ', pres(j7,j6)
!!!       call pause ( 'sdas' ) 
!!  endif
! 570     continue 
! 560  continue 
!      write ( 6, * ) ' hei_r4= ', hei_r4 ! %%%%
!      call plot_grid_r4 ( 1, 7, 40, 1, l_lon, l_lat, pres, 'pres', 'Pa', '/tmp/boo', -2 ) 
!      deallocate ( pres )
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!
! --- Build arrays of longitude/latitude
!
      DO 460 J6=1,L_LON+1
         MALO%LON(J6) = (J6-1)*PI2/L_LON
 460  CONTINUE 
!
      DO 470 J7=1,L_LAT
         MALO%LAT(J7) = -P2I + (J7-1)*PI__NUM/(L_LAT-1)
 470  CONTINUE 
      MALO%PPWTEM_STATUS = MALO__LOAD
!
!$OMP CRITICAL   
      IF ( TEST_STR == 'timer' ) THEN
           CALL WALL_TIMER ( STR )
           WRITE ( 6, '(A)' ) 'Collecting three functions:    '//STR(1:I_LEN(STR)-5)
           CALL WALL_TIMER ( %VAL(0) )
      END IF
!$OMP END CRITICAL   
!
! --- Compute coefficients of 3D interpolating B-splines
!
      DIMS(1) = MALO__MHEI
      DIMS(2) = L_LON+1
      DIMS(3) = L_LAT
      DIMS(4) = L_TIM
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!@      allocate ( pres(l_lon,l_lat) )
!@      ip = malo__pw
!@      r1_min = minval ( malo%ppwtem_4d(1-malo__mdeg:malo__mhei, &
!@     &                                 1-malo__mdeg:malo%nlon+1, &
!@     &                                 1-malo__mdeg:malo%nlat, &
!@     &                                 1-malo__mdeg:malo%ntim,ip) )
!@      r1_max = maxval ( malo%ppwtem_4d(1-malo__mdeg:malo__mhei, &
!@     &                                 1-malo__mdeg:malo%nlon+1, &
!@     &                                 1-malo__mdeg:malo%nlat, &
!@     &                                 1-malo__mdeg:malo%ntim,ip) )
!@      allocate ( pres(1:malo%nlon,1:malo%nlat) )
!@ 710  continue 
!@      write ( 6, * ) 'PW-val Enter layer' 
!@      read ( 5, '(i4)', iostat=ier ) is
!@      if ( is < 1 .or. is > dims(1) ) goto 810
!@      do 510 j1=1,malo%ntim
!@         do 520 j2=1,malo%nlat
!@            do 530 j3=1,malo%nlon
!@                do 540 j4=1,malo__mhei
!@                   if ( j1 == 1 .and. j4 == is ) pres(j3,j2) = malo%ppwtem_4d(j4,j3,j2,j1,ip) 
!@ 540            continue 
!@ 530        continue 
!@ 520     continue 
!@ 510  continue 
!@      write ( 6, * ) ' r1_min= ', r1_min, ' r1_max= ', r1_max ! %%%%%%%%%%%%%%%%%%%%%%%
!@      write ( 6, * ) ' r2_min= ', r2_min, ' r2_max= ', r2_max ! %%%%%%%%%%%%%%%%%%%%%%%
!@      write ( 6, * ) ' ntim= ', ntim, ' l_tim= ', l_tim ! %%%%
!@      call plot_grid_r4 ( 1, 7, 0, 1, malo%nlon, malo%nlat, pres, &
!@     &                    'pw-val', 'log(Pa)', '/tmp/boo', ier )
!@      goto 710
!@ 810  continue 
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      DO 480 J8=1,3
         IF ( NTIM > 1 ) THEN
              CALL ERR_PASS ( IUER, IER )
              CALL BSPL4_4D_CMP ( MALO__MDEG, 0, DIMS, MALO%LEV, MALO%LON, MALO%LAT, MALO%TIM, &
     &                            MALO%PPWTEM_4D(1-MALO__MDEG,1-MALO__MDEG,1-MALO__MDEG,1-MALO__MDEG,J8), &
     &                            IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 4616, IUER, 'MALO_INTRP_PPWTEM', 'Failure in '// &
     &                 'an attempt to compute coefficients of the 3D '// &
     &                 'interpolating spline' )
                   RETURN 
              END IF
            ELSE IF ( NTIM == 1 ) THEN
              CALL ERR_PASS ( IUER, IER )
              CALL BSPL4_3D_CMP ( MALO__MDEG, 0, DIMS, MALO%LEV, MALO%LON, MALO%LAT, &
     &                            MALO%PPWTEM_4D(1-MALO__MDEG,1-MALO__MDEG,1-MALO__MDEG,1,J8), &
     &                            IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 4617, IUER, 'MALO_INTRP_PPWTEM', 'Failure in '// &
     &                 'an attempt to compute coefficients of the 3D '// &
     &                 'interpolating spline' )
                   RETURN 
              END IF
         END IF
 480  CONTINUE 
!$OMP CRITICAL   
      IF ( TEST_STR == 'timer' ) THEN
           CALL WALL_TIMER ( STR )
           WRITE ( 6, '(A)' ) 'Computation of 3D B-splines:   '//STR(1:I_LEN(STR)-5)
           CALL WALL_TIMER ( %VAL(0) )
      END IF
!$OMP END CRITICAL   
      MALO%PPWTEM_STATUS = MALO__COMP
!@%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
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
!@                   if ( j1 == 1 .and. j4 == is ) then
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
!@     &                                         malo%ppwtem_4d(1-malo__mdeg,1-malo__mdeg,1-malo__mdeg,1-malo__mdeg,ip) )
!@                        temp_val = val_4d_bspl4 ( args, malo__mdeg, dims, inds, &
!@     &                                         malo%lev, malo%lon, malo%lat, malo%tim, &
!@     &                                         malo%ppwtem_4d(1-malo__mdeg,1-malo__mdeg,1-malo__mdeg,1-malo__mdeg,malo__tem) )
!@!                        pres(j3,j2) = pres_val
!@                        pres_val = exp ( pres_val )
!@!!                        pres(j3,j2) = log10 ( pres_val/temp_val * spd__h2o/spd__r )
!@                        pres(j3,j2) = log10 ( pw_to_relh ( dble(pres_val), dble(temp_val) ) )
!@                   endif
!@ 640            continue 
!@ 630        continue 
!@ 620     continue 
!@ 610  continue 
!@      call clrch ( str )
!@      write ( unit=str(1:10), fmt='(f10.0)' ) h_arr(is) 
!@      call chashl ( str )
!@!      call plot_grid_r4 ( 1, 7, 0, 1, malo%nlon, malo%nlat, pres, &
!@!     &                    'Lg(water vapor density kg/m**3) at height '//str(1:i_len(str)), &
!@!     &                    'lg(rho)', '/tmp/boo', ier )
!@      call plot_grid_r4 ( 1, 7, 0, 1, malo%nlon, malo%nlat, pres, &
!@     &                    'lg(rel. humid) at height '//str(1:i_len(str))//' on '//str1(1:16), &
!@     &                    'lg(rhel)', '/tmp/boo', ier )
!@      goto 720
!@ 820  continue 
!@! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  MALO_INTRP_PPWTEM  !#!#
