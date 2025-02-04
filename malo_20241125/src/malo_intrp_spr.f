      SUBROUTINE MALO_INTRP_SPR ( HEB_H, HEB_T, HEB_G, HEB_OH, LP_LEV, &
     &                            PRES_LEV, MAL, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine MALO_INTRP_SPR computes surface pressure for the grid      *
! *   of orthoheights specifed in HEB_OH object using parameters of      *
! *   4D numerical models outputs: 
! *   Results are written in object MAL.  *
! *                                                                      *
! * ### 15-FEB-2013  MALO_INTRP_SPR v1.0 (c)  L. Petrov  15-FEB-2013 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'malo.i'
      INCLUDE   'heb.i'
      TYPE     ( HEB__TYPE  ) :: HEB_H, HEB_T, HEB_G, HEB_OH
      TYPE     ( MALO__TYPE ) :: MAL
      INTEGER*4  LP_LEV, IUER
      REAL*8     PRES_LEV(LP_LEV)
      REAL*4,    ALLOCATABLE  :: PRS(:,:,:), LAT_R4(:), LON_R4(:), &
     &                           SURF(:,:)
      REAL*8     EPS, PRES_HIGH, HEI_MIN, HEI_MAX, HEI_MIN_INT, HEI_MAX_INT
      PARAMETER  ( PRES_HIGH    = 25000.0D0 )
      PARAMETER  ( HEI_MIN      =  -500.0D0 )
      PARAMETER  ( HEI_MAX      = 12700.0D0 )
      PARAMETER  ( HEI_MIN_INT  = -1000.0D0 )
      PARAMETER  ( HEI_MAX_INT  = 40000.0D0 )
      PARAMETER  ( EPS = 1.D-5 )
      INTEGER*4    M_LEV
      REAL*8       MD, MW
      PARAMETER  ( M_LEV = 42        )
      PARAMETER  ( MD    = MA__MAPL  )
      PARAMETER  ( MW    = H2O__MAPL )
      CHARACTER  STR*128, STR1*128, TEST_STR*8
      REAL*4,    ALLOCATABLE  :: PRS_SUR(:,:)
      REAL*4     HEI_R4(M_LEV), LAT_VAL, LON_VAL, ARGS(3)
      REAL*8     T(M_LEV), H(M_LEV), H_ARR(M_LEV), P_ARR(M_LEV), P_SPL(M_LEV), &
     &           P_LOG(M_LEV)
      INTEGER*4  L_LON, L_LAT, IND, K_LEV, L_LEV, N_LEV, DIMS(3), &
     &           J1, J2, J3, J4, J5, J6, J7, J8, J9, J10, J11, J12, &
     &           INDS(3), IND_1ST, IND_LAST, IER
      REAL*8     GE, G, PHI, T_1ST, T_RATE, TEMP
      REAL*8     HARR(2), TARR(2), MIN_VAL, MAX_VAL 
      INTEGER*4, EXTERNAL :: I_LEN, ILEN, IXMN8, IXMN4, IXMN4_S
      REAL*4,    EXTERNAL :: VAL_3D_BSPL4 
      REAL*8,    EXTERNAL :: ISPL8, FSPL8
!
! --- Grid of the input numerical models
!
      L_LON = HEB_H%DIMS(1)
      L_LAT = HEB_H%DIMS(2)
      L_LEV = HEB_H%DIMS(3)
      IF ( LP_LEV .NE. L_LEV ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( LP_LEV, STR1 )
           CALL CLRCH ( STR1 )
           CALL INCH  ( L_LEV,  STR )
           CALL ERR_LOG ( 4611, IUER, 'MALO_INTRP_SPR', 'Wrong parameter '// &
     &         'LP_LEV: '//STR(1:I_LEN(STR))//' it differs from '// &
     &         'HEB%H_DIMS(3) = '//STR1 )
           RETURN 
      END IF
!
      TEST_STR = 'none'
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
           CALL ERR_LOG ( 4611, IUER, 'MALO_INTRP_SPR', 'Failure in an attempt '// &
     &         'to allocate '//STR(1:I_LEN(STR))//' bytes of dynamic memory '// &
     &         'for array MAL%SPR' )
           RETURN 
      END IF
!
      ALLOCATE ( SURF(L_LON,L_LAT), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( 4*L_LON*L_LAT, STR )
           CALL ERR_LOG ( 4611, IUER, 'MALO_INTRP_SPR', 'Failure in an attempt '// &
     &         'to allocate '//STR(1:I_LEN(STR))//' bytes of dynamic memory '// &
     &         'for array SURF' )
           RETURN 
      END IF
!
      IF ( TEST_STR == 'timer' ) THEN
           WRITE ( 6, * ) ' MAL%NLON/MAL%NLAT = ', MAL%NLON, MAL%NLAT
           CALL WALL_TIMER ( %VAL(0) ) 
      END IF
!
      ALLOCATE ( PRS(1-MALO__MDEG:M_LEV,1-MALO__MDEG:L_LON+1,1-MALO__MDEG:L_LAT), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( 4*(M_LEV+MALO__MDEG)*(L_LON+1+MALO__MDEG)*(L_LAT+MALO__MDEG), STR )
           CALL ERR_LOG ( 4612, IUER, 'MALO_INTRP_SPR', 'Failure in an attempt '// &
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
           CALL ERR_LOG ( 4612, IUER, 'MALO_INTRP_SPR', 'Failure in an attempt '// &
     &         'to allocate '//STR(1:I_LEN(STR))//' bytes of dynamic memory '// &
     &         'for array LAT_R4' )
           RETURN 
      END IF
!
      ALLOCATE ( LON_R4(L_LON+1), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( 4*L_LON, STR )
           CALL ERR_LOG ( 4613, IUER, 'MALO_INTRP_SPR', 'Failure in an attempt '// &
     &         'to allocate '//STR(1:I_LEN(STR))//' bytes of dynamic memory '// &
     &         'for array LON_R4' )
           RETURN 
      END IF
!
      PRS    = 0.0
      LAT_R4 = 0.0
      LON_R4 = 0.0
!
      DO 410 J1=1,L_LAT
!
! ------ PHI -- is geodetic latitude
!
         PHI = -P2I + (J1-1)*PI__NUM/(L_LAT-1)
!
! ------ Compute gravity on the reference ellipsoid for a latitude
!
         GE = ACC_EQU__WGS84*(1.D0 + GRV_LAT__WGS84*DSIN(PHI)**2)/ &
     &          DSQRT(1.D0 - (2.D0*FLAT__WGS84 - FLAT__WGS84**2)*DSIN(PHI)**2 )
         LAT_R4(J1) = PHI
         DO 420 J2=1,L_LON
            LON_R4(J2) = (J2-1)*PI2/L_LON
            IND_1ST  = 0
            IND_LAST = 0
            DO 430 J3=1,L_LEV
               H(J3) = HEB_H%VAL(J2,J1,J3,1) 
               T(J3) = HEB_T%VAL(J2,J1,J3,1) 
               IF ( H(J3) > HEB_G%VAL(J2,J1,1,1) ) THEN
                    IF ( IND_1ST == 0 ) IND_1ST = J3
               END IF
               IF ( H(J3) < HEI_MAX ) THEN
                    IND_LAST = J3
               END IF
               P_LOG(J3) = DLOG(PRES_LEV(J3))
 430        CONTINUE 
            IND_1ST = IND_1ST + 2
!
            CALL ERR_PASS ( IUER, IER )
            CALL REGR8 ( IND_LAST-IND_1ST+1, H(IND_1ST), T(IND_1ST), T_RATE, T_1ST, IER )
            IF ( IER .NE. 0 ) THEN
                 write ( 6,  * ) ' j2= ', j2, ' j1= ', j1, &
     &                           ' ind_1st= ', ind_1st, ' ind_last= ', ind_last
                 write ( 6,  * ) ' T = ', T
                 CALL ERR_LOG ( 4613, IUER, 'MALO_INTRP_SPR', 'Failure '// &
     &               'in an attempt to compute lapse rate' ) 
                 RETURN 
            END IF
!
            CALL ERR_PASS ( IUER, IER ) 
            CALL MAKE_SPLINE ( 3, L_LEV, H, P_LOG, 0.0D0, 0.0D0, P_SPL, H_ARR, IER )
            IF ( IER .NE. 0 ) THEN
                 CALL ERR_LOG ( 4614, IUER, 'MALO_COMP_SPR', 'Failure in an attempt '// &
     &                'to compute coefficients of the interpolating spline' )
                 RETURN 
            END IF
!
            DO 440 J4=1,M_LEV
               H_ARR(J4) = HEI_MIN_INT + (J4-1)*(HEI_MAX_INT - HEI_MIN_INT)/(M_LEV-1)
               HEI_R4(J4) = H_ARR(J4)
               IF ( H_ARR(J4) < H(1) ) THEN
                    TEMP = T(1) + T_RATE * (H_ARR(J4) - H(1))
                    G = GE*(1.D0 - 2.D0/REA__WGS84*(1.0D0 + OMEGA__EGM96**2*REA__WGS84**2* &
     &                     (1.D0 - FLAT__WGS84)/GM__EGM96 + (1.D0 - 2.D0*FLAT__WGS84)* &
     &                     DSIN(PHI)**2)*H_ARR(J4) + 3.D0/REA__WGS84**2*H_ARR(J4)**2)
!
                    P_ARR(J4) = PRES_LEV(1)*(TEMP/T(1))**(-G*MD/(R__MAPL*T_RATE))
                 ELSE IF ( H_ARR(J4) > H(L_LEV) ) THEN
                    T_RATE = 2.8D-3
                    TEMP = T(L_LEV) + T_RATE * (H_ARR(J4) - H(L_LEV))
                    G = GE*(1.D0 - 2.D0/REA__WGS84*(1.0D0 + OMEGA__EGM96**2*REA__WGS84**2* &
     &                     (1.D0 - FLAT__WGS84)/GM__EGM96 + (1.D0 - 2.D0*FLAT__WGS84)* &
     &                     DSIN(PHI)**2)*H_ARR(J4) + 3.D0/REA__WGS84**2*H_ARR(J4)**2)
!
                    P_ARR(J4) = PRES_LEV(L_LEV)*(TEMP/T(L_LEV))**(-G*MD/(R__MAPL*T_RATE))
                 ELSE
                    IND = IXMN8 ( L_LEV, H, H_ARR(J4) )
                    P_ARR(J4) = DEXP ( FSPL8 ( H_ARR(J4), L_LEV, H, P_LOG, IND, P_SPL )  )
               END IF
               PRS(J4,J2,J1) = DLOG( P_ARR(J4) )
 440        CONTINUE 
 420     CONTINUE 
         PRS(1:L_LEV,L_LON+1,J1) = PRS(1:L_LEV,1,J1)
         LON_R4(L_LON+1) = PI2
 410  CONTINUE 
!!      CALL PLOT_GRID_R4 ( 1, 7, 0, 1, L_LON, L_LAT, SURF, ' ', 'K/km', '/tmp/boo', IER ) ! %%%%
!
! --- Compute coefficients of 3D interpolating B-splines
!
      DIMS(1) = M_LEV
      DIMS(2) = L_LON+1
      DIMS(3) = L_LAT
! 
!!   write ( 6, * ) ' lon_r4= ', lon_r4 ! %%%%
!
      CALL ERR_PASS ( IUER, IER )
      CALL BSPL4_3D_CMP ( MALO__MDEG, 0, DIMS, HEI_R4, LON_R4, LAT_R4, &
     &                    PRS, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 4617, IUER, 'MALO_COMP_SPR', 'Failure in an attempt '// &
     &         'to compute coefficients of the 3D interpolating spline' )
           RETURN 
      END IF
!
      MAL%MJD_BEG = HEB_H%MJD
      MAL%TAI_BEG = HEB_H%TAI
!
      INDS(3) = 1
      DO 450 J5=1,MAL%NLAT
         LAT_VAL = -P2I + (J5-1)*PI__NUM/(MAL%NLAT-1)
         INDS(3) = IXMN4_S ( INDS(3), L_LAT, LAT_R4, LAT_VAL )
         IF ( LAT_VAL .LE. LAT_R4(1)     ) INDS(3) = 1
         IF ( LAT_VAL .GE. LAT_R4(L_LAT) ) INDS(3) = L_LAT-1
         INDS(2) = 1
         DO 460 J6=1,MAL%NLON
            LON_VAL = (J6-1)*PI2/MAL%NLON
            INDS(2) = IXMN4_S ( INDS(2), L_LON+1, LON_R4, LON_VAL )
            IF ( LON_VAL .LE. LON_R4(1)       ) INDS(2) = 1
            IF ( LON_VAL .GE. LON_R4(L_LON+1) ) INDS(2) = L_LON
            IF ( TEST_STR == 'intrp' ) THEN
                 HEB_OH%VAL(J6,J5,1,1) = HEB_G%VAL(J6,J5,1,1)
            END IF
            INDS(1) = IXMN4   ( M_LEV, HEI_R4, HEB_OH%VAL(J6,J5,1,1) )
            ARGS(1) = HEB_OH%VAL(J6,J5,1,1)
            ARGS(2) = LON_VAL
            ARGS(3) = LAT_VAL
            MAL%SPR(J6,J5,1) = EXP ( VAL_3D_BSPL4 ( ARGS, MALO__MDEG, DIMS, INDS, &
     &                                              HEI_R4, LON_R4, LAT_R4, PRS ) )
            IF ( TEST_STR == 'diff' ) THEN
                 MAL%SPR(J6,J5,1) = MAL%SPR(J6,J5,1) - PRS_SUR(J6,J5)
               ELSE IF ( TEST_STR == 'spr' .OR. TEST_STR == 'intrp' ) THEN
                 MAL%SPR(J6,J5,1) = PRS_SUR(J6,J5)
            END IF
 460    CONTINUE 
 450 CONTINUE 
!
      DEALLOCATE ( PRS ) 
      DEALLOCATE ( SURF ) 
      DEALLOCATE ( LAT_R4 ) 
      DEALLOCATE ( LON_R4 ) 
      IF ( TEST_STR == 'timer' ) THEN
           CALL WALL_TIMER ( STR ) 
           WRITE ( 6, '(A)' ) 'Perform 3D B-spline intrpolation: '//STR(1:I_LEN(STR)-5)
         ELSE IF ( TEST_STR == 'diff'  .OR.  TEST_STR == 'spr'  .OR.  TEST_STR == 'intrp' ) THEN
           DEALLOCATE ( PRS_SUR )
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  MALO_INTRP_SPR  !#!#
