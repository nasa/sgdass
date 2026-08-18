      PROGRAM   EQUILIBRIUM_TIDE_LOADING
! ************************************************************************
! *                                                                      *
! *   Routine EQUILIBRIUM_TIDE_LOADING
! *
! * $MALO_DIR/bin_static/equilibrium_tide_loading load 2 $MALO_DIR/share/toc_grid_d2699.cnf /imsl/oper_model/eqt_sinlam_d2699.heb
! *                                                                      *
! * ## 15-MAY-2014 EQUILIBRIUM_TIDE_LOADING v3.0 L. Petrov 06-JUN-2017 # *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'malo.i'
      INCLUDE   'fourpack.i'
      INCLUDE   'fourpack_constants.i'
      INCLUDE   'fftw3.f'
      INCLUDE   'heb.i'
      TYPE     ( HEB__TYPE  ) :: HEB_LS, HEB_OUT
      TYPE     ( MALO__TYPE ), POINTER :: MAL(:)
      REAL*8       K20_LOVE, H20_LOVE, GE, LS_SHARE_MAX, RHO_SEA_WATER, OMEGA, REA
      PARAMETER  ( K20_LOVE = 0.29525D0 )
      PARAMETER  ( H20_LOVE = 0.6078D0  )
      PARAMETER  ( GE       = ACC_EQU__WGS84 )
      PARAMETER  ( OMEGA    = OM__EAR    )
      PARAMETER  ( REA      = REA__WGS84 )
      PARAMETER  ( LS_SHARE_MAX = 0.95D0 )
      PARAMETER  ( RHO_SEA_WATER = 1034.0D0 )
      CHARACTER  STR*128, FIL_CNF*128, FIL_LS*128, FIL_IN*128, FILOUT*128, &
     &           LOAD_TYPE*8, FRAME_STR*128
      REAL*8,    ALLOCATABLE :: SPR_R8(:,:), SPH_HAR(:,:,:), DSP_ARR3(:,:,:)
      REAL*8     AMPL, PHI_GDT, PHI_GCN, AREA, VOL, TOT_AREA, SEA_AREA, MC, &
     &           DSP_VEC(3), LAT_VAL, LON_VAL, LAM
      REAL*4     VAL_MIN, VAL_MAX
      LOGICAL*1  FL_LAT_NP_POLE 
      INTEGER*8  FSH
      INTEGER*4  M_ITER
      PARAMETER ( M_ITER = 8 )
      INTEGER*4  DEG, NORM, IPHS, J1, J2, J3, J4, J5, J6, J7, J8, J9, J10, &
     &           MODE, MLAT, IVRB, IUER
      INTEGER*4, EXTERNAL :: ILEN, I_LEN
      INTEGER*8, EXTERNAL :: SPHE_INIT 
!
      IVRB = 3
      IF ( IARGC() < 4 ) THEN
           WRITE ( 6, * ) 'Usage equilibrium_tide_loading load_type mode fil_conf fil_out_loa'
           CALL  EXIT ( 0 )
         ELSE
           CALL GETARG ( 1, LOAD_TYPE )
           CALL GETARG ( 2, STR     )
           CALL CHIN   ( STR, MODE  )
           CALL GETARG ( 3, FIL_CNF )
           CALL GETARG ( 4, FILOUT  )
      END IF
      IF ( MODE == 0 .OR. MODE == 1 .OR. MODE == 2 ) THEN
           CONTINUE 
         ELSE 
           IUER = -1
           CALL ERR_LOG ( 8001, IUER, 'EQUILIBRIUM_TIDE_LOADING', 'Unsupported '// &
     &         'order: '//STR(1:I_LEN(STR))//' -- only 0, 1 or 2 are supported' )
           CALL EXIT ( 1 )
      END IF
!
      IF ( LOAD_TYPE == "load" ) THEN
           CONTINUE 
         ELSE IF ( LOAD_TYPE == "load_d1" ) THEN
           CONTINUE 
         ELSE 
           IUER = -1
           CALL ERR_LOG ( 8002, IUER, 'EQUILIBRIUM_TIDE_LOADING', 'Unsupported '// &
     &         '1st argument: '//LOAD_TYPE//' -- only load or load_d1 are '// &
     &         'supported' ) 
           CALL EXIT ( 1 )
      END IF
!
! --- Initialize MALO object
!
      ALLOCATE ( MAL(1), STAT=IUER )
      IF ( IUER .NE. 0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 8003, IUER, 'EQUILIBRIUM_TIDE_LOADING', 'Error in an '// &
     &         'attempt to allocate memory for object MALO' )
           CALL EXIT ( 1 )
      END IF
!
      IUER = -1
      CALL MALO_INIT ( MAL(1), IUER )
      IF ( IUER .NE. 0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 8004, IUER, 'EQUILIBRIUM_TIDE_LOADING', 'Error in an attempt '// &
     &         'to initialize object MALO' )
           CALL EXIT ( 1 )
      END IF
!
! --- Read MALO configuration file
!
      IUER = -1
      CALL MALO_CONFIG ( FIL_CNF, MAL(1), IUER )
      IF ( IUER .NE. 0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 8005, IUER, 'EQUILIBRIUM_TIDE_LOADING', 'Failure in parsing MALO '// &
     &         'configuration file '//FIL_CNF )
           CALL EXIT ( 1 )
      END IF
!
      DEG = MAL(1)%CONF%OUTPUT_GRID_DEG
      FIL_LS = MAL(1)%CONF%FINAM_LS_MASK
      NORM = 1
      IPHS = 1
!
! --- Read land-sea mask
!
      IUER = -1
      CALL READ_HEB ( FIL_LS, HEB_LS, IUER ) 
      IF ( IUER .NE. 0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 8006, IUER, 'EQUILIBRIUM_TIDE_LOADING', 'Error in '// &
     &         'reading input land-sea HEB-file '//FIL_LS )
           CALL EXIT ( 1 )
      END IF
      MAL(1)%NLON = HEB_LS%DIMS(1)
      MAL(1)%NLAT = HEB_LS%DIMS(2)
      IF ( DABS ( DLOG(MAL(1)%NLAT-1.D0)/DLOG(2.0D0) - IDINT(DLOG(MAL(1)%NLAT-1.D0)/DLOG(2.0D0)) ) < 1.0D-5 ) THEN
           FL_LAT_NP_POLE = .TRUE.
           MLAT = MAL(1)%NLAT
         ELSE 
           FL_LAT_NP_POLE = .FALSE.
           MLAT = MAL(1)%NLAT - 1
      END IF
      IF ( IVRB .GE. 3 ) THEN
           WRITE ( 6, * ) 'FL_LAT_NP_POLE= ', FL_LAT_NP_POLE, ' MLAT= ', MLAT
      END IF
!
! --- Read Love numbers
!
      IUER = -1
      CALL READ_LOVE ( MAL(1), IUER ) 
      IF ( IUER .NE. 0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 8009, IUER, 'EQUILIBRIUM_TIDE_LOADING', 'Error in '// &
     &         'an attempt to read a file with Love numbers '// &
     &          MAL(1)%CONF%LOVE_FILE )
           CALL EXIT ( 1 )
      END IF   
!
! --- Initialization for spherical harmonics package
!
      IUER = -1
      FSH = SPHE_INIT ( -1, IUER )
      IF ( IUER .NE. 0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 8010, IUER, 'EQUILIBRIUM_TIDE_LOADING', 'Error in an '// &
     &         'attempt to initialize FSH object for spherical '// &
     &         'harmonics transform' )
           CALL EXIT ( 1 )
      END IF   
!
! --- Allocate memory for the output file
!
      HEB_OUT = HEB_LS
      HEB_OUT%DIMS(3) = 3
      ALLOCATE ( HEB_OUT%VAL(HEB_OUT%DIMS(1),HEB_OUT%DIMS(2),HEB_OUT%DIMS(3),HEB_OUT%DIMS(4)), &
     &           STAT=IUER )
      IF ( IUER .NE. 0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 8011, IUER, 'EQUILIBRIUM_TIDE_LOADING', 'Cannot '// &
     &         'allocate dynamic memory for HEB_OUT%VAL' )
           CALL EXIT ( 1 )
      END IF
      HEB_OUT%VAL = 0.0
!
! --- Compute the volume of the ocean tide counting its sign VOL
! --- Compute the total Earth's area  TOT_AREA
! --- Compute the sea area            SEA_AREA
!
      VOL = 0.0D0
      TOT_AREA = 0.0D0
      SEA_AREA = 0.0D0
!
      ALLOCATE ( SPR_R8(MAL(1)%NLON,MAL(1)%NLAT), STAT=IUER )
      IF ( IUER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( 8*MAL(1)%NLON,MAL(1)%NLAT, STR )
           IUER = -1
           CALL ERR_LOG ( 8012, IUER, 'EQUILIBRIUM_TIDE_LOADING', 'Error in '// &
     &         'an attempt to allocate '//STR(1:I_LEN(STR))// &
     &         ' bytes of dynamic memory for array SPH_R8' )
           RETURN 
      END IF
!
      ALLOCATE ( SPH_HAR(2,0:DEG,0:DEG), STAT=IUER )
      IF ( IUER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( 8*(DEG+1)**2, STR )
           IUER = -1
           CALL ERR_LOG ( 8013, IUER, 'EQUILIBRIUM_TIDE_LOADING', 'Error in '// &
     &         'an attempt to allocate '//STR(1:I_LEN(STR))// &
     &         ' bytes of dynamic memory for array SPH_HAR' )
           CALL EXIT ( 1 )
      END IF
!
      ALLOCATE ( MAL(1)%SPH(2,0:DEG,0:DEG,2,1), STAT=IUER )
      IF ( IUER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( 2*8*(DEG+1)**2, STR )
           IUER = -1
           CALL ERR_LOG ( 8014, IUER, 'EQUILIBRIUM_TIDE_LOADING', 'Error in '// &
     &         'an attempt to allocate '//STR(1:I_LEN(STR))// &
     &         ' bytes of dynamic memory for array MAL(1)%SPH' )
           CALL EXIT ( 1 )
      END IF
!
      ALLOCATE ( DSP_ARR3(MAL(1)%NLON,MAL(1)%NLAT,3), STAT=IUER )
      IF ( IUER .NE. 0 ) THEN
           CALL CLRCH  ( STR )
           CALL IINCH8 ( INT8(8*3)*INT8(MAL(1)%NLON)*INT8(MLAT), STR )
           IUER = -1
           CALL ERR_LOG ( 8015, IUER, 'EQUILIBRIUM_TIDE_LOADING', 'Failure to '// &
     &         'allocate '//STR(1:I_LEN(STR))//' bytes of dymanic '// &
     &         'memory' )
           CALL EXIT ( 1 )
      END IF
      DSP_ARR3 = 0.0D0
      MAL(1)%SPH_STATUS = MALO__ALLO
!
      DO 410 J1=1,M_ITER
         DO 420 J2=1,HEB_LS%DIMS(2)
            IF ( FL_LAT_NP_POLE ) THEN
                 PHI_GDT = -P2I + (J2-1)*PI__NUM/(HEB_LS%DIMS(2)-1)
               ELSE 
                 PHI_GDT = -P2I + (J2-1)*PI__NUM/HEB_LS%DIMS(2)
            END IF
            PHI_GCN = DATAN ( (1.D0 - FLAT__WGS84)**2 * DTAN(PHI_GDT) )
!
! --------- Compute the amplitude of the equllibrium ocean tide
!
            IF ( MODE == 0   ) THEN
                 AMPL = DSQRT(5.0D0/(4.0D0*PI__NUM))*(1.5D0*DSIN(PHI_GCN)**2 - 0.5D0)* &
     &                  (1.D0 + K20_LOVE - H20_LOVE)/GE 
            END IF
            AREA = PI__NUM/HEB_LS%DIMS(2)*PI2/HEB_LS%DIMS(1)*DCOS(PHI_GDT)
            DO 430 J3=1,HEB_LS%DIMS(1)
               LAM = (J3-1)*PI2/HEB_LS%DIMS(1)
               IF ( MODE == 1 ) THEN
                    AMPL = -OMEGA**2*REA**2*DSIN(PHI_GCN)*DCOS(PHI_GCN)*DCOS(LAM)* &
     &                      (1.D0 + K20_LOVE - H20_LOVE)/GE 
                  ELSE IF ( MODE == 2 ) THEN
                    AMPL =  OMEGA**2*REA**2*DSIN(PHI_GCN)*DCOS(PHI_GCN)*DSIN(LAM)* &
     &                      (1.D0 + K20_LOVE - H20_LOVE)/GE 
               END IF
               VOL = VOL + AREA*(AMPL + DSP_ARR3(J3,J2,1))* &
   &                            (1.D0 - HEB_LS%VAL(J3,J2,1,1))
               TOT_AREA = TOT_AREA + AREA
               SEA_AREA = SEA_AREA + AREA*(1.D0 - HEB_LS%VAL(J3,J2,1,1))
 430        CONTINUE 
 420     CONTINUE 
!
! ------ Compute the mass conservation correction
!
         MC = VOL/SEA_AREA
!
! ------ Compute the equillibrium ocean tide once again taking into account 
! ------ the mass conservation correction and mass loading deformation
! ------ and put in in HEB_OUT%VAL
!
         DO 440 J4=1,HEB_LS%DIMS(2)
            IF ( FL_LAT_NP_POLE ) THEN
                 PHI_GDT = -P2I + (J4-1)*PI__NUM/(HEB_LS%DIMS(2)-1)
              ELSE 
                PHI_GDT = -P2I + (J4-1)*PI__NUM/HEB_LS%DIMS(2)
            END IF
            PHI_GCN = DATAN ( (1.D0 - FLAT__WGS84)**2 * DTAN(PHI_GDT) )
            IF ( MODE == 0 ) THEN
                 AMPL = DSQRT(5.0D0/(4.0D0*PI__NUM))*(1.5D0*DSIN(PHI_GCN)**2 - 0.5D0)* &
     &                  (1.D0 + K20_LOVE - H20_LOVE)/GE
            END IF
            DO 450 J5=1,HEB_LS%DIMS(1)
               LAM = (J5-1)*PI2/HEB_LS%DIMS(1)
               IF ( MODE == 1 ) THEN
                    AMPL = -OMEGA**2*REA**2*DSIN(PHI_GCN)*DCOS(PHI_GCN)*DCOS(LAM)* &
     &                      (1.D0 + K20_LOVE - H20_LOVE)/GE
                  ELSE IF ( MODE == 2 ) THEN
                    AMPL =  OMEGA**2*REA**2*DSIN(PHI_GCN)*DCOS(PHI_GCN)*DSIN(LAM)* &
     &                      (1.D0 + K20_LOVE - H20_LOVE)/GE
               END IF
               IF ( HEB_LS%VAL(J5,J4,1,1) < LS_SHARE_MAX ) THEN
!
! ----------------- Fill the surface pressure
!
                    SPR_R8(J5,J4) = (AMPL + DSP_ARR3(J5,J4,1) - MC)* &
     &                              (1.D0 - HEB_LS%VAL(J5,J4,1,1))*RHO_SEA_WATER*GE
                  ELSE
                    SPR_R8(J5,J4) = 0.0D0
               END IF
               HEB_OUT%VAL(J5,J4,1,1) = SPR_R8(J5,J4) 
 450        CONTINUE 
 440     CONTINUE 
         IF ( IVRB .GE. 7 ) THEN
              VAL_MIN =  1.0
              VAL_MAX = -1.0
              CALL PLOT_GRID_R4 ( 1, 7, 0, 1, MAL(1)%NLON, MAL(1)%NLAT, HEB_OUT%VAL, &
        &                         'EQT', 'hz', VAL_MIN, VAL_MAX, '/tmp/boo', iuer )
         END IF
         IUER = -1
         IF ( IVRB .GE. 1 ) THEN
              WRITE ( 6, 110 ) J1, DEG
 110          FORMAT ( 'Iteration ', I1, '. Start spherical harmonic transform of degeree ', I5 )
         END IF
         IF ( IVRB .GE. 3 ) THEN
              WRITE ( 6, 120 ) MC
 120          FORMAT ( 'Global water conservation correction: ', F12.5 )
         END IF
!
         IF ( J1 == 1 .AND. IVRB .GE. 4 ) THEN
              WRITE ( 6, * ) 'DEG=        ', DEG
              WRITE ( 6, * ) 'SEA_AREA=   ', SEA_AREA
              WRITE ( 6, * ) 'TOT_AREA=   ', TOT_AREA
              WRITE ( 6, * ) 'VOL=        ', VOL
              WRITE ( 6, * ) 'NLON/NLAT=  ', MAL(1)%NLON, MAL(1)%NLAT
              WRITE ( 6, * ) 'MC=         ', MC
              WRITE ( 6, * ) 'COEF=       ', OMEGA**2*REA**2*(1.D0 + K20_LOVE - H20_LOVE)/GE 
         END IF
!
! ------ Perform spherical harmonics transform
!
         CALL SPHE_DIR_2NN ( %VAL(FSH), MLAT, SPR_R8(1,1), DEG, DEG, NORM, &
     &                       IPHS, SPH_HAR(1,0,0), IUER )
         IF ( IUER .NE. 0 ) THEN
              IUER = -1
              CALL ERR_LOG ( 8016, IUER, 'EQUILIBRIUM_TIDE_LOADING', 'Error in '// &
     &             'an attempt to perform spherical transform of the '// &
     &             'pressure field' )
              CALL EXIT ( 1 )
         END IF
         MAL(1)%SPH_STATUS = MALO__LOAD
         IF ( IVRB .GE. 3 ) THEN
              WRITE ( 6, '(A)' ) 'Finished spherical harmonic transform'
         END IF
!
! ------ Scale harmonics of the spherical harmonics transform
!
         DO 480 J8=0,DEG
            DO 490 J9=J8,DEG
               DO 4100 J10=1,2
                  IF ( J1 < M_ITER ) THEN
!
! -------------------- Sea level change due in a response to the additional 
! -------------------- potential arising from redistribution of the oceans 
! -------------------- and their altered loading of the Earth
!
                       MAL(1)%SPH(J10,J8,J9,1,1) = SPH_HAR(J10,J9,J8)* &
     &                                             3.0D0*(1.0D0 + MAL(1)%LOVE(J9,MALO__K) - MAL(1)%LOVE(J9,MALO__H))/(2*J9+1)/ &
     &                                             (MALO__DENS*MALO__GRAV)
                       MAL(1)%SPH(J10,J8,J9,2,1) = 0.0D0
                     ELSE 
!
! -------------------- Last iteration. Crust deformation
!
                       MAL(1)%SPH(J10,J8,J9,1,1) = SPH_HAR(J10,J9,J8)* &
     &                                             3.0D0*MAL(1)%LOVE(J9,MALO__H)/(2*J9+1)/ &
     &                                             (MALO__DENS*MALO__GRAV)
                       MAL(1)%SPH(J10,J8,J9,2,1) = SPH_HAR(J10,J9,J8)* &
     &                                             3.0D0*MAL(1)%LOVE(J9,MALO__L)/(2*J9+1)/ &
     &                                             (MALO__DENS*MALO__GRAV)
                  END IF
!
                  MAL(1)%SPH(J10,J9,J8,1,1) = MAL(1)%SPH(J10,J8,J9,1,1) 
                  MAL(1)%SPH(J10,J9,J8,2,1) = MAL(1)%SPH(J10,J8,J9,2,1) 
 4100           CONTINUE 
 490        CONTINUE 
 480     CONTINUE 
!
! ------ Perform inverse spherical harmonics tranform
!
         IF ( J1 < M_ITER .OR. LOAD_TYPE == "load" ) THEN
              IUER = -1
              CALL SPHE_INV_2NN_VEC ( %VAL(FSH), DEG, DEG, MAL(1)%SPH(1,0,0,1,1), &
     &                                MAL(1)%NLAT, DSP_ARR3, IUER )
              IF ( IUER .NE. 0 ) THEN
                   IUER = -1
                   CALL ERR_LOG ( 8017, IUER, 'EQUILIBRIUM_TIDE_LOADING', 'Failure '// &
     &                 'in an attempt to perform inverse vector spherical '// &
     &                 'harmonic transform' )
                   CALL EXIT ( 1 )
              END IF
            ELSE IF ( J1 == M_ITER .AND. LOAD_TYPE == 'load_d1' ) THEN
!
! ------------- In a case if compute correction for D1-term, we 
! ------------- unscale spherical harmonic of degree one.
!
                MAL(1)%SPH(1:2,0:1,1,MALO__H,1) = 1.0D0/MAL(1)%LOVE(1,MALO__H)* &
     &                                                  MAL(1)%SPH(1:2,0:1,1,MALO__H,1)
                MAL(1)%SPH(1:2,0:1,1,MALO__L,1) = 1.0D0/MAL(1)%LOVE(1,MALO__L)* &
     &                                                  MAL(1)%SPH(1:2,0:1,1,MALO__L,1)
                MAL(1)%SPH(1:2,0:2,2,MALO__H,1) = 0.0D0
                MAL(1)%SPH(1:2,0:2,2,MALO__L,1) = 0.0D0
!
                MAL(1)%SPH(1:2,1,0,MALO__H,1)  = MAL(1)%SPH(1:2,0,1,MALO__H,1)  
                MAL(1)%SPH(1:2,1,0,MALO__L,1)  = MAL(1)%SPH(1:2,0,1,MALO__L,1)  
!
                IUER = -1
                CALL SPHE_INV_2NN_VEC ( %VAL(FSH), DEG, 2, MAL(1)%SPH(1,0,0,1,1), &
     &                                  MAL(1)%NLAT, DSP_ARR3, IUER )
                IF ( IUER .NE. 0 ) THEN
                     IUER = -1
                     CALL ERR_LOG ( 8018, IUER, 'EQUILIBRIUM_TIDE_LOADING', 'Failure '// &
     &                   'in an attempt to perform inverse vector spherical '// &
     &                    'harmonic transform' )
                     CALL EXIT ( 1 )
                END IF
         END IF
!
         IF ( J1 == M_ITER ) THEN
              DO 460 J6=1,MAL(1)%NLAT
                 DO 470 J7=1,MAL(1)%NLON
                    HEB_OUT%VAL(J7,J6,1,1) = DSP_ARR3(J7,J6,1)
                    HEB_OUT%VAL(J7,J6,2,1) = DSP_ARR3(J7,J6,2)
                    HEB_OUT%VAL(J7,J6,3,1) = DSP_ARR3(J7,J6,3)
 470             CONTINUE
 460          CONTINUE 
         END IF
 410  CONTINUE 
      IF ( IVRB .GE. 1 ) THEN
           WRITE ( 6, * ) 'Finished Loading computation'
      END IF
!
! --- Prepare comments and other header information
!
      IF ( INDEX ( MAL(1)%CONF%LOVE_FILE, '_cm' ) > 0 ) THEN
           FRAME_STR = 'at the center of the Earth total mass coordinate system'
         ELSE IF ( INDEX ( MAL(1)%CONF%LOVE_FILE, '_cf' ) > 0 ) THEN
           FRAME_STR = 'at the center of the solid Earth mass coordinate system'
         ELSE
           CALL CLRCH ( FRAME_STR )
      END IF 
      HEB_OUT%TITLE       = 'Eqillibrium tidal admittance'
      HEB_OUT%SDS_NAME    = 'Eqillibrium tidal admittance '//FRAME_STR(1:I_LEN(FRAME_STR))
      HEB_OUT%PROD_NAME   = 'Eqillibrium tidal admittance '//FRAME_STR(1:I_LEN(FRAME_STR))
      HEB_OUT%INSTITUTION = 'Astrogeo Center'
      HEB_OUT%UNITS       = 'm'
!
! --- Find the mininmum and maximum value
!
      CALL HEB_MINMAX ( HEB_OUT, HEB_OUT%VAL, 1000.0 )
!
! --- Write down the output file with mass loading caused by
! --- the equillibroum ocean mass loading
!
      IUER = -1
      CALL WRITE_HEB ( HEB_OUT, HEB_OUT%VAL, FILOUT, IUER )
      IF ( IUER .NE. 0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 8019, IUER, 'EQUILIBRIUM_TIDE_LOADING', 'Failure '// &
     &         'in an attempt to write into output file '//FILOUT )
           CALL EXIT ( 1 )
      END IF
      IF ( IVRB .GE. 1 ) THEN
           WRITE ( 6, * ) 'Write into the output file '//FILOUT(1:I_LEN(FILOUT))
      END IF
!
      END  PROGRAM  EQUILIBRIUM_TIDE_LOADING  !#!  
