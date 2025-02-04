      PROGRAM    TOC_BIG
! ************************************************************************
! *                                                                      *
! *   Program TOC_BIG
! *                                                                      *
! *  ### 22-JAN-2016    TOC_BIG    v1.2 (c)  L. Petrov  03-JUN-2017 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'malo.i'
      INCLUDE   'malo_local.i'
      INCLUDE   'heb.i'
      INCLUDE   'fourpack.i'
      INCLUDE   'fourpack_constants.i'
      INCLUDE   'fftw3.f'
      TYPE     ( MALO__TYPE ) :: MALO
      TYPE     ( HEB__TYPE  ) :: HEB_MOD, HEB_LS, HEB_DSPL
      INTEGER*8  FSH
      CHARACTER  CONFIG_FILE*128, FILIN_HEB*128, FILCOM*128, &
     &           FILDSC*128, FILOUT*128, FILNC*128, FILHEB*128, FILHPS*128, &
     &           FRQ_STR*1024, FOLLOWUP_COM*1024, WISDOM_FILE*128
      CHARACTER  STR*128, STR1*128, LOAD_TYPE*8, NUM_THR_STR*128, EXT*8
      LOGICAL*1  LEX
      REAL*8,    ALLOCATABLE :: SPR_R8(:,:), DSP_ARR(:,:,:,:), DSPL_ARR(:,:,:,:), &
     &           DSP_ARR3_R8(:,:,:), SPH_MD(:,:,:,:)
      REAL*4,    ALLOCATABLE :: DSP_ARR_R4(:,:,:,:,:), SPR_R4(:,:), ARR_TMP_R4(:,:,:,:), &
     &           DSP_ARR3_R4(:,:,:)
      REAL*8     LAT_VAL, LON_VAL, RES(3), LAT_GDT, GE, TIM_R8, MALO__MAX_VAL 
      PARAMETER  ( MALO__MAX_VAL = 1.D6 )
      REAL*4     MOD_VAL
      INTEGER*4  M_FRQ
      PARAMETER  ( M_FRQ = (MALO__MFRQ-1)*2 + 1 )
      INTEGER*4  DEG, NORM, IPHS, J1, J2, J3, J4, J5, J6, J7, J8, J9, &
     &           J10, J11, J12, J13, J14, J15, J16, J17, J18, J19, &
     &           L_LON, L_LAT, IP, ID, IS, IVRB, L_FRQ, MD, MR, MLON, MLAT, &
     &           IND_LON, IND_LAT, IND_4, I_WAV, IND_HEB(MALO__MWAV), &
     &           IND_FRQ(MALO__MWAV), NUM_CMP(MALO__MWAV), L_CMP, NUM_THR, &
     &           IND3_SECT(2), IND4_SECT(2), IND_MOD, &
     &           IX_FRQ(MALO__MWAV), IX_CMP(MALO__MWAV), IUER
      INTEGER*8  NEL_I8
      CHARACTER  WAVE_FRQ(MALO__MWAV)*4, C_CMP(2)*1
      DATA       C_CMP / 'c', 's' /
      COMPLEX*16 CMPL_AMP_FRQ(MALO__MWAV)
      CHARACTER, EXTERNAL :: GET_CDATE*19
      INTEGER*4, EXTERNAL :: ILEN, I_LEN, LINDEX, SYSTEM
      INTEGER*8, EXTERNAL :: SPHE_INIT, SPHE_INIT_PLAN
      REAL*8,    EXTERNAL :: WALL_TIMER
!
      IVRB = 1
      CALL CLRCH ( FILDSC )
      CALL CLRCH ( FILCOM )
      CALL CLRCH ( FOLLOWUP_COM )
!
      IF ( IARGC() < 4 ) THEN
           WRITE ( 6, '(A)' ) 'Usage: malo_loading_model load|load_d1 malo_config input_heb '//&
     &                        ' output_file [fildsc] [filcom] [ivrb] [command]'
           CALL EXIT ( 1 )
         ELSE 
           CALL GETARG ( 1, LOAD_TYPE   )
           IF ( LOAD_TYPE == 'load' ) THEN
                CONTINUE 
              ELSE  IF ( LOAD_TYPE == 'load_d1' ) THEN
                CONTINUE 
              ELSE
                IUER = -1
                CALL ERR_LOG ( 6301, IUER, 'TOC_BIG', 'Wrong first '// &
     &              'argument '//LOAD_TYPE//' one of load or load_d1 were '// &
     &              'expected' )
                CALL EXIT ( 1 )
           END IF
!
           CALL GETARG ( 2, CONFIG_FILE )
           INQUIRE ( FILE=CONFIG_FILE, EXIST=LEX )
           IF ( .NOT. LEX ) THEN
                IUER = -1
                CALL ERR_LOG ( 6302, IUER, 'TOC_BIG', 'Cannot '// &
     &             'find malo configuration file '//CONFIG_FILE )
                CALL EXIT ( 1 )
           END IF
!
           CALL GETARG ( 3, FILIN_HEB )
           IUER = -1
           CALL MALO_CHECK_SHARE_FILE ( FILIN_HEB, IUER )
           IF ( IUER .NE. 0 ) THEN
                IUER = -1
                CALL ERR_LOG ( 6303, IUER, 'TOC_BIG', &
     &              'Cannot find infput model file '//FILIN_HEB )
                CALL EXIT ( 1 )
           END IF
!
           CALL GETARG ( 4, FILOUT )
           CALL CLRCH ( EXT  )
           IP = LINDEX ( FILOUT, '.' ) 
           IF ( IP > 1 ) THEN
                EXT = FILOUT(IP:)
           END IF
           IF ( EXT == '.nc' ) THEN
                FILNC  = FILOUT
                FILHEB = FILOUT(1:I_LEN(FILOUT)-3)//'.heb'
              ELSE IF ( EXT == '.heb' ) THEN
                FILHEB = FILOUT
                FILNC  = FILOUT(1:I_LEN(FILOUT)-4)//'.nc'
                CONTINUE 
              ELSE IF ( EXT == '.hps' ) THEN
                FILHPS = FILOUT
              ELSE IF ( EXT == '.all' ) THEN
                FILHEB = FILOUT(1:I_LEN(FILOUT)-4)//'.heb'
                FILNC  = FILOUT(1:I_LEN(FILOUT)-4)//'.nc'
                FILHPS = FILOUT(1:I_LEN(FILOUT)-4)//'.hps'
                CONTINUE 
              ELSE 
                IUER = -1
                CALL ERR_LOG ( 6304, IUER, 'TOC_BIG', 'Unsupported '// &
     &              'output file name extension '//EXT(1:I_LEN(EXT))// &
     &              ' . Supported extensions: .nc, .heb, .hps, .all' )
                CALL EXIT ( 1 )
           END IF
           IF ( IARGC() .GE. 5 ) THEN
                CALL GETARG ( 5, FILDSC )
                CALL MALO_CHECK_SHARE_FILE ( FILDSC, IUER )
                IF ( IUER .NE. 0 ) THEN
                     IUER = -1
                     CALL ERR_LOG ( 6305, IUER, 'TOC_BIG', &
               &         'Cannot find data source description file '//FILDSC )
                     CALL EXIT ( 1 )
                END IF
           END IF
           IF ( IARGC() .GE. 6 ) THEN
                CALL GETARG ( 6, FILCOM )
                CALL MALO_CHECK_SHARE_FILE ( FILCOM, IUER )
                IF ( IUER .NE. 0 ) THEN
                     IUER = -1
                     CALL ERR_LOG ( 6306, IUER, 'TOC_BIG', &
               &         'Cannot find loading description file '//FILCOM )
                     CALL EXIT ( 1 )
                END IF
           END IF
           IF ( IARGC() .GE. 7 ) THEN
                CALL GETARG ( 7, STR )
                CALL CHIN   ( STR, IVRB )
           END IF
           IF ( IARGC() .GE. 8 ) THEN
                CALL GETARG ( 8, FOLLOWUP_COM )
           END IF
      END IF
!
      IUER = -1
      CALL MALO_CHECK_SHARE_FILE ( CONFIG_FILE, IUER )
      IF ( IUER .NE. 0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 6307, IUER, 'TOC_BIG', &
     &         'Cannot find configuration file '//CONFIG_FILE )
           CALL EXIT ( 1 )
      END IF
!
      IUER = -1
      CALL MALO_CONFIG ( CONFIG_FILE, MALO, IUER )
      IF ( IUER .NE. 0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 6308, IUER, 'TOC_BIG', 'Failure in '// &
     &         'an attempt to read and parse configuration file '// &
     &         CONFIG_FILE(1:I_LEN(CONFIG_FILE))//' in heb format' )
           CALL EXIT ( 1 )
      END IF
!
      IF ( ILEN(FILDSC) > 0 ) MALO%CONF%LOA_FINAM_DESCR = FILDSC
      IF ( ILEN(FILCOM) > 0 ) MALO%CONF%LOA_FINAM_COMM  = FILCOM
!
      IUER = -1
      CALL READ_LOVE ( MALO, IUER ) 
      IF ( IUER .NE. 0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 6309, IUER, 'TOC_BIG', 'Error in '// &
     &         'an attempt to read a file with Love numbers '// &
     &          MALO%CONF%LOVE_FILE )
           CALL EXIT ( 1 )
      END IF   
!
      IUER = -1
      CALL READ_HEB_HEADER ( FILIN_HEB, HEB_MOD, IUER )
      IF ( IUER .NE. 0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 6310, IUER, 'TOC_BIG', &
     &         'Failure in an attempt to read and parse input file '// &
     &          FILIN_HEB(1:I_LEN(FILIN_HEB))//' in heb format' )
           CALL EXIT ( 1 )
      END IF
!
      IF ( MALO%CONF%SURFACE_TYPE == MALO__LAND  .OR. &
     &     MALO%CONF%SURFACE_TYPE == MALO__OCEAN      ) THEN
!
           IUER = -1
           CALL MALO_CHECK_SHARE_FILE ( MALO%CONF%FINAM_LS_MASK, IUER )
           IUER = -1
           CALL READ_HEB ( MALO%CONF%FINAM_LS_MASK, HEB_LS, IUER ) 
           IF ( IUER .NE. 0 ) THEN
                IUER = -1
                CALL ERR_LOG ( 6313, IUER, 'TOC_BIG', 'Error '// &
     &              'in an attempt to read nc-file with land-sea '// &
     &              'mask '//MALO%CONF%FINAM_LS_MASK )
                CALL EXIT ( 1 )
           END IF   
      END IF
      IF ( IVRB .GE. 2 ) THEN
           WRITE ( 6, '(A)' ) 'TOC_BIG: '// &
     &             'Started computation of loading harmonics coefficients'
           CALL FLUSH ( 6 )
      END IF
!
! --- Check how many threads shoujld we use
!
      CALL GETENVAR ( 'OMP_NUM_THREADS', NUM_THR_STR )
      IF ( ILEN(NUM_THR_STR) > 0 ) THEN
!
! --------- Well, OMP_NUM_THREADS was setup. Read it.
!
            CALL CHIN ( NUM_THR_STR, NUM_THR )
            IF ( NUM_THR < 1 ) NUM_THR = 1
         ELSE 
            NUM_THR = 1
      END IF
      IF ( IVRB .GE. 2 ) THEN
           WRITE ( 6, '(A,I2)' ) 'TOC_BIG: number of threads: ', NUM_THR
           CALL FLUSH ( 6 )
      END IF
!
! --- Build wisdom file name
!
      CALL CLRCH ( WISDOM_FILE )
      CALL INCH  ( NUM_THR, WISDOM_FILE )
      WISDOM_FILE = MALO_SHARE//'/malo_fftw_plan_'// &
     &              WISDOM_FILE(1:I_LEN(WISDOM_FILE))//'thr.wis'
      INQUIRE ( FILE=WISDOM_FILE, EXIST=LEX )
      IF ( .NOT. LEX ) THEN
           WRITE ( 6, '(A)' ) 'WARNING: wisdom file '// &
     &                         WISDOM_FILE(1:I_LEN(WISDOM_FILE))// &
     &                         ' was not found. Nevertheless, continue.'
           CALL CLRCH ( WISDOM_FILE )
      END IF
!
! --- Initialization for spherical harmonics package
!
      IUER = -1
      FSH = SPHE_INIT_PLAN ( WISDOM_FILE, 1, 2.0D0, NUM_THR, IUER )
      IF ( IUER .NE. 0 ) THEN
           CALL ERR_LOG ( 6016, -2, 'TOC_BIG', 'Error in an '// &
     &         'attempt to initialize FSH object for spherical harmonics '// &
     &         'transform'  )
           CALL EXIT ( 1 )
      END IF   
!
      MALO%NLON = 4*(MALO%CONF%OUTPUT_GRID_DEG+1)
      MALO%NLAT = 2*(MALO%CONF%OUTPUT_GRID_DEG+1)+1
      MLON = MALO%NLON
      MLAT = MALO%NLAT
      MD = MALO%CONF%OUTPUT_GRID_DEG
      L_LON     = HEB_MOD%DIMS(1)
      L_LAT     = HEB_MOD%DIMS(2)
      IF ( 2*(L_LAT-1) == L_LON ) THEN
           L_LAT = L_LAT - 1
      END IF
      DEG  = L_LAT/2 - 1
      NORM = 1
      IPHS = 1
      IF ( IVRB .GE. 3 ) THEN
           WRITE ( 6, * ) 'MALO%NLON = ', MALO%NLON, ' MLON= ', MLON
           WRITE ( 6, * ) 'MALO%NLAT = ', MALO%NLAT, ' MLAT= ', MLAT
           WRITE ( 6, * ) 'DEG       = ', DEG,       ' MD=   ', MD
           WRITE ( 6, * ) 'L_LON, L_LAT= ', L_LON, L_LAT, ' NUM_THR= ', NUM_THR
      END IF
!
      IUER = -1
      CALL MALO_WAVE_TABLE ( MALO, HEB_MOD%SDS_NAME, L_FRQ, IND_FRQ, &
     &                       IND_MOD, IND_HEB, NUM_CMP, L_CMP,       &
     &                       IX_CMP, IX_FRQ, CMPL_AMP_FRQ, FRQ_STR,  &
     &                       IVRB, IUER )
      IF ( IUER .NE. 0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 6317, IUER, 'TOC_BIG', 'Error in '// &
     &         'an attempt to assigne cross-reference table to tidal '// &
     &         'waves' )
           CALL EXIT ( 1 )
      END IF
      IF ( INDEX ( HEB_MOD%SDS_NAME, '10 kPa' ) > 0 ) THEN
           NUM_CMP(1) = 1
      END IF
      write ( 6, * ) 'toc_big: ', l_frq, ' num_cmp= ', num_cmp(1)
!
      ALLOCATE ( DSP_ARR3_R8(MLON,MLAT,3), STAT=IUER )
      IF ( IUER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( INT8(3*8)*INT8(MLON)*INT8(MLAT-1), STR )
           IUER = -2
           CALL ERR_LOG ( 6326, IUER, 'TOC_BIG', 'Failure to allocate '// &
     &                    STR(1:I_LEN(STR))//' bytes of dymanic memory' )
           CALL EXIT ( 1 ) 
      END IF
!
      IF ( IVRB == 11 .OR. IVRB == 12 ) THEN
           ALLOCATE ( DSP_ARR_R4(MLON,MLAT,3,1,1), STAT=IUER )
         ELSE
           ALLOCATE ( DSP_ARR_R4(MLON,MLAT,3,2,L_FRQ), STAT=IUER )
      END IF
      IF ( IUER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( INT8(3*8)*INT8(MLON)*INT8(MLAT-1), STR )
           IUER = -2
           CALL ERR_LOG ( 6326, IUER, 'TOC_BIG', 'Failure to allocate '// &
     &                    STR(1:I_LEN(STR))//' bytes of dymanic memory' )
           CALL EXIT ( 1 ) 
      END IF
!
      ALLOCATE ( SPR_R8(L_LON,L_LAT), STAT=IUER )
      IF ( IUER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( 8*L_LON*L_LAT, STR )
           IUER = -1
           CALL ERR_LOG ( 6315, IUER, 'TOC_BIG', 'Error in '// &
     &         'an attempt to allocate '//STR(1:I_LEN(STR))// &
     &         ' bytes of dynamic memory for array SPR_R8' )
           CALL EXIT ( 1 )
      END IF
!
      IF ( IVRB .GE. 8 ) THEN
           WRITE ( 6, * ) 'h5 HEB_MOD = ', HEB_MOD%DIMS
      END IF
      ALLOCATE ( MALO%SPH(2,0:DEG,0:DEG,3,1), STAT=IUER )
      IF ( IUER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( 8*2*3*(DEG+1)**2, STR )
           IUER = -1
           CALL ERR_LOG ( 6316, IUER, 'TOC_BIG', 'Error in '// &
     &         'an attempt to allocate '//STR(1:I_LEN(STR))// &
     &         ' bytes of dynamic memory for array MALO%SPH' )
           CALL EXIT ( 1 )
      END IF
      IF ( IVRB .GE. 8 ) THEN
           WRITE ( 6, * ) 'h6 HEB_MOD = ', HEB_MOD%DIMS
      END IF
!
      IF ( MALO%CONF%OUTPUT_GRID_DEG > 0 ) THEN
           IF ( IVRB .GE. 8 ) THEN
                WRITE ( 6, * ) 'H7 ', MALO%NLON, MALO%NLAT, L_FRQ
                CALL FLUSH ( 6 )
           END IF
         ELSE
           IUER = -1
           CALL MALO_INP_STA ( MALO, MALO%CONF%STATION_FINAM, 0.0D0, IUER )
           IF ( IUER .NE. 0 ) THEN
                IUER = -1
                CALL ERR_LOG ( 6320, IUER, 'MALO', 'Failure in '// &
     &              'an attempt to load station file '// &
     &               MALO%CONF%STATION_FINAM )
                CALL EXIT ( 1 )
           END IF 
!
           ALLOCATE ( DSPL_ARR(3,MALO%NSTA,L_FRQ,2), STAT=IUER )
           IF ( IUER .NE. 0 ) THEN
                CALL CLRCH ( STR )
                CALL IINCH ( 8*3*MALO%NSTA*L_FRQ, STR )
                IUER = -1
                CALL ERR_LOG ( 6321, IUER, 'TOC_BIG', 'Failure '// &
     &              'to allocate '//STR(1:I_LEN(STR))//' bytes of dymanic '// &
     &              'memory' )
                CALL EXIT ( 1 ) 
           END IF
      END IF
!
      IND4_SECT(1) = 1
      IND4_SECT(2) = 1
      DO 410 J1=1,L_FRQ
         DO 420 J2=1,NUM_CMP(J1)
            IF ( J2 == 1 .AND. IVRB == 12 ) GOTO 420
            IF ( J2 == 2 .AND. IVRB == 11 ) GOTO 420
            IND_4 = IND_4 + 1
            IF ( IVRB .GE. 2 ) THEN
                 WRITE ( 6, 110 ) J1, L_FRQ, J2, NUM_CMP(J1), IND_HEB(J1)
 110             FORMAT ( 'Wave ', I3, ' ( ', I3, ' )   Component ', I1, ' ( ', I2, ' )  Ind_Heb: ', I2 ) 
                 CALL FLUSH ( 6 )
            END IF
!
            IND3_SECT(1) = J2
            IND3_SECT(2) = J2
            IF ( ASSOCIATED ( HEB_MOD%VAL ) ) DEALLOCATE ( HEB_MOD%VAL )
!
            IUER = -1
            CALL READ_HEB_SECT ( FILIN_HEB, HEB_MOD, IND3_SECT, IND4_SECT, IUER )
            IF ( IUER .NE. 0 ) THEN
                 IUER = -1
                 CALL ERR_LOG ( 6370, IUER, 'TOC_BIG', &
     &               'Failure in an attempt to read and parse a section in the '// &
     &               'input file '//FILIN_HEB(1:I_LEN(FILIN_HEB))//' in heb format' )
                 CALL EXIT ( 1 )
            END IF
!
            DO 430 J3=1,L_LAT
               IND_LAT = J3
               IF ( IND_LAT > HEB_LS%DIMS(2) ) IND_LAT = HEB_LS%DIMS(2) 
               LAT_GDT = -P2I + (J3-1)*PI__NUM/(L_LAT-1)
               GE = MALO__ACC_EQU_WGS84*(1.D0 + MALO__GRV_LAT_WGS84*DSIN(LAT_GDT)**2)/ &
     &              DSQRT(1.D0 - (2.D0*MALO__FLAT_WGS84 - MALO__FLAT_WGS84**2)*DSIN(LAT_GDT)**2 )
               DO 440 J4=1,L_LON
                  IND_LON = J4
                  IF ( IND_LON > HEB_LS%DIMS(1) ) IND_LON = IND_LON - HEB_LS%DIMS(1) 
                  IF ( HEB_MOD%VAL(J4,J3,1,IND_HEB(J1)) > MALO__MAX_VAL ) THEN
                       SPR_R8(J4,J3) = 0.0D0
                       GOTO 440
                  END IF
!
! --------------- Main wave
!
                  MOD_VAL = HEB_MOD%VAL(J4,J3,1,IND_HEB(J1)) ! Because data occupy a part of %VAL 
                  IF ( HEB_MOD%UNITS == "meter" ) THEN
!
! -------------------- Convert sea height (m) to bottom pressure (Pa)
!
                       MOD_VAL = GE*MALO__SW_DENS*MOD_VAL 
                  END IF
                  IF ( MALO%CONF%SURFACE_TYPE == MALO__LAND ) THEN
                       SPR_R8(J4,J3) = MOD_VAL*HEB_LS%VAL(IND_LON,IND_LAT,1,1)
                    ELSE IF ( MALO%CONF%SURFACE_TYPE == MALO__OCEAN ) THEN
                       SPR_R8(J4,J3) = MOD_VAL*(1.D0 - HEB_LS%VAL(IND_LON,IND_LAT,1,1))* &
     &                                 MALO__SW_DENS/1000.0D0
                    ELSE 
                       SPR_R8(J4,J3) = MOD_VAL
                  END IF
 440           CONTINUE 
 430        CONTINUE 
            DEALLOCATE ( HEB_MOD%VAL )
!
            IF ( IVRB .EQ. 2 .OR. IVRB .GE. 8 ) THEN
                 WRITE ( 6, * ) 'MLM '//GET_CDATE()
                 WRITE ( 6, * ) 'MLM Before SPHE-transform J1= ', INT2(J1), ' L_LAT= ', L_LAT
                 CALL FLUSH ( 6 )
                 TIM_R8 = WALL_TIMER ( %VAL(0) )
            END IF
            IUER = -1
            CALL SPHE_DIR_2NN ( %VAL(FSH), L_LAT, SPR_R8, DEG, DEG, NORM, &
     &                          IPHS, MALO%SPH(1,0,0,3,1), IUER )
            IF ( IUER .NE. 0 ) THEN
                 IUER = -1
                 CALL ERR_LOG ( 6322, IUER, 'TOC_BIG', 'Error in '// &
     &               'an attempt to perform spherical transform of the '// &
     &               'pressure field' )
                 CALL EXIT ( 1 )
            END IF
            IF ( IVRB .GE. 8 ) THEN
                 TIM_R8 = WALL_TIMER ( %VAL(2) )
                 WRITE ( 6, * ) 'MLM '//GET_CDATE()
                 WRITE ( 6, 210 ) DEG, TIM_R8
 210             FORMAT ( 'Direct SHT of degree ', I6, ' took ', F12.3, ' sec of wall time' )
                 TIM_R8 = WALL_TIMER ( %VAL(0) )
            END IF
!
! --------- Multiply spherical harmonics by Love numbers
!
            DO 450 J5=0,DEG
               DO 460 J6=J5,DEG
                  DO 470 J7=1,2
!
! ------------------ NB: We transpose the 2nd and 3rd index since 
! ------------------ SPHE_COMP_VEC is expects to find coefficients of the 
! ------------------ spherical harmonic transform in this order:
! ------------------ real/image, order, degree, radial/transverse
!
                     MALO%SPH(J7,J5,J6,1,1) = MALO%SPH(J7,J6,J5,3,1)* &
     &                                      3.0D0*MALO%LOVE(J6,MALO__H)/(2*J6+1)/ &
     &                                      (MALO__DENS*MALO__GRAV)
                     MALO%SPH(J7,J5,J6,2,1) = MALO%SPH(J7,J6,J5,3,1)* &
     &                                      3.0D0*MALO%LOVE(J6,MALO__L)/(2*J6+1)/ &
     &                                      (MALO__DENS*MALO__GRAV)
 470              CONTINUE 
 460           CONTINUE 
 450        CONTINUE 
!
            IF ( LOAD_TYPE == 'load_d1' ) THEN
!
! ------------- In a case when we compute correction for D1-term, we 
! ------------- unscale spherical harmonic of degree one.
!
                MALO%SPH(1:2,0:1,1,MALO__H,1) = 1.0D0/MALO%LOVE(1,MALO__H)* &
     &                                                MALO%SPH(1:2,0:1,1,MALO__H,1)
                MALO%SPH(1:2,0:1,1,MALO__L,1) = 1.0D0/MALO%LOVE(1,MALO__L)* &
     &                                                MALO%SPH(1:2,0:1,1,MALO__L,1)
                MALO%SPH(1:2,0:2,2,MALO__H,1) = 0.0D0
                MALO%SPH(1:2,0:2,2,MALO__L,1) = 0.0D0
!
                MALO%SPH(1:2,1,0,MALO__H,1)  = MALO%SPH(1:2,0,1,MALO__H,1)  
                MALO%SPH(1:2,1,0,MALO__L,1)  = MALO%SPH(1:2,0,1,MALO__L,1)  
            END IF
            IF ( IVRB .EQ. 2 ) THEN
                 WRITE ( 6, * ) 'MLM After  SPHE-transform J1= ', INT2(J1), ' DEG= ', DEG
                 CALL FLUSH ( 6 )
            END IF
!
            IF ( MALO%CONF%OUTPUT_GRID_DEG > 0 ) THEN
!
! -------------- Create the extended copy of scaled spherical harmonics
!
                 ALLOCATE ( SPH_MD(2,0:MD,0:MD,2), STAT=IUER )
                 IF ( IUER .NE. 0 ) THEN
                      CALL CLRCH ( STR )
                      CALL IINCH ( 8*2*2*(MD+1)*(MD+1), STR )
                      IUER = -1
                      CALL ERR_LOG ( 6325, IUER, 'TOC_BIG', 'Failure to '// &
     &                    'allocate '//STR(1:I_LEN(STR))//' bytes of dymanic '// &
     &                    'memory' )
                      CALL EXIT ( 1 ) 
                 END IF
                 SPH_MD = 0.0D0
                 DO 480 J8=0,DEG
                    DO 490 J9=0,J8
                       SPH_MD(1:2,J9,J8,1) = MALO%SPH(1:2,J9,J8,1,1)
                       SPH_MD(1:2,J9,J8,2) = MALO%SPH(1:2,J9,J8,2,1)
                       SPH_MD(1:2,J8,J9,1) = MALO%SPH(1:2,J9,J8,1,1)
                       SPH_MD(1:2,J8,J9,2) = MALO%SPH(1:2,J9,J8,2,1)
 490                CONTINUE 
 480             CONTINUE 
!
                 IF ( IVRB .GE. 8 ) THEN
                      TIM_R8 = WALL_TIMER ( %VAL(2) )
                      WRITE ( 6, * ) 'MLM '//GET_CDATE()
                      WRITE ( 6, 204 ) TIM_R8
 204                  FORMAT ( 'Scaling Love numbers took ', F12.3, ' sec of wall timer' )
                      TIM_R8 = WALL_TIMER ( %VAL(0) )
                 END IF
                 DEALLOCATE ( MALO%SPH )
                 IF ( LOAD_TYPE == 'load' ) THEN
                      IUER = -1
                      CALL SPHE_INV_2NN_VEC ( %VAL(FSH), MD, MD, SPH_MD, &
     &                                        MLAT, DSP_ARR3_R8, IUER )
                      IF ( IUER .NE. 0 ) THEN
                           write ( 6, * ) 'DEG= ', DEG, ' NALO%NLAT = ', MLAT 
                           IUER = -2
                           CALL ERR_LOG ( 6327, IUER, 'TOC_BIG', 'Failure '// &
     &                         'in an attempt to perform inverse vector spherical '// &
     &                         'harmonic transform' )
                           CALL EXIT ( 1 ) 
                      END IF
                      IF ( IVRB .GE. 8 ) THEN
                           TIM_R8 = WALL_TIMER ( %VAL(2) )
                           WRITE ( 6, * ) 'MLM '//GET_CDATE()
                           WRITE ( 6, 220 ) DEG, TIM_R8
 220                       FORMAT ( 'Inverse vector SHT of degree ', I6, ' took ', F12.3, &
     &                               ' sec of wall time' )
                           TIM_R8 = WALL_TIMER ( %VAL(0) )
                      END IF
                    ELSE IF ( LOAD_TYPE == 'load_d1' ) THEN
                      IUER = -1
                      CALL SPHE_INV_2NN_VEC ( %VAL(FSH), MD, 2, DEG, SPH_MD, &
     &                                        MLAT, DSP_ARR3_R8, IUER )
                      IF ( IUER .NE. 0 ) THEN
                           IUER = -2
                           CALL ERR_LOG ( 6328, IUER, 'TOC_BIG', 'Failure '// &
     &                         'in an attempt to perform inverse vector spherical '// &
     &                         'harmonic transform' )
                           CALL EXIT ( 1 ) 
                      END IF
                 END IF
                 DEALLOCATE ( SPH_MD   )
               ELSE 
                 DO 4140 J14=1,MALO%NSTA
                    IUER = -1
                    CALL SPHE_COMP_VEC ( %VAL(FSH), DEG, DEG, MALO%STA(J14)%LAT_GDT, &
     &                                   MALO%STA(J14)%LON, NORM, IPHS, &
     &                                   MALO%SPH, DSPL_ARR(1,J14,J1,J2), &
     &                                   IUER )
                    IF ( IUER .NE. 0 ) THEN
                         IUER = -1
                         CALL ERR_LOG ( 6329, IUER, 'TOC_BIG', &
     &                       'Error in an attempt to compute loading '// &
     &                       'for a given station using spherical harmonics' )
                        CALL EXIT ( 1 )
                    END IF
                    IF ( IVRB .GE. 3 ) THEN
                         WRITE ( 6, 130 ) J1, L_FRQ, J2, NUM_CMP(J2), &
     &                                    J14, MALO%NSTA, CHAR(13)
 130                     FORMAT ( '  MLM Computing for freq ', I3, ' ( ', I3, ' ) ', &
     &                            ' Cmp: ', I1, ' ( ', I1, ' )  ', &
     &                            'Station ', I8, ' ( ', I8, ' ) ', A$ )
                         CALL FLUSH ( 6 ) 
                    END IF
 4140            CONTINUE 
            END IF
 420     CONTINUE 
 410  CONTINUE 
!     
      IF ( IVRB .GE. 8 ) THEN
           WRITE ( 6, * ) 'About to start writing output files ', ext
      END IF
!
      IF ( EXT == '.heb' .OR. EXT == '.nc' .OR. EXT == '.all' ) THEN
           IF ( ALLOCATED  ( SPR_R8   ) ) DEALLOCATE ( SPR_R8   )
           IF ( ASSOCIATED ( MALO%SPH ) ) DEALLOCATE ( MALO%SPH )
!
           FRQ_STR = '|'
           DO 4150 J15=1,L_FRQ
              DO 4160 J16=1,NUM_CMP(J15)
                 FRQ_STR = FRQ_STR(1:I_LEN(FRQ_STR))//OTID_WAV(IND_FRQ(J15))// &
     &                     C_CMP(J16)//'|'
 4160         CONTINUE 
 4150      CONTINUE 
! 
           IF ( MALO%CONF%OUTPUT_GRID_DEG == 0 ) THEN
                IUER = -1
                CALL ERR_LOG ( 6330, IUER, 'TOC_BIG', &
     &              'Output in HEB-format is supported only for a case '// &
     &              'of gridded output. Please use HARPOS-format '// &
     &              'for station output' )
                CALL EXIT ( 1 )
           END IF  
!
           ID = LINDEX ( FILOUT, '/' ) 
!
           HEB_DSPL%MJD     = HEB_MOD%MJD
           HEB_DSPL%UTC     = HEB_MOD%UTC
           HEB_DSPL%TAI     = HEB_MOD%TAI 
           HEB_DSPL%DIMS(1) = MALO%NLON
           HEB_DSPL%DIMS(2) = MALO%NLAT 
           HEB_DSPL%DIMS(3) = 3
           HEB_DSPL%DIMS(4) = L_CMP
           IF ( IVRB == 11 .OR. IVRB == 12 ) HEB_DSPL%DIMS(4) = 1
           HEB_DSPL%DATA_OFFSET = HEB__HDS
           HEB_DSPL%ENDIAN      = HEB__LE
           HEB_DSPL%DATA_TRANSFORM = HEB__NONE
           HEB_DSPL%FILL_VALUE     = 1.0E15
           HEB_DSPL%OFFSET         = 0.0
           HEB_DSPL%SCALE_FACTOR   = 1.0 ! 3.D-6
           HEB_DSPL%DATA_COMPRESSION = HEB__NONE
           HEB_DSPL%SDS_NAME       = 'Site displacement '//FRQ_STR(1:I_LEN(FRQ_STR))
           HEB_DSPL%UNITS          = 'meter'
           HEB_DSPL%DATA_FORMAT    = HEB__R4
           HEB_DSPL%MIN_VALUE      =  -0.096D0
           HEB_DSPL%MAX_VALUE      =   0.096D0
           HEB_DSPL%VALID_RANGE(1) =  -0.096D0
           HEB_DSPL%VALID_RANGE(2) =   0.096D0
           HEB_DSPL%PROD_DATE_TIME = GET_CDATE()
!
           HEB_DSPL%FILE_NAME      = FILHEB(ID+1:)
           HEB_DSPL%HISTORY        = 'Processed using input numerical weather '// &
     &                               'model and input digital elevation map'
           HEB_DSPL%SOURCE         = 'Output of the regression model over surface pressure'
           HEB_DSPL%TITLE          = 'Displacements caused by harmonic variations in loading'
           HEB_DSPL%PROD_NAME      = 'Displacements caused by harmonic variations in loading'
           HEB_DSPL%INSTITUTION    = 'Astrogeo Center'
           HEB_DSPL%REFERENCES     = 'http://astrogeo.org/malo/'
           HEB_DSPL%VERSION_ID     = MALO__LABEL
!
           IF ( IVRB .GE. 8 ) THEN
                WRITE ( 6, * ) 'MLM About to start writing results '//GET_CDATE()
                TIM_R8 = WALL_TIMER ( %VAL(0) )
           END IF
!
           ALLOCATE ( DSP_ARR3_R4(MLON,MLAT,3), STAT=IUER )
           IF ( IUER .NE. 0 ) THEN
                CALL CLRCH ( STR )
                CALL IINCH ( INT8(3*8)*INT8(MLON)*INT8(MLAT-1), STR )
                IUER = -2
                CALL ERR_LOG ( 6332, IUER, 'TOC_BIG', 'Failure to allocate '// &
     &                    STR(1:I_LEN(STR))//' bytes of dymanic memory' )
               CALL EXIT ( 1 ) 
           END IF
           DSP_ARR3_R4 = DSP_ARR3_R8
           DEALLOCATE ( DSP_ARR3_R8 )
!
           IUER = -1 
           CALL WRITE_HEB ( HEB_DSPL, DSP_ARR3_R4, FILHEB, IUER )
           IF ( IUER .NE. 0 ) THEN
                IUER = -1
                CALL ERR_LOG ( 6332, IUER, 'TOC_BIG', 'Failure '// &
     &              'to write loading displacements into the output '// &
     &              'file '//FILHEB )
                CALL EXIT ( 1 )
           END IF
           DEALLOCATE ( DSP_ARR3_R4 )
!
           IF ( IVRB == 11 .OR. IVRB == 12 ) THEN
                CONTINUE 
                IUER = 0
              ELSE 
                IUER = -1
                CALL WRITE_LOADING_NC ( MALO%NLON, MALO%NLAT, L_FRQ, IND_FRQ, &
     &                                  HEB_MOD%MJD, HEB_MOD%TAI, %VAL(0), DSP_ARR_R4, &
     &                                  MALO__LABEL, 'NO', FILDSC, FILCOM, FILNC, &
     &                                  IUER )
           END IF
           IF ( IUER .NE. 0 ) THEN
                IUER = -1
                CALL ERR_LOG ( 6332, IUER, 'TOC_BIG', 'Failure '// &
     &              'to write loading displacements into the output '// &
     &              'file '//FILNC )
                CALL EXIT ( 1 )
           END IF
           IF ( IVRB .GE. 8 ) THEN
                TIM_R8 = WALL_TIMER ( %VAL(2) )
                WRITE ( 6, * ) 'MLM '//GET_CDATE()
                WRITE ( 6, 230 ) TIM_R8
 230            FORMAT ( 'Writing loading displacemetns took ', F12.3, &
     &                    ' sec of wall time' )
                TIM_R8 = WALL_TIMER ( %VAL(0) )
           END IF
        ELSE IF ( EXT == '.hps' ) THEN
           IUER = -1
           CALL MALO_HARPOS_WRITE ( MALO%NSTA, L_FRQ, IND_FRQ, NUM_CMP, &
     &                              DSPL_ARR, MALO, MALO__LABEL, FILHPS, IUER )
           IF ( IUER .NE. 0 ) THEN
                IUER = -1
                CALL ERR_LOG ( 6333, IUER, 'TOC_BIG', 'Failure '// &
     &              'to write loading displacements into the output '// &
     &              'file '//FILHPS )
                CALL EXIT ( 1 )
           END IF
      END IF
      IF ( ILEN(FOLLOWUP_COM) > 0 ) THEN
           WRITE ( 6, '(A)' ) 'TOC_BIG: Running the followup command'
           CALL FLUSH ( 6 )
           IS = SYSTEM ( FOLLOWUP_COM(1:I_LEN(FOLLOWUP_COM))//CHAR(0) )
           IF ( IS .NE. 0 ) THEN
                CALL ERR_LOG ( 6334, IUER, 'TOC_BIG', 'Error in '// &
     &              'running the followup command '//FOLLOWUP_COM )
                CALL EXIT(  1 )
           ENDIF
      END IF
      IF ( IVRB .GE. 8 ) THEN
           TIM_R8 = WALL_TIMER ( %VAL(2) )
           WRITE ( 6, * ) 'Last steps took ', SNGL(TIM_R8), ' sec'
           WRITE ( 6, * ) 'MLM '//GET_CDATE()
      END IF
      WRITE ( 6, '(A)' ) 'TOC_BIG: finished'
      CALL FLUSH ( 6 )
!
      END  PROGRAM  TOC_BIG  !#!  
