      PROGRAM GEN_MASK
! ************************************************************************
! *                                                                      *
! *   Program GEN_MASK
! *                                                                      *
! *  ### 11-JAN-2016    GEN_MASK   v3.3 (c)  L. Petrov  24-FEB-2016 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'heb.i'
      INCLUDE   'malo.i'
      INCLUDE   'malo_local.i'
      TYPE     ( HEB__TYPE  ) :: HEB_IN, HEB_OUT
      LOGICAL*1  LEX
      CHARACTER  MODE_STR*32, STR*128, STR1*128, FILIN*128, FILOUT*128, &
     &           WISDOM_FILE*128
      CHARACTER  GEN_MASK__LABEL*30
      PARAMETER  ( GEN_MASK__LABEL = 'GEN_MASK v  3.3  of 2016.02.24' )
      INTEGER*4  J1, J2, J3, J4, DIM, DIM2, IVRB, NUM_THR, IUER
      INTEGER*8  FSH
      INTEGER*4, EXTERNAL :: ILEN, I_LEN
      INTEGER*8, EXTERNAL :: SPHE_INIT_PLAN
!
!      $MALO_DIR/bin/gen_mask sea-coast 21599 /s0/mod44w/mod44w_ls_sht.heb $MALO_DIR/share/sea_coast_d21599.heb
!      $MALO_DIR/bin/gen_mask bt-mask   21599 /s0/mod44w/mod44w_ls_sht.heb /s0/temp/load/ocean_ls_bt_d21599_d2699.heb 2699
!      $MALO_DIR/bin/gen_mask p-mask 5399 /s0/mod44w/mod44w_ls_sht.heb /s0/mod44w/mod44w_ls_p_d5399.heb
!
      WISDOM_FILE = MALO_SHARE//'/malo_fftw_plan_16thr.wis'
      IVRB = 3
!
      IF ( IARGC() < 4 ) THEN
           WRITE ( 6, * ) 'Usage: gen_mask  mode dimension input_file output_file [dim2]'
           CALL EXIT ( 1 )
         ELSE 
           CALL GETARG ( 1, MODE_STR )
           CALL GETARG ( 2, STR      )
           CALL CHIN   ( STR, DIM    )
           IF ( DIM < 2 .OR. DIM > MALO__MDIM ) THEN
                CALL CLRCH ( STR1 ) 
                CALL INCH  ( MALO__MDIM, STR1 )
                IUER = -1
                CALL ERR_LOG ( 3501, IUER, 'GEN_MASK', 'Wrong dimension: '// &
     &               STR(1:I_LEN(STR))//'. Dimension should be in range '// &
     &              '[2, '//STR1(1:I_LEN(STR1))//']' )
                CALL EXIT ( 1 )
           END IF
           CALL GETARG ( 3, FILIN  )
           CALL GETARG ( 4, FILOUT )
           INQUIRE ( FILE=FILIN, EXIST=LEX )
           IF ( DIM < 2 .OR. DIM > MALO__MDIM ) THEN
                CALL CLRCH ( STR1 ) 
                CALL INCH  ( MALO__MDIM, STR1 )
                IUER = -1
                CALL ERR_LOG ( 3502, IUER, 'GEN_MASK', 'Wrong dimension: '// &
     &               STR(1:I_LEN(STR))//'. Dimension should be in range '// &
     &              '[2, '//STR1 )
                CALL EXIT ( 1 )
           END IF
           IF ( IARGC() .GE. 5 ) THEN
                CALL GETARG ( 5, STR    )
                CALL CHIN   ( STR, DIM2 )
              ELSE
                DIM2 = 0
           END IF
      END IF
      IF ( ( MODE_STR == 'bt-mask' .OR. MODE_STR == 'bu-mask' ) .AND. DIM2 == 0 ) THEN
           CALL ERR_LOG ( 3503, IUER, 'GEN_MASK', 'Secondary dimension is not '// &
     &         'specified. Mode bt-mask or bu-mask reqiuries the secondary dimension' )
           CALL EXIT ( 1 )
      END IF
!
! --- Initialization of the spherical harmonics package
!
      IUER = -1
      NUM_THR = 16
      FSH = SPHE_INIT_PLAN ( WISDOM_FILE, 1, 2.0D0, NUM_THR, IUER )
      IF ( IUER .NE. 0 ) THEN
           CALL ERR_LOG ( 3504, IUER, 'GEN_MASK', 'Error in an attempt to '// &
     &         'initialize FSH object for spherical harmonics transform'  )
           CALL EXIT ( 1 )
      END IF   
!
! --- Read the input file. 
! --- This may be either the land-sea(lake) mask or its spherical harmomics transform
!
      IUER = -1
      CALL READ_HEB ( FILIN, HEB_IN, IUER ) 
      IF ( IUER .NE. 0 ) THEN
           IUER = -2
           CALL ERR_LOG ( 3505, IUER, 'GEN_MASK', 'Error '// &
     &         'in an attempt to read heb-file with land-water '// &
     &         'mask '//FILIN )
           CALL EXIT ( 1 )
      END IF   
      IF ( ( MODE_STR == 'bt-mask' .OR. MODE_STR == 'bu-mask' ) .AND. &
     &     DIM2 > HEB_IN%DIMS(2) ) THEN
           CALL ERR_LOG ( 3506, IUER, 'GEN_MASK', 'Secondary dimension is '// &
     &         'too high: greater than the dimension of the land-sea '// &
     &         'mask spherical harmonics transform' )
           CALL EXIT ( 1 )
      END IF
      IF ( MODE_STR == 'sht_ls' .OR. MODE_STR == 'sht_lw' .OR. MODE_STR == 'sht_ll' ) THEN
!
! -------- Perform direct spherical harmomics transform
!
           IUER = -1
           CALL GEN_DIR_MASK_SHT ( MODE_STR, NUM_THR, %VAL(FSH), HEB_IN, HEB_OUT, IUER )
           IF ( IUER .NE. 0 ) THEN
                IUER = -2
                CALL ERR_LOG ( 3507, IUER, 'GEN_MASK', 'Error '// &
     &              'in computation of the spherical harmnonic transform '// &
     &              'under original mask '//FILIN )
                CALL EXIT ( 1 )
           END IF   
           CALL HEB_MINMAX_R8 ( HEB_OUT, HEB_OUT%VAL8, 1.D8 )
           IF ( MODE_STR == 'sht_ls' ) THEN
                HEB_OUT%SDS_NAME    = 'Spherical harmonics transform of the land-sea mask'
                HEB_OUT%INSTITUTION = 'Astrogeo Center'
                HEB_OUT%COMMENT(1)  = 'Input mask: 1 -- 100% land, 0 -- 100% sea'
                HEB_OUT%FILL_VALUE  = -127.0
                HEB_OUT%UNITS       = 'd/l'
                HEB_OUT%DATA_FORMAT = HEB__R8
           END IF
           HEB_OUT%TITLE = HEB_OUT%SDS_NAME
           HEB_OUT%VALID_RANGE(1) = HEB_OUT%MIN_VALUE        
           HEB_OUT%VALID_RANGE(2) = HEB_OUT%MAX_VALUE        
         ELSE IF ( MODE_STR == 'p-mask'         .OR. &
     &             MODE_STR == 'b-mask'         .OR. &
     &             MODE_STR == 'bt-mask'        .OR. &
     &             MODE_STR == 'bu-mask'        .OR. &
     &             MODE_STR == 'hn-mask'        .OR. &
     &             MODE_STR == 'hm-mask'        .OR. &
     &             MODE_STR == 'sea-coast'      .OR. &
     &             MODE_STR == 'sea-lake-coast'      ) THEN
!
! -------- Perform inverse spherical harmonics transform
!
           IUER = -1
           CALL GEN_INV_MASK_SHT ( MODE_STR, NUM_THR, %VAL(FSH), DIM, HEB_IN, HEB_OUT, &
     &                             DIM2, IUER )
           IF ( IUER .NE. 0 ) THEN
                IUER = -2
                CALL ERR_LOG ( 3508, IUER, 'GEN_MASK', 'Error '// &
     &              'in computation of the land-sea mask from its spherical harmonics '// &
     &              'transform' )
                CALL EXIT ( 1 )
           END IF   
           IF ( MODE_STR == 'sea-coast' .OR. MODE_STR == 'sea-lake-coast' ) THEN
!
! ------------- Generate the coast line
!
                IUER = -1
                CALL GEN_COAST_MASK ( HEB_OUT, IVRB, IUER )
                IF ( IUER .NE. 0 ) THEN
                     IUER = -2
                     CALL ERR_LOG ( 3509, IUER, 'GEN_MASK', 'Error in computation '// &
     &                   'of the land-sea mask from its spherical harmonics '// &
     &                   'transform' )
                     CALL EXIT ( 1 )
                 END IF
           END IF
!
! -------- Write the scientific data set name
!
           IF ( MODE_STR == 'p-mask' ) THEN
                HEB_OUT%SDS_NAME = 'Band-limited land-sea mask from MOD44W without windowing'
                HEB_OUT%COMMENT(1)  = ' '
              ELSE IF ( MODE_STR == 'b-mask' ) THEN
                HEB_OUT%SDS_NAME = 'Band-limited land-sea mask from MOD44W with Blackman window'
                HEB_OUT%COMMENT(1)  = ' '
              ELSE IF ( MODE_STR == 'bt-mask' ) THEN
                HEB_OUT%SDS_NAME = 'Band-limited low truncated land-sea mask from MOD44W with Blackman window'
                CALL CLRCH ( STR )
                CALL INCH  ( DIM2, STR ) 
                HEB_OUT%COMMENT(1)  = 'Low limit of truncation: '//STR(1:I_LEN(STR))
              ELSE IF ( MODE_STR == 'bu-mask' ) THEN
                HEB_OUT%SDS_NAME = 'Band-limited low truncated land-sea mask from MOD44W with Blackman window at low limit'
                CALL CLRCH ( STR )
                CALL INCH  ( DIM2, STR ) 
                HEB_OUT%COMMENT(1)  = 'Low limit of truncation: '//STR(1:I_LEN(STR))
              ELSE IF ( MODE_STR == 'hn-mask' ) THEN
                HEB_OUT%SDS_NAME = 'Band-limited land-sea mask from MOD44W with Hann window'
              ELSE IF ( MODE_STR == 'hm-mask' ) THEN
                HEB_OUT%SDS_NAME = 'Band-limited land-sea mask from MOD44W with Hamming window'
                HEB_OUT%COMMENT(1)  = ' '
              ELSE IF ( MODE_STR == 'sea-coast' ) THEN
                HEB_OUT%SDS_NAME = 'Sea-coast mask from MOD44W with Hamming window'
                HEB_OUT%COMMENT(1)  = '1 -- coast, 0 -- otherwise'
              ELSE IF ( MODE_STR == 'sea-lake-coast' ) THEN
                HEB_OUT%SDS_NAME = 'Sea-lake-coast mask from MOD44W with Hamming window'
                HEB_OUT%COMMENT(1)  = '1 -- coast, 0 -- otherwise'
           END IF
           HEB_OUT%TITLE = HEB_OUT%SDS_NAME
           HEB_OUT%INSTITUTION = 'Astrogeo Center'
           HEB_OUT%UNITS       = 'd/l'
           HEB_OUT%FILL_VALUE  = -127.0
           HEB_OUT%VERSION_ID  = GEN_MASK__LABEL
!
           IF ( MODE_STR == 'p-mask'   .OR. &
     &          MODE_STR == 'b-mask'   .OR. &
     &          MODE_STR == 'bt-mask'  .OR. &
     &          MODE_STR == 'bu-mask'  .OR. &
     &          MODE_STR == 'hn-mask'  .OR. &
     &          MODE_STR == 'hm-mask'       ) THEN
!
                HEB_OUT%COMMENT(2)  = '1.0 -- 100% land, 0.0 -- 100% sea'
                HEB_OUT%COMMENT(3)  = 'Values above 1.0 and below 0.0 are allowed'
                HEB_OUT%DATA_FORMAT = HEB__R4
                CALL HEB_MINMAX ( HEB_OUT, HEB_OUT%VAL, 1.0E8 )
              ELSE
                HEB_OUT%DATA_TRANSFORM = HEB__NONE
                HEB_OUT%DATA_FORMAT    = HEB__I1
                HEB_OUT%MIN_VALUE      = 0
                HEB_OUT%MAX_VALUE      = 1
           END IF
!
           HEB_OUT%VALID_RANGE(1) = HEB_OUT%MIN_VALUE        
           HEB_OUT%VALID_RANGE(2) = HEB_OUT%MAX_VALUE        
         ELSE IF ( MODE_STR == 'ls_avr' ) THEN
           IUER = -1
           CALL MASK_AVR ( MODE_STR, HEB_IN, DIM, HEB_OUT, IUER )
           IF ( IUER .NE. 0 ) THEN
                IUER = -2
                CALL ERR_LOG ( 3510, IUER, 'GEN_MASK', 'Error '// &
     &              'in computation of the averaged land/sea mask' )
                CALL EXIT ( 1 )
           END IF   
           HEB_OUT%DATA_TRANSFORM = HEB__NONE
           HEB_OUT%DATA_FORMAT    = HEB__R4
           HEB_OUT%SDS_NAME    = 'land-sea mask from MOD44W produced by arithmetic averaging'
           HEB_OUT%TITLE       = HEB_OUT%SDS_NAME
           HEB_OUT%INSTITUTION = 'Astrogeo Center'
           HEB_OUT%UNITS       = 'd/l'
           HEB_OUT%FILL_VALUE  = -127.0
           HEB_OUT%VERSION_ID  = GEN_MASK__LABEL
           HEB_OUT%MIN_VALUE   = 0.0
           HEB_OUT%MAX_VALUE   = 1.0
           HEB_OUT%VALID_RANGE(1) = HEB_OUT%MIN_VALUE
           HEB_OUT%VALID_RANGE(2) = HEB_OUT%MAX_VALUE
           HEB_OUT%COMMENT(1)  = '1.0 -- 100% land, 0.0 -- 100% sea'
           CALL CLRCH ( HEB_OUT%COMMENT(2) )
           CALL CLRCH ( HEB_OUT%COMMENT(3) )
         ELSE 
           IUER = -2
           CALL ERR_LOG ( 3511, IUER, 'GEN_MASK', 'Unknown mode '//MODE_STR )
           CALL EXIT ( 1 )
      END IF
!
      IUER = -1
      IF ( HEB_OUT%DATA_FORMAT == HEB__R4 ) THEN
           CALL WRITE_HEB ( HEB_OUT, HEB_OUT%VAL, FILOUT, IUER )
        ELSE IF ( HEB_OUT%DATA_FORMAT == HEB__R8 ) THEN
           CALL WRITE_HEB ( HEB_OUT, HEB_OUT%VAL8, FILOUT, IUER )
        ELSE IF ( HEB_OUT%DATA_FORMAT == HEB__I1 ) THEN
           CALL WRITE_HEB ( HEB_OUT, HEB_OUT%VAL1, FILOUT, IUER )
      END IF
      IF ( IUER .NE. 0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 3512, IUER, 'GEN_MASK', 'Failure in writing '// &
     &         'into the output file '//FILOUT )
           CALL EXIT ( 1 )
      END IF
      WRITE ( 6, '(A)' ) 'Output file: '//FILOUT(1:I_LEN(FILOUT))
      CALL FLUSH ( 6 ) 
!
      END  PROGRAM  GEN_MASK  !#!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE GEN_DIR_MASK_SHT ( MODE_STR, NUM_THR, FSH, HEB_IN, HEB_OUT, &
     &                              IUER )
! ************************************************************************
! *                                                                      *
! *   Routine GEN_DIR_MASK_SHT 
! *                                                                      *
! * ### 11-JAN-2016 GEN_DIR_MASK_SHT  v1.0 (c) L. Petrov 11-JAN-2016 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'heb.i'
      INCLUDE   'malo.i'
      INCLUDE   'fourpack.i'
      INCLUDE   'fourpack_constants.i'
      INCLUDE   'fftw3.f'
      TYPE     ( SPHE_TYPE  ) :: FSH
      CHARACTER  MODE_STR*(*)
      TYPE     ( HEB__TYPE  ) :: HEB_IN, HEB_OUT
      INTEGER*4  NUM_THR, IUER
      CHARACTER  STR*128
      REAL*8,    ALLOCATABLE :: MASK_R8(:,:)
      INTEGER*4  SHA(8), NLON, NLAT, J1, J2, ILAT, ILON, DIM, TEST_MASK, IER
      REAL*8     TIM_WALL_R8, TIM_CPU_R8
      REAL*8,    EXTERNAL :: WALL_TIMER, CPU_TIMER
      INTEGER*4, EXTERNAL :: ILEN, I_LEN
!
      TEST_MASK = 0 !  0 -- normal, 1 -- test mode when the land-sea mask is written
!
      TIM_WALL_R8 = WALL_TIMER ( %VAL(0) )
      SHA(1:4) = SHAPE(HEB_IN%VAL1)
      IF ( SHA(1) == 172800 .AND. SHA(2) == 86401 ) THEN
           CONTINUE 
         ELSE 
           WRITE ( 6, * ) 'SHAPE(HEB_IN%VAL1) = ', SHA(1:4)
           CALL ERR_LOG ( 3511, IUER, 'GEN_DIR_MASK_SHT', 'Trap of internal '// &
     &         'control: expected shape of the input HEB file is '// &
     &         '17280, 86401, 1, 1, but we got another values' )
           RETURN 
      END IF
!
      NLON = HEB_IN%DIMS(1)/2
      NLAT = (HEB_IN%DIMS(2)-1)/2 + 1
      ALLOCATE ( MASK_R8(NLON,NLAT), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH8 ( INT8(8)*INT8(NLON)*INT8(NLAT), STR )
           CALL ERR_LOG ( 3512, IUER, 'GEN_DIR_MASK_SHT', 'Failure to allocate '// &
     &          STR(1:I_LEN(STR))//' bytes of dynamic memory for array '// &
     &         'MASK_R8' )
           RETURN 
      END IF
      WRITE ( 6, * ) 'Started regridding'
      MASK_R8 = 0.0D0
      MASK_R8(1:NLON,1)    = 1.0D0
      MASK_R8(1:NLON,NLAT) = 0.0D0
!
      ILAT = 0
      DO 410 J1=2,NLAT-2
         ILAT = ILAT + 2
         IF ( MODE_STR == 'sht_ls' ) THEN
!
! ----------- The eastmost cell for at a given latitude
!
              IF ( HEB_IN%VAL1(2*NLON,ILAT-1,1,1) .NE. MALO__SEA_VAL ) MASK_R8(1,J1) = MASK_R8(1,J1) + 1.0D0/16.0D0
              IF ( HEB_IN%VAL1(2*NLON,ILAT-0,1,1) .NE. MALO__SEA_VAL ) MASK_R8(1,J1) = MASK_R8(1,J1) + 2.0D0/16.0D0
              IF ( HEB_IN%VAL1(2*NLON,ILAT+1,1,1) .NE. MALO__SEA_VAL ) MASK_R8(1,J1) = MASK_R8(1,J1) + 1.0D0/16.0D0
              IF ( HEB_IN%VAL1(1,ILAT-1,1,1)      .NE. MALO__SEA_VAL ) MASK_R8(1,J1) = MASK_R8(1,J1) + 2.0D0/16.0D0
              IF ( HEB_IN%VAL1(1,ILAT-0,1,1)      .NE. MALO__SEA_VAL ) MASK_R8(1,J1) = MASK_R8(1,J1) + 4.0D0/16.0D0
              IF ( HEB_IN%VAL1(1,ILAT+1,1,1)      .NE. MALO__SEA_VAL ) MASK_R8(1,J1) = MASK_R8(1,J1) + 2.0D0/16.0D0
              IF ( HEB_IN%VAL1(2,ILAT-1,1,1)      .NE. MALO__SEA_VAL ) MASK_R8(1,J1) = MASK_R8(1,J1) + 1.0D0/16.0D0
              IF ( HEB_IN%VAL1(2,ILAT-0,1,1)      .NE. MALO__SEA_VAL ) MASK_R8(1,J1) = MASK_R8(1,J1) + 2.0D0/16.0D0
              IF ( HEB_IN%VAL1(2,ILAT+1,1,1)      .NE. MALO__SEA_VAL ) MASK_R8(1,J1) = MASK_R8(1,J1) + 1.0D0/16.0D0
!
! ----------- The westmost cell for at a given latitude
!
              IF ( HEB_IN%VAL1(2*NLON-1,ILAT-1,1,1) .NE. MALO__SEA_VAL ) MASK_R8(NLON,J1) = MASK_R8(NLON,J1) + 1.0D0/16.0D0
              IF ( HEB_IN%VAL1(2*NLON-1,ILAT-0,1,1) .NE. MALO__SEA_VAL ) MASK_R8(NLON,J1) = MASK_R8(NLON,J1) + 2.0D0/16.0D0
              IF ( HEB_IN%VAL1(2*NLON-1,ILAT+1,1,1) .NE. MALO__SEA_VAL ) MASK_R8(NLON,J1) = MASK_R8(NLON,J1) + 1.0D0/16.0D0
              IF ( HEB_IN%VAL1(2*NLON-0,ILAT-1,1,1) .NE. MALO__SEA_VAL ) MASK_R8(NLON,J1) = MASK_R8(NLON,J1) + 2.0D0/16.0D0
              IF ( HEB_IN%VAL1(2*NLON-0,ILAT-0,1,1) .NE. MALO__SEA_VAL ) MASK_R8(NLON,J1) = MASK_R8(NLON,J1) + 4.0D0/16.0D0
              IF ( HEB_IN%VAL1(2*NLON-0,ILAT+1,1,1) .NE. MALO__SEA_VAL ) MASK_R8(NLON,J1) = MASK_R8(NLON,J1) + 2.0D0/16.0D0
              IF ( HEB_IN%VAL1(1,ILAT-1,1,1)        .NE. MALO__SEA_VAL ) MASK_R8(NLON,J1) = MASK_R8(NLON,J1) + 1.0D0/16.0D0
              IF ( HEB_IN%VAL1(1,ILAT-0,1,1)        .NE. MALO__SEA_VAL ) MASK_R8(NLON,J1) = MASK_R8(NLON,J1) + 2.0D0/16.0D0
              IF ( HEB_IN%VAL1(1,ILAT+1,1,1)        .NE. MALO__SEA_VAL ) MASK_R8(NLON,J1) = MASK_R8(NLON,J1) + 1.0D0/16.0D0
            ELSE IF ( MODE_STR == 'sht_lw' ) THEN
!
! ----------- The eastmost cell for at a given latitude
!
              IF ( HEB_IN%VAL1(2*NLON,ILAT-1,1,1) .EQ. MALO__WATER_VAL  .OR.  HEB_IN%VAL1(2*NLON,ILAT-1,1,1) .EQ. MALO__LAND_VAL ) MASK_R8(1,J1) = MASK_R8(1,J1) + 1.0D0/16.0D0
              IF ( HEB_IN%VAL1(2*NLON,ILAT-0,1,1) .EQ. MALO__WATER_VAL  .OR.  HEB_IN%VAL1(2*NLON,ILAT-0,1,1) .EQ. MALO__LAND_VAL ) MASK_R8(1,J1) = MASK_R8(1,J1) + 2.0D0/16.0D0
              IF ( HEB_IN%VAL1(2*NLON,ILAT+1,1,1) .EQ. MALO__WATER_VAL  .OR.  HEB_IN%VAL1(2*NLON,ILAT+1,1,1) .EQ. MALO__LAND_VAL ) MASK_R8(1,J1) = MASK_R8(1,J1) + 1.0D0/16.0D0
              IF ( HEB_IN%VAL1(1,ILAT-1,1,1)      .EQ. MALO__WATER_VAL  .OR.  HEB_IN%VAL1(1,ILAT-1,1,1)      .EQ. MALO__LAND_VAL ) MASK_R8(1,J1) = MASK_R8(1,J1) + 2.0D0/16.0D0
              IF ( HEB_IN%VAL1(1,ILAT-0,1,1)      .EQ. MALO__WATER_VAL  .OR.  HEB_IN%VAL1(1,ILAT-0,1,1)      .EQ. MALO__LAND_VAL ) MASK_R8(1,J1) = MASK_R8(1,J1) + 4.0D0/16.0D0
              IF ( HEB_IN%VAL1(1,ILAT+1,1,1)      .EQ. MALO__WATER_VAL  .OR.  HEB_IN%VAL1(1,ILAT+1,1,1)      .EQ. MALO__LAND_VAL ) MASK_R8(1,J1) = MASK_R8(1,J1) + 2.0D0/16.0D0
              IF ( HEB_IN%VAL1(2,ILAT-1,1,1)      .EQ. MALO__WATER_VAL  .OR.  HEB_IN%VAL1(2,ILAT-1,1,1)      .EQ. MALO__LAND_VAL ) MASK_R8(1,J1) = MASK_R8(1,J1) + 1.0D0/16.0D0
              IF ( HEB_IN%VAL1(2,ILAT-0,1,1)      .EQ. MALO__WATER_VAL  .OR.  HEB_IN%VAL1(2,ILAT-0,1,1)      .EQ. MALO__LAND_VAL ) MASK_R8(1,J1) = MASK_R8(1,J1) + 2.0D0/16.0D0
              IF ( HEB_IN%VAL1(2,ILAT+1,1,1)      .EQ. MALO__WATER_VAL  .OR.  HEB_IN%VAL1(2,ILAT+1,1,1)      .EQ. MALO__LAND_VAL ) MASK_R8(1,J1) = MASK_R8(1,J1) + 1.0D0/16.0D0
!
! ----------- The westmost cell for at a given latitude
!
              IF ( HEB_IN%VAL1(2*NLON-1,ILAT-1,1,1) .EQ. MALO__WATER_VAL  .OR.  HEB_IN%VAL1(2*NLON-1,ILAT-1,1,1) .NE. MALO__LAND_VAL ) MASK_R8(NLON,J1) = MASK_R8(NLON,J1) + 1.0D0/16.0D0
              IF ( HEB_IN%VAL1(2*NLON-1,ILAT-0,1,1) .EQ. MALO__WATER_VAL  .OR.  HEB_IN%VAL1(2*NLON-1,ILAT-0,1,1) .NE. MALO__LAND_VAL ) MASK_R8(NLON,J1) = MASK_R8(NLON,J1) + 2.0D0/16.0D0
              IF ( HEB_IN%VAL1(2*NLON-1,ILAT+1,1,1) .EQ. MALO__WATER_VAL  .OR.  HEB_IN%VAL1(2*NLON-1,ILAT+1,1,1) .NE. MALO__LAND_VAL ) MASK_R8(NLON,J1) = MASK_R8(NLON,J1) + 1.0D0/16.0D0
              IF ( HEB_IN%VAL1(2*NLON-0,ILAT-1,1,1) .EQ. MALO__WATER_VAL  .OR.  HEB_IN%VAL1(2*NLON-0,ILAT-1,1,1) .NE. MALO__LAND_VAL ) MASK_R8(NLON,J1) = MASK_R8(NLON,J1) + 2.0D0/16.0D0
              IF ( HEB_IN%VAL1(2*NLON-0,ILAT-0,1,1) .EQ. MALO__WATER_VAL  .OR.  HEB_IN%VAL1(2*NLON-0,ILAT-0,1,1) .NE. MALO__LAND_VAL ) MASK_R8(NLON,J1) = MASK_R8(NLON,J1) + 4.0D0/16.0D0
              IF ( HEB_IN%VAL1(2*NLON-0,ILAT+1,1,1) .EQ. MALO__WATER_VAL  .OR.  HEB_IN%VAL1(2*NLON-0,ILAT+1,1,1) .NE. MALO__LAND_VAL ) MASK_R8(NLON,J1) = MASK_R8(NLON,J1) + 2.0D0/16.0D0
              IF ( HEB_IN%VAL1(1,ILAT-1,1,1)        .EQ. MALO__WATER_VAL  .OR.  HEB_IN%VAL1(1,ILAT-1,1,1)        .NE. MALO__LAND_VAL ) MASK_R8(NLON,J1) = MASK_R8(NLON,J1) + 1.0D0/16.0D0
              IF ( HEB_IN%VAL1(1,ILAT-0,1,1)        .EQ. MALO__WATER_VAL  .OR.  HEB_IN%VAL1(1,ILAT-0,1,1)        .NE. MALO__LAND_VAL ) MASK_R8(NLON,J1) = MASK_R8(NLON,J1) + 2.0D0/16.0D0
              IF ( HEB_IN%VAL1(1,ILAT+1,1,1)        .EQ. MALO__WATER_VAL  .OR.  HEB_IN%VAL1(1,ILAT+1,1,1)        .NE. MALO__LAND_VAL ) MASK_R8(NLON,J1) = MASK_R8(NLON,J1) + 1.0D0/16.0D0
         END IF
         ILON = 2
         DO 420 J2=2,NLON-1
            ILON = ILON + 2
            IF ( MODE_STR == 'sht_ls' ) THEN
                 IF ( HEB_IN%VAL1(ILON-1,ILAT-1,1,1) .NE. MALO__SEA_VAL ) MASK_R8(J2,J1) = MASK_R8(J2,J1) + 1.0D0/16.0D0
                 IF ( HEB_IN%VAL1(ILON-0,ILAT-1,1,1) .NE. MALO__SEA_VAL ) MASK_R8(J2,J1) = MASK_R8(J2,J1) + 2.0D0/16.0D0
                 IF ( HEB_IN%VAL1(ILON+1,ILAT-1,1,1) .NE. MALO__SEA_VAL ) MASK_R8(J2,J1) = MASK_R8(J2,J1) + 1.0D0/16.0D0
                 IF ( HEB_IN%VAL1(ILON-1,ILAT-0,1,1) .NE. MALO__SEA_VAL ) MASK_R8(J2,J1) = MASK_R8(J2,J1) + 2.0D0/16.0D0
                 IF ( HEB_IN%VAL1(ILON-0,ILAT-0,1,1) .NE. MALO__SEA_VAL ) MASK_R8(J2,J1) = MASK_R8(J2,J1) + 4.0D0/16.0D0
                 IF ( HEB_IN%VAL1(ILON+1,ILAT-0,1,1) .NE. MALO__SEA_VAL ) MASK_R8(J2,J1) = MASK_R8(J2,J1) + 2.0D0/16.0D0
                 IF ( HEB_IN%VAL1(ILON-1,ILAT+1,1,1) .NE. MALO__SEA_VAL ) MASK_R8(J2,J1) = MASK_R8(J2,J1) + 1.0D0/16.0D0
                 IF ( HEB_IN%VAL1(ILON-0,ILAT+1,1,1) .NE. MALO__SEA_VAL ) MASK_R8(J2,J1) = MASK_R8(J2,J1) + 2.0D0/16.0D0
                 IF ( HEB_IN%VAL1(ILON+1,ILAT+1,1,1) .NE. MALO__SEA_VAL ) MASK_R8(J2,J1) = MASK_R8(J2,J1) + 1.0D0/16.0D0
               ELSE IF ( MODE_STR == 'sht_lw' ) THEN
                 IF ( HEB_IN%VAL1(ILON-1,ILAT-1,1,1) .EQ. MALO__WATER_VAL  .OR.  HEB_IN%VAL1(ILON-1,ILAT-1,1,1) .EQ. MALO__LAND_VAL ) MASK_R8(J2,J1) = MASK_R8(J2,J1) + 1.0D0/16.0D0
                 IF ( HEB_IN%VAL1(ILON-0,ILAT-1,1,1) .EQ. MALO__WATER_VAL  .OR.  HEB_IN%VAL1(ILON-0,ILAT-1,1,1) .EQ. MALO__LAND_VAL ) MASK_R8(J2,J1) = MASK_R8(J2,J1) + 2.0D0/16.0D0
                 IF ( HEB_IN%VAL1(ILON+1,ILAT-1,1,1) .EQ. MALO__WATER_VAL  .OR.  HEB_IN%VAL1(ILON+1,ILAT-1,1,1) .EQ. MALO__LAND_VAL ) MASK_R8(J2,J1) = MASK_R8(J2,J1) + 1.0D0/16.0D0
                 IF ( HEB_IN%VAL1(ILON-1,ILAT-0,1,1) .EQ. MALO__WATER_VAL  .OR.  HEB_IN%VAL1(ILON-1,ILAT-0,1,1) .EQ. MALO__LAND_VAL ) MASK_R8(J2,J1) = MASK_R8(J2,J1) + 2.0D0/16.0D0
                 IF ( HEB_IN%VAL1(ILON-0,ILAT-0,1,1) .EQ. MALO__WATER_VAL  .OR.  HEB_IN%VAL1(ILON-0,ILAT-0,1,1) .EQ. MALO__LAND_VAL ) MASK_R8(J2,J1) = MASK_R8(J2,J1) + 4.0D0/16.0D0
                 IF ( HEB_IN%VAL1(ILON+1,ILAT-0,1,1) .EQ. MALO__WATER_VAL  .OR.  HEB_IN%VAL1(ILON+1,ILAT-0,1,1) .EQ. MALO__LAND_VAL ) MASK_R8(J2,J1) = MASK_R8(J2,J1) + 2.0D0/16.0D0
                 IF ( HEB_IN%VAL1(ILON-1,ILAT+1,1,1) .EQ. MALO__WATER_VAL  .OR.  HEB_IN%VAL1(ILON-1,ILAT+1,1,1) .EQ. MALO__LAND_VAL ) MASK_R8(J2,J1) = MASK_R8(J2,J1) + 1.0D0/16.0D0
                 IF ( HEB_IN%VAL1(ILON-0,ILAT+1,1,1) .EQ. MALO__WATER_VAL  .OR.  HEB_IN%VAL1(ILON-0,ILAT+1,1,1) .EQ. MALO__LAND_VAL ) MASK_R8(J2,J1) = MASK_R8(J2,J1) + 2.0D0/16.0D0
                 IF ( HEB_IN%VAL1(ILON+1,ILAT+1,1,1) .EQ. MALO__WATER_VAL  .OR.  HEB_IN%VAL1(ILON+1,ILAT+1,1,1) .EQ. MALO__LAND_VAL ) MASK_R8(J2,J1) = MASK_R8(J2,J1) + 1.0D0/16.0D0
               ELSE IF ( MODE_STR == 'sht_ll' ) THEN
                 IF ( HEB_IN%VAL1(ILON-1,ILAT,1,1) .EQ. MALO__LAND_VAL ) THEN
                      MASK_R8(J2,J1) = MASK_R8(J2,J1) + 0.25D0
                 END IF
                 IF ( HEB_IN%VAL1(ILON,ILAT,1,1) .EQ. MALO__LAND_VAL ) THEN
                      MASK_R8(J2,J1) = MASK_R8(J2,J1) + 0.25D0
                 END IF
                 IF ( HEB_IN%VAL1(ILON,ILAT+1,1,1) .EQ. MALO__LAND_VAL ) THEN
                      MASK_R8(J2,J1) = MASK_R8(J2,J1) + 0.25D0
                 END IF
                 IF ( HEB_IN%VAL1(ILON-1,ILAT+1,1,1) .EQ. MALO__LAND_VAL ) THEN
                      MASK_R8(J2,J1) = MASK_R8(J2,J1) + 0.25D0
                 END IF
            END IF
 420     CONTINUE 
 410  CONTINUE 
      DEALLOCATE ( HEB_IN%VAL1 )
      TIM_WALL_R8 = WALL_TIMER ( %VAL(2) )
      WRITE ( 6, 110 ) TIM_WALL_R8
 110  FORMAT ( ' Finished re-gridding. Wall time: ', F10.3, ' sec' )
      HEB_OUT = HEB_IN
      HEB_OUT%DIMS(1) = 2
      HEB_OUT%DIMS(2) = NLON/4
      HEB_OUT%DIMS(3) = NLON/4
      HEB_OUT%DIMS(4) = 1
      IF ( TEST_MASK == 1 ) THEN
           HEB_OUT%DIMS(1) = NLON
           HEB_OUT%DIMS(2) = NLAT
           HEB_OUT%DIMS(3) = 1
           HEB_OUT%DIMS(4) = 1
      END IF
!
      ALLOCATE ( HEB_OUT%VAL8(HEB_OUT%DIMS(1),HEB_OUT%DIMS(2),HEB_OUT%DIMS(3),HEB_OUT%DIMS(4)), &
     &           STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH8 ( INT8(8)*HEB_OUT%DIMS(1)*HEB_OUT%DIMS(2)*HEB_OUT%DIMS(3), STR )
           CALL ERR_LOG ( 3513, IUER, 'GEN_DIR_MASK_SHT', 'Failure to allocate '// &
     &          STR(1:I_LEN(STR))//' bytes of dynamic memory for array '// &
     &         'HEB_OUT%VAL8' )
           RETURN 
      END IF
      IF ( TEST_MASK == 1 ) THEN
           HEB_OUT%VAL8(1:NLON,1:NLAT,1,1) = MASK_R8(1:NLON,1:NLAT)
           WRITE ( 6, * ) 'Completed generation of the mask in the test mode' 
           DEALLOCATE ( MASK_R8 )
           CALL ERR_LOG ( 0, IUER )
           RETURN 
      END IF
      DIM = HEB_OUT%DIMS(2) - 1
      WRITE ( 6, 120 ) DIM, NUM_THR
 120  FORMAT ( 'Started direct SHT of dimension ', I6, ' using ', I2, ' threads' )
      TIM_WALL_R8 = WALL_TIMER ( %VAL(0) )
      TIM_CPU_R8  = CPU_TIMER  ( %VAL(0) )
      CALL ERR_PASS ( IUER, IER )
      CALL SPHE_DIR_2NN ( FSH, NLAT-1, MASK_R8, DIM, DIM, 1, 1, HEB_OUT%VAL8, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 3514, IUER, 'GEN_DIR_MASK_SHT', 'Failure in computation'// &
     &         'of spherical harmonics transform' )
           RETURN 
      END IF
      TIM_CPU_R8  = CPU_TIMER  ( %VAL(2) )
      TIM_WALL_R8 = WALL_TIMER ( %VAL(2) )
      WRITE ( 6, 130 ) DIM, TIM_WALL_R8, TIM_CPU_R8
 130  FORMAT ( 'Ended direct SHT of dimension ', I6, ' Wall time: ', F10.3, ' sec ', &
     &         'CPU time: ', F10.3 )
      DEALLOCATE ( MASK_R8 )
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  GEN_DIR_MASK_SHT  !#!  
!
! ------------------------------------------------------------------------
!
      SUBROUTINE GEN_INV_MASK_SHT ( MODE_STR, NUM_THR, FSH, DIM, HEB_IN, &
     &                              HEB_OUT, DIM2, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine GEN_INV_MASK_SHT 
! *                                                                      *
! * ## 13-JAN-2016  GEN_INV_MASK_SHT  v2.0 (c)  L. Petrov 02-FEB-2016 ## *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'heb.i'
      INCLUDE   'malo.i'
      INCLUDE   'fourpack.i'
      INCLUDE   'fourpack_constants.i'
      INCLUDE   'fftw3.f'
      TYPE     ( SPHE_TYPE  ) :: FSH
      CHARACTER  MODE_STR*(*)
      CHARACTER  STR*128
      TYPE     ( HEB__TYPE  ) :: HEB_IN, HEB_OUT
      INTEGER*4  NUM_THR, DIM, DIM2, IUER
      REAL*8     WIN, WIN_B, WIN_S, TIM_WALL_R8, TIM_CPU_R8 
      INTEGER*4  J1, J2, IER
      INTEGER*4, EXTERNAL :: ILEN, I_LEN
      REAL*8,    EXTERNAL :: WALL_TIMER, CPU_TIMER
!
      HEB_OUT = HEB_IN
      HEB_OUT%DIMS(1) = 4*(DIM+1)
      HEB_OUT%DIMS(2) = 2*(DIM+1)+1
      HEB_OUT%DIMS(3) = 1
      HEB_OUT%DIMS(4) = 1
!
      ALLOCATE ( HEB_OUT%VAL8(HEB_OUT%DIMS(1),HEB_OUT%DIMS(2),HEB_OUT%DIMS(3),HEB_OUT%DIMS(4)), &
     &           STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH8 ( INT8(8)*HEB_OUT%DIMS(1)*HEB_OUT%DIMS(2)*HEB_OUT%DIMS(3), STR )
           CALL ERR_LOG ( 3611, IUER, 'GEN_INV_MASK_SHT', 'Failure to allocate '// &
     &          STR(1:I_LEN(STR))//' bytes of dynamic memory for array '// &
     &         'HEB_OUT%VAL8' )
           RETURN 
      END IF
      HEB_OUT%VAL8 = 0.0D0
      DO 410 J1=1,DIM+1
         IF ( MODE_STR == 'hn-mask' ) THEN
!
! ----------- Set Hann window
!
              WIN = 0.50D0    + 0.50D0 * DCOS ( (PI__NUM*(J1-1))/DIM )
            ELSE IF ( MODE_STR == 'hm-mask' ) THEN
!
! ----------- Set Hamming window
!
              WIN = 0.53836D0 + 0.46164* DCOS ( (PI__NUM*(J1-1))/DIM )
            ELSE IF ( MODE_STR == 'b-mask'         .OR. &
     &                MODE_STR == 'sea-coast'      .OR. &
     &                MODE_STR == 'sea-lake-coast'      ) THEN
!
! ----------- Set exact Blackman window
!
              WIN =   7938.D0/18608.D0 &
     &              + 9240.D0/18608.D0* DCOS ( (PI__NUM*(J1-1))/DIM ) &
     &              + 1430.D0/18608.D0* DCOS ( (PI2*(J1-1))/DIM ) 
            ELSE IF ( MODE_STR == 'bt-mask' ) THEN
!
! ----------- Blackman window truncated from the lower dimension DIM2.
! ----------- This is differences of two Blackman windows: 
! ----------- at dimension DIM minus at dimension DIM2
!
              WIN_B =   7938.D0/18608.D0 &
     &                + 9240.D0/18608.D0* DCOS ( (PI__NUM*(J1-1))/DIM ) &
     &                + 1430.D0/18608.D0* DCOS ( (PI2*(J1-1))/DIM ) 
              WIN_S =   7938.D0/18608.D0 &
     &                + 9240.D0/18608.D0* DCOS ( (PI__NUM*(J1-1))/DIM2 ) &
     &                + 1430.D0/18608.D0* DCOS ( (PI2*(J1-1))/DIM2 ) 
              IF ( J1 .GE. DIM2 ) THEN
                   WIN_S = 0.0
              END IF
              WIN = WIN_B - WIN_S
            ELSE IF ( MODE_STR == 'bu-mask' ) THEN
!
! ----------- Blackman window truncated from the lower dimension DIM2.
! ----------- This is differences of two Blackman windows: 
! ----------- at dimension DIM minus at dimension DIM2
!
              WIN_S =   7938.D0/18608.D0 &
     &                + 9240.D0/18608.D0* DCOS ( (PI__NUM*(J1-1))/DIM2 ) &
     &                + 1430.D0/18608.D0* DCOS ( (PI2*(J1-1))/DIM2 ) 
              WIN = 1.0 - WIN_S
            ELSE IF ( MODE_STR == 'p-mask' ) THEN
!
! ----------- Set no window
!
              WIN = 1.0D0
         END IF
         DO 420 J2=1,DIM+1
            HEB_IN%VAL8(1:2,J1,J2,1) = WIN*HEB_IN%VAL8(1:2,J1,J2,1)
 420     CONTINUE 
 410  CONTINUE 
!
      WRITE ( 6, 110 ) DIM
 110  FORMAT ( 'Started inverse SHT of dimension ', I6 )
      TIM_WALL_R8 = WALL_TIMER ( %VAL(0) )
      TIM_CPU_R8  = CPU_TIMER  ( %VAL(0) )
      CALL ERR_PASS ( IUER, IER )
      CALL SPHE_INV_2NN ( FSH, INT(HEB_IN%DIMS(2),KIND=4)-1, DIM, 1, 1, &
     &                    HEB_IN%VAL8, 2*(DIM+1), HEB_OUT%VAL8, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 3612, IUER, 'GEN_INV_MASK_SHT', 'Error in performing'// &
     &         ' inverse spherical harmonic transform' )
           RETURN 
      END IF
      TIM_CPU_R8  = CPU_TIMER  ( %VAL(2) )
      TIM_WALL_R8 = WALL_TIMER ( %VAL(2) )
      WRITE ( 6, 130 ) DIM, TIM_WALL_R8, TIM_CPU_R8
 130  FORMAT ( 'Ended   inverse SHT of dimension ', I6, ' Wall time: ', &
     &          F10.3, ' sec ', 'CPU time: ', F10.3 )
      DEALLOCATE ( HEB_IN%VAL8 ) 
!
      ALLOCATE ( HEB_OUT%VAL(HEB_OUT%DIMS(1),HEB_OUT%DIMS(2),HEB_OUT%DIMS(3),HEB_OUT%DIMS(4)), &
     &           STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH8 ( INT8(4)*HEB_OUT%DIMS(1)*HEB_OUT%DIMS(2)*HEB_OUT%DIMS(3), STR )
           CALL ERR_LOG ( 3613, IUER, 'GEN_INV_MASK_SHT', 'Failure to allocate '// &
     &          STR(1:I_LEN(STR))//' bytes of dynamic memory for array '// &
     &         'HEB_OUT%VAL8' )
           RETURN 
      END IF
      HEB_OUT%VAL = 0.0
      CALL SPD_R8_TO_R4 ( HEB_OUT%DIMS(1)*HEB_OUT%DIMS(2), HEB_OUT%VAL8, HEB_OUT%VAL )
      DEALLOCATE ( HEB_OUT%VAL8 ) 
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE   GEN_INV_MASK_SHT  !#!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE GEN_COAST_MASK ( HEB_LW, IVRB, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  GEN_COAST_MASK 
! *                                                                      *
! * ### 07-JAN-2016  GEN_COAST_MASK  v1.0 (c) L. Petrov  08-JAN-2016 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'malo.i'
      INCLUDE   'heb.i'
      TYPE     ( HEB__TYPE  ) :: HEB_LW
      INTEGER*4  IVRB, IUER 
      INTEGER*4  IND_LAT_PREV, IND_LAT_HERE, IND_LAT_NEXT, &
     &           IND_LON_PREV, IND_LON_HERE, IND_LON_NEXT, &
     &           J1, J2, J3, J4, IER
      INTEGER*1  LS_SUM
      INTEGER*1, ALLOCATABLE :: LW(:,:,:,:)
      CHARACTER  STR*128
      INTEGER*4, EXTERNAL :: ILEN, I_LEN
!
      IF ( IVRB > 2 ) THEN
           WRITE ( 6, * ) 'GEN_COAST_MASK 502' ; CALL FLUSH ( 6 )
      END IF
      ALLOCATE ( HEB_LW%VAL1(HEB_LW%DIMS(1),HEB_LW%DIMS(2),HEB_LW%DIMS(3),HEB_LW%DIMS(4)), &
     &           STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 5771, IUER, 'GEN_COAST_MASK', 'Failure to '// &
     &          'allocated '//STR(1:I_LEN(STR))//' bytes of dynamic '// &
     &          'memory for array HEB_LW%VAL1' )
           RETURN 
      END IF
      IF ( IVRB > 2 ) THEN
           WRITE ( 6, * ) 'GEN_COAST_MASK 513' ; CALL FLUSH ( 6 )
      END IF
      ALLOCATE ( LW(HEB_LW%DIMS(1),HEB_LW%DIMS(2),HEB_LW%DIMS(3),HEB_LW%DIMS(4)), &
     &           STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 5771, IUER, 'GEN_COAST_MASK', 'Failure to '// &
     &          'allocated '//STR(1:I_LEN(STR))//' bytes of dynamic '// &
     &          'memory for array LW' )
           RETURN 
      END IF
      IF ( IVRB > 2 ) THEN
           WRITE ( 6, * ) 'GEN_COAST_MASK 524' ; CALL FLUSH ( 6 )
      END IF
!
      DO 410 J1=1,HEB_LW%DIMS(2)
          DO 420 J2=1,HEB_LW%DIMS(1)
             IF ( NINT(HEB_LW%VAL(J2,J1,1,1)) > 0.05 ) THEN
                  LW(J2,J1,1,1) = MALO__LAND_VAL
                ELSE
                  LW(J2,J1,1,1) = MALO__WATER_VAL
             END IF
 420      CONTINUE 
 410  CONTINUE 
      IF ( IVRB > 2 ) THEN
           WRITE ( 6, * ) 'GEN_COAST_MASK 537' ; CALL FLUSH ( 6 )
      END IF
!
      DO 430 J3=1,HEB_LW%DIMS(2)
         IND_LAT_PREV = J3-1
         IND_LAT_HERE = J3
         IND_LAT_NEXT = J3+1
         IF ( IND_LAT_PREV < 1              ) IND_LAT_PREV = 2
         IF ( IND_LAT_NEXT > HEB_LW%DIMS(2) ) IND_LAT_NEXT = HEB_LW%DIMS(2) - 1
         DO 440 J4=1,HEB_LW%DIMS(1)
            IND_LON_PREV = J4-1
            IND_LON_HERE = J4
            IND_LON_NEXT = J4+1
            IF ( IND_LON_PREV < 1              ) IND_LON_PREV = HEB_LW%DIMS(1)
            IF ( IND_LON_NEXT > HEB_LW%DIMS(1) ) IND_LON_NEXT = 1
            HEB_LW%VAL1(J4,J3,1,1) = MALO__WATER_VAL
            IF ( LW(IND_LON_HERE,IND_LAT_HERE,1,1) == MALO__WATER_VAL ) THEN
                 LS_SUM = LW(IND_LON_PREV,IND_LAT_HERE,1,1) + &
     &                    LW(IND_LON_HERE,IND_LAT_PREV,1,1) + &
     &                    LW(IND_LON_HERE,IND_LAT_NEXT,1,1) + &
     &                    LW(IND_LON_NEXT,IND_LAT_HERE,1,1)
                 IF ( LS_SUM > 0 ) THEN
                      HEB_LW%VAL1(J4,J3,1,1) = MALO__LAND_VAL
                 END IF
            END IF
 440     CONTINUE 
 430  CONTINUE 
      IF ( IVRB > 2 ) THEN
           WRITE ( 6, * ) 'GEN_COAST_MASK 585' ; CALL FLUSH ( 6 )
      END IF
      DEALLOCATE ( LW ) 
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  GEN_COAST_MASK  !#!  
!
! ------------------------------------------------------------------------
!
      SUBROUTINE MASK_AVR ( MODE_STR, HEB_IN, DIM, HEB_OUT, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine MASK_AVR
! *                                                                      *
! *  ### 22-FEB-2016    MASK_AVR  v1.0 (c)   L. Petrov  22-FEB-2016 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'heb.i'
      INCLUDE   'malo.i'
      INTEGER*4  DIM, IUER
      CHARACTER  MODE_STR*(*)
      TYPE     ( HEB__TYPE  ) :: HEB_IN, HEB_OUT
      CHARACTER  STR*128
      INTEGER*4  J1, J2, J3, J4, ILON, ILAT, IR, IER
      INTEGER*4, EXTERNAl :: ILEN, I_LEN
!
      HEB_OUT = HEB_IN
      HEB_OUT%DIMS(1) = (DIM+1)*4
      HEB_OUT%DIMS(2) = (DIM+1)*2 + 1
      HEB_OUT%DIMS(3) = 1
      HEB_OUT%DIMS(4) = 1
      IR = HEB_IN%DIMS(1)/HEB_OUT%DIMS(1)
      IF ( HEB_IN%DIMS(1) .NE. IR*HEB_OUT%DIMS(1) ) THEN
           CALL ERR_LOG ( 5781, IUER, 'MASK_AVR', 'Wrong dimension: '// &
     &         'it should be commensurate to dimension of the MODIS '// &
     &         'land/lake/sea mask' )
           RETURN 
      END IF
!
      ALLOCATE ( HEB_OUT%VAL(HEB_OUT%DIMS(1),HEB_OUT%DIMS(2),HEB_OUT%DIMS(3),HEB_OUT%DIMS(4)), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH8 ( INT8(4)*HEB_OUT%DIMS(1)*HEB_OUT%DIMS(2), STR )
           CALL ERR_LOG ( 5781, IUER, 'MASK_AVR', 'Failure to allocate '// &
     &          STR(1:I_LEN(STR))//' bytes of dynamic memory for array '// &
     &         'HEB_OUT%VAL' )
           RETURN 
      END IF
!
      HEB_OUT%VAL = 0.0
      IF ( MODE_STR == 'ls_avr' ) THEN
           DO 410 J1=1,HEB_IN%DIMS(2)
              ILAT = NINT((J1-1.0)/IR+0.0001) + 1
              DO 420 J2=1,HEB_IN%DIMS(1)
                 ILON = NINT((J2-1.0)/IR+0.0001) + 1
                 IF ( ILON > HEB_OUT%DIMS(1) ) ILON = ILON - HEB_OUT%DIMS(1) 
                 IF ( HEB_IN%VAL1(J2,J1,1,1) .NE. MALO__SEA_VAL ) THEN
                      HEB_OUT%VAL(ILON,ILAT,1,1) = HEB_OUT%VAL(ILON,ILAT,1,1) + 1.0
                 END IF
 420          CONTINUE 
 410       CONTINUE 
      END IF
      HEB_OUT%VAL = HEB_OUT%VAL/(IR*IR)
      HEB_OUT%VAL(1:HEB_OUT%DIMS(1),1,1,1 ) = 1.0
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  MASK_AVR  !#!#
