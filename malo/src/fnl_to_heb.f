      PROGRAM  FNL_TO_HEB
! ************************************************************************
! *                                                                      *
! *   Program FNL_TO_HEB reads numberical weather data files from NCEP   *
! *   in GRIB1 format,extraces geopotential height, air temperature,     *
! *   partial pressure of the water vapor, and geopotenial at the        *
! *   surface andwrites them into HEB-format.                            *
! *                                                                      *
! * ###  24-JUL-2013  FNL_TO_HEB  v1.3 (c)  L. Petrov  30-AUG-2013  ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'heb.i'
      TYPE     ( HEB__TYPE ) :: HEB
      INTEGER*4  M_LON, M_LAT, MW
      PARAMETER  ( M_LON = 5760, M_LAT = 2880, MW = 256 )
      REAL*4     PRES_MAX
      PARAMETER  ( PRES_MAX = 32000.0 ) ! Pa
      REAL*4,    ALLOCATABLE :: ARR_R4(:,:,:), TEMP(:,:,:)
      LOGICAL*1, ALLOCATABLE :: ARR_L1(:,:,:)
      CHARACTER  FILIN*128, FIL_GEOID*128, HEB_PREF*128, FILHEB*128, &
     &           STR*128, DATE_STR*19
      INTEGER*4  NP1, NP2, NN
      PARAMETER  ( NP1 =  4 )
      PARAMETER  ( NP2 = 26 )
      INTEGER*2  MODE_I2
      DATA       MODE_I2 / O'0775' /
      CHARACTER  COMPR*16, COMPR_COM*64, COM_STR*256, PREF(NP1)*1
      INTEGER*4  SDS_ID1(NP1), SDS_ID2(NP1), SDS_ID3(NP2), K_LON, K_LAT, K_HEI
      REAL*8     PRES_LEV(NP2)
      DATA     ( SDS_ID1(NN),  SDS_ID2(NN), PREF(NN), NN=1,NP1 ) &
     &         / &
     &             7, 26, 'h', & !  1
     &            11, 26, 't', & !  2
     &            52, 21, 'w', & !  3
     &             7,  1, 'g'  & !  4
     &         /
      DATA     ( SDS_ID3(NN),  NN=1,NP2 ) &
     &         / &
     &            1000, & !   1 
     &            975,  & !   2 
     &            950,  & !   3 
     &            925,  & !   4 
     &            900,  & !   5 
     &            850,  & !   6 
     &            800,  & !   7 
     &            750,  & !   8 
     &            700,  & !   9 
     &            650,  & !  10 
     &            600,  & !  11 
     &            550,  & !  12 
     &            500,  & !  13 
     &            450,  & !  14 
     &            400,  & !  15 
     &            350,  & !  16 
     &            300,  & !  17 
     &            250,  & !  18 
     &            200,  & !  19 
     &            150,  & !  20 
     &            100,  & !  21 
     &            70,   & !  22 
     &            50,   & !  23 
     &            30,   & !  24 
     &            20,   & !  25 
     &            10    & !  26 
     &         /
      PARAMETER  ( K_LON = 360 )
      PARAMETER  ( K_LAT = 181 )
      PARAMETER  ( K_HEI =  26 )
      LOGICAL*1  LEX
      INTEGER*4  J1, J2, J3, J4, J5, J6, L_LON, L_LAT, LUN, LUGI, MODE, &
     &           IX, ID, IP, IS, DIR_DESC, IUER
      INTEGER*4  JPDS(MW), JGDS(MW), N_PT, L_REC, KPDS(MW), KGDS(MW)
      REAL*8     LAT_MIN, LON_MIN, LAT_MAX, LON_MAX
      INTEGER*4, EXTERNAL :: ILEN, I_LEN, LINDEX, OPENDIR, CLOSEDIR, MKDIR
!
      IF ( IARGC() < 3 ) THEN
           WRITE ( 6, '(A)' ) 'Usage: fnl_to_geb mode input_grib_file '// &
     &                        'output_heb_file/dir [compr]' 
           CALL EXIT ( 1 )
         ELSE
           CALL GETARG ( 1, STR )
           CALL CHIN ( STR, MODE )
           IF ( MODE < 1 .OR. MODE > 1 ) THEN
                IUER = -1
                CALL ERR_LOG ( 1801, IUER, 'FNL_TO_HEB', 'Wrong mode '// &
     &               STR(1:I_LEN(STR))//' only mode 1 is supported' )
                CALL EXIT ( 1 )
           END IF
           CALL GETARG ( 2, FILIN     )
           CALL GETARG ( 3, HEB_PREF  )
           IF ( IARGC() .GE. 4 ) THEN
                CALL GETARG ( 4, COMPR )
              ELSE
                COMPR = 'none'
           END IF
      END IF
      PRES_LEV = 100.0*SDS_ID3
!
! --- Check compression method
!
      IF ( COMPR == 'none' ) THEN
           CONTINUE 
           CALL CLRCH ( COMPR_COM )
         ELSE IF ( COMPR == 'gzip' ) THEN
           COMPR_COM = 'gzip -1 -f '
         ELSE IF ( COMPR == 'bzip2' ) THEN
           COMPR_COM = 'bzip2 -9 -f '
         ELSE IF ( COMPR == 'pbzip2' ) THEN
           COMPR_COM = 'pbzip2 -r -m1024 -S4096 -9 -f '
         ELSE IF ( COMPR == 'pbzip2_p1' ) THEN
           COMPR_COM = 'pbzip2 -r -m1024 -S4096 -9 -p1 -f '
         ELSE 
           CALL ERR_LOG ( 1802, -2, 'FNL_TO_HEB', 'Unsupported '// &
     &         'compression method: '//COMPR//' . Supported methods: '// &
     &         ' none gzip bzip2 pbzip2 pbzip2_p1' )
           CALL EXIT ( 1 )
      END IF
!
      ALLOCATE ( ARR_R4(K_LON,K_LAT,K_HEI), STAT=IUER )
      IF ( IUER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( 4*K_LON*K_LAT*K_HEI, STR )
           CALL ERR_LOG ( 1803, -2, 'FNL_TO_HEB', 'Failure to allocate '// &
     &          STR(1:I_LEN(STR))//' bytes of dynamic memory for array ARR_R4' )
           CALL EXIT ( 1 )
      END IF
!
      ALLOCATE ( ARR_L1(K_LON,K_LAT,K_HEI), STAT=IUER )
      IF ( IUER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( K_LON*K_LAT*K_HEI, STR )
           CALL ERR_LOG ( 1804, -2, 'FNL_TO_HEB', 'Failure to allocate '// &
     &          STR(1:I_LEN(STR))//' bytes of dynamic memory for array ARR_L1' )
           CALL EXIT ( 1 )
      END IF
!
      LUN = 50
      LUGI = 0
!
! --- Check whether the input file exists
!
      INQUIRE ( FILE=FILIN, EXIST=LEX ) 
      IF ( .NOT. LEX ) THEN
           CALL ERR_LOG ( 1805, -2, 'FNL_TO_HEB', 'Unput grib1 file '// &
     &          FILIN(1:I_LEN(FILIN))//' was not found' )
           CALL EXIT ( 1 )
      END IF
!
! --- Open the input file
!
      CALL BAOPENR ( LUN, FILIN, IUER )
      JPDS = -1
      JGDS = -1
      IF ( MODE == 1 ) THEN
!
! -------- Set dimensions of the output file
!
           HEB%DIMS(1) = K_LON
           HEB%DIMS(2) = K_LAT
           HEB%DIMS(3) = K_HEI
           HEB%DIMS(4) = 1
           ALLOCATE ( HEB%VAL(HEB%DIMS(1),HEB%DIMS(2),HEB%DIMS(3),1), STAT=IUER )
           IF ( IUER .NE. 0 ) THEN
                CALL CLRCH  ( STR ) 
                CALL IINCH8 ( INT8(4)*HEB%DIMS(1)*HEB%DIMS(2)*HEB%DIMS(3), STR )
                CALL ERR_LOG ( 1806, IUER, 'FNL_TO_HEB', 'Error in '// &
     &              'an attempt to allocate '//STR(1:I_LEN(STR))// &
     &              ' bytes of dynamic memory for array HEB%VAL' )
                CALL EXIT ( 1 )
           END IF
           HEB%VAL = 0.0
!
           ALLOCATE ( TEMP(HEB%DIMS(1),HEB%DIMS(2),HEB%DIMS(3)), STAT=IUER )
           IF ( IUER .NE. 0 ) THEN
                CALL CLRCH  ( STR ) 
                CALL IINCH8 ( INT8(4)*HEB%DIMS(1)*HEB%DIMS(2)*HEB%DIMS(3), STR )
                CALL ERR_LOG ( 1807, IUER, 'FNL_TO_HEB', 'Error in '// &
     &              'an attempt to allocate '//STR(1:I_LEN(STR))// &
     &              ' bytes of dynamic memory for array HEB%VAL' )
                CALL EXIT ( 1 )
           END IF
           TEMP = 0.0
!
! -------- Cycle over 3 constituents
!
           DO 410 J1=1,NP1
              ARR_R4 = 0.0
              DO 420 J2=1,SDS_ID2(J1)
!
! -------------- Set search parameters: SDS integer ID and the level index
!
                 JPDS(5) = SDS_ID1(J1)
                 JPDS(7) = SDS_ID3(J2)
                 IF ( J1 == NP1 ) THEN
                      JPDS(6) = 1
                      JPDS(7) = 0 
                 END IF
!
! -------------- Extract the SDS
!
                 CALL GETGB ( LUN, LUGI, K_LON*K_LAT, 0, JPDS, JGDS, &
     &                        N_PT, L_REC, KPDS, KGDS, ARR_L1(1,1,J2), &
     &                        ARR_R4(1,1,J2), IUER )
                 IF ( IUER .NE. 0 ) THEN
                      WRITE ( 6, * ) ' IUER = ', IUER
                      IUER = -1
                      CALL CLRCH ( STR )
                      CALL INCH ( J1, STR )
                      CALL ERR_LOG ( 1808, IUER, 'FNL_TO_HEB', 'Error in '// &
     &                    'reading parameter '//STR )
                      CALL EXIT ( 1 )
                 END IF
                 IF ( KPDS(8) == 100 ) KPDS(8) = 0 ! Fix for year 2000
!
! -------------- Encode the date
!
                 WRITE ( DATE_STR(1:19), 110 ) KPDS(8:12), 0
 110             FORMAT ( 'xx', I2, ".", I2, ".", I2, ":", I2, ":", I2, ":", &
     &                     I2 )
                 IF ( KPDS(8) < 50 ) DATE_STR(1:2) = '20'
                 IF ( KPDS(8) > 50 ) DATE_STR(1:2) = '19'
                 CALL BLANK_TO_ZERO ( DATE_STR(1:19) )
!
! -------------- Extract the grid parameters
!
                 L_LON   = KGDS(2)
                 L_LAT   = KGDS(3)
                 LAT_MIN = KGDS(4)/1000.0D0*DEG__TO__RAD
                 LON_MIN = KGDS(5)/1000.0D0*DEG__TO__RAD
                 LAT_MAX = KGDS(7)/1000.0D0*DEG__TO__RAD
                 LON_MAX = KGDS(8)/1000.0D0*DEG__TO__RAD
 420          CONTINUE 
              IUER = -1
              CALL FNL_PROC ( K_LON, K_LAT, K_HEI, SDS_ID1(J1), ARR_L1, ARR_R4, &
     &                        TEMP, PRES_LEV, IUER )
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!  if ( j1 == 3 ) then ! 13
!      iuer = -2
!      call plot_grid_r4 ( 1, 7, 0, 1, k_lon, k_lat, arr_r4(1,1,9), &
!     &                    'rel at 700mb', 'lg(proc)', '/tmp/boo', iuer )
!  end if
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
              IF ( J1 == 2 ) TEMP = ARR_R4
!
! ----------- Set HEB headers
!
              HEB%DATA_OFFSET = HEB__HDS
              HEB%ENDIAN      = HEB__LE
              HEB%FILL_VALUE     = 1.0E15
              HEB%DATA_COMPRESSION = HEB__NONE
              HEB%MIN_VALUE      = MINVAL(ARR_R4)
              HEB%MAX_VALUE      = MAXVAL(ARR_R4)
              HEB%VALID_RANGE(1) = 0
              HEB%VALID_RANGE(2) = HEB%MAX_VALUE      
              IF ( J1 == 1 ) THEN
                   HEB%DATA_TRANSFORM = HEB__SCOF
                   HEB%SDS_NAME       = 'Geometric height above geoid'
                   HEB%UNITS          = 'm'
                   HEB%DATA_FORMAT    = HEB__R4
                   HEB%OFFSET         = 0.0
                   HEB%SCALE_FACTOR   = 1.0
                   HEB%VALID_RANGE(1) = -500.0
                 ELSE IF ( J1 == 2 ) THEN
                   HEB%DATA_TRANSFORM = HEB__SCOF
                   HEB%SDS_NAME       = 'Air temperature'
                   HEB%UNITS          = 'K'
                   HEB%DATA_FORMAT    = HEB__I2
                   HEB%OFFSET         = 280.0
                   HEB%SCALE_FACTOR   = 3.D-3
                   HEB%VALID_RANGE(1) = 150.0
                 ELSE IF ( J1 == 3 ) THEN
                   HEB%DATA_TRANSFORM = HEB__SCOF
                   HEB%SDS_NAME       = 'Water Vapor Pressure'
                   HEB%UNITS          = 'Pa'
                   HEB%DATA_FORMAT    = HEB__I2
                   HEB%OFFSET         = 6000.0
                   HEB%SCALE_FACTOR   = 6000.0/32000.0
                   HEB%VALID_RANGE(1) = 0.0
                 ELSE IF ( J1 == 4 ) THEN
                   HEB%DATA_TRANSFORM = HEB__SCOF
                   HEB%SDS_NAME       = 'Surface height'
                   HEB%UNITS          = 'm'
                   HEB%DATA_FORMAT    = HEB__I2
                   HEB%OFFSET         = 4000.0
                   HEB%SCALE_FACTOR   = 8000.0/32000.0
                   HEB%DIMS(3)        = 1
                   HEB%VALID_RANGE(1) = -500.0
              END IF
              IUER = -1
              CALL DATE_TO_TIME ( DATE_STR, HEB%MJD, HEB%UTC, IUER )
              IF ( IUER .NE. 0 ) THEN
                   CALL ERR_LOG ( 1809, IUER, 'FNL_TO_HEB', 'Failure in '// &
     &                 'an attempt to decode data string '//DATE_STR )
                   CALL EXIT ( 1 )
              END IF
              IX = LINDEX ( HEB_PREF, '.heb' )
              IF ( IX == 0 ) IX = I_LEN(HEB_PREF)
              ID = LINDEX ( HEB_PREF, '/' )
              IF ( ID == 0 ) THEN
                   HEB_PREF = './'//HEB_PREF
                   ID = 2
              END IF
              IF ( ID == ILEN(HEB_PREF) ) ID = ID-1
              DIR_DESC = OPENDIR ( HEB_PREF(1:ID)//'/'//DATE_STR(1:4)//CHAR(0) )
              IF ( DIR_DESC .LE. 0 ) THEN
                   IS = MKDIR ( HEB_PREF(1:ID)//'/'//DATE_STR(1:4)//CHAR(0), &
     &                          %VAL(MODE_I2) )
                   IF ( IS .EQ. -1 ) THEN
                        IUER = -1
                        CALL GERROR ( STR )
                        CALL ERR_LOG ( 1810, IUER, 'FNL_TO_HEB', 'Failure '// &
     &                      'in an attempt to create output directory '// &
     &                       HEB_PREF(1:ID)//'/'//DATE_STR(1:4) )
                        CALL EXIT ( 1 )
                   END IF
                 ELSE 
                  IP = CLOSEDIR ( %VAL(DIR_DESC) )
              END IF
!              
              FILHEB = HEB_PREF(1:ID)//'/'//DATE_STR(1:4)//'/'//PREF(J1)//'/'// &
     &                 HEB_PREF(ID+1:IX-1)//PREF(J1)//'_'//DATE_STR(1:4)// &
     &                 DATE_STR(6:7)//DATE_STR(9:10)//'_'// &
     &                 DATE_STR(12:13)//DATE_STR(15:16)//'.heb'
              DIR_DESC = OPENDIR ( HEB_PREF(1:ID)//'/'//DATE_STR(1:4)//'/'//PREF(J1)//CHAR(0) )
              IF ( DIR_DESC .LE. 0 ) THEN
                   IS = MKDIR ( HEB_PREF(1:ID)//'/'//DATE_STR(1:4)//'/'//PREF(J1)//CHAR(0), %VAL(MODE_I2) )
                   IF ( IS .EQ. -1 ) THEN
                        IUER = -1
                        CALL GERROR ( STR )
                        CALL ERR_LOG ( 1811, IUER, 'FNL_TO_HEB', 'Failure '// &
     &                      'in an attempt to create output directory '// &
     &                       HEB_PREF(1:ID)//'/'//DATE_STR(1:4)//'/'//PREF(J1) )
                        CALL EXIT ( 1 )
                   END IF
                 ELSE 
                  IP = CLOSEDIR ( %VAL(DIR_DESC) )
              END IF
!
              HEB%TAI = HEB%UTC
              HEB%PROD_NAME = 'FNL'
              HEB%HISTORY   = '???'
              HEB%SOURCE    = 'NCEP'
              HEB%TITLE     = '1.0 Degree 6-Hourly Data From the NCEP Operational weather model'
              HEB%INSTITUTION    = 'NCEP'
              HEB%REFERENCES     = 'n/a'
              HEB%PROD_DATE_TIME = 'n/a'
              HEB%VERSION_ID     = '1'
              HEB%FILE_NAME      = FILHEB
!
! ----------- Write the output heb-file
!
              IUER = -1
              CALL WRITE_HEB ( HEB, ARR_R4, FILHEB, IUER )
              IF ( IUER .NE. 0 ) THEN
                   CALL ERR_LOG ( 1812, IUER, 'FNL_TO_HEB', 'Error in '// &
     &                 'an attempt to write output file in heb format: '//FILHEB )
                   CALL EXIT ( 1 )
              END IF
!
              IF ( ILEN(COMPR_COM) .NE. 0 ) THEN
!
! ---------------- Now compress the output file 
!
                  COM_STR = COMPR_COM(1:I_LEN(COMPR_COM))//' '// &
     &                      FILHEB(1:I_LEN(FILHEB))
                  CALL SYSTEM ( COM_STR(1:I_LEN(COM_STR))//CHAR(0) )
              END IF
 410      CONTINUE 
      END IF
!
! --- Close the input of the FNL file
!
      CALL BACLOSE  ( LUN, IUER )
      END  PROGRAM  FNL_TO_HEB  !#!  
!
! ------------------------------------------------------------------------
!
      SUBROUTINE FNL_PROC ( L_LON, L_LAT, L_HEI, SDS_ID, ARR_L1, ARR_R4, &
     &                      TEMP_R4, PRES_LEV, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine FNL_PROC 
! *                                                                      *
! *  ### 29-JUL-2013    FNL_PROC   v1.0 (c)  L. Petrov  29-JUL-2013 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'heb.i'
!
      TYPE     ( HEB__TYPE ) :: HEB
      INTEGER*4  L_LON, L_LAT, L_HEI, SDS_ID, IUER 
      LOGICAL*1  ARR_L1(L_LON,L_LAT,L_HEI)
      REAL*4     ARR_R4(L_LON,L_LAT,L_HEI), TEMP_R4(L_LON,L_LAT,L_HEI)
      REAL*8     PRES_LEV(L_HEI)
      REAL*4     SWAP, LAT_STEP, PRES_SAT
      REAL*8     LAT_GCN
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
      INTEGER*4  J1, J2, J3, J4, J5, J6, J7, J8, J9
      REAL*8,    EXTERNAL :: GPH_TO_HEI, LEGENDRE_VAL
!      
      LAT_STEP = PI__NUM/(L_LAT-1)
!
      DO 410 J1=1,L_HEI
         DO 420 J2=1,L_LON
            DO 430 J3=1,L_LAT/2
               SWAP = ARR_R4(J2,J3,J1)
               ARR_R4(J2,J3,J1) = ARR_R4(J2,L_LAT+1-J3,J1)
               ARR_R4(J2,L_LAT+1-J3,J1) = SWAP
 430        CONTINUE 
 420     CONTINUE 
 410  CONTINUE 
!
      IF ( SDS_ID == 7 ) THEN
           DO 440 J4=1,L_HEI
              DO 450 J5=1,L_LAT
                 LAT_GCN = -P2I + LAT_STEP*(J5-1)
                 DO 460 J6=1,L_LON 
                    ARR_R4(J6,J5,J4) = GPH_TO_HEI ( LAT_GCN, DBLE(ARR_R4(J6,J5,J4)) ) 
 460             CONTINUE 
 450          CONTINUE 
 440       CONTINUE 
      END IF
      IF ( SDS_ID == 52 ) THEN
           DO 470 J7=1,L_HEI
              DO 480 J8=1,L_LAT
                 DO 490 J9=1,L_LON 
                    PRES_SAT = DEXP ( LEGENDRE_VAL ( L_GG, T_MIN, T_MAX, DBLE(TEMP_R4(J9,J8,J7)), GG_LEG_COEF ) )
                    ARR_R4(J9,J8,J7) = PRES_SAT*ARR_R4(J9,J8,J7)/100.0
!!                    ARR_R4(J9,J8,J7) = LOG10(ARR_R4(J9,J8,J7)/100.0 + 1.e-6)
 490             CONTINUE 
 480          CONTINUE 
 470       CONTINUE 
      END IF
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  FNL_PROC  !#!  
!
! ------------------------------------------------------------------------
!
      FUNCTION   GPH_TO_HEI ( LAT_GCN, GPH )
! ************************************************************************
! *                                                                      *
! *   Routine GPH_TO_HEI converts geopotential height to geometric       *
! *   height for a given geocentric latitude.                            *
! *                                                                      *
! *  ### 11-DEC-2007   GPH_TO_HEI  v1.2 (c)  L. Petrov  03-APR-2008 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      REAL*8     GPH_TO_HEI
      REAL*8     LAT_GCN, GPH
      REAL*8     REA, FE, EXC_SQ, ACC_EQU, GRV_LAT, GM, OM__EAR, ACC_REF, MR
!
! --- Constants according to the WGS-84 reference ellipsoid
!
      PARAMETER  ( REA     = 6378137.0D0 )       ! Earth's equatorial radius
      PARAMETER  ( FE      = 1.D0/298.257223563D0 )    ! Earth's flattening
      PARAMETER  ( EXC_SQ  = 2.D0*FE - FE**2 )   ! Earth's eccentricity
      PARAMETER  ( ACC_EQU = 9.7803253359D0 )    ! Equatorial gravity acc.
      PARAMETER  ( GRV_LAT = 0.001931852653D0  ) ! D(ACC_EQU)/D(phi)
      PARAMETER  ( GM      = 3.986004418D14    ) ! GM for the EArth
      PARAMETER  ( OM__EAR = 7.292115146706979D-05 ) ! rad/sec
      PARAMETER  ( MR      = OM__EAR**2*REA**3*(1.D0-FE)/GM )
      PARAMETER  ( ACC_REF = 9.80665D0      )    ! Reference gravity acc.
      REAL*8      PI, PI2, P2I
      PARAMETER ( PI=3.141592653589793D0, PI2=2.D0*PI, P2I=PI/2D0 ) ! Pi
      REAL*8     MU, LAT_GDT, RAD_EFF, G_ACC
!
! --- Computation of geodetic latitude
!
      MU = DATAN ( DTAN(LAT_GCN) * ( 1.D0 - FE + EXC_SQ*(1.0D0 - GPH/REA) ) )
!
      LAT_GDT = DATAN( ( (1.0D0 + GPH/REA)*(1.D0 - FE)*DSIN(LAT_GCN) + EXC_SQ*DSIN(MU)**3 ) / &
     &                 ( (1.D0 - FE)*( (1.0D0 + GPH/REA)*DCOS(LAT_GCN) - EXC_SQ*DCOS(MU)**3 )) )
      RAD_EFF = REA/(1 + FE + MR - 2.D0*FE*DSIN(LAT_GDT)**2)
!
      G_ACC = ACC_EQU* (1.D0 + GRV_LAT*DSIN(LAT_GDT)**2 ) / &
     &          DSQRT  (1.D0 - EXC_SQ*DSIN(LAT_GDT)**2)
!      
      GPH_TO_HEI = RAD_EFF*GPH/(G_ACC/ACC_REF*RAD_EFF - GPH)
!
      RETURN
      END  FUNCTION   GPH_TO_HEI  !#!#
