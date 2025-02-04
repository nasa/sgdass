      PROGRAM  GRIB1_TO_HEB
! ************************************************************************
! *                                                                      *
! *   Program GRIB1_TO_HEB
! *                                                                      *
! *  ### 22-APR-2013  GRIB1_TO_HEB v1.1 (c)  L. Petrov  09-OCT-2013 ###  *
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
      REAL*4     ARR_R4(M_LON,M_LAT)
      LOGICAL*1  ARR_L1(M_LON,M_LAT)
      CHARACTER  FILIN*128, HEB_PREF*128, FILHEB*128, STR*128, DATE_STR*19
      CHARACTER  COMPR*16, COMPR_COM*64, COM_STR*256
      INTEGER*4  NP1, NP2, N1, N2
      PARAMETER  ( NP1 = 6 )
      PARAMETER  ( NP2 = 3 )
      INTEGER*4  SDS_ID1(NP1,NP2), LEV_IND1(NP1,NP2), K_LON1, K_LAT1
      DATA     ( ( SDS_ID1(N1,N2),  LEV_IND1(N1,N2), N1=1,NP1 ), N2=1,NP2 ) &
     &         / &
     &            65, 0, & !  1
     &            86, 1, & !  2
     &            86, 2, & !  3
     &            86, 3, & !  4
     &            86, 4, & !  5
     &            71, 0, & !  6
!
     &            65,     0, & !  1
     &            86,     2, & !  2
     &            86,   614, & !  3
     &            86, 26158, & !  4
     &           223,     0, & !  5
     &             0,     0, & !  6
!
     &             1, 0, & !  1
     &             0, 0, & !  2
     &             0, 0, & !  3
     &             0, 0, & !  4
     &             0, 0, & !  5
     &             0, 0  & !  6
     &         /
      LOGICAL*1  LEX
      INTEGER*2  MODE_I2
      DATA       MODE_I2 / O'0775' /
      INTEGER*4  J1, J2, J3, J4, J5, J6, L_LON, L_LAT, LUN, LUGI, MODE, &
     &           ID, IS, IUER
      INTEGER*4  JPDS(MW), JGDS(MW), N_PT, L_REC, KPDS(MW), KGDS(MW)
      REAL*8     LAT_MIN, LON_MIN, LAT_MAX, LON_MAX
      INTEGER*8  DIR_DESC
      INTEGER*4, EXTERNAL :: ILEN, I_LEN, CLOSEDIR, MKDIR, OPENDIR, LINDEX
!
      IF ( IARGC() < 3 ) THEN
           WRITE ( 6, '(A)' ) 'Usage: grib1_to_heb mode input_grib_file output_heb_file [compr]' 
           CALL EXIT ( 1 )
         ELSE
           CALL GETARG ( 1, STR )
           CALL CHIN ( STR, MODE )
           IF ( MODE < 1 .OR. MODE > 3 ) THEN
                IUER = -1
                CALL ERR_LOG ( 1801, IUER, 'GRIB1_TO_HEB', 'Wrong mode '// &
     &               STR(1:I_LEN(STR))//' only mode 1 and 2 are supported' )
                CALL EXIT ( 1 )
           END IF
           CALL GETARG ( 2, FILIN    )
           CALL GETARG ( 3, HEB_PREF )
           IF ( IARGC() > 3 ) THEN
                CALL GETARG ( 4, COMPR )
              ELSE
                COMPR = 'none'
           END IF
      END IF
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
           CALL ERR_LOG ( 1802, -2, 'GRIB1_TO_HEB', 'Unsupported '// &
     &         'compression method: '//COMPR//' . Supported methods: '// &
     &         ' none gzip bzip2 pbzip2 pbzip2_p1' )
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
           CALL ERR_LOG ( 1803, -2, 'GRIB1_TO_HEB', 'Unput grib1 file '// &
     &          FILIN(1:I_LEN(FILIN))//' was not found' )
           CALL EXIT ( 1 )
      END IF
!
! --- Open the input file
!
      CALL BAOPENR ( LUN, FILIN, IUER )
      JPDS = -1
      JGDS = -1
!
! --- Set dimensions of the output file
!
      IF ( MODE == 1 ) THEN
           HEB%DIMS(1) = 1440 
           HEB%DIMS(2) = 720 
         ELSE IF ( MODE == 2 .OR. MODE == 3 ) THEN
           HEB%DIMS(1) = 360
           HEB%DIMS(2) = 180
      END IF
      HEB%DIMS(3) = 1
      HEB%DIMS(4) = 1
      ALLOCATE ( HEB%VAL(HEB%DIMS(1),HEB%DIMS(2),1,1), STAT=IUER )
      IF ( IUER .NE. 0 ) THEN
           CALL CLRCH  ( STR ) 
           CALL IINCH8 ( INT8(4)*HEB%DIMS(2)*HEB%DIMS(2), STR )
           CALL ERR_LOG ( 1804, IUER, 'GRIB1_TO_HEB', 'Error in '// &
     &         'an attempt to allocate '//STR(1:I_LEN(STR))// &
     &         ' bytes of dynamic memory for array HEB%VAL' )
           CALL EXIT ( 1 )
      END IF
      HEB%VAL = 0.0
!     
! --- Cycle over constituents
!     
      DO 410 J1=1,NP1
!     
! ------ Set search parameters: SDS integer ID and the level index
!     
         JPDS(5) = SDS_ID1(J1,MODE)
         JPDS(7) = LEV_IND1(J1,MODE)
!%  write ( 6, * ) ' jpds(5) = ', jpds(5), ' jpds(7) = ', jpds(7) ! %%%
         IF ( JPDS(5) == 0 ) GOTO 410
!     
! ------ Extract the SDS
!     
         CALL GETGB ( LUN, LUGI, M_LON*M_LAT, 0, JPDS, JGDS, &
     &                N_PT, L_REC, KPDS, KGDS, ARR_L1, ARR_R4, IUER )
         IF ( IUER .NE. 0 ) THEN
              WRITE ( 6, * ) ' IUER = ', IUER
              IUER = -1
              CALL CLRCH ( STR )
              CALL INCH ( J1, STR )
              CALL ERR_LOG ( 1805, IUER, 'GRIB1_TO_HEB', 'Error in '// &
     &            'reading parameter '//STR )
              CALL EXIT ( 1 )
         END IF
         IF ( KPDS(8) == 100 ) KPDS(8) = 0 ! Fix for year 2000
!     
! ------ Encode the date
!     
         WRITE ( DATE_STR(1:19), 110 ) KPDS(8:13)
 110     FORMAT ( 'xx', I2, ".", I2, ".", I2, ":", I2, ":", I2, ":", &
     &             I2 )
         IF ( KPDS(8) < 50 ) DATE_STR(1:2) = '20'
         IF ( KPDS(8) > 50 ) DATE_STR(1:2) = '19'
         CALL BLANK_TO_ZERO ( DATE_STR(1:19) )
!     
! ------ Extract the grid parameters
!     
         L_LON   = KGDS(2)
         L_LAT   = KGDS(3)
         LAT_MIN = KGDS(4)/1000.0D0*DEG__TO__RAD
         LON_MIN = KGDS(5)/1000.0D0*DEG__TO__RAD
         LAT_MAX = KGDS(7)/1000.0D0*DEG__TO__RAD
         LON_MAX = KGDS(8)/1000.0D0*DEG__TO__RAD
!     
! ------ Summ SDSs and make a simple grid transformation:
! ------ padding missed pixels in the the south, move lonitude
! ------ origin
!     
!!   call plot_grid_r4 ( 1, 7, 0, 1, l_lon, l_lat, arr_r4, 'nyam', 'h/z', '/tmp/boo', iuer ) ! %%%%%
         CALL SUM_ARR_R4 ( HEB, MODE, L_LON, L_LAT, LON_MIN, LON_MAX, &
     &                     LAT_MIN, LAT_MAX, PRES_MAX, ARR_R4 )
 410  CONTINUE 
!
! --- Close the input grib1 file
!
      CALL BACLOSE  ( LUN, IUER )
!
! --- Set HEB headers
!
      HEB%DATA_OFFSET = HEB__HDS
      HEB%ENDIAN      = HEB__LE
      HEB%DATA_TRANSFORM = HEB__SCOF
      HEB%FILL_VALUE     = 1.0E15
      HEB%DATA_COMPRESSION = HEB__NONE
      HEB%SDS_NAME       = 'Pressure caused by land water mass storage'
      HEB%UNITS          = 'Pa'
      HEB%DATA_FORMAT    = HEB__I2
      HEB%MIN_VALUE      = MINVAL(HEB%VAL)
      HEB%MAX_VALUE      = MAX ( PRES_MAX, MAXVAL(HEB%VAL) )
      HEB%VALID_RANGE(1) = 0
      HEB%VALID_RANGE(2) = PRES_MAX
      HEB%OFFSET         = 16000.0
      HEB%SCALE_FACTOR   = 0.5
      IUER = -1
      CALL DATE_TO_TIME ( DATE_STR, HEB%MJD, HEB%UTC, IUER )
      HEB%UTC = HEB%UTC - 1.0D0 ! ?? Do not know why, but gldas had epoch 1 sec forward
      IF ( IUER .NE. 0 ) THEN
           CALL ERR_LOG ( 1806, IUER, 'GRIB1_TO_HEB', 'Failure in '// &
     &         'an attempt to decode data string '//DATE_STR )
           CALL EXIT ( 1 )
      END IF
      FILHEB = HEB_PREF(1:I_LEN(HEB_PREF))//DATE_STR(1:4)//DATE_STR(6:7)// &
     &         DATE_STR(9:10)//'_'//DATE_STR(12:13)//DATE_STR(15:16)//'.heb'
      HEB%TAI = HEB%UTC
      HEB%PROD_NAME = 'GLDAS'
      HEB%HISTORY   = 'Imported from ftp://agdisc.gsfc.nasa.gov, summed soil moisture, canoply and snow water contents'
      HEB%SOURCE    = 'GLDAS'
      IF ( MODE == 1 ) THEN
           HEB%TITLE     = '0.25 Degree 3-Hourly Data From the Noah Model'
         ELSE IF ( MODE == 2 ) THEN
           HEB%TITLE     = '1.00 Degree 3-Hourly Data From the GLDAS-2.0 Model'
         ELSE IF ( MODE == 3 ) THEN
           HEB%TITLE     = '1.00 Degree 3-Hourly Data From the GLDAS-2.0 Model'
      END IF
      HEB%INSTITUTION    = 'Processed in Astrogeo Center. Originally developed by GLDAS'
      HEB%REFERENCES     = 'n/a'
      HEB%PROD_DATE_TIME = 'n/a'
      HEB%VERSION_ID     = '1'
      HEB%FILE_NAME      = FILHEB
!
! --- Write the output heb-file
!
      ID = LINDEX ( FILHEB, '/' )
      IF ( ID > 1 ) THEN
           DIR_DESC = OPENDIR ( FILHEB(1:ID-1)//CHAR(0) )
           IF ( DIR_DESC .LE. 0 ) THEN
                ID = LINDEX ( FILHEB(1:ID-1), '/' )
                DIR_DESC = OPENDIR ( FILHEB(1:ID-1)//CHAR(0) )
                IF ( DIR_DESC .LE. 0 ) THEN
                     IS = MKDIR ( FILHEB(1:ID-1)//CHAR(0), %VAL(MODE_I2) )
                     IF ( IS .EQ. -1 ) THEN
                          CALL GERROR ( STR )
                          CALL ERR_LOG ( 1807, -2, 'GRIB1_TO_HEB', 'Failure '// &
     &                       'in an attempt to create output directory '// &
     &                        FILHEB(1:ID-1) )
                          CALL EXIT ( 1 )
                     END IF
                   ELSE
                     IS = CLOSEDIR ( %VAL(DIR_DESC) )
                END IF                
                ID = LINDEX ( FILHEB, '/' )
                IS = MKDIR ( FILHEB(1:ID-1)//CHAR(0), %VAL(MODE_I2) )
                IF ( IS .EQ. -1 ) THEN
                     CALL GERROR ( STR )
                     CALL ERR_LOG ( 1808, -2, 'GRIB1_TO_HEB', 'Failure '// &
     &                   'in an attempt to create output directory '// &
     &                    FILHEB(1:ID-1) )
                     CALL EXIT ( 1 )
                END IF
              ELSE
                IS = CLOSEDIR ( %VAL(DIR_DESC) )
           END IF
      END IF
      IUER = -1
      CALL WRITE_HEB ( HEB, HEB%VAL, FILHEB, IUER )
      IF ( IUER .NE. 0 ) THEN
           CALL ERR_LOG ( 1809, IUER, 'GRIB1_TO_HEB', 'Error in '// &
     &         'an attempt to write output file in heb format: '//FILHEB )
           CALL EXIT ( 1 )
      END IF
!
      IF ( ILEN(COMPR_COM) .NE. 0 ) THEN
!
! ------- Now compress the output file 
!
          COM_STR = COMPR_COM(1:I_LEN(COMPR_COM))//' '// &
     &              FILHEB(1:I_LEN(FILHEB))
          CALL SYSTEM ( COM_STR(1:I_LEN(COM_STR))//CHAR(0) )
      END IF
      END  PROGRAM  GRIB1_TO_HEB  !#!  
!
! ------------------------------------------------------------------------
!
      SUBROUTINE SUM_ARR_R4 ( HEB, MODE, L_LON, L_LAT, LON_MIN, LON_MAX, &
     &                        LAT_MIN, LAT_MAX, PRES_MAX, ARR_R4 )
! ************************************************************************
! *                                                                      *
! *   Auxiliary routine SUM_ARR_R4
! *                                                                      *
! *  ### 23-APR-2013   SUM_ARR_R4  v1.0 (c)  L. Petrov  23-APR-2013 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'heb.i'
      TYPE     ( HEB__TYPE ) :: HEB
      REAL*8     REA, FE, EXC_SQ, ACC_EQU, GRV_LAT, GRV_H
      PARAMETER  ( REA     = 6378136.3D0 )       ! Earth's equatorial radius
      PARAMETER  ( FE      = 1.D0/298.257D0 )    ! Earth's flattening
      PARAMETER  ( EXC_SQ  = 2.D0*FE - FE**2 )   ! Earth's eccentricity
      PARAMETER  ( ACC_EQU = 9.7803184558D0 )    ! Equatorial gravity acc.
      PARAMETER  ( GRV_H   = -2.D0*ACC_EQU/REA ) ! D(ACC_EQU)/DH
      PARAMETER  ( GRV_LAT = 0.001931663  )      ! D(ACC_EQU)/D(phi)
!
      INTEGER*4  MODE, L_LON, L_LAT
      REAL*4     ARR_R4(L_LON,L_LAT), PRES_MAX
      REAL*8     LAT_MIN, LON_MIN, LAT_MAX, LON_MAX, &
     &           LAT_STEP, LON_STEP
      REAL*8     G_ACC, LAT, LON
      INTEGER*4  J1, J2, IND_LAT, IND_LON
!
      G_ACC = 9.81D0
      LON_STEP = (LON_MAX - LON_MIN)/(L_LON - 1)
      LAT_STEP = (LAT_MAX - LAT_MIN)/(L_LAT - 1)
      DO 410 J1=1,L_LAT
         LAT = LAT_MIN + (J1-1.5)*LAT_STEP
         IND_LAT = IDNINT( (LAT + P2I)/PI__NUM*HEB%DIMS(2) ) + 1
         G_ACC = ACC_EQU* (1.D0 + GRV_LAT* DSIN(LAT)**2 ) / &
     &           DSQRT  (1.D0 - EXC_SQ*  DSIN(LAT)**2)
         DO 420 J2=1,L_LON
            LON = LON_MIN + (J2-1.5)*LON_STEP
            IND_LON = IDNINT( LON/PI2*HEB%DIMS(1) ) + 1
            IF ( IND_LON < 1           ) IND_LON = IND_LON + HEB%DIMS(1) 
            IF ( MODE == 1 ) THEN
                 HEB%VAL(IND_LON,IND_LAT,1,1) = HEB%VAL(IND_LON,IND_LAT,1,1) + &
     &                                          ARR_R4(J2,J1)*G_ACC
               ELSE IF ( MODE == 2 .OR. MODE == 3 ) THEN
                 HEB%VAL(IND_LON,IND_LAT,1,1) = HEB%VAL(IND_LON,IND_LAT,1,1) + &
     &                                          ARR_R4(J2,J1)
            END IF
            IF ( HEB%VAL(IND_LON,IND_LAT,1,1) > PRES_MAX ) THEN
                 HEB%VAL(IND_LON,IND_LAT,1,1) = PRES_MAX
            END IF
 420     CONTINUE 
 410  CONTINUE 
      RETURN
      END  SUBROUTINE  SUM_ARR_R4  !#!#
