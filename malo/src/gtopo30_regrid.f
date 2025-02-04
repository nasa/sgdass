      PROGRAM    GTOPO30_REGRID
! ************************************************************************
! *                                                                      *
! *   Program GTOPO30_REGRID
! *                                                                      *
! * ### 05-OCT-2012  GTOPO30_REGRID  v2.1 (c) L. Petrov  09-JUN-2019 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE    'astro_constants.i'
      INCLUDE    'heb.i'
      TYPE      ( HEB__TYPE  ) :: DEL, GEOID_BSPL_HEB
      INTEGER*4    MLON, MLAT
      PARAMETER  ( MLON = 43200 )
      PARAMETER  ( MLAT = 21600 )
      INTEGER*2  HEI_ARR(MLON,MLAT)
      CHARACTER  FILGTOPO*128, FIL_GEOID_BSPL*128, FILOUT*128, &
     &           STR*128, DTYP*8, CELL_DEF*12
      REAL*4,    ALLOCATABLE :: HEI_OUT(:,:)
      REAL*8     LAT_GDT, LON
      INTEGER*4  J1, J2, J3, J4, J5, J6, J7, J8, DIM, LUN
      INTEGER*4  IDEV, IPAL, ISCL, IPRC, NLON, NLAT, IUER
      REAL*4     VAL_MIN, VAL_MAX
      CHARACTER  TITLE*128, FILPLO*128, GRID_TYPE*8
      LOGICAL*1  FL_PLOT, FL_UNITS_DEG
      REAL*4     ADD_OFFSET_R4, SCALE_R4
      INTEGER*8  IR
      INTEGER*8, EXTERNAL :: READ, WRITE
      INTEGER*4, EXTERNAL :: ILEN, I_LEN, GET_FILE_FROM_DIR 
      REAL*8,    EXTERNAL :: GET_GEOID
!
      FILGTOPO = '/g1/gtopo30/gtopo30.dat'
      FIL_GEOID_BSPL = '/progs/malo_20150601/share/EGM2008_geoid_height_bspl_d1023.heb'
      DIM      = 1023
      DTYP     = 'dig_elev'
      FILOUT   = '/d2/gtopo30/dig_evel_d1023.nc'
      FL_PLOT  = .TRUE.
!!      CELL_DEF = 'southeast'
      CELL_DEF = 'center'
!
! $MALO_DIR/bin/gtopo30_regrid /s0/gtopo30/gtopo30.dat  d8191 fls_mask $MALO_DIR/share/EGM2008_geoid_height_bspl_d1023.heb $MALO_DIR/share/fls_mask_d8191.heb plot
! $MALO_DIR/bin/gtopo30_regrid /s0/gtopo30/gtopo30.dat  d2699 dig_elev $MALO_DIR/share/EGM2008_geoid_height_bspl_d1023.heb $MALO_DIR/share/gtopo30_dig_elev_above_geoid_d2699.heb plot
! $MALO_DIR/bin/gtopo30_regrid /g1/gtopo30/gtopo30.dat d10799 dig_elev $MALO_DIR/share/EGM2008_geoid_height_bspl_d1023.heb $MALO_DIR/share/gtopo30_dig_elev_above_ellipsoid_d10799.heb plot
!
      IF ( IARGC() < 4 ) THEN
           WRITE ( 6, '(A)' ) 'Usage: gtopo30_regrid topo_file dim data_type fil_geoid_bspl output_file [plot/noplot]'
           CALL EXIT ( 1 )
         ELSE
           CALL GETARG ( 1, FILGTOPO )
           CALL GETARG ( 2, STR      )
           IF ( STR == 'merra' ) THEN
                GRID_TYPE = 'merra'
              ELSE IF ( str == 'geos57' ) THEN
                GRID_TYPE = 'geos57'
              ELSE 
                GRID_TYPE = 'd'
                CALL CHIN   ( STR(2:), DIM )
                IF ( DIM < 1 .OR. DIM > 10799 ) THEN
                     IUER = -2
                     CALL ERR_LOG ( 4201, IUER, 'GTOPO30_REGRID', 'Unsupported '// &
     &                   'value '//STR(1:I_LEN(STR))//' of the 2nd argument: '// &
     &                   'an integer in range [2, 10799] was expected' )
                     CALL EXIT ( 1 )
                END IF
                WRITE ( 6, * ) 'DIM= ', DIM
           END IF
           CALL GETARG ( 3, DTYP     )
           IF ( DTYP == 'bls_mask' ) THEN
                CONTINUE 
              ELSE IF ( DTYP == 'tls_mask' ) THEN
                CONTINUE 
              ELSE IF ( DTYP == 'fls_mask' ) THEN
                CONTINUE 
              ELSE IF ( DTYP == 'dig_elev' ) THEN
                CONTINUE 
              ELSE IF ( DTYP == 'dig_elel' ) THEN
                CONTINUE 
              ELSE IF ( DTYP == 'el_area' ) THEN
                CONTINUE 
              ELSE IF ( DTYP == 'coast' ) THEN
                CONTINUE 
              ELSE 
                IUER = -2
                CALL ERR_LOG ( 4202, IUER, 'GTOPO30_REGRID', 'Unsupported '// &
     &              'value '//DTYP(1:I_LEN(DTYP))//' of the 3rd argument: '// &
     &              'one of bls_mask, tls_mask, fls_mask, dig_elev, dig_elel, '// &
     &              'el_area, coast were expected' )
                CALL EXIT ( 1 )
           END IF
!
           CALL GETARG ( 4, FIL_GEOID_BSPL )
           CALL GETARG ( 5, FILOUT   )
           IF ( IARGC() .GE. 6 ) THEN
                CALL GETARG ( 6, STR )
              ELSE 
                STR = 'noplot'
           END IF
           IF ( STR == 'plot' ) THEN
                FL_PLOT = .TRUE.
              ELSE IF ( STR == 'noplot' ) THEN
                FL_PLOT = .FALSE.
              ELSE 
                IUER = -2
                CALL ERR_LOG ( 4203, IUER, 'GTOPO30_REGRID', 'Unsupported '// &
     &              'value '//STR(1:I_LEN(STR))//' of the 5th argument: '// &
     &              'one of plot or noplot were expected' )
                CALL EXIT ( 1 )
           END IF
      END IF 
      IF ( GRID_TYPE == 'd' ) THEN
           NLON     = 4*(DIM+1)
           NLAT     = 2*(DIM+1)+1
         ELSE IF ( GRID_TYPE == 'merra' ) THEN
           NLON     = 540
           NLAT     = 361
         ELSE IF ( GRID_TYPE == 'geos57' ) THEN
           NLON     = 1152
           NLAT     = 721
      END IF
!
      ALLOCATE ( HEI_OUT(NLON,NLAT), STAT=IUER )
      IF ( IUER .NE. 0 ) THEN
           CALL ERR_LOG ( 4204, -2, 'GTOPO30_REGRID', 'Failure in '// &
     &         'an attempt to allocate '//STR(1:I_LEN(STR))//' bytes '// &
     &         'dynamic memory for the elevation field' )
           CALL EXIT ( 1 )
      END IF
!
      IUER = -1
      CALL BINF_OPEN ( FILGTOPO, 'OLD', LUN, IUER )
      IF ( IUER .NE. 0 ) THEN
           CALL CLRCH  ( STR )
           CALL GERROR ( STR )
           CALL ERR_LOG ( 4205, -2, 'GTOPO30_REGRID', 'Failure in '// &
     &         'an attempt to open the input file '// &
     &          FILGTOPO(1:I_LEN(FILGTOPO))//' -- '//STR )
           CALL EXIT ( 1 )
      END IF
!
      IR = READ ( %VAL(LUN), HEI_ARR, %VAL(2*MLON*MLAT) )
      IF ( IR .NE. 2*MLON*MLAT ) THEN
           CALL CLRCH  ( STR )
           CALL GERROR ( STR )
           CALL ERR_LOG ( 4206, -2, 'GTOPO30_REGRID', 'Failure in '// &
     &         'an attempt to read contents of data file from '//FILGTOPO )
           CALL EXIT ( 1 )
      END IF
!
      IUER = -1
      CALL BINF_CLOSE ( LUN, IUER )
!
      IUER = -1
      CALL GRID_HEI ( DTYP, CELL_DEF, MLON, MLAT, HEI_ARR, NLON, NLAT, HEI_OUT, IUER )
      IF ( IUER .NE. 0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 4207, IUER, 'GTOPO30_REGRID', 'Failure in '// &
     &         'an attempt to perform elevation data regridding' )
           CALL EXIT ( 1 )
      END IF
!
      IF ( DTYP == 'dig_elel' ) THEN
           DO 410 J1=1,NLAT
              LAT_GDT = -P2I + (J1-1)*PI__NUM/NLAT
              DO 420 J2=1,NLON
                 LON = (J2-1)*PI2/NLON
                 IUER  = -1
                 HEI_OUT(J2,J1) = HEI_OUT(J2,J1) + &
     &                            GET_GEOID ( LAT_GDT, LON, FIL_GEOID_BSPL, &
     &                                        GEOID_BSPL_HEB, IUER )
                 IF ( IUER  .NE. 0 ) THEN
                      CALL GERROR  ( STR )
                      IUER = -1
                      CALL ERR_LOG ( 4608, IUER, 'GTOPO30_REGRID', 'Error '// &
     &                   'in compting geoid height' )
                      CALL EXIT ( 1 )
                 END IF
 420          CONTINUE 
 410       CONTINUE 
      END IF
!
      IDEV = 1
      IPRC = 1
      IF ( DTYP == 'bls_mask' ) THEN
           IPAL = 1
           ISCL = 1
           TITLE = 'Land/sea mask (0/1)'
           ADD_OFFSET_R4 = 0.0
           SCALE_R4      = 1.0
        ELSE IF ( DTYP == 'tls_mask' ) THEN
           IPAL = 1
           ISCL = 3
           TITLE = 'Land/sea mask (0/1/2)'
           ADD_OFFSET_R4 = 0.0
           SCALE_R4      = 1.0
        ELSE IF ( DTYP == 'fls_mask' ) THEN
           IPAL = 1
           ISCL = 1
           TITLE = 'Land/sea mask [0.0,1.0] -- a fraction of land'
           ADD_OFFSET_R4 = 0.0
           SCALE_R4      = 0.01
        ELSE IF ( DTYP == 'dig_elev' ) THEN
           IPAL = 7
           ISCL = 14
           TITLE = 'Digital elevation above the geoid' 
           ADD_OFFSET_R4 = 0.0
           SCALE_R4      = 1.0
        ELSE IF ( DTYP == 'dig_elel' ) THEN
           IPAL = 7
           ISCL = 14
           TITLE = 'Digital elevation above the ellipsoid' 
           ADD_OFFSET_R4 = 0.0
           SCALE_R4      = 1.0
        ELSE IF ( DTYP == 'el_area' ) THEN
           IPAL = 7
           ISCL = 14
           TITLE = 'Digital elevation averaged over the pixel area' 
           ADD_OFFSET_R4 = 0.0
           SCALE_R4      = 1.0
        ELSE IF ( DTYP == 'coast' ) THEN
           IPAL = 1
           ISCL = 1
           TITLE = 'Coast line'
           ADD_OFFSET_R4 = 0.0
           SCALE_R4      = 1.0
      END IF
      FILPLO = '/tmp/boo'
!
      IF ( INDEX ( FILOUT, '.heb' ) == ILEN(FILOUT)-3 ) THEN
           IUER = -1
           DEL%SDS_NAME    = 'Digital elevation wrt WGS84 geoid averaged over pixel areas'
           DEL%UNITS       = 'm'
           DEL%PROD_NAME   = 'G3TOPO'
           DEL%FILE_NAME   = FILGTOPO 
           DEL%HISTORY     = 'Downscaled by program gtoto30_regrid'
           DEL%SOURCE      = 'a global digital elevation model (DEM)'
           DEL%TITLE       = 'Digital elevation averaged over the pixel area, center cell' 
           DEL%INSTITUTION = 'U.S. Geological Survey'
           DEL%REFERENCES  = 'http://webmap.ornl.gov/wcsdown/dataset.jsp?ds_id=10003'
           DEL%PROD_DATE_TIME = 'n/a'
           DEL%VERSION_ID     = '3'
           DEL%MJD            = 51544       
           DEL%UTC            = 0.0D0
           DEL%TAI            = 0.0D0
           DEL%DIMS(1)        = NLON
           DEL%DIMS(2)        = NLAT
           DEL%DIMS(3)        = 1
           DEL%DIMS(4)        = 1
           DEL%DATA_FORMAT      = HEB__R4
           DEL%DATA_TRANSFORM   = HEB__NONE 
           DEL%DATA_COMPRESSION = HEB__NONE 
           DEL%MIN_VALUE        = MINVAL ( HEI_OUT )
           DEL%MAX_VALUE        = MAXVAL ( HEI_OUT )
           DEL%VALID_RANGE(1)   = -500.0
           DEL%VALID_RANGE(2)   = 9000.0
           DEL%FILL_VALUE       = -9999.0
           DEL%OFFSET           = 0.0
           DEL%SCALE_FACTOR     = 1.0
           CALL WRITE_HEB ( DEL, HEI_OUT, FILOUT, IUER )
           IF ( IUER .NE. 0 ) CALL EXIT ( 1 )
           WRITE ( 6, '(A)' ) 'Output file: '//FILOUT(1:I_LEN(FILOUT))
         ELSE IF ( INDEX ( FILOUT, '.nc' ) == ILEN(FILOUT)-2 ) THEN
           IUER = -1
           CALL WRITE_LS_MASK_NC ( DTYP, NLON, NLAT, HEI_OUT, ADD_OFFSET_R4, &
     &                             SCALE_R4, FILOUT, FL_UNITS_DEG, IUER ) 
           IF ( IUER .NE. 0 ) CALL EXIT ( 1 )
           WRITE ( 6, '(A)' ) 'Output file: '//FILOUT(1:I_LEN(FILOUT))
         ELSE IF ( INDEX ( FILOUT, '.pgm' ) == ILEN(FILOUT)-3 ) THEN
           IUER = -1
           CALL WRITE_LS_MASK_PGM ( DTYP, NLON, NLAT, HEI_OUT, &
     &                             FILOUT, IUER ) 
           IF ( IUER .NE. 0 ) CALL EXIT ( 1 )
           WRITE ( 6, '(A)' ) 'Output file: '//FILOUT(1:I_LEN(FILOUT))
      END IF
!
      IF ( FL_PLOT ) THEN
           IUER = -1
           VAL_MIN = MINVAL ( HEI_OUT )
           VAL_MAX = MAXVAL ( HEI_OUT )
           CALL PLOT_GRID_R4 ( IDEV, IPAL, ISCL, IPRC, NLON, NLAT, HEI_OUT, &
     &                         TITLE, 'm', VAL_MIN, VAL_MAX, FILPLO, IUER )
           IF ( IUER .NE. 0 ) CALL EXIT ( 1 )
      END IF 
!
      END  PROGRAM  GTOPO30_REGRID  !#!  
!
! ------------------------------------------------------------------------
!
      SUBROUTINE GRID_HEI ( DTYP, CELL_DEF, MLON, MLAT, HEI_ARR, NLON, NLAT, &
     &                      HEI_OUT, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine GRID_HEI
! *                                                                      *
! *  ### 05-OCT-2012    GRID_HEI   v2.0 (c)  L. Petrov  13-JUN-2014 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INTEGER*4  MLON, MLAT, NLON, NLAT, IUER
      CHARACTER  DTYP*(*), CELL_DEF*(*)
      INTEGER*2  HEI_ARR(MLON,MLAT)
      REAL*4     HEI_OUT(NLON,NLAT)
      REAL*4     LAT_STEP, LON_STEP, LAT_MAX_CELL, LAT_MIN_CELL, &
     &           LON_MIN_CELL, LON_MAX_CELL, LAT_STEP_M, LON_STEP_M, &
     &           LS_SUM 
      INTEGER*1, ALLOCATABLE :: COAST_I1(:,:)
      CHARACTER  STR*128
      INTEGER*4  J1, J2, J3, J4, J5, J6, J7, J8, IND_LAT_MIN, IND_LAT_MAX, &
     &           IND_LON_MIN, IND_LON_MAX, ILON, ILAT, NP, KP, IER, &
     &           IND_LAT_PREV, IND_LAT_HERE, IND_LAT_NEXT, &
     &           IND_LON_PREV, IND_LON_HERE, IND_LON_NEXT
      INTEGER*2  IVAL_I2
      INTEGER*4, EXTERNAL :: ILEN, I_LEN
!
      LAT_STEP = PI__NUM/(NLAT-1)
      LON_STEP = PI2/NLON
!
      LAT_STEP_M = PI__NUM/MLAT
      LON_STEP_M = PI2/MLON
!
      DO 410 J1=1,NLAT
         IF ( CELL_DEF == 'southeast' ) THEN
              LAT_MAX_CELL = P2I - (J1-2)*LAT_STEP
              LAT_MIN_CELL = P2I - (J1-1)*LAT_STEP
            ELSE IF ( CELL_DEF == 'center' ) THEN
              LAT_MAX_CELL = P2I - (J1-1.5)*LAT_STEP
              LAT_MIN_CELL = P2I - (J1-0.5)*LAT_STEP
         END IF
         IND_LAT_MAX = NINT ( (P2I - LAT_MAX_CELL)/LAT_STEP_M ) + 1
         IND_LAT_MIN = NINT ( (P2I - LAT_MIN_CELL)/LAT_STEP_M ) + 1
         DO 420 J2=1,NLON
            IF ( CELL_DEF == 'southeast' ) THEN
                 LON_MIN_CELL = (J2-1)*LON_STEP
                 LON_MAX_CELL =  J2*LON_STEP
               ELSE IF ( CELL_DEF == 'center' ) THEN
                 LON_MIN_CELL = (J2-1.5)*LON_STEP
                 LON_MAX_CELL = (J2-0.5)*LON_STEP
            END IF
            IND_LON_MIN = NINT ( LON_MIN_CELL/LON_STEP_M ) + 1
            IND_LON_MAX = NINT ( LON_MAX_CELL/LON_STEP_M ) + 1
            HEI_OUT(J2,J1) = 0.0
            IF ( DTYP == 'dig_elev' .OR. DTYP == 'dig_elel' ) THEN
                 NP = 0
                 DO 430 J3=IND_LAT_MAX,IND_LAT_MIN
                    IF ( J3 < 1 ) THEN
                         ILAT = 1 - J3
                       ELSE IF ( J3 > MLAT ) THEN
                         ILAT = 2*MLAT - ILAT
                       ELSE 
                         ILAT = J3
                    END IF
                    DO 440 J4=IND_LON_MIN,IND_LON_MAX
                       IF ( J4 < 1 ) THEN
                             ILON = J4 + MLON
                           ELSE IF ( J4 > MLON ) THEN
                             ILON = J4 - MLON
                           ELSE 
                             ILON = J4
                       END IF
                       IF ( HEI_ARR(ILON,ILAT) < -500 ) HEI_ARR(ILON,ILAT) = 0
                       HEI_OUT(J2,J1) = HEI_OUT(J2,J1) + HEI_ARR(ILON,ILAT)
                       NP = NP + 1
 440                CONTINUE 
 430             CONTINUE 
                 HEI_OUT(J2,J1) = HEI_OUT(J2,J1)/NP
               ELSE IF ( DTYP == 'fls_mask' .OR. DTYP == 'bls_mask' .OR. &
     &                   DTYP == 'tls_mask' .OR. DTYP == 'coast'         ) THEN
                 NP = 0
                 DO 450 J5=IND_LAT_MAX,IND_LAT_MIN
                    IF ( J5 < 1 ) THEN
                         ILAT = 1 - J5
                       ELSE IF ( J5 > MLAT ) THEN
                         ILAT = 2*MLAT - ILAT
                       ELSE 
                         ILAT = J5
                    END IF
                    DO 460 J6=IND_LON_MIN,IND_LON_MAX
                       IF ( J6 < 1 ) THEN
                             ILON = J6 + MLON
                           ELSE IF ( J6 > MLON ) THEN
                             ILON = J6 - MLON
                           ELSE 
                             ILON = J6
                       END IF
                       IF ( HEI_ARR(ILON,ILAT) > -500 ) THEN
                            HEI_OUT(J2,J1) = HEI_OUT(J2,J1) + 1.0
                       END IF
                       NP = NP + 1
 460                CONTINUE 
 450             CONTINUE 
                 HEI_OUT(J2,J1) = HEI_OUT(J2,J1)/NP
                 IF ( DTYP == 'tls_mask' ) THEN
                      IF ( HEI_OUT(J2,J1) > 0.0 .AND. HEI_OUT(J2,J1) < 1.0 ) THEN
                           HEI_OUT(J2,J1) = 2.0
                      END IF 
                    ELSE IF ( DTYP == 'bls_mask' ) THEN
                      IF ( HEI_OUT(J2,J1) > 0.5 ) THEN
                           HEI_OUT(J2,J1) = 1.0
                         ELSE 
                           HEI_OUT(J2,J1) = 0.0
                      END IF 
                 END IF
            END IF
 420     CONTINUE 
 410  CONTINUE 
!
      IF ( DTYP == 'coast' ) THEN 
           ALLOCATE ( COAST_I1(NLON,NLAT), STAT=IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 4211, IUER, 'GTOPO30_REGRID', 'Failure to '// &
     &              'allocate dynamic memory for temporary array COAST_I1' )
                RETURN 
           END IF
!
           DO 470 J7=1,NLAT
              IND_LAT_PREV = J7-1
              IND_LAT_HERE = J7
              IND_LAT_NEXT = J7+1
              IF ( IND_LAT_PREV < 1    ) IND_LAT_PREV = 2
              IF ( IND_LAT_NEXT > NLAT ) IND_LAT_NEXT = NLAT - 1
              DO 480 J8=1,NLON
                 IND_LON_PREV = J8-1
                 IND_LON_HERE = J8
                 IND_LON_NEXT = J8+1
                 IF ( IND_LON_PREV < 1    ) IND_LON_PREV = NLON
                 IF ( IND_LON_NEXT > NLON ) IND_LON_NEXT = 1
                 COAST_I1(J8,J7) = 0
                 IF ( HEI_OUT(IND_LON_HERE,IND_LAT_HERE) < 0.05 ) THEN
                      LS_SUM = HEI_OUT(IND_LON_PREV,IND_LAT_HERE) + &
     &                         HEI_OUT(IND_LON_HERE,IND_LAT_PREV) + &
     &                         HEI_OUT(IND_LON_HERE,IND_LAT_NEXT) + &
     &                         HEI_OUT(IND_LON_NEXT,IND_LAT_HERE)
                      IF ( LS_SUM > 0.01 ) THEN
                           COAST_I1(J8,J7) = 1
                      END IF
                 END IF
 480          CONTINUE 
 470       CONTINUE 
           HEI_OUT(1:NLON,1:NLAT) = COAST_I1(1:NLON,1:NLAT)
           DEALLOCATE ( COAST_I1 )
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE GRID_HEI  !#!  
