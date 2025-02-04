      PROGRAM    LS_REGRID
! ************************************************************************
! *                                                                      *
! *   Prgoram LS_REGRID_MAIN
! *                                                                      *
! *   $MALO_DIR/share/GHRSST_1km_water_msak.nc
! *                                                                      *
! * ### 22-DEC-2013  LS_REGRID_MAIN  v1.1 (c)  L. Petrov 13-MAY-2015 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'malo.i'
      INCLUDE   'heb.i'
      TYPE     ( HEB__TYPE ) :: HEBLS
      CHARACTER  FILIN*128, FILOUT*128, STR*128
      INTEGER*4  MDEG, IUER
      INTEGER*4, EXTERNAL :: ILEN, I_LEN
!
      IF ( IARGC() < 3 ) THEN
           WRITE ( 6, * ) 'Usage: ls_regrid filin deg filout'
           CALL EXIT ( 1 )
         ELSE 
           CALL GETARG ( 1, FILIN  )
           CALL GETARG ( 2, STR    )
           CALL CHIN   ( STR, MDEG )
           IF ( MDEG < 1 .OR. MDEG > 32768 ) THEN
                CALL ERR_LOG ( 2201, IUER, 'LS_REGRID', 'Wrong deg '// &
     &              'parameter: should be in an integeger in a range [1, 32768]')
                CALL EXIT ( 1 )
           END IF
           CALL GETARG ( 3, FILOUT )
      END IF
!
      IF ( INDEX ( FILIN, '.heb' ) - 1 == ILEN(FILIN) - ILEN('.heb') ) THEN
           IUER = -1
           CALL READ_LS_HEB_03 ( FILIN, MDEG, HEBLS, IUER )
           IF ( IUER .NE. 0 ) THEN
                CALL ERR_LOG ( 2202, IUER, 'LS_REGRID', 'Failure in reading the '// &
     &              'input file with land-sea mask '//FILIN(1:I_LEN(FILIN))// &
     &              ' that is supposed to be in heb format' )
                CALL EXIT ( 1 )
           END IF
         ELSE 
          IUER = -1
          CALL READ_LS_NC_02 ( FILIN, MDEG, HEBLS, IUER )
          IF ( IUER .NE. 0 ) THEN
                CALL ERR_LOG ( 2203, IUER, 'LS_REGRID', 'Failure in reading the '// &
     &              'input file with land-sea mask '//FILIN(1:I_LEN(FILIN))// &
     &              ' that is supposed to be in netcdf format' )
                CALL EXIT ( 1 )
          END IF
      END IF
!
      HEBLS%FILE_NAME = FILOUT
      IUER = -1
      CALL WRITE_HEB ( HEBLS, HEBLS%VAL, FILOUT, IUER )
      IF ( IUER .NE. 0 ) THEN
           CALL ERR_LOG ( 2203, IUER, 'LS_REGRID', 'Error in an attempt to '// &
     &         'write output file '//FILOUT )
           CALL EXIT ( 1 )
      END IF
!
      END  PROGRAM   LS_REGRID  !#!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE READ_LS_NC_01 ( FILIN, DEG, HEBLS, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine READ_LS_NC reads input file with land-sea mask in netCDF   *
! *   format and writes the output in the HEBLS object.                  *
! *                                                                      *
! *  ### 22-DEC-2013   READ_LS_NC  v1.0 (c)  L. Petrov  22-DEC-2013 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'heb.i'
      INCLUDE   'netcdf.inc'
      TYPE     ( HEB__TYPE ) :: HEBLS
      CHARACTER  FILIN*(*)
      INTEGER*4  DEG, IUER
      CHARACTER  STR*128
      INTEGER*4  IS, NCID,   DIMLEN_LAT, DIMLEN_LON, &
     &           ID_DIM_LON, ID_DIM_LAT, ID_DIM_LS, ID_VAR_LS, IER
      REAL*4     LON_MIN, LAT_MIN, LON_RES, LAT_RES, LAT_STEP, LON_STEP, &
     &           LAT, LON
      INTEGER*1, ALLOCATABLE :: ARR_I1(:,:)
      INTEGER*4, ALLOCATABLE :: NUM_LAND(:,:)
      INTEGER*4  J1, J2, J3, J4, ILON, ILAT
      INTEGER*4, EXTERNAL :: ILEN, I_LEN
      CHARACTER, EXTERNAL :: GET_CDATE*19
!
! --- Open the new output file in netcdf format
!
      IS = NF_OPEN ( FILIN, NF_NOWRITE, NCID )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 6271, IUER, 'READ_LS_NC', 'Error in an '// &
     &         'attempt to open the netcf file with land/sea mask '// &
     &         FILIN(1:I_LEN(FILIN))//' NF_CREATE: '//NF_STRERROR(IS) )
           RETURN
      END IF
!
! --- Learn th ID of dimension: 'longitude'
!
      IS = NF_INQ_DIMID ( NCID, 'longitude', ID_DIM_LON )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 6272, IUER, 'READ_LS_NC', 'Error in '// &
     &         ' an attempt to read dimension lon: '// &
     &          NF_STRERROR(IS) )
           RETURN
      END IF
!
! --- Learn the ID of dimension 'latitude'
!
      IS = NF_INQ_DIMID ( NCID, 'latitude', ID_DIM_LAT )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 6273, IUER, 'READ_LS_NC', 'Error in '// &
     &         ' an attempt to read dimension lat: '// &
     &          NF_STRERROR(IS) )
           RETURN
      END IF
!
! --- Get the length of dimension "Lon"
!
      IS = NF_INQ_DIMLEN ( NCID, ID_DIM_LON, DIMLEN_LON  )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 6274, IUER, 'READ_LS_NC', 'Error in getting '// &
     &         'the length of the dimension "lon" in file '// &
     &          FILIN(1:I_LEN(FILIN))//' error: '// &
     &          NF_STRERROR(IS) )
           RETURN
      END IF
!
! --- Get the length of dimension "lat"
!
      IS = NF_INQ_DIMLEN ( NCID, ID_DIM_LAT, DIMLEN_LAT  )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 6275, IUER, 'READ_LS_NC', 'Error in getting '// &
     &         'the length of the dimension "lat" in file '// &
     &          FILIN(1:I_LEN(FILIN))//' error: '// &
     &          NF_STRERROR(IS) )
           RETURN
      END IF
!
! --- Learn the ID of the variable "mask"
!
      IS = NF_INQ_VARID ( NCID, 'mask', ID_VAR_LS )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 6276, IUER, 'READ_LS_NC', 'Variable "land" '// &
     &         'was not found in the input netcdf file '// &
     &          FILIN(1:I_LEN(FILIN))//' error: '//NF_STRERROR(IS) )
           RETURN
      END IF
!
      IS = NF_GET_ATT_REAL ( NCID, 0, 'geospatial_lon_min',  LON_MIN )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 6277, IUER, 'READ_LS_NC', 'Failure '// &
     &         'in attempt to retrieve global attribute geospatial_lon_min '// &
     &         'glvariable from the file '//FILIN(1:I_LEN(FILIN))//' error: '// &
     &          NF_STRERROR(IS) )
           RETURN
      END IF
      LON_MIN = LON_MIN*DEG__TO__RAD
!
      IS = NF_GET_ATT_REAL ( NCID, 0, 'geospatial_lat_min',  LAT_MIN )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 6278, IUER, 'READ_LS_NC', 'Failure '// &
     &         'in attempt to retrieve global attribute geospatial_lat_min '// &
     &         'variable from the file '//FILIN(1:I_LEN(FILIN))//' error: '// &
     &          NF_STRERROR(IS) )
           RETURN
      END IF
      LAT_MIN = LAT_MIN*DEG__TO__RAD
!
      IS = NF_GET_ATT_REAL ( NCID, 0, 'geospatial_lon_resolution',  LON_RES )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 6279, IUER, 'READ_LS_NC', 'Failure '// &
     &         'in attempt to retrieve global attribute geospatial_lon_res '// &
     &         'variable from the file '//FILIN(1:I_LEN(FILIN))//' error: '// &
     &          NF_STRERROR(IS) )
           RETURN
      END IF
      LON_RES = LON_RES*DEG__TO__RAD
!
      IS = NF_GET_ATT_REAL ( NCID, 0, 'geospatial_lat_resolution',  LAT_RES )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 6279, IUER, 'READ_LS_NC', 'Failure '// &
     &         'in attempt to retrieve global attribute geospatial_lat_res '// &
     &         'variable from the file '//FILIN(1:I_LEN(FILIN))//' error: '// &
     &          NF_STRERROR(IS) )
           RETURN
      END IF
      LAT_RES = LAT_RES*DEG__TO__RAD
!
      IS = NF_GET_ATT_TEXT ( NCID, 0, 'references',      HEBLS%REFERENCES  )
      IS = NF_GET_ATT_TEXT ( NCID, 0, 'title',           HEBLS%TITLE       )
      IS = NF_GET_ATT_TEXT ( NCID, 0, 'institution',     HEBLS%INSTITUTION )
      IS = NF_GET_ATT_TEXT ( NCID, 0, 'source_data',     HEBLS%SOURCE      )
      IS = NF_GET_ATT_TEXT ( NCID, 0, 'product_version', HEBLS%VERSION_ID  )
      HEBLS%PROD_NAME = 'GHRSST'
      HEBLS%SDS_NAME  = 'Land/Water mask (0.0: sea or lake, 1.0: land, 0-1: mixed water/land'
      HEBLS%TITLE     = 'Land/Water mask'
!
      ALLOCATE ( ARR_I1(DIMLEN_LON,DIMLEN_LAT), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 6280, IUER, 'READ_LS_NC', 'Failure in attempt '// &
     &         'to allocate dynamic memory for temporary array ARR_I1' )
           RETURN
      END IF
!
! --- Get the variable: mask 
!
      IS = NF_GET_VAR_INT1 ( NCID, ID_VAR_LS, ARR_I1 )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 6281, IUER, 'READ_LS_NC', 'Error in getting '// &
     &         'the values of the variable "land" from the file '// &
     &          FILIN(1:I_LEN(FILIN))//' error: '//NF_STRERROR(IS) )
           RETURN
      END IF
!
! --- Close file
!
      IS = NF_CLOSE ( NCID )
!
      HEBLS%DIMS(1) = (DEG+1)*4
      HEBLS%DIMS(2) = (DEG+1)*2 + 1
      HEBLS%DIMS(3) = 1
      HEBLS%DIMS(4) = 1
!
      ALLOCATE ( HEBLS%VAL(HEBLS%DIMS(1),HEBLS%DIMS(2),HEBLS%DIMS(3),HEBLS%DIMS(4)), &
     &           STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 6282, IUER, 'READ_LS_NC', 'Failure in attempt '// &
     &         'to allocate dynamic memory for array HEBLS%VAL' )
           RETURN
      END IF
      HEBLS%VAL = 0.0
!
      ALLOCATE ( NUM_LAND(HEBLS%DIMS(1),HEBLS%DIMS(2)), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 6280, IUER, 'READ_LS_NC', 'Failure in attempt '// &
     &         'to allocate dynamic memory for temporary array NUM_LAND' )
           RETURN
      END IF
      NUM_LAND = 0
!
      LON_STEP = PI2/HEBLS%DIMS(1)
      LAT_STEP = PI__NUM/(HEBLS%DIMS(2) - 1)
!
      DO 410 J1=1,DIMLEN_LAT
         LAT = LAT_MIN + (J1-1)*LAT_RES
         ILAT = NINT ( (LAT+P2I)/LAT_STEP ) + 1
         DO 420 J2=1,DIMLEN_LON
            LON = LON_MIN + (J2-1)*LON_RES
            IF ( LON < 0.0 ) LON = LON + PI2
            ILON = NINT ( LON/LON_STEP ) + 1
!
! --------- This may happen due to rounding
!
            IF ( ILON > HEBLS%DIMS(1) ) ILON = ILON - HEBLS%DIMS(1)
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
! if ( arr_i1(j2,j1) > 0 ) then
!   write  ( 6, * ) 'j2 = ', int2(j2), ' j1= ', int2(j1), ' ari = ', ARR_I1(J2,J1), ' ill= ', int2(ilon), int2(ilat) ! %%%
!   write  ( 6, * ) 'lon= ', lon, ' lon_step = ', lon_step 
!endif 
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
            IF ( BTEST(ARR_I1(J2,J1),0) .OR. &       ! open ocean
     &           BTEST(ARR_I1(J2,J1),2) .OR. &       ! lake
     &           BTEST(ARR_I1(J2,J1),3)      ) THEN  ! sea ice
                 CONTINUE 
               ELSE 
!
! -------------- Update land counter
!
                 HEBLS%VAL(ILON,ILAT,1,1)  = HEBLS%VAL(ILON,ILAT,1,1) + 1.0
            END IF
!
! --------- Update the counter of total hits of the given HEBLS cell
!
            NUM_LAND(ILON,ILAT) = NUM_LAND(ILON,ILAT) + 1
 420     CONTINUE 
 410  CONTINUE 
!
      DO 430 J3=1,HEBLS%DIMS(2)
         DO 440 J4=1,HEBLS%DIMS(1)
            IF ( NUM_LAND(J4,J3) > 0 ) THEN
                 HEBLS%VAL(J4,J3,1,1) = HEBLS%VAL(J4,J3,1,1)/NUM_LAND(J4,J3)
               ELSE 
                 HEBLS%VAL(J4,J3,1,1) = 0.0
            END IF
            LAT = -P2I + (J3-1)*LAT_STEP
            LON =        (J4-1)*LON_STEP
            IF ( LON .GE. 135.0*DEG__TO__RAD .AND. LON .LE. 150.1*DEG__TO__RAD .AND. &
     &           LAT .LE. -68.0*DEG__TO__RAD ) THEN
                 HEBLS%VAL(J4,J3,1,1) = 1.0
            END IF
 440     CONTINUE 
 430  CONTINUE 
!
      HEBLS%DATA_FORMAT      = 'R4'
      HEBLS%DATA_TRANSFORM   = 'NONE'
      HEBLS%DATA_COMPRESSION = 'NONE'
      HEBLS%UNITS = 'dimensionless'
      HEBLS%HISTORY = 'Generatred by READ_LS_NC on '//GET_CDATE()
      HEBLS%MIN_VALUE  = 0.0
      HEBLS%MAX_VALUE  = 1.0
      HEBLS%FILL_VALUE = -1.0
      HEBLS%VALID_RANGE(1) = HEBLS%MIN_VALUE
      HEBLS%VALID_RANGE(2) = HEBLS%MAX_VALUE 
      HEBLS%MJD      = J2000__MJD
      HEBLS%UTC      = 43200.0
      HEBLS%TAI      = 43200.0
!
      DEALLOCATE ( NUM_LAND )
      DEALLOCATE ( ARR_I1   )
      CALL ERR_LOG ( 0, IUER )      
      RETURN
      END  SUBROUTINE READ_LS_NC_01  !#!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE READ_LS_NC_02 ( FILIN, DEG, HEBLS, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine READ_LS_NC reads input file with land-sea mask in netCDF   *
! *   format and writes the output in the HEBLS object.                  *
! *                                                                      *
! *  ### 22-DEC-2013   READ_LS_NC  v1.0 (c)  L. Petrov  22-DEC-2013 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'heb.i'
      INCLUDE   'netcdf.inc'
      TYPE     ( HEB__TYPE ) :: HEBLS
      CHARACTER  FILIN*(*)
      INTEGER*4  DEG, IUER
      CHARACTER  STR*128
      INTEGER*4  IS, NCID,   DIMLEN_LAT, DIMLEN_LON, &
     &           ID_DIM_LON, ID_DIM_LAT, ID_DIM_LS, ID_VAR_LS, IER
      REAL*4     LON_MIN, LAT_MIN, LON_MAX, LAT_MAX, LON_RES, LAT_RES, &
     &           LAT_STEP, LON_STEP, LAT, LON
      INTEGER*1, ALLOCATABLE :: ARR_I1(:,:)
      INTEGER*4, ALLOCATABLE :: NUM_LAND(:,:)
      INTEGER*4  J1, J2, J3, J4, ILON, ILAT
      INTEGER*4, EXTERNAL :: ILEN, I_LEN
      CHARACTER, EXTERNAL :: GET_CDATE*19
!
! --- Open the new output file in netcdf format
!
      IS = NF_OPEN ( FILIN, NF_NOWRITE, NCID )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 6271, IUER, 'READ_LS_NC', 'Error in an '// &
     &         'attempt to open the netcf file with land/sea mask '// &
     &         FILIN(1:I_LEN(FILIN))//' NF_CREATE: '//NF_STRERROR(IS) )
           RETURN
      END IF
!
! --- Learn the ID of dimension: 'lon'
!
      IS = NF_INQ_DIMID ( NCID, 'lon', ID_DIM_LON )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 6272, IUER, 'READ_LS_NC', 'Error in '// &
     &         ' an attempt to read dimension lon: '// &
     &          NF_STRERROR(IS) )
           RETURN
      END IF
!
! --- Learn the ID of dimension 'lat'
!
      IS = NF_INQ_DIMID ( NCID, 'lat', ID_DIM_LAT )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 6273, IUER, 'READ_LS_NC', 'Error in '// &
     &         ' an attempt to read dimension lat: '// &
     &          NF_STRERROR(IS) )
           RETURN
      END IF
!
! --- Get the length of dimension "Lon"
!
      IS = NF_INQ_DIMLEN ( NCID, ID_DIM_LON, DIMLEN_LON  )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 6274, IUER, 'READ_LS_NC', 'Error in getting '// &
     &         'the length of the dimension "lon" in file '// &
     &          FILIN(1:I_LEN(FILIN))//' error: '// &
     &          NF_STRERROR(IS) )
           RETURN
      END IF
!
! --- Get the length of dimension "lat"
!
      IS = NF_INQ_DIMLEN ( NCID, ID_DIM_LAT, DIMLEN_LAT  )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 6275, IUER, 'READ_LS_NC', 'Error in getting '// &
     &         'the length of the dimension "lat" in file '// &
     &          FILIN(1:I_LEN(FILIN))//' error: '// &
     &          NF_STRERROR(IS) )
           RETURN
      END IF
!
! --- Learn the ID of the variable "dst"
!
      IS = NF_INQ_VARID ( NCID, 'dst', ID_VAR_LS )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 6276, IUER, 'READ_LS_NC', 'Variable "land" '// &
     &         'was not found in the input netcdf file '// &
     &          FILIN(1:I_LEN(FILIN))//' error: '//NF_STRERROR(IS) )
           RETURN
      END IF
!
      IS = NF_GET_ATT_REAL ( NCID, 0, 'westernmost_longitude',  LON_MIN )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 6277, IUER, 'READ_LS_NC', 'Failure '// &
     &         'in attempt to retrieve global attribute geospatial_lon_min '// &
     &         'glvariable from the file '//FILIN(1:I_LEN(FILIN))//' error: '// &
     &          NF_STRERROR(IS) )
           RETURN
      END IF
      LON_MIN = LON_MIN*DEG__TO__RAD
!
      IS = NF_GET_ATT_REAL ( NCID, 0, 'easternmost_longitude',  LON_MAX )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 6277, IUER, 'READ_LS_NC', 'Failure '// &
     &         'in attempt to retrieve global attribute geospatial_lon_min '// &
     &         'glvariable from the file '//FILIN(1:I_LEN(FILIN))//' error: '// &
     &          NF_STRERROR(IS) )
           RETURN
      END IF
      LON_MAX = LON_MAX*DEG__TO__RAD
      LON_RES = (LON_MAX - LON_MIN)/(DIMLEN_LON-1)
!
      IS = NF_GET_ATT_REAL ( NCID, 0, 'southernmost_latitude',  LAT_MIN )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 6278, IUER, 'READ_LS_NC', 'Failure '// &
     &         'in attempt to retrieve global attribute geospatial_lat_min '// &
     &         'variable from the file '//FILIN(1:I_LEN(FILIN))//' error: '// &
     &          NF_STRERROR(IS) )
           RETURN
      END IF
      LAT_MIN = LAT_MIN*DEG__TO__RAD
!
      IS = NF_GET_ATT_REAL ( NCID, 0, 'northernmost_latitude',  LAT_MAX )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 6278, IUER, 'READ_LS_NC', 'Failure '// &
     &         'in attempt to retrieve global attribute geospatial_lat_min '// &
     &         'variable from the file '//FILIN(1:I_LEN(FILIN))//' error: '// &
     &          NF_STRERROR(IS) )
           RETURN
      END IF
      LAT_MAX = LAT_MAX*DEG__TO__RAD
      LAT_RES = (LAT_MAX - LAT_MIN)/(DIMLEN_LAT-1)
!
      IS = NF_GET_ATT_TEXT ( NCID, 0, 'title',         HEBLS%TITLE       )
      IS = NF_GET_ATT_TEXT ( NCID, 0, 'institution',   HEBLS%INSTITUTION )
      IS = NF_GET_ATT_TEXT ( NCID, 0, 'creation_date', HEBLS%PROD_DATE_TIME )
      IS = NF_GET_ATT_TEXT ( NCID, 0, 'creation_date', HEBLS%VERSION_ID  )
      HEBLS%REFERENCES = ' '
      HEBLS%SOURCE     = 'https://www.ghrsst.org/files/download.php?m=documents&f=NAVO-lsmask-world8-var.dist5.5.nc.bz2'
      HEBLS%PROD_NAME  = 'LWMASK'
      HEBLS%SDS_NAME   = 'Land/Water mask (0.0: sea or lake, 1.0: land, 0-1: mixed water/land'
      HEBLS%TITLE      = 'Land/Water mask'
!
      ALLOCATE ( ARR_I1(DIMLEN_LON,DIMLEN_LAT), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 6280, IUER, 'READ_LS_NC', 'Failure in attempt '// &
     &         'to allocate dynamic memory for temporary array ARR_I1' )
           RETURN
      END IF
!
! --- Get the variable: mask 
!
      IS = NF_GET_VAR_INT1 ( NCID, ID_VAR_LS, ARR_I1 )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 6281, IUER, 'READ_LS_NC', 'Error in getting '// &
     &         'the values of the variable "land" from the file '// &
     &          FILIN(1:I_LEN(FILIN))//' error: '//NF_STRERROR(IS) )
           RETURN
      END IF
!
! --- Close file
!
      IS = NF_CLOSE ( NCID )
!
      HEBLS%DIMS(1) = (DEG+1)*4
      HEBLS%DIMS(2) = (DEG+1)*2 + 1
      HEBLS%DIMS(3) = 1
      HEBLS%DIMS(4) = 1
!
      ALLOCATE ( HEBLS%VAL(HEBLS%DIMS(1),HEBLS%DIMS(2),HEBLS%DIMS(3),HEBLS%DIMS(4)), &
     &           STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 6282, IUER, 'READ_LS_NC', 'Failure in attempt '// &
     &         'to allocate dynamic memory for array HEBLS%VAL' )
           RETURN
      END IF
      HEBLS%VAL = 0.0
!
      ALLOCATE ( NUM_LAND(HEBLS%DIMS(1),HEBLS%DIMS(2)), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 6280, IUER, 'READ_LS_NC', 'Failure in attempt '// &
     &         'to allocate dynamic memory for temporary array NUM_LAND' )
           RETURN
      END IF
      NUM_LAND = 0
!
      LON_STEP = PI2/HEBLS%DIMS(1)
      LAT_STEP = PI__NUM/(HEBLS%DIMS(2) - 1)
!
      DO 410 J1=1,DIMLEN_LAT
         LAT = LAT_MAX - (J1-1)*LAT_RES
         ILAT = NINT ( (LAT+P2I)/LAT_STEP ) + 1
         DO 420 J2=1,DIMLEN_LON
            LON = LON_MIN + (J2-1)*LON_RES
            IF ( LON < 0.0 ) LON = LON + PI2
            ILON = NINT ( LON/LON_STEP ) + 1
!
! --------- This may happen due to rounding
!
            IF ( ILON > HEBLS%DIMS(1) ) ILON = ILON - HEBLS%DIMS(1)
            IF ( ARR_I1(J2,J1) > 4 .OR. ARR_I1(J2,J1) == -1 ) THEN
                 CONTINUE 
               ELSE 
!
! -------------- Update land counter
!
                 HEBLS%VAL(ILON,ILAT,1,1)  = HEBLS%VAL(ILON,ILAT,1,1) + 1.0
            END IF
!
! --------- Update the counter of total hits of the given HEBLS cell
!
            NUM_LAND(ILON,ILAT) = NUM_LAND(ILON,ILAT) + 1
 420     CONTINUE 
 410  CONTINUE 
!
      DO 430 J3=1,HEBLS%DIMS(2)
         DO 440 J4=1,HEBLS%DIMS(1)
            IF ( NUM_LAND(J4,J3) > 0 ) THEN
                 HEBLS%VAL(J4,J3,1,1) = HEBLS%VAL(J4,J3,1,1)/NUM_LAND(J4,J3)
               ELSE 
                 HEBLS%VAL(J4,J3,1,1) = 0.0
            END IF
 440     CONTINUE 
 430  CONTINUE 
!
      HEBLS%DATA_FORMAT      = 'R4'
      HEBLS%DATA_TRANSFORM   = 'NONE'
      HEBLS%DATA_COMPRESSION = 'NONE'
      HEBLS%UNITS = 'dimensionless'
      HEBLS%HISTORY = 'Generatred by READ_LS_NC on '//GET_CDATE()
      HEBLS%MIN_VALUE  = 0.0
      HEBLS%MAX_VALUE  = 1.0
      HEBLS%FILL_VALUE = -1.0
      HEBLS%VALID_RANGE(1) = HEBLS%MIN_VALUE
      HEBLS%VALID_RANGE(2) = HEBLS%MAX_VALUE 
      HEBLS%MJD      = J2000__MJD
      HEBLS%UTC      = 43200.0
      HEBLS%TAI      = 43200.0
!
      DEALLOCATE ( NUM_LAND )
      DEALLOCATE ( ARR_I1   )
      CALL ERR_LOG ( 0, IUER )      
      RETURN
      END  SUBROUTINE READ_LS_NC_02  !#!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE READ_LS_HEB_03 ( FILIN, DEG, HEBLS, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine READ_LS_HEB_03 reads input file with land-sea mask in netCDF   *
! *   format and writes the output in the HEBLS object.                  *
! *                                                                      *
! *  ### 13-MAY-2015 READ_LS_HEB_03 v1.0 (c)  L. Petrov  13-MAY-2015 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'heb.i'
      INCLUDE   'netcdf.inc'
      TYPE     ( HEB__TYPE ) :: HEBIN
      TYPE     ( HEB__TYPE ) :: HEBLS
      CHARACTER  FILIN*(*)
      INTEGER*4  DEG, IUER
      CHARACTER  STR*128
      INTEGER*4  IS, NCID,   DIMLEN_LAT, DIMLEN_LON, &
     &           ID_DIM_LON, ID_DIM_LAT, ID_DIM_LS, ID_VAR_LS, IER
      REAL*4     LON_MIN, LAT_MIN, LON_MAX, LAT_MAX, LON_RES, LAT_RES, &
     &           LAT_STEP, LON_STEP, LAT, LON
      INTEGER*1, ALLOCATABLE :: ARR_I1(:,:)
      INTEGER*4, ALLOCATABLE :: NUM_CELL(:,:)
      INTEGER*4  J1, J2, J3, J4, ILON, ILAT
      INTEGER*4, EXTERNAL :: ILEN, I_LEN
      CHARACTER, EXTERNAL :: GET_CDATE*19
!
      CALL ERR_PASS ( IUER, IER )
      CALL READ_HEB ( FILIN, HEBIN, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 6279, IUER, 'READ_LS_HEB_03', 'Failure in attempt '// &
     &         'to allocate dynamic memory for temporary array NUM_CELL' )
           RETURN
      END IF
!
      HEBLS = HEBIN
      HEBLS%DIMS(1) = (DEG+1)*4
      HEBLS%DIMS(2) = (DEG+1)*2 + 1
      HEBLS%DIMS(3) = 1
      HEBLS%DIMS(4) = 1
!
      ALLOCATE ( HEBLS%VAL(HEBLS%DIMS(1),HEBLS%DIMS(2),HEBLS%DIMS(3),HEBLS%DIMS(4)), &
     &           STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 6282, IUER, 'READ_LS_NC', 'Failure in attempt '// &
     &         'to allocate dynamic memory for array HEBLS%VAL' )
           RETURN
      END IF
      HEBLS%VAL = 0.0
!
      ALLOCATE ( NUM_CELL(HEBLS%DIMS(1),HEBLS%DIMS(2)), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 6280, IUER, 'READ_LS_HEB_03', 'Failure in attempt '// &
     &         'to allocate dynamic memory for temporary array NUM_CELL' )
           RETURN
      END IF
      NUM_CELL = 0
!
      LAT_MIN = -P2I
      LAT_MAX =  P2I
      LAT_RES = (LAT_MAX - LAT_MIN)/(HEBIN%DIMS(2)-1)
      LAT_STEP = PI__NUM/(HEBLS%DIMS(2) - 1)
      LON_MIN = 0.0D0
      LON_MAX = PI2
      LON_RES = (LON_MAX - LON_MIN)/(HEBIN%DIMS(1)-1)
      LON_STEP = PI2/HEBLS%DIMS(1)
!
      DO 410 J1=1,HEBIN%DIMS(2)
         LAT = LAT_MIN + (J1-1)*LAT_RES
         ILAT = NINT ( (LAT+P2I)/LAT_STEP ) + 1
         DO 420 J2=1,HEBIN%DIMS(1)
            LON = LON_MIN + (J2-1)*LON_RES
            ILON = NINT ( LON/LON_STEP ) + 1
!
! --------- This may happen due to rounding
!
            IF ( ILON > HEBLS%DIMS(1) ) ILON = ILON - HEBLS%DIMS(1)
            HEBLS%VAL(ILON,ILAT,1,1)  = HEBLS%VAL(ILON,ILAT,1,1) + HEBIN%VAL(J2,J1,1,1)
!
! --------- Update the counter of total hits of the given HEBLS cell
!
            NUM_CELL(ILON,ILAT) = NUM_CELL(ILON,ILAT) + 1
 420     CONTINUE 
 410  CONTINUE 
!
      DO 430 J3=1,HEBLS%DIMS(2)
         DO 440 J4=1,HEBLS%DIMS(1)
            IF ( NUM_CELL(J4,J3) > 0 ) THEN
                 HEBLS%VAL(J4,J3,1,1) = HEBLS%VAL(J4,J3,1,1)/NUM_CELL(J4,J3)
               ELSE 
                 HEBLS%VAL(J4,J3,1,1) = 0.0
            END IF
 440     CONTINUE 
 430  CONTINUE 
!
      HEBLS%DATA_FORMAT      = 'R4'
      HEBLS%DATA_TRANSFORM   = 'NONE'
      HEBLS%DATA_COMPRESSION = 'NONE'
      HEBLS%UNITS = 'dimensionless'
      HEBLS%HISTORY = 'Generatred by READ_LS_NC on '//GET_CDATE()
      HEBLS%MIN_VALUE  = 0.0
      HEBLS%MAX_VALUE  = 1.0
      HEBLS%FILL_VALUE = -1.0
      HEBLS%VALID_RANGE(1) = HEBLS%MIN_VALUE
      HEBLS%VALID_RANGE(2) = HEBLS%MAX_VALUE 
      HEBLS%MJD      = J2000__MJD
      HEBLS%UTC      = 43200.0
      HEBLS%TAI      = 43200.0
      HEBLS%COMMENT(1) = ' '
      HEBLS%COMMENT(2) = ' '
      HEBLS%COMMENT(3) = ' '
      HEBLS%COMMENT(4) = ' '
!
      DEALLOCATE ( NUM_CELL )
      CALL ERR_LOG ( 0, IUER )      
      RETURN
      END  SUBROUTINE  READ_LS_HEB_03 !#!#
