      SUBROUTINE WRITE_LS_MASK_NC ( DTYP, NLON, NLAT, HEI_ARR, &
     &                              ADD_OFFSET_R4, SCALE_R4, FILOUT, &
     &                              FL_UNITS_DEG, IUER ) 
! ************************************************************************
! *                                                                      *
! *   Routine WRITE_LS_MASK_NC
! *                                                                      *
! *  ### 18-NOV-2002 WRITE_LS_MASK_NC v3.0 (c) L. Petrov 13-NOV-2012 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'astro_constants.i'
      INCLUDE   'netcdf.inc'
      INTEGER*4  NLON, NLAT, IUER
      REAL*4     HEI_ARR(NLON,NLAT), ADD_OFFSET_R4, SCALE_R4
      CHARACTER  DTYP*(*), FILOUT*(*), STR*256
      REAL*4     RANGE_R4(2), ARR_LAT(NLAT), ARR_LON(NLON)
      INTEGER*1  RANGE_I1(2)
      INTEGER*1, ALLOCATABLE :: HEI_ARR_I1(:,:)
      INTEGER*4  IS, NCID,   VECDIM(2), &
     &           ID_DIM_LON, ID_DIM_LAT, ID_DIM_LS, &
     &           ID_VAR_LS,  ID_VAR_LAT, ID_VAR_LON
      LOGICAL*1  FL_UNITS_DEG
      INTEGER*4  J1, J2, J3, IER
      INTEGER*4, EXTERNAL :: I_LEN, ILEN
!
      VECDIM(1) = 1
      VECDIM(2) = 2
!
! --- Open the new ouput file in netcdf format
!
      IS = NF_CREATE ( FILOUT, 0, NCID )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 6211, IUER, 'WRITE_LS_MASK_NC', 'Error in an '// &
     &         'attempt to create the output netcf file '// &
     &         FILOUT(1:I_LEN(FILOUT))//' NF_CREATE: '//NF_STRERROR(IS) )
           RETURN
      END IF
!
! --- Declare three dimensions: lon, lat, tim
!
      IS = NF_DEF_DIM ( NCID, 'lon', NLON, ID_DIM_LON )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 6212, IUER, 'WRITE_LS_MASK_NC', 'Error in '// &
     &         ' an attempt to create new dimension lon: '// &
     &          NF_STRERROR(IS) )
           RETURN
      END IF
!
      IS = NF_DEF_DIM ( NCID, 'lat', NLAT, ID_DIM_LAT )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 6213, IUER, 'WRITE_LS_MASK_NC', 'Error in '// &
     &         ' an attempt to create new dimension lat: '// &
     &          NF_STRERROR(IS) )
           RETURN
      END IF
!
      IS = NF_DEF_VAR ( NCID, 'lon', NF_REAL, 1, VECDIM(1), ID_VAR_LON )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 6214, IUER, 'WRITE_LS_MASK_NC', 'Error in '// &
     &         ' an attempt to create new variable lon: '// &
     &          NF_STRERROR(IS) )
           RETURN
      END IF
!
      IS = NF_DEF_VAR ( NCID, 'lat', NF_REAL, 1, VECDIM(2), ID_VAR_LAT )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 6215, IUER, 'WRITE_LS_MASK_NC', 'Error in '// &
     &         ' an attempt to create new variable lat: '// &
     &         NF_STRERROR(IS) )
           RETURN
      END IF
!
      IF ( DTYP == 'dig_elev' ) THEN
           IS = NF_DEF_VAR ( NCID, 'land', NF_REAL, 2, VECDIM, ID_VAR_LS )
         ELSE 
           IS = NF_DEF_VAR ( NCID, 'land', NF_INT1, 2, VECDIM, ID_VAR_LS )
      END IF
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 6216, IUER, 'WRITE_LS_MASK_NC', 'Error in '// &
     &         ' an attempt to create new variable land_sea'// &
     &          NF_STRERROR(IS) )
           RETURN
      END IF
!
      IS = NF_PUT_ATT_TEXT ( NCID, ID_VAR_LAT, 'long_name', &
     &                       LEN('Latitude'), 'Latitude' )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 6217, IUER, 'WRITE_LS_MASK_NC', 'Error in '// &
     &         ' an attempt to create new attribute: '// &
     &         NF_STRERROR(IS) )
           RETURN
      END IF
!
      IF ( FL_UNITS_DEG ) THEN
           IS = NF_PUT_ATT_TEXT ( NCID, ID_VAR_LAT, 'units', &
     &                            LEN('degrees_north'), 'degrees_north' )
         ELSE 
           IS = NF_PUT_ATT_TEXT ( NCID, ID_VAR_LAT, 'units', &
     &                            LEN('radian'), 'radian' )
      END IF
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 6218, IUER, 'WRITE_LS_MASK_NC', 'Error in '// &
     &         ' an attempt to create new attribute: '// &
     &          NF_STRERROR(IS) )
           RETURN
      END IF
!
      RANGE_R4(1) = -P2I
      RANGE_R4(2) =  P2I - PI__NUM/NLAT
      IF ( FL_UNITS_DEG ) THEN
           RANGE_R4 = RANGE_R4/DEG__TO__RAD
      END IF
!
      IS = NF_PUT_ATT_REAL ( NCID, ID_VAR_LAT, 'actual_range', &
     &                       NF_REAL, 2, RANGE_R4 )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 6222, IUER, 'WRITE_LS_MASK_NC', 'Error in '// &
     &          ' an attempt to create new attribute: '// &
     &          NF_STRERROR(IS) )
           RETURN
      END IF
!
      IS = NF_PUT_ATT_TEXT ( NCID, ID_VAR_LON, 'long_name', &
     &                       LEN('Longitude'), 'Longitude' )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 6220, IUER, 'WRITE_LS_MASK_NC', 'Error in '// &
     &         'an attempt to create new attribute: '// &
     &          NF_STRERROR(IS) )
           RETURN
      END IF
!
      IF ( FL_UNITS_DEG ) THEN
           IS = NF_PUT_ATT_TEXT ( NCID, ID_VAR_LON, 'units', &
     &                            LEN('degrees_east'), 'degrees_east' )
         ELSE 
           IS = NF_PUT_ATT_TEXT ( NCID, ID_VAR_LON, 'units', &
     &                            LEN('radian'), 'radian' )
      END IF
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 6221, IUER, 'WRITE_LS_MASK_NC', 'Error in '// &
     &         'an attempt to create new attribute: '// &
     &          NF_STRERROR(IS) )
           RETURN
      END IF
!
      RANGE_R4(1) =   0.0
      RANGE_R4(2) = PI2 - PI2/NLON
      IF ( FL_UNITS_DEG ) THEN
           RANGE_R4 = RANGE_R4/DEG__TO__RAD
      END IF
!
      IS = NF_PUT_ATT_REAL ( NCID, ID_VAR_LON, 'actual_range', &
     &                       NF_REAL, 2, RANGE_R4 )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 6219, IUER, 'WRITE_LS_MASK_NC', 'Error in '// &
     &         ' an attempt to create new attribute: '// &
     &          NF_STRERROR(IS) )
           RETURN
      END IF
!
      IF ( DTYP == 'bls_mask' ) THEN
           STR = 'Land_sea mask: 0 -- sea, 1 -- land'
           RANGE_I1(1) = 0
           RANGE_I1(2) = 1
        ELSE IF ( DTYP == 'tls_mask' ) THEN
           STR = 'Land_sea mask: 0 -- sea, 1 -- land, 2 -- land and sea'
           RANGE_I1(1) =  0
           RANGE_I1(2) =  2
        ELSE IF ( DTYP == 'fls_mask' ) THEN
           STR = 'Land_sea mask: 0 -- totally sea, 100 -- totally land, (0, 100) -- share of land'
           RANGE_I1(1) = 0
           RANGE_I1(2) = 100
        ELSE IF ( DTYP == 'dig_elev' ) THEN
           STR = 'Elevation with respect WGS84' 
           RANGE_R4(1) = -407.0
           RANGE_R4(2) = 8752.0
        ELSE IF ( DTYP == 'el_area' ) THEN
           STR = 'Elevation averaged over pixel areas with respect to WGS84' 
           RANGE_R4(1) = -407.0
           RANGE_R4(2) = 8752.0
      END IF
!
      IS = NF_PUT_ATT_TEXT ( NCID, ID_VAR_LS, 'long_name', &
     &                       ILEN(STR), STR )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 6223, IUER, 'WRITE_LS_MASK_NC', 'Error in '// &
     &         ' an attempt to create new attribute: '// &
     &          NF_STRERROR(IS) )
           RETURN
      END IF
!
      IF ( DTYP == 'dig_elev' ) THEN
           IS = NF_PUT_ATT_REAL ( NCID, ID_VAR_LS, 'actual_range', &
     &                             NF_REAL, 2, RANGE_R4 )
         ELSE IF ( DTYP == 'el_area' ) THEN
           IS = NF_PUT_ATT_REAL ( NCID, ID_VAR_LS, 'actual_range', &
     &                             NF_REAL, 2, RANGE_R4 )
         ELSE 
           IS = NF_PUT_ATT_INT1 ( NCID, ID_VAR_LS, 'actual_range', &
     &                            NF_INT1, 2, RANGE_I1 )
      END IF
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 6224, IUER, 'WRITE_LS_MASK_NC', 'Error in '// &
     &         ' an attempt to create new attribute: '// &
     &          NF_STRERROR(IS) )
           RETURN
      END IF
!
      IF ( DTYP == 'dig_elev' ) THEN
           RANGE_R4(1) = -9999.0
           IS = NF_PUT_ATT_INT1 ( NCID, ID_VAR_LS, 'missing_value', &
     &                            NF_REAL, 1, RANGE_R4(1) )
         ELSE IF ( DTYP == 'el_area' ) THEN
           RANGE_R4(1) = -9999.0
           IS = NF_PUT_ATT_INT1 ( NCID, ID_VAR_LS, 'missing_value', &
     &                            NF_REAL, 1, RANGE_R4(1) )
         ELSE 
           RANGE_I1(1) = -128
           IS = NF_PUT_ATT_INT1 ( NCID, ID_VAR_LS, 'missing_value', &
     &                            NF_INT1, 1, RANGE_I1(1) )
      END IF
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 6225, IUER, 'WRITE_LS_MASK_NC', 'Error in '// &
     &         ' an attempt to create new attribute: '// &
     &          NF_STRERROR(IS) )
           RETURN
      END IF
!
      STR = 'Derived from GTOPO30' 
      IS = NF_PUT_ATT_TEXT ( NCID, ID_VAR_LS, 'dataset', ILEN(STR), STR )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 6226, IUER, 'WRITE_LS_MASK_NC', 'Error in '// &
     &         ' an attempt to create new attribute: '// &
     &         NF_STRERROR(IS) )
           RETURN
      END IF
!
      STR = 'USGS GTOPO30'
      IS = NF_PUT_ATT_TEXT ( NCID, ID_VAR_LS, 'reference', ILEN(STR), STR )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 6227, IUER, 'WRITE_LS_MASK_NC', 'Error in '// &
     &         ' an attempt to create new attribute: '// &
     &          NF_STRERROR(IS) )
           RETURN
      END IF
!
      IS = NF_PUT_ATT_REAL ( NCID, ID_VAR_LS, 'add_offset', &
     &                       NF_REAL, 1, ADD_OFFSET_R4 )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 6228, IUER, 'WRITE_LS_MASK_NC', 'Error in '// &
     &         ' an attempt to create new attribute add_offset: '// &
     &          NF_STRERROR(IS) )
           RETURN
      END IF
!
      IS = NF_PUT_ATT_REAL ( NCID, ID_VAR_LS, 'scale_factor', &
     &                       NF_REAL, 1, SCALE_R4 )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 6229, IUER, 'WRITE_LS_MASK_NC', 'Error in '// &
     &         ' an attempt to create new attribute scale_factor: '// &
     &          NF_STRERROR(IS) )
           RETURN
      END IF
!
      IS = NF_ENDDEF ( NCID )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 6230, IUER, 'WRITE_LS_MASK_NC', 'Error in '// &
     &         ' NF_ENDDEF for output file '//FILOUT(1:I_LEN(FILOUT))// &
     &         ' error: '//NF_STRERROR(IS) )
           RETURN
      END IF
!
      DO 420 J2=1,NLON
         ARR_LON(J2) = (J2-1)*PI2/NLON
         IF ( FL_UNITS_DEG ) THEN
              ARR_LON(J2) = ARR_LON(J2)/DEG__TO__RAD
         END IF
 420  CONTINUE
!
! --- Writing longitude
!
      IS = NF_PUT_VAR_REAL ( NCID, ID_VAR_LON, ARR_LON )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 6231, IUER, 'WRITE_LS_MASK_NC', 'Error in '// &
     &            ' an attempt to write varabile longitude in the '// &
     &            'output file '//FILOUT(1:I_LEN(FILOUT))// &
     &            ' -- error: '//NF_STRERROR(IS) )
           RETURN
      END IF
!
      DO 430 J3=1,NLAT
         ARR_LAT(J3) = -P2I + (J3-1)*PI__NUM/NLAT
         IF ( FL_UNITS_DEG ) THEN
              ARR_LAT(J3) = ARR_LAT(J3)/DEG__TO__RAD
         END IF
 430  CONTINUE
!
! --- Writing latitude
!
      IS = NF_PUT_VAR_REAL ( NCID, ID_VAR_LAT, ARR_LAT )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 6232, IUER, 'WRITE_LS_MASK_NC', 'Error in '// &
     &         ' an attempt to write varabile latitude in the '// &
     &         'output file '//FILOUT(1:I_LEN(FILOUT))// &
     &         ' -- error: '//NF_STRERROR(IS) )
           RETURN
      END IF
!
      IF ( DTYP == 'dig_elev' ) THEN
           IS = NF_PUT_VAR_REAL ( NCID, ID_VAR_LS, HEI_ARR )
         ELSE IF ( DTYP == 'el_area' ) THEN
           IS = NF_PUT_VAR_REAL ( NCID, ID_VAR_LS, HEI_ARR )
         ELSE
           ALLOCATE ( HEI_ARR_I1(NLON,NLAT), STAT=IER ) 
           IF ( IER .NE. 0 ) THEN
                CALL CLRCH ( STR )
                CALL IINCH ( NLON*NLAT, STR )
                CALL ERR_LOG ( 6233, IUER, 'WRITE_LS_MASK_NC', 'Failure '// &
     &              'in attempt to allocate '//STR(1:I_LEN(STR))// &
     &              ' bytes dynamic memory for array HEI_ARR_I1' )
                RETURN
           END IF
           HEI_ARR_I1 = HEI_ARR
           IS = NF_PUT_VAR_INT1 ( NCID, ID_VAR_LS, HEI_ARR_I1 )
           DEALLOCATE ( HEI_ARR_I1 )
      END IF
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 6234, IUER, 'WRITE_LS_MASK_NC', 'Error in '// &
     &         ' an attempt to write varabile land_sea in the '// &
     &         'output file '//FILOUT(1:I_LEN(FILOUT))// &
     &         ' -- error: '//NF_STRERROR(IS) )
           RETURN
      END IF
!
! --- Uph! Close file and go home to drink hot tea (or cold bear)
!
      IS = NF_CLOSE ( NCID )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 6235, IUER, 'WRITE_LS_MASK_NC', 'Error in an '// &
     &         'attempt to close the output netcf file '// &
     &         FILOUT(1:I_LEN(FILOUT))//' NF_CLOSE: '//NF_STRERROR(IS) )
           RETURN
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  WRITE_LS_MASK_NC  !#!#
