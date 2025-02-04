      SUBROUTINE READ_LS_MASK_NC ( FILIN, MALO, IUER ) 
! ************************************************************************
! *                                                                      *
! *   Routine READ_LS_MASK_NC
! *                                                                      *
! *  ### 18-NOV-2002 READ_LS_MASK_NC v1.0 (c) L. Petrov 18-NOV-2002 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'astro_constants.i'
      INCLUDE   'netcdf.inc'
      INCLUDE   'malo.i'
      INTEGER*4  NLON, NLAT, IUER
      TYPE     ( MALO__TYPE ) :: MALO
      CHARACTER  FILIN*(*)
      INTEGER*4  IS, NCID,   DIMLEN_LAT, DIMLEN_LON, VECDIM(2), &
     &           ID_DIM_LON, ID_DIM_LAT, ID_DIM_LS, &
     &           ID_VAR_LS,  ID_VAR_LAT, ID_VAR_LON
      REAL*4     ADD_OFFSET, SCALE_FACTOR
      CHARACTER  STR*128, STR1*128
      INTEGER*4  J1, J2, J3, J4, IER
      INTEGER*4, EXTERNAL :: I_LEN, ILEN
!
      IF ( MALO%NLAT .LE. 0 ) THEN
           CALL CLRCH ( STR ) 
           CALL INCH  ( MALO%NLAT, STR )
           CALL CLRCH ( STR1 ) 
           CALL INCH  ( MALO__MDIM, STR1 )
           CALL ERR_LOG ( 6271, IUER, 'READ_LS_MASK_NC', 'Vairiable '// &
     &         'MALO%NLAT = '//STR(1:I_LEN(STR))//' -- an integer '// &
     &         ' in range [0, '//STR1(1:I_LEN(STR1))//'] was expected' ) 
           RETURN
      END IF
!
      IF ( MALO%NLON .LE. 0 ) THEN
           CALL CLRCH ( STR ) 
           CALL INCH  ( MALO%NLON, STR )
           CALL CLRCH ( STR1 ) 
           CALL INCH  ( MALO__MDIM, STR1 )
           CALL ERR_LOG ( 6272, IUER, 'READ_LS_MASK_NC', 'Vairiable '// &
     &         'MALO%NLON = '//STR(1:I_LEN(STR))//' -- an integer '// &
     &         ' in range [0, '//STR1(1:I_LEN(STR1))//'] was expected' ) 
           RETURN
      END IF
!
      IF ( ASSOCIATED ( MALO%LSM ) ) THEN
           DEALLOCATE ( MALO%LSM )
      END IF
      MALO%LSM_STATUS = MALO__UNDF
!
      ALLOCATE ( MALO%LSM(MALO%NLON,MALO%NLAT), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( 4*MALO%NLON*MALO%NLAT )
           CALL ERR_LOG ( 6273, IUER, 'READ_LS_MASK_NC', 'Failure to '// &
     &         'allocate '//STR(1:I_LEN(STR))//' bytes of dynamic '// &
     &         'memory for array land/sea mask' )
           RETURN
      END IF
      MALO%LSM_STATUS = MALO__ALLO
!
      VECDIM(1) = 1
      VECDIM(2) = 2
!
! --- Open the new ouput file in netcdf format
!
      IS = NF_OPEN ( FILIN, NF_NOWRITE, NCID )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 6274, IUER, 'READ_LS_MASK_NC', 'Error in an '// &
     &         'attempt to open the netcf file with land/sea mask '// &
     &         FILIN(1:I_LEN(FILIN))//' NF_CREATE: '//NF_STRERROR(IS) )
           RETURN
      END IF
!
! --- Learn th ID of dimension: 'lon'
!
      IS = NF_INQ_DIMID ( NCID, 'lon', ID_DIM_LON )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 6275, IUER, 'READ_LS_MASK_NC', 'Error in '// &
     &         ' an attempt to read dimension lon: '// &
     &          NF_STRERROR(IS) )
           RETURN
      END IF
!
! --- Learn the ID of dimensions 'lat'
!
      IS = NF_INQ_DIMID ( NCID, 'lat', ID_DIM_LAT )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 6276, IUER, 'READ_LS_MASK_NC', 'Error in '// &
     &         ' an attempt to read dimension lat: '// &
     &          NF_STRERROR(IS) )
           RETURN
      END IF
!
! --- Get the length of the dimension "lon"
!
      IS = NF_INQ_DIMLEN ( NCID, ID_DIM_LON, DIMLEN_LON  )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 6277, IUER, 'READ_LS_MASK_NC', 'Error in getting '// &
     &         'the length of the dimension "lon" in file '// &
     &          FILIN(1:I_LEN(FILIN))//' error: '// &
     &          NF_STRERROR(IS) )
           RETURN
      END IF
!
! --- Check dimension "lon"
!
      IF ( DIMLEN_LON .NE. MALO%NLON ) THEN
           WRITE ( 6, * ) ' Expected dimension: ', MALO%NLON, &
     &                    ' Found dimension: ', DIMLEN_LON
           CALL ERR_LOG ( 6278, IUER, 'READ_LS_MASK_NC', 'The land sea '// &
     &                   'mask file '//FILIN(1:I_LEN(FILIN))// &
     &                   ' has unexpected longiutude dimension' )
           RETURN
      END IF
!
! --- Get the length of the dimension "lat"
!
      IS = NF_INQ_DIMLEN ( NCID, ID_DIM_LAT, DIMLEN_LAT  )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 6279, IUER, 'READ_LS_MASK_NC', 'Error in getting '// &
     &         'the length of the dimension "lat" in file '// &
     &          FILIN(1:I_LEN(FILIN))//' error: '// &
     &          NF_STRERROR(IS) )
           RETURN
      END IF
!
! --- Check dimsnetion "lat"
!
      IF ( DIMLEN_LAT .NE. MALO%NLAT ) THEN
           WRITE ( 6, * ) ' Expected dimension: ', MALO%NLAT, &
     &                    ' Found dimension: ', DIMLEN_LON
           CALL ERR_LOG ( 6280, IUER, 'READ_LS_MASK_NC', 'NETcdf land_sea '// &
     &         'mask file '//FILIN(1:I_LEN(FILIN))// &
     &         'has unexpected longiutude dimension' )
           RETURN
      END IF
!
! --- Learn the ID of the variable "land"
!
      IS = NF_INQ_VARID ( NCID, 'land', ID_VAR_LS )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 6281, IUER, 'READ_LS_MASK_NC', 'Variable "land" '// &
     &         'was not found in the input netcdf file '// &
     &          FILIN(1:I_LEN(FILIN))//' error: '//NF_STRERROR(IS) )
           RETURN
      END IF
!
! --- Get the variable: land_sea mask
!
      IS = NF_GET_VAR_REAL ( NCID, ID_VAR_LS, MALO%LSM )
!
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 6282, IUER, 'READ_LS_MASK_NC', 'Error in getting '// &
     &         'the values of the variable "land" from the file '// &
     &          FILIN(1:I_LEN(FILIN))//' error: '// &
     &          NF_STRERROR(IS) )
           RETURN
      END IF
!
      IS = NF_GET_ATT_REAL ( NCID, ID_VAR_LS, 'add_offset',   ADD_OFFSET   )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 6283, IUER, 'READ_LS_MASK_NC', 'Failure '// &
     &         'in attempt to retrieve attribute add_offset for '// &
     &         'the "land" variable from the file '// &
     &          FILIN(1:I_LEN(FILIN))//' error: '// &
     &          NF_STRERROR(IS) )
           RETURN
      END IF
!
      IS = NF_GET_ATT_REAL ( NCID, ID_VAR_LS, 'scale_factor', SCALE_FACTOR )
      IF ( IS .NE. 0 ) THEN
           IS = NF_GET_ATT_REAL ( NCID, ID_VAR_LS, 'scale', SCALE_FACTOR )
      END IF
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 6284, IUER, 'READ_LS_MASK_NC', 'Failure '// &
     &         'in attempt to retrieve attribute scale_factor for '// &
     &         'the "land" variable from the file '// &
     &          FILIN(1:I_LEN(FILIN))//' error: '// &
     &          NF_STRERROR(IS) )
           RETURN
      END IF
!
      MALO%LSM = (MALO%LSM + ADD_OFFSET)*SCALE_FACTOR
      MALO%LSM_STATUS = MALO__LOAD
!
! --- Close file
!
      IS = NF_CLOSE ( NCID )
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  READ_LS_MASK_NC  !#!#
