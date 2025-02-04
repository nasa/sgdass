      SUBROUTINE READ_GEOS_RAD ( FILIN, HEB_LWTUP, HEB_LWTUPCLR, HEB_TAUTOT, &
     &                           TIM_STEP, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine READ_GEOS_RAD reads nd parses infput filr in NETCDF4       *
! *   format. It extracts datasets LWTUP, LWTUPCLR, TAUTOT, and fills    *
! *   field of the of datastructures HEB_LWTUP, HEB_LWTUPCLR, and        *
! *   HEB_TAUTOT.                                                        *
! *                                                                      *
! * ###  04-FEB-2013  READ_GEOS_RAD  v3.0 (c) L. Petrov 29-APR-2013  ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'netcdf.inc'
      INCLUDE   'heb.i'
      INTEGER*4  IUER
      REAL*8     TIM_STEP
      CHARACTER  FILIN*128
      TYPE     ( HEB__TYPE ) :: HEB_LWTUP, HEB_LWTUPCLR, HEB_TAUTOT
      INTEGER*4  IS, NCID,   DIMLEN_LAT, DIMLEN_LON, DIMLEN_TIM, &
     &           ID_DIM_LON, ID_DIM_LAT, ID_DIM_TIM, VECDIM(3),  &
     &           ID_VAR_LAT, ID_VAR_LON, ID_VAR_TIM, &
     &           ID_VAR_LWTUP, ID_VAR_LWTUPCLR, ID_VAR_TAUTOT
      REAL*8     TIM_BEG, TIM_END
      REAL*8,    ALLOCATABLE :: DAT_MIN(:)
      CHARACTER  STR*128, STR_DAT*32, STR_TIM*32, DATE_STR*32
      INTEGER*4  J1, J2, J3, ID, IP, MJD_BEG, MJD_END,INT4_STR_DAT, IER
      INTEGER*4, EXTERNAL :: ILEN, I_LEN, LINDEX
!
      HEB_LWTUP%STATUS = HEB__UNDF
!
! --- Open the new ouput file in netcdf format
!
      IS = NF_OPEN ( FILIN, NF_NOWRITE, NCID )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 7411, IUER, 'REED_GEOS_RAD', 'Error in an '// &
     &         'attempt to open the netcf file with latent flux '// &
     &          FILIN(1:I_LEN(FILIN))//' NF_OPEN: '//NF_STRERROR(IS) )
           RETURN
      END IF
!
! --- Learn the ID of dimension: 'lon'
!
      IS = NF_INQ_DIMID ( NCID, 'lon', ID_DIM_LON )
      IF ( IS .NE. 0 ) THEN
           IS = NF_INQ_DIMID ( NCID, 'XDim:EOSGRID', ID_DIM_LON )
      END IF
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 7412, IUER, 'REED_GEOS_RAD', 'Error in '// &
     &         ' an attempt to read dimension lon: '// &
     &          NF_STRERROR(IS) )
           RETURN
      END IF
!
! --- Learn the ID of dimensions 'lat'
!
      IS = NF_INQ_DIMID ( NCID, 'lat', ID_DIM_LAT )
      IF ( IS .NE. 0 ) THEN
           IS = NF_INQ_DIMID ( NCID, 'YDim:EOSGRID', ID_DIM_LAT )
      END IF
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 7413, IUER, 'REED_GEOS_RAD', 'Error in '// &
     &         ' an attempt to read dimension lat: '// &
     &          NF_STRERROR(IS) )
           RETURN
      END IF
!
! --- Learn the ID of dimensions 'tim'
!
      IS = NF_INQ_DIMID ( NCID, 'time', ID_DIM_TIM )
      IF ( IS .NE. 0 ) THEN
           IS = NF_INQ_DIMID ( NCID, 'TIME:EOSGRID', ID_DIM_TIM )
      END IF
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 7414, IUER, 'REED_GEOS_RAD', 'Error in '// &
     &         ' an attempt to read dimension tim: '// &
     &          NF_STRERROR(IS) )
           RETURN
      END IF
!
! --- Get the length of the dimension "lon"
!
      IS = NF_INQ_DIMLEN ( NCID, ID_DIM_LON, DIMLEN_LON  )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 7415, IUER, 'REED_GEOS_RAD', 'Error in '// &
     &         'getting the length of the dimension "lon" in file '// &
     &          FILIN(1:I_LEN(FILIN))//' error: '// &
     &          NF_STRERROR(IS) )
           RETURN
      END IF
!
! --- Get the length of the dimension "lat"
!
      IS = NF_INQ_DIMLEN ( NCID, ID_DIM_LAT, DIMLEN_LAT  )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 7416, IUER, 'REED_GEOS_RAD', 'Error in '// &
     &         'getting the length of the dimension "lat" in file '// &
     &          FILIN(1:I_LEN(FILIN))//' error: '// &
     &          NF_STRERROR(IS) )
           RETURN
      END IF
!
! --- Get the length of the dimension "tim"
!
      IS = NF_INQ_DIMLEN ( NCID, ID_DIM_TIM, DIMLEN_TIM  )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 7417, IUER, 'REED_GEOS_RAD', 'Error in '// &
     &         'getting the length of the dimension "tim" in file '// &
     &          FILIN(1:I_LEN(FILIN))//' error: '// &
     &          NF_STRERROR(IS) )
           RETURN
      END IF
!
! --- Learn the ID of the variable "LWTUP"
!
      IS = NF_INQ_VARID ( NCID, 'LWTUP', ID_VAR_LWTUP )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 7418, IUER, 'REED_GEOS_RAD', 'Variable '// &
     &         '"LWTUP" was not found in the input netcdf file '// &
     &          FILIN(1:I_LEN(FILIN))//' error: '//NF_STRERROR(IS) )
           RETURN
      END IF
!
! --- Learn the ID of the variable "LWTUPCLR"
!
      IS = NF_INQ_VARID ( NCID, 'LWTUP', ID_VAR_LWTUPCLR )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 7419, IUER, 'REED_GEOS_RAD', 'Variable '// &
     &         '"LWTUPCLR" was not found in the input netcdf file '// &
     &          FILIN(1:I_LEN(FILIN))//' error: '//NF_STRERROR(IS) )
           RETURN
      END IF
!
! --- Learn the ID of the variable "TAUTOT"
!
      IS = NF_INQ_VARID ( NCID, 'TAUTOT', ID_VAR_TAUTOT )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 7420, IUER, 'REED_GEOS_RAD', 'Variable '// &
     &         '"TAUTOT" was not found in the input netcdf file '// &
     &          FILIN(1:I_LEN(FILIN))//' error: '//NF_STRERROR(IS) )
           RETURN
      END IF
!
! --- Allocate memory for "LWTUP"
!
      HEB_LWTUP%DIMS(1) = DIMLEN_LON
      HEB_LWTUP%DIMS(2) = DIMLEN_LAT
      HEB_LWTUP%DIMS(3) = 1
      HEB_LWTUP%DIMS(4) = DIMLEN_TIM
      ALLOCATE ( HEB_LWTUP%VAL(HEB_LWTUP%DIMS(1),HEB_LWTUP%DIMS(2),HEB_LWTUP%DIMS(3),HEB_LWTUP%DIMS(4)), &
     &           STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH  ( STR )
           CALL IINCH8 ( INT8(4)*HEB_LWTUP%DIMS(1)*HEB_LWTUP%DIMS(2)*HEB_LWTUP%DIMS(3)*HEB_LWTUP%DIMS(4), &
     &                   STR )
           CALL ERR_LOG ( 7421, IUER, 'REED_GEOS_RAD', 'Failure in '// &
     &         'an attempt to allocate '//STR(1:I_LEN(STR))//' bytes of memory '// &
     &         'for array LWTUP' )
           RETURN
      END IF
      HEB_LWTUP%STATUS = HEB__ALLO
!
! --- Allocate memory for "LWTUPCLR"
!
      HEB_LWTUPCLR%DIMS(1) = DIMLEN_LON
      HEB_LWTUPCLR%DIMS(2) = DIMLEN_LAT
      HEB_LWTUPCLR%DIMS(3) = 1
      HEB_LWTUPCLR%DIMS(4) = DIMLEN_TIM
      ALLOCATE ( HEB_LWTUPCLR%VAL(HEB_LWTUPCLR%DIMS(1),HEB_LWTUPCLR%DIMS(2),HEB_LWTUPCLR%DIMS(3),HEB_LWTUPCLR%DIMS(4)), &
     &           STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH  ( STR )
           CALL IINCH8 ( INT8(4)*HEB_LWTUPCLR%DIMS(1)*HEB_LWTUPCLR%DIMS(2)*HEB_LWTUPCLR%DIMS(3)*HEB_LWTUPCLR%DIMS(4), &
     &                   STR )
           CALL ERR_LOG ( 7422, IUER, 'REED_GEOS_RAD', 'Failure in '// &
     &         'an attempt to allocate '//STR(1:I_LEN(STR))//' bytes of memory '// &
     &         'for array LWTUPCLR' )
           RETURN
      END IF
      HEB_LWTUPCLR%STATUS = HEB__ALLO
!
! --- Allocate memory for "TAUTOT"
!
      HEB_TAUTOT%DIMS(1) = DIMLEN_LON
      HEB_TAUTOT%DIMS(2) = DIMLEN_LAT
      HEB_TAUTOT%DIMS(3) = 1
      HEB_TAUTOT%DIMS(4) = DIMLEN_TIM
      ALLOCATE ( HEB_TAUTOT%VAL(HEB_TAUTOT%DIMS(1),HEB_TAUTOT%DIMS(2),HEB_TAUTOT%DIMS(3),HEB_TAUTOT%DIMS(4)), &
     &           STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH  ( STR )
           CALL IINCH8 ( INT8(4)*HEB_TAUTOT%DIMS(1)*HEB_TAUTOT%DIMS(2)*HEB_TAUTOT%DIMS(3)*HEB_TAUTOT%DIMS(4), &
     &                   STR )
           CALL ERR_LOG ( 7423, IUER, 'REED_GEOS_RAD', 'Failure in '// &
     &         'an attempt to allocate '//STR(1:I_LEN(STR))//' bytes of memory '// &
     &         'for array TAUTOT' )
           RETURN
      END IF
      HEB_TAUTOT%STATUS = HEB__ALLO
!
! --- Get "LWTUP" variable
!
      IS = NF_GET_VAR_REAL ( NCID, ID_VAR_LWTUP, HEB_LWTUP%VAL )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 7424, IUER, 'REED_GEOS_RAD', 'Error in '// &
     &         'getting the values of the variable "LWTUP" from the file '// &
     &          FILIN(1:I_LEN(FILIN))//' error: '// &
     &          NF_STRERROR(IS) )
           RETURN
      END IF
!
! --- Get "LWTUPCLR" variable
!
      IS = NF_GET_VAR_REAL ( NCID, ID_VAR_LWTUPCLR, HEB_LWTUPCLR%VAL )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 7425, IUER, 'REED_GEOS_RAD', 'Error in '// &
     &         'getting the values of the variable "LWTUPCLR" from the file '// &
     &          FILIN(1:I_LEN(FILIN))//' error: '// &
     &          NF_STRERROR(IS) )
           RETURN
      END IF
!
! --- Get "TAUTOT" variable
!
      IS = NF_GET_VAR_REAL ( NCID, ID_VAR_TAUTOT, HEB_TAUTOT%VAL )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 7426, IUER, 'REED_GEOS_RAD', 'Error in '// &
     &         'getting the values of the variable "TAUTOT" from the file '// &
     &          FILIN(1:I_LEN(FILIN))//' error: '// &
     &          NF_STRERROR(IS) )
           RETURN
      END IF
!
      CALL CLRCH ( HEB_LWTUP%HISTORY )
      IS = NF_GET_ATT_TEXT ( NCID, 0, 'History', HEB_LWTUP%HISTORY )
      IF ( IS .NE. 0 ) THEN
           IS = NF_GET_ATT_TEXT ( NCID, 0, 'history', HEB_LWTUP%HISTORY )
      END IF
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 7427, IUER, 'REED_GEOS_RAD', 'Error in '// &
     &         ' an attempt to read attribute History: '// &
     &          NF_STRERROR(IS) )
           RETURN
      END IF
!
      CALL CLRCH ( HEB_LWTUP%SOURCE )
      IS = NF_GET_ATT_TEXT ( NCID, 0, 'Source', HEB_LWTUP%SOURCE )
      IF ( IS .NE. 0 ) THEN
           IS = NF_GET_ATT_TEXT ( NCID, 0, 'source', HEB_LWTUP%SOURCE )
      END IF
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 7428, IUER, 'REED_GEOS_RAD', 'Error in '// &
     &         ' an attempt to read attribute Source: '// &
     &          NF_STRERROR(IS) )
           RETURN
      END IF
      CALL TRAN ( 13, HEB_LWTUP%SOURCE, HEB_LWTUP%SOURCE )
!
      CALL CLRCH ( HEB_LWTUP%TITLE )
      IS = NF_GET_ATT_TEXT ( NCID, 0, 'Title', HEB_LWTUP%TITLE )
      IF ( IS .NE. 0 ) THEN
           IS = NF_GET_ATT_TEXT ( NCID, 0, 'title', HEB_LWTUP%TITLE )
      END IF
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 7429, IUER, 'REED_GEOS_RAD', 'Error in '// &
     &         ' an attempt to read attribute Title: '// &
     &          NF_STRERROR(IS) )
           RETURN
      END IF
!
      CALL CLRCH ( HEB_LWTUP%INSTITUTION )
      IS = NF_GET_ATT_TEXT ( NCID, 0, 'Institution', HEB_LWTUP%INSTITUTION )
      IF ( IS .NE. 0 ) THEN
           IS = NF_GET_ATT_TEXT ( NCID, 0, 'institution', HEB_LWTUP%INSTITUTION )
      END IF
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 7430, IUER, 'REED_GEOS_RAD', 'Error in '// &
     &         ' an attempt to read attribute Institution: '// &
     &          NF_STRERROR(IS) )
           RETURN
      END IF
!
      CALL CLRCH ( HEB_LWTUP%REFERENCES )
      IS = NF_GET_ATT_TEXT ( NCID, 0, 'References', HEB_LWTUP%REFERENCES )
      IF ( IS .NE. 0 ) THEN
           IS = NF_GET_ATT_TEXT ( NCID, 0, 'references', HEB_LWTUP%REFERENCES )
      END IF
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 7431, IUER, 'REED_GEOS_RAD', 'Error in '// &
     &         ' an attempt to read attribute References: '// &
     &          NF_STRERROR(IS) )
           RETURN
      END IF
!
      CALL CLRCH ( HEB_LWTUP%VERSION_ID )
      IS = NF_GET_ATT_TEXT ( NCID, 0, 'VersionID', HEB_LWTUP%VERSION_ID )
      IF ( IS .NE. 0 ) THEN
           IS = NF_GET_ATT_TEXT ( NCID, 0, 'comment', HEB_LWTUP%VERSION_ID )
      END IF
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 7432, IUER, 'REED_GEOS_RAD', 'Error in '// &
     &         ' an attempt to read attribute VersionID: '// &
     &          NF_STRERROR(IS) )
           RETURN
      END IF
!
      CALL CLRCH ( HEB_LWTUP%FILE_NAME )
      IS = NF_GET_ATT_TEXT ( NCID, 0, 'Filename', HEB_LWTUP%FILE_NAME )
      IF ( IS .NE. 0 ) THEN
           ID = LINDEX ( FILIN, '/' ) 
           IF ( ID < 1 ) ID = 1
           HEB_LWTUP%FILE_NAME = FILIN(ID:)
      END IF
!
      CALL CLRCH ( HEB_LWTUP%PROD_DATE_TIME )
      IS = NF_GET_ATT_TEXT ( NCID, 0, 'ProductionDateTime', HEB_LWTUP%PROD_DATE_TIME )
      IF ( IS .NE. 0 ) THEN
           HEB_LWTUP%PROD_DATE_TIME = 'n/a'
      END IF
!
      CALL CLRCH ( HEB_LWTUP%PROD_NAME )
      IP = LINDEX ( FILIN, '/' ) + 1
      ID = LINDEX ( FILIN, '.' ) - 1
      IF ( ID < IP ) ID = IP
      ID = LINDEX ( FILIN(1:ID), '.' ) - 1
      IF ( ID < IP ) ID = IP
      HEB_LWTUP%PROD_NAME  = FILIN(IP:ID)
      ID = INDEX ( HEB_LWTUP%PROD_NAME, '.GEOS' ) 
      IF ( ID > 0 ) CALL CLRCH ( HEB_LWTUP%PROD_NAME(ID:) )
!
      IF ( DIMLEN_TIM == 1 ) THEN
           CALL CLRCH ( STR_DAT )
           IS = NF_GET_ATT_TEXT ( NCID, 0, 'RangeBeginningDate', STR_DAT )
           IF ( IS .NE. 0 ) THEN
                CALL ERR_LOG ( 7433, IUER, 'REED_GEOS_RAD', 'Error in '// &
     &              ' an attempt to read attribute RangeBeginningDate: '// &
     &               NF_STRERROR(IS) )
                RETURN
           END IF
!
           CALL CLRCH ( STR_TIM )
           IS = NF_GET_ATT_TEXT ( NCID, 0, 'RangeBeginningTime', STR_TIM )
           IF ( IS .NE. 0 ) THEN
                CALL ERR_LOG ( 7434, IUER, 'REED_GEOS_RAD', 'Error in '// &
     &              ' an attempt to read attribute RangeBeginningTime: '// &
     &               NF_STRERROR(IS) )
                RETURN
           END IF
!
           DATE_STR = STR_DAT(1:I_LEN(STR_DAT))//'_'//STR_TIM(1:I_LEN(STR_TIM))
           CALL ERR_PASS ( IUER, IER )
           CALL DATE_TO_TIME ( DATE_STR, HEB_LWTUP%MJD, HEB_LWTUP%UTC, IER )
           IF ( IS .NE. 0 ) THEN
                CALL ERR_LOG ( 7435, IUER, 'REED_GEOS_RAD', 'Error in '// &
     &              ' an attempt to decode beginning date '//DATE_STR )
                RETURN
           END IF
!
           CALL CLRCH ( STR_DAT )
           IS = NF_GET_ATT_TEXT ( NCID, 0, 'RangeEndingDate', STR_DAT )
           IF ( IS .NE. 0 ) THEN
                CALL ERR_LOG ( 7436, IUER, 'REED_GEOS_RAD', 'Error in '// &
     &              ' an attempt to read attribute RangeBeginningDate: '// &
     &               NF_STRERROR(IS) )
                RETURN
           END IF
!
           CALL CLRCH ( STR_TIM )
           IS = NF_GET_ATT_TEXT ( NCID, 0, 'RangeEndingTime', STR_TIM )
           IF ( IS .NE. 0 ) THEN
                CALL ERR_LOG ( 7437, IUER, 'REED_GEOS_RAD', 'Error in '// &
     &              ' an attempt to read attribute RangeBeginningTime: '// &
     &               NF_STRERROR(IS) )
                RETURN
           END IF
!
           DATE_STR = STR_DAT(1:I_LEN(STR_DAT))//'_'//STR_TIM(1:I_LEN(STR_TIM))
           CALL ERR_PASS ( IUER, IER )
           CALL DATE_TO_TIME ( DATE_STR, MJD_END, TIM_END, IER )
           IF ( IS .NE. 0 ) THEN
                CALL ERR_LOG ( 7438, IUER, 'REED_GEOS_RAD', 'Error in '// &
     &              ' an attempt to decode beginning date '//DATE_STR )
                RETURN
           END IF
!
           HEB_LWTUP%UTC = (HEB_LWTUP%UTC + (MJD_END - HEB_LWTUP%MJD)*86400.0D0 + TIM_END)/2.0D0
           IF ( HEB_LWTUP%UTC > 86400.0D0 ) THEN
                HEB_LWTUP%UTC = HEB_LWTUP%UTC - 86400.0D0
                HEB_LWTUP%MJD = HEB_LWTUP%MJD + 1
           END IF
           TIM_STEP = 0.0D0
         ELSE IF ( DIMLEN_TIM > 1 ) THEN
!
! -------- Learn the ID of the variable "time"
!
           IS = NF_INQ_VARID ( NCID, 'time', ID_VAR_TIM)
           IF ( IS .NE. 0 ) THEN
                IS = NF_INQ_VARID ( NCID, 'TIME:EOSGRID', ID_VAR_TIM)
           END IF
           IF ( IS .NE. 0 ) THEN
                CALL ERR_LOG ( 7439, IUER, 'REED_GEOS_RAD', 'Variable '// &
     &              '"time" was not found in the input netcdf file '// &
     &              FILIN(1:I_LEN(FILIN))//' error: '//NF_STRERROR(IS) )
                RETURN
           END IF
!
           ALLOCATE ( DAT_MIN(DIMLEN_TIM), STAT=IER )
           IF ( IER .NE. 0 ) THEN
                CALL CLRCH ( STR )
                CALL IINCH ( 8*DIMLEN_TIM, STR )
                CALL ERR_LOG ( 7440, IUER, 'REED_GEOS_RAD', 'Failure in '// &
     &              'an attempt to allocate '//STR(1:I_LEN(STR))// &
     &              ' bytes of memory for array DAT_MIN' )
               RETURN
           END IF
!
           IS = NF_GET_ATT_INT  ( NCID, ID_VAR_TIM, 'begin_date', INT4_STR_DAT )
           IF ( IS .NE. 0 ) THEN
                CALL ERR_LOG ( 7441, IUER, 'REED_GEOS_RAD', 'Error in '// &
     &              ' an attempt to read attribute begin_date: '// &
     &               NF_STRERROR(IS) )
                RETURN
           END IF
!
! -------- Get variable DAT_MIN
!
           IS = NF_GET_VAR_REAL ( NCID, ID_VAR_TIM, DAT_MIN )
           IF ( IS .NE. 0 ) THEN
                CALL ERR_LOG ( 7442, IUER, 'REED_GEOS_RAD', 'Error in '// &
     &                        'getting the values of the SDS parameter "Time" '// &
     &                        'from file '//FILIN(1:I_LEN(FILIN))//' error: '// &
     &                         NF_STRERROR(IS) )
                RETURN
           END IF
!
           WRITE ( UNIT=DATE_STR, FMT='(I8)' ) INT4_STR_DAT
           CALL RH_TAT ( DAT_MIN(1)/60.0D0/24.0D0*PI2, 1, STR_TIM, IER )
!
           DATE_STR = DATE_STR(1:4)//'.'//DATE_STR(5:6)//'.'//&
     &                DATE_STR(7:8)//'_'//STR_TIM(2:)
           TIM_STEP = (DAT_MIN(2) - DAT_MIN(1))*60.0D0
!
           CALL ERR_PASS ( IUER, IER )
           CALL DATE_TO_TIME ( DATE_STR, HEB_LWTUP%MJD, HEB_LWTUP%UTC, IER )
           IF ( IS .NE. 0 ) THEN
                CALL ERR_LOG ( 7443, IUER, 'REED_GEOS_RAD', 'Error in '// &
     &              ' an attempt to decode beginning date '//DATE_STR )
                RETURN
           END IF
!
           CALL CLRCH ( HEB_LWTUP%PROD_DATE_TIME )
           HEB_LWTUP%PROD_DATE_TIME = 'not_available'
!
           DEALLOCATE ( DAT_MIN )
      END IF
!
! --- Close the NetCDF-file
!
      IS = NF_CLOSE ( NCID )
!
      HEB_LWTUPCLR%PROD_NAME      = HEB_LWTUP%PROD_NAME
      HEB_LWTUPCLR%HISTORY        = HEB_LWTUP%HISTORY 
      HEB_LWTUPCLR%SOURCE         = HEB_LWTUP%SOURCE 
      HEB_LWTUPCLR%TITLE          = HEB_LWTUP%TITLE
      HEB_LWTUPCLR%INSTITUTION    = HEB_LWTUP%INSTITUTION 
      HEB_LWTUPCLR%REFERENCES     = HEB_LWTUP%REFERENCES 
      HEB_LWTUPCLR%VERSION_ID     = HEB_LWTUP%VERSION_ID 
      HEB_LWTUPCLR%FILE_NAME      = HEB_LWTUP%FILE_NAME 
      HEB_LWTUPCLR%PROD_DATE_TIME = HEB_LWTUP%PROD_DATE_TIME 
      HEB_LWTUPCLR%MJD            = HEB_LWTUP%MJD
      HEB_LWTUPCLR%UTC            = HEB_LWTUP%UTC
!
      HEB_TAUTOT%PROD_NAME        = HEB_LWTUP%PROD_NAME
      HEB_TAUTOT%HISTORY          = HEB_LWTUP%HISTORY 
      HEB_TAUTOT%SOURCE           = HEB_LWTUP%SOURCE 
      HEB_TAUTOT%TITLE            = HEB_LWTUP%TITLE
      HEB_TAUTOT%INSTITUTION      = HEB_LWTUP%INSTITUTION 
      HEB_TAUTOT%REFERENCES       = HEB_LWTUP%REFERENCES 
      HEB_TAUTOT%VERSION_ID       = HEB_LWTUP%VERSION_ID 
      HEB_TAUTOT%FILE_NAME        = HEB_LWTUP%FILE_NAME 
      HEB_TAUTOT%PROD_DATE_TIME   = HEB_LWTUP%PROD_DATE_TIME 
      HEB_TAUTOT%MJD              = HEB_LWTUP%MJD
      HEB_TAUTOT%UTC              = HEB_LWTUP%UTC
!
      CALL ERR_LOG ( 0, IUER ) 
      RETURN
      END  SUBROUTINE  READ_GEOS_RAD  !#!#
