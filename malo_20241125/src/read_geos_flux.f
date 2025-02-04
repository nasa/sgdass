      SUBROUTINE READ_GEOS_FLUX ( FILIN, HEB_EFLUX, HEB_HFLUX, TIM_STEP, &
     &                              IUER )
! ************************************************************************
! *                                                                      *
! *   Routine READ_GEOS_FLUX
! *                                                                      *
! * ### 11-JAN-2013  READ_GEOS_FLUX v1.0 (c) L. Petrov 02-FEB-2013 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'netcdf.inc'
      INCLUDE   'heb.i'
      INTEGER*4  IUER
      CHARACTER  FILIN*128
      TYPE     ( HEB__TYPE ) :: HEB_EFLUX, HEB_HFLUX
      REAL*8     TIM_STEP
      INTEGER*4  IS, NCID,   DIMLEN_LAT, DIMLEN_LON, DIMLEN_TIM, &
     &           ID_DIM_LON, ID_DIM_LAT, ID_DIM_TIM, VECDIM(3),  &
     &           ID_VAR_LAT, ID_VAR_LON, ID_VAR_TIM, &
     &           ID_VAR_EFLUX, ID_VAR_HFLUX
      REAL*8     TIM_BEG, TIM_END
      REAL*8,    ALLOCATABLE :: DAT_MIN(:)
      CHARACTER  STR*128, STR_DAT*32, STR_TIM*32, DATE_STR*32
      INTEGER*4  J1, J2, J3, MJD_BEG, MJD_END, INT4_STR_DAT, ID, IP, IER
      INTEGER*4, EXTERNAL :: ILEN, I_LEN, LINDEX
!
      HEB_EFLUX%STATUS = HEB__UNDF
      HEB_HFLUX%STATUS = HEB__UNDF
!
! --- Open the new ouput file in netcdf format
!
      IS = NF_OPEN ( FILIN, NF_NOWRITE, NCID )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 7321, IUER, 'READ_GEOS_FLUX', 'Error in an '// &
     &         'attempt to open the netcf file with latent flux '// &
     &          FILIN(1:I_LEN(FILIN))//' NF_OPEN: '//NF_STRERROR(IS) )
           RETURN
      END IF
!
! --- Learn th ID of dimension: 'lon'
!
      IS = NF_INQ_DIMID ( NCID, 'lon', ID_DIM_LON )
      IF ( IS .NE. 0 ) THEN
           IS = NF_INQ_DIMID ( NCID, 'XDim:EOSGRID', ID_DIM_LON )
      END IF
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 7322, IUER, 'READ_GEOS_FLUX', 'Error in '// &
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
           CALL ERR_LOG ( 7323, IUER, 'READ_GEOS_FLUX', 'Error in '// &
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
           CALL ERR_LOG ( 7324, IUER, 'READ_GEOS_FLUX', 'Error in '// &
     &         ' an attempt to read dimension tim: '// &
     &          NF_STRERROR(IS) )
           RETURN
      END IF
!
! --- Get the length of the dimension "lon"
!
      IS = NF_INQ_DIMLEN ( NCID, ID_DIM_LON, DIMLEN_LON  )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 7325, IUER, 'READ_GEOS_FLUX', 'Error in '// &
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
           CALL ERR_LOG ( 7326, IUER, 'READ_GEOS_FLUX', 'Error in '// &
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
           CALL ERR_LOG ( 7327, IUER, 'READ_GEOS_FLUX', 'Error in '// &
     &         'getting the length of the dimension "tim" in file '// &
     &          FILIN(1:I_LEN(FILIN))//' error: '// &
     &          NF_STRERROR(IS) )
           RETURN
      END IF
!
      HEB_EFLUX%DIMS(1) = DIMLEN_LON
      HEB_EFLUX%DIMS(2) = DIMLEN_LAT
      HEB_EFLUX%DIMS(3) = 1
      HEB_EFLUX%DIMS(4) = DIMLEN_TIM
      ALLOCATE ( HEB_EFLUX%VAL(HEB_EFLUX%DIMS(1),HEB_EFLUX%DIMS(2),HEB_EFLUX%DIMS(3),HEB_EFLUX%DIMS(4)), &
     &           STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH  ( STR )
           CALL IINCH8 ( INT8(4)*HEB_EFLUX%DIMS(1)*HEB_EFLUX%DIMS(2)*HEB_EFLUX%DIMS(3)*HEB_EFLUX%DIMS(4), &
     &                   STR )
           CALL ERR_LOG ( 7328, IUER, 'READ_GEOS_FLUX', 'Failure in '// &
     &         'an attempt to allocate '//STR(1:I_LEN(STR))//' bytes of memory '// &
     &         'for array EFLUX' )
           RETURN
      END IF
      HEB_EFLUX%STATUS = HEB__ALLO
!
      HEB_HFLUX%DIMS(1) = DIMLEN_LON
      HEB_HFLUX%DIMS(2) = DIMLEN_LAT
      HEB_HFLUX%DIMS(3) = 1
      HEB_HFLUX%DIMS(4) = DIMLEN_TIM
      ALLOCATE ( HEB_HFLUX%VAL(HEB_HFLUX%DIMS(1),HEB_HFLUX%DIMS(2),HEB_HFLUX%DIMS(3),HEB_HFLUX%DIMS(4)), &
     &           STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH  ( STR )
           CALL IINCH8 ( INT8(4)*HEB_HFLUX%DIMS(1)*HEB_HFLUX%DIMS(2)*HEB_HFLUX%DIMS(3)*HEB_HFLUX%DIMS(4), &
     &                   STR )
           CALL ERR_LOG ( 7329, IUER, 'READ_GEOS_FLUX', 'Failure in '// &
     &         'an attempt to allocate '//STR(1:I_LEN(STR))//' bytes of memory '// &
     &         'for array EFLUX' )
           RETURN
      END IF
      HEB_HFLUX%STATUS = HEB__ALLO
!
! --- Learn the ID of the variable "EFLUX"
!
      IS = NF_INQ_VARID ( NCID, 'EFLUX', ID_VAR_EFLUX )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 7330, IUER, 'READ_GEOS_FLUX', 'Variable '// &
     &         '"EFLUX" was not found in the input netcdf file '// &
     &          FILIN(1:I_LEN(FILIN))//' error: '//NF_STRERROR(IS) )
           RETURN
      END IF
!
! --- Read array with EFLUX data
!
      IS = NF_GET_VAR_REAL ( NCID, ID_VAR_EFLUX, HEB_EFLUX%VAL )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 7331, IUER, 'READ_GEOS_FLUX', 'Error in '// &
     &         'getting the values of the variable "EFLUX" from the file '// &
     &          FILIN(1:I_LEN(FILIN))//' error: '// &
     &          NF_STRERROR(IS) )
           RETURN
      END IF
      HEB_EFLUX%STATUS = HEB__LOAD
!
! --- Learn the ID of the variable "HFLUX"
!
      IS = NF_INQ_VARID ( NCID, 'HFLUX', ID_VAR_HFLUX )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 7332, IUER, 'READ_GEOS_FLUX', 'Variable '// &
     &         '"EFLUX" was not found in the input netcdf file '// &
     &          FILIN(1:I_LEN(FILIN))//' error: '//NF_STRERROR(IS) )
           RETURN
      END IF
!
! --- Read array with HFLUX data
!
      IS = NF_GET_VAR_REAL ( NCID, ID_VAR_HFLUX, HEB_HFLUX%VAL )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 7333, IUER, 'READ_GEOS_FLUX', 'Error in '// &
     &         'getting the values of the variable "HFLUX" from the file '// &
     &          FILIN(1:I_LEN(FILIN))//' error: '// &
     &          NF_STRERROR(IS) )
           RETURN
      END IF
      HEB_HFLUX%STATUS = HEB__LOAD
!
      CALL CLRCH ( HEB_EFLUX%HISTORY )
      IS = NF_GET_ATT_TEXT ( NCID, 0, 'History', HEB_EFLUX%HISTORY )
      IF ( IS .NE. 0 ) THEN
           IS = NF_GET_ATT_TEXT ( NCID, 0, 'history', HEB_EFLUX%HISTORY )
      END IF
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 7335, IUER, 'READ_GEOS_FLUX', 'Error in '// &
     &         ' an attempt to read attribute History: '// &
     &          NF_STRERROR(IS) )
           RETURN
      END IF
!
      CALL CLRCH ( HEB_EFLUX%SOURCE )
      IS = NF_GET_ATT_TEXT ( NCID, 0, 'Source', HEB_EFLUX%SOURCE )
      IF ( IS .NE. 0 ) THEN
           IS = NF_GET_ATT_TEXT ( NCID, 0, 'source', HEB_EFLUX%SOURCE )
      END IF
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 7336, IUER, 'READ_GEOS_FLUX', 'Error in '// &
     &         ' an attempt to read attribute Source: '// &
     &          NF_STRERROR(IS) )
           RETURN
      END IF
      CALL TRAN ( 13, HEB_EFLUX%SOURCE, HEB_EFLUX%SOURCE )
!
      CALL CLRCH ( HEB_EFLUX%TITLE )
      IS = NF_GET_ATT_TEXT ( NCID, 0, 'Title', HEB_EFLUX%TITLE )
      IF ( IS .NE. 0 ) THEN
           IS = NF_GET_ATT_TEXT ( NCID, 0, 'title', HEB_EFLUX%TITLE )
      END IF
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 7337, IUER, 'READ_GEOS_FLUX', 'Error in '// &
     &         ' an attempt to read attribute Title: '// &
     &          NF_STRERROR(IS) )
           RETURN
      END IF
!
      CALL CLRCH ( HEB_EFLUX%INSTITUTION )
      IS = NF_GET_ATT_TEXT ( NCID, 0, 'Institution', HEB_EFLUX%INSTITUTION )
      IF ( IS .NE. 0 ) THEN
           IS = NF_GET_ATT_TEXT ( NCID, 0, 'institution', HEB_EFLUX%INSTITUTION )
      END IF
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 7338, IUER, 'READ_GEOS_FLUX', 'Error in '// &
     &         ' an attempt to read attribute Institution: '// &
     &          NF_STRERROR(IS) )
           RETURN
      END IF
!
      CALL CLRCH ( HEB_EFLUX%REFERENCES )
      IS = NF_GET_ATT_TEXT ( NCID, 0, 'References', HEB_EFLUX%REFERENCES )
      IF ( IS .NE. 0 ) THEN
           IS = NF_GET_ATT_TEXT ( NCID, 0, 'references', HEB_EFLUX%REFERENCES )
      END IF
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 7339, IUER, 'READ_GEOS_FLUX', 'Error in '// &
     &         ' an attempt to read attribute References: '// &
     &          NF_STRERROR(IS) )
           RETURN
      END IF
!
      CALL CLRCH ( HEB_EFLUX%VERSION_ID )
      IS = NF_GET_ATT_TEXT ( NCID, 0, 'VersionID', HEB_EFLUX%VERSION_ID )
      IF ( IS .NE. 0 ) THEN
           IS = NF_GET_ATT_TEXT ( NCID, 0, 'comment', HEB_EFLUX%VERSION_ID )
      END IF
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 7340, IUER, 'READ_GEOS_FLUX', 'Error in '// &
     &         ' an attempt to read attribute VersionID: '// &
     &          NF_STRERROR(IS) )
           RETURN
      END IF
!
      CALL CLRCH ( HEB_EFLUX%FILE_NAME )
      IS = NF_GET_ATT_TEXT ( NCID, 0, 'Filename', HEB_EFLUX%FILE_NAME )
      IF ( IS .NE. 0 ) THEN
           ID = LINDEX ( FILIN, '/' ) 
           HEB_EFLUX%FILE_NAME = FILIN(ID:)
      END IF
!
      CALL CLRCH ( HEB_EFLUX%PROD_DATE_TIME )
      IS = NF_GET_ATT_TEXT ( NCID, 0, 'ProductionDateTime', HEB_EFLUX%PROD_DATE_TIME )
      IF ( IS .NE. 0 ) THEN
           HEB_EFLUX%PROD_DATE_TIME = 'n/a'
      END IF
!
      IF ( DIMLEN_TIM == 1 ) THEN
           CALL CLRCH ( HEB_EFLUX%PROD_NAME )
           IS = NF_GET_ATT_TEXT ( NCID, 0, 'LongName', HEB_EFLUX%PROD_NAME )
           IF ( IS .NE. 0 ) THEN
                IS = NF_GET_ATT_TEXT ( NCID, 0, 'long_name', HEB_EFLUX%PROD_NAME )
           END IF
           IF ( IS .NE. 0 ) THEN
                CALL ERR_LOG ( 7334, IUER, 'READ_GEOS_FLUX', 'Error in '// &
          &         ' an attempt to read attribute LongName: '// &
          &          NF_STRERROR(IS) )
                RETURN
           END IF
!
           CALL CLRCH ( STR_DAT )
           IS = NF_GET_ATT_TEXT ( NCID, 0, 'RangeBeginningDate', STR_DAT )
           IF ( IS .NE. 0 ) THEN
                CALL ERR_LOG ( 7341, IUER, 'READ_GEOS_FLUX', 'Error in '// &
     &              ' an attempt to read attribute RangeBeginningDate: '// &
     &               NF_STRERROR(IS) )
                RETURN
           END IF
!
           CALL CLRCH ( STR_TIM )
           IS = NF_GET_ATT_TEXT ( NCID, 0, 'RangeBeginningTime', STR_TIM )
           IF ( IS .NE. 0 ) THEN
                CALL ERR_LOG ( 7342, IUER, 'READ_GEOS_FLUX', 'Error in '// &
     &              ' an attempt to read attribute RangeBeginningTime: '// &
     &               NF_STRERROR(IS) )
                RETURN
           END IF
!
           DATE_STR = STR_DAT(1:I_LEN(STR_DAT))//'_'//STR_TIM(1:I_LEN(STR_TIM))
           CALL ERR_PASS ( IUER, IER )
           CALL DATE_TO_TIME ( DATE_STR, HEB_EFLUX%MJD, HEB_EFLUX%UTC, IER )
           IF ( IS .NE. 0 ) THEN
                CALL ERR_LOG ( 7343, IUER, 'READ_GEOS_FLUX', 'Error in '// &
     &              ' an attempt to decode beginning date '//DATE_STR )
                RETURN
           END IF
!
           CALL CLRCH ( STR_DAT )
           IS = NF_GET_ATT_TEXT ( NCID, 0, 'RangeEndingDate', STR_DAT )
           IF ( IS .NE. 0 ) THEN
                CALL ERR_LOG ( 7344, IUER, 'READ_GEOS_FLUX', 'Error in '// &
     &              ' an attempt to read attribute RangeBeginningDate: '// &
     &               NF_STRERROR(IS) )
                RETURN
           END IF
!
           CALL CLRCH ( STR_TIM )
           IS = NF_GET_ATT_TEXT ( NCID, 0, 'RangeEndingTime', STR_TIM )
           IF ( IS .NE. 0 ) THEN
                CALL ERR_LOG ( 7345, IUER, 'READ_GEOS_FLUX', 'Error in '// &
     &              ' an attempt to read attribute RangeBeginningTime: '// &
     &               NF_STRERROR(IS) )
                RETURN
           END IF
!
           DATE_STR = STR_DAT(1:I_LEN(STR_DAT))//'_'//STR_TIM(1:I_LEN(STR_TIM))
           CALL ERR_PASS ( IUER, IER )
           CALL DATE_TO_TIME ( DATE_STR, MJD_END, TIM_END, IER )
           IF ( IS .NE. 0 ) THEN
                CALL ERR_LOG ( 7346, IUER, 'READ_GEOS_FLUX', 'Error in '// &
     &              ' an attempt to decode beginning date '//DATE_STR )
                RETURN
           END IF
!
           HEB_EFLUX%UTC = (HEB_EFLUX%UTC + (MJD_END - HEB_EFLUX%MJD)*86400.0D0 + TIM_END)/2.0D0
           IF ( HEB_EFLUX%UTC > 86400.0D0 ) THEN
                HEB_EFLUX%UTC = HEB_EFLUX%UTC - 86400.0D0
                HEB_EFLUX%MJD = HEB_EFLUX%MJD + 1
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
                CALL ERR_LOG ( 7330, IUER, 'READ_GEOS_FLUX', 'Variable '// &
     &              '"time" was not found in the input netcdf file '// &
     &              FILIN(1:I_LEN(FILIN))//' error: '//NF_STRERROR(IS) )
                RETURN
           END IF
!
           ALLOCATE ( DAT_MIN(DIMLEN_TIM), STAT=IER )
           IF ( IER .NE. 0 ) THEN
                CALL CLRCH ( STR )
                CALL IINCH ( 8*DIMLEN_TIM, STR )
                CALL ERR_LOG ( 7329, IUER, 'READ_GEOS_FLUX', 'Failure in '// &
     &              'an attempt to allocate '//STR(1:I_LEN(STR))// &
     &              ' bytes of memory for array DAT_MIN' )
               RETURN
           END IF
!
           CALL CLRCH ( HEB_EFLUX%PROD_NAME )
           IP = LINDEX ( FILIN, '/' ) + 1
           ID = LINDEX ( FILIN, '.' ) - 1
           IF ( ID < IP ) ID = IP
           ID = LINDEX ( FILIN(1:ID), '.' ) - 1
           IF ( ID < IP ) ID = IP
           HEB_EFLUX%PROD_NAME  = FILIN(IP:ID)
!
           IS = NF_GET_ATT_INT  ( NCID, ID_VAR_TIM, 'begin_date', INT4_STR_DAT )
           IF ( IS .NE. 0 ) THEN
                CALL ERR_LOG ( 7347, IUER, 'READ_GEOS_FLUX', 'Error in '// &
     &              ' an attempt to read attribute begin_date: '// &
     &               NF_STRERROR(IS) )
                RETURN
           END IF
!
! -------- Get variable DAT_MIN
!
           IS = NF_GET_VAR_REAL ( NCID, ID_VAR_TIM, DAT_MIN )
           IF ( IS .NE. 0 ) THEN
                CALL ERR_LOG ( 7348, IUER, 'READ_GEOS_FLUX', 'Error in '// &
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
           CALL DATE_TO_TIME ( DATE_STR, HEB_EFLUX%MJD, HEB_EFLUX%UTC, IER )
           IF ( IS .NE. 0 ) THEN
                CALL ERR_LOG ( 7351, IUER, 'READ_GEOS_FLUX', 'Error in '// &
     &              ' an attempt to decode beginning date '//DATE_STR )
                RETURN
           END IF
!
           CALL CLRCH ( HEB_EFLUX%PROD_DATE_TIME )
           HEB_EFLUX%PROD_DATE_TIME = 'not_available'
!
           DEALLOCATE ( DAT_MIN )
      END IF
      HEB_HFLUX%MJD = HEB_EFLUX%MJD 
      HEB_HFLUX%UTC = HEB_EFLUX%UTC 
      HEB_HFLUX%PROD_NAME = HEB_EFLUX%PROD_NAME
      HEB_HFLUX%FILE_NAME = HEB_EFLUX%FILE_NAME
      HEB_HFLUX%HISTORY   = HEB_EFLUX%HISTORY 
      HEB_HFLUX%SOURCE    = HEB_EFLUX%SOURCE
      HEB_HFLUX%TITLE     = HEB_EFLUX%TITLE
      HEB_HFLUX%INSTITUTION    = HEB_EFLUX%INSTITUTION
      HEB_HFLUX%REFERENCES     = HEB_EFLUX%REFERENCES
      HEB_HFLUX%PROD_DATE_TIME = HEB_EFLUX%PROD_DATE_TIME
      HEB_HFLUX%VERSION_ID     = HEB_EFLUX%VERSION_ID
      HEB_HFLUX%PROD_DATE_TIME = HEB_EFLUX%PROD_DATE_TIME 
!
! --- Close the NetCDF-file
!
      IS = NF_CLOSE ( NCID )
!
      CALL ERR_LOG ( 0, IUER ) 
      RETURN
      END  SUBROUTINE  READ_GEOS_FLUX  !#!#
