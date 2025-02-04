      SUBROUTINE READ_GEOS_SLV ( FILIN, HEB_U250, HEB_V250, HEB_CLDPRS, &
     &                           HEB_CLDTMP, TIM_STEP, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine READ_GEOS_SLV
! *                                                                      *
! * ### 06-MAY-2013  READ_GEOS_SLV  v1.0  (c) L. Petrov 06-MAY-2013  ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'netcdf.inc'
      INCLUDE   'heb.i'
      INTEGER*4  IUER
      CHARACTER  FILIN*128
      TYPE     ( HEB__TYPE ) :: HEB_U250, HEB_V250, HEB_CLDPRS, HEB_CLDTMP
      REAL*8     TIM_STEP
      INTEGER*4  IS, NCID,   DIMLEN_LAT, DIMLEN_LON, DIMLEN_TIM, &
     &           ID_DIM_LON, ID_DIM_LAT, ID_DIM_TIM, VECDIM(3),  &
     &           ID_VAR_LAT, ID_VAR_LON, ID_VAR_TIM, &
     &           ID_VAR_U250, ID_VAR_V250, ID_VAR_CLDTMP, ID_VAR_CLDPRS 
      INTEGER*4    M_DIM
      PARAMETER  ( M_DIM = 8192 )
      REAL*4     ARR_ALONG_LON(M_DIM)
      REAL*8     TIM_BEG, TIM_END, LAT, G_ACC
      REAL*8,    ALLOCATABLE :: DAT_MIN(:)
      CHARACTER  STR*128, STR_DAT*32, STR_TIM*32, DATE_STR*32
      INTEGER*4  J1, J2, J3, MJD_BEG, MJD_END, INT4_STR_DAT, ID, IP, &
     &           IND_LON, IER
      INTEGER*4, EXTERNAL :: ILEN, I_LEN, LINDEX
!
      HEB_U250%STATUS = HEB__UNDF
!
! --- Open the new ouput file in netcdf format
!
      IS = NF_OPEN ( FILIN, NF_NOWRITE, NCID )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 7321, IUER, 'READ_GEOS_SLV', 'Error in an '// &
     &         'attempt to open the netcf file with SLV diagnistics '// &
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
           CALL ERR_LOG ( 7322, IUER, 'READ_GEOS_SLV', 'Error in '// &
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
           CALL ERR_LOG ( 7323, IUER, 'READ_GEOS_SLV', 'Error in '// &
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
           CALL ERR_LOG ( 7324, IUER, 'READ_GEOS_SLV', 'Error in '// &
     &         ' an attempt to read dimension tim: '// &
     &          NF_STRERROR(IS) )
           RETURN
      END IF
!
! --- Get the length of the dimension "lon"
!
      IS = NF_INQ_DIMLEN ( NCID, ID_DIM_LON, DIMLEN_LON  )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 7325, IUER, 'READ_GEOS_SLV', 'Error in '// &
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
           CALL ERR_LOG ( 7326, IUER, 'READ_GEOS_SLV', 'Error in '// &
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
           CALL ERR_LOG ( 7327, IUER, 'READ_GEOS_SLV', 'Error in '// &
     &         'getting the length of the dimension "tim" in file '// &
     &          FILIN(1:I_LEN(FILIN))//' error: '// &
     &          NF_STRERROR(IS) )
           RETURN
      END IF
!
! --- Allocate memory for U250 variable
!
      HEB_U250%DIMS(1) = DIMLEN_LON
      HEB_U250%DIMS(2) = DIMLEN_LAT
      HEB_U250%DIMS(3) = 1
      HEB_U250%DIMS(4) = DIMLEN_TIM
      ALLOCATE ( HEB_U250%VAL(HEB_U250%DIMS(1),HEB_U250%DIMS(2),HEB_U250%DIMS(3),HEB_U250%DIMS(4)), &
     &           STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH  ( STR )
           CALL IINCH8 ( INT8(4)*HEB_U250%DIMS(1)*HEB_U250%DIMS(2)*HEB_U250%DIMS(3)*HEB_U250%DIMS(4), &
     &                   STR )
           CALL ERR_LOG ( 7328, IUER, 'READ_GEOS_SLV', 'Failure in '// &
     &         'an attempt to allocate '//STR(1:I_LEN(STR))// &
     &         ' bytes of memory for array U250' )
           RETURN
      END IF
      HEB_U250%STATUS = HEB__ALLO
!
! --- Learn the ID of the variable "U250"
!
      IS = NF_INQ_VARID ( NCID, 'U250', ID_VAR_U250 )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 7330, IUER, 'READ_GEOS_SLV', 'Variable '// &
     &         '"U250" was not found in the input netcdf file '// &
     &          FILIN(1:I_LEN(FILIN))//' error: '//NF_STRERROR(IS) )
           RETURN
      END IF
!
! --- Read array with U250 data
!
      IS = NF_GET_VAR_REAL ( NCID, ID_VAR_U250, HEB_U250%VAL )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 7331, IUER, 'READ_GEOS_SLV', 'Error in '// &
     &         'getting the values of the variable "U250" from the file '// &
     &          FILIN(1:I_LEN(FILIN))//' error: '// &
     &          NF_STRERROR(IS) )
           RETURN
      END IF
      HEB_U250%STATUS = HEB__LOAD
!
! --- Allocate memory for V250 variable
!
      HEB_V250%DIMS(1) = DIMLEN_LON
      HEB_V250%DIMS(2) = DIMLEN_LAT
      HEB_V250%DIMS(3) = 1
      HEB_V250%DIMS(4) = DIMLEN_TIM
      ALLOCATE ( HEB_V250%VAL(HEB_V250%DIMS(1),HEB_V250%DIMS(2),HEB_V250%DIMS(3),HEB_V250%DIMS(4)), &
     &           STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH  ( STR )
           CALL IINCH8 ( INT8(4)*HEB_V250%DIMS(1)*HEB_V250%DIMS(2)*HEB_V250%DIMS(3)*HEB_V250%DIMS(4), &
     &                   STR )
           CALL ERR_LOG ( 7328, IUER, 'READ_GEOS_SLV', 'Failure in '// &
     &         'an attempt to allocate '//STR(1:I_LEN(STR))// &
     &         ' bytes of memory for array V250' )
           RETURN
      END IF
      HEB_V250%STATUS = HEB__ALLO
!
! --- Learn the ID of the variable "V250"
!
      IS = NF_INQ_VARID ( NCID, 'V250', ID_VAR_V250 )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 7330, IUER, 'READ_GEOS_SLV', 'Variable '// &
     &         '"V250" was not found in the input netcdf file '// &
     &          FILIN(1:I_LEN(FILIN))//' error: '//NF_STRERROR(IS) )
           RETURN
      END IF
!
! --- Read array with V250 data
!
      IS = NF_GET_VAR_REAL ( NCID, ID_VAR_V250, HEB_V250%VAL )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 7331, IUER, 'READ_GEOS_SLV', 'Error in '// &
     &         'getting the values of the variable "V250" from the file '// &
     &          FILIN(1:I_LEN(FILIN))//' error: '// &
     &          NF_STRERROR(IS) )
           RETURN
      END IF
      HEB_V250%STATUS = HEB__LOAD
!
! --- Allocate memory for CLDPRS variable
!
      HEB_CLDPRS%DIMS(1) = DIMLEN_LON
      HEB_CLDPRS%DIMS(2) = DIMLEN_LAT
      HEB_CLDPRS%DIMS(3) = 1
      HEB_CLDPRS%DIMS(4) = DIMLEN_TIM
      ALLOCATE ( HEB_CLDPRS%VAL(HEB_CLDPRS%DIMS(1),HEB_CLDPRS%DIMS(2),HEB_CLDPRS%DIMS(3),HEB_CLDPRS%DIMS(4)), &
     &           STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH  ( STR )
           CALL IINCH8 ( INT8(4)*HEB_CLDPRS%DIMS(1)*HEB_CLDPRS%DIMS(2)*HEB_CLDPRS%DIMS(3)*HEB_CLDPRS%DIMS(4), &
     &                   STR )
           CALL ERR_LOG ( 7328, IUER, 'READ_GEOS_SLV', 'Failure in '// &
     &         'an attempt to allocate '//STR(1:I_LEN(STR))// &
     &         ' bytes of memory for array CLDPRS' )
           RETURN
      END IF
      HEB_CLDPRS%STATUS = HEB__ALLO
!
! --- Learn the ID of the variable "CLDPRS"
!
      IS = NF_INQ_VARID ( NCID, 'CLDPRS', ID_VAR_CLDPRS )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 7330, IUER, 'READ_GEOS_SLV', 'Variable '// &
     &         '"CLDPRS" was not found in the input netcdf file '// &
     &          FILIN(1:I_LEN(FILIN))//' error: '//NF_STRERROR(IS) )
           RETURN
      END IF
!
! --- Read array with CLDPRS data
!
      IS = NF_GET_VAR_REAL ( NCID, ID_VAR_CLDPRS, HEB_CLDPRS%VAL )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 7331, IUER, 'READ_GEOS_SLV', 'Error in '// &
     &         'getting the values of the variable "CLDPRS" from the file '// &
     &          FILIN(1:I_LEN(FILIN))//' error: '// &
     &          NF_STRERROR(IS) )
           RETURN
      END IF
      HEB_CLDPRS%STATUS = HEB__LOAD
!
! --- Allocate memory for CLDTMP variable
!
      HEB_CLDTMP%DIMS(1) = DIMLEN_LON
      HEB_CLDTMP%DIMS(2) = DIMLEN_LAT
      HEB_CLDTMP%DIMS(3) = 1
      HEB_CLDTMP%DIMS(4) = DIMLEN_TIM
      ALLOCATE ( HEB_CLDTMP%VAL(HEB_CLDTMP%DIMS(1),HEB_CLDTMP%DIMS(2),HEB_CLDTMP%DIMS(3),HEB_CLDTMP%DIMS(4)), &
     &           STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH  ( STR )
           CALL IINCH8 ( INT8(4)*HEB_CLDTMP%DIMS(1)*HEB_CLDTMP%DIMS(2)*HEB_CLDTMP%DIMS(3)*HEB_CLDTMP%DIMS(4), &
     &                   STR )
           CALL ERR_LOG ( 7328, IUER, 'READ_GEOS_SLV', 'Failure in '// &
     &         'an attempt to allocate '//STR(1:I_LEN(STR))// &
     &         ' bytes of memory for array CLDTMP' )
           RETURN
      END IF
      HEB_CLDTMP%STATUS = HEB__ALLO
!
! --- Learn the ID of the variable "CLDTMP"
!
      IS = NF_INQ_VARID ( NCID, 'CLDTMP', ID_VAR_CLDTMP )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 7330, IUER, 'READ_GEOS_SLV', 'Variable '// &
     &         '"CLDTMP" was not found in the input netcdf file '// &
     &          FILIN(1:I_LEN(FILIN))//' error: '//NF_STRERROR(IS) )
           RETURN
      END IF
!
! --- Read array with CLDTMP data
!
      IS = NF_GET_VAR_REAL ( NCID, ID_VAR_CLDTMP, HEB_CLDTMP%VAL )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 7331, IUER, 'READ_GEOS_SLV', 'Error in '// &
     &         'getting the values of the variable "CLDTMP" from the file '// &
     &          FILIN(1:I_LEN(FILIN))//' error: '// &
     &          NF_STRERROR(IS) )
           RETURN
      END IF
      HEB_CLDTMP%STATUS = HEB__LOAD
!
      CALL CLRCH ( HEB_U250%HISTORY )
      IS = NF_GET_ATT_TEXT ( NCID, 0, 'History', HEB_U250%HISTORY )
      IF ( IS .NE. 0 ) THEN
           IS = NF_GET_ATT_TEXT ( NCID, 0, 'history', HEB_U250%HISTORY )
      END IF
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 7335, IUER, 'READ_GEOS_SLV', 'Error in '// &
     &         ' an attempt to read attribute History: '// &
     &          NF_STRERROR(IS) )
           RETURN
      END IF
!
      CALL CLRCH ( HEB_U250%SOURCE )
      IS = NF_GET_ATT_TEXT ( NCID, 0, 'Source', HEB_U250%SOURCE )
      IF ( IS .NE. 0 ) THEN
           IS = NF_GET_ATT_TEXT ( NCID, 0, 'source', HEB_U250%SOURCE )
      END IF
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 7336, IUER, 'READ_GEOS_SLV', 'Error in '// &
     &         ' an attempt to read attribute Source: '// &
     &          NF_STRERROR(IS) )
           RETURN
      END IF
      CALL TRAN ( 13, HEB_U250%SOURCE, HEB_U250%SOURCE )
!
      CALL CLRCH ( HEB_U250%TITLE )
      IS = NF_GET_ATT_TEXT ( NCID, 0, 'Title', HEB_U250%TITLE )
      IF ( IS .NE. 0 ) THEN
           IS = NF_GET_ATT_TEXT ( NCID, 0, 'title', HEB_U250%TITLE )
      END IF
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 7337, IUER, 'READ_GEOS_SLV', 'Error in '// &
     &         ' an attempt to read attribute Title: '// &
     &          NF_STRERROR(IS) )
           RETURN
      END IF
!
      CALL CLRCH ( HEB_U250%INSTITUTION )
      IS = NF_GET_ATT_TEXT ( NCID, 0, 'Institution', HEB_U250%INSTITUTION )
      IF ( IS .NE. 0 ) THEN
           IS = NF_GET_ATT_TEXT ( NCID, 0, 'institution', HEB_U250%INSTITUTION )
      END IF
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 7338, IUER, 'READ_GEOS_SLV', 'Error in '// &
     &         ' an attempt to read attribute Institution: '// &
     &          NF_STRERROR(IS) )
           RETURN
      END IF
!
      CALL CLRCH ( HEB_U250%REFERENCES )
      IS = NF_GET_ATT_TEXT ( NCID, 0, 'References', HEB_U250%REFERENCES )
      IF ( IS .NE. 0 ) THEN
           IS = NF_GET_ATT_TEXT ( NCID, 0, 'references', HEB_U250%REFERENCES )
      END IF
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 7339, IUER, 'READ_GEOS_SLV', 'Error in '// &
     &         ' an attempt to read attribute References: '// &
     &          NF_STRERROR(IS) )
           RETURN
      END IF
!
      CALL CLRCH ( HEB_U250%VERSION_ID )
      IS = NF_GET_ATT_TEXT ( NCID, 0, 'VersionID', HEB_U250%VERSION_ID )
      IF ( IS .NE. 0 ) THEN
           IS = NF_GET_ATT_TEXT ( NCID, 0, 'comment', HEB_U250%VERSION_ID )
      END IF
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 7340, IUER, 'READ_GEOS_SLV', 'Error in '// &
     &         ' an attempt to read attribute VersionID: '// &
     &          NF_STRERROR(IS) )
           RETURN
      END IF
!
      CALL CLRCH ( HEB_U250%FILE_NAME )
      IS = NF_GET_ATT_TEXT ( NCID, 0, 'Filename', HEB_U250%FILE_NAME )
      IF ( IS .NE. 0 ) THEN
           ID = LINDEX ( FILIN, '/' ) 
           HEB_U250%FILE_NAME = FILIN(ID:)
      END IF
!
      CALL CLRCH ( HEB_U250%PROD_DATE_TIME )
      IS = NF_GET_ATT_TEXT ( NCID, 0, 'ProductionDateTime', HEB_U250%PROD_DATE_TIME )
      IF ( IS .NE. 0 ) THEN
           HEB_U250%PROD_DATE_TIME = 'n/a'
      END IF
!
      IF ( DIMLEN_TIM == 1 ) THEN
           CALL CLRCH ( HEB_U250%PROD_NAME )
           IS = NF_GET_ATT_TEXT ( NCID, 0, 'LongName', HEB_U250%PROD_NAME )
           IF ( IS .NE. 0 ) THEN
                IS = NF_GET_ATT_TEXT ( NCID, 0, 'long_name', HEB_U250%PROD_NAME )
           END IF
           IF ( IS .NE. 0 ) THEN
                CALL ERR_LOG ( 7334, IUER, 'READ_GEOS_SLV', 'Error in '// &
          &         ' an attempt to read attribute LongName: '// &
          &          NF_STRERROR(IS) )
                RETURN
           END IF
!
           CALL CLRCH ( STR_DAT )
           IS = NF_GET_ATT_TEXT ( NCID, 0, 'RangeBeginningDate', STR_DAT )
           IF ( IS .NE. 0 ) THEN
                CALL ERR_LOG ( 7341, IUER, 'READ_GEOS_SLV', 'Error in '// &
     &              ' an attempt to read attribute RangeBeginningDate: '// &
     &               NF_STRERROR(IS) )
                RETURN
           END IF
!
           CALL CLRCH ( STR_TIM )
           IS = NF_GET_ATT_TEXT ( NCID, 0, 'RangeBeginningTime', STR_TIM )
           IF ( IS .NE. 0 ) THEN
                CALL ERR_LOG ( 7342, IUER, 'READ_GEOS_SLV', 'Error in '// &
     &              ' an attempt to read attribute RangeBeginningTime: '// &
     &               NF_STRERROR(IS) )
                RETURN
           END IF
!
           DATE_STR = STR_DAT(1:I_LEN(STR_DAT))//'_'//STR_TIM(1:I_LEN(STR_TIM))
           CALL ERR_PASS ( IUER, IER )
           CALL DATE_TO_TIME ( DATE_STR, HEB_U250%MJD, HEB_U250%UTC, IER )
           IF ( IS .NE. 0 ) THEN
                CALL ERR_LOG ( 7343, IUER, 'READ_GEOS_SLV', 'Error in '// &
     &              ' an attempt to decode beginning date '//DATE_STR )
                RETURN
           END IF
!
           CALL CLRCH ( STR_DAT )
           IS = NF_GET_ATT_TEXT ( NCID, 0, 'RangeEndingDate', STR_DAT )
           IF ( IS .NE. 0 ) THEN
                CALL ERR_LOG ( 7344, IUER, 'READ_GEOS_SLV', 'Error in '// &
     &              ' an attempt to read attribute RangeBeginningDate: '// &
     &               NF_STRERROR(IS) )
                RETURN
           END IF
!
           CALL CLRCH ( STR_TIM )
           IS = NF_GET_ATT_TEXT ( NCID, 0, 'RangeEndingTime', STR_TIM )
           IF ( IS .NE. 0 ) THEN
                CALL ERR_LOG ( 7345, IUER, 'READ_GEOS_SLV', 'Error in '// &
     &              ' an attempt to read attribute RangeBeginningTime: '// &
     &               NF_STRERROR(IS) )
                RETURN
           END IF
!
           DATE_STR = STR_DAT(1:I_LEN(STR_DAT))//'_'//STR_TIM(1:I_LEN(STR_TIM))
           CALL ERR_PASS ( IUER, IER )
           CALL DATE_TO_TIME ( DATE_STR, MJD_END, TIM_END, IER )
           IF ( IS .NE. 0 ) THEN
                CALL ERR_LOG ( 7346, IUER, 'READ_GEOS_SLV', 'Error in '// &
     &              ' an attempt to decode beginning date '//DATE_STR )
                RETURN
           END IF
!
           HEB_U250%UTC = (HEB_U250%UTC + (MJD_END - HEB_U250%MJD)*86400.0D0 + TIM_END)/2.0D0
           IF ( HEB_U250%UTC > 86400.0D0 ) THEN
                HEB_U250%UTC = HEB_U250%UTC - 86400.0D0
                HEB_U250%MJD = HEB_U250%MJD + 1
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
                CALL ERR_LOG ( 7330, IUER, 'READ_GEOS_SLV', 'Variable '// &
     &              '"time" was not found in the input netcdf file '// &
     &              FILIN(1:I_LEN(FILIN))//' error: '//NF_STRERROR(IS) )
                RETURN
           END IF
!
           ALLOCATE ( DAT_MIN(DIMLEN_TIM), STAT=IER )
           IF ( IER .NE. 0 ) THEN
                CALL CLRCH ( STR )
                CALL IINCH ( 8*DIMLEN_TIM, STR )
                CALL ERR_LOG ( 7329, IUER, 'READ_GEOS_SLV', 'Failure in '// &
     &              'an attempt to allocate '//STR(1:I_LEN(STR))// &
     &              ' bytes of memory for array DAT_MIN' )
               RETURN
           END IF
!
           CALL CLRCH ( HEB_U250%PROD_NAME )
           IP = LINDEX ( FILIN, '/' ) + 1
           ID = LINDEX ( FILIN, '.' ) - 1
           IF ( ID < IP ) ID = IP
           ID = LINDEX ( FILIN(1:ID), '.' ) - 1
           IF ( ID < IP ) ID = IP
           HEB_U250%PROD_NAME  = FILIN(IP:ID)
!
           IS = NF_GET_ATT_INT  ( NCID, ID_VAR_TIM, 'begin_date', INT4_STR_DAT )
           IF ( IS .NE. 0 ) THEN
                CALL ERR_LOG ( 7347, IUER, 'READ_GEOS_SLV', 'Error in '// &
     &              ' an attempt to read attribute begin_date: '// &
     &               NF_STRERROR(IS) )
                RETURN
           END IF
!
! -------- Get variable DAT_MIN
!
           IS = NF_GET_VAR_REAL ( NCID, ID_VAR_TIM, DAT_MIN )
           IF ( IS .NE. 0 ) THEN
                CALL ERR_LOG ( 7348, IUER, 'READ_GEOS_SLV', 'Error in '// &
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
           CALL DATE_TO_TIME ( DATE_STR, HEB_U250%MJD, HEB_U250%UTC, IER )
           IF ( IS .NE. 0 ) THEN
                CALL ERR_LOG ( 7351, IUER, 'READ_GEOS_SLV', 'Error in '// &
     &              ' an attempt to decode beginning date '//DATE_STR )
                RETURN
           END IF
!
           CALL CLRCH ( HEB_U250%PROD_DATE_TIME )
           HEB_U250%PROD_DATE_TIME = 'not_available'
!
           DEALLOCATE ( DAT_MIN )
      END IF
!
      HEB_V250%MJD       = HEB_U250%MJD 
      HEB_V250%UTC       = HEB_U250%UTC 
      HEB_V250%PROD_NAME = HEB_U250%PROD_NAME
      HEB_V250%FILE_NAME = HEB_U250%FILE_NAME
      HEB_V250%HISTORY   = HEB_U250%HISTORY 
      HEB_V250%SOURCE    = HEB_U250%SOURCE
      HEB_V250%TITLE     = HEB_U250%TITLE
      HEB_V250%INSTITUTION    = HEB_U250%INSTITUTION
      HEB_V250%REFERENCES     = HEB_U250%REFERENCES
      HEB_V250%PROD_DATE_TIME = HEB_U250%PROD_DATE_TIME
      HEB_V250%VERSION_ID     = HEB_U250%VERSION_ID
      HEB_V250%PROD_DATE_TIME = HEB_U250%PROD_DATE_TIME 
!
      HEB_CLDPRS%MJD       = HEB_U250%MJD 
      HEB_CLDPRS%UTC       = HEB_U250%UTC 
      HEB_CLDPRS%PROD_NAME = HEB_U250%PROD_NAME
      HEB_CLDPRS%FILE_NAME = HEB_U250%FILE_NAME
      HEB_CLDPRS%HISTORY   = HEB_U250%HISTORY 
      HEB_CLDPRS%SOURCE    = HEB_U250%SOURCE
      HEB_CLDPRS%TITLE     = HEB_U250%TITLE
      HEB_CLDPRS%INSTITUTION    = HEB_U250%INSTITUTION
      HEB_CLDPRS%REFERENCES     = HEB_U250%REFERENCES
      HEB_CLDPRS%PROD_DATE_TIME = HEB_U250%PROD_DATE_TIME
      HEB_CLDPRS%VERSION_ID     = HEB_U250%VERSION_ID
      HEB_CLDPRS%PROD_DATE_TIME = HEB_U250%PROD_DATE_TIME 
!
      HEB_CLDTMP%MJD       = HEB_U250%MJD 
      HEB_CLDTMP%UTC       = HEB_U250%UTC 
      HEB_CLDTMP%PROD_NAME = HEB_U250%PROD_NAME
      HEB_CLDTMP%FILE_NAME = HEB_U250%FILE_NAME
      HEB_CLDTMP%HISTORY   = HEB_U250%HISTORY 
      HEB_CLDTMP%SOURCE    = HEB_U250%SOURCE
      HEB_CLDTMP%TITLE     = HEB_U250%TITLE
      HEB_CLDTMP%INSTITUTION    = HEB_U250%INSTITUTION
      HEB_CLDTMP%REFERENCES     = HEB_U250%REFERENCES
      HEB_CLDTMP%PROD_DATE_TIME = HEB_U250%PROD_DATE_TIME
      HEB_CLDTMP%VERSION_ID     = HEB_U250%VERSION_ID
      HEB_CLDTMP%PROD_DATE_TIME = HEB_U250%PROD_DATE_TIME 
!
! --- Close the NetCDF-file
!
      IS = NF_CLOSE ( NCID )
!
      CALL ERR_LOG ( 0, IUER ) 
      RETURN
      END  SUBROUTINE  READ_GEOS_SLV  !#!#
