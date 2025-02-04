      SUBROUTINE READ_GEOS_O3 ( FILIN, MLON, MLAT, MLEV, O3, HEB, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine READ_GEOS_O3
! *                                                                      *
! * ### 26-MAR-2018   READ_GEOS_O3   v1.0 (c) L. Petrov 26-MAR-2018 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'netcdf.inc'
      INCLUDE   'heb.i'
      TYPE     ( HEB__TYPE ) :: HEB
      CHARACTER  FILIN*(*)
      INTEGER*4  MLON, MLAT, MLEV, IUER
      REAL*4     O3(MLON,MLAT,MLEV)
      INTEGER*4  ID_VAR_LAT, ID_VAR_LON, ID_VAR_LEV, ID_VAR_TIM
      INTEGER*4  ID_DIM_LAT, ID_DIM_LON, ID_DIM_LEV, ID_DIM_TIM
      INTEGER*4  DIMLEN_LAT, DIMLEN_LON, DIMLEN_LEV, DIMLEN_TIM
      INTEGER*4  ID_VAR_O3
      INTEGER*4  J1, J2, J3, J4, IS, NDIMS, NVARS, NATTS, NUNL, ID_VAR(4), &
     &           NCID, IL, IER
      CHARACTER  STR_DAT*16, STR_TIM*16, DATE_STR*32
      LOGICAL*4, EXTERNAL :: IS_R4_NAN 
      CHARACTER, EXTERNAL :: MJDSEC_TO_DATE*30
      INTEGER*4, EXTERNAL :: ILEN, I_LEN
!
! --- Open the NETcdf database
!
      IS = NF_OPEN ( FILIN, NF_NOWRITE, NCID )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 5711, IUER, 'READ_GEOS_O3', &
     &                   'Error in NF_OPEN: '//NF_STRERROR(IS) )
           RETURN
      END IF
!
! --- Learn the number of dimension, variables and attributes
!
      IS = NF_INQ ( NCID, NDIMS, NVARS, NATTS, NUNL )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 5712, IUER, 'READ_GEOS_O3', 'Error in '// &
     &         'an attempt to open input file '// &
     &          FILIN(1:I_LEN(FILIN))//' NF_INQ: '//NF_STRERROR(IS) )
           RETURN
      END IF
!
! --- Learn the ID of the dimension lon
!
      IS = NF_INQ_DIMID ( NCID, 'lon', ID_DIM_LON )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 5713, IUER, 'READ_GEOS_O3', 'Dimension '// &
     &                   '"lon" was not found in the input netcdf file '// &
     &                    FILIN(1:I_LEN(FILIN))//' error: '// &
     &                    NF_STRERROR(IS) )
           RETURN
      END IF
!
! --- Get the length of the dimension longitude
!
      IS = NF_INQ_DIMLEN ( NCID, ID_DIM_LON, DIMLEN_LON  )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 5714, IUER, 'READ_GEOS_O3', 'Error in '// &
     &                   'getting the length of the dimension "lon" in file '// &
     &                    FILIN(1:I_LEN(FILIN))//' error: '// &
     &                    NF_STRERROR(IS) )
           RETURN
      END IF
!
! --- Learn the ID of the dimension lat
!
      IS = NF_INQ_DIMID ( NCID, 'lat', ID_DIM_LAT )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 5715, IUER, 'READ_GEOS_O3', 'Dimension '// &
     &                   '"lat" was not found in the input netcdf file '// &
     &                    FILIN(1:I_LEN(FILIN))//' error: '// &
     &                    NF_STRERROR(IS) )
           RETURN
      END IF
!
! --- Get the length of the dimension latitude
!
      IS = NF_INQ_DIMLEN ( NCID, ID_DIM_LAT, DIMLEN_LAT  )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 5716, IUER, 'READ_GEOS_O3', 'Error in '// &
     &                   'getting the length of the dimension "lat" in file '// &
     &                    FILIN(1:I_LEN(FILIN))//' error: '// &
     &                    NF_STRERROR(IS) )
           RETURN
      END IF
!
! --- Learn the ID of the dimension lev
!
      IS = NF_INQ_DIMID ( NCID, 'lev', ID_DIM_LEV )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 5717, IUER, 'READ_GEOS_O3', 'Dimension '// &
     &                   '"lev" was not found in the input netcdf file '// &
     &                    FILIN(1:I_LEN(FILIN))//' error: '// &
     &                    NF_STRERROR(IS) )
           RETURN
      END IF
!
! --- Get the length of the dimension lev
!
      IS = NF_INQ_DIMLEN ( NCID, ID_DIM_LEV, DIMLEN_LEV  )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 5718, IUER, 'READ_GEOS_O3', 'Error in '// &
     &                   'getting the length of the dimension "lev" in file '// &
     &                    FILIN(1:I_LEN(FILIN))//' error: '// &
     &                    NF_STRERROR(IS) )
           RETURN
      END IF
!
! --- Learn the ID of the dimension time
!
      IS = NF_INQ_DIMID ( NCID, 'time', ID_DIM_TIM )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 5719, IUER, 'READ_GEOS_O3', 'Dimension '// &
     &                   '"time" was not found in the input netcdf file '// &
     &                    FILIN(1:I_LEN(FILIN))//' error: '// &
     &                    NF_STRERROR(IS) )
           RETURN
      END IF
!
! --- Get the length of the dimension time
!
      IS = NF_INQ_DIMLEN ( NCID, ID_DIM_TIM, DIMLEN_TIM  )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 5720, IUER, 'READ_GEOS_O3', 'Error in '// &
     &                   'getting the length of the dimension "tim" in '// &
     &                   'file '//FILIN(1:I_LEN(FILIN))//' error: '// &
     &                    NF_STRERROR(IS) )
           RETURN
      END IF
!
! --- Learn the ID of the variable longitude
!
      IS = NF_INQ_VARID ( NCID, 'lon', ID_VAR_LON )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 5721, IUER, 'READ_GEOS_O3', 'Variable '// &
     &                   '"lon" was not found in the input netcdf file '// &
     &                    FILIN(1:I_LEN(FILIN))//' error: '// &
     &                    NF_STRERROR(IS) )
           RETURN
      END IF
!
! --- Learn the ID of the variable latitude
!
      IS = NF_INQ_VARID ( NCID, 'lat', ID_VAR_LAT )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 5722, IUER, 'READ_GEOS_O3', 'Variable '// &
     &                   '"lat" was not found in the input netcdf file '// &
     &                    FILIN(1:I_LEN(FILIN))//' error: '// &
     &                    NF_STRERROR(IS) )
           RETURN
      END IF
!
! --- Learn the ID of the variable lev
!
      IS = NF_INQ_VARID ( NCID, 'lev', ID_VAR_LEV )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 5723, IUER, 'READ_GEOS_O3', 'Variable '// &
     &                   '"lev" was not found in the input netcdf file '// &
     &                    FILIN(1:I_LEN(FILIN))//' error: '// &
     &                    NF_STRERROR(IS) )
           RETURN
      END IF
!
! --- Learn the ID of the variable time
!
      IS = NF_INQ_VARID ( NCID, 'time', ID_VAR_TIM )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 5724, IUER, 'READ_GEOS_O3', 'Variable '// &
     &                   '"time" was not found in the input netcdf file '// &
     &                    FILIN(1:I_LEN(FILIN))//' error: '// &
     &                    NF_STRERROR(IS) )
           RETURN
      END IF
!
! --- Learn the ID of the requested variable O3
!
      IS = NF_INQ_VARID ( NCID, 'O3', ID_VAR_O3 )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 5725, IUER, 'READ_GEOS_O3', 'Variable '// &
     &                   '"O3" was not found in the input netcdf file '// &
     &                    FILIN(1:I_LEN(FILIN))//' error: '// &
     &                    NF_STRERROR(IS) )
           RETURN
      END IF
!
! --- Get variable DELP
!
      IS = NF_GET_VAR_REAL ( NCID, ID_VAR_O3, O3 )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 5726, IUER, 'READ_GEOS_O3', 'Error in '// &
     &                   'getting the values of the SDS parameter "DELP" '// &
     &                   'from file '//FILIN(1:I_LEN(FILIN))//' error: '// &
     &                    NF_STRERROR(IS) )
           RETURN
      END IF
!
      CALL CLRCH ( HEB%PROD_NAME )
      IS = NF_GET_ATT_TEXT ( NCID, 0, 'LongName', HEB%PROD_NAME )
!      IF ( IS .NE. 0 ) THEN
!           CALL ERR_LOG ( 5737, IUER, 'READ_GEOS_O3', 'Error in '// &
!     &         ' an attempt to read attribute LongName: '// &
!     &          NF_STRERROR(IS) )
!           RETURN
!      END IF
!
      CALL CLRCH ( HEB%HISTORY )
      IS = NF_GET_ATT_TEXT ( NCID, 0, 'History', HEB%HISTORY )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 5738, IUER, 'READ_GEOS_O3', 'Error in '// &
     &         ' an attempt to read attribute History: '// &
     &          NF_STRERROR(IS) )
           RETURN
      END IF
!
      CALL CLRCH ( HEB%SOURCE )
      IS = NF_GET_ATT_TEXT ( NCID, 0, 'Source', HEB%SOURCE )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 5739, IUER, 'READ_GEOS_O3', 'Error in '// &
     &         ' an attempt to read attribute Source: '// &
     &          NF_STRERROR(IS) )
           RETURN
      END IF
      CALL TRAN ( 13, HEB%SOURCE, HEB%SOURCE )
!
      CALL CLRCH ( HEB%TITLE )
      IS = NF_GET_ATT_TEXT ( NCID, 0, 'Title', HEB%TITLE )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 5740, IUER, 'READ_GEOS_O3', 'Error in '// &
     &         ' an attempt to read attribute Title: '// &
     &          NF_STRERROR(IS) )
           RETURN
      END IF
      IF ( ILEN(HEB%PROD_NAME) == 0 ) HEB%PROD_NAME = HEB%TITLE 
!
      CALL CLRCH ( HEB%INSTITUTION )
      IS = NF_GET_ATT_TEXT ( NCID, 0, 'Institution', HEB%INSTITUTION )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 5741, IUER, 'READ_GEOS_O3', 'Error in '// &
     &         ' an attempt to read attribute Institution: '// &
     &          NF_STRERROR(IS) )
           RETURN
      END IF
!
      CALL CLRCH ( HEB%REFERENCES )
      IS = NF_GET_ATT_TEXT ( NCID, 0, 'References', HEB%REFERENCES )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 5742, IUER, 'READ_GEOS_O3', 'Error in '// &
     &         ' an attempt to read attribute References: '// &
     &          NF_STRERROR(IS) )
           RETURN
      END IF
!
      CALL CLRCH ( HEB%VERSION_ID )
      IS = NF_GET_ATT_TEXT ( NCID, 0, 'VersionID', HEB%VERSION_ID )
      IF ( IS == 0 ) IS = NF_GET_ATT_TEXT ( NCID, 0, 'NCO', HEB%VERSION_ID )
      
!      IF ( IS .NE. 0 ) THEN
!           CALL ERR_LOG ( 5743, IUER, 'READ_GEOS_O3', 'Error in '// &
!     &         ' an attempt to read attribute VersionID: '// &
!     &          NF_STRERROR(IS) )
!           RETURN
!      END IF
      IF ( ILEN(HEB%VERSION_ID) == 0 ) THEN
           HEB%VERSION_ID = "0.0"
         ELSE 
!
! -------- Replace unreadable characters, if any, with blanks
!
           CALL TRAN ( 13, HEB%VERSION_ID(1:ILEN(HEB%VERSION_ID)), &
     &                     HEB%VERSION_ID(1:ILEN(HEB%VERSION_ID))  )
      END IF
!
      CALL CLRCH ( HEB%FILE_NAME )
      IS = NF_GET_ATT_TEXT ( NCID, 0, 'Filename', HEB%FILE_NAME )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 5743, IUER, 'READ_GEOS_O3', 'Error in '// &
     &         ' an attempt to read attribute Filename: '// &
     &          NF_STRERROR(IS) )
           RETURN
      END IF
!
      CALL CLRCH ( HEB%PROD_DATE_TIME )
      IS = NF_GET_ATT_TEXT ( NCID, 0, 'ProductionDateTime', HEB%PROD_DATE_TIME )
!      IF ( IS .NE. 0 ) THEN
!           CALL ERR_LOG ( 5743, IUER, 'READ_GEOS_O3', 'Error in '// &
!     &         ' an attempt to read attribute ProductionDateTime: '// &
!     &          NF_STRERROR(IS) )
!           RETURN
!      END IF
!
      CALL CLRCH ( STR_DAT )
      IS = NF_GET_ATT_TEXT ( NCID, 0, 'RangeBeginningDate', STR_DAT )
!      IF ( IS .NE. 0 ) THEN
!           CALL ERR_LOG ( 5744, IUER, 'READ_GEOS_O3', 'Error in '// &
!     &         ' an attempt to read attribute RangeBeginningDate: '// &
!     &          NF_STRERROR(IS) )
!           RETURN
!      END IF
!
      CALL CLRCH ( STR_TIM )
      IS = NF_GET_ATT_TEXT ( NCID, 0, 'RangeBeginningTime', STR_TIM )
!      IF ( IS .NE. 0 ) THEN
!           CALL ERR_LOG ( 5745, IUER, 'READ_GEOS_O3', 'Error in '// &
!     &         ' an attempt to read attribute RangeBeginningTime: '// &
!     &          NF_STRERROR(IS) )
!           RETURN
!      END IF
      IF ( ILEN(STR_DAT) == 0 ) THEN
           IL = ILEN(HEB%FILE_NAME)
           STR_DAT = HEB%FILE_NAME(IL-20:IL-17)//'.'// &
     &               HEB%FILE_NAME(IL-16:IL-15)//'.'// &
     &               HEB%FILE_NAME(IL-14:IL-13)
           STR_TIM = HEB%FILE_NAME(IL-11:IL-10)//':'// &
     &               HEB%FILE_NAME(IL-9:IL-8)//':00.0'
      END IF
!
      DATE_STR = STR_DAT(1:I_LEN(STR_DAT))//'_'//STR_TIM(1:I_LEN(STR_TIM))
      CALL ERR_PASS ( IUER, IER )
      CALL DATE_TO_TIME ( DATE_STR, HEB%MJD, HEB%UTC, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 5746, IUER, 'READ_GEOS_O3', 'Error in '// &
     &         ' an attempt to decode beginning date '//DATE_STR )
           RETURN
      END IF
!
! --- Close the NetCDF-file
!
      IS = NF_CLOSE ( NCID )
      CALL ERR_LOG  ( 0, IUER )
      RETURN
      END  SUBROUTINE  READ_GEOS_O3  !#!#
