      SUBROUTINE READ_GEOS_DQGTUV ( FILIN, MLON, MLAT, MLEV, &
     &                                  DELP, PHIS, QV, T, U, V, HEB, &
     &                                  IUER )
! ************************************************************************
! *                                                                      *
! *   Routine READ_GEOS_DQGTUV
! *                                                                      *
! * ### 27-JAN-2013  READ_GEOS_DQGTUV v1.1 (c) L. Petrov 03-FEB-2024 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'netcdf.inc'
      INCLUDE   'heb.i'
      TYPE     ( HEB__TYPE ) :: HEB
      CHARACTER  FILIN*(*)
      INTEGER*4  MLON, MLAT, MLEV, IUER
      REAL*4     DELP(MLON,MLAT,MLEV), PHIS(MLON,MLAT),     &
     &           QV(MLON,MLAT,MLEV),   T(MLON,MLAT,MLEV),   &
     &           U(MLON,MLAT,MLEV),    V(MLON,MLAT,MLEV)
      INTEGER*4  ID_VAR_LAT, ID_VAR_LON, ID_VAR_LEV, ID_VAR_TIM
      INTEGER*4  ID_DIM_LAT, ID_DIM_LON, ID_DIM_LEV, ID_DIM_TIM
      INTEGER*4  DIMLEN_LAT, DIMLEN_LON, DIMLEN_LEV, DIMLEN_TIM
      INTEGER*4  ID_VAR_DELP, ID_VAR_PHIS, ID_VAR_QV, ID_VAR_T, &
     &           ID_VAR_U, ID_VAR_V, BEGIN_DATE_I4, BEGIN_TIME_I4
      INTEGER*4  J1, J2, J3, J4, IS, NDIMS, NVARS, NATTS, NUNL, ID_VAR(4), &
     &           NCID, IL, ID, IER
      CHARACTER  STR_DAT*16, STR_TIM*16, DATE_STR*32
      LOGICAL*4, EXTERNAL :: IS_R4_NAN 
      CHARACTER, EXTERNAL :: MJDSEC_TO_DATE*30
      INTEGER*4, EXTERNAL :: ILEN, I_LEN, LINDEX
!
! --- Open the NETcdf database
!
      IS = NF_OPEN ( FILIN, NF_NOWRITE, NCID )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 5711, IUER, 'READ_GEOS_DQGTUV', &
     &                   'Error in NF_OPEN: '//NF_STRERROR(IS) )
           RETURN
      END IF
!
! --- Learn the number of dimension, variables and attributes
!
      IS = NF_INQ ( NCID, NDIMS, NVARS, NATTS, NUNL )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 5712, IUER, 'READ_GEOS_DQGTUV', 'Error in '// &
     &         'an attempt to open input file '// &
     &          FILIN(1:I_LEN(FILIN))//' NF_INQ: '//NF_STRERROR(IS) )
           RETURN
      END IF
!
! --- Learn the ID of the dimension lon
!
      IS = NF_INQ_DIMID ( NCID, 'lon', ID_DIM_LON )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 5713, IUER, 'READ_GEOS_DQGTUV', 'Dimension '// &
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
           CALL ERR_LOG ( 5714, IUER, 'READ_GEOS_DQGTUV', 'Error in '// &
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
           CALL ERR_LOG ( 5715, IUER, 'READ_GEOS_DQGTUV', 'Dimension '// &
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
           CALL ERR_LOG ( 5716, IUER, 'READ_GEOS_DQGTUV', 'Error in '// &
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
           CALL ERR_LOG ( 5717, IUER, 'READ_GEOS_DQGTUV', 'Dimension '// &
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
           CALL ERR_LOG ( 5718, IUER, 'READ_GEOS_DQGTUV', 'Error in '// &
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
           CALL ERR_LOG ( 5719, IUER, 'READ_GEOS_DQGTUV', 'Dimension '// &
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
           CALL ERR_LOG ( 5720, IUER, 'READ_GEOS_DQGTUV', 'Error in '// &
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
           CALL ERR_LOG ( 5721, IUER, 'READ_GEOS_DQGTUV', 'Variable '// &
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
           CALL ERR_LOG ( 5722, IUER, 'READ_GEOS_DQGTUV', 'Variable '// &
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
           CALL ERR_LOG ( 5723, IUER, 'READ_GEOS_DQGTUV', 'Variable '// &
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
           CALL ERR_LOG ( 5724, IUER, 'READ_GEOS_DQGTUV', 'Variable '// &
     &                   '"time" was not found in the input netcdf file '// &
     &                    FILIN(1:I_LEN(FILIN))//' error: '// &
     &                    NF_STRERROR(IS) )
           RETURN
      END IF
!
! --- Learn the ID of the requested variable DELP
!
      IS = NF_INQ_VARID ( NCID, 'DELP', ID_VAR_DELP )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 5725, IUER, 'READ_GEOS_DQGTUV', 'Variable '// &
     &                   '"DELP" was not found in the input netcdf file '// &
     &                    FILIN(1:I_LEN(FILIN))//' error: '// &
     &                    NF_STRERROR(IS) )
           RETURN
      END IF
!
! --- Get variable DELP
!
      IS = NF_GET_VAR_REAL ( NCID, ID_VAR_DELP, DELP )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 5726, IUER, 'READ_GEOS_DQGTUV', 'Error in '// &
     &                   'getting the values of the SDS parameter "DELP" '// &
     &                   'from file '//FILIN(1:I_LEN(FILIN))//' error: '// &
     &                    NF_STRERROR(IS) )
           RETURN
      END IF
!
! --- Learn the ID of the requested variable PHIS
!
      IS = NF_INQ_VARID ( NCID, 'PHIS', ID_VAR_PHIS )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 5727, IUER, 'READ_GEOS_DQGTUV', 'Variable '// &
     &                   '"PHIS" was not found in the input netcdf file '// &
     &                    FILIN(1:I_LEN(FILIN))//' error: '// &
     &                    NF_STRERROR(IS) )
           RETURN
      END IF
!
      IS = NF_GET_VAR_REAL ( NCID, ID_VAR_PHIS, PHIS )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 5728, IUER, 'READ_GEOS_DQGTUV', 'Error in '// &
     &                   'getting the values of the SDS parameter "PHIS" '// &
     &                   'from file '//FILIN(1:I_LEN(FILIN))//' error: '// &
     &                    NF_STRERROR(IS) )
           RETURN
      END IF
!
! --- Learn ID of variable QV
!
      IS = NF_INQ_VARID ( NCID, 'QV', ID_VAR_QV )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 5729, IUER, 'READ_GEOS_DQGTUV', 'Variable '// &
     &                   '"QV" was not found in the input netcdf file '// &
     &                    FILIN(1:I_LEN(FILIN))//' error: '// &
     &                    NF_STRERROR(IS) )
           RETURN
      END IF
!
! --- Get variable QV
!
      IS = NF_GET_VAR_REAL ( NCID, ID_VAR_QV, QV )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 5730, IUER, 'READ_GEOS_DQGTUV', 'Error in '// &
     &                   'getting the values of the SDS parameter "QV" '// &
     &                   'from file '//FILIN(1:I_LEN(FILIN))//' error: '// &
     &                    NF_STRERROR(IS) )
           RETURN
      END IF
!
! --- Learn the ID of the requested variable T
!
      IS = NF_INQ_VARID ( NCID, 'T', ID_VAR_T )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 5731, IUER, 'READ_GEOS_DQGTUV', 'Variable '// &
     &                   '"T" was not found in the input netcdf file '// &
     &                    FILIN(1:I_LEN(FILIN))//' error: '// &
     &                    NF_STRERROR(IS) )
           RETURN
      END IF
!
! --- Get variable T
!
      IS = NF_GET_VAR_REAL ( NCID, ID_VAR_T, T )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 5732, IUER, 'READ_GEOS_DQGTUV', 'Error in '// &
     &                   'getting the values of the SDS parameter "T" '// &
     &                   'from file '//FILIN(1:I_LEN(FILIN))//' error: '// &
     &                    NF_STRERROR(IS) )
           RETURN
      END IF
!
! --- Learn the ID of the requested variable U
!
      IS = NF_INQ_VARID ( NCID, 'U', ID_VAR_U )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 5733, IUER, 'READ_GEOS_DQGTUV', 'Variable '// &
     &                   '"U" was not found in the input netcdf file '// &
     &                    FILIN(1:I_LEN(FILIN))//' error: '// &
     &                    NF_STRERROR(IS) )
           RETURN
      END IF
!
! --- Get variable U
!
      IS = NF_GET_VAR_REAL ( NCID, ID_VAR_U, U )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 5734, IUER, 'READ_GEOS_DQGTUV', 'Error in '// &
     &                   'getting the values of the SDS parameter "U" '// &
     &                   'from file '//FILIN(1:I_LEN(FILIN))//' error: '// &
     &                    NF_STRERROR(IS) )
           RETURN
      END IF
!
! --- Learn the ID of the requested variable V
!
      IS = NF_INQ_VARID ( NCID, 'V', ID_VAR_V )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 5735, IUER, 'READ_GEOS_DQGTUV', 'Variable '// &
     &                   '"V" was not found in the input netcdf file '// &
     &                    FILIN(1:I_LEN(FILIN))//' error: '// &
     &                    NF_STRERROR(IS) )
           RETURN
      END IF
!
! --- Get variable V
!
      IS = NF_GET_VAR_REAL ( NCID, ID_VAR_V, V )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 5736, IUER, 'READ_GEOS_DQGTUV', 'Error in '// &
     &                   'getting the values of the SDS parameter "V" '// &
     &                   'from file '//FILIN(1:I_LEN(FILIN))//' error: '// &
     &                    NF_STRERROR(IS) )
           RETURN
      END IF
!
      CALL CLRCH ( HEB%PROD_NAME )
      IS = NF_GET_ATT_TEXT ( NCID, 0, 'LongName', HEB%PROD_NAME )
!      IF ( IS .NE. 0 ) THEN
!           CALL ERR_LOG ( 5737, IUER, 'READ_GEOS_DQGTUV', 'Error in '// &
!     &         ' an attempt to read attribute LongName: '// &
!     &          NF_STRERROR(IS) )
!           RETURN
!      END IF
!
      CALL CLRCH ( HEB%HISTORY )
      IS = NF_GET_ATT_TEXT ( NCID, 0, 'History', HEB%HISTORY )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 5738, IUER, 'READ_GEOS_DQGTUV', 'Error in '// &
     &         ' an attempt to read attribute History: '// &
     &          NF_STRERROR(IS) )
           RETURN
      END IF
!
      CALL CLRCH ( HEB%SOURCE )
      IS = NF_GET_ATT_TEXT ( NCID, 0, 'Source', HEB%SOURCE )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 5739, IUER, 'READ_GEOS_DQGTUV', 'Error in '// &
     &         ' an attempt to read attribute Source: '// &
     &          NF_STRERROR(IS) )
           RETURN
      END IF
      CALL TRAN ( 13, HEB%SOURCE, HEB%SOURCE )
!
      CALL CLRCH ( HEB%TITLE )
      IS = NF_GET_ATT_TEXT ( NCID, 0, 'Title', HEB%TITLE )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 5740, IUER, 'READ_GEOS_DQGTUV', 'Error in '// &
     &         ' an attempt to read attribute Title: '// &
     &          NF_STRERROR(IS) )
           RETURN
      END IF
      IF ( ILEN(HEB%PROD_NAME) == 0 ) HEB%PROD_NAME = HEB%TITLE 
!
      CALL CLRCH ( HEB%INSTITUTION )
      IS = NF_GET_ATT_TEXT ( NCID, 0, 'Institution', HEB%INSTITUTION )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 5741, IUER, 'READ_GEOS_DQGTUV', 'Error in '// &
     &         ' an attempt to read attribute Institution: '// &
     &          NF_STRERROR(IS) )
           RETURN
      END IF
!
      CALL CLRCH ( HEB%REFERENCES )
      IS = NF_GET_ATT_TEXT ( NCID, 0, 'References', HEB%REFERENCES )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 5742, IUER, 'READ_GEOS_DQGTUV', 'Error in '// &
     &         ' an attempt to read attribute References: '// &
     &          NF_STRERROR(IS) )
           RETURN
      END IF
!
      CALL CLRCH ( HEB%VERSION_ID )
      IS = NF_GET_ATT_TEXT ( NCID, 0, 'VersionID', HEB%VERSION_ID )
      IF ( IS == 0 ) IS = NF_GET_ATT_TEXT ( NCID, 0, 'NCO', HEB%VERSION_ID )
      
!      IF ( IS .NE. 0 ) THEN
!           CALL ERR_LOG ( 5743, IUER, 'READ_GEOS_DQGTUV', 'Error in '// &
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
!           CALL ERR_LOG ( 5743, IUER, 'READ_GEOS_DQGTUV', 'Error in '// &
!     &         ' an attempt to read attribute Filename: '// &
!     &          NF_STRERROR(IS) )
!           RETURN
           ID = LINDEX ( FILIN, '/' )
           HEB%FILE_NAME = FILIN(ID+1:)
      END IF
!
      CALL CLRCH ( HEB%PROD_DATE_TIME )
      IS = NF_GET_ATT_TEXT ( NCID, 0, 'ProductionDateTime', HEB%PROD_DATE_TIME )
!      IF ( IS .NE. 0 ) THEN
!           CALL ERR_LOG ( 5743, IUER, 'READ_GEOS_DQGTUV', 'Error in '// &
!     &         ' an attempt to read attribute ProductionDateTime: '// &
!     &          NF_STRERROR(IS) )
!           RETURN
!      END IF
!
      CALL CLRCH ( STR_DAT )
      IS = NF_GET_ATT_TEXT ( NCID, 0, 'RangeBeginningDate', STR_DAT )
!      IF ( IS .NE. 0 ) THEN
!           CALL ERR_LOG ( 5744, IUER, 'READ_GEOS_DQGTUV', 'Error in '// &
!     &         ' an attempt to read attribute RangeBeginningDate: '// &
!     &          NF_STRERROR(IS) )
!           RETURN
!      END IF
!
      CALL CLRCH ( STR_TIM )
      IS = NF_GET_ATT_TEXT ( NCID, 0, 'RangeBeginningTime', STR_TIM )
!      IF ( IS .NE. 0 ) THEN
!           CALL ERR_LOG ( 5745, IUER, 'READ_GEOS_DQGTUV', 'Error in '// &
!     &         ' an attempt to read attribute RangeBeginningTime: '// &
!     &          NF_STRERROR(IS) )
!           RETURN
!      END IF
!
      IF ( ILEN(STR_DAT) == 0 ) THEN
           IL = ILEN(HEB%FILE_NAME)
           IF ( IL > 20 ) THEN
                STR_DAT = HEB%FILE_NAME(IL-20:IL-17)//'.'// &
      &                   HEB%FILE_NAME(IL-16:IL-15)//'.'// &
      &                   HEB%FILE_NAME(IL-14:IL-13)
                STR_TIM = HEB%FILE_NAME(IL-11:IL-10)//':'// &
      &                   HEB%FILE_NAME(IL-9:IL-8)//':00.0'
              ELSE
                 IS = NF_GET_ATT_INT ( NCID, ID_VAR_TIM, 'begin_date', BEGIN_DATE_I4 )
                 IS = NF_GET_ATT_INT ( NCID, ID_VAR_TIM, 'begin_time', BEGIN_TIME_I4 )
                 WRITE ( UNIT=STR_DAT, FMT='(I8)' ) BEGIN_DATE_I4 
                 WRITE ( UNIT=STR_TIM, FMT='(I6)' ) BEGIN_TIME_I4 
                 CALL CHASHR ( STR_TIM(1:6) )
                 CALL BLANK_TO_ZERO ( STR_TIM(1:6) )
                 STR_DAT = STR_DAT(1:4)//'.'//STR_DAT(5:6)//'.'//STR_DAT(7:8)
                 STR_TIM = STR_TIM(1:2)//':'//STR_TIM(3:4)//':'//STR_TIM(5:6)
           END IF
      END IF
!
      DATE_STR = STR_DAT(1:I_LEN(STR_DAT))//'_'//STR_TIM(1:I_LEN(STR_TIM))
      CALL ERR_PASS ( IUER, IER )
      CALL DATE_TO_TIME ( DATE_STR, HEB%MJD, HEB%UTC, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 5746, IUER, 'READ_GEOS_DQGTUV', 'Error in '// &
     &         ' an attempt to decode beginning date '//DATE_STR )
           RETURN
      END IF
!
! --- Close the NetCDF-file
!
      IS = NF_CLOSE ( NCID )
      CALL ERR_LOG  ( 0, IUER )
      RETURN
      END  SUBROUTINE  READ_GEOS_DQGTUV  !#!#
