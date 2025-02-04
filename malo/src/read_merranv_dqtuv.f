      SUBROUTINE READ_MERRANV_DQTUV ( FILIN, MLON, MLAT, MLEV, MTIM, &
     &                                DELP, QV, T, U, V, HEB, TIM_STEP, &
     &                                IUER )
! ************************************************************************
! *                                                                      *
! *   Routine READ_MERRANV_DQTUV 
! *                                                                      *
! * ## 27-JAN-2013 READ_MERRANV_DQTUV v2.0 (c) L. Petrov 12-APR-2016 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'netcdf.inc'
      INCLUDE   'heb.i'
      TYPE     ( HEB__TYPE ) :: HEB
      CHARACTER  FILIN*(*)
      INTEGER*4  MLON, MLAT, MLEV, MTIM, IUER
      REAL*4     DELP(MLON,MLAT,MLEV,MTIM), &
     &           QV(MLON,MLAT,MLEV,MTIM),   T(MLON,MLAT,MLEV,MTIM),   &
     &           U(MLON,MLAT,MLEV,MTIM),    V(MLON,MLAT,MLEV,MTIM)
      REAL*8     TIM_STEP
      INTEGER*4  ID_VAR_LAT, ID_VAR_LON, ID_VAR_LEV, ID_VAR_TIM
      INTEGER*4  ID_DIM_LAT, ID_DIM_LON, ID_DIM_LEV, ID_DIM_TIM
      INTEGER*4  DIMLEN_LAT, DIMLEN_LON, DIMLEN_LEV, DIMLEN_TIM
      INTEGER*4  ID_VAR_DELP, ID_VAR_PHIS, ID_VAR_QV, ID_VAR_T, &
     &           ID_VAR_U, ID_VAR_V
      INTEGER*4  J1, J2, J3, J4, IS, NDIMS, NVARS, NATTS, NUNL, ID_VAR(4), &
     &           NCID, IP, ID, INT4_STR_DAT, IER
      REAL*8     DAT_MIN_R8(MTIM)
      INTEGER*2  DAT_MIN_I2(MTIM)
      LOGICAL*1  FL_MERRA2
      CHARACTER  STR*128, STR_DAT*32, STR_TIM*32, DATE_STR*32
      LOGICAL*4, EXTERNAL :: IS_R4_NAN 
      CHARACTER, EXTERNAL :: MJDSEC_TO_DATE*30
      INTEGER*4, EXTERNAL :: ILEN, I_LEN, LINDEX
!
      FL_MERRA2 = .FALSE.
!
! --- Open the NETcdf database
!
      IS = NF_OPEN ( FILIN, NF_NOWRITE, NCID )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 5711, IUER, 'READ_MERRANV_DQTUV', &
     &                   'Error in NF_OPEN: '//NF_STRERROR(IS) )
           RETURN
      END IF
!
! --- Learn the number of dimension, variables and attributes
!
      IS = NF_INQ ( NCID, NDIMS, NVARS, NATTS, NUNL )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 5712, IUER, 'READ_MERRANV_DQTUV', 'Error in '// &
     &         'an attempt to open input file '// &
     &          FILIN(1:I_LEN(FILIN))//' NF_INQ: '//NF_STRERROR(IS) )
           RETURN
      END IF
!
! --- Learn the ID of the dimension lon
!
      IS = NF_INQ_DIMID ( NCID, 'XDim:EOSGRID', ID_DIM_LON )
      IF ( IS .NE. 0 ) THEN
           FL_MERRA2 = .TRUE.
           IS = NF_INQ_DIMID ( NCID, 'lon', ID_DIM_LON )
      END IF
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 5713, IUER, 'READ_MERRANV_DQTUV', 'Dimension '// &
     &                   '"XDim:EOSGRID" or "lon" was not found in the input '// &
     &                   'netcdf file '//FILIN(1:I_LEN(FILIN))//' error: '// &
     &                    NF_STRERROR(IS) )
           RETURN
      END IF
!
! --- Get the length of the dimension longitude
!
      IS = NF_INQ_DIMLEN ( NCID, ID_DIM_LON, DIMLEN_LON  )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 5714, IUER, 'READ_MERRANV_DQTUV', 'Error in '// &
     &                   'getting the length of the dimension "lon" in file '// &
     &                    FILIN(1:I_LEN(FILIN))//' error: '// &
     &                    NF_STRERROR(IS) )
           RETURN
      END IF
!
! --- Learn the ID of the dimension lat
!
      IS = NF_INQ_DIMID ( NCID, 'YDim:EOSGRID', ID_DIM_LAT )
      IF ( IS .NE. 0 ) IS = NF_INQ_DIMID ( NCID, 'lat', ID_DIM_LAT )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 5715, IUER, 'READ_MERRANV_DQTUV', 'Dimension '// &
     &                   '"YDim:EOSGRID" or "lat" was not found in the input '// &
     &                   'netcdf file '//FILIN(1:I_LEN(FILIN))//' error: '// &
     &                    NF_STRERROR(IS) )
           RETURN
      END IF
!
! --- Get the length of the dimension latitude
!
      IS = NF_INQ_DIMLEN ( NCID, ID_DIM_LAT, DIMLEN_LAT  )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 5716, IUER, 'READ_MERRANV_DQTUV', 'Error in '// &
     &                   'getting the length of the dimension "lat" in file '// &
     &                    FILIN(1:I_LEN(FILIN))//' error: '// &
     &                    NF_STRERROR(IS) )
           RETURN
      END IF
!
! --- Learn the ID of the dimension lev
!
      IS = NF_INQ_DIMID ( NCID, 'Height:EOSGRID', ID_DIM_LEV )
      IF ( IS .NE. 0 ) IS = NF_INQ_DIMID ( NCID, 'lev', ID_DIM_LEV )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 5717, IUER, 'READ_MERRANV_DQTUV', 'Dimension '// &
     &                   '"Height:EOSGRID" or "lev" was not found in '// &
     &                   'the input netcdf file '//FILIN(1:I_LEN(FILIN))// &
     &                   ' error: '//NF_STRERROR(IS) )
           RETURN
      END IF
!
! --- Get the length of the dimension lev
!
      IS = NF_INQ_DIMLEN ( NCID, ID_DIM_LEV, DIMLEN_LEV  )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 5718, IUER, 'READ_MERRANV_DQTUV', 'Error in '// &
     &                   'getting the length of the dimension "lev" in file '// &
     &                    FILIN(1:I_LEN(FILIN))//' error: '// &
     &                    NF_STRERROR(IS) )
           RETURN
      END IF
!
! --- Learn the ID of the dimension time
!
      IS = NF_INQ_DIMID ( NCID, 'TIME:EOSGRID', ID_DIM_TIM )
      IF ( IS .NE. 0 ) IS = NF_INQ_DIMID ( NCID, 'time', ID_DIM_TIM )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 5719, IUER, 'READ_MERRANV_DQTUV', 'Dimension '// &
     &                   '"TIME:EOSGRID" or "time" was not found in the input '// &
     &                   'netcdf file '//FILIN(1:I_LEN(FILIN))//' error: '// &
     &                    NF_STRERROR(IS) )
           RETURN
      END IF
!
! --- Get the length of the dimension time
!
      IS = NF_INQ_DIMLEN ( NCID, ID_DIM_TIM, DIMLEN_TIM  )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 5720, IUER, 'READ_MERRANV_DQTUV', 'Error in '// &
     &                   'getting the length of the dimension "tim" in '// &
     &                   'file '//FILIN(1:I_LEN(FILIN))//' error: '// &
     &                    NF_STRERROR(IS) )
           RETURN
      END IF
!
! --- Learn the ID of the variable longitude
!
      IS = NF_INQ_VARID ( NCID, 'XDim:EOSGRID', ID_VAR_LON )
      IF ( IS .NE. 0 ) IS = NF_INQ_VARID ( NCID, 'lon', ID_VAR_LON )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 5721, IUER, 'READ_MERRANV_DQTUV', 'Variable '// &
     &                   '"XDim:EOSGRID" or "lon" was not found in the '// &
     &                   'input netcdf file '//FILIN(1:I_LEN(FILIN))// &
     &                   ' error: '//NF_STRERROR(IS) )
           RETURN
      END IF
!
! --- Learn the ID of the variable latitude
!
      IS = NF_INQ_VARID ( NCID, 'YDim:EOSGRID', ID_VAR_LAT )
      IF ( IS .NE. 0 ) IS = NF_INQ_VARID ( NCID, 'lat', ID_VAR_LAT )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 5722, IUER, 'READ_MERRANV_DQTUV', 'Variable '// &
     &                   '"YDim:EOSGRID" or "lat" was not found in the '// &
     &                   'input netcdf file '//FILIN(1:I_LEN(FILIN))// &
     &                   ' error: '//NF_STRERROR(IS) )
           RETURN
      END IF
!
! --- Learn the ID of the variable lev
!
      IS = NF_INQ_VARID ( NCID, 'Height:EOSGRID', ID_VAR_LEV )
      IF ( IS .NE. 0 ) IS = NF_INQ_VARID ( NCID, 'lev', ID_VAR_LEV )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 5723, IUER, 'READ_MERRANV_DQTUV', 'Variable '// &
     &                   '"Height:EOSGRID" or "lev" was not found in the '// &
     &                   'input netcdf file '//FILIN(1:I_LEN(FILIN))// &
     &                   ' error: '//NF_STRERROR(IS) )
           RETURN
      END IF
!
! --- Learn the ID of the variable time
!
      IS = NF_INQ_VARID ( NCID, 'TIME:EOSGRID', ID_VAR_TIM )
      IF ( IS .NE. 0 ) IS = NF_INQ_VARID ( NCID, 'time', ID_VAR_TIM )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 5724, IUER, 'READ_MERRANV_DQTUV', 'Variable '// &
     &                   '"TIME:EOSGRID" or "time" was not found in '// &
     &                   'the input netcdf file '//FILIN(1:I_LEN(FILIN))// &
     &                   ' error: '//NF_STRERROR(IS) )
           RETURN
      END IF
!
! --- Learn the ID of the requested variable DELP
!
      IS = NF_INQ_VARID ( NCID, 'DELP', ID_VAR_DELP )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 5725, IUER, 'READ_MERRANV_DQTUV', 'Variable '// &
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
           CALL ERR_LOG ( 5726, IUER, 'READ_MERRANV_DQTUV', 'Error in '// &
     &                   'getting the values of the SDS parameter "DELP" '// &
     &                   'from file '//FILIN(1:I_LEN(FILIN))//' error: '// &
     &                    NF_STRERROR(IS) )
           RETURN
      END IF
!
! --- Learn ID of variable QV
!
      IS = NF_INQ_VARID ( NCID, 'QV', ID_VAR_QV )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 5729, IUER, 'READ_MERRANV_DQTUV', 'Variable '// &
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
           CALL ERR_LOG ( 5730, IUER, 'READ_MERRANV_DQTUV', 'Error in '// &
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
           CALL ERR_LOG ( 5731, IUER, 'READ_MERRANV_DQTUV', 'Variable '// &
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
           CALL ERR_LOG ( 5732, IUER, 'READ_MERRANV_DQTUV', 'Error in '// &
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
           CALL ERR_LOG ( 5733, IUER, 'READ_MERRANV_DQTUV', 'Variable '// &
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
           CALL ERR_LOG ( 5734, IUER, 'READ_MERRANV_DQTUV', 'Error in '// &
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
           CALL ERR_LOG ( 5735, IUER, 'READ_MERRANV_DQTUV', 'Variable '// &
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
           CALL ERR_LOG ( 5736, IUER, 'READ_MERRANV_DQTUV', 'Error in '// &
     &                   'getting the values of the SDS parameter "V" '// &
     &                   'from file '//FILIN(1:I_LEN(FILIN))//' error: '// &
     &                    NF_STRERROR(IS) )
           RETURN
      END IF
!
      CALL CLRCH ( HEB%PROD_NAME )
      IP = LINDEX ( FILIN, '/' ) + 1
      ID = LINDEX ( FILIN, '.' ) - 1
      IF ( ID < IP ) ID = IP
      ID = LINDEX ( FILIN(1:ID), '.' ) - 1
      IF ( ID < IP ) ID = IP
      HEB%PROD_NAME  = FILIN(IP:ID)
!
      CALL CLRCH ( HEB%HISTORY )
      IS = NF_GET_ATT_TEXT ( NCID, 0, 'history', HEB%HISTORY )
      IF ( IS .NE. 0 ) IS = NF_GET_ATT_TEXT ( NCID, 0, 'History', HEB%HISTORY )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 5738, IUER, 'READ_MERRANV_DQTUV', 'Error in '// &
     &         ' an attempt to read attribute History: '// &
     &          NF_STRERROR(IS) )
           RETURN
      END IF
!
      CALL CLRCH ( HEB%SOURCE )
      IS = NF_GET_ATT_TEXT ( NCID, 0, 'source', HEB%SOURCE )
      IF ( IS .NE. 0 ) IS = NF_GET_ATT_TEXT ( NCID, 0, 'Source', HEB%SOURCE )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 5739, IUER, 'READ_MERRANV_DQTUV', 'Error in '// &
     &         ' an attempt to read attribute Source: '// &
     &          NF_STRERROR(IS) )
           RETURN
      END IF
      CALL TRAN ( 13, HEB%SOURCE, HEB%SOURCE )
!
      CALL CLRCH ( HEB%TITLE )
      IS = NF_GET_ATT_TEXT ( NCID, 0, 'title', HEB%TITLE )
      IF ( IS .NE. 0 ) IS = NF_GET_ATT_TEXT ( NCID, 0, 'Title', HEB%TITLE )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 5740, IUER, 'READ_MERRANV_DQTUV', 'Error in '// &
     &         ' an attempt to read attribute Title: '// &
     &          NF_STRERROR(IS) )
           RETURN
      END IF
!
      CALL CLRCH ( HEB%INSTITUTION )
      IS = NF_GET_ATT_TEXT ( NCID, 0, 'institution', HEB%INSTITUTION )
      IF ( IS .NE. 0 ) IS = NF_GET_ATT_TEXT ( NCID, 0, 'Institution', HEB%INSTITUTION )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 5741, IUER, 'READ_MERRANV_DQTUV', 'Error in '// &
     &         ' an attempt to read attribute Institution: '// &
     &          NF_STRERROR(IS) )
           RETURN
      END IF
!
      CALL CLRCH ( HEB%REFERENCES )
      IS = NF_GET_ATT_TEXT ( NCID, 0, 'references', HEB%REFERENCES )
      IF ( IS .NE. 0 ) IS = NF_GET_ATT_TEXT ( NCID, 0, 'References', HEB%REFERENCES )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 5742, IUER, 'READ_MERRANV_DQTUV', 'Error in '// &
     &         ' an attempt to read attribute References: '// &
     &          NF_STRERROR(IS) )
           RETURN
      END IF
!
      CALL CLRCH ( HEB%VERSION_ID )
      IS = NF_GET_ATT_TEXT ( NCID, 0, 'comment', HEB%VERSION_ID )
      IF ( IS .NE. 0 ) IS = NF_GET_ATT_TEXT ( NCID, 0, 'Comment', HEB%VERSION_ID )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 5743, IUER, 'READ_MERRANV_DQTUV', 'Error in '// &
     &         ' an attempt to read attribute VersionID: '// &
     &          NF_STRERROR(IS) )
           RETURN
      END IF
!
      CALL CLRCH ( HEB%FILE_NAME )
      HEB%FILE_NAME  = FILIN(IP:)
!
      IS = NF_GET_ATT_INT  ( NCID, ID_VAR_TIM, 'begin_date', INT4_STR_DAT )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 5743, IUER, 'READ_MERRANV_DQTUV', 'Error in '// &
     &         ' an attempt to read attribute begin_date: '// &
     &          NF_STRERROR(IS) )
           RETURN
      END IF
!
! --- Get variable DAT_MIN_R8 or DAT_MIN_I2
!
      IF ( FL_MERRA2 ) THEN
           IS = NF_GET_VAR_INT2 ( NCID, ID_VAR_TIM, DAT_MIN_I2 )
         ELSE 
           IS = NF_GET_VAR_REAL ( NCID, ID_VAR_TIM, DAT_MIN_R8 )
      END IF
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 5744, IUER, 'READ_MERRANV_DQTUV', 'Error in '// &
     &                   'getting the values of the SDS parameter "Time" '// &
     &                   'from file '//FILIN(1:I_LEN(FILIN))//' error: '// &
     &                    NF_STRERROR(IS) )
           RETURN
      END IF
!
      WRITE ( UNIT=DATE_STR, FMT='(I8)' ) INT4_STR_DAT
      IF ( FL_MERRA2 ) THEN
           CALL RH_TAT ( DAT_MIN_I2(1)/60.0D0/24.0D0*PI2, 1, STR_TIM, IER )
           TIM_STEP = (DAT_MIN_I2(2) - DAT_MIN_I2(1))*60.0D0
         ELSE 
           CALL RH_TAT ( DAT_MIN_R8(1)/60.0D0/24.0D0*PI2, 1, STR_TIM, IER )
           TIM_STEP = (DAT_MIN_R8(2) - DAT_MIN_R8(1))*60.0D0
      END IF
!
      DATE_STR = DATE_STR(1:4)//'.'//DATE_STR(5:6)//'.'//DATE_STR(7:8)// &
     &         '_'//STR_TIM(2:)
!
      DATE_STR = DATE_STR(1:I_LEN(DATE_STR)) ! //'_'//STR_TIM(1:I_LEN(STR_TIM))
      CALL ERR_PASS ( IUER, IER )
      CALL DATE_TO_TIME ( DATE_STR, HEB%MJD, HEB%UTC, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 5746, IUER, 'READ_MERRANV_DQTUV', 'Error in '// &
     &         ' an attempt to decode beginning date '//DATE_STR )
           RETURN
      END IF
!
      CALL CLRCH ( HEB%PROD_DATE_TIME )
      HEB%PROD_DATE_TIME = 'not_available'
!
! --- Close the NetCDF-file
!
      IS = NF_CLOSE ( NCID )
      CALL ERR_LOG  ( 0, IUER )
      RETURN
      END  SUBROUTINE  READ_MERRANV_DQTUV  !#!#
