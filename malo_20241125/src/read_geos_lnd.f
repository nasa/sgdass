      SUBROUTINE READ_GEOS_LND ( FILIN, HEB_TWLAND, TIM_STEP, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine READ_GEOS_LND
! *                                                                      *
! * ### 11-JAN-2013  READ_GEOS_LND  v1.1  (c) L. Petrov 10-JAN-2016  ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'netcdf.inc'
      INCLUDE   'heb.i'
      INTEGER*4  IUER
      CHARACTER  FILIN*128
      TYPE     ( HEB__TYPE ) :: HEB_TWLAND      
      REAL*8     TIM_STEP
      INTEGER*4  IS, NCID,   DIMLEN_LAT, DIMLEN_LON, DIMLEN_TIM, &
     &           ID_DIM_LON, ID_DIM_LAT, ID_DIM_TIM, VECDIM(3),  &
     &           ID_VAR_LAT, ID_VAR_LON, ID_VAR_TIM, &
     &           ID_VAR_TWLAND
      REAL*8     REA, FE, EXC_SQ, ACC_EQU, GRV_LAT, GRV_H
      PARAMETER  ( REA     = 6378136.3D0 )       ! Earth's equatorial radius
      PARAMETER  ( FE      = 1.D0/298.257D0 )    ! Earth's flattening
      PARAMETER  ( EXC_SQ  = 2.D0*FE - FE**2 )   ! Earth's eccentricity
      PARAMETER  ( ACC_EQU = 9.7803184558D0 )    ! Equatorial gravity acc.
      PARAMETER  ( GRV_H   = -2.D0*ACC_EQU/REA ) ! D(ACC_EQU)/DH
      PARAMETER  ( GRV_LAT = 0.001931663  )      ! D(ACC_EQU)/D(phi)
      INTEGER*4    M_DIM
      PARAMETER  ( M_DIM = 8192 )
      REAL*4     ARR_ALONG_LON(M_DIM)
      REAL*8     TIM_BEG, TIM_END, LAT, G_ACC
      REAL*8,    ALLOCATABLE :: TIM_MINUTE_R8(:)
      INTEGER*4, ALLOCATABLE :: TIM_MINUTE_I4(:)
      CHARACTER  STR*128, STR_DAT*32, STR_TIM*32, DATE_STR*32
      INTEGER*4  J1, J2, J3, MJD_BEG, MJD_END, INT4_STR_DAT, VAR_TIM_TYPE,  &
     &           ID, IP, IND_LON, IER
      INTEGER*4, EXTERNAL :: ILEN, I_LEN, LINDEX
!
      HEB_TWLAND%STATUS = HEB__UNDF
!
! --- Open the new ouput file in netcdf format
!
      IS = NF_OPEN ( FILIN, NF_NOWRITE, NCID )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 7321, IUER, 'READ_GEOS_LND', 'Error in an '// &
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
           CALL ERR_LOG ( 7322, IUER, 'READ_GEOS_LND', 'Error in '// &
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
           CALL ERR_LOG ( 7323, IUER, 'READ_GEOS_LND', 'Error in '// &
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
           CALL ERR_LOG ( 7324, IUER, 'READ_GEOS_LND', 'Error in '// &
     &         ' an attempt to read dimension tim: '// &
     &          NF_STRERROR(IS) )
           RETURN
      END IF
!
! --- Get the length of the dimension "lon"
!
      IS = NF_INQ_DIMLEN ( NCID, ID_DIM_LON, DIMLEN_LON  )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 7325, IUER, 'READ_GEOS_LND', 'Error in '// &
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
           CALL ERR_LOG ( 7326, IUER, 'READ_GEOS_LND', 'Error in '// &
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
           CALL ERR_LOG ( 7327, IUER, 'READ_GEOS_LND', 'Error in '// &
     &         'getting the length of the dimension "tim" in file '// &
     &          FILIN(1:I_LEN(FILIN))//' error: '// &
     &          NF_STRERROR(IS) )
           RETURN
      END IF
!
      HEB_TWLAND%DIMS(1) = DIMLEN_LON
      HEB_TWLAND%DIMS(2) = DIMLEN_LAT
      HEB_TWLAND%DIMS(3) = 1
      HEB_TWLAND%DIMS(4) = DIMLEN_TIM
      ALLOCATE ( HEB_TWLAND%VAL(HEB_TWLAND%DIMS(1),HEB_TWLAND%DIMS(2),HEB_TWLAND%DIMS(3),HEB_TWLAND%DIMS(4)), &
     &           STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH  ( STR )
           CALL IINCH8 ( INT8(4)*HEB_TWLAND%DIMS(1)*HEB_TWLAND%DIMS(2)*HEB_TWLAND%DIMS(3)*HEB_TWLAND%DIMS(4), &
     &                   STR )
           CALL ERR_LOG ( 7328, IUER, 'READ_GEOS_LND', 'Failure in '// &
     &         'an attempt to allocate '//STR(1:I_LEN(STR))//' bytes of memory '// &
     &         'for array TWLAND' )
           RETURN
      END IF
      HEB_TWLAND%STATUS = HEB__ALLO
!
! --- Learn the ID of the variable "TWLAND"
!
      IS = NF_INQ_VARID ( NCID, 'TWLAND', ID_VAR_TWLAND )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 7329, IUER, 'READ_GEOS_LND', 'Variable '// &
     &         '"TWLAND" was not found in the input netcdf file '// &
     &          FILIN(1:I_LEN(FILIN))//' error: '//NF_STRERROR(IS) )
           RETURN
      END IF
!
! --- Read array with TWLAND data
!
      IS = NF_GET_VAR_REAL ( NCID, ID_VAR_TWLAND, HEB_TWLAND%VAL )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 7330, IUER, 'READ_GEOS_LND', 'Error in '// &
     &         'getting the values of the variable "TWLAND" from the file '// &
     &          FILIN(1:I_LEN(FILIN))//' error: '// &
     &          NF_STRERROR(IS) )
           RETURN
      END IF
      HEB_TWLAND%STATUS = HEB__LOAD
!
      CALL CLRCH ( HEB_TWLAND%HISTORY )
      IS = NF_GET_ATT_TEXT ( NCID, 0, 'History', HEB_TWLAND%HISTORY )
      IF ( IS .NE. 0 ) THEN
           IS = NF_GET_ATT_TEXT ( NCID, 0, 'history', HEB_TWLAND%HISTORY )
      END IF
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 7331, IUER, 'READ_GEOS_LND', 'Error in '// &
     &         ' an attempt to read attribute History: '// &
     &          NF_STRERROR(IS) )
           RETURN
      END IF
!
      CALL CLRCH ( HEB_TWLAND%SOURCE )
      IS = NF_GET_ATT_TEXT ( NCID, 0, 'Source', HEB_TWLAND%SOURCE )
      IF ( IS .NE. 0 ) THEN
           IS = NF_GET_ATT_TEXT ( NCID, 0, 'source', HEB_TWLAND%SOURCE )
      END IF
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 7332, IUER, 'READ_GEOS_LND', 'Error in '// &
     &         ' an attempt to read attribute Source: '// &
     &          NF_STRERROR(IS) )
           RETURN
      END IF
      CALL TRAN ( 13, HEB_TWLAND%SOURCE, HEB_TWLAND%SOURCE )
!
      CALL CLRCH ( HEB_TWLAND%TITLE )
      IS = NF_GET_ATT_TEXT ( NCID, 0, 'Title', HEB_TWLAND%TITLE )
      IF ( IS .NE. 0 ) THEN
           IS = NF_GET_ATT_TEXT ( NCID, 0, 'title', HEB_TWLAND%TITLE )
      END IF
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 7333, IUER, 'READ_GEOS_LND', 'Error in '// &
     &         ' an attempt to read attribute Title: '// &
     &          NF_STRERROR(IS) )
           RETURN
      END IF
!
      CALL CLRCH ( HEB_TWLAND%INSTITUTION )
      IS = NF_GET_ATT_TEXT ( NCID, 0, 'Institution', HEB_TWLAND%INSTITUTION )
      IF ( IS .NE. 0 ) THEN
           IS = NF_GET_ATT_TEXT ( NCID, 0, 'institution', HEB_TWLAND%INSTITUTION )
      END IF
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 7334, IUER, 'READ_GEOS_LND', 'Error in '// &
     &         ' an attempt to read attribute Institution: '// &
     &          NF_STRERROR(IS) )
           RETURN
      END IF
!
      CALL CLRCH ( HEB_TWLAND%REFERENCES )
      IS = NF_GET_ATT_TEXT ( NCID, 0, 'References', HEB_TWLAND%REFERENCES )
      IF ( IS .NE. 0 ) THEN
           IS = NF_GET_ATT_TEXT ( NCID, 0, 'references', HEB_TWLAND%REFERENCES )
      END IF
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 7335, IUER, 'READ_GEOS_LND', 'Error in '// &
     &         ' an attempt to read attribute References: '// &
     &          NF_STRERROR(IS) )
           RETURN
      END IF
!
      CALL CLRCH ( HEB_TWLAND%VERSION_ID )
      IS = NF_GET_ATT_TEXT ( NCID, 0, 'VersionID', HEB_TWLAND%VERSION_ID )
      IF ( IS .NE. 0 ) THEN
           IS = NF_GET_ATT_TEXT ( NCID, 0, 'comment', HEB_TWLAND%VERSION_ID )
      END IF
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 7336, IUER, 'READ_GEOS_LND', 'Error in '// &
     &         ' an attempt to read attribute VersionID: '// &
     &          NF_STRERROR(IS) )
           RETURN
      END IF
!
      CALL CLRCH ( HEB_TWLAND%FILE_NAME )
      IS = NF_GET_ATT_TEXT ( NCID, 0, 'Filename', HEB_TWLAND%FILE_NAME )
      IF ( IS .NE. 0 ) THEN
           ID = LINDEX ( FILIN, '/' ) 
           IF ( ID < 1 ) ID = 1
           HEB_TWLAND%FILE_NAME = FILIN(ID:)
      END IF
!
      CALL CLRCH ( HEB_TWLAND%PROD_DATE_TIME )
      IS = NF_GET_ATT_TEXT ( NCID, 0, 'ProductionDateTime', HEB_TWLAND%PROD_DATE_TIME )
      IF ( IS .NE. 0 ) THEN
           HEB_TWLAND%PROD_DATE_TIME = 'n/a'
      END IF
!
      IF ( DIMLEN_TIM == 1 ) THEN
           CALL CLRCH ( HEB_TWLAND%PROD_NAME )
           IS = NF_GET_ATT_TEXT ( NCID, 0, 'LongName', HEB_TWLAND%PROD_NAME )
           IF ( IS .NE. 0 ) THEN
                IS = NF_GET_ATT_TEXT ( NCID, 0, 'long_name', HEB_TWLAND%PROD_NAME )
           END IF
           IF ( IS .NE. 0 ) THEN
                CALL ERR_LOG ( 7337, IUER, 'READ_GEOS_LND', 'Error in '// &
          &         ' an attempt to read attribute LongName: '// &
          &          NF_STRERROR(IS) )
                RETURN
           END IF
!
           CALL CLRCH ( STR_DAT )
           IS = NF_GET_ATT_TEXT ( NCID, 0, 'RangeBeginningDate', STR_DAT )
           IF ( IS .NE. 0 ) THEN
                CALL ERR_LOG ( 7338, IUER, 'READ_GEOS_LND', 'Error in '// &
     &              ' an attempt to read attribute RangeBeginningDate: '// &
     &               NF_STRERROR(IS) )
                RETURN
           END IF
!
           CALL CLRCH ( STR_TIM )
           IS = NF_GET_ATT_TEXT ( NCID, 0, 'RangeBeginningTime', STR_TIM )
           IF ( IS .NE. 0 ) THEN
                CALL ERR_LOG ( 7339, IUER, 'READ_GEOS_LND', 'Error in '// &
     &              ' an attempt to read attribute RangeBeginningTime: '// &
     &               NF_STRERROR(IS) )
                RETURN
           END IF
!
           DATE_STR = STR_DAT(1:I_LEN(STR_DAT))//'_'//STR_TIM(1:I_LEN(STR_TIM))
           CALL ERR_PASS ( IUER, IER )
           CALL DATE_TO_TIME ( DATE_STR, HEB_TWLAND%MJD, HEB_TWLAND%UTC, IER )
           IF ( IS .NE. 0 ) THEN
                CALL ERR_LOG ( 7340, IUER, 'READ_GEOS_LND', 'Error in '// &
     &              ' an attempt to decode beginning date '//DATE_STR )
                RETURN
           END IF
!
           CALL CLRCH ( STR_DAT )
           IS = NF_GET_ATT_TEXT ( NCID, 0, 'RangeEndingDate', STR_DAT )
           IF ( IS .NE. 0 ) THEN
                CALL ERR_LOG ( 7341, IUER, 'READ_GEOS_LND', 'Error in '// &
     &              ' an attempt to read attribute RangeBeginningDate: '// &
     &               NF_STRERROR(IS) )
                RETURN
           END IF
!
           CALL CLRCH ( STR_TIM )
           IS = NF_GET_ATT_TEXT ( NCID, 0, 'RangeEndingTime', STR_TIM )
           IF ( IS .NE. 0 ) THEN
                CALL ERR_LOG ( 7342, IUER, 'READ_GEOS_LND', 'Error in '// &
     &              ' an attempt to read attribute RangeBeginningTime: '// &
     &               NF_STRERROR(IS) )
                RETURN
           END IF
!
           DATE_STR = STR_DAT(1:I_LEN(STR_DAT))//'_'//STR_TIM(1:I_LEN(STR_TIM))
           CALL ERR_PASS ( IUER, IER )
           CALL DATE_TO_TIME ( DATE_STR, MJD_END, TIM_END, IER )
           IF ( IS .NE. 0 ) THEN
                CALL ERR_LOG ( 7343, IUER, 'READ_GEOS_LND', 'Error in '// &
     &              ' an attempt to decode beginning date '//DATE_STR )
                RETURN
           END IF
!
           HEB_TWLAND%UTC = (HEB_TWLAND%UTC + (MJD_END - HEB_TWLAND%MJD)*86400.0D0 + TIM_END)/2.0D0
           IF ( HEB_TWLAND%UTC > 86400.0D0 ) THEN
                HEB_TWLAND%UTC = HEB_TWLAND%UTC - 86400.0D0
                HEB_TWLAND%MJD = HEB_TWLAND%MJD + 1
           END IF
           TIM_STEP = 0.0D0
         ELSE IF ( DIMLEN_TIM > 1 ) THEN
!
! -------- Learn the ID of the variable "time"
!
           IS = NF_INQ_VARID ( NCID, 'time', ID_VAR_TIM)
           IF ( IS .NE. 0 ) THEN
                IS = NF_INQ_VARID ( NCID, 'TIME:EOSGRID', ID_VAR_TIM )
           END IF
           IF ( IS .NE. 0 ) THEN
                CALL ERR_LOG ( 7344, IUER, 'READ_GEOS_LND', 'Variable '// &
     &              '"time" was not found in the input netcdf file '// &
     &              FILIN(1:I_LEN(FILIN))//' error: '//NF_STRERROR(IS) )
                RETURN
           END IF
!
           IS = NF_INQ_VARTYPE ( NCID, ID_VAR_TIM, VAR_TIM_TYPE )
           IF ( IS .NE. 0 ) THEN
                CALL ERR_LOG ( 7345, IUER, 'READ_GEOS_LND', 'Variable '// &
     &              '"time" was not found in the input netcdf file '// &
     &              FILIN(1:I_LEN(FILIN))//' error: '//NF_STRERROR(IS) )
                RETURN
           END IF
!
           CALL CLRCH ( HEB_TWLAND%PROD_NAME )
           IP = LINDEX ( FILIN, '/' ) + 1
           ID = LINDEX ( FILIN, '.' ) - 1
           IF ( ID < IP ) ID = IP
           ID = LINDEX ( FILIN(1:ID), '.' ) - 1
           IF ( ID < IP ) ID = IP
           HEB_TWLAND%PROD_NAME  = FILIN(IP:ID)
!
           IS = NF_GET_ATT_INT  ( NCID, ID_VAR_TIM, 'begin_date', INT4_STR_DAT )
           IF ( IS .NE. 0 ) THEN
                CALL ERR_LOG ( 7346, IUER, 'READ_GEOS_LND', 'Error in '// &
     &              ' an attempt to read attribute begin_date: '// &
     &               NF_STRERROR(IS) )
                RETURN
           END IF
!
           IF ( VAR_TIM_TYPE == NF_DOUBLE ) THEN
                ALLOCATE ( TIM_MINUTE_R8(DIMLEN_TIM), STAT=IER )
                IF ( IER .NE. 0 ) THEN
                     CALL CLRCH ( STR )
                     CALL IINCH ( 8*DIMLEN_TIM, STR )
                     CALL ERR_LOG ( 7347, IUER, 'READ_GEOS_LND', 'Failure in '// &
     &                   'an attempt to allocate '//STR(1:I_LEN(STR))// &
     &                   ' bytes of memory for array TIM_MINUTE_R8' )
                    RETURN
                END IF
!
! ------------- Get variable TIM_MINUTE_R8
!
                IS = NF_GET_VAR_DOUBLE ( NCID, ID_VAR_TIM, TIM_MINUTE_R8 )
                IF ( IS .NE. 0 ) THEN
                     CALL ERR_LOG ( 7348, IUER, 'READ_GEOS_LND', 'Error in '// &
     &                             'getting the values of the SDS parameter "Time" '// &
     &                             'from file '//FILIN(1:I_LEN(FILIN))//' error: '// &
     &                              NF_STRERROR(IS) )
                     RETURN
                END IF
!
                WRITE ( UNIT=DATE_STR, FMT='(I8)' ) INT4_STR_DAT
                CALL RH_TAT ( TIM_MINUTE_R8(1)/60.0D0/24.0D0*PI2, 1, STR_TIM, IER )
!
                DATE_STR = DATE_STR(1:4)//'.'//DATE_STR(5:6)//'.'//&
     &                     DATE_STR(7:8)//'_'//STR_TIM(2:)
                TIM_STEP = (TIM_MINUTE_R8(2) - TIM_MINUTE_R8(1))*60.0D0
                DEALLOCATE ( TIM_MINUTE_R8 )
              ELSE IF ( VAR_TIM_TYPE == NF_INT ) THEN
                ALLOCATE ( TIM_MINUTE_I4(DIMLEN_TIM), STAT=IER )
                IF ( IER .NE. 0 ) THEN
                     CALL CLRCH ( STR )
                     CALL IINCH ( 4*DIMLEN_TIM, STR )
                     CALL ERR_LOG ( 7349, IUER, 'READ_GEOS_LND', 'Failure in '// &
     &                   'an attempt to allocate '//STR(1:I_LEN(STR))// &
     &                   ' bytes of memory for array TIM_MINUTE_I4' )
                    RETURN
                END IF
!
! ------------- Get variable TIM_MINUTE_I4
!
                IS = NF_GET_VAR_INT ( NCID, ID_VAR_TIM, TIM_MINUTE_I4 )
                IF ( IS .NE. 0 ) THEN
                     CALL ERR_LOG ( 7350, IUER, 'READ_GEOS_LND', 'Error in '// &
     &                             'getting the values of the SDS parameter "Time" '// &
     &                             'from file '//FILIN(1:I_LEN(FILIN))//' error: '// &
     &                              NF_STRERROR(IS) )
                     RETURN
                END IF
!
                WRITE ( UNIT=DATE_STR, FMT='(I8)' ) INT4_STR_DAT
                CALL RH_TAT ( TIM_MINUTE_I4(1)/60.0D0/24.0D0*PI2, 1, STR_TIM, IER )
!
                DATE_STR = DATE_STR(1:4)//'.'//DATE_STR(5:6)//'.'//&
     &                     DATE_STR(7:8)//'_'//STR_TIM(2:)
                TIM_STEP = (TIM_MINUTE_I4(2) - TIM_MINUTE_I4(1))*60.0D0
                DEALLOCATE ( TIM_MINUTE_I4 )
           END IF
!
           CALL ERR_PASS ( IUER, IER )
           CALL DATE_TO_TIME ( DATE_STR, HEB_TWLAND%MJD, HEB_TWLAND%UTC, IER )
           IF ( IS .NE. 0 ) THEN
                CALL ERR_LOG ( 7351, IUER, 'READ_GEOS_LND', 'Error in '// &
     &              ' an attempt to decode beginning date '//DATE_STR )
                RETURN
           END IF
!
           CALL CLRCH ( HEB_TWLAND%PROD_DATE_TIME )
           HEB_TWLAND%PROD_DATE_TIME = 'not_available'
      END IF
!
      DO 410 J1=1,HEB_TWLAND%DIMS(4)
         DO 420 J2=1,HEB_TWLAND%DIMS(2)
            LAT = -P2I + (J2-1)*PI__NUM/(HEB_TWLAND%DIMS(2)-1)
            G_ACC = ACC_EQU* (1.D0 + GRV_LAT* DSIN(LAT)**2 ) / &
     &                DSQRT  (1.D0 - EXC_SQ*  DSIN(LAT)**2)
            DO 430 J3=1,HEB_TWLAND%DIMS(1)
               IND_LON = J3 + HEB_TWLAND%DIMS(1)/2
               IF ( IND_LON > HEB_TWLAND%DIMS(1) ) IND_LON = IND_LON - HEB_TWLAND%DIMS(1)
               IF ( HEB_TWLAND%VAL(J3,J2,1,J1) > 1.0E5 .OR. &
     &              HEB_TWLAND%VAL(J3,J2,1,J1) < 0.0        )  THEN
                    ARR_ALONG_LON(IND_LON) = 0.0
                 ELSE 
                    ARR_ALONG_LON(IND_LON) = HEB_TWLAND%VAL(J3,J2,1,J1)*G_ACC
               END IF
 430        CONTINUE 
            HEB_TWLAND%VAL(1:HEB_TWLAND%DIMS(1),J2,1,J1) = ARR_ALONG_LON(1:HEB_TWLAND%DIMS(1))
 420     CONTINUE 
 410  CONTINUE 
!
! --- Close the NetCDF-file
!
      IS = NF_CLOSE ( NCID )
!
      CALL ERR_LOG ( 0, IUER ) 
      RETURN
      END  SUBROUTINE  READ_GEOS_LND  !#!#
