      SUBROUTINE SPR_WRITE_NC ( MALO, FILGEN, MJD_BEG, UTC_BEG, &
     &                          MJD_END, UTC_END, IVRB, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine SPR_WRITE_NC 
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *    MALO ( MALO__TYPE ) -- Object that keesp variuables and arrays    *
! *                           relaveant to MAss LOading software package.*
! *  MJD_BEG ( INTEGER*4 ) -- Time of the first time epoch: MJD fraction.*
! *  UTC_BEG ( REAL*8    ) -- Time of the first time epoch: seconds      *
! *                           fraction.                                  *
! *  MJD_END ( INTEGER*4 ) -- Time of the last time epoch: MJD fraction. *
! *  UTC_END ( REAL*8    ) -- Time of the last time epoch: seconds       *
! *                           fraction.                                  *
! *  FILGEN ( CHARACTER  ) -- Generic file name. The suffix in the form  *
! *                           yyyy.mm.nc where yyyy -- year, mm -- month *
! *                           of the first time epoch, will be appended. *
! *    IVRB ( INTEGER*4 ) -- Verbosity level.                            *
! *                           0 -- no information messages will be       *
! *                                printed in screen;                    *
! *                           1 -- some information messages will be     *
! *                                printed in screen.                    *
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
! *    IUER ( INTEGER*4, OPT ) -- Universal error handler.               *
! *                           Input: switch IUER=0 -- no error messages  *
! *                                  will be generated even in the case  *
! *                                  of error. IUER=-1 -- in the case of *
! *                                  error the message will be put on    *
! *                                  stdout.                             *
! *                           Output: 0 in the case of successful        *
! *                                   completion and non-zero in the     *
! *                                   case of error.                     *
! *                                                                      *
! *  ### 16-OCT-2012  SPR_WRITE_NC  v1.1 (c)  L. Petrov 01-NOV-2012 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'malo.i'
      INCLUDE   'netcdf.inc'
      TYPE     ( MALO__TYPE ) :: MALO
      INTEGER*4  MJD_BEG, MJD_END, IVRB, IUER
      REAL*8     UTC_BEG, UTC_END
      CHARACTER  FILGEN*(*)
      CHARACTER  STR_BEG*32, STR*80, STR1*80
      REAL*8     JD, RANGE_HRS(2), RANGE_R8(2)
      INTEGER*4  ID_DIM_LON, ID_DIM_LAT, ID_DIM_TIM
      INTEGER*4  ID_VAR_LON, ID_VAR_LAT, ID_VAR_TIM, ID_VAR_SPR, ID_VAR(4)
      INTEGER*4  IND_BEG, IND_END, NTIM, NCID
      INTEGER*4  IS, VECDIM(3), IL, J1, J2, J3, J4, IND_EPC, IER
      INTEGER*2  VAL_I2
      REAL*4     LAT_RANGE(2), LON_RANGE(2), SPR_RANGE(2)
      REAL*8     TIM_RANGE(2), TAI_BEG, TAI_END, EPOCH_STEP
      REAL*8,    ALLOCATABLE :: TIM_ARR(:)
      CHARACTER  SPR_WRITE_NC__LABEL*36
      PARAMETER  ( SPR_WRITE_NC__LABEL = 'SPR_WRITE_NC   Version of 2012.10.24' )
      CHARACTER, EXTERNAL :: JD_TO_DATE*23, MJDSEC_TO_DATE*30, GET_CDATE*19
      INTEGER*4, EXTERNAL :: I_LEN, ILEN
!
! --- Learn how many time epochs the data segment has
!
      IER = -1
      CALL MALO_UTC_TO_TAI ( MALO, MJD_BEG, UTC_BEG, TAI_BEG, IER )
      IER = -1
      CALL MALO_UTC_TO_TAI ( MALO, MJD_END, UTC_END, TAI_END, IER )
!
      IF ( MALO%NTIM == 1 ) THEN
           EPOCH_STEP = 0.0D0
           IND_BEG    = 1
           IND_END    = 1
         ELSE 
           EPOCH_STEP = (MALO%MJD_ARR(2)*86400.0D0 + MALO%TAI_ARR(2)) - &
     &                  (MALO%MJD_ARR(1)*86400.0D0 + MALO%TAI_ARR(1))
           IND_BEG   =  IDNINT ( ( (MJD_BEG*86400.0D0 + TAI_BEG) - &
     &                             (MALO%MJD_ARR(1)*86400.0 + MALO%TAI_ARR(1)) &
     &                         )/EPOCH_STEP ) + 1
           IND_END   =  IDNINT ( ( (MJD_END*86400.0D0 + TAI_END) - &
     &                             (MALO%MJD_ARR(1)*86400.0D0 + MALO%TAI_ARR(1)) &
     &                          )/EPOCH_STEP ) + 1
      END IF
      IF ( IND_BEG < 1         ) IND_BEG = 1
      IF ( IND_END > MALO%NTIM ) IND_END = MALO%NTIM
      IF ( IND_BEG > MALO%NTIM ) THEN
           CALL CLRCH ( STR  ) 
           CALL CLRCH ( STR1 ) 
           STR  = MJDSEC_TO_DATE ( MJD_BEG, TAI_BEG, -2 )
           STR1 = MJDSEC_TO_DATE ( MALO%MJD_ARR(MALO%NTIM), MALO%TAI_ARR(MALO%NTIM), -2 )
           CALL ERR_LOG ( 6411, IUER, 'SPR_WRITE_NC', 'Wrong begin TAI date: '// &
     &          STR(1:21)//' -- it is after the last date of '// &
     &          'the surface pressure data '//STR1(1:21) )
           RETURN 
      END IF
      IF ( IND_END < 1 ) THEN
           CALL CLRCH ( STR  ) 
           CALL CLRCH ( STR1 ) 
           STR  = MJDSEC_TO_DATE ( MJD_END, TAI_END, -2 )
           STR1 = MJDSEC_TO_DATE ( MALO%MJD_ARR(1), MALO%TAI_ARR(1), -2 )
           CALL ERR_LOG ( 6412, IUER, 'SPR_WRITE_NC', 'Wrong last TAI date: '// &
     &          STR(1:21)//' -- it is before the first date of '// &
     &          'the surface pressure data '//STR1(1:21) )
           RETURN 
      END IF
      IF ( IND_END < IND_BEG ) THEN
           CALL ERR_LOG ( 6413, IUER, 'SPR_WRITE_NC', 'Wrong first/last '// &
     &         'TAI dates: they do not fit the time interval for which '// &
     &         'the surface pressure data are defined' )
           RETURN 
      END IF
      NTIM = IND_END - IND_BEG + 1 
!
! --- Build the output file name
!
      JD = 2400000.5D0 + MJD_BEG + UTC_BEG/86400.0
      STR_BEG = JD_TO_DATE ( JD, -3 )
      STR_BEG(5:5) = '_'
      IF ( MALO%DATA_TYPE == 'Time_series_NCEP_Reanalysis_96  ' ) THEN
           MALO%FILOUT = FILGEN(1:I_LEN(FILGEN))//STR_BEG(1:7)//'.nc'
         ELSE IF ( MALO%DATA_TYPE == 'Mean_Pressure_NCEP_Reanalysis_96' ) THEN
           MALO%FILOUT = FILGEN
         ELSE 
           CALL ERR_LOG ( 6414, IUER, 'SPR_WRITE_NC', 'Unsupported '// &
     &          'data type '//MALO%DATA_TYPE )
           RETURN
      END IF
!
! --- Store the vector of dimensions ID (needed for betcdf data handler)
!
      VECDIM(1) = 1
      VECDIM(2) = 2
      VECDIM(3) = 3
!
! --- Open the new ouput file in netcdf format
!
      IS = NF_CREATE ( MALO%FILOUT, 0, NCID )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 6414, IUER, 'SPR_WRITE_NC', 'Error in an attempt '// &
     &         'to create the output netcf file '//MALO%FILOUT(1:I_LEN(MALO%FILOUT))// &
     &         ' NF_CREATE: '//NF_STRERROR(IS) )
           RETURN
      END IF
!
! --- Declare three dimensions: lon, lat, tim
!
      IS = NF_DEF_DIM ( NCID, 'lon', MALO%NLON, ID_DIM_LON )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 6415, IUER, 'SPR_WRITE_NC', 'Error in '// &
     &         ' an attempt to create new dimension lon: '// &
     &         NF_STRERROR(IS) )
           RETURN
      END IF
!
      IS = NF_DEF_DIM ( NCID, 'lat', MALO%NLAT, ID_DIM_LAT )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 6416, IUER, 'SPR_WRITE_NC', 'Error in '// &
     &         ' an attempt to create new dimension lat: '// &
     &         NF_STRERROR(IS) )
           RETURN
      END IF
!
      IS = NF_DEF_DIM ( NCID, 'time', NTIM, ID_DIM_TIM )
      IF ( IS .NE. 0 ) THEN
           WRITE ( 6, * ) ' '
           WRITE ( 6, * ) ' NTIM', NTIM
           WRITE ( 6, * ) ' MJD_BEG = ', MJD_BEG, ' MJD_BEG = ', MJD_BEG
           WRITE ( 6, * ) ' UTC_BEG = ', UTC_BEG, ' UTC_BEG = ', UTC_BEG
           WRITE ( 6, * ) ' MJD_END = ', MJD_END, ' MJD_END = ', MJD_END
           WRITE ( 6, * ) ' UTC_END = ', UTC_END, ' UTC_END = ', UTC_END
           CALL ERR_LOG ( 6417, IUER, 'SPR_WRITE_NC', 'Error in '// &
     &         ' an attempt to create new dimension time for '// &
     &         'output file '//MALO%FILOUT(1:I_LEN(MALO%FILOUT))//' -- '// &
     &          NF_STRERROR(IS) )
           RETURN
      END IF
!
! --- Declare 4 variables: lon, lat, time, and spr
!
      IS = NF_DEF_VAR ( NCID, 'lon', NF_FLOAT, 1, 1, ID_VAR_LON )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 6418, IUER, 'SPR_WRITE_NC', 'Error in '// &
     &         ' an attempt to create new variable lon: '// &
     &         NF_STRERROR(IS) )
           RETURN
      END IF
!
      IS = NF_DEF_VAR ( NCID, 'lat', NF_FLOAT, 1, 2, ID_VAR_LAT )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 6419, IUER, 'SPR_WRITE_NC', 'Error in '// &
     &         ' an attempt to create new variable lat: '// &
     &         NF_STRERROR(IS) )
           RETURN
      END IF
!
      IS = NF_DEF_VAR ( NCID, 'time', NF_DOUBLE, 1, 3, ID_VAR_TIM )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 6420, IUER, 'SPR_WRITE_NC', 'Error in '// &
     &         ' an attempt to create new variable time: '// &
     &         NF_STRERROR(IS) )
           RETURN
      END IF
!
      IS = NF_DEF_VAR ( NCID, 'sur_pres', NF_FLOAT, 3, VECDIM, ID_VAR_SPR )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 6421, IUER, 'SPR_WRITE_NC', 'Error in '// &
     &         ' an attempt to create new variable sur_pres: '// &
     &         NF_STRERROR(IS) )
           RETURN
      END IF
!
! --- Keep variables IDs
!
      ID_VAR(1) = ID_VAR_LON
      ID_VAR(2) = ID_VAR_LAT
      ID_VAR(3) = ID_VAR_TIM
      ID_VAR(4) = ID_VAR_SPR
!
      LON_RANGE(1) = MALO%LON(1)
      LON_RANGE(2) = MALO%LON(MALO%NLON)
!
      LAT_RANGE(1) = MALO%LAT(1)
      LAT_RANGE(2) = MALO%LAT(MALO%NLAT)
!
      TIM_RANGE(1) = (MALO%MJD_ARR(IND_BEG) - J2000__MJD)*86400.0D0 + MALO%TAI_ARR(IND_BEG)
      TIM_RANGE(2) = (MALO%MJD_ARR(IND_END) - J2000__MJD)*86400.0D0 + MALO%TAI_ARR(IND_END)
!
      SPR_RANGE(1) = MINVAL ( MALO%SPR(1:MALO%NLON,1:MALO%NLAT,IND_BEG:IND_END) )
      SPR_RANGE(2) = MAXVAL ( MALO%SPR(1:MALO%NLON,1:MALO%NLAT,IND_BEG:IND_END) )
!
! --- Put attributes for longitudes, lattude, time
!
      IS = NF_PUT_ATT_TEXT ( NCID, ID_VAR(1), 'units', LEN('radian'), 'radian' )
      IS = NF_PUT_ATT_TEXT ( NCID, ID_VAR(1), 'long_name', LEN('Longutde'), 'Longitude' )
      IS = NF_PUT_ATT_TEXT ( NCID, ID_VAR(1), 'standard_name', LEN('longitude'), 'longitude' )
      IS = NF_PUT_ATT_TEXT ( NCID, ID_VAR(1), 'axis', LEN('X'), 'X' )
      IS = NF_PUT_ATT_REAL ( NCID, ID_VAR(1), 'actual_range', NF_REAL, 2, LON_RANGE )
!
      IS = NF_PUT_ATT_TEXT ( NCID, ID_VAR(2), 'units', LEN('radian'), 'radian' )
      IS = NF_PUT_ATT_TEXT ( NCID, ID_VAR(2), 'long_name', LEN('Latitude'), 'Latitude' )
      IS = NF_PUT_ATT_TEXT ( NCID, ID_VAR(2), 'standard_name', LEN('latitude'), 'latitude' )
      IS = NF_PUT_ATT_TEXT ( NCID, ID_VAR(2), 'axis', LEN('Y'), 'Y' )
      IS = NF_PUT_ATT_REAL ( NCID, ID_VAR(2), 'actual_range', NF_REAL, 2, LAT_RANGE )
!
      IS = NF_PUT_ATT_TEXT   ( NCID, ID_VAR(3), 'units', LEN('sec'), 'sec' )
      IS = NF_PUT_ATT_TEXT   ( NCID, ID_VAR(3), 'long_name', LEN('Time in TAI since J2000.0'), &
     &                                         'Time in TAI since J2000.0' )
      IS = NF_PUT_ATT_TEXT   ( NCID, ID_VAR(3), 'standard_name', LEN('time'), 'time' )
      IS = NF_PUT_ATT_TEXT   ( NCID, ID_VAR(3), 'axis', LEN('T'), 'T' )
      IS = NF_PUT_ATT_DOUBLE ( NCID, ID_VAR(3), 'actual_range', NF_DOUBLE, 2, TIM_RANGE )
!
      DO 410 J1=1,MALO%NUM_ATT(4)
         IF ( MALO%NAM_ATT(J1,4) == 'long_name'   .OR. &
     &        MALO%NAM_ATT(J1,4) == 'var_desc'    .OR. & 
     &        MALO%NAM_ATT(J1,4) == 'dataset'     .OR. & 
     &        MALO%NAM_ATT(J1,4) == 'units'       .OR. & 
     &        MALO%NAM_ATT(J1,4) == 'level_desc'  .OR. & 
     &        MALO%NAM_ATT(J1,4) == 'statistic'   .OR. & 
     &        MALO%NAM_ATT(J1,4) == 'parent_stat' .OR. & 
     &        MALO%NAM_ATT(J1,4) == 'GRIB_name'        ) THEN
              CALL CLRCH ( STR )
              CALL MEMCPY ( %REF(STR), %REF(MALO%VAL_ATT(1,J1,4)), &
     &                      %VAL(MALO%LEN_ATT(J1,4)) )
              IS = NF_PUT_ATT_TEXT ( NCID, ID_VAR(4), MALO%NAM_ATT(J1,4), ILEN(STR), STR )
           ELSE IF ( MALO%NAM_ATT(J1,4) == 'GRIB_id' ) THEN
              CALL MEMCPY ( VAL_I2, %REF(MALO%VAL_ATT(1,J1,4)), %VAL(2) )
              IS = NF_PUT_ATT_INT2 ( NCID, ID_VAR(4), 'GRIB_id', NF_INT2, 2, VAL_I2 )
           ELSE IF ( MALO%NAM_ATT(J1,4) == 'missing_value' ) THEN
              IS = NF_PUT_ATT_REAL ( NCID, ID_VAR(4), 'missing_value', NF_REAL, 1, MALO%SPR_MISSING )
            ELSE IF ( MALO%NAM_ATT(J1,4) == 'scale_factor' ) THEN
              IS = NF_PUT_ATT_REAL ( NCID, ID_VAR(4), 'scale_factor',  NF_REAL, 1, 1.0 )
           ELSE IF ( MALO%NAM_ATT(J1,4) == 'add_offset' ) THEN
              IS = NF_PUT_ATT_REAL ( NCID, ID_VAR(4), 'add_offset',    NF_REAL, 1, 0.0 )
           ELSE IF ( MALO%NAM_ATT(J1,4) == 'actual_range' ) THEN
              IS = NF_PUT_ATT_REAL ( NCID, ID_VAR(4), 'actual_range',  NF_REAL, 2, SPR_RANGE )
           ELSE IF ( MALO%NAM_ATT(J1,4) == 'valid_range' ) THEN
              IS = NF_PUT_ATT_REAL ( NCID, ID_VAR(4), 'valid_range',   NF_REAL, 2, SPR_RANGE )
         END IF
 410  CONTINUE 
!
! --- Setting global attributes  
!
! --- date_beg, date_end, mjd_beg, mjd_end, tai_beg, tai_end
!
      STR = SPR_WRITE_NC__LABEL
      IS = NF_PUT_ATT_TEXT ( NCID, 0, 'Generated_by', I_LEN(STR), STR(1:I_LEN(STR)) )
      STR = GET_CDATE()
      IS = NF_PUT_ATT_TEXT ( NCID, 0, 'Generated_on', I_LEN(STR), STR(1:I_LEN(STR)) )
!
      IS  = NF_PUT_ATT_TEXT ( NCID, 0, 'Data_type', LEN(MALO%DATA_TYPE), MALO%DATA_TYPE )
      IF ( MALO%DATA_TYPE == 'Time_series_NCEP_Reanalysis_96  ' ) THEN
           STR = MJDSEC_TO_DATE ( MALO%MJD_ARR(IND_BEG), MALO%TAI_ARR(IND_BEG), IER )
           IS  = NF_PUT_ATT_TEXT ( NCID, 0, 'Start_epoch_TAI', 24, STR(1:24) )
           STR = MJDSEC_TO_DATE ( MALO%MJD_ARR(IND_END), MALO%TAI_ARR(IND_END), IER )
           IS  = NF_PUT_ATT_TEXT ( NCID, 0, 'Stop_epoch_UTC',  24, STR(1:24) )
           IS = NF_PUT_ATT_DOUBLE ( NCID, 0, 'Time_step', NF_DOUBLE, 1, EPOCH_STEP )
         ELSE IF ( MALO%DATA_TYPE == 'Mean_Pressure_NCEP_Reanalysis_96' ) THEN
           STR = MJDSEC_TO_DATE  ( MALO%MJD_BEG, MALO%TAI_BEG, IER )
           IS  = NF_PUT_ATT_TEXT ( NCID, 0, 'Start_epoch_TAI', 24, STR(1:24) )
           STR = MJDSEC_TO_DATE  ( MALO%MJD_END, MALO%TAI_END, IER )
           IS  = NF_PUT_ATT_TEXT ( NCID, 0, 'Stop_epoch_TAI', 24, STR(1:24) )
           IS = NF_PUT_ATT_INT   ( NCID, ID_VAR(4), 'Number_of_points_used', &
     &                             NF_INT, 1, MALO%N_ACC_TIM )
      END IF
      IS  = NF_PUT_ATT_INT  ( NCID, 0, 'Start_date_MJD', NF_INT,  1, MJD_BEG )
      IS  = NF_PUT_ATT_REAL ( NCID, 0, 'Start_time_UTC', NF_REAL, 1, SNGL(UTC_BEG) )
      IS  = NF_PUT_ATT_INT  ( NCID, 0, 'Stop_date_MJD',  NF_INT,  1, MJD_END )
      IS  = NF_PUT_ATT_REAL ( NCID, 0, 'Stop_time_UTC',  NF_REAL, 1, SNGL(UTC_END) )
!
! --- Enough hassle with attributes! It is so boring...
!
      IS = NF_ENDDEF ( NCID )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 6422, IUER, 'SPR_WRITE_NC', 'Error in '// &
     &         ' NF_ENDDEF for output file '// &
     &         MALO%FILOUT(1:I_LEN(MALO%FILOUT))//' error: '// &
     &         NF_STRERROR(IS) )
           RETURN
      END IF
!
! --- Writing longitude
!
      IS = NF_PUT_VAR_REAL ( NCID, ID_VAR_LON, MALO%LON )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 6423, IUER, 'SPR_WRITE_NC', 'Error in '// &
     &         ' an attempt to write varabile longitude in the '// &
     &         'output file '//MALO%FILOUT(1:I_LEN(MALO%FILOUT))// &
     &         ' -- error: '//NF_STRERROR(IS) )
           RETURN
      END IF
!
! --- Writing latitude
!
      IS = NF_PUT_VAR_REAL ( NCID, ID_VAR_LAT, MALO%LAT )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 6424, IUER, 'SPR_WRITE_NC', 'Error in '// &
     &         ' an attempt to write varabile latitude in the '// &
     &         'output file '//MALO%FILOUT(1:I_LEN(MALO%FILOUT))// &
     &         ' -- error: '//NF_STRERROR(IS) )
           RETURN
      END IF
!
      ALLOCATE ( TIM_ARR(MALO%NTIM), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( 8*MALO%NTIM, STR )
           CALL ERR_LOG ( 6425, IUER, 'SPR_WRITE_NC', 'Error in '// &
     &         ' an attempt to allocate '//STR(1:I_LEN(STR))// &
     &         ' bytes of dynamic memory' )
           RETURN
      END IF
      IND_EPC = 0
      DO 420 J2=IND_BEG,IND_END
         IND_EPC = IND_EPC + 1
         TIM_ARR(IND_EPC) = (MALO%MJD_ARR(J2) - J2000__MJD)*86400.0D0 + &
     &                       MALO%TAI_ARR(J2)
 420  CONTINUE 
!
! --- Writing time
!
      IS = NF_PUT_VAR_DOUBLE ( NCID, ID_VAR_TIM, TIM_ARR )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 6426, IUER, 'SPR_WRITE_NC', 'Error in '// &
     &         ' an attempt to write varabile latitude in the '// &
     &         'output file '//MALO%FILOUT(1:I_LEN(MALO%FILOUT))// &
     &         ' -- error: '//NF_STRERROR(IS) )
           RETURN
      END IF
      DEALLOCATE ( TIM_ARR )
!
! --- Writing surface pressure 
!
      IS = NF_PUT_VAR_REAL ( NCID, ID_VAR_SPR, &
     &        MALO%SPR(1:MALO%NLON,1:MALO%NLAT,IND_BEG:IND_END) )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 6427, IUER, 'SPR_WRITE_NC', 'Error in '// &
     &         ' an attempt to write varabile "surface pressure" in the '// &
     &         'output file '//MALO%FILOUT(1:I_LEN(MALO%FILOUT))// &
     &         ' -- error: '//NF_STRERROR(IS) )
           RETURN
      END IF
!
! --- Uph! Close file and go home to drink hot tea (or cold bear)
!
      IS = NF_CLOSE ( NCID )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 6239, IUER, 'SPR_WRITE_NC', 'Error in an attempt '// &
     &         'to close the output netcf file '// &
     &          MALO%FILOUT(1:I_LEN(MALO%FILOUT))//' NF_CLOSE: '// &
     &          NF_STRERROR(IS) )
           RETURN
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE SPR_WRITE_NC  !#!#
