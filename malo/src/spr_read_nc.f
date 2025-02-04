      SUBROUTINE SPR_READ_NC ( FILIN, MALO, IVRB, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine SPR_READ_NC 
! *                                                                      *
! *   FILIN ( CHARACTER  ) -- File in netCDF format with surface         *
! *                           pressure.                                  *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *    MALO ( MALO__TYPE ) -- Object that keesp variuables and arrays    *
! *                           relaveant to MAss LOading software package.*
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
! *  ###  17-OCT-2012   SPR_READ_NC  v1.1 (c) L. Petrov 10-NOV-2012 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'malo.i'
      INCLUDE   'netcdf.inc'
      TYPE     ( MALO__TYPE ) :: MALO
      CHARACTER  FILIN*(*)
      INTEGER*4  IVRB, IUER
!
      INTEGER*4  IS, NCID, LEN_ATM3
      INTEGER*4  ID_VAR_LON, ID_VAR_LAT, ID_VAR_TIM, ID_VAR_SPR, &
     &           ID_DIM_LON, ID_DIM_LAT, ID_DIM_TIM, &
     &           DIMLEN_LON, DIMLEN_LAT, DIMLEN_TIM, ID_VAR(0:4), &
     &           J1, J2, J3, UNIX_DATE, IP, IER
      INTEGER*8  SIZE_I8
      REAL*8     RANGE_R8(2)
      REAL*4     UTC_BEG_R4, UTC_END_R4
      REAL*8,    ALLOCATABLE :: TIM_ARR(:)
      CHARACTER  STR*128, STR1*128
      CHARACTER, EXTERNAL :: MJDSEC_TO_DATE*30, GET_CDATE*19
      INTEGER*4, EXTERNAL :: ILEN, I_LEN, FILE_INFO 
!
! --- Open the NETcdf database
!
      IF ( IVRB .GE. 3 ) THEN 
           CALL WALL_TIMER ( %VAL(0) )
      END IF
      IS = NF_OPEN ( FILIN, NF_NOWRITE, NCID )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 6441, IUER, 'SPR_READ_NC', 'File '// &
     &          FILIN(1:I_LEN(FILIN))//' -- Error in NF_OPEN: '// &
     &          NF_STRERROR(IS)//' File: '//FILIN )
           RETURN
      END IF
!
! --- Learn the ID of the dimension "lon"
!
      IS = NF_INQ_DIMID ( NCID, 'lon', ID_DIM_LON )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 6442, IUER, 'SPR_READ_NC', 'Dimension "lon" was '// &
     &         'not found in the input netcdf file '// &
     &          FILIN(1:I_LEN(FILIN))//' error: '// &
     &          NF_STRERROR(IS)//' File: '//FILIN )
           RETURN
      END IF
!
! --- Get the length of the dimension "lon"
!
      IS = NF_INQ_DIMLEN ( NCID, ID_DIM_LON, DIMLEN_LON  )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 6443, IUER, 'SPR_READ_NC', 'Error in getting the '// &
     &         'length of the dimension "lon" in file '// &
     &          FILIN(1:I_LEN(FILIN))//' error: '// &
     &          NF_STRERROR(IS) )
           RETURN
      END IF
!
! --- Learn the ID of the dimension "lat"
!
      IS = NF_INQ_DIMID ( NCID, 'lat', ID_DIM_LAT )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 6444, IUER, 'SPR_READ_NC', 'Dimension "lat" was '// &
     &                   'not found in the input netcdf file '// &
     &                    FILIN(1:I_LEN(FILIN))//' error: '// &
     &                    NF_STRERROR(IS) )
           RETURN
      END IF
!
! --- Get the length of the dimension "lat"
!
      IS = NF_INQ_DIMLEN ( NCID, ID_DIM_LAT, DIMLEN_LAT  )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 6445, IUER, 'SPR_READ_NC', 'Error in getting the '// &
     &                   'length of the dimension "lat" in file '// &
     &                    FILIN(1:I_LEN(FILIN))//' error: '// &
     &                    NF_STRERROR(IS) )
           RETURN
      END IF
!
! --- Learn the ID of the dimension "time"
!
      IS = NF_INQ_DIMID ( NCID, 'time', ID_DIM_TIM )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 6446, IUER, 'SPR_READ_NC', 'Dimension "time" was '// &
     &                   'not found in the input netcdf file '// &
     &                    FILIN(1:I_LEN(FILIN))//' error: '// &
     &                    NF_STRERROR(IS) )
           RETURN
      END IF
!
! --- Get the length of the dimension "time"
!
      IS = NF_INQ_DIMLEN ( NCID, ID_DIM_TIM, DIMLEN_TIM )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 6447, IUER, 'SPR_READ_NC', 'Error in getting the '// &
     &                   'length of the dimension "time" in file '// &
     &                    FILIN(1:I_LEN(FILIN))//' error: '// &
     &                    NF_STRERROR(IS) )
           RETURN
      END IF
!
! --- Learn the ID of the variable "lon"
!
      IS = NF_INQ_VARID ( NCID, 'lon', ID_VAR_LON )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 6448, IUER, 'SPR_READ_NC', 'Variable "lon" was '// &
     &         'not found in the input netcdf file '// &
     &          FILIN(1:I_LEN(FILIN))//' error: '//NF_STRERROR(IS) )
           RETURN
      END IF
!
! --- Learn the ID of the variable "lat"
!
      IS = NF_INQ_VARID ( NCID, 'lat', ID_VAR_LAT )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 6449, IUER, 'SPR_READ_NC', 'Variable "lat" was '// &
     &         'not found in the input netcdf file '// &
     &          FILIN(1:I_LEN(FILIN))//' error: '//NF_STRERROR(IS) )
           RETURN
      END IF
!
! --- Learn the ID of the variable "time"
!
      IS = NF_INQ_VARID ( NCID, 'time', ID_VAR_TIM )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 6450, IUER, 'SPR_READ_NC', 'Variable "time" was '// &
     &         'not found in the input netcdf file '// &
     &          FILIN(1:I_LEN(FILIN))//' error: '//NF_STRERROR(IS) )
           RETURN
      END IF
!
! --- Learn the ID of the variable "sur_pres"
!
      IS = NF_INQ_VARID ( NCID, 'sur_pres', ID_VAR_SPR )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 6451, IUER, 'SPR_READ_NC', 'Variable "sur_pres" '// &
     &         'was not found in the input netcdf file '// &
     &          FILIN(1:I_LEN(FILIN))//' error: '//NF_STRERROR(IS) )
           RETURN
      END IF
!
      ID_VAR(0) = 0
      ID_VAR(1) = ID_VAR_LON
      ID_VAR(2) = ID_VAR_LAT
      ID_VAR(3) = ID_VAR_TIM
      ID_VAR(4) = ID_VAR_SPR
!
      MALO%NLON = DIMLEN_LON
      MALO%NLAT = DIMLEN_LAT
      MALO%NTIM = DIMLEN_TIM
!
! --- Get attributes of all exported variables and store it in the fields of
! --- the object MALO
!
      DO 410 J1=0,4
!
! ------ Learn the number of attributes
!
         IS = NF_INQ_VARNATTS ( NCID, ID_VAR(J1), MALO%NUM_ATT(J1) )
         IF ( IS .NE. 0 ) THEN
              CALL ERR_LOG ( 6452, IUER, 'SPR_READ_NC', 'Error in getting '// &
     &            'the number of attributes '//NF_STRERROR(IS) )
              RETURN
         END IF
!
! ------ Cycle over attributes of the J1-th variable
!
         DO 420 J2=1,MALO%NUM_ATT(J1)
!
! --------- Get information about the attribute
!
            IS = NF_INQ_ATTNAME ( NCID, ID_VAR(J1), J2, MALO%NAM_ATT(J2,J1) )
            IF ( IS .NE. 0 ) THEN
                 WRITE ( 6, * ) 'J1=',J1,' J2=',J2
                 CALL ERR_LOG ( 6453, IUER, 'SPR_READ_NC', 'Error in getting '// &
     &               'information about the attribute while reading '// &
     &               'the netcdf file'//FILIN(1:I_LEN(FILIN))//' '// &
     &                NF_STRERROR(IS) )
                 RETURN
            END IF
!
! --------- Get information about the attribute
!
            IS = NF_INQ_ATT ( NCID, ID_VAR(J1),   MALO%NAM_ATT(J2,J1), &
     &                        MALO%TYP_ATT(J2,J1), MALO%LEN_ATT(J2,J1)  )
            IF ( IS .NE. 0 ) THEN
                 WRITE ( 6, * ) 'J1=',J1,' J2=',J2
                 CALL ERR_LOG ( 6454, IUER, 'SPR_READ_NC', 'Error in getting '// &
     &               'information about the attribute '// &
     &                MALO%NAM_ATT(J2,J1)(1:I_LEN(MALO%NAM_ATT(J2,J1)))// &
     &               ' while reading the netcdf file'// &
     &                FILIN(1:I_LEN(FILIN))//' '//NF_STRERROR(IS) )
                 RETURN
            END IF
!
! --------- Get the value of the attribute
!
            IF ( MALO%TYP_ATT(J2,J1) .EQ. NF_BYTE ) THEN
                 IS = NF_GET_ATT_INT1 ( NCID, ID_VAR(J1), MALO%NAM_ATT(J2,J1), &
     &                                  MALO%VAL_ATT(1,J2,J1) )
               ELSE IF ( MALO%TYP_ATT(J2,J1) .EQ. NF_CHAR   ) THEN
                 IS = NF_GET_ATT_TEXT ( NCID, ID_VAR(J1), MALO%NAM_ATT(J2,J1), &
     &                                  %REF(MALO%VAL_ATT(1,J2,J1)) )
               ELSE IF ( MALO%TYP_ATT(J2,J1) .EQ. NF_SHORT  ) THEN
                 IS = NF_GET_ATT_INT2 ( NCID, ID_VAR(J1), MALO%NAM_ATT(J2,J1), &
     &                                  MALO%VAL_ATT(1,J2,J1) )
               ELSE IF ( MALO%TYP_ATT(J2,J1) .EQ. NF_INT    ) THEN
                 IS = NF_GET_ATT_INT ( NCID, ID_VAR(J1), MALO%NAM_ATT(J2,J1), &
     &                                 MALO%VAL_ATT(1,J2,J1) )
               ELSE IF ( MALO%TYP_ATT(J2,J1) .EQ. NF_FLOAT  ) THEN
                 IS = NF_GET_ATT_REAL ( NCID, ID_VAR(J1), MALO%NAM_ATT(J2,J1), &
     &                                  MALO%VAL_ATT(1,J2,J1) )
               ELSE IF ( MALO%TYP_ATT(J2,J1) .EQ. NF_DOUBLE ) THEN
                 IS = NF_GET_ATT_DOUBLE ( NCID, ID_VAR(J1), MALO%NAM_ATT(J2,J1), &
     &                                    MALO%VAL_ATT(1,J2,J1) )
            END IF
!
            IF ( IS .NE. 0 ) THEN
                 CALL ERR_LOG ( 6455, IUER, 'SPR_READ_NC', 'Error in '// &
     &               'getting the value of the atttribute '// &
     &               MALO%NAM_ATT(J2,J1)//' while reading the netcdf file'// &
     &               FILIN(1:I_LEN(FILIN))//' error: '//NF_STRERROR(IS) )
                 RETURN
            END IF
!
            IF ( ID_VAR(J1) .EQ. ID_VAR_TIM .AND. &
     &           MALO%NAM_ATT(J2,J1) .EQ. 'actual_range' ) THEN
!
                 CALL LIB$MOVC3  ( 16, MALO%VAL_ATT(1,J2,J1), RANGE_R8 )
                 MALO%MJD_BEG = J2000__MJD + RANGE_R8(1)/86400.0D0
                 MALO%TAI_BEG = RANGE_R8(1) - (MALO%MJD_BEG - J2000__MJD)*86400.0D0
                 MALO%MJD_END = J2000__MJD + RANGE_R8(2)/86400.0D0
                 MALO%TAI_END = RANGE_R8(2) - (MALO%MJD_END - J2000__MJD)*86400.0D0
            END IF
            IF ( ID_VAR(J1) .EQ. 0 .AND. &
     &           MALO%NAM_ATT(J2,J1) .EQ. 'Data_type' ) THEN
                 CALL CLRCH     ( MALO%DATA_TYPE )
                 CALL LIB$MOVC3 ( LEN(MALO%DATA_TYPE), MALO%VAL_ATT(1,J2,J1), &
     &                            MALO%DATA_TYPE )
            END IF
            IF ( ID_VAR(J1) .EQ. 0 .AND. &
     &           MALO%NAM_ATT(J2,J1) .EQ. 'Start_time_UTC' ) THEN
                 CALL LIB$MOVC3 ( 4, MALO%VAL_ATT(1,J2,J1), UTC_BEG_R4 )
                 MALO%UTC_BEG = UTC_BEG_R4
            END IF
            IF ( ID_VAR(J1) .EQ. 0 .AND. &
     &           MALO%NAM_ATT(J2,J1) .EQ. 'Stop_time_UTC' ) THEN
                 CALL LIB$MOVC3 ( 4, MALO%VAL_ATT(1,J2,J1), UTC_END_R4 )
                 MALO%UTC_END = UTC_END_R4
            END IF
 420     CONTINUE
 410  CONTINUE
!
      IF ( IVRB .GE. 2 ) THEN
           STR  = MJDSEC_TO_DATE ( MALO%MJD_BEG, MALO%TAI_BEG, -2 )
           STR1 = MJDSEC_TO_DATE ( MALO%MJD_END, MALO%TAI_END, -2 )
           WRITE  ( 6, 110 ) FILIN(1:I_LEN(FILIN)), STR(1:24), STR1(1:24), &
     &                       MALO%NLON, MALO%NLAT, MALO%NTIM
 110       FORMAT ( '  Reading File ', A/ &
     &              '  Date_beg: ', A, ' Date_end: ', A/ &
     &              '  Num_longitudes: ', I4 / &
     &              '  Num_latitudes:  ', I4 / &
     &              '  Num_epochs:     ', I4   )
      END IF
!
      ALLOCATE ( MALO%LON(MALO%NLON), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH  ( 4*MALO%NLON, STR )
           CALL ERR_LOG ( 6456, IUER, 'SPR_READ_NC', 'Failure to allocate '// &
     &          STR(1:I_LEN(STR))//' bytes dynamic memory for array MALO%LON' )
           RETURN 
      END IF 
!
      ALLOCATE ( MALO%LAT(MALO%NLAT), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( 4*MALO%NLAT, STR )
           CALL ERR_LOG ( 6457, IUER, 'SPR_READ_NC', 'Failure to allocate '// &
     &          STR(1:I_LEN(STR))//' bytes dynamic memory for array MALO%LAT' )
           RETURN 
      END IF 
!
      ALLOCATE ( MALO%MJD_ARR(MALO%NTIM), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( 4*MALO%NTIM, STR )
           CALL ERR_LOG ( 6458, IUER, 'SPR_READ_NC', 'Failure to allocate '// &
     &          STR(1:I_LEN(STR))//' bytes dynamic memory for array MALO%LAT' )
           RETURN 
      END IF 
!
      ALLOCATE ( MALO%TAI_ARR(MALO%NTIM), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( 4*MALO%NTIM, STR )
           CALL ERR_LOG ( 6459, IUER, 'SPR_READ_NC', 'Failure to allocate '// &
     &          STR(1:I_LEN(STR))//' bytes dynamic memory for array MALO%LAT' )
           RETURN 
      END IF 
!
      ALLOCATE ( MALO%SPR(MALO%NLON,MALO%NLAT,MALO%NTIM), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( 4*MALO%NLON*MALO%NLAT*MALO%NTIM, STR )
           CALL ERR_LOG ( 6460, IUER, 'SPR_READ_NC', 'Failure to allocate '// &
     &          STR(1:I_LEN(STR))//' bytes dynamic memory for array MALO%LAT' )
           RETURN 
      END IF 
      MALO%SPR_STATUS = MALO__ALLO
!
! --- Get the variable: longitude.
!
      IS = NF_GET_VAR_REAL ( NCID, ID_VAR_LON, MALO%LON )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 6461, IUER, 'SPR_READ_NC', 'Error in getting the '// &
     &                   'values of the variable "lon" in file '// &
     &                    FILIN(1:I_LEN(FILIN))//' error: '// &
     &                    NF_STRERROR(IS) )
           RETURN
      END IF
!
! --- Get the variable: latitude.
!
      IS = NF_GET_VAR_REAL ( NCID, ID_VAR_LAT, MALO%LAT )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 6462, IUER, 'SPR_READ_NC', 'Error in getting the '// &
     &                   'values of the variable "lat" in file '// &
     &                    FILIN(1:I_LEN(FILIN))//' error: '// &
     &                    NF_STRERROR(IS) )
           RETURN
      END IF
!
      ALLOCATE ( TIM_ARR(MALO%NTIM), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( 8*MALO%NTIM, STR )
           CALL ERR_LOG ( 6463, IUER, 'SPR_READ_NC', 'Failure to allocate '// &
     &          STR(1:I_LEN(STR))//' bytes dynamic memory for array MALO%LAT' )
           RETURN 
      END IF 
!
! --- Get the variable: time tag
!
      IS = NF_GET_VAR_DOUBLE ( NCID, ID_VAR_TIM, TIM_ARR )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 6464, IUER, 'SPR_READ_NC', 'Error in getting the '// &
     &                   'values dimension "time" in file '// &
     &                    FILIN(1:I_LEN(FILIN))//' error: '// &
     &                    NF_STRERROR(IS) )
           RETURN
      END IF
!
      DO 430 J3=1,MALO%NTIM
         MALO%MJD_ARR(J3) = J2000__MJD  + TIM_ARR(J3)/86400.0D0
         MALO%TAI_ARR(J3) = TIM_ARR(J3) - (MALO%MJD_ARR(J3) - J2000__MJD)*86400.0D0
         STR  = MJDSEC_TO_DATE ( MALO%MJD_ARR(J3), MALO%TAI_ARR(J3), -2 )
 430  CONTINUE 
      DEALLOCATE ( TIM_ARR )
!
! --- Get the variable: surface pressure
!
      IS = NF_GET_VAR_REAL ( NCID, ID_VAR_SPR, MALO%SPR )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 6465, IUER, 'SPR_READ_NC', 'Error in getting '// &
     &                   'the values of the variable "lat" in file '// &
     &                    FILIN(1:I_LEN(FILIN))//' error: '// &
     &                    NF_STRERROR(IS) )
           RETURN
      END IF
      MALO%SPR_STATUS = MALO__LOAD
!
! --- Close file and go home to drink hot tea with honey
!
      IS = NF_CLOSE ( NCID )
      IF ( IVRB .GE. 3 ) THEN
           CALL WALL_TIMER ( STR )
           IP = INDEX ( STR, '.' )
           IS = FILE_INFO ( FILIN(1:I_LEN(FILIN)), UNIX_DATE, SIZE_I8 )
           WRITE  ( 6, 120 ) FILIN(1:I_LEN(FILIN)), &
     &                       SIZE_I8/1024.D0/1024.D0, STR(1:IP+2)
 120       FORMAT ( 'We read file ', A, ' ', F8.1, ' Mb for ',A, ' sec' )
         ELSE IF ( IVRB == 2 ) THEN
           WRITE  ( 6, 120 ) FILIN(1:I_LEN(FILIN))
 130       FORMAT ( 'We have read file ', A )
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  SPR_READ_NC  !#!#
