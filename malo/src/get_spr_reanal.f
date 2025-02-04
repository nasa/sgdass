      SUBROUTINE GET_SPR_REANAL ( FILIN, MAL, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  GET_SPR_REANAL  extracts surface preassure from the input *
! *   file in netCDF fromat and puts in the SPR data structure.          *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *    FILIN ( CHARACTER ) -- File with meteorlogical data in netCDF     *
! *                           format.                                    *
! *                                                                      *
! * _________________________ Output parameters: _______________________ *
! *                                                                      *
! *      SPR ( RECORD    ) -- The object which keeps 3-D array with      *
! *                           surface pressure for a range of time       *
! *                           epochs.                                    *
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
! *     IUER ( INTEGER*4, OPT ) -- Universal error handler.              *
! *                           Input: switch IUER=0 -- no error messages  *
! *                                  will be generated even in the case  *
! *                                  of error. IUER=-1 -- in the case of *
! *                                  error the message will be put on    *
! *                                  stdout.                             *
! *                           Output: 0 in the case of successful        *
! *                                   completion and non-zero in the     *
! *                                   case of error.                     *
! *                                                                      *
! * ### 24-JUL-2002  GET_SPR_REANAL  v1.0 (c) L. Petrov  25-JUL-2002 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      CHARACTER  FILIN*(*)
      INTEGER*4  IUER
      INCLUDE   'malo.i'
      INCLUDE   'netcdf.inc'
      TYPE ( SPR__STRU ) ::  SPR
!
      INTEGER*4  IS, NCID
      REAL*8     MBAR_EPS
      PARAMETER  ( MBAR_EPS = 1.0D0 )
      CHARACTER  STR*32
      INTEGER*4  NDIMS, NVARS, NATTS, NUNL, &
     &           ID_DIM_LON, ID_DIM_LAT, ID_DIM_TIM, &
     &           DIMLEN_LON, DIMLEN_LAT, DIMLEN_TIM, &
     &           ID_VAR_LON, ID_VAR_LAT, ID_VAR_TIM, ID_VAR_SPR, &
     &           ATT_TYPE, ATT_LEN, ID_VAR(4), J1, J2, IER
      REAL*8     HRS_ARR(2)
      INTEGER*4  I_LEN
!
! --- Open the NETcdf database
!
      IS = NF_OPEN ( FILIN, NF_NOWRITE, NCID )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 6131, IUER, 'GET_SPR_REANAL', 'Error in NF_OPEN: '// &
     &                    NF_STRERROR(IS) )
           RETURN
      END IF
!
! --- Learn the number of dimensions, variables and attributes
!
      IS = NF_INQ ( NCID, NDIMS, NVARS, NATTS, NUNL )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 6102, IUER, 'GET_SPR_REANAL', 'Error in an attempt '// &
     &         'to open input file '//FILIN(1:I_LEN(FILIN))//' NF_INQ: '// &
     &          NF_STRERROR(IS) )
           RETURN
      END IF
!
! --- Learn the ID of the variable time
!
      IS = NF_INQ_VARID ( NCID, 'time', ID_VAR_TIM )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 6132, IUER, 'GET_SPR_REANAL', 'Variable "time" was '// &
     &                   'not found in the input netcdf file '// &
     &                    FILIN(1:I_LEN(FILIN))//' error: '// &
     &                    NF_STRERROR(IS) )
           RETURN
      END IF
!
! --- Get the ID of the attribute actual time range
!
      IS = NF_INQ_ATT ( NCID, ID_VAR_TIM, 'actual_range', ATT_TYPE, ATT_LEN )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 6132, IUER, 'GET_SPR_REANAL', 'Error in an '// &
     &         'attempt to get the attribute "atctual_range" of the '// &
     &         'variable "time" in file '//FILIN(1:I_LEN(FILIN))// &
     &         ' error: '//NF_STRERROR(IS) )
           RETURN
      END IF
!
      IF ( ATT_TYPE .NE. NF_DOUBLE .AND. ATT_LEN .NE. 2 ) THEN
!
! -------- Hmm. Attribute has different type and length than it was expected
!
           WRITE ( 6, * ) 'ATT_TYPE=',ATT_TYPE, ' ATT_LEN = ',ATT_LEN
           CALL ERR_LOG ( 6133, IUER, 'GET_SPR_REANAL', 'Attribute '// &
     &         '"atctual_range" of the variable "time" in file '// &
     &          FILIN(1:I_LEN(FILIN))//' has unexpected type and length' )
           RETURN
      END IF
!
! --- Get the values of the attribute actual time range
!
      IS = NF_GET_ATT_DOUBLE ( NCID, ID_VAR_TIM, 'actual_range', HRS_ARR )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 6134, IUER, 'GET_SPR_REANAL', 'Error in an '// &
     &         'attempt to get the value of the attribute "atctual_range" '// &
     &         'of the variable "time" in file '//FILIN(1:I_LEN(FILIN))// &
     &         ' error: '//NF_STRERROR(IS) )
           RETURN
      END IF
!
! --- Convert dates from mythological to astronomical format
!
      CALL HRS_TO_MJD ( HRS_ARR(1), SPR%MJD_BEG, SPR%SEC_BEG )
      CALL HRS_TO_MJD ( HRS_ARR(2), SPR%MJD_END, SPR%SEC_END )
!
! --- Learn the ID of the variable time
!
      IS = NF_INQ_DIMID ( NCID, 'time', ID_DIM_TIM )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 6135, IUER, 'GET_SPR_REANAL', 'Dimention "time" was '// &
     &                   'not found in the input netcdf file '// &
     &                    FILIN(1:I_LEN(FILIN))//' error: '// &
     &                    NF_STRERROR(IS) )
           RETURN
      END IF
!
! --- Get the lenght of the dimension TIME
!
      IS = NF_INQ_DIMLEN ( NCID, ID_DIM_TIM, DIMLEN_TIM  )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 6136, IUER, 'GET_SPR_REANAL', 'Error in getting the '// &
     &                   'length of the dimension "time" in file '// &
     &                    FILIN(1:I_LEN(FILIN))//' error: '// &
     &                    NF_STRERROR(IS) )
           RETURN
      END IF
!
! --- Learn the ID of the variable lon
!
      IS = NF_INQ_DIMID ( NCID, 'lon', ID_DIM_LON )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 6137, IUER, 'GET_SPR_REANAL', 'Dimention "lon" was '// &
     &                   'not found in the input netcdf file '// &
     &                    FILIN(1:I_LEN(FILIN))//' error: '// &
     &                    NF_STRERROR(IS) )
           RETURN
      END IF
!
! --- Get the length of the dimension longitude
!
      IS = NF_INQ_DIMLEN ( NCID, ID_DIM_LON, DIMLEN_LON  )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 6138, IUER, 'GET_SPR_REANAL', 'Error in getting the '// &
     &                   'length of the dimension "lon" in file '// &
     &                    FILIN(1:I_LEN(FILIN))//' error: '// &
     &                    NF_STRERROR(IS) )
           RETURN
      END IF
!
! --- Learn the ID of the variable lat
!
      IS = NF_INQ_DIMID ( NCID, 'lat', ID_DIM_LAT )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 6139, IUER, 'GET_SPR_REANAL', 'Dimention "lat" was '// &
     &                   'not found in the input netcdf file '// &
     &                    FILIN(1:I_LEN(FILIN))//' error: '// &
     &                    NF_STRERROR(IS) )
           RETURN
      END IF
!
! --- Get the length of the dimenstion lattitude
!
      IS = NF_INQ_DIMLEN ( NCID, ID_DIM_LAT, DIMLEN_LAT  )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 6140, IUER, 'GET_SPR_REANAL', 'Error in getting the '// &
     &                   'length of the dimension "lat" in file '// &
     &                    FILIN(1:I_LEN(FILIN))//' error: '// &
     &                    NF_STRERROR(IS) )
           RETURN
      END IF
!
! --- Store dimenstions
!
      SPR%L_LON = DIMLEN_LON
      SPR%L_LAT = DIMLEN_LAT
      SPR%L_TIM = DIMLEN_TIM
!
! --- Grab dynamic memory for the varaiables which will be read from the file
!
      SPR%L_MEM = SPR%L_MEM+1
      SPR%LEN_LON = 4*SPR%L_LON
      SPR%LEN_LAT = 4*SPR%L_LAT
      SPR%LEN_TIM = 8*SPR%L_TIM
      SPR%LEN_SPR = 2*SPR%L_LON*SPR%L_LAT*SPR%L_TIM
      CALL ERR_PASS ( IUER, IER )
      CALL GRAB_MEM ( IER, SPR%MEM_LEN(SPR%L_MEM), SPR%MEM_ADR(SPR%L_MEM), 4, &
     &                SPR%LEN_LON, SPR%ADR_LON, &
     &                SPR%LEN_LAT, SPR%ADR_LAT, &
     &                SPR%LEN_TIM, SPR%ADR_TIM, &
     &                SPR%LEN_SPR, SPR%ADR_SPR  )
      IF ( IER .NE. 0 ) THEN
           SPR%L_MEM = SPR%L_MEM-1
           CALL CLRCH ( STR )
#ifdef ADR_32BIT
           CALL IINCH  ( SPR%LEN_TIM + SPR%LEN_SPR, STR )
#else
           CALL IINCH8 ( SPR%LEN_TIM + SPR%LEN_SPR, STR )
#endif
           CALL ERR_LOG ( 6141, IUER, 'GET_SPR_REANAL', 'Error in an attempt '// &
     &         'grab '//STR(1:I_LEN(STR))//' bytes of dynamic memory' )
           RETURN
         ELSE
           SPR%STS_TIM = RCD__ALL
           SPR%STS_SPR = RCD__ALL
      END IF
!
! --- Learn the ID of the variable longitude
!
      IS = NF_INQ_VARID ( NCID, 'lon', ID_VAR_LON )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 6142, IUER, 'GET_SPR_REANAL', 'Variable "lon" was '// &
     &                   'not found in the input netcdf file '// &
     &                    FILIN(1:I_LEN(FILIN))//' error: '// &
     &                    NF_STRERROR(IS) )
           RETURN
      END IF
!
! --- Get the varaiable: longitude.
!
      IS = NF_GET_VAR_REAL ( NCID, ID_VAR_LON, %VAL(SPR%ADR_LON) )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 6143, IUER, 'GET_SPR_REANAL', 'Error in getting the '// &
     &                   'values of the variable "lon" in file '// &
     &                    FILIN(1:I_LEN(FILIN))//' error: '// &
     &                    NF_STRERROR(IS) )
           RETURN
         ELSE
           SPR%STS_LON = RCD__GET
      END IF
!
! --- Learn the ID of the variable latitude
!
      IS = NF_INQ_VARID ( NCID, 'lat', ID_VAR_LAT )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 6144, IUER, 'GET_SPR_REANAL', 'Variable "lat" was '// &
     &                   'not found in the input netcdf file '// &
     &                    FILIN(1:I_LEN(FILIN))//' error: '// &
     &                    NF_STRERROR(IS) )
           RETURN
      END IF
!
! --- Get the varaiable: latitude.
!
      IS = NF_GET_VAR_REAL ( NCID, ID_VAR_LAT, %VAL(SPR%ADR_LAT) )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 6145, IUER, 'GET_SPR_REANAL', 'Error in getting the '// &
     &                   'values of the variable "lat" in file '// &
     &                    FILIN(1:I_LEN(FILIN))//' error: '// &
     &                    NF_STRERROR(IS) )
           RETURN
         ELSE
           SPR%STS_LAT = RCD__GET
      END IF
!
! --- Get the varaiable: time tag. We store it in mythological date format
!
      IS = NF_GET_VAR_DOUBLE ( NCID, ID_VAR_TIM, %VAL(SPR%ADR_TIM) )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 6146, IUER, 'GET_SPR_REANAL', 'Error in getting the '// &
     &                   'values dimension "time" in file '// &
     &                    FILIN(1:I_LEN(FILIN))//' error: '// &
     &                    NF_STRERROR(IS) )
           RETURN
         ELSE
           SPR%STS_TIM = RCD__GET
      END IF
!
! --- Learn the ID of the variable surface pressure
!
      IS = NF_INQ_VARID ( NCID, 'pres', ID_VAR_SPR )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 6147, IUER, 'GET_SPR_REANAL', 'Variable "pres" was '// &
     &                   'not found in the input netcdf file '// &
     &                    FILIN(1:I_LEN(FILIN))//' error: '// &
     &                    NF_STRERROR(IS) )
           RETURN
      END IF
!
! --- Get what what we came for: surface pressure
!
      IS = NF_GET_VAR_INT2 ( NCID, ID_VAR_SPR, %VAL(SPR%ADR_SPR) )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 6148, IUER, 'GET_SPR_REANAL', 'Error in getting the '// &
     &                   'values of surface pressure from file '// &
     &                    FILIN(1:I_LEN(FILIN))//' error: '// &
     &                    NF_STRERROR(IS) )
           RETURN
         ELSE
           SPR%STS_SPR = RCD__GET
      END IF
!
      ID_VAR(1) = ID_VAR_LON
      ID_VAR(2) = ID_VAR_LAT
      ID_VAR(3) = ID_VAR_TIM
      ID_VAR(4) = ID_VAR_SPR
!
! --- Get attributes of all exported variables
!
      DO 410 J1=1,4
!
! ------ Learn the number of attributes
!
         IS = NF_INQ_VARNATTS ( NCID, ID_VAR(J1), SPR%NUM_ATT(J1) )
         IF ( IS .NE. 0 ) THEN
              CALL ERR_LOG ( 6149, IUER, 'GET_SPR_REANAL', 'Error in getting '// &
     &            'the number of attributes '//NF_STRERROR(IS) )
              RETURN
         END IF
!
! ------ Cycle over attributes of the J1-th variable
!
         DO 420 J2=1,SPR%NUM_ATT(J1)
!
! --------- Get information about the attribute
!
            IS = NF_INQ_ATTNAME ( NCID, ID_VAR(J1), J2, SPR%NAM_ATT(J2,J1) )
            IF ( IS .NE. 0 ) THEN
                 WRITE ( 6, * ) 'J1=',J1,' J2=',J2
                 CALL ERR_LOG ( 6150, IUER, 'GET_SPR_REANAL', 'Error in getting '// &
     &               'information about the attribute while reading '// &
     &               'the netcdf file'//FILIN(1:I_LEN(FILIN))//' '// &
     &                NF_STRERROR(IS) )
                 RETURN
            END IF
!
! --------- Get information about the attribute
!
            IS = NF_INQ_ATT ( NCID, ID_VAR(J1),   SPR%NAM_ATT(J2,J1), &
     &                        SPR%TYP_ATT(J2,J1), SPR%LEN_ATT(J2,J1)  )
            IF ( IS .NE. 0 ) THEN
                 WRITE ( 6, * ) 'J1=',J1,' J2=',J2
                 CALL ERR_LOG ( 6151, IUER, 'GET_SPR_REANAL', 'Error in getting '// &
     &               'information about the attribute '// &
     &                SPR%NAM_ATT(J2,J1)(1:I_LEN(SPR%NAM_ATT(J2,J1)))// &
     &               ' while reading the netcdf file'// &
     &                FILIN(1:I_LEN(FILIN))//' '//NF_STRERROR(IS) )
                 RETURN
            END IF
!
! --------- Get the value of the attribute
!
            IF ( SPR%TYP_ATT(J2,J1) .EQ. NF_BYTE ) THEN
                 IS = NF_GET_ATT_INT1 ( NCID, ID_VAR(J1), SPR%NAM_ATT(J2,J1), &
     &                                  SPR%VAL_ATT(1,J2,J1) )
               ELSE IF ( SPR%TYP_ATT(J2,J1) .EQ. NF_CHAR   ) THEN
                 IS = NF_GET_ATT_TEXT ( NCID, ID_VAR(J1), SPR%NAM_ATT(J2,J1), &
     &                                  %REF(SPR%VAL_ATT(1,J2,J1)) )
               ELSE IF ( SPR%TYP_ATT(J2,J1) .EQ. NF_SHORT  ) THEN
                 IS = NF_GET_ATT_INT2 ( NCID, ID_VAR(J1), SPR%NAM_ATT(J2,J1), &
     &                                  SPR%VAL_ATT(1,J2,J1) )
               ELSE IF ( SPR%TYP_ATT(J2,J1) .EQ. NF_INT    ) THEN
                 IS = NF_GET_ATT_INT ( NCID, ID_VAR(J1), SPR%NAM_ATT(J2,J1), &
     &                                 SPR%VAL_ATT(1,J2,J1) )
               ELSE IF ( SPR%TYP_ATT(J2,J1) .EQ. NF_FLOAT  ) THEN
                 IS = NF_GET_ATT_REAL ( NCID, ID_VAR(J1), SPR%NAM_ATT(J2,J1), &
     &                                  SPR%VAL_ATT(1,J2,J1) )
               ELSE IF ( SPR%TYP_ATT(J2,J1) .EQ. NF_DOUBLE ) THEN
                 IS = NF_GET_ATT_DOUBLE ( NCID, ID_VAR(J1), SPR%NAM_ATT(J2,J1), &
     &                                    SPR%VAL_ATT(1,J2,J1) )
            END IF
!
            IF ( IS .NE. 0 ) THEN
                 CALL ERR_LOG ( 6152, IUER, 'GET_SPR_REANAL', 'Error in '// &
     &               'getting the value of the atttribute '// &
     &               SPR%NAM_ATT(J2,J1)//' while reading the netcdf file'// &
     &               FILIN(1:I_LEN(FILIN))//' error: '//NF_STRERROR(IS) )
                 RETURN
            END IF
 420     CONTINUE
 410  CONTINUE
!
      IS = NF_CLOSE ( NCID )
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  GET_SPR_REANAL  #!#
