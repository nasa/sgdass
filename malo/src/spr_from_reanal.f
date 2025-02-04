      PROGRAM    SPR_FROM_REANAL_MAIN
! ************************************************************************
! *                                                                      *
! *   Program  SPR_FROM_REANAL  reads yearly files with meteorological   *
! *   information in netCDF format from NCEP Reanalysis 1996 which       *
! *   contains gridded datasets and extrats from there the surface       *
! *   pressure. The data are split into monthly chunks. The output is    *
! *   written in netCDF format.                                          *
! *                                                                      *
! *   Usage:                                                             *
! *                                                                      *
! *      spr_from_reanal_main <in_directory> <year> <output_directory>   *
! *                    [verbosity_level]                                 *
! *                                                                      *
! *   <in_directory>      --  input directory which contains netCDF      *
! *                           files.                                     *
! *   <year>              --  4-digit year.                              *
! *   <output_directory>  --  generic name of the output nc-files.       *
! *   [verbosity_level]   --  0 -- silent mode, 1 -- verbose mode,       *
! *                           2 -- verbose mode with progress counter    *
! *                                                                      *
! * ### 25-JUL-2002  SPR_FROM_REANAL  v2.1 (c) L. Petrov 01-NOV-2012 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'malo.i'
      CHARACTER  FILSPR*128, FILGEN*128, FILTEMP*128, DATE_BEG*23, DATE_END*23, &
     &           STR_DATE_BEG*23, STR_DATE_END*23, STR*80, &
     &           DIR_IN*128, DIR_OUT*128, YEAR_STR*4
      CHARACTER  JD_TO_DATE*23
      REAL*8     JD
      INTEGER*4  MJD_BEG, MJD_END, YEAR_BEG, YEAR_END, YEAR_CUR, NMON, &
     &           MON_BEG, MON_END, MON_CUR, MON_4YR
      INTEGER*4  NDIMS, NVARS, NATTS, NUNL, &
     &           ID_DIM_LON, ID_DIM_LAT, ID_DIM_TIM, &
     &           DIMLEN_LON, DIMLEN_LAT, DIMLEN_TIM, &
     &           ID_VAR_LON, ID_VAR_LAT, ID_VAR_TIM, ID_VAR_SPR, &
     &           ATT_TYPE, ATT_LEN, ID_VAR(4), J1, J2, IER
      REAL*8     HRS_ARR(2)
      INTEGER*4  IS, NCID
      REAL*8     UTC_BEG, UTC_END
      INTEGER*4  LEN_GPH, LEN_SPR, LEN_RHI, IVRB, IUER
      TYPE ( MALO__TYPE ) :: MALO
      INTEGER*4  DAY_TAB(48)
      DATA DAY_TAB / &
     &    31,  29,  31,  30,  31,  30,  31,  31,  30,  31,  30,  31,  & !  1 - 12
     &    31,  28,  31,  30,  31,  30,  31,  31,  30,  31,  30,  31,  & ! 13 - 24
     &    31,  28,  31,  30,  31,  30,  31,  31,  30,  31,  30,  31,  & ! 25 - 36
     &    31,  28,  31,  30,  31,  30,  31,  31,  30,  31,  30,  31   & ! 37 - 48
     &             /
      INTEGER*4,  EXTERNAL :: I_LEN, ILEN
!
      IVRB = 1
!
! --- Get arguments
!
      IF ( IARGC() .LT. 3 ) THEN
           WRITE ( 6, '(A)' ) 'Usage: spr_from_reanal <in_directory> <year> '// &
     &                        '<output_directory> [verbosity_level]'
           CALL EXIT ( 1 )
         ELSE
           CALL GETARG ( 1, DIR_IN   )
           CALL GETARG ( 2, YEAR_STR )
           CALL GETARG ( 3, DIR_OUT  )
           IF ( IARGC() .GE. 4 ) THEN
                CALL GETARG ( 4, STR    )
                CALL CHIN   ( STR, IVRB )
                IF ( IVRB .LT. 1 ) IVRB = 0
                IF ( IVRB .GT. 9 ) IVRB = 9
           END IF
      END IF
!
! --- Build file names
!
      IF ( DIR_IN(I_LEN(DIR_IN):I_LEN(DIR_IN)) .NE. '/' ) THEN
           DIR_IN(I_LEN(DIR_IN)+1:) = '/'
      END IF
      IF ( DIR_OUT(I_LEN(DIR_OUT):I_LEN(DIR_OUT)) .NE. '/' ) THEN
           DIR_OUT(I_LEN(DIR_OUT)+1:) = '/'
      END IF
!
      IF ( ILEN(YEAR_STR) .NE. 4 ) THEN
           IUER = -1
           CALL ERR_LOG ( 611, IUER, 'SPR_FROM_REANAL_MAIN', &
     &         'Wrong year: 4 digits should be supplied' )
           CALL EXIT ( 1 )
      END IF
!
! --- Build specific names of the input files
!
      FILGEN  = DIR_OUT(1:I_LEN(DIR_OUT))//'ncep96_'
      FILSPR  = DIR_IN(1:I_LEN(DIR_IN))//'pres.sfc.'//YEAR_STR//'.nc'
!
! --- Initialization
!
      IUER = -1
      CALL MALO_INIT ( MALO, IUER )
      IF ( IUER .NE. 0 ) CALL EXIT ( 1 )
!
! --- Extract surface pressure
!
      IUER = -1
      CALL SPR_FROM_REANAL ( MALO, FILSPR, IUER )
!
      IF ( ILEN(YEAR_STR) .NE. 4 ) THEN
           IUER = -1
           CALL ERR_LOG ( 612, IUER, 'SPR_FROM_REANAL_MAIN', &
     &         'Wrong year: 4 digits should be supplied' )
           CALL EXIT ( 1 )
      END IF
!
! --- Compute the dates for starting and ending dates
!
      JD = 2400000.5D0 + MALO%MJD_BEG + MALO%UTC_BEG/86400.0
      STR_DATE_BEG = JD_TO_DATE ( JD, -3 )
      JD = 2400000.5D0 + MALO%MJD_END + MALO%UTC_END/86400.0
      STR_DATE_END = JD_TO_DATE ( JD, -3 )
!
      IF ( IVRB .GT. 0 ) THEN
           WRITE ( 6, * ) ' SPR data starts at '//STR_DATE_BEG(1:16)
           WRITE ( 6, * ) ' SPR data ends   at '//STR_DATE_END(1:16)
      END IF
!
      CALL CHIN ( STR_DATE_BEG(1:4), YEAR_BEG )
      CALL CHIN ( STR_DATE_BEG(6:7), MON_BEG  )
      CALL CHIN ( STR_DATE_END(1:4), YEAR_END )
      CALL CHIN ( STR_DATE_END(6:7), MON_END  )
!
! --- Learn the number of months
!
      NMON = (YEAR_END - YEAR_BEG)*12 + (MON_END-MON_BEG) + 1
!
      DO 410 J1=1,NMON  ! cycle over months
         YEAR_CUR = YEAR_BEG + (J1-1)/12
         MON_CUR = 1 + MOD((J1-1),12)
         MON_4YR = MON_CUR + MOD(YEAR_CUR,4)*12
         IF ( J1 .EQ. 1 ) THEN
!
! ----------- It is the first output file. Learn beginning date
!
              MJD_BEG = MALO%MJD_BEG
              UTC_BEG = MALO%UTC_BEG
              DATE_BEG = JD_TO_DATE ( 2400000.5D0 + MJD_BEG + &
     &                                UTC_BEG/86400.0D0, -3 )
            ELSE
!
! ----------- It is not the first month.
!
              WRITE ( UNIT=DATE_BEG, FMT='(I4,".",I2,".",I2,":00_00_00.0")' ) &
     &                YEAR_CUR, MON_CUR, 1
              CALL BLANK_TO_ZERO ( DATE_BEG )
              CALL DATE_TO_TIME  ( DATE_BEG, MJD_BEG, UTC_BEG, -3 )
         END IF
!
         IF ( J1 .EQ. NMON  ) THEN
!
! ----------- It is the last month. Learn the end date
!
              MJD_END = MALO%MJD_END
              UTC_END = MALO%UTC_END
              DATE_END = JD_TO_DATE ( 2400000.5D0 + MJD_END + &
     &                                UTC_END/86400.0D0, -3 )
            ELSE
              WRITE ( UNIT=DATE_END, FMT='(I4,".",I2,".",I2,":18_00_00.0")' ) &
     &                YEAR_CUR, MON_CUR, DAY_TAB(MON_4YR)
              CALL BLANK_TO_ZERO ( DATE_END )
              CALL DATE_TO_TIME  ( DATE_END, MJD_END, UTC_END, -3 )
         END IF
!
         IF ( IVRB .GE. 1 ) THEN
              WRITE ( 6, '(A$)' ) '  Writing file for '//DATE_BEG(1:16)// &
     &                            ' -- '//DATE_END(1:16)//' '//CHAR(13)
              CALL FLUSH ( 6 )
         END IF
!
! ------ Correct dates if neccessary
!
         IF ( UTC_BEG .GT. 86400.001D0 ) THEN
              MJD_BEG = MJD_BEG + 1
              UTC_BEG = UTC_BEG - 86400.0D0
         END IF
         IF ( UTC_END .GT. 86400.001D0 ) THEN
              MJD_END = MJD_END + 1
              UTC_END = UTC_END - 86400.0D0
         END IF
!
         IF ( UTC_BEG .LT. -0.001D0 ) THEN
              MJD_BEG = MJD_BEG - 1
              UTC_BEG = UTC_BEG + 86400.0D0
         END IF
         IF ( UTC_END .LT. -0.001D0 ) THEN
              MJD_END = MJD_END - 1
              UTC_END = UTC_END + 86400.0D0
         END IF
!
! ------ Write output nc-file
!
         IUER = -1
         CALL SPR_WRITE_NC ( MALO, FILGEN, MJD_BEG, UTC_BEG, &
     &                       MJD_END, UTC_END, IVRB, IUER )
         IF ( IUER .NE. 0 ) THEN
              CALL ERR_LOG ( 6091, IUER, 'SPR_FROM_REANAL_MAIN', 'Error in '// &
     &            'attempt to write output nc-file' )
              CALL EXIT ( 1 )
         END IF
 410  CONTINUE
    WRITE ( 6, * ) ' '
!
    END  PROGRAM   SPR_FROM_REANAL_MAIN !#!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE SPR_FROM_REANAL ( MALO, FILSPR, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine SPR_FROM_REANAL 
! *                                                                      *
! * ### 15-OCT-2012  SPR_FROM_REANAL v1.0 (c) L. Petrov  15-OCT-2012 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'malo.i'
      INCLUDE   'netcdf.inc'
      TYPE     ( MALO__TYPE ) :: MALO
      CHARACTER  FILSPR*(*)
      INTEGER*4  IUER
!
      CHARACTER  DATE_BEG*23, DATE_END*23, STR*128
      INTEGER*4  MJD_BEG, MJD_END, NDIMS, NVARS, NATTS, NUNL, &
     &           ID_DIM_LON, ID_DIM_LAT, ID_DIM_TIM, &
     &           DIMLEN_LON, DIMLEN_LAT, DIMLEN_TIM, &
     &           ID_VAR_LON, ID_VAR_LAT, ID_VAR_TIM, ID_VAR_SPR, &
     &           ATT_TYPE, ATT_LEN, ID_VAR(4), IS, NCID, IVRB, &
     &           J1, J2, J3, J4, J5, J6, J7, J8, IER
      REAL*4,    ALLOCATABLE :: LAT_R4(:)
      REAL*8,    ALLOCATABLE :: DAT_R8(:)
      INTEGER*2, ALLOCATABLE :: SPR_I2(:,:,:)
      INTEGER*2  SPR_MISSING_I2
      REAL*8     HRS_ARR(2), UTC, UTC_BEG, UTC_END
!
      CHARACTER, EXTERNAL  :: JD_TO_DATE*23
      INTEGER*4, EXTERNAL  :: ILEN, I_LEN, LINDEX
      REAL*4,    EXTERNAL  :: SPR_I2_TO_SI
!
      ALLOCATE ( MALO%MJD_ARR(MALO%NTIM), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( 4*MALO%NLAT, STR )
           CALL ERR_LOG ( 6141, IUER, 'SPR_FROM_REANAL', 'Error in an attempt to '// &
     &                   'allocate '//STR(1:I_LEN(STR))//'bytes of dynamic memory '// &
     &                   'for array MJD_ARR' )
           RETURN
      END IF 
!
! --- Open the NETcdf database
!
      IS = NF_OPEN ( FILSPR, NF_NOWRITE, NCID )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 6142, IUER, 'SPR_FROM_REANAL', 'Error in NF_OPEN: '// &
     &                    NF_STRERROR(IS) )
           RETURN
      END IF
!
! --- Learn the number of dimensions, variables and attributes
!
      IS = NF_INQ ( NCID, NDIMS, NVARS, NATTS, NUNL )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 6143, IUER, 'SPR_FROM_REANAL', 'Error in an attempt '// &
     &         'to open input file '//FILSPR(1:I_LEN(FILSPR))//' NF_INQ: '// &
     &          NF_STRERROR(IS) )
           RETURN
      END IF
!
! --- Learn the ID of the variable time
!
      IS = NF_INQ_VARID ( NCID, 'time', ID_VAR_TIM )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 6144, IUER, 'SPR_FROM_REANAL', 'Variable "time" was '// &
     &                   'not found in the input netcdf file '// &
     &                    FILSPR(1:I_LEN(FILSPR))//' error: '// &
     &                    NF_STRERROR(IS) )
           RETURN
      END IF
!
! --- Get the ID of the attribute actual time range
!
      IS = NF_INQ_ATT ( NCID, ID_VAR_TIM, 'actual_range', ATT_TYPE, ATT_LEN )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 6145, IUER, 'SPR_FROM_REANAL', 'Error in an '// &
     &         'attempt to get the attribute "atctual_range" of the '// &
     &         'variable "time" in file '//FILSPR(1:I_LEN(FILSPR))// &
     &         ' error: '//NF_STRERROR(IS) )
           RETURN
      END IF
!
      IF ( ATT_TYPE .NE. NF_DOUBLE .AND. ATT_LEN .NE. 2 ) THEN
!
! -------- Hmm. Attribute has different type and length than it was expected
!
           WRITE ( 6, * ) 'ATT_TYPE=',ATT_TYPE, ' ATT_LEN = ',ATT_LEN
           CALL ERR_LOG ( 6146, IUER, 'SPR_FROM_REANAL', 'Attribute '// &
     &         '"atctual_range" of the variable "time" in file '// &
     &          FILSPR(1:I_LEN(FILSPR))//' has unexpected type and length' )
           RETURN
      END IF
!
! --- Get the values of the attribute actual time range
!
      IS = NF_GET_ATT_DOUBLE ( NCID, ID_VAR_TIM, 'actual_range', HRS_ARR )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 6147, IUER, 'SPR_FROM_REANAL', 'Error in an '// &
     &         'attempt to get the value of the attribute "atctual_range" '// &
     &         'of the variable "time" in file '//FILSPR(1:I_LEN(FILSPR))// &
     &         ' error: '//NF_STRERROR(IS) )
           RETURN
      END IF
!
! --- Convert dates from mythological to astronomical format
!
      CALL HRS_TO_MJD ( HRS_ARR(1), MALO%MJD_BEG, MALO%UTC_BEG )
      CALL HRS_TO_MJD ( HRS_ARR(2), MALO%MJD_END, MALO%UTC_END )
!
! --- Learn the ID of the variable time
!
      IS = NF_INQ_DIMID ( NCID, 'time', ID_DIM_TIM )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 6148, IUER, 'SPR_FROM_REANAL', 'Dimention "time" was '// &
     &                   'not found in the input netcdf file '// &
     &                    FILSPR(1:I_LEN(FILSPR))//' error: '// &
     &                    NF_STRERROR(IS) )
           RETURN
      END IF
!
! --- Get the lenght of the dimension TIME
!
      IS = NF_INQ_DIMLEN ( NCID, ID_DIM_TIM, DIMLEN_TIM  )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 6149, IUER, 'SPR_FROM_REANAL', 'Error in getting the '// &
     &                   'length of the dimension "time" in file '// &
     &                    FILSPR(1:I_LEN(FILSPR))//' error: '// &
     &                    NF_STRERROR(IS) )
           RETURN
      END IF
!
! --- Learn the ID of the variable lon
!
      IS = NF_INQ_DIMID ( NCID, 'lon', ID_DIM_LON )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 6150, IUER, 'SPR_FROM_REANAL', 'Dimention "lon" was '// &
     &                   'not found in the input netcdf file '// &
     &                    FILSPR(1:I_LEN(FILSPR))//' error: '// &
     &                    NF_STRERROR(IS) )
           RETURN
      END IF
!
! --- Get the length of the dimension longitude
!
      IS = NF_INQ_DIMLEN ( NCID, ID_DIM_LON, DIMLEN_LON  )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 6151, IUER, 'SPR_FROM_REANAL', 'Error in getting the '// &
     &                   'length of the dimension "lon" in file '// &
     &                    FILSPR(1:I_LEN(FILSPR))//' error: '// &
     &                    NF_STRERROR(IS) )
           RETURN
      END IF
!
! --- Learn the ID of the variable lat
!
      IS = NF_INQ_DIMID ( NCID, 'lat', ID_DIM_LAT )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 6152, IUER, 'SPR_FROM_REANAL', 'Dimention "lat" was '// &
     &                   'not found in the input netcdf file '// &
     &                    FILSPR(1:I_LEN(FILSPR))//' error: '// &
     &                    NF_STRERROR(IS) )
           RETURN
      END IF
!
! --- Get the length of the dimenstion lattitude
!
      IS = NF_INQ_DIMLEN ( NCID, ID_DIM_LAT, DIMLEN_LAT  )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 6153, IUER, 'SPR_FROM_REANAL', 'Error in getting the '// &
     &                   'length of the dimension "lat" in file '// &
     &                    FILSPR(1:I_LEN(FILSPR))//' error: '// &
     &                    NF_STRERROR(IS) )
           RETURN
      END IF
!
! --- Store dimenstions
!
      MALO%NLON = DIMLEN_LON
      MALO%NLAT = DIMLEN_LAT-1
      MALO%NTIM = DIMLEN_TIM
!
! --- Grab dynamic memory for the varaiables which will be read from the file
!
      ALLOCATE ( MALO%LON(MALO%NLON), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( 4*MALO%NLON, STR )
           CALL ERR_LOG ( 6154, IUER, 'SPR_FROM_REANAL', 'Error in an attempt to '// &
     &                   'allocate '//STR(1:I_LEN(STR))//'bytes of dynamic memory '// &
     &                   'for array LON' )
           RETURN
      END IF 
!
      ALLOCATE ( MALO%LAT(MALO%NLAT), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( 4*MALO%NLAT, STR )
           CALL ERR_LOG ( 6155, IUER, 'SPR_FROM_REANAL', 'Error in an attempt to '// &
     &                   'allocate '//STR(1:I_LEN(STR))//'bytes of dynamic memory '// &
     &                   'for array LAT' )
           RETURN
      END IF 
!
      ALLOCATE ( MALO%MJD_ARR(MALO%NTIM), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( 4*MALO%NLAT, STR )
           CALL ERR_LOG ( 6156, IUER, 'SPR_FROM_REANAL', 'Error in an attempt to '// &
     &                   'allocate '//STR(1:I_LEN(STR))//'bytes of dynamic memory '// &
     &                   'for array MJD_ARR' )
           RETURN
      END IF 
!
      ALLOCATE ( MALO%TAI_ARR(MALO%NTIM), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( 4*MALO%NLAT, STR )
           CALL ERR_LOG ( 6157, IUER, 'SPR_FROM_REANAL', 'Error in an attempt to '// &
     &                   'allocate '//STR(1:I_LEN(STR))//'bytes of dynamic memory '// &
     &                   'for array MJD_ARR' )
           RETURN
      END IF 
!
      ALLOCATE ( MALO%SPR(MALO%NLON,MALO%NLAT,MALO%NTIM), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( 4*MALO%NLON*MALO%NLAT*MALO%NTIM, STR )
           CALL ERR_LOG ( 6158, IUER, 'SPR_FROM_REANAL', 'Error in an attempt to '// &
     &                   'allocate '//STR(1:I_LEN(STR))//'bytes of dynamic memory '// &
     &                   'for array SPR' )
           RETURN
      END IF 
!
      ALLOCATE ( SPR_I2(MALO%NLON,MALO%NLAT+1,MALO%NTIM), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( 2*MALO%NLON*MALO%NLAT*MALO%NTIM, STR )
           CALL ERR_LOG ( 6159, IUER, 'SPR_FROM_REANAL', 'Error in an attempt to '// &
     &                   'allocate '//STR(1:I_LEN(STR))//'bytes of dynamic memory '// &
     &                   'for array SPR_I2' )
           RETURN
      END IF 
!
      ALLOCATE ( DAT_R8(MALO%NTIM), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( 2*MALO%NLON*MALO%NLAT*MALO%NTIM, STR )
           CALL ERR_LOG ( 6160, IUER, 'SPR_FROM_REANAL', 'Error in an attempt to '// &
     &                   'allocate '//STR(1:I_LEN(STR))//'bytes of dynamic memory '// &
     &                   'for array SPR_I2' )
           RETURN
      END IF 
!
      ALLOCATE ( LAT_R4(MALO%NLAT+1), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( 4*(MALO%NLAT+1), STR )
           CALL ERR_LOG ( 6155, IUER, 'SPR_FROM_REANAL', 'Error in an attempt to '// &
     &                   'allocate '//STR(1:I_LEN(STR))//'bytes of dynamic memory '// &
     &                   'for array LAT_R4' )
           RETURN
      END IF 
!
! --- Learn the ID of the variable longitude
!
      IS = NF_INQ_VARID ( NCID, 'lon', ID_VAR_LON )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 6161, IUER, 'SPR_FROM_REANAL', 'Variable "lon" was '// &
     &                   'not found in the input netcdf file '// &
     &                    FILSPR(1:I_LEN(FILSPR))//' error: '// &
     &                    NF_STRERROR(IS) )
           RETURN
      END IF
!
! --- Get the varaiable: longitude.
!
      IS = NF_GET_VAR_REAL ( NCID, ID_VAR_LON, MALO%LON )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 6162, IUER, 'SPR_FROM_REANAL', 'Error in getting the '// &
     &                   'values of the variable "lon" in file '// &
     &                    FILSPR(1:I_LEN(FILSPR))//' error: '// &
     &                    NF_STRERROR(IS) )
           RETURN
      END IF
!
! --- Learn the ID of the variable latitude
!
      IS = NF_INQ_VARID ( NCID, 'lat', ID_VAR_LAT )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 6163, IUER, 'SPR_FROM_REANAL', 'Variable "lat" was '// &
     &                   'not found in the input netcdf file '// &
     &                    FILSPR(1:I_LEN(FILSPR))//' error: '// &
     &                    NF_STRERROR(IS) )
           RETURN
      END IF
!
! --- Get the variable: latitude.
!
      IS = NF_GET_VAR_REAL ( NCID, ID_VAR_LAT, LAT_R4 )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 6164, IUER, 'SPR_FROM_REANAL', 'Error in getting the '// &
     &                   'values of the variable "lat" in file '// &
     &                    FILSPR(1:I_LEN(FILSPR))//' error: '// &
     &                    NF_STRERROR(IS) )
           RETURN
      END IF
!
! --- Get the varaiable: time tag. We store it in mythological date format
!
      IS = NF_GET_VAR_DOUBLE ( NCID, ID_VAR_TIM, DAT_R8 )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 6165, IUER, 'SPR_FROM_REANAL', 'Error in getting the '// &
     &                   'values dimension "time" in file '// &
     &                    FILSPR(1:I_LEN(FILSPR))//' error: '// &
     &                    NF_STRERROR(IS) )
           RETURN
      END IF
!
! --- Learn the ID of the variable surface pressure
!
      IS = NF_INQ_VARID ( NCID, 'pres', ID_VAR_SPR )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 6166, IUER, 'SPR_FROM_REANAL', 'Variable "pres" was '// &
     &                   'not found in the input netcdf file '// &
     &                    FILSPR(1:I_LEN(FILSPR))//' error: '// &
     &                    NF_STRERROR(IS) )
           RETURN
      END IF
!
! --- Get what what we came for: surface pressure
!
      IS = NF_GET_VAR_INT2 ( NCID, ID_VAR_SPR, SPR_I2 )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 6167, IUER, 'SPR_FROM_REANAL', 'Error in getting the '// &
     &                   'values of surface pressure from file '// &
     &                    FILSPR(1:I_LEN(FILSPR))//' error: '// &
     &                    NF_STRERROR(IS) )
           RETURN
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
         IS = NF_INQ_VARNATTS ( NCID, ID_VAR(J1), MALO%NUM_ATT(J1) )
         IF ( IS .NE. 0 ) THEN
              CALL ERR_LOG ( 6168, IUER, 'SPR_FROM_REANAL', 'Error in getting '// &
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
                 CALL ERR_LOG ( 6169, IUER, 'SPR_FROM_REANAL', 'Error in getting '// &
     &               'information about the attribute while reading '// &
     &               'the netcdf file'//FILSPR(1:I_LEN(FILSPR))//' '// &
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
                 CALL ERR_LOG ( 6170, IUER, 'SPR_FROM_REANAL', 'Error in getting '// &
     &               'information about the attribute '// &
     &                MALO%NAM_ATT(J2,J1)(1:I_LEN(MALO%NAM_ATT(J2,J1)))// &
     &               ' while reading the netcdf file'// &
     &                FILSPR(1:I_LEN(FILSPR))//' '//NF_STRERROR(IS) )
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
                 IF ( MALO%NAM_ATT(J2,J1) == 'missing_value' ) THEN
                     IS = NF_GET_ATT_INT ( NCID, ID_VAR(J1), MALO%NAM_ATT(J2,J1), &
     &                                     SPR_MISSING_I2 )
                 END IF
               ELSE IF ( MALO%TYP_ATT(J2,J1) .EQ. NF_FLOAT  ) THEN
                 IS = NF_GET_ATT_REAL ( NCID, ID_VAR(J1), MALO%NAM_ATT(J2,J1), &
     &                                  MALO%VAL_ATT(1,J2,J1) )
               ELSE IF ( MALO%TYP_ATT(J2,J1) .EQ. NF_DOUBLE ) THEN
                 IS = NF_GET_ATT_DOUBLE ( NCID, ID_VAR(J1), MALO%NAM_ATT(J2,J1), &
     &                                    MALO%VAL_ATT(1,J2,J1) )
            END IF
!
            IF ( IS .NE. 0 ) THEN
                 CALL ERR_LOG ( 6171, IUER, 'SPR_FROM_REANAL', 'Error in '// &
     &               'getting the value of the atttribute '// &
     &               MALO%NAM_ATT(J2,J1)//' while reading the netcdf file'// &
     &               FILSPR(1:I_LEN(FILSPR))//' error: '//NF_STRERROR(IS) )
                 RETURN
            END IF
 420     CONTINUE
 410  CONTINUE
!
      DO 430 J3=1,MALO%NLON
         MALO%LON(J3) = MALO%LON(J3)*DEG__TO__RAD
 430  CONTINUE 
!
! --- Invert the order. New order: from south pole to the northen pole
!
      DO 440 J4=1,MALO%NLAT
         MALO%LAT(J4) = LAT_R4(MALO%NLAT+2-J4)*DEG__TO__RAD
 440  CONTINUE 
      DEALLOCATE ( LAT_R4 )
!
! --- Transform date from a mythological format to MJD/TAI
!
      DO 450 J5=1,MALO%NTIM
         CALL HRS_TO_MJD ( DAT_R8(J5), MALO%MJD_ARR(J5), UTC )
         CALL ERR_PASS ( IUER, IER )
         CALL MALO_UTC_TO_TAI ( MALO, MALO%MJD_ARR(J5), UTC, MALO%TAI_ARR(J5), IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 6172, IUER, 'SPR_FROM_REANAL', 'Error in computing '// &
     &            'TAI for the specified UTC time time tag' )
              RETURN
         END IF
         IF ( MALO%TAI_ARR(J5) > 86400.0D0 ) THEN
              MALO%TAI_ARR(J5) = MALO%TAI_ARR(J5) - 86400.0D0
              MALO%MJD_ARR(J5) = MALO%MJD_ARR(J5) + 1
         END IF
 450  CONTINUE 
!
! --- Convert units to Pascal
!
      MALO%SPR_MISSING = -9999.0
      DO 460 J6=1,MALO%NTIM
         DO 470 J7=1,MALO%NLAT
            DO 480 J8=1,MALO%NLON
               IF ( SPR_I2(J8,J7,J6) == SPR_MISSING_I2 ) THEN
                    MALO%SPR(J8,J7,J6) = MALO%SPR_MISSING 
                  ELSE 
                    MALO%SPR(J8,J7,J6) = SPR_I2_TO_SI( SPR_I2(J8,MALO%NLAT+2-J7,J6) ) 
               END IF
               IF ( MALO%SPR(J8,J7,J6) < 40000.0 .OR. MALO%SPR(J8,J7,J6) > 120000.0 ) THEN
                    MALO%SPR(J8,J7,J6) = MALO%SPR_MISSING 
               END IF 
 480        CONTINUE 
 470     CONTINUE 
 460  CONTINUE 
!
      DEALLOCATE ( DAT_R8 ) 
      DEALLOCATE ( SPR_I2 ) 
      MALO%DATA_TYPE = 'Time_series_NCEP_Reanalysis_96  '
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  SPR_FROM_REANAL  !#!#
