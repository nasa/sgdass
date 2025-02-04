      PROGRAM    GEOS_NC4_TO_HEB
! ************************************************************************
! *                                                                      *
! *   Progam GEOS_NC4_TO_HEB reads input file with the output of         *
! *   numerical weather model in NETCDF-4 format, extractgs one of the   *
! *   variables (DELP,T,QV,U,V,W) and writes them in a gile in heb       *
! *   format and then compresses it.                                     *
! *                                                                      *
! * ### 07-APR-2018  GEOS_NC4_TO_HEB  v1.0 (c) L. Petrov 08-APR-2018 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'netcdf.inc'
      INCLUDE   'malo.i'
      INCLUDE   'malo_local.i'
      INCLUDE   'heb.i'
      TYPE     ( HEB__TYPE ) :: HEB, GEOID_BSPL_HEB
      INTEGER*4  MV, N1
      PARAMETER  ( MV = 7 )
      CHARACTER  VAR_TAB(MV,2)*16
      DATA       ( VAR_TAB(N1,1), VAR_TAB(N1,2), N1=1,MV) &
     &           / &
     &             'T',    't', &
     &             'U',    'u', &
     &             'V',    'v', &
     &             'W',    'w', &
     &             'QV',   'q', &
     &             'DELP', 'd', &
     &             'PHIS', 'g'  &
     &           / 
      INTEGER*8  DIR_DESC, IP, IS8
      CHARACTER  FILIN*128, FILOUT*128, DIROUT*128, DIR*128, SUBDIR*128, &
     &           COMPR*128, COMPR_COM*128, COM_STR*128, STR*128, VAR_NAM*16, &
     &           VAR_OUT_NAM*16, FIL_BSPL_GEOID_HEB*128
      CHARACTER  STR_DAT*16, STR_TIM*16, DATE_STR*32
      INTEGER*4  ID_VAR_LAT, ID_VAR_LON, ID_VAR_LEV, ID_VAR_TIM, ID_VAR_DAT
      INTEGER*4  ID_DIM_LAT, ID_DIM_LON, ID_DIM_LEV, ID_DIM_TIM
      INTEGER*4  DIMLEN_LAT, DIMLEN_LON, DIMLEN_LEV, DIMLEN_TIM
      INTEGER*4  J1, J2, J3, J4, J5, IS, NDIMS, NVARS, NATTS, NUNL, ID_VAR(4), &
     &           IDAT(2), NCID, IL, IUER
      INTEGER*2  MODE_I2
      INTEGER*2  MASK_I2 
      DATA       MASK_I2  / O'0777' /
      REAL*8     LON, LAT
      REAL*8,    EXTERNAL :: GET_GEOID
      INTEGER*8, EXTERNAL :: OPENDIR, CLOSEDIR 
      INTEGER*4, EXTERNAL :: MKDIR, I_LEN, ILEN
      CHARACTER, EXTERNAL :: MJDSEC_TO_DATE*30
!
      COMPR = 'none'
      IF ( IARGC() < 2 ) THEN
           WRITE ( 6, '(A)' ) 'Usage geos_nc4_to_heb filin output_directory [compr]'
           CALL EXIT ( 1 )
         ELSE 
           CALL GETARG ( 1, FILIN  )
           CALL GETARG ( 2, DIROUT )
           IF ( IARGC() .GE. 3 ) THEN
                CALL GETARG ( 3, COMPR ) 
           END IF
      END IF
      IF ( COMPR == 'none' ) THEN
           CONTINUE 
           CALL CLRCH ( COMPR_COM )
         ELSE IF ( COMPR == 'gzip' ) THEN
           COMPR_COM = 'gzip -1 -f '
         ELSE IF ( COMPR == 'bzip2' ) THEN
           COMPR_COM = 'bzip2 -9 -f '
         ELSE IF ( COMPR == 'pbzip2' ) THEN
           COMPR_COM = 'pbzip2 -r -m1024 -S4096 -9 -f '
         ELSE IF ( COMPR == 'pbzip2_p1' ) THEN
           COMPR_COM = 'pbzip2 -r -m1024 -S4096 -9 -p1 -f '
         ELSE IF ( COMPR == 'lbzip2' ) THEN
           COMPR_COM= 'lbzip2 -9 -f '
         ELSE IF ( COMPR == 'lbzip2_p1' ) THEN
           COMPR_COM= 'lbzip2 -9 -n1 -f '
         ELSE IF ( COMPR == 'lbzip2_1' ) THEN
           COMPR_COM= 'lbzip2 -1 -f '
         ELSE IF ( COMPR == 'lbzip2_1p1' ) THEN
           COMPR_COM= 'lbzip2 -1 -n1 -f '
         ELSE IF ( COMPR == 'lbzip2_2p1' ) THEN
           COMPR_COM = 'lbzip2 -2 -n1 -f '
         ELSE IF ( INDEX ( COMPR, 'geoid_height_bspl' ) > 0 ) THEN
           FIL_BSPL_GEOID_HEB = COMPR
           IF ( INDEX ( FIL_BSPL_GEOID_HEB, '/' ) < 1 ) THEN
                FIL_BSPL_GEOID_HEB = MALO_SHARE//'/'//FIL_BSPL_GEOID_HEB
           END IF
           CALL CLRCH ( COMPR_COM )
         ELSE 
           CALL ERR_LOG ( 6301, -2, 'GEOS_NC4_TO_HEB', 'Unsupported '// &
     &         'compression method: '//COMPR//' . Supported methods: '// &
     &         ' none gzip bzip2 pbzip2 pbzip2_p1 lbzip lbzip2_p1 '// &
     &         'lbzip2 lbzip2_1 lbzip2_1p1 lbzip2_2p1' ) 
           CALL EXIT ( 1 )
      END IF
!
! --- Check whether the output directory exists
!
      DIR_DESC = OPENDIR ( DIROUT(1:I_LEN(DIROUT))//CHAR(0) ) 
      IF ( DIR_DESC .LE. 0 ) THEN
           CALL GERROR ( STR )
           CALL ERR_LOG ( 6302, -2, 'GEOS_NC4_TO_HEB', 'Output '// &
     &         'directory '//DIROUT(1:I_LEN(DIROUT))//' does not exist' )
           CALL EXIT ( 1 )
         ELSE 
           IP = CLOSEDIR ( %VAL(DIR_DESC) )
      END IF
!
!
! --- Open the NETcdf database file
!
      IS = NF_OPEN ( FILIN, NF_NOWRITE, NCID )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 6303, IUER, 'GEOS_NC4_TO_HEB', &
     &                   'Error in NF_OPEN: '//TRIM(NF_STRERROR(IS))//' -- '//FILIN )
           RETURN
      END IF
!
! --- Learn the number of dimension, variables and attributes
!
      IS = NF_INQ ( NCID, NDIMS, NVARS, NATTS, NUNL )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 6304, IUER, 'GEOS_NC4_TO_HEB', 'Error in '// &
     &         'an attempt to open input file '// &
     &          FILIN(1:I_LEN(FILIN))//' NF_INQ: '//NF_STRERROR(IS) )
           RETURN
      END IF
!
! --- Learn the ID of the dimension lon
!
      IS = NF_INQ_DIMID ( NCID, 'lon', ID_DIM_LON )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 6305, IUER, 'GEOS_NC4_TO_HEB', 'Dimension '// &
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
           CALL ERR_LOG ( 6306, IUER, 'GEOS_NC4_TO_HEB', 'Error in '// &
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
           CALL ERR_LOG ( 6307, IUER, 'GEOS_NC4_TO_HEB', 'Dimension '// &
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
           CALL ERR_LOG ( 6308, IUER, 'GEOS_NC4_TO_HEB', 'Error in '// &
     &                   'getting the length of the dimension "lat" in file '// &
     &                    FILIN(1:I_LEN(FILIN))//' error: '// &
     &                    NF_STRERROR(IS) )
           RETURN
      END IF
!
! --- Learn the ID of the dimension lev
!
      IF ( INDEX ( FILIN, 'const' ) .LE. 0 ) THEN
           IS = NF_INQ_DIMID ( NCID, 'lev', ID_DIM_LEV )
           IF ( IS .NE. 0 ) THEN
                CALL ERR_LOG ( 6309, IUER, 'GEOS_NC4_TO_HEB', 'Dimension '// &
     &                        '"lev" was not found in the input netcdf file '// &
     &                         FILIN(1:I_LEN(FILIN))//' error: '// &
     &                         NF_STRERROR(IS) )
                RETURN
           END IF
!
! -------- Get the length of the dimension lev
!
           IS = NF_INQ_DIMLEN ( NCID, ID_DIM_LEV, DIMLEN_LEV  )
           IF ( IS .NE. 0 ) THEN
                CALL ERR_LOG ( 6310, IUER, 'GEOS_NC4_TO_HEB', 'Error in '// &
     &                        'getting the length of the dimension "lev" in file '// &
     &                         FILIN(1:I_LEN(FILIN))//' error: '// &
     &                         NF_STRERROR(IS) )
                RETURN
           END IF
         ELSE
           DIMLEN_LEV = 1
      END IF
!
! --- Learn the ID of the dimension time
!
      IS = NF_INQ_DIMID ( NCID, 'time', ID_DIM_TIM )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 6311, IUER, 'GEOS_NC4_TO_HEB', 'Dimension '// &
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
           CALL ERR_LOG ( 6312, IUER, 'GEOS_NC4_TO_HEB', 'Error in '// &
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
           CALL ERR_LOG ( 6313, IUER, 'GEOS_NC4_TO_HEB', 'Variable '// &
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
           CALL ERR_LOG ( 6314, IUER, 'GEOS_NC4_TO_HEB', 'Variable '// &
     &                   '"lat" was not found in the input netcdf file '// &
     &                    FILIN(1:I_LEN(FILIN))//' error: '// &
     &                    NF_STRERROR(IS) )
           RETURN
      END IF
!
! --- Learn the ID of the variable lev
!
      IF ( INDEX ( FILIN, 'const' ) .LE. 0 ) THEN
           IS = NF_INQ_VARID ( NCID, 'lev', ID_VAR_LEV )
           IF ( IS .NE. 0 ) THEN
                CALL ERR_LOG ( 6315, IUER, 'GEOS_NC4_TO_HEB', 'Variable '// &
     &                        '"lev" was not found in the input netcdf file '// &
     &                         FILIN(1:I_LEN(FILIN))//' error: '// &
     &                         NF_STRERROR(IS) )
                RETURN
           END IF
      END IF
!
! --- Learn the ID of the variable time
!
      IS = NF_INQ_VARID ( NCID, 'time', ID_VAR_TIM )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 6316, IUER, 'GEOS_NC4_TO_HEB', 'Variable '// &
     &                   '"time" was not found in the input netcdf file '// &
     &                    FILIN(1:I_LEN(FILIN))//' error: '// &
     &                    NF_STRERROR(IS) )
           RETURN
      END IF
!
      DO 410 J1=1,MV
!
! ------ Learn the ID of the requested variable DELP
!
         IS = NF_INQ_VARID ( NCID, TRIM(VAR_TAB(J1,1)), ID_VAR_DAT )
         IF ( IS == 0 ) THEN
              VAR_NAM = TRIM(VAR_TAB(J1,1))
              VAR_OUT_NAM = TRIM(VAR_TAB(J1,2))
              GOTO 810
         END IF
 410  CONTINUE 
      IUER = -1
      CALL ERR_LOG ( 6317, IUER, 'GEOS_NC4_TO_HEB', 'No supported variable '// &
     &    'was foind in the input file '//FILIN )
      CALL EXIT ( 1 )
 810  CONTINUE 
!
      CALL CLRCH ( HEB%UNITS )
      IS = NF_GET_ATT_TEXT ( NCID, ID_VAR_DAT, 'units', HEB%UNITS )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 6318, IUER, 'GEOS_NC4_TO_HEB', 'Error in '// &
     &         ' an attempt to read attribute unit: '// &
     &          NF_STRERROR(IS) )
           RETURN
      END IF
!
      CALL CLRCH ( HEB%SDS_NAME )
      IS = NF_GET_ATT_TEXT ( NCID, ID_VAR_DAT, 'standard_name', HEB%SDS_NAME )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 6318, IUER, 'GEOS_NC4_TO_HEB', 'Error in '// &
     &         ' an attempt to read attribute unit: '// &
     &          NF_STRERROR(IS) )
           RETURN
      END IF
      HEB%TITLE = HEB%SDS_NAME
!
      IS = NF_GET_ATT_REAL ( NCID, ID_VAR_DAT, '_FillValue', HEB%FILL_VALUE )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 6319, IUER, 'GEOS_NC4_TO_HEB', 'Error in '// &
     &         ' an attempt to read attribute _FillValue: '// &
     &          NF_STRERROR(IS) )
           RETURN
      END IF
!
      IS = NF_GET_ATT_REAL ( NCID, ID_VAR_DAT, 'vmin', HEB%VALID_RANGE(1) )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 6320, IUER, 'GEOS_NC4_TO_HEB', 'Error in '// &
     &         ' an attempt to read attribute vmin: '// &
     &          NF_STRERROR(IS) )
           RETURN
      END IF
!
      IS = NF_GET_ATT_REAL ( NCID, ID_VAR_DAT, 'vmin', HEB%VALID_RANGE(2) )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 6321, IUER, 'GEOS_NC4_TO_HEB', 'Error in '// &
     &         ' an attempt to read attribute vmax: '// &
     &          NF_STRERROR(IS) )
           RETURN
      END IF
!
      CALL CLRCH ( HEB%HISTORY )
      IS = NF_GET_ATT_TEXT ( NCID, 0, 'History', HEB%HISTORY )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 6322, IUER, 'GEOS_NC4_TO_HEB', 'Error in '// &
     &         ' an attempt to read attribute History: '// &
     &          NF_STRERROR(IS) )
           RETURN
      END IF
!
      CALL CLRCH ( HEB%SOURCE )
      IS = NF_GET_ATT_TEXT ( NCID, 0, 'Source', HEB%SOURCE )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 6323, IUER, 'GEOS_NC4_TO_HEB', 'Error in '// &
     &         ' an attempt to read attribute Source: '// &
     &          NF_STRERROR(IS) )
           RETURN
      END IF
      CALL TRAN ( 13, HEB%SOURCE, HEB%SOURCE )
!
      CALL CLRCH ( HEB%TITLE )
      IS = NF_GET_ATT_TEXT ( NCID, 0, 'Title', HEB%TITLE )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 6324, IUER, 'GEOS_NC4_TO_HEB', 'Error in '// &
     &         ' an attempt to read attribute Title: '// &
     &          NF_STRERROR(IS) )
           RETURN
      END IF
      IF ( ILEN(HEB%PROD_NAME) == 0 ) HEB%PROD_NAME = HEB%TITLE 
!
      CALL CLRCH ( HEB%INSTITUTION )
      IS = NF_GET_ATT_TEXT ( NCID, 0, 'Institution', HEB%INSTITUTION )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 6325, IUER, 'GEOS_NC4_TO_HEB', 'Error in '// &
     &         ' an attempt to read attribute Institution: '// &
     &          NF_STRERROR(IS) )
           RETURN
      END IF
!
      CALL CLRCH ( HEB%REFERENCES )
      IS = NF_GET_ATT_TEXT ( NCID, 0, 'References', HEB%REFERENCES )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 6326, IUER, 'GEOS_NC4_TO_HEB', 'Error in '// &
     &         ' an attempt to read attribute References: '// &
     &          NF_STRERROR(IS) )
           RETURN
      END IF
!
      CALL CLRCH ( HEB%VERSION_ID )
      IS = NF_GET_ATT_TEXT ( NCID, 0, 'VersionID', HEB%VERSION_ID )
      IF ( IS == 0 ) IS = NF_GET_ATT_TEXT ( NCID, 0, 'NCO', HEB%VERSION_ID )
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
      CALL CLRCH ( HEB%PROD_DATE_TIME )
      IS = NF_GET_ATT_TEXT ( NCID, 0, 'ProductionDateTime', HEB%PROD_DATE_TIME )
!      IF ( IS .NE. 0 ) THEN
!           CALL ERR_LOG ( 6327, IUER, 'GEOS_NC4_TO_HEB', 'Error in '// &
!     &         ' an attempt to read attribute ProductionDateTime: '// &
!     &          NF_STRERROR(IS) )
!           RETURN
!      END IF
!
      IS = NF_GET_ATT_INT ( NCID, ID_VAR_TIM, 'begin_date', IDAT(1) )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 6328, IUER, 'GEOS_NC4_TO_HEB', 'Error in '// &
     &         ' an attempt to read attribute begin_date: '// &
     &          NF_STRERROR(IS) )
           RETURN
      END IF
!
      IS = NF_GET_ATT_INT ( NCID, ID_VAR_TIM, 'begin_time', IDAT(2) )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 6329, IUER, 'GEOS_NC4_TO_HEB', 'Error in '// &
     &         ' an attempt to read attribute begin_time: '// &
     &          NF_STRERROR(IS) )
           RETURN
      END IF
      WRITE ( UNIT=STR_DAT, FMT='(I8,"_",I6)' ) IDAT
      CALL BLANK_TO_ZERO ( STR_DAT(1:15) )
      STR_DAT = STR_DAT(1:4)//'.'//STR_DAT(5:6)//'.'//STR_DAT(7:8)//STR_DAT(9:11)// &
     &          ':'//STR_DAT(12:13)//':'//STR_DAT(14:15)//':00'
      CALL DATE_TO_TIME ( STR_DAT, HEB%MJD, HEB%UTC, IUER )
      HEB%TAI = HEB%UTC
!
      DATE_STR = STR_DAT(1:I_LEN(STR_DAT))//'_'//STR_TIM(1:I_LEN(STR_TIM))
      CALL DATE_TO_TIME ( DATE_STR, HEB%MJD, HEB%UTC, IUER )
      IF ( IUER .NE. 0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 6330, IUER, 'GEOS_NC4_TO_HEB', 'Error in '// &
     &         ' an attempt to decode beginning date '//DATE_STR )
           RETURN
      END IF
!
! --- Allocate dynamic memory for the variable 
!
      HEB%DIMS(1) = DIMLEN_LON
      HEB%DIMS(2) = DIMLEN_LAT
      HEB%DIMS(3) = DIMLEN_LEV
      HEB%DIMS(4) = 1
      ALLOCATE ( HEB%VAL(HEB%DIMS(1),HEB%DIMS(2),HEB%DIMS(3),HEB%DIMS(4)), STAT=IUER )
      IF ( IUER .NE. 0 ) THEN
           CALL CLRCH  ( STR )
           CALL IINCH8 ( INT8(4)*HEB%DIMS(1)*HEB%DIMS(2)*HEB%DIMS(3)*HEB%DIMS(4), STR )
           IUER = -1
           CALL ERR_LOG ( 6331, IUER, 'GEOS_NC4_TO_HEB', 'Error in '// &
     &         'an attempt to allocate '//TRIM(STR)//' bytes of dynamic '// &
     &         'memory for array HEB%VAL' )
           RETURN
      END IF
!
! --- Get the varaiable
!
      IS = NF_GET_VAR_REAL ( NCID, ID_VAR_DAT, HEB%VAL )
      IF ( IS .NE. 0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 6332, IUER, 'GEOS_NC4_TO_HEB', 'Error in '// &
     &                   'getting the values of the SDS parameter "DELP" '// &
     &                   'from file '//FILIN(1:I_LEN(FILIN))//' error: '// &
     &                    NF_STRERROR(IS) )
           RETURN
      END IF
!
! --- Close the NetCDF-file
!
      IS = NF_CLOSE ( NCID )
      HEB%MIN_VALUE = MINVAL(HEB%VAL)
      HEB%MAX_VALUE = MAXVAL(HEB%VAL)
!
      DIR = TRIM(DIROUT)//'/'//STR_DAT(1:4)
      DIR_DESC = OPENDIR ( DIR(1:I_LEN(DIR))//CHAR(0) ) 
      IF ( DIR_DESC .LE. 0 ) THEN
           MODE_I2 = MASK_I2
           IS = MKDIR ( DIR(1:I_LEN(DIR))//CHAR(0), %VAL(MODE_I2) )
           IF ( IS .EQ. -1 ) THEN
                CALL GERROR ( STR )
                CALL ERR_LOG ( 6333, -2, 'GEOS_NC4_TO_HEB', 'Failure '// &
     &              'in an attempt to create output directory '//DIR )
                CALL EXIT ( 1 )
           END IF
         ELSE 
           IP = CLOSEDIR ( %VAL(DIR_DESC) )
      END IF
!
      DIR = TRIM(DIROUT)//'/'//STR_DAT(1:4)//'/'//VAR_OUT_NAM
      DIR_DESC = OPENDIR ( DIR(1:I_LEN(DIR))//CHAR(0) ) 
      IF ( DIR_DESC .LE. 0 ) THEN
           MODE_I2 = MASK_I2
           IS = MKDIR ( DIR(1:I_LEN(DIR))//CHAR(0), %VAL(MODE_I2) )
           IF ( IS .EQ. -1 ) THEN
                CALL GERROR ( STR )
                CALL ERR_LOG ( 6334, -2, 'GEOS_NC4_TO_HEB', 'Failure '// &
     &              'in an attempt to create output directory '//DIR )
                CALL EXIT ( 1 )
           END IF
         ELSE 
           IP = CLOSEDIR ( %VAL(DIR_DESC) )
      END IF
      FILOUT = TRIM(DIROUT)//'/'//STR_DAT(1:4)//'/'//TRIM(VAR_OUT_NAM)//'/'// &
     &         TRIM(VAR_OUT_NAM)//'_'//STR_DAT(1:4)//STR_DAT(6:7)// &
     &         STR_DAT(9:10)//'_'//STR_DAT(12:13)//STR_DAT(15:16)//'.heb'
      HEB%DATA_COMPRESSION = HEB__NONE
      HEB%DATA_OFFSET    = HEB__HDS
      HEB%ENDIAN         = HEB__LE
      HEB%DATA_TRANSFORM = HEB__SCOF
      HEB%DATA_FORMAT    = HEB__R4
      HEB%OFFSET         = 0.0
      HEB%SCALE_FACTOR   = 1.0
      IF ( VAR_NAM  == 'T' ) THEN
           HEB%VALID_RANGE(1) =   0.0
           HEB%VALID_RANGE(2) = 512.0
           HEB%DATA_TRANSFORM = HEB__SCOF
           HEB%DATA_FORMAT    = HEB__I2
           HEB%OFFSET         = 256.0
           HEB%SCALE_FACTOR   = 512.0/64000.0
         ELSE IF ( VAR_NAM == 'U' .OR. VAR_NAM == 'V' .OR. VAR_NAM == 'W' ) THEN
           HEB%VALID_RANGE(1) = -256.0
           HEB%VALID_RANGE(2) =  256.0
           HEB%DATA_TRANSFORM = HEB__SCOF
           HEB%DATA_FORMAT    = HEB__I2
           HEB%OFFSET         = 0.0
           HEB%SCALE_FACTOR   = 512.0/64000.0
         ELSE IF ( VAR_NAM == 'QV' ) THEN
           HEB%VALID_RANGE(1) = EXP(-34.0)
           HEB%VALID_RANGE(2) = EXP(-2.0)
           HEB%DATA_TRANSFORM = HEB__LOG
           HEB%DATA_FORMAT    = HEB__I2
           HEB%OFFSET         = -18.0
           HEB%SCALE_FACTOR   = 32.0/64000.0
         ELSE IF ( VAR_NAM == 'DELP' ) THEN
           HEB%VALID_RANGE(1) = 0.0
           HEB%VALID_RANGE(2) = 6400.0
           HEB%DATA_TRANSFORM = HEB__SCOF
           HEB%DATA_FORMAT    = HEB__I2
           HEB%OFFSET         = 3200.0
           HEB%SCALE_FACTOR   = 6400.0/64000.0
         ELSE IF ( VAR_NAM == 'PHIS' ) THEN
           FILOUT = TRIM(DIROUT)//'/'//STR_DAT(1:4)//'/'//TRIM(VAR_OUT_NAM)//'/'// &
     &              TRIM(VAR_OUT_NAM)//'_height_above_geoid_'//STR_DAT(1:4)//STR_DAT(6:7)// &
     &              STR_DAT(9:10)//'_'//STR_DAT(12:13)//STR_DAT(15:16)//'.heb'
           HEB%UNITS = 'm'
           DO 420 J2=1,HEB%DIMS(2)
              DO 430 J3=1,HEB%DIMS(1)
                 HEB%VAL(J3,J2,1,1) = HEB%VAL(J3,J2,1,1)/ACCREF__MAPL
 430          CONTINUE 
 420       CONTINUE 
           HEB%SDS_NAME  = 'height above the WGS84 geoid'
           HEB%MIN_VALUE = MINVAL(HEB%VAL)
           HEB%MAX_VALUE = MAXVAL(HEB%VAL)
      END IF
!
      IUER = -1
      CALL WRITE_HEB ( HEB, HEB%VAL, FILOUT, IUER )
      IF ( IUER .NE. 0 ) THEN
           CALL GERROR ( STR )
           CALL ERR_LOG ( 6335, -2, 'GEOS_NC4_TO_HEB', 'Failure '// &
     &         'in an attempt to write into output file '//FILOUT )
           CALL EXIT ( 1 )
      END IF
      IF ( ILEN(COMPR_COM) .NE. 0 ) THEN
!
! -------- Now compress the output file 
!
           COM_STR = COMPR_COM(1:I_LEN(COMPR_COM))//' '// &
     &               FILOUT(1:I_LEN(FILOUT))
           CALL SYSTEM ( COM_STR(1:I_LEN(COM_STR))//CHAR(0) )
           WRITE ( 6, * ) 'Written file '//TRIM(FILOUT)//'.bz2'
         ELSE
           WRITE ( 6, * ) 'Written file '//TRIM(FILOUT)
      END IF
      IF ( VAR_NAM == 'PHIS' ) THEN
           DO 440 J4=1,HEB%DIMS(2)
              LAT = -P2I + (J4-1)*PI__NUM/(HEB%DIMS(2)-1)
              DO 450 J5=1,HEB%DIMS(1)
                 LON = 0.0D0 + (J5-1)*PI2/HEB%DIMS(1)
                 IUER = -1
                 HEB%VAL(J5,J4,1,1) = HEB%VAL(J5,J4,1,1) + &
     &               GET_GEOID ( LAT, LON, FIL_BSPL_GEOID_HEB, GEOID_BSPL_HEB, IUER )
                 IF ( IUER .NE. 0 ) THEN
                      CALL ERR_LOG ( 6336, -2, 'GEOS_NC4_TO_HEB', 'Failure '// &
     &                    'in an attempt to compute geoid height' )
                      CALL EXIT ( 1 )
                 END IF
 450          CONTINUE 
 440       CONTINUE 
           HEB%SDS_NAME       = 'height above the WGS84 ellipsoid'
           HEB%MIN_VALUE = MINVAL(HEB%VAL)
           HEB%MAX_VALUE = MAXVAL(HEB%VAL)
           FILOUT = TRIM(DIROUT)//'/'//STR_DAT(1:4)//'/'//TRIM(VAR_OUT_NAM)//'/'// &
     &              TRIM(VAR_OUT_NAM)//'_height_above_ellipsoid_'//STR_DAT(1:4)//STR_DAT(6:7)// &
     &              STR_DAT(9:10)//'_'//STR_DAT(12:13)//STR_DAT(15:16)//'.heb'
!
           IUER = -1
           CALL WRITE_HEB ( HEB, HEB%VAL, FILOUT, IUER )
           IF ( IUER .NE. 0 ) THEN
                CALL GERROR ( STR )
                CALL ERR_LOG ( 6335, -2, 'GEOS_NC4_TO_HEB', 'Failure '// &
     &              'in an attempt to write into output file '//FILOUT )
                CALL EXIT ( 1 )
           END IF
           WRITE ( 6, * ) 'Written file '//TRIM(FILOUT)
      END IF
!
      END  PROGRAM  GEOS_NC4_TO_HEB   !#!#
