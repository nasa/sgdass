      PROGRAM    PRES_NC_TO_HEB
! ************************************************************************
! *                                                                      *
! *   Program PRES_NC_TO_HEB processes input file with surface           *
! *   pressure and writes down the results in the output directory in    *
! *   files in HEB-format: one file per ecpoch.                          *
! *                                                                      *
! *  ### 10-FEB-2014  PRES_NC_TO_HEB  v1.0 (c) L. Petrov 10-FEB-2014 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'malo.i'
      INCLUDE   'heb.i'
      INTEGER*4  M_TIM
      PARAMETER  ( M_TIM = 2048 )
      TYPE     ( HEB__TYPE ) :: HEB_PRES(M_TIM)
      CHARACTER  FILNC*128, DIRHEB*128, FILHEB*128, SUBDIR*128, &
     &           STR*128, DATE_STR*32, PREF*128, COMPR*128, COMPR_COM*128, &
     &           COM_STR*512
      REAL*8     UTC(M_TIM)
      INTEGER*2  MODE_I2
      DATA       MODE_I2 / O'0775' /
      INTEGER*4  MJD(M_TIM), L_TIM, J1, J2, DIR_DESC, IS, IUER
      CHARACTER, EXTERNAL :: MJDSEC_TO_DATE*30
      INTEGER*4, EXTERNAL :: OPENDIR, MKDIR, CLOSEDIR, ILEN, I_LEN
!
      COMPR = 'none'
      IF ( IARGC() < 3 ) THEN
           WRITE ( 6, * ) 'Usage: pres_nc_to_heb filin dirout pref [compr]'
           CALL EXIT ( 1 )
         ELSE 
           CALL GETARG ( 1, FILNC  ) 
           CALL GETARG ( 2, DIRHEB ) 
           CALL GETARG ( 3, PREF   ) 
           IF ( IARGC() .GE. 4 ) THEN
                CALL GETARG ( 4, COMPR ) 
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
         ELSE 
           IUER = -1
           CALL ERR_LOG ( 6311, IUER, 'PRES_NC_TO_HEB', 'Unsupported '// &
     &         'compression method: '//COMPR//' . Supported methods: '// &
     &         ' none gzip bzip2 pbzip2 pbzip_p1' )
           CALL EXIT ( 1 )
      END IF
!
      DIR_DESC = OPENDIR ( DIRHEB(1:I_LEN(DIRHEB))//CHAR(0) )
      IF ( DIR_DESC == 0 )    THEN
           IS = MKDIR ( DIRHEB(1:I_LEN(DIRHEB))//CHAR(0), %VAL(MODE_I2) )
           IF ( IS .NE. 0 ) THEN
                CALL CLRCH  ( STR )
                CALL GERROR ( STR )
                IUER = -1
                CALL ERR_LOG ( 6311, IUER, 'PRES_NC_TO_HEB', &
     &              'Failure to create directory '// &
     &               DIRHEB(1:I_LEN(DIRHEB))//' -- '//STR )
                CALL EXIT ( 1 )
           END IF
        ELSE 
          IS = CLOSEDIR ( %VAL(DIR_DESC) )
      END IF
!
      IUER = -1
      CALL READ_PRES_NC ( FILNC, M_TIM, L_TIM, MJD, UTC, HEB_PRES, IUER )
      IF ( IUER .NE. 0 ) THEN
           CALL ERR_LOG ( 6312, IUER, 'PRES_NC_TO_HEB', 'Failure in '// &
     &         'an attempt parse input file with pressure '//FILNC )
           CALL EXIT ( 1 )
      END IF
!
!!      write ( 6, * ) ' l_tim= ', l_tim ! %%%%
      DO 410 J1=1,L_TIM
         IUER = -1
         DATE_STR = MJDSEC_TO_DATE ( MJD(J1), UTC(J1), IUER )
         SUBDIR = DIRHEB(1:I_LEN(DIRHEB))//'/'//DATE_STR(1:4)
         DIR_DESC = OPENDIR ( SUBDIR(1:I_LEN(SUBDIR))//CHAR(0) )
         IF ( DIR_DESC == 0 )    THEN
              IS = MKDIR ( SUBDIR(1:I_LEN(SUBDIR))//CHAR(0), %VAL(MODE_I2) )
              IF ( IS .NE. 0 ) THEN
                   CALL CLRCH  ( STR )
                   CALL GERROR ( STR )
                   IUER = -1
                   CALL ERR_LOG ( 6313, IUER, 'PRES_NC_TO_HEB', &
     &                 'Failure to create directory '// &
     &                  SUBDIR(1:I_LEN(SUBDIR))//' -- '//STR )
                   CALL EXIT ( 1 )
              END IF
            ELSE 
              IS = CLOSEDIR ( %VAL(DIR_DESC) )
         END IF
!
         FILHEB = SUBDIR(1:I_LEN(SUBDIR))//'/'//PREF(1:I_LEN(PREF))// &
     &            DATE_STR(1:4)//DATE_STR(6:7)//DATE_STR(9:10)// &
     &            '_'//DATE_STR(12:13)//DATE_STR(15:16)//'.heb'
!!             write ( 6, * ) 'j1= ', int2(j1), ' filheb= '//filheb(1:i_len(filheb)) ! %%%
!
         HEB_PRES(J1)%FILE_NAME = FILHEB
         IUER = -1
         CALL WRITE_HEB ( HEB_PRES(J1), HEB_PRES(J1)%VAL, FILHEB, IUER )
         IF ( IUER .NE. 0 ) THEN
              IUER = -1
              CALL ERR_LOG ( 6314, IUER, 'PRES_NC_TO_HEB', 'Failure '// &
     &            'in an attempt to write into output file '//FILHEB )
              CALL EXIT ( 1 )
         END IF
         IF ( ILEN(COMPR_COM) .NE. 0 ) THEN
!
! ----------- Now compress the output file 
!
              COM_STR = COMPR_COM(1:I_LEN(COMPR_COM))//' '// &
     &                  FILHEB(1:I_LEN(FILHEB))
              CALL SYSTEM ( COM_STR(1:I_LEN(COM_STR))//CHAR(0) )
              WRITE ( 6, 110 ) J1, L_TIM, FILHEB(1:I_LEN(FILHEB))//'.bz2', CHAR(13)
            ELSE 
              WRITE ( 6, 110 ) J1, L_TIM, FILHEB(1:I_LEN(FILHEB)), CHAR(13)
 110          FORMAT ( '  ', I5, ' ( ', I5, ' )  Written file ',A,2X,A$ )
         END IF
         CALL FLUSH ( 6  )
 410  CONTINUE 
      WRITE ( 6, '(A)' ) ' '
!
      END   PROGRAM    PRES_NC_TO_HEB
!
! ------------------------------------------------------------------------
!
      SUBROUTINE READ_PRES_NC ( FILIN, M_TIM, L_TIM, MJD_PRES, UTC_PRES, &
     &                          HEB_PRES, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine READ_PRES_NC 
! *                                                                      *
! *  ### 10-FEB-2014   READ_PRES_NC 1.0 (c)  L. Petrov  10-FEB-2014 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      CHARACTER  FILIN*(*)
      INCLUDE   'netcdf.inc'
      INCLUDE   'heb.i'
      TYPE     ( HEB__TYPE ) :: HEB_PRES(M_TIM)
      INTEGER*4  M_TIM, L_TIM, MJD_PRES(M_TIM), IUER
      REAL*8     UTC_PRES(M_TIM)
      CHARACTER  STR*128, STR1*128, PAR_NAM*128, UNIT_NAM*128, &
     &           INST_NAM*128, PROD_NAM*128
      INTEGER*4  IS, NCID,   &
     &           DIMLEN_TIM, DIMLEN_LEV, DIMLEN_LAT, DIMLEN_LON, VECDIM(4), &
     &           ID_DIM_TIM, ID_DIM_LEV, ID_DIM_LAT, ID_DIM_LON, ID_VAR_TIM, &
     &           ID_VAR_BPR
      REAL*4,    ALLOCATABLE :: BPR_ARR(:,:,:,:)
      REAL*8,    ALLOCATABLE :: TIM_ARR(:)
      REAL*4     PRES_ADD, FILL_VALUE
      REAL*8     UTC_BEG
      INTEGER*4  J1, J2, J3, J4, IP, MJD_BEG, IDAY, L_LON, L_LAT, IER
      INTEGER*4, EXTERNAL :: ILEN, I_LEN
!
      VECDIM(1) = 1
      VECDIM(2) = 2
      VECDIM(3) = 3
      VECDIM(4) = 4
!
!	float bottom_pressure(time, depth, lat, lon) ;
!
! --- Open the new ouput file in netcdf format
!
      IS = NF_OPEN ( FILIN, NF_NOWRITE, NCID )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 6321, IUER, 'READ_PRES_NC', 'Error in an '// &
     &         'attempt to open the netcf file with ocean bootom pressure '// &
     &         FILIN(1:I_LEN(FILIN))//' NF_OPEN: '//NF_STRERROR(IS) )
           RETURN
      END IF
!
! --- Learn the ID of dimension: 'lon'
!
      IS = NF_INQ_DIMID ( NCID, 'lon', ID_DIM_LON )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 6322, IUER, 'READ_PRES_NC', 'Error in '// &
     &         ' an attempt to read dimension lon: '// &
     &          NF_STRERROR(IS) )
           RETURN
      END IF
!
! --- Learn the ID of dimensions 'lat'
!
      IS = NF_INQ_DIMID ( NCID, 'lat', ID_DIM_LAT )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 6323, IUER, 'READ_PRES_NC', 'Error in '// &
     &         ' an attempt to read dimension lat: '// &
     &          NF_STRERROR(IS) )
           RETURN
      END IF
!
! --- Learn the ID of dimensions 'depth'
!
      IS = NF_INQ_DIMID ( NCID, 'depth', ID_DIM_LEV )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 6324, IUER, 'READ_PRES_NC', 'Error in '// &
     &         ' an attempt to read dimension depth: '// &
     &          NF_STRERROR(IS) )
           RETURN
      END IF
!
! --- Learn the ID of dimensions 'time'
!
      IS = NF_INQ_DIMID ( NCID, 'time', ID_DIM_TIM )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 6325, IUER, 'READ_PRES_NC', 'Error in '// &
     &         ' an attempt to read dimension lat: '// &
     &          NF_STRERROR(IS) )
           RETURN
      END IF
!
! --- Get the length of the dimension "lon"
!
      IS = NF_INQ_DIMLEN ( NCID, ID_DIM_LON, DIMLEN_LON  )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 6326, IUER, 'READ_PRES_NC', 'Error in getting '// &
     &         'the length of the dimension "lon" in file '// &
     &          FILIN(1:I_LEN(FILIN))//' error: '// &
     &          NF_STRERROR(IS) )
           RETURN
      END IF
!
! --- Get the length of the dimension "lat"
!
      IS = NF_INQ_DIMLEN ( NCID, ID_DIM_LAT, DIMLEN_LAT  )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 6327, IUER, 'READ_PRES_NC', 'Error in getting '// &
     &         'the length of the dimension "lat" in file '// &
     &          FILIN(1:I_LEN(FILIN))//' error: '// &
     &          NF_STRERROR(IS) )
           RETURN
      END IF
!
! --- Get the length of the dimension "depth"
!
      IS = NF_INQ_DIMLEN ( NCID, ID_DIM_LEV, DIMLEN_LEV  )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 6328, IUER, 'READ_PRES_NC', 'Error in getting '// &
     &         'the length of the dimension "depth" in file '// &
     &          FILIN(1:I_LEN(FILIN))//' error: '// &
     &          NF_STRERROR(IS) )
           RETURN
      END IF
!
! --- Get the length of the dimension "time"
!
      IS = NF_INQ_DIMLEN ( NCID, ID_DIM_TIM, DIMLEN_TIM  )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 6329, IUER, 'READ_PRES_NC', 'Error in getting '// &
     &         'the length of the dimension "time" in file '// &
     &          FILIN(1:I_LEN(FILIN))//' error: '// &
     &          NF_STRERROR(IS) )
           RETURN
      END IF
!!      write ( 6, * ) ' dims= ', dimlen_lon, dimlen_lat, dimlen_lev, dimlen_tim ! %%%
!
      L_TIM = DIMLEN_TIM 
      IF ( L_TIM > M_TIM ) THEN
           CALL CLRCH ( STR  ) 
           CALL CLRCH ( STR1 ) 
           CALL INCH  ( M_TIM, STR  )
           CALL INCH  ( L_TIM, STR1 )
           CALL ERR_LOG ( 6330, IUER, 'READ_PRES_NC', 'Parameter M_TIM '// &
     &          STR(1:I_LEN(STR))//' is too small. Input file '// &
     &          FILIN(1:I_LEN(FILIN))//' has data for '//STR1(1:I_LEN(STR1))// &
     &         ' epochs' )
           RETURN 
      END IF
!
! --- Learn the ID of the variable "time"
!
      IS = NF_INQ_VARID ( NCID, 'time', ID_VAR_TIM )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 6331, IUER, 'READ_PRES_NC', 'Variable "time" '// &
     &         'was not found in the input netcdf file '// &
     &          FILIN(1:I_LEN(FILIN))//' error: '//NF_STRERROR(IS) )
           RETURN
      END IF
!
! --- Read attribute "units"
!
      IS = NF_GET_ATT_TEXT ( NCID, ID_VAR_TIM, 'units', STR )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 6332, IUER, 'READ_PRES_NC', 'Error in '// &
     &          ' an attempt to get the attribute '// &
     &          NF_STRERROR(IS) )
           RETURN
      END IF
!!        write ( 6, * ) 'str  >>'//str(1:i_len(str)) ! %%%%
      IF ( STR(1:10) == 'days since' ) THEN
           STR(22:22) = '_'
           CALL ERR_PASS ( IUER, IER )
           CALL DATE_TO_TIME ( STR(12:30), MJD_BEG, UTC_BEG, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 6333, IUER, 'READ_PRES_NC', 'Error in '// &
     &          ' an attempt to parse "days since" date '//STR )
                RETURN
           END IF
         ELSE 
           CALL ERR_LOG ( 6334, IUER, 'READ_PRES_NC', 'Unrecognizable '// &
     &         ' attribute time:units '//STR )
           RETURN
      END IF
!
! --- Get the variable: time
!
      ALLOCATE ( TIM_ARR(L_TIM), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 6335, IUER, 'READ_PRES_NC', 'Failure to '// &
     &         'allocate memory for the temporary array TIM_ARR' )
           RETURN
      END IF
      IS = NF_GET_VAR_DOUBLE ( NCID, ID_VAR_TIM, TIM_ARR )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 6336, IUER, 'READ_PRES_NC', 'Error in '// &
     &         'getting the values of the variable "time" from '// &
     &         'the file '//FILIN(1:I_LEN(FILIN))//' error: '// &
     &          NF_STRERROR(IS) )
           RETURN
      END IF
      DO 410 J1=1,L_TIM
         UTC_PRES(J1) = UTC_BEG + TIM_ARR(J1)*86400.0D0
         IDAY = IDINT ( UTC_PRES(J1)/86400.0D0 )
         MJD_PRES(J1) = MJD_BEG + IDAY
         UTC_PRES(J1)  = UTC_PRES(J1) - IDAY*86400.0D0
 410  CONTINUE 
      DEALLOCATE ( TIM_ARR )
!
! --- Learn the ID of the variable "bottom_pressure"
!
      IS = NF_INQ_VARID ( NCID, 'bottom_pressure', ID_VAR_BPR )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 6331, IUER, 'READ_PRES_NC', 'Variable '// &
     &         '"bottom_pressure" was not found in the input netcdf '// &
     &         'file '//FILIN(1:I_LEN(FILIN))//' error: '//NF_STRERROR(IS) )
           RETURN
      END IF
      L_LON = DIMLEN_LON
      L_LAT = DIMLEN_LAT
!
      ALLOCATE ( BPR_ARR(L_LON,L_LAT,1,L_TIM), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH  ( STR )
           CALL IINCH8 ( INT8(L_LON)*INT8(L_LAT)*INT8(L_TIM), STR )
           CALL ERR_LOG ( 6335, IUER, 'READ_PRES_NC', 'Failure to '// &
     &         'allocate '//STR(1:I_LEN(STR))//' bytes memory for '// &
     &         'the temporary array BPR_ARR' )
           RETURN
      END IF
      IS = NF_GET_VAR_REAL ( NCID, ID_VAR_BPR, BPR_ARR )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 6336, IUER, 'READ_PRES_NC', 'Error in '// &
     &         'getting the values of the variable "bottom_presssure" '// &
     &         'from the file '//FILIN(1:I_LEN(FILIN))//' error: '// &
     &          NF_STRERROR(IS) )
           RETURN
      END IF
!
! --- Read attribute "long_name"
!
      IS = NF_GET_ATT_TEXT ( NCID, ID_VAR_BPR, 'long_name', PAR_NAM )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 6337, IUER, 'READ_PRES_NC', 'Error in '// &
     &          ' an attempt to get the attribute lon_name '// &
     &          NF_STRERROR(IS) )
           RETURN
      END IF
!
! --- Read attribute "long_name"
!
      IS = NF_GET_ATT_REAL ( NCID, ID_VAR_BPR, '_FillValue', FILL_VALUE )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 6338, IUER, 'READ_PRES_NC', 'Error in '// &
     &          ' an attempt to get the attribute fill_value '// &
     &          NF_STRERROR(IS) )
           RETURN
      END IF
      IP = INDEX ( PAR_NAM, '[' )
      IF ( IP > 0 ) THEN
           UNIT_NAM = PAR_NAM(IP+1:)
           CALL CLRCH ( PAR_NAM(IP:) )
           IP = INDEX ( UNIT_NAM, ']' )
           IF ( IP > 0) THEN
                CALL CLRCH ( UNIT_NAM(IP:) )
           END IF
           PROD_NAM = 'Ocean bottom pressure from OMCT model'
         ELSE 
           PAR_NAM = 'Bottom pressure from PRES'
           UNIT_NAM = 'Pa'
           PROD_NAM = 'Ocean bottom pressure from PRES model'
      END IF
      INST_NAM = 'GFZ Potstam'
!
      IS = NF_CLOSE ( NCID )
!
      DO 420 J2=1,L_TIM
         HEB_PRES(J2)%DIMS(1) = L_LON
         HEB_PRES(J2)%DIMS(2) = L_LAT
         HEB_PRES(J2)%DIMS(3) = 1
         HEB_PRES(J2)%DIMS(4) = 1
         HEB_PRES(J2)%SDS_NAME  = PAR_NAM
         HEB_PRES(J2)%UNITS     = UNIT_NAM
         HEB_PRES(J2)%PROD_NAME = PROD_NAM
         HEB_PRES(J2)%HISTORY   = ' '
         HEB_PRES(J2)%TITLE     = PROD_NAM
         HEB_PRES(J2)%INSTITUTION = INST_NAM
         HEB_PRES(J2)%SOURCE  = ' '
         HEB_PRES(J2)%REFERENCES  = ' '
         HEB_PRES(J2)%PROD_DATE_TIME  = ' '
         HEB_PRES(J2)%VERSION_ID = ' '
         HEB_PRES(J2)%VALID_RANGE(1) = -50000.0D0
         HEB_PRES(J2)%VALID_RANGE(2) =  50000.0D0
         HEB_PRES(J2)%FILL_VALUE = FILL_VALUE
         HEB_PRES(J2)%OFFSET      = 0.0
         HEB_PRES(J2)%SCALE_FACTOR = 1.0
         HEB_PRES(J2)%DATA_FORMAT = HEB__R4
         HEB_PRES(J2)%DATA_COMPRESSION = HEB__NONE
         HEB_PRES(J2)%DATA_TRANSFORM = HEB__NONE
!
         HEB_PRES(J2)%MJD = MJD_PRES(J2)
         HEB_PRES(J2)%UTC = UTC_PRES(J2)
         HEB_PRES(J2)%TAI = UTC_PRES(J2)
         ALLOCATE ( HEB_PRES(J2)%VAL(HEB_PRES(J2)%DIMS(1),HEB_PRES(J2)%DIMS(2),HEB_PRES(J2)%DIMS(3),HEB_PRES(J2)%DIMS(4)), &
     &              STAT=IER )
         IF ( IER .NE. 0 ) THEN
              CALL CLRCH  ( STR )
              CALL IINCH8 ( INT8(L_LON)*INT8(L_LAT), STR )
              CALL INCH   ( J2, STR1 )
              CALL ERR_LOG ( 6339, IUER, 'PRES_NC_TO_HEB', 'Failure to '// &
     &            'allocate '//STR(1:I_LEN(STR))//' bytes of dynamic '// &
     &            'memory for the '//STR1(1:I_LEN(STR1))//'th epoch of '// &
     &            'input pressure data' )
              RETURN 
         END IF
         IF ( INDEX ( PAR_NAM, 'RRSea water pressure' ) == 1 ) THEN
              PRES_ADD = 20000.0D0
            ELSE
              PRES_ADD = 0.0D0
         END IF
         DO 430 J3=1,L_LAT
            DO 440 J4=1,L_LON
               HEB_PRES(J2)%VAL(J4,J3,1,1) = BPR_ARR(J4,L_LAT+1-J3,1,J2) + PRES_ADD
               IF ( ABS(HEB_PRES(J2)%VAL(J4,J3,1,1)) > 1.0E10 ) THEN
                    HEB_PRES(J2)%VAL(J4,J3,1,1) = 0.0
               END IF
 440        CONTINUE 
 430     CONTINUE 
         HEB_PRES(J2)%MIN_VALUE = MINVAL(HEB_PRES(J2)%VAL)
         HEB_PRES(J2)%MAX_VALUE = MAXVAL(HEB_PRES(J2)%VAL)
         HEB_PRES(J2)%FILL_VALUE = 0.0
 420  CONTINUE 
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  READ_PRES_NC  !#!#
