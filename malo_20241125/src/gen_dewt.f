      PROGRAM    GEN_DEWT
! ************************************************************************
! *                                                                      *
! *   Routine GEN_DEWT generates dew temperature using                   *
! *   1) 3D input field of atmosphere layer thickness;                   *
! *   2) 3D input field of air temperature;                              *
! *   3) 3D input field of specific humidity;                            *
! *   4) 2D input grid of the heights of grid point above                *
! *      the WGS84 reference ellipsoid;                                  *
! *   5) 2D output grid of the surface height avereged over the pixel    *
! *      area.                                                           *
! *                                                                      *
! *   Usage: gen_dewt heb-dir date inp_grid_height deg                   *
! *          output_grid_height output_dewt                              *
! *                                                                      *
! *   The results is written in heb-format.                              *
! *                                                                      *
! * ###  13-MAR-2013    GEN_DEWT   v2.0 (c)  L. Petrov  13-MAR-2013  ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'malo.i'
      INCLUDE   'heb.i'
      TYPE     ( MALO__TYPE ) :: MAL
      TYPE     ( HEB__TYPE  ) :: HEB_DELP, HEB_T, HEB_Q, HEB_G, &
     &                           HEB_OGH, HEB_DEWT, HEB_ST, HEB_SPR
      CHARACTER  DIR_HEB*128, OBS_DATE*128, FIL_IGH*128, FIL_OGH*128, &
     &           FIL_OUT*128, DIR_DEWT_OUT*128, &
     &           DIR_ST_OUT*128,  STR*128, CAL_DATE*19
      CHARACTER  GEN_DEWT__LABEL*32
      REAL*8     HEI_DEW_POINT 
      PARAMETER  ( GEN_DEWT__LABEL = 'GEN_DEWT Vers  2.0 of 2013.04.04' )
      PARAMETER  ( HEI_DEW_POINT = 100.0D0 )
      INTEGER*8  DIR_DESC
      INTEGER*4  ID, DEG, IUER
      CHARACTER, EXTERNAL :: GET_CDATE*19
      INTEGER*4, EXTERNAL :: I_LEN, ILEN
      INTEGER*8, EXTERNAL :: OPENDIR, CLOSEDIR, LINDEX
!
      IF ( IARGC() < 6 ) THEN
           WRITE ( 6, '(A)' ) 'Usage: gen_dewt heb-dir date inp_grid_height '// &
     &                        'output_grid_height dewt_output_dir st_output_dir'
           CALL EXIT ( 1 )
         ELSE 
           CALL GETARG ( 1, DIR_HEB  )
           CALL GETARG ( 2, OBS_DATE )
           CALL GETARG ( 3, FIL_IGH  )
           CALL GETARG ( 4, FIL_OGH  )
           CALL GETARG ( 5, DIR_DEWT_OUT  )
           CALL GETARG ( 6, DIR_ST_OUT  )
      END IF
!
      DIR_DESC = OPENDIR ( DIR_DEWT_OUT(1:I_LEN(DIR_DEWT_OUT))//CHAR(0) )
      IF ( DIR_DESC > 0 ) THEN 
           DIR_DESC = CLOSEDIR ( %VAL(DIR_DESC) )
         ELSE
           CALL ERR_LOG ( 6701, IUER, 'GEN_DEWT', 'Wrong 5th argument: '// &
     &         'output directory '//DIR_DEWT_OUT(1:I_LEN(DIR_DEWT_OUT))// &
     &         ' does not exist' )
           CALL EXIT ( 1 )
      END IF
!
      DIR_DESC = OPENDIR ( DIR_ST_OUT(1:I_LEN(DIR_ST_OUT))//CHAR(0) )
      IF ( DIR_DESC > 0 ) THEN 
           DIR_DESC = CLOSEDIR ( %VAL(DIR_DESC) )
         ELSE
           CALL ERR_LOG ( 6702, IUER, 'GEN_DEWT', 'Wrong 6th argument: '// &
     &         'output directory '//DIR_ST_OUT(1:I_LEN(DIR_ST_OUT))// &
     &         ' does not exist' )
           CALL EXIT ( 1 )
      END IF
!
      IUER = -1
      CALL READ_DQT_HEB ( DIR_HEB, OBS_DATE, HEB_DELP, HEB_Q, HEB_T, IUER )
      IF ( IUER .NE. 0 ) THEN
           CALL ERR_LOG ( 6703, IUER, 'GEN_DEWT', 'Error in an attempt '// &
     &                   'to read numerical weather model for date '//OBS_DATE )
           CALL EXIT ( 1 )
      END IF
!
      IUER = -1
      CALL READ_HEB_HEADER ( FIL_IGH, HEB_G, IUER )
      IF ( IUER  .NE. 0 ) THEN
           CALL GERROR  ( STR )
           CALL ERR_LOG ( 6704, IUER, 'GEN_DEWT', 'Error in reading '// &
     &         'heb-file '//FIL_IGH )
           CALL EXIT ( 1 )
      END IF
!
      IUER = -1
      CALL READ_HEB_DATA ( FIL_IGH, HEB_G, IUER )
      IF ( IUER  .NE. 0 ) THEN
           CALL GERROR  ( STR )
           CALL ERR_LOG ( 6705, IUER, 'GEN_DEWT', 'Error in reading '// &
     &         'heb-file '//FIL_IGH )
           CALL EXIT ( 1 )
      END IF
!
      IUER = -1
      CALL READ_HEB_HEADER ( FIL_OGH, HEB_OGH, IUER )
      IF ( IUER  .NE. 0 ) THEN
           CALL GERROR  ( STR )
           CALL ERR_LOG ( 6706, IUER, 'GEN_DEWT', 'Error in reading '// &
     &         'heb-file '//FIL_OGH )
           CALL EXIT ( 1 )
      END IF
!
      IUER = -1
      CALL READ_HEB_DATA ( FIL_OGH, HEB_OGH, IUER )
      IF ( IUER  .NE. 0 ) THEN
           CALL GERROR  ( STR )
           CALL ERR_LOG ( 6707, IUER, 'GEN_DEWT', 'Error in reading '// &
     &         'heb-file '//FIL_OGH )
           CALL EXIT ( 1 )
      END IF
!
      IUER = -1
      CALL MALO_COMP_DEWT ( HEI_DEW_POINT, HEB_DELP, HEB_T, HEB_Q, HEB_G, &
     &                      HEB_OGH, HEB_DEWT, HEB_ST, HEB_SPR, IUER )
      IF ( IUER .NE. 0 ) THEN
           CALL ERR_LOG ( 6708, IUER, 'GEN_DEWT', 'Error in an attempt '// &
     &         'to compute surface atmospheric pressure' )
           CALL EXIT ( 1 )
      END IF
!
      FIL_OUT = DIR_DEWT_OUT(1:I_LEN(DIR_DEWT_OUT))//'/dewt_'// &
     &          OBS_DATE(1:I_LEN(OBS_DATE))//'.heb'
      ID = LINDEX ( FIL_OUT, '/' )
!
! --- Heb header parameters
!
      IF ( HEB_DELP%TITLE(1:5) == 'MERRA' ) THEN
           CAL_DATE = OBS_DATE(1:4)//'.'//OBS_DATE(5:6)//'.'//OBS_DATE(7:11)//':'// &
     &                OBS_DATE(12:13)//'_00.0'
           IUER = -1
           CALL DATE_TO_TIME ( CAL_DATE, HEB_DEWT%MJD, HEB_DEWT%UTC, IUER )
           IF ( IUER .NE. 0 ) THEN
                CALL ERR_LOG ( 6709, IUER, 'GEN_DEWT', 'Wrong observation date '// &
     &               OBS_DATE )
                CALL EXIT ( 1 )
           END IF
           HEB_DEWT%TAI  = HEB_DEWT%UTC
      END IF
!
      HEB_DEWT%DIMS = HEB_OGH%DIMS
      HEB_DEWT%DATA_OFFSET = HEB__HDS
      HEB_DEWT%ENDIAN      = HEB__LE
      HEB_DEWT%DATA_TRANSFORM = HEB__NONE
      HEB_DEWT%FILL_VALUE     = 1.0E15
      HEB_DEWT%OFFSET         = 0.0
      HEB_DEWT%SCALE_FACTOR   = 1.0
      HEB_DEWT%DATA_COMPRESSION = HEB__NONE
      CALL CLRCH ( STR )
      WRITE ( UNIT=STR(1:8), FMT='(F8.1)' ) HEI_DEW_POINT 
      CALL CHASHL ( STR(1:8) )
      HEB_DEWT%SDS_NAME       = 'Dew point temperature at '//STR(1:I_LEN(STR))// &
     &                          ' m above surface' 
      HEB_DEWT%UNITS          = 'K'
      HEB_DEWT%DATA_FORMAT    = HEB__R4
      HEB_DEWT%MIN_VALUE      = MINVAL ( HEB_DEWT%VAL )
      HEB_DEWT%MAX_VALUE      = MAXVAL ( HEB_DEWT%VAL )
      HEB_DEWT%VALID_RANGE(1) = 1.0
      HEB_DEWT%VALID_RANGE(2) = 400.0
      HEB_DEWT%PROD_DATE_TIME = GET_CDATE()
!
      HEB_DEWT%FILE_NAME      = FIL_OUT(ID+1:)
      HEB_DEWT%HISTORY        = 'Processed using input numerical weather '// &
     &                         'model and input digital elevation map'
      HEB_DEWT%SOURCE         = '1) '//HEB_DELP%SOURCE(1:I_LEN(HEB_DELP%SOURCE))// &
     &                         '; 2) '//HEB_OGH%SOURCE(1:I_LEN(HEB_OGH%SOURCE))// &
     &                         ' '//HEB_OGH%PROD_NAME 
      HEB_DEWT%TITLE          = 'Surface atmospheric pressure'
      HEB_DEWT%PROD_NAME      = HEB_DEWT%SDS_NAME       
      HEB_DEWT%INSTITUTION    = 'Astrogeo Center'
      HEB_DEWT%REFERENCES     = 'http://astrogeo.org/malo/'
      HEB_DEWT%VERSION_ID     = GEN_DEWT__LABEL 
!
      IUER = -1
      CALL WRITE_HEB ( HEB_DEWT, HEB_DEWT%VAL, FIL_OUT, IUER )
      IF ( IUER .NE. 0 ) THEN
           CALL ERR_LOG ( 6710, IUER, 'GEN_DEWT', 'Error in an attempt '// &
     &         'to write surface dew point temperature into the output '// &
     &         'file '//FIL_OUT )
           CALL EXIT ( 1 )
      END IF
!
! === Now we need to wrtie surface air temperature
!
      FIL_OUT = DIR_ST_OUT(1:I_LEN(DIR_ST_OUT))//'/st_'// &
     &          OBS_DATE(1:I_LEN(OBS_DATE))//'.heb'
      ID = LINDEX ( FIL_OUT, '/' )
!
! --- Heb header parameters
!
      IF ( HEB_DELP%TITLE(1:5) == 'MERRA' ) THEN
           CAL_DATE = OBS_DATE(1:4)//'.'//OBS_DATE(5:6)//'.'//OBS_DATE(7:11)//':'// &
     &                OBS_DATE(12:13)//'_00.0'
           IUER = -1
           CALL DATE_TO_TIME ( CAL_DATE, HEB_ST%MJD, HEB_ST%UTC, IUER )
           IF ( IUER .NE. 0 ) THEN
                CALL ERR_LOG ( 6711, IUER, 'GEN_DEWT', 'Wrong observation date '// &
     &               OBS_DATE )
                CALL EXIT ( 1 )
           END IF
           HEB_ST%TAI  = HEB_ST%UTC
      END IF
!
      HEB_ST%DIMS = HEB_OGH%DIMS
      HEB_ST%DATA_OFFSET = HEB__HDS
      HEB_ST%ENDIAN      = HEB__LE
      HEB_ST%DATA_TRANSFORM = HEB__NONE
      HEB_ST%FILL_VALUE     = 1.0E15
      HEB_ST%OFFSET         = 0.0
      HEB_ST%SCALE_FACTOR   = 1.0
      HEB_ST%DATA_COMPRESSION = HEB__NONE
      CALL CLRCH ( STR )
      WRITE ( UNIT=STR(1:8), FMT='(F8.1)' ) HEI_DEW_POINT 
      CALL CHASHL ( STR(1:8) )
      HEB_ST%SDS_NAME       = 'Air temperature at '//STR(1:I_LEN(STR))// &
     &                          ' m above surface' 
      HEB_ST%UNITS          = 'K'
      HEB_ST%DATA_FORMAT    = HEB__R4
      HEB_ST%MIN_VALUE      = MINVAL ( HEB_ST%VAL )
      HEB_ST%MAX_VALUE      = MAXVAL ( HEB_ST%VAL )
      HEB_ST%VALID_RANGE(1) = 1.0
      HEB_ST%VALID_RANGE(2) = 400.0
      HEB_ST%PROD_DATE_TIME = GET_CDATE()
!
      HEB_ST%FILE_NAME      = FIL_OUT(ID+1:)
      HEB_ST%HISTORY        = 'Processed using input numerical weather '// &
     &                         'model and input digital elevation map'
      HEB_ST%SOURCE         = '1) '//HEB_DELP%SOURCE(1:I_LEN(HEB_DELP%SOURCE))// &
     &                         '; 2) '//HEB_OGH%SOURCE(1:I_LEN(HEB_OGH%SOURCE))// &
     &                         ' '//HEB_OGH%PROD_NAME 
      HEB_ST%TITLE          = 'Surface atmospheric pressure'
      HEB_ST%PROD_NAME      = HEB_ST%SDS_NAME       
      HEB_ST%INSTITUTION    = 'Astrogeo Center'
      HEB_ST%REFERENCES     = 'http://astrogeo.org/malo/'
      HEB_ST%VERSION_ID     = GEN_DEWT__LABEL 
!
      IUER = -1
      CALL WRITE_HEB ( HEB_ST, HEB_ST%VAL, FIL_OUT, IUER )
      IF ( IUER .NE. 0 ) THEN
           CALL ERR_LOG ( 6712, IUER, 'GEN_DEWT', 'Error in an attempt '// &
     &         'to write surface temperature pressure into the output '// &
     &         'file '//FIL_OUT )
           CALL EXIT ( 1 )
      END IF
!
      END  PROGRAM  GEN_DEWT  !#!  
