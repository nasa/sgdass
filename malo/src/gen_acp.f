      PROGRAM    GEN_ACP
! ************************************************************************
! *                                                                      *
! *   Routine GEN_ACP generates two output fields: 1) the field of the   *
! *   atmosphere chemical potential at the specified height above and    *
! *   the air temperature at than height. It uses the following import   *
! *   datasets:                                                          *
! *                                                                      *
! *   1) 3D input field of atmosphere layer thickness;                   *
! *   2) 3D input field of air temperature;                              *
! *   3) 3D input field of specific humidity;                            *
! *   4) 2D input grid of the heights of grid point above                *
! *      the WGS84 reference ellipsoid;                                  *
! *   5) 2D output grid of the surface height avereged over the pixel    *
! *      area.                                                           *
! *                                                                      *
! *   Usage: gen_acp heb-dir height date inp_grid_height deg             *
! *          output_grid_height output_acp output_rh                     *
! *                                                                      *
! *   The results is written in heb-format.                              *
! *                                                                      *
! * ###  13-MAR-2013     GEN_ACP   v4.0 (c)  L. Petrov  28-NOV-2017 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'malo.i'
      INCLUDE   'heb.i'
      TYPE     ( MALO__TYPE ) :: MAL
      TYPE     ( HEB__TYPE  ) :: HEB_DELP, HEB_T, HEB_Q, HEB_G, &
     &                           HEB_OGH, HEB_ACP, HEB_ST, HEB_RH
      CHARACTER  DIR_HEB*128, OBS_DATE*128, FIL_IGH*128, FIL_OGH*128, &
     &           FIL_OUT*128, DIR_ACP_OUT*128, &
     &           DIR_ST_OUT*128,  DIR_RH_OUT*128, STR*128, CAL_DATE*19
      CHARACTER  GEN_ACP__LABEL*32
      REAL*8     HEI_ABOVE_SUR, COO_HLP(3), ACP_VAL
      PARAMETER  ( GEN_ACP__LABEL = 'GEN_ACP Vers  4.0  of 2017.11.28' )
      INTEGER*8  DIR_DESC
      INTEGER*4  ID, DEG, IUER
      CHARACTER, EXTERNAL :: GET_CDATE*19
      INTEGER*4, EXTERNAL :: I_LEN, ILEN
      INTEGER*8, EXTERNAL :: OPENDIR, CLOSEDIR, LINDEX
!
      IF ( IARGC() < 8 ) THEN
           WRITE ( 6, '(A)' ) 'Usage: gen_acp height heb-dir date inp_grid_height '// &
     &                        'output_grid_height acp_output_dir st_output_dir rh_output_dir'
           CALL EXIT ( 1 )
         ELSE 
           CALL GETARG ( 1, DIR_HEB  )
           CALL GETARG ( 2, STR      )
           IF ( INDEX ( STR, '.' ) < 1 ) STR = STR(1:I_LEN(STR))//'.'
           READ ( UNIT=STR, FMT='(F10.2)', IOSTAT=IUER ) HEI_ABOVE_SUR
           IF ( IUER .NE. 0 ) THEN
                IUER  = -1
                CALL ERR_LOG ( 6701, IUER, 'GEN_ACP', 'Wrong the 2nd argument: '// &
     &              'height above the surface should be a real number' )
                CALL EXIT ( 1 )
           END IF
           CALL GETARG ( 3, OBS_DATE )
           CALL GETARG ( 4, FIL_IGH  )
           CALL GETARG ( 5, FIL_OGH  )
           CALL GETARG ( 6, DIR_ACP_OUT  )
           CALL GETARG ( 7, DIR_ST_OUT  )
           CALL GETARG ( 8, DIR_RH_OUT  )
      END IF
!
! --- Check whether the output directory with atmospheric 
! --- chemical potential exists
!
      DIR_DESC = OPENDIR ( DIR_ACP_OUT(1:I_LEN(DIR_ACP_OUT))//CHAR(0) )
      IF ( DIR_DESC > 0 ) THEN 
           DIR_DESC = CLOSEDIR ( %VAL(DIR_DESC) )
         ELSE
           IUER = -1
           CALL ERR_LOG ( 6702, IUER, 'GEN_ACP', 'Wrong 6th argument: '// &
     &         'output directory '//DIR_ACP_OUT(1:I_LEN(DIR_ACP_OUT))// &
     &         ' does not exist' )
           CALL EXIT ( 1 )
      END IF
!
! --- Check whether the output directory with dew point
! --- temperature exists
!
      DIR_DESC = OPENDIR ( DIR_ST_OUT(1:I_LEN(DIR_ST_OUT))//CHAR(0) )
      IF ( DIR_DESC > 0 ) THEN 
           DIR_DESC = CLOSEDIR ( %VAL(DIR_DESC) )
         ELSE
           IUER = -1
           CALL ERR_LOG ( 6703, IUER, 'GEN_ACP', 'Wrong 7th argument: '// &
     &         'output directory '//DIR_ST_OUT(1:I_LEN(DIR_ST_OUT))// &
     &         ' does not exist' )
           CALL EXIT ( 1 )
      END IF
!
! --- Check whether the output directory with dew point
! --- temperature exists
!
      DIR_DESC = OPENDIR ( DIR_RH_OUT(1:I_LEN(DIR_RH_OUT))//CHAR(0) )
      IF ( DIR_DESC > 0 ) THEN 
           DIR_DESC = CLOSEDIR ( %VAL(DIR_DESC) )
         ELSE
           IUER = -1
           CALL ERR_LOG ( 6703, IUER, 'GEN_ACP', 'Wrong 8th argument: '// &
     &         'output directory '//DIR_RH_OUT(1:I_LEN(DIR_RH_OUT))// &
     &         ' does not exist' )
           CALL EXIT ( 1 )
      END IF
!
! --- Read input HEB files with pressure thickness, specific humidity,
! --- and air temperature
!
      IUER = -1
      CALL READ_DQT_HEB ( DIR_HEB, OBS_DATE, HEB_DELP, HEB_Q, HEB_T, IUER )
      IF ( IUER .NE. 0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 6704, IUER, 'GEN_ACP', 'Error in an attempt '// &
     &                   'to read numerical weather model for date '//OBS_DATE )
           CALL EXIT ( 1 )
      END IF
!
      IUER = -1
      CALL MALO_CHECK_SHARE_FILE ( FIL_IGH, IUER )
      IF ( IUER  .NE. 0 ) THEN
           IUER = -1
           CALL GERROR  ( STR )
           CALL ERR_LOG ( 6705, IUER, 'GEN_ACP', 'Cannot find input grid '// &
     &         'height file '//FIL_IGH )
           CALL EXIT ( 1 )
      END IF
!
! --- Read the data file with original surface geopotential used 
! --- by the numerical weather model
!
      IUER = -1
      CALL READ_HEB_HEADER ( FIL_IGH, HEB_G, IUER )
      IF ( IUER  .NE. 0 ) THEN
           IUER = -1
           CALL GERROR  ( STR )
           CALL ERR_LOG ( 6706, IUER, 'GEN_ACP', 'Error in reading '// &
     &         'heb-file '//FIL_IGH )
           CALL EXIT ( 1 )
      END IF
!
      IUER = -1
      CALL READ_HEB_DATA ( FIL_IGH, HEB_G, IUER )
      IF ( IUER  .NE. 0 ) THEN
           CALL GERROR  ( STR )
           CALL ERR_LOG ( 6707, IUER, 'GEN_ACP', 'Error in reading '// &
     &         'heb-file '//FIL_IGH )
           CALL EXIT ( 1 )
      END IF
!
! --- Read the data file with surface elevation data from G3TOPO
! --- model for the output grid
!
      IUER = -1
      CALL MALO_CHECK_SHARE_FILE ( FIL_OGH, IUER )
      IF ( IUER  .NE. 0 ) THEN
           IUER = -1
           CALL GERROR  ( STR )
           CALL ERR_LOG ( 6708, IUER, 'GEN_ACP', 'Cannot find output grid '// &
     &         'height file '//FIL_OGH )
           CALL EXIT ( 1 )
      END IF
      IUER = -1
      CALL READ_HEB_HEADER ( FIL_OGH, HEB_OGH, IUER )
      IF ( IUER  .NE. 0 ) THEN
           IUER = -1
           CALL GERROR  ( STR )
           CALL ERR_LOG ( 6709, IUER, 'GEN_ACP', 'Error in reading '// &
     &         'heb-file '//FIL_OGH )
           CALL EXIT ( 1 )
      END IF
!
      IUER = -1
      CALL READ_HEB_DATA ( FIL_OGH, HEB_OGH, IUER )
      IF ( IUER  .NE. 0 ) THEN
           IUER = -1
           CALL GERROR  ( STR )
           CALL ERR_LOG ( 6710, IUER, 'GEN_ACP', 'Error in reading '// &
     &         'heb-file '//FIL_OGH )
           CALL EXIT ( 1 )
      END IF
!
! --- Compute the 2D grid of the atmospheric chemical potential
! --- and air temperature at the specified height above the 
! --- surface
!
      IUER = -1
      CALL MALO_COMP_ACP ( MALO__ACP_GRID, HEI_ABOVE_SUR, HEB_DELP, HEB_T, HEB_Q, &
     &                     HEB_G, HEB_OGH, HEB_ACP, HEB_ST, HEB_RH, COO_HLP, &
     &                     ACP_VAL, IUER )
      IF ( IUER .NE. 0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 6711, IUER, 'GEN_ACP', 'Error in an attempt '// &
     &         'to compute surface atmospheric pressure' )
           CALL EXIT ( 1 )
      END IF
!
! --- Generate name of the output file
!
      FIL_OUT = DIR_ACP_OUT(1:I_LEN(DIR_ACP_OUT))//'/acp_'// &
     &          OBS_DATE(1:I_LEN(OBS_DATE))//'.heb'
      ID = LINDEX ( FIL_OUT, '/' )
!
! --- Heb header parameters
!
      IF ( HEB_DELP%TITLE(1:5) == 'MERRA' ) THEN
           CAL_DATE = OBS_DATE(1:4)//'.'//OBS_DATE(5:6)//'.'//OBS_DATE(7:11)//':'// &
     &                OBS_DATE(12:13)//'_00.0'
           IUER = -1
           CALL DATE_TO_TIME ( CAL_DATE, HEB_ACP%MJD, HEB_ACP%UTC, IUER )
           IF ( IUER .NE. 0 ) THEN
                IUER = -1
                CALL ERR_LOG ( 6712, IUER, 'GEN_ACP', 'Wrong observation date '// &
     &               OBS_DATE )
                CALL EXIT ( 1 )
           END IF
           HEB_ACP%TAI  = HEB_ACP%UTC
      END IF
!
! --- Fill metadata
!
      HEB_ACP%DIMS = HEB_OGH%DIMS
      HEB_ACP%DATA_OFFSET = HEB__HDS
      HEB_ACP%ENDIAN      = HEB__LE
      HEB_ACP%DATA_TRANSFORM = HEB__NONE
      HEB_ACP%FILL_VALUE     = 1.0E15
      HEB_ACP%OFFSET         = 0.0
      HEB_ACP%SCALE_FACTOR   = 1.0
      HEB_ACP%DATA_COMPRESSION = HEB__NONE
      CALL CLRCH ( STR )
      WRITE ( UNIT=STR(1:8), FMT='(F8.1)' ) HEI_ABOVE_SUR 
      CALL CHASHL ( STR(1:8) )
      HEB_ACP%SDS_NAME       = 'Atmosphere chemical potential at height '// &
     &                          STR(1:I_LEN(STR))//' m '
      HEB_ACP%UNITS          = 'ev'
      HEB_ACP%DATA_FORMAT    = HEB__R4
      HEB_ACP%MIN_VALUE      = MINVAL ( HEB_ACP%VAL )
      HEB_ACP%MAX_VALUE      = MAXVAL ( HEB_ACP%VAL )
      HEB_ACP%VALID_RANGE(1) = 0.0
      HEB_ACP%VALID_RANGE(2) = 1.0
      HEB_ACP%PROD_DATE_TIME = GET_CDATE()
!
      HEB_ACP%FILE_NAME      = FIL_OUT(ID+1:)
      HEB_ACP%HISTORY        = 'Processed using input numerical weather '// &
     &                         'model and input digital elevation map'
      HEB_ACP%SOURCE         = '1) '//HEB_DELP%SOURCE(1:I_LEN(HEB_DELP%SOURCE))// &
     &                         '; 2) '//HEB_OGH%SOURCE(1:I_LEN(HEB_OGH%SOURCE))// &
     &                         ' '//HEB_OGH%PROD_NAME 
      HEB_ACP%TITLE          = 'Atmosphere chemical potential'
      HEB_ACP%PROD_NAME      = HEB_ACP%SDS_NAME       
      HEB_ACP%INSTITUTION    = 'Astrogeo Center'
      HEB_ACP%REFERENCES     = 'http://astrogeo.org/malo/'
      HEB_ACP%VERSION_ID     = GEN_ACP__LABEL 
!
! --- Write atmospheric chemical potential in the output file
!
      IUER = -1
      CALL WRITE_HEB ( HEB_ACP, HEB_ACP%VAL, FIL_OUT, IUER )
      IF ( IUER .NE. 0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 6713, IUER, 'GEN_ACP', 'Error in an attempt '// &
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
                IUER = -1
                CALL ERR_LOG ( 6714, IUER, 'GEN_ACP', 'Wrong observation date '// &
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
      WRITE ( UNIT=STR(1:8), FMT='(F8.1)' ) HEI_ABOVE_SUR 
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
      HEB_ST%VERSION_ID     = GEN_ACP__LABEL 
      IUER = -1
      CALL WRITE_HEB ( HEB_ST, HEB_ST%VAL, FIL_OUT, IUER )
      IF ( IUER .NE. 0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 6715, IUER, 'GEN_ACP', 'Error in an attempt '// &
     &         'to write surface air temperature into the output '// &
     &         'file '//FIL_OUT )
           CALL EXIT ( 1 )
      END IF
!
      FIL_OUT = DIR_RH_OUT(1:I_LEN(DIR_RH_OUT))//'/rh_'// &
     &          OBS_DATE(1:I_LEN(OBS_DATE))//'.heb'
!
      HEB_RH%DIMS = HEB_OGH%DIMS
      HEB_RH%DATA_OFFSET = HEB__HDS
      HEB_RH%ENDIAN      = HEB__LE
      HEB_RH%DATA_TRANSFORM = HEB__NONE
      HEB_RH%FILL_VALUE     = 1.0E15
      HEB_RH%OFFSET         = 0.0
      HEB_RH%SCALE_FACTOR   = 1.0
      HEB_RH%DATA_COMPRESSION = HEB__NONE
      CALL CLRCH ( STR )
      WRITE ( UNIT=STR(1:8), FMT='(F8.1)' ) HEI_ABOVE_SUR 
      CALL CHASHL ( STR(1:8) )
      HEB_RH%SDS_NAME       = 'relative humidity at '//STR(1:I_LEN(STR))// &
     &                          ' m above surface' 
      HEB_RH%UNITS          = 'dimensionless'
      HEB_RH%DATA_FORMAT    = HEB__R4
      HEB_RH%MIN_VALUE      = MINVAL ( HEB_RH%VAL )
      HEB_RH%MAX_VALUE      = MAXVAL ( HEB_RH%VAL )
      HEB_RH%VALID_RANGE(1) = 0.0
      HEB_RH%VALID_RANGE(2) = 1.0
      HEB_RH%PROD_DATE_TIME = GET_CDATE()
!
      HEB_RH%FILE_NAME      = FIL_OUT(ID+1:)
      HEB_RH%HISTORY        = 'Processed using input numerical weather '// &
     &                         'model and input digital elevation map'
      HEB_RH%SOURCE         = '1) '//HEB_DELP%SOURCE(1:I_LEN(HEB_DELP%SOURCE))// &
     &                         '; 2) '//HEB_OGH%SOURCE(1:I_LEN(HEB_OGH%SOURCE))// &
     &                         ' '//HEB_OGH%PROD_NAME 
      HEB_RH%TITLE          = 'Surface relative humidity'
      HEB_RH%PROD_NAME      = HEB_RH%SDS_NAME       
      HEB_RH%INSTITUTION    = 'Astrogeo Center'
      HEB_RH%REFERENCES     = 'http://astrogeo.org/malo/'
      HEB_RH%VERSION_ID     = GEN_ACP__LABEL 
!
      IUER = -1
      CALL WRITE_HEB ( HEB_RH, HEB_RH%VAL, FIL_OUT, IUER )
      IF ( IUER .NE. 0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 6715, IUER, 'GEN_ACP', 'Error in an attempt '// &
     &         'to write surface relatuve humidity into the output '// &
     &         'file '//FIL_OUT )
           CALL EXIT ( 1 )
      END IF
!
      END  PROGRAM  GEN_ACP  !#!  
