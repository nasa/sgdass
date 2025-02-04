      PROGRAM    GEN_SPR_LAUNCH
      IMPLICIT   NONE 
      INCLUDE   'malo.i'
      CHARACTER    STR*32
      INTEGER*8    STACK_SIZE_IN_BYTES, GB, IS
      PARAMETER  ( GB = 1024*1024*1024 )
      PARAMETER  ( STACK_SIZE_IN_BYTES = MALO__STACK_SIZE_IN_GIGABYTES * GB )
      INTEGER*8, EXTERNAL :: SET_STACKSIZE 
!
! --- Set stacksize
!
      IS = SET_STACKSIZE ( %VAL(STACK_SIZE_IN_BYTES) )
      CALL INCH8    ( STACK_SIZE_IN_BYTES/INT8(1024), STR )
      CALL SETENV   ( 'GOMP_STACKSIZE'//CHAR(0), TRIM(STR)//CHAR(0), %VAL(1) )
      CALL GEN_SPR()
      END  PROGRAM  GEN_SPR_LAUNCH  !#!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE GEN_SPR()
! ************************************************************************
! *                                                                      *
! *   Routine GEN_SPR generates surface atmospheric pressure using       *
! *   1) 3D input field of atmosphere layer thickness;                   *
! *   2) 3D input field of air temperature;                              *
! *   3) 3D input field of specific humidity;                            *
! *   4) 2D input grid of the heights of grid point above                *
! *      the WGS84 reference ellipsoid;                                  *
! *   5) 2D output grid of the surface height avereged over the pixel    *
! *      area.                                                           *
! *                                                                      *
! *   Usage: gen_spr heb-dir date inp_grid_height deg output_grid_height *
! *          output_spr                                                  *
! *                                                                      *
! *   The results is written in heb-format.                              *
! *                                                                      *
! *  ### 21-FEB-2013     GEN_SPR   v1.4  (c) L. Petrov  22-MAY-2017 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'malo.i'
      INCLUDE   'heb.i'
      TYPE     ( MALO__TYPE ) :: MAL
      TYPE     ( HEB__TYPE  ) :: HEB_DELP, HEB_T, HEB_Q, HEB_G, HEB_H, HEB_W, &
     &                           HEB_OGH, HEB_SPR
      CHARACTER  DIR_HEB*128, OBS_DATE*128, FIL_IGH*128, FIL_OGH*128, &
     &           OUT_PREF*128, FIL_OUT*128, STR*128, CAL_DATE*19, DIR_OUT*128, &
     &           FINAM_DELP*128, FINAM_W*128, DATA_SRC*128, GEN_SPR__LABEL*32
      CHARACTER  COMPR*16, COMPR_COM*64, COM_STR*256
      PARAMETER  ( GEN_SPR__LABEL = 'GEN_SPR  Vers  1.3 of 2014.08.17' )
      INTEGER*4    L_LEV_FNL, NN
      PARAMETER  ( L_LEV_FNL = 26 )
      REAL*8     PRES_LEV_FNL(L_LEV_FNL)
      DATA     ( PRES_LEV_FNL(NN), NN=1,L_LEV_FNL ) &
     &         / &
     &            100000.0D0, & !   1 
     &             97500.0D0, & !   2 
     &             95000.0D0, & !   3 
     &             92500.0D0, & !   4 
     &             90000.0D0, & !   5 
     &             85000.0D0, & !   6 
     &             80000.0D0, & !   7 
     &             75000.0D0, & !   8 
     &             70000.0D0, & !   9 
     &             65000.0D0, & !  10 
     &             60000.0D0, & !  11 
     &             55000.0D0, & !  12 
     &             50000.0D0, & !  13 
     &             45000.0D0, & !  14 
     &             40000.0D0, & !  15 
     &             35000.0D0, & !  16 
     &             30000.0D0, & !  17 
     &             25000.0D0, & !  18 
     &             20000.0D0, & !  19 
     &             15000.0D0, & !  20 
     &             10000.0D0, & !  21 
     &              7000.0D0, & !  22 
     &              5000.0D0, & !  23 
     &              3000.0D0, & !  24 
     &              2000.0D0, & !  25 
     &              1000.0D0  & !  26 
     &         /
      INTEGER*8  DIR_DESC
      INTEGER*4  ID, IL, DEG, IUER
      LOGICAL*1  EX_DELP, EX_W
      CHARACTER, EXTERNAL :: GET_CDATE*19
      INTEGER*4, EXTERNAL :: I_LEN, ILEN
      INTEGER*8, EXTERNAL :: OPENDIR, CLOSEDIR, LINDEX
!
      IF ( IARGC() < 5 ) THEN
           WRITE ( 6, '(A)' ) 'Usage: gen_spr heb-dir date inp_grid_height '// &
     &                        'output_grid_height output_pref [compr]'
           CALL EXIT ( 1 )
         ELSE 
           CALL GETARG ( 1, DIR_HEB  )
           CALL GETARG ( 2, OBS_DATE )
           CALL GETARG ( 3, FIL_IGH  )
           CALL GETARG ( 4, FIL_OGH  )
           CALL GETARG ( 5, OUT_PREF )
           IF ( IARGC() .GE. 6 ) THEN
                CALL GETARG ( 6, COMPR )
              ELSE 
                CALL CLRCH ( COMPR )
           END IF
      END IF
!
      ID = LINDEX ( OUT_PREF, '/' )
      DIR_OUT = OUT_PREF(1:ID)
      DIR_DESC = OPENDIR ( DIR_OUT(1:I_LEN(DIR_OUT))//CHAR(0) )
      IF ( DIR_DESC > 0 ) THEN 
           DIR_DESC = CLOSEDIR ( %VAL(DIR_DESC) )
         ELSE
           CALL ERR_LOG ( 6701, IUER, 'GEN_SPR', 'Wrong 5th argument: '// &
     &         'output directory '//DIR_OUT(1:I_LEN(DIR_OUT))// &
     &         ' does not exist' )
           CALL EXIT ( 1 )
      END IF
!
      FINAM_DELP = DIR_HEB(1:I_LEN(DIR_HEB))//'/'//OBS_DATE(1:4)// &
     &            '/d/d_'//OBS_DATE(1:I_LEN(OBS_DATE))//'.heb.bz2'
      FINAM_W    = DIR_HEB(1:I_LEN(DIR_HEB))//'/'//OBS_DATE(1:4)// &
     &            '/w/w_'//OBS_DATE(1:I_LEN(OBS_DATE))//'.heb.bz2'
      INQUIRE ( FILE=FINAM_DELP, EXIST=EX_DELP )
      IF ( .NOT. EX_DELP ) THEN
           IL = ILEN(FINAM_DELP)
           CALL CLRCH ( FINAM_DELP(IL-3:) )
           INQUIRE ( FILE=FINAM_DELP, EXIST=EX_DELP )
      END IF
      INQUIRE ( FILE=FINAM_W,    EXIST=EX_W    )
      IF ( .NOT. EX_W ) THEN
           IL = ILEN(FINAM_W)
           CALL CLRCH ( FINAM_W(IL-3:) )
           INQUIRE ( FILE=FINAM_W, EXIST=EX_W )
      END IF
      IF ( EX_DELP .AND. .NOT. EX_W ) THEN
           IUER = -1
           CALL READ_DQT_HEB ( DIR_HEB, OBS_DATE, HEB_DELP, HEB_Q, HEB_T, IUER )
           IF ( IUER .NE. 0 ) THEN
                CALL ERR_LOG ( 6702, IUER, 'GEN_SPR', 'Error in an attempt '// &
     &                        'to read numerical weather model for date '//OBS_DATE )
                CALL EXIT ( 1 )
           END IF
           DATA_SRC = HEB_DELP%SOURCE
         ELSE IF ( .NOT. EX_DELP .AND. EX_W ) THEN
           IUER = -1
           CALL READ_HTW_HEB ( DIR_HEB, OBS_DATE, HEB_H, HEB_T, HEB_W, IUER )
           IF ( IUER .NE. 0 ) THEN
                CALL ERR_LOG ( 6703, IUER, 'GEN_SPR', 'Error in an attempt '// &
     &                        'to read numerical weather model for date '//OBS_DATE )
                CALL EXIT ( 1 )
           END IF
           DATA_SRC = HEB_W%SOURCE
         ELSE 
           IUER = -1
           CALL ERR_LOG ( 6704, IUER, 'GEN_SPR', 'Did not find neither delp file '// &
     &                    FINAM_DELP(1:I_LEN(FINAM_DELP))//' nor w file '// &
     &                    FINAM_W(1:I_LEN(FINAM_W)) )
           CALL EXIT ( 1 )
      END IF
!
! --- Check compression method
!
      IF ( ILEN(COMPR) == 0 ) THEN
           CONTINUE 
         ELSE IF ( COMPR == 'none' ) THEN
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
         ELSE 
           CALL ERR_LOG ( 6705, -2, 'GEN_SPR', 'Unsupported '// &
     &         'compression method: '//COMPR//' . Supported methods: '// &
     &         ' none gzip bzip2 pbzip2 pbzip2_p1 lbzip lbzip2_p1 '// &
     &         'lbzip2 lbzip2_1 lbzip2_1p1 lbzip2_2p1' ) 
           CALL EXIT ( 1 )
      END IF
!
      IUER = -1
      CALL MALO_CHECK_SHARE_FILE ( FIL_IGH, IUER )
      IF ( IUER .NE. 0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 6706, IUER, 'GEN_SPR', 'Error in an attempt '// &
     &        'to find the file with geoid undulations '//FIL_IGH )
           CALL EXIT ( 1 )
      END IF
!
      IUER = -1
      CALL MALO_CHECK_SHARE_FILE ( FIL_OGH, IUER )
      IF ( IUER .NE. 0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 6707, IUER, 'GEN_SPR', 'Error in an attempt '// &
     &        'to find the file with difital elevations above geoid '//FIL_OGH )
           CALL EXIT ( 1 )
      END IF
!
      IUER = -1
      CALL READ_HEB ( FIL_IGH, HEB_G, IUER )
      IF ( IUER  .NE. 0 ) THEN
           CALL GERROR  ( STR )
           CALL ERR_LOG ( 6708, IUER, 'GEN_SPR', 'Error in reading '// &
     &         'heb-file '//FIL_IGH )
           CALL EXIT ( 1 )
      END IF
!
      IUER = -1
      CALL READ_HEB ( FIL_OGH, HEB_OGH, IUER )
      IF ( IUER  .NE. 0 ) THEN
           CALL GERROR  ( STR )
           CALL ERR_LOG ( 6709, IUER, 'GEN_SPR', 'Error in reading '// &
     &         'heb-file '//FIL_OGH )
           CALL EXIT ( 1 )
      END IF
!
      IF ( EX_DELP .AND. .NOT. EX_W ) THEN
           IUER = -1
           CALL MALO_COMP_SPR ( HEB_DELP, HEB_T, HEB_Q, HEB_G, HEB_OGH, MAL, IUER )
           IF ( IUER .NE. 0 ) THEN
                CALL ERR_LOG ( 6710, IUER, 'GEN_SPR', 'Error in an attempt '// &
     &              'to compute surface atmospheric pressure' )
                CALL EXIT ( 1 )
           END IF
         ELSE IF ( .NOT. EX_DELP .AND. EX_W ) THEN
           IUER = -1
           CALL MALO_INTRP_SPR ( HEB_H, HEB_T, HEB_G, HEB_OGH, L_LEV_FNL, &
     &                           PRES_LEV_FNL, MAL, IUER )
           IF ( IUER .NE. 0 ) THEN
                CALL ERR_LOG ( 6711, IUER, 'GEN_SPR', 'Error in an attempt '// &
     &                        'to read numerical weather model for date '//OBS_DATE )
                CALL EXIT ( 1 )
           END IF
      END IF
!
      FIL_OUT = OUT_PREF(1:I_LEN(OUT_PREF))//OBS_DATE(1:I_LEN(OBS_DATE))//'.heb'
      ID = LINDEX ( FIL_OUT, '/' )
!
! --- Heb header parameters
!
      IF ( HEB_DELP%TITLE(1:5) == 'MERRA' ) THEN
           CAL_DATE = OBS_DATE(1:4)//'.'//OBS_DATE(5:6)//'.'//OBS_DATE(7:11)//':'// &
     &                OBS_DATE(12:13)//'_00.0'
           IUER = -1
           CALL DATE_TO_TIME ( CAL_DATE, HEB_SPR%MJD, HEB_SPR%UTC, IUER )
           IF ( IUER .NE. 0 ) THEN
                CALL ERR_LOG ( 6712, IUER, 'GEN_SPR', 'Wrong observation date '// &
     &               OBS_DATE )
                CALL EXIT ( 1 )
            END IF
           HEB_SPR%TAI  = HEB_SPR%UTC
         ELSE 
           IF ( EX_DELP .AND. .NOT. EX_W ) THEN
                HEB_SPR%MJD  = HEB_DELP%MJD
                HEB_SPR%UTC  = HEB_DELP%UTC
                HEB_SPR%TAI  = HEB_DELP%TAI
              ELSE 
                HEB_SPR%MJD  = HEB_H%MJD
                HEB_SPR%UTC  = HEB_H%UTC
                HEB_SPR%TAI  = HEB_H%TAI
           END IF
      END IF
!
      HEB_SPR%DIMS = HEB_OGH%DIMS
      HEB_SPR%DATA_OFFSET = HEB__HDS
      HEB_SPR%ENDIAN      = HEB__LE
      HEB_SPR%DATA_TRANSFORM = HEB__NONE
      HEB_SPR%FILL_VALUE     = 1.0E15
      HEB_SPR%OFFSET         = 0.0
      HEB_SPR%SCALE_FACTOR   = 1.0
      HEB_SPR%DATA_COMPRESSION = HEB__NONE
      HEB_SPR%SDS_NAME       = 'Surface atmospheric pressure'
      HEB_SPR%UNITS          = 'Pa'
      HEB_SPR%DATA_FORMAT    = HEB__R4
      HEB_SPR%MIN_VALUE      = MINVAL(MAL%SPR)
      HEB_SPR%MAX_VALUE      = MAXVAL(MAL%SPR)
      HEB_SPR%VALID_RANGE(1) =  40000.0
      HEB_SPR%VALID_RANGE(2) = 110000.0
      HEB_SPR%PROD_DATE_TIME = GET_CDATE()
!
      HEB_SPR%FILE_NAME      = FIL_OUT(ID+1:)
      HEB_SPR%HISTORY        = 'Processed using input numerical weather '// &
     &                         'model and input digital elevation map'
      HEB_SPR%SOURCE         = '1) '//DATA_SRC(1:I_LEN(DATA_SRC))// &
     &                         '; 2) '//HEB_OGH%SOURCE(1:I_LEN(HEB_OGH%SOURCE))// &
     &                         ' '//HEB_OGH%PROD_NAME 
      HEB_SPR%TITLE          = 'Surface atmospheric pressure'
      HEB_SPR%PROD_NAME      = 'Atmospheric pressure from the numerical weather model at the surface defined by the digital elevation model'
      HEB_SPR%INSTITUTION    = 'Astrogeo Center'
      HEB_SPR%REFERENCES     = 'http://astrogeo.org/malo/'
      HEB_SPR%VERSION_ID     = GEN_SPR__LABEL 
!
      IUER = -1
      CALL WRITE_HEB ( HEB_SPR, MAL%SPR, FIL_OUT, IUER )
      IF ( IUER .NE. 0 ) THEN
           CALL ERR_LOG ( 6713, IUER, 'GEN_SPR', 'Error in an attempt '// &
     &         'to write surface atmospheric pressure into the output '// &
     &         'file '//FIL_OUT )
           CALL EXIT ( 1 )
      END IF
!
      IF ( ILEN(COMPR_COM) .NE. 0 ) THEN
!
! -------- Now compress the output file 
!
           COM_STR = COMPR_COM(1:I_LEN(COMPR_COM))//' '// &
     &               FIL_OUT(1:I_LEN(FIL_OUT))
           CALL SYSTEM ( COM_STR(1:I_LEN(COM_STR))//CHAR(0) )
      END IF
!
      END  SUBROUTINE  GEN_SPR  !#!#
