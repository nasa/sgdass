      PROGRAM  GEOSFPIT_TO_O3
! ************************************************************************
! *                                                                      *
! *   Program GEOSFPIT_TO_O3 reads a given file with results of          *
! *   assimilation run of GEOS-FPIT model at native resolution, extacts  *
! *   o3 dataset with ozone mixin ratio, encodes it in HEB format and    *
! *   writes in it in the the output subdirectory /o .                   *
! *                                                                      *
! *   Compression can be specified optinoally.                           *
! *                                                                      *
! *   Ussage: geosfit_to_o3 input_file output_directory [compr]          *
! *                                                                      *
! * ### 26-MAR-2018  GEOSFPIT_TO_O3  v1.0  (c) L. Petrov 26-MAR-2018 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'heb.i'
      TYPE     ( HEB__TYPE ) :: HEB
      INTEGER*4    MLON, MLAT, MLEV, MP
      PARAMETER  ( MLON =  576 )
      PARAMETER  ( MLAT =  361 )
      PARAMETER  ( MLEV =   72 )
      PARAMETER  (   MP =    1 )
      REAL*4,    ALLOCATABLE :: O3(:,:,:)
      LOGICAL*1  LEX, FL_WRITE_PHIS
      INTEGER*8  DIR_DESC, IP, IS
      INTEGER*2  MODE_I2
      INTEGER*2  MASK_I2 
      DATA       MASK_I2  / O'0777' /
      INTEGER*4  J1, J2, J3, IUER
      CHARACTER  FILIN*128, FILOUT*128, DIROUT*128, DIR*128, STR*128, &
     &           COMPR*16, COMPR_COM*64, COM_STR*256, SUBDIR(MP)*1
      DATA       SUBDIR /   &
     &                 'o'  &
     &           /
      INTEGER*8, EXTERNAL :: OPENDIR, CLOSEDIR 
      INTEGER*4, EXTERNAL :: MKDIR, I_LEN, ILEN
      CHARACTER, EXTERNAL :: MJDSEC_TO_DATE*30
!
      FL_WRITE_PHIS = .FALSE.
      COMPR = 'none'
      IF ( IARGC() < 2 ) THEN
           WRITE ( 6, '(A)' ) 'Usage geosfpit_to_o3 filin output_directory [compr]'
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
         ELSE 
           CALL ERR_LOG ( 7301, -2, 'GEOSFPIT_TO_O3', 'Unsupported '// &
     &         'compression method: '//COMPR//' . Supported methods: '// &
     &         ' none gzip bzip2 pbzip2 pbzip2_p1 lbzip lbzip2_p1 '// &
     &         'lbzip2 lbzip2_1 lbzip2_1p1 lbzip2_2p1' ) 
           CALL EXIT ( 1 )
      END IF
!
! --- Allocate memory for the array with the dataset
!
      ALLOCATE ( O3(MLON,MLAT,MLEV)   )
!
      INQUIRE ( FILE=FILIN, EXIST=LEX )
      IF ( .NOT. LEX ) THEN
           CALL ERR_LOG ( 7302, -2, 'GEOSFPIT_TO_O3', 'Cannot find '// &
     &         'input file '//FILIN )
           CALL EXIT ( 1 )
      END IF
!
! --- Check whether the output directory exists
!
      DIR_DESC = OPENDIR ( DIROUT(1:I_LEN(DIROUT))//CHAR(0) ) 
      IF ( DIR_DESC .LE. 0 ) THEN
           CALL GERROR ( STR )
           CALL ERR_LOG ( 7202, -2, 'GEOSFPIT_TO_O3', 'Output '// &
     &         'directory '//DIROUT(1:I_LEN(DIROUT))//' does not exist' )
           CALL EXIT ( 1 )
         ELSE 
           IP = CLOSEDIR ( %VAL(DIR_DESC) )
      END IF
!
! --- Check whether the output subdirectory exist
!
      DO 410 J1=1,MP
         DIR = DIROUT(1:I_LEN(DIROUT))//'/'//SUBDIR(J1)
         DIR_DESC = OPENDIR ( DIR(1:I_LEN(DIR))//CHAR(0) ) 
         IF ( DIR_DESC .LE. 0 ) THEN
              MODE_I2 = MASK_I2
              IS = MKDIR ( DIR(1:I_LEN(DIR))//CHAR(0), %VAL(MODE_I2) )
              IF ( IS .EQ. -1 ) THEN
                   CALL GERROR ( STR )
                   CALL ERR_LOG ( 7203, -2, 'GEOSFPIT_TO_O3', 'Failure '// &
     &                 'in an attempt to create output directory '//DIR )
                   CALL EXIT ( 1 )
              END IF
            ELSE 
              IP = CLOSEDIR ( %VAL(DIR_DESC) )
         END IF
 410  CONTINUE 
!
! --- Parse the input file
!
      IUER = -1
      CALL READ_GEOS_O3 ( FILIN, MLON, MLAT, MLEV, O3, HEB, IUER )
      IF ( IUER .NE. 0 ) THEN
           CALL GERROR ( STR )
           CALL ERR_LOG ( 7204, -2, 'GEOSFPIT_TO_O3', 'Failure '// &
     &         'in an attempt to parse the input file '//FILIN )
           CALL EXIT ( 1 )
      END IF
!
! --- Generic parameters
!
      HEB%TAI = HEB%UTC
      HEB%DIMS(1) = MLON
      HEB%DIMS(2) = MLAT
      HEB%DIMS(3) = MLEV
      HEB%DIMS(4) =    1
      HEB%DATA_OFFSET    = HEB__HDS
      HEB%ENDIAN         = HEB__LE
      HEB%DATA_TRANSFORM = HEB__SCOF
      HEB%OFFSET         = 0.0
      HEB%SCALE_FACTOR   = 1.0
      HEB%FILL_VALUE     = 1.0E15
      HEB%DATA_COMPRESSION = HEB__NONE
!
      IUER = -1
      STR = MJDSEC_TO_DATE ( HEB%MJD, HEB%UTC, IUER )
      IF ( IUER .NE. 0 ) THEN
           CALL GERROR ( STR )
           CALL ERR_LOG ( 7205, -2, 'GEOSFPIT_TO_O3', 'Failure '// &
     &         'in an attempt to parse the input file '//FILIN )
           CALL EXIT ( 1 )
      END IF
!
! --- Now update O3 mixing ratio data
!
      FILOUT = DIROUT(1:I_LEN(DIROUT))//'/'//SUBDIR(1)//'/'//SUBDIR(1)//'_'// &
     &         STR(1:4)//STR(6:7)//STR(9:10)//'_'//STR(12:13)// &
     &         STR(15:16)//'.heb'
      HEB%SDS_NAME       = 'o3 misxing ratio'
      HEB%UNITS          = 'dimensionless'
      HEB%DATA_FORMAT    = HEB__I2
      HEB%DATA_TRANSFORM = HEB__LOG
      HEB%VALID_RANGE(1) = EXP(-34.0)
      HEB%VALID_RANGE(2) = EXP(-2.0)
      HEB%MIN_VALUE      = MAX ( MINVAL(O3), HEB%VALID_RANGE(1) )
      HEB%MAX_VALUE      = MIN ( MAXVAL(O3), HEB%VALID_RANGE(2) )
      HEB%OFFSET         = -18.0
      HEB%SCALE_FACTOR   = 32.0/64000.0
      HEB%DIMS(3)        = MLEV
!
      IUER = -1
      CALL WRITE_HEB ( HEB, O3, FILOUT, IUER )
      IF ( IUER .NE. 0 ) THEN
           CALL GERROR ( STR )
           CALL ERR_LOG ( 7205, -2, 'GEOSFPIT_TO_O3', 'Failure '// &
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
      END IF
!
      END  PROGRAM  GEOSFPIT_TO_O3  !#!  
