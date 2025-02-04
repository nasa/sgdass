      PROGRAM    GEOS57NV_TO_DGQTUV_LAUNCH
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
      CALL GEOS57NV_TO_DGQTUV()
      END  PROGRAM  GEOS57NV_TO_DGQTUV_LAUNCH  !#!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE GEOS57NV_TO_DGQTUV()
! ************************************************************************
! *                                                                      *
! *   Program GEOS57NV_TO_DGQTUV reads a given file with results of      *
! *   assimilation run of GEOS-57 model at native resolution, extacts    *
! *   six datasets:                                                      *
! *   1) DELP (d) -- pressure_thickness                                  *
! *   2) PHIS (g) -- surface geopotential height                         *
! *   3) QV (q)   -- specific_humidity                                   *
! *   4) T (t)    -- air_temperature                                     *
! *   5) U (u)    -- eastward_wind                                       *
! *   6) V (v)    -- northward_wind                                      *
! *                                                                      *
! *   encodes them in HEB format and writes in the output                *
! *   subdirectories directory six files: one file per parameter.        *
! *   Six subdirectories under the output are specifed:                  *
! *   /d, /g, /q, /t, /u, /v                                             *
! *                                                                      *
! *   Compression can be specified optinoally.                           *
! *                                                                      *
! *   Ussage: geos57nv_to_dgqtuv input_file output_directory [compr]     *
! *                                                                      *
! * ## 27-JAN-2013  GEOS57NV_TO_DGQTUV v1.4 (c) L. Petrov 05-AUG-2015 ## *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'heb.i'
      TYPE     ( HEB__TYPE ) :: HEB
      INTEGER*4    MLON, MLAT, MLEV, MP
      PARAMETER  ( MLON = 1152 )
      PARAMETER  ( MLAT =  721 )
      PARAMETER  ( MLEV =   72 )
      PARAMETER  (   MP =    6 )
      REAL*4,    ALLOCATABLE :: DELP(:,:,:), T(:,:,:), QV(:,:,:), &
     &                          U(:,:,:),    V(:,:,:), PHIS(:,:)
      LOGICAL*1  LEX, FL_WRITE_PHIS
      INTEGER*8  DIR_DESC, IP, IS
      INTEGER*2  MODE_I2
      DATA       MODE_I2 / O'0775' /
      INTEGER*4  J1, J2, J3, IL, IUER
      CHARACTER  FILIN*128, FILOUT*128, DIROUT*128, DIR*128, STR*128, &
     &           COMPR*16, COMPR_COM*64, COM_STR*256, SUBDIR(MP)*1
      DATA       SUBDIR /   &
     &                 'd', &
     &                 'g', &
     &                 'q', &
     &                 't', &
     &                 'u', &
     &                 'v'  &
     &           /
      INTEGER*8, EXTERNAL :: OPENDIR, CLOSEDIR 
      INTEGER*4, EXTERNAL :: MKDIR, I_LEN, ILEN
      CHARACTER, EXTERNAL :: MJDSEC_TO_DATE*30
!
      FL_WRITE_PHIS = .FALSE.
      COMPR = 'none'
      IF ( IARGC() < 2 ) THEN
           WRITE ( 6, '(A)' ) 'Usage geos57nv_to_dgqtuv filin output_directory [compr]'
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
           CALL ERR_LOG ( 7301, -2, 'GEOS57NV_TO_DGQTUV', 'Unsupported '// &
     &         'compression method: '//COMPR//' . Supported methods: '// &
     &         ' none gzip' )
           CALL EXIT ( 1 )
      END IF
!
! --- Allocate memory for arrays with datasets
!
      ALLOCATE ( DELP(MLON,MLAT,MLEV) )
      ALLOCATE ( PHIS(MLON,MLAT)      )
      ALLOCATE ( T(MLON,MLAT,MLEV)    )
      ALLOCATE ( QV(MLON,MLAT,MLEV)   )
      ALLOCATE ( U(MLON,MLAT,MLEV)    )
      ALLOCATE ( V(MLON,MLAT,MLEV)    )
!
      INQUIRE ( FILE=FILIN, EXIST=LEX )
      IF ( .NOT. LEX ) THEN
           CALL ERR_LOG ( 7302, -2, 'GEOS57NV_TO_DGQTUV', 'Cannot find '// &
     &         'input file '//FILIN )
           CALL EXIT ( 1 )
      END IF
!
! --- Check whether the output directory exists
!
      DIR_DESC = OPENDIR ( DIROUT(1:I_LEN(DIROUT))//CHAR(0) ) 
      IF ( DIR_DESC .LE. 0 ) THEN
           CALL GERROR ( STR )
           CALL ERR_LOG ( 7202, -2, 'GEOS57NV_TO_DGQTUV', 'Output '// &
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
              IS = MKDIR ( DIR(1:I_LEN(DIR))//CHAR(0), %VAL(MODE_I2) )
              IF ( IS .EQ. -1 ) THEN
                   CALL GERROR ( STR )
                   CALL ERR_LOG ( 7203, -2, 'GEOS57NV_TO_DGQTUV', 'Failure '// &
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
      CALL READ_GEOS_DQGTUV ( FILIN, MLON, MLAT, MLEV, &
     &                        DELP, PHIS, QV, T, U, V, HEB, IUER )
      IF ( IUER .NE. 0 ) THEN
           CALL GERROR ( STR )
           CALL ERR_LOG ( 7204, -2, 'GEOS57NV_TO_DGQTUV', 'Failure '// &
     &         'in an attempt to parse the input file '//FILIN )
           CALL EXIT ( 1 )
      END IF
      IL = ILEN(FILIN)
!
! --- Generic parameters
!
      HEB%TAI = HEB%UTC
      HEB%DIMS(1) = MLON
      HEB%DIMS(2) = MLAT
      HEB%DIMS(3) = MLEV
      HEB%DIMS(4) =    1
      HEB%DATA_OFFSET = HEB__HDS
      HEB%ENDIAN      = HEB__LE
      HEB%DATA_TRANSFORM = HEB__SCOF
      HEB%FILL_VALUE     = 1.0E15
      HEB%OFFSET         = 0.0
      HEB%SCALE_FACTOR   = 1.0
      HEB%DATA_COMPRESSION = HEB__NONE
!
      IUER = -1
      STR = MJDSEC_TO_DATE ( HEB%MJD, HEB%UTC, IUER )
      IF ( IUER .NE. 0 ) THEN
           CALL GERROR ( STR )
           CALL ERR_LOG ( 7205, -2, 'GEOS57NV_TO_DGQTUV', 'Failure '// &
     &         'in an attempt to parse the input file '//FILIN )
           CALL EXIT ( 1 )
      END IF
!
! --- Update headers for "pressure thickness" variable
!
      IF ( ILEN(FILIN) > 38 ) THEN
           IF ( FILIN(IL-33:IL-33) == '.' .AND. FILIN(IL-21:IL-21) == '+' ) THEN
                FILOUT = DIROUT(1:I_LEN(DIROUT))//'/'//SUBDIR(1)//'/'// &
     &                   SUBDIR(1)//'_'//FILIN(IL-32:IL-8)//'.heb'
              ELSE 
                FILOUT = DIROUT(1:I_LEN(DIROUT))//'/'//SUBDIR(1)//'/'// &
     &                   SUBDIR(1)//'_'//STR(1:4)//STR(6:7)//STR(9:10)// &
     &                   '_'//STR(12:13)//STR(15:16)//'.heb'
           END IF
         ELSE
           FILOUT = DIROUT(1:I_LEN(DIROUT))//'/'//SUBDIR(1)//'/'// &
     &              SUBDIR(1)//'_'//STR(1:4)//STR(6:7)//STR(9:10)// &
     &              '_'//STR(12:13)//STR(15:16)//'.heb'
      END IF
!
      HEB%SDS_NAME       = 'pressure_thickness'
      HEB%UNITS          = 'Pa'
      HEB%DATA_FORMAT    = HEB__I2
      HEB%MIN_VALUE      = MINVAL(DELP)
      HEB%MAX_VALUE      = MAXVAL(DELP)
      HEB%VALID_RANGE(1) = 0.0
      HEB%VALID_RANGE(2) = 6400.0
      HEB%OFFSET         = 3200.0
      HEB%SCALE_FACTOR   = 6400.0/64000.0
!
! --- Write "pressure thickness variable"
!
      IUER = -1
      CALL WRITE_HEB ( HEB, DELP, FILOUT, IUER )
      IF ( IUER .NE. 0 ) THEN
           CALL GERROR ( STR )
           CALL ERR_LOG ( 7205, -2, 'GEOS57NV_TO_DGQTUV', 'Failure '// &
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
! --- Now update headers for surface geopotential height variable
!
      IF ( ILEN(FILIN) > 38 ) THEN
           IF ( FILIN(IL-33:IL-33) == '.' .AND. FILIN(IL-21:IL-21) == '+' ) THEN
                FILOUT = DIROUT(1:I_LEN(DIROUT))//'/'//SUBDIR(2)//'/'// &
     &                   SUBDIR(2)//'_'//FILIN(IL-32:IL-8)//'.heb'
              ELSE 
                FILOUT = DIROUT(1:I_LEN(DIROUT))//'/'//SUBDIR(2)//'/'// &
     &                   SUBDIR(2)//'_'//STR(1:4)//STR(6:7)//STR(9:10)// &
     &                   '_'//STR(12:13)//STR(15:16)//'.heb'
           END IF
         ELSE
           FILOUT = DIROUT(1:I_LEN(DIROUT))//'/'//SUBDIR(2)//'/'// &
     &              SUBDIR(2)//'_'//STR(1:4)//STR(6:7)//STR(9:10)// &
     &              '_'//STR(12:13)//STR(15:16)//'.heb'
      END IF
      HEB%SDS_NAME     = 'surface geopotential height'
      HEB%UNITS        = 'm^2/s^2'
      HEB%DATA_FORMAT  = HEB__R4
      HEB%MIN_VALUE    = MINVAL(PHIS)
      HEB%MAX_VALUE    = MAXVAL(PHIS)
      HEB%VALID_RANGE(1) = -2000.0
      HEB%VALID_RANGE(2) =  7600.0
      HEB%OFFSET       = 0.0
      HEB%SCALE_FACTOR = 1.0
      HEB%DIMS(3) = 1
!      
      IF ( FL_WRITE_PHIS ) THEN
           IUER = -1
           CALL WRITE_HEB ( HEB, PHIS, FILOUT, IUER )
           IF ( IUER .NE. 0 ) THEN
                CALL GERROR ( STR )
                CALL ERR_LOG ( 7205, -2, 'GEOS57NV_TO_DGQTUV', 'Failure '// &
          &         'in an attempt to write into output file '//FILOUT )
                CALL EXIT ( 1 )
           END IF
           IF ( ILEN(COMPR_COM) .NE. 0 ) THEN
!
! ------------- Now compress the output file 
!
                COM_STR = COMPR_COM(1:I_LEN(COMPR_COM))//' '// &
          &               FILOUT(1:I_LEN(FILOUT))
                CALL SYSTEM ( COM_STR(1:I_LEN(COM_STR))//CHAR(0) )
           END IF
      END IF
!
! --- Now update specific humidity data
!
      IF ( ILEN(FILIN) > 38 ) THEN
           IF ( FILIN(IL-33:IL-33) == '.' .AND. FILIN(IL-21:IL-21) == '+' ) THEN
                FILOUT = DIROUT(1:I_LEN(DIROUT))//'/'//SUBDIR(3)//'/'// &
     &                   SUBDIR(3)//'_'//FILIN(IL-32:IL-8)//'.heb'
              ELSE 
                FILOUT = DIROUT(1:I_LEN(DIROUT))//'/'//SUBDIR(3)//'/'// &
     &                   SUBDIR(3)//'_'//STR(1:4)//STR(6:7)//STR(9:10)// &
     &                   '_'//STR(12:13)//STR(15:16)//'.heb'
           END IF
         ELSE
           FILOUT = DIROUT(1:I_LEN(DIROUT))//'/'//SUBDIR(3)//'/'// &
     &              SUBDIR(3)//'_'//STR(1:4)//STR(6:7)//STR(9:10)// &
     &              '_'//STR(12:13)//STR(15:16)//'.heb'
      END IF
      HEB%SDS_NAME     = 'specific_humidity'
      HEB%UNITS        = 'dimensionless'
      HEB%DATA_FORMAT  = HEB__I2
      HEB%VALID_RANGE(1) = EXP(-34.0)
      HEB%VALID_RANGE(2) = EXP(-2.0)
      HEB%MIN_VALUE      = MAX ( MINVAL(QV), HEB%VALID_RANGE(1) )
      HEB%MAX_VALUE      = MIN ( MAXVAL(QV), HEB%VALID_RANGE(2) )
      HEB%DATA_TRANSFORM = HEB__LOG
      HEB%OFFSET         = -18.0
      HEB%SCALE_FACTOR   = 32.0/64000.0
      HEB%DIMS(3)        = MLEV
!
      IUER = -1
      CALL WRITE_HEB ( HEB, QV, FILOUT, IUER )
      IF ( IUER .NE. 0 ) THEN
           CALL GERROR ( STR )
           CALL ERR_LOG ( 7205, -2, 'GEOS57NV_TO_DGQTUV', 'Failure '// &
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
! --- Now update air temperature data
!
      IF ( ILEN(FILIN) > 38 ) THEN
           IF ( FILIN(IL-33:IL-33) == '.' .AND. FILIN(IL-21:IL-21) == '+' ) THEN
                FILOUT = DIROUT(1:I_LEN(DIROUT))//'/'//SUBDIR(4)//'/'// &
     &                   SUBDIR(4)//'_'//FILIN(IL-32:IL-8)//'.heb'
              ELSE 
                FILOUT = DIROUT(1:I_LEN(DIROUT))//'/'//SUBDIR(4)//'/'// &
     &                   SUBDIR(4)//'_'//STR(1:4)//STR(6:7)//STR(9:10)// &
     &                   '_'//STR(12:13)//STR(15:16)//'.heb'
           END IF
         ELSE
           FILOUT = DIROUT(1:I_LEN(DIROUT))//'/'//SUBDIR(4)//'/'// &
     &              SUBDIR(4)//'_'//STR(1:4)//STR(6:7)//STR(9:10)// &
     &              '_'//STR(12:13)//STR(15:16)//'.heb'
      END IF
      HEB%SDS_NAME       = 'air_temperature'
      HEB%UNITS          = 'K'
      HEB%DATA_FORMAT    = HEB__I2
      HEB%VALID_RANGE(1) =   0.0
      HEB%VALID_RANGE(2) = 512.0
      HEB%MIN_VALUE      = MINVAL(T)
      HEB%MAX_VALUE      = MAXVAL(T)
      HEB%DATA_TRANSFORM = HEB__SCOF
      HEB%OFFSET         = 256.0
      HEB%SCALE_FACTOR   = 512.0/64000.0
      HEB%DIMS(3)        = MLEV
!
      IUER = -1
      CALL WRITE_HEB ( HEB, T, FILOUT, IUER )
      IF ( IUER .NE. 0 ) THEN
           CALL GERROR ( STR )
           CALL ERR_LOG ( 7205, -2, 'GEOS57NV_TO_DGQTUV', 'Failure '// &
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
! --- Now update eastward wind data
!
      IF ( ILEN(FILIN) > 38 ) THEN
           IF ( FILIN(IL-33:IL-33) == '.' .AND. FILIN(IL-21:IL-21) == '+' ) THEN
                FILOUT = DIROUT(1:I_LEN(DIROUT))//'/'//SUBDIR(5)//'/'// &
     &                   SUBDIR(5)//'_'//FILIN(IL-32:IL-8)//'.heb'
              ELSE 
                FILOUT = DIROUT(1:I_LEN(DIROUT))//'/'//SUBDIR(5)//'/'// &
     &                   SUBDIR(5)//'_'//STR(1:4)//STR(6:7)//STR(9:10)// &
     &                   '_'//STR(12:13)//STR(15:16)//'.heb'
           END IF
         ELSE
           FILOUT = DIROUT(1:I_LEN(DIROUT))//'/'//SUBDIR(5)//'/'// &
     &              SUBDIR(5)//'_'//STR(1:4)//STR(6:7)//STR(9:10)// &
     &              '_'//STR(12:13)//STR(15:16)//'.heb'
      END IF
      HEB%SDS_NAME     = 'eastward_wind'
      HEB%UNITS        = 'm/s'
      HEB%DATA_FORMAT  = HEB__I2
      HEB%VALID_RANGE(1) = -256.0
      HEB%VALID_RANGE(2) =  256.0
      HEB%MIN_VALUE      = MINVAL(U)
      HEB%MAX_VALUE      = MAXVAL(U)
      HEB%DATA_TRANSFORM = HEB__SCOF
      HEB%OFFSET         = 0.0
      HEB%SCALE_FACTOR   = 512.0/64000.0
      HEB%DIMS(3)        = MLEV
!
      IUER = -1
      CALL WRITE_HEB ( HEB, U, FILOUT, IUER )
      IF ( IUER .NE. 0 ) THEN
           CALL GERROR ( STR )
           CALL ERR_LOG ( 7205, -2, 'GEOS57NV_TO_DGQTUV', 'Failure '// &
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
! --- Now update northward wind data
!
      IF ( ILEN(FILIN) > 38 ) THEN
           IF ( FILIN(IL-33:IL-33) == '.' .AND. FILIN(IL-21:IL-21) == '+' ) THEN
                FILOUT = DIROUT(1:I_LEN(DIROUT))//'/'//SUBDIR(6)//'/'// &
     &                   SUBDIR(6)//'_'//FILIN(IL-32:IL-8)//'.heb'
              ELSE 
                FILOUT = DIROUT(1:I_LEN(DIROUT))//'/'//SUBDIR(6)//'/'// &
     &                   SUBDIR(6)//'_'//STR(1:4)//STR(6:7)//STR(9:10)// &
     &                   '_'//STR(12:13)//STR(15:16)//'.heb'
           END IF
         ELSE
           FILOUT = DIROUT(1:I_LEN(DIROUT))//'/'//SUBDIR(6)//'/'// &
     &              SUBDIR(6)//'_'//STR(1:4)//STR(6:7)//STR(9:10)// &
     &              '_'//STR(12:13)//STR(15:16)//'.heb'
      END IF
      HEB%SDS_NAME     = 'northward_wind'
      HEB%UNITS        = 'm/s'
      HEB%DATA_FORMAT  = HEB__I2
      HEB%VALID_RANGE(1) = -256.0
      HEB%VALID_RANGE(2) =  256.0
      HEB%MIN_VALUE      = MINVAL(V)
      HEB%MAX_VALUE      = MAXVAL(V)
      HEB%DATA_TRANSFORM = HEB__SCOF
      HEB%OFFSET         = 0.0
      HEB%SCALE_FACTOR   = 512.0/64000.0
      HEB%DIMS(3)        = MLEV
!
      IUER = -1
      CALL WRITE_HEB ( HEB, V, FILOUT, IUER )
      IF ( IUER .NE. 0 ) THEN
           CALL GERROR ( STR )
           CALL ERR_LOG ( 7205, -2, 'GEOS57NV_TO_DGQTUV', 'Failure '// &
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
      END  SUBROUTINE  GEOS57NV_TO_DGQTUV  !#!#
