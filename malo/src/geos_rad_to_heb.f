      PROGRAM    GEOS_RAD_TO_HEB
! ************************************************************************
! *                                                                      *
! *   Program GEOS_RAD_TO_HEB  reads GEOS files with suffix FLX,         *
! *   extracts two parameter: eflux and hflux, and writes them in        *
! *   subdirectories eflux and hflux ofthe output directory. The output  *
! *   is written in HEB format.                                          *
! *                                                                      *
! * ### 04-FEB-2013  GEOS_RAD_TO_HEB  v3.0 (c) L. Petrov 29-APR-2013 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'heb.i'
      TYPE     ( HEB__TYPE ) :: HEB_LWTUP, HEB_LWTUPCLR, HEB_TAUTOT
      INTEGER*4    MLON, MLAT, MPAR
      PARAMETER  ( MLON = 1152 )
      PARAMETER  ( MLAT =  721 )
      PARAMETER  ( MPAR =    3 )
      CHARACTER    FILIN*128, DIROUT*128, FILOUT*128, STR*128, &
     &             DIR*128, COMPR*16, SUBDIR(MPAR)*8, COM_STR*256, &
     &             COMPR_COM*64
      DATA       SUBDIR /               &
     &                    'lwtup   ',   &
     &                    'lwtupclr',   &
     &                    'tautot  '    &
     &                  /
      LOGICAL*1  LEX
      INTEGER*2  MODE_I2
      DATA       MODE_I2 / O'0775' /
      REAL*8     TIM_STEP, UTC_START
      INTEGER*4  J1, J2, MJD_START, L_TIM, IUER
      INTEGER*8  DIR_DESC, IP, IS
      INTEGER*8, EXTERNAL :: OPENDIR, CLOSEDIR 
      INTEGER*4, EXTERNAL :: MKDIR, I_LEN, ILEN
      CHARACTER, EXTERNAL :: MJDSEC_TO_DATE*30
!
      IF ( IARGC() < 2 ) THEN
           WRITE ( 6, '(A)' ) 'Usage: file_in dirout [compr]'
           CALL EXIT ( 0 )
         ELSE 
           CALL GETARG ( 1, FILIN  )
           CALL GETARG ( 2, DIROUT )
           IF ( IARGC() .GE. 3 ) THEN
                CALL GETARG ( 3, COMPR ) 
              ELSE 
                COMPR = 'none'
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
           CALL ERR_LOG ( 7101, -2, 'GEOS_RAD_TO_HEB', 'Unsupported '// &
     &         'compression method: '//COMPR//' . Supported methods: '// &
     &         ' none gzip bzip2 pbzip2 pbzip2_p1' )
           CALL EXIT ( 1 )
      END IF
!
      INQUIRE ( FILE=FILIN, EXIST=LEX )
      IF ( .NOT. LEX ) THEN
           CALL ERR_LOG ( 7102, -2, 'GEOS_RAD_TO_HEB', 'Cannot find '// &
     &         'input file '//FILIN )
           CALL EXIT ( 1 )
      END IF
!
! --- Check whether the output directory exists
!
      DIR_DESC = OPENDIR ( DIROUT(1:I_LEN(DIROUT))//CHAR(0) ) 
      IF ( DIR_DESC .LE. 0 ) THEN
           CALL GERROR ( STR )
           CALL ERR_LOG ( 7103, -2, 'GEOS_RAD_TO_HEB', 'Output '// &
     &         'directory '//DIROUT(1:I_LEN(DIROUT))//' does not exist' )
           CALL EXIT ( 1 )
         ELSE 
           IP = CLOSEDIR ( %VAL(DIR_DESC) )
      END IF
!
! --- Check whether the output subdirectory exist
!
      DO 410 J1=1,MPAR
         DIR = DIROUT(1:I_LEN(DIROUT))//'/'//SUBDIR(J1)
         DIR_DESC = OPENDIR ( DIR(1:I_LEN(DIR))//CHAR(0) ) 
         IF ( DIR_DESC .LE. 0 ) THEN
              IS = MKDIR ( DIR(1:I_LEN(DIR))//CHAR(0), %VAL(MODE_I2) )
              IF ( IS .EQ. -1 ) THEN
                   CALL GERROR ( STR )
                   CALL ERR_LOG ( 7104, -2, 'GEOS_RAD_TO_HEB', 'Failure '// &
     &                 'in an attempt to create output directory '//DIR )
                   CALL EXIT ( 1 )
              END IF
            ELSE 
              IP = CLOSEDIR ( %VAL(DIR_DESC) )
         END IF
 410  CONTINUE 
!
      IUER = -1
      CALL READ_GEOS_RAD ( FILIN, HEB_LWTUP, HEB_LWTUPCLR, HEB_TAUTOT, &
     &                     TIM_STEP, IUER )
      IF ( IUER .NE. 0 ) THEN
           CALL ERR_LOG ( 7105, -2, 'GEOS_RAD_TO_HEB', 'Failure in '// &
     &         'parsing input file '//FILIN )
           CALL EXIT ( 1 )
      END IF
!
! --- Generic parameters
!
      HEB_LWTUP%DATA_OFFSET = HEB__HDS
      HEB_LWTUP%ENDIAN      = HEB__LE
      HEB_LWTUP%DATA_TRANSFORM = HEB__NONE
      HEB_LWTUP%FILL_VALUE     = 1.0E15
      HEB_LWTUP%OFFSET         = 0.0
      HEB_LWTUP%SCALE_FACTOR   = 1.0
      HEB_LWTUP%DATA_COMPRESSION = HEB__NONE
!
      HEB_LWTUPCLR%DATA_OFFSET = HEB__HDS
      HEB_LWTUPCLR%ENDIAN      = HEB__LE
      HEB_LWTUPCLR%DATA_TRANSFORM = HEB__NONE
      HEB_LWTUPCLR%FILL_VALUE     = 1.0E15
      HEB_LWTUPCLR%OFFSET         = 0.0
      HEB_LWTUPCLR%SCALE_FACTOR   = 1.0
      HEB_LWTUPCLR%DATA_COMPRESSION = HEB__NONE
!
      HEB_TAUTOT%DATA_OFFSET = HEB__HDS
      HEB_TAUTOT%ENDIAN      = HEB__LE
      HEB_TAUTOT%DATA_TRANSFORM = HEB__NONE
      HEB_TAUTOT%FILL_VALUE     = 1.0E15
      HEB_TAUTOT%OFFSET         = 0.0
      HEB_TAUTOT%SCALE_FACTOR   = 1.0
      HEB_TAUTOT%DATA_COMPRESSION = HEB__NONE
!
! --- Write "total_latent_energy_flux"
!
      MJD_START = HEB_LWTUP%MJD
      UTC_START = HEB_LWTUP%UTC
      L_TIM = HEB_LWTUP%DIMS(4)
      HEB_LWTUP%DIMS(3)    = 1
      HEB_LWTUP%DIMS(4)    = 1
      HEB_LWTUPCLR%DIMS(3) = 1
      HEB_LWTUPCLR%DIMS(4) = 1
      HEB_TAUTOT%DIMS(3)   = 1
      HEB_TAUTOT%DIMS(4)   = 1
      DO 420 J2=1,L_TIM
         HEB_LWTUP%UTC = UTC_START + (J2-1)*TIM_STEP
         HEB_LWTUP%MJD = MJD_START
         IF ( HEB_LWTUP%UTC > 86400.0D0 ) THEN
              HEB_LWTUP%UTC = HEB_LWTUP%UTC - 86400.0D0 
              HEB_LWTUP%MJD = MJD_START + 1
         END IF
         HEB_LWTUP%TAI = HEB_LWTUP%UTC
!
         IUER = -1
         STR = MJDSEC_TO_DATE ( HEB_LWTUP%MJD, HEB_LWTUP%UTC, IUER )
         IF ( IUER .NE. 0 ) THEN
              CALL GERROR ( STR )
              CALL ERR_LOG ( 7105, -2, 'GEOS_RAD_TO_HEB', 'Failure '// &
     &            'in an attempt to parse the date '//FILIN )
              CALL EXIT ( 1 )
         END IF
!
         HEB_LWTUPCLR%MJD = HEB_LWTUP%MJD
         HEB_LWTUPCLR%UTC = HEB_LWTUP%UTC
         HEB_LWTUPCLR%TAI = HEB_LWTUP%TAI
!
! ------ Update headers for "upwelling_longwave_flux_at_toa" variable
!
         FILOUT = DIROUT(1:I_LEN(DIROUT))//'/'//SUBDIR(1)(1:I_LEN(SUBDIR(1)))// &
     &            '/'//SUBDIR(1)(1:I_LEN(SUBDIR(1)))//'_'// &
     &            STR(1:4)//STR(6:7)//STR(9:10)//'_'//STR(12:13)// &
     &            STR(15:16)//'.heb'
!
         HEB_LWTUP%SDS_NAME       = 'upwelling_longwave_flux_at_toa'
         HEB_LWTUP%UNITS          = 'W m-2'
         HEB_LWTUP%DATA_FORMAT    = HEB__R4
         HEB_LWTUP%MIN_VALUE      = MINVAL(HEB_LWTUP%VAL)
         HEB_LWTUP%MAX_VALUE      = MAXVAL(HEB_LWTUP%VAL)
         HEB_LWTUP%VALID_RANGE(1) = 0.0
         HEB_LWTUP%VALID_RANGE(2) = 500.0
         HEB_LWTUP%OFFSET         = 0.0
         HEB_LWTUP%SCALE_FACTOR   = 1.0
!
! ------ Write "upwelling_longwave_flux_at_toa"
!
         IUER = -1
         CALL WRITE_HEB ( HEB_LWTUP, HEB_LWTUP%VAL(1,1,1,J2), FILOUT, IUER )
         IF ( IUER .NE. 0 ) THEN
              CALL GERROR ( STR )
              CALL ERR_LOG ( 7106, -2, 'GEOS_RAD_TO_HEB', 'Failure '// &
     &            'in an attempt to write into output file '//FILOUT )
              CALL EXIT ( 1 )
         END IF
         IF ( ILEN(COMPR_COM) .NE. 0 ) THEN
!
! ----------- Now compress the output file 
!
              COM_STR = COMPR_COM(1:I_LEN(COMPR_COM))//' '// &
     &                  FILOUT(1:I_LEN(FILOUT))
              CALL SYSTEM ( COM_STR(1:I_LEN(COM_STR))//CHAR(0) )
         END IF
!
! ====== LTWUPCL
!
         HEB_LWTUPCLR%SDS_NAME       = 'upwelling_longwave_flux_at_toa_assuming_clear_sky'
         HEB_LWTUPCLR%UNITS          = 'W m-2'
         HEB_LWTUPCLR%DATA_FORMAT    = HEB__R4
         HEB_LWTUPCLR%MIN_VALUE      = MINVAL(HEB_LWTUPCLR%VAL)
         HEB_LWTUPCLR%MAX_VALUE      = MAXVAL(HEB_LWTUPCLR%VAL)
         HEB_LWTUPCLR%VALID_RANGE(1) = 0.0
         HEB_LWTUPCLR%VALID_RANGE(2) = 500.0
         HEB_LWTUPCLR%OFFSET         = 0.0
         HEB_LWTUPCLR%SCALE_FACTOR   = 1.0
!
! ------ Update headers for "upwelling_longwave_flux_at_toa" variable
!
         FILOUT = DIROUT(1:I_LEN(DIROUT))//'/'//SUBDIR(2)(1:I_LEN(SUBDIR(2)))// &
     &            '/'//SUBDIR(2)(1:I_LEN(SUBDIR(2)))//'_'// &
     &            STR(1:4)//STR(6:7)//STR(9:10)//'_'//STR(12:13)// &
     &            STR(15:16)//'.heb'
!
! ------ Write "upwelling_longwave_flux_at_toa_assuming_clear_sky"
!
         IUER = -1
         CALL WRITE_HEB ( HEB_LWTUPCLR, HEB_LWTUPCLR%VAL(1,1,1,J2), FILOUT, IUER )
         IF ( IUER .NE. 0 ) THEN
              CALL GERROR ( STR )
              CALL ERR_LOG ( 7106, -2, 'GEOS_RAD_TO_HEB', 'Failure '// &
     &            'in an attempt to write into output file '//FILOUT )
              CALL EXIT ( 1 )
         END IF
         IF ( ILEN(COMPR_COM) .NE. 0 ) THEN
!
! ----------- Now compress the output file 
!
              COM_STR = COMPR_COM(1:I_LEN(COMPR_COM))//' '// &
     &                  FILOUT(1:I_LEN(FILOUT))
              CALL SYSTEM ( COM_STR(1:I_LEN(COM_STR))//CHAR(0) )
         END IF
!
! ====== TAUTOT
!
         HEB_TAUTOT%SDS_NAME       = 'optical_thickness_of_all_clouds'
         HEB_TAUTOT%UNITS          = 'd/l'
         HEB_TAUTOT%DATA_FORMAT    = HEB__I2
         HEB_TAUTOT%MIN_VALUE      = MINVAL(HEB_TAUTOT%VAL)
         HEB_TAUTOT%MAX_VALUE      = MAXVAL(HEB_TAUTOT%VAL)
         HEB_TAUTOT%VALID_RANGE(1) = 0.0
         HEB_TAUTOT%VALID_RANGE(2) = 50.0
         HEB_TAUTOT%OFFSET         = 0.0
         HEB_TAUTOT%SCALE_FACTOR   = 0.0005
!
! ------ Update headers for "upwelling_longwave_flux_at_toa" variable
!
         FILOUT = DIROUT(1:I_LEN(DIROUT))//'/'//SUBDIR(3)(1:I_LEN(SUBDIR(3)))// &
     &            '/'//SUBDIR(3)(1:I_LEN(SUBDIR(3)))//'_'// &
     &            STR(1:4)//STR(6:7)//STR(9:10)//'_'//STR(12:13)// &
     &            STR(15:16)//'.heb'
!
! ------ Write "optical_thickness_of_all_clouds"
!
         IUER = -1
         CALL WRITE_HEB ( HEB_TAUTOT, HEB_TAUTOT%VAL(1,1,1,J2), FILOUT, IUER )
         IF ( IUER .NE. 0 ) THEN
              CALL GERROR ( STR )
              CALL ERR_LOG ( 7106, -2, 'GEOS_RAD_TO_HEB', 'Failure '// &
     &            'in an attempt to write into output file '//FILOUT )
              CALL EXIT ( 1 )
         END IF
         IF ( ILEN(COMPR_COM) .NE. 0 ) THEN
!
! ----------- Now compress the output file 
!
              COM_STR = COMPR_COM(1:I_LEN(COMPR_COM))//' '// &
     &                  FILOUT(1:I_LEN(FILOUT))
              CALL SYSTEM ( COM_STR(1:I_LEN(COM_STR))//CHAR(0) )
         END IF
 420  CONTINUE 
      CALL EXIT ( 0 )
      END  PROGRAM    GEOS_RAD_TO_HEB   !#!#
