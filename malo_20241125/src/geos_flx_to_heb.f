      PROGRAM    GEOS_FLX_TO_HEB
! ************************************************************************
! *                                                                      *
! *   Program GEOS_FLX_TO_HEB  reads GEOS files with suffix FLX,         *
! *   extracts two parameter: eflux and hflux, and writes them in        *
! *   subdirectories eflux and hflux of the output directory. The output *
! *   is written in HEB format.                                          *
! *                                                                      *
! * ### 11-JAN-2013 GEOS_FLX_TO_HEB v2.0 (c) L. Petrov 11-FEB-2013  ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'heb.i'
      TYPE     ( HEB__TYPE ) :: HEB_EFLUX, HEB_HFLUX
      INTEGER*4    MPAR
      PARAMETER  ( MPAR =    2 )
      CHARACTER    FILIN*128, DIROUT*128, FILOUT*128, STR*128, &
     &             DIR*128, SUBDIR(MPAR)*8, COM_STR*256, COMPR*16, &
     &             COMPR_COM*64
      DATA       SUBDIR /              &
     &                    'eflux   ',  &
     &                    'hflux   '   &
     &                  /
      LOGICAL*1  LEX
      INTEGER*2  MODE_I2
      DATA       MODE_I2 / O'0775' /
      INTEGER*4  J1, J2, J3, MJD_START, L_TIM, IUER
      REAL*8     TIM_STEP, UTC_START 
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
           CALL ERR_LOG ( 7101, -2, 'GEOS_FLX_TO_HEB', 'Unsupported '// &
     &         'compression method: '//COMPR//' . Supported methods: '// &
     &         ' none gzip bzip2 pbzip2 pbzip2_p1' )
           CALL EXIT ( 1 )
      END IF
!
      INQUIRE ( FILE=FILIN, EXIST=LEX )
      IF ( .NOT. LEX ) THEN
           CALL ERR_LOG ( 7102, -2, 'GEOS_FLX_TO_HEB', 'Cannot find '// &
     &         ' none gzip bzip2 pbzip2 pbzip_p1' )
           CALL EXIT ( 1 )
      END IF
!
! --- Check whether the output directory exists
!
      DIR_DESC = OPENDIR ( DIROUT(1:I_LEN(DIROUT))//CHAR(0) ) 
      IF ( DIR_DESC .LE. 0 ) THEN
           CALL GERROR ( STR )
           CALL ERR_LOG ( 7103, -2, 'GEOS_FLX_TO_HEB', 'Output '// &
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
                   CALL ERR_LOG ( 7104, -2, 'GEOS_FLX_TO_HEB', 'Failure '// &
     &                 'in an attempt to create output directory '//DIR )
                   CALL EXIT ( 1 )
              END IF
            ELSE 
              IP = CLOSEDIR ( %VAL(DIR_DESC) )
         END IF
 410  CONTINUE 
!
      IUER = -1
      CALL READ_GEOS_FLUX ( FILIN, HEB_EFLUX, HEB_HFLUX, TIM_STEP, IUER )
      IF ( IUER .NE. 0 ) THEN
           CALL ERR_LOG ( 7105, -2, 'GEOS_FLX_TO_HEB', 'Failure in '// &
     &         'parsing input file '//FILIN )
           CALL EXIT ( 1 )
      END IF
!
! --- Generic parameters
!
      HEB_EFLUX%DATA_OFFSET = HEB__HDS
      HEB_EFLUX%ENDIAN      = HEB__LE
      HEB_EFLUX%DATA_TRANSFORM = HEB__NONE
      HEB_EFLUX%FILL_VALUE     = 1.0E15
      HEB_EFLUX%DATA_COMPRESSION = HEB__NONE
      HEB_EFLUX%SDS_NAME       = 'total_latent_energy_flux'
      HEB_EFLUX%UNITS          = 'W m-2'
      HEB_EFLUX%DATA_FORMAT    = HEB__R4
      HEB_EFLUX%MIN_VALUE      = MINVAL(HEB_EFLUX%VAL)
      HEB_EFLUX%MAX_VALUE      = MAXVAL(HEB_EFLUX%VAL)
      HEB_EFLUX%VALID_RANGE(1) = MINVAL(HEB_EFLUX%VAL)
      HEB_EFLUX%VALID_RANGE(2) = MAXVAL(HEB_EFLUX%VAL)
      HEB_EFLUX%OFFSET         = 0.0
      HEB_EFLUX%SCALE_FACTOR   = 1.0
!
! --- Write "total_latent_energy_flux"
!
      MJD_START = HEB_EFLUX%MJD
      UTC_START = HEB_EFLUX%UTC
      L_TIM = HEB_EFLUX%DIMS(4)
      HEB_EFLUX%DIMS(3) = 1
      HEB_EFLUX%DIMS(4) = 1
      DO 420 J2=1,L_TIM
         HEB_EFLUX%UTC = UTC_START + (J2-1)*TIM_STEP
         HEB_EFLUX%MJD = MJD_START
         IF ( HEB_EFLUX%UTC > 86400.0D0 ) THEN
              HEB_EFLUX%UTC = HEB_EFLUX%UTC - 86400.0D0 
              HEB_EFLUX%MJD = MJD_START + 1
         END IF
         HEB_EFLUX%TAI = HEB_EFLUX%UTC
!
         IUER = -1
         STR = MJDSEC_TO_DATE ( HEB_EFLUX%MJD, HEB_EFLUX%UTC, IUER )
         IF ( IUER .NE. 0 ) THEN
              CALL GERROR ( STR )
              CALL ERR_LOG ( 7105, -2, 'GEOS_FLX_TO_HEB', 'Failure '// &
     &            'in an attempt to parse the date '//FILIN )
              CALL EXIT ( 1 )
         END IF
!
! ------ Update headers for "total_latent_energy_flux" variable
!
         FILOUT = DIROUT(1:I_LEN(DIROUT))//'/'//SUBDIR(1)(1:I_LEN(SUBDIR(1)))// &
     &            '/'//SUBDIR(1)(1:I_LEN(SUBDIR(1)))//'_'// &
     &            STR(1:4)//STR(6:7)//STR(9:10)//'_'//STR(12:13)// &
     &            STR(15:16)//'.heb'
!
         IUER = -1
         CALL WRITE_HEB ( HEB_EFLUX, HEB_EFLUX%VAL(1,1,1,J2), FILOUT, IUER )
         IF ( IUER .NE. 0 ) THEN
              CALL GERROR ( STR )
              CALL ERR_LOG ( 7106, -2, 'GEOS_FLX_TO_HEB', 'Failure '// &
     &            'in an attempt to write into output file '//FILOUT )
              CALL EXIT ( 1 )
         END IF
         IF ( ILEN(COMPR_COM) .NE. 0 ) THEN
!
! ----------- Now compress the output file 
!
              COM_STR = COMPR_COM(1:I_LEN(COMPR_COM))//' '//FILOUT(1:I_LEN(FILOUT))
              CALL SYSTEM ( COM_STR(1:I_LEN(COM_STR))//CHAR(0) )
         END IF
 420  CONTINUE 
!
! --- Update headers for "total_latent_energy_flux" variable
!
      FILOUT = DIROUT(1:I_LEN(DIROUT))//'/'//SUBDIR(2)(1:I_LEN(SUBDIR(2)))// &
     &         '/'//SUBDIR(2)(1:I_LEN(SUBDIR(2)))//'_'// &
     &         STR(1:4)//STR(6:7)//STR(9:10)//'_'//STR(12:13)// &
     &         STR(15:16)//'.heb'
!
      HEB_HFLUX%SDS_NAME       = 'sensible_heat_flux_from_turbulence'
      HEB_HFLUX%UNITS          = 'W m-2'
      HEB_HFLUX%DATA_FORMAT    = HEB__R4
      HEB_HFLUX%MIN_VALUE      = MINVAL(HEB_HFLUX%VAL)
      HEB_HFLUX%MAX_VALUE      = MAXVAL(HEB_HFLUX%VAL)
      HEB_HFLUX%VALID_RANGE(1) = MINVAL(HEB_HFLUX%VAL)
      HEB_HFLUX%VALID_RANGE(2) = MAXVAL(HEB_HFLUX%VAL)
      HEB_HFLUX%OFFSET         = 0.0
      HEB_HFLUX%SCALE_FACTOR   = 1.0
!
      HEB_HFLUX%TAI         = HEB_EFLUX%TAI 
      HEB_HFLUX%DATA_OFFSET = HEB_EFLUX%DATA_OFFSET 
      HEB_HFLUX%ENDIAN      = HEB_EFLUX%ENDIAN      
      HEB_HFLUX%DATA_TRANSFORM   = HEB_EFLUX%DATA_TRANSFORM 
      HEB_HFLUX%FILL_VALUE       = HEB_EFLUX%FILL_VALUE     
      HEB_HFLUX%DATA_COMPRESSION = HEB__NONE
!
! --- Write "sensible_heat_flux_from_turbulence"
!
      MJD_START = HEB_HFLUX%MJD
      UTC_START = HEB_HFLUX%UTC
      L_TIM = HEB_HFLUX%DIMS(4)
      HEB_HFLUX%DIMS(3) = 1
      HEB_HFLUX%DIMS(4) = 1
      DO 430 J3=1,L_TIM
         HEB_HFLUX%UTC = UTC_START + (J3-1)*TIM_STEP
         HEB_HFLUX%MJD = MJD_START
         IF ( HEB_HFLUX%UTC > 86400.0D0 ) THEN
              HEB_HFLUX%UTC = HEB_HFLUX%UTC - 86400.0D0 
              HEB_HFLUX%MJD = MJD_START + 1
         END IF
         HEB_HFLUX%TAI = HEB_HFLUX%UTC
!
         IUER = -1
         STR = MJDSEC_TO_DATE ( HEB_HFLUX%MJD, HEB_HFLUX%UTC, IUER )
         IF ( IUER .NE. 0 ) THEN
              CALL GERROR ( STR )
              CALL ERR_LOG ( 7105, -2, 'GEOS_FLX_TO_HEB', 'Failure '// &
     &            'in an attempt to parse the date '//FILIN )
              CALL EXIT ( 1 )
         END IF
!
! ------ Update headers for "total_latent_energy_flux" variable
!
         FILOUT = DIROUT(1:I_LEN(DIROUT))//'/'//SUBDIR(2)(1:I_LEN(SUBDIR(2)))// &
     &            '/'//SUBDIR(2)(1:I_LEN(SUBDIR(2)))//'_'// &
     &            STR(1:4)//STR(6:7)//STR(9:10)//'_'//STR(12:13)// &
     &            STR(15:16)//'.heb'
!
         IUER = -1
         CALL WRITE_HEB ( HEB_HFLUX, HEB_HFLUX%VAL(1,1,1,J3), FILOUT, IUER )
         IF ( IUER .NE. 0 ) THEN
              CALL GERROR ( STR )
              CALL ERR_LOG ( 7106, -2, 'GEOS_FLX_TO_HEB', 'Failure '// &
     &            'in an attempt to write into output file '//FILOUT )
              CALL EXIT ( 1 )
         END IF
         IF ( ILEN(COMPR_COM) .NE. 0 ) THEN
!
! ----------- Now compress the output file 
!
              COM_STR = COMPR_COM(1:I_LEN(COMPR_COM))//' '//FILOUT(1:I_LEN(FILOUT))
              CALL SYSTEM ( COM_STR(1:I_LEN(COM_STR))//CHAR(0) )
         END IF
 430  CONTINUE 
!
      CALL EXIT ( 0 )
      END  PROGRAM  GEOS_FLX_TO_HEB  !#!#
