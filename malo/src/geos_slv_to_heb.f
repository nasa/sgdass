      PROGRAM    GEOS_SLV_TO_HEB
! ************************************************************************
! *                                                                      *
! *   Program GEOS_SLV_TO_HEB  reads GEOS files with suffix SLV,         *
! *   extract parameter: eflux and hflux, and writes it in               *
! *   subdirectory eflux or hflux of the output directory. The output    *
! *   is written in HEB format.                                          *
! *                                                                      *
! * ### 06-MAY-2013  GEOS_SLV_TO_HEB v1.0 (c) L. Petrov 07-MAY-2013 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'heb.i'
      TYPE     ( HEB__TYPE ) :: HEB_U250, HEB_V250, HEB_CLDPRS, HEB_CLDTMP
      INTEGER*4    MPAR
      PARAMETER  ( MPAR =    4 )
      CHARACTER    FILIN*128, DIROUT*128, FILOUT*128, STR*128, &
     &             DIR*128, SUBDIR(MPAR)*8, COM_STR*256, COMPR*16, &
     &             COMPR_COM*64
      DATA         SUBDIR /            &
     &                     'U250    ', &
     &                     'V250    ', &
     &                     'CLDPRS  ', &
     &                     'CLDTMP  '  &
     &                    /
      LOGICAL*1  LEX
      INTEGER*2  MODE_I2
      DATA       MODE_I2 / O'0775' /
      INTEGER*4  J1, J2, J3, J4, J5, J6, MJD_START, ID, L_TIM, IUER
      REAL*8     TIM_STEP, UTC_START 
      INTEGER*8  DIR_DESC, IP, IS
      INTEGER*8, EXTERNAL :: OPENDIR, CLOSEDIR 
      INTEGER*4, EXTERNAL :: I_LEN, ILEN, LINDEX, MKDIR
      CHARACTER, EXTERNAL :: MJDSEC_TO_DATE*30
!
      IF ( IARGC() < 2 ) THEN
           WRITE ( 6, '(A)' ) 'Usage: geos_slv_to_heb file_in dirout [compr]'
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
           CALL ERR_LOG ( 7301, -2, 'GEOS_SLV_TO_HEB', 'Unsupported '// &
     &         'compression method: '//COMPR//' . Supported methods: '// &
     &         ' none gzip bzip2 pbzip2 pbzip2_p1' )
           CALL EXIT ( 1 )
      END IF
!
      INQUIRE ( FILE=FILIN, EXIST=LEX )
      IF ( .NOT. LEX ) THEN
           CALL ERR_LOG ( 7302, -2, 'GEOS_SLV_TO_HEB', 'Cannot find '// &
     &         ' none gzip bzip2 pbzip2 pbzip_p1' )
           CALL EXIT ( 1 )
      END IF
!
! --- Check whether the output directory exists
!
      DIR_DESC = OPENDIR ( DIROUT(1:I_LEN(DIROUT))//CHAR(0) ) 
      IF ( DIR_DESC .LE. 0 ) THEN
           CALL GERROR ( STR )
           CALL ERR_LOG ( 7303, -2, 'GEOS_SLV_TO_HEB', 'Output '// &
     &         'directory '//DIROUT(1:I_LEN(DIROUT))//' does not exist' )
           CALL EXIT ( 1 )
         ELSE 
           IP = CLOSEDIR ( %VAL(DIR_DESC) )
      END IF
!
! --- Check whether the output directory exist
!
      DIR = DIROUT(1:I_LEN(DIROUT))
      DIR_DESC = OPENDIR ( DIR(1:I_LEN(DIR))//CHAR(0) ) 
      IF ( DIR_DESC .LE. 0 ) THEN
           IS = MKDIR ( DIR(1:I_LEN(DIR))//CHAR(0), %VAL(MODE_I2) )
           IF ( IS .EQ. -1 ) THEN
                CALL GERROR ( STR )
                CALL ERR_LOG ( 7304, -2, 'GEOS_SLV_TO_HEB', 'Failure '// &
     &              'in an attempt to create output directory '//DIR )
                CALL EXIT ( 1 )
           END IF
         ELSE 
           IP = CLOSEDIR ( %VAL(DIR_DESC) )
      END IF
!
      IUER = -1
      CALL READ_GEOS_SLV ( FILIN, HEB_U250, HEB_V250, HEB_CLDPRS, HEB_CLDTMP, &
     &                     TIM_STEP, IUER )
      IF ( IUER .NE. 0 ) THEN
           CALL ERR_LOG ( 7305, -2, 'GEOS_SLV_TO_HEB', 'Failure in '// &
     &         'parsing input file '//FILIN )
           CALL EXIT ( 1 )
      END IF
!
! --- Generic parameters
!
      HEB_U250%DATA_OFFSET = HEB__HDS
      HEB_U250%ENDIAN      = HEB__LE
      HEB_U250%DATA_COMPRESSION = HEB__NONE
      HEB_U250%DATA_TRANSFORM = HEB__SCOF
      HEB_U250%FILL_VALUE     = 1.0E15
      HEB_U250%SDS_NAME       = 'Eastward wind at 250 hPa'
      HEB_U250%UNITS          = 'm/s'
      HEB_U250%DATA_FORMAT    = HEB__I2
      HEB_U250%MIN_VALUE      = MINVAL(HEB_U250%VAL)
      HEB_U250%MAX_VALUE      = MAXVAL(HEB_U250%VAL)
      HEB_U250%VALID_RANGE(1) = -256.0
      HEB_U250%VALID_RANGE(2) =  256.0
      HEB_U250%SCALE_FACTOR   = 512.0/64000.0
      HEB_U250%OFFSET         = 0.0
!
      HEB_V250%DATA_OFFSET = HEB__HDS
      HEB_V250%ENDIAN      = HEB__LE
      HEB_V250%DATA_COMPRESSION = HEB__NONE
      HEB_V250%DATA_TRANSFORM = HEB__SCOF
      HEB_V250%FILL_VALUE     = 1.0E15
      HEB_V250%SDS_NAME       = 'Northward wind at 250 hPa'
      HEB_V250%UNITS          = 'm/s'
      HEB_V250%DATA_FORMAT    = HEB__I2
      HEB_V250%MIN_VALUE      = MINVAL(HEB_V250%VAL)
      HEB_V250%MAX_VALUE      = MAXVAL(HEB_V250%VAL)
      HEB_V250%VALID_RANGE(1) = -256.0
      HEB_V250%VALID_RANGE(2) =  256.0
      HEB_V250%SCALE_FACTOR   = 512.0/64000.0
      HEB_V250%OFFSET         = 0.0
!
      HEB_CLDPRS%DATA_OFFSET = HEB__HDS
      HEB_CLDPRS%ENDIAN      = HEB__LE
      HEB_CLDPRS%DATA_COMPRESSION = HEB__NONE
      HEB_CLDPRS%DATA_TRANSFORM = HEB__NONE
      HEB_CLDPRS%FILL_VALUE     = 1.0E15
      HEB_CLDPRS%SDS_NAME       = 'Cloud-top pressure'
      HEB_CLDPRS%UNITS          = 'Pa'
      HEB_CLDPRS%DATA_FORMAT    = HEB__R4
      HEB_CLDPRS%MIN_VALUE      = MINVAL(HEB_CLDPRS%VAL)
      HEB_CLDPRS%VALID_RANGE(1) = 1.
      HEB_CLDPRS%VALID_RANGE(2) = 100000.
      HEB_CLDPRS%OFFSET         = 0.0
      HEB_CLDPRS%SCALE_FACTOR   = 1.0
!
      HEB_CLDTMP%DATA_OFFSET = HEB__HDS
      HEB_CLDTMP%ENDIAN      = HEB__LE
      HEB_CLDTMP%DATA_COMPRESSION = HEB__NONE
      HEB_CLDTMP%DATA_TRANSFORM = HEB__NONE
      HEB_CLDTMP%FILL_VALUE     = 1.0E15
      HEB_CLDTMP%SDS_NAME       = 'Cloud-top temperature'
      HEB_CLDTMP%UNITS          = 'K'
      HEB_CLDTMP%DATA_FORMAT    = HEB__R4
      HEB_CLDTMP%MIN_VALUE      = MINVAL(HEB_CLDTMP%VAL)
      HEB_CLDTMP%VALID_RANGE(1) = 0.0
      HEB_CLDTMP%VALID_RANGE(2) = 512.0
      HEB_CLDTMP%OFFSET         = 0.0
      HEB_CLDTMP%SCALE_FACTOR   = 1.0
!
      HEB_CLDTMP%MAX_VALUE = HEB_CLDTMP%MIN_VALUE
      HEB_CLDPRS%MAX_VALUE = HEB_CLDPRS%MIN_VALUE
      DO 410 J1=1,HEB_CLDTMP%DIMS(3)
         DO 420 J2=1,HEB_CLDTMP%DIMS(2)
            DO 430 J3=1,HEB_CLDTMP%DIMS(1)
               IF ( HEB_CLDTMP%VAL(J3,J2,J1,1) < HEB_CLDTMP%FILL_VALUE/2 .AND. &
     &              HEB_CLDTMP%VAL(J3,J2,J1,1) > HEB_CLDTMP%MAX_VALUE ) THEN
                    HEB_CLDTMP%MAX_VALUE = HEB_CLDTMP%VAL(J3,J2,J1,1) 
               END IF
               IF ( HEB_CLDPRS%VAL(J3,J2,J1,1) < HEB_CLDPRS%FILL_VALUE/2 .AND. &
     &              HEB_CLDPRS%VAL(J3,J2,J1,1) > HEB_CLDPRS%MAX_VALUE ) THEN
                    HEB_CLDPRS%MAX_VALUE = HEB_CLDPRS%VAL(J3,J2,J1,1) 
               END IF
 430        CONTINUE 
 420     CONTINUE 
 410  CONTINUE 
!
      MJD_START = HEB_U250%MJD
      UTC_START = HEB_U250%UTC
      L_TIM = HEB_U250%DIMS(4)
!
      HEB_U250%DIMS(3) = 1
      HEB_U250%DIMS(4) = 1
!
      HEB_V250%DIMS(3) = 1
      HEB_V250%DIMS(4) = 1
!
      HEB_CLDTMP%DIMS(3) = 1
      HEB_CLDTMP%DIMS(4) = 1
!
      HEB_CLDPRS%DIMS(3) = 1
      HEB_CLDPRS%DIMS(4) = 1
!
      DO 440 J4=1,L_TIM
         HEB_U250%UTC = UTC_START + (J4-1)*TIM_STEP
         HEB_U250%MJD = MJD_START
         IF ( HEB_U250%UTC > 86400.0D0 ) THEN
              HEB_U250%UTC = HEB_U250%UTC - 86400.0D0 
              HEB_U250%MJD = MJD_START + 1
         END IF
         HEB_U250%TAI = HEB_U250%UTC
!
         IUER = -1
         STR = MJDSEC_TO_DATE ( HEB_U250%MJD, HEB_U250%UTC, IUER )
         IF ( IUER .NE. 0 ) THEN
              CALL GERROR ( STR )
              CALL ERR_LOG ( 7306, -2, 'GEOS_SLV_TO_HEB', 'Failure '// &
     &            'in an attempt to parse the date '//FILIN )
              CALL EXIT ( 1 )
         END IF
!
         DO 450 J5=1,MPAR
            FILOUT = DIROUT(1:I_LEN(DIROUT))//'/'//STR(1:4)//'/'// &
     &               SUBDIR(J5)(1:I_LEN(SUBDIR(J5)))// &
     &               '/'//SUBDIR(J5)(1:I_LEN(SUBDIR(J5)))//'_'// &
     &               STR(1:4)//STR(6:7)//STR(9:10)//'_'//STR(12:13)// &
     &               STR(15:16)//'.heb'
            ID = LINDEX ( FILOUT, '/' )
            HEB_U250%FILE_NAME = FILOUT(ID+1:)
!
! --------- Check esistence of the subdirectory with year
!
            DIR = DIROUT(1:I_LEN(DIROUT))//'/'//STR(1:4)
            DIR_DESC = OPENDIR ( DIR(1:I_LEN(DIR))//CHAR(0) ) 
            IF ( DIR_DESC .LE. 0 ) THEN
                 IS = MKDIR ( DIR(1:I_LEN(DIR))//CHAR(0), %VAL(MODE_I2) )
                 IF ( IS .EQ. -1 ) THEN
                      CALL GERROR ( STR )
                      CALL ERR_LOG ( 7307, -2, 'GEOS_SLV_TO_HEB', 'Failure '// &
     &                    'in an attempt to create output subdirectory '//DIR )
                      CALL EXIT ( 1 )
                 END IF
               ELSE 
                 IP = CLOSEDIR ( %VAL(DIR_DESC) )
            END IF
!
! --------- Check existence of the subdirectory with year and product name
!
            DIR = DIROUT(1:I_LEN(DIROUT))//'/'//STR(1:4)//'/'// &
     &            SUBDIR(J5)(1:I_LEN(SUBDIR(J5)))
            DIR_DESC = OPENDIR ( DIR(1:I_LEN(DIR))//CHAR(0) ) 
            IF ( DIR_DESC .LE. 0 ) THEN
                 IS = MKDIR ( DIR(1:I_LEN(DIR))//CHAR(0), %VAL(MODE_I2) )
                 IF ( IS .EQ. -1 ) THEN
                      CALL GERROR ( STR )
                      CALL ERR_LOG ( 7308, -2, 'GEOS_SLV_TO_HEB', 'Failure '// &
     &                    'in an attempt to create output subdirectory '//DIR )
                      CALL EXIT ( 1 )
                 END IF
               ELSE 
                 IP = CLOSEDIR ( %VAL(DIR_DESC) )
            END IF
!
            IUER = -1
            IF ( J5 == 1 ) THEN
                 CALL WRITE_HEB ( HEB_U250, HEB_U250%VAL, FILOUT, IUER )
               ELSE IF ( J5 == 2 ) THEN
                 CALL WRITE_HEB ( HEB_V250, HEB_V250%VAL, FILOUT, IUER )
               ELSE IF ( J5 == 3 ) THEN
                 CALL WRITE_HEB ( HEB_CLDPRS, HEB_CLDPRS%VAL, FILOUT, IUER )
               ELSE IF ( J5 == 4 ) THEN
                 CALL WRITE_HEB ( HEB_CLDTMP, HEB_CLDTMP%VAL, FILOUT, IUER )
            END IF
            IF ( IUER .NE. 0 ) THEN
                 CALL GERROR ( STR )
                 CALL ERR_LOG ( 7309, -2, 'GEOS_SLV_TO_HEB', 'Failure '// &
     &               'in an attempt to write into output file '//FILOUT )
                 CALL EXIT ( 1 )
            END IF
            WRITE ( 6, '(A)' ) 'Written output file '//FILOUT(1:I_LEN(FILOUT))
            IF ( ILEN(COMPR_COM) .NE. 0 ) THEN
!
! -------------- Now compress the output file 
!
                 COM_STR = COMPR_COM(1:I_LEN(COMPR_COM))//' '//FILOUT(1:I_LEN(FILOUT))
                 CALL SYSTEM ( COM_STR(1:I_LEN(COM_STR))//CHAR(0) )
            END IF
 450     CONTINUE 
 440  CONTINUE 
!
      CALL EXIT ( 0 )
      END  PROGRAM  GEOS_SLV_TO_HEB  !#!#
