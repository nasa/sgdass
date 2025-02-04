#include <mk5_preprocessor_directives.inc>
      PROGRAM    SPD_RESP
! ************************************************************************
! *                                                                      *
! *   Program SPD_RESP reads the 3D state of the atmsosphere derived     *
! *   from the output of a numerical weather model at the specfied       *
! *   date, regrids it into the regular grid, computes the refractivity  *
! *   field, expends it into the 3D B-spline basis, and writes the       *
! *   coefficients of the expansion in HEB format into the specified     *
! *   directory.                                                         *
! *                                                                      *
! *  ### 22-SEP-2014    SPD_RESP   v2.3 (c)  L. Petrov  15-SEP-2021 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'malo.i'
      INCLUDE   'spd.i'
      INCLUDE   'spd_local.i'
      INCLUDE   'heb.i'
      TYPE     ( SPD_3D__TYPE ) :: SPD
      TYPE     ( HEB__TYPE    ) :: HEB_GEOID_BSPL, HEB_OH, HEB_OUT, &
     &                             HEB_D, HEB_Q, HEB_T, HEB_REFR, &
     &                             HEB_PRES, HEB_TEMP, HEB_PPWV
      CHARACTER  FIL_MET*128, FILD*128, FILT*128, FILQ*128, SUB_DIR*128, &
     &           DIR_OUT*128, DIR_YEAR_OUT*128, FIL_GEOID*128, &
     &           FIL_OH*128, FIL_REFR_OUT*128, &
     &           FIL_PRES_OUT*128, FIL_TEMP_OUT*128, FIL_PPWV_OUT*128, &
     &           SYSNAME*128, NODENAME*128, HARDWARE*128, FIL_LEAPSEC*128, &
     &           COMPR*16, COMPR_COM*64, COM_STR*256, STR*128
      LOGICAL*1  LEX
      REAL*8     TRIAL_GEO
      REAL*4,    ALLOCATABLE :: DATA_R4(:,:,:,:)
      LOGICAL*1  FL_R4
      INTEGER*8  DIR_DESC, MEL
      INTEGER*2  MASK_I2
      DATA       MASK_I2  / O'775' /
      REAL*8       CNS_DR2(2)
      INTEGER*4  IL, ID, IP, IS, DIMO(3), IVRB, IUER
      CHARACTER, EXTERNAL :: GET_CDATE*19
      INTEGER*8, EXTERNAL :: FUNC_OPENDIR
      INTEGER*4, EXTERNAL :: ILEN, I_LEN, CLOSEDIR, LINDEX, MKDIR
      REAL*8,    EXTERNAL :: GET_GEOID 
!
      IF ( IARGC() < 10 ) THEN
           IF ( IARGC() .GE. 1 ) THEN
                CALL GETARG ( 1, FIL_MET )
                IF ( FIL_MET == 'version'   .OR.  &
     &               FIL_MET == '--version' .OR.  &
     &               FIL_MET == '-v' ) THEN
!
                     WRITE ( 6, '(A)' ) SPD__VERSION
                     CALL EXIT ( 0 )
                 END IF
           END IF
           WRITE ( 6, '(A)' ) 'Usage: spd_resp met_file out_dir fil_geoid fil_oh '// &
     &                        ' dim1 dim2 dim3 cns_v cns_h [verbosity] [compress]'
           CALL EXIT ( 1 )
         ELSE
           CALL GETARG ( 1, FIL_MET   )
           CALL GETARG ( 2, DIR_OUT   )
           CALL GETARG ( 3, FIL_GEOID )
           CALL GETARG ( 4, FIL_OH    )
           CALL GETARG ( 5, STR       )
           CALL CHIN   ( STR, DIMO(1) )
           CALL GETARG ( 6, STR       )
           CALL CHIN   ( STR, DIMO(2) )
           CALL GETARG ( 7, STR       )
           CALL CHIN   ( STR, DIMO(3) )
           CALL GETARG ( 8, STR       )
           READ ( UNIT=STR, FMT='(F20.10)' ) CNS_DR2(1)
           CALL GETARG ( 9, STR       )
           READ ( UNIT=STR, FMT='(F20.10)' ) CNS_DR2(2)
!
           IF ( IARGC() .GE. 10 ) THEN
                CALL GETARG ( 10, STR )
                CALL CHIN   ( STR, IVRB )
              ELSE 
                IVRB = 1
           END IF
           IF ( IARGC() .GE. 11 ) THEN
                CALL GETARG ( 11, COMPR )
              ELSE 
                COMPR = 'none'
           ENDIF
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
               CALL ERR_LOG ( 5401, IUER, 'SPD_RESP', 'Unsupported '// &
     &             'compression method: '//COMPR//' . Supported methods: '// &
     &             ' none gzip bzip2 pbzip2 pbzip2_p1' )
               CALL EXIT ( 1 )
           END IF
      END IF
      IF ( DIMO(1) == 0 ) THEN
           FL_R4 = .TRUE.
         ELSE
           FL_R4 = .FALSE.
      END IF
!
      IL = ILEN(FIL_MET)
      IF ( IL < 26 ) THEN
           IUER = -1
           CALL ERR_LOG ( 5402, IUER, 'SPD_RESP', 'Wrong name (too short) '// &
     &         'of the input meteorological file '//FIL_MET )
           CALL EXIT ( 1 )
      END IF
!
      IF ( DIR_OUT(ILEN(DIR_OUT):ILEN(DIR_OUT)) == '/' ) THEN
           DIR_OUT(ILEN(DIR_OUT):ILEN(DIR_OUT)) = ' '
      END IF
!    
      IF ( FIL_MET(IL-7:IL) == '.heb.bz2' ) THEN
           FILD = FIL_MET(1:IL-25)//'d/d'//FIL_MET(IL-21:IL)
           FILT = FIL_MET(1:IL-25)//'t/t'//FIL_MET(IL-21:IL)
           FILQ = FIL_MET(1:IL-25)//'q/q'//FIL_MET(IL-21:IL)
           DIR_YEAR_OUT = DIR_OUT(1:I_LEN(DIR_OUT))//'/'// &
     &                    FIL_MET(IL-20:IL-17)
           FIL_REFR_OUT = DIR_YEAR_OUT(1:I_LEN(DIR_YEAR_OUT))//'/refr/refr_'// &
     &                    FIL_MET(IL-20:IL-8)//'.heb'
           FIL_PRES_OUT = DIR_YEAR_OUT(1:I_LEN(DIR_YEAR_OUT))//'/pres/pres_'// &
     &                    FIL_MET(IL-20:IL-8)//'.heb'
           FIL_TEMP_OUT = DIR_YEAR_OUT(1:I_LEN(DIR_YEAR_OUT))//'/temp/temp_'// &
     &                    FIL_MET(IL-20:IL-8)//'.heb'
           FIL_PPWV_OUT = DIR_YEAR_OUT(1:I_LEN(DIR_YEAR_OUT))//'/ppwv/ppwv_'// &
     &                    FIL_MET(IL-20:IL-8)//'.heb'
        ELSE IF ( FIL_MET(IL-3:IL) == '.heb' ) THEN
           FILD = FIL_MET(1:IL-21)//'d/d'//FIL_MET(IL-17:IL)
           FILT = FIL_MET(1:IL-21)//'t/t'//FIL_MET(IL-17:IL)
           FILQ = FIL_MET(1:IL-21)//'q/q'//FIL_MET(IL-17:IL)
           DIR_YEAR_OUT = DIR_OUT(1:I_LEN(DIR_OUT))//'/'// &
     &                    FIL_MET(IL-16:IL-13)
           FIL_REFR_OUT = DIR_YEAR_OUT(1:I_LEN(DIR_YEAR_OUT))//'/refr/refr_'// &
     &                    FIL_MET(IL-16:IL-4)//'.heb'
           FIL_PRES_OUT = DIR_YEAR_OUT(1:I_LEN(DIR_YEAR_OUT))//'/pres/pres_'// &
     &                    FIL_MET(IL-16:IL-4)//'.heb'
           FIL_TEMP_OUT = DIR_YEAR_OUT(1:I_LEN(DIR_YEAR_OUT))//'/temp/temp_'// &
     &                    FIL_MET(IL-16:IL-4)//'.heb'
           FIL_PPWV_OUT = DIR_YEAR_OUT(1:I_LEN(DIR_YEAR_OUT))//'/ppwv/ppwv_'// &
     &                    FIL_MET(IL-16:IL-4)//'.heb'
        ELSE 
           IUER = -1
           CALL ERR_LOG ( 5403, IUER, 'SPD_RESP', 'Wrong extension of '// &
     &         'the input meteorological file '//FIL_MET(1:I_LEN(FIL_MET))// &
     &         ' -- extensions .heb or .heb.bz2 were expected' )
           CALL EXIT ( 1 )
      END IF
!
      INQUIRE ( FILE=FILD, EXIST=LEX ) 
      IF ( .NOT. LEX ) THEN
           IUER = -1
           CALL ERR_LOG ( 5404, IUER, 'SPD_RESP', 'Cannot find input file '// &
     &         'with pressure thickness '//FILD )
           CALL EXIT ( 1 )
      END IF
!
      INQUIRE ( FILE=FILT, EXIST=LEX ) 
      IF ( .NOT. LEX ) THEN
           IUER = -1
           CALL ERR_LOG ( 5405, IUER, 'SPD_RESP', 'Cannot find input file '// &
     &         'air temperature '//FILT )
           CALL EXIT ( 1 )
      END IF
!
      INQUIRE ( FILE=FILQ, EXIST=LEX ) 
      IF ( .NOT. LEX ) THEN
           IUER = -1
           CALL ERR_LOG ( 5406, IUER, 'SPD_RESP', 'Cannot find input file '// &
     &         'with specific humidity '//FILQ )
           CALL EXIT ( 1 )
      END IF
!
      IUER = -1
      CALL SPD_CHECK_SHARE_FILE ( FIL_GEOID, IUER )
      IF ( IUER .NE. 0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 5407, IUER, 'SPD_RESP', 'Cannot find input file '// &
     &         'with geoid height '//FIL_GEOID )
           CALL EXIT ( 1 )
      END IF
!
      IUER = -1
      CALL SPD_CHECK_SHARE_FILE ( FIL_OH, IUER )
      IF ( IUER .NE. 0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 5408, IUER, 'SPD_RESP', 'Cannot find input file '// &
     &         'with orthoheights '//FIL_OH )
           CALL EXIT ( 1 )
      END IF
!
      IUER = -1
      FIL_LEAPSEC = MALO__LEAPSEC_FILE
      CALL SPD_CHECK_SHARE_FILE ( FIL_LEAPSEC, IUER )
      IF ( IUER .NE. 0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 5409, IUER, 'SPD_RESP', 'Wrong 5th argument: '// &
     &         'leap second file '//FIL_LEAPSEC )
           CALL EXIT ( 1 )
      END IF
!
! --- Check whether the output directory exists
!
      DIR_DESC = FUNC_OPENDIR ( DIR_OUT(1:I_LEN(DIR_OUT))//CHAR(0) )
      IF ( DIR_DESC == 0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 5410, IUER, 'SPD_RESP', 'Wrong 2nd argument: '// &
     &          DIR_OUT(1:I_LEN(DIR_OUT))//' -- the path '// &
     &          DIR_OUT(1:ID)//' does not exist' )
           CALL EXIT ( 1 )
         ELSE
           IP = CLOSEDIR ( %VAL(DIR_DESC) )
      END IF
!
! --- Check whether subdirectories exist. If not, create them
!
      DIR_DESC = FUNC_OPENDIR ( DIR_YEAR_OUT(1:I_LEN(DIR_YEAR_OUT))//CHAR(0) )
      IF ( DIR_DESC == 0 ) THEN
           IS = MKDIR ( DIR_YEAR_OUT(1:I_LEN(DIR_YEAR_OUT))//CHAR(0), &
     &                  %VAL(MASK_I2) )
           IF ( IS .NE. 0 ) THEN
                CALL CLRCH  ( STR )
                CALL GERROR ( STR )
                IUER = -1
                CALL ERR_LOG ( 5411, IUER, 'SPD_RESP', 'Failure to '// &
     &              'create subdirectory '//DIR_YEAR_OUT(1:I_LEN(DIR_YEAR_OUT))// &
     &              ' -- '//STR )
                CALL EXIT ( 1 )
          END IF
        ELSE
           IP = CLOSEDIR ( %VAL(DIR_DESC) )
      END IF
!
! --- Subdirectory with air refractivity
!
      SUB_DIR = DIR_YEAR_OUT(1:I_LEN(DIR_YEAR_OUT))//'/refr'
      DIR_DESC = FUNC_OPENDIR ( SUB_DIR(1:I_LEN(SUB_DIR))//CHAR(0) )
      IF ( DIR_DESC == 0 ) THEN
           IS = MKDIR ( SUB_DIR(1:I_LEN(SUB_DIR))//CHAR(0), %VAL(MASK_I2) )
           IF ( IS .NE. 0 ) THEN
                CALL CLRCH  ( STR )
                CALL GERROR ( STR )
                IUER = -1
                CALL ERR_LOG ( 5412, IUER, 'SPD_RESP', 'Failure to '// &
     &              'create subdirectory '//SUB_DIR(1:I_LEN(SUB_DIR))// &
     &              ' -- '//STR )
                CALL EXIT ( 1 )
          END IF
        ELSE
           IP = CLOSEDIR ( %VAL(DIR_DESC) )
      END IF
!
! --- Subdirectory with atmospheric pressure
!
      SUB_DIR = DIR_YEAR_OUT(1:I_LEN(DIR_YEAR_OUT))//'/pres'
      DIR_DESC = FUNC_OPENDIR ( SUB_DIR(1:I_LEN(SUB_DIR))//CHAR(0) )
      IF ( DIR_DESC == 0 ) THEN
           IS = MKDIR ( SUB_DIR(1:I_LEN(SUB_DIR))//CHAR(0), %VAL(MASK_I2) )
           IF ( IS .NE. 0 ) THEN
                CALL CLRCH  ( STR )
                CALL GERROR ( STR )
                IUER = -1
                CALL ERR_LOG ( 5413, IUER, 'SPD_RESP', 'Failure to '// &
     &              'create subdirectory '//SUB_DIR(1:I_LEN(SUB_DIR))// &
     &              ' -- '//STR )
                CALL EXIT ( 1 )
          END IF
        ELSE
           IP = CLOSEDIR ( %VAL(DIR_DESC) )
      END IF
!
! --- Subdirectory with air temperature
!
      SUB_DIR = DIR_YEAR_OUT(1:I_LEN(DIR_YEAR_OUT))//'/temp'
      DIR_DESC = FUNC_OPENDIR ( SUB_DIR(1:I_LEN(SUB_DIR))//CHAR(0) )
      IF ( DIR_DESC == 0 ) THEN
           IS = MKDIR ( SUB_DIR(1:I_LEN(SUB_DIR))//CHAR(0), %VAL(MASK_I2) )
           IF ( IS .NE. 0 ) THEN
                CALL CLRCH  ( STR )
                CALL GERROR ( STR )
                IUER = -1
                CALL ERR_LOG ( 5414, IUER, 'SPD_RESP', 'Failure to '// &
     &              'create subdirectory '//SUB_DIR(1:I_LEN(SUB_DIR))// &
     &              ' -- '//STR )
                CALL EXIT ( 1 )
          END IF
        ELSE
           IP = CLOSEDIR ( %VAL(DIR_DESC) )
      END IF
!
! --- Subdirectory with partial pressure of water vapor
!
      SUB_DIR = DIR_YEAR_OUT(1:I_LEN(DIR_YEAR_OUT))//'/ppwv'
      DIR_DESC = FUNC_OPENDIR ( SUB_DIR(1:I_LEN(SUB_DIR))//CHAR(0) )
      IF ( DIR_DESC == 0 ) THEN
           IS = MKDIR ( SUB_DIR(1:I_LEN(SUB_DIR))//CHAR(0), %VAL(MASK_I2) )
           IF ( IS .NE. 0 ) THEN
                CALL CLRCH  ( STR )
                CALL GERROR ( STR )
                IUER = -1
                CALL ERR_LOG ( 5415, IUER, 'SPD_RESP', 'Failure to '// &
     &              'create subdirectory '//SUB_DIR(1:I_LEN(SUB_DIR))// &
     &              ' -- '//STR )
                CALL EXIT ( 1 )
          END IF
        ELSE
           IP = CLOSEDIR ( %VAL(DIR_DESC) )
      END IF
!
      IUER = -1
      CALL SPD_INIT ( SPD, IUER )
      IF ( IVRB .GE. 2 ) THEN
           WRITE ( 6, * ) 'SPD_RESP: Read input data files'
      END IF
!
! --- Read heb file with atmosphere layer thickness
!
      IUER = -1
      CALL READ_HEB ( FILD, HEB_D, IUER )
      IF ( IUER .NE. 0 ) THEN
           CALL ERR_LOG ( 5416, IUER, 'SPD_RESP', 'Error in reading '// &
     &         'the input file with atmosphere layer thickness '//FILD )
           CALL EXIT ( 1 )
      END IF
      SPD%MJD = HEB_D%MJD
      SPD%UTC = HEB_D%UTC
      SPD%TAI = HEB_D%TAI
      SPD%CONF%BSPL_3WAV = SPD__YES
      SPD%NTYP = SPD__MWAV     
      SPD%CNS_DR2_SIG = CNS_DR2
!
! --- Read heb file with air temperature
!
      IUER = -1
      CALL READ_HEB ( FILT, HEB_T, IUER )
      IF ( IUER .NE. 0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 5417, IUER, 'SPD_RESP', 'Error in reading '// &
     &         'the input file with air temperature '//FILT )
           CALL EXIT ( 1 )
      END IF
!
! --- Read heb file with specific humidity
!
      IUER = -1
      CALL READ_HEB ( FILQ, HEB_Q, IUER )
      IF ( IUER .NE. 0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 5418, IUER, 'SPD_RESP', 'Error in reading '// &
     &         'the input file with specific humidity '//FILQ )
           CALL EXIT ( 1 )
      END IF
      IF ( IVRB .GE. 2 ) THEN
           WRITE ( 6, * ) 'SPD_RESP: Read input auxiliary data'
      END IF
!
! --- Read file with surface ortho-height
!
      IUER = -1
      CALL READ_HEB ( FIL_OH, HEB_OH, IUER )
      IF ( IUER .NE. 0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 5419, IUER, 'SPD_RESP', 'Error in reading '// &
     &         'the input file with heights above geoid '//FIL_OH )
           CALL EXIT ( 1 )
      END IF
!
      IUER = -1 
      TRIAL_GEO = GET_GEOID ( 0.0D0, 0.0D0, FIL_GEOID, HEB_GEOID_BSPL, IUER )
      IF ( IUER .NE. 0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 5420, IUER, 'SPD_RESP', 'Error in an attempt to load '// &
     &         'geoid height data from file '//FIL_GEOID )
           CALL EXIT ( 1 )
      END IF
!
      SPD%CONF%FIL_LEAPSEC = FIL_LEAPSEC
      IUER = -1
      CALL SPD_LOAD_LEAPSEC ( SPD, IUER )
      IF ( IUER .NE. 0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 5421, IUER, 'SPD_RESP', 'Error in an attempt '// &
     &         'to load leapsecond data from file '//FIL_LEAPSEC )
           CALL EXIT ( 1 )
      END IF
      IF ( IVRB .GE. 2 ) THEN
           WRITE ( 6, * ) 'SPD_RESP: Compute refractivity'
      END IF
!
      ALLOCATE ( SPD%MASK(HEB_D%DIMS(1),HEB_D%DIMS(2)), STAT=IUER )
      IF ( IUER .NE. 0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 5422, IUER, 'SPD_RESP', 'Error in an attempt '// &
     &         'to allocate SPD%MASK array' )
           RETURN 
      END IF
      SPD%MASK = .TRUE.
!
      IUER = -1
      CALL SPD_3D_REFRA ( SPD, HEB_D, HEB_T, HEB_Q, HEB_OH, HEB_GEOID_BSPL, &
     &                    DIMO, CNS_DR2, IVRB, IUER )
      IF ( IUER .NE. 0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 5423, IUER, 'SPD_RESP', 'Error in an attempt '// &
     &         'to compute the refractivity field and expand it into '// &
     &         'B-spline basis' )
           CALL EXIT ( 1 )
      END IF
      IF ( IVRB .GE. 2 ) THEN
           WRITE ( 6, * ) 'SPD_RESP: Write down results' 
      END IF
!
      ALLOCATE ( DATA_R4(1-SPD__MDEG:SPD%NLEV,1-SPD__MDEG:SPD%NLON,1-SPD__MDEG:SPD%NLAT,SPD%NTYP), &
     &           STAT=IUER )
      IF ( IUER .NE. 0 ) THEN
           MEL = INT8(SPD%NLEV+SPD__MDEG)*INT8(SPD%NLON+SPD__MDEG)*INT8(SPD%NLAT+SPD__MDEG)*INT8(SPD%NTYP)
           CALL CLRCH  ( STR )
           CALL IINCH8 ( INT8(4)*MEL, STR )
           CALL ERR_LOG ( 5424, IUER, 'SPD_RESP', 'Failure in an attempt '// &
     &         'to allocate '//STR(1:I_LEN(STR))//' bytes of dynamic memory '// &
     &         'for array DATA_R4' )
           CALL EXIT ( 1 )
      END IF
!
      DATA_R4 = 0.0
      IF ( FL_R4 ) THEN
           CALL SPD_R8_TO_R4 ( INT8((SPD%NLEV+SPD__MDEG)*(SPD%NLON+SPD__MDEG)*(SPD%NLAT+SPD__MDEG)*SPD%NTYP), &
     &                         SPD%REF_3D, DATA_R4 )
      END IF
!
! --- Learn the Internet name of the current computer
!
      CALL GETINFO_SYSTEM ( SYSNAME, NODENAME, HARDWARE )
!
! --- Generate the header of refractivity data
!
      HEB_REFR           = HEB_D
      HEB_REFR%DIMS(1)   = SPD%NLEV+SPD__MDEG
      HEB_REFR%DIMS(2)   = SPD%NLON+SPD__MDEG
      HEB_REFR%DIMS(3)   = SPD%NLAT+SPD__MDEG
      HEB_REFR%DIMS(4)   = SPD%NTYP
      HEB_REFR%TITLE     = '3D air refractivity expansion over B-spline basis at 3 wavelengths'
      HEB_REFR%PROD_NAME = 'Air refractivity derived from '//HEB_D%TITLE 
      HEB_REFR%SDS_NAME  = 'Air refractivity 3D B-spline expansion coefficients'
!
      STR = SPD_RESP_PROG__LABEL(10:15)
      CALL CHASHL ( STR )
      IF ( NODENAME == 'astrogeo'      .OR. &
     &     NODENAME == 'pethome'       .OR. &
     &     NODENAME == 'earthrotation'      ) THEN
           HEB_REFR%INSTITUTION    = 'Astrogeo Center'
         ELSE IF ( INDEX ( NODENAME, 'gsfc.nasa.gov' ) > 0 ) THEN
           HEB_REFR%INSTITUTION    = 'NASA GSFC Code 698'
         ELSE
           HEB_REFR%INSTITUTION    = 'Unknown'
      END IF
!
      HEB_REFR%REFERENCES     = 'http://astrogeo.org/spd and '//HEB_D%REFERENCES
      HEB_REFR%VERSION_ID     = STR(1:I_LEN(STR))
      HEB_REFR%UNITS          = 'dimensionless'
      HEB_REFR%HISTORY        = 'Generated by '//SPD_RESP_PROG__LABEL
      HEB_REFR%PROD_DATE_TIME = GET_CDATE()//'  '//HEB_D%PROD_DATE_TIME
      IF ( FL_R4 ) THEN
           HEB_REFR%DATA_FORMAT    = HEB__R4
         ELSE
           HEB_REFR%DATA_FORMAT    = HEB__R8
      END IF
      HEB_REFR%DATA_TRANSFORM = HEB__NONE
      HEB_REFR%OFFSET         = 0.0
      HEB_REFR%SCALE_FACTOR   = 1.0
      HEB_REFR%FILL_VALUE     = 1.0E15
      HEB_REFR%VALID_RANGE(1) = 1.E-12
      HEB_REFR%VALID_RANGE(2) = 5.E-4
      WRITE ( UNIT=HEB_REFR%COMMENT(1), FMT=210 ) 1-SPD__MDEG, SPD%NLEV, &
     &                                            1-SPD__MDEG, SPD%NLAT, 180.0/(SPD%NLAT-1), &
     &                                            1-SPD__MDEG, SPD%NLON, 360.0/(SPD%NLON-2)
 210  FORMAT ( '1st dimension height: ', I2, ' : ', I3, '; ', &
     &         '2nd dimension lat: ', I2, ' : ', I4, ', step: ', F5.3, ' deg; ' &
     &         '3rd dimension lon: ', I2, ' : ', I4, ', step: ', F5.3, ' deg.'  )
      WRITE ( UNIT=HEB_REFR%COMMENT(2), FMT=220 ) SPD__U1_GMAO72, SPD__U2_GMAO72, SPD__U3_GMAO72
 220  FORMAT ( 'Parameters for the non-uniform height grid: ', 3(F10.5,1X) )
      HEB_REFR%COMMENT(3) = 'The 4th dimension runs over wavelength: 1 -- 532nm, '// &
     &                      '2 -- 1064 nm, 3 -- radio (0.3mm -- 0.3m)'
      WRITE ( UNIT=HEB_REFR%COMMENT(4), FMT=230 ) CNS_DR2(1),  CNS_DR2(2)
 230  FORMAT ( 'Constraints on the second derivative: vertical: ', 1PD10.3, ' horizontal: ', 1PD10.3 )
!
      IF ( FL_R4 ) THEN
           CALL HEB_SETMIN ( HEB_REFR, DATA_R4, 2.0E-12 )
           CALL HEB_MINMAX ( HEB_REFR, DATA_R4, 1.0     )
!
           IUER = -1
           CALL WRITE_HEB ( HEB_REFR, DATA_R4, FIL_REFR_OUT, IUER )
         ELSE
!           CALL HEB_SETMIN_R8 ( HEB_REFR, SPD%REF_3D(-2:SPD%NLEV-1,-2:SPD%NLON-1,-2:SPD%NLAT-1,1:SPD%NTYP), 2.0D-12 )
!           CALL HEB_MINMAX_R8 ( HEB_REFR, SPD%REF_3D(-2:SPD%NLEV-1,-2:SPD%NLON-1,-2:SPD%NLAT-1,1:SPD%NTYP), 1.0D0   )
           CALL HEB_SETMIN_R8_SECT ( HEB_REFR, 1, SPD%NLEV+2, 1, SPD%NLON+2, 1, SPD%NLAT+2, 1, SPD%NTYP, &
     &                               SPD%REF_3D, 2.0D-12 )
           CALL HEB_MINMAX_R8_SECT ( HEB_REFR, 1, SPD%NLEV+2, 1, SPD%NLON+2, 1, SPD%NLAT+2, 1, SPD%NTYP, &
     &                               SPD%REF_3D, 1.0D0   )
           IUER = -1
           CALL WRITE_HEB ( HEB_REFR, SPD%REF_3D, FIL_REFR_OUT, IUER )
      END IF
      IF ( IUER .NE. 0 ) THEN
           CALL GERROR ( STR )
           CALL ERR_LOG ( 5425, IUER, 'SPD_RESP', 'Failure '// &
     &         'in an attempt to write into output file '// &
     &          FIL_REFR_OUT )
           CALL EXIT ( 1 )
      END IF
!
      IF ( FL_R4 ) THEN
           CALL SPD_R8_TO_R4 ( INT8((SPD%NLEV+SPD__MDEG)*(SPD%NLON+SPD__MDEG)*(SPD%NLAT+SPD__MDEG)), &
     &                         SPD%SPR_3D, DATA_R4 )
      END IF
      IF ( ASSOCIATED ( SPD%SPR_3D ) ) THEN
!
! -------- Generate the header of atmospheric pressure data data
!
           HEB_PRES = HEB_REFR
           HEB_PRES%DIMS(1) = SPD%NLEV+SPD__MDEG
           HEB_PRES%DIMS(2) = SPD%NLON+SPD__MDEG
           HEB_PRES%DIMS(3) = SPD%NLAT+SPD__MDEG
           HEB_PRES%DIMS(4) = 1
           HEB_PRES%TITLE     = '3D B-spline expansion of atmosphere pressure'
           HEB_PRES%PROD_NAME = 'Atmospheric pressure derived from '//HEB_D%TITLE 
           HEB_PRES%SDS_NAME  = 'atmosperic pressure 3D B-spline expansion coefficients'
           IF ( FL_R4 ) THEN
                HEB_PRES%DATA_FORMAT    = HEB__R4
              ELSE 
                HEB_PRES%DATA_FORMAT    = HEB__R8
           END IF
           HEB_PRES%DATA_TRANSFORM = HEB__NONE
           HEB_PRES%OFFSET         = 0.0
           HEB_PRES%SCALE_FACTOR   = 1.0
           HEB_PRES%FILL_VALUE     = 1.0D15
           HEB_PRES%VALID_RANGE(1) = 0.1
           HEB_PRES%VALID_RANGE(2) = 130000.
           CALL CLRCH ( HEB_PRES%COMMENT(3) )
           IF ( FL_R4 ) THEN
                CALL HEB_SETMIN ( HEB_PRES, DATA_R4, 0.1      ) 
                CALL HEB_MINMAX ( HEB_PRES, DATA_R4, 130000.0 )
!
                IUER = -1
                CALL WRITE_HEB ( HEB_PRES, DATA_R4, FIL_PRES_OUT, IUER )
              ELSE
                 CALL HEB_SETMIN_R8 ( HEB_PRES, SPD%SPR_3D, 0.1D0      ) 
                 CALL HEB_MINMAX_R8 ( HEB_PRES, SPD%SPR_3D, 130000.0D0 )
!
                 IUER = -1
                 CALL WRITE_HEB ( HEB_PRES, SPD%SPR_3D, FIL_PRES_OUT, IUER )
           END IF
           IF ( IUER .NE. 0 ) THEN
                CALL GERROR ( STR )
                CALL ERR_LOG ( 5426, IUER, 'SPD_RESP', 'Failure '// &
     &              'in an attempt to write into output file '// &
     &               FIL_PRES_OUT )
                CALL EXIT ( 1 )
           END IF
      END IF
!
      IF ( ASSOCIATED ( SPD%STM_3D ) ) THEN
           CALL SPD_R8_TO_R4 ( INT8((SPD%NLEV+SPD__MDEG)*(SPD%NLON+SPD__MDEG)*(SPD%NLAT+SPD__MDEG)), &
     &                         SPD%STM_3D, DATA_R4 )
!
! -------- Generate the header of air temperature data
!
           HEB_TEMP = HEB_REFR
           HEB_TEMP%DIMS(1) = SPD%NLEV+SPD__MDEG
           HEB_TEMP%DIMS(2) = SPD%NLON+SPD__MDEG
           HEB_TEMP%DIMS(3) = SPD%NLAT+SPD__MDEG
           HEB_TEMP%DIMS(4) = 1
           HEB_TEMP%TITLE     = '3D B-spline expansion of air temperature'
           HEB_TEMP%PROD_NAME = 'Air temperature derived from '//HEB_D%TITLE 
           HEB_TEMP%SDS_NAME  = 'air temperature 3D B-spline expansion coefficients'
           HEB_TEMP%DATA_FORMAT    = HEB__I2
           HEB_TEMP%DATA_TRANSFORM = HEB__SCOF
           HEB_TEMP%OFFSET         = 273.0
           HEB_TEMP%SCALE_FACTOR   = 120.0/32000.0
           HEB_TEMP%FILL_VALUE     = 1.0E15
           HEB_TEMP%VALID_RANGE(1) = 150.0
           HEB_TEMP%VALID_RANGE(2) = 360.0
           CALL HEB_MINMAX ( HEB_TEMP, DATA_R4, 360.0 )
           CALL CLRCH ( HEB_TEMP%COMMENT(3) )
!
           IUER = -1
           CALL WRITE_HEB ( HEB_TEMP, DATA_R4, FIL_TEMP_OUT, IUER )
           IF ( IUER .NE. 0 ) THEN
                CALL GERROR ( STR )
                CALL ERR_LOG ( 5427, IUER, 'SPD_RESP', 'Failure '// &
     &              'in an attempt to write into output file '// &
     &               FIL_TEMP_OUT )
                CALL EXIT ( 1 )
           END IF
      END IF
!
      IF ( ASSOCIATED ( SPD%SPW_3D ) ) THEN
           CALL SPD_R8_TO_R4 ( INT8((SPD%NLEV+SPD__MDEG)*(SPD%NLON+SPD__MDEG)*(SPD%NLAT+SPD__MDEG)), &
     &                         SPD%SPW_3D, DATA_R4 )
!
! ------- Generate the header of partial pressure of the water vapor
!
          HEB_PPWV = HEB_REFR
          HEB_PPWV%DIMS(1) = SPD%NLEV+SPD__MDEG
          HEB_PPWV%DIMS(2) = SPD%NLON+SPD__MDEG
          HEB_PPWV%DIMS(3) = SPD%NLAT+SPD__MDEG
          HEB_PPWV%DIMS(4) = 1
          HEB_PPWV%TITLE     = '3D B-spline expansion of partial pressure of water vapor'
          HEB_PPWV%PROD_NAME = 'Water vapor partial pressure derived from '//HEB_D%TITLE 
          HEB_PPWV%SDS_NAME  = 'Water vapor partial pressure B-spline expansion coefficients'
          HEB_PPWV%DATA_FORMAT    = HEB__R4
          HEB_PPWV%DATA_TRANSFORM = HEB__NONE
          HEB_PPWV%OFFSET         = 0.0
          HEB_PPWV%SCALE_FACTOR   = 1.0
          HEB_PPWV%FILL_VALUE     = 1.0E15
          HEB_PPWV%VALID_RANGE(1) = 0.0
          HEB_PPWV%VALID_RANGE(2) = 50000.0
          CALL HEB_MINMAX ( HEB_PPWV, DATA_R4, 50000.0 )
          CALL CLRCH ( HEB_PPWV%COMMENT(3) )
!
          IUER = -1
          CALL WRITE_HEB ( HEB_PPWV, DATA_R4, FIL_PPWV_OUT, IUER )
          IF ( IUER .NE. 0 ) THEN
               CALL GERROR ( STR )
               CALL ERR_LOG ( 5428, IUER, 'SPD_RESP', 'Failure '// &
     &             'in an attempt to write into output file '// &
     &              FIL_PPWV_OUT )
               CALL EXIT ( 1 )
          END IF
      END IF
!
      IF ( ILEN(COMPR_COM) .NE. 0 ) THEN
           IF ( IVRB .GE. 2 ) THEN
                WRITE ( 6, * ) 'SPD_RESP: Compress the output'
           END IF
!
! -------- Now compress the output file 
!
           COM_STR = COMPR_COM(1:I_LEN(COMPR_COM))//' '// &
     &               FIL_REFR_OUT
           CALL SYSTEM ( COM_STR(1:I_LEN(COM_STR))//CHAR(0) )
!
           IF ( ASSOCIATED ( SPD%SPR_3D ) ) THEN
                COM_STR = COMPR_COM(1:I_LEN(COMPR_COM))//' '// &
     &                    FIL_PRES_OUT
                CALL SYSTEM ( COM_STR(1:I_LEN(COM_STR))//CHAR(0) )
           END IF
           IF ( ASSOCIATED ( SPD%STM_3D ) ) THEN
                COM_STR = COMPR_COM(1:I_LEN(COMPR_COM))//' '// &
     &                    FIL_TEMP_OUT
                CALL SYSTEM ( COM_STR(1:I_LEN(COM_STR))//CHAR(0) )
           END IF
           IF ( ASSOCIATED ( SPD%SPW_3D ) ) THEN
                COM_STR = COMPR_COM(1:I_LEN(COMPR_COM))//' '// &
     &                    FIL_PPWV_OUT
                CALL SYSTEM ( COM_STR(1:I_LEN(COM_STR))//CHAR(0) )
           END IF
!
           IF ( IVRB .GE. 1 ) THEN
                WRITE ( 6, * ) 'Output file: '//FIL_REFR_OUT(1:I_LEN(FIL_REFR_OUT))// &
     &                         '.bz2'
                IF ( ASSOCIATED ( SPD%SPR_3D ) ) THEN
                     WRITE ( 6, * ) 'Output file: '//FIL_PRES_OUT(1:I_LEN(FIL_PRES_OUT))// &
     &                              '.bz2'
                END IF
                IF ( ASSOCIATED ( SPD%STM_3D ) ) THEN
                     WRITE ( 6, * ) 'Output file: '//FIL_TEMP_OUT(1:I_LEN(FIL_TEMP_OUT))// &
     &                              '.bz2'
                END IF
                IF ( ASSOCIATED ( SPD%SPW_3D ) ) THEN
                     WRITE ( 6, * ) 'Output file: '//FIL_PPWV_OUT(1:I_LEN(FIL_PPWV_OUT))// &
     &                              '.bz2'
                END IF
           END IF
         ELSE 
           IF ( IVRB .GE. 1 ) THEN
                WRITE ( 6, * ) 'Output file: '//FIL_REFR_OUT(1:I_LEN(FIL_REFR_OUT))
                IF ( ASSOCIATED ( SPD%SPR_3D ) ) THEN
                     WRITE ( 6, * ) 'Output file: '//FIL_PRES_OUT(1:I_LEN(FIL_PRES_OUT))
                END IF
                IF ( ASSOCIATED ( SPD%STM_3D ) ) THEN
                     WRITE ( 6, * ) 'Output file: '//FIL_TEMP_OUT(1:I_LEN(FIL_TEMP_OUT))
                END IF
                IF ( ASSOCIATED ( SPD%SPW_3D ) ) THEN
                     WRITE ( 6, * ) 'Output file: '//FIL_PPWV_OUT(1:I_LEN(FIL_PPWV_OUT))
                END IF
           END IF
      END IF
!
      CALL EXIT ( 0 )
      END  PROGRAM  SPD_RESP  !#!  
