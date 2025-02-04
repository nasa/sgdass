      PROGRAM    LWS_GEN_LS_MASK_MAIN
! ************************************************************************
! *                                                                      *
! *   Routine LWS_GEN_LS_MAIN  generage land-water mask for land water   *
! *   mass storage loading computation. It finds glaciers as pisels with *
! *   anomalous rate: over 500Pa/yr and sets them as zero (water).       *
! *                                                                      *
! * ### 24-APR-2013 LWS_GEN_LS_MAIN  v1.3  (c) L. Petrov 08-MAY-2015 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'heb.i'
      INCLUDE   'malo_local.i'
      TYPE     ( HEB__TYPE  ) :: HEB_MOD, HEB_LSM, HEB_MASK
      CHARACTER  FIL_MOD*128, FIL_LSM*128, FIL_OUT*128, STR*128
      INTEGER*4  MODE, IK, IUER
      LOGICAL*1  FL_PLOT
      INTEGER*4, EXTERNAL :: ILEN, I_LEN
!
!  $MALO_DIR/bin/lws_gen_ls_mask $MALO_DIR/share/merra2_twland_spr_model_1980_2016_d2699.heb /progs/malo_20151228/share/fls_mask_d2699.heb /tmp/lws_merra2_mask_d2699.heb
!
      FL_PLOT = .TRUE.
      MODE = 2
      IF ( IARGC() < 3 ) THEN
           WRITE ( 6, * ) 'Usage: lws_gen_ls_mask  fil_mod fil_lsm fil_out' 
           CALL EXIT ( 1 )
         ELSE
           CALL GETARG ( 1, FIL_MOD )
           CALL GETARG ( 2, FIL_LSM )
           CALL GETARG ( 3, FIL_OUT )
      END IF
!
      IF ( FIL_MOD(1:1) .NE. '/' ) FIL_MOD = MALO_SHARE//'/'//FIL_MOD
      IF ( FIL_LSM(1:1) .NE. '/' ) FIL_LSM = MALO_SHARE//'/'//FIL_LSM
!
      IUER = -1
      CALL READ_HEB ( FIL_MOD, HEB_MOD, IUER )
      IF ( IUER .NE. 0 ) THEN
           IUER = -2
           CALL ERR_LOG ( 6201, IUER, 'LWS_GEN_LS_MAIN', &
     &         'Failure in an attempt to read and parse input file '// &
     &          FIL_MOD(1:I_LEN(FIL_MOD))//' in heb format' )
           CALL EXIT ( 1 )
      END IF
!
      IUER = -1
      CALL READ_HEB ( FIL_LSM, HEB_LSM, IUER ) 
      IF ( IUER .NE. 0 ) THEN
           IUER = -2
           CALL ERR_LOG ( 6202, IUER, 'LWS_GEN_LS_MAIN', 'Error '// &
     &         'in an attempt to read heb-file with land-sea '// &
     &         'mask '//FIL_LSM )
           CALL EXIT ( 1 )
      END IF   
      IK = HEB_LSM%DIMS(1)/HEB_MOD%DIMS(1)
      IF ( HEB_LSM%DIMS(1) .NE. IK*HEB_MOD%DIMS(1) ) THEN
           IUER = -2
           CALL ERR_LOG ( 6203, IUER, 'LWS_GEN_LS_MAIN', 'Trap of internal '// &
     &         'control: dimension of the model mask should be '// &
     &         'an integer divisor of the land-sea mask dimension' )
           CALL EXIT ( 1 )
      END IF
!
      IUER = -1
      CALL LWS_GEN_LS ( MODE, HEB_MOD, HEB_LSM, HEB_MASK, FL_PLOT, IUER )
      IF ( IUER .NE. 0 ) THEN
           IUER = -2
           CALL ERR_LOG ( 6204, IUER, 'LWS_GEN_LS_MAIN', 'Error '// &
     &         'in an attempt to generate mask for GLDAS data' )
           CALL EXIT ( 1 )
      END IF   
      HEB_MASK%FILE_NAME = FIL_OUT
!
      IUER = -1
      CALL WRITE_HEB ( HEB_MASK, HEB_MASK%VAL, FIL_OUT, IUER )
      IF ( IUER .NE. 0 ) THEN
           IUER = -2
           CALL ERR_LOG ( 6205, IUER, 'LWS_GEN_LS_MAIN', 'Error '// &
     &         'in an attempt to write down GLDAS mask into output file '// &
     &          FIL_OUT )
           CALL EXIT ( 1 )
      END IF   
      WRITE ( 6, '(A)' ) 'Wrote output file '//FIL_OUT(1:I_LEN(FIL_OUT))
      END  !#!  
!
! ------------------------------------------------------------------------
!
      SUBROUTINE LWS_GEN_LS ( MODE, HEB_MOD, HEB_LSM, HEB_MASK, FL_PLOT, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine LWS_GEN_LS
! *                                                                      *
! * ### 24-APR-2013    LWS_GEN_LS    v2.0 (c)  L. Petrov 24-MAR-2016 ### *
! *                                                                      *
! ************************************************************************
      INCLUDE   'astro_constants.i'
      INCLUDE   'heb.i'
      TYPE     ( HEB__TYPE  ) :: HEB_MOD, HEB_LSM, HEB_MASK
      INTEGER*4  MODE, IUER
      LOGICAL*1  FL_PLOT
      REAL*8     PRES_MIN, PRES_MAX
      PARAMETER  ( PRES_MIN      =    0.01D0 )
      PARAMETER  ( PRES_MAX      = 24000.0D0 )
      PARAMETER  ( PRES_RATE_MAX =   800.0D0 ) ! 400 for merra, 500 for noah025, 800 for geosfpit
      INTEGER*4  J1, J2, J3, J4, J5, J6, J7, J8, ILAT, ILON, IER
      REAL*8     LAT, LON
      REAL*8       LAT_GLA_MIN, LAT_GLA_MAX, LON_GLA_MIN, LON_GLA_MAX, PRS_GLA_MAX
      REAL*8       LAT_GRE_MIN, LAT_GRE_MAX, LOM_GRE_MIN, LON_GRE_MAX, PRS_GRE_MAX
      PARAMETER  ( LAT_GLA_MIN = 60.0D0*DEG__TO__RAD ) 
      PARAMETER  ( LAT_GLA_MAX = 62.0D0*DEG__TO__RAD ) 
      PARAMETER  ( LON_GLA_MIN = 215.0D0*DEG__TO__RAD ) 
      PARAMETER  ( LON_GLA_MAX = 225.0D0*DEG__TO__RAD ) 
      PARAMETER  ( PRS_GLA_MAX = 115000.0D0 ) 
!
      PARAMETER  ( LAT_GRE_MIN = 64.0D0*DEG__TO__RAD ) 
      PARAMETER  ( LAT_GRE_MAX = 70.0D0*DEG__TO__RAD ) 
      PARAMETER  ( LON_GRE_MIN = 316.0D0*DEG__TO__RAD ) 
      PARAMETER  ( LON_GRE_MAX = 336.0D0*DEG__TO__RAD ) 
      PARAMETER  ( PRS_GRE_MAX = 120000.0D0 ) 
      CHARACTER  STR*128
      CHARACTER, EXTERNAL :: GET_CDATE*19
      INTEGER*4, EXTERNAL :: ILEN, I_LEN 
!
! --- Set HEB_MASK data structure
!
      HEB_MASK%SDS_NAME         = 'Land-water-glacier [0.0,1.0] -- a fraction of land'
      HEB_MASK%UNITS            = 'n/a'
      HEB_MASK%PROD_NAME        = 'Land-water-glacier mask'
      HEB_MASK%FILE_NAME        = ' '
      HEB_MASK%HISTORY          = ' '
      HEB_MASK%SOURCE           = 'Land-water-glacier mask'
      HEB_MASK%TITLE            = 'Land-water-glacier mask'
      HEB_MASK%INSTITUTION      = 'Astrogeo Center'
      HEB_MASK%REFERENCES       = 'n/a'
      HEB_MASK%PROD_DATE_TIME   = GET_CDATE()
      HEB_MASK%VERSION_ID       = '1'
      HEB_MASK%MJD              = HEB_LSM%MJD
      HEB_MASK%UTC              = HEB_LSM%UTC
      HEB_MASK%TAI              = HEB_LSM%TAI
      HEB_MASK%DIMS             = HEB_LSM%DIMS
      HEB_MASK%DATA_OFFSET      = HEB_LSM%DATA_OFFSET
      HEB_MASK%DATA_LENGTH      = HEB_LSM%DATA_LENGTH
      HEB_MASK%DATA_FORMAT      = HEB_LSM%DATA_FORMAT
      HEB_MASK%DATA_TRANSFORM   = HEB_LSM%DATA_TRANSFORM
      HEB_MASK%ENDIAN           = HEB_LSM%ENDIAN
      HEB_MASK%MIN_VALUE        = 0.0
      HEB_MASK%MAX_VALUE        = 1.0
      HEB_MASK%FILL_VALUE       = HEB_LSM%FILL_VALUE
      HEB_MASK%VALID_RANGE(1)   = 0.0
      HEB_MASK%VALID_RANGE(2)   = 1.0
      HEB_MASK%OFFSET           = HEB_LSM%OFFSET
      HEB_MASK%SCALE_FACTOR     = HEB_LSM%SCALE_FACTOR
      HEB_MASK%DATA_COMPRESSION = HEB_LSM%DATA_COMPRESSION
!
      ALLOCATE ( HEB_MASK%VAL(HEB_MASK%DIMS(1),HEB_MASK%DIMS(2),1,1), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH  ( STR )
           CALL IINCH8 ( INT8(4)*HEB_MASK%DIMS(1)*HEB_MASK%DIMS(2), STR )
           CALL ERR_LOG ( 6211, IUER, 'LWS_GEN_LS', 'Failure '// &
     &         'to allocate '//STR(1:I_LEN(STR))//' bytes of dynamic memory '// &
     &         'for arrea HEB_MASK' )
           RETURN 
      END IF
!
      IF ( MODE == 1 ) THEN
           DO 410 J1=1,HEB_MOD%DIMS(2)
              DO 420 J2=1,HEB_MOD%DIMS(1)
                 HEB_MASK%VAL(J2,J1,1,1) = HEB_LSM%VAL(J2,J1,1,1) 
                 IF ( HEB_MOD%VAL(J2,J1,1,1) > PRES_MAX ) THEN
                      HEB_MASK%VAL1(J2,J1,1,1) = 0.0D0
                 END IF
                 IF ( ABS(HEB_MOD%VAL(J2,J1,2,1)) > PRES_RATE_MAX ) THEN
                      HEB_MASK%VAL1(J2,J1,1,1) = 0.0D0 
                 END IF
                 IF ( HEB_MOD%VAL(J2,J1,1,1) < PRES_MIN ) THEN
                      HEB_MASK%VAL1(J2,J1,1,1) = 0.0D0
                 END IF
 420          CONTINUE 
 410       CONTINUE 
         ELSE IF ( MODE == 2 ) THEN
           HEB_MASK%VAL = 1.0         
           IK = HEB_LSM%DIMS(1)/HEB_MOD%DIMS(1)
           DO 430 J3=1,HEB_MASK%DIMS(2)
              LAT = (J3-1)*PI__NUM/(HEB_MASK%DIMS(2)-1) - P2I
              ILAT = NINT( (HEB_MOD%DIMS(2)-1)*(LAT + P2I)/PI__NUM ) +  1
              IF ( ILAT  > HEB_MOD%DIMS(2) ) ILAT = HEB_MOD%DIMS(2)
              DO 440 J4=1,HEB_MASK%DIMS(1)
                 HEB_MASK%VAL(J4,J3,1,1) = 1.0
                 LON = (J4-1)*PI2/HEB_MASK%DIMS(1)
                 ILON = NINT( HEB_MOD%DIMS(1)*LON/PI2 ) +  1
                 IF ( ILON > HEB_MOD%DIMS(1) ) ILON = ILON - HEB_MOD%DIMS(1) 
!
                 IF ( HEB_LSM%VAL(J4,J3,1,1) > 0.5 .AND. HEB_MOD%VAL(ILON,ILAT,1,1) < 0.5 ) THEN
                      HEB_MASK%VAL(J4,J3,1,1) = 0.0
                 END IF
                 IF ( LAT > LAT_GLA_MIN .AND. LAT < LAT_GLA_MAX  .AND. &
     &                LON > LON_GLA_MIN .AND. LON < LON_GLA_MAX  .AND. &
     &                HEB_MOD%VAL(ILON,ILAT,1,1)  > PRS_GLA_MAX        ) THEN
                      HEB_MASK%VAL(J4,J3,1,1) = 0.0
                 END IF
                 IF ( LAT > LAT_GRE_MIN .AND. LAT < LAT_GRE_MAX  .AND. &
     &                LON > LON_GRE_MIN .AND. LON < LON_GRE_MAX  .AND. &
     &                HEB_MOD%VAL(ILON,ILAT,1,1)  > PRS_GRE_MAX        ) THEN
                      HEB_MASK%VAL(J4,J3,1,1) = 0.0
                 END IF
 440          CONTINUE 
 430       CONTINUE 
      END IF
!
      IF ( FL_PLOT ) THEN
           CALL ERR_PASS ( IUER, IER )
           CALL PLOT_GRID_R4 ( 1, 7, 1, 1, INT(HEB_MASK%DIMS(1),KIND=4), INT(HEB_MASK%DIMS(2),KIND=4), &
     &                    HEB_MASK%VAL, 'HEB_MASK', 'd/l', &
     &                    '/tmp/foo', IER )
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  LWS_GEN_LS  !#!#
