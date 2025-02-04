      PROGRAM    LW_TRANSFORM
! ************************************************************************
! *                                                                      *
! *   Program LW_TRANSFORM
! *                                                                      *
! *  ### 06-JAN-2016  LW_TRANSFORM  v1.0 (c)  L. Petrov  06-JAN-2016 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'malo.i'
      INCLUDE   'heb.i'
      TYPE     ( HEB__TYPE  ) :: HEB_LW, HEB_OUT
      INTEGER*4  IVRB, MODE, IUER
      CHARACTER  FILIN*128, FILLAC*128, FILDAMB*128, FILOUT*128, STR*128
      INTEGER*4, EXTERNAL :: ILEN, I_LEN
!
!      IF ( IARGC() < 3 ) THEN
!           WRITE ( 6, '(A)' ) 'Usage: mode filin filout'
!           CALL EXIT ( 1 )
!         ELSE
!           CALL GETARG ( 1, STR     )
!           CALL GETARG ( 2, FILIN   )
!           CALL GETARG ( 3, FILOUOT )
!      END IF
!
      FILIN   = '/s0/mod44w/mod44w_water.heb'
      FILLAC  = '/progs/malo_20151228/share/lake_list.txt'
      FILDAMB = '/progs/malo_20151228/share/damb_list.txt'
      FILOUT  = '/s0/mod44w/mod44w_ls_coast.heb'
      MODE    = 4
      IVRB    = 3
!
      WRITE ( 6, * ) 'LW_TRANSFORM MODE= ', MODE 
      HEB_OUT%VAL => NULL()
      WRITE ( 6, * ) 'Started READ_HEB ' ; CALL FLUSH ( 6 ) 
      IUER = -1
      CALL READ_HEB ( FILIN, HEB_LW, IUER )
      IF ( IUER .NE. 0 ) THEN
           IUER = -2
           CALL ERR_LOG ( 3801, IUER, 'LW_TRANSFORM', 'Error in reading '// &
     &         'input heb-file '//FILIN )
           CALL EXIT ( 1 )
      END IF
!
      IUER = -1
      CALL APPLY_DAMB ( FILDAMB, HEB_LW, IVRB, IUER )
      IF ( IUER .NE. 0 ) THEN
           IUER = -2
           CALL ERR_LOG ( 3802, IUER, 'LW_TRANSFORM', 'Error in applying '// &
     &         'dambs '//FILIN )
           CALL EXIT ( 1 )
      END IF 
!
      IF ( MODE == 1 ) THEN
           IF ( IVRB > 0 ) THEN
                WRITE ( 6, * ) 'Started LW_TO_LS_MASK '
                CALL FLUSH ( 6 ) 
           END IF
           IUER = -1
           CALL LW_TO_LS_MASK ( HEB_LW, HEB_OUT, IVRB, IUER )
           IF ( IUER .NE. 0 ) THEN
                IUER = -2
                CALL ERR_LOG ( 3803, IUER, 'LW_TRANSFORM', 'Error in computing '// &
     &              'land-sea mask ' )
                CALL EXIT ( 1 )
           END IF 
!
           IUER = -1
           HEB_OUT%SDS_NAME    = 'Land-sea mask at native resolution'
           HEB_OUT%UNITS       = 'd/m'
           HEB_OUT%PROD_NAME   = 'MOD44W'
           HEB_OUT%FILE_NAME   =  FILOUT
           HEB_OUT%HISTORY     = 'Converted from HDF files with modis_lw_gen and with lw_transform'
           HEB_OUT%SOURCE      = 'MODIS/Terra Land Water Mask Derived from MODIS and SRTM L3 Global 250m SIN Grid'
           HEB_OUT%TITLE       = 'Land/Sea mask from MOD44W'
           HEB_OUT%INSTITUTION = 'NASA'
           HEB_OUT%COMMENT(1)  = '1 for land and 0 for inland water, 2 for sea'
           HEB_OUT%REFERENCES  = 'http://landweb.nascom.nasa.gov/cgi-bin/QA_WWW/qaFlagPage?cgi?sat=terra&ver=C5'
           HEB_OUT%PROD_DATE_TIME = '2009-07-31T17:28:14.000Z'
           HEB_OUT%VERSION_ID     = '5.1.0'
           HEB_OUT%MJD            = 51544       
           HEB_OUT%UTC            = 0.0D0
           HEB_OUT%TAI            = 0.0D0
           HEB_OUT%DATA_FORMAT      = HEB__I1
           HEB_OUT%DATA_TRANSFORM   = HEB__NONE 
           HEB_OUT%DATA_COMPRESSION = HEB__NONE 
           HEB_OUT%MIN_VALUE        = 0.0
           HEB_OUT%MAX_VALUE        = 2.0
           HEB_OUT%VALID_RANGE(1)   = 0.0
           HEB_OUT%VALID_RANGE(2)   = 2.0
           HEB_OUT%FILL_VALUE       = -127.0
           HEB_OUT%OFFSET           = 0.0
           HEB_OUT%SCALE_FACTOR     = 1.0
        ELSE IF ( MODE == 2 ) THEN
           IF ( IVRB > 0 ) THEN
                WRITE ( 6, * ) 'Started LW_TO_LL_MASK ' ; CALL FLUSH ( 6 ) 
           END IF
!
           IUER = -1
           CALL LW_TO_LL_MASK ( FILLAC, HEB_LW, HEB_OUT, IVRB, IUER )
           IF ( IUER .NE. 0 ) THEN
                IUER = -2
                CALL ERR_LOG ( 3804, IUER, 'LW_TRANSFORM', 'Error in computing '// &
     &              'land-lake mask ' )
                CALL EXIT ( 1 )
           END IF 
!
           IUER = -1
           HEB_OUT%SDS_NAME    = 'Land-lake mask at native resolution'
           HEB_OUT%UNITS       = 'd/m'
           HEB_OUT%PROD_NAME   = 'MOD44W'
           HEB_OUT%FILE_NAME   =  FILOUT
           HEB_OUT%HISTORY     = 'Converted from HDF files with modis_lw_gen and with lw_transform'
           HEB_OUT%SOURCE      = 'MODIS/Terra Land Water Mask Derived from MODIS and SRTM L3 Global 250m SIN Grid'
           HEB_OUT%TITLE       = 'Land/Lake mask from MOD44W'
           HEB_OUT%INSTITUTION = 'NASA'
           HEB_OUT%COMMENT(1)  = '1 for land, [-32, -1] for big lakes, and 0 for other water'
           HEB_OUT%REFERENCES  = 'http://landweb.nascom.nasa.gov/cgi-bin/QA_WWW/qaFlagPage?cgi?sat=terra&ver=C5'
           HEB_OUT%PROD_DATE_TIME = '2009-07-31T17:28:14.000Z'
           HEB_OUT%VERSION_ID     = '5.1.0'
           HEB_OUT%MJD            = 51544       
           HEB_OUT%UTC            = 0.0D0
           HEB_OUT%TAI            = 0.0D0
           HEB_OUT%DATA_FORMAT      = HEB__I1
           HEB_OUT%DATA_TRANSFORM   = HEB__NONE 
           HEB_OUT%DATA_COMPRESSION = HEB__NONE 
           HEB_OUT%MIN_VALUE        =  -32.0
           HEB_OUT%MAX_VALUE        =    1.0
           HEB_OUT%VALID_RANGE(1)   =  -32.0
           HEB_OUT%VALID_RANGE(2)   =    1.0
           HEB_OUT%FILL_VALUE       = -127.0
           HEB_OUT%OFFSET           =    0.0
           HEB_OUT%SCALE_FACTOR     =    1.0
        ELSE IF ( MODE == 3 ) THEN
           IF ( IVRB > 0 ) THEN
                WRITE ( 6, * ) 'Started LW_TO_LL_MASK '
                CALL FLUSH ( 6 ) 
           END IF
           IUER = -1
           CALL LW_TO_LL_MASK ( FILLAC, HEB_LW, HEB_OUT, IVRB, IUER )
           IF ( IUER .NE. 0 ) THEN
                IUER = -2
                CALL ERR_LOG ( 3805, IUER, 'LW_TRANSFORM', 'Error in computing '// &
     &              'land-lake mask ' )
                CALL EXIT ( 1 )
           END IF 
!
           IF ( IVRB > 0 ) THEN
                WRITE ( 6, * ) 'Started LW_TO_LS_MASK '
                CALL FLUSH ( 6 ) 
           END IF
           IUER = -1
           CALL LW_TO_LS_MASK ( HEB_LW, HEB_OUT, IVRB, IUER )
           IF ( IUER .NE. 0 ) THEN
                IUER = -2
                CALL ERR_LOG ( 3806, IUER, 'LW_TRANSFORM', 'Error in computing '// &
     &              'land-lake mask ' )
                CALL EXIT ( 1 )
           END IF 
!
           IUER = -1
           HEB_OUT%SDS_NAME    = 'Land-lake-sea mask at native resolution'
           HEB_OUT%UNITS       = 'd/m'
           HEB_OUT%PROD_NAME   = 'MOD44W'
           HEB_OUT%FILE_NAME   =  FILOUT
           HEB_OUT%HISTORY     = 'Converted from HDF files with modis_lw_gen and with lw_transform'
           HEB_OUT%SOURCE      = 'MODIS/Terra Land Water Mask Derived from MODIS and SRTM L3 Global 250m SIN Grid'
           HEB_OUT%TITLE       = 'Land/Lake/Sea mask from MOD44W'
           HEB_OUT%INSTITUTION = 'NASA'
           HEB_OUT%COMMENT(1)  = '1 for land, 2 for sea, 0 for inland water except 32 largest lakes'
           HEB_OUT%COMMENT(2)  = '-32 through -1 for large lakes'
           HEB_OUT%REFERENCES  = 'http://landweb.nascom.nasa.gov/cgi-bin/QA_WWW/qaFlagPage?cgi?sat=terra&ver=C5'
           HEB_OUT%PROD_DATE_TIME = '2009-07-31T17:28:14.000Z'
           HEB_OUT%VERSION_ID     = '5.1.0'
           HEB_OUT%MJD            = 51544       
           HEB_OUT%UTC            = 0.0D0
           HEB_OUT%TAI            = 0.0D0
           HEB_OUT%DATA_FORMAT      = HEB__I1
           HEB_OUT%DATA_TRANSFORM   = HEB__NONE 
           HEB_OUT%DATA_COMPRESSION = HEB__NONE 
           HEB_OUT%MIN_VALUE        =  -32.0
           HEB_OUT%MAX_VALUE        =    2.0
           HEB_OUT%VALID_RANGE(1)   =  -32.0
           HEB_OUT%VALID_RANGE(2)   =    2.0
           HEB_OUT%FILL_VALUE       = -127.0
           HEB_OUT%OFFSET           =    0.0
           HEB_OUT%SCALE_FACTOR     =    1.0
        ELSE IF ( MODE == 4 ) THEN
           IF ( IVRB > 0 ) THEN
                WRITE ( 6, * ) 'Started LW_TO_LS_MASK(4) IVRB=', IVRB
                CALL FLUSH ( 6 ) 
           END IF
           IUER = -1
           CALL LW_TO_LS_MASK ( HEB_LW, HEB_OUT, IVRB, IUER )
           IF ( IUER .NE. 0 ) THEN
                IUER = -2
                CALL ERR_LOG ( 3807, IUER, 'LW_TRANSFORM', 'Error in computing '// &
     &              'land-lake mask ' )
                CALL EXIT ( 1 )
           END IF 
           IF ( IVRB > 0 ) THEN
                WRITE ( 6, * ) 'Started LW_TO_COAST_MASK(4) '
                CALL FLUSH ( 6 ) 
           END IF
!
           IUER = -1
           CALL LW_TO_COAST_MASK ( MODE, HEB_OUT, IVRB, IUER )
           IF ( IUER .NE. 0 ) THEN
                IUER = -2
                CALL ERR_LOG ( 3808, IUER, 'LW_TRANSFORM', 'Error in computing '// &
     &              'coastal mask ' )
                CALL EXIT ( 1 )
           END IF 
           IF ( IVRB > 0 ) THEN
                WRITE ( 6, * ) 'Ended LW_TO_COAST_MASK '
                CALL FLUSH ( 6 ) 
           END IF
           IUER = -1
           HEB_OUT%SDS_NAME    = 'Land-sea coast line at native resolution'
           HEB_OUT%UNITS       = 'd/m'
           HEB_OUT%PROD_NAME   = 'MOD44W'
           HEB_OUT%FILE_NAME   =  FILOUT
           HEB_OUT%HISTORY     = 'Converted from HDF files with modis_lw_gen and with lw_transform'
           HEB_OUT%SOURCE      = 'MODIS/Terra Land Water Mask Derived from MODIS and SRTM L3 Global 250m SIN Grid'
           HEB_OUT%TITLE       = 'Land/Sea coast line from MOD44W'
           HEB_OUT%INSTITUTION = 'NASA'
           HEB_OUT%COMMENT(1)  = '1 for coast-line, 0 for enything else'
           HEB_OUT%REFERENCES  = 'http://landweb.nascom.nasa.gov/cgi-bin/QA_WWW/qaFlagPage?cgi?sat=terra&ver=C5'
           HEB_OUT%PROD_DATE_TIME = '2009-07-31T17:28:14.000Z'
           HEB_OUT%VERSION_ID     = '5.1.0'
           HEB_OUT%MJD            = 51544       
           HEB_OUT%UTC            = 0.0D0
           HEB_OUT%TAI            = 0.0D0
           HEB_OUT%DATA_FORMAT      = HEB__I1
           HEB_OUT%DATA_TRANSFORM   = HEB__NONE 
           HEB_OUT%DATA_COMPRESSION = HEB__NONE 
           HEB_OUT%MIN_VALUE        =    0.0
           HEB_OUT%MAX_VALUE        =    1.0
           HEB_OUT%VALID_RANGE(1)   =    0.0
           HEB_OUT%VALID_RANGE(2)   =    1.0
           HEB_OUT%FILL_VALUE       = -127.0
           HEB_OUT%OFFSET           =    0.0
           HEB_OUT%SCALE_FACTOR     =    1.0
        ELSE IF ( MODE == 5 ) THEN
           IF ( IVRB > 0 ) THEN
                WRITE ( 6, * ) 'Started LW_TO_LL_MASK '
                CALL FLUSH ( 6 ) 
           END IF
           IUER = -1
           CALL LW_TO_LL_MASK ( FILLAC, HEB_LW, HEB_OUT, IVRB, IUER )
           IF ( IUER .NE. 0 ) THEN
                IUER = -2
                CALL ERR_LOG ( 3809, IUER, 'LW_TRANSFORM', 'Error in computing '// &
     &              'land-lake mask ' )
                CALL EXIT ( 1 )
           END IF 
!
           IF ( IVRB > 0 ) THEN
                WRITE ( 6, * ) 'Started LW_TO_LS_MASK(5) '
                CALL FLUSH ( 6 ) 
           END IF
           IUER = -1
           CALL LW_TO_LS_MASK ( HEB_LW, HEB_OUT, IVRB, IUER )
           IF ( IUER .NE. 0 ) THEN
                IUER = -2
                CALL ERR_LOG ( 3810, IUER, 'LW_TRANSFORM', 'Error in computing '// &
     &              'land-lake mask ' )
                CALL EXIT ( 1 )
           END IF 
           IF ( IVRB > 0 ) THEN
                WRITE ( 6, * ) 'Started LW_TO_COAST_MASK(5) '
                CALL FLUSH ( 6 ) 
           END IF
!
           IUER = -1
           CALL LW_TO_COAST_MASK ( MODE, HEB_OUT, IVRB, IUER )
           IF ( IUER .NE. 0 ) THEN
                IUER = -2
                CALL ERR_LOG ( 3811, IUER, 'LW_TRANSFORM', 'Error in computing '// &
     &              'coastal mask ' )
                CALL EXIT ( 1 )
           END IF 
!
           IUER = -1
           HEB_OUT%SDS_NAME    = 'Land-lake-sea coast line at native resolution'
           HEB_OUT%UNITS       = 'd/m'
           HEB_OUT%PROD_NAME   = 'MOD44W'
           HEB_OUT%FILE_NAME   =  FILOUT
           HEB_OUT%HISTORY     = 'Converted from HDF files with modis_lw_gen and with lw_transform'
           HEB_OUT%SOURCE      = 'MODIS/Terra Land Water Mask Derived from MODIS and SRTM L3 Global 250m SIN Grid'
           HEB_OUT%TITLE       = 'Land/Lake/Sea coast line from MOD44W'
           HEB_OUT%INSTITUTION = 'NASA'
           HEB_OUT%COMMENT(1)  = '1 for coast-line, 0 for enything else'
           HEB_OUT%REFERENCES  = 'http://landweb.nascom.nasa.gov/cgi-bin/QA_WWW/qaFlagPage?cgi?sat=terra&ver=C5'
           HEB_OUT%PROD_DATE_TIME = '2009-07-31T17:28:14.000Z'
           HEB_OUT%VERSION_ID     = '5.1.0'
           HEB_OUT%MJD            = 51544       
           HEB_OUT%UTC            = 0.0D0
           HEB_OUT%TAI            = 0.0D0
           HEB_OUT%DATA_FORMAT      = HEB__I1
           HEB_OUT%DATA_TRANSFORM   = HEB__NONE 
           HEB_OUT%DATA_COMPRESSION = HEB__NONE 
           HEB_OUT%MIN_VALUE        =    0.0
           HEB_OUT%MAX_VALUE        =    1.0
           HEB_OUT%VALID_RANGE(1)   =    0.0
           HEB_OUT%VALID_RANGE(2)   =    1.0
           HEB_OUT%FILL_VALUE       = -127.0
           HEB_OUT%OFFSET           =    0.0
           HEB_OUT%SCALE_FACTOR     =    1.0
      END IF
      IF ( IVRB > 0 ) THEN
           WRITE ( 6, * ) 'Started writing the output mask '
           CALL FLUSH ( 6 ) 
      END IF
!
      IUER = -1
      IF ( MODE == 1  .OR. MODE == 2  .OR.  MODE == 3 ) THEN
           CALL WRITE_HEB ( HEB_OUT, HEB_OUT%VAL, FILOUT, IUER )
         ELSE IF ( MODE == 4 .OR. MODE == 5 ) THEN
           CALL WRITE_HEB ( HEB_OUT, HEB_OUT%VAL1, FILOUT, IUER )
      END IF
      IF ( IUER .NE. 0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 3812, IUER, 'LW_TRANSFORM', 'Failure in writing '// &
     &         'into the output file '//FILOUT )
           CALL EXIT ( 1 )
      END IF
      IF ( IVRB > 0 ) THEN
           WRITE ( 6, '(A)' ) 'Output file: '//FILOUT(1:I_LEN(FILOUT))
           CALL FLUSH ( 6 ) 
      END IF
!
      END  PROGRAM  LW_TRANSFORM  !#!  
!
! ------------------------------------------------------------------------
!
      SUBROUTINE LW_TO_LS_MASK ( HEB_LW, HEB_OUT, IVRB, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  LW_TO_LS_MASK 
! *                                                                      *
! *  ### 06-JAN-2016  LW_TO_LS_MASK v1.0 (c) L. Petrov  06-JAN-2016 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'malo.i'
      INCLUDE   'heb.i'
      TYPE     ( HEB__TYPE  ) :: HEB_LW, HEB_OUT
      INTEGER*4  IVRB, IUER 
      REAL*8       PHI_MAX, PHI_MIN
      PARAMETER  ( PHI_MAX =  85.0D0*DEG__TO__RAD )
      PARAMETER  ( PHI_MIN = -85.0D0*DEG__TO__RAD )
      CHARACTER  STR*128
      INTEGER*4  ILO, ILA, ILO_S, ILA_S, ILA_MIN, ILA_MAX, &
     &           J1, J2, J3, J4, J5, J6, J7, J8, J9, NLO, &
     &           IER
      INTEGER*8  NSEA_H, NSEA_L, NSEA_A, NSEA_T
      INTEGER*4, EXTERNAL :: ILEN, I_LEN
      INTEGER*8, EXTERNAL :: LW_LONG, LW_LAT_LONG
!
      IF ( IVRB > 2 ) THEN
           WRITE ( 6, * ) 'LW_TO_LS: 358'
           CALL FLUSH ( 6 )
      END IF
      ILA_MAX = (P2I + PHI_MAX)/PI__NUM*(HEB_LW%DIMS(2)-1)
      ILA_MIN = (P2I + PHI_MIN)/PI__NUM*(HEB_LW%DIMS(2)-1)
      NSEA_A = 0
      DO 410 J1=ILA_MAX,HEB_LW%DIMS(2)
         DO 420 J2=1,HEB_LW%DIMS(1)
            HEB_LW%VAL1(J2,J1,1,1) = MALO__SEA_VAL
            NSEA_A = NSEA_A + 1
 420     CONTINUE 
 410  CONTINUE 
!
      NLO = HEB_LW%DIMS(1)
      ILO_S = 1
      IF ( IVRB > 2 ) THEN
           WRITE ( 6, * ) 'LW_TO_LS: 371' ; CALL FLUSH ( 6 ) 
           CALL FLUSH ( 6 )
      END IF
!
      DO 430 J3=1,128
         ILA_S = ILA_MAX
         NSEA_T = 0
         DO 440 J4=1,HEB_LW%DIMS(2)
            ILA = ILA_S - J4
            IF ( ILA .GE. ILA_MAX ) GOTO 840
            IF ( ILA .LE. ILA_MIN ) GOTO 840
            ILO = ILO_S
            NSEA_L = LW_LAT_LONG ( MALO__SEA_VAL, ILO, ILA, NLO, HEB_LW%DIMS(1), HEB_LW%DIMS(2), HEB_LW%VAL1 )
            NSEA_H = LW_LONG     ( MALO__SEA_VAL, ILO, ILA, NLO, HEB_LW%DIMS(1), HEB_LW%DIMS(2), HEB_LW%VAL1 )
            NSEA_T = NSEA_T + NSEA_L + NSEA_H
            NSEA_A = NSEA_A + NSEA_L + NSEA_H
            IF ( IVRB > 3 ) THEN
                 WRITE ( 6, * ) 'R4: j3= ', INT2(J3), ' ILA= ', ILA, &
     &                          ' NSEA L/H/A= ', INT(NSEA_L,KIND=4), INT(NSEA_H,KIND=4), &
     &                          NSEA_A ; CALL FLUSH ( 6 )
            END IF
 440     CONTINUE 
 840     CONTINUE 
      IF ( IVRB > 2 ) THEN
           WRITE ( 6, * ) 'LW_TO_LS: 398' ; CALL FLUSH ( 6 ) 
           CALL FLUSH ( 6 )
      END IF
!
         ILA_S = ILA_MIN
         DO 450 J5=1,HEB_LW%DIMS(2)
            ILA = ILA_S + J5
            IF ( ILA .GE. ILA_MAX ) GOTO 850
            IF ( ILA .LE. ILA_MIN ) GOTO 850
            NSEA_L = LW_LAT_LONG ( MALO__SEA_VAL, ILO, ILA, NLO, HEB_LW%DIMS(1), HEB_LW%DIMS(2), HEB_LW%VAL1 )
            NSEA_H = LW_LONG     ( MALO__SEA_VAL, ILO, ILA, NLO, HEB_LW%DIMS(1), HEB_LW%DIMS(2), HEB_LW%VAL1 )
            NSEA_T = NSEA_T + NSEA_L + NSEA_H
            NSEA_A = NSEA_A + NSEA_L + NSEA_H
            IF ( IVRB > 3 ) THEN
                 WRITE ( 6, * ) 'R5: j3= ', INT2(J3), ' ILA= ', ILA, &
     &                          ' NSEA L/H/A= ', INT(NSEA_L,KIND=4), INT(NSEA_H,KIND=4), &
     &                          NSEA_A ; CALL FLUSH ( 6 )
            END IF
 450     CONTINUE 
 850     CONTINUE 
         IF ( IVRB > 1 ) THEN
              WRITE ( 6, * ) 'LW_TO_LS Iteration: ', INT2(J3), ' NSEA_T= ', NSEA_T, ' NSEA_A= ', NSEA_A
         END IF
         IF ( NSEA_T == 0 ) GOTO 830
 430  CONTINUE 
 830  CONTINUE 
!
      IF ( IVRB > 1 ) THEN
           WRITE ( 6, * ) 'LS_TO_LS: NSEA= ', NSEA_A
           CALL FLUSH ( 6 )
      END IF
!
      IF ( .NOT. ASSOCIATED ( HEB_OUT%VAL ) ) THEN
           HEB_OUT = HEB_LW
           HEB_OUT%VAL1 => NULL()
           HEB_OUT%VAL  => NULL()
           ALLOCATE ( HEB_OUT%VAL(HEB_OUT%DIMS(1),HEB_OUT%DIMS(2),HEB_OUT%DIMS(3),HEB_OUT%DIMS(4)), &
     &                STAT=IER )
           IF ( IER .NE. 0 ) THEN
                CALL CLRCH ( STR )
                CALL IINCH8 ( INT8(8)*HEB_OUT%DIMS(1)*HEB_OUT%DIMS(2)*HEB_OUT%DIMS(3)*HEB_OUT%DIMS(4), &
     &                        STR )
                CALL ERR_LOG ( 3811, IUER, 'Failure to allocate '//STR(1:I_LEN(STR))// &
     &              'bytes of dynamic memory for array HEB_OUT' )
                RETURN
           END IF
      END IF
!
      DO 480 J8=1,HEB_LW%DIMS(2)
         DO 490 J9=1,HEB_LW%DIMS(1)
            IF ( HEB_LW%VAL1(J9,J8,1,1) == MALO__SEA_VAL ) THEN
                 HEB_OUT%VAL(J9,J8,1,1) = MALO__SEA_VAL
              ELSE IF ( HEB_LW%VAL1(J9,J8,1,1) == MALO__LAND_VAL ) THEN
                 HEB_OUT%VAL(J9,J8,1,1) = MALO__LAND_VAL 
              ELSE IF ( HEB_LW%VAL1(J9,J8,1,1) == MALO__WATER_VAL ) THEN
                 HEB_OUT%VAL(J9,J8,1,1) = MALO__WATER_VAL
            END IF
 490     CONTINUE 
 480  CONTINUE 
      IF ( IVRB > 1 ) THEN
           WRITE ( 6, * ) 'LW_TO_LS:  Finished HEB_OUT '
           CALL FLUSH ( 6 )
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  LW_TO_LS_MASK  !#!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE LW_TO_LL_MASK ( FILLAC, HEB_LW, HEB_OUT, IVRB, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  LW_TO_LL_MASK 
! *                                                                      *
! *  ### 07-JAN-2016  LW_TO_LL_MASK v1.0 (c) L. Petrov  08-JAN-2016 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'malo.i'
      INCLUDE   'heb.i'
      TYPE     ( HEB__TYPE  ) :: HEB_LW, HEB_OUT
      INTEGER*4  IVRB, IUER 
      CHARACTER  FILLAC*(*)
      REAL*8       PHI_MAX, PHI_MIN, LAT_SEMI_WIN, LON_SEMI_WIN
      PARAMETER  ( PHI_MAX =  85.0D0*DEG__TO__RAD )
      PARAMETER  ( PHI_MIN = -85.0D0*DEG__TO__RAD )
      PARAMETER  ( LAT_SEMI_WIN = 16.0D0*DEG__TO__RAD )
      PARAMETER  ( LON_SEMI_WIN = 16.0D0*DEG__TO__RAD )
      INTEGER*4    MBUF, MIND
      PARAMETER  ( MBUF = 128 ) 
      PARAMETER  ( MIND =  32 ) 
      CHARACTER  STR*128, BUF(MBUF)*128
      TYPE       ( MALO__LAKE_TYPE ) :: LAKE(MALO__MLAC)
      REAL*8     LON_STEP, LAT_STEP
      INTEGER*8  NLAC_H, NLAC_L, NLAC_A, NLAC_T
      INTEGER*4  ILO, ILA, ILO_S, ILA_S, ILA_MIN, ILA_MAX, &
     &           J1, J2, J3, J4, J5, J6, J7, J8, J9, L_LAK, NLO, &
     &           NB, LIND, IND(2,MIND), IER
      INTEGER*4, EXTERNAL :: ILEN, I_LEN
      INTEGER*8, EXTERNAL :: LW_LONG, LW_LAT_LONG
!
      CALL ERR_PASS ( IUER, IER  )
      CALL RD_TEXT  ( FILLAC, MBUF, BUF, NB, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 3411, IUER, 'LW_TRANSFORM', 'Error in reading '// &
     &         'lake file '//FILLAC )
           RETURN 
      END IF
      IF ( BUF(1)(1:LEN(MALO__LAK_LABEL)) == MALO__LAK_LABEL ) THEN
           CONTINUE 
         ELSE 
           CALL ERR_LOG ( 3412, IUER, 'LW_TRANSFORM', 'Unrecognized format of '// &
     &         'lake defintion file '//FILLAC(1:I_LEN(FILLAC))// &
     &         ' a format label '//MALO__LAK_LABEL//' was expected' )
           RETURN 
      END IF  
!
      L_LAK = 0
      DO 410 J1=1,NB
         IF ( BUF(J1)(1:1)  == '#' ) GOTO 410
         IF ( ILEN(BUF(J1)) ==  0  ) GOTO 410
         L_LAK = L_LAK + 1
         CALL EXWORD ( BUF(J1), MIND, LIND, IND, CHAR(0)//CHAR(32)//CHAR(9), IER )
         IF ( LIND < 4 ) THEN
              CALL CLRCH ( STR )
              CALL INCH  ( J1, STR)
              CALL ERR_LOG ( 3413, IUER, 'LW_TRANSFORM', 'Error in parsing '// &
     &            'line '//STR(1:I_LEN(STR))//' of the lake defintion file '// &
     &             FILLAC(1:I_LEN(FILLAC))//' -- less than 4 words' )
              RETURN 
         END IF
         READ ( UNIT=BUF(J1)(IND(1,2):IND(2,2)), FMT='(F12.5)' ) LAKE(L_LAK)%LAT_GDT
         READ ( UNIT=BUF(J1)(IND(1,3):IND(2,3)), FMT='(F12.5)' ) LAKE(L_LAK)%LON
         LAKE(L_LAK)%LAT_GDT = LAKE(L_LAK)%LAT_GDT*DEG__TO__RAD
         LAKE(L_LAK)%LON     = LAKE(L_LAK)%LON*DEG__TO__RAD
         LAKE(L_LAK)%NAME = BUF(J1)(IND(1,4):IND(2,LIND))
         LAKE(L_LAK)%IND  = -L_LAK
         IF ( L_LAK == MALO__MLAC ) GOTO 810
 410  CONTINUE 
 810  CONTINUE 
      IF ( IVRB > 1 ) THEN
           WRITE ( 6, * ) 'LW_TO_LS ', INT2(L_LAK), ' lakes will be processed' 
           CALL FLUSH  ( 6 )
      END IF
!
      LAT_STEP = PI__NUM/(HEB_LW%DIMS(2)-1)
      LON_STEP = PI2/HEB_LW%DIMS(1)
      NLO = IDNINT ( LON_SEMI_WIN/LON_STEP )
      DO 420 J2=1,L_LAK
         ILA_S = 1 + IDNINT( (LAKE(J2)%LAT_GDT+P2I)/LAT_STEP )
         ILO_S = 1 + IDNINT( LAKE(J2)%LON/LON_STEP )
         ILA_MAX = ILA_S + LAT_SEMI_WIN/LAT_STEP
         ILA_MIN = ILA_S - LAT_SEMI_WIN/LAT_STEP
         IF ( ILA_MAX > (P2I + PHI_MAX)/LAT_STEP ) THEN
              ILA_MAX = (P2I + PHI_MAX)/LAT_STEP
         END IF
         HEB_LW%VAL1(ILO_S,ILA_S,1,1) = LAKE(J2)%IND
         NLAC_A = 1
         ILA = ILA_S
         ILO = ILO_S
         NLAC_A = NLAC_A + LW_LONG ( LAKE(J2)%IND, ILO, ILA, NLO, HEB_LW%DIMS(1), HEB_LW%DIMS(2), HEB_LW%VAL1 )
         IF ( IVRB > 3 ) THEN
              WRITE ( 6, * ) 'R1 J2= ', INT2(J2), ' NLAC_A = ', NLAC_A
              CALL FLUSH ( 6 )
         END IF
!
         DO 430 J3=1,64
            NLAC_T = 0
            DO 440 J4=1,HEB_LW%DIMS(2)
               IF ( J3 == 1 ) THEN
                    ILA = ILA_S - J4
                  ELSE 
                    ILA = ILA_MAX - J4
               END IF
               IF ( ILA .GE. ILA_MAX ) GOTO 840
               IF ( ILA .LE. ILA_MIN ) GOTO 840
               ILO = ILO_S - NLO/2 + 1
               IF ( ILO < 1 ) ILO = ILO + HEB_LW%DIMS(1)
               NLAC_L = LW_LAT_LONG ( LAKE(J2)%IND, ILO, ILA, NLO, HEB_LW%DIMS(1), HEB_LW%DIMS(2), HEB_LW%VAL1 )
               ILO = ILO_S - NLO/2 + 1
               IF ( ILO < 1 ) ILO = ILO + HEB_LW%DIMS(1)
               NLAC_H = LW_LONG     ( LAKE(J2)%IND, ILO, ILA, NLO, HEB_LW%DIMS(1), HEB_LW%DIMS(2), HEB_LW%VAL1 )
               NLAC_T = NLAC_T + NLAC_L + NLAC_H
               NLAC_A = NLAC_A + NLAC_L + NLAC_H
               IF ( IVRB > 3 ) THEN
                    WRITE ( 6, * ) 'R4: J2= ', INT2(J2),' J3= ',INT2(J3), ' ILA= ', ILA, ' LAT= ', SNGL(-P2I+(ILA-1)*LAT_STEP/DEG__TO__RAD), ' NLAC L/H/A= ', INT(NLAC_L,KIND=4), INT(NLAC_H,KIND=4), NLAC_A
                    CALL FLUSH ( 6 )
               END IF
 440        CONTINUE 
 840        CONTINUE 
!
            ILA_S = ILA_MIN
            DO 450 J5=1,HEB_LW%DIMS(2)
               ILA = ILA_S + J5
               IF ( ILA .GE. ILA_MAX ) GOTO 850
               IF ( ILA .LE. ILA_MIN ) GOTO 850
               ILO = ILO_S - NLO/2 + 1
               IF ( ILO < 1 ) ILO = ILO + HEB_LW%DIMS(1)
               NLAC_L = LW_LAT_LONG ( LAKE(J2)%IND, ILO, ILA, NLO, HEB_LW%DIMS(1), HEB_LW%DIMS(2), HEB_LW%VAL1 )
               ILO = ILO_S - NLO/2 + 1
               IF ( ILO < 1 ) ILO = ILO + HEB_LW%DIMS(1)
               NLAC_H = LW_LONG     ( LAKE(J2)%IND, ILO, ILA, NLO, HEB_LW%DIMS(1), HEB_LW%DIMS(2), HEB_LW%VAL1 )
               NLAC_T = NLAC_T + NLAC_L + NLAC_H
               NLAC_A = NLAC_A + NLAC_L + NLAC_H
               IF ( IVRB > 3 ) THEN
                    WRITE ( 6, * ) 'R5: J2= ', INT2(J2),' J3= ',INT2(J3), ' ILA= ', ILA, ' LAT= ', SNGL(-P2I+(ILA-1)*LAT_STEP/DEG__TO__RAD), ' NLAC L/H/A= ', INT(NLAC_L,KIND=4), INT(NLAC_H,KIND=4), NLAC_A
                    CALL FLUSH ( 6 )
               END IF
 450        CONTINUE 
 850        CONTINUE 
            IF ( IVRB > 1 ) THEN
                 WRITE ( 6, * ) 'LW_TO_LS ', ' Lake ', INT2(J2), ' Iteration: ', INT2(J3), &
     &                          ' NLAC_T= ', NLAC_T, ' NLAC_A= ', NLAC_A
                 CALL FLUSH ( 6 )
            END IF
            IF ( NLAC_T == 0 ) GOTO 830
 430     CONTINUE 
 830     CONTINUE 
         IF ( IVRB > 3 ) THEN
              write ( 6, * ) 'R2 j2= ', int2(j2), ' nlac_a= ', nlac_a ; call flush ( 6 ) ! %%%%%%%%%%%%%
         END IF
 420  CONTINUE 
!
      IF ( IVRB > 1 ) THEN
           WRITE ( 6, * ) 'LW_TO_LS ', INT2(L_LAK), ' lakes have been processed' 
           CALL FLUSH  ( 6 )
      END IF
!
      HEB_OUT = HEB_LW
      HEB_OUT%VAL1 => NULL()
      HEB_OUT%VAL  => NULL()
      ALLOCATE ( HEB_OUT%VAL(HEB_OUT%DIMS(1),HEB_OUT%DIMS(2),HEB_OUT%DIMS(3),HEB_OUT%DIMS(4)), &
     &           STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH8 ( INT8(8)*HEB_OUT%DIMS(1)*HEB_OUT%DIMS(2)*HEB_OUT%DIMS(3)*HEB_OUT%DIMS(4), &
     &                   STR )
           CALL ERR_LOG ( 3811, IUER, 'Failure to allocate '//STR(1:I_LEN(STR))// &
     &         'bytes of dynamic memory for array HEB_OUT' )
           RETURN
      END IF
      DO 480 J8=1,HEB_LW%DIMS(2)
         DO 490 J9=1,HEB_LW%DIMS(1)
            HEB_OUT%VAL(J9,J8,1,1) = HEB_LW%VAL1(J9,J8,1,1) 
 490     CONTINUE 
 480  CONTINUE 
!
      IF ( IVRB > 1 ) THEN
           WRITE ( 6, * ) 'LS_TO_LS:  Finished HEB_OUT '
           CALL FLUSH ( 6 )
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  LW_TO_LL_MASK  !#!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE APPLY_DAMB ( FILDAMB, HEB_LW, IVRB, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine APPLY_DAMB 
! *                                                                      *
! *  ### 08-JAN-2016  APPLY_DAMB   v1.0 (c)  L. Petrov  08-JAN-2016 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'malo.i'
      INCLUDE   'heb.i'
      CHARACTER  FILDAMB*(*)
      TYPE     ( HEB__TYPE  ) :: HEB_LW
      INTEGER*4  IVRB, IUER 
      INTEGER*4  MBUF, MIND
      PARAMETER  ( MBUF = 1024 ) 
      PARAMETER  ( MIND =  32  )
!      
      TYPE ( MALO__DAMB_TYPE ) :: DAMB(MALO__MDAMB)
      CHARACTER  BUF(MBUF)*128, STR*128
      REAL*8     LAT_STEP, LON_STEP, ANG
      INTEGER*4  J1, J2, J3, J4, J5, NB, L_DAM, L_ARC, ILO, ILA, &
     &           IND_LAT_BEG, IND_LAT_END, IND_LON_BEG, IND_LON_END, &
     &           LIND, IND(2,MIND), IER
      REAL*8,    EXTERNAL :: ATAN_CS
      INTEGER*4, EXTERNAL :: ILEN, I_LEN
!
      CALL ERR_PASS ( IUER, IER  )
      CALL RD_TEXT  ( FILDAMB, MBUF, BUF, NB, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 3441, IUER, 'APPLY_DAMB', 'Error in reading '// &
     &         'lake file '//FILDAMB )
           RETURN 
      END IF
      IF ( BUF(1)(1:LEN(MALO__DAMB_LABEL)) == MALO__DAMB_LABEL ) THEN
           CONTINUE 
         ELSE 
           CALL ERR_LOG ( 3442, IUER, 'APPLY_DAMB', 'Unrecognized format of '// &
     &         'damb defintion file '//FILDAMB(1:I_LEN(FILDAMB))// &
     &         ' a format label '//MALO__DAMB_LABEL//' was expected' )
           RETURN 
      END IF  
!
      L_DAM = 0
      DO 410 J1=1,NB
         IF ( BUF(J1)(1:1)  == '#' ) GOTO 410
         IF ( ILEN(BUF(J1)) ==  0  ) GOTO 410
         L_DAM = L_DAM + 1
         CALL EXWORD ( BUF(J1), MIND, LIND, IND, CHAR(0)//CHAR(32)//CHAR(9), IER )
         IF ( LIND < 4 ) THEN
              CALL CLRCH ( STR )
              CALL INCH  ( J1, STR)
              CALL ERR_LOG ( 3443, IUER, 'APPLY_DAMB', 'Error in parsing '// &
     &            'line '//STR(1:I_LEN(STR))//' of the lake defintion file '// &
     &             FILDAMB(1:I_LEN(FILDAMB))//' -- less than 4 words' )
              RETURN 
         END IF
         READ ( UNIT=BUF(J1)(IND(1,2):IND(2,2)), FMT='(F12.5)' ) DAMB(L_DAM)%LAT_BEG
         READ ( UNIT=BUF(J1)(IND(1,3):IND(2,3)), FMT='(F12.5)' ) DAMB(L_DAM)%LON_BEG
         READ ( UNIT=BUF(J1)(IND(1,4):IND(2,4)), FMT='(F12.5)' ) DAMB(L_DAM)%LAT_END
         READ ( UNIT=BUF(J1)(IND(1,5):IND(2,5)), FMT='(F12.5)' ) DAMB(L_DAM)%LON_END
         DAMB(L_DAM)%LAT_BEG = DAMB(L_DAM)%LAT_BEG*DEG__TO__RAD
         DAMB(L_DAM)%LON_BEG = DAMB(L_DAM)%LON_BEG*DEG__TO__RAD
         DAMB(L_DAM)%LAT_END = DAMB(L_DAM)%LAT_END*DEG__TO__RAD
         DAMB(L_DAM)%LON_END = DAMB(L_DAM)%LON_END*DEG__TO__RAD
         IF ( BUF(J1)(IND(1,1):IND(2,1)) == '0' ) THEN
              DAMB(L_DAM)%IND = MALO__WATER_VAL
            ELSE IF ( BUF(J1)(IND(1,1):IND(2,1)) == '1' ) THEN
              DAMB(L_DAM)%IND = MALO__LAND_VAL
         END IF
         DAMB(L_DAM)%NAME = BUF(J1)(IND(1,6):IND(2,LIND))
         IF ( L_DAM == MALO__MDAMB ) GOTO 810
 410  CONTINUE 
 810  CONTINUE 
!
      LAT_STEP = PI__NUM/(HEB_LW%DIMS(2)-1)
      LON_STEP = PI2/HEB_LW%DIMS(1)
      DO 420 J2=1,L_DAM
         IND_LAT_BEG = 1 + IDNINT( (DAMB(J2)%LAT_BEG+P2I)/LAT_STEP )
         IND_LON_BEG = 1 + IDNINT(  DAMB(J2)%LON_BEG/LON_STEP )
         IND_LAT_END = 1 + IDNINT( (DAMB(J2)%LAT_END+P2I)/LAT_STEP )
         IND_LON_END = 1 + IDNINT(  DAMB(J2)%LON_END/LON_STEP )
         L_ARC = IDNINT ( DSQRT( 1.D0*(IND_LON_END - IND_LON_BEG)**2 + &
     &                           1.D0*(IND_LAT_END - IND_LAT_BEG)**2   ) )
         ANG = DSQRT ( ATAN_CS ( 1.0D0*(IND_LON_END - IND_LON_BEG), &
     &                           1.0D0*(IND_LAT_END - IND_LAT_BEG)  )  )
         DO 430 J3=1,L_ARC
            ILA = IND_LAT_BEG + IDINT ( J3*DSIN(ANG) )
            ILO = IND_LON_BEG + IDINT ( J3*DCOS(ANG) )
            HEB_LW%VAL1(ILO,ILA,1,1)   = DAMB(J2)%IND 
            HEB_LW%VAL1(ILO+1,ILA,1,1) = DAMB(J2)%IND 
            HEB_LW%VAL1(ILO-1,ILA,1,1) = DAMB(J2)%IND 
            HEB_LW%VAL1(ILO,ILA+1,1,1) = DAMB(J2)%IND 
            HEB_LW%VAL1(ILO,ILA-1,1,1) = DAMB(J2)%IND 
 430     CONTINUE 
 420  CONTINUE 
      IF ( IVRB > 1 ) THEN
           WRITE ( 6, * ) 'APPLY_DAMB: ', INT2(L_DAM), ' dambs were applied'
      END IF
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE APPLY_DAMB   !#!  
!
! ------------------------------------------------------------------------
!
      FUNCTION   LW_LAT_LONG ( WAT_IND, ILO, ILA, NLO, DIMLO, DIMLA, ARR1 )
! ************************************************************************
! *                                                                      *
! *   Routine LW_LAT_LONG 
! *                                                                      *
! * ### 06-JAN-2016   LW_LAT_LONG   v1.0 (c)  L. Petrov  06-JAN-2016 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'malo.i'
      INTEGER*8  LW_LAT_LONG
      INTEGER*1  WAT_IND
      INTEGER*4  ILO, ILA, NLO
      INTEGER*8  DIMLO, DIMLA
      INTEGER*1  ARR1(DIMLO,DIMLA)
      INTEGER*4  ILO_S, ILOM, ILOP, J1, J2
!
      LW_LAT_LONG = 0
      IF ( ARR1(ILO,ILA) == MALO__WATER_VAL ) THEN
           ILOM = ILO - 1
           IF ( ILOM < 1 ) ILOM = DIMLO 
           ILOP = ILO + 1
           IF ( ILOP > DIMLO ) ILOP = 1 
           IF ( ARR1(ILO,ILA+1) == WAT_IND ) THEN
                ARR1(ILO,ILA) = WAT_IND 
                LW_LAT_LONG = LW_LAT_LONG + 1
              ELSE IF ( ARR1(ILO,ILA-1) == WAT_IND ) THEN
                ARR1(ILO,ILA) = WAT_IND 
                LW_LAT_LONG = LW_LAT_LONG + 1
              ELSE IF ( ARR1(ILOM,ILA) == WAT_IND ) THEN
                ARR1(ILO,ILA) = WAT_IND 
                LW_LAT_LONG = LW_LAT_LONG + 1
              ELSE IF ( ARR1(ILOP,ILA) == WAT_IND ) THEN
                ARR1(ILO,ILA) = WAT_IND 
                LW_LAT_LONG = LW_LAT_LONG + 1
           END IF
      END IF
!
      ILO_S = ILO
      DO 410 J1=1,NLO
         ILO = ILO + 1
         IF ( ILO > DIMLO ) ILO = 1
         IF ( ARR1(ILO,ILA) == MALO__WATER_VAL ) THEN
              IF ( ARR1(ILO,ILA+1) == WAT_IND ) THEN
                   ARR1(ILO,ILA) = WAT_IND 
                   LW_LAT_LONG = LW_LAT_LONG + 1
                ELSE IF ( ARR1(ILO,ILA-1) == WAT_IND ) THEN
                   ARR1(ILO,ILA) = WAT_IND 
                   LW_LAT_LONG = LW_LAT_LONG + 1
              END IF
         END IF
 410  CONTINUE 
!
      IF ( NLO == DIMLO ) THEN
           ILO = ILO_S
      END IF
      DO 420 J2=1,NLO
         ILO = ILO - 1
         IF ( ILO < 1 ) ILO = DIMLO
         IF ( ARR1(ILO,ILA) == MALO__WATER_VAL ) THEN
              IF ( ARR1(ILO,ILA+1) == WAT_IND ) THEN
                   ARR1(ILO,ILA) = WAT_IND 
                   LW_LAT_LONG = LW_LAT_LONG + 1
                ELSE IF ( ARR1(ILO,ILA-1) == WAT_IND ) THEN
                   ARR1(ILO,ILA) = WAT_IND 
                   LW_LAT_LONG = LW_LAT_LONG + 1
              END IF
         END IF
 420  CONTINUE 
      RETURN
      END  FUNCTION   LW_LAT_LONG  !#!  
!
! ------------------------------------------------------------------------
!
      FUNCTION   LW_LONG ( WAT_IND, ILO, ILA, NLO, DIMLO, DIMLA, ARR1 )
! ************************************************************************
! *                                                                      *
! *   Routine LW_LONG 
! *                                                                      *
! *  ### 06-JAN-2016    LW_LONG    v1.0 (c)  L. Petrov  06-JAN-2016 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'malo.i'
      INTEGER*8  LW_LONG
      INTEGER*1  WAT_IND
      INTEGER*4  NLO, ILO, ILA
      INTEGER*8  DIMLO, DIMLA
      INTEGER*1  ARR1(DIMLO,DIMLA)
      INTEGER*4  ILOM, ILOP, ILO_S, J1, J2
!
      LW_LONG = 0
      ILO_S = ILO
      ILOM  = ILO
      DO 410 J1=1,NLO
         ILO = ILO + 1
         IF ( ILO > DIMLO ) ILO = 1
         IF ( ARR1(ILO,ILA) == MALO__WATER_VAL ) THEN
              IF ( ARR1(ILOM,ILA) == WAT_IND ) THEN
                   ARR1(ILO,ILA) = WAT_IND 
                   LW_LONG = LW_LONG + 1
              END IF
         END IF
         ILOM = ILO
 410  CONTINUE 
!
      IF ( NLO == DIMLO ) THEN
           ILO = ILO_S
      END IF
      ILOP = ILO
      DO 420 J2=1,NLO
         ILO = ILO - 1
         IF ( ILO < 1 ) ILO = DIMLO
         IF ( ARR1(ILO,ILA) == MALO__WATER_VAL ) THEN
              IF ( ARR1(ILOP,ILA) == WAT_IND ) THEN
                   ARR1(ILO,ILA) = WAT_IND 
                   LW_LONG = LW_LONG + 1
              END IF
         END IF
         ILOP = ILO
 420  CONTINUE 
      RETURN
      END  FUNCTION   LW_LONG  !#!  
!
! ------------------------------------------------------------------------
!
      SUBROUTINE LW_TO_COAST_MASK ( MODE, HEB_LW, IVRB, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  LW_TO_COAST_MASK 
! *                                                                      *
! * ### 07-JAN-2016 LW_TO_COAST_MASK v1.0 (c) L. Petrov  08-JAN-2016 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'malo.i'
      INCLUDE   'heb.i'
      TYPE     ( HEB__TYPE  ) :: HEB_LW
      INTEGER*4  MODE, IVRB, IUER 
      INTEGER*4  IND_LAT_PREV, IND_LAT_HERE, IND_LAT_NEXT, &
     &           IND_LON_PREV, IND_LON_HERE, IND_LON_NEXT, &
     &           J1, J2, J3, J4, IER
      INTEGER*1  LS_SUM
      INTEGER*1, ALLOCATABLE :: LW(:,:,:,:)
      CHARACTER  STR*128
      INTEGER*4, EXTERNAL :: ILEN, I_LEN
!
      IF ( IVRB > 2 ) THEN
           WRITE ( 6, * ) 'LW_TO_COAST_MASK 912' ; CALL FLUSH ( 6 )
      END IF
      ALLOCATE ( HEB_LW%VAL1(HEB_LW%DIMS(1),HEB_LW%DIMS(2),HEB_LW%DIMS(3),HEB_LW%DIMS(4)), &
     &           STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 5771, IUER, 'LW_TO_COAST_MASK', 'Failure to '// &
     &          'allocated '//STR(1:I_LEN(STR))//' bytes of dynamic '// &
     &          'memory for array HEB_LW%VAL1' )
           RETURN 
      END IF
      IF ( IVRB > 2 ) THEN
           WRITE ( 6, * ) 'LW_TO_COAST_MASK 925' ; CALL FLUSH ( 6 )
      END IF
      ALLOCATE ( LW(HEB_LW%DIMS(1),HEB_LW%DIMS(2),HEB_LW%DIMS(3),HEB_LW%DIMS(4)), &
     &           STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 5771, IUER, 'LW_TO_COAST_MASK', 'Failure to '// &
     &          'allocated '//STR(1:I_LEN(STR))//' bytes of dynamic '// &
     &          'memory for array LW' )
           RETURN 
      END IF
      IF ( IVRB > 2 ) THEN
           WRITE ( 6, * ) 'LW_TO_COAST_MASK 936' ; CALL FLUSH ( 6 )
      END IF
!
      DO 410 J1=1,HEB_LW%DIMS(2)
          DO 420 J2=1,HEB_LW%DIMS(1)
             IF ( MODE == 4 ) THEN
                  IF ( NINT(HEB_LW%VAL(J2,J1,1,1)) .EQ. MALO__SEA_VAL ) THEN
                       LW(J2,J1,1,1) = MALO__WATER_VAL
                     ELSE 
                       LW(J2,J1,1,1) = MALO__LAND_VAL
                  END IF
                ELSE IF ( MODE == 5 ) THEN
                  IF ( NINT(HEB_LW%VAL(J2,J1,1,1)) .NE. MALO__LAND_VAL ) THEN
                       LW(J2,J1,1,1) = MALO__WATER_VAL
                     ELSE 
                       LW(J2,J1,1,1) = MALO__LAND_VAL
                  END IF
             END IF
 420      CONTINUE 
 410  CONTINUE 
      IF ( IVRB > 2 ) THEN
           WRITE ( 6, * ) 'LW_TO_COAST_MASK 957' ; CALL FLUSH ( 6 )
      END IF
!
      DO 430 J3=1,HEB_LW%DIMS(2)
         IND_LAT_PREV = J3-1
         IND_LAT_HERE = J3
         IND_LAT_NEXT = J3+1
         IF ( IND_LAT_PREV < 1              ) IND_LAT_PREV = 2
         IF ( IND_LAT_NEXT > HEB_LW%DIMS(2) ) IND_LAT_NEXT = HEB_LW%DIMS(2) - 1
         DO 440 J4=1,HEB_LW%DIMS(1)
            IND_LON_PREV = J4-1
            IND_LON_HERE = J4
            IND_LON_NEXT = J4+1
            IF ( IND_LON_PREV < 1              ) IND_LON_PREV = HEB_LW%DIMS(1)
            IF ( IND_LON_NEXT > HEB_LW%DIMS(1) ) IND_LON_NEXT = 1
            HEB_LW%VAL1(J4,J3,1,1) = MALO__WATER_VAL
            IF ( LW(IND_LON_HERE,IND_LAT_HERE,1,1) == MALO__WATER_VAL ) THEN
                 LS_SUM = LW(IND_LON_PREV,IND_LAT_HERE,1,1) + &
     &                    LW(IND_LON_HERE,IND_LAT_PREV,1,1) + &
     &                    LW(IND_LON_HERE,IND_LAT_NEXT,1,1) + &
     &                    LW(IND_LON_NEXT,IND_LAT_HERE,1,1)
                 IF ( LS_SUM > 0 ) THEN
                      HEB_LW%VAL1(J4,J3,1,1) = MALO__LAND_VAL
                 END IF
            END IF
 440     CONTINUE 
 430  CONTINUE 
      IF ( IVRB > 2 ) THEN
           WRITE ( 6, * ) 'LW_TO_COAST_MASK 985' ; CALL FLUSH ( 6 )
      END IF
      DEALLOCATE ( LW ) 
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  LW_TO_COAST_MASK  !#!  
