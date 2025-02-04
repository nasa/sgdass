      PROGRAM    GEN_OTIDE
! ************************************************************************
! *                                                                      *
! *   Program  GEN_OTIDE  reads original files with ocean tide maps.     *
! *   converts them in cos/sin form, upgrids, expands to the external    *
! *   land-sea maks and write down in HEB format.                        *
! *                                                                      *
! *  ### 14-APR-2014   GEN_OTIDE   v2.1 (c)  L. Petrov  07-JUL-2017 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'malo.i'
      INCLUDE   'heb.i'
      INTEGER*4  MF
      PARAMETER  ( MF = 128 )
      TYPE       ( HEB__TYPE  ) :: HEB, HEB_LS
      CHARACTER  STR*128, DIRIN*128, FILS(MF)*128, FILNAM*128, FILOUT*128, &
     &           WAV_ARR(MF)*8, WAV_STR*1024, FIL_LS_MASK*128, MODE*3, WAVE_ENV*16
      REAL*4     VAL_MAX_R4, SCALE, FILL_VALUE, MINVAL_R4, MAXVAL_R4
      REAL*4,    ALLOCATABLE :: ARRIN_R4(:,:,:,:), ARROUT_R4(:,:,:,:), LS(:,:)
      REAL*4     AMPL_IN, AMPL_OUT
      ADDRESS__TYPE :: DIR_DESC
      LOGICAL*1  FL_LARGE
      INTEGER*4  J1, J2, J3, J4, J5, IS, LEV, LF, NLON, NLAT, UNIX_DATE, &
     &           DEG, LLON, LLAT, NCMP, NWAV, IVRB, SEEK_SET, ARG_LN, LUN, IUER
      INTEGER*4  ILON, ILAT
      REAL*4     VAL_MIN, VAL_MAX
      INTEGER*8  SIZE_I8, LEN_I8, OFFSET_RET
      INTEGER*4, EXTERNAL :: ILEN, I_LEN, GET_FILE_FROM_DIR, FILE_INFO
      CHARACTER, EXTERNAL :: GET_CDATE*19
!
      CALL GET_SYSTEM_CONSTANT ( 'SEEK_SET', SEEK_SET, ARG_LN )
!
      IVRB = 2
      IF ( IARGC() < 4 ) THEN
           WRITE ( 6, * ) 'Usage: gen_otide mode dirin dls_mask filout [ivrb]' 
           CALL EXIT ( 1 ) 
         ELSE
           CALL GETARG ( 1, MODE   )
           CALL GETARG ( 2, DIRIN  )
           CALL GETARG ( 3, FIL_LS_MASK )
           CALL GETARG ( 4, FILOUT )
           IF ( IARGC() .GE. 5 ) THEN
                CALL GETARG ( 5, STR )
                CALL CHIN   ( STR, IVRB )
           END IF
      END IF
!
! --- Check the mode
!
      IF ( MODE == 'got' ) THEN
           CONTINUE
         ELSE IF ( MODE == 'tpx' ) THEN
           CONTINUE
         ELSE IF ( MODE == 'eot' ) THEN
           CONTINUE
         ELSE IF ( MODE == 'fes' ) THEN
           CONTINUE
         ELSE 
           CALL ERR_LOG ( 6801, IUER, 'GEN_OTIDE', 'Wrong mode '// &
     &          STR(1:I_LEN(STR))//' while only modes got, tps, eot, '// &
     &          'or fes are supported' )
           CALL EXIT ( 1 )
      END IF
      IF ( INDEX ( DIRIN, 'got48' ) > 0 ) THEN
           CONTINUE 
        ELSE IF ( INDEX ( DIRIN, 'got410c' ) > 0 ) THEN
           CONTINUE 
        ELSE IF ( INDEX ( DIRIN, 'tpx08' ) > 0 ) THEN
           CONTINUE 
        ELSE IF ( INDEX ( DIRIN, 'eot11a' ) > 0 ) THEN
           CONTINUE 
        ELSE IF ( INDEX ( DIRIN, 'fes2012' ) > 0 ) THEN
           CONTINUE 
        ELSE IF ( INDEX ( DIRIN, 'fes2014b' ) > 0 ) THEN
           CONTINUE 
        ELSE
           CALL ERR_LOG ( 6802, IUER, 'GEN_OTIDE', 'The name of the input '// &
     &          'directory '//DIRIN(1:I_LEN(DIRIN))//' does not contains '// &
     &          'a recognizable ocean tide model' )
           CALL EXIT ( 1 )
      END IF
!
      CALL GETENVAR ( 'GEN_OTIDE_WAVE', WAVE_ENV )
      IF ( ILEN(WAVE_ENV) > 0 ) WRITE ( 6, * ) '!!! Processing  only wave envar GEN_OTIDE_WAVE '//TRIM(WAVE_ENV)
!
! --- Read the land-sea mask
!
      IUER = -1
      CALL MALO_CHECK_SHARE_FILE ( FIL_LS_MASK, IUER )
      IF ( IUER .NE. 0 ) THEN
           CALL ERR_LOG ( 6803, IUER, 'GEN_OTIDE', 'Cannot find land-sea '// &
     &         'mask file '//FIL_LS_MASK )
           CALL EXIT ( 1 )
      END IF
!
      IUER = -1
      CALL READ_HEB ( FIL_LS_MASK, HEB_LS, IUER ) 
      IF ( IUER .NE. 0 ) THEN
           CALL ERR_LOG ( 6804, IUER, 'GEN_OTIDE', 'Error in '// &
          &    'an attempt to read heb-file with land-sea '// &
          &    'mask '//FIL_LS_MASK )
           CALL EXIT ( 1 )
      END IF   
!
! --- Search for the original files with ocean tides
!
      LF  = 0
      LEV = 0
      DO 410 J1=1,1024*1024*1024
         IS = GET_FILE_FROM_DIR ( LEV, DIR_DESC, DIRIN, FILNAM )
         IF ( IS .NE. 0 ) THEN
              IUER = -1
              CALL ERR_LOG ( 6805, -2, 'GEN_OTIDE', 'Error in '// &
     &           'reading input directory '//DIRIN(1:I_LEN(DIRIN))// &
     &           '  '//FILNAM )
              CALL EXIT ( 1 )
         END IF
         IF ( LEV == 0 ) GOTO 810 ! End of work
!
         IF ( ILEN(WAVE_ENV) > 0 ) THEN
              IF ( INDEX ( FILNAM, TRIM(WAVE_ENV) ) < 1 ) GOTO 410
         END IF
!
         IF ( MODE == 'got' ) THEN
              IF ( INDEX ( FILNAM, '.d' ) > 0 ) THEN
                   LF = LF + 1 
                   FILS(LF) = FILNAM
              END IF
           ELSE IF ( MODE == 'tpx' ) THEN
              IF ( INDEX ( FILNAM, '.nc' ) > 0 ) THEN
                   LF = LF + 1 
                   FILS(LF) = FILNAM
              END IF
           ELSE IF ( MODE == 'eot' ) THEN
              IF ( INDEX ( FILNAM, '.nc'    ) .GT. 0 .AND. &
     &             INDEX ( FILNAM, '.load.' ) .LE. 0       ) THEN
                   LF = LF + 1 
                   FILS(LF) = FILNAM
              END IF
           ELSE IF ( MODE == 'fes' ) THEN
              IF ( INDEX ( FILNAM, '_SLEV.nc' ) .GT. 0 ) THEN
                   LF = LF + 1 
                   FILS(LF) = FILNAM
               ELSE IF ( INDEX ( FILNAM, '.nc' ) .GT. 0 ) THEN
                   LF = LF + 1 
                   FILS(LF) = FILNAM
              END IF
         END IF
 410  CONTINUE 
 810  CONTINUE 
      IF ( LF == 0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 6806, -2, 'GEN_OTIDE', 'Sorry, no relevant '// &
     &         'files were found in input directory '//DIRIN )
           CALL EXIT ( 1 )
         ELSE 
!
! -------- Sort the list alphabetically
!
           CALL SORT_FAST_CH ( LF, FILS )
      END IF    
!
! --- Learn dimension of the oritignal maps and the list of wave names
!
      IF ( MODE == 'got' ) THEN
!
! -------- GOT type of maps
!
           IUER = -1
           CALL INQ_GOT ( FILS(1), NLON, NLAT, WAV_ARR(1), IUER )
           IF ( IUER .NE. 0 ) THEN
                IUER = -1
                CALL ERR_LOG ( 6807, IUER, 'GEN_OTIDE', 'Failure in an attempt to '// &
     &              'make an inquiry about file '//FILS(1) )
                CALL EXIT ( 1 )
           END IF
           VAL_MAX_R4 = 3000.0
           SCALE      = 0.01
        ELSE IF ( MODE == 'tpx' ) THEN
           IUER = -1
           CALL INQ_NC ( FILS(1), NLON, NLAT, WAV_ARR(1), IUER )
           IF ( IUER .NE. 0 ) THEN
                IUER = -1
                CALL ERR_LOG ( 6808, IUER, 'GEN_OTIDE', 'Failure in an attempt to '// &
     &              'make an inquiry about file '//FILS(1) )
                CALL EXIT ( 1 )
           END IF
           ALLOCATE ( LS(NLON,NLAT) )
           LS = 0
           VAL_MAX_R4 = 30000.0
           SCALE      = 0.001
        ELSE IF ( MODE == 'eot' .OR. MODE == 'fes' ) THEN
!
! -------- Tidal maps in the NetCDF format
!
           IUER = -1
           CALL INQ_NC ( FILS(1), NLON, NLAT, WAV_ARR(1), IUER )
           IF ( IUER .NE. 0 ) THEN
                IUER = -1
                CALL ERR_LOG ( 6809, IUER, 'GEN_OTIDE', 'Failure in an attempt to '// &
     &              'make an inquiry about file '//FILS(1) )
                CALL EXIT ( 1 )
           END IF
           VAL_MAX_R4 = 30000.0
           SCALE      = 0.01
      END IF
!
      FILL_VALUE = 1.0E15
      NWAV = LF
      NCMP = 2
      IF ( IVRB .GE. 3 ) THEN
           WRITE ( 6, * ) 'NLON= ', NLON, ' NLAT= ', NLAT, ' WAV_ARR(1)= ', WAV_ARR(1)
      END IF
!
! --- Allocate an array for input maps
!
      ALLOCATE ( ARRIN_R4(NLON,NLAT,NCMP,NWAV), STAT=IUER )
      IF ( IUER .NE. 0 ) THEN
           IUER = -1
           CALL IINCH ( 4*NLON,NLAT*NCMP*NWAV, STR )
           CALL ERR_LOG ( 6810, IUER, 'GEN_OTIDE', 'Failure in an attempt '// &
     &         'to allocate '//STR(1:I_LEN(STR))//' bytes of memory for '// &
     &         'array ARRIN_R4' )
           CALL EXIT ( 1 )
      END IF
      ARRIN_R4 = 0.0
!
! --- Read files with the ocean tide model
!
      CALL CLRCH ( WAV_STR )
      DO 420 J2=1,LF
         IUER = -1
         WRITE ( 6, * ) ' J2= ',INT2(J2), ' FILS= '//FILS(J2)(1:I_LEN(FILS(J2)))
         IF ( MODE == 'got' ) THEN
!
! ----------- GOT format
!
              CALL READ_GOT   ( FILS(J2), NLON, NLAT, WAV_ARR(J2), ARRIN_R4(1,1,1,J2), IUER )
            ELSE IF ( MODE == 'tpx'  .OR.  MODE == 'eot'  .OR.  MODE == 'fes' ) THEN
!
! ----------- NetCDF format
!
              CALL READ_OT_NC ( FILS(J2), NLON, NLAT, WAV_ARR(J2), ARRIN_R4(1,1,1,J2), IUER )
         END IF
         IF ( IUER .NE. 0 ) THEN
              IUER = -1
              CALL ERR_LOG ( 6811, IUER, 'GEN_OTIDE', 'Failure in an attempt '// &
     &            'to read the '//STR(1:I_LEN(STR))//'th file '//FILS )
              CALL EXIT ( 1 )
         END IF
         CALL TRAN ( 12, WAV_ARR(J2), WAV_ARR(J2) )
!
! ------ Append the wave name to the wave string
!
         WAV_STR = WAV_STR(1:I_LEN(WAV_STR))//','//WAV_ARR(J2)
!
         IF ( MODE == 'fes' ) THEN
              IF ( WAV_ARR(J2) == 'sa'   .OR. &
     &             WAV_ARR(J2) == 'ssa'  .OR. &
     &             WAV_ARR(J2) == 'mm'   .OR. &
     &             WAV_ARR(J2) == 'mf'   .OR. &
     &             WAV_ARR(J2) == 'msf'  .OR. &
     &             WAV_ARR(J2) == 'msqm' .OR. &
     &             WAV_ARR(J2) == 'mtm'       ) THEN
!
! ---------------- Flip phase for zonal tides in fes2014b model.
! ---------------- It is not clear why. May be some archaic phase convention,
! ---------------- or a bug
!
                   ARRIN_R4(1:NLON,1:NLAT,2,J2) = ARRIN_R4(1:NLON,1:NLAT,2,J2) - 180.0
                   WRITE ( 6, * ) ' Flip sign for wave ', WAV_ARR(J2)
              END IF
         END IF
 420  CONTINUE 
         IF ( IVRB .GE. 9 ) THEN
              VAL_MIN =  1.0
              VAL_MAX = -1.0
              CALL PLOT_GRID_R4 ( 1, 7, 0, 1, NLON, NLAT, ARRIN_R4(1,1,1,1), &
        &                         'Ampl_in', 'cm', VAL_MIN, VAL_MAX, '/tmp/boo', iuer )
              CALL PLOT_GRID_R4 ( 1, 7, 0, 1, NLON, NLAT, ARRIN_R4(1,1,2,1), &
        &                         'Phase', 'deg', VAL_MIN, VAL_MAX, '/tmp/boo', iuer )
         END IF
!
      IF ( MODE == 'got' .OR. MODE == 'fes' ) THEN
!
! -------- Transform maps from amplitude/phase to cosine/sine and convert
! -------- units to meters
!
           IUER = -1
           CALL AMP_PHS_ARR ( NLON, NLAT, NCMP, NWAV, SCALE, VAL_MAX_R4, &
     &                        FILL_VALUE, ARRIN_R4, MINVAL_R4, MAXVAL_R4, IUER )
           IF ( IUER .NE. 0 ) THEN
                IUER = -1
                CALL ERR_LOG ( 6812, IUER, 'GEN_OTIDE', 'Failure in an attempt '// &
     &              'to clean the ocean tide model' )
                CALL EXIT ( 1 )
           END IF
        ELSE IF ( MODE == 'tpx' ) THEN
!
! -------- ??
!
           IUER = -1
           CALL CMPLX_ARR_SWAP ( NLON, NLAT, NCMP, NWAV, SCALE, VAL_MAX_R4, &
     &                           FILL_VALUE, FILS, ARRIN_R4, &
     &                           MINVAL_R4, MAXVAL_R4, IUER )
           IF ( IUER .NE. 0 ) THEN
                IUER = -1
                CALL ERR_LOG ( 6813, IUER, 'GEN_OTIDE', 'Failure in an attempt '// &
     &              'to clean the ocean tide model' )
                CALL EXIT ( 1 )
           END IF
!
           IUER = -1
           CALL GET_TPXO8_LS ( LF, FILS, NLON, NLAT, ARRIN_R4, LS, IUER )
!
           IUER = -1
           CALL TPXO8_UPDATE_ARR ( NLON, NLAT, NCMP, NWAV, SCALE, &
     &                             FILL_VALUE, FILS, ARRIN_R4, LS, &
     &                             MINVAL_R4, MAXVAL_R4, IUER )
        ELSE IF ( MODE == 'eot' ) THEN
           IUER = -1
           CALL EOT11A_UPDATE_ARR ( NLON, NLAT, NCMP, NWAV, SCALE, &
     &                              FILL_VALUE, FILS, ARRIN_R4, &
     &                              MINVAL_R4, MAXVAL_R4, IUER )
           NLON = NLON - 1
      END IF
      IF ( MODE == 'fes' ) THEN
!
! -------- ??
!
           IUER = -1
           CALL EOT11A_UPDATE_ARR ( NLON, NLAT, NCMP, NWAV, 1.0, &
     &                              FILL_VALUE, FILS, ARRIN_R4, &
     &                              MINVAL_R4, MAXVAL_R4, IUER )
           NLON = NLON - 1
      END IF
!
      WAV_STR = WAV_STR(3:)
      CALL TRAN ( 12, WAV_STR, WAV_STR )
      WRITE ( 6, * ) 'WAV_STR >>'//WAV_STR(1:I_LEN(WAV_STR))
!
! --- Determine whether we will write tidal maps at once or wave by wave
!
      IF ( INT8(4)*HEB_LS%DIMS(1)*HEB_LS%DIMS(2)*INT8(NCMP)*INT8(NWAV) > 32000000000_8 ) THEN
           FL_LARGE = .TRUE.
         ELSE
           FL_LARGE = .FALSE.
      END IF
!
      LLON = HEB_LS%DIMS(1)
      LLAT = HEB_LS%DIMS(2)
!
! --- Allocate arrays for the output 
!
      IF ( FL_LARGE ) THEN
           ALLOCATE ( ARROUT_R4(LLON,LLAT,NCMP,1), STAT=IUER )
           IF ( IUER .NE. 0 ) THEN
                IUER = -1
                CALL IINCH8 ( INT8(4)*INT8(LLON)*INT8(LLAT)*INT8(NCMP), STR )
                CALL ERR_LOG ( 6814, IUER, 'GEN_OTIDE', 'Failure in an attempt '// &
     &              'to allocate '//STR(1:I_LEN(STR))//' bytes of memory for '// &
     &               'array ARROUT_R4' )
                CALL EXIT ( 1 )
           END IF
         ELSE 
           ALLOCATE ( ARROUT_R4(LLON,LLAT,NCMP,NWAV), STAT=IUER )
           IF ( IUER .NE. 0 ) THEN
                IUER = -1
                WRITE ( 6, * ) 'LLON,LLAT,NCMP,NWAV= ', LLON, LLAT, NCMP, NWAV
                CALL IINCH8 ( INT8(4)*INT8(LLON)*INT8(LLAT)*INT8(NCMP)*INT8(NWAV), STR )
                CALL ERR_LOG ( 6815, IUER, 'GEN_OTIDE', 'Failure in an attempt '// &
     &              'to allocate '//STR(1:I_LEN(STR))//' bytes of memory for '// &
     &               'array ARROUT_R4' )
                CALL EXIT ( 1 )
           END IF
      END IF
      ARROUT_R4 = 0.0
!
! --- Transfrom the distance to the ocean mask to the land-sea mask.
!
      IF ( IVRB .GE. 4 ) WRITE ( 6, * ) 'GEN_OTIDE: DLS_OCEAN_TO_LS' ; call flush ( 6 )
!
! --- Convert DLS masl to the land-sea mask suitable for computation of the ocean loading
!
      CALL DLS_OCEAN_TO_LS ( HEB_LS )
      IF ( IVRB .GE. 8 ) THEN
           VAL_MIN =  1.0
           VAL_MAX = -1.0
           CALL PLOT_GRID_R4 ( 1, 7, 0, 1, INT(HEB_LS%DIMS(1),KIND=4), &
     &                            INT(HEB_LS%DIMS(2),KIND=4), HEB_LS%VAL(1,1,1,1), &
        &                         'ls', 'd/l', VAL_MIN, VAL_MAX, '/tmp/boo', iuer )
      END IF
!
! --- Prepare the output
!
      HEB%MJD     = J2000__MJD  
      HEB%UTC     = 43200.0D0
      HEB%TAI     = 43200.0D0
      HEB%DIMS(1) = LLON
      HEB%DIMS(2) = LLAT 
      HEB%DIMS(3) = NCMP
      HEB%DIMS(4) = NWAV
      HEB%DATA_OFFSET = HEB__HDS
      HEB%ENDIAN      = HEB__LE
      HEB%DATA_TRANSFORM = HEB__NONE
      HEB%FILL_VALUE     = FILL_VALUE
      HEB%OFFSET         = 0.0
      HEB%SCALE_FACTOR   = 1.0
      HEB%DATA_COMPRESSION = HEB__NONE
      HEB%SDS_NAME       = 'Ocean tide height '//WAV_STR(1:I_LEN(WAV_STR))
      HEB%UNITS          = 'meter'
      HEB%DATA_FORMAT    = HEB__R4
      HEB%MIN_VALUE      = MINVAL_R4
      HEB%MAX_VALUE      = MAXVAL_R4
      HEB%VALID_RANGE(1) =  -30.0D0
      HEB%VALID_RANGE(2) =   30.0D0
      HEB%PROD_DATE_TIME = GET_CDATE()
!
      HEB%FILE_NAME      = FILOUT
      IF ( MODE == 'got' ) THEN
           HEB%SOURCE         = 'GOT 4.8 ocean tide model by Richard Ray'
           HEB%INSTITUTION    = 'NASA Goddard Space Flight Center'
           HEB%REFERENCES     = 'n/a'
           HEB%PROD_NAME      = 'Ocean tide height according to GOT4.8 model'
        ELSE IF ( INDEX ( DIRIN, 'got410c' ) > 0 ) THEN
           HEB%SOURCE         = 'TPXO8 ocean tide model by Garry Egbert and Lana Erofeeva'
           HEB%INSTITUTION    = 'Oregon State University, US'
           HEB%REFERENCES     = 'http://volkov.oce.orst.edu/tides/tpxo8_atlas.html'
           HEB%PROD_NAME      = 'Ocean tide height according to tpxo8 model'
        ELSE IF ( INDEX ( DIRIN, 'eot11a' ) > 0 ) THEN
           HEB%SOURCE         = 'EOT11a ocean tide model by ??'
           HEB%INSTITUTION    = 'DGFI, Technical University Graz, and GFZ'
           HEB%REFERENCES     = 'http://dx.doi.org/10.1016/j.jog.2011.10.009'
           HEB%PROD_NAME      = 'Ocean tide height according to eot11a model'
        ELSE IF ( INDEX ( DIRIN, 'fes2012' ) > 0 ) THEN
           HEB%SOURCE         = 'FES2012 ocean tide model by Carrere L., F. Lyard, M. Cancet, A. Guillot, L. Roblou'
           HEB%INSTITUTION    = 'CNES'
           HEB%REFERENCES     = 'n/a'
           HEB%PROD_NAME      = 'Ocean tide height according to fes2012 model'
        ELSE IF ( INDEX ( DIRIN, 'fes2014b' ) > 0 ) THEN
           HEB%SOURCE         = 'FES2014b ocean tide model by Carrere L., F. Lyard, M. Cancet, A. Guillot, N. Picot'
           HEB%INSTITUTION    = 'CNES'
           HEB%REFERENCES     = 'Carrere L., F. Lyard, M. Cancet, A. Guillot, N. Picot, Finite Element Solution FES2014, a new tidal model - Validation results and perspectives for improvements, presentation to ESA Living Planet Conference, Prague 2016.'
           HEB%PROD_NAME      = 'Ocean tide height according to fes2014b model'
           HEB%HISTORY        = 'FES2014 was produced by NOVELTIS, LEGOS, CLS Space Oceanography Division and CNES. It is distributed by AVISO, with support from CNES'
        ELSE
           CALL ERR_LOG ( 6816, IUER, 'GEN_OTIDE', 'Trap of internal '// &
     &         'control: the name of the input directory '// &
     &          DIRIN(1:I_LEN(DIRIN))//' does not contains '// &
     &         'a recognizable ocean tide model' )
           CALL EXIT ( 1 )
      END IF
!
      HEB%HISTORY        = 'Processed using the input of the ocean tide model'
      HEB%TITLE          = HEB%PROD_NAME      
      HEB%VERSION_ID     = MALO__LABEL
!
      IF ( FL_LARGE ) THEN
           DO 430 J3=1,NWAV
              IUER = -1
              CALL OTIDE_UPGRID ( NLON, NLAT, NCMP, J3, J3, NWAV, LLON, LLAT, &
     &                            FILL_VALUE, HEB_LS, ARRIN_R4, ARROUT_R4, IVRB, &
     &                            IUER )
              IF ( IUER .NE. 0 ) THEN
                   CALL ERR_LOG ( 6817, IUER, 'GEN_OTIDE', 'Error in an '// &
     &                 'attempt to perform ocean tidal model upgridding' )
                   RETURN 
              END IF
              IF ( IVRB > 1 ) THEN
                   WRITE ( 6, '(A$)' ) 'Writing results ... '
                   CALL FLUSH ( 6 )
              END IF
!
              IF ( J3 == 1 ) THEN
                   IUER = -1 
                   HEB%DIMS(4) = 1
                   CALL WRITE_HEB ( HEB, ARROUT_R4, FILOUT, IUER )
                   IF ( IUER .NE. 0 ) THEN
                        IUER = -1
                        CALL ERR_LOG ( 6818, IUER, 'GEN_OTIDE', 'Failure '// &
     &                      'to write ocean tide model into the output '// &
     &                       'file '//FILOUT )
                        CALL EXIT ( 1 )
                   END IF
                   HEB%DIMS(4) = NWAV
              END IF
              IS = FILE_INFO ( FILOUT(1:I_LEN(FILOUT))//CHAR(0), &
     &                         UNIX_DATE, SIZE_I8 )
              IF ( IS .NE. 0 ) THEN
                   CALL CLRCH  ( STR )
                   CALL GERROR ( STR )
                   CALL ERR_LOG ( 6819, IUER, 'GEN_OTIDE', 'Error in inquiring '// &
     &                 'information about the output file '// &
     &                  FILOUT(1:I_LEN(FILOUT))//' -- '//STR )
                   CALL EXIT ( 1 )
              END IF
!
              IF ( J3 == NWAV ) THEN
                   HEB%STATUS = HEB__HDON
                   IUER = -1
                   CALL WRITE_HEB ( HEB, ARROUT_R4, FILOUT, IUER )
                   IF ( IUER .NE. 0 ) THEN
                        IUER = -1
                        CALL ERR_LOG ( 6820, IUER, 'GEN_OTIDE', 'Failure '// &
     &                      'to write ocean tide model into the output '// &
     &                       'file '//FILOUT )
                        CALL EXIT ( 1 )
                   END IF
                   HEB%STATUS = HEB__LOAD
              END IF
!
              IF ( J3 > 1 ) THEN
!
! ---------------- Open the output file for writing
!
                   IUER = -1
                   CALL BINF_OPEN_NEW ( FILOUT, 'APPEND', LUN, IUER )
                   IF ( IUER .NE. 0 ) THEN
                        IUER = -1
                        CALL ERR_LOG ( 6821, IUER, 'GEN_OTIDE', 'Error in '// &
     &                      'an attempt to re-open output file '//FILOUT )
                        CALL EXIT ( 1 )
                   END IF
!
                   LEN_I8 = INT8(4)*INT8(LLON)*INT8(LLAT)*INT8(NCMP)
                   IUER = -1
                   CALL BIG_WRITE ( LUN, SIZE_I8, LEN_I8, ARROUT_R4, IUER )
                   IF ( IUER .NE. 0 ) THEN
                        IUER = -1
                        CALL ERR_LOG ( 6822, IUER, 'GEN_OTIDE', 'Failure in an attempt '// &
     &                      'to write to the end of the output file '// &
     &                       FILOUT(1:I_LEN(FILOUT))//' -- '//STR )
                        CALL EXIT ( 1 )
                   END IF
!
                   CALL BINF_CLOSE ( LUN, IUER )
                   IF ( IUER .NE. 0 ) THEN
                        IUER = -1
                        CALL ERR_LOG ( 6823, IUER, 'GEN_OTIDE', 'Error in '// &
     &                      'attempt to close file '//FILOUT )
                        CALL EXIT ( 1 )
                   END IF
              END IF
              IF ( IVRB > 1 ) THEN
                   WRITE ( 6, '(A)' ) 'Results are written'
                   CALL FLUSH ( 6 )
              END IF
 430       CONTINUE
         ELSE
!
! -------- Small size. We can process all waves simultaneously
!
           IUER = -1
!
! -------- Ocean tide model upgridding 
!
           CALL OTIDE_UPGRID ( NLON, NLAT, NCMP, 1, NWAV, NWAV, LLON, LLAT, &
     &                         FILL_VALUE, HEB_LS, ARRIN_R4, ARROUT_R4, IVRB, IUER )
           IF ( IUER .NE. 0 ) THEN
                CALL ERR_LOG ( 6824, IUER, 'GEN_OTIDE', 'Error in an '// &
     &              'attempt to perform ocean tidal model upgridding' )
                RETURN 
           END IF
!
! -------- Write the output ocean tide maps
!
           IUER = -1 
           CALL WRITE_HEB ( HEB, ARROUT_R4, FILOUT, IUER )
           IF ( IUER .NE. 0 ) THEN
                IUER = -1
                CALL ERR_LOG ( 6825, IUER, 'GEN_OTIDE', 'Failure '// &
     &              'to write ocean tide model into the output '// &
     &              'file '//FILOUT )
                CALL EXIT ( 1 )
           END IF
      END IF
!
      WRITE ( 6, '(A)' ) 'The ocean tide model is written in '// &
     &                    FILOUT(1:I_LEN(FILOUT))
      END  PROGRAM  GEN_OTIDE  !#!#
