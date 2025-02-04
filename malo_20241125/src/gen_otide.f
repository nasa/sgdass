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
     &           WAV_ARR(MF)*8, WAV_STR*1024, FIL_LS_MASK*128, MODE*4, WAVE_ENV*16
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
         ELSE IF ( MODE == 'psi1' ) THEN
           CONTINUE
         ELSE IF ( MODE == 'eot' ) THEN
           CONTINUE
         ELSE IF ( MODE == 'fes' ) THEN
           CONTINUE
         ELSE 
           CALL ERR_LOG ( 6801, IUER, 'GEN_OTIDE', 'Wrong mode '// &
     &          STR(1:I_LEN(STR))//' while only modes got, tpx, psi1, eot, '// &
     &          'or fes are supported' )
           CALL EXIT ( 1 )
      END IF
      IF ( INDEX ( DIRIN, 'got48' ) > 0 ) THEN
           CONTINUE 
        ELSE IF ( INDEX ( DIRIN, 'got410c' ) > 0 ) THEN
           CONTINUE 
        ELSE IF ( INDEX ( DIRIN, 'tpx08' ) > 0 ) THEN
           CONTINUE 
        ELSE IF ( INDEX ( DIRIN, 'psi1' ) > 0 ) THEN
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
           ELSE IF ( MODE == 'tpx' .OR. MODE == 'psi1' ) THEN
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
     &            'to read the '//STR(1:I_LEN(STR))//'th file '//FILS(J2) )
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
        ELSE IF ( MODE == 'tpx' .OR. MODE == 'psi1' ) THEN
!
! -------- ??
!
   write ( 6, * ) ' NLON, NLAT, NCMP, NWAV = ', NLON, NLAT, NCMP, NWAV, SCALE ! %%%%%%%%%
           IF ( INDEX ( FILS(J1), '_30c.nc'  ) > 0 ) THEN
                IUER = -1
                CALL CMPLX_ARR_SWAP ( NLON, NLAT, NCMP, NWAV, SCALE, VAL_MAX_R4, &
     &                                FILL_VALUE, FILS, ARRIN_R4, &
     &                                MINVAL_R4, MAXVAL_R4, IUER )
                 IF ( IUER .NE. 0 ) THEN
                      IUER = -1
                      CALL ERR_LOG ( 6813, IUER, 'GEN_OTIDE', 'Failure in an attempt '// &
     &                    'to clean the ocean tide model' )
                      CALL EXIT ( 1 )
                 END IF
           END IF
                write ( 6, * ) 'GEN_OTIDE: fls = ', trim(fils(1)) ! %%%%%%%%%%%%
           IF ( INDEX ( FILS(1), 'psi1'  ) > 0 ) THEN
                IUER = -1
                CALL AMP_PHS_ARR ( NLON, NLAT, NCMP, NWAV, SCALE, VAL_MAX_R4, &
     &                             FILL_VALUE, ARRIN_R4, MINVAL_R4, MAXVAL_R4, IUER )
                IF ( IUER .NE. 0 ) THEN
                     IUER = -1
                     CALL ERR_LOG ( 6812, IUER, 'GEN_OTIDE', 'Failure in an attempt '// &
     &                   'to clean the ocean tide model' )
                     CALL EXIT ( 1 )
                END IF
                write ( 6, * ) 'GEN_OTIDE: MINVAL_R4, MAXVAL_R4 ', MINVAL_R4, MAXVAL_R4 ! %%%%%%%%%%%%
           END IF
!
           IUER = -1
           CALL GET_TPXO8_LS ( LF, FILS, NLON, NLAT, ARRIN_R4, LS, IUER )
           IF ( IUER .NE. 0 ) THEN
                IUER = -1
                CALL ERR_LOG ( 6882, IUER, 'GEN_OTIDE', 'Failure in '// &
     &              'an attempt to appllly land-water mask to the ocean data' )
                CALL EXIT ( 1 )
           END IF
!
           IUER = -1
           CALL TPXO8_UPDATE_ARR ( NLON, NLAT, NCMP, NWAV, SCALE, &
     &                             FILL_VALUE, FILS, ARRIN_R4, LS, &
     &                             MINVAL_R4, MAXVAL_R4, IUER )
           IF ( IUER .NE. 0 ) THEN
                IUER = -1
                CALL ERR_LOG ( 6883, IUER, 'GEN_OTIDE', 'Failure in '// &
     &              'an attempt to clean the ocean tide model' )
                CALL EXIT ( 1 )
           END IF
!
           write ( 6, * ) 'GEN_OTIDE MINVAL_R4, MAXVAL_R4= ', MINVAL_R4, MAXVAL_R4 ! %%%%%%%%
           IF ( IVRB .GE. 7 ) THEN
                VAL_MIN =  1.0
                VAL_MAX = -1.0
                CALL PLOT_GRID_R4 ( 1, 7, 0, 1, NLON, NLAT, ARRIN_R4(1,1,1,1), &
        &                           'Ampl_in', 'cm', VAL_MIN, VAL_MAX, '/tmp/boo', iuer )
                CALL PLOT_GRID_R4 ( 1, 7, 0, 1, NLON, NLAT, ARRIN_R4(1,1,2,1), &
        &                          'Phase', 'deg', VAL_MIN, VAL_MAX, '/tmp/boo', iuer )
           END IF
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
        ELSE IF ( INDEX ( DIRIN, 'psi1' ) > 0 ) THEN
           HEB%SOURCE         = 'R Ray, 2021, in preparation'
           HEB%INSTITUTION    = 'NASA Goddard Space Flight Center'
           HEB%REFERENCES     = 'Ray, 2021, in preparation'
           HEB%PROD_NAME      = 'Ocean tide height of psi1 wave'
           HEB%HISTORY        = ' '
        ELSE
           IUER = -1
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
!
! ------------------------------------------------------------------------
!
      SUBROUTINE INQ_GOT ( FILIN, NLON, NLAT, WAVE_NAME, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine INQ_GOT 
! *                                                                      *
! *  ### 14-APR-2014     INQ_GOT   v1.0 (c)  L. Petrov  14-APR-2014 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INTEGER*4  NLON, NLAT, IUER
      CHARACTER  FILIN*(*), WAVE_NAME*(*)
      CHARACTER  STR*128
      CHARACTER, ALLOCATABLE :: BUF(:)*80
      INTEGER*8  SIZE_I8 
      INTEGER*4  MIND
      PARAMETER  ( MIND = 32  )
      INTEGER*4  UNIX_DATE, IS, MBUF, NBUF, LIND, IND(2,MIND), IER
      INTEGER*4, EXTERNAL :: FILE_INFO, ILEN, I_LEN
!
      IS = FILE_INFO ( FILIN(1:I_LEN(FILIN))//CHAR(0), UNIX_DATE, SIZE_I8 )
      IF ( IS .NE. 0 ) THEN
           CALL GERROR ( STR )
           CALL ERR_LOG ( 6841, IUER, 'INQ_GOT', 'Error in inquiring '// &
     &         'information about file '//FILIN(1:I_LEN(FILIN))//' -- '// &
     &          STR )
           RETURN 
      END IF
!
      MBUF =  SIZE_I8/75 + 256
      ALLOCATE ( BUF(MBUF), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( 80*MBUF, STR )
           CALL ERR_LOG ( 6842, IUER, 'INQ_GOT', 'Failure in an attempt '// &
     &         'to allocate '//STR(1:I_LEN(STR))//' bytes of dynamic '// &
     &         'memory for array BUF' )
           RETURN 
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL RD_TEXT  ( FILIN, MBUF, BUF, NBUF, IER )
      IF ( IER .NE. 0 ) THEN
           DEALLOCATE ( BUF )
           CALL ERR_LOG ( 6843, IUER, 'INQ_GOT', 'Error in an attempt '// &
     &         'to read input file '//FILIN ) 
           RETURN 
      END IF
!
      CALL EXWORD ( BUF(1), MIND, LIND, IND, CHAR(0)//CHAR(32)//CHAR(0), IER )
      WAVE_NAME = BUF(1)(IND(1,1):IND(2,1))
      CALL EXWORD ( BUF(3), MIND, LIND, IND, CHAR(0)//CHAR(32)//CHAR(0), IER )
      READ ( UNIT=BUF(3)(IND(1,1):IND(2,1)), FMT='(I6)' ) NLAT
      READ ( UNIT=BUF(3)(IND(1,2):IND(2,2)), FMT='(I6)' ) NLON
!
      DEALLOCATE ( BUF )
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  INQ_GOT  !#!  
!
! ------------------------------------------------------------------------
!
      SUBROUTINE READ_GOT ( FILIN, NLON, NLAT, WAV, ARR_R4, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine READ_GOT 
! *                                                                      *
! *  ### 14-APR-2014    READ_GOT   v1.0 (c)  L. Petrov  14-APR-2014 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INTEGER*4  NLON, NLAT, IUER
      CHARACTER  FILIN*(*), WAV*(*)
      REAL*4     ARR_R4(NLON*NLAT,2)
      CHARACTER, ALLOCATABLE :: BUF(:)*128
      CHARACTER  STR*128, FMT*10
      INTEGER*8  SIZE_I8 
      INTEGER*4  MIND
      PARAMETER  ( MIND = 32 )
      INTEGER*4  UNIX_DATE, IS, MBUF, NBUF, LIND, IND(2,MIND), IB, IL, &
     &           J1, J2, NL, IER
      INTEGER*4, EXTERNAL :: FILE_INFO, ILEN, I_LEN
!
      IS = FILE_INFO ( FILIN(1:I_LEN(FILIN))//CHAR(0), UNIX_DATE, SIZE_I8 )
      IF ( IS .NE. 0 ) THEN
           CALL GERROR ( STR )
           CALL ERR_LOG ( 6851, IUER, 'READ_GOT', 'Error in inquiring '// &
     &         'information about file '//FILIN(1:I_LEN(FILIN))//' -- '// &
     &          STR )
      END IF
!
      MBUF =  SIZE_I8/77 + 256
      ALLOCATE ( BUF(MBUF), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( 80*MBUF, STR )
           CALL ERR_LOG ( 6852, IUER, 'READ_GOT', 'Failure in an attempt '// &
     &         'to allocate '//STR(1:I_LEN(STR))//' bytes of dynamic '// &
     &         'memory for array BUF' )
           RETURN 
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL RD_TEXT  ( FILIN, MBUF, BUF, NBUF, IER )
      IF ( IER .NE. 0 ) THEN
           DEALLOCATE ( BUF )
           CALL ERR_LOG ( 6853, IUER, 'READ_GOT', 'Error in an attempt '// &
     &         'to read input file '//FILIN ) 
           RETURN 
      END IF
!
      CALL EXWORD ( BUF(1), MIND, LIND, IND, CHAR(0)//CHAR(32)//CHAR(0), IER )
      WAV = BUF(1)(IND(1,1):IND(2,1))
      FMT = BUF(7)(1:10)
      NL = NLON*NLAT
      IL = 1
      DO 410 J1=1,2
         IB = 1
         IL = IL + 7
         DO 420 J2=1,NL
            CALL EXWORD ( BUF(IL), MIND, LIND, IND, CHAR(0)//CHAR(32)//CHAR(0), IER )
            READ ( UNIT=BUF(IL), FMT='('//FMT(1:I_LEN(FMT))//')' ) ARR_R4(IB:IB+LIND-1,J1)
            IL = IL + 1
            IB = IB + LIND
            IF ( IB > NLON*NLAT ) GOTO 820
 420     CONTINUE 
 820     CONTINUE 
 410  CONTINUE 
!
      DEALLOCATE ( BUF )
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  READ_GOT  !#!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE AMP_PHS_ARR ( NLON, NLAT, NCMP, NWAV, SCALE, VAL_MAX, &
     &                         FILL_VALUE, ARRIN_R4, MINVAL_R4, &
     &                         MAXVAL_R4, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine AMP_PHS_ARR 
! *                                                                      *
! *  ### 16-APR-2014   AMP_PHS_ARR  v1.1 (c)  L. Petrov  26-MAY-2014 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INTEGER*4  NLON, NLAT, NCMP, NWAV, IUER
      REAL*4     ARRIN_R4(NLON,NLAT,NCMP,NWAV), SCALE, VAL_MAX, FILL_VALUE, &
     &           MINVAL_R4, MAXVAL_R4
      REAL*4     CMP_COS, CMP_SIN, PHS_MAX
      PARAMETER  ( PHS_MAX = 365.0 )
      INTEGER*4  J1, J2, J3
!
      MAXVAL_R4 = -1.0E30
      MINVAL_R4 =  1.0E30
      DO 410 J1=1,NWAV
         DO 420 J2=1,NLAT
            DO 430 J3=1,NLON
               IF ( ABS(ARRIN_R4(J3,J2,1,J1)) > VAL_MAX .OR. &
     &              ABS(ARRIN_R4(J3,J2,2,J1)) > PHS_MAX      ) THEN
                    CMP_COS = FILL_VALUE
                    CMP_SIN = FILL_VALUE
                  ELSE 
                    CMP_COS = SCALE*ARRIN_R4(J3,J2,1,J1)*DCOS(ARRIN_R4(J3,J2,2,J1)*DEG__TO__RAD)
                    CMP_SIN = SCALE*ARRIN_R4(J3,J2,1,J1)*DSIN(ARRIN_R4(J3,J2,2,J1)*DEG__TO__RAD)
                    MAXVAL_R4 = MAX ( CMP_COS, MAXVAL_R4 )
                    MAXVAL_R4 = MAX ( CMP_SIN, MAXVAL_R4 )
                    MINVAL_R4 = MIN ( CMP_COS, MINVAL_R4 )
                    MINVAL_R4 = MIN ( CMP_SIN, MINVAL_R4 )
               END IF
               ARRIN_R4(J3,J2,1,J1) = CMP_COS
               ARRIN_R4(J3,J2,2,J1) = CMP_SIN
 430        CONTINUE 
 420     CONTINUE 
 410  CONTINUE 
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  AMP_PHS_ARR  !#!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE INQ_NC ( FILIN, NLON, NLAT, WAVE_NAME, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  INQ_NC
! *                                                                      *
! *  ### 16-APR-2014     INQ_NC    v2.0 (c)  L. Petrov  17-APR-2014 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'netcdf.inc'
      INTEGER*4  NLON, NLAT, IUER
      CHARACTER  FILIN*(*), WAVE_NAME*(*)
      CHARACTER  STR*128, TITLE*128
      INTEGER*4  IS, NCID, LEN_ATM3, MIND
      PARAMETER  ( MIND = 32 )
      INTEGER*4  ID_VAR_LON, ID_VAR_LAT, ID_VAR_CON, &
     &           ID_DIM_LON, ID_DIM_LAT, ID_DIM_CON, &
     &           DIMLEN_LON, DIMLEN_LAT, DIMLEN_CON, &
     &           IP, ID, LIND, IND(2,MIND), IER
      INTEGER*4, EXTERNAL :: FILE_INFO, ILEN, I_LEN, LINDEX
!
      IS = NF_OPEN ( FILIN, NF_NOWRITE, NCID )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 6861, IUER, 'INQ_NC', 'File '// &
     &          FILIN(1:I_LEN(FILIN))//' -- Error in NF_OPEN: '// &
     &          NF_STRERROR(IS)//' File: '//FILIN )
           RETURN
      END IF
!
! --- Learn the ID of the dimension "lon"
!
      IS = NF_INQ_DIMID ( NCID, 'nx', ID_DIM_LON )
      IF ( IS .NE. 0 ) IS = NF_INQ_DIMID ( NCID, 'x', ID_DIM_LON )
      IF ( IS .NE. 0 ) IS = NF_INQ_DIMID ( NCID, 'lon', ID_DIM_LON )
      IF ( IS .NE. 0 ) IS = NF_INQ_DIMID ( NCID, 'longitude', ID_DIM_LON )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 6862, IUER, 'INQ_NC', 'Dimension "lon" was '// &
     &         'not found in the input netcdf file '// &
     &          FILIN(1:I_LEN(FILIN))//' error: '// &
     &          NF_STRERROR(IS)//' File: '//FILIN )
           RETURN
      END IF
!
! --- Get the length of the dimension "lon"
!
      IS = NF_INQ_DIMLEN ( NCID, ID_DIM_LON, DIMLEN_LON  )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 6863, IUER, 'INQ_NC', 'Error in getting the '// &
     &         'length of the dimension "lon" in file '// &
     &          FILIN(1:I_LEN(FILIN))//' error: '// &
     &          NF_STRERROR(IS) )
           RETURN
      END IF
!
! --- Learn the ID of the dimension "lat"
!
      IS = NF_INQ_DIMID ( NCID, 'ny', ID_DIM_LAT )
      IF ( IS .NE. 0 ) IS = NF_INQ_DIMID ( NCID, 'y',        ID_DIM_LAT )
      IF ( IS .NE. 0 ) IS = NF_INQ_DIMID ( NCID, 'lat', ID_DIM_LAT )
      IF ( IS .NE. 0 ) IS = NF_INQ_DIMID ( NCID, 'latitude', ID_DIM_LAT )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 6864, IUER, 'INQ_NC', 'Dimension "lat" was '// &
     &                   'not found in the input netcdf file '// &
     &                    FILIN(1:I_LEN(FILIN))//' error: '// &
     &                    NF_STRERROR(IS) )
           RETURN
      END IF
!
! --- Get the length of the dimension "lat"
!
      IS = NF_INQ_DIMLEN ( NCID, ID_DIM_LAT, DIMLEN_LAT  )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 6865, IUER, 'INQ_NC', 'Error in getting the '// &
     &                   'length of the dimension "lat" in file '// &
     &                    FILIN(1:I_LEN(FILIN))//' error: '// &
     &                    NF_STRERROR(IS) )
           RETURN
      END IF
!
      CALL CLRCH ( TITLE )
      IS = NF_GET_ATT_TEXT ( NCID, NF_GLOBAL, 'title', %REF(TITLE) )
      IF ( IS == 0 ) THEN
           IF ( TITLE(1:4) == 'TPXO' ) THEN
!
! ------------- Learn the ID of the variable "con"
!
                IS = NF_INQ_VARID ( NCID, 'con', ID_VAR_CON )
                IF ( IS .NE. 0 ) THEN
                     CALL ERR_LOG ( 6866, IUER, 'INQ_NC', 'Variable "con" was '// &
     &                   'not found in the input netcdf file '// &
     &                    FILIN(1:I_LEN(FILIN))//' error: '//NF_STRERROR(IS) )
                     RETURN
                END IF
!
! ------------- Get the variable: constituent name 
!
                IS = NF_GET_VAR_TEXT ( NCID, ID_VAR_CON, WAVE_NAME )
                IF ( IS .NE. 0 ) THEN
                     CALL ERR_LOG ( 6867, IUER, 'INQ_NC', 'Error in getting '// &
     &                             'the values of the variable "cov" in file '// &
     &                              FILIN(1:I_LEN(FILIN))//' error: '// &
     &                              NF_STRERROR(IS) )
                     RETURN
                END IF
              ELSE 
                CALL EXWORD ( TITLE, MIND, LIND, IND, CHAR(32), IER )
                IF ( LIND == 1  ) THEN
                     CALL ERR_LOG ( 6868, IUER, 'INQ_NC', 'Error in parsing the '// &
     &                 'global attribute "title" '//TITLE(1:I_LEN(TITLE))// &
     &                 'in file '//FILIN  )
                     RETURN
                   ELSE
                     WAVE_NAME = TITLE(IND(1,2):IND(2,2))
                END IF
           END IF
         ELSE 
           ID = LINDEX ( FILIN, '/' ) + 1
           WAVE_NAME = FILIN(ID:)
           IP = INDEX ( WAVE_NAME, '_' ) 
           IF ( IP > 1 ) CALL CLRCH ( WAVE_NAME(IP:) )
      END IF
!
      NLON = DIMLEN_LON
      NLAT = DIMLEN_LAT  
!
      IS = NF_CLOSE ( NCID )
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  INQ_NC  !#!  
!
! ------------------------------------------------------------------------
!
      SUBROUTINE READ_OT_NC ( FILIN, NLON, NLAT, WAVE_NAME, ARR_R4, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  READ_OT_NC 
! *                                                                      *
! *  ### 16-APR-2014   READ_OT_NC  v1.0 (c)  L. Petrov  16-APR-2014 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'netcdf.inc'
      INTEGER*4  NLON, NLAT, IUER
      CHARACTER  FILIN*(*), WAVE_NAME*(*)
      REAL*4     ARR_R4(NLON*NLAT,2)
      CHARACTER  STR*128, TITLE*128
      INTEGER*4  ID_VAR_LON, ID_VAR_LAT, ID_VAR_CON, &
     &           ID_DIM_LON, ID_DIM_LAT, ID_DIM_CON, &
     &           DIMLEN_LON, DIMLEN_LAT, DIMLEN_CON, &
     &           ID_VAR_HRE, ID_VAR_HIM, IP, ID, IER
      INTEGER*4  MIND
      PARAMETER  ( MIND = 32 )
      INTEGER*4  IS, NCID, LEN_ATM3, LIND, IND(2,MIND)
      INTEGER*4, EXTERNAL :: FILE_INFO, ILEN, I_LEN, LINDEX
!
      IS = NF_OPEN ( FILIN, NF_NOWRITE, NCID )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 6871, IUER, 'READ_OT_NC', 'File '// &
     &          FILIN(1:I_LEN(FILIN))//' -- Error in NF_OPEN: '// &
     &          NF_STRERROR(IS)//' File: '//FILIN )
           RETURN
      END IF
!
! --- Learn the ID of the dimension "lon"
!
      IS = NF_INQ_DIMID ( NCID, 'nx', ID_DIM_LON )
      IF ( IS .NE. 0 ) IS = NF_INQ_DIMID ( NCID, 'x', ID_DIM_LON )
      IF ( IS .NE. 0 ) IS = NF_INQ_DIMID ( NCID, 'lon', ID_DIM_LON )
      IF ( IS .NE. 0 ) IS = NF_INQ_DIMID ( NCID, 'longitude', ID_DIM_LON )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 6872, IUER, 'READ_OT_NC', 'Dimension "lon" '// &
     &         'was not found in the input netcdf file '// &
     &          FILIN(1:I_LEN(FILIN))//' error: '// &
     &          NF_STRERROR(IS)//' File: '//FILIN )
           RETURN
      END IF
!
! --- Get the length of the dimension "lon"
!
      IS = NF_INQ_DIMLEN ( NCID, ID_DIM_LON, DIMLEN_LON  )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 6873, IUER, 'READ_OT_NC', 'Error in getting '// &
     &         'the length of the dimension "lon" in file '// &
     &          FILIN(1:I_LEN(FILIN))//' error: '// &
     &          NF_STRERROR(IS) )
           RETURN
      END IF
!
! --- Learn the ID of the dimension "lat"
!
      IS = NF_INQ_DIMID ( NCID, 'ny', ID_DIM_LAT )
      IF ( IS .NE. 0 ) IS = NF_INQ_DIMID ( NCID, 'y', ID_DIM_LAT )
      IF ( IS .NE. 0 ) IS = NF_INQ_DIMID ( NCID, 'lat', ID_DIM_LAT )
      IF ( IS .NE. 0 ) IS = NF_INQ_DIMID ( NCID, 'latitude', ID_DIM_LAT )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 6874, IUER, 'READ_OT_NC', 'Dimension "lat" '// &
     &                   'was not found in the input netcdf file '// &
     &                    FILIN(1:I_LEN(FILIN))//' error: '// &
     &                    NF_STRERROR(IS) )
           RETURN
      END IF
!
! --- Get the length of the dimension "lat"
!
      IS = NF_INQ_DIMLEN ( NCID, ID_DIM_LAT, DIMLEN_LAT  )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 6875, IUER, 'READ_OT_NC', 'Error in getting the '// &
     &                   'length of the dimension "lat" in file '// &
     &                    FILIN(1:I_LEN(FILIN))//' error: '// &
     &                    NF_STRERROR(IS) )
           RETURN
      END IF
!
      CALL CLRCH ( TITLE )
      IS = NF_GET_ATT_TEXT ( NCID, NF_GLOBAL, 'title', %REF(TITLE) )
      IF ( IS .EQ. 0 ) THEN
           IF ( TITLE(1:4) == 'TPXO' ) THEN
!
! ------------- Learn the ID of the variable "con"
!
                IS = NF_INQ_VARID ( NCID, 'con', ID_VAR_CON )
                IF ( IS .NE. 0 ) THEN
                     CALL ERR_LOG ( 6876, IUER, 'READ_OT_NC', 'Variable "con" was '// &
     &                   'not found in the input netcdf file '// &
     &                    FILIN(1:I_LEN(FILIN))//' error: '//NF_STRERROR(IS) )
                     RETURN
                END IF
!
! ------------- Get the variable: constituent name 
!
                IS = NF_GET_VAR_TEXT ( NCID, ID_VAR_CON, WAVE_NAME )
                IF ( IS .NE. 0 ) THEN
                     CALL ERR_LOG ( 6877, IUER, 'READ_OT_NC', 'Error in getting '// &
     &                             'the values of the variable "cov" in file '// &
     &                              FILIN(1:I_LEN(FILIN))//' error: '// &
     &                              NF_STRERROR(IS) )
                     RETURN
                END IF
              ELSE 
                CALL EXWORD ( TITLE, MIND, LIND, IND, CHAR(32), IER )
                IF ( LIND == 1  ) THEN
                     CALL ERR_LOG ( 6878, IUER, 'READ_OT_NC', 'Error in parsing the '// &
     &                 'global attribute "title" '//TITLE(1:I_LEN(TITLE))// &
     &                 'in file '//FILIN  )
                     RETURN
                   ELSE
                     WAVE_NAME = TITLE(IND(1,2):IND(2,2))
                END IF
           END IF
         ELSE 
           ID = LINDEX ( FILIN, '/' ) + 1
           WAVE_NAME = FILIN(ID:)
           IP = INDEX ( WAVE_NAME, '_' ) 
           IF ( IP > 1 ) CALL CLRCH ( WAVE_NAME(IP:) )
           IP = INDEX ( WAVE_NAME, '.' )
           IF ( IP > 1 ) CALL CLRCH ( WAVE_NAME(IP:) )
      END IF
!
! --- Learn the ID of the variable "hRe"
!
      IS = NF_INQ_VARID ( NCID, 'hRe', ID_VAR_HRE )
      IF ( IS .NE. 0 ) IS = NF_INQ_VARID ( NCID, 're', ID_VAR_HRE )
      IF ( IS .NE. 0 ) IS = NF_INQ_VARID ( NCID, 'Ha', ID_VAR_HRE )
      IF ( IS .NE. 0 ) IS = NF_INQ_VARID ( NCID, 'amplitude', ID_VAR_HRE )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 6879, IUER, 'READ_OT_NC', 'Variable "con" was '// &
     &         'not found in the input netcdf file '// &
     &          FILIN(1:I_LEN(FILIN))//' error: '//NF_STRERROR(IS) )
           RETURN
      END IF
!
! --- Learn the ID of the variable "hIm"
!
      IS = NF_INQ_VARID ( NCID, 'hIm', ID_VAR_HIM )
      IF ( IS .NE. 0 ) IS = NF_INQ_VARID ( NCID, 'im', ID_VAR_HIM )
      IF ( IS .NE. 0 ) IS = NF_INQ_VARID ( NCID, 'Hg', ID_VAR_HIM )
      IF ( IS .NE. 0 ) IS = NF_INQ_VARID ( NCID, 'phase', ID_VAR_HIM )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 6880, IUER, 'READ_OT_NC', 'Variable "con" was '// &
     &         'not found in the input netcdf file '// &
     &          FILIN(1:I_LEN(FILIN))//' error: '//NF_STRERROR(IS) )
           RETURN
      END IF
!
      IS = NF_GET_VAR_REAL ( NCID, ID_VAR_HRE, ARR_R4(1,1) )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 6881, IUER, 'READ_OT_NC', 'Error in getting '// &
     &                   'the values of the variable "hRe" in file '// &
     &                    FILIN(1:I_LEN(FILIN))//' error: '// &
     &                    NF_STRERROR(IS) )
           RETURN
      END IF
!
      IS = NF_GET_VAR_REAL ( NCID, ID_VAR_HIM, ARR_R4(1,2) )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 6882, IUER, 'READ_OT_NC', 'Error in getting '// &
     &                   'the values of the variable "hIm" in file '// &
     &                    FILIN(1:I_LEN(FILIN))//' error: '// &
     &                    NF_STRERROR(IS) )
           RETURN
      END IF
!
      IS = NF_CLOSE ( NCID )
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  READ_OT_NC  !#!  
!
! ------------------------------------------------------------------------
!
      SUBROUTINE CMPLX_ARR_SWAP ( NLON, NLAT, NCMP, NWAV, SCALE, VAL_MAX, &
     &                            FILL_VALUE, FILS, ARRIN_R4, &
     &                            MINVAL_R4, MAXVAL_R4, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  CMPLX_ARR_SWAP
! *                                                                      *
! * ### 16-APR-2014  CMPLX_ARR_SWAP  v1.0 (c) L. Petrov  16-APR-2014 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INTEGER*4  NLON, NLAT, NCMP, NWAV, IUER
      REAL*4     ARRIN_R4(NLON,NLAT,NCMP,NWAV), SCALE, VAL_MAX, FILL_VALUE, &
     &           MINVAL_R4, MAXVAL_R4
      CHARACTER  FILS(NWAV)*(*)
      REAL*4     CMP_COS, CMP_SIN
      REAL*4,    ALLOCATABLE :: TMP_ARR(:,:,:)
      INTEGER*4  J1, J2, J3
!
      MAXVAL_R4 = -1.0E30
      MINVAL_R4 =  1.0E30
      ALLOCATE ( TMP_ARR(NLAT,NLON,2) )
      DO 410 J1=1,NWAV
         IF ( INDEX ( FILS(J1), '_30c.nc' ) .LE. 0 ) GOTO 410
         CALL LIB$MOVC3 ( 4*NLON*NLAT*2, ARRIN_R4(1,1,1,J1), TMP_ARR )
         DO 420 J2=1,NLAT
            DO 430 J3=1,NLON
               IF ( ABS(TMP_ARR(J2,J3,1)) > VAL_MAX .OR. &
     &              ABS(TMP_ARR(J2,J3,2)) > VAL_MAX      ) THEN
                    CMP_COS = FILL_VALUE
                    CMP_SIN = FILL_VALUE
                  ELSE 
                    CMP_COS =  SCALE*TMP_ARR(J2,J3,1)
                    CMP_SIN = -SCALE*TMP_ARR(J2,J3,2)
                    MAXVAL_R4 = MAX ( CMP_COS, MAXVAL_R4 )
                    MAXVAL_R4 = MAX ( CMP_SIN, MAXVAL_R4 )
                    MINVAL_R4 = MIN ( CMP_COS, MINVAL_R4 )
                    MINVAL_R4 = MIN ( CMP_SIN, MINVAL_R4 )
               END IF
               ARRIN_R4(J3,J2,1,J1) = CMP_COS
               ARRIN_R4(J3,J2,2,J1) = CMP_SIN
 430        CONTINUE 
 420     CONTINUE 
 410  CONTINUE 
!
      DEALLOCATE ( TMP_ARR )
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  CMPLX_ARR_SWAP  !#!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE GET_TPXO8_LS ( LF, FILS, NLON, NLAT, ARR_R4, LS, IUER )
      IMPLICIT   NONE 
      INTEGER*4  LF, NLON, NLAT, IUER
      CHARACTER  FILS(LF)*(*)
      REAL*4     ARR_R4(NLON,NLAT,2,LF), LS(NLON,NLAT)
      REAL*4     VAL_MIN
      PARAMETER  ( VAL_MIN = 0.0001 )
      LOGICAL*1  FL_HR(8192), FL_LAND
      INTEGER*4  J1, J2, J3, J4
!
      DO 410 J1=1,LF
         IF ( INDEX ( FILS(J1), '_30c.nc'  ) > 0 .OR. &
     &        INDEX ( FILS(J1), 'psi1_ray' ) > 0      ) THEN
              FL_HR(J1) = .TRUE.
            ELSE 
              FL_HR(J1) = .FALSE.
         END IF
 410  CONTINUE 
!! FL_HR = .true. ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!
      DO 420 J2=1,NLAT 
         DO 430 J3=1,NLON
            FL_LAND = .TRUE.
            DO 440 J4=1,LF
               IF ( FL_HR(J4) ) THEN
                    IF ( ABS(ARR_R4(J3,J2,1,J4)) > VAL_MIN ) FL_LAND = .FALSE.
                    IF ( ABS(ARR_R4(J3,J2,2,J4)) > VAL_MIN ) FL_LAND = .FALSE.
               END IF
 440        CONTINUE 
            IF ( FL_LAND ) THEN
                 LS(J3,J2) = 1.0
               ELSE 
                 LS(J3,J2) = 0.0
            END IF
 430     CONTINUE 
 420  CONTINUE 
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  GET_TPXO8_LS !#!  
!
! ------------------------------------------------------------------------
!
      SUBROUTINE TPXO8_UPDATE_ARR ( NLON, NLAT, NCMP, NWAV, SCALE, &
     &                              FILL_VALUE, FILS, ARR_R4, LS, &
     &                              MINVAL_R4, MAXVAL_R4, IUER )
      IMPLICIT   NONE 
      INTEGER*4  NLON, NLAT, NCMP, NWAV, IUER
      CHARACTER  FILS(NWAV)*(*)
      REAL*4     ARR_R4(NLON,NLAT,2,NWAV), LS(NLON,NLAT)
      REAL*4,    ALLOCATABLE :: TMP_ARR(:,:,:)
      REAL*4     SCALE, FILL_VALUE, MINVAL_R4, MAXVAL_R4
      LOGICAL*1  FL_HR(8192), FL_LAND
      REAL*4     CMP_COS, CMP_SIN
      INTEGER*4  J1, J2, J3, J4, J5, J6, J7, LLAT, LLON, &
     &           INDC_LAT, INDC_LON, IND_LAT, IND_LON
!
      LLAT = (NLAT-1)/5+1
      LLON =  NLON/5
      ALLOCATE ( TMP_ARR(LLAT,LLON,2) )
!
      DO 410 J1=1,NWAV
         IF ( INDEX ( FILS(J1), '_30c.nc' ) > 0 .OR. INDEX ( FILS(J1), 'psi1_ray' )  > 0  ) THEN
              DO 420 J2=1,NLAT
                 DO 430 J3=1,NLON
                    IF ( LS(J3,J2) == 1.0 ) THEN
                         ARR_R4(J3,J2,1,J1) = FILL_VALUE
                         ARR_R4(J3,J2,2,J1) = FILL_VALUE
                    END IF
 430             CONTINUE 
 420          CONTINUE 
              GOTO 410
         ENDIF 
         CALL LIB$MOVC3 ( 4*LLON*LLAT, ARR_R4(1,1,1,J1), TMP_ARR(1,1,1) )
         CALL LIB$MOVC3 ( 4*LLON*LLAT, ARR_R4(1,1,2,J1), TMP_ARR(1,1,2) )
!
         DO 440 J4=1,LLAT 
            INDC_LAT = 1 + (J4-1)*5
            DO 450 J5=1,LLON
               INDC_LON = 1 + (J5-1)*5
               DO 460 J6=1,5
                  IND_LAT = INDC_LAT - 1 + J6
                  IF ( IND_LAT > NLAT ) GOTO 460
                  DO 470 J7=1,5
                     IND_LON = INDC_LON - 1 + J7
                     IF ( LS(IND_LON,IND_LAT) == 1.0 ) THEN
                          ARR_R4(IND_LON,IND_LAT,1,J1) = FILL_VALUE
                          ARR_R4(IND_LON,IND_LAT,2,J1) = FILL_VALUE
                        ELSE
                          CMP_COS = TMP_ARR(J4,J5,1)
                          CMP_SIN = TMP_ARR(J4,J5,2)
                          MAXVAL_R4 = MAX ( CMP_COS, MAXVAL_R4 )
                          MAXVAL_R4 = MAX ( CMP_SIN, MAXVAL_R4 )
                          MINVAL_R4 = MIN ( CMP_COS, MINVAL_R4 )
                          MINVAL_R4 = MIN ( CMP_SIN, MINVAL_R4 )
                          ARR_R4(IND_LON,IND_LAT,1,J1) = CMP_COS
                          ARR_R4(IND_LON,IND_LAT,2,J1) = CMP_SIN
                     END IF
 470              CONTINUE 
 460           CONTINUE 
 450        CONTINUE 
 440     CONTINUE 
 410  CONTINUE 
!
      DEALLOCATE ( TMP_ARR )
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  TPXO8_UPDATE_ARR  !#!  
!
! ------------------------------------------------------------------------
!
      SUBROUTINE EOT11A_UPDATE_ARR ( NLON, NLAT, NCMP, NWAV, SCALE, &
     &                               FILL_VALUE, FILS, ARR_R4, &
     &                               MINVAL_R4, MAXVAL_R4, IUER )
      IMPLICIT   NONE 
      INTEGER*4  NLON, NLAT, NCMP, NWAV, IUER
      CHARACTER  FILS(NWAV)*(*)
      REAL*4     ARR_R4(NLON,NLAT,2,NWAV)
      REAL*4,    ALLOCATABLE :: TMP_ARR(:,:,:,:)
      REAL*4     SCALE, FILL_VALUE, MINVAL_R4, MAXVAL_R4
      LOGICAL*1  FL_HR(8192), FL_LAND
      REAL*4     CMP_COS, CMP_SIN
      INTEGER*4  J1, J2, J3, J4, J5, J6, J7, LLAT, LLON, &
     &           INDC_LAT, INDC_LON, IND_LAT, IND_LON
      LOGICAL*4, EXTERNAL :: IS_R4_NAN 
!
      LLAT =  NLAT
      LLON =  NLON-1
      ALLOCATE ( TMP_ARR(LLON,LLAT,NCMP,NWAV) )
!
      DO 410 J1=1,NWAV
         DO 420 J2=1,LLAT
            DO 430 J3=1,LLON
               IF ( IS_R4_NAN ( ARR_R4(J3,J2,1,J1) )   .OR. &
     &              IS_R4_NAN ( ARR_R4(J3,J2,2,J1) )   .OR. &
     &              ARR_R4(J3,J2,1,J1) > FILL_VALUE/2  .OR. &
     &              ARR_R4(J3,J2,2,J1) > FILL_VALUE/2       ) THEN
!
                    TMP_ARR(J3,J2,1,J1) = FILL_VALUE
                    TMP_ARR(J3,J2,2,J1) = FILL_VALUE
                  ELSE 
                    CMP_COS = SCALE*ARR_R4(J3,J2,1,J1)
                    CMP_SIN = SCALE*ARR_R4(J3,J2,2,J1) 
                    MAXVAL_R4 = MAX ( CMP_COS, MAXVAL_R4 )
                    MAXVAL_R4 = MAX ( CMP_SIN, MAXVAL_R4 )
                    MINVAL_R4 = MIN ( CMP_COS, MINVAL_R4 )
                    MINVAL_R4 = MIN ( CMP_SIN, MINVAL_R4 )
                    TMP_ARR(J3,J2,1,J1) = CMP_COS
                    TMP_ARR(J3,J2,2,J1) = CMP_SIN
               END IF
 430        CONTINUE 
 420     CONTINUE 
 410  CONTINUE 
!
      CALL LIB$MOVC8 ( INT8(4)*INT8(LLAT)*INT8(LLON)*INT8(NCMP)*INT8(NWAV), &
     &                 TMP_ARR, ARR_R4 )
!
      DEALLOCATE ( TMP_ARR )
      RETURN
      END  SUBROUTINE  EOT11A_UPDATE_ARR  !#!  
!
! ------------------------------------------------------------------------

      SUBROUTINE swap_dim2 ( nlon, nlat, ARR_R4 ) ! %%%
      INTEGER*4  NLON, NLAT, IUER
      REAL*4,    ALLOCATABLE :: TMP_ARR(:,:,:)
      REAL*4     ARR_R4(NLAT,NLON,2) 
      REAL*4,    EXTERNAL :: ATAN_CS_R4 
      INTEGER*4  J1, J2
!
      ALLOCATE ( TMP_ARR(NLON,NLAT,2) )
!
      DO 410 J1=1,NLAT
         DO 420 J2=1,NLON
            TMP_ARR(J2,J1,1:2) = ARR_R4(J1,J2,1:2)
            TMP_ARR(J2,J1,2) = ATAN_CS_R4 ( TMP_ARR(J2,J1,1), TMP_ARR(J2,J1,2)  )
 420     CONTINUE 
 410  CONTINUE 
      CALL LIB$MOVC3 ( 4*NLON*NLAT*2, TMP_ARR, ARR_R4 )
!
      DEALLOCATE ( TMP_ARR )
      RETURN
      END  SUBROUTINE  swap_dim2   !#!  
!
! ------------------------------------------------------------------------
!
      SUBROUTINE DLS_OCEAN_TO_LS ( HEB_LS )
! ************************************************************************
! *                                                                      *
! *   Routine  DLS_OCEAN_TO_LS
! *                                                                      *
! * ### 16-FEB-2016 DLS_OCEAN_TO_LS v1.0 (c)  L. Petrov  16-FEB-2016 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'malo.i'
      INCLUDE   'heb.i'
      TYPE       ( HEB__TYPE  ) :: HEB_LS
      REAL*4       DLS_MIN, DLS_MAX, EPS
      PARAMETER  ( DLS_MAX =  7.0 ) 
      PARAMETER  ( EPS     =  0.0001 )
      INTEGER*4  J1, J2
!
! --- Transform the distance to the ocean mask to the ocean land-sea mask
!
      DO 410 J1=1,HEB_LS%DIMS(2)
         DO 420 J2=1,HEB_LS%DIMS(1)
            IF ( HEB_LS%VAL(J2,J1,1,1) > -EPS .AND. & 
     &           HEB_LS%VAL(J2,J1,1,1) <  EPS       ) THEN
!
! -------------- Land, because the dls mask is zero
!
                 HEB_LS%VAL(J2,J1,1,1) = MALO__LAND_VAL
               ELSE 
                 IF ( HEB_LS%VAL(J2,J1,1,1) < DLS_MAX ) THEN
!
! ------------------- Water because dls mask less less than the maximum
!
                      HEB_LS%VAL(J2,J1,1,1) = MALO__WATER_VAL
                    ELSE 
                      HEB_LS%VAL(J2,J1,1,1) = MALO__LAND_VAL
                 END IF
            END IF 
 420     CONTINUE 
 410  CONTINUE 
      RETURN
      END  SUBROUTINE DLS_OCEAN_TO_LS  !#!  
!
! ------------------------------------------------------------------------
!
      SUBROUTINE BIG_WRITE ( LUN, OFFS_I8, LEN_I8, ARR_I1, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine BIG_WRITE 
! *                                                                      *
! *  ### 26-APR-2016    BIG_WRITE  v1.0 (c)  L. Petrov  26-APR-2016 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INTEGER*4  LUN, IUER
      INTEGER*8  OFFS_I8, LEN_I8
      INTEGER*1  ARR_I1(LEN_I8)
      INTEGER*8    DATA_CHUNK_LEN
      PARAMETER  ( DATA_CHUNK_LEN = 128*1024*1024 )
      CHARACTER  STR*128
      INTEGER*8  CHUNKS_TO_WRITE, OFFSET_RET, OFFS_WRITE, BYTES_TO_WRITE
      INTEGER*4  IS, SEEK_SET, ARG_LN, J1
      INTEGER*8, EXTERNAL :: LSEEK
      INTEGER*4, EXTERNAL :: ILEN, I_LEN, WRITE
!
      CALL GET_SYSTEM_CONSTANT ( 'SEEK_SET', SEEK_SET, ARG_LN )
      OFFSET_RET = LSEEK( %VAL(LUN), %VAL(OFFS_I8), %VAL(SEEK_SET) )
      IF ( OFFSET_RET .NE. OFFS_I8 ) THEN
           CALL CLRCH  ( STR )
           CALL GERROR ( STR )
           CALL ERR_LOG ( 1511, IUER, 'BIG_WRITE', 'Failure in '// &
     &         'position the file into beginning the section: '// &
     &          STR )
           RETURN 
      END IF
      CHUNKS_TO_WRITE = 1 + LEN_I8/DATA_CHUNK_LEN
      OFFS_WRITE = 0
      DO 410 J1=1,CHUNKS_TO_WRITE
         IF ( J1 == CHUNKS_TO_WRITE ) THEN
              BYTES_TO_WRITE = LEN_I8 - (J1-1)*DATA_CHUNK_LEN
              IF ( BYTES_TO_WRITE == 0 ) THEN
                   GOTO 810
              END IF
           ELSE 
              BYTES_TO_WRITE = DATA_CHUNK_LEN
         END IF
         IS = WRITE ( %VAL(LUN), %VAL(LOC(ARR_I1) + OFFS_WRITE), &
     &                %VAL(BYTES_TO_WRITE) )
         IF ( IS == -1 ) THEN
              CALL CLRCH  ( STR )
              CALL GERROR ( STR )
              CALL ERR_LOG ( 1412, IUER, 'BIG_WRITE', 'Failure in '// &
     &            'writing data: '//STR )
              RETURN 
            ELSE IF ( IS < BYTES_TO_WRITE ) THEN
              CALL CLRCH ( STR )
              CALL INCH8 ( OFFS_WRITE + IS, STR )
              CALL ERR_LOG ( 1413, IUER, 'BIG_WRITE', 'Not all data '// &
     &            'were written, only '//STR(1:I_LEN(STR))//' bytes' )
              RETURN 
         END IF
         OFFS_WRITE = OFFS_WRITE + IS
 410  CONTINUE 
 810  CONTINUE 
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE BIG_WRITE  !#!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE BINF_OPEN_NEW ( FINAM, STATUS, LUN, IUER )
! ************************************************************************
! *                                                                      *
! *    Routine  BINF_OPEN  opens file with name  FINAM on the logical    *
! *    unit  LUN. File may be openned:                                   *
! *     -- for reading only (STATUS='OLD'),                              *
! *     -- for creating and writing only (STATUS='NEW') -- file be       *
! *        created anew. If the file with the same name has existed it   *
! *        will be removed before operation! No confirmation will be     *
! *        askedf or deleting previous version of the file.              *
! *     -- for reading and writing (STATUS='UNKNOWN'). If file with the  *
! *        same name has not existed it will be created.                 *
! *                                                                      *
! *  ###  03-JAN-1997   BINF_OPEN   v 2.9 (c) L. Petrov 28-SEP-2011 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INTEGER*4  LUN, IUER
      CHARACTER  FINAM*(*), STATUS*(*)
      CHARACTER  FINAM_ZERO*8192
      INTEGER*4  IO
      CHARACTER  STR*64, STATUS_USE*7, STRI*16
      INTEGER*4  ARG_LEN
      INTEGER*4  O_CREAT_FLAG, O_RDONLY_FLAG, O_RDWR_FLAG, O_WRONLY_FLAG
      INTEGER*4  OPEN_FLAGS, MODE_FLAGS
      INTEGER*4  MODE_1, MODE_2, MODE_3, MODE_4, MODE_5, MODE_6
      LOGICAL*4  LEX
      INTEGER*4, EXTERNAL :: ILEN, I_LEN, OPEN, OPEN64, LOC__SUN$$_STR
!
! --- Learn values of the system constants from Unix headers
!
      CALL GET_SYSTEM_CONSTANT ( 'O_WRONLY', O_WRONLY_FLAG, ARG_LEN )
      CALL GET_SYSTEM_CONSTANT ( 'O_CREAT',  O_CREAT_FLAG,  ARG_LEN )
      CALL GET_SYSTEM_CONSTANT ( 'O_RDONLY', O_RDONLY_FLAG, ARG_LEN )
      CALL GET_SYSTEM_CONSTANT ( 'O_RDWR',   O_RDWR_FLAG,   ARG_LEN )
!
      CALL GET_SYSTEM_CONSTANT ( 'S_IRUSR',  MODE_1, ARG_LEN )
      CALL GET_SYSTEM_CONSTANT ( 'S_IWUSR',  MODE_2, ARG_LEN )
      CALL GET_SYSTEM_CONSTANT ( 'S_IRGRP',  MODE_3, ARG_LEN )
      CALL GET_SYSTEM_CONSTANT ( 'S_IWGRP',  MODE_4, ARG_LEN )
      CALL GET_SYSTEM_CONSTANT ( 'S_IROTH',  MODE_5, ARG_LEN )
      CALL GET_SYSTEM_CONSTANT ( 'S_IWOTH',  MODE_6, ARG_LEN )
#ifdef GNU
      MODE_FLAGS = MODE_1 + &
     &             MODE_2 + &
     &             MODE_3 + &
     &             MODE_4 + &
     &             MODE_5 + &
     &             MODE_6
#else
      MODE_FLAGS = MODE_1 .OR. &
     &             MODE_2 .OR. &
     &             MODE_3 .OR. &
     &             MODE_4 .OR. &
     &             MODE_5 .OR. &
     &             MODE_6
#endif
!
      CALL CLRCH ( STRI )
!
! --- Transformation string STATUS in upper registr and writing to STAT
!
      CALL TRAN ( 11, STATUS, STATUS_USE )
!
! --- Test: whether the file exist?
!
      IF ( ILEN(FINAM) .EQ. 0 ) THEN
           CALL ERR_LOG ( 9001, IUER, 'BINF_OPEN', 'Argument FINAM '// &
     &         '(file name to be opened) contains only blanks or '// &
     &         'binary zeroes' )
           RETURN
      END IF
      IF ( ILEN(FINAM) > LEN(FINAM_ZERO) ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( LEN(FINAM_ZERO), STR )
           CALL ERR_LOG ( 9002, IUER, 'BINF_OPEN', 'Trap of internal control: '// &
     &         'filename '//FINAM(1:I_LEN(FINAM))//' -- is too long: '// &
     &         'longer than '//STR(1:I_LEN(STR))//' characters' )
           RETURN
      END IF
!
      INQUIRE ( FILE=FINAM(1:I_LEN(FINAM)), EXIST=LEX, IOSTAT=IO )
      IF ( IO .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( IO, STR )
           CALL ERR_LOG ( 9003, IUER, 'BINF_OPEN', 'Wrong file name "'// &
     &          FINAM(1:I_LEN(FINAM))//'". IOSTAT='//STR(1:I_LEN(STR)) )
           RETURN
      END IF
      FINAM_ZERO = FINAM(1:I_LEN(FINAM))     ! This is necessary in order to
      FINAM_ZERO(I_LEN(FINAM)+1:) = CHAR(0)  ! circumvent memory leackage bug
!                                            ! In HP Fortran90 2.5.1
!
! --- Define the values of the flag
!
      IF ( STATUS_USE(1:3) .EQ. 'NEW' ) THEN
           IF ( LEX .AND.  FINAM(1:8) .NE. '/dev/nul' ) THEN
                CALL UNLINK ( FINAM(1:I_LEN(FINAM))//CHAR(0) )
           END IF
#ifdef GNU
           OPEN_FLAGS = O_WRONLY_FLAG + O_CREAT_FLAG
#else
           OPEN_FLAGS = O_WRONLY_FLAG .OR. O_CREAT_FLAG
#endif
           STRI = 'output'
         ELSE IF ( STATUS_USE(1:3) .EQ. 'OLD' ) THEN
           IF ( .NOT. LEX ) THEN
                CALL ERR_LOG ( 9004, IUER, 'BINF_OPEN', 'File '// &
     &               FINAM(1:I_LEN(FINAM))//' not found' )
                RETURN
           END IF
           OPEN_FLAGS = O_RDONLY_FLAG
           STRI = 'input'
         ELSE IF ( STATUS_USE(1:6) .EQ. 'APPEND' ) THEN
           OPEN_FLAGS = O_RDWR_FLAG 
           STRI = 'input/output'
         ELSE IF ( STATUS_USE(1:7) .EQ. 'UNKNOWN' ) THEN
#ifdef GNU
           OPEN_FLAGS = O_RDWR_FLAG + O_CREAT_FLAG
#else
           OPEN_FLAGS = O_RDWR_FLAG .OR. O_CREAT_FLAG
#endif
           STRI = 'input/output'
         ELSE
           CALL ERR_LOG ( 9005, IUER, 'BINF_OPEN', 'Parameter STATUS has '// &
     &         'unacceptable value: "'//STATUS(1:I_LEN(STATUS))// &
     &         '". Acceptable only OLD, NEW, APPEND or UNKNOWN' )
           RETURN
      END IF
!
! --- System call for opening file
!
#ifdef SUN
      LUN = OPEN ( %VAL(LOC__SUN$$_STR(FINAM_ZERO)), %VAL(OPEN_FLAGS), %VAL(MODE_FLAGS) )
#else
#ifdef ADR_32BIT
      LUN = OPEN64 ( %REF(FINAM_ZERO), %VAL(OPEN_FLAGS), %VAL(MODE_FLAGS) )
#else
      LUN = OPEN   ( %REF(FINAM_ZERO), %VAL(OPEN_FLAGS), %VAL(MODE_FLAGS) )
#endif
#endif
      IF ( LUN .LT. 0 ) THEN
           CALL CLRCH  ( STR )
           CALL GERROR ( STR )
           CALL ERR_LOG ( 9006, IUER, 'BINF_OPEN', 'Error during '// &
     &              'opening '//STRI(1:I_LEN(STRI))//' file '// &
     &               FINAM(1:I_LEN(FINAM))//'":  '//STR )
           RETURN
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  BINF_OPEN_NEW  !#!#
