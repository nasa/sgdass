      PROGRAM   MALO_EPH_TO_MONTHLY
! ************************************************************************
! *                                                                      *
! *   Program MALO_EPH_TO_MONTHLY
! *                                                                      *
! * ## 20-MAY-2017 MALO_EPH_TO_MONTHLY v1.0 (c) L. Petrov 20-MAY-2017 ## *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'malo.i'
      INCLUDE   'malo_local.i'
      TYPE     ( MALO__TYPE ), POINTER :: MAL(:)
      CHARACTER  DIR*128, FILNAM*128, STR*128, C_FIL(MALO__FIL)*128
      CHARACTER  LOAD_TYPE*3, LOAD_MODEL*8, LOAD_FRAME*2
      INTEGER*8  DIR_DESC(16), IP8
      INTEGER*4  J1, IS, ID, LEV, L_FIL, IVRB, IUER
      LOGICAL*1  FL_KEEP_ORIG
      INTEGER*4, EXTERNAL :: ILEN, I_LEN, GET_FILE_FROM_DIR, LINDEX
!
      IF ( IARGC() < 2 ) THEN
           WRITE ( 6, * ) 'Usage: malo_eph_to_monthly dir keep|erase [ivrb]'
           CALL EXIT ( 1 )
         ELSE
           CALL GETARG ( 1, DIR )
           CALL GETARG ( 2, STR )
           IF ( STR == 'keep' ) THEN
                FL_KEEP_ORIG = .TRUE.
              ELSE IF ( STR == 'erase' ) THEN
                FL_KEEP_ORIG = .FALSE.
              ELSE 
                IUER = -1
                CALL ERR_LOG ( 6901, IUER, 'MALO_EPH_TO_MONTHLY', 'Wrong the '// &
     &              'second argument '//TRIM(STR)//' while keep or erase were '// &
     &              'expected' )
                CALL EXIT ( 1 )
           END IF
           IF ( IARGC() .GE. 3 ) THEN
                CALL GETARG ( 3, STR )
                CALL CHIN   ( STR, IVRB )
                IF ( IVRB < 0 ) THEN
                     CALL ERR_LOG ( 6902, IUER, 'MALO_EPH_TO_MONTHLY', 'Wrong the '// &
     &                   'third argument '//TRIM(STR)//' a non-negative integer '// &
     &                   'was expected' )
                     CALL EXIT ( 1 )
                END IF
              ELSE
                IVRB = 1
           END IF
      END IF
!
      ALLOCATE ( MAL(1), STAT=IUER )
      IF ( IUER .NE. 0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 6903, IUER, 'MALO_EPH_TO_MONTHLY', 'Error in an attempt '// &
     &         'to allocate memory for the MALO object' )
           CALL EXIT ( 1 )
      END IF
!
! --- Initialize MALO object
!
      IUER = -1
      CALL MALO_INIT ( MAL(1), IUER )
      IF ( IUER .NE. 0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 6904, IUER, 'MALO_EPH_TO_MONTHLY', 'Error in an attempt '// &
     &         'to initialize object MALO' )
           CALL EXIT ( 1 )
      END IF
!
      MAL(1)%CONF%KEEP_EPHEDISP_ORIG = FL_KEEP_ORIG
!
      LOAD_FRAME = '??'
      LEV = 0
      DO 410 J1=1,16*MALO__FIL
         IS = GET_FILE_FROM_DIR ( LEV, DIR_DESC, DIR, FILNAM )
         IF ( IS .NE. 0 ) THEN
              CALL ERR_LOG ( 6905, -2, 'MALO_EPH_TO_MONTHLY', 'Error in '// &
     &            'reading input directory '//DIR(1:I_LEN(DIR))// &
     &                 '  '//FILNAM )
              CALL EXIT ( 1 )
         END IF
         IF ( LEV == 0 ) GOTO 810 ! End of work
         IF ( INDEX ( FILNAM, '#' ) .GT. 0 ) GOTO 410
         IF ( ILEN(FILNAM) < 8 ) GOTO 410
         IF ( FILNAM(ILEN(FILNAM)-3:ILEN(FILNAM)) == '.eph' ) THEN
!
              ID = LINDEX ( FILNAM, '/' ) + 1
              IF ( ID + 2 .GE. ILEN(FILNAM) ) GOTO 410
              IF ( INDEX ( FILNAM(ID:), 'cm_' ) > 0 ) THEN
                   LOAD_FRAME = 'cm'
                ELSE IF ( INDEX ( FILNAM(ID:), 'cf_' ) > 0 ) THEN
                   LOAD_FRAME = 'cf'
                ELSE IF ( INDEX ( FILNAM(ID:), 'd1_' ) > 0 ) THEN
                   LOAD_FRAME = 'd1'
                ELSE
                   LOAD_FRAME = 'cm'
              END IF
              IF ( INDEX ( FILNAM(ID:), 'atm' ) > 0 ) THEN
                   LOAD_TYPE = 'atm'
                 ELSE IF ( INDEX ( FILNAM(ID:), 'lws' ) > 0 ) THEN
                   LOAD_TYPE = 'lws'
                 ELSE IF ( INDEX ( FILNAM(ID:), 'nto' ) > 0 ) THEN
                   LOAD_TYPE = 'nto'
                 ELSE
                   CALL ERR_LOG ( 6906, -2, 'MALO_EPH_TO_MONTHLY', 'Failure '// &
     &                 'in parsing file name '//TRIM(FILNAM)//' -- cannot '// &
     &                 'determine loading type. Types atm, lws, and nto are '// &
     &                 'supported' )
                   CALL EXIT ( 1 )
              END IF
              IF ( INDEX ( FILNAM(ID:), 'merra2' ) > 0 ) THEN
                   LOAD_MODEL = 'merra2'
                 ELSE IF ( INDEX ( FILNAM(ID:), 'geosfpit' ) > 0 ) THEN
                   LOAD_MODEL = 'geosfpit'
                 ELSE IF ( INDEX ( FILNAM(ID:), 'omct05' ) > 0 ) THEN
                   LOAD_MODEL = 'omct05'
                 ELSE IF ( INDEX ( FILNAM(ID:), 'mpiom06' ) > 0 ) THEN
                   LOAD_MODEL = 'mpiom06'
                 ELSE
                   CALL ERR_LOG ( 6907, -2, 'MALO_EPH_TO_MONTHLY', 'Failure '// &
     &                 'in parsing file name '//TRIM(FILNAM)//' -- cannot '// &
     &                 'determine loading model. Models merra2, geosfpit, '// &
     &                 'omct05, and mpiom06 are supported' )
                   CALL EXIT ( 1 )
              END IF
         END IF
 410  CONTINUE 
 810  CONTINUE 
      IF ( LOAD_FRAME == '??' ) THEN
           CALL ERR_LOG ( 6908, -2, 'MALO_EPH_TO_MONTHLY', 'No files with '// &
     &         'extension .eph were found in the input directory '//DIR )
           CALL EXIT ( 1 )
      END IF
!
      IF ( LOAD_TYPE == 'atm' ) THEN
           IF ( LOAD_FRAME == 'cm' ) THEN
                MAL(1)%CONF%LOA_FINAM_COMM = MALO_SHARE//'/atm_description.txt'
              ELSE IF ( LOAD_FRAME == 'd1' ) THEN
                MAL(1)%CONF%LOA_FINAM_COMM = MALO_SHARE//'/d1_atm_description.txt'
              ELSE IF ( LOAD_FRAME == 'cf' ) THEN
                MAL(1)%CONF%LOA_FINAM_COMM = MALO_SHARE//'/cf_atm_description.txt'
           END IF
           IF ( LOAD_MODEL == 'geosfpit' ) THEN
                MAL(1)%CONF%LOA_FINAM_DESCR = MALO_SHARE//'/atm_geosfpit_data_source.txt'
              ELSE IF ( LOAD_MODEL == 'merra2' ) THEN
                MAL(1)%CONF%LOA_FINAM_DESCR = MALO_SHARE//'/atm_merra2_data_source.txt'
           END IF
         ELSE IF ( LOAD_TYPE == 'lws' ) THEN
           IF ( LOAD_FRAME == 'cm' ) THEN
                MAL(1)%CONF%LOA_FINAM_COMM = MALO_SHARE//'/lws_description.txt'
              ELSE IF ( LOAD_FRAME == 'd1' ) THEN
                MAL(1)%CONF%LOA_FINAM_COMM = MALO_SHARE//'/d1_lws_description.txt'
              ELSE IF ( LOAD_FRAME == 'cf' ) THEN
                MAL(1)%CONF%LOA_FINAM_COMM = MALO_SHARE//'/cf_lws_description.txt'
           END IF
           IF ( LOAD_MODEL == 'geosfpit' ) THEN
                MAL(1)%CONF%LOA_FINAM_DESCR = MALO_SHARE//'/lws_geosfpit_data_source.txt'
              ELSE IF ( LOAD_MODEL == 'merra2' ) THEN
                MAL(1)%CONF%LOA_FINAM_DESCR = MALO_SHARE//'/lws_merra2_data_source.txt'
           END IF
         ELSE IF ( LOAD_TYPE == 'nto' ) THEN
           IF ( LOAD_FRAME == 'cm' ) THEN
                MAL(1)%CONF%LOA_FINAM_COMM = MALO_SHARE//'/nto_description.txt'
              ELSE IF ( LOAD_FRAME == 'd1' ) THEN
                MAL(1)%CONF%LOA_FINAM_COMM = MALO_SHARE//'/d1_nto_description.txt'
              ELSE IF ( LOAD_FRAME == 'cf' ) THEN
                MAL(1)%CONF%LOA_FINAM_COMM = MALO_SHARE//'/cf_nto_description.txt'
           END IF
           IF ( LOAD_MODEL == 'omct05' ) THEN
                MAL(1)%CONF%LOA_FINAM_DESCR = MALO_SHARE//'/nto_omct05_data_source.txt'
             ELSE IF ( LOAD_MODEL == 'mpiom06' ) THEN
                MAL(1)%CONF%LOA_FINAM_DESCR = MALO_SHARE//'/nto_mpiom06_data_source.txt'
           END IF
      END IF
!
      MAL(1)%CONF%EPHEDISP_FINAM_FMT = MALO_SHARE//'/ephedisp_format.txt'
!
      IUER = -1
      CALL MALO_EPHE_MONTHLY ( MAL(1), DIR, IVRB, IUER )
      IF ( IUER .NE. 0 ) THEN
           CALL ERR_LOG ( 6909, -2, 'MALO_EPH_TO_MONTHLY', 'Failure in an '// &
     &         'attempt to re-organize output mass loading files in EPHEDISP '// &
     &         'format to monthly files' )
           CALL EXIT ( 1 )
      END IF
      END  PROGRAM  MALO_EPH_TO_MONTHLY  !#!#
