      PROGRAM    MALO_INQ
      INCLUDE   'malo_local.i'
      CHARACTER  MODE*10
      IF ( IARGC() < 1 ) THEN
           WRITE ( 6, * ) 'Usage: malo_inq root|prefix|share|script|bin|bin_shared|bin_static|model|dev_model'
      END IF
      CALL GETARG ( 1, MODE )
      IF ( MODE == "root" .OR. MODE == "--root" ) THEN
           WRITE ( 6, '(A)' ) MALO_ROOT
        ELSE IF ( MODE == 'share' .OR. MODE == '--share' ) THEN
           WRITE ( 6, '(A)' ) MALO_SHARE
        ELSE IF ( MODE == 'script' .OR. MODE == '--script' ) THEN
           WRITE ( 6, '(A)' ) MALO_SCRIPT
        ELSE IF ( MODE == 'prefix' .OR. MODE == '--prefix' ) THEN
           WRITE ( 6, '(A)' ) MALO_PREFIX
        ELSE IF ( MODE == 'bin' .OR. MODE == '--bin' ) THEN
           WRITE ( 6, '(A)' ) MALO_PREFIX//'/bin'
        ELSE IF ( MODE == 'bin_shared' .OR. MODE == '--bin_shared' ) THEN
           WRITE ( 6, '(A)' ) MALO_PREFIX//'/bin'
        ELSE IF ( MODE == 'bin_static' .OR. MODE == '--bin_static' ) THEN
           WRITE ( 6, '(A)' ) MALO_ROOT//'/bin_static'
        ELSE IF ( MODE == 'model' .OR. MODE == '--model' ) THEN
           WRITE ( 6, '(A)' ) MALO_MODEL
        ELSE IF ( MODE == 'dev_model' .OR. MODE == '--dev_model' ) THEN
           WRITE ( 6, '(A)' ) MALO_DEV_MODEL
        ELSE
           WRITE ( 6, '(A)' ) 'Wrong argument '//TRIM(MODE)//' -- supported modes: '// &
     &                        'root prefix share script bin bin_shared bin_static model dev_model'
      END IF
      END  PROGRAM    MALO_INQ  !#!#
