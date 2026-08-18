      PROGRAM    TLE_INQ_MAIN
      IMPLICIT   NONE 
      INCLUDE   'tle_local.i'
      CHARACTER  MODE*10, ANS*128
      IF ( IARGC() < 1 ) THEN
           WRITE ( 6, * ) 'Usage: tle_inq root|prefix|bin'
           CALL  EXIT ( 1 )
      END IF
      CALL GETARG ( 1, MODE )
      CALL TLE_INQ ( MODE, ANS )
      WRITE ( 6, '(A)' ) TRIM(ANS)
      END  PROGRAM   TLE_INQ_MAIN  !#!#
