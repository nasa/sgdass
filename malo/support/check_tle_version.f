      PROGRAM    CHECK_TLE_VERSION
      INCLUDE   'tle_sgp4.i'
      CHARACTER  QUE*32, ANS*128
      QUE = '--version'
      CALL TLE_INQ ( QUE, ANS )
      WRITE ( 6, '(A)' ) TRIM(ANS)
      END  !#!  
