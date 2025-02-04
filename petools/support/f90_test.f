      PROGRAM    F90_TEST
      TYPE       F90_TYPE
          INTEGER*2 I2
          INTEGER*4 I4
      END TYPE   F90_TYPE
      TYPE ( F90_TYPE ) :: REC
      REC%I2 = 2
      REC%I4 = 4
      CALL EXIT ( 0 ) 
      END  PROGRAM F90_TEST
