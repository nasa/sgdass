      PROGRAM    CHECK_ATP_VERSION
      INCLUDE   'atp.i'
      CHARACTER  QUE*32, ANS*128
      QUE = '--version'
      CALL ATP_INQ ( QUE, ANS )
      WRITE ( 6, '(A)' ) TRIM(ANS)
      END  PROGRAM  CHECK_ATP_VERSION  !#!#
