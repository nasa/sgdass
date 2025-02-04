      PROGRAM  NERS_LEN
      INCLUDE 'ners.i'
      TYPE   ( NERS__TYPE ) :: NERS
      WRITE ( 6, '(I6)' ) SIZEOF(NERS) + NERS__SIZE_EXTRA
      END  !#!  
