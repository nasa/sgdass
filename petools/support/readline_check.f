      PROGRAM READLINE_CHECK
      ADDRESS__TYPE, EXTERNAL :: READLINE
      ADDRESS__TYPE  IADR
      IADR = READLINE ( '>>'//CHAR(0) )
      CALL EXIT ( 0 ) 
      END  !#!  
