      PROGRAM    ATP_INQ_MAIN
      IMPLICIT   NONE 
      INCLUDE   'atp_local.i'
      CHARACTER  MODE*10, ANS*128
      IF ( IARGC() < 1 ) THEN
           WRITE ( 6, * ) 'Usage: atp_inq root|prefix|bin'
           CALL  EXIT ( 1 )
      END IF
      CALL GETARG ( 1, MODE )
      CALL ATP_INQ ( MODE, ANS )
      WRITE ( 6, '(A)' ) TRIM(ANS)
      END  PROGRAM   ATP_INQ_MAIN  !#!#
