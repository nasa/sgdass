      INTEGER*4 FUNCTION DECMLTODINT(STRING,ERROR)
      IMPLICIT NONE
      CHARACTER*(*) STRING
      INTEGER*2 ERROR
!
!  DECMLTODINT: convert ascii string to I*4 variable
!
!  ERROR is 0 if successful, -1 otherwise
!
      READ(STRING,*,ERR=999) DECMLTODINT
      ERROR=0
      RETURN
!
999   CONTINUE
      ERROR=-1
      RETURN
      END
