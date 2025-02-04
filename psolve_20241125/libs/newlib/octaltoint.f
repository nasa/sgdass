      INTEGER*2 FUNCTION OCTALTOINT(STRING,ERROR)
      IMPLICIT NONE
      CHARACTER*(*) STRING
      INTEGER*2 ERROR
!
!  OCTALTOINT: convert ascii octal string to I*2 variable
!
!  ERROR is 0 if successful, -1 otherwise
!
      CHARACTER*1 JUNK
      CHARACTER*6 TOKEN,ST
      INTEGER*2 IL,TRIMLEN
!
      CALL SPLITSTRING(STRING,TOKEN,JUNK)
      IL=TRIMLEN(TOKEN)
      IF(IL.LE.0) GOTO 999
      ST=' '
      ST(7-IL:)=TOKEN(1:IL)
      READ(ST,'(O6)',ERR=999) OCTALTOINT
      ERROR=0
      RETURN
!
999   CONTINUE
      ERROR=-1
      RETURN
      END
