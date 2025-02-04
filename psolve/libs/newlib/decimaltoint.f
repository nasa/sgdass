      INTEGER*2 FUNCTION DECIMALTOINT(STRING,ERROR)
      IMPLICIT NONE
      CHARACTER*(*) STRING
      INTEGER*2 ERROR
      INTEGER*4  IOS
!
!  DECIMALTOINT: convert ascii string to I*2 variable
!
!  ERROR is 0 if successful, -1 otherwise
!
      READ(STRING,*,IOSTAT=IOS) DECIMALTOINT
      ERROR = IOS
      RETURN
      END
