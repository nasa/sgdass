      INTEGER*4 FUNCTION NULL_TERM ( OUT, IN )
      IMPLICIT NONE
      CHARACTER*(*) OUT, IN
!
!  null_terminate in string putting result in out string
!  return value is  0 if okay
!                  -1 if null terminated string would not fit in output
!
!  behavior undefined for out and in overlapping
!
      INTEGER*4 I
!
      NULL_TERM=0
      DO I=LEN(IN),1,-1
         IF ( IN(I:I) .NE. ' ' )  THEN
              IF ( I .GE. LEN(OUT) ) THEN
                   NULL_TERM=-1
                ELSE
                   OUT(1:I)=IN(1:I)
                   OUT(I+1:I+1)=CHAR(0)
              ENDIF
              RETURN
          ENDIF
      ENDDO
!
      IF ( LEN(OUT) .LT. 1 ) THEN
           NULL_TERM=-1
         ELSE
          OUT(1:1)=CHAR(0)
      ENDIF
!
      RETURN
      END  !#!  NULL_TERM  #!#
