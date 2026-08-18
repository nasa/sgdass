      CHARACTER*(*) FUNCTION INTTODECIMAL(INT)
      IMPLICIT NONE
      INTEGER*2 INT
!
!  INTTODECIMAL: convert I*2 variable to ascii string left justified
!
      CHARACTER*6 STRING
      INTEGER*2 I
!
      WRITE(STRING,'(I6)') INT
      DO I=1,6
         IF ( STRING(I:I).NE.' ') THEN
              INTTODECIMAL=STRING(I:6)
              RETURN
         ENDIF
      ENDDO
!
      WRITE ( 6, * ) 'Fatal failure in INITDECIMAL: INT= ', INT
!
! --- Delibetatey crash in order to unwind the stack
!
      I = -22
      WRITE ( 6, * ) STRING(I:I)
      RETURN
      END
