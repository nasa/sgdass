      CHARACTER*(*) FUNCTION DINTTODECML(INT)
      IMPLICIT NONE
      INTEGER*4 INT
!
!  INTTODECIMAL: convert I*4 variable to ascii string left justified
!
      CHARACTER*11 STRING
      INTEGER*2 I
!
      WRITE(STRING,'(I11)') INT
      DO I=1,11
        IF(STRING(I:I).NE.' ') THEN
          DINTTODECML=STRING(I:11)
          RETURN
        ENDIF
      ENDDO
!
!!      PAUSE 'dinttodecml can''t get here'
      RETURN
      END
