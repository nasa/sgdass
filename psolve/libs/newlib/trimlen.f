       INTEGER*2 FUNCTION TRIMLEN ( STRING )
       IMPLICIT NONE
       CHARACTER*(*) STRING
!
! TRIMLEN returns the index of the last nonblank character in STRING
!                 0 if all characters are blank
!
! Modifications:
!      92.06.18
!  BA  95.11.13  Changed to check for nulls as well as blanks.
!
       INTEGER*2 I
!
! Read backwards down array, stopping at first non-blank character
!
       DO I=LEN(STRING),1,-1
          TRIMLEN = I
          IF ( STRING(I:I) .NE. ' ' .AND. ICHAR(STRING(I:I)) .NE. 0 ) THEN
               RETURN
          END IF
       END DO
       TRIMLEN = 0
!
       RETURN 
       END  !#!  TRIMLEN  #!#
