      LOGICAL*2 FUNCTION EQUAL(IX,I,IY,J,N)
      IMPLICIT NONE
      INTEGER*2 IX(*), I, IY(*), J, N
!
! EQUAL: compare two hollerith strings
!
! input: IX   one string
!        I    first character in IX to use
!        IY   second string
!        J    first character in IY to use
!        N    number of characters to compare
!
! output: return value .TRUE.  if strings are equal to N characters
!                      .FALSE. otherwise
!
! WARNING: I and J must be odd and N must be even
!          other cases can be supported with a little work
!
      INTEGER*2 IO,JO,IN
!
      IF ( MOD(I,INT2(2)) .EQ.0 .OR. MOD(J,INT2(2)).EQ.0 .OR. MOD(N, &
     &     INT2(2)).EQ.1 ) THEN
           WRITE(7,'("Unsupported args in EQUAL",3I7)') I,J,N
      ENDIF
!
      EQUAL=.FALSE.
      IO=((I+1)/2)-1
      JO=((J+1)/2)-1
!
      DO IN=1,N/2
         IF ( IX(IO+IN) .NE. IY(JO+IN) ) THEN
              RETURN
         END IF
      ENDDO
!
      EQUAL=.TRUE.
      RETURN
      END
