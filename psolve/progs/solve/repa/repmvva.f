      SUBROUTINE REPMVVA ( IX, VAL, ARR )
!
!     copy a value to an array element
!
      INTEGER*4  IX
      REAL*4     VAL
      REAL*4     ARR(*)
!
      ARR(IX) = VAL
!
      RETURN
      END
