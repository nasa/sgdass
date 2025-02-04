      FUNCTION ALL_VAL_ZERO ( N, ARR )
      IMPLICIT   NONE 
      LOGICAL*2  ALL_VAL_ZERO
      INTEGER*2  N, ARR(N)
      INTEGER*2  J1
!
      DO 410 J1=1,N
         IF ( ARR(J1) .NE. 0 ) THEN
              ALL_VAL_ZERO = .FALSE.
              RETURN 
         END IF
 410  CONTINUE 
      ALL_VAL_ZERO = .TRUE.
      RETURN
      END  FUNCTION  ALL_VAL_ZERO  !#!  
