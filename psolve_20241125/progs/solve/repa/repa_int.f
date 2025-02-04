        FUNCTION REPA_INT ( VAL_R8 )
        IMPLICIT   NONE
        INTEGER*4 REPA_INT
        REAL*8   VAL_R8
        IF ( VAL_R8 .GT.  2.14748364799D9 ) THEN
           REPA_INT = 2147483647
        ELSE IF ( VAL_R8 .LT. -2.14748364699D9 ) THEN
           REPA_INT = -2147483646
        ELSE
!%         REPA_INT = IDINT ( VAL_R8 )   ! cut non-integer part
           REPA_INT = IDNINT ( VAL_R8 )  ! nearest integer value
        END IF
        RETURN
        END
