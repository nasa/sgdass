      SUBROUTINE SPLIT_STRING ( STRING_IN, TOKEN, STRING_OUT )
      IMPLICIT   NONE
      INTEGER*4  LENGTH, CFREAD
!
!  2002.12.13  pet  Added support of bypassing comments
!
!
      CHARACTER STRING_IN*(*), STRING_OUT*(*), TOKEN*(*)
!
 910  CONTINUE
         CALL SPLITSTRING ( STRING_OUT, TOKEN, STRING_IN )
         IF ( TOKEN(1:1) .EQ. '\' ) THEN
 920          CONTINUE
              LENGTH = CFREAD ( STRING_IN )
              IF ( STRING_IN(1:1) .EQ. '*' ) GOTO 920
              GOTO 910
         END IF
      CONTINUE
      RETURN
      END  !#!  SPLIT_STRING  #!#
