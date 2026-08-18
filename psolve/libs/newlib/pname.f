      SUBROUTINE PNAME(INAME)
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
      INTEGER*2 INAME(3)
!
!  PNAME: return first size characters of program name
!         in a hollerith array
!
      INTEGER*2 IL,TRIMLEN,IS,I
      CHARACTER*80 NAME
!
      CALL RCPAR( INT2(0), NAME )
      IL=TRIMLEN(NAME)
      IF(IL.GE.80.or.IL.LE.0) PAUSE 'PNAME failed'
      IS=IL
      DO I=IL-1,1,-1
        IF(NAME(I:I).EQ.'/') GO TO 100
        IS=I
      ENDDO
100   CONTINUE
      CALL CHAR2HOL( NAME(IS:IL), INAME, INT2(1), INT2(6) )
      RETURN
      END
