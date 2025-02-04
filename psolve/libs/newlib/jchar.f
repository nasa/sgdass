        INTEGER*2 FUNCTION JCHAR(IAR,I)
        IMPLICIT NONE
        INTEGER*2 IAR(*), I
        INTEGER*2 IND
!
! JCHAR: returns the Ith character in hollerith array IAR
!
! 2004.06.30  pet  Updated logic for support both BIG_ENDIAN and LITTLE_ENDIAN
!                  architecture
!
        JCHAR = 0
        IF ( I .GT. 0 ) THEN
             IND = (I-1)/2 + 1
#ifdef BIG_ENDIAN
             IF ( MOD(I,INT2(2)) .EQ. 0 ) THEN
                  CALL MVBITS ( IAR(IND), INT2(0), INT2(8), JCHAR, INT2(0) )
                ELSE 
                  CALL MVBITS ( IAR(IND), INT2(8), INT2(8), JCHAR, INT2(0) )
             END IF
#else
             IF ( MOD(I,INT2(2)) .EQ. 0 ) THEN
                  CALL MVBITS ( IAR(IND), INT2(8), INT2(8), JCHAR, INT2(0) )
                ELSE 
                  CALL MVBITS ( IAR(IND), INT2(0), INT2(8), JCHAR, INT2(0) )
             END IF
#endif
        END IF
        RETURN
        END  !#!  JCHAR  #!#
