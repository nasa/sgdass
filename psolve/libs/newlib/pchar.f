        SUBROUTINE PCHAR(IAR,I,ICH)
        IMPLICIT   NONE
        INTEGER*2  IAR(*), I, ICH
!
! PCHAR: puts the character in the lower byte of ICH into the
!        Ith position in array IAR
!
       INTEGER*2 IWORD,NWORD
!
#ifdef BIG_ENDIAN
!
! ---- old way to do it
!
       IWORD=IAR((I+1)/2)
!
       IF(MOD(I,INT2(2)).EQ.1) THEN
         IWORD=IAND(IWORD,INT2(Z'FF'))
         NWORD=ISHFT(ICH,8)
       ELSE
         IWORD=IAND(IWORD,INT2(Z'FF00'))
         NWORD=IAND(ICH,INT2(Z'FF'))
       ENDIF
!
       IAR((I+1)/2)=IOR(IWORD,NWORD)
#else
!
! ---- Modern way for doing it.  L. Petrov 06-JUL-2003 12:40:20
!
       IF ( MOD(I,INT2(2)) .EQ. 1 ) THEN
            CALL MVBITS ( ICH, INT2(8), INT2(8), IAR((I+1)/2), INT2(0) )
         ELSE
            CALL MVBITS ( ICH, INT2(8), INT2(8), IAR((I+1)/2), INT2(8) )
       END IF
!
#endif
       RETURN
       END
