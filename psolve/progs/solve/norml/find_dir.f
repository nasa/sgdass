      SUBROUTINE FIND_DIR(KVEL,ISTAO,ISUPVL)
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
      INTEGER*2 ISTAO,ISUPVL
      LOGICAL*2 KVEL(3)
!
!  FIND ORIGIN STATION FOR DIRECTION SUPPRESION
!
      INCLUDE 'solve.i'
      INCLUDE 'socom.i'
      INCLUDE 'prfil.i'
      INCLUDE 'glbc3.i'
!
      INTEGER*2 I,J
      LOGICAL*2 EQUAL,KBIT
!
      ISUPVL=DEFVEL
      DO J=1,ISTASP
         IF ( STASUP(J) == DATSTA_CHR ) THEN
          CALL KSBIT( ISUPVL, INT2(1), KBIT(VELSUP(1,1),J) )
          CALL KSBIT( ISUPVL, INT2(2), KBIT(VELSUP(1,2),J) )
          CALL KSBIT( ISUPVL, INT2(3), KBIT(VELSUP(1,3),J) )
          GO TO 10
        ENDIF
      ENDDO
10    CONTINUE
!
      KVEL(1)=.FALSE.
      KVEL(2)=.FALSE.
      KVEL(3)=.FALSE.
      ISTAO=0
!
      DO I=1,NUMSTA
        IF(EQUAL( DATSTA, INT2(1), ISITN(1,I), INT2(1), INT2(8) )) THEN
          ISTAO=I
          DO J=1,3
            KVEL(J)=KBIT(LSITEV(1,J),I)
            CALL SBIT( LSITEV(1,J), I, INT2(0) )
          ENDDO
          RETURN
        ENDIF
      ENDDO
!     PAUSE 'DIRECTION STATION NOT AVAILABLE'
      call ferr( INT2(107), 'DIRECTION STATION NOT AVAILABLE', INT2(0), &
     &     INT2(0) )
!
      RETURN
      END
