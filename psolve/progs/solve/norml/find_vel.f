      SUBROUTINE FIND_VEL(KVEL,ISTA,ISUPVL)
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
      INTEGER*2 ISTA,ISUPVL
      LOGICAL*2 KVEL(3)
!
!  FIND STATION FOR UEN SUPPRESION
!
      INCLUDE 'solve.i'
      INCLUDE 'socom.i'
      INCLUDE 'prfil.i'
      INCLUDE 'glbc3.i'
!
      INTEGER*2 I,J
      LOGICAL*2 KBIT,EQUAL
!
      KVEL(1)=.FALSE.
      KVEL(2)=.FALSE.
      KVEL(3)=.FALSE.
!
      ISUPVL=DEFVEL
      DO J=1,ISTASP
        IF ( STASUP(J) == ISITN_CHR(ISTA) ) THEN
          CALL KSBIT( ISUPVL, INT2(1), KBIT(VELSUP(1,1),J) )
          CALL KSBIT( ISUPVL, INT2(2), KBIT(VELSUP(1,2),J) )
          CALL KSBIT( ISUPVL, INT2(3), KBIT(VELSUP(1,3),J) )
          GO TO 10
        ENDIF
      ENDDO
10    CONTINUE
      IF(ISUPVL.EQ.0) RETURN
!
      DO J=1,3
        KVEL(J)=KBIT(LSITEV(1,J),ISTA)
        CALL SBIT( LSITEV(1,J), ISTA, INT2(0) )
      ENDDO
      RETURN
!
      END
