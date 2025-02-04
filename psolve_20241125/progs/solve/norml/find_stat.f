      SUBROUTINE FIND_STAT(KSTAT,ICMP)
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
      INCLUDE 'solve.i'
      INTEGER*2 ICMP,KSTAT(STA_BIT_WORDS)
!
!  FIND ORIGIN STATION FOR DIRECTION SUPPRESION
!
      INCLUDE 'socom.i'
      INCLUDE 'prfil.i'
      INCLUDE 'glbc3.i'
!
      INTEGER*2 I,J
      LOGICAL*2 KBIT,EQUAL,KORIG
!
      DO I=1,NUMSTA
        CALL KSBIT(KSTAT,I,KBIT(LSITEC(1,ICMP),I) )
        KORIG=.NOT.KBIT( DEFCMP, INT2(4) )
        DO J=1,ISTASP
           IF ( STASUP(J) == ISITN_CHR(I) ) THEN
                KORIG=.NOT.KBIT(CMPSUP(1,4),J)
                GOTO 10
           ENDIF
        ENDDO
10      CONTINUE
!
         CALL KSBIT( LSITEC(1,ICMP), I, KORIG.AND.KBIT(KSTAT,I) )
      ENDDO
!
      RETURN
      END
