      SUBROUTINE FIND_VELO(KVELO,ICMP)
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
      INCLUDE 'solve.i'
      INTEGER*2 ICMP,Kvelo(STA_BIT_WORDS)
!
!  FIND ORIGIN VELOCITY
!
      INCLUDE 'socom.i'
      INCLUDE 'prfil.i'
      INCLUDE 'glbc3.i'
!
      INTEGER*2 I,J
      LOGICAL*2 KBIT,EQUAL,KORIG
!
      DO I=1,NUMSTA
        CALL KSBIT(KVELO,I,KBIT(LSITEV(1,ICMP),I) )
        KORIG=.NOT.KBIT( DEFVEL, INT2(4) )
        DO J=1,ISTASP
           IF ( STASUP(J) == ISITN_CHR(I) ) THEN
                KORIG = .NOT. KBIT ( VELSUP(1,4), J )
                GOTO 10
          ENDIF
        ENDDO
10      CONTINUE
        CALL KSBIT( LSITEV(1,ICMP), I, KORIG .AND. KBIT(KVELO,I) )
      ENDDO
!
      RETURN
      END
