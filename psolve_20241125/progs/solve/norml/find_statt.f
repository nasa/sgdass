      SUBROUTINE FIND_STATT ( KST, ICMP, IGRP, M_STA, L_STA, C_STA  )
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
      INCLUDE   'solve.i'
      INTEGER*2  ICMP, KST(STA_BIT_WORDS), IGRP
      INTEGER*4  M_STA, L_STA
      CHARACTER  C_STA(M_STA)*(*)
!
!  FIND station group to be tied together
!
      INCLUDE 'socom.i'
      INCLUDE 'prfil.i'
      INCLUDE 'glbc3.i'
!
      INTEGER*2 I,J
      LOGICAL*2 KBIT,EQUAL,KORIG
!
      L_STA = 0
      DO I=1,NUMSTA
         CALL KSBIT ( KST, I, KBIT(LSITEC(1,ICMP), I ) )
         KORIG=.TRUE.
         DO J=1,ISTASP
            IF ( STASUP(I) == ISITN_CHR(I) ) THEN
                 IF ( STATIES(J) .EQ. IGRP ) KORIG = .FALSE.
                 GOTO 10
           ENDIF
         ENDDO
10       CONTINUE
         CALL KSBIT( LSITEC(1,ICMP), I, KORIG .AND. KBIT(KST,I) )
         IF ( .NOT. KBIT ( LSITEC(1,ICMP), I ) ) THEN
              L_STA = L_STA + 1
              C_STA(L_STA) = ISITN_CHR(I) 
         END IF
      ENDDO
!
      RETURN
      END
