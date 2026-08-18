      SUBROUTINE FIND_VELT ( KVELO, ICMP, IGRP, M_STA, L_STA, C_STA  )
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
      INCLUDE   'solve.i'
!CCCCC
!  pet  2004.03.15  Imporoved comments
!CCCCC
      INTEGER*2  ICMP, KVELO(STA_BIT_WORDS), IGRP
      INTEGER*4  M_STA, L_STA
      CHARACTER  C_STA(M_STA)*(*)
!
! --- Find velocity group to be tied together
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
         CALL KSBIT ( KVELO, I, KBIT ( LSITEV(1,ICMP), I ) )
         KORIG = .TRUE.
         DO J=1,ISTASP
            IF ( STASUP(J) .EQ. ISITN_CHR(I) ) THEN
                 IF ( VELTIES(J) .EQ. IGRP ) THEN
                      KORIG = .FALSE.
                      L_STA = L_STA + 1
                      C_STA(L_STA) = ISITN_CHR(I) 
                 END IF
                 GOTO 10
            ENDIF
         ENDDO
   10    CONTINUE
         CALL KSBIT ( LSITEV(1,ICMP), I, KORIG .AND. KBIT(KVELO,I) )
      ENDDO
!
      RETURN
      END  !#!  FIND_VELT  #!#
