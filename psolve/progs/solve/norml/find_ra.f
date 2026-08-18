      SUBROUTINE FIND_RA ( KSRCC, IRA )
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
      INCLUDE 'solve.i'
      INTEGER*2 ICMP,KSRCC(SRC_BIT_WORDS),IRA(MAX_SRC)
!
!  FIND ORIGIN STATION FOR DIRECTION SUPPRESION
!
      INCLUDE 'socom.i'
      INCLUDE 'prfil.i'
      INCLUDE 'glbc3.i'
!
      INTEGER*4 I,J,INOW
      LOGICAL*2 KBIT4, KBIT, KORIG
!
      INOW=0
      DO I=1,NUMSTR
         KORIG = KBIT( DEFSRC, INT2(3) )
         DO J=1,ISRCSP
            IF ( SRCSUP(J) .EQ. ISTRN_CHR(I) ) THEN
                 KORIG = KBIT4 ( SOUSUP(1,3), J )
                 GOTO 10
            ENDIF
         ENDDO
10       CONTINUE
!
         IF ( KORIG .AND. KBIT4(LSTAR(1,1),I) ) THEN
              CALL SBIT4 ( LSTAR(1,1), I, INT2(0) )
              CALL SBIT4 ( KSRCC, I, INT2(1) )
              INOW=INOW+1
              IRA(INOW)=I
            ELSE
              CALL SBIT4 ( KSRCC, I, INT2(0) )
         ENDIF
      ENDDO
!
      RETURN
      END  !#!  FINR_RA  #!#
