      SUBROUTINE FIND_DC ( KSRCC, IRA )
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
      INCLUDE   'solve.i'
      INCLUDE   'socom.i'
      INCLUDE   'prfil.i'
      INCLUDE   'glbc3.i'
      INTEGER*2  KSRCC(SRC_BIT_WORDS)
      INTEGER*4  IRA(MAX_SRC)
!
!  FIND ???
!
      INTEGER*4 I, J, INOW
      LOGICAL*2 KBIT4, KBIT, KORIG
!
      INOW=0
      DO I=1,NUMSTR
         KORIG=KBIT( DEFSRC, INT2(6))
         DO J=1,ISRCSP
            IF ( SRCSUP(J) .EQ. ISTRN_CHR(I) ) THEN
                 KORIG = KBIT4 ( SOUSUP(1,6), J )
                 GOTO 10
            ENDIF
         ENDDO
  10     CONTINUE
!
         IF ( KORIG .AND. KBIT4 ( LSTAR(1,2), I) ) THEN
              CALL SBIT4 ( LSTAR(1,2), I, INT2(0) )
              CALL SBIT4 ( KSRCC, I, INT2(1) )
              INOW=INOW+1
              IRA(INOW)=I
           ELSE
              CALL SBIT4 ( KSRCC, I, INT2(0) )
         ENDIF
      ENDDO
!
      RETURN
      END  !#!  FIND_DC  #!#
