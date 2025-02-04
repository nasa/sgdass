      SUBROUTINE GET_GCLOCK_DEG ( MAX_GCLOCK_DEG )
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
      INCLUDE 'solve.i'
      INCLUDE 'glbc4.i'
      INCLUDE 'socom.i'
!
      INTEGER*4 MAX_GCLOCK_DEG, ISTA, POS
      LOGICAL*2 KEEP_GOING, KBIT
!
      MAX_GCLOCK_DEG = SETFL_MDEG
      ISTA = 0
      KEEP_GOING = .TRUE.
      DO WHILE ( KEEP_GOING .AND. ISTA .LT. NUMSTA )
         ISTA = ISTA+1
         POS=ICLSTR(ISTA)+1
         IF ( KBIT( LCLK(POS), INT2(1) ) ) THEN ! Initial clock for this site on.
              MAX_GCLOCK_DEG = 0
              KEEP_GOING = .FALSE.
              IF ( KBIT ( LCLK(POS), INT2(2) ) ) MAX_GCLOCK_DEG = 1
              IF ( KBIT ( LCLK(POS), INT2(3) ) ) MAX_GCLOCK_DEG = 2
        ENDIF
      ENDDO
!
      RETURN
      END  !#!  GET_GCLOCK_DEG  #!#
