      SUBROUTINE FIND_NUT(KNUT,KFLPSI,KFLEPS,KIDPNUT, &
     &                     KNDPNUT,KNFLPSI,KNFLEPS,INUT)
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
      INTEGER*2 KNUT(3),KFLPSI(14),KFLEPS(14),KNDPNUT,KNFLPSI,KNFLEPS
      INTEGER*2 INUT(2,2,116),KIDPNUT(7)
!
      INCLUDE 'solve.i'
      INCLUDE 'socom.i'
      INCLUDE 'prfil.i'
      INCLUDE 'glbc3.i'
!
      INTEGER*2 I,J,NP,K
      LOGICAL*2 EQUAL,KBIT
!
!  FIND NUTATION PARMETERS
!
      NP=0
      DO I=1,14
        KFLPSI(I)=0
        KFLEPS(I)=0
      ENDDO
      DO I=1,116
        DO J=1,2
          DO K=1,2
            INUT(K,J,I)=0
          ENDDO
        ENDDO
      ENDDO
!
!   OFFSETS
!
      KNUT(1)=LNUT(1)
      IF(KBIT( LNUT(1), INT2(1) )) THEN
        CALL SBIT( LNUT(1), INT2(1), INT2(0) )
        NP=NP+1
        INUT(1,1,7)=NP
      ENDIF
      IF(KBIT( LNUT(1), INT2(2) )) THEN
        CALL SBIT( LNUT(1), INT2(2), INT2(0) )
        NP=NP+1
        INUT(1,1,7)=NP
      ENDIF
!
!  MAIN SIX PERIODS
!
      KNUT(2)=LNUT(2)
      DO J=1,6
        INUT(1,1,J)=0
        INUT(2,1,J)=0
        IF(KBIT(LNUT(2),J)) THEN
          CALL SBIT( LNUT(2), J, INT2(0) )
          NP=NP+1
          INUT(1,1,J)=NP
          NP=NP+1
          INUT(2,1,J)=NP
        ENDIF
      ENDDO
      KNUT(3)=LNUT(3)
      DO J=1,6
        INUT(1,2,J)=0
        INUT(2,2,J)=0
        IF(KBIT(LNUT(3),J)) THEN
          CALL SBIT( LNUT(3), J, INT2(0) )
          NP=NP+1
          INUT(1,2,J)=NP
          NP=NP+1
          INUT(2,2,J)=NP
        ENDIF
      ENDDO
!
      IF(KBIT( FLPSI, INT2(1) )) THEN
        CALL SBIT( FLPSI, INT2(1), INT2(0) )
        NP=NP+1
        INUT(1,1,8)=NP
      ENDIF
!
      IF(KBIT( FLPSI, INT2(2) )) THEN
        CALL SBIT( FLPSI, INT2(2), INT2(0) )
        CALL SBIT( KFLPSI, INT2(2), INT2(1) )
        NP=NP+1
        INUT(1,1,9)=NP
      ENDIF
!
      IF(KBIT( FLPSI, INT2(3) )) THEN
        CALL SBIT( FLPSI, INT2(3), INT2(0) )
        CALL SBIT( KFLPSI, INT2(3), INT2(1) )
        NP=NP+1
        INUT(1,1,10)=NP
      ENDIF
!
      IF(KBIT( FLPSI, INT2(4) )) THEN
        CALL SBIT( FLPSI, INT2(4), INT2(0) )
        CALL SBIT( KFLPSI, INT2(4), INT2(1) )
        NP=NP+1
        INUT(2,1,10)=NP
      ENDIF
!
      DO J=11,116
        I=J-10
        IF(KBIT( FLPSI, INT2(4+(I-1)*2+2))) THEN
          CALL SBIT( FLPSI, INT2(4+(I-1)*2+2), INT2(0) )
          CALL SBIT( KFLPSI, INT2(4+(I-1)*2+2), INT2(1) )
          NP=NP+1
          INUT(1,1,J)=NP
        ENDIF
        IF(KBIT( FLPSI, INT2(4+(I-1)*2+1))) THEN
          CALL SBIT( FLPSI, INT2(4+(I-1)*2+1), INT2(0) )
          CALL SBIT( KFLPSI, INT2(4+(I-1)*2+1), INT2(1) )
          NP=NP+1
          INUT(2,1,J)=NP
        ENDIF
      ENDDO
!
      IF(KBIT( FLEPS, INT2(1) )) THEN
        CALL SBIT( FLEPS, INT2(1), INT2(0) )
        CALL SBIT( KFLEPS, INT2(1), INT2(1) )
        NP=NP+1
        INUT(1,2,8)=NP
      ENDIF
!
      IF(KBIT( FLEPS, INT2(2) )) THEN
        CALL SBIT( FLEPS, INT2(2), INT2(0) )
        CALL SBIT( KFLEPS, INT2(2), INT2(1) )
        NP=NP+1
        INUT(1,2,9)=NP
      ENDIF
!
      IF(KBIT( FLEPS, INT2(3) )) THEN
        CALL SBIT( FLEPS, INT2(3), INT2(0) )
        CALL SBIT( KFLEPS, INT2(3), INT2(1) )
        NP=NP+1
        INUT(1,2,10)=NP
      ENDIF
!
      IF(KBIT( FLEPS, INT2(4) )) THEN
        CALL SBIT( FLEPS, INT2(4), INT2(0) )
        CALL SBIT( KFLEPS, INT2(4), INT2(1) )
        NP=NP+1
        INUT(2,2,10)=NP
      ENDIF
!
      DO J=11,116
        I=J-10
        IF(KBIT( FLEPS, INT2(4+(I-1)*2+1))) THEN
          CALL SBIT( FLEPS, INT2(4+(I-1)*2+1), INT2(0) )
          CALL SBIT( KFLEPS, INT2(4+(I-1)*2+1), INT2(1) )
          NP=NP+1
          INUT(1,2,J)=NP
        ENDIF
        IF(KBIT( FLEPS, INT2(4+(I-1)*2+2))) THEN
          CALL SBIT( FLEPS, INT2(4+(I-1)*2+2), INT2(0) )
          CALL SBIT( KFLEPS, INT2(4+(I-1)*2+2), INT2(1) )
          NP=NP+1
          INUT(2,2,J)=NP
        ENDIF
      ENDDO
!
      KNDPNUT=NDPNUT
      KNFLPSI=NFLPSI
      KNFLEPS=NFLEPS
      DO I=1,7
        KIDPNUT(I)=IDPNUT(I)
        IDPNUT(I)=0
      ENDDO
!
      NDPNUT=0
      NFLPSI=0
      NFLEPS=0
!
      RETURN
      END
