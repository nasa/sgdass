      SUBROUTINE FIND_SUPRS()
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!     Updated to handle logical expression difficulties
      logical*2 LHOLD
!     Jim Ryan Feb 20022
!-------------------------------------------------
!
      INCLUDE 'solve.i'
      INCLUDE 'socom.i'
      INCLUDE 'prfil.i'
      INCLUDE 'glbc3.i'
!
      INTEGER*2 ICHCM,IBIT1,IBIT2,IBIT3,IBIT4,KBITN4,VEL,CMP,RAD
      INTEGER*4 I, J
      LOGICAL*2 EQUAL,KBIT4
      CHARACTER*1 COMP,PHASE
      CHARACTER*2 LOCAL
!
      DO I=1,NUMSTA
        VEL=DEFVEL
        CMP=DEFCMP
        DO J=1,ISTASP
          IF ( STASUP(J) == ISITN_CHR(I) ) THEN
            CALL KSBIT( VEL, INT2(1), KBIT4(VELSUP(1,1),J) )
            CALL KSBIT( VEL, INT2(2), KBIT4(VELSUP(1,2),J) )
            CALL KSBIT( VEL, INT2(3), KBIT4(VELSUP(1,3),J) )
            CALL KSBIT( CMP, INT2(1), KBIT4(CMPSUP(1,1),J) )
            CALL KSBIT( CMP, INT2(2), KBIT4(CMPSUP(1,2),J) )
            CALL KSBIT( CMP, INT2(3), KBIT4(CMPSUP(1,3),J) )
            GO TO 10
          ENDIF
        ENDDO
10      CONTINUE
        DO J=1,3
          IF(KBIT4(LSITEV(1,J),I).AND.KBIT4( IUEN, 1 )) THEN
            LHOLD = .NOT.KBIT4(VEL,J)
            CALL KSBIT( LSITEV(1,J), I, LHOLD )
          ENDIF
          IF(KBIT4(LSITEC(1,J),I)) THEN
            LHOLD = .NOT.KBIT4(CMP,J)
            CALL KSBIT( LSITEC(1,J), I, LHOLD )
          ENDIF
        ENDDO
      ENDDO
!
      DO I=1,NUMSTR
        RAD=DEFSRC
        DO J=1,ISRCSP
          IF ( SRCSUP(J) == ISTRN_CHR(I) ) THEN
            CALL KSBIT( RAD, INT2(1), KBIT4(SOUSUP(1,1),J) )
            CALL KSBIT( RAD, INT2(2), KBIT4(SOUSUP(1,2),J) )
            CALL KSBIT( RAD, INT2(4), KBIT4(SOUSUP(1,4),J) )
            CALL KSBIT( RAD, INT2(5), KBIT4(SOUSUP(1,5),J) )
            GO TO 20
          ENDIF
        ENDDO
20      CONTINUE
        DO J=1,2
          IF(KBIT4(LSTAR(1,J),I)) THEN
            LHOLD = .NOT.KBIT4(RAD,J)
            CALL KSBIT( LSTAR(1,J), I, LHOLD )
          ENDIF
          IF(KBIT4(LPROP(1,J),I)) THEN
            LHOLD = .NOT.KBIT4( RAD, J+3 )
            CALL KSBIT( LPROP(1,J), I, LHOLD )
          ENDIF
        ENDDO
      ENDDO
!
!  DO WE SUPPRESS NUTATION
!
      DO J=1,6
        CALL HOL2CHAR( NUTSUP(J), INT2(1), INT2(2), LOCAL )
        COMP=LOCAL(1:1)
        PHASE=LOCAL(2:2)
        IF((COMP.EQ.'P'.OR.COMP.EQ.'B').AND.KBIT4(LNUT(2),J)) THEN
          CALL SBIT( LNUT(2), J, INT2(0) )
        ENDIF
        IF((COMP.EQ.'E'.OR.COMP.EQ.'B').AND.KBIT4(LNUT(3),J)) THEN
          CALL SBIT( LNUT(3), J, INT2(0) )
        ENDIF
      ENDDO
      CALL HOL2CHAR( NUTSUP(7), INT2(1), INT2(2), LOCAL )
      IF( LOCAL(1:1).EQ.'P'.OR.LOCAL(1:1).EQ.'B')CALL SBIT( LNUT(1), INT2(1), &
     &   INT2(0) )
      IF( LOCAL(1:1).EQ.'E'.OR.LOCAL(1:1).EQ.'B')CALL SBIT( LNUT(1), INT2(2), &
     &   INT2(0) )
!
      CALL HOL2CHAR( NUTSUP(8), INT2(1), INT2(2), LOCAL )
      IF(LOCAL(1:1).EQ.'P'.OR.LOCAL(1:1).EQ.'B')CALL SBIT( FLPSI(1), INT2(1), &
     &   INT2(0) )
      IF(LOCAL(1:1).EQ.'E'.OR.LOCAL(1:1).EQ.'B')CALL SBIT( FLEPS(1), INT2(1), &
     &   INT2(0) )
!
      CALL HOL2CHAR( NUTSUP(9), INT2(1), INT2(2), LOCAL )
      IF(LOCAL(1:1).EQ.'P'.OR.LOCAL(1:1).EQ.'B')CALL SBIT( FLPSI(1), INT2(2), &
     &   INT2(0) )
      IF(LOCAL(1:1).EQ.'E'.OR.LOCAL(1:1).EQ.'B')CALL SBIT( FLEPS(1), INT2(2), &
     &   INT2(0) )
!
      CALL HOL2CHAR( NUTSUP(10), INT2(1), INT2(2), LOCAL )
      IF((LOCAL(1:1).EQ.'P'.OR.LOCAL(1:1).EQ.'B').AND. &
     &   (KBIT4( FLPSI(1), 3) .OR. KBIT4( FLPSI(1), 4 ))) THEN
        CALL SBIT( FLPSI(1), INT2(3), INT2(0) )
        CALL SBIT( FLPSI(1), INT2(4), INT2(0) )
      ENDIF
      IF(LOCAL(1:1).EQ.'E'.OR.LOCAL(1:1).EQ.'B') THEN
        CALL SBIT( FLEPS(1), INT2(3), INT2(0) )
        CALL SBIT( FLEPS(1), INT2(4), INT2(0) )
      ENDIF
!
      NDPNUT=0
      DO I=11,116
        CALL HOL2CHAR( NUTSUP(I), INT2(1), INT2(2), LOCAL )
        COMP =LOCAL(1:1)
        PHASE=LOCAL(2:2)
        IF(COMP.EQ.'P'.OR.COMP.EQ.'B') THEN
          IF(PHASE.EQ.'I'.OR.PHASE.EQ.'B') THEN
            CALL SBIT( FLPSI(1), INT2(4+(I-11)*2+2), INT2(0) )
          ENDIF
        ENDIF
        IF(COMP.EQ.'P'.OR.COMP.EQ.'B') THEN
          IF(PHASE.EQ.'O'.OR.PHASE.EQ.'B') THEN
            CALL SBIT( FLPSI(1), INT2(4+(I-11)*2+1), INT2(0) )
          ENDIF
        ENDIF
        IF(COMP.EQ.'E'.OR.COMP.EQ.'B') THEN
          IF(PHASE.EQ.'I'.OR.PHASE.EQ.'B') THEN
            CALL SBIT( FLEPS(1), INT2(4+(I-11)*2+1), INT2(0) )
          ENDIF
        ENDIF
        IF(COMP.EQ.'E'.OR.COMP.EQ.'B') THEN
          IF(PHASE.EQ.'O'.OR.PHASE.EQ.'B') THEN
            CALL SBIT( FLEPS(1), INT2(4+(I-11)*2+2), INT2(0) )
          ENDIF
        ENDIF
        IBIT1=KBITN4( FLPSI(1), 4+(I-11)*2+2 )
        IBIT2=KBITN4( FLPSI(1), 4+(I-11)*2+1 )
        IBIT3=KBITN4( FLEPS(1), 4+(I-11)*2+1 )
        IBIT4=KBITN4( FLEPS(1), 4+(I-11)*2+2 )
        CALL SBIT( IDPNUT, INT2(I-10), INT2(0) )
        IF(IBIT1+IBIT2+IBIT3+IBIT4.GT.0) THEN
          CALL SBIT( IDPNUT, INT2(I-10), INT2(1) )
          NDPNUT=NDPNUT+1
        ENDIF
      ENDDO
!
      NFLPSI=0
      NFLEPS=0
      DO I=1,216
        NFLPSI=NFLPSI+KBITN4(FLPSI,I)
        NFLEPS=NFLEPS+KBITN4(FLEPS,I)
      ENDDO
!
!   PRECESSION?
!
      IF(PRESUP) LPREC=0
!
!   RELATIVITY?
!
      IF(RELSUP) LREL=0
!
!   TIDES?
!
      IF(TIDSUP) THEN
        LTIDE(1,1)=0
        LTIDE(1,2)=0
        LTIDE(1,3)=0
        ITDGLB=0
      ENDIF
      RETURN
      END
