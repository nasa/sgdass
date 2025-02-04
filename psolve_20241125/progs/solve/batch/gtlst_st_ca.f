      SUBROUTINE GTLST_ST_CA(TOKEN,STRING,KCAR,ISTRT,NOBJCT,SELAR, &
     &                        NXSEL,IMAX,SPACE,STACMP,CARCMP)
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
! 1.  GTLST_ST_CA PROGRAM SPECIFICATION
!
! 1.1 Get list of stations from CARRY section of the control file.
!
! 1.2 REFERENCES:
!
! 2.  GTLST_ST_CA INTERFACE
!
! 2.1 Parameter File
!
! 2.2 INPUT Variables:
!
      CHARACTER*1 SPACE
      CHARACTER*(*) STRING,STACMP
      INTEGER*2 NOBJCT,NXSEL,IMAX
!
! IMAX -
! NOBJCT -
! NXSEL -
! SPACE - Single blank character
! STACMP - Station component flag
! STRING - String to be parsed
!
! 2.3 OUTPUT Variables:
!
      CHARACTER*(*) TOKEN,CARCMP
      LOGICAL*2 KCAR
      INTEGER*2 ISTRT,SELAR(IMAX)
!
! CARCMP - Carry component flag
! ISTRT -
! KCAR - Carry flag
! SELAR -
! TOKEN - Token to be picked up from string
!
! 2.4 COMMON BLOCKS USED
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES: gcarry
!       CALLED SUBROUTINES: utility routines
!
! 3.  LOCAL VARIABLES
!
      LOGICAL*2 CFEOF,OKAY
      CHARACTER*1 STDUM(8)
      INTEGER*2 I,ISTDUM(4),LENGTH,CFREAD,IDUM
      CHARACTER*1 BSLASH
      EQUIVALENCE (STDUM(1),ISTDUM(1))
      BSLASH = CHAR(92)
!
! 4.  HISTORY
!   WHO   WHEN   WHAT
!   JLR   921215   replace '\' with BSLASH
!
!  5.  GTLST_ST_CA PROGRAM STRUCTURE
!
! Parse the string:
!
      CALL SPLITSTRING(STRING,TOKEN,STRING )
!
      IF(TOKEN.EQ.'YES') THEN
        KCAR=.TRUE.
      ELSE IF(TOKEN.EQ.'NO') THEN
        KCAR=.FALSE.
      ELSE
        CALL FERR( INT2(9010), 'ILLEGAL CARRY DEFAULT '//TOKEN(1:16), INT2(0), &
     &       INT2(0) )
      ENDIF
!
      CALL SPLITSTRING(STRING,TOKEN,STRING )
      OKAY=.TRUE.
      DO I=1,3
        OKAY=OKAY.AND.(TOKEN(I:I).EQ.'-'.OR.TOKEN(I:I).EQ.STACMP(I:I))
      ENDDO
      IF(TOKEN.EQ.' '.AND.STACMP.NE.' ') THEN
        CARCMP=STACMP
        RETURN
      ELSE IF(TOKEN.NE.'EXCEPT'.AND..NOT.OKAY) THEN
        CALL FERR( INT2(9005), 'CARRY COMPONENTS MUST MATCH STATIONS', &
     &       INT2(0), INT2(0) )
      ELSE IF(TOKEN.NE.'EXCEPT'.AND.STACMP.EQ.' ') THEN
        CALL FERR( INT2(9006), '$FLAGS MUST OCCCUR BEFORE $CARRY', INT2(0), &
     &       INT2(0) )
      ELSE IF(TOKEN.NE.'EXCEPT'.AND.OKAY) THEN
        CARCMP=TOKEN
        CALL SPLITSTRING(STRING,TOKEN,STRING )
      ENDIF
      IF(TOKEN.EQ.' ') THEN
        RETURN
      ELSE IF(TOKEN.EQ.'EXCEPT') THEN
        CALL SPLITSTRING(STRING,TOKEN,STRING )
        DO WHILE (TOKEN.NE.' ')
          IF(TOKEN.EQ.BSLASH) THEN
            LENGTH=CFREAD(STRING)
            IF(STRING(1:1).EQ.'$'.OR.CFEOF(IDUM)) THEN
              CALL FERR( INT2(9020), 'ILLEGAL CONTINUATION LINE '// &
     &             STRING(1:10), INT2(0), INT2(0) )
            ENDIF
          ELSE
            IF(NXSEL+3.GT.IMAX) CALL FERR( INT2(9030), &
     &        'RUN OUT OF CARRY ROOM '//TOKEN(1:10), INT2(0), INT2(0) )
            IF(NOBJCT.EQ.0) ISTRT=NXSEL
            DO I=1,8
              STDUM(I)=TOKEN(I:I)
              IF(STDUM(I).EQ.SPACE) STDUM(I)=' '
            ENDDO
            DO I=1,4
              SELAR(NXSEL)=ISTDUM(I)
              NXSEL=NXSEL+1
            ENDDO
            NOBJCT=NOBJCT+1
          ENDIF
          CALL SPLITSTRING(STRING,TOKEN,STRING )
        ENDDO
      ENDIF
!
      RETURN
      END
