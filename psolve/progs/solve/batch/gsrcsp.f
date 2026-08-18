      SUBROUTINE GSRCSP(DEFSRC,SOUSUP,SRCSUP,ISRCSP,TOKEN,STRING)
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
      LOGICAL*2 L4TOL2
!
! 1.  GSRCSP PROGRAM SPECIFICATION
!
! 1.1
!
! 1.2 REFERENCES:
!
! 2.  GSRCSP INTERFACE
!
! 2.1 Parameter File
      INCLUDE 'solve.i'
!
! 2.2 INPUT Variables:
!
      INTEGER*2 ISRCSP
      CHARACTER*(*) STRING
!
! ISRCSP -
! STRING - String to be parsed
!
! 2.3 OUTPUT Variables:
!
      INTEGER*2 DEFSRC,SOUSUP(SRC_BIT_WORDS,*),SRCSUP(4,MAX_SRC)
      CHARACTER*(*) TOKEN
!
! DEFSRC - Default right_ascension
! SOUSUP -
! SRCSUP -
! TOKEN - Token picked up from string
!
! 2.4 COMMON BLOCKS USED
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES: gsuprs
!       CALLED SUBROUTINES: utility routines
!
! 3.  LOCAL VARIABLES
!
      LOGICAL*2 CFEOF,CHKCMP,OKAY,KBIT
      CHARACTER*2 RAD
      CHARACTER*8 TEMP
      INTEGER*2 I,LENGTH,CFREAD,IDUM,J,IVAL,KBITN,IPOS
      CHARACTER*1 BSLASH
      BSLASH = CHAR(92)
!
! 4.  HISTORY
!   WHO   WHEN   WHAT
!   JLR   921215   replaced '\' with BSLASH
!   kdb   961125   SOUSUP now declared with *.  (Also fixes error; SOUSUP was
!                  declared (,5), not (,6)).
!   pet   2000.07.19  Added support of qualfiers YES, NO
!
! 5.  GSRCSP PROGRAM STRUCTURE
!
      CALL SPLITSTRING(STRING,TOKEN,STRING )
      OKAY=CHKCMP(TOKEN,'RD')
      IF(OKAY) THEN
        CALL KSBIT( DEFSRC, INT2(1), l4tol2(TOKEN(1:1).EQ.'R') )
        CALL KSBIT( DEFSRC, INT2(2), l4tol2(TOKEN(2:2).EQ.'D') )
      ELSE IF(TOKEN.EQ.'NO') THEN
        CONTINUE
      ELSE IF(TOKEN.EQ.'YES') THEN
        CALL SBIT( DEFSRC, INT2(1), INT2(1) )
        CALL SBIT( DEFSRC, INT2(2), INT2(1) )
      ELSE
        CALL FERR( INT2(9010), 'ILLEGAL SOURCES PARAMETER '//TOKEN(1:16), &
     &       INT2(0), INT2(0) )
      ENDIF
!
      DO J=1,2
        IVAL=KBITN(DEFSRC,J)
        DO I=1,ISRCSP
          CALL SBIT(SOUSUP(1,J),I,IVAL )
        ENDDO
      ENDDO
!
      CALL SPLITSTRING(STRING,TOKEN,STRING )
      IF(TOKEN.EQ.'EXCEPT') THEN
        CALL SPLITSTRING(STRING,TOKEN,STRING )
        RAD=' '
        DO WHILE (TOKEN.NE.' ')
          OKAY=CHKCMP(TOKEN,'RD')
          IF(OKAY) THEN
            RAD=TOKEN
          ELSE IF(TOKEN.EQ.BSLASH) THEN
            LENGTH=CFREAD(STRING)
            IF(STRING(1:1).EQ.'$'.OR.CFEOF(IDUM))CALL FERR( INT2(9020), &
     &        'ILLEGAL CONTINUATION LINE '//STRING(1:10), INT2(0), INT2(0) )
          ELSE
            IF(RAD.EQ.' ') CALL FERR( INT2(9025), &
     &        'FIRST TOKEN AFTER EXCEPT '//'MUST ESTABLISH COMPONENTS', &
     &         INT2(0), INT2(0) )
!@U            CALL UNDSCR(TOKEN )
            DO I=1,ISRCSP
              CALL HOL2CHAR( SRCSUP(1,I), INT2(1), INT2(8), TEMP )
              IPOS=I
              IF(TOKEN.EQ.TEMP) GO TO 90
            ENDDO
            ISRCSP=ISRCSP+1
            IF(ISRCSP.GT.MAX_SRC)CALL FERR( INT2(9030), 'TOO MANY SOURCES '// &
     &         TOKEN(1:10), INT2(0), INT2(0) )
            CALL CHAR2HOL( TOKEN, SRCSUP(1,ISRCSP), INT2(1), INT2(8) )
            CALL KSBIT( SOUSUP(1,3), INT2((ISRCSP)), KBIT( DEFSRC, INT2(3) ) )
            IPOS=ISRCSP
90          CONTINUE
            CALL KSBIT( SOUSUP(1,1), IPOS, l4tol2(RAD(1:1).EQ.'R') )
            CALL KSBIT( SOUSUP(1,2), IPOS, l4tol2(RAD(2:2).EQ.'D') )
          ENDIF
          CALL SPLITSTRING(STRING,TOKEN,STRING )
        ENDDO
      ELSE IF (TOKEN.NE.' ') THEN
        CALL FERR( INT2(9040), 'INCORECT EXCEPT CLAUSE: '//TOKEN(1:10), &
     &       INT2(0), INT2(0) )
      ENDIF
!
      RETURN
      END
