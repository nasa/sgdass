      SUBROUTINE GSTASP(DEFVEL,VELSUP,STASUP,ISTASP,DEFCMP,CMPSUP, &
     &                  STACMP,TOKEN,STRING)
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
      LOGICAL*2 L4TOL2
! 1.  GSTASP PROGRAM SPECIFICATION
!
! 1.1 Parse STATIONS line of $SUPPRESSION section.
!
! 1.2 REFERENCES:
!
! 2.  GSTASP INTERFACE
!
! 2.1 Parameter File
      INCLUDE 'solve.i'
!
! 2.2 INPUT Variables:
!
      INTEGER*2 DEFVEL
      INTEGER*2 ISTASP
      CHARACTER STRING*(*), STACMP*(*) 
!
! DEFVEL - Default tation components for velocities
! ISTASP - Number of stations in exception list
! STACMP - Station component flag
! STRING - Remainder of STATIONS line
!
! 2.3 OUTPUT Variables:
!
      INTEGER*2 VELSUP(STA_BIT_WORDS,6)
      CHARACTER STASUP(MAX_STA)*(*)
      INTEGER*2 DEFCMP,CMPSUP(STA_BIT_WORDS,*)
      CHARACTER TOKEN*(*) 
!
! CMPSUP - Station suppression flag
! DEFCMP - Default station components
! STASUP - Station components for exception list
! TOKEN - Single token from STRING
! VELSUP - Velocity suppression flag
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
      CHARACTER*3 CMP
      CHARACTER*8 TEMP
      INTEGER*2 I,LENGTH,CFREAD,IDUM,J,IPOS,KBITN,IVAL
      CHARACTER*1 BSLASH
      BSLASH = CHAR(92)
!
! 4.  HISTORY
!   WHO   WHEN   WHAT
!   JLR   921215     replace '\' with BSLASH
!   kdb   961125     declare cmpsup,velsup with *.  (Also fixes error where velsup
!                    was declared (,3),  not (,4)).
!   pet   2022.02.28 Fixed a bug: STASUP is to be character array
!
! 5.  GSTASP PROGRAM STRUCTURE
!
      CALL SPLITSTRING(STRING,TOKEN,STRING )
      OKAY=CHKCMP(TOKEN,STACMP)
      IF(OKAY) THEN
        CALL KSBIT( DEFCMP, INT2(1), l4tol2(TOKEN(1:1).NE.'-') )
        CALL KSBIT( DEFCMP, INT2(2), l4tol2(TOKEN(2:2).NE.'-') )
        CALL KSBIT( DEFCMP, INT2(3), l4tol2(TOKEN(3:3).NE.'-') )
      ELSE IF(TOKEN.EQ.'NO') THEN
        CONTINUE
      ELSE IF(TOKEN.EQ.'YES') THEN
        CALL SBIT( DEFCMP, INT2(1), INT2(1) )
        CALL SBIT( DEFCMP, INT2(2), INT2(1) )
        CALL SBIT( DEFCMP, INT2(3), INT2(1) )
      ELSE
        CALL FERR( INT2(9010), 'ILLEGAL STATIONS PARAMETER '//TOKEN(1:16), &
     &       INT2(0), INT2(0) )
      ENDIF
!
      DO J=1,3
        IVAL=KBITN(DEFCMP,J)
!
!***** Initialize entire cmpsup instead of just first istasp elements
!
!        DO I=1,ISTASP
        DO I=1,MAX_STA
          CALL SBIT(CMPSUP(1,J),I,IVAL )
        ENDDO
      ENDDO
!
      CALL SPLITSTRING(STRING,TOKEN,STRING )
      IF(TOKEN.EQ.'EXCEPT') THEN
        CALL SPLITSTRING(STRING,TOKEN,STRING )
        CMP=' '
        DO WHILE (TOKEN.NE.' ')
          OKAY=CHKCMP(TOKEN,STACMP)
          IF(OKAY) THEN
            CMP=TOKEN
          ELSE IF(TOKEN.EQ.BSLASH) THEN
            LENGTH=CFREAD(STRING)
            IF(STRING(1:1).EQ.'$'.OR.CFEOF(IDUM))CALL FERR( INT2(9020), &
     &        'ILLEGAL CONTINUATION LINE '//STRING(1:10), INT2(0), INT2(0) )
          ELSE
            IF(CMP.EQ.' ') CALL FERR( INT2(9025), &
     &        'FIRST TOKEN AFTER EXCEPT '//'MUST ESTABLISH COMPONENTS', &
     &         INT2(0), INT2(0) )
!@U            CALL UNDSCR(TOKEN )
            DO I=1,ISTASP
!@              CALL HOL2CHAR( STASUP(1,I), INT2(1), INT2(8), TEMP )
!@              IPOS=I
!@              IF(TOKEN.EQ.TEMP) GO TO 90
              IPOS=I
              IF ( TOKEN .EQ. STASUP(I) ) GOTO 90
            ENDDO
            ISTASP=ISTASP+1
            IF(ISTASP.GT.MAX_STA)CALL FERR( INT2(9030), 'TOO MANY STATIONS '// &
     &         TOKEN(1:10), INT2(0), INT2(0) )
!@            CALL CHAR2HOL( TOKEN, STASUP(1,ISTASP), INT2(1), INT2(8) )
            STASUP(ISTASP) = TOKEN
!
!***** remove following lines because this initialization will now be
!       done elsewhere    mwh  930830
!
!            CALL KSBIT(VELSUP(1,1),(ISTASP),KBIT(DEFVEL,1))
!            CALL KSBIT(VELSUP(1,2),(ISTASP),KBIT(DEFVEL,2))
!            CALL KSBIT(VELSUP(1,3),(ISTASP),KBIT(DEFVEL,3))
!            CALL KSBIT(CMPSUP(1,4),(ISTASP),KBIT(DEFCMP,4))
            IPOS=ISTASP
90          CONTINUE
            CALL KSBIT( CMPSUP(1,1), IPOS, l4tol2(CMP(1:1).NE.'-') )
            CALL KSBIT( CMPSUP(1,2), IPOS, l4tol2(CMP(2:2).NE.'-') )
            CALL KSBIT( CMPSUP(1,3), IPOS, l4tol2(CMP(3:3).NE.'-') )
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
