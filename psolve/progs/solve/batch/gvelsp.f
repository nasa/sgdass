      SUBROUTINE GVELSP(DEFVEL,VELSUP,STASUP,ISTASP,DEFCMP,CMPSUP, &
     &                  TOKEN,STRING)
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
      LOGICAL*2 L4TOL2
! 1.  GVELSP PROGRAM SPECIFICATION
!
! 1.1 Get VELOCITY info from SUPPRESSION section of control file.
!
! 1.2 REFERENCES:
!
! 2.  GVELSP INTERFACE
!
! 2.1 Parameter File
      INCLUDE 'solve.i'
!
! 2.2 INPUT Variables:
!
      INTEGER*2 STASUP(4,MAX_STA)
      INTEGER*2 DEFCMP
      CHARACTER*(*) STRING
!
! DEFCMP - default station components
! STASUP - Station components for stations in exception list
! STRING - String to be parsed
!
! 2.3 OUTPUT Variables:
!
      INTEGER*2 DEFVEL(2),VELSUP(STA_BIT_WORDS,*)
      INTEGER*2 CMPSUP(STA_BIT_WORDS,*),ISTASP
      CHARACTER*(*) TOKEN
!
! CMPSUP - Station suppression flag
! DEFVEL - default station components for velocities
! ISTASP - Number of stations in exception list
! TOKEN - Individual token from STRING
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
      CHARACTER*3 VEL
      CHARACTER*8 TEMP
      INTEGER*2 I,LENGTH,CFREAD,IDUM,J,IPOS,KBITN,IVAL
      CHARACTER*1  BSLASH
      BSLASH = CHAR(92)
!
! 4.  HISTORY
!   WHO   WHEN   WHAT
!   JLR   921215   replace '\' with BSLASH
!   kdb   961125   declare cmpsup,velsup with *.  (Also fixes error where velsup
!                  was declared (,3),  not (,4)).
!   pet   2000.07.19  Added support of qualifer NO
!
! 5.  GVELSP PROGRAM STRUCTURE
!
! Get first token from input string; must be YES, NO or UEN
!
      CALL SPLITSTRING(STRING,TOKEN,STRING )
      OKAY=CHKCMP(TOKEN,'UEN')
      IF(OKAY) THEN
        CALL KSBIT( DEFVEL, INT2(1), l4tol2(TOKEN(1:1).EQ.'U') )
        CALL KSBIT( DEFVEL, INT2(2), l4tol2(TOKEN(2:2).EQ.'E') )
        CALL KSBIT( DEFVEL, INT2(3), l4tol2(TOKEN(3:3).EQ.'N') )
      ELSE IF(TOKEN.EQ.'NO') THEN
        CONTINUE
      ELSE IF(TOKEN.EQ.'YES') THEN
        CALL SBIT( DEFVEL, INT2(1), INT2(1) )
        CALL SBIT( DEFVEL, INT2(2), INT2(1) )
        CALL SBIT( DEFVEL, INT2(3), INT2(1) )
      ELSE
        CALL FERR( INT2(9010), 'ILLEGAL VELOCITIES PARAMETER '//TOKEN(1:16), &
     &       INT2(0), INT2(0) )
      ENDIF
!
! Set up velocity suppression flag with appropriate defaults
!
      DO J=1,3
        IVAL=KBITN(DEFVEL,J)
!
!******* initialize entire velsup array, not just first istasp elements
!           mwh  930830
!
!        DO I=1,ISTASP
        DO I=1,MAX_STA
          CALL SBIT(VELSUP(1,J),I,IVAL )
        ENDDO
      ENDDO
!
! Get next token; if not EXCEPT then we're finished
!
      CALL SPLITSTRING(STRING,TOKEN,STRING )
      IF(TOKEN.EQ.'EXCEPT') THEN
!
! Get next token and then loop until no more tokens
!
        CALL SPLITSTRING(STRING,TOKEN,STRING )
        VEL=' '
        DO WHILE (TOKEN.NE.' ')
          OKAY=CHKCMP(TOKEN,'UEN')
          IF(OKAY) THEN
            VEL=TOKEN
!
! Handle continuation line by reading next line of control file
!
          ELSE IF(TOKEN.EQ.BSLASH) THEN
            LENGTH=CFREAD(STRING)
            IF(STRING(1:1).EQ.'$'.OR.CFEOF(IDUM))CALL FERR( INT2(9020), &
     &        'ILLEGAL CONTINUATION LINE '//STRING(1:10), INT2(0), INT2(0) )
          ELSE
            IF(VEL.EQ.' ') CALL FERR( INT2(9025), &
     &        'FIRST TOKEN AFTER EXCEPT '//'MUST ESTABLISH COMPONENTS', &
     &         INT2(0), INT2(0) )
!@U            CALL UNDSCR(TOKEN )
            DO I=1,ISTASP
              CALL HOL2CHAR( STASUP(1,I), INT2(1), INT2(8), TEMP )
              IPOS=I
              IF(TOKEN.EQ.TEMP) GO TO 90
            ENDDO
            ISTASP=ISTASP+1
            IF(ISTASP.GT.MAX_STA)CALL FERR( INT2(9030), &
     &        'TOO MANY VELOCITIES '//TOKEN(1:10), INT2(0), INT2(0) )
            CALL CHAR2HOL( TOKEN, STASUP(1,ISTASP), INT2(1), INT2(8) )
!
!**** remove following lines because this initialization will now be
!     done elsewhere mwh 930830
!
!            CALL KSBIT(CMPSUP(1,1),(ISTASP),KBIT(DEFCMP,1))
!            CALL KSBIT(CMPSUP(1,2),(ISTASP),KBIT(DEFCMP,2))
!            CALL KSBIT(CMPSUP(1,3),(ISTASP),KBIT(DEFCMP,3))
!            CALL KSBIT(CMPSUP(1,4),(ISTASP),KBIT(DEFCMP,4))
            IPOS=ISTASP
90          CONTINUE
            CALL KSBIT( VELSUP(1,1), IPOS, l4tol2(VEL(1:1).EQ.'U') )
            CALL KSBIT( VELSUP(1,2), IPOS, l4tol2(VEL(2:2).EQ.'E') )
            CALL KSBIT( VELSUP(1,3), IPOS, l4tol2(VEL(3:3).EQ.'N') )
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
