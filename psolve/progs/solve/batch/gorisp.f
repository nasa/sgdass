      SUBROUTINE GORISP(DEFVEL,VELSUP,STASUP,ISTASP,DEFCMP,CMPSUP, &
     &                  TOKEN,STRING)
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
      LOGICAL*2 L4TOL2
! 1.  GORISP PROGRAM SPECIFICATION
!
! 1.1 Parse STATION_ORIGIN line of $SUPPRESSION section.
!
! 1.2 REFERENCES:
!
! 2.  GORISP INTERFACE
!
! 2.1 Parameter File
      INCLUDE 'solve.i'
!
! 2.2 INPUT Variables:
!
      INTEGER*2 DEFVEL(2),STASUP(4,MAX_STA)
      INTEGER*2 ISTASP,DEFCMP,CMPSUP(STA_BIT_WORDS,*)
      CHARACTER*(*) STRING
!
! DEFVEL - Default station componenets for velocities
! DEFCMP - Default station componenets for positions
! ISTASP - Number of stations in the exception list
! STASUP - Station components for stations in exception list
! STRING - Remainder of STATION_ORIGIN line
!
! 2.3 OUTPUT Variables:
!
      INTEGER*2 VELSUP(STA_BIT_WORDS,*)
      CHARACTER*(*) TOKEN
!
! CMPSUP - Station suppression flag
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
      CHARACTER*1 CMP
      CHARACTER   TEMP*8, FIRST_TOKEN*32
      INTEGER*2 I,LENGTH,CFREAD,IDUM,J,IPOS,KBITN,IVAL
      CHARACTER*1 BSLASH
      BSLASH = CHAR(92)
!
! 4.  HISTORY
!   WHO   WHEN   WHAT
!   JLR   921215   replace '\' with BSLASH
!   KDB   961125   declare cmpsup,velsup with *.  (Also fixes error where velsup
!                  was declared (,3),  not (,4)).
!
! 5.  GORISP PROGRAM STRUCTURE
!
      CALL SPLITSTRING(STRING,TOKEN,STRING )
      FIRST_TOKEN = TOKEN
      IF ( TOKEN .EQ. 'NO' ) THEN
           CONTINUE
         ELSE IF ( TOKEN .EQ. 'YES' ) THEN
           CALL SBIT( DEFCMP, INT2(4), INT2(1) )
         ELSE
           CALL FERR ( INT2(9010), 'ILLEGAL ORIGIN PARAMETER '//TOKEN(1:16), &
     &          INT2(0), INT2(0) )
      ENDIF
!
      CALL SPLITSTRING ( STRING, TOKEN, STRING )
      IF ( FIRST_TOKEN .EQ. 'NO' .AND. TOKEN(1:1) .EQ. ' ' ) THEN
           RETURN
      END IF
!
      IVAL=KBITN( DEFCMP, INT2(4) )
!
!**** Initialize entire cmpsup, not just first istasp elements  mwh  930830
!
!      DO I=1,ISTASP
      DO I=1,MAX_STA
        CALL SBIT(CMPSUP(1,4),I,IVAL )
      ENDDO
!
      IF(TOKEN.EQ.'EXCEPT') THEN
        IF(KBIT( DEFCMP, INT2(4) )) THEN
          CMP='N'
        ELSE
          CMP='Y'
        ENDIF
        CALL SPLITSTRING(STRING,TOKEN,STRING )
        DO WHILE (TOKEN.NE.' ')
          IF(TOKEN.EQ.BSLASH) THEN
            LENGTH=CFREAD(STRING)
            IF(STRING(1:1).EQ.'$'.OR.CFEOF(IDUM))CALL FERR( INT2(9020), &
     &        'ILLEGAL CONTINUATION LINE '//STRING(1:10), INT2(0), INT2(0) )
          ELSE
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
!***** remove following lines because this initialization will now be done
!       elsewhere   mwh  930830
!            CALL KSBIT(VELSUP(1,1),(ISTASP),KBIT(DEFVEL,1))
!            CALL KSBIT(VELSUP(1,2),(ISTASP),KBIT(DEFVEL,2))
!            CALL KSBIT(VELSUP(1,3),(ISTASP),KBIT(DEFVEL,3))
!            CALL KSBIT(CMPSUP(1,1),(ISTASP),KBIT(DEFCMP,1))
!            CALL KSBIT(CMPSUP(1,2),(ISTASP),KBIT(DEFCMP,2))
!            CALL KSBIT(CMPSUP(1,3),(ISTASP),KBIT(DEFCMP,3))
            IPOS=ISTASP
90          CONTINUE
            CALL KSBIT( CMPSUP(1,4), IPOS, l4tol2(CMP(1:1).EQ.'Y') )
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
