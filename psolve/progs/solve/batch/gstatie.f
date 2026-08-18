      SUBROUTINE GSTATIE(numgrp,staties,stasup,istasp,token,string, &
     &   defvel,velsup,defcmp,cmpsup)
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
! 1.  GSTATIE PROGRAM SPECIFICATION
!
! 1.1 Parse STATION_TIE line of $SUPPRESSION section.
!
! 1.2 REFERENCES:
!
! 2.  GSTATIE INTERFACE
!
! 2.1 Parameter File
      INCLUDE 'solve.i'
!
! 2.2 INPUT Variables:
!
      CHARACTER*(*) string
      integer*2 defvel(2),defcmp
      integer*2 velsup(STA_BIT_WORDS,*),cmpsup(STA_BIT_WORDS,*)
!
! STRING - Remainder of STATION_TIE line
!
! 2.3 OUTPUT Variables:
!
      INTEGER*2 numgrp,staties(*),stasup(4,MAX_STA),istasp
      CHARACTER*(*) TOKEN
!
! ISTASP - Number of stations in suppression list
! NUMGRP - Number of distinct groups of position-tied stations
! STASUP - List of station names for suppression
! TOKEN - Single token from STRING
! STATIES - Group number to which each station belongs (0 = no group)
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
      CHARACTER*8 TEMP
      INTEGER*2 I,LENGTH,CFREAD,IDUM,J,IPOS,KBITN,IVAL
      CHARACTER*1 BSLASH
      BSLASH = CHAR(92)
!
! 4.  HISTORY
!   WHO   WHEN   WHAT
!   JLR  921215  replaced '\' with BSLASH
!   mwh  930521  created, based on gveltie
!   kdb  961125  declare cmpsup,velsup with *
!   pet  2000.05.10  Added support of the qualifiers YES and NO
!
! 5.  GSTATIE PROGRAM STRUCTURE
!
      CALL SPLITSTRING ( STRING, TOKEN, STRING )
      IF ( TOKEN .EQ. 'NO' ) THEN
           NUMGRP = 0
           RETURN
        ELSE IF ( TOKEN .EQ. 'YES' ) THEN
           CALL SPLITSTRING ( STRING, TOKEN, STRING )
      END IF
!
      NUMGRP=1
      DO WHILE (TOKEN.NE.' ')
        IF(TOKEN.EQ.'AND') THEN
          NUMGRP = NUMGRP+1
          CALL SPLITSTRING(STRING,TOKEN,STRING )
        else
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
            IF(ISTASP.GT.MAX_STA)CALL FERR( INT2(9030), 'TOO MANY STATIONS '// &
     &         TOKEN(1:10), INT2(0), INT2(0) )
            CALL CHAR2HOL( TOKEN, STASUP(1,ISTASP), INT2(1), INT2(8) )
!
!*****  remove following lines because this initialization will now be
!        done elsewhere   mwh  930830
!
!            CALL KSBIT(cmpSUP(1,1),(ISTASP),KBIT(DEFcmp,1))
!            CALL KSBIT(cmpSUP(1,2),(ISTASP),KBIT(DEFcmp,2))
!            CALL KSBIT(cmpSUP(1,3),(ISTASP),KBIT(DEFcmp,3))
!            CALL KSBIT(cmpSUP(1,4),(ISTASP),KBIT(DEFcmp,4))
!            CALL KSBIT(velSUP(1,1),(ISTASP),KBIT(DEFvel,1))
!            CALL KSBIT(velSUP(1,2),(ISTASP),KBIT(DEFvel,2))
!            CALL KSBIT(velSUP(1,3),(ISTASP),KBIT(DEFvel,3))
!            CALL KSBIT(velSUP(1,4),(ISTASP),KBIT(DEFvel,4))
            IPOS=ISTASP
90          CONTINUE
            staties(ipos)=numgrp
          ENDIF
          CALL SPLITSTRING(STRING,TOKEN,STRING )
        endif
      ENDDO
!
      RETURN
      END
