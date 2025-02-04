      SUBROUTINE GTELEV(TOKEN,STRING)
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
! 1.  GTELEV PROGRAM SPECIFICATION
!
! 1.1 Parse ELEVATION cutoff line of DATA section of the control file.
!
! 1.2 REFERENCES:
!
! 2.  GTELEV INTERFACE
!
! 2.1 Parameter File
      INCLUDE 'solve.i'
!
! 2.2 INPUT Variables:
!
      CHARACTER*(*) STRING
!
! STRING - String to be parsed
!
! 2.3 OUTPUT Variables:
!
      CHARACTER*(*) TOKEN
!
! TOKEN - Individual token from STRING
!
! 2.4 COMMON BLOCKS USED
      INCLUDE 'belev.i'
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES: gdata
!       CALLED SUBROUTINES: utility routines
!
! 3.  LOCAL VARIABLES
!
      LOGICAL*2 CFEOF,KEL
      REAL*8 REAL8_DECODE,TEMP,EL
      INTEGER*2 LENGTH,CFREAD,IDUM,J,IPOS,IERR
      CHARACTER*1 BSLASH
      BSLASH = CHAR(92)
!
! 4.  HISTORY
!   WHO   WHEN   WHAT
!   JLR   921215   replaced '\' with BSLASH
!
! 5.  GTELEV PROGRAM STRUCTURE
!
! Get first token (new default cutoff) and make sure it's a number
!
      CALL SPLITSTRING(STRING,TOKEN,STRING )
!
      ELDEF=REAL8_DECODE(TOKEN,IERR)
      IF(IERR.NE.0) THEN
        CALL FERR( INT2(9010), 'ELEVATION MUST BE A NUMBER '//TOKEN(1:16), &
     &       INT2(0), INT2(0) )
      ENDIF
!
      NEL=0
!
! Get next token from input string, and process if EXCEPT
!
      CALL SPLITSTRING(STRING,TOKEN,STRING )
      IF(TOKEN.EQ.'EXCEPT') THEN
!
! Get next token (cutoff for first exception list)
!
        CALL SPLITSTRING(STRING,TOKEN,STRING )
        KEL=.FALSE.
!
! Loop over input tokens
!
        DO WHILE (TOKEN.NE.' ')
          TEMP=REAL8_DECODE(TOKEN,IERR)
          IF(IERR.EQ.0) THEN
            EL=TEMP
            KEL=.TRUE.
          ELSE IF(TOKEN.EQ.BSLASH) THEN
!
! Handle continuation line by reading next record
!
            LENGTH=CFREAD(STRING)
            IF(STRING(1:1).EQ.'$'.OR.CFEOF(IDUM)) THEN
              CALL FERR( INT2(9020), 'ILLEGAL CONTINUATION LINE '// &
     &             STRING(1:10), INT2(0), INT2(0) )
            ENDIF
          ELSE
            IF(.NOT.KEL) CALL FERR( INT2(9025), 'FIRST EXCEPTION LIST '// &
     &        'ENTRY MUST SPECIFY NEW ELEVATION', INT2(0), INT2(0) )
            IF(NEL.GE.MAX_STA) CALL FERR( INT2(9030), 'TOO MANY ELEVATIONS '// &
     &         TOKEN(1:10), INT2(0), INT2(0) )
!@U            CALL UNDSCR(TOKEN )
            NEL=NEL+1
            CALL CHAR2HOL( TOKEN, IELNM(1,NEL), INT2(1), INT2(8) )
            ELC(NEL)=EL
          ENDIF
!
! get next token
!
          CALL SPLITSTRING(STRING,TOKEN,STRING )
        ENDDO
      ENDIF
!
      RETURN
      END
