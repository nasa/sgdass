      SUBROUTINE ATMEXC(TOKEN,STRING)
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
! 1.  ATMEXC PROGRAM SPECIFICATION
!
! 1.1 Handle atmosphere exceptions.
!
! 1.2 REFERENCES:
!
! 2.  ATMEXC INTERFACE
!
! 2.1 Parameter File
      INCLUDE 'solve.i'
!
! 2.2 INPUT Variables:
!
      CHARACTER*(*) STRING
!
! STRING - Sring containing exception(s)
!
! 2.3 OUTPUT Variables:
!
      CHARACTER*(*) TOKEN
!
! TOKEN - Individual token from exception line
!
! 2.4 COMMON BLOCKS USED
      INCLUDE 'batme.i'
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES: gflags
!       CALLED SUBROUTINES: utility routines
!
! 3.  LOCAL VARIABLES
!
      LOGICAL*2 CFEOF,KNO
      INTEGER*2 LENGTH,CFREAD,IDUM,J,IPOS,IERR
      CHARACTER*1 BSLASH
      DATA KNO/.FALSE./
      BSLASH = CHAR(92)
!
! 4.  HISTORY
!   WHO   WHEN  WHAT
!   JLR  921215   replaced '\' with BSLASH
!
! 5.  ATMEXC PROGRAM STRUCTURE
!
      NATMEX=0
!
! Get first token from input string; if not EXCEPT, then error
!
      CALL SPLITSTRING(STRING,TOKEN,STRING )
      IF(TOKEN.EQ.'EXCEPT') THEN
        CALL SPLITSTRING(STRING,TOKEN,STRING )
!
! Loop over all tokens in ATMOSPHERES entry
!
        DO WHILE (TOKEN.NE.' ')
          IF (TOKEN.EQ.BSLASH) THEN
!
! Handle continuation line by reading next line of control file
!
            LENGTH=CFREAD(STRING)
            IF(STRING(1:1).EQ.'$'.OR.CFEOF(IDUM)) THEN
              CALL FERR( INT2(9020), 'ILLEGAL CONTINUATION LINE '// &
     &             STRING(1:10), INT2(0), INT2(0) )
            ENDIF
!
! Make sure EXCEPT is followed by NO, and check for too many exceptions
!
          ELSE IF(TOKEN.EQ.'NO') THEN
            KNO=.TRUE.
          ELSE
            IF(.NOT.KNO) CALL FERR( INT2(9025), &
     &        'NO REQUIRED IN ATMOSPHERE EXCEPT CLAUSE', INT2(0), INT2(0) )
            IF(NATMEX.GE.MAX_STA) CALL FERR( INT2(9030), &
     &        'TOO MANY ATM EXCEPTIONS '//TOKEN(1:10), INT2(0), INT2(0) )
!
! increment number of exceptions and put info in exception list
!
!@U            CALL UNDSCR(TOKEN )
            NATMEX=NATMEX+1
            CALL CHAR2HOL( TOKEN, IATMEX(1,NATMEX), INT2(1), INT2(8) )
          ENDIF
!
! Get next token from input string
!
          CALL SPLITSTRING(STRING,TOKEN,STRING )
        ENDDO
      ELSE IF(TOKEN.NE.' ') THEN
        CALL FERR( INT2(9040), 'ILLEGAL ATMOSPHERE KEYWORD', INT2(0), INT2(0) )
      ENDIF
!
      RETURN
      END
