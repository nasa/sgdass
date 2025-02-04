      SUBROUTINE GTWVM(TOKEN,STRING)
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
! 1.  GTWVM PROGRAM SPECIFICATION
!
! 1.1 Get wvr mask info from DATA section of the control file.
!
! 1.2 REFERENCES:
!
! 2.  GTWVM INTERFACE
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
! TOKEN - Individual token pulled from STRING
!
! 2.4 COMMON BLOCKS USED
      INCLUDE 'bwvrm.i'
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES: gdata
!       CALLED SUBROUTINES: utility routines
!
! 3.  LOCAL VARIABLES
!
      LOGICAL*2 CFEOF,KMASK
      INTEGER*2 OCTALTOINT,TEMP,MASK
      INTEGER*2 LENGTH,CFREAD,IDUM,J,IPOS,IERR
      CHARACTER*1 BSLASH
      BSLASH = CHAR(92)
!
! 4.  HISTORY
!   WHO   WHEN   WHAT
!   JLR   921215  replaced '\' with BSLASH
!   pet   2000.05.08  added support of a qualifier NO
!
! 5.  GTWVM PROGRAM STRUCTURE
!
! Get first token (default mask) from STRING; make sure it's a number
!
      CALL SPLITSTRING ( STRING, TOKEN, STRING )
      IF ( TOKEN .EQ. 'NO'  .OR.  TOKEN .EQ. 'no' ) THEN
!
! -------- Nothing to do
!
           RETURN
      END IF
!
      WVMDEF=OCTALTOINT(TOKEN,IERR)
      IF(IERR.NE.0) THEN
        CALL FERR( INT2(9010), 'ERROR IN MASK '//TOKEN(1:16), INT2(0), &
     &       INT2(0) )
      ENDIF
!
! Get next token from STRING; if not EXCEPT, we're finished
!
      NWVM=0
      CALL SPLITSTRING(STRING,TOKEN,STRING )
      IF(TOKEN.EQ.'EXCEPT') THEN
!
! Get first exception list's mask
!
        CALL SPLITSTRING(STRING,TOKEN,STRING )
        KMASK=.FALSE.
!
! Loop over all tokens in WVR_MASK entry
!
        DO WHILE (TOKEN.NE.' ')
          TEMP=OCTALTOINT(TOKEN,IERR)
          IF(IERR.EQ.0) THEN
            MASK=TEMP
            KMASK=.TRUE.
          ELSE IF(TOKEN.EQ.BSLASH) THEN
!
! Handle continuation line by reading next line of control file
!
            LENGTH=CFREAD(STRING)
            IF(STRING(1:1).EQ.'$'.OR.CFEOF(IDUM)) THEN
              CALL FERR( INT2(9020), 'ILLEGAL CONTINUATION LINE '// &
     &             STRING(1:10), INT2(0), INT2(0) )
            ENDIF
          ELSE
!
! Check for too many masks or missing mask for first exception list
!
            IF(.NOT.KMASK) CALL FERR( INT2(9025), 'FIRST EXCEPTION LIST '// &
     &        'ENTRY MUST SPECIFY NEW MASK', INT2(0), INT2(0) )
            IF(NWVM.GE.MAX_STA) CALL FERR( INT2(9030), 'TOO MANY MASKS '// &
     &         TOKEN(1:10), INT2(0), INT2(0) )
!@U            CALL UNDSCR(TOKEN )
!
! Increment number of masks and store this one in the appropriate arrays
!
            NWVM=NWVM+1
            CALL CHAR2HOL( TOKEN, IWVMNM(1,NWVM), INT2(1), INT2(8) )
            WVM(NWVM)=MASK
          ENDIF
!
! Get next token from STRING
!
          CALL SPLITSTRING(STRING,TOKEN,STRING )
        ENDDO
      ENDIF
!
      RETURN
      END
