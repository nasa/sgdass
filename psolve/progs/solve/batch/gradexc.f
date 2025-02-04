      SUBROUTINE GRADEXC ( TOKEN, STRING, GRINTRVL, GRADFLG )
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
! 1.  GRADEXC PROGRAM SPECIFICATION
!
! 1.1 Handle gradient exceptions.
!
! 1.2 REFERENCES:
!
! 2.  GRADEXC INTERFACE
!
! 2.1 Parameter File
      INCLUDE 'solve.i'
!
! 2.2 INPUT Variables:
!
      CHARACTER*(*) STRING,gradflg
!
! STRING - Sring containing exception(s)
!
! 2.3 OUTPUT Variables:
!
      CHARACTER*(*) TOKEN
      integer*2 grintrvl
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
      INTEGER*2 LENGTH,CFREAD,IDUM,J,IPOS,IERR,decimaltoint
      CHARACTER*1 BSLASH
      DATA KNO/.FALSE./
      BSLASH = CHAR(92)
!
! 4.  HISTORY
!   WHO   WHEN  WHAT
!   mwh  941219     Created (based on atmexc)
!   pet  2024.06.02 Fixed a big in YES EXCEPT logic
!
! 5.  GRADEXC PROGRAM STRUCTURE
!
      NGRADEX=0
!
! Get first token from input string; if not EXCEPT, then error
!
      CALL SPLITSTRING(STRING,TOKEN,STRING )
      IF ( TOKEN .EQ. 'EXCEPT' ) THEN
           CALL SPLITSTRING ( STRING, TOKEN, STRING )
           IF ( GRADFLG .EQ. 'Y' ) THEN
                IF ( TOKEN .NE. 'NO' ) THEN
                     CALL FERR( INT2(9010), 'EXCEPT must be followed by NO', INT2(0), &
     &                          INT2(0) )
                ENDIF
             ELSE
               IF ( TOKEN .NE. 'YES' ) THEN
                    CALL FERR ( INT2(9010), 'EXCEPT must be followed by YES', INT2(0), &
     &                          INT2(0) )
               ENDIF
           ENDIF
!
! -------- Loop over all tokens in GRADIENTS entry
!
           DO WHILE (TOKEN.NE.' ')
              IF ( TOKEN .EQ. BSLASH ) THEN
!
!  --------------- Handle continuation line by reading next line of control file
!
                   LENGTH=CFREAD(STRING)
                   IF ( STRING(1:1) .EQ. '$' .OR. CFEOF(IDUM) ) THEN
                        CALL FERR ( INT2(9020), 'Illegal continuation line '// &
     &                              STRING(1:10), INT2(0), INT2(0) )
                   ENDIF
                 ELSE
!
! ---------------- Check for too many exceptions
!
                   IF ( NGRADEX .GE. MAX_STA ) THEN
                        CALL FERR ( INT2(9030), 'Too many grad exceptions '//TOKEN(1:10), &
     &                              INT2(0), INT2(0) )
                   END IF
!
! ---------------- Increment number of exceptions and put info in exception list
!
                   NGRADEX=NGRADEX+1
                   CALL CHAR2HOL( TOKEN, IGRADEX(1,NGRADEX), INT2(1), INT2(8) )
              ENDIF
!
! ----------- Get next token from input string
!  
              CALL SPLITSTRING(STRING,TOKEN,STRING )
           END DO
        ELSE IF(TOKEN.NE.' ') THEN
           CALL FERR( INT2(9040), 'ILLEGAL GRADIENT KEYWORD: TOKEN='//TRIM(TOKEN), &
     &                INT2(0), INT2(0) )
      ENDIF
!
      RETURN
      END  SUBROUTINE GRADEXC  !#!#
