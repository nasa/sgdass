      FUNCTION   MATCH_REGEXP ( STRING, PATTERN, IUER )
! ************************************************************************
! *                                                                      *
! *   Function  MATH_REGEXP  tries to match a STRING against regular     *
! *   expression PATTERN. If it succeeds then MATCH_REGEXP is .TRUE.,    *
! *   otherwise .FALSE.                                                  *
! *                                                                      *
! *   NB: trailng blanks are significant in call of MATCH_WILD.          *
! *                                                                      *
! * ________________________ Input parameters: _________________________ *
! *                                                                      *
! *   STRING ( CHARACTER ) -- the string to be examined.                 *
! *  PATTERN ( CHARACTER ) -- string which defined a basic regular       *
! *                           expression.                                *
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
! *    IUER ( INTEGER*4, OPT ) -- Universal error handler.               *
! *                           Input: switch IUER=0 -- no error messages  *
! *                                  will be generated even in the case  *
! *                                  of error. IUER=-1 -- in the case of *
! *                                  error the message will be put on    *
! *                                  stdout.                             *
! *                           Output: 0 in the case of successful        *
! *                                   completion and non-zero in the     *
! *                                   case of error.                     *
! *                                                                      *
! *                                                                      *
! *  ###  12-JUL-99   MATCH_REGEXP  v1.2  (c) L. Petrov 04-MAY-2004 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      LOGICAL*4  MATCH_REGEXP
      CHARACTER  STRING*(*), PATTERN*(*)
      CHARACTER  MESSAGE*256
      INTEGER*4  IUER
      INTEGER*4  IP
      INTEGER*4, EXTERNAL :: MATCH_REGEXP_C, LOC__SUN$$_STR
!
      CALL CLRCH ( MESSAGE )
!
! --- Call a C-routine which calls system subroutines regcomp, regexec
! --- and regerror
!
#ifdef SUN
      IP = MATCH_REGEXP_C ( %VAL(LOC__SUN$$_STR(STRING//CHAR(0))), &
     &                      %VAL(LOC__SUN$$_STR(PATTERN//CHAR(0))), &
     &                      %VAL(LOC__SUN$$_STR(MESSAGE)) )
#else
      IP = MATCH_REGEXP_C ( %REF(STRING//CHAR(0)), &
     &                      %REF(PATTERN//CHAR(0)), &
     &                      %REF(MESSAGE) )
#endif
!
! --- Analyse the condition code
!
      IF ( IP .EQ. 0 ) THEN
!
! -------- Matching!
!
           MATCH_REGEXP = .TRUE.
           CALL ERR_LOG ( 0, IUER )
         ELSE IF ( IP .EQ. -1 ) THEN
!
! -------- Not matching
!
           MATCH_REGEXP = .FALSE.
           CALL ERR_LOG ( 0, IUER )
         ELSE
!
! -------- Somewehere error occurred
!
           MATCH_REGEXP = .FALSE.
           CALL ERR_LOG ( 602, IUER, 'MATCH_REGEXP', MESSAGE )
      END IF
      RETURN
      END  !#!  MATCH_REGEXP  #!#
