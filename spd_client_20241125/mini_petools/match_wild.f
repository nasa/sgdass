      FUNCTION   MATCH_WILD ( STRING, PATTERN )
! ************************************************************************
! *                                                                      *
! *   Function  MATCH_WILD  tries to match a STRING against a pattern    *
! *   PATTERN. Pattern may contains wildcard symbols like * ? and others *
! *   used for filenames matching. If the examined pattern matches to    *
! *   a pattern PATTERN then MATCH_WILD is .TRUE., otherwise .FALSE.     *
! *                                                                      *
! *   Function MATCH provide a convenient interface to a system          *
! *   subroutine fnmatch.                                                *
! *                                                                      *
! *   NB: trailing blanks are significant in call of MATCH_WILD.         *
! *                                                                      *
! * ________________________ Input parameters: _________________________ *
! *                                                                      *
! *   STRING ( CHARACTER ) -- the string to be examined.                 *
! *  PATTERN ( CHARACTER ) -- string which defined a pattern which may   *
! *                           contain wildcard symbols.                  *
! *                                                                      *
! * ________________________Output parameters: _________________________ *
! *                                                                      *
! * <MATCH_WILD> ( LOGICAL*4 ) -- .TRUE. if the pattern and the string   *
! *                               matches each other.                    *
! *                                                                      *
! *  ###  12-JUL-99    MATCH_WILD   v1.0  (c) L. Petrov  12-JUL-99  ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      LOGICAL*4  MATCH_WILD
      CHARACTER  STRING*(*), PATTERN*(*)
      INTEGER*4  IP
      INTEGER*4, EXTERNAL :: FNMATCH, LOC__SUN$$_STR
!
! --- Call a system routine FNMATCH
! --- and regerror
!
#ifdef SUN
      IP = FNMATCH ( %VAL(LOC__SUN$$_STR(PATTERN//CHAR(0))), &
     &               %VAL(LOC__SUN$$_STR(STRING//CHAR(0))),  %VAL(0) )
#else
      IP = FNMATCH ( %REF(PATTERN//CHAR(0)), %REF(STRING//CHAR(0)), %VAL(0) )
#endif
!
      IF ( IP .EQ. 0 ) THEN
!
! -------- Matching!
!
           MATCH_WILD = .TRUE.
         ELSE
!
! -------- Not matching
!
           MATCH_WILD = .FALSE.
      END IF
      RETURN
      END  !#!  MATCH_WILD  #!#
