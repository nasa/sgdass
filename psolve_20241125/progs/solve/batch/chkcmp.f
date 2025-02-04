      LOGICAL*2 FUNCTION CHKCMP(TOKEN,STACMP)
      IMPLICIT NONE
!
! 1.  CHKCMP PROGRAM SPECIFICATION
!
! 1.1 Check the length and content of a station component string.
!
! 1.2 REFERENCES:
!
! 2.  CHKCMP INTERFACE
!
! 2.1 Parameter File
!
! 2.2 INPUT Variables:
!
      CHARACTER*(*) TOKEN,STACMP
!
! STACMP - Station component flag to check against
! TOKEN - String being tested
!
! 2.3 OUTPUT Variables:
!
! CHKCMP - True if TOKEN matches properly
!
! 2.4 COMMON BLOCKS USED
!
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES: gsrcsp,gstasp,gvelsp
!       CALLED SUBROUTINES: None
!
! 3.  LOCAL VARIABLES
!
      INTEGER*2 J,TRIMLEN
!
! 4.  HISTORY
!   WHO   WHEN   WHAT
!
! 5.  CHKCMP PROGRAM STRUCTURE
!
      CHKCMP=.TRUE.
!
! Make sure each character of string matches the corresponding character
!  of the station component flag, or is '-'
!
      DO J=1,TRIMLEN(STACMP)
        CHKCMP=CHKCMP.AND.TOKEN(J:J).EQ.STACMP(J:J).OR.TOKEN(J:J).EQ.'-'
      ENDDO
!
! check length of the component string
!
      CHKCMP=CHKCMP.AND.TRIMLEN(TOKEN).EQ.TRIMLEN(STACMP)
!
      RETURN
      END
