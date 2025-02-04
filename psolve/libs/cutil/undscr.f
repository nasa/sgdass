      SUBROUTINE UNDSCR(TOKEN)
      IMPLICIT NONE
!
! 1.  UNDSCR PROGRAM SPECIFICATION
!
! 1.1 Replace all underscores in the input string with blanks.
!
! 1.2 REFERENCES:
!
! 2.  UNDSCR INTERFACE
!
! 2.1 Parameter File
!
! 2.2 INPUT Variables:
!
      CHARACTER*(*) TOKEN
!
! TOKEN - String to be checked/modified
!
! 2.3 OUTPUT Variables: None
!
! 2.4 COMMON BLOCKS USED
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES: Numerous routines
!       CALLED SUBROUTINES: None
!
! 3.  LOCAL VARIABLES
!
      INTEGER*2 I,TRIMLEN,L1
!
! 4.  HISTORY
!   WHO   WHEN   WHAT
!
! 5.  UNDSCR PROGRAM STRUCTURE
!
      L1=TRIMLEN(TOKEN)
      DO I=1,L1
        IF(TOKEN(I:I).EQ.'_') TOKEN(I:I)=' '
      ENDDO
!
      RETURN
      END
