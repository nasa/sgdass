      SUBROUTINE TESTF(NTAGS,FAIL)
      IMPLICIT    NONE
!
! 1.  TESTF PROGRAM SPECIFICATION
!
! 1.1 Determine whether the number of atmosphere epochs requested
!      exceeds the maximum allowable.
!
! 1.2 REFERENCES:
!
! 2.  TESTF INTERFACE
!
! 2.1 Parameter File
      INCLUDE 'solve.i'
!
! 2.2 INPUT Variables:
!
      INTEGER*2 NTAGS
!
! NTAGS - Number of atmosphere epochs requested
!
! 2.3 OUTPUT Variables:
!
      LOGICAL*2 FAIL
!
! FAIL - True if number of epochs exceeds maximum allowed
!
! 2.4 COMMON BLOCKS USED
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES:
!       CALLED SUBROUTINES:
!
! 3.  LOCAL VARIABLES
!
! 4.  HISTORY
!   WHO   WHEN   WHAT
!
! 5.  TESTF PROGRAM STRUCTURE
!
      FAIL=.FALSE.
      IF(NTAGS.GT.MAX_ATM) FAIL=.TRUE.
      RETURN
      END
