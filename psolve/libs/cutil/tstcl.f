      SUBROUTINE TSTCL(NEPOC,NOFST,FAIL)
      IMPLICIT NONE
!
! 1.  TSTCL PROGRAM SPECIFICATION
!
! 1.1 Check whether the number of clock epochs exceeds the maximum allowable.
!
! 1.2 REFERENCES:
!
! 2.  TSTCL INTERFACE
!
! 2.1 Parameter File
      INCLUDE 'solve.i'
!
! 2.2 INPUT Variables:
!
      INTEGER*2 NEPOC,NOFST
!
! NEPOC - Number of regular clock epochs
! NOFST - Number of offset epochs (clock breaks)
!
! 2.3 OUTPUT Variables:
!
      LOGICAL*2 FAIL
!
! FAIL - True if maximum has been exceeded
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
      INTEGER*2 SUM
!
!
! 4.  HISTORY
!   WHO   WHEN   WHAT
!
! 5.  TSTCL PROGRAM STRUCTURE
!
      FAIL=.FALSE.
      SUM=NEPOC+NOFST
      IF(SUM.GT.MAX_CLK) FAIL=.TRUE.
      RETURN
      END
