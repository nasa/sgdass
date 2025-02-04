      SUBROUTINE TESTG(NEPOCS,FAIL)
      IMPLICIT    NONE
!
! 1.  TESTG PROGRAM SPECIFICATION
!
! 1.1  Check whether the required number of gradient parameters
!      exceeds the allowable limit.
!
! 1.2 REFERENCES:
!
! 2.  INTERFACE
!
! 2.1 Parameter File
      INCLUDE 'solve.i'
!
! 2.2 INPUT Variables:
!
      INTEGER*2   NEPOCS
!
! NEPOCS - Number of gradient epochs for current station
!
! 2.3 OUTPUT Variables:
!
      LOGICAL*2 FAIL
!
! FAIL - True if maximum allowable number of atmosphere parameters is exceeded
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
      INTEGER*2   SUM,I
!
! I - Loop index
!
! 4.  HISTORY
!   WHO   WHEN   WHAT
!
! 5.  TESTG PROGRAM STRUCTURE
!
      FAIL=.FALSE.
      IF(nepocs.GT.MAX_GRAD) FAIL=.TRUE.
      RETURN
      END
