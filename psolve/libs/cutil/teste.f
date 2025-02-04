      SUBROUTINE TESTE(NEPOCS,ISITE,NUMATM,NUMSTA,FAIL)
      IMPLICIT    NONE
!
! 1.  TESTE PROGRAM SPECIFICATION
!
! 1.1  Check whether the required number of atmosphere parameters
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
      INTEGER*2   NEPOCS,NUMSTA,NUMATM(NUMSTA),ISITE
!
! ISITE - Site number of station currently being checked
! NEPOCS - Number of atmosphere epochs for current station
! NUMATM - Number of atmosphere epochs for each station
! NUMSTA - Number of stations
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
! 5.  TESTE PROGRAM STRUCTURE
!
      FAIL=.FALSE.
      SUM=0
      DO I=1,NUMSTA
          IF(I.NE.ISITE) SUM=SUM+NUMATM(I)
      ENDDO
      SUM=SUM+NEPOCS
      IF(SUM.GT.MAX_ATM) FAIL=.TRUE.
      RETURN
      END
