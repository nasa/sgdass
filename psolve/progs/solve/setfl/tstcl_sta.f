      SUBROUTINE TSTCL_STA(ISTA,NEPOC,FAIL)
      IMPLICIT NONE
!
! 1.  TSTCL_STA PROGRAM SPECIFICATION
!
! 1.1 Test for excessive number of clock parameters.
!
! 1.2 REFERENCES:
!
! 2.  TSTCL_STA INTERFACE
!
! 2.1 Parameter File
      INCLUDE 'solve.i'
!
! 2.2 INPUT Variables:
!
      INTEGER*2 NEPOC,ISTA
!
! ISTA - Site number of this station
! NEPOC - Number of clock epochs
!
! 2.3 OUTPUT Variables:
!
      LOGICAL*2 FAIL
!
! FAIL - Returns TRUE if maximum allowable number of clock parameters is
!        exceeded
!
! 2.4 COMMON BLOCKS USED
      INCLUDE 'socom.i'
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES: autoc
!       CALLED SUBROUTINES: None
!
! 3.  LOCAL VARIABLES
!
      INTEGER*2 I,SUM
!
! 4.  HISTORY
!   WHO   WHEN   WHAT
!
! 5.  TSTCL_STA PROGRAM STRUCTURE
!
      FAIL=.FALSE.
!
! Initialize SUM to number of epochs for the current station
!
      SUM=NEPOC
!
! Add in number of parameters for all other stations so far
!
      DO I=1,NUMSTA
          IF(I.NE.ISTA) &
     &        SUM=SUM+NUMCLK(I)
      ENDDO
!
! Check against maximum allowed
!
      IF(SUM.GT.MAX_CLK) FAIL=.TRUE.
      RETURN
      END
