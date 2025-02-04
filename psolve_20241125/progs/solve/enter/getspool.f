      SUBROUTINE GETSPOOL(IPRNX)
      IMPLICIT NONE
!
! 1.  GETSPOOL PROGRAM SPECIFICATION
!
! 1.1 Get spooling state from GLBCM.
!
! 1.2 REFERENCES:
!
! 2.  GETSPOOL INTERFACE
!
! 2.1 Parameter File
      INCLUDE 'solve.i'
!
! 2.2 INPUT Variables: None
!
! 2.3 OUTPUT Variables:
!
      INTEGER*2 IPRNX
!
! IPRNX - Unit number for spooling (23 if spooling is on)
!
! 2.4 COMMON BLOCKS USED
      INCLUDE 'glbcm.i'
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES: enter
!       CALLED SUBROUTINES: use_glbfil
!
! 3.  LOCAL VARIABLES
!
! 4.  HISTORY
!   WHO   WHEN   WHAT
!
! 5.  GETSPOOL PROGRAM STRUCTURE
!
      CALL USE_GLBFIL('ORC')
      IPRNX=IPRNT
      RETURN
      END
