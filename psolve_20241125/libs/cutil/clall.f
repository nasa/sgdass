      SUBROUTINE CLALL()
      IMPLICIT    NONE
!
! 1.  CLALL PROGRAM SPECIFICATION
!
! 1.1 Clear out atmosphere flags for all stations
!
! 1.2 REFERENCES:
!
! 2.  CLALL INTERFACE
!
! 2.1 Parameter File
      INCLUDE 'solve.i'
!
! 2.2 INPUT Variables: None
!
! 2.3 OUTPUT Variables: None
!
! 2.4 COMMON BLOCKS USED
      INCLUDE 'socom.i'
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES:
!       CALLED SUBROUTINES:
!
! 3.  LOCAL VARIABLES
!
      INTEGER*2   I
!
! I - Loop index
!
! 4.  HISTORY
!   WHO   WHEN   WHAT
!
! 5.  CLALL PROGRAM STRUCTURE
!
      DO I=1,NUMSTA
          IATSTR(I)=0
      ENDDO
      RETURN
      END
