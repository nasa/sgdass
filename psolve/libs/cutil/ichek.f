      FUNCTION ICHEK(L1,L2)
      implicit none
!
! 1.  ICHEK PROGRAM SPECIFICATION
!
! 1.1 Check to see whether two four-word strings match.
!
! 1.2 REFERENCES:
!
! 2.  ICHEK INTERFACE
!
! 2.1 Parameter File
!
! 2.2 INPUT Variables:
!
      INTEGER*2 L1(4),L2(4)
!
! L1,L2 - Two strings to be compared
!
! 2.3 OUTPUT Variables:
!
      INTEGER*2 ICHEK
!
! ICHEK - Returns 0 if no match, 1 if good match
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
      INTEGER*2 I
!
! I - Loop index
!
! 4.  HISTORY
!   WHO   WHEN   WHAT
!
! 5.  ICHEK PROGRAM STRUCTURE
!
      ICHEK = 1
      DO I=1,4
        IF(L1(I).NE.L2(I)) ICHEK = 0
      ENDDO
      RETURN
      END
