      SUBROUTINE ISWCH(LFLG,MASK,IBIT,IDSP)
      implicit none
!
! 1.  ISWCH PROGRAM SPECIFICATION
!
! 1.1  Switch the specified bit in the flag variable LFLG.
!
! 1.2 REFERENCES:
!
! 2.  ISWCH INTERFACE
!
! 2.1 Parameter File
!
! 2.2 INPUT Variables:
!
      INTEGER*2 MASK(16),LFLG,IBIT,IDSP
!
! IBIT - Number of the bit to be switched
! IDSP - Output value of the bit being switched
! LFLG - Flag variable being modified
! MASK - Mask for the specified bit
!
! 2.3 OUTPUT Variables: None
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
! 5.  ISWCH PROGRAM STRUCTURE
      IDSP = 1
!
! If bit isn't already set, add the mask
!
      IF(IAND(LFLG,MASK(IBIT)).NE.0) GO TO 100
      LFLG = LFLG + MASK(IBIT)
      GO TO 200
!
! If bit is already set, subtract the mask
!
  100 CONTINUE
      LFLG = LFLG - MASK(IBIT)
      IDSP = 0
  200 CONTINUE
!
      RETURN
      END
