      FUNCTION KBITN(IARRAY,IBIT)
      IMPLICIT NONE
!
! 1.  KBITN PROGRAM SPECIFICATION
!
! 1.1 Test the specified bit of the specified array to see
!     whether it is set or not, by calling KBIT. This is the
!     integer (0 or 1) version of the logical function KBIT.
!
! 1.2 REFERENCES:
!
! 2.  KBITN INTERFACE
!
! 2.1 Parameter File
!
! 2.2 INPUT Variables:
!
      INTEGER*2 IARRAY(*),IBIT
!
! IARRAY - Variable in which the flag bits are located.
!          May or may not be an array in the calling program.
! IBIT - Index of bit to test. Bits are numbered starting with
!        1 as the lowest order bit in IARRAY(1), 16 is the
!        sign bit in IARRAY(1), 17 is the lowest order bit
!        in IARRAY(2), etc.
!
! 2.3 OUTPUT Variables:
!
      INTEGER*2 KBITN
!
! KBITN - 1 if the indicated bit is 1; 0 if bit is 0.
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
      LOGICAL*2 KBIT
!
! 4.  HISTORY
!   WHO   WHEN   WHAT
!
! 5.  KBITN PROGRAM STRUCTURE
!
      KBITN=0
      IF(KBIT(IARRAY,IBIT)) KBITN=1
      RETURN
!
      END
