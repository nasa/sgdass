      INTEGER*2 FUNCTION KFLIP(IWD,IBIT)
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
! 1.  KFLIP PROGRAM SPECIFICATION
!
! 1.1 Flip a specified bit in an integer array and return
!     the integer value of the new bit setting (0 or 1).
!
! 1.2 REFERENCES:
!
! 2.  KFLIP INTERFACE
!
! 2.1 Parameter File
!
! 2.2 INPUT Variables:
!
      INTEGER*2 IWD(*),IBIT
!
! IBIT - Which bit to flip in IWD
! IWD - Integer array to be modified
!
! 2.3 OUTPUT Variables:
!
! KFLIP - Returns new value of flipped bit
!
! 2.4 COMMON BLOCKS USED
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES:
!       CALLED SUBROUTINES: kbitn,sbit
!
! 3.  LOCAL VARIABLES
!
      INTEGER*2 KBITN
!
! 4.  HISTORY
!   WHO   WHEN   WHAT
!
! 5.  KFLIP PROGRAM STRUCTURE
!
      KFLIP = 1 - KBITN(IWD,IBIT)
      CALL SBIT( IWD, IBIT, INT2(KFLIP) )
!
      RETURN
      END
