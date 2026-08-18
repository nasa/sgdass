      SUBROUTINE SWBIT(IARRAY,IBIT)
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
! 1.  SWBIT PROGRAM SPECIFICATION
!
! 1.1 Switch the appropriate bit of an array.
!
! 1.2 REFERENCES:
!
! 2.  SWBIT INTERFACE
!
! 2.1 Parameter File
!
! 2.2 INPUT Variables:
!
      INTEGER*2 IARRAY(*),IBIT
!
! IARRAY - The array
! IBIT - Which bit of the array to switch
!
! 2.3 OUTPUT Variables: None
!
! 2.4 COMMON BLOCKS USED
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES:
!       CALLED SUBROUTINES: sbit,kbitn
!
! 3.  LOCAL VARIABLES
!
      INTEGER*2 KBITN
!
! 4.  HISTORY
!   WHO   WHEN   WHAT
!
! 5.  SWBIT PROGRAM STRUCTURE
!
      CALL SBIT( IARRAY, IBIT, INT2(1-KBITN(IARRAY,IBIT)) )
      RETURN
!
      END
