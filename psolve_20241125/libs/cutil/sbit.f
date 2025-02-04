      SUBROUTINE SBIT ( IARRAY, IBIT, IVALUE )
      IMPLICIT NONE
#ifdef LAHEY
ML_EXTERNAL memcpy
#endif
!
! 1.  SBIT PROGRAM SPECIFICATION
!
! 1.1 Set the specified bit of an array to the value (0 or 1)
!     specified.  Logical function KBIT can then test the bit
!     (TRUE if bit is 1).
!
! 1.2 REFERENCES:
!
! 2.  SBIT INTERFACE
!
! 2.1 Parameter File
!
! 2.2 INPUT Variables:
!
      INTEGER*2 IARRAY(*),IBIT,IVALUE
!
! IARRAY - The array containing the bit to be set (may or may not
!          really be dimensioned in calling program)
! IBIT - Index of bit to be set or reset.  Bit 1 is the lowest order
!        bit of IARRAY(1), bit 17 is the lowest order bit of IARRAY(2),
!        etc.  This is the same indexing scheme employed in function KBIT.
! IVALUE - Value to which bit is to be set (0 or 1). If IVALUE is omitted
!          or nonzero, 1 is assumed.
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
      INTEGER*2 IA,IB,IVAL
!
! IA - Array index for call to IBSET or IBCLR
! IB - Bit index for call to IBSET or IBCLR
! IVAL - Local copy of value to which bit is to be set
!
! 4.  HISTORY
!   WHO   WHEN        WHAT
!   pet   2002.10.24  Added trap of initernal control: new version checks
!                     validity of the second argument
!
! 5.  SBIT PROGRAM STRUCTURE
!
      IVAL=0
      IF ( IVALUE .NE. 0 ) IVAL = 1
      IF ( IBIT .LE. 0 ) THEN
           WRITE ( 6, * ) ' Wrong second argument of SBIT: ',IBIT
!
! -------- This operator should cause abnormal termination of the program
! -------- (copy something in address 0) and the system should unwrap
! -------- the stack of calls. Hope, it will help for diagnostic.
!
           CALL MEMCPY ( %VAL(0), 1, %VAL(2) )
           STOP 'Wrong second argument of SBIT'
      END IF
!
! --- Decompose IBIT into an array index IA and a bit index IB.
!
      IA = (IBIT+15)/16
      IB = IBIT - (IA-1)*16
!
! --- IF IVAL is 1, force the appropriate bit to 1.
!
      IF ( IVAL.EQ.1)  THEN
           IARRAY(IA) = IBSET ( IARRAY(IA), IB-1 )
         ELSE
           IARRAY(IA) = IBCLR ( IARRAY(IA), IB-1 )
      ENDIF
!
      RETURN
      END  !#!  SBIT  #!#
