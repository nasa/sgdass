      SUBROUTINE SBIT4(IARRAY,IBIT,IVALUE)
      IMPLICIT NONE
!
! 1.  SBIT4 PROGRAM SPECIFICATION
!
! 1.1 Set the specified bit of an array to the value (0 or 1)
!     specified.  Logical function KBIT4 can then test the bit
!     (TRUE if bit is 1).
!     Theoretically allows integer*4 - 15 (2,147,483,632)  number of bits.
!     (The 15 bit restriction comes from the algorithm for calculating IA.
!     A temporary variable larger than integer*4 is required.)
!     In reality, this version of fortran only handled a 41 million element
!     integer*2 iarray in testing.  A 42 million element array caused a bus
!     error, and a 43 million element array caused a segmentation violation.
!
! 1.2 REFERENCES:
!
! 2.  SBIT4 INTERFACE
!
! 2.1 Parameter File
!
! 2.2 INPUT Variables:
!
      INTEGER*2 IARRAY(*),IVALUE
      INTEGER*4 IBIT
!
! IARRAY - The array containing the bit to be set (may or may not
!          really be dimensioned in calling program)
! IBIT - Index of bit to be set or reset.  Bit 1 is the lowest order
!        bit of IARRAY(1), bit 17 is the lowest order bit of IARRAY(2),
!        etc.  This is the same indexing scheme employed in function KBIT4.
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
      INTEGER*4 IA
      INTEGER*2 IB,IVAL
!
! IA - Array index for call to IBSET or IBCLR
! IB - Bit index for call to IBSET or IBCLR
! IVAL - Local copy of value to which bit is to be set
!
! 4.  HISTORY
!   WHO   WHEN   WHAT
!
!   kdb  951208  Created.
!
! 5.  SBIT4 PROGRAM STRUCTURE
!
      IVAL=0
      IF(IVALUE.NE.0) IVAL = 1
!
!    Decompose IBIT into an array index IA and a bit index IB.
!
      IA = (IBIT+15)/16
      IB = IBIT - (IA-1)*16
!
!    IF IVAL is 1, force the appropriate bit to 1.
!
      IF (IVAL.EQ.1)  THEN
         IARRAY(IA)=IBSET(IARRAY(IA),IB-1)
      ELSE
         IARRAY(IA)=IBCLR(IARRAY(IA),IB-1)
      ENDIF
      RETURN
!
      END
