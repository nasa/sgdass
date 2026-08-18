      FUNCTION KBIT ( IARRAY, IBIT )
      IMPLICIT NONE
!
! 1.  KBIT PROGRAM SPECIFICATION
!
! 1.1 Test the specified bit of the specified array to see
!     whether it is set or not.  designed to complement SBIT,
!     which sets or resets bits identified in the same way.
!
! 1.2 REFERENCES:
!
! 2.  KBIT INTERFACE
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
      LOGICAL*2 KBIT,k
      LOGICAL*2 HTEST
!
! KBIT - TRUE if the indicated bit is 1; FALSE if bit is 0.
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
      INTEGER*2 IA,IB,IC
!
! IA - Array index (which word in array)
! IB - Bit index (which bit in word)
!
! 4.  HISTORY
!   WHO   WHEN   WHAT
!   :16:10:2002: JWR  Introduced IC to eliminate difficulty in converting
!                     the '-i2' version
!   :21:10:2003: KDB  Convert htest to btest 
!   :27.05.2004: jwr  Test on IBIT = 0 added. 
!
! 5.  KBIT PROGRAM STRUCTURE
!
      IF ( IBIT .EQ. 0 ) THEN
           KBIT = .FALSE.
           RETURN
      ENDIF
!
! --- Decompose IBIT into an array index IA and a bit index IB.
!
      IA = (IBIT+15)/16
      IB = IBIT - (IA-1)*16
!
! --- Test the appropriate bit
!
      IC = IB-1
      KBIT = BTEST ( IARRAY(IA), IC )
!
      RETURN
      END  !#!  KBIT   #!#
