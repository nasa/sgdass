      SUBROUTINE KSBIT(IARR,I,LOGVAL)
      IMPLICIT NONE
!
! 1.  KSBIT PROGRAM SPECIFICATION
!
! 1.1 Set a bit in an array according to a specified logical value.
!
! 1.2 REFERENCES:
!
! 2.  KSBIT INTERFACE
!
! 2.1 Parameter File
!
! 2.2 INPUT Variables:
!
      INTEGER*2 I,IARR(*)
      LOGICAL*2 LOGVAL
!
! I - Which bit to set
! IARR - The array
! LOGVAL - Logical value incicating how to set bit (TRUE=1; FALSE=0)
!
! 2.3 OUTPUT Variables: None
!
! 2.4 COMMON BLOCKS USED
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES:
!       CALLED SUBROUTINES: sbit
!
! 3.  LOCAL VARIABLES
!
      INTEGER*2 IVALUE
!
! IVALUE - Value to be given to the specified bit
!
! 4.  HISTORY
!   WHO   WHEN   WHAT
!
! 5.  KSBIT PROGRAM STRUCTURE
!
      IVALUE=0
      IF(LOGVAL) IVALUE=1
      CALL SBIT(IARR,I,IVALUE)
      RETURN
      END
