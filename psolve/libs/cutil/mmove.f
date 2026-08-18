      SUBROUTINE MMOVE(ORIGIN,DESTIN,N)
      IMPLICIT NONE
!
! 1.  MMOVE PROGRAM SPECIFICATION
!
! 1.1 Copy one double precision array into another
!
! 1.2 REFERENCES:
!
! 2.  MMOVE INTERFACE
!
! 2.1 Parameter File
!
! 2.2 INPUT Variables:
!
      INTEGER*4 N
      REAL*8 ORIGIN(N)
!
! N - Number of elements to copy
! ORIGIN - Array to be copied from
!
! 2.3 OUTPUT Variables:
!
      REAL*8 DESTIN(N)
!
! DESTIN - Destination array
!
! 2.4 COMMON BLOCKS USED
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES:
!       CALLED SUBROUTINES: dcopy
!
! 3.  LOCAL VARIABLES
!
      INTEGER*4 INC
      DATA INC/1/
!
! INC - Index increment between each element
!
! 4.  HISTORY
!   WHO   WHEN   WHAT
!
! 5.  MMOVE PROGRAM STRUCTURE
!
      CALL DCOPY(N,ORIGIN,INC,DESTIN,INC)
!
      RETURN
      END
