      SUBROUTINE DUMPR(NAME,RECT,DIM1,DIM2)
      IMPLICIT NONE
!
! 1.  DUMPR PROGRAM SPECIFICATION
!
! 1.1 Dump the contents of a rectangular array to the printer (LU 7)
!
! 1.2 REFERENCES:
!
! 2.  DUMPR INTERFACE
!
! 2.1 Parameter File
!
! 2.2 INPUT Variables:
!
      INTEGER*2 DIM1,DIM2
      REAL*8 RECT(DIM1,DIM2)
      CHARACTER*(*) NAME
!
! DIM1,DIM2 - Dimensions of the array to be dumped
! NAME - Name to be printed as header
! RECT - The array to be dumped
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
      INTEGER*2 I,J
!
! I,J - Loop indices
!
! 4.  HISTORY
!   WHO   WHEN   WHAT
!
! 5.  DUMPR PROGRAM STRUCTURE
!
      WRITE(7,1110) NAME
!
      DO I=1,DIM1
        WRITE(7,2000) I,(RECT(I,J),J=1,DIM2)
      ENDDO
!
 1110 FORMAT("1  ",A)
 2000 FORMAT("0",I10/(" ",5D25.17))
!
      RETURN
      END
