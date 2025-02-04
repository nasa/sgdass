      FUNCTION INDX4(I,J)
      IMPLICIT NONE                         !Added by IMP/jwr
!
! 1.  INDX4 PROGRAM SPECIFICATION
!
! 1.1 Calculate the array element occupied by the matrix element I,J
!     where I is the row and J is the column.  The matrix is stored
!     in row major, lower triangular form: A(1,1),A(2,1),A(2,2)...
!     ..A(N,1),A(N,N)
!
! 1.2 REFERENCES:
!
! 2.  INDX4 INTERFACE
!
! 2.1 Parameter File
!
! 2.2 INPUT Variables:
!
      INTEGER*2 I,J
!
! I,J - Row and column of element to be located
!
! 2.3 OUTPUT Variables:
!
      INTEGER*4 INDX4
!
! INDX4 - Array index of the specified matrix element
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
      INTEGER*4 J4,I4
!
! I4,J4 - I*4 versions of row and column
!
! 4.  HISTORY
!   WHO   WHEN   WHAT
!
! 5.  INDX4 PROGRAM STRUCTURE
!
      J4=J
      I4=I
      IF(J.GT.I) THEN
        INDX4=I4+(J4-1)*J4/2
      ELSE
        INDX4=J4+(I4-1)*I4/2
      ENDIF
!
      RETURN
      END
