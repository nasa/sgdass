      SUBROUTINE ADE ( IELEM, IROW, ICOL )
      IMPLICIT NONE                         !Added by IMP/jwr
!
! 1.  ADE PROGRAM SPECIFICATION
!
! 1.1 Given an element ielem of a vector representing a row major,
!     lower triangular
!     matrix (e.g., the A matrix),  return the row and column of the element.
!     I.e., return the indices irow and icol of A(irow,icol) corresponding
!     to A(ielem) where the vector is stored
!     in row major, lower triangular form: A(1,1),A(2,1),A(2,2)...
!     ..A(N,1),A(N,N)
!
! 1.2 REFERENCES:
!
! 2.  ADE INTERFACE
!
! 2.1 Parameter File
!
! 2.2 INPUT Variables:
!
      INTEGER*4 IELEM
!
! 2.3 OUTPUT Variables:
!
      INTEGER*4 IROW,ICOL
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
      INTEGER*4 IVAL
      INTEGER*4 ISUB
!
! 4.  HISTORY
!   WHO   WHEN   WHAT
!   kdb  960625  Created
!
! 5.  ADE PROGRAM STRUCTURE
!
      IVAL = IELEM
      ISUB = 0
      DO WHILE ( IVAL .GT. 0 )
         ISUB = ISUB + 1
         IVAL = IVAL - ISUB
      ENDDO
      IVAL = IVAL + ISUB
!
      IROW = ISUB
      ICOL = IVAL
!
      RETURN
      END
