      SUBROUTINE UNSCALER ( MATRIX, VECTOR, SCALE, DIM )
      IMPLICIT NONE
!
! 1.  UNSCALER PROGRAM SPECIFICATION
!
! 1.1 Unscale SOLVE format matrix and B vector.
!
! 1.2 REFERENCES:
!
! 2.  UNSCALER INTERFACE
!
! 2.1 Parameter File
!
! 2.2 INPUT Variables:
!
      INTEGER*4 DIM
      REAL*8 SCALE(DIM)
!
! DIM - Dimension of the vector
! SCALE -
!
! 2.3 OUTPUT Variables:
!
      REAL*8 MATRIX(*),VECTOR(DIM)
!
! MATRIX - The SOLVE format matrix
! VECTOR - The B vector
!
! 2.4 COMMON BLOCKS USED
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES:
!       CALLED SUBROUTINES:  DSCAL, DXMPY
!
! 3.  LOCAL VARIABLES
!
      INTEGER*4 I
      INTEGER*4 IBLAS1, NBLAS
      INTEGER*8 IJ
      REAL*8    LOCAL
!
! I - Loop index
! IJ - Index into MATRIX
!
! 4.  HISTORY
!   WHO   WHEN   WHAT
!
! 5.  UNSCALER PROGRAM STRUCTURE
!
!   unscaling
!
      IJ=1
      DO I=1,DIM
         LOCAL=SCALE(I)
         NBLAS=I
         CALL DSCAL ( NBLAS, LOCAL, MATRIX(IJ), 1 )
         CALL DXMPY ( NBLAS, SCALE, 1, MATRIX(IJ), 1 )
         IJ = IJ + I
      ENDDO
      NBLAS=DIM
      CALL DXMPY ( NBLAS, SCALE, 1, VECTOR, 1 )
!
      RETURN
      END  !#!  UNSCALER  #!#
