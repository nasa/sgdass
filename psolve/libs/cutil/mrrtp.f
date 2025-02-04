      SUBROUTINE MRRTP(N1,LEFT,N2,RIGHT,N3,PROD)
      IMPLICIT NONE
!
! 1.  MRRTP PROGRAM SPECIFICATION
!
! 1.1 Multiply one rectangular matrix by the transpose of another,
!     and put the result in packed format.
!                       T
!         [LEFT] [RIGHT]  = [PROD]
!
! 1.2 REFERENCES:
!
! 2.  MRRTP INTERFACE
!
! 2.1 Parameter File
!
! 2.2 INPUT Variables:
!
      INTEGER*2 N1,N2,N3
      REAL*8 LEFT(N1,N2),RIGHT(N3,N2)
!
! N1 - Number of rows in LEFT and PROD
! N2 - Number of columns in LEFT and RIGHT
! N3 - Number of rows in RIGHT, columns in PROD
! LEFT - First matrix to be multiplied, N1 X N2
! RIGHT - Second matrix, whose transpose is to be multiplied, N3 X N2
!
! 2.3 OUTPUT Variables:
!
      REAL*8 PROD(*)
!
! PROD - Product matrix, N1 X N3, in packed format
!
! 2.4 COMMON BLOCKS USED
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES:
!       CALLED SUBROUTINES: clrmt,indx4
!
! 3.  LOCAL VARIABLES
!
      INTEGER*2 I,J,K
      INTEGER*4 N4,INDX4,IJ
!
! I,J,K - Loop indices
! IJ - Index for output array elements
! N4 - Number of elements in output array
!
! 4. HISTORY
!   WHO   WHEN   WHAT
!
! 5.  MRRTP PROGRAM STRUCTURE
!
! First clear out the product array
!
      N4=N3
      N4=(N4*(N4+1))/2
      CALL CLRMT(PROD,N4)
!
! Now do the multiplication
!
      DO I=1,N1
        DO J=1,I
          IJ=INDX4(I,J)
          DO K=1,N2
            PROD(IJ)=PROD(IJ)+LEFT(I,K)*RIGHT(J,K)
          ENDDO
        ENDDO
      ENDDO
!
      RETURN
      END
