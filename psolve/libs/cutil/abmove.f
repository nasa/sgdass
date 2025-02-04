      SUBROUTINE ABMOVE(A1,B1,A2,B2,IX2T1, NPARM)
      Implicit    NONE
!
! 1.  ABMOVE PROGRAM SPECIFICATION
!
! 1.1 Make a new copy of the SOLVE matrices in rearranged order.
!
! 1.2 REFERENCES:
!
! 2.  ABMOVE INTERFACE
!
! 2.1 Parameter File
      INCLUDE 'solve.i'
!
! 2.2 INPUT Variables:
!
      REAL*8      A2(*),B2(*)
      INTEGER*4   IX2T1(*), NPARM
!
! A2 - The SOLVE A matrix
! B2 - The SOLVE B matrix
! IX2T1 - Cross-reference array
! NPARM - Number of parameters
!
! 2.3 OUTPUT Variables:
!
      REAL*8      A1(*), B1(*)
!
! A1 - The rearranged A matrix
! B1 - The rearranged B matrix
!
! 2.4 COMMON BLOCKS USED
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES: reorder
!       CALLED SUBROUTINES: indx8
!
! 3.  LOCAL VARIABLES
!
      INTEGER*4 I, J
      INTEGER*4 INDX8
!
! 4.  HISTORY
!   WHO   WHEN   WHAT
!
! 5.  ABMOVE PROGRAM STRUCTURE
!
      DO I=1,NPARM
         B1(IX2T1(I))=B2(I)
         DO J=1,I
            A1( INDX8(IX2T1(I),IX2T1(J)) )=A2( INDX8(I,J) )
         END DO
      END DO
!
      RETURN
      END
