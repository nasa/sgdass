      REAL*8 FUNCTION QUADP(A,V,TEMP,I,IN)
      IMPLICIT NONE
!
! 1.  QUADP PROGRAM SPECIFICATION
!
! 1.1 COLLECT OFF-DIAGONAL ELEMENTS
!
! 1.2 REFERENCES:
!
! 2.  QUADP INTERFACE
!
! 2.1 Parameter File
      INCLUDE 'solve.i'
!
! 2.2 INPUT Variables:
!
      INTEGER*2 I,IN
      REAL*8 A(*), V(*)
!
!  A - Matrix reflecting adjusted parameters
!  V - Earth orientation partials array element
!  I - Where in array start using adjustments
!  IN - Number of elements to use from the I position
!
! 2.3 OUTPUT Variables:
!
      REAL*8 TEMP(*)
!
!  TEMP - Off diagonal elements array
!  QUADP - total number of elements
!
! 2.4 COMMON BLOCKS USED
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES: eo_plot_entry
!       CALLED SUBROUTINES: utility routines
!
! 3.  LOCAL VARIABLES
!
      INTEGER*2 IK
      INTEGER*4 INDX4,nblas,iblas1
      REAL*8    DS
      REAL*8,   EXTERNAL :: DDOT
!
! 4.  HISTORY
!  WHO  WHEN    WHAT
!
! 5.  QUADP PROGRAM STRUCTUREE
!
!  FORM TRANSPOSE(V(I:I+IN-1))*A(I:I+IN-1,I:I+IN-1)*V(I:I+IN-1)
!  ASSUMING A IS ACTUALLY SQUARE, BUT ITS NOT
!
!
      iblas1=1
      DO IK=I+1,I+IN-1
        nblas=ik-I
        ds = DDOT ( nblas,v(i),iblas1,a(indx4(i,ik)),iblas1)
        TEMP(IK)=DS
      ENDDO
      nblas=in-1
      ds = DDOT ( nblas,temp(i+1),iblas1,v(i+1),iblas1)
!
! MULTIPLY BY TWO FOR THE MISSING LOWER TRIANGLE
!
      DS=DS*2.0D0
!
! ADD IN THE DIAGONAL ELEMENTS
!
      DO IK=I,I+IN-1
        DS=DS+V(IK)*A(INDX4(IK,IK))*V(IK)
      ENDDO
!
      QUADP=DS
      RETURN
      END
