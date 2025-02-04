      REAL*8 FUNCTION QUADP(A,V,TEMP,I,IN)
      IMPLICIT NONE
!
! 1.  QUADP PROGRAM SPECIFICATION
!
! 1.1 FORM TRANSPOSE(V(I:I+IN-1))*A(I:I+IN-1,I:I+IN-1)*V(I:I+IN-1)
!     ASSUMING A IS ACTUALLY SQUARE, BUT ITS NOT
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
      REAL*8 A(*),V(*),TEMP(*)
!
! A - Input Matrix
! V - Array of partial derivatives of the parameters
! TEMP - Alternate horizontal scaling factor
! I - Lower index
! IN - Upper index
!
! 2.3 OUTPUT Variables:
!
! TEMP - Alternate horizontal scaling factor
! QUADP - Returns DS
!
! 2.4 COMMON BLOCKS USED
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES: secnd
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
!  WHO  WHEN   WHAT
!
! 5.  QUADP PROGRAM STRUCTURE
!
! COLLECT OFF-DIAGONAL ELEMENTS
!
      iblas1=1
      DO IK=I+1,I+IN-1
!       CALL DWDOT(DS,V(I),1,A(INDX4(I,IK)),1,IK-I)
        nblas=ik-I
        ds = DDOT ( nblas,v(i),iblas1,a(indx4(i,ik)),iblas1)
        TEMP(IK)=DS
      ENDDO
!     DS=0.0D0
!     IF(IN.GT.0) CALL DWDOT(DS,TEMP(I+1),1,V(I+1),1,IN-1)
      nblas=in-1
      ds = DDOT(nblas,temp(i+1),iblas1,v(i+1),iblas1)
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
