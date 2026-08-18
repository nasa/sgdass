      SUBROUTINE NRMEQ ( A, B, NPARAM, DERIV, OC, ERR, NELEM )
      IMPLICIT NONE
      INCLUDE 'solve.i'
!
! 1.  NRMEQ PROGRAM SPECIFICATION
!
! 1.1 Increment the normal equations.
!   NOTE: The matrix is stored in row(?) major lower triangular format
!         or is it column major upper triangular format
!         anyway (you can switch the order of subscripts IFyou like):
!         A(1,1),A(1,2),A(2,2),...,A(1,N),...A(N,N)
!
! 1.2 REFERENCES:
!
! 2.  NRMEQ INTERFACE
!
! 2.1 Parameter File
!
! 2.2 INPUT Variables:
!
      INTEGER*8 NELEM
      REAL*8    DERIV(*), A(NELEM), B(*), OC, ERR
      INTEGER*4 NPARAM
!
! A,B - Normal equation matrix and vector
! DERIV - Partial derivatives
! ERR - Size of the error on this observation
! NELEM - Size of the matrix array A
! NPARAM - Number of parameters
! OC - Residual delay
!
! 2.3 OUTPUT Variables:
!
! A,B - Incremented matrix and vector
!
! 2.4 COMMON BLOCKS USED
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES: loop
!       CALLED SUBROUTINES: utility routines
!
! 3.  LOCAL VARIABLES
!
      INTEGER*4 NN
      CHARACTER BUFSTR*79
      REAL*8    DD, WGT
      INTEGER*4 NBLAS, IBLAS
! this is index into non-zero elements of Deriv
      INTEGER*4 INDX(M_GPA), IPTR, JPTR, I, J, NUM_NON_ZERO
      INTEGER*8 IJ
      INTEGER*8, EXTERNAL :: INDX8
!
! 4.  HISTORY
!   WHO   WHEN   WHAT
!   Modified by JMGipson to only use non-zero parts of DERIV.
!
! 5.  NRMEQ PROGRAM STRUCTURE
!
! Determine weight based on error size
!
      WGT = 1.0D0/ERR**2
!
      NUM_NON_ZERO=0
      DO I=1,NPARAM
         IF ( DERIV(I) .NE. 0 ) THEN
              NUM_NON_ZERO = NUM_NON_ZERO+1
              INDX(NUM_NON_ZERO) = I
         ENDIF
      END DO
!
      DO I=1,NUM_NON_ZERO
         IPTR = INDX(I)
         B(IPTR) = B(IPTR) + WGT*DERIV(IPTR)*OC
         DD=DERIV(IPTR)*WGT
         DO J=1,I
            JPTR=INDX(J)
            IJ=INDX8(IPTR,JPTR)
            A(IJ)=A(IJ)+DD*DERIV(JPTR)
         ENDDO
      END DO
      RETURN
!
! --- old code is left in below so that you can see what it did.
!
      NN = 1
!
!  B VECTOR
!
      DD=WGT*OC
      IBLAS=1
      NBLAS=NPARAM
      CALL DAXPY ( NBLAS, DD, DERIV, IBLAS, B, IBLAS )
!
!  A MATRIX
!
      DO J=1, NPARAM
         NN=NN+J-1
!
! ------ Test for a non-zero derivative
!
         IF ( DERIV(J) .NE. 0.0D0 ) THEN
              DD=WGT*DERIV(J)
              NBLAS=J
              CALL DAXPY ( NBLAS, DD, DERIV, IBLAS, A(NN), IBLAS )
          ENDIF
      ENDDO
!
      RETURN
      END
