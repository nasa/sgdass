      SUBROUTINE LOCAL ( A, B, NG, NL, ND, NGTA, AT )
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
! 1.  LOCAL PROGRAM SPECIFICATION
!
! 1.1 Manipulate the sub-matrices in the saved arcfile to produce
!     the solution for the local parameters.
!
! 1.2 REFERENCES:
!
! 2.  LOCAL INTERFACE
!
! 2.1 Parameter File
      INCLUDE 'solve.i'
!
! 2.2 INPUT Variables:
!
      INTEGER*4 NG, NL, ND, NGTA
      REAL*8    A(*),B(ND),AT(NL,NGTA)
!
! A,B - Sub-matrices from the arc file
! AT - A sub-matrix of sub-matrix A
! ND - Number of parameters
! NG - If CORRLN is True: NG is the total number of global parameters in
!                         in this solution
!                   False: NG is number of global parameters in an
!                          individual solution
! NGTA - Number of global parameters in an individual solution
! NL - Number of arc parameters in an individual solution
!
! 2.3 OUTPUT Variables: None
!
! 2.4 COMMON BLOCKS USED
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES: manip
!       CALLED SUBROUTINES: utility routines
!
! 3.  LOCAL VARIABLES
!
      INTEGER*4  I, J, IBLAS0, IBLAS1, NBLAS
      INTEGER*8  IN1J
      INTEGER*8, EXTERNAL ::  INDX8
!
! 4.  HISTORY
!   WHO   WHEN   WHAT
!
! 5.  LOCAL PROGRAM STRUCTURE
!
      IBLAS0=0
      IBLAS1=1
      NBLAS=NL
      DO J=NL+1,NL+NGTA
         IN1J=INDX8( 1, J)
         CALL DCOPY ( NBLAS, A(IN1J), IBLAS1, AT(1,J-NL), IBLAS1 )
         CALL DCOPY ( NBLAS, 0.0D0, IBLAS0, A(IN1J), IBLAS1 )
      ENDDO
      DO J=NL+NGTA+1,NL+NG
         IN1J=INDX8( 1, J)
         CALL DCOPY ( NBLAS, 0.0D0, IBLAS0, A(IN1J), IBLAS1 )
      ENDDO
!
!   B[i]  = B[i] - inv(A[ii]) * A[ig] * B[g]
! locally = B[i] + transpose(A[gi])   * B[g]
!
!
      DO I=NL+1,NL+NGTA
         CALL DAXPY ( NBLAS, B(I), AT(1,I-NL), IBLAS1, B(1), IBLAS1 )
      ENDDO
!
!   form the covariance matrix
!
!   A[ig] = - inv(A[ii]) * A[ig] * inv(A[gg])
! locally =   transpose(A[gi])   *     A[gg]
!
      DO J=NL+1,NL+NG
         IN1J=INDX8( 1, J)
         DO I=NL+1,NL+NGTA
            CALL DAXPY ( NBLAS, A(INDX8(I,J)), AT(1,I-NL), IBLAS1, A(IN1J), &
     &                   IBLAS1 )
         ENDDO
      ENDDO
!
!   A[ii] = inv(A[ii]) + inv(A[ii])*A[ig]*inv(A[gg])*A[gi]*inv(A[ii])
! locally = A[ii] + A[ig] * A[gi]
!
      DO J=1,NL
         IN1J=INDX8( 1, J)
         DO I=NL+1,NL+NGTA
            NBLAS=J
            CALL DAXPY ( NBLAS, AT(J,I-NL), A(INDX8(1,I)), IBLAS1, &
     &                   A(IN1J), IBLAS1 )
         ENDDO
      ENDDO
!
      RETURN
      END
