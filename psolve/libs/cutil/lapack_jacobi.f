      SUBROUTINE LAPACK_JACOBI ( A, N, NP, D, V, NROT, B, Z )
! ************************************************************************
! *                                                                      *
! *   Routine LAPACK_JACOBI diagonalizes a symmetric matrix using brute  *
! *   force planar rotations.                                            *
! *                                                                      *
! *  ### 23-FEB-2022 LAPACK_JACOBI v1.0 (c)  L. Petrov  23-FEB-2022 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INTEGER*2  N, NP, NROT
      REAL*8     A(N,N), D(N), V(N,N), B(N), Z(N)
      REAL*8,    ALLOCATABLE :: WORK(:)
      INTEGER*4  NB, DIM, LWORK, INFO
!
      DIM = N
      NB = MIN(64,DIM)
      LWORK = N*NB 
      ALLOCATE ( WORK(LWORK) )
      V = A
      CALL DSYEV ( 'V', 'U', DIM, V, 3, D, WORK, LWORK, INFO )
      IF ( INFO .NE. 0 ) THEN
           CALL FERR ( INT2(132), 'Failure in computatinion of eignvectors', INT2(0), INT2(0) )
      END IF
      DEALLOCATE ( WORK )
      B = 0.0D0
      Z = 0.D0
      RETURN 
      END SUBROUTINE LAPACK_JACOBI
