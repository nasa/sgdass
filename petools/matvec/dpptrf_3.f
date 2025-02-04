#include <mk5_preprocessor_directives.inc>
      SUBROUTINE DPPTRF_3  ( A, EPS, IERR )
! ************************************************************************
! *                                                                      *
! *   Compute Cholesky decomposition of a square symmetric matrix in     *
! *   upper triangular format.  A := U   ( A = U(T) * U )                *
! *                                                                      *
! * ### Source code was created automatically                         ## *
! * ### Generator: INVS_GENERATOR (c) L. Petrov version of 2002.11.28 ## *
! *                                                                      *
! ************************************************************************
      REAL*8     A(*), S
      REAL*8     EPS
      INTEGER*4  IERR
!
      IF ( A ( 1 ) .LT. EPS ) THEN
           IERR = 1
           RETURN
      END IF
      A(1) = DSQRT ( A(1) )
      S = 1.D0 / A(1)
      A( 2  ) = A( 2  ) * S
      A( 4  ) = A( 4  ) * S
      S = 0.0D0
      S = S + A( 2  ) * A( 2  )
      A( 3  ) = ( A( 3  ) - S )
      IF ( A( 3  ) .LT. EPS ) THEN
           IERR = 2
           RETURN
      END IF
      A( 3  ) = DSQRT ( A( 3  ) )
      S = 0.0D0
      S = S + A( 2  ) * A( 4  )
      A( 5  ) = ( A( 5  ) - S ) / A( 3  )
      S = 0.0D0
      S = S + A( 4  ) * A( 4  )
      S = S + A( 5  ) * A( 5  )
      A( 6  ) = ( A( 6  ) - S )
      IF ( A( 6  ) .LT. EPS ) THEN
           IERR = 3
           RETURN
      END IF
      A( 6  ) = DSQRT ( A( 6  ) )
!
      IERR = 0
      RETURN
      END !#! SUBROUTINE DPPTRF_3 
