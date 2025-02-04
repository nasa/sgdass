#include <mk5_preprocessor_directives.inc>
      SUBROUTINE INVS_3  ( A, EPS, IERR )
! ************************************************************************
! *                                                                      *
! *       Inverts square symmetric matrix in packed upper triangular     *
! *     storage format using Cholesky decomposition.                     *
! *                                                                      *
! * ### Source code was created automatically                         ## *
! * ### Generator: INVS_GENERATOR (c) L. Petrov version of 2002.11.28  ## *
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
      A(1) = 1.0D0 / DSQRT ( A(1) )
      S = A(1)
      A( 2  ) = A( 2  ) * S
      A( 4  ) = A( 4  ) * S
      S = 0.0D0
      S = S + A( 2  ) * A( 2  )
      A( 3  ) = ( A( 3  ) - S )
      IF ( A( 3  ) .LT. EPS ) THEN
           IERR = 2
           RETURN
      END IF
      A( 3  ) = 1.0D0 / DSQRT ( A( 3  ) )
      S = 0.0D0
      S = S + A( 2  ) * A( 4  )
      A( 5  ) = ( A( 5  ) - S ) * A( 3  )
      S = 0.0D0
      S = S + A( 4  ) * A( 4  )
      S = S + A( 5  ) * A( 5  )
      A( 6  ) = ( A( 6  ) - S )
      IF ( A( 6  ) .LT. EPS ) THEN
           IERR = 3
           RETURN
      END IF
      A( 6  ) = 1.0D0 / DSQRT ( A( 6  ) )
      S = 0.0D0
      S = S - A( 1  ) * A( 2  )
      A( 2 ) = S * A( 3  )
      S = 0.0D0
      S = S - A( 1  ) * A( 4  )
      S = S - A( 2  ) * A( 5  )
      A( 4 ) = S * A( 6  )
      S = 0.0D0
      S = S - A( 3  ) * A( 5  )
      A( 5 ) = S * A( 6  )
      S = 0.0D0
      S = S + A( 1  ) * A( 1  )
      S = S + A( 2  ) * A( 2  )
      S = S + A( 4  ) * A( 4  )
      A( 1  ) = S
      S = 0.0D0
      S = S + A( 2  ) * A( 3  )
      S = S + A( 4  ) * A( 5  )
      A( 2  ) = S
      S = 0.0D0
      S = S + A( 4  ) * A( 6  )
      A( 4  ) = S
      S = 0.0D0
      S = S + A( 3  ) * A( 3  )
      S = S + A( 5  ) * A( 5  )
      A( 3  ) = S
      S = 0.0D0
      S = S + A( 5  ) * A( 6  )
      A( 5  ) = S
      S = 0.0D0
      S = S + A( 6  ) * A( 6  )
      A( 6  ) = S
!
      IERR = 0
      RETURN
      END  !#!  INVS_3   #!#
