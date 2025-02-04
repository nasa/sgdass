#include <mk5_preprocessor_directives.inc>
      SUBROUTINE INVS_6  ( A, EPS, IERR )
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
      A( 7  ) = A( 7  ) * S
      A( 11  ) = A( 11  ) * S
      A( 16  ) = A( 16  ) * S
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
      S = S + A( 2  ) * A( 7  )
      A( 8  ) = ( A( 8  ) - S ) * A( 3  )
      S = 0.0D0
      S = S + A( 2  ) * A( 11  )
      A( 12  ) = ( A( 12  ) - S ) * A( 3  )
      S = 0.0D0
      S = S + A( 2  ) * A( 16  )
      A( 17  ) = ( A( 17  ) - S ) * A( 3  )
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
      S = S + A( 4  ) * A( 7  )
      S = S + A( 5  ) * A( 8  )
      A( 9  ) = ( A( 9  ) - S ) * A( 6  )
      S = 0.0D0
      S = S + A( 4  ) * A( 11  )
      S = S + A( 5  ) * A( 12  )
      A( 13  ) = ( A( 13  ) - S ) * A( 6  )
      S = 0.0D0
      S = S + A( 4  ) * A( 16  )
      S = S + A( 5  ) * A( 17  )
      A( 18  ) = ( A( 18  ) - S ) * A( 6  )
      S = 0.0D0
      S = S + A( 7  ) * A( 7  )
      S = S + A( 8  ) * A( 8  )
      S = S + A( 9  ) * A( 9  )
      A( 10  ) = ( A( 10  ) - S )
      IF ( A( 10  ) .LT. EPS ) THEN
           IERR = 4
           RETURN
      END IF
      A( 10  ) = 1.0D0 / DSQRT ( A( 10  ) )
      S = 0.0D0
      S = S + A( 7  ) * A( 11  )
      S = S + A( 8  ) * A( 12  )
      S = S + A( 9  ) * A( 13  )
      A( 14  ) = ( A( 14  ) - S ) * A( 10  )
      S = 0.0D0
      S = S + A( 7  ) * A( 16  )
      S = S + A( 8  ) * A( 17  )
      S = S + A( 9  ) * A( 18  )
      A( 19  ) = ( A( 19  ) - S ) * A( 10  )
      S = 0.0D0
      S = S + A( 11  ) * A( 11  )
      S = S + A( 12  ) * A( 12  )
      S = S + A( 13  ) * A( 13  )
      S = S + A( 14  ) * A( 14  )
      A( 15  ) = ( A( 15  ) - S )
      IF ( A( 15  ) .LT. EPS ) THEN
           IERR = 5
           RETURN
      END IF
      A( 15  ) = 1.0D0 / DSQRT ( A( 15  ) )
      S = 0.0D0
      S = S + A( 11  ) * A( 16  )
      S = S + A( 12  ) * A( 17  )
      S = S + A( 13  ) * A( 18  )
      S = S + A( 14  ) * A( 19  )
      A( 20  ) = ( A( 20  ) - S ) * A( 15  )
      S = 0.0D0
      S = S + A( 16  ) * A( 16  )
      S = S + A( 17  ) * A( 17  )
      S = S + A( 18  ) * A( 18  )
      S = S + A( 19  ) * A( 19  )
      S = S + A( 20  ) * A( 20  )
      A( 21  ) = ( A( 21  ) - S )
      IF ( A( 21  ) .LT. EPS ) THEN
           IERR = 6
           RETURN
      END IF
      A( 21  ) = 1.0D0 / DSQRT ( A( 21  ) )
      S = 0.0D0
      S = S - A( 1  ) * A( 2  )
      A( 2 ) = S * A( 3  )
      S = 0.0D0
      S = S - A( 1  ) * A( 4  )
      S = S - A( 2  ) * A( 5  )
      A( 4 ) = S * A( 6  )
      S = 0.0D0
      S = S - A( 1  ) * A( 7  )
      S = S - A( 2  ) * A( 8  )
      S = S - A( 4  ) * A( 9  )
      A( 7 ) = S * A( 10  )
      S = 0.0D0
      S = S - A( 1  ) * A( 11  )
      S = S - A( 2  ) * A( 12  )
      S = S - A( 4  ) * A( 13  )
      S = S - A( 7  ) * A( 14  )
      A( 11 ) = S * A( 15  )
      S = 0.0D0
      S = S - A( 1  ) * A( 16  )
      S = S - A( 2  ) * A( 17  )
      S = S - A( 4  ) * A( 18  )
      S = S - A( 7  ) * A( 19  )
      S = S - A( 11  ) * A( 20  )
      A( 16 ) = S * A( 21  )
      S = 0.0D0
      S = S - A( 3  ) * A( 5  )
      A( 5 ) = S * A( 6  )
      S = 0.0D0
      S = S - A( 3  ) * A( 8  )
      S = S - A( 5  ) * A( 9  )
      A( 8 ) = S * A( 10  )
      S = 0.0D0
      S = S - A( 3  ) * A( 12  )
      S = S - A( 5  ) * A( 13  )
      S = S - A( 8  ) * A( 14  )
      A( 12 ) = S * A( 15  )
      S = 0.0D0
      S = S - A( 3  ) * A( 17  )
      S = S - A( 5  ) * A( 18  )
      S = S - A( 8  ) * A( 19  )
      S = S - A( 12  ) * A( 20  )
      A( 17 ) = S * A( 21  )
      S = 0.0D0
      S = S - A( 6  ) * A( 9  )
      A( 9 ) = S * A( 10  )
      S = 0.0D0
      S = S - A( 6  ) * A( 13  )
      S = S - A( 9  ) * A( 14  )
      A( 13 ) = S * A( 15  )
      S = 0.0D0
      S = S - A( 6  ) * A( 18  )
      S = S - A( 9  ) * A( 19  )
      S = S - A( 13  ) * A( 20  )
      A( 18 ) = S * A( 21  )
      S = 0.0D0
      S = S - A( 10  ) * A( 14  )
      A( 14 ) = S * A( 15  )
      S = 0.0D0
      S = S - A( 10  ) * A( 19  )
      S = S - A( 14  ) * A( 20  )
      A( 19 ) = S * A( 21  )
      S = 0.0D0
      S = S - A( 15  ) * A( 20  )
      A( 20 ) = S * A( 21  )
      S = 0.0D0
      S = S + A( 1  ) * A( 1  )
      S = S + A( 2  ) * A( 2  )
      S = S + A( 4  ) * A( 4  )
      S = S + A( 7  ) * A( 7  )
      S = S + A( 11  ) * A( 11  )
      S = S + A( 16  ) * A( 16  )
      A( 1  ) = S
      S = 0.0D0
      S = S + A( 2  ) * A( 3  )
      S = S + A( 4  ) * A( 5  )
      S = S + A( 7  ) * A( 8  )
      S = S + A( 11  ) * A( 12  )
      S = S + A( 16  ) * A( 17  )
      A( 2  ) = S
      S = 0.0D0
      S = S + A( 4  ) * A( 6  )
      S = S + A( 7  ) * A( 9  )
      S = S + A( 11  ) * A( 13  )
      S = S + A( 16  ) * A( 18  )
      A( 4  ) = S
      S = 0.0D0
      S = S + A( 7  ) * A( 10  )
      S = S + A( 11  ) * A( 14  )
      S = S + A( 16  ) * A( 19  )
      A( 7  ) = S
      S = 0.0D0
      S = S + A( 11  ) * A( 15  )
      S = S + A( 16  ) * A( 20  )
      A( 11  ) = S
      S = 0.0D0
      S = S + A( 16  ) * A( 21  )
      A( 16  ) = S
      S = 0.0D0
      S = S + A( 3  ) * A( 3  )
      S = S + A( 5  ) * A( 5  )
      S = S + A( 8  ) * A( 8  )
      S = S + A( 12  ) * A( 12  )
      S = S + A( 17  ) * A( 17  )
      A( 3  ) = S
      S = 0.0D0
      S = S + A( 5  ) * A( 6  )
      S = S + A( 8  ) * A( 9  )
      S = S + A( 12  ) * A( 13  )
      S = S + A( 17  ) * A( 18  )
      A( 5  ) = S
      S = 0.0D0
      S = S + A( 8  ) * A( 10  )
      S = S + A( 12  ) * A( 14  )
      S = S + A( 17  ) * A( 19  )
      A( 8  ) = S
      S = 0.0D0
      S = S + A( 12  ) * A( 15  )
      S = S + A( 17  ) * A( 20  )
      A( 12  ) = S
      S = 0.0D0
      S = S + A( 17  ) * A( 21  )
      A( 17  ) = S
      S = 0.0D0
      S = S + A( 6  ) * A( 6  )
      S = S + A( 9  ) * A( 9  )
      S = S + A( 13  ) * A( 13  )
      S = S + A( 18  ) * A( 18  )
      A( 6  ) = S
      S = 0.0D0
      S = S + A( 9  ) * A( 10  )
      S = S + A( 13  ) * A( 14  )
      S = S + A( 18  ) * A( 19  )
      A( 9  ) = S
      S = 0.0D0
      S = S + A( 13  ) * A( 15  )
      S = S + A( 18  ) * A( 20  )
      A( 13  ) = S
      S = 0.0D0
      S = S + A( 18  ) * A( 21  )
      A( 18  ) = S
      S = 0.0D0
      S = S + A( 10  ) * A( 10  )
      S = S + A( 14  ) * A( 14  )
      S = S + A( 19  ) * A( 19  )
      A( 10  ) = S
      S = 0.0D0
      S = S + A( 14  ) * A( 15  )
      S = S + A( 19  ) * A( 20  )
      A( 14  ) = S
      S = 0.0D0
      S = S + A( 19  ) * A( 21  )
      A( 19  ) = S
      S = 0.0D0
      S = S + A( 15  ) * A( 15  )
      S = S + A( 20  ) * A( 20  )
      A( 15  ) = S
      S = 0.0D0
      S = S + A( 20  ) * A( 21  )
      A( 20  ) = S
      S = 0.0D0
      S = S + A( 21  ) * A( 21  )
      A( 21  ) = S
!
      IERR = 0
      RETURN
      END  !#!  INVS_6   #!#
