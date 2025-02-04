#include <mk5_preprocessor_directives.inc>
      SUBROUTINE DPPTRK_3  ( A )
! ************************************************************************
! *                                                                      *
! *   Computes A: = A * A(T)  where A is a triangular matrix in upper    *
! *   triangular format.                                                 *
! *                                                                      *
! * ### Source code was created automatically                         ## *
! * ### Generator: INVS_GENERATOR (c) L. Petrov version of 2002.11.28 ## *
! *                                                                      *
! ************************************************************************
      REAL*8     A(*), S
!
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
      RETURN
      END !#! SUBROUTINE DPPTRK_3 
