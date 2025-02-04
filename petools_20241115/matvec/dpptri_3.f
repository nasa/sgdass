#include <mk5_preprocessor_directives.inc>
      SUBROUTINE DPPTRI_3  ( A, IERR )
! ************************************************************************
! *                                                                      *
! *   Inverts triangular matrix in upper triangular format.              *
! *                                                                      *
! * ### Source code was created automatically                         ## *
! * ### Generator: INVS_GENERATOR (c) L. Petrov version of 2002.11.28 ## *
! *                                                                      *
! ************************************************************************
      REAL*8     A(*), S
      INTEGER*4  IERR
!
      A( 1  ) = 1.D0 / A( 1  )
      S = 0.0D0
      S = S - A( 1  ) * A( 2  )
      A( 2  ) = S / A( 3  )
      S = 0.0D0
      S = S - A( 1  ) * A( 4  )
      S = S - A( 2  ) * A( 5  )
      A( 4  ) = S / A( 6  )
      A( 3  ) = 1.D0 / A( 3  )
      S = 0.0D0
      S = S - A( 3  ) * A( 5  )
      A( 5  ) = S / A( 6  )
      A( 6  ) = 1.D0 / A( 6  )
!
      IERR = 0
      RETURN
      END !#! SUBROUTINE DPPTRI_3 
