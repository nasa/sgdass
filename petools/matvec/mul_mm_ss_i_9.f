#include <mk5_preprocessor_directives.inc>
      SUBROUTINE MUL_MM_SS_I_9  ( A, B, C )
! ************************************************************************
! *                                                                      *
! *   Compute a product of two square symmetric matrices in upper        *
! *   triangular representation.                                         *
! *                                                                      *
! * ### Source code was created automatically                         ## *
! * ### Generator: INVS_GENERATOR (c) L. Petrov version of 2002.12.12 ## *
! *                                                                      *
! ************************************************************************
      REAL*8     A(*), B(*), C( 9 , 9 )
!
      C( 1 , 1 ) = A( 1 ) * B( 1 )
      C( 1 , 1 ) = C( 1 , 1 ) + A( 2 ) * B( 2 )
      C( 1 , 1 ) = C( 1 , 1 ) + A( 4 ) * B( 4 )
      C( 1 , 1 ) = C( 1 , 1 ) + A( 7 ) * B( 7 )
      C( 1 , 1 ) = C( 1 , 1 ) + A( 11 ) * B( 11 )
      C( 1 , 1 ) = C( 1 , 1 ) + A( 16 ) * B( 16 )
      C( 1 , 1 ) = C( 1 , 1 ) + A( 22 ) * B( 22 )
      C( 1 , 1 ) = C( 1 , 1 ) + A( 29 ) * B( 29 )
      C( 1 , 1 ) = C( 1 , 1 ) + A( 37 ) * B( 37 )
!
      C( 1 , 2 ) = A( 1 ) * B( 2 )
      C( 1 , 2 ) = C( 1 , 2 ) + A( 2 ) * B( 3 )
      C( 1 , 2 ) = C( 1 , 2 ) + A( 4 ) * B( 5 )
      C( 1 , 2 ) = C( 1 , 2 ) + A( 7 ) * B( 8 )
      C( 1 , 2 ) = C( 1 , 2 ) + A( 11 ) * B( 12 )
      C( 1 , 2 ) = C( 1 , 2 ) + A( 16 ) * B( 17 )
      C( 1 , 2 ) = C( 1 , 2 ) + A( 22 ) * B( 23 )
      C( 1 , 2 ) = C( 1 , 2 ) + A( 29 ) * B( 30 )
      C( 1 , 2 ) = C( 1 , 2 ) + A( 37 ) * B( 38 )
!
      C( 1 , 3 ) = A( 1 ) * B( 4 )
      C( 1 , 3 ) = C( 1 , 3 ) + A( 2 ) * B( 5 )
      C( 1 , 3 ) = C( 1 , 3 ) + A( 4 ) * B( 6 )
      C( 1 , 3 ) = C( 1 , 3 ) + A( 7 ) * B( 9 )
      C( 1 , 3 ) = C( 1 , 3 ) + A( 11 ) * B( 13 )
      C( 1 , 3 ) = C( 1 , 3 ) + A( 16 ) * B( 18 )
      C( 1 , 3 ) = C( 1 , 3 ) + A( 22 ) * B( 24 )
      C( 1 , 3 ) = C( 1 , 3 ) + A( 29 ) * B( 31 )
      C( 1 , 3 ) = C( 1 , 3 ) + A( 37 ) * B( 39 )
!
      C( 1 , 4 ) = A( 1 ) * B( 7 )
      C( 1 , 4 ) = C( 1 , 4 ) + A( 2 ) * B( 8 )
      C( 1 , 4 ) = C( 1 , 4 ) + A( 4 ) * B( 9 )
      C( 1 , 4 ) = C( 1 , 4 ) + A( 7 ) * B( 10 )
      C( 1 , 4 ) = C( 1 , 4 ) + A( 11 ) * B( 14 )
      C( 1 , 4 ) = C( 1 , 4 ) + A( 16 ) * B( 19 )
      C( 1 , 4 ) = C( 1 , 4 ) + A( 22 ) * B( 25 )
      C( 1 , 4 ) = C( 1 , 4 ) + A( 29 ) * B( 32 )
      C( 1 , 4 ) = C( 1 , 4 ) + A( 37 ) * B( 40 )
!
      C( 1 , 5 ) = A( 1 ) * B( 11 )
      C( 1 , 5 ) = C( 1 , 5 ) + A( 2 ) * B( 12 )
      C( 1 , 5 ) = C( 1 , 5 ) + A( 4 ) * B( 13 )
      C( 1 , 5 ) = C( 1 , 5 ) + A( 7 ) * B( 14 )
      C( 1 , 5 ) = C( 1 , 5 ) + A( 11 ) * B( 15 )
      C( 1 , 5 ) = C( 1 , 5 ) + A( 16 ) * B( 20 )
      C( 1 , 5 ) = C( 1 , 5 ) + A( 22 ) * B( 26 )
      C( 1 , 5 ) = C( 1 , 5 ) + A( 29 ) * B( 33 )
      C( 1 , 5 ) = C( 1 , 5 ) + A( 37 ) * B( 41 )
!
      C( 1 , 6 ) = A( 1 ) * B( 16 )
      C( 1 , 6 ) = C( 1 , 6 ) + A( 2 ) * B( 17 )
      C( 1 , 6 ) = C( 1 , 6 ) + A( 4 ) * B( 18 )
      C( 1 , 6 ) = C( 1 , 6 ) + A( 7 ) * B( 19 )
      C( 1 , 6 ) = C( 1 , 6 ) + A( 11 ) * B( 20 )
      C( 1 , 6 ) = C( 1 , 6 ) + A( 16 ) * B( 21 )
      C( 1 , 6 ) = C( 1 , 6 ) + A( 22 ) * B( 27 )
      C( 1 , 6 ) = C( 1 , 6 ) + A( 29 ) * B( 34 )
      C( 1 , 6 ) = C( 1 , 6 ) + A( 37 ) * B( 42 )
!
      C( 1 , 7 ) = A( 1 ) * B( 22 )
      C( 1 , 7 ) = C( 1 , 7 ) + A( 2 ) * B( 23 )
      C( 1 , 7 ) = C( 1 , 7 ) + A( 4 ) * B( 24 )
      C( 1 , 7 ) = C( 1 , 7 ) + A( 7 ) * B( 25 )
      C( 1 , 7 ) = C( 1 , 7 ) + A( 11 ) * B( 26 )
      C( 1 , 7 ) = C( 1 , 7 ) + A( 16 ) * B( 27 )
      C( 1 , 7 ) = C( 1 , 7 ) + A( 22 ) * B( 28 )
      C( 1 , 7 ) = C( 1 , 7 ) + A( 29 ) * B( 35 )
      C( 1 , 7 ) = C( 1 , 7 ) + A( 37 ) * B( 43 )
!
      C( 1 , 8 ) = A( 1 ) * B( 29 )
      C( 1 , 8 ) = C( 1 , 8 ) + A( 2 ) * B( 30 )
      C( 1 , 8 ) = C( 1 , 8 ) + A( 4 ) * B( 31 )
      C( 1 , 8 ) = C( 1 , 8 ) + A( 7 ) * B( 32 )
      C( 1 , 8 ) = C( 1 , 8 ) + A( 11 ) * B( 33 )
      C( 1 , 8 ) = C( 1 , 8 ) + A( 16 ) * B( 34 )
      C( 1 , 8 ) = C( 1 , 8 ) + A( 22 ) * B( 35 )
      C( 1 , 8 ) = C( 1 , 8 ) + A( 29 ) * B( 36 )
      C( 1 , 8 ) = C( 1 , 8 ) + A( 37 ) * B( 44 )
!
      C( 1 , 9 ) = A( 1 ) * B( 37 )
      C( 1 , 9 ) = C( 1 , 9 ) + A( 2 ) * B( 38 )
      C( 1 , 9 ) = C( 1 , 9 ) + A( 4 ) * B( 39 )
      C( 1 , 9 ) = C( 1 , 9 ) + A( 7 ) * B( 40 )
      C( 1 , 9 ) = C( 1 , 9 ) + A( 11 ) * B( 41 )
      C( 1 , 9 ) = C( 1 , 9 ) + A( 16 ) * B( 42 )
      C( 1 , 9 ) = C( 1 , 9 ) + A( 22 ) * B( 43 )
      C( 1 , 9 ) = C( 1 , 9 ) + A( 29 ) * B( 44 )
      C( 1 , 9 ) = C( 1 , 9 ) + A( 37 ) * B( 45 )
!
      C( 2 , 1 ) = A( 2 ) * B( 1 )
      C( 2 , 1 ) = C( 2 , 1 ) + A( 3 ) * B( 2 )
      C( 2 , 1 ) = C( 2 , 1 ) + A( 5 ) * B( 4 )
      C( 2 , 1 ) = C( 2 , 1 ) + A( 8 ) * B( 7 )
      C( 2 , 1 ) = C( 2 , 1 ) + A( 12 ) * B( 11 )
      C( 2 , 1 ) = C( 2 , 1 ) + A( 17 ) * B( 16 )
      C( 2 , 1 ) = C( 2 , 1 ) + A( 23 ) * B( 22 )
      C( 2 , 1 ) = C( 2 , 1 ) + A( 30 ) * B( 29 )
      C( 2 , 1 ) = C( 2 , 1 ) + A( 38 ) * B( 37 )
!
      C( 2 , 2 ) = A( 2 ) * B( 2 )
      C( 2 , 2 ) = C( 2 , 2 ) + A( 3 ) * B( 3 )
      C( 2 , 2 ) = C( 2 , 2 ) + A( 5 ) * B( 5 )
      C( 2 , 2 ) = C( 2 , 2 ) + A( 8 ) * B( 8 )
      C( 2 , 2 ) = C( 2 , 2 ) + A( 12 ) * B( 12 )
      C( 2 , 2 ) = C( 2 , 2 ) + A( 17 ) * B( 17 )
      C( 2 , 2 ) = C( 2 , 2 ) + A( 23 ) * B( 23 )
      C( 2 , 2 ) = C( 2 , 2 ) + A( 30 ) * B( 30 )
      C( 2 , 2 ) = C( 2 , 2 ) + A( 38 ) * B( 38 )
!
      C( 2 , 3 ) = A( 2 ) * B( 4 )
      C( 2 , 3 ) = C( 2 , 3 ) + A( 3 ) * B( 5 )
      C( 2 , 3 ) = C( 2 , 3 ) + A( 5 ) * B( 6 )
      C( 2 , 3 ) = C( 2 , 3 ) + A( 8 ) * B( 9 )
      C( 2 , 3 ) = C( 2 , 3 ) + A( 12 ) * B( 13 )
      C( 2 , 3 ) = C( 2 , 3 ) + A( 17 ) * B( 18 )
      C( 2 , 3 ) = C( 2 , 3 ) + A( 23 ) * B( 24 )
      C( 2 , 3 ) = C( 2 , 3 ) + A( 30 ) * B( 31 )
      C( 2 , 3 ) = C( 2 , 3 ) + A( 38 ) * B( 39 )
!
      C( 2 , 4 ) = A( 2 ) * B( 7 )
      C( 2 , 4 ) = C( 2 , 4 ) + A( 3 ) * B( 8 )
      C( 2 , 4 ) = C( 2 , 4 ) + A( 5 ) * B( 9 )
      C( 2 , 4 ) = C( 2 , 4 ) + A( 8 ) * B( 10 )
      C( 2 , 4 ) = C( 2 , 4 ) + A( 12 ) * B( 14 )
      C( 2 , 4 ) = C( 2 , 4 ) + A( 17 ) * B( 19 )
      C( 2 , 4 ) = C( 2 , 4 ) + A( 23 ) * B( 25 )
      C( 2 , 4 ) = C( 2 , 4 ) + A( 30 ) * B( 32 )
      C( 2 , 4 ) = C( 2 , 4 ) + A( 38 ) * B( 40 )
!
      C( 2 , 5 ) = A( 2 ) * B( 11 )
      C( 2 , 5 ) = C( 2 , 5 ) + A( 3 ) * B( 12 )
      C( 2 , 5 ) = C( 2 , 5 ) + A( 5 ) * B( 13 )
      C( 2 , 5 ) = C( 2 , 5 ) + A( 8 ) * B( 14 )
      C( 2 , 5 ) = C( 2 , 5 ) + A( 12 ) * B( 15 )
      C( 2 , 5 ) = C( 2 , 5 ) + A( 17 ) * B( 20 )
      C( 2 , 5 ) = C( 2 , 5 ) + A( 23 ) * B( 26 )
      C( 2 , 5 ) = C( 2 , 5 ) + A( 30 ) * B( 33 )
      C( 2 , 5 ) = C( 2 , 5 ) + A( 38 ) * B( 41 )
!
      C( 2 , 6 ) = A( 2 ) * B( 16 )
      C( 2 , 6 ) = C( 2 , 6 ) + A( 3 ) * B( 17 )
      C( 2 , 6 ) = C( 2 , 6 ) + A( 5 ) * B( 18 )
      C( 2 , 6 ) = C( 2 , 6 ) + A( 8 ) * B( 19 )
      C( 2 , 6 ) = C( 2 , 6 ) + A( 12 ) * B( 20 )
      C( 2 , 6 ) = C( 2 , 6 ) + A( 17 ) * B( 21 )
      C( 2 , 6 ) = C( 2 , 6 ) + A( 23 ) * B( 27 )
      C( 2 , 6 ) = C( 2 , 6 ) + A( 30 ) * B( 34 )
      C( 2 , 6 ) = C( 2 , 6 ) + A( 38 ) * B( 42 )
!
      C( 2 , 7 ) = A( 2 ) * B( 22 )
      C( 2 , 7 ) = C( 2 , 7 ) + A( 3 ) * B( 23 )
      C( 2 , 7 ) = C( 2 , 7 ) + A( 5 ) * B( 24 )
      C( 2 , 7 ) = C( 2 , 7 ) + A( 8 ) * B( 25 )
      C( 2 , 7 ) = C( 2 , 7 ) + A( 12 ) * B( 26 )
      C( 2 , 7 ) = C( 2 , 7 ) + A( 17 ) * B( 27 )
      C( 2 , 7 ) = C( 2 , 7 ) + A( 23 ) * B( 28 )
      C( 2 , 7 ) = C( 2 , 7 ) + A( 30 ) * B( 35 )
      C( 2 , 7 ) = C( 2 , 7 ) + A( 38 ) * B( 43 )
!
      C( 2 , 8 ) = A( 2 ) * B( 29 )
      C( 2 , 8 ) = C( 2 , 8 ) + A( 3 ) * B( 30 )
      C( 2 , 8 ) = C( 2 , 8 ) + A( 5 ) * B( 31 )
      C( 2 , 8 ) = C( 2 , 8 ) + A( 8 ) * B( 32 )
      C( 2 , 8 ) = C( 2 , 8 ) + A( 12 ) * B( 33 )
      C( 2 , 8 ) = C( 2 , 8 ) + A( 17 ) * B( 34 )
      C( 2 , 8 ) = C( 2 , 8 ) + A( 23 ) * B( 35 )
      C( 2 , 8 ) = C( 2 , 8 ) + A( 30 ) * B( 36 )
      C( 2 , 8 ) = C( 2 , 8 ) + A( 38 ) * B( 44 )
!
      C( 2 , 9 ) = A( 2 ) * B( 37 )
      C( 2 , 9 ) = C( 2 , 9 ) + A( 3 ) * B( 38 )
      C( 2 , 9 ) = C( 2 , 9 ) + A( 5 ) * B( 39 )
      C( 2 , 9 ) = C( 2 , 9 ) + A( 8 ) * B( 40 )
      C( 2 , 9 ) = C( 2 , 9 ) + A( 12 ) * B( 41 )
      C( 2 , 9 ) = C( 2 , 9 ) + A( 17 ) * B( 42 )
      C( 2 , 9 ) = C( 2 , 9 ) + A( 23 ) * B( 43 )
      C( 2 , 9 ) = C( 2 , 9 ) + A( 30 ) * B( 44 )
      C( 2 , 9 ) = C( 2 , 9 ) + A( 38 ) * B( 45 )
!
      C( 3 , 1 ) = A( 4 ) * B( 1 )
      C( 3 , 1 ) = C( 3 , 1 ) + A( 5 ) * B( 2 )
      C( 3 , 1 ) = C( 3 , 1 ) + A( 6 ) * B( 4 )
      C( 3 , 1 ) = C( 3 , 1 ) + A( 9 ) * B( 7 )
      C( 3 , 1 ) = C( 3 , 1 ) + A( 13 ) * B( 11 )
      C( 3 , 1 ) = C( 3 , 1 ) + A( 18 ) * B( 16 )
      C( 3 , 1 ) = C( 3 , 1 ) + A( 24 ) * B( 22 )
      C( 3 , 1 ) = C( 3 , 1 ) + A( 31 ) * B( 29 )
      C( 3 , 1 ) = C( 3 , 1 ) + A( 39 ) * B( 37 )
!
      C( 3 , 2 ) = A( 4 ) * B( 2 )
      C( 3 , 2 ) = C( 3 , 2 ) + A( 5 ) * B( 3 )
      C( 3 , 2 ) = C( 3 , 2 ) + A( 6 ) * B( 5 )
      C( 3 , 2 ) = C( 3 , 2 ) + A( 9 ) * B( 8 )
      C( 3 , 2 ) = C( 3 , 2 ) + A( 13 ) * B( 12 )
      C( 3 , 2 ) = C( 3 , 2 ) + A( 18 ) * B( 17 )
      C( 3 , 2 ) = C( 3 , 2 ) + A( 24 ) * B( 23 )
      C( 3 , 2 ) = C( 3 , 2 ) + A( 31 ) * B( 30 )
      C( 3 , 2 ) = C( 3 , 2 ) + A( 39 ) * B( 38 )
!
      C( 3 , 3 ) = A( 4 ) * B( 4 )
      C( 3 , 3 ) = C( 3 , 3 ) + A( 5 ) * B( 5 )
      C( 3 , 3 ) = C( 3 , 3 ) + A( 6 ) * B( 6 )
      C( 3 , 3 ) = C( 3 , 3 ) + A( 9 ) * B( 9 )
      C( 3 , 3 ) = C( 3 , 3 ) + A( 13 ) * B( 13 )
      C( 3 , 3 ) = C( 3 , 3 ) + A( 18 ) * B( 18 )
      C( 3 , 3 ) = C( 3 , 3 ) + A( 24 ) * B( 24 )
      C( 3 , 3 ) = C( 3 , 3 ) + A( 31 ) * B( 31 )
      C( 3 , 3 ) = C( 3 , 3 ) + A( 39 ) * B( 39 )
!
      C( 3 , 4 ) = A( 4 ) * B( 7 )
      C( 3 , 4 ) = C( 3 , 4 ) + A( 5 ) * B( 8 )
      C( 3 , 4 ) = C( 3 , 4 ) + A( 6 ) * B( 9 )
      C( 3 , 4 ) = C( 3 , 4 ) + A( 9 ) * B( 10 )
      C( 3 , 4 ) = C( 3 , 4 ) + A( 13 ) * B( 14 )
      C( 3 , 4 ) = C( 3 , 4 ) + A( 18 ) * B( 19 )
      C( 3 , 4 ) = C( 3 , 4 ) + A( 24 ) * B( 25 )
      C( 3 , 4 ) = C( 3 , 4 ) + A( 31 ) * B( 32 )
      C( 3 , 4 ) = C( 3 , 4 ) + A( 39 ) * B( 40 )
!
      C( 3 , 5 ) = A( 4 ) * B( 11 )
      C( 3 , 5 ) = C( 3 , 5 ) + A( 5 ) * B( 12 )
      C( 3 , 5 ) = C( 3 , 5 ) + A( 6 ) * B( 13 )
      C( 3 , 5 ) = C( 3 , 5 ) + A( 9 ) * B( 14 )
      C( 3 , 5 ) = C( 3 , 5 ) + A( 13 ) * B( 15 )
      C( 3 , 5 ) = C( 3 , 5 ) + A( 18 ) * B( 20 )
      C( 3 , 5 ) = C( 3 , 5 ) + A( 24 ) * B( 26 )
      C( 3 , 5 ) = C( 3 , 5 ) + A( 31 ) * B( 33 )
      C( 3 , 5 ) = C( 3 , 5 ) + A( 39 ) * B( 41 )
!
      C( 3 , 6 ) = A( 4 ) * B( 16 )
      C( 3 , 6 ) = C( 3 , 6 ) + A( 5 ) * B( 17 )
      C( 3 , 6 ) = C( 3 , 6 ) + A( 6 ) * B( 18 )
      C( 3 , 6 ) = C( 3 , 6 ) + A( 9 ) * B( 19 )
      C( 3 , 6 ) = C( 3 , 6 ) + A( 13 ) * B( 20 )
      C( 3 , 6 ) = C( 3 , 6 ) + A( 18 ) * B( 21 )
      C( 3 , 6 ) = C( 3 , 6 ) + A( 24 ) * B( 27 )
      C( 3 , 6 ) = C( 3 , 6 ) + A( 31 ) * B( 34 )
      C( 3 , 6 ) = C( 3 , 6 ) + A( 39 ) * B( 42 )
!
      C( 3 , 7 ) = A( 4 ) * B( 22 )
      C( 3 , 7 ) = C( 3 , 7 ) + A( 5 ) * B( 23 )
      C( 3 , 7 ) = C( 3 , 7 ) + A( 6 ) * B( 24 )
      C( 3 , 7 ) = C( 3 , 7 ) + A( 9 ) * B( 25 )
      C( 3 , 7 ) = C( 3 , 7 ) + A( 13 ) * B( 26 )
      C( 3 , 7 ) = C( 3 , 7 ) + A( 18 ) * B( 27 )
      C( 3 , 7 ) = C( 3 , 7 ) + A( 24 ) * B( 28 )
      C( 3 , 7 ) = C( 3 , 7 ) + A( 31 ) * B( 35 )
      C( 3 , 7 ) = C( 3 , 7 ) + A( 39 ) * B( 43 )
!
      C( 3 , 8 ) = A( 4 ) * B( 29 )
      C( 3 , 8 ) = C( 3 , 8 ) + A( 5 ) * B( 30 )
      C( 3 , 8 ) = C( 3 , 8 ) + A( 6 ) * B( 31 )
      C( 3 , 8 ) = C( 3 , 8 ) + A( 9 ) * B( 32 )
      C( 3 , 8 ) = C( 3 , 8 ) + A( 13 ) * B( 33 )
      C( 3 , 8 ) = C( 3 , 8 ) + A( 18 ) * B( 34 )
      C( 3 , 8 ) = C( 3 , 8 ) + A( 24 ) * B( 35 )
      C( 3 , 8 ) = C( 3 , 8 ) + A( 31 ) * B( 36 )
      C( 3 , 8 ) = C( 3 , 8 ) + A( 39 ) * B( 44 )
!
      C( 3 , 9 ) = A( 4 ) * B( 37 )
      C( 3 , 9 ) = C( 3 , 9 ) + A( 5 ) * B( 38 )
      C( 3 , 9 ) = C( 3 , 9 ) + A( 6 ) * B( 39 )
      C( 3 , 9 ) = C( 3 , 9 ) + A( 9 ) * B( 40 )
      C( 3 , 9 ) = C( 3 , 9 ) + A( 13 ) * B( 41 )
      C( 3 , 9 ) = C( 3 , 9 ) + A( 18 ) * B( 42 )
      C( 3 , 9 ) = C( 3 , 9 ) + A( 24 ) * B( 43 )
      C( 3 , 9 ) = C( 3 , 9 ) + A( 31 ) * B( 44 )
      C( 3 , 9 ) = C( 3 , 9 ) + A( 39 ) * B( 45 )
!
      C( 4 , 1 ) = A( 7 ) * B( 1 )
      C( 4 , 1 ) = C( 4 , 1 ) + A( 8 ) * B( 2 )
      C( 4 , 1 ) = C( 4 , 1 ) + A( 9 ) * B( 4 )
      C( 4 , 1 ) = C( 4 , 1 ) + A( 10 ) * B( 7 )
      C( 4 , 1 ) = C( 4 , 1 ) + A( 14 ) * B( 11 )
      C( 4 , 1 ) = C( 4 , 1 ) + A( 19 ) * B( 16 )
      C( 4 , 1 ) = C( 4 , 1 ) + A( 25 ) * B( 22 )
      C( 4 , 1 ) = C( 4 , 1 ) + A( 32 ) * B( 29 )
      C( 4 , 1 ) = C( 4 , 1 ) + A( 40 ) * B( 37 )
!
      C( 4 , 2 ) = A( 7 ) * B( 2 )
      C( 4 , 2 ) = C( 4 , 2 ) + A( 8 ) * B( 3 )
      C( 4 , 2 ) = C( 4 , 2 ) + A( 9 ) * B( 5 )
      C( 4 , 2 ) = C( 4 , 2 ) + A( 10 ) * B( 8 )
      C( 4 , 2 ) = C( 4 , 2 ) + A( 14 ) * B( 12 )
      C( 4 , 2 ) = C( 4 , 2 ) + A( 19 ) * B( 17 )
      C( 4 , 2 ) = C( 4 , 2 ) + A( 25 ) * B( 23 )
      C( 4 , 2 ) = C( 4 , 2 ) + A( 32 ) * B( 30 )
      C( 4 , 2 ) = C( 4 , 2 ) + A( 40 ) * B( 38 )
!
      C( 4 , 3 ) = A( 7 ) * B( 4 )
      C( 4 , 3 ) = C( 4 , 3 ) + A( 8 ) * B( 5 )
      C( 4 , 3 ) = C( 4 , 3 ) + A( 9 ) * B( 6 )
      C( 4 , 3 ) = C( 4 , 3 ) + A( 10 ) * B( 9 )
      C( 4 , 3 ) = C( 4 , 3 ) + A( 14 ) * B( 13 )
      C( 4 , 3 ) = C( 4 , 3 ) + A( 19 ) * B( 18 )
      C( 4 , 3 ) = C( 4 , 3 ) + A( 25 ) * B( 24 )
      C( 4 , 3 ) = C( 4 , 3 ) + A( 32 ) * B( 31 )
      C( 4 , 3 ) = C( 4 , 3 ) + A( 40 ) * B( 39 )
!
      C( 4 , 4 ) = A( 7 ) * B( 7 )
      C( 4 , 4 ) = C( 4 , 4 ) + A( 8 ) * B( 8 )
      C( 4 , 4 ) = C( 4 , 4 ) + A( 9 ) * B( 9 )
      C( 4 , 4 ) = C( 4 , 4 ) + A( 10 ) * B( 10 )
      C( 4 , 4 ) = C( 4 , 4 ) + A( 14 ) * B( 14 )
      C( 4 , 4 ) = C( 4 , 4 ) + A( 19 ) * B( 19 )
      C( 4 , 4 ) = C( 4 , 4 ) + A( 25 ) * B( 25 )
      C( 4 , 4 ) = C( 4 , 4 ) + A( 32 ) * B( 32 )
      C( 4 , 4 ) = C( 4 , 4 ) + A( 40 ) * B( 40 )
!
      C( 4 , 5 ) = A( 7 ) * B( 11 )
      C( 4 , 5 ) = C( 4 , 5 ) + A( 8 ) * B( 12 )
      C( 4 , 5 ) = C( 4 , 5 ) + A( 9 ) * B( 13 )
      C( 4 , 5 ) = C( 4 , 5 ) + A( 10 ) * B( 14 )
      C( 4 , 5 ) = C( 4 , 5 ) + A( 14 ) * B( 15 )
      C( 4 , 5 ) = C( 4 , 5 ) + A( 19 ) * B( 20 )
      C( 4 , 5 ) = C( 4 , 5 ) + A( 25 ) * B( 26 )
      C( 4 , 5 ) = C( 4 , 5 ) + A( 32 ) * B( 33 )
      C( 4 , 5 ) = C( 4 , 5 ) + A( 40 ) * B( 41 )
!
      C( 4 , 6 ) = A( 7 ) * B( 16 )
      C( 4 , 6 ) = C( 4 , 6 ) + A( 8 ) * B( 17 )
      C( 4 , 6 ) = C( 4 , 6 ) + A( 9 ) * B( 18 )
      C( 4 , 6 ) = C( 4 , 6 ) + A( 10 ) * B( 19 )
      C( 4 , 6 ) = C( 4 , 6 ) + A( 14 ) * B( 20 )
      C( 4 , 6 ) = C( 4 , 6 ) + A( 19 ) * B( 21 )
      C( 4 , 6 ) = C( 4 , 6 ) + A( 25 ) * B( 27 )
      C( 4 , 6 ) = C( 4 , 6 ) + A( 32 ) * B( 34 )
      C( 4 , 6 ) = C( 4 , 6 ) + A( 40 ) * B( 42 )
!
      C( 4 , 7 ) = A( 7 ) * B( 22 )
      C( 4 , 7 ) = C( 4 , 7 ) + A( 8 ) * B( 23 )
      C( 4 , 7 ) = C( 4 , 7 ) + A( 9 ) * B( 24 )
      C( 4 , 7 ) = C( 4 , 7 ) + A( 10 ) * B( 25 )
      C( 4 , 7 ) = C( 4 , 7 ) + A( 14 ) * B( 26 )
      C( 4 , 7 ) = C( 4 , 7 ) + A( 19 ) * B( 27 )
      C( 4 , 7 ) = C( 4 , 7 ) + A( 25 ) * B( 28 )
      C( 4 , 7 ) = C( 4 , 7 ) + A( 32 ) * B( 35 )
      C( 4 , 7 ) = C( 4 , 7 ) + A( 40 ) * B( 43 )
!
      C( 4 , 8 ) = A( 7 ) * B( 29 )
      C( 4 , 8 ) = C( 4 , 8 ) + A( 8 ) * B( 30 )
      C( 4 , 8 ) = C( 4 , 8 ) + A( 9 ) * B( 31 )
      C( 4 , 8 ) = C( 4 , 8 ) + A( 10 ) * B( 32 )
      C( 4 , 8 ) = C( 4 , 8 ) + A( 14 ) * B( 33 )
      C( 4 , 8 ) = C( 4 , 8 ) + A( 19 ) * B( 34 )
      C( 4 , 8 ) = C( 4 , 8 ) + A( 25 ) * B( 35 )
      C( 4 , 8 ) = C( 4 , 8 ) + A( 32 ) * B( 36 )
      C( 4 , 8 ) = C( 4 , 8 ) + A( 40 ) * B( 44 )
!
      C( 4 , 9 ) = A( 7 ) * B( 37 )
      C( 4 , 9 ) = C( 4 , 9 ) + A( 8 ) * B( 38 )
      C( 4 , 9 ) = C( 4 , 9 ) + A( 9 ) * B( 39 )
      C( 4 , 9 ) = C( 4 , 9 ) + A( 10 ) * B( 40 )
      C( 4 , 9 ) = C( 4 , 9 ) + A( 14 ) * B( 41 )
      C( 4 , 9 ) = C( 4 , 9 ) + A( 19 ) * B( 42 )
      C( 4 , 9 ) = C( 4 , 9 ) + A( 25 ) * B( 43 )
      C( 4 , 9 ) = C( 4 , 9 ) + A( 32 ) * B( 44 )
      C( 4 , 9 ) = C( 4 , 9 ) + A( 40 ) * B( 45 )
!
      C( 5 , 1 ) = A( 11 ) * B( 1 )
      C( 5 , 1 ) = C( 5 , 1 ) + A( 12 ) * B( 2 )
      C( 5 , 1 ) = C( 5 , 1 ) + A( 13 ) * B( 4 )
      C( 5 , 1 ) = C( 5 , 1 ) + A( 14 ) * B( 7 )
      C( 5 , 1 ) = C( 5 , 1 ) + A( 15 ) * B( 11 )
      C( 5 , 1 ) = C( 5 , 1 ) + A( 20 ) * B( 16 )
      C( 5 , 1 ) = C( 5 , 1 ) + A( 26 ) * B( 22 )
      C( 5 , 1 ) = C( 5 , 1 ) + A( 33 ) * B( 29 )
      C( 5 , 1 ) = C( 5 , 1 ) + A( 41 ) * B( 37 )
!
      C( 5 , 2 ) = A( 11 ) * B( 2 )
      C( 5 , 2 ) = C( 5 , 2 ) + A( 12 ) * B( 3 )
      C( 5 , 2 ) = C( 5 , 2 ) + A( 13 ) * B( 5 )
      C( 5 , 2 ) = C( 5 , 2 ) + A( 14 ) * B( 8 )
      C( 5 , 2 ) = C( 5 , 2 ) + A( 15 ) * B( 12 )
      C( 5 , 2 ) = C( 5 , 2 ) + A( 20 ) * B( 17 )
      C( 5 , 2 ) = C( 5 , 2 ) + A( 26 ) * B( 23 )
      C( 5 , 2 ) = C( 5 , 2 ) + A( 33 ) * B( 30 )
      C( 5 , 2 ) = C( 5 , 2 ) + A( 41 ) * B( 38 )
!
      C( 5 , 3 ) = A( 11 ) * B( 4 )
      C( 5 , 3 ) = C( 5 , 3 ) + A( 12 ) * B( 5 )
      C( 5 , 3 ) = C( 5 , 3 ) + A( 13 ) * B( 6 )
      C( 5 , 3 ) = C( 5 , 3 ) + A( 14 ) * B( 9 )
      C( 5 , 3 ) = C( 5 , 3 ) + A( 15 ) * B( 13 )
      C( 5 , 3 ) = C( 5 , 3 ) + A( 20 ) * B( 18 )
      C( 5 , 3 ) = C( 5 , 3 ) + A( 26 ) * B( 24 )
      C( 5 , 3 ) = C( 5 , 3 ) + A( 33 ) * B( 31 )
      C( 5 , 3 ) = C( 5 , 3 ) + A( 41 ) * B( 39 )
!
      C( 5 , 4 ) = A( 11 ) * B( 7 )
      C( 5 , 4 ) = C( 5 , 4 ) + A( 12 ) * B( 8 )
      C( 5 , 4 ) = C( 5 , 4 ) + A( 13 ) * B( 9 )
      C( 5 , 4 ) = C( 5 , 4 ) + A( 14 ) * B( 10 )
      C( 5 , 4 ) = C( 5 , 4 ) + A( 15 ) * B( 14 )
      C( 5 , 4 ) = C( 5 , 4 ) + A( 20 ) * B( 19 )
      C( 5 , 4 ) = C( 5 , 4 ) + A( 26 ) * B( 25 )
      C( 5 , 4 ) = C( 5 , 4 ) + A( 33 ) * B( 32 )
      C( 5 , 4 ) = C( 5 , 4 ) + A( 41 ) * B( 40 )
!
      C( 5 , 5 ) = A( 11 ) * B( 11 )
      C( 5 , 5 ) = C( 5 , 5 ) + A( 12 ) * B( 12 )
      C( 5 , 5 ) = C( 5 , 5 ) + A( 13 ) * B( 13 )
      C( 5 , 5 ) = C( 5 , 5 ) + A( 14 ) * B( 14 )
      C( 5 , 5 ) = C( 5 , 5 ) + A( 15 ) * B( 15 )
      C( 5 , 5 ) = C( 5 , 5 ) + A( 20 ) * B( 20 )
      C( 5 , 5 ) = C( 5 , 5 ) + A( 26 ) * B( 26 )
      C( 5 , 5 ) = C( 5 , 5 ) + A( 33 ) * B( 33 )
      C( 5 , 5 ) = C( 5 , 5 ) + A( 41 ) * B( 41 )
!
      C( 5 , 6 ) = A( 11 ) * B( 16 )
      C( 5 , 6 ) = C( 5 , 6 ) + A( 12 ) * B( 17 )
      C( 5 , 6 ) = C( 5 , 6 ) + A( 13 ) * B( 18 )
      C( 5 , 6 ) = C( 5 , 6 ) + A( 14 ) * B( 19 )
      C( 5 , 6 ) = C( 5 , 6 ) + A( 15 ) * B( 20 )
      C( 5 , 6 ) = C( 5 , 6 ) + A( 20 ) * B( 21 )
      C( 5 , 6 ) = C( 5 , 6 ) + A( 26 ) * B( 27 )
      C( 5 , 6 ) = C( 5 , 6 ) + A( 33 ) * B( 34 )
      C( 5 , 6 ) = C( 5 , 6 ) + A( 41 ) * B( 42 )
!
      C( 5 , 7 ) = A( 11 ) * B( 22 )
      C( 5 , 7 ) = C( 5 , 7 ) + A( 12 ) * B( 23 )
      C( 5 , 7 ) = C( 5 , 7 ) + A( 13 ) * B( 24 )
      C( 5 , 7 ) = C( 5 , 7 ) + A( 14 ) * B( 25 )
      C( 5 , 7 ) = C( 5 , 7 ) + A( 15 ) * B( 26 )
      C( 5 , 7 ) = C( 5 , 7 ) + A( 20 ) * B( 27 )
      C( 5 , 7 ) = C( 5 , 7 ) + A( 26 ) * B( 28 )
      C( 5 , 7 ) = C( 5 , 7 ) + A( 33 ) * B( 35 )
      C( 5 , 7 ) = C( 5 , 7 ) + A( 41 ) * B( 43 )
!
      C( 5 , 8 ) = A( 11 ) * B( 29 )
      C( 5 , 8 ) = C( 5 , 8 ) + A( 12 ) * B( 30 )
      C( 5 , 8 ) = C( 5 , 8 ) + A( 13 ) * B( 31 )
      C( 5 , 8 ) = C( 5 , 8 ) + A( 14 ) * B( 32 )
      C( 5 , 8 ) = C( 5 , 8 ) + A( 15 ) * B( 33 )
      C( 5 , 8 ) = C( 5 , 8 ) + A( 20 ) * B( 34 )
      C( 5 , 8 ) = C( 5 , 8 ) + A( 26 ) * B( 35 )
      C( 5 , 8 ) = C( 5 , 8 ) + A( 33 ) * B( 36 )
      C( 5 , 8 ) = C( 5 , 8 ) + A( 41 ) * B( 44 )
!
      C( 5 , 9 ) = A( 11 ) * B( 37 )
      C( 5 , 9 ) = C( 5 , 9 ) + A( 12 ) * B( 38 )
      C( 5 , 9 ) = C( 5 , 9 ) + A( 13 ) * B( 39 )
      C( 5 , 9 ) = C( 5 , 9 ) + A( 14 ) * B( 40 )
      C( 5 , 9 ) = C( 5 , 9 ) + A( 15 ) * B( 41 )
      C( 5 , 9 ) = C( 5 , 9 ) + A( 20 ) * B( 42 )
      C( 5 , 9 ) = C( 5 , 9 ) + A( 26 ) * B( 43 )
      C( 5 , 9 ) = C( 5 , 9 ) + A( 33 ) * B( 44 )
      C( 5 , 9 ) = C( 5 , 9 ) + A( 41 ) * B( 45 )
!
      C( 6 , 1 ) = A( 16 ) * B( 1 )
      C( 6 , 1 ) = C( 6 , 1 ) + A( 17 ) * B( 2 )
      C( 6 , 1 ) = C( 6 , 1 ) + A( 18 ) * B( 4 )
      C( 6 , 1 ) = C( 6 , 1 ) + A( 19 ) * B( 7 )
      C( 6 , 1 ) = C( 6 , 1 ) + A( 20 ) * B( 11 )
      C( 6 , 1 ) = C( 6 , 1 ) + A( 21 ) * B( 16 )
      C( 6 , 1 ) = C( 6 , 1 ) + A( 27 ) * B( 22 )
      C( 6 , 1 ) = C( 6 , 1 ) + A( 34 ) * B( 29 )
      C( 6 , 1 ) = C( 6 , 1 ) + A( 42 ) * B( 37 )
!
      C( 6 , 2 ) = A( 16 ) * B( 2 )
      C( 6 , 2 ) = C( 6 , 2 ) + A( 17 ) * B( 3 )
      C( 6 , 2 ) = C( 6 , 2 ) + A( 18 ) * B( 5 )
      C( 6 , 2 ) = C( 6 , 2 ) + A( 19 ) * B( 8 )
      C( 6 , 2 ) = C( 6 , 2 ) + A( 20 ) * B( 12 )
      C( 6 , 2 ) = C( 6 , 2 ) + A( 21 ) * B( 17 )
      C( 6 , 2 ) = C( 6 , 2 ) + A( 27 ) * B( 23 )
      C( 6 , 2 ) = C( 6 , 2 ) + A( 34 ) * B( 30 )
      C( 6 , 2 ) = C( 6 , 2 ) + A( 42 ) * B( 38 )
!
      C( 6 , 3 ) = A( 16 ) * B( 4 )
      C( 6 , 3 ) = C( 6 , 3 ) + A( 17 ) * B( 5 )
      C( 6 , 3 ) = C( 6 , 3 ) + A( 18 ) * B( 6 )
      C( 6 , 3 ) = C( 6 , 3 ) + A( 19 ) * B( 9 )
      C( 6 , 3 ) = C( 6 , 3 ) + A( 20 ) * B( 13 )
      C( 6 , 3 ) = C( 6 , 3 ) + A( 21 ) * B( 18 )
      C( 6 , 3 ) = C( 6 , 3 ) + A( 27 ) * B( 24 )
      C( 6 , 3 ) = C( 6 , 3 ) + A( 34 ) * B( 31 )
      C( 6 , 3 ) = C( 6 , 3 ) + A( 42 ) * B( 39 )
!
      C( 6 , 4 ) = A( 16 ) * B( 7 )
      C( 6 , 4 ) = C( 6 , 4 ) + A( 17 ) * B( 8 )
      C( 6 , 4 ) = C( 6 , 4 ) + A( 18 ) * B( 9 )
      C( 6 , 4 ) = C( 6 , 4 ) + A( 19 ) * B( 10 )
      C( 6 , 4 ) = C( 6 , 4 ) + A( 20 ) * B( 14 )
      C( 6 , 4 ) = C( 6 , 4 ) + A( 21 ) * B( 19 )
      C( 6 , 4 ) = C( 6 , 4 ) + A( 27 ) * B( 25 )
      C( 6 , 4 ) = C( 6 , 4 ) + A( 34 ) * B( 32 )
      C( 6 , 4 ) = C( 6 , 4 ) + A( 42 ) * B( 40 )
!
      C( 6 , 5 ) = A( 16 ) * B( 11 )
      C( 6 , 5 ) = C( 6 , 5 ) + A( 17 ) * B( 12 )
      C( 6 , 5 ) = C( 6 , 5 ) + A( 18 ) * B( 13 )
      C( 6 , 5 ) = C( 6 , 5 ) + A( 19 ) * B( 14 )
      C( 6 , 5 ) = C( 6 , 5 ) + A( 20 ) * B( 15 )
      C( 6 , 5 ) = C( 6 , 5 ) + A( 21 ) * B( 20 )
      C( 6 , 5 ) = C( 6 , 5 ) + A( 27 ) * B( 26 )
      C( 6 , 5 ) = C( 6 , 5 ) + A( 34 ) * B( 33 )
      C( 6 , 5 ) = C( 6 , 5 ) + A( 42 ) * B( 41 )
!
      C( 6 , 6 ) = A( 16 ) * B( 16 )
      C( 6 , 6 ) = C( 6 , 6 ) + A( 17 ) * B( 17 )
      C( 6 , 6 ) = C( 6 , 6 ) + A( 18 ) * B( 18 )
      C( 6 , 6 ) = C( 6 , 6 ) + A( 19 ) * B( 19 )
      C( 6 , 6 ) = C( 6 , 6 ) + A( 20 ) * B( 20 )
      C( 6 , 6 ) = C( 6 , 6 ) + A( 21 ) * B( 21 )
      C( 6 , 6 ) = C( 6 , 6 ) + A( 27 ) * B( 27 )
      C( 6 , 6 ) = C( 6 , 6 ) + A( 34 ) * B( 34 )
      C( 6 , 6 ) = C( 6 , 6 ) + A( 42 ) * B( 42 )
!
      C( 6 , 7 ) = A( 16 ) * B( 22 )
      C( 6 , 7 ) = C( 6 , 7 ) + A( 17 ) * B( 23 )
      C( 6 , 7 ) = C( 6 , 7 ) + A( 18 ) * B( 24 )
      C( 6 , 7 ) = C( 6 , 7 ) + A( 19 ) * B( 25 )
      C( 6 , 7 ) = C( 6 , 7 ) + A( 20 ) * B( 26 )
      C( 6 , 7 ) = C( 6 , 7 ) + A( 21 ) * B( 27 )
      C( 6 , 7 ) = C( 6 , 7 ) + A( 27 ) * B( 28 )
      C( 6 , 7 ) = C( 6 , 7 ) + A( 34 ) * B( 35 )
      C( 6 , 7 ) = C( 6 , 7 ) + A( 42 ) * B( 43 )
!
      C( 6 , 8 ) = A( 16 ) * B( 29 )
      C( 6 , 8 ) = C( 6 , 8 ) + A( 17 ) * B( 30 )
      C( 6 , 8 ) = C( 6 , 8 ) + A( 18 ) * B( 31 )
      C( 6 , 8 ) = C( 6 , 8 ) + A( 19 ) * B( 32 )
      C( 6 , 8 ) = C( 6 , 8 ) + A( 20 ) * B( 33 )
      C( 6 , 8 ) = C( 6 , 8 ) + A( 21 ) * B( 34 )
      C( 6 , 8 ) = C( 6 , 8 ) + A( 27 ) * B( 35 )
      C( 6 , 8 ) = C( 6 , 8 ) + A( 34 ) * B( 36 )
      C( 6 , 8 ) = C( 6 , 8 ) + A( 42 ) * B( 44 )
!
      C( 6 , 9 ) = A( 16 ) * B( 37 )
      C( 6 , 9 ) = C( 6 , 9 ) + A( 17 ) * B( 38 )
      C( 6 , 9 ) = C( 6 , 9 ) + A( 18 ) * B( 39 )
      C( 6 , 9 ) = C( 6 , 9 ) + A( 19 ) * B( 40 )
      C( 6 , 9 ) = C( 6 , 9 ) + A( 20 ) * B( 41 )
      C( 6 , 9 ) = C( 6 , 9 ) + A( 21 ) * B( 42 )
      C( 6 , 9 ) = C( 6 , 9 ) + A( 27 ) * B( 43 )
      C( 6 , 9 ) = C( 6 , 9 ) + A( 34 ) * B( 44 )
      C( 6 , 9 ) = C( 6 , 9 ) + A( 42 ) * B( 45 )
!
      C( 7 , 1 ) = A( 22 ) * B( 1 )
      C( 7 , 1 ) = C( 7 , 1 ) + A( 23 ) * B( 2 )
      C( 7 , 1 ) = C( 7 , 1 ) + A( 24 ) * B( 4 )
      C( 7 , 1 ) = C( 7 , 1 ) + A( 25 ) * B( 7 )
      C( 7 , 1 ) = C( 7 , 1 ) + A( 26 ) * B( 11 )
      C( 7 , 1 ) = C( 7 , 1 ) + A( 27 ) * B( 16 )
      C( 7 , 1 ) = C( 7 , 1 ) + A( 28 ) * B( 22 )
      C( 7 , 1 ) = C( 7 , 1 ) + A( 35 ) * B( 29 )
      C( 7 , 1 ) = C( 7 , 1 ) + A( 43 ) * B( 37 )
!
      C( 7 , 2 ) = A( 22 ) * B( 2 )
      C( 7 , 2 ) = C( 7 , 2 ) + A( 23 ) * B( 3 )
      C( 7 , 2 ) = C( 7 , 2 ) + A( 24 ) * B( 5 )
      C( 7 , 2 ) = C( 7 , 2 ) + A( 25 ) * B( 8 )
      C( 7 , 2 ) = C( 7 , 2 ) + A( 26 ) * B( 12 )
      C( 7 , 2 ) = C( 7 , 2 ) + A( 27 ) * B( 17 )
      C( 7 , 2 ) = C( 7 , 2 ) + A( 28 ) * B( 23 )
      C( 7 , 2 ) = C( 7 , 2 ) + A( 35 ) * B( 30 )
      C( 7 , 2 ) = C( 7 , 2 ) + A( 43 ) * B( 38 )
!
      C( 7 , 3 ) = A( 22 ) * B( 4 )
      C( 7 , 3 ) = C( 7 , 3 ) + A( 23 ) * B( 5 )
      C( 7 , 3 ) = C( 7 , 3 ) + A( 24 ) * B( 6 )
      C( 7 , 3 ) = C( 7 , 3 ) + A( 25 ) * B( 9 )
      C( 7 , 3 ) = C( 7 , 3 ) + A( 26 ) * B( 13 )
      C( 7 , 3 ) = C( 7 , 3 ) + A( 27 ) * B( 18 )
      C( 7 , 3 ) = C( 7 , 3 ) + A( 28 ) * B( 24 )
      C( 7 , 3 ) = C( 7 , 3 ) + A( 35 ) * B( 31 )
      C( 7 , 3 ) = C( 7 , 3 ) + A( 43 ) * B( 39 )
!
      C( 7 , 4 ) = A( 22 ) * B( 7 )
      C( 7 , 4 ) = C( 7 , 4 ) + A( 23 ) * B( 8 )
      C( 7 , 4 ) = C( 7 , 4 ) + A( 24 ) * B( 9 )
      C( 7 , 4 ) = C( 7 , 4 ) + A( 25 ) * B( 10 )
      C( 7 , 4 ) = C( 7 , 4 ) + A( 26 ) * B( 14 )
      C( 7 , 4 ) = C( 7 , 4 ) + A( 27 ) * B( 19 )
      C( 7 , 4 ) = C( 7 , 4 ) + A( 28 ) * B( 25 )
      C( 7 , 4 ) = C( 7 , 4 ) + A( 35 ) * B( 32 )
      C( 7 , 4 ) = C( 7 , 4 ) + A( 43 ) * B( 40 )
!
      C( 7 , 5 ) = A( 22 ) * B( 11 )
      C( 7 , 5 ) = C( 7 , 5 ) + A( 23 ) * B( 12 )
      C( 7 , 5 ) = C( 7 , 5 ) + A( 24 ) * B( 13 )
      C( 7 , 5 ) = C( 7 , 5 ) + A( 25 ) * B( 14 )
      C( 7 , 5 ) = C( 7 , 5 ) + A( 26 ) * B( 15 )
      C( 7 , 5 ) = C( 7 , 5 ) + A( 27 ) * B( 20 )
      C( 7 , 5 ) = C( 7 , 5 ) + A( 28 ) * B( 26 )
      C( 7 , 5 ) = C( 7 , 5 ) + A( 35 ) * B( 33 )
      C( 7 , 5 ) = C( 7 , 5 ) + A( 43 ) * B( 41 )
!
      C( 7 , 6 ) = A( 22 ) * B( 16 )
      C( 7 , 6 ) = C( 7 , 6 ) + A( 23 ) * B( 17 )
      C( 7 , 6 ) = C( 7 , 6 ) + A( 24 ) * B( 18 )
      C( 7 , 6 ) = C( 7 , 6 ) + A( 25 ) * B( 19 )
      C( 7 , 6 ) = C( 7 , 6 ) + A( 26 ) * B( 20 )
      C( 7 , 6 ) = C( 7 , 6 ) + A( 27 ) * B( 21 )
      C( 7 , 6 ) = C( 7 , 6 ) + A( 28 ) * B( 27 )
      C( 7 , 6 ) = C( 7 , 6 ) + A( 35 ) * B( 34 )
      C( 7 , 6 ) = C( 7 , 6 ) + A( 43 ) * B( 42 )
!
      C( 7 , 7 ) = A( 22 ) * B( 22 )
      C( 7 , 7 ) = C( 7 , 7 ) + A( 23 ) * B( 23 )
      C( 7 , 7 ) = C( 7 , 7 ) + A( 24 ) * B( 24 )
      C( 7 , 7 ) = C( 7 , 7 ) + A( 25 ) * B( 25 )
      C( 7 , 7 ) = C( 7 , 7 ) + A( 26 ) * B( 26 )
      C( 7 , 7 ) = C( 7 , 7 ) + A( 27 ) * B( 27 )
      C( 7 , 7 ) = C( 7 , 7 ) + A( 28 ) * B( 28 )
      C( 7 , 7 ) = C( 7 , 7 ) + A( 35 ) * B( 35 )
      C( 7 , 7 ) = C( 7 , 7 ) + A( 43 ) * B( 43 )
!
      C( 7 , 8 ) = A( 22 ) * B( 29 )
      C( 7 , 8 ) = C( 7 , 8 ) + A( 23 ) * B( 30 )
      C( 7 , 8 ) = C( 7 , 8 ) + A( 24 ) * B( 31 )
      C( 7 , 8 ) = C( 7 , 8 ) + A( 25 ) * B( 32 )
      C( 7 , 8 ) = C( 7 , 8 ) + A( 26 ) * B( 33 )
      C( 7 , 8 ) = C( 7 , 8 ) + A( 27 ) * B( 34 )
      C( 7 , 8 ) = C( 7 , 8 ) + A( 28 ) * B( 35 )
      C( 7 , 8 ) = C( 7 , 8 ) + A( 35 ) * B( 36 )
      C( 7 , 8 ) = C( 7 , 8 ) + A( 43 ) * B( 44 )
!
      C( 7 , 9 ) = A( 22 ) * B( 37 )
      C( 7 , 9 ) = C( 7 , 9 ) + A( 23 ) * B( 38 )
      C( 7 , 9 ) = C( 7 , 9 ) + A( 24 ) * B( 39 )
      C( 7 , 9 ) = C( 7 , 9 ) + A( 25 ) * B( 40 )
      C( 7 , 9 ) = C( 7 , 9 ) + A( 26 ) * B( 41 )
      C( 7 , 9 ) = C( 7 , 9 ) + A( 27 ) * B( 42 )
      C( 7 , 9 ) = C( 7 , 9 ) + A( 28 ) * B( 43 )
      C( 7 , 9 ) = C( 7 , 9 ) + A( 35 ) * B( 44 )
      C( 7 , 9 ) = C( 7 , 9 ) + A( 43 ) * B( 45 )
!
      C( 8 , 1 ) = A( 29 ) * B( 1 )
      C( 8 , 1 ) = C( 8 , 1 ) + A( 30 ) * B( 2 )
      C( 8 , 1 ) = C( 8 , 1 ) + A( 31 ) * B( 4 )
      C( 8 , 1 ) = C( 8 , 1 ) + A( 32 ) * B( 7 )
      C( 8 , 1 ) = C( 8 , 1 ) + A( 33 ) * B( 11 )
      C( 8 , 1 ) = C( 8 , 1 ) + A( 34 ) * B( 16 )
      C( 8 , 1 ) = C( 8 , 1 ) + A( 35 ) * B( 22 )
      C( 8 , 1 ) = C( 8 , 1 ) + A( 36 ) * B( 29 )
      C( 8 , 1 ) = C( 8 , 1 ) + A( 44 ) * B( 37 )
!
      C( 8 , 2 ) = A( 29 ) * B( 2 )
      C( 8 , 2 ) = C( 8 , 2 ) + A( 30 ) * B( 3 )
      C( 8 , 2 ) = C( 8 , 2 ) + A( 31 ) * B( 5 )
      C( 8 , 2 ) = C( 8 , 2 ) + A( 32 ) * B( 8 )
      C( 8 , 2 ) = C( 8 , 2 ) + A( 33 ) * B( 12 )
      C( 8 , 2 ) = C( 8 , 2 ) + A( 34 ) * B( 17 )
      C( 8 , 2 ) = C( 8 , 2 ) + A( 35 ) * B( 23 )
      C( 8 , 2 ) = C( 8 , 2 ) + A( 36 ) * B( 30 )
      C( 8 , 2 ) = C( 8 , 2 ) + A( 44 ) * B( 38 )
!
      C( 8 , 3 ) = A( 29 ) * B( 4 )
      C( 8 , 3 ) = C( 8 , 3 ) + A( 30 ) * B( 5 )
      C( 8 , 3 ) = C( 8 , 3 ) + A( 31 ) * B( 6 )
      C( 8 , 3 ) = C( 8 , 3 ) + A( 32 ) * B( 9 )
      C( 8 , 3 ) = C( 8 , 3 ) + A( 33 ) * B( 13 )
      C( 8 , 3 ) = C( 8 , 3 ) + A( 34 ) * B( 18 )
      C( 8 , 3 ) = C( 8 , 3 ) + A( 35 ) * B( 24 )
      C( 8 , 3 ) = C( 8 , 3 ) + A( 36 ) * B( 31 )
      C( 8 , 3 ) = C( 8 , 3 ) + A( 44 ) * B( 39 )
!
      C( 8 , 4 ) = A( 29 ) * B( 7 )
      C( 8 , 4 ) = C( 8 , 4 ) + A( 30 ) * B( 8 )
      C( 8 , 4 ) = C( 8 , 4 ) + A( 31 ) * B( 9 )
      C( 8 , 4 ) = C( 8 , 4 ) + A( 32 ) * B( 10 )
      C( 8 , 4 ) = C( 8 , 4 ) + A( 33 ) * B( 14 )
      C( 8 , 4 ) = C( 8 , 4 ) + A( 34 ) * B( 19 )
      C( 8 , 4 ) = C( 8 , 4 ) + A( 35 ) * B( 25 )
      C( 8 , 4 ) = C( 8 , 4 ) + A( 36 ) * B( 32 )
      C( 8 , 4 ) = C( 8 , 4 ) + A( 44 ) * B( 40 )
!
      C( 8 , 5 ) = A( 29 ) * B( 11 )
      C( 8 , 5 ) = C( 8 , 5 ) + A( 30 ) * B( 12 )
      C( 8 , 5 ) = C( 8 , 5 ) + A( 31 ) * B( 13 )
      C( 8 , 5 ) = C( 8 , 5 ) + A( 32 ) * B( 14 )
      C( 8 , 5 ) = C( 8 , 5 ) + A( 33 ) * B( 15 )
      C( 8 , 5 ) = C( 8 , 5 ) + A( 34 ) * B( 20 )
      C( 8 , 5 ) = C( 8 , 5 ) + A( 35 ) * B( 26 )
      C( 8 , 5 ) = C( 8 , 5 ) + A( 36 ) * B( 33 )
      C( 8 , 5 ) = C( 8 , 5 ) + A( 44 ) * B( 41 )
!
      C( 8 , 6 ) = A( 29 ) * B( 16 )
      C( 8 , 6 ) = C( 8 , 6 ) + A( 30 ) * B( 17 )
      C( 8 , 6 ) = C( 8 , 6 ) + A( 31 ) * B( 18 )
      C( 8 , 6 ) = C( 8 , 6 ) + A( 32 ) * B( 19 )
      C( 8 , 6 ) = C( 8 , 6 ) + A( 33 ) * B( 20 )
      C( 8 , 6 ) = C( 8 , 6 ) + A( 34 ) * B( 21 )
      C( 8 , 6 ) = C( 8 , 6 ) + A( 35 ) * B( 27 )
      C( 8 , 6 ) = C( 8 , 6 ) + A( 36 ) * B( 34 )
      C( 8 , 6 ) = C( 8 , 6 ) + A( 44 ) * B( 42 )
!
      C( 8 , 7 ) = A( 29 ) * B( 22 )
      C( 8 , 7 ) = C( 8 , 7 ) + A( 30 ) * B( 23 )
      C( 8 , 7 ) = C( 8 , 7 ) + A( 31 ) * B( 24 )
      C( 8 , 7 ) = C( 8 , 7 ) + A( 32 ) * B( 25 )
      C( 8 , 7 ) = C( 8 , 7 ) + A( 33 ) * B( 26 )
      C( 8 , 7 ) = C( 8 , 7 ) + A( 34 ) * B( 27 )
      C( 8 , 7 ) = C( 8 , 7 ) + A( 35 ) * B( 28 )
      C( 8 , 7 ) = C( 8 , 7 ) + A( 36 ) * B( 35 )
      C( 8 , 7 ) = C( 8 , 7 ) + A( 44 ) * B( 43 )
!
      C( 8 , 8 ) = A( 29 ) * B( 29 )
      C( 8 , 8 ) = C( 8 , 8 ) + A( 30 ) * B( 30 )
      C( 8 , 8 ) = C( 8 , 8 ) + A( 31 ) * B( 31 )
      C( 8 , 8 ) = C( 8 , 8 ) + A( 32 ) * B( 32 )
      C( 8 , 8 ) = C( 8 , 8 ) + A( 33 ) * B( 33 )
      C( 8 , 8 ) = C( 8 , 8 ) + A( 34 ) * B( 34 )
      C( 8 , 8 ) = C( 8 , 8 ) + A( 35 ) * B( 35 )
      C( 8 , 8 ) = C( 8 , 8 ) + A( 36 ) * B( 36 )
      C( 8 , 8 ) = C( 8 , 8 ) + A( 44 ) * B( 44 )
!
      C( 8 , 9 ) = A( 29 ) * B( 37 )
      C( 8 , 9 ) = C( 8 , 9 ) + A( 30 ) * B( 38 )
      C( 8 , 9 ) = C( 8 , 9 ) + A( 31 ) * B( 39 )
      C( 8 , 9 ) = C( 8 , 9 ) + A( 32 ) * B( 40 )
      C( 8 , 9 ) = C( 8 , 9 ) + A( 33 ) * B( 41 )
      C( 8 , 9 ) = C( 8 , 9 ) + A( 34 ) * B( 42 )
      C( 8 , 9 ) = C( 8 , 9 ) + A( 35 ) * B( 43 )
      C( 8 , 9 ) = C( 8 , 9 ) + A( 36 ) * B( 44 )
      C( 8 , 9 ) = C( 8 , 9 ) + A( 44 ) * B( 45 )
!
      C( 9 , 1 ) = A( 37 ) * B( 1 )
      C( 9 , 1 ) = C( 9 , 1 ) + A( 38 ) * B( 2 )
      C( 9 , 1 ) = C( 9 , 1 ) + A( 39 ) * B( 4 )
      C( 9 , 1 ) = C( 9 , 1 ) + A( 40 ) * B( 7 )
      C( 9 , 1 ) = C( 9 , 1 ) + A( 41 ) * B( 11 )
      C( 9 , 1 ) = C( 9 , 1 ) + A( 42 ) * B( 16 )
      C( 9 , 1 ) = C( 9 , 1 ) + A( 43 ) * B( 22 )
      C( 9 , 1 ) = C( 9 , 1 ) + A( 44 ) * B( 29 )
      C( 9 , 1 ) = C( 9 , 1 ) + A( 45 ) * B( 37 )
!
      C( 9 , 2 ) = A( 37 ) * B( 2 )
      C( 9 , 2 ) = C( 9 , 2 ) + A( 38 ) * B( 3 )
      C( 9 , 2 ) = C( 9 , 2 ) + A( 39 ) * B( 5 )
      C( 9 , 2 ) = C( 9 , 2 ) + A( 40 ) * B( 8 )
      C( 9 , 2 ) = C( 9 , 2 ) + A( 41 ) * B( 12 )
      C( 9 , 2 ) = C( 9 , 2 ) + A( 42 ) * B( 17 )
      C( 9 , 2 ) = C( 9 , 2 ) + A( 43 ) * B( 23 )
      C( 9 , 2 ) = C( 9 , 2 ) + A( 44 ) * B( 30 )
      C( 9 , 2 ) = C( 9 , 2 ) + A( 45 ) * B( 38 )
!
      C( 9 , 3 ) = A( 37 ) * B( 4 )
      C( 9 , 3 ) = C( 9 , 3 ) + A( 38 ) * B( 5 )
      C( 9 , 3 ) = C( 9 , 3 ) + A( 39 ) * B( 6 )
      C( 9 , 3 ) = C( 9 , 3 ) + A( 40 ) * B( 9 )
      C( 9 , 3 ) = C( 9 , 3 ) + A( 41 ) * B( 13 )
      C( 9 , 3 ) = C( 9 , 3 ) + A( 42 ) * B( 18 )
      C( 9 , 3 ) = C( 9 , 3 ) + A( 43 ) * B( 24 )
      C( 9 , 3 ) = C( 9 , 3 ) + A( 44 ) * B( 31 )
      C( 9 , 3 ) = C( 9 , 3 ) + A( 45 ) * B( 39 )
!
      C( 9 , 4 ) = A( 37 ) * B( 7 )
      C( 9 , 4 ) = C( 9 , 4 ) + A( 38 ) * B( 8 )
      C( 9 , 4 ) = C( 9 , 4 ) + A( 39 ) * B( 9 )
      C( 9 , 4 ) = C( 9 , 4 ) + A( 40 ) * B( 10 )
      C( 9 , 4 ) = C( 9 , 4 ) + A( 41 ) * B( 14 )
      C( 9 , 4 ) = C( 9 , 4 ) + A( 42 ) * B( 19 )
      C( 9 , 4 ) = C( 9 , 4 ) + A( 43 ) * B( 25 )
      C( 9 , 4 ) = C( 9 , 4 ) + A( 44 ) * B( 32 )
      C( 9 , 4 ) = C( 9 , 4 ) + A( 45 ) * B( 40 )
!
      C( 9 , 5 ) = A( 37 ) * B( 11 )
      C( 9 , 5 ) = C( 9 , 5 ) + A( 38 ) * B( 12 )
      C( 9 , 5 ) = C( 9 , 5 ) + A( 39 ) * B( 13 )
      C( 9 , 5 ) = C( 9 , 5 ) + A( 40 ) * B( 14 )
      C( 9 , 5 ) = C( 9 , 5 ) + A( 41 ) * B( 15 )
      C( 9 , 5 ) = C( 9 , 5 ) + A( 42 ) * B( 20 )
      C( 9 , 5 ) = C( 9 , 5 ) + A( 43 ) * B( 26 )
      C( 9 , 5 ) = C( 9 , 5 ) + A( 44 ) * B( 33 )
      C( 9 , 5 ) = C( 9 , 5 ) + A( 45 ) * B( 41 )
!
      C( 9 , 6 ) = A( 37 ) * B( 16 )
      C( 9 , 6 ) = C( 9 , 6 ) + A( 38 ) * B( 17 )
      C( 9 , 6 ) = C( 9 , 6 ) + A( 39 ) * B( 18 )
      C( 9 , 6 ) = C( 9 , 6 ) + A( 40 ) * B( 19 )
      C( 9 , 6 ) = C( 9 , 6 ) + A( 41 ) * B( 20 )
      C( 9 , 6 ) = C( 9 , 6 ) + A( 42 ) * B( 21 )
      C( 9 , 6 ) = C( 9 , 6 ) + A( 43 ) * B( 27 )
      C( 9 , 6 ) = C( 9 , 6 ) + A( 44 ) * B( 34 )
      C( 9 , 6 ) = C( 9 , 6 ) + A( 45 ) * B( 42 )
!
      C( 9 , 7 ) = A( 37 ) * B( 22 )
      C( 9 , 7 ) = C( 9 , 7 ) + A( 38 ) * B( 23 )
      C( 9 , 7 ) = C( 9 , 7 ) + A( 39 ) * B( 24 )
      C( 9 , 7 ) = C( 9 , 7 ) + A( 40 ) * B( 25 )
      C( 9 , 7 ) = C( 9 , 7 ) + A( 41 ) * B( 26 )
      C( 9 , 7 ) = C( 9 , 7 ) + A( 42 ) * B( 27 )
      C( 9 , 7 ) = C( 9 , 7 ) + A( 43 ) * B( 28 )
      C( 9 , 7 ) = C( 9 , 7 ) + A( 44 ) * B( 35 )
      C( 9 , 7 ) = C( 9 , 7 ) + A( 45 ) * B( 43 )
!
      C( 9 , 8 ) = A( 37 ) * B( 29 )
      C( 9 , 8 ) = C( 9 , 8 ) + A( 38 ) * B( 30 )
      C( 9 , 8 ) = C( 9 , 8 ) + A( 39 ) * B( 31 )
      C( 9 , 8 ) = C( 9 , 8 ) + A( 40 ) * B( 32 )
      C( 9 , 8 ) = C( 9 , 8 ) + A( 41 ) * B( 33 )
      C( 9 , 8 ) = C( 9 , 8 ) + A( 42 ) * B( 34 )
      C( 9 , 8 ) = C( 9 , 8 ) + A( 43 ) * B( 35 )
      C( 9 , 8 ) = C( 9 , 8 ) + A( 44 ) * B( 36 )
      C( 9 , 8 ) = C( 9 , 8 ) + A( 45 ) * B( 44 )
!
      C( 9 , 9 ) = A( 37 ) * B( 37 )
      C( 9 , 9 ) = C( 9 , 9 ) + A( 38 ) * B( 38 )
      C( 9 , 9 ) = C( 9 , 9 ) + A( 39 ) * B( 39 )
      C( 9 , 9 ) = C( 9 , 9 ) + A( 40 ) * B( 40 )
      C( 9 , 9 ) = C( 9 , 9 ) + A( 41 ) * B( 41 )
      C( 9 , 9 ) = C( 9 , 9 ) + A( 42 ) * B( 42 )
      C( 9 , 9 ) = C( 9 , 9 ) + A( 43 ) * B( 43 )
      C( 9 , 9 ) = C( 9 , 9 ) + A( 44 ) * B( 44 )
      C( 9 , 9 ) = C( 9 , 9 ) + A( 45 ) * B( 45 )
!
      RETURN
      END !#! SUBROUTINE MUL_MM_SS_I_9 
