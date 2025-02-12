#include <mk5_preprocessor_directives.inc>
      SUBROUTINE MUL_MM_SS_I_7  ( A, B, C )
! ************************************************************************
! *                                                                      *
! *   Compute a product of two square symmetric matrices in upper        *
! *   triangular representation.                                         *
! *                                                                      *
! * ### Source code was created automatically                         ## *
! * ### Generator: INVS_GENERATOR (c) L. Petrov version of 2002.12.12 ## *
! *                                                                      *
! ************************************************************************
      REAL*8     A(*), B(*), C( 7 , 7 )
!
      C( 1 , 1 ) = A( 1 ) * B( 1 )
      C( 1 , 1 ) = C( 1 , 1 ) + A( 2 ) * B( 2 )
      C( 1 , 1 ) = C( 1 , 1 ) + A( 4 ) * B( 4 )
      C( 1 , 1 ) = C( 1 , 1 ) + A( 7 ) * B( 7 )
      C( 1 , 1 ) = C( 1 , 1 ) + A( 11 ) * B( 11 )
      C( 1 , 1 ) = C( 1 , 1 ) + A( 16 ) * B( 16 )
      C( 1 , 1 ) = C( 1 , 1 ) + A( 22 ) * B( 22 )
!
      C( 1 , 2 ) = A( 1 ) * B( 2 )
      C( 1 , 2 ) = C( 1 , 2 ) + A( 2 ) * B( 3 )
      C( 1 , 2 ) = C( 1 , 2 ) + A( 4 ) * B( 5 )
      C( 1 , 2 ) = C( 1 , 2 ) + A( 7 ) * B( 8 )
      C( 1 , 2 ) = C( 1 , 2 ) + A( 11 ) * B( 12 )
      C( 1 , 2 ) = C( 1 , 2 ) + A( 16 ) * B( 17 )
      C( 1 , 2 ) = C( 1 , 2 ) + A( 22 ) * B( 23 )
!
      C( 1 , 3 ) = A( 1 ) * B( 4 )
      C( 1 , 3 ) = C( 1 , 3 ) + A( 2 ) * B( 5 )
      C( 1 , 3 ) = C( 1 , 3 ) + A( 4 ) * B( 6 )
      C( 1 , 3 ) = C( 1 , 3 ) + A( 7 ) * B( 9 )
      C( 1 , 3 ) = C( 1 , 3 ) + A( 11 ) * B( 13 )
      C( 1 , 3 ) = C( 1 , 3 ) + A( 16 ) * B( 18 )
      C( 1 , 3 ) = C( 1 , 3 ) + A( 22 ) * B( 24 )
!
      C( 1 , 4 ) = A( 1 ) * B( 7 )
      C( 1 , 4 ) = C( 1 , 4 ) + A( 2 ) * B( 8 )
      C( 1 , 4 ) = C( 1 , 4 ) + A( 4 ) * B( 9 )
      C( 1 , 4 ) = C( 1 , 4 ) + A( 7 ) * B( 10 )
      C( 1 , 4 ) = C( 1 , 4 ) + A( 11 ) * B( 14 )
      C( 1 , 4 ) = C( 1 , 4 ) + A( 16 ) * B( 19 )
      C( 1 , 4 ) = C( 1 , 4 ) + A( 22 ) * B( 25 )
!
      C( 1 , 5 ) = A( 1 ) * B( 11 )
      C( 1 , 5 ) = C( 1 , 5 ) + A( 2 ) * B( 12 )
      C( 1 , 5 ) = C( 1 , 5 ) + A( 4 ) * B( 13 )
      C( 1 , 5 ) = C( 1 , 5 ) + A( 7 ) * B( 14 )
      C( 1 , 5 ) = C( 1 , 5 ) + A( 11 ) * B( 15 )
      C( 1 , 5 ) = C( 1 , 5 ) + A( 16 ) * B( 20 )
      C( 1 , 5 ) = C( 1 , 5 ) + A( 22 ) * B( 26 )
!
      C( 1 , 6 ) = A( 1 ) * B( 16 )
      C( 1 , 6 ) = C( 1 , 6 ) + A( 2 ) * B( 17 )
      C( 1 , 6 ) = C( 1 , 6 ) + A( 4 ) * B( 18 )
      C( 1 , 6 ) = C( 1 , 6 ) + A( 7 ) * B( 19 )
      C( 1 , 6 ) = C( 1 , 6 ) + A( 11 ) * B( 20 )
      C( 1 , 6 ) = C( 1 , 6 ) + A( 16 ) * B( 21 )
      C( 1 , 6 ) = C( 1 , 6 ) + A( 22 ) * B( 27 )
!
      C( 1 , 7 ) = A( 1 ) * B( 22 )
      C( 1 , 7 ) = C( 1 , 7 ) + A( 2 ) * B( 23 )
      C( 1 , 7 ) = C( 1 , 7 ) + A( 4 ) * B( 24 )
      C( 1 , 7 ) = C( 1 , 7 ) + A( 7 ) * B( 25 )
      C( 1 , 7 ) = C( 1 , 7 ) + A( 11 ) * B( 26 )
      C( 1 , 7 ) = C( 1 , 7 ) + A( 16 ) * B( 27 )
      C( 1 , 7 ) = C( 1 , 7 ) + A( 22 ) * B( 28 )
!
      C( 2 , 1 ) = A( 2 ) * B( 1 )
      C( 2 , 1 ) = C( 2 , 1 ) + A( 3 ) * B( 2 )
      C( 2 , 1 ) = C( 2 , 1 ) + A( 5 ) * B( 4 )
      C( 2 , 1 ) = C( 2 , 1 ) + A( 8 ) * B( 7 )
      C( 2 , 1 ) = C( 2 , 1 ) + A( 12 ) * B( 11 )
      C( 2 , 1 ) = C( 2 , 1 ) + A( 17 ) * B( 16 )
      C( 2 , 1 ) = C( 2 , 1 ) + A( 23 ) * B( 22 )
!
      C( 2 , 2 ) = A( 2 ) * B( 2 )
      C( 2 , 2 ) = C( 2 , 2 ) + A( 3 ) * B( 3 )
      C( 2 , 2 ) = C( 2 , 2 ) + A( 5 ) * B( 5 )
      C( 2 , 2 ) = C( 2 , 2 ) + A( 8 ) * B( 8 )
      C( 2 , 2 ) = C( 2 , 2 ) + A( 12 ) * B( 12 )
      C( 2 , 2 ) = C( 2 , 2 ) + A( 17 ) * B( 17 )
      C( 2 , 2 ) = C( 2 , 2 ) + A( 23 ) * B( 23 )
!
      C( 2 , 3 ) = A( 2 ) * B( 4 )
      C( 2 , 3 ) = C( 2 , 3 ) + A( 3 ) * B( 5 )
      C( 2 , 3 ) = C( 2 , 3 ) + A( 5 ) * B( 6 )
      C( 2 , 3 ) = C( 2 , 3 ) + A( 8 ) * B( 9 )
      C( 2 , 3 ) = C( 2 , 3 ) + A( 12 ) * B( 13 )
      C( 2 , 3 ) = C( 2 , 3 ) + A( 17 ) * B( 18 )
      C( 2 , 3 ) = C( 2 , 3 ) + A( 23 ) * B( 24 )
!
      C( 2 , 4 ) = A( 2 ) * B( 7 )
      C( 2 , 4 ) = C( 2 , 4 ) + A( 3 ) * B( 8 )
      C( 2 , 4 ) = C( 2 , 4 ) + A( 5 ) * B( 9 )
      C( 2 , 4 ) = C( 2 , 4 ) + A( 8 ) * B( 10 )
      C( 2 , 4 ) = C( 2 , 4 ) + A( 12 ) * B( 14 )
      C( 2 , 4 ) = C( 2 , 4 ) + A( 17 ) * B( 19 )
      C( 2 , 4 ) = C( 2 , 4 ) + A( 23 ) * B( 25 )
!
      C( 2 , 5 ) = A( 2 ) * B( 11 )
      C( 2 , 5 ) = C( 2 , 5 ) + A( 3 ) * B( 12 )
      C( 2 , 5 ) = C( 2 , 5 ) + A( 5 ) * B( 13 )
      C( 2 , 5 ) = C( 2 , 5 ) + A( 8 ) * B( 14 )
      C( 2 , 5 ) = C( 2 , 5 ) + A( 12 ) * B( 15 )
      C( 2 , 5 ) = C( 2 , 5 ) + A( 17 ) * B( 20 )
      C( 2 , 5 ) = C( 2 , 5 ) + A( 23 ) * B( 26 )
!
      C( 2 , 6 ) = A( 2 ) * B( 16 )
      C( 2 , 6 ) = C( 2 , 6 ) + A( 3 ) * B( 17 )
      C( 2 , 6 ) = C( 2 , 6 ) + A( 5 ) * B( 18 )
      C( 2 , 6 ) = C( 2 , 6 ) + A( 8 ) * B( 19 )
      C( 2 , 6 ) = C( 2 , 6 ) + A( 12 ) * B( 20 )
      C( 2 , 6 ) = C( 2 , 6 ) + A( 17 ) * B( 21 )
      C( 2 , 6 ) = C( 2 , 6 ) + A( 23 ) * B( 27 )
!
      C( 2 , 7 ) = A( 2 ) * B( 22 )
      C( 2 , 7 ) = C( 2 , 7 ) + A( 3 ) * B( 23 )
      C( 2 , 7 ) = C( 2 , 7 ) + A( 5 ) * B( 24 )
      C( 2 , 7 ) = C( 2 , 7 ) + A( 8 ) * B( 25 )
      C( 2 , 7 ) = C( 2 , 7 ) + A( 12 ) * B( 26 )
      C( 2 , 7 ) = C( 2 , 7 ) + A( 17 ) * B( 27 )
      C( 2 , 7 ) = C( 2 , 7 ) + A( 23 ) * B( 28 )
!
      C( 3 , 1 ) = A( 4 ) * B( 1 )
      C( 3 , 1 ) = C( 3 , 1 ) + A( 5 ) * B( 2 )
      C( 3 , 1 ) = C( 3 , 1 ) + A( 6 ) * B( 4 )
      C( 3 , 1 ) = C( 3 , 1 ) + A( 9 ) * B( 7 )
      C( 3 , 1 ) = C( 3 , 1 ) + A( 13 ) * B( 11 )
      C( 3 , 1 ) = C( 3 , 1 ) + A( 18 ) * B( 16 )
      C( 3 , 1 ) = C( 3 , 1 ) + A( 24 ) * B( 22 )
!
      C( 3 , 2 ) = A( 4 ) * B( 2 )
      C( 3 , 2 ) = C( 3 , 2 ) + A( 5 ) * B( 3 )
      C( 3 , 2 ) = C( 3 , 2 ) + A( 6 ) * B( 5 )
      C( 3 , 2 ) = C( 3 , 2 ) + A( 9 ) * B( 8 )
      C( 3 , 2 ) = C( 3 , 2 ) + A( 13 ) * B( 12 )
      C( 3 , 2 ) = C( 3 , 2 ) + A( 18 ) * B( 17 )
      C( 3 , 2 ) = C( 3 , 2 ) + A( 24 ) * B( 23 )
!
      C( 3 , 3 ) = A( 4 ) * B( 4 )
      C( 3 , 3 ) = C( 3 , 3 ) + A( 5 ) * B( 5 )
      C( 3 , 3 ) = C( 3 , 3 ) + A( 6 ) * B( 6 )
      C( 3 , 3 ) = C( 3 , 3 ) + A( 9 ) * B( 9 )
      C( 3 , 3 ) = C( 3 , 3 ) + A( 13 ) * B( 13 )
      C( 3 , 3 ) = C( 3 , 3 ) + A( 18 ) * B( 18 )
      C( 3 , 3 ) = C( 3 , 3 ) + A( 24 ) * B( 24 )
!
      C( 3 , 4 ) = A( 4 ) * B( 7 )
      C( 3 , 4 ) = C( 3 , 4 ) + A( 5 ) * B( 8 )
      C( 3 , 4 ) = C( 3 , 4 ) + A( 6 ) * B( 9 )
      C( 3 , 4 ) = C( 3 , 4 ) + A( 9 ) * B( 10 )
      C( 3 , 4 ) = C( 3 , 4 ) + A( 13 ) * B( 14 )
      C( 3 , 4 ) = C( 3 , 4 ) + A( 18 ) * B( 19 )
      C( 3 , 4 ) = C( 3 , 4 ) + A( 24 ) * B( 25 )
!
      C( 3 , 5 ) = A( 4 ) * B( 11 )
      C( 3 , 5 ) = C( 3 , 5 ) + A( 5 ) * B( 12 )
      C( 3 , 5 ) = C( 3 , 5 ) + A( 6 ) * B( 13 )
      C( 3 , 5 ) = C( 3 , 5 ) + A( 9 ) * B( 14 )
      C( 3 , 5 ) = C( 3 , 5 ) + A( 13 ) * B( 15 )
      C( 3 , 5 ) = C( 3 , 5 ) + A( 18 ) * B( 20 )
      C( 3 , 5 ) = C( 3 , 5 ) + A( 24 ) * B( 26 )
!
      C( 3 , 6 ) = A( 4 ) * B( 16 )
      C( 3 , 6 ) = C( 3 , 6 ) + A( 5 ) * B( 17 )
      C( 3 , 6 ) = C( 3 , 6 ) + A( 6 ) * B( 18 )
      C( 3 , 6 ) = C( 3 , 6 ) + A( 9 ) * B( 19 )
      C( 3 , 6 ) = C( 3 , 6 ) + A( 13 ) * B( 20 )
      C( 3 , 6 ) = C( 3 , 6 ) + A( 18 ) * B( 21 )
      C( 3 , 6 ) = C( 3 , 6 ) + A( 24 ) * B( 27 )
!
      C( 3 , 7 ) = A( 4 ) * B( 22 )
      C( 3 , 7 ) = C( 3 , 7 ) + A( 5 ) * B( 23 )
      C( 3 , 7 ) = C( 3 , 7 ) + A( 6 ) * B( 24 )
      C( 3 , 7 ) = C( 3 , 7 ) + A( 9 ) * B( 25 )
      C( 3 , 7 ) = C( 3 , 7 ) + A( 13 ) * B( 26 )
      C( 3 , 7 ) = C( 3 , 7 ) + A( 18 ) * B( 27 )
      C( 3 , 7 ) = C( 3 , 7 ) + A( 24 ) * B( 28 )
!
      C( 4 , 1 ) = A( 7 ) * B( 1 )
      C( 4 , 1 ) = C( 4 , 1 ) + A( 8 ) * B( 2 )
      C( 4 , 1 ) = C( 4 , 1 ) + A( 9 ) * B( 4 )
      C( 4 , 1 ) = C( 4 , 1 ) + A( 10 ) * B( 7 )
      C( 4 , 1 ) = C( 4 , 1 ) + A( 14 ) * B( 11 )
      C( 4 , 1 ) = C( 4 , 1 ) + A( 19 ) * B( 16 )
      C( 4 , 1 ) = C( 4 , 1 ) + A( 25 ) * B( 22 )
!
      C( 4 , 2 ) = A( 7 ) * B( 2 )
      C( 4 , 2 ) = C( 4 , 2 ) + A( 8 ) * B( 3 )
      C( 4 , 2 ) = C( 4 , 2 ) + A( 9 ) * B( 5 )
      C( 4 , 2 ) = C( 4 , 2 ) + A( 10 ) * B( 8 )
      C( 4 , 2 ) = C( 4 , 2 ) + A( 14 ) * B( 12 )
      C( 4 , 2 ) = C( 4 , 2 ) + A( 19 ) * B( 17 )
      C( 4 , 2 ) = C( 4 , 2 ) + A( 25 ) * B( 23 )
!
      C( 4 , 3 ) = A( 7 ) * B( 4 )
      C( 4 , 3 ) = C( 4 , 3 ) + A( 8 ) * B( 5 )
      C( 4 , 3 ) = C( 4 , 3 ) + A( 9 ) * B( 6 )
      C( 4 , 3 ) = C( 4 , 3 ) + A( 10 ) * B( 9 )
      C( 4 , 3 ) = C( 4 , 3 ) + A( 14 ) * B( 13 )
      C( 4 , 3 ) = C( 4 , 3 ) + A( 19 ) * B( 18 )
      C( 4 , 3 ) = C( 4 , 3 ) + A( 25 ) * B( 24 )
!
      C( 4 , 4 ) = A( 7 ) * B( 7 )
      C( 4 , 4 ) = C( 4 , 4 ) + A( 8 ) * B( 8 )
      C( 4 , 4 ) = C( 4 , 4 ) + A( 9 ) * B( 9 )
      C( 4 , 4 ) = C( 4 , 4 ) + A( 10 ) * B( 10 )
      C( 4 , 4 ) = C( 4 , 4 ) + A( 14 ) * B( 14 )
      C( 4 , 4 ) = C( 4 , 4 ) + A( 19 ) * B( 19 )
      C( 4 , 4 ) = C( 4 , 4 ) + A( 25 ) * B( 25 )
!
      C( 4 , 5 ) = A( 7 ) * B( 11 )
      C( 4 , 5 ) = C( 4 , 5 ) + A( 8 ) * B( 12 )
      C( 4 , 5 ) = C( 4 , 5 ) + A( 9 ) * B( 13 )
      C( 4 , 5 ) = C( 4 , 5 ) + A( 10 ) * B( 14 )
      C( 4 , 5 ) = C( 4 , 5 ) + A( 14 ) * B( 15 )
      C( 4 , 5 ) = C( 4 , 5 ) + A( 19 ) * B( 20 )
      C( 4 , 5 ) = C( 4 , 5 ) + A( 25 ) * B( 26 )
!
      C( 4 , 6 ) = A( 7 ) * B( 16 )
      C( 4 , 6 ) = C( 4 , 6 ) + A( 8 ) * B( 17 )
      C( 4 , 6 ) = C( 4 , 6 ) + A( 9 ) * B( 18 )
      C( 4 , 6 ) = C( 4 , 6 ) + A( 10 ) * B( 19 )
      C( 4 , 6 ) = C( 4 , 6 ) + A( 14 ) * B( 20 )
      C( 4 , 6 ) = C( 4 , 6 ) + A( 19 ) * B( 21 )
      C( 4 , 6 ) = C( 4 , 6 ) + A( 25 ) * B( 27 )
!
      C( 4 , 7 ) = A( 7 ) * B( 22 )
      C( 4 , 7 ) = C( 4 , 7 ) + A( 8 ) * B( 23 )
      C( 4 , 7 ) = C( 4 , 7 ) + A( 9 ) * B( 24 )
      C( 4 , 7 ) = C( 4 , 7 ) + A( 10 ) * B( 25 )
      C( 4 , 7 ) = C( 4 , 7 ) + A( 14 ) * B( 26 )
      C( 4 , 7 ) = C( 4 , 7 ) + A( 19 ) * B( 27 )
      C( 4 , 7 ) = C( 4 , 7 ) + A( 25 ) * B( 28 )
!
      C( 5 , 1 ) = A( 11 ) * B( 1 )
      C( 5 , 1 ) = C( 5 , 1 ) + A( 12 ) * B( 2 )
      C( 5 , 1 ) = C( 5 , 1 ) + A( 13 ) * B( 4 )
      C( 5 , 1 ) = C( 5 , 1 ) + A( 14 ) * B( 7 )
      C( 5 , 1 ) = C( 5 , 1 ) + A( 15 ) * B( 11 )
      C( 5 , 1 ) = C( 5 , 1 ) + A( 20 ) * B( 16 )
      C( 5 , 1 ) = C( 5 , 1 ) + A( 26 ) * B( 22 )
!
      C( 5 , 2 ) = A( 11 ) * B( 2 )
      C( 5 , 2 ) = C( 5 , 2 ) + A( 12 ) * B( 3 )
      C( 5 , 2 ) = C( 5 , 2 ) + A( 13 ) * B( 5 )
      C( 5 , 2 ) = C( 5 , 2 ) + A( 14 ) * B( 8 )
      C( 5 , 2 ) = C( 5 , 2 ) + A( 15 ) * B( 12 )
      C( 5 , 2 ) = C( 5 , 2 ) + A( 20 ) * B( 17 )
      C( 5 , 2 ) = C( 5 , 2 ) + A( 26 ) * B( 23 )
!
      C( 5 , 3 ) = A( 11 ) * B( 4 )
      C( 5 , 3 ) = C( 5 , 3 ) + A( 12 ) * B( 5 )
      C( 5 , 3 ) = C( 5 , 3 ) + A( 13 ) * B( 6 )
      C( 5 , 3 ) = C( 5 , 3 ) + A( 14 ) * B( 9 )
      C( 5 , 3 ) = C( 5 , 3 ) + A( 15 ) * B( 13 )
      C( 5 , 3 ) = C( 5 , 3 ) + A( 20 ) * B( 18 )
      C( 5 , 3 ) = C( 5 , 3 ) + A( 26 ) * B( 24 )
!
      C( 5 , 4 ) = A( 11 ) * B( 7 )
      C( 5 , 4 ) = C( 5 , 4 ) + A( 12 ) * B( 8 )
      C( 5 , 4 ) = C( 5 , 4 ) + A( 13 ) * B( 9 )
      C( 5 , 4 ) = C( 5 , 4 ) + A( 14 ) * B( 10 )
      C( 5 , 4 ) = C( 5 , 4 ) + A( 15 ) * B( 14 )
      C( 5 , 4 ) = C( 5 , 4 ) + A( 20 ) * B( 19 )
      C( 5 , 4 ) = C( 5 , 4 ) + A( 26 ) * B( 25 )
!
      C( 5 , 5 ) = A( 11 ) * B( 11 )
      C( 5 , 5 ) = C( 5 , 5 ) + A( 12 ) * B( 12 )
      C( 5 , 5 ) = C( 5 , 5 ) + A( 13 ) * B( 13 )
      C( 5 , 5 ) = C( 5 , 5 ) + A( 14 ) * B( 14 )
      C( 5 , 5 ) = C( 5 , 5 ) + A( 15 ) * B( 15 )
      C( 5 , 5 ) = C( 5 , 5 ) + A( 20 ) * B( 20 )
      C( 5 , 5 ) = C( 5 , 5 ) + A( 26 ) * B( 26 )
!
      C( 5 , 6 ) = A( 11 ) * B( 16 )
      C( 5 , 6 ) = C( 5 , 6 ) + A( 12 ) * B( 17 )
      C( 5 , 6 ) = C( 5 , 6 ) + A( 13 ) * B( 18 )
      C( 5 , 6 ) = C( 5 , 6 ) + A( 14 ) * B( 19 )
      C( 5 , 6 ) = C( 5 , 6 ) + A( 15 ) * B( 20 )
      C( 5 , 6 ) = C( 5 , 6 ) + A( 20 ) * B( 21 )
      C( 5 , 6 ) = C( 5 , 6 ) + A( 26 ) * B( 27 )
!
      C( 5 , 7 ) = A( 11 ) * B( 22 )
      C( 5 , 7 ) = C( 5 , 7 ) + A( 12 ) * B( 23 )
      C( 5 , 7 ) = C( 5 , 7 ) + A( 13 ) * B( 24 )
      C( 5 , 7 ) = C( 5 , 7 ) + A( 14 ) * B( 25 )
      C( 5 , 7 ) = C( 5 , 7 ) + A( 15 ) * B( 26 )
      C( 5 , 7 ) = C( 5 , 7 ) + A( 20 ) * B( 27 )
      C( 5 , 7 ) = C( 5 , 7 ) + A( 26 ) * B( 28 )
!
      C( 6 , 1 ) = A( 16 ) * B( 1 )
      C( 6 , 1 ) = C( 6 , 1 ) + A( 17 ) * B( 2 )
      C( 6 , 1 ) = C( 6 , 1 ) + A( 18 ) * B( 4 )
      C( 6 , 1 ) = C( 6 , 1 ) + A( 19 ) * B( 7 )
      C( 6 , 1 ) = C( 6 , 1 ) + A( 20 ) * B( 11 )
      C( 6 , 1 ) = C( 6 , 1 ) + A( 21 ) * B( 16 )
      C( 6 , 1 ) = C( 6 , 1 ) + A( 27 ) * B( 22 )
!
      C( 6 , 2 ) = A( 16 ) * B( 2 )
      C( 6 , 2 ) = C( 6 , 2 ) + A( 17 ) * B( 3 )
      C( 6 , 2 ) = C( 6 , 2 ) + A( 18 ) * B( 5 )
      C( 6 , 2 ) = C( 6 , 2 ) + A( 19 ) * B( 8 )
      C( 6 , 2 ) = C( 6 , 2 ) + A( 20 ) * B( 12 )
      C( 6 , 2 ) = C( 6 , 2 ) + A( 21 ) * B( 17 )
      C( 6 , 2 ) = C( 6 , 2 ) + A( 27 ) * B( 23 )
!
      C( 6 , 3 ) = A( 16 ) * B( 4 )
      C( 6 , 3 ) = C( 6 , 3 ) + A( 17 ) * B( 5 )
      C( 6 , 3 ) = C( 6 , 3 ) + A( 18 ) * B( 6 )
      C( 6 , 3 ) = C( 6 , 3 ) + A( 19 ) * B( 9 )
      C( 6 , 3 ) = C( 6 , 3 ) + A( 20 ) * B( 13 )
      C( 6 , 3 ) = C( 6 , 3 ) + A( 21 ) * B( 18 )
      C( 6 , 3 ) = C( 6 , 3 ) + A( 27 ) * B( 24 )
!
      C( 6 , 4 ) = A( 16 ) * B( 7 )
      C( 6 , 4 ) = C( 6 , 4 ) + A( 17 ) * B( 8 )
      C( 6 , 4 ) = C( 6 , 4 ) + A( 18 ) * B( 9 )
      C( 6 , 4 ) = C( 6 , 4 ) + A( 19 ) * B( 10 )
      C( 6 , 4 ) = C( 6 , 4 ) + A( 20 ) * B( 14 )
      C( 6 , 4 ) = C( 6 , 4 ) + A( 21 ) * B( 19 )
      C( 6 , 4 ) = C( 6 , 4 ) + A( 27 ) * B( 25 )
!
      C( 6 , 5 ) = A( 16 ) * B( 11 )
      C( 6 , 5 ) = C( 6 , 5 ) + A( 17 ) * B( 12 )
      C( 6 , 5 ) = C( 6 , 5 ) + A( 18 ) * B( 13 )
      C( 6 , 5 ) = C( 6 , 5 ) + A( 19 ) * B( 14 )
      C( 6 , 5 ) = C( 6 , 5 ) + A( 20 ) * B( 15 )
      C( 6 , 5 ) = C( 6 , 5 ) + A( 21 ) * B( 20 )
      C( 6 , 5 ) = C( 6 , 5 ) + A( 27 ) * B( 26 )
!
      C( 6 , 6 ) = A( 16 ) * B( 16 )
      C( 6 , 6 ) = C( 6 , 6 ) + A( 17 ) * B( 17 )
      C( 6 , 6 ) = C( 6 , 6 ) + A( 18 ) * B( 18 )
      C( 6 , 6 ) = C( 6 , 6 ) + A( 19 ) * B( 19 )
      C( 6 , 6 ) = C( 6 , 6 ) + A( 20 ) * B( 20 )
      C( 6 , 6 ) = C( 6 , 6 ) + A( 21 ) * B( 21 )
      C( 6 , 6 ) = C( 6 , 6 ) + A( 27 ) * B( 27 )
!
      C( 6 , 7 ) = A( 16 ) * B( 22 )
      C( 6 , 7 ) = C( 6 , 7 ) + A( 17 ) * B( 23 )
      C( 6 , 7 ) = C( 6 , 7 ) + A( 18 ) * B( 24 )
      C( 6 , 7 ) = C( 6 , 7 ) + A( 19 ) * B( 25 )
      C( 6 , 7 ) = C( 6 , 7 ) + A( 20 ) * B( 26 )
      C( 6 , 7 ) = C( 6 , 7 ) + A( 21 ) * B( 27 )
      C( 6 , 7 ) = C( 6 , 7 ) + A( 27 ) * B( 28 )
!
      C( 7 , 1 ) = A( 22 ) * B( 1 )
      C( 7 , 1 ) = C( 7 , 1 ) + A( 23 ) * B( 2 )
      C( 7 , 1 ) = C( 7 , 1 ) + A( 24 ) * B( 4 )
      C( 7 , 1 ) = C( 7 , 1 ) + A( 25 ) * B( 7 )
      C( 7 , 1 ) = C( 7 , 1 ) + A( 26 ) * B( 11 )
      C( 7 , 1 ) = C( 7 , 1 ) + A( 27 ) * B( 16 )
      C( 7 , 1 ) = C( 7 , 1 ) + A( 28 ) * B( 22 )
!
      C( 7 , 2 ) = A( 22 ) * B( 2 )
      C( 7 , 2 ) = C( 7 , 2 ) + A( 23 ) * B( 3 )
      C( 7 , 2 ) = C( 7 , 2 ) + A( 24 ) * B( 5 )
      C( 7 , 2 ) = C( 7 , 2 ) + A( 25 ) * B( 8 )
      C( 7 , 2 ) = C( 7 , 2 ) + A( 26 ) * B( 12 )
      C( 7 , 2 ) = C( 7 , 2 ) + A( 27 ) * B( 17 )
      C( 7 , 2 ) = C( 7 , 2 ) + A( 28 ) * B( 23 )
!
      C( 7 , 3 ) = A( 22 ) * B( 4 )
      C( 7 , 3 ) = C( 7 , 3 ) + A( 23 ) * B( 5 )
      C( 7 , 3 ) = C( 7 , 3 ) + A( 24 ) * B( 6 )
      C( 7 , 3 ) = C( 7 , 3 ) + A( 25 ) * B( 9 )
      C( 7 , 3 ) = C( 7 , 3 ) + A( 26 ) * B( 13 )
      C( 7 , 3 ) = C( 7 , 3 ) + A( 27 ) * B( 18 )
      C( 7 , 3 ) = C( 7 , 3 ) + A( 28 ) * B( 24 )
!
      C( 7 , 4 ) = A( 22 ) * B( 7 )
      C( 7 , 4 ) = C( 7 , 4 ) + A( 23 ) * B( 8 )
      C( 7 , 4 ) = C( 7 , 4 ) + A( 24 ) * B( 9 )
      C( 7 , 4 ) = C( 7 , 4 ) + A( 25 ) * B( 10 )
      C( 7 , 4 ) = C( 7 , 4 ) + A( 26 ) * B( 14 )
      C( 7 , 4 ) = C( 7 , 4 ) + A( 27 ) * B( 19 )
      C( 7 , 4 ) = C( 7 , 4 ) + A( 28 ) * B( 25 )
!
      C( 7 , 5 ) = A( 22 ) * B( 11 )
      C( 7 , 5 ) = C( 7 , 5 ) + A( 23 ) * B( 12 )
      C( 7 , 5 ) = C( 7 , 5 ) + A( 24 ) * B( 13 )
      C( 7 , 5 ) = C( 7 , 5 ) + A( 25 ) * B( 14 )
      C( 7 , 5 ) = C( 7 , 5 ) + A( 26 ) * B( 15 )
      C( 7 , 5 ) = C( 7 , 5 ) + A( 27 ) * B( 20 )
      C( 7 , 5 ) = C( 7 , 5 ) + A( 28 ) * B( 26 )
!
      C( 7 , 6 ) = A( 22 ) * B( 16 )
      C( 7 , 6 ) = C( 7 , 6 ) + A( 23 ) * B( 17 )
      C( 7 , 6 ) = C( 7 , 6 ) + A( 24 ) * B( 18 )
      C( 7 , 6 ) = C( 7 , 6 ) + A( 25 ) * B( 19 )
      C( 7 , 6 ) = C( 7 , 6 ) + A( 26 ) * B( 20 )
      C( 7 , 6 ) = C( 7 , 6 ) + A( 27 ) * B( 21 )
      C( 7 , 6 ) = C( 7 , 6 ) + A( 28 ) * B( 27 )
!
      C( 7 , 7 ) = A( 22 ) * B( 22 )
      C( 7 , 7 ) = C( 7 , 7 ) + A( 23 ) * B( 23 )
      C( 7 , 7 ) = C( 7 , 7 ) + A( 24 ) * B( 24 )
      C( 7 , 7 ) = C( 7 , 7 ) + A( 25 ) * B( 25 )
      C( 7 , 7 ) = C( 7 , 7 ) + A( 26 ) * B( 26 )
      C( 7 , 7 ) = C( 7 , 7 ) + A( 27 ) * B( 27 )
      C( 7 , 7 ) = C( 7 , 7 ) + A( 28 ) * B( 28 )
!
      RETURN
      END !#! SUBROUTINE MUL_MM_SS_I_7 
