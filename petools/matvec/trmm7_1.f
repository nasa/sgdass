#include <mk5_preprocessor_directives.inc>
      SUBROUTINE TRMM7_1  ( M, A, B, LB )
! ************************************************************************
! *                                                                      *
! *   Computes B := -B*A                                                 *
! *   B dimensioned as (LB,N), A in upper triangular format.             *
! *                                                                      *
! * ### Source code was created automatically                         ## *
! * ### Generator: INVS_GENERATOR (c) L. Petrov version of 2002.11.28 ## *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INTEGER*4  M, LB
      REAL*8     A(*), B(LB,1 )
      INTEGER*4  J1
      REAL*8     DDOT
      IF ( M .EQ. 1 ) THEN
           B( 1, 1 ) = - B( 1, 1 )* A( 1 )
         ELSE IF ( M .EQ. 2 ) THEN
           B( 1, 1 ) = - B( 1, 1 ) * A( 1 )
           B( 2, 1 ) = - B( 2, 1 ) * A( 1 )
         ELSE IF ( M .EQ. 3 ) THEN
           B( 1, 1 ) = - B( 1, 1 ) * A( 1 )
           B( 2, 1 ) = - B( 2, 1 ) * A( 1 )
           B( 3, 1 ) = - B( 3, 1 ) * A( 1 )
         ELSE IF ( M .EQ. 4 ) THEN
           B( 1, 1 ) = - B( 1, 1 ) * A( 1 )
           B( 2, 1 ) = - B( 2, 1 ) * A( 1 )
           B( 3, 1 ) = - B( 3, 1 ) * A( 1 )
           B( 4, 1 ) = - B( 4, 1 ) * A( 1 )
         ELSE
           DO 810 J1=1,M
              B( J1, 1 ) = - DDOT (  1 , B( J1, 1 ), LB, A( 1 ), 1 )
  810      CONTINUE
      END IF
!
      RETURN 
      END  !#!  SUBROUTINE TRMM7_1 
