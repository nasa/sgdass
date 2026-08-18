#include <mk5_preprocessor_directives.inc>
      SUBROUTINE SYRK3_1  ( N, A, LA, C )
 ! ************************************************************************
 ! *                                                                      *
 ! *   Calculates C = C + A*A(T)                                          *
 ! *   A dimensioned as (LA,N), C in packed upper triangular format.      *
 ! *                                                                      *
 ! * ### Source code was created automatically                         ## *
 ! * ### Generator: INVS_GENERATOR (c) L. Petrov version of 2002.11.28 ## *
 ! *                                                                      *
 ! ************************************************************************
      IMPLICIT   NONE
      INTEGER*4  N, LA
      REAL*8     A(LA,N), C(*)
      INTEGER*4  J1, J2, J3
      REAL*8     DDOT
!
      IF ( N .EQ. 1 ) THEN
           C( 1 ) = C( 1 ) + A( 1 ,1 ) * A( 1 ,1 )
          ELSE IF ( N .EQ. 2 ) THEN
           C( 1 ) = C( 1 ) + A( 1 ,1 ) * A( 1 ,1 )
           C( 1 ) = C( 1 ) + A( 1 ,2 ) * A( 1 ,2 )
          ELSE IF ( N .EQ. 3 ) THEN
           C( 1 ) = C( 1 ) + A( 1 ,1 ) * A( 1 ,1 )
           C( 1 ) = C( 1 ) + A( 1 ,2 ) * A( 1 ,2 )
           C( 1 ) = C( 1 ) + A( 1 ,3 ) * A( 1 ,3 )
          ELSE IF ( N .EQ. 4 ) THEN
           C( 1 ) = C( 1 ) + A( 1 ,1 ) * A( 1 ,1 )
           C( 1 ) = C( 1 ) + A( 1 ,2 ) * A( 1 ,2 )
           C( 1 ) = C( 1 ) + A( 1 ,3 ) * A( 1 ,3 )
           C( 1 ) = C( 1 ) + A( 1 ,4 ) * A( 1 ,4 )
          ELSE
           C( 1 ) = C( 1 ) + DDOT ( N, A( 1 , 1 ), LA, A( 1 , 1 ), LA )
      END IF
!
      RETURN 
      END  !#!  SUBROUTINE SYRK3_1 
