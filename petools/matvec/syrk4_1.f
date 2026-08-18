#include <mk5_preprocessor_directives.inc>
      SUBROUTINE SYRK4_1  ( N, A, LA, C )
 ! ************************************************************************
 ! *                                                                      *
 ! *   Calculates C = C - A(T)*A                                          *
 ! *   A dimensioned as (LA,N), C in packed upper triangular format.      *
 ! *                                                                      *
 ! * ### Source code was created automatically                         ## *
 ! * ### Generator: INVS_GENERATOR (c) L. Petrov version of 2002.11.28 ## *
 ! *                                                                      *
 ! ************************************************************************
      IMPLICIT   NONE
      INTEGER*4  N, LA
      REAL*8     A(LA,1 ), C(*)
      REAL*8     DDOT
!
      IF ( N .EQ. 1 ) THEN
           C( 1 ) = C( 1 ) - A( 1, 1 ) * A( 1, 1  )
          ELSE IF ( N .EQ. 2 ) THEN
           C( 1 ) = C( 1 ) - A( 1, 1 ) * A( 1, 1 )
           C( 1 ) = C( 1 ) - A( 2, 1 ) * A( 2, 1 )
          ELSE IF ( N .EQ. 3 ) THEN
           C( 1 ) = C( 1 ) - A( 1, 1 ) * A( 1, 1 )
           C( 1 ) = C( 1 ) - A( 2, 1 ) * A( 2, 1 )
           C( 1 ) = C( 1 ) - A( 3, 1 ) * A( 3, 1 )
          ELSE IF ( N .EQ. 4 ) THEN
           C( 1 ) = C( 1 ) - A( 1, 1 ) * A( 1, 1 )
           C( 1 ) = C( 1 ) - A( 2, 1 ) * A( 2, 1 )
           C( 1 ) = C( 1 ) - A( 3, 1 ) * A( 3, 1 )
           C( 1 ) = C( 1 ) - A( 4, 1 ) * A( 4, 1 )
          ELSE
           C( 1 ) = C( 1 ) - DDOT ( N, A( 1, 1 ), 1, A( 1, 1 ), 1 )
      END IF
!
      RETURN 
      END SUBROUTINE SYRK4_1 
