#include <mk5_preprocessor_directives.inc>
      SUBROUTINE TRSM4_1  ( N, A, B, LB )
! ************************************************************************
! *                                                                      *
! *   Solves B=A'*X , B(M,N), A(N,N).                                    *
! *   B dimensioned as (LB,N), A in upper triangular format.             *
! *                                                                      *
! * ### Source code was created automatically                         ## *
! * ### Generator: INVS_GENERATOR (c) L. Petrov version of 2002.11.28 ## *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INTEGER*4  N, LB
      REAL*8     A(*), B(LB,N)
!
      IF ( N .EQ. 1 ) THEN
           B( 1 ,1) = B( 1 ,1 ) / A( 1 )
        ELSE IF ( N .EQ. 2 ) THEN
           B( 1 ,1) = B( 1 ,1 ) / A( 1 )
           B( 1 ,2) = B( 1 ,2 ) / A( 1 )
        ELSE IF ( N .EQ. 3 ) THEN
           B( 1 ,1) = B( 1 ,1 ) / A( 1 )
           B( 1 ,2) = B( 1 ,2 ) / A( 1 )
           B( 1 ,3) = B( 1 ,3 ) / A( 1 )
        ELSE IF ( N .EQ. 4 ) THEN
           B( 1 ,1) = B( 1 ,1 ) / A( 1 )
           B( 1 ,2) = B( 1 ,2 ) / A( 1 )
           B( 1 ,3) = B( 1 ,3 ) / A( 1 )
           B( 1 ,4) = B( 1 ,4 ) / A( 1 )
        ELSE
           CALL DSCAL ( N, 1.0D0 / A( 1 ), B( 1 ,1 ), LB )
      END IF
!
      RETURN 
      END SUBROUTINE TRSM4_1 
