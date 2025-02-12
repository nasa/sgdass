#include <mk5_preprocessor_directives.inc>
      SUBROUTINE TRSM4_4  ( N, A, B, LB )
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
           B( 2 ,1) = B( 2 ,1 ) - A( 2 ) * B( 1 ,1 )
           B( 2 ,1) = B( 2 ,1 ) / A( 3 )
           B( 3 ,1) = B( 3 ,1 ) - A( 4 ) * B( 1 ,1 )
           B( 3 ,1) = B( 3 ,1 ) - A( 5 ) * B( 2 ,1 )
           B( 3 ,1) = B( 3 ,1 ) / A( 6 )
           B( 4 ,1) = B( 4 ,1 ) - A( 7 ) * B( 1 ,1 )
           B( 4 ,1) = B( 4 ,1 ) - A( 8 ) * B( 2 ,1 )
           B( 4 ,1) = B( 4 ,1 ) - A( 9 ) * B( 3 ,1 )
           B( 4 ,1) = B( 4 ,1 ) / A( 10 )
        ELSE IF ( N .EQ. 2 ) THEN
           B( 1 ,1) = B( 1 ,1 ) / A( 1 )
           B( 1 ,2) = B( 1 ,2 ) / A( 1 )
           B( 2 ,1) = B( 2 ,1 ) - A( 2 ) * B( 1 ,1 )
           B( 2 ,2) = B( 2 ,2 ) - A( 2 ) * B( 1 ,2 )
           B( 2 ,1) = B( 2 ,1 ) / A( 3 )
           B( 2 ,2) = B( 2 ,2 ) / A( 3 )
           B( 3 ,1) = B( 3 ,1 ) - A( 4 ) * B( 1 ,1 )
           B( 3 ,2) = B( 3 ,2 ) - A( 4 ) * B( 1 ,2 )
           B( 3 ,1) = B( 3 ,1 ) - A( 5 ) * B( 2 ,1 )
           B( 3 ,2) = B( 3 ,2 ) - A( 5 ) * B( 2 ,2 )
           B( 3 ,1) = B( 3 ,1 ) / A( 6 )
           B( 3 ,2) = B( 3 ,2 ) / A( 6 )
           B( 4 ,1) = B( 4 ,1 ) - A( 7 ) * B( 1 ,1 )
           B( 4 ,2) = B( 4 ,2 ) - A( 7 ) * B( 1 ,2 )
           B( 4 ,1) = B( 4 ,1 ) - A( 8 ) * B( 2 ,1 )
           B( 4 ,2) = B( 4 ,2 ) - A( 8 ) * B( 2 ,2 )
           B( 4 ,1) = B( 4 ,1 ) - A( 9 ) * B( 3 ,1 )
           B( 4 ,2) = B( 4 ,2 ) - A( 9 ) * B( 3 ,2 )
           B( 4 ,1) = B( 4 ,1 ) / A( 10 )
           B( 4 ,2) = B( 4 ,2 ) / A( 10 )
        ELSE IF ( N .EQ. 3 ) THEN
           B( 1 ,1) = B( 1 ,1 ) / A( 1 )
           B( 1 ,2) = B( 1 ,2 ) / A( 1 )
           B( 1 ,3) = B( 1 ,3 ) / A( 1 )
           B( 2 ,1) = B( 2 ,1 ) - A( 2 ) * B( 1 ,1 )
           B( 2 ,2) = B( 2 ,2 ) - A( 2 ) * B( 1 ,2 )
           B( 2 ,3) = B( 2 ,3 ) - A( 2 ) * B( 1 ,3 )
           B( 2 ,1) = B( 2 ,1 ) / A( 3 )
           B( 2 ,2) = B( 2 ,2 ) / A( 3 )
           B( 2 ,3) = B( 2 ,3 ) / A( 3 )
           B( 3 ,1) = B( 3 ,1 ) - A( 4 ) * B( 1 ,1 )
           B( 3 ,2) = B( 3 ,2 ) - A( 4 ) * B( 1 ,2 )
           B( 3 ,3) = B( 3 ,3 ) - A( 4 ) * B( 1 ,3 )
           B( 3 ,1) = B( 3 ,1 ) - A( 5 ) * B( 2 ,1 )
           B( 3 ,2) = B( 3 ,2 ) - A( 5 ) * B( 2 ,2 )
           B( 3 ,3) = B( 3 ,3 ) - A( 5 ) * B( 2 ,3 )
           B( 3 ,1) = B( 3 ,1 ) / A( 6 )
           B( 3 ,2) = B( 3 ,2 ) / A( 6 )
           B( 3 ,3) = B( 3 ,3 ) / A( 6 )
           B( 4 ,1) = B( 4 ,1 ) - A( 7 ) * B( 1 ,1 )
           B( 4 ,2) = B( 4 ,2 ) - A( 7 ) * B( 1 ,2 )
           B( 4 ,3) = B( 4 ,3 ) - A( 7 ) * B( 1 ,3 )
           B( 4 ,1) = B( 4 ,1 ) - A( 8 ) * B( 2 ,1 )
           B( 4 ,2) = B( 4 ,2 ) - A( 8 ) * B( 2 ,2 )
           B( 4 ,3) = B( 4 ,3 ) - A( 8 ) * B( 2 ,3 )
           B( 4 ,1) = B( 4 ,1 ) - A( 9 ) * B( 3 ,1 )
           B( 4 ,2) = B( 4 ,2 ) - A( 9 ) * B( 3 ,2 )
           B( 4 ,3) = B( 4 ,3 ) - A( 9 ) * B( 3 ,3 )
           B( 4 ,1) = B( 4 ,1 ) / A( 10 )
           B( 4 ,2) = B( 4 ,2 ) / A( 10 )
           B( 4 ,3) = B( 4 ,3 ) / A( 10 )
        ELSE IF ( N .EQ. 4 ) THEN
           B( 1 ,1) = B( 1 ,1 ) / A( 1 )
           B( 1 ,2) = B( 1 ,2 ) / A( 1 )
           B( 1 ,3) = B( 1 ,3 ) / A( 1 )
           B( 1 ,4) = B( 1 ,4 ) / A( 1 )
           B( 2 ,1) = B( 2 ,1 ) - A( 2 ) * B( 1 ,1 )
           B( 2 ,2) = B( 2 ,2 ) - A( 2 ) * B( 1 ,2 )
           B( 2 ,3) = B( 2 ,3 ) - A( 2 ) * B( 1 ,3 )
           B( 2 ,4) = B( 2 ,4 ) - A( 2 ) * B( 1 ,4 )
           B( 2 ,1) = B( 2 ,1 ) / A( 3 )
           B( 2 ,2) = B( 2 ,2 ) / A( 3 )
           B( 2 ,3) = B( 2 ,3 ) / A( 3 )
           B( 2 ,4) = B( 2 ,4 ) / A( 3 )
           B( 3 ,1) = B( 3 ,1 ) - A( 4 ) * B( 1 ,1 )
           B( 3 ,2) = B( 3 ,2 ) - A( 4 ) * B( 1 ,2 )
           B( 3 ,3) = B( 3 ,3 ) - A( 4 ) * B( 1 ,3 )
           B( 3 ,4) = B( 3 ,4 ) - A( 4 ) * B( 1 ,4 )
           B( 3 ,1) = B( 3 ,1 ) - A( 5 ) * B( 2 ,1 )
           B( 3 ,2) = B( 3 ,2 ) - A( 5 ) * B( 2 ,2 )
           B( 3 ,3) = B( 3 ,3 ) - A( 5 ) * B( 2 ,3 )
           B( 3 ,4) = B( 3 ,4 ) - A( 5 ) * B( 2 ,4 )
           B( 3 ,1) = B( 3 ,1 ) / A( 6 )
           B( 3 ,2) = B( 3 ,2 ) / A( 6 )
           B( 3 ,3) = B( 3 ,3 ) / A( 6 )
           B( 3 ,4) = B( 3 ,4 ) / A( 6 )
           B( 4 ,1) = B( 4 ,1 ) - A( 7 ) * B( 1 ,1 )
           B( 4 ,2) = B( 4 ,2 ) - A( 7 ) * B( 1 ,2 )
           B( 4 ,3) = B( 4 ,3 ) - A( 7 ) * B( 1 ,3 )
           B( 4 ,4) = B( 4 ,4 ) - A( 7 ) * B( 1 ,4 )
           B( 4 ,1) = B( 4 ,1 ) - A( 8 ) * B( 2 ,1 )
           B( 4 ,2) = B( 4 ,2 ) - A( 8 ) * B( 2 ,2 )
           B( 4 ,3) = B( 4 ,3 ) - A( 8 ) * B( 2 ,3 )
           B( 4 ,4) = B( 4 ,4 ) - A( 8 ) * B( 2 ,4 )
           B( 4 ,1) = B( 4 ,1 ) - A( 9 ) * B( 3 ,1 )
           B( 4 ,2) = B( 4 ,2 ) - A( 9 ) * B( 3 ,2 )
           B( 4 ,3) = B( 4 ,3 ) - A( 9 ) * B( 3 ,3 )
           B( 4 ,4) = B( 4 ,4 ) - A( 9 ) * B( 3 ,4 )
           B( 4 ,1) = B( 4 ,1 ) / A( 10 )
           B( 4 ,2) = B( 4 ,2 ) / A( 10 )
           B( 4 ,3) = B( 4 ,3 ) / A( 10 )
           B( 4 ,4) = B( 4 ,4 ) / A( 10 )
        ELSE
           CALL DSCAL ( N, 1.0D0 / A( 1 ), B( 1 ,1 ), LB )
           CALL DAXPY ( N, -A( 2 ), B( 1 ,1), LB, B( 2 ,1), LB )
           CALL DSCAL ( N, 1.0D0 / A( 3 ), B( 2 ,1 ), LB )
           CALL DAXPY ( N, -A( 4 ), B( 1 ,1), LB, B( 3 ,1), LB )
           CALL DAXPY ( N, -A( 5 ), B( 2 ,1), LB, B( 3 ,1), LB )
           CALL DSCAL ( N, 1.0D0 / A( 6 ), B( 3 ,1 ), LB )
           CALL DAXPY ( N, -A( 7 ), B( 1 ,1), LB, B( 4 ,1), LB )
           CALL DAXPY ( N, -A( 8 ), B( 2 ,1), LB, B( 4 ,1), LB )
           CALL DAXPY ( N, -A( 9 ), B( 3 ,1), LB, B( 4 ,1), LB )
           CALL DSCAL ( N, 1.0D0 / A( 10 ), B( 4 ,1 ), LB )
      END IF
!
      RETURN 
      END SUBROUTINE TRSM4_4 
