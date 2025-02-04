#include <mk5_preprocessor_directives.inc>
      SUBROUTINE TRMM8_2  ( M, A, B, LB )
! ************************************************************************
! *                                                                      *
! *   Computes B := B*A(T)                                               *
! *   B dimensioned as (LB,N), A in upper triangular format.             *
! *                                                                      *
! * ### Source code was created automatically                         ## *
! * ### Generator: INVS_GENERATOR (c) L. Petrov version of 2002.11.28 ## *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INTEGER*4  M, LB
      REAL*8     A(*), B(LB,2 )
      REAL*8     S, S1, S2, S3, S4
      INTEGER*4  J1
      IF ( M .EQ. 1 ) THEN
           S = 0.0D0
           S = S + B( 1, 1 ) * A( 1 )
           S = S + B( 1, 2 ) * A( 2 )
           B(1, 1 ) = S
           S = 0.0D0
           S = S + B( 1, 2 ) * A( 3 )
           B(1, 2 ) = S
         ELSE IF ( M .EQ. 2 ) THEN
           S1 = 0.0D0
           S2 = 0.0D0
           S1 = S1 + B( 1, 1 ) * A( 1 )
           S2 = S2 + B( 2, 1 ) * A( 1 )
           S1 = S1 + B( 1, 2 ) * A( 2 )
           S2 = S2 + B( 2, 2 ) * A( 2 )
           B( 1, 1 ) = S1
           B( 2, 1 ) = S2
           S1 = 0.0D0
           S2 = 0.0D0
           S1 = S1 + B( 1, 2 ) * A( 3 )
           S2 = S2 + B( 2, 2 ) * A( 3 )
           B( 1, 2 ) = S1
           B( 2, 2 ) = S2
         ELSE IF ( M .EQ. 3 ) THEN
           S1 = 0.0D0
           S2 = 0.0D0
           S3 = 0.0D0
           S1 = S1 + B( 1, 1 ) * A( 1 )
           S2 = S2 + B( 2, 1 ) * A( 1 )
           S3 = S3 + B( 3, 1 ) * A( 1 )
           S1 = S1 + B( 1, 2 ) * A( 2 )
           S2 = S2 + B( 2, 2 ) * A( 2 )
           S3 = S3 + B( 3, 2 ) * A( 2 )
           B( 1, 1 ) = S1
           B( 2, 1 ) = S2
           B( 3, 1 ) = S3
           S1 = 0.0D0
           S2 = 0.0D0
           S3 = 0.0D0
           S1 = S1 + B( 1, 2 ) * A( 3 )
           S2 = S2 + B( 2, 2 ) * A( 3 )
           S3 = S3 + B( 3, 2 ) * A( 3 )
           B( 1, 2 ) = S1
           B( 2, 2 ) = S2
           B( 3, 2 ) = S3
         ELSE IF ( M .EQ. 4 ) THEN
           S1 = 0.0D0
           S2 = 0.0D0
           S3 = 0.0D0
           S4 = 0.0D0
           S1 = S1 + B( 1, 1 ) * A( 1 )
           S2 = S2 + B( 2, 1 ) * A( 1 )
           S3 = S3 + B( 3, 1 ) * A( 1 )
           S4 = S4 + B( 4, 1 ) * A( 1 )
           S1 = S1 + B( 1, 2 ) * A( 2 )
           S2 = S2 + B( 2, 2 ) * A( 2 )
           S3 = S3 + B( 3, 2 ) * A( 2 )
           S4 = S4 + B( 4, 2 ) * A( 2 )
           B( 1, 1 ) = S1
           B( 2, 1 ) = S2
           B( 3, 1 ) = S3
           B( 4, 1 ) = S4
           S1 = 0.0D0
           S2 = 0.0D0
           S3 = 0.0D0
           S4 = 0.0D0
           S1 = S1 + B( 1, 2 ) * A( 3 )
           S2 = S2 + B( 2, 2 ) * A( 3 )
           S3 = S3 + B( 3, 2 ) * A( 3 )
           S4 = S4 + B( 4, 2 ) * A( 3 )
           B( 1, 2 ) = S1
           B( 2, 2 ) = S2
           B( 3, 2 ) = S3
           B( 4, 2 ) = S4
         ELSE
           DO 810 J1=1,M
              S = 0.0D0
              S = S + B ( J1, 1 ) * A( 1 )
              S = S + B ( J1, 2 ) * A( 2 )
              B ( J1, 1 ) = S
              S = 0.0D0
              S = S + B ( J1, 2 ) * A( 3 )
              B ( J1, 2 ) = S
  810      CONTINUE
      END IF
!
      RETURN 
      END SUBROUTINE TRMM8_2 
