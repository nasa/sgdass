#include <mk5_preprocessor_directives.inc>
      SUBROUTINE EMVS_5  ( A, EMVS, IUER )
! ************************************************************************
! *                                                                      *
! *     Routine EMVS comptes the maximal by module eigenvalue of the     *
! *     square symmetric mstrix using the methods of consequitive        *
! *     iterations ( Faddeev, D.K., Faddeva V.N. "Computational methods  *
! *     of linear algebra, M.-L., 1963, pp.349-372 (in Russian) ).       *
! *                                                                      *
! *     This methods will not give a result if                           *
! *     1) There exists an eigenvalue L2, such that L1=-L2 where K1      *
! *        is the maximal eigen value;                                   *
! *     2) The maximal eigenvalue forms a complex-conjugate pair;        *
! *     3) L1 occupies Jordan cell of the 2-nd order.                    *
! *                                                                      *
! *     In this case the error parameter IUER will signal that the       *
! *     required precision, 0.1 has not been achived for 20 iteration.   *
! *                                                                      *
! *     Convergance of the process is accelerated by computing dot       *
! *     products on each step of iterations.                             *
! *                                                                      *
! * ________________________ Input parameters: _________________________ *
! *                                                                      *
! *         A ( REAL*8    ) -- The matrix under investigation in upper   *
! *                            triangular representation.                *
! *                                                                      *
! * ________________________ Output parameters _________________________ *
! *                                                                      *
! *      EMVS ( REAL*8    ) -- Maximal by module eigenvalue.             *
! *                            at the last iteration step.               *
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
! *    IUER ( INTEGER*4, OPT ) -- Universal error handler.               *
! *                           Input: switch IUER=0 -- no error messages  *
! *                                  will be generated even in the case  *
! *                                  of error. IUER=-1 -- in the case of *
! *                                  error the message will be put on    *
! *                                  stdout.                             *
! *                           Output: 0 in the case of successful        *
! *                                   completion and non-zero in the     *
! *                                   case of error.                     *
! *                                                                      *
! * ### Source code was created automatically                         ## *
! * ### Generator: INVS_GENERATOR (c) L. Petrov version of 2002.11.30 ## *
! *                                                                      *
! ************************************************************************
       IMPLICIT   NONE 
       REAL*8     A(*), EMVS
       INTEGER*4  N, IUER
       CHARACTER STR*20
       INTEGER*4  MAXIT, LEP
       PARAMETER ( MAXIT = 16 )
       PARAMETER ( LEP = 30 )
       REAL*8     V1( 5 ), V2( 5 ), EM, EPS, ER
       PARAMETER ( EPS = 0.1 )
       REAL*8     R, S, ER_L, EM_L, EM_LL, D1, D2, EPS_L, V1_V1, V1_V2
       INTEGER*4  IT, IPOR, J2
       REAL*8  DP_VV_V 
       INTEGER*4  I_LEN
!
       EPS_L = EPS * 3.D0
       V1( 1 ) = 1.0
       V1( 2 ) = -1.0
       V1( 3 ) = 1.0
       V1( 4 ) = -1.0
       V1( 5 ) = 1.0
       EM = 1.D14
       ER = 1.D14
!
       DO 420 J2=1,MAXIT
          V2( 1 ) = 0.0D0
          V2( 1 ) = V2( 1 ) + A( 1 ) * V1( 1 )
          V2( 1 ) = V2( 1 ) + A( 2 ) * V1( 2 )
          V2( 1 ) = V2( 1 ) + A( 4 ) * V1( 3 )
          V2( 1 ) = V2( 1 ) + A( 7 ) * V1( 4 )
          V2( 1 ) = V2( 1 ) + A( 11 ) * V1( 5 )
          V2( 2 ) = 0.0D0
          V2( 2 ) = V2( 2 ) + A( 2 ) * V1( 1 )
          V2( 2 ) = V2( 2 ) + A( 3 ) * V1( 2 )
          V2( 2 ) = V2( 2 ) + A( 5 ) * V1( 3 )
          V2( 2 ) = V2( 2 ) + A( 8 ) * V1( 4 )
          V2( 2 ) = V2( 2 ) + A( 12 ) * V1( 5 )
          V2( 3 ) = 0.0D0
          V2( 3 ) = V2( 3 ) + A( 4 ) * V1( 1 )
          V2( 3 ) = V2( 3 ) + A( 5 ) * V1( 2 )
          V2( 3 ) = V2( 3 ) + A( 6 ) * V1( 3 )
          V2( 3 ) = V2( 3 ) + A( 9 ) * V1( 4 )
          V2( 3 ) = V2( 3 ) + A( 13 ) * V1( 5 )
          V2( 4 ) = 0.0D0
          V2( 4 ) = V2( 4 ) + A( 7 ) * V1( 1 )
          V2( 4 ) = V2( 4 ) + A( 8 ) * V1( 2 )
          V2( 4 ) = V2( 4 ) + A( 9 ) * V1( 3 )
          V2( 4 ) = V2( 4 ) + A( 10 ) * V1( 4 )
          V2( 4 ) = V2( 4 ) + A( 14 ) * V1( 5 )
          V2( 5 ) = 0.0D0
          V2( 5 ) = V2( 5 ) + A( 11 ) * V1( 1 )
          V2( 5 ) = V2( 5 ) + A( 12 ) * V1( 2 )
          V2( 5 ) = V2( 5 ) + A( 13 ) * V1( 3 )
          V2( 5 ) = V2( 5 ) + A( 14 ) * V1( 4 )
          V2( 5 ) = V2( 5 ) + A( 15 ) * V1( 5 )
          EM_LL=EM_L  !  Set last-last of eigen value
          EM_L=EM     !  Set last of eigen value
          ER_L=ER     !  Set last uncertainty
          V1_V2 = 0.0D0
          V1_V1 = 0.0D0
!
          V1_V2 = V1_V2 + V1( 1  ) * V2( 1 )
          V1_V1 = V1_V1 + V1( 1  ) * V1( 1 )
          V1_V2 = V1_V2 + V1( 2  ) * V2( 2 )
          V1_V1 = V1_V1 + V1( 2  ) * V1( 2 )
          V1_V2 = V1_V2 + V1( 3  ) * V2( 3 )
          V1_V1 = V1_V1 + V1( 3  ) * V1( 3 )
          V1_V2 = V1_V2 + V1( 4  ) * V2( 4 )
          V1_V1 = V1_V1 + V1( 4  ) * V1( 4 )
          V1_V2 = V1_V2 + V1( 5  ) * V2( 5 )
          V1_V1 = V1_V1 + V1( 5  ) * V1( 5 )
!
          EM = V1_V2/V1_V1
          IF ( DLOG10(DABS(EM_L)+1.D-32) - DLOG10(DABS(EM)+1.D-32) .GT. LEP ) THEN
              CALL ERR_LOG ( 1241, IUER, 'EMVS_5 ', 'Error in an attempt '//   &
     &             'to compute maximal by modulo eigenvalue: the process '// &
     &             'is diverging' )
               EM=1.D30
               RETURN
          END IF
          ER = DABS ( (EM-EM_L)/EM )
!
          V1( 1 ) = V2( 1 )/EM
          V1( 2 ) = V2( 2 )/EM
          V1( 3 ) = V2( 3 )/EM
          V1( 4 ) = V2( 4 )/EM
          V1( 5 ) = V2( 5 )/EM
!
          IF ( ER_L .LT. EPS_L  .AND.  ER .LT. EPS ) GOTO 810
  420 CONTINUE
!
      J2=MAXIT
      IF ( DABS(ER) .GE. EPS ) THEN
           EM=-1.D0
           CALL CLRCH ( STR )
           CALL INCH  ( MAXIT, STR )
          CALL ERR_LOG ( 1242, IUER, 'EMVS_5 ', 'Error in an attempt '//   &
     &          'to compute the maximal; by module eigenvalue -- '//      &
     &           STR(1:I_LEN(STR))//' iterations was not enough to '//    &
     &          'reach desirable precision' )
           RETURN
      END IF
  810 CONTINUE
      IT = J2
      EMVS = EM
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  EMVS_5   #!#
