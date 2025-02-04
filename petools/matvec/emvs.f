        FUNCTION EMVS ( N, A, IT, ER, IUER )
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
! *         N ( INTEGER*4 ) -- Matrix dimension.                         *
! *         A ( REAL*8    ) -- The matrix under investigation in upper   *
! *                            triangular representation.                *
! *                                                                      *
! * ________________________ Output parameters _________________________ *
! *                                                                      *
! *    <EMVS> ( REAL*8    ) -- Maximal by module eigenvalue.             *
! *        IT ( INTEGER*4 ) -- The number of executed iterations.        *
! *        ER ( REAL*8    ) -- The estimate of the uncertainty obtained  *
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
! *   ###  28-MAY-1991    EMVS   v2.2  (c)  L. Petrov  30-JAN-2006  ###  *
! *                                                                      *
! ************************************************************************
        IMPLICIT   NONE 
        INCLUDE   'matvec.i'
        REAL*8     EMVS, A(*), ER
        REAL*8     EPS, EM
        INTEGER*4  N, IUER
        CHARACTER STR*20
        INTEGER*4  MAXIT
        PARAMETER ( MAXIT = 20 )
        PARAMETER ( EPS = 0.1 )
        REAL*8     V1(MAX__DIM ), V2(MAX__DIM )
        REAL*8     R, ER_L, EM_L, EM_LL, D1, D2, EPS_L
        INTEGER*4  IT, LEP, IPOR, J1, J2 , IER
        PARAMETER ( LEP = 30 )
        REAL*8,    EXTERNAL :: DP_VV_V 
        INTEGER*4, EXTERNAL :: I_LEN
        LOGICAL*4, EXTERNAL :: IS_R8_NAN
!
        EPS_L=EPS*3.D0
!
! ----- Intial value of vector V1: --  +1 and -1
!
        R = 1.D0
        DO 410 J1=1,N
           V1(J1) = R
           R = -R
  410   CONTINUE
!
        EM=1.D14
        ER=1.D14
!
! ----- Cycle over iteratcions 
!
        DO 420 J2=1,MAXIT
!
! -------- Multiply symmetric matrix A  by vector V1
!
           CALL MUL_MV_SV_V ( N, A, N, V1, N, V2, IER )
!
! -------- Compute dor products 
!
           EM_LL=EM_L  !  Set last-last of eigen value 
           EM_L=EM     !  Set last of eigen value 
           ER_L=ER     !  Set last uncertainty 
!
! -------- Compute the maximal eigen value
!
           EM = DP_VV_V ( N, V1, V2 ) / DP_VV_V ( N, V1, V1 )
!
! -------- Compute uncertainty 
!
           IF ( IS_R8_NAN(EM) ) THEN
                WRITE ( 6, * ) ' EM = ', EM 
                WRITE ( 6, * ) ' V2 = ', V2(1:N)
                WRITE ( 6, * ) ' N =', N
                WRITE ( 6, * ) ' Par1: ', DP_VV_V ( N, V1, V1 ) 
                WRITE ( 6, * ) ' Par2: ', DP_VV_V ( N, V1, V2 ) 
                CALL ERR_LOG ( 1241, IUER, 'EMVS', 'Error in an attempt '//   &
     &              'to compute maximal by modulo eigenvalue: NaN '// &
     &              'number occured' )
                RETURN 
           END IF
!
           IF ( DLOG10(DABS(EM_L)+1.D-32) - DLOG10(DABS(EM)+1.D-32) &
     &     .GT. LEP ) THEN
!
! ------------- Thread of overflow 
!
                CALL ERR_LOG ( 1242, IUER, 'EMVS', 'Error in an attempt '//   &
     &              'to compute maximal by modulo eigenvalue: the process '// &
     &              'is diverging' )
                EM=1.D30
                RETURN
           END IF
           ER=DABS ( (EM-EM_L)/EM )
!
! -------- Norm the vector
!
           R=1.D0/EM
           CALL COPY_R8  ( N, V2, V1 )
           CALL MUL_VC_V ( N, V1, R )
!
! -------- Condition of iterations end: the last and last-last values are 
! -------- less than EPS
!
           IF ( ER_L .LT. EPS_L  .AND.  ER .LT. EPS ) GOTO 810
  420   CONTINUE
        J2=MAXIT
        IF ( DABS(ER) .GE. EPS ) THEN
             EM=-1.D0
             CALL CLRCH ( STR )
             CALL INCH  ( MAXIT, STR )
             CALL ERR_LOG ( 1243, IUER, 'EMVS', 'Error in an attempt '//   &
     &           'to compute the maximal; by module eigenvalue -- '//      &
     &            STR(1:I_LEN(STR))//' iterations was not enough to '//    &
     &           'reach desirable precision' )
             RETURN
        END IF
  810   CONTINUE
        IT=J2
        EMVS = EM
        CALL ERR_LOG ( 0, IUER )
        RETURN
        END  !#!  EMVS  #!#
