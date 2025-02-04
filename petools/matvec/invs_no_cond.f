#include <mk5_preprocessor_directives.inc>
      SUBROUTINE INVS_NO_COND ( N, MAT, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine INVS_NO_COND inverts the square symmetric, positively      *
! *   defined matrix in packed upper triangular representation without   *
! *   evaluation of the condition number.                                *
! *                                                                      *
! *   Matrix inversion is performed in three steps:                      *
! *   1) The matrix is decomposed on a product of the triangular matrix  *
! *      and its transpose as MAT = U(T) * U using Cholesky              *
! *      decomposition.                                                  *
! *   2) Triangular matrix is inverted.                                  *
! *   3) The invert of the initial matrix is computed as a product of    *
! *      the invert of the Cholesky factor and its transpose:            *
! *      MAT^{-1} = U^{-1} * U^{-1}(T).                                  *
! *                                                                      *
! *   Direct highly optimized code is used for dimensions                *
! *   =< DB__INVMAT_DIR. For higher dimensions the matrix is             *
! *   transformed to the packed recursive format and then recursive      *
! *   algorithm of Andersen et al. is used. Recursions are done down     *
! *   the dimension of DB__INVMAT. Recurrent algorithm requires          *
! *   additional memory of 1/4 of the size of the initial matrix.        *
! *                                                                      *
! *                                                                      *
! *   Constants DB__INVMAT_MIN, DB__INVMAT_MAX, COND__MAX are defined    *
! *   in matvec.i                                                        *
! *                                                                      *
! *   Reference:                                                         *
! *     B.S. Andersen, J.A. Gunnels, F. Gustavson, J. Wasniewski,        *
! *    "A recursive formulation of the inversion of symmetric positive   *
! *     definite matrices in packed storage data format", PARA 2002,     *
! *     Lecture Notes in Computer Science, vol. 2367. pp. 287--296, 2002 *
! *                                                                      *
! * ________________________ Input parameters: _________________________ *
! *                                                                      *
! *       N ( INTEGER*4 ) -- Matrix dimension.                           *
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
! *     MAT ( REAL*8    ) -- Input:  initial matrix in packed upper      *
! *                                  triangular representation.          *
! *                          Output: inverse of the initial matrix.      *
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
! *  ### 20-FEB-1989   INVS_NO_COND  v8.0 (c) L. Petrov 30-NOV-2002 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'matvec.i'
      INTEGER*4  N, IUER
      REAL*8     MAT(*)
      REAL*8     Z(MAX__DIM), DET, MAT1_OLD
      INTEGER*4  ARG_INVS(4)
      INTEGER*4  INVS_FUNC(DB__INVMAT_MAX)
      CHARACTER  STR*32, STR1*32
      EXTERNAL   INVS_3,  INVS_4,  INVS_5,  INVS_6,  INVS_7,  INVS_8,     &
     &           INVS_9,  INVS_10, INVS_11, INVS_12, INVS_13, INVS_14,    &
     &           INVS_15, INVS_16, INVS_17, INVS_18, INVS_19, INVS_20,    &
     &           INVS_21, INVS_22, INVS_23, INVS_24, INVS_25, INVS_26,    &
     &           INVS_27, INVS_28, INVS_29, INVS_30, INVS_31, INVS_32
      INTEGER*4  IT, IER
      REAL*8,    ALLOCATABLE :: MAT_TEMP(:)
      REAL*8     EPS
      INTEGER*4  NB
      INTEGER*4, EXTERNAL :: I_LEN, FUNC_ADDRESS
!
      IF ( N .EQ. 1 ) THEN
!
! -------- Dimension 1
!
           IF ( DABS(MAT(1)) .LT. 1.D0/COND__MAX ) THEN
                CALL ERR_LOG ( 1211, IUER, 'INVS_NO_COND', 'Matrix is '// &
     &              '(almost) singular' )
                RETURN
              ELSE
                MAT(1) = 1.D0/MAT(1)
                CALL ERR_LOG ( 0, IUER )
                RETURN
           END IF
         ELSE IF ( N .EQ. 2 ) THEN
!
! -------- Dimension 2
!
           DET = MAT(1)*MAT(3) - MAT(2)**2
           MAT1_OLD = MAT(1)
           MAT(1) =  MAT(3)/DET
           MAT(2) = -MAT(2)/DET
           MAT(3) = MAT1_OLD/DET
         ELSE IF ( N .LE. DB__INVMAT_DIR ) THEN
!
! ======== Small dimensions. Schedule highly optimized code for the dimension N
!
           IF ( N .EQ. 3 ) THEN
                INVS_FUNC(3)  = FUNC_ADDRESS ( INVS_3 )
              ELSE IF ( N .EQ. 4 ) THEN
                INVS_FUNC(4)  = FUNC_ADDRESS ( INVS_4 )
              ELSE IF ( N .EQ. 5 ) THEN
                INVS_FUNC(5)  = FUNC_ADDRESS ( INVS_5 )
              ELSE IF ( N .EQ. 6 ) THEN
                INVS_FUNC(6)  = FUNC_ADDRESS ( INVS_6 )
              ELSE IF ( N .EQ. 7 ) THEN
                INVS_FUNC(7)  = FUNC_ADDRESS ( INVS_7 )
              ELSE IF ( N .EQ. 8 ) THEN
                INVS_FUNC(8)  = FUNC_ADDRESS ( INVS_8 )
              ELSE
                INVS_FUNC(9)  = FUNC_ADDRESS ( INVS_9  )
                INVS_FUNC(10) = FUNC_ADDRESS ( INVS_10 )
                INVS_FUNC(11) = FUNC_ADDRESS ( INVS_11 )
                INVS_FUNC(12) = FUNC_ADDRESS ( INVS_12 )
                INVS_FUNC(13) = FUNC_ADDRESS ( INVS_13 )
                INVS_FUNC(14) = FUNC_ADDRESS ( INVS_14 )
                INVS_FUNC(15) = FUNC_ADDRESS ( INVS_15 )
                INVS_FUNC(16) = FUNC_ADDRESS ( INVS_16 )
                INVS_FUNC(17) = FUNC_ADDRESS ( INVS_17 )
                INVS_FUNC(18) = FUNC_ADDRESS ( INVS_18 )
                INVS_FUNC(19) = FUNC_ADDRESS ( INVS_19 )
                INVS_FUNC(20) = FUNC_ADDRESS ( INVS_20 )
                INVS_FUNC(21) = FUNC_ADDRESS ( INVS_21 )
                INVS_FUNC(22) = FUNC_ADDRESS ( INVS_22 )
                INVS_FUNC(23) = FUNC_ADDRESS ( INVS_23 )
                INVS_FUNC(24) = FUNC_ADDRESS ( INVS_24 )
                INVS_FUNC(25) = FUNC_ADDRESS ( INVS_25 )
                INVS_FUNC(26) = FUNC_ADDRESS ( INVS_26 )
                INVS_FUNC(27) = FUNC_ADDRESS ( INVS_27 )
                INVS_FUNC(28) = FUNC_ADDRESS ( INVS_28 )
                INVS_FUNC(29) = FUNC_ADDRESS ( INVS_29 )
                INVS_FUNC(30) = FUNC_ADDRESS ( INVS_30 )
                INVS_FUNC(31) = FUNC_ADDRESS ( INVS_31 )
                INVS_FUNC(32) = FUNC_ADDRESS ( INVS_32 )
            END IF
!
! -------- Build the argument lists
!
           EPS = DB__INVMAT_EPS
           ARG_INVS(1) = 3
           ARG_INVS(2) = LOC(MAT)
           ARG_INVS(3) = LOC(EPS)
           ARG_INVS(4) = LOC(IER)
!
! -------- Invert the matrix
!
           CALL LIB$CALLG ( ARG_INVS, %VAL(INVS_FUNC(N)) )
           IF ( IER .NE. 0 ) THEN
                CALL CLRCH ( STR )
                CALL INCH ( IER, STR )
                CALL ERR_LOG ( IER, IUER, 'INVS_NO_COND', 'Error in matrix '// &
     &              'inversion at the '//STR(1:I_LEN(STR))//'-th step' )
                RETURN 
           END IF
         ELSE 
!
! ======== Large dimensions
!
!
! -------- Allocate additional memory for reordering
!
           NB = (N-N/2)
           ALLOCATE ( MAT_TEMP((NB*(NB+1))/2), STAT=IER )
           IF ( IER .NE. 0 ) THEN
                CALL CLRCH ( STR ) 
                CALL IINCH ( (4*NB*(NB+1)), STR )
                CALL ERR_LOG ( 1217, IUER, 'INVS_NO_COND', 'Failure to allocate '// &
     &               STR(1:I_LEN(STR))//' bytes of dynamic memory for '// & 
     &              'a temporary array needed for the recursive algorithm '// &
     &              'for matrix inversion' )
                RETURN 
           END IF
#ifdef BLAS_NOT_A_NUMBER
           CALL MEMSET ( MAT_TEMP, 0, %VAL(8*(NB*(NB+1))/2) )
#endif
!
! -------- Matrix reordering to recursive packed upper triangular format
!
           CALL DREORDER5 ( N, MAT, MAT_TEMP )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 1218, IUER, 'INVS_NO_COND', 'Error in DREORDER '// &
     &              'IER = '//STR )
                DEALLOCATE ( MAT_TEMP, STAT=IER )
                RETURN 
           END IF
!
! -------- Cholesky decomposition
!
           IER = 0
           CALL DRPPTRF2 ( N, MAT, IER )
           IF ( IER .NE. 0 ) THEN
                CALL CLRCH  ( STR ) 
                CALL INCH   ( IER, STR ) 
                CALL ERR_LOG ( IER, IUER, 'INVS_NO_COND', 'Error in matrix '// &
     &              'factorization at the '//STR(1:I_LEN(STR))//'-th step' )
                DEALLOCATE ( MAT_TEMP, STAT=IER )
                RETURN 
           END IF
!
! -------- Inversion of the triangular Cholesky factor
!
           CALL DRPTRTRI2 ( N, MAT )
!
! -------- Get the invert as A : = U * U(T)
!
           CALL DRPTRRK3  ( N, MAT )
!
! -------- Matrix reordering from recursive packed upper triangular format
!
           CALL DREORDER6 ( N, MAT, MAT_TEMP )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 1219, IUER, 'INVS_NO_COND', 'Error in DREORDER '// &
     &              'IER = '//STR )
                DEALLOCATE ( MAT_TEMP, STAT=IER )
                RETURN 
           END IF
!
! -------- Return dynamic memory
!
           DEALLOCATE ( MAT_TEMP, STAT=IER )
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  INVS_NO_COND  #!#
