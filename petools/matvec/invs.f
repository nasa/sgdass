#include <mk5_preprocessor_directives.inc>
      SUBROUTINE OPT_INVS ( N, MAT, RC, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine INVS inverts a square symmetric, positively defined        *
! *   matrix in packed upper triangular representation. Spectral         *
! *   condition number is calculated in the process of computation.      *
! *                                                                      *
! *   Order of elements of a matrix in the packed upper triangular       *
! *   representation:                                                    *
! *                                                                      *
! *   1   2   4   7   ...                                                *
! *       3   5   8   ...                                                *
! *           6   9   ...                                                *
! *              10   ...                                                *
! *              ...  ...                                                *
! *                                                                      *
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
! *   If during factorization a diagonal element less than               *
! *   DB__INVMAT_EPS is found then, INVS returns the error code.         *
! *   In this case the output matrix is neither initial matrix, nor its  *
! *   inverse.                                                           *
! *                                                                      *
! *   INVS computes the maximal eigenvalue of the initial matrix and     *
! *   the maximal eigenvalue number of the invert. Their product is      *
! *   returned as a condition number. If the condition number exceeds    *
! *   COND__MAX then the error message is returned. Precision of         *
! *   computing condition number is 1%. COND__MAX is defiend in matvec.i *
! *   The environment variable COND_MAX, if defined, overrides it.       *
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
! * ________________________ Output parameters _________________________ *
! *                                                                      *
! *      RC ( REAL*8    ) -- Spectral condition number.                  *
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
! *  ### 20-FEB-1989       INVS    v9.1(c)  L. Petrov  26-JUN-2021  ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'matvec.i'
      INTEGER*4  N, IUER
      REAL*8     MAT(*), RC
      REAL*8     DET, MAT1_OLD, MAT_INV(136), MATO(136), MAT_ORIG(136)
      REAL*8     EM_MAX, EM_MIN, ER, EMVS
      ADDRESS__TYPE :: ARG_INVS(4), ARG_EMVS(4), ARG_SS(8)
      ADDRESS__TYPE :: INVS_FUNC(DB__INVMAT_MAX), EMVS_FUNC(DB__INVMAT_MAX), &
     &                 SS_FUNC(DB__INVMAT_MAX)
      CHARACTER  STR*32, STR1*32
      ADDRESS__TYPE, EXTERNAL :: &
     &           INVS_3,  INVS_4,  INVS_5,  INVS_6,  INVS_7,  INVS_8,     &
     &           INVS_9,  INVS_10, INVS_11, INVS_12, INVS_13, INVS_14,    &
     &           INVS_15, INVS_16, INVS_17, INVS_18, INVS_19, INVS_20,    &
     &           INVS_21, INVS_22, INVS_23, INVS_24, INVS_25, INVS_26,    &
     &           INVS_27, INVS_28, INVS_29, INVS_30, INVS_31, INVS_32
      ADDRESS__TYPE, EXTERNAL :: &
     &           EMVS_3,  EMVS_4,  EMVS_5,  EMVS_6,  EMVS_7,  EMVS_8,     &
     &           EMVS_9,  EMVS_10, EMVS_11, EMVS_12, EMVS_13, EMVS_14,    &
     &           EMVS_15, EMVS_16, EMVS_17, EMVS_18, EMVS_19, EMVS_20,    &
     &           EMVS_21, EMVS_22, EMVS_23, EMVS_24, EMVS_25, EMVS_26,    &
     &           EMVS_27, EMVS_28, EMVS_29, EMVS_30, EMVS_31, EMVS_32
      ADDRESS__TYPE, EXTERNAL   :: &
     &           MUL_MM_SS_S_5,  MUL_MM_SS_S_6,  MUL_MM_SS_S_7,  &
     &           MUL_MM_SS_S_8,  MUL_MM_SS_S_9,  MUL_MM_SS_S_10, &
     &           MUL_MM_SS_S_11, MUL_MM_SS_S_12, MUL_MM_SS_S_13, &
     &           MUL_MM_SS_S_14, MUL_MM_SS_S_15, MUL_MM_SS_S_16
      INTEGER*4  KP, IT, J1, J2, IER
      REAL*8     COND_MAX
      LOGICAL*4  INVS_COMMON_INIT
      DATA       INVS_COMMON_INIT / .FALSE. /
      COMMON   / INVS_COMMON / COND_MAX, INVS_COMMON_INIT
      REAL*8,    ALLOCATABLE :: MAT_TEMP(:)
      REAL*8     EPS
      INTEGER*4  NB
      INTEGER*8  NBB
      ADDRESS__TYPE, EXTERNAL :: FUNC_ADDRESS
      INTEGER*4, EXTERNAL :: ILEN, I_LEN
!
      IF ( .NOT. INVS_COMMON_INIT ) THEN
!
! --------- If INVS is called the first time, check for the enviroment variable
! --------- COND_MAX which can override default COND__MAX defiend in mavec.i
!
            CALL GETENVAR  ( 'COND_MAX', STR )
            IF ( ILEN(STR) == 0 ) THEN
                 COND_MAX = COND__MAX
               ELSE
                 READ ( UNIT=STR, FMT=*, IOSTAT=IER ) COND_MAX
                 IF ( IER .NE. 0 .OR. COND_MAX < 1.D-30 ) THEN
                      CALL ERR_LOG ( 1201, IUER, 'INVS', 'Wrong value '// &
     &                    'of the environment variable COND_MAX' )
                      RETURN 
                 END IF
            END IF
            INVS_COMMON_INIT = .TRUE.
      END IF
!
      IF ( N .LE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( N, STR )
           CALL ERR_LOG ( 1210, IUER, 'INVS', 'Wrong matrix dimension: '//STR )
           RETURN
         ELSE IF ( N .EQ. 1 ) THEN
!
! -------- Dimension 1
!
           IF ( DABS(MAT(1)) .LT. 1.D0/COND_MAX ) THEN
                CALL ERR_LOG ( 1211, IUER, 'INVS', 'Matrix is '// &
     &              '(almost) singular' )
                RETURN
              ELSE
                RC = 1.0
                MAT(1) = 1.D0/MAT(1)
                CALL ERR_LOG ( 0, IUER )
                RETURN
           END IF
         ELSE IF ( N .EQ. 2 ) THEN
!
! -------- Dimension 2
!
           DET = MAT(1)*MAT(3) - MAT(2)**2
           IF ( DET < MIN_VAL ) THEN
                RC = DET
                IF ( RC .LT. 1.0D0  .AND.  RC .NE. 0.0D0 ) RC = 1.0D0/DET
                WRITE ( UNIT=STR, FMT='(1PG12.4 )' ) DET
!
                WRITE ( UNIT=6, FMT=110 ) 1, 1, MAT(1)
                WRITE ( UNIT=6, FMT=110 ) 1, 2, MAT(2)
                WRITE ( UNIT=6, FMT=110 ) 2, 2, MAT(3)
 110            FORMAT ( 'MAT(', I1, ',', I1, ')=', 1PG22.15 )
                CALL ERR_LOG ( 1212, IUER, 'INVS', '2x2 matrix is '// &
     &                        '(almost) singular: determinant = '//STR )
                WRITE ( 6, * ) 'MAT = ', MAT(1:3)
                RETURN
           END IF
           MAT_ORIG(1:3) = MAT(1:3)
           MAT1_OLD = MAT(1)
           MAT(1) =  MAT(3)/DET
           MAT(2) = -MAT(2)/DET
           MAT(3) = MAT1_OLD/DET
!
           RC = -1.D30
           RC = MAX ( RC, DABS(MAT_ORIG(1)*MAT(1) + MAT_ORIG(2)*MAT(2) - 1.0D0) )
           RC = MAX ( RC, DABS(MAT_ORIG(2)*MAT(1) + MAT_ORIG(3)*MAT(2)) )
           RC = MAX ( RC, DABS(MAT_ORIG(2)*MAT(2) + MAT_ORIG(3)*MAT(3) - 1.0D0) )
!
           RC = RC_REF*RC
           IF ( RC > COND_MAX ) THEN
                CALL CLRCH ( STR )
                WRITE ( UNIT=STR(1:24), FMT='(1PD24.16)' ) RC/RC_REF
                CALL ERR_LOG ( 1213, IUER, 'INVS', '2x2 matrix is '// &
     &                        '(almost) singular. Maximum residual '// &
     &                        'is too high: '//STR )
                WRITE ( 6, * ) 'MAT = ', MAT(1:3)
                RETURN
           END IF
!
           IUER = 0
           RETURN
         ELSE IF ( N .EQ. 3 ) THEN
!
! -------- Direct computation of the determinant
!
           DET =   MAT(1)*( MAT(6)*MAT(3) - MAT(5)*MAT(5) ) &
     &           - MAT(2)*( MAT(6)*MAT(2) - MAT(5)*MAT(4) ) &
     &           + MAT(4)*( MAT(5)*MAT(2) - MAT(3)*MAT(4) ) 
           IF ( DET < MIN_VAL ) THEN
                CALL CLRCH ( STR )
                WRITE ( UNIT=STR(1:24), FMT='(1PD24.16)' ) DET
                CALL ERR_LOG ( 1214, IUER, 'INVS', '3x3 matrix is '// &
     &                        '(almost) singular: determinant = '//STR )
                WRITE ( 6, * ) 'MAT = ', MAT(1:6)
                RETURN
           END IF
!
! -------- Direct computation of the inverse matrix
!
           MAT_INV(1) =   MAT(6)*MAT(3) - MAT(5)*MAT(5)
           MAT_INV(2) = -(MAT(6)*MAT(2) - MAT(5)*MAT(4) )   
           MAT_INV(3) =   MAT(6)*MAT(1) - MAT(4)*MAT(4)
           MAT_INV(4) =   MAT(5)*MAT(2) - MAT(3)*MAT(4)
           MAT_INV(5) = -(MAT(5)*MAT(1) - MAT(2)*MAT(4) )
           MAT_INV(6) =   MAT(3)*MAT(1) - MAT(2)*MAT(2)
!
! -------- And then scale it by 1/DET
!
           MAT_INV(1:6) = MAT_INV(1:6)/DET
!
! -------- Now compute the product ot the inverse matix and the inital matrix
!
           MATO(1) = MAT(1)*MAT_INV(1) + MAT(2)*MAT_INV(2) + MAT(4)*MAT_INV(4) 
           MATO(2) = MAT(2)*MAT_INV(1) + MAT(3)*MAT_INV(2) + MAT(5)*MAT_INV(4) 
           MATO(4) = MAT(4)*MAT_INV(1) + MAT(5)*MAT_INV(2) + MAT(6)*MAT_INV(4) 
!
           MATO(3) = MAT(2)*MAT_INV(2) + MAT(3)*MAT_INV(3) + MAT(5)*MAT_INV(5) 
           MATO(5) = MAT(4)*MAT_INV(2) + MAT(5)*MAT_INV(3) + MAT(6)*MAT_INV(5) 
!
           MATO(6) = MAT(4)*MAT_INV(4) + MAT(5)*MAT_INV(5) + MAT(6)*MAT_INV(6) 
!
! -------- Now compute the maximum by modulo residual
!
           RC = -1.D30
           RC = MAX( DABS(MATO(1)-1.0D0), RC )
           RC = MAX( DABS(MATO(2)),       RC )
           RC = MAX( DABS(MATO(3)-1.0D0), RC )
           RC = MAX( DABS(MATO(4)),       RC )
           RC = MAX( DABS(MATO(5)),       RC )
           RC = MAX( DABS(MATO(6)-1.0D0), RC )
           RC = RC_REF*RC
           IF ( RC > COND_MAX ) THEN
                CALL CLRCH ( STR )
                WRITE ( UNIT=STR(1:24), FMT='(1PD24.16)' ) RC
                CALL ERR_LOG ( 1215, IUER, 'INVS', '3x3 matrix is '// &
     &                        '(almost) singular. Maximum residual '// &
     &                        'is too high: '//STR )
                WRITE ( 6, * ) 'MAT = ', MAT(1:10)
                RETURN
           END IF
!
           MAT(1:6) = MAT_INV(1:6)
           IUER = 0
           RETURN
         ELSE IF ( N .EQ. 4 ) THEN
!
! -------- Direct computation of the determinant
!
           DET = &
     &           MAT(7)*MAT(5)*MAT(5)*MAT(7)  - MAT(4)*MAT(8)*MAT(5)*MAT(7)  - MAT(7)*MAT(3)*MAT(6)*MAT(7)  + MAT(2)*MAT(8)*MAT(6)*MAT(7)  + &
     &           MAT(4)*MAT(3)*MAT(9)*MAT(7)  - MAT(2)*MAT(5)*MAT(9)*MAT(7)  - MAT(7)*MAT(5)*MAT(4)*MAT(8)  + MAT(4)*MAT(8)*MAT(4)*MAT(8)  + &
     &           MAT(7)*MAT(2)*MAT(6)*MAT(8)  - MAT(1)*MAT(8)*MAT(6)*MAT(8)  - MAT(4)*MAT(2)*MAT(9)*MAT(8)  + MAT(1)*MAT(5)*MAT(9)*MAT(8)  + &
     &           MAT(7)*MAT(3)*MAT(4)*MAT(9)  - MAT(2)*MAT(8)*MAT(4)*MAT(9)  - MAT(7)*MAT(2)*MAT(5)*MAT(9)  + MAT(1)*MAT(8)*MAT(5)*MAT(9)  + &
     &           MAT(2)*MAT(2)*MAT(9)*MAT(9)  - MAT(1)*MAT(3)*MAT(9)*MAT(9)  - MAT(4)*MAT(3)*MAT(4)*MAT(10) + MAT(2)*MAT(5)*MAT(4)*MAT(10) + &
     &           MAT(4)*MAT(2)*MAT(5)*MAT(10) - MAT(1)*MAT(5)*MAT(5)*MAT(10) - MAT(2)*MAT(2)*MAT(6)*MAT(10) + MAT(1)*MAT(3)*MAT(6)*MAT(10) 
           IF ( DET < MIN_VAL ) THEN
                CALL CLRCH ( STR )
                WRITE ( UNIT=STR(1:24), FMT='(1PD24.16)' ) DET
                CALL ERR_LOG ( 1216, IUER, 'INVS', '4x4 matrix is '// &
     &                        '(almost) singular: determinant = '//STR )
                WRITE ( 6, * ) 'MAT = ', MAT(1:10)
                RETURN
           END IF
!
! -------- Direct computation of the inverse matrix
!
           MAT_INV(1)  = MAT(5)*MAT(9)*MAT(8) - MAT(8)*MAT(6)*MAT(8) + MAT(8)*MAT(5)*MAT(9) - MAT(3)*MAT(9)*MAT(9) - MAT(5)*MAT(5)*MAT(10) + MAT(3)*MAT(6)*MAT(10)
           MAT_INV(2)  = MAT(7)*MAT(6)*MAT(8) - MAT(4)*MAT(9)*MAT(8) - MAT(7)*MAT(5)*MAT(9) + MAT(2)*MAT(9)*MAT(9) + MAT(4)*MAT(5)*MAT(10) - MAT(2)*MAT(6)*MAT(10)
           MAT_INV(3)  = MAT(4)*MAT(9)*MAT(7) - MAT(7)*MAT(6)*MAT(7) + MAT(7)*MAT(4)*MAT(9) - MAT(1)*MAT(9)*MAT(9) - MAT(4)*MAT(4)*MAT(10) + MAT(1)*MAT(6)*MAT(10)
           MAT_INV(4)  = MAT(4)*MAT(8)*MAT(8) - MAT(7)*MAT(5)*MAT(8) + MAT(7)*MAT(3)*MAT(9) - MAT(2)*MAT(8)*MAT(9) - MAT(4)*MAT(3)*MAT(10) + MAT(2)*MAT(5)*MAT(10)
           MAT_INV(5)  = MAT(7)*MAT(5)*MAT(7) - MAT(4)*MAT(8)*MAT(7) - MAT(7)*MAT(2)*MAT(9) + MAT(1)*MAT(8)*MAT(9) + MAT(4)*MAT(2)*MAT(10) - MAT(1)*MAT(5)*MAT(10)
           MAT_INV(6)  = MAT(2)*MAT(8)*MAT(7) - MAT(7)*MAT(3)*MAT(7) + MAT(7)*MAT(2)*MAT(8) - MAT(1)*MAT(8)*MAT(8) - MAT(2)*MAT(2)*MAT(10) + MAT(1)*MAT(3)*MAT(10)
           MAT_INV(7)  = MAT(7)*MAT(5)*MAT(5) - MAT(4)*MAT(8)*MAT(5) - MAT(7)*MAT(3)*MAT(6) + MAT(2)*MAT(8)*MAT(6) + MAT(4)*MAT(3)*MAT(9)  - MAT(2)*MAT(5)*MAT(9)
           MAT_INV(8)  = MAT(4)*MAT(8)*MAT(4) - MAT(7)*MAT(5)*MAT(4) + MAT(7)*MAT(2)*MAT(6) - MAT(1)*MAT(8)*MAT(6) - MAT(4)*MAT(2)*MAT(9)  + MAT(1)*MAT(5)*MAT(9)
           MAT_INV(9)  = MAT(7)*MAT(3)*MAT(4) - MAT(2)*MAT(8)*MAT(4) - MAT(7)*MAT(2)*MAT(5) + MAT(1)*MAT(8)*MAT(5) + MAT(2)*MAT(2)*MAT(9)  - MAT(1)*MAT(3)*MAT(9)
           MAT_INV(10) = MAT(2)*MAT(5)*MAT(4) - MAT(4)*MAT(3)*MAT(4) + MAT(4)*MAT(2)*MAT(5) - MAT(1)*MAT(5)*MAT(5) - MAT(2)*MAT(2)*MAT(6)  + MAT(1)*MAT(3)*MAT(6)
!
! -------- And then scale it by 1/DET
!
           MAT_INV(1:10) = MAT_INV(1:10)/DET
!
           MATO(1)  = MAT(1)*MAT_INV(1) + MAT(2)*MAT_INV(2) + &
     &                MAT(4)*MAT_INV(4) + MAT(7)*MAT_INV(7)
           MATO(2)  = MAT(2)*MAT_INV(1) + MAT(3)*MAT_INV(2) + &
     &                MAT(5)*MAT_INV(4) + MAT(8)*MAT_INV(7)
           MATO(4)  = MAT(4)*MAT_INV(1) + MAT(5)*MAT_INV(2) + &
     &                MAT(6)*MAT_INV(4) + MAT(9)*MAT_INV(7) 
           MATO(7)  = MAT(7)*MAT_INV(1) + MAT(8)*MAT_INV(2) + &
     &                MAT(9)*MAT_INV(4) + MAT(10)*MAT_INV(7) 
!
           MATO(3)  = MAT(2)*MAT_INV(2) + MAT(3)*MAT_INV(3) + &
     &                MAT(5)*MAT_INV(5) + MAT(8)*MAT_INV(8)
           MATO(5)  = MAT(4)*MAT_INV(2) + MAT(5)*MAT_INV(3) + &
     &                MAT(6)*MAT_INV(5) + MAT(9)*MAT_INV(8) 
           MATO(8)  = MAT(7)*MAT_INV(2) + MAT(8)*MAT_INV(3) + &
     &                MAT(9)*MAT_INV(5) + MAT(10)*MAT_INV(8) 
!
           MATO(6)  = MAT(4)*MAT_INV(4) + MAT(5)*MAT_INV(5) + &
     &                MAT(6)*MAT_INV(6) + MAT(9)*MAT_INV(9) 
           MATO(9)  = MAT(7)*MAT_INV(4) + MAT(8)*MAT_INV(5) + &
     &                MAT(9)*MAT_INV(6) + MAT(10)*MAT_INV(9) 
!
           MATO(10) = MAT(7)*MAT_INV(7) + MAT(8)*MAT_INV(8) + &
     &                MAT(9)*MAT_INV(9) + MAT(10)*MAT_INV(10) 
           RC = -1.D30
           RC = MAX( DABS(MATO(1)-1.0D0), RC )
           RC = MAX( DABS(MATO(2)),       RC )
           RC = MAX( DABS(MATO(3)-1.0D0), RC )
           RC = MAX( DABS(MATO(4)),       RC )
           RC = MAX( DABS(MATO(5)),       RC )
           RC = MAX( DABS(MATO(6)-1.0D0), RC )
           RC = MAX( DABS(MATO(7)),       RC )
           RC = MAX( DABS(MATO(8)),       RC )
           RC = MAX( DABS(MATO(9)),       RC )
           RC = MAX( DABS(MATO(10)-1.0D0),       RC )
           RC = RC_REF*RC
           IF ( RC > COND_MAX ) THEN
                CALL CLRCH ( STR )
                WRITE ( UNIT=STR(1:24), FMT='(1PD24.16)' ) RC
                CALL ERR_LOG ( 1217, IUER, 'INVS', '4x4 matrix is '// &
     &                        '(almost) singular. Maximum residual '// &
     &                        'is too high: '//STR )
                WRITE ( 6, * ) 'MAT = ', MAT(1:10)
                RETURN
           END IF
!
           MAT(1:10) = MAT_INV(1:10)
           IUER = 0
           RETURN
         ELSE IF ( N .LE. 16 ) THEN
           IF ( N .EQ. 5 ) THEN
                INVS_FUNC(5) = FUNC_ADDRESS ( INVS_5 )
                SS_FUNC(5)   = FUNC_ADDRESS ( MUL_MM_SS_S_5 ) 
              ELSE IF ( N .EQ. 6 ) THEN
                INVS_FUNC(6) = FUNC_ADDRESS ( INVS_6 )
                SS_FUNC(6)   = FUNC_ADDRESS ( MUL_MM_SS_S_6 ) 
              ELSE IF ( N .EQ. 7 ) THEN
                INVS_FUNC(7) = FUNC_ADDRESS ( INVS_7 )
                SS_FUNC(7)   = FUNC_ADDRESS ( MUL_MM_SS_S_7 ) 
              ELSE IF ( N .EQ. 8 ) THEN
                INVS_FUNC(8) = FUNC_ADDRESS ( INVS_8 )
                SS_FUNC(8)   = FUNC_ADDRESS ( MUL_MM_SS_S_8 ) 
              ELSE IF ( N .EQ. 9 ) THEN
                INVS_FUNC(9) = FUNC_ADDRESS ( INVS_9 )
                SS_FUNC(9)   = FUNC_ADDRESS ( MUL_MM_SS_S_9 ) 
              ELSE IF ( N .EQ. 10 ) THEN
                INVS_FUNC(10) = FUNC_ADDRESS ( INVS_10 )
                SS_FUNC(10)   = FUNC_ADDRESS ( MUL_MM_SS_S_10 ) 
              ELSE IF ( N .EQ. 11 ) THEN
                INVS_FUNC(11) = FUNC_ADDRESS ( INVS_11 )
                SS_FUNC(11)   = FUNC_ADDRESS ( MUL_MM_SS_S_11 ) 
              ELSE IF ( N .EQ. 12 ) THEN
                INVS_FUNC(12) = FUNC_ADDRESS ( INVS_12 )
                SS_FUNC(12)   = FUNC_ADDRESS ( MUL_MM_SS_S_12 ) 
              ELSE IF ( N .EQ. 13 ) THEN
                INVS_FUNC(13) = FUNC_ADDRESS ( INVS_13 )
                SS_FUNC(13)   = FUNC_ADDRESS ( MUL_MM_SS_S_13 ) 
              ELSE IF ( N .EQ. 14 ) THEN
                INVS_FUNC(14) = FUNC_ADDRESS ( INVS_14 )
                SS_FUNC(14)   = FUNC_ADDRESS ( MUL_MM_SS_S_14 ) 
              ELSE IF ( N .EQ. 15 ) THEN
                INVS_FUNC(15) = FUNC_ADDRESS ( INVS_15 )
                SS_FUNC(15)   = FUNC_ADDRESS ( MUL_MM_SS_S_15 ) 
              ELSE IF ( N .EQ. 16 ) THEN
                INVS_FUNC(16) = FUNC_ADDRESS ( INVS_16 )
                SS_FUNC(16)   = FUNC_ADDRESS ( MUL_MM_SS_S_16 ) 
           END IF
!
! -------- Build the argument lists
!
           KP = (N*(N+1))/2
           MAT_ORIG(1:KP) = MAT(1:KP)
           EPS = DB__INVMAT_EPS
           ARG_INVS(1) = 3
           ARG_INVS(2) = LOC(MAT)
           ARG_INVS(3) = LOC(EPS)
           ARG_INVS(4) = LOC(IER)
!
! -------- Invert the matrix
!
           CALL ERR_PASS  ( IUER, IER )
           CALL LIB$CALLG ( ARG_INVS, %VAL(INVS_FUNC(N)) )
           IF ( IER > 0 ) THEN
                CALL CLRCH ( STR )
                CALL INCH ( IER, STR )
                RC = -1.D0
                CALL ERR_LOG ( IER, IUER, 'INVS', 'Error in matrix '// &
     &              'inversion at the '//STR(1:I_LEN(STR))//'-th step' )
                RETURN
           END IF
!
! -------- Build the argiument list for comutation of the matrix product
!
           ARG_SS(1) = 3
           ARG_SS(2) = LOC(MAT)
           ARG_SS(3) = LOC(MAT_ORIG)
           ARG_SS(4) = LOC(MATO)
!
! -------- Compute the product of the original matrix and matrix invert
!
           CALL LIB$CALLG ( ARG_SS, %VAL(SS_FUNC(N)) )
           KP = 0
           RC = 1.D-30
           DO 410 J1=1,N
              DO 420 J2=1,J1
                 KP = KP + 1
                 IF ( J2 == J1 ) THEN
                      RC = MAX ( RC, DABS(MATO(KP)-1.0D0) )
                    ELSE
                      RC = MAX ( RC, DABS(MATO(KP)) )
                 END IF
 420          CONTINUE 
 410       CONTINUE 
           RC = RC_REF*RC
           IF ( RC > COND_MAX ) THEN
                CALL CLRCH ( STR )
                WRITE ( UNIT=STR(1:24), FMT='(1PD24.16)' ) RC
                CALL ERR_LOG ( 1218, IUER, 'INVS', 'Matrix is '// &
     &                        '(almost) singular. Maxumum residual '// &
     &                        'is too high: '//STR )
                WRITE ( 6, * ) 'MAT = ', MAT(1:KP)
                RETURN
           END IF
           IUER = 0
           RETURN
         ELSE IF ( N .LE. DB__INVMAT_DIR ) THEN
!
! ======== Small dimensions. Schedule highly optimized code for the dimension N
!
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
!
           EMVS_FUNC(9)  = FUNC_ADDRESS ( EMVS_9  )
           EMVS_FUNC(10) = FUNC_ADDRESS ( EMVS_10 )
           EMVS_FUNC(11) = FUNC_ADDRESS ( EMVS_11 )
           EMVS_FUNC(12) = FUNC_ADDRESS ( EMVS_12 )
           EMVS_FUNC(13) = FUNC_ADDRESS ( EMVS_13 )
           EMVS_FUNC(14) = FUNC_ADDRESS ( EMVS_14 )
           EMVS_FUNC(15) = FUNC_ADDRESS ( EMVS_15 )
           EMVS_FUNC(16) = FUNC_ADDRESS ( EMVS_16 )
           EMVS_FUNC(17) = FUNC_ADDRESS ( EMVS_17 )
           EMVS_FUNC(18) = FUNC_ADDRESS ( EMVS_18 )
           EMVS_FUNC(19) = FUNC_ADDRESS ( EMVS_19 )
           EMVS_FUNC(20) = FUNC_ADDRESS ( EMVS_20 )
           EMVS_FUNC(21) = FUNC_ADDRESS ( EMVS_21 )
           EMVS_FUNC(22) = FUNC_ADDRESS ( EMVS_22 )
           EMVS_FUNC(23) = FUNC_ADDRESS ( EMVS_23 )
           EMVS_FUNC(24) = FUNC_ADDRESS ( EMVS_24 )
           EMVS_FUNC(25) = FUNC_ADDRESS ( EMVS_25 )
           EMVS_FUNC(26) = FUNC_ADDRESS ( EMVS_26 )
           EMVS_FUNC(27) = FUNC_ADDRESS ( EMVS_27 )
           EMVS_FUNC(28) = FUNC_ADDRESS ( EMVS_28 )
           EMVS_FUNC(29) = FUNC_ADDRESS ( EMVS_29 )
           EMVS_FUNC(30) = FUNC_ADDRESS ( EMVS_30 )
           EMVS_FUNC(31) = FUNC_ADDRESS ( EMVS_31 )
           EMVS_FUNC(32) = FUNC_ADDRESS ( EMVS_32 )
!
! -------- Build the argument lists
!
           EPS = DB__INVMAT_EPS
           ARG_INVS(1) = 3
           ARG_INVS(2) = LOC(MAT)
           ARG_INVS(3) = LOC(EPS)
           ARG_INVS(4) = LOC(IER)
!
           ARG_EMVS(1) = 3
           ARG_EMVS(2) = LOC(MAT)
           ARG_EMVS(3) = LOC(EM_MAX)
           ARG_EMVS(4) = LOC(IER)
!
! -------- Compute the maximal eigen value
!
           CALL ERR_PASS  ( IUER, IER )
           CALL LIB$CALLG ( ARG_EMVS, %VAL(EMVS_FUNC(N)) )
           IF ( IER > 0 ) THEN
                RC = -1.0D0
                CALL ERR_LOG ( 1219, IUER, 'INVS', 'Matrix is not '// &
     &              'a positively determined. ' )
                RETURN
           END IF
!
! -------- Invert the matrix
!
           CALL ERR_PASS  ( IUER, IER )
           CALL LIB$CALLG ( ARG_INVS, %VAL(INVS_FUNC(N)) )
           IF ( IER > 0 ) THEN
                CALL CLRCH ( STR )
                CALL INCH ( IER, STR )
                RC = -1.D0
                CALL ERR_LOG ( IER, IUER, 'INVS', 'Error in matrix '// &
     &              'inversion at the '//STR(1:I_LEN(STR))//'-th step' )
                RETURN
           END IF
!
! -------- Compute the maximal eigen value of inverted matrix
!
           ARG_EMVS(3) = LOC(EM_MIN)
           CALL ERR_PASS  ( IUER, IER )
           CALL LIB$CALLG ( ARG_EMVS, %VAL(EMVS_FUNC(N)) )
           IF ( IER > 0 ) THEN
                CALL ERR_LOG ( 1220, IUER, 'INVS', 'Matrix is not '// &
     &              'a positively determined' )
                RETURN
           END IF
!
! -------- Compute condition number
!
           RC = EM_MAX*EM_MIN
!
           IF ( RC .LT. 1.D0/COND_MAX  .OR.  RC .GT. COND_MAX ) THEN
                CALL CLRCH   ( STR )
                WRITE ( UNIT=STR, FMT='(1PD12.4)' ) RC
                CALL CHASHL  ( STR )
!
                CALL CLRCH   ( STR1 )
                WRITE ( UNIT=STR1, FMT='(1PD12.4)' ) COND_MAX
                CALL CHASHL  ( STR1 )
!
                CALL ERR_LOG ( 1221, IUER, 'INVS', 'Matrix is almost '// &
     &              'singular: Condition number = '//STR(1:I_LEN(STR))// &
     &              ' what exceeds the specified limit: '//STR1 )
                RETURN
           END IF
           IUER = 0
           RETURN
         ELSE
!
! ======== Large dimensions
!
! -------- Compute the maximal eigen value of initial matrix
!
           CALL ERR_PASS ( IUER, IER )
           EM_MAX = EMVS ( N, MAT, IT, ER, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 1222, IUER, 'INVS', 'Matrix is not '// &
     &              'a positively determined' )
                RETURN
           END IF
!
! -------- Allocate additional memory for reordering
!
           NB = (N-N/2)
           NBB = (INT8(NB)*INT8(NB+1))/2
           ALLOCATE ( MAT_TEMP(NBB), STAT=IER )
           IF ( IER .NE. 0 ) THEN
                CALL CLRCH  ( STR )
                CALL IINCH8 ( INT8(8)*NBB, STR )
                CALL ERR_LOG ( 1223, IUER, 'INVS', 'Failure to allocate '// &
     &               STR(1:I_LEN(STR))//' bytes of dynamic memory for '// &
     &              'a temporary array needed for the recursive algorithm '// &
     &              'for matrix inversion' )
                RETURN
           END IF
#ifdef BLAS_NOT_A_NUMBER
           CALL MEMSET ( MAT_TEMP, 0, %VAL(INT8(8)*NBB) )
#endif
!
! -------- Matrix reordering to recursive packed upper triangular format
!
           CALL DREORDER5 ( N, MAT, MAT_TEMP )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 1224, IUER, 'INVS', 'Error in DREORDER '// &
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
                RC = -1.0D0
                CALL ERR_LOG ( IER, IUER, 'INVS', 'Error in matrix '// &
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
                CALL ERR_LOG ( 1225, IUER, 'INVS', 'Error in DREORDER '// &
     &              'IER = '//STR )
                DEALLOCATE ( MAT_TEMP, STAT=IER )
                RETURN
           END IF
!
! -------- Return dynamic memory
!
           DEALLOCATE ( MAT_TEMP, STAT=IER )
!
! -------- Compute the maximal eigen value of inverted matrix
!
           CALL ERR_PASS ( IUER, IER )
           EM_MIN = EMVS ( N, MAT, IT, ER, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 1226, IUER, 'INVS', 'Matrix is not '// &
     &              'a positively determined' )
                RETURN
           END IF
!
! -------- Compute condition number
!
           RC = EM_MAX*EM_MIN
!
           IF ( RC .LT. 1.D0/COND_MAX  .OR.  RC .GT. COND_MAX ) THEN
                CALL CLRCH   ( STR )
                WRITE ( UNIT=STR, FMT='(1PD12.4)' ) RC
                CALL CHASHL  ( STR )
!
                CALL CLRCH   ( STR1 )
                WRITE ( UNIT=STR1, FMT='(1PD12.4)' ) COND_MAX
                CALL CHASHL  ( STR1 )
!
                CALL ERR_LOG ( 1227, IUER, 'INVS', 'Matrix is almost '// &
     &              'singular: Condition number = '//STR(1:I_LEN(STR))// &
     &              ' what exceeds the specified limit: '//STR1 )
                RETURN
           END IF
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  OPT_INVS  !#!#
