      SUBROUTINE POLY_SOLVE ( NP, TIM, T0, VAL, DEG, POLY_COEF, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine POLY_SOLVE computes the coeffients of the interpolating    *
! *   polynomial. If DEG = NP-1, then the exansion is unique and is      *
! *   computed exactly. If DEG < NP-1, the expansion is sought with      *
! *   least squares assuming each point has equal weight.                *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! * NP        ( INTEGER*4  ) -- The number of points. Dimension: NP.     *
! * TIM       ( REAL*8     ) -- The argument of the function.            *
! * T0        ( REAL*8     ) -- Reference argument. The polynomial       *
! *                             is a funtion of TIM(i) - T0.             *
! * VAL       ( REAL*8     ) -- Array of the vallyes. Dimension: NP.     *
! * DEG       ( INTEGER*4  ) -- Degree of the polynomial in a range      *
! *                             of [0, 63]. Should be less than NP.      *
! *                                                                      *
! * _________________________ Output parameters: _______________________ *
! *                                                                      *
! * POLY_COEF ( REAL*8    ) -- Arracy of the polynomial expansion.       *
! *                            Dimension: (0:DEG).                       *
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
! *  ### 27-APR-2021               v1.0 (c)  L. Petrov  27-APR-2021 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INTEGER*4  NP, DEG, IUER
      REAL*8     TIM(NP), T0, VAL(NP), POLY_COEF(0:DEG)
      CHARACTER  STR*32
      INTEGER*4  MP
      PARAMETER  ( MP = 64 )
      REAL*8     NOR_MAT((MP*(MP+1))/2), NOR_VEC(MP), EQU_OBS(MP), RC
      INTEGER*4  J1, J2, IER
!
      IF ( DEG <  0 ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( DEG, STR )
           CALL ERR_LOG ( 5921, IUER, 'POLY_SOLVE', 'Wrong degree of '// &
     &         'the polyonal. A non-negative integer was expected, '// &
     &         'but got '//STR )
           RETURN 
      END IF
      IF ( DEG >  MP-1 ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( DEG, STR )
           CALL ERR_LOG ( 5922, IUER, 'POLY_SOLVE', 'Wrong degree of '// &
     &         'the polyonal. A non-negative integer < 64 was expected, '// &
     &         'but got '//STR )
           RETURN 
      END IF
      IF ( DEG .GE. NP ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( DEG, STR )
           CALL ERR_LOG ( 5923, IUER, 'POLY_SOLVE', 'Wrong degree of '// &
     &         'the polyonal. It should be less than the number of point' )
           RETURN 
      END IF
!
      NOR_MAT = 0.0D0
      NOR_VEC = 0.0D0
      DO 410 J1=1,NP
         EQU_OBS(1) = 1.0D0
         IF ( DEG > 0) THEN
              DO 420 J2=2,DEG+1
                 EQU_OBS(J2) = EQU_OBS(J2-1)*(TIM(J1) - T0)
 420          CONTINUE 
         END IF
         CALL DIAD_CVT_S ( 1.0D0, DEG+1, EQU_OBS, EQU_OBS, NOR_MAT )
         CALL NORVEC_UPD ( DEG+1, 1.0D0, VAL(J1), EQU_OBS, NOR_VEC )
 410  CONTINUE 
!
      CALL ERR_PASS ( IUER, IER )
      CALL INVS ( DEG+1, NOR_MAT, RC, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 5924, IUER, 'POLY_SOLVE', 'Error in an attempt '// &
     &         'to invert normal matrix' )
           RETURN 
      END IF
!
      CALL MUL_MV_SV_V ( DEG+1, NOR_MAT, DEG+1, NOR_VEC, DEG+1, POLY_COEF, IER )
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE POLY_SOLVE   !#!  
!
! ------------------------------------------------------------------------
!
      FUNCTION   POLY_VAL ( TIM, T0, DEG, POLY_COEF )
! ************************************************************************
! *                                                                      *
! *   Routine  POLY_VAL computes the polynomial of the degeee DEG with   *
! *   coeffieicnts POLY_COEF (starting with index 0) at the argument     *
! *   TIM. The polynomial is a funtion of (t - T0). Array POLY_COEF      *
! *   has dimension (0:DEG).                                             *
! *                                                                      *
! *  ### 27-APR-2021    POLY_VAL   v1.0 (c)  L. Petrov  27-APR-2021 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      REAL*8     POLY_VAL
      INTEGER*4  DEG
      REAL*8     TIM, T0, POLY_COEF(0:DEG)
      INTEGER*4  J1
      REAL*8     DT
!
      DT = TIM - T0
      POLY_VAL = POLY_COEF(DEG)
      DO 410 J1=DEG-1,0,-1
         POLY_VAL = POLY_VAL*DT + POLY_COEF(J1)
 410  CONTINUE 
      RETURN
      END  FUNCTION  POLY_VAL  !#!  
