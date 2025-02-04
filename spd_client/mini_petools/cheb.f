      FUNCTION CHEB ( N, ARG_MIN, ARG_MAX, ARG )
! ************************************************************************
! *                                                                      *
! *   Function CHEB computes the coefficient of Chebyshev polynomial     *
! *   defined at the rangfe [ARG_MIN, ARG_MAX] for the arguyment ARG.    *
! *   NB: no check of whether the argument is in the range is made.      *
! *                                                                      *
! *  ### 14-SEP-2008      CHEB     v1.0 (c)  L. Petrov  14-SEP-2008 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INTEGER*4  N 
      REAL*8     ARG_MIN, ARG_MAX, ARG
      REAL*8     CHEB
      REAL*8     W, CHEB_LAST, CHEB_LAST_LAST
      INTEGER*4  J1
!
      CHEB = 1.0D0
      IF ( N == 0 ) RETURN 
!
! --- Compute W -- normalized argument reduced to the range [-1,1]
!
      W = -1.D0 + 2.D0*(ARG - ARG_MIN)/(ARG_MAX - ARG_MIN)
      CHEB_LAST = CHEB
      CHEB = W
      IF ( N == 1 ) RETURN 
!
      DO 410 J1=2,N
         CHEB_LAST_LAST = CHEB_LAST
         CHEB_LAST      = CHEB
         CHEB = 2.D0*CHEB_LAST*W - CHEB_LAST_LAST
 410  CONTINUE 
      RETURN
      END  FUNCTION CHEB  !#!#
!
! ------------------------------------------------------------------------
!
      FUNCTION CHEB_VAL ( ND, ARG_MIN, ARG_MAX, ARG, CHE_COEF )
! ************************************************************************
! *                                                                      *
! *   Function  CHEB_VAL  computes the values of the function that is    *
! *   represeneted as an expansion over the Chebyshev polynomials basis  *
! *   at the point ARG that is at the range [ARG_min, ARG_max].          *
! *                                                                      *
! *  ### 16-SEP-2008    CHEB_VAL   v1.0 (c)  L. Petrov  16-SEP-2008 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INTEGER*4  ND 
      REAL*8     ARG_MIN, ARG_MAX, ARG, CHE_COEF(0:ND)
      REAL*8     W, CHEB_POL, CHEB_LAST, CHEB_LAST_LAST
      REAL*8     CHEB_VAL
      INTEGER*4  J1
!
      CHEB_POL = 1.0D0
      CHEB_VAL = CHE_COEF(0)
      IF ( ND == 0 ) RETURN 
!
      W = -1.D0 + 2.D0*(ARG - ARG_MIN)/(ARG_MAX - ARG_MIN)
      CHEB_LAST = CHEB_POL
      CHEB_POL = W
      CHEB_VAL =  CHEB_VAL + CHEB_POL*CHE_COEF(1)
      IF ( ND == 1 ) RETURN 
      DO 410 J1=2,ND
         CHEB_LAST_LAST = CHEB_LAST
         CHEB_LAST      = CHEB_POL
         CHEB_POL  = 2.D0*CHEB_LAST*W - CHEB_LAST_LAST
         CHEB_VAL =  CHEB_VAL + CHEB_POL*CHE_COEF(J1)
 410  CONTINUE 
      RETURN
      END  FUNCTION CHEB_VAL  !#!#
!
! ------------------------------------------------------------------------
!
      FUNCTION CHEB_DER ( ND, ARG_MIN, ARG_MAX, ARG, CHE_COEF )
! ************************************************************************
! *                                                                      *
! *   Function  CHEB_DER  computes the first derivative of the function  *
! *   that is represeneted as an expansion over the Chebyshev            *
! *   polynomials basis at the point ARG that is at the range            *
! *   [ARG_min, ARG_max].                                                *
! *                                                                      *
! *  ### 15-OCT-2008    CHEB_DER   v1.0 (c)  L. Petrov  15-OCT-2008 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INTEGER*4  ND 
      REAL*8     ARG_MIN, ARG_MAX, ARG, CHE_COEF(0:ND)
      REAL*8     W, CHEB_POL_DER, V, V_LAST, V_LAST_LAST
      REAL*8     CHEB_DER
      INTEGER*4  J1
!
      IF ( ND == 0 ) RETURN 
      CHEB_DER = CHE_COEF(1)
      IF ( ND == 1 ) RETURN 
!
      W = -1.D0 + 2.D0*(ARG - ARG_MIN)/(ARG_MAX - ARG_MIN)
      V_LAST_LAST = 1.0D0
      V_LAST = 2.D0*W
      DO 410 J1=2,ND
         V = 2.0D0*W*V_LAST - V_LAST_LAST
         CHEB_POL_DER = J1*V_LAST
         CHEB_DER =  CHEB_DER + CHEB_POL_DER*CHE_COEF(J1)
         V_LAST_LAST = V_LAST
         V_LAST = V
 410  CONTINUE 
      CHEB_DER = CHEB_DER*2.0D0/(ARG_MAX - ARG_MIN)
      RETURN
      END  FUNCTION  CHEB_DER  !#!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE CHE_EVAL_LSQ ( N, ARG_BEG, ARG_END, ARG, VAL, NC, CH, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  CHE_COMP_EVAL_LSQ  evaluates coefficients of the          *
! *   expansion of the function VAL(ARG) in the range [ARG_BEG, ARG_END] *
! *   over Chebyshev polynomials of order NC.                            *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *       N ( INTEGER*4 ) -- The number of point of function VAL(ARG).   *
! *      NC ( INTEGER*4 ) -- The maximal degree of the Chebyshev         *
! *                          polynomial.                                 *
! * ARG_BEG ( REAL*8    ) -- The low (left) boundary of the expansion    *
! *                          validity.                                   *
! * ARG_END ( REAL*8    ) -- The high (right) boundary of the expansion  *
! *                          validity.                                   *
! *     ARG ( REAL*8    ) -- Array of arguments of the function under    *
! *                          considration.                               *
! *     VAL ( REAL*8    ) -- Array of values of the function under       *
! *                          consideration.                              *
! *                                                                      *
! * _________________________ Output parameters: _______________________ *
! *                                                                      *
! *      CH ( REAL*8    ) -- Array of Chebyshev polynomail coefficients. *
! *                          Dimension: [0, NC]. NB: array CH runs from  *
! *                          index 0.                                    *
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
! *  ### 17-JUN-2009  CHE_EVAL_LSQ  1.0 (c)  L. Petrov  17-JUN-2009 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INTEGER*4  N, NC, IUER
      REAL*8     ARG_BEG, ARG_END, ARG(N), VAL(N), CH(0:NC)
      REAL*8,    ALLOCATABLE :: NOR_VEC(:), NOR_MAT(:), OBS_EQU(:)
      REAL*8     RC, ARG_NRM
      INTEGER*4  J1, J2, J3, J4, LD, IER
!
      LD = NC + 1
      ALLOCATE ( OBS_EQU(0:NC) )
      ALLOCATE ( NOR_VEC(0:NC) )
      ALLOCATE ( NOR_MAT((LD*(LD+1))/2) )
!
      CALL NOUT_R8 ( LD, NOR_VEC ) 
      CALL NOUT_R8 ( (LD*(LD+1))/2, NOR_MAT ) 
!
! --- Create the noraml system of equations
!
      DO 410 J1=1,N
         ARG_NRM = -1.D0 + 2.D0*(ARG(J1) - ARG_BEG )/(ARG_END - ARG_BEG)
         DO 420 J2=0,NC
            IF ( J2 == 0 ) THEN
                 OBS_EQU(J2) = 1.0D0
               ELSE IF ( J2 == 1 ) THEN
                 OBS_EQU(J2) = ARG_NRM
               ELSE 
!
! -------------- We use recurrent expression for evaluating the value of 
! -------------- Chebyshev polynomial
!
                 OBS_EQU(J2) = 2.D0*ARG_NRM*OBS_EQU(J2-1) - OBS_EQU(J2-2) 
            END IF
 420     CONTINUE 
         CALL DIAD_CVT_S ( 1.0D0, LD, OBS_EQU, OBS_EQU, NOR_MAT )
         DO 430 J3=0,NC
            NOR_VEC(J3) = NOR_VEC(J3) + OBS_EQU(J3)*VAL(J1)
 430     CONTINUE 
 410  CONTINUE 
!
! --- Invert the matrix
!
      CALL ERR_PASS ( IUER, IER )
      CALL INVS ( LD, NOR_MAT, RC, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 1701, IUER, 'CHE_EVAL_LSQ', 'Failure in matrix '// &
     &         'inversion' )
           RETURN 
      END IF
!
! --- Find solution
!
      IER=-1
      CALL MUL_MV_SV_V ( LD, NOR_MAT, LD, NOR_VEC, LD, CH, IER )
!
      DEALLOCATE ( NOR_MAT )
      DEALLOCATE ( NOR_VEC )
      DEALLOCATE ( OBS_EQU )
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  CHE_EVAL_LSQ  !#!  
