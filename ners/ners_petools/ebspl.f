      FUNCTION EBSPL_VAL_R8 ( MN, DEG, ARG, ARG_VEC, BCOE_VEC )
! ************************************************************************
! *                                                                      *
! *   Routine  EBSPL_VAL_R8  computes value of function at point with    *
! *   argument ARG using coefficients of its expansion over the B-spline *
! *   basis.                                                             *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *       MN ( INTEGER*4 ) -- The number of knots of the B-spline.       *
! *      DEG ( INTEGER*4 ) -- Degree of B-spline.                        *
! *      ARG ( REAL*8    ) -- Argument for which the function is         *
! *                           computed.
! *  ARG_VEC ( REAL*8    ) -- Array of arguments for B-spline at knots.  *
! *                           Dimension: [1:MN].                         *
! * BCOE_VEC ( REAL*8    ) -- Array of B-spline coefficients.            *
! *                           Dimension: [1-DEG:MN-1].                   *
! *                                                                      *
! * _________________________ Output parameters: _______________________ *
! *                                                                      *
! * <EBSPL_VAL_R8> ( REAL*8    ) -- value of the function computed using *
! *                                 its expansion over B-spline basis.   *
! *                                                                      *
! *  ### 25-MAR-2010  EBSPL_VAL_R8  v1.0 (c)  L. Petrov 25-MAR-2010 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      REAL*8     EBSPL_VAL_R8 
      INTEGER*4  MN, DEG
      REAL*8     ARG, ARG_VEC(MN), BCOE_VEC(1-DEG:MN-1)
      INTEGER*4  J1, IP
      REAL*8,    EXTERNAL :: BSPL_VAL 
      INTEGER*4, EXTERNAL :: IXMN8 
!
      EBSPL_VAL_R8 = 0.0D0
      IF ( ARG < ARG_VEC(1)  .OR.  ARG > ARG_VEC(MN) ) THEN
!
! -------- If the argument is away from the the interpolation range,
! -------- nothing to do: its value is zero.
!
           RETURN 
      END IF
      IP = IXMN8 ( MN, ARG_VEC, ARG )
      IF ( IP < 1    ) IP = 1 
      IF ( IP > MN-1 ) IP = MN-1
      DO 410 J1=0,DEG
         EBSPL_VAL_R8 = EBSPL_VAL_R8 + BCOE_VEC(IP-J1)* &
     &                                 BSPL_VAL ( MN, ARG_VEC, DEG, IP-J1, ARG )
 410  CONTINUE 
      RETURN
      END  FUNCTION EBSPL_VAL_R8  !#!#
!
! ------------------------------------------------------------------------
!
      FUNCTION EBSPL_DER_R8 ( MN, DEG, ARG, ARG_VEC, BCOE_VEC )
! ************************************************************************
! *                                                                      *
! *   Routine  EBSPL_DER_R8  computes the first derivative of a function *
! *   at the point with argument ARG using coefficients of its expansion *
! *   over the B-spline basis.                                           *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *       MN ( INTEGER*4 ) -- The number of knots of the B-spline.       *
! *      DEG ( INTEGER*4 ) -- Degree of B-spline.                        *
! *      ARG ( REAL*8    ) -- Argument for which the first derivative    *
! *                           of the function is computed.               *
! *  ARG_VEC ( REAL*8    ) -- Array of arguments for B-spline at knots.  *
! *                           Dimension: [1:MN].                         *
! * BCOE_VEC ( REAL*8    ) -- Array of B-spline coefficients.            *
! *                           Dimension: [1-DEG:MN-1].                   *
! *                                                                      *
! * _________________________ Output parameters: _______________________ *
! *                                                                      *
! * <EBSPL_DER_R8> ( REAL*8    ) -- Value of the first derivative of the *
! *                                 function computed using its          *
! *                                 expansion over the B-spline basis.   *
! *                                                                      *
! *  ### 25-MAR-2010  EBSPL_DER_R8  v1.0 (c)  L. Petrov 25-MAR-2010 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      REAL*8     EBSPL_DER_R8 
      INTEGER*4  MN, DEG
      REAL*8     ARG, ARG_VEC(MN), BCOE_VEC(1-DEG:MN-1)
      INTEGER*4  J1, IP
      REAL*8,    EXTERNAL :: BSPL_DER
      INTEGER*4, EXTERNAL :: IXMN8 
!
      EBSPL_DER_R8 = 0.0D0
      IF ( ARG < ARG_VEC(1)  .OR.  ARG > ARG_VEC(MN) ) THEN
!
! -------- If the argument is away from the the interpolation range,
! -------- nothing to do: its value is zero.
!
           RETURN 
      END IF
      IP = IXMN8 ( MN, ARG_VEC, ARG )
      IF ( IP < 1    ) IP = 1 
      IF ( IP > MN-1 ) IP = MN-1
      DO 410 J1=0,DEG
         EBSPL_DER_R8 = EBSPL_DER_R8 + BCOE_VEC(IP-J1)* &
     &                                 BSPL_DER ( MN, ARG_VEC, DEG, IP-J1, ARG )
 410  CONTINUE 
      RETURN
      END  FUNCTION EBSPL_DER_R8  !#!#
!
! ------------------------------------------------------------------------
!
      FUNCTION EBSPL_DR2_R8 ( MN, DEG, ARG, ARG_VEC, BCOE_VEC )
! ************************************************************************
! *                                                                      *
! *   Routine  EBSPL_DR2_R8  computes the second derivative of the       *
! *   function at point with argument ARG using coefficients of its      *
! *   expansion over the B-spline basis.                                 *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *       MN ( INTEGER*4 ) -- The number of knots of the B-spline.       *
! *      DEG ( INTEGER*4 ) -- Degree of B-spline.                        *
! *      ARG ( REAL*8    ) -- Argument for which the second derivative   *
! *                           of the function is computed.               *
! *  ARG_VEC ( REAL*8    ) -- Array of arguments for B-spline at knots.  *
! *                           Dimension: [1:MN].                         *
! * BCOE_VEC ( REAL*8    ) -- Array of B-spline coefficients.            *
! *                           Dimension: [1-DEG:MN-1].                   *
! *                                                                      *
! * _________________________ Output parameters: _______________________ *
! *                                                                      *
! * <EBSPL_DE2_R8> ( REAL*8    ) -- Value of the second derivative of    *
! *                                 the function computed using its      *
! *                                 expansion over the B-spline basis.   *
! *                                                                      *
! *  ### 25-MAR-2010  EBSPL_DR2_R8  v1.0 (c)  L. Petrov 25-MAR-2010 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      REAL*8     EBSPL_DR2_R8 
      INTEGER*4  MN, DEG
      REAL*8     ARG, ARG_VEC(MN), BCOE_VEC(1-DEG:MN-1)
      INTEGER*4  J1, IP
      REAL*8,    EXTERNAL :: BSPL_DR2
      INTEGER*4, EXTERNAL :: IXMN8 
!
      EBSPL_DR2_R8 = 0.0D0
      IF ( ARG < ARG_VEC(1)  .OR.  ARG > ARG_VEC(MN) ) THEN
!
! -------- If the argument is away from the the interpolation range,
! -------- nothing to do: its value is zero.
!
           RETURN 
      END IF
      IP = IXMN8 ( MN, ARG_VEC, ARG )
      IF ( IP < 1    ) IP = 1 
      IF ( IP > MN-1 ) IP = MN-1
      DO 410 J1=0,DEG
         EBSPL_DR2_R8 = EBSPL_DR2_R8 + BCOE_VEC(IP-J1)* &
     &                                 BSPL_DR2 ( MN, ARG_VEC, DEG, IP-J1, ARG )
 410  CONTINUE 
      RETURN
      END  FUNCTION EBSPL_DR2_R8  !#!#
