#ifdef GENERIC
#define GEN_PREFIX
#endif
#ifdef GEN_PREFIX
#define FUNC_DP_VV_V GEN_DP_VV_V
#else
#define FUNC_DP_VV_V OPT_DP_VV_V
#endif
#ifdef GENERIC
#define FUNC_DP_VV_V DP_VV_V
#else
#include <mk5_preprocessor_directives.inc>
#endif
      FUNCTION FUNC_DP_VV_V ( N, VEC1, VEC2 )
! ************************************************************************
! *                                                                      *
! *   Function  DP_VV_V  calculates dot product VEC1 and VEC2            *
! *   DP_VV_V = VEC1 * VEC2                                              *
! *                                                                      *
! *  ###  12-DEC-96   DP_VV_V      v2.0  (c)  L. Petrov 05-APR-2001 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'matvec.i' ! definition of DB__DP_VV_V 
      INTEGER*4  N 
      REAL*8     FUNC_DP_VV_V, VEC1(N), VEC2(N), VEC_$DDOT, DDOT
!
#ifdef GEN_PREFIX
!
! --- Generic version of the program
!
      INTEGER*4  J1
      FUNC_DP_VV_V = 0.0D0
      IF ( N .GT. 0 ) THEN
           DO 410 J1=1,N
              FUNC_DP_VV_V = FUNC_DP_VV_V + VEC1(J1)*VEC2(J1)
 410       CONTINUE 
      END IF
#else
      IF ( N .LE. 0 ) THEN
           FUNC_DP_VV_V = 0.0D0
        ELSE IF ( N .EQ. 1 ) THEN
           FUNC_DP_VV_V = VEC1(1)*VEC2(1)
        ELSE IF ( N .EQ. 2 ) THEN
           FUNC_DP_VV_V = VEC1(1)*VEC2(1) + VEC1(2)*VEC2(2) 
        ELSE IF ( N .EQ. 3 ) THEN
           FUNC_DP_VV_V = VEC1(1)*VEC2(1) + VEC1(2)*VEC2(2) + VEC1(3)*VEC2(3)
        ELSE IF ( N .EQ. 4 ) THEN
           FUNC_DP_VV_V = VEC1(1)*VEC2(1) + VEC1(2)*VEC2(2) + VEC1(3)*VEC2(3) + &
     &               VEC1(4)*VEC2(4) 
        ELSE IF ( N .EQ. 5 ) THEN
           FUNC_DP_VV_V = VEC1(1)*VEC2(1) + VEC1(2)*VEC2(2) + VEC1(3)*VEC2(3) + &
     &               VEC1(4)*VEC2(4) + VEC1(5)*VEC2(5) 
        ELSE IF ( N .LE. DB__DP_VV_V ) THEN
#ifdef HPUX
           FUNC_DP_VV_V = VEC_$DDOT ( VEC1, VEC2, N )
#else
           FUNC_DP_VV_V = DOT_PRODUCT ( VEC1, VEC2 )
#endif
        ELSE 
#ifdef HPUX
           FUNC_DP_VV_V = DDOT ( N, VEC1, 1, VEC2, 1 )
#else
           FUNC_DP_VV_V = DOT_PRODUCT ( VEC1, VEC2 )
#endif
      END IF
#endif
!
      RETURN
      END  !#!  DP_VV_V  #!#
