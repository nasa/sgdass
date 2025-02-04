#ifdef GEN_PREFIX
#define FUNC_DIAD_CVT   GEN_DIAD_CVT
#define FUNC_DIAD_CVT_S GEN_DIAD_CVT_S
#else
#define FUNC_DIAD_CVT   OPT_DIAD_CVT
#define FUNC_DIAD_CVT_S OPT_DIAD_CVT_S
#define FUNC_DAXPY      DAXPY 
#define FUNC_DGER       DGER
#endif
#include <mk5_preprocessor_directives.inc>
      SUBROUTINE FUNC_DIAD_CVT ( CONST, NA, VEC_A, NT, VEC_T, MAT )
! ************************************************************************
! *                                                                      *
! *     Routine  DIAD_CVT  makes diad multiplication vector VEC_A by the *
! *   vector to be transposed with respect to the VEC_T, multiplies the  *
! *   product by the constant CONST and adds the results to the          *
! *   rectangular matrix MAT:                                            *
! *     MAT = MAT + CONST * VEC_A * VEC_T(T).                            *
! *                                                                      *
! *  ###  09-SEP-97    DIAD_CVT    v2.0  (c)  L. Petrov 21-JUL-2002 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INTEGER*4  NA, NT
      REAL*8     CONST, VEC_A(NA), VEC_T(NT), MAT(NA,NT)
!
#ifdef GEN_PREFIX
      INTEGER*4  J1, J2
      DO 410 J1=1,NT
         DO 420 J2=1,NA
            MAT(J2,J1) = MAT(J2,J1) + CONST*VEC_A(J2)*VEC_T(J1)
 420     CONTINUE 
 410  CONTINUE 
      RETURN
#else
      CALL FUNC_DGER ( NA, NT, CONST, VEC_A, 1, VEC_T, 1, MAT, NA )
#endif
!
      RETURN
      END  !#!  FUNC_DIAD_CVT  #!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE FUNC_DIAD_CVT_S ( CONST, N, VEC_A, VEC_T, MAT )
! ************************************************************************
! *                                                                      *
! *   Routine  DIAD_CVT_S  makes diad multiplication vector VEC_A by the *
! *   vector to be transposed with respect to the VEC_T (the same        *
! *   dimension as the vector VEC_A), multiplies product by the constant *
! *   CONST and adds the results to the symmetric matrix MAT in upper    *
! *   triangle representastion: MAT = MAT + CONST * VEC_A * VEC_T(T).    *
! *                                                                      *
! *  ###  05-SEP-97   DIAD_CVT_S   v1.2  (c)  L. Petrov 16-JUL-2003 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INTEGER*4  N, NEL
      REAL*8     CONST, VEC_A(N), VEC_T(N), MAT(*)
!
#ifdef GEN_PREFIX
      INTEGER*4  J1, J2, IP
      IP = 1
      DO 410 J1=1,N
         DO 420 J2=1,J1
            MAT(IP) = MAT(IP) + CONST*VEC_A(J2)*VEC_T(J1)
            IP = IP + 1
 420     CONTINUE 
 410  CONTINUE 
      RETURN
#else
      INTEGER*4  J1, IP
      IP = 1
      DO 410 J1=1,N
         CALL FUNC_DAXPY ( J1, CONST*VEC_T(J1), VEC_A, 1, MAT(IP), 1 )
         IP = IP + J1
 410  CONTINUE 
#endif
!
      RETURN
      END  !#!  DIAD_CVT_S  #!#
