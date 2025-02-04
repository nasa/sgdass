      SUBROUTINE DIAD_CVT_S ( CONST, N, VEC_A, VEC_T, MAT )
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
      INTEGER*4  J1, J2, IP
      IP = 1
      DO 410 J1=1,N
         DO 420 J2=1,J1
            MAT(IP) = MAT(IP) + CONST*VEC_A(J2)*VEC_T(J1)
            IP = IP + 1
 420     CONTINUE 
 410  CONTINUE 
      RETURN
!
      RETURN
      END  !#!  DIAD_CVT_S  #!#
