      SUBROUTINE MUL_VC_V ( N, VEC, C )
! ************************************************************************
! *                                                                      *
! *   Subroutine  MUL_VC_V  multilies vector  VEC  by constant  C  and   *
! *   put the result to VEC.                                             *
! *                                                                      *
! *  ###  12-Dec-96    MUL_VC_V     v1.0  (c)  L. Petrov  12-Dec-96 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INTEGER*4  N, J1
      REAL*8     VEC(N), C
!
      DO 410 J1=1,N
         VEC(J1) = VEC(J1)*C
 410  CONTINUE
!
      RETURN
      END  !#!  MUL_VC_V  #!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE MUL_VC_V8 ( N8, VEC, C )
! ************************************************************************
! *                                                                      *
! *   Subroutine  MUL_VC_V  multilies vector  VEC  by constant  C  and   *
! *   put the result to VEC.                                             *
! *                                                                      *
! *  ###  12-Dec-96    MUL_VC_V     v1.0  (c)  L. Petrov  12-Dec-96 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INTEGER*8  N8, J1
      REAL*8     VEC(N8), C
!
      DO 410 J1=1,N8
         VEC(J1) = VEC(J1)*C
 410  CONTINUE
!
      RETURN
      END  !#!  MUL_VC_V8  #!#
