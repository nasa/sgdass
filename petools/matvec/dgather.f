      SUBROUTINE DGATHER ( N, IND, VEC_IN, VEC_OUT )
! ************************************************************************
! *                                                                      *
! *   Routine DGATHER  gathers array VEC_IN to the array VEC_OUT         *
! *   in such a way:                                                     *
! *   VEC_OUT(i) = VEC_IN(ind(i))                                        *
! *                                                                      *
! *   Integer array of indices has dimension N.                          *
! *                                                                      *
! *  ### 24-AUG-2002    DGATHER    v1.0 (c)  L. Petrov  24-AUG-2002 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INTEGER*4  N, IND(N)
      REAL*8     VEC_IN(*), VEC_OUT(*)
      INTEGER*4  J1
!
      DO 410 J1=1,N
         VEC_OUT(J1) = VEC_IN(IND(J1))
 410  CONTINUE 
      RETURN
      END  !#!  DGARTHER  #!#
