      SUBROUTINE DGATHER ( N, IND, VEC_IN, VEC_OUT )
! ************************************************************************
! *                                                                      *
! *   Routine DGATHER  gathers array VEC_IN to the array VEC_OUT         *
! *   in such a way:                                                     *
! *   VEC_OUT(i) = VEC_IN(ind(i))                                        *
! *                                                                      *
! *   Integer array of indices has dimension N.                          *
! *                                                                      *
! *   Copyright (c) 1975-2025 United States Government as represented by *
! *   the Administrator of the National Aeronautics and Space            *
! *   Administration. All Rights Reserved.                               *
! *   License: NASA Open Source Software Agreement (NOSA).               *
! *                                                                      *
! *  ### 24-AUG-2002    DGATHER    v1.0 (d)  L. Petrov  24-AUG-2002 ###  *
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
