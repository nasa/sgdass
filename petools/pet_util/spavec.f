      FUNCTION   DPSPA_VV_V ( N, NIND, VEC1, IND, VEC2 )
! ************************************************************************
! *                                                                      *
! *   Function  DPSPA_VV_V  calculates dot product for the case when     *
! *   one of the constituents is a sparse vector.                        *
! *                                                                      *
! *  ###  26-JAN-98   DPSPA_VV_V   v1.0  (c)  L. Petrov  26-JAN-98  ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'matvec.i'
      INTEGER*4  N, NIND, IND(NIND), M
      PARAMETER  ( M = MAX__DIM )
      REAL*8     VEC1(NIND), VEC2(N), VECW(M), DPSPA_VV_V
      REAL*8,    EXTERNAL :: DP_VV_V
!
      IF ( NIND .GT. M ) THEN
           WRITE ( 6, * ) ' m=',m,' nind=',nind
           STOP 'DPSPA_VV_V -- fatal error: NIND > M'
      END IF
      CALL VEC_$DGATHER  ( VEC2, IND, NIND, VECW )
      DPSPA_VV_V  = DP_VV_V ( NIND, VEC1, VECW )
!
      RETURN
      END  !#!  DPSPA_VV_V  #!#
