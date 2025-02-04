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
      INTEGER*4  N, NIND, IND(NIND)
      REAL*8     VEC1(NIND), VEC2(N), VECW(MAX__DIM), DPSPA_VV_V
      REAL*8,    EXTERNAL :: DP_VV_V
!
      IF ( NIND .GT. MAX__DIM ) THEN
           WRITE ( 6, * ) ' max__dim=',max__dim,' nind=',nind
           WRITE ( 6, * ) 'DPSPA_VV_V -- fatal error: NIND > M'
           CALL EXIT ( 1 ) 
      END IF
!!      CALL VEC_$DGATHER  ( VEC2, IND, NIND, VECW )
      CALL DGATHER ( NIND, IND, VEC2, VECW )
      DPSPA_VV_V  = DP_VV_V ( NIND, VEC1, VECW )
!
      RETURN
      END  !#!  DPSPA_VV_V  #!#
