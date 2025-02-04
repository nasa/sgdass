      SUBROUTINE NORVEC_UPD ( N, WEI, RH, EQU_OBS, NOR_VEC )
! ************************************************************************
! *                                                                      *
! *   Routine NORVEC_UPD 
! *                                                                      *
! *  ### 12-MAY-2010  NORVEC_UPD   v1.0 (c)  L. Petrov  12-MAY-2010 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INTEGER*4  N
      REAL*8     WEI, RH, EQU_OBS(N), NOR_VEC(N)
      INTEGER*4  J1
!
#ifdef GEN_PREFIX
      DO 410 J1=1,N
         NOR_VEC(J1) = NOR_VEC(J1) + WEI**2*RH*EQU_OBS(J1)
 410  CONTINUE 
#else
      CALL DAXPY ( N, WEI**2*RH, EQU_OBS, 1, NOR_VEC, 1 )
#endif
      RETURN
      END  SUBROUTINE  NORVEC_UPD !#!#  
