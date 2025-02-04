      SUBROUTINE HPE_NNR_POS ( FAST_DBG, FL_NN_LISTING, L_PAR, C_PAR, &
     &                         MAT, L_HPE, HPE, B3DOBJ, CNSTROBJ, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine HPE_NNR_POS 
! *                                                                      *
! *  ### 25-FEB-2005  HPE_NNR_POS  v1.0 (c)  L. Petrov  25-FEB-2005 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'solve.i'
      INCLUDE   'erm.i'
      INCLUDE   'socom.i'
      INCLUDE   'socom_plus.i'
      INCLUDE   'fast.i'
      INCLUDE   'cnstr.i'
      INTEGER*4  FAST_DBG, L_PAR, L_HPE, IUER
      TYPE ( B3D__STRU   ) :: B3DOBJ
      TYPE ( CNSTR__STRU ) :: CNSTROBJ
      TYPE ( HPE__TYPE   ) :: HPE(L_HPE)
      REAL*8     MAT(*)
      LOGICAL*1  FL_NN_LISTING
      CHARACTER  C_PAR(L_PAR)*(*)
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  HPE_NNR_POS 
