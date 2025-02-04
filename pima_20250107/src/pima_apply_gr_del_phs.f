      SUBROUTINE PIMA_APPLY_GR_DEL_PHS ( NCHN, LFRQ, FREQ_ARR, FREQ_REF, &
     &                                   GR_DEL, PHS_DIF, UV )
! ************************************************************************
! *                                                                      *
! *   Routine PIMA_APPLY_GR_DEL_PHS
! *                                                                      *
! *  ### 17-FEB-2024               v1.0 (c)  L. Petrov  17-FEB-2024 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INTEGER*4  NCHN, LFRQ
      REAL*8     FREQ_ARR(NCHN,LFRQ), FREQ_REF, GR_DEL, PHS_DIF
      REAL*8     PHAS_ADD_R8
      COMPLEX*8  UV(NCHN,LFRQ)
      INTEGER*4  J1, J2
!
      DO 410 J1=1,LFRQ
         DO 420 J2=1,NCHN
            PHAS_ADD_R8 = PHS_DIF + GR_DEL*PI2*(FREQ_ARR(J2,J1) - FREQ_REF)
            UV(J2,J1) = CMPLX ( COS(PHAS_ADD_R8), SIN(PHAS_ADD_R8) )*UV(J2,J1)
 420     CONTINUE 
 410  CONTINUE 
      RETURN
      END  SUBROUTINE  PIMA_APPLY_GR_DEL_PHS  !#!  
