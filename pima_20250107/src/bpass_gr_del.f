      SUBROUTINE BPASS_GR_DEL ( ISTA, PIM, BPS, BPS_GR_DEL, BPS_PHAS )
! ************************************************************************
! *                                                                      *
! *   Routine  BPASS_GR_DEL 
! *                                                                      *
! *  ### 01-AUG-2010  BPASS_GR_DEL  v1.0 (c) L. Petrov  01-AUG-2010 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'pima.i'
      INTEGER*4  ISTA
      REAL*4     BPS_GR_DEL, BPS_PHAS 
      TYPE     ( PIMA__TYPE         ) :: PIM
      TYPE     ( PIM_BPS_STA__TYPE  ) :: BPS
      REAL*8     MB_DEL_MAX
      INTEGER*4  NSTEP
      PARAMETER  ( NSTEP = 8*1024 )
      PARAMETER  ( MB_DEL_MAX = 500.0D-9 )
      REAL*8     M_STEP, MB_DEL, BPS_MB_AMP_MAX, FREQ_DIF
      COMPLEX*8  BPS_MB
      REAL*4     PHAS_ADD
      INTEGER*4  J1, J2, J3, J4, J5, KP
      REAL*4,    EXTERNAL :: PHAS_CMPL_R4
!      
      M_STEP = 2.0D0*MB_DEL_MAX/(NSTEP-1)
      BPS_MB_AMP_MAX = -1.0D0
      DO 410 J1=1,NSTEP
         MB_DEL = -MB_DEL_MAX + M_STEP*(J1-1)
         BPS_MB = 0.0
         KP = 0 
         DO 420 J2=PIM%CONF%BEG_FRQ,PIM%CONF%END_FRQ
            DO 430 J3=1,PIM%NCHN
               FREQ_DIF = PIM%FREQ_ARR(J3,J2,PIM%CONF%FRQ_GRP) - &
     &                    PIM%FREQ_ARR(1,PIM%CONF%BEG_FRQ,PIM%CONF%FRQ_GRP)
               PHAS_ADD = PI2*FREQ_DIF*MB_DEL
               BPS_MB = BPS_MB + BPS%CMPL(J3,J2,ISTA)*CMPLX(COS(PHAS_ADD),SIN(PHAS_ADD))
               KP = KP + 1
 430        CONTINUE 
 420     CONTINUE 
         IF ( ABS(BPS_MB) > BPS_MB_AMP_MAX ) THEN
              BPS_MB_AMP_MAX = ABS(BPS_MB) 
              BPS_GR_DEL =  MB_DEL
              BPS_PHAS   = -PHAS_CMPL_R4(BPS_MB)
         END IF
 410  CONTINUE 
      RETURN
      END  SUBROUTINE  BPASS_GR_DEL  !#!  
