      SUBROUTINE PIMA_UV_DRF ( LTIM, LCHN, LFRQ, WEI, AP_LEN, WEI_THR, &
     &                         TIME_FRT, RAT_MOD, GRD_MOD, &
     &                         FREQ_ARR, FREQ_REF, CFRQ_REF, UV, DRF )
! ************************************************************************
! *                                                                      *
! *   Routine  PIMA_UV_DRF
! *                                                                      *
! *  ### 21-AUG-2006  PIMA_UV_DRF  v2.0 (c)  L. Petrov  02-AUG-2009 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INTEGER*4  LTIM, LCHN, LFRQ
      LOGICAL*4  FL_OPT
      REAL*4     WEI(LTIM)
      REAL*8     TIME_FRT, AP_LEN, WEI_THR, RAT_MOD, GRD_MOD, &
     &           FREQ_ARR(LCHN,LFRQ), FREQ_REF, CFRQ_REF(LCHN,LFRQ)
      COMPLEX*8  UV(LCHN,LFRQ,LTIM), DRF, DRF_MOM
      REAL*8     CFRQ_VAL
      REAL*4     PHS_MOD, PHS_STEP, TIM_VAL
      COMPLEX*8  UV_BASE,  UV_STEP
      COMPLEX*8  UV_BASE1, UV_BASE2, UV_BASE3, UV_BASE4, &
     &           UV_BASE5, UV_BASE6, UV_BASE7, UV_BASE8, &
     &           UV_STEP1, UV_STEP2, UV_STEP4, UV_STEP8
      INTEGER*4  J1, J2, J3
!
      FL_OPT = .TRUE. 
!!  fl_opt = .false. ! $$$$%$$$$$$$$$$$$$$$ %%%%%%%%%%%%%
!
      DRF    = CMPLX ( 0.0, 0.0 )
      CFRQ_VAL = PI2*FREQ_REF
      IF ( FL_OPT .AND. LCHN == 4 ) THEN
!
! -------- Optimized version of the algorithm for 4 channels
!
           DO 110 J1=1,LTIM
              IF ( WEI(J1) < WEI_THR ) GOTO 110
              TIM_VAL = ((J1-1)*AP_LEN - TIME_FRT)
!
              PHS_STEP = (CFRQ_REF(2,1)-CFRQ_REF(1,1))* &
     &                   (GRD_MOD + RAT_MOD*TIM_VAL)
              UV_STEP  = CMPLX ( COS(PHS_STEP), SIN(PHS_STEP) )
              UV_STEP1 = CMPLX ( COS(PHS_STEP), SIN(PHS_STEP) )
              UV_STEP2 = UV_STEP1*UV_STEP1 
!
              DRF_MOM  = CMPLX ( 0.0, 0.0 )
              DO 120 J2=1,LFRQ
                 PHS_MOD  =   CFRQ_VAL*RAT_MOD*TIM_VAL       &
     &                      + CFRQ_REF(1,J2)*(GRD_MOD + RAT_MOD*TIM_VAL)
                 UV_BASE1 = CMPLX ( COS(PHS_MOD), SIN(PHS_MOD) )
                 UV_BASE2 = UV_BASE1*UV_STEP1
                 UV_BASE3 = UV_BASE1*UV_STEP2
                 UV_BASE4 = UV_BASE2*UV_STEP2
!
                 DRF_MOM = DRF_MOM + UV(1,J2,J1)*UV_BASE1
                 DRF_MOM = DRF_MOM + UV(2,J2,J1)*UV_BASE2
                 DRF_MOM = DRF_MOM + UV(3,J2,J1)*UV_BASE3
                 DRF_MOM = DRF_MOM + UV(4,J2,J1)*UV_BASE4
 120          CONTINUE 
              DRF = DRF + DRF_MOM*WEI(J1)
 110       CONTINUE 
         ELSE IF ( FL_OPT .AND. LCHN == 8 ) THEN
!
! -------- Optimized version of the algorithm for 8 channels
!
           DO 210 J1=1,LTIM
              IF ( WEI(J1) < WEI_THR ) GOTO 210
              TIM_VAL = ((J1-1)*AP_LEN - TIME_FRT)
!
              PHS_STEP = (CFRQ_REF(2,1)-CFRQ_REF(1,1))* &
     &                   (GRD_MOD + RAT_MOD*TIM_VAL)
              UV_STEP  = CMPLX ( COS(PHS_STEP), SIN(PHS_STEP) )
              UV_STEP1 = CMPLX ( COS(PHS_STEP), SIN(PHS_STEP) )
              UV_STEP2 = UV_STEP1*UV_STEP1 
              UV_STEP4 = UV_STEP2*UV_STEP2
!
              DRF_MOM  = CMPLX ( 0.0, 0.0 )
              DO 220 J2=1,LFRQ
                 PHS_MOD  =   CFRQ_VAL*RAT_MOD*TIM_VAL       &
     &                      + CFRQ_REF(1,J2)*(GRD_MOD + RAT_MOD*TIM_VAL)
                 UV_BASE1 = CMPLX ( COS(PHS_MOD), SIN(PHS_MOD) )
                 UV_BASE2 = UV_BASE1*UV_STEP1
                 UV_BASE3 = UV_BASE1*UV_STEP2
                 UV_BASE4 = UV_BASE2*UV_STEP2
!
                 UV_BASE5 = UV_BASE1*UV_STEP4
                 UV_BASE6 = UV_BASE2*UV_STEP4
                 UV_BASE7 = UV_BASE3*UV_STEP4
                 UV_BASE8 = UV_BASE4*UV_STEP4
!
                 DRF_MOM = DRF_MOM + UV(1,J2,J1)*UV_BASE1
                 DRF_MOM = DRF_MOM + UV(2,J2,J1)*UV_BASE2
                 DRF_MOM = DRF_MOM + UV(3,J2,J1)*UV_BASE3
                 DRF_MOM = DRF_MOM + UV(4,J2,J1)*UV_BASE4
!
                 DRF_MOM = DRF_MOM + UV(5,J2,J1)*UV_BASE5
                 DRF_MOM = DRF_MOM + UV(6,J2,J1)*UV_BASE6
                 DRF_MOM = DRF_MOM + UV(7,J2,J1)*UV_BASE7
                 DRF_MOM = DRF_MOM + UV(8,J2,J1)*UV_BASE8
 220          CONTINUE 
              DRF = DRF + DRF_MOM*WEI(J1)
 210       CONTINUE 
         ELSE IF ( FL_OPT .AND. LCHN == 16 ) THEN
!
! -------- Optimized version of the algorithm for 16 channels
!
           DO 310 J1=1,LTIM
              IF ( WEI(J1) < WEI_THR ) GOTO 310
              TIM_VAL = ((J1-1)*AP_LEN - TIME_FRT)
!
              PHS_STEP = (CFRQ_REF(2,1)-CFRQ_REF(1,1))* &
     &                   (GRD_MOD + RAT_MOD*TIM_VAL)
              UV_STEP  = CMPLX ( COS(PHS_STEP), SIN(PHS_STEP) )
              UV_STEP1 = CMPLX ( COS(PHS_STEP), SIN(PHS_STEP) )
              UV_STEP2 = UV_STEP1*UV_STEP1 
              UV_STEP4 = UV_STEP2*UV_STEP2
              UV_STEP8 = UV_STEP4*UV_STEP4
!
              DRF_MOM  = CMPLX ( 0.0, 0.0 )
              DO 320 J2=1,LFRQ
                 PHS_MOD  =   CFRQ_VAL*RAT_MOD*TIM_VAL       &
     &                      + CFRQ_REF(1,J2)*(GRD_MOD + RAT_MOD*TIM_VAL)
                 UV_BASE1 = CMPLX ( COS(PHS_MOD), SIN(PHS_MOD) )
                 UV_BASE2 = UV_BASE1*UV_STEP1
                 UV_BASE3 = UV_BASE1*UV_STEP2
                 UV_BASE4 = UV_BASE2*UV_STEP2
                 UV_BASE5 = UV_BASE1*UV_STEP4
                 UV_BASE6 = UV_BASE2*UV_STEP4
                 UV_BASE7 = UV_BASE3*UV_STEP4
                 UV_BASE8 = UV_BASE4*UV_STEP4
!
                 DRF_MOM = DRF_MOM + UV(1,J2,J1)*UV_BASE1
                 DRF_MOM = DRF_MOM + UV(2,J2,J1)*UV_BASE2
                 DRF_MOM = DRF_MOM + UV(3,J2,J1)*UV_BASE3
                 DRF_MOM = DRF_MOM + UV(4,J2,J1)*UV_BASE4
                 DRF_MOM = DRF_MOM + UV(5,J2,J1)*UV_BASE5
                 DRF_MOM = DRF_MOM + UV(6,J2,J1)*UV_BASE6
                 DRF_MOM = DRF_MOM + UV(7,J2,J1)*UV_BASE7
                 DRF_MOM = DRF_MOM + UV(8,J2,J1)*UV_BASE8
!
                 UV_BASE1 = UV_BASE1*UV_STEP8
                 UV_BASE2 = UV_BASE2*UV_STEP8
                 UV_BASE3 = UV_BASE3*UV_STEP8
                 UV_BASE4 = UV_BASE4*UV_STEP8
                 UV_BASE5 = UV_BASE5*UV_STEP8
                 UV_BASE6 = UV_BASE6*UV_STEP8
                 UV_BASE7 = UV_BASE7*UV_STEP8
                 UV_BASE8 = UV_BASE8*UV_STEP8
!
                 DRF_MOM = DRF_MOM + UV(9,J2,J1)*UV_BASE1
                 DRF_MOM = DRF_MOM + UV(10,J2,J1)*UV_BASE2
                 DRF_MOM = DRF_MOM + UV(11,J2,J1)*UV_BASE3
                 DRF_MOM = DRF_MOM + UV(12,J2,J1)*UV_BASE4
                 DRF_MOM = DRF_MOM + UV(13,J2,J1)*UV_BASE5
                 DRF_MOM = DRF_MOM + UV(14,J2,J1)*UV_BASE6
                 DRF_MOM = DRF_MOM + UV(15,J2,J1)*UV_BASE7
                 DRF_MOM = DRF_MOM + UV(16,J2,J1)*UV_BASE8
 320          CONTINUE 
              DRF = DRF + DRF_MOM*WEI(J1)
 310       CONTINUE 
         ELSE IF ( FL_OPT .AND. LCHN == 32 ) THEN
!
! --------- Optimized version of the algorithm for 32 channels
!
           DO 410 J1=1,LTIM
              IF ( WEI(J1) < WEI_THR ) GOTO 410
              TIM_VAL = ((J1-1)*AP_LEN - TIME_FRT)
!
              PHS_STEP = (CFRQ_REF(2,1)-CFRQ_REF(1,1))* &
     &                   (GRD_MOD + RAT_MOD*TIM_VAL)
              UV_STEP  = CMPLX ( COS(PHS_STEP), SIN(PHS_STEP) )
              UV_STEP1 = CMPLX ( COS(PHS_STEP), SIN(PHS_STEP) )
              UV_STEP2 = UV_STEP1*UV_STEP1 
              UV_STEP4 = UV_STEP2*UV_STEP2
              UV_STEP8 = UV_STEP4*UV_STEP4
!
              DRF_MOM  = CMPLX ( 0.0, 0.0 )
              DO 420 J2=1,LFRQ
                 PHS_MOD  =   CFRQ_VAL*RAT_MOD*TIM_VAL       &
     &                      + CFRQ_REF(1,J2)*(GRD_MOD + RAT_MOD*TIM_VAL)
                 UV_BASE1 = CMPLX ( COS(PHS_MOD), SIN(PHS_MOD) )
                 UV_BASE2 = UV_BASE1*UV_STEP1
                 UV_BASE3 = UV_BASE1*UV_STEP2
                 UV_BASE4 = UV_BASE2*UV_STEP2
                 UV_BASE5 = UV_BASE1*UV_STEP4
                 UV_BASE6 = UV_BASE2*UV_STEP4
                 UV_BASE7 = UV_BASE3*UV_STEP4
                 UV_BASE8 = UV_BASE4*UV_STEP4
!
                 DRF_MOM = DRF_MOM + UV(1,J2,J1)*UV_BASE1
                 DRF_MOM = DRF_MOM + UV(2,J2,J1)*UV_BASE2
                 DRF_MOM = DRF_MOM + UV(3,J2,J1)*UV_BASE3
                 DRF_MOM = DRF_MOM + UV(4,J2,J1)*UV_BASE4
                 DRF_MOM = DRF_MOM + UV(5,J2,J1)*UV_BASE5
                 DRF_MOM = DRF_MOM + UV(6,J2,J1)*UV_BASE6
                 DRF_MOM = DRF_MOM + UV(7,J2,J1)*UV_BASE7
                 DRF_MOM = DRF_MOM + UV(8,J2,J1)*UV_BASE8
!
                 UV_BASE1 = UV_BASE1*UV_STEP8
                 UV_BASE2 = UV_BASE2*UV_STEP8
                 UV_BASE3 = UV_BASE3*UV_STEP8
                 UV_BASE4 = UV_BASE4*UV_STEP8
                 UV_BASE5 = UV_BASE5*UV_STEP8
                 UV_BASE6 = UV_BASE6*UV_STEP8
                 UV_BASE7 = UV_BASE7*UV_STEP8
                 UV_BASE8 = UV_BASE8*UV_STEP8
!
                 DRF_MOM = DRF_MOM + UV(9,J2,J1)*UV_BASE1
                 DRF_MOM = DRF_MOM + UV(10,J2,J1)*UV_BASE2
                 DRF_MOM = DRF_MOM + UV(11,J2,J1)*UV_BASE3
                 DRF_MOM = DRF_MOM + UV(12,J2,J1)*UV_BASE4
                 DRF_MOM = DRF_MOM + UV(13,J2,J1)*UV_BASE5
                 DRF_MOM = DRF_MOM + UV(14,J2,J1)*UV_BASE6
                 DRF_MOM = DRF_MOM + UV(15,J2,J1)*UV_BASE7
                 DRF_MOM = DRF_MOM + UV(16,J2,J1)*UV_BASE8
!
                 UV_BASE1 = UV_BASE1*UV_STEP8
                 UV_BASE2 = UV_BASE2*UV_STEP8
                 UV_BASE3 = UV_BASE3*UV_STEP8
                 UV_BASE4 = UV_BASE4*UV_STEP8
                 UV_BASE5 = UV_BASE5*UV_STEP8
                 UV_BASE6 = UV_BASE6*UV_STEP8
                 UV_BASE7 = UV_BASE7*UV_STEP8
                 UV_BASE8 = UV_BASE8*UV_STEP8
!
                 DRF_MOM = DRF_MOM + UV(17,J2,J1)*UV_BASE1
                 DRF_MOM = DRF_MOM + UV(18,J2,J1)*UV_BASE2
                 DRF_MOM = DRF_MOM + UV(19,J2,J1)*UV_BASE3
                 DRF_MOM = DRF_MOM + UV(20,J2,J1)*UV_BASE4
                 DRF_MOM = DRF_MOM + UV(21,J2,J1)*UV_BASE5
                 DRF_MOM = DRF_MOM + UV(22,J2,J1)*UV_BASE6
                 DRF_MOM = DRF_MOM + UV(23,J2,J1)*UV_BASE7
                 DRF_MOM = DRF_MOM + UV(24,J2,J1)*UV_BASE8
!
                 UV_BASE1 = UV_BASE1*UV_STEP8
                 UV_BASE2 = UV_BASE2*UV_STEP8
                 UV_BASE3 = UV_BASE3*UV_STEP8
                 UV_BASE4 = UV_BASE4*UV_STEP8
                 UV_BASE5 = UV_BASE5*UV_STEP8
                 UV_BASE6 = UV_BASE6*UV_STEP8
                 UV_BASE7 = UV_BASE7*UV_STEP8
                 UV_BASE8 = UV_BASE8*UV_STEP8
!
                 DRF_MOM = DRF_MOM + UV(25,J2,J1)*UV_BASE1
                 DRF_MOM = DRF_MOM + UV(26,J2,J1)*UV_BASE2
                 DRF_MOM = DRF_MOM + UV(27,J2,J1)*UV_BASE3
                 DRF_MOM = DRF_MOM + UV(28,J2,J1)*UV_BASE4
                 DRF_MOM = DRF_MOM + UV(29,J2,J1)*UV_BASE5
                 DRF_MOM = DRF_MOM + UV(30,J2,J1)*UV_BASE6
                 DRF_MOM = DRF_MOM + UV(31,J2,J1)*UV_BASE7
                 DRF_MOM = DRF_MOM + UV(32,J2,J1)*UV_BASE8
 420          CONTINUE 
              DRF = DRF + DRF_MOM*WEI(J1)
 410       CONTINUE 
         ELSE IF ( FL_OPT .AND. LCHN == 64 ) THEN
!
! --------- Optimized version of the algorithm for 64 channels
!   
           DO 510 J1=1,LTIM
              IF ( WEI(J1) < WEI_THR ) GOTO 510
              TIM_VAL = ((J1-1)*AP_LEN - TIME_FRT)
!
              PHS_STEP = (CFRQ_REF(2,1)-CFRQ_REF(1,1))* &
     &                   (GRD_MOD + RAT_MOD*TIM_VAL)
              UV_STEP  = CMPLX ( COS(PHS_STEP), SIN(PHS_STEP) )
              UV_STEP1 = CMPLX ( COS(PHS_STEP), SIN(PHS_STEP) )
              UV_STEP2 = UV_STEP1*UV_STEP1 
              UV_STEP4 = UV_STEP2*UV_STEP2
              UV_STEP8 = UV_STEP4*UV_STEP4
!
              DRF_MOM  = CMPLX ( 0.0, 0.0 )
              DO 520 J2=1,LFRQ
                 PHS_MOD  =   CFRQ_VAL*RAT_MOD*TIM_VAL       &
     &                      + CFRQ_REF(1,J2)*(GRD_MOD + RAT_MOD*TIM_VAL)
                 UV_BASE1 = CMPLX ( COS(PHS_MOD), SIN(PHS_MOD) )
                 UV_BASE2 = UV_BASE1*UV_STEP1
                 UV_BASE3 = UV_BASE1*UV_STEP2
                 UV_BASE4 = UV_BASE2*UV_STEP2
                 UV_BASE5 = UV_BASE1*UV_STEP4
                 UV_BASE6 = UV_BASE2*UV_STEP4
                 UV_BASE7 = UV_BASE3*UV_STEP4
                 UV_BASE8 = UV_BASE4*UV_STEP4
!
                 DRF_MOM = DRF_MOM + UV(1,J2,J1)*UV_BASE1
                 DRF_MOM = DRF_MOM + UV(2,J2,J1)*UV_BASE2
                 DRF_MOM = DRF_MOM + UV(3,J2,J1)*UV_BASE3
                 DRF_MOM = DRF_MOM + UV(4,J2,J1)*UV_BASE4
                 DRF_MOM = DRF_MOM + UV(5,J2,J1)*UV_BASE5
                 DRF_MOM = DRF_MOM + UV(6,J2,J1)*UV_BASE6
                 DRF_MOM = DRF_MOM + UV(7,J2,J1)*UV_BASE7
                 DRF_MOM = DRF_MOM + UV(8,J2,J1)*UV_BASE8
!
                 UV_BASE1 = UV_BASE1*UV_STEP8
                 UV_BASE2 = UV_BASE2*UV_STEP8
                 UV_BASE3 = UV_BASE3*UV_STEP8
                 UV_BASE4 = UV_BASE4*UV_STEP8
                 UV_BASE5 = UV_BASE5*UV_STEP8
                 UV_BASE6 = UV_BASE6*UV_STEP8
                 UV_BASE7 = UV_BASE7*UV_STEP8
                 UV_BASE8 = UV_BASE8*UV_STEP8
!
                 DRF_MOM = DRF_MOM + UV(9,J2,J1)*UV_BASE1
                 DRF_MOM = DRF_MOM + UV(10,J2,J1)*UV_BASE2
                 DRF_MOM = DRF_MOM + UV(11,J2,J1)*UV_BASE3
                 DRF_MOM = DRF_MOM + UV(12,J2,J1)*UV_BASE4
                 DRF_MOM = DRF_MOM + UV(13,J2,J1)*UV_BASE5
                 DRF_MOM = DRF_MOM + UV(14,J2,J1)*UV_BASE6
                 DRF_MOM = DRF_MOM + UV(15,J2,J1)*UV_BASE7
                 DRF_MOM = DRF_MOM + UV(16,J2,J1)*UV_BASE8
!
                 UV_BASE1 = UV_BASE1*UV_STEP8
                 UV_BASE2 = UV_BASE2*UV_STEP8
                 UV_BASE3 = UV_BASE3*UV_STEP8
                 UV_BASE4 = UV_BASE4*UV_STEP8
                 UV_BASE5 = UV_BASE5*UV_STEP8
                 UV_BASE6 = UV_BASE6*UV_STEP8
                 UV_BASE7 = UV_BASE7*UV_STEP8
                 UV_BASE8 = UV_BASE8*UV_STEP8
!
                 DRF_MOM = DRF_MOM + UV(17,J2,J1)*UV_BASE1
                 DRF_MOM = DRF_MOM + UV(18,J2,J1)*UV_BASE2
                 DRF_MOM = DRF_MOM + UV(19,J2,J1)*UV_BASE3
                 DRF_MOM = DRF_MOM + UV(20,J2,J1)*UV_BASE4
                 DRF_MOM = DRF_MOM + UV(21,J2,J1)*UV_BASE5
                 DRF_MOM = DRF_MOM + UV(22,J2,J1)*UV_BASE6
                 DRF_MOM = DRF_MOM + UV(23,J2,J1)*UV_BASE7
                 DRF_MOM = DRF_MOM + UV(24,J2,J1)*UV_BASE8
!
                 UV_BASE1 = UV_BASE1*UV_STEP8
                 UV_BASE2 = UV_BASE2*UV_STEP8
                 UV_BASE3 = UV_BASE3*UV_STEP8
                 UV_BASE4 = UV_BASE4*UV_STEP8
                 UV_BASE5 = UV_BASE5*UV_STEP8
                 UV_BASE6 = UV_BASE6*UV_STEP8
                 UV_BASE7 = UV_BASE7*UV_STEP8
                 UV_BASE8 = UV_BASE8*UV_STEP8
!
                 DRF_MOM = DRF_MOM + UV(25,J2,J1)*UV_BASE1
                 DRF_MOM = DRF_MOM + UV(26,J2,J1)*UV_BASE2
                 DRF_MOM = DRF_MOM + UV(27,J2,J1)*UV_BASE3
                 DRF_MOM = DRF_MOM + UV(28,J2,J1)*UV_BASE4
                 DRF_MOM = DRF_MOM + UV(29,J2,J1)*UV_BASE5
                 DRF_MOM = DRF_MOM + UV(30,J2,J1)*UV_BASE6
                 DRF_MOM = DRF_MOM + UV(31,J2,J1)*UV_BASE7
                 DRF_MOM = DRF_MOM + UV(32,J2,J1)*UV_BASE8
!
                 UV_BASE1 = UV_BASE1*UV_STEP8
                 UV_BASE2 = UV_BASE2*UV_STEP8
                 UV_BASE3 = UV_BASE3*UV_STEP8
                 UV_BASE4 = UV_BASE4*UV_STEP8
                 UV_BASE5 = UV_BASE5*UV_STEP8
                 UV_BASE6 = UV_BASE6*UV_STEP8
                 UV_BASE7 = UV_BASE7*UV_STEP8
                 UV_BASE8 = UV_BASE8*UV_STEP8
!
                 DRF_MOM = DRF_MOM + UV(33,J2,J1)*UV_BASE1
                 DRF_MOM = DRF_MOM + UV(34,J2,J1)*UV_BASE2
                 DRF_MOM = DRF_MOM + UV(35,J2,J1)*UV_BASE3
                 DRF_MOM = DRF_MOM + UV(36,J2,J1)*UV_BASE4
                 DRF_MOM = DRF_MOM + UV(37,J2,J1)*UV_BASE5
                 DRF_MOM = DRF_MOM + UV(38,J2,J1)*UV_BASE6
                 DRF_MOM = DRF_MOM + UV(39,J2,J1)*UV_BASE7
                 DRF_MOM = DRF_MOM + UV(40,J2,J1)*UV_BASE8
!
                 UV_BASE1 = UV_BASE1*UV_STEP8
                 UV_BASE2 = UV_BASE2*UV_STEP8
                 UV_BASE3 = UV_BASE3*UV_STEP8
                 UV_BASE4 = UV_BASE4*UV_STEP8
                 UV_BASE5 = UV_BASE5*UV_STEP8
                 UV_BASE6 = UV_BASE6*UV_STEP8
                 UV_BASE7 = UV_BASE7*UV_STEP8
                 UV_BASE8 = UV_BASE8*UV_STEP8
!
                 DRF_MOM = DRF_MOM + UV(41,J2,J1)*UV_BASE1
                 DRF_MOM = DRF_MOM + UV(42,J2,J1)*UV_BASE2
                 DRF_MOM = DRF_MOM + UV(43,J2,J1)*UV_BASE3
                 DRF_MOM = DRF_MOM + UV(44,J2,J1)*UV_BASE4
                 DRF_MOM = DRF_MOM + UV(45,J2,J1)*UV_BASE5
                 DRF_MOM = DRF_MOM + UV(46,J2,J1)*UV_BASE6
                 DRF_MOM = DRF_MOM + UV(47,J2,J1)*UV_BASE7
                 DRF_MOM = DRF_MOM + UV(48,J2,J1)*UV_BASE8
!
                 UV_BASE1 = UV_BASE1*UV_STEP8
                 UV_BASE2 = UV_BASE2*UV_STEP8
                 UV_BASE3 = UV_BASE3*UV_STEP8
                 UV_BASE4 = UV_BASE4*UV_STEP8
                 UV_BASE5 = UV_BASE5*UV_STEP8
                 UV_BASE6 = UV_BASE6*UV_STEP8
                 UV_BASE7 = UV_BASE7*UV_STEP8
                 UV_BASE8 = UV_BASE8*UV_STEP8
!
                 DRF_MOM = DRF_MOM + UV(49,J2,J1)*UV_BASE1
                 DRF_MOM = DRF_MOM + UV(50,J2,J1)*UV_BASE2
                 DRF_MOM = DRF_MOM + UV(51,J2,J1)*UV_BASE3
                 DRF_MOM = DRF_MOM + UV(52,J2,J1)*UV_BASE4
                 DRF_MOM = DRF_MOM + UV(53,J2,J1)*UV_BASE5
                 DRF_MOM = DRF_MOM + UV(54,J2,J1)*UV_BASE6
                 DRF_MOM = DRF_MOM + UV(55,J2,J1)*UV_BASE7
                 DRF_MOM = DRF_MOM + UV(56,J2,J1)*UV_BASE8
!
                 UV_BASE1 = UV_BASE1*UV_STEP8
                 UV_BASE2 = UV_BASE2*UV_STEP8
                 UV_BASE3 = UV_BASE3*UV_STEP8
                 UV_BASE4 = UV_BASE4*UV_STEP8
                 UV_BASE5 = UV_BASE5*UV_STEP8
                 UV_BASE6 = UV_BASE6*UV_STEP8
                 UV_BASE7 = UV_BASE7*UV_STEP8
                 UV_BASE8 = UV_BASE8*UV_STEP8
!
                 DRF_MOM = DRF_MOM + UV(57,J2,J1)*UV_BASE1
                 DRF_MOM = DRF_MOM + UV(58,J2,J1)*UV_BASE2
                 DRF_MOM = DRF_MOM + UV(59,J2,J1)*UV_BASE3
                 DRF_MOM = DRF_MOM + UV(60,J2,J1)*UV_BASE4
                 DRF_MOM = DRF_MOM + UV(61,J2,J1)*UV_BASE5
                 DRF_MOM = DRF_MOM + UV(62,J2,J1)*UV_BASE6
                 DRF_MOM = DRF_MOM + UV(63,J2,J1)*UV_BASE7
                 DRF_MOM = DRF_MOM + UV(64,J2,J1)*UV_BASE8
 520          CONTINUE 
              DRF = DRF + DRF_MOM*WEI(J1)
 510       CONTINUE 
         ELSE IF ( FL_OPT .AND. LCHN == 128 ) THEN
!
! --------- Optimized version of the algorithm for 128 channels
!   
           DO 610 J1=1,LTIM
              IF ( WEI(J1) < WEI_THR ) GOTO 610
              TIM_VAL = ((J1-1)*AP_LEN - TIME_FRT)
!
              PHS_STEP  = (CFRQ_REF(2,1)-CFRQ_REF(1,1))* &
     &                    (GRD_MOD + RAT_MOD*TIM_VAL)
              UV_STEP   = CMPLX ( COS(PHS_STEP), SIN(PHS_STEP) )
              UV_STEP1  = CMPLX ( COS(PHS_STEP), SIN(PHS_STEP) )
              UV_STEP2  = UV_STEP1*UV_STEP1 
              UV_STEP4  = UV_STEP2*UV_STEP2
              UV_STEP8  = UV_STEP4*UV_STEP4
!
              DRF_MOM  = CMPLX ( 0.0, 0.0 )
              DO 620 J2=1,LFRQ
                 PHS_MOD  =   CFRQ_VAL*RAT_MOD*TIM_VAL       &
     &                      + CFRQ_REF(1,J2)*(GRD_MOD + RAT_MOD*TIM_VAL)
                 UV_BASE1  = CMPLX ( COS(PHS_MOD), SIN(PHS_MOD) )
                 UV_BASE2  = UV_BASE1*UV_STEP1
                 UV_BASE3  = UV_BASE1*UV_STEP2
                 UV_BASE4  = UV_BASE2*UV_STEP2
                 UV_BASE5  = UV_BASE1*UV_STEP4
                 UV_BASE6  = UV_BASE2*UV_STEP4
                 UV_BASE7  = UV_BASE3*UV_STEP4
                 UV_BASE8  = UV_BASE4*UV_STEP4
!
                 DRF_MOM = DRF_MOM + UV(1,J2,J1)*UV_BASE1
                 DRF_MOM = DRF_MOM + UV(2,J2,J1)*UV_BASE2
                 DRF_MOM = DRF_MOM + UV(3,J2,J1)*UV_BASE3
                 DRF_MOM = DRF_MOM + UV(4,J2,J1)*UV_BASE4
                 DRF_MOM = DRF_MOM + UV(5,J2,J1)*UV_BASE5
                 DRF_MOM = DRF_MOM + UV(6,J2,J1)*UV_BASE6
                 DRF_MOM = DRF_MOM + UV(7,J2,J1)*UV_BASE7
                 DRF_MOM = DRF_MOM + UV(8,J2,J1)*UV_BASE8
!
                 UV_BASE1 = UV_BASE1*UV_STEP8
                 UV_BASE2 = UV_BASE2*UV_STEP8
                 UV_BASE3 = UV_BASE3*UV_STEP8
                 UV_BASE4 = UV_BASE4*UV_STEP8
                 UV_BASE5 = UV_BASE5*UV_STEP8
                 UV_BASE6 = UV_BASE6*UV_STEP8
                 UV_BASE7 = UV_BASE7*UV_STEP8
                 UV_BASE8 = UV_BASE8*UV_STEP8
!
                 DRF_MOM = DRF_MOM + UV(9,J2,J1)*UV_BASE1
                 DRF_MOM = DRF_MOM + UV(10,J2,J1)*UV_BASE2
                 DRF_MOM = DRF_MOM + UV(11,J2,J1)*UV_BASE3
                 DRF_MOM = DRF_MOM + UV(12,J2,J1)*UV_BASE4
                 DRF_MOM = DRF_MOM + UV(13,J2,J1)*UV_BASE5
                 DRF_MOM = DRF_MOM + UV(14,J2,J1)*UV_BASE6
                 DRF_MOM = DRF_MOM + UV(15,J2,J1)*UV_BASE7
                 DRF_MOM = DRF_MOM + UV(16,J2,J1)*UV_BASE8
!
                 UV_BASE1 = UV_BASE1*UV_STEP8
                 UV_BASE2 = UV_BASE2*UV_STEP8
                 UV_BASE3 = UV_BASE3*UV_STEP8
                 UV_BASE4 = UV_BASE4*UV_STEP8
                 UV_BASE5 = UV_BASE5*UV_STEP8
                 UV_BASE6 = UV_BASE6*UV_STEP8
                 UV_BASE7 = UV_BASE7*UV_STEP8
                 UV_BASE8 = UV_BASE8*UV_STEP8
!
                 DRF_MOM = DRF_MOM + UV(17,J2,J1)*UV_BASE1
                 DRF_MOM = DRF_MOM + UV(18,J2,J1)*UV_BASE2
                 DRF_MOM = DRF_MOM + UV(19,J2,J1)*UV_BASE3
                 DRF_MOM = DRF_MOM + UV(20,J2,J1)*UV_BASE4
                 DRF_MOM = DRF_MOM + UV(21,J2,J1)*UV_BASE5
                 DRF_MOM = DRF_MOM + UV(22,J2,J1)*UV_BASE6
                 DRF_MOM = DRF_MOM + UV(23,J2,J1)*UV_BASE7
                 DRF_MOM = DRF_MOM + UV(24,J2,J1)*UV_BASE8
!
                 UV_BASE1 = UV_BASE1*UV_STEP8
                 UV_BASE2 = UV_BASE2*UV_STEP8
                 UV_BASE3 = UV_BASE3*UV_STEP8
                 UV_BASE4 = UV_BASE4*UV_STEP8
                 UV_BASE5 = UV_BASE5*UV_STEP8
                 UV_BASE6 = UV_BASE6*UV_STEP8
                 UV_BASE7 = UV_BASE7*UV_STEP8
                 UV_BASE8 = UV_BASE8*UV_STEP8
!
                 DRF_MOM = DRF_MOM + UV(25,J2,J1)*UV_BASE1
                 DRF_MOM = DRF_MOM + UV(26,J2,J1)*UV_BASE2
                 DRF_MOM = DRF_MOM + UV(27,J2,J1)*UV_BASE3
                 DRF_MOM = DRF_MOM + UV(28,J2,J1)*UV_BASE4
                 DRF_MOM = DRF_MOM + UV(29,J2,J1)*UV_BASE5
                 DRF_MOM = DRF_MOM + UV(30,J2,J1)*UV_BASE6
                 DRF_MOM = DRF_MOM + UV(31,J2,J1)*UV_BASE7
                 DRF_MOM = DRF_MOM + UV(32,J2,J1)*UV_BASE8
!
                 UV_BASE1 = UV_BASE1*UV_STEP8
                 UV_BASE2 = UV_BASE2*UV_STEP8
                 UV_BASE3 = UV_BASE3*UV_STEP8
                 UV_BASE4 = UV_BASE4*UV_STEP8
                 UV_BASE5 = UV_BASE5*UV_STEP8
                 UV_BASE6 = UV_BASE6*UV_STEP8
                 UV_BASE7 = UV_BASE7*UV_STEP8
                 UV_BASE8 = UV_BASE8*UV_STEP8
!
                 DRF_MOM = DRF_MOM + UV(33,J2,J1)*UV_BASE1
                 DRF_MOM = DRF_MOM + UV(34,J2,J1)*UV_BASE2
                 DRF_MOM = DRF_MOM + UV(35,J2,J1)*UV_BASE3
                 DRF_MOM = DRF_MOM + UV(36,J2,J1)*UV_BASE4
                 DRF_MOM = DRF_MOM + UV(37,J2,J1)*UV_BASE5
                 DRF_MOM = DRF_MOM + UV(38,J2,J1)*UV_BASE6
                 DRF_MOM = DRF_MOM + UV(39,J2,J1)*UV_BASE7
                 DRF_MOM = DRF_MOM + UV(40,J2,J1)*UV_BASE8
!
                 UV_BASE1 = UV_BASE1*UV_STEP8
                 UV_BASE2 = UV_BASE2*UV_STEP8
                 UV_BASE3 = UV_BASE3*UV_STEP8
                 UV_BASE4 = UV_BASE4*UV_STEP8
                 UV_BASE5 = UV_BASE5*UV_STEP8
                 UV_BASE6 = UV_BASE6*UV_STEP8
                 UV_BASE7 = UV_BASE7*UV_STEP8
                 UV_BASE8 = UV_BASE8*UV_STEP8
!
                 DRF_MOM = DRF_MOM + UV(41,J2,J1)*UV_BASE1
                 DRF_MOM = DRF_MOM + UV(42,J2,J1)*UV_BASE2
                 DRF_MOM = DRF_MOM + UV(43,J2,J1)*UV_BASE3
                 DRF_MOM = DRF_MOM + UV(44,J2,J1)*UV_BASE4
                 DRF_MOM = DRF_MOM + UV(45,J2,J1)*UV_BASE5
                 DRF_MOM = DRF_MOM + UV(46,J2,J1)*UV_BASE6
                 DRF_MOM = DRF_MOM + UV(47,J2,J1)*UV_BASE7
                 DRF_MOM = DRF_MOM + UV(48,J2,J1)*UV_BASE8
!
                 UV_BASE1 = UV_BASE1*UV_STEP8
                 UV_BASE2 = UV_BASE2*UV_STEP8
                 UV_BASE3 = UV_BASE3*UV_STEP8
                 UV_BASE4 = UV_BASE4*UV_STEP8
                 UV_BASE5 = UV_BASE5*UV_STEP8
                 UV_BASE6 = UV_BASE6*UV_STEP8
                 UV_BASE7 = UV_BASE7*UV_STEP8
                 UV_BASE8 = UV_BASE8*UV_STEP8
!
                 DRF_MOM = DRF_MOM + UV(49,J2,J1)*UV_BASE1
                 DRF_MOM = DRF_MOM + UV(50,J2,J1)*UV_BASE2
                 DRF_MOM = DRF_MOM + UV(51,J2,J1)*UV_BASE3
                 DRF_MOM = DRF_MOM + UV(52,J2,J1)*UV_BASE4
                 DRF_MOM = DRF_MOM + UV(53,J2,J1)*UV_BASE5
                 DRF_MOM = DRF_MOM + UV(54,J2,J1)*UV_BASE6
                 DRF_MOM = DRF_MOM + UV(55,J2,J1)*UV_BASE7
                 DRF_MOM = DRF_MOM + UV(56,J2,J1)*UV_BASE8
!
                 UV_BASE1 = UV_BASE1*UV_STEP8
                 UV_BASE2 = UV_BASE2*UV_STEP8
                 UV_BASE3 = UV_BASE3*UV_STEP8
                 UV_BASE4 = UV_BASE4*UV_STEP8
                 UV_BASE5 = UV_BASE5*UV_STEP8
                 UV_BASE6 = UV_BASE6*UV_STEP8
                 UV_BASE7 = UV_BASE7*UV_STEP8
                 UV_BASE8 = UV_BASE8*UV_STEP8
!
                 DRF_MOM = DRF_MOM + UV(57,J2,J1)*UV_BASE1
                 DRF_MOM = DRF_MOM + UV(58,J2,J1)*UV_BASE2
                 DRF_MOM = DRF_MOM + UV(59,J2,J1)*UV_BASE3
                 DRF_MOM = DRF_MOM + UV(60,J2,J1)*UV_BASE4
                 DRF_MOM = DRF_MOM + UV(61,J2,J1)*UV_BASE5
                 DRF_MOM = DRF_MOM + UV(62,J2,J1)*UV_BASE6
                 DRF_MOM = DRF_MOM + UV(63,J2,J1)*UV_BASE7
                 DRF_MOM = DRF_MOM + UV(64,J2,J1)*UV_BASE8
!
                 UV_BASE1 = UV_BASE1*UV_STEP8
                 UV_BASE2 = UV_BASE2*UV_STEP8
                 UV_BASE3 = UV_BASE3*UV_STEP8
                 UV_BASE4 = UV_BASE4*UV_STEP8
                 UV_BASE5 = UV_BASE5*UV_STEP8
                 UV_BASE6 = UV_BASE6*UV_STEP8
                 UV_BASE7 = UV_BASE7*UV_STEP8
                 UV_BASE8 = UV_BASE8*UV_STEP8
!
                 DRF_MOM = DRF_MOM + UV(65,J2,J1)*UV_BASE1
                 DRF_MOM = DRF_MOM + UV(66,J2,J1)*UV_BASE2
                 DRF_MOM = DRF_MOM + UV(67,J2,J1)*UV_BASE3
                 DRF_MOM = DRF_MOM + UV(68,J2,J1)*UV_BASE4
                 DRF_MOM = DRF_MOM + UV(69,J2,J1)*UV_BASE5
                 DRF_MOM = DRF_MOM + UV(70,J2,J1)*UV_BASE6
                 DRF_MOM = DRF_MOM + UV(71,J2,J1)*UV_BASE7
                 DRF_MOM = DRF_MOM + UV(72,J2,J1)*UV_BASE8
!
                 UV_BASE1 = UV_BASE1*UV_STEP8
                 UV_BASE2 = UV_BASE2*UV_STEP8
                 UV_BASE3 = UV_BASE3*UV_STEP8
                 UV_BASE4 = UV_BASE4*UV_STEP8
                 UV_BASE5 = UV_BASE5*UV_STEP8
                 UV_BASE6 = UV_BASE6*UV_STEP8
                 UV_BASE7 = UV_BASE7*UV_STEP8
                 UV_BASE8 = UV_BASE8*UV_STEP8
!
                 DRF_MOM = DRF_MOM + UV(73,J2,J1)*UV_BASE1
                 DRF_MOM = DRF_MOM + UV(74,J2,J1)*UV_BASE2
                 DRF_MOM = DRF_MOM + UV(75,J2,J1)*UV_BASE3
                 DRF_MOM = DRF_MOM + UV(76,J2,J1)*UV_BASE4
                 DRF_MOM = DRF_MOM + UV(77,J2,J1)*UV_BASE5
                 DRF_MOM = DRF_MOM + UV(78,J2,J1)*UV_BASE6
                 DRF_MOM = DRF_MOM + UV(79,J2,J1)*UV_BASE7
                 DRF_MOM = DRF_MOM + UV(80,J2,J1)*UV_BASE8
!
                 UV_BASE1 = UV_BASE1*UV_STEP8
                 UV_BASE2 = UV_BASE2*UV_STEP8
                 UV_BASE3 = UV_BASE3*UV_STEP8
                 UV_BASE4 = UV_BASE4*UV_STEP8
                 UV_BASE5 = UV_BASE5*UV_STEP8
                 UV_BASE6 = UV_BASE6*UV_STEP8
                 UV_BASE7 = UV_BASE7*UV_STEP8
                 UV_BASE8 = UV_BASE8*UV_STEP8
!
                 DRF_MOM = DRF_MOM + UV(81,J2,J1)*UV_BASE1
                 DRF_MOM = DRF_MOM + UV(82,J2,J1)*UV_BASE2
                 DRF_MOM = DRF_MOM + UV(83,J2,J1)*UV_BASE3
                 DRF_MOM = DRF_MOM + UV(84,J2,J1)*UV_BASE4
                 DRF_MOM = DRF_MOM + UV(85,J2,J1)*UV_BASE5
                 DRF_MOM = DRF_MOM + UV(86,J2,J1)*UV_BASE6
                 DRF_MOM = DRF_MOM + UV(87,J2,J1)*UV_BASE7
                 DRF_MOM = DRF_MOM + UV(88,J2,J1)*UV_BASE8
!
                 UV_BASE1 = UV_BASE1*UV_STEP8
                 UV_BASE2 = UV_BASE2*UV_STEP8
                 UV_BASE3 = UV_BASE3*UV_STEP8
                 UV_BASE4 = UV_BASE4*UV_STEP8
                 UV_BASE5 = UV_BASE5*UV_STEP8
                 UV_BASE6 = UV_BASE6*UV_STEP8
                 UV_BASE7 = UV_BASE7*UV_STEP8
                 UV_BASE8 = UV_BASE8*UV_STEP8
!
                 DRF_MOM = DRF_MOM + UV(89,J2,J1)*UV_BASE1
                 DRF_MOM = DRF_MOM + UV(90,J2,J1)*UV_BASE2
                 DRF_MOM = DRF_MOM + UV(91,J2,J1)*UV_BASE3
                 DRF_MOM = DRF_MOM + UV(92,J2,J1)*UV_BASE4
                 DRF_MOM = DRF_MOM + UV(93,J2,J1)*UV_BASE5
                 DRF_MOM = DRF_MOM + UV(94,J2,J1)*UV_BASE6
                 DRF_MOM = DRF_MOM + UV(95,J2,J1)*UV_BASE7
                 DRF_MOM = DRF_MOM + UV(96,J2,J1)*UV_BASE8
!
                 UV_BASE1 = UV_BASE1*UV_STEP8
                 UV_BASE2 = UV_BASE2*UV_STEP8
                 UV_BASE3 = UV_BASE3*UV_STEP8
                 UV_BASE4 = UV_BASE4*UV_STEP8
                 UV_BASE5 = UV_BASE5*UV_STEP8
                 UV_BASE6 = UV_BASE6*UV_STEP8
                 UV_BASE7 = UV_BASE7*UV_STEP8
                 UV_BASE8 = UV_BASE8*UV_STEP8
!
                 DRF_MOM = DRF_MOM + UV(97,J2,J1)*UV_BASE1
                 DRF_MOM = DRF_MOM + UV(98,J2,J1)*UV_BASE2
                 DRF_MOM = DRF_MOM + UV(99,J2,J1)*UV_BASE3
                 DRF_MOM = DRF_MOM + UV(100,J2,J1)*UV_BASE4
                 DRF_MOM = DRF_MOM + UV(101,J2,J1)*UV_BASE5
                 DRF_MOM = DRF_MOM + UV(102,J2,J1)*UV_BASE6
                 DRF_MOM = DRF_MOM + UV(103,J2,J1)*UV_BASE7
                 DRF_MOM = DRF_MOM + UV(104,J2,J1)*UV_BASE8
!
                 UV_BASE1 = UV_BASE1*UV_STEP8
                 UV_BASE2 = UV_BASE2*UV_STEP8
                 UV_BASE3 = UV_BASE3*UV_STEP8
                 UV_BASE4 = UV_BASE4*UV_STEP8
                 UV_BASE5 = UV_BASE5*UV_STEP8
                 UV_BASE6 = UV_BASE6*UV_STEP8
                 UV_BASE7 = UV_BASE7*UV_STEP8
                 UV_BASE8 = UV_BASE8*UV_STEP8
!
                 DRF_MOM = DRF_MOM + UV(105,J2,J1)*UV_BASE1
                 DRF_MOM = DRF_MOM + UV(106,J2,J1)*UV_BASE2
                 DRF_MOM = DRF_MOM + UV(107,J2,J1)*UV_BASE3
                 DRF_MOM = DRF_MOM + UV(108,J2,J1)*UV_BASE4
                 DRF_MOM = DRF_MOM + UV(109,J2,J1)*UV_BASE5
                 DRF_MOM = DRF_MOM + UV(110,J2,J1)*UV_BASE6
                 DRF_MOM = DRF_MOM + UV(111,J2,J1)*UV_BASE7
                 DRF_MOM = DRF_MOM + UV(112,J2,J1)*UV_BASE8
!
                 UV_BASE1 = UV_BASE1*UV_STEP8
                 UV_BASE2 = UV_BASE2*UV_STEP8
                 UV_BASE3 = UV_BASE3*UV_STEP8
                 UV_BASE4 = UV_BASE4*UV_STEP8
                 UV_BASE5 = UV_BASE5*UV_STEP8
                 UV_BASE6 = UV_BASE6*UV_STEP8
                 UV_BASE7 = UV_BASE7*UV_STEP8
                 UV_BASE8 = UV_BASE8*UV_STEP8
!
                 DRF_MOM = DRF_MOM + UV(113,J2,J1)*UV_BASE1
                 DRF_MOM = DRF_MOM + UV(114,J2,J1)*UV_BASE2
                 DRF_MOM = DRF_MOM + UV(115,J2,J1)*UV_BASE3
                 DRF_MOM = DRF_MOM + UV(116,J2,J1)*UV_BASE4
                 DRF_MOM = DRF_MOM + UV(117,J2,J1)*UV_BASE5
                 DRF_MOM = DRF_MOM + UV(118,J2,J1)*UV_BASE6
                 DRF_MOM = DRF_MOM + UV(119,J2,J1)*UV_BASE7
                 DRF_MOM = DRF_MOM + UV(120,J2,J1)*UV_BASE8
!
                 UV_BASE1 = UV_BASE1*UV_STEP8
                 UV_BASE2 = UV_BASE2*UV_STEP8
                 UV_BASE3 = UV_BASE3*UV_STEP8
                 UV_BASE4 = UV_BASE4*UV_STEP8
                 UV_BASE5 = UV_BASE5*UV_STEP8
                 UV_BASE6 = UV_BASE6*UV_STEP8
                 UV_BASE7 = UV_BASE7*UV_STEP8
                 UV_BASE8 = UV_BASE8*UV_STEP8
!
                 DRF_MOM = DRF_MOM + UV(121,J2,J1)*UV_BASE1
                 DRF_MOM = DRF_MOM + UV(122,J2,J1)*UV_BASE2
                 DRF_MOM = DRF_MOM + UV(123,J2,J1)*UV_BASE3
                 DRF_MOM = DRF_MOM + UV(124,J2,J1)*UV_BASE4
                 DRF_MOM = DRF_MOM + UV(125,J2,J1)*UV_BASE5
                 DRF_MOM = DRF_MOM + UV(126,J2,J1)*UV_BASE6
                 DRF_MOM = DRF_MOM + UV(127,J2,J1)*UV_BASE7
                 DRF_MOM = DRF_MOM + UV(128,J2,J1)*UV_BASE8
 620          CONTINUE 
              DRF = DRF + DRF_MOM*WEI(J1)
 610       CONTINUE 
         ELSE 
!
! -------- Non optimized version
!
           DRF_MOM = CMPLX ( 0.0, 0.0 )
           DO 710 J1=1,LTIM
              IF ( WEI(J1) < WEI_THR ) GOTO 710
              TIM_VAL = ((J1-1)*AP_LEN - TIME_FRT)
              DRF_MOM  = CMPLX ( 0.0, 0.0 )
              DO 720 J2=1,LFRQ
                  DO 730 J3=1,LCHN
                     PHS_MOD = PI2*FREQ_ARR(J3,J2)*RAT_MOD*TIM_VAL + &
     &                         PI2*(FREQ_ARR(J3,J2)-FREQ_REF)*GRD_MOD
                     DRF_MOM = DRF_MOM + UV(J3,J2,J1)* &
     &                             CMPLX ( COS(PHS_MOD), SIN(PHS_MOD) )
  730             CONTINUE 
  720         CONTINUE 
!@              DRF = DRF + DRF_MOM*WEI(J1)
  710      CONTINUE 
      END IF
!
      RETURN
      END  SUBROUTINE PIMA_UV_DRF  !#!#
