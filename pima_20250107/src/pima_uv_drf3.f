      SUBROUTINE PIMA_UV_DRF3 ( MODE, FL_OPT, LTIM, LCHN, LFRQ, &
     &                          WEI, TIM_ARR, WEI_THR, TIME_FRT, PH_RAT, &
     &                          GR_DEL, GR_RAT, PH_ACC, FREQ_ARR, &
     &                          FREQ_REF, CFRQ_REF, UV, DRF_BAND, DRF_IF )
! ************************************************************************
! *                                                                      *
! *   Routine  PIMA_UV_DRF3 applies phase delay rate, group delay,       *
! *   group delay rate or phase acceleration to input visibilities,      *
! *   performs time and frequency averaging and computes visibilities    *
! *   summed over time and frequency within each IF and within a band.   *
! *                                                                      *
! *   PIMA_UV_DRF3 perfroms summation without division by the number     *
! *   of samples.                                                        *
! *                                                                      *
! *   This routine is highly optimized buy in-lining the code.           *
! *   A non-optimized version of the algorithm can be found at the       *
! *   bottom of the program.                                             *
! *                                                                      *
! * __________________________ Input parameters: _______________________ *
! *                                                                      *
! *      MODE ( INTEGER*4 ) -- Processing mode. Support-red modes:       *
! *                            PIMA__GRAT -- group delay rate is         *
! *                                          accounted, but phase        *
! *                                          acceleration is ignored.    *
! *                            PIMA__ACC  -- phase acceleration is       *
! *                                          accounted, but group delay  *
! *                                          is ignored.                 *
! *    FL_OPT ( LOGICAL*1 ) -- Optimization flag. Normally should be     *
! *                            .TRUE. When .TRUE., then the frequency    *
! *                            table *must be* equidistant. Value        *
! *                            .FALSE. causes to fall back to the        *
! *                            non-optimized version.                    *
! *      LTIM ( INTEGER*4 ) -- The number of accumulation periods over   *
! *                            time.                                     *
! *      LCHN ( INTEGER*4 ) -- The number of frequency channels in a     *
! *                            given intermediate frequency IF.          *
! *      LFRQ ( INTEGER*4 ) -- The number of intermediate frequencies    *
! *                            (IFs).                                    *
! *       WEI ( REAL*4    ) -- Array of weights for each accumulation    *
! *                            period in range [0, 1]. Dimension: LTIM.  *
! *   TIM_ARR ( REAL*8    ) -- Array of time epochs counted from the     *
! *                            1-st epoch (1st epoch is zero).           *
! *                            Units: sec.                               *
! *   WEI_THR ( REAL*8    ) -- Weight threshold: observations with       *
! *                            weights that are less than WEI_THR are    *
! *                            discarded.                                *
! *  TIME_FRT ( REAL*8    ) -- Fringe reference time from the nominal    *
! *                            start of the observation.                 *
! *    PH_RAT ( REAL*8    ) -- Phase delay rate.                         *
! *    GR_DEL ( REAL*8    ) -- Group delay.                              *
! *    GR_RAT ( REAL*8    ) -- Group delay rate.                         *
! *    PH_ACC ( REAL*8    ) -- Phase delay acceleration. Units 1/s**2    *
! *  FREQ_ARR ( REAL*8    ) -- Frequency array. Dimension:               *
! *                            (LCHN,LFRQ).                              *
! *  FREQ_REF ( REAL*8    ) -- Reference frequency.                      *
! *  CFRQ_REF ( REAL*8    ) -- Auxiliary array for speed up of           *
! *                            computation:                              *
! *                            PI2*( FREQ_ARR(k,IFRQ) - FREQ_REF ).      *
! *                            Dimension: (LCHN,LFRQ).                   *
! *        UV ( COMPLEX*8 ) -- Complex visibilities. Dimension:          *
! *                            (LCHN,LFRQ,LTIM).                         *
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
! *  DRF_BAND ( COMPLEX*8 ) -- Complex visibility summed up within       *
! *                            a band over spectral channels, IFs, and   *
! *                            time after phase rotation due to phase    *
! *                            delay rate, group delay, group delay      *
! *                            rate or phase acceleration.               *
! *    DRF_IF ( COMPLEX*8 ) -- Array of complex visibilities summed up   *
! *                            within each IF over spectral channels and *
! *                            time after phase rotation due to phase    *
! *                            delay rate, group delay, group delay rate *
! *                            or phase acceleration.                    *
! *                                                                      *
! *  ### 14-AUG-2009  PIMA_UV_DRF3  v5.0 (c) L. Petrov  31-MAR-2020 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'pima.i'
      INTEGER*4  MODE, LTIM, LCHN, LFRQ
      LOGICAL*1  FL_OPT
      REAL*4     WEI(LTIM)
      REAL*8     TIME_FRT, TIM_ARR(LTIM), WEI_THR, PH_RAT, GR_DEL, GR_RAT, &
     &           PH_ACC, FREQ_ARR(LCHN,LFRQ), FREQ_REF, CFRQ_REF(LCHN,LFRQ)
      COMPLEX*8  UV(LCHN,LFRQ,LTIM), DRF_BAND, DRF_IF(LFRQ)
      REAL*8     CFRQ_VAL, TIM_VAL, PHS_STEP
      REAL*4     PHS_MOD
      COMPLEX*8  UV_BASE,  UV_STEP
      COMPLEX*8, ALLOCATABLE :: UV_ARR(:,:)
      COMPLEX*8  UV_BASE1, UV_BASE2, UV_BASE3, UV_BASE4, &
     &           UV_BASE5, UV_BASE6, UV_BASE7, UV_BASE8, &
     &           UV_STEP1, UV_STEP2, UV_STEP4, UV_STEP8
      INTEGER*4  J1, J2, J3, NBLO
!
      DRF_BAND = CMPLX ( 0.0, 0.0 )
      CFRQ_VAL = PI2*FREQ_REF
      CALL NOUT_R4 ( 2*LFRQ, DRF_IF )
!
      IF ( FL_OPT .AND. LCHN == 4 ) THEN
!
! -------- Optimized version of the algorithm for 4 channels
!
           DO 110 J1=1,LTIM
              IF ( WEI(J1) < WEI_THR ) GOTO 110
              TIM_VAL = TIM_ARR(J1) - TIME_FRT
!
              IF ( MODE == PIMA__GRAT ) THEN
                   PHS_STEP = (GR_DEL + GR_RAT*TIM_VAL)* &
     &                        (CFRQ_REF(2,1)-CFRQ_REF(1,1))
                 ELSE IF ( MODE == PIMA__ACC ) THEN
                   PHS_STEP = GR_DEL*(CFRQ_REF(2,1)-CFRQ_REF(1,1))
              END IF
              UV_STEP  = DCMPLX ( DCOS(PHS_STEP), DSIN(PHS_STEP) )
              UV_STEP1 = UV_STEP
              UV_STEP2 = UV_STEP1*UV_STEP1 
!
              DO 120 J2=1,LFRQ
                 IF ( MODE == PIMA__GRAT ) THEN
                      PHS_MOD =    PH_RAT*TIM_VAL*CFRQ_VAL &
     &                          + (GR_DEL + GR_RAT*TIM_VAL)*CFRQ_REF(1,J2)
                    ELSE IF ( MODE == PIMA__ACC ) THEN
                      PHS_MOD =    CFRQ_VAL*(PH_RAT + PH_ACC*TIM_VAL/2.0D0)*TIM_VAL &
     &                          +  GR_DEL*CFRQ_REF(1,J2)
                 END IF
                 UV_BASE  = CMPLX ( COS(PHS_MOD), SIN(PHS_MOD) )
!
                 UV_BASE1 = UV_BASE*WEI(J1)
                 UV_BASE2 = UV_BASE1*UV_STEP1
                 UV_BASE3 = UV_BASE1*UV_STEP2
                 UV_BASE4 = UV_BASE2*UV_STEP2
!
                 DRF_IF(J2) =   DRF_IF(J2) &
     &                         + UV(1,J2,J1)*UV_BASE1 &
     &                         + UV(2,J2,J1)*UV_BASE2 &
     &                         + UV(3,J2,J1)*UV_BASE3 &
     &                         + UV(4,J2,J1)*UV_BASE4
!
 120          CONTINUE 
 110       CONTINUE 
         ELSE IF ( FL_OPT .AND. LCHN == 8 ) THEN
!
! -------- Optimized version of the algorithm for 8 channels
!
           DO 210 J1=1,LTIM
              IF ( WEI(J1) < WEI_THR ) GOTO 210
              TIM_VAL = TIM_ARR(J1) - TIME_FRT
!
              IF ( MODE == PIMA__GRAT ) THEN
                   PHS_STEP = (GR_DEL + GR_RAT*TIM_VAL)* &
     &                        (CFRQ_REF(2,1)-CFRQ_REF(1,1))
                 ELSE IF ( MODE == PIMA__ACC ) THEN
                   PHS_STEP = GR_DEL*(CFRQ_REF(2,1)-CFRQ_REF(1,1))
              END IF
              UV_STEP  = DCMPLX ( DCOS(PHS_STEP), DSIN(PHS_STEP) )
              UV_STEP1 = UV_STEP
              UV_STEP2 = UV_STEP1*UV_STEP1 
              UV_STEP4 = UV_STEP2*UV_STEP2
!
              DO 220 J2=1,LFRQ
                 IF ( MODE == PIMA__GRAT ) THEN
                      PHS_MOD =    PH_RAT*TIM_VAL*CFRQ_VAL &
     &                          + (GR_DEL + GR_RAT*TIM_VAL)*CFRQ_REF(1,J2)
                    ELSE IF ( MODE == PIMA__ACC ) THEN
                      PHS_MOD =    CFRQ_VAL*(PH_RAT + PH_ACC*TIM_VAL/2.0D0)*TIM_VAL &
     &                          +  GR_DEL*CFRQ_REF(1,J2)
                 END IF
                 UV_BASE  = CMPLX ( COS(PHS_MOD), SIN(PHS_MOD) )
!
                 UV_BASE1 = UV_BASE*WEI(J1)
                 UV_BASE2 = UV_BASE1*UV_STEP1
                 UV_BASE3 = UV_BASE1*UV_STEP2
                 UV_BASE4 = UV_BASE2*UV_STEP2
!
                 UV_BASE5 = UV_BASE1*UV_STEP4
                 UV_BASE6 = UV_BASE2*UV_STEP4
                 UV_BASE7 = UV_BASE3*UV_STEP4
                 UV_BASE8 = UV_BASE4*UV_STEP4
!
                 DRF_IF(J2) =   DRF_IF(J2) &
     &                         + UV(1,J2,J1)*UV_BASE1 &
     &                         + UV(2,J2,J1)*UV_BASE2 &
     &                         + UV(3,J2,J1)*UV_BASE3 &
     &                         + UV(4,J2,J1)*UV_BASE4 &
     &                         + UV(5,J2,J1)*UV_BASE5 &
     &                         + UV(6,J2,J1)*UV_BASE6 &
     &                         + UV(7,J2,J1)*UV_BASE7 &
     &                         + UV(8,J2,J1)*UV_BASE8
!
 220          CONTINUE 
 210       CONTINUE 
         ELSE IF ( FL_OPT .AND. LCHN == 16 ) THEN
!
! -------- Optimized version of the algorithm for 16 channels
!
           DO 310 J1=1,LTIM
              IF ( WEI(J1) < WEI_THR ) GOTO 310
              TIM_VAL = TIM_ARR(J1) - TIME_FRT
!
              IF ( MODE == PIMA__GRAT ) THEN
                   PHS_STEP = (GR_DEL + GR_RAT*TIM_VAL)* &
     &                        (CFRQ_REF(2,1)-CFRQ_REF(1,1))
                 ELSE IF ( MODE == PIMA__ACC ) THEN
                   PHS_STEP = GR_DEL*(CFRQ_REF(2,1)-CFRQ_REF(1,1))
              END IF
              UV_STEP  = DCMPLX ( DCOS(PHS_STEP), DSIN(PHS_STEP) )
              UV_STEP1 = UV_STEP
              UV_STEP2 = UV_STEP1*UV_STEP1 
              UV_STEP4 = UV_STEP2*UV_STEP2
              UV_STEP8 = UV_STEP4*UV_STEP4
!
              DO 320 J2=1,LFRQ
                 IF ( MODE == PIMA__GRAT ) THEN
                      PHS_MOD =    PH_RAT*TIM_VAL*CFRQ_VAL &
     &                          + (GR_DEL + GR_RAT*TIM_VAL)*CFRQ_REF(1,J2)
                    ELSE IF ( MODE == PIMA__ACC ) THEN
                      PHS_MOD =    CFRQ_VAL*(PH_RAT + PH_ACC*TIM_VAL/2.0D0)*TIM_VAL &
     &                          +  GR_DEL*CFRQ_REF(1,J2)
                 END IF
                 UV_BASE  = CMPLX ( COS(PHS_MOD), SIN(PHS_MOD) )
!
                 UV_BASE1 = UV_BASE*WEI(J1)
                 UV_BASE2 = UV_BASE1*UV_STEP1
                 UV_BASE3 = UV_BASE1*UV_STEP2
                 UV_BASE4 = UV_BASE2*UV_STEP2
                 UV_BASE5 = UV_BASE1*UV_STEP4
                 UV_BASE6 = UV_BASE2*UV_STEP4
                 UV_BASE7 = UV_BASE3*UV_STEP4
                 UV_BASE8 = UV_BASE4*UV_STEP4
!
                 DRF_IF(J2) =   DRF_IF(J2) &
     &                         + UV(1,J2,J1)*UV_BASE1 &
     &                         + UV(2,J2,J1)*UV_BASE2 &
     &                         + UV(3,J2,J1)*UV_BASE3 &
     &                         + UV(4,J2,J1)*UV_BASE4 &
     &                         + UV(5,J2,J1)*UV_BASE5 &
     &                         + UV(6,J2,J1)*UV_BASE6 &
     &                         + UV(7,J2,J1)*UV_BASE7 &
     &                         + UV(8,J2,J1)*UV_BASE8
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
                 DRF_IF(J2) =   DRF_IF(J2) &
     &                         + UV(9,J2,J1)*UV_BASE1  &
     &                         + UV(10,J2,J1)*UV_BASE2 &
     &                         + UV(11,J2,J1)*UV_BASE3 &
     &                         + UV(12,J2,J1)*UV_BASE4 &
     &                         + UV(13,J2,J1)*UV_BASE5 &
     &                         + UV(14,J2,J1)*UV_BASE6 &
     &                         + UV(15,J2,J1)*UV_BASE7 &
     &                         + UV(16,J2,J1)*UV_BASE8
 320          CONTINUE 
 310       CONTINUE 
         ELSE IF ( FL_OPT .AND. LCHN == 32 ) THEN
!
! -------- Optimized version of the algorithm for 32 channels
!
           DO 410 J1=1,LTIM
              IF ( WEI(J1) < WEI_THR ) GOTO 410
              TIM_VAL = TIM_ARR(J1) - TIME_FRT
!
!
              IF ( MODE == PIMA__GRAT ) THEN
                   PHS_STEP = (GR_DEL + GR_RAT*TIM_VAL)* &
     &                        (CFRQ_REF(2,1)-CFRQ_REF(1,1))
                 ELSE IF ( MODE == PIMA__ACC ) THEN
                   PHS_STEP = GR_DEL*(CFRQ_REF(2,1)-CFRQ_REF(1,1))
              END IF
              UV_STEP  = DCMPLX ( DCOS(PHS_STEP), DSIN(PHS_STEP) )
              UV_STEP1 = UV_STEP
              UV_STEP2 = UV_STEP1*UV_STEP1 
              UV_STEP4 = UV_STEP2*UV_STEP2
              UV_STEP8 = UV_STEP4*UV_STEP4
!
              DO 420 J2=1,LFRQ
                 IF ( MODE == PIMA__GRAT ) THEN
                      PHS_MOD =    PH_RAT*TIM_VAL*CFRQ_VAL &
     &                          + (GR_DEL + GR_RAT*TIM_VAL)*CFRQ_REF(1,J2)
                    ELSE IF ( MODE == PIMA__ACC ) THEN
                      PHS_MOD =    CFRQ_VAL*(PH_RAT + PH_ACC*TIM_VAL/2.0D0)*TIM_VAL &
     &                          +  GR_DEL*CFRQ_REF(1,J2)
                 END IF
                 UV_BASE  = CMPLX ( COS(PHS_MOD), SIN(PHS_MOD) )
!
                 UV_BASE1 = UV_BASE*WEI(J1)
                 UV_BASE2 = UV_BASE1*UV_STEP1
                 UV_BASE3 = UV_BASE1*UV_STEP2
                 UV_BASE4 = UV_BASE2*UV_STEP2
                 UV_BASE5 = UV_BASE1*UV_STEP4
                 UV_BASE6 = UV_BASE2*UV_STEP4
                 UV_BASE7 = UV_BASE3*UV_STEP4
                 UV_BASE8 = UV_BASE4*UV_STEP4
!
                 DRF_IF(J2) =   DRF_IF(J2) &
     &                         + UV(1,J2,J1)*UV_BASE1 &
     &                         + UV(2,J2,J1)*UV_BASE2 &
     &                         + UV(3,J2,J1)*UV_BASE3 &
     &                         + UV(4,J2,J1)*UV_BASE4 &
     &                         + UV(5,J2,J1)*UV_BASE5 &
     &                         + UV(6,J2,J1)*UV_BASE6 &
     &                         + UV(7,J2,J1)*UV_BASE7 &
     &                         + UV(8,J2,J1)*UV_BASE8
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
                 DRF_IF(J2) =   DRF_IF(J2) &
     &                         + UV(9,J2,J1)*UV_BASE1  &
     &                         + UV(10,J2,J1)*UV_BASE2 &
     &                         + UV(11,J2,J1)*UV_BASE3 &
     &                         + UV(12,J2,J1)*UV_BASE4 &
     &                         + UV(13,J2,J1)*UV_BASE5 &
     &                         + UV(14,J2,J1)*UV_BASE6 &
     &                         + UV(15,J2,J1)*UV_BASE7 &
     &                         + UV(16,J2,J1)*UV_BASE8
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
                 DRF_IF(J2) =   DRF_IF(J2) &
     &                         + UV(17,J2,J1)*UV_BASE1 &
     &                         + UV(18,J2,J1)*UV_BASE2 &
     &                         + UV(19,J2,J1)*UV_BASE3 &
     &                         + UV(20,J2,J1)*UV_BASE4 &
     &                         + UV(21,J2,J1)*UV_BASE5 &
     &                         + UV(22,J2,J1)*UV_BASE6 &
     &                         + UV(23,J2,J1)*UV_BASE7 &
     &                         + UV(24,J2,J1)*UV_BASE8
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
                 DRF_IF(J2) =   DRF_IF(J2) &
     &                         + UV(25,J2,J1)*UV_BASE1 &
     &                         + UV(26,J2,J1)*UV_BASE2 &
     &                         + UV(27,J2,J1)*UV_BASE3 &
     &                         + UV(28,J2,J1)*UV_BASE4 &
     &                         + UV(29,J2,J1)*UV_BASE5 &
     &                         + UV(30,J2,J1)*UV_BASE6 &
     &                         + UV(31,J2,J1)*UV_BASE7 &
     &                         + UV(32,J2,J1)*UV_BASE8
 420          CONTINUE 
 410       CONTINUE 
         ELSE IF ( FL_OPT .AND. LCHN == 64 ) THEN
!
! --------- Optimized version of the algorithm for 64 channels
!   
           DO 510 J1=1,LTIM
              IF ( WEI(J1) < WEI_THR ) GOTO 510
              TIM_VAL = TIM_ARR(J1) - TIME_FRT
!
              IF ( MODE == PIMA__GRAT ) THEN
                   PHS_STEP = (GR_DEL + GR_RAT*TIM_VAL)* &
     &                        (CFRQ_REF(2,1)-CFRQ_REF(1,1))
                 ELSE IF ( MODE == PIMA__ACC ) THEN
                   PHS_STEP = GR_DEL*(CFRQ_REF(2,1)-CFRQ_REF(1,1))
              END IF
              UV_STEP  = DCMPLX ( DCOS(PHS_STEP), DSIN(PHS_STEP) )
              UV_STEP1 = UV_STEP
              UV_STEP2 = UV_STEP1*UV_STEP1 
              UV_STEP4 = UV_STEP2*UV_STEP2
              UV_STEP8 = UV_STEP4*UV_STEP4
!
              DO 520 J2=1,LFRQ
                 IF ( MODE == PIMA__GRAT ) THEN
                      PHS_MOD =    PH_RAT*TIM_VAL*CFRQ_VAL &
     &                          + (GR_DEL + GR_RAT*TIM_VAL)*CFRQ_REF(1,J2)
                    ELSE IF ( MODE == PIMA__ACC ) THEN
                      PHS_MOD =    CFRQ_VAL*(PH_RAT + PH_ACC*TIM_VAL/2.0D0)*TIM_VAL &
     &                          +  GR_DEL*CFRQ_REF(1,J2)
                 END IF
                 UV_BASE  = CMPLX ( COS(PHS_MOD), SIN(PHS_MOD) )
!
                 UV_BASE1 = UV_BASE*WEI(J1)
                 UV_BASE2 = UV_BASE1*UV_STEP1
                 UV_BASE3 = UV_BASE1*UV_STEP2
                 UV_BASE4 = UV_BASE2*UV_STEP2
                 UV_BASE5 = UV_BASE1*UV_STEP4
                 UV_BASE6 = UV_BASE2*UV_STEP4
                 UV_BASE7 = UV_BASE3*UV_STEP4
                 UV_BASE8 = UV_BASE4*UV_STEP4
!
                 DRF_IF(J2) =   DRF_IF(J2) &
     &                         + UV(1,J2,J1)*UV_BASE1 &
     &                         + UV(2,J2,J1)*UV_BASE2 &
     &                         + UV(3,J2,J1)*UV_BASE3 &
     &                         + UV(4,J2,J1)*UV_BASE4 &
     &                         + UV(5,J2,J1)*UV_BASE5 &
     &                         + UV(6,J2,J1)*UV_BASE6 &
     &                         + UV(7,J2,J1)*UV_BASE7 &
     &                         + UV(8,J2,J1)*UV_BASE8
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
                 DRF_IF(J2) =   DRF_IF(J2) &
     &                         + UV(9,J2,J1)*UV_BASE1  &
     &                         + UV(10,J2,J1)*UV_BASE2 &
     &                         + UV(11,J2,J1)*UV_BASE3 &
     &                         + UV(12,J2,J1)*UV_BASE4 &
     &                         + UV(13,J2,J1)*UV_BASE5 &
     &                         + UV(14,J2,J1)*UV_BASE6 &
     &                         + UV(15,J2,J1)*UV_BASE7 &
     &                         + UV(16,J2,J1)*UV_BASE8
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
                 DRF_IF(J2) =   DRF_IF(J2) &
     &                         + UV(17,J2,J1)*UV_BASE1 &
     &                         + UV(18,J2,J1)*UV_BASE2 &
     &                         + UV(19,J2,J1)*UV_BASE3 &
     &                         + UV(20,J2,J1)*UV_BASE4 &
     &                         + UV(21,J2,J1)*UV_BASE5 &
     &                         + UV(22,J2,J1)*UV_BASE6 &
     &                         + UV(23,J2,J1)*UV_BASE7 &
     &                         + UV(24,J2,J1)*UV_BASE8
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
                 DRF_IF(J2) =   DRF_IF(J2) &
     &                         + UV(25,J2,J1)*UV_BASE1 &
     &                         + UV(26,J2,J1)*UV_BASE2 &
     &                         + UV(27,J2,J1)*UV_BASE3 &
     &                         + UV(28,J2,J1)*UV_BASE4 &
     &                         + UV(29,J2,J1)*UV_BASE5 &
     &                         + UV(30,J2,J1)*UV_BASE6 &
     &                         + UV(31,J2,J1)*UV_BASE7 &
     &                         + UV(32,J2,J1)*UV_BASE8 
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
                 DRF_IF(J2) =   DRF_IF(J2) &
     &                         + UV(33,J2,J1)*UV_BASE1 &
     &                         + UV(34,J2,J1)*UV_BASE2 &
     &                         + UV(35,J2,J1)*UV_BASE3 &
     &                         + UV(36,J2,J1)*UV_BASE4 &
     &                         + UV(37,J2,J1)*UV_BASE5 &
     &                         + UV(38,J2,J1)*UV_BASE6 &
     &                         + UV(39,J2,J1)*UV_BASE7 &
     &                         + UV(40,J2,J1)*UV_BASE8 
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
                 DRF_IF(J2) =   DRF_IF(J2) &
     &                         + UV(41,J2,J1)*UV_BASE1 &
     &                         + UV(42,J2,J1)*UV_BASE2 &
     &                         + UV(43,J2,J1)*UV_BASE3 &
     &                         + UV(44,J2,J1)*UV_BASE4 &
     &                         + UV(45,J2,J1)*UV_BASE5 &
     &                         + UV(46,J2,J1)*UV_BASE6 &
     &                         + UV(47,J2,J1)*UV_BASE7 &
     &                         + UV(48,J2,J1)*UV_BASE8 
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
                 DRF_IF(J2) =   DRF_IF(J2) &
     &                         + UV(49,J2,J1)*UV_BASE1 &
     &                         + UV(50,J2,J1)*UV_BASE2 &
     &                         + UV(51,J2,J1)*UV_BASE3 &
     &                         + UV(52,J2,J1)*UV_BASE4 &
     &                         + UV(53,J2,J1)*UV_BASE5 &
     &                         + UV(54,J2,J1)*UV_BASE6 &
     &                         + UV(55,J2,J1)*UV_BASE7 &
     &                         + UV(56,J2,J1)*UV_BASE8 
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
                 DRF_IF(J2) =   DRF_IF(J2) &
     &                         + UV(57,J2,J1)*UV_BASE1 &
     &                         + UV(58,J2,J1)*UV_BASE2 &
     &                         + UV(59,J2,J1)*UV_BASE3 &
     &                         + UV(60,J2,J1)*UV_BASE4 &
     &                         + UV(61,J2,J1)*UV_BASE5 &
     &                         + UV(62,J2,J1)*UV_BASE6 &
     &                         + UV(63,J2,J1)*UV_BASE7 &
     &                         + UV(64,J2,J1)*UV_BASE8
 520          CONTINUE 
 510       CONTINUE 
         ELSE IF ( FL_OPT .AND. LCHN == 128 ) THEN
!
! -------- Optimized version of the algorithm for 128 channels
!   
           DO 610 J1=1,LTIM
              IF ( WEI(J1) < WEI_THR ) GOTO 610
              TIM_VAL = TIM_ARR(J1) - TIME_FRT
!
              IF ( MODE == PIMA__GRAT ) THEN
                   PHS_STEP = (GR_DEL + GR_RAT*TIM_VAL)* &
     &                        (CFRQ_REF(2,1)-CFRQ_REF(1,1))
                 ELSE IF ( MODE == PIMA__ACC ) THEN
                   PHS_STEP = GR_DEL*(CFRQ_REF(2,1)-CFRQ_REF(1,1))
              END IF
              UV_STEP  = DCMPLX ( DCOS(PHS_STEP), DSIN(PHS_STEP) )
              UV_STEP1 = UV_STEP
              UV_STEP2  = UV_STEP1*UV_STEP1 
              UV_STEP4  = UV_STEP2*UV_STEP2
              UV_STEP8  = UV_STEP4*UV_STEP4
!
              DO 620 J2=1,LFRQ
                 IF ( MODE == PIMA__GRAT ) THEN
                      PHS_MOD =    PH_RAT*TIM_VAL*CFRQ_VAL &
     &                          + (GR_DEL + GR_RAT*TIM_VAL)*CFRQ_REF(1,J2)
                    ELSE IF ( MODE == PIMA__ACC ) THEN
                      PHS_MOD =    CFRQ_VAL*(PH_RAT + PH_ACC*TIM_VAL/2.0D0)*TIM_VAL &
     &                          +  GR_DEL*CFRQ_REF(1,J2)
                 END IF
                 UV_BASE  = CMPLX ( COS(PHS_MOD), SIN(PHS_MOD) )
!
                 UV_BASE1 = UV_BASE*WEI(J1)
                 UV_BASE2 = UV_BASE1*UV_STEP1
                 UV_BASE3 = UV_BASE1*UV_STEP2
                 UV_BASE4 = UV_BASE2*UV_STEP2
                 UV_BASE5 = UV_BASE1*UV_STEP4
                 UV_BASE6 = UV_BASE2*UV_STEP4
                 UV_BASE7 = UV_BASE3*UV_STEP4
                 UV_BASE8 = UV_BASE4*UV_STEP4
!
                 DRF_IF(J2) =   DRF_IF(J2) &
     &                         + UV(1,J2,J1)*UV_BASE1 &
     &                         + UV(2,J2,J1)*UV_BASE2 &
     &                         + UV(3,J2,J1)*UV_BASE3 &
     &                         + UV(4,J2,J1)*UV_BASE4 &
     &                         + UV(5,J2,J1)*UV_BASE5 &
     &                         + UV(6,J2,J1)*UV_BASE6 &
     &                         + UV(7,J2,J1)*UV_BASE7 &
     &                         + UV(8,J2,J1)*UV_BASE8
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
                 DRF_IF(J2) =   DRF_IF(J2) &
     &                         + UV(9,J2,J1)*UV_BASE1  &
     &                         + UV(10,J2,J1)*UV_BASE2 &
     &                         + UV(11,J2,J1)*UV_BASE3 &
     &                         + UV(12,J2,J1)*UV_BASE4 &
     &                         + UV(13,J2,J1)*UV_BASE5 &
     &                         + UV(14,J2,J1)*UV_BASE6 &
     &                         + UV(15,J2,J1)*UV_BASE7 &
     &                         + UV(16,J2,J1)*UV_BASE8
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
                 DRF_IF(J2) =   DRF_IF(J2) &
     &                         + UV(17,J2,J1)*UV_BASE1 &
     &                         + UV(18,J2,J1)*UV_BASE2 &
     &                         + UV(19,J2,J1)*UV_BASE3 &
     &                         + UV(20,J2,J1)*UV_BASE4 &
     &                         + UV(21,J2,J1)*UV_BASE5 &
     &                         + UV(22,J2,J1)*UV_BASE6 &
     &                         + UV(23,J2,J1)*UV_BASE7 &
     &                         + UV(24,J2,J1)*UV_BASE8
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
                 DRF_IF(J2) =   DRF_IF(J2) &
     &                         + UV(25,J2,J1)*UV_BASE1 &
     &                         + UV(26,J2,J1)*UV_BASE2 &
     &                         + UV(27,J2,J1)*UV_BASE3 &
     &                         + UV(28,J2,J1)*UV_BASE4 &
     &                         + UV(29,J2,J1)*UV_BASE5 &
     &                         + UV(30,J2,J1)*UV_BASE6 &
     &                         + UV(31,J2,J1)*UV_BASE7 &
     &                         + UV(32,J2,J1)*UV_BASE8
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
                 DRF_IF(J2) =   DRF_IF(J2) &
     &                         + UV(33,J2,J1)*UV_BASE1 &
     &                         + UV(34,J2,J1)*UV_BASE2 &
     &                         + UV(35,J2,J1)*UV_BASE3 &
     &                         + UV(36,J2,J1)*UV_BASE4 &
     &                         + UV(37,J2,J1)*UV_BASE5 &
     &                         + UV(38,J2,J1)*UV_BASE6 &
     &                         + UV(39,J2,J1)*UV_BASE7 &
     &                         + UV(40,J2,J1)*UV_BASE8 
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
                 DRF_IF(J2) =   DRF_IF(J2) &
     &                         + UV(41,J2,J1)*UV_BASE1 &
     &                         + UV(42,J2,J1)*UV_BASE2 &
     &                         + UV(43,J2,J1)*UV_BASE3 &
     &                         + UV(44,J2,J1)*UV_BASE4 &
     &                         + UV(45,J2,J1)*UV_BASE5 &
     &                         + UV(46,J2,J1)*UV_BASE6 &
     &                         + UV(47,J2,J1)*UV_BASE7 &
     &                         + UV(48,J2,J1)*UV_BASE8
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
                 DRF_IF(J2) =   DRF_IF(J2) &
     &                         + UV(49,J2,J1)*UV_BASE1 &
     &                         + UV(50,J2,J1)*UV_BASE2 &
     &                         + UV(51,J2,J1)*UV_BASE3 &
     &                         + UV(52,J2,J1)*UV_BASE4 &
     &                         + UV(53,J2,J1)*UV_BASE5 &
     &                         + UV(54,J2,J1)*UV_BASE6 &
     &                         + UV(55,J2,J1)*UV_BASE7 &
     &                         + UV(56,J2,J1)*UV_BASE8
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
                 DRF_IF(J2) =   DRF_IF(J2) &
     &                         + UV(57,J2,J1)*UV_BASE1 &
     &                         + UV(58,J2,J1)*UV_BASE2 &
     &                         + UV(59,J2,J1)*UV_BASE3 &
     &                         + UV(60,J2,J1)*UV_BASE4 &
     &                         + UV(61,J2,J1)*UV_BASE5 &
     &                         + UV(62,J2,J1)*UV_BASE6 &
     &                         + UV(63,J2,J1)*UV_BASE7 &
     &                         + UV(64,J2,J1)*UV_BASE8
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
                 DRF_IF(J2) =   DRF_IF(J2) &
     &                         + UV(65,J2,J1)*UV_BASE1 &
     &                         + UV(66,J2,J1)*UV_BASE2 &
     &                         + UV(67,J2,J1)*UV_BASE3 &
     &                         + UV(68,J2,J1)*UV_BASE4 &
     &                         + UV(69,J2,J1)*UV_BASE5 &
     &                         + UV(70,J2,J1)*UV_BASE6 &
     &                         + UV(71,J2,J1)*UV_BASE7 &
     &                         + UV(72,J2,J1)*UV_BASE8
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
                 DRF_IF(J2) =   DRF_IF(J2) &
     &                         + UV(73,J2,J1)*UV_BASE1 &
     &                         + UV(74,J2,J1)*UV_BASE2 &
     &                         + UV(75,J2,J1)*UV_BASE3 &
     &                         + UV(76,J2,J1)*UV_BASE4 &
     &                         + UV(77,J2,J1)*UV_BASE5 &
     &                         + UV(78,J2,J1)*UV_BASE6 &
     &                         + UV(79,J2,J1)*UV_BASE7 &
     &                         + UV(80,J2,J1)*UV_BASE8
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
                 DRF_IF(J2) =   DRF_IF(J2) &
     &                         + UV(81,J2,J1)*UV_BASE1 &
     &                         + UV(82,J2,J1)*UV_BASE2 &
     &                         + UV(83,J2,J1)*UV_BASE3 &
     &                         + UV(84,J2,J1)*UV_BASE4 &
     &                         + UV(85,J2,J1)*UV_BASE5 &
     &                         + UV(86,J2,J1)*UV_BASE6 &
     &                         + UV(87,J2,J1)*UV_BASE7 &
     &                         + UV(88,J2,J1)*UV_BASE8
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
                 DRF_IF(J2) =   DRF_IF(J2) &
     &                         + UV(89,J2,J1)*UV_BASE1 &
     &                         + UV(90,J2,J1)*UV_BASE2 &
     &                         + UV(91,J2,J1)*UV_BASE3 &
     &                         + UV(92,J2,J1)*UV_BASE4 &
     &                         + UV(93,J2,J1)*UV_BASE5 &
     &                         + UV(94,J2,J1)*UV_BASE6 &
     &                         + UV(95,J2,J1)*UV_BASE7 &
     &                         + UV(96,J2,J1)*UV_BASE8
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
                 DRF_IF(J2) =   DRF_IF(J2) &
     &                         + UV(97,J2,J1)*UV_BASE1  &
     &                         + UV(98,J2,J1)*UV_BASE2  &
     &                         + UV(99,J2,J1)*UV_BASE3  &
     &                         + UV(100,J2,J1)*UV_BASE4 &
     &                         + UV(101,J2,J1)*UV_BASE5 &
     &                         + UV(102,J2,J1)*UV_BASE6 &
     &                         + UV(103,J2,J1)*UV_BASE7 &
     &                         + UV(104,J2,J1)*UV_BASE8
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
                 DRF_IF(J2) =   DRF_IF(J2) &
     &                         + UV(105,J2,J1)*UV_BASE1 &
     &                         + UV(106,J2,J1)*UV_BASE2 &
     &                         + UV(107,J2,J1)*UV_BASE3 &
     &                         + UV(108,J2,J1)*UV_BASE4 &
     &                         + UV(109,J2,J1)*UV_BASE5 &
     &                         + UV(110,J2,J1)*UV_BASE6 &
     &                         + UV(111,J2,J1)*UV_BASE7 &
     &                         + UV(112,J2,J1)*UV_BASE8
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
                 DRF_IF(J2) =   DRF_IF(J2) &
     &                         + UV(113,J2,J1)*UV_BASE1 &
     &                         + UV(114,J2,J1)*UV_BASE2 &
     &                         + UV(115,J2,J1)*UV_BASE3 &
     &                         + UV(116,J2,J1)*UV_BASE4 &
     &                         + UV(117,J2,J1)*UV_BASE5 &
     &                         + UV(118,J2,J1)*UV_BASE6 &
     &                         + UV(119,J2,J1)*UV_BASE7 &
     &                         + UV(120,J2,J1)*UV_BASE8 
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
                 DRF_IF(J2) =   DRF_IF(J2) &
     &                         + UV(121,J2,J1)*UV_BASE1 &
     &                         + UV(122,J2,J1)*UV_BASE2 &
     &                         + UV(123,J2,J1)*UV_BASE3 &
     &                         + UV(124,J2,J1)*UV_BASE4 &
     &                         + UV(125,J2,J1)*UV_BASE5 &
     &                         + UV(126,J2,J1)*UV_BASE6 &
     &                         + UV(127,J2,J1)*UV_BASE7 &
     &                         + UV(128,J2,J1)*UV_BASE8
 620          CONTINUE 
 610       CONTINUE 
         ELSE IF ( FL_OPT .AND. LCHN == 256 ) THEN
!
! --------- Optimized version of the algorithm for 256 channels
!   
           DO 710 J1=1,LTIM
              IF ( WEI(J1) < WEI_THR ) GOTO 710
              TIM_VAL = TIM_ARR(J1) - TIME_FRT
!
              IF ( MODE == PIMA__GRAT ) THEN
                   PHS_STEP = (GR_DEL + GR_RAT*TIM_VAL)* &
     &                        (CFRQ_REF(2,1)-CFRQ_REF(1,1))
                 ELSE IF ( MODE == PIMA__ACC ) THEN
                   PHS_STEP = GR_DEL*(CFRQ_REF(2,1)-CFRQ_REF(1,1))
              END IF
              UV_STEP  = DCMPLX ( DCOS(PHS_STEP), DSIN(PHS_STEP) )
              UV_STEP1 = UV_STEP
              UV_STEP2  = UV_STEP1*UV_STEP1 
              UV_STEP4  = UV_STEP2*UV_STEP2
              UV_STEP8  = UV_STEP4*UV_STEP4
!
              DO 720 J2=1,LFRQ
                 IF ( MODE == PIMA__GRAT ) THEN
                      PHS_MOD =    PH_RAT*TIM_VAL*CFRQ_VAL &
     &                          + (GR_DEL + GR_RAT*TIM_VAL)*CFRQ_REF(1,J2)
                    ELSE IF ( MODE == PIMA__ACC ) THEN
                      PHS_MOD =    CFRQ_VAL*(PH_RAT + PH_ACC*TIM_VAL/2.0D0)*TIM_VAL &
     &                          +  GR_DEL*CFRQ_REF(1,J2)
                 END IF
                 UV_BASE  = CMPLX ( COS(PHS_MOD), SIN(PHS_MOD) )
!
                 UV_BASE1 = UV_BASE*WEI(J1)
                 UV_BASE2 = UV_BASE1*UV_STEP1
                 UV_BASE3 = UV_BASE1*UV_STEP2
                 UV_BASE4 = UV_BASE2*UV_STEP2
                 UV_BASE5 = UV_BASE1*UV_STEP4
                 UV_BASE6 = UV_BASE2*UV_STEP4
                 UV_BASE7 = UV_BASE3*UV_STEP4
                 UV_BASE8 = UV_BASE4*UV_STEP4
!
                 DRF_IF(J2) =   DRF_IF(J2) &
     &                         + UV(1,J2,J1)*UV_BASE1 &
     &                         + UV(2,J2,J1)*UV_BASE2 &
     &                         + UV(3,J2,J1)*UV_BASE3 &
     &                         + UV(4,J2,J1)*UV_BASE4 &
     &                         + UV(5,J2,J1)*UV_BASE5 &
     &                         + UV(6,J2,J1)*UV_BASE6 &
     &                         + UV(7,J2,J1)*UV_BASE7 &
     &                         + UV(8,J2,J1)*UV_BASE8
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
                 DRF_IF(J2) =   DRF_IF(J2) &
     &                         + UV(9,J2,J1)*UV_BASE1  &
     &                         + UV(10,J2,J1)*UV_BASE2 &
     &                         + UV(11,J2,J1)*UV_BASE3 &
     &                         + UV(12,J2,J1)*UV_BASE4 &
     &                         + UV(13,J2,J1)*UV_BASE5 &
     &                         + UV(14,J2,J1)*UV_BASE6 &
     &                         + UV(15,J2,J1)*UV_BASE7 &
     &                         + UV(16,J2,J1)*UV_BASE8
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
                 DRF_IF(J2) =   DRF_IF(J2) &
     &                         + UV(17,J2,J1)*UV_BASE1 &
     &                         + UV(18,J2,J1)*UV_BASE2 &
     &                         + UV(19,J2,J1)*UV_BASE3 &
     &                         + UV(20,J2,J1)*UV_BASE4 &
     &                         + UV(21,J2,J1)*UV_BASE5 &
     &                         + UV(22,J2,J1)*UV_BASE6 &
     &                         + UV(23,J2,J1)*UV_BASE7 &
     &                         + UV(24,J2,J1)*UV_BASE8
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
                 DRF_IF(J2) =   DRF_IF(J2) &
     &                         + UV(25,J2,J1)*UV_BASE1 &
     &                         + UV(26,J2,J1)*UV_BASE2 &
     &                         + UV(27,J2,J1)*UV_BASE3 &
     &                         + UV(28,J2,J1)*UV_BASE4 &
     &                         + UV(29,J2,J1)*UV_BASE5 &
     &                         + UV(30,J2,J1)*UV_BASE6 &
     &                         + UV(31,J2,J1)*UV_BASE7 &
     &                         + UV(32,J2,J1)*UV_BASE8
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
                 DRF_IF(J2) =   DRF_IF(J2) &
     &                         + UV(33,J2,J1)*UV_BASE1 &
     &                         + UV(34,J2,J1)*UV_BASE2 &
     &                         + UV(35,J2,J1)*UV_BASE3 &
     &                         + UV(36,J2,J1)*UV_BASE4 &
     &                         + UV(37,J2,J1)*UV_BASE5 &
     &                         + UV(38,J2,J1)*UV_BASE6 &
     &                         + UV(39,J2,J1)*UV_BASE7 &
     &                         + UV(40,J2,J1)*UV_BASE8 
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
                 DRF_IF(J2) =   DRF_IF(J2) &
     &                         + UV(41,J2,J1)*UV_BASE1 &
     &                         + UV(42,J2,J1)*UV_BASE2 &
     &                         + UV(43,J2,J1)*UV_BASE3 &
     &                         + UV(44,J2,J1)*UV_BASE4 &
     &                         + UV(45,J2,J1)*UV_BASE5 &
     &                         + UV(46,J2,J1)*UV_BASE6 &
     &                         + UV(47,J2,J1)*UV_BASE7 &
     &                         + UV(48,J2,J1)*UV_BASE8
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
                 DRF_IF(J2) =   DRF_IF(J2) &
     &                         + UV(49,J2,J1)*UV_BASE1 &
     &                         + UV(50,J2,J1)*UV_BASE2 &
     &                         + UV(51,J2,J1)*UV_BASE3 &
     &                         + UV(52,J2,J1)*UV_BASE4 &
     &                         + UV(53,J2,J1)*UV_BASE5 &
     &                         + UV(54,J2,J1)*UV_BASE6 &
     &                         + UV(55,J2,J1)*UV_BASE7 &
     &                         + UV(56,J2,J1)*UV_BASE8
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
                 DRF_IF(J2) =   DRF_IF(J2) &
     &                         + UV(57,J2,J1)*UV_BASE1 &
     &                         + UV(58,J2,J1)*UV_BASE2 &
     &                         + UV(59,J2,J1)*UV_BASE3 &
     &                         + UV(60,J2,J1)*UV_BASE4 &
     &                         + UV(61,J2,J1)*UV_BASE5 &
     &                         + UV(62,J2,J1)*UV_BASE6 &
     &                         + UV(63,J2,J1)*UV_BASE7 &
     &                         + UV(64,J2,J1)*UV_BASE8
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
                 DRF_IF(J2) =   DRF_IF(J2) &
     &                         + UV(65,J2,J1)*UV_BASE1 &
     &                         + UV(66,J2,J1)*UV_BASE2 &
     &                         + UV(67,J2,J1)*UV_BASE3 &
     &                         + UV(68,J2,J1)*UV_BASE4 &
     &                         + UV(69,J2,J1)*UV_BASE5 &
     &                         + UV(70,J2,J1)*UV_BASE6 &
     &                         + UV(71,J2,J1)*UV_BASE7 &
     &                         + UV(72,J2,J1)*UV_BASE8
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
                 DRF_IF(J2) =   DRF_IF(J2) &
     &                         + UV(73,J2,J1)*UV_BASE1 &
     &                         + UV(74,J2,J1)*UV_BASE2 &
     &                         + UV(75,J2,J1)*UV_BASE3 &
     &                         + UV(76,J2,J1)*UV_BASE4 &
     &                         + UV(77,J2,J1)*UV_BASE5 &
     &                         + UV(78,J2,J1)*UV_BASE6 &
     &                         + UV(79,J2,J1)*UV_BASE7 &
     &                         + UV(80,J2,J1)*UV_BASE8
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
                 DRF_IF(J2) =   DRF_IF(J2) &
     &                         + UV(81,J2,J1)*UV_BASE1 &
     &                         + UV(82,J2,J1)*UV_BASE2 &
     &                         + UV(83,J2,J1)*UV_BASE3 &
     &                         + UV(84,J2,J1)*UV_BASE4 &
     &                         + UV(85,J2,J1)*UV_BASE5 &
     &                         + UV(86,J2,J1)*UV_BASE6 &
     &                         + UV(87,J2,J1)*UV_BASE7 &
     &                         + UV(88,J2,J1)*UV_BASE8
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
                 DRF_IF(J2) =   DRF_IF(J2) &
     &                         + UV(89,J2,J1)*UV_BASE1 &
     &                         + UV(90,J2,J1)*UV_BASE2 &
     &                         + UV(91,J2,J1)*UV_BASE3 &
     &                         + UV(92,J2,J1)*UV_BASE4 &
     &                         + UV(93,J2,J1)*UV_BASE5 &
     &                         + UV(94,J2,J1)*UV_BASE6 &
     &                         + UV(95,J2,J1)*UV_BASE7 &
     &                         + UV(96,J2,J1)*UV_BASE8
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
                 DRF_IF(J2) =   DRF_IF(J2) &
     &                         + UV(97,J2,J1)*UV_BASE1  &
     &                         + UV(98,J2,J1)*UV_BASE2  &
     &                         + UV(99,J2,J1)*UV_BASE3  &
     &                         + UV(100,J2,J1)*UV_BASE4 &
     &                         + UV(101,J2,J1)*UV_BASE5 &
     &                         + UV(102,J2,J1)*UV_BASE6 &
     &                         + UV(103,J2,J1)*UV_BASE7 &
     &                         + UV(104,J2,J1)*UV_BASE8
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
                 DRF_IF(J2) =   DRF_IF(J2) &
     &                         + UV(105,J2,J1)*UV_BASE1 &
     &                         + UV(106,J2,J1)*UV_BASE2 &
     &                         + UV(107,J2,J1)*UV_BASE3 &
     &                         + UV(108,J2,J1)*UV_BASE4 &
     &                         + UV(109,J2,J1)*UV_BASE5 &
     &                         + UV(110,J2,J1)*UV_BASE6 &
     &                         + UV(111,J2,J1)*UV_BASE7 &
     &                         + UV(112,J2,J1)*UV_BASE8
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
                 DRF_IF(J2) =   DRF_IF(J2) &
     &                         + UV(113,J2,J1)*UV_BASE1 &
     &                         + UV(114,J2,J1)*UV_BASE2 &
     &                         + UV(115,J2,J1)*UV_BASE3 &
     &                         + UV(116,J2,J1)*UV_BASE4 &
     &                         + UV(117,J2,J1)*UV_BASE5 &
     &                         + UV(118,J2,J1)*UV_BASE6 &
     &                         + UV(119,J2,J1)*UV_BASE7 &
     &                         + UV(120,J2,J1)*UV_BASE8 
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
                 DRF_IF(J2) =   DRF_IF(J2) &
     &                         + UV(121,J2,J1)*UV_BASE1 &
     &                         + UV(122,J2,J1)*UV_BASE2 &
     &                         + UV(123,J2,J1)*UV_BASE3 &
     &                         + UV(124,J2,J1)*UV_BASE4 &
     &                         + UV(125,J2,J1)*UV_BASE5 &
     &                         + UV(126,J2,J1)*UV_BASE6 &
     &                         + UV(127,J2,J1)*UV_BASE7 &
     &                         + UV(128,J2,J1)*UV_BASE8
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
                 DRF_IF(J2) =   DRF_IF(J2) &
     &                         + UV(129,J2,J1)*UV_BASE1 &
     &                         + UV(130,J2,J1)*UV_BASE2 &
     &                         + UV(131,J2,J1)*UV_BASE3 &
     &                         + UV(132,J2,J1)*UV_BASE4 &
     &                         + UV(133,J2,J1)*UV_BASE5 &
     &                         + UV(134,J2,J1)*UV_BASE6 &
     &                         + UV(135,J2,J1)*UV_BASE7 &
     &                         + UV(136,J2,J1)*UV_BASE8
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
                 DRF_IF(J2) =   DRF_IF(J2) &
     &                         + UV(137,J2,J1)*UV_BASE1 &
     &                         + UV(138,J2,J1)*UV_BASE2 &
     &                         + UV(139,J2,J1)*UV_BASE3 &
     &                         + UV(140,J2,J1)*UV_BASE4 &
     &                         + UV(141,J2,J1)*UV_BASE5 &
     &                         + UV(142,J2,J1)*UV_BASE6 &
     &                         + UV(143,J2,J1)*UV_BASE7 &
     &                         + UV(144,J2,J1)*UV_BASE8
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
                 DRF_IF(J2) =   DRF_IF(J2) &
     &                         + UV(145,J2,J1)*UV_BASE1 &
     &                         + UV(146,J2,J1)*UV_BASE2 &
     &                         + UV(147,J2,J1)*UV_BASE3 &
     &                         + UV(148,J2,J1)*UV_BASE4 &
     &                         + UV(149,J2,J1)*UV_BASE5 &
     &                         + UV(150,J2,J1)*UV_BASE6 &
     &                         + UV(151,J2,J1)*UV_BASE7 &
     &                         + UV(152,J2,J1)*UV_BASE8
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
                 DRF_IF(J2) =   DRF_IF(J2) &
     &                         + UV(153,J2,J1)*UV_BASE1 &
     &                         + UV(154,J2,J1)*UV_BASE2 &
     &                         + UV(155,J2,J1)*UV_BASE3 &
     &                         + UV(156,J2,J1)*UV_BASE4 &
     &                         + UV(157,J2,J1)*UV_BASE5 &
     &                         + UV(158,J2,J1)*UV_BASE6 &
     &                         + UV(159,J2,J1)*UV_BASE7 &
     &                         + UV(160,J2,J1)*UV_BASE8
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
                 DRF_IF(J2) =   DRF_IF(J2) &
     &                         + UV(161,J2,J1)*UV_BASE1 &
     &                         + UV(162,J2,J1)*UV_BASE2 &
     &                         + UV(163,J2,J1)*UV_BASE3 &
     &                         + UV(164,J2,J1)*UV_BASE4 &
     &                         + UV(165,J2,J1)*UV_BASE5 &
     &                         + UV(166,J2,J1)*UV_BASE6 &
     &                         + UV(167,J2,J1)*UV_BASE7 &
     &                         + UV(168,J2,J1)*UV_BASE8
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
                 DRF_IF(J2) =   DRF_IF(J2) &
     &                         + UV(169,J2,J1)*UV_BASE1 &
     &                         + UV(170,J2,J1)*UV_BASE2 &
     &                         + UV(171,J2,J1)*UV_BASE3 &
     &                         + UV(172,J2,J1)*UV_BASE4 &
     &                         + UV(173,J2,J1)*UV_BASE5 &
     &                         + UV(174,J2,J1)*UV_BASE6 &
     &                         + UV(175,J2,J1)*UV_BASE7 &
     &                         + UV(176,J2,J1)*UV_BASE8
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
                 DRF_IF(J2) =   DRF_IF(J2) &
     &                         + UV(177,J2,J1)*UV_BASE1 &
     &                         + UV(178,J2,J1)*UV_BASE2 &
     &                         + UV(179,J2,J1)*UV_BASE3 &
     &                         + UV(180,J2,J1)*UV_BASE4 &
     &                         + UV(181,J2,J1)*UV_BASE5 &
     &                         + UV(182,J2,J1)*UV_BASE6 &
     &                         + UV(183,J2,J1)*UV_BASE7 &
     &                         + UV(184,J2,J1)*UV_BASE8
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
                 DRF_IF(J2) =   DRF_IF(J2) &
     &                         + UV(185,J2,J1)*UV_BASE1 &
     &                         + UV(186,J2,J1)*UV_BASE2 &
     &                         + UV(187,J2,J1)*UV_BASE3 &
     &                         + UV(188,J2,J1)*UV_BASE4 &
     &                         + UV(189,J2,J1)*UV_BASE5 &
     &                         + UV(190,J2,J1)*UV_BASE6 &
     &                         + UV(191,J2,J1)*UV_BASE7 &
     &                         + UV(192,J2,J1)*UV_BASE8
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
                 DRF_IF(J2) =   DRF_IF(J2) &
     &                         + UV(193,J2,J1)*UV_BASE1 &
     &                         + UV(194,J2,J1)*UV_BASE2 &
     &                         + UV(195,J2,J1)*UV_BASE3 &
     &                         + UV(196,J2,J1)*UV_BASE4 &
     &                         + UV(197,J2,J1)*UV_BASE5 &
     &                         + UV(198,J2,J1)*UV_BASE6 &
     &                         + UV(199,J2,J1)*UV_BASE7 &
     &                         + UV(200,J2,J1)*UV_BASE8
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
                 DRF_IF(J2) =   DRF_IF(J2) &
     &                         + UV(201,J2,J1)*UV_BASE1 &
     &                         + UV(202,J2,J1)*UV_BASE2 &
     &                         + UV(203,J2,J1)*UV_BASE3 &
     &                         + UV(204,J2,J1)*UV_BASE4 &
     &                         + UV(205,J2,J1)*UV_BASE5 &
     &                         + UV(206,J2,J1)*UV_BASE6 &
     &                         + UV(207,J2,J1)*UV_BASE7 &
     &                         + UV(208,J2,J1)*UV_BASE8
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
                 DRF_IF(J2) =   DRF_IF(J2) &
     &                         + UV(209,J2,J1)*UV_BASE1 &
     &                         + UV(210,J2,J1)*UV_BASE2 &
     &                         + UV(211,J2,J1)*UV_BASE3 &
     &                         + UV(212,J2,J1)*UV_BASE4 &
     &                         + UV(213,J2,J1)*UV_BASE5 &
     &                         + UV(214,J2,J1)*UV_BASE6 &
     &                         + UV(215,J2,J1)*UV_BASE7 &
     &                         + UV(216,J2,J1)*UV_BASE8
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
                 DRF_IF(J2) =   DRF_IF(J2) &
     &                         + UV(217,J2,J1)*UV_BASE1 &
     &                         + UV(218,J2,J1)*UV_BASE2 &
     &                         + UV(219,J2,J1)*UV_BASE3 &
     &                         + UV(220,J2,J1)*UV_BASE4 &
     &                         + UV(221,J2,J1)*UV_BASE5 &
     &                         + UV(222,J2,J1)*UV_BASE6 &
     &                         + UV(223,J2,J1)*UV_BASE7 &
     &                         + UV(224,J2,J1)*UV_BASE8
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
                 DRF_IF(J2) =   DRF_IF(J2) &
     &                         + UV(225,J2,J1)*UV_BASE1 &
     &                         + UV(226,J2,J1)*UV_BASE2 &
     &                         + UV(227,J2,J1)*UV_BASE3 &
     &                         + UV(228,J2,J1)*UV_BASE4 &
     &                         + UV(229,J2,J1)*UV_BASE5 &
     &                         + UV(230,J2,J1)*UV_BASE6 &
     &                         + UV(231,J2,J1)*UV_BASE7 &
     &                         + UV(232,J2,J1)*UV_BASE8
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
                 DRF_IF(J2) =   DRF_IF(J2) &
     &                         + UV(233,J2,J1)*UV_BASE1 &
     &                         + UV(234,J2,J1)*UV_BASE2 &
     &                         + UV(235,J2,J1)*UV_BASE3 &
     &                         + UV(236,J2,J1)*UV_BASE4 &
     &                         + UV(237,J2,J1)*UV_BASE5 &
     &                         + UV(238,J2,J1)*UV_BASE6 &
     &                         + UV(239,J2,J1)*UV_BASE7 &
     &                         + UV(240,J2,J1)*UV_BASE8
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
                 DRF_IF(J2) =   DRF_IF(J2) &
     &                         + UV(241,J2,J1)*UV_BASE1 &
     &                         + UV(242,J2,J1)*UV_BASE2 &
     &                         + UV(243,J2,J1)*UV_BASE3 &
     &                         + UV(244,J2,J1)*UV_BASE4 &
     &                         + UV(245,J2,J1)*UV_BASE5 &
     &                         + UV(246,J2,J1)*UV_BASE6 &
     &                         + UV(247,J2,J1)*UV_BASE7 &
     &                         + UV(248,J2,J1)*UV_BASE8
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
                 DRF_IF(J2) =   DRF_IF(J2) &
     &                         + UV(249,J2,J1)*UV_BASE1 &
     &                         + UV(250,J2,J1)*UV_BASE2 &
     &                         + UV(251,J2,J1)*UV_BASE3 &
     &                         + UV(252,J2,J1)*UV_BASE4 &
     &                         + UV(253,J2,J1)*UV_BASE5 &
     &                         + UV(254,J2,J1)*UV_BASE6 &
     &                         + UV(255,J2,J1)*UV_BASE7 &
     &                         + UV(256,J2,J1)*UV_BASE8
 720          CONTINUE
 710       CONTINUE 
         ELSE IF ( FL_OPT .AND. LCHN > 256  .AND. MOD(LCHN,256) == 0 ) THEN
!
! -------- Optimized version of the algorithm for > 256 channels
!   
           NBLO = LCHN/256
           ALLOCATE ( UV_ARR(256,LFRQ) )
           DO 810 J1=1,LTIM
              IF ( WEI(J1) < WEI_THR ) GOTO 810
              TIM_VAL = TIM_ARR(J1) - TIME_FRT
!
              IF ( MODE == PIMA__GRAT ) THEN
                   PHS_STEP = (GR_DEL + GR_RAT*TIM_VAL)* &
     &                        (CFRQ_REF(2,1)-CFRQ_REF(1,1))
                 ELSE IF ( MODE == PIMA__ACC ) THEN
                   PHS_STEP = GR_DEL*(CFRQ_REF(2,1)-CFRQ_REF(1,1))
              END IF
              UV_STEP  = DCMPLX ( DCOS(PHS_STEP), DSIN(PHS_STEP) )
              UV_STEP1 = UV_STEP
              UV_STEP2 = UV_STEP1*UV_STEP1 
              UV_STEP4 = UV_STEP2*UV_STEP2
              UV_STEP8 = UV_STEP4*UV_STEP4
!
              DO 820 J2=1,NBLO
                 UV_ARR(1:256,1:LFRQ) = UV(1+(J2-1)*256:J2*256,1:LFRQ,J1)
                 DO 830 J3=1,LFRQ
                    IF ( MODE == PIMA__GRAT ) THEN
                         PHS_MOD =    PH_RAT*TIM_VAL*CFRQ_VAL &
     &                             + (GR_DEL + GR_RAT*TIM_VAL)*CFRQ_REF(1+(J2-1)*256,J3)
                       ELSE IF ( MODE == PIMA__ACC ) THEN
                         PHS_MOD =    CFRQ_VAL*(PH_RAT + PH_ACC*TIM_VAL/2.0D0)*TIM_VAL &
     &                             +  GR_DEL*CFRQ_REF(1+(J2-1)*256,J3)
                    END IF
!
                    UV_BASE  = CMPLX ( COS(PHS_MOD), SIN(PHS_MOD) )
!
                    UV_BASE1 = UV_BASE*WEI(J1)
                    UV_BASE2 = UV_BASE1*UV_STEP1
                    UV_BASE3 = UV_BASE1*UV_STEP2
                    UV_BASE4 = UV_BASE2*UV_STEP2
                    UV_BASE5 = UV_BASE1*UV_STEP4
                    UV_BASE6 = UV_BASE2*UV_STEP4
                    UV_BASE7 = UV_BASE3*UV_STEP4
                    UV_BASE8 = UV_BASE4*UV_STEP4
!
                    DRF_IF(J3) =   DRF_IF(J3) &
     &                            + UV_ARR(1,J3)*UV_BASE1 &
     &                            + UV_ARR(2,J3)*UV_BASE2 &
     &                            + UV_ARR(3,J3)*UV_BASE3 &
     &                            + UV_ARR(4,J3)*UV_BASE4 &
     &                            + UV_ARR(5,J3)*UV_BASE5 &
     &                            + UV_ARR(6,J3)*UV_BASE6 &
     &                            + UV_ARR(7,J3)*UV_BASE7 &
     &                            + UV_ARR(8,J3)*UV_BASE8
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
                    DRF_IF(J3) =   DRF_IF(J3) &
     &                            + UV_ARR(9,J3)*UV_BASE1  &
     &                            + UV_ARR(10,J3)*UV_BASE2 &
     &                            + UV_ARR(11,J3)*UV_BASE3 &
     &                            + UV_ARR(12,J3)*UV_BASE4 &
     &                            + UV_ARR(13,J3)*UV_BASE5 &
     &                            + UV_ARR(14,J3)*UV_BASE6 &
     &                            + UV_ARR(15,J3)*UV_BASE7 &
     &                            + UV_ARR(16,J3)*UV_BASE8
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
                    DRF_IF(J3) =   DRF_IF(J3) &
     &                            + UV_ARR(17,J3)*UV_BASE1 &
     &                            + UV_ARR(18,J3)*UV_BASE2 &
     &                            + UV_ARR(19,J3)*UV_BASE3 &
     &                            + UV_ARR(20,J3)*UV_BASE4 &
     &                            + UV_ARR(21,J3)*UV_BASE5 &
     &                            + UV_ARR(22,J3)*UV_BASE6 &
     &                            + UV_ARR(23,J3)*UV_BASE7 &
     &                            + UV_ARR(24,J3)*UV_BASE8
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
                    DRF_IF(J3) =   DRF_IF(J3) &
     &                            + UV_ARR(25,J3)*UV_BASE1 &
     &                            + UV_ARR(26,J3)*UV_BASE2 &
     &                            + UV_ARR(27,J3)*UV_BASE3 &
     &                            + UV_ARR(28,J3)*UV_BASE4 &
     &                            + UV_ARR(29,J3)*UV_BASE5 &
     &                            + UV_ARR(30,J3)*UV_BASE6 &
     &                            + UV_ARR(31,J3)*UV_BASE7 &
     &                            + UV_ARR(32,J3)*UV_BASE8
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
                    DRF_IF(J3) =   DRF_IF(J3) &
     &                            + UV_ARR(33,J3)*UV_BASE1 &
     &                            + UV_ARR(34,J3)*UV_BASE2 &
     &                            + UV_ARR(35,J3)*UV_BASE3 &
     &                            + UV_ARR(36,J3)*UV_BASE4 &
     &                            + UV_ARR(37,J3)*UV_BASE5 &
     &                            + UV_ARR(38,J3)*UV_BASE6 &
     &                            + UV_ARR(39,J3)*UV_BASE7 &
     &                            + UV_ARR(40,J3)*UV_BASE8 
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
                    DRF_IF(J3) =   DRF_IF(J3) &
     &                            + UV_ARR(41,J3)*UV_BASE1 &
     &                            + UV_ARR(42,J3)*UV_BASE2 &
     &                            + UV_ARR(43,J3)*UV_BASE3 &
     &                            + UV_ARR(44,J3)*UV_BASE4 &
     &                            + UV_ARR(45,J3)*UV_BASE5 &
     &                            + UV_ARR(46,J3)*UV_BASE6 &
     &                            + UV_ARR(47,J3)*UV_BASE7 &
     &                            + UV_ARR(48,J3)*UV_BASE8
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
                    DRF_IF(J3) =   DRF_IF(J3) &
     &                            + UV_ARR(49,J3)*UV_BASE1 &
     &                            + UV_ARR(50,J3)*UV_BASE2 &
     &                            + UV_ARR(51,J3)*UV_BASE3 &
     &                            + UV_ARR(52,J3)*UV_BASE4 &
     &                            + UV_ARR(53,J3)*UV_BASE5 &
     &                            + UV_ARR(54,J3)*UV_BASE6 &
     &                            + UV_ARR(55,J3)*UV_BASE7 &
     &                            + UV_ARR(56,J3)*UV_BASE8
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
                    DRF_IF(J3) =   DRF_IF(J3) &
     &                            + UV_ARR(57,J3)*UV_BASE1 &
     &                            + UV_ARR(58,J3)*UV_BASE2 &
     &                            + UV_ARR(59,J3)*UV_BASE3 &
     &                            + UV_ARR(60,J3)*UV_BASE4 &
     &                            + UV_ARR(61,J3)*UV_BASE5 &
     &                            + UV_ARR(62,J3)*UV_BASE6 &
     &                            + UV_ARR(63,J3)*UV_BASE7 &
     &                            + UV_ARR(64,J3)*UV_BASE8
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
                    DRF_IF(J3) =   DRF_IF(J3) &
     &                            + UV_ARR(65,J3)*UV_BASE1 &
     &                            + UV_ARR(66,J3)*UV_BASE2 &
     &                            + UV_ARR(67,J3)*UV_BASE3 &
     &                            + UV_ARR(68,J3)*UV_BASE4 &
     &                            + UV_ARR(69,J3)*UV_BASE5 &
     &                            + UV_ARR(70,J3)*UV_BASE6 &
     &                            + UV_ARR(71,J3)*UV_BASE7 &
     &                            + UV_ARR(72,J3)*UV_BASE8
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
                    DRF_IF(J3) =   DRF_IF(J3) &
     &                            + UV_ARR(73,J3)*UV_BASE1 &
     &                            + UV_ARR(74,J3)*UV_BASE2 &
     &                            + UV_ARR(75,J3)*UV_BASE3 &
     &                            + UV_ARR(76,J3)*UV_BASE4 &
     &                            + UV_ARR(77,J3)*UV_BASE5 &
     &                            + UV_ARR(78,J3)*UV_BASE6 &
     &                            + UV_ARR(79,J3)*UV_BASE7 &
     &                            + UV_ARR(80,J3)*UV_BASE8
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
                    DRF_IF(J3) =   DRF_IF(J3) &
     &                            + UV_ARR(81,J3)*UV_BASE1 &
     &                            + UV_ARR(82,J3)*UV_BASE2 &
     &                            + UV_ARR(83,J3)*UV_BASE3 &
     &                            + UV_ARR(84,J3)*UV_BASE4 &
     &                            + UV_ARR(85,J3)*UV_BASE5 &
     &                            + UV_ARR(86,J3)*UV_BASE6 &
     &                            + UV_ARR(87,J3)*UV_BASE7 &
     &                            + UV_ARR(88,J3)*UV_BASE8
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
                    DRF_IF(J3) =   DRF_IF(J3) &
     &                            + UV_ARR(89,J3)*UV_BASE1 &
     &                            + UV_ARR(90,J3)*UV_BASE2 &
     &                            + UV_ARR(91,J3)*UV_BASE3 &
     &                            + UV_ARR(92,J3)*UV_BASE4 &
     &                            + UV_ARR(93,J3)*UV_BASE5 &
     &                            + UV_ARR(94,J3)*UV_BASE6 &
     &                            + UV_ARR(95,J3)*UV_BASE7 &
     &                            + UV_ARR(96,J3)*UV_BASE8
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
                    DRF_IF(J3) =   DRF_IF(J3) &
     &                            + UV_ARR(97,J3)*UV_BASE1  &
     &                            + UV_ARR(98,J3)*UV_BASE2  &
     &                            + UV_ARR(99,J3)*UV_BASE3  &
     &                            + UV_ARR(100,J3)*UV_BASE4 &
     &                            + UV_ARR(101,J3)*UV_BASE5 &
     &                            + UV_ARR(102,J3)*UV_BASE6 &
     &                            + UV_ARR(103,J3)*UV_BASE7 &
     &                            + UV_ARR(104,J3)*UV_BASE8
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
                    DRF_IF(J3) =   DRF_IF(J3) &
     &                            + UV_ARR(105,J3)*UV_BASE1 &
     &                            + UV_ARR(106,J3)*UV_BASE2 &
     &                            + UV_ARR(107,J3)*UV_BASE3 &
     &                            + UV_ARR(108,J3)*UV_BASE4 &
     &                            + UV_ARR(109,J3)*UV_BASE5 &
     &                            + UV_ARR(110,J3)*UV_BASE6 &
     &                            + UV_ARR(111,J3)*UV_BASE7 &
     &                            + UV_ARR(112,J3)*UV_BASE8
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
                    DRF_IF(J3) =   DRF_IF(J3) &
     &                            + UV_ARR(113,J3)*UV_BASE1 &
     &                            + UV_ARR(114,J3)*UV_BASE2 &
     &                            + UV_ARR(115,J3)*UV_BASE3 &
     &                            + UV_ARR(116,J3)*UV_BASE4 &
     &                            + UV_ARR(117,J3)*UV_BASE5 &
     &                            + UV_ARR(118,J3)*UV_BASE6 &
     &                            + UV_ARR(119,J3)*UV_BASE7 &
     &                            + UV_ARR(120,J3)*UV_BASE8 
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
                    DRF_IF(J3) =   DRF_IF(J3) &
     &                               + UV_ARR(121,J3)*UV_BASE1 &
     &                               + UV_ARR(122,J3)*UV_BASE2 &
     &                               + UV_ARR(123,J3)*UV_BASE3 &
     &                               + UV_ARR(124,J3)*UV_BASE4 &
     &                               + UV_ARR(125,J3)*UV_BASE5 &
     &                               + UV_ARR(126,J3)*UV_BASE6 &
     &                               + UV_ARR(127,J3)*UV_BASE7 &
     &                               + UV_ARR(128,J3)*UV_BASE8
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
                    DRF_IF(J3) =   DRF_IF(J3) &
     &                            + UV_ARR(129,J3)*UV_BASE1 &
     &                            + UV_ARR(130,J3)*UV_BASE2 &
     &                            + UV_ARR(131,J3)*UV_BASE3 &
     &                            + UV_ARR(132,J3)*UV_BASE4 &
     &                            + UV_ARR(133,J3)*UV_BASE5 &
     &                            + UV_ARR(134,J3)*UV_BASE6 &
     &                            + UV_ARR(135,J3)*UV_BASE7 &
     &                            + UV_ARR(136,J3)*UV_BASE8
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
                    DRF_IF(J3) =   DRF_IF(J3) &
     &                            + UV_ARR(137,J3)*UV_BASE1 &
     &                            + UV_ARR(138,J3)*UV_BASE2 &
     &                            + UV_ARR(139,J3)*UV_BASE3 &
     &                            + UV_ARR(140,J3)*UV_BASE4 &
     &                            + UV_ARR(141,J3)*UV_BASE5 &
     &                            + UV_ARR(142,J3)*UV_BASE6 &
     &                            + UV_ARR(143,J3)*UV_BASE7 &
     &                            + UV_ARR(144,J3)*UV_BASE8
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
                    DRF_IF(J3) =   DRF_IF(J3) &
     &                            + UV_ARR(145,J3)*UV_BASE1 &
     &                            + UV_ARR(146,J3)*UV_BASE2 &
     &                            + UV_ARR(147,J3)*UV_BASE3 &
     &                            + UV_ARR(148,J3)*UV_BASE4 &
     &                            + UV_ARR(149,J3)*UV_BASE5 &
     &                            + UV_ARR(150,J3)*UV_BASE6 &
     &                            + UV_ARR(151,J3)*UV_BASE7 &
     &                            + UV_ARR(152,J3)*UV_BASE8
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
                    DRF_IF(J3) =   DRF_IF(J3) &
     &                            + UV_ARR(153,J3)*UV_BASE1 &
     &                            + UV_ARR(154,J3)*UV_BASE2 &
     &                            + UV_ARR(155,J3)*UV_BASE3 &
     &                            + UV_ARR(156,J3)*UV_BASE4 &
     &                            + UV_ARR(157,J3)*UV_BASE5 &
     &                            + UV_ARR(158,J3)*UV_BASE6 &
     &                            + UV_ARR(159,J3)*UV_BASE7 &
     &                            + UV_ARR(160,J3)*UV_BASE8
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
                    DRF_IF(J3) =   DRF_IF(J3) &
     &                            + UV_ARR(161,J3)*UV_BASE1 &
     &                            + UV_ARR(162,J3)*UV_BASE2 &
     &                            + UV_ARR(163,J3)*UV_BASE3 &
     &                            + UV_ARR(164,J3)*UV_BASE4 &
     &                            + UV_ARR(165,J3)*UV_BASE5 &
     &                            + UV_ARR(166,J3)*UV_BASE6 &
     &                            + UV_ARR(167,J3)*UV_BASE7 &
     &                            + UV_ARR(168,J3)*UV_BASE8
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
                    DRF_IF(J3) =   DRF_IF(J3) &
     &                            + UV_ARR(169,J3)*UV_BASE1 &
     &                            + UV_ARR(170,J3)*UV_BASE2 &
     &                            + UV_ARR(171,J3)*UV_BASE3 &
     &                            + UV_ARR(172,J3)*UV_BASE4 &
     &                            + UV_ARR(173,J3)*UV_BASE5 &
     &                            + UV_ARR(174,J3)*UV_BASE6 &
     &                            + UV_ARR(175,J3)*UV_BASE7 &
     &                            + UV_ARR(176,J3)*UV_BASE8
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
                    DRF_IF(J3) =   DRF_IF(J3) &
     &                            + UV_ARR(177,J3)*UV_BASE1 &
     &                            + UV_ARR(178,J3)*UV_BASE2 &
     &                            + UV_ARR(179,J3)*UV_BASE3 &
     &                            + UV_ARR(180,J3)*UV_BASE4 &
     &                            + UV_ARR(181,J3)*UV_BASE5 &
     &                            + UV_ARR(182,J3)*UV_BASE6 &
     &                            + UV_ARR(183,J3)*UV_BASE7 &
     &                            + UV_ARR(184,J3)*UV_BASE8
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
                    DRF_IF(J3) =   DRF_IF(J3) &
     &                            + UV_ARR(185,J3)*UV_BASE1 &
     &                            + UV_ARR(186,J3)*UV_BASE2 &
     &                            + UV_ARR(187,J3)*UV_BASE3 &
     &                            + UV_ARR(188,J3)*UV_BASE4 &
     &                            + UV_ARR(189,J3)*UV_BASE5 &
     &                            + UV_ARR(190,J3)*UV_BASE6 &
     &                            + UV_ARR(191,J3)*UV_BASE7 &
     &                            + UV_ARR(192,J3)*UV_BASE8
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
                    DRF_IF(J3) =   DRF_IF(J3) &
     &                            + UV_ARR(193,J3)*UV_BASE1 &
     &                            + UV_ARR(194,J3)*UV_BASE2 &
     &                            + UV_ARR(195,J3)*UV_BASE3 &
     &                            + UV_ARR(196,J3)*UV_BASE4 &
     &                            + UV_ARR(197,J3)*UV_BASE5 &
     &                            + UV_ARR(198,J3)*UV_BASE6 &
     &                            + UV_ARR(199,J3)*UV_BASE7 &
     &                            + UV_ARR(200,J3)*UV_BASE8
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
                    DRF_IF(J3) =   DRF_IF(J3) &
     &                            + UV_ARR(201,J3)*UV_BASE1 &
     &                            + UV_ARR(202,J3)*UV_BASE2 &
     &                            + UV_ARR(203,J3)*UV_BASE3 &
     &                            + UV_ARR(204,J3)*UV_BASE4 &
     &                            + UV_ARR(205,J3)*UV_BASE5 &
     &                            + UV_ARR(206,J3)*UV_BASE6 &
     &                            + UV_ARR(207,J3)*UV_BASE7 &
     &                            + UV_ARR(208,J3)*UV_BASE8
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
                    DRF_IF(J3) =   DRF_IF(J3) &
     &                            + UV_ARR(209,J3)*UV_BASE1 &
     &                            + UV_ARR(210,J3)*UV_BASE2 &
     &                            + UV_ARR(211,J3)*UV_BASE3 &
     &                            + UV_ARR(212,J3)*UV_BASE4 &
     &                            + UV_ARR(213,J3)*UV_BASE5 &
     &                            + UV_ARR(214,J3)*UV_BASE6 &
     &                            + UV_ARR(215,J3)*UV_BASE7 &
     &                            + UV_ARR(216,J3)*UV_BASE8
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
                    DRF_IF(J3) =   DRF_IF(J3) &
     &                            + UV_ARR(217,J3)*UV_BASE1 &
     &                            + UV_ARR(218,J3)*UV_BASE2 &
     &                            + UV_ARR(219,J3)*UV_BASE3 &
     &                            + UV_ARR(220,J3)*UV_BASE4 &
     &                            + UV_ARR(221,J3)*UV_BASE5 &
     &                            + UV_ARR(222,J3)*UV_BASE6 &
     &                            + UV_ARR(223,J3)*UV_BASE7 &
     &                            + UV_ARR(224,J3)*UV_BASE8
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
                    DRF_IF(J3) =   DRF_IF(J3) &
     &                            + UV_ARR(225,J3)*UV_BASE1 &
     &                            + UV_ARR(226,J3)*UV_BASE2 &
     &                            + UV_ARR(227,J3)*UV_BASE3 &
     &                            + UV_ARR(228,J3)*UV_BASE4 &
     &                            + UV_ARR(229,J3)*UV_BASE5 &
     &                            + UV_ARR(230,J3)*UV_BASE6 &
     &                            + UV_ARR(231,J3)*UV_BASE7 &
     &                            + UV_ARR(232,J3)*UV_BASE8
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
                    DRF_IF(J3) =   DRF_IF(J3) &
     &                            + UV_ARR(233,J3)*UV_BASE1 &
     &                            + UV_ARR(234,J3)*UV_BASE2 &
     &                            + UV_ARR(235,J3)*UV_BASE3 &
     &                            + UV_ARR(236,J3)*UV_BASE4 &
     &                            + UV_ARR(237,J3)*UV_BASE5 &
     &                            + UV_ARR(238,J3)*UV_BASE6 &
     &                            + UV_ARR(239,J3)*UV_BASE7 &
     &                            + UV_ARR(240,J3)*UV_BASE8
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
                    DRF_IF(J3) =   DRF_IF(J3) &
     &                            + UV_ARR(241,J3)*UV_BASE1 &
     &                            + UV_ARR(242,J3)*UV_BASE2 &
     &                            + UV_ARR(243,J3)*UV_BASE3 &
     &                            + UV_ARR(244,J3)*UV_BASE4 &
     &                            + UV_ARR(245,J3)*UV_BASE5 &
     &                            + UV_ARR(246,J3)*UV_BASE6 &
     &                            + UV_ARR(247,J3)*UV_BASE7 &
     &                            + UV_ARR(248,J3)*UV_BASE8
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
                    DRF_IF(J3) =   DRF_IF(J3) &
     &                            + UV_ARR(249,J3)*UV_BASE1 &
     &                            + UV_ARR(250,J3)*UV_BASE2 &
     &                            + UV_ARR(251,J3)*UV_BASE3 &
     &                            + UV_ARR(252,J3)*UV_BASE4 &
     &                            + UV_ARR(253,J3)*UV_BASE5 &
     &                            + UV_ARR(254,J3)*UV_BASE6 &
     &                            + UV_ARR(255,J3)*UV_BASE7 &
     &                            + UV(256,J3,J1)*UV_BASE8
 830             CONTINUE
 820          CONTINUE
 810       CONTINUE 
           DEALLOCATE ( UV_ARR )
         ELSE 
!
! -------- Non optimized version
!
           DRF_IF = 0.0
           DO 910 J1=1,LTIM
              IF ( WEI(J1) < WEI_THR ) GOTO 910
              TIM_VAL = TIM_ARR(J1) - TIME_FRT
              DO 920 J2=1,LFRQ
                  DO 930 J3=1,LCHN
                     IF ( UV(J3,J2,J1) == 0.0 ) GOTO 930
                     IF ( MODE == PIMA__GRAT ) THEN
                          PHS_MOD =   PH_RAT*PI2*FREQ_REF*TIM_VAL &
     &                              + GR_DEL*PI2*(FREQ_ARR(J3,J2)-FREQ_REF) &
     &                              + GR_RAT*PI2*(FREQ_ARR(J3,J2)-FREQ_REF)*TIM_VAL
                       ELSE IF ( MODE == PIMA__ACC ) THEN
                          PHS_MOD =   PH_RAT*PI2*FREQ_REF*TIM_VAL &
     &                              + GR_DEL*PI2*(FREQ_ARR(J3,J2)-FREQ_REF) &
     &                              + PH_ACC*PI2*FREQ_REF*TIM_VAL*TIM_VAL/2.0D0
                     END IF
                     DRF_IF(J2) = DRF_IF(J2) + UV(J3,J2,J1)* &
     &                             CMPLX ( COS(PHS_MOD), SIN(PHS_MOD) )* &
     &                             WEI(J1)
  930             CONTINUE 
  920         CONTINUE 
  910      CONTINUE 
      END IF
!
! --- Compute band avereged visibilities
!
      DO 1010 J1=1,LFRQ
         DRF_BAND = DRF_BAND + DRF_IF(J1)
 1010 CONTINUE 
!
      RETURN
      END  SUBROUTINE PIMA_UV_DRF3  !#!#
