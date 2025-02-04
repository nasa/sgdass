      SUBROUTINE FRINGE_ADJ ( MODE, LTIM, LCHN, LFRQ, TIME_FRT, FREQ_ARR, &
     &                        FREQ_REF, WEI_R4, WEI_THR, TIM_ARR, PHAS_INP, &
     &                        PH_RAT_INP, GR_DEL_INP, PHAS_ADJ, PHRAT_ADJ, &
     &                        GRDEL_ADJ, GRRAT_ADJ, PHACC_ADJ, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine FRINGE_ADJ  re-ajdust parameters of fringe fit.            *
! *   The input parameters PHAS_INP, PH_RAT_INP, GR_DEL_INP determined   *
! *   by the coarse fringe fitting are used to compute modeled phase.    *
! *   NB: the coarse fringe fitting determines only group delay and      *
! *   phase delay rate, but not group delay rate or phase acceleration.  *
! *   That modeled phase is used as an input for computing four          *
! *   parameters: PHAS_ADJ, PHRAT_ADJ, GRDEL_ADJ and either GRRAT_ADJ or *
! *   PHACC_ADJ depending on the mode. The purpose of this routine is to *
! *   gt the best initial value of either GRRAT_ADJ or PHACC_ADJ.        *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *       MODE ( INTEGER*4 ) -- Parameter that determines the mode of    *
! *                             computation:                             *
! *                             PIMA__GRAT -- to adjust group delay      *
! *                                           rate.                      *
! *                             PIMA__ACC  -- to adjust phase            *
! *                                           acceleration.              *
! *       LTIM ( INTEGER*4 ) -- The number of accumulation periods over  *
! *                             time.                                    *
! *       LCHN ( INTEGER*4 ) -- The number of frequency channels in a    *
! *                             given intermediate frequency IF.         *
! *       LFRQ ( INTEGER*4 ) -- The number of intermediate frequencies   *
! *                             (IFs).                                   *
! *        WEI ( REAL*4    ) -- Array of weights for each accumulation   *
! *                             period in range [0, 1]. Dimension: LTIM. *
! *   TIME_FRT ( REAL*8    ) -- Fringe reference time from the nominal   *
! *                             start of the observation.                *
! *   FREQ_ARR ( REAL*8    ) -- Frequency array. Dimension:              *
! *                             (LCHN,LFRQ).                             *
! *   FREQ_REF ( REAL*8    ) -- Frequency array. Dimension:              *
! *     WEI_R4 ( REAL*4    ) -- Array of weights for each accumulation   *
! *                             period in range [0, 1]. Dimension: LTIM. *
! *    WEI_THR ( REAL*8    ) -- Weight threshold: observations with      *
! *                             weights that are less than WEI_THR are   *
! *                             discarded.                               *
! *    TIM_ARR ( REAL*8    ) -- Array of time epochs counted from the    *
! *                             1-st epoch (1st epoch is zero).          *
! *                             Units: sec.                              *
! *    WEI_THR ( REAL*8    ) -- Array of weights for each accumulation   *
! *   PHAS_INP ( REAL*8    ) -- Fringe phase from the coarse procedure   *
! * PH_RAT_INP ( REAL*8    ) -- Phase delay rate from the coarse         *
! *                             procedure.                               *
! * GR_DEL_INP ( REAL*8    ) -- Group delay from the coarse procedure.   *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *   PHAS_ADJ ( REAL*8    ) -- Output adjusted phase.                   *
! * PH_RAT_ADJ ( REAL*8    ) -- Output adjusted phase delay rate.        *
! * GR_DEL_ADJ ( REAL*8    ) -- Output adjusted group delay.             *
! * PHACC_ADJ  ( REAL*8    ) -- Output adjusted phase acceleration.      *
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
! *    IUER ( INTEGER*4, OPT ) -- Universal error handler.               *
! *                      Input: IUER=0 -- no error message will be       *
! *                                       printed even in the case       *
! *                                       of error.                      *
! *                             IUER=-1,-2,-3 -- in the case of error    *
! *                                       the message will be put on     *
! *                                       stdout.                        *
! *                             IUER=-3 -- in the case of error after    *
! *                                        printing the error message    *
! *                                        the program will terminate.   *
! *                       Output:                                        *
! *                             if input value of IUER =-2,-3 -- IUER    *
! *                                        is not modified.              *
! *                             otherwise, the output value of IUER is 0 *
! *                             in the case of successful and            *
! *                             positive non-zero in the vase of errors. *
! *                                                                      *
! *  ### 13-AUG-2009    FRINGE_ADJ  v1.0 (c) L. Petrov  31-MAR-2020 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'pima.i'
      INTEGER*4  MODE, LTIM, LCHN, LFRQ, IUER
      REAL*8     TIME_FRT, FREQ_ARR(LCHN,LFRQ), FREQ_REF, TIM_ARR(LTIM), &
     &           PHAS_INP, PH_RAT_INP, GR_DEL_INP, GRDEL_ADJ, PHAS_ADJ, &
     &           PHRAT_ADJ, GRRAT_ADJ, PHACC_ADJ, WEI_THR
      REAL*4     WEI_R4(LTIM)
      INTEGER*4  MP, MMP
      PARAMETER  ( MP = 4, MMP=(MP*(MP+1))/2 )
      REAL*8     NOR_MAT(MMP), NOR_VEC(MP), EQU_OBS(MP), &
     &           SCL(MP), SCL_DEF(MP), EST(MP), RC, RH
!      DATA       SCL / 1.0D0, 1.0D0, 1.0D0, 1.0D0  /
      DATA       SCL_DEF &
     &               / &
     &                 0.10D0,  &
     &                 0.05D0, &
     &                20.0D0,  &
     &                10.0D0   &
     &               /
      INTEGER*4  J1, J2, J3, J4, J5, J6, J7, J8, KP, L_EQU, IER
!
! --- Re-refer the phase to FREQ_REF  since the original phase 
! --- was referred to FREQ_ARR(1,1)
!
!?      PHAS_INP = PHAS_INP + GR_DEL*(FREQ_REF - FREQ_ARR(1,1))
!
      CALL NOUT_R8 ( MMP, NOR_MAT ) 
      CALL NOUT_R8 (  MP, NOR_VEC ) 
      KP = 0 
      SCL = SCL_DEF/LTIM
      DO 410 J1=1,LTIM
         IF ( WEI_R4(J1) > WEI_THR ) THEN
              DO 420 J2=1,LFRQ
                 DO 430 J3=1,LCHN
                    EQU_OBS(1) = SCL(1)*1.0D0                                    ! phase
                    EQU_OBS(2) = SCL(2)*(TIM_ARR(J1) - TIME_FRT)                 ! phase delay rate
                    EQU_OBS(3) = SCL(3)*( FREQ_ARR(J3,J2)/FREQ_REF - 1.0D0 )     ! group delay
                    IF ( MODE == PIMA__GRAT ) THEN
                         EQU_OBS(4) = SCL(4)*( FREQ_ARR(J3,J2)/FREQ_REF - 1.0D0 )* &  
     &                                         (TIM_ARR(J1) - TIME_FRT)          ! group delay rate
                       ELSE 
                         EQU_OBS(4) = SCL(4)*(TIM_ARR(J1) - TIME_FRT)**2/2.0D0   ! phase delay acceleration
                    END IF
!
! ----------------- Update normnal matrix elements
!
                    CALL DIAD_CVT_S ( DBLE(WEI_R4(J1))**2, MP, EQU_OBS, &
     &                                EQU_OBS, NOR_MAT )
!
! ----------------- Right hand side: we apply phase, phase delay rate and
! ----------------- gropup delay from the coarse fringe fittig
!
                    RH = -PHAS_INP &                                             ! phase
     &                   +PI2*( + &
     &                          PH_RAT_INP*FREQ_REF*(TIM_ARR(J1) - TIME_FRT) + & ! phase delay rate
     &                          GR_DEL_INP*(FREQ_ARR(J3,J2)- FREQ_REF) &         ! group delay
     &                        )
!
! ----------------- Update normal vector
!
                    DO 440 J4=1,MP
                       NOR_VEC(J4) = NOR_VEC(J4) + WEI_R4(J1)**2*EQU_OBS(J4)*RH
 440                CONTINUE 
 430             CONTINUE 
 420          CONTINUE 
              KP = KP + 1
         END IF         
 410  CONTINUE 
!
      IF ( KP < 2 ) THEN
!
! -------- Too few usable data
!
           PHAS_ADJ  = PHAS_INP
           PHRAT_ADJ = PH_RAT_INP
           GRDEL_ADJ = GR_DEL_INP
           GRRAT_ADJ = 0.0D0
           PHACC_ADJ = 0.0D0
           CALL ERR_LOG ( 0, IUER )
           RETURN 
      END IF
!
! --- Invert the normal matrix
!
      CALL ERR_PASS ( IUER, IER )
      CALL INVS ( MP, NOR_MAT, RC, IER )
      IF ( IER .NE. 0 ) THEN
           WRITE ( 6, * ) 'TIME_FRT= ', TIME_FRT
           CALL ERR_LOG ( 8611, IUER, 'FRINGE_ADJ', 'Failure to invert '// &
     &         'the normal matrix during an attempt to re-adjust '// &
     &         'parameters of coarse fringe fitting' )
           RETURN 
      END IF
!
! --- Get solution
!
      CALL MUL_MV_SV_V ( MP, NOR_MAT, MP, NOR_VEC, MP, EST, -2 )
      PHAS_ADJ  = EST(1)*SCL(1)
      PHRAT_ADJ = EST(2)*SCL(2)/(PI2*FREQ_REF)
      GRDEL_ADJ = EST(3)*SCL(3)/(PI2*FREQ_REF)
      IF ( MODE == PIMA__GRAT ) THEN
           GRRAT_ADJ = EST(4)*SCL(4)/(PI2*FREQ_REF)
           PHACC_ADJ = 0.0D0
         ELSE
           GRRAT_ADJ = 0.0D0
           PHACC_ADJ = EST(4)*SCL(4)/(PI2*FREQ_REF)
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  FRINGE_ADJ  !#!#
