      SUBROUTINE NUT_FRESEL_HEO ( FRESEL, IVRB, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine NUT_FRESEL_HEO
! *                                                                      *
! *  ### 17-OCT-2003  NUT_FRESEL_HEO v2.1 (c) L. Petrov 04-DEC-2025 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'astro_constants.i'
      INCLUDE   'nut_const.i'
      INCLUDE   'nut.i'
      INCLUDE   'fresel.i'
      TYPE ( FRESEL__STRU ) :: FRESEL
      INTEGER*4  IVRB, IUER
      REAL*8     PHS_HEO(M_HEO),      FRQ_HEO(M_HEO),  ACC_HEO(M_HEO), &
     &           PMC_HEO(M_HEO),      PMS_HEO(M_HEO),                  &
     &           PMC_RATE_HEO(M_HEO), PMS_RATE_HEO(M_HEO)
!
      REAL*8       RFCN_AMP, PFICN_AMP, FRQ_EPS, TOL_FRQ, FRQ_PRC, &
     &             AMP_PRC, AMP_EPS, AMP_MIN
      PARAMETER  (  RFCN_AMP = 5.D-10 )
      PARAMETER  ( PFICN_AMP = 2.D-10 )
      PARAMETER  (   FRQ_EPS = 1.D-12 )
      PARAMETER  (   TOL_FRQ = 0.90D0  )
      PARAMETER  (   FRQ_PRC = -OM__EAR - OM_PRC )
      PARAMETER  (   AMP_PRC = 4.D-8  )
      PARAMETER  (   AMP_EPS = 0.25D0  )
      PARAMETER  (   AMP_MIN = 1.D-13  )
      REAL*8     ARR1(M_HEO), ARR2(M_HEO), PHS_ARR(M_HEO), FRQ_ARR(M_HEO), &
     &           ACC_ARR(M_HEO), AMP_ARR(M_HEO), PMC_ARR(M_HEO), &
     &           PMS_ARR(M_HEO), AMP, FRQ_MIN
      LOGICAL*4  FRQ_USED(M_HEO)
      INTEGER*4  IND_FRQ(M_HEO), IND_BIG(M_HEO)
      REAL*8     FRQ_STEP, FRQ_LAST, DIF_MIN, TIM_STEP, POW_MAX, WIN, ARG, &
     &           ACC_MAIN, FRQ_MAIN, PHS_MAIN, FRQ_LEFT, FRQ_RIGHT
      REAL*8     TIM_SDL(M_PWR), AC_SDL(M_PWR), AS_SDL(M_PWR), &
     &           PC_SDL(M_PWR),  PS_SDL(M_PWR), VAL(M_PWR)
      REAL*8     CI_COS, CI_SIN, SI_COS, SI_SIN, AC_INTG, AS_INTG, &
     &           PC_INTG, PS_INTG, PHS_DIF
      INTEGER*4  L_HEO, IND, N_FRQ, I_FRQ, K_FRQ, L_BIG, ITURN, IND_MAX, &
     &           N_SDL, K_SDL, IND_SDL(M_HEO), N_VAR, N_NEG_FRQ, &
     &           J1, J2, J3, J4, J5, J6, J7, J8, J9, J10, J11, J12, J13, IER
!!                     real*8     fun1(m_pwr), fun2(m_pwr), chint
      REAL*8     ATAN_CS
      INTEGER*4  IFIND_PL
!
      CALL ERR_PASS ( IUER, IER )
      IF ( FRESEL%NUTEXP .EQ. REN2000__REF ) THEN
           CALL GET_REN2000 ( M_HEO, L_HEO, PHS_HEO, FRQ_HEO, ACC_HEO, &
     &                      PMC_HEO, PMS_HEO, PMC_RATE_HEO, PMS_RATE_HEO, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 3331, IUER, 'NUT_FRESEL_HEO', 'Error in '// &
     &              'getting REN2000 nutation expansion' )
                RETURN
           END IF
        ELSE IF ( FRESEL%NUTEXP .EQ. MHB2000__REF ) THEN
           CALL GET_MHB2000 ( M_HEO, L_HEO, PHS_HEO, FRQ_HEO, ACC_HEO, &
     &                      PMC_HEO, PMS_HEO, PMC_RATE_HEO, PMS_RATE_HEO, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 3332, IUER, 'NUT_FRESEL_HEO', 'Error in '// &
     &              'getting MHB2000 nutation expansion' )
                RETURN
           END IF
        ELSE IF ( FRESEL%NUTEXP .EQ. MHB2000_TRANSF__REF ) THEN
           CALL GET_MHB2000_TRANSF ( M_HEO, L_HEO, PHS_HEO, FRQ_HEO, ACC_HEO, &
     &                      PMC_HEO, PMS_HEO, PMC_RATE_HEO, PMS_RATE_HEO, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 3332, IUER, 'NUT_FRESEL_HEO', 'Error in '// &
     &              'getting MHB2000 nutation expansion' )
                RETURN
           END IF
        ELSE IF ( FRESEL%NUTEXP .EQ. MHB2000__REN2000__REF ) THEN
           CALL GET_MHB2000_M_REN2000 ( M_HEO, L_HEO, PHS_HEO, FRQ_HEO, &
     &          ACC_HEO, PMC_HEO, PMS_HEO, PMC_RATE_HEO, PMS_RATE_HEO, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 3332, IUER, 'NUT_FRESEL_HEO', 'Error in '// &
     &              'getting MHB2000 nutation expansion' )
                RETURN
           END IF
        ELSE IF ( FRESEL%NUTEXP .EQ. WAHR1980__REF ) THEN
           CALL GET_WAHR1980 ( M_HEO, L_HEO, PHS_HEO, FRQ_HEO, &
     &          ACC_HEO, PMC_HEO, PMS_HEO, PMC_RATE_HEO, PMS_RATE_HEO, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 3333, IUER, 'NUT_FRESEL_HEO', 'Error in '// &
     &              'getting WAHR1980 nutation expansion' )
                RETURN
           END IF
        ELSE IF ( FRESEL%NUTEXP .EQ. IERS1996__REF ) THEN
           CALL GET_IERS1996 ( M_HEO, L_HEO, PHS_HEO, FRQ_HEO, &
     &          ACC_HEO, PMC_HEO, PMS_HEO, PMC_RATE_HEO, PMS_RATE_HEO, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 3334, IUER, 'NUT_FRESEL_HEO', 'Error in '// &
     &              'getting IERS1996  nutation expansion' )
                RETURN
           END IF
        ELSE
           CALL ERR_LOG ( 3335, IUER, 'NUT_FRESEL_HEO', 'Unsupported '// &
     &         'expansion: '//FRESEL%NUTEXP )
           RETURN
      END IF
!
      FRQ_MIN = PI2/(FRESEL%TIME_END - FRESEL%TIME_BEG)
      IF ( IVRB .EQ. 3 ) THEN
           WRITE ( 6, * ) ' FRQ_MIN = ', FRQ_MIN
           WRITE ( 6, * ) ' NF      = ', FRESEL%NF
           WRITE ( 6, * ) ' FRESEL%NUTAMP_MIN = ',FRESEL%NUTAMP_MIN
      END IF
!
      N_FRQ = 0
!
! --- First nutation offset
!
      N_FRQ = N_FRQ + 1
      FRQ_ARR(N_FRQ) = FRQ_PRC
      AMP_ARR(N_FRQ) = 0.0D0
      IND_FRQ(N_FRQ) = IND__OFFS
!
      IF ( DABS(FRESEL%PFICN_FREQ_MIN) .GT. FRQ_EPS .AND. &
     &     DABS(FRESEL%PFICN_FREQ_MAX - FRESEL%PFICN_FREQ_MIN) .GT.FRQ_EPS) THEN
!
! -------- Then PFICN
!
           K_FRQ = DABS( FRESEL%PFICN_FREQ_MAX - FRESEL%PFICN_FREQ_MIN )/FRESEL%NF
           IF ( K_FRQ .LT. 2 ) THEN
                N_FRQ = N_FRQ + 1
                ACC_ARR(N_FRQ) = 0.0D0
                FRQ_ARR(N_FRQ) = &
     &                 (FRESEL%PFICN_FREQ_MIN + FRESEL%PFICN_FREQ_MAX)/2.0D0
                PHS_ARR(N_FRQ) = -FRQ_ARR(N_FRQ)*(FRESEL%TIME_REF)
                AMP_ARR(N_FRQ) = DSQRT( PFICN_AMP**2 +  FRESEL%NUTAMP_MIN**2 )
                IND_FRQ(N_FRQ) = IND__PFICN
             ELSE
                FRQ_STEP = (FRESEL%PFICN_FREQ_MAX - FRESEL%PFICN_FREQ_MIN)/K_FRQ
                DO 410 J1=1,K_FRQ
                   N_FRQ = N_FRQ + 1
                   ACC_ARR(N_FRQ) = 0.0D0
                   FRQ_ARR(N_FRQ) = FRESEL%PFICN_FREQ_MIN + (J1-1)*FRQ_STEP
                   PHS_ARR(N_FRQ) = -FRQ_ARR(N_FRQ)*(FRESEL%TIME_REF)
                   AMP_ARR(N_FRQ) = DSQRT ( PFICN_AMP**2 +  FRESEL%NUTAMP_MIN**2 )
                   IND_FRQ(N_FRQ) = IND__PFICN
 410           CONTINUE
           END IF
      END IF
!
      IF ( DABS(FRESEL%RFCN_FREQ_MIN) .GT. FRQ_EPS .AND. &
     &     DABS(FRESEL%RFCN_FREQ_MAX - FRESEL%RFCN_FREQ_MIN) .GT. FRQ_EPS ) THEN
!
! -------- Then RFCN
!
           K_FRQ = DABS( FRESEL%RFCN_FREQ_MAX - FRESEL%RFCN_FREQ_MIN )/FRESEL%NF
           IF ( K_FRQ .LT. 2 ) THEN
                N_FRQ = N_FRQ + 1
                ACC_ARR(N_FRQ) = 0.0D0
                FRQ_ARR(N_FRQ) = &
     &                 (FRESEL%RFCN_FREQ_MIN + FRESEL%RFCN_FREQ_MAX)/2.0D0
                PHS_ARR(N_FRQ) = -FRQ_ARR(N_FRQ)*(FRESEL%TIME_REF)
                AMP_ARR(N_FRQ) = DSQRT ( RFCN_AMP**2 +  FRESEL%NUTAMP_MIN**2 )
                IND_FRQ(N_FRQ) = IND__RFCN
              ELSE
                FRQ_STEP = (FRESEL%RFCN_FREQ_MAX - FRESEL%RFCN_FREQ_MIN)/K_FRQ
!!  write ( 6, * ) '@@@ frq_step = ', frq_step ! %%%w
                DO 420 J2=1,K_FRQ
                   N_FRQ = N_FRQ + 1
                   ACC_ARR(N_FRQ) = 0.0D0
                   FRQ_ARR(N_FRQ) = FRESEL%RFCN_FREQ_MIN + (J2-1)*FRQ_STEP
                   PHS_ARR(N_FRQ) = -FRQ_ARR(N_FRQ)*(FRESEL%TIME_REF)
                   AMP_ARR(N_FRQ) = DSQRT( RFCN_AMP**2 +  FRESEL%NUTAMP_MIN**2 )
                   IND_FRQ(N_FRQ) = IND__RFCN
 420           CONTINUE
           END IF
      END IF
!
      DO 430 J3=1,L_HEO
         ARR1(J3) = -DSQRT ( PMC_HEO(J3)**2 + PMS_HEO(J3)**2 )
         ARR2(J3) = J3 + 0.001D0
         FRQ_USED(J3) = .FALSE.
 430  CONTINUE
!
      CALL SORT8 ( L_HEO, ARR1, ARR2 )
!
! --- Then other frequencies in descending their amplitudes
!
      DO 440 J4=1,L_HEO
         IF ( -ARR1(J4) .LT. FRESEL%NUTAMP_MIN ) GOTO 840 ! We reached the threshold
         IND = ARR2(J4)
         DIF_MIN = 1.D10
         DO 450 J5=1,N_FRQ
            IF ( DABS ( FRQ_HEO(IND) - FRQ_ARR(J5) ) .LT. DIF_MIN ) THEN
                 DIF_MIN = DABS ( FRQ_HEO(IND) - FRQ_ARR(J5) )
            END IF
 450     CONTINUE
         IF ( DIF_MIN .GT. FRESEL%NF*TOL_FRQ  .AND.  &
     &        DABS(FRQ_HEO(IND)) .GT. FRQ_LOW      ) THEN
!
              N_FRQ = N_FRQ + 1
              FRQ_ARR(N_FRQ) = FRQ_HEO(IND)
              PHS_ARR(N_FRQ) = PHS_HEO(IND)
              ACC_ARR(N_FRQ) = ACC_HEO(IND)
              AMP_ARR(N_FRQ) = -ARR1(IND)
              IND_FRQ(N_FRQ) = IND
         END IF
 440  CONTINUE
 840  CONTINUE
!
! --- Build arrays for sorting in increasing frequencies
!
      DO 460 J6=1,N_FRQ
         ARR1(J6) = FRQ_ARR(J6)
         ARR2(J6) = 1.0D0*J6 + 0.001D0
 460  CONTINUE
!
! --- Sort arrays
!
      CALL SORT8 ( N_FRQ, ARR1, ARR2 )
!
! --- Put phases and frequencies in FRESEL
!
      IF ( IVRB .GE. 1 ) THEN
           WRITE ( 6, * ) ' N_FRQ=', N_FRQ
      END IF
      N_VAR = 0
      N_NEG_FRQ = 0
      DO 470 J7=1,N_FRQ
         IND = ARR2(J7)
!
         IF ( FRESEL%PM_EST_NUT ) THEN
              FRESEL%DAT(J7)%USE_PM = .TRUE.
              N_VAR = N_VAR + 2
            ELSE
              FRESEL%DAT(J7)%USE_PM = .FALSE.
         END IF
!
         IF ( FRESEL%UT1_EST_NUT .AND. FRESEL%DAT(J7)%FRQ .LT. -FRQ_LOW ) THEN
!
! ----------- variations in UT1 are allowed only for a large negative frequency
!
              FRESEL%DAT(J7)%USE_UT1 = .TRUE.
            ELSE
              FRESEL%DAT(J7)%USE_UT1 = .FALSE.
         END IF
!
         FRESEL%DAT(J7)%TID_AMPL = 0.0D0
         IF ( IND_FRQ(IND) .LE. 0 ) THEN
              FRESEL%DAT(J7)%TYP = IND_FRQ(IND)
              FRESEL%DAT(J7)%IND = 0
            ELSE
              FRESEL%DAT(J7)%TYP = IND__NUT
              FRESEL%DAT(J7)%IND = IND_FRQ(IND)
              FRQ_USED(FRESEL%DAT(J7)%IND) = .TRUE.
         END IF
!
         FRESEL%DAT(J7)%PHS  = PHS_ARR(IND)
         FRESEL%DAT(J7)%FRQ  = FRQ_ARR(IND)
         FRESEL%DAT(J7)%ACCL = ACC_ARR(IND)
         ITURN = IDNINT ( PHS_ARR(IND)/PI2 )
         FRESEL%DAT(J7)%PHS = FRESEL%DAT(J7)%PHS - ITURN*PI2
         IF ( FRESEL%DAT(J7)%PHS .LT. 0.0D0 ) THEN
              FRESEL%DAT(J7)%PHS = FRESEL%DAT(J7)%PHS + PI2
         END IF
!
         IF ( IVRB .GE. 3 ) THEN
              IF ( J7 .EQ. 1 ) FRQ_LAST = FRESEL%DAT(J7)%FRQ
              WRITE ( 6, 120 ) J7, FRESEL%DAT(J7)%TYP, FRESEL%DAT(J7)%IND, &
     &                         FRESEL%DAT(J7)%PHS, FRESEL%DAT(J7)%FRQ, &
     &                         (FRESEL%DAT(J7)%FRQ - FRQ_LAST)/FRESEL%NF
 120          FORMAT ( I4,') ', I2,' Ind=',I4,' PHS=',F12.10, &
     &                 ' FRQ=',1PD19.12,'  Dif=',0PF8.2 )
         END IF
         FRQ_LAST = FRESEL%DAT(J7)%FRQ
         IF ( FRESEL%DAT(J7)%FRQ .LT. -FRQ_LOW ) THEN
              N_NEG_FRQ = N_NEG_FRQ + 1
         END IF
 470  CONTINUE
      FRESEL%N_NUT = N_FRQ
      FRESEL%N_FRQ = N_FRQ
      FRESEL%N_PRC = 0
!
      IF ( FRESEL%PRC_EST ) THEN
           FRESEL%N_PRC = 1
!
! -------- And at last, precession
!
           N_VAR = N_VAR + 2
           FRESEL%N_FRQ = FRESEL%N_FRQ + 1
           FRESEL%DAT(FRESEL%N_FRQ)%ACCL  = 0.0D0
           FRESEL%DAT(FRESEL%N_FRQ)%FRQ   = FRQ_PRC
           FRESEL%DAT(FRESEL%N_FRQ)%PHS   = 0.0D0
           FRESEL%DAT(FRESEL%N_FRQ)%PMC   = 1.0D0
           FRESEL%DAT(FRESEL%N_FRQ)%PMS   = 0.0D0
           FRESEL%DAT(FRESEL%N_FRQ)%L_PWR = 0
           FRESEL%DAT(FRESEL%N_FRQ)%IND   = 0
           FRESEL%DAT(FRESEL%N_FRQ)%TYP   = IND__PRC
           FRESEL%DAT(FRESEL%N_FRQ)%NAM   = 'Precession      '
           FRESEL%DAT(FRESEL%N_FRQ)%TID_AMPL = 0.0D0
!
           FRESEL%DAT(FRESEL%N_FRQ)%USE_PM  = .FALSE.
           FRESEL%DAT(FRESEL%N_FRQ)%USE_UT1 = .FALSE.
      END IF
!
! === Now search for sidelobes
!
      DO 480 J8=1,FRESEL%N_NUT
         N_SDL = 0
         ACC_MAIN = FRESEL%DAT(J8)%ACCL
         FRQ_MAIN = FRESEL%DAT(J8)%FRQ
         PHS_MAIN = FRESEL%DAT(J8)%PHS
         IF ( FRESEL%DAT(J8)%TYP .EQ. IND__RFCN  .OR. &
     &        FRESEL%DAT(J8)%TYP .EQ. IND__PFICN      ) THEN
!
! ----------- Do not apply sidelobe treatment for RFCN, PFICN
!
              GOTO 480
         END IF
!
         IF ( J8 .GT. 1 ) THEN
              FRQ_LEFT = FRQ_MAIN + ( FRESEL%DAT(J8-1)%FRQ - FRQ_MAIN )/2.0D0
            ELSE
              FRQ_LEFT = 2.0D0*FRESEL%DAT(J8)%FRQ
         END IF
!
         IF ( J8 .LT. FRESEL%N_NUT ) THEN
              FRQ_RIGHT = FRQ_MAIN + ( FRESEL%DAT(J8+1)%FRQ - FRQ_MAIN )/2.0D0
            ELSE
              FRQ_RIGHT = 2.D0*FRQ_MAIN
         END IF
!
         DO 490 J9=1,L_HEO
            IF ( FRQ_HEO(J9) .GE. FRQ_LEFT   .AND.  &
     &           FRQ_HEO(J9) .LT. FRQ_RIGHT  .AND.  &
     &           .NOT. FRQ_USED(J9)                 ) THEN
!
! -------------- This frequency is a sidelobe
!
                 N_SDL = N_SDL + 1
                 IND_SDL(N_SDL) = J9
                 FRQ_USED(J9) = .TRUE.
            END IF
 490     CONTINUE
         IF ( N_SDL .GT. 0 ) THEN
              K_SDL = K_SDL + 1
         END IF
         DO 4100 J10=1,M_PWR
!
! --------- Compute varying amplitude in the points of Chebyshev alternance
!
            TIM_SDL(J10) = FRESEL%TIME_BEG + &
     &                     (FRESEL%TIME_END - FRESEL%TIME_BEG)* &
     &                     (1.0D0 + DCOS( (M_PWR-J10)*PI__NUM/(M_PWR-1) ) )/2.D0
!
            CI_COS = 0.0D0
            CI_SIN = 0.0D0
            SI_COS = 0.0D0
            SI_SIN = 0.0D0
            IF ( FRESEL%DAT(J8)%TYP .EQ. IND__NUT ) THEN
                 CI_COS = PMC_HEO(FRESEL%DAT(J8)%IND) + &
     &                    PMC_RATE_HEO(FRESEL%DAT(J8)%IND)*TIM_SDL(J10)
                 SI_COS = PMS_HEO(FRESEL%DAT(J8)%IND) + &
     &                    PMS_RATE_HEO(FRESEL%DAT(J8)%IND)*TIM_SDL(J10)
            END IF
!
            IF ( N_SDL .GT. 0 ) THEN
!
! -------------- Cycle over all sidelobes. update CI_COS, CI_SIN, SI_COS, SI_SIN
!
                 DO 4110 J11=1,N_SDL
                    CI_COS = CI_COS + &
     &                     ( PMC_HEO(IND_SDL(J11)) +                        &
     &                       PMC_RATE_HEO(IND_SDL(J11))*TIM_SDL(J10) )*     &
     &                DCOS( (ACC_HEO(IND_SDL(J11))-ACC_MAIN)*TIM_SDL(J10)**2/2.D0 + &
     &                      (FRQ_HEO(IND_SDL(J11))-FRQ_MAIN)*TIM_SDL(J10) + &
     &                      (PHS_HEO(IND_SDL(J11))-PHS_MAIN) )
!
                    CI_SIN = CI_SIN + &
     &                     ( PMC_HEO(IND_SDL(J11)) +                        &
     &                       PMC_RATE_HEO(IND_SDL(J11))*TIM_SDL(J10) )*     &
     &                DSIN( (ACC_HEO(IND_SDL(J11))-ACC_MAIN)*TIM_SDL(J10)**2/2.D0 + &
     &                      (FRQ_HEO(IND_SDL(J11))-FRQ_MAIN)*TIM_SDL(J10) + &
     &                      (PHS_HEO(IND_SDL(J11))-PHS_MAIN) )
!
                    SI_COS = SI_COS + &
     &                     ( PMS_HEO(IND_SDL(J11)) +                        &
     &                       PMS_RATE_HEO(IND_SDL(J11))*TIM_SDL(J10) )*     &
     &                DCOS( (ACC_HEO(IND_SDL(J11))-ACC_MAIN)*TIM_SDL(J10)**2/2.D0 + &
     &                      (FRQ_HEO(IND_SDL(J11))-FRQ_MAIN)*TIM_SDL(J10) + &
     &                      (PHS_HEO(IND_SDL(J11))-PHS_MAIN) )
!
                    SI_SIN = SI_SIN + &
     &                     ( PMS_HEO(IND_SDL(J11)) +                        &
     &                       PMS_RATE_HEO(IND_SDL(J11))*TIM_SDL(J10) )*     &
     &                DSIN( (ACC_HEO(IND_SDL(J11))-ACC_MAIN)*TIM_SDL(J10)**2/2.D0 + &
     &                      (FRQ_HEO(IND_SDL(J11))-FRQ_MAIN)*TIM_SDL(J10) + &
     &                      (PHS_HEO(IND_SDL(J11))-PHS_MAIN) )
 4110            CONTINUE
            END IF
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!         write ( 6, * ) ' j10=',j10,' ci_cos=',ci_cos,' ci_sin=',ci_sin, & ! %%
!     &                              ' si_cos=',si_cos,' si_sin=',si_sin    ! %%
!         fun1(j10) = si_sin                                                ! %%
!         fun2(j10) = si_cos                                                ! %%
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
            AC_SDL(J10) = DSQRT ( CI_COS**2 + CI_SIN**2 )
            AS_SDL(J10) = DSQRT ( SI_COS**2 + SI_SIN**2 )
            IF ( AC_SDL(J10) .GT. AMP_MIN ) THEN
                 PC_SDL(J10) = ATAN_CS ( CI_COS, CI_SIN )
              ELSE
                 PC_SDL(J10) = 0.0D0
            END IF
!
            IF ( AS_SDL(J10) .GT. AMP_MIN ) THEN
                 PS_SDL(J10) = ATAN_CS ( SI_COS, SI_SIN )
               ELSE
                 PS_SDL(J10) = 0.0D0
            END IF
            IF ( J10 .GT. 1 ) THEN
!
! -------------- Resolve phase ambiguity. There should not been PI2 jumps in
! -------------- phase
!
                 ITURN = IDNINT ( (PC_SDL(J10) - PC_SDL(J10-1))/PI2 )
                 PC_SDL(J10) = PC_SDL(J10) - ITURN*PI2
!
                 ITURN = IDNINT ( (PS_SDL(J10) - PS_SDL(J10-1))/PI2 )
                 PS_SDL(J10) = PS_SDL(J10) - ITURN*PI2
            END IF
 4100    CONTINUE
         FRESEL%DAT(J8)%L_PWR = M_PWR ! Reserve for future needs...
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!           if ( j8 .eq. 211  .or.  j8 .eq. 212 ) then               ! %%%%
!                write ( 6, * ) 'j8=',j8,' n_sdl=',n_sdl, &          ! %%%%
!     &                         ' accl=',fresel%dat(j8)%accl, &      ! %%%%
!     &                         ' pms=',pms_heo(fresel%dat(j8)%ind)  ! %%%%
!                call diagi_1 ( m_pwr, tim_sdl, fun1, -3 )           ! %%%%
!                call diagi_1 ( m_pwr, tim_sdl, fun2, -3 )           ! %%%%
!                call diagi_1 ( m_pwr, tim_sdl, ac_sdl, -3 )         ! %%%%
!                call diagi_1 ( m_pwr, tim_sdl, as_sdl, -3 )         ! %%%%
!                call diagi_1 ( m_pwr, tim_sdl, pc_sdl, -3 )         ! %%%%
!                call diagi_1 ( m_pwr, tim_sdl, ps_sdl, -3 )         ! %%%%
!           end if                                                   ! %%%%
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!
! ------ Compute coefficients of expansion of the varying amplutudes in
! ------ Chebyshev polynomials
!
         CALL COPY_R8 ( M_PWR, AC_SDL, VAL ) ! CHCR destroys array of values
         CALL CHCR ( M_PWR, FRESEL%TIME_BEG, &
     &               (FRESEL%TIME_END - FRESEL%TIME_BEG), TIM_SDL, VAL, &
     &               FRESEL%DAT(J8)%AC_CHE )
!
         CALL COPY_R8 ( M_PWR, AS_SDL, VAL )
         CALL CHCR ( M_PWR, FRESEL%TIME_BEG, &
     &               (FRESEL%TIME_END - FRESEL%TIME_BEG), TIM_SDL, VAL, &
     &               FRESEL%DAT(J8)%AS_CHE )
!
         CALL COPY_R8 ( M_PWR, PC_SDL, VAL )
         CALL CHCR ( M_PWR, FRESEL%TIME_BEG, &
     &               (FRESEL%TIME_END - FRESEL%TIME_BEG), TIM_SDL, VAL, &
     &               FRESEL%DAT(J8)%PC_CHE )
!
         CALL COPY_R8 ( M_PWR, PS_SDL, VAL )
         CALL CHCR ( M_PWR, FRESEL%TIME_BEG, &
     &               (FRESEL%TIME_END - FRESEL%TIME_BEG), TIM_SDL, VAL, &
     &               FRESEL%DAT(J8)%PS_CHE )
!
! ------ Compute the average value of AC
!
         CALL CHINTI ( M_PWR, FRESEL%TIME_BEG, FRESEL%TIME_BEG, &
     &                 FRESEL%TIME_END, (FRESEL%TIME_END - FRESEL%TIME_BEG), &
     &                 FRESEL%DAT(J8)%AC_CHE, AC_INTG, IER )
         FRESEL%DAT(J8)%AC_AVR = AC_INTG/(FRESEL%TIME_END - FRESEL%TIME_BEG)
!
! ------ Compute the average value of AS
!
         CALL CHINTI ( M_PWR, FRESEL%TIME_BEG, FRESEL%TIME_BEG, &
     &                 FRESEL%TIME_END, (FRESEL%TIME_END - FRESEL%TIME_BEG), &
     &                 FRESEL%DAT(J8)%AS_CHE, AS_INTG, IER )
         FRESEL%DAT(J8)%AS_AVR = AS_INTG/(FRESEL%TIME_END - FRESEL%TIME_BEG)
!
! ------ Compute the average value of PC
!
         CALL CHINTI ( M_PWR, FRESEL%TIME_BEG, FRESEL%TIME_BEG, &
     &                 FRESEL%TIME_END, (FRESEL%TIME_END - FRESEL%TIME_BEG), &
     &                 FRESEL%DAT(J8)%PC_CHE, PC_INTG, IER )
         FRESEL%DAT(J8)%PC_AVR = PC_INTG/(FRESEL%TIME_END - FRESEL%TIME_BEG)
!
! ------ and resolve ambiguity in PC_AVR
!
         ITURN = IDNINT ( FRESEL%DAT(J8)%PC_AVR/PI2 )
         FRESEL%DAT(J8)%PC_AVR = FRESEL%DAT(J8)%PC_AVR - ITURN*PI2
!
! ------ Compute the average value of PS
!
         CALL CHINTI ( M_PWR, FRESEL%TIME_BEG, FRESEL%TIME_BEG, &
     &                 FRESEL%TIME_END, (FRESEL%TIME_END - FRESEL%TIME_BEG), &
     &                 FRESEL%DAT(J8)%PS_CHE, PS_INTG, IER )
         FRESEL%DAT(J8)%PS_AVR = PS_INTG/(FRESEL%TIME_END - FRESEL%TIME_BEG)
!
! ------ and resolve ambiguity in PS_AVR
!
         ITURN = IDNINT ( FRESEL%DAT(J8)%PS_AVR/PI2 )
         FRESEL%DAT(J8)%PS_AVR = FRESEL%DAT(J8)%PS_AVR - ITURN*PI2
!
! ------ Now normalize AC_SDL and AS_SDL
!
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!!                write ( 6, * ) 'ac_avr=',fresel%dat(j8)%ac_avr ! %%%%%
!!                write ( 6, * ) 'as_avr=',fresel%dat(j8)%as_avr ! %%%%%
!!                call diagi_1 ( m_pwr, tim_sdl, ac_sdl, -3 )   ! %%%%%%
!!                call diagi_1 ( m_pwr, tim_sdl, as_sdl, -3 )   ! %%%%%%
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
         DO 4120 J12=1,M_PWR
            IF ( DABS(FRESEL%DAT(J8)%AC_AVR) .GT. AMP_MIN ) THEN
                 AC_SDL(J12) = AC_SDL(J12)/FRESEL%DAT(J8)%AC_AVR - 1.0D0
            END IF
            IF ( DABS(FRESEL%DAT(J8)%AS_AVR) .GT. AMP_MIN ) THEN
                 AS_SDL(J12) = AS_SDL(J12)/FRESEL%DAT(J8)%AS_AVR - 1.0D0
            END IF
!
            IF ( FRESEL%DAT(J8)%PC_AVR .GT.  3.0D0*PI__NUM/4.0D0 .OR. &
     &           FRESEL%DAT(J8)%PC_AVR .LT. -3.0D0*PI__NUM/4.0D0      ) THEN
                 PC_SDL(J12) = PC_SDL(J12) - PI__NUM
            END IF
!
            IF ( FRESEL%DAT(J8)%PS_AVR .GT.  3.0D0*PI__NUM/4.0D0 .OR. &
     &           FRESEL%DAT(J8)%PS_AVR .LT. -3.0D0*PI__NUM/4.0D0      ) THEN
                 PS_SDL(J12) = PS_SDL(J12) - PI__NUM
            END IF
 4120    CONTINUE
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!!                call diagi_1 ( m_pwr, tim_sdl, fun1, -3 )   ! %%%%%%
!!                call diagi_2 ( m_pwr, tim_sdl, ac_sdl, m_pwr, tim_sdl, fun1, -3 )   ! %%%%%%
!!                call diagi_1 ( m_pwr, tim_sdl, ac_sdl, -3 )   ! %%%%%%
!!                call diagi_1 ( m_pwr, tim_sdl, as_sdl, -3 )   ! %%%%%%
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!
! ------ If phase is around -PI ( [-3/4*PI, 3/4*PI] ), then we flip the sign
! ------ of the of the amplitude
!
         IF ( FRESEL%DAT(J8)%PC_AVR .GT.  3.0D0*PI__NUM/4.0D0 .OR. &
     &        FRESEL%DAT(J8)%PC_AVR .LT. -3.0D0*PI__NUM/4.0D0      ) THEN
!
              FRESEL%DAT(J8)%AC_AVR = -FRESEL%DAT(J8)%AC_AVR
              FRESEL%DAT(J8)%PC_AVR =  FRESEL%DAT(J8)%PC_AVR - PI__NUM
              IF ( FRESEL%DAT(J8)%PC_AVR .LT. 0.0D0 ) THEN
                   FRESEL%DAT(J8)%PC_AVR = FRESEL%DAT(J8)%PC_AVR + PI2
              END IF
         END IF
!
         IF ( FRESEL%DAT(J8)%PS_AVR .GT.  3.0D0*PI__NUM/4.0D0 .OR. &
     &        FRESEL%DAT(J8)%PS_AVR .LT. -3.0D0*PI__NUM/4.0D0      ) THEN
!
              FRESEL%DAT(J8)%AS_AVR = -FRESEL%DAT(J8)%AS_AVR
              FRESEL%DAT(J8)%PS_AVR =  FRESEL%DAT(J8)%PS_AVR - PI__NUM
              IF ( FRESEL%DAT(J8)%PS_AVR .LT. 0.0D0 ) THEN
                   FRESEL%DAT(J8)%PS_AVR = FRESEL%DAT(J8)%PS_AVR + PI2
              END IF
         END IF
!
! ------ Compute Chebyshev polynomials of normalized amplitude function
!
         IF ( DABS(FRESEL%DAT(J8)%AC_AVR) .GT. FRESEL%NUTAMP_MIN ) THEN
              CALL CHCR ( M_PWR, FRESEL%TIME_BEG, &
     &                  (FRESEL%TIME_END - FRESEL%TIME_BEG), TIM_SDL, AC_SDL, &
     &                   FRESEL%DAT(J8)%AC_CHE )
              CALL CHCR ( M_PWR, FRESEL%TIME_BEG, &
     &                  (FRESEL%TIME_END - FRESEL%TIME_BEG), TIM_SDL, PC_SDL, &
     &                   FRESEL%DAT(J8)%PC_CHE )
            ELSE
              CALL NOUT_R8 ( M_PWR, FRESEL%DAT(J8)%AC_CHE )
              CALL NOUT_R8 ( M_PWR, FRESEL%DAT(J8)%PC_CHE )
              FRESEL%DAT(J8)%PC_AVR = 0.0D0
         END IF
!
         IF ( DABS(FRESEL%DAT(J8)%AS_AVR) .GT. FRESEL%NUTAMP_MIN  ) THEN
              CALL CHCR ( M_PWR, FRESEL%TIME_BEG, &
     &                  (FRESEL%TIME_END - FRESEL%TIME_BEG), TIM_SDL, AS_SDL, &
     &                   FRESEL%DAT(J8)%AS_CHE )
              CALL CHCR ( M_PWR, FRESEL%TIME_BEG, &
     &                  (FRESEL%TIME_END - FRESEL%TIME_BEG), TIM_SDL, PS_SDL, &
     &                   FRESEL%DAT(J8)%PS_CHE )
            ELSE
              CALL NOUT_R8 ( M_PWR, FRESEL%DAT(J8)%AS_CHE )
              CALL NOUT_R8 ( M_PWR, FRESEL%DAT(J8)%PS_CHE )
              FRESEL%DAT(J8)%PS_AVR = 0.0D0
         END IF
!
! ------ Now look, do we have the situaion when teh sidelopbe phase is close
! ------ to -+ PI/2? If yes, it would cause corss-term singularity
!
         IF ( ( FRESEL%DAT(J8)%PC_AVR .GT.  1.0D0*PI__NUM/3.0D0  .AND.        &
     &          FRESEL%DAT(J8)%PC_AVR .LT.  2.0D0*PI__NUM/3.0D0        ) .OR. &
     &        ( FRESEL%DAT(J8)%PC_AVR .LT. -1.0D0*PI__NUM/3.0D0  .AND.        &
     &          FRESEL%DAT(J8)%PC_AVR .GT. -2.0D0*PI__NUM/3.0D0        )      ) THEN
              CALL NOUT_R8 ( M_PWR, FRESEL%DAT(J8)%AC_CHE )
              CALL NOUT_R8 ( M_PWR, FRESEL%DAT(J8)%PC_CHE )
              FRESEL%DAT(J8)%PC_AVR = 0.0D0
         END IF
!
         IF ( ( FRESEL%DAT(J8)%PS_AVR .GT.  1.0D0*PI__NUM/3.0D0  .AND.        &
     &          FRESEL%DAT(J8)%PS_AVR .LT.  2.0D0*PI__NUM/3.0D0        ) .OR. &
     &        ( FRESEL%DAT(J8)%PS_AVR .LT. -1.0D0*PI__NUM/3.0D0  .AND.        &
     &          FRESEL%DAT(J8)%PS_AVR .GT. -2.0D0*PI__NUM/3.0D0        )      ) THEN
              CALL NOUT_R8 ( M_PWR, FRESEL%DAT(J8)%AS_CHE )
              CALL NOUT_R8 ( M_PWR, FRESEL%DAT(J8)%PS_CHE )
              FRESEL%DAT(J8)%PS_AVR = 0.0D0
         END IF
!
! ------ Finally, look at the difference in cross phases
!
         PHS_DIF = FRESEL%DAT(J8)%PS_AVR - FRESEL%DAT(J8)%PC_AVR
         IF ( ( PHS_DIF .GT.  1.0D0*PI__NUM/3.0D0  .AND.        &
     &          PHS_DIF .LT.  2.0D0*PI__NUM/3.0D0        ) .OR. &
     &        ( PHS_DIF .LT. -1.0D0*PI__NUM/3.0D0  .AND.        &
     &          PHS_DIF .GT. -2.0D0*PI__NUM/3.0D0        )      ) THEN
              IF ( DABS(FRESEL%DAT(J8)%PC_AVR) .GT. &
     &             DABS(FRESEL%DAT(J8)%PS_AVR)      ) THEN
                   CALL NOUT_R8 ( M_PWR, FRESEL%DAT(J8)%AC_CHE )
                   CALL NOUT_R8 ( M_PWR, FRESEL%DAT(J8)%PC_CHE )
                   FRESEL%DAT(J8)%PC_AVR = 0.0D0
                 ELSE
                   CALL NOUT_R8 ( M_PWR, FRESEL%DAT(J8)%AS_CHE )
                   CALL NOUT_R8 ( M_PWR, FRESEL%DAT(J8)%PS_CHE )
                   FRESEL%DAT(J8)%PS_AVR = 0.0D0
             END IF
         END IF
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!         iturn = idnint ( fresel%dat(j8)%pc_avr/pi2 )
!         fresel%dat(j8)%pc_avr = fresel%dat(j8)%pc_avr - iturn*pi2
!         iturn = idnint ( fresel%dat(j8)%ps_avr/pi2 )
!         fresel%dat(j8)%ps_avr = fresel%dat(j8)%ps_avr - iturn*pi2
!!
!        write ( 6, 210 ) j8, fresel%dat(j8)%ac_avr, fresel%dat(j8)%as_avr, &  ! %%
!      &                      fresel%dat(j8)%pc_avr, fresel%dat(j8)%ps_avr
! 210    format ( ' j8=',i4,' ac_avr=',1pd11.3,' as_avr=',1pd11.3,  &   ! %%%%
!     &           ' pc_avr=',0pf7.3,' ps_avr=',0pf7.3 )                 ! %%%%
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
         IF ( N_SDL .EQ. 0  .OR.  FRESEL%IGNORE_SIDELOBES ) THEN
              CALL NOUT_R8 ( M_PWR, FRESEL%DAT(J8)%AC_CHE )
              CALL NOUT_R8 ( M_PWR, FRESEL%DAT(J8)%AS_CHE )
              CALL NOUT_R8 ( M_PWR, FRESEL%DAT(J8)%PC_CHE )
              CALL NOUT_R8 ( M_PWR, FRESEL%DAT(J8)%PS_CHE )
              FRESEL%DAT(J8)%PC_AVR = 0.0D0
              FRESEL%DAT(J8)%PS_AVR = 0.0D0
              FRESEL%DAT(J8)%L_PWR = 0
         END IF
 480  CONTINUE
!
      IF ( N_VAR .GE. 2 ) THEN
           CALL ERR_PASS ( IUER, IER )
           CALL FRESEL_NUTEST ( FRESEL, L_HEO, PHS_HEO, FRQ_HEO, ACC_HEO, &
     &          PMC_HEO, PMS_HEO, PMC_RATE_HEO, PMS_RATE_HEO, N_NEG_FRQ, &
     &          IVRB, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 3334, IUER, 'FRESEL_HEO', 'Error in '// &
     &              'FRESEL_NUTEST' )
                RETURN
           END IF
         ELSE
           FRESEL%NUT_RMS = 0.0D0
           FRESEL%NUT_MAX = 0.0D0
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  NUT_FRESEL_HEO  #!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE FRESEL_NUTEST ( FRESEL, L_HEO, PHS_HEO, FRQ_HEO, ACC_HEO, &
     &           PMC_HEO, PMS_HEO, PMC_RATE_HEO, PMS_RATE_HEO, N_NEG_FRQ, &
     &           IVRB, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine FRESEL_NUTEST  estimates average amplitudes of nutation    *
! *   expansion of the specified period of time.                         *
! *                                                                      *
! *  ### 17-OCT-2003  FRESEL_NUTEST v2.1 (c)  L. Petrov 03-OCT-2004 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'astro_constants.i'
      INCLUDE   'fresel.i'
      TYPE ( FRESEL__STRU ) :: FRESEL
      INTEGER*4  L_HEO, N_NEG_FRQ, IVRB, IUER
      REAL*8     PHS_HEO(L_HEO), FRQ_HEO(L_HEO), ACC_HEO(L_HEO)
      REAL*8     PMC_HEO(L_HEO), PMS_HEO(L_HEO)
      REAL*8     PMC_RATE_HEO(L_HEO), PMS_RATE_HEO(L_HEO)
      REAL*8     TIM(M_EPN), PM_VAL(M_EPN), RES(M_EPN), &
     &           FRQ_ARR(M_EPN), FTC(M_EPN), FTS(M_EPN), POW(M_EPN)
      REAL*8     ARG, TIM_STEP, EQU_CON(M_HEO), FRQ_NYQUIST
      REAL*8,    ALLOCATABLE :: NOR_MAT(:), NOR_VEC(:), EST_VEC(:)
      REAL*8     OM_PRC, FRQ_PRC
      REAL*8     AMP_FCT
      PARAMETER  ( AMP_FCT = 0.01D0 )
      PARAMETER  ( OM_PRC  = 7.08618327D-12 ) ! Precssion rate (L. Petrov, 2002)
      PARAMETER  ( FRQ_PRC = -OM__EAR - OM_PRC )
      CHARACTER  STR*32
      COMPLEX*16, ALLOCATABLE :: ARR_C16(:)
      REAL*8     RCOND, TIME_STEP, RES_MAX, RES_RMS, AC_AMP, AS_AMP, &
     &           PC_PHS, PS_PHS, WIN, POW_MAX
      INTEGER*4  IBEG, IEND
      INTEGER*4  N_VAR, I_VAR, IOS, IND_MAX, IND_FRQ(M_HEO), J1, J2, J3, J4, &
     &           J5, J6, J7, J8, J9, J10, J11, J12, IER
      REAL*8     CHINT, DP_VV_V
      INTEGER*4  I_LEN
!
      N_VAR = 2*(N_NEG_FRQ + FRESEL%N_PRC)
      IF ( N_VAR .LE. 2 ) THEN
           CALL ERR_LOG ( 0, IUER )
           RETURN
      END IF
!
      IF ( IVRB .GE. 1 ) THEN
           WRITE ( 6, * ) 'FRESEL_NUTEST: N_VAR=',N_VAR
      END IF
!
      ALLOCATE ( NOR_MAT((N_VAR*(N_VAR+1))/2), STAT=IOS )
      IF ( IOS .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( (N_VAR*(N_VAR+1))*4, STR )
           CALL ERR_LOG ( 3341, IUER, 'FRESEL_NUTEST', 'Error in allocation '// &
     &         'of '//STR(1:I_LEN(STR))//' bytes of dynamic memory' )
           RETURN
      END IF
      CALL NOUT_R8 ( (N_VAR*(N_VAR+1))/2, NOR_MAT )
!
      ALLOCATE ( NOR_VEC(N_VAR), STAT=IOS )
      IF ( IOS .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( N_VAR*8, STR )
           CALL ERR_LOG ( 3342, IUER, 'FRESEL_NUTEST', 'Error in allocation '// &
     &         'of '//STR(1:I_LEN(STR))//' bytes of dynamic memory' )
           RETURN
      END IF
      CALL NOUT_R8 ( N_VAR, NOR_VEC )
!
      ALLOCATE ( EST_VEC(N_VAR), STAT=IOS )
      IF ( IOS .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( N_VAR*8, STR )
           CALL ERR_LOG ( 3343, IUER, 'FRESEL_NUTEST', 'Error in allocation '// &
     &         'of '//STR(1:I_LEN(STR))//' bytes of dynamic memory' )
           RETURN
      END IF
      CALL NOUT_R8 ( N_VAR, EST_VEC )
      IF ( IVRB .GE. 2 ) THEN
           WRITE ( 6, * ) ' Compute equation of conditions...'
      END IF
      TIME_STEP = (FRESEL%TIME_END - FRESEL%TIME_BEG)/(M_EPN-1)
!@ call nout_r8 ( l_heo, pmc_rate_heo ) ! %%%%%%%
!@ call nout_r8 ( l_heo, pms_rate_heo ) ! %%%%%%%
!
! --- Compute time series
!
      DO 410 J1=1,M_EPN
         IF ( IVRB .GE. 3  .AND.  MOD(J1,1024) .EQ. 0 ) THEN
              WRITE ( 6, 110 ) J1, M_EPN, CHAR(13)
              CALL FLUSH ( 6 )
 110          FORMAT ( '  ',I6,' ( ',I6,' )    ',A$ )
         END IF
!
         TIM(J1) = FRESEL%TIME_BEG + (J1-1)*TIME_STEP
         CALL NOUT_R8 ( N_VAR, EQU_CON )
!
         I_VAR = 0
         DO 420 J2=1,FRESEL%N_FRQ
            IF ( .NOT. ( FRESEL%DAT(J2)%TYP .EQ. IND__NUT   .OR. &
     &                   FRESEL%DAT(J2)%TYP .EQ. IND__SDL   .OR. &
     &                   FRESEL%DAT(J2)%TYP .EQ. IND__OFFS  .OR. &
     &                   FRESEL%DAT(J2)%TYP .EQ. IND__RFCN  .OR. &
     &                   FRESEL%DAT(J2)%TYP .EQ. IND__PFICN      ) ) GOTO 420
!
! --------- Bypass low and positive frequencies
!
            IF ( FRESEL%DAT(J2)%FRQ .GT. -FRQ_LOW ) GOTO 420
!
            ARG = ( FRESEL%DAT(J2)%ACCL*TIM(J1)/2.0D0 + &
     &              FRESEL%DAT(J2)%FRQ )*TIM(J1) + FRESEL%DAT(J2)%PHS
            IF ( FRESEL%IGNORE_SIDELOBES ) THEN
                 AC_AMP = 1.0D0
                 PC_PHS = 0.0D0
               ELSE 
                 AC_AMP = 1.D0 + CHINT (FRESEL%DAT(J2)%L_PWR, FRESEL%TIME_BEG,      &
     &                             FRESEL%TIME_END - FRESEL%TIME_BEG, TIM(J1), &
     &                             FRESEL%DAT(J2)%AC_CHE, -3 )
                 PC_PHS =        CHINT (FRESEL%DAT(J2)%L_PWR, FRESEL%TIME_BEG,      &
     &                             FRESEL%TIME_END - FRESEL%TIME_BEG, TIM(J1), &
     &                             FRESEL%DAT(J2)%PC_CHE, -3 )
            END IF
            I_VAR = I_VAR + 1
            IND_FRQ(I_VAR) = J2
            EQU_CON(I_VAR) = AC_AMP*DCOS(ARG+PC_PHS)
!
            IF ( FRESEL%IGNORE_SIDELOBES ) THEN
                 AS_AMP = 1.0D0
                 PS_PHS = 0.0D0
               ELSE
                 AS_AMP = 1.D0 + CHINT (FRESEL%DAT(J2)%L_PWR, FRESEL%TIME_BEG,      &
     &                             FRESEL%TIME_END - FRESEL%TIME_BEG, TIM(J1), &
     &                             FRESEL%DAT(J2)%AS_CHE, -3 )
                 PS_PHS =        CHINT (FRESEL%DAT(J2)%L_PWR, FRESEL%TIME_BEG,      &
     &                             FRESEL%TIME_END - FRESEL%TIME_BEG, TIM(J1), &
     &                             FRESEL%DAT(J2)%PS_CHE, -3 )

            END IF
            I_VAR = I_VAR + 1
            IND_FRQ(I_VAR) = J2
            EQU_CON(I_VAR) = AS_AMP*DSIN(ARG+PS_PHS)
 420     CONTINUE
         IF ( FRESEL%N_PRC .GT. 0 ) THEN
!
! ----------- Precession
!
              I_VAR = I_VAR + 1
              IND_FRQ(I_VAR) = -1
              EQU_CON(I_VAR) = 2.0D0 * ( TIM(J1) - FRESEL%TIME_REF )/ &
     &             ( FRESEL%TIME_END - FRESEL%TIME_BEG )* DCOS(FRQ_PRC*TIM(J1))

              I_VAR = I_VAR + 1
              IND_FRQ(I_VAR) = -1
              EQU_CON(I_VAR) = 2.0D0 * ( TIM(J1) - FRESEL%TIME_REF )/ &
     &             ( FRESEL%TIME_END - FRESEL%TIME_BEG )* DSIN(FRQ_PRC*TIM(J1))
         END IF
!
! ------ Update normal matrix
!
         CALL DIAD_CVT_S ( 1.0D0, N_VAR, EQU_CON, EQU_CON, NOR_MAT )
!@  if ( j1 == 1 ) write ( 16, * ) ' i_var=',i_var,' frq=', FRESEL%DAT(J6)%FRQ ! %%%%
!
! ------ Compute right hand side
!
         PM_VAL(J1) = 0.0D0
         DO 430 J3=1,L_HEO
            IF ( FRQ_HEO(J3) .GT. -FRQ_LOW ) GOTO 430 ! Bypass low frequencies
            ARG = PHS_HEO(J3)+ FRQ_HEO(J3)*TIM(J1)+ ACC_HEO(J3)*TIM(J1)**2/2.0D0
!@  if ( j1 == 1 ) write ( 16, * ) ' j3= ', j3,' frq=', FRQ_HEO(J3) ! %%%
            PM_VAL(J1) = PM_VAL(J1) +                                         &
     &                   (PMC_HEO(J3) + PMC_RATE_HEO(J3)*TIM(J1))*DCOS(ARG) + &
     &                   (PMS_HEO(J3) + PMS_RATE_HEO(J3)*TIM(J1))*DSIN(ARG)
 430     CONTINUE
!
! ------ ... and normal vector
!
         DO 440 J4=1,N_VAR
            NOR_VEC(J4) = NOR_VEC(J4) + EQU_CON(J4)*PM_VAL(J1)
 440     CONTINUE
 410  CONTINUE
!
! --- Invert normal matrix
!
      IF ( IVRB .GE. 2 ) THEN
           WRITE ( 6, * ) ' Invert normal matrix                      '
      END IF
!
!@        call matview_2 ( n_var, nor_mat ) ! %%%
      CALL ERR_PASS ( IUER, IER )
      CALL INVS ( N_VAR, NOR_MAT, RCOND, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 3344, IUER, 'FRESEL_NUTEST', 'Error in an '// &
     &         'attempt to invert normal matrix' )
           RETURN
      END IF
      IF ( IVRB .GE. 2 ) THEN
           WRITE ( 6, * ) ' R_COND = ',RCOND
      END IF
!
! --- Get solution
!
      IER = -1
      CALL MUL_MV_SV_V ( N_VAR, NOR_MAT, N_VAR, NOR_VEC, N_VAR, EST_VEC, IER )
!
      RES_RMS = 0.0D0
      RES_MAX = 0.0D0
      IF ( IVRB .GE. 2 ) THEN
           CALL ANAL_COV ( N_VAR, NOR_MAT, 0.70D0, IND_FRQ )
      END IF
      IF ( IVRB .GE. 2 ) THEN
           WRITE ( 6, * ) ' Compute residuals'
      END IF
!
! --- Compute residuals
!
      DO 450 J5=1,M_EPN
         IF ( IVRB .GE. 3  .AND.  MOD(J5,2048) .EQ. 0 ) THEN
              WRITE ( 6, 110 ) J5, M_EPN, CHAR(13)
              CALL FLUSH ( 6 )
         END IF
!
! ------ Initialization of the equation of conditions
!
         CALL NOUT_R8 ( N_VAR, EQU_CON )
         I_VAR = 0
         DO 460 J6=1,FRESEL%N_FRQ
            IF ( .NOT. ( FRESEL%DAT(J6)%TYP .EQ. IND__NUT   .OR. &
     &                   FRESEL%DAT(J6)%TYP .EQ. IND__SDL   .OR. &
     &                   FRESEL%DAT(J6)%TYP .EQ. IND__OFFS  .OR. &
     &                   FRESEL%DAT(J6)%TYP .EQ. IND__RFCN  .OR. &
     &                   FRESEL%DAT(J6)%TYP .EQ. IND__PFICN      ) ) GOTO 460
            IF ( FRESEL%DAT(J6)%FRQ .GT. -FRQ_LOW ) GOTO 460
!
            ARG = ( FRESEL%DAT(J6)%ACCL*TIM(J5)/2.0D0 + &
     &              FRESEL%DAT(J6)%FRQ )*TIM(J5) + FRESEL%DAT(J6)%PHS
            IF ( FRESEL%IGNORE_SIDELOBES ) THEN
                 AC_AMP = 1.0D0
                 PC_PHS = 0.0D0
               ELSE 
                 AC_AMP = 1.D0 + CHINT (FRESEL%DAT(J6)%L_PWR, FRESEL%TIME_BEG,  &
     &                             FRESEL%TIME_END - FRESEL%TIME_BEG, TIM(J5),  &
     &                             FRESEL%DAT(J6)%AC_CHE, -3 )
                 PC_PHS =        CHINT ( FRESEL%DAT(J6)%L_PWR, FRESEL%TIME_BEG, &
     &                             FRESEL%TIME_END - FRESEL%TIME_BEG, TIM(J5),  &
     &                             FRESEL%DAT(J6)%PC_CHE, -3 )
            END IF
            I_VAR = I_VAR + 1
            EQU_CON(I_VAR) = AC_AMP*DCOS(ARG+PC_PHS)
!
            IF ( FRESEL%IGNORE_SIDELOBES ) THEN
                 AS_AMP = 1.0D0
                 PS_PHS = 0.0D0
               ELSE
                 AS_AMP = 1.D0 + CHINT (FRESEL%DAT(J6)%L_PWR, FRESEL%TIME_BEG, &
     &                             FRESEL%TIME_END - FRESEL%TIME_BEG, TIM(J5), &
     &                             FRESEL%DAT(J6)%AS_CHE, -3 )
                 PS_PHS =        CHINT (FRESEL%DAT(J6)%L_PWR, FRESEL%TIME_BEG, &
     &                             FRESEL%TIME_END - FRESEL%TIME_BEG, TIM(J5), &
     &                             FRESEL%DAT(J6)%PS_CHE, -3 )
            END IF
            I_VAR = I_VAR + 1
            EQU_CON(I_VAR) = AS_AMP*DSIN(ARG+PS_PHS)
 460     CONTINUE
!
         IF ( FRESEL%N_PRC .GT. 0 ) THEN
              I_VAR = I_VAR + 1
              EQU_CON(I_VAR) = 2.0D0 * ( TIM(J5) - FRESEL%TIME_REF )/ &
     &             ( FRESEL%TIME_END - FRESEL%TIME_BEG )* DCOS(FRQ_PRC*TIM(J5))
!
              I_VAR = I_VAR + 1
              EQU_CON(I_VAR) = 2.0D0 * ( TIM(J5) - FRESEL%TIME_REF )/ &
     &             ( FRESEL%TIME_END - FRESEL%TIME_BEG )* DSIN(FRQ_PRC*TIM(J5))
         END IF
!
         RES(J5) = PM_VAL(J5) - DP_VV_V ( N_VAR, EQU_CON, EST_VEC )
         RES_RMS = RES_RMS + RES(J5)**2
         IF ( DABS(RES(J5)) .GT. RES_MAX ) RES_MAX = DABS(RES(J5))
 450  CONTINUE
      RES_RMS = DSQRT ( RES_RMS/M_EPN )
!
      FRESEL%NUT_RMS = RES_RMS
      FRESEL%NUT_MAX = RES_MAX
      IF ( IVRB .GE. 1 ) THEN
           WRITE ( 6, 210 ) RES_RMS*1.D12,  RES_MAX*1.D12
 210       FORMAT ( ' Residuals: rms= ', F16.2, ' prad,   max= ', F16.2,' prad' )
           WRITE ( 6, * ) 'Nutation solution is done'
      END IF
!
      IF ( IVRB .GE. 4 ) THEN
           IER = -1
           CALL DIAGI_1 ( M_EPN, TIM, RES, IER  )
      END IF
      IF ( IVRB .GE. 2 ) THEN
!
! -------- Apply Hann window
!
           DO 470 J7=1,M_EPN
              WIN = 0.5D0 - 0.5D0*DCOS((PI2*J7)/M_EPN)
              RES(J7) = 1.D12*WIN*RES(J7)
 470       CONTINUE
!
! -------- Run fast Fourier transform
!
           ALLOCATE ( ARR_C16(M_EPN) )
!
           FRQ_NYQUIST = PI2/(TIM(2) - TIM(1))
!
           CALL FFT_1D_R2C_R8 ( M_EPN, RES, ARR_C16, IER )
           DO 490 J9=1,M_EPN
              IF ( J9 .LE. M_EPN/2 ) THEN
                   FRQ_ARR(J9) = (J9-1)*FRQ_NYQUIST/M_EPN
                ELSE 
                   FRQ_ARR(J9) = (J9-1-M_EPN)*FRQ_NYQUIST/M_EPN
              END IF
              FTC(J9) = REAL ( ARR_C16(J9) )
              FTS(J9) = IMAG ( ARR_C16(J9) )
 490       CONTINUE 
           DEALLOCATE   ( ARR_C16 )
!
! -------- Compute power spectrum
!
           POW_MAX = -1.0D0
           IND_MAX = -1
           IBEG = 0
           DO 4100 J10=1,M_EPN/2
              IF ( J10 .GT. 1 ) THEN
                   POW(J10) = 2.0D0*( FTC(J10)**2 + FTS(J10)**2 )
                 ELSE
                   POW(J10) = FTC(J10)**2 + FTS(J10)**2
              END IF
              IF ( POW(J10) .GT. POW_MAX ) THEN
                   POW_MAX = POW(J10)
                   IND_MAX = J10
              END IF
              IF ( FRQ_ARR(J10) .GT. 6.5D-5  .AND. IBEG .EQ. 0 ) IBEG = J10
              IF ( FRQ_ARR(J10) .LT. 8.0D-5                    ) IEND = J10
 4100       CONTINUE
           WRITE ( 6, * ) ' FRQ_VARR(IND_MAX)=', FRQ_ARR(IND_MAX), &
     &                    ' POW=',POW_MAX
           IF ( IVRB .GE. 4 ) THEN
               CALL DIAGI_1 ( IEND-IBEG, FRQ_ARR(IBEG), POW(IBEG), IER )
           END IF
      END IF
!
! --- Store the estimate of nutation amplitudes
!
      I_VAR = 0
      DO 4110 J11=1,FRESEL%N_FRQ
         IF ( .NOT. ( FRESEL%DAT(J11)%TYP .EQ. IND__NUT   .OR. &
     &                FRESEL%DAT(J11)%TYP .EQ. IND__SDL   .OR. &
     &                FRESEL%DAT(J11)%TYP .EQ. IND__OFFS  .OR. &
     &                FRESEL%DAT(J11)%TYP .EQ. IND__RFCN  .OR. &
     &                FRESEL%DAT(J11)%TYP .EQ. IND__PFICN      ) ) GOTO 4110
!
! ------ Bypass low and positive frequencies
!
         IF ( FRESEL%DAT(J11)%FRQ .GT. -FRQ_LOW ) GOTO 4110
!
         I_VAR = I_VAR + 1
         FRESEL%DAT(J11)%PMC_EST = EST_VEC(I_VAR)
!
         I_VAR = I_VAR + 1
         FRESEL%DAT(J11)%PMS_EST = EST_VEC(I_VAR)
 4110 CONTINUE
!
! --- Store the estimate of the precession amaplitude
!
      DO 4120 J12=1,FRESEL%N_FRQ
         IF ( FRESEL%DAT(J12)%FRQ .GT. -FRQ_LOW  ) GOTO 4120
         IF ( FRESEL%DAT(J12)%TYP == IND__PRC ) THEN
              I_VAR = I_VAR + 1
              FRESEL%DAT(J12)%PMC_EST = EST_VEC(I_VAR)
!
              I_VAR = I_VAR + 1
              FRESEL%DAT(J12)%PMS_EST = EST_VEC(I_VAR)
         END IF
 4120 CONTINUE
!
      DEALLOCATE ( NOR_MAT )
      DEALLOCATE ( NOR_VEC )
      DEALLOCATE ( EST_VEC )
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  FRESEL_NUTEST  #!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE  ANAL_COV ( N_VAR, NOR_MAT, VAL, IND_FRQ )
      IMPLICIT   NONE
      INTEGER*4  N_VAR, IND_FRQ(N_VAR)
      REAL*8     NOR_MAT(*), VAL, COV
      INTEGER*4  J1, J2
      INTEGER*4  LOCS, I, J
      LOCS(I,J) = MIN(I,J) + (MAX(I,J)*(MAX(I,J)-1))/2
!
      DO 410 J1=1,N_VAR-1
         DO 420 J2=J1+1,N_VAR
            COV= NOR_MAT(LOCS(J1,J2))/ &
     &           DSQRT(NOR_MAT(LOCS(J1,J1))*NOR_MAT(LOCS(J2,J2)))
            IF ( DABS(COV) .GT. VAL ) THEN
                 WRITE ( 6, 110 ) J1, J2, IND_FRQ(J1), IND_FRQ(J2), COV
 110             FORMAT ( 'I=',I4,' J=',I4,'  IND_I=',I4,' IND_J=',I4, &
     &                    '  Cov=',F7.4 )
            END IF
 420     CONTINUE
 410  CONTINUE
!
      RETURN
      END  !#!  ANAL_COV  #!#
