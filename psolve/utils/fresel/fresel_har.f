      SUBROUTINE FRESEL_HAR ( FRESEL, LUN_LOG, IVRB, IUER ) 
! ************************************************************************
! *                                                                      *
! *   Routine FRESEL_HAR
! *                                                                      *
! *  ### 11-FEB-2006   FRESEL_HAR  v1.0 (c)  L. Petrov  11-FEB-2006 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'astro_constants.i'
      INCLUDE   'nut_const.i'
      INCLUDE   'nut.i'
      INCLUDE   'fresel.i'
      TYPE     ( FRESEL__STRU ) :: FRESEL
      INTEGER*4  LUN_LOG, IVRB, IUER
      CHARACTER  BUF(M_HEO)*128, STR*80
      REAL*8     TOL_FRQ, FRQ_EPS, FRQ_PRC, FRQ_NUT_MIN, FRQ_NUT_MAX
      PARAMETER  ( TOL_FRQ = 0.95D0 )
      PARAMETER  ( FRQ_EPS = 1.D-12 )
      PARAMETER  ( FRQ_PRC = -OM__EAR - OM_PRC )
      PARAMETER  ( FRQ_NUT_MIN = -9.7D-5 )
      PARAMETER  ( FRQ_NUT_MAX = -4.9D-5 )
      REAL*8     ARR1(M_HEO), ARR2(M_HEO), AMP_ARR(M_HEO)
      REAL*8     FRQ_STEP, FRQ_ADH, PHS_ADH, ARG
      REAL*8     PHS_HEO(M_HEO),      FRQ_HEO(M_HEO),  ACC_HEO(M_HEO), &
     &           PMC_HEO(M_HEO),      PMS_HEO(M_HEO),                  &
     &           PMC_RATE_HEO(M_HEO), PMS_RATE_HEO(M_HEO)
      REAL*8     FRQ_ARR(M_HEO)
      REAL*8     TIM(M_EPN), PM(M_EPN), RES(M_EPN), RES_RMS, &
     &           RES_MAX, TIME_STEP, RCOND, WIN, POW_MAX, FRQ_MAX 
      REAL*8     AMP_APR, AMP_EST_SQ, PHS_ARR(M_HEO), FRQ_APR(M_HEO), &
     &           ACC_ARR(M_HEO), PMC_ARR(M_HEO), PMS_ARR(M_HEO)
      REAL*8     RFCN_AMP, PFICN_AMP, AMP_APR_NUT, AMP_STC_NUT, &
     &           FRQ_STC_MIN, FRQ_STC_MAX, AMP_PRC, DIF_MIN, &
     &           DIF_MIN_ALL, FRQ_LAST, FRQ_MIN, &
     &           FRQ_CMP_TOL
      PARAMETER  (  RFCN_AMP = 5.D-10 )
      PARAMETER  ( PFICN_AMP = 2.D-10 )
      PARAMETER  ( AMP_APR_NUT = 2.D-9 )
      PARAMETER  ( AMP_STC_NUT = 2.D-9 )
      PARAMETER  ( FRQ_STC_MIN = -7.36D-5 )
      PARAMETER  ( FRQ_STC_MAX = -7.25D-5 )
      PARAMETER  ( FRQ_CMP_TOL = 1.D-15 ) 
      PARAMETER  ( AMP_PRC = 1.D-17 ) ! rad/s
      LOGICAL*4  FRQ_USED(M_HEO)
      INTEGER*4  NBUF, L_HEO, K_FRQ, L_FRQ, N_FRQ, IND, IND_MIN, ITURN, &
     &           J1, J2, J3, J4, J5, J6, J7, J8, J9, J10, L_LAB, &
     &           IND_MAX, IPL, IP, IOS, L_VAR, I_VAR, N_VAR, N_NEG_FRQ, &
     &           IND_FRQ(M_HEO), IND_MAIN(M_HEO), IS, IER
      CHARACTER  LAB_ARR(M_HEO)*10
      REAL*8,    EXTERNAL :: DP_VV_V 
      INTEGER*4, EXTERNAL :: ILEN, I_LEN, LTM_DIF
!
      FRQ_STEP = PI2/(FRESEL%TIME_END - FRESEL%TIME_BEG)
      WRITE ( 6, * ) ' FRQ_STEP = ', FRQ_STEP
!
      CALL ERR_PASS ( IUER, IER )
      IF ( FRESEL%NUTEXP .EQ. REN2000__REF ) THEN
           CALL GET_REN2000 ( M_HEO, L_HEO, PHS_HEO, FRQ_HEO, ACC_HEO, &
     &                      PMC_HEO, PMS_HEO, PMC_RATE_HEO, PMS_RATE_HEO, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 3513, IUER, 'FRESEL_HAR', 'Error in '// &
     &              'getting REN2000 nutation expansion' )
                RETURN
           END IF
        ELSE IF ( FRESEL%NUTEXP .EQ. MHB2000__REF ) THEN
           CALL GET_MHB2000 ( M_HEO, L_HEO, PHS_HEO, FRQ_HEO, ACC_HEO, &
     &                      PMC_HEO, PMS_HEO, PMC_RATE_HEO, PMS_RATE_HEO, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 3514, IUER, 'FRESEL_HAR', 'Error in '// &
     &              'getting MHB2000 nutation expansion' )
                RETURN
           END IF
        ELSE IF ( FRESEL%NUTEXP .EQ. MHB2000_TRANSF__REF ) THEN
           CALL GET_MHB2000_TRANSF ( M_HEO, L_HEO, PHS_HEO, FRQ_HEO, ACC_HEO, &
     &                      PMC_HEO, PMS_HEO, PMC_RATE_HEO, PMS_RATE_HEO, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 3515, IUER, 'FRESEL_HAR', 'Error in '// &
     &              'getting MHB2000 nutation expansion' )
                RETURN
           END IF
        ELSE IF ( FRESEL%NUTEXP .EQ. MHB2000__REN2000__REF ) THEN
           CALL GET_MHB2000_M_REN2000 ( M_HEO, L_HEO, PHS_HEO, FRQ_HEO, &
     &          ACC_HEO, PMC_HEO, PMS_HEO, PMC_RATE_HEO, PMS_RATE_HEO, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 3516, IUER, 'FRESEL_HAR', 'Error in '// &
     &              'getting MHB2000 nutation expansion' )
                RETURN
           END IF
        ELSE IF ( FRESEL%NUTEXP .EQ. WAHR1980__REF ) THEN
           CALL GET_WAHR1980 ( M_HEO, L_HEO, PHS_HEO, FRQ_HEO, &
     &          ACC_HEO, PMC_HEO, PMS_HEO, PMC_RATE_HEO, PMS_RATE_HEO, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 3517, IUER, 'FRESEL_HAR', 'Error in '// &
     &              'getting WAHR1980 nutation expansion' )
                RETURN
           END IF
        ELSE IF ( FRESEL%NUTEXP .EQ. IERS1996__REF ) THEN
           CALL GET_IERS1996 ( M_HEO, L_HEO, PHS_HEO, FRQ_HEO, &
     &          ACC_HEO, PMC_HEO, PMS_HEO, PMC_RATE_HEO, PMS_RATE_HEO, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 3518, IUER, 'FRESEL_HAR', 'Error in '// &
     &              'getting IERS1996  nutation expansion' )
                RETURN
           END IF
        ELSE
           CALL ERR_LOG ( 3519, IUER, 'FRESEL_HAR', 'Unsupported '// &
     &         'expansion: '//FRESEL%NUTEXP )
           RETURN
      END IF
!
      N_FRQ = 0
!
      FRQ_MIN = PI2/(FRESEL%TIME_END - FRESEL%TIME_BEG)
      IF ( IVRB .EQ. 3 ) THEN
           WRITE ( 6, * ) ' FRQ_MIN=',FRQ_MIN
           WRITE ( 6, * ) ' FRESEL%NUTAMP_MIN = ',FRESEL%NUTAMP_MIN
      END IF
!
! --- First nutation offset
!
      N_FRQ = N_FRQ + 1
      FRQ_ARR(N_FRQ)  = FRQ_PRC
      PHS_ARR(N_FRQ)  = 0.0D0 
      AMP_ARR(N_FRQ)  = 0.0D0
      IND_FRQ(N_FRQ)  = IND__OFFS
      IND_MAIN(N_FRQ) = 0
!
      IF ( DABS(FRESEL%PFICN_FREQ_MIN) .GT. FRQ_EPS .AND. &
     &     DABS(FRESEL%PFICN_FREQ_MAX - FRESEL%PFICN_FREQ_MIN) .GT.FRQ_EPS) THEN
!
! -------- Then PFICN
!
           K_FRQ = NINT ( DABS( FRESEL%PFICN_FREQ_MAX - FRESEL%PFICN_FREQ_MIN )/FRQ_MIN ) + 1 
           IF ( K_FRQ .LT. 2 ) THEN
                N_FRQ = N_FRQ + 1
                ACC_ARR(N_FRQ) = 0.0D0
                PMC_ARR(N_FRQ) = 0.0D0
                PMS_ARR(N_FRQ) = 0.0D0
                FRQ_ARR(N_FRQ) = &
     &                 (FRESEL%PFICN_FREQ_MIN + FRESEL%PFICN_FREQ_MAX)/2.0D0
                PHS_ARR(N_FRQ) = -FRQ_ARR(N_FRQ)*(FRESEL%TIME_REF)
                AMP_ARR(N_FRQ) = DSQRT( PFICN_AMP**2 +  FRESEL%NUTAMP_MIN**2 )
                IND_FRQ(N_FRQ) = IND__PFICN
                IND_MAIN(N_FRQ) = 0
             ELSE
                FRQ_STEP = (FRESEL%PFICN_FREQ_MAX - FRESEL%PFICN_FREQ_MIN)/(K_FRQ - 1)
                DO 410 J1=1,K_FRQ
                   N_FRQ = N_FRQ + 1
                   ACC_ARR(N_FRQ) = 0.0D0
                   PMC_ARR(N_FRQ) = 0.0D0
                   PMS_ARR(N_FRQ) = 0.0D0
                   FRQ_ARR(N_FRQ) = FRESEL%PFICN_FREQ_MIN + (J1-1)*FRQ_STEP
                   PHS_ARR(N_FRQ) = -FRQ_ARR(N_FRQ)*(FRESEL%TIME_REF)
                   AMP_ARR(N_FRQ) = DSQRT ( PFICN_AMP**2 +  FRESEL%NUTAMP_MIN**2 )
                   IND_FRQ(N_FRQ) = IND__PFICN
                   IND_MAIN(N_FRQ) = 0
 410           CONTINUE
           END IF
      END IF
!
      IF ( DABS(FRESEL%RFCN_FREQ_MIN) .GT. FRQ_EPS .AND. &
     &     DABS(FRESEL%RFCN_FREQ_MAX - FRESEL%RFCN_FREQ_MIN) .GT. FRQ_EPS ) THEN
!
! -------- Then RFCN
!
           K_FRQ = NINT ( DABS( FRESEL%RFCN_FREQ_MAX - FRESEL%RFCN_FREQ_MIN )/FRQ_MIN  ) + 1
           IF ( K_FRQ .LT. 2 ) THEN
                N_FRQ = N_FRQ + 1
                ACC_ARR(N_FRQ) = 0.0D0
                PMC_ARR(N_FRQ) = 0.0D0
                PMS_ARR(N_FRQ) = 0.0D0
                FRQ_ARR(N_FRQ) = &
     &                 (FRESEL%RFCN_FREQ_MIN + FRESEL%RFCN_FREQ_MAX)/2.0D0
                PHS_ARR(N_FRQ) = -FRQ_ARR(N_FRQ)*(FRESEL%TIME_REF)
                AMP_ARR(N_FRQ) = DSQRT ( RFCN_AMP**2 +  FRESEL%NUTAMP_MIN**2 )
                IND_FRQ(N_FRQ) = IND__RFCN
                IND_MAIN(N_FRQ) = 0
              ELSE
                FRQ_STEP = (FRESEL%RFCN_FREQ_MAX - FRESEL%RFCN_FREQ_MIN)/(K_FRQ - 1)
                DO 420 J2=1,K_FRQ
                   N_FRQ = N_FRQ + 1
                   ACC_ARR(N_FRQ) = 0.0D0
                   PMC_ARR(N_FRQ) = 0.0D0
                   PMS_ARR(N_FRQ) = 0.0D0
                   FRQ_ARR(N_FRQ) = FRESEL%RFCN_FREQ_MIN + (J2-1)*FRQ_STEP
                   PHS_ARR(N_FRQ) = -FRQ_ARR(N_FRQ)*(FRESEL%TIME_REF)
                   AMP_ARR(N_FRQ) = DSQRT( RFCN_AMP**2 +  FRESEL%NUTAMP_MIN**2 )
                   IND_FRQ(N_FRQ) = IND__RFCN
                   IND_MAIN(N_FRQ) = 0
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
         IND = ARR2(J4)
         IF ( -ARR1(J4) .LT. FRESEL%NUTAMP_MIN ) GOTO 840 ! We reached the threshold
         IF ( DABS(FRQ_HEO(IND)) .LT. FRQ_LOW  ) GOTO 440
         IF ( FRQ_HEO(IND) > FRESEL%PFICN_FREQ_MIN - FRQ_CMP_TOL .AND. &
     &        FRQ_HEO(IND) < FRESEL%PFICN_FREQ_MAX + FRQ_CMP_TOL       ) GOTO 440
         IF ( FRQ_HEO(IND) > FRESEL%RFCN_FREQ_MIN - FRQ_CMP_TOL  .AND. &
     &        FRQ_HEO(IND) < FRESEL%RFCN_FREQ_MAX + FRQ_CMP_TOL        ) GOTO 440
!
         DIF_MIN     = 1.D10
         DIF_MIN_ALL = 1.D10
         IND_MIN     = 0 
         DO 450 J5=1,N_FRQ
            IF ( IND_FRQ(J5) .NE. IND__OFFS  .AND. &
     &           IND_FRQ(J5) .NE. IND__PFICN .AND. &
     &           IND_FRQ(J5) .NE. IND__RFCN  .AND. &
     &           IND_MAIN(J5) == 0           .AND. &
     &           DABS ( FRQ_HEO(IND) - FRQ_ARR(J5) ) .LT. DIF_MIN ) THEN
!
                 DIF_MIN = DABS ( FRQ_HEO(IND) - FRQ_ARR(J5) )
                 IND_MIN = J5
            END IF
!
            IF ( IND_MAIN(J5) == 0           .AND. &
     &           IND_MAIN(J5) == 0           .AND. &
     &           DABS ( FRQ_HEO(IND) - FRQ_ARR(J5) ) .LT. DIF_MIN_ALL ) THEN
!
                 DIF_MIN_ALL = DABS ( FRQ_HEO(IND) - FRQ_ARR(J5) )
            END IF
 450     CONTINUE
         IF ( DIF_MIN < FRQ_MIN*TOL_FRQ  .AND.  IND_MIN > 0 ) THEN
              IF ( DABS(ARR1(J4))* &
     &             DSQRT(1.0D0 - DSIN(2.0D0*MAX(DIF_MIN,FRQ_CMP_TOL)/FRQ_MIN)/ &
     &                               (2.0D0*MAX(DIF_MIN,FRQ_CMP_TOL)/FRQ_MIN) ) < &
     &            FRESEL%NUTAMP_MIN ) THEN
!
! ---------------- Skip such a frequency
!
                   GOTO 440
              END IF
         END IF
!
         N_FRQ = N_FRQ + 1
         FRQ_ARR(N_FRQ) = FRQ_HEO(IND)
         PHS_ARR(N_FRQ) = PHS_HEO(IND)
         ACC_ARR(N_FRQ) = ACC_HEO(IND)
         PMC_ARR(N_FRQ) = PMC_HEO(IND)
         PMS_ARR(N_FRQ) = PMS_HEO(IND)
         AMP_ARR(N_FRQ) = -ARR1(IND)
         IND_FRQ(N_FRQ) = IND
         IF ( DIF_MIN_ALL .LT. FRQ_MIN*TOL_FRQ ) THEN
              IND_MAIN(N_FRQ) = IND_MIN
            ELSE 
              IND_MAIN(N_FRQ) = 0
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
      L_LAB = 0
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
              IF ( IND_MAIN(IND) == 0 ) THEN
                   FRESEL%DAT(J7)%IND_MAIN = 0
                 ELSE 
                   FRESEL%DAT(J7)%IND_MAIN = IND_MAIN(IND)
              END IF
              FRQ_USED(FRESEL%DAT(J7)%IND) = .TRUE.
         END IF
         FRESEL%DAT(J7)%PHS  = PHS_ARR(IND)
         FRESEL%DAT(J7)%FRQ  = FRQ_ARR(IND)
         FRESEL%DAT(J7)%ACCL = ACC_ARR(IND)
         FRESEL%DAT(J7)%PMC  = PMC_ARR(IND)
         FRESEL%DAT(J7)%PMS  = PMS_ARR(IND)
         ITURN = IDNINT ( PHS_ARR(IND)/PI2 )
         FRESEL%DAT(J7)%PHS = FRESEL%DAT(J7)%PHS - ITURN*PI2
         IF ( FRESEL%DAT(J7)%PHS .LT. 0.0D0 ) THEN
              FRESEL%DAT(J7)%PHS = FRESEL%DAT(J7)%PHS + PI2
         END IF
         IF ( IND_FRQ(IND)  == IND__OFFS ) THEN
              FRESEL%DAT(J7)%USE_UT1 = .FALSE.
         END IF
!
         IF ( IVRB .GE. 3 ) THEN
              IF ( J7 .EQ. 1 ) FRQ_LAST = FRESEL%DAT(J7)%FRQ
              WRITE ( 6, 120 ) J7, FRESEL%DAT(J7)%TYP, FRESEL%DAT(J7)%IND, &
     &                         FRESEL%DAT(J7)%PHS, FRESEL%DAT(J7)%FRQ, &
     &                         (FRESEL%DAT(J7)%FRQ - FRQ_LAST)/FRQ_MIN
 120          FORMAT ( I4,') ', I2,' Ind=',I4,' PHS=',F12.10, &
     &                 ' FRQ=',1PD19.12,'  Dif=',0PF8.2 )
         END IF
         FRQ_LAST = FRESEL%DAT(J7)%FRQ
         IF ( FRESEL%DAT(J7)%FRQ .LT. -FRQ_LOW ) THEN
              N_NEG_FRQ = N_NEG_FRQ + 1
         END IF
!
         WRITE ( UNIT=STR(1:11), FMT='(1PD11.4)' ) FRESEL%DAT(J7)%FRQ
         CALL CHASHR ( STR(1:11) )
         STR = STR(1:9)//STR(11:11) 
         IP = LTM_DIF ( 0, L_LAB, LAB_ARR, STR(1:10) )
         IF ( IP > 0 ) THEN
              DO 480 J8=1,26
                 STR(8:8) = CHAR(96+J8)
                 IP = LTM_DIF ( 0, L_LAB, LAB_ARR, STR(1:10) )
                 IF ( IP == 0 ) GOTO 880
 480          CONTINUE 
 880          CONTINUE 
         END IF
         FRESEL%DAT(J7)%LAB = STR(1:10) 
         LAB_ARR(J7) = STR(1:10)
         L_LAB = J7
 470  CONTINUE
!
      DO 490 J9=1,N_FRQ
         IF ( FRESEL%DAT(J9)%IND_MAIN > 0 ) THEN
              DO 4100 J10=1,N_FRQ
                 IF ( FRESEL%DAT(J9)%IND_MAIN == J10 ) GOTO 4100
                 IF ( DABS(FRESEL%DAT(J10)%FRQ - FRQ_ARR(FRESEL%DAT(J9)%IND_MAIN) ) < FRQ_CMP_TOL ) THEN
                      FRESEL%DAT(J9)%IND_MAIN = J10
                      GOTO 890
                 END IF
 4100         CONTINUE 
         END IF
 890     CONTINUE 
 490  CONTINUE 
!
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
           FRESEL%DAT(FRESEL%N_FRQ)%LAB = '-7.2921P-5'
      END IF
!
      FRESEL%NUT_RMS = 0.0D0
      FRESEL%NUT_MAX = 0.0D0
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  FRESEL_HAR  !#!#
