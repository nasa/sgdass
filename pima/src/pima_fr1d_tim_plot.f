      SUBROUTINE PIMA_FR1D_TIM_PLOT ( PIM, IND_OBS, FINAM_PLOT, LTIM, LCHN, &
     &                                LFRQ, DECOR, FREQ_ARR, FREQ_REF, WEI, &
     &                                UV, TIME_FRT, AP_LEN, GR_DEL, PH_RAT, &
     &                                GR_RAT, PH_ACC, PHAS, AMPL, SNR, &
     &                                POL_BAS_LABEL, IDEV, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine PIMA_FR1D_TIM_PLOT 
! *                                                                      *
! * ## 15-JAN-2006  PIMA_FR1D_TIM_PLOT v3.4 (c) L. Petrov 23-APR-2020 ## *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'pima.i'
      INCLUDE   'diagi.i'
      CHARACTER  FINAM_PLOT*(*), POL_BAS_LABEL*(*)
      TYPE     ( PIMA__TYPE  ) :: PIM
      TYPE     ( DIAGI_STRU  ) :: DIA(2)
      INTEGER*4  IND_OBS, LTIM, LCHN, LFRQ, IDEV, IUER
      REAL*8     FREQ_ARR(LCHN,LFRQ), FREQ_REF, TIME_FRT, AP_LEN, GR_DEL, &
     &           PH_RAT, GR_RAT, PH_ACC, PHAS, AMPL, SNR
      COMPLEX*8  UV(PIM%NCHN,LFRQ,LTIM)
      REAL*4     WEI(LTIM)
      INTEGER*4  MARR, MPB
      PARAMETER  ( MARR = PIM__MUV )
      PARAMETER  ( MPB  = 4    )
      REAL*4     PHS_MOD, AMPL_TOT, PHAS_TOT
      REAL*8     ARG(LTIM,LFRQ), VAL(LTIM,LFRQ)
      REAL*8     T8(MARR), X8(MARR), Y8(MARR), GRD_INIT, GRD_STEP, GRD_VAL, &
     &           RAT_INIT, RAT_STEP, RAT_VAL, AMP_MIN, AMP_MAX, &
     &           WPOI, WPOI_TOT, WEI_THR, RMS, TIM_VAL, TIM_SUM, &
     &           PHS_MIN, PHS_MAX, WVR_PHS(MARR), WVR_STA_TIM(MARR,2), &
     &           WVR_STA_PHS(MARR,2), DECOR
      COMPLEX*8  DRF, DRF_TOT
      INTEGER*4  IBST, ILST, IOST, IPST, IWST, IDEV_DEF, ICL1, ICL2, ICL3, &
     &           IND_STA(2), IND_SOU, KP, J1, J2, J3, J4, J5, J6, J7, &
     &           MSEG, KSEG, ICODE, NC, NR, NN, FRG_IND, UV_IND, NO, &
     &           SGN_STA(2), LV(2), IND_BEG, IND_END, IP, BEG_IF, END_IF, IER
      CHARACTER  COMMON_TIT*80, TITS(PIM__MSTA)*80, BUTTON_NAME(MPB)*24, &
     &           BUTTON_LET(MPB)*2, PREF_NAME*128, ZAG*128, UNIT*128, STR*128, &
     &           STR1*32, POL_STR*8, FINAM_TXT_PLOT*128
      CHARACTER*128, ALLOCATABLE :: OUT(:)
      LOGICAL*1  FL_WVR_PLOT
      DATA      ( BUTTON_LET(NN), BUTTON_NAME(NN), NN=1,MPB ) &
     &          / &
     &          '  ', '    ',  &
     &          '  ', '    ',  &
     &          'Qq', 'Quit',  &
     &          '  ', '    '   &
     &          /
      LOGICAL*4, EXTERNAL :: IS_R4_NAN 
      REAL*4,    EXTERNAL :: PHAS_CMPL_R4 
      INTEGER*4, EXTERNAL :: ILEN, I_LEN, IXMN8
      CHARACTER, EXTERNAL :: GET_CDATE*19
#ifdef NO_PLOT
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  PIMA_FR1D_PLOT  !#!  
#else
!
! --- Setting defaults values of the plotting parameters
!
      CALL ERR_PASS   ( IUER, IER )
      CALL DIAGI_DEF  ( IBST, ILST, IOST, IPST, IWST, IDEV_DEF, ZAG, UNIT, &
     &                  ICL1, ICL2, ICL3, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 7591, IUER, 'PIMA_FR1D_TIM_PLOT', 'Error in '// &
     &                    'setting default values for the plot' )
           RETURN
      END IF
      IF ( IDEV == 0 ) IDEV = IDEV_DEF
      PREF_NAME = '/tmp/'
!
      MSEG = PIM%CONF%FRIB_1D_TIM_MSEG
      IF ( PIM%FRG_USE == PIMA__COMBINE ) THEN
           MSEG = MSEG*(PIM%NFRG-1)
      END IF
      WEI_THR  = PIM%CONF%FRIB_WEIGHTS_THRESHOLD
 910  CONTINUE 
!
      KP = 0
      KSEG = 0
      DRF = ( 0.0, 0.0 )
      WPOI = 0.0
      DRF_TOT  = ( 0.0, 0.0 )
      WPOI_TOT = 0.0
      AMP_MIN  =  1.D30
      AMP_MAX  = -1.D30
      RMS = 0.0D0
      TIM_SUM = 0.0D0
      FRG_IND = PIM%OBS(IND_OBS)%REF_FRG_INDS(PIM%CONF%FRQ_GRP)
      IF ( FRG_IND == 0 ) THEN
           WRITE ( 6, * ) 'No uv data for obs ', IND_OBS, ' frequency group ', &
     &                     PIM%CONF%FRQ_GRP
           CALL ERR_LOG ( 0, IUER )
           RETURN 
      END IF
!
      PHS_MIN =  1.0D30
      PHS_MAX = -1.0D30
      AMP_MIN =  1.0D30
      AMP_MAX = -1.0D30
      FL_WVR_PLOT = .FALSE.
      IF ( PIM%CONF%WVR_USE .NE. PIMA__WVR_NO ) THEN
           CALL GETENVAR ( 'PIMAVAR_WVR_PLOT_ONLY', STR )
           IF ( STR == 'YES' ) FL_WVR_PLOT = .TRUE.
      END IF
      IF ( FL_WVR_PLOT ) THEN
           SGN_STA(1) = -1
           SGN_STA(2) =  1
           LV = 0
           DO 510 J1=1,2
              IF ( PIM%STA(PIM%OBS(IND_OBS)%STA_IND(J1))%L_WVR > 0 ) THEN
                   IND_BEG = IXMN8 ( PIM%STA( PIM%OBS(IND_OBS)%STA_IND(J1))%L_WVR, &
     &                               PIM%STA(PIM%OBS(IND_OBS)%STA_IND(J1))%WVR%TIM_ARR, &
     &                               PIM%OBS(IND_OBS)%TIM_BEG )
                   IND_BEG = IND_BEG + 1
                   IND_END = IXMN8 ( PIM%STA(PIM%OBS(IND_OBS)%STA_IND(J1))%L_WVR, &
     &                               PIM%STA(PIM%OBS(IND_OBS)%STA_IND(J1))%WVR%TIM_ARR, &
     &                               PIM%OBS(IND_OBS)%TIM_END )
                   LV(J1) = IND_END - IND_BEG + 1
                   DO 520 J2=1,LV(J1)
                      WVR_STA_TIM(J2,J1) = PIM%STA(PIM%OBS(IND_OBS)%STA_IND(J1))%WVR%TIM_ARR(IND_BEG+J2-1) - &
     &                                     PIM%OBS(IND_OBS)%TIM_BEG
                      WVR_STA_PHS(J2,J1) = PI2*PIM%FREQ_ARR(1,1,PIM%CONF%FRQ_GRP)* &
     &                                     (SGN_STA(J1)*PIM%STA(PIM%OBS(IND_OBS)%STA_IND(J1))%WVR%DEL_ARR(IND_BEG+J2-1) - &
     &                                     PIM%OBS(IND_OBS)%WVR_DEL_AVR)
 520               CONTINUE 
              END IF
 510       CONTINUE 
      END IF
      CALL GETENVAR ( 'PIMAVAR_FR1D_BEG_IF', STR )
      IF ( ILEN(STR) > 0 ) THEN
           CALL CHIN ( STR, BEG_IF )
           WRITE ( 6, '(A,I4)' ) 'PIMA_FR1D_TIM_PLOT  BEG_IF= ', BEG_IF
         ELSE
           BEG_IF = 1
      END IF
      CALL GETENVAR ( 'PIMAVAR_FR1D_END_IF', STR )
      IF ( ILEN(STR) > 0 ) THEN
           CALL CHIN ( STR, END_IF )
           WRITE ( 6, '(A,I4)' ) 'PIMA_FR1D_TIM_PLOT  END_IF= ', END_IF
         ELSE
           END_IF = LFRQ
      END IF
      DO 410 J1=1,LTIM
         IF ( WEI(J1) < WEI_THR ) GOTO 410
         UV_IND  = PIM%OBS(IND_OBS)%UV_IND(J1,FRG_IND)
         IF ( UV_IND == 0 ) THEN
              CALL CLRCH ( STR  )
              CALL CLRCH ( STR1 )
              CALL INCH  ( IND_OBS, STR  )
              CALL INCH  ( J1,      STR1 )
              CALL ERR_LOG ( 7593, IUER, 'PIMA_FR1D_TIM_PLOT', 'Trap of '// &
     &            'internal control: cannot find UV-index for observation '// &
     &             STR(1:I_LEN(STR))//' epoch '//STR1 )
              RETURN
         END IF
         TIM_VAL = PIM%TIM_R8(PIM%UV_IND(UV_IND)%TIM_IND) - &
     &             PIM%OBS(IND_OBS)%TIM_BEG
         IF ( PIM%CONF%DEBUG_LEVEL .EQ. 17 ) THEN
              WRITE ( 6, 210 ) J1, PIM%TIM_R8(PIM%UV_IND(UV_IND)%TIM_IND) - &
     &                         PIM%OBS(IND_OBS)%TIM_BEG, WEI(J1)  
 210          FORMAT ( 'Ap_ind: ', i4, ' Tim: ', F12.6, ' Wei: ', F10.6 ) 
         END IF
!
         DO 430 J3=BEG_IF,END_IF
            DO 440 J4=1,LCHN
               IF ( PIM%BANDPASS_MASK_STYLE .NE. PIMA__NO ) THEN
                    IF ( PIM%BANDPASS_MASK(J4,PIM%CONF%BEG_FRQ+J3-1,PIM%OBS(IND_OBS)%STA_IND(1),PIMA__MASK_FRNG) == 0 .OR. &
                         PIM%BANDPASS_MASK(J4,PIM%CONF%BEG_FRQ+J3-1,PIM%OBS(IND_OBS)%STA_IND(2),PIMA__MASK_FRNG) == 0      ) THEN
                         GOTO 440
                    END IF                
               END IF
               IF ( IS_R4_NAN ( REAL ( UV(J4,J3,J1) ) ) .OR. &
     &              IS_R4_NAN ( IMAG ( UV(J4,J3,J1) ) )      ) THEN
                    WEI(J1) = 0.0
                    UV(J4,J3,J1) = CMPLX ( 0.0, 0.0 )
               END IF
               IF ( WEI(J1) < PIM%CONF%FRIB_WEIGHTS_THRESHOLD ) THEN
                    WEI(J1) = 0.0
                    UV(J4,J3,J1) = CMPLX ( 0.0, 0.0 )
               END IF
               IF ( PIM%CONF%CORR_FLAG_MIN .GE. -2 ) THEN
!
! ----------------- Apply calibration for the correlator flag
!
                    IF ( PIM%OBS(IND_OBS)%CORR_FLAG(J1,FRG_IND) .LE. &
     &                   PIM%CONF%CORR_FLAG_MIN ) THEN
                         WEI(J1) = 0.0
                         UV(J4,J3,J1) = CMPLX ( 0.0, 0.0 )
                    END IF
               END IF
!
! ------------ Check for insane UV values
!
               IF ( REAL ( UV(J4,J3,J1) ) >  PIMA__AMP_MAX .OR. &
     &              REAL ( UV(J4,J3,J1) ) < -PIMA__AMP_MAX .OR. &
     &              IMAG ( UV(J4,J3,J1) ) >  PIMA__AMP_MAX .OR. &
     &              IMAG ( UV(J4,J3,J1) ) < -PIMA__AMP_MAX      ) THEN
                    WEI(J1) = 0.0
                    UV(J4,J3,J1) = CMPLX ( 0.0, 0.0 )
               END IF
!
! ------------ We apply phase rotation due to the phase delay rate and group
! ------------ delay and counter-rotation due to the total phase. 
! ------------ As a result, we will get the residual phase
!
               IF ( GR_RAT < -0.5D0 ) THEN
                    PHS_MOD = - PHAS &
     &                        + PH_RAT*PI2*FREQ_REF*(TIM_VAL - TIME_FRT) &
     &                        + GR_DEL*PI2*(FREQ_ARR(J4,J3)-FREQ_REF) &
     &                        + PH_ACC*PI2*FREQ_REF*(TIM_VAL - TIME_FRT)**2/2.0D0
                  ELSE 
                    PHS_MOD = - PHAS &
     &                        + PH_RAT*PI2*FREQ_REF*(TIM_VAL - TIME_FRT) & 
     &                        + GR_DEL*PI2*(FREQ_ARR(J4,J3)-FREQ_REF) &
     &                        + GR_RAT*PI2*(FREQ_ARR(J4,J3)-FREQ_REF)* &
     &                                     (TIM_VAL - TIME_FRT) 
               END IF
               DRF = DRF + CMPLX(WEI(J1), 0.0)*UV(J4,J3,J1)* &
     &                     CMPLX ( COS(PHS_MOD), SIN(PHS_MOD) )/DECOR
               WPOI = WPOI + WEI(J1)
!
               DRF_TOT = DRF_TOT + WEI(J1)*UV(J4,J3,J1)* &
     &                             CMPLX ( COS(PHS_MOD), SIN(PHS_MOD) )/DECOR
               WPOI_TOT = WPOI_TOT + WEI(J1)
 440        CONTINUE 
 430     CONTINUE 
!
         TIM_SUM = TIM_SUM + TIM_VAL
         KSEG = KSEG + 1
         IF ( KSEG == MSEG  .OR.  J1 == LTIM ) THEN
              KP = KP + 1
              T8(KP) = TIM_SUM/KSEG
              X8(KP) = PHAS_CMPL_R4 ( DRF )
              IF ( X8(KP) >  PI__NUM ) X8(KP) = X8(KP) - PI2
              IF ( X8(KP) < -PI__NUM ) X8(KP) = X8(KP) + PI2
              IF ( WPOI > 1.D-12 ) THEN
                   Y8(KP) = ABS( DRF )/WPOI
              END IF
              IF ( WPOI < 1.D-12 ) THEN
                   KP = KP - 1
              END IF
              RMS = RMS + X8(KP)**2
!
              DRF  = ( 0.0, 0.0 )
              WPOI = 0.0
              KSEG = 0
              TIM_SUM = 0.0D0
!
              PHS_MIN = MIN ( PHS_MIN, X8(KP) )
              PHS_MAX = MAX ( PHS_MAX, X8(KP) )
!
              AMP_MIN = MIN ( AMP_MIN, Y8(KP) )
              AMP_MAX = MAX ( AMP_MAX, Y8(KP) )
         END IF
!
         IF ( FL_WVR_PLOT ) THEN
              WVR_PHS(J1) = PI2*PIM%FREQ_ARR(1,1,PIM%CONF%FRQ_GRP)* &
     &                      (PIM%OBS(IND_OBS)%WVR_DELAY(J1) - PIM%OBS(IND_OBS)%WVR_DEL_AVR)
         END IF
 410  CONTINUE 
!
      IF ( KP > 0 ) THEN
           RMS = DSQRT ( RMS/KP ) 
           AMPL_TOT = ABS(DRF_TOT)/WPOI_TOT
           PHAS_TOT = PHAS_CMPL_R4( DRF_TOT )
         ELSE 
           RMS = 0.0D0
           AMPL_TOT = 0.0
           PHAS_TOT = 0.0
      END IF
      IF ( PIM%CONF%DEBUG_LEVEL .GE. 5 ) THEN
           WRITE ( 6, 220 ) KP, AMPL_TOT, PHAS + PHAS_TOT, RMS
 220       FORMAT ( 'PIMA_FR1D_TIM_PLOT KP = ', I4, ' Ampl_tot: ', F9.6, &
     &              ' Phas_tot: ', F8.5, ' Phas_rms: ', F8.5, ' rad' )
      END IF
!
      IND_STA(1) = PIM%OBS(IND_OBS)%STA_IND(1)
      IND_STA(2) = PIM%OBS(IND_OBS)%STA_IND(2)
      IND_SOU    = PIM%OBS(IND_OBS)%SOU_IND
!
      DO 450 J5=1,2
         CALL NOUT ( SIZEOF(DIA(J5)), DIA(J5) )
         DIA(J5)%IDEV = IDEV
         DIA(J5)%NCLR = 1
         DIA(J5)%NPOI(1)   = KP
         DIA(J5)%ADR_X8(1) = LOC(T8)
         IF ( J5 == 1 ) THEN
              DIA(J5)%ADR_Y8(1) = LOC(X8)
            ELSE 
              DIA(J5)%ADR_Y8(1) = LOC(Y8)
         END IF
         DIA(J5)%ADR_E8(1) = 0
         DIA(J5)%LER(1)    = .FALSE.
         DIA(J5)%ICOL(1)   = J5
         DIA(J5)%IBST(1)   = 0
         DIA(J5)%ILST(1)   = 2
         DIA(J5)%IOST(1)   = 1
         DIA(J5)%IWST(1)   = 1
         DIA(J5)%ICLR      = 1
         IF ( J5 == 1 .AND. FL_WVR_PLOT ) THEN
              DIA(J5)%NCLR = 2
              DIA(J5)%NPOI(2) = KP
              DIA(J5)%ADR_X8(2) = LOC(T8)
              DIA(J5)%ADR_Y8(2) = LOC(WVR_PHS)
              DIA(J5)%ADR_E8(2) = 0
              DIA(J5)%LER(2)    = .FALSE.
              DIA(J5)%ICOL(2)   = 2
              DIA(J5)%IBST(2)   = 0
              DIA(J5)%ILST(2)   = 2
              DIA(J5)%IOST(2)   = 1
              DIA(J5)%IWST(2)   = 2
              DIA(J5)%IPST(2)   = 1
              DIA(J5)%ILST(2)   = 3
!
              IF ( LV(1) > 0 ) THEN
                   DIA(J5)%NCLR = DIA(J5)%NCLR + 1
                   DIA(J5)%NPOI(DIA(J5)%NCLR) = LV(1)
                   DIA(J5)%ADR_X8(DIA(J5)%NCLR) = LOC(WVR_STA_TIM(1,1))
                   DIA(J5)%ADR_Y8(DIA(J5)%NCLR) = LOC(WVR_STA_PHS(1,1))
                   DIA(J5)%ADR_E8(DIA(J5)%NCLR) = 0
                   DIA(J5)%LER(DIA(J5)%NCLR)    = .FALSE.
                   DIA(J5)%ICOL(DIA(J5)%NCLR)   = 3
                   DIA(J5)%IBST(DIA(J5)%NCLR)   = 0
                   DIA(J5)%ILST(DIA(J5)%NCLR)   = 1
                   DIA(J5)%IOST(DIA(J5)%NCLR)   = 1
                   DIA(J5)%IWST(DIA(J5)%NCLR)   = 1
                   DIA(J5)%IPST(DIA(J5)%NCLR)   = 4
                   DIA(J5)%ILST(DIA(J5)%NCLR)   = 2
              END IF
!
              IF ( LV(2) > 1 ) THEN
                   DIA(J5)%NCLR = DIA(J5)%NCLR + 1
                   DIA(J5)%NPOI(DIA(J5)%NCLR) = LV(1)
                   DIA(J5)%ADR_X8(DIA(J5)%NCLR) = LOC(WVR_STA_TIM(1,2))
                   DIA(J5)%ADR_Y8(DIA(J5)%NCLR) = LOC(WVR_STA_PHS(1,2))
                   DIA(J5)%ADR_E8(DIA(J5)%NCLR) = 0
                   DIA(J5)%LER(DIA(J5)%NCLR)    = .FALSE.
                   DIA(J5)%ICOL(DIA(J5)%NCLR)   = 4
                   DIA(J5)%IBST(DIA(J5)%NCLR)   = 0
                   DIA(J5)%ILST(DIA(J5)%NCLR)   = 1
                   DIA(J5)%IOST(DIA(J5)%NCLR)   = 1
                   DIA(J5)%IWST(DIA(J5)%NCLR)   = 1
                   DIA(J5)%IPST(DIA(J5)%NCLR)   = 4
                   DIA(J5)%ILST(DIA(J5)%NCLR)   = 2
              END IF
         END IF
         IF ( J5 == 1 ) THEN
              DIA(J5)%IPST(1)=  5
              DIA(J5)%ILST(1) = 2
              IF ( KP > 0 ) THEN
                   DIA(J5)%XMIN   =  0.0    - (T8(KP) - T8(1))*DIAGI_FIE 
                   DIA(J5)%XMAX   =  T8(KP) + (T8(KP) - T8(1))*DIAGI_FIE 
                   DIA(J5)%YMIN   = -PI__NUM 
                   DIA(J5)%YMAX   =  PI__NUM 
                 ELSE 
                   DIA(J5)%XMIN   =  0.0
                   DIA(J5)%XMAX   =  1.0
                   DIA(J5)%YMIN   =  0.0
                   DIA(J5)%YMAX   =  1.0
              END IF
            ELSE IF ( J5 == 2 ) THEN
              DIA(J5)%IPST(1) = 4
              DIA(J5)%ILST(1) = 2
              IF ( KP > 0 ) THEN
                   DIA(J5)%XMIN    = 0.0     - (T8(KP)  - T8(1))*DIAGI_FIE 
                   DIA(J5)%XMAX    = T8(KP)  + (T8(KP)  - T8(1))*DIAGI_FIE 
                   DIA(J5)%YMIN    = 0.0     - (AMP_MAX - AMP_MIN)*DIAGI_FIE 
                   DIA(J5)%YMAX    = AMP_MAX + (AMP_MAX - AMP_MIN)*DIAGI_FIE 
                 ELSE 
                   DIA(J5)%XMIN   =  0.0
                   DIA(J5)%XMAX   =  1.0
                   DIA(J5)%YMIN   =  0.0
                   DIA(J5)%YMAX   =  1.0
             END IF
         END IF
!
         CALL CLRCH ( STR ) 
         CALL INCH  ( MSEG, STR )
         DIA(J5)%ARG_UNITS = 'Mseg: '//STR(1:I_LEN(STR))//' APs  Time in sec'
         CALL CLRCH ( STR ) 
         CALL INCH  ( IND_OBS, STR )
         DIA(J5)%NAME   = FINAM_PLOT
         DIA(J5)%ITRM   = 0
         IF ( IDEV == 1  .OR. &
     &        IDEV == 2  .OR. &
     &        IDEV == 3  .OR. &
     &        IDEV == 4  .OR. &
     &        IDEV == 5  .OR. &
     &        IDEV == 6       ) THEN
              DIA(J5)%IBATCH = 0
            ELSE IF ( IDEV == 100 ) THEN
              IP = ILEN(FINAM_PLOT)
              FINAM_PLOT = FINAM_PLOT(1:IP-1)//'.sav'
              DIA%NAME   = FINAM_PLOT
              DIA(J5)%IBATCH = 2
            ELSE 
              DIA(J5)%IBATCH = 1
         END IF
         DIA(J5)%STATUS = DIA__DEF
         IF ( PIM%NPOL == 2 ) THEN 
              POL_STR = ' '//POL_BAS_LABEL//'-pol'
            ELSE 
              CALL CLRCH ( POL_STR )
         END IF
         IF ( J5 == 1 ) THEN
              TITS(J5) = PIM%CONF%BAND//'-band'// &
     &                   POL_STR(1:I_LEN(POL_STR))//' fringe phase for '// &
     &                   PIM%SOU(IND_SOU)%IVS_NAME// &
     &                   ' at '//PIM%STA(IND_STA(1))%IVS_NAME// &
     &                   '/'//PIM%STA(IND_STA(2))%IVS_NAME// &
     &                   ' in '//PIM%OBS_CODE
              DIA(J5)%ZAG  = PIM%CONF%BAND//'-band'// &
     &                       POL_STR(1:I_LEN(POL_STR))//' fringe phase for '// &
     &                       PIM%SOU(IND_SOU)%IVS_NAME// &
     &                       ' at '//PIM%STA(IND_STA(1))%IVS_NAME// &
     &                       '/'//PIM%STA(IND_STA(2))%IVS_NAME
            ELSE 
              TITS(J5) = PIM%CONF%BAND//'-band'// &
     &                   POL_STR(1:I_LEN(POL_STR))//' fringe amplitude for '// &
     &                   PIM%SOU(IND_SOU)%IVS_NAME// &
     &                   ' at '//PIM%STA(IND_STA(1))%IVS_NAME// &
     &                   '/'//PIM%STA(IND_STA(2))%IVS_NAME// &
     &                   ' in '//PIM%OBS_CODE
              DIA(J5)%ZAG = PIM%CONF%BAND//'-band'// &
     &                      POL_STR(1:I_LEN(POL_STR))//' fringe amplitude for '// &
     &                      PIM%SOU(IND_SOU)%IVS_NAME// &
     &                      ' at '//PIM%STA(IND_STA(1))%IVS_NAME// &
     &                      '/'//PIM%STA(IND_STA(2))%IVS_NAME
         END IF
!
         IF ( IDEV == -1 .AND. KP > 0 ) THEN
!
! ----------- Plot is written as a text file. In fact, we write two text
! ----------- tables: one for phase (J5=1) and another for amplitude (J5=2)
!
              IF ( J5 == 1 ) THEN
                   FINAM_TXT_PLOT = FINAM_PLOT(1:I_LEN(FINAM_PLOT))//'phs.txt'
                 ELSE IF ( J5 == 2 ) THEN
                   FINAM_TXT_PLOT = FINAM_PLOT(1:I_LEN(FINAM_PLOT))//'amp.txt'
              END IF
!
! ----------- Allocate memory for temporary buffer
!
              ALLOCATE   ( OUT(KP+32), STAT=IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 7594, IUER, 'PIMA_FR1D_TIM_PLOT', 'Failure '// &
     &                            'to allocate memory for internal buffer '// &
     &                            'of the output text file' )
                   DEALLOCATE ( OUT )
                   RETURN 
              END IF
!
! ----------- Prepare table preamble
!
              OUT(1) = PIMA__TXT1D_LABEL
              OUT(2) = '#'
              OUT(2) = '# Plot of fringe phase and fringe amplitude versus frequency'
              OUT(2) = '#'
              OUT(3) = '# Generated on '//GET_CDATE()
              OUT(4) = '# Generated by '//PIMA__LABEL
              OUT(5) = '#'
!
! ----------- Write into the table auxiliary information
!
              NO = 5
              CALL CLRCH ( STR  ) 
              CALL CLRCH ( STR1 ) 
              CALL INCH  ( IND_OBS, STR1 )
              WRITE ( UNIT=STR(1:8), FMT='(F8.1)' ) SNR
              CALL CHASHL ( STR )
!
              IF ( J5 == 1 ) THEN
                   STR = 'Fringe phase for obs #'//STR1(1:I_LEN(STR1))// &
     &                   ' Exp '//PIM%OBS_CODE(1:I_LEN(PIM%OBS_CODE))//' SNR='// &
     &                   STR(1:ILEN(STR))
                 ELSE IF ( J5 == 2 ) THEN
                   STR = 'Fringe ampl for obs #'//STR1(1:I_LEN(STR1))// &
     &                   ' Exp '//PIM%OBS_CODE(1:I_LEN(PIM%OBS_CODE))//' SNR='// &
     &                   STR(1:ILEN(STR))
              END IF
              NO = NO + 1; OUT(NO) = 'PLOT_TITLE:  '//STR
              CALL INCH  ( MSEG, STR )
              NO = NO + 1; OUT(NO) = 'SUBTITLE:    Mseg: '//STR
              CALL INCH ( KP, STR )
              NO = NO + 1; OUT(NO) = 'NUM_POINTS:  '//STR
              NO = NO + 1; OUT(NO) = 'AXIS1_TITLE: Time'
              NO = NO + 1; OUT(NO) = 'AXIS1_UNITS: s'
!
              WRITE ( UNIT=STR(1:15), FMT='(F15.1)' ) T8(1)  
              CALL CHASHL ( STR )
              NO = NO + 1; OUT(NO) = 'AXIS1_MIN:   '//STR(1:15)
              WRITE ( UNIT=STR(1:15), FMT='(F15.1)' ) T8(KP)
              CALL CHASHL ( STR )
              NO = NO + 1; OUT(NO) = 'AXIS1_MAX:   '//STR(1:15)
!
              IF ( J5 == 1 ) THEN
                   NO = NO + 1; OUT(NO) = 'AXIS2_NAME:  Fringe phase'
                   NO = NO + 1; OUT(NO) = 'AXIS2_UNITS: rad'
                   WRITE ( UNIT=STR(1:15), FMT='(F10.7)' ) PHS_MIN
                   CALL CHASHL ( STR )
                   NO = NO + 1; OUT(NO) = 'AXIS2_MIN:   '//STR(1:15)
                   WRITE ( UNIT=STR(1:15), FMT='(F10.7)' ) PHS_MAX
                   CALL CHASHL ( STR )
                   NO = NO + 1; OUT(NO) = 'AXIS2_MAX:   '//STR(1:15)
                 ELSE IF ( J5 == 2 ) THEN
                   NO = NO + 1; OUT(NO) = 'AXIS2_NAME:  Normalized fringe ampitude'
                   NO = NO + 1; OUT(NO) = 'AXIS2_UNITS: dimensionless'
                   WRITE ( UNIT=STR(1:15), FMT='(F10.7)' ) AMP_MIN
                   CALL CHASHL ( STR )
                   NO = NO + 1; OUT(NO) = 'AXIS2_MIN:   '//STR(1:15)
                   WRITE ( UNIT=STR(1:15), FMT='(F10.7)' ) AMP_MAX
                   CALL CHASHL ( STR )
                   NO = NO + 1; OUT(NO) = 'AXIS2_MAX:   '//STR(1:15)
              END IF
              NO = NO + 1; OUT(NO) = 'AXIS2_ERRS:  no'
!
! ----------- Write the agrguments and values of plotting variables
!
              DO 460 J6=1,KP
                 NO = NO + 1
                 IF ( J5 == 1 ) THEN
                      WRITE  ( OUT(NO), 110 ) J6, T8(J6), X8(J6)
                    ELSE IF ( J5 == 2 ) THEN
                      WRITE  ( OUT(NO), 110 ) J6, T8(J6), Y8(J6)
                 END IF
 110             FORMAT ( 'POINT: ', I5, 1X, F11.5, 1X, F10.7 )
 460          CONTINUE 
              NO = NO + 1; OUT(NO) = '#'
              NO = NO + 1; OUT(NO) = PIMA__TXT1D_LABEL
!
! ----------- Write the buffer into file
!
              CALL ERR_PASS ( IUER, IER )
              CALL WR_TEXT  ( NO, OUT, FINAM_TXT_PLOT, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 7595, IUER, 'PIMA_FR1D_TIM_PLOT', 'Failure '// &
     &                            'to write into the output table file '// &
     &                            FINAM_PLOT )
                   DEALLOCATE ( OUT )
                   RETURN 
              END IF
              WRITE ( 6, * ) 'File written '//FINAM_TXT_PLOT(1:I_LEN(FINAM_TXT_PLOT))
!
! ----------- Finally, do not forget to deallocate the buffer
!
              DEALLOCATE ( OUT )
         END IF         
 450  CONTINUE 
      IF ( IDEV .NE. -1 ) THEN
           NC = 1
           NR = 2
           BUTTON_LET(1)  = 'Ss'
           BUTTON_NAME(1) = 'Segment length'
           BUTTON_LET(2)  = 'Xx'
           BUTTON_NAME(2) = 'Exit plot'
           BUTTON_LET(3)  = 'Qq'
           BUTTON_NAME(3) = 'Quit'
!
           CALL CLRCH ( STR  ) 
           CALL CLRCH ( STR1 ) 
           CALL INCH  ( IND_OBS, STR1 )
           WRITE ( UNIT=STR(1:8), FMT='(F8.1)' ) SNR
           CALL CHASHL ( STR )
           COMMON_TIT = 'Fringe phs/amp for obs #'//STR1(1:I_LEN(STR1))// &
     &                  ' Exp '//PIM%OBS_CODE(1:I_LEN(PIM%OBS_CODE))//' SNR='// &
     &                  STR(1:ILEN(STR))
!
           ICODE = 0
           CALL ERR_PASS ( IUER, IER )
           CALL MULTI_DIAGI ( COMMON_TIT, 2, NC, NR, TITS, MPB, &
     &                        BUTTON_NAME, BUTTON_LET, PREF_NAME, DIA, &
     &                        ICODE, IER )
           IF ( ICODE == 1 ) THEN
                WRITE ( 6, '(A,I4,A)' ) 'Current segment length: ',MSEG, ' Aps'
                WRITE ( 6, '(A,$)'  ) 'New segment length >> '
                READ  ( 5, '(A)'    )  STR
                CALL CHIN ( STR, MSEG )
                IF ( MSEG .LE. 1 ) MSEG = 1 
                GOTO 910
           END IF
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 7596, IUER, 'PIMA_FR1D_TIM_PLOT', 'Failure to '// &
     &              'make a plot of autocorrelation' )
                RETURN 
           END IF
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  PIMA_FR1D_TIM_PLOT  !#!#
#endif
