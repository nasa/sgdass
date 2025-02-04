      SUBROUTINE PIMA_PBP_ACCUM ( PIM, VTD, BPS, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine PIMA_PBP_ACCUM  computes polarization bandpass using       *
! *   accumulation algorithm. It is asssumed that the init algorithm     *
! *   has already run successfully.                                      *
! *                                                                      *
! *  ### 29-AUG-2010  PIMA_PBP_ACCUM  v4.3 (c) L. Petrov 16-DEC-2023 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'astro_constants.i'
      INCLUDE   'pima.i'
      INCLUDE   'vtd.i'
      TYPE     ( PIMA__TYPE         ) :: PIM
      TYPE     ( VTD__TYPE          ) :: VTD
      TYPE     ( PIM_BPS_STA__TYPE  ) :: BPS
      INTEGER*4  IUER
      CHARACTER  SEARCH_MODE*8, STR*128, STR1*128
      COMPLEX*8  RES_AVR(4), DRF_REM, DRF_ARR(PIM__MCHN,PIM__MPLR)
      COMPLEX*8, ALLOCATABLE :: RES(:,:,:)
      REAL*4,    ALLOCATABLE :: AC_AVR(:,:,:), &
     &                          ACC_REM_AMPL(:,:,:), ACC_REM_PHAS(:,:,:), WEI_REM(:,:,:), &
     &                          ACC_REF_AMPL(:,:),   ACC_REF_PHAS(:,:),   WEI_REF(:,:)
      CHARACTER  POLAR_SAVE*8, POLAR_STYLE*8, POL_CONF*7
      INTEGER*4  J1, J2, J3, J4, J5, J6, J7, J8, J9, J10, J11, J12, J13, &
     &           J14, J15, J16, J17, J18, J19, J20, J21, J22, &
     &           IND_STA(2), IS1(2), IS2(2), IND_REF, IND_REM, LCHN, LFRQ, LTIM, &
     &           IFRQ, ITURN, MOD_AMB, IND_FRA, KCHN, L_OBS, IND_OBS, &
     &           NSEG_USED, NSEG_USED_IF, IP, ICHN, KCHN_ARR(PIM__MCHN,PIM__MFRQ), IAMB, &
     &           POL_MODE, IND_POL, KP, KP2, K1, K2, K3, IER
      REAL*8     SNR(2), PH_RAT(PIM__MFRA), GR_DEL(PIM__MFRA), PHS(PIM__MFRA), &
     &           WEI_CHN(PIM__MCHN), &
     &           POL_COEF_AMP(0:PIM__MPOL), POL_COEF_PHS(0:PIM__MPOL), &
     &           POL_COEF_ERR(0:PIM__MPOL), DR_AMP, SH_AMP, DR_PHS, SH_PHS, &
     &           PHAS_RMS_MAX, FREQ_REF
      REAL*4     FEED_ANG_DIF, EPS
      PARAMETER  ( PHAS_RMS_MAX = 1.0D0 )
      PARAMETER  ( EPS = 1.0E-5 ) 
      REAL*8     T1(8192), T2(8192), T3(8192), T4(8192), T5(8192), T6(8192), &
     &           X1(8192), X2(8192), X3(8192), X4(8192), X5(8192), X6(8192), &
     &           Y1(8192), Y2(8192), Y3(8192), Y4(8192), Y5(8192), Y6(8192) 
      INTEGER*1  MASK_CHN, SGN_REM, SGN_REF
      LOGICAL*1  FL_BMASK, FL_SKIP
      LOGICAL*4  FL_AMBIG_RES, FL_PLOT
      REAL*4     WEI__MIN
      PARAMETER  ( WEI__MIN = 1.E-14 ) ! Minumum weight
      REAL*4     WEI_SUM, NSUM, PHAS, AMPL, &
     &           AMPL_RES(PIM__MCHN,2), AMPL_MOD(PIM__MCHN,PIM__MFRQ,2), &
     &           PHAS_RES(PIM__MCHN,2), PHAS_MOD(PIM__MCHN,PIM__MFRQ,2), &
     &           PHAS_AVR, AMPL_AVR, PHAS_RMS, AMPL_RMS, &
     &           FREQ_CHN(PIM__MCHN), &
     &           AMPL_OUT(PIM__MCHN,PIM__MFRQ,2), PHAS_OUT(PIM__MCHN,PIM__MFRQ,2), &
     &           AMPL_REF_OUT(PIM__MCHN), PHAS_REF_OUT(PIM__MCHN), &                     
     &           FREQ_ARR_R4(PIM__MCHN,PIM__MFRQ), BPS_AMP, BPS_AMP_AVR, &
     &           PHAS_ADD, AMP_ACC, &
     &           AMPL_FRQ_NRML(PIM__MFRQ), AMPL_BAND_NRML, &
     &           AMPL_FRQ_AVR(PIM%NFRQ), AMPL_FRQ_RMS(PIM%NFRQ), &
     &           AMPL_TOT_RMS, AMPL_INTEGRAL, PHA_OFF(2)
   complex*8 cmpl_save(pim%nchn,pim%nfrq,pim%nsta) ! %%%
   real*4    p1, p2, p3, p4, s1, s2, s3, s4
      REAL*4,    EXTERNAL :: PHAS_CMPL_R4
      REAL*8,    EXTERNAL :: LEGENDRE_POL
      INTEGER*4, EXTERNAL :: ILEN, I_LEN
      CHARACTER, EXTERNAL :: PIMA_GET_POL_CONF*7
!
      FL_PLOT = .FALSE.
      FL_AMBIG_RES = .FALSE.
      CALL GETENVAR ( 'PIMAVAR_PBP_AMBIG_RES', STR )
      CALL TRAN ( 11, STR, STR )
      IF ( STR == 'YES' ) FL_AMBIG_RES = .TRUE.
      IF ( STR == 'NO'  ) FL_AMBIG_RES = .FALSE.
!
      LCHN = PIM%NCHN
      LFRQ = PIM%CONF%END_FRQ - PIM%CONF%BEG_FRQ + 1
      IF ( PIM%CONF%BPS_MSEG_ACCUM .LE. 0 ) PIM%CONF%BPS_MSEG_ACCUM = 1
      KCHN = PIM%NCHN/PIM%CONF%BPS_MSEG_ACCUM
!
      IF ( PIM%CONF%DEBUG_LEVEL .GE. 2 ) THEN
           WRITE ( 6, * ) '--------------------------- ' 
           WRITE ( 6, * ) ' PIMA_PBP_ACCUM                ' 
           WRITE ( 6, * ) '--------------------------- ' 
      END IF
!
      IF ( FL_AMBIG_RES ) THEN
           MOD_AMB = 2
         ELSE 
           MOD_AMB = 1  ! 1 for production mode; 11 for test mode 
      END IF
!
      IF ( PIM%CONF%FRIB_FINE_SEARCH == PIMA__FINE_SEARCH_LSQ ) THEN
           IND_FRA = PIMA__LSQ
         ELSE
           IND_FRA = PIMA__DRF
      END IF
!
      ALLOCATE ( RES(LCHN,PIM%NFRQ,4), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR  )
           CALL INCH  ( LCHN*PIM%NFRQ*4*8, STR )
           CALL ERR_LOG ( 6251, IUER, 'PIMA_PBP_ACCUM', 'Error in an '// &
     &         'attempt to allocate '//STR(1:I_LEN(STR))//' bytes of '// &
     &         'dynamic memory for residuals' )
           RETURN
      END IF
      RES = CMPLX(0.0,0.0)
!
      ALLOCATE ( AC_AVR(LCHN,LFRQ,4), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR  )
           CALL INCH  ( 8*LCHN*LFRQ*4, STR )
           CALL ERR_LOG ( 6252, IUER, 'PIMA_PBP_ACCUM', 'Error in an '// &
     &         'attempt to allocate '//STR(1:I_LEN(STR))//' bytes of '// &
     &         'dynamic memory for array autospectrum AC_AVR' )
           GOTO 710
      END IF
      AC_AVR = 0.0
!
      ALLOCATE ( ACC_REM_AMPL(LCHN,LFRQ,PIM%NSTA), WEI_REM(LCHN,LFRQ,PIM%NSTA), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR  )
           CALL INCH  ( 2*4*LCHN*LFRQ*PIM%NSTA, STR )
           CALL ERR_LOG ( 6253, IUER, 'PIMA_PBP_ACCUM', 'Error in an '// &
     &         'attempt to allocate '//STR(1:I_LEN(STR))//' bytes of '// &
     &         'dynamic memory for the accumalted bandpass amplitude' )
           GOTO 710
      END IF
      ACC_REM_AMPL = 1.0
      WEI_REM      = 0.0
!
      ALLOCATE ( ACC_REM_PHAS(LCHN,LFRQ,PIM%NSTA), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR  )
           CALL INCH  ( 4*LCHN*LFRQ*PIM%NSTA, STR )
           CALL ERR_LOG ( 6254, IUER, 'PIMA_PBP_ACCUM', 'Error in an '// &
     &         'attempt to allocate '//STR(1:I_LEN(STR))//' bytes of '// &
     &         'dynamic memory for the accumalted bandpass phase' )
           GOTO 710
      END IF
      ACC_REM_PHAS = 0.0
      ALLOCATE ( ACC_REF_AMPL(LCHN,LFRQ), WEI_REF(LCHN,LFRQ), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR  )
           CALL INCH  ( 2*4*LCHN*LFRQ, STR )
           CALL ERR_LOG ( 6255, IUER, 'PIMA_PBP_ACCUM', 'Error in an '// &
     &         'attempt to allocate '//STR(1:I_LEN(STR))//' bytes of '// &
     &         'dynamic memory for the accumalted bandpass amplitude '// &
     &         'of the reference station' )
           GOTO 710
      END IF
      ACC_REF_AMPL = 1.0
      WEI_REF      = 0.0
!
      ALLOCATE ( ACC_REF_PHAS(LCHN,LFRQ), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR  )
           CALL INCH  ( 4*LCHN*LFRQ, STR )
           CALL ERR_LOG ( 6256, IUER, 'PIMA_PBP_ACCUM', 'Error in an '// &
     &         'attempt to allocate '//STR(1:I_LEN(STR))//' bytes of '// &
     &         'dynamic memory for the accumalted bandpass phase '// &
     &         'of the reference station' )
           GOTO 710
      END IF
      ACC_REF_PHAS = 0.0
!
      IF ( PIM%CONF%BANDPASS_MASK_FILE .NE. PIMA__BPASS_NO  .AND. &
     &     ASSOCIATED ( PIM%BANDPASS_MASK )                       ) THEN
           FL_BMASK = .TRUE.
         ELSE
           FL_BMASK = .FALSE.
      END IF
      FREQ_REF = PIM%FRQ(PIM%CONF%BEG_FRQ,PIM%CONF%FRQ_GRP)%FREQ
      PHAS_RES = 0.0
      PHAS_MOD = 0.0
      PHAS_OUT = 0.0
!
      DO 410 J1=1,PIM%NSTA
         IF ( .NOT. ASSOCIATED ( PIM%PBP(J1)%CMPL ) ) GOTO 410
         IF ( POL_CONF == PIMA__PC_CC ) THEN
              IF ( J1 == BPS%IND_STA_REF ) THEN
                   GOTO 410
              END IF
         END IF
!%%         DO 420 J2=1,2 ! par/crs polarization combination
         DO 420 J2=1,1 ! par/crs polarization combination
            IF ( BPS%NUM_OBS_ACCUM(J1,J2) == 0 ) THEN
                 IF ( J2 == 1 .AND. PIM%CONF%DEBUG_LEVEL .GE. 2 ) THEN
                      IF ( PIM%C_STA(J1) < PIM%C_STA(BPS%IND_STA_REF) ) THEN
                           WRITE ( 6, '(A)' ) 'PIMA_PBP_INIT: No suitable observations at baseline '// &
     &                                         PIM%C_STA(J1)//' / '//PIM%C_STA(BPS%IND_STA_REF)
                        ELSE IF ( PIM%C_STA(J1) > PIM%C_STA(BPS%IND_STA_REF) ) THEN
                            WRITE ( 6, '(A)' ) 'PIMA_PBP_INIT: No suitable observations at baseline '// &
     &                                          PIM%C_STA(BPS%IND_STA_REF)//' / '//PIM%C_STA(J1)
                      END IF
                 END IF
                 GOTO 420
             END IF
!
! ---------- Cycle over observations
!
             L_OBS = 0
             WEI_SUM = 0.0
             DO 430 J3=1,PIM%BPS%NUM_OBS_ACCUM(J1,J2)+1
                IND_OBS = BPS%IND_OBS_SEL(J3,J1,J2)
                IF ( IND_OBS == 0 ) GOTO 430
                IF ( PIM%OBS(IND_OBS)%REF_FRG_INDS(PIM%CONF%FRQ_GRP) == 0 ) THEN
                     GOTO 430
                END IF
                IF ( DABS(BPS%SNR(J3,J1,J2)) < PIM%CONF%BPS_SNR_MIN_ACCUM ) GOTO 430
!
                LTIM = PIM%OBS(IND_OBS)%NUM_EPC(PIM%OBS(IND_OBS)%REF_FRG_INDS(PIM%CONF%FRQ_GRP))
!
                CALL ERR_PASS ( IUER, IER )
                POL_CONF = PIMA_GET_POL_CONF ( PIM, IND_OBS, IER ) 
                IF ( IER .NE. 0 ) THEN
                     CALL ERR_LOG ( 6257, IUER, 'PIMA_PBP_ACCUM', 'Error in '// &
     &                   'an attempt to get polarizartion code' )
                     RETURN 
                END IF
!
                IND_STA(1) = PIM%OBS(IND_OBS)%STA_IND(1)
                IND_STA(2) = PIM%OBS(IND_OBS)%STA_IND(2)
                IF ( IND_STA(1) == BPS%IND_STA_REF ) THEN
                     IND_REF =  1
                     IND_REM =  2
                     SGN_REM =  1
                     SGN_REF = -1
                  ELSE
                     IND_REF =  2
                     IND_REM =  1
                     SGN_REM = -1
                     SGN_REF =  1
                END IF
                RES = 0.0
!
! ------------- Summary:
! ------------- 1) bandpass phases over frequency channels are flat
! ------------- 2) ALL phase cal tones are applied
! ------------- 3) pcal_rate is used for pcal ambiguity resolution
! ------------- 4) We have BPASS%PH_RAT that allows to resolve ambiguity
!
                POLAR_SAVE = PIM%CONF%POLAR
                IF ( POLAR_SAVE .NE. PIMA__POLAR_I ) THEN
                     CALL ERR_LOG ( 6258, IUER, 'PIMA_PBP_ACCUM', 'Trap '// &
     &                   'of internal control: polarization '// &
     &                   PIM%CONF%POLAR//' is not suitable '// &
     &                   ' for polarization bandpass computation, only '// &
     &                   ' I is supported' )
                     GOTO 710
                END IF
!
                SEARCH_MODE = PIMA__2FFT 
                IF ( J2 == 1 ) THEN
                     POL_MODE = PIMA__PAR
                   ELSE
                     POL_MODE = PIMA__PALL_MIXED
                END IF
!
                IND_POL = 1
                CALL ERR_PASS  ( IUER, IER )
                CALL PIMA_GET_RESID ( PIM, VTD, IND_OBS, IND_POL, &
     &                     LCHN, LFRQ, LTIM, &
     &                     GR_DEL, PH_RAT, BPS%GR_RAT(J3,J1,J2), PHS, &
     &                     BPS%SNR(J3,J1,J2), BPS%TIME_FRT(J3,J1,J2), &
     &                     SEARCH_MODE, PIMA__BPASS_PHS, POL_MODE, &
     &                     RES, RES_AVR, SNR, IER )
                IF ( IER .NE. 0 ) THEN
                     CALL CLRCH ( STR )
                     CALL INCH  ( BPS%IND_OBS_SEL(J3,J1,J2), STR )
                     CALL ERR_LOG ( 6260, IUER, 'PIMA_PBP_ACCUM', 'Error in '// &
     &                   'an attempt to compute residuals for observation '// &
     &                    STR )
                     GOTO 710
                END IF
!
                WEI_SUM = WEI_SUM + SNR(1)
!
! ------------- Now compute complex gain of the opposite polarization:
! ------------- RES_opp = GAIN*RES_dir
!
                L_OBS = L_OBS + 1
                PHAS_RMS = 0.0
                AMPL_RMS = 0.0
                IFRQ = 0
                NSEG_USED = 0
                FEED_ANG_DIF =   PIM%OBS(IND_OBS)%FEED_ANG(1) &
     &                         - PIM%OBS(IND_OBS)%FEED_ANG(2) 
                KP  = 0
                KP2 = 0
                DO 460 J6=PIM%CONF%BEG_FRQ,PIM%CONF%END_FRQ
                   IFRQ = IFRQ + 1
                   ICHN = 0
!
! ----------------- Compute segment averaged residual polarization bandpass
!
                    DRF_ARR = 0.0
                    DO 470 J7=1,KCHN ! cycle over spectral segments
                       DRF_REM = 0.0
                       WEI_CHN(J7)  = 0.0
                       FREQ_CHN(J7) = 0.0
                       DO 480 J8=1,PIM%CONF%BPS_MSEG_ACCUM ! cycle over the spectral channales of the segment
                          ICHN = ICHN + 1
                          IF ( FL_BMASK ) THEN
                               MASK_CHN = PIM%BANDPASS_MASK(ICHN,J6,IND_STA(1),PIMA__MASK_BPAS) * &
     &                                    PIM%BANDPASS_MASK(ICHN,J6,IND_STA(2),PIMA__MASK_BPAS)
                             ELSE
                               MASK_CHN = 1
                          END IF
!
! ----------------------- Second dimension: 1 -- direct polariation, 2 -- opposite polarizaiton
!
                          DRF_ARR(J7,1) = DRF_ARR(J7,1) + MASK_CHN*RES(ICHN,IFRQ,1)
                          DRF_ARR(J7,2) = DRF_ARR(J7,2) + MASK_CHN*RES(ICHN,IFRQ,2)
                          FREQ_CHN(J7) = FREQ_CHN(J7) + MASK_CHN*PIM%FREQ_ARR(ICHN,J6,PIM%CONF%FRQ_GRP)
                          WEI_CHN(J7)  = WEI_CHN(J7)  + MASK_CHN
                          FREQ_ARR_R4(ICHN,J6) = PIM%FREQ_ARR(ICHN,J6,PIM%CONF%FRQ_GRP)
 480                   CONTINUE
!
! -------------------- Compute the segment averaged frequency, amplitude and phase
! -------------------- of the residuals
!
                       IF ( WEI_CHN(J7) > 0.0 ) THEN
                            FREQ_CHN(J7) = FREQ_CHN(J7)/WEI_CHN(J7)  
                            AMPL_RES(J7,1) = ABS ( DRF_ARR(J7,1) )/WEI_CHN(J7)
                            AMPL_RES(J7,2) = ABS ( DRF_ARR(J7,2) )/WEI_CHN(J7)
                            PHAS_RES(J7,1) = PHAS_CMPL_R4 ( DRF_ARR(J7,1) ) - PHS(1)
                            PHAS_RES(J7,2) = PHAS_CMPL_R4 ( DRF_ARR(J7,2) ) - PHS(1)
                            IF ( PHAS_RES(J7,1) < -PI__NUM ) PHAS_RES(J7,1) = PHAS_RES(J7,1) + PI2
                            IF ( PHAS_RES(J7,2) < -PI__NUM ) PHAS_RES(J7,2) = PHAS_RES(J7,2) + PI2
                            IF ( PHAS_RES(J7,1) >  PI__NUM ) PHAS_RES(J7,1) = PHAS_RES(J7,1) - PI2
                            IF ( PHAS_RES(J7,2) >  PI__NUM ) PHAS_RES(J7,2) = PHAS_RES(J7,2) - PI2
                          ELSE
                            FREQ_CHN(J7) = PIM%FREQ_ARR(ICHN,J6,PIM%CONF%FRQ_GRP)
                            WEI_CHN(J7)  = 0.0
                            AMPL_RES(J7,1) = 0.0
                            PHAS_RES(J7,1) = 0.0
                            AMPL_RES(J7,2) = 0.0
                            PHAS_RES(J7,2) = 0.0
                            DRF_ARR(J7,1:PIM__MPLR) = 0.0
                       END IF
 470                CONTINUE
!
                    IF ( PIM%CONF%BPS_INTRP_METHOD == PIMA__LEGENDRE ) THEN
                         DO 490 J9=1,2
                            CALL ERR_PASS ( IUER, IER )
                            CALL BPASS_MOD_POLY ( MOD_AMB, DBLE(PIMA__AMP_MIN), &
     &                              PIM%CONF%BPS_DEG_AMP, PIM%CONF%BPS_DEG_PHS, &
     &                              KCHN, FREQ_CHN, PHAS_RES(1,J9), AMPL_RES(1,J9), &
     &                                    PHAS_MOD(1,J6,J9), AMPL_MOD(1,J6,J9), &
     &                              PIM%NCHN, FREQ_ARR_R4(1,J6), &
     &                                    PHAS_OUT(1,J6,J9), AMPL_OUT(1,J6,J9), IER )
                            IF ( IER .NE. 0 ) THEN
                                 CALL CLRCH ( STR )
                                 CALL INCH  ( IND_OBS, STR )
                                 CALL ERR_LOG ( 6262, IUER, 'PIMA_PBP_ACCUM', 'Failure '// &
     &                              'to compute interpolating polynomial for '// &
     &                              'bandpass phases for remote station '//PIM%C_STA(J1)// &
     &                              ' observation '//STR )               
                                 GOTO 710
                            END IF
 490                     CONTINUE 
                      ELSE IF ( PIM%CONF%BPS_INTRP_METHOD == PIMA__SPLINE ) THEN
!
! ---------------------- ... Using expansion into the B-spline basis
!
                         DO 4100 J10=1,2
                            CALL ERR_PASS ( IUER, IER )
                            CALL BPASS_MOD_SPLINE ( MOD_AMB, &
     &                                 PIM%CONF%BPS_AMP_MIN*BPS%AMPL(J3,J1,J2), &
     &                                 PIM%CONF%BPS_DEG_AMP, &
     &                                 PIM%CONF%BPS_DEG_PHS, &
     &                                 KCHN, FREQ_CHN, PHAS_RES(1,J10), AMPL_RES(1,J10), &
     &                                       PHAS_MOD(1,J6,J10), AMPL_MOD(1,J6,J10), &
     &                                 PIM%NCHN, FREQ_ARR_R4(1,J6), &
     &                                       PHAS_OUT(1,J6,J10), AMPL_OUT(1,J6,J10), IER )
                            IF ( IER .NE. 0 ) THEN
                                 CALL CLRCH ( STR  )
                                 CALL INCH  ( IND_OBS, STR  )
                                 CALL ERR_LOG ( 6264, IUER, 'PIMA_PBP_ACCUM', &
     &                               'Failure to compute interpolating polynomial '// &
     &                               'for bandpass phases for station '// &
     &                                PIM%C_STA(J1)//' observation '//STR )
                                 GOTO 710
                            END IF
                            IF ( PIM%CONF%DEBUG_LEVEL == 27 ) THEN
                                 IF ( J10 == 1 ) T5(1:KCHN) = FREQ_CHN(1:KCHN)
                                 IF ( J10 == 1 ) X5(1:KCHN) = PHAS_MOD(1:KCHN,J6,J10)
                                 IF ( J10 == 2 ) T6(1:KCHN) = FREQ_CHN(1:KCHN)
                                 IF ( J10 == 2 ) X6(1:KCHN) = PHAS_MOD(1:KCHN,J6,J10)
                            END IF 
 4100                    CONTINUE 
                         IF ( PIM%CONF%DEBUG_LEVEL == 27 ) THEN
                              CALL CLRCH ( STR )
                              CALL INCH  ( J6, STR )
                              CALL DIAGI_SETDEF ( IER, 'DIAGI_CTIT', 'Raw seg phase diff in IF '//TRIM(STR)// &
     &                                           ' at STA: '//PIM%C_STA(J1) )
                              CALL DIAGI_2 ( KCHN, T5, X5, KCHN, T6, X6, IER )
                         END IF
                       ELSE IF ( PIM%CONF%BPS_INTRP_METHOD == PIMA__LINEAR ) THEN
                         DO 4110 J11=1,2
!
! ---------------------- ... a linear function
!
                            CALL ERR_PASS ( IUER, IER )
                            CALL BPASS_MOD_LINEAR ( 1, PIM%PBP(J1)%CMPL(1,J6), &
     &                                 AC_AVR(1,J6,IND_REF), AC_AVR(1,J6,IND_REM), &
     &                                 KCHN, FREQ_CHN, PHAS_RES(1,J11), AMPL_RES(1,J11), &
     &                                       PHAS_MOD(1,J6,J11), AMPL_MOD(1,J6,J11), &
     &                                 PIM%NCHN, FREQ_ARR_R4(1,J6), &
     &                                           PHAS_OUT(1,J6,J11), AMPL_OUT(1,J6,J11), IER )
                            IF ( IER .NE. 0 ) THEN
                                 CALL CLRCH ( STR  )
                                 CALL INCH  ( IND_OBS, STR  )
                                 CALL ERR_LOG ( 6266, IUER, 'PIMA_PBP_ACCUM', &
     &                               'Failure to compute linear interpolation'// &
     &                               'for bandpass phases for station '// &
     &                                PIM%C_STA(J1)//' observation '//STR )
                                 GOTO 710
                            END IF    
 4110                    CONTINUE 
                    END IF
!
                    PHAS_AVR = 0.0
                    AMPL_AVR = 0.0
                    NSEG_USED_IF = 0
                    KP = 0
                    DO 4120 J12=1,KCHN
                       DO 4130 J13=1,PIM%CONF%BPS_MSEG_ACCUM ! cycle over the spectral channales of the segment
                          KP = KP + 1
                          IF ( AMPL_RES(J12,1) > PIM%CONF%BPS_AMP_MIN*BPS%AMPL(J3,J1,J2) .AND. WEI_CHN(J12) > 0 ) THEN
                               IF ( J2 == 1 ) THEN
                                    IF ( SGN_REM == -1 ) THEN
!
! -------------------------------------- The second station is the reference
!
                                         IF ( PIM%CONF%DEBUG_LEVEL == 27 ) THEN
                                              T1(KP) = PIM%FREQ_ARR(KP,J6,PIM%CONF%FRQ_GRP)
                                              X1(KP) = PHAS_OUT(KP,J6,1)
                                              X2(KP) = PHAS_OUT(KP,J6,2)
                                              X3(KP) = PHAS_OUT(KP,J6,1) - PHAS_OUT(KP,J6,2)
                                         END IF
                                         PHAS_OUT(KP,J6,1) = PHAS_OUT(KP,J6,1) - PHAS_OUT(KP,J6,2) 
                                       ELSE
                                         IF ( PIM%CONF%DEBUG_LEVEL == 27 ) THEN
                                             T1(KP) = PIM%FREQ_ARR(KP,J6,PIM%CONF%FRQ_GRP)
                                             X1(KP) = PHAS_OUT(KP,J6,1)
                                             X2(KP) = PHAS_OUT(KP,J6,2)
                                             X3(KP) = PHAS_OUT(KP,J6,2) - PHAS_OUT(KP,J6,1)
                                         END IF
!
! -------------------------------------- The first station is the reference
!
                                         PHAS_OUT(KP,J6,1) = PHAS_OUT(KP,J6,2) - PHAS_OUT(KP,J6,1) 
                                     END IF
                                     IF ( AMPL_OUT(KP,J6,1)  > PIMA__AMP_MIN ) THEN
!
! --------------------------------------- Get the ratio of the amplitude of LL data to RR data
!
                                          AMPL_OUT(KP,J6,1) = AMPL_OUT(KP,J6,2)/AMPL_OUT(KP,J6,1)
                                        ELSE 
                                          AMPL_OUT(KP,J6,1) = 1.0D0
                                     END IF
                               END IF
                               PHAS_AVR = PHAS_AVR + PHAS_OUT(KP,J6,1)
                               AMPL_AVR = AMPL_AVR + AMPL_OUT(KP,J6,1)
                               NSEG_USED = NSEG_USED + 1
                               NSEG_USED_IF = NSEG_USED_IF + 1
                             ELSE
                               PHAS_OUT(KP,J6,1) = 0.0
                               AMPL_OUT(KP,J6,1) = 1.0
                               IF ( PIM%CONF%DEBUG_LEVEL == 27 ) THEN
                                    T1(KP) = PIM%FREQ_ARR(KP,J6,PIM%CONF%FRQ_GRP)
                                    X1(KP) = 0.0
                                    X2(KP) = 0.0
                                    X3(KP) = 0.0
                               END IF
                          END IF
 4130                  CONTINUE
 4120               CONTINUE
                    IF ( NSEG_USED_IF > 0 ) THEN
                         PHAS_AVR = PHAS_AVR/NSEG_USED_IF
                         AMPL_AVR = AMPL_AVR/NSEG_USED_IF
                    END IF
!
                    DO 4140 J14=1,KCHN
                       IF ( AMPL_RES(J14,2) > PIM%CONF%BPS_AMP_MIN*BPS%AMPL(J3,J1,J2) .AND. WEI_CHN(J14) > 0 ) THEN
                            PHAS_RMS = PHAS_RMS + (PHAS_MOD(J14,J6,1) - PHAS_AVR)**2
                            AMPL_RMS = AMPL_RMS + (AMPL_MOD(J14,J6,1) - AMPL_AVR)**2
                       END IF
 4140               CONTINUE 
 460             CONTINUE
                 IF ( PIM%CONF%DEBUG_LEVEL == 27 ) THEN
                      WRITE ( 6, * ) 'FEED_ANG_DIF = ', FEED_ANG_DIF
                      CALL DIAGI_SETDEF ( IER, 'DIAGI_CTIT', 'Phase diff at STA: '//PIM%C_STA(J1) )
                      CALL DIAGI_3 ( KP, T1, X1, KP, T1, X2, KP, T1, X3, IER )
                 END IF
!
                 IF ( NSEG_USED > 0 ) THEN
                      PHAS_RMS = SQRT ( PHAS_RMS/NSEG_USED )
                      AMPL_RMS = SQRT ( AMPL_RMS/NSEG_USED )
                    ELSE
                      PHAS_RMS = 0.0
                      AMPL_RMS = 0.0
                 END IF
!
                 CALL CLRCH ( STR )
                 IF ( PHAS_RMS > PHAS_RMS_MAX ) THEN
!
! ------------------- Phase rms is too big. This is an indication of an error
! ------------------- in phase unwrap. Remove this observation from
! ------------------- the accumulators
!
                      STR = 'REJECTED'
                    ELSE 
!
! ------------------- Bypass the reference observation
!
                      IFRQ = 0
                      IP = 0
                      DO 4160 J16=PIM%CONF%BEG_FRQ,PIM%CONF%END_FRQ
                         IFRQ = IFRQ + 1
                         DO 4170 J17=1,PIM%NCHN ! cycle over spectral channels
!
! ------------------------- Update accumulators taking into account mask
!
                            IF ( FL_BMASK ) THEN
                                 MASK_CHN = PIM%BANDPASS_MASK(J17,J16,IND_STA(1),PIMA__MASK_BPAS) * &
     &                                      PIM%BANDPASS_MASK(J17,J16,IND_STA(2),PIMA__MASK_BPAS)
                               ELSE
                                 MASK_CHN = 1
                            END IF
                            IF ( ABS(PIM%PBP(J1)%CMPL(J17,IFRQ)) > PIMA__AMP_MIN .AND. &
     &                           AMPL_OUT(J17,J16,1)             > PIMA__AMP_MIN .AND. &
     &                           ABS(PIM%PBP(J1)%CMPL(J17,IFRQ)) > PIMA__AMP_MIN       ) THEN
!
! ------------------------------ Update amplitude accumulator for the remtoe and reference stations.
! ------------------------------ We take the square root of the amplitude of this obsevations and 
! ------------------------------ divide it by the initial polarization bandpass
!
                                 ACC_REM_AMPL(J17,IFRQ,J1) = ACC_REM_AMPL(J17,IFRQ,J1)* &
     &                                                       MASK_CHN*SQRT( AMPL_OUT(J17,J16,1) )/ABS(PIM%PBP(J1)%CMPL(J17,IFRQ))
                                 WEI_REM(J17,IFRQ,J1)      = WEI_REM(J17,IFRQ,J1) + MASK_CHN
                                 ACC_REF_AMPL(J17,IFRQ)    = ACC_REF_AMPL(J17,IFRQ)* &
     &                                                       MASK_CHN*SQRT( AMPL_OUT(J17,J16,1) )/ABS(PIM%PBP(BPS%IND_STA_REF)%CMPL(J17,IFRQ))
                                 WEI_REF(J17,IFRQ)         = WEI_REF(J17,IFRQ) + MASK_CHN
                            END IF
                            ACC_REM_PHAS(J17,IFRQ,J1) = ACC_REM_PHAS(J17,IFRQ,J1) + &
     &                                                  MASK_CHN*SNR(1)*PHAS_OUT(J17,J16,1)
                            IP = IP + 1
 4170                     CONTINUE
 4160                 CONTINUE
                 END IF
!
                 IF ( PIM%CONF%DEBUG_LEVEL .GE. 2 ) THEN
                      WRITE ( 6, 210 ) PIM%C_STA(J1), J2, AMPL_RMS, PHAS_RMS, &
     &                                 STR(1:8)
 210                  FORMAT ( 'PBP   Sta: ',A, '  Case: ', I4,' Ampl_rms: ', F6.3, &
     &                    ' Phas_rms: ', F8.4, 2X, A )
                 END IF
 430          CONTINUE
 420     CONTINUE
!
! ------ Well, we processed all observations of this baseline. Now we compute
! ------ the polynomial coefficients over accumulated phases 
! ------ and amplitudes
!
         IF ( L_OBS > 0 ) THEN
              IFRQ = 0
              DO 4180 J18=PIM%CONF%BEG_FRQ,PIM%CONF%END_FRQ
                 IFRQ = IFRQ + 1
                 DO 4190 J19=1,PIM%NCHN ! cycle over averaged spectral channels
                    IF ( WEI_SUM >                   WEI__MIN      .AND. &
     &                   WEI_REM(J19,IFRQ,J1)      > PIMA__AMP_MIN .AND. &
     &                   ACC_REM_AMPL(J19,IFRQ,J1) > PIMA__AMP_MIN       ) THEN
                         ACC_REM_AMPL(J19,IFRQ,J1) = ACC_REM_AMPL(J19,IFRQ,J1)**(1.0/WEI_REM(J19,IFRQ,J1))
!!                         ACC_REM_AMPL(J19,IFRQ,J1) = ACC_REM_AMPL(J19,IFRQ,J1)**(1.0/(2.0*WEI_REM(J19,IFRQ,J1)))
                         ACC_REM_PHAS(J19,IFRQ,J1) = ACC_REM_PHAS(J19,IFRQ,J1)/WEI_SUM
                       ELSE
                         ACC_REM_AMPL(J19,IFRQ,J1) = 1.0
                         ACC_REM_PHAS(J19,IFRQ,J1) = 0.0
                    END IF
                    IAMB = NINT ( ACC_REM_PHAS(J19,IFRQ,J1)/PI2 )
                    PHAS_OUT(J19,J18,1) = ACC_REM_PHAS(J19,IFRQ,J1) - IAMB*PI2
                    IF ( FL_BMASK ) THEN
                         IF ( PIM%BANDPASS_MASK(J19,J18,J1,PIMA__MASK_BPAS) == 0 ) THEN
                              ACC_REM_AMPL(J19,IFRQ,J1) = 1.0
                              ACC_REM_PHAS(J19,IFRQ,J1) = 0.0
                         END IF
                    END IF
 4190            CONTINUE
!
                 IF ( PIM%CONF%BPS_INTRP_METHOD == PIMA__LEGENDRE ) THEN
                      CALL ERR_PASS ( IUER, IER )
                      CALL BPASS_MOD_POLY ( 2, &
     &                     PIM%CONF%BPS_AMP_MIN*BPS%AMPL(J3,J1,J2),  &
     &                     PIM%CONF%BPS_DEG_AMP, PIM%CONF%BPS_DEG_PHS, &
     &                     PIM%NCHN, FREQ_ARR_R4(1,J18), &
     &                     ACC_REM_PHAS(1,IFRQ,J1), &
     &                     ACC_REM_AMPL(1,IFRQ,J1), &
     &                     PHAS_MOD(1,J18,1), AMPL_MOD(1,J18,1), &
     &                     PIM%NCHN, FREQ_ARR_R4(1,J18), &
     &                     PHAS_OUT(1,J18,1), AMPL_OUT(1,J18,1), &
     &                     IER )
                      IF ( IER .NE. 0 ) THEN
                           CALL ERR_LOG ( 6268, IUER, 'PIMA_PBP_ACCUM', &
     &                         'Failure to compute interpolating polynomial '// &
     &                         'for bandass phases for remote station '// &
     &                         PIM%C_STA(J1) )
                           GOTO 710
                      END IF
                    ELSE IF ( PIM%CONF%BPS_INTRP_METHOD == PIMA__SPLINE ) THEN
!
! -------------------- ... Using expansion into the B-spline basis
!
                      CALL ERR_PASS ( IUER, IER )
                      CALL BPASS_MOD_SPLINE ( MOD_AMB, &
     &                           PIM%CONF%BPS_AMP_MIN*BPS%AMPL(J3,J1,J2),  &
     &                           PIM%CONF%BPS_DEG_AMP, &
     &                           PIM%CONF%BPS_DEG_PHS, &
     &                           PIM%NCHN, FREQ_ARR_R4(1,J18), &
     &                           ACC_REM_PHAS(1,IFRQ,J1), &
     &                           ACC_REM_AMPL(1,IFRQ,J1), &
     &                           PHAS_MOD(1,J18,1), AMPL_MOD(1,J18,1), &
     &                           PIM%NCHN, FREQ_ARR_R4(1,J18), &
     &                           PHAS_OUT(1,J18,1), AMPL_OUT(1,J18,1), &
     &                           IER )
                      IF ( IER .NE. 0 ) THEN
                           CALL CLRCH ( STR  )
                           CALL INCH  ( IND_OBS, STR  )
                           CALL ERR_LOG ( 6270, IUER, 'PIMA_PBP_ACCUM', &
     &                              'Failure to compute interpolating polynomial '// &
     &                              'for bandass phases for remote station '// &
     &                               PIM%C_STA(J1) )
                           GOTO 710
                      END IF    
                    ELSE IF ( PIM%CONF%BPS_INTRP_METHOD == PIMA__LINEAR ) THEN
!
! ------------------- ... a linear function
!
                      CALL ERR_PASS ( IUER, IER )
                      CALL BPASS_MOD_POLY ( 1, PIM%CONF%BPS_AMP_MIN*BPS%AMPL(J3,J1,J2),  0, 1, &
     &                          PIM%NCHN, FREQ_ARR_R4(1,J18), &
     &                                    ACC_REM_PHAS(1,IFRQ,J1), &
     &                                    ACC_REM_AMPL(1,IFRQ,J1), &
     &                                    PHAS_MOD(1,J18,1), AMPL_MOD(1,J18,1), &
     &                          PIM%NCHN, FREQ_ARR_R4(1,J18), &
     &                                    PHAS_OUT(1,J18,1), AMPL_OUT(1,J18,1), IER )
                      IF ( IER .NE. 0 ) THEN
                           CALL ERR_LOG ( 6272, IUER, 'PIMA_PBP_ACCUM', &
     &                              'Failure to compute interpolating polynomial '// &
     &                              'for bandass phases for station '// &
     &                               PIM%C_STA(J1) )
                           GOTO 710
                      END IF
                 END IF
!
! -------------- Combine amplitudes and phases into the complex bandpass
!
                 DO 4200 J20=1,PIM%NCHN ! cycle over spectral channels
                    IF ( AMPL_OUT(J20,J18,1) < PIMA__BPS_AMP_SPLN_MIN ) THEN
                         AMPL_OUT(J20,J18,1) = PIMA__BPS_AMP_SPLN_MIN
                    END IF
                    IF ( FL_BMASK ) THEN
                         IF ( PIM%BANDPASS_MASK(J20,J18,J1,PIMA__MASK_BPAS) == 0 ) THEN
!
! --------------------------- Special case of the channels that were
! --------------------------- masked out
!
                              AMPL_OUT(J20,J18,1) = 1.0
                              PHAS_OUT(J20,J18,1) = 0.0
                         END IF
                    END IF
                    PIM%PBP(J1)%CMPL(J20,J18) = PIM%PBP(J1)%CMPL(J20,J18)* &
     &                          CMPLX( AMPL_OUT(J20,J18,1)*COS(PHAS_OUT(J20,J18,1)), &
     &                                 AMPL_OUT(J20,J18,1)*SIN(PHAS_OUT(J20,J18,1))  )
 4200            CONTINUE
 4180         CONTINUE
!
! ----------- Now we normalize polarization bandpass
!
              CALL BPASS_AMP_NRML ( PIM, J1, BPS%IND_STA_REF, &
     &                         PIM%PBP(J1)%CMPL(1,1), PIM%PBP(BPS%IND_STA_REF)%CMPL(1,1), &
     &                         AMPL_FRQ_NRML,  &
     &                         AMPL_BAND_NRML, &
     &                         AMPL_FRQ_AVR,   &
     &                         AMPL_FRQ_RMS,   &
     &                         AMPL_TOT_RMS,   &
     &                         AMPL_INTEGRAL  )
         END IF
 410  CONTINUE
!
! ---- Compute the bandpass amplitude of the reference station.
! ---- NB: Currently the amplitude of the reference station bandpass
! ----     is a product of amplitueds of all the stations that 
! ----     had a common base with the reference one. 
! ---- We first take the root of the nth degere, second take the square root 
! ----    of the result, and third invert the result
!
      IF ( POL_CONF == PIMA__PC_CC ) THEN
           IFRQ = 0
           DO 4210 J21=PIM%CONF%BEG_FRQ,PIM%CONF%END_FRQ
              IFRQ = IFRQ + 1
              DO 4220 J22=1,PIM%NCHN ! cycle over spectral channels
                 IF ( WEI_REF(J22,IFRQ)                           > 0             .AND. &
     &                ACC_REF_AMPL(J22,IFRQ)                      > PIMA__AMP_MIN .AND. & 
     &                ABS(PIM%PBP(BPS%IND_STA_REF)%CMPL(J22,J21)) > PIMA__AMP_MIN       ) THEN
!
                      PIM%PBP(BPS%IND_STA_REF)%CMPL(J22,J21) = PIM%PBP(BPS%IND_STA_REF)%CMPL(J22,J21)* &
     &                        ACC_REF_AMPL(J22,IFRQ)**(1.0/WEI_REF(J22,IFRQ))
!!     &                        ACC_REF_AMPL(J22,IFRQ)**(1.0/(2.0*WEI_REF(J22,IFRQ)))
                 END IF
 4220         CONTINUE
 4210      CONTINUE
      END IF
      IF ( L_OBS > 0 .AND. POL_CONF .NE. PIMA__PC_CC ) THEN 
      END IF
!
      CALL ERR_LOG ( 0, IUER )
 710  CONTINUE 
      IF ( ALLOCATED ( ACC_REF_AMPL ) ) DEALLOCATE ( ACC_REF_AMPL )
      IF ( ALLOCATED ( ACC_REF_PHAS ) ) DEALLOCATE ( ACC_REF_PHAS )
      IF ( ALLOCATED ( WEI_REF      ) ) DEALLOCATE ( WEI_REF      )
      IF ( ALLOCATED ( ACC_REM_AMPL ) ) DEALLOCATE ( ACC_REM_AMPL )
      IF ( ALLOCATED ( ACC_REM_PHAS ) ) DEALLOCATE ( ACC_REM_PHAS )
      IF ( ALLOCATED ( WEI_REM      ) ) DEALLOCATE ( WEI_REM      )
      IF ( ALLOCATED ( AC_AVR   ) ) DEALLOCATE ( AC_AVR   )
      IF ( ALLOCATED ( RES      ) ) DEALLOCATE ( RES      )
      RETURN
      END  SUBROUTINE  PIMA_PBP_ACCUM  !#!
