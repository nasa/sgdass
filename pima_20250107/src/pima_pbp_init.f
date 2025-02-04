      SUBROUTINE PIMA_PBP_INIT ( PIM, VTD, BPS, POL_ARR, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  PIMA_PBP_INIT computes the initial polarization bandpass. *
! *                                                                      *
! * ### 18-AUG-2010  PIMA_PBP_INIT  v4.0 (c)  L. Petrov  09-DEC-2020 ### *
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
      CHARACTER  POL_ARR*(*), STR*128, STR1*128
      LOGICAL*1  FL_BMASK, FL_PIMAVAR_LINPOL_ROTSWAP
      COMPLEX*8  RES_AVR(4)
      COMPLEX*8, ALLOCATABLE :: RES(:,:,:)
      REAL*4,    ALLOCATABLE :: AC_AVR(:,:,:)
      CHARACTER  POLAR_SAVE*8, POLAR_STYLE*8, SAVE_BPS_NORML*8, POL_CONF*7, FINAM_2D_PLOT*128
      INTEGER*4  J1, J2, J3, J4, J5, J6, J7, J8, J9, J10, J11, J12, J13, J14, &
     &           J15, J16, J17, J18, J19, J20, J21, J22, J23, J24, IP, KP, KP2, LSTA, &
     &           IND_STA(2), LCHN, LFRQ, LTIM, IFRQ, L_PBP, IND_FRA, IND_OBS, &
     &           KCHN, ICHN, MOD_AMB, NUM_PROC_OBS, IND_REF, IND_REM, &
     &           POL_MODE, MODE_INIT, FRG_IND, J_PHS, K_PHS, IAMB, K1, K2, SGN, &
     &           IND_OBS_PROC(PIM__MSTA,PIM__MPLR), IND_OBS_REF(PIM__MPLR), &
     &           IND_POL, FRI_STS, DEB_LEV_SAVE, LPOL, IND_STA_1, IND_STA_2, IER
      REAL*4,    POINTER :: AMPL_FRQ_AVR(:,:) => NULL()
      REAL*4,    POINTER :: AMPL_FRQ_RMS(:,:) => NULL()
      REAL*4,    POINTER :: FREQ_SEG(:,:)     => NULL()
      REAL*4,    POINTER :: WEI_SEG(:,:)      => NULL()
      REAL*4,    POINTER :: PHAS_INIT(:,:,:)  => NULL()
      COMPLEX*8, POINTER :: DRF_SEG(:,:)      => NULL()
      COMPLEX*8, POINTER :: REF_CMPL(:,:)     => NULL()
      INTEGER*4, POINTER :: JSTA(:,:,:,:)     => NULL()
      REAL*4     FREQ_IN(PIM__MCHN),  AMPL_RES(PIM__MCHN), PHAS_RES(PIM__MCHN), &
     &           FREQ_OUT(PIM__MCHN), AMPL_MOD(PIM__MCHN), PHAS_MOD(PIM__MCHN), &
     &           PHAS_OUT(PIM__MCHN), &
     &           PHAS_ALL, AMPL_FRQ_NRML(PIM__MFRQ), AMPL_BAND_NRML, &
     &           WEI_IN(PIM__MCHN),AMPL_TOT_RMS(PIM__MSTA), &
     &           AMPL_INTEGRAL(PIM__MSTA), FEED_ANG_DIF, &
     &           GD_EST_R4(PIM__MSTA,2), AMP_R4(PIM__MPLR), PHS_R4(PIM__MPLR), &
     &           PHS_ACC
      REAL*8     SNR_NEW(PIM__MPLR), SNR_LL, PH_RAT(PIM__MFRA,2), GR_DEL(PIM__MFRA,2), &
     &           PHS(PIM__MFRA,2), FREQ(PIM__MCHN), AMPL(PIM__MFRA), &
     &           PHAS(PIM__MCHN), WEI(PIM__MCHN), &
     &           POL_COEF_AMP(0:PIM__MPOL), POL_COEF_PHS(0:PIM__MPOL), &
     &           POL_COEF_ERR(0:PIM__MPOL), DR_AMP, SH_AMP, DR_PHS, SH_PHS, &
     &           WEI_AVR, P_AVR, G_DEL, P_RMS, REF_STA_WEI_ACC, PHAS_DIF, &
     &           PHS_DIF_AVR_ACC_IF,  PHS_DIF_SQR_ACC_IF, &
     &           PHS_DIF_AVR_ACC_ALL, PHS_DIF_SQR_ACC_ALL, &
     &           PHS_DIF_AVR, PHS_DIF_RMS, GR_RAT(2), &
     &           SNR_MIN_REF(PIM__MPLR), TIM_FRT_REF(PIM__MPLR), &
     &           GR_DEL_DIF, PH_RAT_DIF, GR_RAT_DIF, PHS_DIF, FREQ_REF, &
     &           GR_DEL_POL_DIF(PIM__MSTA), PHS_POL_DIF(PIM__MSTA)
      REAL*8     GR_DEL_ERR(PIM__MFRA), PH_RAT_ERR(PIM__MFRA), &
     &           PH_DEL_ERR(PIM__MFRA), GR_RAT_ERR, GRAMBSP, &
     &           PHAS_ERR(PIM__MFRA), EFF_FRQ_PHS, EFF_FRQ_GRP, &
     &           EFF_FRQ_RAT, FREQ_DIF, COV_PR_PH, COV_GR_MD, &
     &           PH_ACC, PH_ACC_ERR, EFF_DURA, TIME_FRT, &
     &           TEC, TEC_RATE, TEC_ERR, TEC_RATE_ERR, DECOR_TIM

      COMPLEX*8  RES_MSEG(4), CMPL_DIF_AVR_ACC_IF, BPASS_C8
      REAL*8     EPS
      PARAMETER  ( EPS = 1.D-5 )
      INTEGER*1  MASK_CHN 
      INTEGER*1  SGN_REF, SGN_REM
      LOGICAL*4  FL_AMBIG_RES, FL_AMB_IF, FL_AMB_OBS, FL_PBP_PLOT, &
     &           FL_LIN_TREND, FL_PROC(2,PIM__MSTA)
      REAL*8     T1(8192), T2(8192), X1(8192), X2(8192), X3(8192), X4(8192)
      REAL*4     AMPL_R4, PHAS_R4
      COMPLEX*8  DRF_ACC(4)
      COMPLEX*8, ALLOCATABLE :: REF_STA_CMPL(:,:)
      REAL*4,    EXTERNAL :: PHAS_CMPL_R4
      CHARACTER, EXTERNAL :: PIMA_GET_POL_CONF*7
      REAL*8,    EXTERNAL :: LEGENDRE_POL
      INTEGER*4, EXTERNAL :: ILEN, I_LEN
      LOGICAL*4, EXTERNAL :: IS_R8_NAN
!
      FINAM_2D_PLOT = '/tmp/plot.gif'
!
      CALL GETENVAR ( 'PIMAVAR_LINPOL_ROTSWAP', STR )
      IF ( ILEN(STR) > 0 ) THEN
           CALL TRAN ( 11, STR, STR )
           IF ( STR(1:3) == 'YES' ) THEN
                FL_PIMAVAR_LINPOL_ROTSWAP = .TRUE.
             ELSE
                FL_PIMAVAR_LINPOL_ROTSWAP = .FALSE.
           END IF
         ELSE
           FL_PIMAVAR_LINPOL_ROTSWAP = .FALSE.
      END IF
!
      LCHN = PIM%NCHN
      LFRQ = PIM%CONF%END_FRQ - PIM%CONF%BEG_FRQ + 1
      KCHN = PIM%NCHN/PIM%CONF%BPS_MSEG_ACCUM
!
      IF ( PIM%CONF%DEBUG_LEVEL .GE. 3 ) THEN
           WRITE ( 6, * ) '--------------------------- ' 
           WRITE ( 6, * ) ' PIMA_PBP_INIT              ' 
           WRITE ( 6, * ) '--------------------------- ' 
      END IF
      IF ( PIM%CONF%FRIB_FINE_SEARCH == PIMA__FINE_SEARCH_LSQ ) THEN
           IND_FRA = PIMA__LSQ
         ELSE
           IND_FRA = PIMA__DRF
      END IF
!
      IF ( PIM%CONF%BANDPASS_MASK_FILE .NE. PIMA__BPASS_NO  .AND. &
     &     ASSOCIATED ( PIM%BANDPASS_MASK )                       ) THEN
           FL_BMASK = .TRUE.
         ELSE
           FL_BMASK = .FALSE.
      END IF
      CALL GETENVAR ( 'PIMAVAR_PBP_PLOT', STR )
      IF ( STR == 'YES' .OR. STR == 'yes' ) THEN
           FL_PBP_PLOT = .TRUE.
         ELSE
           FL_PBP_PLOT = .FALSE.
      END IF
!
      ALLOCATE ( PIM%PBP(PIM%NSTA), &
     &           RES(LCHN,PIM%NFRQ,4), &
     &           AC_AVR(LCHN,PIM%NFRQ,4), &
     &           AMPL_FRQ_AVR(PIM%NFRQ,PIM%NSTA), &
     &           AMPL_FRQ_RMS(PIM%NFRQ,PIM%NSTA), &
     &           REF_STA_CMPL(PIM%NCHN,PIM%NFRQ), &
     &           DRF_SEG(KCHN,LFRQ), &
     &           JSTA(LCHN,LFRQ,2,PIM%NSTA), &
     &           FREQ_SEG(KCHN,LFRQ), &
     &           WEI_SEG(KCHN,LFRQ), &
     &           PHAS_INIT(PIM%NCHN,PIM%NFRQ,2), &
     &           REF_CMPL(PIM%NCHN,PIM%NFRQ), &
     &           PIM%PBP_INIT_CMPL(PIM%NCHN,PIM%NFRQ,PIM__MPLR,PIM%NSTA), &
     &           STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( PIM%NSTA*SIZEOF(PIM%PBP(1)) + &
     &                  8*LCHN*PIM%NFRQ*4   + &
     &                  8*LCHN*PIM%NFRQ*4   + &
     &                  8*PIM%NFRQ*PIM%NSTA + &
     &                  8*PIM%NFRQ*PIM%NSTA + &
     &                  8*PIM%NCHN*PIM%NFRQ + &
     &                  8*KCHN*LFRQ + &
     &                  4*LCHN*LFRQ + &
     &                  8*KCHN*LFRQ + &
     &                  4*KCHN*LFRQ + &
     &                  4*PIM%NCHN*PIM%NFRQ*PIM__MPLR + &
     &                  8*PIM%NCHN*PIM%NFRQ*PIM__MPLR*PIM%NSTA, STR )
           CALL ERR_LOG ( 6381, IUER, 'PIMA_PBP_INIT', 'Error in an '// &
     &         'attempt to allocate '//STR(1:I_LEN(STR))//' bytes of '// &
     &         'dynamic memory for eleven arrays' )
           RETURN 
      END IF
      PIM%PBP_INIT_CMPL = 0.0
!
      JSTA = 0
      REF_STA_CMPL = 1.E-12*CMPLX ( 1.0, 0.0 )
!
! --- Intialization and memory allocation
!
      PIM%L_STA_PBP = L_PBP 
      REF_STA_WEI_ACC = 0.0D0
      L_PBP = 0
      LSTA  = 0
      NUM_PROC_OBS = 0
      IND_OBS_PROC = 0
      IND_OBS_REF  = 0
      SNR_MIN_REF  = 0.0
      DO 410 J1=1,PIM%NSTA
         PIM%PBP(J1)%TYP = 'NO'
         CALL CHIN ( PIMA__LABEL(6:13), PIM%PBP(J1)%PIMA_VERS )
!
         PIM%PBP(J1)%STATUS      = PIMA__BPASS_NO
         PIM%PBP(J1)%IND_STA_REF = BPS%IND_STA_REF
         L_PBP = L_PBP + 1
         SNR_NEW = 0.0
         DO 420 J2=1,2  !! PIM__MPLR  ! Par Crs Ref Rem
            IF ( INDEX ( POL_ARR, 'H' ) < 1 .OR. INDEX ( POL_ARR, 'V' ) < 1 .OR. &
     &           INDEX ( POL_ARR, 'X' ) < 1 .OR. INDEX ( POL_ARR, 'Y' ) < 1      ) THEN
                 IF ( J2 > 2 .AND. BPS%NUM_OBS_ACCUM(J1,J2) == 0 ) GOTO 420
            END IF
!
            IF ( BPS%NUM_OBS_ACCUM(J1,J2) == 0 ) THEN
                 IF ( J2 == 1 .AND. PIM%CONF%DEBUG_LEVEL .GE. 2 ) THEN
                      IF ( PIM%C_STA(J1) < PIM%C_STA(BPS%IND_STA_REF) ) THEN
                           WRITE ( 6, '(A)'  ) 'PIMA_PBP_INIT: No suitable observations at baseline '// &
     &                                          PIM%C_STA(J1)//' / '//PIM%C_STA(BPS%IND_STA_REF)
                        ELSE IF ( PIM%C_STA(J1) > PIM%C_STA(BPS%IND_STA_REF) ) THEN
                            WRITE ( 6, '(A)' ) 'PIMA_PBP_INIT: No suitable observations at baseline '// &
     &                                          PIM%C_STA(BPS%IND_STA_REF)//' / '//PIM%C_STA(J1)
                      END IF
                 END IF
                 GOTO 420
            END IF
!
            DO 430 J3=1,PIM%BPS%NUM_OBS_ACCUM(J1,J2)
               IND_OBS = BPS%IND_OBS_SEL(J3,J1,J2)
               IND_POL = BPS%IND_OBS_POL(J3,J1,J2)
               FRG_IND = PIM%OBS(IND_OBS)%REF_FRG_INDS(PIM%CONF%FRQ_GRP) 
               IF ( FRG_IND == 0 ) THEN
                    GOTO 430
               END IF
               POL_CONF = PIMA_GET_POL_CONF ( PIM, IND_OBS, IER ) 
               IND_STA(1) = PIM%OBS(IND_OBS)%STA_IND(1)
               IND_STA(2) = PIM%OBS(IND_OBS)%STA_IND(2)
               IF ( POL_CONF .EQ. PIMA__PC_CC ) THEN
                    MODE_INIT = 2
                    IF ( J2 == 2 ) GOTO 420
                 ELSE 
                    MODE_INIT = 1
               END IF
               NUM_PROC_OBS = NUM_PROC_OBS + 1
!
               IF ( .NOT. ASSOCIATED ( PIM%PBP(IND_STA(1))%CMPL ) ) THEN
!
! ----------------- Allocate memory for the the bandpass for the 1st station, if
! ----------------- it is not yet allocated
!
                    ALLOCATE ( PIM%PBP(IND_STA(1))%CMPL(PIM%NCHN,PIM%NFRQ), STAT=IER )
                    IF ( IER .NE. 0 ) THEN
                         CALL CLRCH ( STR  )
                         CALL IINCH ( 8*PIM%NCHN*PIM%NFRQ, STR )
                         CALL ERR_LOG ( 6390, IUER, 'PIMA_PBP_INIT', 'Error in an '// &
     &                       'attempt to allocate '//STR(1:I_LEN(STR))//' bytes of '// &
     &                       'dynamic memory for the field CMPL in PIM%PBP' )
                         RETURN
                     END IF
!
! ------------------ ... and initialize it
!
                     PIM%PBP(IND_STA(1))%CMPL = CMPLX ( 1.0, 0.0 )
                     IF ( POL_CONF .NE. PIMA__PC_CC .AND. IND_STA(1) == BPS%IND_STA_REF ) THEN
                          PIM%PBP(IND_STA(1))%CMPL = 1.0E-12*PIM%PBP(IND_STA(1))%CMPL 
                     END IF
                     PIM%PBP(IND_STA(1))%STATUS = PIMA__BPASS_INIT
               END IF
!
               IF ( .NOT. ASSOCIATED ( PIM%PBP(IND_STA(2))%CMPL ) ) THEN
!
! ----------------- Allocate memory for the the bandpass for the 2nd station, if
! ----------------- it not yet allocated
!
                    ALLOCATE ( PIM%PBP(IND_STA(2))%CMPL(PIM%NCHN,PIM%NFRQ), STAT=IER )
                    IF ( IER .NE. 0 ) THEN
                         CALL CLRCH ( STR  )
                         CALL IINCH ( 8*PIM%NCHN*PIM%NFRQ, STR )
                         CALL ERR_LOG ( 6391, IUER, 'PIMA_PBP_INIT', 'Error in an '// &
     &                       'attempt to allocate '//STR(1:I_LEN(STR))//' bytes of '// &
     &                       'dynamic memory for the field CMPL in PIM%PBP' )
                         RETURN
                    END IF
!
! ------------------ ... and initialize it
!
                    PIM%PBP(IND_STA(2))%CMPL = CMPLX ( 1.0, 0.0 )
                    IF ( POL_CONF .NE. PIMA__PC_CC .AND. IND_STA(2) == BPS%IND_STA_REF ) THEN
                         PIM%PBP(IND_STA(2))%CMPL = 1.0E-12*PIM%PBP(IND_STA(2))%CMPL 
                    END IF
                    PIM%PBP(IND_STA(1))%STATUS = PIMA__BPASS_INIT
               END IF
!
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
!
               LTIM = PIM%OBS(IND_OBS)%NUM_EPC(PIM%OBS(IND_OBS)%REF_FRG_INDS(PIM%CONF%FRQ_GRP))
!
! ------------ Feed horn rotation angle difference
!
               FEED_ANG_DIF =   PIM%OBS(IND_OBS)%FEED_ANG(1) &
     &                        - PIM%OBS(IND_OBS)%FEED_ANG(2) 
               IF ( FL_PIMAVAR_LINPOL_ROTSWAP ) THEN
                    FEED_ANG_DIF = -FEED_ANG_DIF 
               END IF
!
! ------------ To avoid overflow in tan(feed_ang_dif)
!
               IF ( ABS(FEED_ANG_DIF)                 < EPS ) FEED_ANG_DIF =  EPS
               IF ( ABS(FEED_ANG_DIF - PI2)           < EPS ) FEED_ANG_DIF =  PI2           - EPS
               IF ( ABS(FEED_ANG_DIF - PI__NUM)       < EPS ) FEED_ANG_DIF =  PI__NUM       - EPS
               IF ( ABS(FEED_ANG_DIF - PI__NUM - PI2) < EPS ) FEED_ANG_DIF =  PI__NUM + PI2 - EPS
               IF ( ABS(FEED_ANG_DIF + PI2          ) < EPS ) FEED_ANG_DIF = -PI2           + EPS
               IF ( ABS(FEED_ANG_DIF + PI__NUM      ) < EPS ) FEED_ANG_DIF = -PI__NUM       + EPS
               IF ( ABS(FEED_ANG_DIF + PI__NUM + PI2) < EPS ) FEED_ANG_DIF = -PI2 - PI__NUM + EPS
!
               POLAR_SAVE = PIM%CONF%POLAR
               IF ( POLAR_SAVE .NE. PIMA__POLAR_I ) THEN
                    CALL ERR_LOG ( 6392, IUER, 'PIMA_PBP_INIT', 'Trap '// &
     &                  'of internal control: polarization '// &
     &                  PIM%CONF%POLAR//' is not suitable '// &
     &                  ' for polarization bandpass computation, only '// &
     &                  ' I is supported' )
                    RETURN 
               END IF
!
               IF ( POL_CONF == PIMA__PC_CC ) THEN
!
! ----------------- Get UV data and put them in PIM%OBS(IND_OBS)%UV, PIM%OBS(IND_OBS)%UV_IF, PIM%OBS(IND_OBS)%UV_BAND
!
                    LPOL = 2
                    CALL ERR_PASS ( IUER, IER )
                    CALL PIMA_GET_OBS ( PIM, VTD, IND_OBS, PIMA__LLCC, LPOL, &
     &                                  .TRUE., .TRUE., .FALSE., FRI_STS, IER )
                    IF ( IER .NE. 0 ) THEN
                         CALL ERR_LOG ( 6395, IUER, 'PIMA_PBP_INIT', 'Error in getting '// &
     &                       'visibility data' )
                         RETURN 
                    END IF
                    IND_STA_1 = PIM%OBS(IND_OBS)%STA_IND(1)
                    IND_STA_2 = PIM%OBS(IND_OBS)%STA_IND(2)
!
! ----------------- Apply RR bandpass
!
                    IFRQ = 0
                    DO 590 J9=PIM%CONF%BEG_FRQ,PIM%CONF%END_FRQ
                       IFRQ = IFRQ + 1
                       DO 5100 J10=1,PIM%NCHN
                          PHAS_R4 = -PHAS_CMPL_R4 ( PIM%BPS%CMPL(J10,J9,IND_STA_1) ) &
     &                              +PHAS_CMPL_R4 ( PIM%BPS%CMPL(J10,J9,IND_STA_2) )
                          BPASS_C8 = CMPLX ( COS(-PHAS_R4), SIN(-PHAS_R4) )
                          DO 5110 J11=1,LTIM
                             PIM%OBS(IND_OBS)%UV(J10,IFRQ,J11,1) = PIM%OBS(IND_OBS)%UV(J10,IFRQ,J11,1)*BPASS_C8
 5110                     CONTINUE
 5100                  CONTINUE
 590                CONTINUE
!
! ----------------- Proceed with fringe fitting for LL data without polarization bandpass
! ----------------- using RR bandpass
!
                    FREQ_REF = PIM%FREQ_ARR(1,PIM%CONF%BEG_FRQ,PIM%CONF%FRQ_GRP)
                    DEB_LEV_SAVE = PIM%CONF%DEBUG_LEVEL
                    PIM%CONF%DEBUG_LEVEL = 0
!
! ----------------- Run fringe fitting
!
                    CALL ERR_PASS ( IUER, IER )
                    CALL PIMA_2FFT ( PIM, VTD, IND_OBS, LCHN, LFRQ, LTIM, &
     &                               PIM%FREQ_ARR(1,PIM%CONF%BEG_FRQ,PIM%CONF%FRQ_GRP), &
     &                               FREQ_REF, PIM%OBS(IND_OBS)%UV(1,1,1,1), &
     &                               PIM%OBS(IND_OBS)%WEI_1D, PIM%OBS(IND_OBS)%AP_LEN, &
     &                               FINAM_2D_PLOT, TIME_FRT, GR_DEL(1,2), PH_RAT(1,2), GR_RAT(2), &
     &                               PH_ACC, PHS(1,2), AMPL, &
     &                               SNR_LL, GR_DEL_ERR, PH_DEL_ERR, PH_RAT_ERR, &
     &                               GR_RAT_ERR, PH_ACC_ERR, PHAS_ERR, GRAMBSP, &
     &                               EFF_FRQ_PHS, EFF_FRQ_GRP, EFF_FRQ_RAT, &
     &                               COV_PR_PH, COV_GR_MD, EFF_DURA, &
     &                               TEC, TEC_RATE, TEC_ERR, TEC_RATE_ERR, DECOR_TIM, IER )
                    IF ( IER .NE. 0 ) THEN
                         CALL CLRCH ( STR )
                         CALL INCH  ( IND_OBS, STR )
                         CALL ERR_LOG ( 6396, IUER, 'PIMA_PBP_INIT', 'Failure in '// &
     &                        'attempt to fringe search in observation '//STR )
                         RETURN
                    END IF
                    PIM%CONF%DEBUG_LEVEL = DEB_LEV_SAVE 
               END IF
!
! ------------ Getting residulas for first polarization (RR or XX)
!
               IF ( POL_CONF == PIMA__PC_CC ) THEN
                    POL_MODE = PIMA__PAR
                  ELSE
                    POL_MODE = PIMA__PALL_NOR  ! all polarizations without parallactic angle rotation
               END IF
!
               PIM%BPASS(IND_STA(IND_REM))%BPS(1:PIM%NCHN,1:PIM%NFRQ) = BPS%CMPL(1:PIM%NCHN,1:PIM%NFRQ,IND_STA(IND_REM))
               PIM%BPASS(IND_STA(IND_REF))%BPS(1:PIM%NCHN,1:PIM%NFRQ) = BPS%CMPL(1:PIM%NCHN,1:PIM%NFRQ,IND_STA(IND_REF))
               CALL ERR_PASS ( IUER, IER )
               CALL PIMA_GET_RESID ( PIM, VTD, IND_OBS, IND_POL, LCHN, LFRQ, LTIM, &
     &                    GR_DEL(1,J2), PH_RAT(1,J2), GR_RAT(J2), PHS(1,J2), &
     &                    BPS%SNR(J3,J1,J2), BPS%TIME_FRT(J3,J1,J2), &
     &                    PIMA__2FFT, PIMA__BPASS_PHS, POL_MODE, &
     &                    RES, RES_AVR, SNR_NEW(J2), IER )
               BPS%GR_RAT(J3,J1,J2) = GR_RAT(J2)
               IF ( IER .NE. 0 ) THEN
                     CALL CLRCH ( STR )
                     CALL INCH  ( IND_OBS, STR )
                     CALL ERR_LOG ( 6394, IUER, 'PIMA_PBP_INIT', 'Error '// &
     &                   'in an attempt to compute residuals for '// &
     &                   'observation '//STR )
                    RETURN
               END IF
               IF ( POL_CONF == PIMA__PC_CC ) THEN
                    GR_DEL_POL_DIF(J1) = GR_DEL(IND_FRA,2) - GR_DEL(IND_FRA,1)
                    PHS_POL_DIF(J1)    = PHS(IND_FRA,2)    - PHS(IND_FRA,1)
                    IF ( PIM%CONF%DEBUG_LEVEL .GE. 3 ) THEN
                         WRITE ( 6, 205 ) IND_OBS, PIM%C_STA(J1), PIM%C_STA(J2), &
     &                                    1.D9*GR_DEL_POL_DIF(J1), PHS_POL_DIF(J1)
 205                     FORMAT ( 'PIMA_PBP_INIT  OBS: ', I6,  1X, A, ' / ', A, 1X, 'Gr_pol_del_dif = ', &
     &                            F12.3, ' ns ', ' Phs_pol_dif= ', F8.5 )
                    END IF
!
! ----------------- Eliminate the contribution of the differences in group delay and mean phase 
! ----------------- between RR and LL polarizations on residual phases in LL data.
! ----------------- This procedure will make residual phases small without 2pi ambiguities
!
                    CALL PIMA_APPLY_GR_DEL_PHS ( PIM%NCHN, LFRQ, PIM%FREQ_ARR(1,PIM%CONF%BEG_FRQ,PIM%CONF%FRQ_GRP), &
     &                                             FREQ_REF, GR_DEL_POL_DIF(J1), PHS_POL_DIF(J1), RES(1,1,2) )
                  ELSE
                    GR_DEL_POL_DIF = 0.0D0
                    PHS_POL_DIF    = 0.0D0 
               END IF
               IF ( J2 == 2 ) THEN
                    GR_DEL_DIF = GR_DEL(IND_FRA,2) - GR_DEL(IND_FRA,1)
                    PH_RAT_DIF = PH_RAT(IND_FRA,2) - PH_RAT(IND_FRA,1)
                    PHS_DIF    = PHS(IND_FRA,2)    - PHS(IND_FRA,1) 
                    GR_RAT_DIF = GR_RAT(2)         - GR_RAT(1)
                    IF ( PIM%CONF%DEBUG_LEVEL .GE. 3 ) THEN
                         WRITE ( 6, 205 ) IND_OBS, PIM%C_STA(J1), PIM%C_STA(J2), 1.D9*GR_DEL_DIF, PHS_DIF
                    END IF
               END IF
!
               FL_AMB_OBS = .FALSE.
               KP    = 0
               KP2   = 0
               IFRQ  = 0
               K_PHS = 0
               DO 480 J8=PIM%CONF%BEG_FRQ,PIM%CONF%END_FRQ
                  IFRQ = IFRQ + 1
!
! --------------- Compute complex residual with the feed horn angle rotation applied 
! --------------- with opposite sign coherenly averaged over KCHN channels of the segment
! --------------- and put them into the preliminary polarization complex bandpass
!
                  DO 490 J9=1,PIM%NCHN ! cycle over spectral channels
!
! ------------------ This means that the first station of the baseline is the
! ------------------ bandpass-reference station
!
                     IF ( POL_CONF == PIMA__PC_CC ) THEN
!
! ----------------------- Cir-cir polarization
!
                          IF ( ABS(RES(J9,IFRQ,1)) > PIMA__AMP_MIN ) THEN
                               PIM%PBP_INIT_CMPL(J9,J8,J2,J1) =  RES(J9,IFRQ,2)/RES(J9,IFRQ,1)
                             ELSE
                               PIM%PBP_INIT_CMPL(J9,J8,J2,J1) = CMPLX ( 1.0, 0.0 )
                          END IF
                          IF ( FL_PBP_PLOT ) THEN
                               K_PHS = K_PHS + 1
                               T1(K_PHS) = PIM%FREQ_ARR(J9,J8,FRG_IND)
                               X1(K_PHS) = PHAS_CMPL_R4 ( PIM%PBP_INIT_CMPL(J9,J8,J2,J1) )
                          END IF
                        ELSE IF ( POL_CONF == PIMA__PC_LL ) THEN
!
! ----------------------- Lin-lin polarization
!
                          IF ( J2 == 1 ) THEN
                               IF ( ABS(RES(J9,IFRQ,1)) > PIMA__AMP_MIN ) THEN
!
! --------------------------------- We put the ratio of amplitudeat 
!
                                    PIM%PBP_INIT_CMPL(J9,J8,J2,J1) =  RES(J9,IFRQ,4)/RES(J9,IFRQ,1)
                                 ELSE
                                    PIM%PBP_INIT_CMPL(J9,J8,J2,J1) =  CMPLX ( 1.0, 0.0 )
                                END IF
                                IF ( FL_PBP_PLOT ) THEN
                                     T1(J9) = PIM%FREQ_ARR(J9,J8,FRG_IND)
                                     X1(J9) = PHAS_CMPL_R4 ( RES(J9,IFRQ,4) )
                                     X2(J9) = PHAS_CMPL_R4 ( RES(J9,IFRQ,1) )
                                     X3(J9) = PHAS_CMPL_R4 ( PIM%PBP_INIT_CMPL(J9,J8,J2,J1) )
                                END IF
                             ELSE IF ( J2 == 2 ) THEN
                                IF ( ABS(RES(J9,IFRQ,3)) > PIMA__AMP_MIN ) THEN
                                     PIM%PBP_INIT_CMPL(J9,J8,J2,J1) = -RES(J9,IFRQ,2)/RES(J9,IFRQ,3)
                                   ELSE
                                     PIM%PBP_INIT_CMPL(J9,J8,J2,J1) =  CMPLX ( 0.0, 0.0 )
                                END IF
                                IF ( FL_PBP_PLOT ) THEN
                                     T1(J9) = PIM%FREQ_ARR(J9,J8,FRG_IND)
                                     X1(J9) = PHAS_CMPL_R4 ( RES(J9,IFRQ,2) )
                                     X2(J9) = PHAS_CMPL_R4 ( RES(J9,IFRQ,3) )
                                     X3(J9) = PHAS_CMPL_R4 ( PIM%PBP_INIT_CMPL(J9,J8,J2,J1) )
                                END IF
                             ELSE IF ( J2 == 3 ) THEN
                               IF ( ABS(RES(J9,IFRQ,1)) > PIMA__AMP_MIN ) THEN
                                    PIM%PBP_INIT_CMPL(J9,J8,J2,J1) =  RES(J9,IFRQ,2)/RES(J9,IFRQ,1)/TAN(FEED_ANG_DIF)
                                ELSE
                                    PIM%PBP_INIT_CMPL(J9,J8,J2,J1) =  CMPLX ( 0.0, 0.0 )
                                END IF
                             ELSE IF ( J2 == 4 ) THEN
                               IF ( ABS(RES(J9,IFRQ,2)) > PIMA__AMP_MIN ) THEN
                                    PIM%PBP_INIT_CMPL(J9,J8,J2,J1) =  RES(J9,IFRQ,4)/RES(J9,IFRQ,2)*TAN(FEED_ANG_DIF)
                                 ELSE
                                    PIM%PBP_INIT_CMPL(J9,J8,J2,J1) =  CMPLX ( 0.0, 0.0 )
                               END IF
                          END IF
                          IF ( FL_PBP_PLOT ) THEN
                              X1(J9) = PIM%PBP_INIT_CMPL(J9,J8,J2,J1)
                          END IF
                     END IF ! POL_CONF
 490              CONTINUE 
                  IF ( FL_PBP_PLOT .AND. POL_CONF == PIMA__PC_LL ) THEN
                       write ( 6, * ) ' ind_obs= ', ind_obs, ' j9= ', j9, ' mode_init= ', mode_init
                       call diagi_3 ( pim%nchn, t1, x1, pim%nchn, t1, x2, pim%nchn, t1, x3, ier )
                     ELSE IF ( FL_PBP_PLOT .AND. POL_CONF == PIMA__PC_CC ) THEN
                       write ( 6, * ) ' ind_obs= ', ind_obs, ' j9= ', j9, ' mode_init= ', mode_init, ' pim%nchn= ', pim%nchn ! %%%
                       call diagi_1 ( k_phs, t1, x1, ier )
                  END IF
!
! --------------- Compute the model of the polarization bandpass for the
! --------------- J8 -th IF of the J1-th station and write down in PIM%PBP_INIT_CMPL(x,J8)
!
                  CALL ERR_PASS ( IUER, IER )
                  CALL PIMA_INIT_PBP_STA ( PIM, MODE_INIT, IND_OBS, J1, LFRQ, J8, &
     &                                     IND_REF, IND_REM, PIM%PBP_INIT_CMPL(1,1,J2,J1), &
     &                                     AC_AVR, DRF_SEG(1,IFRQ), FREQ_SEG(1,IFRQ), &
     &                                     WEI_SEG(1,IFRQ), IER )
                  IF ( IER .NE. 0 ) THEN
                       CALL CLRCH ( STR )
                       CALL INCH  ( IND_OBS, STR )
                       CALL CLRCH ( STR1 )
                       CALL INCH  ( J8, STR1 )
                       CALL ERR_LOG ( 6395, IUER, 'PIMA_PBP_INIT', 'Error '// &
     &                     'in an attempt to model residuals for '// &
     &                     'observation '//TRIM(STR)//' IF '//STR1 )
                       RETURN
                  END IF
 480           CONTINUE
!
               IF ( MODE_INIT == 1 ) THEN
                    CALL ERR_PASS ( IUER, IER )
                    CALL PIMA_PBP_LIN ( PIM, J1, IND_OBS, KCHN, LFRQ, FRG_IND, &
     &                                  PIM%PBP_INIT_CMPL(1,1,J2,J1), DRF_SEG, &
     &                                  FREQ_SEG, WEI_SEG, IER )
                    IF ( IER .NE. 0 ) THEN
                         CALL CLRCH ( STR )
                         CALL INCH  ( IND_OBS, STR )
                         CALL ERR_LOG ( 6396, IUER, 'PIMA_PBP_INIT', 'Error '// &
     &                       'in an attempt to model segmented residuals for '// &
     &                       'observation '//TRIM(STR)//' using the linear model' )
                         RETURN
                    END IF
               END IF
!
! ------------ Perform polarizartion bandpass normalization
! ------------ NB: At this point the polarization amplitude bandpass 
! ------------ of the reference station is not known. 
! ------------ We use the J1-th station as the reference station
!
               SAVE_BPS_NORML = PIM%CONF%BPS_NORML 
               PIM%CONF%BPS_NORML = PIMA__NORML_NO 
               CALL BPASS_AMP_NRML ( PIM, J1, 0, &
     &                               PIM%PBP(J1)%CMPL(1,1), 0, &
     &                               AMPL_FRQ_NRML,         &
     &                               AMPL_BAND_NRML,        &
     &                               AMPL_FRQ_AVR(1,J1),    &
     &                               AMPL_FRQ_RMS(1,J1),    &
     &                               AMPL_TOT_RMS(J1),      &
     &                               AMPL_INTEGRAL(J1) )
               PIM%CONF%BPS_NORML = SAVE_BPS_NORML
               IND_OBS_PROC(J1,J2) = IND_OBS
!
! ------------ NB: In the INIT mode we process only eligible observation per baseline
!
               IF ( J2 == 2 ) THEN
                    IF ( SNR_MIN_REF(1) == 0.0 ) THEN
                         SNR_MIN_REF(1) = SNR_NEW(1)
                         SNR_MIN_REF(2) = SNR_NEW(2)
                       ELSE
                         IF ( IS_R8_NAN(SNR_NEW(1)) .OR. IS_R8_NAN(SNR_NEW(2)) ) THEN
                              WRITE ( 6, * ) 'SNR_NEW= ', SNR_NEW
                              CALL ERR_LOG ( 6397, IUER, 'PIMA_PBP_INIT', 'Trap of internal '// &
     &                            'control: SNR is not a number' )
                              RETURN 
                         END IF
                         IF ( MIN(SNR_NEW(1),SNR_NEW(2)) > MIN(SNR_MIN_REF(1),SNR_MIN_REF(2)) ) THEN
                              SNR_MIN_REF(1) = SNR_NEW(1)
                              SNR_MIN_REF(2) = SNR_NEW(2)
                         END IF
                    END IF
               END IF
               IF ( J1 == BPS%IND_STA_REF ) THEN
                    IND_OBS_REF(J2) = BPS%IND_OBS_SEL(J3,J1,J2)
                    TIM_FRT_REF(J2) = BPS%TIME_FRT(J3,J1,J2)
               END IF
!
! ------------  Set flag that we processed the J1-th station, the J2-th polarization combination
!
               FL_PROC(J2,J1) = .TRUE.
               GOTO 820
 430        CONTINUE 
 820        CONTINUE 
 420     CONTINUE 
 410  CONTINUE 
!
      IF ( PIM%CONF%DEBUG_LEVEL .GE. 4 ) THEN
           WRITE ( 6, '(A,4(2X,I5))' ) 'PIMA_PBP_INIT: IND_OBS_REF= ', IND_OBS_REF
      END IF
      JSTA = 0
      IF ( POL_CONF .NE. PIMA__PC_CC  ) THEN
           IF ( IND_OBS_REF(2) > 0 .AND. IND_OBS_REF(2) > 0 ) THEN
                IND_STA(1) = PIM%OBS(IND_OBS_REF(2))%STA_IND(1)
                IND_STA(2) = PIM%OBS(IND_OBS_REF(2))%STA_IND(2)
                IF ( IND_STA(1) == BPS%IND_STA_REF ) THEN
                     IND_REF =  1
                     IND_REM =  2
                     SGN_REM =  1
                     SGN_REF = -1
                     CALL PIMA_PBP_BAND_LIN ( PIM, PIM%PBP_INIT_CMPL(1,1,1,IND_STA(2)), &
     &                                        PHAS_INIT(1,1,1), GD_EST_R4(BPS%IND_STA_REF,1), IER )
                     CALL PIMA_PBP_BAND_LIN ( PIM, PIM%PBP_INIT_CMPL(1,1,2,IND_STA(2)), &
     &                                        PHAS_INIT(1,1,2), GD_EST_R4(BPS%IND_STA_REF,2), IER )
                  ELSE
                     IND_REF =  2
                     IND_REM =  1
                     SGN_REM = -1
                     SGN_REF =  1
                     CALL PIMA_PBP_BAND_LIN ( PIM, PIM%PBP_INIT_CMPL(1,1,1,IND_STA(1)), &
     &                                        PHAS_INIT(1,1,1), GD_EST_R4(BPS%IND_STA_REF,1), IER )
                     CALL PIMA_PBP_BAND_LIN ( PIM, PIM%PBP_INIT_CMPL(1,1,2,IND_STA(1)), &
     &                                        PHAS_INIT(1,1,2), GD_EST_R4(BPS%IND_STA_REF,2), IER )
                END IF
!
                IF ( PIM%CONF%DEBUG_LEVEL .GE. 3 ) THEN
                     WRITE ( 6, 210 ) PIM%C_STA(BPS%IND_STA_REF), GD_EST_R4(BPS%IND_STA_REF,1:2)
 210                 FORMAT ( 'PIMA_PBP_INIT STA= ', A, ' GD_est: ', 1PE12.5, 1X, 1PE12.5 )
                END IF
                IFRQ = 0
                DRF_ACC = 0.0
                DO 4100 J10=1,2
                   IF ( J10 == 1 ) SGN =  1
                   IF ( J10 == 2 ) SGN = -1
                   DO 4110 J11=PIM%CONF%BEG_FRQ,PIM%CONF%END_FRQ
                      IFRQ = IFRQ + 1
                      DRF_ACC(1) = 0.0
                      DRF_ACC(2) = 0.0
                      DO 4120 J12=1,PIM%NCHN ! cycle over spectral channels
                         IF ( FL_BMASK ) THEN
                              MASK_CHN = PIM%BANDPASS_MASK(J12,J11,IND_STA(1),PIMA__MASK_BPAS) * &
     &                                   PIM%BANDPASS_MASK(J12,J11,IND_STA(2),PIMA__MASK_BPAS)
                           ELSE 
                              MASK_CHN = 1
                         END IF
                         IF ( IND_STA(1) == BPS%IND_STA_REF ) THEN
                              AMP_R4(1) = ABS ( SQRT ( PIM%PBP_INIT_CMPL(J12,J11,1,IND_STA(2))*PIM%PBP_INIT_CMPL(J12,J11,2,IND_STA(2)) ) )
                              PHS_R4(1) = ( PHAS_INIT(J12,J11,1) + PHAS_INIT(J12,J11,2) )/2.0
                              PIM%PBP(BPS%IND_STA_REF)%CMPL(J12,J11) = SGN*CMPLX ( AMP_R4(1)*COS(PHS_R4(1)), AMP_R4(1)*SIN(PHS_R4(1)) )
!
                              AMP_R4(2) = ABS ( SQRT ( PIM%PBP_INIT_CMPL(J12,J11,1,IND_STA(2))*PIM%PBP_INIT_CMPL(J12,J11,2,IND_STA(2)) ) )
                              PHS_R4(2) = ( PHAS_INIT(J12,J11,1) - PHAS_INIT(J12,J11,2) )/2.0
                              PIM%PBP(IND_STA(2))%CMPL(J12,J11) = SGN*CONJG ( CMPLX ( AMP_R4(2)*COS(PHS_R4(2)), AMP_R4(2)*SIN(PHS_R4(2)) ) )
                              IF ( PIM%CONF%DEBUG_LEVEL .GE. 7 ) THEN
                                   WRITE ( 6, 220 ) PIM%C_STA(BPS%IND_STA_REF), J11, J12, &
     &                                              SGN_REM, PHAS_INIT(J12,J11,1), PHS_R4(1)
 220                               FORMAT ( 'PIMA_PBP_INIT STA= ', A, ' Ifrq= ', I2, ' Ichn= ', I3, ' Sgn_rem= ', I2, &
     &                                      ' Phs: ', F9.5, ' Cphs: ', F9.5 )
                              END IF
                           ELSE IF ( IND_STA(2) == BPS%IND_STA_REF ) THEN
                              AMP_R4(2) = ABS ( SQRT ( PIM%PBP_INIT_CMPL(J12,J11,1,IND_STA(1))/PIM%PBP_INIT_CMPL(J12,J11,2,IND_STA(1)) ) )
                              PHS_R4(2) = ( PHAS_INIT(J12,J11,1) - PHAS_INIT(J12,J11,2) )/2.0
                              PIM%PBP(BPS%IND_STA_REF)%CMPL(J12,J11) = SGN*CONJG ( CMPLX ( AMP_R4(2)*COS(PHS_R4(2)), AMP_R4(2)*SIN(PHS_R4(2)) ) )
                              AMP_R4(1) = ABS ( SQRT ( PIM%PBP_INIT_CMPL(J12,J11,1,IND_STA(2))*PIM%PBP_INIT_CMPL(J12,J11,2,IND_STA(2)) ) )
                              PHS_R4(1) = ( PHAS_INIT(J12,J11,1) + PHAS_INIT(J12,J11,2) )/2.0
                              PIM%PBP(IND_STA(1))%CMPL(J12,J11) = SGN*CMPLX ( AMP_R4(1)*COS(PHS_R4(1)), AMP_R4(1)*SIN(PHS_R4(1)) )
!
                              IF ( PIM%CONF%DEBUG_LEVEL .GE. 7 ) THEN
                                   WRITE ( 6, 220 ) PIM%C_STA(BPS%IND_STA_REF), J11, J12, &
     &                                              SGN_REM, PHAS_INIT(J12,J11,2), PHS_R4(2)
                              END IF
                              IF ( ABS(PIM%PBP_INIT_CMPL(J12,J11,4,IND_STA(2))) > PIMA__AMP_MIN ) THEN
                                   DRF_ACC(4) = DRF_ACC(4) + PIM%PBP(BPS%IND_STA_REF)%CMPL(J12,J11)/ &
     &                                                       CONJG ( PIM%PBP_INIT_CMPL(J12,J11,4,IND_STA(2)) )
                                   DRF_ACC(2) = DRF_ACC(2) + PIM%PBP(BPS%IND_STA_REF)%CMPL(J12,J11)/ &
     &                                                       CONJG ( PIM%PBP_INIT_CMPL(J12,J11,4,IND_STA(2)) )
                              END IF
!
                         END IF
 4120                 CONTINUE 
 4110              CONTINUE 
!
                   IND_OBS = IND_OBS_REF(2)
                   IND_POL = 1
                   FRG_IND = PIM%OBS(IND_OBS)%REF_FRG_INDS(PIM%CONF%FRQ_GRP) 
                   POL_CONF = PIMA_GET_POL_CONF ( PIM, IND_OBS, IER ) 
                   LTIM = PIM%OBS(IND_OBS)%NUM_EPC(PIM%OBS(IND_OBS)%REF_FRG_INDS(PIM%CONF%FRQ_GRP))
!
                   PIM%CONF%BANDPASS_USE  = PIMA__BPASS_PHS
                   POL_MODE = PIMA__IPLL
                   CALL ERR_PASS ( IUER, IER )
                   CALL PIMA_GET_RESID ( PIM, VTD, IND_OBS_REF(2), IND_POL, LCHN, LFRQ, LTIM, &
     &                                   GR_DEL(1,2), PH_RAT(1,2), GR_RAT(2), PHS(1,2), 0.0D0, &
     &                                   TIM_FRT_REF(1), PIMA__POLAR_I, PIMA__BPASS_PHS, POL_MODE, &
     &                                   RES, RES_AVR, SNR_NEW(J10), IER )
                   IF ( IER .NE. 0 ) THEN
                        CALL CLRCH ( STR )
                        CALL INCH  ( IND_OBS, STR )
                        CALL ERR_LOG ( 6394, IUER, 'PIMA_PBP_INIT', 'Error '// &
     &                      'in an attempt to compute residuals for '// &
     &                      'observation '//STR )
                        RETURN
                   END IF
!
                   GR_DEL_DIF = GR_DEL(IND_FRA,2) - GR_DEL(IND_FRA,1)
                   PH_RAT_DIF = PH_RAT(IND_FRA,2) - PH_RAT(IND_FRA,1)
                   PHS_DIF    = PHS(IND_FRA,2)    - PHS(IND_FRA,1) 
                   GR_RAT_DIF = GR_RAT(2)         - GR_RAT(1)
!
                   IF ( PIM%CONF%DEBUG_LEVEL .GE. 2 ) THEN
                        WRITE ( 6, 250 ) PIM%C_STA(BPS%IND_STA_REF), IND_OBS, SGN, &
     &                                   SNR_NEW(J10), PIM%C_STA(IND_STA(IND_REM)), GR_DEL_DIF
 250                    FORMAT ( 'PIM_PBP_INIT REF_STA: ', A, ' Ind_obs: ', I5, &
     &                           ' Sign: ', I2, ' SNR: ', F8.2, ' rem_sta: ', A, &
     &                           ' Gr_del_dif: ', F12.3, ' ns ' )
                   END IF
 4100           CONTINUE 
!
                IF ( SNR_NEW(1) > SNR_NEW(2) ) THEN
!
! ------------------ Flip the sign back
!
                     PIM%PBP(IND_STA(1))%CMPL(1:PIM%NCHN,PIM%CONF%BEG_FRQ:PIM%CONF%END_FRQ) = -PIM%PBP(IND_STA(1))%CMPL(1:PIM%NCHN,PIM%CONF%BEG_FRQ:PIM%CONF%END_FRQ)
                     PIM%PBP(IND_STA(2))%CMPL(1:PIM%NCHN,PIM%CONF%BEG_FRQ:PIM%CONF%END_FRQ) = -PIM%PBP(IND_STA(2))%CMPL(1:PIM%NCHN,PIM%CONF%BEG_FRQ:PIM%CONF%END_FRQ)
                     SGN = 1
                END IF 
                IF ( PIM%CONF%DEBUG_LEVEL .GE. 2 ) THEN
                     WRITE ( 6, '(A,I2)' ) 'PIM_PBP_INIT Used sign: ', SGN
                END IF
              ELSE
                IF ( PIM%CONF%WARNING ) THEN
                     WRITE ( 6, * ) 'PIM_PBP_INIT: no observations with '// &
     &                 'parallel and cross polarization suitable for computing '// &
     &                 'polarization band pass of the reference station '//PIM%C_STA(BPS%IND_STA_REF)
                END IF
           END IF
      END IF
!
      DO 4140 J14=1,PIM%NSTA
         IF ( IND_OBS_PROC(J14,1) .NE. 0 ) THEN
              IND_OBS = IND_OBS_PROC(J14,1)
            ELSE 
              IND_OBS = IND_OBS_PROC(J14,2)
         END IF
         IF ( IND_OBS < 1 ) GOTO 4140
         FRG_IND = PIM%OBS(IND_OBS)%REF_FRG_INDS(PIM%CONF%FRQ_GRP) 
         POL_CONF = PIMA_GET_POL_CONF ( PIM, IND_OBS, IER ) 
         IF ( J14 .EQ. BPS%IND_STA_REF ) GOTO 4140
         IF ( POL_CONF == PIMA__PC_CC ) THEN
              IF ( IND_OBS_PROC(J14,1) == 0 ) THEN
                   GOTO 4140
              END IF
           ELSE IF ( POL_CONF .NE. PIMA__PC_CC  ) THEN
              IF ( .NOT. ( FL_PROC(1,J14) .AND. FL_PROC(2,J14) ) ) THEN
!
! ---------------- Lin or mixed pol: we did not find observations for both
! ---------------- polarization combinations
!
                   IF ( PIM%CONF%DEBUG_LEVEL .GE. 1 ) THEN
                        WRITE ( 6, * ) 'PIM_PBP_INIT: Skip station ', PIM%C_STA(J14), &
     &                                 ' because of lack of suitable observations'
                   END IF
                   GOTO 4140
             END IF
         END IF
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
!
         IF ( POL_CONF .EQ. PIMA__PC_CC  ) THEN
!
! ----------- Apply differences in group delay and phase between LL and RR visibilites back
!
              CALL PIMA_APPLY_GR_DEL_PHS ( PIM%NCHN, LFRQ, PIM%FREQ_ARR(1,PIM%CONF%BEG_FRQ,PIM%CONF%FRQ_GRP), &
     &                                     FREQ_REF, -GR_DEL_POL_DIF(J14), -PHS_POL_DIF(J14), &
     &                                     PIM%PBP_INIT_CMPL(1,1,1,J14) )
            ELSE
              CALL PIMA_PBP_BAND_LIN ( PIM, PIM%PBP_INIT_CMPL(1,1,1,J14), &
     &                                 PHAS_INIT(1,1,1), GD_EST_R4(J14,1), IER )
              CALL PIMA_PBP_BAND_LIN ( PIM, PIM%PBP_INIT_CMPL(1,1,2,J14), &
     &                                 PHAS_INIT(1,1,2), GD_EST_R4(J14,2), IER )
              IF ( PIM%CONF%DEBUG_LEVEL .GE. 3 ) THEN
                   WRITE ( 6, 210 ) PIM%C_STA(J14), GD_EST_R4(J14,1:2)
              END IF
         END IF
!
         IFRQ  = 0
         K_PHS = 0
         PHS_DIF_AVR_ACC_ALL = 0.0D0
         PHS_DIF_SQR_ACC_ALL = 0.0D0
         DO 4150 J15=PIM%CONF%BEG_FRQ,PIM%CONF%END_FRQ
            IFRQ = IFRQ + 1
            J_PHS = 0
            CMPL_DIF_AVR_ACC_IF = 0.0
            PHS_DIF_AVR_ACC_IF  = 0.0D0
            PHS_DIF_SQR_ACC_IF  = 0.0D0
            DO 4160 J16=1,PIM%NCHN ! cycle over spectral channels
               IF ( FL_BMASK ) THEN
                    MASK_CHN = PIM%BANDPASS_MASK(J16,J15,IND_STA(1),PIMA__MASK_BPAS) * &
     &              PIM%BANDPASS_MASK(J16,J15,IND_STA(2),PIMA__MASK_BPAS)
                  ELSE 
                    MASK_CHN = 1
               END IF
               IF ( MASK_CHN .NE. 1 ) GOTO 4160
               IF ( POL_CONF .EQ. PIMA__PC_CC  ) THEN
!
! ----------------- Update the accumulator
!
                    IF ( IND_STA(1) == BPS%IND_STA_REF ) THEN
!
! ---------------------- The first station of the baseline is the reference station.
! ---------------------- NB: PIMA applies bandpass as Vis/( conjg(B1) * B2 )
!
                         PIM%PBP(J14)%CMPL(J16,J15) =         PIM%PBP_INIT_CMPL(J16,J15,1,J14) 
                       ELSE
                         PIM%PBP(J14)%CMPL(J16,J15) = CONJG ( PIM%PBP_INIT_CMPL(J16,J15,1,J14) )
                    END IF
                    PIM%PBP(BPS%IND_STA_REF)%CMPL(J16,J15) = ABS(PIM%PBP(J14)%CMPL(J16,J15))* &
     &                                       ABS(PIM%PBP(BPS%IND_STA_REF)%CMPL(J16,J15))
!
! ----------------- Update the counter of how many times the accumulator has been updated
!
                    JSTA(J16,IFRQ,1,J14) = JSTA(J16,IFRQ,1,J14) + 1
                    JSTA(J16,IFRQ,1,BPS%IND_STA_REF) = JSTA(J16,IFRQ,1,BPS%IND_STA_REF) + 1
                  ELSE IF ( POL_CONF .NE. PIMA__PC_CC  ) THEN
!
! ----------------- Compute the bandpass of the baseline reference station
!
                    AMP_R4(1) = ABS ( SQRT ( PIM%PBP_INIT_CMPL(J16,J15,1,J14)*PIM%PBP_INIT_CMPL(J16,J15,2,J14) ) )
                    PHS_R4(1) = ( PHAS_INIT(J16,J15,1) + PHAS_INIT(J16,J15,2) )/2.0
                    IF ( IND_STA(1) == BPS%IND_STA_REF ) THEN
                         REF_CMPL(J16,J15)          = SGN*CMPLX ( AMP_R4(1)*COS(PHS_R4(1)), AMP_R4(1)*SIN(PHS_R4(1)) )
                       ELSE
                         PIM%PBP(J14)%CMPL(J16,J15) = SGN*CMPLX ( AMP_R4(1)*COS(PHS_R4(1)), AMP_R4(1)*SIN(PHS_R4(1)) )
                    END IF
!
! ----------------- Compute the bandpass of the baseline remote station
!
                    AMP_R4(2) = ABS ( SQRT ( PIM%PBP_INIT_CMPL(J16,J15,1,J14)/PIM%PBP_INIT_CMPL(J16,J15,2,J14) ) )
                    PHS_R4(2) = ( PHAS_INIT(J16,J15,1) - PHAS_INIT(J16,J15,2) )/2.0
                    IF ( SGN_REM == 1 ) THEN
                         PIM%PBP(J14)%CMPL(J16,J15) = SGN*CONJG ( CMPLX ( AMP_R4(2)*COS(PHS_R4(2)), AMP_R4(2)*SIN(PHS_R4(2)) ) )
                       ELSE
                         REF_CMPL(J16,J15)          = SGN*CONJG ( CMPLX ( AMP_R4(2)*COS(PHS_R4(2)), AMP_R4(2)*SIN(PHS_R4(2)) ) )
                    END IF
!
                    PHAS_DIF = PHAS_CMPL_R4 ( REF_CMPL(J16,J15) ) - PHAS_CMPL_R4 ( PIM%PBP(BPS%IND_STA_REF)%CMPL(J16,J15) )
                    CMPL_DIF_AVR_ACC_IF = CMPLX ( COS(PHAS_DIF), SIN(PHAS_DIF) )
!
! ----------------- Resolve 2pi ambiguity and update the accumulator of the average phase
! ----------------- NB: the average phase may have 1pi ambiguity
!
                    PHAS_DIF = PHAS_DIF - PI2 * NINT ( PHAS_DIF/PI2 )
                    PHS_DIF_AVR_ACC_IF = PHS_DIF_AVR_ACC_IF + PHAS_DIF
                    IF ( PIM%CONF%DEBUG_LEVEL .GE. 7 ) THEN
                         WRITE ( 6, 230 ) PIM%C_STA(J14), J15, J16, SGN_REM, &
     &                                    PHAS_INIT(J16,J15,1), PHAS_INIT(J16,J15,2), &
     &                                    PHS_R4(1), PHS_R4(2), PHAS_DIF
 230                     FORMAT ( 'PIMA_PBP_INIT STA= ', A, ' Ifrq= ', I2, ' Ichn= ', I3, ' Sgr= ', I2, &
     &                            ' Phs_1: ', F9.5, ' Phs_2: ', F9.5, &
     &                            ' Cphs_1: ', F9.5, ' Cphs_2: ', F9.5, ' PHAS_DIF: ', F9.5 )
                    END IF
!
! ----------------- Resolve 1pi ambiguity and update the accumulator of phase squares
!
                    PHAS_DIF = PHAS_DIF - PI__NUM * NINT ( PHAS_DIF/PI__NUM )
                    PHS_DIF_SQR_ACC_IF = PHS_DIF_SQR_ACC_IF + PHAS_DIF**2
!
                    J_PHS = J_PHS + 1
                    K_PHS = K_PHS + 1
                    IF ( FL_PBP_PLOT ) THEN
                         T1(K_PHS) = PIM%FREQ_ARR(J16,J15,FRG_IND)
                         X1(K_PHS) = PHAS_CMPL_R4 ( PIM%PBP(BPS%IND_STA_REF)%CMPL(J16,J15) )
                         X2(K_PHS) = PHAS_CMPL_R4 ( REF_CMPL(J16,J15) )
                         X3(K_PHS) = PHAS_DIF
                    END IF
                END IF
 4160       CONTINUE 
            IF ( J_PHS > 0  .AND. POL_CONF .NE. PIMA__PC_CC  ) THEN
!
! -------------- Check whether we need resolve a Pi-ambiguity jump
!
                 PHS_DIF_AVR_ACC_IF = PHAS_CMPL_R4 ( CMPL_DIF_AVR_ACC_IF )
                 IAMB = NINT ( PHS_DIF_AVR_ACC_IF/PI__NUM ) 
                 IF ( IAMB .NE. 0 ) THEN
!
! ------------------- Yes, we need
!
                      DO 4180 J18=1,PIM%NCHN
                         PIM%PBP(J14)%CMPL(J18,J15) = -PIM%PBP(J14)%CMPL(J18,J15)
 4180                 CONTINUE 
                      IF ( PIM%CONF%DEBUG_LEVEL .GE. 4 ) THEN
                           WRITE ( 6, 240 ) PIM%C_STA(J14), J15, IAMB, PHS_DIF_AVR_ACC_IF
 240                       FORMAT ( 'PIMA_PBP_INIT c_sta: ', A, ' Ind_frq: ', I2, ' Iamb= ', I2, ' Phs_dif: ', F6.3 )
                      END IF
                      PHS_DIF_AVR_ACC_IF = PHS_DIF_AVR_ACC_IF - IAMB*PI__NUM
                    ELSE
                      IF ( PIM%CONF%DEBUG_LEVEL .GE. 4 ) THEN
                           WRITE ( 6, 240 ) PIM%C_STA(J14), J15, 0, PHS_DIF_AVR_ACC_IF
                      END IF
                 END IF
                 PHS_DIF_AVR_ACC_ALL  = PHS_DIF_SQR_ACC_ALL + PHS_DIF_AVR_ACC_IF*J_PHS
                 PHS_DIF_SQR_ACC_ALL  = PHS_DIF_SQR_ACC_ALL + PHS_DIF_SQR_ACC_IF
            END IF 
 4150    CONTINUE 
         IF ( K_PHS > 0 .AND. POL_CONF .NE. PIMA__PC_CC  ) THEN
              PHS_DIF_AVR = PHS_DIF_AVR_ACC_ALL/K_PHS
              PHS_DIF_RMS = SQRT ( abs(PHS_DIF_SQR_ACC_ALL - PHS_DIF_AVR**2)/K_PHS  )
              IF ( FL_PBP_PLOT )  THEN
                   CALL DIAGI_3 ( K_PHS, T1, X1, K_PHS, T1, X2, K_PHS, T1, X3, IER )
              END IF
         END IF
         IF ( PIM%CONF%DEBUG_LEVEL .GE. 3 .AND. POL_CONF .NE. PIMA__PC_CC  ) THEN
              WRITE ( 6, 260 ) PIM%C_STA(J14), PHS_DIF_AVR, PHS_DIF_RMS 
 260          FORMAT ( 'PIMA_PBP_INIT Sta= ', A, ' Ref_sta_phs_dif: ', F8.4, ' Ref_sta_phs_rms: ', F8.4 )
         END IF
 4140 CONTINUE 
!
      IF ( NUM_PROC_OBS == 0 ) THEN
           CALL ERR_LOG ( 6397, IUER, 'PIMA_PBP_INIT', 'No observations suitable for '// &
     &         'bandpass computation were found' )
           RETURN 
      END IF
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
           DO 4190 J19=PIM%CONF%BEG_FRQ,PIM%CONF%END_FRQ
              IFRQ = IFRQ + 1
              DO 4200 J20=1,PIM%NCHN ! cycle over spectral channels
                 IF ( JSTA(J20,IFRQ,1,BPS%IND_STA_REF) > 0 .AND. &
     &                ABS(PIM%PBP(BPS%IND_STA_REF)%CMPL(J20,J19)) > PIM%CONF%BPS_AMP_MIN**JSTA(J20,IFRQ,1,BPS%IND_STA_REF) ) THEN
                      PIM%PBP(BPS%IND_STA_REF)%CMPL(J20,J19) = &
     &                    ABS(PIM%PBP(BPS%IND_STA_REF)%CMPL(J20,J19))**(1.0/(2.0*JSTA(J20,IFRQ,1,BPS%IND_STA_REF)))
                 END IF
 4200         CONTINUE
 4190      CONTINUE
      END IF
!
      IF ( .NOT. ASSOCIATED ( PIM%PBP(BPS%IND_STA_REF)%CMPL ) ) THEN
           CALL CLRCH ( STR )
           CALL ERR_LOG ( 6399, IUER, 'PIMA_PBP_INIT', 'Trap of internal control: '// &
     &         'bandpass for the reference station '//PIM%C_STA(BPS%IND_STA_REF)// &
     &         ' was not associated' ) 
           RETURN
      END IF
!
! --- Perform normalization of the polarization bandpass of the reference station
!
      CALL BPASS_AMP_NRML ( PIM, BPS%IND_STA_REF, BPS%IND_STA_REF, &
     &                      PIM%PBP(BPS%IND_STA_REF)%CMPL(1,1), PIM%PBP(BPS%IND_STA_REF)%CMPL(1,1), &
     &                      AMPL_FRQ_NRML,                   &
     &                      AMPL_BAND_NRML,                  &
     &                      AMPL_FRQ_AVR(1,BPS%IND_STA_REF), &
     &                      AMPL_FRQ_RMS(1,BPS%IND_STA_REF), &
     &                      AMPL_TOT_RMS(BPS%IND_STA_REF),   &
     &                      AMPL_INTEGRAL(BPS%IND_STA_REF)   )
!
! --- Now we renormalize amplitudes of the polarization bandpass 
! --- for all remote stations. Before the amplitude bandpass 
! --- of the reference station was assumed implicitly to be one. 
! --- We have just changed it and we need to recompute the 
! --- amplitude bandpass of remotes stations
!
      DO 4220 J22=1,PIM%NSTA
         IF ( J22 .NE. BPS%IND_STA_REF ) THEN
              IF ( .NOT. ASSOCIATED ( PIM%PBP(J22)%CMPL ) ) THEN
                  GOTO 4220
              END IF
              IFRQ = 0
              DO 4230 J23=PIM%CONF%BEG_FRQ,PIM%CONF%END_FRQ 
                 IFRQ = IFRQ + 1
                 DO 4240 J24=1,PIM%NCHN
                    IF ( ABS(PIM%PBP(BPS%IND_STA_REF)%CMPL(J24,J23)) > PIMA__AMP_MIN ) THEN
                         PIM%PBP(J22)%CMPL(J24,J23) = PIM%PBP(J22)%CMPL(J24,J23)/ &
     &                                ABS(PIM%PBP(BPS%IND_STA_REF)%CMPL(J24,J23))
                       ELSE
                         PIM%PBP(J22)%CMPL(J24,J23) = CMPLX(1.0,0.0)
                    END IF
 4240            CONTINUE 
 4230         CONTINUE 
!
! ----------- And we need also to renormalize it
!
              CALL BPASS_AMP_NRML ( PIM, J22, BPS%IND_STA_REF, &
     &             PIM%PBP(J22)%CMPL(1,1), PIM%PBP(BPS%IND_STA_REF)%CMPL(1,1), &
     &             AMPL_FRQ_NRML,         &
     &             AMPL_BAND_NRML,        &
     &             AMPL_FRQ_AVR(1,J22),   &
     &             AMPL_FRQ_RMS(1,J22),   &
     &             AMPL_TOT_RMS(J22),     &
     &             AMPL_INTEGRAL(J22) )
         END IF
 4220 CONTINUE 
!
      DEALLOCATE ( WEI_SEG      )
      DEALLOCATE ( REF_CMPL     )
      DEALLOCATE ( PHAS_INIT    )
      DEALLOCATE ( JSTA         )
      DEALLOCATE ( FREQ_SEG     )
      DEALLOCATE ( DRF_SEG      )
      DEALLOCATE ( REF_STA_CMPL )
      DEALLOCATE ( AMPL_FRQ_RMS )
      DEALLOCATE ( AMPL_FRQ_AVR )
      DEALLOCATE ( AC_AVR )
      DEALLOCATE ( RES )
!
      PIM%L_STA_PBP = L_PBP 
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  PIMA_PBP_INIT  !#!#  
