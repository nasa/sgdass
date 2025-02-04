      SUBROUTINE PIMA_BPASS_ACCUM ( PIM, VTD, BPS, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  PIMA_BPASS_ACCUM  computes station-dependend bandpass     *
! *   in the accumulated mode.                                           *
! *                                                                      *
! * ### 25-JAN-2009 PIMA_BPASS_ACCUM  v4.0 (c) L. Petrov 06-NOV-2020 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'astro_constants.i'
      INCLUDE   'pima.i'
      INCLUDE   'vtd.i'
      TYPE     ( PIMA__TYPE         ) :: PIM
      TYPE     ( PIM_BPS_STA__TYPE  ) :: BPS
      TYPE     ( VTD__TYPE          ) :: VTD
      INTEGER*4  IUER
      COMPLEX*8, ALLOCATABLE :: RES(:,:), RES_TEMP(:,:)
      REAL*4,    ALLOCATABLE :: ACC_AMPL(:,:,:), ACC_PHAS(:,:,:), ACC_RATE(:,:)
      REAL*4,    ALLOCATABLE :: AC_AVR(:,:,:)
      CHARACTER  STR*128, POLAR_SAVE*8
      COMPLEX*8  RES_AVR(2), RES_CMPL, BPS_REF_CMPL, BPS_REM_CMPL, &
     &           BPS_REF_AVR_CMPL, BPS_REM_AVR_CMPL
      REAL*4     AMPL_FRQ_AVR(PIM%NFRQ), AMPL_FRQ_RMS(PIM%NFRQ), &
     &           AMPL_TOT_RMS, AMPL_INTEGRAL, AMPL_FRQ_NRML(PIM__MFRQ), &
     &           AMPL_BAND_NRML, P1, P2
      REAL*4     AMPL, PHAS, WEI_SUM, NSUM, AMPL_RES(PIM__MCHN), &
     &           PHAS_RES_INIT(PIM__MCHN), PHAS_RES_PCAL(PIM__MCHN), &
     &           PHAS_MOD(PIM__MCHN,PIM__MFRQ), &
     &           AMPL_MOD(PIM__MCHN,PIM__MFRQ), PHAS_RMS, AMPL_RMS, &
     &           FREQ_CHN(PIM__MCHN), &
     &           AMPL_OUT(PIM__MCHN,PIM__MFRQ), PHAS_OUT(PIM__MCHN,PIM__MFRQ), &
     &           FREQ_ARR_R4(PIM__MCHN,PIM__MFRQ), BPS_AMP, &
     &           BPS_REM_AMP_AVR, BPS_REF_AMP_AVR
      REAL*8     RES_PHS_ARR(PIM__MFRQ), RES_AMP_ARR(PIM__MFRQ), SNR_NEW, &
     &           GR_DEL(PIM__MFRA), PH_RAT(PIM__MFRA), PHS(PIM__MFRA)
      REAL*4     AMPL_CAL_CH1(2), PHAS_CAL_CH1(2), AMPL_CAL(2), PHAS_CAL(2)
      REAL*8     PHAS_RMS_MAX
      PARAMETER  ( PHAS_RMS_MAX = 1.0D0 )
      INTEGER*4    MDBG
      PARAMETER  ( MDBG = 64*1024 )
      REAL*8     FRQ_DBG(MDBG), AMPL_ORIG_DBG(MDBG), AMPL_OUT_DBG(MDBG), &
     &           PHAS_ORIG_DBG(MDBG), PHAS_OUT_DBG(MDBG), AC_BPS_DBG(MDBG), &
     &           PHAS_AVR_DBG(PIM__MFRQ), FREQ_OUT_DBG(MDBG)
      REAL*8     X1(MDBG), X2(MDBG), X3(MDBG)
      LOGICAL*1  FL_BMASK
      INTEGER*4  MASK_CHN
      REAL*4     WEI__MIN
      PARAMETER  ( WEI__MIN = 1.E-12 ) ! Minumum weight
      INTEGER*4  IND_STA(2), IND_REF, IND_REM
      INTEGER*1  SGN_REM, SGN_REF
      INTEGER*4  LCHN, LFRQ, LTIM, IFRQ, IND_FRQ, IND_OBS, IP, KP, LP, L_OBS, &
     &           J1, J2, J3, J4, J5, J6, J7, J8, J9, J10, J11, J12, J13, J14, &
     &           J15, J16, NSEG_USED, IAMB, ICHN, KCHN, MOD_AMB, IND, IND_FRA, &
     &           KCHN_ARR(PIM__MCHN,PIM__MFRQ), L_ACC, LP_1ST, POL_MODE, ISTA, &
     &           PCI, IND_POL, IER
      CHARACTER  POL_CONF*7
      LOGICAL*4  FL_AMBIG_RES
      CHARACTER, EXTERNAL :: PIMA_GET_POL_CONF*7
      REAL*4,    EXTERNAL :: ATAN_CS_R4, PHAS_CMPL_R4
      INTEGER*4, EXTERNAL :: ILEN, I_LEN, LTM_DIF
!
      LCHN = PIM%NCHN
      LFRQ = PIM%CONF%END_FRQ - PIM%CONF%BEG_FRQ + 1
!
      IF ( PIM%CONF%DEBUG_LEVEL .GE. 3 ) THEN
           WRITE ( 6, * ) '--------------------------- ' 
           WRITE ( 6, * ) ' PIMA_BPASS_ACCUM           ' 
           WRITE ( 6, * ) '--------------------------- ' 
           CALL FLUSH ( 6 )
      END IF
      IF ( PIM%CONF%BPS_NORML == PIMA__NORML_NO ) THEN
           CALL ERR_LOG ( 6571, IUER, 'PIMA_BPASS_ACCUM', 'Bandpass '// &
     &         'noramlization is set to '//PIMA__NORML_NO//'. Please '// &
     &         'set BPS.NORML: to IF or BAND' )
           RETURN
      END IF
!
      FL_AMBIG_RES = .FALSE.
      CALL GETENVAR ( 'PIMAVAR_BPAS_ACCUM_AMBRES', STR )
      IF ( STR == 'YES' ) FL_AMBIG_RES = .TRUE.
      IF ( PIM%CONF%BPS_MSEG_ACCUM .LE. 0 ) PIM%CONF%BPS_MSEG_ACCUM = 1
      KCHN = PIM%NCHN/PIM%CONF%BPS_MSEG_ACCUM
!
      IF ( FL_AMBIG_RES ) THEN
           MOD_AMB = 2
         ELSE
           MOD_AMB = 1
      END IF
!
      IF ( PIM%CONF%FRIB_FINE_SEARCH == PIMA__FINE_SEARCH_LSQ ) THEN
           IND_FRA = PIMA__LSQ
         ELSE
           IND_FRA = PIMA__DRF
      END IF
!
      ALLOCATE ( RES(LCHN,PIM%NFRQ), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR  )
           CALL INCH  ( 8*LCHN*PIM%NFRQ, STR )
           CALL ERR_LOG ( 6572, IUER, 'PIMA_BPASS_ACCUM', 'Error in an '// &
     &         'attempt to allocate '//STR(1:I_LEN(STR))//' bytes of '// &
     &         'dynamic memory for residuals' )
           RETURN
      END IF
      RES = 0.0
!
      ALLOCATE ( AC_AVR(LCHN,LFRQ,2), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR  )
           CALL INCH  ( 8*LCHN*LFRQ, STR )
           CALL ERR_LOG ( 6573, IUER, 'PIMA_BPASS_ACCUMUM', 'Error in an '// &
     &         'attempt to allocate '//STR(1:I_LEN(STR))//' bytes of '// &
     &         'dynamic memory for array autspectrum AC_AVR' )
           RETURN
      END IF
      AC_AVR = 0.0
!
      ALLOCATE ( RES_TEMP(LCHN,PIM%NFRQ), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR  )
           CALL INCH  ( 8*LCHN*PIM%NFRQ, STR )
           CALL ERR_LOG ( 6574, IUER, 'PIMA_BPASS_ACCUM', 'Error in an '// &
     &         'attempt to allocate '//STR(1:I_LEN(STR))//' bytes of '// &
     &         'dynamic memory for residuals' )
           RETURN
      END IF
      RES_TEMP = 0.0
!
      ALLOCATE ( ACC_AMPL(LCHN,LFRQ,PIM%NSTA), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR  )
           CALL INCH  ( 4*LCHN*LFRQ*PIM%NSTA, STR )
           CALL ERR_LOG ( 6575, IUER, 'PIMA_BPASS_ACCUM', 'Error in an '// &
     &         'attempt to allocate '//STR(1:I_LEN(STR))//' bytes of '// &
     &         'dynamic memory for the accumalted bandpass amplitude' )
           DEALLOCATE ( RES      )
           RETURN
      END IF
      ACC_AMPL = 1.0
!
      ALLOCATE ( ACC_PHAS(LCHN,LFRQ,PIM%NSTA), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR  )
           CALL INCH  ( 4*LCHN*LFRQ*PIM%NSTA, STR )
           CALL ERR_LOG ( 6576, IUER, 'PIMA_BPASS_ACCUM', 'Error in an '// &
     &         'attempt to allocate '//STR(1:I_LEN(STR))//' bytes of '// &
     &         'dynamic memory for the accumalted bandpass phase' )
           DEALLOCATE ( ACC_AMPL )
           DEALLOCATE ( RES      )
           RETURN
      END IF
      ACC_PHAS = 1.0
!
      ALLOCATE ( ACC_RATE(LFRQ,PIM%NSTA), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR  )
           CALL INCH  ( 4*LFRQ*PIM%NSTA, STR )
           CALL ERR_LOG ( 6577, IUER, 'PIMA_BPASS_ACCUM', 'Error in an '// &
     &         'attempt to allocate '//STR(1:I_LEN(STR))//' bytes of '// &
     &         'dynamic memory for the accumalted bandpass phase-cal rates' )
           DEALLOCATE ( ACC_PHAS )
           DEALLOCATE ( ACC_AMPL )
           DEALLOCATE ( RES      )
           RETURN
      END IF
      ACC_RATE = 0.0
!
      IF ( PIM%CONF%BANDPASS_MASK_FILE .NE. PIMA__BPASS_NO  .AND. &
     &     ASSOCIATED ( PIM%BANDPASS_MASK )                       ) THEN
           FL_BMASK = .TRUE.
        ELSE
           FL_BMASK = .FALSE.
      END IF
!
      DO 410 J1=1,PIM%NSTA
         IF ( PIM%C_STA(J1) == PIM%CONF%STA_REF ) GOTO 410
         L_ACC = 0
         PCI = PIM%BPS%PCI(J1)
         IF ( PCI < 1 ) GOTO 410
         IF ( BPS%NUM_OBS_ACCUM(J1,PCI) > 0 ) THEN
              L_OBS = 0
              WEI_SUM = 0.0
              NSUM    = 0.0
              DO 420 J2=2,BPS%NUM_OBS_ACCUM(J1,PCI)+1
                 IF ( DABS(BPS%SNR(J2,J1,PCI)) < PIM%CONF%BPS_SNR_MIN_ACCUM ) GOTO 420
                 IND_OBS = BPS%IND_OBS_SEL(J2,J1,PCI)
                 IF ( PIM%OBS(IND_OBS)%REF_FRG_INDS(PIM%CONF%FRQ_GRP) == 0 ) THEN
                      GOTO 420
                 END IF
!
                 LTIM = PIM%OBS(IND_OBS)%NUM_EPC(PIM%OBS(IND_OBS)%REF_FRG_INDS(PIM%CONF%FRQ_GRP))
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
                 CALL ERR_PASS ( IUER, IER )
                 POL_CONF = PIMA_GET_POL_CONF ( PIM, IND_OBS, IER ) 
                 IF ( IER .NE. 0 ) THEN
                      CALL ERR_LOG ( 7588, IUER, 'PIMA_BPASS_ACCUM', 'Trap of internal control: '// &
     &                    'error in computation of the polarization code' )
                      CALL EXIT ( 1 )
                      RETURN
                 END IF
!
! -------------- Summary:
! -------------- 1) bandpass phases over frequency channels are flat
! -------------- 2) ALL phase cal tones are applied
! -------------- 3) pcal_rate is used for pcal ambiguity resolution
! -------------- 4) We have BPASS%PH_RAT that allows to resolve ambiguity
!
                 GR_DEL(IND_FRA) = BPS%GR_DEL(J2,J1,PCI)
                 PH_RAT(IND_FRA) = BPS%PH_RAT(J2,J1,PCI)
                 PHS(IND_FRA) = BPS%PHS(J2,J1,PCI)
!
! -------------- Compute residuals with applying phase (but not the amplitude!)
! -------------- of the bandpass from the init run
!
                 CALL ERR_PASS  ( IUER, IER )
                 PIM%BPASS(IND_STA(IND_REM))%BPS(1:PIM%NCHN,1:PIM%NFRQ) = BPS%CMPL(1:PIM%NCHN,1:PIM%NFRQ,IND_STA(IND_REM))
                 PIM%BPASS(IND_STA(IND_REF))%BPS(1:PIM%NCHN,1:PIM%NFRQ) = BPS%CMPL(1:PIM%NCHN,1:PIM%NFRQ,IND_STA(IND_REF))
                 IF ( POL_CONF == PIMA__PC_CC ) THEN
                      IF ( PIM%CONF%POLAR == PIMA__POLAR_I ) THEN
                           IF ( PIM%STA(PIM%BPS%IND_STA_REF)%POL_TYP(1) == PIMA__POL_R ) THEN
                                POL_MODE = PIMA__RRCC
                              ELSE
                                POL_MODE = PIMA__LLCC
                           END IF
                         ELSE IF ( PIM%CONF%POLAR == PIMA__POLAR_RR ) THEN
                           POL_MODE = PIMA__RRCC
                         ELSE IF ( PIM%CONF%POLAR == PIMA__POLAR_LL ) THEN
                           POL_MODE = PIMA__LLCC
                         ELSE IF ( PIM%CONF%POLAR == PIMA__POLAR_RL ) THEN
                           POL_MODE = PIMA__RLCC
                         ELSE IF ( PIM%CONF%POLAR == PIMA__POLAR_LR ) THEN
                           POL_MODE = PIMA__LRCC
                      END IF
                    ELSE IF ( POL_CONF == PIMA__PC_LL ) THEN
                      POL_MODE = PIMA__HHLL
                    ELSE IF ( POL_CONF == PIMA__PC_LC ) THEN
                      POL_MODE = PIMA__HRLC
                    ELSE IF ( POL_CONF == PIMA__PC_CL ) THEN
                      POL_MODE = PIMA__RHCL
                 END IF
                 IND_POL = 1
                 CALL PIMA_GET_RESID ( PIM, VTD, IND_OBS, IND_POL, &
     &                      LCHN, LFRQ, LTIM, &
     &                      GR_DEL, PH_RAT, BPS%GR_RAT(J2,J1,PCI), PHS, &
     &                      BPS%SNR(J2,J1,PCI), BPS%TIME_FRT(J2,J1,PCI), &
     &                      PIM%CONF%FRIB_SEARCH_TYPE, PIMA__BPASS_PHS, &
     &                      POL_MODE, RES, RES_AVR, SNR_NEW, IER )
                 IF ( IER .NE. 0 ) THEN
                      CALL CLRCH ( STR )
                      CALL INCH  ( IND_OBS, STR )
                      CALL ERR_LOG ( 6578, IUER, 'PIMA_BPASS_ACCUM', 'Error in '// &
     &                    'an attempt to compute residuals for observation '// &
     &                     STR )
                      DEALLOCATE ( ACC_RATE )
                      DEALLOCATE ( ACC_PHAS )
                      DEALLOCATE ( ACC_AMPL )
                      DEALLOCATE ( RES      )
                      RETURN
                 END IF
                 BPS%GR_DEL(J2,J1,PCI) = GR_DEL(IND_FRA)
                 BPS%PH_RAT(J2,J1,PCI) = PH_RAT(IND_FRA)
                 BPS%PHS(J2,J1,PCI)    = PHS(IND_FRA)
!
! -------------- NB: RES has IF frequencies run from 1 to LFRQ, while
! -------------- BPASS_AMP_NRML expects to run from
! -------------- PIM%CONF%BEG_FRQ to PIM%CONF%END_FRQ.
! -------------- To circumvent this problem, we use the temporary array
! -------------- RES_TEMP for normalization, then move the residuals back
!
                 IF ( SGN_REM == -1 ) THEN
                      RES_TEMP(1:PIM%NCHN,PIM%CONF%BEG_FRQ:PIM%CONF%END_FRQ) = &
     &                         CONJG(RES(1:PIM%NCHN,1:LFRQ))/CONJG(RES_AVR(1))
                    ELSE
                      RES_TEMP(1:PIM%NCHN,PIM%CONF%BEG_FRQ:PIM%CONF%END_FRQ) = &
     &                         RES(1:PIM%NCHN,1:LFRQ)/RES_AVR(1)
                 END IF
!
! -------------- Normalization of the amplitude of residuals RES
!
                 CALL BPASS_AMP_NRML ( PIM, J1, BPS%IND_STA_REF, &
     &                                 RES_TEMP, BPS%CMPL(1,1,BPS%IND_STA_REF), &
     &                                 AMPL_FRQ_NRML,  &
     &                                 AMPL_BAND_NRML, &
     &                                 AMPL_FRQ_AVR,   &
     &                                 AMPL_FRQ_RMS,   &
     &                                 AMPL_TOT_RMS,   &
     &                                 AMPL_INTEGRAL )
                 RES(1:PIM%NCHN,1:LFRQ) = &
     &               RES_TEMP(1:PIM%NCHN,PIM%CONF%BEG_FRQ:PIM%CONF%END_FRQ)
                 WEI_SUM = WEI_SUM + ABS(RES_AVR(1))
                 NSUM    = NSUM + 1
                 BPS%SNR(J2,J1,PCI) = SNR_NEW
!
! -------------- Now compute residuals with respect to the initial bandpass
!
                 L_OBS = L_OBS + 1
                 PHAS_RMS = 0.0
                 AMPL_RMS = 0.0
                 IFRQ = 0
                 NSEG_USED = 0
                 KP = 0
                 LP = 0
                 DO 450 J5=PIM%CONF%BEG_FRQ,PIM%CONF%END_FRQ
                    IFRQ = IFRQ + 1
                    ICHN = 0
                    IP = 0
                    DO 460 J6=1,KCHN ! cycle over spectral channels
                       FREQ_CHN(J6) = 0.0
                       RES_CMPL = 0.0
                       BPS_REF_AVR_CMPL = 0.0
                       BPS_REM_AVR_CMPL = 0.0
                       KCHN_ARR(J6,IFRQ) = 0
                       DO 470 J7=1,PIM%CONF%BPS_MSEG_ACCUM
                          ICHN = ICHN + 1
                          IF ( IND_REF == 1 ) THEN
                               BPS_REF_CMPL = BPS%CMPL(ICHN,J5,PIM%OBS(IND_OBS)%STA_IND(1))
                               BPS_REM_CMPL = BPS%CMPL(ICHN,J5,PIM%OBS(IND_OBS)%STA_IND(2))
                             ELSE
                               BPS_REF_CMPL = BPS%CMPL(ICHN,J5,PIM%OBS(IND_OBS)%STA_IND(2))
                               BPS_REM_CMPL = BPS%CMPL(ICHN,J5,PIM%OBS(IND_OBS)%STA_IND(1))
                          END IF
                          IF ( FL_BMASK ) THEN
                               MASK_CHN = PIM%BANDPASS_MASK(ICHN,J5,IND_STA(1),PIMA__MASK_BPAS) * &
     &                                    PIM%BANDPASS_MASK(ICHN,J5,IND_STA(2),PIMA__MASK_BPAS)
                             ELSE
                               MASK_CHN = 1
                          END IF
!
! ----------------------- Update complex residual mseg accumulator, frequency accumulator and ...
!
                          IF ( MASK_CHN*RES(ICHN,IFRQ) .NE. CMPLX ( 0.0, 0.0 ) ) THEN
                               KCHN_ARR(J6,IFRQ) = KCHN_ARR(J6,IFRQ) + 1
                               RES_CMPL = RES_CMPL + RES(ICHN,IFRQ)
                               FREQ_CHN(J6) = FREQ_CHN(J6) + PIM%FREQ_ARR(ICHN,J5,PIM%CONF%FRQ_GRP)
                          END IF
                          FREQ_ARR_R4(ICHN,J5) = PIM%FREQ_ARR(ICHN,J5,PIM%CONF%FRQ_GRP)
!
! ----------------------- ... and the complex bandpass accumulator
!
                          BPS_REM_AVR_CMPL = BPS_REM_AVR_CMPL + BPS_REM_CMPL
                          BPS_REF_AVR_CMPL = BPS_REF_AVR_CMPL + BPS_REF_CMPL
 470                   CONTINUE
!
! -------------------- Get the effective frequency of the multi-channel segment
!
                       IF ( KCHN_ARR(J6,IFRQ) > 0 ) THEN
                            FREQ_CHN(J6) = FREQ_CHN(J6)/KCHN_ARR(J6,IFRQ)
                            BPS_REM_AMP_AVR = ABS(BPS_REM_AVR_CMPL)/KCHN_ARR(J6,IFRQ)
                            BPS_REF_AMP_AVR = ABS(BPS_REF_AVR_CMPL)/KCHN_ARR(J6,IFRQ)
                          ELSE
                            FREQ_CHN(J6) = PIM%FREQ_ARR(ICHN,J5,PIM%CONF%FRQ_GRP)
                            BPS_REM_AMP_AVR = 1.0
                            BPS_REF_AMP_AVR = 1.0
                       END IF
!
                       IP = IP + 1
                       IF ( SGN_REM == -1 ) THEN
                            IF ( KCHN_ARR(J6,IFRQ) > 0 ) THEN
                                 RES_CMPL = RES_CMPL/KCHN_ARR(J6,IFRQ)
                               ELSE
                                 RES_CMPL = CMPLX ( 1.0, 0.0 )
                            END IF
                          ELSE
                            IF ( KCHN_ARR(J6,IFRQ) > 0 ) THEN
                                 RES_CMPL = RES_CMPL/KCHN_ARR(J6,IFRQ)
                               ELSE
                                 RES_CMPL = CMPLX ( 1.0, 0.0 )
                            END IF
                       END IF
!
! -------------------- Get residual amplitude of the bandpass
!
                       IF ( KCHN_ARR(J6,IFRQ) > 0 ) THEN
                            IF ( BPS_REM_AMP_AVR .GE. PIM%CONF%BPS_AMP_MIN ) THEN
!
! ------------------------------ NB: we take square because when we apply amplitude bandpass,
! ------------------------------ we take square root
!
                                 AMPL_RES(J6) = ABS(RES_CMPL)/(BPS_REM_AMP_AVR*BPS_REF_AMP_AVR)
                               ELSE 
                                 AMPL_RES(J6) = 1.0D0
                            END IF
                          ELSE
                            AMPL_RES(J6) = 0.0D0
                       END IF
!
! -------------------- Get the residual phase with respect to the INIT state
! -------------------- With the OPPOSITE sign
!
                       PHAS_RES_INIT(J6) = -PHAS_CMPL_R4 ( RES_CMPL )
                       IF ( J6 == 1 ) THEN
                            IAMB = NINT ( PHAS_RES_INIT(J6)/PI2 )
                         ELSE IF ( J6 > 1 ) THEN
                            IF ( FL_AMBIG_RES ) THEN
                                 IAMB = NINT ( (PHAS_RES_INIT(J6) - PHAS_RES_INIT(J6-1))/PI2 )
                               ELSE
                                 IAMB = NINT ( PHAS_RES_INIT(J6)/PI2 )
                            END IF
                       END IF
                       PHAS_RES_INIT(J6) = PHAS_RES_INIT(J6) - IAMB*PI2
!
                       IF ( PIM%CONF%DEBUG_LEVEL .EQ. 23 .OR. &
     &                      PIM%CONF%DEBUG_LEVEL .EQ. 24      ) THEN
!
                            KP = KP + 1
                            FRQ_DBG(KP) = FREQ_CHN(J6) - PIM%FREQ_ARR(1,PIM%CONF%BEG_FRQ,PIM%CONF%FRQ_GRP)
                            PHAS_ORIG_DBG(KP) = PHAS_RES_INIT(J6)
                            AMPL_ORIG_DBG(KP) = AMPL_RES(J6)
                       END IF
 460                CONTINUE
                    IF ( ( PIM%CONF%PHAS_CAL_CODE == PIMA__PCAL_NO      .OR. &
     &                     PIM%CONF%PHAS_CAL_CODE == PIMA__PCAL_USE_ONE    ) ) THEN
!
                         ACC_RATE(IFRQ,J1) = ACC_RATE(IFRQ,J1) + &
     &                           SGN_REM*(PHAS_RES_INIT(PIM%NCHN) - PHAS_RES_INIT(1))/ &
     &                                   (FREQ_CHN(PIM%NCHN) - FREQ_CHN(1))
                    END IF
!
                    CALL ERR_PASS ( IUER, IER )
                    IF ( PIM%CONF%BPS_INTRP_METHOD == PIMA__LEGENDRE ) THEN
!
! ---------------------- Using expansion into the Legendere polynomial basis
!
                         CALL BPASS_MOD_POLY ( MOD_AMB, PIM%CONF%BPS_AMP_MIN,  &
     &                                  PIM%CONF%BPS_DEG_AMP, PIM%CONF%BPS_DEG_PHS, &
     &                                  KCHN, FREQ_CHN, PHAS_RES_INIT, AMPL_RES, &
     &                                        PHAS_MOD(1,J5), AMPL_MOD(1,J5), &
     &                                  PIM%NCHN, FREQ_ARR_R4(1,J5), &
     &                                            PHAS_OUT(1,J5), AMPL_OUT(1,J5), IER )
                         IF ( IER .NE. 0 ) THEN
                              CALL CLRCH ( STR )
                              CALL INCH  ( IND_OBS, STR )
                              CALL ERR_LOG( 6579, IUER, 'PIMA_BPASS_ACCUM', 'Failure '// &
     &                            'to compute interpolating polynomial for '// &
     &                            'complex bandpass for station '//PIM%C_STA(J1)// &
     &                            ' observation '//STR )
                              RETURN
                         END IF
                       ELSE IF ( PIM%CONF%BPS_INTRP_METHOD == PIMA__SPLINE ) THEN
!
! ---------------------- ... Using expansion into the B-spline basis
!
                         CALL BPASS_MOD_SPLINE ( MOD_AMB,    &
     &                                 PIM%CONF%BPS_AMP_MIN, &
     &                                 PIM%CONF%BPS_DEG_AMP, &
     &                                 PIM%CONF%BPS_DEG_PHS, &
     &                                 KCHN, FREQ_CHN, PHAS_RES_INIT, AMPL_RES, &
     &                                       PHAS_MOD(1,J5), AMPL_MOD(1,J5), &
     &                                 PIM%NCHN, FREQ_ARR_R4(1,J5), &
     &                                           PHAS_OUT(1,J5), AMPL_OUT(1,J5), IER )
                         IF ( IER .NE. 0 ) THEN
                              CALL CLRCH ( STR  )
                              CALL INCH  ( IND_OBS, STR  )
                              CALL ERR_LOG( 6580, IUER, 'PIMA_BPASS_ACCUM', &
     &                            'Failure to compute expansion over '// &
     &                            'B-spline basis for computing complex '// &
     &                            'bandpass for station '//PIM%C_STA(J2)// &
     &                            ' observation '//STR(1:I_LEN(STR)) )
                              RETURN
                         END IF
                       ELSE IF ( PIM%CONF%BPS_INTRP_METHOD == PIMA__LINEAR ) THEN
                         CALL BPASS_MOD_LINEAR ( 1, BPS%CMPL(1,J5,J1), &
     &                                           AC_AVR(1,J5,IND_REF), AC_AVR(1,J5,IND_REM), &
     &                                           KCHN, FREQ_CHN, PHAS_RES_INIT, AMPL_RES,    &
     &                                                 PHAS_MOD(1,J5), AMPL_MOD(1,J5),       &
     &                                           PIM%NCHN, FREQ_ARR_R4(1,J5),                &
     &                                                     PHAS_OUT(1,J5), AMPL_OUT(1,J5),   &
     &                                           IER )
                         IF ( IER .NE. 0 ) THEN
                              CALL CLRCH ( STR  )
                              CALL INCH  ( IND_OBS, STR  )
                              CALL ERR_LOG( 6581, IUER, 'PIMA_BPASS_ACCUM', &
     &                            'Failure to perform linear interpolation '// &
     &                            'for computing complex badnpass for station '// &
     &                             PIM%C_STA(J2)//' observation '// &
     &                             STR(1:I_LEN(STR)) )
                              RETURN
                         END IF
                    END IF
!
                    LP_1ST = LP + 1
                    DO 480 J8=1,KCHN
                       IF ( AMPL_RES(J8) > PIM%CONF%BPS_AMP_MIN .AND. &
     &                      KCHN_ARR(J8,IFRQ) > 0                     ) THEN
!
                            PHAS_RMS = PHAS_RMS + (PHAS_RES_INIT(J8) - PHAS_OUT(J8,J5))**2
                            AMPL_RMS = AMPL_RMS + (AMPL_RES(J8)-1.0)**2
                            NSEG_USED = NSEG_USED + 1
                       END IF
 480                CONTINUE
                    IF ( PIM%CONF%DEBUG_LEVEL .EQ. 23 .OR. PIM%CONF%DEBUG_LEVEL .EQ. 24 ) THEN
                         DO 490 J9=1,PIM%NCHN
                            LP = LP + 1
                            PHAS_OUT_DBG(LP)  = PHAS_OUT(J9,J5)
                            AMPL_OUT_DBG(LP)  = AMPL_OUT(J9,J5)
                            FREQ_OUT_DBG(LP)  = FREQ_ARR_R4(J9,J5) - PIM%FREQ_ARR(1,PIM%CONF%BEG_FRQ,PIM%CONF%FRQ_GRP)
 490                     CONTINUE 
                    END IF
 450             CONTINUE
!
                 IF ( PIM%CONF%DEBUG_LEVEL == 23 ) THEN
                      CALL CLRCH ( STR ) 
                      CALL INCH  ( IND_OBS, STR )
                      CALL DIAGI_SETDEF ( IER, 'DIAGI_UNIT', 'Freq (Hz)' )
                      CALL DIAGI_SETDEF ( IER, 'DIAGI_CTIT', 'bpas_accum '// &
     &                                   'Obs: '//TRIM(STR)// &
     &                                   ' Phas at baseline '// &
     &                                   PIM%C_STA(IND_STA(1))//'/'// &
     &                                   PIM%C_STA(IND_STA(2)) )
                      CALL DIAGI_2 ( KP, FRQ_DBG,      PHAS_ORIG_DBG, &
     &                               LP, FREQ_OUT_DBG, PHAS_OUT_DBG, IER )
                   ELSE IF ( PIM%CONF%DEBUG_LEVEL == 24 ) THEN
                      CALL CLRCH ( STR ) 
                      CALL INCH  ( IND_OBS, STR )
                      CALL DIAGI_SETDEF ( IER, 'DIAGI_UNIT', 'Freq (Hz)' )
                      CALL DIAGI_SETDEF ( IER, 'DIAGI_CTIT', 'bpas_accum '// &
     &                                   'Obs: '//TRIM(STR)// &
     &                                   ' Ampl at baseline '// &
     &                                   PIM%C_STA(IND_STA(1))//'/'// &
     &                                   PIM%C_STA(IND_STA(2)) )
                      CALL DIAGI_2 ( KP, FRQ_DBG,      AMPL_ORIG_DBG, &
     &                               LP, FREQ_OUT_DBG, AMPL_OUT_DBG, IER )
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
                      IFRQ = 0
                      DO 4100 J10=PIM%CONF%BEG_FRQ,PIM%CONF%END_FRQ
                         IFRQ = IFRQ + 1
                         DO 4110 J11=1,KCHN ! cycle over spectral channels
!
! ------------------------- Here ACC_AMPL is a 1./prod(acc_1*acc_2*acc_3..._acc_n)
!
                            IF ( AMPL_RES(J11) > PIM%CONF%BPS_AMP_MIN ) THEN
                                 ACC_AMPL(J11,IFRQ,J1) = ACC_AMPL(J11,IFRQ,J1)/AMPL_RES(J11)
                            END IF
                            IF ( PIM%CONF%PHAS_CAL_CODE == PIMA__PCAL_NO      .OR.  &
     &                           PIM%CONF%PHAS_CAL_CODE == PIMA__PCAL_USE_ONE       ) THEN
                                 ACC_RATE(IFRQ,J1) = ACC_RATE(IFRQ,J1) - &
     &                               SGN_REM*(PHAS_RES_INIT(PIM%NCHN) - PHAS_RES_INIT(1))/ &
     &                                       (FREQ_CHN(PIM%NCHN) - FREQ_CHN(1))
                            END IF
 4110                    CONTINUE
 4100                 CONTINUE
                      STR = 'REJECTED'
                    ELSE 
                      IFRQ = 0
                      L_ACC = L_ACC + 1
                      DO 4120 J12=PIM%CONF%BEG_FRQ,PIM%CONF%END_FRQ
                         IFRQ = IFRQ + 1
                         DO 4130 J13=1,PIM%NCHN ! cycle over spectral channels
!
! ------------------------- Update accumulators
!
                            ACC_AMPL(J13,IFRQ,J1) = ACC_AMPL(J13,IFRQ,J1)*AMPL_OUT(J13,J12)
                            ACC_PHAS(J13,IFRQ,J1) = ACC_PHAS(J13,IFRQ,J1) + &
     &                                              ABS(RES_AVR(1))*PHAS_OUT(J13,J12)
 4130                    CONTINUE
 4120                 CONTINUE
                 END IF
!
                 IF ( PIM%CONF%DEBUG_LEVEL .GE. 2 ) THEN
                      WRITE ( 6, 210 ) PIM%C_STA(J1), J2, AMPL_RMS, PHAS_RMS, STR(1:8), L_ACC
 210                  FORMAT ( 'BPASS Sta: ',A, '  Case: ', I4,' Ampl_rms: ', F6.3, &
     &                         ' Phas_rms: ', F8.4, 2X, A, 2X, ' L_ACC: ', I2 )
                      CALL FLUSH ( 6 )
                 END IF
 420          CONTINUE ! End of cycle over observations
!
              IF ( L_OBS > 0 ) THEN
!
! ---------------- Well, we have processed all observations. Now we compute the 
! ---------------- polynomial coefficients over accumulated phases and amplitudes
!
                   IFRQ = 0
                   DO 4140 J14=PIM%CONF%BEG_FRQ,PIM%CONF%END_FRQ
                      IFRQ = IFRQ + 1
                      DO 4150 J15=1,PIM%NCHN ! cycle over averaged spectral channels
                         IF ( FL_BMASK ) THEN
                               MASK_CHN = PIM%BANDPASS_MASK(J15,J14,IND_STA(1),PIMA__MASK_BPAS) * &
     &                                    PIM%BANDPASS_MASK(J15,J14,IND_STA(2),PIMA__MASK_BPAS)
                            ELSE 
                               MASK_CHN = 1
                         END IF
!
                         IF ( WEI_SUM > WEI__MIN .AND. &
     &                        L_ACC > 0          .AND. &
     &                        ACC_AMPL(J15,IFRQ,J1) > PIM%CONF%BPS_AMP_MIN**L_ACC ) THEN

                              AMPL_OUT(J15,J14) = MASK_CHN*ABS ( BPS%CMPL(J15,J14,J1) )* &
     &                                            ACC_AMPL(J15,IFRQ,J1)**(1.D0/L_ACC)
                            ELSE
                              AMPL_OUT(J15,J14) = MASK_CHN*ABS ( BPS%CMPL(J15,J14,J1) )
                         END IF
!
! ---------------------- PHAS -- phase of the init band pass
! ---------------------- PHAS_OUT -- phase of the residual accumulative bandpass
!
                         PHAS = PHAS_CMPL_R4 ( BPS%CMPL(J15,J14,J1) )
                         IAMB = NINT ( PHAS/PI2 )
                         PHAS = PHAS - IAMB*PI2
                         IF ( WEI_SUM > WEI__MIN ) THEN
                              PHAS_OUT(J15,J14) = PHAS - ACC_PHAS(J15,IFRQ,J1)/WEI_SUM
                            ELSE
                              PHAS_OUT(J15,J14) = PHAS
                         END IF
!
! ---------------------- Resolve ambiguity in the bandpass phase
!
                         IAMB = NINT ( PHAS_OUT(J15,J14)/PI2 )
                         PHAS_OUT(J15,J14) = MASK_CHN*(PHAS_OUT(J15,J14) - IAMB*PI2)
 4150                 CONTINUE
!
                      CALL ERR_PASS ( IUER, IER )
                      IF ( PIM%CONF%BPS_INTRP_METHOD == PIMA__LEGENDRE ) THEN
!
! ------------------------ Modeling bandpass using expansion into Legendre polynomials
!
                           CALL BPASS_MOD_POLY ( 2, PIM%CONF%BPS_AMP_MIN,  &
     &                                PIM%CONF%BPS_DEG_AMP, PIM%CONF%BPS_DEG_PHS, &
     &                                PIM%NCHN, FREQ_ARR_R4(1,J14), &
     &                                          PHAS_OUT(1,J14), AMPL_OUT(1,J14), &
     &                                          PHAS_OUT(1,J14), AMPL_OUT(1,J14), &
     &                                PIM%NCHN, FREQ_ARR_R4(1,J14), &
     &                                          PHAS_OUT(1,J14), AMPL_OUT(1,J14), IER )
                           IF ( IER .NE. 0 ) THEN
                                CALL ERR_LOG( 6582, IUER, 'PIMA_BPASS_ACCUM', &
     &                              'Failure to compute interpolating polynomial '// &
     &                              'for complex bandpass for station '// &
     &                               PIM%C_STA(J1) )
                                RETURN
                           END IF
                         ELSE IF ( PIM%CONF%BPS_INTRP_METHOD == PIMA__SPLINE ) THEN
!
! ------------------------ Modeling bandpass using expansion into B-spline basis
!
                           CALL BPASS_MOD_SPLINE ( 2, &
     &                                 PIM%CONF%BPS_AMP_MIN, &
     &                                 PIM%CONF%BPS_DEG_AMP, &
     &                                 PIM%CONF%BPS_DEG_PHS, &
     &                                 PIM%NCHN, FREQ_ARR_R4(1,J14), &
     &                                           PHAS_OUT(1,J14), AMPL_OUT(1,J14), &
     &                                           PHAS_OUT(1,J14), AMPL_OUT(1,J14), &
     &                                 PIM%NCHN, FREQ_ARR_R4(1,J14), &
     &                                           PHAS_OUT(1,J14), AMPL_OUT(1,J14), IER )
                           IF ( IER .NE. 0 ) THEN
                                CALL CLRCH ( STR  )
                                CALL INCH  ( IND_OBS, STR  )
                                CALL ERR_LOG( 6583, IUER, 'PIMA_BPASS_ACCUM', &
     &                              'Failure to compute expansion into B-spline '// &
     &                              'basis for complex bandpass for '// &
     &                              'station '//PIM%C_STA(J2)// &
     &                              ' observation '//STR(1:I_LEN(STR)) )
                                RETURN
                           END IF
                      END IF
!
! ------------------- Combine amplitudes and phases into the complex bandpass
!
                      DO 4160 J16=1,PIM%NCHN ! cycle over spectral channels
                         IF ( FL_BMASK ) THEN
                              MASK_CHN = PIM%BANDPASS_MASK(J16,J14,IND_STA(1),PIMA__MASK_BPAS) * &
     &                                    PIM%BANDPASS_MASK(J16,J14,IND_STA(2),PIMA__MASK_BPAS)
                            ELSE 
                              MASK_CHN = 1
                         END IF
                         AMPL_OUT(J16,J14) = MASK_CHN*AMPL_OUT(J16,J14)
                         IF ( AMPL_OUT(J16,J14) < PIM%CONF%BPS_AMP_MIN ) THEN
                              AMPL_OUT(J16,J14) = PIM%CONF%BPS_AMP_MIN 
                         END IF
!
                         IF ( PIM%CONF%DEBUG_LEVEL == 23 ) THEN
                              KP = (IFRQ-1)*LCHN + J16
                              FRQ_DBG(KP) = PIM%FREQ_ARR(J16,J14,PIM%CONF%FRQ_GRP)
                              PHAS_ORIG_DBG(KP) = MASK_CHN*PHAS_CMPL_R4 ( BPS%CMPL(J16,J14,J1) )
                              PHAS_OUT_DBG(KP)  = MASK_CHN*PHAS_OUT(J16,J14)
                           ELSE IF ( PIM%CONF%DEBUG_LEVEL == 24 ) THEN
                              KP = (IFRQ-1)*LCHN + J16
                              FRQ_DBG(KP) = PIM%FREQ_ARR(J16,J14,PIM%CONF%FRQ_GRP)
                              AMPL_ORIG_DBG(KP) = MASK_CHN*ABS(BPS%CMPL(J16,J14,J1))
                              AMPL_OUT_DBG(KP)  = MASK_CHN*AMPL_OUT(J16,J14)
                         END IF
!
! ---------------------- Finally, update bandpass
!
                         BPS%CMPL(J16,J14,J1) = &
     &                            CMPLX( AMPL_OUT(J16,J14)*COS(PHAS_OUT(J16,J14)), &
     &                                   AMPL_OUT(J16,J14)*SIN(PHAS_OUT(J16,J14))  )
 4160                 CONTINUE
!
                      ACC_RATE(IFRQ,J1) = ACC_RATE(IFRQ,J1)/L_OBS
!
                      IF ( PIM%CONF%PHAS_CAL_CODE == PIMA__PCAL_NO      .OR. &
     &                     PIM%CONF%PHAS_CAL_CODE == PIMA__PCAL_USE_ONE      ) THEN
                           BPS%PHAS_FRQ_RATE(IFRQ,J1) = BPS%PHAS_FRQ_RATE(IFRQ,J1) + &
     &                                                  ACC_RATE(IFRQ,J1)
                      END IF
 4140              CONTINUE
!
                   IF ( PIM%CONF%DEBUG_LEVEL == 23 ) THEN
                        CALL DIAGI_SETDEF ( IER, 'DIAGI_UNIT', 'Freq (Hz)' )
                        CALL DIAGI_SETDEF ( IER, 'DIAGI_CTIT', 'BPAS_ACCUM '// &
     &                                     ' Phas at baseline '// &
     &                                     PIM%C_STA(IND_STA(1))//'/'// &
     &                                     PIM%C_STA(IND_STA(2)) )
                        CALL DIAGI_2 ( KP, FRQ_DBG, PHAS_ORIG_DBG, &
     &                                 KP, FRQ_DBG, PHAS_OUT_DBG,  -2 )
                      ELSE IF ( PIM%CONF%DEBUG_LEVEL == 24 ) THEN
                        CALL DIAGI_SETDEF ( IER, 'DIAGI_UNIT', 'Freq (Hz)' )
                        CALL DIAGI_SETDEF ( IER, 'DIAGI_CTIT', 'BPAS_ACCUM '// &
     &                                     ' Ampl at baseline '// &
     &                                     PIM%C_STA(IND_STA(1))//'/'// &
     &                                     PIM%C_STA(IND_STA(2)) )
                        CALL DIAGI_2 ( KP, FRQ_DBG, AMPL_ORIG_DBG, &
     &                                 KP, FRQ_DBG, AMPL_OUT_DBG, -2 )
                    END IF
              END IF
         END IF
 410  CONTINUE
!
      DEALLOCATE ( ACC_RATE )
      DEALLOCATE ( ACC_PHAS )
      DEALLOCATE ( ACC_AMPL )
      DEALLOCATE ( RES_TEMP )
      DEALLOCATE ( AC_AVR   )
      DEALLOCATE ( RES      )
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  PIMA_BPASS_ACCUM  !#!
