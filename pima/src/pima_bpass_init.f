      SUBROUTINE PIMA_BPASS_INIT ( PIM, VTD, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  PIMA_BPASS_INIT computes the station-dependent bandpass   *
! *   in the initial mode.                                               *
! *                                                                      *
! * ### 25-JAN-2009  PIMA_BPASS_INIT v5.0 (c) L. Petrov  06-NOV-2020 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'astro_constants.i'
      INCLUDE   'pima.i'
      INCLUDE   'vtd.i'
      TYPE     ( PIMA__TYPE         ) :: PIM
      TYPE     ( VTD__TYPE          ) :: VTD
      INTEGER*4  IUER
      CHARACTER  STR*128, STR1*128, PHAS_CAL_CODE_SAVE*8, POLAR*8, &
     &           POLAR_SAVE*8, POL_CONF*7
      COMPLEX*8, ALLOCATABLE :: RES(:,:)
      REAL*4,    ALLOCATABLE :: AC_AVR(:,:,:)
      LOGICAL*1  FL_BMASK, FL_PROC, FL_TEST
      COMPLEX*8  RES_AVR(2)
      LOGICAL*4  FL_AMBIG_RES, FL_AMB_IF, FL_AMB_OBS, FL_IGNORE_AMP
      COMPLEX*8  DRF, DRF_ALL, PCAL_C8(PIM%NCHN,PIM%NFRQ,2)
      REAL*4     FREQ_IN(PIM__MCHN),  AMPL_RES(PIM__MCHN), PHAS_RES(PIM__MCHN), &
     &           FREQ_OUT(PIM__MCHN), AMPL_MOD(PIM__MCHN), PHAS_MOD(PIM__MCHN), &
     &           PHAS_OUT(PIM__MCHN), AMPL_OUT(PIM__MCHN), &
     &           PHAS_ALL, AMPL_FRQ_NRML(PIM__MFRQ), AMPL_BAND_NRML, &
     &           WEI_IN(PIM__MCHN), BPS_ACC, BPS_NRML, PC_GDEL(2)
      REAL*8     SNR_NEW, PH_RAT(PIM__MFRA), GR_DEL(PIM__MFRA), &
     &           PHS(PIM__MFRA), GR_RAT, GR_DEL_ORIG, FREQ_DIF, &
     &           FREQ_ALL_CHN(PIM__MCHN*PIM__MFRQ), &
     &           BPAS_PHS_CHN(PIM__MCHN*PIM__MFRQ), &
     &           BPAS_AMP_CHN(PIM__MCHN*PIM__MFRQ), &
     &           PHS_AVR_FRQ, PHS_ADD, GR_DEL_FRQ, FRQ_NOMASK
      INTEGER*4  MDBG
      PARAMETER  ( MDBG = 256*1024 )
      REAL*8     FRQ_DBG(MDBG), AMPL_ORIG_DBG(MDBG), AMPL_MOD_DBG(MDBG), &
     &           PHAS_ORIG_DBG(MDBG), PHAS_MOD_DBG(MDBG), AC_BPS_DBG(MDBG), &
     &           PHAS_AVR_DBG(PIM__MFRQ), FREQ_ORIG_DBG(MDBG)
      INTEGER*4  LFRQ, LTIM, IFRQ, IND_STA(2), ITURN, &
     &           J1, J2, J3, J4, J5, J6, J7, J8, J9, J10, J11, J12, J13, &
     &           J14, J15, J16, J17, J18, J19, J20, J21, J22, J23, J24, &
     &           LSTA, LP, IAMB, &
     &           SGN_REM, SGN_REF, KCHN, LCHN, JCHN, ICHN, MOD_AMB, IND_FRA, &
     &           IND_REM, IND_REF, IND_STA_REM, IND_STA_REF, &
     &           IND_OBS, KP, IP, POL_MODE, IND_POL, U_FRG, PC_IND, PCI, IER
      REAL*4     PHAS_MOD_ARR(PIM__MCHN,PIM__MFRQ), PCAL_FREQ_1, &
     &           PCAL_FREQ_2, PHAS_ADD, BPS_PHAS, FREQ_AMB
      INTEGER*1  MASK_CHN, MASK_CHN_LAST
      CHARACTER  BPASS_STYLE*8, FRIB_STYLE*8
      CHARACTER, EXTERNAL :: GET_CDATE*19, PIMA_GET_POL_CONF*7
      LOGICAL*4, EXTERNAL :: BPASS_MOD_POLY, BPASS_MOD_SPLINE
      REAL*4,    EXTERNAL :: PHAS_CMPL_R4
      INTEGER*4, EXTERNAL :: ILEN, I_LEN
!
      LCHN = PIM%NCHN
      LFRQ = PIM%CONF%END_FRQ - PIM%CONF%BEG_FRQ + 1
!
      IF ( PIM%CONF%DEBUG_LEVEL .GE. 3 ) THEN
           WRITE ( 6, * ) '--------------------------- ' 
           WRITE ( 6, * ) ' PIMA_BPASS_INIT            '//GET_CDATE()
           WRITE ( 6, * ) '--------------------------- ' 
           CALL FLUSH ( 6 )
      END IF
      IF ( PIM%NCHN > PIM__MCHN ) THEN
           CALL CLRCH ( STR  )
           CALL INCH  ( PIM%NCHN, STR )
           CALL CLRCH ( STR1 )
           CALL INCH  ( PIM__MCHN, STR1 )
           CALL ERR_LOG ( 6381, IUER, 'PIMA_BPAS_INIT', 'Trap of internal control: '// &
     &         'the number of channels '//STR(1:I_LEN(STR))//' exceeds '// &
     &         'parameter PIM__MCHN= '//STR1 )
           RETURN 
      END IF

!
      FL_TEST = .FALSE.
      FL_AMBIG_RES = .TRUE.
      CALL GETENVAR ( 'PIMAVAR_BPAS_AMBIG_RES', STR )
      CALL TRAN ( 11, STR, STR )
      IF ( STR == 'YES' ) FL_AMBIG_RES = .TRUE.
      IF ( STR == 'NO'  ) FL_AMBIG_RES = .FALSE.
!
      CALL GETENVAR ( 'PIMAVAR_BPAS_IGNORE_AMP', STR )
      CALL TRAN ( 11, STR, STR )
      IF ( STR == 'YES' ) FL_IGNORE_AMP = .TRUE.
      IF ( STR == 'NO'  ) FL_IGNORE_AMP = .FALSE.
!
      IF ( PIM%CONF%BPS_MSEG_ACCUM .LE. 0 ) PIM%CONF%BPS_MSEG_ACCUM = 1
      IF ( PIM%CONF%BPS_INTRP_METHOD .EQ. PIMA__LINEAR .AND. &
     &     PIM%CONF%BPS_MSEG_ACCUM .NE. LCHN/2               ) THEN
           CALL CLRCH ( STR  )
           CALL CLRCH ( STR1 )
           CALL INCH  ( LCHN,                    STR  )
           CALL INCH  ( PIM%CONF%BPS_MSEG_ACCUM, STR1 )
           CALL ERR_LOG ( 6382, IUER, 'PIMA_BPAS_INIT', 'Trap of internal control: '// &
     &         'when '//PIMA__LINEAR//' bandpass interpolation method is used '// &
     &         'parameter PIM%CONF%BPS_MSEG_ACCUM should be equal to one half '// &
     &         'the number of spectral channels in a given IF that is '//TRIM(STR)// &
     &         ' for this experiment, but control files specifies '// &
     &         'PIM%CONF%BPS_MSEG_ACCUM = '//STR1 )
           RETURN 
      END IF
      IF ( PIM%CONF%BPS_INTRP_METHOD .EQ. PIMA__LINEAR .AND. &
     &     .NOT. ( PIM%CONF%BPS_DEG_AMP .EQ. 0 .AND. PIM%CONF%BPS_DEG_PHS .EQ. 1 ) ) THEN
           CALL CLRCH ( STR  )
           CALL CLRCH ( STR1 )
           CALL INCH  ( PIM%CONF%BPS_DEG_AMP, STR  )
           CALL INCH  ( PIM%CONF%BPS_DEG_PHS, STR1 )
           CALL ERR_LOG ( 6383, IUER, 'PIMA_BPAS_INIT', 'Trap of internal control: '// &
     &         'when '//PIMA__LINEAR//' bandpass interpolation method is used '// &
     &         'parameter BPS.DEG_AMP should be equal to zero and  '// &
     &         'parameter BPS.DEG_PHS should be equal to one, but the '// &
     &         'control file specifies '//TRIM(STR)//' and '//TRIM(STR1)// &
     &         ' respectively' )
           RETURN 
      END IF
      KCHN = PIM%NCHN/PIM%CONF%BPS_MSEG_ACCUM
!
      IF ( PIM%CONF%FRIB_FINE_SEARCH == PIMA__FINE_SEARCH_LSQ ) THEN
           IND_FRA = PIMA__LSQ
         ELSE
           IND_FRA = PIMA__DRF
      END IF
!
      IF ( FL_AMBIG_RES ) THEN
           MOD_AMB = 2
         ELSE
           MOD_AMB = 1
      END IF
!
      ALLOCATE ( PIM%BPASS(PIM%NSTA), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR  )
           CALL INCH  ( SIZEOF(PIM%BPASS(J1)), STR )
           CALL ERR_LOG ( 6384, IUER, 'PIMA_BPAS_INIT', 'Error in an '// &
     &         'attempt to allocate '//STR(1:I_LEN(STR))//' bytes of '// &
     &         'dynamic memory for PIM%BPASS' )
           RETURN
      END IF
      CALL NOUT ( PIM%NSTA*SIZEOF(PIM%BPASS(1)), PIM%BPASS )
!
      ALLOCATE ( RES(LCHN,PIM%NFRQ), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR  )
           CALL INCH  ( 8*LCHN*LFRQ, STR )
           CALL ERR_LOG ( 6385, IUER, 'PIMA_BPAS_INIT', 'Error in an '// &
     &         'attempt to allocate '//STR(1:I_LEN(STR))//' bytes of '// &
     &         'dynamic memory for residuals' )
           RETURN
      END IF
      RES = CMPLX ( 0.0, 0.0 )
!
      ALLOCATE ( AC_AVR(LCHN,LFRQ,2), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR  )
           CALL INCH  ( 8*LCHN*LFRQ, STR )
           CALL ERR_LOG ( 6386, IUER, 'PIMA_BPAS_INIT', 'Error in an '// &
     &         'attempt to allocate '//STR(1:I_LEN(STR))//' bytes of '// &
     &         'dynamic memory for array autspectrum AC_AVR' )
           RETURN
      END IF
      AC_AVR = 0.0
!
      PHAS_CAL_CODE_SAVE = PIM%CONF%PHAS_CAL_CODE
!
      IF ( PIM%CONF%BANDPASS_MASK_FILE .NE. PIMA__BPASS_NO  .AND. &
     &     ASSOCIATED ( PIM%BANDPASS_MASK )                       ) THEN
           FL_BMASK = .TRUE.
         ELSE
           FL_BMASK = .FALSE.
      END IF
!
! --- Intialization and memory allocation
!
      DO 410 J1=1,PIM%NSTA
         PIM%BPASS(J1)%STATUS = PIMA__BPASS_ALLOC
         ALLOCATE ( PIM%BPASS(J1)%AMP_RMS_FRQ(PIM%NFRQ), STAT=IER )
         IF ( IER .NE. 0 ) THEN
              CALL CLRCH ( STR  )
              CALL INCH  ( 4*PIM%NFRQ, STR )
              CALL ERR_LOG ( 6387, IUER, 'PIMA_BPAS_INIT', 'Error in an '// &
     &            'attempt to allocate '//STR(1:I_LEN(STR))//' bytes of '// &
     &            'dynamic memory for the field AMP_RMS_FRQ in PIM%BPASS' )
              IF ( ALLOCATED ( AC_AVR) ) DEALLOCATE ( AC_AVR )
              DEALLOCATE ( RES )
              RETURN
         END IF
         PIM%BPASS(J1)%AMP_RMS_FRQ = 0.0
!
         ALLOCATE ( PIM%BPASS(J1)%PHS_RMS_FRQ(PIM%NFRQ), STAT=IER )
         IF ( IER .NE. 0 ) THEN
              CALL CLRCH ( STR  )
              CALL INCH  ( 4*PIM%NFRQ, STR )
              CALL ERR_LOG ( 6388, IUER, 'PIMA_BPAS_INIT', 'Error in an '// &
     &            'attempt to allocate '//STR(1:I_LEN(STR))//' bytes of '// &
     &            'dynamic memory for the field PHS_RMS_FRQ in PIM%BPASS' )
              IF ( ALLOCATED ( AC_AVR) ) DEALLOCATE ( AC_AVR )
              DEALLOCATE ( RES )
              RETURN
         END IF
         PIM%BPASS(J1)%PHS_RMS_FRQ = 0.0
!
         ALLOCATE ( PIM%BPASS(J1)%FREQ(PIM%NCHN,PIM%NFRQ), STAT=IER )
         IF ( IER .NE. 0 ) THEN
              CALL CLRCH ( STR  )
              CALL INCH  ( 4*PIM%NCHN*PIM%NFRQ, STR )
              CALL ERR_LOG ( 6389, IUER, 'PIMA_BPAS_INIT', 'Error in an '// &
     &            'attempt to allocate '//STR(1:I_LEN(STR))//' bytes of '// &
     &            'dynamic memory for the field FREQ in PIM%BPASS' )
              IF ( ALLOCATED ( AC_AVR) ) DEALLOCATE ( AC_AVR )
              DEALLOCATE ( RES )
              RETURN
         END IF
         PIM%BPASS(J1)%FREQ = 0.0
!
         ALLOCATE ( PIM%BPASS(J1)%BPS(PIM%NCHN,PIM%NFRQ), STAT=IER )
         IF ( IER .NE. 0 ) THEN
              CALL CLRCH ( STR  )
              CALL INCH  ( 4*PIM%NCHN*PIM%NFRQ, STR )
              CALL ERR_LOG ( 6390, IUER, 'PIMA_BPAS_INIT', 'Error in an '// &
     &            'attempt to allocate '//STR(1:I_LEN(STR))//' bytes of '// &
     &            'dynamic memory for the field BPS in PIM%BPASS' )
              IF ( ALLOCATED ( AC_AVR) ) DEALLOCATE ( AC_AVR )
              DEALLOCATE ( RES )
              RETURN
         END IF
         PIM%BPASS(J1)%BPS = CMPLX ( 0.0, 0.0 )
!
         ALLOCATE ( PIM%BPASS(J1)%PHS_RATE(PIM%NFRQ), STAT=IER )
         IF ( IER .NE. 0 ) THEN
              CALL CLRCH ( STR  )
              CALL INCH  ( 4*PIM%NFRQ, STR )
              CALL ERR_LOG ( 6391, IUER, 'PIMA_BPAS_INIT', 'Error in an '// &
     &            'attempt to allocate '//STR(1:I_LEN(STR))//' bytes of '// &
     &            'dynamic memory for the field PHS_RATE in PIM%BPASS' )
              IF ( ALLOCATED ( AC_AVR) ) DEALLOCATE ( AC_AVR )
              DEALLOCATE ( RES )
              RETURN
         END IF
         PIM%BPASS(J1)%PHS_RATE = 0.0
         PIM%BPS%CMPL(1:PIM%NCHN,1:PIM%NFRQ,J1) = ( 0.0, 0.0 )
         PIM%BPASS(J1)%IND_STA_REF = PIM%BPS%IND_STA_REF
 410  CONTINUE
!
      PIM%BPS%CMPL(1:PIM%NCHN,1:PIM%NFRQ,PIM%BPS%IND_STA_REF) = ( 1.0, 0.0 )
      LSTA = 0
!
      DO 420 J2=1,PIM%NSTA
         FL_PROC = .FALSE.
         IF ( PIM%C_STA(J2) == PIM%CONF%STA_REF ) GOTO 420
         IF ( PIM%BPS%NUM_OBS_ACCUM(J2,PIMA__PCI_PAR) > 0 ) THEN
              PIM%BPS%PCI(J2) = PIMA__PCI_PAR
            ELSE IF ( PIM%BPS%NUM_OBS_ACCUM(J2,PIMA__PCI_CRS) > 0 ) THEN
              PIM%BPS%PCI(J2) = PIMA__PCI_CRS
            ELSE
              PIM%BPS%PCI(J2) = 0
              IF ( PIM%CONF%DEBUG_LEVEL .GE. 2 ) THEN
                   IF ( PIM%C_STA(J2) < PIM%C_STA(PIM%BPS%IND_STA_REF) ) THEN
                        WRITE ( 6, '(A)' ) 'PIMA_BPASS_INIT: No suitable observations at baseline '// &
     &                          PIM%C_STA(J2)//' / '//PIM%C_STA(PIM%BPS%IND_STA_REF)
                      ELSE IF ( PIM%C_STA(J2) > PIM%C_STA(PIM%BPS%IND_STA_REF) ) THEN
                        WRITE ( 6, '(A)' ) 'PIMA_BPASS_INIT: No suitable observations at baseline '// &
     &                          PIM%C_STA(PIM%BPS%IND_STA_REF)//' / '//PIM%C_STA(J2)
                   END IF
              END IF
              GOTO 420
         END IF
         PCI = PIM%BPS%PCI(J2)
!
         IF ( PCI < 1 ) GOTO 420
         DO 430 J3=1,PIM%BPS%NUM_OBS_ACCUM(J2,PCI) 
            IND_OBS = PIM%BPS%IND_OBS_SEL(J3,J2,PCI)
            IF ( PIM%OBS(IND_OBS)%REF_FRG_INDS(PIM%CONF%FRQ_GRP) == 0 ) THEN
                 GOTO 430
            END IF
            IND_STA(1) = PIM%OBS(IND_OBS)%STA_IND(1)
            IND_STA(2) = PIM%OBS(IND_OBS)%STA_IND(2)
            IF ( IND_STA(1) == PIM%BPS%IND_STA_REF ) THEN
                 IND_STA_REF = IND_STA(1)
                 IND_STA_REM = IND_STA(2)
                 IND_REF =  1
                 IND_REM =  2
                 SGN_REM =  1
                 SGN_REF = -1
              ELSE
                 IND_STA_REF = IND_STA(2)
                 IND_STA_REM = IND_STA(1)
                 IND_REF =  2
                 IND_REM =  1
                 SGN_REM = -1
                 SGN_REF =  1
            END IF
!
            CALL ERR_PASS ( IUER, IER )
            POL_CONF = PIMA_GET_POL_CONF ( PIM, IND_OBS, IER ) 
            IF ( IER .NE. 0 ) THEN
                 CALL ERR_LOG ( 7588, IUER, 'PIMA_BPASS_INIT', 'Trap of internal control ' )
                 CALL EXIT ( 1 )
                 RETURN
            END IF
!
            LTIM = PIM%OBS(IND_OBS)%NUM_EPC(PIM%OBS(IND_OBS)%REF_FRG_INDS(PIM%CONF%FRQ_GRP))
!
            IF ( PIM%CONF%DEBUG_LEVEL .GE. 2 ) THEN
                 WRITE ( 6, 210 ) PIM%C_STA(J2), IND_OBS, PIM%BPS%SNR(J3,J2,PCI)
 210             FORMAT ( 'BPASS_INIT  Sta: ',A, ' Obs: ',I6, &
     &                    '  SNR: ', F8.1 )
                 CALL FLUSH ( 6 )
            END IF
!
            FRIB_STYLE   = PIMA__2FFT
            GR_DEL(IND_FRA) = PIM%BPS%GR_DEL(J3,J2,PCI)
            PH_RAT(IND_FRA) = PIM%BPS%PH_RAT(J3,J2,PCI)
            PHS(IND_FRA) = PIM%BPS%PHS(J3,J2,PCI)
!
! --------- Compute residuals. NB: we do not apply amplitude 
! --------- of the bandpass
!
            BPASS_STYLE = PIMA__BPASS_NO
            CALL ERR_PASS ( IUER, IER )
            IF ( POL_CONF == PIMA__PC_CC ) THEN
                 POL_MODE = PIMA__PAR
                 IF ( PIM%CONF%POLAR == PIMA__POLAR_I ) THEN
                      POL_MODE = PIMA__RRCC
                    ELSE IF ( PIM%CONF%POLAR == PIMA__POLAR_RR ) THEN
                      POL_MODE = PIMA__RRCC
                    ELSE IF ( PIM%CONF%POLAR == PIMA__POLAR_LL ) THEN
                      POL_MODE = PIMA__LLCC
                    ELSE IF ( PIM%CONF%POLAR == PIMA__POLAR_RL ) THEN
                      POL_MODE = PIMA__RLCC
                    ELSE IF ( PIM%CONF%POLAR == PIMA__POLAR_LR ) THEN
                      POL_MODE = PIMA__LRCC
                    ELSE
                      CALL ERR_LOG ( 6398, IUER, 'PIMA_BPAS_INIT', 'POLAR: '// &
     &                     PIM%CONF%POLAR//' is not supported for bandpass '// &
     &                    'computation: only I, RR, LL, LR, or RL' )
                      DEALLOCATE ( AC_AVR )
                      DEALLOCATE ( RES )
                      RETURN
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
     &           LCHN, LFRQ, LTIM, &
     &           GR_DEL, PH_RAT, PIM%BPS%GR_RAT(J3,J2,PCI), PHS, &
     &           PIM%BPS%SNR(J3,J2,PCI), PIM%BPS%TIME_FRT(J3,J2,PCI), &
     &           FRIB_STYLE, BPASS_STYLE, POL_MODE, &
     &           RES, RES_AVR, SNR_NEW, IER )
            IF ( IER .NE. 0 ) THEN
                 CALL CLRCH ( STR )
                 CALL INCH  ( IND_OBS, STR )
                 CALL ERR_LOG ( 6397, IUER, 'PIMA_BPAS_INIT', 'Error '// &
     &               'in an attempt to compute residuals for '// &
     &               'observation '//STR  )
                 DEALLOCATE ( AC_AVR )
                 DEALLOCATE ( RES )
                 RETURN
            END IF
            IF ( RES_AVR(1) == 0.0 ) THEN
                 CALL CLRCH ( STR )
                 CALL INCH  ( IND_OBS, STR )
                 CALL ERR_LOG ( 6398, IUER, 'PIMA_BPAS_INIT', 'Trap '// &
     &               'of internal control: zero residuals for '// &
     &               'observation '//STR(1:I_LEN(STR))//'. One of '// &
     &               'the possible reasons: low SNR' )
                 DEALLOCATE ( AC_AVR )
                 DEALLOCATE ( RES )
                 RETURN
            END IF
!
! --------- Propagate results of fringe fiting to parameters of all types
! --------- NB: DRF type is used in the "substitute" residual mode
!
            PIM%BPS%GR_DEL(J3,J2,PCI) = GR_DEL(IND_FRA)
            PIM%BPS%PH_RAT(J3,J2,PCI) = PH_RAT(IND_FRA)
            PIM%BPS%PHS(J3,J2,PCI)    = PHS(IND_FRA)
!
            IFRQ = 0
            DO 450 J5=PIM%CONF%BEG_FRQ,PIM%CONF%END_FRQ
               IFRQ = IFRQ + 1
               DO 460 J6=1,PIM%NCHN ! cycle over spectral channels
                  IF ( SGN_REM == -1 ) THEN
!
! -------------------- This means that the first station is the
! -------------------- bandpass-reference station
!
                       PIM%BPS%CMPL(J6,J5,J2) = CONJG(RES(J6,IFRQ))/RES_AVR(1)
                     ELSE
!
! -------------------- This means that the second station is the
! -------------------- bandpass-reference station. NB: we invert the sign
! -------------------- of phase residuals here
!
                       PIM%BPS%CMPL(J6,J5,J2) = RES(J6,IFRQ)/RES_AVR(1)
                  END IF
 460           CONTINUE
 450        CONTINUE
!
            FL_AMB_OBS = .FALSE.
            KP = 0
            LP = 0
            IFRQ = 0
            DO 470 J7=PIM%CONF%BEG_FRQ,PIM%CONF%END_FRQ
               IFRQ = IFRQ + 1
               PHAS_AVR_DBG(IFRQ) = 0.0D0
               ICHN = 0
               DRF_ALL = 0.0
               DO 480 J8=1,KCHN ! cycle over spectral channels
                  DRF = 0.0
                  FREQ_IN(J8) = 0.0
                  FRQ_NOMASK  = 0.0
                  WEI_IN(J8)  = 0.0
                  DO 490 J9=1,PIM%CONF%BPS_MSEG_ACCUM
                     ICHN = ICHN + 1
                     FREQ_OUT(ICHN) = PIM%FREQ_ARR(ICHN,J7,PIM%CONF%FRQ_GRP)
                     IF ( FL_BMASK ) THEN
                          MASK_CHN = PIM%BANDPASS_MASK(ICHN,J7,IND_STA(1),PIMA__MASK_BPAS) * &
     &                               PIM%BANDPASS_MASK(ICHN,J7,IND_STA(2),PIMA__MASK_BPAS)
                        ELSE
                          MASK_CHN = 1
                     END IF
                     DRF = DRF + MASK_CHN*PIM%BPS%CMPL(ICHN,J7,J2)
                     IF ( PIM%BPS%CMPL(ICHN,J7,J2) .NE. CMPLX ( 0.0, 0.0 ) ) THEN
                          FREQ_IN(J8) = FREQ_IN(J8) + PIM%FREQ_ARR(ICHN,J7,PIM%CONF%FRQ_GRP)*MASK_CHN
                          FRQ_NOMASK  = FRQ_NOMASK  + PIM%FREQ_ARR(ICHN,J7,PIM%CONF%FRQ_GRP)
                          WEI_IN(J8)  = WEI_IN(J8)  + MASK_CHN
                     END IF
 490              CONTINUE
                  DRF_ALL = DRF_ALL + DRF
                  IF ( WEI_IN(J8) > 0.0 ) THEN
                       FREQ_IN(J8)  = FREQ_IN(J8)/WEI_IN(J8)  
                       AMPL_RES(J8) = ABS ( DRF )/WEI_IN(J8)
                       PHAS_RES(J8) = PHAS_CMPL_R4 ( DRF )
                     ELSE
                       FREQ_IN(J8)  = PIM%FREQ_ARR(ICHN,J7,PIM%CONF%FRQ_GRP)
                       WEI_IN(J8)   = 0.0
                       AMPL_RES(J8) = 0.0
                       PHAS_RES(J8) = 0.0
                  END IF
                  LP = LP + 1
                  FREQ_ORIG_DBG(LP) = FREQ_IN(J8)
                  AMPL_ORIG_DBG(LP) = AMPL_RES(J8)
                  PHAS_ORIG_DBG(LP) = PHAS_RES(J8) 
 480           CONTINUE
!
! ------------ Resolve phase ambiguity with respect to the averaged
! ------------ phase of this IF
!
               PHAS_ALL = PHAS_CMPL_R4 ( DRF_ALL )
               DO 4100 J10=1,KCHN
                  ITURN = IDNINT( (PHAS_RES(J10) - PHAS_ALL)/PI2 )
                  PHAS_RES(J10) = PHAS_RES(J10) - ITURN*PI2
 4100          CONTINUE
!
! ------------ Compute the interpolating polynomial for phases and
! ------------ amplitudes and replace the actual amplitudes and phases
! ------------ with those computed using these polynomials
!
               CALL ERR_PASS ( IUER, IER )
               IF ( PIM%CONF%BPS_INTRP_METHOD == PIMA__LEGENDRE ) THEN
!
! ----------------- ... Using expansion into the Legendere polynomial basis
!
                    CALL ERR_PASS ( IUER, IER )
                    FL_AMB_IF = BPASS_MOD_POLY ( MOD_AMB, &
     &                             PIM%CONF%BPS_AMP_MIN,  &
     &                             PIM%CONF%BPS_DEG_AMP,  &
     &                             PIM%CONF%BPS_DEG_PHS,  &
     &                             KCHN,     FREQ_IN,  PHAS_RES, AMPL_RES, &
     &                                                 PHAS_MOD, AMPL_MOD, &
     &                             PIM%NCHN, FREQ_OUT, PHAS_OUT, AMPL_OUT, &
     &                             IER )
                    IF ( IER .NE. 0 ) THEN
                         CALL CLRCH ( STR  )
                         CALL CLRCH ( STR1 )
                         CALL INCH  ( IND_OBS, STR  )
                         CALL INCH  ( J7,      STR1 )
                         CALL ERR_LOG ( 6399, IUER, 'PIMA_BPAS_INIT', &
     &                       'Failure to compute interpolating '// &
     &                       'polynomial for badnpass phases for '// &
     &                       'station '//PIM%C_STA(J2)// &
     &                       ' observation '//STR(1:I_LEN(STR))// &
     &                       ' IF# '//STR1 )
                         RETURN
                    END IF
                  ELSE IF ( PIM%CONF%BPS_INTRP_METHOD == PIMA__SPLINE ) THEN
!
! ----------------- ... Using expansion into the B-spline basis
!
                    CALL ERR_PASS ( IUER, IER )
                    FL_AMB_IF = BPASS_MOD_SPLINE ( MOD_AMB, &
     &                             PIM%CONF%BPS_AMP_MIN,  &
     &                             PIM%CONF%BPS_DEG_AMP, &
     &                             PIM%CONF%BPS_DEG_PHS, &
     &                             KCHN,     FREQ_IN,  PHAS_RES, AMPL_RES, &
     &                                                 PHAS_MOD, AMPL_MOD, &
     &                             PIM%NCHN, FREQ_OUT, PHAS_OUT, AMPL_OUT, &
     &                             IER )
                    IF ( IER .NE. 0 ) THEN
                         CALL CLRCH ( STR  )
                         CALL CLRCH ( STR1 )
                         CALL INCH  ( IND_OBS, STR  )
                         CALL INCH  ( J7,      STR1 )
                         CALL ERR_LOG ( 6400, IUER, 'PIMA_BPAS_INIT', &
     &                       'Failure to compute interpolating '// &
     &                       'polynomial for badnpass phases for '// &
     &                       'station '//PIM%C_STA(J2)// &
     &                       ' observation '//STR(1:I_LEN(STR))// &
     &                       ' IF# '//STR1 )
                         RETURN
                    END IF
                  ELSE IF ( PIM%CONF%BPS_INTRP_METHOD == PIMA__LINEAR ) THEN
                    CALL BPASS_MOD_LINEAR ( 2, PIM%BPS%CMPL(1,J7,J2), &
     &                                      PIM%OBS(IND_OBS)%AC_AVR_TIM(1,IFRQ,IND_REF,1), &
     &                                      PIM%OBS(IND_OBS)%AC_AVR_TIM(1,IFRQ,IND_REM,1), &
     &                                      KCHN,     FREQ_IN,  PHAS_RES, AMPL_RES, &
     &                                                          PHAS_MOD, AMPL_MOD, &
     &                                      PIM%NCHN, FREQ_OUT, PHAS_OUT, AMPL_OUT, &
     &                                      IER )
                    IF ( IER .NE. 0 ) THEN
                         CALL CLRCH ( STR  )
                         CALL CLRCH ( STR1 )
                         CALL INCH  ( IND_OBS, STR  )
                         CALL INCH  ( J7,      STR1 )
                         CALL ERR_LOG ( 6401, IUER, 'PIMA_BPASS_INIT', &
     &                       'Failure to perform linear interpolation '// &
     &                       ' for badnpass phases for station '//PIM%C_STA(J2)// &
     &                       ' observation '//STR(1:I_LEN(STR))// &
     &                       ' IF# '//STR1 )
                         RETURN
                    END IF
               END IF
               IF ( FL_AMB_IF ) FL_AMB_OBS = .TRUE.
               DO 4110 J11=1,PIM%NCHN ! cycle over spectral channels
                  IF ( FL_BMASK ) THEN
                       MASK_CHN = PIM%BANDPASS_MASK(J11,J7,IND_STA(1),PIMA__MASK_BPAS) * &
     &                            PIM%BANDPASS_MASK(J11,J7,IND_STA(2),PIMA__MASK_BPAS)
                     ELSE 
                       MASK_CHN = 1
                  END IF
!
                  AMPL_OUT(J11) = MASK_CHN*MAX(AMPL_OUT(J11),0.0)
                  IF ( PIM%CONF%BPS_MODE    == PIMA__BPASS_INSP .OR. &
     &                 PIM%CONF%DEBUG_LEVEL == 21               .OR. &
     &                 PIM%CONF%DEBUG_LEVEL == 22                    ) THEN
                       KP = KP + 1
                       FRQ_DBG(KP) = PIM%FREQ_ARR(J11,J7,PIM%CONF%FRQ_GRP)
                       AMPL_MOD_DBG(KP)   = AMPL_OUT(J11)
                       AC_BPS_DBG(KP)     = PIM%OBS(IND_OBS)%AC_AVR_TIM(J11,IFRQ,IND_REM,1)/ &
     &                                      PIM%OBS(IND_OBS)%AC_MEAN(IFRQ,IND_REM,1)
                       PHAS_MOD_DBG(KP)   = PHAS_OUT(J11)
                       PHAS_AVR_DBG(IFRQ) = PHAS_AVR_DBG(IFRQ) + PHAS_OUT(J11)/PIM%NCHN 
                  END IF
!
                  IF ( .NOT. FL_IGNORE_AMP ) THEN
                       PIM%BPS%CMPL(J11,J7,J2) = &
     &                         CMPLX( AMPL_OUT(J11)*COS(PHAS_OUT(J11)), &
     &                                AMPL_OUT(J11)*SIN(PHAS_OUT(J11))  )
                     ELSE
                       PIM%BPS%CMPL(J11,J7,J2) = &
     &                         CMPLX( COS(PHAS_OUT(J11)), &
     &                                SIN(PHAS_OUT(J11))  )
                  END IF
                  PHAS_MOD_ARR(J11,J7) = PHAS_OUT(J11)
 4110          CONTINUE
 470        CONTINUE
!
! --------- Normalization of the amplitude.
! --------- NB: At this point the amplitude bandpass of the reference
! --------- station is not known. We use the J2-th station as the 
! --------- reference station
!
            CALL BPASS_AMP_NRML ( PIM, J2, 0, &
     &                            PIM%BPS%CMPL(1,1,J2), PIM%BPS%CMPL(1,1,J2), &
     &                            AMPL_FRQ_NRML,          &
     &                            AMPL_BAND_NRML,         &
     &                            PIM%BPS%AMPL_FRQ_AVR(1,J2), &
     &                            PIM%BPS%AMPL_FRQ_RMS(1,J2), &
     &                            PIM%BPS%AMPL_TOT_RMS(J2),   &
     &                            PIM%BPS%AMPL_INTEGRAL(J2)   )
!
! --------- Compute the group delay and phase over the bandpass.
! --------- We want to present bandpass without group delay and 
! --------- zero average phase.
! --------- For that, we eavluate the group delay and phase delay over 
! --------- the bandpass by fitting trial multi-band delay slope
!
            CALL BPASS_GR_DEL ( J2, PIM, PIM%BPS, &
     &                          PIM%BPASS(J2)%BPS_MB_GRDEL, &
     &                          BPS_PHAS )
            IF ( PIM%CONF%DEBUG_LEVEL .GE. 5 ) THEN
                 WRITE  ( 6, 120 ) PIM%C_STA(J2), PIM%BPASS(J2)%BPS_MB_GRDEL
 120             FORMAT ( 'PIMA_BPASS_INIT: Station ',A, &
     &                    ' Bpass_gr_del: ', 1PD13.6 )
                CALL FLUSH ( 6 )
            END IF
            IFRQ = 0
            LP = 0
            JCHN = 0
            DO 4140 J14=PIM%CONF%BEG_FRQ,PIM%CONF%END_FRQ
               IFRQ = IFRQ + 1
               DO 4150 J15=1,PIM%NCHN ! cycle over spectral channels
                  IF ( ABS ( PIM%BPS%CMPL(J15,J14,J2) ) < PIM%CONF%BPS_AMP_MIN .AND. &
     &                 ABS ( PIM%BPS%CMPL(J15,J14,J2) ) > PIMA__AMP_MIN  ) THEN
                       PIM%BPS%CMPL(J15,J14,J2) = PIM%CONF%BPS_AMP_MIN * &
     &                                            PIM%BPS%CMPL(J15,J14,J2)/ABS(PIM%BPS%CMPL(J15,J14,J2))
                     ELSE IF ( ABS ( PIM%BPS%CMPL(J15,J14,J2) ) < PIMA__AMP_MIN  ) THEN
                       PIM%BPS%CMPL(J15,J14,J2) = PIM%CONF%BPS_AMP_MIN
                  END IF 
                  JCHN = JCHN + 1
                  FREQ_ALL_CHN(JCHN) = PIM%FREQ_ARR(J15,J14,PIM%CONF%FRQ_GRP)
                  BPAS_PHS_CHN(JCHN) = PHAS_CMPL_R4 ( PIM%BPS%CMPL(J15,J14,J2) )
                  BPAS_AMP_CHN(JCHN) = ABS ( PIM%BPS%CMPL(J15,J14,J2) )
!
! --------------- Rotate the modelled phase to compensate group delay of phase cal
! --------------- over the band
!
                  FREQ_DIF = PIM%FREQ_ARR(J15,J14,PIM%CONF%FRQ_GRP) - &
     &                       PIM%FREQ_ARR(1,PIM%CONF%BEG_FRQ,PIM%CONF%FRQ_GRP)
                  PHAS_ADD = -PI2*FREQ_DIF*PIM%BPASS(J2)%BPS_MB_GRDEL &
     &                       - BPS_PHAS
                  PIM%BPS%CMPL(J15,J14,J2) = PIM%BPS%CMPL(J15,J14,J2)* &
     &                                   CMPLX(COS(PHAS_ADD),SIN(PHAS_ADD))
                  PHAS_MOD_ARR(J15,J14) = PHAS_MOD_ARR(J15,J14) &
     &                                    - PHAS_ADD
                  IF ( FL_BMASK ) THEN
                       MASK_CHN = PIM%BANDPASS_MASK(J15,J14,IND_STA(1),PIMA__MASK_BPAS) * &
     &                            PIM%BANDPASS_MASK(J15,J14,IND_STA(2),PIMA__MASK_BPAS)
                     ELSE
                       MASK_CHN = 1
                  END IF
                  IF ( PIM%CONF%BPS_MODE    == PIMA__BPASS_INSP .OR. &
     &                 PIM%CONF%DEBUG_LEVEL == 21               .OR. &
     &                 PIM%CONF%DEBUG_LEVEL == 22                    ) THEN
                       IP = (IFRQ-1)*LCHN + J15
                       IF ( PIM%CONF%BPS_NORML == PIMA__NORML_BAND ) THEN
                            AMPL_MOD_DBG(IP) = AMPL_MOD_DBG(IP)*AMPL_BAND_NRML
                          ELSE IF ( PIM%CONF%BPS_NORML == PIMA__NORML_IF ) THEN
                            AMPL_MOD_DBG(IP) = AMPL_MOD_DBG(IP)*AMPL_FRQ_NRML(IFRQ)
                       END IF                                 
                       IP = (IFRQ-1)*LCHN + J15
                       IF ( MASK_CHN == 0 ) THEN
                            PHAS_MOD_DBG(IP) = 0
                          ELSE                                 
                            ITURN = IDNINT ( PHAS_AVR_DBG(IFRQ)/PI2 )
                            PHAS_MOD_DBG(IP) = PHAS_MOD_DBG(IP) - PI2*ITURN
                        END IF
                  END IF
 4150          CONTINUE
               DO 4160 J16=1,KCHN
                  LP = LP + 1
                  IF ( PIM%CONF%BPS_NORML == PIMA__NORML_BAND ) THEN
                       AMPL_ORIG_DBG(LP) = AMPL_ORIG_DBG(LP)*AMPL_BAND_NRML
                     ELSE IF ( PIM%CONF%BPS_NORML == PIMA__NORML_IF ) THEN
                       AMPL_ORIG_DBG(LP) = AMPL_ORIG_DBG(LP)*AMPL_FRQ_NRML(IFRQ)
                  END IF 
 4160          CONTINUE
 4140       CONTINUE
!
            IF ( PIM%CONF%BPS_MODE    == PIMA__BPASS_INSP .OR. &
     &           PIM%CONF%DEBUG_LEVEL == 22                    ) THEN
                 CALL DIAGI_SETDEF ( IER, 'DIAGI_UNIT', 'Freq (Hz)' )
                 CALL DIAGI_SETDEF ( IER, 'DIAGI_CTIT', 'BPAS_INIT '// &
     &                               'Ampl at baseline '// &
     &                                PIM%C_STA(IND_STA(1))//'/'// &
     &                                PIM%C_STA(IND_STA(2)) )
                 CALL DIAGI_3 ( LP, FREQ_ORIG_DBG, AMPL_ORIG_DBG, &
     &                          KP, FRQ_DBG, AMPL_MOD_DBG,   &
     &                          KP, FRQ_DBG, AC_BPS_DBG, IER ) 
            END IF
!
            IF ( PIM%CONF%BPS_MODE    == PIMA__BPASS_INSP .OR. &
     &           PIM%CONF%DEBUG_LEVEL == 21                    ) THEN
                 CALL DIAGI_SETDEF ( IER, 'DIAGI_UNIT', 'Freq (Hz)' )
                 CALL DIAGI_SETDEF ( IER, 'DIAGI_CTIT', 'BPAS_INIT '// &
     &                               'Phas at baseline '// &
     &                                PIM%C_STA(IND_STA(1))//'/'// &
     &                                PIM%C_STA(IND_STA(2)) )
                 CALL DIAGI_2 ( LP, FREQ_ORIG_DBG, PHAS_ORIG_DBG, &
     &                          KP, FRQ_DBG, PHAS_MOD_DBG, IER )
            END IF
!
! --------- Collect bandpass statistics
!
            CALL BPASS_STA_STAT  ( J2, PIM, PIM%BPS )
!
            CALL ERR_PASS ( IUER, IER )
            IND_POL = 1
!
! --------- Get single and multi-IF group delay of the phase calibration signal
! --------- for both stations of the IND_OBS-th observation
!
            U_FRG = PIM%CONF%FRQ_GRP
            CALL ERR_PASS ( IUER, IER )
            CALL PIMA_USE_PCAL ( PIM, PIMA__SET_PCAL, IND_OBS, &
     &                           LFRQ, PIM%CONF%BEG_FRQ, PIM%CONF%END_FRQ, &
     &                           IND_POL, PCAL_C8, PC_GDEL, IER )
            IF ( IER .NE. 0 ) THEN
                 CALL ERR_LOG ( 6402, IUER, 'PIMA_BPASS_INIT', &
     &               'Error in computing group delays in phase calibration '// &
     &               'for observation '//TRIM(STR)//' at stations '// &
     &                PIM%C_STA(IND_STA(1))//'/'//PIM%C_STA(IND_STA(2)) )
                 RETURN 
            END IF
!
! --------- Store group delay in phase calibration signal 
! --------- for both stations of the IND_OBS-th observation in PIM%BPS object
!
            PIM%BPS%PCAL_MB_GRDEL(IND_STA(1)) = PIM%STA(IND_STA(1))%PCAL(U_FRG)%PCAL_MB_GRDEL
            PIM%BPS%PCAL_MB_GRDEL(IND_STA(2)) = PIM%STA(IND_STA(2))%PCAL(U_FRG)%PCAL_MB_GRDEL
            DO 4170 J17=PIM%CONF%BEG_FRQ, PIM%CONF%END_FRQ
               PIM%BPS%PCAL_SB_GRDEL(J17,IND_STA(1)) = PIM%STA(IND_STA(1))%PCAL(U_FRG)%PCAL_SB_GRDEL(J17)
               PIM%BPS%PCAL_SB_GRDEL(J17,IND_STA(2)) = PIM%STA(IND_STA(2))%PCAL(U_FRG)%PCAL_SB_GRDEL(J17)
 4170       CONTINUE 
!
            IF ( POL_MODE == PIMA__RRCC ) THEN
                 PIM%BPS%INIT_GR_DEL(J2,1) = PIM%BPS%GR_DEL(J3,J2,PCI)
                 PIM%BPS%INIT_GR_RAT(J2,1) = PIM%BPS%GR_RAT(J3,J2,PCI)
                 PIM%BPS%INIT_PH_RAT(J2,1) = PIM%BPS%PH_RAT(J3,J2,PCI)
                 PIM%BPS%INIT_PHS(J2,1) = PIM%BPS%PHS(J3,J2,PCI)   
                 PIM%BPS%INIT_PLR(J2) = PIMA__RRCC
            END IF
!
            FL_PROC = .TRUE.
            GOTO 820
 430     CONTINUE
 820     CONTINUE 
         PIM%CONF%PHAS_CAL_CODE = PHAS_CAL_CODE_SAVE
!
         IF ( FL_PROC .AND. J2 .NE. PIM%BPS%IND_STA_REF ) THEN
              PIM%BPS%CMPL(1:PIM%NCHN,1:PIM%NFRQ,PIM%BPS%IND_STA_REF) = ABS(PIM%BPS%CMPL(1:PIM%NCHN,1:PIM%NFRQ,J2))* &
     &                                     ABS(PIM%BPS%CMPL(1:PIM%NCHN,1:PIM%NFRQ,PIM%BPS%IND_STA_REF))
              LSTA = LSTA +  1
         END IF
!
         CALL ERR_PASS ( IUER, IER )
         CALL PIMA_BPASS_DETREND ( PIM, PIM%BPS%CMPL(1,1,J2), IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 6403, IUER, 'PIMA_BPAS_INIT', 'Error in an attemt '// &
     &            'to detrend bandlass for '//PIM%C_STA(J2)//' station' )
              RETURN 
         END IF
!
         IF ( FL_TEST ) THEN
              JCHN = 0
              DO 4180 J18=PIM%CONF%BEG_FRQ,PIM%CONF%END_FRQ
                 DO 4190 J19=1,PIM%NCHN ! cycle over spectral channels
                    JCHN = JCHN + 1
                    FREQ_ALL_CHN(JCHN) = PIM%FREQ_ARR(J19,J18,PIM%CONF%FRQ_GRP)
                    BPAS_PHS_CHN(JCHN) = PHAS_CMPL_R4 ( PIM%BPS%CMPL(J19,J18,J2) )
                    BPAS_AMP_CHN(JCHN) = ABS ( PIM%BPS%CMPL(J19,J18,J2) )
 4190           CONTINUE 
 4180        CONTINUE 
             CALL CLRCH ( STR )
             CALL INCH  ( IND_OBS, STR )
             CALL DIAGI_SETDEF ( IER, 'DIAGI_CTIT', 'Bandpass phase for station '//PIM%C_STA(J2)// &
     &                           ' observation '//STR )
             CALL DIAGI_1 ( JCHN, FREQ_ALL_CHN, BPAS_PHS_CHN, IER )
         END IF
 420  CONTINUE
!
! ---- Compute the bandpass amplitude of the reference station
!
       DO 4200 J20=PIM%CONF%BEG_FRQ,PIM%CONF%END_FRQ
          DO 4210 J21=1,PIM%NCHN ! cycle over spectral channels
             IF ( LSTA > 0 .AND. &
     &            ABS(PIM%BPS%CMPL(J21,J20,PIM%BPS%IND_STA_REF)) > PIM%CONF%BPS_AMP_MIN**LSTA ) THEN
!
                  PIM%BPS%CMPL(J21,J20,PIM%BPS%IND_STA_REF) = ABS(PIM%BPS%CMPL(J21,J20,PIM%BPS%IND_STA_REF))**(1.0/(2.0*LSTA))
                ELSE
                  PIM%BPS%CMPL(J21,J20,PIM%BPS%IND_STA_REF) = CMPLX(PIM%CONF%BPS_AMP_MIN, 0.0)
             END IF
 4210     CONTINUE
 4200 CONTINUE
!
! --- Normalization of the bandpass for the reference station
!
      CALL BPASS_AMP_NRML ( PIM, PIM%BPS%IND_STA_REF, 0, &
     &                      PIM%BPS%CMPL(1,1,PIM%BPS%IND_STA_REF), PIM%BPS%CMPL(1,1,PIM%BPS%IND_STA_REF), &
     &                      AMPL_FRQ_NRML,                                 &
     &                      AMPL_BAND_NRML,                                &
     &                      PIM%BPS%AMPL_FRQ_AVR(1,PIM%BPS%IND_STA_REF),           &
     &                      PIM%BPS%AMPL_FRQ_RMS(1,PIM%BPS%IND_STA_REF),           &
     &                      PIM%BPS%AMPL_TOT_RMS(PIM%BPS%IND_STA_REF),             &
     &                      PIM%BPS%AMPL_INTEGRAL(PIM%BPS%IND_STA_REF)   )
!
! --- Now we renormalize amplitudes of the bandpass for all remote stations.
! --- Before the amplitude bandpass of the reference station 
! --- was assumed implicitly to be one. We have just changed
! --- it and we need to recompute the amplitude bandpass of
! --- remotes stations
!
      DO 4220 J22=1,PIM%NSTA
         IF ( J22 .NE. PIM%BPS%IND_STA_REF ) THEN
              DO 4230 J23=PIM%CONF%BEG_FRQ,PIM%CONF%END_FRQ
                 DO 4240 J24=1,PIM%NCHN
                     IF ( REAL(PIM%BPS%CMPL(J24,J23,PIM%BPS%IND_STA_REF)) > PIMA__WEI_MIN ) THEN
!
                          PIM%BPS%CMPL(J24,J23,J22) = PIM%BPS%CMPL(J24,J23,J22)/ &
     &                             ABS(PIM%BPS%CMPL(J24,J23,PIM%BPS%IND_STA_REF))
                     END IF
 4240             CONTINUE 
 4230         CONTINUE 
!
! ----------- And we need also to renormalize it
!
              CALL BPASS_AMP_NRML ( PIM, J22, PIM%BPS%IND_STA_REF, &
     &             PIM%BPS%CMPL(1,1,J22), PIM%BPS%CMPL(1,1,PIM%BPS%IND_STA_REF), &
     &             AMPL_FRQ_NRML,          &
     &             AMPL_BAND_NRML,         &
     &             PIM%BPS%AMPL_FRQ_AVR(1,J22), &
     &             PIM%BPS%AMPL_FRQ_RMS(1,J22), &
     &             PIM%BPS%AMPL_TOT_RMS(J22),   &
     &             PIM%BPS%AMPL_INTEGRAL(J22)   )
         END IF
 4220 CONTINUE 
!
      PIM%CONF%PHAS_CAL_CODE = PHAS_CAL_CODE_SAVE
      DEALLOCATE ( RES )
      DEALLOCATE ( AC_AVR )
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  PIMA_BPASS_INIT  !#!#
