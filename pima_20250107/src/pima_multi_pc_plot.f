      SUBROUTINE PIMA_MULTI_PC_PLOT ( PIM, MODE, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine PIMA_MULTI_PC_PLOT prints plot of phase calibration phase  *
! *   and/or phase calibration amplitude as a function of frequency.     *
! *   This routine may not be very usefull unless all phase calibration  *
! *   tones have been extracted by the correlator.                       *
! *                                                                      *
! * ## 08-MAY-2015  PIMA_MULTI_PC_PLOT v1.6 (c) L. Petrov 17-SEP-2022 ## *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'astro_constants.i'
      INCLUDE   'pima.i'
      INCLUDE   'diagi.i'
      TYPE     ( PIMA__TYPE ) :: PIM
      CHARACTER  MODE*(*)
      INTEGER*4  IUER
      TYPE     ( DIAGI_STRU ) :: DIA(4)
      INTEGER*4    MPB, MF
      PARAMETER  ( MPB = 7 )
      PARAMETER  ( MF  = 4 )
      REAL*8,    ALLOCATABLE :: FREQ(:), FREQ_USE(:,:), &
     &                          PHAS(:,:), PHAS_AMB(:,:), PHAS_USE(:,:), &
     &                          AMPL(:,:), AMPL_SCL(:,:), AMPL_USE(:,:), &
     &                          FREQ_MOD(:), PHAS_MOD(:,:), AMPL_MOD(:,:), &
     &                          PHAS_MSK(:,:), AMPL_MSK(:,:), &
     &                          FREQ_OUT(:,:), PHAS_OUT(:,:), &
     &                          PCAL_FREQ(:,:), PCAL_AMPL(:,:), PCAL_PHAS(:,:)
      INTEGER*4, ALLOCATABLE :: IND_FRQ(:), IND_TON(:)
      REAL*8     TIM, SGN, PHS_DIF, AMPL_AVR(2), &
     &           FREQ_1ST(PIM__MFRQ), PHAS_AVR(PIM__MFRQ,2), &
     &           FRQ_DIF, FRQ_DIF_MIN, PCAL_FRQ_STEP, PHAS_LAST, PHS_FLAT, &
     &           FRQ_LAST, PCAL_FREQ_STEP(PIM__MFRQ), &
     &           PCAL_FREQ_STEP_ALL
      REAL*4     PC_GDEL(2), PC_SDEL(2), PC_SDEL_ARR(PIM__MFRQ,2)
      CHARACTER  BUTTON_NAME(MPB)*32, BUTTON_LET(MPB)*2
      CHARACTER  STR*128, STR_OBS*128, COMMON_TIT*128, PREF_NAME*128, &
     &           TITS(MF)*128, ZAG*128, UNIT*16
      LOGICAL*1  FL_BPASS, FL_USE_BPAS
      INTEGER*4  J1, J2, J3, J4, J5, J6, J7, J8, J9, J10, J11, J12, J13, J14, J15, &
     &           NO_TON, NPCL, IND_MOD, ITYP, IND_STA, KU, &
     &           B_FRQ, E_FRQ, L_FRQ, I_FRQ, U_FRQ, U_FRG, I_FRG, IND_OBS, &
     &           KP, LO(2), LP(2), IND_POL, IND_PCAL(3,2), ISTL, NC, NR, ICODE, IP, &
     &           IKS, ILS, LP_IF, ITURN, KPR(PIM__MFRQ,4), NP_MSK, LAST_TON, &
     &           INDS_TON(PIM__MFRQ,2), IAMB, NPCT, NPCT_MAX, IER
      INTEGER*4  IBST, ILST, IOST, IPST, IWST, IDEV, &
     &           ICL1, ICL2, ICL3, IND_FREQ, IND_TONE, KCHN
      REAL*4,    EXTERNAL :: PIMA_PC_GDEL
      INTEGER*4, EXTERNAL :: ILEN, I_LEN
!
      IF ( MODE .NE. PIMA__MPPL_OBS ) THEN
           CALL ERR_LOG ( 7461, IUER, 'PIMA_MULTI_PC_PLOT', 'Wrong mode: '// &
     &          MODE(1:I_LEN(MODE))//' -- only OBS is supported' )
           RETURN
      END IF
!
      PHS_FLAT = 0.0D0
      PCAL_FRQ_STEP = 10.D6
      CALL GETENVAR ( 'PIMAVAR_PCAL_FLAT', STR )
      CALL TRAN ( 11, STR, STR )
      IF ( STR(1:2) == 'NO' ) THEN
           PHS_FLAT = 1.0
      END IF
!
! --- Get frequency indexes
!
      B_FRQ = PIM%CONF%BEG_FRQ
      E_FRQ = PIM%CONF%END_FRQ
      L_FRQ = E_FRQ - B_FRQ + 1
      KCHN  = L_FRQ*PIM%NCHN
      IF ( PIM%CONF%BANDPASS_USE .NE. PIMA__BPASS_NO  .AND. &
     &     ASSOCIATED ( PIM%BPASS )                         ) THEN
           FL_BPASS = .TRUE.
         ELSE
           FL_BPASS = .FALSE.
      END IF
!
! --- Setting defaults values of the plotting parameters
!
      CALL ERR_PASS   ( IUER, IER )
      CALL DIAGI_DEF  ( IBST, ILST, IOST, IPST, IWST, IDEV, ZAG, UNIT, &
     &                  ICL1, ICL2, ICL3, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 7462, IUER, 'PIMA_PLOT_PCAL', 'Error in setting '// &
     &         'default values for the plot' )
           RETURN
      END IF
      CALL NOUT ( SIZEOF(DIA(1)), DIA(1) )
      CALL NOUT ( SIZEOF(DIA(2)), DIA(2) )
!
      NO_TON = PIM%STA(1)%PCAL(1)%NO_TONES
      NPCL = NO_TON*L_FRQ
!
! --- Memory allocation
!
      ALLOCATE ( FREQ(NPCL) )
      ALLOCATE ( FREQ_USE(NPCL,2) )
      ALLOCATE ( FREQ_MOD(KCHN)  )
      ALLOCATE ( FREQ_OUT(KCHN,2)  )
      ALLOCATE ( PCAL_FREQ(NO_TON,PIM%NFRQ) )
!
      ALLOCATE ( PHAS(NPCL,2) )
      ALLOCATE ( PHAS_AMB(NPCL,2) )
      ALLOCATE ( PHAS_USE(NPCL,2) )
      ALLOCATE ( PHAS_MOD(KCHN,2) )
      ALLOCATE ( PHAS_MSK(NPCL,2) )
      ALLOCATE ( PHAS_OUT(NPCL,2) )
      ALLOCATE ( PCAL_PHAS(NO_TON,PIM%NFRQ) )
!
      ALLOCATE ( AMPL(NPCL,2) )
      ALLOCATE ( AMPL_SCL(NPCL,2) )
      ALLOCATE ( AMPL_USE(NPCL,2) )
      ALLOCATE ( AMPL_MOD(KCHN,2) )
      ALLOCATE ( AMPL_MSK(NPCL,2) )
      ALLOCATE ( PCAL_AMPL(NO_TON,PIM%NFRQ) )
!
      ALLOCATE ( IND_FRQ(NPCL)    )
      ALLOCATE ( IND_TON(NPCL)    )
!
      PHAS      = 0.0
      PHAS_AMB  = 0.0
      PHAS_USE  = 0.0
      PHAS_MOD  = 0.0
      PHAS_MSK  = 0.0
      PHAS_OUT  = 0.0
!
      FREQ      = 0.0
      FREQ_USE  = 0.0
      FREQ_MOD  = 0.0
      FREQ_OUT  = 0.0
!
      AMPL      = 0.0
      AMPL_SCL  = 0.0
      AMPL_USE  = 0.0
      AMPL_MOD  = 0.0
      AMPL_MSK  = 0.0
!
      IF ( .NOT. ASSOCIATED ( PIM%PCAL_MASK ) ) THEN
           ALLOCATE ( PIM%PCAL_MASK(PIM%NPCT,PIM%NFRQ,PIM%NSTA), STAT=IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 7463, IUER, 'PIMA_PLOT_PCAL', 'Error in '// &
     &              'allocating dynamic memory for array PIM%PCAL_MASK' )
                RETURN
           END IF
           PIM%PCAL_MASK = 1
           PIM%PCAL_MASK_STATUS = PIMA__ALLOCATED
      END IF
!
! --- Get the list of observation indexes
!
      IF ( PIM%CONF%FRIB_OBS_STATUS  == PIMA__OBS_NO ) THEN
           CALL ERR_LOG ( 0, IUER )
           RETURN
         ELSE IF ( PIM%CONF%FRIB_OBS_STATUS  == PIMA__OBS_ALL .AND. &
     &             PIM%CONF%FRIB_NOBS        == -1                  ) THEN
           PIM%CONF%FRIB_NOBS = PIM%NOBS
           ALLOCATE ( PIM%CONF%FRIB_OBS(PIM%CONF%FRIB_NOBS), STAT=IER )
           IF ( IER .NE. 0 ) THEN
                CALL CLRCH ( STR )
                CALL IINCH ( 4*PIM%CONF%FRIB_NOBS, STR )
                CALL ERR_LOG ( 7465, IUER, 'PIMA_MULTI_PC_PLOT', &
     &              'Failure to allocate '//STR(1:I_LEN(STR))//' bytes '//&
     &              'of dynamic memory for the list of observations' )
                RETURN
           END IF
!
           DO 410 J1=1,PIM%CONF%FRIB_NOBS
              PIM%CONF%FRIB_OBS(J1) = J1
 410       CONTINUE
      END IF
!
      ISTL = 2  ! Unitial style: unwrapped pcal phase
      NC = 1
      NR = 2
      DO 420 J2=1,PIM%CONF%FRIB_NOBS
!
! ------ Store the observations index
!
         IND_OBS = PIM%CONF%FRIB_OBS(J2)
         IF ( IND_OBS < 1 .OR. IND_OBS > PIM%NOBS ) THEN
              CALL CLRCH ( STR )
              CALL INCH  ( PIM%NOBS, STR )
              CALL ERR_LOG ( 7466, IUER, 'PIMA_MULTI_PC_PLOT', 'Wrong '// &
     &            'observation index. It should be in a range of [1,'// &
     &             STR(1:I_LEN(STR))//']' )
              RETURN
         END IF
 910     CONTINUE
!
! ------ Cycle over stations
!
         DO 430 J3=1,2
            IND_STA = PIM%OBS(IND_OBS)%STA_IND(J3)
            PCAL_AMPL = 0.0
            PCAL_PHAS = 0.0
            PCAL_FREQ = 0.0
!
! --------- Set polarization index
!
            IND_POL = 0
            IF ( PIM%CONF%POLAR == PIMA__POLAR_RR .OR. &
     &           PIM%CONF%POLAR == PIMA__POLAR_HH .OR. &
     &           PIM%CONF%POLAR == PIMA__POLAR_HR .OR. &
     &           PIM%CONF%POLAR == PIMA__POLAR_HL .OR. &
     &           PIM%CONF%POLAR == PIMA__POLAR_RH .OR. &
     &           PIM%CONF%POLAR == PIMA__POLAR_RV      ) THEN
                 IND_POL = 1
              ELSE IF ( ( PIM%CONF%POLAR == PIMA__POLAR_LL .OR. &
     &                    PIM%CONF%POLAR == PIMA__POLAR_VV .OR. &
     &                    PIM%CONF%POLAR == PIMA__POLAR_VR .OR. &
     &                    PIM%CONF%POLAR == PIMA__POLAR_VL .OR. &
     &                    PIM%CONF%POLAR == PIMA__POLAR_LH .OR. &
     &                    PIM%CONF%POLAR == PIMA__POLAR_LR      ) .AND. &
     &                  PIM%NPOL == 2 ) THEN
                 IND_POL = 2
              ELSE IF ( PIM%CONF%POLAR == PIMA__POLAR_LL .AND. &
     &                  PIM%NPOL == 1 ) THEN
                 IND_POL = 1
              ELSE
                 CALL ERR_LOG ( 7467, IUER, 'PIMA_MULTI_PC_PLOT', 'Polarization '// &
     &               'code '//TRIM(PIM%CONF%POLAR)//' is not supported for '//&
     &               'plotting pcal. Supported codes: RR, LL, HH, VV' )
                 RETURN
            END IF
            IF ( J3 == 1 ) THEN
                 SGN = -1.0
              ELSE IF ( J3 == 2 ) THEN
                 SGN =  1.0
            END IF
!
            KP = 0
            LO(J3) = 0
            I_FRQ  = 0
            KPR    = 0
!
! --------- Cycle over IFs
!
            NP_MSK = 0
            PCAL_FREQ_STEP_ALL = 0.0D0
            DO 440 J4=B_FRQ,E_FRQ
               IF ( PIM%FRG_USE == PIMA__SINGLE ) THEN
                    U_FRG = PIM%CONF%FRQ_GRP
                    I_FRG = PIM%CONF%FRQ_GRP
                    U_FRQ = J4
                  ELSE IF ( PIM%FRG_USE == PIMA__MERGE ) THEN
                    U_FRG = PIM%REV_FRG(J4)
                    I_FRG = 1
                    U_FRQ = PIM%REV_FRQ(J4)
                  ELSE IF ( PIM%FRG_USE == PIMA__COMBINE ) THEN
                    U_FRG = PIM%REV_FRG(J4)
                    I_FRG = PIM%REV_FRG(J4)
                    U_FRQ = PIM%REV_FRQ(J4)
               END IF
               I_FRQ = I_FRQ + 1
!
! ------------ pcal index that corresponds to the IND_OBS th observation
!
               IND_PCAL(1:3,J3) = PIM%OBS(IND_OBS)%PCAL_IND(1:3,J3,I_FRG)
               IF ( IND_PCAL(1,J3) < 1 ) THEN
                    CALL CLRCH ( STR )
                    CALL INCH  ( IND_OBS, STR )
                    WRITE ( 6, * ) 'I_FRG= ', I_FRG
                    CALL ERR_PASS ( IUER, IER )
                    CALL ERR_LOG ( 7468, IER, 'PIMA_MULTI_PC_PLOT', &
     &                  'No phase calibration for station '// &
     &                   PIM%C_STA(IND_STA)//' for observations '// &
     &                   STR(1:I_LEN(STR))//' was found' )
                    GOTO 420
               END IF
               IF ( IND_PCAL(1,J3) > PIM%STA(IND_STA)%PCAL(U_FRG)%NPOI ) THEN
                    CALL CLRCH ( STR )
                    CALL INCH  ( IND_OBS, STR )
                    CALL ERR_LOG ( 7469, IUER, 'PIMA_MULTI_PC_PLOT', &
     &                  'Wrong phase calibration index for station '// &
     &                   PIM%C_STA(IND_STA)//' for observations '// &
     &                   STR(1:I_LEN(STR))//' was found' )
                    RETURN
               END IF
!
! ------------ Get time of the pcal measurement
!
               TIM = PIM%STA(IND_STA)%PCAL(U_FRG)%TIME_MID_R8(IND_PCAL(1,J3))
               FREQ_1ST(I_FRQ) = PIM%STA(IND_STA)%PCAL(U_FRG)%FREQ(1,U_FRQ,IND_PCAL(1,J3))
               INDS_TON(J4,1) = 0
               FRQ_LAST           = 0.0
               PCAL_FREQ_STEP(J4) = 0.0
               DO 450 J5=1,NO_TON
                  IF ( PIM%STA(IND_STA)%PCAL(U_FRG)%FREQ(J5,U_FRQ,IND_PCAL(1,J3)) > PIMA__MIN_FRQ .AND. &
     &                 PIM%STA(IND_STA)%PCAL(U_FRG)%AMPL(J5,U_FRQ,IND_PCAL(1,J3),IND_POL)* &
     &                 PIM%PCAL_MASK(J5,J4,IND_STA)                             > PIMA__AMP_MIN       ) THEN
!
                       PCAL_FREQ(J5,J4) = PIM%STA(IND_STA)%PCAL(U_FRG)%FREQ(J5,U_FRQ,IND_PCAL(1,J3))
                       PCAL_AMPL(J5,J4) = PIM%STA(IND_STA)%PCAL(U_FRG)%AMPL(J5,U_FRQ,IND_PCAL(1,J3),IND_POL)
                       PCAL_PHAS(J5,J4) = PIM%STA(IND_STA)%PCAL(U_FRG)%PHAS(J5,U_FRQ,IND_PCAL(1,J3),IND_POL)
                  END IF
!
                  KP = KP + 1
                  IF ( KPR(I_FRQ,1) == 0 ) KPR(I_FRQ,1) = KP
                  KPR(I_FRQ,2) = KP
                  FREQ(KP) = PIM%STA(IND_STA)%PCAL(U_FRG)%FREQ(J5,U_FRQ,IND_PCAL(1,J3))
                  IF ( FREQ(KP) .GE. PIMA__MIN_FRQ ) THEN
                       IF ( INDS_TON(J4,1) == 0 ) THEN
                            INDS_TON(J4,1) = J5
                            KPR(I_FRQ,3) = KP
                       END IF
                       INDS_TON(J4,2) = J5
                       KPR(I_FRQ,4)   = KP
                  END IF
                  PHAS(KP,J3) = PIM%STA(IND_STA)%PCAL(U_FRG)%PHAS(J5,U_FRQ,IND_PCAL(1,J3),IND_POL)
                  AMPL(KP,J3) = PIM%STA(IND_STA)%PCAL(U_FRG)%AMPL(J5,U_FRQ,IND_PCAL(1,J3),IND_POL)
                  IF ( FREQ(KP) > PIMA__MIN_FRQ .AND. AMPL(KP,J3) > PIMA__PCAL_AMP_MIN ) THEN
                       IF ( FRQ_LAST > PIMA__MIN_FRQ ) THEN
                            IF ( PCAL_FREQ_STEP(J4) > PIMA__MIN_FRQ ) THEN
                                 PCAL_FREQ_STEP(J4) = MIN ( FREQ(KP) - FRQ_LAST, PCAL_FREQ_STEP(J4) )
                               ELSE
                                 PCAL_FREQ_STEP(J4) = FREQ(KP) - FRQ_LAST
                            END IF
                            IF ( PCAL_FREQ_STEP_ALL > PIMA__MIN_FRQ ) THEN
                                 PCAL_FREQ_STEP_ALL = MIN ( FREQ(KP), PCAL_FREQ_STEP_ALL )
                               ELSE
                                 PCAL_FREQ_STEP_ALL = PCAL_FREQ_STEP(J4)
                            END IF
                       END IF
                       FRQ_LAST = FREQ(KP)
                   END IF
                  IF ( PIM%CONF%DEBUG_LEVEL .GE. 6 ) THEN
                       WRITE ( 6, 210 ) PIM%C_STA(IND_STA), J2, J4, J5, IND_PCAL(1,J3), IND_POL, &
     &                                  PIM%STA(IND_STA)%PCAL(U_FRG)%FREQ(J5,U_FRQ,IND_PCAL(1,J3))*1.D-6, &
     &                                  PIM%STA(IND_STA)%PCAL(U_FRG)%PHAS(J5,U_FRQ,IND_PCAL(1,J3),IND_POL), &
     &                                  PIM%STA(IND_STA)%PCAL(U_FRG)%AMPL(J5,U_FRQ,IND_PCAL(1,J3),IND_POL)
 210                   FORMAT ( 'Sta: ', A, ' Ind_obs: ', I5, ' Ifrq: ', I3, ' Ind_tone: ', I3, ' Ind_pcal: ', I6, &
     &                          ' Ind_pol: ', I1, ' Freq: ', F13.6, ' MHz ', ' Phas: ', F8.4, ' Ampl: ', F8.6 )
                  END IF
                  AMPL_MSK(KP,J3) = PIM%PCAL_MASK(J5,J4,IND_STA) * AMPL(KP,J3)
                  IND_FRQ(KP) = I_FRQ
                  IND_TON(KP) = J5
                  NP_MSK = NP_MSK + PIM%PCAL_MASK(J5,J4,IND_STA)
 450           CONTINUE
 440        CONTINUE
!
            CALL ERR_PASS ( IUER, IER )
            PC_GDEL(J3) = PIMA_PC_GDEL ( KP, FREQ, PHAS(1,J3), AMPL_MSK(1,J3), IER )
            IF ( IER .NE. 0 ) THEN
                 CALL ERR_LOG ( 7579, IUER, 'PIMA_MULTI_PC_PLOT', 'Error '// &
     &               'in computing phase-cal group delay' )
                 RETURN
            END IF
            FL_USE_BPAS = .FALSE.
            IF ( FL_BPASS ) THEN
                 IAMB = -127
                 IF ( PCAL_FREQ_STEP_ALL > PIMA__MIN_FRQ .AND. &
     &                 PIM%STA(PIM%OBS(IND_OBS)%STA_IND(J3))%PCAL(U_FRG)%PCAL_GRDEL_STATUS .EQ. PIMA__LOADED ) THEN
!
                       IAMB  = NINT ( (PC_GDEL(J3) - PIM%STA(IND_STA)%PCAL(PIM%CONF%FRQ_GRP)%PCAL_MB_GRDEL)*PCAL_FREQ_STEP_ALL )
                       PC_GDEL(J3) = PC_GDEL(J3) - IAMB/PCAL_FREQ_STEP_ALL
                       FL_USE_BPAS = .TRUE.
                  END IF
                ELSE
                  IAMB = 0
            END IF
!
! --------- NB: pima_pc_sdel assumes FREQ(NO_TON,LFRQ), PHAS(NO_TON,LFRQ), AMPL(NO_TON,LFRQ)
!
            PIM%STA(IND_STA)%PCAL(U_FRG)%PCAL_SB_GRDEL = PC_GDEL(J3)
!
            CALL ERR_PASS ( IUER, IER )
            CALL PIMA_PC_SDEL ( NO_TON, L_FRQ, PCAL_FREQ(1,B_FRQ), PCAL_PHAS(1,B_FRQ), &
     &                          PCAL_AMPL(1,B_FRQ), PIM%STA(IND_STA)%PCAL(U_FRG)%PCAL_SB_GRDEL, &
     &                          PC_SDEL(J3), PC_SDEL_ARR(1,J3), IER )
            WRITE ( 6, 220 ) PIM%C_STA(IND_STA), PC_GDEL(J3), PC_SDEL(J3), IAMB, &
     &                       FL_USE_BPAS, 1.D0/MAX(PIMA__MIN_FRQ,PCAL_FREQ_STEP_ALL)
  220       FORMAT ( 'PIMA_MULTI_PC_PLOT sta: ', A, ' PC_Gdel= ', 1PE12.5, ' PC_Sdel= ', 1PE12.5, &
     &               ' IAMB: ', I4, ' Bps: ', L1,  ' Amb_sp: ', 1PE12.5  )
!
! --------- Apply pcal group delay and compute average pcal amplitude
!
            LAST_TON  = PIM__MFRQ + 1
            PHAS_LAST = 0.0D0
            AMPL_AVR(J3) = 0.0
            DO 480 J8=1,KP
               PHAS_AMB(J8,J3) = PHAS(J8,J3) - PI2*(FREQ(J8) - FREQ_1ST(I_FRQ))*PC_SDEL_ARR(IND_FRQ(J8),J3)
               PHAS_AMB(J8,J3) = PHAS_AMB(J8,J3) - PI2*DNINT(PHAS_AMB(J8,J3)/PI2)
               IF ( AMPL_MSK(J8,J3) > 0.0 ) THEN
!
! ----------------- Now we try to resolve remaining ambiguity
!
                    IF ( IND_TON(J8) > LAST_TON ) THEN
!
! ---------------------- This tone has index older than the first index with data
!
                         ITURN = IDNINT ( (PHAS_AMB(J8,J3) - PHAS_LAST)/PI2 )
                         PHAS_AMB(J8,J3) = PHAS_AMB(J8,J3) - PI2*ITURN
                    END IF
                    PHAS_MSK(J8,J3) = PHAS_AMB(J8,J3)
                    LAST_TON = IND_TON(J8)
                    PHAS_LAST = PHAS_MSK(J8,J3)
                  ELSE
                    PHAS_MSK(J8,J3) = 0.0
               END IF
               AMPL_AVR = AMPL_AVR + AMPL(J8,J3)
 480        CONTINUE
            IF ( NP_MSK > 0 ) THEN
                 AMPL_AVR(J3) = AMPL_AVR(J3)/NP_MSK
            END IF
!
! --------- Unwrap phase ambiguity over IF and compute array with scaled amplitude: ampl_scl
!
            KP = 0
            IKS = 0
            LP(J3) = 0
            I_FRQ = 0
            KU = 0
!
            DO 490 J9=B_FRQ,E_FRQ
               IP = 0
               ILS = 0
               LP_IF = 0
               I_FRQ = I_FRQ + 1
               IF ( PIM%CONF%DEBUG_LEVEL .GE. 2 ) THEN
                    WRITE ( 6, 230 ) PIM%C_STA(IND_STA), I_FRQ, PC_SDEL_ARR(I_FRQ,J3)
 230                FORMAT ( 'PIMA_MULTI_PC_PLOT sta: ', A, ' I_frq: ', I4, ' Sdel_frq: ', 1PD12.5 )
               END IF
               IF ( PIM%FRG_USE == PIMA__SINGLE ) THEN
                    U_FRG = PIM%CONF%FRQ_GRP
                    I_FRG = PIM%CONF%FRQ_GRP
                    U_FRQ = J9
                  ELSE IF ( PIM%FRG_USE == PIMA__MERGE ) THEN
                    U_FRG = PIM%REV_FRG(J9)
                    I_FRG = 1
                    U_FRQ = J9
                  ELSE IF ( PIM%FRG_USE == PIMA__COMBINE ) THEN
                    U_FRG = PIM%REV_FRG(J9)
                    I_FRG = PIM%REV_FRG(J9)
                    U_FRQ = PIM%REV_FRQ(J9) !? not sure
               END IF
!
               FREQ_MOD((I_FRQ-1)*PIM%NCHN+1:I_FRQ*PIM%NCHN) = PIM%FREQ_ARR(1:PIM%NCHN,U_FRQ,I_FRG)
!
               IF ( PIM%CONF%DEBUG_LEVEL == 6 ) THEN
                    WRITE ( 6, * ) 'PIMA_MULTI_PC_PLOT J3= ', INT2(J3), ' I_FRQ= ', INT2(I_FRQ), ' KPR= ', INT2(KPR(I_FRQ,1:4)), ' IN = ', INT2(INDS_TON(J9,1:2))
               END IF
!
               IF ( KPR(I_FRQ,3) > 0 ) THEN
                    CALL ERR_PASS ( IUER, IER )
                    CALL PIMA_PC_MOD ( PIM, KPR(I_FRQ,4) - KPR(I_FRQ,3) + 1, &
     &                                 PIM%NCHN, U_FRQ, IND_STA, &
     &                                 FREQ(KPR(I_FRQ,3)), &
     &                                 PHAS_MSK(KPR(I_FRQ,3):KPR(I_FRQ,4),J3), &
     &                                 AMPL_MSK(KPR(I_FRQ,3):KPR(I_FRQ,4),J3), &
     &                                 0.0D0, PIM%FREQ_ARR(1,U_FRQ,I_FRG), &
     &                                 PHAS_MOD((I_FRQ-1)*PIM%NCHN+1,J3), &
     &                                 AMPL_MOD((I_FRQ-1)*PIM%NCHN+1,J3), IER )
                    IF ( IER .NE. 0 ) THEN
                         IF ( PIM%STA(IND_STA)%PCAL(PIM%CONF%FRQ_GRP)%PCAL_AVAIL .AND. &
     &                        PIM%STA(IND_STA)%PCAL(PIM%CONF%FRQ_GRP)%PCAL_USE         ) THEN
                              CALL ERR_LOG ( 7471, IUER, 'PIMA_MULTI_PC_PLOT', 'Trap of '// &
     &                            'internal control in computing parameters phase '// &
     &                            'calibration model' )
                              RETURN
                            ELSE
                              IER = -1
                              CALL ERR_LOG ( 7471, IER, 'PIMA_MULTI_PC_PLOT', 'Trap of '// &
     &                            'internal control in computing parameters phase '// &
     &                            'calibration model' )
                         END IF
                    END IF
                  ELSE
                    PHAS_MOD((I_FRQ-1)*PIM%NCHN+1:(I_FRQ-1)*PIM%NCHN+PIM%NCHN,J3) = 0.0D0
                    AMPL_MOD((I_FRQ-1)*PIM%NCHN+1:(I_FRQ-1)*PIM%NCHN+PIM%NCHN,J3) = 0.0D0
               END IF
               PHAS_AVR(I_FRQ,J3) = 0.0
               DO 4100 J10=1,NO_TON
                  KP = KP + 1
                  IF ( PIM%PCAL_MASK(J10,J9,IND_STA) == 0            ) GOTO 4100
                  IF ( J10 < INDS_TON(J9,1) .OR. J10 > INDS_TON(J9,2) ) GOTO 4100
!
                  LP(J3) = LP(J3) + 1
                  IF ( LP_IF == 0 ) LP_IF = LP(J3)
                  IP = IP + 1
                  FREQ_USE(LP(J3),J3) = FREQ(KP)
                  AMPL_USE(LP(J3),J3) = AMPL(KP,J3)
!
! --------------- ILS -- index of the last tone without this IF
! --------------- IKS -- index of the last pcal excluding the frequency that were masked out
!
                  IF ( ILS > 0 ) THEN
                       PHS_DIF = PHAS_AMB(KP,J3) - PHAS_USE(IKS,J3) + &
     &                                PI2*PHS_FLAT*(FREQ(KP) - FREQ_1ST(I_FRQ))*(PC_SDEL_ARR(I_FRQ,J3) - PC_GDEL(J3))
                       PHS_DIF = PHS_DIF - PI2*IDNINT(PHS_DIF/PI2)
                       PHAS_USE(LP(J3),J3) = PHAS_USE(IKS,J3) + PHS_DIF
                     ELSE
                       PHAS_USE(LP(J3),J3) = PHAS_AMB(KP,J3) + &
     &                                PI2*PHS_FLAT*(FREQ(KP) - FREQ_1ST(I_FRQ))*(PC_SDEL_ARR(I_FRQ,J3) - PC_GDEL(J3))
                  END IF
!
                  IF ( J10 > 1 ) THEN
                       PHS_DIF = PHAS_AMB(KP,J3) - PHAS_AMB(KP-1,J3)
                       PHS_DIF = PHS_DIF - PI2*IDNINT(PHS_DIF/PI2)
                       PHAS_AMB(KP,J3) = PHAS_AMB(KP-1,J3) + PHS_DIF
                  END IF
!
                  IF ( AMPL_AVR(J3) > PIMA__AMP_MIN ) THEN
                       AMPL_SCL(LP(J3),J3) = PI__NUM/AMPL_AVR(J3)*AMPL_USE(LP(J3),J3)
                     ELSE
                       AMPL_SCL(LP(J3),J3) = 0.0D0
                  END IF
                  IKS = LP(J3)
                  ILS = J10
                  PHAS_AVR(I_FRQ,J3) = PHAS_AVR(I_FRQ,J3) + PHAS_USE(LP(J3),J3)
 4100          CONTINUE
               IF ( IP > 0 ) THEN
                    PHAS_AVR(I_FRQ,J3) = PHAS_AVR(I_FRQ,J3)/IP
               END IF
               DO 4110 J11=LP_IF,LP(J3)
                  IF ( LP_IF < 1 ) GOTO 4110
                  IF ( I_FRQ == 1 ) THEN
                       PHAS_USE(J11,J3) = PHAS_USE(J11,J3)  - PHAS_AVR(I_FRQ,J3)
                     ELSE
                       ITURN = IDNINT((PHAS_AVR(I_FRQ,J3) - PHAS_AVR(1,J3))/PI2)
                       PHAS_USE(J11,J3) = PHAS_USE(J11,J3)  - PHAS_AVR(1,J3) - ITURN*PI2
                  END IF
 4110          CONTINUE
!
               DO 4120 J12=(I_FRQ-1)*PIM%NCHN+1,I_FRQ*PIM%NCHN
                  PHAS_MOD(J12,J3) = PHAS_MOD(J12,J3) + &
     &                               PI2*PHS_FLAT*(PIM%FREQ_ARR(J12-(I_FRQ-1)*PIM%NCHN,U_FRQ,I_FRG) - FREQ_1ST(I_FRQ))* &
     &                               (PC_SDEL_ARR(I_FRQ,J3) - PC_GDEL(J3))
                  IF ( I_FRQ == 1 ) THEN
                       PHAS_MOD(J12,J3) = PHAS_MOD(J12,J3) - PHAS_AVR(1,J3)
                     ELSE
                       ITURN = IDNINT((PHAS_AVR(I_FRQ,J3) - PHAS_AVR(1,J3))/PI2)
                       PHAS_MOD(J12,J3) = PHAS_MOD(J12,J3) - PHAS_AVR(1,J3) - ITURN*PI2
                  END IF
 4120          CONTINUE
               KP = KP - NO_TON
               DO 4130 J13=1,NO_TON
                  KP = KP + 1
                  IF ( J13 .GE. INDS_TON(J9,1) .AND. J13 .LE. INDS_TON(J9,2) ) THEN
                       KU = KU + 1
                  END IF
                  IF ( PIM%PCAL_MASK(J13,J9,IND_STA) == 0 ) THEN
                       FRQ_DIF_MIN = 1.D12
                       DO 4140 J14=1,KCHN
                          FRQ_DIF = DABS ( FREQ(KP) - FREQ_MOD(J14) )
                          IF ( FRQ_DIF < FRQ_DIF_MIN ) THEN
                               IND_MOD = J14
                               FRQ_DIF_MIN = DABS ( FREQ(KP) - FREQ_MOD(J14) )
                          END IF
 4140                  CONTINUE
                       IF ( J13 < INDS_TON(J9,1) .OR. J13 > INDS_TON(J9,2) ) GOTO 4130
!
                       LO(J3) = LO(J3) + 1
                       FREQ_OUT(LO(J3),J3) = FREQ(KP)
                       PHAS_OUT(LO(J3),J3) = PHAS_AMB(KP,J3) - PHAS_AVR(1,J3)
                       PHS_DIF = PHAS_OUT(LO(J3),J3) - PHAS_MOD(IND_MOD,J3)
                       ITURN = IDNINT(PHS_DIF/PI2)
                       PHAS_OUT(LO(J3),J3) = PHAS_OUT(LO(J3),J3) - ITURN*PI2
                  END IF
 4130          CONTINUE
 490        CONTINUE
            IF ( LP(J3) == 0 ) THEN
                 LP(J3) = 2
                 FREQ_USE(1,J3)      = FREQ_MOD(1)
                 FREQ_USE(LP(J3),J3) = FREQ_MOD(KCHN)
            END IF
 430     CONTINUE
!
! ------ Prepare the list of buttons
!
         CALL CLRCH ( STR_OBS )
         CALL INCH  ( IND_OBS, STR_OBS )
         BUTTON_LET(1)  = 'Rr'
         BUTTON_NAME(1) = 'Raw Phase'
         BUTTON_LET(2)  = 'Uu'
         BUTTON_NAME(2) = 'Unwrapped phase'
         BUTTON_LET(3)  = 'Cc'
         BUTTON_NAME(3) = 'Phase & Amplitude'
         BUTTON_LET(4)  = 'Mm'
         BUTTON_NAME(4) = 'Amplitude'
         BUTTON_LET(5)  = 'Nn'
         BUTTON_NAME(5) = 'Next obs'
         BUTTON_LET(6)  = 'Pp'
         BUTTON_NAME(6) = 'Prev obs'
         BUTTON_NAME(7) = 'Quit'
         BUTTON_LET(7)  = 'Qq'
!
 920     CONTINUE
!
! ------ Initialize DIA objects
!
         DIA(1)%IDEV = IDEV
         DO 4150 J15=1,2
            DIA(1)%NPOI(J15)   = KP
            DIA(1)%ADR_E8(J15) = 0
            DIA(1)%ADR_E8(J15) = 0
            DIA(1)%LER(J15)    = .FALSE.
            DIA(1)%ICOL(J15)   = ICL1
            DIA(1)%IBST(J15)   = 0
            DIA(1)%ILST(J15)   = 1
            DIA(1)%IPST(J15)   = 4
            DIA(1)%IOST(J15)   = 1
            DIA(1)%IWST(J15)   = 1
 4150    CONTINUE
         DIA(1)%ICOL(2)   = ICL2
         DIA(1)%STATUS = DIA__DEF
         DIA(1)%XMIN   =  1.0
         DIA(1)%XMAX   = -1.0
         DIA(1)%ARG_UNITS = 'Frequency in Hz'
         DIA(1)%YMIN   =  1.0
         DIA(1)%YMAX   = -1.0
         DIA(1)%ITRM   = 0
         DIA(1)%IBATCH = 0
         DIA(1)%STATUS = DIA__DEF
!
         DIA(2)= DIA(1)
         DIA(3)= DIA(1)
         DIA(4)= DIA(1)
!
! ------ Set DIA objects depending on plot style
!
         IF ( ISTL == 1 ) THEN
              DIA(1)%NCLR      = 1
              DIA(1)%NPOI(1)   = KP
              DIA(1)%ADR_X8(1) = LOC(FREQ)
              DIA(1)%ADR_Y8(1) = LOC(PHAS(1,1))
              DIA(1)%ZAG       = 'Raw pc phase at '//PIM%STA(PIM%OBS(IND_OBS)%STA_IND(1))%IVS_NAME// &
     &                           ' in obs #'//STR_OBS(1:I_LEN(STR_OBS))//' in '// &
     &                           TRIM(PIM%CONF%SESS_CODE)//' '//TRIM(PIM%CONF%POLAR)
              DIA(1)%NAME      = '/tmp/pcal_raw_'//PIM%STA(PIM%OBS(IND_OBS)%STA_IND(1))%IVS_NAME// &
     &                           '_'//STR_OBS(1:I_LEN(STR_OBS))//'_'
!
              DIA(2)%NCLR      = 1
              DIA(2)%NPOI(1)   = KP
              DIA(2)%ADR_X8(1) = LOC(FREQ)
              DIA(2)%ADR_Y8(1) = LOC(PHAS(1,2))
              DIA(2)%ZAG       = 'Raw pc phase at '//PIM%STA(PIM%OBS(IND_OBS)%STA_IND(2))%IVS_NAME// &
     &                           ' in obs #'//STR_OBS(1:I_LEN(STR_OBS))//' in '// &
     &                           PIM%CONF%SESS_CODE
              DIA(2)%NAME      = '/tmp/pcal_raw_'//PIM%STA(PIM%OBS(IND_OBS)%STA_IND(2))%IVS_NAME// &
     &                           '_'//STR_OBS(1:I_LEN(STR_OBS))//'_'
              TITS(1)          = 'PC raw phase at '//PIM%STA(PIM%OBS(IND_OBS)%STA_IND(1))%IVS_NAME// &
     &                           ' '//TRIM(PIM%CONF%POLAR)
              TITS(2)          = 'PC raw phase at '//PIM%STA(PIM%OBS(IND_OBS)%STA_IND(2))%IVS_NAME// &
     &                           ' '//TRIM(PIM%CONF%POLAR)
              PREF_NAME = '/tmp/pcal_raw_'//STR_OBS(1:I_LEN(STR_OBS))
              COMMON_TIT = 'Raw pc phase vs freq for obs #'// &
     &                      STR_OBS(1:I_LEN(STR_OBS))//' in '// &
     &                      TRIM(PIM%CONF%SESS_CODE)//' '//TRIM(PIM%CONF%POLAR)
            ELSE IF ( ISTL == 2 ) THEN
              DIA(1)%NCLR      = 3
              DIA(1)%NPOI(1)   = LP(1)
              DIA(1)%ADR_X8(1) = LOC(FREQ_USE(1,1))
              DIA(1)%ADR_Y8(1) = LOC(PHAS_USE(1,1))
              DIA(1)%ILST(1)   = 1
              DIA(1)%IPST(1)   = 5
              DIA(1)%NPOI(2)   = KCHN
              DIA(1)%ADR_X8(2) = LOC(FREQ_MOD(1))
              DIA(1)%ADR_Y8(2) = LOC(PHAS_MOD(1,1))
              DIA(1)%ICOL(2)   = 9
              DIA(1)%ILST(2)   = 2
              DIA(1)%IPST(2)   = 4
              DIA(1)%NPOI(3)   = LO(1)
              DIA(1)%ADR_X8(3) = LOC(FREQ_OUT(1,1))
              DIA(1)%ADR_Y8(3) = LOC(PHAS_OUT(1,1))
              DIA(1)%ICOL(3)   = 3
              DIA(1)%ILST(3)   = 1
              DIA(1)%IPST(3)   = 5
              DIA(1)%ZAG       = 'Unwrapped pc phase at '//PIM%STA(PIM%OBS(IND_OBS)%STA_IND(1))%IVS_NAME// &
     &                           ' in obs #'//STR_OBS(1:I_LEN(STR_OBS))//' in '// &
     &                           TRIM(PIM%CONF%SESS_CODE)//' '//TRIM(PIM%CONF%POLAR)
              DIA(1)%NAME      = '/tmp/pcal_unr_'//PIM%STA(PIM%OBS(IND_OBS)%STA_IND(1))%IVS_NAME// &
     &                           '_'//STR_OBS(1:I_LEN(STR_OBS))
              TITS(1)          = 'PC unwrapped phase at '//PIM%STA(PIM%OBS(IND_OBS)%STA_IND(1))%IVS_NAME// &
     &                           ' '//TRIM(PIM%CONF%POLAR)
!
              DIA(2)%NCLR      = 3
              DIA(2)%NPOI(1)   = LP(2)
              DIA(2)%ADR_X8(1) = LOC(FREQ_USE(1,2))
              DIA(2)%ADR_Y8(1) = LOC(PHAS_USE(1,2))
              DIA(2)%ILST(1)   = 1
              DIA(2)%IPST(1)   = 5
              DIA(2)%NPOI(2)   = KCHN
              DIA(2)%ADR_X8(2) = LOC(FREQ_MOD(1))
              DIA(2)%ADR_Y8(2) = LOC(PHAS_MOD(1,2))
              DIA(2)%ICOL(2)   = 9
              DIA(2)%ILST(2)   = 2
              DIA(2)%IPST(2)   = 4
              DIA(2)%NPOI(3)   = LO(2)
              DIA(2)%ADR_X8(3) = LOC(FREQ_OUT(1,2))
              DIA(2)%ADR_Y8(3) = LOC(PHAS_OUT(1,2))
              DIA(2)%ICOL(3)   = 3
              DIA(2)%ILST(3)   = 1
              DIA(2)%IPST(3)   = 5
              DIA(2)%ZAG       = 'Unwrapped pc phase at '//PIM%STA(PIM%OBS(IND_OBS)%STA_IND(2))%IVS_NAME// &
     &                           ' in obs #'//STR_OBS(1:I_LEN(STR_OBS))//' in '// &
     &                           PIM%CONF%SESS_CODE
              DIA(2)%NAME      = '/tmp/pcal_unr_'//PIM%STA(PIM%OBS(IND_OBS)%STA_IND(2))%IVS_NAME// &
     &                           '_'//STR_OBS(1:I_LEN(STR_OBS))
              TITS(2)          = 'PC unwrapped phase at '//PIM%STA(PIM%OBS(IND_OBS)%STA_IND(2))%IVS_NAME// &
     &                           ' '//TRIM(PIM%CONF%POLAR)
              PREF_NAME = '/tmp/pcal_unr_'//STR_OBS(1:I_LEN(STR_OBS))
              COMMON_TIT = 'Unwrapped pcal phase vs freq obs #'// &
     &                      STR_OBS(1:I_LEN(STR_OBS))//' in '// &
     &                      TRIM(PIM%CONF%SESS_CODE)//' '//TRIM(PIM%CONF%POLAR)
            ELSE IF ( ISTL == 3 ) THEN
              DIA(1)%NCLR      = 2
              DIA(1)%NPOI(1)   = LP(1)
              DIA(1)%NPOI(2)   = LP(1)
              DIA(1)%ADR_X8(1) = LOC(FREQ_USE(1,1))
              DIA(1)%ADR_Y8(1) = LOC(PHAS_USE(1,1))
              DIA(1)%ADR_X8(2) = LOC(FREQ_USE)
              DIA(1)%ADR_Y8(2) = LOC(AMPL_SCL(1,1))
              DIA(1)%ZAG       = 'Unwrapped pcal phase + scaled ampl at '// &
     &                           PIM%STA(PIM%OBS(IND_OBS)%STA_IND(1))%IVS_NAME// &
     &                           ' in obs #'//STR_OBS(1:I_LEN(STR_OBS))//' in '// &
     &                           TRIM(PIM%CONF%SESS_CODE)//' '//TRIM(PIM%CONF%POLAR)
              DIA(1)%NAME      = '/tmp/pcal_pc+amp_'//PIM%STA(PIM%OBS(IND_OBS)%STA_IND(1))%IVS_NAME// &
     &                           '_'//STR_OBS(1:I_LEN(STR_OBS))
              TITS(1)          = 'PC unwrapped phase + scaled ampl at '//PIM%STA(PIM%OBS(IND_OBS)%STA_IND(1))%IVS_NAME// &
     &                           ' '//TRIM(PIM%CONF%POLAR)
              DIA(1)%ILST(1)   = 2
              DIA(1)%ILST(2)   = 2
!
              DIA(2)%NCLR      = 2
              DIA(2)%NPOI(1)   = LP(2)
              DIA(2)%NPOI(2)   = LP(2)
              DIA(2)%ADR_X8(1) = LOC(FREQ_USE(1,2))
              DIA(2)%ADR_Y8(1) = LOC(PHAS_USE(1,2))
              DIA(2)%ADR_X8(2) = LOC(FREQ_USE(1,2))
              DIA(2)%ADR_Y8(2) = LOC(AMPL_SCL(1,2))
              DIA(2)%ZAG       = 'Unwrapped pcal phase + scaled ampl at '//PIM%STA(PIM%OBS(IND_OBS)%STA_IND(2))%IVS_NAME// &
     &                           ' in obs #'//STR_OBS(1:I_LEN(STR_OBS))//' in '// &
     &                           TRIM(PIM%CONF%SESS_CODE)//' '//TRIM(PIM%CONF%POLAR)
              DIA(2)%NAME      = '/tmp/pcal_pc+amp_'//PIM%STA(PIM%OBS(IND_OBS)%STA_IND(2))%IVS_NAME// &
     &                           '_'//STR_OBS(1:I_LEN(STR_OBS))
              TITS(2)          = 'PC unwrapped phase + scaled ampl at '// &
     &                           PIM%STA(PIM%OBS(IND_OBS)%STA_IND(2))%IVS_NAME// &
     &                           ' '//TRIM(PIM%CONF%POLAR)
              DIA(2)%ILST(1)   = 2
              DIA(2)%ILST(2)   = 2
              PREF_NAME = '/tmp/pcal_pc+am_'//STR_OBS(1:I_LEN(STR_OBS))
              COMMON_TIT = 'Pcal phs & ampl vs frequency for obs #'// &
     &                      STR_OBS(1:I_LEN(STR_OBS))//' in '// &
     &                      TRIM(PIM%CONF%SESS_CODE)//' '//TRIM(PIM%CONF%POLAR)
            ELSE IF ( ISTL == 4 ) THEN
              DIA(1)%NCLR      = 1
              DIA(1)%NPOI(1)   = LP(1)
              DIA(1)%ADR_X8(1) = LOC(FREQ_USE(1,1))
              DIA(1)%ADR_Y8(1) = LOC(AMPL_USE(1,1))
              DIA(1)%ICOL(1)   = ICL2
              DIA(1)%ILST(1)   = 2
              DIA(1)%ZAG       = 'pc amplitude at '//PIM%STA(PIM%OBS(IND_OBS)%STA_IND(1))%IVS_NAME// &
     &                           ' in obs #'//STR_OBS(1:I_LEN(STR_OBS))//' in '// &
     &                           TRIM(PIM%CONF%SESS_CODE)//' '//TRIM(PIM%CONF%POLAR)
              DIA(1)%NAME      = '/tmp/pcal_amp_'//PIM%STA(PIM%OBS(IND_OBS)%STA_IND(1))%IVS_NAME// &
     &                           '_'//STR_OBS(1:I_LEN(STR_OBS))//'_'
              TITS(1)          = 'PC amplitude at '// &
     &                           PIM%STA(PIM%OBS(IND_OBS)%STA_IND(1))%IVS_NAME// &
     &                           ' '//TRIM(PIM%CONF%POLAR)
!
              DIA(2)%NCLR      = 1
              DIA(2)%NPOI(1)   = LP(2)
              DIA(2)%ADR_X8(1) = LOC(FREQ_USE(1,2))
              DIA(2)%ADR_Y8(1) = LOC(AMPL_USE(1,2))
              DIA(2)%ICOL(1)   = ICL2
              DIA(2)%ILST(1)   = 2
              DIA(2)%ZAG       = 'pc amplitude at '//PIM%STA(PIM%OBS(IND_OBS)%STA_IND(2))%IVS_NAME// &
     &                           ' in obs #'//STR_OBS(1:I_LEN(STR_OBS))//' in '// &
     &                           TRIM(PIM%CONF%SESS_CODE)//' '//TRIM(PIM%CONF%POLAR)
              DIA(2)%NAME      = '/tmp/pcal_amp_'//PIM%STA(PIM%OBS(IND_OBS)%STA_IND(2))%IVS_NAME// &
     &                           '_'//STR_OBS(1:I_LEN(STR_OBS))
              TITS(2)          = 'PC amplitude at '//PIM%STA(PIM%OBS(IND_OBS)%STA_IND(2))%IVS_NAME// &
     &                           ' '//TRIM(PIM%CONF%POLAR)
              PREF_NAME = '/tmp/pcal_amp_'//STR_OBS(1:I_LEN(STR_OBS))
              COMMON_TIT = 'Pcal amplitude vs frequency for obs #'// &
     &                      STR_OBS(1:I_LEN(STR_OBS))//' in '// &
     &                      TRIM(PIM%CONF%SESS_CODE)//' '//TRIM(PIM%CONF%POLAR)
         END IF
         IF ( ISTL .LE. 4 ) THEN
              ICODE = 0
              CALL ERR_PASS ( IUER, IER )
              CALL MULTI_DIAGI ( COMMON_TIT, 2, NC, NR, TITS, MPB, &
     &                           BUTTON_NAME, BUTTON_LET, PREF_NAME, DIA, &
     &                           ICODE, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 7472, IUER, 'PIMA_MULTI_PC_PLOT', 'Failure '// &
     &                'to make a plot of phase calibration' )
                   RETURN
              END IF
         END IF
         IF ( ICODE .LE. 4 ) THEN
!
! ----------- Repeat the plot in the different style
!
              ISTL = ICODE
              GOTO 920
            ELSE IF ( ICODE == 5 ) THEN
!
! ----------- Got to the next observation
!
              IND_OBS = IND_OBS + 1
              IF ( IND_OBS > PIM%NOBS ) IND_OBS = 1
              GOTO 910
            ELSE IF ( ICODE == 6 ) THEN
!
! ----------- Got to the previous observation
!
              IND_OBS = IND_OBS - 1
              IF ( IND_OBS < 1 ) IND_OBS = PIM%NOBS
              GOTO 910
         END IF
 420  CONTINUE
!
! --- Good bye, my love, good bye.
!
      DEALLOCATE ( IND_TON   )
      DEALLOCATE ( IND_FRQ   )
      DEALLOCATE ( PCAL_AMPL )
      DEALLOCATE ( AMPL_MSK  )
      DEALLOCATE ( AMPL_MOD  )
      DEALLOCATE ( AMPL_USE  )
      DEALLOCATE ( AMPL_SCL  )
      DEALLOCATE ( AMPL      )
      DEALLOCATE ( PCAL_PHAS )
      DEALLOCATE ( PHAS_OUT  )
      DEALLOCATE ( PHAS_MSK  )
      DEALLOCATE ( PHAS_USE  )
      DEALLOCATE ( PHAS_MOD  )
      DEALLOCATE ( PHAS_AMB  )
      DEALLOCATE ( PHAS      )
      DEALLOCATE ( PCAL_FREQ )
      DEALLOCATE ( FREQ_OUT  )
      DEALLOCATE ( FREQ_USE  )
      DEALLOCATE ( FREQ      )
      IF ( ASSOCIATED ( PIM%CONF%FRIB_OBS ) ) DEALLOCATE ( PIM%CONF%FRIB_OBS )
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  PIMA_MULTI_PC_PLOT  !#!#
