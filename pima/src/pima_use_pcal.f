      SUBROUTINE PIMA_USE_PCAL ( PIM, OP_CODE, IND_OBS, N_FRQ, B_FRQ, E_FRQ, &
     &                           IND_POL, PCAL_C8, PC_GDEL, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine PIMA_USE_PCAL 
! *                                                                      *
! * ### 12-JAN-2006  PIMA_USE_PCAL   v6.1 (c) L. Petrov  15-DEC-2023 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'pima.i'
      TYPE     ( PIMA__TYPE  ) :: PIM
      REAL*4     PI, PI2
      PARAMETER  ( PI  = 3.141592653589793E0 )
      PARAMETER  ( PI2 = 2.0E0*PI            )
      INTEGER*4  OP_CODE, IND_OBS, N_FRQ, B_FRQ, E_FRQ, IUER
      COMPLEX*8  PCAL_C8(PIM%NCHN,N_FRQ,2)
      REAL*8,    ALLOCATABLE :: PCAL_FREQ(:), PCAL_PHAS(:), PCAL_AMPL(:), &
                                PHAS_MOD(:), AMPL_MOD(:)
      REAL*8  t8(8192), x8(8192), PHS_DIF
      INTEGER*4  J1, J2, J3, J4, J5, J6, J7, J8, J9, J10, J11, J12, IP, IB, IE, I_FRQ, &
     &           IND_POL, IND_TONE, U_FRQ, U_FRG, F_FRQ, F_FRG, L_FRQ, L_PCT, &
     &           NPCT, NPCT_MAX, LA_CHN, I_FRG, IND_TONE_BEG, IND_TONE_END, &
     &           NUM_USED_TONES, IAMB, IND_PUS_STA, IER
      INTEGER*4  IND_STA, IND_PCAL(4,2)
      LOGICAL*1  FL_GRD, FL_PCAL_TOUSE, FL_PCAL_USED, FL_BPASS
      CHARACTER  STR*128, STR1*128, STR_PCAL_NOAVR*8
      REAL*4     PHAS_PCAL, PHAS_DIF, FREQ_DIF, SGN
      REAL*8     TIM_PCAL_BEG, TIM_PCAL_END, TIM_PCAL_END_PREV, &
     &           FREQ, CHAN_WIDTH, DIF_PHAS, PHAS_RATE, PC_FREQ_REF, &
     &           FRQ_LAST, PCAL_FREQ_STEP(PIM__MFRQ), &
     &           PCAL_FREQ_STEP_ALL
      REAL*4     PC_GDEL(2), PC_SDEL(2), PCI_SDEL(2), PC_SDEL_ARR(PIM__MFRQ,2)
      REAL*4     PHAS_CMPL_R4
      REAL*4,    EXTERNAL :: PIMA_PC_GDEL
      CHARACTER, EXTERNAL :: MJDSEC_TO_DATE*30
      INTEGER*4, EXTERNAL :: ILEN, I_LEN, LTM_DIF
!
      IF ( OP_CODE == PIMA__APPLY_PCAL .OR. OP_CODE == PIMA__SET_PCAL ) THEN
           CONTINUE 
         ELSE
           CALL ERR_LOG ( 7571, IUER, 'PIMA_USE_PCAL', 'Trap of internal '// &
     &         'control: unsupported OP_CODE '//TRIM(STR)//' parameter' )
           RETURN
      END IF 
!
      NPCT_MAX = MAX ( PIM%STA(PIM%OBS(IND_OBS)%STA_IND(1))%PCAL(PIM%CONF%FRQ_GRP)%NO_TONES, &
     &                 PIM%STA(PIM%OBS(IND_OBS)%STA_IND(2))%PCAL(PIM%CONF%FRQ_GRP)%NO_TONES  )
      IF ( .NOT. ASSOCIATED ( PIM%PCAL_MASK ) ) THEN
           ALLOCATE ( PIM%PCAL_MASK(NPCT_MAX,PIM%NFRQ,PIM%NSTA), STAT=IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 7572, IUER, 'PIMA_USE_PCAL', 'Error in '// &
     &              'allocating dynamic memory for array PIM%PCAL_MASK' )
                RETURN
           END IF
           PIM%PCAL_MASK = 1
           PIM%PCAL_MASK_STATUS = PIMA__ALLOCATED
      END IF
!
      CALL CLRCH    ( STR_PCAL_NOAVR )
      CALL GETENVAR ( 'PIMAVAR_PCAL_NOAVR', STR_PCAL_NOAVR )
      IF ( ILEN(STR_PCAL_NOAVR) > 0 ) THEN
           CALL TRAN ( 11, STR_PCAL_NOAVR, STR_PCAL_NOAVR )
      END IF
!
      IF ( PIM%CONF%BANDPASS_USE .NE. PIMA__BPASS_NO  .AND. &
     &     ASSOCIATED ( PIM%BPASS )                         ) THEN
           FL_BPASS = .TRUE.
         ELSE
           FL_BPASS = .FALSE.
      END IF
      PC_GDEL = 0.0
!
      IF ( PIM%CONF%PHAS_CAL_CODE == PIMA__PCAL_NO ) THEN
           IF ( OP_CODE == PIMA__APPLY_PCAL ) THEN
!
! ------------- No pcal to use? Nothing to do!
!
                PCAL_C8 = CMPLX ( 1.0, 0.0 )
                CALL ERR_LOG ( 0, IUER )
                RETURN
           END IF
!
           IF ( OP_CODE == PIMA__SET_PCAL ) THEN
!
! ------------- Initialization of pcal group delays.
!
                DO 410 J1=B_FRQ,E_FRQ
                   I_FRQ = I_FRQ + 1
                   IF ( PIM%FRG_USE == PIMA__SINGLE ) THEN
                        U_FRG = PIM%CONF%FRQ_GRP
                        U_FRQ = J1
                      ELSE IF ( PIM%FRG_USE == PIMA__MERGE ) THEN
                        U_FRG = PIM%REV_FRG(J1)
                        U_FRQ = PIM%REV_FRQ(J1)
                      ELSE IF ( PIM%FRG_USE == PIMA__COMBINE ) THEN
                        U_FRG = PIM%REV_FRG(J1)
                        U_FRQ = PIM%REV_FRQ(J1)
                   END IF
!
                   PIM%STA(PIM%OBS(IND_OBS)%STA_IND(1))%PCAL(U_FRG)%PCAL_MB_GRDEL     = 0.0
                   PIM%STA(PIM%OBS(IND_OBS)%STA_IND(1))%PCAL(U_FRG)%PCAL_SB_GRDEL(J1) = 0.0
                   PIM%STA(PIM%OBS(IND_OBS)%STA_IND(1))%PCAL(U_FRG)%PCAL_GRDEL_STATUS = PIMA__INIT
!
                   PIM%STA(PIM%OBS(IND_OBS)%STA_IND(2))%PCAL(U_FRG)%PCAL_MB_GRDEL     = 0.0
                   PIM%STA(PIM%OBS(IND_OBS)%STA_IND(2))%PCAL(U_FRG)%PCAL_SB_GRDEL(J1) = 0.0
                   PIM%STA(PIM%OBS(IND_OBS)%STA_IND(2))%PCAL(U_FRG)%PCAL_GRDEL_STATUS = PIMA__INIT
 410            CONTINUE 
           END IF
!
           CALL ERR_LOG ( 0, IUER )
           RETURN
      END IF
!
      DO 420 J2=1,2 ! Cycle over stations
!
! ------ Initialization
!
         I_FRQ = 0
         DO 430 J3=B_FRQ,E_FRQ
            I_FRQ = I_FRQ + 1
            DO 440 J4=1,PIM%NCHN
               PCAL_C8(J4,I_FRQ,J2) = CMPLX ( 1.0, 0.0 )
 440        CONTINUE 
 430     CONTINUE 
         FL_PCAL_USED = .FALSE.
!
         IND_STA = PIM%OBS(IND_OBS)%STA_IND(J2)
         IF ( J2 == 1 ) THEN
              SGN = -1.0
            ELSE IF ( J2 == 2 ) THEN
              SGN =  1.0
         END IF
!
         IF ( .NOT. ( IND_POL == 1 .OR. IND_POL == 2 ) ) THEN
              WRITE ( 6, * ) ' J2= ', J2, ' IND_POL = ', IND_POL
              CALL ERR_LOG ( 7573, IUER, 'PIMA_USE_PCAL', 'Trap of internal '// &
     &            'control: wrong IND_POL index' )
              RETURN 
         END IF
!
         FL_PCAL_TOUSE = PIM%STA(IND_STA)%PCAL(PIM%CONF%FRQ_GRP)%PCAL_AVAIL .AND. &
     &                   PIM%STA(IND_STA)%PCAL(PIM%CONF%FRQ_GRP)%PCAL_USE
         IF ( PIM%CONF%L_PUS > 0 ) THEN
!
! ----------- Fine-grained pcal selection
!
              IND_PUS_STA = LTM_DIF ( 0, PIM%CONF%L_PUS, &
     &                                PIM%CONF%PCAL_USE_STA, PIM%C_STA(IND_STA) )
              IF ( PIM%CONF%PUS_TYPE == PIMA__USE ) THEN
!
! ---------------- The fine-grained list sets the list of station to use pcal
!
                   IF ( IND_PUS_STA .LT. 1 ) FL_PCAL_TOUSE = .FALSE.
                 ELSE IF ( PIM%CONF%PUS_TYPE == PIMA__NOT_USE ) THEN
!
! ---------------- The fine-grained list sets the list of station not to use pcal
!
                   IF ( IND_PUS_STA .GE. 1 ) FL_PCAL_TOUSE = .FALSE.
              END IF
         END IF
!
         CALL CHIN ( PIM%CONF%PHAS_CAL_CODE, IND_TONE )
         IF ( FL_PCAL_TOUSE .AND. &
     &        ( PIM%STA(IND_STA)%PCAL(PIM%CONF%FRQ_GRP)%NO_TONES == 1  .OR. &
     &          PIM%CONF%PHAS_CAL_CODE .EQ. PIMA__PCAL_USE_ONE         .OR. &
     &          ( IND_TONE .GE. 1 .AND. IND_TONE .LE. PIM__MTON )           ) ) THEN
!
              I_FRQ = 0
              DO 450 J5=B_FRQ,E_FRQ
                 I_FRQ = I_FRQ + 1
                 IF ( PIM%FRG_USE == PIMA__SINGLE ) THEN
                      U_FRG = PIM%CONF%FRQ_GRP
                      U_FRQ = J5
                    ELSE IF ( PIM%FRG_USE == PIMA__MERGE ) THEN
                      U_FRG = PIM%REV_FRG(J5)
                      U_FRQ = PIM%REV_FRQ(J5)
                    ELSE IF ( PIM%FRG_USE == PIMA__COMBINE ) THEN
                      U_FRG = PIM%REV_FRG(J5)
                      U_FRQ = PIM%REV_FRQ(J5)
                 END IF
                 IF ( PIM%STA(IND_STA)%PCAL(U_FRG)%NO_TONES > 2 ) THEN
!
! ------------------- Select the tone of phase calibrartion to use
!
                      IF ( IND_TONE < 1 .OR. IND_TONE > PIM%STA(IND_STA)%PCAL(U_FRG)%NO_TONES ) THEN
!
! ------------------------ The tone was not selected by the user, or was not
! ------------------------ selected correctly. Let us set it ourselves
!
                           IND_TONE = PIM%STA(IND_STA)%PCAL(U_FRG)%NO_TONES/3
                      END IF
                    ELSE 
                      IF ( IND_TONE > PIM%STA(IND_STA)%PCAL(U_FRG)%NO_TONES .OR. &
     &                     IND_TONE < 1                                          ) THEN
                           IND_TONE = 1
                      END IF
                 END IF
                 IND_PCAL(1:4,J2) = PIM%OBS(IND_OBS)%PCAL_IND(1:4,J2,U_FRG)
!
                 IF ( STR_PCAL_NOAVR(1:3) == 'YES' ) THEN
                      IND_PCAL(2:4,J2) = 0
                 END IF
                 IF ( OP_CODE == PIMA__SET_PCAL ) THEN
!
! ------------------- Initialization of pcal group delays. It is zero in the one tone mode
!
                      PIM%STA(IND_STA)%PCAL(U_FRG)%PCAL_MB_GRDEL     = 0.0
                      PIM%STA(IND_STA)%PCAL(U_FRG)%PCAL_SB_GRDEL(J5) = 0.0
                      PIM%STA(IND_STA)%PCAL(U_FRG)%PCAL_GRDEL_STATUS = PIMA__INIT
                 END IF
!
                 IF ( IND_PCAL(1,J2) < 1 .OR. &
     &                IND_PCAL(1,J2) > PIM%STA(IND_STA)%PCAL(U_FRG)%NPOI ) THEN
                      IF ( I_FRQ == 1 .AND. PIM%CONF%DEBUG_LEVEL .GE. 6 ) THEN
                            WRITE  ( 6, 203 ) IND_TONE, PIM%C_STA(IND_STA)
 203                        FORMAT ( 'PIMA_USE_PCAL: Ind_obs: ', I4, &
     &                               ' Station: ', A, ' No pcal tone was found' )
                      END IF
                      GOTO 450
                 END IF
!
! -------------- Get pcal phase
!
                 IF ( PIM%STA(IND_STA)%PCAL(U_FRG)%PCAL_SCA .AND. IND_PCAL(4,J2) > 0 .AND. &
     &                IND_PCAL(4,J2) .LE. PIM%STA(IND_STA)%PCAL(U_FRG)%NPOI ) THEN
                      PHAS_PCAL = SGN*PIM%STA(IND_STA)%PCAL(U_FRG)%PHAS_SCA(IND_TONE,U_FRQ,IND_PCAL(4,J2),IND_POL) 
                    ELSE IF ( IND_PCAL(3,J2) > 0 ) THEN
                      PHAS_PCAL = SGN*PIM%STA(IND_STA)%PCAL(U_FRG)%PHAS(IND_TONE,U_FRQ,IND_PCAL(3,J2),IND_POL) 
                    ELSE
                      PHAS_PCAL = SGN*PIM%STA(IND_STA)%PCAL(U_FRG)%PHAS(IND_TONE,U_FRQ,IND_PCAL(1,J2),IND_POL) 
                 END IF
                 IF ( I_FRQ == 1 .AND. PIM%CONF%DEBUG_LEVEL .GE. 6 ) THEN
                      WRITE  ( 6, 205 ) IND_TONE, PIM%C_STA(IND_STA)
 205                  FORMAT ( 'PIMA_USE_PCAL: tone ', I4, ' was selected for station ', A)
                 END IF
                 IF ( PIM%CONF%DEBUG_LEVEL == 10 ) THEN
                      WRITE  ( 6, 210 ) IND_OBS, PIM%C_STA(IND_STA), &
     &                         J5, IND_PCAL(1:4,J2), IND_TONE, PHAS_PCAL, PHAS_PCAL*180.0D0/PI, &
     &                         U_FRG, U_FRQ, IND_POL, PIM%STA(IND_STA)%PCAL(U_FRG)%PCAL_SCA
 210                  FORMAT ( 'PIMA_USE_PCAL Ind_obs: ', I6, &
     &                         ' sta: ', A, ' IFRQ: ', I4, ' Pcal_ind: ', 4(I6,1X), &
     &                         ' Ind_tone: ', I4, ' Ph_cal: ', F8.5, ' (rad) ', F8.3, ' (deg) ', &
     &                         ' Inds: ', I1, 1X, I4, 1X, I1, ' Pcal_sca_flag: ', L1 )
                 END IF
                 DO 460 J6=1,PIM%NCHN
                    PCAL_C8(J6,I_FRQ,J2) = CMPLX ( COS(PHAS_PCAL), &
     &                                             SIN(PHAS_PCAL) )
 460             CONTINUE 
 450          CONTINUE 
              FL_PCAL_USED = .TRUE.
           ELSE IF ( FL_PCAL_TOUSE .AND. &
     &               ( PIM%STA(IND_STA)%PCAL(PIM%CONF%FRQ_GRP)%NO_TONES > 2 .AND. &
     &                 PIM%CONF%PHAS_CAL_CODE == PIMA__PCAL_USE_ALL               ) ) THEN
              NPCT   = PIM%STA(IND_STA)%PCAL(PIM%CONF%FRQ_GRP)%NO_TONES 
              L_FRQ  = E_FRQ - B_FRQ + 1
              L_PCT  = NPCT*L_FRQ
              LA_CHN = PIM%NCHN*L_FRQ
!
              IF ( ALLOCATED ( PCAL_FREQ ) ) DEALLOCATE ( PCAL_FREQ )
              ALLOCATE ( PCAL_FREQ(L_PCT), STAT=IER )
              IF ( IER .NE. 0 ) THEN
                   CALL CLRCH ( STR ) 
                   CALL IINCH ( 8*L_PCT, STR )
                   CALL ERR_LOG ( 7574, IUER, 'PIMA_USE_PCAL', 'Failure to '// &
     &                 'allocate '//STR(1:I_LEN(STR))//' bytes of dynamic '// &
     &                 'memory' )
                   RETURN 
              END IF
!
              ALLOCATE ( PCAL_PHAS(L_PCT), STAT=IER )
              IF ( IER .NE. 0 ) THEN
                   CALL CLRCH ( STR ) 
                   CALL IINCH ( 8*L_PCT, STR )
                   CALL ERR_LOG ( 7575, IUER, 'PIMA_USE_PCAL', 'Failure to '// &
     &                 'allocate '//STR(1:I_LEN(STR))//' bytes of dynamic '// &
     &                 'memory' )
                   RETURN 
              END IF
!
              ALLOCATE ( PCAL_AMPL(L_PCT), STAT=IER )
              IF ( IER .NE. 0 ) THEN
                   CALL CLRCH ( STR ) 
                   CALL IINCH ( 8*L_PCT, STR )
                   CALL ERR_LOG ( 7576, IUER, 'PIMA_USE_PCAL', 'Failure to '// &
     &                 'allocate '//STR(1:I_LEN(STR))//' bytes of dynamic '// &
     &                 'memory' )
                   RETURN 
              END IF
!
              ALLOCATE ( PHAS_MOD(LA_CHN), STAT=IER )
              IF ( IER .NE. 0 ) THEN
                   CALL CLRCH ( STR ) 
                   CALL IINCH ( 8*LA_CHN, STR )
                   CALL ERR_LOG ( 7577, IUER, 'PIMA_USE_PCAL', 'Failure to '// &
     &                 'allocate '//STR(1:I_LEN(STR))//' bytes of dynamic '// &
     &                 'memory' )
                   RETURN 
              END IF
!
              ALLOCATE ( AMPL_MOD(LA_CHN), STAT=IER )
              IF ( IER .NE. 0 ) THEN
                   CALL CLRCH ( STR ) 
                   CALL IINCH ( 8*LA_CHN, STR )
                   CALL ERR_LOG ( 7578, IUER, 'PIMA_USE_PCAL', 'Failure to '// &
     &                 'allocate '//STR(1:I_LEN(STR))//' bytes of dynamic '// &
     &                 'memory' )
                   RETURN 
              END IF
!
              I_FRQ = 0
              IP = 0
              PCAL_FREQ_STEP_ALL = 0.0D0
              DO 470 J7=B_FRQ,E_FRQ
                 IF ( PIM%FRG_USE == PIMA__SINGLE ) THEN
                      U_FRG = PIM%CONF%FRQ_GRP
                      I_FRG = PIM%CONF%FRQ_GRP
                      U_FRQ = J7
                    ELSE IF ( PIM%FRG_USE == PIMA__MERGE ) THEN
                      U_FRG = PIM%REV_FRG(J7)
                      I_FRG = 1
                      U_FRQ = PIM%REV_FRQ(J7)
                    ELSE IF ( PIM%FRG_USE == PIMA__COMBINE ) THEN
                      U_FRG = PIM%REV_FRG(J7)
                      I_FRG = PIM%REV_FRG(J7)
                      U_FRQ = PIM%REV_FRQ(J7)
                 END IF
                 IND_PCAL(1:4,J2) = PIM%OBS(IND_OBS)%PCAL_IND(1:4,J2,I_FRG)
                 IF ( OP_CODE == PIMA__SET_PCAL ) THEN
!
! ------------------- Initialization of pcal group delays
!
                      PIM%STA(IND_STA)%PCAL(U_FRG)%PCAL_MB_GRDEL     = 0.0
                      PIM%STA(IND_STA)%PCAL(U_FRG)%PCAL_SB_GRDEL(J7) = 0.0
                 END IF
                 IF ( IND_PCAL(1,J2) < 1 ) GOTO 470
                 IF ( IND_PCAL(1,J2) > PIM%STA(IND_STA)%PCAL(U_FRG)%NPOI ) GOTO 470
!
                 FRQ_LAST           = 0.0
                 PCAL_FREQ_STEP(J7) = 0.0
!
                 IF ( STR_PCAL_NOAVR == 'YES' ) THEN
                      IND_PCAL(2:4,J2) = 0
                 END IF
                 DO 480 J8=1,NPCT
                    IP = IP + 1
                    IF ( PIM%STA(IND_STA)%PCAL(U_FRG)%PCAL_SCA .AND. &
     &                  IND_PCAL(4,J2) > 0 .AND. IND_PCAL(4,J2) .LE. PIM%STA(IND_STA)%PCAL(U_FRG)%NPOI ) THEN
                         PCAL_PHAS(IP) = PIM%STA(IND_STA)%PCAL(U_FRG)%PHAS_SCA(J8,U_FRQ,IND_PCAL(4,J2),IND_POL) 
                         PCAL_AMPL(IP) = PIM%STA(IND_STA)%PCAL(U_FRG)%AMPL_SCA(J8,U_FRQ,IND_PCAL(4,J2),IND_POL) 
                      ELSE IF ( IND_PCAL(3,J2) > 0 ) THEN
                         PCAL_PHAS(IP) = PIM%STA(IND_STA)%PCAL(U_FRG)%PHAS(J8,U_FRQ,IND_PCAL(3,J2),IND_POL) 
                         PCAL_AMPL(IP) = PIM%STA(IND_STA)%PCAL(U_FRG)%AMPL(J8,U_FRQ,IND_PCAL(3,J2),IND_POL) 
                      ELSE
                         PCAL_PHAS(IP) = PIM%STA(IND_STA)%PCAL(U_FRG)%PHAS(J8,U_FRQ,IND_PCAL(1,J2),IND_POL) 
                         PCAL_AMPL(IP) = PIM%STA(IND_STA)%PCAL(U_FRG)%AMPL(J8,U_FRQ,IND_PCAL(1,J2),IND_POL) 
                    END IF
!
                    PCAL_PHAS(IP) = PIM%PCAL_MASK(J8,J7,IND_STA)*PCAL_PHAS(IP)
                    PCAL_AMPL(IP) = PIM%PCAL_MASK(J8,J7,IND_STA)*PCAL_AMPL(IP)
!
                    PCAL_FREQ(IP) = PIM%STA(IND_STA)%PCAL(U_FRG)%FREQ(J8,U_FRQ,IND_PCAL(1,J2)) 
                    IF ( PCAL_FREQ(IP) > PIMA__MIN_FRQ .AND. PCAL_AMPL(IP) > PIMA__PCAL_AMP_MIN ) THEN
                         IF ( FRQ_LAST > PIMA__MIN_FRQ ) THEN
                              IF ( PCAL_FREQ_STEP(J7) > PIMA__MIN_FRQ ) THEN
                                   PCAL_FREQ_STEP(J7) = MIN ( PCAL_FREQ(IP) - FRQ_LAST, PCAL_FREQ_STEP(J7) )
                                 ELSE
                                   PCAL_FREQ_STEP(J7) = PCAL_FREQ(IP) - FRQ_LAST
                              END IF
                              IF ( PCAL_FREQ_STEP_ALL > PIMA__MIN_FRQ ) THEN
                                   PCAL_FREQ_STEP_ALL = MIN ( PCAL_FREQ_STEP(J7), PCAL_FREQ_STEP_ALL )
                                 ELSE
                                   PCAL_FREQ_STEP_ALL = PCAL_FREQ_STEP(J7)
                              END IF
                         END IF
                         FRQ_LAST = PCAL_FREQ(IP) 
                    END IF
 480             CONTINUE 
 470          CONTINUE 
!
              IF ( IP .GE. 2 ) THEN
!
! ---------------- If the number of used phase-cal tones > 2, compute group delay
!
                   CALL ERR_PASS ( IUER, IER )
                   PC_GDEL(J2) = PIMA_PC_GDEL ( IP, PCAL_FREQ, PCAL_PHAS, PCAL_AMPL, IER )
                   IF ( IER .NE. 0 ) THEN
                        CALL ERR_LOG ( 7579, IUER, 'PIMA_USE_PCAL', 'Error '// &
     &                      'in computing phase-cal group delay' )
                        RETURN 
                   END IF
                   IAMB = -127
                   IF ( FL_BPASS ) THEN
                        IF ( PCAL_FREQ_STEP_ALL > PIMA__MIN_FRQ .AND. &
     &                       PIM%STA(PIM%OBS(IND_OBS)%STA_IND(J2))%PCAL(U_FRG)%PCAL_GRDEL_STATUS .EQ. PIMA__LOADED ) THEN
                             IAMB  = NINT ( (PC_GDEL(J2) - PIM%STA(IND_STA)%PCAL(PIM%CONF%FRQ_GRP)%PCAL_MB_GRDEL)*PCAL_FREQ_STEP_ALL )
                             PC_GDEL(J2) = PC_GDEL(J2) - IAMB/PCAL_FREQ_STEP_ALL 
                        END IF
                   END IF
                 ELSE
                   PC_GDEL(J2) = 0.0D0
              END IF
              IF ( PIM%CONF%DEBUG_LEVEL == 9 .OR. PIM%CONF%DEBUG_LEVEL == 10 ) THEN
                   WRITE ( 6, 220 ) PIM%C_STA(IND_STA), IND_OBS, IND_POL, PC_GDEL(J2), &
     &                              PIM%STA(PIM%OBS(IND_OBS)%STA_IND(J2))%PCAL(U_FRG)%PCAL_GRDEL_STATUS, &
     &                              IAMB, PIM%STA(IND_STA)%PCAL(U_FRG)%PCAL_MB_GRDEL
 220               FORMAT ( 'PIMA_USE_PCAL sta: ', A, ' Ind_obs: ', I6, ' Ind_pol: ', I1, &
     &                      '  Gdel: ', 1PE11.4, ' Gdel_status: ', I7, ' IAMB: ', I4, &
     &                      ' Stored_Gdel: ', 1PD12.5  )
              END IF
!
              I_FRQ = 0
              IP = 1
              DO 490 J9=B_FRQ,E_FRQ
                 IF ( IND_PCAL(1,J2) < 1 ) GOTO 490
                 IF ( IND_PCAL(1,J2) > PIM%STA(IND_STA)%PCAL(U_FRG)%NPOI ) GOTO 490
                 IF ( PIM%FRG_USE == PIMA__SINGLE ) THEN
                      U_FRG = PIM%CONF%FRQ_GRP
                      U_FRQ = J9
                      F_FRQ = J9
                      F_FRG = PIM%CONF%FRQ_GRP
                    ELSE IF ( PIM%FRG_USE == PIMA__MERGE ) THEN
                      U_FRG = PIM%REV_FRG(J9)
                      U_FRQ = PIM%REV_FRQ(J9)
                      F_FRQ = J9
                      F_FRG = 1
                    ELSE IF ( PIM%FRG_USE == PIMA__COMBINE ) THEN
                      U_FRG = PIM%REV_FRG(J9)
                      U_FRQ = PIM%REV_FRQ(J9)
                      F_FRG = PIM%REV_FRG(J9)
                      F_FRQ = PIM%REV_FRQ(J9)
                 END IF
                 I_FRQ = I_FRQ + 1
!
                 IND_TONE_BEG   = 0
                 IND_TONE_END   = 0
                 NUM_USED_TONES = 0
                 DO 4100 J10=1,NPCT
                    IF ( PCAL_FREQ(J10+NPCT*(I_FRQ-1)) .GE. PIMA__MIN_FRQ ) THEN
                         IF (  IND_TONE_BEG == 0 ) IND_TONE_BEG = J10
                         IND_TONE_END = J10
                         NUM_USED_TONES = NUM_USED_TONES + 1
                    END IF
 4100            CONTINUE 
!
                 IF ( NUM_USED_TONES == 0 ) THEN
                      CALL CLRCH ( STR )
                      CALL INCH  ( IND_OBS, STR )
                      CALL CLRCH ( STR )
                      CALL INCH  ( J9,  STR1 )
!@                      CALL ERR_LOG ( 7580, IUER, 'PIMA_USE_PCAL', 'Trap of '// &
!@     &                    'internal control in computing parameters phase '// &
!@     &                    'calibration model for station '//PIM%C_STA(IND_STA)// &
!@     &                    ' observation '//TRIM(STR)//' -- no used phase '// &
!@     &                    'calibration tones for IF '//STR1 )
!@                      RETURN 
                      NUM_USED_TONES = 1
                      IND_TONE_BEG   = 1
                      IND_TONE_END   = 1
                 END IF
!
                 CALL ERR_PASS ( IUER, IER )
                 CALL PIMA_PC_MOD ( PIM, NUM_USED_TONES, PIM%NCHN, & 
     &                              U_FRQ, IND_STA, PCAL_FREQ(IND_TONE_BEG+NPCT*(I_FRQ-1)), &
     &                              PCAL_PHAS(IND_TONE_BEG+NPCT*(I_FRQ-1)), &
     &                              PCAL_AMPL(IND_TONE_BEG+NPCT*(I_FRQ-1)), &
     &                              PC_GDEL(J2), &
     &                              PIM%FREQ_ARR(1,F_FRQ,F_FRG), &
     &                              PHAS_MOD((I_FRQ-1)*PIM%NCHN+1), &
     &                              AMPL_MOD((I_FRQ-1)*PIM%NCHN+1), IER )
                 IF ( IER .NE. 0 ) THEN
                      WRITE ( 6, * ) ' NPCT=  ', NPCT, ' I_FRQ= ', I_FRQ
                      WRITE ( 6, * ) ' PF= ', PCAL_FREQ(1:NPCT)
                      CALL CLRCH ( STR )
                      CALL INCH  ( IND_OBS, STR )
                      IF ( ALLOCATED ( PCAL_FREQ ) ) DEALLOCATE ( PCAL_FREQ )
                      IF ( ALLOCATED ( PCAL_PHAS ) ) DEALLOCATE ( PCAL_PHAS )
                      IF ( ALLOCATED ( PCAL_AMPL ) ) DEALLOCATE ( PCAL_AMPL )
                      IF ( ALLOCATED ( PHAS_MOD  ) ) DEALLOCATE ( PHAS_MOD  )
                      IF ( ALLOCATED ( AMPL_MOD  ) ) DEALLOCATE ( AMPL_MOD  )
!
                      CALL ERR_LOG ( 7581, IUER, 'PIMA_USE_PCAL', 'Trap of '// &
     &                    'internal control in computing parameters phase '// &
     &                    'calibration model for station '//PIM%C_STA(IND_STA)// &
     &                    ' observation '//STR )
                      RETURN
                 END IF
!
                 IF ( OP_CODE == PIMA__SET_PCAL ) THEN
!
! ------------------- Store phase cal group delay over a given IF
!
! ------------------- Important: the input single band group delay is 
! ------------------- set to group delay
!
                      PCI_SDEL(J2) = PC_GDEL(J2)
                      CALL PIMA_PC_SDEL ( NUM_USED_TONES, 1,                      &
     &                                    PCAL_FREQ(IND_TONE_BEG+NPCT*(I_FRQ-1)), &
     &                                    PCAL_PHAS(IND_TONE_BEG+NPCT*(I_FRQ-1)), &
     &                                    PCAL_AMPL(IND_TONE_BEG+NPCT*(I_FRQ-1)), &
     &                                    PCI_SDEL(J2), PC_SDEL(J2), PC_SDEL_ARR(1,J2), IER )
                      PIM%STA(IND_STA)%PCAL(U_FRG)%PCAL_SB_GRDEL(J9) = PC_SDEL(J2)
                 END IF
                 DO 4110 J11=1,PIM%NCHN
                    PCAL_C8(J11,I_FRQ,J2) = CMPLX ( COS(SNGL(SGN*PHAS_MOD(IP))), &
     &                                              SIN(SNGL(SGN*PHAS_MOD(IP)))  )
                    IP = IP + 1
 4110            CONTINUE 
                 IF ( PIM%CONF%DEBUG_LEVEL == 10 ) THEN
                      WRITE ( 6, 230 ) PIM%C_STA(IND_STA), IND_OBS, IND_PCAL(1:4,J2), &
     &                                 IND_POL, I_FRQ, PC_SDEL(J2)
 230                  FORMAT ( 'PIMA_USE_pcal sta: ', A, ' Ind_obs: ', I6, &
     &                         ' Ind_pcal: ', 4(I6, 1X), &
     &                         ' Ind_pol: ', I1, '  I_FRQ: ', I4, ' Gdel: ', 1PD12.5 )
!
                      DO 4120 J12=1,PIM%NCHN
                          WRITE  ( 6, 240 ) PIM%C_STA(IND_STA), IND_OBS, IND_POL, &
     &                             I_FRQ, J12, SGN, PHAS_MOD(IP-PIM%NCHN+J12-1)
 240                      FORMAT ( 'PIMA_USE_PCAL sta: ', A, ' Ind_obs: ', I6, &
     &                             ' I_POL: ', I1, &
     &                             ' I_FRQ: ', I4, ' I_CHN: ', I4, &
     &                             ' Sgn: ', F3.0,' Ph_cal: ', F8.4, ' (rad) ', F8.3, ' (deg) ' )
 4120                 CONTINUE 
                      WRITE  ( 6, '(A)' ) ' '
                 END IF
 490          CONTINUE 
              IF ( OP_CODE == PIMA__SET_PCAL ) THEN
!
! ---------------- Store phase cal group delay over a given IF
!
                   PIM%STA(IND_STA)%PCAL(U_FRG)%PCAL_MB_GRDEL     = PC_SDEL(J2)
                   PIM%STA(IND_STA)%PCAL(U_FRG)%PCAL_GRDEL_STATUS = PIMA__INIT
              END IF
              FL_PCAL_USED = .TRUE.
           ELSE IF ( FL_PCAL_TOUSE ) THEN
              WRITE ( 6, * ) 'NO_TONES = ', PIM%STA(IND_STA)%PCAL(PIM%CONF%FRQ_GRP)%NO_TONES
              CALL ERR_LOG ( 7582, IUER, 'PIMA_USE_PCAL', 'This number '// &
     &            'of phase calibration tones is not supported' )
              RETURN 
         END IF
!
         IF ( PIM%CONF%DEBUG_LEVEL == 10 ) THEN
              IF ( .NOT. PIM%STA(IND_STA)%PCAL(PIM%CONF%FRQ_GRP)%PCAL_AVAIL ) THEN
                   WRITE  ( 6, 250 ) IND_OBS, PIM%C_STA(IND_STA)
 250               FORMAT ( 'PIMA_USE_PCAL Ind_obs: ', I6, ' Sta: ', A, &
     &                      ' pcal is not available' )
                 ELSE IF ( .NOT. PIM%STA(IND_STA)%PCAL(PIM%CONF%FRQ_GRP)%PCAL_USE ) THEN
                   WRITE  ( 6, 260 ) IND_OBS, PIM%C_STA(IND_STA)
 260               FORMAT ( 'PIMA_USE_PCAL Ind_obs: ', I6, ' Sta: ', A, &
     &                      ' pcal is available, but disabled' )
                 ELSE IF ( PIM%STA(IND_STA)%PCAL(PIM%CONF%FRQ_GRP)%PCAL_AVAIL .AND. &
     &                     PIM%STA(IND_STA)%PCAL(PIM%CONF%FRQ_GRP)%PCAL_USE   .AND. &
     &                     .NOT. FL_PCAL_USED                                       ) THEN
                   IF ( FL_PCAL_USED ) THEN
                        WRITE  ( 6, 270 ) IND_OBS, PIM%C_STA(IND_STA)
 270                    FORMAT ( 'PIMA_USE_PCAL Ind_obs: ', I6, ' Sta: ', A, &
     &                           ' pcal is available, but fine-grained disabled' )
                      ELSE 
                        WRITE  ( 6, 280 ) IND_OBS, PIM%C_STA(IND_STA)
 280                    FORMAT ( 'PIMA_USE_PCAL Ind_obs: ', I6, ' Sta: ', A, &
     &                           ' Did not find pcal for this time epoch' )
                   END IF
              END IF
         END IF
         IF ( PIM%STA(IND_STA)%PCAL(PIM%CONF%FRQ_GRP)%NO_TONES > 2 ) THEN
              IF ( ALLOCATED ( PCAL_FREQ ) ) DEALLOCATE ( PCAL_FREQ )
              IF ( ALLOCATED ( PCAL_PHAS ) ) DEALLOCATE ( PCAL_PHAS )
              IF ( ALLOCATED ( PCAL_AMPL ) ) DEALLOCATE ( PCAL_AMPL )
              IF ( ALLOCATED ( PHAS_MOD  ) ) DEALLOCATE ( PHAS_MOD  )
              IF ( ALLOCATED ( AMPL_MOD  ) ) DEALLOCATE ( AMPL_MOD  )
         END IF
 420  CONTINUE 
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  PIMA_USE_PCAL  !#!#
