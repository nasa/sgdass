     SUBROUTINE PIMA_CLPC ( PIM, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  PIMA_CLPC
! *                                                                      *
! *  ### 12-JUN-2019   PIMA_CLPC   v1.0 (c)  L. Petrov  12-JUN-2019 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'pima.i'
      TYPE     ( PIMA__TYPE ) :: PIM   
      INTEGER*4  IUER
      CHARACTER  STR*128
      INTEGER*4  J0, J1, J2, J3, J4, J5, J6, J7, J8, J9, J10, J11, J12, J13, &
     &           NO_TON, NPCL, IND_MOD, ITYP, KCHN, NT_USED, &
     &           B_FRQ, E_FRQ, L_FRQ, I_FRQ, U_FRQ, U_FRG, I_FRG, IND_OBS, &
     &           KP, LO(2), LP(2), IND_POL, NC, NR, ICODE, IP, &
     &           IKS, ILS, LP_IF, ITURN, KPR(PIM__MFRQ,2), NP_MSK, LAST_TON, IER
      REAL*8,    ALLOCATABLE :: FREQ(:), TIM(:), PHAS(:,:), PHAS_AMB(:,:), AMPL(:,:)
      INTEGER*4, ALLOCATABLE :: IND_FRQ(:), IND_TON(:)
      REAL*4     PC_GDEL, PC_SDEL, PC_SDEL_ARR(PIM__MFRQ)
      REAL*8     PHS_DIF, AMPL_AVR, FREQ_1ST(PIM__MFRQ), PHAS_AVR(PIM__MFRQ,2), &
     &           FRQ_DIF, FRQ_DIF_MIN, PCAL_FRQ_STEP, PHAS_LAST, PHS_FLAT
      REAL*4,    EXTERNAL :: PIMA_PC_GDEL
      INTEGER*4, EXTERNAL :: ILEN, I_LEN
!
! --- Get frequency indexes
!
      B_FRQ = PIM%CONF%BEG_FRQ
      E_FRQ = PIM%CONF%END_FRQ
      L_FRQ = E_FRQ - B_FRQ + 1
      KCHN  = L_FRQ*PIM%NCHN
!
! --- Set polarization index
!
      IND_POL = 0
      IF ( PIM%CONF%POLAR == PIMA__POLAR_RR .OR. &
     &     PIM%CONF%POLAR == PIMA__POLAR_HH      ) THEN
           IND_POL = 1
        ELSE IF ( ( PIM%CONF%POLAR == PIMA__POLAR_LL .OR. &
     &              PIM%CONF%POLAR == PIMA__POLAR_VV     ) .AND. &
     &            PIM%NPOL == 2 ) THEN
           IND_POL = 2
        ELSE IF ( PIM%CONF%POLAR == PIMA__POLAR_LL .AND. &
     &            PIM%NPOL == 1 ) THEN
           IND_POL = 1
        ELSE 
           CALL ERR_LOG ( 7483, IUER, 'PIMA_CLPC', 'Polarization '// &
     &         'code '//TRIM(PIM%CONF%POLAR)//' is not supported for '//&
     &         'plotting pcal. Supported codes: RR, LL, HH, VV' )  
           RETURN 
      END IF
!
! --- NB: does not support multiple frequency groups as of 2019.06.21
!
      DO 410 J1=1,PIM%NSTA
         IF ( ASSOCIATED ( PIM%STA(J1)%PCAL(1)%MASK ) ) THEN
              DEALLOCATE ( PIM%STA(J1)%PCAL(1)%MASK )
         END IF
         ALLOCATE ( PIM%STA(J1)%PCAL(1)%MASK ( PIM%STA(J1)%PCAL(1)%NO_TONES, PIM%NFRQ, PIM%STA(J1)%PCAL(1)%NPOI, PIM%STA(J1)%PCAL(1)%NPOL), &
     &              STAT=IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 7462, IUER, 'PIMA_CLPC', 'Error in '// &
     &            'allocating dynamic memory for array PIM%STA(J1)%PCAL(1)%MASK' )
              RETURN
         END IF
!
! ------ Initialization
!
         PIM%STA(J1)%PCAL(1)%MASK = 1
         PIM%STA(J1)%PCAL(1)%PCAL_MASK_STATUS = PIMA__ALLOCATED
 410  CONTINUE 
!
      IF ( .NOT. ASSOCIATED ( PIM%PCAL_MASK ) ) THEN
!
! -------- Allocate memory for pcal mask
!
           ALLOCATE ( PIM%PCAL_MASK(PIM%NPCT,PIM%NFRQ,PIM%NSTA), STAT=IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 7463, IUER, 'PIMA_CLPCL', 'Error in '// &
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
                CALL ERR_LOG ( 7481, IUER, 'PIMA_CLPC', &
     &              'Failure to allocate '//STR(1:I_LEN(STR))//' bytes '//&
     &              'of dynamic memory for the list of observations' )
                RETURN
           END IF
!
           DO 420 J2=1,PIM%CONF%FRIB_NOBS
              PIM%CONF%FRIB_OBS(J2) = J2
 420       CONTINUE
      END IF
!
! --- Start the cycle over stations
!
      DO 430 J3=1,PIM%NSTA
!
! ------ Free previosly allocated memory
!
         IF ( ALLOCATED ( AMPL     ) ) DEALLOCATE ( AMPL )
         IF ( ALLOCATED ( PHAS     ) ) DEALLOCATE ( PHAS )
         IF ( ALLOCATED ( PHAS_AMB ) ) DEALLOCATE ( PHAS_AMB )
         IF ( ALLOCATED ( FREQ     ) ) DEALLOCATE ( FREQ     )
         IF ( ALLOCATED ( TIM      ) ) DEALLOCATE ( TIM      )
         IF ( ALLOCATED ( IND_FRQ  ) ) DEALLOCATE ( IND_FRQ  )
         IF ( ALLOCATED ( IND_TON  ) ) DEALLOCATE ( IND_TON  )
!
! ------ NB: this logic does not support merged and combiend frequency groups!!
!
         IF ( PIM%STA(J3)%PCAL(1)%PCAL_AVAIL .AND. &
     &        PIM%STA(J3)%PCAL(1)%PCAL_USE   .AND. &
     &        PIM%STA(J3)%PCAL(1)%NPOI .GE. 3      ) THEN
!
              NPCL = PIM%STA(J3)%PCAL(1)%NO_TONES * PIM%NFRQ
!
! ----------- Memory allocation
!
              ALLOCATE ( AMPL(NPCL,PIM%STA(J3)%PCAL(1)%NPOI) )
              ALLOCATE ( PHAS(NPCL,PIM%STA(J3)%PCAL(1)%NPOI) )
              ALLOCATE ( PHAS_AMB(NPCL,PIM%STA(J3)%PCAL(1)%NPOI) )
              ALLOCATE ( FREQ(NPCL) )
              ALLOCATE ( TIM(PIM__MOBS) )
              ALLOCATE ( IND_FRQ(NPCL)    )
              ALLOCATE ( IND_TON(NPCL)    )
!
              FREQ     = 0.0
              IND_FRQ  = 0
              IND_TON  = 0
              PHAS     = 0.0
              PHAS_AMB = 0.0
              AMPL     = 0.0
!
! ----------- Then over epochs of phase-cal
!
              DO 440 J4=1,PIM%STA(J3)%PCAL(1)%NPOI
                 TIM(J4) = PIM%STA(J3)%PCAL(1)%TIME_MID_R8(J4) ! Time in seconds wrt the session nominal start
                 I_FRQ = 0
                 KP = 0
                 DO 450 J5=B_FRQ,E_FRQ ! Cycle over used IFs
!
! ----------------- Get frequency group indices for three cases of frequency group uses
!
                    IF ( PIM%FRG_USE == PIMA__SINGLE ) THEN
                         U_FRG = PIM%CONF%FRQ_GRP
                         I_FRG = PIM%CONF%FRQ_GRP
                         U_FRQ = J5
                       ELSE IF ( PIM%FRG_USE == PIMA__MERGE ) THEN
                         U_FRG = PIM%REV_FRG(J5)
                         I_FRG = 1
                         U_FRQ = PIM%REV_FRQ(J5)
                       ELSE IF ( PIM%FRG_USE == PIMA__COMBINE ) THEN
                         U_FRG = PIM%REV_FRG(J5)
                         I_FRG = PIM%REV_FRG(J5)
                         U_FRQ = PIM%REV_FRQ(J5)
                    END IF
                    I_FRQ = I_FRQ + 1
!
! ----------------- The lowest frequency of the phase cal tone in the given IF
!
                    FREQ_1ST(I_FRQ) = PIM%STA(J3)%PCAL(U_FRG)%FREQ(1,U_FRQ,J4) 
                    DO 460 J6=1,PIM%STA(J3)%PCAL(U_FRG)%NO_TONES ! Cycle over phase cal tones within the given IF
                       IF ( PIM%STA(J3)%PCAL(U_FRG)%FREQ(J6,U_FRQ,J4) > PIMA__MIN_FRQ ) THEN
!
! ------------------------- NB: There are cases when Pcal frequency is zero. 
! -------------------------      We bypass such tones
!
                            KP = KP + 1
                            FREQ(KP) = PIM%STA(J3)%PCAL(U_FRG)%FREQ(J6,U_FRQ,J4) 
                            PHAS(KP,J4) = PIM%STA(J3)%PCAL(U_FRG)%PHAS(J6,U_FRQ,J4,IND_POL)
                            AMPL(KP,J4) = PIM%STA(J3)%PCAL(U_FRG)%AMPL(J6,U_FRQ,J4,IND_POL)
                            IND_FRQ(KP) = I_FRQ
                            IND_TON(KP) = J6
                       END IF
 460                CONTINUE 
 450             CONTINUE 
!
! -------------- Compute group delay of the phase calibration over the band
!
                 CALL ERR_PASS ( IUER, IER )
                 PC_GDEL = PIMA_PC_GDEL ( KP, FREQ, PHAS(1,J4), AMPL(1,J4), IER )
                 IF ( IER .NE. 0 ) THEN
                      CALL ERR_LOG ( 7486, IUER, 'PIMA_CLPC', &
             &            'Trap of internal control in computing phase calibration '// &
             &            'group delay' )
                      RETURN
                 END IF
!
! -------------- Compute group delay of phase cal over each individual IF separately.
!
                 CALL ERR_PASS ( IUER, IER )
                 CALL PIMA_PC_SDEL ( PIM%STA(J3)%PCAL(U_FRG)%NO_TONES, 1, &
     &                               FREQ, PHAS(1,J4), AMPL(1,J4), &
     &                               PIM%STA(J3)%PCAL(U_FRG)%PCAL_SB_GRDEL(I_FRQ), &
     &                               PC_SDEL, PC_SDEL_ARR, IER )
                 IF ( PIM%CONF%DEBUG_LEVEL .GE. 6 ) THEN
                      WRITE ( 6, 210 ) PIM%C_STA(J3), J4, TIM(J3), PC_GDEL, PC_SDEL
  210                 FORMAT ( 'PIMA_CLPC sta: ', A, ' Epc: ', I5, ' Tim: ', F8.2, &
     &                         '  PC_Gdel= ', 1PD12.5, ' PC_Sdel= ', 1PD12.5 )
                 END IF
!
! -------------- Apply pcal group delay and compute average pcal amplitude
!
                 LAST_TON  = PIM__MFRQ + 1 ! index of the last tone
                 PHAS_LAST = 0.0D0 ! Phase of the previous tone 
                 AMPL_AVR = 0.0
                 NT_USED = 0
                 DO 470 J7=1,KP
!
! ----------------- Apply group delay in phase cal
!
                    PHAS_AMB(J7,J4) = PHAS(J7,J4) - PI2*(FREQ(J7) - FREQ_1ST(I_FRQ))*PC_SDEL_ARR(IND_FRQ(J7))
!
! ----------------- Resolve phase ambiguity
!
                    PHAS_AMB(J7,J4) = PHAS_AMB(J7,J4) - PI2*IDNINT(PHAS_AMB(J7,J4)/PI2)
                    IF ( AMPL(J7,J4) > 0.0 ) THEN
!
! -------------------- Now we try to resolve remaining ambiguity (pcal is not masked out)
!
                       IF ( IND_TON(J7) > LAST_TON ) THEN
!
! ------------------------- This tone has index older than the first index with data
!
                            ITURN = IDNINT ( (PHAS_AMB(J7,J4) - PHAS_LAST)/PI2 )
                            PHAS_AMB(J7,J4) = PHAS_AMB(J7,J4) - PI2*ITURN
                       END IF
                       LAST_TON = IND_TON(J7)
                       PHAS_LAST = PHAS_AMB(J7,J4) 
                       NT_USED = NT_USED + 1
                     ELSE 
                       PHAS_AMB(J7,J4) = 0.0
                  END IF
                  AMPL_AVR = AMPL_AVR + AMPL(J7,J4) 
 470           CONTINUE 
               IF ( NT_USED > 0 ) THEN
                    AMPL_AVR = AMPL_AVR/NT_USED
                  ELSE 
                    AMPL_AVR = 0.0
               END IF
 440        CONTINUE 
!
            CALL ERR_PASS ( IUER, IER )
            CALL PIMA_CLPC_ARR ( PIM%STA(J3)%PCAL(1)%NO_TONES, PIM%NFRQ, NPCL, PIM%STA(J3)%PCAL(1)%NPOI, KP, &
     &                           PIM%C_STA(J3), FREQ, TIM, IND_FRQ, IND_TON, AMPL, PHAS_AMB, &
     &                           PIM%STA(J3)%PCAL(1)%MASK(1,1,1,IND_POL), PIM%CONF%DEBUG_LEVEL, IER )
            IF ( IER .NE. 0 ) THEN
                 CALL ERR_LOG ( 7487, IUER, 'PIMA_CLPC', 'Error in cleaning ', &
     &               'pcal for station '//PIM%C_STA(J3) )
                 RETURN
            END IF
         END IF
 430  CONTINUE 
!
! --- Deallocate memory
!
      DEALLOCATE ( IND_TON  )
      DEALLOCATE ( IND_FRQ  )
      DEALLOCATE ( TIM  )
      DEALLOCATE ( AMPL )
      DEALLOCATE ( PHAS )
      DEALLOCATE ( PHAS_AMB )
      DEALLOCATE ( FREQ )
      IF ( ASSOCIATED ( PIM%CONF%FRIB_OBS ) ) DEALLOCATE ( PIM%CONF%FRIB_OBS )
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  PIMA_CLPC  !#!  
