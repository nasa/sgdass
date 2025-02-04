      SUBROUTINE PIMA_PCAL_CLEAN ( PIM, IND_STA, TIM, PHAS_AMB, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine PIMA_PCAL_CLEAN 
! *                                                                      *
! *   Remove group delay and group delay amibiguity, no phase cal mask   *
! *  ### 8-JUL-2022  PIMA_PCAL_CLEAN   v1.0 (c)        8-JUL-2022 ###    *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'astro_constants.i'
      INCLUDE   'pima.i'
      INCLUDE   'diagi.i'
      TYPE     ( PIMA__TYPE  ) :: PIM
      INTEGER*4  IUER, IER
      INTEGER*4  MPB, MP, NN, MDEG
      PARAMETER  ( MPB    =   7 )
      PARAMETER  ( MP     =  128*1024 )
      PARAMETER  ( MDEG   =   3 )
      REAL*4     PC_SDEL, PC_SDEL_ARR(PIM__MFRQ), PC_GDEL_ARR(PIM__MFRQ)
      REAL*8,    ALLOCATABLE :: FREQ(:), FREQ_SDEL(:,:), AMPL_SDEL(:,:), &
     &           PHAS_SDEL(:,:), PHAS(:,:), AMPL(:,:)
      CHARACTER  PREF_NAME*128, STR*32, STR1*32, &
     &           MES_SELSTA*80, STA_STR*8
      INTEGER*4  MODE, IND_STA, ISTA_LAST, ICODE, IFRQ, NC, NR, NFRQ, &
     &           I1, I2, I3, I4, I5, NP, IND_POL, IND_PLT, &
     &           M_TONES, IND_TONE, IND_FREQ, N_TONES, I_TONE, IND(2,32), I_ABOVE, &
     &           NO_TON, NPCL, IND_MOD, ITYP, KCHN, NT_USED, &
     &           B_FRQ, E_FRQ, L_FRQ, I_FRQ, U_FRQ, U_FRG, I_FRG, IND_OBS, &
     &           KP, LAST_TON, ITURN
      INTEGER*4, ALLOCATABLE :: IND_FRQ(:), IND_TON(:)
      INTEGER*4, EXTERNAL :: ILEN, I_LEN, LTM_DIF
      REAL*4,    EXTERNAL :: PIMA_PC_GDEL
      REAL*8     TIM(PIM__MOBS), PHAS_AMB(PIM%STA(IND_STA)%PCAL(1)%NO_TONES * PIM%NFRQ, &
     &                                     PIM%STA(IND_STA)%PCAL(1)%NPOI), &
     &           FREQ_1ST(PIM__MFRQ), PHAS_LAST
!
      CALL CHIN ( PIM%CONF%PHAS_CAL_CODE, IND_TONE ) ! Set IND_TONE
!
      IND_FREQ = PIM%CONF%BEG_FRQ

      IF ( ( PIM%CONF%POLAR == PIMA__POLAR_RR  .OR. &
     &        PIM%CONF%POLAR == PIMA__POLAR_HH       ) .OR. &
     &      PIM%CONF%ACT_CODE == PIMA__PDPL_CODE            ) THEN 
            IND_POL = 1
          ELSE IF ( ( PIM%CONF%POLAR == PIMA__POLAR_LL .OR. &
     &                PIM%CONF%POLAR == PIMA__POLAR_VV      ) .AND. &
     &              PIM%NPOL == 2 ) THEN
            IND_POL = 2
          ELSE IF ( PIM%CONF%POLAR == PIMA__POLAR_LL .AND. &
     &                PIM%NPOL == 1 ) THEN
            IND_POL = 1
          ELSE 
            CALL ERR_LOG ( 8857, IUER, 'PIMA_PCAL_CLEAN', 'Polarization code '// &
     &           TRIM(PIM%CONF%POLAR)//' is not supported for plotting pcal. '//&
     &          'Supported codes: RR, LL, HH, VV' )  
            RETURN 
      END IF
!
! ------ NB: this logic does not support merged and combined frequency groups!!
!
      IF ( PIM%STA(IND_STA)%PCAL(1)%PCAL_AVAIL .AND. &
  &        PIM%STA(IND_STA)%PCAL(1)%PCAL_USE   .AND. &
  &        PIM%STA(IND_STA)%PCAL(1)%NPOI .GE. 3      ) THEN
!
           NPCL = PIM%STA(IND_STA)%PCAL(1)%NO_TONES * PIM%NFRQ
!
! ----------- Memory allocation
!
           ALLOCATE ( AMPL(NPCL,PIM%STA(IND_STA)%PCAL(1)%NPOI), STAT=IER )
           ALLOCATE ( AMPL_SDEL(PIM%STA(IND_STA)%PCAL(1)%NO_TONES, PIM%NFRQ), STAT=IER )
           ALLOCATE ( PHAS(NPCL,PIM%STA(IND_STA)%PCAL(1)%NPOI), STAT=IER )
           ALLOCATE ( PHAS_SDEL(PIM%STA(IND_STA)%PCAL(1)%NO_TONES, PIM%NFRQ), STAT=IER )
           ALLOCATE ( FREQ(NPCL), STAT=IER )
           ALLOCATE ( FREQ_SDEL(PIM%STA(IND_STA)%PCAL(1)%NO_TONES, PIM%NFRQ), STAT=IER )
           ALLOCATE ( IND_FRQ(NPCL), STAT=IER )
           ALLOCATE ( IND_TON(NPCL), STAT=IER )
           
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 8858, IUER, 'PIMA_PCAL_CLEAN', &
               &               'Failure to allocate arrays for phase cal cleaning')
                RETURN
           ENDIF
!
           FREQ     = 0.0
           IND_FRQ  = 0
           IND_TON  = 0
           PHAS     = 0.0
           PHAS_AMB = 0.0
           AMPL     = 0.0
           PC_GDEL_ARR = 0.0
           PC_SDEL_ARR = 0.0
!
! ----------- Then over epochs of phase-cal
!
           DO 310 I1=1,PIM%STA(IND_STA)%PCAL(1)%NPOI
              TIM(I1) = PIM%STA(IND_STA)%PCAL(1)%TIME_MID_R8(I1) 
              I_FRQ = 0
              KP = 0
              DO 320 I2=PIM%CONF%BEG_FRQ,PIM%CONF%END_FRQ ! Cycle over used IFs
!
! ----------------- Get frequency group indices for three cases of frequency group uses
!
                 U_FRG = PIM%CONF%FRQ_GRP
                 N_TONES = PIM%STA(IND_STA)%PCAL(U_FRG)%NO_TONES 
                 I_FRG = PIM%CONF%FRQ_GRP
                 U_FRQ = I2
                 I_FRQ = I_FRQ + 1
!
! ----------------- The lowest frequency of the phase cal tone in the given IF
!
                 FREQ_1ST(I_FRQ) = PIM%STA(IND_STA)%PCAL(U_FRG)%FREQ(1,U_FRQ,I1) 
                 DO 330 I3=1,PIM%STA(IND_STA)%PCAL(U_FRG)%NO_TONES 
                    KP = KP + 1
                    FREQ(KP) = PIM%STA(IND_STA)%PCAL(U_FRG)%FREQ(I3,U_FRQ,I1) 
                    FREQ_SDEL(I3,I_FRQ) = PIM%STA(IND_STA)%PCAL(U_FRG)%FREQ(I3,U_FRQ,I1) 
                    PHAS(KP,I1) = PIM%STA(IND_STA)%PCAL(U_FRG)%PHAS(I3,U_FRQ,I1,IND_POL)
                    PHAS_SDEL(I3,I_FRQ) =  PIM%STA(IND_STA)%PCAL(U_FRG)%PHAS(I3,U_FRQ,I1,IND_POL)
                    AMPL(KP,I1) = PIM%STA(IND_STA)%PCAL(U_FRG)%AMPL(I3,U_FRQ,I1,IND_POL)
                    AMPL_SDEL(I3,I_FRQ) = PIM%STA(IND_STA)%PCAL(U_FRG)%AMPL(I3,U_FRQ,I1,IND_POL)
                    IND_FRQ(KP) = I_FRQ
                    IND_TON(KP) = I3
330              CONTINUE 
                 CALL ERR_PASS ( IUER, IER )
                 PC_GDEL_ARR(I_FRQ) = PIMA_PC_GDEL ( PIM%STA(IND_STA)%PCAL(U_FRG)%NO_TONES, &
                &                                    FREQ_SDEL(1,I_FRQ), PHAS_SDEL(1,I_FRQ), &
                &                                    AMPL_SDEL(1,I_FRQ), IER )
                 IF  ( IER .NE. 0 ) THEN
                      CALL ERR_LOG ( 8859, IUER, 'PIMA_PCAL_CLEAN', 'Failure in computing group delay')
                      RETURN
                 ENDIF 
320           CONTINUE 
!
! -------------- Compute group delay of phase cal over each individual IF
!
              CALL ERR_PASS ( IUER, IER )
              CALL PIMA_PC_SDEL ( PIM%STA(IND_STA)%PCAL(U_FRG)%NO_TONES, I_FRQ, &
  &                               FREQ_SDEL, PHAS_SDEL, AMPL_SDEL, &
  &                               PC_GDEL_ARR, &
  &                               PC_SDEL, PC_SDEL_ARR, IER )
              IF  ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 8860, IUER, 'PIMA_PCAL_CLEAN', 'Failure in computing single-band'// &
                  &               'group delays' )
                   RETURN
              ENDIF     
              IF ( PIM%CONF%DEBUG_LEVEL .GE. 6 ) THEN
                   WRITE ( 6, 210 ) PIM%C_STA(IND_STA), I1, TIM(IND_STA), PC_GDEL_ARR(I_FRQ), PC_SDEL
210                FORMAT ( 'PIMA_PCAL_CLEAN sta: ', A, ' Epc: ', I5, ' Tim: ', F8.2, &
  &                         '  PC_Gdel= ', 1PD12.5, ' PC_Sdel= ', 1PD12.5 )
              END IF
!
! -------------- Apply pcal group delay
!
              LAST_TON  = PIM__MFRQ + 1 ! index of the last tone
              PHAS_LAST = 0.0D0 ! Phase of the previous tone 
              DO 340 I4=1,KP
!
! ----------------- Apply group delay in phase cal
!
                 PHAS_AMB(I4,I1) = PHAS(I4,I1) - PI2*(FREQ(I4) - FREQ_1ST(I_FRQ))*PC_SDEL_ARR(IND_FRQ(I4))
!
! ----------------- Resolve phase ambiguity
!
                 PHAS_AMB(I4,I1) = PHAS_AMB(I4,I1) - PI2*IDNINT(PHAS_AMB(I4,I1)/PI2)
                 IF ( AMPL(I4,I1) > 0.0 ) THEN
!
! ---------------------- Now we try to resolve remaining ambiguity (pcal is not masked out)
!
                      IF ( IND_TON(I4) > LAST_TON ) THEN
!
! ------------------------- This tone has index older than the first index with data
!
                           ITURN = IDNINT ( (PHAS_AMB(I4,I1) - PHAS_LAST)/PI2 )
                           PHAS_AMB(I4,I1) = PHAS_AMB(I4,I1) - PI2*ITURN
                      END IF
                      LAST_TON = IND_TON(I4)
                      PHAS_LAST = PHAS_AMB(I4,I1) 
                   ELSE 
                      PHAS_AMB(I4,I1) = 0.0
                 END IF
!
340           CONTINUE 
310         CONTINUE
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  PIMA_PCAL_CLEAN !#!
!
! ------------------------------------------------------------------------
!
      SUBROUTINE PIMA_PCAL_CLEAN8 ( PIM, IND_STA, NUM_ACCUM, NPOI_ACCUM, &
&                                     T8, P8, A8, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine PIMA_PCAL_CLEAN8 
! *                                                                      *
! *   Remove group delay and group delay amibiguity, no phase cal mask   *
! *   
! *  ### 8-JUL-2022  PIMA_PCAL_CLEAN   v1.0 (c)        8-JUL-2022 ###    *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'astro_constants.i'
      INCLUDE   'pima.i'
      INCLUDE   'diagi.i'
      TYPE     ( PIMA__TYPE  ) :: PIM
      INTEGER*4  IUER, IER
      INTEGER*4  MPB, MP, NN, MDEG
      PARAMETER  ( MPB    =   7 )
      PARAMETER  ( MP     =  128*1024 )
      PARAMETER  ( MDEG   =   3 )
      REAL*4     PC_SDEL, PC_SDEL_ARR(PIM__MFRQ), PC_GDEL_ARR(PIM__MFRQ)
      REAL*8,    ALLOCATABLE :: FREQ(:), FREQ_SDEL(:,:), AMPL_SDEL(:,:), &
     &           PHAS_SDEL(:,:), PHAS(:,:), AMPL(:,:)
      CHARACTER  PREF_NAME*128, STR*32, STR1*32, &
     &           MES_SELSTA*80, STA_STR*8
      INTEGER*4  MODE, IND_STA, ISTA_LAST, ICODE, IFRQ, NC, NR, NFRQ, &
     &           J1, J2, J3, I1, I2, I3, I4, I5, NP, IND_POL, IND_PLT, &
     &           M_TONES, IND_TONE, IND_FREQ, N_TONES, I_TONE, IND(2,32), I_ABOVE, &
     &           NO_TON, NPCL, IND_MOD, ITYP, KCHN, NT_USED, &
     &           B_FRQ, E_FRQ, L_FRQ, I_FRQ, U_FRQ, U_FRG, I_FRG, IND_OBS, &
     &           KP, LAST_TON, ITURN, NPOI_ACCUM, NUM_ACCUM, NPOI
      INTEGER*4, ALLOCATABLE :: IND_FRQ(:), IND_TON(:)
      INTEGER*4, EXTERNAL :: ILEN, I_LEN, LTM_DIF
      REAL*4,    EXTERNAL :: PIMA_PC_GDEL
      REAL*8     TIM(PIM__MOBS), PHAS_AMB(PIM%STA(IND_STA)%PCAL(1)%NO_TONES * PIM%NFRQ, &
     &                                     PIM%STA(IND_STA)%PCAL(1)%NPOI), &
     &           FREQ_1ST(PIM__MFRQ), PHAS_LAST, AMP_MIN, &
     &           T8(PIM%NPCT,MP,PIM%NFRQ), P8(PIM%NPCT,MP,PIM%NFRQ), A8(PIM%NPCT,MP,PIM%NFRQ)

      CALL CHIN ( PIM%CONF%PHAS_CAL_CODE, IND_TONE ) ! Set IND_TONE
!
      IND_FREQ = PIM%CONF%BEG_FRQ

      IF ( ( PIM%CONF%POLAR == PIMA__POLAR_RR  .OR. &
     &        PIM%CONF%POLAR == PIMA__POLAR_HH       ) .OR. &
     &      PIM%CONF%ACT_CODE == PIMA__PDPL_CODE            ) THEN 
            IND_POL = 1
          ELSE IF ( ( PIM%CONF%POLAR == PIMA__POLAR_LL .OR. &
     &                PIM%CONF%POLAR == PIMA__POLAR_VV      ) .AND. &
     &              PIM%NPOL == 2 ) THEN
            IND_POL = 2
          ELSE IF ( PIM%CONF%POLAR == PIMA__POLAR_LL .AND. &
     &                PIM%NPOL == 1 ) THEN
            IND_POL = 1
          ELSE 
            CALL ERR_LOG ( 8862, IUER, 'PIMA_PCAL_CLEAN8', 'Polarization code '// &
     &           TRIM(PIM%CONF%POLAR)//' is not supported for plotting pcal. '//&
     &          'Supported codes: RR, LL, HH, VV' )  
            RETURN 
      END IF
!
! ------ NB: this logic does not support merged and combined frequency groups!!
!
   
      IF ( PIM%STA(IND_STA)%PCAL(1)%PCAL_AVAIL .AND. &
  &        PIM%STA(IND_STA)%PCAL(1)%PCAL_USE   .AND. &
  &        PIM%STA(IND_STA)%PCAL(1)%NPOI .GE. 3      ) THEN
!
           NPCL = PIM%STA(IND_STA)%PCAL(1)%NO_TONES * PIM%NFRQ
!
! ----------- Memory allocation
!
           ALLOCATE ( AMPL(NPCL,PIM%STA(IND_STA)%PCAL(1)%NPOI), STAT=IER )
           ALLOCATE ( AMPL_SDEL(PIM%STA(IND_STA)%PCAL(1)%NO_TONES, PIM%NFRQ), STAT=IER )
           ALLOCATE ( PHAS(NPCL,PIM%STA(IND_STA)%PCAL(1)%NPOI), STAT=IER )
           ALLOCATE ( PHAS_SDEL(PIM%STA(IND_STA)%PCAL(1)%NO_TONES, PIM%NFRQ), STAT=IER )
           ALLOCATE ( FREQ(NPCL), STAT=IER )
           ALLOCATE ( FREQ_SDEL(PIM%STA(IND_STA)%PCAL(1)%NO_TONES, PIM%NFRQ), STAT=IER )
           ALLOCATE ( IND_FRQ(NPCL), STAT=IER )
           ALLOCATE ( IND_TON(NPCL), STAT=IER )
           IF  ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 8863, IUER, 'PIMA_PCAL_CLEAN8', &
               &               'Failure to allocate arrays for phase cal cleaning' )
                RETURN
           ENDIF 
!
           FREQ     = 0.0
           IND_FRQ  = 0
           IND_TON  = 0
           PHAS     = 0.0
           PHAS_AMB = 0.0
           AMPL     = 0.0
           PC_GDEL_ARR = 0.0
           PC_SDEL_ARR = 0.0
!
! ----------- Then over epochs of phase-cal
!
           DO 310 I1=1,NPOI_ACCUM
              TIM(I1) = T8(1,I1,1) ! Time in seconds wrt the session nominal start
              I_FRQ = 0
              KP = 0
              DO 320 I2=PIM%CONF%BEG_FRQ,PIM%CONF%END_FRQ 
!
! ----------------- Get frequency group indices for three cases of frequency group uses
!
                 U_FRG = PIM%CONF%FRQ_GRP
                 N_TONES = PIM%STA(IND_STA)%PCAL(U_FRG)%NO_TONES 
                 I_FRG = PIM%CONF%FRQ_GRP
                 U_FRQ = I2
                 I_FRQ = I_FRQ + 1
!
! ----------------- The lowest frequency of the phase cal tone in the given IF
!
                 FREQ_1ST(I_FRQ) = PIM%STA(IND_STA)%PCAL(U_FRG)%FREQ(1,U_FRQ,I1) 
                 DO 330 I3=1,PIM%STA(IND_STA)%PCAL(U_FRG)%NO_TONES 
                    KP = KP + 1
                    FREQ(KP) = PIM%STA(IND_STA)%PCAL(U_FRG)%FREQ(I3,U_FRQ,I1) 
                    FREQ_SDEL(I3,I_FRQ) = PIM%STA(IND_STA)%PCAL(U_FRG)%FREQ(I3,U_FRQ,I1) 
                    PHAS(KP,I1) = P8(I3,I1,I_FRQ)
                    PHAS_SDEL(I3,I_FRQ) = P8(I3,I1,I_FRQ)
                    AMPL(KP,I1) = A8(I3,I1,I_FRQ)
                    AMPL_SDEL(I3,I_FRQ) = A8(I3,I1,I_FRQ)
                    IND_FRQ(KP) = I_FRQ
                    IND_TON(KP) = I3
330              CONTINUE 
                 CALL ERR_PASS ( IUER, IER )
                 PC_GDEL_ARR(I_FRQ) = PIMA_PC_GDEL ( PIM%STA(IND_STA)%PCAL(U_FRG)%NO_TONES, &
     &                                    FREQ_SDEL(1,I_FRQ), PHAS_SDEL(1,I_FRQ), &
     &                                    AMPL_SDEL(1,I_FRQ), IER )
                 IF  ( IER .NE. 0 ) THEN
                      CALL ERR_LOG ( 8864, IUER, 'PIMA_PCAL_CLEAN8', 'Failure in computing group delay')
                      RETURN
                 ENDIF 
320             CONTINUE 
!
! -------------- Compute group delay of phase cal over each individual IF
!
              CALL ERR_PASS ( IUER, IER )
              CALL PIMA_PC_SDEL ( PIM%STA(IND_STA)%PCAL(U_FRG)%NO_TONES, I_FRQ, &
  &                               FREQ_SDEL, PHAS_SDEL, AMPL_SDEL, &
  &                               PC_GDEL_ARR, &
  &                               PC_SDEL, PC_SDEL_ARR, IER )
              IF  ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 8865, IUER, 'PIMA_PCAL_CLEAN8', 'Failure in computing '// &
                  &               'single-band group delays')
                   RETURN
              ENDIF    
              IF ( PIM%CONF%DEBUG_LEVEL .GE. 8 ) THEN
                   WRITE ( 6, 210 ) PIM%C_STA(IND_STA), I1, TIM(IND_STA), PC_GDEL_ARR(I_FRQ), PC_SDEL
210                FORMAT ( 'PIMA_PCAL_CLEAN8 sta: ', A, ' Epc: ', I5, ' Tim: ', F8.2, &
  &                         '  PC_Gdel= ', 1PD12.5, ' PC_Sdel= ', 1PD12.5 )
              END IF
!
! -------------- Apply pcal group delay
!
              LAST_TON  = PIM__MFRQ + 1 ! index of the last tone
              PHAS_LAST = 0.0D0 ! Phase of the previous tone 
              DO 340 I4=1,KP
!
! ----------------- Apply group delay in phase cal
!
                 PHAS_AMB(I4,I1) = PHAS(I4,I1) - PI2*(FREQ(I4) - FREQ_1ST(I_FRQ))*PC_SDEL_ARR(IND_FRQ(I4))
!
! ----------------- Resolve phase ambiguity
!
                 PHAS_AMB(I4,I1) = PHAS_AMB(I4,I1) - PI2*IDNINT(PHAS_AMB(I4,I1)/PI2)
                 IF ( AMPL(I4,I1) > 0.0 ) THEN
!
! ---------------------- Now we try to resolve remaining ambiguity (pcal is not masked out)
!
                      IF ( IND_TON(I4) > LAST_TON ) THEN
!
! ------------------------- This tone has index older than the first index with data
!
                           ITURN = IDNINT ( (PHAS_AMB(I4,I1) - PHAS_LAST)/PI2 )
                           PHAS_AMB(I4,I1) = PHAS_AMB(I4,I1) - PI2*ITURN
                      END IF
                      LAST_TON = IND_TON(I4)
                      PHAS_LAST = PHAS_AMB(I4,I1) 
                   ELSE 
                      PHAS_AMB(I4,I1) = 0.0
                 END IF
!
340           CONTINUE 
310         CONTINUE
      END IF
!
!----Put phas_amb back in P8
!
      I_FRQ = 0
      KP = 0
      DO 410 J1 = PIM%CONF%BEG_FRQ,PIM%CONF%END_FRQ
         I_FRQ = I_FRQ + 1
         DO 420 J2 = 1,NPOI_ACCUM
            DO 430 J3 = 1,N_TONES
               KP = (I_FRQ - 1)*N_TONES + J3
               P8(J3,J2,I_FRQ) = PHAS_AMB(KP,J2)
 430        CONTINUE
 420     CONTINUE
 410  CONTINUE
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  PIMA_PCAL_CLEAN8  !#!  
!
! ------------------------------------------------------------------------
!
      SUBROUTINE PIMA_PCAL_AVG ( PIM, IND_STA, AMP_MIN, NUM_ACCUM, &
&                                  NPOI_ACCUM, FREQS, IND_TONE_FREQ, T8, P8, A8, Y8, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine PIMA_PCAL_AVG                                              *
! *   Average phase/amplitude/time data to desired interval              *
! *  ### 8-JUL-2022  PIMA_PCAL_AVG   v1.0 (c)        8-JUL-2022 ###      *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'astro_constants.i'
      INCLUDE   'pima.i'
      INCLUDE   'diagi.i'
      TYPE     ( PIMA__TYPE  ) :: PIM
      INTEGER*4  IUER
      INTEGER*4  MPB, MP, NN, MDEG, IND_STA
      REAL*8     PHASE_MAX
      PARAMETER  ( MPB    =   7 )
      PARAMETER  ( MP     =  128*1024 )
      PARAMETER  ( MDEG   =   3 )
      PARAMETER  ( PHASE_MAX = 250 ) 
      REAL*8     FREQS(PIM__MTON,PIM__MFRQ), &
     &           AMP_MIN, T8(PIM%NPCT,MP,PIM%NFRQ), &
     &           P8(PIM%NPCT,MP,PIM%NFRQ), A8(PIM%NPCT,MP,PIM%NFRQ), &
     &           Y8(PIM%NPCT,MP,PIM%NFRQ), PHS_DIF, T_ABOVE(NPOI_ACCUM), Y_ABOVE(NPOI_ACCUM), &
     &           Y_BACKUP(NPOI_ACCUM), T_POS(NPOI_ACCUM), PHASE_POS(NPOI_ACCUM)
      CHARACTER  PREF_NAME*128, STR*32, STR1*32, &
     &           MES_SELSTA*80, STA_STR*8
      INTEGER*4  MODE, ISTA_LAST, ICODE, IFRQ, NC, NR, NFRQ, &
     &           J1, J2, J3, J4, J5, J6, J7, J8, J9, K1, K2, NP, IND_POL,  &
     &           M_TONES, IND_TONE, IND_FRQ, N_TONES, I_TONE, IND(2,32), I_ABOVE, &
     &           I_NONZERO, LIND, IER, NPT, I_POS, REF_FREQ, &
     &           NPOI_ACCUM, NUM_ACCUM, NPOI, IND_TONE_FREQ(PIM__MTON)
      LOGICAL*4  INDS_ABOVE(NPOI_ACCUM), POS_INDS(NUM_ACCUM)
      INTEGER*4, EXTERNAL :: ILEN, I_LEN, LTM_DIF
                 
      CALL CHIN ( PIM%CONF%PHAS_CAL_CODE, IND_TONE ) ! Set IND_TONE
!
      IF ( ( PIM%CONF%POLAR == PIMA__POLAR_RR  .OR. &
     &        PIM%CONF%POLAR == PIMA__POLAR_HH       ) .OR. &
     &      PIM%CONF%ACT_CODE == PIMA__PDPL_CODE            ) THEN 
            IND_POL = 1
          ELSE IF ( ( PIM%CONF%POLAR == PIMA__POLAR_LL .OR. &
     &                PIM%CONF%POLAR == PIMA__POLAR_VV      ) .AND. &
     &              PIM%NPOL == 2 ) THEN
            IND_POL = 2
          ELSE IF ( PIM%CONF%POLAR == PIMA__POLAR_LL .AND. &
     &                PIM%NPOL == 1 ) THEN
            IND_POL = 1
          ELSE 
            CALL ERR_LOG ( 8866, IUER, 'PIMA_PCAL_AVG', 'Polarization code '// &
     &           TRIM(PIM%CONF%POLAR)//' is not supported for plotting pcal. '//&
     &          'Supported codes: RR, LL, HH, VV' )  
            RETURN 
      END IF
      NPOI =  PIM%STA(IND_STA)%PCAL(1)%NPOI
      M_TONES = PIM%STA(IND_STA)%PCAL(PIM%CONF%FRQ_GRP)%NO_TONES
      Y8 = 0.0D0
      IF ( PIM%STA(IND_STA)%PCAL(PIM%CONF%FRQ_GRP)%PCAL_AVAIL .AND. &
  &        PIM%STA(IND_STA)%PCAL(PIM%CONF%FRQ_GRP)%PCAL_USE         ) THEN
           IFRQ = 0
           DO 410 IND_FRQ=PIM%CONF%BEG_FRQ,PIM%CONF%END_FRQ
              IFRQ = IFRQ + 1
              NP = 0
! --- --------Loop over epochs
              DO 420 J2=1,NPOI_ACCUM
                NP = NP + 1
!---------------Loop over tones within frequency group
                DO 430 J3=1,M_TONES
                   I_TONE = J3
                   REF_FREQ = IND_TONE_FREQ(J3)
                   IF ( REF_FREQ == 0 ) THEN
                      REF_FREQ = 1
                   END IF
                   IF ( J2 == 1 ) THEN
                        FREQS(I_TONE,IFRQ) = PIM%STA(IND_STA)%PCAL(PIM%CONF%FRQ_GRP)%FREQ(J3,IND_FRQ,J2)
                   END IF
!------------------Fill up T8, P8, A8 arrays, average to given interval
                   IF ( NUM_ACCUM .EQ. 0 ) THEN ! No averaging
                        T8(I_TONE,NP,IFRQ) = PIM%STA(IND_STA)%PCAL(PIM%CONF%FRQ_GRP)%TIME_MID_R8(J2)
                        P8(I_TONE,NP,IFRQ) = PIM%STA(IND_STA)%PCAL(PIM%CONF%FRQ_GRP)%PHAS(J3,IND_FRQ,J2,IND_POL)
                        A8(I_TONE,NP,IFRQ) = PIM%STA(IND_STA)%PCAL(PIM%CONF%FRQ_GRP)%AMPL(J3,IND_FRQ,J2,IND_POL)
                      ELSE IF ( NP * NUM_ACCUM <  NPOI ) THEN ! Averaging
                        POS_INDS = PIM%STA(IND_STA)%PCAL(PIM%CONF%FRQ_GRP)%AMPL(J3,IND_FRQ, &
                       &           (J2-1) * NUM_ACCUM + 1:J2 * NUM_ACCUM,IND_POL) > PIMA__AMP_MIN
                        T8(I_TONE,NP,IFRQ) = SUM ( PIM%STA(IND_STA)%PCAL(PIM%CONF%FRQ_GRP)%TIME_MID_R8( &
                      &                            (J2-1) * NUM_ACCUM + 1:J2 * NUM_ACCUM) ) / NUM_ACCUM
                        I_POS = 0
                        T_POS = 0.0D0
                        PHASE_POS = 0.0D0
!-----------------------Make T_POS and PHASE_POS arrays to ensure correct time unwrapping
!-----------------------Averaging makes correct unwrapping EXTREMELY important
!-----------------------Need to account for blanked epochs, low amplitude points (POS_INDS)
                        DO 440 J4=1,NUM_ACCUM
                           IF ( POS_INDS(J4) .EQV. .TRUE. ) THEN
                                I_POS = I_POS + 1
                                T_POS(I_POS) = &
                              & PIM%STA(IND_STA)%PCAL(PIM%CONF%FRQ_GRP)%TIME_MID_R8((J2-1) * NUM_ACCUM + J4)
                                PHASE_POS(I_POS) = PIM%STA(IND_STA)%PCAL(PIM%CONF%FRQ_GRP)%PHAS(J3,IND_FRQ, &
                       &        (J2-1) * NUM_ACCUM + J4, IND_POL)
                           ENDIF
    440                 CONTINUE
                        IF ( I_POS > 0 ) THEN
                             CALL ERR_PASS ( IUER, IER )
                             CALL AMBIG_RESOLVE ( ARA2__PHD, I_POS, T_POS, PHASE_POS, IER)
                             IF  ( IER .NE. 0 ) THEN
                                  CALL ERR_LOG ( 8867, IUER, 'PIMA_PCAL_AVG', &
                       &                         'Failure in resolving time unwrapped phase ambiguity')
                                  RETURN
                             ENDIF
                        ENDIF
!-----------------------NUM_ACCUM is the number of points in each averaging interval
                        I_POS = 0 
                        DO 450 J5=1,NUM_ACCUM
                           IF ( POS_INDS(J5) .EQV. .TRUE. ) THEN
                                I_POS = I_POS + 1
                                P8(I_TONE,NP,IFRQ) = P8(I_TONE,NP,IFRQ) + PHASE_POS(I_POS) 
                                A8(I_TONE,NP,IFRQ) = A8(I_TONE,NP,IFRQ) + &
                                                     PIM%STA(IND_STA)%PCAL(PIM%CONF%FRQ_GRP)%AMPL(J3, IND_FRQ, &
                      &                              (J2-1) * NUM_ACCUM + J5, IND_POL)
                           ENDIF
    450                 CONTINUE
                        IF  ( I_POS > 0 ) THEN  
                             P8(I_TONE,NP,IFRQ) = P8(I_TONE,NP,IFRQ) / I_POS
                             A8(I_TONE,NP,IFRQ) = A8(I_TONE,NP,IFRQ) / I_POS
                        ENDIF
                      ELSE ! NP * NUM_ACCUM >=  NPOI - special logic in last averaging interval
                        POS_INDS(1:NPOI - (J2-1) * NUM_ACCUM) = PIM%STA(IND_STA)%PCAL(PIM%CONF%FRQ_GRP)%AMPL(J3,IND_FRQ, &
                       &           (J2-1) * NUM_ACCUM + 1:NPOI,IND_POL)  > PIMA__AMP_MIN
                        T8(I_TONE,NP,IFRQ) = SUM ( PIM%STA(IND_STA)%PCAL(PIM%CONF%FRQ_GRP)%TIME_MID_R8( &
                       &                           (J2-1) * NUM_ACCUM + 1:NPOI) ) &
                       &                           / (NPOI - (J2-1) * NUM_ACCUM)
                        I_POS = 0 
                        T_POS = 0.0D0
                        PHASE_POS = 0.0D0
                        DO 460 J6=1,NPOI - (J2-1) * NUM_ACCUM
                           IF ( POS_INDS(J6) .EQV. .TRUE. ) THEN
                                I_POS = I_POS + 1
                                T_POS(I_POS) = &
                              & PIM%STA(IND_STA)%PCAL(PIM%CONF%FRQ_GRP)%TIME_MID_R8((J2-1) * NUM_ACCUM + J6)
                                PHASE_POS(I_POS) = PIM%STA(IND_STA)%PCAL(PIM%CONF%FRQ_GRP)%PHAS(J3,IND_FRQ, &
                       &        (J2-1) * NUM_ACCUM + J6, IND_POL)
                           ENDIF
    460                 CONTINUE
                        IF ( I_POS > 0 ) THEN
                             CALL ERR_PASS ( IUER, IER )
                             CALL AMBIG_RESOLVE ( ARA2__PHD, I_POS, T_POS, PHASE_POS, IER )
                             IF  ( IER .NE. 0 ) THEN
                                  CALL ERR_LOG ( 8868, IUER, 'PIMA_PCAL_AVG', &
                       &                         'Failure in resolving time unwrapped phase ambiguity')
                                  RETURN
                             ENDIF
                        ENDIF
                        I_POS = 0
                        DO 470 J7=1,NPOI - (J2-1) * NUM_ACCUM
                           IF ( POS_INDS(J7) .EQV. .TRUE. ) THEN
                                I_POS = I_POS + 1
                                P8(I_TONE,NP,IFRQ) = P8(I_TONE,NP,IFRQ) + PHASE_POS(I_POS)
                                A8(I_TONE,NP,IFRQ) = A8(I_TONE,NP,IFRQ) + &
                                                     PIM%STA(IND_STA)%PCAL(PIM%CONF%FRQ_GRP)%AMPL(J3, IND_FRQ, &
                        &                            (J2-1) * NUM_ACCUM + J7, IND_POL)
                           ENDIF
    470                 CONTINUE
                        IF  ( I_POS > 0 ) THEN  
                             P8(I_TONE,NP,IFRQ) = P8(I_TONE,NP,IFRQ) / I_POS
                             A8(I_TONE,NP,IFRQ) = A8(I_TONE,NP,IFRQ) / I_POS
                        ENDIF
                   ENDIF
!----------------Compute relative phase Y8 by differencing with respect to reference IF 
!----------------This uses the averaged data in P8
                   IF ( IFRQ /= REF_FREQ .AND. A8(1,NP,IFRQ) > PIMA__AMP_MIN .AND. &
                   &     A8(1,NP,1) > PIMA__AMP_MIN ) THEN
                         Y8(I_TONE,NP,IFRQ) =  P8(I_TONE,NP,IFRQ) - P8(I_TONE,NP,REF_FREQ) 
                         IF ( NP > 1 ) THEN
                              PHS_DIF = Y8(I_TONE,NP,IFRQ) - Y8(I_TONE,NP-1,IFRQ) 
                              PHS_DIF = PHS_DIF - PI2*IDNINT(PHS_DIF/PI2)
                              Y8(I_TONE,NP,IFRQ) = Y8(I_TONE,NP-1,IFRQ) + PHS_DIF
                         ENDIF
                   ENDIF       
    430         CONTINUE
    420       CONTINUE
              Y_ABOVE = 0.0D0
!-------------Unwrapping must be done very carefully here, too, as jumps are identified
!-------------in PIMA_GEPM, and tones will be deactivated if too many are present
!-------------Using similar logic to POS_INDS above, INDS_ABOVE finds valid epochs
              I_TONE=0
              DO 431 J3=1,M_TONES
                 I_TONE=I_TONE+1
                 IF ( IFRQ /= REF_FREQ ) THEN
                      I_ABOVE = 0
                      INDS_ABOVE = A8(I_TONE,1:NPOI_ACCUM,IFRQ) > PIMA__AMP_MIN
                      DO 480 J8=1,NPOI_ACCUM
                         IF ( INDS_ABOVE(J8) .EQV. .TRUE. ) THEN
                              I_ABOVE = I_ABOVE + 1
                              T_ABOVE(I_ABOVE) = T8(1,J8,IFRQ)
                              Y_ABOVE(I_ABOVE) = Y8(I_TONE,J8,IFRQ)
                         ENDIF
       480            CONTINUE
                      Y_BACKUP = Y_ABOVE
                      CALL ERR_PASS ( IUER, IER )
                      CALL AMBIG_RESOLVE ( ARA3__PHD, I_ABOVE, T_ABOVE, &
           &                               Y_ABOVE, IER )
!---------------------Use a backup method of unwrapping if the first try fails
                      IF ( MAXVAL( ABS ( Y_ABOVE ) ) > PHASE_MAX ) THEN
                           Y_ABOVE = Y_BACKUP
                           CALL AMBIG_RESOLVE ( ARA2__PHD, I_ABOVE, T_ABOVE, &
           &                                    Y_ABOVE, IER )
                           IF ( MAXVAL( ABS ( Y_ABOVE ) ) > PHASE_MAX ) THEN
                                Y_ABOVE = Y_BACKUP
                                CALL SIMPLE_AMBIG_RESOLVE ( I_ABOVE, T_ABOVE, &
           &                                                Y_ABOVE, IER )
                           ENDIF
                      ENDIF
                      IF  ( IER .NE. 0 ) THEN
                            CALL ERR_LOG ( 8869, IUER, 'PIMA_PCAL_AVG', &
                &               'Failure in resolving time unwrapped phase ambiguity')
                           RETURN
                      ENDIF
                      I_ABOVE = 0
                      DO 490 J9=1,NPOI_ACCUM
                         IF ( INDS_ABOVE(J9) .EQV. .TRUE. ) THEN
                              I_ABOVE = I_ABOVE + 1
                              Y8(I_TONE,J9,IFRQ) = Y_ABOVE(I_ABOVE) 
                         ENDIF
       490            CONTINUE
                 END IF
    431       CONTINUE
 410       CONTINUE
      ENDIF
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  PIMA_PCAL_AVG  !#!
!
! ------------------------------------------------------------------------
!
      SUBROUTINE PIMA_PCAL_BPASS ( PIM, IND_STA, IND_FRQ, IFRQ, AMP_MIN, &
&                                    NUM_ACCUM, NPOI_ACCUM, FREQS, T8, P8, A8, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine PIMA_PCAL_BPASS                                            *
! *   Remove bandpass shape for given IF (IND_FRQ)                       *
! *  ### 8-JUL-2022  PIMA_PCAL_BPASS   v1.0 (c)        8-JUL-2022 ###    *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'astro_constants.i'
      INCLUDE   'pima.i'
      INCLUDE   'diagi.i'
      TYPE     ( PIMA__TYPE  ) :: PIM
      INTEGER*4  IUER
      INTEGER*4  MPB, MP, NN, MDEG, IND_STA
      PARAMETER  ( MPB    =   7 )
      PARAMETER  ( MP     =  128*1024 )
      PARAMETER  ( MDEG   =   3 )
      REAL*8     FREQS(PIM__MTON,PIM__MFRQ), BPAS_PHASE(PIM__MTON), &
     &           BPAS_AMP(PIM__MTON), AMP_MIN, T8(PIM%NPCT,MP,PIM%NFRQ), &
     &           P8(PIM%NPCT,MP,PIM%NFRQ), A8(PIM%NPCT,MP,PIM%NFRQ), &
     &           PHS_DIF
      CHARACTER  PREF_NAME*128, STR*32, STR1*32, &
     &           MES_SELSTA*80, STA_STR*8
      INTEGER*4  MODE, ISTA_LAST, ICODE, IFRQ, NC, NR, NFRQ, &
     &           J1, J2, J3, J4, J5, J6, J7, J8, J9, NP, IND_POL,  &
     &           M_TONES, IND_TONE, IND_FRQ, N_TONES, I_TONE, IND(2,32), I_ABOVE, &
     &           I_NONZERO, LIND, IER, NPT, POS_NUM,  &
     &           KP, NPOI_ACCUM, NUM_ACCUM, NPOI
      LOGICAL*4  INDS_ABOVE(PIM__MTON), POS_INDS(NUM_ACCUM)
      INTEGER*4, EXTERNAL :: ILEN, I_LEN, LTM_DIF                 
      CALL CHIN ( PIM%CONF%PHAS_CAL_CODE, IND_TONE ) ! Set IND_TONE
!
      IF ( ( PIM%CONF%POLAR == PIMA__POLAR_RR  .OR. &
     &        PIM%CONF%POLAR == PIMA__POLAR_HH       ) .OR. &
     &      PIM%CONF%ACT_CODE == PIMA__PDPL_CODE            ) THEN 
            IND_POL = 1
          ELSE IF ( ( PIM%CONF%POLAR == PIMA__POLAR_LL .OR. &
     &                PIM%CONF%POLAR == PIMA__POLAR_VV      ) .AND. &
     &              PIM%NPOL == 2 ) THEN
            IND_POL = 2
          ELSE IF ( PIM%CONF%POLAR == PIMA__POLAR_LL .AND. &
     &                PIM%NPOL == 1 ) THEN
            IND_POL = 1
          ELSE 
            CALL ERR_LOG ( 8871, IUER, 'PIMA_PCAL_BPASS', 'Polarization code '// &
     &           TRIM(PIM%CONF%POLAR)//' is not supported for plotting pcal. '//&
     &          'Supported codes: RR, LL, HH, VV' )  
            RETURN 
      END IF

      BPAS_PHASE = 0.0D0
      BPAS_AMP = 0.0D0
      NP = 0
      KP = 0
      NPOI =  PIM%STA(IND_STA)%PCAL(1)%NPOI
      M_TONES = PIM%STA(IND_STA)%PCAL(PIM%CONF%FRQ_GRP)%NO_TONES
      IF ( PIM%STA(IND_STA)%PCAL(PIM%CONF%FRQ_GRP)%PCAL_AVAIL .AND. &
  &        PIM%STA(IND_STA)%PCAL(PIM%CONF%FRQ_GRP)%PCAL_USE         ) THEN
!----------Add together all epochs of P8 and A8 to find common bandpass shape
           DO 410 J1=1,NPOI_ACCUM
             NP = NP + 1
             BPAS_PHASE(1:M_TONES) = BPAS_PHASE(1:M_TONES) + P8(1:M_TONES,NP,IFRQ)
             BPAS_AMP(1:M_TONES) = BPAS_AMP(1:M_TONES) + A8(1:M_TONES,NP,IFRQ)  
 410       CONTINUE 
!----------Find average bandpass shape by dividing by number of tones
!----------Set to be 0-centered by subtracting the average phase/amp across the tones
           BPAS_PHASE(1:M_TONES) = BPAS_PHASE(1:M_TONES)/NP
           BPAS_PHASE(1:M_TONES) = BPAS_PHASE(1:M_TONES) - SUM(BPAS_PHASE(1:M_TONES))/M_TONES
           BPAS_AMP(1:M_TONES) = BPAS_AMP(1:M_TONES)/NP
           BPAS_AMP(1:M_TONES) = BPAS_AMP(1:M_TONES) - SUM(BPAS_AMP(1:M_TONES))/M_TONES
           NP = 0
           DO 420 J2=1,NPOI_ACCUM
              NP = NP+1
!-------------Remove bandpass shape in phase and amplitude in P8, A8
              IF ( ALL ( A8(1:M_TONES,NP,IFRQ) >  AMP_MIN ) ) THEN
                   P8(1:M_TONES,NP,IFRQ) = P8(1:M_TONES,NP,IFRQ) - BPAS_PHASE(1:M_TONES)
                   A8(1:M_TONES,NP,IFRQ) = A8(1:M_TONES,NP,IFRQ) - BPAS_AMP(1:M_TONES)

                ELSE ! Identify which amplitude values are below threshold, exclude
                   I_ABOVE = 0
                   INDS_ABOVE(1:M_TONES) = A8(1:M_TONES,NP,IFRQ) > AMP_MIN
                   DO 430 J3 = 1,M_TONES
                      IF ( INDS_ABOVE(J3) .EQV. .TRUE. ) THEN
                           A8(J3,NP,IFRQ) = A8(J3,NP,IFRQ) - BPAS_AMP(J3)
                           P8(J3,NP,IFRQ) = P8(J3,NP,IFRQ) - BPAS_PHASE(J3)
                      ENDIF
 430               CONTINUE
              ENDIF
 420       CONTINUE
      ENDIF
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  PIMA_PCAL_BPASS  !#!  
