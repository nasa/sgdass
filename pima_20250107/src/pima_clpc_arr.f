      SUBROUTINE PIMA_CLPC_ARR ( N_TONES, NFRQ, NPCL, NPOI, KP, C_STA, &
     &                           FREQ, TIM, IND_FRQ, IND_TON, AMPL, PHAS_AMB, &
     &                           MASK, IVRB, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine PIMA_CLPC_ARR
! *                                                                      *
! *  ### 24-JUN-2019  PIMA_CLPC_ARR v1.0 (c)  L. Petrov  24-JUN-2019 ###  
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INTEGER*4  N_TONES, NFRQ, NPCL, NPOI, KP, IND_FRQ(NPCL), IND_TON(NPCL), IVRB, IUER
      REAL*8     FREQ(NPCL), TIM(NPOI), AMPL(NPCL,NPOI), PHAS_AMB(NPCL,NPOI)
      REAL*8     ARG(8192), VAL1(8192), VAL2(8192)
      INTEGER*1  MASK(N_TONES,NFRQ,NPOI)
      CHARACTER  C_STA*(*)
      CHARACTER  STR*128
      LOGICAL*1  FL_PLOT
      INTEGER*4  J1, J2, J3, LP, K_IF, K_TON, IER
      FL_PLOT = .TRUE.
!
      K_IF   = 1
      K_TON = 3
      LP = 0
!
      IF ( IVRB .GE. 2 ) write ( 6, * ) 'Process pcal for station '//c_sta
      IF ( FL_PLOT ) THEN
           DO 410 J1=1,NPOI
              CALL CLRCH ( STR )
              CALL INCH  ( J1, STR )
!
              CALL DIAGI_SETDEF ( IER, 'DIAGI_ICL1', 1 )
              CALL DIAGI_SETDEF ( IER, 'DIAGI_CTIT', 'Phase cal phase, station '// &
     &                                 C_STA//' epoch '//TRIM(STR) )
!@              CALL DIAGI_1 ( KP, FREQ, PHAS_AMB(1,J1), IER )
!
              CALL DIAGI_SETDEF ( IER, 'DIAGI_CTIT', 'Phase cal amplitude, station '// &
     &                                 C_STA//' epoch '//TRIM(STR) )
              CALL DIAGI_SETDEF ( IER, 'DIAGI_ICL1', 2 )
!@              CALL DIAGI_1 ( KP, FREQ, AMPL(1,J1), IER )
              DO 420 J2=1,NPCL
                  IF ( IND_FRQ(J2) == K_IF .AND. IND_TON(J2) == K_TON ) THEN
                       LP = LP + 1
                       ARG(LP) = TIM(J1)
                       VAL1(LP) = PHAS_AMB(J2,J1)
                       VAL2(LP) = PHAS_AMB(J2+1,J1)
                  END IF
 420          CONTINUE 
 410       CONTINUE 
           IF ( LP > 1 ) THEN
                CALL DIAGI_SETDEF ( IER, 'DIAGI_ICL1', 1 )
                CALL DIAGI_SETDEF ( IER, 'DIAGI_ICL2', 2 )
                CALL DIAGI_2 ( LP, ARG, VAL1, LP, ARG, VAL2, IER )
              ELSE
                WRITE ( 6, * ) 'No points to plot for station '//C_STA 
           END IF
      END IF
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  PIMA_CLPC_ARR  !#!#
