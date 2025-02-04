      SUBROUTINE BPASS_STA_STAT ( IND_STA, PIM, BPS )
! ************************************************************************
! *                                                                      *
! *   Routine  BPASS_STA_STAT 
! *                                                                      *
! * ## 27-JAN-2009  BPASS_STA_STAT   v1.1 (c)  L. Petrov  29-JAN-2011 ## *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'pima.i'
      TYPE     ( PIMA__TYPE         ) :: PIM
      TYPE     ( PIM_BPS_STA__TYPE  ) :: BPS
      INTEGER*4  IND_STA
      REAL*4     PHAS_CHAN, PHAS_TOT_AVR, WEI_SUM_TOT, WEI_SUM_FRQ, &
     &           WW_SUM_TOT, WW_SUM_FRQ
      INTEGER*4  J1, J2, J3, J4, J5, J6, J7, J8, &
     &           IND_MAX, IFRQ, KCHN, KFRQ, IER
      LOGICAL*4  FL_BMASK
      REAL*4,    EXTERNAL :: PHAS_CMPL_R4 
!
      IF ( PIM%CONF%BANDPASS_MASK_FILE .NE. PIMA__BPASS_NO  .AND. &
     &     ASSOCIATED ( PIM%BANDPASS_MASK )                       ) THEN
           FL_BMASK = .TRUE.
         ELSE 
           FL_BMASK = .FALSE.
      END IF
!
! --- Compute the average phase over each frequency channel and the 
! --- average phase over the band
!
      PHAS_TOT_AVR = 0.0
      WEI_SUM_TOT  = 0.0
      IFRQ = 0
      DO 410 J1=PIM%CONF%BEG_FRQ,PIM%CONF%END_FRQ
         IFRQ = IFRQ + 1
         BPS%PHAS_FRQ_AVR(IFRQ,IND_STA) = 0.0
         WEI_SUM_FRQ = 0.0
         DO 420 J2=1,PIM%NCHN
            IF ( FL_BMASK ) THEN
                 IF ( PIM%BANDPASS_MASK(J2,J1,IND_STA,PIMA__FINE) == 0 ) THEN
                      GOTO 420
                 END IF
            END IF
            PHAS_CHAN = PHAS_CMPL_R4 ( BPS%CMPL(J2,IFRQ,IND_STA) )
            IF ( PHAS_CHAN > PI__NUM ) PHAS_CHAN = PHAS_CHAN - PI2
            BPS%PHAS_FRQ_AVR(IFRQ,IND_STA) = BPS%PHAS_FRQ_AVR(IFRQ,IND_STA) + &
     &               PHAS_CHAN*ABS(BPS%CMPL(J2,IFRQ,IND_STA))
            WEI_SUM_FRQ = WEI_SUM_FRQ + ABS(BPS%CMPL(J2,IFRQ,IND_STA))
 420     CONTINUE 
         PHAS_TOT_AVR = PHAS_TOT_AVR + BPS%PHAS_FRQ_AVR(IFRQ,IND_STA) 
         WEI_SUM_TOT = WEI_SUM_TOT + WEI_SUM_FRQ 
         IF ( WEI_SUM_FRQ > 0.0 ) THEN
              BPS%PHAS_FRQ_AVR(IFRQ,IND_STA) = BPS%PHAS_FRQ_AVR(IFRQ,IND_STA)/WEI_SUM_FRQ 
            ELSE 
              BPS%PHAS_FRQ_AVR(IFRQ,IND_STA) = 0.0
         END IF
 410  CONTINUE 
      IF ( WEI_SUM_TOT > PIMA__WEI_MIN ) THEN
           PHAS_TOT_AVR = PHAS_TOT_AVR/WEI_SUM_TOT 
      END IF
!
! --- Compute the rms phase over each frequency channel and the 
! --- rms phase over the band
!
      BPS%PHAS_TOT_RMS(IND_STA) = 0.0
      WW_SUM_TOT  = 0.0
      IFRQ = 0
      DO 430 J3=PIM%CONF%BEG_FRQ,PIM%CONF%END_FRQ
         IFRQ = IFRQ + 1
         BPS%PHAS_FRQ_RMS(IFRQ,IND_STA) = 0.0
         WW_SUM_FRQ = 0.0
         DO 440 J4=1,PIM%NCHN
            IF ( FL_BMASK ) THEN
                 IF ( PIM%BANDPASS_MASK(J4,J3,IND_STA,PIMA__FINE) == 0 ) THEN
                      GOTO 440
                 END IF
            END IF
            PHAS_CHAN = PHAS_CMPL_R4 ( BPS%CMPL(J4,IFRQ,IND_STA) )
            IF ( PHAS_CHAN > PI__NUM ) PHAS_CHAN = PHAS_CHAN - PI2
            BPS%PHAS_FRQ_RMS(IFRQ,IND_STA) = BPS%PHAS_FRQ_RMS(IFRQ,IND_STA) + &
                    (PHAS_CHAN - BPS%PHAS_FRQ_AVR(IFRQ,IND_STA))**2* &
     &              (ABS(BPS%CMPL(J4,IFRQ,IND_STA)))**2
            WW_SUM_FRQ = WW_SUM_FRQ + (ABS(BPS%CMPL(J4,IFRQ,IND_STA)))**2
 440     CONTINUE 
         BPS%PHAS_TOT_RMS(IND_STA)= BPS%PHAS_TOT_RMS(IND_STA) + &
     &                                  BPS%PHAS_FRQ_RMS(IFRQ,IND_STA) 
         WW_SUM_TOT = WW_SUM_TOT + WW_SUM_FRQ 
         IF ( WW_SUM_FRQ > 0.0 ) THEN
              BPS%PHAS_FRQ_RMS(IFRQ,IND_STA) = SQRT ( BPS%PHAS_FRQ_RMS(IFRQ,IND_STA)/WW_SUM_FRQ )
         END IF
 430  CONTINUE 
      IF ( WW_SUM_TOT > 1.E-10 ) THEN
           BPS%PHAS_TOT_RMS(IND_STA) = SQRT ( BPS%PHAS_TOT_RMS(IND_STA)/WW_SUM_TOT )
         ELSE 
           BPS%PHAS_TOT_RMS(IND_STA) = 0.0
      END IF
!
      RETURN
      END  SUBROUTINE  BPASS_STA_STAT  !#!#
