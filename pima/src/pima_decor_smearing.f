      SUBROUTINE PIMA_DECOR_SMEARING ( PIM, VTD, IND_OBS, TIM_FRT, &
     &                                 RES_GR_DEL, DECOR_BS, DECOR_TS, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  PIMA_DECOR_SMEARING
! *                                                                      *
! * # 26-NOV-2019 PIMA_DECOR_SMEARING v2.1 (c)  L. Petrov  26-MAR-2020 # *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'pima.i'
      INCLUDE   'vtd.i'
      TYPE     ( PIMA__TYPE  ) :: PIM
      TYPE     ( VTD__TYPE   ) :: VTD
      TYPE     ( VTD__OBS_TYPE  ) :: OBS_TYP
      INTEGER*4  IND_OBS, IUER
      REAL*8     TIM_FRT, RES_GR_DEL, DECOR_BS, DECOR_TS
      REAL*8     DER_DEL(VTD__NDER), DER_RAT(VTD__NDER), TAU_PH, RATE_PH
      REAL*8     ALPHA_INP, DELTA_INP, ARG_BS, ARG_TS, DF, DT
      REAL*8     EPS, DECOR_TS__MIN
      PARAMETER  ( EPS = 1.D-3 )
      PARAMETER  ( DECOR_TS__MIN = 0.001D0 )
      INTEGER*4  IND_SOU, IND_STA(2), IER
      INTEGER*4, EXTERNAL :: ILEN, I_LEN
!
      IND_SOU = PIM%OBS(IND_OBS)%SOU_IND
      IND_STA = PIM%OBS(IND_OBS)%STA_IND
!
      OBS_TYP%PLRZ    = 'RR'     
      OBS_TYP%FRQ_REF(1) = PIM%REF_FREQ
      OBS_TYP%FRQ_REF(2) = 0.0D0
      OBS_TYP%N_BND      = 1
      OBS_TYP%DELAY_TYPE = VTD__ML__DTP 
      OBS_TYP%FRQ_ION_EFF(1) = 2.3D9
      OBS_TYP%FRQ_ION_EFF(2) = 0.0D0
      OBS_TYP%STATUS     = VTD__BND 
!
! --- Disable automatic NERS update during run
!
      VTD%NERS%CNF%AGE_FCS = 1.D15
      VTD%NERS%CNF%AGE_SPL = 1.D15
!
      IF ( PIM%SOU(IND_SOU)%IND_SWAP == 0 ) THEN
           ALPHA_INP = PIM%SOU(IND_SOU)%ALPHA_INP
           DELTA_INP = PIM%SOU(IND_SOU)%DELTA_INP
        ELSE
           ALPHA_INP = PIM%SOU(PIM%SOU(IND_SOU)%IND_SWAP)%ALPHA_INP
           DELTA_INP = PIM%SOU(PIM%SOU(IND_SOU)%IND_SWAP)%DELTA_INP
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL VTD_DELAY ( PIM%C_SOU(IND_SOU), PIM%C_STA(IND_STA(1)), &
     &                 PIM%C_STA(IND_STA(2)), PIM%MJD_0, &
     &                 PIM%TAI_0 + PIM%OBS(IND_OBS)%TIM_BEG - TIM_FRT, &
     &                 OBS_TYP, VTD, TAU_PH, RATE_PH, &
     &                 DER_DEL, DER_RAT, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 7581, IUER, 'PIMA_DECOR_SMEARING', 'Error in '// &
     &         'computing path delay and its partial derivatives' )
           RETURN 
      END IF
!
      DF = PIM%FRQ(1,1)%CHAN_WIDTH
      DT = PIM%OBS(IND_OBS)%AP_LEN
!
      ARG_BS = PI__NUM*DF*(DER_DEL(VTD__DER_RA)*(ALPHA_INP - PIM%SOU(IND_SOU)%ALPHA) + &
     &                     DER_DEL(VTD__DER_DL)*(DELTA_INP - PIM%SOU(IND_SOU)%DELTA)   )
      ARG_TS = PI__NUM*DT*(DER_RAT(VTD__DER_RA)*(ALPHA_INP - PIM%SOU(IND_SOU)%ALPHA) + &
     &                     DER_RAT(VTD__DER_DL)*(DELTA_INP - PIM%SOU(IND_SOU)%DELTA)   )* &
     &                     PIM%FREQ_ARR(1,PIM%CONF%BEG_FRQ,PIM%CONF%FRQ_GRP)
      IF ( DABS(ARG_TS) > EPS ) THEN
           DECOR_TS = DABS ( DSIN(ARG_TS)/ARG_TS )
         ELSE 
           DECOR_TS = 1.0D0
      END IF
!
      IF ( DECOR_TS < DECOR_TS__MIN ) THEN
           DECOR_TS = DECOR_TS__MIN
      END IF
!
      IF ( PIM%CORR_VERS(1:4) .NE. "????" ) THEN
!
! -------- FX correlator
!
           IF ( DABS(RES_GR_DEL) > 1.D-14 ) THEN
                DECOR_BS = (1.0D0 - DABS(RES_GR_DEL)*DF)
              ELSE
                DECOR_BS = 1.0D0
           END IF
           IF ( DECOR_BS < 0.001 ) DECOR_BS = 0.001
         ELSE
!
! -------- Unknown correlator
!
           IF ( DABS(ARG_BS) > EPS ) THEN
                DECOR_BS = DABS ( DSIN(ARG_BS)/ARG_BS )
             ELSE 
                DECOR_BS = 1.0D0
           END IF
      END IF

!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  PIMA_DECOR_SMEARING   !#!#
