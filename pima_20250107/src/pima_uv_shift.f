      SUBROUTINE PIMA_UV_SHIFT ( PIM, VTD, SCA_TYP, IND_FRIP_OBS, &
     &                           ALPHA_OLD, DELTA_OLD, ALPHA_NEW, DELTA_NEW, &
     &                           FREQ_REF, TIM_SRT, UVW, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine PIMA_UV_SHIFT
! *                                                                      *
! * ### 19-FEB-2012  PIMA_UV_SHIFT  v1.0 (c) L. Petrov  19-FEB-2012 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'astro_constants.i'
      INCLUDE   'pima.i'
      INCLUDE   'vtd.i'
      TYPE     ( PIMA__TYPE  ) :: PIM
      TYPE     ( VTD__TYPE   ) :: VTD
      TYPE     ( VTD__OBS_TYPE  ) :: OBS_TYP
      REAL*8     ALPHA_OLD, DELTA_OLD, ALPHA_NEW, DELTA_NEW, &
     &           TIM_SRT, FREQ_REF, UVW(3)
      INTEGER*4  SCA_TYP, IND_FRIP_OBS, IUER
      CHARACTER  C_SOU*8, C_STA(2)*8, STR*32
      INTEGER*4  IND_SOU_PIMA, IND_SOU_VTD, J1, J2, J3, J4, J5, &
     &           IND_OBS, FRG_IND, UV_IND, IFRQ, IER
      REAL*8     GR_DEL_1, PH_DEL_1, PH_RAT_1, GR_DEL_2, PH_DEL_2, PH_RAT_2, &
     &           DER_DEL(VTD__NDER), DER_RAT(VTD__NDER), GR_DEL_OLD, &
     &           PH_RAT_OLD, GR_DEL_NEW, PH_RAT_NEW, PHS_MOD, &
     &           D_DEL, D_RAT, D_PHS, DT
      COMPLEX*8  PHASOR, PHASOR_OLD, PSR_ADD
      INTEGER*4, EXTERNAL :: VTD_SOU_INDEX, LTM_DIF, ILEN, I_LEN
!
      IND_OBS = PIM%FRIP(SCA_TYP)%OBS(IND_FRIP_OBS)%IND_OBS
      OBS_TYP%PLRZ       = 'RR'
      OBS_TYP%FRQ_REF(1) = FREQ_REF
      OBS_TYP%N_BND      = 1
      OBS_TYP%DELAY_TYPE = VTD__PL__DTP
      OBS_TYP%FRQ_ION_EFF(1) = FREQ_REF
      OBS_TYP%STATUS     = VTD__BND
!
      C_SOU = PIM%C_SOU(PIM%OBS(IND_OBS)%SOU_IND)
      C_STA(1) = PIM%C_STA(PIM%OBS(IND_OBS)%STA_IND(1))
      C_STA(2) = PIM%C_STA(PIM%OBS(IND_OBS)%STA_IND(2))
      IND_SOU_VTD  = VTD_SOU_INDEX ( VTD, C_SOU )
      IND_SOU_PIMA = LTM_DIF ( 0, PIM%NSOU, PIM%C_SOU, C_SOU )
!
! --- Update the source coordinates that were used for correlation
!
      VTD%SOU(IND_SOU_VTD)%ALPHA = ALPHA_OLD
      VTD%SOU(IND_SOU_VTD)%DELTA = DELTA_OLD
      VTD%SOU(IND_SOU_VTD)%S_CRS(1) = DCOS(VTD%SOU(IND_SOU_VTD)%DELTA)* &
     &                                DCOS(VTD%SOU(IND_SOU_VTD)%ALPHA)
      VTD%SOU(IND_SOU_VTD)%S_CRS(2) = DCOS(VTD%SOU(IND_SOU_VTD)%DELTA)* &
     &                                DSIN(VTD%SOU(IND_SOU_VTD)%ALPHA)
      VTD%SOU(IND_SOU_VTD)%S_CRS(3) = DSIN(VTD%SOU(IND_SOU_VTD)%DELTA)
      VTD%SOU(IND_SOU_VTD)%SOU_CRS  = VTD%SOU(IND_SOU_VTD)%S_CRS(3)
!
! --- Initicalization to avoid using stale intermediary quantities
!
      VTD%MOM%MJD = -1
      DO 410 J1=1,VTD%L_STA
         VTD%STA(J1)%MJD = -1
 410  CONTINUE
!
! --- Compute delay for input source coordinates
!
      CALL ERR_PASS ( IUER, IER )
      CALL VTD_DELAY ( C_SOU, 'GEOCENTR', C_STA(1), &
     &                 PIM%MJD_0, PIM%TAI_0 + TIM_SRT, OBS_TYP, VTD, &
     &                 GR_DEL_1, PH_RAT_1, DER_DEL, DER_RAT, IER )
      IF ( IER .NE. 0 ) THEN
           write ( 6, * ) 'c_sou = ', c_sou, ' c_sta = ', c_sta(1), c_sta(2) ! %%%
           CALL ERR_LOG ( 9771, IUER, 'PIMA_UV_SHIFT', 'Error in an '// &
     &         'attempt to compute theoretical path delay' )
           RETURN
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL VTD_DELAY ( C_SOU, 'GEOCENTR', C_STA(2), &
     &                 PIM%MJD_0, PIM%TAI_0 + TIM_SRT, OBS_TYP, VTD, &
     &                 GR_DEL_2, PH_RAT_2, DER_DEL, DER_RAT, IER )
      IF ( IER .NE. 0 ) THEN
          CALL ERR_LOG ( 9772, IUER, 'PIMA_UV_SHIFT', 'Error in an '// &
     &        'attempt to compute theoretical path delay' )
          RETURN
      END IF
!
      GR_DEL_OLD = GR_DEL_2 - GR_DEL_1
      PH_RAT_OLD = PH_RAT_2 - PH_RAT_1
!
      VTD%SOU(IND_SOU_VTD)%ALPHA = ALPHA_NEW
      VTD%SOU(IND_SOU_VTD)%DELTA = DELTA_NEW
      VTD%SOU(IND_SOU_VTD)%S_CRS(1) = DCOS(VTD%SOU(IND_SOU_VTD)%DELTA)* &
     &                                DCOS(VTD%SOU(IND_SOU_VTD)%ALPHA)
      VTD%SOU(IND_SOU_VTD)%S_CRS(2) = DCOS(VTD%SOU(IND_SOU_VTD)%DELTA)* &
     &                                DSIN(VTD%SOU(IND_SOU_VTD)%ALPHA)
      VTD%SOU(IND_SOU_VTD)%S_CRS(3) = DSIN(VTD%SOU(IND_SOU_VTD)%DELTA)
      VTD%SOU(IND_SOU_VTD)%SOU_CRS  = VTD%SOU(IND_SOU_VTD)%S_CRS(3)
!
! --- Initicalization to avoid using stale intermediary quantities
!
      VTD%MOM%MJD = -1
      DO 420 J2=1,VTD%L_STA
         VTD%STA(J2)%MJD = -1
 420  CONTINUE
!
! --- Compute delay for the best source coordinates
!
      CALL ERR_PASS ( IUER, IER )
      CALL VTD_DELAY ( C_SOU, 'GEOCENTR', C_STA(1), &
     &                 PIM%MJD_0, PIM%TAI_0 + TIM_SRT, OBS_TYP, VTD, &
     &                 GR_DEL_1, PH_RAT_1, DER_DEL, DER_RAT, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 9773, IUER, 'PIMA_UV_SHIFT', 'Error in an '// &
     &         'attempt to compute theoretical path delay' )
           RETURN
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL VTD_DELAY ( C_SOU, 'GEOCENTR', C_STA(2), &
     &                 PIM%MJD_0, PIM%TAI_0 + TIM_SRT, OBS_TYP, VTD, &
     &                 GR_DEL_2, PH_RAT_2, DER_DEL, DER_RAT, IER )
      IF ( IER .NE. 0 ) THEN
          CALL ERR_LOG ( 9774, IUER, 'PIMA_UV_SHIFT', 'Error in an '// &
     &        'attempt to compute theoretical path delay' )
          RETURN
      END IF
!
      GR_DEL_NEW = GR_DEL_2 - GR_DEL_1
      PH_RAT_NEW = PH_RAT_2 - PH_RAT_1
!
      D_DEL = GR_DEL_NEW - GR_DEL_OLD
      D_RAT = PH_RAT_NEW - PH_RAT_OLD
      D_PHS = PI2*FREQ_REF*D_DEL
      D_PHS = D_PHS - PI2*IDNINT ( D_PHS/PI2 )
!
      CALL ERR_PASS ( IUER, IER )
      CALL VTD_GET_UVW ( C_SOU, C_STA(1), C_STA(2), &
     &                   PIM%MJD_0, PIM%TAI_0 + TIM_SRT, &
     &                   FREQ_REF, VTD, UVW, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 9775, IUER, 'PIMA_UV_SHIFT', &
     &         'Error in an attempt to compute UV coordinates delay' )
           RETURN
      END IF
!
      FRG_IND = PIM%OBS(IND_OBS)%REF_FRG_INDS(PIM%CONF%FRQ_GRP)
      IF ( PIM%OBS(IND_OBS)%NUM_EPC(PIM%OBS(IND_OBS)%REF_FRG_INDS(PIM%CONF%FRQ_GRP)) .NE. &
     &     PIM%FRIP(SCA_TYP)%OBS(IND_FRIP_OBS)%NAP ) THEN
           WRITE ( 6, * ) 'IND_FRIP_OBS = ', IND_FRIP_OBS, &
     &                    ' IND_OBS= ', IND_OBS, &
     &                    ' NUM_EPC = ', PIM%OBS(IND_OBS)%NUM_EPC(PIM%OBS(IND_OBS)%REF_FRG_INDS(PIM%CONF%FRQ_GRP)), &
     &                    ' NAP = ', PIM%FRIP(SCA_TYP)%OBS(IND_FRIP_OBS)%NAP
           CALL CLRCH ( STR )
           CALL INCH  ( IND_OBS, STR )
           CALL ERR_LOG ( 9776, IUER, 'PIMA_UV_SHIFT', 'Trap of internal '// &
     &         'control: for observation '//STR(1:I_LEN(STR))// &
     &         ' the number of accumulation periods is different' )
           RETURN 
      END IF
!
      DO 430 J3=1,PIM%OBS(IND_OBS)%NUM_EPC(PIM%OBS(IND_OBS)%REF_FRG_INDS(PIM%CONF%FRQ_GRP))
         IFRQ = 0
         UV_IND  = PIM%OBS(IND_OBS)%UV_IND(J3,FRG_IND)
         DT = PIM%TIM_R8(PIM%UV_IND(UV_IND)%TIM_IND) - TIM_SRT
         DO 440 J4=PIM%CONF%BEG_FRQ,PIM%CONF%END_FRQ
!!!!!
!@            DO 450 J5=1,PIM%NCHN
!@               IFRQ = IFRQ + 1
!@               PHS_MOD =   D_PHS &
!@     &                   + D_RAT*PI2*FREQ_REF*DT &
!@     &                   + D_DEL*PI2*(PIM%FRIP(SCA_TYP)%FRQ(IFRQ) - FREQ_REF)
!@!
!@               PHS_MOD = PHS_MOD - PI2*IDNINT ( PHS_MOD/PI2 )
!@               PIM%FRIP(SCA_TYP)%OBS(IND_FRIP_OBS)%VIS(IFRQ,J3) = &
!@     &             PIM%FRIP(SCA_TYP)%OBS(IND_FRIP_OBS)%VIS(IFRQ,J3)* &
!@     &             CMPLX ( COS(SNGL(PHS_MOD)), SIN(SNGL(PHS_MOD)) )
!@ 450        CONTINUE
!!!!
            PHS_MOD =   D_PHS &
     &                + D_RAT*PI2*FREQ_REF*DT &
     &                + D_DEL*PI2*(PIM%FRIP(SCA_TYP)%FRQ(IFRQ+1) - FREQ_REF)
            PHASOR = CMPLX ( COS(SNGL(PHS_MOD)), SIN(SNGL(PHS_MOD)) )
            PHS_MOD = D_DEL*PI2*(PIM%FRIP(SCA_TYP)%FRQ(IFRQ+2) - &
     &                           PIM%FRIP(SCA_TYP)%FRQ(IFRQ+1))
            PSR_ADD = CMPLX ( COS(SNGL(PHS_MOD)), SIN(SNGL(PHS_MOD)) )
            DO 450 J5=1,PIM%NCHN
               IFRQ = IFRQ + 1
               PIM%FRIP(SCA_TYP)%OBS(IND_FRIP_OBS)%VIS(IFRQ,J3) = &
     &             PIM%FRIP(SCA_TYP)%OBS(IND_FRIP_OBS)%VIS(IFRQ,J3)* &
     &             PHASOR
               PHASOR = PHASOR*PSR_ADD
 450        CONTINUE
 440     CONTINUE
 430  CONTINUE
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!  write ( 6, * ) 'PUS ', c_sta(1), c_sta(2), &
!     &           ' d_del= ', sngl(d_del), ' d_rat= ', sngl(d_rat)
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  PIMA_UV_SHIFT  !#!#
