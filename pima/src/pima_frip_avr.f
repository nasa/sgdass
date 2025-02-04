      SUBROUTINE PIMA_FRIP_AVR ( PIM, SCA_TYP, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine PIMA_FRIP_AVR 
! *                                                                      *
! * ### 07-JAN-2012  PIMA_FRIP_AVR  v1.1 (c)  L. Petrov  04-FEB-2012 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'pima.i'
      TYPE     ( PIMA__TYPE  ) :: PIM
      INTEGER*4  SCA_TYP, GRID_ALG, IUER
      REAL*8     VIS_SCL
      REAL*8     PHS_MOD, FREQ_REF, TIM_FROM_EPOCH, DER
      REAL*4     UV_GRID(2), VIS_STEP, U_MIN, V_MIN, U_SHF, V_SHF, &
     &           CON_U, CON_V, WEI_SUM, TOT_WES, WES_AP, WEI_USED, TOT_WEI_USED
      CHARACTER  STR*128
      COMPLEX*8  VIS, TOT_VIS
      REAL*4       EPS
      PARAMETER  ( EPS = 1.E-4 )
      INTEGER*4  J1, J2, J3, J4, J5, J6, J7, J8, J9, J10, J11, &
     &           LFRQ, FRG_IND, UV_IND, DIM, IFRQ, &
     &           IND_BND, IND_FRA, IND_OBS, IND_AF, IER 
      REAL*4,    EXTERNAL :: ATAN_CS_R4, PHAS_CMPL_R4
      INTEGER*4, EXTERNAL :: ILEN, I_LEN, LTM_DIF
!
      IND_BND = 1
      LFRQ = PIM%CONF%END_FRQ - PIM%CONF%BEG_FRQ + 1
!
      IF ( PIM%CONF%MKDB_FRINGE_ALGORITHM == PIMA__FRA_DRF ) THEN
           IND_FRA = PIMA__DRF
         ELSE IF ( PIM%CONF%MKDB_FRINGE_ALGORITHM == PIMA__FRA_LSQ ) THEN
           IND_FRA = PIMA__LSQ
         ELSE IF ( PIM%CONF%MKDB_FRINGE_ALGORITHM == PIMA__FRA_MUL ) THEN
           IND_FRA = PIMA__MUL
         ELSE IF ( PIM%CONF%MKDB_FRINGE_ALGORITHM == PIMA__FRA_ADD ) THEN
           IND_FRA = PIMA__ADD
      END IF
!
      ALLOCATE ( PIM%FRIP(SCA_TYP)%VIS_AF(LFRQ,PIM%FRIP(SCA_TYP)%NOBS), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( 4*2*LFRQ*PIM%FRIP(SCA_TYP)%NOBS, STR )
           CALL ERR_LOG ( 9211, IUER, 'PIMA_FRIP_AVR', 'Failre in '// &
     &         'an attemp to allocate '//STR(1:I_LEN(STR))// &
     &         ' bytes of dynamic memory for array PIM%FRIP(SCA_TYP)%VIS_AF' )
           RETURN 
      END IF
!
      ALLOCATE ( PIM%FRIP(SCA_TYP)%WEI_AF(LFRQ,PIM%FRIP(SCA_TYP)%NOBS), STAT=IER  )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( 4*LFRQ*PIM%FRIP(SCA_TYP)%NOBS, STR )
           CALL ERR_LOG ( 9212, IUER, 'PIMA_FRIP_AVR', 'Failre in '// &
     &         'an attemp to allocate '//STR(1:I_LEN(STR))// &
     &         ' bytes of dynamic memory for array PIM%FRIP(SCA_TYP)%WEI_AF' )
           RETURN 
      END IF
!
      ALLOCATE ( PIM%FRIP(SCA_TYP)%UVW_AF(3,LFRQ,PIM%FRIP(SCA_TYP)%NOBS), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( 4*3*LFRQ*PIM%FRIP(SCA_TYP)%NOBS, STR )
           CALL ERR_LOG ( 9213, IUER, 'PIMA_FRIP_AVR', 'Failre in '// &
     &         'an attemp to allocate '//STR(1:I_LEN(STR))// &
     &         ' bytes of dynamic memory for array PIM%FRIP(SCA_TYP)%UVW_AF' )
           RETURN 
      END IF
!
      ALLOCATE ( PIM%FRIP(SCA_TYP)%IND_STA(2,PIM%FRIP(SCA_TYP)%NOBS), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( 8*PIM%FRIP(SCA_TYP)%NOBS, STR )
           CALL ERR_LOG ( 9214, IUER, 'PIMA_FRIP_AVR', 'Failre in '// &
     &         'an attemp to allocate '//STR(1:I_LEN(STR))// &
     &         ' bytes of dynamic memory for array PIM%FRIP(SCA_TYP)%IND_STA' )
           RETURN 
      END IF
!
      ALLOCATE ( PIM%FRIP(SCA_TYP)%IAMB_AF(LFRQ,PIM%FRIP(SCA_TYP)%NOBS), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( 4*LFRQ*PIM%FRIP(SCA_TYP)%NOBS, STR )
           CALL ERR_LOG ( 9215, IUER, 'PIMA_FRIP_AVR', 'Failre in '// &
     &         'an attemp to allocate '//STR(1:I_LEN(STR))// &
     &         ' bytes of dynamic memory for array PIM%FRIP(SCA_TYP)%IAMB_AF' )
           RETURN 
      END IF
!
      ALLOCATE ( PIM%FRIP(SCA_TYP)%FREQ_AF(LFRQ), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( 4*LFRQ, STR )
           CALL ERR_LOG ( 9215, IUER, 'PIMA_FRIP_AVR', 'Failre in '// &
     &         'an attemp to allocate '//STR(1:I_LEN(STR))// &
     &         ' bytes of dynamic memory for array PIM%FRIP(SCA_TYP)%FREQ_AF' )
           RETURN 
      END IF
      PIM%FRIP(SCA_TYP)%AF_STATUS = PIMA__ALLOCATED
!
      FREQ_REF = PIM%FRQ(PIM%CONF%BEG_FRQ,PIM%CONF%FRQ_GRP)%FREQ
      PIM%FRIP(SCA_TYP)%VIS_AF  = CMPLX ( 0.0, 0.0 )
      PIM%FRIP(SCA_TYP)%WEI_AF  = 0.0
      PIM%FRIP(SCA_TYP)%UVW_AF  = 0.0
      PIM%FRIP(SCA_TYP)%IND_STA = 0
!
      DO 410 J1=1,PIM%FRIP(SCA_TYP)%NOBS
         IF ( .NOT. PIM%FRIP(SCA_TYP)%USED(J1) ) GOTO 410
         IND_OBS = PIM%FRIP(SCA_TYP)%OBS(J1)%IND_OBS 
         FRG_IND = PIM%OBS(IND_OBS)%REF_FRG_INDS(PIM%CONF%FRQ_GRP) 
!
         DO 430 J3=1,PIM%FRIP(SCA_TYP)%OBS(J1)%NAP
            IFRQ = 0
            DO 440 J4=PIM%CONF%BEG_FRQ,PIM%CONF%END_FRQ
               VIS = 0.0
               WEI_SUM = 0.0
               IF ( 8*(PIM%NCHN/8) == PIM%NCHN ) THEN
                    DO 450 J5=1,PIM%NCHN/8
                       VIS = VIS + PIM%FRIP(SCA_TYP)%OBS(J1)%VIS(IFRQ+1,J3)* &
     &                             PIM%FRIP(SCA_TYP)%OBS(J1)%WEI(IFRQ+1,J3)  &
     &                           + PIM%FRIP(SCA_TYP)%OBS(J1)%VIS(IFRQ+2,J3)* &
     &                             PIM%FRIP(SCA_TYP)%OBS(J1)%WEI(IFRQ+2,J3)  &
     &                           + PIM%FRIP(SCA_TYP)%OBS(J1)%VIS(IFRQ+3,J3)* &
     &                             PIM%FRIP(SCA_TYP)%OBS(J1)%WEI(IFRQ+3,J3)  &
     &                           + PIM%FRIP(SCA_TYP)%OBS(J1)%VIS(IFRQ+4,J3)* &
     &                             PIM%FRIP(SCA_TYP)%OBS(J1)%WEI(IFRQ+4,J3)  &
     &                           + PIM%FRIP(SCA_TYP)%OBS(J1)%VIS(IFRQ+5,J3)* &
     &                             PIM%FRIP(SCA_TYP)%OBS(J1)%WEI(IFRQ+5,J3)  &
     &                           + PIM%FRIP(SCA_TYP)%OBS(J1)%VIS(IFRQ+6,J3)* &
     &                             PIM%FRIP(SCA_TYP)%OBS(J1)%WEI(IFRQ+6,J3)  &
     &                           + PIM%FRIP(SCA_TYP)%OBS(J1)%VIS(IFRQ+7,J3)* &
     &                             PIM%FRIP(SCA_TYP)%OBS(J1)%WEI(IFRQ+7,J3)  &
     &                           + PIM%FRIP(SCA_TYP)%OBS(J1)%VIS(IFRQ+8,J3)* &
     &                             PIM%FRIP(SCA_TYP)%OBS(J1)%WEI(IFRQ+8,J3)
!
                       WEI_SUM = WEI_SUM + PIM%FRIP(SCA_TYP)%OBS(J1)%WEI(IFRQ+1,J3) &
     &                                   + PIM%FRIP(SCA_TYP)%OBS(J1)%WEI(IFRQ+2,J3) &
     &                                   + PIM%FRIP(SCA_TYP)%OBS(J1)%WEI(IFRQ+3,J3) &
     &                                   + PIM%FRIP(SCA_TYP)%OBS(J1)%WEI(IFRQ+4,J3) &
     &                                   + PIM%FRIP(SCA_TYP)%OBS(J1)%WEI(IFRQ+5,J3) &
     &                                   + PIM%FRIP(SCA_TYP)%OBS(J1)%WEI(IFRQ+6,J3) &
     &                                   + PIM%FRIP(SCA_TYP)%OBS(J1)%WEI(IFRQ+7,J3) &
     &                                   + PIM%FRIP(SCA_TYP)%OBS(J1)%WEI(IFRQ+8,J3)
                       IFRQ = IFRQ + 8
 450                CONTINUE 
                 ELSE 
!
! ----------------- Slow variant
!
                    DO 460 J6=1,PIM%NCHN
                       IFRQ = IFRQ + 1
                       VIS = VIS + PIM%FRIP(SCA_TYP)%OBS(J1)%VIS(IFRQ,J3)* &
     &                             PIM%FRIP(SCA_TYP)%OBS(J1)%WEI(IFRQ,J3)
!
                       WEI_SUM = WEI_SUM + PIM%FRIP(SCA_TYP)%OBS(J1)%WEI(IFRQ,J3)
 460                CONTINUE 
               END IF
               PIM%FRIP(SCA_TYP)%VIS_AF(J4,J1) = PIM%FRIP(SCA_TYP)%VIS_AF(J4,J1) + VIS
               PIM%FRIP(SCA_TYP)%WEI_AF(J4,J1) = PIM%FRIP(SCA_TYP)%WEI_AF(J4,J1) + WEI_SUM
!
               TOT_VIS = TOT_VIS + VIS
               TOT_WES = TOT_WES + WEI_SUM
 440        CONTINUE 
 430     CONTINUE 
 410  CONTINUE 
!
      IFRQ = 0
      DO 470 J7=PIM%CONF%BEG_FRQ,PIM%CONF%END_FRQ
         IFRQ = IFRQ + 1
         PIM%FRIP(SCA_TYP)%FREQ_AF(IFRQ) = PIM%FRQ(J7,FRG_IND)%FREQ
 470  CONTINUE 
!
      DO 480 J8=1,PIM%FRIP(SCA_TYP)%NOBS
         IF ( .NOT. PIM%FRIP(SCA_TYP)%USED(J8) ) GOTO 480
         IND_OBS = PIM%FRIP(SCA_TYP)%OBS(J8)%IND_OBS 
         DO 490 J9=1,LFRQ
            IND_AF = (J9-1)*PIM%NCHN + 1
            IF ( PIM%FRIP(SCA_TYP)%WEI_AF(J9,J8) > PIMA__WEI_MIN ) THEN
                 PIM%FRIP(SCA_TYP)%VIS_AF(J9,J8) = PIM%FRIP(SCA_TYP)%VIS_AF(J9,J8)/PIM%FRIP(SCA_TYP)%WEI_AF(J9,J8)
                 PIM%FRIP(SCA_TYP)%WEI_AF(J9,J8) = PIM%FRIP(SCA_TYP)%WEI_AF(J9,J8)/(PIM%NCHN*PIM%FRIP(SCA_TYP)%OBS(J8)%NAP)
               ELSE 
                 PIM%FRIP(SCA_TYP)%VIS_AF(J9,J8) = 0.0
                 PIM%FRIP(SCA_TYP)%WEI_AF(J9,J8) = 0.0
            END IF
            DO 4100 J10=1,3
!
! ------------ Compute UVW for the reference frequency of the IF
! ------------ by scaling UVW_SRT that was computed for the first 
! ------------ frequency
!
               PIM%FRIP(SCA_TYP)%UVW_AF(J10,J9,J8) = PIM%FRIP(SCA_TYP)%OBS(J8)%UVW_SRT(J10)* &
     &                                               PIM%FRIP(SCA_TYP)%FREQ_AF(J9)/ &
     &                                               PIM%FRIP(SCA_TYP)%FRQ(1) 
 4100       CONTINUE 
 490     CONTINUE 
!
! ------ Store station indices for a given observation
!
         DO 4110 J11=1,2
            PIM%FRIP(SCA_TYP)%IND_STA(J11,J8) = LTM_DIF ( 0, PIM%FRIP(SCA_TYP)%L_STA, &
     &               PIM%FRIP(SCA_TYP)%C_STA, &
     &               PIM%C_STA(PIM%OBS(PIM%FRIP(SCA_TYP)%OBS(J8)%IND_OBS)%STA_IND(J11)) )
 4110    CONTINUE 
 480  CONTINUE 
      PIM%FRIP(SCA_TYP)%AF_STATUS = PIMA__LOADED
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  PIMA_FRIP_AVR  !#!  
