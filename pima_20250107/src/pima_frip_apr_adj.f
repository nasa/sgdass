      SUBROUTINE PIMA_FRIP_APR_ADJ ( PIM, VTD, IUER ) 
! ************************************************************************
! *                                                                      *
! *   Routine  PIMA_FRIP_APR_ADJ                                         *
! *                                                                      *
! * ### 20-JAN-2012 PIMA_FRIP_APR_ADJ v2.0 (c) L. Petrov  12-FEB-2012 ## *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'astro_constants.i'
      INCLUDE   'pima.i'
      INCLUDE   'vtd.i'
      TYPE     ( PIMA__TYPE  ) :: PIM
      TYPE     ( VTD__TYPE   ) :: VTD
      INTEGER*4  IND_SCA(2), IUER
      TYPE     ( VTD__OBS_TYPE  ) :: OBS_TYP
      CHARACTER  STR*128, BAS_STA(2)*8
      REAL*8     GR_DEL_1, PH_DEL_1, PH_RAT_1, GR_DEL_2, PH_DEL_2, PH_RAT_2, &
     &           THE_GR_DEL, THE_PH_DEL, THE_PH_RAT, APR_GR_DEL, &
     &           APR_PH_RAT, DER_DEL(VTD__NDER), DER_RAT(VTD__NDER), &
     &           D_PHS(2), D_PH_RAT(2), D_GR_DEL(2), FREQ_REF, DT, PHS_MOD, &
     &           ADD_CLO(2), ADD_RAT(2), TIM_SRT, UVW_ARR(3)
      REAL*8     T8(8192), X8(8192), PHS_AVR
      COMPLEX*8  DRF(8192), DRF_ALL
      LOGICAL*1  FL_ORDER
      INTEGER*4  J1, J2, J3, J4, J5, J6, J7, J8, J9, J10, IFRQ, &
     &           IND_FRA, IND_BND, IND_TAG, IND_OBS_CAL, IND_OBS_TAG, IER
      REAL*4,    EXTERNAL :: PHAS_CMPL_R4
      CHARACTER, EXTERNAL :: MJDSEC_TO_DATE*30
      INTEGER*4, EXTERNAL :: ILEN, I_LEN
!
! --- Set up for VTD
!
      IF ( VTD%STATUS .EQ. VTD__INIT .OR. &
     &     VTD%STATUS .EQ. VTD__ALLC .OR. &
     &     VTD%STATUS .EQ. VTD__LOAD      ) THEN
!
! -------- Initlialize VTD object
!
           CALL ERR_PASS ( IUER, IER )
           CALL VTD_QUIT ( VTD,  IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 9711, IUER, 'PIMA_FRIP_APR_ADJ', &
     &              'Error in an attempt to initialize VTD oibject' )
                RETURN 
           END IF
      END IF
!
! --- Initlialize VTD object
!
      CALL ERR_PASS ( IUER, IER )
      CALL VTD_INIT ( VTD,  IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 9712, IUER, 'PIMA_FRIP_APR_ADJ', 'Error in '// &
     &         'an attempt to initialize VTD oibject' )
           RETURN 
      END IF
!
! --- Read and parse configuration file
!
      CALL ERR_PASS ( IUER, IER )
      CALL VTD_CONF ( PIM%CONF%VTD_CONFIG_FILE, VTD, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 9713, IUER, 'PIMA_FRIP_APR_ADJ', 'Error in '// &
     &         'an attempt to read configuration file '// &
     &          PIM%CONF%VTD_CONFIG_FILE )
           RETURN 
      END IF
!
! --- Load catalogues, ephemerides, EOP series and other data files
!
      CALL ERR_PASS ( IUER, IER )
      PIM%C_STA(PIM%NSTA+1) = 'GEOCENTR' 
      CALL VTD_LOAD ( VTD, PIM%NSTA+1, PIM%C_STA, PIM%NSOU, PIM%C_SOU, &
     &                     PIM%MJD_0, PIM%TAI_0 - PIM__MSCL, PIM%MJD_0, &
     &                     PIM%TAI_0 + PIM%TIM_R8(PIM%NEPC) + PIM__MSCL, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 9713, IUER, 'PIMA_FRIP_APR_ADJ', 'Error in an '// &
     &         'attempt to load the data into VTD data structure' )
           RETURN 
      END IF
!
! --- Disable automatic NERS update during run
!
      VTD%NERS%CNF%AGE_FCS = 1.D15
      VTD%NERS%CNF%AGE_SPL = 1.D15
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
      FREQ_REF = PIM%FRQ(PIM%CONF%BEG_FRQ,PIM%CONF%FRQ_GRP)%FREQ
      TIM_SRT = PIM%FRIP(PIMA__CAL)%TIM_EPC
!
      IND_BND = 1
      DO 410 J1=1,PIM%FRIP(PIMA__CAL)%NOBS
         IF ( .NOT. PIM%FRIP(PIMA__CAL)%USED(J1) ) GOTO 410
         IND_OBS_CAL = PIM%FRIP(PIMA__CAL)%OBS(J1)%IND_OBS 
!
         IF ( PIM%OBS(IND_OBS_CAL)%STA_IND(2) > PIM%OBS(IND_OBS_CAL)%STA_IND(1) ) THEN
              FL_ORDER = .TRUE.
            ELSE 
              FL_ORDER = .FALSE.
         END IF
!
         BAS_STA(1) = PIM%C_STA(PIM%OBS(IND_OBS_CAL)%STA_IND(1))
         BAS_STA(2) = PIM%C_STA(PIM%OBS(IND_OBS_CAL)%STA_IND(2))
         CALL ERR_PASS ( IUER, IER )
!         CALL PIMA_SOU_THEO_DT ( PIM, VTD, &
!     &                           PIM%C_SOU(PIM%OBS(IND_OBS_CAL)%SOU_IND), &
!     &                           BAS_STA, TIM_SRT, FREQ_REF, &
!     &                           D_GR_DEL(PIMA__CAL), &
!     &                           D_PH_RAT(PIMA__CAL), &
!     &                           D_PHS(PIMA__CAL), &
!     &                           PIM%FRIP(PIMA__CAL)%OBS(J1)%UVW_SRT, IER )
         CALL PIMA_APR_THEO_DT ( PIM, VTD, IND_OBS_CAL, IND_BND, &
     &                           PIM%C_SOU(PIM%OBS(IND_OBS_CAL)%SOU_IND), &
     &                           BAS_STA, TIM_SRT, FREQ_REF, &
     &                           D_GR_DEL(PIMA__CAL), &
     &                           D_PH_RAT(PIMA__CAL), &
     &                           D_PHS(PIMA__CAL), &
     &                           PIM%FRIP(PIMA__CAL)%OBS(J1)%UVW_SRT, IER )
         IF ( IER .NE. 0 ) THEN
              CALL CLRCH ( STR )
              CALL INCH  ( IND_OBS_CAL, STR )
              CALL ERR_LOG ( 9714, IUER, 'PIMA_FRIP_APR_ADJ', 'Error in '// &
     &            'an attempt to compute theoretical path delay for '// &
     &            'observation '//STR )
              RETURN 
         END IF
         IF ( PIM%CONF%DEBUG_LEVEL.GE. 6 ) THEN
              STR = MJDSEC_TO_DATE ( PIM%MJD_0, PIM%TAI_0 + TIM_SRT, -2 )
              WRITE ( 6, 210 ) PIMA__FRIP_SCATYP(PIMA__CAL), IND_OBS_CAL, &
     &                         PIM%C_STA(PIM%OBS(IND_OBS_CAL)%STA_IND(1)), &
     &                         PIM%C_STA(PIM%OBS(IND_OBS_CAL)%STA_IND(2)), &
     &                         STR(1:30), &
     &                         D_GR_DEL(PIMA__CAL)*1.D9, &
     &                         D_PH_RAT(PIMA__CAL)*1.D15, &
     &                         D_PHS(PIMA__CAL)
 210          FORMAT ( 'PIMA_FRIP_APR_ADJ  ',A, '  IND_OBS: ', I6, &
     &                 ' Date: ', A, ' Sta: ', A, ' / ', A/ &
     &                 6X, ' D_ph_del: ', F12.6, ' ns ', &
     &                 6X, ' D_ph_rat: ' F12.3, ' fs/s', &
     &                 6X, ' D_phs: ', F8.5, ' rad' )
         END IF
!
! ------ Update fringe parameters for the calibrator
!
         PIM%OBS(IND_OBS_CAL)%RES_MB_DEL(IND_FRA,IND_BND) = &
     &       PIM%OBS(IND_OBS_CAL)%RES_MB_DEL(IND_FRA,IND_BND) - D_GR_DEL(PIMA__CAL)
         PIM%OBS(IND_OBS_CAL)%RES_PH_RAT(IND_FRA,IND_BND) = &
     &       PIM%OBS(IND_OBS_CAL)%RES_PH_RAT(IND_FRA,IND_BND) - D_PH_RAT(PIMA__CAL)
         PIM%OBS(IND_OBS_CAL)%RES_PHS(IND_FRA,IND_BND) =      - D_PHS(PIMA__CAL)
!
! ------ Rotate phases of the calibrator to adjust for the new a priori
!
! %%%%%%%%%%%%%%%%%%%%%%%%%
!  drf = 0.0 ! %%%%%%%%%%%%
!  drf_all = 0.0 ! %%%%%%%%
! %%%%%%%%%%%%%%%%%%%%%%%%%
         DO 420 J2=1,PIM%FRIP(PIMA__CAL)%OBS(J1)%NAP
            IFRQ = 0
            DT = (   PIM%OBS(IND_OBS_CAL)%TIM_BEG &
     &             + PIM%FRIP(PIMA__CAL)%OBS(J1)%TIM_AP(J2) ) &
                 - PIM%FRIP(PIMA__CAL)%TIM_EPC
            DO 430 J3=PIM%CONF%BEG_FRQ,PIM%CONF%END_FRQ
               DO 440 J4=1,PIM%NCHN
                  IFRQ = IFRQ + 1
                  PHS_MOD =    ( PIM%OBS(IND_OBS_CAL)%RES_PHS(IND_FRA,IND_BND) &
     &                           + D_PHS(PIMA__CAL) &
     &                         ) &
     &                       + ( PIM%OBS(IND_OBS_CAL)%RES_PH_RAT(IND_FRA,IND_BND) &
     &                           + D_PH_RAT(PIMA__CAL) &
     &                         )*PI2*FREQ_REF*DT &
     &                       + ( PIM%OBS(IND_OBS_CAL)%RES_MB_DEL(IND_FRA,IND_BND) &
     &                           + D_GR_DEL(PIMA__CAL) &
     &                         )*PI2*(PIM%FRIP(PIMA__CAL)%FRQ(IFRQ) - FREQ_REF)
!
                  PHS_MOD = PHS_MOD - PI2*IDNINT ( PHS_MOD/PI2 )
                  PIM%FRIP(PIMA__CAL)%OBS(J1)%VIS(IFRQ,J2) = &
     &                PIM%FRIP(PIMA__CAL)%OBS(J1)%VIS(IFRQ,J2)* &
     &                CMPLX ( COS(SNGL(PHS_MOD)), SIN(SNGL(PHS_MOD)) )
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!  drf(ifrq) = drf(ifrq) + pim%frip(pima__cal)%obs(j1)%vis(ifrq,j2) ! %%%%%%%%
!  drf_all   = drf_all   + pim%frip(pima__cal)%obs(j1)%vis(ifrq,j2) ! %%%%%%%%
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 440           CONTINUE 
 430        CONTINUE 
 420     CONTINUE 
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!            ifrq = 0
!            do 530 j3=pim%conf%beg_frq,pim%conf%end_frq
!               phs_avr = 0.0
!               do 540 j4=1,pim%nchn
!                  ifrq = ifrq + 1
!                  t8(ifrq) = (pim%frip(pima__cal)%frq(ifrq) - freq_ref)
!                  x8(ifrq) = phas_cmpl_r4 ( drf(ifrq) )
!                  phs_avr = x8(ifrq) + phs_avr 
! 540           continue 
!               write ( 6, 283 ) pim%frip(pima__cal)%obs(j1)%ind_obs, &
!     &                          j3, phs_avr/pim%nchn
! 283           format ( 'ZZZ ind_obs: ', i5, ' if: ', i2, ' phs_avr = ', f8.5 )
! 530        continue 
!     write ( 6, '(A,f8.5)' ) 'Overall phase: ', phas_cmpl_r4 ( drf_all )
!     call diagi_1 ( ifrq, t8, x8, -2 ) ! %%%%
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!
! ------ Learn index of the observation of the calibrator in PIM%FRIP that 
! ------ corresponds to the target
!
         IND_OBS_TAG = 0
         DO 450 J5=1,PIM%FRIP(PIMA__TAG)%NOBS
            IF ( PIM%OBS(PIM%FRIP(PIMA__TAG)%OBS(J5)%IND_OBS)%STA_IND(1) == &
     &           PIM%OBS(IND_OBS_CAL)%STA_IND(1) .AND. &
     &           PIM%OBS(PIM%FRIP(PIMA__TAG)%OBS(J5)%IND_OBS)%STA_IND(2) == &
     &           PIM%OBS(IND_OBS_CAL)%STA_IND(2)                                ) THEN
                 IND_OBS_TAG = PIM%FRIP(PIMA__TAG)%OBS(J5)%IND_OBS 
                 IND_TAG = J5
            END IF
 450     CONTINUE 
         IF ( IND_OBS_TAG == 0 ) THEN
              IF ( PIM%CONF%DEBUG_LEVEL > 0 ) THEN
                   WRITE ( 6, '(A)' ) 'PIMA_FRIP_AVR: Did not find a matchig '// &
     &                        'observation for baseline '// &
     &                         PIM%C_STA(PIM%OBS(IND_OBS_CAL)%STA_IND(1))// &
     &                        ' / '// &
     &                         PIM%C_STA(PIM%OBS(IND_OBS_CAL)%STA_IND(2))
                   GOTO 410
              END IF
         END IF
         CALL ERR_PASS ( IUER, IER )
!         CALL PIMA_SOU_THEO_DT ( PIM, VTD, &
!     &                           PIM%C_SOU(PIM%OBS(IND_OBS_TAG)%SOU_IND), &
!     &                           BAS_STA, TIM_SRT, FREQ_REF, &
!     &                           D_GR_DEL(PIMA__TAG), &
!     &                           D_PH_RAT(PIMA__TAG), &
!     &                           D_PHS(PIMA__TAG), &
!     &                           PIM%FRIP(PIMA__TAG)%OBS(IND_TAG)%UVW_SRT, IER )
         CALL PIMA_APR_THEO_DT ( PIM, VTD, IND_OBS_TAG, IND_BND, &
     &                           PIM%C_SOU(PIM%OBS(IND_OBS_TAG)%SOU_IND), &
     &                           BAS_STA, TIM_SRT, FREQ_REF, &
     &                           D_GR_DEL(PIMA__TAG), &
     &                           D_PH_RAT(PIMA__TAG), &
     &                           D_PHS(PIMA__TAG), &
     &                           PIM%FRIP(PIMA__TAG)%OBS(IND_TAG)%UVW_SRT, IER )
         IF ( IER .NE. 0 ) THEN
              CALL CLRCH ( STR )
              CALL INCH  ( IND_OBS_TAG, STR )
              CALL ERR_LOG ( 9714, IUER, 'PIMA_FRIP_APR_ADJ', 'Error in '// &
     &            'an attempt to compute theoretical path delay for '// &
     &            'observation '//STR )
              RETURN 
         END IF
         IF ( PIM%CONF%DEBUG_LEVEL .GE. 6 ) THEN
              WRITE ( 6, 230 ) PIMA__FRIP_SCATYP(PIMA__TAG), &
     &                         IND_OBS_TAG, &
     &                         PIM%C_STA(PIM%OBS(IND_OBS_TAG)%STA_IND(1)), &
     &                         PIM%C_STA(PIM%OBS(IND_OBS_TAG)%STA_IND(2)), &
     &                         PIM%OBS(IND_OBS_CAL)%RES_MB_DEL(IND_FRA,IND_BND)*1.D9, &
     &                         D_GR_DEL(PIMA__TAG)*1.D9, &
     &                         ( ( ADD_CLO(2) - ADD_CLO(1) )*0.0 + &
     &                           ( ADD_RAT(2) - ADD_RAT(1) )*TIM_SRT ) *1.D9
 230          FORMAT ( 'PIMA_FAA  Typ: ', A, ' Ind_obs: ', I5, &
     &                 ' Sta: ', A, ' / ', A, &
     &                 ' FS_gr_del: ', F12.4, &
     &                 ' TA_gr_del: ', F12.4, &
     &                 ' AC_gr_del: ', F12.4, ' ns ' )
         END IF
!
! ------ Set fringe parameters for the target 
!
         PIM%OBS(IND_OBS_TAG)%RES_MB_DEL(IND_FRA,IND_BND) = &
     &             PIM%OBS(IND_OBS_CAL)%RES_MB_DEL(IND_FRA,IND_BND) &
     &           + D_GR_DEL(PIMA__TAG)
         PIM%OBS(IND_OBS_TAG)%RES_PH_RAT(IND_FRA,IND_BND) = &
     &           PIM%OBS(IND_OBS_CAL)%RES_PH_RAT(IND_FRA,IND_BND) &
     &           + D_PH_RAT(PIMA__TAG)
         PIM%OBS(IND_OBS_TAG)%RES_PHS(IND_FRA,IND_BND)    = &
     &           PIM%OBS(IND_OBS_CAL)%RES_PHS(IND_FRA,IND_BND) &
     &           + D_PHS(PIMA__TAG) 
!
! ------ Rotate phases of the target 
!
         DO 460 J6=1,PIM%FRIP(PIMA__TAG)%OBS(IND_TAG)%NAP
            IFRQ = 0
            DT = (   PIM%OBS(IND_OBS_TAG)%TIM_BEG &
     &             + PIM%FRIP(PIMA__TAG)%OBS(IND_TAG)%TIM_AP(J6) ) &
                 - PIM%FRIP(PIMA__TAG)%TIM_EPC
            DO 470 J7=PIM%CONF%BEG_FRQ,PIM%CONF%END_FRQ
               DO 480 J8=1,PIM%NCHN
                  IFRQ = IFRQ + 1
                  PHS_MOD =   PIM%OBS(IND_OBS_TAG)%RES_PHS(IND_FRA,IND_BND) &
     &                      + PIM%OBS(IND_OBS_TAG)%RES_PH_RAT(IND_FRA,IND_BND)* &
     &                        PI2*FREQ_REF*DT &
     &                      + PIM%OBS(IND_OBS_TAG)%RES_MB_DEL(IND_FRA,IND_BND)* &
     &                        PI2*(PIM%FRIP(PIMA__TAG)%FRQ(IFRQ) - FREQ_REF)
!
                  PHS_MOD = PHS_MOD - PI2*IDNINT ( PHS_MOD/PI2 )
!
                  PIM%FRIP(PIMA__TAG)%OBS(IND_TAG)%VIS(IFRQ,J6) = &
     &                PIM%FRIP(PIMA__TAG)%OBS(IND_TAG)%VIS(IFRQ,J6)* &
     &                CMPLX ( COS (SNGL(PHS_MOD)), SIN(SNGL(PHS_MOD)) )
 480           CONTINUE 
 470        CONTINUE 
 460     CONTINUE 
!
         IF ( PIM%CONF%DEBUG_LEVEL.GE. 6 ) THEN
              STR = MJDSEC_TO_DATE ( PIM%MJD_0, PIM%TAI_0 + TIM_SRT, -2 )
              WRITE ( 6, 220 ) PIMA__FRIP_SCATYP(PIMA__TAG), IND_OBS_TAG, &
     &                         PIM%C_STA(PIM%OBS(IND_OBS_TAG)%STA_IND(1)), &
     &                         PIM%C_STA(PIM%OBS(IND_OBS_TAG)%STA_IND(2)), &
     &                         STR(1:30), &
     &                         D_GR_DEL(PIMA__TAG), D_PH_RAT(PIMA__TAG), &
     &                         D_PHS(PIMA__TAG)
 220          FORMAT ( 'PIMA_FRIP_APR_ADJ  ',A, '  IND_OBS: ', I6, &
     &                 ' Date: ', A, ' Sta: ', A, ' / ', A/ &
     &                 2X, ' Dif_ph_del: ', 1PD16.8,  ' Dif_ph_rat: ', 1PD16.8, &
     &                 ' Dif_phs: ', 0PF9.6 )
         END IF
 410  CONTINUE 
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE PIMA_FRIP_APR_ADJ  !#!  
!
! ------------------------------------------------------------------------
!
      SUBROUTINE PIMA_SOU_THEO_DT ( PIM, VTD, C_SOU, C_STA, TIM_SRT, &
     &                              FREQ_REF, D_DEL, D_RAT, D_PHS, UVW, &
     &                              IUER )
! ************************************************************************
! *                                                                      *
! *   Program PIMA_SOU_THEO_DT
! *                                                                      *
! * ### 12-FEB-2012 PIMA_SOU_THEO_DT  v1.0 (c) L. Petrov 12-FEB-2012 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'astro_constants.i'
      INCLUDE   'pima.i'
      INCLUDE   'vtd.i'
      TYPE     ( PIMA__TYPE  ) :: PIM
      TYPE     ( VTD__TYPE   ) :: VTD
      TYPE     ( VTD__OBS_TYPE  ) :: OBS_TYP
      CHARACTER  C_SOU*8, C_STA(2)*8
      REAL*8     TIM_SRT, FREQ_REF, D_DEL, D_RAT, D_PHS, UVW(3)
      INTEGER*4  IUER
      REAL*8     GR_DEL_1, PH_DEL_1, PH_RAT_1, GR_DEL_2, PH_DEL_2, PH_RAT_2, &
     &           DER_DEL(VTD__NDER), DER_RAT(VTD__NDER), GR_DEL_INP, &
     &           PH_RAT_INP, GR_DEL_NEW, PH_RAT_NEW
      INTEGER*4  IND_SOU_PIMA, IND_SOU_VTD, J1, J2, IER
      INTEGER*4, EXTERNAL :: VTD_SOU_INDEX, LTM_DIF, ILEN, I_LEN
!
      OBS_TYP%PLRZ       = 'RR'
      OBS_TYP%FRQ_REF(1) = FREQ_REF
      OBS_TYP%N_BND      = 1
      OBS_TYP%DELAY_TYPE = VTD__PL__DTP
      OBS_TYP%FRQ_ION_EFF(1) = FREQ_REF
      OBS_TYP%STATUS     = VTD__BND
!
      IND_SOU_VTD  = VTD_SOU_INDEX ( VTD, C_SOU )
      IND_SOU_PIMA = LTM_DIF ( 0, PIM%NSOU, PIM%C_SOU, C_SOU )
!
! --- Update the source coordinates that were used for correlation
!
      VTD%SOU(IND_SOU_VTD)%ALPHA = PIM%SOU(IND_SOU_PIMA)%ALPHA_INP
      VTD%SOU(IND_SOU_VTD)%DELTA = PIM%SOU(IND_SOU_PIMA)%DELTA_INP
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
           CALL ERR_LOG ( 9751, IUER, 'PIMA_SOU_THEO_DT', 'Error in an '// &
     &         'attempt to compute theoretical path delay' )
           RETURN 
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL VTD_DELAY ( C_SOU, 'GEOCENTR', C_STA(2), &
     &                 PIM%MJD_0, PIM%TAI_0 + TIM_SRT, OBS_TYP, VTD, &
     &                 GR_DEL_2, PH_RAT_2, DER_DEL, DER_RAT, IER )
      IF ( IER .NE. 0 ) THEN
          CALL ERR_LOG ( 9752, IUER, 'PIMA_SOU_THEO_DT', 'Error in an '// &
     &        'attempt to compute theoretical path delay' )
          RETURN 
      END IF
!
      GR_DEL_INP = GR_DEL_2 - GR_DEL_1
      PH_RAT_INP = PH_RAT_2 - PH_RAT_1
!
      VTD%SOU(IND_SOU_VTD)%ALPHA = PIM%SOU(IND_SOU_PIMA)%ALPHA
      VTD%SOU(IND_SOU_VTD)%DELTA = PIM%SOU(IND_SOU_PIMA)%DELTA
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
           CALL ERR_LOG ( 9753, IUER, 'PIMA_SOU_THEO_DT', 'Error in an '// &
     &         'attempt to compute theoretical path delay' )
           RETURN 
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL VTD_DELAY ( C_SOU, 'GEOCENTR', C_STA(2), &
     &                 PIM%MJD_0, PIM%TAI_0 + TIM_SRT, OBS_TYP, VTD, &
     &                 GR_DEL_2, PH_RAT_2, DER_DEL, DER_RAT, IER )
      IF ( IER .NE. 0 ) THEN
          CALL ERR_LOG ( 9754, IUER, 'PIMA_SOU_THEO_DT', 'Error in an '// &
     &        'attempt to compute theoretical path delay' )
          RETURN 
      END IF
!
      GR_DEL_NEW = GR_DEL_2 - GR_DEL_1
      PH_RAT_NEW = PH_RAT_2 - PH_RAT_1
!
      D_DEL = GR_DEL_NEW - GR_DEL_INP
      D_RAT = PH_RAT_NEW - PH_RAT_INP
      D_PHS = PI2*FREQ_REF*D_DEL 
      D_PHS = D_PHS - PI2*IDNINT ( D_PHS/PI2 )
!
      CALL ERR_PASS ( IUER, IER )
      CALL VTD_GET_UVW ( C_SOU, C_STA(1), C_STA(2), &
     &                   PIM%MJD_0, PIM%TAI_0 + TIM_SRT, &
     &                   FREQ_REF, VTD, UVW, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 9755, IUER, 'PIMA_SOU_THEO_DT', &
     &         'Error in an '// &
     &         'attempt to compute UV coordinates delay' )
           RETURN 
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  PIMA_SOU_THEO_DT  !#!#  
!
! ------------------------------------------------------------------------
!
      SUBROUTINE PIMA_APR_THEO_DT ( PIM, VTD, IND_OBS, IND_BND, C_SOU, &
     &                              C_STA, TIM_SRT, FREQ_REF, D_DEL, D_RAT, &
     &                              D_PHS, UVW, IUER )
! ************************************************************************
! *                                                                      *
! *   Program PIMA_APR_THEO_DT
! *                                                                      *
! * ### 12-FEB-2012 PIMA_APR_THEO_DT  v1.0 (c) L. Petrov 12-FEB-2012 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'astro_constants.i'
      INCLUDE   'pima.i'
      INCLUDE   'vtd.i'
      TYPE     ( PIMA__TYPE  ) :: PIM
      TYPE     ( VTD__TYPE   ) :: VTD
      TYPE     ( VTD__OBS_TYPE  ) :: OBS_TYP
      CHARACTER  C_SOU*8, C_STA(2)*8
      REAL*8     TIM_SRT, FREQ_REF, D_DEL, D_RAT, D_PHS, UVW(3)
      INTEGER*4  IND_OBS, IND_BND, IUER
      REAL*8     GR_DEL_1, PH_DEL_1, PH_RAT_1, GR_DEL_2, PH_DEL_2, PH_RAT_2, &
     &           DER_DEL(VTD__NDER), DER_RAT(VTD__NDER), &
     &           GR_DEL_NEW, PH_RAT_NEW
      CHARACTER  STR*128
      REAL*8     ADD_CLO(2), ADD_RAT(2), APR_GR_DEL, APR_PH_RAT
      INTEGER*4  IND_SOU_PIMA, IND_SOU_VTD, FRG_IND, J1, J2, IER
      INTEGER*4, EXTERNAL :: VTD_SOU_INDEX, LTM_DIF, ILEN, I_LEN
!
      FRG_IND = PIM%OBS(IND_OBS)%REF_FRG_INDS(PIM%CONF%FRQ_GRP)
      PIM%OBS(IND_OBS)%SRT_OFFSET = TIM_SRT - PIM%TIM_R8(PIM%UV_IND(PIM%OBS(IND_OBS)%UV_IND(1,FRG_IND))%TIM_IND)
      CALL ERR_PASS ( IUER, IER ) 
      CALL PIMA_APR_DELAY ( PIM, VTD, IND_OBS, IND_BND, 'OBS_SRT', &
     &                      'GEOCENTER', IER )
      IF ( IER .NE. 0 ) THEN
           CALL INCH  ( IND_OBS, STR )
           CALL ERR_LOG ( 9761, IUER, 'PIMA_APR_THEO_DT', 'Error in an '// &
     &         'attempt to compute apriori path delay for '// &
     &         'observation '//STR )
           RETURN 
      END IF
!
      ADD_CLO(1) = PIM%STA(PIM%OBS(IND_OBS)%STA_IND(1))%MDC%CLO_OFFS + &
     &             (PIM%TAI_0 + TIM_SRT)*PIM%STA(PIM%OBS(IND_OBS)%STA_IND(1))%MDC%CLO_RATE
      ADD_CLO(2) = PIM%STA(PIM%OBS(IND_OBS)%STA_IND(2))%MDC%CLO_OFFS + &
     &             (PIM%TAI_0 + TIM_SRT)*PIM%STA(PIM%OBS(IND_OBS)%STA_IND(2))%MDC%CLO_RATE
      ADD_RAT(1) = PIM%STA(PIM%OBS(IND_OBS)%STA_IND(1))%MDC%CLO_RATE
      ADD_RAT(2) = PIM%STA(PIM%OBS(IND_OBS)%STA_IND(2))%MDC%CLO_RATE
!      
      APR_GR_DEL =   (   PIM%OBS(IND_OBS)%APR_GC_DEL(2,IND_BND)       &
     &                 - PIM%OBS(IND_OBS)%APR_GC_DEL(1,IND_BND) )     &
     &             - (   PIM%OBS(IND_OBS)%CLO_OFFSET_APR(2,IND_BND)   &
     &                 - PIM%OBS(IND_OBS)%CLO_OFFSET_APR(1,IND_BND) ) &
     &             +     ( ADD_CLO(2) - ADD_CLO(1) ) 
      APR_PH_RAT =   (   PIM%OBS(IND_OBS)%APR_GC_RAT(2,IND_BND)     &
     &                 - PIM%OBS(IND_OBS)%APR_GC_RAT(1,IND_BND) )   &
     &             - (   PIM%OBS(IND_OBS)%CLO_RATE_APR(2,IND_BND)   &
     &                 - PIM%OBS(IND_OBS)%CLO_RATE_APR(1,IND_BND) ) &
     &             +     ( ADD_RAT(2) - ADD_RAT(1) ) 
      
!
      OBS_TYP%PLRZ       = 'RR'
      OBS_TYP%FRQ_REF(1) = FREQ_REF
      OBS_TYP%N_BND      = 1
      OBS_TYP%DELAY_TYPE = VTD__PL__DTP
      OBS_TYP%FRQ_ION_EFF(1) = FREQ_REF
      OBS_TYP%STATUS     = VTD__BND
!
      IND_SOU_VTD  = VTD_SOU_INDEX ( VTD, C_SOU )
      IND_SOU_PIMA = LTM_DIF ( 0, PIM%NSOU, PIM%C_SOU, C_SOU )
!
! --- Initialization to avoid using stale intermediary quantities
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
           CALL ERR_LOG ( 9762, IUER, 'PIMA_APR_THEO_DT', 'Error in an '// &
     &         'attempt to compute theoretical path delay' )
           RETURN 
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL VTD_DELAY ( C_SOU, 'GEOCENTR', C_STA(2), &
     &                 PIM%MJD_0, PIM%TAI_0 + TIM_SRT, OBS_TYP, VTD, &
     &                 GR_DEL_2, PH_RAT_2, DER_DEL, DER_RAT, IER )
      IF ( IER .NE. 0 ) THEN
          CALL ERR_LOG ( 9763, IUER, 'PIMA_APR_THEO_DT', 'Error in an '// &
     &        'attempt to compute theoretical path delay' )
          RETURN 
      END IF
!
      GR_DEL_NEW = GR_DEL_2 - GR_DEL_1
      PH_RAT_NEW = PH_RAT_2 - PH_RAT_1
!
      D_DEL = GR_DEL_NEW - APR_GR_DEL
      D_RAT = PH_RAT_NEW - APR_PH_RAT
      D_PHS = PI2*FREQ_REF*D_DEL 
      D_PHS = D_PHS - PI2*IDNINT ( D_PHS/PI2 )
!
      CALL ERR_PASS ( IUER, IER )
      CALL VTD_GET_UVW ( C_SOU, C_STA(1), C_STA(2), &
     &                   PIM%MJD_0, PIM%TAI_0 + TIM_SRT, &
     &                   FREQ_REF, VTD, UVW, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 9764, IUER, 'PIMA_APR_THEO_DT', &
     &         'Error in an '// &
     &         'attempt to compute UV coordinates delay' )
           RETURN 
      END IF
!%%%
!%  write ( 6, * ) ' apr_gr_del = ', apr_gr_del, ' gr_del_new= ', gr_del_new ! %%
!%  write ( 6, * ) ' d_del = ', d_del, ' d1= ',  - ( pim%obs(ind_obs)%clo_offset_apr(2,ind_bnd)   &
!%     &                 - pim%obs(ind_obs)%clo_offset_apr(1,ind_bnd) ), &
!%     &           ' d2= ', ( add_clo(2) - add_clo(1) ) 
!%  call pause ( 'dsda' ) ! %%%%%%%%%%%
!%%%
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  PIMA_APR_THEO_DT  !#!#  
