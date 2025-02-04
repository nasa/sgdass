      SUBROUTINE PIMA_GET_RESID ( PIM, VTD, IND_OBS, IND_POL, LCHN, &
     &                            LFRQ, LTIM, GR_DEL, PH_RAT, GR_RAT, PHAS, &
     &                            SNR, TIME_FRT, FRIB_SEARCH_TYPE, &
     &                            BPASS_STYLE, POL_MODE, &
     &                            RES, RES_AVR, SNR_NEW, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  PIMA_GET_RESID
! *                                                                      *
! * ### 25-JAN-2009 PIMA_GET_RESID v2.0 (c) L. Petrov  21-NOV-2018  ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'astro_constants.i'
      INCLUDE   'pima.i'
      INCLUDE   'vtd.i'
      TYPE     ( PIMA__TYPE         ) :: PIM
      TYPE     ( VTD__TYPE          ) :: VTD
      INTEGER*4  IND_OBS, IND_POL, LCHN, LFRQ, LTIM, MASK_TYPE, POL_MODE, IUER
      REAL*8     GR_DEL(PIM__MFRA), PH_RAT(PIM__MFRA), GR_RAT, &
     &           PHAS(PIM__MFRA), SNR, TIME_FRT, SNR_NEW
      CHARACTER  FRIB_SEARCH_TYPE*(*), BPASS_STYLE*(*)
      REAL*8     FREQ_REF, AMPL(PIM__MFRA), PHAS_ADD_R8
      REAL*4     WEI_SUM, WEI_SUM_CHN, PHAS_R4
      COMPLEX*8  RES(LCHN,PIM%NFRQ,2,2), RES_AVR(2,2)
      COMPLEX*8  DRF(2,2)
      REAL*4     WEI_USE
      REAL*8     SEFD_FRQ, EFF_DURA, TIM_UV, &
     &           GR_DEL_ERR(PIM__MFRA), PH_RAT_ERR(PIM__MFRA), &
     &           PH_DEL_ERR(PIM__MFRA), GR_RAT_ERR, GRAMBSP, &
     &           PHAS_ERR(PIM__MFRA), EFF_FRQ_PHS, EFF_FRQ_GRP, &
     &           EFF_FRQ_RAT, FREQ_DIF, COV_PR_PH, COV_GR_MD, &
     &           FEED_ANG_DIF, PH_ACC, PH_ACC_ERR, &
     &           TEC, TEC_RATE, TEC_ERR, TEC_RATE_ERR, DECOR_TIM
      CHARACTER  STR*128, STR1*128, FINAM_2D_PLOT*128
      INTEGER*4  IFRQ, IP, DEB_LEV_SAVE, KSMP, IND_STA(2), IND_FRA, UV_IND, &
     &           J1, J2, J3, J4, J5, J6, J7, J8, J9, J10, J11, FRG_IND, &
     &           FRI_STS, LPOL, IER
      INTEGER*4  WG_L, WG_H, WR_L, WR_H
      INTEGER*1  MASK_CHN 
      LOGICAL*4  FL_BMASK
      REAL*4     BPS_AMP, BPS_PHS
      REAL*4,    EXTERNAL :: PHAS_CMPL_R4
      LOGICAL*4, EXTERNAL :: IS_R4_NAN
      INTEGER*4, EXTERNAL :: ILEN, I_LEN
      CHARACTER, EXTERNAL :: MJDSEC_TO_DATE*30
!
      MASK_TYPE = PIMA__MASK_BPAS
!
      FINAM_2D_PLOT = '/tmp/plot.gif'
      PIM%CONF%FRIB_2D_FRINGE_PLOT = PIMA__PLOT_NO
      IF ( PIM%CONF%FRIB_FINE_SEARCH == PIMA__FINE_SEARCH_LSQ ) THEN
           IND_FRA = PIMA__LSQ
         ELSE
           IND_FRA = PIMA__DRF
      END IF
!
      FRG_IND = PIM%OBS(IND_OBS)%REF_FRG_INDS(PIM%CONF%FRQ_GRP)
      IF ( FRG_IND .LE. 0 ) THEN
           CALL CLRCH ( STR  )
           CALL CLRCH ( STR1 )
           CALL INCH  ( IND_OBS, STR  )
           CALL INCH  ( FRG_IND, STR1 )
           CALL ERR_LOG ( 6361, IUER, 'PIMA_GET_RESID', 'Trap of internal '// &
     &         'control: observation '//STR(1:I_LEN(STR))//' does not '// &
     &         'have APs with frequency index '//STR1 )
           RETURN 
      END IF
!
      IF ( POL_MODE == PIMA__PALL_NOR   .OR. &
     &     POL_MODE == PIMA__PALL_XY    .OR. &
     &     POL_MODE == PIMA__PALL_1ST   .OR. &
     &     POL_MODE == PIMA__PALL_2ND   .OR. &
     &     POL_MODE == PIMA__PALL_MIXED      ) THEN
           LPOL = 4
         ELSE IF ( POL_MODE == PIMA__PAR             ) THEN
           LPOL = 2
         ELSE 
           LPOL = 1
      END IF
!
! --- Get UV data and put them in PIM%OBS(IND_OBS)%UV, PIM%OBS(IND_OBS)%UV_IF, PIM%OBS(IND_OBS)%UV_BAND
!
      CALL ERR_PASS ( IUER, IER )
      CALL PIMA_GET_OBS ( PIM, VTD, IND_OBS, POL_MODE, LPOL, &
     &                    .TRUE., .TRUE., .FALSE., FRI_STS, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 6362, IUER, 'PIMA_GET_RESID', 'Error in getting '// &
     &         'visibility data' )
           RETURN 
      END IF
!
! --- Proceed with fringe fitting
!
      FREQ_REF = PIM%FREQ_ARR(1,PIM%CONF%BEG_FRQ,PIM%CONF%FRQ_GRP)
      DEB_LEV_SAVE = PIM%CONF%DEBUG_LEVEL
      PIM%CONF%DEBUG_LEVEL = 0
      IF ( FRIB_SEARCH_TYPE == PIMA__2FFT ) THEN
!
! -------- Run fringe fitting
!
           CALL ERR_PASS ( IUER, IER )
           CALL PIMA_2FFT ( PIM, VTD, IND_OBS, LCHN, LFRQ, LTIM, &
     &                      PIM%FREQ_ARR(1,PIM%CONF%BEG_FRQ,PIM%CONF%FRQ_GRP), &
     &                      FREQ_REF, PIM%OBS(IND_OBS)%UV(1,1,1,IND_POL), &
     &                      PIM%OBS(IND_OBS)%WEI_1D, PIM%OBS(IND_OBS)%AP_LEN, &
     &                      FINAM_2D_PLOT, TIME_FRT, GR_DEL, PH_RAT, GR_RAT, &
     &                      PH_ACC, PHAS, AMPL, &
     &                      SNR_NEW, GR_DEL_ERR, PH_DEL_ERR, PH_RAT_ERR, &
     &                      GR_RAT_ERR, PH_ACC_ERR, PHAS_ERR, GRAMBSP, &
     &                      EFF_FRQ_PHS, EFF_FRQ_GRP, EFF_FRQ_RAT, &
     &                      COV_PR_PH, COV_GR_MD, EFF_DURA, &
     &                      TEC, TEC_RATE, TEC_ERR, TEC_RATE_ERR, DECOR_TIM, IER )
           IF ( IER .NE. 0 ) THEN
                CALL CLRCH ( STR )
                CALL INCH  ( IND_OBS, STR )
                CALL ERR_LOG ( 6364, IUER, 'PIMA_GET_RESID', 'Failure in '// &
     &               'attempt to fringe search in observation '//STR )
                RETURN
           END IF
         ELSE IF ( FRIB_SEARCH_TYPE == PIMA__FRIB_NO ) THEN
           CONTINUE
           SNR_NEW = SNR
           IER = 0
         ELSE IF ( FRIB_SEARCH_TYPE == PIMA__FRIB_SUBS ) THEN
           CONTINUE
           SNR_NEW = SNR
           IER = 0
         ELSE IF ( FRIB_SEARCH_TYPE == PIMA__POLAR_I ) THEN
           CALL ERR_PASS ( IUER, IER )
           CALL PIMA_2FFT ( PIM, VTD, IND_OBS, LCHN, LFRQ, LTIM, &
     &                      PIM%FREQ_ARR(1,PIM%CONF%BEG_FRQ,PIM%CONF%FRQ_GRP), &
     &                      FREQ_REF, PIM%OBS(IND_OBS)%UV(1,1,1,IND_POL), &
     &                      PIM%OBS(IND_OBS)%WEI_1D, PIM%OBS(IND_OBS)%AP_LEN, &
     &                      FINAM_2D_PLOT, TIME_FRT, GR_DEL, PH_RAT, GR_RAT, &
     &                      PH_ACC, PHAS, AMPL, &
     &                      SNR_NEW, GR_DEL_ERR, PH_DEL_ERR, PH_RAT_ERR, &
     &                      GR_RAT_ERR, PH_ACC_ERR, PHAS_ERR, GRAMBSP, &
     &                      EFF_FRQ_PHS, EFF_FRQ_GRP, EFF_FRQ_RAT, &
     &                      COV_PR_PH, COV_GR_MD, EFF_DURA, &
     &                      TEC, TEC_RATE, TEC_ERR, TEC_RATE_ERR, DECOR_TIM, IER )
           IF ( IER .NE. 0 ) THEN
                CALL CLRCH ( STR )
                CALL INCH  ( IND_OBS, STR )
                CALL ERR_LOG ( 6364, IUER, 'PIMA_GET_RESID', 'Failure in '// &
     &               'attempt to fringe search in observation '//STR )
                RETURN
           END IF
!
           PIM%CONF%DEBUG_LEVEL = DEB_LEV_SAVE 
           CALL ERR_LOG ( 0, IUER )        
           RETURN 
         ELSE IF ( FRIB_SEARCH_TYPE == PIMA__POLAR_ALL ) THEN
           DO 410 J1=1,PIM__MPLR
              IF ( J1 == 1 ) THEN
                   PIM%OBS(IND_OBS)%POLAR_USED = PIM%OBS(IND_OBS)%POLARIZ(1)(1:1)//PIM%OBS(IND_OBS)%POLARIZ(1)(1:1)
                 ELSE IF ( J1 == 2 ) THEN
                   PIM%OBS(IND_OBS)%POLAR_USED = PIM%OBS(IND_OBS)%POLARIZ(1)(2:2)//PIM%OBS(IND_OBS)%POLARIZ(2)(1:1)
                 ELSE IF ( J1 == 3 ) THEN
                   PIM%OBS(IND_OBS)%POLAR_USED = PIM%OBS(IND_OBS)%POLARIZ(1)(1:1)//PIM%OBS(IND_OBS)%POLARIZ(2)(2:2)
                 ELSE IF ( J1 == 4 ) THEN
                   PIM%OBS(IND_OBS)%POLAR_USED = PIM%OBS(IND_OBS)%POLARIZ(1)(2:2)//PIM%OBS(IND_OBS)%POLARIZ(2)(2:2)
              END IF
!
              IF ( PIM%OBS(IND_OBS)%POLAR_USED .NE. 'RL' .AND. &
     &             PIM%OBS(IND_OBS)%POLAR_USED .NE. 'LR'       ) THEN
                   IF ( PIM%OBS(IND_OBS)%POLAR_USED(1:1) == 'H' .OR. &
     &                  PIM%OBS(IND_OBS)%POLAR_USED(1:1) == 'R' ) PIM%OBS(IND_OBS)%POLAR_USED(1:1) = 'X'
                   IF ( PIM%OBS(IND_OBS)%POLAR_USED(1:1) == 'V' .OR. &
     &                  PIM%OBS(IND_OBS)%POLAR_USED(1:1) == 'L' ) PIM%OBS(IND_OBS)%POLAR_USED(1:1) = 'Y'
!
                   IF ( PIM%OBS(IND_OBS)%POLAR_USED(2:2) == 'H' .OR. &
     &                  PIM%OBS(IND_OBS)%POLAR_USED(2:2) == 'R' ) PIM%OBS(IND_OBS)%POLAR_USED(2:2) = 'X'
                   IF ( PIM%OBS(IND_OBS)%POLAR_USED(2:2) == 'V' .OR. &
     &                  PIM%OBS(IND_OBS)%POLAR_USED(2:2) == 'L' ) PIM%OBS(IND_OBS)%POLAR_USED(2:2) = 'Y'
              END IF
!
              CALL ERR_PASS ( IUER, IER )
              CALL PIMA_2FFT ( PIM, VTD, IND_OBS, LCHN, LFRQ, LTIM, &
     &                         PIM%FREQ_ARR(1,PIM%CONF%BEG_FRQ,PIM%CONF%FRQ_GRP), &
     &                         FREQ_REF, PIM%OBS(IND_OBS)%UV(1,1,1,J1), &
     &                         PIM%OBS(IND_OBS)%WEI_1D, PIM%OBS(IND_OBS)%AP_LEN, &
     &                         FINAM_2D_PLOT, TIME_FRT, GR_DEL, PH_RAT, GR_RAT, &
     &                         PH_ACC, PHAS, AMPL, &
     &                         SNR_NEW, GR_DEL_ERR, PH_DEL_ERR, PH_RAT_ERR, &
     &                         GR_RAT_ERR, PH_ACC_ERR, PHAS_ERR, GRAMBSP, &
     &                         EFF_FRQ_PHS, EFF_FRQ_GRP, EFF_FRQ_RAT, &
     &                         COV_PR_PH, COV_GR_MD, EFF_DURA, &
     &                         TEC, TEC_RATE, TEC_ERR, TEC_RATE_ERR, DECOR_TIM, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( IND_OBS, STR )
                   CALL ERR_LOG ( 6365, IUER, 'PIMA_GET_RESID', 'Failure in '// &
     &                  'attempt to fringe search in observation '//STR )
                   RETURN
              END IF
 410       CONTINUE 
!
! -------- Temporarily
!
           CALL ERR_LOG ( 0, IUER )        
           RETURN 
         ELSE
           CALL ERR_LOG ( 6363, IUER, 'PIMA_GET_RESID', 'Unsupported '// &
     &         'value of argument FRIB_SEARCH_TYPE: '//FRIB_SEARCH_TYPE )
           RETURN
      END IF
      PIM%CONF%DEBUG_LEVEL = DEB_LEV_SAVE
!
      IF ( PIM%CONF%DEBUG_LEVEL .GE. 3  .AND. &
     &     FRIB_SEARCH_TYPE .NE. PIMA__FRIB_SUBS  )  THEN
!
           IER = -2
           IF ( SNR_NEW  < 0.5*SNR ) THEN
                STR(1:1) = 'B'
              ELSE IF ( SNR_NEW < 0.9*SNR ) THEN
                STR(1:1) = 'b'
              ELSE IF ( SNR_NEW > 1.5*SNR ) THEN
                STR(1:1) = 'G'
              ELSE IF ( SNR_NEW > 1.1*SNR ) THEN
                STR(1:1) = 'g'
              ELSE
                STR(1:1) = ' '
           END IF
           WRITE ( 6, 210 ) IND_OBS, &
     &             PIM%STA(PIM%OBS(IND_OBS)%STA_IND(1))%IVS_NAME, &
     &             PIM%STA(PIM%OBS(IND_OBS)%STA_IND(2))%IVS_NAME, &
     &             PIM%SOU(PIM%OBS(IND_OBS)%SOU_IND)%IVS_NAME, &
                   ABS(SNGL(SNR)), SNGL(SNR_NEW), STR(1:1)
 210       FORMAT ( 'Obs: ',I6, 2X, A, '/', A, 2X, A, 2X, &
     &              'SNR old: ', F8.1, 1X, ' new: ', F8.1, 2X, A )
           IF ( SNR_NEW < (1.0D0 - 1.D-5)*SNR ) SNR = -SNR
           IF ( PIM%CONF%DEBUG_LEVEL .GE. 6 ) THEN
                WRITE ( 6, 220 ) IND_OBS, GR_DEL(IND_FRA), PH_RAT(IND_FRA), &
     &                           GR_RAT, PHAS(IND_FRA)
 220            FORMAT ( 'PIMA_GET_RESID IND_OBS: ', I6, ' Gr_del: ', 1PD19.12, &
     &                   ' Ph_rat: ', 1PD19.12, ' Gr_rat: ', 1PD15.8, ' Phas: ', 0PF10.7 )
           END IF
      END IF
!
      DRF     = CMPLX ( 0.0, 0.0 )
      RES_AVR(1,1) = CMPLX ( 0.0, 0.0 )
      IF ( POL_MODE == PIMA__PAR        .OR. &
     &     POL_MODE == PIMA__PALL_NOR   .OR. &
     &     POL_MODE == PIMA__PALL_XY    .OR. &
     &     POL_MODE == PIMA__PALL_1ST   .OR. &
     &     POL_MODE == PIMA__PALL_2ND   .OR. &
     &     POL_MODE == PIMA__PALL_MIXED      ) THEN
           RES_AVR = CMPLX ( 0.0, 0.0 )
      END IF 
      IFRQ = 0
!
      KSMP = 0
      DO 480 J8=PIM%CONF%BEG_FRQ,PIM%CONF%END_FRQ
         IFRQ = IFRQ + 1
         DO 490 J9=1,LCHN ! cycle over spectral channels
            IF ( IFRQ == 1 .AND.  J9 == 1 ) THEN
                 FREQ_REF = PIM%FREQ_ARR(1,PIM%CONF%BEG_FRQ,PIM%CONF%FRQ_GRP)
            END IF
            RES(J9,IFRQ,1,1) = ( 0.0, 0.0 )
            IF ( POL_MODE == PIMA__PAR ) THEN
                 RES(J9,IFRQ,2,1) = ( 0.0, 0.0 )
                 RES_AVR(2,1)     = 0.0
               ELSE IF ( POL_MODE == PIMA__PALL_NOR   .OR. &
     &                   POL_MODE == PIMA__PALL_XY    .OR. &
     &                   POL_MODE == PIMA__PALL_1ST   .OR. &
     &                   POL_MODE == PIMA__PALL_2ND   .OR. &
     &                   POL_MODE == PIMA__PALL_MIXED      ) THEN
                 RES(J9,IFRQ,2,1) = ( 0.0, 0.0 )
                 RES(J9,IFRQ,1,2) = ( 0.0, 0.0 )
                 RES(J9,IFRQ,2,2) = ( 0.0, 0.0 )
                 RES_AVR(2,1)     =   0.0
                 RES_AVR(1,2)     =   0.0
                 RES_AVR(2,2)     =   0.0
            END IF
!
! --------- Get amplitude bandpass calibration
!
            IF ( BPASS_STYLE == PIMA__BPASS_AMP_PHS  .OR. &
     &           BPASS_STYLE == PIMA__BPASS_AMP           ) THEN
                 BPS_AMP = ABS(PIM%BPS%CMPL(J9,J8,PIM%OBS(IND_OBS)%STA_IND(1)))* &
     &                     ABS(PIM%BPS%CMPL(J9,J8,PIM%OBS(IND_OBS)%STA_IND(2)))
               ELSE
                 BPS_AMP = 1.0
            END IF
            DO 4100 J10=1,LTIM ! cycle over time
               IF ( PIM%OBS(IND_OBS)%WEI_1D(J10,1) < PIM%CONF%FRIB_WEIGHTS_THRESHOLD ) GOTO 4100
               DO 4110 J11=1,PIM%OBS(IND_OBS)%NUVS
                  UV_IND  = PIM%OBS(IND_OBS)%UV_IND(J10,J11)
                  IF ( UV_IND > 0 ) GOTO 8110
 4110          CONTINUE
 8110          CONTINUE
               TIM_UV = (PIM%TIM_R8(PIM%UV_IND(UV_IND)%TIM_IND) - &
     &                  PIM%OBS(IND_OBS)%TIM_BEG)
!
               IF ( FRIB_SEARCH_TYPE .NE. PIMA__FRIB_NO ) THEN
                    FREQ_DIF = PIM%FREQ_ARR(J9,J8,PIM%CONF%FRQ_GRP) - FREQ_REF
                    PHAS_ADD_R8 = -PHAS(IND_FRA)*0.0 &
     &                           + PH_RAT(IND_FRA)*PI2*FREQ_REF*(TIM_UV - TIME_FRT) &
     &                           + GR_DEL(IND_FRA)*PI2*FREQ_DIF &
     &                           + GR_RAT*PI2*FREQ_DIF*(TIM_UV - TIME_FRT)
! 
                    DRF(1,1) = DRF(1,1) + PIM%OBS(IND_OBS)%UV(J9,IFRQ,J10,1)*PIM%OBS(IND_OBS)%WEI_1D(J10,1)* &
     &                                    CMPLX ( COS(PHAS_ADD_R8), SIN(PHAS_ADD_R8) )
                    RES(J9,IFRQ,1,1) = RES(J9,IFRQ,1,1) + PIM%OBS(IND_OBS)%UV(J9,IFRQ,J10,1)* &
     &                                                    PIM%OBS(IND_OBS)%WEI_1D(J10,1)* &
     &                                                    CMPLX ( COS(PHAS_ADD_R8), SIN(PHAS_ADD_R8) )/ &
     &                                                    LTIM/BPS_AMP
                    IF ( POL_MODE == PIMA__PAR ) THEN
                         DRF(2,1) = DRF(2,1) + PIM%OBS(IND_OBS)%UV(J9,IFRQ,J10,2)* &
     &                                             PIM%OBS(IND_OBS)%WEI_1D(J10,2)* &
     &                                                 CMPLX ( COS(PHAS_ADD_R8), SIN(PHAS_ADD_R8) )
                         RES(J9,IFRQ,2,1) = RES(J9,IFRQ,2,1) + PIM%OBS(IND_OBS)%UV(J9,IFRQ,J10,2)* &
     &                                                         PIM%OBS(IND_OBS)%WEI_1D(J10,2)* &
     &                                                         CMPLX ( COS(PHAS_ADD_R8), SIN(PHAS_ADD_R8) )/ &
     &                                                         LTIM/BPS_AMP
                      ELSE IF ( POL_MODE == PIMA__PALL_NOR   .OR. &
     &                          POL_MODE == PIMA__PALL_XY    .OR. &
     &                          POL_MODE == PIMA__PALL_1ST   .OR. &
     &                          POL_MODE == PIMA__PALL_2ND   .OR. &
     &                          POL_MODE == PIMA__PALL_MIXED      ) THEN
                         DRF(2,1) = DRF(2,1) + PIM%OBS(IND_OBS)%UV(J9,IFRQ,J10,2)*PIM%OBS(IND_OBS)%WEI_1D(J10,2)* &
     &                                             CMPLX ( COS(PHAS_ADD_R8), SIN(PHAS_ADD_R8) )
                         DRF(1,2) = DRF(1,2) + PIM%OBS(IND_OBS)%UV(J9,IFRQ,J10,3)*PIM%OBS(IND_OBS)%WEI_1D(J10,3)* &
     &                                             CMPLX ( COS(PHAS_ADD_R8), SIN(PHAS_ADD_R8) )
                         DRF(2,2) = DRF(2,2) + PIM%OBS(IND_OBS)%UV(J9,IFRQ,J10,4)*PIM%OBS(IND_OBS)%WEI_1D(J10,4)* &
     &                                             CMPLX ( COS(PHAS_ADD_R8), SIN(PHAS_ADD_R8) )
                         RES(J9,IFRQ,2,1) = RES(J9,IFRQ,2,1) + PIM%OBS(IND_OBS)%UV(J9,IFRQ,J10,2)* &
     &                                                         PIM%OBS(IND_OBS)%WEI_1D(J10,2)* &
     &                                                         CMPLX ( COS(PHAS_ADD_R8), SIN(PHAS_ADD_R8) )/ &
     &                                                         LTIM/BPS_AMP
                         RES(J9,IFRQ,1,2) = RES(J9,IFRQ,1,2) + PIM%OBS(IND_OBS)%UV(J9,IFRQ,J10,3)* &
     &                                                         PIM%OBS(IND_OBS)%WEI_1D(J10,3)* &
     &                                                         CMPLX ( COS(PHAS_ADD_R8), SIN(PHAS_ADD_R8) )/ &
     &                                                         LTIM/BPS_AMP
                         RES(J9,IFRQ,2,2) = RES(J9,IFRQ,2,2) + PIM%OBS(IND_OBS)%UV(J9,IFRQ,J10,4)* &
     &                                                         PIM%OBS(IND_OBS)%WEI_1D(J10,4)* &
     &                                                         CMPLX ( COS(PHAS_ADD_R8), SIN(PHAS_ADD_R8) )/ &
     &                                                         LTIM/BPS_AMP
                    END IF
                  ELSE !! FRIB_SEARCH_TYPE == PIMA__FRIB_NO
                    DRF(1,1) = DRF(1,1) + PIM%OBS(IND_OBS)%UV(J9,IFRQ,J10,1)*PIM%OBS(IND_OBS)%WEI_1D(J10,1)
                    RES(J9,IFRQ,1,1) = RES(J9,IFRQ,1,1) + PIM%OBS(IND_OBS)%UV(J9,IFRQ,J10,1)* &
     &                                                    PIM%OBS(IND_OBS)%WEI_1D(J10,1)/ &
     &                                                    LTIM/BPS_AMP
                    IF ( POL_MODE == PIMA__PAR ) THEN
                         DRF(2,1) = DRF(2,1) + PIM%OBS(IND_OBS)%UV(J9,IFRQ,J10,2)*PIM%OBS(IND_OBS)%WEI_1D(J10,2)
                         RES(J9,IFRQ,2,1) = RES(J9,IFRQ,2,1) + PIM%OBS(IND_OBS)%UV(J9,IFRQ,J10,2)* &
     &                                                         PIM%OBS(IND_OBS)%WEI_1D(J10,2)/ &
     &                                                         LTIM/BPS_AMP
                      ELSE IF ( POL_MODE == PIMA__PALL_NOR  .OR. &
     &                          POL_MODE == PIMA__PALL_XY   .OR. &
     &                          POL_MODE == PIMA__PALL_1ST  .OR. &
     &                          POL_MODE == PIMA__PALL_2ND  .OR. &
     &                          POL_MODE == PIMA__PALL_MIXED     ) THEN
                         DRF(2,1) = DRF(2,1) + PIM%OBS(IND_OBS)%UV(J9,IFRQ,J10,2)*PIM%OBS(IND_OBS)%WEI_1D(J10,2)
                         DRF(1,2) = DRF(1,2) + PIM%OBS(IND_OBS)%UV(J9,IFRQ,J10,3)*PIM%OBS(IND_OBS)%WEI_1D(J10,3)
                         DRF(2,2) = DRF(2,2) + PIM%OBS(IND_OBS)%UV(J9,IFRQ,J10,4)*PIM%OBS(IND_OBS)%WEI_1D(J10,4)
                         RES(J9,IFRQ,2,1) = RES(J9,IFRQ,2,1) + PIM%OBS(IND_OBS)%UV(J9,IFRQ,J10,2)* &
     &                                                         PIM%OBS(IND_OBS)%WEI_1D(J10,2)/ &
     &                                                         LTIM/BPS_AMP
                         RES(J9,IFRQ,1,2) = RES(J9,IFRQ,1,2) + PIM%OBS(IND_OBS)%UV(J9,IFRQ,J10,3)* &
     &                                                         PIM%OBS(IND_OBS)%WEI_1D(J10,3)/ &
     &                                                         LTIM/BPS_AMP
                         RES(J9,IFRQ,2,2) = RES(J9,IFRQ,2,2) + PIM%OBS(IND_OBS)%UV(J9,IFRQ,J10,4)* &
     &                                                         PIM%OBS(IND_OBS)%WEI_1D(J10,4)/ &
     &                                                         LTIM/BPS_AMP
                    END IF
               END IF
               IF ( FL_BMASK ) THEN
                    MASK_CHN = PIM%BANDPASS_MASK(J9,J8,PIM%OBS(IND_OBS)%STA_IND(1),MASK_TYPE) * &
     &                         PIM%BANDPASS_MASK(J9,J8,PIM%OBS(IND_OBS)%STA_IND(2),MASK_TYPE)
                    KSMP = KSMP + MASK_CHN
                  ELSE
                    KSMP = KSMP + 1
               END IF
 4100        CONTINUE
 490     CONTINUE
 480  CONTINUE
      IF ( POL_MODE == PIMA__PALL_NOR   .OR. &
     &     POL_MODE == PIMA__PALL_XY    .OR. &
     &     POL_MODE == PIMA__PALL_1ST   .OR. &
     &     POL_MODE == PIMA__PALL_2ND   .OR. &
     &     POL_MODE == PIMA__PALL_MIXED      ) THEN
           IF ( KSMP > 0 ) THEN
                RES_AVR = DRF/KSMP
              ELSE
                RES_AVR = 0.0
           END IF
         ELSE IF ( POL_MODE == PIMA__PAR ) THEN
           IF ( KSMP > 0 ) THEN
                RES_AVR(1,1) = DRF(1,1)/KSMP
                RES_AVR(2,1) = DRF(1,1)/KSMP
              ELSE
                RES_AVR(1,1) = 0.0
                RES_AVR(2,1) = 0.0
           END IF
         ELSE
           IF ( KSMP > 0 ) THEN
                RES_AVR(1,1) = DRF(1,1)/KSMP
              ELSE
                RES_AVR(1,1) = 0.0
           END IF
      END IF
!
      IF ( PIM%CONF%DEBUG_LEVEL .GE. 1  .AND. &
     &     DABS(SNR_NEW) < PIM%CONF%FRIB_SNR_DETECTION ) THEN
!
           WRITE ( 6, 230 ) IND_OBS, SNR_NEW, PIM%CONF%FRIB_SNR_DETECTION           
 230       FORMAT ( 'PIMA_GET_RESID Observation ', I5, ' has too low ', &
     &              'SNR: ', F8.2, ' -- less than the detection limit ', &
     &               F5.2 )
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  PIMA_GET_RESID  !#!#
