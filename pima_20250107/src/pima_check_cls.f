      SUBROUTINE PIMA_CHECK_CLS ( PIM, IND_SCA, IND_FRQ, C_STA1, C_STA2, &
     &                            C_STA3, MSEG_TIM, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine PIMA_CHECK_CLS generates the plots of phase misclosure     *
! *                                                                      *
! * ### 27-SEP-2013 PIMA_CHECK_CLS v1.0 (c)  L. Petrov  27-SEP-2013 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'astro_constants.i'
      INCLUDE   'pima.i'
      TYPE     ( PIMA__TYPE     ) :: PIM
      INTEGER*4  IND_SCA, MSEG_TIM, IUER
      CHARACTER  C_STA1*(*), C_STA2*(*), C_STA3*(*)
      CHARACTER  STR*128, STR1*128
      REAL*8     T1(PIM__MUV), X1(PIM__MUV), E1(PIM__MUV), AP_LEN, TIM_VAL, &
     &           FREQ_REF, FREQ_DIF, TIME_FRT, TIME_SRT, TIM_SUM
      REAL*4     WEI_1D(PIM__MUV), PHS(PIM__MUV,3), TIM(PIM__MUV,3), PHS_MOD
      INTEGER*4  J1, J2, J3, J4, J5, J6, J7, ISTA1, ISTA2, ISTA3, IND_OBS, &
     &           IND_FRQ, SGN(3), IOBS(3), LCHN, LFRQ, LTIM, FRG_IND, &
     &           UV_IND, NP(3), KP, ISEG(3), KTIM, FRI_STS, IER
      COMPLEX*8, ALLOCATABLE :: UV(:,:,:)
      COMPLEX*8  CRS_ACC
      INTEGER*4, EXTERNAL :: ILEN, I_LEN, LTM_DIF
      REAL*4,    EXTERNAL :: ATAN_CS_R4, PHAS_CMPL_R4
!
      ISTA1 = LTM_DIF ( 0, PIM%NSTA, PIM%C_STA, C_STA1 )
      ISTA2 = LTM_DIF ( 0, PIM%NSTA, PIM%C_STA, C_STA2 )
      ISTA3 = LTM_DIF ( 0, PIM%NSTA, PIM%C_STA, C_STA3 )
      IF ( ISTA1 < 1 ) THEN
           CALL ERR_LOG ( 7881, IUER, 'PIMA_CHECK_CLS', 'Wrong 1st station '// &
     &          C_STA1//' -- it did not observe in this experiment' )
           RETURN 
      END IF
      IF ( ISTA2 < 1 ) THEN
           CALL ERR_LOG ( 7882, IUER, 'PIMA_CHECK_CLS', 'Wrong 2st station '// &
     &          C_STA2//' -- it did not observe in this experiment' )
           RETURN 
      END IF
      IF ( ISTA3 < 1 ) THEN
           CALL ERR_LOG ( 7883, IUER, 'PIMA_CHECK_CLS', 'Wrong 3rd station '// &
     &          C_STA3//' -- it did not observe in this experiment' )
           RETURN 
      END IF
!
      IF ( IND_SCA < 1 .OR. IND_SCA > PIM%NSCA ) THEN
           CALL CLRCH ( STR  )
           CALL INCH  ( IND_SCA, STR )
           CALL ERR_LOG ( 7884, IUER, 'PIMA_CHECK_CLS', 'Wrong scan '// &
     &         'index: '//STR )
           RETURN 
      END IF
      IF ( IND_FRQ < PIM%CONF%BEG_FRQ .OR. IND_FRQ > PIM%CONF%END_FRQ ) THEN
           CALL CLRCH ( STR  )
           CALL INCH  ( IND_FRQ, STR )
           CALL ERR_LOG ( 7885, IUER, 'PIMA_CHECK_CLS', 'Wrong frequency '// &
     &         'index: '//STR )
           RETURN 
      END IF
      FREQ_REF = PIM%FREQ_ARR(1,IND_FRQ,PIM%CONF%FRQ_GRP)
!
      IOBS = 1
      DO 410 J1=1,PIM%SCA(IND_SCA)%NBAS
         IND_OBS = PIM%SCA(IND_SCA)%OBS_IND(J1)
!
         IF ( PIM%OBS(IND_OBS)%STA_IND(1) == ISTA1 .AND. &
     &        PIM%OBS(IND_OBS)%STA_IND(2) == ISTA2       ) THEN
              IOBS(1) = IND_OBS
              SGN(1)  = 1
         END IF
         IF ( PIM%OBS(IND_OBS)%STA_IND(1) == ISTA2 .AND. &
     &        PIM%OBS(IND_OBS)%STA_IND(2) == ISTA1       ) THEN
              IOBS(1) = IND_OBS
              SGN(1)  = -1
         END IF
!
         IF ( PIM%OBS(IND_OBS)%STA_IND(1) == ISTA1 .AND. &
     &        PIM%OBS(IND_OBS)%STA_IND(2) == ISTA3       ) THEN
              IOBS(2) = IND_OBS
              SGN(2)  = -1
         END IF
         IF ( PIM%OBS(IND_OBS)%STA_IND(1) == ISTA3 .AND. &
     &        PIM%OBS(IND_OBS)%STA_IND(2) == ISTA1       ) THEN
              IOBS(2) = IND_OBS
              SGN(2)  = 1
         END IF
!
         IF ( PIM%OBS(IND_OBS)%STA_IND(1) == ISTA2 .AND. &
     &        PIM%OBS(IND_OBS)%STA_IND(2) == ISTA3       ) THEN
              IOBS(3) = IND_OBS
              SGN(3)  = 1
         END IF
         IF ( PIM%OBS(IND_OBS)%STA_IND(1) == ISTA3 .AND. &
     &        PIM%OBS(IND_OBS)%STA_IND(2) == ISTA2       ) THEN
              IOBS(3) = IND_OBS
              SGN(3)  = -1
         END IF
 410  CONTINUE 
!
      IF ( IOBS(1) < 1 ) THEN
           CALL ERR_LOG ( 7886, IUER, 'PIMA_CHECK_CLS', 'Did not find '// &
     &         'an observation at baseline '//C_STA1//' / '//C_STA2 )
           RETURN 
      END IF
!
      IF ( IOBS(2) < 1 ) THEN
           CALL ERR_LOG ( 7887, IUER, 'PIMA_CHECK_CLS', 'Did not find '// &
     &         'an observation at baseline '//C_STA1//' / '//C_STA3 )
           RETURN 
      END IF
!
      IF ( IOBS(3) < 1 ) THEN
           CALL ERR_LOG ( 7888, IUER, 'PIMA_CHECK_CLS', 'Did not find '// &
     &         'an observation at baseline '//C_STA2//' / '//C_STA3 )
           RETURN 
      END IF
      LFRQ = PIM%CONF%END_FRQ - PIM%CONF%BEG_FRQ + 1
      LCHN = PIM%NCHN
      NP = 0
      TIME_SRT = (PIM%OBS(IOBS(1))%TIM_END - PIM%OBS(IOBS(1))%TIM_BEG)/2.0D0
      DO 420 J2=1,3
         FRG_IND = PIM%OBS(IOBS(J2))%REF_FRG_INDS(PIM%CONF%FRQ_GRP)
         LTIM = PIM%OBS(IOBS(J2))%NUM_EPC(FRG_IND)
         ALLOCATE ( UV(LCHN,LFRQ,LTIM), STAT=IER )
         IF ( IER .NE. 0 ) THEN
              CALL CLRCH ( STR )
              CALL IINCH ( 8*LTIM*PIM%NCHN*LFRQ, STR )
              CALL ERR_LOG ( 7889, IUER, 'PIMA_CHECK_CLS', 'Failure to '// &
     &             'allocate '//STR(1:I_LEN(STR))//' bytes of dynamic '// &
     &             'memory for array UV' )
              RETURN
         END IF
!
! ------ Get cross-correlation data
!
         CALL ERR_PASS ( IUER, IER )
! $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
         CALL PIMA_GET_UV ( PIM, IOBS(J2), LFRQ, PIM%CONF%BEG_FRQ, &
     &                      PIM%CONF%END_FRQ, PIM%CONF%FRQ_GRP, &
     &                      UV, WEI_1D, AP_LEN, FRI_STS, IER  )
! $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
         IF ( IER .NE. 0 ) THEN
              CALL CLRCH ( STR )
              CALL INCH  ( IOBS(J2), STR )
              CALL ERR_LOG ( 7890, IUER, 'PIMA_CHECK_CLS', 'Failure in '// &
     &             'readig cross-correlation data of observation '//STR )
              RETURN
         END IF 
         TIME_FRT = PIM%OBS(IOBS(J2))%FRT_OFFSET(1)
         TIM_SUM = 0.0
         CRS_ACC = 0.0
         KTIM = 0
         DO 430 J3=1,LTIM
            UV_IND  = PIM%OBS(IOBS(J2))%UV_IND(J3,FRG_IND)
            TIM_VAL = PIM%TIM_R8(PIM%UV_IND(UV_IND)%TIM_IND) - &
     &                PIM%OBS(IOBS(J2))%TIM_BEG
            TIM_SUM = TIM_SUM + TIM_VAL
            KTIM = KTIM + 1
            DO 440 J4=1,LCHN
               FREQ_DIF = PIM%FREQ_ARR(J4,PIM%CONF%BEG_FRQ,PIM%CONF%FRQ_GRP) - FREQ_REF
               PHS_MOD  =  (PIM%OBS(IOBS(J2))%RES_MB_DEL(PIMA__LSQ,1) + &
     &                     (TIME_SRT - TIME_FRT)*PIM%OBS(IOBS(J2))%RES_GR_RAT(1))* &
     &                      PI2*FREQ_DIF &
     &                   + PIM%OBS(IOBS(J2))%RES_PH_RAT(PIMA__LSQ,1)*PI2* &
     &                     FREQ_REF*(TIM_VAL - TIME_SRT)
!!  phs_mod = 0.0
               CRS_ACC = CRS_ACC + WEI_1D(J3)*UV(J4,IND_FRQ,J3)*CMPLX ( COS(PHS_MOD), SIN(PHS_MOD) )
 440        CONTINUE 
            IF ( MOD(J3,MSEG_TIM) == 0 .OR. ( MSEG_TIM > LTIM .AND. J3 == LTIM ) ) THEN
                 NP(J2) = NP(J2) + 1
                 TIM(NP(J2),J2) = TIM_SUM/KTIM
                 PHS(NP(J2),J2) = PHAS_CMPL_R4 ( CRS_ACC )
                 T1(NP(J2)) = TIM(NP(J2),J2) 
                 X1(NP(J2)) = PHS(NP(J2),J2) 
                 TIM_SUM = 0.0D0
                 CRS_ACC = CMPLX(0.0, 0.0)
                 KTIM = 0
            END IF
 430     CONTINUE 
         DEALLOCATE ( UV )
!!         CALL DIAGI_1 ( NP(J2), T1, X1, -2 )
 420  CONTINUE 
      KP = 0
      DO 450 J5=1,NP(1)
         ISEG = 0
         ISEG(1) = J5
         DO 460 J6=2,3
            DO 470 J7=1,NP(J6)
               IF ( ABS( TIM(J7,J6) - TIM(J5,1) ) < AP_LEN/2 ) THEN
                    ISEG(J6) = J7
               END IF
 470        CONTINUE 
 460     CONTINUE 
         IF ( ISEG(2) > 0 .AND. ISEG(3) > 0 ) THEN
              KP = KP + 1
              T1(KP) = TIM(J5,1)
              X1(KP) = SGN(1)*PHS(J5,1) + SGN(2)*PHS(J5,2) + SGN(3)*PHS(J5,3) 
              X1(KP) = X1(KP) - PI2*NINT(X1(KP)/PI2)
         END IF
 450  CONTINUE 
      IF ( KP > 1 ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( IND_SCA, STR )
           CALL CLRCH ( STR1 )
           CALL INCH  ( IND_FRQ, STR1 )
           CALL DIAGI_SETDEF ( IER, 'DIAGI_CTIT', 'Phase misclosure at stations '// &
     &          C_STA1//' '//C_STA2//' '//C_STA3//' Scan '//STR(1:I_LEN(STR))// &
     &          ' IF# '//STR1(1:I_LEN(STR1)) )
           CALL DIAGI_1 ( KP, T1, X1, -2 )
         ELSE IF ( KP == 1 ) THEN
           WRITE ( 6, 210 ) C_STA1, C_STA2, C_STA3, IND_SCA, IND_FRQ, X1(1)
 210       FORMAT ( 'Phase misclosure at ',A, 1X, A, 1X, A, ' in scan ', I3, &
     &              ', IF ', I2, ' is ',F6.3, ' rad' )
         ELSE 
           WRITE ( 6, * ) 'Did not find phase misclose of stations '// &
     &                     C_STA1//' '//C_STA2//' '//C_STA3
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE PIMA_CHECK_CLS  !#!#
