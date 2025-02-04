      SUBROUTINE COMP_IONO_MOD ( FIL_IONO, FIL_DTEC, IONO, VTD, &
     &                           N_BRK, BUF_BRK, &
     &                           N_BCL, BUF_BCL, &
     &                           IONO_EST, EXP_NAME, &
     &                           IVRB, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine COMP_IONO_MOD
! *                                                                      *
! * ### 06-DEC-2021  COMP_IONO_MOD  v3.1 (c)  L. Petrov 12-JAN-2023 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'solve.i'
      INCLUDE   'iono_solve.i'
      INCLUDE   'vtd.i'
      INTEGER*4  N_BRK, N_BCL, IVRB, IUER 
      TYPE     ( IONO_DEL__TYPE ) :: IONO(M_OBS)
      TYPE     ( IONO__EST_TYPE ) :: IONO_EST
      TYPE     ( VTD__TYPE      ) :: VTD
      CHARACTER  FIL_IONO*(*), FIL_DTEC*(*), EXP_NAME*(*), &
     &           BUF_BRK(N_BRK)*(*), BUF_BCL(N_BCL)*(*)
!
      CHARACTER  LAST_STA_SOU(M_STA)*8, C_BAS(M_BAS)*17, &
     &           C_STA_SOU(M_STA)*8, OUT(M_OBS)*228, CM_BAS(M_BAS)*17, STR*128
      REAL*8,    ALLOCATABLE  :: CNS_MAT(:), NOR_MAT(:), NOR_VEC(:), EQU_OBS(:), EST_VEC(:)
      REAL*8       OVR__SHR
      PARAMETER  ( OVR__SHR = 0.25D0 )
      REAL*8     TIM(M_OBS), ARG_SPL(M_OBS), TIM_SPL(M_OBS), VAL_SPL(M_OBS)
      REAL*8     WEI, RH, RES, RES2, RES3, RES4, RES5, RES6, RES7, RES_RMS, RES_RMS2, &
     &           RH_RMS, RC, WW, WW_ALL, TAI_0, STEP_SPL, SCL, TAI_BRK, WW_BAS(M_BAS), &
     &           RES_PRE(M_OBS),   RES_RMS_PRE_BAS_SLA(M_BAS), RES_RMS_PRE_ALL, &
     &           RES_PSF(M_OBS),   RES_RMS_PSF_BAS_SLA(M_BAS), RES_RMS_PSF_ALL, &
     &           RES_PSF_ZEN(M_OBS),  RES_AVR_PSF_BAS_ZEN(M_BAS), RES_RMS_PSF_BAS_ZEN(M_BAS), &
     &           RES_ZEN, RES_RMS_BAS_ZEN(M_BAS), RES_RMS_ZEN_ALL, &
     &           FUL_ZEN, FUL_RMS_BAS_ZEN(M_BAS), FUL_RMS_ZEN_ALL, &
     &           ERR_IOA(M_OBS), ERR_IOA_BAS(M_BAS),     ERR_IOA_ALL, &
     &           ERR_RAT, ERR_RAT_BAS(M_BAS),     ERR_RAT_ALL,        &
     &           AVR_GNSS_BAS_SLA(M_BAS),  RMS_GNSS_BAS_SLA(M_BAS),   &
     &           AVR_GNSS_BAS_ZEN(M_BAS),  RMS_GNSS_BAS_ZEN(M_BAS),   &
     &           RES_AVR_BAS_ZEN(M_BAS), RES_AVR_ZEN_ALL,             &
     &           FUL_AVR_BAS_ZEN(M_BAS), FUL_AVR_ZEN_ALL,             &
     &           ACC_SUM_BAS(M_BAS), ACC_SQR_BAS(M_BAS), ACC_CRS_BAS(M_BAS), &
     &           ACC_SUM_BAS_STA(M_BAS,2), ACC_SQR_BAS_STA(M_BAS,2), &
     &           AVR_GNSS_BAS_STA(M_BAS,2), RMS_GNSS_BAS_STA(M_BAS,2), &
     &           RES_AVR_PSF_BAS_SLA(M_BAS), RES_AVR_PSF_ALL, &
     &           COV_CRS_GNSS_BAS(M_BAS), CORR_GNSS_BAS(M_BAS), CORR_VLBI_BAS(M_BAS), &
     &           IONO_MOD_ERR_PRE(M_OBS), IONO_ERR_NRML_PRE(M_OBS), &
     &           IONO_MOD_ERR_PSF(M_OBS), IONO_ERR_NRML_PSF(M_OBS), &
     &           IONO_ERR_NRML_MOD(M_OBS), IONO_ADJ, IONO_BIAS_STA(2), &
     &           IONO_BIAS_AVR_BAS(M_BAS), IONO_BIAS_RMS_BAS(M_BAS), &
     &           IONO_ZEN_AVR_BAS(M_BAS),  IONO_ZEN_RMS_BAS(M_BAS), &
     &           ACC_VAL, ACC_SQR, ERR_SCL, DSTR_AVR_PRE, DSTR_SIG_PRE, &
     &           DSTR_AVR_PSF, DSTR_SIG_PSF, DSTR_AVR_MOD, DSTR_SIG_MOD, &
     &           BIAS_STA_AVR(M_STA), BIAS_STA_RMS(M_STA), FREQ_EFF_AVR(2), &
     &           IONO_AVR, IONO_RMS, IONO_FIT_RMS, IONO_DEL_ADJ(2), &
     &           IONO_DEL_ADJ_ERR, IONO_ADJ_STA_ERR(M_BAS), CHI_SQ, CNS_SP, &
     &           DEG_FRE, ME, DTEC(2), DEL_BIAS, DTEC_ERR, C11(M_BAS), &
     &           C12(M_BAS), C22(M_BAS), RMS_MOD_FUDGE, DUR, TAI_CEN, &
     &           MOD_AVR_BAS(M_BAS), MOD_RMS_BAS(M_BAS), ME_BAS(M_BAS), &
     &           DTEC_VMG, VIONO_ERR, VLBI_IONO(M_OBS)
      REAL*8     T1(M_OBS), X1(M_OBS), E1(M_OBS)
      INTEGER*4  J1, J2, J3, J4, J5, J6, J7, J8, J9, J10, J11, J12, J13, J14, &
     &           J15, J16, J17, J18, J19, J20, J21, J22, J23, J24, J25, J26, &
     &           J27, J28, J29, J30, J31, J32, J33, J34, J35, J36, J37, J38, &
     &           J39, J40, J41, J42, J43, J44, J45, J46, J47, J48, J49, J50, &
     &           J51, J52, J53, J54, J55, J56, &
     &           IM, INC, L_BAS, NK, I_STA(2), EXP_VERS, IND_REC(M_OBS), &
     &           K_STA(M_STA), K_BAS(M_BAS), &
     &           IND_STA_MAX, K_STA_MAX, IND_STA_REF, &
     &           LPAR, LPA2, SGN(2), MJD_0, IND, I_BAS, KP, &
     &           INC_STA(M_STA), INI_STA(M_STA), N_BRK_STA(M_STA), &
     &           IND_BRK_STA(M_BRK,M_STA), IND_REC_BRK_STA(M_BRK,M_STA), &
     &           MJD_BRK, N_TOT_BRK, &
     &           N_BCL_EXP, IND_BAS, INB(M_BAS), KK_BAS, KOBS, MRES, &
     &           NOBS, NO, N_CNS, MJD_CEN, IDAY, LM_BAS, IND_USED_REC, ISEED, IER 
      INTEGER*8  ITM_BRK_STA(M_BRK,M_STA), ITM
      REAL*8     FLOOR_VERR
      REAL*8     CNS_VAL_SIG, CNS_TIM_DER_SIG, CNS_TIM_DR2_SIG, CNS_PRX_SIG
      INTEGER*4, EXTERNAL :: ADD_CLIST, LTM_DIF
      REAL*8,    EXTERNAL :: DP_VV_V, BSPL_VAL, BSPL_DER, BSPL_DR2, GET_IONO_MOD_RES_RMS
      CHARACTER, EXTERNAL :: MJDSEC_TO_DATE*32, GET_CDATE*19
      INTEGER*4  LOCS, I, J
      LOCS(I,J) = min(I,J) +(max(I,J)*(max(I,J)-1))/2
!
      CNS_VAL_SIG     =  5.0D-10
      CNS_TIM_DER_SIG =  4.0D-14
      CNS_TIM_DR2_SIG =  2.0D-18
      CNS_PRX_SIG     =  1.0D-11
      FLOOR_VERR      = 10.0D-12
      ERR_SCL         =  0.889D0
      RMS_MOD_FUDGE   =  1.191D0
      ISEED           = 1239141
!
!  time step: 900
!      
      SCL = 3.0D-11
      SGN(1) = -1
      SGN(2) =  1
!
      CALL ERR_PASS ( IUER, IER )
      CALL IONO_READ_INFO ( FIL_IONO, IONO__3BND, M_OBS, NOBS, IONO, EXP_NAME, &
     &                      EXP_VERS, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 7221, IUER, 'COMP_IONO_MOD', 'Error in parsing '// &
     &         'pSolve iono information file '//FIL_IONO )
           RETURN
      END IF
      IF ( NOBS == 0 ) THEN
           CALL ERR_LOG ( 7222, IUER, 'COMP_IONO_MOD', 'No suitable '// &
     &         'observations for the ionosphere adjusment have been '// &
     &         'found in the pSolve iono information file pSolve '// &
     &          FIL_IONO )
           RETURN
      END IF
!
      IF ( IONO_EST%MODE == 2 ) THEN
           MRES = 1
         ELSE IF ( IONO_EST%MODE == 3 ) THEN
           MRES = 5
      END IF
!
! --- Collect station list 
!
      DO 410 J1=1,NOBS
         I_STA(1) = ADD_CLIST ( M_STA, IONO_EST%L_STA, IONO_EST%C_STA, IONO(J1)%STA(1), IER )
         I_STA(2) = ADD_CLIST ( M_STA, IONO_EST%L_STA, IONO_EST%C_STA, IONO(J1)%STA(2), IER )
         IER = -1
         I_BAS = ADD_CLIST ( M_BAS, L_BAS, C_BAS, IONO(J1)%STA(1)//'/'//IONO(J1)%STA(2), IER )
         IND_REC(J1) = IONO(J1)%IND_REC
 410  CONTINUE 
!
! --- Sort the station and baseline lists
!
      CALL SORT_CH ( IONO_EST%L_STA, IONO_EST%C_STA )
      CALL SORT_CH ( L_BAS, C_BAS )
!
      DUR = (IONO(NOBS)%MJD*86400.0D0 + IONO(NOBS)%TAI) - (IONO(1)%MJD*86400.0D0 + IONO(1)%TAI)
      TAI_CEN = IONO(1)%TAI + DUR/2
      IDAY = TAI_CEN/86400.0D0
      TAI_CEN = TAI_CEN - IDAY*86400.0D0
      MJD_CEN = IONO(1)%MJD + IDAY
!
      LM_BAS = (IONO_EST%L_STA*(IONO_EST%L_STA-1))/2
      CALL ERR_PASS ( IUER, IER )
      CALL COMP_IONO_ERR_REGR ( MJD_CEN, TAI_CEN, IONO_EST%L_STA, LM_BAS, &
     &                          IONO_EST%C_STA, CM_BAS, &
     &                          MOD_AVR_BAS, MOD_RMS_BAS, ME_BAS, &
     &                          VTD, ISEED, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 7223, IUER, 'COMP_IONO_MOD', 'Error in '// &
     &         'computation of ionosphere statistics' )
           RETURN 
      END IF
!
      N_BRK_STA        = 0
      IND_BRK_STA      = 0
      IND_REC_BRK_STA  = 0
      N_BCL_EXP        = 0
      INB              = 0
      FREQ_EFF_AVR     = 0.0D0
!
      DO 420 J2=1,NOBS
         DO 430 J3=1,2
            IER = -1
            I_STA(J3) = ADD_CLIST ( M_STA, IONO_EST%L_STA, IONO_EST%C_STA, IONO(J2)%STA(J3), IER )
            DO 440 J4=1,N_BRK
               IF ( BUF_BRK(J4)(1:10) == EXP_NAME .AND. BUF_BRK(J4)(24:31) == IONO(J2)%STA(J3) ) THEN
                    CALL ERR_PASS ( IUER, IER )
                    CALL DATE_TO_TIME ( BUF_BRK(J4)(55:77), MJD_BRK, TAI_BRK, IER )
                    IF ( (IONO(J2)%MJD - MJD_BRK)*86400.0D0 + (IONO(J2)%TAI - TAI_BRK) > 0.0D0 ) THEN
                         ITM = INT8(MJD_BRK)*INT8(86400) + IDNINT(TAI_BRK)
                         IF ( N_BRK_STA(I_STA(J3)) == 0 ) THEN
!
! --------------------------- There were no breaks before
!
                              N_BRK_STA(I_STA(J3)) = N_BRK_STA(I_STA(J3)) + 1
                              IND_BRK_STA(N_BRK_STA(I_STA(J3)),I_STA(J3)) = J2
                              IND_REC_BRK_STA(N_BRK_STA(I_STA(J3)),I_STA(J3)) = IND_REC(J2)
                              ITM_BRK_STA(N_BRK_STA(I_STA(J3)),I_STA(J3)) = ITM
                            ELSE
!
! --------------------------- There were breaks before. Check existing breaks
!
                              DO 450 J5=1,N_BRK_STA(I_STA(J3))
                                 IF ( ITM == ITM_BRK_STA(J5,I_STA(J3)) ) GOTO 850
 450                          CONTINUE 
!
! --------------------------- Add a new break
!
                              N_BRK_STA(I_STA(J3)) = N_BRK_STA(I_STA(J3)) + 1
                              IND_BRK_STA(N_BRK_STA(I_STA(J3)),I_STA(J3)) = J2
                              IND_REC_BRK_STA(N_BRK_STA(I_STA(J3)),I_STA(J3)) = IND_REC(J2)
                              ITM_BRK_STA(N_BRK_STA(I_STA(J3)),I_STA(J3)) = ITM
!
 850                          CONTINUE 
                         END IF
                    END IF
               END IF
 440        CONTINUE 
 430     CONTINUE 
         IONO_EST%IONO_BIAS = 0.0D0
!
         DO 470 J7=1,N_BCL
!
! --------- INB -- index of the baseline clock parameter for the given baseline or 
! ---------        0 if not baseline dependent clock is estimated
!
            IF ( BUF_BCL(J7)(1:10) == EXP_NAME         .AND. &
     &           BUF_BCL(J7)(12:19) == IONO(J2)%STA(1) .AND. &
     &           BUF_BCL(J7)(21:28) == IONO(J2)%STA(2)       ) THEN
!
                 IND_BAS = LTM_DIF ( 0, L_BAS, C_BAS, IONO(J2)%STA(1)//'/'//IONO(J2)%STA(2) )
                 IF ( INB(IND_BAS) == 0 ) THEN
                      N_BCL_EXP = N_BCL_EXP + 1 ! the total number of baseline clocks used for parameter estimation
                      INB(IND_BAS) = N_BCL_EXP
                 END IF
            END IF
 470     CONTINUE 
 420  CONTINUE 
!
! --- Compute K_STA -- the number of scans per station
!
      LAST_STA_SOU = '????????'
      K_STA = 0
      NK    = 0
      DO 480 J8=1,NOBS
!
! ------ Fill TIM array with epochs
!
         IF ( J8 == 1 ) THEN
              MJD_0 = IONO(J8)%MJD
              TAI_0 = IONO(J8)%TAI
         END IF 
         TIM(J8) = (IONO(J8)%MJD - MJD_0)*86400.0D0 + &
     &             (IONO(J8)%TAI - TAI_0)
         DO 490 J9=1,2
            I_STA(J9) = ADD_CLIST ( M_STA, IONO_EST%L_STA, IONO_EST%C_STA, IONO(J8)%STA(J9), IER )
            IF ( IONO(J8)%SOU .NE. LAST_STA_SOU(I_STA(J9)) ) THEN
                 K_STA(I_STA(J9)) = K_STA(I_STA(J9)) + 1
                 LAST_STA_SOU(I_STA(J9)) = IONO(J8)%SOU
            END IF 
 490     CONTINUE 
 480  CONTINUE 
!
! --- NK -- the number of knots
!
      IF ( IONO_EST%MODE == 2 .OR. IONO_EST%MODE == 3 ) THEN
           NK = (TIM(NOBS) - TIM(1))/IONO_EST%TIM_STEP
           IF ( TIM(NOBS) - ( TIM(1) + NK*IONO_EST%TIM_STEP ) > OVR__SHR*IONO_EST%TIM_STEP ) THEN
                NK = NK + 1
           END IF
!
           STEP_SPL = (TIM(NOBS) - TIM(1))/NK
           DO 4100 J10=1,NK
              ARG_SPL(J10) = TIM(1) + (J10-1)*STEP_SPL
 4100      CONTINUE 
           ARG_SPL(1)  = TIM(1)    - 1.D-4*STEP_SPL
           ARG_SPL(NK) = TIM(NOBS) + 1.D-4*STEP_SPL
      END IF
!
! --- Find the station with the maximum number of observations 
!
      IND_STA_MAX = -1
      K_STA_MAX   = -1
      N_TOT_BRK   = 0
      DO 4110 J11=1,IONO_EST%L_STA 
         IF ( N_BRK_STA(J11) == 0 .AND. K_STA(J11) > K_STA_MAX ) THEN
              K_STA_MAX   = K_STA(J11) 
              IND_STA_MAX = J11
         END IF
         N_TOT_BRK = N_TOT_BRK + N_BRK_STA(J11)
 4110 CONTINUE 
      IF ( IVRB .GE. 6 ) THEN
           WRITE ( 6, * ) 'STA_MAX ', IONO_EST%C_STA(IND_STA_MAX), ' K_STA_MAX= ', K_STA_MAX
      END IF
!
! --- This station will become the reference 
!
      IND_STA_REF = IND_STA_MAX
!
      INC = 1
      KK_BAS = 0
      IF ( BUF_BCL(1)(1:3) == 'ALL' ) THEN
!
! -------- Case of all baseline-dependent clock
!
           DO 4120 J12=1,L_BAS
              I_STA(1) = LTM_DIF ( 0, IONO_EST%L_STA, IONO_EST%C_STA, C_BAS(J12)(1:8)   )
              I_STA(2) = LTM_DIF ( 0, IONO_EST%L_STA, IONO_EST%C_STA, C_BAS(J12)(10:17) )
              IF ( .NOT. ( I_STA(1) == IND_STA_REF .OR. &
     &                     I_STA(2) == IND_STA_REF      ) ) THEN
                   IF ( INB(J12) == 0 ) THEN
                        N_BCL_EXP = N_BCL_EXP + 1 ! the total number of baseline clocks used for parameter estimation
                        INB(J12) = N_BCL_EXP
                   END IF
              END IF
 4120      CONTINUE 
      END IF
!
      IF ( N_BCL_EXP > 0 ) THEN
           DO 4130 J13=1,L_BAS
              IF ( INB(J13) > 0 ) THEN
                   INB(J13) = INB(J13) + (IONO_EST%L_STA - 1 + N_TOT_BRK)
                   KK_BAS = KK_BAS + 1
                   IF ( IVRB .GE. 2 ) THEN
                        WRITE ( 6, 110 ) KK_BAS, C_BAS(J13), INB(J13) 
 110                    FORMAT ( I3, ') BAS: ', A, ' INB= ', I5 )
                   END IF
              END IF
 4130      CONTINUE 
      END IF
!
      IF ( IVRB .GE. 4 ) THEN
           WRITE ( 6, 240 ) TRIM(EXP_NAME), IONO_EST%L_STA, N_TOT_BRK, L_BAS, N_BCL_EXP, NK
 240       FORMAT ( 'Exp: ', A, ' L_STA= ', I2, ' N_TOT_BRK: ', I4, ' L_BAS: ', I3, &
     &              ' N_BCL_EXP: ', I3, ' NK = ', I3 )
      END IF
!
! --- Order of parameters:
! ---       a) station clock offset (except the reference station)
! ---       b) station clock break
! ---       c) baseline dependent clock
! ---       d) B-spline for the ionosphere bias
!
      DO 4140 J14=1,IONO_EST%L_STA 
!
! ------ INC_STA index of the station time offset between bands paramweter
!
         IF ( J14 < IND_STA_REF ) THEN
              INC_STA(J14) = INC
              INC = INC + 1 + N_BRK_STA(J14)
            ELSE IF ( J14 == IND_STA_REF ) THEN
              INC_STA(J14) = 0
            ELSE IF ( J14 > IND_STA_REF ) THEN
              INC_STA(J14) = INC
              INC = INC + 1 + N_BRK_STA(J14)
         END IF
!
! ------ INI_STA index of the first parameter of B-spline that models the ionosperic bias
!
         IF ( IONO_EST%MODE == 1 ) THEN 
              INI_STA(J14) = IONO_EST%L_STA - 1 + N_TOT_BRK + N_BCL_EXP + J14
            ELSE IF ( IONO_EST%MODE == 2 ) THEN
              INI_STA(J14) = IONO_EST%L_STA - 1 + N_TOT_BRK + N_BCL_EXP + (J14-1)*(NK - 1 + IONO_EST%MDEG) + 1
            ELSE IF ( IONO_EST%MODE == 3 ) THEN
              INI_STA(J14) = IONO_EST%L_STA - 1 + N_TOT_BRK + N_BCL_EXP + (J14-1)*MRES*(NK - 1 + IONO_EST%MDEG) + 1
         END IF
         IF ( IVRB .GE. 2 ) THEN
              WRITE ( 6, 120 ) J14, IONO_EST%C_STA(J14), K_STA(J14), N_BRK_STA(J14), INC_STA(J14), INI_STA(J14)
  120         FORMAT ( I2, ') Sta: ', A, ' K_obs: ', I5,' Num_brk: ', I2, ' Inc: ', I5, ' INI: ', I5 )
              IF ( N_BRK_STA(J14) > 0 ) THEN
                   DO 4150 J15=1,N_BRK_STA(J14)
                      MJD_BRK = ITM_BRK_STA(J15,J14)/INT8(86400)
                      TAI_BRK = ITM_BRK_STA(J15,J14) - INT8(MJD_BRK)*INT8(86400)
                      CALL ERR_PASS ( IUER, IER )
                      STR = MJDSEC_TO_DATE ( MJD_BRK, TAI_BRK, IER ) 
                      WRITE ( 6, 130 ) IONO_EST%C_STA(J14), J15, IND_BRK_STA(J15,J14), STR(1:19)
  130                 FORMAT ( '    Sta: ', A, ' Brk: ', I1, ' OBS_IND_BRK: ', I5, ' Date: ', A )
 4150              CONTINUE 
              END IF
         END IF
 4140 CONTINUE 
!
! --- Compute the number of parameters to estimate
!
      IF ( IONO_EST%MODE == 1 ) THEN
           LPAR = (IONO_EST%L_STA - 1 + N_TOT_BRK) + N_BCL_EXP + IONO_EST%L_STA 
         ELSE IF ( IONO_EST%MODE == 2 .OR. IONO_EST%MODE == 3 ) THEN 
           LPAR = (IONO_EST%L_STA - 1 + N_TOT_BRK) + N_BCL_EXP + &
     &             MRES*(NK - 1 + IONO_EST%MDEG)*IONO_EST%L_STA
      END IF
      IF ( IVRB .GE. 6 ) THEN
           WRITE ( 6, * ) 'LPAR= ', LPAR, &
     &                    ' IND_STA_REF= ', INT2(IND_STA_REF), &
     &                    ' N_TOT_BRK= ', INT2(N_TOT_BRK), &
     &                    ' N_BCL_EXP= ', INT2(N_BCL_EXP), &
     &                    ' NK= ', INT2(NK), ' MRES= ', INT2(MRES)
      END IF
      LPA2 = (LPAR*(LPAR+1))/2
      ALLOCATE ( NOR_MAT(LPA2), CNS_MAT(LPA2), NOR_VEC(LPAR), EQU_OBS(LPAR), EST_VEC(LPAR) )
!
! --- Initialize normal matrix
!
      NOR_MAT = 0.0D0
      NOR_VEC = 0.0D0
!
! --- Build a normal system of equations
!
      DO 4160 J16=1,NOBS
         IND_BAS = LTM_DIF ( 0, L_BAS, C_BAS, IONO(J16)%STA(1)//'/'//IONO(J16)%STA(2) )
         EQU_OBS = 0.0D0
         DO 4170 J17=1,2
            I_STA(J17) = ADD_CLIST ( M_STA, IONO_EST%L_STA, IONO_EST%C_STA, IONO(J16)%STA(J17), IER )
            IF ( INC_STA(I_STA(J17)) .NE. 0 ) THEN
!
! -------------- Clock bias
!
                 IND = INC_STA(I_STA(J17)) 
                 EQU_OBS(IND) = SGN(J17)
!
                 IF ( N_BRK_STA(I_STA(J17)) > 0 ) THEN
!
! ------------------- Clock breaks
!
                      DO 4180 J18=1,N_BRK_STA(I_STA(J17))
                         IF ( J16 .GE. IND_BRK_STA(J18,I_STA(J17)) ) THEN
!
! --------------------------- Insert estimation of the J18-th clock break
!
                              IND = INC_STA(I_STA(J17)) + J18
                              EQU_OBS(IND) = SGN(J17)
                         END IF
 4180                 CONTINUE 
                 END IF
            END IF
!
! --------- Baseline dependent clock
!
            IF ( INB(IND_BAS) > 0 ) THEN
                 EQU_OBS(INB(IND_BAS)) = 1.0D0
            END IF
!
! --------- Scaled TEC bias
!
            IF ( IONO_EST%MODE == 1 ) THEN
                 IND = INI_STA(I_STA(J17))
                 EQU_OBS(IND)   = SGN(J17)*IONO(J16)%IONO_MAP(J17)
               ELSE IF ( IONO_EST%MODE == 2 .OR. IONO_EST%MODE == 3 ) THEN
                 DO 4190 J19=1-IONO_EST%MDEG,NK-1
                    IF ( IONO_EST%MODE == 2 ) THEN
                         IND = INI_STA(I_STA(J17)) - 1 + (J19 + IONO_EST%MDEG)
                       ELSE IF ( IONO_EST%MODE == 3 ) THEN
                         IF ( IONO(J16)%EL(J17) > 27.0D0 ) THEN
                              IND = INI_STA(I_STA(J17)) - 1 + MRES*(J19 + IONO_EST%MDEG - 1) + 1
                            ELSE IF ( IONO(J16)%AZ(J17) < 90.0D0 ) THEN
                              IND = INI_STA(I_STA(J17)) - 1 + MRES*(J19 + IONO_EST%MDEG - 1) + 2
                            ELSE IF ( IONO(J16)%AZ(J17) < 180.0D0 ) THEN
                              IND = INI_STA(I_STA(J17)) - 1 + MRES*(J19 + IONO_EST%MDEG - 1) + 3
                            ELSE IF ( IONO(J16)%AZ(J17) < 270.0D0 ) THEN
                              IND = INI_STA(I_STA(J17)) - 1 + MRES*(J19 + IONO_EST%MDEG - 1) + 4
                            ELSE IF ( IONO(J16)%AZ(J17) < 360.1D0 ) THEN
                              IND = INI_STA(I_STA(J17)) - 1 + MRES*(J19 + IONO_EST%MDEG - 1) + 5
                         END IF
                    END IF
                    EQU_OBS(IND) = BSPL_VAL ( NK, ARG_SPL, IONO_EST%MDEG, J19, TIM(J16) )* &
     &                             SGN(J17)*IONO(J16)%IONO_MAP(J17)
 4190            CONTINUE 
            END IF
 4170    CONTINUE 
         WEI = SCL/DSQRT(FLOOR_VERR**2 + IONO(J16)%IONO_VERR**2)
         RH = IONO(J16)%IONO_V - IONO_EST%APR_SCALE*IONO(J16)%IONO_G 
         CALL DIAD_CVT_S ( WEI**2, LPAR, EQU_OBS, EQU_OBS, NOR_MAT )
         CALL NORVEC_UPD ( LPAR, WEI, RH, EQU_OBS, NOR_VEC )
         IF ( IVRB == 7 ) THEN
              WRITE ( 6, * ) 'COMP_IONO_MOD J16= ', int2(j16), ' WEI= ', WEI, ' RH= ', RH
         END IF 
 4160 CONTINUE 
!
! --- Impose constraints
!
      N_CNS = 0.0
      CNS_MAT = 0.0D0
      DO 4200 J20=1,IONO_EST%L_STA
         IF ( IONO_EST%MODE == 2 .OR. IONO_EST%MODE == 3 ) THEN
              DO 4210 J21=1,MRES
!
! -------------- Constraint on value
!
                 DO 4220 J22=1,NK-1
                    EQU_OBS = 0.0D0
                    DO 4230 J23=1-IONO_EST%MDEG,NK-1
                       IND = INI_STA(J20) + MRES*(J23 - 1 + IONO_EST%MDEG) + (J21-1)
                       EQU_OBS(IND) = BSPL_VAL ( NK, ARG_SPL, IONO_EST%MDEG, J23, ARG_SPL(J22) + 0.5D0*STEP_SPL )
 4230               CONTINUE
                    IF ( CNS_VAL_SIG > 0.0D0 ) THEN
                         CALL DIAD_CVT_S ( SCL**2/CNS_VAL_SIG**2, LPAR, EQU_OBS, EQU_OBS, NOR_MAT )
                         N_CNS =  N_CNS + 1
                         CALL DIAD_CVT_S ( SCL**2/CNS_VAL_SIG**2, LPAR, EQU_OBS, EQU_OBS, CNS_MAT )
                    END IF
 4220            CONTINUE 
!
! -------------- Constraint on the first time derivative
!
                 DO 4240 J24=1,NK-1
                    EQU_OBS = 0.0D0
                    DO 4250 J25=1-IONO_EST%MDEG,NK-1
                       IND = INI_STA(J20) + MRES*(J25 - 1 + IONO_EST%MDEG) + (J21-1)
                       EQU_OBS(IND) = BSPL_DER ( NK, ARG_SPL, IONO_EST%MDEG, J25, ARG_SPL(J24) + 0.5D0*STEP_SPL  )
 4250               CONTINUE 
                    IF ( CNS_TIM_DER_SIG > 0.0D0 ) THEN
                         CALL DIAD_CVT_S ( SCL**2/CNS_TIM_DER_SIG**2, LPAR, EQU_OBS, EQU_OBS, NOR_MAT )
                         N_CNS =  N_CNS + 1
                         CALL DIAD_CVT_S ( SCL**2/CNS_TIM_DER_SIG**2, LPAR, EQU_OBS, EQU_OBS, CNS_MAT )
                    END IF
 4240            CONTINUE 
!
! -------------- Constraint on the second time derivative
!
                 DO 4260 J26=1,NK-1
                    EQU_OBS = 0.0D0
                    DO 4270 J27=1-IONO_EST%MDEG,NK-1
                       IND = INI_STA(J20) + MRES*(J27 - 1 + IONO_EST%MDEG) + (J21-1)
                       EQU_OBS(IND) = BSPL_DR2 ( NK, ARG_SPL, IONO_EST%MDEG, J27, ARG_SPL(J26) + 0.5D0*STEP_SPL  )
 4270               CONTINUE 
                    IF ( CNS_TIM_DR2_SIG > 0.0D0 ) THEN
                         CALL DIAD_CVT_S ( SCL**2/CNS_TIM_DR2_SIG**2, LPAR, EQU_OBS, EQU_OBS, NOR_MAT )
                         N_CNS =  N_CNS + 1
                         CALL DIAD_CVT_S ( SCL**2/CNS_TIM_DR2_SIG**2, LPAR, EQU_OBS, EQU_OBS, CNS_MAT )
                    END IF
 4260            CONTINUE 
 4210         CONTINUE 
         END IF
!
         IF ( IONO_EST%MODE == 3 .AND. MRES == 5 ) THEN
!
! ----------- Constraint in proximity of az/el slices
!
              DO 4280 J28=1-IONO_EST%MDEG,NK-1
                 EQU_OBS = 0.0D0
                 DO 4290 J29=1,4
                    IND = INI_STA(J20) - 1 + MRES*(J28 - 1 + IONO_EST%MDEG) + 1
                    EQU_OBS(IND) =  1.0D0
                    IND = INI_STA(J20) - 1 + MRES*(J28 - 1 + IONO_EST%MDEG) + 1 + J29
                    EQU_OBS(IND) = -1.0D0
 4290            CONTINUE 
                 IF ( CNS_PRX_SIG > 0.0D0 ) THEN
                      CALL DIAD_CVT_S ( SCL**2/CNS_PRX_SIG**2, LPAR, EQU_OBS, EQU_OBS, NOR_MAT )
                      N_CNS =  N_CNS + 1
                      CALL DIAD_CVT_S ( SCL**2/CNS_PRX_SIG**2, LPAR, EQU_OBS, EQU_OBS, CNS_MAT )
                 END IF
 4280         CONTINUE 
         END IF
 4200 CONTINUE 
!
! --- Invert the normal matrix
!
      IF ( IVRB .GE. 55 ) THEN
           CALL MATVIEW_2 ( LPAR, NOR_MAT ) 
      END IF
      CALL ERR_PASS ( IUER, IER )
      CALL INVS ( LPAR, NOR_MAT, RC, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 7224, IUER, 'COMP_IONO_MOD', 'Failure in '// &
     &         'adusting the ionospheric model using least squares '// &
     &         'while processing experiment '//EXP_NAME )
           RETURN 
      END IF
      IF ( IVRB .GE. 1 ) THEN
           WRITE ( 6, 140 ) EXP_NAME, RC
 140       FORMAT ( 'Experiment: ', A, ' Condition number: ', 1PD12.5 )
      END IF
!
! --- Find a vector of parameter estimates
!
      IER=-1
      CALL MUL_MV_SV_V ( LPAR, NOR_MAT, LPAR, NOR_VEC, LPAR, EST_VEC, IER )
!
! --- Compute Sp ( CNS_MAT * NOR_MAT)
! --- NB: since these matrices are in the upper triangle representaiton, 
! --- there are some complications
!
      CNS_SP = 2.0D0*DP_VV_V ( LPA2, NOR_MAT, CNS_MAT )
      KP = 0
      DO 4300 J30=1,LPAR
         KP = KP + J30
         CNS_SP = CNS_SP - NOR_MAT(KP)*CNS_MAT(KP)
 4300 CONTINUE 
!
      DO 4310 J31=1,L_BAS
         I_STA(1) = LTM_DIF ( 0, IONO_EST%L_STA, IONO_EST%C_STA, C_BAS(J31)(1:8)   )
         I_STA(2) = LTM_DIF ( 0, IONO_EST%L_STA, IONO_EST%C_STA, C_BAS(J31)(10:17) )
         IF ( INC_STA(I_STA(1)) > 0 .AND. INC_STA(I_STA(2)) > 0 ) THEN
              CORR_VLBI_BAS(J31) = NOR_MAT(LOCS(INC_STA(I_STA(1)),INC_STA(I_STA(2))))/ &
     &                                 DSQRT ( NOR_MAT(LOCS(INC_STA(I_STA(1)),INC_STA(I_STA(1))))* &
     &                                         NOR_MAT(LOCS(INC_STA(I_STA(2)),INC_STA(I_STA(2))))  )
           
            ELSE 
              CORR_VLBI_BAS(J31) = 0.0D0
         END IF
 4310 CONTINUE 
!
      CHI_SQ = 0.0D0
      RH_RMS = 0.0D0
      WW     = 0.0D0
      WW_BAS = 0.0D0
      K_BAS  = 0
      KP     = 0
      C11    = 0.0D0
      C12    = 0.0D0
      C22    = 0.0D0
      RES_RMS_PRE_BAS_SLA = 0.0D0
      RES_RMS_PSF_BAS_SLA = 0.0D0
      RES_AVR_PSF_BAS_SLA = 0.0D0
      RES_AVR_BAS_ZEN     = 0.0D0
      RES_RMS_BAS_ZEN     = 0.0D0
      FUL_AVR_BAS_ZEN     = 0.0D0
      FUL_RMS_BAS_ZEN     = 0.0D0
      RES_AVR_PSF_BAS_ZEN = 0.0D0
      RES_RMS_PSF_BAS_ZEN = 0.0D0
      ERR_IOA_BAS         = 0.0D0
!
! --- Compute residuals and statistics
!
      DO 4320 J32=1,NOBS
         EQU_OBS = 0.0D0
         IND_BAS = LTM_DIF ( 0, L_BAS, C_BAS, IONO(J32)%STA(1)//'/'//IONO(J32)%STA(2) )
         DO 4330 J33=1,2
            I_STA(J33) = ADD_CLIST ( M_STA, IONO_EST%L_STA, IONO_EST%C_STA, IONO(J32)%STA(J33), IER )
            IF ( INC_STA(I_STA(J33)) .NE. 0 ) THEN
                 IND = INC_STA(I_STA(J33)) 
                 EQU_OBS(IND) = SGN(J33)
                 IF ( N_BRK_STA(I_STA(J33)) > 0 ) THEN
                      DO 4340 J34=1,N_BRK_STA(I_STA(J33))
                         IF ( J32 .GE. IND_BRK_STA(J34,I_STA(J33)) ) THEN
                              IND = INC_STA(I_STA(J33)) + J34
                              EQU_OBS(IND) = SGN(J33)
                         END IF
 4340                 CONTINUE 
                 END IF
            END IF
!
            IF ( INB(IND_BAS) > 0 ) THEN
                 EQU_OBS(INB(IND_BAS)) = 1.0D0
            END IF
!
            IF ( IONO_EST%MODE == 1 ) THEN
                 IND = INI_STA(I_STA(J33))
                 EQU_OBS(IND)   = SGN(J33)*IONO(J32)%IONO_MAP(J33)
               ELSE IF ( IONO_EST%MODE == 2 .OR. IONO_EST%MODE == 3 ) THEN 
                 DO 4350 J35=1-IONO_EST%MDEG,NK-1
                    IF ( IONO_EST%MODE == 2 ) THEN
                         IND = INI_STA(I_STA(J33)) - 1 + (J35 + IONO_EST%MDEG)
                       ELSE IF ( IONO_EST%MODE == 3 ) THEN
                         IF ( IONO(J32)%EL(J33) > 27.0D0 ) THEN
                              IND = INI_STA(I_STA(J33)) - 1 + MRES*(J35 + IONO_EST%MDEG - 1) + 1
                            ELSE IF ( IONO(J32)%IONO_MAP(J33) < 90.0D0 ) THEN
                              IND = INI_STA(I_STA(J33)) - 1 + MRES*(J35 + IONO_EST%MDEG - 1) + 2
                            ELSE IF ( IONO(J32)%IONO_MAP(J33) < 180.0D0 ) THEN
                              IND = INI_STA(I_STA(J33)) - 1 + MRES*(J35 + IONO_EST%MDEG - 1) + 3
                            ELSE IF ( IONO(J32)%IONO_MAP(J33) < 270.0D0 ) THEN
                              IND = INI_STA(I_STA(J33)) - 1 + MRES*(J35 + IONO_EST%MDEG - 1) + 4
                            ELSE IF ( IONO(J32)%IONO_MAP(J33) < 360.1D0 ) THEN
                              IND = INI_STA(I_STA(J33)) - 1 + MRES*(J35 + IONO_EST%MDEG - 1) + 5
                         END IF
                    END IF
                    EQU_OBS(IND) = BSPL_VAL ( NK, ARG_SPL, IONO_EST%MDEG, J35, TIM(J32) ) * &
     &                             SGN(J33)*IONO(J32)%IONO_MAP(J33)
 4350            CONTINUE 
            END IF
 4330    CONTINUE 
         I_BAS = LTM_DIF ( 0, L_BAS, C_BAS, IONO(J32)%STA(1)//'/'//IONO(J32)%STA(2) )
!
         WEI = SCL/DSQRT(FLOOR_VERR**2 + IONO(J32)%IONO_VERR**2)
!
! ------ RH -- raw VLBI minus GNSS slant ionospheric path delay that includes clock function
!
         RH = IONO(J32)%IONO_V - IONO_EST%APR_SCALE*IONO(J32)%IONO_G
!
! ------ RES -- postfit residual after taking into account both clock and
! ------        TEC adjuments
!
         RES_PSF(J32) = RH - DP_VV_V ( LPAR, EST_VEC, EQU_OBS )
!
! ------ RES_PRE(J32) -- VLBI minus GNSS slant ionospheric path delay that does not includes clock function
!
         RES_PRE(J32) = RH - DP_VV_V ( IONO_EST%L_STA - 1 + N_TOT_BRK + N_BCL_EXP, EST_VEC, EQU_OBS ) 
!
! ------ VLBI_IONO -- VLBI slant ionospheric path delay minus contribution of clock
!
         VLBI_IONO(J32) = IONO(J32)%IONO_V - DP_VV_V ( IONO_EST%L_STA - 1 + N_TOT_BRK + N_BCL_EXP, EST_VEC, EQU_OBS ) 
!
! ------ RES_ZEN   -- postfit residuals normalized to the zenith with the averaged mapping function
!
         ME = ( IONO(J32)%IONO_MAP(1) + IONO(J32)%IONO_MAP(2))/2.0D0
         RES_ZEN = (RES_PRE(J32) - ( IONO_EST%IONO_BIAS(I_STA(2))*IONO(J32)%IONO_MAP(2) - &
     &                               IONO_EST%IONO_BIAS(I_STA(1))*IONO(J32)%IONO_MAP(1) ))/ME
         RES_PSF_ZEN(J32) = RES_PSF(J32)/ME
!
! ------ FUL_ZEN   -- Full zenith VLBI ionospheric path delay derived with the averaged mapping funcion
!
         FUL_ZEN = (IONO(J32)%IONO_V - DP_VV_V ( IONO_EST%L_STA - 1 + N_TOT_BRK + N_BCL_EXP, EST_VEC, EQU_OBS ) )/ME
!
! ------ IONO_ADJ  -- aduststment to the slant ionospheric path delay
!
         IONO_ADJ = DP_VV_V ( LPAR - INI_STA(1) + 1, &
     &                        EST_VEC(INI_STA(1)), EQU_OBS(INI_STA(1)) ) 
!
! ------ ERR_IOA -- error of ionosphere adjustment 
!
         CALL MUL_MV_SV_V ( LPAR, NOR_MAT, LPAR, EQU_OBS, LPAR, NOR_VEC, IER )
         ERR_IOA(J32) = SCL * DSQRT ( DP_VV_V ( LPAR, EQU_OBS, NOR_VEC ) )
!
         CHI_SQ   = CHI_SQ   + RES_PSF(J32)**2/(FLOOR_VERR**2 + IONO(J32)%IONO_VERR**2)
         IF ( IVRB .EQ. 3 ) THEN
              WRITE ( 6, 150 ) J32, IONO(J32)%STA(1), IONO(J32)%STA(2), &
     &                         IONO(J32)%IONO_V, IONO_EST%APR_SCALE*IONO(J32)%IONO_G, &
     &                         WEI, RH, RES_PRE(J32), RES_PSF(J32), &
     &                         RES_ZEN, ERR_IOA(J32), IONO_ADJ
 150          FORMAT ( 'Obs: ', I6, ' Sta: ',A, 1X, A, &
     &                 ' Iono_V: ', 1PD12.5, ' Iono_G: ', 1PD12.5, ' Wei: ', 1PD12.5, &
     &                 ' Rh: ', 1PD12.5, ' Res_pre: ', 1PD12.5, ' Res_psf: ', 1PD12.5, &
     &                 ' Res_mmp: ', 1PD12.5, ' Err_ioa: ', 1PD12.5, ' iono_adj= ', 1PD12.5 )
         END IF
!
         WW = WW + WEI
         RES_AVR_BAS_ZEN(I_BAS) = RES_AVR_BAS_ZEN(I_BAS)  + WEI*RES_ZEN
         FUL_AVR_BAS_ZEN(I_BAS) = FUL_AVR_BAS_ZEN(I_BAS)  + WEI*FUL_ZEN
         RES_AVR_PSF_BAS_SLA(I_BAS) = RES_AVR_PSF_BAS_SLA(I_BAS) + WEI*RES_PSF(J32)
         RES_AVR_PSF_BAS_ZEN(I_BAS) = RES_AVR_PSF_BAS_ZEN(I_BAS) + WEI*RES_PSF_ZEN(J32)
!
         RES_RMS_BAS_ZEN(I_BAS) = RES_RMS_BAS_ZEN(I_BAS)  + WEI*RES_ZEN**2
         FUL_RMS_BAS_ZEN(I_BAS) = FUL_RMS_BAS_ZEN(I_BAS)  + WEI*FUL_ZEN**2
         RES_RMS_PRE_BAS_SLA(I_BAS) = RES_RMS_PRE_BAS_SLA(I_BAS) + WEI*RES_PRE(J32)**2
         RES_RMS_PSF_BAS_ZEN(I_BAS) = RES_RMS_PSF_BAS_ZEN(I_BAS) + WEI*RES_PSF_ZEN(J32)**2
!
         RES_RMS_PSF_BAS_SLA(I_BAS) = RES_RMS_PSF_BAS_SLA(I_BAS)  + WEI*RES_PSF(J32)**2
         ERR_IOA_BAS(I_BAS)         = ERR_IOA_BAS(I_BAS)          + ERR_IOA(J32)
!
         C11(I_BAS) = C11(I_BAS) + IONO(J32)%IONO_MAP(1)**2
         C12(I_BAS) = C12(I_BAS) + IONO(J32)%IONO_MAP(1)*IONO(J32)%IONO_MAP(2)
         C22(I_BAS) = C22(I_BAS) + IONO(J32)%IONO_MAP(2)**2
         WW_BAS(I_BAS)      = WW_BAS(I_BAS) + WEI
         K_BAS(I_BAS)       = K_BAS(I_BAS)  + 1
         IF ( C_BAS(I_BAS) == 'LA-VLBA /MK-VLBA' ) THEN
              KP = KP + 1
              T1(KP) = TIM(J32)
              X1(KP) = IONO_ADJ
              E1(KP) = IONO(J32)%IONO_VERR
         END IF
 4320 CONTINUE 
      IF ( IVRB .GE. 34 ) THEN
           CALL DIAGI_1E ( KP, T1, X1, E1, IUER ) 
      END IF
!
      RES_RMS_PRE_ALL = 0.0D0
      RES_RMS_PSF_ALL = 0.0D0
      RES_AVR_PSF_ALL = 0.0D0
      RES_RMS_ZEN_ALL = 0.0D0
      ERR_IOA_ALL     = 0.0D0
      ERR_RAT_ALL     = 0.0D0
      WW_ALL          = 0.0D0
!
      DEG_FRE = NOBS - (LPAR - CNS_SP)
      WW_ALL = 0.0D0
      DO 4360 J36=1,L_BAS
         RES_AVR_PSF_ALL  = RES_AVR_PSF_ALL + RES_AVR_PSF_BAS_SLA(J36)
         RES_AVR_ZEN_ALL  = RES_AVR_ZEN_ALL + RES_AVR_BAS_ZEN(J36)
         FUL_AVR_ZEN_ALL  = FUL_AVR_ZEN_ALL + FUL_AVR_BAS_ZEN(J36)
!
         RES_RMS_PRE_ALL  = RES_RMS_PRE_ALL + RES_RMS_PRE_BAS_SLA(J36)
         RES_RMS_PSF_ALL  = RES_RMS_PSF_ALL + RES_RMS_PSF_BAS_SLA(J36)
         RES_RMS_ZEN_ALL  = RES_RMS_ZEN_ALL + RES_RMS_BAS_ZEN(J36)
!
         FUL_RMS_ZEN_ALL  = FUL_RMS_ZEN_ALL + FUL_RMS_BAS_ZEN(J36)
         ERR_IOA_ALL      = ERR_IOA_ALL     + ERR_IOA_BAS(J36)
         WW_ALL           = WW_ALL          + WW_BAS(J36)
         ERR_IOA_BAS(J36) = ERR_IOA_BAS(J36)/K_BAS(J36) * DSQRT(CHI_SQ/DEG_FRE)
!
         RES_AVR_PSF_BAS_SLA(J36)     = RES_AVR_PSF_BAS_SLA(J36)/WW_BAS(J36)
         RES_AVR_BAS_ZEN(J36)     = RES_AVR_BAS_ZEN(J36)/WW_BAS(J36)
         FUL_AVR_BAS_ZEN(J36)     = FUL_AVR_BAS_ZEN(J36)/WW_BAS(J36)
         RES_AVR_PSF_BAS_ZEN(J36) = RES_AVR_PSF_BAS_ZEN(J36)/WW_BAS(J36)
!
         RES_RMS_PRE_BAS_SLA(J36) = DSQRT ( RES_RMS_PRE_BAS_SLA(J36)/WW_BAS(J36) )
         RES_RMS_PSF_BAS_SLA(J36) = DSQRT ( RES_RMS_PSF_BAS_SLA(J36)/WW_BAS(J36) )
         RES_RMS_BAS_ZEN(J36) = DSQRT ( RES_RMS_BAS_ZEN(J36)/WW_BAS(J36) - RES_AVR_BAS_ZEN(J36)**2 )
         FUL_RMS_BAS_ZEN(J36) = DSQRT ( FUL_RMS_BAS_ZEN(J36)/WW_BAS(J36) - FUL_AVR_BAS_ZEN(J36)**2 )
         RES_RMS_PSF_BAS_ZEN(J36) = DSQRT ( RES_RMS_PSF_BAS_ZEN(J36)/WW_BAS(J36) - RES_AVR_PSF_BAS_ZEN(J36)**2 )
!
         ERR_RAT_BAS(J36)     = ERR_SCL*RES_RMS_PSF_BAS_SLA(J36)/ERR_IOA_BAS(J36) 
         ERR_RAT_ALL          = ERR_RAT_ALL + ERR_RAT_BAS(J36)*K_BAS(J36)
 4360 CONTINUE 
!
      RES_RMS_PRE_ALL = DSQRT ( RES_RMS_PRE_ALL/WW_ALL )
      RES_RMS_PSF_ALL = DSQRT ( RES_RMS_PSF_ALL/WW_ALL )
      RES_AVR_PSF_ALL =         RES_AVR_PSF_ALL/WW_ALL
      RES_RMS_ZEN_ALL = DSQRT ( RES_RMS_ZEN_ALL/WW_ALL - RES_AVR_PSF_ALL**2 )
      ERR_IOA_ALL     = DSQRT ( ERR_IOA_ALL/NOBS       )
      ERR_RAT_ALL     = ERR_RAT_ALL/NOBS
!
      DO 4370 J37=1,IONO_EST%L_STA
         ACC_VAL = 0.0D0
         ACC_SQR = 0.0D0
         IF ( J37 .NE. IND_STA_REF  ) THEN
              IONO_ADJ_STA_ERR(J37) = SCL * DSQRT ( NOR_MAT(LOCS(INC_STA(J37),INC_STA(J37))) ) * DSQRT ( CHI_SQ/DEG_FRE )
         END IF
!
         DO 4380 J38=1,NK-1
            EQU_OBS = 0.0D0
            TIM_SPL(J38) = ARG_SPL(J38) + 0.5D0*STEP_SPL
            DO 4390 J39=1-IONO_EST%MDEG,NK-1
               IND = INI_STA(J37) + MRES*(J39 + IONO_EST%MDEG - 1)
               EQU_OBS(IND) = BSPL_VAL ( NK, ARG_SPL, IONO_EST%MDEG, J39, TIM_SPL(J38) )
 4390       CONTINUE 
            VAL_SPL(J38) = DP_VV_V ( LPAR, EST_VEC, EQU_OBS )
            ACC_VAL = ACC_VAL + VAL_SPL(J38)
            ACC_SQR = ACC_SQR + VAL_SPL(J38)**2
 4380    CONTINUE 
         BIAS_STA_AVR(J37) = ACC_VAL/(NK-1)
         BIAS_STA_RMS(J37) = DSQRT ( ACC_SQR/(NK-1) - BIAS_STA_AVR(J37)**2 )
!
         CALL ERR_PASS ( IUER, IER )
         CALL GNSS_IONO_RMS_STA ( 1440, IONO_EST%C_STA(J37), .FALSE., ISEED, &
     &                            5.0D0*DEG__TO__RAD, 85.D0*DEG__TO__RAD, &
     &                            60.0D0, MJD_0, 0.0D0, &
     &                            IONO_AVR, IONO_RMS, IONO_FIT_RMS, IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 7225, IUER, 'COMP_IONO_MOD', 'Failure in '// &
     &            'running COMP_IONO_RMS for station '//IONO_EST%C_STA(J37) )
              RETURN 
         END IF
!
         IF ( J37 .NE. IND_STA_REF  ) THEN
              IND = INC_STA(J37) 
              IF ( IVRB .GE. 1 ) THEN
                   IF ( IONO_EST%MODE == 1 ) THEN
                        WRITE  ( 6, 160 ) EXP_NAME, IONO_EST%C_STA(J37), EST_VEC(IND), EST_VEC(INI_STA(J37))
  160                   FORMAT ( 'Exp: ', A, ' Sta: ',A, ' Const time offset:   ', 1PD12.5, &
     &                           ' Iono bias: ', 1PD12.5 )
                      ELSE IF  ( IONO_EST%MODE == 2 ) THEN
                        WRITE  ( 6, 170 ) EXP_NAME, IONO_EST%C_STA(J37), EST_VEC(IND), &
     &                                    1.D12*BIAS_STA_AVR(J37), 1.D12*BIAS_STA_RMS(J37), &
     &                                    IONO_AVR, IONO_RMS, IONO_FIT_RMS, 1.D12*IONO_ADJ_STA_ERR(J37)
  170                   FORMAT ( 'Exp: ', A, ' Sta: ',A, ' Time offset: ', 1PD12.5, ' sec ', &
     &                           ' Bias: ', 0PF8.1, ' Rms: ', 0PF8.1, ' ps || ', &
     &                           'Avr: ', F8.1, ' Rms: ', F8.1, ' Fit: ', F8.1, ' ps ', &
     &                           ' adj_err: ', F8.1 )
                   END IF
                   IF ( N_BRK_STA(J37) > 0 ) THEN
                        DO 4400 J40=1,N_BRK_STA(J37)
                           WRITE  ( 6, 180 ) IONO_EST%C_STA(J37), J40, EST_VEC(INC_STA(J37) +J40)
 180                       FORMAT ( 'Station ',A, ' Break ', I1, ' time offset: ', 1PD12.5 )
 4400                   CONTINUE 
                   END IF
              END IF
            ELSE 
              IF ( IVRB .GE. 1 ) THEN
                   IF ( IONO_EST%MODE == 1 ) THEN
                        WRITE  ( 6, 190 ) IONO_EST%C_STA(J37)
  190                   FORMAT ( 'Exp: ', A, ' Sta: ',A, ' Time offset: reference        ' )
                     ELSE IF ( IONO_EST%MODE == 2 ) THEN
                        WRITE  ( 6, 195 ) EXP_NAME, IONO_EST%C_STA(J37), 1.D12*BIAS_STA_AVR(J37), &
     &                                    1.D12*BIAS_STA_RMS(J37), &
     &                                    IONO_AVR, IONO_RMS, IONO_FIT_RMS
 195                    FORMAT ( 'Exp: ', A, ' Sta: ',A, ' Time offset: reference        ', &
     &                           ' Bias: ', 0PF8.1, ' Rms: ',0PF8.1, ' ps || ', &
     &                           'Avr: ', F8.1, ' Rms: ', F8.1, ' Fit: ', F8.1, ' ps ' )
                   END IF
              END IF
         END IF
!
         IF ( IVRB .GE. 8 .AND. ( IONO_EST%MODE == 2 .OR. IONO_EST%MODE == 3 ) ) THEN
              DO 4410 J41=1,NK-1
                 EQU_OBS = 0.0D0
                 TIM_SPL(J41) = ARG_SPL(J41) + 0.5D0*STEP_SPL
                 DO 4420 J42=1-IONO_EST%MDEG,NK-1
                    IND = INI_STA(J37) - 1 + MRES*(J42 + IONO_EST%MDEG - 1) + 1
                    EQU_OBS(IND) = BSPL_VAL ( NK, ARG_SPL, IONO_EST%MDEG, J42, TIM_SPL(J41) )
 4420            CONTINUE 
                 VAL_SPL(J41) = DP_VV_V ( LPAR, EST_VEC, EQU_OBS )
 4410         CONTINUE 
!
              IF ( IVRB .GE. 8 .AND. ( IONO_EST%MODE == 2 .OR. IONO_EST%MODE == 3 ) ) THEN
                   IUER = -1
                   X1 = TIM_SPL/3600.0D0
                   CALL DIAGI_SETDEF ( IUER, 'DIAGI_CTIT', 'Ionospheric bias for station '// &
      &                                IONO_EST%C_STA(J37) )
                   CALL DIAGI_1 ( NK-1, X1, VAL_SPL, IUER ) 
             END IF
         END IF
 4370 CONTINUE 
!
      K_BAS = 0
      ACC_SUM_BAS        = 0.0D0
      AVR_GNSS_BAS_SLA   = 0.0D0
      AVR_GNSS_BAS_ZEN   = 0.0D0
      RMS_GNSS_BAS_SLA   = 0.0D0
      RMS_GNSS_BAS_ZEN   = 0.0D0
      ACC_SQR_BAS        = 0.0D0
      ACC_CRS_BAS        = 0.0D0
      ACC_SUM_BAS_STA    = 0.0D0
      ACC_SQR_BAS_STA    = 0.0D0
      IONO_BIAS_AVR_BAS  = 0.0D0
      IONO_BIAS_RMS_BAS  = 0.0D0
      IONO_ZEN_AVR_BAS   = 0.0D0
      IONO_ZEN_RMS_BAS   = 0.0D0
!
      DO 4430 J43=1,NOBS
         I_BAS = LTM_DIF ( 0, L_BAS, C_BAS, IONO(J43)%STA(1)//'/'//IONO(J43)%STA(2) )
         ACC_SUM_BAS(I_BAS) = ACC_SUM_BAS(I_BAS) + (IONO(J43)%IONO_ZEN(2) - IONO(J43)%IONO_ZEN(1)) 
         ACC_SQR_BAS(I_BAS) = ACC_SQR_BAS(I_BAS) + (IONO(J43)%IONO_ZEN(2) - IONO(J43)%IONO_ZEN(1))**2
         ACC_CRS_BAS(I_BAS) = ACC_CRS_BAS(I_BAS) +  IONO(J43)%IONO_ZEN(1) * IONO(J43)%IONO_ZEN(2)
         K_BAS(I_BAS) = K_BAS(I_BAS) + 1
         DO 4440 J44=1,2
            ACC_SUM_BAS_STA(I_BAS,J44) = ACC_SUM_BAS_STA(I_BAS,J44) + IONO(J43)%IONO_ZEN(J44)
            ACC_SQR_BAS_STA(I_BAS,J44) = ACC_SQR_BAS_STA(I_BAS,J44) + IONO(J43)%IONO_ZEN(J44)**2
 4440    CONTINUE 
         I_STA(1) = LTM_DIF ( 0, IONO_EST%L_STA, IONO_EST%C_STA, C_BAS(I_BAS)(1:8)   )
         I_STA(2) = LTM_DIF ( 0, IONO_EST%L_STA, IONO_EST%C_STA, C_BAS(I_BAS)(10:17) )
         DO 4450 J45=1,2
            EQU_OBS = 0.0D0
            DO 4460 J46=1-IONO_EST%MDEG,NK-1
               IND = INI_STA(I_STA(J45)) - 1 + MRES*(J46 + IONO_EST%MDEG - 1) + 1
               EQU_OBS(IND) = BSPL_VAL ( NK, ARG_SPL, IONO_EST%MDEG, J46, TIM(J43) )
 4460       CONTINUE 
            IONO_BIAS_STA(J45) = DP_VV_V ( LPAR, EST_VEC, EQU_OBS )
 4450    CONTINUE 
         ME = ( IONO(J43)%IONO_MAP(1) + IONO(J43)%IONO_MAP(2))/2.0D0
         AVR_GNSS_BAS_SLA(I_BAS) = AVR_GNSS_BAS_SLA(I_BAS) + IONO_EST%APR_SCALE*IONO(J43)%IONO_G 
         AVR_GNSS_BAS_ZEN(I_BAS) = AVR_GNSS_BAS_ZEN(I_BAS) + IONO_EST%APR_SCALE*IONO(J43)%IONO_G/ME
!
         IONO_BIAS_AVR_BAS(I_BAS) = IONO_BIAS_AVR_BAS(I_BAS) +   IONO_BIAS_STA(2) - IONO_BIAS_STA(1) 
         IONO_BIAS_RMS_BAS(I_BAS) = IONO_BIAS_RMS_BAS(I_BAS) +  (IONO_BIAS_STA(2) - IONO_BIAS_STA(1))**2
         IONO_ZEN_AVR_BAS(I_BAS)  = IONO_ZEN_AVR_BAS(I_BAS)  +  (IONO_BIAS_STA(2) - IONO_BIAS_STA(1))/ME
         IONO_ZEN_RMS_BAS(I_BAS)  = IONO_ZEN_RMS_BAS(I_BAS)  + ((IONO_BIAS_STA(2) - IONO_BIAS_STA(1))/ME)**2
 4430 CONTINUE 
!
      DO 4470 J47=1,L_BAS
         IF ( K_BAS(J47) > 1 ) THEN
              AVR_GNSS_BAS_SLA(J47) = AVR_GNSS_BAS_SLA(J47)/K_BAS(J47)
              AVR_GNSS_BAS_ZEN(J47) = AVR_GNSS_BAS_ZEN(J47)/K_BAS(J47)
         END IF
 4470 CONTINUE 
!
      DO 4480 J48=1,NOBS
         I_BAS = LTM_DIF ( 0, L_BAS, C_BAS, IONO(J48)%STA(1)//'/'//IONO(J48)%STA(2) )
         ME = ( IONO(J48)%IONO_MAP(1) + IONO(J48)%IONO_MAP(2))/2.0D0
         RMS_GNSS_BAS_SLA(I_BAS) = RMS_GNSS_BAS_SLA(I_BAS) + &
     &                             ( IONO_EST%APR_SCALE*IONO(J48)%IONO_G - AVR_GNSS_BAS_SLA(I_BAS) )**2
         RMS_GNSS_BAS_ZEN(I_BAS) = RMS_GNSS_BAS_ZEN(I_BAS) + &
     &                             ( IONO_EST%APR_SCALE*IONO(J48)%IONO_G/ME - AVR_GNSS_BAS_ZEN(I_BAS) )**2
 4480 CONTINUE 
!
! --  AVR_GNSS_BAS_ZEN  -- average    zenith ionosphere path delay from GNSS at a given baseline
! --  RMS_GNSS_BAS_ZEN  -- rms of the zenith ionosphere path delay from GNSS at a given baseline
! --  AVR_GNSS_BAS_ZEN  -- average    slant  ionosphere path delay from GNSS at a given baseline
! --  RMS_GNSS_BAS_ZEN  -- rms of the slant  ionosphere path delay from GNSS at a given baseline
! --  CORR_GNSS_BAS -- correlation for the zenith ionosphere path delay from GNSS between 
! --                   stations of a baseline
!
      DO 4490 J49=1,L_BAS
         IF ( K_BAS(J49) > 1 ) THEN
              RMS_GNSS_BAS_SLA(J49) = DSQRT ( RMS_GNSS_BAS_SLA(J49)/K_BAS(J49) ) 
              RMS_GNSS_BAS_ZEN(J49) = DSQRT ( RMS_GNSS_BAS_ZEN(J49)/K_BAS(J49) ) 
         END IF
!
         DO 4500 J50=1,2
            AVR_GNSS_BAS_STA(J49,J50) = ACC_SUM_BAS_STA(J49,J50)/K_BAS(J49) 
            RMS_GNSS_BAS_STA(J49,J50) = DSQRT ( ACC_SQR_BAS_STA(J49,J50)/K_BAS(J49) - AVR_GNSS_BAS_STA(J49,J50)**2 )
 4500    CONTINUE 
         COV_CRS_GNSS_BAS(J49) = ACC_CRS_BAS(J49)/K_BAS(J49) - AVR_GNSS_BAS_STA(J49,1)*AVR_GNSS_BAS_STA(J49,2)
         CORR_GNSS_BAS(J49)    = COV_CRS_GNSS_BAS(J49) / ( RMS_GNSS_BAS_STA(J49,1)*RMS_GNSS_BAS_STA(J49,2) )
         IF ( K_BAS(J49) > 0 ) THEN
              IONO_BIAS_AVR_BAS(J49) =         IONO_BIAS_AVR_BAS(J49)/K_BAS(J49)
              IONO_BIAS_RMS_BAS(J49) = DSQRT ( IONO_BIAS_RMS_BAS(J49)/K_BAS(J49) )
              IONO_ZEN_AVR_BAS(J49)  =         IONO_ZEN_AVR_BAS(J49)/K_BAS(J49)
              IONO_ZEN_RMS_BAS(J49)  = DSQRT ( IONO_ZEN_RMS_BAS(J49)/K_BAS(J49)  )
         END IF
!         
         C11(J49) = DSQRT ( C11(J49)/MAX(K_BAS(J49),1) )
         C12(J49) = DSQRT ( C12(J49)/MAX(K_BAS(J49),1) )
         C22(J49) = DSQRT ( C22(J49)/MAX(K_BAS(J49),1) )
!
         IF ( IVRB .GE. 1 ) THEN
              IF ( K_BAS(J49) > 1 ) THEN
                   IM = LTM_DIF ( 0, LM_BAS, CM_BAS, C_BAS(J49) )
                   IF ( IM < 1 ) THEN
!
! --------------------- Case of a wrong order of stations in the baseline
!
                        IM = LTM_DIF ( 0, LM_BAS, CM_BAS, C_BAS(J49)(10:17)//'/'//C_BAS(J49)(1:8) )
                   END IF
!
! ---------------- RMS_ful_zen      -- rms of full zenith VLBI path deay with the averaged mapping function (no clock contribution)
! ---------------- RMS_res_sla      -- rms of slant VLBI minus GNSS slant ionospheric path delay that does not includes clock function
! ---------------- RMS_psf_sla      -- rms of postfit residual after taking into account both clock and TEC adjuments
! ---------------- RMS_res_zen      -- rms of zenith VLBI minus GNSS path delay derived with the averaged mapping funcion (no clock contribution)  
! ---------------- AVR_psf_sla      -- average postfit residual after taking into account both clock and TEC adjuments
! ---------------- AVR_psf_zen      -- average zenith postfit residual after taking into account both clock and TEC adjuments derived with the averaged mapping funcion 
! ---------------- RMS_psf_zen      -- rms of the zenith postfit residual after taking into account both clock and TEC adjuments derived with the averaged mapping funcion 
! ---------------- ERR_ioa          -- formal error of ionosphere adjustment 
! ---------------- ERR_rat          -- rms_psf/ERR_ioa
! ---------------- Avr_gnss_bas     -- average or GNSS ionosphere path delay at that baseline (with applying scaling factor!)
! ---------------- RMS_gnss_bas_sla -- rms of the slant  GNSS ionosphere path delay at that baseline (with applying scaling factor!)
! ---------------- RMS_gnss_bas_zen -- rms of the zenith GNSS ionosphere path delay at that baseline (with applying scaling factor!)
! ---------------- AVR_gnss_sta     -- average of the zenith GNSS ionosphere path delay in zenith direction at a given station (1st and 2nd) that participated in a given baseline without applying apriori scale
! ---------------- RMS_gnss_sta     -- rms     of the zenith GNSS ionosphere path delay in zenith direction at a given station (1st and 2nd) that participated in a given baseline without applying apriori scale
! ---------------- Corr_gnss        -- Correlation coefficient between GNSS ionospheric at the 1st and 2nd station in a given baseline
! ---------------- Iono_bias_avr    -- Estimated average ionospheric bias of GNSS wrt VLBI
! ---------------- Iono_bias_rms    -- Estimated rms of the ionospheric bias of GNSS wrt VLBI
! ---------------- Iono_mpf_avr     -- Estimated average ionospheric bias of GNSS wrt VLBI scaled to the zenith
! ---------------- Iono_mpf_rms     -- Estimated rms of the ionospheric bias of GNSS wrt VLBI scaled to the zenith
!

                   WRITE ( 6, 200 ) TRIM(EXP_NAME), C_BAS(J49), K_BAS(J49), &
     &                              1.0D12*FUL_RMS_BAS_ZEN(J49),      &
     &                              1.0D12*RES_RMS_PRE_BAS_SLA(J49),  &
     &                              1.0D12*RES_RMS_PSF_BAS_SLA(J49),  &
     &                              1.0D12*RES_RMS_BAS_ZEN(J49),      &
     &                              1.0D12*RES_AVR_PSF_BAS_SLA(J49),  &
     &                              1.0D12*RES_AVR_PSF_BAS_ZEN(J49),  &
     &                              1.0D12*RES_RMS_PSF_BAS_ZEN(J49),  &
     &                              1.0D12*ERR_IOA_BAS(J49),          &
     &                              ERR_RAT_BAS(J49),                 &
     &                              C11(J49), C12(J49), C22(J49),     &
     &                              1.0D12*AVR_GNSS_BAS_SLA(J49),     &
     &                              1.0D12*RMS_GNSS_BAS_SLA(J49),     &
     &                              1.0D12*AVR_GNSS_BAS_ZEN(J49),     &
     &                              1.0D12*RMS_GNSS_BAS_ZEN(J49),     &
     &                              1.0D12*AVR_GNSS_BAS_STA(J49,1:2), &
     &                              1.0D12*RMS_GNSS_BAS_STA(J49,1:2), &
     &                              CORR_GNSS_BAS(J49),               &
     &                              CORR_VLBI_BAS(J49),               &
     &                              1.0D12*IONO_BIAS_AVR_BAS(J49),    &
     &                              1.0D12*IONO_BIAS_RMS_BAS(J49),    &
     &                              1.0D12*IONO_ZEN_AVR_BAS(J49),     &
     &                              1.0D12*IONO_ZEN_RMS_BAS(J49),     &
     &                              1.0D12*MOD_RMS_BAS(IM)
 200               FORMAT ( 'Exp: ', A10, '  Bas: ', A, ' Num_obs: ', I4, &
     &                      ' RMS_ful_zen: ', F8.1, ' ps ', &
     &                      ' RMS_res_sla: ', F8.1, ' ps ', &
     &                      ' RMS_res_zen: ', F8.1, ' ps ', &
     &                      ' RMS_psf_sla: ', F8.1, ' ps ',     &
     &                      ' AVR_psf_sla: ', F8.1, ' ps ',     &
     &                      ' AVR_psf_zen: ', F8.1, ' ps ', &
     &                      ' RMS_psf_zen: ', F8.1, ' ps ', &
     &                      ' ERR_ioa: ', F8.1, ' ps ',     &
     &                      ' ERR_rat: ', F8.4, ' || ',     &
     &                      ' C11: ', F6.4, ' C12: ', F6.4, ' C22: ', F6.4, ' ||', &
     &                      ' Avr_gnss_bas_sla: ', F8.1, ' RMS_gnss_bas_sla: ', F8.1, &
     &                      ' Avr_gnss_bas_zen: ', F8.1, ' RMS_gnss_bas_zen: ', F8.1, ' ||' &
     &                      ' Avr_gnss_sta: ', F8.1, 1X, F8.1, &
     &                      ' RMS_gnss_sta: ', F8.1, 1X, F8.1, &
     &                      ' Corr_gnss: ', F7.3, ' Corr_vlbi: ', F7.3, &
     &                      ' Iono_bias_avr: ', F8.1, ' ps', &
     &                      ' Iono_bias_rms: ', F8.1, ' ps', &
     &                      ' Iono_avr_zen: ',  F8.1, ' ps', &
     &                      ' Iono_rms_zen: ',  F8.1, ' ps || Iono_mod_rms: ', F8.1, ' ps'  )
              END IF
         END IF
 4490 CONTINUE 
!
      DO 4510 J51=1,NOBS
         I_BAS = LTM_DIF ( 0, L_BAS, C_BAS, IONO(J51)%STA(1)//'/'//IONO(J51)%STA(2) )
         ME = (IONO(J51)%IONO_MAP(1) + IONO(J51)%IONO_MAP(2))/2.0D0
         IONO_MOD_ERR_PSF(J51)  = ERR_IOA(J51)*ERR_RAT_BAS(I_BAS)*DSQRT(CHI_SQ/DEG_FRE)
         IONO_ERR_NRML_PSF(J51) = RES_PSF(J51)/IONO_MOD_ERR_PSF(J51)
         IONO_MOD_ERR_PRE(J51)  = RES_PRE(J51)
         IONO_ERR_NRML_PRE(J51) = RES_PRE(J51)/(RMS_MOD_FUDGE*ME*GET_IONO_MOD_RES_RMS ( RMS_GNSS_BAS_ZEN(I_BAS) ))
!
         IM = LTM_DIF ( 0, LM_BAS, CM_BAS, C_BAS(I_BAS) ) 
         IONO_ERR_NRML_MOD(J51) = RES_PRE(J51)/( RMS_MOD_FUDGE * ME * GET_IONO_MOD_RES_RMS( RMS_GNSS_BAS_ZEN(I_BAS) ) )
!
         IF ( IVRB == 5 ) THEN
              WRITE  ( 6, 222 ) EXP_NAME, J51, IONO(J51)%STA(1), IONO(J51)%STA(2), &
     &                          1.D12*IONO(J51)%IONO_G, &
     &                          1.D12*VLBI_IONO(J51), &
     &                          1.D12*IONO(J51)%IONO_VERR
 222          FORMAT  ( 'EST_IONO_MODE Exp: ', A, ' Ind_obs: ', I5, ' Bas: ', A, 1X, A, &
     &                  ' Iono_gnss: ', F8.2, ' Iono_vlbi: ', F8.2, ' Iono_err: ', F8.2 )
         END IF
!
         IF ( IVRB == 6 ) THEN
              WRITE  ( 6, 220 ) IONO(J51)%IND_REC, IONO(J51)%STA(1), IONO(J51)%STA(2), &
     &                          1.0D12*IONO_EST%APR_SCALE*IONO(J51)%IONO_G, &
     &                          1.0D12*RES_PRE(J51), &
     &                          1.0D12*RES_PSF(J51), &
     &                          1.0D12*IONO(J51)%IONO_VERR, &
     &                          1.0D12*RMS_MOD_FUDGE*ME*GET_IONO_MOD_RES_RMS( RMS_GNSS_BAS_ZEN(I_BAS) ), &
     &                          1.0D12*RMS_MOD_FUDGE*ME*GET_IONO_MOD_RES_RMS( MOD_RMS_BAS(IM) ), &
     &                          1.0D12*IONO_MOD_ERR_PSF(J51)
 220          FORMAT ( 'EST_IONO_RES: Ind_obs: ', I5, ' Bas: ', A, 1X, A, &
     &                 ' Iono_g: ', F8.2, ' Res_clo: ', F8.2, ' Res_psf: ', F8.2, &
     &                 ' || Apr_err: ', F8.2, ' Err_mod_in: ', F8.2, ' Err_mod_out: ', F8.2, &
     &                 ' Err_mod_psf: ', F8.2   )
         END IF
         IF ( IVRB .EQ. 4 ) THEN
              WRITE ( 6, 250 ) EXP_NAME, J51, IONO(J51)%STA(1), IONO(J51)%STA(2), &
     &                         1.D12*RES_PRE(J51), 1.D12*RES_PSF(J51), 1.D12*IONO_MOD_ERR_PSF(J51)
 250          FORMAT ( 'Exp: ', A, ' Obs: ', I6, ' Sta: ',A, 1X, A, &
     &                 ' Res_pre: ', F9.1, ' Res_psf: ', F9.1, ' Err_psf: ', F9.1 )
         END IF
 4510 CONTINUE 
!
!  res_pre  Err_mod_psf
!
      CALL IONO_ERR_DSTR_PAR ( NOBS, IONO_MOD_ERR_PRE, IONO_ERR_NRML_PRE, DSTR_AVR_PRE, DSTR_SIG_PRE, IVRB )
      CALL IONO_ERR_DSTR_PAR ( NOBS, IONO_MOD_ERR_PSF, IONO_ERR_NRML_PSF, DSTR_AVR_PSF, DSTR_SIG_PSF, IVRB )
      CALL IONO_ERR_DSTR_PAR ( NOBS, IONO_MOD_ERR_PRE, IONO_ERR_NRML_MOD, DSTR_AVR_MOD, DSTR_SIG_MOD, IVRB )
      IF ( IVRB .GE. 1 ) THEN
           WRITE  ( 6, 210 ) TRIM(EXP_NAME), NOBS,  1.D12*RES_RMS_PRE_ALL,  &
     &                       1.D12*RES_AVR_PSF_ALL, 1.D12*RES_RMS_PSF_ALL,  &
     &                       ERR_RAT_ALL, 1.D12*FLOOR_VERR, CHI_SQ/DEG_FRE, &
     &                       DSTR_AVR_PRE, DSTR_SIG_PRE, &
     &                       DSTR_AVR_PSF, DSTR_SIG_PSF, &
     &                       DSTR_AVR_MOD, DSTR_SIG_MOD, &
     &                       ERR_SCL, RMS_MOD_FUDGE, IONO_EST%APR_SCALE
 210       FORMAT ( 'Exp: ', A, ' Nobs: ', I6, ' Pre_fit: ', F12.1, &
     &              ' Avr_fit: ',F8.1, ' ps  Post_fit: ', F8.1, &
     &              ' ps | Err_rat: ', F8.3, &
     &              ' Err_floor: ', F6.2, ' ps  Chi_sq/deg_fre= ', F6.2, &
     &              ' | Dstr_Mea_pre: ', F6.3, ' Dstr_sig_pre: ', F6.3,  &
     &                ' Dstr_Mea_psf: ', F6.3, ' Dstr_sig_psf: ', F6.3,  &
     &                ' Dstr_Mea_mod: ', F6.3, ' Dstr_sig_mod: ', F6.3,  &
     &                ' | Err_scl: ', F6.3, ' Rms_mod_fudge: ', F6.3, &
     &                ' Apr_scl: ', F6.3 )
      END IF
!
! --- Reaload IONO. NB: this time we get all the data, not only good!
!
      CALL ERR_PASS ( IUER, IER )
      CALL IONO_READ_INFO ( FIL_IONO, IONO__ABND, M_OBS, KOBS, IONO, EXP_NAME, &
     &                      EXP_VERS, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 7226, IUER, 'COMP_IONO_MOD', 'Error in parsing '// &
     &         'pSolve iono information file '//FIL_IONO )
           RETURN 
      END IF
!
      CALL CLRCH ( STR )
      CALL INCH  ( KOBS, STR )
      CALL CHASHR ( STR(1:6) )
!
      NO = 0
      NO = NO + 1 ; OUT(NO) = LABEL__DTEC
      NO = NO + 1 ; OUT(NO) = '#' 
      NO = NO + 1 ; OUT(NO) = '# Generator: '//IONO_BIAS_ADJ__LABEL
      NO = NO + 1 ; OUT(NO) = '#' 
      NO = NO + 1 ; OUT(NO) = '# Generated on '//GET_CDATE()
      NO = NO + 1 ; OUT(NO) = '#' 
      NO = NO + 1 ; OUT(NO) = '# DTEC_USE:   UPDATE'
      NO = NO + 1 ; OUT(NO) = '# Experiment: '//EXP_NAME
      NO = NO + 1 ; OUT(NO) = '# Nobs:       '//TRIM(STR)
      NO = NO + 1 ; OUT(NO) = '#' 
      DO 4520 J52=1,KOBS
         NO = NO + 1
!
! ------ Fill TIM array with epochs
!
         IF ( J52 == 1 ) THEN
              MJD_0 = IONO(J52)%MJD
              TAI_0 = IONO(J52)%TAI
         END IF 
         TIM(J52) = (IONO(J52)%MJD - MJD_0)*86400.0D0 + &
     &              (IONO(J52)%TAI - TAI_0)
         IF ( IONO_EST%MODE == 2 .OR. IONO_EST%MODE == 3 ) THEN 
              IF ( TIM(J52) < ARG_SPL(1) + 0.5D-4*STEP_SPL ) THEN
                   TIM(J52) = ARG_SPL(1) + 0.5D-4*STEP_SPL
              END IF
              IF ( TIM(J52) > ARG_SPL(NK) - 0.5D-4*STEP_SPL ) THEN
                   TIM(J52) = ARG_SPL(NK) - 0.5D-4*STEP_SPL
              END IF
         END IF
!
         IND_BAS = LTM_DIF ( 0, L_BAS, C_BAS, IONO(J52)%STA(1)//'/'//IONO(J52)%STA(2) )
         IF ( IND_BAS < 1 ) THEN
!
! ----------- This may be because only one band was available at a given baseline
!
              WRITE ( UNIT=OUT(NO), FMT=230 ) J52, &
     &                                        IONO(J52)%IONO_ZEN(1)*IONO(J52)%IONO_MAP(1)/VIO__CONST*IONO__FREQ_REF**2, &
     &                                        IONO(J52)%IONO_ZEN(2)*IONO(J52)%IONO_MAP(2)/VIO__CONST*IONO__FREQ_REF**2, & 
     &                                        0.0D0, 0.0D0, 0.0D0, 0.0D0, .FALSE., .FALSE., .FALSE., 0.0D0, 0.0D0, 0.D0 
 230          FORMAT ( 'Ind_obs: ', I6, '  TEC_GPS: ', F10.2, 1X, F10.2, &
     &                 '  dTEC_adj: ', F9.3, 1X, F9.3, '  Err_dTEC: ', F8.3, &
     &                 '  Del_Bias: ', 1PD13.6, &
     &                 '  Band_use: ', L1, 1X, L1, 1X, L1, &
     &                 '  dTEC_VmG: ', 0PF9.3, '  VmG_res: ', 0PF9.3, '  Viono_err: ', 0PF9.3 )
              GOTO 4520
         END IF
!
! ------ Check whether this observation with index J52 was used for estimation
!
         IND_USED_REC = -1
         DO 4530 J53=1,NOBS
            IF ( IND_REC(J53) == J52 ) THEN
                 IND_USED_REC = J53
            END IF
 4530    CONTINUE 
!
         EQU_OBS = 0.0
         DO 4540 J54=1,2
            I_STA(J54) = ADD_CLIST ( M_STA, IONO_EST%L_STA, IONO_EST%C_STA, IONO(J52)%STA(J54), IER )
            IF ( INC_STA(I_STA(J54)) .NE. 0 ) THEN
                 IND = INC_STA(I_STA(J54)) 
                 EQU_OBS(IND) = SGN(J54)
                 DO 4550 J55=1,N_BRK_STA(I_STA(J54))
                    IF ( J52 .GE. IND_REC_BRK_STA(J55,I_STA(J54)) ) THEN
                         IND = INC_STA(I_STA(J54)) + J55
                         EQU_OBS(IND) = SGN(J54)
                    END IF
 4550            CONTINUE 
            END IF
!
            IF ( INB(IND_BAS) > 0 ) THEN
                 EQU_OBS(INB(IND_BAS)) = 1.0D0
            END IF
!
            IF ( IONO_EST%MODE == 1 ) THEN
                 IND = INI_STA(I_STA(J54))
                 EQU_OBS(IND)   = SGN(J54)*IONO(J52)%IONO_MAP(J54)
               ELSE IF ( IONO_EST%MODE == 2 .OR. IONO_EST%MODE == 3 ) THEN 
                 DO 4560 J56=1-IONO_EST%MDEG,NK-1
                    IF ( IONO_EST%MODE == 2 ) THEN
                         IND = INI_STA(I_STA(J54)) - 1 + J56 + IONO_EST%MDEG
                       ELSE IF ( IONO_EST%MODE == 3 ) THEN
                         IF ( IONO(J52)%EL(J54) > 27.0D0 ) THEN
                              IND = INI_STA(I_STA(J54)) - 1 + MRES*(J56 + IONO_EST%MDEG - 1) + 1
                            ELSE IF ( IONO(J52)%IONO_MAP(J54) < 90.0D0 ) THEN
                              IND = INI_STA(I_STA(J54)) - 1 + MRES*(J56 + IONO_EST%MDEG - 1) + 2
                            ELSE IF ( IONO(J52)%IONO_MAP(J54) < 180.0D0 ) THEN
                              IND = INI_STA(I_STA(J54)) - 1 + MRES*(J56 + IONO_EST%MDEG - 1) + 3
                            ELSE IF ( IONO(J52)%IONO_MAP(J54) < 270.0D0 ) THEN
                              IND = INI_STA(I_STA(J54)) - 1 + MRES*(J56 + IONO_EST%MDEG - 1) + 4
                            ELSE IF ( IONO(J52)%IONO_MAP(J54) < 360.1D0 ) THEN
                              IND = INI_STA(I_STA(J54)) - 1 + MRES*(J56 + IONO_EST%MDEG - 1) + 5
                         END IF
                    END IF
                    EQU_OBS(IND) = BSPL_VAL ( NK, ARG_SPL, IONO_EST%MDEG, J56, TIM(J52) ) * &
     &                             SGN(J54)*IONO(J52)%IONO_MAP(J54)
 4560            CONTINUE 
            END IF
 4540    CONTINUE 
!
! ------ DEL_BIAS is the ionosphere-free delay bias between bands (upper band minus lower band)
!
         DEL_BIAS = DP_VV_V ( IONO_EST%L_STA - 1 + N_TOT_BRK + N_BCL_EXP, EST_VEC, EQU_OBS ) 
         IONO_DEL_ADJ(1) = SGN(1) * DP_VV_V ( NK+2, EST_VEC(INI_STA(I_STA(1))), EQU_OBS(INI_STA(I_STA(1))) ) 
         IONO_DEL_ADJ(2) = SGN(2) * DP_VV_V ( NK+2, EST_VEC(INI_STA(I_STA(2))), EQU_OBS(INI_STA(I_STA(2))) ) 
!
         CALL MUL_MV_SV_V ( LPAR, NOR_MAT, LPAR, EQU_OBS, LPAR, NOR_VEC, IER )
         IONO_DEL_ADJ_ERR = SCL * DSQRT ( DP_VV_V ( LPAR, EQU_OBS, NOR_VEC ) ) * &
     &                            ERR_RAT_BAS(IND_BAS) * DSQRT(CHI_SQ/DEG_FRE)
!
         DTEC     = IONO_DEL_ADJ    /VIO__CONST*IONO__FREQ_REF**2 
         DTEC_ERR = IONO_DEL_ADJ_ERR/VIO__CONST*IONO__FREQ_REF**2
         IF ( IND_USED_REC > 0 ) THEN
              DTEC_VMG  = RES_PRE(IND_USED_REC) * IONO__FREQ_REF**2/VIO__CONST
              VIONO_ERR = DSQRT( IONO(J52)%IONO_VERR**2 + FLOOR_VERR**2 )
            ELSE
              DTEC_VMG = 0.0
              VIONO_ERR = 0.0
         END IF
         IF ( IND_USED_REC > 0 ) THEN
              RH  = IONO(J52)%IONO_V - IONO_EST%APR_SCALE*IONO(J52)%IONO_G 
              RES = RH - DP_VV_V ( LPAR, EST_VEC, EQU_OBS ) 
!!              write ( 6, * ) 'ZZ IND_OBS: ' ,j49, ' res= ', sngl(res), ' est= ', sngl ( DP_VV_V ( LPAR, EST_VEC, EQU_OBS ) ), ' eee= ', sngl(res_pre(ind_used_rec) + del_bias) ! %%%%%%%%%%%%
            ELSE
              RES = 0.0D0
         END IF
!
         WRITE ( UNIT=OUT(NO), FMT=230 ) J52, &
     &                                   IONO(J52)%IONO_ZEN(1)*IONO(J52)%IONO_MAP(1)/VIO__CONST*IONO__FREQ_REF**2, &
     &                                   IONO(J52)%IONO_ZEN(2)*IONO(J52)%IONO_MAP(2)/VIO__CONST*IONO__FREQ_REF**2, & 
     &                                   DTEC, &
     &                                   DTEC_ERR, &
     &                                   DEL_BIAS*IONO__FREQ_REF**2/IONO(J52)%FREQ_EFF(1)**2* &
     &                                           (IONO(J52)%FREQ_EFF(2)**2 - IONO(J52)%FREQ_EFF(1)**2)/ &
     &                                            IONO(J52)%FREQ_EFF(2)**2, &
     &                                   IONO(J52)%USED(1), &
     &                                   IONO(J52)%USED(2), &
     &                                   IONO(J52)%USED(3), &
     &                                   DTEC_VMG, &
     &                                   RES*IONO__FREQ_REF**2/VIO__CONST, &
     &                                   VIONO_ERR*IONO__FREQ_REF**2/VIO__CONST
 4520 CONTINUE 
!
      CALL ERR_PASS ( IUER, IER )
      CALL WR_TEXT  ( NO, OUT, FIL_DTEC, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 7227, IUER, 'COMP_IONO_MOD', 'Failure in '// &
     &         'an attemt to write dTEC into output file '//FIL_DTEC )
           RETURN 
      END IF
!
!     (PI2)**2/ALPHA__VIO*IONO__FREQ_REF**2 = 4.76D10
!
      DEALLOCATE ( NOR_MAT, CNS_MAT, NOR_VEC, EQU_OBS, EST_VEC )
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE COMP_IONO_MOD  !#!  
