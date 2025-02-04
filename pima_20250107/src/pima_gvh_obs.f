      SUBROUTINE PIMA_GVH_OBS ( PIM, PIM_2ND, GVH, NUMB_STA, NUMB_SOU, &
     &                          NUMB_SCA, NUMB_BAS, C_STA, C_SOU, C_SCA, &
     &                          C_BAS, IND_SCA, IND_OBS, IND_DB_OBS, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  PIMA_GVH_OBS
! *                                                                      *
! *  ### 15-JUL-2009  PIMA_GVH_OBS   v1.4 (c) L. Petrov  03-MAY-2020 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'astro_constants.i'
      INCLUDE   'pima.i'
      INCLUDE   'pima_db.i'
      INCLUDE   'gvh.i'
      TYPE     ( PIMA__TYPE ) :: PIM, PIM_2ND
      TYPE     ( GVH__STRU  ) :: GVH
      INTEGER*4  NUMB_STA, NUMB_SOU, NUMB_SCA, NUMB_BAS, IND_SCA, IND_OBS, &
     &           IND_DB_OBS, IUER
      CHARACTER  C_STA(NUMB_STA)*(*), C_SOU(NUMB_SOU)*(*), &
     &           C_SCA(NUMB_SCA)*(*), C_BAS(NUMB_BAS)*(*)
      REAL*8     UTC_OBS, SNR(PIM__MBND), &
     &           ION_GDEL, ION_GERR, ION_PRAT, ION_RERR, ION_GF(2), &
     &           ION_RF(2), ATM_PRES(2), AIR_TEMP(2), &
     &           HUMID(2), CABLE(2), TIM_1ST_AP
      CHARACTER  QUALCODE(PIM__MBND)*1, POLARIZ_CH(PIM__MBND)*2
      CHARACTER  STR*32, STR1*32
      LOGICAL*4  FL_BAS_REVERSE
      INTEGER*4  J0, J1, J2, J3, J4, J5, J6, J7, J8, J9, J10, J11, J12, J13, &
     &           IND_OBS_2ND, IND_FRG, MJD_OBS, IDAY, SOU_IND, STA_IND(2), &
     &           IND, INDO(2), USER_SUP, USER_REC, AUTO_SUP, SWAP_STA, &
     &           N_GRAMB(PIM__MBND), N_PHAMB(PIM__MBND), &
     &           ARR_I4(64), NUMB_BND, IND_POL(2), KFRQ, U_FRQ, U_FRG, &
     &           FRG_IND, UV_IND, IER
      INTEGER*2  IND_CHN1(PIM__MFRQ), IND_CHN2(PIM__MFRQ), &
     &           NUM_AP1(PIM__MFRQ,2),  NUM_AP2(PIM__MFRQ,2), &
     &           BAND_2ND, NUSEDCHN(PIM__MBND), ARR_I2(64), UV_STA_ORDER
      REAL*8     NUM_SAM1(PIM__MFRQ,2), NUM_SAM2(PIM__MFRQ,2), &
     &           POLY_VAL(PIM__MBND), GAIN(PIM__MBND,2), &
     &           PCAL_FR1(PIM__MFRQ,2), PCAL_FR2(PIM__MFRQ,2), &
     &           TOTPHASE(PIM__MBND), PHDELERR(PIM__MBND), &
     &           TOTPHSGC(PIM__MBND), TOTMBDEL(PIM__MBND), &
     &           GRDELERR(PIM__MBND), DEL_RATE(PIM__MBND), &
     &           PHRATERR(PIM__MBND), RESMBDEL(PIM__MBND), &
     &           RESPHRAT(PIM__MBND), RESPHAS(PIM__MBND),  &
     &           GAIN_POLY, ARR_R8(64)
      REAL*8     NOI__MIN
      PARAMETER  ( NOI__MIN = 1.D-9 )
      REAL*4     PCAL_AMP, PCAL_PHS, ARR_R4(64), &
     &           TSYS1(PIM__MFRQ,2),  TSYS2(PIM__MFRQ,2), &
     &           AGAIN(PIM__MBND,2)
      COMPLEX*8  PCAL_CM1(PIM__MFRQ,2), PCAL_CM2(PIM__MFRQ,2)
!
      COMPLEX*8  UV_CHN1(PIM__MFRQ), UV_CHN2(PIM__MFRQ)
      INTEGER*4, EXTERNAL :: I_LEN, ILEN, LTM_DIF
      LOGICAL*4, EXTERNAL :: IS_R4_NAN, IS_R8_NAN
      CHARACTER, EXTERNAL :: MJDSEC_TO_DATE*30
!
      IND_OBS_2ND = PIM%OBS(IND_OBS)%IND_OBS_2ND
      IF ( IND_OBS_2ND == 0 ) IND_OBS_2ND = IND_OBS
      INDO(1) = IND_OBS
      INDO(2) = IND_OBS_2ND
      IF ( ILEN(PIM%CONF%MKDB_2ND_BAND_FILE) == 0 ) THEN
           NUMB_BND = 1
         ELSE 
           NUMB_BND = 2
      END IF
      IF ( PIM%CONF%FRG_USE == PIMA__COMBINE ) THEN
!
! -------- If the frequency group is combined, we search for the first 
! -------- frequency group that is not empty, i.e. has accumulation
! -------- periods
!
           DO 400 J0=PIM%CONF%FRG_LIST(1),PIM%CONF%FRG_LIST(2)
              FRG_IND = PIM%OBS(IND_OBS)%REF_FRG_INDS(J0)
              IF ( FRG_IND == 0 ) GOTO 400
              UV_IND  = PIM%OBS(IND_OBS)%UV_IND(1,FRG_IND)
              IF ( UV_IND == 0 ) GOTO 400
              GOTO 800
 400       CONTINUE 
 800       CONTINUE 
         ELSE
!
! -------- Normal case: the frequency group index is fixed
!
           FRG_IND = PIM%OBS(IND_OBS)%REF_FRG_INDS(PIM%CONF%FRQ_GRP) 
      END IF
!
      IF ( FRG_IND > 0 ) THEN
           IF ( PIM%NFRG == 1 ) THEN
                TIM_1ST_AP = PIM%TIM_R8(PIM%OBS(IND_OBS)%TIM_BEG_IND)
              ELSE
                TIM_1ST_AP = PIM%TIM_R8( PIM%UV_IND(PIM%OBS(IND_OBS)%UV_IND(1,FRG_IND))%TIM_IND )
           END IF
         ELSE
!
! -------- Normally, this should not happen. That means no real data are 
! -------- available for this observation at this frequency group.
! -------- We put the time tag of the first frequency group arguing that 
! -------- in this case it does not matter what is the precise time tag.
!
           TIM_1ST_AP = PIM%TIM_R8(PIM%OBS(IND_OBS)%TIM_BEG_IND) 
      END IF
!
      UTC_OBS = PIM%TAI_0 + TIM_1ST_AP + PIM%OBS(IND_OBS)%SRT_OFFSET + PIM%UTC_MTAI
      IDAY = IDINT(UTC_OBS/86400.0D0)
      UTC_OBS = UTC_OBS - IDAY*86400.0D0
      MJD_OBS = PIM%MJD_0 + IDAY
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PLCODE ( GVH, 'MJD_OBS ', IND_DB_OBS, 0, MJD_OBS, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 7901, IUER, 'PIMA_GVH_OBS', 'Error in putting '// &
     &         '"MJD_OBS" lcode' )
           RETURN
      END IF
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!   str  = mjdsec_to_date ( mjd_obs, utc_obs, ier )                        ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!   str1 = mjdsec_to_date ( mjd_obs, utc_obs - pim%utc_mtai, ier )         ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!   write ( 6, * ) ' PGO: ', ind_obs, ' utc= ', str(1:24), ' tai = ', str1(1:24),  ' utc_obs= ', utc_obs ! %%%%%%%%
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PLCODE ( GVH, 'UTC_OBS ', IND_DB_OBS, 0, UTC_OBS, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 7902, IUER, 'PIMA_GVH_OBS', 'Error in putting '// &
     &         '"MJD_OBS" lcode' )
           RETURN
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PLCODE ( GVH, 'SCANNAME', IND_DB_OBS, 0, C_SCA(IND_SCA), IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 7903, IUER, 'PIMA_GVH_OBS', 'Error in putting '// &
     &         '"SCANNAME" lcode' )
           RETURN
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PLCODE ( GVH, 'SCANPIMA', IND_DB_OBS, 0, &
     &                  PIM%SCA(PIM%OBS(IND_OBS)%SCA_IND)%SCAN_NAME, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 7904, IUER, 'PIMA_GVH_OBS', 'Error in putting '// &
     &         '"SCANPIMA" lcode' )
           RETURN
      END IF
!
      SOU_IND = LTM_DIF ( 0, NUMB_SOU, C_SOU, &
     &                    PIM%C_SOU(PIM%OBS(IND_OBS)%SOU_IND) )
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PLCODE ( GVH, 'SOU_IND ', IND_DB_OBS, 0, SOU_IND, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 7905, IUER, 'PIMA_GVH_OBS', 'Error in putting '// &
     &         '"SOU_IND " lcode' )
           RETURN
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PLCODE ( GVH, 'FRT_OFFS', IND_DB_OBS, 0, &
     &                  PIM%OBS(IND_OBS)%FRT_OFFSET, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 7906, IUER, 'PIMA_GVH_OBS', 'Error in putting '// &
     &         '"FRT_OFFS " lcode' )
           RETURN
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PLCODE ( GVH, 'SRT_OFFS', IND_DB_OBS, 0, &
     &                  PIM%OBS(IND_OBS)%SRT_OFFSET, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 7907, IUER, 'PIMA_GVH_OBS', 'Error in putting '// &
     &         '"SRT_OFFS " lcode' )
           RETURN
      END IF
!
      USER_SUP = 0
      USER_REC = 0
      USER_SUP = IBSET ( USER_SUP, GOOD__SPS  )
      USER_SUP = IBSET ( USER_SUP, INIT__SPS  )
      USER_REC = IBSET ( USER_SUP, INIT__UAS )
      BAND_2ND = 0
      QUALCODE = '0'
      IF ( NUMB_BND == 2 ) THEN
           BAND_2ND = IBSET ( BAND_2ND, OBS__2BN )
           BAND_2ND = IBSET ( BAND_2ND, AVL__2BN )
      END IF
      DO 410 J1=1,NUMB_BND
         IF ( BTEST ( PIM%OBS(INDO(J1))%FRI_STS(J1), FRI__PIM ) ) THEN
              QUALCODE(J1) = '9'
              IF ( BTEST ( PIM%OBS(INDO(J1))%FRI_STS(J1), FAI__PIM ) ) THEN
                   IF ( J1 == 1 ) USER_SUP = IBSET ( USER_SUP, NOFX__SPS )
                   IF ( J1 == 2 ) USER_SUP = IBSET ( USER_SUP, NOFS__SPS )
                   IF ( J1 == 2 ) USER_SUP = IBSET ( USER_SUP, CBAD__SPS )
                   IF ( J1 == 2 ) BAND_2ND = IBSET ( BAND_2ND, DET__2BN )
                   QUALCODE(J1) = 'B'
                 ELSE IF ( BTEST ( PIM%OBS(INDO(J1))%FRI_STS(J1), NDA__PIM ) ) THEN
                   IF ( J1 == 1 ) USER_SUP = IBSET ( USER_SUP, NOFX__SPS )
                   IF ( J1 == 2 ) USER_SUP = IBSET ( USER_SUP, NOFS__SPS )
                   IF ( J1 == 2 ) USER_SUP = IBSET ( USER_SUP, CBAD__SPS )
                   QUALCODE(J1) = 'A'
                 ELSE IF ( BTEST ( PIM%OBS(INDO(J1))%FRI_STS(J1), NPC__PIM ) ) THEN
                   IF ( J1 == 1 ) USER_SUP = IBSET ( USER_SUP, NOFX__SPS )
                   IF ( J1 == 2 ) USER_SUP = IBSET ( USER_SUP, NOFS__SPS )
                   IF ( J1 == 2 ) USER_SUP = IBSET ( USER_SUP, CBAD__SPS )
                   QUALCODE(J1) = 'C'
                 ELSE IF ( BTEST ( PIM%OBS(INDO(J1))%FRI_STS(J1), NOC__PIM ) ) THEN
                   IF ( J1 == 1 ) USER_SUP = IBSET ( USER_SUP, NOFX__SPS )
                   IF ( J1 == 2 ) USER_SUP = IBSET ( USER_SUP, NOFS__SPS )
                   IF ( J1 == 2 ) USER_SUP = IBSET ( USER_SUP, CBAD__SPS )
                   QUALCODE(J1) = 'D'
                 ELSE IF ( BTEST ( PIM%OBS(INDO(J1))%FRI_STS(J1), NDT__PIM ) ) THEN
                   IF ( J1 == 1 ) USER_SUP = IBSET ( USER_SUP, NOFX__SPS )
                   IF ( J1 == 2 ) USER_SUP = IBSET ( USER_SUP, NOFS__SPS )
                   IF ( J1 == 2 ) USER_SUP = IBSET ( USER_SUP, CBAD__SPS )
                   QUALCODE(J1) = '0'
              END IF
            ELSE
              IF ( J1 == 1 ) USER_SUP = IBSET ( USER_SUP, NOFX__SPS )
              IF ( J1 == 2 ) USER_SUP = IBSET ( USER_SUP, NOFS__SPS )
              IF ( J1 == 2 ) USER_SUP = IBSET ( USER_SUP, CBAD__SPS )
              IF ( J1 == 2 ) BAND_2ND = IBSET ( BAND_2ND, DET__2BN )
              QUALCODE(J1) = 'B'
         END IF
         IF ( PIM%OBS(INDO(J1))%NOISE(J1) > NOI__MIN ) THEN
              SNR(J1) = PIM%OBS(INDO(J1))%AMPL(PIMA__DRF,J1)/PIM%OBS(INDO(J1))%NOISE(J1)
            ELSE
              SNR(J1) = 0.0D0
         END IF
 410  CONTINUE
      STA_IND(1) = LTM_DIF ( 0, NUMB_STA, C_STA, &
     &                       PIM%C_STA(PIM%OBS(IND_OBS)%STA_IND(1)) )
      STA_IND(2) = LTM_DIF ( 0, NUMB_STA, C_STA, &
     &                       PIM%C_STA(PIM%OBS(IND_OBS)%STA_IND(2)) )
      IF ( STA_IND(1) > STA_IND(2) ) THEN
           SWAP_STA = STA_IND(1)
           STA_IND(1) = STA_IND(2)
           STA_IND(2) = SWAP_STA
           FL_BAS_REVERSE = .TRUE.
         ELSE
           FL_BAS_REVERSE = .FALSE.
      END IF
      AUTO_SUP = USER_SUP
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PLCODE ( GVH, 'STA_IND ', IND_DB_OBS, 0, STA_IND, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 7908, IUER, 'PIMA_GVH_OBS', 'Error in putting '// &
     &         '"STA_IND " lcode' )
           RETURN
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PLCODE ( GVH, 'QUALCODE', IND_DB_OBS, 0, QUALCODE(1:NUMB_BND), IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 7909, IUER, 'PIMA_GVH_OBS', 'Error in putting '// &
     &         '"QUALCODE" lcode' )
           RETURN
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PLCODE ( GVH, 'BAND_2ND', IND_DB_OBS, 0, BAND_2ND, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 7910, IUER, 'PIMA_GVH_OBS', 'Error in putting '// &
     &         '"BAND_2ND" lcode' )
           RETURN
      END IF
!
      ARR_R8(1) = PIM%OBS(IND_OBS)%AMPL(PIMA__DRF,1)
      IF ( NUMB_BND == 2 ) THEN
           ARR_R8(2) = PIM%OBS(IND_OBS_2ND)%AMPL(PIMA__DRF,2)
      END IF
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PLCODE ( GVH, 'FRN_AMPL', IND_DB_OBS, 0, ARR_R8, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 7911, IUER, 'PIMA_GVH_OBS', 'Error in putting '// &
     &         '"FRN_AMPL" lcode' )
           RETURN
      END IF
!
      IF ( PIM%CONF%MKDB_FRINGE_ALGORITHM == PIMA__FRA_DRF ) THEN
           TOTPHASE(1) = PIM%OBS(IND_OBS)%TOT_PHS(PIMA__DRF,1)
           PHDELERR(1) = PIM%OBS(IND_OBS)%PH_DEL_ERR(PIMA__DRF,1)
           TOTPHSGC(1) = PIM%OBS(IND_OBS)%TOT_PHS_GC(PIMA__DRF,1)
           TOTMBDEL(1) = PIM%OBS(IND_OBS)%TOT_MB_DEL(PIMA__DRF,1)
           GRDELERR(1) = PIM%OBS(IND_OBS)%MB_DEL_ERR(PIMA__DRF,1)
           DEL_RATE(1) = PIM%OBS(IND_OBS)%TOT_PH_RAT(PIMA__DRF,1)
           PHRATERR(1) = PIM%OBS(IND_OBS)%PH_RAT_ERR(PIMA__DRF,1)
           RESMBDEL(1) = PIM%OBS(IND_OBS)%RES_MB_DEL(PIMA__DRF,1)
           RESPHRAT(1) = PIM%OBS(IND_OBS)%RES_PH_RAT(PIMA__DRF,1)
           RESPHAS(1)  = PIM%OBS(IND_OBS)%RES_PHS(PIMA__DRF,1)
           IF ( NUMB_BND == 2 ) THEN
                TOTPHASE(2) = PIM%OBS(IND_OBS_2ND)%TOT_PHS(PIMA__DRF,2)
                PHDELERR(2) = PIM%OBS(IND_OBS_2ND)%PH_DEL_ERR(PIMA__DRF,2)
                TOTPHSGC(2) = PIM%OBS(IND_OBS_2ND)%TOT_PHS_GC(PIMA__DRF,2)
                TOTMBDEL(2) = PIM%OBS(IND_OBS_2ND)%TOT_MB_DEL(PIMA__DRF,2)
                GRDELERR(2) = PIM%OBS(IND_OBS_2ND)%MB_DEL_ERR(PIMA__DRF,2)
                DEL_RATE(2) = PIM%OBS(IND_OBS_2ND)%TOT_PH_RAT(PIMA__DRF,2)
                PHRATERR(2) = PIM%OBS(IND_OBS_2ND)%PH_RAT_ERR(PIMA__DRF,2)
                RESMBDEL(2) = PIM%OBS(IND_OBS_2ND)%RES_MB_DEL(PIMA__DRF,2)
                RESPHRAT(2) = PIM%OBS(IND_OBS_2ND)%RES_PH_RAT(PIMA__DRF,2)
                RESPHAS(2)  = PIM%OBS(IND_OBS)%RES_PHS(PIMA__DRF,2)
           END IF
         ELSE IF ( PIM%CONF%MKDB_FRINGE_ALGORITHM == PIMA__FRA_LSQ ) THEN
           TOTPHASE(1) = PIM%OBS(IND_OBS)%TOT_PHS(PIMA__LSQ,1)
           PHDELERR(1) = PIM%OBS(IND_OBS)%PH_DEL_ERR(PIMA__LSQ,1)
           TOTPHSGC(1) = PIM%OBS(IND_OBS)%TOT_PHS_GC(PIMA__LSQ,1)
           TOTMBDEL(1) = PIM%OBS(IND_OBS)%TOT_MB_DEL(PIMA__LSQ,1)
           GRDELERR(1) = PIM%OBS(IND_OBS)%MB_DEL_ERR(PIMA__LSQ,1)
           DEL_RATE(1) = PIM%OBS(IND_OBS)%TOT_PH_RAT(PIMA__LSQ,1)
           PHRATERR(1) = PIM%OBS(IND_OBS)%PH_RAT_ERR(PIMA__LSQ,1)
           RESMBDEL(1) = PIM%OBS(IND_OBS)%RES_MB_DEL(PIMA__LSQ,1)
           RESPHRAT(1) = PIM%OBS(IND_OBS)%RES_PH_RAT(PIMA__LSQ,1)
           RESPHAS(1)  = PIM%OBS(IND_OBS)%RES_PHS(PIMA__LSQ,1)
           IF ( NUMB_BND == 2 ) THEN
                TOTPHASE(2) = PIM%OBS(IND_OBS_2ND)%TOT_PHS(PIMA__LSQ,2)
                PHDELERR(2) = PIM%OBS(IND_OBS_2ND)%PH_DEL_ERR(PIMA__LSQ,2)
                TOTPHSGC(2) = PIM%OBS(IND_OBS_2ND)%TOT_PHS_GC(PIMA__LSQ,2)
                TOTMBDEL(2) = PIM%OBS(IND_OBS_2ND)%TOT_MB_DEL(PIMA__LSQ,2)
                GRDELERR(2) = PIM%OBS(IND_OBS_2ND)%MB_DEL_ERR(PIMA__LSQ,2)
                DEL_RATE(2) = PIM%OBS(IND_OBS_2ND)%TOT_PH_RAT(PIMA__LSQ,2)
                PHRATERR(2) = PIM%OBS(IND_OBS_2ND)%PH_RAT_ERR(PIMA__LSQ,2)
                RESMBDEL(2) = PIM%OBS(IND_OBS_2ND)%RES_MB_DEL(PIMA__LSQ,2)
                RESPHRAT(2) = PIM%OBS(IND_OBS_2ND)%RES_PH_RAT(PIMA__LSQ,2)
                RESPHAS(2)  = PIM%OBS(IND_OBS)%RES_PHS(PIMA__LSQ,2)
           END IF
         ELSE IF ( PIM%CONF%MKDB_FRINGE_ALGORITHM == PIMA__FRA_MUL ) THEN
           TOTPHASE(1) = PIM%OBS(IND_OBS)%TOT_PHS(PIMA__MUL,1)
           PHDELERR(1) = PIM%OBS(IND_OBS)%PH_DEL_ERR(PIMA__MUL,1)
           TOTPHSGC(1) = PIM%OBS(IND_OBS)%TOT_PHS_GC(PIMA__MUL,1)
           TOTMBDEL(1) = PIM%OBS(IND_OBS)%TOT_MB_DEL(PIMA__MUL,1)
           GRDELERR(1) = PIM%OBS(IND_OBS)%MB_DEL_ERR(PIMA__MUL,1)
           DEL_RATE(1) = PIM%OBS(IND_OBS)%TOT_PH_RAT(PIMA__MUL,1)
           PHRATERR(1) = PIM%OBS(IND_OBS)%PH_RAT_ERR(PIMA__MUL,1)
           RESMBDEL(1) = PIM%OBS(IND_OBS)%RES_MB_DEL(PIMA__MUL,1)
           RESPHRAT(1) = PIM%OBS(IND_OBS)%RES_PH_RAT(PIMA__MUL,1)
           RESPHAS(1)  = PIM%OBS(IND_OBS)%RES_PHS(PIMA__MUL,1)
           IF ( NUMB_BND == 2 ) THEN
                TOTPHASE(2) = PIM%OBS(IND_OBS_2ND)%TOT_PHS(PIMA__MUL,2)
                PHDELERR(2) = PIM%OBS(IND_OBS_2ND)%PH_DEL_ERR(PIMA__MUL,2)
                TOTPHSGC(2) = PIM%OBS(IND_OBS_2ND)%TOT_PHS_GC(PIMA__MUL,2)
                TOTMBDEL(2) = PIM%OBS(IND_OBS_2ND)%TOT_MB_DEL(PIMA__MUL,2)
                GRDELERR(2) = PIM%OBS(IND_OBS_2ND)%MB_DEL_ERR(PIMA__MUL,2)
                DEL_RATE(2) = PIM%OBS(IND_OBS_2ND)%TOT_PH_RAT(PIMA__MUL,2)
                PHRATERR(2) = PIM%OBS(IND_OBS_2ND)%PH_RAT_ERR(PIMA__MUL,2)
                RESMBDEL(2) = PIM%OBS(IND_OBS_2ND)%RES_MB_DEL(PIMA__MUL,2)
                RESPHRAT(2) = PIM%OBS(IND_OBS_2ND)%RES_PH_RAT(PIMA__MUL,2)
                RESPHAS(2)  = PIM%OBS(IND_OBS)%RES_PHS(PIMA__MUL,2)
           END IF
         ELSE IF ( PIM%CONF%MKDB_FRINGE_ALGORITHM == PIMA__FRA_ADD ) THEN
           TOTPHASE(1) = PIM%OBS(IND_OBS)%TOT_PHS(PIMA__ADD,1)
           PHDELERR(1) = PIM%OBS(IND_OBS)%PH_DEL_ERR(PIMA__ADD,1)
           TOTPHSGC(1) = PIM%OBS(IND_OBS)%TOT_PHS_GC(PIMA__ADD,1)
           TOTMBDEL(1) = PIM%OBS(IND_OBS)%TOT_MB_DEL(PIMA__ADD,1)
           GRDELERR(1) = PIM%OBS(IND_OBS)%MB_DEL_ERR(PIMA__ADD,1)
           DEL_RATE(1) = PIM%OBS(IND_OBS)%TOT_PH_RAT(PIMA__ADD,1)
           PHRATERR(1) = PIM%OBS(IND_OBS)%PH_RAT_ERR(PIMA__ADD,1)
           RESMBDEL(1) = PIM%OBS(IND_OBS)%RES_MB_DEL(PIMA__ADD,1)
           RESPHRAT(1) = PIM%OBS(IND_OBS)%RES_PH_RAT(PIMA__ADD,1)
           RESPHAS(1)  = PIM%OBS(IND_OBS)%RES_PHS(PIMA__ADD,1)
           IF ( NUMB_BND == 2 ) THEN
                TOTPHASE(2) = PIM%OBS(IND_OBS_2ND)%TOT_PHS(PIMA__ADD,2)
                PHDELERR(2) = PIM%OBS(IND_OBS_2ND)%PH_DEL_ERR(PIMA__ADD,2)
                TOTPHSGC(2) = PIM%OBS(IND_OBS_2ND)%TOT_PHS_GC(PIMA__ADD,2)
                TOTMBDEL(2) = PIM%OBS(IND_OBS_2ND)%TOT_MB_DEL(PIMA__ADD,2)
                GRDELERR(2) = PIM%OBS(IND_OBS_2ND)%MB_DEL_ERR(PIMA__ADD,2)
                DEL_RATE(2) = PIM%OBS(IND_OBS_2ND)%TOT_PH_RAT(PIMA__ADD,2)
                PHRATERR(2) = PIM%OBS(IND_OBS_2ND)%PH_RAT_ERR(PIMA__ADD,2)
                RESMBDEL(2) = PIM%OBS(IND_OBS_2ND)%RES_MB_DEL(PIMA__ADD,2)
                RESPHRAT(2) = PIM%OBS(IND_OBS_2ND)%RES_PH_RAT(PIMA__ADD,2)
                RESPHAS(2)  = PIM%OBS(IND_OBS)%RES_PHS(PIMA__ADD,2)
           END IF
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PLCODE ( GVH, 'TOTPHASE', IND_DB_OBS, 0, TOTPHASE, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 7912, IUER, 'PIMA_GVH_OBS', 'Error in putting '// &
     &         '"TOTPHASE" lcode' )
           RETURN
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PLCODE ( GVH, 'PHDELERR', IND_DB_OBS, 0, PHDELERR, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 7913, IUER, 'PIMA_GVH_OBS', 'Error in putting '// &
     &         '"PHDELERR" lcode' )
           RETURN
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PLCODE ( GVH, 'GC_PHASE', IND_DB_OBS, 0, TOTPHSGC, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 7914, IUER, 'PIMA_GVH_OBS', 'Error in putting '// &
     &         '"GC_PHASE" lcode' )
           RETURN
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PLCODE ( GVH, 'SNRATIO ', IND_DB_OBS, 0, SNR, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 7915, IUER, 'PIMA_GVH_OBS', 'Error in putting '// &
     &         '"SNRATIO " lcode' )
           RETURN
      END IF
!
      ARR_R8(1) = PIM%OBS(IND_OBS)%REF_FREQ(1)
      IF ( NUMB_BND == 2 ) THEN
           ARR_R8(2) = PIM%OBS(IND_OBS_2ND)%REF_FREQ(2)
      END IF
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PLCODE ( GVH, 'REF_FREQ', IND_DB_OBS, 0, ARR_R8, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 7916, IUER, 'PIMA_GVH_OBS', 'Error in putting '// &
     &         '"REF_FREQ" lcode' )
           RETURN
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PLCODE ( GVH, 'GR_DELAY', IND_DB_OBS, 0, TOTMBDEL, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 7917, IUER, 'PIMA_GVH_OBS', 'Error in putting '// &
     &         '"GR_DELAY" lcode' )
           RETURN
      END IF
!
      ARR_R8(1) = PIM%OBS(IND_OBS)%TOT_GR_RAT(1)
      IF ( NUMB_BND == 2 ) THEN
           ARR_R8(2) = PIM%OBS(IND_OBS_2ND)%TOT_GR_RAT(2)
      END IF
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PLCODE ( GVH, 'GR_RATE ', IND_DB_OBS, 0, ARR_R8, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 7918, IUER, 'PIMA_GVH_OBS', 'Error in putting '// &
     &         '"GR_RATE " lcode' )
           RETURN
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PLCODE ( GVH, 'GRDELERR', IND_DB_OBS, 0, GRDELERR, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 7919, IUER, 'PIMA_GVH_OBS', 'Error in putting '// &
     &         '"GRDELERR" lcode' )
           RETURN
      END IF
!
      ARR_R8(1) = PIM%OBS(IND_OBS)%GR_RAT_ERR(1)
      IF ( NUMB_BND == 2 ) THEN
           ARR_R8(2) = PIM%OBS(IND_OBS_2ND)%GR_RAT_ERR(2)
      END IF
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PLCODE ( GVH, 'GRRATERR', IND_DB_OBS, 0, ARR_R8, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 7920, IUER, 'PIMA_GVH_OBS', 'Error in putting '// &
     &         '"GRRATERR" lcode' )
           RETURN
      END IF
!
      ARR_R8(1) = PIM%OBS(IND_OBS)%GRAMBSP(1)
      IF ( NUMB_BND == 2 ) THEN
           ARR_R8(2) = PIM%OBS(IND_OBS_2ND)%GRAMBSP(2)
      END IF
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PLCODE ( GVH, 'GDAMBSP ', IND_DB_OBS, 0, ARR_R8, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 7921, IUER, 'PIMA_GVH_OBS', 'Error in putting '// &
     &         '"GDAMBSP " lcode' )
           RETURN
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PLCODE ( GVH, 'DEL_RATE', IND_DB_OBS, 0, DEL_RATE, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 7922, IUER, 'PIMA_GVH_OBS', 'Error in putting '// &
     &         '"DEL_RATE" lcode' )
           RETURN
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PLCODE ( GVH, 'PHRATERR', IND_DB_OBS, 0, PHRATERR, IER ) 
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 7923, IUER, 'PIMA_GVH_OBS', 'Error in putting '// &
     &         '"PHRATERR" lcode' )
           RETURN
      END IF
!
      ARR_R8(1) = PIM%OBS(IND_OBS)%TOT_SB_DEL(1)
      IF ( NUMB_BND == 2 ) THEN
           ARR_R8(2) = PIM%OBS(IND_OBS_2ND)%TOT_SB_DEL(2)
      END IF
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PLCODE ( GVH, 'SB_DELAY', IND_DB_OBS, 0, ARR_R8, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 7924, IUER, 'PIMA_GVH_OBS', 'Error in putting '// &
     &         '"SB_DELAY" lcode' )
           RETURN
      END IF
!
      ARR_R8(1) = PIM%OBS(IND_OBS)%SB_DEL_ERR(1)
      IF ( NUMB_BND == 2 ) THEN
           ARR_R8(2) = PIM%OBS(IND_OBS_2ND)%SB_DEL_ERR(2)
      END IF
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PLCODE ( GVH, 'SBDELERR', IND_DB_OBS, 0, ARR_R8, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 7925, IUER, 'PIMA_GVH_OBS', 'Error in putting '// &
     &         '"SBDELERR" lcode' )
           RETURN
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PLCODE ( GVH, 'SCAN_DUR', IND_DB_OBS, 0, &
     &                  PIM%OBS(IND_OBS)%SCAN_DURA, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 7926, IUER, 'PIMA_GVH_OBS', 'Error in putting '// &
     &         '"SCAN_DURA" lcode' )
           RETURN
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PLCODE ( GVH, 'PIND_OBS', IND_DB_OBS, 0, IND_OBS, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 7927, IUER, 'PIMA_GVH_OBS', 'Error in putting '// &
     &         '"PIND_OBS" lcode' )
           RETURN
      END IF
!
      IF ( PIM%OBS(IND_OBS)%STA_IND(1) < PIM%OBS(IND_OBS)%STA_IND(2) ) THEN
           UV_STA_ORDER = 1
         ELSE
           UV_STA_ORDER = -1
      END IF
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PLCODE ( GVH, 'UVSTAORD', IND_DB_OBS, 0, UV_STA_ORDER, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 7928, IUER, 'PIMA_GVH_OBS', 'Error in putting '// &
     &         '"UV_STA_ORDER" lcode' )
           RETURN
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PLCODE ( GVH, 'DELW_CEN', IND_DB_OBS, 0, &
     &     PIM%CONF%FRIB_DELAY_WINDOW_CENTER, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 7929, IUER, 'PIMA_GVH_OBS', 'Error in putting '// &
     &         '"DELW_CEN" lcode' )
           RETURN
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PLCODE ( GVH, 'DELW_WDT', IND_DB_OBS, 0, &
     &     PIM%CONF%FRIB_DELAY_WINDOW_WIDTH, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 7930, IUER, 'PIMA_GVH_OBS', 'Error in putting '// &
     &         '"DELW_WDT" lcode' )
           RETURN
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PLCODE ( GVH, 'RATE_CEN', IND_DB_OBS, 0, &
     &     PIM%CONF%FRIB_RATE_WINDOW_CENTER, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 7931, IUER, 'PIMA_GVH_OBS', 'Error in putting '// &
     &         '"RATE_CEN" lcode' )
           RETURN
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PLCODE ( GVH, 'RATE_WDT', IND_DB_OBS, 0, &
     &     PIM%CONF%FRIB_RATE_WINDOW_WIDTH, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 7932, IUER, 'PIMA_GVH_OBS', 'Error in putting '// &
     &         '"RATE_WDT" lcode' )
           RETURN
      END IF
!
      ARR_R8(1) = PIM%OBS(IND_OBS)%EFF_FRQ(1,1)
      ARR_R8(2) = PIM%OBS(IND_OBS)%EFF_FRQ(2,1)
      ARR_R8(3) = PIM%OBS(IND_OBS)%EFF_FRQ(3,1)
      IF ( NUMB_BND == 2 ) THEN
           ARR_R8(4) = PIM%OBS(IND_OBS_2ND)%EFF_FRQ(1,2)
           ARR_R8(5) = PIM%OBS(IND_OBS_2ND)%EFF_FRQ(2,2)
           ARR_R8(6) = PIM%OBS(IND_OBS_2ND)%EFF_FRQ(3,2)
      END IF
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PLCODE ( GVH, 'EFF_FREQ', IND_DB_OBS, 0, ARR_R8, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 7933, IUER, 'PIMA_GVH_OBS', 'Error in putting '// &
     &         '"EFF_FREQ" lcode' )
           RETURN
      END IF
!
! --- Check whether observations at both bands existed and they gave
! --- fringes
!
      IF ( BTEST ( PIM%OBS(IND_OBS)%FRI_STS(1), FRI__PIM ) .AND. &
     &     BTEST ( PIM%OBS(IND_OBS)%FRI_STS(2), FRI__PIM )       ) THEN
!
! -------- Check whether observations at both bands were good enough
! -------- to have effective frequencies
!
           IF ( PIM%OBS(IND_OBS)%EFF_FRQ(1,1)     > 0.0D0 .AND. &
     &          PIM%OBS(IND_OBS_2ND)%EFF_FRQ(1,2) > 0.0D0 .AND. &
     &          DABS(PIM%OBS(IND_OBS)%EFF_FRQ(1,1) - PIM%OBS(IND_OBS_2ND)%EFF_FRQ(1,2)) > 1.D0 ) THEN
                
                ION_GF(1) = PIM%OBS(IND_OBS)%EFF_FRQ(1,1)**2/ &
                           (PIM%OBS(IND_OBS)%EFF_FRQ(1,1)**2 - PIM%OBS(IND_OBS_2ND)%EFF_FRQ(1,2)**2)
                ION_GF(2) = PIM%OBS(IND_OBS_2ND)%EFF_FRQ(1,2)**2/ &
     &                     (PIM%OBS(IND_OBS)%EFF_FRQ(1,1)**2 - PIM%OBS(IND_OBS_2ND)%EFF_FRQ(1,2)**2)
                ION_RF(1) = PIM%OBS(IND_OBS)%EFF_FRQ(3,1)**2/ &
                           (PIM%OBS(IND_OBS)%EFF_FRQ(3,1)**2 - PIM%OBS(IND_OBS_2ND)%EFF_FRQ(3,2)**2)
                ION_RF(2) = PIM%OBS(IND_OBS_2ND)%EFF_FRQ(3,2)**2/ &
     &                     (PIM%OBS(IND_OBS)%EFF_FRQ(3,1)**2 - PIM%OBS(IND_OBS_2ND)%EFF_FRQ(3,2)**2)
!
                ION_GDEL = - ION_GF(2)*(   PIM%OBS(IND_OBS)%TOT_MB_DEL(1,1) &
     &                                   - PIM%OBS(IND_OBS_2ND)%TOT_MB_DEL(1,2) )
                ION_PRAT = - ION_RF(2)*(   PIM%OBS(IND_OBS)%TOT_PH_RAT(1,1) &
     &                                   - PIM%OBS(IND_OBS_2ND)%TOT_PH_RAT(1,2) )
                IF ( PIM%CONF%MKDB_FRINGE_ALGORITHM == PIMA__FRA_DRF ) THEN
                     ION_GERR = ION_GF(2) * DSQRT( PIM%OBS(IND_OBS)%MB_DEL_ERR(PIMA__DRF,1)**2 + &
     &                                             PIM%OBS(IND_OBS_2ND)%MB_DEL_ERR(PIMA__DRF,2)**2   )
                  ELSE IF ( PIM%CONF%MKDB_FRINGE_ALGORITHM == PIMA__FRA_LSQ ) THEN
                     ION_GERR = ION_GF(2) * DSQRT( PIM%OBS(IND_OBS)%MB_DEL_ERR(PIMA__LSQ,1)**2 + &
     &                                             PIM%OBS(IND_OBS_2ND)%MB_DEL_ERR(PIMA__LSQ,2)**2   )
                  ELSE IF ( PIM%CONF%MKDB_FRINGE_ALGORITHM == PIMA__FRA_MUL ) THEN
                     ION_GERR = ION_GF(2) * DSQRT( PIM%OBS(IND_OBS)%MB_DEL_ERR(PIMA__MUL,1)**2 + &
     &                                             PIM%OBS(IND_OBS_2ND)%MB_DEL_ERR(PIMA__MUL,2)**2   )
                  ELSE IF ( PIM%CONF%MKDB_FRINGE_ALGORITHM == PIMA__FRA_ADD  ) THEN
                     ION_GERR = ION_GF(2) * DSQRT( PIM%OBS(IND_OBS)%MB_DEL_ERR(PIMA__ADD,1)**2 + &
     &                                             PIM%OBS(IND_OBS_2ND)%MB_DEL_ERR(PIMA__ADD,2)**2   )
                END IF
                ION_RERR = ION_RF(2) * DSQRT( PIM%OBS(IND_OBS)%PH_RAT_ERR(1,1)**2 + &
     &                                        PIM%OBS(IND_OBS_2ND)%PH_RAT_ERR(1,2)**2   )
              ELSE
                ION_GDEL = 0.0D0
                ION_GERR = 0.0D0
                ION_PRAT = 0.0D0
                ION_RERR = 0.0D0
           END IF
         ELSE
           ION_GDEL = 0.0D0
           ION_GERR = 0.0D0
           ION_PRAT = 0.0D0
           ION_RERR = 0.0D0
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PLCODE ( GVH, 'ION_GDEL', IND_DB_OBS, 0, ION_GDEL, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 7934, IUER, 'PIMA_GVH_OBS', 'Error in putting '// &
     &         '"ION_GDEL" lcode' )
           RETURN
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PLCODE ( GVH, 'ION_GERR', IND_DB_OBS, 0, ION_GERR, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 7935, IUER, 'PIMA_GVH_OBS', 'Error in putting '// &
     &         '"ION_GERR" lcode' )
           RETURN
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PLCODE ( GVH, 'ION_PRAT', IND_DB_OBS, 0, ION_PRAT, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 7936, IUER, 'PIMA_GVH_OBS', 'Error in putting '// &
     &         '"ION_PRAT" lcode' )
           RETURN
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PLCODE ( GVH, 'ION_RERR', IND_DB_OBS, 0, ION_RERR, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 7937, IUER, 'PIMA_GVH_OBS', 'Error in putting '// &
     &         '"ION_RERR" lcode' )
           RETURN
      END IF
!
! --- Coarse resolving ambiguities in phase delays
!
      DO 420 J2=1,2
         N_GRAMB(J2) = 0 ! Set zero group delay ambiguities
         IF ( BTEST ( PIM%OBS(INDO(J2))%FRI_STS(J2), FRI__PIM ) .AND. &
     &        DABS(PIM%OBS(INDO(J2))%TOT_PHS(PIMA__DRF,J2)) < 4*PI__NUM ) THEN
!
! ----------- Resolve phase delay ambiguity, by adjusting phase to the
! ----------- multiband delay
!
              IF ( PIM%CONF%MKDB_FRINGE_ALGORITHM == PIMA__FRA_DRF ) THEN
                   N_PHAMB(J2) = - IDNINT ( ( PIM%OBS(INDO(J2))%TOT_PHS(PIMA__DRF,J2) - &
     &                                 ( PIM%OBS(INDO(J2))%TOT_MB_DEL(PIMA__DRF,J2) + &
     &                                   N_GRAMB(J2)*PIM%OBS(INDO(J2))%GRAMBSP(J2) &
     &                                   )*(PI2*PIM%OBS(INDO(J2))%REF_FREQ(J2)) &
     &                                 )/PI2 &
     &                               )
                ELSE IF ( PIM%CONF%MKDB_FRINGE_ALGORITHM == PIMA__FRA_LSQ ) THEN
                   N_PHAMB(J2) = - IDNINT ( ( PIM%OBS(INDO(J2))%TOT_PHS(PIMA__LSQ,J2) - &
     &                                 ( PIM%OBS(INDO(J2))%TOT_MB_DEL(PIMA__LSQ,J2) + &
     &                                   N_GRAMB(J2)*PIM%OBS(INDO(J2))%GRAMBSP(J2) &
     &                                   )*(PI2*PIM%OBS(INDO(J2))%REF_FREQ(J2)) &
     &                                 )/PI2 &
     &                               )
                ELSE IF ( PIM%CONF%MKDB_FRINGE_ALGORITHM == PIMA__FRA_MUL ) THEN
                   N_PHAMB(J2) = - IDNINT ( ( PIM%OBS(INDO(J2))%TOT_PHS(PIMA__MUL,J2) - &
     &                                 ( PIM%OBS(INDO(J2))%TOT_MB_DEL(PIMA__MUL,J2) + &
     &                                   N_GRAMB(J2)*PIM%OBS(INDO(J2))%GRAMBSP(J2) &
     &                                   )*(PI2*PIM%OBS(INDO(J2))%REF_FREQ(J2)) &
     &                                 )/PI2 &
     &                               )
                ELSE IF ( PIM%CONF%MKDB_FRINGE_ALGORITHM == PIMA__FRA_ADD ) THEN
                   N_PHAMB(J2) = - IDNINT ( ( PIM%OBS(INDO(J2))%TOT_PHS(PIMA__ADD,J2) - &
     &                                 ( PIM%OBS(INDO(J2))%TOT_MB_DEL(PIMA__ADD,J2) + &
     &                                   N_GRAMB(J2)*PIM%OBS(INDO(J2))%GRAMBSP(J2) &
     &                                   )*(PI2*PIM%OBS(INDO(J2))%REF_FREQ(J2)) &
     &                                 )/PI2 &
     &                               )
              END IF
           ELSE
             N_PHAMB(J2) = 0
         END IF
!
! ------ However, let us not put the group delay ambiguity into the database
!
         N_GRAMB(J2) = 0
 420  CONTINUE
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PLCODE ( GVH, 'N_GRAMB ', IND_DB_OBS, 0, N_GRAMB, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 7938, IUER, 'PIMA_GVH_OBS', 'Error in putting '// &
     &         '"N_GRAMB" lcode' )
           RETURN
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PLCODE ( GVH, 'N_PHAMB ', IND_DB_OBS, 0, N_PHAMB, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 7939, IUER, 'PIMA_GVH_OBS', 'Error in putting '// &
     &         '"N_PHAMB" 2nd station lcode' )
           RETURN
      END IF
!
! --- Put apriori baseline quantities referred to the 1st station
!
      ARR_R8(1) = PIM%OBS(IND_OBS)%APR_GR_DEL(1)
      IF ( NUMB_BND == 2 ) THEN
           ARR_R8(2) = PIM%OBS(IND_OBS_2ND)%APR_GR_DEL(2)
      END IF
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PLCODE ( GVH, 'APR_DEL ', IND_DB_OBS, 0, ARR_R8, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 7940, IUER, 'PIMA_GVH_OBS', 'Error in putting '// &
     &         '"APR_DEL " lcode' )
           RETURN
      END IF
!
      ARR_R8(1) = PIM%OBS(IND_OBS)%APR_RAT(1)
      IF ( NUMB_BND == 2 ) THEN
           ARR_R8(2) = PIM%OBS(IND_OBS_2ND)%APR_RAT(2)
      END IF
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PLCODE ( GVH, 'APR_RATE', IND_DB_OBS, 0, ARR_R8, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 7941, IUER, 'PIMA_GVH_OBS', 'Error in putting '// &
     &         '"APR_RATE" lcode' )
           RETURN
      END IF
!
      ARR_R8(1) = PIM%OBS(IND_OBS)%APR_PHS(1)
      IF ( NUMB_BND == 2 ) THEN
           ARR_R8(2) = PIM%OBS(IND_OBS_2ND)%APR_PHS(2)
      END IF
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PLCODE ( GVH, 'APR_PHAS', IND_DB_OBS, 0, ARR_R8, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 7942, IUER, 'PIMA_GVH_OBS', 'Error in putting '// &
     &         '"APR_PHAS" lcode' )
           RETURN
      END IF
!
      ARR_R8(1) = PIM%OBS(IND_OBS)%APR_GC_PHS(1,1)
      ARR_R8(2) = PIM%OBS(IND_OBS)%APR_GC_PHS(2,1)
      IF ( NUMB_BND == 2 ) THEN
           ARR_R8(3) = PIM%OBS(IND_OBS_2ND)%APR_GC_PHS(1,2)
           ARR_R8(4) = PIM%OBS(IND_OBS_2ND)%APR_GC_PHS(2,2)
      END IF
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PLCODE ( GVH, 'APR_PHGC', IND_DB_OBS, 0, ARR_R8, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 7943, IUER, 'PIMA_GVH_OBS', 'Error in putting '// &
     &         '"APR_PHGC" lcode' )
           RETURN
      END IF
!
! --- Put residual baseline quantities referred to the 1st station
!
      CALL GVH_PLCODE ( GVH, 'RES_GRDL', IND_DB_OBS, 0, RESMBDEL, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 7944, IUER, 'PIMA_GVH_OBS', 'Error in putting '// &
     &         '"RES_GRDL" lcode' )
           RETURN
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PLCODE ( GVH, 'RES_RATE', IND_DB_OBS, 0, RESPHRAT, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 7945, IUER, 'PIMA_GVH_OBS', 'Error in putting '// &
     &         '"RES_RATE" lcode' )
           RETURN
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PLCODE ( GVH, 'RES_PHGC', IND_DB_OBS, 0, RESPHAS, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 7946, IUER, 'PIMA_GVH_OBS', 'Error in putting '// &
     &         '"RES_PHGC" lcode' )
           RETURN
      END IF
!
      ARR_R4(1) = PIM%OBS(IND_OBS)%NOISE(1)
      IF ( NUMB_BND == 2 ) THEN
           ARR_R4(2) = PIM%OBS(IND_OBS_2ND)%NOISE(2)
      END IF
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PLCODE ( GVH, 'NOISERMS', IND_DB_OBS, 0, ARR_R4, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 7947, IUER, 'PIMA_GVH_OBS', 'Error in putting '// &
     &         '"NOISERMS" lcode' )
           RETURN
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PLCODE ( GVH, 'APRCLOOF', IND_DB_OBS, 1, &
     &                  PIM%OBS(IND_OBS)%CLO_OFFSET_APR(1,1), IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 7948, IUER, 'PIMA_GVH_OBS', 'Error in putting '// &
     &         '"APRCLOOF" lcode for the first station' )
           RETURN
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PLCODE ( GVH, 'APRCLOOF', IND_DB_OBS, 2, &
     &                  PIM%OBS(IND_OBS)%CLO_OFFSET_APR(2,1), IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 7949, IUER, 'PIMA_GVH_OBS', 'Error in putting '// &
     &         '"APRCLOOF" lcode for the second station' )
           RETURN
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PLCODE ( GVH, 'APRCLORT', IND_DB_OBS, 1, &
     &                  PIM%OBS(IND_OBS)%CLO_RATE_APR(1,1), IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 7950, IUER, 'PIMA_GVH_OBS', 'Error in putting '// &
     &         '"APRCLOOF" lcode for the first station' )
           RETURN
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PLCODE ( GVH, 'APRCLORT', IND_DB_OBS, 2, &
     &                  PIM%OBS(IND_OBS)%CLO_RATE_APR(2,1), IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 7951, IUER, 'PIMA_GVH_OBS', 'Error in putting '// &
     &         '"APRCLORT" lcode for the second station' )
           RETURN
      END IF
!
! --- Set poalrization.
! --- As of 2010.01.03 we do not support a case when both polarizations are used
! --- for fringe fitting
!
      POLARIZ_CH(1) = PIM%CONF%POLAR
      POLARIZ_CH(2) = PIM%CONF%POLAR
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PLCODE ( GVH, 'POLARIZ ', IND_DB_OBS, 0, POLARIZ_CH, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 7952, IUER, 'PIMA_GVH_OBS', 'Error in putting '// &
     &         '"POLARIZ " lcode' )
           RETURN
      END IF
!
! --- Get meteorological parameters
!
      IF ( .NOT. FL_BAS_REVERSE ) THEN
!
! -------- NB: the order of stations can be reverse
!
           IF ( PIM%STA(PIM%OBS(IND_OBS)%STA_IND(1))%WEATHER%AVAIL ) THEN
!
! ------------- Meteorological data are available for the 1st station
!
                ATM_PRES(1) = PIM%OBS(IND_OBS)%PRES(1)
                AIR_TEMP(1) = PIM%OBS(IND_OBS)%TEMP(1)
                HUMID(1)    = PIM%OBS(IND_OBS)%HUMID(1)
              ELSE
                ATM_PRES(1) = ATM_PRES_DEF
                AIR_TEMP(1) = AIR_TEMP_DEF
                HUMID(1)    = REL_HUMID_DEF
           END IF
!
           IF ( PIM%STA(PIM%OBS(IND_OBS)%STA_IND(2))%WEATHER%AVAIL ) THEN
!
! ------------- Meteorological data ara available for the 2nd station
!
                ATM_PRES(2) = PIM%OBS(IND_OBS)%PRES(2)
                AIR_TEMP(2) = PIM%OBS(IND_OBS)%TEMP(2)
                HUMID(2)    = PIM%OBS(IND_OBS)%HUMID(2)
              ELSE
                ATM_PRES(2) = ATM_PRES_DEF
                AIR_TEMP(2) = AIR_TEMP_DEF
                HUMID(2)    = REL_HUMID_DEF
           END IF
         ELSE
!
! -------- Order of stations in this observation is reversed
!
           IF ( PIM%STA(PIM%OBS(IND_OBS)%STA_IND(1))%WEATHER%AVAIL ) THEN
!
! ------------- Meteorological data ara available for the 1st station (ORIGINAL)
!
                ATM_PRES(1) = PIM%OBS(IND_OBS)%PRES(2)
                AIR_TEMP(1) = PIM%OBS(IND_OBS)%TEMP(2)
                HUMID(1)    = PIM%OBS(IND_OBS)%HUMID(2)
              ELSE
                ATM_PRES(1) = ATM_PRES_DEF
                AIR_TEMP(1) = AIR_TEMP_DEF
                HUMID(1)    = REL_HUMID_DEF
           END IF
           IF ( PIM%STA(PIM%OBS(IND_OBS)%STA_IND(2))%WEATHER%AVAIL ) THEN
!
! ------------- Meteorological data ara available for the 2nd station (ORIGINAL)
!
                ATM_PRES(2) = PIM%OBS(IND_OBS)%PRES(1)
                AIR_TEMP(2) = PIM%OBS(IND_OBS)%TEMP(1)
                HUMID(2)    = PIM%OBS(IND_OBS)%HUMID(1)
              ELSE
                ATM_PRES(2) = ATM_PRES_DEF
                AIR_TEMP(2) = AIR_TEMP_DEF
                HUMID(2)    = REL_HUMID_DEF
           END IF
      END IF
!
! --- Check, whether meteorological parameters are in range
!
      DO 430 J3=1,2
         IF ( ATM_PRES(J3) < ATM_PRES_MIN  .OR. &
     &        ATM_PRES(J3) > ATM_PRES_MAX       ) THEN
              ATM_PRES(J3) = ATM_PRES_DEF
         END IF
!
         IF ( AIR_TEMP(J3) < AIR_TEMP_MIN  .OR. &
     &        AIR_TEMP(J3) > AIR_TEMP_MAX       ) THEN
              AIR_TEMP(J3) = AIR_TEMP_DEF
         END IF
!
         IF ( HUMID(J3) < REL_HUMID_MIN  .OR. &
     &        HUMID(J3) > REL_HUMID_MAX       ) THEN
              HUMID(J3) = REL_HUMID_DEF
         END IF
 430  CONTINUE
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PLCODE ( GVH, 'AIR_TEMP', IND_DB_OBS, 1, AIR_TEMP(1), IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 7953, IUER, 'PIMA_GVH_OBS', 'Error in putting '// &
     &         '"AIR_TEMP" 1st station lcode' )
           RETURN
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PLCODE ( GVH, 'AIR_TEMP', IND_DB_OBS, 2, AIR_TEMP(2), IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 7954, IUER, 'PIMA_GVH_OBS', 'Error in putting '// &
     &         '"AIR_TEMP" 2nd station lcode' )
           RETURN
      END IF
!
! --- NB: the order of stations can be reverse
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PLCODE ( GVH, 'ATM_PRES', IND_DB_OBS, 1, ATM_PRES(1), IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 7955, IUER, 'PIMA_GVH_OBS', 'Error in putting '// &
     &         '"ATM_PRES" 1st station lcode' )
           RETURN
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PLCODE ( GVH, 'ATM_PRES', IND_DB_OBS, 2, ATM_PRES(2), IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 7956, IUER, 'PIMA_GVH_OBS', 'Error in putting '// &
     &         '"ATM_PRES" 2nd station lcode' )
           RETURN
      END IF
!
! --- NB: the order of stations can be reverse!
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PLCODE ( GVH, 'REL_HUMD', IND_DB_OBS, 1, HUMID(1), IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 7957, IUER, 'PIMA_GVH_OBS', 'Error in putting '// &
     &         '"REL_HUMD" 1st station lcode' )
           RETURN
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PLCODE ( GVH, 'REL_HUMD', IND_DB_OBS, 2, HUMID(2), IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 7958, IUER, 'PIMA_GVH_OBS', 'Error in putting '// &
     &         '"REL_HUMD" 2nd station lcode' )
           RETURN
      END IF
!
      IF ( .NOT. FL_BAS_REVERSE ) THEN
            CABLE(1) = PIM%OBS(IND_OBS)%CABLE(1)
            CABLE(2) = PIM%OBS(IND_OBS)%CABLE(2)
         ELSE
            CABLE(1) = PIM%OBS(IND_OBS)%CABLE(2)
            CABLE(2) = PIM%OBS(IND_OBS)%CABLE(1)
      END IF
!
      DO 440 J4=1,2
         IF ( IS_R8_NAN(CABLE(J4)) ) THEN
              CABLE(J4) = CABLE_DEL_DEF
         END IF
         IF ( CABLE(J4) < CABLE_DEL_MIN  .OR. &
     &        CABLE(J4) > CABLE_DEL_MAX       ) THEN
              CABLE(J4) = CABLE_DEL_DEF
         END IF
 440  CONTINUE
!
! --- Get antenna gain
!
      IND_FRG = PIM%CONF%FRQ_GRP
      GAIN = 0.0D0
      DO 450 J5=1,2
         IF ( PIM%STA(PIM%OBS(IND_OBS)%STA_IND(J5))%GAIN(IND_FRG)%AVAIL ) THEN
              IF ( PIM%STA(PIM%OBS(IND_OBS)%STA_IND(J5))%GAIN(IND_FRG)%TYP(1,1) == 2 .AND. &
     &             PIM%STA(PIM%OBS(IND_OBS)%STA_IND(J5))%GAIN(IND_FRG)%NTAB > 0           ) THEN
!
! ---------------- Compute the value of the polynomial that models
! ---------------- elevation depedence of the gain
!
! ---------------- The caveat: indexing of array GAIN(IND_FRG)%Y_VAL starts from 1
!
                   POLY_VAL = 0.0D0
                   DO 460 J6=1,PIM%STA(PIM%OBS(IND_OBS)%STA_IND(J5))%GAIN(IND_FRG)%NTAB 
                      IF ( IS_R4_NAN ( PIM%STA(PIM%OBS(IND_OBS)%STA_IND(J5))%GAIN(IND_FRG)%GAIN(J6,PIM%CONF%BEG_FRQ,1) ) ) THEN
                           GAIN_POLY = PIM%STA(PIM%OBS(IND_OBS)%STA_IND(J5))%GAIN(IND_FRG)%Y_VAL(J6,PIM%CONF%BEG_FRQ,1) 
                         ELSE 
                           GAIN_POLY = PIM%STA(PIM%OBS(IND_OBS)%STA_IND(J5))%GAIN(IND_FRG)%GAIN(J6,PIM%CONF%BEG_FRQ,1) 
                      END IF
                      POLY_VAL(1) = POLY_VAL(1) + GAIN_POLY* &
     &                             (PIM%OBS(IND_OBS)%ELEV(1)/DEG__TO__RAD)**(J6-1)
!
                      IF ( NUMB_BND == 2 ) THEN
                           IF ( IS_R4_NAN ( PIM%STA(PIM%OBS(IND_OBS)%STA_IND(J5))%GAIN(IND_FRG)%GAIN(J6,PIM%CONF%BEG_FRQ,1) ) ) THEN
                                GAIN_POLY = PIM%STA(PIM%OBS(IND_OBS)%STA_IND(J5))%GAIN(IND_FRG)%Y_VAL(J6,PIM_2ND%CONF%BEG_FRQ,1) 
                              ELSE 
                                GAIN_POLY = PIM%STA(PIM%OBS(IND_OBS)%STA_IND(J5))%GAIN(IND_FRG)%GAIN(J6,PIM_2ND%CONF%BEG_FRQ,1) 
                           END IF
!
                           POLY_VAL(2) = POLY_VAL(2) + GAIN_POLY* &
     &                                   (PIM%OBS(IND_OBS)%ELEV(1)/DEG__TO__RAD)**(J6-1)
                      END IF
 460               CONTINUE
!
! ---------------- In the future we may find a more elegant way to deal with 
! ---------------- this issue
!
                   IF ( FL_BAS_REVERSE ) THEN
                        IF ( J5 == 1 ) IND = 2
                        IF ( J5 == 2 ) IND = 1
                      ELSE 
                        IND = J5
                   END IF
                   GAIN(1,IND) = PIM%STA(PIM%OBS(IND_OBS)%STA_IND(J5))%GAIN(IND_FRG)%SENS(PIM%CONF%BEG_FRQ,1)*POLY_VAL(1)
                   IF ( NUMB_BND == 2 ) THEN
                        GAIN(2,IND) = PIM%STA(PIM%OBS(IND_OBS)%STA_IND(J5))%GAIN(IND_FRG)%SENS(PIM_2ND%CONF%BEG_FRQ,1)*POLY_VAL(1)
                   END IF
              END IF
         END IF
 450  CONTINUE
!
      AGAIN = GAIN
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PLCODE ( GVH, 'ANT_GAIN', IND_DB_OBS, 1, AGAIN(1,1), IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 7959, IUER, 'PIMA_GVH_OBS', 'Error in putting '// &
     &         '"ANT_GAIN" lcode 1st station' )
           RETURN
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PLCODE ( GVH, 'ANT_GAIN', IND_DB_OBS, 2, AGAIN(1,2), IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 7960, IUER, 'PIMA_GVH_OBS', 'Error in putting '// &
     &         '"ANT_GAIN" lcode 2nd station' )
           RETURN
      END IF
!
      NUSEDCHN = 0
      TSYS1    = 0.0
      IND_CHN1 = 0
      NUM_SAM1 = 0.0D0
      NUM_AP1  = 0
      UV_CHN1  = CMPLX ( 0.0, 0.0 )
      IND_CHN2 = 0
      NUM_SAM2 = 0.0D0
      NUM_AP2  = 0
      UV_CHN2  = CMPLX ( 0.0, 0.0 )
      TSYS2    = 0.0
!
      KFRQ = 0
      DO 470 J7=PIM%CONF%BEG_FRQ,PIM%CONF%END_FRQ
         KFRQ = KFRQ + 1
!
! ------ We bypass channels with too low amplitude, because they are
! ------ apparently missing
!
         NUSEDCHN(1) = NUSEDCHN(1) + 1
         IND_CHN1(NUSEDCHN(1)) = J7
         NUM_SAM1(NUSEDCHN(1),1) = PIM%OBS(IND_OBS)%EFF_DUR(1)* &
     &                             PIM%FRQ(J7,PIM%CONF%FRQ_GRP)%BAND_WIDTH*2.0D0
         NUM_AP1(NUSEDCHN(1),1)  = PIM%OBS(IND_OBS)%EFF_DUR(1)/ &
     &                             PIM%OBS(IND_OBS)%AP_LEN

         UV_CHN1(NUSEDCHN(1))  = PIM%OBS(IND_OBS)%RES_FRN(J7,1)
         IF ( PIM%FRG_USE == PIMA__SINGLE ) THEN
              U_FRG = PIM%CONF%FRQ_GRP
              U_FRQ = J7
            ELSE IF ( PIM%FRG_USE == PIMA__MERGE ) THEN
              U_FRG = PIM%REV_FRG(J7)
              U_FRQ = PIM%REV_FRQ(J7)
            ELSE IF ( PIM%FRG_USE == PIMA__COMBINE ) THEN
              U_FRG = PIM%REV_FRG(J7)
              U_FRQ = PIM%REV_FRQ(J7)
         END IF
         IF ( NUSEDCHN(1) > 0 ) THEN
              DO 480 J8=1,2
                 IF ( PIM%STA(PIM%OBS(IND_OBS)%STA_IND(J8))%TSYS(IND_FRG)%AVAIL .AND. &
     &                PIM%OBS(IND_OBS)%TSYS_IND(J8,PIM%CONF%FRQ_GRP) > 0 ) THEN
!
                 END IF
                 IF ( PIM%STA(PIM%OBS(IND_OBS)%STA_IND(J8))%PCAL(IND_FRG)%PCAL_AVAIL .AND. &
     &                PIM%STA(PIM%OBS(IND_OBS)%STA_IND(J8))%PCAL(IND_FRG)%PCAL_USE         ) THEN
                      IF ( PIM%OBS(IND_OBS)%PCAL_IND(1,J8,U_FRG) > 0 ) THEN
                           PCAL_FR1(NUSEDCHN(1),J8) = PIM%STA(PIM%OBS(IND_OBS)%STA_IND(J8))%PCAL(IND_FRG)%FREQ(1,J7,PIM%OBS(IND_OBS)%PCAL_IND(1,J8,PIM%CONF%FRQ_GRP))
                           PCAL_AMP = PIM%STA(PIM%OBS(IND_OBS)%STA_IND(J8))%PCAL(U_FRG)%AMPL(1,U_FRQ,PIM%OBS(IND_OBS)%PCAL_IND(1,J8,PIM%CONF%FRQ_GRP),1)
                           PCAL_PHS = PIM%STA(PIM%OBS(IND_OBS)%STA_IND(J8))%PCAL(U_FRG)%PHAS(1,U_FRQ,PIM%OBS(IND_OBS)%PCAL_IND(1,J8,PIM%CONF%FRQ_GRP),1)
                         ELSE
                           PCAL_FR1(NUSEDCHN(1),J8) = 0.0
                           PCAL_AMP = 0.0
                           PCAL_PHS = 0.0
                      END IF
                      PCAL_CM1(NUSEDCHN(1),J8) = CMPLX ( PCAL_AMP*COS(PCAL_PHS), &
     &                                                   PCAL_AMP*SIN(PCAL_PHS)  )
                 END IF
 480          CONTINUE
         END IF
!
         IF ( PIM%STA(PIM%OBS(IND_OBS)%STA_IND(1))%TSYS(IND_FRG)%AVAIL .AND. &
     &        PIM%OBS(IND_OBS)%TSYS_IND(1,PIM%CONF%FRQ_GRP) > 0 ) THEN
!
              IF ( FL_BAS_REVERSE ) THEN
                   TSYS1(KFRQ,2) = PIM%STA(PIM%OBS(IND_OBS)%STA_IND(1))%TSYS(IND_FRG)%TSYS(J7,PIM%OBS(IND_OBS)%TSYS_IND(1,PIM%CONF%FRQ_GRP),1)
                 ELSE 
                   TSYS1(KFRQ,1) = PIM%STA(PIM%OBS(IND_OBS)%STA_IND(1))%TSYS(IND_FRG)%TSYS(J7,PIM%OBS(IND_OBS)%TSYS_IND(1,PIM%CONF%FRQ_GRP),1)
              END IF
         END IF
         IF ( PIM%STA(PIM%OBS(IND_OBS)%STA_IND(2))%TSYS(IND_FRG)%AVAIL .AND. &
     &        PIM%OBS(IND_OBS)%TSYS_IND(2,PIM%CONF%FRQ_GRP) > 0 ) THEN
              IF ( FL_BAS_REVERSE ) THEN
                   TSYS1(KFRQ,1) = PIM%STA(PIM%OBS(IND_OBS)%STA_IND(2))%TSYS(IND_FRG)%TSYS(J7,PIM%OBS(IND_OBS)%TSYS_IND(2,PIM%CONF%FRQ_GRP),1)
                 ELSE 
                   TSYS1(KFRQ,2) = PIM%STA(PIM%OBS(IND_OBS)%STA_IND(2))%TSYS(IND_FRG)%TSYS(J7,PIM%OBS(IND_OBS)%TSYS_IND(2,PIM%CONF%FRQ_GRP),1)
              END IF
         END IF
 470  CONTINUE
!
      IF ( NUMB_BND == 2 ) THEN
           KFRQ = 0
           DO 490 J9=PIM_2ND%CONF%BEG_FRQ,PIM_2ND%CONF%END_FRQ
              KFRQ = KFRQ + 1
!
! ----------- We bypass channels with too low amplitude, because they are
! ----------- apparently missing
!
              NUSEDCHN(2) = NUSEDCHN(2) + 1
              IND_CHN2(NUSEDCHN(2))   = J9
              NUM_SAM2(NUSEDCHN(2),1) = PIM%OBS(IND_OBS_2ND)%EFF_DUR(2)* &
     &                                  PIM%FRQ(J9,PIM%CONF%FRQ_GRP)%BAND_WIDTH*2.0D0
              NUM_AP2(NUSEDCHN(2),1)  = PIM%OBS(IND_OBS_2ND)%EFF_DUR(2)/ &
     &                                  PIM%OBS(IND_OBS_2ND)%AP_LEN
              UV_CHN2(NUSEDCHN(2))    = PIM%OBS(IND_OBS_2ND)%RES_FRN(J9,2)
              IF ( PIM%FRG_USE == PIMA__SINGLE ) THEN
                   U_FRG = PIM%CONF%FRQ_GRP
                   U_FRQ = J9
                 ELSE IF ( PIM%FRG_USE == PIMA__MERGE ) THEN
                   U_FRG = PIM%REV_FRG(J9)
                   U_FRQ = PIM%REV_FRQ(J9)
                 ELSE IF ( PIM%FRG_USE == PIMA__COMBINE ) THEN
                   U_FRG = PIM%REV_FRG(J9)
                   U_FRQ = PIM%REV_FRQ(J9)
              END IF
              IF ( NUSEDCHN(2) > 0 ) THEN
                   DO 4100 J10=1,2
                      IF ( PIM%STA(PIM%OBS(IND_OBS_2ND)%STA_IND(J10))%TSYS(IND_FRG)%AVAIL .AND. &
     &                     PIM%OBS(IND_OBS_2ND)%TSYS_IND(J10,PIM%CONF%FRQ_GRP) > 0 ) THEN
                           TSYS2(NUSEDCHN(2),J10) = PIM%STA(PIM%OBS(IND_OBS_2ND)%STA_IND(J10))%TSYS(IND_FRG)%TSYS(J9,PIM%OBS(IND_OBS_2ND)%TSYS_IND(J10,PIM%CONF%FRQ_GRP),1)
                      END IF
                      IF ( PIM%STA(PIM%OBS(IND_OBS_2ND)%STA_IND(J10))%PCAL(IND_FRG)%PCAL_AVAIL .AND. &
     &                     PIM%STA(PIM%OBS(IND_OBS_2ND)%STA_IND(J10))%PCAL(IND_FRG)%PCAL_USE         ) THEN
                           IF ( PIM%OBS(IND_OBS_2ND)%PCAL_IND(1,J10,U_FRG) > 0 ) THEN
                                PCAL_FR2(NUSEDCHN(2),J10) = PIM%STA(PIM%OBS(IND_OBS_2ND)%STA_IND(J10))%PCAL(IND_FRG)%FREQ(1,J9,PIM%OBS(IND_OBS_2ND)%PCAL_IND(1,J10,PIM%CONF%FRQ_GRP))
                                PCAL_AMP = PIM%STA(PIM%OBS(IND_OBS_2ND)%STA_IND(J10))%PCAL(U_FRG)%AMPL(1,U_FRQ,PIM%OBS(IND_OBS_2ND)%PCAL_IND(1,J10,PIM%CONF%FRQ_GRP),1)
                                PCAL_PHS = PIM%STA(PIM%OBS(IND_OBS_2ND)%STA_IND(J10))%PCAL(U_FRG)%PHAS(1,U_FRQ,PIM%OBS(IND_OBS_2ND)%PCAL_IND(1,J10,PIM%CONF%FRQ_GRP),1)
                              ELSE
                                PCAL_FR2(NUSEDCHN(2),J10) = 0.0
                                PCAL_AMP = 0.0
                                PCAL_PHS = 0.0
                           END IF
                           PCAL_CM2(NUSEDCHN(2),J10) = CMPLX ( PCAL_AMP*COS(PCAL_PHS), &
     &                                                         PCAL_AMP*SIN(PCAL_PHS)  )
                      END IF
 4100              CONTINUE
              END IF
!
              IF ( PIM%STA(PIM%OBS(IND_OBS_2ND)%STA_IND(1))%TSYS(IND_FRG)%AVAIL .AND. &
     &             PIM%OBS(IND_OBS_2ND)%TSYS_IND(1,PIM%CONF%FRQ_GRP) > 0 ) THEN
!
                   IF ( FL_BAS_REVERSE ) THEN
                        TSYS2(KFRQ,2) = PIM%STA(PIM%OBS(IND_OBS_2ND)%STA_IND(1))%TSYS(IND_FRG)%TSYS(J9,PIM%OBS(IND_OBS_2ND)%TSYS_IND(1,PIM%CONF%FRQ_GRP),1)
                      ELSE 
                        TSYS2(KFRQ,1) = PIM%STA(PIM%OBS(IND_OBS_2ND)%STA_IND(1))%TSYS(IND_FRG)%TSYS(J9,PIM%OBS(IND_OBS_2ND)%TSYS_IND(1,PIM%CONF%FRQ_GRP),1)
                   END IF
              END IF
              IF ( PIM%STA(PIM%OBS(IND_OBS_2ND)%STA_IND(2))%TSYS(IND_FRG)%AVAIL .AND. &
     &             PIM%OBS(IND_OBS_2ND)%TSYS_IND(2,PIM%CONF%FRQ_GRP) > 0 ) THEN
                   IF ( FL_BAS_REVERSE ) THEN
                        TSYS2(KFRQ,1) = PIM%STA(PIM%OBS(IND_OBS_2ND)%STA_IND(2))%TSYS(IND_FRG)%TSYS(J9,PIM%OBS(IND_OBS_2ND)%TSYS_IND(2,PIM%CONF%FRQ_GRP),1)
                      ELSE 
                        TSYS2(KFRQ,2) = PIM%STA(PIM%OBS(IND_OBS_2ND)%STA_IND(2))%TSYS(IND_FRG)%TSYS(J9,PIM%OBS(IND_OBS_2ND)%TSYS_IND(2,PIM%CONF%FRQ_GRP),1)
                   END IF
              END IF
 490      CONTINUE
      END IF
!
! --- Get polarization index
!
      IF ( PIM%NPOL == 1 ) THEN
           IND_POL = 1
         ELSE IF ( PIM%NPOL == 2 ) THEN
           IND_POL(1) = 1
           IND_POL(2) = 2
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PLCODE ( GVH, 'IND_CHN1', IND_DB_OBS, 0, IND_CHN1, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 7961, IUER, 'PIMA_GVH_OBS', 'Error in putting '// &
     &         '"IND_CHN1" lcode' )
           RETURN
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PLCODE ( GVH, 'NUM_AP1 ', IND_DB_OBS, 0, NUM_AP1, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 7962, IUER, 'PIMA_GVH_OBS', 'Error in putting '// &
     &         '"NUM_AP1 " lcode' )
           RETURN
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PLCODE ( GVH, 'NUM_SAM1', IND_DB_OBS, 0, NUM_SAM1, IER )

      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 7963, IUER, 'PIMA_GVH_OBS', 'Error in putting '// &
     &         '"NUM_SAM1" lcode' )
           RETURN
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PLCODE ( GVH, 'UV_CHN1 ', IND_DB_OBS, 0, UV_CHN1, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 7964, IUER, 'PIMA_GVH_OBS', 'Error in putting '// &
     &         '"UV_CHN1 " lcode' )
           RETURN
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PLCODE ( GVH, 'TSYS1   ', IND_DB_OBS, 1, TSYS1(1,1), IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 7965, IUER, 'PIMA_GVH_OBS', 'Error in putting '// &
     &         '"TSYS1   " lcode, 1st station' )
           RETURN
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PLCODE ( GVH, 'TSYS1   ', IND_DB_OBS, 2, TSYS1(1,2), IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 7966, IUER, 'PIMA_GVH_OBS', 'Error in putting '// &
     &         '"TSYS1   " lcode, 2nd station' )
           RETURN
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PLCODE ( GVH, 'PCAL_FR1', IND_DB_OBS, 1, PCAL_FR1(1,1), IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 7969, IUER, 'PIMA_GVH_OBS', 'Error in putting '// &
     &         '"PCAL_FR1" lcode, 1st station' )
           RETURN
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PLCODE ( GVH, 'PCAL_FR1', IND_DB_OBS, 2, PCAL_FR1(1,2), IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 7970, IUER, 'PIMA_GVH_OBS', 'Error in putting '// &
     &         '"PCAL_FR1" lcode, 2nd station' )
           RETURN
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PLCODE ( GVH, 'PCAL_CM1', IND_DB_OBS, 1, PCAL_CM1(1,1), IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 7971, IUER, 'PIMA_GVH_OBS', 'Error in putting '// &
     &         '"PCAL_CM1" lcode, 1st station' )
           RETURN
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PLCODE ( GVH, 'PCAL_CM1', IND_DB_OBS, 2, PCAL_CM1(1,2), IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 7972, IUER, 'PIMA_GVH_OBS', 'Error in putting '// &
     &         '"PCAL_CM1" lcode, 2nd station' )
           RETURN
      END IF
!
      IF ( NUMB_BND == 2 ) THEN
           CALL ERR_PASS ( IUER, IER )
           CALL GVH_PLCODE ( GVH, 'IND_CHN2', IND_DB_OBS, 0, IND_CHN2, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 7973, IUER, 'PIMA_GVH_OBS', 'Error in '// &
     &              'putting "IND_CHN2" lcode' )
                RETURN
           END IF
!
           CALL ERR_PASS ( IUER, IER )
           CALL GVH_PLCODE ( GVH, 'NUM_AP2 ', IND_DB_OBS, 0, NUM_AP2, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 7974, IUER, 'PIMA_GVH_OBS', 'Error in '// &
     &              'putting "NUM_AP2 " lcode' )
                RETURN
           END IF
!
           CALL ERR_PASS ( IUER, IER )
           CALL GVH_PLCODE ( GVH, 'NUM_SAM2', IND_DB_OBS, 0, NUM_SAM2, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 7975, IUER, 'PIMA_GVH_OBS', 'Error in '// &
     &              'putting "NUM_SAM2" lcode' )
                RETURN
           END IF
!
           CALL ERR_PASS ( IUER, IER )
           CALL GVH_PLCODE ( GVH, 'UV_CHN2 ', IND_DB_OBS, 0, UV_CHN2, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 7976, IUER, 'PIMA_GVH_OBS', 'Error in '// &
     &              'putting "UV_CHN2 " lcode' )
                RETURN
           END IF
!
           CALL ERR_PASS ( IUER, IER )
           CALL GVH_PLCODE ( GVH, 'TSYS2   ', IND_DB_OBS, 1, TSYS2(1,1), IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 7977, IUER, 'PIMA_GVH_OBS', 'Error in '// &
     &              'putting "TSYS2   " lcode, 1st station' )
                RETURN
           END IF
!
           CALL ERR_PASS ( IUER, IER )
           CALL GVH_PLCODE ( GVH, 'TSYS2   ', IND_DB_OBS, 2, TSYS2(1,2), IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 7978, IUER, 'PIMA_GVH_OBS', 'Error in '// &
     &              'putting "TSYS2   " lcode, 2nd station' )
                RETURN
           END IF
!
           CALL ERR_PASS ( IUER, IER )
           CALL GVH_PLCODE ( GVH, 'PCAL_FR2', IND_DB_OBS, 1, PCAL_FR2(1,2), IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 7979, IUER, 'PIMA_GVH_OBS', 'Error in '// &
     &              'putting "PCAL_FR1" lcode, 1st station' )
                RETURN
           END IF
!
           CALL ERR_PASS ( IUER, IER )
           CALL GVH_PLCODE ( GVH, 'PCAL_FR2', IND_DB_OBS, 2, PCAL_FR2(1,2), IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 7980, IUER, 'PIMA_GVH_OBS', 'Error in '// &
     &              'putting "PCAL_FR2" lcode, 2nd station' )
                RETURN
           END IF
!
           CALL ERR_PASS ( IUER, IER )
           CALL GVH_PLCODE ( GVH, 'PCAL_CM2', IND_DB_OBS, 1, PCAL_CM2(1,1), IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 7981, IUER, 'PIMA_GVH_OBS', 'Error in '// &
     &              'putting "PCAL_CM2" lcode, 1st station' )
                RETURN
           END IF
!
           CALL ERR_PASS ( IUER, IER )
           CALL GVH_PLCODE ( GVH, 'PCAL_CM2', IND_DB_OBS, 2, PCAL_CM2(1,2), IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 7982, IUER, 'PIMA_GVH_OBS', 'Error in '// &
     &              'putting "PCAL_CM2" lcode, 2nd station' )
                RETURN
           END IF
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PLCODE ( GVH, 'NUSEDCHN', IND_DB_OBS, 0, NUSEDCHN, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 7983, IUER, 'PIMA_GVH_OBS', 'Error in putting '// &
     &         '"NUSEDCHN" lcode' )
           RETURN
      END IF
!
! --- NB: the order of stations can be reverse!
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PLCODE ( GVH, 'CABL_DEL', IND_DB_OBS, 1, CABLE(1), IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 7984, IUER, 'PIMA_GVH_OBS', 'Error in putting '// &
     &         '"CABL_DEL" 1st station lcode' )
           RETURN
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PLCODE ( GVH, 'CABL_DEL', IND_DB_OBS, 2, CABLE(2), IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 7985, IUER, 'PIMA_GVH_OBS', 'Error in putting '// &
     &         '"CABL_DEL" 2nd station lcode' )
           RETURN
      END IF
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!%   write ( 6, * ) ' PIMA_GVF_OBS  ind_db_obs= ', ind_db_obs, ' cab= ', sngl(cable(1:2)) ! %%%%%%
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PLCODE ( GVH, 'AUTO_SUP', IND_DB_OBS, 0, AUTO_SUP, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 7986, IUER, 'PIMA_GVH_OBS', 'Error in putting '// &
     &         '"AUTO_SUP" lcode' )
           RETURN
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PLCODE ( GVH, 'USER_REC', IND_DB_OBS, 0, USER_REC, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 7987, IUER, 'PIMA_GVH_OBS', 'Error in putting '// &
     &         '"USER_REC" lcode' )
           RETURN
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PLCODE ( GVH, 'USER_SUP', IND_DB_OBS, 0, USER_SUP, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 7988, IUER, 'PIMA_GVH_OBS', 'Error in putting '// &
     &         '"USER_SUP" lcode' )
           RETURN
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  PIMA_GVH_OBS  !#!#
