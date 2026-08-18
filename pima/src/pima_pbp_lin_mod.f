      SUBROUTINE PIMA_PBP_LIN_MOD ( MODE, PIM, VTD, POL_ARR, IUER )
! ************************************************************************
! *                                                                      *
! *   PIMA_PBP_LIN_MOD computes the linear model of the polarization     *
! *   bandpass for both reference and remote stations the lin-lin        *
! *   polarization mode.                                                 *
! *                                                                      *
! *   Copyright (c) 1975-2026 United States Government as represented by *
! *   the Administrator of the National Aeronautics and Space            *
! *   Administration. All Rights Reserved.                               *
! *   License: NASA Open Source Software Agreement (NOSA).               *
! *                                                                      *
! * ## 10-JUL-2026  PIMA_PBP_LIN_MOD v1.0 (d) L. Petrov  17-JUL-2026 ##  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'pima.i'
      INCLUDE   'vtd.i'
      TYPE     ( PIMA__TYPE         ) :: PIM
      TYPE     ( VTD__TYPE          ) :: VTD
      CHARACTER  MODE*(*), POL_ARR(PIMA__POL_MAX)*1
      INTEGER*4  IUER
      CHARACTER  BPASS_STYLE*8, FRIB_STYLE*8, POLARCAL_FILE_SAVE*128, STR*128
      REAL*8     GR_DEL(PIM__MFRA), PH_RAT(PIM__MFRA), GR_RAT, &
     &           PHAS(PIM__MFRA), TIME_FRT(PIM__MPLR), SNR, PH_ACC, &
     &           AMPL(PIM__MFRA), &
     &           GR_DEL_ERR(PIM__MFRA), PH_DEL_ERR(PIM__MFRA), &
     &           PH_RAT_ERR(PIM__MFRA), PH_ACC_ERR, &
     &           PHAS_ERR(PIM__MFRA), GR_RAT_ERR, GRAMBSP, &
     &           EFF_FRQ_PHS, EFF_FRQ_GRP, EFF_FRQ_RAT, &
     &           COV_PR_PH, COV_GR_MD, EFF_DURA, FREQ_REF, &
     &           TEC, TEC_RATE, TEC_ERR, TEC_RATE_ERR, DECOR_TIM, &
     &           PHS_MSC, GRD_MSC, PHR_MSC, PA, &
     &           PHS_ARR(4), GRD_ARR(4), SNR_ARR(4), &
     &           PH_REF, PH_REM, GD_REF, GD_REM, PH_MAX_RES, GD_MAX_RES
      INTEGER*4  J1, J2, J3, J4, J5, J6, J7, POL_MODE, IND_OBS, LCHN, LFRQ, &
     &           LTIM, IND_FRA, FRI_STS, LPOL, DEBUG_LEVEL_SAVE, IER
!
      IF ( PIM%CONF%FRIB_FINE_SEARCH == PIMA__FINE_SEARCH_LSQ ) THEN
           IND_FRA = PIMA__LSQ
         ELSE
           IND_FRA = PIMA__DRF
      END IF
      FRIB_STYLE  = PIMA__2FFT
      BPASS_STYLE = PIMA__BPASS_AMP_PHS
      LCHN = PIM%NCHN
      LFRQ = PIM%CONF%END_FRQ - PIM%CONF%BEG_FRQ + 1
!
      PIM%CONF%POLARCAL_FILE       = PIMA__POLARCAL_NO
      PIM%CONF%FRIB_2D_FRINGE_PLOT = PIMA__PLOT_NO
      LPOL = 1
!
      DO 410 J1=1,PIM%NSTA
         DO 420 J2=PIM%CONF%BEG_FRQ,PIM%CONF%END_FRQ
            DO 430 J3=1,LCHN
               PIM%BPASS(J1)%BPS(J3,J2) = PIM%BPS%CMPL(J3,J2,J1)
 430        CONTINUE 
 420     CONTINUE 
 410  CONTINUE 
!
      DO 440 J4=1,PIM%NSTA
         IF ( PIM%CONF%DEBUG_LEVEL .GE. 4 ) THEN
              WRITE ( 6, 160 ) J4, PIM%C_STA(J4)
 160          FORMAT ( 'PIMA_PBP_LIN_MOD: Ista: ', I2, ' Sta: ', A )
         END IF
!!         IF ( J4 .NE. PIM%BPS%IND_STA_REF ) GOTO 440
         DO 450 J5=1,PIM%BPS%NUM_OBS_ACCUM(J4)
            IND_OBS = PIM%BPS%IND_OBS_SEL(J5,J4)
            LTIM = PIM%OBS(IND_OBS)%NUM_EPC(PIM%OBS(IND_OBS)%REF_FRG_INDS(PIM%CONF%FRQ_GRP))
!
            DEBUG_LEVEL_SAVE     = PIM%CONF%DEBUG_LEVEL
            IF ( PIM%CONF%DEBUG_LEVEL < 6 ) PIM%CONF%DEBUG_LEVEL = 1
            DO 460 J6=1,PIM%NSTK
               TIME_FRT(J6) = PIMA__FRT_UNDF
               IF ( ( PIM%STA(PIM%OBS(IND_OBS)%STA_IND(1))%POL_TYP(1) == PIMA__POL_H .OR. &
     &                PIM%STA(PIM%OBS(IND_OBS)%STA_IND(1))%POL_TYP(1) == PIMA__POL_V      ) .AND. &
     &              ( PIM%STA(PIM%OBS(IND_OBS)%STA_IND(2))%POL_TYP(1) == PIMA__POL_H .OR. &
     &                PIM%STA(PIM%OBS(IND_OBS)%STA_IND(2))%POL_TYP(1) == PIMA__POL_V      )     ) THEN
                    IF ( J6 == 1 ) POL_MODE = PIMA__HHLL
                    IF ( J6 == 2 ) POL_MODE = PIMA__VHLL  
                    IF ( J6 == 3 ) POL_MODE = PIMA__HVLL
                    IF ( J6 == 4 ) POL_MODE = PIMA__VVLL
                  ELSE 
                    CALL CLRCH ( STR )
                    CALL INCH  ( IND_OBS, STR )
                    CALL ERR_LOG ( 6431, IUER, 'PIMA_PBP_LIM_MOD', 'Mixed polarization '// &
     &                  'mode is not yet supported' )
                    PIM%CONF%POLARCAL_FILE = POLARCAL_FILE_SAVE 
                    PIM%CONF%DEBUG_LEVEL   = DEBUG_LEVEL_SAVE             
                    RETURN 
               END IF
!
! ------------ Get UV data and put them in PIM%OBS(IND_OBS)%UV, PIM%OBS(IND_OBS)%UV_IF, PIM%OBS(IND_OBS)%UV_BAND
!
               CALL ERR_PASS ( IUER, IER )
               CALL PIMA_GET_OBS ( PIM, VTD, IND_OBS, POL_MODE, LPOL, &
     &                             .TRUE., .TRUE., .FALSE., FRI_STS, IER )
               IF ( IER .NE. 0 ) THEN
                    CALL ERR_LOG ( 6432, IUER, 'PIMA_PBP_LIM_MOD', 'Error in getting '// &
     &                  'visibility data' )
                    PIM%CONF%POLARCAL_FILE = POLARCAL_FILE_SAVE 
                    PIM%CONF%DEBUG_LEVEL   = DEBUG_LEVEL_SAVE             
                    RETURN 
               END IF
               FREQ_REF = PIM%FREQ_ARR(1,PIM%CONF%BEG_FRQ,PIM%CONF%FRQ_GRP)
!
! ------------ Run fringe fitting
!
               CALL ERR_PASS ( IUER, IER )
               CALL PIMA_2FFT ( PIM, VTD, IND_OBS, LCHN, LFRQ, LTIM, &
     &                          PIM%FREQ_ARR(1,PIM%CONF%BEG_FRQ,PIM%CONF%FRQ_GRP), &
     &                          FREQ_REF, PIM%OBS(IND_OBS)%UV(1,1,1,1), &
     &                          PIM%OBS(IND_OBS)%WEI_1D, PIM%OBS(IND_OBS)%AP_LEN, &
     &                          PIMA__PLOT_NO, TIME_FRT(J6), GR_DEL, PH_RAT, GR_RAT, &
     &                          PH_ACC, PHAS, AMPL, SNR, &
     &                          GR_DEL_ERR, PH_DEL_ERR, PH_RAT_ERR, &
     &                          GR_RAT_ERR, PH_ACC_ERR, PHAS_ERR, GRAMBSP, &
     &                          EFF_FRQ_PHS, EFF_FRQ_GRP, EFF_FRQ_RAT, &
     &                          COV_PR_PH, COV_GR_MD, EFF_DURA, &
     &                          TEC, TEC_RATE, TEC_ERR, TEC_RATE_ERR, DECOR_TIM, IER )
!
               IF ( IER .NE. 0 ) THEN
                    CALL CLRCH ( STR )
                    CALL INCH  ( IND_OBS, STR )
                    CALL ERR_LOG ( 6433, IUER, 'PIMA_PBP_LIM_MOD', 'Error in '// &
     &                  'running fringe fitting for observation '//TRIM(STR) )
                    PIM%CONF%POLARCAL_FILE = POLARCAL_FILE_SAVE 
                    PIM%CONF%DEBUG_LEVEL   = DEBUG_LEVEL_SAVE             
                    RETURN 
               END IF
               PIM%BPS%GR_DEL(J5,J4,J6) = GR_DEL(IND_FRA)
               PIM%BPS%PH_RAT(J5,J4,J6) = PH_RAT(IND_FRA)
               PIM%BPS%GR_RAT(J5,J4,J6) = GR_RAT
               PIM%BPS%PHS(J5,J4,J6) = PHAS(IND_FRA)
               PIM%BPS%SNR(J5,J4,J6) = SNR
 460        CONTINUE 
            PIM%CONF%DEBUG_LEVEL = DEBUG_LEVEL_SAVE             
            PA = PIM%OBS(IND_OBS)%FEED_ANG(1) - PIM%OBS(IND_OBS)%FEED_ANG(2)
            IF ( PA < 0 )  THEN
                 PIM%BPS%PHS(J5,J4,2) = PIM%BPS%PHS(J5,J4,2) + PI__NUM
                 PIM%BPS%PHS(J5,J4,3) = PIM%BPS%PHS(J5,J4,3) + PI__NUM
            END IF
            IF ( PIM%BPS%PHS(J5,J4,2) - PIM%BPS%PHS(J5,J4,1) < 0.0 ) PIM%BPS%PHS(J5,J4,2) = PIM%BPS%PHS(J5,J4,2) + PI2
            IF ( PIM%BPS%PHS(J5,J4,3) - PIM%BPS%PHS(J5,J4,1) < 0.0 ) PIM%BPS%PHS(J5,J4,3) = PIM%BPS%PHS(J5,J4,3) + PI2
            PIM%BPS%SNR_ALL(J5,J4) = MIN ( PIM%BPS%SNR(J5,J4,1), &
     &                                                      PIM%BPS%SNR(J5,J4,2), &
     &                                                      PIM%BPS%SNR(J5,J4,3), &
     &                                                      PIM%BPS%SNR(J5,J4,4)  )
            PIM%BPS%SNR_PAR(J5,J4) = MIN ( PIM%BPS%SNR(J5,J4,1), &
     &                                                      PIM%BPS%SNR(J5,J4,4)  )
            PIM%BPS%SNR_CRS(J5,J4) = MIN ( PIM%BPS%SNR(J5,J4,2), &
     &                                                      PIM%BPS%SNR(J5,J4,3)  )
!
            PHS_MSC  = (PIM%BPS%PHS(J5,J4,2) - PIM%BPS%PHS(J5,J4,1)) + &
     &                 PI__NUM + &
     &                 (PIM%BPS%PHS(J5,J4,3) - PIM%BPS%PHS(J5,J4,1)) - &
     &                 (PIM%BPS%PHS(J5,J4,4) - PIM%BPS%PHS(J5,J4,1)) 
            IF ( PHS_MSC < -PI__NUM ) THEN
                 PIM%BPS%PHS(J5,J4,3) = PIM%BPS%PHS(J5,J4,3) + PI2
                 PHS_MSC = PHS_MSC + PI2
            END IF
            IF ( PHS_MSC > PI__NUM ) THEN
                 PIM%BPS%PHS(J5,J4,3) = PIM%BPS%PHS(J5,J4,3) - PI2
                 PHS_MSC = PHS_MSC - PI2
            END IF
            IF ( PHS_MSC > PI__NUM ) THEN
                 PHS_MSC = PHS_MSC - PI2
                 PIM%BPS%PHS(J5,J4,4) = PIM%BPS%PHS(J5,J4,4) + PI2
            END IF
            IF ( PHS_MSC < -PI__NUM ) THEN
                 PHS_MSC = PHS_MSC + PI2
                 PIM%BPS%PHS(J5,J4,4) = PIM%BPS%PHS(J5,J4,4) - PI2
            END IF
            PHS_MSC = (PIM%BPS%PHS(J5,J4,2) - PIM%BPS%PHS(J5,J4,1)) + &
     &                ((PIM%BPS%PHS(J5,J4,3) - PIM%BPS%PHS(J5,J4,1)) + PI__NUM ) - &
     &                (PIM%BPS%PHS(J5,J4,4) - PIM%BPS%PHS(J5,J4,1))
            GRD_MSC = (PIM%BPS%GR_DEL(J5,J4,2) - PIM%BPS%GR_DEL(J5,J4,1)) + &
     &                (PIM%BPS%GR_DEL(J5,J4,3) - PIM%BPS%GR_DEL(J5,J4,1)) - &
     &                (PIM%BPS%GR_DEL(J5,J4,4) - PIM%BPS%GR_DEL(J5,J4,1))
            PHR_MSC = (PIM%BPS%PH_RAT(J5,J4,2) - PIM%BPS%PH_RAT(J5,J4,1)) - &
     &                (PIM%BPS%PH_RAT(J5,J4,3) - PIM%BPS%PH_RAT(J5,J4,1)) - &
     &                (PIM%BPS%PH_RAT(J5,J4,4) - PIM%BPS%PH_RAT(J5,J4,1))
!
            IF ( PIM%CONF%DEBUG_LEVEL .GE. 4 ) THEN
                 WRITE ( 6, 110 ) IND_OBS, &
     &                            1.D9*(PIM%BPS%GR_DEL(J5,J4,2) - PIM%BPS%GR_DEL(J5,J4,1)), &
     &                            1.D9*(PIM%BPS%GR_DEL(J5,J4,3) - PIM%BPS%GR_DEL(J5,J4,1)), &
     &                            1.D9*(PIM%BPS%GR_DEL(J5,J4,4) - PIM%BPS%GR_DEL(J5,J4,1)), &
     &                            1.D9*GRD_MSC, PIM%OBS(PIM%BPS%IND_OBS_SEL(J5,J4))%STA_IND
 110             FORMAT ( 'PIMA_PBP_LIN_MOD Obs: ', I6, ' Gr_del= ', 3(F9.3,1X), &
     &                    '  Gr_del_msc: ', F9.3, ' ns  sta_ind: ', I2, 1X, I2 )
                 WRITE ( 6, 120 ) PIM%C_SOU(PIM%OBS(IND_OBS)%SOU_IND), &
     &                           (PIM%BPS%PHS(J5,J4,2) - PIM%BPS%PHS(J5,J4,1)), &
     &                           (PIM%BPS%PHS(J5,J4,3) - PIM%BPS%PHS(J5,J4,1)), &
     &                           (PIM%BPS%PHS(J5,J4,4) - PIM%BPS%PHS(J5,J4,1)), &
     &                           PHS_MSC, PIM%OBS(PIM%BPS%IND_OBS_SEL(J5,J4))%STA_IND, &
     &                           PA
 120             FORMAT ( 'PIMA_PBP_LIN_MOD ', 3X, A, ' Phs= ', 3(F8.5,1X), &
     &                    '  Phs_msc: ', F8.5, ' rad  sta_ind: ', I2, 1X, I2, ' Pad: ', F8.5, ' rad' )
                 WRITE ( 6, 130 ) PIM%C_SOU(PIM%OBS(IND_OBS)%SOU_IND), &
     &                           1.D12*(PIM%BPS%PH_RAT(J5,J4,2) - PIM%BPS%PH_RAT(J5,J4,1)), &
     &                           1.D12*(PIM%BPS%PH_RAT(J5,J4,3) - PIM%BPS%PH_RAT(J5,J4,1)), &
     &                           1.D12*(PIM%BPS%PH_RAT(J5,J4,4) - PIM%BPS%PH_RAT(J5,J4,1)), &
     &                           1.D12*PHR_MSC, PIM%OBS(PIM%BPS%IND_OBS_SEL(J5,J4))%STA_IND
 130             FORMAT ( 'PIMA_PBP_LIN_MOD ', 3X, A, ' Phr= ', 3(F8.3,1X), &
     &                    '  Phr_msc: ', F8.3, ' rad/sec  ind: ', I2, 1X, I2 )
                 WRITE ( 6, 140 ) PIM%BPS%SNR(J5,J4,1:4)
 140             FORMAT ( 'PIMA_PBP_LIN_MOD ', 11X, ' SNR =', 4(F7.1,1X) )
                 WRITE ( 6, 150 ) TIME_FRT
 150             FORMAT ( 'PIMA_PBP_LIN_MOD ', 11X, ' TIM_FRT =', 4(F12.6,1X) )
            END IF
!
!
            IF ( MODE .NE. PIMA__BPASS_ACCUM .AND. &
     &           MODE .NE. PIMA__BPASS_FINE        ) THEN
                 CALL ERR_PASS ( IUER, IER )
                 PHS_ARR(1:4) = PIM%BPS%PHS(J5,J4,1:4)
                 GRD_ARR(1:4) = PIM%BPS%GR_DEL(J5,J4,1:4)
                 SNR_ARR(1:4) = PIM%BPS%SNR(J5,J4,1:4)
                 CALL PIMA_PBP_SOLVE_LIN ( PHS_ARR, GRD_ARR, SNR_ARR, PH_REF, PH_REM, &
     &                                     GD_REF, GD_REM, PH_MAX_RES, GD_MAX_RES, IER )
                 IF ( IER .NE. 0 ) THEN
                      CALL CLRCH ( STR )
                      CALL INCH  ( IND_OBS, STR )
                      CALL ERR_LOG ( 6434, IUER, 'PIMA_PBP_LIM_MOD', 'Error in '// &
     &                    'running fringe fitting for observation '//TRIM(STR) )
                      PIM%CONF%POLARCAL_FILE = POLARCAL_FILE_SAVE 
                      PIM%CONF%DEBUG_LEVEL   = DEBUG_LEVEL_SAVE             
                      RETURN 
                 END IF
                 IF ( PIM%CONF%DEBUG_LEVEL .GE. 2 ) THEN
                      WRITE ( 6, 170 ) IND_OBS, PIM%C_STA(PIM%OBS(IND_OBS)%STA_IND(1)), &
     &                                          PIM%C_STA(PIM%OBS(IND_OBS)%STA_IND(2)), &
     &                                          PIM%C_SOU(PIM%OBS(IND_OBS)%SOU_IND), &
     &                                          PH_REF, PH_REM, PH_MAX_RES, &
     &                                          1.D9*GD_REF, 1.D9*GD_REM, 1.D9*GD_MAX_RES
 170                  FORMAT ( 'Ind_obs: ', I6, ' Sta: ', A, ' / ', A, ' Sou: ', A8, &
     &                         ' VH_Phs_ref: ', F8.4, ' VH_Phs_rem: ', F8.4, ' Phs_max_res: ', F8.4, ' rad ', &
     &                         ' VH_Grd_ref: ', F8.4, ' VH_Grd_rem: ', F8.4, ' Grd_max_res: ', F8.4, ' nsec ' )
                  END IF
!                 GOTO 440
            END IF
            IF ( PIM%CONF%DEBUG_LEVEL .GE. 4 ) THEN
                 WRITE ( 6, '(A)' ) ' '
            END IF
 450     CONTINUE 
 440  CONTINUE 
      PIM%CONF%POLARCAL_FILE = POLARCAL_FILE_SAVE 
      PIM%CONF%DEBUG_LEVEL   = DEBUG_LEVEL_SAVE             
!
      DO 470 J7=1,PIM%NSTA
         PIM%BPASS(J7)%BPS = CMPLX(0.0,0.0)
 470  CONTINUE 
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  PIMA_PBP_LIN_MOD  !#!#  
!
! ------------------------------------------------------------------------
!
      SUBROUTINE PIMA_PBP_SOLVE_LIN ( PHS_ARR, GRD_ARR, SNR_ARR, & 
     &                                PHS_REF, PHS_REM, GRD_REF, GRD_REM, &
     &                                PHS_MAX_RES, GRD_MAX_RES, IUER  )
! ************************************************************************
! *                                                                      *
! *   Routine PIMA_PBP_SOLVE_LIN solves the linear system of equations   *
! *   using on the array of phases and group delays of HH, VH, VH, VV    *
! *   polarizations. It determines the difference in phases V minus H    *
! *   polarizations for the reference and remote stations (PHS_REF and   *
! *   PHS_REM) and differences of group delays V minus H polarizations.  *
! *                                                                      *
! *   It also compuites PHS_MAX_RES and GRD_MAX_RES -- maximum by        *
! *   abolsute value of phase and group delay residuals.                 *
! *                                                                      *
! *   Copyright (c) 1975-2026 United States Government as represented by *
! *   the Administrator of the National Aeronautics and Space            *
! *   Administration. All Rights Reserved.                               *
! *   License: NASA Open Source Software Agreement (NOSA).               *
! *                                                                      *
! * ## 17-JUL-2026 PIMA_PBP_SOLVE_LIN v1.0 (d) L. Petrov 17-JUL-2026 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      REAL*8     PHS_ARR(4), GRD_ARR(4), SNR_ARR(4), PHS_REF, PHS_REM, &
     &           GRD_REF, GRD_REM, PHS_MAX_RES, GRD_MAX_RES
      REAL*8     NOR_MAT(3), NOR_VEC(2), OBS_MAT(2,3), OBS_VEC(3), &
     &           WEI, RCOND, EST_VEC(2), RES
      INTEGER*4  IUER
      REAL*8,    EXTERNAL :: DP_VV_V
      INTEGER*4  J1, J2, J3, J4, IER
!
      OBS_VEC(1)   = PHS_ARR(2) - PHS_ARR(1)
      OBS_VEC(2)   = PHS_ARR(3) - PHS_ARR(1) + PI__NUM
      OBS_VEC(3)   = PHS_ARR(4) - PHS_ARR(1)
      OBS_MAT(1,1) =  1.0D0 ; OBS_MAT(2,1) =  0.0D0 
      OBS_MAT(1,2) =  0.0D0 ; OBS_MAT(2,2) = -1.0D0
      OBS_MAT(1,3) =  1.0D0 ; OBS_MAT(2,3) = -1.0D0
!
      NOR_MAT = 0.0D0
      NOR_VEC = 0.0D0
      DO 410 J1=1,3
         WEI = DSQRT ( SNR_ARR(1)**2 + SNR_ARR(J1+1)**2 )
         CALL DIAD_CVT_S ( WEI**2, 2, OBS_MAT(1,J1), OBS_MAT(1,J1), NOR_MAT )
         CALL NORVEC_UPD ( 2, WEI, OBS_VEC(J1), OBS_MAT(1,J1), NOR_VEC )
 410  CONTINUE 
!
      CALL ERR_PASS ( IUER, IER )
      CALL INVS     ( 2, NOR_MAT, RCOND, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 4641, IUER, 'PIMA_PBP_SOLVE_LIN', 'Error '// &
     &         'in matrix inversion' )
           RETURN 
      END IF
!
      CALL MUL_MV_SV_V ( 2, NOR_MAT, 2, NOR_VEC, 2, EST_VEC, IER )
      PHS_REF = EST_VEC(1)
      PHS_REM = EST_VEC(2)
      PHS_MAX_RES = -1.0D0
      DO 420 J2=1,3
         RES = OBS_VEC(J2) - DP_VV_V ( 2, EST_VEC, OBS_MAT(1,J2) ) 
         IF ( ABS(RES) > PHS_MAX_RES ) THEN
              PHS_MAX_RES = ABS(RES)
         END IF
 420  CONTINUE 
!
! --- Now group delays
!
      OBS_VEC(1)   = GRD_ARR(2) - GRD_ARR(1)
      OBS_VEC(2)   = GRD_ARR(3) - GRD_ARR(1)
      OBS_VEC(3)   = GRD_ARR(4) - GRD_ARR(1)
      OBS_MAT(1,1) =  1.0D0 ; OBS_MAT(2,1) =  0.0D0 
      OBS_MAT(1,2) =  0.0D0 ; OBS_MAT(2,2) =  1.0D0
      OBS_MAT(1,3) =  1.0D0 ; OBS_MAT(2,3) =  1.0D0
!
      NOR_MAT = 0.0D0
      NOR_VEC = 0.0D0
      DO 430 J3=1,3
         WEI = DSQRT ( SNR_ARR(1)**2 + SNR_ARR(J3+1)**2 )
         CALL DIAD_CVT_S ( WEI**2, 2, OBS_MAT(1,J3), OBS_MAT(1,J3), NOR_MAT )
         CALL NORVEC_UPD ( 2, WEI, OBS_VEC(J3), OBS_MAT(1,J3), NOR_VEC )
 430  CONTINUE 
!
      CALL ERR_PASS ( IUER, IER )
      CALL INVS     ( 2, NOR_MAT, RCOND, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 4641, IUER, 'PIMA_PBP_SOLVE_LIN', 'Error '// &
     &         'in matrix inversion' )
           RETURN 
      END IF
!
      CALL MUL_MV_SV_V ( 2, NOR_MAT, 2, NOR_VEC, 2, EST_VEC, IER )
      GRD_REF = EST_VEC(1)
      GRD_REM = EST_VEC(2)
      GRD_MAX_RES = -1.0D0
      DO 440 J4=1,3
         RES = OBS_VEC(J4) - DP_VV_V ( 2, EST_VEC, OBS_MAT(1,J4) ) 
         IF ( ABS(RES) > GRD_MAX_RES ) THEN
              GRD_MAX_RES = ABS(RES)
         END IF
 440  CONTINUE 
!      
      CALL ERR_LOG ( 0, IUER )      
      RETURN
      END  SUBROUTINE  PIMA_PBP_SOLVE_LIN  !#!#
