      SUBROUTINE PIMA_UVO_UPDATE ( PIM, LFRQ, KFRQ, KPOL, LTIM, IND_OBS, ITIM, &
     &                             UV, WEI_FRQ_CHN, IND_FRQ, UVO, FRQ_ARR, &
     &                             TIM_AP, NUMP_SEG, SUMVIS_WEISEG, &
     &                             SUMVIS_WEISQ_SEG, SUM_WEI_SEG, &
     &                             SUM_WEI_BAND, NUMP_BAND, DRF_IF, &
     &                             WEI_IF, WW_IF, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine PIMA_UVO_UPDATE updates the accumulators for output        *
! *   spectrum, weights, and weighted time epoch for the data at time    *
! *   time epoch ITIM.                                                   *
! *                                                                      *
! * ### 17-FEB-2011 PIMA_UVO_UPDATE  v3.2 (c) L. Petrov  28-NOV-2020 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'astro_constants.i'
      INCLUDE   'pima.i'
      TYPE     ( PIMA__TYPE  ) :: PIM
      INTEGER*4  LFRQ, KFRQ, KPOL, LTIM, IND_OBS, ITIM, &
     &           KSEG, IND_FRQ(4,KFRQ), NUMP_SEG(PIM__MPLR,KFRQ), &
     &           NUMP_BAND(PIM__MPLR), IUER
      COMPLEX*8  DRF_IF(PIM__MCHN*PIM__MFRQ,PIM__MPLR)
      REAL*8     FRQ_ARR(KFRQ)
      REAL*4     WEI_FRQ_CHN(PIM%NCHN,PIM%NFRQ), &
     &           SUMVIS_WEISQ_SEG(PIM__MPLR,KFRQ), &
     &           SUM_WEI_SEG(PIM__MPLR,KFRQ), &
     &           SUM_WEI_BAND(PIM__MPLR), &
     &           WEI_IF(PIM__MCHN*PIM__MFRQ,PIM__MPLR), &
     &           WW_IF(PIM__MCHN*PIM__MFRQ,PIM__MPLR), WEI
      LOGICAL*1  FL_BPASS, FL_NOMOD
      REAL*8     TIM_AP, FRQ_STP
      COMPLEX*8  UV(PIM%NCHN,LFRQ,LTIM,KPOL), SUMVIS_WEISEG(PIM__MPLR,KFRQ)
      TYPE     ( UVO__TYPE ) :: UVO
      CHARACTER  STR*128
      COMPLEX*8  DRF, VIS_CAL, VIS_ROT, VIS_STP, BPASS_C8
      LOGICAL*1  FL_NOFRI, FL_USE_1ST_IF, FL_NODATA, FL_SUBSRT, FL_PRF
      REAL*4     PHS_MOD, WEI_SUM
      REAL*8     PHAS, PH_RAT, GR_RAT, GR_DEL, PH_ACC, AP_LEN, TIME_FRT, &
     &           FRQ_STEP, DTF, DTS, DTU
      INTEGER*4  J1, J2, J3, J4, J5, IND_BND, IND_FRA, IFRQ, UV_IND, IND_STA(2), &
     &           FRG_IND, IER
      REAL*8,    EXTERNAL :: GET_APR_SEFD_BAS_FRQ
      REAL*4,    EXTERNAL :: ATAN_CS_R4, PHAS_CMPL_R4
      LOGICAL*4, EXTERNAL :: IS_R4_NAN
!
      FL_NOFRI = .FALSE.
      CALL GETENVAR ( 'PIMAVAR_SPLT_NOFRINGE', STR )
      CALL TRAN ( 11, STR, STR )
      IF ( STR == 'YES' ) FL_NOFRI = .TRUE.
!
      FL_USE_1ST_IF = .FALSE.
      CALL GETENVAR ( 'PIMAVAR_SPLT_TSYS_FIRST_IF', STR )
      CALL TRAN ( 11, STR, STR )
      IF ( STR == 'YES' ) FL_USE_1ST_IF = .TRUE.
!
      FL_NODATA = .FALSE.
      CALL GETENVAR ( 'PIMAVAR_SPLT_NODATA', STR )
      CALL TRAN ( 11, STR, STR )
      IF ( STR == 'YES' ) FL_NODATA = .TRUE.
!
      FL_SUBSRT = .FALSE.
      CALL GETENVAR ( 'PIMAVAR_SPLT_SUB_SRT', STR )
      CALL TRAN ( 11, STR, STR )
      IF ( STR == 'YES' ) FL_SUBSRT = .TRUE.
!
      FL_PRF = .FALSE.
      CALL GETENVAR ( 'PIMAVAR_PHS_REF_FRQ', STR )
      CALL TRAN ( 11, STR, STR )
      IF ( STR == 'YES' ) FL_PRF = .TRUE.
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
      IF ( PIM%CONF%BANDPASS_USE .NE. PIMA__BPASS_NO  .AND. &
     &     ASSOCIATED ( PIM%BPASS )                         ) THEN
           FL_BPASS = .TRUE.
         ELSE
           FL_BPASS = .FALSE.
      END IF
      IND_STA = PIM%OBS(IND_OBS)%STA_IND
!
      IND_BND = 1
      PHAS     = PIM%OBS(IND_OBS)%RES_PHS(IND_FRA,IND_BND)
      PH_RAT   = PIM%OBS(IND_OBS)%RES_PH_RAT(IND_FRA,IND_BND)
      GR_DEL   = PIM%OBS(IND_OBS)%RES_MB_DEL(IND_FRA,IND_BND)
      GR_RAT   = PIM%OBS(IND_OBS)%RES_GR_RAT(IND_BND)
      PH_ACC   = PIM%OBS(IND_OBS)%RES_PH_ACC(IND_BND)
      AP_LEN   = PIM%OBS(IND_OBS)%AP_LEN
      TIME_FRT = PIM%OBS(IND_OBS)%FRT_OFFSET(IND_BND)
      IF ( GR_RAT < -0.5D0 ) THEN
           GR_RAT   = 0.0
         ELSE 
           PH_ACC   = 0.0D0
      END IF
      FRG_IND = PIM%OBS(IND_OBS)%REF_FRG_INDS(PIM%CONF%FRQ_GRP)
!
! --- DTF  -- amount of time elapsed from fringe   reference time
! --- DTS  -- amount of time elapsed from segement reference time
!
      DTF  = TIM_AP - ( PIM%TIM_R8( PIM%UV_IND(PIM%OBS(IND_OBS)%UV_IND(1,FRG_IND))%TIM_IND )  + TIME_FRT)
      DTS  = TIM_AP - UVO%TIM
      IF ( FL_SUBSRT .AND. UVO%IND_SUB > 0 ) THEN
           DTS = TIM_AP - PIM%SUB%TIM_SRT(UVO%IND_SUB,PIM%OBS(IND_OBS)%SCA_IND)
      END IF
      IF ( FL_NODATA )  THEN
           UV = (1.0, 0.0 )
      END IF
!
!     ind_frq(1,kfrq) -- index of the 1st spectral channel
!     ind_frq(2,kfrq) -- index of the last spectral channel within the segement
!     ind_frq(3,kfrq) -- global IF index
!     ind_frq(4,kfrq) -- IF index counting from the 1st used IFs
!
      DO 410 J1=1,KFRQ
!
! ------ Get apriori SEFD
!
         IF ( PIM%CONF%GAIN_CAL_CODE == PIMA__GAIN_USE ) THEN
              IF ( FL_USE_1ST_IF ) THEN
                   UVO%SEFD(J1,KPOL) = GET_APR_SEFD_BAS_FRQ ( PIM, IND_OBS, IND_FRQ(3,1)  )
                 ELSE
                   UVO%SEFD(J1,KPOL) = GET_APR_SEFD_BAS_FRQ ( PIM, IND_OBS, IND_FRQ(3,J1) )
              END IF
            ELSE
              UVO%SEFD(J1,KPOL) = 1.0D0
         END IF
         FRQ_STEP = PIM%FREQ_ARR(2,IND_FRQ(3,J1),PIM%CONF%FRQ_GRP) - &
     &              PIM%FREQ_ARR(1,IND_FRQ(3,J1),PIM%CONF%FRQ_GRP) 
!
         DRF = 0.0
         WEI_SUM = 0.0
         IFRQ = IND_FRQ(3,J1) - PIM%CONF%BEG_FRQ + 1 ! IF index relative to the low freq of the band
!
! ------ Phase rotation term for the IND_FRQ(1,J1)th spectral channel and
! ------ multiplied by SEFD
!
         PHS_MOD =   PI2* PH_RAT*FRQ_ARR(J1)*DTS &
     &             + PI2*(GR_DEL + GR_RAT*DTF)*&
     &               (PIM%FREQ_ARR(IND_FRQ(1,J1),IND_FRQ(3,J1),PIM%CONF%FRQ_GRP) - FRQ_ARR(J1))
         IF ( FL_PRF ) THEN
              PHS_MOD = PHS_MOD - PI2*(GR_DEL + GR_RAT*DTF)* &
     &                           (PIM%FRQ(PIM%CONF%BEG_FRQ,PIM%CONF%FRQ_GRP)%FREQ - FRQ_ARR(J1) )
         END IF
         VIS_ROT = CMPLX ( COS(PHS_MOD), SIN(PHS_MOD) ) * UVO%SEFD(J1,KPOL)
!
! ------ Complex phasor for the phase rotation increment for the next spectral channel
!
         FRQ_STP = ( PIM%FREQ_ARR(2,IND_FRQ(3,J1),PIM%CONF%FRQ_GRP) - &
     &               PIM%FREQ_ARR(1,IND_FRQ(3,J1),PIM%CONF%FRQ_GRP)   )
         VIS_STP = CMPLX ( COS( PI2*(GR_DEL + GR_RAT*DTF)* FRQ_STP),  &
     &                     SIN( PI2*(GR_DEL + GR_RAT*DTF)* FRQ_STP)   )
         DO 420 J2=IND_FRQ(1,J1),IND_FRQ(2,J1)
            IF ( FL_NOFRI )  THEN
                 VIS_ROT = (1.0, 0.0)
            END IF
!
! --------- Compute calibrated visibility after phase rotation and scaling by the SEFD
!
            VIS_CAL = UV(J2,IFRQ,ITIM,KPOL)* VIS_ROT
            IF ( IS_R4_NAN(REAL(VIS_CAL)) .OR. IS_R4_NAN(IMAG(VIS_CAL)) ) THEN
                 VIS_CAL = 0.0
            END IF
!
            IF ( ABS(VIS_CAL) > PIMA__AMP_MIN     ) THEN
                 WEI = WEI_FRQ_CHN(J2,IND_FRQ(3,J1))
               ELSE
                 WEI = 0.0
            END IF
            DRF      = DRF     + VIS_CAL * WEI
            WEI_SUM  = WEI_SUM +           WEI
!
! --------- Coherent weighted sum of complex visibilities
!
            SUMVIS_WEISEG(KPOL,J1)    = SUMVIS_WEISEG(KPOL,J1) + WEI* VIS_CAL
!
! --------- Coherent weighted sum of complex visibilities squared
!
            SUMVIS_WEISQ_SEG(KPOL,J1) = SUMVIS_WEISQ_SEG(KPOL,J1) + WEI* VIS_CAL*CONJG(VIS_CAL)
            IF ( ABS(VIS_CAL) > PIMA__AMP_MIN ) THEN
                 NUMP_SEG(KPOL,J1) = NUMP_SEG(KPOL,J1) + 1
                 NUMP_BAND(KPOL)   = NUMP_BAND(KPOL)   + 1
            END IF
            VIS_ROT = VIS_ROT * VIS_STP
 420     CONTINUE
         UVO%SPE(J1,KPOL) = UVO%SPE(J1,KPOL) + DRF
         UVO%WEI(J1,KPOL) = UVO%WEI(J1,KPOL) + WEI_SUM
!
         IF ( FL_PRF ) THEN
              VIS_ROT = CMPLX ( 1.0, 0.0 )
            ELSE
!
! ----------- When we compute total visilities averaged over entire scan, we need 
! ----------- re-refer phase to
! ----------- 1) fringe reference time
! ----------- 2) band reference frequency
!
              PHS_MOD = + PI2*  PH_RAT*(PIM%FRQ(PIM%CONF%BEG_FRQ,PIM%CONF%FRQ_GRP)%FREQ - FRQ_ARR(J1))*DTF  &
     &                  + PI2*  PH_RAT*FRQ_ARR(J1)*(DTF - DTS) &
     &                  - PI2* (GR_DEL + GR_RAT*DTF)* &
     &                         (PIM%FRQ(PIM%CONF%BEG_FRQ,PIM%CONF%FRQ_GRP)%FREQ - FRQ_ARR(J1))
              VIS_ROT = CMPLX ( COS(PHS_MOD), SIN(PHS_MOD) )
         END IF
         DRF_IF(J1,KPOL) = DRF_IF(J1,KPOL) + DRF*VIS_ROT
         WEI_IF(J1,KPOL) = WEI_IF(J1,KPOL) + WEI_SUM
         WW_IF(J1,KPOL)  = WW_IF(J1,KPOL)  + WEI_SUM**2
!
         SUM_WEI_SEG(KPOL,J1) = SUM_WEI_SEG(KPOL,J1) + WEI_SUM  ! Sum of weights in the current segment
         SUM_WEI_BAND(KPOL)   = SUM_WEI_BAND(KPOL)   + WEI_SUM  ! Sum of weights over a band
 410  CONTINUE
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  PIMA_UVO_UPDATE  !#!
