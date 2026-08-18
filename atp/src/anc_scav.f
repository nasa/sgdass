      SUBROUTINE ANC_SCAV ( ANC, DT_MAX, LPRV, CPRV, FILOUT, IUER )
!
! *****************************************************************************
! *                                                                           *
! *   Routine ANC_WRITE                                                       *
! *                                                                           *
! *   INPUT:                                                                  *
! *       ANC    =  Parsed Antenna Calibration file        { DERIVED TYPE }   *
! *                                                                           *
! *       LPRV   =                                         { INT }            *
! *                 If zero then write the provenance as given in ANC%PRV     *
! *                 Else augment provenance with contents of C_PRV            *
! *                                                                           *
! *       CPRV   =  Character holding the provenance block { CHAR }
! *                                                                           *
! *       IUER   =  Error Handler                          { INT, OPT }       *
! *                 If IUER=0 no error message will be printed, even in the   *
! *                 event of an error. However, for other possible values,    *
! *                 i.e. IUER=-1,-2, & -3, the error message will print to    *
! *                 screen. For the latter case, i.e., IUER = -3, after       *
! *                 printing the program will terminate.                      *
! *                 Default, IUER = -1                                        *
! *                                                                           *
! *   OUTPUT:                                                                 *
! *       FILOUT   =  ASCII  antcal File                   { CHAR }           *
! *                   N.B: If declared file exists, it will be overwritten    *
! *                                                                           *
! *   Copyright (c) 1975-2025 United States Government as represented by      *
! *   the Administrator of the National Aeronautics and Space                 *
! *   Administration. All Rights Reserved.                                    *
! *   License: NASA Open Source Software Agreement (NOSA).                    *
! *                                                                           *
! *  ### 9-SEP-2025  ANC_PARSE       v1.0 (d)  N. Habana  9-SEP-2025  ###   *
! *                                                                           *
! *****************************************************************************
!
      IMPLICIT   NONE 
      INCLUDE   'atp.i'
      TYPE     ( ANC__TYP   ) :: ANC      
      CHARACTER  FILOUT*(*), CPRV(ANC__MVPV)*(256)
      INTEGER*4  LPRV, IUER, IER, IVRB, ICNT, IVAR_FLG
      CHARACTER  STR*128, STR1(2)*32, STR2*16
      CHARACTER  STR_DO_DATE*64, TIT*64, CH_EXP*4, CH_VRB
      CHARACTER  CH_IND*12, CH_POL*2, CH_PLOT*4, CH_DATE(2)*22
      INTEGER*4  LBNC, LANC, NUM_TPS, NUM_PCS, LSTR
      INTEGER*8  SIZE_I8, MLEN_STR
      INTEGER*4  UNIX_DATE, IS
      LOGICAL*1  LEX
      CHARACTER  CH_TIM_DIF*6
      REAL*8,    DIMENSION(:,:), ALLOCATABLE :: TIM_TSYS_AVR
      REAL*8,    DIMENSION(:,:), ALLOCATABLE :: TSYS_RMS, TSYS_AVR
      REAL*8,    DIMENSION(:,:), ALLOCATABLE :: AZ_SCA, EL_SCA
      REAL*8     DT_MAX, TIM(ANC__MEPC)
      REAL*8     TSYS(ANC__MEPC), EL_ARR(ANC__MEPC)
      REAL*8     AMP_SCA(ANC__MEPC), AZ_ARR(ANC__MEPC)
      REAL*8     PHA_SCA(ANC__MEPC), TIM_AVR(ANC__MEPC)
      INTEGER*4  NPSCA(ANC__MEPC)
      REAL*8,    DIMENSION(:,:), ALLOCATABLE :: TIM_PCAL_AVR
      COMPLEX*8, DIMENSION(:,:), ALLOCATABLE :: PCAL_AVR
      REAL*8,    DIMENSION(:,:), ALLOCATABLE :: PCAL_AMP_AVR
      REAL*8,    DIMENSION(:,:), ALLOCATABLE :: PCAL_PHA_AVR
      REAL*8,    DIMENSION(:,:), ALLOCATABLE :: PCAL_AMP_RMS
      REAL*8,    DIMENSION(:,:), ALLOCATABLE :: PCAL_PHA_RMS
      COMPLEX*8  PCAL(ANC__MEPC), Z_AVR(ANC__MEPC)
      REAL*8     T1_AVR(ANC__MEPC)
      REAL*8     Y1_AVR(ANC__MEPC), Y1_RMS(ANC__MEPC), Y2_RMS(ANC__MEPC)
      INTEGER*4  IY1_AVR(ANC__MEPC), IY1_RMS(ANC__MEPC)
      INTEGER*4  IY0_AVR(ANC__MEPC), IY0_RMS(ANC__MEPC)
      INTEGER*4  IND_SCA(ANC__MEPC), IARR(ANC__MEPC)
      INTEGER*4  JP, J0, J1, J2, J3, J4, J5, J6, J7, J8
      INTEGER*4  K0, K1, K2, K3, K4, K5, K2CNT
      INTEGER*4  IND_CNT, NP, NOUT, IDX1, IDX2
      INTEGER*4  ICNT_MAX, IPCAL, IPCAL_MAX
      INTEGER*4  NST_ARR(ANC__MTPS), NS_PCAL(ANC__MPCS)
      REAL*8     DELTS, DEL_MIN
      PARAMETER  ( DEL_MIN = 1.D-6 )
      INTEGER*4  ITPS(ANC__MEPC)
      INTEGER*4, DIMENSION(:,:), ALLOCATABLE :: ITPS_SCA, IPCS_SCA
      REAL*8,    EXTERNAL :: MJDSEC_TO_TIM
      CHARACTER, EXTERNAL :: MJDSEC_TO_DATE*30, TIM_TO_DATE*23
      LOGICAL*4, EXTERNAL :: IS_R4_NAN, IS_R8_NAN
      INTEGER*8, EXTERNAL :: LSEEK, READ
      INTEGER*4, EXTERNAL :: IXMN8, IXMN4, LINDEX, ILEN, I_LEN
      INTEGER*4, EXTERNAL :: LTM_DIF, FILE_INFO, IFIND_PL, FINDEX
      REAL*4,    EXTERNAL :: PHAS_CMPL_R4
      CHARACTER, EXTERNAL :: GET_CDATE*19
      INTEGER*4  MS, NS, IARR_TPI(ANC__MEPC), IARR_PCAL(ANC__MEPC)
      INTEGER*4  NP_SCA(ANC__MEPC), IN_SCAN(ANC__MEPC), IFND
      INTEGER*4  MP_SCA(ANC__MEPC)
      INTEGER*4  NTSYS_OBS, NPCAL_OBS, NTPI_OBS
      INTEGER*4  IT_WR, IP_WR
      CHARACTER  SCA_NAM*10, SOU_NAM*8, DATA_SCOPE*14, SPEC_DATE*23
      CHARACTER  SOU_ARR(ANC__MEPC)*8, SCA_ARR(ANC__MEPC)*10
      CHARACTER, ALLOCATABLE :: SOU_SCA(:)*(8), SCA_NAM_AVR(:)*(10)
      CHARACTER  CUR_DATE*20
      REAL*8     TSINCE
      CHARACTER  CBNC*8, CBTS*8, CANC*8, CSCAV*8
      CHARACTER  SCAV_STA*8, SCAV_EXP*16
      LOGICAL*1  FL_CHK
      INTEGER*4  IT_CNT, IP_CNT, IND_PCS
      REAL*8     RPHAS, RAMP
      COMPLEX*8  CPCAL
      INTEGER*4  TPION(ANC__MEPC), TPIOFF(ANC__MEPC)
      REAL*8,    DIMENSION(:,:), ALLOCATABLE :: TIM_TPI_AVR
      INTEGER*4, DIMENSION(:,:), ALLOCATABLE :: TPION_RMS, TPION_AVR
      INTEGER*4, DIMENSION(:,:), ALLOCATABLE :: TPIOFF_RMS, TPIOFF_AVR
!     
!==========================================================================
!
! --- How many scans are there?
!
      IUER = -1
      IVAR_FLG = 1 !  Tsys
      CALL ATP_SCANS ( ANC, DT_MAX, IVAR_FLG, MS, IARR, MP_SCA, IUER )
!     
! --- Allocate the scan averaging variables
!
      ALLOCATE ( ITPS_SCA     ( ANC%NUM_TPS, MS ), STAT = IER )
      ALLOCATE ( TIM_TSYS_AVR ( ANC%NUM_TPS, MS ), STAT = IER )
      ALLOCATE ( TSYS_AVR     ( ANC%NUM_TPS, MS ), STAT = IER )
      ALLOCATE ( TSYS_RMS     ( ANC%NUM_TPS, MS ), STAT = IER )
      ALLOCATE ( AZ_SCA       ( ANC%NUM_TPS, MS ), STAT = IER )
      ALLOCATE ( EL_SCA       ( ANC%NUM_TPS, MS ), STAT = IER )
      ALLOCATE ( SOU_SCA      ( MS              ), STAT = IER )
      ALLOCATE ( SCA_NAM_AVR  ( MS              ), STAT = IER )
!
! --- Go through ANC and compute the tsys averages for each scan at a
!     given frequency
!
      ICNT_MAX = 0
      ICNT     = 0
      ITPS_SCA = 0
      IT_WR    = 0
! ---
      DO 310 J1 = 1, ANC%NUM_TPS
!
! ------ Accept only Tsys elements with defined frequencies
!
         DELTS = ABS( ANC%TPS(J1)%SKY_FRQ - ANC%FILLER_R8 ) 
         IF ( DELTS .LT. DEL_MIN  ) GOTO 310
! ------
         ICNT = ICNT + 1
! ------
         IF ( ICNT_MAX < ICNT) ICNT_MAX = ICNT
!
! ------ Prefill the arrays
!
         TIM    = 0.D0
         TSYS   = 0.D0
         EL_ARR = 0.D0
         AZ_ARR = 0.D0
         IND_SCA   = 0
         NP     = 0
         NPSCA  = 0
!
! ------ Get the raw Tsys data (filtered for outliers) at the index == J1
!
         IUER = -1
         CALL TSYS_TIME_FILTER_RAW ( ANC, J1, NP, TIM, TSYS, EL_ARR,    &
     &                               AZ_ARR, SOU_ARR, SCA_ARR, IUER )
!     
! ------ Get the scan averages and RMS for Tsys (at the given scan
!        differences)
!        The averages are also filtered out for outliers.
!
         IUER = -1
         CALL TSYS_TIME_FILTER_SCAN (ANC, J1, DT_MAX, NP, MP_SCA, MS,   &
     &                               NS, TIM, TSYS, T1_AVR, Y1_AVR,     &
     &                               Y1_RMS, IND_SCA, IARR, NPSCA, IUER)
!     
! ------ Proceed only if there are actual points to use
!
         IF ( NP > 0 ) THEN 
!
! --------- For this frequency, assign scan results
!
            DO 320 J2 = 1, MS
!     
! ------------ Did this frequency participate in that scan
!
               IF ( NPSCA(J2) == 0 ) THEN
                  TSYS_AVR(ICNT,J2)   = ANC%FILLER_R8
                  TSYS_RMS(ICNT,J2)   = ANC%FILLER_R8
               ELSE
!
! --------------- Is the average Tsys of this scan above minimum Tsys
!
                  IF ( Y1_AVR(J2) > ANC__TSYS_MIN .AND.                  &
     &                 Y1_AVR(J2) < ANC__TSYS_MAX       ) THEN
!
! ------------------ Scan azimuth
!
                     AZ_SCA(ICNT,J2)   = AZ_ARR(IND_SCA(J2)) !/DEG__TO__RAD
!
! ------------------ Scan Elevations (> ANC__EL_MIN)
!
                     EL_SCA(ICNT,J2)   = EL_ARR(IND_SCA(J2))   !/DEG__TO__RAD
!
! ------------------ Source 
!
                     SOU_SCA(J2)     = SOU_ARR(IND_SCA(J2)) 
!
! ------------------ Scan name
!
                     SCA_NAM_AVR(J2) = SCA_ARR(IND_SCA(J2)) 
!
! ------------------ average time, tsys, and RMS for this scan at 
!                    the given frequency
!     
                     TIM_TSYS_AVR(ICNT,J2) = T1_AVR(J2)
                     TSYS_AVR(ICNT,J2)     = Y1_AVR(J2)
                     TSYS_RMS(ICNT,J2)     = Y1_RMS(J2)
!     
! ------------------ What is the index within ANC%TPS for the frequency 
!                    we are looking at?
!     
                     ITPS_SCA(ICNT,J2) = J1
! ------------------ How many points have we observed thus far
                     IT_WR = IT_WR + 1
!     
! --------------- Anything out of range we put as empty
!
                  ELSE
                     TSYS_AVR(ICNT,J2)     = ANC%FILLER_R8
                     TSYS_RMS(ICNT,J2)     = ANC%FILLER_R8
                  END IF
               END IF
 320        CONTINUE
         END IF
 310  CONTINUE
!
! --- Clear the ANC%TSYS to make room for the updated values
!
      ANC%NUM_TSYS    = 0
      ANC%NUM_TTO     = 0
      ANC%NUM_EPO_TTO = 0
      IF ( ASSOCIATED ( ANC%TTO ) )  DEALLOCATE ( ANC%TTO )
!     
! --- Reallocate based on the scan sizes!

      ALLOCATE ( ANC%TTO(MS), STAT=IER )
      IF ( IER .NE. 0 ) THEN
         CALL ERR_LOG ( 4100, IUER, 'ANC_SCAV',                         &
     &           'Error in allocating memory for ANC%TTO while '//      &
     &           'processing antenna calibration file '//ANC%FILIN )
         RETURN
      END IF
!
! --- Allocate NUM_TPS in each time stamp's TSYS
!
      DO 350 J1=1, MS !ANC%NUM_EPOTTO
         ALLOCATE ( ANC%TTO(J1)%TSYS(ANC%NUM_TPS), STAT=IER )
         IF ( IER .NE. 0 ) THEN
            CALL ERR_LOG ( 4110, IUER, 'ANC_SCAV',                      &
     &              'Error in allocating memory for the '//             &
     &              'ANC%TTO(j1)%TSYS while processing ' //             &
     &              'antenna calibration file '//TRIM(ANC%FILIN) )
            RETURN
         END IF
         ANC%TTO(J1)%TSYS = ANC%FILLER_R8
 350  CONTINUE
!
! ---
!
      NTSYS_OBS = 0
      DO 330 K1 = 1, MS !ANC%NUM_EPOTTO
         K2CNT = 0
         DO 340 K2 = 1, ICNT_MAX
!     
! --------- Parse only if the element exists
!     
            IF ( ITPS_SCA(K2,K1) .NE. 0 ) THEN
! ------------
               K2CNT = K2CNT + K2               
! ------------ 
               NTSYS_OBS = NTSYS_OBS + 1
! ------------
               IF ( K2CNT == K2 ) THEN
                  ANC%TTO(K1)%TIM      =  TIM_TSYS_AVR(K2,K1)
                  ANC%TTO(K1)%AZ       =  AZ_SCA(K2,K1)
                  ANC%TTO(K1)%EL       =  EL_SCA(K2,K1)
                  ANC%TTO(K1)%SOU_NAM  =  SOU_SCA(K1)
                  ANC%TTO(K1)%IND_SCA  =  K1
                  CALL INCH ( K1, STR2 )
                  IF ( ANC%EXP_TYPE == 'SDE' ) THEN
                     ANC%TTO(K1)%SCA_NAM  =  "tno0"//TRIM(STR2)
                  ELSE
                     ANC%TTO(K1)%SCA_NAM  = SCA_NAM_AVR(K1)
                  END IF
!
! --------------- Compute duration
!
                  IF ( K1 == 1 ) THEN
                     ANC%TTO(K1)%DUR = ANC%TTO(K1)%TIM
                  ELSE
                     ANC%TTO(K1)%DUR =  ANC%TTO(K1)%TIM    -            &
     &                                  ANC%TTO(K1-1)%TIM
                  END IF
               END IF
               ANC%TTO(K1)%TSYS(ITPS_SCA(K2,K1)) = TSYS_AVR(K2,K1)
            END IF
 340     CONTINUE
 330  CONTINUE
!
! --- Allocate based on FMT
!
      IF ( ANC%ANTCAL_FMT == ANTCAL__FMT_1 ) THEN
         ANC%NUM_TSYS = MS
      ELSEIF ( ANC%ANTCAL_FMT == ANTCAL__FMT_2 ) THEN
         ANC%NUM_EPO_TTO = MS
         ANC%NUM_TTO     = NTSYS_OBS
         ANC%NUM_TSYS    = NTSYS_OBS
      ELSE
         CALL ERR_LOG ( 4120, IUER, 'ANC_SCAV',             &
     &           'Unsupported format of ANTCAL '//ANC%ANTCAL_FMT  )
      END IF
!
      DEALLOCATE ( ITPS_SCA     )
      DEALLOCATE ( TIM_TSYS_AVR )
      DEALLOCATE ( TSYS_AVR     )
      DEALLOCATE ( TSYS_RMS     )
      DEALLOCATE ( AZ_SCA       )
      DEALLOCATE ( EL_SCA       )
      DEALLOCATE ( SOU_SCA      )
      DEALLOCATE ( SCA_NAM_AVR  )
      T1_AVR = 0.D0
!==========================================================================
!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
!
! --- Deal with PCal
! --- How many scans are there?
!
      IUER     = -1
      IVAR_FLG =  2     !  PCal
      CALL ATP_SCANS ( ANC, DT_MAX, IVAR_FLG, MS, IARR_PCAL,            &
     &                 MP_SCA, IUER )
!
! --- Allocate the scan averaging variables
!
      ALLOCATE ( IPCS_SCA     ( ANC%NUM_PCS, MS), STAT = IER )
      ALLOCATE ( TIM_PCAL_AVR ( ANC%NUM_PCS, MS), STAT = IER )
      ALLOCATE ( PCAL_AMP_AVR ( ANC%NUM_PCS, MS), STAT = IER )
      ALLOCATE ( PCAL_PHA_AVR ( ANC%NUM_PCS, MS), STAT = IER )
      ALLOCATE ( PCAL_AMP_RMS ( ANC%NUM_PCS, MS), STAT = IER )
      ALLOCATE ( PCAL_PHA_RMS ( ANC%NUM_PCS, MS), STAT = IER )
      ALLOCATE ( SOU_SCA      ( MS             ), STAT = IER )
      ALLOCATE ( SCA_NAM_AVR  ( MS             ), STAT = IER )
!     
! --- Go through ANC and compute the PCal averages for each scan at a
!     given frequency
!
      IPCAL_MAX = 0
      IPCAL     = 0
      IPCS_SCA  = 0
      IP_WR     = 0
! ---
      DO 410 J1 = 1, ANC%NUM_PCS
!
! ------ Accept only PCal elements with defined frequencies
!
         DELTS = ABS( ANC%PCS(J1)%SKY_FRQ - ANC%FILLER_R8 ) 
         IF ( DELTS .LT. DEL_MIN  ) GOTO 410
! ------
         IPCAL = IPCAL + 1
! ------
         IF ( IPCAL_MAX < IPCAL) IPCAL_MAX = IPCAL
!
! ------ Prefill the arrays
!
         TIM      = 0.D0
         PCAL     = CMPLX(0.D0)
         IND_SCA  = 0
         NP       = 0
         NPSCA    = 0
!
! ------ Get the raw Phase cal data (Filtered for NaN values) at the index == J1
!
         IUER = -1
         CALL PCAL_TIME_FILTER_RAW ( ANC, J1, NP, TIM, PCAL, SOU_ARR,   &
     &                               SCA_ARR, IUER )
!     
! ------ Get the PCAL scan averages and RMS for Amplitude and
!        Phase (at the given scan differences).
! ------ The averages are also filtered out for outliers.
!         T1_AVR = TIM_AVR,
!         Z_AVR = PCAL_AVR {CMPLX},
!         Y1_RMS = AMP_RMS,
!         Y2_RMS = PHA_RMS
!
         IUER = -1
         CALL PCAL_TIME_FILTER_SCAN ( ANC, J1, DT_MAX, NP, MP_SCA, MS,  &
     &                                NS, TIM, PCAL, T1_AVR, Z_AVR,     &
     &                                Y1_RMS, Y2_RMS, IND_SCA,          &
     &                                IARR_PCAL, NPSCA, IUER )
!     
! ------ Proceed only if there are actual points to use
!
         IF ( NP > 0 ) THEN 
!
! --------- For this frequency, assign scan results
!
            DO 420 J2 = 1, MS
!
! ------------ Did this frequency participate in that scan
!
               IF ( NPSCA(J2) == 0 ) THEN
                  PCAL_PHA_AVR(IPCAL,J2)  =  ANC%FILLER_R8
                  PCAL_PHA_RMS(IPCAL,J2)  =  ANC%FILLER_R8
                  PCAL_AMP_AVR(IPCAL,J2)  =  ANC%FILLER_R8
                  PCAL_AMP_RMS(IPCAL,J2)  =  ANC%FILLER_R8
               ELSE
!
! --------------- Is the average Pcal of this scan above minimum Amplitude
!
                  IF ( ( ABS(Z_AVR(J2)) > ANC__AMP_MIN ) .AND.          &
     &                 ( ABS(Z_AVR(J2)) < ANC__AMP_MAX )       ) THEN
!
! ------------------ Average time, phase, amplitude, and their respective
!                    rms's for this scan at the given frequency
!                    Average Phase is resolved to [-pi,pi]
!     
                     TIM_PCAL_AVR(IPCAL,J2) = T1_AVR(J2)
                     PCAL_AMP_AVR(IPCAL,J2) = ABS(Z_AVR(J2))
                     PCAL_PHA_AVR(IPCAL,J2) = PHAS_CMPL_R4(Z_AVR(J2))
                     PCAL_AMP_RMS(IPCAL,J2) = Y1_RMS(J2)
                     PCAL_PHA_RMS(IPCAL,J2) = Y2_RMS(J2)
!
! ------------------ What is the index within ANC%PCS for the frequency 
!                    we are looking at?
!
                     IPCS_SCA(IPCAL,J2) = J1
!
! ------------------ Source 
!
                     SOU_SCA(J2)     = SOU_ARR(IND_SCA(J2)) 
!
! ------------------ Scan name
!
                     SCA_NAM_AVR(J2) = SCA_ARR(IND_SCA(J2))
!
! ------------------ How many points have we observed thus far
!
                     IP_WR = IP_WR + 1
!     
! --------------- Anything out of range we put as empty
!
                  ELSE
                     PCAL_PHA_AVR(IPCAL,J2)  =  ANC%FILLER_R8
                     PCAL_PHA_RMS(IPCAL,J2)  =  ANC%FILLER_R8
                     PCAL_AMP_AVR(IPCAL,J2)  =  ANC%FILLER_R8
                     PCAL_AMP_RMS(IPCAL,J2)  =  ANC%FILLER_R8
                  END IF
               END IF
 420        CONTINUE
         END IF
 410  CONTINUE
!
! --- Clear the ANC%PCAL to make room for the updated values
!
      ANC%NUM_PCAL     = 0
      ANC%NUM_EPO_PCAL = 0
      IF ( ASSOCIATED ( ANC%PCAL ) )  DEALLOCATE ( ANC%PCAL )
!     
! --- Reallocate based on the scan sizes
!
      ALLOCATE ( ANC%PCAL(MS), STAT=IER )
      IF ( IER .NE. 0 ) THEN
         CALL ERR_LOG ( 4130, IUER, 'ANC_SCAV',                         &
     &           'Error in allocating memory for ANC%PCAL while '//      &
     &           'processing antenna calibration file '//ANC%FILIN )
         RETURN
      END IF
! ---
      DO 450 J2 = 1, MS
         ALLOCATE ( ANC%PCAL(J2)%PCAL_CMPL(ANC%NUM_PCS), STAT=IER )
         IF ( IER .NE. 0 ) THEN
            CALL ERR_LOG ( 4140, IUER, 'ANC_PARSE',                     &
     &              'Error in allocating memory for '//                 &
     &              'ANC%PCAL(j2)%PCAL_CMPL while processing '//        &
     &              'antenna calibration file '//ANC%FILIN)
            RETURN
         END IF
         ANC%PCAL(J2)%PCAL_CMPL = CMPLX(ANC__FILLER_R4, 0.0)
 450  CONTINUE
!
! --- Write these results to the PCAL BUFFER
!
      NPCAL_OBS = 0
      DO 430 K1 = 1, MS
         K2CNT = 0
         DO 440 K2 = 1, IPCAL_MAX
!     
! --------- Write only if the element exists
! --------- For now we assume that the elements for source and data scope
!           are empty
!     
            IF ( IPCS_SCA(K2,K1) .NE. 0 ) THEN
! ------------
               K2CNT = K2CNT + K2               
! ------------
               NPCAL_OBS = NPCAL_OBS + 1
! ------------ 
!
! ------------ Convert the average time to calendar date
! ------------ Get seconds since J2000 and (MJD_PCAL, TAI_PCAL)
!
! ------------ The IF statement is to ensure all the observations in
!              a scan have the same time stamp.
!
               IF ( K2CNT == K2 ) THEN

                  ANC%PCAL(K1)%TIM      =  TIM_PCAL_AVR(K2,K1) 
                  ANC%PCAL(K1)%IND_SCA  =  K1
                  ANC%PCAL(K1)%SOU_NAM  =  SOU_SCA(K1)
                  CALL INCH ( K1, STR2 )

                  IF ( ANC%EXP_TYPE == 'SDE' ) THEN
                     ANC%PCAL(K1)%SCA_NAM  =  "pno0"//TRIM(STR2)
                  ELSE
                     ANC%PCAL(K1)%SCA_NAM  = SCA_NAM_AVR(K1)
                  END IF
!     
! --------------- Compute Duration
!
                  IF ( K1 == 1 ) THEN
                     ANC%PCAL(K1)%DUR =  ANC%PCAL(K1)%TIM
                  ELSE
                     ANC%PCAL(K1)%DUR =  ANC%PCAL(K1)%TIM    -          &
     &                                   ANC%PCAL(K1-1)%TIM
                  END IF
               END IF
!
! ------------ Convert the range of PHAS from [-pi,pi] to [0,2pi]
!     
               PCAL_PHA_AVR(K2,K1) = PCAL_PHA_AVR(K2,K1) + 2*PI__NUM
               IF ( PCAL_PHA_AVR(K2,K1) > 2*PI__NUM )  THEN
                  PCAL_PHA_AVR(K2,K1) = PCAL_PHA_AVR(K2,K1) - 2*PI__NUM
               END IF
!
! ------------ Rescale Amplitude
!
               PCAL_AMP_AVR(K2,K1) = PCAL_AMP_AVR(K2,K1)/ANC__AMP_SCA               
!     
! ------------ Convert PHA and AMP to CMPLX values
!
               ANC%PCAL(K1)%PCAL_CMPL(IPCS_SCA(K2,K1)) =                &
     &                      CMPLX ( PCAL_AMP_AVR(K2,K1)*                &
     &                                  DCOS(PCAL_PHA_AVR(K2,K1)),      &
     &                              PCAL_AMP_AVR(K2,K1)*                &
     &                                  DSIN(PCAL_PHA_AVR(K2,K1)) )
            END IF
 440     CONTINUE
 430  CONTINUE
      IF ( ANC%ANTCAL_FMT == ANTCAL__FMT_1 ) THEN
         ANC%NUM_PCAL = MS
      ELSEIF ( ANC%ANTCAL_FMT == ANTCAL__FMT_2 ) THEN
         ANC%NUM_EPO_PCAL = MS
         ANC%NUM_PCAL     = NPCAL_OBS
      ELSE
         CALL ERR_LOG ( 4150, IUER, 'ANC_SCAV',             &
     &           'Unsupported format of ANTCAL '//ANC%ANTCAL_FMT  )
      END IF
! ---
      DEALLOCATE ( IPCS_SCA     )
      DEALLOCATE ( TIM_PCAL_AVR )
      DEALLOCATE ( PCAL_AMP_AVR )
      DEALLOCATE ( PCAL_PHA_AVR )
      DEALLOCATE ( PCAL_AMP_RMS )
      DEALLOCATE ( PCAL_PHA_RMS )
      DEALLOCATE ( SOU_SCA      )
      DEALLOCATE ( SCA_NAM_AVR  )
      T1_AVR = 0.D0
!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
!>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
!
! --- How many scans are there?
!
      IUER     = -1
      IVAR_FLG = 3              !  TPI
      CALL ATP_SCANS (ANC, DT_MAX, IVAR_FLG, MS, IARR_TPI, MP_SCA, IUER)
!
! --- Allocate the scan averaging variables
!
      ALLOCATE ( ITPS_SCA     ( ANC%NUM_TPS, MS ), STAT = IER )
      ALLOCATE ( TIM_TPI_AVR  ( ANC%NUM_TPS, MS ), STAT = IER )
      ALLOCATE ( TPION_AVR    ( ANC%NUM_TPS, MS ), STAT = IER )
      ALLOCATE ( TPION_RMS    ( ANC%NUM_TPS, MS ), STAT = IER )
      ALLOCATE ( TPIOFF_AVR   ( ANC%NUM_TPS, MS ), STAT = IER )
      ALLOCATE ( TPIOFF_RMS   ( ANC%NUM_TPS, MS ), STAT = IER )
      ALLOCATE ( AZ_SCA       ( ANC%NUM_TPS, MS ), STAT = IER )
      ALLOCATE ( EL_SCA       ( ANC%NUM_TPS, MS ), STAT = IER )
      ALLOCATE ( SOU_SCA      ( MS              ), STAT = IER )
      ALLOCATE ( SCA_NAM_AVR  ( MS              ), STAT = IER )
!
! --- Go through ANC and compute the tpi averages for each scan at a
!     given frequency
!
      ICNT_MAX = 0
      ICNT     = 0
      ITPS_SCA = 0
      IT_WR    = 0
! ---
      DO 510 J1 = 1, ANC%NUM_TPS
!
! ------ Accept only Tpi elements with defined frequencies
!
         DELTS = ABS( ANC%TPS(J1)%SKY_FRQ - ANC%FILLER_R8 ) 
         IF ( DELTS .LT. DEL_MIN  ) GOTO 310
! ------
         ICNT = ICNT + 1
! ------
         IF ( ICNT_MAX < ICNT) ICNT_MAX = ICNT
!
! ------ Prefill the arrays
!
         TIM     =  0.D0
         TPION   =  0
         TPIOFF  =  0
         EL_ARR  =  0.D0
         AZ_ARR  =  0.D0
         IND_SCA =  0
         NP      =  0
         NPSCA   =  0
!
! ------ Get the raw Tpi data (filtered for outliers) at the index == J1
!
         IUER = -1
         CALL TPI_TIME_FILTER_RAW ( ANC, J1, NP, TIM, TPION, TPIOFF,    &
     &                              EL_ARR, AZ_ARR, SOU_ARR, SCA_ARR,   &
     &                              IUER )
!     
! ------ Get the scan averages and RMS for Tpi (at the given scan
!        differences)
!        The averages are also filtered out for outliers.
!
         IUER = -1
         CALL TPI_TIME_FILTER_SCAN ( ANC, J1, DT_MAX, NP, MP_SCA, MS,   &
     &                               NS, TIM, TPION, TPIOFF, T1_AVR,    &
     &                               IY1_AVR, IY1_RMS, IY0_AVR,         &
     &                               IY0_RMS, IND_SCA, IARR_TPI,        &
     &                               NPSCA, IUER )
!
! ------ Proceed only if there are actual points to use
!
         IF ( NP > 0 ) THEN 
!
! --------- For this frequency, assign scan results
!
            DO 520 J2 = 1, MS
!
! ------------ Did this frequency participate in that scan
!
               IF ( NPSCA(J2) == 0 ) THEN
                  TPION_AVR(ICNT,J2)   = ANC%FILLER_I4
                  TPION_RMS(ICNT,J2)   = ANC%FILLER_I4
                  TPIOFF_AVR(ICNT,J2)  = ANC%FILLER_I4
                  TPIOFF_RMS(ICNT,J2)  = ANC%FILLER_I4
               ELSE
!
! --------------- Is the average TPION of this scan above -999
!
                  IF ( IY1_AVR(J2) > ANC%FILLER_I4  ) THEN
!
! ------------------ Scan azimuth
!
                     AZ_SCA(ICNT,J2)  = AZ_ARR(IND_SCA(J2))   !/DEG__TO__RAD
!
! ------------------ Scan Elevations (> ANC__EL_MIN)
!
                     EL_SCA(ICNT,J2)  = EL_ARR(IND_SCA(J2))   !/DEG__TO__RAD
!
! ------------------ Source 
!
                     SOU_SCA(J2)      = SOU_ARR(IND_SCA(J2))
!
! ------------------ Scan name
!
                     SCA_NAM_AVR(J2)  = SCA_ARR(IND_SCA(J2)) 
!
! ------------------ average time, tpi, and RMS for this scan at 
!                    the given frequency
!     
                     TIM_TPI_AVR(ICNT,J2)  = T1_AVR(J2)
                     TPION_AVR(ICNT,J2)    = IY1_AVR(J2)
                     TPION_RMS(ICNT,J2)    = IY1_RMS(J2)
                     TPIOFF_AVR(ICNT,J2)   = IY0_AVR(J2)
                     TPIOFF_RMS(ICNT,J2)   = IY0_RMS(J2)
!     
! ------------------ What is the index within ANC%TPS for the frequency 
!                    we are looking at?
!     
                     ITPS_SCA(ICNT,J2) = J1
! ------------------ How many points have we observed thus far
                     IT_WR = IT_WR + 1
!     
! --------------- Anything out of range we put as empty
!
                  ELSE
                     TPION_AVR(ICNT,J2)   = ANC%FILLER_I4
                     TPION_RMS(ICNT,J2)   = ANC%FILLER_I4
                     TPIOFF_AVR(ICNT,J2)  = ANC%FILLER_I4
                     TPIOFF_RMS(ICNT,J2)  = ANC%FILLER_I4
                  END IF
               END IF
 520        CONTINUE
         END IF
 510  CONTINUE
!
! --- Clear the ANC%TPI to make room for the updated values
!
      ANC%NUM_TPI     = 0
      ANC%NUM_EPO_TPI = 0
      IF ( ASSOCIATED ( ANC%TPI ) )  DEALLOCATE ( ANC%TPI )
!     
! --- Reallocate based on the scan sizes
!
      ALLOCATE ( ANC%TPI(MS), STAT=IER )
      IF ( IER .NE. 0 ) THEN
         CALL ERR_LOG ( 4160, IUER, 'ANC_SCAV',                         &
     &           'Error in allocating memory for ANC%TPI while '//      &
     &           'processing antenna calibration file '//ANC%FILIN )
         RETURN
      END IF
!
! --- Allocate NUM_TPS in each time stamp's TPI
!
      DO 550 J1=1, MS
         ALLOCATE ( ANC%TPI(J1)%TPION(ANC%NUM_TPS),   STAT=IER )
         ALLOCATE ( ANC%TPI(J1)%TPIOFF(ANC%NUM_TPS),  STAT=IER )
         ALLOCATE ( ANC%TPI(J1)%TPIZERO(ANC%NUM_TPS), STAT=IER )
         IF ( IER .NE. 0 ) THEN
            CALL ERR_LOG ( 4170, IUER, 'ANC_SCAV',                      &
     &              'Error in allocating memory for the '//             &
     &              'ANC%TPI(j1)%TPION, TPIOFF, and TPIZERO while ' //  &
     &              'processing antenna calibration file '//ANC%FILIN )
            RETURN
         END IF
         ANC%TPI(J1)%TPION   = ANC%FILLER_I4
         ANC%TPI(J1)%TPIOFF  = ANC%FILLER_I4
         ANC%TPI(J1)%TPIZERO = ANC%FILLER_I4
 550  CONTINUE
!
! ---
!
      NTPI_OBS = 0
      DO 530 K1 = 1, MS
         K2CNT = 0
         DO 540 K2 = 1, ICNT_MAX
!
! --------- Parse only if the element exists
!
            IF ( ITPS_SCA(K2,K1) .NE. 0 ) THEN
! ------------
               K2CNT = K2CNT + K2               
! ------------ 
               NTPI_OBS = NTPI_OBS + 1
! ------------
               IF ( K2CNT == K2 ) THEN
                  ANC%TPI(K1)%TIM      =  TIM_TPI_AVR(K2,K1)
                  ANC%TPI(K1)%AZ       =  AZ_SCA(K2,K1)
                  ANC%TPI(K1)%EL       =  EL_SCA(K2,K1)
                  ANC%TPI(K1)%SOU_NAM  =  SOU_SCA(K1)
                  ANC%TPI(K1)%IND_SCA  =  K1
                  CALL INCH ( K1, STR2 )

                  IF ( ANC%EXP_TYPE == 'SDE' ) THEN
                     ANC%TPI(K1)%SCA_NAM  =  "tno0"//TRIM(STR2)
                  ELSE
                     ANC%TTO(K1)%SCA_NAM  = SCA_NAM_AVR(K1)
                  END IF
!     
! --------------- Compute duration
!
                  IF ( K1 == 1 ) THEN
                     ANC%TPI(K1)%DUR = ANC%TPI(K1)%TIM
                  ELSE
                     ANC%TPI(K1)%DUR =  ANC%TPI(K1)%TIM    -            &
     &                                  ANC%TPI(K1-1)%TIM
                  END IF
               END IF
               ANC%TPI(K1)%TPION(ITPS_SCA(K2,K1))  = TPION_AVR(K2,K1)
               ANC%TPI(K1)%TPIOFF(ITPS_SCA(K2,K1)) = TPIOFF_AVR(K2,K1)
            END IF
 540     CONTINUE
 530  CONTINUE
!
! --- Allocate based on FMT
!
      IF ( ANC%ANTCAL_FMT == ANTCAL__FMT_1 ) THEN
         ANC%NUM_TPI = MS
      ELSEIF ( ANC%ANTCAL_FMT == ANTCAL__FMT_2 ) THEN
         ANC%NUM_EPO_TPI = MS
         ANC%NUM_TPI     = NTPI_OBS
      ELSE
         CALL ERR_LOG ( 4180, IUER, 'ANC_SCAV',             &
     &           'Unsupported format of ANTCAL '//ANC%ANTCAL_FMT  )
      END IF
!
      DEALLOCATE ( ITPS_SCA     )
      DEALLOCATE ( TIM_TPI_AVR )
      DEALLOCATE ( TPION_AVR     )
      DEALLOCATE ( TPION_RMS     )
      DEALLOCATE ( TPIOFF_AVR     )
      DEALLOCATE ( TPIOFF_RMS     )
      DEALLOCATE ( AZ_SCA       )
      DEALLOCATE ( EL_SCA       )
      DEALLOCATE ( SOU_SCA      )
      DEALLOCATE ( SCA_NAM_AVR  )
!<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
      CALL ERR_LOG ( 0, IUER )
      RETURN
! ---      
      END SUBROUTINE
