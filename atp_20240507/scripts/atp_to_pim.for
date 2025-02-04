      SUBROUTINE ATP_TO_PIM ( IFLAG, ANC, PIM, IUER )
!
! ***************************************************************************
! *                                                                         *
! *   Routine ATP_TO_PIM                                                    *
! *                                                                         *
! *   Copy anc derived typees  to pima for use in gepm.                     *
! *                                                                         *
! *   INPUT:                                                                *
! *                                                                         *
! *       IFLAG      =  Initialisation Flag               { INT*4 }  *
! *                                                                         *
! *       ANC        =  ANC structure.                    { DERIVED TYPE }  *
! *                                                                         *
! *       IUER       =  Error Handler                     { INT*4, OPT }    *
! *                     If IUER=0 no error message will be printed, even    *
! *                     in the event of an error. However, for other        *
! *                     possible values, i.e. IUER=-1,-2, & -3, the error   *
! *                     message will print to screen. For the latter        *
! *                     case, i.e., IUER = -3, after printing the program   *
! *                     will terminate.                                     *
! *                     Default, IUER = -1                                  *
! *                                                                         *
! *   OUTPUT:                                                               *
! *        PIM    =  PIMA structure                       { DERIVED TYPE }  *
! *                                                                         *
! *  ### 21-JUL-2023  PIMA_GEPM      v1.0 (c)  N. HABANA   21-JUL-2023 ###  *
! *                                                                         *
! ***************************************************************************
!
      IMPLICIT 
      IMPLICIT   NONE
      INCLUDE   'atp.i'
      INCLUDE   'pima.i'
      TYPE ( ANC__TYP   ) :: ANC
      TYPE ( PIMA__TYPE  ) :: PIM
!
! --- No. of phase cal tones???
!
      
      PIM%NPCT
!
! --- No. of IF's 
!
      PIM%NFRQ
!
! --- 
!
       PIM%CONF%PHAS_CAL_CODE      
!
! --- Assumption is all tones are good. So, no PCAL Mask Files
!
      PIM%PCAL_MASK  = 1
!
! --- Configuration polarizations
!
!!    Translate the ATP polarizations L,R,H,V,X,Y to
!!    PIMA's dual polarizations: LL, LR, RL, RR, HH, HV, etc
!
!
!
       PIM%NSTA = 1 ! There's one station per log file
!@@@!       PIM%C_STA(1) =  
!
! --- For this station fill in the phase cal information
!
       ISTA = LTM_DIF ( 0, PIM%NSTA, PIM%C_STA, STA_NAM )
                NPOI =  PIM%STA(IND_STA)%PCAL(1)%NPOI 

      PIM%STA(IND_STA)%PCAL(1)%PCAL_AVAIL
      PIM%STA(IND_STA)%PCAL(1)%PCAL_USE
            TIM = PIM%STA(IND_STA)%PCAL(1)%TIME_MID_R8(1:NPOI)
        PIM%STA(IND_STA)%PCAL(1)%NO_TONES
            
!     
              CALL PIMA_PCAL_AVG ( PIM, IND_STA, PIMA__AMP_MIN,           &
     &                           NUM_ACCUM, NPOI_ACCUM, FREQS,          &
     &                           IND_TONE_FREQ, T8, P8, A8, Y8, IER )
               CALL PIMA_PCAL_CLEAN8 ( PIM, IND_STA, NUM_ACCUM,         &
     &                                 NPOI_ACCUM, T8, P8, A8, IER )
               CALL PIMA_PCAL_BPASS ( PIM, IND_STA, J1, IFRQ,           &
     &                                PIMA__AMP_MIN, NUM_ACCUM,         &
     &                                NPOI_ACCUM, FREQS, T8, P8, A8,    &
     &                                BPASS_PHASE, IER )










      


      RETURN
      END SUBROUTINE
!
! ------------------------------------------------------------------------
!
      SUBROUTINE PIMA_GEPM ( PIM, STA_NAM, T_ACCUM, GEPM_OVR,           &
     &                       TIME_THRESH, DIFF_THRESH, MAX_COUNT, IUER )
!
! ***************************************************************************
! *                                                                         *
! *   Routine PIMA_GEPM                                                     *
! *                                                                         *
! *   Automatically generatee phase calibration mask.                       *
! *                                                                         *
! *   INPUT:                                                                *
! *       PIM        =  PIMA structure.                    { DERIVED TYPE}  *
! *                                                                         *
! *       STA_NAM    = Station Name                        { CHAR }         *
! *                    Acepts station names (IVS convention) or "ALL" for   *
! *                    consideration of all stations in PIM                 *
! *                                                                         *
! *       T_ACCUM     = Target time for averaging          { REAL*8 }       *
!                       e.g. pcal may be sampled at 1 sample/second and we  *
!                       want to average the pcal data such that it is       *
!                       sampled at  1 sample per T_ACCUM seconds. The data  *
!                       is commonly irregularly sampled, though, so the     *
!                       sample rate is tuned via averaging such that T_ACCUM*
!                       is as close to the average time between samples as  *
!                       possible.                                           *
! *                                                                         *
! *       GEPM_OVR    = Overwrite request                  { LOGICAL*4 }    *
! *                     Should GEPM overwrite the existing Phase            *
! *                     calibration mask file?                              *
! *                                                                         *
! *       TIME_THRESH = threshold fraction for flagging    { REAL*8 }       *
!                       is the threshold fraction of epochs at which a tone *
!                       is flagged as spurious before it is deactivated,    *
!                       e.g. for TIME_THRESH = 0.1, if the tone is flagged  *
!                       as spurious in 10% of all epochs, it is deactivated *
! *                                                                         *
! *       DIFF_THRESH =                                    { REAL*8 }       *
!                       is the threshold distance on the complex plane      *
!                       between the spline and data curve that results      *
!                       in the tone being flagged at the epoch.             *
! *                                                                         *
! *       MAX_COUNT   =  Maximum number of ???             { INT*4 }        *
!              ????-Confirm with Joe-?????
! *        IUER       =  Error Handler                     { INT*4, OPT }   *
! *                      If IUER=0 no error message will be printed, even   *
! *                      in the event of an error. However, for other       *
! *                      possible values, i.e. IUER=-1,-2, & -3, the error  *
! *                      message will print to screen. For the latter       *
! *                      case, i.e., IUER = -3, after printing the program  *
! *                      will terminate.                                    *
! *                      Default, IUER = -1                                 *
! *                                                                         *
! *   OUTPUT:        ???-????                                               *
! *        PIM    =  Updated PIM structure                 { DERIVED TYPE } *
! *        exp_band_pcal_rms.txt - A file indicating the health of the      *
! *                                phase calibration tones in the form of   *
! *                                the root-mean-square phase jitter in the *
! *                                time domain and the frequency domain.    *    
! *                                                                         *
! *        exp_band_pcal_report.gen - This report file shows the specific   *
! *                                   tones that have been masked out as    *
! *                                   well as the reason for their masking  *
! *                                   outreport file                        *
! *  ### 21-JUN-2022  PIMA_GEPM      v1.0 (c)  J. SKEENS   21-JUN-2022 ###  *
! *                                                                         *
! ***************************************************************************
!
      IMPLICIT   NONE
      INCLUDE   'astro_constants.i'
      INCLUDE   'pima.i'
      INCLUDE   'diagi.i'
      TYPE     ( PIMA__TYPE  ) :: PIM
      CHARACTER  STA_NAM*(*)
      LOGICAL*4  GEPM_OVR, TWO_TONE_LOGIC
      INTEGER*4  IUER, IER
      INTEGER*4  MPB, MP, NN, MDEG, N_KNOT, SIG_NUM
      INTEGER*4  FAIL_AMP, FAIL_CPDIFF, FAIL_Y8DIFF, NUM_SUM, MAX_COUNT
      REAL*8     EPS, T_ACCUM, SIGMA_DETX, SIGMA_DETY, VAR_THRESH
      REAL*8     T_SHIFT, TIME_THRESH, DIFF_THRESH, BPASS_THRESH
      PARAMETER  ( MPB    =   7 )
      PARAMETER  ( MP     =  128*1024 )
      PARAMETER  ( MDEG   =   3 )
      PARAMETER  ( EPS = 1.D0 )
      PARAMETER  ( N_KNOT = 2 )
      PARAMETER  ( NUM_SUM = 3 ) 
      PARAMETER  ( SIG_NUM = 10 )
      PARAMETER  ( FAIL_AMP = 1 )
      PARAMETER  ( FAIL_CPDIFF = 2 )
      PARAMETER  ( FAIL_Y8DIFF = 3 )
      PARAMETER  ( VAR_THRESH = 5.D-2 )
      PARAMETER  ( SIGMA_DETX = 23.929070929070928 )
      PARAMETER  ( SIGMA_DETY = 19.511488511488512 )
      PARAMETER  ( T_SHIFT = 600 )
      PARAMETER  ( BPASS_THRESH = 5 )
      PARAMETER  ( TWO_TONE_LOGIC = .TRUE. )
      REAL*4     G_DEL
      REAL*8,    ALLOCATABLE :: T8(:,:,:), P8(:,:,:)
      REAL*8,    ALLOCATABLE :: A8(:,:,:), Y8(:,:,:)
      REAL*8,    ALLOCATABLE :: FREQ(:), FREQ_SDEL(:,:), AMPL_SDEL(:,:)
      REAL*8,    ALLOCATABLE :: PHAS_SDEL(:,:), TIM(:), PHAS(:,:)
      REAL*8,    ALLOCATABLE :: PHAS_AMB(:,:), AMPL(:,:), T_NONZERO(:)
      REAL*8,    ALLOCATABLE :: TONE_SORT(:), DIFF_Y8(:)
      REAL*8     FRQ_DIF, FREQS(PIM__MTON,PIM__MFRQ), P8_SPL(PIM__MTON)
      REAL*8     P8_SPL_COEFF(1-MDEG:PIM__MTON), TIME_AVG
      REAL*8     P8_SPL_RMS, KNOT_LOC(N_KNOT), CP_DIFF(PIM__MTON)
      REAL*8     A8_SPL_COEFF(1-MDEG:PIM__MTON), MEAN_MASK, AMPMAX
      REAL*8     A8_SPL_RMS, A8_SPL(PIM__MTON), FREQ_ABOVE(PIM__MTON)
      REAL*8     PHASE_ABOVE(PIM__MTON), AMP_ABOVE(PIM__MTON)
      REAL*8     BPASS_PHASE(PIM__MTON), BPASS_SORTED(PIM__MTON-1)
      REAL*8     DIFF_PHASE(PIM__MTON), AVE_PHASE, AVE_AMP, TIM_DIFF
      REAL*8     DIFF_AMP(PIM__MTON), FREQ_1ST(PIM__MFRQ), T_ACCUM_AVG
      REAL*8     FRQ_DIF_MIN, PCAL_FRQ_STEP, PHAS_LAST, SIG, T8_DIFF
      REAL*8     AMP_MED_TONE, DIFF_Y8_THRESHX, DIFF_Y8_THRESHY, SIG_Y8
      REAL*8     BPASS_DIFF(PIM__MTON), BPASS_MEDIAN
      CHARACTER  PREF_NAME*128, MES_SELSTA*80, STA_STR*8
      CHARACTER  STR*32, STR1*32
      INTEGER*4  MODE, IND_STA, ISTA_LAST, ICODE, IFRQ, NC, NR, NFRQ
      INTEGER*4   NP, IND_POL, IND_PLT, COUNT_DIFF
      INTEGER*4  I1, I2, I3, I4, I5
      INTEGER*4  J1, J2, J3, J4, J5, J6, J7, J8, J9
      INTEGER*4  K1, K2, K3, K4, K5, K6, K7, K8, K9
      INTEGER*4  L1, L2, L3
      INTEGER*4  M1, M2, M3, M4
      INTEGER*4  M_TONES, IND_TONE, IND_FREQ, N_TONES, LIND, IND(2,32)
      INTEGER*4  I_ABOVE, I_NONZERO, I_TONE, NPT, BEG_STA, END_STA
      INTEGER*4  PREFIX, CP_MAXLOC, RECURSIVE, NO_TON, NPCL, IND_MOD
      INTEGER*4  ITYP, KCHN, NT_USED, KP, IND_TONE_FREQ(PIM__MTON)
      INTEGER*4  NUM_NONZERO, NPOI_ACCUM, NUM_ACCUM, N_AVG,NPOI, N_SHIFT
      LOGICAL*4  INDS_ABOVE(PIM__MTON), LEX
      LOGICAL*4, ALLOCATABLE :: INDS_NONZERO(:)
      INTEGER*4, ALLOCATABLE :: DIFF_MASK(:,:,:), FAIL_ARR(:,:,:)
      INTEGER*4, ALLOCATABLE :: IND_FRQ(:), IND_TON(:)
      INTEGER*4, EXTERNAL :: ILEN, I_LEN, LTM_DIF
      REAL*4,    EXTERNAL :: PIMA_PC_GDEL
      REAL*8,    EXTERNAL :: EBSPL_VAL_R8
      COMPLEX*16 CP_DATA(PIM__MTON), CP_SPL(PIM__MTON)
      INTEGER*8  INT8_HLD 
!
! --- Allocate the T8 array (NPCT x MP x NFRQ) || (1 x MP x NFRQ)
!
      ALLOCATE ( T8(MAX(1,PIM%NPCT), MP, PIM%NFRQ), STAT=IER )
      IF ( IER .NE. 0 ) THEN
         CALL CLRCH  ( STR )
         INT8_HLD = INT8(MP)*INT8(PIM%NPCT)*INT8(PIM%NFRQ)*INT8(8)
         CALL IINCH8 ( INT8_HLD, STR )
         CALL ERR_LOG ( 8821, IUER, 'PIMA_GEPM',                        &
     &           'Failure to allocate '//TRIM(STR)//                    &
     &           ' bytes of dynamic memory for array T8' )
         RETURN
      END IF
!
! --- Allocate the P8 array (NPCT x MP x NFRQ) || (1 x MP x NFRQ)
!
      ALLOCATE ( P8(MAX(1,PIM%NPCT), MP, PIM%NFRQ), STAT=IER )
      IF ( IER .NE. 0 ) THEN
         CALL CLRCH ( STR )
         CALL IINCH ( MP*PIM%NPCT*PIM%NFRQ*8, STR )
         CALL ERR_LOG ( 8822, IUER, 'PIMA_GEPM',                        &
     &           'Failure to allocate '//STR(1:I_LEN(STR))//            &
     &           ' bytes of dynamic memory for array P8' )
         RETURN
      END IF
!
! --- Allocate the A8 array (NPCT x MP x NFRQ) || (1 x MP x NFRQ)
!
      ALLOCATE ( A8(MAX(1,PIM%NPCT), MP, PIM%NFRQ), STAT=IER )
      IF ( IER .NE. 0 ) THEN
         CALL CLRCH ( STR )
         CALL IINCH ( MP*PIM%NPCT*PIM%NFRQ*8, STR )
         CALL ERR_LOG ( 8823, IUER, 'PIMA_GEPM',                        &
     &           'Failure to allocate '//STR(1:I_LEN(STR))//            &
     &           ' bytes of dynamic memory for array A8' )
         RETURN
      END IF
!
! --- Allocate the Y8 array (NPCT x MP x NFRQ) || (1 x MP x NFRQ)
!
      ALLOCATE ( Y8(MAX(1,PIM%NPCT), MP, PIM%NFRQ), STAT=IER )
      IF ( IER .NE. 0 ) THEN
         CALL CLRCH ( STR )
         CALL IINCH ( MP*PIM%NPCT*PIM%NFRQ*8, STR )
         CALL ERR_LOG ( 8824, IUER, 'PIMA_GEPM',                        &
     &           'Failure to allocate '//STR(1:I_LEN(STR))//            &
     &           ' bytes of dynamic memory for array Y8' )
           RETURN
      END IF
!
! --- Allocate the DIFF_MASK array (NPCT x MP x NFRQ) || (1 x MP x NFRQ)
!
      ALLOCATE ( DIFF_MASK(MAX(1,PIM%NPCT), MP, PIM%NFRQ), STAT=IER )
      IF ( IER .NE. 0 ) THEN
         CALL CLRCH ( STR )
         CALL IINCH ( MP*PIM%NPCT*PIM%NFRQ*4, STR )
         CALL ERR_LOG ( 8825, IUER, 'PIMA_GEPM',                        &
     &           'Failure to allocate '//STR(1:I_LEN(STR))//            &
     &           ' bytes of dynamic memory for array DIFF_MASK' )
         RETURN
      END IF
!
! --- Allocate the FAIL_ARR array (PIM__MTON x NFRQ x NSTA)
!
      ALLOCATE ( FAIL_ARR(PIM__MTON,PIM%NFRQ,PIM%NSTA), STAT=IER )
      IF ( IER .NE. 0 ) THEN
         CALL CLRCH ( STR )
         CALL IINCH ( MP * PIM%NFRQ * PIM%NSTA * 4, STR )
         CALL ERR_LOG ( 8826, IUER, 'PIMA_GEPM',                        &
     &           'Failure to allocate '//STR(1:I_LEN(STR))//            &
     &           ' bytes of dynamic memory for array FAIL_ARR' )
         RETURN
      END IF
!
! --- Set the IND_TONE
!
      CALL CHIN ( PIM%CONF%PHAS_CAL_CODE, IND_TONE ) ! Set IND_TONE
!
! --- Read/allocate pcal mask if desired
!
      IF ( .NOT. ASSOCIATED ( PIM%PCAL_MASK )  ) THEN
         ALLOCATE (PIM%PCAL_MASK(PIM%NPCT,P
