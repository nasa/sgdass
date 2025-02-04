#include <mk5_preprocessor_directives.inc>
      SUBROUTINE PIMA_SPLT ( PIM, VTD, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  PIMA_SPLT  splits the UV data in FITS-IDI format into     *
! *   a set of smaller files in FITS format, one file per source.        *
! *   It applyes parameters of fringe fitting and averages over          *
! *   the specified number of spectral channels and accumulation         *
! *   periods.                                                           *
! *                                                                      *
! *  ### 27-JAN-2011   PIMA_SPLT  v4.13  (c) L. Petrov  26-NOV-2020  ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'astro_constants.i'
      INCLUDE   'pima.i'
      INCLUDE   'vtd.i'
      INTEGER*4  IUER
      TYPE     ( PIMA__TYPE     ) :: PIM
      TYPE     ( VTD__TYPE      ) :: VTD
      TYPE     ( VTD__OBS_TYPE  ) :: OBS_TYP
      REAL*8     SNR, SNR_TOT
      CHARACTER  STR*128, STR1*128, STR2*128, STR_UV_PRINT*32, STRC*4096, STRA*4096
      CHARACTER  FITSDIR*128, FIL_TOT*128, PIMA_FRI_USED_VERS*24, POL_CONF*7, POL_LAB*2
      REAL*4     PHAS_R4, AMPL_R4, WEI_MAX, AMP_BPASS
      REAL*8     TIM_EPC, TIM_EPC_WEI
      REAL*4,    ALLOCATABLE :: WEI_FRQ_CHN(:,:)
      TYPE     ( UVO__TYPE ), POINTER :: UVO(:)
      INTEGER*4, ALLOCATABLE :: IND_SCA_SEG(:)
      REAL*8,    ALLOCATABLE :: TIM_SEG(:,:), TIM_ARR(:)
      COMPLEX*8  BPASS_C8, PBP, SUMVIS_WEISEG(PIM__MPLR,PIM__MCHN*PIM__MFRQ)
      COMPLEX*8  DRF_ALL, DRF_IF(PIM__MCHN*PIM__MFRQ,PIM__MPLR)
      LOGICAL*1  FL_NODATA, FL_BPASS, FL_PBP, FL_FIND_VALID_AP, FL_NOPHASE, &
     &           FL_WEI_OBS, FL_NOFRI, FL_TIMER, FL_AMPL_0001, &
     &           FL_SKIP, FL_NOI_LOSS_MISSED, FL_USE_OBS_ORIG(PIM__MOBS), &
     &           FL_AC_USE, FL_SPLT_SNR_FRI_ONLY
      ADDRESS__TYPE :: DIR_DESC, IP
      INTEGER*2  MODE_MKDIR
      DATA       MODE_MKDIR / O'00755' /
      INTEGER*1  MASK_I1
      INTEGER*4  J1, J2, J3, J4, J5, J6, J7, J8, J9, J10, J11, J12, J13, J14, J15, &
     &           J16, J17, J18, J19, J20, J21, J22, J23, J24, J25, J26, J27, &
     &           J28, J29, J30, J31, J32, J33, J34, J35, J36, J37, J38, J39, J40, &
     &           J41, J42, &
     &           IS, M_SEG, FRG_IND, IND_BND, IND_STA(2), IND_OBS, L_OBS, &
     &           OBS_LIS(PIM__MOBS), N_SEG, K_SEG, KFRQ, LFRQ, LTIM, ISEG_TIM, &
     &           KSEG_LAST, IND_AP_USED(2), NCHN_SEG, NOBS_SOU, KOBS_SOU, &
     &           LOBS_PROC, L_UVO, K_UVO, K_UVO_LAST, BPS_SGN, IND_BPS_BAS, IFRQ, &
     &           IND_EPC_BEG, IND_EPC_END, IND_FRQ(4,PIM__MCHN*PIM__MFRQ), &
     &           ICHN, L_SEG,  IND_SEG_LAST, IND_AP_LAST, IND_TIM, NSEG_MAX, &
     &           NUMP_SEG(4,PIM__MCHN*PIM__MFRQ), LUN_TOT, &
     &           UV_IND, IND_FRA, L_UV, L_UVP, LPOL, LA_POL, &
     &           POLA_INDS(2), POLC_INDS(4), NUMP_ALL, &
     &           NUMP_BAND(4), NOBS_IN_USED, NOBS_USED, NSCA_USED, SWAP_I4, &
     &           IND_USED_ACC, NUM_SEG_WEI, IAC_SEG, KACC_UVO_BEG, &
     &           KACC_UVO_END, L_WEI_SEG, POL_MODE, FRI_STS, INDS_OBS_USED(PIM__MOBS), &
     &           LU_OBS, NTHR, K_GAI, K_SEFD, U_UVO, NUM_USED_AP(PIM__MFRQ,PIM__MPLR), IER
      REAL*4     SUM_WEI_SEG(4,PIM__MCHN*PIM__MFRQ), SUM_WEI_BAND(PIM__MPLR), &
     &           SUMVIS_WEISQ_SEG(4,PIM__MCHN*PIM__MFRQ), WEI_SEG(4,PIM__MCHN*PIM__MFRQ),  &
     &           SUM_WEI_ALL, SIG_SQ, AMPL, PHAS, FEED_ANG_DIF, PBP_PHS, PBP_AMP, &
     &           WEI_IF(PIM__MCHN*PIM__MFRQ,PIM__MPLR), &
     &           WW_IF(PIM__MCHN*PIM__MFRQ,PIM__MPLR), WEI_ALL, WW_ALL
      REAL*4,    ALLOCATABLE :: ACCVIS_WEI_SEG(:,:), ACCVIS_WEISQ_SEG(:,:), &
     &                          ACC_WEI_SEG(:,:),    ACC_WEI(:,:,:), ACC_AMPL(:,:)
      REAL*8,    ALLOCATABLE :: RENRML_CROS_IF(:,:), RENRML_AUTO_IF(:,:,:), SEFD_IF(:,:)
      INTEGER*4, ALLOCATABLE :: IND_ACC_UVO(:), ACC_NAP(:,:)
      INTEGER*4   PIMA__APSO_LTIM_LIM  
      REAL*8      PIMA__PHS_APSO_LIM, PHS_APSO_LIM
!      PARAMETER ( PIMA__PHS_APSO_LIM  = 0.1D0 )
      PARAMETER ( PIMA__PHS_APSO_LIM  = 1.D-9 )
      PARAMETER ( PIMA__APSO_LTIM_LIM = 5 )
      INTEGER*8  ISS
      INTEGER*4  PIM__MSCS, NAP__SIG
      REAL*8     EPS__TIM, SNR_SHR_WARN
      PARAMETER  ( PIM__MSCS = 16 ) ! Maximal number of scans of the same source
      PARAMETER  ( NAP__SIG  =  3 ) ! Minimal number of Aps in segment for using rms of fringe amplitude for weight calculation
      PARAMETER  ( EPS__TIM = 2.D-3 )
      PARAMETER  ( SNR_SHR_WARN = 1.20D0 ) ! Thersold ofthe differenfeces between SRN  frib and splt to raise the warning
      REAL*8     FRQ_ARR(PIM__MCHN*PIM__MFRQ), FRQ_AVR, LAMBDA, &
     &           TIM_AP, AP_LEN_SCA, THE_GR_DEL, THE_RATE, &
     &           DER_DEL(VTD__NDER), DER_RAT(VTD__NDER), TIM_SCA_BEG, &
     &           TIM_SCA_END, SUM_WEI_1D, NRML_BEAM_ATT(2), GAIN_CORR, &
     &           TIM_EFF_SEG, TIM_EFF_BAND, BDW_EFF_BAND(PIM__MOBS), &
     &           NOI, SEFD_TOTAL, GAIN_TOTAL, DECOR_BS, DECOR_TS, &
     &           NOI_SMEARING, NOI_BEAM, NRML_BEAM_ATT_LOW, SRT_WRT_FRT_OFFS, VAR1
      REAL*8     OLD_GR_DEL(PIM__MOBS), OLD_PH_RAT(PIM__MOBS), &
     &           OLD_GR_RAT(PIM__MOBS), OLD_PH_ACC(PIM__MOBS), DURA_ACC
      REAL*8     T1(PIM__MOBS), X1(PIM__MOBS), X2(PIM__MOBS), X3(PIM__MOBS)
      REAL*8     PH_DEL_APSO, PH_RAT_APSO, PH_ACC_APSO, PHS_APSO_MAX, &
     &           DEL_APSO, RAT_APSO, ACC_APSO, DEL_QUAD_MAX, &
     &           TIM_FRT, FRI_COEF, TIM_TOL, RENRML_CROS_IF_ALL(PIM__MPLR), &
     &           RENRML_AUTO_IF_ALL(PIM__MPLR)
!
      ADDRESS__TYPE, EXTERNAL :: FUNC_OPENDIR, CLOSEDIR
      REAL*8,    EXTERNAL :: PIMA_AUTC_AMPL_NRML, PIMA_AUTC_AMPL_NRML_PRE20170303, &
     &                       PIMA_BEAM_ATT
      INTEGER*4, EXTERNAL :: ADD_LIS, GET_PROC_INFO, GET_UNIT, ILEN, I_LEN, LTM_DIF, MKDIR
      LOGICAL*4, EXTERNAL :: IS_R4_NAN, IS_R8_NAN, OMP_IN_PARALLEL
      REAL*4,    EXTERNAL :: ATAN_CS_R4, PHAS_CMPL_R4
      CHARACTER, EXTERNAL :: MJDSEC_TO_DATE*30, GET_CDATE*19, GET_CDATE_MS*23, &
     &                       PIMA_GET_POL_CONF*7
#ifdef GNU
      INTEGER*4, EXTERNAL :: PIMA_COMPAR_UVO
#else
      INTEGER*2, EXTERNAL :: PIMA_COMPAR_UVO
#endif
!
!  Mean_w  = sum( wei_i * x_i )/sum(wei_i)
!  rms_w   = sum( wei_i * ( x_i  - Mean_w)**2 ) / sum(wei_i) = sum( wei_i * x_i**2 )/sum(wei_i) - Mean_w**2
!
      IF ( PIM%CONF%DEBUG_LEVEL .GE. 4 ) THEN
           WRITE ( 6, * ) 'PIMA_SPLT Start'
      END IF
      NTHR = PIM%CONF%NUM_THREADS
      CALL OMP_SET_NUM_THREADS ( %VAL(NTHR) )
      IF ( OMP_IN_PARALLEL() ) THEN
           NTHR = 1
      END IF
!
! --- Process kludge variables
!
      FL_NOPHASE   = .FALSE.
      FL_AMPL_0001 = .FALSE.
      FL_TIMER     = .FALSE.
!
      CALL GETENVAR ( 'PIMAVAR_SPLT_NOPHASE', STR )
      CALL TRAN ( 11, STR, STR )
      IF ( STR == 'YES' ) FL_NOPHASE = .TRUE.
!
      CALL GETENVAR ( 'PIMAVAR_SPLT_AMPL_0001', STR )
      CALL TRAN ( 11, STR, STR )
      IF ( STR == 'YES' ) FL_AMPL_0001 = .TRUE.
!
      CALL GETENVAR ( 'PIMAVAR_PHS_APSO_LIM', STR )
      IF ( ILEN(STR) > 0 ) THEN
           READ ( UNIT=STR, FMT='(F12.5)', IOSTAT=IER ) PHS_APSO_LIM
           IF ( IER .NE. 0 ) PHS_APSO_LIM = PIMA__PHS_APSO_LIM
         ELSE
           PHS_APSO_LIM = PIMA__PHS_APSO_LIM
      END IF
      CALL GETENVAR ( 'PIMAVAR_SPLT_SNR_FRI_ONLY', STR )
      IF ( STR == 'yes' .OR. STR == 'YES' ) THEN
           FL_SPLT_SNR_FRI_ONLY = .TRUE.
         ELSE
           FL_SPLT_SNR_FRI_ONLY = .FALSE.
      END IF
!
      CALL GETENVAR ( 'PIMAVAR_SPLT_TIMER', STR )
      CALL TRAN ( 11, STR, STR )
      IF ( STR == 'YES' ) FL_TIMER = .TRUE.
      IF ( FL_TIMER ) THEN
           CALL WALL_TIMER ( %VAL(0) ) 
      END IF
      CALL GETENVAR ( 'PIMAVAR_UV_PRINT', STR_UV_PRINT )
      CALL GETENVAR ( 'PIMAVAR_FITS_DIR', STR )
      IF ( ILEN(STR) == 0 ) THEN
           FITSDIR =  PIM%CONF%EXPER_DIR(1:I_LEN(PIM%CONF%EXPER_DIR))// &
     &                '/'//PIM%CONF%SESS_CODE(1:I_LEN(PIM%CONF%SESS_CODE))// &
     &                '_uvs'
         ELSE 
           FITSDIR = STR
      END IF
!
      FL_NOI_LOSS_MISSED = .FALSE.
      CALL GETENVAR ( 'PIMAVAR_NOI_LOSS_MISSED', STR )
      IF ( STR == 'YES' .OR. STR == 'yes' ) THEN
           FL_NOI_LOSS_MISSED = .TRUE.
      END IF
!
! --- Check whether the FITSDIR exists
!
      DIR_DESC = FUNC_OPENDIR ( FITSDIR(1:I_LEN(FITSDIR))//CHAR(0) )
      IF ( DIR_DESC == 0 ) THEN
!
! -------- Does not exist? Let us create it
!
           IS = MKDIR ( FITSDIR(1:I_LEN(FITSDIR))//CHAR(0), &
     &                  %VAL(MODE_MKDIR) )
           IF ( IS .NE. 0 ) THEN
                CALL GERROR ( STR )
                CALL ERR_LOG ( 8411, IUER, 'PIMA_SPLT', 'Failure '// &
     &              'in attempt to create the directory for output '// &
     &              'uv data '//FITSDIR(1:I_LEN(FITSDIR))//' -- '// &
     &               STR )
                RETURN 
           END IF
         ELSE 
           IP = CLOSEDIR ( %VAL(DIR_DESC) )
      END IF
!
! --- Set up for VTD
!
      OBS_TYP%PLRZ           = 'RR'
      OBS_TYP%FRQ_REF(1)     = PIM%REF_FREQ
      OBS_TYP%N_BND          = 1
      OBS_TYP%DELAY_TYPE     = VTD__ML__DTP
      OBS_TYP%FRQ_ION_EFF(1) = PIM%REF_FREQ
      OBS_TYP%STATUS         = VTD__BND
      IND_BND                = 1 ! So far, only the first band
!
      IF ( PIM%NSTK == 1 ) THEN
           LPOL = 1
           POLC_INDS(1) = 1
         ELSE IF ( PIM%NSTK == 2 ) THEN
           IF ( PIM%CONF%POLAR == PIMA__POLAR_I  ) THEN
                LPOL = 2
                POLC_INDS(1) = 1
                POLC_INDS(2) = 2
              ELSE IF ( PIM%CONF%POLAR == PIMA__POLAR_RR ) THEN
                LPOL = 1
                POLC_INDS(1) = 1
              ELSE IF ( PIM%CONF%POLAR == PIMA__POLAR_LL ) THEN
                LPOL = 1
                POLC_INDS(2) = 2
              ELSE
                CALL ERR_LOG ( 8412, IUER, 'PIMA_SPLT', 'PIMA SPLT task '// &
     &              'does not support polarization '//TRIM(PIM%CONF%POLAR)// &
     &              'when only two stokes parameters have been correlated' )
                RETURN 
           END IF
         ELSE IF ( PIM%NSTK == 4 ) THEN
           IF ( PIM%CONF%POLAR == PIMA__POLAR_I ) THEN
                LPOL  = PIM%NSTK
                POLC_INDS(1) = 1
                POLC_INDS(2) = 2
                POLC_INDS(3) = 3
                POLC_INDS(4) = 4
              ELSE IF ( PIM%CONF%POLAR == PIMA__POLAR_RR ) THEN
                LPOL = 1
                POLC_INDS(1) = 1
              ELSE IF ( PIM%CONF%POLAR == PIMA__POLAR_LL ) THEN
                LPOL = 1
                POLC_INDS(1) = 2
              ELSE IF ( PIM%CONF%POLAR == PIMA__POLAR_RL ) THEN
                LPOL = 1
                POLC_INDS(1) = 3
              ELSE IF ( PIM%CONF%POLAR == PIMA__POLAR_LR ) THEN
                LPOL = 1
                POLC_INDS(1) = 4
              ELSE IF ( PIM%CONF%POLAR == PIMA__POLAR_HH ) THEN
                LPOL = 1
                POLC_INDS(1) = 1
              ELSE IF ( PIM%CONF%POLAR == PIMA__POLAR_HV ) THEN
                LPOL = 1
                POLC_INDS(1) = 2
              ELSE IF ( PIM%CONF%POLAR == PIMA__POLAR_VH ) THEN
                LPOL = 1
                POLC_INDS(1) = 3
              ELSE IF ( PIM%CONF%POLAR == PIMA__POLAR_VV ) THEN
                LPOL = 1
                POLC_INDS(1) = 4
              ELSE
                CALL ERR_LOG ( 8413, IUER, 'PIMA_SPLT', 'PIMA SPLT task '// &
     &              'does not support polarization '//TRIM(PIM%CONF%POLAR) )
                RETURN 
           END IF
      END IF
!
      IF ( PIM%CONF%BANDPASS_USE .NE. PIMA__BPASS_NO  .AND. &
     &     ASSOCIATED ( PIM%BPASS )                         ) THEN
           FL_BPASS = .TRUE.
         ELSE
           FL_BPASS = .FALSE.
      END IF
      IF ( PIM%CONF%POLARCAL_FILE .NE. PIMA__POLARCAL_NO .AND. &
     &     ASSOCIATED ( PIM%PBP )                         ) THEN
           FL_PBP = .TRUE.
         ELSE
           FL_PBP = .FALSE.
      END IF
!
      IF ( ILEN(PIM%CONF%SPLT_GAIN_CORR_FILE) == 0 ) THEN
           CALL ERR_PASS ( IUER, IER )
           CALL PIMA_GACO_INIT ( PIM, 1.0D0, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 8414, IUER, 'PIMA_SPLT', 'Failure in an attempt '// &
     &              'to initialize GACO object' ) 
                RETURN 
           END IF
         ELSE 
           CALL ERR_PASS ( IUER, IER )
           CALL PIMA_READ_GACO ( PIM, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 8415, IUER, 'PIMA_SPLT', 'Failure in an attempt '// &
     &              'to read gain correction file' )
                RETURN 
           END IF
      END IF
!
! --- Get fringe results and put them in appropriate slots of PIM object
!
      CALL ERR_PASS ( IUER, IER )
      CALL PIMA_FRI_READ ( PIM, IND_BND, PIMA_FRI_USED_VERS, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 8416, IUER, 'PIMA_SPLT', 'Error in an '// &
     &         'attempt to read results of fringing from the fringe file '// &
     &          PIM%CONF%FRINGE_FILE )
           RETURN
      END IF
!
! --- Compute theoretical path delay for all observations (and UV coordinates as well)
!
      CALL ERR_PASS ( IUER, IER )
      CALL PIMA_THEO ( PIM, VTD, 'OBS_SRT', '1ST_STA', IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 8417, IUER, 'PIMA_SPLT', 'Error in an attempt '// &
     &         'to compute theoretical path delays' )
           RETURN
      END IF
!
      IF ( PIM%CONF%FRIB_OBS_STATUS  == PIMA__OBS_NO ) THEN
           CALL ERR_LOG ( 0, IUER )
           RETURN
         ELSE IF ( PIM%CONF%FRIB_OBS_STATUS  == PIMA__OBS_ALL .AND. &
     &             PIM%CONF%FRIB_NOBS        == -1                  ) THEN
!
! -------- Create and populate array of used observations
!
           PIM%CONF%FRIB_NOBS = PIM%NOBS
           ALLOCATE ( PIM%CONF%FRIB_OBS(PIM%CONF%FRIB_NOBS), STAT=IER )
           IF ( IER .NE. 0 ) THEN
                CALL CLRCH ( STR )
                CALL IINCH ( 4*PIM%CONF%FRIB_NOBS, STR )
                CALL ERR_LOG ( 8419, IUER, 'PIMA_SPLT', &
     &              'Failure to allocate '//STR(1:I_LEN(STR))//' bytes '//&
     &              'of dynamic memory for the list of observations' )
                RETURN
           END IF
!
! -------- As a default, use all observations
!
           DO 410 J1=1,PIM%CONF%FRIB_NOBS
              PIM%CONF%FRIB_OBS(J1) = J1
 410       CONTINUE
      END IF
      OLD_GR_DEL = 0.0D0
      OLD_PH_RAT = 0.0D0
      OLD_GR_RAT = 0.0D0
      OLD_PH_ACC = 0.0D0
!
! --- Create the frequency table:
! --- 1) array FRQ_ARR with the average frequency
! --- 2) array IND_FRQ array with indieces:
! ---          IND_FRQ(1,*) -- index of the first spectral channel
! ---          IND_FRQ(2,*) -- index of the last  spectral channel
! ---          IND_FRQ(3,*) -- index of the IF
! ---          IND_FRQ(4,*) -- index of the IF relative fo BEG_FRQ
! --- 3) KFRQ -- the number of output frequencues
!
      LFRQ = PIM%CONF%END_FRQ - PIM%CONF%BEG_FRQ + 1
      FRQ_AVR = 0.0D0
      KFRQ = 1
      ICHN = 0
      IND_FRQ(1,1) = 0
      DO 420 J2=PIM%CONF%BEG_FRQ,PIM%CONF%END_FRQ
         DO 430 J3=1,PIM%NCHN
            ICHN = ICHN + 1
            IF ( ICHN == 1 ) THEN
                 IND_FRQ(1,KFRQ) = J3
                 IND_FRQ(3,KFRQ) = J2
                 IND_FRQ(4,KFRQ) = J2 - PIM%CONF%BEG_FRQ + 1
                 FRQ_ARR(KFRQ) = PIM%FREQ_ARR(J3,J2,PIM%CONF%FRQ_GRP)
            END IF
            IF ( ICHN == PIM%CONF%SPLT_FRQ_MSEG .OR. J3 == PIM%NCHN  ) THEN
                 IND_FRQ(2,KFRQ) = J3
                 IF ( .NOT. ( J3 == PIM%NCHN  .AND.  J2 == PIM%CONF%END_FRQ ) ) THEN
                      KFRQ = KFRQ + 1
                 END IF
                 ICHN = 0
            END IF
 430     CONTINUE
 420  CONTINUE
!
      ALLOCATE ( WEI_FRQ_CHN(PIM%NCHN,PIM%NFRQ), &
     &           RENRML_CROS_IF(KFRQ,LPOL), &
     &           RENRML_AUTO_IF(KFRQ,2,PIM%NSTK), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( 4*PIM%NCHN*PIM%NFRQ + 8*KFRQ*LPOL + 8*KFRQ*2*PIM%NSTK, STR )
           CALL ERR_LOG ( 8420, IUER, 'PIMA_SPLT', 'Failure to '// &
     &         'allocate '//STR(1:I_LEN(STR))//' bytes of dynamic '// &
     &         'memory for arrays WEI_FRQ_CHN, RENRML_CROS_IF, and RENRML_AUTO_IF' )
           RETURN
      END IF
      WEI_FRQ_CHN = 1.0D0
      RENRML_CROS_IF = 0.0
      RENRML_AUTO_IF = 0.0
      FL_USE_OBS_ORIG(1:PIM%NOBS) = PIM%USE_OBS(1:PIM%NOBS)
!
! --- NCHN_SEG -- the number of spectral channels in one segment
!
      NCHN_SEG = PIM%NCHN/PIM%CONF%SPLT_FRQ_MSEG
      IF ( NCHN_SEG*PIM%CONF%SPLT_FRQ_MSEG < PIM%NCHN ) THEN
           NCHN_SEG = NCHN_SEG + 1
      END IF
!
      OBS_LIS = 0
      IF ( FL_TIMER ) THEN
           CALL WALL_TIMER ( STR ) 
           WRITE ( 6, '(A)' ) 'PIMA_SPLT: Time_prep: '//STR(1:I_LEN(STR))
      END IF
      IF ( PIM%CONF%MKDB_FRINGE_ALGORITHM == PIMA__FRA_DRF ) THEN
           IND_FRA = PIMA__DRF
         ELSE IF ( PIM%CONF%MKDB_FRINGE_ALGORITHM == PIMA__FRA_LSQ ) THEN
           IND_FRA = PIMA__LSQ
         ELSE IF ( PIM%CONF%MKDB_FRINGE_ALGORITHM == PIMA__FRA_ACC ) THEN
           IND_FRA = PIMA__LSQ
         ELSE IF ( PIM%CONF%MKDB_FRINGE_ALGORITHM == PIMA__FRA_MUL ) THEN
           IND_FRA = PIMA__MUL
         ELSE IF ( PIM%CONF%MKDB_FRINGE_ALGORITHM == PIMA__FRA_ADD ) THEN
           IND_FRA = PIMA__ADD
      END IF
!
! --- Cycle over sources
!
      DO 440 J4=1,PIM%NSOU
!
! ------ Check the source name
!
         IF ( PIM%CONF%SPLT_SOU_NAME == '*'   .OR. &
     &        PIM%CONF%SPLT_SOU_NAME == 'ALL'      ) THEN
              CONTINUE
            ELSE IF ( PIM%CONF%SPLT_SOU_NAME == PIM%SOU(J4)%NAME ) THEN
              CONTINUE
            ELSE IF ( PIM%CONF%SPLT_SOU_NAME == PIM%SOU(J4)%J2000_NAME ) THEN
              CONTINUE
            ELSE
              GOTO 440
         END IF
         LU_OBS = 0
         INDS_OBS_USED = 0
!
         PIM%L_SUB = 0
         L_OBS     = 0
         ISEG_TIM  = 0
         IF ( PIM%CONF%DEBUG_LEVEL .GE. 1 ) THEN
              WRITE ( 6, 110 ) PIM%C_SOU(J4), J4, PIM%NSOU
 110          FORMAT ( ' PIMA_SPLT Processing source ', A, &
     &                 2X, I4, ' ( ', I4, ' ) ' )
              CALL FLUSH ( 6 )
         END IF
!
! ------ Transform group delay, fringe delay rate, group delay, 
! ------ and the total fringe from baseline-based quantities to station-based
! ------ quantities. This routine also splits the data into subarrays
!
         CALL ERR_PASS ( IUER, IER )
         CALL PIMA_SPLT_FITSTA ( PIM, VTD, J4, NSCA_USED, NOBS_IN_USED, &
     &                           NOBS_USED, OLD_GR_DEL, OLD_PH_RAT, &
     &                           OLD_GR_RAT, OLD_PH_ACC, IER )
         IF ( NOBS_USED == 0 ) THEN
              IF ( PIM%CONF%DEBUG_LEVEL .GE. 1 ) THEN
                   WRITE ( 6, 170 ) PIM%SOU(J4)%IVS_NAME, PIM%SOU(J4)%J2000_NAME
                   CALL FLUSH ( 6 )
              END IF
              GOTO 440
         END IF
!
         IF ( IER .NE. 0 ) THEN
              CALL ERR_PASS ( IUER, IER )
              CALL ERR_LOG ( 8421, IER, 'PIMA_SPLT', 'Failure in '// &
     &            'attempt to fix fringe fitting to make it station '// &
     &            ' based for source '//PIM%C_SOU(J4) )
              IF ( PIM%CONF%DEBUG_LEVEL .GE. 1 ) THEN
                   WRITE ( 6, 120 ) J4, PIM%NSOU, PIM%C_SOU(J4)
 120               FORMAT ( ' PIMA_SPLT ', I4, ' ( ', I4, ' ) Source ', A, &
     &                      ' cannot split the fit. SKIPPING.' )
                   CALL FLUSH ( 6 )
              END IF
              GOTO 440
         END IF
!
! ------ Allocate dynamic memory for temporary arrays
!
         L_UVP = (PIM%NSTA*(PIM%NSTA+1))/2 * &            ! The number of baselines
     &           PIM%CONF%MAX_SCAN_LEN/PIM%AP_LEN_MIN * & ! Maximum nuymber of APs in the scan
     &           PIM__MSCS                                ! Maxiumum number of scans
         ALLOCATE ( TIM_SEG(3,L_UVP), IND_SCA_SEG(L_UVP), STAT=IER )
         IF ( IER .NE. 0 ) THEN
              CALL CLRCH ( STR )
              CALL IINCH ( 8*3*L_UVP + 4*L_UVP, STR )
              CALL ERR_LOG ( 8422, IUER, 'PIMA_SPLT', 'Failure to '// &
     &            'allocate '//STR(1:I_LEN(STR))//' bytes of dynamic '// &
     &            'memory for arrays TIM_SEG and IND_SCA_SEG' )
              RETURN
         END IF
         TIM_SEG = 0.0D0
         IND_SCA_SEG = 0.0
!
! ------ Compute array of segments N_SEG/TIM_SEG that contains
! ------ start and stop moments of time for a given segement.
! ------ Segements starts at the nominal scan begnning time and 
! ------ last AP_legth*SPLT_TIM_MSEG.
! ------ IF the number of APs is not commensurate with the scan
! ------ length, the last segment will be longer.
!
         N_SEG = 0
         L_UVO = 0
         DO 450 J5=1,PIM%NSCA
            IF ( PIM%SCA(J5)%SOU_IND == J4 ) THEN
                 AP_LEN_SCA = PIM%OBS(PIM%SCA(J5)%OBS_IND(1))%AP_LEN
!
! -------------- K_SEG is the number of segments in the J5-th scan
!
                 DO 460 J6=1,PIM%SCA(J5)%NUM_EPC
 460             CONTINUE 
                 TIM_SCA_BEG = PIM%TIM_R8(PIM%SCA(J5)%TIM_IND)
                 TIM_SCA_END = PIM%TIM_R8(PIM%SCA(J5)%TIM_IND+PIM%SCA(J5)%NUM_EPC-1)
                 K_SEG = INT( (TIM_SCA_END - TIM_SCA_BEG)/ &
     &                        (AP_LEN_SCA*PIM%CONF%SPLT_TIM_MSEG) ) + 1
                 IF ( K_SEG < 1 ) K_SEG = 1
                 DO 470 J7=1,K_SEG
                    N_SEG = N_SEG + 1 ! Segement counter
                    IF ( N_SEG > L_UVP ) THEN
                         CALL CLRCH ( STR )
                         CALL IINCH ( L_UVP, STR )
                         CALL ERR_LOG ( 8423, IUER, 'PIMA_SPLT', 'Trap of internal '// &
     &                       'control: parameter L_UVP is too small. Please check '// &
     &                       'parameter PIM__MSC defined i PIMA_SPLT' )
                         RETURN
                    END IF
!
! ----------------- Put in TIM_SEG nominal start segment time, nominal stop segment time,
! ----------------- and reference time of the segment that is approximately the mean epoch
!
                    TIM_SEG(1,N_SEG) = PIM%TIM_R8(PIM%SCA(J5)%TIM_IND) + &
     &                                 (J7-1)*AP_LEN_SCA*PIM%CONF%SPLT_TIM_MSEG
                    TIM_SEG(2,N_SEG) = TIM_SEG(1,N_SEG) + AP_LEN_SCA*(PIM%CONF%SPLT_TIM_MSEG-1)
                    TIM_SEG(3,N_SEG) = (TIM_SEG(1,N_SEG) + TIM_SEG(2,N_SEG) + AP_LEN_SCA)/2.0D0
!
! ----------------- Round the segment reference time
!
                    IF ( PIM%AP_LEN_MIN*PIM%CONF%SPLT_TIM_MSEG > 1.0 ) THEN
                         TIM_SEG(3,N_SEG) = IDNINT ( TIM_SEG(3,N_SEG) + PIM%TAI_0 ) - PIM%TAI_0
                         TIM_TOL = 0.50001D0
                      ELSE IF ( PIM%AP_LEN_MIN*PIM%CONF%SPLT_TIM_MSEG > 0.5D0 ) THEN
                         TIM_SEG(3,N_SEG) = IDNINT ( (TIM_SEG(3,N_SEG) + PIM%TAI_0)*2.0D0 )/2.0D0 - PIM%TAI_0 
                         TIM_TOL = 0.25001D0
                      ELSE IF ( PIM%AP_LEN_MIN*PIM%CONF%SPLT_TIM_MSEG > 0.2D0 ) THEN
                         TIM_SEG(3,N_SEG) = IDNINT ( (TIM_SEG(3,N_SEG) + PIM%TAI_0)*5.0 )/5.0D0 - PIM%TAI_0 
                         TIM_TOL = 0.10001D0
                      ELSE IF ( PIM%AP_LEN_MIN*PIM%CONF%SPLT_TIM_MSEG > 0.1 ) THEN
                         TIM_SEG(3,N_SEG) = IDNINT ( (TIM_SEG(3,N_SEG) + PIM%TAI_0)*10.0 )/10.0D0 - PIM%TAI_0 
                         TIM_TOL = 0.05001D0
                      ELSE IF ( PIM%AP_LEN_MIN*PIM%CONF%SPLT_TIM_MSEG > 0.05 ) THEN
                         TIM_SEG(3,N_SEG) = IDNINT ( (TIM_SEG(3,N_SEG) + PIM%TAI_0)*20.0 )/20.0D0 - PIM%TAI_0 
                         TIM_TOL = 0.02501D0
                      ELSE
                         TIM_TOL = PIM%AP_LEN_MIN*0.50001D0
                    END IF
!
! ----------------- A special trick: for the first and last segment we would like to
! ----------------- align scan segment start and segment end with segment start and segment
! ----------------- end of all *used* observations
!
                    IF ( J7 ==     1 ) TIM_SEG(1,N_SEG) =  1.D9
                    IF ( J7 == K_SEG ) TIM_SEG(2,N_SEG) = -1.D9
!
                    IND_SCA_SEG(N_SEG) = J5
                    DO 480 J8=1,PIM%SCA(J5)%NBAS
                       IND_OBS = PIM%SCA(J5)%OBS_IND(J8)
                       SNR = PIM%OBS(IND_OBS)%AMPL(PIMA__DRF,IND_BND)/PIM%OBS(IND_OBS)%NOISE(IND_BND)
                       FRG_IND = PIM%OBS(IND_OBS)%REF_FRG_INDS(PIM%CONF%FRQ_GRP)
!
! -------------------- Bypass observation if at least one station did not have Tsys
!
                       IF ( .NOT. PIM%STA(PIM%OBS(IND_OBS)%STA_IND(1))%TSYS(PIM%CONF%FRQ_GRP)%AVAIL ) GOTO 680
                       IF ( .NOT. PIM%STA(PIM%OBS(IND_OBS)%STA_IND(2))%TSYS(PIM%CONF%FRQ_GRP)%AVAIL ) GOTO 680
                       IF ( .NOT. PIM%USE_OBS(IND_OBS) ) GOTO 680 ! Bypass the deselected observation
                       IF ( FRG_IND == 0 ) GOTO 680
!
                       IF ( PIM%CONF%TSYS_CAL_CODE == PIMA__TSYS_MEASURED ) THEN
!
! ------------------------- Case of measured system temperature
!
                            IF ( PIM%OBS(IND_OBS)%TSYS_IND(1,PIM%CONF%FRQ_GRP) .LE. 0 ) GOTO 680
                            IF ( PIM%OBS(IND_OBS)%TSYS_IND(2,PIM%CONF%FRQ_GRP) .LE. 0 ) GOTO 680
                         ELSE IF ( PIM%CONF%TSYS_CAL_CODE == PIMA__TSYS_CLEANED .OR. &
     &                             PIM%CONF%TSYS_CAL_CODE == PIMA__TSYS_MODELED      ) THEN
!
! ------------------------- Case of modeled system temperature
!
                            IF ( .NOT. PIM%STA(PIM%OBS(IND_OBS)%STA_IND(1))%STMO(PIM%CONF%FRQ_GRP)%TSYS_AVAIL ) THEN
                                 CALL ERR_LOG ( 8424, IUER, 'PIMA_SPLT', 'Trap of internal '// &
     &                               'control: modeled system temperature is not available '// &
     &                               'for station '//PIM%STA(PIM%OBS(IND_OBS)%STA_IND(1))%IVS_NAME// &
     &                               ' -- please run task tsmo' )
                                 RETURN 
                            END IF
                            IF ( .NOT. PIM%STA(PIM%OBS(IND_OBS)%STA_IND(2))%STMO(PIM%CONF%FRQ_GRP)%TSYS_AVAIL ) THEN
                                 CALL ERR_LOG ( 8425, IUER, 'PIMA_SPLT', 'Trap of internal '// &
     &                               'control: modeled system temperature is not available '// &
     &                               'for station '//PIM%STA(PIM%OBS(IND_OBS)%STA_IND(2))%IVS_NAME// &
     &                               ' -- please run task tsmo' )
                                 RETURN 
                            END IF
                            IF ( PIM%OBS(IND_OBS)%STMO_IND(1) .LE. 0 ) GOTO 680
                            IF ( PIM%OBS(IND_OBS)%STMO_IND(2) .LE. 0 ) GOTO 680
                       END IF
!
                       L_UVO = L_UVO + 1
!
! -------------------- A special case of the first...
!
                       IF ( J7 == 1 ) THEN
                            TIM_SEG(1,N_SEG) = MIN ( TIM_SEG(1,N_SEG), PIM%OBS(IND_OBS)%TIM_BEG )
                       END IF
!
! -------------------- ... and the last segment
!
                       IF ( J7 == K_SEG ) THEN
                            TIM_SEG(2,N_SEG) = MAX ( TIM_SEG(2,N_SEG), PIM%OBS(IND_OBS)%TIM_END )
                       END IF
                       TIM_SEG(3,N_SEG) = (TIM_SEG(1,N_SEG) + TIM_SEG(2,N_SEG))/2.0D0
 680                   CONTINUE 
 480                CONTINUE 
                    IF ( PIM%CONF%DEBUG_LEVEL .GE. 7 ) THEN
                         WRITE ( 6, 140 ) N_SEG, TIM_SEG(1:3,N_SEG), L_UVO 
 140                     FORMAT ( 'PIMA_SPLT Seg: ', I5, ' Tim_123: ', &
     &                             F9.3, 1X, F9.3, 1X, F9.3, ' L_UVO: ', I6 )
                    END IF
 470             CONTINUE 
                 IF ( PIM%CONF%DEBUG_LEVEL .GE. 7 ) THEN
                      IF ( TIM_SEG(1,N_SEG) < -86400.0D0 .OR. &
     &                     TIM_SEG(2,N_SEG) < -86400.0D0      ) THEN
                           WRITE ( 6, 150 ) J5, L_UVO, K_SEG, N_SEG
 150                       FORMAT ( 'PIMA_SPLT Sca: ', I4,' L_UVO: ', I5, &
     &                              ' K_SEG: ', I5, ' N_SEG: ', I5, &
     &                              ' Skipped ' )
                        ELSE 
                          WRITE ( 6, 160 ) J5, L_UVO, K_SEG, N_SEG, &
     &                                     TIM_SEG(1,N_SEG), TIM_SEG(3,N_SEG)
 160                      FORMAT ( 'PIMA_SPLT Sca: ', I4,' L_UVO: ', I5, &
     &                         ' K_SEG: ', I5, ' N_SEG: ', I5, &
     &                         ' Tim start/stop/mid: ', F8.2, 1X, F8.2, 1X, F8.2 )
                      END IF
                 END IF
            END IF
 450     CONTINUE 
         IF ( L_UVO == 0 ) THEN
              IF ( PIM%CONF%DEBUG_LEVEL .GE. 1 ) THEN
                   WRITE ( 6, 170 ) PIM%SOU(J4)%IVS_NAME, PIM%SOU(J4)%J2000_NAME
 170               FORMAT ( ' PIMA_SPLT No useful observations were found ', &
     &                      'for source ', A, 1X, A, ' SKIPPING'/ )
                   CALL FLUSH ( 6 )
              END IF
              DEALLOCATE ( TIM_SEG     )
              DEALLOCATE ( IND_SCA_SEG )
              GOTO 440
         END IF
!
! ------ Allocate memory for time+frequency averaged UV data
!
         ALLOCATE ( UVO(L_UVO), STAT=IER )
         IF ( IER .NE. 0 ) THEN
              CALL CLRCH  ( STR )
              CALL IINCH8 ( INT8(L_UVO)*INT8(SIZEOF(UVO(1))), STR )
              CALL ERR_LOG  ( 8426, IUER, 'PIMA_SPLT', 'Failure to '// &
     &            'allocate '//STR(1:I_LEN(STR))//' bytes of dynamic '// &
     &            'memory for UVO data for source '//PIM%C_SOU(J4) )
              RETURN
         END IF
         CALL NOUT ( L_UVO*SIZEOF(UVO(1)), UVO )
!
! ------ Check whether we are going to compute wieghts over an interval
! ------ different than the segment length
!
         FL_WEI_OBS = .FALSE.
         IF ( PIM%CONF%SPLT_WEIGHT_TYPE == PIMA__SPLT_WEI_OBS_SNR ) THEN
!
! ----------- Yes: we will compute SNR over the observation
!
              FL_WEI_OBS = .TRUE.
              L_WEI_SEG = N_SEG
           ELSE IF ( PIM%CONF%SPLT_WEIGHT_TYPE == PIMA__SPLT_WEI_OBS_RMS ) THEN
!
! ----------- Yes: we will compute visibility rms over the observation
!
              FL_WEI_OBS = .TRUE.
              L_WEI_SEG = N_SEG
           ELSE IF ( PIM%CONF%SPLT_WEIGHT_TYPE == PIMA__SPLT_WEI_AUTO .AND. &
     &               PIM%CONF%SPLT_TIM_MSEG*PIM%CONF%SPLT_FRQ_MSEG < PIM__SPLT_SNR_MINSEG ) THEN
!
! ----------- Yes: we will compute visibility rms over PIM__SPLT_SNR_MINSEG/PIM%CONF%SPLT_FRQ_MSEG  or
! ----------- the total number of segments, whichever is the least
!
              FL_WEI_OBS = .TRUE.
              L_WEI_SEG = MIN ( N_SEG, MAX ( 1, PIM__SPLT_SNR_MINSEG/PIM%CONF%SPLT_FRQ_MSEG ) )
           ELSE 
              FL_WEI_OBS = .TRUE.
              L_WEI_SEG  = 1
         END IF
!
         ALLOCATE ( SEFD_IF(KFRQ,LPOL), STAT=IER )
         IF ( IER .NE. 0 ) THEN
              CALL CLRCH ( STR )
              CALL INCH  ( 8*KFRQ*LPOL, STR )
              CALL ERR_LOG ( 8427, IUER, 'PIMA_SPLT', 'Failure to '// &
     &            'allocate '//STR(1:I_LEN(STR))//' bytes of dynamic '// &
     &            'memory for SEFD_IF object for source '// &
     &             PIM%C_SOU(J4) )
              RETURN
         END IF
         DO 490 J9=1,L_UVO
            ALLOCATE ( UVO(J9)%SPE(KFRQ,LPOL), &
     &                 UVO(J9)%WEI(KFRQ,LPOL), &
     &                 UVO(J9)%SNR(KFRQ,LPOL), &
     &                 UVO(J9)%SEFD(KFRQ,LPOL), STAT=IER )
            IF ( IER .NE. 0 ) THEN
                 CALL CLRCH ( STR )
                 CALL IINCH ( 4*KFRQ*LPOL*SIZEOF(UVO(J9)%SPE(1,1)), STR )
                 CALL ERR_LOG ( 8428, IUER, 'PIMA_SPLT', 'Failure to '// &
     &               'allocate '//STR(1:I_LEN(STR))//' bytes of dynamic '// &
     &               'memory for UVO object for source '// &
     &                PIM%C_SOU(J4) )
                 RETURN
            END IF
!
! --------- Initialization of UVO object
!
            UVO(J9)%SPE  = (0.0, 0.0)
            UVO(J9)%WEI  = 0.0
            UVO(J9)%SNR  = 0.0
            UVO(J9)%SEFD = 0.0
 490     CONTINUE 
         IF ( PIM%CONF%SPLT_TOTAL_UV == PIMA__TOTAL_UV_YES ) THEN
              FIL_TOT= TRIM(FITSDIR)//'/'//PIM%SOU(J4)%J2000_NAME//'_'// &
     &                 PIM%CONF%BAND//'_uvt.txt'
              LUN_TOT = GET_UNIT()
              OPEN ( UNIT=LUN_TOT, FILE=FIL_TOT, IOSTAT=IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 8429, IUER, 'PIMA_SPLT', 'Error in '// &
     &                 'openning output file '//FIL_TOT )
                   RETURN 
              END IF
              WRITE ( UNIT=LUN_TOT, FMT='(A)' ) PIMA__SPLT_TOT_LABEL   
              WRITE ( UNIT=LUN_TOT, FMT='(A)' ) '# '
              WRITE ( UNIT=LUN_TOT, FMT='(A)' ) '# Generated by '//PIMA__LABEL//'  on '//GET_CDATE()
              WRITE ( UNIT=LUN_TOT, FMT='(A)' ) '# Using data of VLBI experiment '//TRIM(PIM%CONF%SESS_CODE)//'  band '//TRIM(PIM%CONF%BAND)
              WRITE ( UNIT=LUN_TOT, FMT='(A)' ) '# '
         END IF
!
! ------ Cycle over scans
!
         K_UVO = 1
         DO 4100 J10=1,PIM%NSCA
            IF ( PIM%SCA(J10)%SOU_IND .NE. J4 ) GOTO 4100
   write ( 6, * ) 'PIMA_SPLT-751 j10 ', j10, ' l_uvo= ', l_uvo ! %%%%%%%%%%%%%%
!
! --------- Cycle over all *used* observations of this scans
!
            DO 4110 J11=1,PIM%SCA(J10)%NBAS
               IF ( FL_TIMER ) CALL WALL_TIMER ( %VAL(0) )
               IND_OBS = PIM%SCA(J10)%OBS_IND(J11)
               FRG_IND = PIM%OBS(IND_OBS)%REF_FRG_INDS(PIM%CONF%FRQ_GRP)
               IF ( FRG_IND == 0 ) THEN
                    IF ( PIM%CONF%DEBUG_LEVEL .GE. 4 ) THEN
                         WRITE ( 6, 210 ) IND_OBS
 210                     FORMAT ( 'PIMA_SPLT: observation ', I6, ' is discarded '// &
     &                            'because it does not belong to any frequency group' )
                    END IF
                    GOTO 4110
               END IF
               SNR = PIM%OBS(IND_OBS)%AMPL(PIMA__DRF,IND_BND)/PIM%OBS(IND_OBS)%NOISE(IND_BND)
               IND_STA(1) = PIM%OBS(IND_OBS)%STA_IND(1)
               IND_STA(2) = PIM%OBS(IND_OBS)%STA_IND(2)
!
! ------------ Bypass observations that 
! ------------ 1) have the SNR below the detection threshold
! ------------ 2) are in the deselection list
! ------------ 3) do not have Tsys at at least one station
!
               IF ( .NOT. PIM%USE_OBS(IND_OBS) ) GOTO 4110 ! Bypass deselected observation
               IF ( PIM%CONF%TSYS_CAL_CODE == PIMA__TSYS_MEASURED ) THEN
                    IF ( PIM%OBS(IND_OBS)%TSYS_IND(1,PIM%CONF%FRQ_GRP) .LE. 0 ) THEN
                         IF ( PIM%CONF%DEBUG_LEVEL .GE. 4 ) THEN
                              WRITE ( 6, 220 ) IND_OBS, PIM%C_STA(PIM%OBS(IND_OBS)%STA_IND(1))
 220                          FORMAT ( 'PIMA_SPLT: observation ', I6, ' is discarded '// &
     &                                 'because thre is not Tsys at ', A )
                         END IF
                         GOTO 4110
                    END IF
                    IF ( PIM%OBS(IND_OBS)%TSYS_IND(2,PIM%CONF%FRQ_GRP) .LE. 0 ) THEN
                         WRITE ( 6, 220 ) IND_OBS, PIM%C_STA(PIM%OBS(IND_OBS)%STA_IND(2))
                         GOTO 4110
                    END IF
                 ELSE IF ( PIM%CONF%TSYS_CAL_CODE == PIMA__TSYS_CLEANED .OR. &
     &                     PIM%CONF%TSYS_CAL_CODE == PIMA__TSYS_MODELED      ) THEN
                    IF ( PIM%OBS(IND_OBS)%STMO_IND(1) .LE. 0 ) THEN
                         WRITE ( 6, 230 ) IND_OBS, PIM%C_STA(PIM%OBS(IND_OBS)%STA_IND(1))
 230                     FORMAT ( 'PIMA_SPLT: observation ', I6, ' is discarded '// &
     &                            'because Tsys at ', A, ' was not cleaned' )
                         GOTO 4110
                    ENDIF
                    IF ( PIM%OBS(IND_OBS)%STMO_IND(2) .LE. 0 ) THEN
                         WRITE ( 6, 230 ) IND_OBS, PIM%C_STA(PIM%OBS(IND_OBS)%STA_IND(2))
                         GOTO 4110
                    END IF
               END IF
               IF ( PIM%SUB%OBS_IND_SUB(IND_OBS) < 1 ) THEN
                    PIM%USE_OBS(IND_OBS) = .FALSE.
                    CALL CLRCH ( STR ) 
                    CALL INCH  ( IND_OBS, STR )
                    IER = -1
                    CALL ERR_LOG ( 8430, IER, 'PIMA_SPLT', 'Trap of internal '// &
     &                  'control: no subarray was associated with observation '// &
     &                   STR(1:I_LEN(STR))//' -- suppress this observation and '// &
     &                  'continue' )
                    GOTO 4110
               END IF
!
               LTIM = PIM%OBS(IND_OBS)%NUM_EPC(FRG_IND)
!
               CALL ERR_PASS ( IUER, IER )
               CALL PIMA_GET_POL_MODE ( PIM, PIM%CONF%POLAR, IND_OBS, POL_MODE, IER )
               IF ( IER .NE. 0 ) THEN
                    CALL CLRCH ( STR )
                    CALL INCH  ( IND_OBS, STR )
                    CALL ERR_LOG ( 8431, IER, 'PIMA_SPLT', 'Cannot set polarization '// &
     &                  'mode for observation '//TRIM(STR)//' with requested '// &
     &                  'polarization '//PIM%CONF%POLAR )
                    IF ( PIM%CONF%SPLT_TOTAL_UV == PIMA__TOTAL_UV_YES ) THEN
                         CLOSE ( UNIT=LUN_TOT )
                    END IF
                    RETURN
               END IF
!
               IF ( POL_MODE == PIMA__IPLL  .OR.  &
     &              POL_MODE == PIMA__IPLC  .OR.  &
     &              POL_MODE == PIMA__IPCL        ) THEN
!
                    POL_MODE = PIMA__PALL_XY
                  ELSE IF ( POL_MODE == PIMA__IPCC  ) THEN
                    POL_MODE = PIMA__PALL_CIR
               END IF
!
               CALL ERR_PASS ( IUER, IER )
               CALL PIMA_GET_OBS ( PIM, VTD, IND_OBS, POL_MODE, LPOL, &
     &                             .TRUE., .TRUE., .FALSE., FRI_STS, IER )
               IF ( IER .NE. 0 ) THEN
                    CALL CLRCH ( STR )
                    CALL INCH  ( IND_OBS, STR )
                    CALL ERR_LOG ( 8432, IER, 'PIMA_SPLT', 'Error in reading '// &
     &                  'visibilities for observation '//STR )
                    IF ( PIM%CONF%SPLT_TOTAL_UV == PIMA__TOTAL_UV_YES ) THEN
                         CLOSE ( UNIT=LUN_TOT )
                    END IF
                    RETURN
               END IF
               IF ( INDEX ( STR_UV_PRINT, 'RAW1' ) > 0 ) THEN
                    CALL PIMA_UV_PRINT ( PIM, IND_OBS, 1, 'PS-RAW1' )
                  ELSE IF ( INDEX ( STR_UV_PRINT, 'RAW2' ) > 0 ) THEN
                    CALL PIMA_UV_PRINT ( PIM, IND_OBS, 2, 'PS-RAW2' )
               END IF
               LU_OBS = LU_OBS + 1
               INDS_OBS_USED(LU_OBS) = IND_OBS
               IFRQ = 0
               RENRML_CROS_IF_ALL = 1.0D0
               RENRML_AUTO_IF_ALL = 1.0D0
               NOI_SMEARING = 1.0D0
               NOI_BEAM     = 1.0D0
!
               IF ( PIM%CONF%FRIB_AMPL_EDGE_WINDOW_COR == PIMA__EDGE_AMP_USE ) THEN
!
! ----------------- Compute decorrelation losses in an individual UV-point
! ----------------- due to significant group delay and/or phase delay rate
!
                    CALL ERR_PASS ( IUER, IER )
                    CALL PIMA_DECOR_SMEARING ( PIM, VTD, IND_OBS, &
     &                                         PIM%OBS(IND_OBS)%FRT_OFFSET(IND_BND), &
     &                                         PIM%OBS(IND_OBS)%RES_MB_DEL(IND_FRA,IND_BND), &
     &                                         DECOR_BS, DECOR_TS, IER )
                    IF ( IER .NE. 0 ) THEN
                         CALL ERR_LOG ( 7536, IUER, 'PIMA_SPLT', 'Error in '// &
     &                       'computation decorrelation due to bandwidth and time '// &
     &                       'smearing' )
                         RETURN 
                    END IF
!
! ----------------- Apply decorretion to visibilities
!
                    PIM%OBS(IND_OBS)%UV = PIM%OBS(IND_OBS)%UV/(DECOR_BS * DECOR_TS)
                 ELSE
                    DECOR_BS = 1.0D0
                    DECOR_TS = 1.0D0
               END IF
!
               IF ( PIMA_FRI_USED_VERS .LT. "PIMA v 2.35   2020.04.22" .OR. &
     &              FL_NOI_LOSS_MISSED                                      ) THEN
!
! ----------------- We apply noise amplification due to smearing only for 
! ----------------- processing old observations
!
                    NOI_SMEARING = 1.0D0/(DECOR_BS * DECOR_TS)
               END IF
               IF ( PIM%CONF%FRIB_AMPL_EDGE_BEAM_COR == PIMA__BEAM_YES ) THEN
!
! ----------------- Compute factors for the primary beam attenuation at the 
! ----------------- lowest frequency of the band 
!
                    NRML_BEAM_ATT_LOW = DSQRT ( PIMA_BEAM_ATT ( PIM, PIM%FREQ_ARR(1,PIM%CONF%BEG_FRQ,PIM%CONF%FRQ_GRP),   &
     &                                               INT(PIM%OBS(IND_OBS)%SOU_IND,KIND=4),      &
     &                                               INT(PIM%OBS(IND_OBS)%STA_IND(1),KIND=4) )* &
                                                PIMA_BEAM_ATT ( PIM, PIM%FREQ_ARR(1,PIM%CONF%BEG_FRQ,PIM%CONF%FRQ_GRP),   &
     &                                               INT(PIM%OBS(IND_OBS)%SOU_IND,KIND=4),      &
     &                                               INT(PIM%OBS(IND_OBS)%STA_IND(2),KIND=4) ) &
     &                                        )
                  ELSE 
                    NRML_BEAM_ATT_LOW = 1.0D0
               END IF
               IF ( DECOR_BS * DECOR_TS * NRML_BEAM_ATT_LOW < PIMA__AMP_ATT_MIN ) THEN
!
! ----------------- Discard an observation with a very large combined attenuation
!
                    WRITE  ( 6, 240 ) IND_OBS, DECOR_BS, DECOR_TS, NRML_BEAM_ATT_LOW, &
     &                                PIMA__AMP_ATT_MIN 
 240                FORMAT ( 'PIMA_SPLT: observation ', I6, ' is discarded '   &
     &                       'because combined amplitude attenuation due to '  &
     &                       ' band smearing, time smearing, primiary beam, ', &
     &                        F6.4, 1X, F6.4, 1X, F6.4, ' is below the threshold ', F6.4 )
                    GOTO 4110
               END IF
!
               DO 4130 J13=1,LPOL
                  IFRQ = 0
                  DO 4140 J14=PIM%CONF%BEG_FRQ,PIM%CONF%END_FRQ
                     IFRQ = IFRQ + 1
!
! ------------------ Compute the renormalization bandpass factor for cross-correlation amplitude
! ------------------ for the given baseline using voltage renormalization factors.
!
                     IF ( FL_BPASS .AND. PIM%CONF%SPLT_BPASS_NRML_METHOD .EQ. PIMA__WEIGHTED ) THEN
                          IF ( PIM%BPASS(1)%PIMA_VERS .GE. PIMA__BPS_AMP_VERS ) THEN
                               IF ( J13 == 1 ) THEN
                                    RENRML_CROS_IF(IFRQ,J13) = &
     &                                   PIM%BPASS(PIM%OBS(IND_OBS)%STA_IND(1))%SPLT_NRML_FRQ(J14)* &
     &                                   PIM%BPASS(PIM%OBS(IND_OBS)%STA_IND(2))%SPLT_NRML_FRQ(J14)
                                  ELSE
!
! --------------------------------- In the case of dual-pol observations, bandpass renormalization for
! --------------------------------- LL pol is kept in PIM%PBP()%SPLT_NRML_FRQ()
!
                                     RENRML_CROS_IF(IFRQ,J13) = &
     &                                   PIM%PBP(PIM%OBS(IND_OBS)%STA_IND(1))%SPLT_NRML_FRQ(J14)* &
     &                                   PIM%BPASS(PIM%OBS(IND_OBS)%STA_IND(2))%SPLT_NRML_FRQ(J14)
                                END IF
                            ELSE
                               IF ( PIM%BPASS(PIM%OBS(IND_OBS)%STA_IND(1))%L_OBS > 0 .AND. &
     &                              PIM%BPASS(PIM%OBS(IND_OBS)%STA_IND(2))%L_OBS > 0       ) THEN
                                    RENRML_CROS_IF(IFRQ,J13) = DSQRT ( &
     &                                   PIM%BPASS(PIM%OBS(IND_OBS)%STA_IND(1))%SPLT_NRML_FRQ(J14)* &
     &                                   PIM%BPASS(PIM%OBS(IND_OBS)%STA_IND(2))%SPLT_NRML_FRQ(J14) &
     &                                                )
                                 ELSE IF ( PIM%BPASS(PIM%OBS(IND_OBS)%STA_IND(1))%L_OBS == 0 .AND. &
     &                                     PIM%BPASS(PIM%OBS(IND_OBS)%STA_IND(2))%L_OBS >  0       ) THEN
                                    RENRML_CROS_IF(IFRQ,J13) = PIM%BPASS(PIM%OBS(IND_OBS)%STA_IND(2))%SPLT_NRML_FRQ(J14) 
                                 ELSE IF ( PIM%BPASS(PIM%OBS(IND_OBS)%STA_IND(1))%L_OBS >  0 .AND. &
     &                                    PIM%BPASS(PIM%OBS(IND_OBS)%STA_IND(2))%L_OBS == 0       ) THEN
                                    RENRML_CROS_IF(IFRQ,J13) = PIM%BPASS(PIM%OBS(IND_OBS)%STA_IND(1))%SPLT_NRML_FRQ(J14) 
                                 ELSE 
                                    RENRML_CROS_IF(IFRQ,J13) = 1.0
                               END IF
                               IF ( IS_R8_NAN(RENRML_CROS_IF(IFRQ,J13)) ) THEN
                                    RENRML_CROS_IF(IFRQ,J13) = 1.0
                               END IF
                          END IF
                        ELSE 
                          RENRML_CROS_IF(IFRQ,J13) = 1.0
                     END IF
                     IF ( PIM%CONF%SPLT_AUTOCORR_NRML_METHOD == PIMA__AVERAGED .AND. &
     &                    PIM%CONF%FRIB_AUTOCORR_CALIB .NE. PIMA__ACCR_NO ) THEN
!
! ----------------------- Copy renormalizaion factors for autocorrelation that supposed
! ----------------------- to correct the fringe amplitude due to the bias in effective Tsys
! ----------------------- with respect to the measured Tsys that emerges when only a fraction 
! ----------------------- of the bandwidth is used because of masking since the autocorrelation 
! ----------------------- spectrum is not uniform
!
                          RENRML_AUTO_IF(IFRQ,1,J13) = PIM%OBS(IND_OBS)%TSRF(IFRQ,1,J13) 
                          RENRML_AUTO_IF(IFRQ,2,J13) = PIM%OBS(IND_OBS)%TSRF(IFRQ,2,J13) 
                          IF ( RENRML_AUTO_IF(IFRQ,2,J13) < PIMA__WEI_MIN ) THEN
!
! ---------------------------- This is a phathological case. Just reset the renormalization
! ---------------------------- factors to 1.
!
                               RENRML_AUTO_IF(IFRQ,1,J13) = 1.0D0
                               RENRML_AUTO_IF(IFRQ,2,J13) = 1.0D0
                          END IF
                        ELSE 
                          RENRML_AUTO_IF(IFRQ,1,J13) = 1.0D0
                          RENRML_AUTO_IF(IFRQ,2,J13) = 1.0D0
                     END IF
!
                     RENRML_CROS_IF_ALL(J13) = RENRML_CROS_IF_ALL(J13)*RENRML_CROS_IF(IFRQ,J13)
                     RENRML_AUTO_IF_ALL(J13) = RENRML_AUTO_IF_ALL(J13)*RENRML_AUTO_IF(IFRQ,1,J13)* &
     &                                                                 RENRML_AUTO_IF(IFRQ,2,J13)
!
                     IF ( PIM%CONF%FRIB_AMPL_EDGE_BEAM_COR == PIMA__BEAM_YES ) THEN
!
! ----------------------- Compute factors for the primary beam attenuation
!
                          NRML_BEAM_ATT(1) = PIMA_BEAM_ATT ( PIM, PIM%FREQ_ARR(1,J14,PIM%CONF%FRQ_GRP),   &
     &                                                            INT(PIM%OBS(IND_OBS)%SOU_IND,KIND=4),   &
     &                                                            INT(PIM%OBS(IND_OBS)%STA_IND(1),KIND=4) )
                          NRML_BEAM_ATT(2) = PIMA_BEAM_ATT ( PIM, PIM%FREQ_ARR(1,J14,PIM%CONF%FRQ_GRP),   &
     &                                                            INT(PIM%OBS(IND_OBS)%SOU_IND,KIND=4),   &
     &                                                            INT(PIM%OBS(IND_OBS)%STA_IND(2),KIND=4) )
                        ELSE 
                          NRML_BEAM_ATT(1) = 1.0D0
                          NRML_BEAM_ATT(2) = 1.0D0
                     END IF
                     IF ( PIM%CONF%DEBUG_LEVEL .GE. 6 .AND. &
     &                    PIM%CONF%FRIB_AMPL_EDGE_BEAM_COR == PIMA__BEAM_YES .AND. &
     &                    IFRQ == 1 ) THEN
!
                          WRITE ( 6, 190 ) PIM%C_STA(PIM%OBS(IND_OBS)%STA_IND(1)), &
     &                                     NRML_BEAM_ATT(1), & 
     &                                     PIM%C_STA(PIM%OBS(IND_OBS)%STA_IND(2)), &
     &                                     NRML_BEAM_ATT(2) 
 190                      FORMAT ( 'PIMA_SPLT BEAM_COR Sta_1: ', A, ' Beam= ', F6.4, &
     &                                               ' Sta_2: ', A, ' Beam= ', F6.4  )
                     END IF 
!
! ------------------ Apply amplitude renormalization and correction for the beam attenuation
!
                     PIM%OBS(IND_OBS)%UV(1:PIM%NCHN,IFRQ,1:LTIM,J13) = &
     &                       PIM%OBS(IND_OBS)%UV(1:PIM%NCHN,IFRQ,1:LTIM,J13)*RENRML_CROS_IF(IFRQ,J13)/ &
     &                       DSQRT ( NRML_BEAM_ATT(1) * NRML_BEAM_ATT(2) )
                     NOI_BEAM = 1.0D0/DSQRT ( NRML_BEAM_ATT(1) * NRML_BEAM_ATT(2) )
 4140             CONTINUE
                  IF ( FL_TIMER .AND. PIM%CONF%DEBUG_LEVEL .EQ. 7 ) THEN
                       CALL WALL_TIMER ( STR     )
                       WRITE ( 6, '(A,I5,1X,A)' ) 'Proc. 2    Observation ', IND_OBS, STR(1:I_LEN(STR))
                       CALL WALL_TIMER ( %VAL(0) )
                  END IF
                  RENRML_CROS_IF_ALL(J13)= RENRML_CROS_IF_ALL(J13)**(1.D0/LFRQ)
                  RENRML_AUTO_IF_ALL(J13)= RENRML_AUTO_IF_ALL(J13)**(1.D0/(2.0D0*LFRQ))
!
                  PH_DEL_APSO = 0.0D0
                  PH_RAT_APSO = 0.0D0
                  PH_ACC_APSO = 0.0D0
!
                  ALLOCATE ( TIM_ARR(LTIM), STAT=IER )
                  IF ( IER .NE. 0 ) THEN
                       CALL CLRCH ( STR )
                       CALL INCH  ( 8*LTIM, STR )
                       CALL ERR_LOG ( 8433, IUER, 'PIMA_SPLT', 'Error in '// &
     &                     'an attempt to allocate '//STR(1:I_LEN(STR))// &
     &                     ' bytes of dynamic memory for array TIM_ARR' )
                       IF ( PIM%CONF%SPLT_TOTAL_UV == PIMA__TOTAL_UV_YES ) THEN
                            CLOSE ( UNIT=LUN_TOT )
                       END IF
                       RETURN
                  END IF
                  TIM_ARR = 0.0D0
!
                  IND_AP_USED = 0
                  DURA_ACC = 0.0D0
                  DO 4170 J17=1,LTIM
                     UV_IND  = PIM%OBS(IND_OBS)%UV_IND(J17,FRG_IND)
                     TIM_ARR(J17) = PIM%TIM_R8(PIM%UV_IND(UV_IND)%TIM_IND) - &
     &                              PIM%OBS(IND_OBS)%TIM_BEG
                     IF ( TIM_ARR(J17) < PIM%CONF%SCAN_LEN_SKIP ) THEN
                          PIM%OBS(IND_OBS)%WEI_1D(J17,J13) = 0.0D0
                     END IF 
                     DURA_ACC = DURA_ACC + PIM%OBS(IND_OBS)%WEI_1D(J17,J13)*PIM%OBS(IND_OBS)%AP_LEN
                     IF ( DURA_ACC > PIM%CONF%SCAN_LEN_USED ) THEN
                          DURA_ACC = DURA_ACC - PIM%OBS(IND_OBS)%WEI_1D(J17,J13)*PIM%OBS(IND_OBS)%AP_LEN
                          PIM%OBS(IND_OBS)%WEI_1D(J17,J13) = 0.0D0
                     END IF
!
                     IF ( PIM%CONF%BANDPASS_USE == PIMA__BPASS_AMP_PHS  .OR. &
     &                    PIM%CONF%BANDPASS_USE == PIMA__BPASS_AMP           ) THEN
                          IFRQ = 0
                          DO 4180 J18=PIM%CONF%BEG_FRQ,PIM%CONF%END_FRQ
                             IFRQ = IFRQ + 1
                             DO 4190 J19=1,PIM%NCHN
                                IF ( PIM%BPASS(1)%PIMA_VERS .GE. PIMA__BPS_AMP_VERS ) THEN
!
! ---------------------------------- Modern, post 2017.03.03 case
!
                                     AMP_BPASS = 1.0/( &
     &                                                 ABS( PIM%BPASS(IND_STA(1))%BPS(J19,J18) ) * &
     &                                                 ABS( PIM%BPASS(IND_STA(2))%BPS(J19,J18) )   &
     &                                                )
                                     IF ( ASSOCIATED ( PIM%PBP ) ) THEN
                                          IF ( J13 == 2 .AND. ASSOCIATED ( PIM%PBP(IND_STA(1))%CMPL ) ) THEN
                                               AMP_BPASS = AMP_BPASS/( &
     &                                                                 ABS( PIM%PBP(IND_STA(1))%CMPL(J19,J18) ) &
     &                                                                )
                                            ELSE IF ( J13 == 3 .AND. ASSOCIATED ( PIM%PBP(IND_STA(2))%CMPL ) ) THEN
                                               AMP_BPASS = AMP_BPASS/( &
     &                                                                 ABS( PIM%PBP(IND_STA(2))%CMPL(J19,J18) ) &
     &                                                     )
                                            ELSE IF ( J13 == 4 .AND.                               &
     &                                                ASSOCIATED ( PIM%PBP(IND_STA(1))%CMPL ) .AND. &
     &                                                ASSOCIATED ( PIM%PBP(IND_STA(2))%CMPL )       ) THEN
                                               AMP_BPASS = AMP_BPASS/( &
     &                                                                 ABS( PIM%PBP(IND_STA(1))%CMPL(J19,J18) )* &
     &                                                                 ABS( PIM%PBP(IND_STA(2))%CMPL(J19,J18) )  &
     &                                                                )
                                          END IF
                                     END IF
                                   ELSE
!
! ---------------------------------- Legacy case: the bandpass for a reference station was
! ---------------------------------- set to (1.0, 0.0). Parameter LOBS for the bandpass
! ---------------------------------- reference station is 0
!
                                     IF ( PIM%BPASS(IND_STA(1))%L_OBS > 0 .AND. &
     &                                    PIM%BPASS(IND_STA(2))%L_OBS > 0       ) THEN
!
! --------------------------------------- Neither station was the bandpass reference station
!
                                          AMP_BPASS = 1.0/SQRT(                                           &
     &                                                          ABS( PIM%BPASS(IND_STA(1))%BPS(J19,J18) )* &
     &                                                          ABS( PIM%BPASS(IND_STA(2))%BPS(J19,J18) )  &
     &                                                        )
                                        ELSE IF ( PIM%BPASS(IND_STA(1))%L_OBS == 0 .AND. &
     &                                            PIM%BPASS(IND_STA(2))%L_OBS >  0       ) THEN
!
! -------------------------------------- The first station was the bandpass reference station
!
                                          AMP_BPASS = 1.0/ABS( PIM%BPASS(IND_STA(2))%BPS(J19,J18) )
                                        ELSE IF ( PIM%BPASS(IND_STA(2))%L_OBS == 0 .AND. &
     &                                            PIM%BPASS(IND_STA(1))%L_OBS >  0       ) THEN
!
! --------------------------------------- The second station was the bandpass reference station
!
                                          AMP_BPASS = 1.0/ABS( PIM%BPASS(IND_STA(1))%BPS(J19,J18) )
                                        ELSE
!
! --------------------------------------- This is a pathological case
!
                                          AMP_BPASS = 1.0
                                     END IF
                                END IF ! vers
!
! ----------------------------- Apply amplitude bandpass calibration
!
                                PIM%OBS(IND_OBS)%UV(J19,IFRQ,J17,J13) = AMP_BPASS * PIM%OBS(IND_OBS)%UV(J19,IFRQ,J17,J13) 
 4190                        CONTINUE 
 4180                     CONTINUE 
                     END IF ! bandpass use
!
                     IF ( PIM%OBS(IND_OBS)%WEI_1D(J17,J13) > PIMA__WEI_MIN ) THEN
!
! ----------------------- Update index of the first and the last used AP
!
                          IF ( IND_AP_USED(1) == 0 ) IND_AP_USED(1) = J17
                          IND_AP_USED(2) = J17
                     END IF
 4170             CONTINUE 
!
                  IF ( LTIM .GE. PIMA__APSO_LTIM_LIM ) THEN
                       CALL ERR_PASS ( IUER, IER )
                       TIM_FRT = PIM%OBS(IND_OBS)%FRT_OFFSET(1)
!
! -------------------- Phase rotation for a source position shift from pointing direction
! -------------------- to the apriori  direction. This takes into account non-linear
! -------------------- (mainly quadratic) effect on fringe phases
!
                       CALL PIMA_APSO_FIX ( PIM, VTD, IND_OBS, LFRQ, LTIM, PIM%NCHN, &
     &                           DEL_APSO, RAT_APSO, ACC_APSO, DEL_QUAD_MAX, &
     &                           PHS_APSO_MAX, PHS_APSO_LIM, TIM_ARR, .TRUE., &
     &                           PIM%OBS(IND_OBS)%UV(1,1,1,J13), IER )
                       IF ( IER .NE. 0 ) THEN
                            CALL ERR_LOG ( 8434, IUER, 'PIMA_SPLT', 'Failure to perform '// &
     &                          'correction for quadratic term in fringe phase for '// &
     &                          'significant offset in position of source '// &
     &                           PIM%C_SOU(J4) )
                            IF ( PIM%CONF%SPLT_TOTAL_UV == PIMA__TOTAL_UV_YES ) THEN
                                 CLOSE ( UNIT=LUN_TOT )
                            END IF
                            RETURN
                       END IF
                       IF ( PIM%CONF%DEBUG_LEVEL .GE. 6 ) THEN
                            WRITE ( 6, * ) 'PIMA_SPLT-1075  DEL_APSO, RAT_APSO, ACC_APSO, PHS_APSO_MAX = ', &
     &                                                      DEL_APSO, RAT_APSO, ACC_APSO, PHS_APSO_MAX
                       END IF
                       IF ( PIM%OBS(IND_OBS)%RES_GR_RAT(IND_BND) > -0.5D0 ) THEN
                            IF ( PIMA_FRI_USED_VERS < 'PIMA v 2.35   2020.04.22' ) THEN
!
! ------------------------------ Correct group delay rate if we use an old fringe result file.
! ------------------------------ Once upon a time, just after the Big Bang, group delay rate
! ------------------------------ was corrected for the fringe rate due to the differences
! ------------------------------ in a priori source positions used from fringe fitting and 
! ------------------------------ the a priori source positions used for correltion.
! ------------------------------ If was discontinued after the 150th anniversary of 
! ------------------------------ Lenin's birth.
!
                                 PIM%OBS(IND_OBS)%RES_GR_RAT(IND_BND) = &
     &                                   PIM%OBS(IND_OBS)%RES_GR_RAT(IND_BND) - PH_RAT_APSO
                            END IF
                       END IF
                     ELSE
                       DEL_APSO  = 0.0D0
                       RAT_APSO  = 0.0D0
                       ACC_APSO  = 0.0D0
                       PHS_APSO_MAX = 0.0D0
                 END IF
!
                 IF ( PIM%OBS(IND_OBS)%RES_GR_RAT(IND_BND) < -0.5D0 ) THEN
                      CALL ERR_PASS ( IUER, IER )
                      CALL PIMA_APPLY_ACCEL ( PIM, PIM%NCHN, LFRQ, LTIM, &
     &                                        PIM%OBS(IND_OBS)%FRT_OFFSET(IND_BND), &
     &                                        TIM_ARR, PIM%OBS(IND_OBS)%WEI_1D(1,J13), &
     &                                        PIM%OBS(IND_OBS)%RES_PH_ACC(IND_BND), &
     &                                        PIM%OBS(IND_OBS)%UV(1,1,1,J13), IER )
                      IF ( IER .NE. 0 ) THEN
                           CALL ERR_LOG ( 8435, IUER, 'PIMA_SPLT', 'Failre to perform '// &
     &                         'correction for phase acceleration term in fringe phase' )
                           IF ( PIM%CONF%SPLT_TOTAL_UV == PIMA__TOTAL_UV_YES ) THEN
                                CLOSE ( UNIT=LUN_TOT )
                           END IF
                           RETURN
                      END IF
                 END IF
                 DEALLOCATE ( TIM_ARR )
!
                 IF ( ( PIM%CONF%DEBUG_LEVEL == 31 .AND. J13 == 1 ) .OR. &
     &                ( PIM%CONF%DEBUG_LEVEL == 32 .AND. J13 == 4 )      ) THEN
!
! ------------------- These plots are for deep testings
!
                      FL_NOFRI = .FALSE.
                      CALL GETENVAR ( 'PIMAVAR_SPLT_NOFRINGE', STR )
                      CALL TRAN ( 11, STR, STR )
                      IF ( STR == 'YES' ) FL_NOFRI = .TRUE.
                      IF ( FL_NOFRI ) THEN
                           FRI_COEF = 0.0D0
                         ELSE
                           FRI_COEF = 1.0D0
                      END IF
                      POL_LAB = PIM%CONF%POLAR
                      IF ( POL_MODE == PIMA__PALL_CIR ) THEN
                           IF ( J13 == 1 ) POL_LAB = 'RR'
                           IF ( J13 == 2 ) POL_LAB = 'LR'
                           IF ( J13 == 3 ) POL_LAB = 'RL'
                           IF ( J13 == 4 ) POL_LAB = 'LL'
                      END IF
                      CALL ERR_PASS ( IUER, IER )
                      CALL PIMA_FR1D_FRQ_PLOT ( PIM, IND_OBS, '/tmp/boo', LTIM, &
     &                                  PIM%NCHN, LFRQ, 1.0D0, &
     &                                  PIM%FREQ_ARR(1,PIM%CONF%BEG_FRQ,PIM%CONF%FRQ_GRP), &
     &                                  PIM%FRQ(PIM%CONF%BEG_FRQ,PIM%CONF%FRQ_GRP)%FREQ, &
     &                                  PIM%OBS(IND_OBS)%WEI_1D, &
     &                                  PIM%OBS(IND_OBS)%UV(1,1,1,J13), &
     &                                  PIM%OBS(IND_OBS)%AC(1,1,1,1,J13), &
     &                                  PIM%OBS(IND_OBS)%AC_MEAN, &
     &                                  PIM%OBS(IND_OBS)%FRT_OFFSET(1),&
     &                                  FRI_COEF*PIM%OBS(IND_OBS)%RES_MB_DEL(PIMA__LSQ,1), &
     &                                  FRI_COEF*PIM%OBS(IND_OBS)%RES_PH_RAT(PIMA__LSQ,1), &
     &                                  FRI_COEF*PIM%OBS(IND_OBS)%RES_GR_RAT(1), &
     &                                  0.0D0, &
     &                                  FRI_COEF*PIM%OBS(IND_OBS)%RES_PHS(PIMA__LSQ,1), &
     &                                  PIM%OBS(IND_OBS)%AMPL(PIMA__LSQ,1), VAR1,  &
     &                                  SNR, POL_LAB, 3, IER )
                 END IF
!
                 IF ( ( PIM%CONF%DEBUG_LEVEL == 33 .AND. J13 == 1 ) .OR. &
     &                ( PIM%CONF%DEBUG_LEVEL == 34 .AND. J13 == 2 )      ) THEN
!
! ------------------- These plots are for deep testsings
!
                      FL_NOFRI = .FALSE.
                      CALL GETENVAR ( 'PIMAVAR_SPLT_NOFRINGE', STR )
                      CALL TRAN ( 11, STR, STR )
                      IF ( STR == 'YES' ) FL_NOFRI = .TRUE.
                      IF ( FL_NOFRI ) THEN
                           FRI_COEF = 0.0D0
                         ELSE
                           FRI_COEF = 1.0D0
                      END IF
                      CALL PIMA_FR1D_TIM_PLOT ( PIM, IND_OBS, '/tmp/boo', LTIM, &
     &                                  PIM%NCHN, LFRQ, 1.0D0, &
     &                                  PIM%FREQ_ARR(1,PIM%CONF%BEG_FRQ,PIM%CONF%FRQ_GRP), &
     &                                  PIM%FRQ(PIM%CONF%BEG_FRQ,PIM%CONF%FRQ_GRP)%FREQ, &
     &                                  PIM%OBS(IND_OBS)%WEI_1D, PIM%OBS(IND_OBS)%UV, &
     &                                  PIM%OBS(IND_OBS)%FRT_OFFSET(1), &
     &                                  PIM%OBS(IND_OBS)%AP_LEN, &
     &                                  FRI_COEF*PIM%OBS(IND_OBS)%RES_MB_DEL(PIMA__LSQ,1), &
     &                                  FRI_COEF*PIM%OBS(IND_OBS)%RES_PH_RAT(PIMA__LSQ,1), &
     &                                  FRI_COEF*PIM%OBS(IND_OBS)%RES_GR_RAT(1), &
     &                                  0.0D0, &
     &                                  FRI_COEF*PIM%OBS(IND_OBS)%RES_PHS(PIMA__LSQ,1), &
     &                                  PIM%OBS(IND_OBS)%AMPL(PIMA__LSQ,1), &
     &                                  SNR, '??', 3, IER )
                 END IF
 4130         CONTINUE 
              IF ( FL_TIMER .AND. PIM%CONF%DEBUG_LEVEL .EQ. 7 ) THEN
                   CALL WALL_TIMER ( STR     )
                   WRITE ( 6, '(A,I5,1X,A)' ) 'Processing observation ', IND_OBS, STR(1:I_LEN(STR))
                   CALL WALL_TIMER ( %VAL(0) )
              END IF
!
! ----------- Now we process chunks of input UV data and generate output
! ----------- UV data, averaged over time and frequency within a data segment
!
              KSEG_LAST = 0
              IFRQ = PIM%CONF%BEG_FRQ - 1
!
              IND_SEG_LAST = 0
              IND_AP_LAST  = 0
!
              IF ( FL_WEI_OBS ) THEN
!
! ---------------- Allocate memory for accumulators that are needed to compute
! ---------------- the ampltide or the rms over the interval different than the 
! ---------------- segment interval
!
                   ALLOCATE ( ACC_WEI_SEG(1:LPOL,1:KFRQ),      &
     &                        ACCVIS_WEISQ_SEG(1:LPOL,1:KFRQ), &
     &                        ACCVIS_WEI_SEG(1:LPOL,1:KFRQ),   &
     &                        ACC_WEI(1:LPOL,1:KFRQ,N_SEG),    &
     &                        IND_ACC_UVO(L_UVO),              &
     &                        ACC_NAP(1:LPOL,1:KFRQ),          &
     &                        ACC_AMPL(1:LPOL,1:KFRQ),         &
     &                        STAT=IER )   
                   IF ( IER .NE. 0 ) THEN
                        CALL CLRCH ( STR )
                        CALL IINCH ( 4*LPOL*KFRQ + 4*LPOL*KFRQ +4*LPOL*KFRQ + &
     &                               4*LPOL*KFRQ*N_SEG + 4*L_UVO + 4*LPOL*KFRQ + 4*LPOL*KFRQ, &
     &                               STR )
                        CALL ERR_LOG ( 8436, IUER, 'PIMA_SPLT', 'Error in allocating '// &
     &                       TRIM(STR)//' bytes of dynamic memory for arrays '// &
     &                      'ACC_WEI_SEG, ACCVIS_WEISQ_SEG ACCVIS_WEI_SEG, ACC_WEI, '// &
     &                      'IND_ACC_UVO, ACC_NAP, ACC_AMPL' )
                        IF ( PIM%CONF%SPLT_TOTAL_UV == PIMA__TOTAL_UV_YES ) THEN
                             CLOSE ( UNIT=LUN_TOT )
                        END IF
                        RETURN 
                   END IF
!
! ---------------- Initialization
!
                   NUM_SEG_WEI      = 1
                   IAC_SEG          = 0
                   ACC_WEI          = 0.0
                   ACC_WEI_SEG      = 0.0
                   ACCVIS_WEISQ_SEG = 0.0
                   ACCVIS_WEI_SEG   = 0.0
                   ACC_NAP          = 0
                   IND_ACC_UVO      = 0
                   KACC_UVO_BEG     = K_UVO
                   KACC_UVO_END     = 0
              END IF
!
! ----------- Cycle over segments
!
              DRF_IF  = 0.0
              WEI_IF  = 0.0
              WW_IF   = 0.0
              SEFD_IF = 0.0
              K_UVO_LAST  = K_UVO
              NUM_USED_AP = 0
!
! ----------- Compute SRT_WRT_FRT_OFFS -- offset of the scan reference time with resepect
! ----------- to fringe reference  time
!
              UV_IND  = PIM%OBS(IND_OBS)%UV_IND(1,FRG_IND)
              SRT_WRT_FRT_OFFS = PIM%SUB%TIM_SRT(PIM%SUB%OBS_IND_SUB(IND_OBS),J10) - &
     &                           ( PIM%TIM_R8( PIM%UV_IND(UV_IND)%TIM_IND ) + PIM%OBS(IND_OBS)%FRT_OFFSET(IND_BND) )
              DO 4200 J20=1,N_SEG
!
! -------------- Check whether this segment corresponds to this scan
!
                 IF ( IND_SCA_SEG(J20) .NE. PIM%OBS(IND_OBS)%SCA_IND ) GOTO 4200
                 IF ( K_UVO > L_UVO ) THEN
                      IER = 0
                      CALL CLRCH ( STR )
                      CALL INCH  ( IND_OBS, STR )
                      CALL ERR_LOG ( 8443, IER, 'PIMA_SPLT', 'Trap of '// &
     &                    'inernal control: K_UVO > L_UVO while proecssing '// &
     &                    ' observation '//TRIM(STR)//' for source '//PIM%C_SOU(J4)// &
     &                    '. Nevertheless, continue ' )
                      GOTO 8200
                      RETURN 
                 END IF
!
! -------------- Initialization
!
                 SUM_WEI_SEG      = 0.0
                 SUMVIS_WEISEG    = 0.0
                 SUMVIS_WEISQ_SEG = 0.0
                 SUM_WEI_BAND     = 0.0 
                 NUMP_SEG         = 0
                 NUMP_BAND        = 0
                 NUMP_ALL         = 0
                 NUM_USED_AP      = 0
                 UVO(K_UVO)%TIM   = TIM_SEG(3,J20)
                 UVO(K_UVO)%IND_OBS = IND_OBS
!
! -------------- Cycle over AP (accumulation peroids) of this observation
!
                 IF ( INDEX ( STR_UV_PRINT, 'FIN1' ) > 0 ) THEN
                      CALL PIMA_UV_PRINT ( PIM, IND_OBS, 1, 'PS-FIN1' )
                    ELSE IF ( INDEX ( STR_UV_PRINT, 'FIN2' ) > 0 ) THEN
                      CALL PIMA_UV_PRINT ( PIM, IND_OBS, 2, 'PS-FIN2' )
                 END IF
!
                 UVO(K_UVO)%IND_OBS = IND_OBS
                 UVO(K_UVO)%IND_SEG = J20
                 UVO(K_UVO)%STA_IND = PIM%OBS(IND_OBS)%STA_IND
                 UVO(K_UVO)%IND_SUB = PIM%SUB%OBS_IND_SUB(IND_OBS)
!
                 DO 4210 J21=1,LTIM
                    IND_TIM = PIM%UV_IND(PIM%OBS(IND_OBS)%UV_IND(J21,FRG_IND))%TIM_IND
!
                    IF ( PIM%TIM_R8(IND_TIM) .GE. TIM_SEG(1,J20) - TIM_TOL .AND. &
     &                   PIM%TIM_R8(IND_TIM) .LE. TIM_SEG(2,J20) + TIM_TOL       ) THEN
!
! ---------------------- Get TIM_AP -- time tag to the AP with respect to the nominal 
! ---------------------- session start
!
                         TIM_AP = PIM%TIM_R8(IND_TIM)
!
! ---------------------- Update accumulators for the output time+frequency averaged visibilities
!
                         FL_AC_USE = .FALSE.
                         DO 4220 J22=1,LPOL 
                            IF ( PIM%OBS(IND_OBS)%WEI_1D(J21,J22) > PIM%CONF%FRIB_WEIGHTS_THRESHOLD ) THEN
                                 CALL ERR_PASS ( IUER, IER )
                                 CALL PIMA_UVO_UPDATE ( PIM, LFRQ, KFRQ, J22, LTIM, &
     &                                                  IND_OBS, J21, PIM%OBS(IND_OBS)%UV, &
     &                                                  PIM%OBS(IND_OBS)%WEI_1D(J21,J22)*WEI_FRQ_CHN, &
     &                                                  IND_FRQ, UVO(K_UVO), FRQ_ARR, &
     &                                                  TIM_AP, NUMP_SEG, &
     &                                                  SUMVIS_WEISEG, SUMVIS_WEISQ_SEG, &
     &                                                  SUM_WEI_SEG,   SUM_WEI_BAND, &
     &                                                  NUMP_BAND, DRF_IF, WEI_IF, WW_IF, IER )
                                 IF ( IER .NE. 0 ) THEN
                                      CALL ERR_LOG ( 8443, IUER, 'PIMA_SPLT', 'Failure '// &
     &                                    'to update the accummulator for the '// &
     &                                     STR(1:I_LEN(STR))//'th observation from '// &
     &                                    'input FITS-IDI file ' )
                                      IF ( PIM%CONF%SPLT_TOTAL_UV == PIMA__TOTAL_UV_YES ) THEN
                                           CLOSE ( UNIT=LUN_TOT )
                                      END IF
                                      RETURN
                                 END IF
                                 DO 4230 J23=1,KFRQ
                                    IF ( UVO(K_UVO)%SEFD(J23,J22) > PIMA__SEFD_MIN ) THEN
                                         FL_AC_USE = .TRUE.
                                         IF ( WEI_IF(J23,J22) > PIMA__AMP_MIN ) THEN
                                              NUM_USED_AP(J23,J22) = NUM_USED_AP(J23,J22) + 1
                                         END IF
                                    END IF
 4230                            CONTINUE 
                            END IF
 4220                    CONTINUE 
!
! ---------------------- Fill other fields of the UVO object
!
                         IF ( UVO(K_UVO)%TIM_IND(1) == 0 ) THEN
                              UVO(K_UVO)%TIM_IND(1) = PIM%UV_IND(PIM%OBS(IND_OBS)%UV_IND(J21,FRG_IND))%TIM_IND
                         END IF
                         UVO(K_UVO)%TIM_IND(2) = PIM%UV_IND(PIM%OBS(IND_OBS)%UV_IND(J21,FRG_IND))%TIM_IND
                         IND_SEG_LAST = J20
                         IND_AP_LAST  = J21
!
                         IF ( FL_WEI_OBS ) THEN
!
! --------------------------- Case when we compute ampltude and visibility rms 
! --------------------------- for weight computation over the interval that is
! --------------------------- different than the segment length interval
!
                              IF ( FL_AC_USE ) THEN
                                   IAC_SEG = IAC_SEG + 1
                              END IF
                              IF ( IAC_SEG == L_WEI_SEG  .OR. J21 == LTIM ) THEN
!
! -------------------------------- Well, we collected enough accumulation periods
! -------------------------------- for computing weights. Let us do it
!
                                   ACC_WEI_SEG(1:LPOL,1:KFRQ) = ACC_WEI_SEG(1:LPOL,1:KFRQ) + &
     &                                                              SUM_WEI_SEG(1:LPOL,1:KFRQ) 
                                   ACCVIS_WEISQ_SEG(1:LPOL,1:KFRQ) = ACCVIS_WEISQ_SEG(1:LPOL,1:KFRQ) + &
     &                                                                      SUMVIS_WEISQ_SEG(1:LPOL,1:KFRQ) 
                                   ACCVIS_WEI_SEG(1:LPOL,1:KFRQ) = ACCVIS_WEI_SEG(1:LPOL,1:KFRQ) + &
     &                                                                    SUMVIS_WEISEG(1:LPOL,1:KFRQ) 
                                   ACC_NAP(1:LPOL,1:KFRQ) = ACC_NAP(1:LPOL,1:KFRQ) + NUMP_SEG(1:LPOL,1:KFRQ)
                                   DO 4240 J24=1,LPOL
                                      DO 4250 J25=1,KFRQ
                                         IF ( UVO(K_UVO)%SEFD(J25,J24) > PIMA__SEFD_MIN ) THEN
                                              SEFD_IF(J25,J24) = UVO(K_UVO)%SEFD(J25,J24)
                                         END IF
                                         IF ( ACC_WEI_SEG(J24,J25) > PIMA__WEI_MIN .AND. &
     &                                        ACC_NAP(J24,J25) > 0                       ) THEN
!
! ------------------------------------------- Compute the weighted amplitude and weighted variance
! ------------------------------------------- of the amplitudes averaged over the segment
!
                                              ACC_AMPL(J24,J25) = ABS(ACCVIS_WEI_SEG(J24,J25))/ACC_WEI_SEG(J24,J25)
                                              ACC_WEI(J24,J25,NUM_SEG_WEI) = ( ACCVIS_WEISQ_SEG(J24,J25)/ACC_WEI_SEG(J24,J25)  - &
     &                                                                         ACC_AMPL(J24,J25)**2 )/ &
     &                                                                         ACC_WEI_SEG(J24,J25)
                                            ELSE 
                                              ACC_AMPL(J24,J25) = 0.0
                                              ACC_WEI(J24,J25,NUM_SEG_WEI) = 0.0
                                         END IF
 4250                                 CONTINUE 
 4240                              CONTINUE 
                                   IF ( NUM_SEG_WEI < N_SEG ) THEN
!
! ------------------------------------- Update the counter of the number of segments
! ------------------------------------- for weights and re-initialize accumulators
!
                                        NUM_SEG_WEI      = NUM_SEG_WEI + 1
                                        ACC_WEI_SEG      = 0.0
                                        ACCVIS_WEISQ_SEG = 0.0
                                        ACCVIS_WEI_SEG   = 0.0
                                        ACC_NAP          = 0
                                        IAC_SEG = 0
                                   END IF
                              END IF
                         END IF
                    END IF
 4210            CONTINUE 
!
                 SUM_WEI_ALL = 0.0
                 NUMP_ALL = 0
                 DO 4260 J26=1,LPOL
                    SUM_WEI_ALL = SUM_WEI_ALL + SUM_WEI_BAND(J26)
                    NUMP_ALL    = NUMP_ALL    + NUMP_BAND(J26)
 4260            CONTINUE 
!
                 IF ( SUM_WEI_ALL > PIMA__WEI_MIN ) THEN
!
! ------------------- We see that the J20-th segment contributed 
! ------------------- to time+frequency averaged UVO
!
                      IF ( PIM%CONF%DEBUG_LEVEL .GE. 8 ) THEN
                           CALL FLUSH ( 6 ) 
                           WRITE ( 6, 1000 ) IND_OBS, K_UVO, UVO(K_UVO)%TIM, &
     &                                       NUMP_SEG(LPOL,1), UVO(K_UVO)%WEI(1,LPOL), &
     &                                       PIM%TIM_R8(PIM%UV_IND(PIM%OBS(IND_OBS)%UV_IND(IND_AP_USED(1),FRG_IND))%TIM_IND), &
     &                                       PIM%TIM_R8(PIM%UV_IND(PIM%OBS(IND_OBS)%UV_IND(IND_AP_USED(2),FRG_IND))%TIM_IND), &
     &                                       TIM_SEG(3,J20)
 1000                      FORMAT ( ' PIMA_SPLT IND_OBS: ', I5, ' NSEG= ', I6, &
     &                              ' TIM: ', F8.2, ' NUMP_1: ', I6, &
     &                              ' Wei: ', F12.5, ' Tims: ', F8.2, 1X, F8.2, ' TS3: ', F8.2 )
                      END IF
!
                      DO 4270 J27=1,LPOL
!
! ---------------------- Compute time+frequency averaged spectrum using accumulators
!
                         DO 4280 J28=1,KFRQ
                            IF ( UVO(K_UVO)%WEI(J28,J27) > 0.0 ) THEN
                                 UVO(K_UVO)%SPE(J28,J27) = UVO(K_UVO)%SPE(J28,J27)/UVO(K_UVO)%WEI(J28,J27) 
                            END IF
                            IF ( NUMP_SEG(J27,J28) > 0 ) THEN
                                 UVO(K_UVO)%WEI(J28,J27) = UVO(K_UVO)%WEI(J28,J27)/NUMP_SEG(J27,J28)
                            END IF
!
                            IF ( LPOL .GE. 2 .AND. PIMA__POLAR_CODE(POLC_INDS(J27)) == PIMA__POLAR_LL ) THEN
                                 FEED_ANG_DIF = PIM%OBS(IND_OBS)%FEED_ANG(1) - &
     &                                          PIM%OBS(IND_OBS)%FEED_ANG(2) 
                                 UVO(K_UVO)%SPE(J28,J27) = UVO(K_UVO)%SPE(J28,J27)* &
     &                                                     CMPLX( COS(2.0*FEED_ANG_DIF), &
     &                                                            SIN(2.0*FEED_ANG_DIF) )
                            END IF
!
! ------------------------- This is done for compatibility with Difmap. It expects
! ------------------------- to find visibilities  at baselines with order sta_ind(1) < sta_ind(2)
!
                            IF ( UVO(K_UVO)%STA_IND(1) > UVO(K_UVO)%STA_IND(2) ) THEN
!
! ------------------------------ Wrong order of stations in the baseline? 
! ------------------------------ Conjugate visibility and then later fix the order
!
                                 UVO(K_UVO)%SPE(J28,J27) = CONJG ( UVO(K_UVO)%SPE(J28,J27) )
                            END IF
!
                            IF ( FL_BPASS ) THEN
                                 IF ( PIM%BPASS(1)%PIMA_VERS .GE. PIMA__BPS_AMP_VERS ) THEN
!
! ----------------------------------- Renormalization of fringe amplitude
!
                                      IF ( RENRML_AUTO_IF(IND_FRQ(4,J28),1,J27) > PIMA__AMP_MIN .AND. &
     &                                     RENRML_AUTO_IF(IND_FRQ(4,J28),2,J27) > PIMA__AMP_MIN       ) THEN
                                           UVO(K_UVO)%SPE(J28,J27) = UVO(K_UVO)%SPE(J28,J27)/ &
     &                                                    DSQRT(RENRML_AUTO_IF(IND_FRQ(4,J28),1,J27)* &
     &                                                          RENRML_AUTO_IF(IND_FRQ(4,J28),2,J27)  )
                                      END IF
                                    ELSE IF ( PIM%BPASS(1)%PIMA_VERS < PIMA__BPS_AMP_VERS .AND.  &
     &                                         PIM%CONF%FRIB_AUTOCORR_CALIB .NE. PIMA__ACCR_NO   ) THEN
!
! ------------------------------------ Obsolete bandpass style
!
                                       RENRML_AUTO_IF(J28,1,J27) = 1.0D0
!
! ------------------------------------ Renormalization of fringe amplitude
!
                                       UVO(K_UVO)%SPE(J28,J27) = UVO(K_UVO)%SPE(J28,J27)/RENRML_AUTO_IF(J28,1,J27)
                                 END IF
                            END IF
 4280                    CONTINUE
!
                         DO 4290 J29=1,KFRQ
                            IF ( SUM_WEI_SEG(J27,J29) > PIMA__WEI_MIN .AND. NUMP_SEG(J27,J29) > 0 ) THEN
!
! ------------------------------ Now we compute weighted amplitude, phase and square of 
! ------------------------------ amplitude standard deviation for a given segment
!
                                 AMPL = ABS(SUMVIS_WEISEG(J27,J29))/SUM_WEI_SEG(J27,J29)
                                 PHAS = PHAS_CMPL_R4 ( SUMVIS_WEISEG(J27,J29) )
                                 SIG_SQ = SUMVIS_WEISQ_SEG(J27,J29)/SUM_WEI_SEG(J27,J29) - AMPL**2
!
                                 IF ( PIM%CONF%DEBUG_LEVEL .GE. 6 ) THEN
                                      TIM_EFF_SEG  = PIM%OBS(IND_OBS)%AP_LEN * PIM%CONF%SPLT_TIM_MSEG * &
     &                                               SUM_WEI_SEG(J27,J29)/NUMP_SEG(J27,J29)
                                      TIM_EFF_BAND = PIM%OBS(IND_OBS)%EFF_DUR(IND_BND) 
                                      WRITE ( 6, 1110 ) K_UVO, J27, J29, UVO(K_UVO)%IND_OBS, AMPL, PHAS, &
     &                                                  SQRT ( SIG_SQ/NUMP_SEG(J27,J29) ), &
     &                                                  AMPL/( SNR* DSQRT ( TIM_EFF_SEG/TIM_EFF_BAND ) ), &
     &                                                  NUMP_SEG(J27,J29), UVO(K_UVO)%WEI(J29,J27), &
     &                                                  NUM_USED_AP(J29,J27)
 1110                                 FORMAT ( ' PIMA_SPLT uvo ', I6, ' Lpol: ', I1, &
     &                                         ' Kfrq: ', I4, ' Ind_obs: ', I5, &
     &                                         ' Ampl: ', F9.6, ' Jy Phas: ', F8.5, &
     &                                         ' rad  Amp_rms(from scatter): ', F8.5, &
     &                                         ' Jy  Amp_rms(from SNR):     ', F8.5, &
     &                                         ' Jy  Num_seg: ', I5, ' Wei: ', F12.5, &
     &                                         ' Num_used_ap: ', I5 )
                                 END IF
                            END IF
!
                            IF ( SUM_WEI_SEG(J27,J29) > PIMA__WEI_MIN .AND. &
     &                           NUMP_SEG(J27,J29) > 0                .AND. &
                                 ( ( PIM%CONF%SPLT_WEIGHT_TYPE == PIMA__SPLT_WEI_SEG_RMS .AND.  &
                                     NUMP_SEG(J27,J29)/PIM%NCHN .GE.  NAP__SIG )     .OR.   &
                                   ( PIM%CONF%SPLT_WEIGHT_TYPE == PIMA__SPLT_WEI_AUTO .AND. &
                                     NUMP_SEG(J27,J29)/PIM%NCHN .GE. L_WEI_SEG              ) ) ) THEN
!
! ------------------------------ We have enough APs in a segment in order to compute the 
! ------------------------------ rms of visibility scatter within this UVO. We use this value
! ------------------------------ as the best estimate of amplitude uncertainty.
! ------------------------------ NB: we divide the rms on the square root of the number of 
! ------------------------------ samples in the segment since we are interested in standard 
! ------------------------------ deviation (square root of the variance) of the segment amplitude
!
                                 UVO(K_UVO)%SNR(J29,J27) = AMPL/SQRT( SIG_SQ/NUMP_SEG(J27,J29) )
!
! ------------------------------ We scale the weights to the total smearing
! ------------------------------ NB: FITS-IDI convention: weights are reciprocal to the variance
!
                                 UVO(K_UVO)%WEI(J29,J27) = 1.0D0/(SIG_SQ/NUMP_SEG(J27,J29))*(DECOR_BS * DECOR_TS)**2
                               ELSE IF ( SUM_WEI_SEG(J27,J29) > PIMA__WEI_MIN      .AND. &
     &                                   NUMP_SEG(J27,J29) > 0                     .AND. &
                                         PIM%CONF%SPLT_WEIGHT_TYPE == PIMA__SPLT_WEI_ONE ) THEN
!
! ------------------------------ We set weight to unity (except a patological case when there are no data)
!
                                 IF ( NUMP_SEG(J27,J29) > 0 .AND. &
     &                                SUM_WEI_SEG(J27,J29) > PIMA__WEI_MIN ) THEN
                                      UVO(K_UVO)%WEI(J29,J27) = 1.0
                                      UVO(K_UVO)%SNR(J29,J27) = 1.0
                                    ELSE 
                                      UVO(K_UVO)%WEI(J29,J27) = 0.0
                                      UVO(K_UVO)%SNR(J29,J27) = 0.0
                                 END IF
                               ELSE 
!
! ------------------------------ Scale the SNR over the observation for 1) less effective time; 2) less bandwidth
!
                                 IF ( NUMP_SEG(J27,J29) > 0 .AND. &
     &                                SUM_WEI_SEG(J27,J29) > PIMA__WEI_MIN  .AND. &
     &                                PIM%OBS(IND_OBS)%EFF_DUR(IND_BND) > 0.999*PIM%OBS(IND_OBS)%AP_LEN ) THEN
!
                                      TIM_EFF_SEG  = PIM%OBS(IND_OBS)%AP_LEN * PIM%CONF%SPLT_TIM_MSEG * &
     &                                               SUM_WEI_SEG(J27,J29)/NUMP_SEG(J27,J29)
                                      TIM_EFF_BAND = PIM%OBS(IND_OBS)%EFF_DUR(IND_BND) 
!
                                      UVO(K_UVO)%SNR(J29,J27) = SNR* DSQRT ( TIM_EFF_SEG/TIM_EFF_BAND )
                                      UVO(K_UVO)%WEI(J29,J27) = 1.0D0/(PIM%OBS(IND_OBS)%AMPL(PIMA__DRF,IND_BND)/UVO(K_UVO)%SNR(J29,J27))**2
                                    ELSE 
!
! ----------------------------------- No data contributed to this segement
!
                                      UVO(K_UVO)%WEI(J29,J27) = 0.0
                                      UVO(K_UVO)%SNR(J29,J27) = 0.0
                                 END IF
                            END IF
! 
                            IF ( PIM%CONF%DEBUG_LEVEL .GE. 6 ) THEN
                                 WRITE ( 6, 1120 ) J27, IND_OBS, K_UVO, J29, &
     &                                   PIM%TAI_0 + UVO(K_UVO)%TIM + PIM%UTC_MTAI, &
     &                                   ABS( UVO(K_UVO)%SPE(J29,J27) ), &
     &                                   PHAS_CMPL_R4 ( UVO(K_UVO)%SPE(J29,J27) ), &
     &                                   UVO(K_UVO)%WEI(J29,J27), 1 
 1120                            FORMAT ( ' PIMA_SPLT Pol: ',I1, ' Ind_obs: ', I5, &
     &                                    ' K_uvo: ', I6, ' Ifrq: ', I4, &
     &                                    '  UTC= ', F9.2, ' Ampl= ', F9.6, &
     &                                    ' Jy ', ' Phas= ', F9.6, ' rad Wei_IF= ', 1PD12.6, 1X, I1 ) 
!                                 WRITE ( 6, * ) 'PIMA_SPLT: ampl, snr ', PIM%OBS(IND_OBS)%AMPL(PIMA__DRF,IND_BND), SNR, &
!     &                                          ' tims = ', TIM_EFF_SEG, TIM_EFF_BAND, &
!     &                                          ' a1234= ', PIM%OBS(IND_OBS)%AP_LEN, PIM%CONF%SPLT_TIM_MSEG, &
!     &                                                      SUM_WEI_SEG(J27,J29), NUMP_SEG(J27,J29)
                            END IF
 4290                    CONTINUE
 4270                 CONTINUE 
!
                      IF ( FL_WEI_OBS ) THEN
!
! ------------------------ Update KACC_UVO_END -- the last segment used for weights computation
! ------------------------ Update IND_ACC_UVO  -- the cross reference for the weight segment index
!
                           KACC_UVO_END = K_UVO
                           IND_ACC_UVO(K_UVO) = NUM_SEG_WEI
                      END IF
!
! ------------------- Compute UVW coordinates of the baseline projection and 
! ------------------- apply gain correction
!
                      CALL ERR_PASS ( IUER, IER )
                      CALL VTD_DELAY ( PIM%C_SOU(PIM%OBS(IND_OBS)%SOU_IND), &
     &                                 PIM%C_STA(PIM%OBS(IND_OBS)%STA_IND(1)), &
     &                                 PIM%C_STA(PIM%OBS(IND_OBS)%STA_IND(2)), &
     &                                 PIM%MJD_0, PIM%TAI_0 + UVO(K_UVO)%TIM, &
     &                                 OBS_TYP, VTD, THE_GR_DEL, THE_RATE, &
     &                                 DER_DEL, DER_RAT, IER )
                      IF ( IER .NE. 0 ) THEN
                           CALL CLRCH ( STR )
                           CALL INCH  ( IND_OBS, STR )
                           WRITE ( 6, * ) ' K_UVO= ', K_UVO, ' UVO(K_UVO)%TIM= ', UVO(K_UVO)%TIM
                           CALL ERR_LOG ( 8444, IUER, 'PIMA_SPLT', 'Error in '// &
     &                         'an attempt to compute theoretical path delay for '// &
     &                         'the '//STR(1:I_LEN(STR))//' th observation from '// &
     &                          'input FITS-IDI file ' )
                           IF ( PIM%CONF%SPLT_TOTAL_UV == PIMA__TOTAL_UV_YES ) THEN
                                CLOSE ( UNIT=LUN_TOT )
                           END IF
                           RETURN
                      END IF
!
                      CALL ERR_PASS ( IUER, IER )
                      CALL VTD_GET_UVW ( PIM%C_SOU(PIM%OBS(IND_OBS)%SOU_IND), &
     &                                   PIM%C_STA(PIM%OBS(IND_OBS)%STA_IND(1)), &
     &                                   PIM%C_STA(PIM%OBS(IND_OBS)%STA_IND(2)), &
     &                                   PIM%MJD_0, PIM%TAI_0 + UVO(K_UVO)%TIM, &
     &                                   PIM%FREQ_ARR(1,PIM%CONF%BEG_FRQ,PIM%CONF%FRQ_GRP), &
     &                                   VTD, UVO(K_UVO)%UVW(1), IER )
                      IF ( IER .NE. 0 ) THEN
                           CALL CLRCH ( STR )
                           CALL INCH  ( IND_OBS, STR )
                           CALL ERR_LOG ( 8445, IUER, 'PIMA_SPLT', 'Error in '// &
     &                         'an attempt to compute UVW for the '// &
     &                          STR(1:I_LEN(STR))//' th observation from '// &
     &                          'input FITS-IDI file ' )
                           IF ( PIM%CONF%SPLT_TOTAL_UV == PIMA__TOTAL_UV_YES ) THEN
                                CLOSE ( UNIT=LUN_TOT )
                           END IF
                           RETURN
                      END IF
                      IF ( PIM%OBS(IND_OBS)%STA_IND(1) < PIM%OBS(IND_OBS)%STA_IND(2) ) THEN
                           UVO(K_UVO)%UVW(1:3) =  UVO(K_UVO)%UVW(1:3)/PIM%FREQ_ARR(1,PIM%CONF%BEG_FRQ,PIM%CONF%FRQ_GRP)
                         ELSE
!
! ------------------------ Wrong order? Let us fix the order
!
                           SWAP_I4 = UVO(K_UVO)%STA_IND(1)
                           UVO(K_UVO)%STA_IND(1) = UVO(K_UVO)%STA_IND(2) 
                           UVO(K_UVO)%STA_IND(2) = SWAP_I4
                           UVO(K_UVO)%UVW(1:3)   = -UVO(K_UVO)%UVW(1:3)/PIM%FREQ_ARR(1,PIM%CONF%BEG_FRQ,PIM%CONF%FRQ_GRP)
                      END IF
!
                      K_UVO = K_UVO + 1
                 END IF
 4200         CONTINUE 
 8200         CONTINUE 
!
              SEFD_TOTAL = 1.0D0
              IFRQ = 0
              K_SEFD  = 0
              K_GAI   = 0
              GAIN_TOTAL = 1.0D0
              DRF_ALL = 0.0
              WEI_ALL = 0.0
              DO 4300 J30=1,LPOL
                 DO 4310 J31=1,KFRQ
                    IF ( LPOL == 1 .OR.                         &
     &                   LPOL == 2 .OR.                         &
     &                   LPOL == 4 .AND. J30 == 1 .OR. J30 == 4 ) THEN
                         WEI_ALL = WEI_ALL + WEI_IF(J31,J30)
                         WW_ALL  = WW_ALL  + WW_IF(J31,J30)
                         DRF_ALL = DRF_ALL + DRF_IF(J31,J30)* &
     &                                           PIM%GACO(PIM%OBS(IND_OBS)%STA_IND(1))%GAIN_CORR(J31)* &
     &                                           PIM%GACO(PIM%OBS(IND_OBS)%STA_IND(2))%GAIN_CORR(J31)
                    END IF
!
                    IF ( WEI_IF(J31,J30) > PIMA__WEI_MIN  ) THEN
                         IF ( PIM%CONF%SPLT_TOTAL_UV == PIMA__TOTAL_UV_YES ) THEN
!
! --------------------------- Printing the table of amplitudes and phases referred to the 
! --------------------------- reference frequency
!
                              STR = MJDSEC_TO_DATE ( PIM%MJD_0,   PIM%TAI_0 &
     &                                                          + PIM%OBS(IND_OBS)%TIM_BEG  &
     &                                                          + PIM%OBS(IND_OBS)%FRT_OFFSET(1) &
     &                                                          + PIM%UTC_MTAI, -2 )
                              IF ( IS_R4_NAN ( REAL(DRF_IF(J31,J30)) ) .OR. &
     &                             IS_R4_NAN ( IMAG(DRF_IF(J31,J30)) )      ) THEN
                                   DRF_IF(J31,J30) = 0.0
                              END IF
                              WRITE ( LUN_TOT, 1131 ) PIM%SOU(J4)%J2000_NAME, &
                                                      PIM%C_STA(PIM%OBS(IND_OBS)%STA_IND(1)), & 
     &                                                PIM%C_STA(PIM%OBS(IND_OBS)%STA_IND(2)), &
     &                                                STR(1:23), IND_OBS, J31, J30, &
     &                                                ABS(DRF_IF(J31,J30)/WEI_IF(J31,J30))* &
     &                                                PIM%GACO(PIM%OBS(IND_OBS)%STA_IND(1))%GAIN_CORR(J31)* &
     &                                                PIM%GACO(PIM%OBS(IND_OBS)%STA_IND(2))%GAIN_CORR(J31), &
     &                                                PHAS_CMPL_R4(DRF_IF(J31,J30)), SEFD_IF(J31,J30), &
     &                                                UVO(K_UVO-1)%UVW(1:2)*PIM%FREQ_ARR(1,PIM%CONF%BEG_FRQ,PIM%CONF%FRQ_GRP)
 1131                         FORMAT ( 'Sou: ', A, ' Bas: ', A, 1X, A, &
     &                                 ' UTC_epoch: ' ,A, ' Ind_obs: ', I5, ' I_frq: ', I2, &
     &                                 ' I_pol: ', I1, ' Ampl: ', F14.7, ' Phase: ', F9.6, ' SEFD: ', F9.2, &
     &                                 ' UV: ', 1PD13.6, 1X, 1PD13.6 )
                         END IF
                         IFRQ = IFRQ + 1
                         IF ( SEFD_IF(J31,J30) > PIMA__SEFD_MIN ) THEN
                              SEFD_TOTAL = SEFD_TOTAL * SEFD_IF(J31,J30)
                              K_SEFD = K_SEFD + 1
                         END IF
                         IF ( PIM%GACO(PIM%OBS(IND_OBS)%STA_IND(1))%GAIN_CORR(J31)* &
     &                        PIM%GACO(PIM%OBS(IND_OBS)%STA_IND(2))%GAIN_CORR(J31)  > PIMA__WEI_MIN ) THEN
                              GAIN_TOTAL = GAIN_TOTAL*PIM%GACO(PIM%OBS(IND_OBS)%STA_IND(1))%GAIN_CORR(J31)* &
     &                                                PIM%GACO(PIM%OBS(IND_OBS)%STA_IND(2))%GAIN_CORR(J31)
                              K_GAI = K_GAI + 1
                         END IF
                    END IF
 4310            CONTINUE 
 4300         CONTINUE 
!
              IF ( IS_R4_NAN ( REAL(DRF_ALL) ) .OR. &
     &             IS_R4_NAN ( IMAG(DRF_ALL) )      ) THEN
                   DRF_ALL = 0.0
              END IF
              IF ( WEI_ALL > PIMA__WEI_MIN  ) THEN
!
! ---------------- NB: at the moment, statistics is computed only for the 1st polarization
!
                   IF ( K_SEFD > 0 ) THEN
                        SEFD_TOTAL = SEFD_TOTAL**(1.0D0/K_SEFD )
                      ELSE
                        SEFD_TOTAL = 1.0D0
                   END IF
                   IF ( K_GAI > 0 ) THEN
                        GAIN_TOTAL = GAIN_TOTAL**(1.0D0/K_GAI)
                      ELSE
                        GAIN_TOTAL = 1.0D0
                   END IF
                   IF ( PIM%OBS(IND_OBS)%NOISE(1) > PIMA__AMP_MIN ) THEN
                        NOI = PIM%OBS(IND_OBS)%NOISE(1) * &
     &                        NOI_BEAM * &
     &                        RENRML_CROS_IF_ALL(1)
                        SNR_TOT = ABS(DRF_ALL/WEI_ALL)/NOI/SEFD_TOTAL/GAIN_TOTAL
                      ELSE 
                        SNR_TOT = PIMA__AMP_MIN
                   END IF
                   IF ( FL_SPLT_SNR_FRI_ONLY ) THEN
                        SNR_TOT = SNR
                   END IF
                   IF ( PIM%CONF%SPLT_TOTAL_UV == PIMA__TOTAL_UV_YES ) THEN
                        STR = MJDSEC_TO_DATE ( PIM%MJD_0,   PIM%TAI_0 &
     &                                                    + PIM%OBS(IND_OBS)%TIM_BEG  &
     &                                                    + PIM%OBS(IND_OBS)%FRT_OFFSET(1) &
     &                                                    + PIM%UTC_MTAI, -2 )
                        WRITE ( LUN_TOT, 1140 ) PIM%SOU(J4)%J2000_NAME,                     &
                                                PIM%C_STA(PIM%OBS(IND_OBS)%STA_IND(1)),     & 
     &                                          PIM%C_STA(PIM%OBS(IND_OBS)%STA_IND(2)),     &
     &                                          STR(1:23), IND_OBS,                         &
     &                                          PIM%FREQ_ARR(1,PIM%CONF%BEG_FRQ,PIM%CONF%FRQ_GRP), &
     &                                          ABS(DRF_ALL/WEI_ALL),                       &
     &                                          PHAS_CMPL_R4(DRF_ALL), SNR_TOT, SEFD_TOTAL, &
     &                                          DSQRT ( UVO(K_UVO-1)%UVW(1)**2 + UVO(K_UVO-1)%UVW(2)**2 )* &
     &                                          PIM%FREQ_ARR(1,PIM%CONF%BEG_FRQ,PIM%CONF%FRQ_GRP)
 1140                   FORMAT ( 'SOU: ', A, ' Bas: ', A, 1X, A, &
     &                           ' UTC_epoch: ', A, ' Ind_obs: ', I5, ' Ref_freq: ', 1PD13.6, &
     &                           ' Total_ampl: ', 0PF14.7, ' Total_phase: ', 0PF9.6, ' SNR: ', 0PF9.2, &
     &                           ' Total_SEFD: ', 0PF9.2, ' UV_rad: ', 1PD13.6 )
                   END IF
                 ELSE
                   SNR_TOT = 0.0
              END IF
!
              IF ( PIM%CONF%DEBUG_LEVEL .GE. 2 ) THEN
                   IF ( FL_USE_OBS_ORIG(IND_OBS) ) THEN
                        STR(1:1) = ' '  
                        IF ( SNR_TOT > SNR * SNR_SHR_WARN ) STR(1:1) = '/'
                        IF ( SNR_TOT < SNR / SNR_SHR_WARN ) STR(1:1) = '\'
                      ELSE
                        STR(1:2) = '@ '
                   END IF
                   IF ( PIM%OBS(IND_OBS)%RES_GR_RAT(IND_BND) > -0.5D0 ) THEN
                        WRITE ( 6, 180 ) PIM%C_SOU(J4), IND_OBS, &
     &                                   PIM%C_STA(PIM%OBS(IND_OBS)%STA_IND(1)), & 
     &                                   PIM%C_STA(PIM%OBS(IND_OBS)%STA_IND(2)), &
     &                                   PIM%SUB%OBS_IND_SUB(IND_OBS), SNR, SNR_TOT, &
     &                                   STR(1:1), SRT_WRT_FRT_OFFS, &
     &                                   1.D9* (PIM%OBS(IND_OBS)%RES_MB_DEL(IND_FRA,1) - OLD_GR_DEL(IND_OBS)), &
     &                                   1.D12*(PIM%OBS(IND_OBS)%RES_PH_RAT(IND_FRA,1) - OLD_PH_RAT(IND_OBS)), &
     &                                   1.D12*(PIM%OBS(IND_OBS)%RES_GR_RAT(1)         - OLD_GR_RAT(IND_OBS)), &
     &                                   GAIN_TOTAL
                      ELSE
                        WRITE ( 6, 180 ) PIM%C_SOU(J4), IND_OBS, &
     &                                   PIM%C_STA(PIM%OBS(IND_OBS)%STA_IND(1)), & 
     &                                   PIM%C_STA(PIM%OBS(IND_OBS)%STA_IND(2)), &
     &                                   PIM%SUB%OBS_IND_SUB(IND_OBS), SNR, SNR_TOT, &
     &                                   STR(1:1), SRT_WRT_FRT_OFFS, &
     &                                   1.D9* (PIM%OBS(IND_OBS)%RES_MB_DEL(IND_FRA,1) - OLD_GR_DEL(IND_OBS)), &
     &                                   1.D12*(PIM%OBS(IND_OBS)%RES_PH_RAT(IND_FRA,1) - OLD_PH_RAT(IND_OBS)), &
     &                                   GAIN_TOTAL
                   END IF
 180               FORMAT ( 'PIMA_SPLT Sou: ', A, ' Obs: ', I5, &
     &                      ' Bas: ', A, ' / ', A, ' Sa: ', I2, &
     &                      ' SNR_frib: ', F7.1, ' SNR_splt: ', F7.1, 2X, A, &
     &                      ' SF_offs: ', F7.2,             &
     &                      ' Gr_del_dif: ', F9.3, ' ns',   &
     &                      ' Ph_rat_dif: ', F9.3, ' ps/s', &
     &                      ' Gr_rat_dif: ', F9.3, ' ps/s ', &
     &                      ' Gaco_tot: ', F9.5  )
                   CALL FLUSH ( 6 )
              END IF
!
! ----------- Put the total SNR to the UVO objects that belong to this observation
!
              DO 4320 J32=K_UVO_LAST,K_UVO-1
                 UVO(J32)%SNR_FRI = SNR
                 UVO(J32)%SNR_TOT = SNR_TOT
 4320         CONTINUE 
!
              IF ( FL_WEI_OBS .AND. KACC_UVO_END > 0 ) THEN
!
! ---------------- We had the situation that the lenght of segements for 
! ---------------- UV computation and for weight computation is different.
! ---------------- We will recompute weights
!
                   DO 4330 J33=KACC_UVO_BEG,KACC_UVO_END
                      DO 4340 J34=1,LPOL
                         DO 4350 J35=1,KFRQ
                            IF ( IND_ACC_UVO(J33) .GE.  NUM_SEG_WEI  .AND. &
     &                           ACC_NAP(J34,J35) == 0               .AND. &
     &                           J33 > 1                             .AND. &
     &                           ACC_NAP(J34,J35) == 0 ) THEN
!
! ------------------------------ If case when the last weight segment is empty, take
! ------------------------------ the weights from the previous segment
!
                                 IF ( IND_ACC_UVO(J33-1) > 0 ) THEN
                                      IND_USED_ACC = IND_ACC_UVO(J33-1)
                                    ELSE 
                                      IND_USED_ACC = IND_ACC_UVO(J33)
                                 END IF
                               ELSE 
                                 IND_USED_ACC = IND_ACC_UVO(J33) 
                                 IF ( IND_USED_ACC > N_SEG ) IND_USED_ACC = N_SEG
                            END IF
!
                            IF ( ACC_WEI(J34,J35,IND_USED_ACC) < PIMA__WEI_MIN**2 ) THEN
                                 UVO(J33)%WEI(J35,J34) = 0.0D0
                               ELSE 
                                 IF ( PIM%CONF%SPLT_WEIGHT_TYPE == PIMA__SPLT_WEI_OBS_RMS .OR. &
     &                                PIM%CONF%SPLT_WEIGHT_TYPE == PIMA__SPLT_WEI_AUTO         ) THEN
!
! ----------------------------------- Weights are reciprocal to the visibility variance  !! Mulitplied by normalized weight sum
!
                                      UVO(J33)%WEI(J35,J34) = 1.0D0/ACC_WEI(J34,J35,IND_USED_ACC)
                                    ELSE IF ( PIM%CONF%SPLT_WEIGHT_TYPE == PIMA__SPLT_WEI_OBS_SNR ) THEN
!
! ----------------------------------- Weights are reciprocal to the obserevation-averaged square of noise rms  !! Multiply by the normalized weight sum
!
                                      UVO(J33)%WEI(J35,J34) = (2.D0/PI__NUM)/ &
     &                                                        (KFRQ*(ACC_AMPL(J34,J35)/SNR))**2
                                 END IF
                            END IF
 4350                    CONTINUE 
 4340                 CONTINUE 
 4330              CONTINUE 
                 ELSE IF ( FL_WEI_OBS .AND. KACC_UVO_END == 0 ) THEN
                   CONTINUE 
              END IF    
              IF ( FL_WEI_OBS ) THEN
                   DEALLOCATE ( ACC_WEI          )
                   DEALLOCATE ( ACC_WEI_SEG      )
                   DEALLOCATE ( ACCVIS_WEI_SEG   )
                   DEALLOCATE ( ACCVIS_WEISQ_SEG )
                   DEALLOCATE ( IND_ACC_UVO      )
                   DEALLOCATE ( ACC_NAP          )
                   DEALLOCATE ( ACC_AMPL         )
              END IF
!
              IF ( PIM%CONF%DEBUG_LEVEL .GE. 5 ) THEN
                   CALL CLRCH ( STRC ) 
                   CALL CLRCH ( STRA ) 
                   WRITE ( UNIT=STRC, FMT='(1024(F7.5,1X))' ) RENRML_CROS_IF(1:LFRQ,1)
                   WRITE ( UNIT=STRA, FMT='(1024(F7.5,1X))' ) DSQRT(RENRML_AUTO_IF(1:LFRQ,1,1)* &
     &                                                              RENRML_AUTO_IF(1:LFRQ,2,1))
                   WRITE ( 6, 1150 ) PIM%C_SOU(J4), IND_OBS, &
     &                               PIM%C_STA(PIM%OBS(IND_OBS)%STA_IND(1)), & 
     &                               PIM%C_STA(PIM%OBS(IND_OBS)%STA_IND(2)), &
     &                               NRML_BEAM_ATT(1), NRML_BEAM_ATT(2), LFRQ, KFRQ, &
     &                               RENRML_CROS_IF_ALL(1), TRIM(STRC), TRIM(STRA)
 1150              FORMAT ( 'PIMA_SPLT renoromalization ',A, 2X, &
     &                       I5, 2X, A, 1X, A, &
     &                      ' NRML_beam: ', F7.5, 1X, F7.5, &
     &                      ' Num_frq: ', I2, ' Num_seg_frq: ', I4, &
     &                      ' NRML_cross_com: ', F7.5, &
     &                      ' NRML_cross: ', A, &
     &                      ' NRML_autc_frng: ', A )
              END IF
              IF ( FL_TIMER ) THEN
                   CALL WALL_TIMER ( STR )
                   WRITE ( 6, '(A,I5,1X,A)' ) 'Processed  observation ', IND_OBS, STR(1:27)
              END IF
 4110       CONTINUE 
 4100    CONTINUE 
!
         DO 4360 J36=1,L_UVO
            DO 4370 J37=1,KFRQ
               IF ( UVO(J36)%STA_IND(1) > 0 .AND. UVO(J36)%STA_IND(2) > 0 ) THEN
                    DO 4380 J38=1,LPOL
                       IF ( PIM%GACO_STATUS == PIMA__LOADED ) THEN
!
                            GAIN_CORR = PIM%GACO(UVO(J36)%STA_IND(1))%GAIN_CORR(J37)* &
     &                                  PIM%GACO(UVO(J36)%STA_IND(2))%GAIN_CORR(J37)
                            UVO(J36)%SPE(J37,J38) = UVO(J36)%SPE(J37,J38) * GAIN_CORR
                            IF ( GAIN_CORR == 0.0D0 ) THEN
                                 UVO(J36)%WEI(J37,J38) = 0.0D0
                               ELSE
                                 UVO(J36)%WEI(J37,J38) = UVO(J36)%WEI(J37,J38) / GAIN_CORR
                            END IF
                       END IF
                       IF ( PIM%CONF%DEBUG_LEVEL .GE. 6 ) THEN
                            WRITE ( 6, 1120 ) J38, IND_OBS, J36, J37, &
     &                              PIM%TAI_0 + UVO(J36)%TIM + PIM%UTC_MTAI, &
     &                              ABS( UVO(J36)%SPE(J37,J38) ), &
     &                              PHAS_CMPL_R4 ( UVO(J36)%SPE(J37,J38) ), &
     &                              UVO(J36)%WEI(J37,J38), 2
                       END IF
 4380               CONTINUE 
               END IF
 4370       CONTINUE 
 4360    CONTINUE 
!
         IF ( K_UVO > 1 ) THEN
              K_UVO = K_UVO - 1
!
! ----------- Sort UVO objects first over time, then over the first station index,
! ----------- then over the second station index
!
              CALL FOR_QSORT ( UVO, K_UVO, SIZEOF(UVO(1)), PIMA_COMPAR_UVO )
!
              IF ( PIM%L_SUB > 1                                         .AND. &
     &             PIM%CONF%SPLT_SUBARRAY_CONSOLIDATION .NE. PIMA__SA_NO       ) THEN
!
! ---------------- Consolidate sub-arrays
!
                   CALL ERR_PASS ( IUER, IER )
                   CALL PIMA_SPLT_CONSOL ( PIM, K_UVO, UVO, PIM%C_SOU(J4), IER )
                   IF ( IER .NE. 0 ) THEN
                        CALL ERR_LOG ( 8446, IUER, 'PIMA_SPLT', 'Failure '// &
    &                       'to consolidate subarrays for source '//PIM%C_SOU(J4) )
                        IF ( PIM%CONF%SPLT_TOTAL_UV == PIMA__TOTAL_UV_YES ) THEN
                             CLOSE ( UNIT=LUN_TOT )
                        END IF
                        RETURN
                  END IF
              END IF
!
! ----------- Now we write the uv data into the output array.
!
              CALL ERR_PASS ( IUER, IER )
              CALL PIMA_SPLT_WRITE ( PIM, LFRQ, KFRQ, K_UVO, UVO, &
     &                               IND_FRQ, FRQ_ARR, AP_LEN_SCA, J4, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 8447, IUER, 'PIMA_SPLT', 'Failure '// &
     &                 'to write the output file in FITS-format with '// &
     &                 'calibrated uv-data for source '//PIM%C_SOU(J4) )
                   IF ( PIM%CONF%SPLT_TOTAL_UV == PIMA__TOTAL_UV_YES ) THEN
                        CLOSE ( UNIT=LUN_TOT )
                   END IF
                   RETURN
              END IF
         END IF
         IF ( PIM%CONF%SPLT_TOTAL_UV == PIMA__TOTAL_UV_YES ) THEN
              CLOSE ( UNIT=LUN_TOT )
         END IF
!
! ------ Memory deallocation section
!
         IF ( L_UVO > 0 ) THEN
              DO 4390 J39=1,L_UVO
                 DEALLOCATE ( UVO(J39)%SPE  )
                 DEALLOCATE ( UVO(J39)%WEI  )
                 DEALLOCATE ( UVO(J39)%SNR  )
                 DEALLOCATE ( UVO(J39)%SEFD )
 4390         CONTINUE 
         END IF
         IF ( ASSOCIATED ( UVO         ) ) DEALLOCATE ( UVO )
         IF ( ALLOCATED  ( TIM_SEG     ) ) DEALLOCATE ( TIM_SEG     )
         IF ( ALLOCATED  ( IND_SCA_SEG ) ) DEALLOCATE ( IND_SCA_SEG )
         IF ( ALLOCATED  ( SEFD_IF     ) ) DEALLOCATE ( SEFD_IF     )
!
         IF ( ASSOCIATED ( PIM%SUB%OBS_IND_SUB ) ) THEN
              DEALLOCATE ( PIM%SUB%OBS_IND_SUB )
         END IF 
         IF ( ASSOCIATED ( PIM%SUB%TIM_SRT ) ) THEN
              DEALLOCATE ( PIM%SUB%TIM_SRT )
         END IF 
         IF ( PIM%CONF%DEBUG_LEVEL .GE. 1 ) THEN
              WRITE ( 6, 1160 ) PIM%SOU(J4)%IVS_NAME, PIM%SOU(J4)%J2000_NAME, &
     &                          NOBS_USED, NSCA_USED
 1160         FORMAT ( ' PIMA_SPLT Processed  source ', A, 1X, A, &
     &                 ' Used_obs: ', I5, ' Used_scans: ', I3/ )
              CALL FLUSH ( 6 )
         END IF
!
         IF ( LU_OBS > 0 ) THEN
!
! ----------- Deallocate memory for observations used for processing this source
!
              DO 4400 J40=1,LU_OBS
                 IND_OBS = INDS_OBS_USED(J40)
                 IF ( ASSOCIATED ( PIM%OBS(IND_OBS)%RES_FRN )   ) DEALLOCATE ( PIM%OBS(IND_OBS)%RES_FRN )
                 IF ( ASSOCIATED ( PIM%OBS(IND_OBS)%USER_FLAG ) ) DEALLOCATE ( PIM%OBS(IND_OBS)%USER_FLAG )
                 IF ( ASSOCIATED ( PIM%OBS(IND_OBS)%UV   )      ) DEALLOCATE ( PIM%OBS(IND_OBS)%UV )
                 IF ( ASSOCIATED ( PIM%OBS(IND_OBS)%UV_IF )     ) DEALLOCATE ( PIM%OBS(IND_OBS)%UV_IF )
                 IF ( ASSOCIATED ( PIM%OBS(IND_OBS)%UV_BAND )   ) DEALLOCATE ( PIM%OBS(IND_OBS)%UV_BAND )
                 IF ( ASSOCIATED ( PIM%OBS(IND_OBS)%AC   )      ) DEALLOCATE ( PIM%OBS(IND_OBS)%AC )
                 IF ( ASSOCIATED ( PIM%OBS(IND_OBS)%AC_AVR_TIM )) DEALLOCATE ( PIM%OBS(IND_OBS)%AC_AVR_TIM )
                 IF ( ASSOCIATED ( PIM%OBS(IND_OBS)%AC_MEAN )   ) DEALLOCATE ( PIM%OBS(IND_OBS)%AC_MEAN )
                 IF ( ASSOCIATED ( PIM%OBS(IND_OBS)%WEI_1D )    ) DEALLOCATE ( PIM%OBS(IND_OBS)%WEI_1D )
                 IF ( ASSOCIATED ( PIM%OBS(IND_OBS)%WVR_DELAY ) ) DEALLOCATE ( PIM%OBS(IND_OBS)%WVR_DELAY )
                 IF ( ASSOCIATED ( PIM%OBS(IND_OBS)%TSRF )      ) DEALLOCATE ( PIM%OBS(IND_OBS)%TSRF )
 4400         CONTINUE 
         END IF
 440  CONTINUE
!
      DEALLOCATE ( WEI_FRQ_CHN       )
      DEALLOCATE ( RENRML_CROS_IF    )
      DEALLOCATE ( RENRML_AUTO_IF    )
      DEALLOCATE ( PIM%CONF%FRIB_OBS )
      IF ( PIM%CONF%DEBUG_LEVEL .GE. 4 ) THEN
           WRITE ( 6, * ) 'PIMA_SPLT Finished 1st run'
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  PIMA_SPLT  !#!#
