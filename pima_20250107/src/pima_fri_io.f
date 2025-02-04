      SUBROUTINE PIMA_FRI_WRI_HEAD ( PIM, LUN, IUER )
! ************************************************************************
! *                                                                      *
! *  Routine  PIMA_FRI_WRI_HEAD
! *                                                                      *
! * ## 06-JUL-2009  PIMA_FRI_WRI_HEAD  v1.1 (c) L. Petrov 08-FEB-2014 ## *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'astro_constants.i'
      INCLUDE   'pima.i'
      TYPE     ( PIMA__TYPE  ) :: PIM
      INTEGER*4  LUN, IUER
      CHARACTER  STR*128
      INTEGER*4  IER
      CHARACTER, EXTERNAL :: GET_CDATE*19
      INTEGER*4, EXTERNAL :: GET_UNIT
      INTEGER*4, EXTERNAL :: ILEN, I_LEN
!
      LUN = GET_UNIT()
!
      OPEN ( UNIT=LUN, FILE=PIM%CONF%FRINGE_FILE, STATUS='UNKNOWN', &
     &       POSITION='APPEND', IOSTAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 7371, IUER, 'PIMA_FRI_WRI_HEAD', 'Failure to '// &
     &          'open output file with the fringe output table '// &
     &          PIM%CONF%FRINGE_FILE(1:I_LEN(PIM%CONF%FRINGE_FILE))// &
     &          ' -- error '//STR )
           RETURN
      END IF
!
      WRITE ( UNIT=LUN, FMT='(A)', IOSTAT=IER ) PIMA__FRIRES_LABEL
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 7372, IUER, 'PIMA_FRI_WRI_HEAD', 'Failure to '// &
     &          'write output file with the fringe output table  '// &
     &          PIM%CONF%FRINGE_FILE(1:I_LEN(PIM%CONF%FRINGE_FILE))// &
     &          ' -- error '//STR )
           RETURN
      END IF
!
      WRITE ( UNIT=LUN, FMT='(A)', IOSTAT=IER ) '# '
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 7373, IUER, 'PIMA_FRI_WRI_HEAD', 'Failure to '// &
     &          'write output file with the fringe output table '// &
     &          PIM%CONF%FRINGE_FILE(1:I_LEN(PIM%CONF%FRINGE_FILE))// &
     &          ' -- error '//STR )
           RETURN
      END IF
!
      WRITE ( UNIT=LUN, FMT='(A)', IOSTAT=IER ) '# PIMA_FRINGE started on '// &
     &                                           GET_CDATE()
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( IER, STR )
           CALL ERR_LOG ( 7374, IUER, 'PIMA_FRI_WRI_HEAD', 'Failure to '// &
     &          'write in output file with SNR '// &
     &          PIM%CONF%FRINGE_FILE(1:I_LEN(PIM%CONF%FRINGE_FILE))// &
     &          ' -- error '//STR )
           RETURN
      END IF
!
      WRITE ( UNIT=LUN, FMT='(A)', IOSTAT=IER ) '# '//PIMA__LABEL
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 7375, IUER, 'PIMA_FRI_WRI_HEAD', 'Failure to '// &
     &          'write output file with the fringe output table '// &
     &          PIM%CONF%FRINGE_FILE(1:I_LEN(PIM%CONF%FRINGE_FILE))// &
     &          ' -- error '//STR )
           RETURN
      END IF
!
      WRITE ( UNIT=LUN, FMT='(A)', IOSTAT=IER ) '# '
      WRITE ( UNIT=LUN, FMT='(A)', IOSTAT=IER ) '# Control file:    '// &
     &                  PIM%CONF_FILE(1:I_LEN(PIM%CONF_FILE))
      WRITE ( UNIT=LUN, FMT='(A)', IOSTAT=IER ) '# Experiment code: '// &
     &                  PIM%OBS_CODE(1:I_LEN(PIM%OBS_CODE))
      WRITE ( UNIT=LUN, FMT='(A)', IOSTAT=IER ) '# Session code     '// &
     &                  PIM%CONF%SESS_CODE(1:I_LEN(PIM%CONF%SESS_CODE))
      WRITE ( UNIT=LUN, FMT='(A)', IOSTAT=IER ) '# '
      WRITE ( UNIT=LUN, FMT='(A,A)', IOSTAT=IER ) &
     &      '# BANDPASS_USE:              ', PIM%CONF%BANDPASS_USE
      WRITE ( UNIT=LUN, FMT='(A,I4)', IOSTAT=IER ) &
     &      '# CORR_FLAG_MIN:             ', PIM%CONF%CORR_FLAG_MIN
      IF ( ILEN(PIM%CONF%BANDPASS_MASK_FILE) == 0 ) THEN
           WRITE ( UNIT=LUN, FMT='(A,A)', IOSTAT=IER ) &
     &            '# BANDPASS_MASK_FILE:        NO'
         ELSE
           WRITE ( UNIT=LUN, FMT='(A,A)', IOSTAT=IER ) &
     &            '# BANDPASS_MASK_FILE:        ', &
     &       PIM%CONF%BANDPASS_MASK_FILE(1:I_LEN(PIM%CONF%BANDPASS_MASK_FILE))
      END IF
      WRITE ( UNIT=LUN, FMT='(A,A)', IOSTAT=IER ) &
     &      '# PCAL_USE:                  ', PIM%CONF%PHAS_CAL_CODE
      IF ( PIM%CONF%FRIB_FINE_SEARCH == PIMA__FINE_SEARCH_ACC ) THEN
           WRITE ( UNIT=LUN, FMT='(A,A)', IOSTAT=IER ) &
     &      '# FRINGE_FITTING_STYLE:      ', 'PH_ACC'
         ELSE
           WRITE ( UNIT=LUN, FMT='(A,A)', IOSTAT=IER ) &
     &      '# FRINGE_FITTING_STYLE:      ', 'GR_RAT'
      END IF
!
      WRITE ( UNIT=LUN, FMT='(A,A)', IOSTAT=IER ) &
     &      '# FRIB.SEARCH_TYPE:          ', PIM%CONF%FRIB_SEARCH_TYPE
      WRITE ( UNIT=LUN, FMT='(A,1PD12.4)', IOSTAT=IER ) &
     &      '# FRIB.DELAY_WINDOW_CENTER:  ', PIM%CONF%FRIB_DELAY_WINDOW_CENTER
      WRITE ( UNIT=LUN, FMT='(A,1PD12.4)', IOSTAT=IER ) &
     &      '# FRIB.RATE_WINDOW_CENTER:   ', PIM%CONF%FRIB_RATE_WINDOW_CENTER
      WRITE ( UNIT=LUN, FMT='(A,1PD12.4)', IOSTAT=IER ) &
     &      '# FRIB.DELAY_WINDOW_WIDTH:   ', PIM%CONF%FRIB_DELAY_WINDOW_WIDTH
      WRITE ( UNIT=LUN, FMT='(A,1PD12.4)', IOSTAT=IER ) &
     &      '# FRIB.RATE_WINDOW_WIDTH:    ', PIM%CONF%FRIB_RATE_WINDOW_WIDTH
      WRITE ( UNIT=LUN, FMT='(A,I3)', IOSTAT=IER ) &
     &      '# FRIB.FRQ_GRP:              ', PIM%CONF%FRQ_GRP
      WRITE ( UNIT=LUN, FMT='(A,I3)', IOSTAT=IER ) &
     &      '# FRIB.BEG_IFRQ:             ', PIM%CONF%BEG_FRQ
      WRITE ( UNIT=LUN, FMT='(A,I3)', IOSTAT=IER ) &
     &      '# FRIB.END_IFRQ:             ', PIM%CONF%END_FRQ
      WRITE ( UNIT=LUN, FMT='(A,A8)', IOSTAT=IER ) &
     &      '# FRIB.POLAR:                 ', PIM%CONF%POLAR
      WRITE ( UNIT=LUN, FMT='(A,I4)', IOSTAT=IER ) &
     &      '# FRIB.OVERSAMPLE_MD:       ', PIM%CONF%FRIB_OVERSAMPLE_MD
      WRITE ( UNIT=LUN, FMT='(A,I4)', IOSTAT=IER ) &
     &      '# FRIB.OVERSAMPLE_RT:       ', PIM%CONF%FRIB_OVERSAMPLE_RT
      WRITE ( UNIT=LUN, FMT='(A,A)', IOSTAT=IER ) &
     &      '# FRIB.FINE_SEARCH:          ', PIM%CONF%FRIB_FINE_SEARCH
      WRITE ( UNIT=LUN, FMT='(A,A)',  IOSTAT=IER ) &
     &      '# FRIB.AUTOCORR_CALIB:       ', PIM%CONF%FRIB_AUTOCORR_CALIB
      WRITE ( UNIT=LUN, FMT='(A,A)',  IOSTAT=IER ) &
     &      '# FRIB.AMPL_FUDGE_TYPE:      ', PIM%CONF%FRIB_AMPL_FUDGE_TYPE
!
      WRITE ( UNIT=LUN, FMT='(A,1PD12.4)', IOSTAT=IER ) &
     &      '# FRIB.AUTOCORR_THRESHOLD:   ', PIM%CONF%FRIB_AUTOCORR_THRESHOLD
      WRITE ( UNIT=LUN, FMT='(A,1PD12.4)', IOSTAT=IER ) &
     &      '# FRIB.WEIGHTS_THRESHOLD:    ', PIM%CONF%FRIB_WEIGHTS_THRESHOLD
      WRITE ( UNIT=LUN, FMT='(A,1PD12.4)', IOSTAT=IER ) &
     &      '# FRIB.NOISE_NSIGMA:         ', PIM%CONF%FRIB_NOISE_NSIGMA
      WRITE ( UNIT=LUN, FMT='(A,1PD12.4)', IOSTAT=IER ) &
     &      '# FRIB.SNR_DETECTION:        ', PIM%CONF%FRIB_SNR_DETECTION
      WRITE ( UNIT=LUN, FMT='(A,1PD12.4)', IOSTAT=IER ) &
     &      '# PHASE_ACCELERATION:        ', PIM%CONF%PHASE_ACCELERATION
      WRITE ( UNIT=LUN, FMT='(A,A)', IOSTAT=IER ) '# '
      CALL FLUSH ( LUN )
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  PIMA_FRI_WRI_HEAD  !#!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE PIMA_FRI_WRI_TAIL ( PIM, LUN, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  PIMA_FRI_WRI_TAIL
! *                                                                      *
! * ### 06-JUL-2009 PIMA_FRI_WRI_TAIL v1.0 (c) L. Petrov 06-JUL-2009 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'astro_constants.i'
      INCLUDE   'pima.i'
      TYPE     ( PIMA__TYPE  ) :: PIM
      INTEGER*4  LUN, IUER
      CHARACTER  STR*128
      INTEGER*4  IER
      CHARACTER, EXTERNAL :: GET_CDATE*19
      INTEGER*4, EXTERNAL :: ILEN, I_LEN
!
      WRITE ( UNIT=LUN, FMT='(A)', IOSTAT=IER ) '# '
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 7361, IUER, 'PIMA_FRI_WRI_TAIL', 'Failure '// &
     &          'to write output file with SNR '// &
     &          PIM%CONF%FRINGE_FILE(1:I_LEN(PIM%CONF%FRINGE_FILE))// &
     &          ' -- error '//STR )
           RETURN
      END IF
!
      WRITE ( UNIT=LUN, FMT='(A)', IOSTAT=IER ) '# PIMA_FRINGE ended on '// &
     &                                           GET_CDATE()
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( IER, STR )
           CALL ERR_LOG ( 7362, IUER, 'PIMA_FRI_WRI_TAIL', 'Failure '// &
     &          'to write in output file with SNR '// &
     &          PIM%CONF%FRINGE_FILE(1:I_LEN(PIM%CONF%FRINGE_FILE))// &
     &          ' -- error '//STR )
           RETURN
      END IF
      CLOSE ( UNIT=LUN )
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  PIMA_FRI_WRI_TAIL  !#!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE PIMA_FRI_WRI_OBS ( PIM, LUN, IND_OBS, SNR, AMPL, AMPL_INTG,  &
     &           NOI_AVR, TIME_FRT, GR_DEL, PH_RAT, GR_RAT, PH_ACC, SB_DEL,   &
     &           PHS, GR_DEL_ERR, PH_RAT_ERR, GR_RAT_ERR, PH_ACC_ERR,         &
     &           SB_DEL_ERR, PH_DEL_ERR, GRAMBSP, AP_LEN, EFF_DURA, FREQ_REF, &
     &           EFF_FRQ_PHS, EFF_FRQ_GRP, EFF_FRQ_RAT, COV_PR_PH, &
     &           COV_GR_MD, TEC, TEC_RATE, TEC_ERR, TEC_RATE_ERR, &
     &           POLAR_USED, PA_USED, DECOR_TIM, PCAL_GDEL, FRI_STS, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  PIMA_FRI_WRI_OBS
! *                                                                      *
! * ### 06-JUL-2009 PIMA_FRI_WRI_OBS v3.3 (c) L. Petrov  15-DEC-2022 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'astro_constants.i'
      INCLUDE   'pima.i'
      TYPE     ( PIMA__TYPE  ) :: PIM
      INTEGER*4  LUN, IND_OBS, FRI_STS, IUER
      REAL*8     SNR, AMPL(PIM__MFRA), AMPL_INTG, NOI_AVR, TIME_FRT, &
     &           GR_DEL(PIM__MFRA), PH_RAT(PIM__MFRA), GR_RAT, PH_ACC, &
     &           SB_DEL, PHS(PIM__MFRA), GR_DEL_ERR(PIM__MFRA), &
     &           PH_RAT_ERR(PIM__MFRA), GR_RAT_ERR, PH_ACC_ERR, SB_DEL_ERR, &
     &           PH_DEL_ERR(PIM__MFRA), GRAMBSP, AP_LEN, EFF_DURA, &
     &           FREQ_REF, EFF_FRQ_PHS, EFF_FRQ_GRP, EFF_FRQ_RAT, &
     &           COV_PR_PH, COV_GR_MD, TEC, TEC_RATE, TEC_ERR, TEC_RATE_ERR, &
     &           DECOR_TIM
      REAL*4     PCAL_GDEL(2,2)
      REAL*8     EL_USE(2), PA_USED
      CHARACTER  STR*128, STR_DAT_BEG*30, STR_DAT_END*30, POLAR_USED*(*)
      INTEGER*4  FRG_IND, IER
      LOGICAL*1  FL_PIMAVAR_PARANG_WRITE
      CHARACTER, EXTERNAL :: MJDSEC_TO_DATE*30, GET_CDATE*19
      INTEGER*4, EXTERNAL :: GET_UNIT
      INTEGER*4, EXTERNAL :: ILEN, I_LEN
!
      CALL GETENVAR ( 'PIMAVAR_PARANG_WRITE', STR )
      IF ( STR == 'YES' .OR. STR == 'yes' ) THEN
           FL_PIMAVAR_PARANG_WRITE = .TRUE.
         ELSE
           FL_PIMAVAR_PARANG_WRITE = .FALSE.
      END IF
!
      FRG_IND = PIM%OBS(IND_OBS)%REF_FRG_INDS(PIM%CONF%FRQ_GRP)
      STR_DAT_BEG = MJDSEC_TO_DATE ( PIM%MJD_0, PIM%TAI_0 + &
     &                               PIM%TIM_R8(PIM%UV_IND(PIM%OBS(IND_OBS)%UV_IND(1,FRG_IND))%TIM_IND), -3 )
      STR_DAT_END = MJDSEC_TO_DATE ( PIM%MJD_0, PIM%TAI_0 + &
     &                               PIM%TIM_R8(PIM%UV_IND(PIM%OBS(IND_OBS)%UV_IND(PIM%OBS(IND_OBS)%NUM_EPC(FRG_IND),FRG_IND))%TIM_IND), -3 )
      IF ( SNR > 0.0D0 ) THEN
           EL_USE(1) = PIM%OBS(IND_OBS)%ELEV(1)
           EL_USE(2) = PIM%OBS(IND_OBS)%ELEV(2)
           IF ( EL_USE(1) < -0.1 ) EL_USE(1) = -0.1
           IF ( EL_USE(2) < -0.1 ) EL_USE(2) = -0.1
           IF ( PIM%OBS(IND_OBS)%REF_FRG_INDS(PIM%CONF%FRQ_GRP) == 0 ) THEN
                WRITE ( UNIT=LUN, FMT=110, IOSTAT=IER ) &
     &             IND_OBS, &
     &             PIM%OBS(IND_OBS)%SCA_IND, &
     &             PIM%SCA(PIM%OBS(IND_OBS)%SCA_IND)%SCAN_NAME(1:10), &
     &             PIM%SOU(PIM%OBS(IND_OBS)%SOU_IND)%IVS_NAME, &
     &             PIM%STA(PIM%OBS(IND_OBS)%STA_IND(1))%IVS_NAME, &
     &             PIM%STA(PIM%OBS(IND_OBS)%STA_IND(2))%IVS_NAME, &
     &             'NO OBS AT THIS FREQUENCY GROUP'
 110            FORMAT ( I6, 1X, I4, 1X, A, 1X, A, 1X, A, 1X, A, &
     &              1X, A )
                CALL FLUSH ( LUN )
                CALL ERR_LOG ( 0, IUER )
           END IF
           IF ( PH_ACC == -1.0D0 .AND. PH_ACC_ERR == -1.0D0 )   THEN
                WRITE ( UNIT=LUN, FMT=120, IOSTAT=IER ) &
     &                  IND_OBS, &
     &                  PIM%OBS(IND_OBS)%SCA_IND, &
     &                  PIM%SCA(PIM%OBS(IND_OBS)%SCA_IND)%SCAN_NAME(1:10), &
     &                  PIM%SOU(PIM%OBS(IND_OBS)%SOU_IND)%IVS_NAME, &
     &                  PIM%STA(PIM%OBS(IND_OBS)%STA_IND(1))%IVS_NAME, &
     &                  PIM%STA(PIM%OBS(IND_OBS)%STA_IND(2))%IVS_NAME, &
     &                  MIN(SNR,99999.00D0), AMPL(PIMA__DRF), &
     &                  STR_DAT_BEG(1:23), STR_DAT_END(1:23), &
     &                  AMPL(PIMA__LSQ), &
     &                  PIM%OBS(IND_OBS)%NUM_EPC(PIM%OBS(IND_OBS)%REF_FRG_INDS(PIM%CONF%FRQ_GRP)), &
     &                  TIME_FRT, GR_DEL, PH_RAT, GR_RAT, SB_DEL, PHS, &
     &                  GR_DEL_ERR, PH_RAT_ERR, GR_RAT_ERR, SB_DEL_ERR, &
     &                  PH_DEL_ERR, GRAMBSP, &
     &                  EFF_DURA, AP_LEN, &
     &                  PIM%OBS(IND_OBS)%UVW(1), PIM%OBS(IND_OBS)%UVW(2), &
     &                  AMPL_INTG, &
     &                  EL_USE(1)/DEG__TO__RAD, &
     &                  EL_USE(2)/DEG__TO__RAD, &
     &                  NOI_AVR, &
     &                  FREQ_REF, EFF_FRQ_PHS, EFF_FRQ_GRP, EFF_FRQ_RAT, &
     &                  COV_PR_PH, COV_GR_MD, TEC, TEC_RATE, TEC_ERR, TEC_RATE_ERR, &
     &                  PIM%OBS(IND_OBS)%FEED_ANG(1), PIM%OBS(IND_OBS)%FEED_ANG(2), &
     &                  DECOR_TIM, PCAL_GDEL, PA_USED, POLAR_USED, FRI_STS
 120            FORMAT ( I6, 1X, I4, 1X, A, 1X, A, 1X, A, 1X, A, &
     &                   ' SNR: ', 0PF8.2, ' Ampl: ', 0PF9.7, &
     &                   ' [ ', A, ', ', A, ' ] Ampl_lsq: ', 0PF9.7, &
     &                   ' No_ap: ', I6, 1X, ' FRT_offst: ', 0PF16.9, &
     &                   ' Gr_del_drf: ', 1PD15.8, ' Gr_del_lsq: ', 1PD15.8, &
     &                   ' Gr_del_mul: ', 1PD15.8, ' Gr_del_add: ', 1PD15.8, &
     &                   ' Ph_rat_drf: ', 1PD15.8, ' Ph_rat_lsq: ', 1PD15.8, &
     &                   ' Ph_rat_mul: ', 1PD15.8, ' Ph_rat_add: ', 1PD15.8, &
     &                   ' Gr_rat: ', 1PD15.8, ' Sb_del: ', 1PD15.8, &
     &                   ' Phs_drf: ', 0PF9.5, ' Phs_lsq: ', 0PF9.5, &
     &                   ' Phs_mul: ', 0PF9.5, ' Phs_add: ', 0PF9.5, &
     &                   ' Gd_err_drf: ', 1PD13.6, ' Gd_err_lsq: ', 1PD13.6, &
     &                   ' Gd_err_mul: ', 1PD13.6, ' Gd_err_add: ', 1PD13.6, &
     &                   ' Pr_err_drf: ', 1PD13.6, ' Pr_err_lsq: ', 1PD13.6, &
     &                   ' Pr_err_mul: ', 1PD13.6, ' Pr_err_add: ', 1PD13.6, &
     &                   ' Gr_rat_err: ', 1PD13.6, ' Sb_err:  ', 1PD13.6, &
     &                   ' Pd_err_drf: ', 1PD13.6, ' Pd_err_lsq: ', 1PD13.6, &
     &                   ' Pd_err_mul: ', 1PD13.6, ' Pd_err_add: ', 1PD13.6, &
     &                   ' Gr_amb_sp: ', 1PD15.8, &
     &                   ' Duration: ', 0PF7.2,' s  AP_len: ', 0PF9.7, &
     &                   ' UV: ',2(1PD14.7,1X), ' Amp_Intg: ', 0PF8.5, &
     &                   ' El_Deg: ', 0PF6.3, 1X, 0PF6.3, ' Noi: ',1PD10.4, &
     &                   ' Ref_Frq: ', 1PD19.12,2X, &
     &                   ' Eff_Frq: ', 3(1PD19.12,2X), &
     &                   ' Cov_Pr: ', 1PD12.5, ' Cov_Gr: ', 1PD12.5, &
     &                   ' Tec: ', 1PD13.5, ' Tec_rate: ', 1PD13.5, &
     &                   ' Tec_err: ', 1PD12.5, ' Tec_rate_err: ', 1PD12.5, &
     &                   ' Par_ang: ', 0PF6.3, 1X, 0PF6.3, &
     &                   ' Decor_tim: ', 0PF5.3, &
     &                   ' Pcal_gdel_pol1: ', 1PE13.6, 1X, 1PE13.6, &
     &                   ' Pcal_gdel_pol2: ', 1PE13.6, 1X, 1PE13.6, &
     &                   ' Pa_used: ', 0PF6.3, ' Polar: ', A2, ' Sts: ', B16 )
!         PARAMETER  ( PIMA__FRIRES_LABEL     = '# PIMA Fringe results  v  1.1   Format version of 2019.02.24' )
             ELSE
                WRITE ( UNIT=LUN, FMT=130, IOSTAT=IER ) &
     &                  IND_OBS, &
     &                  PIM%OBS(IND_OBS)%SCA_IND, &
     &                  PIM%SCA(PIM%OBS(IND_OBS)%SCA_IND)%SCAN_NAME(1:10), &
     &                  PIM%SOU(PIM%OBS(IND_OBS)%SOU_IND)%IVS_NAME, &
     &                  PIM%STA(PIM%OBS(IND_OBS)%STA_IND(1))%IVS_NAME, &
     &                  PIM%STA(PIM%OBS(IND_OBS)%STA_IND(2))%IVS_NAME, &
     &                  MIN(SNR,99999.00D0), AMPL(PIMA__DRF), &
     &                  STR_DAT_BEG(1:23), STR_DAT_END(1:23), &
     &                  AMPL(PIMA__LSQ), &
     &                  PIM%OBS(IND_OBS)%NUM_EPC(PIM%OBS(IND_OBS)%REF_FRG_INDS(PIM%CONF%FRQ_GRP)), &
     &                  TIME_FRT, GR_DEL, PH_RAT, PH_ACC, SB_DEL, PHS, &
     &                  GR_DEL_ERR, PH_RAT_ERR, PH_ACC_ERR, SB_DEL_ERR, &
     &                  PH_DEL_ERR, GRAMBSP, &
     &                  EFF_DURA, AP_LEN, &
     &                  PIM%OBS(IND_OBS)%UVW(1), PIM%OBS(IND_OBS)%UVW(2), &
     &                  AMPL_INTG, &
     &                  EL_USE(1)/DEG__TO__RAD, &
     &                  EL_USE(2)/DEG__TO__RAD, &
     &                  NOI_AVR, &
     &                  FREQ_REF, EFF_FRQ_PHS, EFF_FRQ_GRP, EFF_FRQ_RAT, &
     &                  COV_PR_PH, COV_GR_MD, &
     &                  TEC, TEC_RATE, TEC_ERR, TEC_RATE_ERR, &
     &                  PIM%OBS(IND_OBS)%FEED_ANG(1), PIM%OBS(IND_OBS)%FEED_ANG(2), &
     &                  DECOR_TIM, PCAL_GDEL, PA_USED, POLAR_USED, FRI_STS
 130            FORMAT ( I6, 1X, I4, 1X, A, 1X, A, 1X, A, 1X, A, &
     &                   ' SNR: ', 0PF8.2, ' Ampl: ', 0PF9.7, &
     &                   ' [ ', A, ', ', A, ' ] Ampl_lsq: ', 0PF9.7, &
     &                   ' No_ap: ', I6, 1X, ' FRT_offst: ', 0PF16.9, &
     &                   ' Gr_del_drf: ', 1PD15.8, ' Gr_del_lsq: ', 1PD15.8, &
     &                   ' Gr_del_mul: ', 1PD15.8, ' Gr_del_add: ', 1PD15.8, &
     &                   ' Ph_rat_drf: ', 1PD15.8, ' Ph_rat_lsq: ', 1PD15.8, &
     &                   ' Ph_rat_add: ', 1PD15.8, ' Ph_rat_add: ', 1PD15.8, &
     &                   ' Ph_acc: ', 1PD15.8, ' Sb_del: ', 1PD15.8, &
     &                   ' Phs_drf: ', 0PF9.5, ' Phs_lsq: ', 0PF9.5, &
     &                   ' Phs_mul: ', 0PF9.5, ' Phs_add: ', 0PF9.5, &
     &                   ' Gd_err_drf: ', 1PD13.6, ' Gd_err_lsq: ', 1PD13.6, &
     &                   ' Gd_err_mul: ', 1PD13.6, ' Gd_err_add: ', 1PD13.6, &
     &                   ' Pr_err_drf: ', 1PD13.6, ' Pr_err_lsq: ', 1PD13.6, &
     &                   ' Pr_err_mul: ', 1PD13.6, ' Pr_err_add: ', 1PD13.6, &
     &                   ' Ph_acc_err: ', 1PD13.6, ' Sb_err:  ',    1PD13.6, &
     &                   ' Pd_err_drf: ', 1PD13.6, ' Pd_err_lsq: ', 1PD13.6, &
     &                   ' Pd_err_mul: ', 1PD13.6, ' Pd_err_add: ', 1PD13.6, &
     &                   ' Gr_amb_sp: ', 1PD15.8, &
     &                   ' Duration: ', 0PF7.2,' s  AP_len: ', 0PF9.7, &
     &                   ' UV: ',2(1PD14.7,1X), ' Amp_Intg: ', 0PF8.5, &
     &                   ' El_Deg: ', 0PF6.3, 1X, 0PF6.3, ' Noi: ',1PD10.4, &
     &                   ' Ref_Frq: ', 1PD19.12,2X, &
     &                   ' Eff_Frq: ', 3(1PD19.12,2X), &
     &                   ' Cov_Pr: ', 1PD12.5, ' Cov_Gr: ', 1PD12.5, &
     &                   ' Tec: ', 1PD13.5, ' Tec_err: ', 1PD13.5, &
     &                   ' Tec_rate: ', 1PD12.5, ' Tec_rate_err: ', 1PD12.5, &
     &                   ' Par_ang: ', 0PF6.3, 1X, 0PF6.3, &
     &                   ' Decor_tim: ', 0PF5.3, &
     &                   ' Pcal_gdel_pol1: ', 1PE13.6, 1X, 1PE13.6, &
     &                   ' Pcal_gdel_pol2: ', 1PE13.6, 1X, 1PE13.6, &
     &                   ' Pa_used: ', 0PF6.3, ' Polar: ', A2, ' Sts: ', B16 )
           END IF
         ELSE
           CALL CLRCH ( STR )
           STR = 'FAILURE '
           WRITE ( UNIT=LUN, FMT=140, IOSTAT=IER ) &
     &             IND_OBS, PIM%OBS(IND_OBS)%SCA_IND, &
     &             PIM%SCA(PIM%OBS(IND_OBS)%SCA_IND)%SCAN_NAME(1:10), &
     &             PIM%SOU(PIM%OBS(IND_OBS)%SOU_IND)%IVS_NAME, &
     &             PIM%STA(PIM%OBS(IND_OBS)%STA_IND(1))%IVS_NAME, &
     &             PIM%STA(PIM%OBS(IND_OBS)%STA_IND(2))%IVS_NAME, &
     &             STR(1:8), FRI_STS, STR_DAT_BEG(1:23), STR_DAT_END(1:23)
 140       FORMAT ( I6, 1X, I4, 1X, A, 1X, A, 1X, A, 1X, A, &
     &              1X, A8, 2X, B16, 3X, ' [ ', A, ', ', A, ' ]' )
      END IF
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( IER, STR )
           CALL ERR_LOG ( 7381, IUER, 'PIMA_FRI_WRI_OBS', 'Failure '// &
     &          'to write an observation record in output file with SNR '// &
     &          PIM%CONF%FRINGE_FILE(1:I_LEN(PIM%CONF%FRINGE_FILE))// &
     &          ' -- error '//STR )
           RETURN
      END IF
      CALL FLUSH ( LUN )
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE PIMA_FRI_WRI_OBS  !#!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE PIMA_FRI_REA_OBS ( PIM, LIN, IND_OBS, IND_SCA, IND_SOU, &
     &           IND_STA, SNR, AMPL, AMPL_INTG, TIME_FRT, &
     &           GR_DEL, PH_RAT, GR_RAT, PH_ACC, SB_DEL, PHS, &
     &           GR_DEL_ERR, PH_RAT_ERR, GR_RAT_ERR, PH_ACC_ERR, SB_DEL_ERR, &
     &           PH_DEL_ERR, GRAMBSP, SCAN_DUR, AP_LEN, FREQ_REF, &
     &           EFF_FRQ_PHS, EFF_FRQ_GRP, EFF_FRQ_RAT, &
     &           COV_PR_PH, COV_GR_MD, TEC, TEC_RATE, TEC_ERR, TEC_RATE_ERR, &
     &           POLAR_USED, PAR_ANG, PA_USED, DECOR_TIM, PCAL_GDEL, &
     &           FRI_STS, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  PIMA_FRI_REA_OBS
! *                                                                      *
! * ### 06-JUL-2009  PIMA_FRI_REA_OBS v2.3 (c) L. Petrov 15-DEC-2022 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'astro_constants.i'
      INCLUDE   'pima.i'
      TYPE     ( PIMA__TYPE  ) :: PIM
      CHARACTER  LIN*(*), POLAR_USED*(*)
      INTEGER*4  IND_OBS, IND_SCA, IND_SOU, IND_STA(2), FRI_STS, IUER
      REAL*8     SNR, AMPL(PIM__MFRA), AMPL_INTG, TIME_FRT, &
     &           GR_DEL(PIM__MFRA), PH_RAT(PIM__MFRA), GR_RAT, SCAN_DUR, &
     &           SB_DEL, PHS(PIM__MFRA), GR_DEL_ERR(PIM__MFRA), &
     &           PH_RAT_ERR(PIM__MFRA), GR_RAT_ERR, SB_DEL_ERR, &
     &           PH_DEL_ERR(PIM__MFRA), GRAMBSP, AP_LEN, &
     &           FREQ_REF, EFF_FRQ_PHS, EFF_FRQ_GRP, EFF_FRQ_RAT, &
     &           COV_PR_PH, COV_GR_MD, PH_ACC, PH_ACC_ERR, &
     &           TEC, TEC_RATE, TEC_ERR, TEC_RATE_ERR, PAR_ANG(2), &
     &           PA_USED, DECOR_TIM
      REAL*4     PCAL_GDEL(2,2)
      REAL*8     NOIR, UVW(2), EL(2)
      CHARACTER  STR*128
      INTEGER*4  NUM_EPC, IER
      INTEGER*4, EXTERNAL :: ILEN, I_LEN, LTM_DIF
!
      IND_OBS      = -1
      SNR          = -1.0D10
      AMPL(PIMA__DRF) = -1.0D10
      AMPL(PIMA__LSQ) = -1.0D10
      AMPL_INTG    = -1.0D10
      TIME_FRT     = -1.0D10
      GR_DEL       = -1.0D10
      PH_RAT       = -1.0D10
      GR_RAT       = -1.0D10
      PH_ACC       = -1.0D10
      SB_DEL       = -1.0D10
      PHS          = -1.0D10
      PH_RAT_ERR   = -1.0D10
      GR_DEL_ERR   = -1.0D10
      PH_ACC_ERR   = -1.0D10
      PH_DEL_ERR   = -1.0D10
      SB_DEL_ERR   = -1.0D10
      SCAN_DUR     = -1.0D10
      GRAMBSP      = -1.0D10
      AP_LEN       = -1.0D10
      FREQ_REF     = -1.0D10
      EFF_FRQ_PHS  = -1.0D10
      EFF_FRQ_GRP  = -1.0D10
      EFF_FRQ_RAT  = -1.0D10
      TEC          = -1.0D10
      TEC_RATE     = -1.0D10
      TEC_ERR      = -1.0D10
      TEC_RATE_ERR = -1.0D10
      PAR_ANG      = -1.0D10
      PA_USED      = -1.0D10
      DECOR_TIM    = -1.0D10
      PCAL_GDEL    = -1.0E10
      POLAR_USED   = '??'
      FRI_STS      = 0
!
      IF ( LIN(1:1) == '#' ) THEN
           CALL ERR_LOG ( 0, IUER )
           RETURN
         ELSE IF ( LIN(51:57) == 'FAILURE' ) THEN
           READ ( UNIT=LIN(1:6),   FMT='(I6)',  IOSTAT=IER ) IND_OBS
           READ ( UNIT=LIN(8:11),  FMT='(I4)',  IOSTAT=IER ) IND_SCA
           READ ( UNIT=LIN(61:76), FMT='(B16)', IOSTAT=IER ) FRI_STS
           IND_SOU    = LTM_DIF ( 1, PIM%NSOU, PIM%C_SOU, LIN(24:31) )
           IND_STA(1) = LTM_DIF ( 1, PIM%NSTA, PIM%C_STA, LIN(33:40) )
           IND_STA(2) = LTM_DIF ( 1, PIM%NSTA, PIM%C_STA, LIN(42:49) )
!
           CALL ERR_LOG ( 0, IUER )
           RETURN
         ELSE
           READ ( UNIT=LIN(1:6), FMT='(I6)', IOSTAT=IER ) IND_OBS
           IF ( IER .NE. 0 ) THEN
                CALL CLRCH ( STR )
                CALL INCH  ( IER, STR )
                CALL ERR_LOG ( 7381, IUER, 'PIMA_FRI_REA_OBS', 'Failure '// &
     &              'to decode obseration index '//LIN(1:6)// &
     &              ' from a record of the fringe file: '//LIN )
                RETURN
             ELSE IF ( IND_OBS < 1 ) THEN
                CALL ERR_LOG ( 7382, IUER, 'PIMA_FRI_REA_OBS', 'Wrong '// &
     &              'obseration index from a record of the '// &
     &              'fringe file: '//LIN(1:6) )
                RETURN
             ELSE IF ( IND_OBS > PIM%NOBS ) THEN
                CALL CLRCH ( STR )
                CALL INCH  ( PIM%NOBS, STR )
                CALL ERR_LOG ( 7383, IUER, 'PIMA_FRI_REA_OBS', 'Wrong '// &
     &              'obseration index from a record of the fringe file '// &
     &               LIN(1:6)//' -- too big. It should be no more than '//STR  )
                RETURN
           END IF
!
           IF ( LIN(421:427) == 'Gr_rat:' .AND. LIN(1456:1459) == 'Sts:' ) THEN
                READ (  UNIT=LIN, FMT=110 ) &
     &                  IND_OBS, IND_SCA, STR(61:70), STR(71:78), &
     &                  STR(81:88), STR(91:98), SNR, AMPL(PIMA__DRF), &
     &                  STR(1:23), STR(31:53), AMPL(PIMA__LSQ), &
     &                  NUM_EPC, TIME_FRT, GR_DEL, PH_RAT, GR_RAT, SB_DEL, PHS, &
     &                  GR_DEL_ERR, PH_RAT_ERR, GR_RAT_ERR, SB_DEL_ERR, &
     &                  PH_DEL_ERR, &
     &                  GRAMBSP, SCAN_DUR, AP_LEN, UVW, AMPL_INTG, &
     &                  EL, NOIR, FREQ_REF, EFF_FRQ_PHS, EFF_FRQ_GRP, &
     &                  EFF_FRQ_RAT, COV_PR_PH, COV_GR_MD, &
     &                  TEC, TEC_RATE, TEC_ERR, TEC_RATE_ERR, &
     &                  PAR_ANG, DECOR_TIM, PCAL_GDEL, PA_USED, POLAR_USED, FRI_STS
 110       FORMAT ( I6, 1X, I4, 1X, A, 1X, A, 1X, A, 1X, A, &
     &              6X, F8.2, 7X, F9.7, &
     &              3X, A, 2X, A, 13X, F9.7, &
     &              8X, I6, 13X, F16.9,      &
     &              13X, D15.8, 13X, D15.8,  &
     &              13X,  D15.8, 13X, D15.8, &
     &              13X,  D15.8, 13X, D15.8, &
     &              13X,  D15.8, 13X, D15.8, &
     &              9X,   D15.8, 9X,  D15.8, &
     &              10X,  F9.5, 10X,  F9.5,  &
     &              10X,  F9.5, 10X,  F9.5,  &
     &              13X, D13.6, 13X,  D13.6, &
     &              13X, D13.6, 13X,  D13.6, &
     &              13X, D13.6, 13X,  D13.6, &
     &              13X, D13.6, 13X,  D13.6, &
     &              13X, D13.6, 10X,  D13.6, &
     &              13X, D13.6, 13X,  D13.6, &
     &              13X, D13.6, 13X,  D13.6, &
     &              12X, D15.8,              &
     &              11X,  F7.2, 12X, F9.7,   &
     &              5X, 2(D14.7,1X), 11X, F8.5, &
     &              9X, F6.3, 1X, F6.3, 6X, D10.4, &
     &              10X, D19.12, 2X,         &
     &              10X, 3(D19.12,2X),       &
     &              9X, D12.5, 9X, D12.5,    &
     &              7X, D12.5, 12X, D12.5,   &
     &              10X, D12.5, 15X, D12.5,  &
     &              10X, F6.3, 1X, F6.3,     &
     &              12X, F6.3,               &
     &              17X,E13.6, 1X, E13.6,    &
     &              17X,E13.6, 1X, E13.6,    &
     &              10X, F6.3, 7X, A2, 6X, B16 )
             ELSE IF ( LIN(421:427) == 'Ph_acc:' .AND. LIN(1456:1459) == 'Sts:' ) THEN
                READ (  UNIT=LIN, FMT=110 ) &
     &                  IND_OBS, IND_SCA, STR(61:70), STR(71:78), &
     &                  STR(81:88), STR(91:98), SNR, AMPL(PIMA__DRF), &
     &                  STR(1:23), STR(31:53), AMPL(PIMA__LSQ), &
     &                  NUM_EPC, TIME_FRT, GR_DEL, PH_RAT, PH_ACC, SB_DEL, PHS, &
     &                  GR_DEL_ERR, PH_RAT_ERR, PH_ACC_ERR, SB_DEL_ERR, &
     &                  PH_DEL_ERR, &
     &                  GRAMBSP, SCAN_DUR, AP_LEN, UVW, AMPL_INTG, &
     &                  EL, NOIR, FREQ_REF, EFF_FRQ_PHS, EFF_FRQ_GRP, &
     &                  EFF_FRQ_RAT, COV_PR_PH, COV_GR_MD, &
     &                  TEC, TEC_RATE, TEC_ERR, TEC_RATE_ERR, &
     &                  PAR_ANG, DECOR_TIM, PCAL_GDEL, PA_USED, POLAR_USED, FRI_STS
             ELSE IF ( LIN(421:427) == 'Gr_rat:' .AND. LIN(1368:1371) == 'Sts:' ) THEN
                READ (  UNIT=LIN, FMT=120 ) &
     &                  IND_OBS, IND_SCA, STR(61:70), STR(71:78), &
     &                  STR(81:88), STR(91:98), SNR, AMPL(PIMA__DRF), &
     &                  STR(1:23), STR(31:53), AMPL(PIMA__LSQ), &
     &                  NUM_EPC, TIME_FRT, GR_DEL, PH_RAT, GR_RAT, SB_DEL, PHS, &
     &                  GR_DEL_ERR, PH_RAT_ERR, GR_RAT_ERR, SB_DEL_ERR, &
     &                  PH_DEL_ERR, &
     &                  GRAMBSP, SCAN_DUR, AP_LEN, UVW, AMPL_INTG, &
     &                  EL, NOIR, FREQ_REF, EFF_FRQ_PHS, EFF_FRQ_GRP, &
     &                  EFF_FRQ_RAT, COV_PR_PH, COV_GR_MD, &
     &                  TEC, TEC_RATE, TEC_ERR, TEC_RATE_ERR, &
     &                  PAR_ANG, DECOR_TIM, PA_USED, POLAR_USED, FRI_STS
                PCAL_GDEL = 0.0
             ELSE IF ( LIN(421:427) == 'Ph_acc:' .AND. LIN(1368:1371) == 'Sts:' ) THEN
                READ (  UNIT=LIN, FMT=120 ) &
     &                  IND_OBS, IND_SCA, STR(61:70), STR(71:78), &
     &                  STR(81:88), STR(91:98), SNR, AMPL(PIMA__DRF), &
     &                  STR(1:23), STR(31:53), AMPL(PIMA__LSQ), &
     &                  NUM_EPC, TIME_FRT, GR_DEL, PH_RAT, PH_ACC, SB_DEL, PHS, &
     &                  GR_DEL_ERR, PH_RAT_ERR, PH_ACC_ERR, SB_DEL_ERR, &
     &                  PH_DEL_ERR, &
     &                  GRAMBSP, SCAN_DUR, AP_LEN, UVW, AMPL_INTG, &
     &                  EL, NOIR, FREQ_REF, EFF_FRQ_PHS, EFF_FRQ_GRP, &
     &                  EFF_FRQ_RAT, COV_PR_PH, COV_GR_MD, &
     &                  TEC, TEC_RATE, TEC_ERR, TEC_RATE_ERR, &
     &                  PAR_ANG, DECOR_TIM, PA_USED, POLAR_USED, FRI_STS
 120       FORMAT ( I6, 1X, I4, 1X, A, 1X, A, 1X, A, 1X, A, &
     &              6X, F8.2, 7X, F9.7, &
     &              3X, A, 2X, A, 13X, F9.7, &
     &              8X, I6, 13X, F16.9,      &
     &              13X, D15.8, 13X, D15.8,  &
     &              13X,  D15.8, 13X, D15.8, &
     &              13X,  D15.8, 13X, D15.8, &
     &              13X,  D15.8, 13X, D15.8, &
     &              9X,   D15.8, 9X,  D15.8, &
     &              10X,  F9.5, 10X,  F9.5,  &
     &              10X,  F9.5, 10X,  F9.5,  &
     &              13X, D13.6, 13X,  D13.6, &
     &              13X, D13.6, 13X,  D13.6, &
     &              13X, D13.6, 13X,  D13.6, &
     &              13X, D13.6, 13X,  D13.6, &
     &              13X, D13.6, 10X,  D13.6, &
     &              13X, D13.6, 13X,  D13.6, &
     &              13X, D13.6, 13X,  D13.6, &
     &              12X, D15.8,              &
     &              11X,  F7.2, 12X, F9.7,   &
     &              5X, 2(D14.7,1X), 11X, F8.5, &
     &              9X, F6.3, 1X, F6.3, 6X, D10.4, &
     &              10X, D19.12, 2X,         &
     &              10X, 3(D19.12,2X),       &
     &              9X, D12.5, 9X, D12.5,    &
     &              7X, D12.5, 12X, D12.5,   &
     &              10X, D12.5, 15X, D12.5,  &
     &              10X, F6.3, 1X, F6.3, 12X, F6.3, 10X, F6.3, 7X, A2, 6X, B16 )
                PCAL_GDEL = 0.0
             ELSE IF ( LIN(421:427) == 'Gr_rat:' .AND. LIN(1351:1354) == 'Sts:' ) THEN
                READ (  UNIT=LIN, FMT=130 ) &
     &                  IND_OBS, IND_SCA, STR(61:70), STR(71:78), &
     &                  STR(81:88), STR(91:98), SNR, AMPL(PIMA__DRF), &
     &                  STR(1:23), STR(31:53), AMPL(PIMA__LSQ), &
     &                  NUM_EPC, TIME_FRT, GR_DEL, PH_RAT, GR_RAT, SB_DEL, PHS, &
     &                  GR_DEL_ERR, PH_RAT_ERR, GR_RAT_ERR, SB_DEL_ERR, &
     &                  PH_DEL_ERR, &
     &                  GRAMBSP, SCAN_DUR, AP_LEN, UVW, AMPL_INTG, &
     &                  EL, NOIR, FREQ_REF, EFF_FRQ_PHS, EFF_FRQ_GRP, &
     &                  EFF_FRQ_RAT, COV_PR_PH, COV_GR_MD, &
     &                  TEC, TEC_RATE, TEC_ERR, TEC_RATE_ERR, &
     &                  PAR_ANG, PA_USED, POLAR_USED, FRI_STS
 130       FORMAT ( I6, 1X, I4, 1X, A, 1X, A, 1X, A, 1X, A, &
     &              6X, F8.2, 7X, F9.7, &
     &              3X, A, 2X, A, 13X, F9.7, &
     &              8X, I6, 13X, F16.9,      &
     &              13X, D15.8, 13X, D15.8,  &
     &              13X,  D15.8, 13X, D15.8, &
     &              13X,  D15.8, 13X, D15.8, &
     &              13X,  D15.8, 13X, D15.8, &
     &              9X,   D15.8, 9X,  D15.8, &
     &              10X,  F9.5, 10X,  F9.5,  &
     &              10X,  F9.5, 10X,  F9.5,  &
     &              13X, D13.6, 13X,  D13.6, &
     &              13X, D13.6, 13X,  D13.6, &
     &              13X, D13.6, 13X,  D13.6, &
     &              13X, D13.6, 13X,  D13.6, &
     &              13X, D13.6, 10X,  D13.6, &
     &              13X, D13.6, 13X,  D13.6, &
     &              13X, D13.6, 13X,  D13.6, &
     &              12X, D15.8,              &
     &              11X,  F7.2, 12X, F9.7,   &
     &              5X, 2(D14.7,1X), 11X, F8.5, &
     &              9X, F6.3, 1X, F6.3, 6X, D10.4, &
     &              10X, D19.12, 2X,         &
     &              10X, 3(D19.12,2X),       &
     &              9X, D12.5, 9X, D12.5,    &
     &              7X, D12.5, 12X, D12.5,   &
     &              10X, D12.5, 15X, D12.5,  &
     &              10X, F6.3, 1X, F6.3, 10X, F6.3, 8X, A2, 6X, B16 )
                PCAL_GDEL = 0.0
             ELSE IF ( LIN(421:427) == 'Ph_acc:' .AND. LIN(1351:1354) == 'Sts:' ) THEN
                READ (  UNIT=LIN, FMT=130 ) &
     &                  IND_OBS, IND_SCA, STR(61:70), STR(71:78), &
     &                  STR(81:88), STR(91:98), SNR, AMPL(PIMA__DRF), &
     &                  STR(1:23), STR(31:53), AMPL(PIMA__LSQ), &
     &                  NUM_EPC, TIME_FRT, GR_DEL, PH_RAT, PH_ACC, SB_DEL, PHS, &
     &                  GR_DEL_ERR, PH_RAT_ERR, PH_ACC_ERR, SB_DEL_ERR, &
     &                  PH_DEL_ERR, &
     &                  GRAMBSP, SCAN_DUR, AP_LEN, UVW, AMPL_INTG, &
     &                  EL, NOIR, FREQ_REF, EFF_FRQ_PHS, EFF_FRQ_GRP, &
     &                  EFF_FRQ_RAT, COV_PR_PH, COV_GR_MD, &
     &                  TEC, TEC_RATE, TEC_ERR, TEC_RATE_ERR, &
     &                  PAR_ANG, PA_USED, POLAR_USED, FRI_STS
                PCAL_GDEL = 0.0
             ELSE IF ( LIN(420:426) == 'Ph_acc:' ) THEN
                READ (  UNIT=LIN, FMT=140, IOSTAT=IER ) &
     &                  IND_OBS, IND_SCA, STR(61:70), STR(71:78), &
     &                  STR(81:88), STR(91:98), SNR, AMPL(PIMA__DRF), &
     &                  STR(1:23), STR(31:53), AMPL(PIMA__LSQ), &
     &                  NUM_EPC, TIME_FRT, GR_DEL, PH_RAT, PH_ACC, SB_DEL, PHS, &
     &                  GR_DEL_ERR, PH_RAT_ERR, PH_ACC_ERR, SB_DEL_ERR, &
     &                  PH_DEL_ERR, &
     &                  GRAMBSP, SCAN_DUR, AP_LEN, UVW, AMPL_INTG, &
     &                  EL, NOIR, FREQ_REF, EFF_FRQ_PHS, EFF_FRQ_GRP, &
     &                  EFF_FRQ_RAT, COV_PR_PH, COV_GR_MD, &
     &                  TEC, TEC_RATE, TEC_ERR, TEC_RATE_ERR, &
     &                  POLAR_USED, PAR_ANG, PA_USED, FRI_STS
                PCAL_GDEL = 0.0
             ELSE IF ( LIN(418:424) == 'Gr_rat:' .AND. LIN(1207:1210) == 'Sts:' ) THEN
                READ (  UNIT=LIN, FMT=140, IOSTAT=IER ) &
     &                  IND_OBS, IND_SCA, STR(61:70), STR(71:78), &
     &                  STR(81:88), STR(91:98), SNR, AMPL(PIMA__DRF), &
     &                  STR(1:23), STR(31:53), AMPL(PIMA__LSQ), &
     &                  NUM_EPC, TIME_FRT, GR_DEL, PH_RAT, GR_RAT, SB_DEL, PHS, &
     &                  GR_DEL_ERR, PH_RAT_ERR, GR_RAT_ERR, SB_DEL_ERR, &
     &                  PH_DEL_ERR, &
     &                  GRAMBSP, SCAN_DUR, AP_LEN, UVW, AMPL_INTG, &
     &                  EL, NOIR, FREQ_REF, EFF_FRQ_PHS, EFF_FRQ_GRP, &
     &                  EFF_FRQ_RAT, COV_PR_PH, COV_GR_MD
                READ (  UNIT=LIN(1212:1227), FMT='(B16)', IOSTAT=IER ) FRI_STS
!!
                TEC          = 0.0D0
                TEC_RATE     = 0.0D0
                TEC_ERR      = 0.0D0
                TEC_RATE_ERR = 0.0D0
                PA_USED      = 0.0D0
                POLAR_USED   = '??'
                PAR_ANG = 0.0
                PA_USED = 0.0
                PCAL_GDEL = 0.0
                DECOR_TIM    = 0.0D0
             ELSE IF ( LIN(418:424) == 'Gr_rat:' .AND. LIN(1299:1302) .EQ. 'Sts:' ) THEN
                READ (  UNIT=LIN, FMT=150, IOSTAT=IER ) &
     &                  IND_OBS, IND_SCA, STR(61:70), STR(71:78), &
     &                  STR(81:88), STR(91:98), SNR, AMPL(PIMA__DRF), &
     &                  STR(1:23), STR(31:53), AMPL(PIMA__LSQ), &
     &                  NUM_EPC, TIME_FRT, GR_DEL, PH_RAT, GR_RAT, SB_DEL, PHS, &
     &                  GR_DEL_ERR, PH_RAT_ERR, GR_RAT_ERR, SB_DEL_ERR, &
     &                  PH_DEL_ERR, &
     &                  GRAMBSP, SCAN_DUR, AP_LEN, UVW, AMPL_INTG, &
     &                  EL, NOIR, FREQ_REF, EFF_FRQ_PHS, EFF_FRQ_GRP, &
     &                  EFF_FRQ_RAT, COV_PR_PH, COV_GR_MD, &
     &                  TEC, TEC_RATE, TEC_ERR, TEC_RATE_ERR, FRI_STS
                POLAR_USED = '??'
                PAR_ANG    = 0.0
                PA_USED    = 0.0
                PA_USED      = 0.0D0
                DECOR_TIM    = 0.0D0
                PCAL_GDEL = 0.0
             ELSE IF ( LIN(418:424) == 'Ph_acc:' .AND. LIN(1299:1302) .EQ. 'Sts:' ) THEN
                READ (  UNIT=LIN, FMT=150, IOSTAT=IER ) &
     &                  IND_OBS, IND_SCA, STR(61:70), STR(71:78), &
     &                  STR(81:88), STR(91:98), SNR, AMPL(PIMA__DRF), &
     &                  STR(1:23), STR(31:53), AMPL(PIMA__LSQ), &
     &                  NUM_EPC, TIME_FRT, GR_DEL, PH_RAT, PH_ACC, SB_DEL, PHS, &
     &                  GR_DEL_ERR, PH_RAT_ERR, PH_ACC_ERR, SB_DEL_ERR, &
     &                  PH_DEL_ERR, &
     &                  GRAMBSP, SCAN_DUR, AP_LEN, UVW, AMPL_INTG, &
     &                  EL, NOIR, FREQ_REF, EFF_FRQ_PHS, EFF_FRQ_GRP, &
     &                  EFF_FRQ_RAT, COV_PR_PH, COV_GR_MD, &
     &                  TEC, TEC_RATE, TEC_ERR, TEC_RATE_ERR, FRI_STS
                POLAR_USED = '??'
                PAR_ANG    = 0.0
                PA_USED    = 0.0
                DECOR_TIM  = 0.0D0
                PCAL_GDEL = 0.0
             ELSE IF ( LIN(1207:1210) == 'Sts:' ) THEN
                READ (  UNIT=LIN, FMT=150, IOSTAT=IER ) &
     &                  IND_OBS, IND_SCA, STR(61:70), STR(71:78), &
     &                  STR(81:88), STR(91:98), SNR, AMPL(PIMA__DRF), &
     &                  STR(1:23), STR(31:53), AMPL(PIMA__LSQ), &
     &                  NUM_EPC, TIME_FRT, GR_DEL, PH_RAT, PH_ACC, SB_DEL, PHS, &
     &                  GR_DEL_ERR, PH_RAT_ERR, PH_ACC_ERR, SB_DEL_ERR, &
     &                  PH_DEL_ERR, &
     &                  GRAMBSP, SCAN_DUR, AP_LEN, UVW, AMPL_INTG, &
     &                  EL, NOIR, FREQ_REF, EFF_FRQ_PHS, EFF_FRQ_GRP, &
     &                  EFF_FRQ_RAT, COV_PR_PH, COV_GR_MD, FRI_STS
                TEC          = 0.0D0
                TEC_RATE     = 0.0D0
                TEC_ERR      = 0.0D0
                TEC_RATE_ERR = 0.0D0
                POLAR_USED   = '??'
                PAR_ANG      = 0.0
                PA_USED      = 0.0
                DECOR_TIM    = 0.0D0
                PCAL_GDEL = 0.0
           END IF
 140       FORMAT ( I6, 1X, I4, 1X, A, 1X, A, 1X, A, 1X, A, &
     &              6X, F8.2, 7X, F9.7, &
     &              3X, A, 2X, A, 13X, F9.7, &
     &              7X, I4, 1X, 12X, F16.9, &
     &              13X, D15.8, 13X, D15.8, &
     &              13X,  D15.8, 13X, D15.8, &
     &              13X,  D15.8, 13X, D15.8, &
     &              13X,  D15.8, 13X, D15.8, &
     &              9X,   D15.8, 9X,  D15.8, &
     &              10X,  F9.5, 10X,  F9.5, &
     &              10X,  F9.5, 10X,  F9.5, &
     &              13X, D13.6, 13X,  D13.6, &
     &              13X, D13.6, 13X,  D13.6, &
     &              13X, D13.6, 13X,  D13.6, &
     &              13X, D13.6, 13X,  D13.6, &
     &              13X, D13.6, 10X,  D13.6, &
     &              13X, D13.6, 13X,  D13.6, &
     &              13X, D13.6, 13X,  D13.6, &
     &              12X, D15.8, &
     &              11X,  F7.2, 12X, F9.7, &
     &              5X, 2(D14.7,1X), 11X, F8.5, &
     &              9X, F6.3, 1X, F6.3, 6X, D10.4, &
     &              10X, D19.12,2X, &
     &              10X, 3(D19.12,2X), &
     &              9X, D12.5, 9X, D12.5, &
     &              9X, D12.5, 9X, D12.5, &
     &              6X, B16 )
 150       FORMAT ( I6, 1X, I4, 1X, A, 1X, A, 1X, A, 1X, A, &
     &              6X, F8.2, 7X, F9.7, &
     &              3X, A, 2X, A, 13X, F9.7, &
     &              7X, I4, 1X, 12X, F16.9, &
     &              13X, D15.8, 13X, D15.8, &
     &              13X,  D15.8, 13X, D15.8, &
     &              13X,  D15.8, 13X, D15.8, &
     &              13X,  D15.8, 13X, D15.8, &
     &              9X,   D15.8, 9X,  D15.8, &
     &              10X,  F9.5, 10X,  F9.5, &
     &              10X,  F9.5, 10X,  F9.5, &
     &              13X, D13.6, 13X,  D13.6, &
     &              13X, D13.6, 13X,  D13.6, &
     &              13X, D13.6, 13X,  D13.6, &
     &              13X, D13.6, 13X,  D13.6, &
     &              13X, D13.6, 10X,  D13.6, &
     &              13X, D13.6, 13X,  D13.6, &
     &              13X, D13.6, 13X,  D13.6, &
     &              12X, D15.8, &
     &              11X,  F7.2, 12X, F9.7, &
     &              5X, 2(D14.7,1X), 11X, F8.5, &
     &              9X, F6.3, 1X, F6.3, 6X, D10.4, &
     &              10X, D19.12,2X, &
     &              10X, 3(D19.12,2X), &
     &              9X,  D12.5, 9X,  D12.5, &
     &              6X,  D13.5, 10X, D13.5, &
     &              11X, D12.5, 15X, D12.5, &
     &              6X, B16 )
           IF ( IER .NE. 0 ) THEN
                CALL CLRCH ( STR )
                CALL INCH  ( IER, STR )
                CALL ERR_LOG ( 7384, IUER, 'PIMA_FRI_REA_OBS', 'Failure '// &
     &              'to read an observation record from the fringe file: '// &
     &              ' -- error '//STR )
                RETURN
           END IF
!
           IND_SOU    = LTM_DIF ( 1, PIM%NSOU, PIM%C_SOU, STR(71:78) )
           IND_STA(1) = LTM_DIF ( 1, PIM%NSTA, PIM%C_STA, STR(81:88) )
           IND_STA(2) = LTM_DIF ( 1, PIM%NSTA, PIM%C_STA, STR(91:98) )
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  PIMA_FRI_REA_OBS  !#!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE PIMA_FRI_READ ( PIM, IND_BND, PIMA_FRI_USED_VERS, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  PIMA_FRI_READ
! *                                                                      *
! *  ### 06-JUL-2009 PIMA_FRI_READ v4.1 (c)  L. Petrov  23-APR-2020 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'pima.i'
      TYPE     ( PIMA__TYPE  ) :: PIM
      CHARACTER  PIMA_FRI_USED_VERS*(*)
      INTEGER*4  IND_BND, IUER
      CHARACTER, ALLOCATABLE :: BUF(:)*1536
      INTEGER*4  MB
      PARAMETER  ( MB = 8192 )
      CHARACTER  STR*128, STR1*128, FRI_FILE*128, CONF_BUF(MB)*128, POLAR_USED*2
      LOGICAL*4  LEX
      REAL*8     SNR, AMPL(PIM__MFRA), AMPL_INTG, TIME_FRT, &
     &           GR_DEL(PIM__MFRA), PH_RAT(PIM__MFRA), GR_RAT, SB_DEL, &
     &           PHS(PIM__MFRA), AP_LEN, FREQ_REF, EFF_FRQ_PHS, EFF_FRQ_GRP, &
     &           EFF_FRQ_RAT, GRAMBSP, SCAN_DUR, &
     &           GR_DEL_ERR(PIM__MFRA), PH_RAT_ERR(PIM__MFRA), &
     &           PH_DEL_ERR(PIM__MFRA), PH_ACC, PH_ACC_ERR, &
     &           GR_RAT_ERR, SB_DEL_ERR, COV_PR_PH, COV_GR_MD, &
     &           TEC, TEC_RATE, TEC_ERR, TEC_RATE_ERR, PAR_ANG(2), &
     &           PA_USED, DECOR_TIM
      REAL*4     PCAL_GDEL(2,2)
      REAL*8     SNR_MIN
      PARAMETER  ( SNR_MIN = 1.D-8 )
      INTEGER*4  J1, J2, J3, J4, J5, NB, NBUF, IND_OBS, IND_OBS_2ND, &
     &           IND_OBS_1ST, IND_SCA, IND_SOU, IND_STA(2), &
     &           FRI_STS, LIN_1ST, IER
      INTEGER*4, EXTERNAL :: ILEN, I_LEN
!
      PIMA_FRI_USED_VERS = '????????????????????????'
!
      IF ( IND_BND < 1  .OR. IND_BND > PIM__MBND ) THEN
           CALL CLRCH ( STR  )
           CALL CLRCH ( STR1 )
           CALL INCH  ( IND_BND,   STR  )
           CALL INCH  ( PIM__MBND, STR1 )
           CALL ERR_LOG ( 7351, IUER, 'PIMA_FRI_READ', 'Wrong argument '// &
     &         'IND_BND: '//STR(1:I_LEN(STR))//' -- it should be in range '// &
     &         '[1, '//STR1(1:I_LEN(STR1))//']' )
           RETURN
      END IF
!
      CALL CLRCH ( FRI_FILE )
      IF ( IND_BND == 1 ) THEN
           FRI_FILE = PIM%CONF%FRINGE_FILE
         ELSE IF ( IND_BND == 2 ) THEN
           CALL ERR_PASS ( IUER, IER )
           ALLOCATE ( BUF(MB), STAT=IER )
           IF ( IER .NE. 0 ) THEN
                CALL CLRCH ( STR )
                CALL IINCH ( 2*PIM__MOBS*LEN(BUF(1)), STR )
                CALL ERR_LOG ( 7352, IUER, 'PIMA_FRI_READ', 'Failure to '// &
     &              'allocate '//STR(1:I_LEN(STR))//' bytes of dynamic '// &
     &              'memory for a temporary buffer of the contril file' )
                RETURN
           END IF
!
           CALL ERR_PASS ( IUER, IER )
           CALL RD_TEXT  ( PIM%CONF%MKDB_2ND_BAND_FILE, MB, BUF, NB, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 7353, IUER, 'PIMA_FRI_READ', 'Error in an '// &
     &              'attempt to read the control file for the second band '// &
     &               PIM%CONF%MKDB_2ND_BAND_FILE )
                DEALLOCATE ( BUF )
                RETURN
           END IF
!
           DO 410 J1=1,NB
              IF ( BUF(J1)(1:1) == '#' ) GOTO 410
              CALL CHASHL ( BUF(J1) )
!
              CALL ERR_PASS ( IUER, IER )
              CALL RESOLVE_ENV ( BUF(J1), IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 7354, IUER, 'PIMA_CONF', 'Failure in an attempt '// &
     &                 'to resolve environment varaibles when processing '// &
     &                 'the '//STR(1:I_LEN(STR))//'-th line of the control file '// &
     &                  TRIM(PIM%CONF%MKDB_2ND_BAND_FILE)//' -- "'// &
     &                  BUF(J1)(1:I_LEN(BUF(J1)))//'"' )
                   RETURN
              END IF
!
              IF ( BUF(J1)(1:LEN('FRINGE_FILE:')) == 'FRINGE_FILE:' ) THEN
                   CALL CLRCH ( BUF(J1)(1:LEN('FRINGE_FILE:')) )
                   CALL CHASHL ( BUF(J1) )
                   FRI_FILE = BUF(J1)
              END IF
 410       CONTINUE
           DEALLOCATE ( BUF )
           IF ( ILEN(FRI_FILE) == 0 ) THEN
                CALL ERR_LOG ( 7355, IUER, 'PIMA_FRI_READ', 'Strange, '// &
     &              'we searched the control file for the second band '// &
     &               PIM%CONF%MKDB_2ND_BAND_FILE(1:I_LEN(PIM%CONF%MKDB_2ND_BAND_FILE))// &
     &              ' , but did not there a line with definition of '// &
     &              'the fringe file' )
                RETURN
           END IF
      END IF
!
      INQUIRE ( FILE=FRI_FILE, EXIST=LEX )
      IF ( .NOT. LEX ) THEN
           CALL ERR_LOG ( 7356, IUER, 'PIMA_FRI_READ', 'Fringe file '// &
     &          FRI_FILE(1:I_LEN(FRI_FILE))// &
     &          ' is not found' )
           RETURN
      END IF
!
      ALLOCATE ( BUF(8*PIM__MOBS), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( 2*PIM__MOBS*LEN(BUF(1)), STR )
           CALL ERR_LOG ( 7357, IUER, 'PIMA_FRI_READ', 'Failure to '// &
     &         'allocate '//STR(1:I_LEN(STR))//' bytes of dynamic memory '// &
     &         'for a temporary buffer of the fringe file' )
           RETURN
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL RD_TEXT  ( FRI_FILE, 8*PIM__MOBS, BUF, NBUF, IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL ERR_LOG ( 7358, IUER, 'PIMA_FRI_READ', 'Failure to read '// &
     &         'the fringe file '//FRI_FILE )
           DEALLOCATE ( BUF )
           RETURN
      END IF
!
      IF ( BUF(1)(1:LEN(PIMA__FRIRES_LABEL)) == PIMA__FRIRES_LABEL ) THEN
           CONTINUE
         ELSE IF ( BUF(1)(1:LEN(PIMA__FRIRES_LABEL_20100405)) == PIMA__FRIRES_LABEL_20100405 ) THEN
           CONTINUE
         ELSE IF ( BUF(1)(1:LEN(PIMA__FRIRES_LABEL_20140208)) == PIMA__FRIRES_LABEL_20140208 ) THEN
           CONTINUE
         ELSE IF ( BUF(1)(1:LEN(PIMA__FRIRES_LABEL_20141224)) == PIMA__FRIRES_LABEL_20141224 ) THEN
           CONTINUE
         ELSE IF ( BUF(1)(1:LEN(PIMA__FRIRES_LABEL_20190224)) == PIMA__FRIRES_LABEL_20190224 ) THEN
           CONTINUE
         ELSE IF ( BUF(1)(1:LEN(PIMA__FRIRES_LABEL_20190420)) == PIMA__FRIRES_LABEL_20190420 ) THEN
           CONTINUE
         ELSE
           CALL CLRCH ( STR )
           CALL TRAN  ( 13, BUF(1), STR )
           CALL ERR_LOG ( 7359, IUER, 'PIMA_FRI_READ', 'Failure to '// &
     &         'interpret the fringe file '// &
     &          FRI_FILE(1:I_LEN(FRI_FILE))// &
     &         ' -- its first line is '//STR(1:LEN(PIMA__FRIRES_LABEL))// &
     &         ' while '//PIMA__FRIRES_LABEL//' was expected' )
           DEALLOCATE ( BUF )
           RETURN
      END IF
      IF ( NBUF > 4 ) THEN
           IF ( BUF(4)(1:8) == '# PIMA v' ) THEN
                PIMA_FRI_USED_VERS = BUF(4)(3:2+LEN(PIMA__LABEL))
              ELSE
                PIMA_FRI_USED_VERS = 'PIMA v 1.30   2012.08.14'
           END IF
         ELSE
           PIMA_FRI_USED_VERS = 'PIMA v 1.30   2012.08.14'
      END IF
!
! --- Initialization
!
      FRI_STS = 0
      FRI_STS = IBSET ( FRI_STS, FAI__PIM )
      FRI_STS = IBSET ( FRI_STS, NDT__PIM )
!
      DO 420 J2=1,PIM%NOBS
         PIM%OBS(J2)%AMPL(PIMA__DRF,IND_BND) = -1.0D10
         PIM%OBS(J2)%AMPL(PIMA__LSQ,IND_BND) = -1.0D10
         PIM%OBS(J2)%NOISE(IND_BND)          = -1.0D10
         PIM%OBS(J2)%FRT_OFFSET(IND_BND)     = -1.0D10
         PIM%OBS(J2)%RES_MB_DEL(1:PIM__MFRA,IND_BND) = -1.0D10
         PIM%OBS(J2)%RES_PH_RAT(1:PIM__MFRA,IND_BND) = -1.0D10
         PIM%OBS(J2)%RES_PHS(1:PIM__MFRA,IND_BND)    = -1.0D10
         PIM%OBS(J2)%RES_SB_DEL(IND_BND) = -1.0D10
         PIM%OBS(J2)%RES_GR_RAT(IND_BND) = -1.0D10
         PIM%OBS(J2)%RES_PH_ACC(IND_BND) = -1.0D10
!
         PIM%OBS(J2)%MB_DEL_ERR(1:PIM__MFRA,IND_BND) = -1.0D10
         PIM%OBS(J2)%PH_DEL_ERR(1:PIM__MFRA,IND_BND) = -1.0D10
         PIM%OBS(J2)%PH_RAT_ERR(1:PIM__MFRA,IND_BND) = -1.0D10
         PIM%OBS(J2)%SB_DEL_ERR(IND_BND)  = -1.0D10
         PIM%OBS(J2)%GR_RAT_ERR(IND_BND)  = -1.0D10
         PIM%OBS(J2)%PH_ACC_ERR(IND_BND)  = -1.0D10
         PIM%OBS(J2)%COV_PR_PH(IND_BND)   = -1.0D10
         PIM%OBS(J2)%COV_GR_GD(IND_BND)   = -1.0D10
         PIM%OBS(J2)%SCAN_DURA(IND_BND)   =  0.0D0
         PIM%OBS(J2)%GRAMBSP(IND_BND)     = -1.0D10
!
         PIM%OBS(J2)%REF_FREQ(IND_BND)    = -1.0D0
         PIM%OBS(J2)%EFF_FRQ(1,IND_BND)   = -1.0D10
         PIM%OBS(J2)%EFF_FRQ(2,IND_BND)   = -1.0D10
         PIM%OBS(J2)%EFF_FRQ(3,IND_BND)   = -1.0D10
         PIM%OBS(J2)%FRT_OFFSET(IND_BND)  =  0.0D0
         PIM%OBS(J2)%TEC                  =  0.0D0
         PIM%OBS(J2)%TEC_RATE             =  0.0D0
         PIM%OBS(J2)%TEC_ERR              =  0.0D0
         PIM%OBS(J2)%TEC_RATE_ERR         =  0.0D0
         PIM%OBS(J2)%POLAR_USED           = '??'
         PIM%OBS(J2)%PA_USED              =  0.0D0
         PIM%OBS(J2)%DECOR_TIM            =  0.0D0
         PIM%OBS(J2)%PCAL_GDEL            =  0.0
         PIM%OBS(J2)%FRI_STS(IND_BND)     = FRI_STS
 420  CONTINUE
!
      DO 440 J4=1,NBUF
         IF ( BUF(J4)(1:1)  == '#' ) GOTO 440
         IF ( ILEN(BUF(J4)) ==  0  ) GOTO 440
!
         CALL ERR_PASS ( IUER, IER )
         CALL PIMA_FRI_REA_OBS ( PIM, BUF(J4), IND_OBS, IND_SCA, &
     &                           IND_SOU, IND_STA, SNR, AMPL, AMPL_INTG, &
     &                           TIME_FRT, GR_DEL, PH_RAT, GR_RAT, PH_ACC, &
     &                           SB_DEL, PHS, GR_DEL_ERR, PH_RAT_ERR,   &
     &                           GR_RAT_ERR, PH_ACC_ERR, SB_DEL_ERR, &
     &                           PH_DEL_ERR, GRAMBSP, SCAN_DUR, AP_LEN, &
     &                           FREQ_REF, EFF_FRQ_PHS, EFF_FRQ_GRP, &
     &                           EFF_FRQ_RAT, COV_PR_PH, COV_GR_MD, &
     &                           TEC, TEC_RATE, TEC_ERR, TEC_RATE_ERR, &
     &                           POLAR_USED, PAR_ANG, PA_USED, DECOR_TIM, &
     &                           PCAL_GDEL, FRI_STS, IER )
         IF ( IER .NE. 0 ) THEN
              CALL CLRCH ( STR )
              CALL INCH  ( J4, STR )
              CALL ERR_LOG ( 7360, IUER, 'PIMA_FRI_READ', 'Failure to '// &
     &            'interpret the line '//STR(1:I_LEN(STR))//' of the '// &
     &            'fringe file '//FRI_FILE )
              DEALLOCATE ( BUF )
              RETURN
         END IF
         IF ( IND_BND == 2 ) THEN
              IF ( BTEST ( FRI_STS, NDA__PIM ) ) THEN

! ---------------- No data for the second band.
! ---------------- NB: the observation index for the 1st and the 2nd band
! ---------------- may be different. The observation at the 1st band
! ---------------- may have station order AB, and at the 2nd band BA.
! ---------------- Let us explore this opportunity.
!
                   IND_OBS_1ST = IND_OBS
                   IND_OBS_2ND = 0
                   LIN_1ST = J4
                   IF ( IND_OBS_1ST > 1 ) THEN
!
! --------------------- Search the data till we find the matching record
!
                        DO 450 J5=1,NB
                           IF ( BUF(J5)(1:1)  == '#' ) GOTO 450
                           IF ( ILEN(BUF(J5)) ==  0  ) GOTO 450
!
                           CALL ERR_PASS ( IUER, IER )
                           CALL PIMA_FRI_REA_OBS ( PIM, BUF(J5), IND_OBS, IND_SCA, &
     &                           IND_SOU, IND_STA, SNR, AMPL, AMPL_INTG, &
     &                           TIME_FRT, GR_DEL, PH_RAT, GR_RAT, PH_ACC, &
     &                           SB_DEL, PHS, GR_DEL_ERR, PH_RAT_ERR, GR_RAT_ERR, &
     &                           PH_ACC_ERR, SB_DEL_ERR, PH_DEL_ERR, &
     &                           GRAMBSP, SCAN_DUR, AP_LEN, FREQ_REF, &
     &                           EFF_FRQ_PHS, EFF_FRQ_GRP, EFF_FRQ_RAT, &
     &                           COV_PR_PH, COV_GR_MD, TEC, TEC_RATE, &
     &                           TEC_ERR, TEC_RATE_ERR, POLAR_USED, PAR_ANG, PA_USED, &
     &                           DECOR_TIM, PCAL_GDEL, FRI_STS, IER )
                           IF ( IER .NE. 0 ) THEN
                                CALL CLRCH ( STR )
                                CALL INCH  ( J5, STR )
                                CALL ERR_LOG ( 7361, IUER, 'PIMA_FRI_READ', &
     &                                         'Failure to interpret the '// &
     &                                         'line '//STR(1:I_LEN(STR))// &
     &                                         ' of the fringe file '// &
     &                                          FRI_FILE )
                                DEALLOCATE ( BUF )
                                RETURN
                           END IF
                           IF ( IND_SCA    == PIM%OBS(IND_OBS_1ST)%SCA_IND    .AND. &
     &                          IND_STA(1) == PIM%OBS(IND_OBS_1ST)%STA_IND(2) .AND. &
     &                          IND_STA(2) == PIM%OBS(IND_OBS_1ST)%STA_IND(1)       ) THEN
!
                                IND_OBS_2ND = IND_OBS
                                PIM%OBS(IND_OBS_1ST)%IND_OBS_2ND = IND_OBS_2ND
                                PIM%OBS(IND_OBS_2ND)%IND_OBS_2ND = IND_OBS_1ST
                           END IF
 450                    CONTINUE
                   END IF
                   IF ( IND_OBS_2ND == 0 ) THEN
                        IND_OBS = IND_OBS_1ST
!
! --------------------- Read again, since during previous attempts to read
! --------------------- the data we overwrote FRI_STS, SNR and other parameters
!
                        IER = 0
                        CALL PIMA_FRI_REA_OBS ( PIM, BUF(J4), IND_OBS, IND_SCA, &
     &                           IND_SOU, IND_STA, SNR, AMPL, AMPL_INTG, &
     &                           TIME_FRT, GR_DEL, PH_RAT, GR_RAT, PH_ACC, SB_DEL, &
     &                           PHS, GR_DEL_ERR, PH_RAT_ERR, GR_RAT_ERR, &
     &                           PH_ACC_ERR,SB_DEL_ERR, PH_DEL_ERR, &
     &                           GRAMBSP, SCAN_DUR, AP_LEN, FREQ_REF, &
     &                           EFF_FRQ_PHS, EFF_FRQ_GRP, EFF_FRQ_RAT, &
     &                           COV_PR_PH, COV_GR_MD, TEC, TEC_RATE, TEC_ERR, &
     &                           TEC_RATE_ERR, POLAR_USED, PAR_ANG, PA_USED, &
     &                           DECOR_TIM, PCAL_GDEL, FRI_STS, IER )
                   END IF
                 ELSE
                   PIM%OBS(IND_OBS)%IND_OBS_2ND = IND_OBS
              END IF
         END IF
!
         FRI_STS = IBSET ( FRI_STS, REA__PIM )
         IF ( SNR > 1.D-8 ) THEN
              PIM%OBS(IND_OBS)%AMPL(PIMA__DRF,IND_BND) = AMPL(PIMA__DRF)
              PIM%OBS(IND_OBS)%AMPL(PIMA__LSQ,IND_BND) = AMPL(PIMA__LSQ)
              PIM%OBS(IND_OBS)%NOISE(IND_BND)      = AMPL(PIMA__DRF)/SNR
              PIM%OBS(IND_OBS)%FRT_OFFSET(IND_BND) = TIME_FRT
              PIM%OBS(IND_OBS)%RES_MB_DEL(1:PIM__MFRA,IND_BND) = GR_DEL
              PIM%OBS(IND_OBS)%RES_PH_RAT(1:PIM__MFRA,IND_BND) = PH_RAT
              PIM%OBS(IND_OBS)%RES_PHS(1:PIM__MFRA,IND_BND)    = PHS
              PIM%OBS(IND_OBS)%RES_SB_DEL(IND_BND) = SB_DEL
              PIM%OBS(IND_OBS)%RES_GR_RAT(IND_BND) = GR_RAT
              PIM%OBS(IND_OBS)%RES_PH_ACC(IND_BND) = PH_ACC
!
              PIM%OBS(IND_OBS)%MB_DEL_ERR(1:PIM__MFRA,IND_BND) = GR_DEL_ERR
              PIM%OBS(IND_OBS)%PH_DEL_ERR(1:PIM__MFRA,IND_BND) = PH_DEL_ERR
              PIM%OBS(IND_OBS)%PH_RAT_ERR(1:PIM__MFRA,IND_BND) = PH_RAT_ERR
              PIM%OBS(IND_OBS)%SB_DEL_ERR(IND_BND)  = SB_DEL_ERR
              PIM%OBS(IND_OBS)%GR_RAT_ERR(IND_BND)  = GR_RAT_ERR
              PIM%OBS(IND_OBS)%PH_ACC_ERR(IND_BND)  = PH_ACC_ERR
              PIM%OBS(IND_OBS)%COV_PR_PH(IND_BND)   = COV_PR_PH
              PIM%OBS(IND_OBS)%COV_GR_GD(IND_BND)   = COV_GR_MD
              PIM%OBS(IND_OBS)%SCAN_DURA(IND_BND)   = SCAN_DUR
              PIM%OBS(IND_OBS)%EFF_DUR(IND_BND)     = SCAN_DUR
              PIM%OBS(IND_OBS)%GRAMBSP(IND_BND)     = GRAMBSP
!
              PIM%OBS(IND_OBS)%REF_FREQ(IND_BND)    = FREQ_REF
              PIM%OBS(IND_OBS)%EFF_FRQ(1,IND_BND)   = EFF_FRQ_GRP
              PIM%OBS(IND_OBS)%EFF_FRQ(2,IND_BND)   = EFF_FRQ_PHS
              PIM%OBS(IND_OBS)%EFF_FRQ(3,IND_BND)   = EFF_FRQ_RAT
              PIM%OBS(IND_OBS)%TEC                  = TEC
              PIM%OBS(IND_OBS)%TEC_RATE             = TEC_RATE
              PIM%OBS(IND_OBS)%TEC_ERR              = TEC_ERR
              PIM%OBS(IND_OBS)%TEC_RATE_ERR         = TEC_RATE_ERR
              PIM%OBS(IND_OBS)%POLAR_USED           = POLAR_USED
              PIM%OBS(IND_OBS)%PA_USED              = PA_USED
              PIM%OBS(IND_OBS)%DECOR_TIM            = DECOR_TIM
              PIM%OBS(IND_OBS)%PCAL_GDEL            = PCAL_GDEL
              PIM%OBS(IND_OBS)%FRI_STS(IND_BND)     = FRI_STS
         END IF
 440  CONTINUE
!
      PIM%FRI_STATUS = PIMA__LOADED
      DEALLOCATE ( BUF )
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE PIMA_FRI_READ  !#!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE PIMA_RESID_WRI_HEAD ( PIM, LUN, IUER )
! ************************************************************************
! *                                                                      *
! *  Routine  PIMA_RESID_WRI_HEAD
! *                                                                      *
! * ## 22-AUG-2009 PIMA_RESID_WRI_HEAD v1.0 (c) L. Petrov 22-AUG-2009 ## *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'astro_constants.i'
      INCLUDE   'pima.i'
      TYPE     ( PIMA__TYPE  ) :: PIM
      INTEGER*4  LUN, IUER
      CHARACTER  STR*128
      INTEGER*4  IER
      CHARACTER, EXTERNAL :: GET_CDATE*19
      INTEGER*4, EXTERNAL :: GET_UNIT
      INTEGER*4, EXTERNAL :: ILEN, I_LEN
!
      LUN = GET_UNIT()
!
      OPEN ( UNIT=LUN, FILE=PIM%CONF%FRIRES_FILE, STATUS='UNKNOWN', &
     &       POSITION='APPEND', IOSTAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 7321, IUER, 'PIMA_RESID_WRI_HEAD', 'Failure to '// &
     &          'open output file with the fringe output table '// &
     &          PIM%CONF%FRIRES_FILE(1:I_LEN(PIM%CONF%FRIRES_FILE))// &
     &          ' -- error '//STR )
           RETURN
      END IF
!
      WRITE ( UNIT=LUN, FMT='(A)', IOSTAT=IER ) PIMA__RESID_LABEL
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 7322, IUER, 'PIMA_RESID_WRI_HEAD', 'Failure to '// &
     &          'write output file with the fringe output table  '// &
     &          PIM%CONF%FRIRES_FILE(1:I_LEN(PIM%CONF%FRIRES_FILE))// &
     &          ' -- error '//STR )
           RETURN
      END IF
!
      WRITE ( UNIT=LUN, FMT='(A)', IOSTAT=IER ) '# '
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 7323, IUER, 'PIMA_RESID_WRI_HEAD', 'Failure to '// &
     &          'write output file with the fringe output table '// &
     &          PIM%CONF%FRIRES_FILE(1:I_LEN(PIM%CONF%FRIRES_FILE))// &
     &          ' -- error '//STR )
           RETURN
      END IF
!
      WRITE ( UNIT=LUN, FMT='(A)', IOSTAT=IER ) '# PIMA_FRINGE started on '// &
     &                                           GET_CDATE()
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( IER, STR )
           CALL ERR_LOG ( 7324, IUER, 'PIMA_RESID_WRI_HEAD', 'Failure to '// &
     &          'write in output file with SNR '// &
     &          PIM%CONF%FRIRES_FILE(1:I_LEN(PIM%CONF%FRIRES_FILE))// &
     &          ' -- error '//STR )
           RETURN
      END IF
!
      WRITE ( UNIT=LUN, FMT='(A)', IOSTAT=IER ) '# '//PIMA__LABEL
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 7325, IUER, 'PIMA_RESID_WRI_HEAD', 'Failure to '// &
     &          'write output file with the fringe output table '// &
     &          PIM%CONF%FRIRES_FILE(1:I_LEN(PIM%CONF%FRIRES_FILE))// &
     &          ' -- error '//STR )
           RETURN
      END IF
!
      WRITE ( UNIT=LUN, FMT='(A)', IOSTAT=IER ) '# '
      WRITE ( UNIT=LUN, FMT='(A)', IOSTAT=IER ) '# Control file:     '// &
     &                  PIM%CONF_FILE(1:I_LEN(PIM%CONF_FILE))
      WRITE ( UNIT=LUN, FMT='(A)', IOSTAT=IER ) '# Experiment code:  '// &
     &                  PIM%OBS_CODE(1:I_LEN(PIM%OBS_CODE))
      WRITE ( UNIT=LUN, FMT='(A)', IOSTAT=IER ) '# Session code:     '// &
     &                  PIM%CONF%SESS_CODE(1:I_LEN(PIM%CONF%SESS_CODE))
      WRITE ( UNIT=LUN, FMT='(A,I3)', IOSTAT=IER ) '# Number of freqs: ', &
     &                  PIM%CONF%END_FRQ - PIM%CONF%BEG_FRQ + 1
      WRITE ( UNIT=LUN, FMT='(A,I3)', IOSTAT=IER ) '# First frequency: ', &
     &                  PIM%CONF%BEG_FRQ
      WRITE ( UNIT=LUN, FMT='(A,I3)', IOSTAT=IER ) '# Last frequency:  ', &
     &                  PIM%CONF%END_FRQ
      WRITE ( UNIT=LUN, FMT='(A)', IOSTAT=IER ) '# '
!
      CALL FLUSH ( LUN )
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  PIMA_RESID_WRI_HEAD  !#!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE PIMA_RESID_WRI_OBS ( PIM, LUN, IND_OBS, SNR, AMPL, &
     &                                FRI_STS, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  PIMA_RESID_WRI_OBS
! *                                                                      *
! * ## 06-JUL-2009 PIMA_RESID_WRI_OBS v1.2 (c) L. Petrov  03-MAY-2020 ## *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'astro_constants.i'
      INCLUDE   'pima.i'
      TYPE     ( PIMA__TYPE  ) :: PIM
      INTEGER*4  LUN, IND_OBS, FRI_STS, IUER
      REAL*8     SNR, AMPL(PIM__MFRA)
      CHARACTER  STR*4096, STR_DAT_BEG*30, STR_DAT_END*30
      INTEGER*4  NUM_FRQ, J1, J2, IL, FRG_IND, IER
      REAL*4,    EXTERNAL :: PHAS_CMPL_R4
      CHARACTER, EXTERNAL :: MJDSEC_TO_DATE*30, GET_CDATE*19
      INTEGER*4, EXTERNAL :: GET_UNIT
      INTEGER*4, EXTERNAL :: ILEN, I_LEN
!
      FRG_IND = PIM%OBS(IND_OBS)%REF_FRG_INDS(PIM%CONF%FRQ_GRP)
      STR_DAT_BEG = MJDSEC_TO_DATE ( PIM%MJD_0, PIM%TAI_0 + &
     &                               PIM%TIM_R8(PIM%UV_IND(PIM%OBS(IND_OBS)%UV_IND(1,FRG_IND))%TIM_IND), -3 )
      STR_DAT_END = MJDSEC_TO_DATE ( PIM%MJD_0, PIM%TAI_0 + &
     &                               PIM%TIM_R8(PIM%UV_IND(PIM%OBS(IND_OBS)%UV_IND(PIM%OBS(IND_OBS)%NUM_EPC(FRG_IND),FRG_IND))%TIM_IND), -3 )
      IF ( SNR > 0.0D0 ) THEN
           NUM_FRQ = 0
!
! -------- Determine the number of frequencies that contributed to fringe fit
!
           DO 410 J1=PIM%CONF%BEG_FRQ,PIM%CONF%END_FRQ
              IF ( ABS(PIM%OBS(IND_OBS)%RES_FRN(J1,1)) > PIMA__AMP_MIN ) THEN
                   NUM_FRQ = NUM_FRQ + 1
              END IF
 410       CONTINUE
!
           CALL CLRCH ( STR )
           WRITE ( UNIT=STR, FMT=110, IOSTAT=IER ) &
     &             IND_OBS, PIM%OBS(IND_OBS)%SCA_IND, &
     &             PIM%SCA(PIM%OBS(IND_OBS)%SCA_IND)%SCAN_NAME(1:10), &
     &             PIM%SOU(PIM%OBS(IND_OBS)%SOU_IND)%IVS_NAME, &
     &             PIM%STA(PIM%OBS(IND_OBS)%STA_IND(1))%IVS_NAME, &
     &             PIM%STA(PIM%OBS(IND_OBS)%STA_IND(2))%IVS_NAME, &
     &             SNR, AMPL(PIMA__DRF), STR_DAT_BEG(1:23), STR_DAT_END(1:23), NUM_FRQ
 110       FORMAT ( I6, ' | ', I4, 1X, A, 1X, A, 1X, A, 1X, A, &
     &              ' SNR: ', 0PF8.2, ' Ampl: ', 0PF9.7, &
     &              ' [ ', A, ', ', A, ' ]  Num_frq: ', I3 )
           DO 420 J2=PIM%CONF%BEG_FRQ,PIM%CONF%END_FRQ
              IF ( ABS(PIM%OBS(IND_OBS)%RES_FRN(J2,1)) > PIMA__AMP_MIN ) THEN
!
! ---------------- Use only frequencies that had minimum of data
!
                   IL = ILEN(STR)+3
                   WRITE ( UNIT=STR(IL:), FMT='("  I_FRQ: ", I3 )' ) J2
!
                   IL = ILEN(STR)+2
                   WRITE ( UNIT=STR(IL:), FMT='("  AMP: ", 1PE11.4 )' ) &
     &                     ABS(PIM%OBS(IND_OBS)%RES_FRN(J2,1))
!
                   IL = ILEN(STR)+2
                   WRITE ( UNIT=STR(IL:), FMT='("  PHS: ", F8.5 )' ) &
     &                     PHAS_CMPL_R4 ( PIM%OBS(IND_OBS)%RES_FRN(J2,1) )
!
                   IL = ILEN(STR)+2
                   WRITE ( UNIT=STR(IL:), FMT='("  EFD: ", F9.4 )' ) &
     &                     PIM%OBS(IND_OBS)%EFF_DUR(1)
              END IF
 420       CONTINUE
           WRITE ( UNIT=LUN, FMT='(A)' ) STR(1:I_LEN(STR))
         ELSE
           CALL CLRCH ( STR )
           STR = 'FAILURE '
           WRITE ( UNIT=LUN, FMT=120, IOSTAT=IER ) &
     &             IND_OBS, PIM%OBS(IND_OBS)%SCA_IND, &
     &             PIM%SCA(PIM%OBS(IND_OBS)%SCA_IND)%SCAN_NAME(1:10), &
     &             PIM%SOU(PIM%OBS(IND_OBS)%SOU_IND)%IVS_NAME, &
     &             PIM%STA(PIM%OBS(IND_OBS)%STA_IND(1))%IVS_NAME, &
     &             PIM%STA(PIM%OBS(IND_OBS)%STA_IND(2))%IVS_NAME, &
     &             STR(1:8), FRI_STS, STR_DAT_BEG(1:23), STR_DAT_END(1:23)
 120       FORMAT ( I6, ' | ', I4, 1X, A, 1X, A, 1X, A, 1X, A, &
     &              1X, A8, 2X, B16, 3X, ' [ ', A, ', ', A, ' ]' )
      END IF
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( IER, STR )
           CALL ERR_LOG ( 7331, IUER, 'PIMA_RESID_WRI_OBS', 'Failure '// &
     &          'to write an observation record in output file with SNR '// &
     &          PIM%CONF%FRIRES_FILE(1:I_LEN(PIM%CONF%FRIRES_FILE))// &
     &          ' -- error '//STR )
           RETURN
      END IF
      CALL FLUSH ( LUN )
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  PIMA_RESID_WRI_OBS  !#!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE PIMA_RESID_WRI_TAIL ( PIM, LUN, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  PIMA_RESID_WRI_TAIL
! *                                                                      *
! * ## 06-JUL-2009 PIMA_RESID_WRI_TAIL v1.0 (c) L. Petrov 06-JUL-2009 ## *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'astro_constants.i'
      INCLUDE   'pima.i'
      TYPE     ( PIMA__TYPE  ) :: PIM
      INTEGER*4  LUN, IUER
      CHARACTER  STR*128
      INTEGER*4  IER
      CHARACTER, EXTERNAL :: GET_CDATE*19
      INTEGER*4, EXTERNAL :: ILEN, I_LEN
!
      WRITE ( UNIT=LUN, FMT='(A)', IOSTAT=IER ) '# '
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 7391, IUER, 'PIMA_RESID_WRI_TAIL', 'Failure '// &
     &          'to write output file with SNR '// &
     &          PIM%CONF%FRIRES_FILE(1:I_LEN(PIM%CONF%FRIRES_FILE))// &
     &          ' -- error '//STR )
           RETURN
      END IF
!
      WRITE ( UNIT=LUN, FMT='(A)', IOSTAT=IER ) '# PIMA_FRIRES  ended on '// &
     &                                           GET_CDATE()
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( IER, STR )
           CALL ERR_LOG ( 7392, IUER, 'PIMA_RESID_WRI_TAIL', 'Failure '// &
     &          'to write in output file with SNR '// &
     &          PIM%CONF%FRIRES_FILE(1:I_LEN(PIM%CONF%FRIRES_FILE))// &
     &          ' -- error '//STR )
           RETURN
      END IF
      CLOSE ( UNIT=LUN )
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  PIMA_RESID_WRI_TAIL  !#!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE PIMA_RESID_REA_OBS ( PIM, BUF, IND_BND, IND_OBS, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  PIMA_RESID_REA_OBS
! *                                                                      *
! * ## 06-JUL-2009 PIMA_RESID_REA_OBS v1.0 (c) L. Petrov  06-JUL-2009 ## *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'astro_constants.i'
      INCLUDE   'pima.i'
      TYPE     ( PIMA__TYPE  ) :: PIM
      CHARACTER  BUF*(*)
      INTEGER*4  IND_BND, IND_OBS, IUER
      REAL*4     AMP, PHS, EFD
      CHARACTER  STR*128, STR1*128, STR2*128
      INTEGER*4  NUM_FRQ, I_FRQ, IB, IB_OLD, J1, IER
      INTEGER*4, EXTERNAL :: ILEN, I_LEN
!
      READ ( UNIT=BUF(1:6), FMT='(I6)', IOSTAT=IER ) IND_OBS
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 8511, IUER, 'PIMA_RESID_REA_OBS', 'Failure '// &
     &         'to decode in observation index '//BUF(1:6) )
           RETURN
      END IF
      IF ( IND_OBS < 1  .OR. IND_OBS > PIM%NOBS ) THEN
           CALL CLRCH ( STR  )
           CALL CLRCH ( STR1 )
           CALL INCH ( IND_OBS,  STR  )
           CALL INCH ( PIM%NOBS, STR1 )
           CALL ERR_LOG ( 8512, IUER, 'PIMA_RESID_REA_OBS', 'Wrong observation '// &
     &         'index '//TRIM(STR)//' while it expected to be in range '// &
     &         '[1, '//TRIM(STR1)//']' )
           RETURN
      END IF
      IB = INDEX ( BUF, 'FAILURE' )
      IF ( IB > 0 ) THEN
           CALL ERR_LOG ( 0, IUER )
           RETURN
      END IF
      IF ( IND_BND == 2 ) THEN
           IND_OBS = PIM%OBS(IND_OBS)%IND_OBS_2ND
           IF ( IND_OBS == 0 ) THEN
                CALL ERR_LOG ( 0, IUER )
                RETURN
           END IF
      END IF
!
      IB = INDEX ( BUF, 'Num_frq:' )
      IF ( IB .LE. 0 ) THEN
           CALL ERR_LOG ( 8515, IUER, 'PIMA_RESID_REA_OBS', 'Could not '// &
     &         'find substring Num_frq' )
           RETURN
      END IF
!
      READ ( UNIT=BUF(IB+9:IB+11), FMT='(I3)', IOSTAT=IER ) NUM_FRQ
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 8516, IUER, 'PIMA_RESID_REA_OBS', 'Failure '// &
     &         'to decode the number of frequencies '//BUF(IB+9:IB+11) )
           RETURN
      END IF
      IF ( NUM_FRQ == 0 ) THEN
!
! -------- Nothing to do
!
           CALL ERR_LOG ( 0, IUER )
           RETURN
      END IF
!
      IF ( NUM_FRQ < 1  .OR.  NUM_FRQ > PIM%NFRQ ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( PIM%NFRQ, STR )
           CALL ERR_LOG ( 8518, IUER, 'PIMA_RESID_REA_OBS', 'Wrong number '// &
     &         'of frequencies: '//BUF(IB+9:IB+11)//' -- it should be in '// &
     &         'the range [1, '//STR(1:I_LEN(STR))//']' )
           RETURN
      END IF
!
      DO 410 J1=1,NUM_FRQ
         IB_OLD = IB
         IB = INDEX ( BUF(IB:), 'I_FRQ:' ) + IB-1
         IF ( IB .LE. IB_OLD ) THEN
              CALL ERR_LOG ( 8519, IUER, 'PIMA_RESID_REA_OBS', 'Could not '// &
     &            'find substring I_FRQ:' )
              RETURN
         END IF
!
         READ ( UNIT=BUF(IB+7:IB+11), FMT='(I3)', IOSTAT=IER ) I_FRQ
         IF ( IER .NE. 0 ) THEN
              write ( 6, * ) ' j1 = ', j1, ' ib = ', ib ! %%%
              CALL ERR_LOG ( 8520, IUER, 'PIMA_RESID_REA_OBS', 'Failure '// &
     &         'to decode the number of frequencies '//BUF(IB+9:IB+11) )
              RETURN
         END IF
         IF ( I_FRQ < 1  .OR. I_FRQ > PIM%NFRQ ) THEN
              CALL CLRCH ( STR )
              CALL INCH  ( J1, STR )
              CALL CLRCH ( STR1 )
              CALL INCH  ( IB+7, STR1 )
              CALL CLRCH ( STR2 )
              CALL INCH  ( PIM%NFRQ, STR2 )
              CALL ERR_LOG ( 8521, IUER, 'PIMA_RESID_REA_OBS', 'Wrong '// &
     &             STR(1:I_LEN(STR))//' -th frequency index: '// &
     &             BUF(IB+9:IB+11)//' at position '//STR1(1:I_LEN(STR1))// &
     &             ' -- it should be in the range [1, '// &
     &             STR2(1:I_LEN(STR2))//']' )
              RETURN
         END IF
!
         READ ( UNIT=BUF(IB+18:IB+28), FMT='(E11.4)', IOSTAT=IER ) AMP
         IF ( IER .NE. 0 ) THEN
              CALL CLRCH ( STR )
              CALL INCH  ( IB+18, STR )
              CALL ERR_LOG ( 8522, IUER, 'PIMA_RESID_REA_OBS', 'Failure '// &
     &            'to decode the amplitude '//BUF(IB+18:IB+28)//' at '// &
     &            'position '//STR )
              RETURN
         END IF
!
         READ ( UNIT=BUF(IB+37:IB+44), FMT='(E8.5)',  IOSTAT=IER ) PHS
         IF ( IER .NE. 0 ) THEN
              CALL CLRCH ( STR )
              CALL INCH  ( IB+37, STR )
              CALL ERR_LOG ( 8523, IUER, 'PIMA_RESID_REA_OBS', 'Failure '// &
     &            'to decode the phase '//BUF(IB+37:IB+44)//' at '// &
     &            'position '//STR )
              RETURN
         END IF
!
         READ ( UNIT=BUF(IB+53:IB+61), FMT='(F9.4)',  IOSTAT=IER ) EFD
         IF ( IER .NE. 0 ) THEN
              CALL CLRCH ( STR )
              CALL INCH  ( IB+53, STR )
              CALL ERR_LOG ( 8524, IUER, 'PIMA_RESID_REA_OBS', 'Failure '// &
     &            'to decode the effective duration '//BUF(IB+53:IB+61)// &
     &            ' at position '//STR )
              RETURN
         END IF
         PIM%OBS(IND_OBS)%RES_FRN(I_FRQ,IND_BND) = CMPLX ( AMP*COS(PHS), AMP*SIN(PHS) )
         PIM%OBS(IND_OBS)%EFF_DUR(IND_BND) = EFD
         IB = IB +61
 410  CONTINUE
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  PIMA_RESID_REA_OBS  !#!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE PIMA_RESID_REA ( PIM, IND_BND, FRIRES_FILE, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  PIMA_RESID_REA
! *                                                                      *
! * ## 30-SEP-2009   PIMA_RESID_REA  v1.0 (c) L. Petrov  30-SEP-2009 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'astro_constants.i'
      INCLUDE   'pima.i'
      TYPE     ( PIMA__TYPE  ) :: PIM
      CHARACTER  FRIRES_FILE*(*)
      INTEGER*4  IUER
      CHARACTER  STR*128, BUF*4096
      INTEGER*4  J1, IND_BND, IND_OBS, LUN, IER
      INTEGER*4, EXTERNAL :: GET_UNIT, ILEN, I_LEN
!
      LUN = GET_UNIT()
!
      OPEN ( UNIT=LUN, FILE=FRIRES_FILE, STATUS='UNKNOWN', &
     &       IOSTAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 8531, IUER, 'PIMA_RESID_REA', 'Failure in '// &
     &         'an attempt to open residual file '//FRIRES_FILE )
           RETURN
      END IF
!
      DO 410 J1=1,1024*1024*1024
         READ ( UNIT=LUN, FMT='(A)', IOSTAT=IER ) BUF
         IF ( IER == -1 ) GOTO 810
         IF ( IER .NE. 0 ) THEN
              CALL CLRCH ( STR )
              CALL INCH  ( J1, STR )
              CALL ERR_LOG ( 8532, IUER, 'PIMA_RESID_REA', 'Failure in '// &
     &            'an attempt to read the '//STR(1:I_LEN(STR))//'-th line '// &
     &            'of the residual file '//FRIRES_FILE )
              RETURN
         END IF
         IF ( J1 == 1 ) THEN
              IF ( BUF(1:LEN(PIMA__RESID_LABEL)) == PIMA__RESID_LABEL ) THEN
                   CONTINUE
                 ELSE
                   CALL CLRCH ( STR )
                   CALL TRAN  ( 13, BUF, STR )
                   CALL ERR_LOG ( 8533, IUER, 'PIMA_RESID_REA', 'Unsupported '// &
     &                 'format of the residual file '// &
     &                  FRIRES_FILE(1:I_LEN(FRIRES_FILE))// &
     &                 ' : a format label '//PIMA__RESID_LABEL//' was '// &
     &                 'expected to be found in the first line of the '// &
     &                 'file, but '//STR(1:I_LEN(STR))//' was found' )
                   RETURN
              END IF
         END IF
         IF ( BUF(1:1) == '#' ) GOTO 410
!
         CALL ERR_PASS ( IUER, IER )
         CALL PIMA_RESID_REA_OBS ( PIM, BUF, IND_BND, IND_OBS, IER )
         IF ( IER .NE. 0 ) THEN
              CALL CLRCH ( STR )
              CALL INCH  ( J1, STR )
              CALL ERR_LOG ( 8534, IUER, 'PIMA_RESID_REA', 'Failure to '// &
     &            'parse the '//STR(1:I_LEN(STR))//'-th line of the '// &
     &            'residual file '//FRIRES_FILE )
              RETURN
         END IF
 410  CONTINUE
 810  CONTINUE
      CLOSE ( UNIT=LUN )
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  PIMA_RESID_REA  !#!#
