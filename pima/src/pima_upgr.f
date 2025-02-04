      SUBROUTINE PIMA_UPGR ( CONF_FILE, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine PIMA_UPGR upgrades configuration file for changes in       *
! *   its syntax, for example for adding new keywords.                   *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *  CONF_FILE ( CHARACTER ) -- name of the configuration file to be     *
! *                             updated.                                 *
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
! *    IUER ( INTEGER*4, OPT ) -- Universal error handler.               *
! *                      Input: IUER=0 -- no error message will be       *
! *                                       printed even in the case       *
! *                                       of error.                      *
! *                             IUER=-1,-2,-3 -- in the case of error    *
! *                                       the message will be put on     *
! *                                       stdout.                        *
! *                             IUER=-3 -- in the case of error after    *
! *                                        printing the error message    *
! *                                        the program will terminate.   *
! *                       Output:                                        *
! *                             if input value of IUER =-2,-3 -- IUER    *
! *                                        is not modified.              *
! *                             otherwise, the output value of IUER is 0 *
! *                             in the case of successful and            *
! *                             positive non-zero in the vase of errors. *
! *                                                                      *
! *  ### 27-JAN-2011   PIMA_UPGR   v1.31 (c)  L. Petrov  24-MAR-2022 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'pima.i'
      CHARACTER  CONF_FILE*(*)
      INTEGER*4  IUER
      INTEGER*4  MBUF, MIND
      PARAMETER  ( MBUF = 8192 )
      PARAMETER  ( MIND =   32 )
      CHARACTER  BUF(MBUF)*256, OUT(MBUF)*256, FILOUT*128, STR*128
      INTEGER*4  J1, J2, J3, J4, J5, J6, J7, J8, J9, J10, J11, J12, &
     &           J13, J14, J15, J16, J17, J18, J19, J20, J21, J22, J23, &
     &           LIND, IND(2,MIND), NB, NOUT, IL, IP, IB, IER
      CHARACTER  STR_FRQ_GRP*128, STR_BEG_FRQ*128, &
     &           STR_END_FRQ*128, STR_POLAR*128, STR_MAX_GAP*128, &
     &           SYSNAME*128, HOSTNAME*128, HARDWARE*128, POLAR_VAL*8, &
     &           EXC_CODE*8
      CHARACTER  REG*3
      PARAMETER  ( REG = CHAR(0)//CHAR(32)//CHAR(9) )
      LOGICAL*1  FL_SPLT, FL_FFT, FL_VTD
      INTEGER*4, EXTERNAL :: ILEN, I_LEN
!
      CALL GETINFO_SYSTEM ( SYSNAME, HOSTNAME, HARDWARE )
!
      CALL ERR_PASS ( IUER, IER )
      CALL RD_TEXT  ( CONF_FILE, MBUF, BUF, NB, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 6511, IUER, 'PIMA_UPGR', 'Failure in '// &
     &         'reading configuration file '//CONF_FILE )
           RETURN 
      END IF
!
      FILOUT = CONF_FILE(1:I_LEN(CONF_FILE))//'.new'
      CALL GETINFO_HOST ( HOSTNAME )
!
      DO 410 J1=1,NB
         CALL EXWORD ( BUF(J1), MIND, LIND, IND, REG, -2 )
         IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'FR1P.FRQ_GRP:' ) THEN
              STR_FRQ_GRP = BUF(J1)(IND(1,2):IND(2,2)) 
         END IF
         IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'FR1P.BEG_IFRQ:' ) THEN
              STR_BEG_FRQ = BUF(J1)(IND(1,2):IND(2,2)) 
         END IF
         IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'FR1P.END_IFRQ:' ) THEN
              STR_END_FRQ = BUF(J1)(IND(1,2):IND(2,2)) 
         END IF
         IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'FR1P.POLAR:' ) THEN
              STR_POLAR = BUF(J1)(IND(1,2):IND(2,2)) 
         END IF
 410  CONTINUE 
!
      IF ( BUF(1)(1:50) == &
     &     '# PIMA_CONTROL file.  Format Version of 2006.01.06' ) THEN
           FL_SPLT = .FALSE.
           FL_FFT  = .FALSE.
           FL_VTD  = .FALSE.
           NOUT = 0
           DO 420 J2=1,NB
              IF ( J2 == 1 ) THEN
                   NOUT = NOUT + 1
                   OUT(NOUT) = PIMA__CONTROL_LABEL
                   GOTO 420
                ELSE IF ( BUF(J2)(1:9)  == 'FRIB.OBS:' ) THEN
                   BUF(J2)(1:9)  = 'OBS:     ' 
                ELSE IF ( BUF(J2)(1:10) == 'SESS_CODE:' ) THEN
                   NOUT = NOUT + 1
                   OUT(NOUT) = BUF(J2) 
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'BAND:             K'
                   NOUT = NOUT + 1
                   OUT(NOUT) = '#'
                   GOTO 420
                ELSE IF ( BUF(J2)(1:47) == 'BANDPASS_FILE:      /vlbi/kcal/kcal_15_0deg.bps' ) THEN
                   BUF(J2) = 'BANDPASS_FILE:      /vlbi/kcal/kcal.bps'
                ELSE IF ( BUF(J2)(1:48) == 'BANDPASS_MASK_FILE: /vlbi/kcal/kcal_bpass_a.mask' ) THEN
                   BUF(J2) = 'BANDPASS_MASK_FILE: /vlbi/kcal/kcal.mask'
                ELSE IF ( BUF(J2)(1:48) == 'BANDPASS_MASK_FILE: /vlbi/kcal/kcal_bpass_b.mask' ) THEN
                   BUF(J2) = 'BANDPASS_MASK_FILE: /vlbi/kcal/kcal.mask'
                ELSE IF ( BUF(J2)(1:48) == 'BANDPASS_MASK_FILE: /vlbi/kcal/kcal_bpass_c.mask' ) THEN
                   BUF(J2) = 'BANDPASS_MASK_FILE: /vlbi/kcal/kcal.mask'
                ELSE IF ( BUF(J2)(1:14) == 'FRINGE_ERRORS:'  .AND. &
     &                    .NOT. FL_FFT                             ) THEN
                   NOUT = NOUT + 1
                   OUT(NOUT) = BUF(J2)
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'FFT_METHOD:       MEASURE'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'FFT_CONFIG_FILE:  /home/lpetrov/tools/pima_big_measure_4thr.wis'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'NUM_THREADS:  4'
                   FL_FFT = .TRUE.
                   GOTO 420
                ELSE IF ( BUF(J2)(1:16) == 'VTD_CONFIG_FILE:'  .AND. &
     &                    .NOT. FL_VTD                             ) THEN
                   FL_VTD = .TRUE.
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'MIN_SCAN_LEN:       10.0'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'MAX_SCAN_LEN:       180.0'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'MAX_SCAN_USED:      180.0'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'MAX_SCAN_GAP:       40.0'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'FRT_OFFSET:         AUTO'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'STA_REF:            VERAMZSW'
                   FL_VTD = .TRUE.
                ELSE IF ( BUF(J2)(1:5) == 'FFTW_' ) THEN
                   GOTO 420
                ELSE IF ( BUF(J2)(1:28) == 'EXPER_DIR:          /s2/pima' ) THEN
                   BUF(J2) = 'EXPER_DIR:          /scr/pima'
                ELSE IF ( BUF(J2)(1:15) == 'TIME_FLAG_FILE:' ) THEN
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'PCAL_MASK_FILE:     NO'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'POLARCAL_FILE:      NO'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'VERA_APRIORI:       NO'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'CORR_FLAG_MIN:      -1'
                ELSE IF ( BUF(J2)(1:12) == 'FRINGE_FILE:' ) THEN
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'TEC_FILE:           NO'
                   NOUT = NOUT + 1
                   OUT(NOUT) = BUF(J2)
                   IL = ILEN(BUF(J2))
                   NOUT = NOUT + 1
                   OUT(NOUT) = BUF(J2)(1:IL-4)//'.frr'
                   OUT(NOUT)(4:6) = 'RES'
                ELSE IF ( BUF(J2)(1:14) == 'BPS.MODE:' ) THEN
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'BEG_FRQ:              1'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'END_FRQ:             16'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'POLAR:               RR'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'FRQ_GRP:              1'
                   NOUT = NOUT + 1
                   OUT(NOUT) = '#'
#
                   NOUT = NOUT + 1
                   OUT(NOUT) = '#'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'BPS.MODE:           FINE'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'BPS.NOBS_ACCUM:     4'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'BPS.MSEG_ACCUM:     4'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'BPS.NOBS_FINE:      8'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'BPS.MINOBS_FINE:    4'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'BPS.MSEG_FINE:      6'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'BPS.SNR_MIN_ACCUM:  40.0'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'BPS.SNR_MIN_FINE:   40.0'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'BPS.DECOR_TIM_MIN:  0.1'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'BPS.AMPL_REJECT:    0.4'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'BPS.PHAS_REJECT:    0.2'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'BPS.DEG_AMP:        5'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'BPS.DEG_PHS:        1'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'BPS.NORML:          IF'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'BPS.SEFD_USE:       NO'
                   NOUT = NOUT + 1
                   OUT(NOUT) = '#'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'FRIB.SEARCH_TYPE:          2FFT'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'FRIB.DELAY_WINDOW_CENTER:  0.0'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'FRIB.RATE_WINDOW_CENTER:   0.0'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'FRIB.DELAY_WINDOW_WIDTH:   4.0D-6'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'FRIB.RATE_WINDOW_WIDTH:    1.5D-10'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'FRIB.AUTOCORR_CALIB:       SQRT_MEA'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'FRIB.AMPL_FUDGE_TYPE:      NO'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'FRIB.AMPL_EDGE_WINDOW_COR: USE'
                   NOUT = NOUT + 1
                   OUT(NOUT) = '#'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'FRIB.OVERSAMPLE_MD:           4'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'FRIB.OVERSAMPLE_RT:           4'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'FRIB.FINE_SEARCH:             LSQ'
                   NOUT = NOUT + 1
                   OUT(NOUT) = '#'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'FRIB.AUTOCORR_THRESHOLD:      0.2'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'FRIB.WEIGHTS_THRESHOLD:       0.7'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'FRIB.NOISE_NSIGMA:            3.5'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'FRIB.SNR_DETECTION:           5.6'
                   NOUT = NOUT + 1
                   OUT(NOUT) = '#'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'FRIB.2D_FRINGE_PLOT:          GIF'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'FRIB.1D_RESFRQ_PLOT:          GIF'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'FRIB.1D_RESTIM_PLOT:          GIF'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'FRIB.1D_DRF_PLOT:             GIF'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'FRIB.PLOT_DELAY_WINDOW_WIDTH: 2.D-8'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'FRIB.PLOT_RATE_WINDOW_WIDTH:  3.D-12'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'FRIB.OVERSAMPLE_PLOT_MD:      1'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'FRIB.OVERSAMPLE_PLOT_RT:      1'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'FRIB.1D_FRQ_MSEG:             16'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'FRIB.1D_TIM_MSEG:             14'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'FRIB.1D_DRF_SPAN:             1.5'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'OBS:                          ALL'
                   NOUT = NOUT + 1
                   OUT(NOUT) = '#'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'FRIP.SCAN_FILE:               NO'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'FRIP.STA_INC_FILE:            NO'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'FRIP.STA_EXC_FILE:            NO'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'FRIP.STA_REFS:                SAME'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'FRIP.RESOLUTION:              4096'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'FRIP.OVERSAMPLE:              1'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'FRIP.SCA:                     ALL'
                   NOUT = NOUT + 1
                   OUT(NOUT) = '#'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'SPLT.SOU_NAME:                ALL'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'SPLT.ANAL_FILE:                NO'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'SPLT.FRQ_MSEG:                128'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'SPLT.TIM_MSEG:                64'
                   NOUT = NOUT + 1
                   OUT(NOUT) = '#'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'MKDB.OUTPUT_TYPE:      GVF'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'MKDB.SRT:              MID_SCAN'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'MKDB.FILTER:           NO'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'MKDB.FRINGE_ALGORITHM: LSQ'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'MKDB.GD_MAX_ADD_ERROR: 5.D-12'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'MKDB.GD_MAX_SCL_ERROR: 0.1'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'MKDB.2ND_BAND:         NO'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'MKDB.VCAT_CONFIG:      NO'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'MKDB.OUTPUT_NAME:      v'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'MKDB.DESC_FILE:        NO'
                   NOUT = NOUT + 1
                   OUT(NOUT) = '#'
                   NOUT = NOUT + 1
                   OUT(NOUT) = '#'
                   GOTO 820
                ELSE IF ( BUF(J2)(1:5) == 'MKDB.'  .AND.  &
     &                    .NOT. FL_SPLT                   ) THEN
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'SPLT.SOU_NAME:                ALL'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'SPLT.ANAL_FILE:                NO'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'SPLT.FRQ_MSEG:                128'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'SPLT.TIM_MSEG:                1'
                   NOUT = NOUT + 1
                   OUT(NOUT) = '#'
                   FL_SPLT = .TRUE.
                ELSE IF ( BUF(J2)(1:20) == 'SPLT.SOU_NAME:' ) THEN
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'ONOF.GEN_FLAGS_MODE:          NO'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'ONOF.KERNEL_START_SHARE:      0.30'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'ONOF.KERNEL_END_SHARE:        0.90'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'ONOF.COHERENT_INTERVAL:       600.0'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'ONOF.AMPL_THRESHOLD:          0.0'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'ONOF.NSIG_THRESHOLD:          3.0'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'ONOF.MIN_LOW_AP:              3'
                   NOUT = NOUT + 1
                   OUT(NOUT) = '#'
#
                   NOUT = NOUT + 1
                   OUT(NOUT) = BUF(J2)
                   GOTO 420
                ELSE IF ( BUF(J2)(1:5) == 'FR1B.' ) THEN
                   NOUT = NOUT + 1
                   OUT(NOUT) = '#'
                   NOUT = NOUT + 1
                   GOTO 820
              END IF
              NOUT = NOUT + 1
              OUT(NOUT) = BUF(J2)
 420       CONTINUE 
 820       CONTINUE 
           OUT(NOUT) = OUT(1)
        ELSE IF ( BUF(1)(1:LEN(PIMA__CONTROL_LABEL)) == &
     &     '# PIMA_CONTROL file.  Format Version of 2010.08.15' ) THEN
           FL_SPLT = .FALSE.
           NOUT = 0
           DO 430 J3=1,NB
              IF ( BUF(J3)(1:5) == 'FR1P.' ) THEN
                   BUF(J3)(1:5) = 'FRIB.'
              END IF
              IF ( J3 == 1 ) THEN
                   NOUT = NOUT + 1
                   OUT(NOUT) = PIMA__CONTROL_LABEL
                   GOTO 430
                ELSE IF ( BUF(J3)(1:23) == 'FRIB.SECONDARY_SNR_MIN:' ) THEN
                   GOTO 430
                ELSE IF ( BUF(J3)(1:25) == 'FRIB.SECONDARY_MAX_TRIES:' ) THEN
                   GOTO 430
                ELSE IF ( BUF(J3)(1:9)  == 'FRIB.OBS:' ) THEN
                   BUF(J3)(1:9)  = 'OBS:     ' 
                ELSE IF ( BUF(J3)(1:48) == 'FFT_CONFIG_FILE:    /progs/pima/fftw_measure.wis' ) THEN
                   BUF(J3) = 'FFT_CONFIG_FILE:    /home/lpetrov/tools/pima_big_measure_4thr.wis'
                ELSE IF ( BUF(J3)(1:13) == 'FRIB.FRQ_GRP:'  ) THEN
                   GOTO 430
                ELSE IF ( BUF(J3)(1:14) == 'FRIB.BEG_IFRQ:' ) THEN
                   GOTO 430
                ELSE IF ( BUF(J3)(1:14) == 'FRIB.END_IFRQ:' ) THEN
                   GOTO 430
                ELSE IF ( BUF(J3)(1:13) == 'FRIB.POLAR:'    ) THEN
                   GOTO 430
                ELSE IF ( BUF(J3)(1:14) == 'BPS.MODE:' ) THEN
                   OUT(NOUT) = 'BEG_FRQ:            '//STR_BEG_FRQ(1:I_LEN(STR_BEG_FRQ))
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'END_FRQ:            '//STR_END_FRQ(1:I_LEN(STR_END_FRQ))
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'POLAR:              '//STR_POLAR(1:I_LEN(STR_POLAR))
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'FRQ_GRP:            '//STR_FRQ_GRP(1:I_LEN(STR_FRQ_GRP))
                   NOUT = NOUT + 1
                   OUT(NOUT) = '#'
                ELSE IF ( BUF(J3)(1:17) == 'BPS.SNR_MIN_FINE:' ) THEN
                   NOUT = NOUT + 1
                   OUT(NOUT) = BUF(J3)
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'BPS.DECOR_TIM_MIN:  0.1'
                   GOTO 430
                ELSE IF ( BUF(J3)(1:13) == 'BPS.SEFD_USE:' ) THEN
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'BPS.NORML:          IF'
                ELSE IF ( BUF(J3)(1:5) == 'MKDB.'  .AND.  &
     &                    .NOT. FL_SPLT                   ) THEN
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'FRIP.SCAN_FILE:               NO'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'FRIP.STA_INC_FILE:            NO'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'FRIP.STA_EXC_FILE:            NO'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'FRIP.STA_REFS:                SAME'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'FRIP.RESOLUTION:              4096'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'FRIP.OVERSAMPLE:              1'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'FRIP.SCA:                     ALL'
                   NOUT = NOUT + 1
                   OUT(NOUT) = '#'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'SPLT.SOU_NAME:                ALL'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'SPLT.ANAL_FILE:                NO'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'SPLT.FRQ_MSEG:                128'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'SPLT.TIM_MSEG:                  4'
                   NOUT = NOUT + 1
                   OUT(NOUT) = '#'
                   FL_SPLT = .TRUE.
                ELSE IF ( BUF(J3)(1:20) == 'SPLT.SOU_NAME:' ) THEN
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'ONOF.GEN_FLAGS_MODE:          NO'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'ONOF.KERNEL_START_SHARE:      0.30'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'ONOF.KERNEL_END_SHARE:        0.90'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'ONOF.COHERENT_INTERVAL:       600.0'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'ONOF.AMPL_THRESHOLD:          0.0'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'ONOF.NSIG_THRESHOLD:          3.0'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'ONOF.MIN_LOW_AP:              3'
                   NOUT = NOUT + 1
                   OUT(NOUT) = '#'
#
                   NOUT = NOUT + 1
                   OUT(NOUT) = BUF(J3)
                   GOTO 430
              END IF
              NOUT = NOUT + 1
              OUT(NOUT) = BUF(J3)
 430       CONTINUE 
           OUT(NOUT) = OUT(1)
        ELSE IF ( BUF(1)(1:LEN(PIMA__CONTROL_LABEL)) == &
     &     '# PIMA_CONTROL file.  Format Version of 2011.01.27' ) THEN
           FL_SPLT = .FALSE.
           NOUT = 0
           DO 440 J4=1,NB
              IF ( BUF(J4)(1:5) == 'FR1P.' ) THEN
                   BUF(J4)(1:5) = 'FRIB.'
              END IF
              IF ( J4 == 1 ) THEN
                   NOUT = NOUT + 1
                   OUT(NOUT) = PIMA__CONTROL_LABEL
                   GOTO 440
                ELSE IF ( BUF(J4)(1:23) == 'FRIB.SECONDARY_SNR_MIN:' ) THEN
                   GOTO 440
                ELSE IF ( BUF(J4)(1:25) == 'FRIB.SECONDARY_MAX_TRIES:' ) THEN
                   GOTO 440
                ELSE IF ( BUF(J4)(1:9)  == 'FRIB.OBS:' ) THEN
                   BUF(J4)(1:9)  = 'OBS:     ' 
                ELSE IF ( BUF(J4)(1:48) == 'FFT_CONFIG_FILE:    /progs/pima/fftw_measure.wis' ) THEN
                   BUF(J4) = 'FFT_CONFIG_FILE:    /home/lpetrov/tools/pima_big_measure_4thr.wis'
                ELSE IF ( BUF(J4)(1:13) == 'FRIB.FRQ_GRP:'  ) THEN
                   GOTO 440
                ELSE IF ( BUF(J4)(1:14) == 'FRIB.BEG_IFRQ:' ) THEN
                   GOTO 440
                ELSE IF ( BUF(J4)(1:14) == 'FRIB.END_IFRQ:' ) THEN
                   GOTO 440
                ELSE IF ( BUF(J4)(1:13) == 'FRIB.POLAR:'    ) THEN
                   GOTO 440
                ELSE IF ( BUF(J4)(1:14) == 'BPS.MODE:' ) THEN
                   OUT(NOUT) = 'BEG_FRQ:            '//STR_BEG_FRQ(1:I_LEN(STR_BEG_FRQ))
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'END_FRQ:            '//STR_END_FRQ(1:I_LEN(STR_END_FRQ))
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'POLAR:              '//STR_POLAR(1:I_LEN(STR_POLAR))
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'FRQ_GRP:            '//STR_FRQ_GRP(1:I_LEN(STR_FRQ_GRP))
                   NOUT = NOUT + 1
                   OUT(NOUT) = '#'
                ELSE IF ( BUF(J4)(1:17) == 'BPS.SNR_MIN_FINE:' ) THEN
                   NOUT = NOUT + 1
                   OUT(NOUT) = BUF(J4)
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'BPS.DECOR_TIM_MIN:  0.1'
                   GOTO 440
                ELSE IF ( BUF(J4)(1:14) == 'SPLT.FRQ_MSEG:' ) THEN
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'FRIP.SCAN_FILE:               NO'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'FRIP.STA_INC_FILE:            NO'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'FRIP.STA_EXC_FILE:            NO'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'FRIP.STA_REFS:                SAME'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'FRIP.RESOLUTION:              4096'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'FRIP.OVERSAMPLE:              1'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'FRIP.SCA:                     ALL'
                   NOUT = NOUT + 1
                   OUT(NOUT) = '#'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'SPLT.SOU_NAME:                ALL'
                ELSE IF ( BUF(J4)(1:20) == 'SPLT.SOU_NAME:' ) THEN
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'ONOF.GEN_FLAGS_MODE:          NO'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'ONOF.KERNEL_START_SHARE:      0.30'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'ONOF.KERNEL_END_SHARE:        0.90'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'ONOF.COHERENT_INTERVAL:       600.0'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'ONOF.AMPL_THRESHOLD:          0.0'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'ONOF.NSIG_THRESHOLD:          3.0'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'ONOF.MIN_LOW_AP:              3'
                   NOUT = NOUT + 1
                   OUT(NOUT) = '#'
#
                   NOUT = NOUT + 1
                   OUT(NOUT) = BUF(J4)
                   GOTO 440
                ELSE IF ( BUF(J4)(1:13) == 'BPS.SEFD_USE:' ) THEN
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'BPS.NORML:          IF'
              END IF
              NOUT = NOUT + 1
              OUT(NOUT) = BUF(J4)
 440       CONTINUE 
           OUT(NOUT) = OUT(1)
        ELSE IF ( BUF(1)(1:LEN(PIMA__CONTROL_LABEL)) == &
     &     '# PIMA_CONTROL file.  Format Version of 2011.04.12' ) THEN
           NOUT = 0
           DO 450 J5=1,NB
              IF ( BUF(J5)(1:5) == 'FR1P.' ) THEN
                   BUF(J5)(1:5) = 'FRIB.'
              END IF
              IF ( J5 == 1 ) THEN
                   NOUT = NOUT + 1
                   OUT(NOUT) = PIMA__CONTROL_LABEL
                   GOTO 450
                ELSE IF ( BUF(J5)(1:23) == 'FRIB.SECONDARY_SNR_MIN:' ) THEN
                   GOTO 450
                ELSE IF ( BUF(J5)(1:25) == 'FRIB.SECONDARY_MAX_TRIES:' ) THEN
                   GOTO 450
                ELSE IF ( BUF(J5)(1:9)  == 'FRIB.OBS:' ) THEN
                   BUF(J5)(1:9)  = 'OBS:     ' 
                ELSE IF ( BUF(J5)(1:13) == 'FRIB.FRQ_GRP:'  ) THEN
                   GOTO 450
                ELSE IF ( BUF(J5)(1:14) == 'FRIB.BEG_IFRQ:' ) THEN
                   GOTO 450
                ELSE IF ( BUF(J5)(1:14) == 'FRIB.END_IFRQ:' ) THEN
                   GOTO 450
                ELSE IF ( BUF(J5)(1:13) == 'FRIB.POLAR:'    ) THEN
                   GOTO 450
                ELSE IF ( BUF(J5)(1:14) == 'BPS.MODE:' ) THEN
                   OUT(NOUT) = 'BEG_FRQ:            '//STR_BEG_FRQ(1:I_LEN(STR_BEG_FRQ))
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'END_FRQ:            '//STR_END_FRQ(1:I_LEN(STR_END_FRQ))
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'POLAR:              '//STR_POLAR(1:I_LEN(STR_POLAR))
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'FRQ_GRP:            '//STR_FRQ_GRP(1:I_LEN(STR_FRQ_GRP))
                   NOUT = NOUT + 1
                   OUT(NOUT) = '#'
                ELSE IF ( BUF(J5)(1:17) == 'BPS.SNR_MIN_FINE:' ) THEN
                   NOUT = NOUT + 1
                   OUT(NOUT) = BUF(J5)
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'BPS.DECOR_TIM_MIN:  0.1'
                   GOTO 450
                ELSE IF ( BUF(J5)(1:14) == 'SPLT.SOU_NAME:' ) THEN
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'FRIP.SCAN_FILE:               NO'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'FRIP.STA_INC_FILE:            NO'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'FRIP.STA_EXC_FILE:            NO'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'FRIP.STA_REFS:                SAME'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'FRIP.RESOLUTION:              4096'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'FRIP.OVERSAMPLE:              1'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'FRIP.SCA:                     ALL'
                   NOUT = NOUT + 1
                   OUT(NOUT) = '#'
                ELSE IF ( BUF(J5)(1:20) == 'SPLT.SOU_NAME:' ) THEN
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'ONOF.GEN_FLAGS_MODE:          NO'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'ONOF.KERNEL_START_SHARE:      0.30'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'ONOF.KERNEL_END_SHARE:        0.90'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'ONOF.COHERENT_INTERVAL:       600.0'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'ONOF.AMPL_THRESHOLD:          0.0'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'ONOF.NSIG_THRESHOLD:          3.0'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'ONOF.MIN_LOW_AP:              3'
                   NOUT = NOUT + 1
                   OUT(NOUT) = '#'
#
                   NOUT = NOUT + 1
                   OUT(NOUT) = BUF(J5)
                   GOTO 450
                ELSE IF ( BUF(J5)(1:15) == 'SPLT.ANAL_FILE:' ) THEN
                   GOTO 450
              END IF
              NOUT = NOUT + 1
              OUT(NOUT) = BUF(J5)
 450       CONTINUE 
           OUT(NOUT) = OUT(1)
        ELSE IF ( BUF(1)(1:LEN(PIMA__CONTROL_LABEL)) == &
     &     '# PIMA_CONTROL file.  Format Version of 2011.04.30' ) THEN
           NOUT = 0
           DO 460 J6=1,NB
              IF ( BUF(J6)(1:5) == 'FR1P.' ) THEN
                   BUF(J6)(1:5) =  'FRIB.'
              END IF
              IF ( J6 == 1 ) THEN
                   NOUT = NOUT + 1
                   OUT(NOUT) = '# PIMA_CONTROL file.  Format Version of 2011.05.23' 
                   GOTO 460
                ELSE IF ( BUF(J6)(1:23) == 'FRIB.SECONDARY_SNR_MIN:' ) THEN
                   GOTO 460
                ELSE IF ( BUF(J6)(1:25) == 'FRIB.SECONDARY_MAX_TRIES:' ) THEN
                   GOTO 460
                ELSE IF ( BUF(J6)(1:9)  == 'FRIB.OBS:' ) THEN
                   BUF(J6)(1:9)  = 'OBS:     ' 
                ELSE IF ( BUF(J6)(1:13) == 'FRIB.FRQ_GRP:'  ) THEN
                   GOTO 460
                ELSE IF ( BUF(J6)(1:14) == 'FRIB.BEG_IFRQ:' ) THEN
                   GOTO 460
                ELSE IF ( BUF(J6)(1:14) == 'FRIB.END_IFRQ:' ) THEN
                   GOTO 460
                ELSE IF ( BUF(J6)(1:13) == 'FRIB.POLAR:'    ) THEN
                   GOTO 460
                ELSE IF ( BUF(J6)(1:14) == 'BPS.MODE:' ) THEN
                   OUT(NOUT) = 'BEG_FRQ:            '//STR_BEG_FRQ(1:I_LEN(STR_BEG_FRQ))
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'END_FRQ:            '//STR_END_FRQ(1:I_LEN(STR_END_FRQ))
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'POLAR:              '//STR_POLAR(1:I_LEN(STR_POLAR))
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'FRQ_GRP:            '//STR_FRQ_GRP(1:I_LEN(STR_FRQ_GRP))
                   NOUT = NOUT + 1
                   OUT(NOUT) = '#'
                ELSE IF ( BUF(J6)(1:17) == 'BPS.SNR_MIN_FINE:' ) THEN
                   NOUT = NOUT + 1
                   OUT(NOUT) = BUF(J6)
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'BPS.DECOR_TIM_MIN:  0.1'
                   GOTO 460
                ELSE IF ( BUF(J6)(1:14) == 'SPLT.SOU_NAME:' ) THEN
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'FRIP.SCAN_FILE:               NO'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'FRIP.STA_INC_FILE:            NO'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'FRIP.STA_EXC_FILE:            NO'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'FRIP.STA_REFS:                SAME'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'FRIP.RESOLUTION:              4096'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'FRIP.OVERSAMPLE:              1'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'FRIP.SCA:                     ALL'
                   NOUT = NOUT + 1
                   OUT(NOUT) = '#'
                ELSE IF ( BUF(J6)(1:20) == 'SPLT.SOU_NAME:' ) THEN
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'ONOF.GEN_FLAGS_MODE:          NO'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'ONOF.KERNEL_START_SHARE:      0.30'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'ONOF.KERNEL_END_SHARE:        0.90'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'ONOF.COHERENT_INTERVAL:       600.0'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'ONOF.AMPL_THRESHOLD:          0.0'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'ONOF.NSIG_THRESHOLD:          3.0'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'ONOF.MIN_LOW_AP:              3'
                   NOUT = NOUT + 1
                   OUT(NOUT) = '#'
#
                   NOUT = NOUT + 1
                   OUT(NOUT) = BUF(J6)
                   GOTO 460
                ELSE IF ( BUF(J6)(1:15) == 'SPLT.ANAL_FILE:' ) THEN
                   GOTO 460
              END IF
              NOUT = NOUT + 1
              OUT(NOUT) = BUF(J6)
 460       CONTINUE 
           OUT(NOUT) = OUT(1)
        ELSE IF ( BUF(1)(1:LEN(PIMA__CONTROL_LABEL)) == &
     &     '# PIMA_CONTROL file.  Format Version of 2011.05.23' ) THEN
           NOUT = 0
           DO 470 J7=1,NB
              IF ( J7 == 1 ) THEN
                   NOUT = NOUT + 1
                   OUT(NOUT) = '# PIMA_CONTROL file.  Format Version of 2012.07.04'
                   GOTO 470
                ELSE IF ( BUF(J7)(1:1)  == '#' ) THEN
                   CONTINUE 
                ELSE IF ( BUF(J7)(1:23) == 'FRIB.SECONDARY_SNR_MIN:' ) THEN
                   GOTO 470
                ELSE IF ( BUF(J7)(1:25) == 'FRIB.SECONDARY_MAX_TRIES:' ) THEN
                   GOTO 470
                ELSE IF ( BUF(J7)(1:9)  == 'FRIB.OBS:' ) THEN
                   BUF(J7)(1:9)  = 'OBS:     ' 
                ELSE IF ( BUF(J7)(1:14) == 'TSYS_EXTERNAL:'  ) THEN
                   GOTO 470
                ELSE IF ( BUF(J7)(1:12) == 'LOAD_ERRORS:'  ) THEN
                   GOTO 470
                ELSE IF ( BUF(J7)(1:16) == 'FFT_NUM_THREADS:'  ) THEN
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'NUM_THREADS:    '//BUF(J7)(17:)
                   GOTO 470
                ELSE IF ( BUF(J7)(1:14) == 'VERA_APRIORI:'  ) THEN
                   IF ( BUF(J7)(1:13) .NE. 'INTMOD_FILE:' ) THEN
                        NOUT = NOUT + 1
                        OUT(NOUT) = 'INTMOD_TYPE:        NO'
                   END IF
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'INTMOD_FILE: '//BUF(J7)(14:)
                   GOTO 470
                ELSE IF ( BUF(J7)(1:65) == 'FFT_CONFIG_FILE:    /home/lpetrov/tools/pima_big_measure_4thr.wis' ) THEN
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'FFT_CONFIG_FILE:    /opt64/share/pima/pima_big_measure_4thr.wis'
                   GOTO 470
                ELSE IF ( BUF(J7)(1:23) == 'FFT_METHOD:         MKL' ) THEN
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'FFT_METHOD:         MEASURE'
                   GOTO 470
                ELSE IF ( BUF(J7)(1:9)   .EQ. 'FRQ_GRP: '          .AND. &
     &                    BUF(J7+1)(1:17) .NE. 'EPHEMERIDES_FILE:'       ) THEN
                   NOUT = NOUT + 1
                   OUT(NOUT) = BUF(J7)
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'EPHEMERIDES_FILE:   NO'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'EPHEMERIDES_USE:    NO'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'PHASE_ACCELERATION: 0.0'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'PHASE_ACCEL_MIN:    0.0'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'PHASE_ACCEL_MAX:    0.0'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'WVR_FILE:               NO'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'WVR_USE:                NO'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'WVR_SMOOTHING_INTERVAL: 0.0'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'WVR_SMOOTHING_SIGMA:    0.0'
                   GOTO 470
                ELSE IF ( BUF(J7)(1:13) == 'MIN_SCAN_LEN:'  ) THEN
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'AP_TOLERANCE:       2.D-6'
                ELSE IF ( BUF(J7)(1:14) == 'MAX_SCAN_USED:'  ) THEN
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'SCAN_LEN_USED:'//BUF(J7)(15:) 
                   GOTO 470
                ELSE IF ( BUF(J7)(1:14) == 'MAX_SCAN_GAP:'  ) THEN
                   IF ( OUT(NOUT)(1:14) == 'SCAN_LEN_USED:' ) THEN
!
! --------------------- Swap two lines
!
                        STR_MAX_GAP = OUT(NOUT)
                        OUT(NOUT) = BUF(J7) 
                        NOUT = NOUT + 1
                        OUT(NOUT) = STR_MAX_GAP
                      ELSE 
                        NOUT = NOUT + 1
                        OUT(NOUT) = BUF(J7) 
                   END IF 
!
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'SCAN_LEN_SKIP:        0.0'
                   GOTO 470
                ELSE IF ( BUF(J7)(1:14) == 'FRIP.SCA:' ) THEN
                   NOUT = NOUT + 1
                   OUT(NOUT) = BUF(J7)
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'FRIP.MAP_DIR:                 SAME'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'FRIP.ATM_ZEN_FILE:            NO'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'FRIP.CAL_PLOT:                GIF'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'FRIP.CAL_RES:                 400'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'FRIP.TAG_PLOT:                GIF'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'FRIP.TAG_RES:                 400'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'FRIP.BEAM_PLOT:               GIF'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'FRIP.FRQ_MSEG:                128'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'FRIP.TIM_MSEG:                1'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'FRIP.RA_CENTER:               APRIORI'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'FRIP.DEC_CENTER:              APRIORI'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'FRIP.RA_STEP:                 0.1'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'FRIP.DEC_STEP:                0.1'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'FRIP.RA_RANGE:                1.0'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'FRIP.DEC_RANGE:               1.0'
                   GOTO 470
                ELSE IF ( BUF(J7)(1:17) == 'BPS.SNR_MIN_FINE:' ) THEN
                   NOUT = NOUT + 1
                   OUT(NOUT) = BUF(J7)
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'BPS.DECOR_TIM_MIN:  0.1'
                   GOTO 470
                ELSE IF ( BUF(J7)(1:20) == 'SPLT.SOU_NAME:' ) THEN
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'ONOF.GEN_FLAGS_MODE:          NO'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'ONOF.KERNEL_START_SHARE:      0.30'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'ONOF.KERNEL_END_SHARE:        0.90'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'ONOF.COHERENT_INTERVAL:       600.0'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'ONOF.AMPL_THRESHOLD:          0.0'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'ONOF.NSIG_THRESHOLD:          3.0'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'ONOF.MIN_LOW_AP:              3'
                   NOUT = NOUT + 1
                   OUT(NOUT) = '#'
#
                   NOUT = NOUT + 1
                   OUT(NOUT) = BUF(J7)
                   GOTO 470
              END IF
              NOUT = NOUT + 1
              OUT(NOUT) = BUF(J7)
 470       CONTINUE 
           OUT(NOUT) = OUT(1)
        ELSE IF ( BUF(1)(1:LEN(PIMA__CONTROL_LABEL)) == &
     &     '# PIMA_CONTROL file.  Format Version of 2012.07.04' ) THEN
           NOUT = 0
           DO 480 J8=1,NB
              IF ( J8 == 1 ) THEN
                   NOUT = NOUT + 1
                   OUT(NOUT) = '# PIMA_CONTROL file.  Format Version of 2012.12.27'
                   GOTO 480
                ELSE IF ( BUF(J8)(1:1)  == '#' ) THEN
                   CONTINUE 
                ELSE IF ( BUF(J8)(1:23) == 'FRIB.SECONDARY_SNR_MIN:' ) THEN
                   GOTO 480
                ELSE IF ( BUF(J8)(1:25) == 'FRIB.SECONDARY_MAX_TRIES:' ) THEN
                   GOTO 480
                ELSE IF ( BUF(J8)(1:9)  == 'FRIB.OBS:' ) THEN
                   BUF(J8)(1:9)  = 'OBS:     ' 
                ELSE IF ( BUF(J8)(1:11) == 'SOU_NAMES: ' ) THEN
                   NOUT = NOUT + 1
                   IF ( HOSTNAME(1:8) == 'astrogeo' .OR. HOSTNAME(1:13) == "gs61a-sagitta" ) THEN
                        OUT(NOUT) = 'STAGING_DIR:        /f0/fits_01'
                      ELSE IF ( HOSTNAME(1:7) == 'pethome' ) THEN
                        OUT(NOUT) = 'STAGING_DIR:        /h0/fits_01'
                      ELSE 
                        OUT(NOUT) = 'STAGING_DIR:        NONE'
                   END IF
                ELSE IF ( BUF(J8)(1:23) == 'FFT_METHOD:         MKL' ) THEN
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'FFT_METHOD:         MEASURE'
                   GOTO 480
                ELSE IF ( BUF(J8)(1:16) == 'FFT_NUM_THREADS:'  ) THEN
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'NUM_THREADS:    '//BUF(J8)(17:)
                   GOTO 480
                ELSE IF ( BUF(J8)(1:65) == 'FFT_CONFIG_FILE:    /home/lpetrov/tools/pima_big_measure_4thr.wis' .OR. &
                          BUF(J8)(1:63) == 'FFT_CONFIG_FILE:    /opt64/share/pima/pima_big_measure_4thr.wis'   .OR. &
                          BUF(J8)(1:63) == 'FFT_CONFIG_FILE:  /home/lpetrov/tools/pima_big_measure_4thr.wis'        ) THEN
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'FFT_CONFIG_FILE:    /opt64/share/pima/pima_big_measure_4thr.wis'
                   IF ( HOSTNAME(1:7) == 'pethome' ) THEN
                        OUT(NOUT) = 'FFT_CONFIG_FILE:    /opt64/share/pima/pima_big_measure_4thr.wis'
                     ELSE IF ( HOSTNAME(1:8) == 'astrogeo' .OR. HOSTNAME(1:13) == "gs61a-sagitta" ) THEN
                        OUT(NOUT) = 'FFT_CONFIG_FILE:    /opt64/share/pima/pima_big_measure_6thr.wis'
                   END IF 
                   GOTO 480
                ELSE IF ( BUF(J8)(1:9)   .EQ. 'FRQ_GRP: '          .AND. &
     &                    BUF(J8+1)(1:17) .NE. 'EPHEMERIDES_FILE:'       ) THEN
                   NOUT = NOUT + 1
                   OUT(NOUT) = BUF(J8)
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'EPHEMERIDES_FILE:   NO'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'EPHEMERIDES_USE:    NO'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'PHASE_ACCELERATION: 0.0'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'PHASE_ACCEL_MIN:    0.0'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'PHASE_ACCEL_MAX:    0.0'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'WVR_FILE:               NO'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'WVR_USE:                NO'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'WVR_SMOOTHING_INTERVAL: 0.0'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'WVR_SMOOTHING_SIGMA:    0.0'
                   GOTO 480
                ELSE IF ( BUF(J8)(1:17) == 'BPS.SNR_MIN_FINE:' ) THEN
                   NOUT = NOUT + 1
                   OUT(NOUT) = BUF(J8)
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'BPS.DECOR_TIM_MIN:  0.1'
                   GOTO 480
                ELSE IF ( BUF(J8)(1:20) == 'SPLT.SOU_NAME:' ) THEN
                   IF ( BUF(J8-2)(1:4) .NE. 'ONOF' ) THEN
                        NOUT = NOUT + 1
                        OUT(NOUT) = 'ONOF.GEN_FLAGS_MODE:          NO'
                        NOUT = NOUT + 1
                        OUT(NOUT) = 'ONOF.KERNEL_START_SHARE:      0.30'
                        NOUT = NOUT + 1
                        OUT(NOUT) = 'ONOF.KERNEL_END_SHARE:        0.90'
                        NOUT = NOUT + 1
                        OUT(NOUT) = 'ONOF.COHERENT_INTERVAL:       600.0'
                        NOUT = NOUT + 1
                        OUT(NOUT) = 'ONOF.AMPL_THRESHOLD:          0.0'
                        NOUT = NOUT + 1
                        OUT(NOUT) = 'ONOF.NSIG_THRESHOLD:          3.0'
                        NOUT = NOUT + 1
                        OUT(NOUT) = 'ONOF.MIN_LOW_AP:              3'
                        NOUT = NOUT + 1
                        OUT(NOUT) = '#'
                   END IF 
#
                   NOUT = NOUT + 1
                   OUT(NOUT) = BUF(J8)
                   GOTO 480
              END IF
              NOUT = NOUT + 1
              OUT(NOUT) = BUF(J8)
 480       CONTINUE 
           OUT(NOUT) = OUT(1)
        ELSE IF ( BUF(1)(1:LEN(PIMA__CONTROL_LABEL)) == &
     &     '# PIMA_CONTROL file.  Format Version of 2012.10.01' ) THEN
           NOUT = 0
           DO 490 J9=1,NB
              IF ( J9 == 1 ) THEN
                   NOUT = NOUT + 1
                   OUT(NOUT) = '# PIMA_CONTROL file.  Format Version of 2012.12.27'
                   GOTO 490
                ELSE IF ( BUF(J9)(1:1)  == '#' ) THEN
                   CONTINUE 
                ELSE IF ( BUF(J9)(1:23) == 'FRIB.SECONDARY_SNR_MIN:' ) THEN
                   GOTO 490
                ELSE IF ( BUF(J9)(1:25) == 'FRIB.SECONDARY_MAX_TRIES:' ) THEN
                   GOTO 490
                ELSE IF ( BUF(J9)(1:9)  == 'FRIB.OBS:' ) THEN
                   BUF(J9)(1:9)  = 'OBS:     ' 
                ELSE IF ( BUF(J9)(1:14) == 'POLARCAL_FILE:' ) THEN
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'PCAL_MASK_FILE:     NO'
                ELSE IF ( BUF(J9)(1:12) == 'FRINGE_FILE:' ) THEN
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'TEC_FILE:           NO'
                ELSE IF ( BUF(J9)(1:11) == 'SOU_NAMES: ' ) THEN
                   NOUT = NOUT + 1
                   IF ( HOSTNAME(1:8) == 'astrogeo' .OR. HOSTNAME(1:13) == "gs61a-sagitta" ) THEN
                        OUT(NOUT) = 'STAGING_DIR:        /f0/fits_01'
                      ELSE IF ( HOSTNAME(1:7) == 'pethome' ) THEN
                        OUT(NOUT) = 'STAGING_DIR:        /h0/fits_01'
                      ELSE 
                        OUT(NOUT) = 'STAGING_DIR:        NONE'
                   END IF
                ELSE IF ( BUF(J9)(1:13) == 'NUM_THREADS: ' ) THEN
                   BUF(J9)(21:21) = '4' 
                ELSE IF ( BUF(J9)(1:17) == 'FFT_CONFIG_FILE: ' ) THEN
                   IF ( HOSTNAME(1:7) == 'pethome' ) THEN
                        BUF(J9) = 'FFT_CONFIG_FILE:    /opt64/share/pima/pima_big_measure_4thr.wis'
                     ELSE IF ( HOSTNAME(1:8) == 'astrogeo' .OR. HOSTNAME(1:13) == "gs61a-sagitta" ) THEN
                        BUF(J9) = 'FFT_CONFIG_FILE:    /opt64/share/pima/pima_big_measure_6thr.wis'
                   END IF 
                ELSE IF ( BUF(J9)(1:17) == 'BPS.SNR_MIN_FINE:' ) THEN
                   NOUT = NOUT + 1
                   OUT(NOUT) = BUF(J9)
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'BPS.DECOR_TIM_MIN:  0.1'
                   GOTO 490
                ELSE IF ( BUF(J9)(1:20) == 'SPLT.SOU_NAME:' ) THEN
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'ONOF.GEN_FLAGS_MODE:          NO'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'ONOF.KERNEL_START_SHARE:      0.30'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'ONOF.KERNEL_END_SHARE:        0.90'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'ONOF.COHERENT_INTERVAL:       600.0'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'ONOF.AMPL_THRESHOLD:          0.0'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'ONOF.NSIG_THRESHOLD:          3.0'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'ONOF.MIN_LOW_AP:              3'
                   NOUT = NOUT + 1
                   OUT(NOUT) = '#'
#
                   NOUT = NOUT + 1
                   OUT(NOUT) = BUF(J9)
                   GOTO 490
              END IF 
              NOUT = NOUT + 1
              OUT(NOUT) = BUF(J9)
 490       CONTINUE
           OUT(NOUT) = OUT(1)
        ELSE IF ( BUF(1)(1:LEN(PIMA__CONTROL_LABEL)) == &
     &     '# PIMA_CONTROL file.  Format Version of 2012.12.27' ) THEN
           NOUT = 0
           DO 4100 J10=1,NB
              IF ( J10 == 1 ) THEN
                   NOUT = NOUT + 1
                   OUT(NOUT) = PIMA__CONTROL_LABEL
                   GOTO 4100
                ELSE IF ( BUF(J10)(1:1)  == '#' ) THEN
                   CONTINUE 
                ELSE IF ( BUF(J10)(1:23) == 'FRIB.SECONDARY_SNR_MIN:' ) THEN
                   GOTO 4100
                ELSE IF ( BUF(J10)(1:25) == 'FRIB.SECONDARY_MAX_TRIES:' ) THEN
                   GOTO 4100
                ELSE IF ( BUF(J10)(1:9)  == 'FRIB.OBS:' ) THEN
                   BUF(J10)(1:9)  = 'OBS:     ' 
                ELSE IF ( BUF(J10)(1:6) == 'POLAR: ' ) THEN
                   STR = BUF(J10)(7:) 
                   CALL CHASHL (  STR )
                   POLAR_VAL = STR
                ELSE IF ( BUF(J10)(1:14) == 'POLARCAL_FILE:' ) THEN
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'PCAL_MASK_FILE:     NO'
                ELSE IF ( BUF(J10)(1:12) == 'FRINGE_FILE:' ) THEN
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'TEC_FILE:           NO'
                ELSE IF ( BUF(J10)(1:27) == 'FRIB.AMPL_EDGE_WINDOW_COR: ' ) THEN
                   NOUT = NOUT + 1
                   OUT(NOUT) = BUF(J10)
                   BUF(J10)  = 'FRIB.AMPL_EDGE_BEAM_COR:      NO'
                ELSE IF ( BUF(J10)(1:17) == 'BPS.SNR_MIN_FINE:' ) THEN
                   NOUT = NOUT + 1
                   OUT(NOUT) = BUF(J10)
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'BPS.DECOR_TIM_MIN:  0.1'
                   GOTO 4100
                ELSE IF ( BUF(J10)(1:12) == 'BPS.DEG_AMP:' ) THEN
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'BPS.INTRP_METHOD:   LEGENDRE'
                ELSE IF ( BUF(J10)(1:10) == 'BPS.NORML:' ) THEN
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'BPS.AMP_MIN:        0.1'
                 ELSE IF ( BUF(J10)(1:15) == 'SPLT.TIM_MSEG: ' ) THEN
                   NOUT = NOUT + 1
                   OUT(NOUT) = BUF(J10)
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'SPLT.WEIGHT_TYPE:             AUTO'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'SPLT.POLAR:                   '//POLAR_VAL(1:I_LEN(POLAR_VAL))
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'SPLT.STA_BASED:               ALL'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'SPLT.AUTOCORR_NRML_METHOD:    AVERAGED'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'SPLT.BPASS_NRML_METHOD:       WEIGHTED'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'SPLT.BPASS_NRML_RANGE:        0.25:0.75'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'SPLT.SUBARRY_CONSOLIDATION:   NO'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'SPLT.TOTAL_UV:                YES'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'SPLT.SNR_MIN:                 4.0'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'SPLT.GAIN_CORR_FILE:          NO'
                ELSE IF ( BUF(J10)(1:20)   .EQ. 'PHASE_ACCELERATION: ' .AND. &
     &                    BUF(J10+1)(1:16) .NE. 'PHASE_ACCEL_MIN:'           ) THEN
                   NOUT = NOUT + 1
                   OUT(NOUT) = BUF(J10)
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'PHASE_ACCEL_MIN:    0.0'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'PHASE_ACCEL_MAX:    0.0'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'WVR_FILE:               NO'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'WVR_USE:                NO'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'WVR_SMOOTHING_INTERVAL: 0.0'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'WVR_SMOOTHING_SIGMA:    0.0'
                   GOTO 4100
                ELSE IF ( BUF(J10)(1:20) == 'SPLT.SOU_NAME:' ) THEN
                   IF ( BUF(J10-2)(1:4) .NE. 'ONOF' ) THEN
                        NOUT = NOUT + 1
                        OUT(NOUT) = 'ONOF.GEN_FLAGS_MODE:          NO'
                        NOUT = NOUT + 1
                        OUT(NOUT) = 'ONOF.KERNEL_START_SHARE:      0.30'
                        NOUT = NOUT + 1
                        OUT(NOUT) = 'ONOF.KERNEL_END_SHARE:        0.90'
                        NOUT = NOUT + 1
                        OUT(NOUT) = 'ONOF.COHERENT_INTERVAL:       600.0'
                        NOUT = NOUT + 1
                        OUT(NOUT) = 'ONOF.AMPL_THRESHOLD:          0.0'
                        NOUT = NOUT + 1
                        OUT(NOUT) = 'ONOF.NSIG_THRESHOLD:          3.0'
                        NOUT = NOUT + 1
                        OUT(NOUT) = 'ONOF.MIN_LOW_AP:              3'
                        NOUT = NOUT + 1
                        OUT(NOUT) = '#'
                   END IF
                   NOUT = NOUT + 1
                   OUT(NOUT) = BUF(J10)
                   GOTO 4100
                ELSE IF ( BUF(J10)(1:19) == 'FRIB.SNR_DETECTION:' ) THEN
                   NOUT = NOUT + 1
                   OUT(NOUT) = BUF(J10)
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'FRIB.FRQ_TRANSFER_BAND:       NO'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'FRIB.FRQ_TRANSFER_METHOD:     NO'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'FRIB.FRQ_TRANSFER_DEG:        1'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'FRIB.FRQ_TRANSFER_MSEG:       1'
                   GOTO 4100
              END IF 
              NOUT = NOUT + 1
              OUT(NOUT) = BUF(J10)
 4100      CONTINUE
           OUT(NOUT) = OUT(1)
        ELSE IF ( BUF(1)(1:LEN(PIMA__CONTROL_LABEL)) == &
     &     '# PIMA_CONTROL file.  Format Version of 2013.07.11' ) THEN
           NOUT = 0
           DO 4110 J11=1,NB
              IF ( J11 == 1 ) THEN
                   NOUT = NOUT + 1
                   OUT(NOUT) = PIMA__CONTROL_LABEL
                   GOTO 4110
                ELSE IF ( BUF(J11)(1:1)  == '#' ) THEN
                   CONTINUE 
                ELSE IF ( BUF(J11)(1:23) == 'FRIB.SECONDARY_SNR_MIN:' ) THEN
                   GOTO 4110
                ELSE IF ( BUF(J11)(1:25) == 'FRIB.SECONDARY_MAX_TRIES:' ) THEN
                   GOTO 4110
                ELSE IF ( BUF(J11)(1:9)  == 'FRIB.OBS:' ) THEN
                   BUF(J11)(1:9)  = 'OBS:     ' 
                ELSE IF ( BUF(J11)(1:14) == 'POLARCAL_FILE:' ) THEN
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'PCAL_MASK_FILE:     NO'
                ELSE IF ( BUF(J11)(1:33) == 'SPLT.STA_BASED:               YES' ) THEN
                   BUF(J11) = 'SPLT.STA_BASED:               ALL'
                ELSE IF ( BUF(J11)(1:12) == 'FRINGE_FILE:' ) THEN
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'TEC_FILE:           NO'
                ELSE IF ( BUF(J11)(1:17) == 'BPS.SNR_MIN_FINE:' ) THEN
                   NOUT = NOUT + 1
                   OUT(NOUT) = BUF(J11)
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'BPS.DECOR_TIM_MIN:  0.1'
                   GOTO 4110
                ELSE IF ( BUF(J11)(1:12) == 'BPS.DEG_AMP:' ) THEN
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'BPS.INTRP_METHOD:   LEGENDRE'
                ELSE IF ( BUF(J11)(1:10) == 'BPS.NORML:' ) THEN
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'BPS.AMP_MIN:        0.1'
                ELSE IF ( BUF(J11)(1:20)   .EQ. 'PHASE_ACCELERATION: ' .AND. &
     &                    BUF(J11+1)(1:16) .NE. 'PHASE_ACCEL_MIN:'           ) THEN
                   NOUT = NOUT + 1
                   OUT(NOUT) = BUF(J11)
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'PHASE_ACCEL_MIN:    0.0'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'PHASE_ACCEL_MAX:    0.0'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'WVR_FILE:               NO'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'WVR_USE:                NO'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'WVR_SMOOTHING_INTERVAL: 0.0'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'WVR_SMOOTHING_SIGMA:    0.0'
                   GOTO 4110
                ELSE IF ( BUF(J11)(1:12) == 'SPLT.POLAR: ' ) THEN
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'SPLT.WEIGHT_TYPE:             AUTO'
                   NOUT = NOUT + 1
                   OUT(NOUT) = BUF(J11)
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'SPLT.AUTOCORR_NRML_METHOD:    AVERAGED'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'SPLT.BPASS_NRML_METHOD:       WEIGHTED'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'SPLT.BPASS_NRML_RANGE:        0.25:0.75'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'SPLT.SUBARRY_CONSOLIDATION:   NO'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'SPLT.TOTAL_UV:                YES'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'SPLT.SNR_MIN:                 4.0'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'SPLT.GAIN_CORR_FILE:          NO'
                ELSE IF ( BUF(J11)(1:20) == 'SPLT.SOU_NAME:' ) THEN
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'ONOF.GEN_FLAGS_MODE:          NO'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'ONOF.KERNEL_START_SHARE:      0.30'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'ONOF.KERNEL_END_SHARE:        0.90'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'ONOF.COHERENT_INTERVAL:       600.0'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'ONOF.AMPL_THRESHOLD:          0.0'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'ONOF.NSIG_THRESHOLD:          3.0'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'ONOF.MIN_LOW_AP:              3'
                   NOUT = NOUT + 1
                   OUT(NOUT) = '#'
                   NOUT = NOUT + 1
                   OUT(NOUT) = BUF(J11)
                   GOTO 4110
                ELSE IF ( BUF(J11)(1:19) == 'FRIB.SNR_DETECTION:' ) THEN
                   NOUT = NOUT + 1
                   OUT(NOUT) = BUF(J11)
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'FRIB.FRQ_TRANSFER_BAND:       NO'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'FRIB.FRQ_TRANSFER_METHOD:     NO'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'FRIB.FRQ_TRANSFER_DEG:        1'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'FRIB.FRQ_TRANSFER_MSEG:       1'
                   GOTO 4110
              END IF 
              NOUT = NOUT + 1
              OUT(NOUT) = BUF(J11)
 4110      CONTINUE
           OUT(NOUT) = OUT(1)
        ELSE IF ( BUF(1)(1:LEN(PIMA__CONTROL_LABEL)) == &
     &     '# PIMA_CONTROL file.  Format Version of 2013.08.12' ) THEN
           NOUT = 0
           DO 4120 J12=1,NB
              IF ( J12 == 1 ) THEN
                   NOUT = NOUT + 1
                   OUT(NOUT) = PIMA__CONTROL_LABEL
                   GOTO 4120
                ELSE IF ( BUF(J12)(1:1)  == '#' ) THEN
                   CONTINUE 
                ELSE IF ( BUF(J12)(1:23) == 'FRIB.SECONDARY_SNR_MIN:' ) THEN
                   GOTO 4120
                ELSE IF ( BUF(J12)(1:25) == 'FRIB.SECONDARY_MAX_TRIES:' ) THEN
                   GOTO 4120
                ELSE IF ( BUF(J12)(1:9)  == 'FRIB.OBS:' ) THEN
                   BUF(J12)(1:9)  = 'OBS:     ' 
                ELSE IF ( BUF(J12)(1:10) == 'BPS.NORML:' ) THEN
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'BPS.AMP_MIN:        0.1'
                ELSE IF ( BUF(J12)(1:17) == 'BPS.SNR_MIN_FINE:' ) THEN
                   NOUT = NOUT + 1
                   OUT(NOUT) = BUF(J12)
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'BPS.DECOR_TIM_MIN:  0.1'
                   GOTO 4120
                ELSE IF ( BUF(J12)(1:14) == 'POLARCAL_FILE:' ) THEN
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'PCAL_MASK_FILE:     NO'
                ELSE IF ( BUF(J12)(1:33) == 'SPLT.STA_BASED:               YES' ) THEN
                   BUF(J12) = 'SPLT.STA_BASED:               ALL'
                ELSE IF ( BUF(J12)(1:12) == 'FRINGE_FILE:' ) THEN
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'TEC_FILE:           NO'
                ELSE IF ( BUF(J12)(1:20)   .EQ. 'PHASE_ACCELERATION: ' .AND. &
     &                    BUF(J12+1)(1:16) .NE. 'PHASE_ACCEL_MIN:'           ) THEN
                   NOUT = NOUT + 1
                   OUT(NOUT) = BUF(J12)
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'PHASE_ACCEL_MIN:    0.0'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'PHASE_ACCEL_MAX:    0.0'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'WVR_FILE:               NO'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'WVR_USE:                NO'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'WVR_SMOOTHING_INTERVAL: 0.0'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'WVR_SMOOTHING_SIGMA:    0.0'
                   GOTO 4120
                ELSE IF ( BUF(J12)(1:12) == 'SPLT.POLAR: ' ) THEN
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'SPLT.WEIGHT_TYPE:             AUTO'
                   NOUT = NOUT + 1
                   OUT(NOUT) = BUF(J12)
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'SPLT.AUTOCORR_NRML_METHOD:    AVERAGED'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'SPLT.BPASS_NRML_METHOD:       WEIGHTED'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'SPLT.BPASS_NRML_RANGE:        0.25:0.75'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'SPLT.SUBARRY_CONSOLIDATION:   NO'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'SPLT.TOTAL_UV:                YES'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'SPLT.SNR_MIN:                 4.0'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'SPLT.GAIN_CORR_FILE:          NO'
                ELSE IF ( BUF(J12)(1:20) == 'SPLT.SOU_NAME:' ) THEN
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'ONOF.GEN_FLAGS_MODE:          NO'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'ONOF.KERNEL_START_SHARE:      0.30'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'ONOF.KERNEL_END_SHARE:        0.90'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'ONOF.COHERENT_INTERVAL:       600.0'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'ONOF.AMPL_THRESHOLD:          0.0'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'ONOF.NSIG_THRESHOLD:          3.0'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'ONOF.MIN_LOW_AP:              3'
                   NOUT = NOUT + 1
                   OUT(NOUT) = '#'
                   NOUT = NOUT + 1
                   OUT(NOUT) = BUF(J12)
                   GOTO 4120
                ELSE IF ( BUF(J12)(1:19) == 'FRIB.SNR_DETECTION:' ) THEN
                   NOUT = NOUT + 1
                   OUT(NOUT) = BUF(J12)
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'FRIB.FRQ_TRANSFER_BAND:       NO'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'FRIB.FRQ_TRANSFER_METHOD:     NO'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'FRIB.FRQ_TRANSFER_DEG:        1'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'FRIB.FRQ_TRANSFER_MSEG:       1'
                   GOTO 4120
              END IF 
              NOUT = NOUT + 1
              OUT(NOUT) = BUF(J12)
 4120      CONTINUE
           OUT(NOUT) = OUT(1)
        ELSE IF ( BUF(1)(1:LEN(PIMA__CONTROL_LABEL)) == &
     &     '# PIMA_CONTROL file.  Format Version of 2013.08.15' ) THEN
           NOUT = 0
           DO 4130 J13=1,NB
              IF ( J13 == 1 ) THEN
                   NOUT = NOUT + 1
                   OUT(NOUT) = PIMA__CONTROL_LABEL
                   GOTO 4130
                ELSE IF ( BUF(J13)(1:1)  == '#' ) THEN
                   CONTINUE 
                ELSE IF ( BUF(J13)(1:23) == 'FRIB.SECONDARY_SNR_MIN:' ) THEN
                   GOTO 4130
                ELSE IF ( BUF(J13)(1:25) == 'FRIB.SECONDARY_MAX_TRIES:' ) THEN
                   GOTO 4130
                ELSE IF ( BUF(J13)(1:9)  == 'FRIB.OBS:' ) THEN
                   BUF(J13)(1:9)  = 'OBS:     ' 
                ELSE IF ( BUF(J13)(1:14) == 'POLARCAL_FILE:' ) THEN
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'PCAL_MASK_FILE:     NO'
                ELSE IF ( BUF(J13)(1:33) == 'SPLT.STA_BASED:               YES' ) THEN
                   BUF(J13) = 'SPLT.STA_BASED:               ALL'
                ELSE IF ( BUF(J13)(1:12) == 'FRINGE_FILE:' ) THEN
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'TEC_FILE:           NO'
                ELSE IF ( BUF(J13)(1:20)   .EQ. 'PHASE_ACCELERATION: ' .AND. &
     &                    BUF(J13+1)(1:16) .NE. 'PHASE_ACCEL_MIN:'           ) THEN
                   NOUT = NOUT + 1
                   OUT(NOUT) = BUF(J13)
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'PHASE_ACCEL_MIN:    0.0'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'PHASE_ACCEL_MAX:    0.0'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'WVR_FILE:               NO'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'WVR_USE:                NO'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'WVR_SMOOTHING_INTERVAL: 0.0'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'WVR_SMOOTHING_SIGMA:    0.0'
                   GOTO 4130
                ELSE IF ( BUF(J13)(1:17) == 'BPS.SNR_MIN_FINE:' ) THEN
                   NOUT = NOUT + 1
                   OUT(NOUT) = BUF(J13)
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'BPS.DECOR_TIM_MIN:  0.1'
                   GOTO 4130
                ELSE IF ( BUF(J13)(1:13) == 'SPLT.POLAR: ' ) THEN
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'SPLT.WEIGHT_TYPE:             AUTO'
                   NOUT = NOUT + 1
                   OUT(NOUT) = BUF(J13)
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'SPLT.AUTOCORR_NRML_METHOD:    AVERAGED'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'SPLT.BPASS_NRML_METHOD:       WEIGHTED'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'SPLT.BPASS_NRML_RANGE:        0.25:0.75'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'SPLT.SUBARRY_CONSOLIDATION:   NO'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'SPLT.TOTAL_UV:                YES'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'SPLT.SNR_MIN:                 4.0'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'SPLT.GAIN_CORR_FILE:          NO'
                ELSE IF ( BUF(J13)(1:20) == 'SPLT.SOU_NAME:' ) THEN
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'ONOF.GEN_FLAGS_MODE:          NO'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'ONOF.KERNEL_START_SHARE:      0.30'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'ONOF.KERNEL_END_SHARE:        0.90'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'ONOF.COHERENT_INTERVAL:       600.0'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'ONOF.AMPL_THRESHOLD:          0.0'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'ONOF.NSIG_THRESHOLD:          3.0'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'ONOF.MIN_LOW_AP:              3'
                   NOUT = NOUT + 1
                   OUT(NOUT) = '#'
                   NOUT = NOUT + 1
                   OUT(NOUT) = BUF(J13)
                   GOTO 4130
                ELSE IF ( BUF(J13)(1:19) == 'FRIB.SNR_DETECTION:' ) THEN
                   NOUT = NOUT + 1
                   OUT(NOUT) = BUF(J13)
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'FRIB.FRQ_TRANSFER_BAND:       NO'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'FRIB.FRQ_TRANSFER_METHOD:     NO'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'FRIB.FRQ_TRANSFER_DEG:        1'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'FRIB.FRQ_TRANSFER_MSEG:       1'
                   GOTO 4130
              END IF 
              NOUT = NOUT + 1
              OUT(NOUT) = BUF(J13)
 4130      CONTINUE
           OUT(NOUT) = OUT(1)
        ELSE IF ( BUF(1)(1:LEN(PIMA__CONTROL_LABEL)) == &
     &     '# PIMA_CONTROL file.  Format Version of 2013.08.18' ) THEN
           NOUT = 0
           DO 4140 J14=1,NB
              IF ( J14 == 1 ) THEN
                   NOUT = NOUT + 1
                   OUT(NOUT) = PIMA__CONTROL_LABEL
                   GOTO 4140
                ELSE IF ( BUF(J14)(1:1)  == '#' ) THEN
                   CONTINUE 
                ELSE IF ( BUF(J14)(1:23) == 'FRIB.SECONDARY_SNR_MIN:' ) THEN
                   GOTO 4140
                ELSE IF ( BUF(J14)(1:25) == 'FRIB.SECONDARY_MAX_TRIES:' ) THEN
                   GOTO 4140
                ELSE IF ( BUF(J14)(1:9)  == 'FRIB.OBS:' ) THEN
                   BUF(J14)(1:9)  = 'OBS:     ' 
                ELSE IF ( BUF(J14)(1:14) == 'POLARCAL_FILE:' ) THEN
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'PCAL_MASK_FILE:     NO'
                ELSE IF ( BUF(J14)(1:17) == 'BPS.SNR_MIN_FINE:' ) THEN
                   NOUT = NOUT + 1
                   OUT(NOUT) = BUF(J14)
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'BPS.DECOR_TIM_MIN:  0.1'
                   GOTO 4140
                ELSE IF ( BUF(J14)(1:33) == 'SPLT.STA_BASED:               YES' ) THEN
                   BUF(J14) = 'SPLT.STA_BASED:               ALL'
                ELSE IF ( BUF(J14)(1:12) == 'FRINGE_FILE:' ) THEN
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'TEC_FILE:           NO'
                ELSE IF ( BUF(J14)(1:20)   .EQ. 'PHASE_ACCELERATION: ' .AND. &
     &                    BUF(J14+1)(1:16) .NE. 'PHASE_ACCEL_MIN:'           ) THEN
                   NOUT = NOUT + 1
                   OUT(NOUT) = BUF(J14)
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'PHASE_ACCEL_MIN:    0.0'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'PHASE_ACCEL_MAX:    0.0'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'WVR_FILE:               NO'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'WVR_USE:                NO'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'WVR_SMOOTHING_INTERVAL: 0.0'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'WVR_SMOOTHING_SIGMA:    0.0'
                   GOTO 4140
                ELSE IF ( BUF(J14)(1:20) == 'SPLT.SOU_NAME:' ) THEN
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'ONOF.GEN_FLAGS_MODE:          NO'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'ONOF.KERNEL_START_SHARE:      0.30'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'ONOF.KERNEL_END_SHARE:        0.90'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'ONOF.COHERENT_INTERVAL:       600.0'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'ONOF.AMPL_THRESHOLD:          0.0'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'ONOF.NSIG_THRESHOLD:          3.0'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'ONOF.MIN_LOW_AP:              3'
                   NOUT = NOUT + 1
                   OUT(NOUT) = '#'
#
                   NOUT = NOUT + 1
                   OUT(NOUT) = BUF(J14)
                   GOTO 4140
                ELSE IF ( BUF(J14)(1:13) == 'SPLT.POLAR: ' ) THEN
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'SPLT.WEIGHT_TYPE:             AUTO'
                ELSE IF ( BUF(J14)(1:22) == 'SPLT.BPASS_NRML_RANGE:' ) THEN
                   NOUT = NOUT + 1
                   OUT(NOUT) = BUF(J14)
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'SPLT.SUBARRY_CONSOLIDATION:   NO'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'SPLT.TOTAL_UV:                YES'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'SPLT.SNR_MIN:                 4.0'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'SPLT.GAIN_CORR_FILE:          NO'
                   GOTO 4140
                ELSE IF ( BUF(J14)(1:19) == 'FRIB.SNR_DETECTION:' ) THEN
                   NOUT = NOUT + 1
                   OUT(NOUT) = BUF(J14)
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'FRIB.FRQ_TRANSFER_BAND:       NO'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'FRIB.FRQ_TRANSFER_METHOD:     NO'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'FRIB.FRQ_TRANSFER_DEG:        1'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'FRIB.FRQ_TRANSFER_MSEG:       1'
                   GOTO 4140
              END IF 
              NOUT = NOUT + 1
              OUT(NOUT) = BUF(J14)
 4140      CONTINUE
           OUT(NOUT) = OUT(1)
        ELSE IF ( BUF(1)(1:LEN(PIMA__CONTROL_LABEL)) == &
     &     '# PIMA_CONTROL file.  Format Version of 2013.09.30' ) THEN
           NOUT = 0
           DO 4150 J15=1,NB
              IF ( J15 == 1 ) THEN
                   NOUT = NOUT + 1
                   OUT(NOUT) = PIMA__CONTROL_LABEL
                   GOTO 4150
                ELSE IF ( BUF(J15)(1:1)  == '#' ) THEN
                   CONTINUE 
                ELSE IF ( BUF(J15)(1:23) == 'FRIB.SECONDARY_SNR_MIN:' ) THEN
                   GOTO 4150
                ELSE IF ( BUF(J15)(1:25) == 'FRIB.SECONDARY_MAX_TRIES:' ) THEN
                   GOTO 4150
                ELSE IF ( BUF(J15)(1:9)  == 'FRIB.OBS:' ) THEN
                   BUF(J15)(1:9)  = 'OBS:     ' 
                ELSE IF ( BUF(J15)(1:20)   .EQ. 'PHASE_ACCELERATION: ' .AND. &
     &                    BUF(J15+1)(1:16) .NE. 'PHASE_ACCEL_MIN:'           ) THEN
                   NOUT = NOUT + 1
                   OUT(NOUT) = BUF(J15)
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'PHASE_ACCEL_MIN:    0.0'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'PHASE_ACCEL_MAX:    0.0'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'WVR_FILE:               NO'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'WVR_USE:                NO'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'WVR_SMOOTHING_INTERVAL: 0.0'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'WVR_SMOOTHING_SIGMA:    0.0'
                   GOTO 4150
                ELSE IF ( BUF(J15)(1:14) == 'POLARCAL_FILE:' ) THEN
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'PCAL_MASK_FILE:     NO'
                ELSE IF ( BUF(J15)(1:17) == 'BPS.SNR_MIN_FINE:' ) THEN
                   NOUT = NOUT + 1
                   OUT(NOUT) = BUF(J15)
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'BPS.DECOR_TIM_MIN:  0.1'
                   GOTO 4150
                ELSE IF ( BUF(J15)(1:33) == 'SPLT.STA_BASED:               YES' ) THEN
                   BUF(J15) = 'SPLT.STA_BASED:               ALL'
                ELSE IF ( BUF(J15)(1:12) == 'FRINGE_FILE:' ) THEN
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'TEC_FILE:           NO'
                ELSE IF ( BUF(J15)(1:20) == 'SPLT.SOU_NAME:' ) THEN
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'ONOF.GEN_FLAGS_MODE:          NO'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'ONOF.KERNEL_START_SHARE:      0.30'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'ONOF.KERNEL_END_SHARE:        0.90'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'ONOF.COHERENT_INTERVAL:       600.0'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'ONOF.AMPL_THRESHOLD:          0.0'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'ONOF.NSIG_THRESHOLD:          3.0'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'ONOF.MIN_LOW_AP:              3'
                   NOUT = NOUT + 1
                   OUT(NOUT) = '#'
#
                   NOUT = NOUT + 1
                   OUT(NOUT) = BUF(J15)
                   GOTO 4150
                ELSE IF ( BUF(J15)(1:22) == 'SPLT.BPASS_NRML_RANGE:' ) THEN
                   NOUT = NOUT + 1
                   OUT(NOUT) = BUF(J15)
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'SPLT.SUBARRY_CONSOLIDATION:   NO'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'SPLT.TOTAL_UV:                YES'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'SPLT.SNR_MIN:                 4.0'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'SPLT.GAIN_CORR_FILE:          NO'
                   GOTO 4150
                ELSE IF ( BUF(J15)(1:19) == 'FRIB.SNR_DETECTION:' ) THEN
                   NOUT = NOUT + 1
                   OUT(NOUT) = BUF(J15)
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'FRIB.FRQ_TRANSFER_BAND:       NO'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'FRIB.FRQ_TRANSFER_METHOD:     NO'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'FRIB.FRQ_TRANSFER_DEG:        1'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'FRIB.FRQ_TRANSFER_MSEG:       1'
                   GOTO 4150
              END IF 
              NOUT = NOUT + 1
              OUT(NOUT) = BUF(J15)
 4150      CONTINUE
           OUT(NOUT) = OUT(1)
        ELSE IF ( BUF(1)(1:LEN(PIMA__CONTROL_LABEL)) == &
     &     '# PIMA_CONTROL file.  Format Version of 2014.02.08' ) THEN
           NOUT = 0
           DO 4160 J16=1,NB
              IF ( J16 == 1 ) THEN
                   NOUT = NOUT + 1
                   OUT(NOUT) = PIMA__CONTROL_LABEL
                   GOTO 4160
                ELSE IF ( BUF(J16)(1:1)  == '#' ) THEN
                   CONTINUE 
                ELSE IF ( BUF(J16)(1:23) == 'FRIB.SECONDARY_SNR_MIN:' ) THEN
                   GOTO 4160
                ELSE IF ( BUF(J16)(1:25) == 'FRIB.SECONDARY_MAX_TRIES:' ) THEN
                   GOTO 4160
                ELSE IF ( BUF(J16)(1:14) == 'POLARCAL_FILE:' ) THEN
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'PCAL_MASK_FILE:     NO'
                ELSE IF ( BUF(J16)(1:17) == 'BPS.SNR_MIN_FINE:' ) THEN
                   NOUT = NOUT + 1
                   OUT(NOUT) = BUF(J16)
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'BPS.DECOR_TIM_MIN:  0.1'
                   GOTO 4160
                ELSE IF ( BUF(J16)(1:33) == 'SPLT.STA_BASED:               YES' ) THEN
                   BUF(J16) = 'SPLT.STA_BASED:               ALL'
                ELSE IF ( BUF(J16)(1:12) == 'FRINGE_FILE:' ) THEN
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'TEC_FILE:           NO'
                ELSE IF ( BUF(J16)(1:16)  .EQ. 'PHASE_ACCEL_MAX:' .AND. &
     &                    BUF(J16+1)(1:9) .NE. 'WVR_FILE:'              ) THEN
                   NOUT = NOUT + 1
                   OUT(NOUT) = BUF(J16)
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'WVR_FILE:               NO'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'WVR_USE:                NO'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'WVR_SMOOTHING_INTERVAL: 0.0'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'WVR_SMOOTHING_SIGMA:    0.0'
                   GOTO 4160
                ELSE IF ( BUF(J16)(1:20) == 'SPLT.SOU_NAME:' ) THEN
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'ONOF.GEN_FLAGS_MODE:          NO'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'ONOF.KERNEL_START_SHARE:      0.30'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'ONOF.KERNEL_END_SHARE:        0.90'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'ONOF.COHERENT_INTERVAL:       600.0'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'ONOF.AMPL_THRESHOLD:          0.0'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'ONOF.NSIG_THRESHOLD:          3.0'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'ONOF.MIN_LOW_AP:              3'
                   NOUT = NOUT + 1
                   OUT(NOUT) = '#'
#
                   NOUT = NOUT + 1
                   OUT(NOUT) = BUF(J16)
                   GOTO 4160
                ELSE IF ( BUF(J16)(1:22) == 'SPLT.BPASS_NRML_RANGE:' ) THEN
                   NOUT = NOUT + 1
                   OUT(NOUT) = BUF(J16)
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'SPLT.SUBARRY_CONSOLIDATION:   NO'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'SPLT.TOTAL_UV:                YES'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'SPLT.SNR_MIN:                 4.0'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'SPLT.TOTAL_UV:                YES'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'SPLT.SNR_MIN:                 4.0'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'SPLT.GAIN_CORR_FILE:          NO'
                   GOTO 4160
                ELSE IF ( BUF(J16)(1:19) == 'FRIB.SNR_DETECTION:' ) THEN
                   NOUT = NOUT + 1
                   OUT(NOUT) = BUF(J16)
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'FRIB.FRQ_TRANSFER_BAND:       NO'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'FRIB.FRQ_TRANSFER_METHOD:     NO'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'FRIB.FRQ_TRANSFER_DEG:        1'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'FRIB.FRQ_TRANSFER_MSEG:       1'
                   GOTO 4160
              END IF 
              NOUT = NOUT + 1
              OUT(NOUT) = BUF(J16)
 4160      CONTINUE
           OUT(NOUT) = OUT(1)
        ELSE IF ( BUF(1)(1:LEN(PIMA__CONTROL_LABEL)) == &
     &     '# PIMA_CONTROL file.  Format Version of 2014.11.14' ) THEN
           NOUT = 0
           DO 4170 J17=1,NB
              IF ( J17 == 1 ) THEN
                   NOUT = NOUT + 1
                   OUT(NOUT) = PIMA__CONTROL_LABEL
                   GOTO 4170
                ELSE IF ( BUF(J17)(1:1)  == '#' ) THEN
                   CONTINUE 
                ELSE IF ( BUF(J17)(1:17) == 'BPS.SNR_MIN_FINE:' ) THEN
                   NOUT = NOUT + 1
                   OUT(NOUT) = BUF(J17)
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'BPS.DECOR_TIM_MIN:  0.1'
                   GOTO 4170
                ELSE IF ( BUF(J17)(1:23) == 'FRIB.SECONDARY_SNR_MIN:' ) THEN
                   GOTO 4170
                ELSE IF ( BUF(J17)(1:25) == 'FRIB.SECONDARY_MAX_TRIES:' ) THEN
                   GOTO 4170
                ELSE IF ( BUF(J17)(1:14) == 'POLARCAL_FILE:' ) THEN
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'PCAL_MASK_FILE:     NO'
                ELSE IF ( BUF(J17)(1:33) == 'SPLT.STA_BASED:               YES' ) THEN
                   BUF(J17) = 'SPLT.STA_BASED:               ALL'
                ELSE IF ( BUF(J17)(1:12) == 'FRINGE_FILE:' ) THEN
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'TEC_FILE:           NO'
                ELSE IF ( BUF(J17)(1:16)  .EQ. 'PHASE_ACCEL_MAX:' .AND. &
     &                    BUF(J17+1)(1:9) .NE. 'WVR_FILE:'              ) THEN
                   NOUT = NOUT + 1
                   OUT(NOUT) = BUF(J17)
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'WVR_FILE:               NO'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'WVR_USE:                NO'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'WVR_SMOOTHING_INTERVAL: 0.0'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'WVR_SMOOTHING_SIGMA:    0.0'
                   GOTO 4170
                ELSE IF ( BUF(J17)(1:22) == 'SPLT.BPASS_NRML_RANGE:' ) THEN
                   NOUT = NOUT + 1
                   OUT(NOUT) = BUF(J17)
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'SPLT.SUBARRY_CONSOLIDATION:   NO'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'SPLT.TOTAL_UV:                YES'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'SPLT.SNR_MIN:                 4.0'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'SPLT.GAIN_CORR_FILE:          NO'
                   GOTO 4170
                ELSE IF ( BUF(J17)(1:19) == 'FRIB.SNR_DETECTION:' ) THEN
                   NOUT = NOUT + 1
                   OUT(NOUT) = BUF(J17)
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'FRIB.FRQ_TRANSFER_BAND:       NO'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'FRIB.FRQ_TRANSFER_METHOD:     NO'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'FRIB.FRQ_TRANSFER_DEG:        1'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'FRIB.FRQ_TRANSFER_MSEG:       1'
                   GOTO 4170
              END IF 
              NOUT = NOUT + 1
              OUT(NOUT) = BUF(J17)
 4170      CONTINUE
           OUT(NOUT) = OUT(1)
        ELSE IF ( BUF(1)(1:LEN(PIMA__CONTROL_LABEL)) == &
     &     '# PIMA_CONTROL file.  Format Version of 2014.12.24' ) THEN
           NOUT = 0
           DO 4180 J18=1,NB
              IF ( J18 == 1 ) THEN
                   NOUT = NOUT + 1
                   OUT(NOUT) = PIMA__CONTROL_LABEL
                   GOTO 4180
                ELSE IF ( BUF(J18)(1:1)  == '#' ) THEN
                   CONTINUE 
                ELSE IF ( BUF(J18)(1:17) == 'BPS.SNR_MIN_FINE:' ) THEN
                   NOUT = NOUT + 1
                   OUT(NOUT) = BUF(J18)
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'BPS.DECOR_TIM_MIN:  0.1'
                   GOTO 4180
                ELSE IF ( BUF(J18)(1:23) == 'FRIB.SECONDARY_SNR_MIN:' ) THEN
                   GOTO 4180
                ELSE IF ( BUF(J18)(1:25) == 'FRIB.SECONDARY_MAX_TRIES:' ) THEN
                   GOTO 4180
                ELSE IF ( BUF(J18)(1:33) == 'SPLT.STA_BASED:               YES' ) THEN
                   BUF(J18) = 'SPLT.STA_BASED:               ALL'
                ELSE IF ( BUF(J18)(1:16)  .EQ. 'PHASE_ACCEL_MAX:' .AND. &
     &                    BUF(J18+1)(1:9) .NE. 'WVR_FILE:'              ) THEN
                   NOUT = NOUT + 1
                   OUT(NOUT) = BUF(J18)
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'WVR_FILE:               NO'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'WVR_USE:                NO'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'WVR_SMOOTHING_INTERVAL: 0.0'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'WVR_SMOOTHING_SIGMA:    0.0'
                   GOTO 4180
                ELSE IF ( BUF(J18)(1:22) == 'SPLT.BPASS_NRML_RANGE:' ) THEN
                   NOUT = NOUT + 1
                   OUT(NOUT) = BUF(J18)
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'SPLT.SUBARRY_CONSOLIDATION:   NO'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'SPLT.TOTAL_UV:                YES'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'SPLT.SNR_MIN:                 4.0'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'SPLT.GAIN_CORR_FILE:          NO'
                   GOTO 4180
                ELSE IF ( BUF(J18)(1:19) == 'FRIB.SNR_DETECTION:' ) THEN
                   NOUT = NOUT + 1
                   OUT(NOUT) = BUF(J18)
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'FRIB.FRQ_TRANSFER_BAND:       NO'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'FRIB.FRQ_TRANSFER_METHOD:     NO'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'FRIB.FRQ_TRANSFER_DEG:        1'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'FRIB.FRQ_TRANSFER_MSEG:       1'
                   GOTO 4180
              END IF 
              NOUT = NOUT + 1
              OUT(NOUT) = BUF(J18)
 4180      CONTINUE
           OUT(NOUT) = OUT(1)
        ELSE IF ( BUF(1)(1:LEN(PIMA__CONTROL_LABEL)) == &
     &     '# PIMA_CONTROL file.  Format Version of 2015.09.01' ) THEN
           NOUT = 0
           DO 4190 J19=1,NB
              IF ( J19 == 1 ) THEN
                   NOUT = NOUT + 1
                   OUT(NOUT) = PIMA__CONTROL_LABEL
                   GOTO 4190
                ELSE IF ( BUF(J19)(1:1)  == '#' ) THEN
                   CONTINUE 
                ELSE IF ( BUF(J19)(1:17) == 'BPS.SNR_MIN_FINE:' ) THEN
                   NOUT = NOUT + 1
                   OUT(NOUT) = BUF(J19)
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'BPS.DECOR_TIM_MIN:  0.1'
                   GOTO 4190
                ELSE IF ( BUF(J19)(1:23) == 'FRIB.SECONDARY_SNR_MIN:' ) THEN
                   GOTO 4190
                ELSE IF ( BUF(J19)(1:25) == 'FRIB.SECONDARY_MAX_TRIES:' ) THEN
                   GOTO 4190
                ELSE IF ( BUF(J19)(1:33) == 'SPLT.STA_BASED:               YES' ) THEN
                   BUF(J19) = 'SPLT.STA_BASED:               ALL'
                ELSE IF ( BUF(J19)(1:22) == 'SPLT.BPASS_NRML_RANGE:' ) THEN
                   NOUT = NOUT + 1
                   OUT(NOUT) = BUF(J19)
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'SPLT.SUBARRY_CONSOLIDATION:   NO'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'SPLT.TOTAL_UV:                YES'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'SPLT.SNR_MIN:                 4.0'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'SPLT.GAIN_CORR_FILE:          NO'
                   GOTO 4190
                ELSE IF ( BUF(J19)(1:19) == 'FRIB.SNR_DETECTION:' ) THEN
                   NOUT = NOUT + 1
                   OUT(NOUT) = BUF(J19)
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'FRIB.FRQ_TRANSFER_BAND:       NO'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'FRIB.FRQ_TRANSFER_METHOD:     NO'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'FRIB.FRQ_TRANSFER_DEG:        1'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'FRIB.FRQ_TRANSFER_MSEG:       1'
                   GOTO 4190
              END IF 
              NOUT = NOUT + 1
              OUT(NOUT) = BUF(J19)
 4190      CONTINUE
           OUT(NOUT) = OUT(1)
        ELSE IF ( BUF(1)(1:LEN(PIMA__CONTROL_LABEL)) == &
     &     '# PIMA_CONTROL file.  Format Version of 2016.02.29' ) THEN
           NOUT = 0
           DO 4200 J20=1,NB
              IF ( J20 == 1 ) THEN
                   NOUT = NOUT + 1
                   OUT(NOUT) = PIMA__CONTROL_LABEL
                   GOTO 4200
                ELSE IF ( BUF(J20)(1:17) == 'BPS.SNR_MIN_FINE:' ) THEN
                   NOUT = NOUT + 1
                   OUT(NOUT) = BUF(J20)
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'BPS.DECOR_TIM_MIN:  0.1'
                   GOTO 4200
                ELSE IF ( BUF(J20)(1:23) == 'FRIB.SECONDARY_SNR_MIN:' ) THEN
                   GOTO 4200
                ELSE IF ( BUF(J20)(1:25) == 'FRIB.SECONDARY_MAX_TRIES:' ) THEN
                   GOTO 4200
                ELSE IF ( BUF(J20)(1:33) == 'SPLT.STA_BASED:               YES' ) THEN
                   BUF(J20) = 'SPLT.STA_BASED:               ALL'
                ELSE IF ( BUF(J20)(1:5) == 'TSYS:' ) THEN
                   IF ( INDEX ( BUF(J20), 'INTRP' ) > 0 ) THEN
                        BUF(J20) = 'TSYS:               MEASURED'
                   END IF
                ELSE IF ( BUF(J20)(1:19) == 'FRIB.SNR_DETECTION:' ) THEN
                   NOUT = NOUT + 1
                   OUT(NOUT) = BUF(J20)
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'FRIB.FRQ_TRANSFER_BAND:       NO'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'FRIB.FRQ_TRANSFER_METHOD:     NO'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'FRIB.FRQ_TRANSFER_DEG:        1'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'FRIB.FRQ_TRANSFER_MSEG:       1'
                   GOTO 4200
                ELSE IF ( BUF(J20)(1:27) == 'SPLT.SUBARRY_CONSOLIDATION:' ) THEN
                   NOUT = NOUT + 1
                   OUT(NOUT) = BUF(J20)
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'SPLT.TOTAL_UV:                YES'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'SPLT.SNR_MIN:                 4.0'
                   GOTO 4200
              END IF 
              NOUT = NOUT + 1
              OUT(NOUT) = BUF(J20)
 4200      CONTINUE
           OUT(NOUT) = OUT(1)
        ELSE IF ( BUF(1)(1:LEN(PIMA__CONTROL_LABEL)) == &
     &     '# PIMA_CONTROL file.  Format Version of 2016.10.19' ) THEN
           NOUT = 0
           DO 4210 J21=1,NB
              IF ( J21 == 1 ) THEN
                   NOUT = NOUT + 1
                   OUT(NOUT) = PIMA__CONTROL_LABEL
                   GOTO 4210
                ELSE IF ( BUF(J21)(1:23) == 'FRIB.SECONDARY_SNR_MIN:' ) THEN
                   GOTO 4210
                ELSE IF ( BUF(J21)(1:25) == 'FRIB.SECONDARY_MAX_TRIES:' ) THEN
                   GOTO 4210
                ELSE IF ( BUF(J21)(1:17) == 'BPS.SNR_MIN_FINE:' ) THEN
                   NOUT = NOUT + 1
                   OUT(NOUT) = BUF(J21)
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'BPS.DECOR_TIM_MIN:  0.1'
                   GOTO 4210
                ELSE IF ( BUF(J21)(1:27) == 'SPLT.SUBARRY_CONSOLIDATION:' ) THEN
                   NOUT = NOUT + 1
                   OUT(NOUT) = BUF(J21)
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'SPLT.TOTAL_UV:                YES'
                   NOUT = NOUT + 1
                   OUT(NOUT) = 'SPLT.SNR_MIN:                 4.0'
                   GOTO 4210
              END IF 
              NOUT = NOUT + 1
              OUT(NOUT) = BUF(J21)
 4210      CONTINUE
           OUT(NOUT) = OUT(1)
        ELSE IF ( BUF(1)(1:LEN(PIMA__CONTROL_LABEL)) == &
     &     '# PIMA_CONTROL file.  Format Version of 2020.04.19' .AND. &
     &     ( HOSTNAME(1:8) == 'astrogeo' .OR. HOSTNAME(1:13) == "gs61a-sagitta" ) ) THEN
           NOUT = 0
           DO 4220 J22=1,NB
              NOUT = NOUT + 1
              OUT(NOUT) = BUF(J22)
 4220      CONTINUE 
        ELSE
           WRITE ( 6, * ) 'Label >>'//BUF(1)(1:I_LEN(BUF(1)))//'<<  '
           CALL ERR_LOG ( 6513, IUER, 'PIMA_UPGR', 'Label of the '// &
     &         'configuration file '//CONF_FILE(1:I_LEN(CONF_FILE))// &
     &         ' is not supported. Your hostname: '//HOSTNAME )
           RETURN 
      END IF
!
      IF ( HOSTNAME(1:8) == 'astrogeo' .OR. HOSTNAME(1:13) == "gs61a-sagitta" ) THEN
           DO 4230 J23=1,NOUT
              CALL EXWORD ( OUT(J23), MIND, LIND, IND, REG, -2 )
              IB = INDEX ( OUT(J23), '/vlba_fits/' )
              IF ( IB > 0 ) THEN
                   IF ( OUT(J23)(IB-3:IB-3) == "/" ) THEN
                        OUT(J23) = OUT(J23)(1:IB-4)//'/l1b/vlba/'//OUT(J23)(IB+LEN('/vlba_fits/'):)
                   END IF
              END IF
              IF ( OUT(J23)(1:5) == 'PCAL:' ) THEN
                   IB = INDEX ( OUT(J23), "USE_ONE") 
                   IF ( IB > 0 ) THEN
                        OUT(J23) = OUT(J23)(1:IB-1)//'1'//OUT(J23)(IB+LEN("USE_ONE"):)
                   END IF
                   IB = INDEX ( OUT(J23), "USE_TWO") 
                   IF ( IB > 0 ) THEN
                        OUT(J23) = OUT(J23)(1:IB-1)//'2'//OUT(J23)(IB+LEN("USE_TWO"):)
                   END IF
              END IF
              IF ( OUT(J23)(1:10) == 'SESS_CODE:' ) THEN
                   EXC_CODE = OUT(J23)(IND(1,2):IND(2,2)) 
              END IF
              IF ( OUT(J23)(1:55) == 'STA_NAMES:          /vlbi/apriori/socorro_station.names' ) THEN
                   OUT(J23) = 'STA_NAMES:          /apr/sta/vlbi_station.names'
              END IF
              IF ( OUT(J23)(1:61) == 'STA_NAMES:          /vlbi/solve/save_files/vlbi_station.names' ) THEN
                   OUT(J23) = 'STA_NAMES:          /apr/sta/vlbi_station.names'
              END IF
              IF ( OUT(J23)(1:55) == 'SOU_NAMES:          /vlbi/solve/save_files/source.names' ) THEN
                   OUT(J23) = 'SOU_NAMES:          /apr/sou/source.names'
              END IF
              IF ( OUT(J23)(1:12) == 'STAGING_DIR:' ) THEN
                   IF ( OUT(J23)(21:24) .NE. 'NONE' ) THEN
                        OUT(J23) = 'STAGING_DIR:        NO'
                   END IF
              END IF
              IF ( OUT(J23)(1:16) == 'FFT_CONFIG_FILE:' ) THEN
                   OUT(J23) = 'FFT_CONFIG_FILE:    /opt64/share/pima/pima_big_measure_${PIMA_NUM_THREADS}thr.wis'
              END IF
              IF ( OUT(J23)(1:12) == 'NUM_THREADS:' ) THEN
                   OUT(J23) = 'NUM_THREADS:        ${PIMA_NUM_THREADS}'
              END IF
              IF ( OUT(J23)(1:45) == 'VTD_CONFIG_FILE:    /vlbi/control/vtd_rdv.cnf' ) THEN
                   OUT(J23) = 'VTD_CONFIG_FILE:    /cont/vtd_rdv.cnf'
              END IF
              IF ( OUT(J23)(1:55) == 'MKDB.VCAT_CONFIG:      /vlbi/solve/save_files/vcat.conf' ) THEN
                   OUT(J23) = 'MKDB.VCAT_CONFIG:      /apr/psolve/vcat.conf'
              END IF
              IF ( OUT(J23)(1:8) == 'UV_FITS:' .AND. EXC_CODE(1:3) == 'bc1' ) THEN
                   IP = INDEX ( OUT(J23), '/v1/' )
                   IF ( IP > 1 ) THEN
                        OUT(J23)(IP:IP+3) = '/s0/'
                   END IF
              END IF
              IF ( OUT(J23)(1:10) == 'EXPER_DIR:' ) THEN
                   OUT(J23) = 'EXPER_DIR:          /scr/pima'
              END IF
              IF ( INDEX ( OUT(J23), '/vlbi/control/vtd_pima.cnf' ) > 0 ) THEN
                   IB = INDEX ( OUT(J23), '/vlbi/control/vtd_pima.cnf' )
                   OUT(J23) = OUT(J23)(1:IB-1)//'/cont/vtd_rdv.cnf'
              END IF
              IF ( EXC_CODE(1:3) == 'bc1' ) THEN
                   IF ( OUT(J23)(1:5) == 'PCAL:' ) THEN
                        OUT(J23)(IND(1,2):) = 'USE_ONE'
                   END IF
                   IF ( 'FRIB.AUTOCORR_CALIB:' == 'bc1' ) THEN
                        OUT(J23) = 'FRIB.AUTOCORR_CALIB:       SQRT_MEA'
                   END IF
                   IF ( OUT(J23)(1:17) == 'BPS.INTRP_METHOD:' ) THEN
                        OUT(J23) = 'BPS.INTRP_METHOD:   SPLINE'
                   END IF
                   IF ( OUT(J23)(1:12) == 'BPS.DEG_AMP:' ) THEN
                        OUT(J23) = 'BPS.DEG_AMP:        9'
                   END IF
                   IF ( OUT(J23)(1:12) == 'BPS.DEG_PHS:' ) THEN
                        OUT(J23) = 'BPS.DEG_PHS:        9'
                   END IF
                   IF ( OUT(J23)(1:17) == 'FRIB.1D_FRQ_MSEG:' ) THEN
                        OUT(J23) = 'FRIB.1D_FRQ_MSEG:             4'
                   END IF
                   IF ( OUT(J23)(1:14) == 'SPLT.TIM_MSEG:' ) THEN
                        OUT(J23) = 'SPLT.TIM_MSEG:                 32'
                   END IF
                   IF ( OUT(J23)(1:23) == 'SPLT.BPASS_NRML_RANGE:' ) THEN
                        OUT(J23) = 'SPLT.BPASS_NRML_RANGE:        0.25:0.85'
                   END IF
                   IF ( OUT(J23)(1:35) == 'FRIB.AUTOCORR_CALIB:       SQRT_KOG' ) THEN
                        OUT(J23) = 'FRIB.AUTOCORR_CALIB:       SQRT_MEA'
                   END IF
              END IF
 4230      CONTINUE 
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL WR_TEXT ( NOUT, OUT, FILOUT, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 6513, IUER, 'PIMA_UPGR', 'Failure in '// &
     &         'writing new configuration file '//FILOUT )
           RETURN 
      END IF
!
      WRITE ( 6, '(A)' ) 'OK. Upgraded configuration file is written in '
      WRITE ( 6, '(A)' ) '    '//FILOUT(1:I_LEN(FILOUT))
      WRITE ( 6, '(A)' ) '    You need re-load the data.'
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  PIMA_UPGR  !#!  
