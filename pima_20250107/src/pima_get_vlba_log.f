      SUBROUTINE PIMA_GET_VLBA_LOG ( PIM, VTD, MODE_STR, LOG_FILE, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  PIMA_GET_VLBA_LOG  parses observing logs at VLBA          *
! *   stations, extracts information from there and put it in PIM        *
! *   object. If the calibration information was already available in    *
! *   PIM, it will be overwritten.                                       *
! *                                                                      *
! *   Currently, only phase calibration and cable calibration section    *
! *   is parsed. The motivation of developing this routine was confirmed *
! *   bug in VLBA post-correlation software that lived at least till     *
! *   2009 and as a result a significant portion of phase-cal            *
! *   information was lost.                                              *
! *                                                                      *
! * ### 17-SEP-2009 PIMA_GET_VLBA_LOG v3.17 (c) L. Petrov 15-DEC-2022 ## *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'astro_constants.i'
      INCLUDE   'pima.i'
      INCLUDE   'vtd.i'
      TYPE     ( PIMA__TYPE ) :: PIM
      TYPE     ( VTD__TYPE  ) :: VTD
      CHARACTER  MODE_STR*(*), LOG_FILE*(*)
      INTEGER*4  IUER
      INTEGER*4  MBUF, M_CHN, M_TIM, MIND, M_POLY
      PARAMETER  ( MBUF   = 2048*1024 )
      PARAMETER  ( M_CHN  =        64 )
      PARAMETER  ( M_TIM  =   32*1024 )
      PARAMETER  ( MIND   =       128 )
      PARAMETER  ( M_POLY =        16 )
      INTEGER*4  PIM__M_TONES, M_VLBA, PIM__SPAN_DEF
      PARAMETER  ( PIM__M_TONES =  2 )
      PARAMETER  ( M_VLBA       = 17 )
      PARAMETER  ( PIM__SPAN_DEF = 300.0D0 ) ! Default time span of phase-cal and Tsys
      CHARACTER, ALLOCATABLE :: BUF(:)*512
      REAL*8,    ALLOCATABLE :: TIM_TSYS_ARR(:,:,:), TSYS_ARR(:,:,:,:,:), &
     &                          TAI_WEA(:,:), &
     &                          PRES(:,:), TEMP(:,:), HUMID(:,:)
      REAL*4,    ALLOCATABLE :: PCAL_AMP(:,:,:,:,:,:), PCAL_PHS(:,:,:,:,:,:), &
     &                          CABLE_CAL(:,:,:), TAI_TAG(:,:,:)
      REAL*8     TIM_ONS_ARR(2,M_TIM), TIM_CAB_ARR(M_TIM), &
     &           TIM_MET_ARR(M_TIM), PCAL_FRQ(PIM__M_TONES,PIM__MFRQ), &
     &           CABLE_MEAN(PIM__MSTA,PIM__MFRG)
      INTEGER*4, ALLOCATABLE :: NUM_TSYS(:,:),        IND_SOU_PCAL(:,:,:), &
     &                          IND_SOU_TSYS(:,:,:),  MJD_TAG(:,:,:), &
     &                          IND_FRG_PCAL(:,:) ,   MJD_WEA(:,:)
      CHARACTER  STR*128, STR1*128, STA_NAM*8, SOU_NAM*8, REG*7, &
     &           SOU_ONS(M_TIM)*8, PCAL_TAB(PIM__M_TONES*PIM__MFRQ)*3, &
     &           DATE_BEG*21, DATE_END*21, DATE_WEA*21, SB_STR*1
!
      REAL*8       TIM_EPS, TSYS__MIN, FRQ_DIF__MIN, FRQ_TOL
      PARAMETER  ( REG = CHAR(0)//CHAR(32)//CHAR(9)//',='//"'"//'/' )
      PARAMETER  ( TIM_EPS = 1.0D0 )
      PARAMETER  ( TSYS__MIN = 2.0D0 )
      PARAMETER  ( FRQ_DIF__MIN = 1.0D7 )
      PARAMETER  ( FRQ_TOL      = 5.D3  ) ! Tolerance in frequency comparison
      REAL*8       M__CAB_SHARE, TIM_UV_EPS
      PARAMETER  ( M__CAB_SHARE = 0.20D0 )
      PARAMETER  ( TIM_UV_EPS = 1.D-7 )
      REAL*8     DPFU(2), FRQ_DPFU(2), GAIN_POLY(0:M_POLY-1), SEC, UTC, TAI, &
     &           MIN_R8, TIM_TSYS, TIM_FLAG_BEG, TIM_FLAG_END, TIM_UV, &
     &           TIM_OBS_BEG, TIM_OBS_END, TSYS_VAL, POLY_VAL, &
     &           UTC_TAG, UTC_WEA, AVR_CAB, MAX_DEV, FRQ_PT(PIM__MTON), &
     &           FRQ_SKY(PIM__MTON), DELTA_FRQ, FRQ_IF, &
     &           ANG_RAD, FREQ_TONE(PIM__M_TONES,PIM__MFRQ,PIM__MFRG,PIM__MSTA), &
     &           TIM_PCAL_BEG, TIM_PCAL_END, OBS_BEG, OBS_END, VAL, &
     &           TIM_PREV, TIM_CURR, TIM_NEXT, MN_VAL, UTC_TSYS, FRQ_VAL, &
     &           TSYS_DT_MIN, TSYS_DT
      LOGICAL*4  FL_USE, FL_PC, FL_FR, FL_PU, FL_WEA, FL_TSYS, FL_GEN_VLBA, FL_PCAL_UPDATE
!
      INTEGER*4  NBUF, J1, J2, J3, J4, J5, J6, J7, J8, J9, J10, J11, J12, &
     &           J13, J14, J15, J16, J17, J18, J19, J20, J21, J22, J23, &
     &           J24, J25, J26, J27, J28, J29, J30, J31, J32, J33, J34, J35, &
     &           J36, J37, J38, J39, J40, J41, J42, J43, &
     &           IVAL, IDOY, I_SOU, K_BAD_OBS, IP, IL, &
     &           L_NOD, L_DEG, NZO_REF, &
     &           CROSS_COL(M_CHN), LIND, IND(2,MIND), IND_VLBA_STA, IND_STA, &
     &           IND_SOU, IND_SCA, IND_FRQ, L_POLY, MJD_NEW_YEAR, MJD, DOY, &
     &           IHR, UV_IND, UV_IND_BEG, UV_IND_END, IND_TSYS, IND_STA_BAS, &
     &           N_ONS, N_CAB, N_MET, K_MET, K_CAB, SIGN_CAB, N_ITER, &
     &           IV(MBUF), NP, IND_MAX_DEV, IND_OBS_FRG, IER, &
     &           NPC_STA(PIM__MSTA,PIM__MFRG), NPT, LPT, &
     &           IND2_TONE(PIM__M_TONES*PIM__MFRQ), &
     &           IND2_FREQ(PIM__M_TONES*PIM__MFRQ), &
     &           IND2_FRG(PIM__M_TONES*PIM__MFRQ), &
     &           IND2_POL(PIM__M_TONES*PIM__MFRQ), &
     &           IND_FRQ_PT(PIM__MFRQ,2,PIM__MUVS), &
     &           IND_TONE, IND_FREQ, IND_POL, IND_POL_USE, &
     &           IND_FRG, IND_FRG_SAVE, IND_PTC, IND_PTC_PREV, NO_TONES, &
     &           MJD_BEG, MJD_END, NUM_WEA(PIM__MSTA), MJD_TSYS, &
     &           IND_TSYS_SHADOW, NTSYS_MIS, PIM_MIN_FRG, PIM_MAX_FRG 
      REAL*8     CROSS_SCA_TSYS(PIM__MSCA), UTC_BEG, UTC_END, &
     &           TAI_BEG, TAI_END
      CHARACTER  DATE_STR*30, WORD4*64, WORD5*64, STA_NAM_2LET*2, PIMAVAR_SUB_BAND*1
      CHARACTER, EXTERNAL ::  MJDSEC_TO_DATE*30, GET_IVS_NAME*8
      INTEGER*4, EXTERNAL ::  ADD_CLIST, ILEN, I_LEN, LTM_DIF, IFIND_PL
!
      LPT = 0
      IF ( PIM%CONF%DEBUG_LEVEL .GE. 2 ) THEN
           WRITE ( 6, '(A)' ) 'PIMA_GET_VLBA_LOG: process VLBA log file'
      END IF
!      
      CALL GETENVAR ( 'PIMAVAR_MIN_FRG', STR )
      IF ( ILEN(STR) > 0 ) THEN
           CALL CHIN ( STR, PIM_MIN_FRG )
        ELSE 
           PIM_MIN_FRG = 1
      END IF
!
      CALL GETENVAR ( 'PIMAVAR_MAX_FRG', STR )
      IF ( ILEN(STR) > 0 ) THEN
           CALL CHIN ( STR, PIM_MAX_FRG )
        ELSE 
           PIM_MAX_FRG = PIM%NFRG
      END IF
!
      CALL GETENVAR ( 'PIMAVAR_SUB_BAND', PIMAVAR_SUB_BAND )
      IF ( ILEN(PIMAVAR_SUB_BAND) > 0 .AND. PIM%CONF%DEBUG_LEVEL > 0 ) THEN
           WRITE ( 6, '(A)' ) 'PIMA_GET_VLBA_LOG: kludge varaiable '// &
     &                        'PIMAVAR_SUB_BAND = '//PIMAVAR_SUB_BAND
      END IF
!
      ALLOCATE ( BUF(MBUF), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( LEN(BUF(1))*MBUF, STR )
           CALL ERR_LOG ( 8311, IUER, 'PIMA_GET_VLBA_LOG', 'Failure to '// &
     &         'allocate '//STR(1:I_LEN(STR))//' bytes of memory for '// &
     &         'array BUF' )
           RETURN
      END IF
!
      ALLOCATE ( TIM_TSYS_ARR(M_TIM,PIM%NFRG,PIM%NSTA), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( 8*M_TIM*PIM%NSTA*PIM%NFRG, STR )
           CALL ERR_LOG ( 8312, IUER, 'PIMA_GET_VLBA_LOG', 'Failure to '// &
     &         'allocate '//STR(1:I_LEN(STR))//' bytes of memory for '// &
     &         'array TIM_TSYS_ARR' )
           RETURN
      END IF
!
      ALLOCATE ( TSYS_ARR(M_CHN,M_TIM,PIM%NFRG,PIM%NPOL,PIM%NSTA), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( 8*M_TIM*M_CHN*PIM%NFRG*PIM%NPOL*PIM%NSTA, STR )
           CALL ERR_LOG ( 8313, IUER, 'PIMA_GET_VLBA_LOG', 'Failure to '// &
     &         'allocate '//STR(1:I_LEN(STR))//' bytes of memory for '// &
     &         'array TSYS_ARR' )
           RETURN
      END IF
!
      ALLOCATE ( NUM_TSYS(PIM%NSTA,PIM__MFRG), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( 4*M_TIM, STR )
           CALL ERR_LOG ( 8314, IUER, 'PIMA_GET_VLBA_LOG', 'Failure to '// &
     &         'allocate '//STR(1:I_LEN(STR))//' bytes of memory for '// &
     &         'array NUM_TSYS' )
           RETURN
      END IF
!
      ALLOCATE ( MJD_TAG(M_TIM,PIM%NFRG,PIM%NSTA), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( 4*M_TIM*PIM%NSTA, STR )
           CALL ERR_LOG ( 8315, IUER, 'PIMA_GET_VLBA_LOG', 'Failure to '// &
     &         'allocate '//STR(1:I_LEN(STR))//' bytes of memory for '// &
     &         'array MJD_TAG' )
           RETURN
      END IF
!
      ALLOCATE ( TAI_TAG(M_TIM,PIM%NFRG,PIM%NSTA), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( 8*M_TIM*PIM%NSTA, STR )
           CALL ERR_LOG ( 8316, IUER, 'PIMA_GET_VLBA_LOG', 'Failure to '// &
     &         'allocate '//STR(1:I_LEN(STR))//' bytes of memory for '// &
     &         'array TAI_TAG' )
           RETURN
      END IF
!
      ALLOCATE ( IND_SOU_PCAL(M_TIM,PIM%NFRG,PIM%NSTA), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( 4*M_TIM*PIM%NSTA, STR )
           CALL ERR_LOG ( 8317, IUER, 'PIMA_GET_VLBA_LOG', 'Failure to '// &
     &         'allocate '//STR(1:I_LEN(STR))//' bytes of memory for '// &
     &         'array IND_SOU_PCAL' )
           RETURN
      END IF
!
      ALLOCATE ( IND_SOU_TSYS(M_TIM,PIM%NFRG,PIM%NSTA), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( 4*M_TIM*PIM%NSTA*PIM%NFRG, STR )
           CALL ERR_LOG ( 8318, IUER, 'PIMA_GET_VLBA_LOG', 'Failure to '// &
     &         'allocate '//STR(1:I_LEN(STR))//' bytes of memory for '// &
     &         'array IND_SOU_TSYS' )
           RETURN
      END IF
!
      ALLOCATE ( IND_FRG_PCAL(M_TIM,PIM%NSTA), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( 4*M_TIM*PIM%NSTA, STR )
           CALL ERR_LOG ( 8319, IUER, 'PIMA_GET_VLBA_LOG', 'Failure to '// &
     &         'allocate '//STR(1:I_LEN(STR))//' bytes of memory for '// &
     &         'array IND_FRG_PCAL' )
           RETURN
      END IF
!
      ALLOCATE ( PCAL_PHS(PIM__M_TONES,PIM%NFRQ,PIM%NPOL,M_TIM,PIM%NFRG,PIM%NSTA), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( 4*PIM__M_TONES*PIM%NFRQ*M_TIM*PIM%NFRG*PIM%NSTA, STR )
           CALL ERR_LOG ( 8320, IUER, 'PIMA_GET_VLBA_LOG', 'Failure to '// &
     &         'allocate '//STR(1:I_LEN(STR))//' bytes of memory for '// &
     &         'array PCAL_PHS' )
           RETURN
      END IF
!
      ALLOCATE ( PCAL_AMP(PIM__M_TONES,PIM%NFRQ,PIM%NPOL,M_TIM,PIM%NFRG,PIM%NSTA), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( 4*PIM__M_TONES*PIM%NFRQ*M_TIM*PIM%NFRG*PIM%NSTA, STR )
           CALL ERR_LOG ( 8321, IUER, 'PIMA_GET_VLBA_LOG', 'Failure to '// &
     &         'allocate '//STR(1:I_LEN(STR))//' bytes of memory for '// &
     &         'array PCAL_AMP' )
           RETURN
      END IF
!
      ALLOCATE ( CABLE_CAL(M_TIM,PIM%NFRG,PIM%NSTA), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( 4*M_TIM*PIM%NFRG*PIM%NSTA, STR )
           CALL ERR_LOG ( 8322, IUER, 'PIMA_GET_VLBA_LOG', 'Failure to '// &
     &         'allocate '//STR(1:I_LEN(STR))//' bytes of memory for '// &
     &         'array CABLE_CAL' )
           RETURN
      END IF
!
      ALLOCATE ( MJD_WEA(M_TIM,PIM%NSTA), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( 4*M_TIM*PIM%NSTA, STR )
           CALL ERR_LOG ( 8323, IUER, 'PIMA_GET_VLBA_LOG', 'Failure to '// &
     &         'allocate '//STR(1:I_LEN(STR))//' bytes of memory for '// &
     &         'array MJD_WEA' )
           RETURN
      END IF
!
      ALLOCATE ( TAI_WEA(M_TIM,PIM%NSTA), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( 8*M_TIM*PIM%NSTA, STR )
           CALL ERR_LOG ( 8324, IUER, 'PIMA_GET_VLBA_LOG', 'Failure to '// &
     &         'allocate '//STR(1:I_LEN(STR))//' bytes of memory for '// &
     &         'array TAI_WEA' )
           RETURN
      END IF
!
      ALLOCATE ( TEMP(M_TIM,PIM%NSTA), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( 8*M_TIM*PIM%NSTA, STR )
           CALL ERR_LOG ( 8325, IUER, 'PIMA_GET_VLBA_LOG', 'Failure to '// &
     &         'allocate '//STR(1:I_LEN(STR))//' bytes of memory for '// &
     &         'array TEMP' )
           RETURN
      END IF
      CALL NOUT_R8 ( M_TIM*PIM%NSTA, TEMP )
!
      ALLOCATE ( PRES(M_TIM,PIM%NSTA), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( 8*M_TIM*PIM%NSTA, STR )
           CALL ERR_LOG ( 8326, IUER, 'PIMA_GET_VLBA_LOG', 'Failure to '// &
     &         'allocate '//STR(1:I_LEN(STR))//' bytes of memory for '// &
     &         'array PRES' )
           RETURN
      END IF
      CALL NOUT_R8 ( M_TIM*PIM%NSTA, PRES )
!
      ALLOCATE ( HUMID(M_TIM,PIM%NSTA), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( 8*M_TIM*PIM%NSTA, STR )
           CALL ERR_LOG ( 8327, IUER, 'PIMA_GET_VLBA_LOG', 'Failure to '// &
     &         'allocate '//STR(1:I_LEN(STR))//' bytes of memory for '// &
     &         'array HUMID' )
           RETURN
      END IF
      CALL NOUT_R8 ( M_TIM*PIM%NSTA, HUMID )
!
      CALL ERR_PASS ( IUER, IER )
      CALL RD_TEXT  ( LOG_FILE, MBUF, BUF, NBUF, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 8328, IUER, 'PIMA_GET_VLBA_LOG', 'Failure in '// &
     &         'an attempt to read antab file '//LOG_FILE )
           GOTO 810
      END IF
      NO_TONES = 0
!
      IF ( BUF(1)(1:18) == '! Produced by: TSM' .OR. &
     &     BUF(1)(1:22) == '! Produced by: rdbetsm' ) THEN
!
           CALL EXWORD ( BUF(2), MIND, LIND, IND, REG, -2 )
           IF ( LIND < 4 ) THEN
                CALL ERR_LOG ( 8329, IUER, 'PIMA_GET_VLBA_LOG', 'Wrong '// &
     &              'format of log file '//LOG_FILE(1:I_LEN(LOG_FILE))// &
     &              ' -- too few words at the second line' )
                GOTO 810
           END IF
           CALL CLRCH ( STR )
           CALL TRAN ( 12, BUF(2)(IND(1,4):IND(2,4)), STR )
           IF ( STR .NE. PIM%CONF%SESS_CODE  .AND. PIM%CONF%WARNING ) THEN
!
                WRITE ( 6, '(A)' ) '!!! WARNING !!!'
                WRITE ( 6, '(A)' ) '    Log file '//LOG_FILE(1:I_LEN(LOG_FILE))// &
     &                             ' corresponds to experiment '// &
     &                             STR(1:I_LEN(STR))
                WRITE ( 6, '(A)' ) '    while we process experiment '// &
     &                             PIM%CONF%SESS_CODE
           END IF
           IF ( STR .NE. PIM%CONF%SESS_CODE  .AND. PIM%CONF%CHECK_SEVERITY == 2 ) THEN
                CALL ERR_LOG ( 8330, IUER, 'PIMA_GET_VLBA_LOG', 'Failure '// &
     &              'in an attempt to parse log file '// &
     &               LOG_FILE(1:I_LEN(LOG_FILE))//' -- this log file '// &
     &              'corresponds to another experiment '//STR(1:I_LEN(STR))// &
     &              ' not '//PIM%CONF%SESS_CODE )
                GOTO 810
           END IF
         ELSE
           CALL ERR_LOG ( 8331, IUER, 'PIMA_GET_VLBA_LOG', 'Failure in '// &
     &         'an attempt to parse vlba_log_file '// &
     &          LOG_FILE(1:I_LEN(LOG_FILE))//' the fist line does '// &
     &         'NOT have magic required for VLBA log file' )
           GOTO 810
      END IF
!
! --- Initlialize VTD object
!
      IF ( VTD%STATUS .NE. VTD__INIT ) THEN
           CALL ERR_PASS ( IUER, IER )
           CALL VTD_INIT ( VTD,  IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 8332, IUER, 'PIMA_GET_VLBA_LOG', 'Error in an '// &
     &              'attempt to initialize VTD oibject' )
                GOTO 810
           END IF
      END IF
!
      IF ( VTD%STATUS .NE. VTD__LOAD ) THEN
!
! -------- Read and parse VTD configuration file
!
           CALL ERR_PASS ( IUER, IER )
           CALL VTD_CONF ( PIM%CONF%VTD_CONFIG_FILE, VTD, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 8333, IUER, 'PIMA_GET_VLBA_LOG', 'Error in '// &
     &              'an attempt to read configuration file '// &
     &               PIM%CONF%VTD_CONFIG_FILE )
                GOTO 810
          END IF
!
! ------- Load catalogues, ephemerides, EOP series and other data files
!
          CALL ERR_PASS ( IUER, IER )
          CALL VTD_LOAD ( VTD, PIM%NSTA, PIM%C_STA, PIM%NSOU, PIM%C_SOU, &
     &                    PIM%MJD_0, PIM%TAI_0, PIM%MJD_0, &
     &                    PIM%TAI_0 + PIM%TIM_R8(PIM%NEPC), IER )
          IF ( IER .NE. 0 ) THEN
               CALL ERR_LOG ( 8334, IUER, 'PIMA_GET_VLBA_LOG', 'Error in an '// &
     &             'attempt to load the data into VTD data structure' )
               GOTO 810
          END IF
!
! ------- Disable automatic NERS update during the run
!
          VTD%NERS%CNF%AGE_FCS = 1.D15
          VTD%NERS%CNF%AGE_SPL = 1.D15
!
          IF ( ILEN(PIM%CONF%EPHEMERIDES_FILE) > 0 ) THEN
!
! ------------ Read the ephemeride of the orbiting station
!
               PIM%NZO%FILNZO = PIM%CONF%EPHEMERIDES_FILE
               CALL ERR_PASS ( IUER, IER )
               CALL VTD_READ_NZO ( PIM%NZO%FILNZO, PIM__MNZO, PIM%NZO%L_NZO, &
     &                             PIM%NZO%MJD_ARR, PIM%NZO%TIM_ARR, PIM%NZO%POS_ARR, &
     &                             PIM%NZO%VEL_ARR, PIM%NZO%NZO_NAME, PIM%NZO%OBJ_TYPE, &
     &                             PIM%NZO%CENTER_NAME, PIM%NZO%REF_NAME, &
     &                             PIM%NZO%TIM_CODE, PIM%NZO%COO_CODE, IER )
               IF ( IER .NE. 0 ) THEN
                    CALL ERR_LOG ( 8335, IUER, 'PIMA_GET_ANTAB', 'Error in an '// &
     &                  'attempt to read NZO data into VTD data structure' )
                    GOTO 810
               END IF
               IF ( PIM%NZO%CENTER_NAME == 'EARTH BARYCENTER' ) THEN
                    NZO_REF = VTD__EME
                  ELSE
                    CALL ERR_LOG ( 8336, IUER, 'PIMA_GET_VLBA_LOG', 'Unsupported '// &
     &                  'coordinate center name: '//PIM%NZO%CENTER_NAME )
                    GOTO 810
               END IF
!
! ------------ Expand the orbiting station ephemeride into the B-spline basis
!
               L_NOD = MIN ( PIM%NZO%L_NZO/PIMA__NZO_NOT_SCAL, VTD__M_NOD )
               L_DEG = 3
               CALL ERR_PASS ( IUER, IER )
               CALL VTD_LOAD_OBJ_NZO ( PIM%NZO%NZO_NAME, VTD__ES, VTD__OR, &
     &                                 NZO_REF, PIM%NZO%TIM_CODE, &
     &                                 VTD, PIM%NZO%L_NZO, PIM%NZO%MJD_ARR, &
     &                                 PIM%NZO%TIM_ARR, PIM%NZO%POS_ARR, &
     &                                 L_NOD, L_DEG, IER )
               IF ( IER .NE. 0 ) THEN
                    CALL ERR_LOG ( 8337, IUER, 'PIMA_GET_VLBA_LOG', 'Error in an '// &
     &                  'attempt to read NZO data into VTD data structure' )
                    GOTO 810
                    RETURN
               END IF
          END IF
      END IF
!
! --- Intitialize correlator flags (set them all to "BEST" )
!
      CALL CORR_FLAG_INIT ( PIM )
!
      IND_STA  = 0
      IND_SCA  = 0
      IND_SOU  = 0
      N_ONS    = 0
      N_CAB    = 0
      N_MET    = 0
      SIGN_CAB = 1
      NUM_TSYS = 0
      IND_SOU_TSYS = 0
      IND_SOU_PCAL = 0
      TIM_TSYS_ARR = 0.0D0
      CALL NOUT_R8 ( M_POLY, GAIN_POLY )
      CALL NOUT_I4 ( M_CHN,  CROSS_COL )
      DATE_STR = MJDSEC_TO_DATE ( PIM%MJD_0, PIM%TAI_0, -2 )
      DATE_STR(6:) = '01.01_00:00:00.0'
      CALL DATE_TO_TIME ( DATE_STR, MJD_NEW_YEAR, SEC, -2 )
!
      L_POLY    = 0
      NPC_STA   = 0
      FL_PC     = .FALSE.
      FL_FR     = .FALSE.
      FL_PU     = .FALSE.
      FL_WEA    = .FALSE.
      FL_TSYS   = .FALSE.
      IND_FRG   = 0
      IF ( BUF(1)(1:22) == '! Produced by: rdbetsm' ) THEN
           IND_FRG = 1 
           FL_PCAL_UPDATE = .FALSE.
         ELSE IF ( BUF(1)(1:18) == '! Produced by: TSM' ) THEN
           IND_FRG = 1 
           FL_PCAL_UPDATE = .TRUE.
         ELSE
           FL_PCAL_UPDATE = .TRUE.
      END IF
      IND_FRG_SAVE = IND_FRG
      IND_FRQ   = 0
      IND2_FREQ = 0
      IND2_FRG  = 0
      NUM_WEA   = 0
      FRQ_PT    = 0.0D0
      CABLE_MEAN = 0.0D0
      FL_GEN_VLBA = .FALSE.
!
      DO 410 J1=2,NBUF
         IF ( BUF(J1)(1:1)  == '#'  ) GOTO 410
         IF ( BUF(J1)(1:2)  == '!@' ) GOTO 410
         IF ( ILEN(BUF(J1)) ==  0   ) GOTO 410
         IF ( INDEX ( BUF(J1), 'Doing section(s):' ) > 0 .OR. &
     &        INDEX ( BUF(J1), 'NRAO stations'     ) > 0      ) THEN 
!
! ----------- This line discriminates genuine VLBA calibration log file from others
!
              FL_GEN_VLBA = .TRUE.
         END IF
         CALL EXWORD ( BUF(J1), MIND, LIND, IND, REG, -2 )
         IF ( BUF(J1)(1:40) == '! ----- PulseCal information for Y -----' ) THEN
              CONTINUE
           ELSE IF ( BUF(J1)(1:32) == '! ----- PulseCal information for' ) THEN
              STA_NAM_2LET = BUF(J1)(IND(1,6):IND(2,6))
              STA_NAM = GET_IVS_NAME ( PIM, STA_NAM_2LET )
              IF ( STA_NAM == 'Unknwon ' ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( J1, STR )
                   CALL ERR_LOG ( 8338, IUER, 'PIMA_GET_VLBA_LOG', &
     &                 'Error in parsing the '//STR(1:I_LEN(STR))// &
     &                 ' line '//BUF(J1)(1:I_LEN(BUF(J1)))//' of the '// &
     &                 'log file '//LOG_FILE(1:I_LEN(LOG_FILE))// &
     &                 ' -- faiulre in decoding the 2-characters '// &
     &                 'VLBA station name' )
                   GOTO 810
              END IF
              IND_STA = LTM_DIF ( 0, PIM%NSTA, PIM%C_STA, STA_NAM )
              IF ( IND_STA .LE. 0 ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( J1, STR )
                   CALL ERR_LOG ( 8339, IUER, 'PIMA_GET_VLBA_LOG', &
     &                 'Error in parsing the '//STR(1:I_LEN(STR))// &
     &                 ' line '//BUF(J1)(1:I_LEN(BUF(J1)))//' of the '// &
     &                 'log file '//LOG_FILE(1:I_LEN(LOG_FILE))// &
     &                 ' -- station '//STA_NAM//' did not observe '// &
     &                 'in experiment '//PIM%CONF%SESS_CODE )
                   GOTO 810
              END IF
              FL_PC = .TRUE.
              FL_PU = .FALSE.
              FL_FR = .FALSE.
              IF ( PIM%CONF%DEBUG_LEVEL .GE. 6 ) THEN
                   WRITE ( 6, * ) 'PIMA_GET_VLBA_LOG-486 line= ', J1, ' FL_PC= T' 
              END IF
         END IF
         IF ( BUF(J1)(1:28) == '! ----- Scan information for' ) THEN
              FL_WEA = .FALSE.
              FL_PC  = .FALSE.
              GOTO 410
         END IF
         IF ( BUF(J1)(1:9) == '/' ) THEN
              IF ( FL_FR ) THEN
                   CALL NOUT_I4 ( PIM__M_TONES*PIM__MFRQ, IND2_TONE  )
                   CALL NOUT_I4 ( 2*PIM__MFRQ*PIM__MUVS,  IND_FRQ_PT )
                   DO 420 J2=1,NPT
                      IF ( IND2_FRG(J2) > PIM__MUVS ) GOTO 420
                      IF ( IND2_FREQ(J2) > 0 ) THEN
                           IND_FRQ_PT(IND2_FREQ(J2),IND2_POL(J2),IND2_FRG(J2)) = &
     &                         IND_FRQ_PT(IND2_FREQ(J2),IND2_POL(J2),IND2_FRG(J2)) + 1
                           IND2_TONE(J2) = IND_FRQ_PT(IND2_FREQ(J2),IND2_POL(J2),IND2_FRG(J2))
                      END IF
 420               CONTINUE
                   FL_FR = .FALSE.
                   GOTO 410
              END IF
              IF ( FL_PU ) THEN
                   FL_PU = .FALSE.
                   GOTO 410
              END IF
              IF ( FL_FR ) THEN
                   FL_FR = .FALSE.
                   GOTO 410
              END IF
         END IF
         IF ( BUF(J1)(1:9) == 'FREQUENCY' ) THEN
              NPT = 0
              LPT = 0
              FL_FR = .TRUE.
              GOTO 410
         END IF
         IF ( BUF(J1)(1:9) == 'PULSE-CAL' .AND. IND_FRG .NE. 0 ) THEN
              FL_PU = .TRUE.
              GOTO 410
         END IF
         IF ( FL_FR ) THEN
              IF ( BUF(J1)(1:1) == '!' ) GOTO 410
              NPT = NPT + 1
              PCAL_TAB(NPT) = BUF(J1)(IND(1,1):IND(2,1))
              IF ( BUF(J1)(IND(1,4):IND(2,4)) == "RCP" ) THEN
                   IND2_POL(NPT) = 1
                 ELSE IF ( BUF(J1)(IND(1,4):IND(2,4)) == "LCP"  .AND. &
     &                     PIM%NPOL == 1 ) THEN
                   IND2_POL(NPT) = 1
                 ELSE IF ( BUF(J1)(IND(1,4):IND(2,4)) == "LCP"  .AND. &
     &                     PIM%NPOL > 1 ) THEN
                   IND2_POL(NPT) = 2
                 ELSE
                   CALL CLRCH ( STR )
                   CALL INCH  ( J1, STR )
                   CALL ERR_LOG ( 8340, IUER, 'PIMA_GET_VLBA_LOG', &
     &                 'Error in parsing the '//STR(1:I_LEN(STR))// &
     &                 ' line '//BUF(J1)(1:I_LEN(BUF(J1)))//' of the '// &
     &                 'log file '//LOG_FILE(1:I_LEN(LOG_FILE))// &
     &                 ' -- unsupported polarization code '// &
     &                 BUF(J1)(IND(1,4):IND(2,4)) )
                   RETURN
              END IF
!
              CALL CHIN ( BUF(J1)(IND(1,2):IND(2,2)), IVAL )
!
              SB_STR = BUF(J1)(IND(1,3):IND(2,3))
              READ ( UNIT=BUF(J1)(IND(1,5):IND(2,5)), FMT='(F20.0)', &
     &               IOSTAT=IER ) FRQ_PT(NPT)
              IF ( IER .NE. 0 ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( J1, STR )
                   CALL ERR_LOG ( 8341, IUER, 'PIMA_GET_VLBA_LOG', &
     &                 'Error in parsing the '//STR(1:I_LEN(STR))// &
     &                 ' line '//BUF(J1)(1:I_LEN(BUF(J1)))//' of the '// &
     &                 'log file '//LOG_FILE(1:I_LEN(LOG_FILE))// &
     &                 ' -- faiulre in decoding the 5th word' )
                   GOTO 810
              END IF
              IF ( FRQ_PT(NPT) == 0.0 ) GOTO 410
!
              READ ( UNIT=BUF(J1)(IND(1,5):IND(2,5)), FMT='(F20.0)', &
     &               IOSTAT=IER ) FRQ_SKY(NPT)
              IF ( IER .NE. 0 ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( J1, STR )
                   CALL ERR_LOG ( 8342, IUER, 'PIMA_GET_VLBA_LOG', &
     &                 'Error in parsing the '//STR(1:I_LEN(STR))// &
     &                 ' line '//BUF(J1)(1:I_LEN(BUF(J1)))//' of the '// &
     &                 'log file '//LOG_FILE(1:I_LEN(LOG_FILE))// &
     &                 ' -- faiulre in decoding the 7th word' )
                   GOTO 810
              END IF
              FRQ_PT(NPT)  = 1.D6*FRQ_PT(NPT)
              FRQ_SKY(NPT) = 1.D6*DABS(FRQ_SKY(NPT))
              IND_FRQ = 0
              IND_FRG = 0
              DELTA_FRQ = 1.D11
!
!!              DO 530 J3=1,PIM%NFRG
              DO 530 J3=PIM_MIN_FRG,PIM_MAX_FRG
                 DO 540 J4=1,PIM%NFRQ
                    IF ( PIM%FRQ(J4,J3)%SIDE_BAND == 1 ) THEN
                         FRQ_IF = PIM%FRQ(J4,J3)%FREQ
                       ELSE IF ( PIM%FRQ(J4,J3)%SIDE_BAND == -1 ) THEN
                         FRQ_IF = PIM%FRQ(J4,J3)%FREQ - PIM%FRQ(J4,J3)%CHAN_WIDTH
                    END IF
                    IF ( FRQ_SKY(NPT) .GE. FRQ_IF - FRQ_TOL .AND. &
     &                   FRQ_SKY(NPT) .LE. FRQ_IF + PIM%FRQ(J4,J3)%BAND_WIDTH + FRQ_TOL ) THEN
                         IF ( IND_FRQ == 0  .AND.  IND_FRG == 0 ) THEN
                              IND_FRQ = J4
                              IND_FRG = J3
                              DELTA_FRQ = DABS ( FRQ_SKY(NPT) - FRQ_IF )
                            ELSE
!
! --------------------------- A special trick for handling a pathological case of
! --------------------------- overlapping IFs
!
                              IF ( DABS ( FRQ_SKY(NPT) - FRQ_IF ) < DELTA_FRQ ) THEN
                                   IND_FRQ = J4
                                   IND_FRG = J3
                                   DELTA_FRQ = DABS ( FRQ_SKY(NPT) - &
     &                                           (PIM%FRQ(J4,J3)%FREQ + PIM%FRQ(J4,J3)%BAND_WIDTH/2.0D0 ) )
                              END IF
                         END IF
                    END IF
 540             CONTINUE
 530          CONTINUE
!
              IF ( PIM%CONF%DEBUG_LEVEL .GE. 8 ) THEN
                   WRITE ( 6, * ) 'PIMA_GET_VLBA_LOG-616 line= ', J1, ' IND_FRQ= ', INT2(IND_FRQ), &
     &                            ' IND_FRG= ', INT2(IND_FRG), ' NPT= ', NPT
              END IF
!
              IF ( FRQ_PT(NPT) <  PIMA__MIN_FRQ ) THEN
                   IND_FRQ = -1
                 ELSE
                   LPT = LPT + 1
              END IF
!
              IF ( IND_FRG == 0 ) THEN
                   IF ( PIM%CONF%CHECK_SEVERITY .GE. 2 ) THEN
                        CALL CLRCH ( STR )
                        CALL INCH  ( J1, STR )
                        CALL ERR_LOG ( 8343, IUER, 'PIMA_GET_VLBA_LOG', &
     &                       'Error in parsing the '//STR(1:I_LEN(STR))// &
     &                       ' -th line '//BUF(J1)(1:I_LEN(BUF(J1)))//' of the '// &
     &                       'log file '//LOG_FILE(1:I_LEN(LOG_FILE))// &
     &                       ' -- frequency '//BUF(J1)(IND(1,5):IND(2,5))// &
     &                       ' MHz was not observed in experiment '// &
     &                       PIM%CONF%SESS_CODE )
                         GOTO 810
                    END IF
!
                    IF ( PIM%CONF%DEBUG_LEVEL .GE. 1 ) THEN
                         CALL CLRCH ( STR )
                         CALL INCH  ( J1, STR )
                         WRITE ( 6, '(A)' ) 'PIMA_GET_VLBA_LOG: Warning during '// &
     &                         'in parsing the '//STR(1:I_LEN(STR))// &
     &                         ' -th line '//BUF(J1)(1:I_LEN(BUF(J1)))//' of '// &
     &                         'the log file '//LOG_FILE(1:I_LEN(LOG_FILE))// &
     &                         ' -- frequency '//BUF(J1)(IND(1,5):IND(2,5))// &
     &                         ' MHz was NOT observed in experiment '// &
     &                         TRIM(PIM%CONF%SESS_CODE)
                   END IF
              END IF
              IND2_FREQ(NPT) = IND_FRQ
              IND2_FRG(NPT)  = IND_FRG
              GOTO 410 ! Added on 2011.09.16
         END IF
         IF ( FL_PC               .AND. &
     &        BUF(J1)(1:1) == '!' .AND. &
     &        LIND .GE. 7               ) THEN
              IF ( BUF(J1)(IND(1,2):IND(2,2)) == STA_NAM_2LET ) THEN
                   IND_SOU = LTM_DIF ( 0, PIM%NSOU, PIM%C_SOU, &
     &                                 BUF(J1)(IND(1,4):IND(2,4)) )
                   IF ( IND_SOU .LE. 0 ) THEN
!
! --------------------- If the source was not found, let us search it
! --------------------- in the lists of alternative source names
!
                        IL = IND(2,4) - IND(1,4) + 1
                        DO 430 J3=1,PIM%NSOU
                           IF ( PIM%SOU(J3)%NAME(1:MIN(IL,16)) == BUF(J1)(IND(1,4):IND(1,4)+MIN(IL,16)-1) ) THEN
                                IND_SOU = J3
                                GOTO 830
                              ELSE IF ( PIM%SOU(J3)%DB_NAME(1:MIN(IL,10)) == BUF(J1)(IND(1,4):IND(1,4)+MIN(IL,10)-1) ) THEN
                                IND_SOU = J3
                                GOTO 830
                              ELSE IF ( PIM%SOU(J3)%J2000_NAME(1:MIN(IL,10)) == BUF(J1)(IND(1,4):IND(1,4)+MIN(IL,10)-1) ) THEN
                                IND_SOU = J3
                                GOTO 830
                              ELSE IF ( PIM%SOU(J3)%IVS_NAME(1:MIN(IL,8)) == BUF(J1)(IND(1,4):IND(1,4)+MIN(IL,8)-1) ) THEN
                                IND_SOU = J3
                                GOTO 830
                           END IF
 430                    CONTINUE
 830                    CONTINUE
!
                        IF ( IND_SOU .LE. 0 ) THEN
                             IF ( PIM%CONF%CHECK_SEVERITY .LE. 1 ) GOTO 410
                             CALL CLRCH ( STR )
                             CALL INCH  ( J1, STR )
                             CALL ERR_LOG ( 8344, IUER, 'PIMA_GET_VLBA_LOG', 'Error'// &
     &                           ' in parsing the '//STR(1:I_LEN(STR))//'-th line'// &
     &                           ' of the log file '//LOG_FILE(1:I_LEN(LOG_FILE))// &
     &                           ' source '//BUF(J1)(IND(1,4):IND(2,4))//' was'// &
     &                           ' not observed in exeperiment '//PIM%CONF%SESS_CODE )
                             GOTO 810
                        END IF
                   END IF
              END IF
         END IF
         IF ( PIM%CONF%DEBUG_LEVEL .GE. 8 ) THEN
              WRITE ( 6, * ) 'PIMA_GET_VLBA_LOG-709 line= ', J1, ' FL_PC= ', FL_PC, ' FL_PU= ', FL_PU, &
     &                       ' FL_FR= ', FL_FR, ' IND_FRG= ', IND_FRG
              CALL FLUSH ( 6 )
         END IF
!
         IF ( FL_PU .AND. BUF(J1)(1:1) .NE. '!' ) THEN
              IF ( BUF(J1)(IND(1,3):IND(2,3)) == 'CC' ) THEN
                   NPC_STA(IND_STA,1:PIM%NFRG) = NPC_STA(IND_STA,1:PIM%NFRG) + 1
                   IND_SOU_PCAL(NPC_STA(IND_STA,IND_FRG),IND_FRG,IND_STA) = IND_SOU
                   READ ( UNIT=BUF(J1)(IND(1,4):IND(2,4)), FMT='(F7.2)', &
     &                    IOSTAT=IER ) CABLE_CAL(NPC_STA(IND_STA,IND_FRG),IND_FRG,IND_STA)
                   IF ( IER .NE. 0 ) THEN
                        CALL CLRCH ( STR )
                        CALL INCH  ( J1, STR )
                        CALL ERR_LOG ( 8345, IUER, 'PIMA_GET_VLBA_LOG', &
     &                     'Error in parsing the '//STR(1:I_LEN(STR))// &
     &                     ' line '//BUF(J1)(1:I_LEN(BUF(J1)))//' of the '// &
     &                     'log file '//LOG_FILE(1:I_LEN(LOG_FILE))// &
     &                     ' -- faiulre in decoding the 4th word' )
                        GOTO 810
                   END IF
                   CABLE_CAL(NPC_STA(IND_STA,IND_FRG),IND_FRG,IND_STA) = &
     &                       1.D-12*CABLE_CAL(NPC_STA(IND_STA,IND_FRG),IND_FRG,IND_STA)
                   CABLE_MEAN(IND_STA,IND_FRG) = CABLE_MEAN(IND_STA,IND_FRG) + &
     &                                   CABLE_CAL(NPC_STA(IND_STA,IND_FRG),IND_FRG,IND_STA)
                 ELSE
                   IND_PTC = LTM_DIF ( 0, NPT, PCAL_TAB, &
     &                                 BUF(J1)(IND(1,3):IND(2,3)) )
                   IF ( IER .NE. 0 ) THEN
                        CALL CLRCH ( STR )
                        CALL INCH  ( J1, STR )
                        CALL ERR_LOG ( 8346, IUER, 'PIMA_GET_VLBA_LOG', &
     &                     'Error in parsing the '//STR(1:I_LEN(STR))// &
     &                     ' line '//BUF(J1)(1:I_LEN(BUF(J1)))//' of the '// &
     &                     'log file '//LOG_FILE(1:I_LEN(LOG_FILE))// &
     &                     ' -- unknown tone code' )
                        GOTO 810
                   END IF
                   IF ( FRQ_PT(IND_PTC) == 0.0D0 ) GOTO 410
                   IF ( BUF(J1-1)(IND(1,3):IND(1,3)+1) .NE. 'CC' .AND. &
     &                  BUF(J1-1)(1:1) .NE. '!'                        ) THEN
                        IND_PTC_PREV = LTM_DIF ( 0, NPT, PCAL_TAB, &
     &                                           BUF(J1-1)(IND(1,3):IND(2,3)) )
                        IF ( IND2_FRG ( IND_PTC_PREV ) .NE. &
     &                       IND2_FRG ( IND_PTC )           ) THEN
                             NPC_STA(IND_STA,IND_FRG) = NPC_STA(IND_STA,IND_FRG) + 1
                             IND_SOU_PCAL(NPC_STA(IND_STA,IND_FRG),IND_FRG,IND_STA) = &
     &                                IND_SOU_PCAL(NPC_STA(IND_STA,IND_FRG)-1,IND_FRG,IND_STA)
                             CABLE_CAL(NPC_STA(IND_STA,IND_FRG),IND_FRG,IND_STA) = &
     &                                 CABLE_CAL(NPC_STA(IND_STA,IND_FRG)-1,IND_FRG,IND_STA)
                        END IF
                   END IF
                   IF ( IND_PTC == 0 ) THEN
                        CALL ERR_LOG ( 8347, IUER, 'PIMA_GET_VLBA_LOG', &
     &                     'Error in parsing the '//STR(1:I_LEN(STR))// &
     &                     ' line '//BUF(J1)(1:I_LEN(BUF(J1)))//' of the '// &
     &                     'log file '//LOG_FILE(1:I_LEN(LOG_FILE))// &
     &                     ' -- IND_PTC = 0' )
                        RETURN
                   END IF
                   IND_FREQ = IND2_FREQ ( IND_PTC )
                   IND_TONE = IND2_TONE ( IND_PTC )
                   IND_POL  = IND2_POL  ( IND_PTC )
                   IND_FRG  = IND2_FRG  ( IND_PTC )
                   IF ( IND_TONE .GT. PIM__M_TONES ) GOTO 410
                   IF ( IND_TONE .LE. 0 ) GOTO 410
                   IF ( IND_FREQ .LE. 0 ) GOTO 410
                   IF ( IND_FRG  .LE. 0 ) GOTO 410
                   NO_TONES = MAX ( NO_TONES, IND_TONE )
!
                   IND_POL_USE = IND_POL
                   IF ( IND_POL == 2 .AND. PIM%NPOL == 1 ) THEN
                        IND_POL_USE = 1
                        IND_FREQ = IND_FREQ + 1
                   END IF
!                   IF ( NPC_STA(IND_STA,IND_FRG) < 1 ) THEN
!                        GOTO 410
!                   END IF
                   READ ( UNIT=BUF(J1)(IND(1,4):IND(2,4)), FMT='(F9.4)', &
     &                    IOSTAT=IER ) PCAL_AMP(IND_TONE,IND_FREQ,IND_POL_USE,NPC_STA(IND_STA,IND_FRG),IND_FRG,IND_STA)
                   IF ( IER .NE. 0 ) THEN
                        CALL CLRCH ( STR )
                        CALL INCH  ( J1, STR )
                        CALL ERR_LOG ( 8348, IUER, 'PIMA_GET_VLBA_LOG', &
     &                     'Error in parsing the '//STR(1:I_LEN(STR))// &
     &                     ' line '//BUF(J1)(1:I_LEN(BUF(J1)))//' of the '// &
     &                     'log file '//LOG_FILE(1:I_LEN(LOG_FILE))// &
     &                     ' -- faiulre in decoding the 4th word' )
                        GOTO 810
                   END IF
!
                   READ ( UNIT=BUF(J1)(IND(1,5):IND(2,5)), FMT='(F8.3)', &
     &                    IOSTAT=IER ) PCAL_PHS(IND_TONE,IND_FREQ,IND_POL_USE,NPC_STA(IND_STA,IND_FRG),IND_FRG,IND_STA)
                   IF ( IER .NE. 0 ) THEN
                        CALL CLRCH ( STR )
                        CALL INCH  ( J1, STR )
                        CALL ERR_LOG ( 8349, IUER, 'PIMA_GET_VLBA_LOG', &
     &                     'Error in parsing the '//STR(1:I_LEN(STR))// &
     &                     ' line '//BUF(J1)(1:I_LEN(BUF(J1)))//' of the '// &
     &                     'log file '//LOG_FILE(1:I_LEN(LOG_FILE))// &
     &                     ' -- faiulre in decoding the 5th word' )
                        GOTO 810
                   END IF
                   PCAL_AMP(IND_TONE,IND_FREQ,IND_POL_USE,NPC_STA(IND_STA,IND_FRG),IND_FRG,IND_STA) = &
     &                      1.D-3*PCAL_AMP(IND_TONE,IND_FREQ,IND_POL_USE,NPC_STA(IND_STA,IND_FRG),IND_FRG,IND_STA)
                   PCAL_PHS(IND_TONE,IND_FREQ,IND_POL_USE,NPC_STA(IND_STA,IND_FRG),IND_FRG,IND_STA) = &
     &                      DEG__TO__RAD*PCAL_PHS(IND_TONE,IND_FREQ,IND_POL_USE,NPC_STA(IND_STA,IND_FRG),IND_FRG,IND_STA)
                   IND_FRG_PCAL(NPC_STA(IND_STA,IND_FRG),IND_STA) = IND_FRG
                   FREQ_TONE(IND_TONE,IND_FREQ,IND_FRG,IND_STA) = FRQ_PT(IND_PTC)
              END IF
!
              CALL CHIN ( BUF(J1)(IND(1,1):IND(2,1)), IDOY )
              IF ( IDOY < 1 .OR. IDOY > 366 ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( J1, STR )
                   CALL ERR_LOG ( 8350, IUER, 'PIMA_GET_VLBA_LOG', 'Error '// &
     &                 'in parsing the first word, DOY, '// &
     &                  BUF(J1)(IND(1,1):IND(2,1))//' of the '// &
     &                  STR(1:I_LEN(STR))//'-th line of the log file '// &
     &                  LOG_FILE )
                   GOTO 810
              END IF
!
! ----------- Now we parse time string, first from string to radians
!
              CALL ERR_PASS ( IUER, IER )
              CALL HR_TAT   ( BUF(J1)(IND(1,2):IND(2,2)), ANG_RAD, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( J1, STR )
                   CALL ERR_LOG ( 8351, IUER, 'PIMA_GET_VLBA_LOG', 'Error '// &
     &                 'in parsing the second word, UTC_time, '// &
     &                  BUF(J1)(IND(1,2):IND(2,2))//' of the '// &
     &                  STR(1:I_LEN(STR))//'-th line of the log file '// &
     &                  LOG_FILE )
                   GOTO 810
              END IF
!
! ----------- ... and then from radians to UTC
!
              CALL RS_TAT ( ANG_RAD, UTC_TAG )

! ----------- ... and then from UTC to TAI
!
              TAI_TAG(NPC_STA(IND_STA,IND_FRG),IND_FRG,IND_STA) = UTC_TAG - PIM%UTC_MTAI
              MJD_TAG(NPC_STA(IND_STA,IND_FRG),IND_FRG,IND_STA) = MJD_NEW_YEAR + IDOY - 1
           ELSE IF ( BUF(J1)(1:9) == 'ant_name=' ) THEN
              STA_NAM_2LET = BUF(J1)(IND(1,2):IND(2,2))
              STA_NAM = GET_IVS_NAME ( PIM, STA_NAM_2LET )
              IF ( STA_NAM == 'Unknown ' ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( J1, STR )
                   CALL ERR_LOG ( 8352, IUER, 'PIMA_GET_VLBA_LOG', &
     &                 'Error in parsing the '//STR(1:I_LEN(STR))// &
     &                 ' line '//BUF(J1)(1:I_LEN(BUF(J1)))//' of the '// &
     &                 'log file '//LOG_FILE(1:I_LEN(LOG_FILE))// &
     &                 ' -- faiulre in decoding the 2-characters '// &
     &                 'VLBA station name '//STA_NAM_2LET// &
     &                 ' apparently the station did not observe in '// &
     &                 'this experiment. If this true, you need to comment '// &
     &                 'out records with '//STA_NAM_2LET//' in the VLBI '// &
     &                 'calibration file' )
                   GOTO 810
              END IF
              IND_STA = LTM_DIF ( 0, PIM%NSTA, PIM%C_STA, STA_NAM )
              IF ( IND_STA .LE. 0 ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( J1, STR )
                   CALL ERR_LOG ( 8353, IUER, 'PIMA_GET_VLBA_LOG', &
     &                 'Error in parsing the '//STR(1:I_LEN(STR))// &
     &                 ' line '//BUF(J1)(1:I_LEN(BUF(J1)))//' of the '// &
     &                 'log file '//LOG_FILE(1:I_LEN(LOG_FILE))// &
     &                 ' -- station '//STA_NAM//' did not observe '// &
     &                 'in experiment '//PIM%CONF%SESS_CODE )
                   GOTO 810
              END IF
!
              CALL CHIN ( BUF(J1)(IND(1,4):IND(2,4)), IDOY )
              IF ( IDOY < 1 .OR. IDOY > 366 ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( J1, STR )
                   CALL ERR_LOG ( 8354, IUER, 'PIMA_GET_VLBA_LOG', &
     &                 'Error in parsing the '//STR(1:I_LEN(STR))// &
     &                 ' line '//BUF(J1)(1:I_LEN(BUF(J1)))//' of the '// &
     &                 'log file '//LOG_FILE(1:I_LEN(LOG_FILE))// &
     &                 ' -- failure in decoding BEG_DOY '// &
     &                 BUF(J1)(IND(1,4):IND(2,4)) )
                   RETURN
              ENDIF
              STR = MJDSEC_TO_DATE ( MJD_NEW_YEAR + IDOY -1, 0.0D0, -2 )
              DATE_BEG = STR(1:10)//'_'//BUF(J1)(IND(1,5):IND(2,5))// &
     &                              ':'//BUF(J1)(IND(1,6):IND(2,6))// &
     &                              ':'//BUF(J1)(IND(1,7):IND(2,7))// &
     &                              '.0'
              CALL CHIN ( BUF(J1)(IND(1,8):IND(2,8)), IDOY )
              IF ( IDOY < 1 .OR. IDOY > 366 ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( J1, STR )
                   CALL ERR_LOG ( 8355, IUER, 'PIMA_GET_VLBA_LOG', &
     &                 'Error in parsing the '//STR(1:I_LEN(STR))// &
     &                 ' line '//BUF(J1)(1:I_LEN(BUF(J1)))//' of the '// &
     &                 'log file '//LOG_FILE(1:I_LEN(LOG_FILE))// &
     &                 ' -- failure in decoding END_DOY '// &
     &                 BUF(J1)(IND(1,8):IND(2,8)) )
                   RETURN
              ENDIF
              STR = MJDSEC_TO_DATE ( MJD_NEW_YEAR + IDOY -1, 0.0D0, -2 )
              DATE_END = STR(1:10)//'_'//BUF(J1)(IND(1,9):IND(2,9))// &
     &                              ':'//BUF(J1)(IND(1,10):IND(2,10))// &
     &                              ':'//BUF(J1)(IND(1,11):IND(2,11))// &
     &                              '.0'
              CALL DATE_TO_TIME ( DATE_BEG, MJD_BEG, UTC_BEG, -2 )
              CALL DATE_TO_TIME ( DATE_END, MJD_END, UTC_END, -2 )
              IF ( PIM%TIM_SCL == PIMA__TAI ) THEN
                   TIM_FLAG_BEG = (MJD_BEG - PIM%MJD_0)*86400.0D0 + &
     &                            (UTC_BEG - PIM%TAI_0)
                   TIM_FLAG_END = (MJD_BEG - PIM%MJD_0)*86400.0D0 + &
     &                            (UTC_END - PIM%TAI_0)
                 ELSE IF ( PIM%TIM_SCL == PIMA__UTC ) THEN
                   TIM_FLAG_BEG = (MJD_BEG - PIM%MJD_0)*86400.0D0 + &
     &                            (UTC_BEG - PIM%TAI_0) - PIM%UTC_MTAI
                   TIM_FLAG_END = (MJD_BEG - PIM%MJD_0)*86400.0D0 + &
     &                            (UTC_END - PIM%TAI_0) - PIM%UTC_MTAI
              END IF
!
              IF ( IND_FRG == 0 ) THEN
                   CALL ERR_LOG ( 8356, IUER, 'PIMA_GET_VLBA_LOG', &
     &                 'Trap of inernal control: IND_FRG == 0 was '// &
     &                 ' detected whil processing '// &
     &                 'log file '//LOG_FILE )
                   GOTO 810
              END IF
              DO 440 J4=1,PIM%NOBS
                 TIM_OBS_BEG = PIM%TIM_R8(PIM%UV_IND(PIM%OBS(J4)%UV_IND(1,IND_FRG))%TIM_IND)
                 TIM_OBS_END = PIM%TIM_R8(PIM%UV_IND(PIM%OBS(J4)%UV_IND(PIM%OBS(J4)%NUM_EPC(IND_FRG),IND_FRG))%TIM_IND)
                 IF ( ( ( TIM_FLAG_BEG > TIM_OBS_BEG - TIM_UV_EPS .AND. &
     &                    TIM_FLAG_BEG < TIM_OBS_END + TIM_UV_EPS       ) .OR. &
     &                  ( TIM_FLAG_END > TIM_OBS_BEG - TIM_UV_EPS .AND. &
     &                    TIM_FLAG_END < TIM_OBS_END + TIM_UV_EPS       ) &
     &                ) &
     &                  .AND. &
     &                ( PIM%OBS(J4)%STA_IND(1) == IND_STA .OR. &
     &                  PIM%OBS(J4)%STA_IND(2) == IND_STA      &
     &                ) &
     &              ) THEN
!
! -------------------- OK. It turned out the flagged interval is within this
! -------------------- observation. Then run over all accumulation periods to
! -------------------- learn whether it should be flagged or not
!
!!                       DO 560 J6=1,PIM%NFRG
                       DO 560 J6=PIM_MIN_FRG,PIM_MAX_FRG
                          DO 450 J5=1,PIM%OBS(J4)%NUM_EPC(J6)
                             UV_IND  = PIM%OBS(J4)%UV_IND(J5,J6)
                             TIM_UV = PIM%TIM_R8(PIM%UV_IND(UV_IND)%TIM_IND)
                             IF ( TIM_UV > TIM_FLAG_BEG - TIM_UV_EPS .AND. &
     &                            TIM_UV < TIM_FLAG_END + TIM_UV_EPS       ) THEN
!
                                  IF ( BUF(J1)(50:57) == ' reason=' ) THEN
                                       BUF(J1)(50:) = BUF(J1)(51:)
                                  END IF
                                  IF ( BUF(J1)(58:81) == 'source change in progres' ) THEN
                                       PIM%OBS(J4)%CORR_FLAG(J5,J6) = 0
                                    ELSE IF ( BUF(J1)(58:81) == 'antenna position error t' ) THEN
                                       PIM%OBS(J4)%CORR_FLAG(J5,J6) = 0
                                    ELSE IF ( BUF(J1)(58:81) == 'recorder 1 not recording' ) THEN
                                       PIM%OBS(J4)%CORR_FLAG(J5,J6) = 0
                                    ELSE IF ( BUF(J1)(58:81) == 'recorder 2 not recording' ) THEN
                                       PIM%OBS(J4)%CORR_FLAG(J5,J6) = 0
                                    ELSE IF ( BUF(J1)(58:81) == 'antenna not in point mod' ) THEN
                                       PIM%OBS(J4)%CORR_FLAG(J5,J6) = 1
                                    ELSE IF ( BUF(J1)(58:81) == 'ellipsoid position error' ) THEN
                                       PIM%OBS(J4)%CORR_FLAG(J5,J6) = 1
                                    ELSE IF ( BUF(J1)(58:81) == 'subreflector position er' ) THEN
                                       PIM%OBS(J4)%CORR_FLAG(J5,J6) = 2
                                    ELSE IF ( BUF(J1)(58:81) == '2-16 ghz synthesizer #2 ' ) THEN
                                       PIM%OBS(J4)%CORR_FLAG(J5,J6) = 1
                                    ELSE IF ( BUF(J1)(58:81) == "observing system idle' /" ) THEN
                                       PIM%OBS(J4)%CORR_FLAG(J5,J6) = 0
                                    ELSE IF ( BUF(J1)(58:81) == 'channel 1 bbc synthesize' ) THEN
                                       PIM%OBS(J4)%CORR_FLAG(J5,J6) = 2
                                    ELSE IF ( BUF(J1)(58:81) == 'channel 2 bbc synthesize' ) THEN
                                       PIM%OBS(J4)%CORR_FLAG(J5,J6) = 2
                                    ELSE IF ( BUF(J1)(58:81) == 'channel 3 bbc synthesize' ) THEN
                                       PIM%OBS(J4)%CORR_FLAG(J5,J6) = 2
                                    ELSE IF ( BUF(J1)(58:81) == 'channel 4 bbc synthesize' ) THEN
                                       PIM%OBS(J4)%CORR_FLAG(J5,J6) = 2
                                    ELSE IF ( BUF(J1)(58:81) == 'channel 5 bbc synthesize' ) THEN
                                       PIM%OBS(J4)%CORR_FLAG(J5,J6) = 2
                                    ELSE IF ( BUF(J1)(58:81) == 'channel 6 bbc synthesize' ) THEN
                                       PIM%OBS(J4)%CORR_FLAG(J5,J6) = 2
                                    ELSE IF ( BUF(J1)(58:81) == 'channel 7 bbc synthesize' ) THEN
                                       PIM%OBS(J4)%CORR_FLAG(J5,J6) = 2
                                    ELSE IF ( BUF(J1)(58:81) == 'channel 8 bbc synthesize' ) THEN
                                       PIM%OBS(J4)%CORR_FLAG(J5,J6) = 2
                                    ELSE IF ( BUF(J1)(58:81) == 'channel 9 bbc synthesize' ) THEN
                                       PIM%OBS(J4)%CORR_FLAG(J5,J6) = 2
                                    ELSE IF ( BUF(J1)(58:81) == 'channel 10 bbc synthesiz' ) THEN
                                       PIM%OBS(J4)%CORR_FLAG(J5,J6) = 2
                                    ELSE IF ( BUF(J1)(58:81) == 'channel 11 bbc synthesiz' ) THEN
                                       PIM%OBS(J4)%CORR_FLAG(J5,J6) = 2
                                    ELSE IF ( BUF(J1)(58:81) == 'channel 12 bbc synthesiz' ) THEN
                                       PIM%OBS(J4)%CORR_FLAG(J5,J6) = 2
                                    ELSE IF ( BUF(J1)(58:81) == 'channel 13 bbc synthesiz' ) THEN
                                       PIM%OBS(J4)%CORR_FLAG(J5,J6) = 2
                                    ELSE IF ( BUF(J1)(58:81) == 'channel 14 bbc synthesiz' ) THEN
                                       PIM%OBS(J4)%CORR_FLAG(J5,J6) = 2
                                    ELSE IF ( BUF(J1)(58:81) == 'channel 15 bbc synthesiz' ) THEN
                                       PIM%OBS(J4)%CORR_FLAG(J5,J6) = 2
                                    ELSE IF ( BUF(J1)(58:81) == 'channel 16 bbc synthesiz' ) THEN
                                       PIM%OBS(J4)%CORR_FLAG(J5,J6) = 2
                                    ELSE IF ( BUF(J1)(58:81) == '2-16 ghz synthesizer #1 ' ) THEN
                                       PIM%OBS(J4)%CORR_FLAG(J5,J6) = 2
                                    ELSE IF ( BUF(J1)(58:81) == '2-16 ghz synthesizer #2 ' ) THEN
                                       PIM%OBS(J4)%CORR_FLAG(J5,J6) = 2
                                    ELSE IF ( BUF(J1)(58:81) == '2-16 ghz synthesizer #3 ' ) THEN
                                       PIM%OBS(J4)%CORR_FLAG(J5,J6) = 2
                                    ELSE IF ( BUF(J1)(58:81) == '2-16 ghz synthesizer #4 ' ) THEN
                                       PIM%OBS(J4)%CORR_FLAG(J5,J6) = 2
                                    ELSE IF ( BUF(J1)(58:81) == 'recorder 2 head not in p' ) THEN
                                       PIM%OBS(J4)%CORR_FLAG(J5,J6) = 2
                                    ELSE IF ( BUF(J1)(58:79) == 'Antenna position error' ) THEN
                                       PIM%OBS(J4)%CORR_FLAG(J5,J6) = 2
                                    ELSE IF ( BUF(J1)(58:80) == 'Subreflector posn error' ) THEN
                                       PIM%OBS(J4)%CORR_FLAG(J5,J6) = 2
                                    ELSE IF ( BUF(J1)(58:70) == 'Source change' ) THEN
                                       PIM%OBS(J4)%CORR_FLAG(J5,J6) = 2
                                    ELSE IF ( BUF(J1)(58:77) == 'Ellipsoid posn error' ) THEN
                                       PIM%OBS(J4)%CORR_FLAG(J5,J6) = 2
                                    ELSE IF ( BUF(J1)(58:76) == 'Synth 2 out of lock' ) THEN
                                       PIM%OBS(J4)%CORR_FLAG(J5,J6) = 2
                                    ELSE IF ( BUF(J1)(58:78) == 'Antenna azimith error' ) THEN
                                       PIM%OBS(J4)%CORR_FLAG(J5,J6) = 2
                                    ELSE IF ( BUF(J1)(58:80) == 'Antenna elevation error' ) THEN
                                       PIM%OBS(J4)%CORR_FLAG(J5,J6) = 2
                                    ELSE
                                       CALL CLRCH ( STR )
                                       CALL INCH  ( J1, STR )
                                       CALL ERR_LOG ( 8357, IUER, 'PIMA_GET_VLBA_LOG', &
     &                                     'Error in parsing the '// &
     &                                      STR(1:I_LEN(STR))//' line '// &
     &                                      BUF(J1)(1:I_LEN(BUF(J1)))//' of '// &
     &                                     'the log file '// &
     &                                      LOG_FILE(1:I_LEN(LOG_FILE))// &
     &                                     ' Unknown reason flag: '// &
     &                                     BUF(J1)(58:81) )
                                           GOTO 810
                                  END IF
                             END IF
 450                     CONTINUE
 560                  CONTINUE
                 END IF
 440          CONTINUE
         END IF
         IF ( BUF(J1)(1:31) == '* ----- Weather information for' ) THEN
              FL_WEA = .TRUE.
              STA_NAM_2LET = BUF(J1)(33:34)
              STA_NAM = GET_IVS_NAME ( PIM, STA_NAM_2LET )
              IF ( STA_NAM == 'Unknown ' ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( J1, STR )
                   CALL ERR_LOG ( 8358, IUER, 'PIMA_GET_VLBA_LOG', &
     &                 'Error in parsing the '//STR(1:I_LEN(STR))// &
     &                 ' line '//BUF(J1)(1:I_LEN(BUF(J1)))//' of the '// &
     &                 'log file '//LOG_FILE(1:I_LEN(LOG_FILE))// &
     &                 ' -- faiulre in decoding the 2-characters '// &
     &                 'station name '//BUF(J1)(33:34) )
                   GOTO 810
              END IF
              IND_STA = LTM_DIF ( 0, PIM%NSTA, PIM%C_STA, STA_NAM )
              IF ( IND_STA .LE. 0 ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( J1, STR )
                   CALL ERR_LOG ( 8359, IUER, 'PIMA_GET_VLBA_LOG', &
     &                 'Error in parsing the '//STR(1:I_LEN(STR))// &
     &                 ' line '//BUF(J1)(1:I_LEN(BUF(J1)))//' of the '// &
     &                 'log file '//LOG_FILE(1:I_LEN(LOG_FILE))// &
     &                 ' -- station '//STA_NAM//'did not observe '// &
     &                 'in experiment '//PIM%CONF%SESS_CODE )
                   GOTO 810
              END IF
              GOTO 410
         END IF
!
         IF ( FL_WEA .AND.                &
     &        BUF(J1)(1:1) .NE. '!' .AND. &
     &        BUF(J1)(1:1) .NE. '*'       ) THEN
!
              NUM_WEA(IND_STA) = NUM_WEA(IND_STA) + 1
!!              CALL CHIN ( BUF(J1)(IND(1,4):IND(2,4)), IDOY )
              CALL CHIN ( BUF(J1)(1:3), IDOY )
              IF ( IDOY < 1 .OR. IDOY > 366 ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( J1, STR )
                   CALL ERR_LOG ( 8360, IUER, 'PIMA_GET_VLBA_LOG', &
     &                 'Error in parsing the '//STR(1:I_LEN(STR))// &
     &                 ' line '//BUF(J1)(1:I_LEN(BUF(J1)))//' of the '// &
     &                 'log file '//LOG_FILE(1:I_LEN(LOG_FILE))// &
     &                 ' -- failure in decoding BEG_DOY '// &
     &                 BUF(J1)(IND(1,4):IND(2,4)) )
                   RETURN
              ENDIF
              STR = MJDSEC_TO_DATE ( MJD_NEW_YEAR + IDOY -1, 0.0D0, -2 )
              DATE_WEA = STR(1:10)//'_'//BUF(J1)(5:12)//'.0'
              CALL DATE_TO_TIME ( DATE_WEA, MJD_WEA(NUM_WEA(IND_STA),IND_STA), &
     &                            UTC_WEA, -2 )
              IF ( PIM%TIM_SCL == PIMA__TAI ) THEN
                   TAI_WEA(NUM_WEA(IND_STA),IND_STA) = UTC_WEA
                 ELSE IF ( PIM%TIM_SCL == PIMA__UTC ) THEN
                   TAI_WEA(NUM_WEA(IND_STA),IND_STA) = UTC_WEA - PIM%UTC_MTAI
              END IF
!
              IF ( BUF(J1)(IND(2,2):IND(2,2)) == '*' ) BUF(J1)(IND(2,2):IND(2,2)) = ' '
              IF ( BUF(J1)(IND(2,3):IND(2,3)) == '*' ) BUF(J1)(IND(2,3):IND(2,3)) = ' '
              IF ( BUF(J1)(IND(2,4):IND(2,4)) == '*' ) BUF(J1)(IND(2,4):IND(2,4)) = ' '
!
              READ ( UNIT=BUF(J1)(IND(1,2):IND(2,2)), FMT='(F4.1)', IOSTAT=IER ) &
     &               TEMP(NUM_WEA(IND_STA),IND_STA)
              IF ( IER .NE. 0 ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( J1, STR )
                   CALL ERR_LOG ( 8361, IUER, 'PIMA_GET_VLBA_LOG', &
     &                 'Error in parsing the '//STR(1:I_LEN(STR))// &
     &                 ' line '//BUF(J1)(1:I_LEN(BUF(J1)))//' of the '// &
     &                 'log file '//LOG_FILE(1:I_LEN(LOG_FILE))// &
     &                 ' -- failure in decoding air temperature '// &
     &                 BUF(J1)(IND(1,2):IND(2,2)) )
                   RETURN
              END IF
!
              READ ( UNIT=BUF(J1)(IND(1,3):IND(2,3)), FMT='(F6.1)', IOSTAT=IER ) &
     &               PRES(NUM_WEA(IND_STA),IND_STA)
              IF ( IER .NE. 0 ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( J1, STR )
                   CALL ERR_LOG ( 8362, IUER, 'PIMA_GET_VLBA_LOG', &
     &                 'Error in parsing the '//STR(1:I_LEN(STR))// &
     &                 ' line '//BUF(J1)(1:I_LEN(BUF(J1)))//' of the '// &
     &                 'log file '//LOG_FILE(1:I_LEN(LOG_FILE))// &
     &                 ' -- failure in decoding air temperature '// &
     &                 BUF(J1)(IND(1,3):IND(2,3)) )
                   RETURN
              END IF
         END IF
         IF ( BUF(J1)(1:5) == 'TSYS ' ) THEN
              IF ( BUF(J1)(IND(1,2):IND(2,2)) == 'timeoff' ) THEN
                   STA_NAM_2LET = BUF(J1)(IND(1,4):IND(2,4))
                ELSE 
                   STA_NAM_2LET = BUF(J1)(IND(1,2):IND(2,2))
              END IF
              STA_NAM = GET_IVS_NAME ( PIM, STA_NAM_2LET )
              IF ( STA_NAM == 'Unknown ' ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( J1, STR )
                   CALL ERR_LOG ( 8363, IUER, 'PIMA_GET_VLBA_LOG', &
     &                 'Error in parsing the '//STR(1:I_LEN(STR))// &
     &                 ' line '//BUF(J1)(1:I_LEN(BUF(J1)))//' of the '// &
     &                 'log file '//LOG_FILE(1:I_LEN(LOG_FILE))// &
     &                 ' -- faiulre in decoding the 2-characters '// &
     &                 'VLBA station name' )
                   GOTO 810
              END IF
!
              IND_STA = LTM_DIF ( 0, PIM%NSTA, PIM%C_STA, STA_NAM )
              IF ( IND_STA .LE. 0 ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( J1, STR )
                   CALL ERR_LOG ( 8364, IUER, 'PIMA_GET_VLBA_LOG', &
     &                 'Error in parsing the '//STR(1:I_LEN(STR))// &
     &                 ' line '//BUF(J1)(1:I_LEN(BUF(J1)))//' of the '// &
     &                 'log file '//LOG_FILE(1:I_LEN(LOG_FILE))// &
     &                 ' -- station '//STA_NAM//' did not observe '// &
     &                 'in experiment '//PIM%CONF%SESS_CODE )
                   GOTO 810
              END IF
              FL_TSYS = .TRUE.
              IND2_FREQ = 0
              IND2_FRG  = 0
              IND2_POL  = 0
              IF ( .NOT. FL_GEN_VLBA ) THEN   ! non genuine VLBA log file
                   IND_FRG = 0
                   IND_FRQ = 0
              END IF
              GOTO 410
         END IF
         IF ( FL_TSYS                .AND. &
     &        BUF(J1)(1:1) .EQ. '!'  .AND. &
     &        LIND .GE. 10                 ) THEN
!
              IF ( FL_GEN_VLBA ) THEN ! This logic for genuine VLBA log type
                   IND_FRG = 0
                   IND_FRQ = 0
              END IF
              CALL CHIN ( BUF(J1)(IND(1,2):IND(2,2)), IVAL )
              IF ( IVAL .GE. 1  .AND.  IVAL .LE. PIM__MFRQ ) THEN
                   IF ( .NOT. FL_GEN_VLBA ) THEN  ! non genuine VLBA log file
                        IND_FRG = 0
                        IND_FRQ = 0
                   END IF
                   STR = BUF(J1)(IND(1,10):IND(2,10))
                   IP = INDEX ( STR, 'MHz' )
                   IF ( IP > 0 ) CALL CLRCH ( STR(IP:) )
                   READ ( UNIT=STR, FMT='(F20.0)', IOSTAT=IER ) FRQ_VAL
!!!                   FRQ_VAL= DABS(FRQ_VAL)
                   IF ( IER .NE. 0 ) THEN
                        CALL CLRCH ( STR )
                        CALL INCH  ( J1, STR )
                        CALL ERR_LOG ( 8365, IUER, 'PIMA_GET_VLBA_LOG', &
     &                      'Error in parsing the '//STR(1:I_LEN(STR))// &
     &                      ' line '//BUF(J1)(1:I_LEN(BUF(J1)))//' of the '// &
     &                      'log file '//LOG_FILE(1:I_LEN(LOG_FILE))// &
     &                      ' -- faiulre in decoding the 10th word' )
                        GOTO 810
                   END IF
                   FRQ_VAL = 1.D6*FRQ_VAL
                   IF ( PIMAVAR_SUB_BAND == 'U' ) BUF(J1)(IND(1,7):IND(2,7)) = "U" ! I am in a loss here. 2013.09.25_21:47:28
                   DO 570 J7=PIM_MIN_FRG,PIM_MAX_FRG
                      DO 580 J8=1,PIM%NFRQ
                         IF ( BUF(J1)(IND(1,7):IND(2,7)) == "L" ) THEN
                            IF ( PIM%FRQ(J8,J7)%SIDE_BAND == -1 .AND. &
     &                           ( FRQ_VAL .GE. PIM%FRQ(J8,J7)%FREQ - FRQ_TOL .AND. &
     &                             FRQ_VAL .LE. PIM%FRQ(J8,J7)%FREQ + &
     &                                              PIM%FRQ(J8,J7)%BAND_WIDTH + FRQ_TOL ) ) THEN
                                 IND_FRG = J7
                                 IND_FRQ = J8
                                 IND2_FREQ(IVAL) = IND_FRQ
                                 IND2_FRG(IVAL)  = IND_FRG
                                 IF ( PIM%CONF%DEBUG_LEVEL .GE. 6 ) THEN
                                      WRITE ( 6, * ) 'T1: J1= ', J1, ' IND2_FREQ(', INT2(IVAL), ') = ', IND_FRQ, &
     &                                               ' IND2_FRG( ', INT2(IVAL), ') = ', IND_FRG
                                 END IF
                            END IF
                            IF ( FRQ_VAL < 0.0 ) THEN
                                 IF ( -FRQ_VAL .GE. PIM%FRQ(J8,J7)%FREQ - FRQ_TOL .AND. &
     &                                -FRQ_VAL .LE. PIM%FRQ(J8,J7)%FREQ + PIM%FRQ(J8,J7)%BAND_WIDTH + FRQ_TOL ) THEN
                                      IND_FRG = J7
                                      IND_FRQ = J8
                                      IND2_FREQ(IVAL) = IND_FRQ
                                      IND2_FRG(IVAL)  = IND_FRG
                                      IF ( PIM%CONF%DEBUG_LEVEL .GE. 6 ) THEN
                                           WRITE ( 6, * ) 'T2: J1= ', J1, ' IND2_FREQ(', INT2(IVAL), ') = ', IND_FRQ, &
     &                                                    ' IND2_FRG( ', INT2(IVAL), ') = ', IND_FRG
                                      END IF
                                 END IF 
                            END IF 
                         ELSE IF ( BUF(J1)(IND(1,7):IND(2,7)) == "U" ) THEN
                            IF ( PIM%FRQ(J8,J7)%SIDE_BAND ==  1 .AND. &
     &                           ( FRQ_VAL .GE. PIM%FRQ(J8,J7)%FREQ - FRQ_TOL .AND. &
     &                             FRQ_VAL .LE. PIM%FRQ(J8,J7)%FREQ + &
     &                                        PIM%FRQ(J8,J7)%BAND_WIDTH + FRQ_TOL ) ) THEN
                                 IND_FRG = J7
                                 IND_FRQ = J8
                                 IND2_FREQ(IVAL) = IND_FRQ
                                 IND2_FRG(IVAL)  = IND_FRG
                                 IF ( PIM%CONF%DEBUG_LEVEL .GE. 6 ) THEN
                                      WRITE ( 6, * ) 'T3: J1= ', J1, ' IND2_FREQ(', INT2(IVAL), ') = ', IND_FRQ, &
     &                                               ' IND2_FRG( ', INT2(IVAL), ') = ', IND_FRG
                                 END IF
                            END IF
                         ELSE
                            CALL CLRCH ( STR )
                            CALL INCH  ( J1, STR )
                            CALL ERR_LOG ( 8366, IUER, 'PIMA_GET_VLBA_LOG', &
     &                         'Error in parsing the '//STR(1:I_LEN(STR))// &
     &                         ' line '//BUF(J1)(1:I_LEN(BUF(J1)))//' of the '// &
     &                         'log file '//LOG_FILE(1:I_LEN(LOG_FILE))// &
     &                         ' -- unsupported sideband code '// &
     &                         BUF(J1)(IND(1,7):IND(2,7)) )
                            RETURN
                         END IF
 580                  CONTINUE
 570               CONTINUE
                   IF ( IND_FRG == 0 ) THEN
                        IF ( PIM%CONF%CHECK_SEVERITY .GE. 2 ) THEN
                             CALL CLRCH ( STR )
                             CALL INCH  ( J1, STR )
                             CALL ERR_LOG ( 8367, IUER, 'PIMA_GET_VLBA_LOG', &
     &                           'Error in parsing the '//STR(1:I_LEN(STR))// &
     &                           ' line '//BUF(J1)(1:I_LEN(BUF(J1)))//' of the '// &
     &                           'log file '//LOG_FILE(1:I_LEN(LOG_FILE))// &
     &                           ' -- frequency '//BUF(J1)(IND(1,10):IND(2,10))// &
     &                           ' MHz was not observed in experiment '// &
     &                           PIM%CONF%SESS_CODE )
                             GOTO 810
                        END IF
!
                        IF ( PIM%CONF%DEBUG_LEVEL .GE. 1 ) THEN
                             CALL CLRCH ( STR )
                             CALL INCH  ( J1, STR )
                             WRITE ( 6, '(A)' ) 'PIMA_GET_VLBA_LOG: Warning during '// &
     &                               'parsing the '//STR(1:I_LEN(STR))// &
     &                               ' line '//BUF(J1)(1:I_LEN(BUF(J1)))//' of the '// &
     &                               'log file '//LOG_FILE(1:I_LEN(LOG_FILE))// &
     &                               ' -- frequency '//BUF(J1)(IND(1,10):IND(2,10))// &
     &                               ' MHz was not observed in experiment '// &
     &                               PIM%CONF%SESS_CODE
                             GOTO 410
                        END IF
                   END IF
!
                   IF ( BUF(J1)(IND(1,5):IND(2,5)) == "RCP" ) THEN
                        IND2_POL(IVAL) = 1
                      ELSE IF ( BUF(J1)(IND(1,5):IND(2,5)) == "LCP"  .AND. &
     &                          PIM%NPOL == 1 ) THEN
                        IND2_POL(IVAL) = 1
                      ELSE IF ( BUF(J1)(IND(1,5):IND(2,5)) == "LCP"  .AND. &
     &                          PIM%NPOL > 1 ) THEN
                        IND2_POL(IVAL) = 2
                      ELSE
                        CALL CLRCH ( STR )
                        CALL INCH  ( J1, STR )
                        CALL ERR_LOG ( 8368, IUER, 'PIMA_GET_VLBA_LOG', &
     &                      'Error in parsing the '//STR(1:I_LEN(STR))// &
     &                      ' line '//BUF(J1)(1:I_LEN(BUF(J1)))//' of the '// &
     &                      'log file '//LOG_FILE(1:I_LEN(LOG_FILE))// &
     &                      ' -- unsupported polarization code '// &
     &                      BUF(J1)(IND(1,5):IND(2,5)) )
                        RETURN
                   END IF
              END IF
         END IF
         IF ( FL_TSYS                .AND. &
     &        BUF(J1)(1:1) .EQ. '!'  .AND. &
     &        LIND .GE. 2                  ) THEN
!
              IF ( BUF(J1)(IND(1,2):IND(2,2)) == STA_NAM_2LET ) THEN
                   STR = BUF(J1)(IND(1,4):IND(2,4))
                   IP = INDEX ( STR, '/' )
                   IF ( IP > 0 ) CALL CLRCH ( STR(IP:) )
                   IND_SOU = LTM_DIF ( 0, PIM%NSOU, PIM%C_SOU, STR )
                   IF ( IND_SOU .LE. 0 ) THEN
!
! --------------------- If the source was not found, let us search it
! --------------------- in the lists of original source names
!
                        DO 460 J6=1,PIM%L_FIL
                           I_SOU = LTM_DIF ( 0, PIM%FILE(J6)%N_SOU, &
     &                                       PIM%FILE(J6)%SOU_NAME_ORIG, STR )
                           IF ( I_SOU > 0 ) THEN
                                IND_SOU = PIM%REF_SOU(I_SOU,J6)
                                GOTO  860
                           END IF
 460                    CONTINUE
 860                    CONTINUE
                        IF ( IND_SOU .LE. 0 ) THEN
                             IF ( PIM%CONF%CHECK_SEVERITY .LE. 1 ) GOTO 410
                             CALL CLRCH ( STR )
                             CALL INCH  ( J1, STR )
                             CALL ERR_LOG ( 8369, IUER, 'PIMA_GET_VLBA_LOG', 'Error'// &
     &                           ' in parsing the '//STR(1:I_LEN(STR))//'-th line'// &
     &                           ' of the log file '//LOG_FILE(1:I_LEN(LOG_FILE))// &
     &                           ' source '//BUF(J1)(IND(1,4):IND(2,4))//' was'// &
     &                           ' not observed in exeperiment '//PIM%CONF%SESS_CODE )
                             GOTO 810
                        END IF
                   END IF
              END IF
         END IF
!
         IF ( FL_TSYS .AND. BUF(J1)(1:1) .NE. '!' .AND. IND_FRG > 0 ) THEN
              IF ( BUF(J1)(1:1) == '/' ) THEN
                   FL_TSYS = .FALSE.
                   GOTO 410
              END IF
!
              IF ( IND_FRG .LE. 0 ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( J1, STR )
                   CALL ERR_LOG ( 8370, IUER, 'PIMA_GET_VLBA_LOG', 'Error'// &
     &                 ' in parsing the '//STR(1:I_LEN(STR))//'-th line'// &
     &                 ' of the log file '//LOG_FILE(1:I_LEN(LOG_FILE))// &
     &                 ' frequency group was not defined in the preceeding '// &
     &                 ' lines' )
                   GOTO 810
              END IF
!
              CALL CHIN ( BUF(J1)(IND(1,1):IND(2,1)), IDOY )
              IF ( IDOY < 1 .OR. IDOY > 366 ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( J1, STR )
                   CALL ERR_LOG ( 8371, IUER, 'PIMA_GET_VLBA_LOG', &
     &                 'Error in parsing the '//STR(1:I_LEN(STR))// &
     &                 ' line '//BUF(J1)(1:I_LEN(BUF(J1)))//' of the '// &
     &                 'log file '//LOG_FILE(1:I_LEN(LOG_FILE))// &
     &                 ' -- failure in decoding DOY '// &
     &                 BUF(J1)(IND(1,1):IND(2,1)) )
                   RETURN
              ENDIF
!
              CALL ERR_PASS ( IUER, IER )
!@              CALL HR_TAT ( BUF(J1)(IND(1,2):IND(2,2)), VAL, IER )
!@              CALL RS_TAT ( VAL, UTC_TSYS )
              READ ( UNIT=BUF(J1)(IND(1,2):IND(1,2)+1), FMT='(I2)', IOSTAT=IER ) &
     &               IHR
              IF ( IER .NE. 0 ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( J1, STR )
                   CALL ERR_LOG ( 8372, IUER, 'PIMA_GET_VLBA_LOG', &
     &                 'Error in parsing the '//STR(1:I_LEN(STR))// &
     &                 ' line '//BUF(J1)(1:I_LEN(BUF(J1)))//' of the '// &
     &                 'log file '//LOG_FILE(1:I_LEN(LOG_FILE))// &
     &                 ' -- failure in decoding hours '// &
     &                 BUF(J1)(IND(1,2):IND(2,2)) )
                   RETURN
              END IF
!
              READ ( UNIT=BUF(J1)(IND(1,2)+3:IND(1,2)+8), FMT='(F6.3)', &
     &               IOSTAT=IER ) MN_VAL
              IF ( IER .NE. 0 ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( J1, STR )
                   CALL ERR_LOG ( 8373, IUER, 'PIMA_GET_VLBA_LOG', &
     &                 'Error in parsing the '//STR(1:I_LEN(STR))// &
     &                 ' -th line '//BUF(J1)(1:I_LEN(BUF(J1)))//' of the '// &
     &                 'log file '//LOG_FILE(1:I_LEN(LOG_FILE))// &
     &                 ' -- failure in decoding minutes '// &
     &                 BUF(J1)(IND(1,2):IND(2,2)) )
                   RETURN
              END IF
              MJD_TSYS = MJD_NEW_YEAR + IDOY -1
              UTC_TSYS = IHR*3600.0D0 + MN_VAL*60.0D0
              IF ( PIM%TIM_SCL == PIMA__TAI ) THEN
                   TIM_TSYS = (MJD_TSYS - PIM%MJD_0)*86400.0D0 + &
     &                        (UTC_TSYS - PIM%TAI_0)
                 ELSE IF ( PIM%TIM_SCL == PIMA__UTC ) THEN
                   TIM_TSYS = (MJD_TSYS - PIM%MJD_0)*86400.0D0  + &
     &                        (UTC_TSYS - PIM%TAI_0) - PIM%UTC_MTAI
              END IF
!
              NUM_TSYS(IND_STA,IND_FRG) = NUM_TSYS(IND_STA,IND_FRG) + 1
              TIM_TSYS_ARR(NUM_TSYS(IND_STA,IND_FRG),IND_FRG,IND_STA) = TIM_TSYS
              IND_SOU_TSYS(NUM_TSYS(IND_STA,IND_FRG),IND_FRG,IND_STA) = IND_SOU
!!
!!              num_tsys(ind_sta,1:pim%nfrg) = num_tsys(ind_sta,1:pim%nfrg) + 1
!!              tim_tsys_arr(num_tsys(ind_sta,ind_frg),1:pim%nfrg,ind_sta) = tim_tsys
!!              ind_sou_tsys(num_tsys(ind_sta,ind_frg),1:pim%nfrg,ind_sta) = ind_sou
!!
              IF ( LIND < PIM%NFRQ+2 ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( J1, STR )
                   CALL CLRCH ( STR1 )
                   CALL INCH  ( 2+PIM%NFRQ, STR1 )
                   CALL ERR_LOG ( 8374, IUER, 'PIMA_GET_VLBA_LOG', &
     &                 'Error in parsing the '//STR(1:I_LEN(STR))// &
     &                 ' line '//BUF(J1)(1:I_LEN(BUF(J1)))//' of the '// &
     &                 'log file '//LOG_FILE(1:I_LEN(LOG_FILE))// &
     &                 ' -- too few words, less than '//STR1(1:I_LEN(STR1))// &
     &                 ' in line '//BUF(J1)(IND(1,2):IND(2,2)) )
                   RETURN
              END IF
!
              IND_POL = 0
              IND_FRQ = 1
              DO 470 J7=1,PIM%NFRQ*PIM%NPOL
                 IND_POL = IND2_POL(J7)
                 IND_FRQ = IND2_FREQ(J7)
                 READ ( UNIT=BUF(J1)(IND(1,2+J7):IND(2,2+J7)), FMT='(F5.1)', &
     &                  IOSTAT=IER ) TSYS_ARR(IND_FRQ,NUM_TSYS(IND_STA,IND_FRG),IND_FRG,IND_POL,IND_STA)
                 IF ( PIM%NFRG > 1 ) THEN
                      TSYS_ARR(IND_FRQ,NUM_TSYS(IND_STA,IND_FRG),1:PIM%NFRG,IND_POL,IND_STA) = &
     &                         TSYS_ARR(IND_FRQ,NUM_TSYS(IND_STA,IND_FRG),IND_FRG,IND_POL,IND_STA)
                 END IF
                 IF ( IER .NE. 0 ) THEN
                      CALL CLRCH ( STR )
                      CALL INCH  ( J1, STR )
                      CALL CLRCH ( STR1 )
                      CALL INCH  ( 2+J7, STR1 )
                      CALL ERR_LOG ( 8375, IUER, 'PIMA_GET_VLBA_LOG', &
     &                    'Error in reading the '//STR(1:I_LEN(STR))// &
     &                    ' line '//BUF(J1)(1:I_LEN(BUF(J1)))//' of the '// &
     &                    'log file '//LOG_FILE(1:I_LEN(LOG_FILE))// &
     &                    ' -- the '//STR1(1:I_LEN(STR1))// &
     &                    ' value is wrong in line '// &
     &                    BUF(J1)(IND(1,2):IND(2,2)) )
                      RETURN
                 END IF
 470          CONTINUE
              STR = MJDSEC_TO_DATE ( MJD_TSYS, UTC_TSYS - PIM%UTC_MTAI, -2 )
              IF ( PIM%CONF%DEBUG_LEVEL .GE. 4 ) THEN
                   IF ( IND_SOU == 0 ) THEN
                        WRITE ( 6, 206 ) J1, PIM%C_STA(IND_STA), &
     &                                   STR(1:21)
                      ELSE
                        WRITE ( 6, 208 ) J1, PIM%C_STA(IND_STA), &
     &                                   PIM%C_SOU(IND_SOU), STR(1:21)
                   END IF
 206               FORMAT ( 'PIMA_GET_VLBA_LOG  Line: ', I7, ' Sta: ', &
     &                       A, ' Sou: UNKNOWN   Date: ', A )
 208               FORMAT ( 'PIMA_GET_VLBA_LOG  Line: ', I7, ' Sta: ', &
     &                       A, ' Sou: ', A, ' Date: ', A )
              END IF
         END IF
 410  CONTINUE
 820  CONTINUE
!
      IF ( PIM%CONF%DEBUG_LEVEL .GE. 2 ) THEN
           WRITE ( 6, * ) 'PIMA_GET_VLBA_LOG: LPT= ', LPT, ' NO_TONES= ', NO_TONES
           IF ( IND_FRQ > 0 ) THEN
                WRITE ( 6, * ) 'PIMA_GET_VLBA_LOG: PCAL    NPC_STA  = ', NPC_STA(1:PIM%NSTA,IND_FRG)
                WRITE ( 6, * ) 'PIMA_GET_VLBA_LOG: TSYS    NUM_TSYS = ', NUM_TSYS(1:PIM%NSTA,IND_FRG)
              ELSE
                WRITE ( 6, * ) 'PIMA_GET_VLBA_LOG: PCAL    NPC_STA  = ', NPC_STA(1:PIM%NSTA,1)
                WRITE ( 6, * ) 'PIMA_GET_VLBA_LOG: TSYS    NUM_TSYS = ', NUM_TSYS(1:PIM%NSTA,1)
           END IF
           WRITE ( 6, * ) 'PIMA_GET_VLBA_LOG: WEATHER NUM_WEA  = ', NUM_WEA(1:PIM%NSTA)
      END IF
!
      DO 4180 J18=1,PIM%NSTA
         IF ( FL_PCAL_UPDATE ) THEN
              DO 4190 J19=PIM_MIN_FRG,PIM_MAX_FRG
                 IF ( NPC_STA(J18,J19) > 0 ) THEN
                      IF ( LPT == 0 ) NO_TONES = 2 ! This is a hack! For cases like br145
                      IF ( PIM%CONF%DEBUG_LEVEL .GE. 2 ) THEN
                           WRITE ( 6, * ) 'PIMA_GET_VLBA_LOG: Sta: '// &
     &                                     PIM%STA(J18)%IVS_NAME, &
     &                                    ' NO_TONES = ', NO_TONES
                      END IF
                      IF ( PIM%NPCT == 0 ) THEN
                           PIM%NPCT = NO_TONES
                      END IF
                      IF ( PIM%STA(J18)%PCAL(J19)%PCAL_AVAIL ) THEN
                           DEALLOCATE ( PIM%STA(J18)%PCAL(J19)%FREQ )
                           DEALLOCATE ( PIM%STA(J18)%PCAL(J19)%AMPL )
                           DEALLOCATE ( PIM%STA(J18)%PCAL(J19)%PHAS )
                           DEALLOCATE ( PIM%STA(J18)%PCAL(J19)%RATE )
                           DEALLOCATE ( PIM%STA(J18)%PCAL(J19)%TIME_MID_R8  )
                           DEALLOCATE ( PIM%STA(J18)%PCAL(J19)%TIME_SPAN_R4 )
                           DEALLOCATE ( PIM%STA(J18)%PCAL(J19)%SOU_IND )
                      END IF
                      PIM%STA(J18)%PCAL(J19)%NPOI = NPC_STA(J18,J19)
                      PIM%STA(J18)%PCAL(J19)%NPOL = PIM%NPOL
                      PIM%STA(J18)%PCAL(J19)%NO_TONES   = NO_TONES
                      PIM%STA(J18)%PCAL(J19)%PCAL_AVAIL = .TRUE.
                      PIM%STA(J18)%PCAL(J19)%PCAL_USE   = .TRUE.
                      ALLOCATE ( PIM%STA(J18)%PCAL(J19)%AMPL(NO_TONES,PIM%NFRQ, &
     &                           PIM%STA(J18)%PCAL(J19)%NPOI,PIM%STA(J18)%PCAL(J19)%NPOL) )
                      ALLOCATE ( PIM%STA(J18)%PCAL(J19)%PHAS(NO_TONES,PIM%NFRQ, &
     &                           PIM%STA(J18)%PCAL(J19)%NPOI,PIM%STA(J18)%PCAL(J19)%NPOL) )
                      ALLOCATE ( PIM%STA(J18)%PCAL(J19)%RATE(NO_TONES,PIM%NFRQ, &
     &                           PIM%STA(J18)%PCAL(J19)%NPOI,PIM%STA(J18)%PCAL(J19)%NPOL)   )
                      ALLOCATE ( PIM%STA(J18)%PCAL(J19)%FREQ(NO_TONES,PIM%NFRQ, &
     &                                                  PIM%STA(J18)%PCAL(J19)%NPOI)   )
                      ALLOCATE ( PIM%STA(J18)%PCAL(J19)%TIME_MID_R8(PIM%STA(J18)%PCAL(J19)%NPOI) )
                      ALLOCATE ( PIM%STA(J18)%PCAL(J19)%TIME_SPAN_R4(PIM%STA(J18)%PCAL(J19)%NPOI) )
                      ALLOCATE ( PIM%STA(J18)%PCAL(J19)%SOU_IND(PIM%STA(J18)%PCAL(J19)%NPOI) )
!
                      IF ( J19 == 1 ) THEN
                           IF ( PIM%STA(J18)%CABLE%CAB_AVAIL ) THEN
                                DEALLOCATE ( PIM%STA(J18)%CABLE%TIM_CAB )
                                DEALLOCATE ( PIM%STA(J18)%CABLE%CAB_DEL )
                           END IF
                           PIM%STA(J18)%CABLE%CAB_AVAIL  = .TRUE.
                           PIM%STA(J18)%CABLE%CABLE_SIGN = 1
                           PIM%STA(J18)%CABLE%MEAN_CABLE = CABLE_MEAN(J18,J19)/NPC_STA(J18,J19)
                           PIM%STA(J18)%CABLE%NPOI = PIM%STA(J18)%PCAL(J19)%NPOI
                           ALLOCATE ( PIM%STA(J18)%CABLE%TIM_CAB(PIM%STA(J18)%CABLE%NPOI)  )
                           ALLOCATE ( PIM%STA(J18)%CABLE%CAB_DEL(PIM%STA(J18)%CABLE%NPOI)  )
                      END IF
                      DO 4200 J20=1,PIM%STA(J18)%PCAL(J19)%NPOI
                         PIM%STA(J18)%PCAL(J19)%SOU_IND(J20) = IND_SOU_PCAL(J20,J19,J18)
                         PIM%STA(J18)%PCAL(J19)%TIME_MID_R8(J20) = &
     &                             (MJD_TAG(J20,J19,J18) - PIM%MJD_0)*86400.0D0 + &
     &                             (TAI_TAG(J20,J19,J18) - PIM%TAI_0)
                         PIM%STA(J18)%PCAL(J19)%TIME_SPAN_R4(J20) = PIM__SPAN_DEF
!
                         IF ( IND_FRG_PCAL(J20,J18) == 0 ) IND_FRG_PCAL(J20,J18) = 1
                         IF ( IND_FRG_PCAL(J20,J18) >    PIM%NFRG  .OR. &
     &                        IND_FRG_PCAL(J20,J18) .LE. 0              ) THEN
!
                              CALL CLRCH ( STR  )
                              CALL CLRCH ( STR1 )
                              CALL INCH  ( J20, STR  )
                              CALL INCH  ( IND_FRG_PCAL(J20,J18), STR1 )
                              CALL ERR_LOG ( 8376, IUER, 'PIMA_GET_VLBA_LOG', &
     &                            'Trap of internal control for processing '// &
     &                            'point '//STR(1:I_LEN(STR))//' for station '// &
     &                            PIM%C_STA(J18)//' index '//STR1(1:I_LEN(STR1))// &
     &                            ' is out of range' )
                              RETURN
                         END IF
                         DO 4210 J21=1,PIM%NFRQ
                            DO 4220 J22=1,NO_TONES
                               PIM%STA(J18)%PCAL(J19)%FREQ(J22,J21,J20) = &
     &                             FREQ_TONE(J22,J21,J19,J18)
                               DO 4230 J23=1,PIM%STA(J18)%PCAL(J19)%NPOL
                                  PIM%STA(J18)%PCAL(J19)%RATE(J22,J21,J20,J23) = 0.0D0
                                  PIM%STA(J18)%PCAL(J19)%AMPL(J22,J21,J20,J23) = PCAL_AMP(J22,J21,J23,J20,J19,J18)
                                  IF ( PIM%FRQ(J21,IND_FRG_PCAL(J20,J18))%SIDE_BAND == 1 ) THEN
                                       PIM%STA(J18)%PCAL(J19)%PHAS(J22,J21,J20,J23) =  PCAL_PHS(J22,J21,J23,J20,J19,J18)
                                     ELSE IF ( PIM%FRQ(J21,IND_FRG_PCAL(J20,J18))%SIDE_BAND == -1 ) THEN
                                       PIM%STA(J18)%PCAL(J19)%PHAS(J22,J21,J20,J23) = -PCAL_PHS(J22,J21,J23,J20,J19,J18)
                                  END IF
 4230                          CONTINUE
 4220                       CONTINUE
 4210                    CONTINUE
                         IF ( PIM%CONF%DEBUG_LEVEL .GE. 4 ) THEN
                              STR = MJDSEC_TO_DATE ( PIM%MJD_0, PIM%TAI_0 + &
     &                                    PIM%STA(J18)%PCAL(J19)%TIME_MID_R8(J20) &
     &                                  - PIM%STA(J18)%PCAL(J19)%TIME_SPAN_R4(J20)/2.0D0, IER )
                              STR(31:) = MJDSEC_TO_DATE ( PIM%MJD_0, PIM%TAI_0 + &
     &                                    PIM%STA(J18)%PCAL(J19)%TIME_MID_R8(J20) &
     &                                  + PIM%STA(J18)%PCAL(J19)%TIME_SPAN_R4(J20)/2.0D0, IER )
                              IF ( PIM%STA(J18)%PCAL(J19)%SOU_IND(J20) > 0 ) THEN
                                   STR(61:68) = PIM%C_SOU(PIM%STA(J18)%PCAL(J19)%SOU_IND(J20))
                                 ELSE
                                   STR(61:68) = 'Unknown '
                              END IF
                              WRITE ( 6, 210 ) PIM%STA(J18)%IVS_NAME, J20, STR(1:21), &
     &                                         STR(31:51), STR(61:68)
 210                          FORMAT ( 'Sta: ', A, ' I_cal: ', I4, ' [ ', A, ' , ', A, &
     &                              ' ]  Sou: ',A )
                         END IF
                         IF ( J19 == 1 ) THEN
                              PIM%STA(J18)%CABLE%TIM_CAB(J20) = PIM%STA(J18)%PCAL(J19)%TIME_MID_R8(J20)
                              PIM%STA(J18)%CABLE%CAB_DEL(J20) = CABLE_CAL(J20,J19,J18) - &
     &                                                  PIM%STA(J18)%CABLE%MEAN_CABLE
                         END IF
 4200                 CONTINUE
                 END IF
 4190         CONTINUE
         END IF ! FL_PCAL_UPDATE
!
         IF ( NUM_WEA(J18) > 0 ) THEN
              IF ( PIM%CONF%DEBUG_LEVEL .GE. 4 ) THEN
                   WRITE ( 6, 212 ) PIM%STA(J18)%IVS_NAME, NUM_WEA(J18)
 212               FORMAT ( ' PIMA_GET_VLBA_LOG WEATHER Sta: ' , A, ' Num_pt: ', I4 )
              END IF
              IF ( PIM%STA(J18)%WEATHER%AVAIL ) THEN
                   DEALLOCATE ( PIM%STA(J18)%WEATHER%TIME_BEG )
                   DEALLOCATE ( PIM%STA(J18)%WEATHER%TIME_END )
                   DEALLOCATE ( PIM%STA(J18)%WEATHER%PRES )
                   DEALLOCATE ( PIM%STA(J18)%WEATHER%TEMP )
                   DEALLOCATE ( PIM%STA(J18)%WEATHER%HUMID )
              END IF
              PIM%STA(J18)%WEATHER%AVAIL = .TRUE.
              PIM%STA(J18)%WEATHER%NPOI = NUM_WEA(J18)
              ALLOCATE ( PIM%STA(J18)%WEATHER%TIME_BEG(PIM%STA(J18)%WEATHER%NPOI) )
              ALLOCATE ( PIM%STA(J18)%WEATHER%TIME_END(PIM%STA(J18)%WEATHER%NPOI) )
              ALLOCATE ( PIM%STA(J18)%WEATHER%PRES(PIM%STA(J18)%WEATHER%NPOI) )
              ALLOCATE ( PIM%STA(J18)%WEATHER%TEMP(PIM%STA(J18)%WEATHER%NPOI) )
              ALLOCATE ( PIM%STA(J18)%WEATHER%HUMID(PIM%STA(J18)%WEATHER%NPOI) )
              CALL NOUT_R8 ( PIM%STA(J18)%WEATHER%NPOI, PIM%STA(J18)%WEATHER%HUMID )
              DO 4240 J24=1,PIM%STA(J18)%WEATHER%NPOI
                 TIM_CURR = (MJD_WEA(J24,J18) - PIM%MJD_0)*86400.0D0 + &
     &                      (TAI_WEA(J24,J18) - PIM%TAI_0)
                 IF ( J24 == 1 ) THEN
!
! ------------------- First point
!
                      TIM_NEXT = (MJD_WEA(J24+1,J18) - PIM%MJD_0)*86400.0D0 + &
     &                           (TAI_WEA(J24+1,J18) - PIM%TAI_0)
                      PIM%STA(J18)%WEATHER%TIME_BEG(J24) = TIM_CURR - &
     &                                             (TIM_NEXT - TIM_CURR)/2.0D0
                      PIM%STA(J18)%WEATHER%TIME_END(J24) = TIM_CURR + &
     &                                             (TIM_NEXT - TIM_CURR)/2.0D0
                    ELSE IF ( J24 == PIM%STA(J18)%WEATHER%NPOI ) THEN
!
! ------------------- Last point
!
                      TIM_PREV = (MJD_WEA(J24-1,J18) - PIM%MJD_0)*86400.0D0 + &
     &                           (TAI_WEA(J24-1,J18) - PIM%TAI_0)
                      PIM%STA(J18)%WEATHER%TIME_BEG(J24) = TIM_CURR - &
     &                                             (TIM_CURR - TIM_PREV)/2.0D0
                      PIM%STA(J18)%WEATHER%TIME_END(J24) = TIM_CURR + &
     &                                             (TIM_CURR - TIM_PREV)/2.0D0
                    ELSE
                      TIM_PREV = (MJD_WEA(J24-1,J18) - PIM%MJD_0)*86400.0D0 + &
     &                           (TAI_WEA(J24-1,J18) - PIM%TAI_0)
                      TIM_NEXT = (MJD_WEA(J24+1,J18) - PIM%MJD_0)*86400.0D0 + &
     &                           (TAI_WEA(J24+1,J18) - PIM%TAI_0)
                      PIM%STA(J18)%WEATHER%TIME_BEG(J24) = TIM_CURR - &
     &                                             (TIM_CURR - TIM_PREV)/2.0D0
                      PIM%STA(J18)%WEATHER%TIME_END(J24) = TIM_CURR + &
     &                                             (TIM_NEXT - TIM_CURR)/2.0D0
                 END IF
                 PIM%STA(J18)%WEATHER%PRES(J24) = PRES(J24,J18)*100.0D0
                 PIM%STA(J18)%WEATHER%TEMP(J24) = TEMP(J24,J18) + 273.15D0
!
                 IF ( PIM%CONF%DEBUG_LEVEL .GE. 6 ) THEN
                      WRITE ( 6, 214 ) PIM%STA(J18)%IVS_NAME, J24, &
     &                        PIM%STA(J18)%WEATHER%TIME_BEG(J24), &
     &                        PIM%STA(J18)%WEATHER%TIME_END(J24), &
     &                        PIM%STA(J18)%WEATHER%PRES(J24), &
     &                        PIM%STA(J18)%WEATHER%TEMP(J24)
 214                  FORMAT ( ' WEATHER  ',A, ' Ind_pt: ', I4, &
     &                         ' Tim_beg: ', F10.2, ' Tim_end: ', F10.2, &
     &                         ' Pres: ', F10.2, ' Temp: ', F8.2 )
                 END IF
 4240         CONTINUE
         END IF
!
! ------ Now time came for Tsys
!
         DO 4250 J25=PIM_MIN_FRG,PIM_MAX_FRG
            IF ( NUM_TSYS(J18,J25) > 0 ) THEN
!
! -------------- Allocate dynamic memory for TSYS
!
                 IF ( ASSOCIATED ( PIM%STA(J18)%TSYS(J25)%TSYS ) ) THEN
!
! ------------------- Deallocated memory for Tsys if it has been previously allocated
!
                      DEALLOCATE ( PIM%STA(J18)%TSYS(J25)%TSYS )
                      DEALLOCATE ( PIM%STA(J18)%TSYS(J25)%TIME_MID_R8  )
                      DEALLOCATE ( PIM%STA(J18)%TSYS(J25)%TIME_SPAN_R4 )
                      DEALLOCATE ( PIM%STA(J18)%TSYS(J25)%SOU_IND )
                      DEALLOCATE ( PIM%STA(J18)%TSYS(J25)%AZ_R4   )
                      DEALLOCATE ( PIM%STA(J18)%TSYS(J25)%ELEV_R4 )
                 END IF
!
                 PIM%STA(J18)%TSYS(J25)%NPOI  = NUM_TSYS(J18,J25)
                 PIM%STA(J18)%TSYS(J25)%NPOL  = PIM%NPOL
                 PIM%STA(J18)%TSYS(J25)%AVAIL = .TRUE.
!
                 ALLOCATE ( PIM%STA(J18)%TSYS(J25)%TSYS(PIM%NFRQ,PIM%STA(J18)%TSYS(J25)%NPOI,PIM%STA(J18)%TSYS(J25)%NPOL) )
                 ALLOCATE ( PIM%STA(J18)%TSYS(J25)%TIME_MID_R8 (PIM%STA(J18)%TSYS(J25)%NPOI) )
                 ALLOCATE ( PIM%STA(J18)%TSYS(J25)%TIME_SPAN_R4(PIM%STA(J18)%TSYS(J25)%NPOI) )
                 ALLOCATE ( PIM%STA(J18)%TSYS(J25)%SOU_IND(PIM%STA(J18)%TSYS(J25)%NPOI) )
                 ALLOCATE ( PIM%STA(J18)%TSYS(J25)%AZ_R4(PIM%STA(J18)%TSYS(J25)%NPOI) )
                 ALLOCATE ( PIM%STA(J18)%TSYS(J25)%ELEV_R4(PIM%STA(J18)%TSYS(J25)%NPOI) )
!
! -------------- Initialize the allocated memory
!
                 PIM%STA(J18)%TSYS(J25)%TSYS = 0.0
                 PIM%STA(J18)%TSYS(J25)%TIME_MID_R8  = 0.0
                 PIM%STA(J18)%TSYS(J25)%TIME_SPAN_R4 = 0.0
                 PIM%STA(J18)%TSYS(J25)%SOU_IND = 0
                 PIM%STA(J18)%TSYS(J25)%AZ_R4   = 0.0
                 PIM%STA(J18)%TSYS(J25)%ELEV_R4 = 0.0
!
                 DO 4260 J26=1,PIM%NOBS
!
! ----------------- Check whether the J18 -th station observed in this observation
! ----------------- and if yes, what was its station index in the baseline
!
                    IND_STA_BAS = 0
                    IF ( PIM%OBS(J26)%STA_IND(1) == J18 ) IND_STA_BAS = 1
                    IF ( PIM%OBS(J26)%STA_IND(2) == J18 ) IND_STA_BAS = 2
                    IF ( IND_STA_BAS == 0 ) GOTO 4260
!
! ----------------- Search for the frequency group index
!
                    IND_OBS_FRG = 0
                    DO 4270 J27=1,PIM%OBS(J26)%NUVS
                       IF ( PIM%OBS(J26)%GLO_FRG_INDS(J27) == J25 ) IND_OBS_FRG = J27
 4270               CONTINUE
                    IF ( IND_OBS_FRG == 0 ) GOTO 4260
                    IF ( PIM%OBS(J26)%NUM_EPC(IND_OBS_FRG) .LE. 0 ) GOTO 4260
!
                    OBS_BEG = PIM%TIM_R8(PIM%UV_IND(PIM%OBS(J26)%UV_IND(1,IND_FRG))%TIM_IND)
                    OBS_END = PIM%TIM_R8(PIM%UV_IND(PIM%OBS(J26)%UV_IND(PIM%OBS(J26)%NUM_EPC(IND_FRG),IND_FRG))%TIM_IND)
                    IND_TSYS = 0
                    IND_TSYS_SHADOW = 0
                    TSYS_DT_MIN = 86400.0D0
                    DO 4280 J28=1,NUM_TSYS(J18,J25)
                       TSYS_DT = 0.0
                       IF ( IND_SOU_TSYS(J28,J25,J18) == PIM%OBS(J26)%ROOT_SOU_IND  .AND. &
     &                      ( DABS(OBS_BEG - TIM_TSYS_ARR(J28,J25,J18)) < PIM__SPAN_DEF .OR.   &
     &                        DABS(OBS_END - TIM_TSYS_ARR(J28,J25,J18)) < PIM__SPAN_DEF      &
     &                      ) ) THEN
                            TSYS_DT = MIN ( DABS(OBS_BEG - TIM_TSYS_ARR(J28,J25,J18)), &
     &                                      DABS(OBS_END - TIM_TSYS_ARR(J28,J25,J18))  )
                            IF ( TSYS_DT < TSYS_DT_MIN ) THEN
                                 IND_TSYS_SHADOW = J28
                                 TSYS_DT_MIN = TSYS_DT
                            END IF
                       END IF
!
                       IF ( IND_SOU_TSYS(J28,J25,J18) == PIM%OBS(J26)%ROOT_SOU_IND  .AND. &
     &                      ( TIM_TSYS_ARR(J28,J25,J18) - OBS_BEG ) > -TIM_EPS .AND. &
     &                      ( OBS_END - TIM_TSYS_ARR(J28,J25,J18) ) > -TIM_EPS       ) THEN
                            IND_TSYS = J28
                       END IF
 4280               CONTINUE
                    IF ( IND_TSYS == 0 .AND. &
     &                   TSYS_DT_MIN < PIM__SPAN_DEF ) THEN
!
! ---------------------- We did not find Tsys that exactly falls within the range
! ---------------------- of the scan, but we found Tsys measurement that is in the
! ---------------------- close vicinity from the start or stop time
!
                         IND_TSYS = IND_TSYS_SHADOW
                      ELSE IF ( IND_TSYS == 0 ) THEN
                         IND_TSYS = IND_TSYS_SHADOW
                         PIM%OBS(J26)%TSYS_IND(IND_STA_BAS,J25) = 0
                         GOTO 4260
                    END IF
!
! ----------------- There is Tsys for this scan. Very good!
!
                    PIM%OBS(J26)%TSYS_IND(IND_STA_BAS,J25) = IND_TSYS
                    PIM%STA(J18)%TSYS(J25)%TIME_SPAN_R4(IND_TSYS) = PIM__SPAN_DEF
                    PIM%STA(J18)%TSYS(J25)%SOU_IND(IND_TSYS) = IND_SOU_TSYS(IND_TSYS,J25,J18)
                    PIM%STA(J18)%TSYS(J25)%AZ_R4(IND_TSYS)   = PIM%OBS(J26)%AZ(IND_STA_BAS)
                    PIM%STA(J18)%TSYS(J25)%ELEV_R4(IND_TSYS) = PIM%OBS(J26)%ELEV(IND_STA_BAS)
!
! ----------------- Finally, put the values of Tsys in appropriate slots
!
                    DO 4290 J29=1,PIM%NFRQ
                       DO 4300 J30=1,PIM%NPOL
                          PIM%STA(J18)%TSYS(J25)%TIME_MID_R8(IND_TSYS)  = TIM_TSYS_ARR(IND_TSYS,J25,J18)
                          PIM%STA(J18)%TSYS(J25)%TSYS(J29,IND_TSYS,J30) = TSYS_ARR(J29,IND_TSYS,J25,J30,J18)
 4300                 CONTINUE
 4290               CONTINUE
 4260            CONTINUE
            END IF
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!    write ( 6, * ) 'PGVL-1823 ind_obs: ', int2(j26), ' obs_tim ', sngl(obs_beg), sngl(obs_end) ! %%
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 4250    CONTINUE
 4180 CONTINUE
!
! --- Now put indexes of pcal into the obs data structure
!
      IND_FRG = IND_FRG_SAVE 
      K_BAD_OBS = 0
      DO 4310 J31=1,PIM%NOBS
         OBS_BEG = PIM%TIM_R8(PIM%UV_IND(PIM%OBS(J31)%UV_IND(1,IND_FRG))%TIM_IND)
         OBS_END = PIM%TIM_R8(PIM%UV_IND(PIM%OBS(J31)%UV_IND(PIM%OBS(J31)%NUM_EPC(IND_FRG),IND_FRG))%TIM_IND)

         DO 4320 J32=1,2
!            DO 4330 J33=1,PIM%NFRG
            DO 4330 J33=PIM_MIN_FRG,PIM_MAX_FRG
!
! ------------ Search for frequency group index
!
               IND_OBS_FRG = 0
               DO 4340 J34=1,PIM%OBS(J31)%NUVS
                  IF ( PIM%OBS(J31)%GLO_FRG_INDS(J34) == J33 ) IND_OBS_FRG = J34
 4340          CONTINUE
               IF ( IND_OBS_FRG == 0 ) GOTO 4330
!
               IF ( PIM%STA(PIM%OBS(J31)%STA_IND(J32))%PCAL(J33)%PCAL_AVAIL ) THEN
                    PIM%OBS(J31)%PCAL_IND(1:2,J32,J33) = 0
                    DO 4350 J35=1,PIM%STA(PIM%OBS(J31)%STA_IND(J32))%PCAL(J33)%NPOI
                       TIM_PCAL_BEG = PIM%STA(PIM%OBS(J31)%STA_IND(J32))%PCAL(J33)%TIME_MID_R8(J35) - &
     &                                PIM%STA(PIM%OBS(J31)%STA_IND(J32))%PCAL(J33)%TIME_SPAN_R4(J35)/2.0D0
                       TIM_PCAL_END = PIM%STA(PIM%OBS(J31)%STA_IND(J32))%PCAL(J33)%TIME_MID_R8(J35) + &
     &                                PIM%STA(PIM%OBS(J31)%STA_IND(J32))%PCAL(J33)%TIME_SPAN_R4(J35)/2.0D0
!
! -------------------- If either beginning or the end of validity interval of
! -------------------- phase cal is within the time range of a given observation
! -------------------- of the same source as pcal, then we store the index
! -------------------- of the pcal to the observation data structure
!
                       IF ( PIM%OBS(J31)%ROOT_SOU_IND == &
     &                      PIM%STA(PIM%OBS(J31)%STA_IND(J32))%PCAL(J33)%SOU_IND(J35) .AND. &
     &                      ( ( TIM_PCAL_BEG .LE. OBS_BEG .AND. &
     &                          TIM_PCAL_END .GE. OBS_BEG       ) .OR. &
     &                        ( TIM_PCAL_BEG .GE. OBS_BEG .AND. &
     &                          TIM_PCAL_END .LE. OBS_END       ) .OR. &
     &                        ( TIM_PCAL_BEG .LE. OBS_END .AND. &
     &                          TIM_PCAL_END .GE. OBS_END       ) .OR. &
     &                        ( TIM_PCAL_BEG .LE. OBS_BEG .AND. &
     &                          TIM_PCAL_END .GE. OBS_END       ) )    ) THEN
                            PIM%OBS(J31)%PCAL_IND(1,J32,J33) = J35
                            PIM%OBS(J31)%PCAL_IND(2,J32,J33) = J35
                       END IF
 4350               CONTINUE
                    IF ( PIM%OBS(J31)%PCAL_IND(1,J32,J33) == 0 ) THEN
!
! ---------------------- Did not find? Let us extend the interval of Pcal
! ---------------------- validity from both ends by PIM%CONF%MAX_SCAN_GAP
! ---------------------- and try again
!
                         DO 4360 J36=1,PIM%STA(PIM%OBS(J31)%STA_IND(J32))%PCAL(J33)%NPOI
                            TIM_PCAL_BEG = PIM%STA(PIM%OBS(J31)%STA_IND(J32))%PCAL(J33)%TIME_MID_R8(J36) - &
     &                                     PIM%STA(PIM%OBS(J31)%STA_IND(J32))%PCAL(J33)%TIME_SPAN_R4(J36)/2.0D0 - &
     &                                     PIM%CONF%MAX_SCAN_GAP
                            TIM_PCAL_END = PIM%STA(PIM%OBS(J31)%STA_IND(J32))%PCAL(J33)%TIME_MID_R8(J36) + &
     &                                     PIM%STA(PIM%OBS(J31)%STA_IND(J32))%PCAL(J33)%TIME_SPAN_R4(J36)/2.0D0 + &
     &                                     PIM%CONF%MAX_SCAN_GAP
                            IF ( ( PIM%STA(PIM%OBS(J31)%STA_IND(J32))%PCAL(J33)%SOU_IND(J36) == &
     &                             PIM%OBS(J31)%ROOT_SOU_IND ) &
     &                           .AND. &
     &                           ( ( TIM_PCAL_BEG .GE. OBS_BEG .AND. &
     &                               TIM_PCAL_BEG .LE. OBS_END       ) .OR. &
     &                             ( TIM_PCAL_END .GE. OBS_BEG .AND. &
     &                               TIM_PCAL_END .LE. OBS_END       ) .OR. &
     &                             ( TIM_PCAL_BEG .LE. OBS_BEG .AND. &
     &                               TIM_PCAL_END .GE. OBS_END       ) &
     &                            ) ) THEN
                                PIM%OBS(J31)%PCAL_IND(1:2,J32,J33) = J36
                           END IF
 4360                   CONTINUE
                    END IF
                    IF ( PIM%OBS(J31)%PCAL_IND(1,J32,J33) == 0 ) THEN
!
! ---------------------- Still did not find?? How can  it be?
! ---------------------- Let us extend the interval of Pcal
! ---------------------- validity from both ends by PIM%CONF%MAX_SCAN_LEN
! ---------------------- and try again.
!
                         DO 4370 J37=1,PIM%STA(PIM%OBS(J31)%STA_IND(J32))%PCAL(J33)%NPOI
                            TIM_PCAL_BEG = PIM%STA(PIM%OBS(J31)%STA_IND(J32))%PCAL(J33)%TIME_MID_R8(J37) - &
     &                                     PIM%STA(PIM%OBS(J31)%STA_IND(J32))%PCAL(J33)%TIME_SPAN_R4(J37)/2.0D0 - &
     &                                     PIM%CONF%MAX_SCAN_LEN
                            TIM_PCAL_END = PIM%STA(PIM%OBS(J31)%STA_IND(J32))%PCAL(J33)%TIME_MID_R8(J37) + &
     &                                     PIM%STA(PIM%OBS(J31)%STA_IND(J32))%PCAL(J33)%TIME_SPAN_R4(J37)/2.0D0 + &
     &                                     PIM%CONF%MAX_SCAN_LEN
                            IF ( ( PIM%STA(PIM%OBS(J31)%STA_IND(J32))%PCAL(J33)%SOU_IND(J37) == &
     &                             PIM%OBS(J31)%ROOT_SOU_IND ) &
     &                           .AND. &
     &                           ( ( TIM_PCAL_BEG .GE. OBS_BEG .AND. &
     &                               TIM_PCAL_BEG .LE. OBS_END       ) .OR. &
     &                             ( TIM_PCAL_END .GE. OBS_BEG .AND. &
     &                               TIM_PCAL_END .LE. OBS_END       ) .OR. &
     &                             ( TIM_PCAL_BEG .LE. OBS_BEG .AND. &
     &                               TIM_PCAL_END .GE. OBS_END       ) &
     &                            ) ) THEN
                                PIM%OBS(J31)%PCAL_IND(1:2,J32,J33) = J37
                           END IF
 4370                   CONTINUE
                    END IF
                    IF ( PIM%OBS(J31)%PCAL_IND(1,J32,J33) == 0 ) THEN
!
! ---------------------- What??? We still did not find?? Mmmm.
! ---------------------- Let us look for all sources and try again
!
                         DO 4380 J38=1,PIM%STA(PIM%OBS(J31)%STA_IND(J32))%PCAL(J33)%NPOI
                            TIM_PCAL_BEG = PIM%STA(PIM%OBS(J31)%STA_IND(J32))%PCAL(J33)%TIME_MID_R8(J38) - &
     &                                     PIM%STA(PIM%OBS(J31)%STA_IND(J32))%PCAL(J33)%TIME_SPAN_R4(J38)/2.0D0 - &
     &                                     PIM%CONF%MAX_SCAN_LEN
                            TIM_PCAL_END = PIM%STA(PIM%OBS(J31)%STA_IND(J32))%PCAL(J33)%TIME_MID_R8(J38) + &
     &                                     PIM%STA(PIM%OBS(J31)%STA_IND(J32))%PCAL(J33)%TIME_SPAN_R4(J38)/2.0D0 + &
     &                                     PIM%CONF%MAX_SCAN_LEN
                            IF ( ( TIM_PCAL_BEG .GE. OBS_BEG .AND. &
     &                             TIM_PCAL_BEG .LE. OBS_END       ) .OR. &
     &                           ( TIM_PCAL_END .GE. OBS_BEG .AND. &
     &                             TIM_PCAL_END .LE. OBS_END       ) .OR. &
     &                           ( TIM_PCAL_BEG .LE. OBS_BEG .AND. &
     &                             TIM_PCAL_END .GE. OBS_END       ) &
     &                           ) THEN
                                PIM%OBS(J31)%PCAL_IND(1:2,J32,J33) = J38
                           END IF
 4380                   CONTINUE
                    END IF
!
                    IF ( PIM%OBS(J31)%PCAL_IND(1,J32,J33) == 0  .AND.  &
     &                   PIM%CONF%CHECK_SEVERITY .GE. 1                ) THEN
!
                         CALL CLRCH  ( STR )
                         CALL INCH   ( J31, STR(1:7) )
                         CALL CHASHR (      STR(1:7) )
                         CALL ERR_PASS ( IUER, IER )
                         STR(11:) = MJDSEC_TO_DATE ( PIM%MJD_0, PIM%TAI_0 + &
     &                                               OBS_BEG, -2 )
                         STR(41:) = MJDSEC_TO_DATE ( PIM%MJD_0, PIM%TAI_0 + &
     &                                               OBS_END, -2 )
!
                         K_BAD_OBS = K_BAD_OBS + 1
                         IF ( PIM%CONF%DEBUG_LEVEL > 0 ) THEN
                              WRITE ( 6, 220 ) STR(1:7), PIM%STA(PIM%OBS(J31)%STA_IND(J32))%IVS_NAME, &
     &                                         STR(11:32), STR(41:61), &
     &                                         PIM%C_SOU(PIM%OBS(J31)%SOU_IND), J33
 220                          FORMAT ( 'MISSING PCAL Obs: ', A, ' Sta: ',A, &
     &                                 ' [ ', A, ' , ', A, ' ]  Sou: ', A, ' Frg: ', I1 )
                         END IF
                    END IF
               END IF
 4330       CONTINUE
 4320    CONTINUE
 4310 CONTINUE
!
! --- Now check whether Tsys was supplied for all observations
!
      NTSYS_MIS = 0
      DO 4390 J39=1,PIM%NOBS
         OBS_BEG = PIM%TIM_R8(PIM%OBS(J39)%TIM_BEG_IND)
         OBS_END = PIM%TIM_R8(PIM%OBS(J39)%TIM_END_IND)
         DO 4400 J40=1,2
            DO 4410 J41=PIM_MIN_FRG,PIM_MAX_FRG
!
! ------------ Search for frequency group index
!
               IND_OBS_FRG = 0
               DO 4420 J42=1,PIM%OBS(J39)%NUVS
                  IF ( PIM%OBS(J39)%GLO_FRG_INDS(J42) == J41 ) IND_OBS_FRG = J42
 4420          CONTINUE
               IF ( IND_OBS_FRG == 0 ) GOTO 4400
               IF ( PIM%OBS(J39)%NUM_EPC(IND_OBS_FRG) .LE. 0 ) GOTO 4400
!
               IF ( PIM%STA(PIM%OBS(J39)%STA_IND(J40))%TSYS(J41)%AVAIL ) THEN
                    IF ( PIM%OBS(J39)%TSYS_IND(J40,J41) .LE. 0 ) THEN
                         IF ( PIM%CONF%CHECK_SEVERITY .GE. 1 .AND. &
     &                        PIM%CONF%DEBUG_LEVEL > 0             ) THEN
!
                              CALL CLRCH  ( STR )
                              CALL INCH   ( J39, STR(1:7) )
                              CALL CHASHR (      STR(1:7) )
                              CALL ERR_PASS ( IUER, IER )
                              CALL INCH   ( J41, STR(71:71) )
                              STR(11:) = MJDSEC_TO_DATE ( PIM%MJD_0, PIM%TAI_0 + &
     &                                                    OBS_BEG, -2 )
                              STR(41:) = MJDSEC_TO_DATE ( PIM%MJD_0, PIM%TAI_0 + &
     &                                                    OBS_END, -2 )

                              WRITE ( 6, 230 ) STR(1:7), PIM%STA(PIM%OBS(J39)%STA_IND(J40))%IVS_NAME, &
     &                                         STR(11:31), STR(41:61), &
     &                                         PIM%C_SOU(PIM%OBS(J39)%SOU_IND), J41
 230                          FORMAT ( 'MISSING TSYS Obs: ', A, ' Sta: ',A, &
     &                                 ' [ ', A, ' , ', A, ' ]  Sou: ', A, &
     &                                 ' Frg: ', I1 )

                              NTSYS_MIS = NTSYS_MIS + 1
                         END IF
                    END IF
               END IF
 4410      CONTINUE
 4400     CONTINUE
 4390 CONTINUE
      IF ( K_BAD_OBS > 0 ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( K_BAD_OBS, STR )
           IF ( PIM%CONF%CHECK_SEVERITY .GE. 3 ) THEN
                CALL ERR_LOG ( 8377, IUER, 'PIMA_GET_VLBA_LOG', &
     &              'Phase calibration was not found for '//&
     &              STR(1:I_LEN(STR))//' observations' )
                RETURN
              ELSE
                IF ( PIM%CONF%DEBUG_LEVEL > 0 ) THEN
                     WRITE ( 6, '(A)' ) 'PHASE-CAL is missing for '// &
     &                                   STR(1:I_LEN(STR))//' observations'
                END IF
           END IF
         ELSE
           IF ( PIM%CONF%DEBUG_LEVEL > 0 ) THEN
                IF ( NO_TONES == 0 ) THEN
                     WRITE ( 6, '(A)' ) 'No PHASE-CAL was found'
                   ELSE
                     WRITE ( 6, '(A)' ) 'PHASE-CAL was found for all observations'
                END IF
           END IF
      END IF
      IF ( NTSYS_MIS > 0 ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( NTSYS_MIS, STR )
           IF ( PIM%CONF%CHECK_SEVERITY .GE. 3 ) THEN
                CALL ERR_LOG ( 8378, IUER, 'PIMA_GET_VLBA_LOG', &
     &              'Tsys was not found for '//&
     &               STR(1:I_LEN(STR))//' observations' )
                RETURN
              ELSE
                IF ( PIM%CONF%DEBUG_LEVEL > 0 ) THEN
                     WRITE ( 6, '(A)' ) 'Tsys is missing for '// &
     &                                   STR(1:I_LEN(STR))//' observations'
                END IF
           END IF
      END IF
!
! --- Clean-up
!
      CALL ERR_LOG ( 0, IUER )
 810  CONTINUE
      DEALLOCATE ( HUMID        )
      DEALLOCATE ( PRES         )
      DEALLOCATE ( TEMP         )
      DEALLOCATE ( TAI_WEA      )
      DEALLOCATE ( MJD_WEA      )
      DEALLOCATE ( CABLE_CAL    )
      DEALLOCATE ( PCAL_AMP     )
      DEALLOCATE ( PCAL_PHS     )
      DEALLOCATE ( IND_SOU_PCAL )
      DEALLOCATE ( IND_SOU_TSYS )
      DEALLOCATE ( IND_FRG_PCAL )
      DEALLOCATE ( MJD_TAG  )
      DEALLOCATE ( TAI_TAG  )
      DEALLOCATE ( NUM_TSYS )
      DEALLOCATE ( TSYS_ARR )
      DEALLOCATE ( TIM_TSYS_ARR  )
      DEALLOCATE ( BUF      )
!
      RETURN
      END  SUBROUTINE  PIMA_GET_VLBA_LOG  !#!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE CORR_FLAG_INIT ( PIM )
! ************************************************************************
! *                                                                      *
! *   Routine CORR_FLAG_INIT  incitializes cross-correlation flags to    *
! *   "BEST" values.                                                     *
! *                                                                      *
! * ### 06-JAN-2010 CORR_FLAG_INIT  v1.0 (c)  L. Petrov  06-JAN-2010 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'astro_constants.i'
      INCLUDE   'pima.i'
      INCLUDE   'vtd.i'
      TYPE     ( PIMA__TYPE ) :: PIM
      INTEGER*4  J1, J2
!
      DO 410 J1=1,PIM%NOBS
         IF ( ASSOCIATED ( PIM%OBS(J1)%CORR_FLAG ) ) THEN
              PIM%OBS(J1)%CORR_FLAG = 4
         END IF
 410  CONTINUE
!
      RETURN
      END  SUBROUTINE  CORR_FLAG_INIT !#!#
!
! ------------------------------------------------------------------------
!
      FUNCTION   GET_IVS_NAME ( PIM, ORIG_NAME )
      IMPLICIT   NONE
      INCLUDE   'pima.i'
      CHARACTER  GET_IVS_NAME*8
      TYPE     ( PIMA__TYPE ) :: PIM
      CHARACTER  ORIG_NAME*(*)
      INTEGER*4  J1
!
      DO 410 J1=1,PIM%NSTA
         IF ( ORIG_NAME == PIM%STA(J1)%ORIG_NAME ) THEN
              GET_IVS_NAME = PIM%STA(J1)%IVS_NAME
              RETURN
         END IF
 410  CONTINUE
!
      GET_IVS_NAME = 'Unknown '
      RETURN
      END  FUNCTION  GET_IVS_NAME  !#!#
