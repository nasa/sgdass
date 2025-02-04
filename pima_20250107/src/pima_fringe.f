#include <mk5_preprocessor_directives.inc>
      SUBROUTINE PIMA_FRINGE ( PIM, VTD, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  PIMA_FRINGE
! *                                                                      *
! * ### 11-JAN-2006    PIMA_FRINGE  v2.8  (c) L. Petrov 18-SEP-2024 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'astro_constants.i'
      INCLUDE   'fftw3.f'
      INCLUDE   'pima.i'
      INCLUDE   'vtd.i'
      TYPE     ( PIMA__TYPE  ) :: PIM
      TYPE     ( VTD__TYPE   ) :: VTD
      INTEGER*4  IUER
      INTEGER*4  MARR
      PARAMETER  ( MARR = 8192 )
      INTEGER*4  IND_TIM, ITURN
      CHARACTER  STR*128, STR1*128, STR_UV_PRINT*32
      REAL*8     T8(MARR), X8(MARR)
      REAL*8     EFF_DURA, FREQ_REF
      REAL*4     WEI_1D(PIM__MUV,PIM__MPLR)
      COMPLEX*8  BPASS_C8
      REAL*4     PHAS_R4, AMPL_R4, WEI_MAX
      REAL*8     TIME_FRT, GR_DEL(PIM__MFRA), PH_RAT(PIM__MFRA), &
     &           PHAS(PIM__MFRA), GR_RAT, PH_ACC, PHS_MOD, &
     &           AMPL(PIM__MFRA), SNR, SNR_FRA, AMPL_INTG
      REAL*8     EFF_FRQ_PHS, EFF_FRQ_GRP, EFF_FRQ_RAT
      REAL*8     PH_RAT_ERR(PIM__MFRA), GR_DEL_ERR(PIM__MFRA), &
     &           PH_DEL_ERR(PIM__MFRA), GR_RAT_ERR, PH_ACC_ERR, &
     &           PHAS_ERR(PIM__MFRA), GRAMBSP, SB_DEL, SB_DEL_ERR, &
     &           COV_PR_PH, COV_GR_MD, NOI_AVR, PAR_ANG_DIF, TEC, TEC_RATE, &
     &           TEC_ERR, TEC_RATE_ERR, PA_USED, FEED_ANG_DIF, DECOR_TIM, &
     &           DECOR_BS, DECOR_TS, NRML_BEAM_ATT(2), BEAM_ATT, FRQ_AVR, AN_FRQ
      REAL*8 T1(8192), X1(8192), X2(8192)
      REAL*8,    ALLOCATABLE :: TIME_FRT_OFFSET_ARR(:)
      LOGICAL*4  FL_BPASS, FL_PBP, FL_NOC, FL_NODATA, FL_NOPCAL, FL_PCAL_TOUSE, &
     &           FL_FRINGE_DFT, FL_AC
      INTEGER*8  DIR_DESC, STREAM, IS
      INTEGER*1  MASK_CHN
      INTEGER*4  J1, J2, J3, J4, J5, J6, J7, J8, J9, J10, J11, J12, &
     &           ISTA, IND_OBS, LTIM, LFRQ, WG_L, WG_H, WR_L, WR_H, &
     &           IP, LUN_FRI, LPOL, IND_PUS_STA, &
     &           LUN_RES, IFRQ, IND_BPS_BAS, BPS_SGN, IB, IDEV, &
     &           IND_STA_1, IND_STA_2, KP, FRI_STS, FRI_STS_INIT, FRG_IND, IND_FRA, &
     &           L_DEG, L_NOD, NZO_REF, FIRST_AMPL_FRQ, LAST_AMPL_FRQ, &
     &           POL_MODE, POL_LAB_IND(2), IND_OBS_PREV, IPTM, IER
      CHARACTER  DIRNAM*128, FINAM_2D_PLOT*128, FINAM_1D_PLOT*128, &
     &           FINAM_FRI*128, POLAR_SAVE*8, POL_BAS_LABEL*2
      INTEGER*2  MASK
      DATA       MASK / O'0755' /
      LOGICAL*1  FL_ALL_WEIGHTS_ONE, FL_FA_PRINT
      LOGICAL*4, EXTERNAL :: IS_R4_NAN, IS_R8_NAN
      REAL*4,    EXTERNAL :: ATAN_CS_R4, PHAS_CMPL_R4
      REAL*8,    EXTERNAL :: PIMA_BEAM_ATT 
      CHARACTER, EXTERNAL :: MJDSEC_TO_DATE*30, GET_CDATE*19, GET_CDATE_MS*23
      INTEGER*4, EXTERNAL :: GET_UNIT, ILEN, I_LEN, LTM_DIF
      INTEGER*8, EXTERNAL :: MKDIR, FUNC_OPENDIR, FOPEN, FCLOSE
      INTEGER*4, EXTERNAL :: FFTWF_IMPORT_WISDOM_FROM_FILE, GET_PROC_INFO
!
      CALL GETENVAR ( 'PIMAVAR_ALL_WEIGHTS_ONE', STR )
      IF ( STR == 'YES' .OR. STR == 'yes' ) THEN
           FL_ALL_WEIGHTS_ONE = .TRUE.
         ELSE 
           FL_ALL_WEIGHTS_ONE = .FALSE.
      END IF
!
      CALL GETENVAR ( 'PIMAVAR_FINE_FIRST_AMPL_FRQ', STR )
      IF ( ILEN(STR) > 0 ) THEN
           CALL CHIN ( STR, FIRST_AMPL_FRQ )
         ELSE
           FIRST_AMPL_FRQ = 1
      END IF
!
      CALL GETENVAR ( 'PIMAVAR_FINE_LAST_AMPL_FRQ', STR )
      IF ( ILEN(STR) > 0 ) THEN
           CALL CHIN ( STR, LAST_AMPL_FRQ )
         ELSE
           LAST_AMPL_FRQ = LFRQ
      END IF
!
      CALL GETENVAR ( 'PIMAVAR_FEED_ANG_PRINT', STR )
      IF ( STR == 'YES' .OR. STR == 'yes' ) THEN
           FL_FA_PRINT = .TRUE.
         ELSE 
           FL_FA_PRINT = .FALSE.
      END IF
!
      CALL GETENVAR ( 'PIMAVAR_FRINGE_DFT', STR )
      IF ( STR == 'YES' .OR. STR == 'yes' ) THEN
           FL_FRINGE_DFT = .TRUE.
         ELSE 
           FL_FRINGE_DFT = .FALSE.
      END IF
!
      CALL GETENVAR ( 'PIMAVAR_UV_PRINT', STR_UV_PRINT )
!
      IF ( PIM%CONF%FRIB_2D_FRINGE_PLOT == PIMA__PLOT_PS  .OR. &
     &     PIM%CONF%FRIB_2D_FRINGE_PLOT == PIMA__PLOT_GIF .OR. &
     &     PIM%CONF%FRIB_2D_FRINGE_PLOT == PIMA__PLOT_TXT .OR. &
     &     PIM%CONF%FRIB_1D_RESFRQ_PLOT == PIMA__PLOT_PS  .OR. &
     &     PIM%CONF%FRIB_1D_RESFRQ_PLOT == PIMA__PLOT_GIF .OR. &
     &     PIM%CONF%FRIB_1D_RESFRQ_PLOT == PIMA__PLOT_TXT .OR. &
     &     PIM%CONF%FRIB_1D_RESTIM_PLOT == PIMA__PLOT_PS  .OR. &
     &     PIM%CONF%FRIB_1D_RESTIM_PLOT == PIMA__PLOT_GIF .OR. &
     &     PIM%CONF%FRIB_1D_RESTIM_PLOT == PIMA__PLOT_TXT .OR. &
     &     PIM%CONF%FRIB_1D_DRF_PLOT    == PIMA__PLOT_PS  .OR. &
     &     PIM%CONF%FRIB_1D_DRF_PLOT    == PIMA__PLOT_GIF .OR. &
     &     PIM%CONF%FRIB_1D_DRF_PLOT    == PIMA__PLOT_TXT      ) THEN
!
           DIRNAM = PIM%CONF%SESS_CODE(1:I_LEN(PIM%CONF%SESS_CODE))// &
     &              '_fpl/'
           IP = I_LEN(PIM%CONF%EXPER_DIR)
           IF ( PIM%CONF%EXPER_DIR(IP:IP) .EQ. '/' ) THEN
                DIRNAM = PIM%CONF%EXPER_DIR(1:IP)//DIRNAM
              ELSE
                DIRNAM = PIM%CONF%EXPER_DIR(1:IP)//'/'//DIRNAM
           END IF
!
           DIR_DESC = FUNC_OPENDIR ( DIRNAM(1:I_LEN(DIRNAM))//CHAR(0) )
           IF ( DIR_DESC .EQ. 0 ) THEN
!
! ------------- Does not exist? We have to create it.
!
                IS = MKDIR ( DIRNAM(1:I_LEN(DIRNAM))//CHAR(0), %VAL(MASK) )
                IF ( IS .NE. 0 ) THEN
                     CALL GERROR  ( STR )
                     CALL ERR_LOG ( 7501, IUER, 'PIMA_FRINGE', 'Failure '// &
     &                   'to create direcotry '//DIRNAM(1:I_LEN(DIRNAM))// &
     &                   ' for fringe plots' )
                     RETURN
                END IF
              ELSE
                CALL CLOSEDIR ( %VAL(DIR_DESC) )
           END IF
         ELSE
           DIRNAM = '/tmp/'
      END IF
!
      IF ( PIM%CONF%BEG_FRQ < 1 ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( PIM%CONF%BEG_FRQ, STR ) 
           CALL ERR_LOG ( 7502, IUER, 'PIMA_FRINGE', 'Wrong parameter '// &
     &         'BEG_FRQ: '//STR(1:I_LEN(STR))//' -- it should be '// &
     &         ' a positive number' )
           RETURN 
      END IF
      IF ( PIM%CONF%END_FRQ > PIM%NFRQ ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( PIM%CONF%END_FRQ, STR ) 
           CALL CLRCH ( STR1 )
           CALL INCH  ( PIM%NFRQ, STR1 ) 
           CALL ERR_LOG ( 7503, IUER, 'PIMA_FRINGE', 'Wrong parameter '// &
     &         'END_FRQ: '//STR(1:I_LEN(STR))//' -- it exceeds '// &
     &         'the total number of frequencies: '//STR1 )
           RETURN 
      END IF
      IF ( PIM%CONF%END_FRQ < PIM%CONF%BEG_FRQ ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( PIM%CONF%END_FRQ, STR ) 
           CALL CLRCH ( STR1 )
           CALL INCH  ( PIM%CONF%BEG_FRQ, STR1 ) 
           CALL ERR_LOG ( 7504, IUER, 'PIMA_FRINGE', 'Wrong parameter '// &
     &         'END_FRQ: '//STR(1:I_LEN(STR))//' -- it exceeds '// &
     &         'BEG_FRQ: '//STR1 )
           RETURN 
      END IF
!
! --- Write down the header of the fringe file
!
      CALL ERR_PASS ( IUER, IER )
      CALL PIMA_FRI_WRI_HEAD ( PIM, LUN_FRI, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 7505, IUER, 'PIMA_FRINGE', 'Failure to write '// &
     &         'the header of the fringe output table' )
           RETURN
      END IF
!
! --- Write down the header of the fringe file
!
      CALL ERR_PASS ( IUER, IER )
      CALL PIMA_RESID_WRI_HEAD ( PIM, LUN_RES, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 7506, IUER, 'PIMA_FRINGE', 'Failure to write '// &
     &         'the header of the fringe residual output table' )
           RETURN
      END IF
!
      IF ( PIM%CONF%FRIB_OBS_STATUS  == PIMA__OBS_NO ) THEN
           CALL ERR_LOG ( 7507, IUER )
           RETURN
         ELSE IF ( PIM%CONF%FRIB_OBS_STATUS  == PIMA__OBS_ALL .AND. &
     &             PIM%CONF%FRIB_NOBS        == -1                  ) THEN
           PIM%CONF%FRIB_NOBS = PIM%NOBS
           ALLOCATE ( PIM%CONF%FRIB_OBS(PIM%CONF%FRIB_NOBS), STAT=IER )
           IF ( IER .NE. 0 ) THEN
                CALL CLRCH ( STR )
                CALL IINCH ( 4*PIM%CONF%FRIB_NOBS, STR )
                CALL ERR_LOG ( 7508, IUER, 'PIMA_FRINGE', &
     &              'Failure to allocate '//STR(1:I_LEN(STR))//' bytes '//&
     &              'of dynamic memory for the list of observations' )
                RETURN
           END IF
!
           DO 410 J1=1,PIM%CONF%FRIB_NOBS
              PIM%CONF%FRIB_OBS(J1) = J1
 410       CONTINUE
      END IF
#ifdef PIMA_USE_MKL
#else
      IF ( ( PIM%CONF%FRIB_SEARCH_TYPE == PIMA__2FFT .OR. &
     &       PIM%CONF%FRIB_SEARCH_TYPE == PIMA__3FFT      )  .AND. &
           PIM%CONF%FFT_METHOD .NE. FFT_MKL                        ) THEN
           STREAM = FOPEN ( PIM%CONF%FFT_CONFIG_FILE(1:I_LEN(PIM%CONF%FFT_CONFIG_FILE))//CHAR(0), 'r'//CHAR(0) )
           IF ( STREAM < 0 ) THEN
                CALL CLRCH  ( STR )
                CALL GERROR ( STR )
                CALL ERR_LOG ( 7509, IUER, 'PIMA_FRINGE', 'Error during '// &
     &              'opening FFTW wisdom file '//PIM%CONF%FFT_CONFIG_FILE )
                RETURN
           END IF
           IS = FFTWF_IMPORT_WISDOM_FROM_FILE ( %VAL(STREAM) )
           IF ( IS < 0 ) THEN
                CALL ERR_LOG ( 7510, IUER, 'PIMA_FRINGE', 'Error during '// &
     &              'reading FFTW wisdom file '//PIM%CONF%FFT_CONFIG_FILE )
                RETURN
           END IF
           IS = FCLOSE ( %VAL(STREAM) )
      END IF
#endif
!
      IF ( PIM%CONF%FRT_USE == PIMA__FRT_FILE ) THEN
           ALLOCATE ( TIME_FRT_OFFSET_ARR(PIM%NOBS), STAT=IER )
           IF ( IER .NE. 0 ) THEN
                CALL CLRCH ( STR )
                CALL IINCH ( 8*PIM%NOBS, STR )
                CALL ERR_LOG ( 7511, IUER, 'PIMA_FRINGE', 'Failure to '// &
     &              'allocate '//STR(1:I_LEN(STR))//' bytes of dynamic '// &
     &              'memory for TIME_FRT_OFFSET_ARR array' )
                RETURN
           END IF
!
           CALL ERR_PASS ( IUER, IER )
           CALL PIMA_READ_FRT_OFFSETS ( PIM%CONF%FRT_FILE, PIM%NOBS, &
     &                                  TIME_FRT_OFFSET_ARR, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 7512, IUER, 'PIMA_FRINGE', 'Failure in '// &
     &              'an attempt to read FRT offsets from the external '// &
     &              'file '//PIM%CONF%FRT_FILE )
                RETURN
           END IF
      END IF
!
! --- Initlialize VTD object
!
      CALL ERR_PASS ( IUER, IER )
      CALL VTD_INIT ( VTD,  IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 7513, IUER, 'PIMA_FRINGE', 'Error in an attempt to '// &
     &         'initialize VTD oibject' )
           RETURN
      END IF
!
! --- Read and parse VTD configuration file
!
      CALL ERR_PASS ( IUER, IER )
      CALL VTD_CONF ( PIM%CONF%VTD_CONFIG_FILE, VTD, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 7514, IUER, 'PIMA_FRINGE', 'Error in an attempt '// &
     &         'to read configuration file '//PIM%CONF%VTD_CONFIG_FILE )
           RETURN
      END IF
!
! --- Load catalogues, ephemerides, EOP series and other data files
!
      CALL ERR_PASS ( IUER, IER )
      PIM%C_STA(PIM%NSTA+1) = 'GEOCENTR'
      CALL VTD_LOAD ( VTD, PIM%NSTA+1, PIM%C_STA, PIM%NSOU, PIM%C_SOU, &
     &                PIM%MJD_0, PIM%TAI_0 - PIM__MSCL, PIM%MJD_0, &
     &                PIM%TAI_0 + PIM%TIM_R8(PIM%NEPC) + PIM__MSCL, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 7515, IUER, 'PIMA_FRINGE', 'Error in an '// &
     &         'attempt to load the data into VTD data structure' )
           RETURN
      END IF
!
! --- Disable automatic NERS update during the run
!
      VTD%NERS%CNF%AGE_FCS = 1.D15
      VTD%NERS%CNF%AGE_SPL = 1.D15
!
      IF ( ILEN(PIM%CONF%EPHEMERIDES_FILE) > 0 ) THEN
!
! -------- Read the ephemeride of the orbiting station
!
           PIM%NZO%FILNZO = PIM%CONF%EPHEMERIDES_FILE
           CALL ERR_PASS ( IUER, IER )
           CALL VTD_READ_NZO ( PIM%NZO%FILNZO, PIM__MNZO, PIM%NZO%L_NZO, &
     &                         PIM%NZO%MJD_ARR, PIM%NZO%TIM_ARR, PIM%NZO%POS_ARR, &
     &                         PIM%NZO%VEL_ARR, PIM%NZO%NZO_NAME, PIM%NZO%OBJ_TYPE, &
     &                         PIM%NZO%CENTER_NAME, PIM%NZO%REF_NAME, &
     &                         PIM%NZO%TIM_CODE, PIM%NZO%COO_CODE, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 7516, IUER, 'PIMA_FRINGE', 'Error in an '// &
     &              'attempt to read NZO data into VTD data structure' )
                RETURN 
           END IF
           IF ( PIM%NZO%CENTER_NAME == 'EARTH BARYCENTER' ) THEN
                NZO_REF = VTD__EME
              ELSE 
                CALL ERR_LOG ( 7517, IUER, 'PIMA_FRINGE', 'Unsupported '// &
     &              'coordinate center name: '//PIM%NZO%CENTER_NAME )
                RETURN 
           END IF
!
! -------- Expand the orbiting station ephemeride into the B-spline basis
!
           L_NOD = MIN ( PIM%NZO%L_NZO/PIMA__NZO_NOT_SCAL, VTD__M_NOD )
           L_DEG = 3
           CALL ERR_PASS ( IUER, IER )
           CALL VTD_LOAD_OBJ_NZO ( PIM%NZO%NZO_NAME, VTD__ES, VTD__OR, &
     &                             NZO_REF, PIM%NZO%TIM_CODE, &
     &                             VTD, PIM%NZO%L_NZO, PIM%NZO%MJD_ARR, &
     &                             PIM%NZO%TIM_ARR, PIM%NZO%POS_ARR, &
     &                             L_NOD, L_DEG, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 7518, IUER, 'PIMA_FRINGE', 'Error in an '// &
     &              'attempt to read NZO data into VTD data structure' )
                RETURN 
           END IF
      END IF
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
      IF ( PIM%CONF%L_WVR > 0 .AND. PIM%WVR_STATUS .NE. PIMA__LOADED ) THEN
!
! -------- Load WVR, since this is needed
!
           CALL ERR_PASS ( IUER, IER )
           CALL PIMA_LOAD_WVR ( PIM, 'load', IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 7519, IUER, 'PIMA_FRINGE', 'Error in '// &
     &              'an attempt to load WVR data into internal PIMA '// &
     &              'data structure' )
                RETURN 
           END IF
      END IF
!
      IND_OBS_PREV = -1
      DO 420 J2=1,PIM%CONF%FRIB_NOBS
         IF ( IND_OBS_PREV .NE. -1 ) THEN
              IF ( ASSOCIATED ( PIM%OBS(IND_OBS_PREV)%AC_AVR_TIM ) ) DEALLOCATE ( PIM%OBS(IND_OBS_PREV)%AC_AVR_TIM )
              IF ( ASSOCIATED ( PIM%OBS(IND_OBS_PREV)%TSRF )       ) DEALLOCATE ( PIM%OBS(IND_OBS_PREV)%TSRF    )
              IF ( ASSOCIATED ( PIM%OBS(IND_OBS_PREV)%WEI_1D )     ) DEALLOCATE ( PIM%OBS(IND_OBS_PREV)%WEI_1D  )
              IF ( ASSOCIATED ( PIM%OBS(IND_OBS_PREV)%UV_BAND )    ) DEALLOCATE ( PIM%OBS(IND_OBS_PREV)%UV_BAND )
              IF ( ASSOCIATED ( PIM%OBS(IND_OBS_PREV)%UV_IF )      ) DEALLOCATE ( PIM%OBS(IND_OBS_PREV)%UV_IF   )
              IF ( ASSOCIATED ( PIM%OBS(IND_OBS_PREV)%UV  )        ) DEALLOCATE ( PIM%OBS(IND_OBS_PREV)%UV      )
         END IF
         IF ( PIM%CONF%DEBUG_LEVEL == 8 ) THEN
              CALL CPU_TIMER ( %VAL(0) )
         END IF
         IND_OBS = PIM%CONF%FRIB_OBS(J2)
         IF ( .NOT. PIM%USE_OBS(IND_OBS) ) GOTO 420 ! Bypass deselected observation
         FRG_IND = PIM%OBS(IND_OBS)%REF_FRG_INDS(PIM%CONF%FRQ_GRP) 
!@         IF ( FRG_IND == 0 ) THEN
!@              WRITE ( 6, * ) 'PIMA_FRINGE NUM_NUVS= ', INT2(PIM%OBS(IND_OBS)%NUVS), &
!@     &                       ' NUM_EPC= ', INT2(PIM%OBS(IND_OBS)%NUM_EPC), &
!@     &                       ' REF_FRG_INDS= ', PIM%OBS(IND_OBS)%REF_FRG_INDS
!@              WRITE ( 6, 265 ) IND_OBS, PIM%CONF%FRQ_GRP
!@ 265          FORMAT ( 'Observation ', I6, &
!@     &                 ' does not have accumulation periods for frequency group ', I1 )
!@              GOTO 420
!@         END IF
         IF (  PIM%OBS(IND_OBS)%NUM_EPC(PIM%OBS(IND_OBS)%REF_FRG_INDS(PIM%CONF%FRQ_GRP)) < 2 ) THEN
               IF ( PIM%CONF%DEBUG_LEVEL .GE. 1 ) THEN
                    WRITE ( 6, * ) 'PIMA_FRINGE NUM_NUVS= ', INT2(PIM%OBS(IND_OBS)%NUVS), &
     &                             ' NUM_EPC= ', INT2(PIM%OBS(IND_OBS)%NUM_EPC)
                    WRITE ( 6, 270 ) IND_OBS
 270                FORMAT ( 'Observation ', I6, &
     &                       ' has less than 2 accumulation periods' )
               END IF
               GOTO 420
         END IF
         FRI_STS = 0
!
! ------ Bypass obervations with wrong index
!
         IF ( IND_OBS < 1  .OR.  IND_OBS > PIM%NOBS ) GOTO 420
!
! ------ Bypass observation without data for this frequency group
!
         IF ( PIM%OBS(IND_OBS)%UV_IND(1,PIM%OBS(IND_OBS)%REF_FRG_INDS(PIM%CONF%FRQ_GRP)) < 1 ) GOTO 420
!
         STR(1:30)  = MJDSEC_TO_DATE ( PIM%MJD_0, PIM%TAI_0 + &
     &                                 PIM%TIM_R8(PIM%UV_IND(PIM%OBS(IND_OBS)%UV_IND(1,FRG_IND))%TIM_IND), -3 )
         STR(31:60) = MJDSEC_TO_DATE ( PIM%MJD_0, PIM%TAI_0 + &
     &                                 PIM%TIM_R8(PIM%UV_IND(PIM%OBS(IND_OBS)%UV_IND(PIM%OBS(IND_OBS)%NUM_EPC(FRG_IND),FRG_IND))%TIM_IND), -3 )
!
         IF ( PIM%CONF%L_WVR > 0  .AND. &
     &        PIM%CONF%WVR_USE .NE. PIMA__WVR_NO ) THEN
!
! ----------- We see that the user requested to compute WVR phases.
! ----------- Let us do it for the IND_OBS -th observations by interpolating
! ----------- and smoothing original WVR data
!
              CALL ERR_PASS ( IUER, IER )
              CALL PIMA_WVR_TO_OBS ( PIM, IND_OBS, IER ) 
              IF ( IER .NE. 0 ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( IND_OBS, STR )
                   CALL ERR_LOG ( 7520, IUER, 'PIMA_FRINGE', 'Error '// &
     &                 'during an attempt to apply WVR phase to '// &
     &                 'observation #'//STR )
                   RETURN 
              END IF
            ELSE 
              PIM%OBS(IND_OBS)%WVR_FLAG = 0
         END IF
!
         FL_NODATA = .FALSE.
         FL_NOPCAL = .FALSE.
!
         LFRQ = PIM%CONF%END_FRQ - PIM%CONF%BEG_FRQ + 1
         LTIM = PIM%OBS(IND_OBS)%NUM_EPC(PIM%OBS(IND_OBS)%REF_FRG_INDS(PIM%CONF%FRQ_GRP))
!
         CALL ERR_PASS ( IUER, IER )
         CALL PIMA_GET_POL_MODE ( PIM, PIM%CONF%POLAR, IND_OBS, POL_MODE, IER )
         IF ( IER .NE. 0 ) THEN
              CALL CLRCH ( STR )
              CALL INCH  ( IND_OBS, STR )
              CALL ERR_LOG ( 7521, IUER, 'PIMA_FRINGE', 'Cannot set polarization '// &
     &            'mode for observation '//STR )
              RETURN
         END IF
         IF ( PIM%CONF%POLAR == PIMA__POLAR_ORIG .OR. &
     &        PIM%CONF%POLAR == PIMA__POLAR_ALL  .OR. &
     &        PIM%CONF%POLAR == PIMA__POLAR_1ST  .OR. &
     &        PIM%CONF%POLAR == PIMA__POLAR_2ND       ) THEN
              LPOL = 4
              IF ( PIM%NSTK .NE. 4 ) THEN
                   CALL ERR_LOG ( 7522, IUER, 'PIMA_FRINGE', 'Wrong value '// &
     &                 'of POLAR: keyword. ORIG or ALL can be specified only of '// &
     &                 'all 4 Stokes parameters have been correlated' )
                   RETURN 
              END IF
            ELSE 
              LPOL = 1
         END IF
!
! ------ Get UV data and put them in PIM%OBS(IND_OBS)%UV, PIM%OBS(IND_OBS)%UV_IF, PIM%OBS(IND_OBS)%UV_BAND
!
         IF ( PIM%CONF%DEBUG_LEVEL .GE. 7 ) THEN
              FL_AC = .TRUE.
            ELSE
              FL_AC = .FALSE.
         END IF
!
         CALL ERR_PASS ( IUER, IER )
         CALL PIMA_GET_OBS ( PIM, VTD, IND_OBS, POL_MODE, LPOL, &
     &                      FL_AC, .FALSE., .FALSE., FRI_STS, IER )
         IF ( INDEX ( STR_UV_PRINT, 'RAW1' ) > 0 ) THEN
              CALL PIMA_UV_PRINT ( PIM, IND_OBS, 1, 'PF-RAW1' )
            ELSE IF ( INDEX ( STR_UV_PRINT, 'RAW2' ) > 0 ) THEN
              CALL PIMA_UV_PRINT ( PIM, IND_OBS, 2, 'PF-RAW2' )
         END IF
         IF ( IER .NE. 0 ) THEN
              CALL PIMA_FRI_WRI_OBS ( PIM, LUN_FRI, IND_OBS,    -1.0D0, &
     &                       -1.0D0, -1.0D0, -1.0D0, -1.0D0, -1.0D0, -1.0D0, &
     &                       -1.0D0, -1.0D0, -1.0D0, -1.0D0, -1.0D0, -1.0D0, &
     &                       -1.0D0, -1.0D0, -1.0D0, -1.0D0, -1.0D0, -1.0D0, &
     &                       -1.0D0, -1.0D0, -1.0D0, -1.0D0, -1.0D0, -1.0D0, &
     &                       -1.0D0, -1.0D0, -1.0D0, -1.0D0, -1.0D0,         &
     &                       -1.0D0, -1.0D0, -1.0D0, '??', FRI_STS, IER )
              CALL CLRCH ( STR )
              CALL INCH  ( IND_OBS, STR )
              CALL ERR_PASS ( IUER, IER )
              CALL ERR_LOG  ( 7530, IER, 'PIMA_FRINGE', 'Failure to get '// &
     &            'UV data for the '//STR(1:I_LEN(STR))//'-th observation '// &
     &            'from input FITS-IDI file ' )
              IF ( PIM%CONF%FRINGE_ERRORS == PIMA__ERR_STOP ) THEN
                   CALL ERR_PASS ( IER, IUER )
                   RETURN
                 ELSE
                   IF ( PIM%CONF%WARNING ) THEN
                        WRITE ( 6, 210 ) IND_OBS
 210                    FORMAT ( 'WARNING: Failed to get data for observation ', I6, &
     &                           '. Nevertheless, continue to the next observation' )
                   END IF
                   GOTO 420
              END IF
         END IF
!
         CALL GETENVAR ( 'PIMAVAR_TIM_MERGE', STR )
         IF ( ILEN(STR) > 0 ) THEN
              CALL CHIN ( STR, IPTM )
              CALL PIMA_TIM_MERGE ( PIM, IND_OBS, IPTM, IER )
         END IF
!
         IF ( PIM%CONF%DEBUG_LEVEL == 8 ) THEN
              CALL CPU_TIMER ( STR )
              WRITE ( 6, '(A)' ) 'Read data timing:  '//STR(1:27)
              CALL CPU_TIMER ( %VAL(0) )
         END IF
         IF ( FL_ALL_WEIGHTS_ONE ) WEI_1D = 1.0
!
! ------ Check whether the data has been received
!
         IF ( FL_NODATA ) THEN
              WEI_MAX = MAXVAL ( WEI_1D(1:PIM%OBS(IND_OBS)%NUM_EPC(PIM%OBS(IND_OBS)%REF_FRG_INDS(PIM%CONF%FRQ_GRP)),1) )
              IF ( WEI_MAX == 0.0 ) FL_NODATA = .TRUE.
         END IF
!
         IF ( FL_NODATA .OR. FL_NOPCAL  ) THEN
              IF ( PIM%CONF%DEBUG_LEVEL .GE. 5 ) THEN
                   WRITE ( 6, 240 ) IND_OBS
 240               FORMAT ( 'PIMA_FRINGE Obs ', I6, ' All weights for ', &
     &                      'cross-correlation data are zero' )
              END IF
!
! ----------- Alas, no cross-correlation data have been received
!
              CALL ERR_PASS ( IUER, IER )
              FRI_STS = IBSET ( FRI_STS, FAI__PIM )
              FRI_STS = IBSET ( FRI_STS, NDT__PIM )
              IF ( FL_NODATA ) FRI_STS = IBSET ( FRI_STS, NDA__PIM )
              IF ( FL_NOPCAL ) FRI_STS = IBSET ( FRI_STS, NPC__PIM )
              CALL PIMA_FRI_WRI_OBS ( PIM, LUN_FRI, IND_OBS,    -1.0D0, &
     &                       -1.0D0, -1.0D0, -1.0D0, -1.0D0, -1.0D0, -1.0D0, &
     &                       -1.0D0, -1.0D0, -1.0D0, -1.0D0, -1.0D0, -1.0D0, &
     &                       -1.0D0, -1.0D0, -1.0D0, -1.0D0, -1.0D0, -1.0D0, &
     &                       -1.0D0, -1.0D0, -1.0D0, -1.0D0, -1.0D0, -1.0D0, &
     &                       -1.0D0, -1.0D0, -1.0D0, -1.0D0, -1.0D0,         &
     &                       -1.0D0, -1.0D0, -1.0D0, '??', FRI_STS, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 7531, IUER, 'PIMA_FRINGE', &
     &                 'Failure to write output file with results '// &
     &                 'of fringing '//PIM%CONF%FRINGE_FILE )
                   RETURN
              END IF
              GOTO 820
         END IF
!
         IF ( PIM%OBS(IND_OBS)%WVR_FLAG == PIMA__WVR_AVL ) THEN
              CALL PIMA_WVR_USE ( PIM, PIM%NCHN, LFRQ, LTIM, IND_OBS, PIM%OBS(IND_OBS)%UV(1,1,1,1) )
         END IF
!
         IFRQ = 0
         FRQ_AVR = 0.0D0
         AN_FRQ  = 0.0D0
         DO 450 J5=PIM%CONF%BEG_FRQ,PIM%CONF%END_FRQ
            IFRQ = IFRQ + 1
            DO 460 J6=1,PIM%NCHN
!
! ------------ Compute frequencies
!
               IF ( IFRQ == 1 .AND.  J6 == 1 ) THEN
                    FREQ_REF = PIM%FRQ(PIM%CONF%BEG_FRQ,PIM%CONF%FRQ_GRP)%FREQ
               END IF
               IF ( PIM%BANDPASS_MASK_STYLE .NE. PIMA__NO ) THEN
!
! ----------------- Apply bandpass mask for computing WEI_PNT
!
                    MASK_CHN = PIM%BANDPASS_MASK(J6,J5,PIM%OBS(IND_OBS)%STA_IND(1),PIMA__MASK_FRNG) * &
     &                         PIM%BANDPASS_MASK(J6,J5,PIM%OBS(IND_OBS)%STA_IND(2),PIMA__MASK_FRNG)
                 ELSE
                    MASK_CHN = 1
               END IF 
               FRQ_AVR = FRQ_AVR + PIM%FREQ_ARR(J6,J5,PIM%CONF%FRQ_GRP)*MASK_CHN
               AN_FRQ  = AN_FRQ + MASK_CHN 
 460        CONTINUE 
 450     CONTINUE 
         IF ( AN_FRQ > 0 ) THEN 
              FRQ_AVR = FRQ_AVR/AN_FRQ
            ELSE
              FRQ_AVR = ( PIM%FREQ_ARR(1,PIM%CONF%BEG_FRQ,PIM%CONF%FRQ_GRP)       + &
     &                    PIM%FREQ_ARR(PIM%NCHN,PIM%CONF%END_FRQ,PIM%CONF%FRQ_GRP)  )/2.0D0
         END IF
!
         IF ( PIM%CONF%DEBUG_LEVEL == 8 ) THEN
              CALL CPU_TIMER ( STR )
              WRITE ( 6, '(A)' ) 'Preprocessing data timing:  '//STR(1:27)
              CALL CPU_TIMER ( %VAL(0) )
         END IF
!
         IF ( PIM%CONF%FRIB_2D_FRINGE_PLOT == PIMA__PLOT_PS  .OR. &
     &        PIM%CONF%FRIB_2D_FRINGE_PLOT == PIMA__PLOT_GIF .OR. &
     &        PIM%CONF%FRIB_2D_FRINGE_PLOT == PIMA__PLOT_TXT      ) THEN
!
! ----------- Build the name of the 2D-plot
!
              FINAM_2D_PLOT = 'fr2d_'// &
     &                   PIM%SCA(PIM%OBS(IND_OBS)%SCA_IND)%SCAN_NAME(1:9)// &
     &                   '_'//PIM%CONF%BAND//'_'// &
     &                   PIM%STA(PIM%OBS(IND_OBS)%STA_IND(1))%IVS_NAME//'_'// &
     &                   PIM%STA(PIM%OBS(IND_OBS)%STA_IND(2))%IVS_NAME
              CALL TRAN ( 12, FINAM_2D_PLOT, FINAM_2D_PLOT )
              CALL BLANK_TO_UNDERSCORE ( FINAM_2D_PLOT(1:I_LEN(FINAM_2D_PLOT)) )
              IF ( ILEN(DIRNAM) > 0 ) THEN
                   FINAM_2D_PLOT = DIRNAM(1:I_LEN(DIRNAM))//FINAM_2D_PLOT
              END IF
         END IF
         IF ( PIM%CONF%FRIB_FINE_SEARCH == PIMA__FINE_SEARCH_LSQ .OR. &
     &        PIM%CONF%FRIB_FINE_SEARCH == PIMA__FINE_SEARCH_LSQ      ) THEN
              IND_FRA = PIMA__LSQ
            ELSE
              IND_FRA = PIMA__DRF
         END IF
!
         FRI_STS_INIT = FRI_STS
         DO 470 J7=1,LPOL
            IF ( PIM%CONF%POLAR == PIMA__POLAR_ORIG .OR. &
     &           PIM%CONF%POLAR == PIMA__POLAR_ALL  .OR. &
     &           PIM%CONF%POLAR == PIMA__POLAR_1ST  .OR. &     
     &           PIM%CONF%POLAR == PIMA__POLAR_2ND       ) THEN
                 IF ( J7 == 1 ) THEN
                      PIM%OBS(IND_OBS)%POLAR_USED = PIM%OBS(IND_OBS)%POLARIZ(1)(1:1)//PIM%OBS(IND_OBS)%POLARIZ(1)(1:1)
                    ELSE IF ( J7 == 2 ) THEN
                      PIM%OBS(IND_OBS)%POLAR_USED = PIM%OBS(IND_OBS)%POLARIZ(1)(2:2)//PIM%OBS(IND_OBS)%POLARIZ(2)(1:1)
                    ELSE IF ( J7 == 3 ) THEN
                      PIM%OBS(IND_OBS)%POLAR_USED = PIM%OBS(IND_OBS)%POLARIZ(1)(1:1)//PIM%OBS(IND_OBS)%POLARIZ(2)(2:2)
                    ELSE IF ( J7 == 4 ) THEN
                      PIM%OBS(IND_OBS)%POLAR_USED = PIM%OBS(IND_OBS)%POLARIZ(1)(2:2)//PIM%OBS(IND_OBS)%POLARIZ(2)(2:2)
                 END IF
                 IF ( PIM%CONF%POLAR == PIMA__POLAR_ALL ) THEN
                      IF ( PIM%OBS(IND_OBS)%POLAR_USED .NE. 'RL' .AND. &
     &                     PIM%OBS(IND_OBS)%POLAR_USED .NE. 'LR'       ) THEN
                           IF ( PIM%OBS(IND_OBS)%POLAR_USED(1:1) == 'H' .OR. &
     &                          PIM%OBS(IND_OBS)%POLAR_USED(1:1) == 'R' ) PIM%OBS(IND_OBS)%POLAR_USED(1:1) = 'X'
                           IF ( PIM%OBS(IND_OBS)%POLAR_USED(1:1) == 'V' .OR. &
     &                          PIM%OBS(IND_OBS)%POLAR_USED(1:1) == 'L' ) PIM%OBS(IND_OBS)%POLAR_USED(1:1) = 'Y'
!
                           IF ( PIM%OBS(IND_OBS)%POLAR_USED(2:2) == 'H' .OR. &
     &                          PIM%OBS(IND_OBS)%POLAR_USED(2:2) == 'R' ) PIM%OBS(IND_OBS)%POLAR_USED(2:2) = 'X'
                           IF ( PIM%OBS(IND_OBS)%POLAR_USED(2:2) == 'V' .OR. &
     &                          PIM%OBS(IND_OBS)%POLAR_USED(2:2) == 'L' ) PIM%OBS(IND_OBS)%POLAR_USED(2:2) = 'Y'
                      END IF
                    ELSE IF ( PIM%CONF%POLAR == PIMA__POLAR_1ST ) THEN
                      IF ( PIM%OBS(IND_OBS)%POLAR_USED(1:1) == 'H' ) PIM%OBS(IND_OBS)%POLAR_USED(1:1) = 'F' 
                      IF ( PIM%OBS(IND_OBS)%POLAR_USED(1:1) == 'V' ) PIM%OBS(IND_OBS)%POLAR_USED(1:1) = 'S' 
                      IF ( PIM%OBS(IND_OBS)%POLAR_USED(2:2) == 'H' ) PIM%OBS(IND_OBS)%POLAR_USED(2:2) = 'F' 
                      IF ( PIM%OBS(IND_OBS)%POLAR_USED(2:2) == 'V' ) PIM%OBS(IND_OBS)%POLAR_USED(2:2) = 'S' 
                    ELSE IF ( PIM%CONF%POLAR == PIMA__POLAR_2ND ) THEN
                      IF ( PIM%OBS(IND_OBS)%POLAR_USED(1:1) == 'H' ) PIM%OBS(IND_OBS)%POLAR_USED(1:1) = 'f' 
                      IF ( PIM%OBS(IND_OBS)%POLAR_USED(1:1) == 'V' ) PIM%OBS(IND_OBS)%POLAR_USED(1:1) = 's' 
                      IF ( PIM%OBS(IND_OBS)%POLAR_USED(2:2) == 'H' ) PIM%OBS(IND_OBS)%POLAR_USED(2:2) = 'f' 
                      IF ( PIM%OBS(IND_OBS)%POLAR_USED(2:2) == 'V' ) PIM%OBS(IND_OBS)%POLAR_USED(2:2) = 's' 
                 END IF
               ELSE
                 PIM%OBS(IND_OBS)%POLAR_USED = PIM%CONF%POLAR
            END IF
!
            CALL ERR_PASS ( IUER, IER )
            IF ( PIM%CONF%FRIB_SEARCH_TYPE == PIMA__2FFT ) THEN
                 IF ( PIM%CONF%FRT_USE == PIMA__FRT_FILE ) THEN
                      TIME_FRT = TIME_FRT_OFFSET_ARR(IND_OBS)
                    ELSE IF ( PIM%CONF%FRT_USE == PIMA__FRT_VAL ) THEN
                      TIME_FRT = PIM%CONF%FRT_OFFSET
                    ELSE
                      TIME_FRT = PIMA__FRT_UNDF
                 END IF
                 CALL PIMA_2FFT ( PIM, VTD, IND_OBS, PIM%NCHN, LFRQ, LTIM, &
     &                            PIM%FREQ_ARR(1,PIM%CONF%BEG_FRQ,PIM%CONF%FRQ_GRP), &
     &                            FREQ_REF, PIM%OBS(IND_OBS)%UV(1,1,1,J7), &
     &                            PIM%OBS(IND_OBS)%WEI_1D, &
     &                            PIM%OBS(IND_OBS)%AP_LEN, FINAM_2D_PLOT, TIME_FRT, &
     &                            GR_DEL, PH_RAT, GR_RAT, PH_ACC, PHAS, AMPL, SNR, &
     &                            GR_DEL_ERR, PH_DEL_ERR, PH_RAT_ERR, GR_RAT_ERR, &
     &                            PH_ACC_ERR, PHAS_ERR, GRAMBSP, EFF_FRQ_PHS, &
     &                            EFF_FRQ_GRP, EFF_FRQ_RAT, COV_PR_PH, COV_GR_MD, &
     &                            EFF_DURA, TEC, TEC_RATE, TEC_ERR, TEC_RATE_ERR, &
     &                            DECOR_TIM, IER )
                 IF ( IER == 0  .AND.  SNR .LE. 0.0D0 ) THEN
                      SNR_FRA = SNR
                      GOTO 870
                 END IF
            END IF
            FRI_STS = FRI_STS_INIT 
            IF ( AMPL(PIMA__DRF) > PIMA__AMP_MIN ) THEN
                 IF ( FIRST_AMPL_FRQ .EQ. 1 .AND. LAST_AMPL_FRQ .EQ. LFRQ ) THEN
                      SNR_FRA = SNR*AMPL(IND_FRA)/AMPL(PIMA__DRF)
                    ELSE 
                      SNR_FRA = SNR
                 END IF
               ELSE
                 SNR_FRA = SNR
            END IF
            IF ( SNR > 0.001 ) THEN
                 NOI_AVR = AMPL(IND_FRA)/SNR_FRA
               ELSE
                 NOI_AVR = -1.0D0
            END IF
!
            IF ( IER .NE. 0 ) THEN
                 CALL CLRCH ( STR )
                 CALL INCH  ( IND_OBS, STR )
                 CALL ERR_PASS ( IUER, IER )
                 CALL ERR_LOG ( 7534, IER, 'PIMA_FRINGE', 'Error '// &
     &               'during attempt to perform fringe fitting for the '// &
     &                STR(1:I_LEN(STR))//' th observation' )
                 IF ( PIM%CONF%FRINGE_ERRORS == PIMA__ERR_STOP ) THEN
                      CALL ERR_PASS ( IER, IUER )
                      RETURN
                    ELSE
                      CALL ERR_PASS ( IUER, IER )
                      FRI_STS = IBSET ( FRI_STS, FAI__PIM )
                      CALL PIMA_FRI_WRI_OBS ( PIM, LUN_FRI, IND_OBS,    -1.0D0, &
     &                          -1.0D0, -1.0D0, -1.0D0, -1.0D0, -1.0D0, -1.0D0, &
     &                          -1.0D0, -1.0D0, -1.0D0, -1.0D0, -1.0D0, -1.0D0, &
     &                          -1.0D0, -1.0D0, -1.0D0, -1.0D0, -1.0D0, -1.0D0, &
     &                          -1.0D0, -1.0D0, -1.0D0, -1.0D0, -1.0D0, -1.0D0, &
     &                          -1.0D0, -1.0D0, -1.0D0, -1.0D0, -1.0D0, &
     &                          -1.0D0, -1.0D0, -1.0D0, '??', FRI_STS, IER )
                      IF ( IER .NE. 0 ) THEN
                           CALL ERR_LOG ( 7535, IUER, 'PIMA_FRINGE', &
     &                         'Failure to write output file with results '// &
     &                         'of fringing '//PIM%CONF%FRINGE_FILE )
                           RETURN
                      END IF
                      GOTO 820
                 END IF
            END IF
!
            IF ( PIM%CONF%FRIB_AMPL_EDGE_BEAM_COR == PIMA__BEAM_YES ) THEN
!
! -------------- Compute beam attenuatiion on the average frequency
!
                 NRML_BEAM_ATT(1) = PIMA_BEAM_ATT ( PIM, FRQ_AVR, &
     &                                        INT(PIM%OBS(IND_OBS)%SOU_IND,KIND=4), &
     &                                        INT(PIM%OBS(IND_OBS)%STA_IND(1),KIND=4)  )
                 NRML_BEAM_ATT(2) = PIMA_BEAM_ATT ( PIM, FRQ_AVR, &
     &                                        INT(PIM%OBS(IND_OBS)%SOU_IND,KIND=4), &
     &                                        INT(PIM%OBS(IND_OBS)%STA_IND(2),KIND=4)  )
                 BEAM_ATT = DSQRT (  NRML_BEAM_ATT(1) * NRML_BEAM_ATT(2) )
               ELSE 
                 BEAM_ATT = 1.0D0
            END IF
            IF ( PIM%CONF%FRIB_AMPL_EDGE_WINDOW_COR == PIMA__EDGE_AMP_USE .AND. &
     &           SNR_FRA > PIM%CONF%FRIB_SNR_DETECTION                          ) THEN
!
! -------------- Compute decorrelation losses in an individual UV-point
! -------------- due to significant group delay and/or phase delay rate
!
                 CALL ERR_PASS ( IUER, IER )
                 CALL PIMA_DECOR_SMEARING ( PIM, VTD, IND_OBS, TIME_FRT, GR_DEL(IND_FRA), &
     &                                      DECOR_BS, DECOR_TS, IER )
                 IF ( IER .NE. 0 ) THEN
                      CALL ERR_LOG ( 7536, IUER, 'PIMA_FRINGE', 'Error in '// &
     &                    'computation decorrelation due to bandwidth and time '// &
     &                    'smearing' )
                      RETURN 
                 END IF
                 IF ( PIM%CONF%DEBUG_LEVEL .GE. 6 ) THEN
                      IF ( IS_R8_NAN(BEAM_ATT) ) BEAM_ATT = 1.0D0
                      WRITE  ( 6, 110 ) DECOR_BS, DECOR_TS, BEAM_ATT, &
     &                                  DECOR_BS*DECOR_TS*BEAM_ATT
 110                  FORMAT ( 'PIMA_FRINGE  Decorrelation ' &
     &                         ' Band_sm: ', F8.5, &
     &                         ' Time_sm: ', F8.5, &
     &                         ' Beam: ', F8.5, &
     &                         ' All: ', F8.5 )
                      CALL FLUSH ( 6 )
                 END IF
!
                 CALL GETENV ( 'PIMAVAR_SMUGGLE_DECOR', STR )
                 IF ( STR == 'YES' .OR. STR == 'yes' ) THEN
                      AMPL = AMPL/BEAM_ATT
                      EFF_FRQ_PHS  = DECOR_BS 
                      EFF_FRQ_GRP  = DECOR_TS
                      EFF_FRQ_RAT  = BEAM_ATT
                      COV_PR_PH    = FRQ_AVR 
                      TEC          = PIM%OBS(IND_OBS)%ELEV(1)
                      TEC_RATE     = PIM%OBS(IND_OBS)%ELEV(2)
                      TEC_ERR      = PIM%OBS(IND_OBS)%AZ(1)
                      TEC_RATE_ERR = PIM%OBS(IND_OBS)%AZ(2)
                      WRITE ( 6, 113 ) PIM%STA(PIM%OBS(IND_OBS)%STA_IND(1))%ANT_DIAM, &
     &                                 PIM%STA(PIM%OBS(IND_OBS)%STA_IND(2))%ANT_DIAM
 113                  FORMAT ( 'Antenna diameters: ', F9.3, 2X, F9.3, ' meters' )
                 END IF
!
! -------------- Apply decorrelation factor on fringe amplitude for bandwidth and 
! -------------- time smearing. NB: we do not apply beam attenuation at this point.
! -------------- Correction for beam attenuation will be applied by splt.
!
                 AMPL    = AMPL/(DECOR_BS * DECOR_TS)
                 NOI_AVR = NOI_AVR/(DECOR_BS * DECOR_TS)
               ELSE
                 DECOR_BS = 1.0D0
                 DECOR_TS = 1.0D0
            END IF
            IF ( FL_FA_PRINT ) THEN
                 FEED_ANG_DIF = PIM%OBS(IND_OBS)%FEED_ANG(1) - PIM%OBS(IND_OBS)%FEED_ANG(2)
                 IF ( FEED_ANG_DIF < -PI__NUM ) FEED_ANG_DIF = FEED_ANG_DIF + PI2
                 IF ( FEED_ANG_DIF >  PI__NUM ) FEED_ANG_DIF = FEED_ANG_DIF - PI2
                 WRITE  ( 6, 115 ) PIM%C_STA(PIM%OBS(IND_OBS)%STA_IND(1)), &
     &                             PIM%OBS(IND_OBS)%FEED_ANG(1), &
     &                             PIM%C_STA(PIM%OBS(IND_OBS)%STA_IND(2)), &
     &                             PIM%OBS(IND_OBS)%FEED_ANG(2), &
     &                             FEED_ANG_DIF
 115             FORMAT ( 'Feed horn angles:  sta1: ',A, ' feed ang_1: ', F5.2, &
     &                    '  sta2: ',A, ' feed ang_2: ', F5.2, ' fa_dif: ', F5.2, ' rad' )
                 CALL FLUSH ( 6 )
            END IF
!
! --------- Get integral over the bandpass amplitude
!
            IF ( PIM%CONF%BANDPASS_USE == PIMA__BPASS_AMP      .OR. &
     &           PIM%CONF%BANDPASS_USE == PIMA__BPASS_AMP_PHS       ) THEN
                 IF ( FL_BPASS ) THEN
                      IND_STA_1 = PIM%OBS(IND_OBS)%STA_IND(1)
                      IND_STA_2 = PIM%OBS(IND_OBS)%STA_IND(2)
                      IF ( PIM%BPASS(1)%PIMA_VERS .GE. PIMA__BPS_AMP_VERS ) THEN
!
! ------------------------ Modern, post 2017.03.03 case
!
                           AMPL_INTG = PIM%BPASS(PIM%OBS(IND_OBS)%STA_IND(1))%INTG_AMPL* &
     &                                 PIM%BPASS(PIM%OBS(IND_OBS)%STA_IND(2))%INTG_AMPL
                         ELSE
!
! ------------------------ Legacy section
!
                           IF ( PIM%BPASS(IND_STA_1)%L_OBS > 0 .AND. &
     &                          PIM%BPASS(IND_STA_2)%L_OBS > 0       ) THEN
!
! ---------------------------- Neither station was the bandpass reference station
!

                                AMPL_INTG = SQRT ( PIM%BPASS(PIM%OBS(IND_OBS)%STA_IND(1))%INTG_AMPL* &
     &                                             PIM%BPASS(PIM%OBS(IND_OBS)%STA_IND(2))%INTG_AMPL  &
     &                                           )
                              ELSE IF ( PIM%BPASS(IND_STA_1)%L_OBS == 0 .AND. &
     &                                  PIM%BPASS(IND_STA_2)%L_OBS >  0       ) THEN
!
! ----------------------------- The first station was the bandpass reference station
!
                                AMPL_INTG = PIM%BPASS(PIM%OBS(IND_OBS)%STA_IND(2))%INTG_AMPL
                              ELSE IF ( PIM%BPASS(IND_STA_2)%L_OBS == 0 .AND. &
     &                                  PIM%BPASS(IND_STA_1)%L_OBS >  0       ) THEN
!
! ----------------------------- The second station was the bandpass reference station
!
                                AMPL_INTG = PIM%BPASS(PIM%OBS(IND_OBS)%STA_IND(1))%INTG_AMPL
                           END IF
                      END IF
                 END IF
              ELSE
                 AMPL_INTG = 1.0D0
            END IF
!
            CALL ERR_PASS ( IUER, IER )
            CALL PIMA_SB_SEARCH ( PIM, IND_OBS, PIM%NCHN, LFRQ, LTIM, &
     &                            PIM%FREQ_ARR(1,PIM%CONF%BEG_FRQ,PIM%CONF%FRQ_GRP), &
     &                            FREQ_REF, PIM%OBS(IND_OBS)%UV(1,1,1,J7), &
     &                            PIM%OBS(IND_OBS)%WEI_1D, PIM%OBS(IND_OBS)%AP_LEN, &
     &                            TIME_FRT, PH_RAT, GR_DEL(IND_FRA), PHAS(IND_FRA), &
     &                            AMPL(PIMA__DRF), SNR_FRA, SB_DEL, SB_DEL_ERR, IER )
            IF ( IER .NE. 0 ) THEN
                 CALL ERR_LOG ( 7536, IUER, 'PIMA_FRINGE', 'Failure in '// &
     &               'an attempt to compute single band delay' )
                 RETURN
            END IF
!
            FL_NOC = .FALSE.
            IF ( SNR_FRA > PIM%CONF%FRIB_SNR_DETECTION ) THEN
                 CALL PIMA_FRINGE_RES ( PIM, IND_OBS, PIM%NCHN, LFRQ, LTIM, &
     &                                  PIM%FREQ_ARR(1,PIM%CONF%BEG_FRQ,PIM%CONF%FRQ_GRP), &
     &                                  FREQ_REF, PIM%OBS(IND_OBS)%UV(1,1,1,J7), &
     &                                  PIM%OBS(IND_OBS)%WEI_1D, PIM%OBS(IND_OBS)%AP_LEN, &
     &                                  TIME_FRT, PH_RAT(IND_FRA), GR_DEL(IND_FRA), &
     &                                  GR_RAT, PHAS(IND_FRA), AMPL(IND_FRA), IER )
                 IF ( IER .NE. 0 ) THEN
                      CALL ERR_LOG ( 7537, IUER, 'PIMA_FRINGE', 'Failure in '// &
     &                    'an attempt to compute fringe residuals' )
                      RETURN
                 END IF
!
! -------------- Check whether the amplitude at the channel is very different
! -------------- from the scan-averaged amplitude
!
                 DO 480 J8=PIM%CONF%BEG_FRQ,PIM%CONF%END_FRQ
                    IF ( ABS(PIM%OBS(IND_OBS)%RES_FRN(J8,1)) < AMPL(PIMA__DRF)*PIM__D_FACT ) THEN
                         FL_NOC = .TRUE.
                    END IF
 480             CONTINUE
            END IF
!
            IF ( FL_BPASS ) THEN
!
! -------------- Apply bandpass filter to amplitudes 
!
                 IFRQ = 0
                 DO 490 J9=PIM%CONF%BEG_FRQ,PIM%CONF%END_FRQ
                    IFRQ = IFRQ + 1
                    DO 4100 J10=1,PIM%NCHN
                       IF ( FL_BPASS ) THEN
                            IF ( PIM%CONF%BANDPASS_USE == PIMA__BPASS_AMP_PHS  .OR. &
     &                           PIM%CONF%BANDPASS_USE == PIMA__BPASS_AMP           ) THEN
                                 IF ( PIM%BPASS(1)%PIMA_VERS .GE. PIMA__BPS_AMP_VERS ) THEN
!
! ----------------------------------- Modern, post 2017.03.03 case
!
                                      BPASS_C8 = CMPLX ( 1.0/( &
     &                                                         ABS( PIM%BPASS(IND_STA_1)%BPS(J10,J9) )* &
     &                                                         ABS( PIM%BPASS(IND_STA_2)%BPS(J10,J9) )  &
     &                                                  ), 0.0 )
                                      IF ( ASSOCIATED ( PIM%PBP ) ) THEN
                                           IF ( PIM%CONF%POLAR .EQ. PIMA__POLAR_LL     .AND. &
     &                                          ASSOCIATED ( PIM%PBP(IND_STA_1)%CMPL ) .AND. &
     &                                          ASSOCIATED ( PIM%PBP(IND_STA_2)%CMPL )       ) THEN
!
! --------------------------------------------- Case of LL polarizaation
!
                                                BPASS_C8 = BPASS_C8 *CMPLX ( 1.0/( &
     &                                                                       ABS( PIM%PBP(IND_STA_1)%CMPL(J10,J9) )* &
     &                                                                       ABS( PIM%PBP(IND_STA_2)%CMPL(J10,J9) )  &
     &                                                                     ), 0.0 )
                                           END IF
                                           IF ( PIM%CONF%POLAR .EQ. PIMA__POLAR_I     .AND. &
     &                                          ASSOCIATED ( PIM%PBP(IND_STA_1)%CMPL ) .AND. &
     &                                          ASSOCIATED ( PIM%PBP(IND_STA_2)%CMPL )       ) THEN
!
! --------------------------------------------- Case of LL polarizaation
!
                                                BPASS_C8 = BPASS_C8 *CMPLX ( 1.0/( &
     &                                                                       SQRT ( ABS( PIM%PBP(IND_STA_1)%CMPL(J10,J9) )* &
     &                                                                              ABS( PIM%PBP(IND_STA_2)%CMPL(J10,J9) )  ) &
     &                                                                     ), 0.0 )
                                           END IF
                                      END IF
                                    ELSE
!
! ----------------------------------- Legacy case: the bandpass for a reference station was
! ----------------------------------- set to (1.0, 0.0). Parameter LOBS for the bandpass
! ----------------------------------- reference station is 0
!
                                      IND_STA_1 = PIM%OBS(IND_OBS)%STA_IND(1)
                                      IND_STA_2 = PIM%OBS(IND_OBS)%STA_IND(2)
                                      IF ( PIM%BPASS(IND_STA_1)%L_OBS > 0 .AND. &
     &                                     PIM%BPASS(IND_STA_2)%L_OBS > 0       ) THEN
!
! ---------------------------------------- Neither station was the bandpass reference station
!
                                           BPASS_C8 = CMPLX ( 1.0/SQRT( &
     &                                                ABS( PIM%BPASS(IND_STA_1)%BPS(J10,J9) )* &
     &                                                ABS( PIM%BPASS(IND_STA_2)%BPS(J10,J9) ) &
     &                                                                ), 0.0 )
                                        ELSE IF ( PIM%BPASS(IND_STA_1)%L_OBS == 0 .AND. &
     &                                            PIM%BPASS(IND_STA_2)%L_OBS >  0       ) THEN
!
! --------------------------------------- The first station was the bandpass reference station
!
                                           BPASS_C8 = CMPLX ( &
     &                                           1.0/ABS( PIM%BPASS(IND_STA_2)%BPS(J10,J9) ), 0.0 )
                                        ELSE IF ( PIM%BPASS(IND_STA_2)%L_OBS == 0 .AND. &
     &                                            PIM%BPASS(IND_STA_1)%L_OBS >  0       ) THEN
!
! ---------------------------------------- The second station was the bandpass reference station
!
                                           BPASS_C8 = CMPLX ( &
     &                                           1.0/ABS( PIM%BPASS(IND_STA_1)%BPS(J10,J9) ), 0.0 )
                                        ELSE
!
! --------------------------------------- This is a pathological case
!
                                           BPASS_C8 = CMPLX ( 1.0, 0.0 )
                                      END IF
                                 END IF
                               ELSE
                                 BPASS_C8 = CMPLX ( 1.0, 0.0 )
                            END IF
                          ELSE
                            BPASS_C8 = CMPLX ( 1.0, 0.0 )
                       END IF
                       DO 4110 J11=1,LTIM
                          PIM%OBS(IND_OBS)%UV(J10,IFRQ,J11,J7) = PIM%OBS(IND_OBS)%UV(J10,IFRQ,J11,J7)*BPASS_C8
 4110                  CONTINUE
 4100               CONTINUE
 490             CONTINUE
            END IF
!
 870        CONTINUE
            FRI_STS = IBSET ( FRI_STS, FRI__PIM )
            IF ( SNR_FRA == 0.0D0 ) THEN
                 FRI_STS = IBSET ( FRI_STS, FAI__PIM )
            END IF
            IF ( SNR_FRA < PIM%CONF%FRIB_SNR_DETECTION ) THEN
                 FRI_STS = IBSET ( FRI_STS, NDT__PIM )
            END IF
            IF ( FL_NOC ) FRI_STS = IBSET ( FRI_STS, FRI__PIM )
!
            IF ( IER == 0 .AND. AMPL(PIMA__DRF) > 1.0 ) THEN
                 IF ( PIM%CONF%DEBUG_LEVEL .GE. 1 ) THEN
                      WRITE ( 6, 224 ) IND_OBS, AMPL(PIMA__DRF)
 224                  FORMAT ( 'PIMA_FRINGE: Too big fringe amplitude ', &
     &                         'for observation # ', I6, ' : ', 1PD14.6 )
                      CALL FLUSH ( 6 )
                 END IF
                 AMPL = 1.1D0
                 FRI_STS = IBSET ( FRI_STS, FAI__PIM )
            END IF
!
! --------- Check, whether we have to set flag: "PHASE-CAL MISSED".
! --------- This should be done if for at least one of the stations
! --------- a) Phase calibration is to be applied
! --------- b) Phase calibration is generally available for that station
! --------- c) Phase calibration is not available for this specific observation
!
            IF ( PIM%CONF%PHAS_CAL_CODE == PIMA__PCAL_USE     .OR. &
     &           PIM%CONF%PHAS_CAL_CODE == PIMA__PCAL_USE_ONE .OR. &
     &           PIM%CONF%PHAS_CAL_CODE == PIMA__PCAL_USE_ALL      ) THEN
                 DO 4120 J12=1,2
                    FL_PCAL_TOUSE = .TRUE.
                    IF ( PIM%CONF%L_PUS > 0 ) THEN
!
! ---------------------- Fine-grained pcal selection
!
                         IND_PUS_STA = LTM_DIF ( 0, PIM%CONF%L_PUS, &
     &                                           PIM%CONF%PCAL_USE_STA, &
     &                                           PIM%C_STA(PIM%OBS(IND_OBS)%STA_IND(J12)) )
                         IF ( PIM%CONF%PUS_TYPE == PIMA__USE ) THEN
!
! --------------------------- The fine-grained list sets the list of station to use pcal
!
                              IF ( IND_PUS_STA .LT. 1 ) FL_PCAL_TOUSE = .FALSE.
                            ELSE IF ( PIM%CONF%PUS_TYPE == PIMA__NOT_USE ) THEN
!
! --------------------------- The fine-grained list sets the list of station not to use pcal
!
                              IF ( IND_PUS_STA .GE. 1 ) FL_PCAL_TOUSE = .FALSE.
                         END IF
                    END IF
!
                    IF ( PIM%STA(PIM%OBS(IND_OBS)%STA_IND(J12))%PCAL(PIM%CONF%FRQ_GRP)%PCAL_AVAIL .AND. &
     &                   PIM%STA(PIM%OBS(IND_OBS)%STA_IND(J12))%PCAL(PIM%CONF%FRQ_GRP)%PCAL_USE   .AND. &
     &                   FL_PCAL_TOUSE                                                                  ) THEN
                         IF ( PIM%OBS(IND_OBS)%PCAL_IND(1,J12,PIM%CONF%FRQ_GRP) == 0 ) THEN
                              FRI_STS = IBSET ( FRI_STS, NPC__PIM )
                         ENDIF
                    END IF
 4120            CONTINUE
            END IF
            IF ( PIM%CONF%FRIB_FINE_SEARCH == PIMA__FINE_SEARCH_ACC ) THEN
                 GR_RAT     = -1.0D0
                 GR_RAT_ERR = -1.0D0
               ELSE 
                 PH_ACC     = -1.0D0
                 PH_ACC_ERR = -1.0D0
            END IF
!
! --------- Write down results of fringe search
!
            IF ( PIM%CONF%POLARCAL_FILE .NE. PIMA__POLARCAL_NO ) THEN
                 IF ( PIM%CONF%POLAR .NE. PIMA__POLAR_ORIG ) THEN
                      PA_USED = PIM%OBS(IND_OBS)%FEED_ANG(1) - PIM%OBS(IND_OBS)%FEED_ANG(2)
                    ELSE 
                      PA_USED = 0.0D0
                 END IF 
               ELSE
                 PA_USED = 0.0D0
            END IF
!
            CALL ERR_PASS ( IUER, IER )
            CALL PIMA_FRI_WRI_OBS ( PIM, LUN_FRI, IND_OBS, SNR_FRA, AMPL, AMPL_INTG, &
     &                              NOI_AVR, TIME_FRT, GR_DEL,                   &
     &                              PH_RAT, GR_RAT, PH_ACC, SB_DEL, PHAS,        &
     &                              GR_DEL_ERR, PH_RAT_ERR, GR_RAT_ERR,          &
     &                              PH_ACC_ERR, SB_DEL_ERR, PH_DEL_ERR, GRAMBSP, &
     &                              PIM%OBS(IND_OBS)%AP_LEN, EFF_DURA, FREQ_REF, &
     &                              EFF_FRQ_PHS, EFF_FRQ_GRP, EFF_FRQ_RAT,       &
     &                              COV_PR_PH, COV_GR_MD,                        &
     &                              TEC, TEC_RATE, TEC_ERR, TEC_RATE_ERR,        &
     &                              PIM%OBS(IND_OBS)%POLAR_USED, PA_USED,        &
     &                              DECOR_TIM, PIM%OBS(IND_OBS)%PCAL_GDEL, &
     &                              FRI_STS, IER )
            IF ( IER .NE. 0 ) THEN
                 CALL ERR_LOG ( 7538, IUER, 'PIMA_FRINGE', 'Failure to write '// &
     &                'output file with results of fringing '// &
     &                PIM%CONF%FRINGE_FILE(1:I_LEN(PIM%CONF%FRINGE_FILE))// &
     &                ' -- error '//STR )
                 RETURN
            END IF
!
            CALL ERR_PASS ( IUER, IER )
            CALL PIMA_RESID_WRI_OBS ( PIM, LUN_RES, IND_OBS, SNR_FRA, AMPL, &
     &                                FRI_STS, IER )
            IF ( IER .NE. 0 ) THEN
                 CALL ERR_LOG ( 7539, IUER, 'PIMA_FRINGE', 'Failure to write '// &
     &                'output file with fringe residuals  '// &
     &                PIM%CONF%FRIRES_FILE(1:I_LEN(PIM%CONF%FRIRES_FILE))// &
     &                ' -- error '//STR )
                 RETURN
            END IF
!
            IF ( PIM%CONF%DEBUG_LEVEL .GE. 1 ) THEN
                 WRITE ( UNIT=6, FMT=130, IOSTAT=IER ) PIM%CONF%SESS_CODE(1:12), &
     &                   IND_OBS, GET_CDATE(), &
     &                   PIM%STA(PIM%OBS(IND_OBS)%STA_IND(1))%IVS_NAME, &
     &                   PIM%STA(PIM%OBS(IND_OBS)%STA_IND(2))%IVS_NAME, &
     &                   PIM%SOU(PIM%OBS(IND_OBS)%SOU_IND)%IVS_NAME, &
     &                   SNR_FRA
 130             FORMAT ( A, 1X, I6,')', 1X, A, 1X, A, '/', A, 1X, A, &
     &                    ' SNR=', F7.1 )
                 CALL FLUSH ( 6 )
            END IF
!
! --------- Deterine polarization label
!
            IF ( POL_MODE == PIMA__PALL_NOR ) THEN
                 IF ( J7 == 1 ) THEN
                      POL_LAB_IND(1) = PIM%STA(PIM%OBS(IND_OBS)%STA_IND(1))%POL_TYP(1)
                      POL_LAB_IND(2) = PIM%STA(PIM%OBS(IND_OBS)%STA_IND(2))%POL_TYP(1)
                    ELSE IF ( J7 == 2 ) THEN
                      POL_LAB_IND(1) = PIM%STA(PIM%OBS(IND_OBS)%STA_IND(1))%POL_TYP(2)
                      POL_LAB_IND(2) = PIM%STA(PIM%OBS(IND_OBS)%STA_IND(2))%POL_TYP(1)
                    ELSE IF ( J7 == 3 ) THEN
                      POL_LAB_IND(1) = PIM%STA(PIM%OBS(IND_OBS)%STA_IND(1))%POL_TYP(1)
                      POL_LAB_IND(2) = PIM%STA(PIM%OBS(IND_OBS)%STA_IND(2))%POL_TYP(2)
                    ELSE IF ( J7 == 4 ) THEN
                      POL_LAB_IND(1) = PIM%STA(PIM%OBS(IND_OBS)%STA_IND(1))%POL_TYP(2)
                      POL_LAB_IND(2) = PIM%STA(PIM%OBS(IND_OBS)%STA_IND(2))%POL_TYP(2)
                 END IF
                 POL_BAS_LABEL = PIMA__POL_STR(POL_LAB_IND(1):POL_LAB_IND(1))// &
     &                           PIMA__POL_STR(POL_LAB_IND(2):POL_LAB_IND(2))
               ELSE IF ( POL_MODE == PIMA__PALL_XY ) THEN
                 IF ( J7 == 1 ) THEN
                      POL_LAB_IND(1) = PIM%STA(PIM%OBS(IND_OBS)%STA_IND(1))%POL_TYP(1)
                      POL_LAB_IND(2) = PIM%STA(PIM%OBS(IND_OBS)%STA_IND(2))%POL_TYP(1)
                    ELSE IF ( J7 == 2 ) THEN
                      POL_LAB_IND(1) = PIM%STA(PIM%OBS(IND_OBS)%STA_IND(1))%POL_TYP(2)
                      POL_LAB_IND(2) = PIM%STA(PIM%OBS(IND_OBS)%STA_IND(2))%POL_TYP(1)
                    ELSE IF ( J7 == 3 ) THEN
                      POL_LAB_IND(1) = PIM%STA(PIM%OBS(IND_OBS)%STA_IND(1))%POL_TYP(1)
                      POL_LAB_IND(2) = PIM%STA(PIM%OBS(IND_OBS)%STA_IND(2))%POL_TYP(2)
                    ELSE IF ( J7 == 4 ) THEN
                      POL_LAB_IND(1) = PIM%STA(PIM%OBS(IND_OBS)%STA_IND(1))%POL_TYP(2)
                      POL_LAB_IND(2) = PIM%STA(PIM%OBS(IND_OBS)%STA_IND(2))%POL_TYP(2)
                 END IF
                 IF ( POL_LAB_IND(1) == PIMA__POL_H ) POL_LAB_IND(1) = PIMA__POL_X
                 IF ( POL_LAB_IND(1) == PIMA__POL_V ) POL_LAB_IND(1) = PIMA__POL_Y
                 IF ( POL_LAB_IND(2) == PIMA__POL_H ) POL_LAB_IND(2) = PIMA__POL_X
                 IF ( POL_LAB_IND(2) == PIMA__POL_V ) POL_LAB_IND(2) = PIMA__POL_Y
                 POL_BAS_LABEL = PIMA__POL_STR(POL_LAB_IND(1):POL_LAB_IND(1))// &
     &                           PIMA__POL_STR(POL_LAB_IND(2):POL_LAB_IND(2))
               ELSE IF ( POL_MODE == PIMA__PALL_1ST ) THEN
                 IF ( J7 == 1 ) THEN
                      POL_LAB_IND(1) = PIM%STA(PIM%OBS(IND_OBS)%STA_IND(1))%POL_TYP(1)
                      POL_LAB_IND(2) = PIM%STA(PIM%OBS(IND_OBS)%STA_IND(2))%POL_TYP(1)
                    ELSE IF ( J7 == 2 ) THEN
                      POL_LAB_IND(1) = PIM%STA(PIM%OBS(IND_OBS)%STA_IND(1))%POL_TYP(2)
                      POL_LAB_IND(2) = PIM%STA(PIM%OBS(IND_OBS)%STA_IND(2))%POL_TYP(1)
                    ELSE IF ( J7 == 3 ) THEN
                      POL_LAB_IND(1) = PIM%STA(PIM%OBS(IND_OBS)%STA_IND(1))%POL_TYP(1)
                      POL_LAB_IND(2) = PIM%STA(PIM%OBS(IND_OBS)%STA_IND(2))%POL_TYP(2)
                    ELSE IF ( J7 == 4 ) THEN
                      POL_LAB_IND(1) = PIM%STA(PIM%OBS(IND_OBS)%STA_IND(1))%POL_TYP(2)
                      POL_LAB_IND(2) = PIM%STA(PIM%OBS(IND_OBS)%STA_IND(2))%POL_TYP(2)
                 END IF
                 IF ( POL_LAB_IND(1) == PIMA__POL_H ) POL_LAB_IND(1) = PIMA__POL_H1
                 IF ( POL_LAB_IND(1) == PIMA__POL_V ) POL_LAB_IND(1) = PIMA__POL_V1
                 IF ( POL_LAB_IND(2) == PIMA__POL_H ) POL_LAB_IND(2) = PIMA__POL_H1
                 IF ( POL_LAB_IND(2) == PIMA__POL_V ) POL_LAB_IND(2) = PIMA__POL_V1
                 POL_BAS_LABEL = PIMA__POL_STR(POL_LAB_IND(1):POL_LAB_IND(1))// &
     &                           PIMA__POL_STR(POL_LAB_IND(2):POL_LAB_IND(2))
               ELSE IF ( POL_MODE == PIMA__PALL_2ND ) THEN
                 IF ( J7 == 1 ) THEN
                      POL_LAB_IND(1) = PIM%STA(PIM%OBS(IND_OBS)%STA_IND(1))%POL_TYP(1)
                      POL_LAB_IND(2) = PIM%STA(PIM%OBS(IND_OBS)%STA_IND(2))%POL_TYP(1)
                    ELSE IF ( J7 == 2 ) THEN
                      POL_LAB_IND(1) = PIM%STA(PIM%OBS(IND_OBS)%STA_IND(1))%POL_TYP(2)
                      POL_LAB_IND(2) = PIM%STA(PIM%OBS(IND_OBS)%STA_IND(2))%POL_TYP(1)
                    ELSE IF ( J7 == 3 ) THEN
                      POL_LAB_IND(1) = PIM%STA(PIM%OBS(IND_OBS)%STA_IND(1))%POL_TYP(1)
                      POL_LAB_IND(2) = PIM%STA(PIM%OBS(IND_OBS)%STA_IND(2))%POL_TYP(2)
                    ELSE IF ( J7 == 4 ) THEN
                      POL_LAB_IND(1) = PIM%STA(PIM%OBS(IND_OBS)%STA_IND(1))%POL_TYP(2)
                      POL_LAB_IND(2) = PIM%STA(PIM%OBS(IND_OBS)%STA_IND(2))%POL_TYP(2)
                 END IF
                 IF ( POL_LAB_IND(1) == PIMA__POL_H ) POL_LAB_IND(1) = PIMA__POL_H2
                 IF ( POL_LAB_IND(1) == PIMA__POL_V ) POL_LAB_IND(1) = PIMA__POL_V2
                 IF ( POL_LAB_IND(2) == PIMA__POL_H ) POL_LAB_IND(2) = PIMA__POL_H2
                 IF ( POL_LAB_IND(2) == PIMA__POL_V ) POL_LAB_IND(2) = PIMA__POL_V2
                 POL_BAS_LABEL = PIMA__POL_STR(POL_LAB_IND(1):POL_LAB_IND(1))// &
     &                           PIMA__POL_STR(POL_LAB_IND(2):POL_LAB_IND(2))
               ELSE
                 POL_BAS_LABEL = PIM%CONF%POLAR(1:I_LEN(PIM%CONF%POLAR))
            END IF
!
            IF ( SNR_FRA > 0.0D0 .AND. &
     &           ( PIM%CONF%FRIB_1D_RESFRQ_PLOT == PIMA__PLOT_PS  .OR. &
     &             PIM%CONF%FRIB_1D_RESFRQ_PLOT == PIMA__PLOT_GIF .OR. &
     &             PIM%CONF%FRIB_1D_RESFRQ_PLOT == PIMA__PLOT_XW  .OR. &
     &             PIM%CONF%FRIB_1D_RESFRQ_PLOT == PIMA__PLOT_TXT      ) ) THEN
!
                 FINAM_1D_PLOT = 'fr1d_frq_'// &
     &                      PIM%SCA(PIM%OBS(IND_OBS)%SCA_IND)%SCAN_NAME(1:9)// &
     &                      '_'//PIM%CONF%BAND//'_'// &
     &                      PIM%STA(PIM%OBS(IND_OBS)%STA_IND(1))%IVS_NAME//'_'// &
     &                      PIM%STA(PIM%OBS(IND_OBS)%STA_IND(2))%IVS_NAME//'_'
                 CALL TRAN ( 12, FINAM_1D_PLOT, FINAM_1D_PLOT )
                 CALL BLANK_TO_UNDERSCORE ( FINAM_1D_PLOT(1:I_LEN(FINAM_1D_PLOT)) )
                 IF ( ILEN(DIRNAM) > 0 ) THEN
                      FINAM_1D_PLOT = DIRNAM(1:I_LEN(DIRNAM))//FINAM_1D_PLOT
                 END IF
!
                 IF ( PIM%CONF%FRIB_1D_RESFRQ_PLOT == PIMA__PLOT_XW ) THEN
                      IDEV = 0
                   ELSE IF ( PIM%CONF%FRIB_1D_RESFRQ_PLOT == PIMA__PLOT_PS  ) THEN
                      IDEV = 8
                   ELSE IF ( PIM%CONF%FRIB_1D_RESFRQ_PLOT == PIMA__PLOT_GIF ) THEN
                      IDEV = 11
                   ELSE IF ( PIM%CONF%FRIB_1D_RESFRQ_PLOT == PIMA__PLOT_TXT ) THEN
                      IDEV = -1
                   ELSE IF ( PIM%CONF%FRIB_1D_RESFRQ_PLOT == PIMA__PLOT_SAV ) THEN
                      IDEV = 100
                   ELSE
                      IDEV = 0
                 END IF
!
                 IF ( INDEX ( STR_UV_PRINT, 'FIN1' ) > 0 ) THEN
                      CALL PIMA_UV_PRINT ( PIM, IND_OBS, 1, 'PF-FIN1' )
                    ELSE IF ( INDEX ( STR_UV_PRINT, 'FIN2' ) > 0 ) THEN
                      CALL PIMA_UV_PRINT ( PIM, IND_OBS, 2, 'PF-FIN2' )
                 END IF
!
                 CALL ERR_PASS ( IUER, IER )
                 IF ( PIM%CONF%FRIB_AUTOCORR_CALIB .NE. PIMA__ACCR_NO ) THEN
                      CALL PIMA_FR1D_FRQ_PLOT ( PIM, IND_OBS, FINAM_1D_PLOT, LTIM, &
     &                                     PIM%NCHN, LFRQ, DECOR_BS*DECOR_TS, &
     &                                     PIM%FREQ_ARR(1,PIM%CONF%BEG_FRQ,PIM%CONF%FRQ_GRP), &
     &                                     FREQ_REF, PIM%OBS(IND_OBS)%WEI_1D, &
     &                                     PIM%OBS(IND_OBS)%UV(1,1,1,J7), &
     &                                     PIM%OBS(IND_OBS)%AC(1,1,1,1,J7), &
     &                                     PIM%OBS(IND_OBS)%AC_MEAN, &
     &                                     TIME_FRT, GR_DEL(IND_FRA), PH_RAT(IND_FRA), &
     &                                     GR_RAT, PH_ACC, PHAS(IND_FRA), AMPL(IND_FRA), &
     &                                     SB_DEL, SNR_FRA, POL_BAS_LABEL, IDEV, IER )
                      IF ( ASSOCIATED ( PIM%OBS(IND_OBS)%AC ) ) THEN
                           DEALLOCATE ( PIM%OBS(IND_OBS)%AC )
                      END IF
                    ELSE
                      CALL PIMA_FR1D_FRQ_PLOT ( PIM, IND_OBS, FINAM_1D_PLOT, LTIM, &
     &                                     PIM%NCHN, LFRQ, DECOR_BS*DECOR_TS, &
     &                                     PIM%FREQ_ARR(1,PIM%CONF%BEG_FRQ,PIM%CONF%FRQ_GRP), &
     &                                     FREQ_REF, PIM%OBS(IND_OBS)%WEI_1D, &
     &                                     PIM%OBS(IND_OBS)%UV(1,1,1,J7), &
     &                                     PIM%OBS(IND_OBS)%AC(1,1,1,1,J7), &
     &                                     %VAL(0), &
     &                                     TIME_FRT, GR_DEL(IND_FRA), PH_RAT(IND_FRA), &
     &                                     GR_RAT, PH_ACC, PHAS(IND_FRA), AMPL(IND_FRA), &
     &                                     SB_DEL, SNR_FRA, POL_BAS_LABEL, IDEV, IER )
                 END IF
                 IF ( IER .NE. 0 ) THEN
                      CALL ERR_PASS ( IUER, IER )
                      CALL ERR_LOG  ( 7540, IER, 'PIMA_FRINGE', 'Failure in '// &
     &                    'an attempt to make 1D RESFRQ fringe plot' )
                      IF ( PIM%CONF%FRINGE_ERRORS == PIMA__ERR_STOP ) THEN
                           CALL ERR_PASS ( IER, IUER )
                           RETURN
                        ELSE
                          GOTO 820
                      END IF
                 END IF
            END IF
!
            IF ( SNR_FRA > 0.0D0 .AND. &
                 ( PIM%CONF%FRIB_1D_RESTIM_PLOT == PIMA__PLOT_PS  .OR. &
     &             PIM%CONF%FRIB_1D_RESTIM_PLOT == PIMA__PLOT_GIF .OR. &
     &             PIM%CONF%FRIB_1D_RESTIM_PLOT == PIMA__PLOT_XW  .OR. &
     &             PIM%CONF%FRIB_1D_RESTIM_PLOT == PIMA__PLOT_TXT      ) ) THEN
!
                 FINAM_1D_PLOT = 'fr1d_tim_'// &
     &                      PIM%SCA(PIM%OBS(IND_OBS)%SCA_IND)%SCAN_NAME(1:9)// &
     &                      '_'//PIM%CONF%BAND//'_'// &
     &                      PIM%STA(PIM%OBS(IND_OBS)%STA_IND(1))%IVS_NAME//'_'// &
     &                      PIM%STA(PIM%OBS(IND_OBS)%STA_IND(2))%IVS_NAME//'_'
                 CALL TRAN ( 12, FINAM_1D_PLOT, FINAM_1D_PLOT )
                 CALL BLANK_TO_UNDERSCORE ( FINAM_1D_PLOT(1:I_LEN(FINAM_1D_PLOT)) )
                 IF ( ILEN(DIRNAM) > 0 ) THEN
                      FINAM_1D_PLOT = DIRNAM(1:I_LEN(DIRNAM))//FINAM_1D_PLOT
                 END IF
!
                 IF ( PIM%CONF%FRIB_1D_RESTIM_PLOT == PIMA__PLOT_XW ) THEN
                      IDEV = 0
                   ELSE IF ( PIM%CONF%FRIB_1D_RESTIM_PLOT  == PIMA__PLOT_PS  ) THEN
                      IDEV = 8
                   ELSE IF ( PIM%CONF%FRIB_1D_RESTIM_PLOT  == PIMA__PLOT_GIF ) THEN
                      IDEV = 11
                   ELSE IF ( PIM%CONF%FRIB_1D_RESTIM_PLOT  == PIMA__PLOT_TXT ) THEN
                      IDEV = -1
                   ELSE IF ( PIM%CONF%FRIB_1D_RESFRQ_PLOT == PIMA__PLOT_SAV ) THEN
                      IDEV = 100
                   ELSE
                      IDEV = 0
                 END IF
!
                 CALL ERR_PASS ( IUER, IER )
                 CALL PIMA_FR1D_TIM_PLOT ( PIM, IND_OBS, FINAM_1D_PLOT, LTIM, &
     &                                     PIM%NCHN, LFRQ, DECOR_BS*DECOR_TS, &
     &                                     PIM%FREQ_ARR(1,PIM%CONF%BEG_FRQ,PIM%CONF%FRQ_GRP), &
     &                                     FREQ_REF, PIM%OBS(IND_OBS)%WEI_1D, &
     &                                     PIM%OBS(IND_OBS)%UV(1,1,1,J7), &
     &                                     TIME_FRT, PIM%OBS(IND_OBS)%AP_LEN, &
     &                                     GR_DEL(IND_FRA), PH_RAT(IND_FRA), &
     &                                     GR_RAT, PH_ACC, PHAS(IND_FRA), AMPL(IND_FRA), &
     &                                     SNR_FRA, POL_BAS_LABEL, IDEV, IER )
                 IF ( IER .NE. 0 ) THEN
                      CALL ERR_PASS ( IUER, IER )
                      CALL ERR_LOG  ( 7541, IER, 'PIMA_FRINGE', 'Failure in '// &
     &                    'an attempt to make 1D RESTIM fringe plot' )
                      IF ( PIM%CONF%FRINGE_ERRORS == PIMA__ERR_STOP ) THEN
                           CALL ERR_PASS ( IER, IUER )
                           RETURN
                        ELSE
                          GOTO 820
                      END IF
                 END IF
            END IF
!
            IF ( SNR_FRA > 0.0D0 .AND. &
                ( PIM%CONF%FRIB_1D_DRF_PLOT == PIMA__PLOT_PS  .OR. &
     &            PIM%CONF%FRIB_1D_DRF_PLOT == PIMA__PLOT_GIF .OR. &
     &            PIM%CONF%FRIB_1D_DRF_PLOT == PIMA__PLOT_XW  .OR. &
     &            PIM%CONF%FRIB_1D_DRF_PLOT == PIMA__PLOT_SAV .OR. &
     &            PIM%CONF%FRIB_1D_DRF_PLOT == PIMA__PLOT_TXT      ) ) THEN
!
                 FINAM_1D_PLOT = 'fr1d_drf_'// &
     &                      PIM%SCA(PIM%OBS(IND_OBS)%SCA_IND)%SCAN_NAME(1:9)// &
     &                      '_'//PIM%CONF%BAND//'_'// &
     &                      PIM%STA(PIM%OBS(IND_OBS)%STA_IND(1))%IVS_NAME//'_'// &
     &                      PIM%STA(PIM%OBS(IND_OBS)%STA_IND(2))%IVS_NAME//'_'
                 CALL TRAN ( 12, FINAM_1D_PLOT, FINAM_1D_PLOT )
                 CALL BLANK_TO_UNDERSCORE ( FINAM_1D_PLOT(1:I_LEN(FINAM_1D_PLOT)) )
                 IF ( ILEN(DIRNAM) > 0 ) THEN
                      FINAM_1D_PLOT = DIRNAM(1:I_LEN(DIRNAM))//FINAM_1D_PLOT
                 END IF
!
                 IF ( PIM%CONF%FRIB_1D_DRF_PLOT == PIMA__PLOT_XW ) THEN
                      IDEV = 0
                   ELSE IF ( PIM%CONF%FRIB_1D_DRF_PLOT == PIMA__PLOT_PS  ) THEN
                      IDEV = 8
                   ELSE IF ( PIM%CONF%FRIB_1D_DRF_PLOT == PIMA__PLOT_GIF ) THEN
                      IDEV = 11
                   ELSE IF ( PIM%CONF%FRIB_1D_DRF_PLOT == PIMA__PLOT_SAV ) THEN
                      IDEV = 100
                   ELSE IF ( PIM%CONF%FRIB_1D_DRF_PLOT == PIMA__PLOT_TXT ) THEN
                      IDEV = -1
                   ELSE
                      IDEV = 0
                 END IF
!
                 CALL ERR_PASS ( IUER, IER )
                 CALL PIMA_FR1D_DRF_PLOT ( PIM, IND_OBS, FINAM_1D_PLOT, LTIM, &
     &                                     PIM%NCHN, LFRQ, &
     &                                     PIM%FREQ_ARR(1,PIM%CONF%BEG_FRQ,PIM%CONF%FRQ_GRP), &
     &                                     FREQ_REF, PIM%OBS(IND_OBS)%WEI_1D, &
     &                                     PIM%OBS(IND_OBS)%UV(1,1,1,J7), &
     &                                     TIME_FRT, PIM%OBS(IND_OBS)%AP_LEN,  &
     &                                     GRAMBSP, GR_DEL(IND_FRA), PH_RAT(IND_FRA), &
     &                                     GR_RAT, PH_ACC, PHAS(IND_FRA), AMPL(IND_FRA), &
     &                                     SNR_FRA, IDEV, IER )
                 IF ( IER .NE. 0 ) THEN
                      CALL ERR_PASS ( IUER, IER )
                      CALL ERR_LOG  ( 7542, IER, 'PIMA_FRINGE', 'Failure in '// &
     &                    'an attempt to make 1D DRF plot' )
                      IF ( PIM%CONF%FRINGE_ERRORS == PIMA__ERR_STOP ) THEN
                           CALL ERR_PASS ( IER, IUER )
                           RETURN
                        ELSE
                          GOTO 820
                      END IF
                 END IF
            END IF
 470     CONTINUE 
 820     CONTINUE
         IND_OBS_PREV = IND_OBS
 420  CONTINUE
      IF ( PIM%CONF%FRT_USE == PIMA__FRT_FILE ) THEN
           IF ( ALLOCATED ( TIME_FRT_OFFSET_ARR ) ) DEALLOCATE ( TIME_FRT_OFFSET_ARR )
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL PIMA_FRI_WRI_TAIL ( PIM, LUN_FRI, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 7543, IER, 'PIMA_FRINGE', 'Failure in '// &
     &         'an attempt to write in to the fringe output table' )
           RETURN
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL PIMA_RESID_WRI_TAIL ( PIM, LUN_RES, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 7544, IER, 'PIMA_FRINGE', 'Failure in '// &
     &         'an attempt to write in to the fringe residual output table' )
           RETURN
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  PIMA_FRINGE  !#!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE PIMA_UV_PRINT ( PIM, IND_OBS, IND_POL, LABEL )
! ************************************************************************
! *                                                                      *
! *   Auxilliary routine UF_PRINT prints visibilities and weigths for    *
! *   the specific observation.
! *                                                                      *
! *  ### 16-APR-2020 PIMA_UV_PRINT v1.0 (c)  L. Petrov  16-APR-2020 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'pima.i'
      TYPE     ( PIMA__TYPE  ) :: PIM
      INTEGER*4  IND_OBS, IND_POL
      CHARACTER  LABEL*(*)
      INTEGER*4  J1, J2, J3, I_FRQ
      REAL*4,    EXTERNAL :: ATAN_CS_R4, PHAS_CMPL_R4
!
      DO 410 J1=1,PIM%OBS(IND_OBS)%NUM_EPC(PIM%OBS(IND_OBS)%REF_FRG_INDS(PIM%CONF%FRQ_GRP))
         WRITE ( 6, 210 ) LABEL, IND_OBS, J1, IND_POL, PIM%OBS(IND_OBS)%WEI_1D(J1,IND_POL)
 210     FORMAT ( A, ' ind_obs: ', I5, ' ind_tim: ', I5, ' ind_pol: ', I1, ' wei = ', F9.6 )
         I_FRQ = 0
         DO 420 J2=PIM%CONF%BEG_FRQ,PIM%CONF%END_FRQ
            I_FRQ = I_FRQ + 1
            DO 430 J3=1,PIM%NCHN
               WRITE ( 6, 220 ) LABEL, IND_OBS, J3, J2, J1, PIM%OBS(IND_OBS)%UV(J3,I_FRQ,J1,IND_POL), &
     &                          PHAS_CMPL_R4(PIM%OBS(IND_OBS)%UV(J3,I_FRQ,J1,IND_POL)), &
     &                          ABS(PIM%OBS(IND_OBS)%UV(J3,I_FRQ,J1,IND_POL))
 220           FORMAT ( A, ' obs: ', I5, ' inds= ', I5, 1X, I2, 1X, I5, &
     &                  ' UV= ', 1PE14.7, ' , ', 1PE14.7, ' Phs: ', 0PF9.6, ' Amp: ', 1PE14.7 )
 430        CONTINUE 
 420     CONTINUE 
 410  CONTINUE 
      RETURN
      END  SUBROUTINE  PIMA_UV_PRINT  !#!#
