#include <mk5_preprocessor_directives.inc>
      SUBROUTINE PIMA_ACTA ( PIM, VTD, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine PIMA_ACTA generates text table with time averaged          *
! *   autocorrelation spectrum for each scan, each station. It also      *
! *   performs frequency averaging over PIM%CONF%FRIB_1D_FRQ_MSEG        *
! *   segments. Routine PIMA_ACTA honors INCLUDE_OBS_FILE,               *
! *   EXCLUDE_OBS_FILE options. In time averaging, it considers          *
! *   the scan interval within SCAN_LEN_USED, SCAN_LEN_SKIP constraints. *
! *   It exludes autcorrelation with weights less than                   *
! *   FRIB.AUTOCORR_THRESHOLD.                                           *
! *                                                                      *
! *  ### 11-APR-2014   PIMA_ACTA   v1.6 (c)  L. Petrov  16-JUL-2021 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'pima.i'
      INCLUDE   'vtd.i'
      TYPE     ( PIMA__TYPE ) :: PIM
      TYPE     ( VTD__TYPE  ) :: VTD
      TYPE     ( VTD__OBS_TYPE  ) :: OBS_TYP
      INTEGER*4  IUER
      INTEGER*8  STREAM, DIR_DESC
      CHARACTER  STR*128, STR1*128
      LOGICAL*1, ALLOCATABLE :: FL_USE(:)
      COMPLEX*8, ALLOCATABLE :: AC(:,:,:,:,:), AC_OBS(:,:)
      REAL*8,    ALLOCATABLE :: FRQ_OBS(:,:)
      CHARACTER, ALLOCATABLE :: OUT(:)*80
      CHARACTER  C_STA(PIM__MSTA)*8, POLAR_STR*2, FILOUT*128, DIROUT*128
      INTEGER*4  J1, J2, J3, J4, J5, J6, J7, J8, J9, J10, J11, J12, &
     &           IND_OBS, IS, FRG_IND, IND_OBS_STA, IND_STA_BAS, &
     &           NUM_EPC_MAX, L_STA, IND_STA, IND_CHN, UV_IND, &
     &           IND_FRQ,  LCHN, LFRQ, LTIM, MOUT, NO, IND_POL, IER
      REAL*8     DER_DEL(VTD__NDER), DER_RAT(VTD__NDER), AP_LEN, &
     &           TIM_ARR(PIM__MUV), DURA_ACC, DEL_FRT, RAT_FRT, &
     &           DEL_FRT_DELTA, RAT_FRT_DELTA, ACC_FRT, FRT_OFFS, &
     &           TIM_BEG, WT, TIM_DELTA
      PARAMETER  ( TIM_DELTA = 1.0D0 )
      REAL*4     WEI_1D(PIM__MEPC,PIM__MPLR,2), AC_MEAN(PIM__MFRQ,2), WEI_SUM
      INTEGER*2  MASK
      DATA       MASK / O'0755' /
      INTEGER*4, EXTERNAL :: ILEN, I_LEN, ADD_CLIST, LTM_DIF 
      INTEGER*4, EXTERNAL :: FFTWF_IMPORT_WISDOM_FROM_FILE
      INTEGER*8, EXTERNAL :: FOPEN, FCLOSE, FUNC_OPENDIR, MKDIR
      REAL*8,    EXTERNAL :: WALL_TIMER
      CHARACTER, EXTERNAL :: MJDSEC_TO_DATE*30, GET_CDATE*19
#ifdef PIMA_USE_MKL
#else
      IF ( ( PIM%CONF%FRIB_SEARCH_TYPE == PIMA__2FFT .OR. &
     &       PIM%CONF%FRIB_SEARCH_TYPE == PIMA__3FFT      )  .AND. &
            PIM%CONF%FFT_METHOD .NE. FFT_MKL                        ) THEN
            STREAM = FOPEN ( PIM%CONF%FFT_CONFIG_FILE(1:I_LEN(PIM%CONF%FFT_CONFIG_FILE))//CHAR(0), 'r'//CHAR(0) )
            IF ( STREAM < 0 ) THEN
                 CALL CLRCH  ( STR )
                 CALL GERROR ( STR )
                 CALL ERR_LOG ( 6711, IUER, 'PIMA_ACTA', 'Error during '// &
     &               'opening FFTW wisdom file '//PIM%CONF%FFT_CONFIG_FILE )
                 RETURN
            END IF
            IS = FFTWF_IMPORT_WISDOM_FROM_FILE ( %VAL(STREAM) )
            IF ( IS < 0 ) THEN
                 CALL ERR_LOG ( 6712, IUER, 'PIMA_ACTA', 'Error during '// &
     &               'reading FFTW wisdom file '//PIM%CONF%FFT_CONFIG_FILE )
                 RETURN
            END IF
            IS = FCLOSE ( %VAL(STREAM) )
      END IF
#endif
!
      OBS_TYP%PLRZ    = 'RR'     
      OBS_TYP%FRQ_REF(1) = PIM%REF_FREQ
      OBS_TYP%FRQ_REF(2) = 0.0D0
      OBS_TYP%N_BND      = 1
      OBS_TYP%DELAY_TYPE = VTD__ML__DTP
      OBS_TYP%FRQ_ION_EFF(1) = 2.3D9
      OBS_TYP%FRQ_ION_EFF(2) = 0.0D0
      OBS_TYP%STATUS     = VTD__BND 
!
      ALLOCATE ( FL_USE(PIM%NOBS), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 6713, IUER, 'PIMA_ACTA', 'Error in an attempt '// &
     &         'to allocate '//STR(1:I_LEN(STR))//' bytes of dynamic '// &
     &         'memory for array FL_USE' )
           RETURN
      END IF
!
      IF ( PIM%CONF%FRIB_NOBS > 0 ) THEN
!
! -------- In a case if INCLUDE_OBS, EXCLUDE_OBS clause was used, 
! -------- we need to find usable observations. We put used/not_used
! -------- in array FL_USE
!
           FL_USE = .FALSE.
           DO 410 J1=1,PIM%CONF%FRIB_NOBS
              IND_OBS = PIM%CONF%FRIB_OBS(J1)
              IF ( .NOT. PIM%USE_OBS(IND_OBS) ) GOTO 410 ! Bypass deselected observation
              FRG_IND = PIM%OBS(IND_OBS)%REF_FRG_INDS(PIM%CONF%FRQ_GRP) 
              IF ( FRG_IND == 0 ) THEN
                   WRITE ( 6, * ) 'PIMA_FRINGE NUM_NUVS= ', INT2(PIM%OBS(IND_OBS)%NUVS), &
     &                            ' NUM_EPC= ', INT2(PIM%OBS(IND_OBS)%NUM_EPC)
                   WRITE ( 6, 210 ) IND_OBS, PIM%CONF%FRQ_GRP
 210               FORMAT ( 'Observation ', I6, &
     &                      ' does not have accumulation periods for frequency group ', I1 )
                   GOTO 410
              END IF
              FL_USE(J1) = .TRUE.
 410       CONTINUE 
         ELSE 
           FL_USE = .TRUE.
      END IF
      IF ( PIM%CONF%POLAR == PIMA__POLAR_RR ) THEN
           IND_POL = 1
           POLAR_STR = 'RR'
         ELSE IF ( PIM%CONF%POLAR == PIMA__POLAR_I ) THEN
           IND_POL = 1
           POLAR_STR = 'RR'
         ELSE IF ( PIM%CONF%POLAR == PIMA__POLAR_LL .AND. &
     &             PIM%NPOL == 2 ) THEN
           IND_POL = 2
           POLAR_STR = 'LL'
         ELSE IF ( PIM%CONF%POLAR == PIMA__POLAR_LL .AND. &
     &             PIM%NPOL == 1 ) THEN
           IND_POL = 1
           POLAR_STR = 'LL'
        ELSE IF ( PIM%CONF%POLAR == PIMA__POLAR_RL ) THEN
           IND_POL = 1
           POLAR_STR = '??'
        ELSE IF ( PIM%CONF%POLAR == PIMA__POLAR_LR ) THEN
           IND_POL = 2
           POLAR_STR = '??'
      END IF
      IF ( PIM%CONF%DEBUG_LEVEL .GE. 4 ) THEN
           WRITE ( 6, * ) 'PIMA_ACTA-116 IND_POL= ', IND_POL, ' POLAR_STR= ', POLAR_STR
      END IF
!
      LCHN = PIM%NCHN/PIM%CONF%FRIB_1D_FRQ_MSEG
      LFRQ = PIM%CONF%END_FRQ - PIM%CONF%BEG_FRQ + 1
      ALLOCATE ( AC_OBS(LCHN,LFRQ), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( 4*LCHN*LFRQ, STR )
           CALL ERR_LOG ( 6714, IUER, 'PIMA_ACTA', 'Failure to '// &
     &         'allocate '//STR(1:I_LEN(STR))//' bytes of dynamic '// &
     &         'memory for array AC_OBS' )
           RETURN
      END IF
      AC_OBS = 0.0
!
      ALLOCATE ( FRQ_OBS(LCHN,LFRQ), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( 8*LCHN*LFRQ, STR )
           CALL ERR_LOG ( 6715, IUER, 'PIMA_ACTA', 'Failure to '// &
     &         'allocate '//STR(1:I_LEN(STR))//' bytes of dynamic '// &
     &         'memory for array FRQ_OBS' )
           RETURN
      END IF
!
! --- Initlialize VTD object
!
      CALL ERR_PASS ( IUER, IER )
      CALL VTD_INIT ( VTD,  IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 6716, IUER, 'PIMA_ACTA', 'Error in an attempt to '// &
     &         'initialize VTD oibject' )
           RETURN
      END IF
!
! --- Read and parse VTD configuration file
!
      CALL ERR_PASS ( IUER, IER )
      CALL VTD_CONF ( PIM%CONF%VTD_CONFIG_FILE, VTD, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 6717, IUER, 'PIMA_ACTA', 'Error in an attempt '// &
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
           CALL ERR_LOG ( 6718, IUER, 'PIMA_ACTA', 'Error in an '// &
     &         'attempt to load the data into VTD data structure' )
           RETURN
      END IF
      FRQ_OBS = 0.0D0
!
! --- Cycle over scans
!
      DO 420 J2=1,PIM%NSCA
         L_STA = 0
!
! ------ Create a list of stations that participated in the J2-th scan
!
         DO 430 J3=1,PIM%SCA(J2)%NBAS
            IND_OBS = PIM%SCA(J2)%OBS_IND(J3)
            IF ( FL_USE(IND_OBS) ) THEN
                 IS = ADD_CLIST ( PIM__MSTA, L_STA, C_STA, &
     &                            PIM%C_STA(PIM%OBS(IND_OBS)%STA_IND(1)), IER )
                 IS = ADD_CLIST ( PIM__MSTA, L_STA, C_STA, &
     &                            PIM%C_STA(PIM%OBS(IND_OBS)%STA_IND(2)), IER )
            END IF
 430     CONTINUE 
!
! ------ Empty list (i.e. all stations were deselected)? Go to another scan
!
         IF ( L_STA == 0 ) GOTO 420
!
! ------ Cycle over all useable stations of the J2-th scan
!
         DO 440 J4=1,L_STA
            IND_OBS_STA = 0
            IND_STA = LTM_DIF ( 1, PIM%NSTA, PIM%C_STA, C_STA(J4) )
!
! --------- Find the observation that have the maximum number of epochs.
! --------- Since stations may start/start early or late, this number
! --------- may vary from observation to observation
!
            NUM_EPC_MAX = -1
            DO 450 J5=1,PIM%SCA(J2)%NBAS
               IND_OBS = PIM%SCA(J2)%OBS_IND(J5)
               FRG_IND = PIM%OBS(IND_OBS)%REF_FRG_INDS(PIM%CONF%FRQ_GRP) 
               IF ( FL_USE(IND_OBS)      .AND. &
     &              PIM%USE_OBS(IND_OBS) .AND. &
     &              ( PIM%OBS(IND_OBS)%STA_IND(1) == IND_STA .OR. &
     &                PIM%OBS(IND_OBS)%STA_IND(2) == IND_STA    ) ) THEN
                    IF ( PIM%OBS(IND_OBS)%NUM_EPC(FRG_IND) > NUM_EPC_MAX ) THEN
                         NUM_EPC_MAX = PIM%OBS(IND_OBS)%NUM_EPC(FRG_IND) 
                         IND_OBS_STA = IND_OBS
                    END IF
               END IF
 450        CONTINUE 
            IF ( IND_OBS_STA == 0 ) GOTO 440
!
! --------- OK, We found such an observation. Its index is IND_OBS_STA.
! --------- Find the the J4th station index in the baseline
!
            IF ( PIM%OBS(IND_OBS_STA)%STA_IND(1) == IND_STA ) THEN
                 IND_STA_BAS = 1
               ELSE IF ( PIM%OBS(IND_OBS_STA)%STA_IND(2) == IND_STA ) THEN
                 IND_STA_BAS = 2
            END IF
!
            LTIM = PIM%OBS(IND_OBS_STA)%NUM_EPC(FRG_IND)
            ALLOCATE ( AC(PIM%NCHN,LFRQ,LTIM,2,PIM%NSTK), STAT=IER )
            IF ( IER .NE. 0 ) THEN
                 CALL CLRCH ( STR )
                 CALL IINCH ( 8*PIM%NCHN*LFRQ*LTIM*2*PIM%NSTK, STR )
                 CALL ERR_LOG ( 6719, IUER, 'PIMA_ACTA', 'Failure to '// &
     &                'allocate '//STR(1:I_LEN(STR))//' bytes of dynamic '// &
     &                'memory for array AC' )
                  RETURN
            END IF
!            
            FRT_OFFS = PIM%OBS(IND_OBS_STA)%FRT_OFFSET(1)
            TIM_BEG  = PIM%OBS(IND_OBS_STA)%TIM_BEG
            IF ( FRT_OFFS < -86400.0D0 ) FRT_OFFS = (PIM%OBS(IND_OBS_STA)%TIM_END - PIM%OBS(IND_OBS_STA)%TIM_BEG)/2.0D0
!
! --------- Compute path delay two times, first with an a time offset TIM_DELTA
! --------- in order to compute numerically ACC_RT -- the second time derivative
! --------- of path delay
!
            CALL ERR_PASS ( IUER, IER )
            CALL VTD_DELAY ( PIM%C_SOU(PIM%OBS(IND_OBS_STA)%SOU_IND), &
     &                       'GEOCENTR', PIM%C_STA(IND_STA), &
     &                       PIM%MJD_0, &
     &                       PIM%TAI_0 + TIM_BEG + FRT_OFFS + TIM_DELTA, &
     &                       OBS_TYP, VTD, DEL_FRT_DELTA, RAT_FRT_DELTA, &
     &                       DER_DEL, DER_RAT, IER )
            IF ( IER .NE. 0 ) THEN
                 CALL ERR_LOG ( 6720, IUER, 'PIMA_ACTA', 'Error in an '// &
     &               'attempt to compute VLBI time delay for source '// &
     &                PIM%C_SOU(PIM%OBS(IND_OBS_STA)%SOU_IND) )
                 RETURN 
            END IF
!
            CALL ERR_PASS ( IUER, IER )
            CALL VTD_DELAY ( PIM%C_SOU(PIM%OBS(IND_OBS_STA)%SOU_IND), &
     &                       'GEOCENTR', PIM%C_STA(IND_STA), &
     &                       PIM%MJD_0, &
     &                       PIM%TAI_0 + TIM_BEG + FRT_OFFS, &
     &                       OBS_TYP, VTD, DEL_FRT, RAT_FRT, &
     &                       DER_DEL, DER_RAT, IER )
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!    write ( 6, * ) 'Sta: ', PIM%C_STA(IND_STA), ' ', C_STA(J4), &
!     &             '  Sou: ', PIM%C_SOU(PIM%OBS(IND_OBS_STA)%SOU_IND), &
!     &             '  Sca: ', PIM%SCA(J2)%SCAN_NAME, &
!     &             ' Frt_offs= ', sngl(frt_offs), &
!     &             ' Tim: ', mjdsec_to_date ( pim%mjd_0, pim%tai_0 + tim_beg + frt_offs, ier ), &
!     &             rat_frt
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
            IF ( IER .NE. 0 ) THEN
                 CALL ERR_LOG ( 6721, IUER, 'PIMA_ACTA', 'Error in an '// &
     &               'attempt to compute VLBI time delay for source '// &
     &                PIM%C_SOU(PIM%OBS(IND_OBS_STA)%SOU_IND) )
                 RETURN 
            END IF
            ACC_FRT = (RAT_FRT_DELTA - RAT_FRT)/TIM_DELTA
!
            AC = 0.0
            DO 460 J6=1,2
!
! ------------ Get autocorrelation data
!
               CALL ERR_PASS ( IUER, IER )
               CALL PIMA_GET_AC ( PIM, IND_OBS_STA, LFRQ, PIM%CONF%BEG_FRQ, &
     &                            PIM%CONF%END_FRQ, PIM%CONF%FRQ_GRP, &
     &                            J6, AC, WEI_1D(1,1,J6), AP_LEN, IER  )
               IF ( IER .NE. 0 ) THEN
                    CALL CLRCH ( STR )
                    CALL INCH  ( IND_OBS, STR )
                    CALL ERR_PASS ( IUER, IER )
                    CALL ERR_LOG  ( 6722, IER, 'PIMA_ACTA', 'Failure to '// &
     &                  'get autocorrelation data for station '// &
     &                   C_STA(PIM%OBS(IND_OBS_STA)%STA_IND(J6))// &
     &                  ' for the '//STR(1:I_LEN(STR))//'th observation'// &
     &                  ' from input FITS-IDI file ' )
                    RETURN 
               END IF
!
! ------------ Apply bandpass mask and re-normalize autocorrelation
!
               CALL ERR_PASS ( IUER, IER )
               CALL PIMA_ACCOR_MEAN ( PIM, IND_OBS_STA, LTIM, J6, AC(1,1,1,J6,IND_POL), &
     &                                WEI_1D(1,IND_POL,J6), AC_MEAN(1,J6), &
     &                               .FALSE., IER  )
               IF ( IER .NE. 0 ) THEN
                    CALL CLRCH ( STR )
                    CALL INCH  ( IND_OBS, STR )
                    CALL ERR_LOG  ( 6723, IUER, 'PIMA_ACTA', 'Failure in an '// &
     &                  'attempt to compute averaged autcorrelation function '// &
     &                  'for the '//STR(1:I_LEN(STR))//' th observation '// &
     &                  'from input FITS-IDI file ' )
                    RETURN
               END IF
 460        CONTINUE 
!
            DURA_ACC = 0.0D0
            AC_OBS   = (0.0, 0.0)
            WEI_SUM  = 0.0
!
! --------- Cycle over epochs
!
            DO 470 J7=1,LTIM
!
! ------------ check for the autocorrelation threshold
!
               IF ( WEI_1D(J7,IND_POL,IND_STA_BAS) < PIM%CONF%FRIB_WEIGHTS_THRESHOLD ) GOTO 470
               UV_IND  = PIM%OBS(IND_OBS_STA)%UV_IND(J7,FRG_IND)
               TIM_ARR(J7) = PIM%TIM_R8(PIM%UV_IND(UV_IND)%TIM_IND) - &
     &                       PIM%OBS(IND_OBS_STA)%TIM_BEG
!
! ------------ Downweight epochs that are too ealry or too late
! ------------ according to SCAN_LEN_SKIP/SCAN_LEN_USED options
!
               IF ( TIM_ARR(J7) < PIM%CONF%SCAN_LEN_SKIP ) THEN
                    WEI_1D(J7,IND_POL,IND_STA_BAS) = 0.0D0
               END IF 
               DURA_ACC = DURA_ACC + WEI_1D(J7,IND_POL,IND_STA_BAS)*AP_LEN
               IF ( DURA_ACC > PIM%CONF%SCAN_LEN_USED ) THEN
                    WEI_1D(J7,IND_POL,IND_STA_BAS) = 0.0D0
               END IF
               IF ( WEI_1D(J7,IND_POL,IND_STA_BAS) .GE. PIM%CONF%FRIB_WEIGHTS_THRESHOLD ) THEN
!
! ----------------- Update autocorrelation spectrum accumulators
!
                    WEI_SUM = WEI_SUM + WEI_1D(J7,IND_POL,IND_STA_BAS)*PIM%CONF%FRIB_1D_FRQ_MSEG
                    DO 480 J8=1,LFRQ
                       DO 490 J9=1,LCHN
                          DO 4100 J10=1,PIM%CONF%FRIB_1D_FRQ_MSEG
                             IND_CHN = (J9-1)*PIM%CONF%FRIB_1D_FRQ_MSEG + J10
                             AC_OBS(J9,J8) = AC_OBS(J9,J8) + AC(IND_CHN,J8,J7,IND_STA_BAS,IND_POL)*WEI_1D(J7,IND_POL,IND_STA_BAS)
 4100                     CONTINUE 
 490                   CONTINUE 
 480                CONTINUE 
               END IF
 470        CONTINUE 
            IF ( WEI_SUM > PIM%CONF%FRIB_WEIGHTS_THRESHOLD ) THEN
!
! -------------- Get memory for the output buffer
!
                 MOUT = LFRQ*LCHN +  64
                 ALLOCATE ( OUT(MOUT), STAT=IER )
                 IF ( IER .NE. 0 ) THEN
                      CALL CLRCH ( STR )
                      CALL IINCH ( 80*MOUT, STR )
                      CALL ERR_LOG ( 6724, IUER, 'PIMA_ACTA', 'Failure to '// &
     &                    'allocate '//STR(1:I_LEN(STR))//' bytes of dynamic '// &
     &                    'memory for array OUT' )
                      RETURN
                 END IF
!
! -------------- Prepare the output file header
!
                 OUT(1)  = PIMA__ACTA_LABEL
                 OUT(2)  = '# '
                 OUT(3)  = '# Experiment:                   '// &
     &                        PIM%CONF%SESS_CODE(1:I_LEN(PIM%CONF%SESS_CODE))
!
                 CALL CLRCH ( STR )
                 WRITE ( UNIT=STR(1:8), FMT='(F8.2)' ) PIM%CONF%SCAN_LEN_USED
                 CALL CHASHL ( STR(1:8)  )
                 OUT(4)  = '# SCAN_LEN_USED:                '//STR(1:8)
!
                 CALL CLRCH ( STR )
                 WRITE ( UNIT=STR(1:8), FMT='(F8.2)' ) PIM%CONF%SCAN_LEN_SKIP
                 CALL CHASHL ( STR )
                 OUT(5)  = '# SCAN_LEN_SKIP:                '//STR(1:8)
!
                 CALL CLRCH ( STR )
                 WRITE ( UNIT=STR(1:4), FMT='(I4)' ) PIM%CONF%FRIB_1D_FRQ_MSEG
                 CALL CHASHL ( STR(1:4) )
                 OUT(6)  = '# FRIB_1D_FRQ_MSEG:             '//STR(1:4)
!
                 CALL CLRCH ( STR )
                 WRITE ( UNIT=STR(1:6), FMT='(F6.4)' ) PIM%CONF%FRIB_AUTOCORR_THRESHOLD
                 CALL CHASHL ( STR(1:6) )
                 OUT(7)  = '# FRIB.AUTOCORR_THRESHOLD:      '//STR(1:6)
!
                 OUT(8)  = '# FRIB.AUTOCORR_CALIB:          '//PIM%CONF%FRIB_AUTOCORR_CALIB
                 OUT(9)  = '# '
                 OUT(10) = '# Station:                      '//C_STA(J4)
                 OUT(11) = '# Source:                       '//PIM%C_SOU(PIM%OBS(IND_OBS_STA)%SOU_IND)
                 OUT(12) = '# Scan_name:                    '//PIM%SCA(J2)%SCAN_NAME
!
                 CALL CLRCH  ( STR )
                 CALL INCH   ( J2, STR(1:5) )
                 CALL CHASHR (     STR(1:5) )
                 OUT(13) = '# Scan_index:                   '//STR(1:5)
!
                 CALL CLRCH  ( STR )
                 CALL INCH   ( IND_OBS_STA, STR(1:6) )
                 CALL CHASHR (     STR(1:6) )
                 OUT(14) = '# Observation_index:            '//STR(1:6)
!
                 CALL CLRCH ( STR)
                 STR = MJDSEC_TO_DATE ( PIM%MJD_0, PIM%TAI_0 + PIM%OBS(IND_OBS_STA)%TIM_BEG, IER )
                 OUT(15) = '# Start_date:                   '//STR(1:30)
!
                 CALL CLRCH ( STR)
                 STR = MJDSEC_TO_DATE ( PIM%MJD_0, PIM%TAI_0 + PIM%OBS(IND_OBS_STA)%TIM_END + AP_LEN, IER )
                 OUT(16) = '# Stop_date:                    '//STR(1:30)
!
                 CALL CLRCH ( STR)
                 WRITE ( UNIT=STR(1:20), FMT='(1PD19.12)' ) DEL_FRT
                 OUT(17) = '# Geocentric group delay:       '//STR(1:20)
!
                 CALL CLRCH ( STR)
                 WRITE ( UNIT=STR(1:20), FMT='(1PD19.12)' ) RAT_FRT
                 OUT(18) = '# Geocentric delay rate:        '//STR(1:20)
!
                 CALL CLRCH ( STR)
                 WRITE ( UNIT=STR(1:20), FMT='(1PD19.12)' ) ACC_FRT
                 OUT(19) = '# Geocentric path acceleration: '//STR(1:20)
!
                 CALL CLRCH ( STR)
                 WRITE ( UNIT=STR(1:8), FMT='(F8.3)' ) DURA_ACC   
                 CALL CHASHL ( STR )
                 OUT(20) = '# Effective duration:           '//STR(1:8)
!
                 CALL CLRCH ( STR )
                 CALL INCH  ( LCHN*LFRQ, STR(1:12) )
                 CALL CHASHL ( STR(1:12) )
                 OUT(21) = '# Number_of_points:             '//STR(1:12)
                 OUT(22) = '# Doppler corrected:            NO'
!
                 OUT(23) = '# '
                 OUT(24) = '# Creation date:                '//GET_CDATE()
                 OUT(25) = '# '
                 NO = 25
!
! -------------- Now write down the autocorrelation spectrum in the 
! -------------- buffer OUT with contents of the output file
!
                 DO 4110 J11=1,LFRQ
                    DO 4120 J12=1,LCHN
                       IND_FRQ = 1 + (J12-1)*PIM%CONF%FRIB_1D_FRQ_MSEG + &
     &                               (PIM%CONF%FRIB_1D_FRQ_MSEG-1)/2
                       FRQ_OBS(J12,J11) = PIM%FREQ_ARR(IND_FRQ,PIM%CONF%BEG_FRQ-1+J11,PIM%CONF%FRQ_GRP)
                       AC_OBS(J12,J11)  = (AC_OBS(J12,J11)/WEI_SUM)/AC_MEAN(J11,IND_STA_BAS)
                       NO = NO + 1
                       WRITE ( UNIT=OUT(NO), FMT=110 ) J12, J11, FRQ_OBS(J12,J11), &
     &                                                 ABS(AC_OBS(J12,J11))
 110                   FORMAT ( 'ACRL  Lchn: ', I6, ' Lfrq: ', I4, &
     &                          ' Freq: ', F15.2, ' Hz   Ac: ', F12.5 )
 4120               CONTINUE 
 4110            CONTINUE 
!
! -------------- Build the name of the output directory ...
!
                 DIROUT = PIM%CONF%EXPER_DIR(1:I_LEN(PIM%CONF%EXPER_DIR))//'/'// &
     &                    PIM%CONF%SESS_CODE(1:I_LEN(PIM%CONF%SESS_CODE))// &
     &                    '_atb'
!
! -------------- ... and check, whether it exists or not
!
                 DIR_DESC = FUNC_OPENDIR ( DIROUT(1:I_LEN(DIROUT))//CHAR(0) )
                 IF ( DIR_DESC .EQ. 0 ) THEN
!
! ------------------- Does not exist? We have to create it.
!
                      IS = MKDIR ( DIROUT(1:I_LEN(DIROUT))//CHAR(0), &
     &                             %VAL(MASK) )
                      IF ( IS .NE. 0 ) THEN
                           CALL GERROR  ( STR )
                           CALL ERR_LOG ( 6725, IUER, 'PIMA_ACTA', 'Failure '// &
     &                         'to create direcotry '//DIROUT(1:I_LEN(DIROUT))// &
     &                         ' for autocorrelation tables' )
                           RETURN
                      END IF
                    ELSE
                      CALL CLOSEDIR ( %VAL(DIR_DESC) )
                 END IF
!
! -------------- Create the fill output filename
!
                 STR = PIM%SCA(J2)%SCAN_NAME
                 CALL BLANK_TO_UNDERSCORE ( STR(1:10) )
                 STR1(1:8) = C_STA(J4)
                 CALL BLANK_TO_UNDERSCORE ( STR1(1:8) )
                 CALL TRAN ( 12, STR1(1:8), STR1(1:8) )
!
                 FILOUT = DIROUT(1:I_LEN(DIROUT))//'/acta_sca_'// &
     &                    STR(1:10)//'_'//STR1(1:8)//'.txt'
!
! -------------- Write down the buffer in the output file
!
                 CALL ERR_PASS ( IUER, IER )
                 CALL WR_TEXT ( NO, OUT, FILOUT, IER )
                 IF ( IER .NE. 0 ) THEN
                      CALL ERR_LOG ( 6726, IUER, 'PIMA_ACTA', 'Error in '// &
     &                    'writing into the output file '//FILOUT )
                      RETURN
                 END IF
                 DEALLOCATE ( OUT )
                 IF ( PIM%CONF%DEBUG_LEVEL .GE. 2 ) THEN
                      WRITE ( 6, '(A)' ) 'PIMA_ACTA created file: '// &
     &                                    FILOUT(1:I_LEN(FILOUT))
                 END IF
            END IF
!
            DEALLOCATE ( AC )
 440     CONTINUE 
 420  CONTINUE 
!
      DEALLOCATE ( FRQ_OBS)
      DEALLOCATE ( AC_OBS)
      DEALLOCATE ( FL_USE )
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE PIMA_ACTA  !#!#
