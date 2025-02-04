      SUBROUTINE PIMA_PLOT_ACRL ( PIM, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine PIMA_PLOT_ACRL
! *                                                                      *
! * ### 30-JAN-2006  PIMA_PLOT_ACRL  v2.3 (c)  L. Petrov 27-NOV-2020 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'astro_constants.i'
      INCLUDE   'pima.i'
      INCLUDE   'vtd.i'
      INCLUDE   'diagi.i'
      TYPE     ( PIMA__TYPE ) :: PIM
      INTEGER*4  IUER
      TYPE     ( DIAGI_STRU ) :: DIA(2)
      INTEGER*4  MARR, MPB, NN
      PARAMETER  ( MARR = 8192 )
      PARAMETER  ( MPB  = 4    )
      REAL*4     AC_MEAN(PIM__MFRQ,2,PIM__MPLR)
      REAL*8     FREQ(PIM__MFRQ*PIM__MCHN), AMP(PIM__MFRQ*PIM__MCHN,2), &
     &           PHS(PIM__MFRQ*PIM__MCHN,2), &
     &           AMP_PAR(PIM__MFRQ*PIM__MCHN,2,2), SUM_WEI1, SUM_WEI2, &
     &           AMP_1, AMP_2
      COMPLEX*8  CMP(PIM__MFRQ*PIM__MCHN,2)
      CHARACTER  COMMON_TIT*80, TITS(PIM__MSTA)*16, BUTTON_NAME(MPB)*24, &
     &           BUTTON_LET(MPB)*2, PREF_NAME*128, ZAG*128, UNIT*128, &
     &           POL_CODES*2, STR*128, STR1*32, STR_IP*16, MODE*3
      DATA      ( BUTTON_LET(NN), BUTTON_NAME(NN), NN=1,MPB ) &
     &          / &
     &          '  ', '    ',  &
     &          'Qq', 'Quit',  &
     &          '  ', '    ',  &
     &          '  ', '    '   &
     &          /
      REAL*4,    ALLOCATABLE :: WEI(:)
      REAL*4     WEI_MAX
      COMPLEX*8, ALLOCATABLE :: AC(:,:,:,:,:)
      REAL*4,    ALLOCATABLE :: WEI_1D(:,:,:)
      INTEGER*4  LFRQ, LTIM, NC, NR, ICODE, J1, J2, J3, J4, J5, J6, J7, J8, &
     &           ICHN, IND_UV1, IND_FIL, IND_FRQ, N_FRQ, IND_STA(2), &
     &           IND_OBS, IND_SCA, I_FRQ, POL_IND, POL_MODE, IER
      REAL*8     AP_LEN, FUDGE(2), AMP_AVR(PIM__MFRQ,2), FREQ_OFFSET
      INTEGER*4  IBST, ILST, IOST, IPST, IWST, IDEV, &
     &           ICL1, ICL2, ICL3, IS, FRI_STS
      INTEGER*8  STREAM
      LOGICAL*1  FL_ACCOR_NOPC, FL_1ST_IF
      REAL*4,    EXTERNAL :: ATAN_CS_R4, PHAS_CMPL_R4
      CHARACTER, EXTERNAL :: MJDSEC_TO_DATE*30
      INTEGER*4, EXTERNAL :: GET_UNIT, ILEN, I_LEN
      INTEGER*4, EXTERNAL :: FFTWF_IMPORT_WISDOM_FROM_FILE
      INTEGER*8, EXTERNAL :: FOPEN, FCLOSE
#ifdef NO_PLOT
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  PIMA_PLOT_ACRL  !#!
#else
!
      FL_ACCOR_NOPC = .FALSE. 
      CALL GETENVAR ( 'PIMAVAR_ACCOR_NOPC', STR )
      IF ( STR(1:3) == 'YES' .OR. STR(1:3) == 'yes' ) THEN
           FL_ACCOR_NOPC = .TRUE. 
      END IF
!
      FL_1ST_IF = .FALSE. 
      CALL GETENVAR ( 'PIMAVAR_ACPL_1ST_IF', STR )
      IF ( STR(1:3) == 'YES' .OR. STR(1:3) == 'yes' ) THEN
           FL_1ST_IF = .TRUE. 
      END IF
!
      CALL GETENVAR ( 'PIMAVAR_ACPL_FREQ_OFFSET', STR )
      IF ( ILEN(STR) > 0 ) THEN
           READ ( UNIT=STR, FMT=* ) FREQ_OFFSET
         ELSE 
           FREQ_OFFSET = 0.0D0
      END IF
!
      MODE = 'amp'
!
! --- Setting defaults values of the plotting parameters
!
      CALL ERR_PASS   ( IUER, IER )
      CALL DIAGI_DEF  ( IBST, ILST, IOST, IPST, IWST, IDEV, ZAG, UNIT, &
     &                  ICL1, ICL2, ICL3, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 7581, IUER, 'PIMA_PLOT_ACRL', 'Error in setting '// &
     &         'default values for the plot' )
           RETURN
      END IF
!
      PREF_NAME = '/tmp/acrl_'// &
     &             PIM%CONF%SESS_CODE(1:I_LEN(PIM%CONF%SESS_CODE))//'_acrl_'
!
      IF ( PIM%CONF%FRIB_OBS_STATUS  == PIMA__OBS_NO ) THEN
           CALL ERR_LOG ( 7582, IUER )
           RETURN
         ELSE IF ( PIM%CONF%FRIB_OBS_STATUS  == PIMA__OBS_ALL .AND. &
     &             PIM%CONF%FRIB_NOBS        == -1                  ) THEN
           PIM%CONF%FRIB_NOBS = PIM%NOBS
           ALLOCATE ( PIM%CONF%FRIB_OBS(PIM%CONF%FRIB_NOBS), STAT=IER )
           IF ( IER .NE. 0 ) THEN
                CALL CLRCH ( STR )
                CALL IINCH ( 4*PIM%CONF%FRIB_NOBS, STR )
                CALL ERR_LOG ( 7583, IUER, 'PIMA_PLOT_ACRL', &
     &              'Failure to allocate '//STR(1:I_LEN(STR))//' bytes '//&
     &              'of dynamic memory for the list of observations' )
                RETURN
           END IF
!
           DO 410 J1=1,PIM%CONF%FRIB_NOBS
              PIM%CONF%FRIB_OBS(J1) = J1
 410       CONTINUE
      END IF
!
      DO 420 J2=1,PIM%CONF%FRIB_NOBS
         IF ( PIM%CONF%FRIB_OBS(J2) > PIM%NOBS .OR. &
     &        PIM%CONF%FRIB_OBS(J2) .LE. 0 ) THEN
              CALL CLRCH ( STR )
              CALL INCH  ( PIM%CONF%FRIB_OBS(J2), STR )
              CALL CLRCH ( STR1 )
              CALL INCH  ( PIM%NOBS, STR1 )
              CALL ERR_LOG ( 7584, IUER, 'PIMA_PLOT_ACRL', 'Wrong observation '// &
     &            'index: '//STR(1:I_LEN(STR))//' -- out of range [1, '// &
     &             STR1(1:I_LEN(STR1))//'] ' )
              RETURN
         END IF
         FUDGE = 1.0D0
#ifdef PIMA_USE_MKL
#else
         IF ( ( PIM%CONF%FRIB_SEARCH_TYPE == PIMA__2FFT .OR. &
     &          PIM%CONF%FRIB_SEARCH_TYPE == PIMA__3FFT      )  .AND. &
              PIM%CONF%FFT_METHOD .NE. FFT_MKL                        ) THEN
              STREAM = FOPEN ( PIM%CONF%FFT_CONFIG_FILE(1:I_LEN(PIM%CONF%FFT_CONFIG_FILE))//CHAR(0), 'r'//CHAR(0) )
              IF ( STREAM < 0 ) THEN
                   CALL CLRCH  ( STR )
                   CALL GERROR ( STR )
                   CALL ERR_LOG ( 7585, IUER, 'PIMA_FRINGE', 'Error during '// &
     &                 'opening FFTW wisdom file '//PIM%CONF%FFT_CONFIG_FILE )
                   RETURN
              END IF
              IS = FFTWF_IMPORT_WISDOM_FROM_FILE ( %VAL(STREAM) )
              IF ( IS < 0 ) THEN
                   CALL ERR_LOG ( 7586, IUER, 'PIMA_FRINGE', 'Error during '// &
     &                 'reading FFTW wisdom file '//PIM%CONF%FFT_CONFIG_FILE )
                   RETURN
              END IF
              IS = FCLOSE ( %VAL(STREAM) )
         END IF
#endif
!
         IND_OBS = PIM%CONF%FRIB_OBS(J2)
         IND_SCA    = PIM%OBS(IND_OBS)%SCA_IND
         IND_STA(1) = PIM%OBS(IND_OBS)%STA_IND(1)
         IND_STA(2) = PIM%OBS(IND_OBS)%STA_IND(2)
!
         LFRQ = PIM%CONF%END_FRQ - PIM%CONF%BEG_FRQ + 1
         IF ( PIM%OBS(IND_OBS)%REF_FRG_INDS(PIM%CONF%FRQ_GRP) == 0 ) THEN
              GOTO 420
         END IF
         LTIM = PIM%OBS(IND_OBS)%NUM_EPC(PIM%OBS(IND_OBS)%REF_FRG_INDS(PIM%CONF%FRQ_GRP))
         IF ( LTIM < 1 ) THEN
              IF ( PIM%CONF%DEBUG_LEVEL .GE. 1 ) THEN
                   WRITE ( 6, 210 ) IND_OBS
 210               FORMAT ( 'PIMA_PLOT_ACPL  Obs ', I5, ' has no data' ) 
              END IF
              GOTO 420
         END IF
!
         POL_CODES = '??'
         IF ( PIM%NSTK == 1 ) THEN
              POL_IND = 1
              POL_MODE = PIMA__RRCC
              IF ( PIM%STK_1 == -1 ) POL_CODES = 'RR'
              IF ( PIM%STK_1 == -2 ) POL_CODES = 'LL'
            ELSE IF ( PIM%NSTK == 2 ) THEN
              IF ( PIM%CONF%POLAR == PIMA__POLAR_RR ) THEN
                   POL_IND = 1
                   POL_MODE = PIMA__RRCC
                   POL_CODES = 'RR'
                 ELSE IF ( PIM%CONF%POLAR == PIMA__POLAR_LL ) THEN
                   POL_IND = 2
                   POL_MODE = PIMA__LLCC
                   POL_CODES = 'LL'
              END IF
            ELSE IF ( PIM%NSTK == 4 ) THEN
              IF ( ( PIM%STA(PIM%OBS(IND_OBS)%STA_IND(1))%POL_TYP(1) == PIMA__POL_R .OR. &
     &               PIM%STA(PIM%OBS(IND_OBS)%STA_IND(1))%POL_TYP(1) == PIMA__POL_L      ) .AND. &
     &             ( PIM%STA(PIM%OBS(IND_OBS)%STA_IND(2))%POL_TYP(1) == PIMA__POL_R .OR. &
     &               PIM%STA(PIM%OBS(IND_OBS)%STA_IND(2))%POL_TYP(1) == PIMA__POL_L      )     ) THEN
!
                   IF ( PIM%CONF%POLAR == PIMA__POLAR_RR ) THEN
                        POL_IND = 1
                        POL_MODE = PIMA__RRCC
                        POL_CODES = 'RR'
                      ELSE IF ( PIM%CONF%POLAR == PIMA__POLAR_LL ) THEN
                        POL_IND = 2
                        POL_MODE = PIMA__LLCC
                        POL_CODES = 'LL'
                      ELSE IF ( PIM%CONF%POLAR == PIMA__POLAR_RL ) THEN
                        POL_IND = 3
                        POL_MODE = PIMA__RLCC
                        POL_CODES = 'RL'
                      ELSE IF ( PIM%CONF%POLAR == PIMA__POLAR_LR ) THEN
                        POL_IND = 4
                        POL_MODE = PIMA__LRCC
                        POL_CODES = 'LR'
                      ELSE 
                        CALL ERR_LOG ( 7587, IUER, 'PIMA_PLOT_ACRL', 'Cannot use '// &
     &                       TRIM(PIM%CONF%POLAR)//' because the visibility file '// &
     &                      'does not contain this polarization' )
                        RETURN
                   END IF
              ELSE IF ( ( PIM%STA(PIM%OBS(IND_OBS)%STA_IND(1))%POL_TYP(1) == PIMA__POL_H .OR. &
     &                    PIM%STA(PIM%OBS(IND_OBS)%STA_IND(1))%POL_TYP(1) == PIMA__POL_V      ) .AND. &
     &                  ( PIM%STA(PIM%OBS(IND_OBS)%STA_IND(2))%POL_TYP(1) == PIMA__POL_H .OR. &
     &                    PIM%STA(PIM%OBS(IND_OBS)%STA_IND(2))%POL_TYP(1) == PIMA__POL_V      )     ) THEN
                   IF ( PIM%CONF%POLAR == PIMA__POLAR_HH ) THEN
                        POL_IND   =  1
                        POL_MODE = PIMA__HHLL
                        POL_CODES = 'HH'
                      ELSE IF ( PIM%CONF%POLAR == PIMA__POLAR_VV ) THEN
                        POL_IND   =  2
                        POL_MODE = PIMA__VVLL
                        POL_CODES = 'VV'
                      ELSE IF ( PIM%CONF%POLAR == PIMA__POLAR_HV ) THEN
                        POL_IND   =  3
                        POL_MODE = PIMA__HVLL
                        POL_CODES = 'HV'
                      ELSE IF ( PIM%CONF%POLAR == PIMA__POLAR_VH ) THEN
                        POL_IND   =  4
                        POL_MODE = PIMA__VHLL
                        POL_CODES = 'VH'
                      ELSE 
                        WRITE ( 6, * ) 'Sta: ', PIM%C_STA(PIM%OBS(IND_OBS)%STA_IND(1)), ' ', PIM%C_STA(PIM%OBS(IND_OBS)%STA_IND(2))
                        WRITE ( 6, * ) 'PIM%STA(PIM%OBS(IND_OBS)%STA_IND(1))%POL_TYP= ', PIM%STA(PIM%OBS(IND_OBS)%STA_IND(1))%POL_TYP
                        WRITE ( 6, * ) 'PIM%STA(PIM%OBS(IND_OBS)%STA_IND(2))%POL_TYP= ', PIM%STA(PIM%OBS(IND_OBS)%STA_IND(2))%POL_TYP
                        CALL ERR_LOG ( 7588, IUER, 'PIMA_PLOT_ACRL', 'Cannot use '// &
     &                       TRIM(PIM%CONF%POLAR)//' because the visibility file '// &
     &                      'does not contain this polarization' )
                        RETURN
                   END IF
              ELSE IF ( ( PIM%STA(PIM%OBS(IND_OBS)%STA_IND(1))%POL_TYP(1) == PIMA__POL_R .OR. &
     &                    PIM%STA(PIM%OBS(IND_OBS)%STA_IND(1))%POL_TYP(1) == PIMA__POL_L      ) .AND. &
     &                  ( PIM%STA(PIM%OBS(IND_OBS)%STA_IND(2))%POL_TYP(1) == PIMA__POL_H .OR. &
     &                    PIM%STA(PIM%OBS(IND_OBS)%STA_IND(2))%POL_TYP(1) == PIMA__POL_V      )     ) THEN
                   IF ( PIM%CONF%POLAR == PIMA__POLAR_RH ) THEN
                        POL_IND   =  1
                        POL_MODE = PIMA__RHCL
                        POL_CODES = 'RH'
                      ELSE IF ( PIM%CONF%POLAR == PIMA__POLAR_LV ) THEN
                        POL_IND   =  2
                        POL_MODE = PIMA__LVCL
                        POL_CODES = 'LV'
                      ELSE IF ( PIM%CONF%POLAR == PIMA__POLAR_RV ) THEN
                        POL_IND   =  3
                        POL_MODE = PIMA__RVCL
                        POL_CODES = 'RV'
                      ELSE IF ( PIM%CONF%POLAR == PIMA__POLAR_LH ) THEN
                        POL_IND   =  4
                        POL_MODE = PIMA__LHCL
                        POL_CODES = 'LH'
                      ELSE 
                        CALL ERR_LOG ( 7589, IUER, 'PIMA_PLOT_ACRL', 'Cannot use '// &
     &                       TRIM(PIM%CONF%POLAR)//' because the visibility file '// &
     &                      'does not contain this polarization' )
                        RETURN
                   END IF
              ELSE IF ( ( PIM%STA(PIM%OBS(IND_OBS)%STA_IND(1))%POL_TYP(1) == PIMA__POL_H .OR. &
     &                    PIM%STA(PIM%OBS(IND_OBS)%STA_IND(1))%POL_TYP(1) == PIMA__POL_V      ) .AND. &
     &                  ( PIM%STA(PIM%OBS(IND_OBS)%STA_IND(2))%POL_TYP(1) == PIMA__POL_R .OR. &
     &                    PIM%STA(PIM%OBS(IND_OBS)%STA_IND(2))%POL_TYP(1) == PIMA__POL_L      )     ) THEN
                   IF ( PIM%CONF%POLAR == PIMA__POLAR_HR ) THEN
                        POL_IND   =  1
                        POL_MODE  = PIMA__HRLC
                        POL_CODES = 'HR'
                      ELSE IF ( PIM%CONF%POLAR == PIMA__POLAR_HL ) THEN
                        POL_IND   =  2
                        POL_MODE  = PIMA__HLLC
                        POL_CODES = 'HL'
                      ELSE IF ( PIM%CONF%POLAR == PIMA__POLAR_VL ) THEN
                        POL_IND   =  4
                        POL_MODE  = PIMA__VLLC
                        POL_CODES = 'VL'
                      ELSE IF ( PIM%CONF%POLAR == PIMA__POLAR_VR ) THEN
                        POL_IND   =  2
                        POL_MODE  = PIMA__VRLC
                        POL_CODES = 'VR'
                      ELSE 
                        CALL ERR_LOG ( 7590, IUER, 'PIMA_PLOT_ACRL', 'Cannot use '// &
     &                       TRIM(PIM%CONF%POLAR)//' because the visibility file '// &
     &                      'does not contain this polarization' )
                        RETURN
                   END IF
              ELSE IF ( ( PIM%STA(PIM%OBS(IND_OBS)%STA_IND(1))%POL_TYP(1) == PIMA__POL_R .OR. &
     &                    PIM%STA(PIM%OBS(IND_OBS)%STA_IND(1))%POL_TYP(1) == PIMA__POL_L      ) .AND. &
     &                  ( PIM%STA(PIM%OBS(IND_OBS)%STA_IND(2))%POL_TYP(1) == PIMA__POL_H .OR. &
     &                    PIM%STA(PIM%OBS(IND_OBS)%STA_IND(2))%POL_TYP(1) == PIMA__POL_V      )     ) THEN
                   IF ( PIM%CONF%POLAR == PIMA__POLAR_RH ) THEN
                        POL_IND   =  1
                        POL_MODE  = PIMA__RHCL
                        POL_CODES = 'RH'
                      ELSE IF ( PIM%CONF%POLAR == PIMA__POLAR_RV ) THEN
                        POL_IND   =  3
                        POL_MODE  = PIMA__RVCL
                        POL_CODES = 'RL'
                      ELSE IF ( PIM%CONF%POLAR == PIMA__POLAR_LH ) THEN
                        POL_IND   =  2
                        POL_MODE  = PIMA__LHCL
                        POL_CODES = 'LH'
                      ELSE IF ( PIM%CONF%POLAR == PIMA__POLAR_LV ) THEN
                        POL_IND   =  4
                        POL_MODE  = PIMA__LVCL
                        POL_CODES = 'LV'
                      ELSE 
                        CALL ERR_LOG ( 7591, IUER, 'PIMA_PLOT_ACRL', 'Cannot use '// &
     &                       TRIM(PIM%CONF%POLAR)//' because the visibility file '// &
     &                      'does not contain this polarization' )
                        RETURN
                   END IF
              END IF
         END IF
!
         IF ( PIM%CONF%DEBUG_LEVEL .GE. 4 ) THEN
              WRITE ( 6, * ) 'PIMA_PLOT_ACRL-313 POL_IND= ', POL_IND, ' POL_MODE= ', POL_MODE,  ' POL_CODES= ', POL_CODES, ' PIM%NPOL= ', PIM%NPOL
         END IF
         ALLOCATE ( AC(PIM%NCHN,LFRQ,LTIM,2,PIM%NSTK), STAT=IER )
         IF ( IER .NE. 0 ) THEN
              CALL CLRCH ( STR )
              CALL IINCH ( 8*LTIM*PIM%NCHN*LFRQ*PIM%NSTK*2, STR )
              CALL ERR_LOG ( 7592, IUER, 'PIMA_PLOT_ACRL', 'Failure to '// &
     &             'allocate '//STR(1:I_LEN(STR))//' bytes of dynamic memory' )
              RETURN
         END IF
!
         ALLOCATE ( WEI_1D(PIM__MEPC,PIM%NSTK,2), STAT=IER )
         IF ( IER .NE. 0 ) THEN
              CALL CLRCH ( STR )
              CALL IINCH ( 4*PIM__MEPC*PIM%NSTK*2, STR )
              CALL ERR_LOG ( 7593, IUER, 'PIMA_PLOT_ACRL', 'Failure to '// &
     &             'allocate '//STR(1:I_LEN(STR))//' bytes of dynamic memory' )
              RETURN
         END IF
!
         ALLOCATE ( WEI(LTIM), STAT=IER )
         IF ( IER .NE. 0 ) THEN
              CALL CLRCH ( STR )
              CALL IINCH ( 8*PIM%NCHN*LFRQ, STR )
              CALL ERR_LOG ( 7594, IUER, 'PIMA_PLOT_ACRL', 'Failure to '// &
     &             'allocate '//STR(1:I_LEN(STR))//' bytes of dynamic memory' )
              RETURN
         END IF
!
! ------ Get autocorrelation data for the first station
!
         CALL ERR_PASS ( IUER, IER )
         CALL PIMA_GET_AC ( PIM, IND_OBS, LFRQ, PIM%CONF%BEG_FRQ, &
     &                      PIM%CONF%END_FRQ, PIM%CONF%FRQ_GRP, &
     &                      1, AC, WEI_1D(1,1,1), AP_LEN, IER )
         IF ( IER .NE. 0 ) THEN
              CALL CLRCH ( STR )
              CALL INCH  ( IND_OBS, STR )
              CALL ERR_LOG ( 7595, IUER, 'PIMA_PLOT_ACRL', 'Failure to '// &
     &            'get autocorrelation data for station '// &
     &            PIM%STA(PIM%OBS(IND_OBS)%STA_IND(1))%IVS_NAME// &
     &            ' of observation #'//STR )
              RETURN
         END IF
         WEI_MAX = MAXVAL ( WEI_1D(1:PIM%OBS(IND_OBS)%NUM_EPC(PIM%OBS(IND_OBS)%REF_FRG_INDS(PIM%CONF%FRQ_GRP)),POL_IND,1) )
         IF ( PIM%CONF%DEBUG_LEVEL .GE. 4 ) THEN
              WRITE ( 6, * ) '1: Sta= ', PIM%C_STA(PIM%OBS(IND_OBS)%STA_IND(1)), ' WEI_MAX= ', WEI_MAX
         END IF
        IF ( WEI_MAX == 0.0 ) THEN
              DEALLOCATE ( WEI )
              DEALLOCATE ( AC  )
              GOTO 420
         END IF
!
! ------ Get autocorrelation data for the second station
!
         CALL ERR_PASS ( IUER, IER )
         CALL PIMA_GET_AC ( PIM, IND_OBS, LFRQ, PIM%CONF%BEG_FRQ, &
     &                      PIM%CONF%END_FRQ, PIM%CONF%FRQ_GRP, &
     &                      2, AC, WEI_1D(1,1,2), AP_LEN, IER )
         IF ( IER .NE. 0 ) THEN
              CALL CLRCH ( STR )
              CALL INCH  ( IND_OBS, STR )
              CALL ERR_LOG ( 7596, IUER, 'PIMA_PLOT_ACRL', 'Failure to '// &
     &            'get autocorrelation data for station '// &
     &            PIM%STA(PIM%OBS(IND_OBS)%STA_IND(2))%IVS_NAME// &
     &            ' of observation #'//STR )
              RETURN
         END IF
         WEI_MAX = MAXVAL ( WEI_1D(1:PIM%OBS(IND_OBS)%NUM_EPC(PIM%OBS(IND_OBS)%REF_FRG_INDS(PIM%CONF%FRQ_GRP)),POL_IND,2) )
         IF ( PIM%CONF%DEBUG_LEVEL .GE. 4 ) THEN
              WRITE ( 6, * ) '2: Sta= ', PIM%C_STA(PIM%OBS(IND_OBS)%STA_IND(1)), ' WEI_MAX= ', WEI_MAX
         END IF
         IF ( WEI_MAX == 0.0 ) THEN
              DEALLOCATE ( WEI )
              DEALLOCATE ( AC  )
              GOTO 420
         END IF
!
         IF ( PIM%CONF%FRIB_AUTOCORR_CALIB .NE. PIMA__ACCR_NO ) THEN
              CALL ERR_PASS ( IUER, IER )
              IF ( POL_MODE == PIMA__RRCC .OR. &
     &             POL_MODE == PIMA__LLCC .OR. &
     &             POL_MODE == PIMA__HHLL .OR. &
     &             POL_MODE == PIMA__VVLL .OR. &
     &             POL_MODE == PIMA__HRLC .OR. &
     &             POL_MODE == PIMA__VRLC .OR. &
     &             POL_MODE == PIMA__VLLC .OR. &
     &             POL_MODE == PIMA__RHCL .OR. &
     &             POL_MODE == PIMA__LVCL      ) THEN
!
! ---------------- Parallel polarizations
!
                   CALL PIMA_ACCOR_MEAN ( PIM, IND_OBS, LTIM, 1, AC(1,1,1,1,POL_IND), WEI_1D(1,POL_IND,1), &
     &                                    AC_MEAN(1,1,POL_IND), .FALSE., IER  )
                   CALL PIMA_ACCOR_MEAN ( PIM, IND_OBS, LTIM, 2, AC(1,1,1,2,POL_IND), WEI_1D(1,POL_IND,2), &
     &                                    AC_MEAN(1,2,POL_IND), .FALSE., IER  )
                 ELSE 
!
! ---------------- Cross polarizations
!
                   CALL PIMA_ACCOR_MEAN ( PIM, IND_OBS, LTIM, 1, AC(1,1,1,1,1), WEI_1D(1,1,1), &
     &                                    AC_MEAN(1,1,1), .FALSE., IER  )
                   CALL PIMA_ACCOR_MEAN ( PIM, IND_OBS, LTIM, 2, AC(1,1,1,2,1), WEI_1D(1,1,2), &
     &                                    AC_MEAN(1,2,1), .FALSE., IER  )
                   CALL PIMA_ACCOR_MEAN ( PIM, IND_OBS, LTIM, 1, AC(1,1,1,1,2), WEI_1D(1,2,1), &
     &                                    AC_MEAN(1,1,2), .FALSE., IER  )
                   CALL PIMA_ACCOR_MEAN ( PIM, IND_OBS, LTIM, 2, AC(1,1,1,2,2), WEI_1D(1,2,2), &
     &                                    AC_MEAN(1,2,2), .FALSE., IER  )
              END IF
              IER = 0
              IF ( IER .NE. 0 ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( IND_OBS, STR )
                   CALL ERR_LOG  ( 7596, IUER, 'PIMA_PLOT_ACRL', 'Failure in an '// &
     &                 'attempt to compute averaged autcorrelation function '// &
     &                 'for the '//STR(1:I_LEN(STR))//' th observation '// &
     &                 'from input FITS-IDI file ' )
                   RETURN
              END IF
            ELSE
              AC_MEAN = 1.0
         END IF
!
         N_FRQ = 0
         I_FRQ = 0
         IND_UV1  = PIM%OBS(IND_OBS)%UV_IND(1,PIM%OBS(IND_OBS)%REF_FRG_INDS(PIM%CONF%FRQ_GRP))
         IND_FIL = PIM%UV_IND(IND_UV1)%FIL_IND
         DO 440 J4=PIM%CONF%BEG_FRQ,PIM%CONF%END_FRQ
            I_FRQ = I_FRQ + 1
            AMP_AVR(J4,1:2) = 0.0D0
            DO 450 J5=1,PIM%NCHN
               N_FRQ = N_FRQ + 1
               IF ( .NOT. FL_1ST_IF ) THEN
                    FREQ(N_FRQ) = PIM%FREQ_ARR(J5,J4,PIM%CONF%FRQ_GRP)
                  ELSE
                    FREQ(N_FRQ) = PIM%FREQ_ARR(J5,J4,PIM%CONF%FRQ_GRP) - PIM%FREQ_ARR(1,PIM%CONF%BEG_FRQ,PIM%CONF%FRQ_GRP) 
               END IF
               IF ( FREQ_OFFSET .NE. 0.0D0 ) THEN
                    FREQ(N_FRQ) = FREQ(N_FRQ) - FREQ_OFFSET
               END IF
               AMP(N_FRQ,1:2)         = 0.0D0
               CMP(N_FRQ,1:2)         = 0.0D0
               AMP_PAR(N_FRQ,1:2,1:2) = 0.0D0
               SUM_WEI1 = 0.0
               SUM_WEI2 = 0.0
               DO 460 J6=1,PIM%OBS(IND_OBS)%NUM_EPC(PIM%OBS(IND_OBS)%REF_FRG_INDS(PIM%CONF%FRQ_GRP))
                  AMP(N_FRQ,1) = AMP(N_FRQ,1) + FUDGE(1)*ABS(AC(J5,I_FRQ,J6,1,POL_IND))*WEI_1D(J6,POL_IND,1)
                  AMP(N_FRQ,2) = AMP(N_FRQ,2) + FUDGE(2)*ABS(AC(J5,I_FRQ,J6,2,POL_IND))*WEI_1D(J6,POL_IND,2)
                  CMP(N_FRQ,1) = CMP(N_FRQ,1) + AC(J5,I_FRQ,J6,1,POL_IND)
                  CMP(N_FRQ,2) = CMP(N_FRQ,2) + AC(J5,I_FRQ,J6,2,POL_IND)
                  IF ( POL_MODE == PIMA__RLCC .OR. &
     &                 POL_MODE == PIMA__LRCC .OR. &
     &                 POL_MODE == PIMA__HVLL .OR. &
     &                 POL_MODE == PIMA__VHLL      ) THEN
!
! -------------------- First station
!
                       AMP_PAR(N_FRQ,1,1) = AMP_PAR(N_FRQ,1,1) + FUDGE(1)*ABS(AC(J5,I_FRQ,J6,1,1))*WEI_1D(J6,1,1)
                       AMP_PAR(N_FRQ,2,1) = AMP_PAR(N_FRQ,2,1) + FUDGE(2)*ABS(AC(J5,I_FRQ,J6,2,1))*WEI_1D(J6,1,2)
!
! -------------------- Second station
!
                       AMP_PAR(N_FRQ,1,2) = AMP_PAR(N_FRQ,1,2) + FUDGE(1)*ABS(AC(J5,I_FRQ,J6,1,2))*WEI_1D(J6,2,1)
                       AMP_PAR(N_FRQ,2,2) = AMP_PAR(N_FRQ,2,2) + FUDGE(2)*ABS(AC(J5,I_FRQ,J6,2,2))*WEI_1D(J6,2,2)
                  END IF
                  SUM_WEI1 = SUM_WEI1 + WEI_1D(J6,POL_IND,1)
                  SUM_WEI2 = SUM_WEI2 + WEI_1D(J6,POL_IND,2)
 460           CONTINUE
               IF ( SUM_WEI1 > PIM%CONF%FRIB_AUTOCORR_THRESHOLD ) THEN
                    IF ( POL_MODE == PIMA__RRCC .OR. &
     &                   POL_MODE == PIMA__LLCC .OR. &
     &                   POL_MODE == PIMA__HHLL .OR. &
     &                   POL_MODE == PIMA__VVLL .OR. &
     &                   POL_MODE == PIMA__HRLC .OR. &
     &                   POL_MODE == PIMA__VLLC .OR. &
     &                   POL_MODE == PIMA__RHCL .OR. &
     &                   POL_MODE == PIMA__LVCL      ) THEN
!
! ----------------------- Parallel polarizations
!
                         AMP(N_FRQ,1) = AMP(N_FRQ,1)/SUM_WEI1/AC_MEAN(I_FRQ,1,POL_IND)
                       ELSE 
!
! ----------------------- Cross polarizations
!
! ----------------------- First station
!
                          AMP_1 = AMP_PAR(N_FRQ,1,1)/SUM_WEI1/AC_MEAN(I_FRQ,1,1)
                          AMP_2 = AMP_PAR(N_FRQ,2,1)/SUM_WEI2/AC_MEAN(I_FRQ,2,1)
                          AMP(N_FRQ,1) = AMP(N_FRQ,1)/SUM_WEI1/DSQRT(AMP_1*AMP_2)
                          PHS(N_FRQ,1) = PHAS_CMPL_R4 ( CMP(N_FRQ,1) )
!
! ----------------------- Second station
!
                          AMP_1 = AMP_PAR(N_FRQ,1,2)/SUM_WEI1/AC_MEAN(I_FRQ,1,2)
                          AMP_2 = AMP_PAR(N_FRQ,2,2)/SUM_WEI2/AC_MEAN(I_FRQ,2,2)
                          AMP(N_FRQ,2) = AMP(N_FRQ,2)/SUM_WEI1/DSQRT(AMP_1*AMP_2)
                          PHS(N_FRQ,2) = PHAS_CMPL_R4 ( CMP(N_FRQ,2) )
                     END IF
               END IF
               IF ( SUM_WEI1 > PIM%CONF%FRIB_AUTOCORR_THRESHOLD ) THEN
                    IF ( POL_MODE == PIMA__RRCC .OR. &
     &                   POL_MODE == PIMA__LLCC .OR. &
     &                   POL_MODE == PIMA__HHLL .OR. &
     &                   POL_MODE == PIMA__VVLL .OR. &
     &                   POL_MODE == PIMA__HRLC .OR. &
     &                   POL_MODE == PIMA__VLLC .OR. &
     &                   POL_MODE == PIMA__RHCL .OR. &
     &                   POL_MODE == PIMA__LVCL      ) THEN
                         AMP(N_FRQ,2) = AMP(N_FRQ,2)/SUM_WEI2/AC_MEAN(I_FRQ,2,POL_IND)
                     END IF
               END IF
               CALL CLRCH ( STR_IP )
               CALL GETENVAR ( 'PIMAVAR_AP_IND', STR_IP )
               IF ( ILEN(STR_IP)>0 ) THEN
                    CALL CHIN ( STR_IP(1:8), ICHN )
                    IF ( ICHN < 1    ) ICHN = 1
                    IF ( ICHN > LTIM ) ICHN = LTIM
                    CALL INCH ( ICHN, STR_IP )
                    STR_IP = 'AP_ind: '//STR_IP
                    AMP(N_FRQ,1) = ABS(AC(J5,I_FRQ,ICHN,1,POL_IND))
                    AMP(N_FRQ,2) = ABS(AC(J5,I_FRQ,ICHN,2,POL_IND))
               END IF
               IF ( POL_MODE == PIMA__RRCC .OR. &
     &              POL_MODE == PIMA__LLCC .OR. &
     &              POL_MODE == PIMA__HHLL .OR. &
     &              POL_MODE == PIMA__VVLL .OR. &
     &              POL_MODE == PIMA__HRLC .OR. &
     &              POL_MODE == PIMA__VLLC .OR. &
     &              POL_MODE == PIMA__RHCL .OR. &
     &              POL_MODE == PIMA__LVCL      ) THEN
!
                    AMP_AVR(J4,1) = AMP_AVR(J4,1) + AMP(N_FRQ,1)/PIM%NCHN*AC_MEAN(I_FRQ,1,POL_IND)
                    AMP_AVR(J4,2) = AMP_AVR(J4,2) + AMP(N_FRQ,2)/PIM%NCHN*AC_MEAN(I_FRQ,2,POL_IND)
                  ELSE 
                    AMP_AVR(J4,1) = AMP_AVR(J4,1) + AMP(N_FRQ,1)/PIM%NCHN
                    AMP_AVR(J4,2) = AMP_AVR(J4,2) + AMP(N_FRQ,2)/PIM%NCHN
                    AC_MEAN(I_FRQ,1,POL_IND) = SQRT ( AC_MEAN(I_FRQ,1,1)*AC_MEAN(I_FRQ,1,2) )
                    AC_MEAN(I_FRQ,2,POL_IND) = SQRT ( AC_MEAN(I_FRQ,2,1)*AC_MEAN(I_FRQ,2,2) )
               END IF
 450        CONTINUE
 440     CONTINUE
!
 910     CONTINUE
         DO 470 J7=1,2
            CALL NOUT ( SIZEOF(DIA(J7)), DIA(J7) )
            DIA(J7)%IDEV = IDEV
            DIA(J7)%NCLR = 1
            DIA(J7)%NPOI(1)   = N_FRQ
            DIA(J7)%ADR_X8(1) = LOC(FREQ)
            IF ( MODE == 'amp' ) THEN
                 DIA(J7)%ADR_Y8(1) = LOC(AMP(1,J7))
               ELSE 
                 DIA(J7)%ADR_Y8(1) = LOC(PHS(1,J7))
            END IF
            DIA(J7)%ADR_E8(1) = 0
            DIA(J7)%LER(1)    = .FALSE.
            DIA(J7)%ICOL(1)   = ICL1
            DIA(J7)%IBST(1)   = 0
            DIA(J7)%ILST(1)   = 2
            DIA(J7)%IOST(1)   = 1
            DIA(J7)%IPST(1)   = 4
            DIA(J7)%IWST(1)   = 1
            DIA(J7)%ICLR      = 1
            DIA(J7)%XMIN   =  1.0
            DIA(J7)%XMAX   = -1.0
            DIA(J7)%ARG_UNITS = 'Frequency in Hz'
            DIA(J7)%YMIN   =  1.0
            DIA(J7)%YMAX   = -1.0
            CALL CLRCH ( STR )
            CALL INCH  ( IND_OBS, STR )
            DIA(J7)%ZAG    = 'Autocorrelation for '//PIM%STA(IND_STA(J7))%IVS_NAME//' '// &
     &                       POL_CODES(J7:J7)//' at obs #'//TRIM(STR)//' '//TRIM(STR_IP)
            DIA(J7)%NAME   = PIM%STA(IND_STA(J7))%IVS_NAME//' '//POL_CODES(J7:J7)
            DIA(J7)%ITRM   = 0
            DIA(J7)%IBATCH = 0
            DIA(J7)%STATUS = DIA__DEF
            TITS(J7) = 'AC at '//PIM%STA(IND_STA(J7))%IVS_NAME// &
     &                       ' '//POL_CODES(J7:J7)
!
            IF ( PIM%CONF%DEBUG_LEVEL .GE. 2 ) THEN
                 I_FRQ = 0
                 DO 480 J8=PIM%CONF%BEG_FRQ,PIM%CONF%END_FRQ
                    I_FRQ = I_FRQ + 1
                    WRITE ( 6, 220 ) PIM%STA(IND_STA(J7))%IVS_NAME, J8, &
     &                               PIM%FRQ(J8,PIM%CONF%FRQ_GRP)%FREQ*1D-6, &
     &                               AMP_AVR(J8,J7), AC_MEAN(I_FRQ,J7,POL_IND)
 220                FORMAT ( 'Sta: ', A, 2X, ' IF: ', I2, ' Freq: ', F9.2, &
     &                       ' MHz   Amp_avr: ', F8.4, ' AC_mean: ', F8.4 )
 480             CONTINUE
                 WRITE ( 6, * ) ' '
            END IF
 470     CONTINUE
         NC = 1
         NR = 2
         IF ( POL_MODE == PIMA__RRCC .OR. &
     &        POL_MODE == PIMA__LLCC .OR. &
     &        POL_MODE == PIMA__HHLL .OR. &
     &        POL_MODE == PIMA__VVLL .OR. &
     &        POL_MODE == PIMA__HRLC .OR. &
     &        POL_MODE == PIMA__VLLC .OR. &
     &        POL_MODE == PIMA__RHCL .OR. &
     &        POL_MODE == PIMA__LVCL      ) THEN
!
              BUTTON_LET(1)  = 'Xx'
              BUTTON_NAME(1) = 'Exit plot'
            ELSE
              IF ( MODE == 'amp' ) THEN
                   BUTTON_LET(1)  = 'Pp'
                   BUTTON_NAME(1) = 'Phase'
                   BUTTON_LET(2)  = 'Xx'
                   BUTTON_NAME(2) = 'Exit plot'
                   BUTTON_LET(3)  = 'Qq'
                   BUTTON_NAME(3) = 'Quit plot'
                 ELSE 
                   BUTTON_LET(1)  = 'Aa'
                   BUTTON_NAME(1) = 'Amplitude'
                   BUTTON_LET(2)  = 'Xx'
                   BUTTON_NAME(2) = 'Exit plot'
                   BUTTON_LET(3)  = 'Qq'
                   BUTTON_NAME(3) = 'Quit plot'
              END IF
         END IF
!
         CALL CLRCH ( STR  )
         CALL CLRCH ( STR1 )
         CALL INCH  ( IND_OBS, STR1 )
         STR = MJDSEC_TO_DATE ( PIM%MJD_0, PIM%TAI_0 + &
     &         PIM%TIM_R8(PIM%SCA(IND_SCA)%TIM_IND+PIM%SCA(IND_SCA)%NUM_EPC-1), -2 )
         COMMON_TIT = 'AC Obs #'//STR1(1:I_LEN(STR1))//' '//STR(1:21)// &
     &                ' Exp '//PIM%OBS_CODE
!
         ICODE = 0
         CALL ERR_PASS ( IUER, IER )
         CALL MULTI_DIAGI ( COMMON_TIT, PIM%NSTA, NC, NR, TITS, MPB, &
     &                      BUTTON_NAME, BUTTON_LET, PREF_NAME, DIA, &
     &                      ICODE, IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 7597, IUER, 'PIMA_PLOT_ACRL', 'Failure to make '// &
     &            'a plot of autocorrelation' )
              RETURN
         END IF
!
         IF ( POL_MODE == PIMA__RRCC .OR. &
     &        POL_MODE == PIMA__LLCC .OR. &
     &        POL_MODE == PIMA__HHLL .OR. &
     &        POL_MODE == PIMA__VVLL .OR. &
     &        POL_MODE == PIMA__HRLC .OR. &
     &        POL_MODE == PIMA__VLLC .OR. &
     &        POL_MODE == PIMA__RHCL .OR. &
     &        POL_MODE == PIMA__LVCL      ) THEN
              IF ( ICODE == 0  .OR.  ICODE == 1 ) THEN
                   DEALLOCATE ( AC     )
                   DEALLOCATE ( WEI    )
                   DEALLOCATE ( WEI_1D )
                   CALL ERR_LOG ( 0, IUER )
                   RETURN
              END IF
            ELSE 
              IF ( ICODE == 1 ) THEN
                   IF ( MODE == 'amp' ) THEN
                        MODE = 'phs'
                      ELSE
                        MODE = 'amp'
                   END IF
                   GOTO 910
              END IF
              IF ( ICODE == 0  .OR.  ICODE == 2 ) THEN
                   DEALLOCATE ( AC     )
                   DEALLOCATE ( WEI    )
                   DEALLOCATE ( WEI_1D )
                   CALL ERR_LOG ( 0, IUER )
                   RETURN
              END IF
         END IF
         DEALLOCATE ( AC     )
         DEALLOCATE ( WEI    )
         DEALLOCATE ( WEI_1D )
 420  CONTINUE
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  PIMA_PLOT_ACRL  !#!
#endif
