      SUBROUTINE PIMA_GET_OBS ( PIM, VTD, IND_OBS, POL_MODE, LPOL, &
     &                          FL_AC, FL_AVR, FL_FRI, FRI_STS, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine PIMA_GET_OBS reads the input file with visibilities,       *
! *   extracts visibilities for the observations with index IND_OBS,     *
! *   applies all necessary calibration and computes IF averaged and     *
! *   band averaged visibilities. Visibility arrays are put in           *
! *   PIM%OBS(IND_OBS)%UV (all the data) PIM%OBS(IND_OBS)%UV_IF          *
! *   (IF avaraged), and PIM%OBS(IND_OBS)%UV_BAND (band avareged).       *
! *                                                                      *
! *   If FL_FRI is .TRUE., then PIMA_GET_OBS applies phase rotation for  *
! *   group delay, group delay rate, phase delay rate and total fringe   *
! *   phase that have been computed before and stored in the fringe file.*
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *      PIM ( PIMA__TYP ) -- Object with information related to program *
! *                           PIMA.                                      *
! *      VTD ( VTD__TYP  ) -- Object with information related to package *
! *                           VTD for computed apriori path delay.       *
! *  IND_OBS ( INTEGER*4 ) -- Observation index.                         *
! * POL_MODE ( INTEGER*4 ) -- Code of the polarization mode.             *
! *    LPOL  ( INTEGER*4 ) -- The number of polarizations.               *
! *  FL_AC   ( LOGICAL*1 ) -- Flag whether to keep autocorrelations.     *
! *                           If .FALSE. PIM%OBS(IND_OBS)%AC arrays      *
! *                           is deallocated in order to free memory.    *
! *  FL_AVR  ( LOGICAL*1 ) -- Flag whether to compute averaged           *
! *                           visibility over IF and band.               *
! *  FL_FRI  ( LOGICAL*1 ) -- Flag whether to apply phase rotation for   *
! *                           group delay, group delay rate, phase delay *
! *                           rate and total fringe phase that are       *
! *                           results of fringe fitting. Averaging       *
! *                           visibilities with FL_FRI = .FALSE. will be *
! *                           most likey go bad.                         *
! *                                                                      *
! * _________________________ Output parameters: _______________________ *
! *                                                                      *
! *                                                                      *
! * FRI_STS ( INTEGER*4 ) -- fringe fitting status. Non-zero in a case   *
! *                          of errors.                                  *
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
! * ### 18-NOV-2014   PIMA_GET_OBS  v2.3 (c)  L. Petrov  25-SEP-2020 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'pima.i'
      INCLUDE   'vtd.i'
      TYPE     ( PIMA__TYPE  ) :: PIM
      TYPE     ( VTD__TYPE   ) :: VTD
      LOGICAL*1  FL_AC, FL_AVR, FL_FRI
      INTEGER*4  IND_OBS, POL_MODE, LPOL, FRI_STS, IUER
      INTEGER*4  J1, J2, J3, J4, J5, J6, J7, J8, J9, J10, J11, J12, J13, &
     &           J14, J15, J16, J17, J18, FRG_IND, IND_BND, &
     &           IND_BPS_BAS, LTIM, LFRQ, BPS_SGN, IFRQ, IND_STA(2), &
     &           UV_IND, IND_FRA, FRI_STS_GET_UV, POL_CROSS_IND(4), IER
      CHARACTER  STR*128, STR1*128, PIMA_FRI_USED_VERS*24
      LOGICAL*1  FL_BPASS, FL_PBP
      REAL*4     WEI_1D(PIM__MEPC,PIM__MPLR,0:2), &
     &           PHAS_R4, AMPL_R4, WEI_MAX, PBP_PHS, PBP_AMP, &
     &           WPOI_IF(PIM__MPLR), WPOI_BAND(PIM__MPLR), PHS_MOD
      REAL*8     SNR, BDW_EFF_BAND(PIM__MOBS), NRML_CROSS_SPLT, &
     &           SUM_WEI_1D, NRML_BEAM_ATT(2), AC_THR
      COMPLEX*8  BPASS_C8, PBP, DRF_BAND(PIM__MPLR), DRF_IF(PIM__MPLR)
      INTEGER*1  MASK_I1
      LOGICAL*1  FL_ALL_WEIGHTS_ONE, FL_NODATA, FL_TSRF_ONE
      REAL*8,    ALLOCATABLE :: TIM_ARR(:)
      REAL*4,    ALLOCATABLE :: WEI_FRQ_CHN(:,:), WEI_1D_AVR(:,:,:,:)
      REAL*8     PH_DEL_APSO, PH_RAT_APSO, PH_ACC_APSO, PHS_APSO_MAX, &
     &           TIM_FRT, TIM_TOL, PHS_APSO_LIM
      REAL*8      PIMA__PHS_APSO_LIM, PIMA__APSO_LTIM_LIM 
      PARAMETER ( PIMA__PHS_APSO_LIM  = 1.D-9 )
      PARAMETER ( PIMA__APSO_LTIM_LIM = 5 )
#ifdef GNU
      INTEGER*4     NTHR, NTHR_SAVED
#else
      ADDRESS__TYPE NTHR, NTHR_SAVED
#endif
!
      REAL*8,    EXTERNAL :: PIMA_BEAM_ATT, PIMA_AUTC_AMPL_NRML
      INTEGER*4, EXTERNAL :: ILEN, I_LEN, ADD_LIS, LTM_DIF
      LOGICAL*4, EXTERNAL :: IS_R4_NAN
      REAL*4,    EXTERNAL :: ATAN_CS_R4, PHAS_CMPL_R4
      CHARACTER, EXTERNAL :: MJDSEC_TO_DATE*30, GET_CDATE*19
!
! --- Check whether IND_OBS in a range
!
      FRI_STS = 0
      IF ( IND_OBS < 1 .OR. IND_OBS > PIM%NOBS ) THEN
           CALL CLRCH ( STR  )
           CALL CLRCH ( STR1 )
           CALL INCH  ( IND_OBS,  STR  )
           CALL INCH  ( PIM%NOBS, STR1 )
           CALL ERR_LOG ( 9111, IUER, 'PIMA_GET_OBS', 'Wrong observation '// &
     &         'index: '//STR(1:I_LEN(STR))//' -- it should be in range '// &
     &         '[1, '//STR1(1:I_LEN(STR1))//']' )
           RETURN 
      END IF
!
      CALL GETENVAR ( 'PIMAVAR_ALL_WEIGHTS_ONE', STR )
      IF ( STR == 'YES' .OR. STR == 'yes' ) THEN
           FL_ALL_WEIGHTS_ONE = .TRUE.
         ELSE 
           FL_ALL_WEIGHTS_ONE = .FALSE.
      END IF
!
      CALL GETENVAR ( 'PIMAVAR_TSRF_ONE', STR )
      IF ( STR == 'YES' .OR. STR == 'yes' ) THEN
           FL_TSRF_ONE = .TRUE.
         ELSE 
           FL_TSRF_ONE = .FALSE.
      END IF
!
      NTHR = PIM%CONF%NUM_THREADS
      CALL OMP_SET_NUM_THREADS ( %VAL(NTHR) )
!
      IND_BND = 1
      FRG_IND = PIM%OBS(IND_OBS)%REF_FRG_INDS(PIM%CONF%FRQ_GRP)
      SNR = PIM%OBS(IND_OBS)%AMPL(PIMA__DRF,IND_BND)/PIM%OBS(IND_OBS)%NOISE(IND_BND)
      LTIM = PIM%OBS(IND_OBS)%NUM_EPC(FRG_IND)
      LFRQ = PIM%CONF%END_FRQ - PIM%CONF%BEG_FRQ + 1
!
      IF ( FL_FRI .AND. PIM%FRI_STATUS .NE. PIMA__LOADED ) THEN
!
! -------- Get fringe results and put them in appropriate slots of PIM object
!
           CALL ERR_PASS ( IUER, IER )
           CALL PIMA_FRI_READ ( PIM, IND_BND, PIMA_FRI_USED_VERS, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 9112, IUER, 'PIMA_GET_OBS', 'Error in an '// &
     &              'attempt to read results of fringing from the fringe file '// &
     &               PIM%CONF%FRINGE_FILE )
                RETURN
           END IF
      END IF
!
      IF ( FL_FRI .AND. PIM%THE_STATUS .NE. PIMA__LOADED ) THEN
!
! -------- Compute theoretical path delay for all observations (and UV coordinates as well)
!
           CALL ERR_PASS ( IUER, IER )
           CALL PIMA_THEO ( PIM, VTD, 'OBS_SRT', '1ST_STA', IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 9113, IUER, 'PIMA_GET_OBS', 'Error in an attempt '// &
     &              'to compute theoretical path delays' )
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
      CALL GETENVAR ( 'PIMAVAR_PHS_APSO_LIM', STR )
      IF ( ILEN(STR) > 0 ) THEN
           READ ( UNIT=STR, FMT='(F12.5)', IOSTAT=IER ) PHS_APSO_LIM
           IF ( IER .NE. 0 ) PHS_APSO_LIM = PIMA__PHS_APSO_LIM
         ELSE
           PHS_APSO_LIM = PIMA__PHS_APSO_LIM
      END IF
!     
      LFRQ = PIM%CONF%END_FRQ - PIM%CONF%BEG_FRQ + 1
      LTIM = PIM%OBS(IND_OBS)%NUM_EPC(PIM%OBS(IND_OBS)%REF_FRG_INDS(PIM%CONF%FRQ_GRP))
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
   if  ( iuer == -555 ) then                                              ! %%%%%%%%%%%
         write ( 6, * ) 'PIMA_GETO_IKBS-195 fl_bpass= ', fl_bpass, ' fl_pbp= ', fl_pbp     ! %%%%%%%%%%%
   end if                                                               ! %%%%%%%%%%%
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!
! --- Allocate memory
!
      IF ( ASSOCIATED ( PIM%OBS(IND_OBS)%UV ) ) DEALLOCATE ( PIM%OBS(IND_OBS)%UV )
      ALLOCATE ( PIM%OBS(IND_OBS)%UV(PIM%NCHN,LFRQ,LTIM,LPOL), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( 8*LTIM*PIM%NCHN*LFRQ*LPOL, STR )
           CALL ERR_LOG ( 9114, IUER, 'PIMA_GET_OBS', 'Failure to '// &
     &         'allocate '//STR(1:I_LEN(STR))//' bytes of dynamic '// &
     &         'memory for array UV' )
           RETURN
      END IF
      PIM%OBS(IND_OBS)%UV = ( 0.0, 0.0 )
!
      IF ( ASSOCIATED ( PIM%OBS(IND_OBS)%UV_IF ) ) DEALLOCATE ( PIM%OBS(IND_OBS)%UV_IF )
      ALLOCATE ( PIM%OBS(IND_OBS)%UV_IF(LTIM,LFRQ,LPOL), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( 8*LTIM*LFRQ*LPOL, STR )
           CALL ERR_LOG ( 9115, IUER, 'PIMA_GET_OBS', 'Failure to '// &
     &         'allocate '//STR(1:I_LEN(STR))//' bytes of dynamic '// &
     &         'memory for array UV' )
           RETURN
      END IF
      PIM%OBS(IND_OBS)%UV_IF = ( 0.0, 0.0 )
!
      IF ( ASSOCIATED ( PIM%OBS(IND_OBS)%UV_BAND ) ) DEALLOCATE ( PIM%OBS(IND_OBS)%UV_BAND )
      ALLOCATE ( PIM%OBS(IND_OBS)%UV_BAND(LTIM,LPOL), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( 8*LTIM*LPOL, STR )
           CALL ERR_LOG ( 9116, IUER, 'PIMA_GET_OBS', 'Failure to '// &
     &         'allocate '//STR(1:I_LEN(STR))//' bytes of dynamic '// &
     &         'memory for array UV' )
           RETURN
      END IF
      PIM%OBS(IND_OBS)%UV_BAND = ( 0.0, 0.0 )
!
      IF ( ASSOCIATED ( PIM%OBS(IND_OBS)%WEI_1D ) ) DEALLOCATE ( PIM%OBS(IND_OBS)%WEI_1D )
      ALLOCATE ( PIM%OBS(IND_OBS)%WEI_1D(LTIM,LPOL), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( 4*LTIM*LPOL, STR )
           CALL ERR_LOG ( 9117, IUER, 'PIMA_GET_OBS', 'Failure to '// &
     &         'allocate '//STR(1:I_LEN(STR))//' bytes of dynamic '// &
     &         'memory for array WEI_1D' )
           RETURN
      END IF
      PIM%OBS(IND_OBS)%WEI_1D = 0.0
!
      IF ( ASSOCIATED ( PIM%OBS(IND_OBS)%TSRF ) ) DEALLOCATE ( PIM%OBS(IND_OBS)%TSRF )
      ALLOCATE ( PIM%OBS(IND_OBS)%TSRF(LFRQ,2,PIM%NSTK), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( 4*LFRQ,2,PIM%NSTK, STR )
           CALL ERR_LOG ( 9118, IUER, 'PIMA_GET_OBS', 'Failure to '// &
     &         'allocate '//STR(1:I_LEN(STR))//' bytes of dynamic '// &
     &         'memory for array TSRF' )
           RETURN
      END IF
      PIM%OBS(IND_OBS)%TSRF = 0.0
!
      ALLOCATE ( WEI_FRQ_CHN(PIM%NCHN,PIM%NFRQ), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( 8*PIM%NCHN*PIM%NFRQ, STR )
           CALL ERR_LOG ( 9119, IUER, 'PIMA_GET_OBS', 'Failure to '// &
     &         'allocate '//STR(1:I_LEN(STR))//' bytes of dynamic '// &
     &         'memory for array WEI_FRQ_CHN' )
           RETURN
      END IF
      WEI_FRQ_CHN = 1.0D0
      IF ( FL_AVR .AND. FL_AC ) THEN
           IF ( ASSOCIATED ( PIM%OBS(IND_OBS)%AC_AVR_TIM ) ) DEALLOCATE ( PIM%OBS(IND_OBS)%AC_AVR_TIM )
!
! -------- Last dimension runs over stations
!
           ALLOCATE ( PIM%OBS(IND_OBS)%AC_AVR_TIM(PIM%NCHN,LFRQ,2,PIM%NSTK), STAT=IER ) 
           IF ( IER .NE. 0 ) THEN
                CALL CLRCH ( STR )
                CALL IINCH ( 4*PIM%NCHN*LFRQ*2*PIM%NSTK, STR )
                CALL ERR_LOG ( 9120, IUER, 'PIMA_GET_OBS', 'Failure to '// &
     &              'allocate '//STR(1:I_LEN(STR))//' bytes of dynamic '// &
     &              'memory for array AC_AVR_TIM' )
                RETURN
           END IF
           PIM%OBS(IND_OBS)%AC_AVR_TIM = 0.0
         ELSE
!
! -------- Last dimension runs over stations
!
           ALLOCATE ( PIM%OBS(IND_OBS)%AC_AVR_TIM(1,1,1,1), STAT=IER ) 
           IF ( IER .NE. 0 ) THEN
                CALL CLRCH ( STR )
                CALL IINCH ( 4, STR )
                CALL ERR_LOG ( 9121, IUER, 'PIMA_GET_OBS', 'Failure to '// &
     &              'allocate '//STR(1:I_LEN(STR))//' bytes of dynamic '// &
     &              'memory for array AC_AVR_TIM' )
                RETURN
           END IF
           PIM%OBS(IND_OBS)%AC_AVR_TIM = 0.0
      END IF
      IF ( FL_AC ) THEN
!
! -------- Last dimension runs over stations
!
           ALLOCATE ( WEI_1D_AVR(PIM%NCHN,LFRQ,2,PIM%NSTK) )
           IF ( IER .NE. 0 ) THEN
                CALL CLRCH ( STR )
                CALL IINCH ( 4*PIM%NCHN*LFRQ*2*PIM%NSTK, STR )
                CALL ERR_LOG ( 9122, IUER, 'PIMA_GET_OBS', 'Failure to '// &
     &              'allocate '//STR(1:I_LEN(STR))//' bytes of dynamic '// &
     &              'memory for array AC_AVR_TIM' )
                RETURN
           END IF
           WEI_1D_AVR = 0.0
         ELSE
!
! -------- Last dimension runs over stations
!
           ALLOCATE ( WEI_1D_AVR(1,1,1,1) )
           IF ( IER .NE. 0 ) THEN
                CALL CLRCH ( STR )
                CALL IINCH ( 4, STR )
                CALL ERR_LOG ( 9123, IUER, 'PIMA_GET_OBS', 'Failure to '// &
     &              'allocate '//STR(1:I_LEN(STR))//' bytes of dynamic '// &
     &              'memory for array AC_AVR_TIM' )
                RETURN
           END IF
      END IF
!
      IF ( PIM%CONF%L_WVR > 0  .AND. &
     &     PIM%CONF%WVR_USE .NE. PIMA__WVR_NO ) THEN
!
! -------- We see that the user requested to compute WVR phases.
! -------- Let us do it for the IND_OBS -th observations by interpolating
! -------- and smoothing original WVR data
!
           CALL ERR_PASS ( IUER, IER )
           CALL PIMA_WVR_TO_OBS ( PIM, IND_OBS, IER ) 
           IF ( IER .NE. 0 ) THEN
                CALL CLRCH ( STR )
                CALL INCH  ( IND_OBS, STR )
                CALL ERR_LOG ( 9124, IUER, 'PIMA_GET_OBS', 'Error '// &
     &              'during an attempt to apply WVR phase to '// &
     &              'observation #'//STR )
                RETURN 
           END IF
         ELSE 
           PIM%OBS(IND_OBS)%WVR_FLAG = 0
      END IF
!
      FL_NODATA = .FALSE.
      IF ( PIM%CONF%FRIB_AUTOCORR_CALIB .NE. PIMA__ACCR_NO ) THEN
           IF ( ASSOCIATED ( PIM%OBS(IND_OBS)%AC ) ) DEALLOCATE ( PIM%OBS(IND_OBS)%AC )
           ALLOCATE ( PIM%OBS(IND_OBS)%AC(PIM%NCHN,LFRQ,LTIM,2,PIM%NSTK), STAT=IER )
           IF ( IER .NE. 0 ) THEN
                CALL CLRCH ( STR )
                CALL IINCH ( 8*PIM%NCHN*LFRQ*LTIM*PIM%NSTK, STR )
                CALL ERR_LOG ( 9125, IUER, 'PIMA_GET_OBS', 'Failure to '// &
     &              'allocate '//STR(1:I_LEN(STR))//' bytes of dynamic '// &
     &              'memory for array AC' )
                RETURN
           END IF
           PIM%OBS(IND_OBS)%AC = 0.0
!
           IF ( ASSOCIATED ( PIM%OBS(IND_OBS)%AC_MEAN ) ) DEALLOCATE ( PIM%OBS(IND_OBS)%AC_MEAN )
           ALLOCATE ( PIM%OBS(IND_OBS)%AC_MEAN(LFRQ,2,PIM%NSTK), STAT=IER )
           IF ( IER .NE. 0 ) THEN
                CALL CLRCH ( STR )
                CALL IINCH ( 4*LFRQ*2*PIM__MPLR, STR )
                CALL ERR_LOG ( 9126, IUER, 'PIMA_GET_OBS', 'Failure to '// &
     &              'allocate '//STR(1:I_LEN(STR))//' bytes of dynamic '// &
     &              'memory for array AC_MEAN' )
                RETURN
           END IF
           PIM%OBS(IND_OBS)%AC_MEAN = 0.0
!
! -------- Cycle over stations: 1st and 2nd
!
           DO 420 J2=1,2
!     
! ----------- Get autocorrelation data for the J2-th station
!     
              CALL ERR_PASS ( IUER, IER )
              CALL PIMA_GET_AC ( PIM, IND_OBS, LFRQ, PIM%CONF%BEG_FRQ, &
     &                           PIM%CONF%END_FRQ, PIM%CONF%FRQ_GRP, J2, &
     &                           PIM%OBS(IND_OBS)%AC, &
     &                           WEI_1D(1,1,J2), PIM%OBS(IND_OBS)%AP_LEN, IER  )
              IF ( IER .NE. 0 ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( IND_OBS, STR )
                   CALL CLRCH ( STR )
                   CALL INCH  ( J2, STR )
                   CALL ERR_PASS ( IUER, IER )
                   CALL ERR_LOG  ( 9127, IUER, 'PIMA_GET_OBS', 'Failure to '// &
     &                 'get autocorrelation data for the '//TRIM(STR1)//'th station '// &
     &                 'for the '//STR(1:I_LEN(STR))//'th observation '// &
     &                 'from the input FITS-IDI file ' )
                   IF ( PIM%CONF%FRINGE_ERRORS == PIMA__ERR_STOP ) THEN
                        CALL ERR_PASS ( IER, IUER )
                        RETURN
                   END IF
              END IF
              IF ( FL_ALL_WEIGHTS_ONE ) WEI_1D = 1.0
!     
! ----------- Check whether the data have been received
!     
              IF ( .NOT. FL_NODATA ) THEN
                   WEI_MAX = MAXVAL ( WEI_1D(1:PIM%OBS(IND_OBS)%NUM_EPC(PIM%OBS(IND_OBS)%REF_FRG_INDS(PIM%CONF%FRQ_GRP)),1:PIM%NSTK,J2) )
                   IF ( WEI_MAX == 0.0 ) FL_NODATA = .TRUE.
              END IF
!     
              IF ( FL_NODATA ) THEN
                   IF ( PIM%CONF%DEBUG_LEVEL .GE. 2 ) THEN
                        WRITE ( 6, 220 ) IND_OBS, 1
 220                    FORMAT ( 'PIMA_FRINGE Obs ', I6, &
     &                           ' All weights of autocorrelation data ', &
     &                           'for station ', I1, ' are zero'  )
                   END IF
!     
! ---------------- Alas, no 1st station autocorrelation data have been received
!     
                   FRI_STS = IBSET ( FRI_STS, FAI__PIM )
                   FRI_STS = IBSET ( FRI_STS, NDA__PIM )
                   FRI_STS = IBSET ( FRI_STS, NDT__PIM )
!
! ---------------- Set the error status and leave
!
                   CALL ERR_PASS  ( 8001, IUER )
                   RETURN 
              END IF
!     
              DO 430 J3=1,PIM%NSTK
!     
! -------------- Re-normalize spectrum of the autocorrlelation function and
! -------------- compute the mean autotocorrelation for each IF ( AC_MEAN )
!     
                 CALL ERR_PASS ( IUER, IER )
                 CALL PIMA_ACCOR_MEAN ( PIM, IND_OBS, LTIM, J2, &
     &                                  PIM%OBS(IND_OBS)%AC(1,1,1,J2,J3), &
     &                                  WEI_1D(1,J3,J2), &
     &                                  PIM%OBS(IND_OBS)%AC_MEAN(1,J2,J3), .FALSE., IER  )
                 IF ( IER .NE. 0 ) THEN
                      CALL CLRCH ( STR )
                      CALL INCH  ( IND_OBS, STR )
                      CALL ERR_LOG ( 9128, IUER, 'PIMA_GET_OBS', 'Failure in an '// &
     &                    'attempt to compute averaged autcorrelation function '// &
     &                    'for the '//STR(1:I_LEN(STR))//' th observation '// &
     &                    'from input FITS-IDI file ' )
                      RETURN
                 END IF
!
                 IFRQ = 0
                 DO 440 J4=PIM%CONF%BEG_FRQ,PIM%CONF%END_FRQ
!
! ----------------- Compute renormalizaion factors for autocorrelation that supposed
! ----------------- to correct the fringe amplitude due to the bias in effective Tsys
! ----------------- with respect to the measured Tsys that emerges when only a fraction 
! ----------------- of the bandwidth used due to masking because autocorrelation spectrum 
! ----------------- is not uniform
!
                    IFRQ = IFRQ + 1
                    IF ( J3 .EQ. 1 .OR. J3 .EQ. 2 ) THEN
                         AC_THR = PIM%CONF%FRIB_AUTOCORR_THRESHOLD
                       ELSE 
                         AC_THR = PIMA__AMP_MIN
                    END IF
!
                    IF ( .NOT. FL_TSRF_ONE ) THEN
                         CALL ERR_PASS ( IUER, IER )
                         PIM%OBS(IND_OBS)%TSRF(IFRQ,J2,J3) = PIMA_AUTC_AMPL_NRML ( PIM,   &
     &                                         IND_OBS, INT(PIM%OBS(IND_OBS)%STA_IND(J2),KIND=4), &
     &                                         IFRQ, PIM%OBS(IND_OBS)%NUM_EPC(FRG_IND), &
     &                                         PIM%OBS(IND_OBS)%AC(1,1,1,J2,J3), &
     &                                         WEI_1D(1,J3,J2), AC_THR, IER )
                         IF ( IER .NE. 0 ) THEN
                              CALL CLRCH   ( STR ) 
                              CALL INCH    ( IND_OBS, STR )
                              CALL ERR_LOG ( 9129, IUER, 'PIMA_GET_OBS', 'Failure in '// &
     &                            'atuocorrelation renormalization for station '// &
     &                             PIM%C_STA(PIM%OBS(IND_OBS)%STA_IND(1))//' during '// &
     &                            'processing the '//STR(1:I_LEN(STR))//' -th observation' )
                              RETURN 
                         END IF
                       ELSE
                         PIM%OBS(IND_OBS)%TSRF(IFRQ,J2,J3) = 1.0
                    END IF
 440             CONTINUE 
 430          CONTINUE 
 420       CONTINUE 
           IF ( .NOT. FL_AC ) THEN
                DEALLOCATE ( PIM%OBS(IND_OBS)%AC )
           END IF
      END IF
!
      IF ( PIM%CONF%DEBUG_LEVEL == 11 ) THEN
           WRITE ( 6, 290 ) IND_OBS, &
     &                     PIM%C_STA(PIM%OBS(IND_OBS)%STA_IND(1)), &
     &                     PIM%C_STA(PIM%OBS(IND_OBS)%STA_IND(2)), &
     &                     PIM%OBS(IND_OBS)%AC_MEAN(1,1,1), &
     &                     PIM%OBS(IND_OBS)%AC_MEAN(1,2,1), &
     &                     SQRT ( PIM%OBS(IND_OBS)%AC_MEAN(1,1,1)*PIM%OBS(IND_OBS)%AC_MEAN(1,2,1) )
 290       FORMAT ( 'PIMA-FRINGE Obs: ', I6, &
     &              ' C_STA: ', A, ' / ',A, &
     &              ' AC_MEAN_1: ', (F10.6,1X), &
     &              ' AC_MEAN_2: ', (F10.6,1X), &
     &              ' AC_MEAN_bas: ', (F10.6,1X) )
      END IF
!
! --- Get cross-correlation UV data and their 1D weights 
! --- that depenend only on time
!
      CALL ERR_PASS ( IUER, IER )
      IF ( PIM%CONF%FRIB_AUTOCORR_CALIB .NE. PIMA__ACCR_NO ) THEN
           CALL PIMA_GET_UV ( PIM, IND_OBS, LPOL, LFRQ, PIM%CONF%BEG_FRQ, &
     &                        PIM%CONF%END_FRQ, PIM%CONF%FRQ_GRP,   &
     &                        POL_MODE, PIM%OBS(IND_OBS)%AC_MEAN, &
     &                        WEI_1D(1,1,0), PIM%OBS(IND_OBS)%UV, &
     &                        PIM%OBS(IND_OBS)%AP_LEN, FRI_STS_GET_UV, IER )
         ELSE
           CALL PIMA_GET_UV ( PIM, IND_OBS, LPOL, LFRQ, PIM%CONF%BEG_FRQ, &
     &                        PIM%CONF%END_FRQ, PIM%CONF%FRQ_GRP,   &
     &                        POL_MODE, %VAL(0), &
     &                        WEI_1D(1,1,0), PIM%OBS(IND_OBS)%UV, &
     &                        PIM%OBS(IND_OBS)%AP_LEN, FRI_STS_GET_UV, IER )
      END IF
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( IND_OBS, STR )
           IF ( ALLOCATED ( WEI_1D_AVR  ) ) DEALLOCATE ( WEI_1D_AVR  )
           IF ( ALLOCATED ( WEI_FRQ_CHN ) ) DEALLOCATE ( WEI_FRQ_CHN )
           IF ( ALLOCATED ( TIM_ARR     ) ) DEALLOCATE ( TIM_ARR     ) 
           FRI_STS = FRI_STS_GET_UV
!
           CALL ERR_LOG ( 9130, IUER, 'PIMA_GET_OBS', 'Failure to get '// &
     &         'UV data for the '//STR(1:I_LEN(STR))//'th observation '// &
     &         'from input FITS-IDI file ' )
           RETURN 
      END IF
!
! --- Apply the following calibration to input UV data:
! --- 1) phase calibration;
! --- 2) correct insane data;
! --- 3) apply bandpass calibration;
! --- 4) apply calibration for frequency-dependency of the
! ---    autocorrelation spectrum;
! --- 5) apply calibration for register saturatiuon (VLBA only)
! --- 6) apply calibration for user time flags
! --- 7) apply calibration for bandpass renormalization when only a part of IF bandweidth is used
! --- 8) apply calibration for beam attenuation
!
      BDW_EFF_BAND(IND_OBS) = 0.0D0
      IFRQ = 0
      DO 450 J5=PIM%CONF%BEG_FRQ,PIM%CONF%END_FRQ
         IFRQ = IFRQ + 1
         IF ( FL_BPASS .AND. PIM%CONF%SPLT_BPASS_NRML_METHOD .EQ. PIMA__WEIGHTED ) THEN
!
! ----------- Get baseline bandpass amplitude renormalization factor
!
              IF ( PIM%BPASS(PIM%OBS(IND_OBS)%STA_IND(1))%L_OBS > 0 .AND. &
     &             PIM%BPASS(PIM%OBS(IND_OBS)%STA_IND(2))%L_OBS > 0       ) THEN
!
                    NRML_CROSS_SPLT = DSQRT ( &
     &                                   PIM%BPASS(PIM%OBS(IND_OBS)%STA_IND(1))%SPLT_NRML_FRQ(J5)* &
     &                                   PIM%BPASS(PIM%OBS(IND_OBS)%STA_IND(2))%SPLT_NRML_FRQ(J5) &
     &                                      )
                  ELSE IF ( PIM%BPASS(PIM%OBS(IND_OBS)%STA_IND(1))%L_OBS == 0 .AND. &
     &                      PIM%BPASS(PIM%OBS(IND_OBS)%STA_IND(2))%L_OBS >  0       ) THEN
                    NRML_CROSS_SPLT = PIM%BPASS(PIM%OBS(IND_OBS)%STA_IND(2))%SPLT_NRML_FRQ(J5) 
                  ELSE IF ( PIM%BPASS(PIM%OBS(IND_OBS)%STA_IND(1))%L_OBS >  0 .AND. &
     &                      PIM%BPASS(PIM%OBS(IND_OBS)%STA_IND(2))%L_OBS == 0       ) THEN
                    NRML_CROSS_SPLT = PIM%BPASS(PIM%OBS(IND_OBS)%STA_IND(1))%SPLT_NRML_FRQ(J5) 
                  ELSE 
                    NRML_CROSS_SPLT = 1.0
               END IF
             ELSE 
               NRML_CROSS_SPLT = 1.0
         END IF
         IF ( PIM%CONF%FRIB_AMPL_EDGE_BEAM_COR == PIMA__BEAM_YES ) THEN
!
! ----------- Compute factors for the primary beam attenuation
!
              NRML_BEAM_ATT(1) = PIMA_BEAM_ATT ( PIM, PIM%FREQ_ARR(1,J5,PIM%CONF%FRQ_GRP),   &
     &                                                INT(PIM%OBS(IND_OBS)%SOU_IND,KIND=4),   &
     &                                                INT(PIM%OBS(IND_OBS)%STA_IND(1),KIND=4) )
              NRML_BEAM_ATT(2) = PIMA_BEAM_ATT ( PIM, PIM%FREQ_ARR(1,J5,PIM%CONF%FRQ_GRP),   &
     &                                                INT(PIM%OBS(IND_OBS)%SOU_IND,KIND=4),   &
     &                                                INT(PIM%OBS(IND_OBS)%STA_IND(2),KIND=4) )
            ELSE 
              NRML_BEAM_ATT(1) = 1.0D0
              NRML_BEAM_ATT(2) = 1.0D0
         END IF
         IF ( PIM%CONF%DEBUG_LEVEL .GE. 6 .AND. &
     &        PIM%CONF%FRIB_AMPL_EDGE_BEAM_COR == PIMA__BEAM_YES .AND. &
     &        IFRQ == 1 ) THEN
!
              WRITE ( 6, 210 ) PIM%C_STA(PIM%OBS(IND_OBS)%STA_IND(1)), &
     &                         NRML_BEAM_ATT(1), & 
     &                         PIM%C_STA(PIM%OBS(IND_OBS)%STA_IND(2)), &
     &                         NRML_BEAM_ATT(2) 
 210          FORMAT ( 'PIMA_GET_OBS BEAM_COR Sta_1: ', A, ' Beam= ', F6.4, &
     &                 ' Sta_2: ', A, ' Beam= ', F6.4  )
          END IF 
 450  CONTINUE 
!
      PH_DEL_APSO = 0.0D0
      PH_RAT_APSO = 0.0D0
      PH_ACC_APSO = 0.0D0
!
! --- Allocate and fill array woth time epochs
!
      ALLOCATE ( TIM_ARR(LTIM), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( 8*LTIM, STR )
           CALL ERR_LOG ( 9131, IUER, 'PIMA_GET_OBS', 'Error in '// &
     &         'an attempt to allocate '//STR(1:I_LEN(STR))// &
     &         ' bytes of dynamic memory for array TIM_ARR' )
           RETURN
      END IF
      TIM_ARR = 0.D0
!     
      DO 460 J6=1,LTIM
         UV_IND  = PIM%OBS(IND_OBS)%UV_IND(J6,FRG_IND)
         TIM_ARR(J6) = PIM%TIM_R8(PIM%UV_IND(UV_IND)%TIM_IND) - &
     &                 PIM%OBS(IND_OBS)%TIM_BEG
         IF ( LPOL == 1 ) THEN
         END IF
 460  CONTINUE 
!     
      IF ( FL_FRI .AND. LTIM .GE. PIMA__APSO_LTIM_LIM ) THEN
           CALL ERR_PASS ( IUER, IER )
           TIM_FRT = PIM%OBS(IND_OBS)%FRT_OFFSET(1)
!
! -------- Phase rotation for a source position shift from pointing direction
! -------- to the apriori  direction. This takes into account non-linear
! -------- (mainly quadratic) effect on fringe phases
!
           CALL PIMA_FIX_APSO_OLD ( PIM, VTD, PIM%NCHN, LFRQ, LTIM, &
     &                          PIM%OBS(IND_OBS)%STA_IND, PIM%OBS(IND_OBS)%SOU_IND, &
     &                          PIM%OBS(IND_OBS)%TIM_BEG, TIM_FRT, TIM_ARR, &
     &                          PHS_APSO_LIM, .TRUE., PIM%OBS(IND_OBS)%UV, &
     &                          PH_DEL_APSO, PH_RAT_APSO, &
     &                          PH_ACC_APSO, PHS_APSO_MAX, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 9132, IUER, 'PIMA_GET_OBS', 'Failre to perform '// &
     &              'correction for quadratic term in fringe phase for '// &
     &              'significant offset in source positions' )
                RETURN
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
            IF ( PIM%OBS(IND_OBS)%RES_GR_RAT(IND_BND) > -0.5D0 ) THEN
!
! -------------- Correct group delay rate
!
                 PIM%OBS(IND_OBS)%RES_GR_RAT(IND_BND) = &
     &                   PIM%OBS(IND_OBS)%RES_GR_RAT(IND_BND) - PH_RAT_APSO
            END IF
          ELSE
            PH_DEL_APSO  = 0.0D0
            PH_RAT_APSO  = 0.0D0
            PH_ACC_APSO  = 0.0D0
            PHS_APSO_MAX = 0.0D0
      END IF
      IF ( FL_FRI .AND. PIM%OBS(IND_OBS)%RES_GR_RAT(IND_BND) < -0.5D0 ) THEN
           DO 570 J7=1,LPOL
              CALL ERR_PASS ( IUER, IER )
              CALL PIMA_APPLY_ACCEL ( PIM, PIM%NCHN, LFRQ, LTIM, &
     &                                PIM%OBS(IND_OBS)%FRT_OFFSET(IND_BND),&
     &                                TIM_ARR, WEI_1D(1,1,0), &
     &                                PIM%OBS(IND_OBS)%RES_PH_ACC(IND_BND), &
     &                                PIM%OBS(IND_OBS)%UV(1,1,1,J7), IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 9133, IUER, 'PIMA_GET_OBS', 'Failre to perform '// &
     &                 'correction for phase acceleration term in fringe phase' )
                   RETURN
              END IF
 570       CONTINUE 
      END IF
!
      IF ( PIM%CONF%DEBUG_LEVEL .EQ. 35 ) THEN
           CALL PIMA_FR1D_TIM_PLOT ( PIM, IND_OBS, '/tmp/boo', LTIM, &
     &               PIM%NCHN, LFRQ, 1.0D0, &
     &               PIM%FREQ_ARR(1,PIM%CONF%BEG_FRQ,PIM%CONF%FRQ_GRP), &
     &               PIM%FRQ(PIM%CONF%BEG_FRQ,PIM%CONF%FRQ_GRP)%FREQ, &
     &               WEI_1D(1,1,0), PIM%OBS(IND_OBS)%UV, &
     &               PIM%OBS(IND_OBS)%FRT_OFFSET(1), &
     &               PIM%OBS(IND_OBS)%AP_LEN, &
     &               PIM%OBS(IND_OBS)%RES_MB_DEL(PIMA__LSQ,1), &
     &               PIM%OBS(IND_OBS)%RES_PH_RAT(PIMA__LSQ,1), &
     &               PIM%OBS(IND_OBS)%RES_GR_RAT(1), &
     &               0.0D0, &
     &               PIM%OBS(IND_OBS)%RES_PHS(PIMA__LSQ,1), &
     &               PIM%OBS(IND_OBS)%AMPL(PIMA__LSQ,1), &
     &               SNR, '??', 3, IER )
      END IF
!
! --- Cross-reference table of polarization. The original order:
! --- 1: 11, 2: 22, 3: 12, 4: 21.
! --- PIMA_GET_UV re-arranges polarizations in this order:
! --- 1: 11, 2: 21, 3: 12, 4: 22
!
      POL_CROSS_IND(1) = 1
      IF ( LPOL == 2 ) THEN
           POL_CROSS_IND(2) = 2       
         ELSE IF ( LPOL == 4 ) THEN
           POL_CROSS_IND(2) = 4  ! pol_cros_ind( orig_ind ) = index of get_uv
           POL_CROSS_IND(3) = 3
           POL_CROSS_IND(4) = 2
      END IF
!
! --- Now compute band-averaged and IF-averaged visibilities
!
      DO 470 J7=1,LTIM
         DRF_BAND  = (0.0, 0.0)
         WPOI_BAND =  0.0
         IF ( FL_AVR ) THEN
!  
!$OMP         PARALLEL DO REDUCTION(+:DRF_BAND, WPOI_BAND, WEI_1D_AVR ) IF ( NTHR > 1 ), &
!$OMP&           DEFAULT  ( NONE ), &
!$OMP&           PRIVATE  ( J8, J9, J10, J11, J12, DRF_IF, WPOI_IF, PHS_MOD ),  &
!$OMP&           SHARED   ( PIM, J7, LFRQ, IND_OBS, WEI_1D, TIM_ARR, LPOL, FL_FRI, FL_AC, &
!$OMP&                      POL_CROSS_IND ), &
!$OMP&           SCHEDULE ( STATIC )
              DO 480 J8=1,LFRQ
                 DRF_IF = (0.0, 0.0)
                 WPOI_IF =  0.0
                 DO 490 J9=1,PIM%NCHN
                    IF ( PIM%BANDPASS_MASK_STYLE .NE. PIMA__NO ) THEN
                         IF ( PIM%BANDPASS_MASK(J9,PIM%CONF%BEG_FRQ+J8-1,PIM%OBS(IND_OBS)%STA_IND(1),PIMA__MASK_FRNG) == 0 .OR. &
                              PIM%BANDPASS_MASK(J9,PIM%CONF%BEG_FRQ+J8-1,PIM%OBS(IND_OBS)%STA_IND(2),PIMA__MASK_FRNG) == 0      ) THEN
                              GOTO 490
                         END IF                
                    END IF
!
! ----------------- We apply phase rotation due to the phase delay rate and group
! ----------------- delay and counter-rotation due to the total phase. 
! ----------------- As a result, we will get the residual phase
!
                    IF ( FL_FRI ) THEN
                         IF ( PIM%OBS(IND_OBS)%RES_GR_RAT(1) < -0.5D0 ) THEN
!
! --------------------------- A case of phase acceleration
!
                              PHS_MOD = - PIM%OBS(IND_OBS)%RES_PHS(PIMA__LSQ,1) &
     &                                  + PIM%OBS(IND_OBS)%RES_PH_RAT(PIMA__LSQ,1)*PI2* &
     &                                    PIM%FRQ(PIM%CONF%BEG_FRQ,PIM%CONF%FRQ_GRP)%FREQ* &
     &                                    (TIM_ARR(J7) - PIM%OBS(IND_OBS)%FRT_OFFSET(1)) &
     &                                  + PIM%OBS(IND_OBS)%RES_MB_DEL(PIMA__LSQ,1)*PI2* &
     &                                    (PIM%FREQ_ARR(J9,PIM%CONF%BEG_FRQ+J8-1,PIM%CONF%FRQ_GRP) - PIM%FRQ(PIM%CONF%BEG_FRQ,PIM%CONF%FRQ_GRP)%FREQ) &
     &                                  + 0.0D0*PI2*PIM%FRQ(PIM%CONF%BEG_FRQ,PIM%CONF%FRQ_GRP)%FREQ* &
     &                                   (TIM_ARR(J7) - PIM%OBS(IND_OBS)%FRT_OFFSET(1))**2/2.0D0
                            ELSE 
!
! --------------------------- A case of group delay rate
!
                              PHS_MOD = - PIM%OBS(IND_OBS)%RES_PHS(PIMA__LSQ,1) &
     &                                  + PIM%OBS(IND_OBS)%RES_PH_RAT(PIMA__LSQ,1)*PI2* &
     &                                    PIM%FRQ(PIM%CONF%BEG_FRQ,PIM%CONF%FRQ_GRP)%FREQ* &
     &                                    (TIM_ARR(J7) - PIM%OBS(IND_OBS)%FRT_OFFSET(1)) & 
     &                                  + PIM%OBS(IND_OBS)%RES_MB_DEL(PIMA__LSQ,1)*PI2* &
     &                                   (PIM%FREQ_ARR(J9,PIM%CONF%BEG_FRQ+J8-1,PIM%CONF%FRQ_GRP) - PIM%FRQ(PIM%CONF%BEG_FRQ,PIM%CONF%FRQ_GRP)%FREQ) &
     &                                  + PIM%OBS(IND_OBS)%RES_GR_RAT(1)*PI2* &
     &                                            (PIM%FREQ_ARR(J9,PIM%CONF%BEG_FRQ+J8-1,PIM%CONF%FRQ_GRP)-PIM%FRQ(PIM%CONF%BEG_FRQ,PIM%CONF%FRQ_GRP)%FREQ)* &
     &                                            (TIM_ARR(J7) - PIM%OBS(IND_OBS)%FRT_OFFSET(1)) 
                         END IF
                       ELSE 
                         PHS_MOD = 0.0
                    END IF
                    DO 4100 J10=1,LPOL
                       IF ( WEI_1D(J7,J10,0) < PIM%CONF%FRIB_WEIGHTS_THRESHOLD ) THEN
                            WEI_1D(J7,J10,0) = 0.0
                            PIM%OBS(IND_OBS)%UV(J9,J8,J7,POL_CROSS_IND(J10)) = CMPLX ( 0.0, 0.0 )
                       END IF
!
                       DRF_IF(J10)  = DRF_IF(J10)  + &
     &                                   CMPLX(WEI_1D(J7,J10,0), 0.0)*PIM%OBS(IND_OBS)%UV(J9,J8,J7,J10)* &
     &                                   CMPLX ( COS(PHS_MOD), SIN(PHS_MOD) )
                       WPOI_IF(J10) = WPOI_IF(J10) + WEI_1D(J7,J10,0)
!
                       DRF_BAND(J10)  = DRF_BAND(J10) + &
     &                                 CMPLX(WEI_1D(J7,J10,0), 0.0)*PIM%OBS(IND_OBS)%UV(J9,J8,J7,J10)* &
     &                                 CMPLX ( COS(PHS_MOD), SIN(PHS_MOD) )
                       WPOI_BAND(J10) = WPOI_BAND(J10) + WEI_1D(J7,J10,0)
 4100               CONTINUE 
!
                    IF ( FL_AC .AND. PIM%CONF%FRIB_AUTOCORR_CALIB .NE. PIMA__ACCR_NO ) THEN
                         DO 4110 J11=1,PIM%NSTK
                            PIM%OBS(IND_OBS)%AC_AVR_TIM(J9,J8,1,J11)= PIM%OBS(IND_OBS)%AC_AVR_TIM(J9,J8,1,J11) + &
     &                                       WEI_1D(J7,J11,1)* &
     &                                       REAL(PIM%OBS(IND_OBS)%AC(J9,J8,J7,1,J11))
                            PIM%OBS(IND_OBS)%AC_AVR_TIM(J9,J8,2,J11) = PIM%OBS(IND_OBS)%AC_AVR_TIM(J9,J8,2,J11) + &
     &                                       WEI_1D(J7,J11,2)* &
     &                                       REAL(PIM%OBS(IND_OBS)%AC(J9,J8,J7,2,J11))
                            WEI_1D_AVR(J9,J8,1,J11) = WEI_1D_AVR(J9,J8,1,J11) + WEI_1D(J7,J11,1)
                            WEI_1D_AVR(J9,J8,2,J11) = WEI_1D_AVR(J9,J8,2,J11) + WEI_1D(J7,J11,2)
 4110                    CONTINUE 
                    END IF
 490             CONTINUE
                 DO 4120 J12=1,LPOL
                    IF ( WPOI_IF(J12) > PIMA__WEI_MIN ) THEN
                         PIM%OBS(IND_OBS)%UV_IF(J7,J8,J12) = DRF_IF(J12)/WPOI_IF(J12)
                       ELSE 
                         PIM%OBS(IND_OBS)%UV_IF(J7,J8,J12) = (0.0, 0.0)
                    END IF
 4120            CONTINUE 
 480          CONTINUE 
!$OMP         END PARALLEL DO
              DO 4130 J13=1,LPOL
                  IF ( WPOI_BAND(J13) > PIMA__WEI_MIN ) THEN
                       PIM%OBS(IND_OBS)%UV_BAND(J7,J13) = DRF_BAND(J13)/WPOI_BAND(J13)
                    ELSE 
                       PIM%OBS(IND_OBS)%UV_BAND(J7,J13) = (0.0, 0.0)
                  END IF
 4130         CONTINUE 
         END IF
         IF ( LPOL == 1 ) THEN
!
! ----------- We have only one polarization
!
              PIM%OBS(IND_OBS)%WEI_1D(J7,1:LPOL)  = WEI_1D(J7,1:LPOL,0)
            ELSE
!
! ----------- We have all polarizations. Compute average weights
!
              PIM%OBS(IND_OBS)%WEI_1D(J7,1:LPOL)  = 0.0D0
              DO 4140 J14=1,PIM%NSTK
                 PIM%OBS(IND_OBS)%WEI_1D(J7,1)  = PIM%OBS(IND_OBS)%WEI_1D(J7,1) + WEI_1D(J7,J14,0)
 4140         CONTINUE 
              PIM%OBS(IND_OBS)%WEI_1D(J7,1) = PIM%OBS(IND_OBS)%WEI_1D(J7,1)/PIM%NSTK
              PIM%OBS(IND_OBS)%WEI_1D(J7,1:LPOL) = PIM%OBS(IND_OBS)%WEI_1D(J7,1)
         END IF
 470  CONTINUE
!
      IF ( FL_AVR .AND. FL_AC ) THEN
!
! -------- Compute autocorrelation averaged over time
!
           DO 4150 J15=1,PIM%NSTK
              DO 4160 J16=1,2
                 DO 4170 J17=1,LFRQ
                    DO 4180 J18=1,PIM%NCHN
                       IF ( WEI_1D_AVR(J18,J17,J16,J15) > PIMA__WEI_MIN ) THEN
                            PIM%OBS(IND_OBS)%AC_AVR_TIM(J18,J17,J16,J15) = PIM%OBS(IND_OBS)%AC_AVR_TIM(J18,J17,J16,J15) / &
     &                                       WEI_1D_AVR(J18,J17,J16,J15) 
                       END IF
 4180               CONTINUE 
 4170            CONTINUE 
 4160         CONTINUE 
 4150      CONTINUE 
      END IF 
!
! --- Deallocate temporary memory
!
      IF ( ALLOCATED ( WEI_1D_AVR  ) ) DEALLOCATE ( WEI_1D_AVR )
      IF ( ALLOCATED ( WEI_FRQ_CHN ) ) DEALLOCATE ( WEI_FRQ_CHN )
      IF ( ALLOCATED ( TIM_ARR     ) ) DEALLOCATE ( TIM_ARR )
!
! --- Good bye!
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  PIMA_GET_OBS  !#!#
