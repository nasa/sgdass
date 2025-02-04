      SUBROUTINE PIMA_GET_AC ( PIM, IND_OBS, N_FRQ, B_FRQ, E_FRQ, U_FRG, &
     &                         ISTA, AC, WEI_1D, AP_LEN, IUER  )
! ************************************************************************
! *                                                                      *
! *   Routine PIMA_GET_AC gets autocorrelation data from input FITS-IDI  *
! *   file(s) for the observation with index IND_OBS, frequency group    *
! *   with index U_FRG for N_FRQ intermediate frequencies with indexes   *
! *   B_FRQ through E_FRQ. In addition to visibility data PIMA_GET_AC    *
! *   returns one-dimensional array of AP weights WEI_1D and AP_LEN --   *
! *   the length of accumulation period seconds. Three modes are         *
! *   supported: MODE=0 -- spectrum of cross-correlation data; ISTA=1 -- *
! *   spectrum of autocorrelation data for the 1st station, ISTA=2 --    *
! *   spectrum of autocorrelation data for the 2nd station.              *
! *                                                                      *
! *   Comments:                                                          *
! *                                                                      *
! *   1) Low-subband data are converted to the form that the frequency   *
! *      is growing along the 1st and 2nd dimension.                     *
! *                                                                      *
! *   2) In a case of I-polarization data are requested, the linear      *
! *      combination of data from RR and LL polarization is returned     *
! *      with feed-horn phase rotation and polarization bandpass         *
! *      applied. I-pulsation data are isle's if no polarization         *
! *      bandpass is available.                                          *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *      PIM ( PIMA__TYPE ) -- Object with information related to        *
! *                            package PIMA.                             *
! *  IND_OBS ( INTEGER*4 ) -- Observation index.                         *
! *    N_FRQ ( INTEGER*4 ) -- The total number of IFs that we be read    *
! *    B_FRQ ( INTEGER*4 ) -- The minimum IF index to be read.           *
! *    E_FRQ ( INTEGER*4 ) -- The maximum IF index to be read.           *
! *    U_FRG ( INTEGER*4 ) -- The frequency group index to be read.      *
! *     ISTA ( INTEGER*4 ) -- Station index.                             *
! *                                                                      *
! * _________________________ Output parameters: _______________________ *
! *                                                                      *
! *      AC ( COMPLEX*8 ) -- 4D complex array of autocorrelations.       *
! *                          1st dimension: the number of spectral       *
! *                                         channels PIM%NCHN            *
! *                          2nd dimension: the number of IFs.           *
! *                          3rd dimension: the number of accumulation   *
! *                                         periods.                     *
! *                          4th dimension: the number of polarizations. *
! *                                         periods.                     *
! *  WEI_1D ( REAL*4    ) -- 2-D array weights averaged over frequency.  *
! *                          1st dimension: the number of accumulation   *
! *                                         periods.                     *
! *                          2nd dimension: polarization index.          *
! *                                         periods.                     *
! *  AP_LEN ( REAL*8    ) -- The length of the accumulation period in    *
! *                          sec.                                        *
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
! *  ### 13-NOV-2018   PIMA_GET_AC  v1.1 (c)  L. Petrov 08-DEC-2018 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'pima.i'
      TYPE     ( PIMA__TYPE  ) :: PIM
      INTEGER*4  IND_OBS, N_FRQ, B_FRQ, E_FRQ, U_FRG, ISTA, IUER
      COMPLEX*8  AC(PIM%NCHN,N_FRQ,PIM%OBS(IND_OBS)%NUM_EPC(PIM%OBS(IND_OBS)%REF_FRG_INDS(PIM%CONF%FRQ_GRP)),2,PIM%NSTK)
      REAL*4     WEI_1D(PIM__MEPC,PIM%NSTK)
      REAL*8     AP_LEN
      CHARACTER  STR*128, POLAR_1ST*8, POLAR_2ND*8
      REAL*4     AP_LEN_R4
      TYPE       UV3__TYPE
          COMPLEX*8 VIS
          REAL*4    WEIGHT
      END TYPE   UV3__TYPE
      TYPE     ( UV3__TYPE ), POINTER :: UV3(:,:,:)
      REAL*4,    ALLOCATABLE :: WEI_ARR(:,:,:)
      COMPLEX*8, ALLOCATABLE :: AC_DATA(:,:,:,:), PCAL_C8(:,:,:,:)
      COMPLEX*8  BPASS_C8, PBP, PC_DIF
      LOGICAL*4  FL_BPASS, FL_PLD, FL_FOUND
      INTEGER*4  UV_IND, FIL_IND, TAB_IND, POI_IND, SCA_IND, &
     &           AUT_IND, IFRQ, LTIM, &
     &           J1, J2, J3, J4, J5, J6, J7, J8, J9, J10, J11, J12, J13, &
     &           J14, J15, J16, J17, J18, J19, J20, J21, J22, J23, J24, &
     &           J25, J26, J27, J28, J29, J30, J31, J32, J33, J34, J35, &
     &           J36, J37, J38, J39, N_FRG, I_FRQ, K_FRG, FIL_FRQ_IND, &
     &           FIL_INDS(PIM__MFRG), IND_FRQ_FIL(PIM__MFRQ), NUM_FRQ, &
     &           FRG_IND, K1, K2, K3, N_AC_BYTES, TIM_IND, &
     &           E_FRG, K_FRQ, IER
      INTEGER*2  STA_IND(2)
      REAL*4     FEED_ANG_DIF, PBP_AMP, PBP_PHS, WEI_POL, WEI_PL2, SCL_FCT, &
     &           PHAS_R4, AMPL_R4
      REAL*8     FREQ_REF
      REAL*8     WALL_TIM_VAL, WALL_TIM_READ_UV, WALL_TIM_READ_AC, WALL_TIM_PROC 
      LOGICAL*1  FL_WEI_NODIV, FL_I_AS_L, FL_DUALPOL, FL_WEI_ONE
      CHARACTER, EXTERNAL :: MJDSEC_TO_DATE*30, GET_CDATE*19, GET_CDATE_MS*23
#ifdef GNU
      INTEGER*4     NTHR, NTHR_SAVED, SCHED_THR
#else
      ADDRESS__TYPE NTHR, NTHR_SAVED, SCHED_THR
#endif
      REAL*4,    EXTERNAL :: PHAS_CMPL_R4
      LOGICAL*4, EXTERNAL :: IS_R4_NAN
      INTEGER*4, EXTERNAL :: ILEN, I_LEN, GET_UNIT, IFIND_PL
      LOGICAL*4, EXTERNAL :: OMP_IN_PARALLEL
#ifdef GNU
      INTEGER*4, EXTERNAL     :: OMP_GET_MAX_THREADS, OMP_GET_NUM_THREADS 
#else
      ADDRESS__TYPE, EXTERNAL :: OMP_GET_MAX_THREADS, OMP_GET_NUM_THREADS 
#endif
      NTHR = PIM%CONF%NUM_THREADS
      CALL OMP_SET_NUM_THREADS ( %VAL(NTHR) )
      IF ( OMP_IN_PARALLEL() ) THEN
           NTHR = 1
      END IF
!
! --- Check kludge variables
!
      FL_WEI_NODIV = .FALSE.
      CALL GETENVAR ( 'PIMAVAR_WEI_NODIV', STR )
      IF ( STR(1:3) == 'yes' .OR. STR(1:3) == 'YES' ) FL_WEI_NODIV = .TRUE.
!
      FL_WEI_ONE = .FALSE.
      CALL GETENVAR ( 'PIMAVAR_AC_WEI_ONE', STR )
      IF ( STR(1:3) == 'yes' .OR. STR(1:3) == 'YES' ) FL_WEI_ONE = .TRUE.
!
      IF ( PIM%CONF%DEBUG_LEVEL == 16 ) THEN
           WRITE ( 6, * ) 'PIMA_GET_AC. FRG_USE, VIRT_NFRG: ', PIM%FRG_USE, PIM%VIRT_NFRG, &
     &                    ' NUM_EPC= ', PIM%OBS(IND_OBS)%NUM_EPC(PIM%OBS(IND_OBS)%REF_FRG_INDS(PIM%CONF%FRQ_GRP))
      END IF
      IF ( PIM%CONF%BANDPASS_USE .NE. PIMA__BPASS_NO  .AND. &
     &     ASSOCIATED ( PIM%BPASS )                         ) THEN
           FL_BPASS = .TRUE.
         ELSE
           FL_BPASS = .FALSE.
      END IF
!
! --- Initialization
!
      FIL_INDS   = -1
      AP_LEN     = 0.0D0
      WEI_1D     = 0.0
      N_AC_BYTES = 0
!
      FRG_IND = PIM%OBS(IND_OBS)%REF_FRG_INDS(PIM%CONF%FRQ_GRP)
      IF ( FRG_IND == 0 ) THEN
           CALL ERR_LOG ( 0, IUER )
           RETURN 
      END IF
!
      LTIM = PIM%OBS(IND_OBS)%NUM_EPC(PIM%OBS(IND_OBS)%REF_FRG_INDS(PIM%CONF%FRQ_GRP))
      WALL_TIM_READ_UV = 0.0D0
      WALL_TIM_READ_AC = 0.0D0
      WALL_TIM_PROC    = 0.0D0
      FL_PLD = .FALSE.
      N_FRG = 1
      DO 410 J1=1,PIM%OBS(IND_OBS)%NUM_EPC(PIM%OBS(IND_OBS)%REF_FRG_INDS(PIM%CONF%FRQ_GRP))
         IF ( PIM%CONF%DEBUG_LEVEL == 8 ) THEN
              CALL WALL_TIMER ( %VAL(0) )
         END IF
!
! ------ Search the UV index of the portion of data from the requested range
! ------ of IFs
!
         UV_IND = PIM%OBS(IND_OBS)%UV_IND(J1,FRG_IND)
! 
         IF ( PIM%UV_IND(UV_IND)%NEXT_UV_IND == 0 ) THEN
!
! ----------- SINGLE or COMBINE case
!
              N_FRG = 1
            ELSE 
!
! ----------- Merged case
!
              IF ( PIM%VIRT_NFRG > N_FRG ) THEN
!
! ---------------- We need to reallocate memory
!
                   IF ( ALLOCATED ( AC_DATA ) ) DEALLOCATE ( AC_DATA )
                   IF ( ALLOCATED ( WEI_ARR ) ) DEALLOCATE ( WEI_ARR )
              END IF
!
! ----------- The number of frequency groups that will be read
!
              N_FRG = PIM%VIRT_NFRG
         END IF
!
! ------ Get the point index ( POI_IND )
! ------     the index of the file where the data reside ( FIL_IND )
! ------     the frequency index ( FRG_IND )
! ------ and the index of th UV table in that file ( TAB_IND )
!
         POI_IND = PIM%UV_IND(UV_IND)%POI_IND
!
! ------ Get the index of autocorrelation
!
         AUT_IND = PIM%UV_IND(UV_IND)%POI_AUT_IND(ISTA)
         FIL_IND = PIM%UV_IND(UV_IND)%FIL_IND
         TAB_IND = PIM%UV_IND(UV_IND)%TAB_IND
         FIL_INDS(1) = FIL_IND
!
! ------ Allocate memory depending on the number of polarizations
!
         IF ( .NOT. ALLOCATED ( WEI_ARR ) ) THEN
              ALLOCATE ( WEI_ARR(PIM%NSTK,PIM%FILE(FIL_IND)%NFRQ,N_FRG), STAT=IER )
              IF ( IER .NE. 0 ) THEN
                   CALL CLRCH ( STR )
                   CALL IINCH ( 4*PIM%NSTK*PIM%FILE(FIL_IND)%NFRQ, STR )
                   CALL ERR_LOG ( 7521, IUER, 'PIMA_GET_AC', 'Error in '// &
     &                 'an attempt to allocate '//STR(1:I_LEN(STR))// &
     &                 ' bytes of dynamic memory for temporary array '// &
     &                 'with weights' )
                   RETURN
              END IF
              WEI_ARR = 0.0
         END IF
!
! ------ Allocate dynamic memory where the data will be temporarily held.
! ------ Amount of memory depends on the number of polarizations.
! ------ If only one polarization is requested, even in that case we download 
! ------ all the data
!
         IF ( .NOT. ALLOCATED ( AC_DATA ) ) THEN
              ALLOCATE ( AC_DATA(PIM%NSTK,PIM%NCHN,PIM%FILE(FIL_IND)%NFRQ,N_FRG), STAT=IER )
              IF ( IER .NE. 0 ) THEN
                   CALL CLRCH ( STR )
                   CALL IINCH ( 4*PIM%NSTK*PIM%NCHN*PIM%FILE(FIL_IND)%NFRQ, STR )
                   CALL ERR_LOG ( 7522, IUER, 'PIMA_GET_AC', &
     &                  'Error in an attempt to allocate '// &
     &                   STR(1:I_LEN(STR))//' bytes of dynamic '// &
     &                  'memory for temporary array with UV data' )
                   IF ( ALLOCATED ( AC_DATA ) ) DEALLOCATE ( AC_DATA )
                   IF ( ALLOCATED ( WEI_ARR ) ) DEALLOCATE ( WEI_ARR )
                   RETURN
              END IF
              AC_DATA = (0.0, 0.0)
         END IF
!
         IF ( AP_LEN == 0.0D0 ) THEN
              AP_LEN = PIM%UV_IND(UV_IND)%AP_LEN
         END IF
!         
         STA_IND = PIM%UV_IND(AUT_IND)%STA_IND
         FIL_IND = PIM%UV_IND(AUT_IND)%FIL_IND
         TAB_IND = PIM%UV_IND(AUT_IND)%TAB_IND
         POI_IND = PIM%UV_IND(AUT_IND)%POI_IND
         FIL_INDS(1) = FIL_IND
         IF ( AUT_IND == 0 ) THEN
              CALL CLRCH ( STR )
              CALL INCH  ( J1,      STR(1:9)   )
              CALL INCH  ( IND_OBS, STR(11:19) )
              CALL INCH  ( UV_IND,  STR(21:29) )
              IF ( STA_IND(ISTA) > 0 ) STR(31:38) = PIM%STA(STA_IND(ISTA))%IVS_NAME
              CALL ERR_LOG ( 7523, IUER, 'PIMA_GET_AC', 'Error in '// &
     &             'an attempt to get AUT-data for station '// &
     &             STR(31:38)//' for the '//STR(1:I_LEN(STR(1:9)))// &
     &             ' -th point of the observation '//STR(11:19)// &
     &             ' which has UV index '//STR(21:29)// &
     &             ' -- no point index found' )
              IF ( ALLOCATED ( AC_DATA ) ) DEALLOCATE ( AC_DATA )
              IF ( ALLOCATED ( WEI_ARR ) ) DEALLOCATE ( WEI_ARR )
              RETURN
         END IF
!         
         IF ( POI_IND == 0 ) THEN
              CALL CLRCH ( STR )
              CALL INCH  ( J1,      STR(1:9)   )
              CALL INCH  ( IND_OBS, STR(11:19) )
              CALL INCH  ( AUT_IND,  STR(21:29) )
              CALL ERR_LOG ( 7524, IUER, 'PIMA_GET_AC', 'Error in '// &
     &            'an attempt to get AUT-data for station from file '// &
     &             PIM%FILE(FIL_IND)%NAME(1:I_LEN(PIM%FILE(FIL_IND)%NAME))// &
     &            ' for the '//STR(1:I_LEN(STR(1:9)))//'th point of '// &
     &            ' the observation '//STR(11:19)//' whcih has UV '// &
     &            'index '//STR(21:29)//' -- no point index has '// &
     &            'been found' )
              IF ( ALLOCATED ( AC_DATA ) ) DEALLOCATE ( AC_DATA )
              IF ( ALLOCATED ( WEI_ARR ) ) DEALLOCATE ( WEI_ARR )
              RETURN
         END IF
         IF ( PIM%GENERATOR == 'EVN'  .OR. &
     &        PIM%GENERATOR == 'SFXC' .OR. &
     &        PIM%GENERATOR == 'ASC'       ) THEN
              IF ( PIM%UV_IND(AUT_IND)%NEXT_UV_IND > 0 ) THEN
                   CALL ERR_LOG ( 7525, IUER, 'PIMA_GET_AC', 'Trap of internal '// &
     &                 'control: at the moment a case of 3D-weights and merged '// &
     &                 'frequency groups is not supported' )
                   RETURN 
              END IF
!         
! ----------- Case of 3-data, i.e. (image, real, weight) triplets
!         
              ALLOCATE ( UV3(PIM%NSTK,PIM%NCHN,PIM%NFRQ) )
              CALL ERR_PASS ( IUER, IER )
              CALL FFITS_GETR4 ( PIM%FILE(FIL_IND)%FITS_DESC,  &
     &                           PIM%FILE(FIL_IND)%IND_UV_TAB(TAB_IND), &
     &                           POI_IND, &
     &                           PIM%FILE(FIL_IND)%KEY(PIM%FILE(FIL_IND)%IND_FLUX_KEY(TAB_IND), &
     &                           PIM%FILE(FIL_IND)%IND_UV_TAB(TAB_IND)), &
     &                           3*PIM%NSTK*PIM%NCHN*PIM%FILE(FIL_IND)%NFRQ, &
     &                           UV3, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( J1,      STR(1:9)   )
                   CALL INCH  ( IND_OBS, STR(11:19) )
                   CALL INCH  ( UV_IND,  STR(21:29) )
                   CALL INCH  ( POI_IND, STR(31:39) )
                   CALL INCH  ( TAB_IND, STR(41:49) )
                   CALL INCH  ( FIL_IND, STR(51:59) )
                   CALL INCH  ( AUT_IND, STR(61:69) )
                   CALL ERR_LOG ( 7526, IUER, 'PIMA_GET_AC', 'Error '// &
     &                 'in an attempt to get AUT-data from file '// &
     &                  PIM%FILE(FIL_IND)%NAME(1:I_LEN(PIM%FILE(FIL_IND)%NAME))// &
     &                 ' for the '//STR(1:9)//' th epoch of the observation '// &
     &                 STR(11:19)//' which has UV index '//STR(21:29)// &
     &                 ', point index '//STR(31:39)//', tab index '// &
     &                 STR(41:49)//', file index '//STR(51:59)// &
     &                 ' auto_corr index '//STR(61:69)//' Key: '// &
     &                 PIM%FILE(FIL_IND)%KEY(PIM%FILE(FIL_IND)%IND_FLUX_KEY(TAB_IND), &
     &                              PIM%FILE(FIL_IND)%IND_UV_TAB(TAB_IND)) )
                   IF ( ALLOCATED ( AC_DATA ) ) DEALLOCATE ( AC_DATA )
                   IF ( ALLOCATED ( WEI_ARR ) ) DEALLOCATE ( WEI_ARR )
                   RETURN
              END IF
              DO 420 J2=1,PIM%NFRQ
                 DO 430 J3=1,PIM%NCHN
                    DO 440 J4=1,PIM%NSTK
                        AC_DATA(J4,J3,J2,1) = UV3(J4,J3,J2)%VIS
                        WEI_ARR(J4,J2,1)    = UV3(J4,J3,J2)%WEIGHT
 440                CONTINUE
 430            CONTINUE
 420         CONTINUE
             DEALLOCATE ( UV3 )
          ELSE
!         
! ----------- Case of 2-data, i.e. (image, real)
!         
              DO 450 J5=1,N_FRG
                 IF ( PIM%FRG_USE == PIMA__MERGE ) THEN
!         
! ------------------- Merged data
!         
                      IF ( J5 == 1 ) THEN
!         
! ------------------------ Update file index
!         
                           FIL_INDS(J5) = FIL_IND
                           AUT_IND = PIM%UV_IND(UV_IND)%POI_AUT_IND(ISTA)
                         ELSE IF ( J5 > 1 ) THEN
!         
! ------------------------ Get chained UV index and then file index
!         
                           AUT_IND  = PIM%UV_IND(AUT_IND)%NEXT_UV_IND
                           FIL_INDS(J5) = PIM%UV_IND(AUT_IND)%FIL_IND
                      END IF
                 END IF
!         
! -------------- Get other indexes
!         
                 FIL_IND = PIM%UV_IND(AUT_IND)%FIL_IND
                 TAB_IND = PIM%UV_IND(AUT_IND)%TAB_IND
                 POI_IND = PIM%UV_IND(AUT_IND)%POI_IND
                 IF ( PIM%NPOL == 1 ) THEN
!         
!-------------------- Read the autocorrelation spectra data
!         
                      CALL ERR_PASS ( IUER, IER )
                      CALL FFITS_GETR4 ( PIM%FILE(FIL_IND)%FITS_DESC,  &
     &                       PIM%FILE(FIL_IND)%IND_UV_TAB(TAB_IND), &
     &                       POI_IND, &
     &                       PIM%FILE(FIL_IND)%KEY(PIM%FILE(FIL_IND)%IND_FLUX_KEY(TAB_IND), &
     &                       PIM%FILE(FIL_IND)%IND_UV_TAB(TAB_IND)), &
     &                       2*PIM%NCHN*PIM%FILE(FIL_IND)%NFRQ, &
     &                       AC_DATA(1,1,1,J5), IER )
                      IF ( IER .NE. 0 ) THEN
                           CALL CLRCH ( STR )
                           CALL INCH  ( J1,      STR(1:9)   )
                           CALL INCH  ( IND_OBS, STR(11:19) )
                           CALL INCH  ( UV_IND,  STR(21:29) )
                           CALL INCH  ( POI_IND, STR(31:39) )
                           CALL INCH  ( TAB_IND, STR(41:49) )
                           CALL INCH  ( FIL_IND, STR(51:59) )
                           CALL INCH  ( AUT_IND, STR(61:69) )
                           CALL ERR_LOG ( 7527, IUER, 'PIMA_GET_AC', 'Error '// &
     &                         'in an attempt to get AUT-data from file '// &
     &                          PIM%FILE(FIL_IND)%NAME(1:I_LEN(PIM%FILE(FIL_IND)%NAME))// &
     &                         ' for the '//STR(1:9)//' th epoch of the observation '// &
     &                         STR(11:19)//' which has UV index '//STR(21:29)// &
     &                         ', point index '//STR(31:39)//', tab index '// &
     &                         STR(41:49)//', file index '//STR(51:59)// &
     &                         ' auto_corr index '//STR(61:69)//' Key: '// &
     &                         PIM%FILE(FIL_IND)%KEY(PIM%FILE(FIL_IND)%IND_FLUX_KEY(TAB_IND), &
     &                                      PIM%FILE(FIL_IND)%IND_UV_TAB(TAB_IND)) )
                           IF ( ALLOCATED ( AC_DATA ) ) DEALLOCATE ( AC_DATA )
                           IF ( ALLOCATED ( WEI_ARR ) ) DEALLOCATE ( WEI_ARR )
                           RETURN
                     END IF
!         
! ------------------- Read the weights
!         
                      CALL ERR_PASS ( IUER, IER )
                      CALL FFITS_GETR4 ( PIM%FILE(FIL_IND)%FITS_DESC,  &
     &                       PIM%FILE(FIL_IND)%IND_UV_TAB(TAB_IND), &
     &                       POI_IND, &
     &                       PIM%FILE(FIL_IND)%KEY(PIM%FILE(FIL_IND)%IND_WEIGHT_KEY(TAB_IND), &
     &                       PIM%FILE(FIL_IND)%IND_UV_TAB(TAB_IND)), &
     &                       PIM%FILE(FIL_IND)%NFRQ, WEI_ARR(1,1,J5), IER )
                      IF ( IER .NE. 0 ) THEN
                           CALL ERR_LOG ( 7528, IUER, 'PIMA_GET_AC', 'Error in reading '// &
     &                         'weights from file '//PIM%FILE(FIL_IND)%NAME )
                           RETURN 
                      END IF
                    ELSE IF ( PIM%NPOL == 2 ) THEN
                      IF ( PIM%CONF%DEBUG_LEVEL == 15 .OR. &
     &                     PIM%CONF%DEBUG_LEVEL == 16      ) THEN
!         
                           WRITE ( 6, * ) 'GET_AC_2: J1= ', J1, &
     &                                 ' FIL_IND= ', FIL_IND, &
     &                                 ' TAB_IND= ', TAB_IND, &
     &                                 ' PIM%FILE(FIL_IND)%IND_UV_TAB(TAB_IND) = ', &
     &                                       PIM%FILE(FIL_IND)%IND_UV_TAB(TAB_IND), &
     &                                 ' POI_IND= ', POI_IND
                           WRITE ( 6, * ) 'Shape(AC_DATA) = ', SHAPE(AC_DATA), &
     &                                 ' IND_TAB= ',  PIM%FILE(FIL_IND)%KEY(PIM%FILE(FIL_IND)%IND_FLUX_KEY(TAB_IND), &
     &                                   PIM%FILE(FIL_IND)%IND_UV_TAB(TAB_IND)), &
     &                                 ' NP= ', 2*PIM%NSTK*PIM%NCHN*PIM%FILE(FIL_IND)%NFRQ
                      END IF
!         
!-------------------- Read the autocorrelation spectra data
!         
                      CALL ERR_PASS ( IUER, IER )
                      CALL FFITS_GETR4 ( PIM%FILE(FIL_IND)%FITS_DESC,  &
     &                       PIM%FILE(FIL_IND)%IND_UV_TAB(TAB_IND), &
     &                       POI_IND, &
     &                       PIM%FILE(FIL_IND)%KEY(PIM%FILE(FIL_IND)%IND_FLUX_KEY(TAB_IND), &
     &                       PIM%FILE(FIL_IND)%IND_UV_TAB(TAB_IND)), &
     &                       2*PIM%NSTK*PIM%NCHN*PIM%FILE(FIL_IND)%NFRQ, &
     &                       AC_DATA(1,1,1,J5), IER )
                      IF ( IER .NE. 0 ) THEN
                           CALL CLRCH ( STR )
                           CALL INCH  ( J1,      STR(1:9)   )
                           CALL INCH  ( IND_OBS, STR(11:19) )
                           CALL INCH  ( UV_IND,  STR(21:29) )
                           CALL INCH  ( POI_IND, STR(31:39) )
                           CALL INCH  ( TAB_IND, STR(41:49) )
                           CALL INCH  ( FIL_IND, STR(51:59) )
                           CALL INCH  ( AUT_IND, STR(61:69) )
                           WRITE ( 6, * ) 'NUM_PT= ', PIM%OBS(IND_OBS)%NUM_EPC(PIM%OBS(IND_OBS)%REF_FRG_INDS(PIM%CONF%FRQ_GRP))
                           CALL ERR_LOG ( 7529, IUER, 'PIMA_GET_AC', 'Error '// &
     &                         'in an attempt to get AUT-data from file '// &
     &                          PIM%FILE(FIL_IND)%NAME(1:I_LEN(PIM%FILE(FIL_IND)%NAME))// &
     &                         ' for the '//STR(1:9)//' th epoch of the observation '// &
     &                         STR(11:19)//' which has UV index '//STR(21:29)// &
     &                         ', point index '//STR(31:39)//', tab index '// &
     &                         STR(41:49)//', file index '//STR(51:59)// &
     &                         ' auto_corr index '//STR(61:69)//' Key: '// &
     &                         PIM%FILE(FIL_IND)%KEY(PIM%FILE(FIL_IND)%IND_FLUX_KEY(TAB_IND), &
     &                                      PIM%FILE(FIL_IND)%IND_UV_TAB(TAB_IND)) )
                           IF ( ALLOCATED ( AC_DATA ) ) DEALLOCATE ( AC_DATA )
                           IF ( ALLOCATED ( WEI_ARR ) ) DEALLOCATE ( WEI_ARR )
                           RETURN
                     END IF
!         
! ------------------- Read the weights
!         
                      CALL ERR_PASS ( IUER, IER )
                      CALL FFITS_GETR4 ( PIM%FILE(FIL_IND)%FITS_DESC,  &
     &                       PIM%FILE(FIL_IND)%IND_UV_TAB(TAB_IND), &
     &                       POI_IND, &
     &                       PIM%FILE(FIL_IND)%KEY(PIM%FILE(FIL_IND)%IND_WEIGHT_KEY(TAB_IND), &
     &                       PIM%FILE(FIL_IND)%IND_UV_TAB(TAB_IND)), &
     &                       PIM%NSTK*PIM%FILE(FIL_IND)%NFRQ, WEI_ARR(1,1,J5), IER )
                      IF ( IER .NE. 0 ) THEN
                           CALL CLRCH ( STR )
                           CALL INCH  ( J1,      STR(1:9)   )
                           CALL INCH  ( IND_OBS, STR(11:19) )
                           CALL INCH  ( UV_IND,  STR(21:29) )
                           CALL INCH  ( POI_IND, STR(31:39) )
                           CALL INCH  ( TAB_IND, STR(41:49) )
                           CALL INCH  ( FIL_IND, STR(51:59) )
                           CALL INCH  ( AUT_IND, STR(61:69) )
                           CALL ERR_LOG ( 7530, IUER, 'PIMA_GET_AC', 'Error '// &
     &                         'in an attempt to get AUT-data from file '// &
     &                          PIM%FILE(FIL_IND)%NAME(1:I_LEN(PIM%FILE(FIL_IND)%NAME))// &
     &                         ' for the '//STR(1:9)//' th epoch of the observation '// &
     &                         STR(11:19)//' which has UV index '//STR(21:29)// &
     &                         ', point index '//STR(31:39)//', tab index '// &
     &                         STR(41:49)//', file index '//STR(51:59)// &
     &                         ' auto_corr index '//STR(61:69)//' Key: '// &
     &                         PIM%FILE(FIL_IND)%KEY(PIM%FILE(FIL_IND)%IND_FLUX_KEY(TAB_IND), &
     &                                      PIM%FILE(FIL_IND)%IND_UV_TAB(TAB_IND)) )
                           IF ( ALLOCATED ( AC_DATA ) ) DEALLOCATE ( AC_DATA )
                           IF ( ALLOCATED ( WEI_ARR ) ) DEALLOCATE ( WEI_ARR )
                           RETURN
                     END IF
                 END IF
                 IF ( PIM%CORR_VERS == 'DiFX-2.1' ) THEN
!         
! ------------------- Correcting wrong autocorrelation amplitudes for
! ------------------- LBA stations due to the bug introduved in DiFX-2.1
! ------------------- version. The bug was fixed in DiFX-2.1.1 and newer
! ------------------- versions
!         
                      IF ( ISTA == 1 ) THEN
                          IF ( PIM%C_STA(PIM%OBS(IND_OBS)%STA_IND(1))(1:4) == 'ATCA'     .OR. &
     &                         PIM%C_STA(PIM%OBS(IND_OBS)%STA_IND(1))      == 'CEDUNA  ' .OR. &
     &                         PIM%C_STA(PIM%OBS(IND_OBS)%STA_IND(1))      == 'MOPRA   ' .OR. &
     &                         ( PIM%C_STA(PIM%OBS(IND_OBS)%STA_IND(1)) == 'PARKES  ' .AND.   &
     &                           PIM%OBS_CODE == 'V441A' )                                    ) THEN
                               AC_DATA = 16.0*AC_DATA
                          END IF
                      END IF
                      IF ( ISTA == 2 ) THEN
                          IF ( PIM%C_STA(PIM%OBS(IND_OBS)%STA_IND(2))(1:4) == 'ATCA'     .OR. &
     &                         PIM%C_STA(PIM%OBS(IND_OBS)%STA_IND(2))      == 'CEDUNA  ' .OR. &
     &                         PIM%C_STA(PIM%OBS(IND_OBS)%STA_IND(2))      == 'MOPRA   ' .OR. &
     &                         ( PIM%C_STA(PIM%OBS(IND_OBS)%STA_IND(2)) == 'PARKES  ' .AND.   &
     &                           PIM%OBS_CODE == 'V441A' )                                    ) THEN
                               AC_DATA = 16.0*AC_DATA
                          END IF
                      END IF
                 END IF
 450         CONTINUE 
          END IF
          IF ( PIM%CONF%DEBUG_LEVEL == 8 ) THEN
               CALL WALL_TIMER ( STR )
               READ ( UNIT=STR(12:27), FMT='(F16.10)' ) WALL_TIM_VAL
               WALL_TIM_READ_AC = WALL_TIM_READ_AC + WALL_TIM_VAL
               CALL WALL_TIMER ( %VAL(0) )
               N_AC_BYTES = N_AC_BYTES + 4*PIM%NSTK*PIM%FILE(FIL_IND)%NFRQ
               N_AC_BYTES = N_AC_BYTES + 4*2*PIM%NSTK*PIM%NCHN*PIM%FILE(FIL_IND)%NFRQ
          END IF
!
          TIM_IND = PIM%UV_IND(UV_IND)%TIM_IND
          STA_IND = PIM%UV_IND(UV_IND)%STA_IND
          STR = MJDSEC_TO_DATE ( PIM%MJD_0, PIM%TAI_0 + PIM%TIM_R8(TIM_IND), IER )
!
          WEI_1D(J1,1:PIM%NSTK) = 0.0D0
          DO 460 J6=1,PIM%FILE(FIL_IND)%NFRQ
             DO 470 J7=1,PIM%NCHN
                DO 480 J8=1,PIM%NSTK
                   IF ( J7 == 1 ) THEN
                        WEI_1D(J1,J8) = WEI_1D(J1,J8) + WEI_ARR(J8,J6,1)/PIM%FILE(FIL_IND)%NFRQ
                   END IF
                   IF ( PIM%CONF%DEBUG_LEVEL == 29 ) THEN
                        WRITE ( 6, 210 ) IND_OBS, J1, J8, J7, J6, PIM%FILE(FIL_IND)%REV_FRQ(J6,FRG_IND), &
     &                                   AC_DATA(J8,J7,J6,1), WEI_ARR(J8,J6,1), &
     &                                   STR(1:29), PIM%TAI_0 + PIM%TIM_R8(TIM_IND) + PIM%UTC_MTAI, &
     &                                   TRIM(PIM%STA(STA_IND(ISTA))%ORIG_NAME)
 210                    FORMAT ( 'Auto  Ind_obs/Ap: ', I5, 1X, I3, &
     &                           ' Ind_Stk/Chn/Frq_orig/Frq: ', I1, 1X, I4, 1X, I2, 1X, I2, &
     &                           ' Ac: ', 1PE14.7, ' , ', 1PE14.7, 1X, ' Wei: ', 0PF10.7, &
     &                           ' Tai: ', A29, ' UTC: ', 0PF10.3, ' Sit: ', A )
                   END IF
 480            CONTINUE 
 470         CONTINUE 
 460      CONTINUE 
          IF ( FL_WEI_ONE ) THEN
               WEI_1D(J1,1:PIM%NSTK) = 1.0D0
          END IF
!
! ------ Finally, put extracted data into the output array
! ------ Put correct polarizaiton
! ------ Conjugate and cyclically rotate the index the UV data if the low
! ------ side-band was requested in order it to look as if was the from the
! ------ upper band.
!
         IF ( PIM%FRG_USE == PIMA__MERGE .AND. PIM%VIRT_NFRG > 1 ) THEN
!
! ----------- Restore the original UV_IND index of the beginning of the UV chain
!
              UV_IND  = PIM%OBS(IND_OBS)%UV_IND(J1,FRG_IND)
         END IF
!
!$OMP    PARALLEL DO IF ( NTHR > 1 .AND. N_FRQ .GE. 4 ), &
!$OMP&                    FIRSTPRIVATE ( FIL_IND ), &
!$OMP&                    DEFAULT ( NONE), &
!$OMP&                    SHARED ( ISTA, IND_OBS, J1, UV_IND, N_FRG, U_FRG, N_FRQ, B_FRQ, E_FRQ, &
!$OMP&                             FIL_INDS, FL_WEI_NODIV, WEI_1D, WEI_ARR, AC, AC_DATA, PIM ), &
!$OMP&                    PRIVATE ( J9, J10, J11, I_FRQ, E_FRG, K_FRQ, K_FRG, FIL_FRQ_IND, &
!$OMP&                              WEI_POL), &
!$OMP&                    SCHEDULE ( STATIC, N_FRQ )
         DO 490 J9=B_FRQ,E_FRQ
            I_FRQ = J9 - B_FRQ + 1
            IF ( PIM%FRG_USE == PIMA__SINGLE .OR. &
     &           ( PIM%FRG_USE == PIMA__MERGE .AND. N_FRG == 1 ) ) THEN
!
! -------------- Single frequency group case or merged data with only one frequency group
!
                 IF ( PIM%FILE(FIL_IND)%REV_FRQ(J9,U_FRG) .LE. 0  ) THEN
                      GOTO 490
                 END IF
                 E_FRG = 1      ! Extended group index
                 K_FRQ = J9    ! frequency index
                 K_FRG = U_FRG  ! frequency group ubdex 
                 FIL_FRQ_IND = PIM%FILE(FIL_IND)%REV_FRQ(J9,K_FRG) ! Frequency index in the table that in the file
               ELSE IF ( PIM%FRG_USE == PIMA__COMBINE ) THEN
!
! -------------- Combined frequency group table
!
                 IF ( PIM%UV_IND(UV_IND)%FRG_IND .NE. PIM%REV_FRG(J9) ) THEN
                      AC(1:PIM%NCHN,N_FRQ,J9,ISTA,1:PIM%NSTK) = ( 0.0, 0.0 )
                      GOTO 490
                 END IF
                 FIL_IND = PIM%UV_IND(UV_IND)%FIL_IND
                 FIL_FRQ_IND = PIM%FILE(FIL_IND)%REV_FRQ(PIM%REV_FRQ(J9),PIM%REV_FRG(J9))
                 E_FRG = 1
                 K_FRQ = PIM%REV_FRQ(J9)
                 K_FRG = U_FRG            ! index of virtial frequency group
               ELSE IF ( PIM%FRG_USE == PIMA__MERGE .AND. N_FRG > 1 ) THEN
!
! -------------- A case of merged frequency group with chained data
!
                 E_FRG = PIM%REV_FRG(J9) ! Global frequency group index before merging
                 K_FRQ = PIM%REV_FRQ(J9) ! Frequency index in the global table before merging
                 K_FRG = 1                ! index of the virtial frequency group
                 FIL_IND = FIL_INDS(E_FRG)
                 FIL_FRQ_IND = PIM%FILE(FIL_IND)%REV_FRQ(K_FRQ,E_FRG)
            END IF
!
! --------- Determine WEI_POL and WEI_PL2 -- weights for the first and second polarizations 
!
            DO 4100 J10=1,PIM%NSTK
               IF ( .NOT. FL_WEI_NODIV ) THEN
                    WEI_POL = WEI_ARR(J10,FIL_FRQ_IND,E_FRG)
                 ELSE 
                    WEI_POL = 1.0
               END IF
!
               IF ( WEI_POL < PIM%CONF%FRIB_WEIGHTS_THRESHOLD ) WEI_POL = 1.0
!
               DO 4110 J11=1,PIM%NCHN
                  IF ( IS_R4_NAN ( REAL ( AC_DATA(J10,J11,FIL_FRQ_IND,E_FRG) ) ) .OR. &
     &                 IS_R4_NAN ( IMAG ( AC_DATA(J10,J11,FIL_FRQ_IND,E_FRG) ) )      ) THEN
                       AC_DATA(J10,J11,FIL_FRQ_IND,E_FRG) = (0.0, 0.0)
                  END IF
                  IF ( ABS(REAL(AC_DATA(J10,J11,FIL_FRQ_IND,E_FRG))) > 1.E10 .OR. &
     &                 ABS(IMAG(AC_DATA(J10,J11,FIL_FRQ_IND,E_FRG))) > 1.E10      ) THEN
                       AC_DATA(J10,J11,FIL_FRQ_IND,E_FRG) = (0.0, 0.0)
                  END IF
                  IF ( PIM%FRQ(K_FRQ,K_FRG)%SIDE_BAND == 1 ) THEN
!
! ---------------------- Upper side-band
!
                       AC(J11,I_FRQ,J1,ISTA,J10) = AC_DATA(J10,J11,FIL_FRQ_IND,E_FRG)/WEI_POL
                    ELSE
!
! ---------------------- Lower side-band
!
                       AC(J11,I_FRQ,J1,ISTA,J10) = CONJG( AC_DATA(J10,PIM%NCHN-J11+1,FIL_FRQ_IND,E_FRG)/WEI_POL )
                  END IF
!
                  IF ( PIM%CONF%DEBUG_LEVEL == 16 ) THEN
                       WRITE ( 6, 230 ) ISTA, &
     &                                  PIM%C_STA(PIM%OBS(IND_OBS)%STA_IND(1)), &
     &                                  PIM%C_STA(PIM%OBS(IND_OBS)%STA_IND(2)), &
     &                                  J11, J9, J1, J10, &
     &                                  AC(J11,I_FRQ,J1,ISTA,J10)*WEI_POL, &
     &                                  WEI_1D(J1,1), E_FRG
 230                   FORMAT ( 'ISTA: ',I1, ' Sta: ',A, ' / ', A, ' Ichn: ', I4, &
     &                               ' Ifrq: ', I2, ' Itim: ', I4, ' Istk: ', I1, ' Ac: ', &
     &                                1PE15.7, 1X, 1PE15.7, &
     &                               ' Wei: ', 1PE15.7, ' EF= ', I1 )
                   END IF
 4110          CONTINUE 
 4100        CONTINUE 
 490     CONTINUE ! IF
!$OMP    END PARALLEL DO
         IF ( PIM%CONF%DEBUG_LEVEL == 8 ) THEN
              CALL WALL_TIMER ( STR )
              READ ( UNIT=STR(12:27), FMT='(F16.10)' ) WALL_TIM_VAL
              WALL_TIM_PROC = WALL_TIM_PROC + WALL_TIM_VAL
              CALL WALL_TIMER ( %VAL(0) )
         END IF
 410  CONTINUE ! Time epoch
      IF ( FL_PLD ) THEN
           DEALLOCATE ( PCAL_C8 )
      END IF
!
      IF ( PIM%OBS(IND_OBS)%REF_FRG_INDS(PIM%CONF%FRQ_GRP) > 1 ) THEN
           PIM%OBS(IND_OBS)%AP_LEN = AP_LEN
      END IF
      IF ( ALLOCATED ( AC_DATA ) ) DEALLOCATE ( AC_DATA )
      IF ( ALLOCATED ( WEI_ARR ) ) DEALLOCATE ( WEI_ARR )
      IF ( PIM%CONF%DEBUG_LEVEL == 8 ) THEN
                WRITE ( 6, 250 ) WALL_TIM_READ_AC, WALL_TIM_PROC, N_AC_BYTES/1048576.0
 250            FORMAT ( 'PIMA_GET_AC AC: Wall time Read: ', F10.6, ' Proc: ', F10.6, &
     &                   1X, F8.3, ' Mb' )
           CALL WALL_TIMER ( %VAL(0) )
      END IF
      DO 4120 J12=1,PIM%NSTK
         DO 4130 J13=1,PIM%OBS(IND_OBS)%NUM_EPC(PIM%OBS(IND_OBS)%REF_FRG_INDS(PIM%CONF%FRQ_GRP))
!$OMP       PARALLEL DO IF ( NTHR > 1 .AND. PIM%NCHN*N_FRQ .GE. 256  ), &
!$OMP&                       PRIVATE ( J14, J15 ), &
!$OMP&                       SCHEDULE ( STATIC, NTHR )
            DO 4140 J14=1,N_FRQ
               DO 4150 J15=1,PIM%NCHN
                  IF ( IS_R4_NAN ( REAL ( AC(J15,J14,J13,ISTA,J12) ) ) .OR. &
     &                 IS_R4_NAN ( IMAG ( AC(J15,J14,J13,ISTA,J12) ) )      ) THEN
                       AC(J15,J14,J13,ISTA,J12) = (0.0, 0.0)
                  END IF
!                   
                  IF ( J12 < 3 ) THEN
                       IF ( ABS(AC(J15,J14,J13,ISTA,J12)) < PIM%CONF%FRIB_AUTOCORR_THRESHOLD .OR. &
     &                      ABS(AC(J15,J14,J13,ISTA,J12)) < PIM%CONF%FRIB_AUTOCORR_THRESHOLD      ) THEN
                            AC(J15,J14,J13,ISTA,J12) = (0.0, 0.0)
                       END IF
                  END IF
                  IF ( ABS(AC(J15,J14,J13,ISTA,J12)) > PIMA__ACC_MAX  .OR. &
     &                 ABS(AC(J15,J14,J13,ISTA,J12)) > PIMA__ACC_MAX       ) THEN
                       AC(J15,J14,J13,ISTA,J12) = (0.0, 0.0)
                  END IF
 4150          CONTINUE 
 4140       CONTINUE 
!$OMP       END PARALLEL DO
 4130    CONTINUE 
 4120 CONTINUE 
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  PIMA_GET_AC  !#!#
