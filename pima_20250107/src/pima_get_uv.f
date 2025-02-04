      SUBROUTINE PIMA_GET_UV ( PIM, IND_OBS, L_POL, N_FRQ, B_FRQ, E_FRQ, &
     &                         U_FRG, POL_MODE, AC_MEAN, WEI_1D, UV, &
     &                         AP_LEN, FRI_STS, IUER  )
! ************************************************************************
! *                                                                      *
! *   Routine PIMA_GET_UV gets visibility data from input FITS-IDI       *
! *   file(s) for the observation with index IND_OBS, frequency group    *
! *   with index U_FRG for N_FRQ intermediate frequencies with indexes   *
! *   B_FRQ through E_FRQ. In addition to visibility data PIMA_GET_UV    *
! *   returns one-dimensional array of AP weights WEI_1D and AP_LEN --   *
! *   the length of accumulation period seconds. Three modes are         *
! *   supported: MODE=0 -- spectrum of cross-correlation data; MODE=1 -- *
! *   spectrum of autocorrelation data for the 1st station, MODE=2 --    *
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
! *     PIM ( PIMA__TYPE ) -- Object with information related to package *
! *                           PIMA.                                      *
! * IND_OBS ( INTEGER*4 ) -- Observation index.                          *
! *   L_POL ( INTEGER*4 ) -- The total number of output polarizations.   *
! *   N_FRQ ( INTEGER*4 ) -- The total number of IFs that will be read   *
! *   B_FRQ ( INTEGER*4 ) -- The minimum IF index to be read.            *
! *   E_FRQ ( INTEGER*4 ) -- The maximum IF index to be read.            *
! *   U_FRG ( INTEGER*4 ) -- The frequency group index to be read.       *
! * POL_MODE ( INTEGER*4 ) -- Polarization mode.                         *
! *                                                                      *
! * _________________________ Output parameters: _______________________ *
! *                                                                      *
! *      UV ( COMPLEX*8 ) -- 3D complex array of visibilities.           *
! *                          1st dimension: the number of spectral       *
! *                                         channels PIM%NCHN            *
! *                          2nd dimension: the number of IFs.           *
! *                          3rd dimension: the number of accumulation   *
! *                                         periods.                     *
! *  WEI_1D ( REAL*4    ) -- 2-D array weights averaged over frequency.  *
! *                          1st dimension: the number of accumulation   *
! *                              periods.                                *
! *                          2nd dimension: the number of Stokes         *
! *                              parameters.                             *
! *  AP_LEN ( REAL*8    ) -- The length of the accumulation period in    *
! *                          sec.                                        *
! * FRI_STS ( INTEGER*4 ) -- Status code. 0 if the data were fetche      *
! *                          successfully.                               *
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
! *  ### 12-JAN-2006  PIMA_GET_UV  v10.4 (c)  L. Petrov  26-SEP-2020 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'pima.i'
      TYPE     ( PIMA__TYPE  ) :: PIM
      INTEGER*4  IND_OBS, L_POL, N_FRQ, B_FRQ, E_FRQ, U_FRG, POL_MODE, FRI_STS, IUER
      COMPLEX*8  UV(PIM%NCHN,N_FRQ,PIM%OBS(IND_OBS)%NUM_EPC(PIM%OBS(IND_OBS)%REF_FRG_INDS(PIM%CONF%FRQ_GRP)),L_POL)
      REAL*4     WEI_1D(PIM__MEPC,PIM__MPLR), AC_MEAN(N_FRQ,2,PIM%NSTK)
      REAL*8     AP_LEN
      CHARACTER  STR*128, STR1*128, POL_CONF*7
      REAL*4     AP_LEN_R4
      TYPE       UV3__TYPE
          COMPLEX*8 VIS
          REAL*4    WEIGHT
      END TYPE   UV3__TYPE
      TYPE     ( UV3__TYPE ), POINTER :: UV3(:,:,:)
      REAL*4,    ALLOCATABLE :: WEI_ARR(:,:,:)
      COMPLEX*8, ALLOCATABLE :: UV_DATA(:,:,:,:), PCAL_C8(:,:,:,:)
      COMPLEX*8  BPASS_C8, PBP_C8, PC_DIF, SWAP_UV
      COMPLEX*8  I_CMPLX
      PARAMETER  ( I_CMPLX = (0.0, 1.0) )
      LOGICAL*4  FL_BPASS, FL_PLD, FL_FOUND
      INTEGER*4  UV_IND, FIL_IND, TAB_IND, POI_IND, SCA_IND, &
     &           AUT_IND, IND_POLC, IND_POLA(2,2), IFRQ, LTIM, &
     &           J1, J2, J3, J4, J5, J6, J7, J8, J9, J10, J11, J12, J13, &
     &           J14, J15, J16, J17, J18, J19, J20, J21, N_FRG, I_FRQ, K_FRG, &
     &           FIL_FRQ_IND, FIL_INDS(PIM__MFRG), IND_FRQ_FIL(PIM__MFRQ), &
     &           NUM_FRQ(PIM__MPLR), IND_STA(2), FRG_IND, K1, K2, K3, N_AC_BYTES, &
     &           N_UV_BYTES, E_FRG, K_FRQ, TIM_IND, IER
      INTEGER*2  STA_IND(2)
      REAL*4     PBP_AMP, PBP_PHS, SCL_FCT, P1, P2, AC_CAL(2,2), &
     &           PHAS_R4, PHAS_R4_1ST, PHAS_R4_2ND, AMPL_R4, VIS_MAX, &
     &           PCAL_PHS(2,2), BPAS_PHS(2,2), SWAP_WEI, &
     &           PIMAVAR_LINPOL_ROTANG, PIMAVAR_LINPOL_ROTANG1, &
     &           PIMAVAR_LINPOL_ROTANG2
      COMPLEX*8  RR_POL, LL_POL
      LOGICAL*1  FL_PIMAVAR_LINPOL_NOROT, FL_PIMAVAR_LINPOL_KLUDGE, FL_PIMAVAR_LINPOL_ROTANG, &
     &           FL_PIMAVAR_LINPOL_ROTANG1, FL_PIMAVAR_LINPOL_ROTANG2, FL_PIMAVAR_LINPOL_ROTSWAP, &
     &           FL_PIMAVAR_BPS_PHS_SIGN_FLIP, FL_PIMAVAR_GETUV_KLUDGE
      REAL*8     FREQ_REF
      REAL*8     WALL_TIM_VAL, WALL_TIM_READ_UV, WALL_TIM_READ_AC, WALL_TIM_PROC 
      COMPLEX*8  UNC_OBS(2,2), CAL_OBS(2,2), CAL_OBS_COPY(2,2), &
     &           JM_A1(2,2), JM_A2(2,2), JM_B1(2,2), JM_B2(2,2), &
     &           JM_C1(2,2), JM_C2(2,2), JM_H1(2,2), JM_H2(2,2), &
     &           JM_P1(2,2), JM_P2(2,2), JM_PD(2,2), TMP_C8(2,2)
      LOGICAL*1  FL_WEI_NODIV, FL_I_AS_L, FL_PIMAVAR_NOFEED_ROT
      CHARACTER, EXTERNAL :: MJDSEC_TO_DATE*30, GET_CDATE*19, GET_CDATE_MS*23, &
     &           PIMA_GET_POL_CONF*7
#ifdef GNU
      INTEGER*4     NTHR, NTHR_SAVED, SCHED_THR
#else
      ADDRESS__TYPE NTHR, NTHR_SAVED, SCHED_THR
#endif
      REAL*4,    EXTERNAL :: PHAS_CMPL_R4
      LOGICAL*4, EXTERNAL :: IS_R4_NAN, IS_R4_INF
      INTEGER*4, EXTERNAL :: ILEN, I_LEN, GET_UNIT, IFIND_PL
      LOGICAL*4, EXTERNAL :: OMP_IN_PARALLEL
#ifdef GNU
      INTEGER*4, EXTERNAL     :: OMP_GET_MAX_THREADS, OMP_GET_NUM_THREADS 
#else
      ADDRESS__TYPE, EXTERNAL :: OMP_GET_MAX_THREADS, OMP_GET_NUM_THREADS 
#endif
!
      FRI_STS = 0
      CALL GETENVAR ( 'PIMAVAR_LINPOL_NOROT', STR )
      IF ( STR == 'YES' .OR. STR == 'yes' ) THEN
           FL_PIMAVAR_LINPOL_NOROT = .TRUE.
           WRITE ( 6, * ) 'PIMA_GET_UV: FL_PIMAVAR_LINPOL_NOROT = .TRUE.'
         ELSE
           FL_PIMAVAR_LINPOL_NOROT = .FALSE.
      END IF
      CALL GETENVAR ( 'PIMAVAR_NOFEED_ROT', STR )
      IF ( STR == 'YES' .OR. STR == 'yes' ) THEN
           FL_PIMAVAR_NOFEED_ROT = .TRUE.
           WRITE ( 6, * ) 'PIMA_GET_UV: FL_PIMAVAR_NOFEED_ROT = .TRUE.'
         ELSE
           FL_PIMAVAR_NOFEED_ROT = .FALSE.
      END IF
!
      CALL GETENVAR ( 'PIMAVAR_NO_ACMEAN', STR )
      IF ( STR == 'YES' .OR. STR == 'yes' ) THEN
           PIM%OBS(IND_OBS)%AC_MEAN = 1.0
           WRITE ( 6, * ) 'PIMA_GET_UV: AC_MEAN is set to 1.0'
      END IF
!
      CALL GETENVAR ( 'PIMAVAR_LINPOL_ROTANG1', STR )
      IF ( ILEN(STR) > 0 ) THEN
           READ ( UNIT=STR, FMT='(F10.5)' ) PIMAVAR_LINPOL_ROTANG1
           FL_PIMAVAR_LINPOL_ROTANG1 = .TRUE.
         ELSE
           FL_PIMAVAR_LINPOL_ROTANG1 = .FALSE.
      END IF
!
      CALL GETENVAR ( 'PIMAVAR_LINPOL_ROTANG2', STR )
      IF ( ILEN(STR) > 0 ) THEN
           READ ( UNIT=STR, FMT='(F10.5)' ) PIMAVAR_LINPOL_ROTANG2
           FL_PIMAVAR_LINPOL_ROTANG2 = .TRUE.
         ELSE
           FL_PIMAVAR_LINPOL_ROTANG2 = .FALSE.
      END IF
!
      CALL GETENVAR ( 'PIMAVAR_LINPOL_ROTSWAP', STR )
      IF ( ILEN(STR) > 0 ) THEN
           CALL TRAN ( 11, STR, STR )
           IF ( STR(1:3) == 'YES' ) THEN
                FL_PIMAVAR_LINPOL_ROTSWAP = .TRUE.
             ELSE
                FL_PIMAVAR_LINPOL_ROTSWAP = .FALSE.
           END IF
         ELSE
           FL_PIMAVAR_LINPOL_ROTSWAP = .FALSE.
      END IF
!
      CALL GETENVAR ( 'PIMAVAR_GETUV_KLUDGE', STR )
      IF ( ILEN(STR) > 0 ) THEN
           CALL TRAN ( 11, STR, STR )
           IF ( STR(1:3) == 'YES' ) THEN
                FL_PIMAVAR_GETUV_KLUDGE = .TRUE.
                IF ( PIM%CONF%DEBUG_LEVEL .GE. 1 ) WRITE ( 6, * ) 'PIMAVAR_GETUV_KLUDGE is set up' 
             ELSE
                FL_PIMAVAR_GETUV_KLUDGE = .FALSE.
           END IF
         ELSE
           FL_PIMAVAR_GETUV_KLUDGE = .FALSE.
      END IF
!
      CALL GETENVAR ( 'PIMAVAR_BPS_PHS_SIGN_FLIP', STR )
      IF ( ILEN(STR) > 0 ) THEN
           CALL TRAN ( 11, STR, STR )
           IF ( STR(1:3) == 'YES' ) THEN
                FL_PIMAVAR_BPS_PHS_SIGN_FLIP = .TRUE.
                IF ( PIM%CONF%DEBUG_LEVEL .GE. 1 ) WRITE ( 6, * ) 'FL_PIMAVAR_BPS_PHS_SIGN_FLIP is set up' 
             ELSE
                FL_PIMAVAR_BPS_PHS_SIGN_FLIP = .FALSE.
           END IF
         ELSE
           FL_PIMAVAR_BPS_PHS_SIGN_FLIP = .FALSE.
      END IF
      CALL GETENVAR ( 'PIMAVAR_VIS_MAX', STR )
      IF ( ILEN(STR) > 0 ) THEN
           IF ( INDEX ( STR, '.' ) < 1 ) STR = TRIM(STR)//'.0'
           READ ( UNIT=STR, FMT='(F10.5)' ) VIS_MAX
         ELSE
           VIS_MAX = PIMA__AMP_MAX
      END IF 
!
      NTHR = PIM%CONF%NUM_THREADS
      CALL OMP_SET_NUM_THREADS ( %VAL(NTHR) )
      IF ( OMP_IN_PARALLEL() ) THEN
           NTHR = 1
      END IF
!
      IER = -1
      POL_CONF = PIMA_GET_POL_CONF ( PIM, IND_OBS, IER ) 
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 7521, IUER, 'PIMA_GET_UV', 'Trap of internal conotrol ' )
           CALL EXIT ( 1 )
           RETURN
      END IF
!
! --- Check kludge variables
!
      FL_WEI_NODIV = .FALSE.
      CALL GETENVAR ( 'PIMAVAR_WEI_NODIV', STR )
      IF ( STR(1:3) == 'yes' .OR. STR(1:3) == 'YES' ) FL_WEI_NODIV = .TRUE.
!
      FL_I_AS_L = .FALSE.
      CALL GETENVAR ( 'PIMAVAR_I_AS_L', STR )
      IF ( STR(1:3) == 'yes' .OR. STR(1:3) == 'YES' ) FL_I_AS_L = .TRUE.
!
      IF ( PIM%CONF%DEBUG_LEVEL == 16 ) THEN
           WRITE ( 6, * ) 'PIMA_GET_UV. FRG_USE, VIRT_NFRG: ', PIM%FRG_USE, PIM%VIRT_NFRG, &
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
      FIL_INDS = -1
      UV  = (0.0, 0.0)
      AP_LEN = 0.0D0
      UV = CMPLX ( 0.0, 0.0 )
      WEI_1D = 0.0
      N_AC_BYTES = 0
      N_UV_BYTES = 0
!
      FRG_IND = PIM%OBS(IND_OBS)%REF_FRG_INDS(PIM%CONF%FRQ_GRP)
      IF ( FRG_IND == 0 ) THEN
           CALL ERR_LOG ( 0, IUER )
           RETURN 
      END IF
      IND_POLC = -2147483647
      IND_POLA = -2147483647
      IF ( POL_MODE == PIMA__RRCC ) THEN ! Single polarization, 1st polarization slot
           IND_POLC = 1
           IND_POLA(1,1) = 1
           IND_POLA(2,1) = 1
      END IF
      IF ( POL_MODE == PIMA__LLCC .AND. PIM%NSTK == 1 ) THEN ! Single polarization, 1st polarization slot
           IND_POLC = 1
           IND_POLA(1,1) = 1
           IND_POLA(2,1) = 1
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
                   IF ( ALLOCATED ( UV_DATA ) ) DEALLOCATE ( UV_DATA )
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
                   CALL ERR_LOG ( 7522, IUER, 'PIMA_GET_UV', 'Error in '// &
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
         IF ( .NOT. ALLOCATED ( UV_DATA ) ) THEN
              ALLOCATE ( UV_DATA(PIM%NSTK,PIM%NCHN,PIM%FILE(FIL_IND)%NFRQ,N_FRG), STAT=IER )
              IF ( IER .NE. 0 ) THEN
                   CALL CLRCH ( STR )
                   CALL IINCH ( 4*PIM%NSTK*PIM%NCHN*PIM%FILE(FIL_IND)%NFRQ, STR )
                   CALL ERR_LOG ( 7527, IUER, 'PIMA_GET_UV', &
     &                  'Error in an attempt to allocate '// &
     &                   STR(1:I_LEN(STR))//' bytes of dynamic '// &
     &                  'memory for temporary array with UV data' )
                   IF ( ALLOCATED ( UV_DATA ) ) DEALLOCATE ( UV_DATA )
                   IF ( ALLOCATED ( WEI_ARR ) ) DEALLOCATE ( WEI_ARR )
                   RETURN
              END IF
              UV_DATA = (0.0, 0.0)
            ELSE IF ( .NOT. ALLOCATED ( UV_DATA ) ) THEN
              CALL CLRCH ( STR )
              CALL INCH  ( PIM%NPOL, STR )
              CALL ERR_LOG ( 7528, IUER, 'PIMA_GET_UV', 'Trap of internal control: '// &
     &            ' PIM%NPOL= '//STR(1:I_LEN(STR))//' while 1 or 2 were expected' )
              IF ( ALLOCATED ( UV_DATA ) ) DEALLOCATE ( UV_DATA )
              IF ( ALLOCATED ( WEI_ARR ) ) DEALLOCATE ( WEI_ARR )
              RETURN
         END IF
!
         IF ( AP_LEN == 0.0D0 ) THEN
              AP_LEN = PIM%UV_IND(UV_IND)%AP_LEN
         END IF
!
         IER = 0
         IF ( PIM%GENERATOR == 'AIPS' ) THEN
              WEI_1D(J1,1:PIM%NSTK) = 1.0
            ELSE IF ( PIM%GENERATOR == 'EVN'  .OR. &
     &                PIM%GENERATOR == 'SFXC' .OR. &
     &                PIM%GENERATOR == 'ASC'       ) THEN
              CONTINUE
            ELSE
!
! ----------- Get 2D weights  (DiFX, KJCC, MITAKA)
!
              DO 420 J2=1,N_FRG ! Cycle over frequency groups that to be read
                 IF ( PIM%FRG_USE == PIMA__MERGE ) THEN
                      IF ( J2 == 1 ) THEN
!
! ------------------------ Just update file index
!
                           FIL_INDS(J2) = FIL_IND
                         ELSE IF ( J2 > 1 ) THEN
!
! ------------------------ Get chained UV index and then file index
!
                           UV_IND  = PIM%UV_IND(UV_IND)%NEXT_UV_IND
                           FIL_INDS(J2) = PIM%UV_IND(UV_IND)%FIL_IND
                      END IF
                 END IF
!
                 FIL_IND = PIM%UV_IND(UV_IND)%FIL_IND
                 TAB_IND = PIM%UV_IND(UV_IND)%TAB_IND
                 POI_IND = PIM%UV_IND(UV_IND)%POI_IND
!
! -------------- Read the chunk of data
!
                 CALL ERR_PASS ( IUER, IER )
                 CALL FFITS_GETR4 ( PIM%FILE(FIL_IND)%FITS_DESC,  &
     &                  PIM%FILE(FIL_IND)%IND_UV_TAB(TAB_IND), &
     &                  POI_IND, &
     &                  PIM%FILE(FIL_IND)%KEY(PIM%FILE(FIL_IND)%IND_WEIGHT_KEY(TAB_IND), &
     &                  PIM%FILE(FIL_IND)%IND_UV_TAB(TAB_IND)), &
     &                  PIM%NSTK*PIM%FILE(FIL_IND)%NFRQ, WEI_ARR(1,1,J2), IER )
                 IF ( IER .NE. 0 ) THEN
                      CALL ERR_LOG ( 7528, IUER, 'PIMA_GET_UV', 'Failure in reading '// &
     &                    'weights from the input FITS-IDI file '//PIM%FILE(FIL_IND)%NAME )
                      IF ( ALLOCATED ( UV_DATA ) ) DEALLOCATE ( UV_DATA )
                      IF ( ALLOCATED ( WEI_ARR ) ) DEALLOCATE ( WEI_ARR )
                      RETURN 
                 END IF
 420          CONTINUE 
!
! ----------- Average weights and transform 1D weights to 2D weights
!
              WEI_1D(J1,1:PIM%NSTK) = 0.0
              NUM_FRQ = 0
              DO 430 J3=B_FRQ,E_FRQ
                 IF ( PIM%FRG_USE == PIMA__SINGLE ) THEN
!
! ------------------- Normal (single group) frequency table
!
                      FIL_FRQ_IND = PIM%FILE(FIL_IND)%REV_FRQ(J3,U_FRG)
                      E_FRG = 1
                   ELSE IF ( PIM%FRG_USE == PIMA__COMBINE ) THEN
!
! ------------------- Combined frequency group table
!
                      FIL_FRQ_IND = PIM%FILE(FIL_IND)%REV_FRQ(PIM%REV_FRQ(J3),PIM%REV_FRG(J3))
                      E_FRG = 1
                   ELSE IF ( PIM%FRG_USE == PIMA__MERGE ) THEN
!
! ------------------- Merged frequency group table
!
                      IF ( N_FRG == 1 ) THEN
!
! ------------------------ First frequency group
!
                           FIL_FRQ_IND = PIM%FILE(FIL_IND)%REV_FRQ(J3,U_FRG)
                           E_FRG = 1
                        ELSE 
!
! ------------------------ Chained frequency group
!
                           E_FRG = PIM%REV_FRG(J3)
                           K_FRQ = PIM%REV_FRQ(J3)
                           FIL_IND = FIL_INDS(E_FRG)
                           FIL_FRQ_IND = PIM%FILE(FIL_IND)%REV_FRQ(K_FRQ,E_FRG)
                      END IF
                 END IF
!
                 DO 440 J4=1,PIM%NSTK
                    IF ( IND_POLC .NE. -2147483647 ) THEN
                         IF ( J4 .NE. IND_POLC ) GOTO 440
                    END IF
                    IF ( FIL_FRQ_IND > 0  ) THEN
                         IF ( WEI_ARR(J4,FIL_FRQ_IND,E_FRG) < 1.D-8 ) THEN
                              WEI_ARR(J4,FIL_FRQ_IND,E_FRG) = 0.0
                           ELSE IF ( WEI_ARR(J4,FIL_FRQ_IND,E_FRG) > 1000.0D0 ) THEN
                              WEI_ARR(J4,FIL_FRQ_IND,E_FRG) = 0.0
                           ELSE
                              WEI_1D(J1,J4) = WEI_1D(J1,J4) + WEI_ARR(J4,FIL_FRQ_IND,E_FRG)
                              NUM_FRQ(J4) = NUM_FRQ(J4) + 1
                         END IF
                    END IF
 440             CONTINUE
 430          CONTINUE
              DO 450 J5=1,PIM%NSTK
                  IF ( NUM_FRQ(J5) > 0 ) THEN
                       WEI_1D(J1,J5) = WEI_1D(J1,J5)/NUM_FRQ(J5)
                     ELSE
                       WEI_1D(J1,J5) = 0.0
                  END IF
 450          CONTINUE 
              IF ( IND_POLC .NE. -2147483647 ) THEN
                   WEI_1D(J1,1) = WEI_1D(J1,IND_POLC)
              END IF
         END IF
         IF ( IER .NE. 0 ) THEN
              CALL CLRCH ( STR )
              CALL INCH  ( J1,      STR(1:9)   )
              CALL INCH  ( IND_OBS, STR(11:19) )
              CALL INCH  ( UV_IND,  STR(21:29) )
              CALL INCH  ( POI_IND, STR(31:39) )
              CALL INCH  ( FIL_IND, STR(41:49) )
              CALL INCH  ( AUT_IND, STR(61:69) )
              CALL ERR_LOG ( 7529, IUER, 'PIMA_GET_UV', 'Error in an '// &
     &            'attempt to get weights of UV-data from file '// &
     &             PIM%FILE(FIL_IND)%NAME(1:I_LEN(PIM%FILE(FIL_IND)%NAME))// &
     &            ' for the '//STR(1:9)//' th point of the observation '// &
     &             STR(11:19)//' which has UV index '//STR(21:29)// &
     &            ', point index '//STR(31:39)//', file index '// &
     &            STR(41:49)//' auto_corr index '//STR(61:69) )
              IF ( ALLOCATED ( UV_DATA ) ) DEALLOCATE ( UV_DATA )
              IF ( ALLOCATED ( WEI_ARR ) ) DEALLOCATE ( WEI_ARR )
              RETURN
         END IF
         UV_IND = PIM%OBS(IND_OBS)%UV_IND(J1,FRG_IND)
!
         IF ( PIM%GENERATOR == 'EVN'  .OR. &
     &        PIM%GENERATOR == 'SFXC' .OR. &
     &        PIM%GENERATOR == 'ASC'       ) THEN
              IF ( PIM%UV_IND(UV_IND)%NEXT_UV_IND > 0 ) THEN
                   CALL ERR_LOG ( 7530, IUER, 'PIMA_GET_UV', 'Trap of internal '// &
     &                 'control: at the moment a case of 3D-weights and merged '// &
     &                 'frequency groups is not supported' )
                   RETURN 
              END IF
              ALLOCATE ( UV3(PIM%NSTK,PIM%NCHN,PIM%NFRQ) )
              IF ( PIM%GENERATOR == 'SFXC' ) THEN
!
! ---------------- Apply special scaling factors that are specific for the SFXC
! ---------------- correlator
!
                   IF ( PIM%NLEV(PIM%OBS(IND_OBS)%STA_IND(1)) == 4 .AND. &
     &                  PIM%NLEV(PIM%OBS(IND_OBS)%STA_IND(2)) == 4      ) THEN
                        SCL_FCT = 1.E0/1.1329552E0
                      ELSE IF ( PIM%NLEV(PIM%OBS(IND_OBS)%STA_IND(1)) == 4 .AND. &
     &                           PIM%NLEV(PIM%OBS(IND_OBS)%STA_IND(2)) == 2       ) THEN
                        SCL_FCT = 1.E0/1.33271E0
                      ELSE IF ( PIM%NLEV(PIM%OBS(IND_OBS)%STA_IND(1)) == 2 .AND. &
     &                           PIM%NLEV(PIM%OBS(IND_OBS)%STA_IND(2)) == 4       ) THEN
                        SCL_FCT = 1.E0/1.33271E0
                      ELSE
                        SCL_FCT = 1.E0/1.56768E0
                    END IF
                  ELSE 
                    SCL_FCT = 1.0E0
              ENDIF !
!
! ----------- Case of 3-data ( Real, Image, Weight ), for examnple SFXC
!
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
                   CALL ERR_LOG ( 7522, IUER, 'PIMA_GET_UV', 'Error in '// &
     &                 'an attempt to read UV data from the FITS-IDI file '// &
     &                  PIM%FILE(FIL_IND)%NAME )
                   RETURN
              END IF
              WEI_1D(J1,1:PIM%NSTK) = 0.0D0
              DO 460 J6=1,PIM%FILE(FIL_IND)%NFRQ
                 DO 470 J7=1,PIM%NCHN
                    DO 480 J8=1,PIM%NSTK
                       UV_DATA(J8,J7,J6,1) = UV3(J8,J7,J6)%VIS*SCL_FCT
!
                       WEI_1D(J1,J8) = WEI_1D(J1,J8) + UV3(J8,J7,J6)%WEIGHT
                       WEI_ARR(J8,J6,1) = WEI_ARR(J8,J6,1) + UV3(J8,J7,J6)%WEIGHT
 480             CONTINUE 
 470             CONTINUE
                 DO 490 J9=1,PIM%NSTK
                    WEI_ARR(J9,J6,1) = WEI_ARR(J9,J6,1)/PIM%NCHN
 490             CONTINUE 
 460          CONTINUE
              DO 4100 J10=1,PIM%NSTK
                 WEI_1D(J1,J10) = WEI_1D(J1,J10)/(PIM%NFRQ*PIM%NCHN)
                 IF ( WEI_1D(J1,J10) < 0.0 ) WEI_1D(J1,J10) = 0.0
 4100         CONTINUE 
              DEALLOCATE ( UV3 )
            ELSE
!
! ----------- Case of 2-data  (Real, Image ), for example DiFX
!
              UV_IND = PIM%OBS(IND_OBS)%UV_IND(J1,FRG_IND)
              DO 4110 J11=1,N_FRG
                 IF ( PIM%FRG_USE == PIMA__MERGE ) THEN
!
! ------------------- Merged data
!
                      IF ( J11 > 1 ) THEN
!
! ------------------------ Get chained UV index and then file index
!
                           UV_IND  = PIM%UV_IND(UV_IND)%NEXT_UV_IND
                      END IF
                 END IF
!
! -------------- Get other indices
!
                 FIL_IND = PIM%UV_IND(UV_IND)%FIL_IND
                 TAB_IND = PIM%UV_IND(UV_IND)%TAB_IND
                 POI_IND = PIM%UV_IND(UV_IND)%POI_IND
                 IF ( PIM%CONF%DEBUG_LEVEL == 16 ) THEN
                      WRITE ( 6, * ) 'PIMA_GET_UV_1: J1= ', J1, &
     &                               ' FIL_IND= ', FIL_IND, &
     &                               ' TAB_IND= ', TAB_IND, &
     &                               ' POI_IND= ', POI_IND
                 END IF
!
! -------------- Read the chunk of cross-correlated data of dual-pol data
!
                 CALL ERR_PASS ( IUER, IER )
                 CALL FFITS_GETR4 ( PIM%FILE(FIL_IND)%FITS_DESC,  &
     &                      PIM%FILE(FIL_IND)%IND_UV_TAB(TAB_IND), &
     &                      POI_IND, &
     &                      PIM%FILE(FIL_IND)%KEY(PIM%FILE(FIL_IND)%IND_FLUX_KEY(TAB_IND), &
     &                      PIM%FILE(FIL_IND)%IND_UV_TAB(TAB_IND)), &
     &                      2*PIM%NSTK*PIM%NCHN*PIM%FILE(FIL_IND)%NFRQ, &
     &                      UV_DATA(1,1,1,J11), IER )
                 IF ( IER .NE. 0 ) THEN
                      CALL CLRCH ( STR )
                      CALL ERR_LOG ( 7522, IUER, 'PIMA_GET_UV', 'Error in '// &
     &                    'an attempt to read UV data from the FITS-IDI file '// &
     &                     PIM%FILE(FIL_IND)%NAME )
                      RETURN
                 END IF
!
                 IF ( IER .NE. 0 ) THEN
                      CALL CLRCH ( STR )
                      CALL INCH  ( J1,      STR(1:9)   )
                      CALL INCH  ( IND_OBS, STR(11:19) )
                      CALL INCH  ( UV_IND,  STR(21:29) )
                      CALL INCH  ( POI_IND, STR(31:39) )
                      CALL INCH  ( FIL_IND, STR(41:49) )
                      CALL ERR_LOG ( 7531, IUER, 'PIMA_GET_UV', 'Error in an '// &
     &                    'attempt to get UV-data from file '// &
     &                     PIM%FILE(FIL_IND)%NAME(1:I_LEN(PIM%FILE(FIL_IND)%NAME))// &
     &                    ' for the '//STR(1:9)//' th point of the observation '// &
     &                     STR(11:19)//' with has UV index '//STR(21:29)// &
     &                    ', point index '//STR(31:39)//', file index '// &
     &                     STR(41:49) )
                      IF ( ALLOCATED ( UV_DATA ) ) DEALLOCATE ( UV_DATA )
                      IF ( ALLOCATED ( WEI_ARR ) ) DEALLOCATE ( WEI_ARR )
                      RETURN
                 END IF
!
                 TIM_IND = PIM%UV_IND(UV_IND)%TIM_IND
                 STA_IND = PIM%UV_IND(UV_IND)%STA_IND
                 STR = MJDSEC_TO_DATE ( PIM%MJD_0, PIM%TAI_0 + PIM%TIM_R8(TIM_IND), IER )
                 DO 4120 J12=1,PIM%FILE(FIL_IND)%NFRQ
                    DO 4130 J13=1,PIM%NCHN
                        DO 4140 J14=1,PIM%NSTK
                           IF ( PIM%OBS(IND_OBS)%STA_IND(1) == PIM%UV_IND(UV_IND)%STA_IND(1) .AND. &
     &                          PIM%OBS(IND_OBS)%STA_IND(2) == PIM%UV_IND(UV_IND)%STA_IND(2)       ) THEN
!
                                CONTINUE 
                              ELSE IF ( PIM%OBS(IND_OBS)%STA_IND(1) == PIM%UV_IND(UV_IND)%STA_IND(2) .AND. &
     &                                  PIM%OBS(IND_OBS)%STA_IND(2) == PIM%UV_IND(UV_IND)%STA_IND(1)       ) THEN
!
! ----------------------------- This is a pathological case!
!
                                UV_DATA(J14,J13,J12,J11) = CONJG ( UV_DATA(J14,J13,J12,J11) )
                           END IF
 4140                   CONTINUE 
!
                        IF ( PIM%CONF%DEBUG_LEVEL == 30 ) THEN
                             DO 5140 J14=1,PIM%NSTK
                                WRITE ( 6, 220 ) IND_OBS, J1, J14, J13, J12, &
     &                                           PIM%FILE(FIL_IND)%REV_FRQ(J12,FRG_IND), FIL_IND, &
     &                                           UV_DATA(J14,J13,J12,J11), WEI_ARR(J14,J12,1), &
     &                                           STR(1:29), PIM%TAI_0 + PIM%TIM_R8(TIM_IND) + PIM%UTC_MTAI, &
     &                                           TRIM(PIM%STA(STA_IND(1))%ORIG_NAME), &
     &                                           TRIM(PIM%STA(STA_IND(2))%ORIG_NAME)
 220                            FORMAT ( 'Cross Ind_obs/Ap: ', I5, 1X, I4, &
     &                                   ' Ind_Stk/Chn/Frq_orig/Frq: ', I1, 1X, I4, 1X, I2, 1X, I2, &
     &                                   ' Fil_ind: ', I2, &
     &                                   ' Cc: ', 1PE14.7, ' , ', 1PE14.7, 1X, ' Wei: ', 0PF10.7, &
     &                                   ' Tai: ', A29, ' UTC: ', 0PF10.3, ' Sit: ', A, 1X, A )
 5140                        CONTINUE 
                        END IF
 4130               CONTINUE 
 4120            CONTINUE 
 4110         CONTINUE 
         END IF
         IF ( PIM%CONF%DEBUG_LEVEL == 8 ) THEN
              CALL WALL_TIMER ( STR )
              READ ( UNIT=STR(12:27), FMT='(F16.10)' ) WALL_TIM_VAL
              WALL_TIM_READ_UV = WALL_TIM_READ_UV + WALL_TIM_VAL
              CALL WALL_TIMER ( %VAL(0) )
              N_UV_BYTES = N_UV_BYTES + 4*PIM%NSTK*PIM%FILE(FIL_IND)%NFRQ
              N_UV_BYTES = N_UV_BYTES + 4*2*PIM%NSTK*PIM%NCHN*PIM%FILE(FIL_IND)%NFRQ
         END IF
         IF ( IER .NE. 0 ) THEN
              CALL CLRCH ( STR )
              CALL INCH  ( J1,      STR(1:9)   )
              CALL INCH  ( IND_OBS, STR(11:19) )
              CALL INCH  ( UV_IND,  STR(21:29) )
              CALL INCH  ( POI_IND, STR(31:39) )
              CALL INCH  ( TAB_IND, STR(41:49) )
              CALL INCH  ( FIL_IND, STR(51:59) )
              CALL INCH  ( AUT_IND, STR(61:69) )
              CALL ERR_LOG ( 7535, IUER, 'PIMA_GET_UV', 'Error '// &
     &            'in an attempt to get AUT-data from file '// &
     &             PIM%FILE(FIL_IND)%NAME(1:I_LEN(PIM%FILE(FIL_IND)%NAME))// &
     &            ' for the '//STR(1:9)//' th epoch of the observation '// &
     &            STR(11:19)//' which has UV index '//STR(21:29)// &
     &            ', point index '//STR(31:39)//', tab index '// &
     &            STR(41:49)//', file index '//STR(51:59)// &
     &            ' auto_corr index '//STR(61:69)//' Key: '// &
     &            PIM%FILE(FIL_IND)%KEY(PIM%FILE(FIL_IND)%IND_FLUX_KEY(TAB_IND), &
     &                         PIM%FILE(FIL_IND)%IND_UV_TAB(TAB_IND)) )
              IF ( ALLOCATED ( UV_DATA ) ) DEALLOCATE ( UV_DATA )
              IF ( ALLOCATED ( WEI_ARR ) ) DEALLOCATE ( WEI_ARR )
              RETURN
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
         I_FRQ = 0
         IF ( .NOT. FL_PLD      .AND. PIM%NSTK == 1 .AND. &
     &        PIM%CONF%PHAS_CAL_CODE .NE. PIMA__PCAL_NO   ) THEN
!
! ----------- Get polarization in a case of single polarization
!
              ALLOCATE ( PCAL_C8(PIM%NCHN,N_FRQ,2,1), STAT=IER )
              IF ( IER .NE. 0 ) THEN
                   CALL CLRCH ( STR )
                   CALL IINCH ( 8*PIM%NCHN*N_FRQ, STR )
                   CALL ERR_LOG ( 7539, IUER, 'PIMA_GET_UV', 'Failure to '// &
     &                  'allocate '//STR(1:I_LEN(STR))//' bytes of dynamic '// &
     &                  'memory for array PCAL_C8' )
                   RETURN
              END IF
!
! ----------- Get PCAL for the current polarization
!
              CALL ERR_PASS ( IUER, IER )
              CALL PIMA_USE_PCAL ( PIM, PIMA__APPLY_PCAL, IND_OBS, N_FRQ, &
     &                             B_FRQ, E_FRQ, 1, PCAL_C8(1,1,1,1), &
     &                             PIM%OBS(IND_OBS)%PCAL_GDEL(1,1), IER )
              IF ( IER .NE. 0 ) THEN
                   DEALLOCATE   ( PCAL_C8 )
                   CALL CLRCH ( STR )
                   CALL IINCH ( 8*N_FRQ, STR )
                   FRI_STS = IBSET ( FRI_STS, NPC__PIM )
!
! ---------------- Error code 7540 is protected
!
                   CALL ERR_LOG ( 7540, IUER, 'PIMA_GET_UV', 'Failure in '// &
     &                 'getting phase-cal for RR polarization' )
                   RETURN
              END IF
              FL_PLD = .TRUE. ! flag indicates we have loaded the phase calibration
           ELSE IF ( .NOT. FL_PLD                                    .AND. &
     &                     PIM%CONF%PHAS_CAL_CODE .NE. PIMA__PCAL_NO       ) THEN
!
! ----------- Get polarization in a case of dual polarization
!
              ALLOCATE ( PCAL_C8(PIM%NCHN,N_FRQ,2,2), STAT=IER )
              IF ( IER .NE. 0 ) THEN
                   CALL CLRCH ( STR )
                   CALL IINCH ( 8*PIM%NCHN*N_FRQ, STR )
                   CALL ERR_LOG ( 7536, IUER, 'PIMA_GET_UV', 'Failure to '// &
     &                  'allocate '//STR(1:I_LEN(STR))//' bytes of dynamic '// &
     &                  'memory for array PCAL_C8' )
                   RETURN
              END IF
!
! ----------- Get PCAL for the first polarization
!
              CALL ERR_PASS ( IUER, IER )
              CALL PIMA_USE_PCAL ( PIM, PIMA__APPLY_PCAL, IND_OBS, N_FRQ, B_FRQ, E_FRQ, &
     &                             1, PCAL_C8(1,1,1,1), PIM%OBS(IND_OBS)%PCAL_GDEL(1,1), IER )
              IF ( IER .NE. 0 ) THEN
                   DEALLOCATE   ( PCAL_C8 )
                   CALL CLRCH ( STR )
                   CALL IINCH ( 8*N_FRQ, STR )
                   FRI_STS = IBSET ( FRI_STS, NPC__PIM )
                   CALL ERR_LOG ( 7540, IUER, 'PIMA_GET_UV', 'Failure in '// &
     &                 'getting phase-cal for the 1st polarization' )
                   RETURN
              END IF
!
! ----------- Get PCAL for the second polarization
!
              CALL ERR_PASS ( IUER, IER )
              CALL PIMA_USE_PCAL ( PIM, PIMA__APPLY_PCAL, IND_OBS, N_FRQ, B_FRQ, E_FRQ, &
     &                             2, PCAL_C8(1,1,1,2), PIM%OBS(IND_OBS)%PCAL_GDEL(1,2), IER )
              IF ( IER .NE. 0 ) THEN
                   DEALLOCATE   ( PCAL_C8 )
                   CALL CLRCH ( STR )
                   CALL IINCH ( 8*N_FRQ, STR )
                   FRI_STS = IBSET ( FRI_STS, NPC__PIM )
                   CALL ERR_LOG ( 7538, IUER, 'PIMA_GET_UV', 'Failure in '// &
     &                 'getting phase-cal for the 2nd polarization' )
                   RETURN
              END IF
              FL_PLD = .TRUE. ! flag indicates we have loaded phase calibration
         END IF
         P1 = PIM%OBS(IND_OBS)%FEED_ANG(1)
         P2 = PIM%OBS(IND_OBS)%FEED_ANG(2)
         IF ( FL_PIMAVAR_LINPOL_ROTSWAP ) THEN
              P1 = PIM%OBS(IND_OBS)%FEED_ANG(2)
              P2 = PIM%OBS(IND_OBS)%FEED_ANG(1)
         END IF
         IF ( FL_PIMAVAR_NOFEED_ROT ) THEN
              P1 = 0.0
              P2 = 0.0
         END IF
!
!$OMP    PARALLEL DO IF ( NTHR > 1 .AND. N_FRQ .GE. 4 ), &
!$OMP&                    DEFAULT ( NONE ), &
!$OMP&                    FIRSTPRIVATE ( FIL_IND ), &
!$OMP&                    PRIVATE ( J15, J16, J17, I_FRQ, E_FRG, K_FRQ, K_FRG, FIL_FRQ_IND, &
!$OMP&                              IND_STA, PHAS_R4, PHAS_R4_1ST, PHAS_R4_2ND, &
!$OMP&                              PBP_C8, PC_DIF, BPASS_C8, UNC_OBS, CAL_OBS, CAL_OBS_COPY, &
!$OMP&                              PCAL_PHS, BPAS_PHS, RR_POL, LL_POL, JM_PD, JM_P1, JM_P2, TMP_C8 ), &
!$OMP&                    SHARED ( J1, N_FRQ, B_FRQ, E_FRQ, N_FRG, U_FRG, UV_IND, L_POL, &
!$OMP&                             IND_POLC, IND_POLA, POL_MODE, PCAL_C8, FL_BPASS, IND_OBS, &
!$OMP&                             WEI_ARR, AC_MEAN, UV_DATA, UV, POL_CONF, FIL_INDS, &
!$OMP&                             FL_PIMAVAR_LINPOL_NOROT, &
!$OMP&                             FL_PIMAVAR_LINPOL_ROTANG1, FL_PIMAVAR_LINPOL_ROTANG2, &
!$OMP&                             PIMAVAR_LINPOL_ROTANG1, PIMAVAR_LINPOL_ROTANG2, &
!$OMP&                             FL_PIMAVAR_GETUV_KLUDGE, FL_PIMAVAR_BPS_PHS_SIGN_FLIP, &
!$OMP&                             P1, P2, AC_CAL, VIS_MAX, PIM, iuer ), &
!$OMP&                    SCHEDULE ( STATIC, N_FRQ )
         DO 4150 J15=B_FRQ,E_FRQ
            I_FRQ = J15 - B_FRQ + 1
            IF ( PIM%FRG_USE == PIMA__SINGLE .OR. &
     &           ( PIM%FRG_USE == PIMA__MERGE .AND. N_FRG == 1 ) ) THEN
!
! -------------- Single frequency group case or merged data with only one frequency group
!
                 IF ( PIM%FILE(FIL_IND)%REV_FRQ(J15,U_FRG) .LE. 0  ) THEN
                      GOTO 4150
                 END IF
                 E_FRG = 1      ! Extended group index
                 K_FRQ = J15    ! frequency index
                 K_FRG = U_FRG  ! frequency group ubdex 
                 FIL_FRQ_IND = PIM%FILE(FIL_IND)%REV_FRQ(J15,K_FRG) ! Frequency index in the table that in the file
               ELSE IF ( PIM%FRG_USE == PIMA__COMBINE ) THEN
!
! -------------- Combined frequency group table
!
                 IF ( PIM%UV_IND(UV_IND)%FRG_IND .NE. PIM%REV_FRG(J15) ) THEN
                      UV(1:PIM%NCHN,I_FRQ,J1,1:L_POL) = ( 0.0, 0.0 )
                      GOTO 4150
                 END IF
                 FIL_IND = PIM%UV_IND(UV_IND)%FIL_IND
                 FIL_FRQ_IND = PIM%FILE(FIL_IND)%REV_FRQ(PIM%REV_FRQ(J15),PIM%REV_FRG(J15))
                 E_FRG = 1
                 K_FRQ = PIM%REV_FRQ(J15)
                 K_FRG = U_FRG            ! index of virtial frequency group
               ELSE IF ( PIM%FRG_USE == PIMA__MERGE .AND. N_FRG > 1 ) THEN
!
! -------------- A case of merged frequency group with chained data
!
                 E_FRG = PIM%REV_FRG(J15) ! Global frequency group index before merging
                 K_FRQ = PIM%REV_FRQ(J15) ! Frequency index in the global table before merging
                 K_FRG = 1                ! index of the virtial frequency group
                 FIL_IND = FIL_INDS(E_FRG)
                 FIL_FRQ_IND = PIM%FILE(FIL_IND)%REV_FRQ(K_FRQ,E_FRG)
            END IF
!
            IF ( PIM%NSTK == 1 ) THEN
!
! ============== Single polarization case
!
                 DO 4160 J16=1,PIM%NCHN
!
! ----------------- 1(4) Copy the data
!
                    IF ( WEI_ARR(IND_POLC,FIL_FRQ_IND,E_FRG) < PIM%CONF%FRIB_WEIGHTS_THRESHOLD ) THEN
                         UV(J16,I_FRQ,J1,1:L_POL) = ( 0.0, 0.0 )
                         GOTO 4160
                    END IF
                    IF ( PIM%FRQ(K_FRQ,K_FRG)%SIDE_BAND == 1 ) THEN
!
! ---------------------- Upper side-band
!
                         UV(J16,I_FRQ,J1,1) = UV_DATA(IND_POLC,J16,FIL_FRQ_IND,E_FRG)/WEI_ARR(IND_POLC,FIL_FRQ_IND,E_FRG)
                       ELSE
!
! ---------------------- Lower side-band
!
                         UV(J16,I_FRQ,J1,1) = CONJG ( UV_DATA(IND_POLC,PIM%NCHN-J16+1,FIL_FRQ_IND,E_FRG)/ &
     &                                                WEI_ARR(IND_POLC,FIL_FRQ_IND,E_FRG) )
                    END IF
!
! ----------------- Sanity check
!
                    IF ( ABS(REAL(UV(J16,I_FRQ,J1,1))) > VIS_MAX .OR. &
     &                   ABS(IMAG(UV(J16,I_FRQ,J1,1))) > VIS_MAX .OR. &
     &                   IS_R4_NAN(REAL(UV(J16,I_FRQ,J1,1)))     .OR. &
     &                   IS_R4_NAN(IMAG(UV(J16,I_FRQ,J1,1)))     .OR. &
     &                   IS_R4_INF(REAL(UV(J16,I_FRQ,J1,1)))     .OR. &
     &                   IS_R4_INF(IMAG(UV(J16,I_FRQ,J1,1)))          ) THEN
                         UV(J16,I_FRQ,J1,1:L_POL) = ( 0.0, 0.0 )
                    END IF
!
                    IF ( PIM%CONF%PHAS_CAL_CODE .NE. PIMA__PCAL_NO  ) THEN
!
! ---------------------- 2(4) Apply phase calibration 
!
                         UV(J16,I_FRQ,J1,1) = UV(J16,I_FRQ,J1,1) * &
     &                                        PCAL_C8(J16,I_FRQ,1,1) * &
     &                                        PCAL_C8(J16,I_FRQ,2,1)
                    END IF
                    IF ( FL_BPASS ) THEN
!
! ---------------------- Get bandpass calibration. NB: sign!
!
                         PHAS_R4 = -PHAS_CMPL_R4 ( PIM%BPASS(PIM%OBS(IND_OBS)%STA_IND(1))%BPS(J16,J15) ) &
     &                             +PHAS_CMPL_R4 ( PIM%BPASS(PIM%OBS(IND_OBS)%STA_IND(2))%BPS(J16,J15) )
                         IF ( PIM%CONF%BANDPASS_USE == PIMA__BPASS_AMP_PHS  ) THEN
                              BPASS_C8 = CMPLX ( COS(-PHAS_R4), SIN(-PHAS_R4) )
                            ELSE IF ( PIM%CONF%BANDPASS_USE == PIMA__BPASS_PHS ) THEN
                              BPASS_C8 = CMPLX ( COS(-PHAS_R4), SIN(-PHAS_R4) )
                           ELSE
                              BPASS_C8 = CMPLX ( 1.0, 0.0 )
                         END IF
!
! ---------------------- 3(4) Apply bandpass
!
                         UV(J16,I_FRQ,J1,1) = UV(J16,I_FRQ,J1,1)*BPASS_C8
                    END IF
!
                    IF ( PIM%CONF%FRIB_AUTOCORR_CALIB == PIMA__ACCR_SQRT_MEA .OR. &
     &                   PIM%CONF%FRIB_AUTOCORR_CALIB == PIMA__ACCR_SQRT_EVR .OR. &
     &                   PIM%CONF%FRIB_AUTOCORR_CALIB == PIMA__ACCR_SQRT_KOG ) THEN
!
! ---------------------- 4(4) Normalize observations for their mean autocorrelation
!
                         IF ( AC_MEAN(I_FRQ,1,IND_POLA(1,1)) > PIM%CONF%FRIB_AUTOCORR_THRESHOLD .AND. &
     &                        AC_MEAN(I_FRQ,2,IND_POLA(2,1)) > PIM%CONF%FRIB_AUTOCORR_THRESHOLD      ) THEN
                              UV(J16,I_FRQ,J1,1) = UV(J16,I_FRQ,J1,1) / &
     &                                             SQRT ( ABS(AC_MEAN(I_FRQ,1,IND_POLA(1,1)))* &
     &                                                    ABS(AC_MEAN(I_FRQ,2,IND_POLA(2,1)))  )
                            ELSE
                              UV(J16,I_FRQ,J1,1) = ( 0.0, 0.0 )
                         END IF
                       ELSE IF ( PIM%CONF%FRIB_AUTOCORR_CALIB == PIMA__ACCR_INTG .OR. &
     &                           PIM%CONF%FRIB_AUTOCORR_CALIB == PIMA__ACCR_VLBA_CNST1 .OR. &
     &                           PIM%CONF%FRIB_AUTOCORR_CALIB == PIMA__ACCR_VLBA_CNST2      ) THEN
                         IF ( AC_MEAN(I_FRQ,1,IND_POLA(1,1)) > PIM%CONF%FRIB_AUTOCORR_THRESHOLD .OR. &
     &                        AC_MEAN(I_FRQ,2,IND_POLA(2,1)) > PIM%CONF%FRIB_AUTOCORR_THRESHOLD      ) THEN
                              UV(J16,I_FRQ,J1,1) = UV(J16,I_FRQ,J1,1) / &
     &                                             SQRT ( ABS(AC_MEAN(I_FRQ,1,IND_POLA(1,1))) * &
     &                                                    ABS(AC_MEAN(I_FRQ,2,IND_POLA(2,1))) )
                           ELSE
!
! -------------------------- Bypass observations with very small
! -------------------------- autocorrelations
!
                             UV(J16,I_FRQ,J1,1) = ( 0.0, 0.0 )
                         END IF
                    END IF
 4160            CONTINUE 
               ELSE
!
! ============== Dual polarization case. All four polarization combinations
!
                 DO 4170 J17=1,PIM%NCHN
!
! ----------------- 1(7) Copy the data
!
                    IF ( WEI_ARR(1,FIL_FRQ_IND,E_FRG) < PIM%CONF%FRIB_WEIGHTS_THRESHOLD .AND. &
     &                   WEI_ARR(2,FIL_FRQ_IND,E_FRG) < PIM%CONF%FRIB_WEIGHTS_THRESHOLD       ) THEN
                         UV(J17,I_FRQ,J1,1:L_POL) = ( 0.0, 0.0 )
                         GOTO 4170
                    END IF
!
! ----------------- Order: a) cir-cir:  RR  RL    1: rr; 2: ll; 3: rl; 4: lr
!                                       LR  LL
!
! ----------------- Order: b) lin-lin:  HH  HV    1: hh; 2: vv; 3: hv; 4: vh
!                                       VH  VV
!
! ----------------- Order: c) lin-cir:  HR  HL    1: hr; 2: vl; 3: hl; 4: vr
!                                       VR  VL
!
! ----------------- Order: d) cir-lin:  RH  RV    1: rh; 2: lv; 3: rv; 4: lh
!                                       LH  LV
!
                    IF ( PIM%FRQ(K_FRQ,K_FRG)%SIDE_BAND == 1 ) THEN
!
! ---------------------- Upper side-band
!
                         IF ( WEI_ARR(1,FIL_FRQ_IND,E_FRG) > PIMA__WEI_MIN ) THEN
                              UNC_OBS(1,1) = UV_DATA(1,J17,FIL_FRQ_IND,E_FRG)/WEI_ARR(1,FIL_FRQ_IND,E_FRG) ! xx
                           ELSE 
                              UNC_OBS(1,1) = 0.0
                         END IF
                         IF ( WEI_ARR(2,FIL_FRQ_IND,E_FRG) > PIMA__WEI_MIN ) THEN
                              UNC_OBS(2,2) = UV_DATA(2,J17,FIL_FRQ_IND,E_FRG)/WEI_ARR(2,FIL_FRQ_IND,E_FRG) ! yy
                           ELSE 
                              UNC_OBS(2,2) = 0.0
                         END IF
                         IF ( PIM%NSTK == 4 ) THEN
                              IF ( WEI_ARR(3,FIL_FRQ_IND,E_FRG) > PIMA__WEI_MIN ) THEN
                                   UNC_OBS(1,2) = UV_DATA(3,J17,FIL_FRQ_IND,E_FRG)/WEI_ARR(3,FIL_FRQ_IND,E_FRG) ! xy 
                                 ELSE
                                   UNC_OBS(1,2) = 0.0
                              END IF
                              IF ( WEI_ARR(4,FIL_FRQ_IND,E_FRG) > PIMA__WEI_MIN ) THEN
                                   UNC_OBS(2,1) = UV_DATA(4,J17,FIL_FRQ_IND,E_FRG)/WEI_ARR(4,FIL_FRQ_IND,E_FRG) ! yx
                                 ELSE
                                   UNC_OBS(2,1) = 0.0
                              END IF
                            ELSE 
                              UNC_OBS(1,2) = 0.0
                              UNC_OBS(2,1) = 0.0
                         END IF
                       ELSE
!
! ---------------------- Lower side-band
!
                         IF ( WEI_ARR(1,FIL_FRQ_IND,E_FRG) > PIMA__WEI_MIN ) THEN
                              UNC_OBS(1,1) = CONJG ( UV_DATA(1,PIM%NCHN-J17+1,FIL_FRQ_IND,E_FRG) )/WEI_ARR(1,FIL_FRQ_IND,E_FRG)
                           ELSE 
                              UNC_OBS(1,1) = 0.0
                         END IF
                         IF ( WEI_ARR(2,FIL_FRQ_IND,E_FRG) > PIMA__WEI_MIN ) THEN
                              UNC_OBS(2,2) = CONJG ( UV_DATA(2,PIM%NCHN-J17+1,FIL_FRQ_IND,E_FRG) )/WEI_ARR(2,FIL_FRQ_IND,E_FRG)
                           ELSE 
                              UNC_OBS(2,2) = 0.0
                         END IF
                         IF ( PIM%NSTK == 4 ) THEN
                              IF ( WEI_ARR(3,FIL_FRQ_IND,E_FRG) > PIMA__WEI_MIN ) THEN
                                   UNC_OBS(1,2) = CONJG ( UV_DATA(3,PIM%NCHN-J17+1,FIL_FRQ_IND,E_FRG) )/WEI_ARR(3,FIL_FRQ_IND,E_FRG)
                                 ELSE
                                   UNC_OBS(1,2) = 0.0
                              END IF
                              IF ( WEI_ARR(4,FIL_FRQ_IND,E_FRG) > PIMA__WEI_MIN ) THEN
                                   UNC_OBS(2,1) = CONJG ( UV_DATA(4,PIM%NCHN-J17+1,FIL_FRQ_IND,E_FRG) )/WEI_ARR(4,FIL_FRQ_IND,E_FRG)
                                 ELSE
                                   UNC_OBS(2,1) = 0.0
                              END IF
                            ELSE 
                              UNC_OBS(1,2) = 0.0
                              UNC_OBS(2,1) = 0.0
                         END IF
                    END IF
!
! ----------------- Sanity check
!
                    IF ( ABS(REAL(UNC_OBS(1,1))) > VIS_MAX .OR. &
     &                   ABS(IMAG(UNC_OBS(1,1))) > VIS_MAX .OR. &
     &                   IS_R4_NAN(REAL(UNC_OBS(1,1)))     .OR. &
     &                   IS_R4_NAN(IMAG(UNC_OBS(1,1)))     .OR. &
     &                   IS_R4_INF(REAL(UNC_OBS(1,1)))     .OR. &
     &                   IS_R4_INF(IMAG(UNC_OBS(1,1)))          ) THEN
                         UNC_OBS = ( 0.0, 0.0 )
                    END IF
                    IF ( ABS(REAL(UNC_OBS(2,2))) > VIS_MAX .OR. &
     &                   ABS(IMAG(UNC_OBS(2,2))) > VIS_MAX .OR. &
     &                   IS_R4_NAN(REAL(UNC_OBS(2,2)))     .OR. &
     &                   IS_R4_NAN(IMAG(UNC_OBS(2,2)))     .OR. &
     &                   IS_R4_INF(REAL(UNC_OBS(2,2)))     .OR. &
     &                   IS_R4_INF(IMAG(UNC_OBS(2,2)))          ) THEN
                         UNC_OBS = ( 0.0, 0.0 )
                    END IF
!
                    IF ( PIM%CONF%PHAS_CAL_CODE .NE. PIMA__PCAL_NO ) THEN
                         PCAL_PHS(1,1) = -PHAS_CMPL_R4 ( PCAL_C8(J17,I_FRQ,1,1) ) ! first  station, 1st pol
                         PCAL_PHS(2,1) =  PHAS_CMPL_R4 ( PCAL_C8(J17,I_FRQ,2,1) ) ! second station, 1st pol
                         PCAL_PHS(1,2) = -PHAS_CMPL_R4 ( PCAL_C8(J17,I_FRQ,1,2) ) ! first  station, 2nd pol
                         PCAL_PHS(2,2) =  PHAS_CMPL_R4 ( PCAL_C8(J17,I_FRQ,2,2) ) ! second station, 2nd pol
                       ELSE
                         PCAL_PHS = 0.0
                    END IF
                    IF ( PIM%CONF%BANDPASS_USE == PIMA__BPASS_PHS .OR. &
     &                   PIM%CONF%BANDPASS_USE == PIMA__BPASS_AMP_PHS  ) THEN
                         IF ( ABS(PIM%BPASS(PIM%OBS(IND_OBS)%STA_IND(1))%BPS(J17,J15)) > PIMA__PCAL_AMP_MIN ) THEN
                              BPAS_PHS(1,1) = PHAS_CMPL_R4 ( PIM%BPASS(PIM%OBS(IND_OBS)%STA_IND(1))%BPS(J17,J15) )
                            ELSE 
                              BPAS_PHS(1,1) = 0.0
                         END IF
                         IF ( ABS(PIM%BPASS(PIM%OBS(IND_OBS)%STA_IND(1))%BPS(J17,J15)) > PIMA__PCAL_AMP_MIN ) THEN
                              BPAS_PHS(2,1) = PHAS_CMPL_R4 ( PIM%BPASS(PIM%OBS(IND_OBS)%STA_IND(2))%BPS(J17,J15) )
                            ELSE 
                              BPAS_PHS(2,1) = 0.0
                         END IF
                       ELSE
                         BPAS_PHS(1,1) =  0.0
                         BPAS_PHS(2,1) =  0.0
                    END IF
                    IF ( FL_PIMAVAR_BPS_PHS_SIGN_FLIP ) THEN
                         BPAS_PHS = -BPAS_PHS
                    END IF
                    IF ( PIM%CONF%POLARCAL_FILE .NE. PIMA__POLARCAL_NO .AND. &
     &                   ASSOCIATED ( PIM%PBP ) ) THEN
                         BPAS_PHS(1,2) =  PHAS_CMPL_R4 ( PIM%PBP(PIM%OBS(IND_OBS)%STA_IND(1))%CMPL(J17,J15) )
                         BPAS_PHS(2,2) =  PHAS_CMPL_R4 ( PIM%PBP(PIM%OBS(IND_OBS)%STA_IND(2))%CMPL(J17,J15) )
                      ELSE
                         BPAS_PHS(1,2) = 0.0
                         BPAS_PHS(2,2) = 0.0
                    END IF
!
                    IF ( PIM%CONF%FRIB_AUTOCORR_CALIB == PIMA__ACCR_SQRT_MEA   .OR. &
     &                   PIM%CONF%FRIB_AUTOCORR_CALIB == PIMA__ACCR_SQRT_EVR   .OR. &
     &                   PIM%CONF%FRIB_AUTOCORR_CALIB == PIMA__ACCR_SQRT_KOG   .OR. &
     &                   PIM%CONF%FRIB_AUTOCORR_CALIB == PIMA__ACCR_INTG       .OR. &
     &                   PIM%CONF%FRIB_AUTOCORR_CALIB == PIMA__ACCR_VLBA_CNST1 .OR. &
     &                   PIM%CONF%FRIB_AUTOCORR_CALIB == PIMA__ACCR_VLBA_CNST2      ) THEN
!
! ---------------------- 4(7) Get Jones matrix for normalization for the mean autocorrelation
!
                         IF ( AC_MEAN(I_FRQ,1,1) > PIM%CONF%FRIB_AUTOCORR_THRESHOLD .AND. &
     &                        AC_MEAN(I_FRQ,1,2) > PIM%CONF%FRIB_AUTOCORR_THRESHOLD       ) THEN
                              AC_CAL(1,1) = 1.0/SQRT(ABS(AC_MEAN(I_FRQ,1,1)))
                              AC_CAL(1,2) = 1.0/SQRT(ABS(AC_MEAN(I_FRQ,1,2)))
                            ELSE
                              AC_CAL(1,1) = 1.0
                              AC_CAL(1,2) = 1.0
                         END IF
                         IF ( AC_MEAN(I_FRQ,2,1) > PIM%CONF%FRIB_AUTOCORR_THRESHOLD .AND. &
     &                        AC_MEAN(I_FRQ,2,2) > PIM%CONF%FRIB_AUTOCORR_THRESHOLD       ) THEN
!
! --------------------------- For the 2nd station
!
                              AC_CAL(2,1) = 1.0/SQRT(ABS(AC_MEAN(I_FRQ,2,1)))
                              AC_CAL(2,2) = 1.0/SQRT(ABS(AC_MEAN(I_FRQ,2,2)))
                            ELSE
                              AC_CAL(2,1) = 1.0
                              AC_CAL(2,2) = 1.0
                         END IF
                       ELSE
                         AC_CAL = 1.0
                    END IF
!
! ----------------- Apply phase calibration
!
! ----------------- NB: Conjugation for the 2nd station. 
! ----------------- PIMA_USE_PCAL sets sign -1 for the 1st station and +1 for the second
! ----------------- station 
!
                    CAL_OBS(1,1) = UNC_OBS(1,1)*AC_CAL(1,1)*AC_CAL(2,1)* &
     &                                          CMPLX( COS( PCAL_PHS(2,1)-PCAL_PHS(1,1) ), &
     &                                                 SIN( PCAL_PHS(2,1)-PCAL_PHS(1,1) )  )
                    CAL_OBS(1,2) = UNC_OBS(1,2)*AC_CAL(1,1)*AC_CAL(2,2)* &
     &                                          CMPLX( COS( PCAL_PHS(2,2)-PCAL_PHS(1,1) ), &
     &                                                 SIN( PCAL_PHS(2,2)-PCAL_PHS(1,1) )  )
                    CAL_OBS(2,1) = UNC_OBS(2,1)*AC_CAL(1,2)*AC_CAL(2,1)* &
     &                                          CMPLX( COS( PCAL_PHS(2,1)-PCAL_PHS(1,2) ), &
     &                                                 SIN( PCAL_PHS(2,1)-PCAL_PHS(1,2) )  )
                    CAL_OBS(2,2) = UNC_OBS(2,2)*AC_CAL(1,2)*AC_CAL(2,2)* &
     &                                          CMPLX( COS( PCAL_PHS(2,2)-PCAL_PHS(1,2) ), &
     &                                                 SIN( PCAL_PHS(2,2)-PCAL_PHS(1,2) )  )
!!!!!!!
!@                    CAL_OBS_COPY = CAL_OBS
!@                    IF ( POL_CONF == PIMA__PC_LL ) THEN
!@                         CAL_OBS(1,1) = CAL_OBS(1,1) / &
!@     &                                      CMPLX ( COS(BPAS_PHS(2,1)-BPAS_PHS(1,1)), &
!@     &                                              SIN(BPAS_PHS(2,1)-BPAS_PHS(1,1))  )
!@                         CAL_OBS(2,1) = CAL_OBS(2,1) / &
!@     &                                      CMPLX ( COS(BPAS_PHS(2,1)-BPAS_PHS(1,1)), &
!@     &                                              SIN(BPAS_PHS(2,1)-BPAS_PHS(1,1))  ) * &
!@     &                                      CMPLX ( COS(-BPAS_PHS(1,2)), &
!@     &                                              SIN(-BPAS_PHS(1,2))  )
!@                         CAL_OBS(1,2) = CAL_OBS(1,2) / &
!@     &                                      CMPLX ( COS(BPAS_PHS(2,1)-BPAS_PHS(1,1)), &
!@     &                                              SIN(BPAS_PHS(2,1)-BPAS_PHS(1,1))  ) * &
!@     &                                      CMPLX ( COS(BPAS_PHS(2,2)), &
!@     &                                              SIN(BPAS_PHS(2,2))  )
!@                         CAL_OBS(2,2) = CAL_OBS(2,2) / &
!@     &                                      CMPLX ( COS(BPAS_PHS(2,1)-BPAS_PHS(1,1)), &
!@     &                                              SIN(BPAS_PHS(2,1)-BPAS_PHS(1,1))  ) * &
!@     &                                      CMPLX ( COS(BPAS_PHS(2,2)-BPAS_PHS(1,2)), &
!@     &                                              SIN(BPAS_PHS(2,2)-BPAS_PHS(1,2))  )
!@                         IF ( POL_MODE == PIMA__PALL_NOR .OR. &
!@     &                        POL_MODE == PIMA__HHLL     .OR. &
!@     &                        POL_MODE == PIMA__VVLL     .OR. &
!@     &                        POL_MODE == PIMA__HVLL     .OR. &
!@     &                        POL_MODE == PIMA__VHLL     .OR. &
!@     &                        POL_MODE == PIMA__HRLC     .OR. &
!@     &                        POL_MODE == PIMA__VRLC     .OR. &
!@     &                        POL_MODE == PIMA__VRLC     .OR. &
!@     &                        POL_MODE == PIMA__VLLC     .OR. &
!@     &                        POL_MODE == PIMA__LHCL     .OR. &
!@     &                        POL_MODE == PIMA__RHCL     .OR. &
!@     &                        POL_MODE == PIMA__LVCL     .OR. &
!@     &                        POL_MODE == PIMA__RVCL     .OR. &
!@     &                        FL_PIMAVAR_LINPOL_NOROT         ) THEN
!@!
!@! --------------------------- No polarization rotation
!@!
!@                              CONTINUE 
!@                           ELSE IF ( POL_MODE == PIMA__PALL_XY .OR. &
!@     &                               POL_MODE == PIMA__XXLL    .OR. &
!@     &                               POL_MODE == PIMA__YYLL    .OR. &
!@     &                               POL_MODE == PIMA__XYLL    .OR. &
!@     &                               POL_MODE == PIMA__YXLL    .OR. &
!@     &                               POL_MODE == PIMA__IPLL    .OR. &
!@     &                               POL_MODE == PIMA__QPLL    .OR. &
!@     &                               POL_MODE == PIMA__UPLL    .OR. &
!@     &                               POL_MODE == PIMA__VPLL         ) THEN
!@!
!@! --------------------------- Polarization rotation to XY frame: X along right ascension, Y along declination
!@!
!@                              IF ( FL_PIMAVAR_GETUV_KLUDGE ) THEN
!@                                   TMP_C8(1,1) = CAL_OBS(1,1)*COS(P1) - CAL_OBS(2,1)*SIN(P1) 
!@                                   TMP_C8(1,2) = CAL_OBS(1,2)*COS(P1) - CAL_OBS(2,2)*SIN(P1) 
!@                                   TMP_C8(2,1) = CAL_OBS(2,1)*COS(P1) + CAL_OBS(1,1)*SIN(P1)
!@                                   TMP_C8(2,2) = CAL_OBS(2,2)*COS(P1) + CAL_OBS(1,2)*SIN(P1) 
!@                              END IF
!@!
!@!                              tmp_c8(1,1) = cal_obs(1,1)*cos(p1-p2) - cal_obs(2,1)*sin(p1-p2) 
!@!                              tmp_c8(1,2) = cal_obs(1,2)*cos(p1-p2) - cal_obs(2,2)*sin(p1-p2) 
!@!                              tmp_c8(2,1) = cal_obs(2,1)*cos(p1-p2) + cal_obs(1,1)*sin(p1-p2)
!@!                              tmp_c8(2,2) = cal_obs(2,2)*cos(p1-p2) + cal_obs(1,2)*sin(p1-p2) 
!@!
!@                              TMP_C8(1,1) =  CAL_OBS(1,1)*COS(P1-P2) + CAL_OBS(2,1)*SIN(P1-P2) 
!@                              TMP_C8(1,2) =  CAL_OBS(1,2)*COS(P1-P2) + CAL_OBS(2,2)*SIN(P1-P2) 
!@                              TMP_C8(2,1) =  CAL_OBS(2,1)*COS(P1-P2) - CAL_OBS(1,1)*SIN(P1-P2)
!@                              TMP_C8(2,2) =  CAL_OBS(2,2)*COS(P1-P2) - CAL_OBS(1,2)*SIN(P1-P2) 
!@                              CAL_OBS = TMP_C8
!@                           ELSE IF ( POL_MODE == PIMA__PALL_1ST ) THEN
!@!
!@! --------------------------- Polarization rotation to HV frame of the 1st ( reference station )
!@!
!@                              TMP_C8(1,1) = CAL_OBS(1,1)*COS(P1-P2) + CAL_OBS(1,2)*SIN(P1-P2)
!@                              TMP_C8(1,2) = CAL_OBS(1,2)*COS(P1-P2) - CAL_OBS(1,1)*SIN(P1-P2) 
!@                              TMP_C8(2,1) = CAL_OBS(2,1)*COS(P1-P2) + CAL_OBS(2,2)*SIN(P1-P2)
!@                              TMP_C8(2,2) = CAL_OBS(2,2)*COS(P1-P2) - CAL_OBS(2,1)*SIN(P1-P2) 
!@                              CAL_OBS = TMP_C8
!@                           ELSE IF ( POL_MODE == PIMA__PALL_2ND ) THEN
!@!
!@! --------------------------- Polarization rotation to HV frame of the 2nd ( remote    station )
!@!
!@                              TMP_C8(1,1) = CAL_OBS(1,1)*COS(P1-P2) - CAL_OBS(2,1)*SIN(P1-P2) 
!@                              TMP_C8(1,2) = CAL_OBS(1,2)*COS(P1-P2) - CAL_OBS(2,2)*SIN(P1-P2) 
!@                              TMP_C8(2,1) = CAL_OBS(1,1)*SIN(P1-P2) + CAL_OBS(2,1)*COS(P1-P2)
!@                              TMP_C8(2,2) = CAL_OBS(1,2)*SIN(P1-P2) + CAL_OBS(2,2)*COS(P1-P2) 
!@                              CAL_OBS = TMP_C8
!@                           ELSE IF ( POL_MODE == PIMA__PALL_MIXED ) THEN
!@                              TMP_C8(1,1) = CAL_OBS(1,1)*COS(P1-P2) + CAL_OBS(1,2)*SIN(P1-P2) ! 1,1 of the HV 1st
!@                              TMP_C8(2,1) = CAL_OBS(2,2)*COS(P1-P2) - CAL_OBS(2,1)*SIN(P1-P2) ! 2,2 of the HV 1st
!@                              TMP_C8(1,2) = CAL_OBS(1,1)*COS(P1-P2) - CAL_OBS(2,1)*SIN(P1-P2) ! 1,1 of the HV 2nd
!@                              TMP_C8(2,2) = CAL_OBS(1,2)*SIN(P1-P2) + CAL_OBS(2,2)*COS(P1-P2) ! 2,2 of the HV 2nd
!@                              CAL_OBS = TMP_C8
!@                         END IF
!@!
!@                         IF ( FL_PIMAVAR_LINPOL_ROTANG1 .AND. &
!@     &                        FL_PIMAVAR_LINPOL_ROTANG2       ) THEN
!@!
!@                              JM_P1(1,1) = COS( PIMAVAR_LINPOL_ROTANG1)
!@                              JM_P1(1,2) = SIN(-PIMAVAR_LINPOL_ROTANG1)
!@                              JM_P1(2,1) = SIN( PIMAVAR_LINPOL_ROTANG1)
!@                              JM_P1(2,2) = COS( PIMAVAR_LINPOL_ROTANG1)
!@!
!@                              JM_P2(1,1) = COS( PIMAVAR_LINPOL_ROTANG2)
!@                              JM_P2(1,2) = SIN(-PIMAVAR_LINPOL_ROTANG2)
!@                              JM_P2(2,1) = SIN( PIMAVAR_LINPOL_ROTANG2)
!@                              JM_P2(2,2) = COS( PIMAVAR_LINPOL_ROTANG2)
!@!
!@                              CALL MUL_C1H ( JM_P1, CAL_OBS_COPY, JM_P2, TMP_C8 )
!@                              CAL_OBS = TMP_C8
!@                         END IF
!@                    END IF
!!!!!
                    CAL_OBS = CAL_OBS / CMPLX ( COS(BPAS_PHS(2,1)-BPAS_PHS(1,1)), &
     &                                          SIN(BPAS_PHS(2,1)-BPAS_PHS(1,1))  )
!
                    IF ( POL_CONF == PIMA__PC_CC .AND. POL_MODE == PIMA__RRCC ) THEN
!
! ======================= Cir-cir polarization case
!
! ----------------------- Cir-cir polarization, RR-pol, no parallactic angle rotation
!
                          UV(J17,I_FRQ,J1,1) = CAL_OBS(1,1)
                        ELSE IF ( POL_CONF == PIMA__PC_CC .AND. POL_MODE == PIMA__LRCC ) THEN
!
! ------------------------ Cir-cir polarization case, LR-pol, no parallactic angle rotation
!
                           UV(J17,I_FRQ,J1,1) = CAL_OBS(2,1)/ &
     &                                              CMPLX ( COS(-BPAS_PHS(1,2)), SIN(-BPAS_PHS(1,2))  )
                        ELSE IF ( POL_CONF == PIMA__PC_CC .AND. POL_MODE == PIMA__RLCC ) THEN
!
! ------------------------ Cir-cir polarization case, RL-pol, no parallactic angle rotation
!
                           UV(J17,I_FRQ,J1,1) = CAL_OBS(1,2)/ &
     &                                              CMPLX ( COS(BPAS_PHS(2,2)), SIN(BPAS_PHS(2,2))  )
                        ELSE IF ( POL_CONF == PIMA__PC_CC .AND. POL_MODE == PIMA__LLCC ) THEN
!
! ------------------------ Cir-cir polarization case, LL-pol, doubled parallactic angle rotation
!
                           UV(J17,I_FRQ,J1,1) = CAL_OBS(2,2) * &
     &                                              CMPLX ( COS(2*(P1-P2) ), SIN(2*(P1-P2) ) )/ &
     &                                              CMPLX ( COS(BPAS_PHS(2,2)-BPAS_PHS(1,2)), &
     &                                                      SIN(BPAS_PHS(2,2)-BPAS_PHS(1,2))  )
                        ELSE IF ( POL_CONF == PIMA__PC_CC .AND. POL_MODE == PIMA__PAR ) THEN
!
! ------------------------ Cir-cir polarization,  RR-pol and LL-pol, parallactic angle rotation is applied
!
                           UV(J17,I_FRQ,J1,1) = CAL_OBS(1,1) * &
     &                                              CMPLX ( COS(-(P1-P2)), SIN(-(P1-P2)) )
                           UV(J17,I_FRQ,J1,2) = CAL_OBS(2,2) * &
     &                                              CMPLX ( COS(  P1-P2 ), SIN(  P1-P2 ) )/ &
     &                                              CMPLX ( COS(BPAS_PHS(2,2)-BPAS_PHS(1,2)), &
     &                                                      SIN(BPAS_PHS(2,2)-BPAS_PHS(1,2))  )
                        ELSE IF ( POL_CONF == PIMA__PC_CC .AND. POL_MODE == PIMA__PALL_CIR ) THEN
!
! ------------------------ Cir-cir polarization, all polarizations, parallactic angle rotation is applied
!
                           UV(J17,I_FRQ,J1,1) = CAL_OBS(1,1) * &
     &                                              CMPLX ( COS(-(P1-P2)), SIN(-(P1-P2)) )
                           UV(J17,I_FRQ,J1,2) = CAL_OBS(2,1) * &
     &                                              CMPLX ( COS(  P1+P2 ), SIN(  P1+P2 ) )/ &
     &                                              CMPLX ( COS(-BPAS_PHS(1,2)), SIN(-BPAS_PHS(1,2)) )
                           UV(J17,I_FRQ,J1,3) = CAL_OBS(1,2) * &
     &                                              CMPLX ( COS(-(P1+P2)), SIN(-(P1+P2)) )/ &
     &                                              CMPLX ( COS( BPAS_PHS(2,2)), SIN( BPAS_PHS(2,2)) )
                           UV(J17,I_FRQ,J1,4) = CAL_OBS(2,2) * &
     &                                              CMPLX ( COS(  P1-P2 ), SIN(  P1-P2 ) )/   &
     &                                              CMPLX ( COS(BPAS_PHS(2,2)-BPAS_PHS(1,2)), &
     &                                                      SIN(BPAS_PHS(2,2)-BPAS_PHS(1,2))  )
                        ELSE IF ( POL_CONF == PIMA__PC_CC .AND. POL_MODE == PIMA__IPCC ) THEN
!
! ------------------------ Cir-cir polarization,  I-pol, parallactic angle rotation is applied
!
                           UV(J17,I_FRQ,J1,1) = ( CAL_OBS(1,1) * &
     &                                                CMPLX ( COS(-(P1-P2)), SIN(-(P1-P2)) ) +   &
     &                                            CAL_OBS(2,2) * &
     &                                                CMPLX ( COS(  P1-P2 ), SIN(  P1-P2 ) )/    &
     &                                                CMPLX ( COS(BPAS_PHS(2,2)-BPAS_PHS(1,2)),  &
     &                                                        SIN(BPAS_PHS(2,2)-BPAS_PHS(1,2)) ) &
     &                                          )/2.0
!
! ===================== Lin-lin polarization case
!
                        ELSE IF ( POL_CONF == PIMA__PC_LL .AND. POL_MODE == PIMA__HHLL ) THEN
!
! ------------------------ Lin-lin polarization, HH-pol
!
                           UV(J17,I_FRQ,J1,1) = CAL_OBS(1,1)
                        ELSE IF ( POL_CONF == PIMA__PC_LL .AND. POL_MODE == PIMA__VVLL ) THEN
!
! ------------------------ Lin-lin polarization, VV-pol
!
                           UV(J17,I_FRQ,J1,1) = CAL_OBS(2,2)
                        ELSE IF ( POL_CONF == PIMA__PC_LL .AND. POL_MODE == PIMA__HVLL ) THEN
!
! ------------------------ Lin-lin polarization, HV-pol
!
                           UV(J17,I_FRQ,J1,1) = CAL_OBS(1,2)
                        ELSE IF ( POL_CONF == PIMA__PC_LL .AND. POL_MODE == PIMA__VHLL ) THEN
!
! ------------------------ Lin-lin polarization, VH-pol
!
                           UV(J17,I_FRQ,J1,1) = CAL_OBS(2,1)
                        ELSE IF ( POL_MODE == PIMA__PALL_MIXED ) THEN
!
! ------------------------ lin-lin polarization, apply only polarization  rotation
!
                           UV(J17,I_FRQ,J1,1) = CAL_OBS(1,1)*COS(P1-P2) + CAL_OBS(1,2)*SIN(P1-P2) ! 1,1 of the HV 1st
                           UV(J17,I_FRQ,J1,2) = CAL_OBS(2,2)*COS(P1-P2) - CAL_OBS(2,1)*SIN(P1-P2) ! 2,2 of the HV 1st
                           UV(J17,I_FRQ,J1,3) = CAL_OBS(1,1)*COS(P1-P2) - CAL_OBS(2,1)*SIN(P1-P2) ! 1,1 of the HV 2nd
                           UV(J17,I_FRQ,J1,4) = CAL_OBS(1,2)*SIN(P1-P2) + CAL_OBS(2,2)*COS(P1-P2) ! 2,2 of the HV 2nd
                        ELSE IF ( POL_CONF == PIMA__PC_LL .AND. POL_MODE == PIMA__XXLL ) THEN
!
! ------------------------ Lin-lin polarization, XX-pol 
!
                           UV(J17,I_FRQ,J1,1) =   CAL_OBS(1,1)*COS(P1-P2)         &
     &                                          + CAL_OBS(2,1)*SIN(P1-P2)/        &
     &                                                CMPLX ( COS(BPAS_PHS(1,2)), &
     &                                                        SIN(BPAS_PHS(1,2))  )
                        ELSE IF ( POL_CONF == PIMA__PC_LL .AND. POL_MODE == PIMA__YXLL ) THEN
!
! ------------------------ Lin-lin polarization, YX-pol 
!
                           UV(J17,I_FRQ,J1,1) = - CAL_OBS(1,1)*SIN(P1-P2)/           &
     &                                                CMPLX ( COS(-BPAS_PHS(2,2)),   &
     &                                                        SIN(-BPAS_PHS(2,2))  ) &
     &                                          + CAL_OBS(2,1)*COS(P1-P2)/           &
     &                                                CMPLX ( COS(BPAS_PHS(1,2)),    &
     &                                                        SIN(BPAS_PHS(1,2))  )/ &
     &                                                CMPLX ( COS(-BPAS_PHS(2,2)),   &
     &                                                        SIN(-BPAS_PHS(2,2)) )
                        ELSE IF ( POL_CONF == PIMA__PC_LL .AND. POL_MODE == PIMA__XYLL ) THEN
!
! ------------------------ Lin-lin polarization, XY-pol
!
                           UV(J17,I_FRQ,J1,1) =   CAL_OBS(1,2)*COS(P1-P2)         &
     &                                          + CAL_OBS(2,2)*SIN(P1-P2)/        &
     &                                                CMPLX ( COS(BPAS_PHS(1,2)), &
     &                                                        SIN(BPAS_PHS(1,2))  )
                        ELSE IF ( POL_CONF == PIMA__PC_LL .AND. POL_MODE == PIMA__YYLL ) THEN
!
! ------------------------ Lin-lin polarization, YY-pol
!
                           UV(J17,I_FRQ,J1,1) = - CAL_OBS(1,2)*SIN(P1-P2)/           &
     &                                                CMPLX ( COS(-BPAS_PHS(2,2)),   &
     &                                                        SIN(-BPAS_PHS(2,2)) )  &
     &                                          + CAL_OBS(2,2)*COS(P1-P2)/           &
     &                                                CMPLX ( COS(BPAS_PHS(1,2)),    &
     &                                                        SIN(BPAS_PHS(1,2))  )/ &
     &                                                CMPLX ( COS(-BPAS_PHS(2,2)),   &
     &                                                        SIN(-BPAS_PHS(2,2))  )
                        ELSE IF ( POL_CONF == PIMA__PC_LL .AND.  POL_MODE == PIMA__PALL_NOR ) THEN
!
! ------------------------ No bandpass, no parallactic angle rotation
!
                           UV(J17,I_FRQ,J1,1) =   CAL_OBS(1,1)
                           UV(J17,I_FRQ,J1,2) =   CAL_OBS(2,1)
                           UV(J17,I_FRQ,J1,3) =   CAL_OBS(1,2)
                           UV(J17,I_FRQ,J1,4) =   CAL_OBS(2,2)
                        ELSE IF ( POL_CONF == PIMA__PC_LL .AND. POL_MODE == PIMA__PALL_XY ) THEN
!
! ------------------------ Lin-lin polarization, all polarization combinations
!
                           UV(J17,I_FRQ,J1,1) =   CAL_OBS(1,1)*COS(P1-P2)            &
     &                                          + CAL_OBS(2,1)*SIN(P1-P2)/           &
     &                                                CMPLX ( COS(BPAS_PHS(1,2)),    &
     &                                                        SIN(BPAS_PHS(1,2))  )
                           UV(J17,I_FRQ,J1,2) = - CAL_OBS(1,1)*SIN(P1-P2)/ &
     &                                                CMPLX ( COS(-BPAS_PHS(2,2)),   &
     &                                                        SIN(-BPAS_PHS(2,2))  ) &
     &                                          + CAL_OBS(2,1)*COS(P1-P2)/           &
     &                                                CMPLX ( COS(BPAS_PHS(1,2)),    &
     &                                                        SIN(BPAS_PHS(1,2))  )/ &
     &                                                CMPLX ( COS(-BPAS_PHS(2,2)),   &
     &                                                        SIN(-BPAS_PHS(2,2)) )
                           UV(J17,I_FRQ,J1,3) =   CAL_OBS(1,2)*COS(P1-P2)            &
     &                                          + CAL_OBS(2,2)*SIN(P1-P2)/           &
     &                                                CMPLX ( COS(BPAS_PHS(1,2)),    &
     &                                                        SIN(BPAS_PHS(1,2)) )
                           UV(J17,I_FRQ,J1,4) = - CAL_OBS(1,2)*SIN(P1-P2)/           &
     &                                                CMPLX ( COS(-BPAS_PHS(2,2)),   &
     &                                                        SIN(-BPAS_PHS(2,2)) )  &
     &                                          + CAL_OBS(2,2)*COS(P1-P2)/           &
     &                                                CMPLX ( COS(BPAS_PHS(1,2)),    &
     &                                                        SIN(BPAS_PHS(1,2)) )/  &
     &                                                CMPLX ( COS(-BPAS_PHS(2,2)),   &
     &                                                        SIN(-BPAS_PHS(2,2))  )
                        ELSE IF ( POL_CONF == PIMA__PC_LL .AND. POL_MODE == PIMA__PAR ) THEN
!
! ------------------------ Lin-lin polarization, XX-pol and YY-pol
!
                           UV(J17,I_FRQ,J1,1) =   CAL_OBS(1,1)*COS(P1-P2)           &
     &                                          + CAL_OBS(2,1)*SIN(P1-P2)/          &
     &                                                CMPLX ( COS(BPAS_PHS(1,2)),   &
     &                                                        SIN(BPAS_PHS(1,2))  )
                           UV(J17,I_FRQ,J1,2) = - CAL_OBS(1,2)*SIN(P1-P2)/          &
     &                                                CMPLX ( COS(-BPAS_PHS(2,2)),  &
     &                                                        SIN(-BPAS_PHS(2,2)) ) &
     &                                          + CAL_OBS(2,2)*COS(P1-P2)/          &
     &                                                CMPLX ( COS(BPAS_PHS(1,2)),   &
     &                                                        SIN(BPAS_PHS(1,2)) )/ &
     &                                                CMPLX ( COS(-BPAS_PHS(2,2)),  &
     &                                                        SIN(-BPAS_PHS(2,2))  )
                        ELSE IF ( POL_CONF == PIMA__PC_LL .AND. POL_MODE == PIMA__IPLL ) THEN
!
! ------------------------ Lin-lin polarization, I-pol
!
                           UV(J17,I_FRQ,J1,1) = (                                   &
     &                                            CAL_OBS(1,1)*COS(P1-P2)           &
     &                                          + CAL_OBS(2,1)*SIN(P1-P2)/          &
     &                                                CMPLX ( COS(BPAS_PHS(1,2)),   &
     &                                                        SIN(BPAS_PHS(1,2)) )  &
     &                                          - CAL_OBS(1,2)*SIN(P1-P2)/          &
     &                                                CMPLX ( COS(-BPAS_PHS(2,2)),  &
     &                                                        SIN(-BPAS_PHS(2,2)) ) &
     &                                          + CAL_OBS(2,2)*COS(P1-P2)/          &
     &                                                CMPLX ( COS(BPAS_PHS(1,2)),   &
     &                                                        SIN(BPAS_PHS(1,2)) )/ &
     &                                                CMPLX ( COS(-BPAS_PHS(2,2)),  &
     &                                                        SIN(-BPAS_PHS(2,2)) ) &
     &                                          )
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!%                           uv(j17,i_frq,j1,1) =   cal_obs(1,1)*cos(p1-p2)           &
!%     &                                          + cal_obs(2,1)*sin(p1-p2)/          &
!%     &                                                cmplx ( cos(bpas_phs(1,2)),   &
!%     &                                                        sin(bpas_phs(1,2))  ) &
!%     &                     - ( - cal_obs(1,2)*sin(p1-p2)/          &
!%     &                                                cmplx ( cos(-bpas_phs(2,2)),  &
!%     &                                                        sin(-bpas_phs(2,2)) ) &
!%     &                                          + cal_obs(2,2)*cos(p1-p2)/          &
!%     &                                                cmplx ( cos(bpas_phs(1,2)),   &
!%     &                                                        sin(bpas_phs(1,2)) )/ &
!%     &                                                cmplx ( cos(-bpas_phs(2,2)),  &
!%     &                                                        sin(-bpas_phs(2,2))  ) )
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
                        ELSE IF ( POL_CONF == PIMA__PC_LL .AND. POL_MODE == PIMA__RRLL ) THEN
!
! ------------------------ Lin-lin polarization, R-pol
!
                           UV(J17,I_FRQ,J1,1) = (             CAL_OBS(1,1)*COS(P1-P2)            &
     &                                            +           CAL_OBS(2,1)*SIN(P1-P2)/           &
     &                                                            CMPLX ( COS(BPAS_PHS(1,2)),    &
     &                                                                    SIN(BPAS_PHS(1,2)) )   &
     &                                            - I_CMPLX * CAL_OBS(1,1)*SIN(P1-P2)/           &
     &                                                            CMPLX ( COS(-BPAS_PHS(2,2)),   &
     &                                                                    SIN(-BPAS_PHS(2,2)) )  &
     &                                            + I_CMPLX * CAL_OBS(2,1)*COS(P1-P2)/           &
     &                                                            CMPLX ( COS(BPAS_PHS(1,2)),    &
     &                                                                    SIN(BPAS_PHS(1,2))  )/ &
     &                                                            CMPLX ( COS(-BPAS_PHS(2,2)),   &
     &                                                                    SIN(-BPAS_PHS(2,2)) )  &
     &                                          )/ SQRT(2.0)
                        ELSE IF ( POL_CONF == PIMA__PC_LL .AND. POL_MODE == PIMA__LLLL ) THEN
!
! ------------------------ Lin-lin polarization, L-pol
!
                           UV(J17,I_FRQ,J1,3) = (             CAL_OBS(1,2)*COS(P1-P2)            &
     &                                            +           CAL_OBS(2,2)*SIN(P1-P2)/           &
     &                                                            CMPLX ( COS(BPAS_PHS(1,2)),    &
     &                                                                    SIN(BPAS_PHS(1,2)) )   &
     &                                            + I_CMPLX * CAL_OBS(1,2)*SIN(P1-P2)/           &
     &                                                            CMPLX ( COS(-BPAS_PHS(2,2)),   &
     &                                                                    SIN(-BPAS_PHS(2,2)) )  &
     &                                            - I_CMPLX * CAL_OBS(2,2)*COS(P1-P2)/           &
     &                                                            CMPLX ( COS(BPAS_PHS(1,2)),    &
     &                                                                    SIN(BPAS_PHS(1,2))  )/ &
     &                                                            CMPLX ( COS(-BPAS_PHS(2,2)),   &
     &                                                                    SIN(-BPAS_PHS(2,2)) )  &
     &                                          )/ SQRT(2.0)
!
! ===================== Lin-cir polarization case
!
                        ELSE IF ( POL_CONF == PIMA__PC_LC .AND. POL_MODE == PIMA__PALL_NOR ) THEN
!
! ------------------------ Lin-lin polarization, no rotation, no bandpass
!
                           UV(J17,I_FRQ,J1,1) = CAL_OBS(1,1)
                           UV(J17,I_FRQ,J1,2) = CAL_OBS(2,1)
                           UV(J17,I_FRQ,J1,3) = CAL_OBS(1,2)
                           UV(J17,I_FRQ,J1,4) = CAL_OBS(2,2)
                        ELSE IF ( POL_CONF == PIMA__PC_LC .AND. POL_MODE == PIMA__HRLC ) THEN
                           UV(J17,I_FRQ,J1,1) = CAL_OBS(1,1)
                        ELSE IF ( POL_CONF == PIMA__PC_LC .AND. POL_MODE == PIMA__VRLC ) THEN
                           UV(J17,I_FRQ,J1,1) = CAL_OBS(2,1)
                        ELSE IF ( POL_CONF == PIMA__PC_LC .AND. POL_MODE == PIMA__HLLC ) THEN
                           UV(J17,I_FRQ,J1,1) = CAL_OBS(1,2)
                        ELSE IF ( POL_CONF == PIMA__PC_LC .AND. POL_MODE == PIMA__VLLC ) THEN
                           UV(J17,I_FRQ,J1,1) = CAL_OBS(2,2) 
                        ELSE IF ( POL_CONF == PIMA__PC_LC .AND. POL_MODE == PIMA__PALL_XY ) THEN
!
! ------------------------ Lin-cir polarization,  all polarizations
!
                           UV(J17,I_FRQ,J1,1) = (            CAL_OBS(1,1) * CMPLX ( COS(-(P1-P2)), SIN(-(P1-P2)) )           &
                                                 + I_CMPLX * CAL_OBS(2,1) * CMPLX ( COS(-(P1-P2)), SIN(-(P1-P2)) ) /         &
     &                                                                      CMPLX ( COS(BPAS_PHS(1,2)), SIN(BPAS_PHS(1,2)) ) &
     &                                          ) / SQRT(2.0) 
                           UV(J17,I_FRQ,J1,2) = (            CAL_OBS(1,1) * CMPLX ( COS((P1-P2)), SIN((P1-P2)) ) /               &
     &                                                                      CMPLX ( COS(-BPAS_PHS(2,2)), SIN(-BPAS_PHS(2,2)) )   &
     &                                            - I_CMPLX* CAL_OBS(2,1) * CMPLX ( COS( (P1-P2)), SIN( (P1-P2)) ) /             &
     &                                                                      CMPLX ( COS( BPAS_PHS(1,2)), SIN( BPAS_PHS(1,2)) ) / &
     &                                                                      CMPLX ( COS(-BPAS_PHS(2,2)), SIN(-BPAS_PHS(2,2)) )   &
     &                                          ) / SQRT(2.0) 
                           UV(J17,I_FRQ,J1,3) = (            CAL_OBS(1,2) * CMPLX ( COS(-(P1-P2)), SIN(-(P1-P2)) )           &
                                                 + I_CMPLX * CAL_OBS(2,2) * CMPLX ( COS(-(P1-P2)), SIN(-(P1-P2)) ) /         &
     &                                                                      CMPLX ( COS(BPAS_PHS(1,2)), SIN(BPAS_PHS(1,2)) ) &
     &                                          ) / SQRT(2.0) 
                           UV(J17,I_FRQ,J1,4) = (            CAL_OBS(1,2) * CMPLX ( COS((P1-P2)), SIN((P1-P2)) ) / &
     &                                                                      CMPLX ( COS(-BPAS_PHS(2,2)), SIN(-BPAS_PHS(2,2)) )   &
     &                                            - I_CMPLX* CAL_OBS(2,2) * CMPLX ( COS( (P1-P2)), SIN( (P1-P2)) ) /             &
     &                                                                      CMPLX ( COS( BPAS_PHS(1,2)), SIN( BPAS_PHS(1,2)) ) / &
     &                                                                      CMPLX ( COS(-BPAS_PHS(2,2)), SIN(-BPAS_PHS(2,2)) )   &
     &                                          ) / SQRT(2.0) 
                        ELSE IF ( POL_CONF == PIMA__PC_LC .AND. POL_MODE == PIMA__XXLC ) THEN
!
! ------------------------ Lin-cir polarization, XX-pol
!
                           UV(J17,I_FRQ,J1,1) = (            CAL_OBS(1,1) * CMPLX ( COS(-(P1-P2)), SIN(-(P1-P2)) ) &
                                                 + I_CMPLX * CAL_OBS(2,1) * CMPLX ( COS(-(P1-P2)), SIN(-(P1-P2)) ) / &
     &                                                                      CMPLX ( COS(BPAS_PHS(1,2)), SIN(BPAS_PHS(1,2)) ) &
     &                                          ) / SQRT(2.0) 
                        ELSE IF ( POL_CONF == PIMA__PC_LC .AND. POL_MODE == PIMA__YXLC ) THEN
!
! ------------------------ Lin-cir polarization, YX-pol
!
                           UV(J17,I_FRQ,J1,1) = (            CAL_OBS(1,1) * CMPLX ( COS((P1-P2)), SIN((P1-P2)) ) /  &
     &                                                                      CMPLX ( COS(-BPAS_PHS(2,2)), SIN(-BPAS_PHS(2,2)) )  &
     &                                            - I_CMPLX* CAL_OBS(2,1) * CMPLX ( COS( (P1-P2)), SIN( (P1-P2)) ) / &
     &                                                                      CMPLX ( COS( BPAS_PHS(1,2)), SIN( BPAS_PHS(1,2)) ) / &
     &                                                                      CMPLX ( COS(-BPAS_PHS(2,2)), SIN(-BPAS_PHS(2,2)) ) &
     &                                          ) / SQRT(2.0) 
                        ELSE IF ( POL_CONF == PIMA__PC_LC .AND. POL_MODE == PIMA__XYLC ) THEN
!
! ------------------------ Lin-cir polarization, XY-pol
!
                           UV(J17,I_FRQ,J1,1) = (            CAL_OBS(1,2) * CMPLX ( COS(-(P1-P2)), SIN(-(P1-P2)) ) &
                                                 + I_CMPLX * CAL_OBS(2,2) * CMPLX ( COS(-(P1-P2)), SIN(-(P1-P2)) ) / &
     &                                                                      CMPLX ( COS(BPAS_PHS(1,2)), SIN(BPAS_PHS(1,2)) ) &
     &                                          ) / SQRT(2.0) 
                        ELSE IF ( POL_CONF == PIMA__PC_LC .AND. POL_MODE == PIMA__YYLC ) THEN
!
! ------------------------ Lin-cir polarization, YY-pol
!
                           UV(J17,I_FRQ,J1,1) = (            CAL_OBS(1,2) * CMPLX ( COS((P1-P2)), SIN((P1-P2)) ) / &
     &                                                                      CMPLX ( COS(-BPAS_PHS(2,2)), SIN(-BPAS_PHS(2,2)) ) &
     &                                            - I_CMPLX* CAL_OBS(2,2) * CMPLX ( COS( (P1-P2)), SIN( (P1-P2)) ) / &
     &                                                                      CMPLX ( COS( BPAS_PHS(1,2)), SIN( BPAS_PHS(1,2)) ) / &
     &                                                                      CMPLX ( COS(-BPAS_PHS(2,2)), SIN(-BPAS_PHS(2,2)) ) &
     &                                          ) / SQRT(2.0) 
                        ELSE IF ( POL_CONF == PIMA__PC_LC .AND. POL_MODE == PIMA__IPLC ) THEN
                           UV(J17,I_FRQ,J1,1) = (            CAL_OBS(1,1) * CMPLX ( COS(-(P1-P2)), SIN(-(P1-P2)) ) &
                                                 + I_CMPLX * CAL_OBS(2,1) * CMPLX ( COS(-(P1-P2)), SIN(-(P1-P2)) ) / &
     &                                                                      CMPLX ( COS(BPAS_PHS(1,2)), SIN(BPAS_PHS(1,2)) ) &
     &                                           +           CAL_OBS(1,2) * CMPLX ( COS((P1-P2)), SIN((P1-P2)) ) / &
     &                                                                      CMPLX ( COS(-BPAS_PHS(2,2)), SIN(-BPAS_PHS(2,2))  ) &
     &                                           - I_CMPLX*  CAL_OBS(2,2) * CMPLX ( COS( (P1-P2)), SIN( (P1-P2)) ) / &
     &                                                                      CMPLX ( COS( BPAS_PHS(1,2)), SIN( BPAS_PHS(1,2)) ) / &
     &                                                                      CMPLX ( COS(-BPAS_PHS(2,2)), SIN(-BPAS_PHS(2,2)) ) &
     &                                          ) / (2.0 * SQRT(2.0) )
!
! ===================== cir-lin polarization case
!
                        ELSE IF ( POL_CONF == PIMA__PC_CL .AND. POL_MODE == PIMA__PALL_NOR ) THEN
!
! ------------------------ Cir-lin polarization, no rotation, no bandpass
!
                           UV(J17,I_FRQ,J1,1) = CAL_OBS(1,1)
                           UV(J17,I_FRQ,J1,2) = CAL_OBS(2,1)
                           UV(J17,I_FRQ,J1,3) = CAL_OBS(1,2)
                           UV(J17,I_FRQ,J1,4) = CAL_OBS(2,2)
                        ELSE IF ( POL_CONF == PIMA__PC_CL .AND. POL_MODE == PIMA__RHCL ) THEN
                           UV(J17,I_FRQ,J1,1) = CAL_OBS(1,1)
                        ELSE IF ( POL_CONF == PIMA__PC_CL .AND. POL_MODE == PIMA__RVCL ) THEN
                           UV(J17,I_FRQ,J1,1) = CAL_OBS(1,2)
                        ELSE IF ( POL_CONF == PIMA__PC_CL .AND. POL_MODE == PIMA__LHCL ) THEN
                           UV(J17,I_FRQ,J1,1) = CAL_OBS(2,2) 
                        ELSE IF ( POL_CONF == PIMA__PC_CL .AND. POL_MODE == PIMA__LVCL ) THEN
                           UV(J17,I_FRQ,J1,1) = CAL_OBS(2,2) 
                        ELSE IF ( POL_CONF == PIMA__PC_CL .AND. POL_MODE == PIMA__XXCL ) THEN
                           UV(J17,I_FRQ,J1,1) = (  CAL_OBS(1,1) * &
     &                                               CMPLX ( COS(-(P1-P2)), SIN(-(P1-P2)) )        &
     &                                           + CAL_OBS(2,1) / &
     &                                               CMPLX ( COS(BPAS_PHS(1,2)), SIN(BPAS_PHS(1,2)) ) * &
     &                                               CMPLX ( COS(P1-P2), SIN(P1-P2) )                   &
     &                                          )/ SQRT(2.0D0)
                        ELSE IF ( POL_CONF == PIMA__PC_CL .AND. POL_MODE == PIMA__YXCL ) THEN
                           UV(J17,I_FRQ,J1,1) = (- I_CMPLX * CAL_OBS(1,1) * &
     &                                               CMPLX ( COS(-(P1-P2)), SIN(-(P1-P2)) )        / &
     &                                               CMPLX ( COS(-BPAS_PHS(2,2)), SIN(-BPAS_PHS(2,2)) )  &
     &                                           + I_CMPLX * CAL_OBS(2,1) * &
     &                                               CMPLX ( COS(P1-P2), SIN(P1-P2) ) /            &
     &                                               CMPLX ( COS(BPAS_PHS(1,2)-BPAS_PHS(2,2)),     &
     &                                                       SIN(BPAS_PHS(1,2)-BPAS_PHS(2,2)) )    &
     &                                          )/ SQRT(2.0D0)
                        ELSE IF ( POL_CONF == PIMA__PC_CL .AND. POL_MODE == PIMA__XYCL ) THEN
                           UV(J17,I_FRQ,J1,1) = (  CAL_OBS(1,2) / &
     &                                               CMPLX ( COS(-(P1-P2)), SIN(-(P1-P2)) )        &
     &                                           + CAL_OBS(2,2) / &
     &                                               CMPLX ( COS(BPAS_PHS(1,2)), SIN(BPAS_PHS(1,2)) ) * &
     &                                               CMPLX ( COS(P1-P2), SIN(P1-P2) )                   &
     &                                          )/ SQRT(2.0D0)
                        ELSE IF ( POL_CONF == PIMA__PC_CL .AND. POL_MODE == PIMA__YYCL ) THEN
                           UV(J17,I_FRQ,J1,1) = (- I_CMPLX * CAL_OBS(1,2) * &
     &                                               CMPLX ( COS(-(P1-P2)), SIN(-(P1-P2)) )     /   &
     &                                               CMPLX ( COS(-BPAS_PHS(2,2)), SIN(-BPAS_PHS(2,2)) )  &
     &                                           + I_CMPLX * CAL_OBS(2,2) * &
     &                                               CMPLX ( COS(P1-P2), SIN(P1-P2) ) /         &
     &                                               CMPLX ( COS(BPAS_PHS(1,2)-BPAS_PHS(2,2)),  &
     &                                                       SIN(BPAS_PHS(1,2)-BPAS_PHS(2,2)) ) &
     &                                          )/ SQRT(2.0D0)
                        ELSE IF ( POL_CONF == PIMA__PC_CL .AND. POL_MODE == PIMA__PALL_XY ) THEN
                           UV(J17,I_FRQ,J1,1) = (  CAL_OBS(1,1) * &
     &                                                 CMPLX ( COS(-(P1-P2)), SIN(-(P1-P2)) )           &
     &                                           + CAL_OBS(2,1) *                                       &
     &                                                 CMPLX ( COS(P1-P2), SIN(P1-P2) ) /               &
     &                                                 CMPLX ( COS(BPAS_PHS(1,2)), SIN(BPAS_PHS(1,2)) ) &
     &                                          )/ SQRT(2.0)
!
                           UV(J17,I_FRQ,J1,2) = (- I_CMPLX * CAL_OBS(1,1) * &
     &                                               CMPLX ( COS(-(P1-P2)), SIN(-(P1-P2)) ) /           &
     &                                               CMPLX ( COS(-BPAS_PHS(2,2)), SIN(-BPAS_PHS(2,2)) ) &
     &                                           + I_CMPLX * CAL_OBS(2,1) * &
     &                                               CMPLX ( COS(P1-P2), SIN(P1-P2) ) /                  &
     &                                               CMPLX ( COS( BPAS_PHS(1,2)), SIN( BPAS_PHS(1,2)) )/ &
     &                                               CMPLX ( COS(-BPAS_PHS(2,2)), SIN(-BPAS_PHS(2,2)) )  &
     &                                          )/ SQRT(2.0)
                          UV(J17,I_FRQ,J1,3) = (   CAL_OBS(1,2) * &
     &                                                 CMPLX ( COS(-(P1-P2)), SIN(-(P1-P2)) )           &
     &                                           + CAL_OBS(2,2) *                                       &
     &                                                 CMPLX ( COS(P1-P2), SIN(P1-P2) ) /               &
     &                                                 CMPLX ( COS(BPAS_PHS(1,2)), SIN(BPAS_PHS(1,2)) ) &
     &                                          )/ SQRT(2.0)
                           UV(J17,I_FRQ,J1,4) = (- I_CMPLX * CAL_OBS(1,2) * &
     &                                               CMPLX ( COS(-(P1-P2)), SIN(-(P1-P2)) ) /           &
     &                                               CMPLX ( COS(-BPAS_PHS(2,2)), SIN(-BPAS_PHS(2,2)) ) &
     &                                           + I_CMPLX * CAL_OBS(2,2) * &
     &                                               CMPLX ( COS(P1-P2), SIN(P1-P2) ) /                  &
     &                                               CMPLX ( COS( BPAS_PHS(1,2)), SIN( BPAS_PHS(1,2)) )/ &
     &                                               CMPLX ( COS(-BPAS_PHS(2,2)), SIN(-BPAS_PHS(2,2)) )  &
     &                                          )/ SQRT(2.0)
                        ELSE IF ( POL_CONF == PIMA__PC_CL .AND. POL_MODE == PIMA__IPCL ) THEN
                           UV(J17,I_FRQ,J1,1) = (  CAL_OBS(1,1) * &
     &                                                 CMPLX ( COS(-(P1-P2)), SIN(-(P1-P2)) )           &
     &                                           + CAL_OBS(2,1) *                                       &
     &                                                 CMPLX ( COS(P1-P2), SIN(P1-P2) ) /               &
     &                                                 CMPLX ( COS(BPAS_PHS(1,2)), SIN(BPAS_PHS(1,2)) ) &
     &                                           - I_CMPLX * CAL_OBS(1,2) * &
     &                                               CMPLX ( COS(-(P1-P2)), SIN(-(P1-P2)) ) /           &
     &                                               CMPLX ( COS(-BPAS_PHS(2,2)), SIN(-BPAS_PHS(2,2)) ) &
     &                                           + I_CMPLX * CAL_OBS(2,2) * &
     &                                               CMPLX ( COS(P1-P2), SIN(P1-P2) ) /                  &
     &                                               CMPLX ( COS( BPAS_PHS(1,2)), SIN( BPAS_PHS(1,2)) )/ &
     &                                               CMPLX ( COS(-BPAS_PHS(2,2)), SIN(-BPAS_PHS(2,2)) )  &
     &                                          )/ SQRT(2.0)
                        ELSE IF ( POL_CONF == PIMA__PC_CL .AND. POL_MODE == PIMA__IPCL ) THEN
                           CONTINUE 
                    END IF
 4170            CONTINUE
            END IF
 4150    CONTINUE
!$OMP    END PARALLEL DO
         IF ( PIM%CONF%DEBUG_LEVEL == 8 ) THEN
              CALL WALL_TIMER ( STR )
              READ ( UNIT=STR(12:27), FMT='(F16.10)' ) WALL_TIM_VAL
              WALL_TIM_PROC = WALL_TIM_PROC + WALL_TIM_VAL
              CALL WALL_TIMER ( %VAL(0) )
         END IF
 410  CONTINUE
      IF ( FL_PLD ) THEN
           DEALLOCATE ( PCAL_C8 )
      END IF
!
      IF ( PIM%OBS(IND_OBS)%REF_FRG_INDS(PIM%CONF%FRQ_GRP) > 1 ) THEN
           PIM%OBS(IND_OBS)%AP_LEN = AP_LEN
      END IF
      IF ( ALLOCATED ( UV_DATA ) ) DEALLOCATE ( UV_DATA )
      IF ( ALLOCATED ( WEI_ARR ) ) DEALLOCATE ( WEI_ARR )
      IF ( PIM%CONF%DEBUG_LEVEL == 8 ) THEN
           WRITE ( 6, 240 ) WALL_TIM_READ_UV, WALL_TIM_PROC, N_UV_BYTES/1048576.0
 240       FORMAT ( 'PIMA_GET_UV UV: Wall time Read: ', F10.6, ' Proc: ', F10.6, &
     &               1X, F8.3, ' Mb' )
           CALL WALL_TIMER ( %VAL(0) )
      END IF
!
      DO 4180 J18=1,L_POL
         IFRQ = 0
         DO 4190 J19=PIM%CONF%BEG_FRQ,PIM%CONF%END_FRQ
            IFRQ = IFRQ + 1
            DO 4200 J20=1,PIM%NCHN
!
! ------------ Compute frequencies
!
               IF ( J18 == 1 .AND. IFRQ == 1 .AND.  J20 == 1 ) THEN
                    FREQ_REF = PIM%FRQ(PIM%CONF%BEG_FRQ,PIM%CONF%FRQ_GRP)%FREQ
               END IF
               DO 4210 J21=1,LTIM
!
! --------------- 1(7) Correct insane data
!
                  IF ( ABS( REAL ( UV(J20,IFRQ,J21,J18) ) ) >  VIS_MAX .OR. &
     &                 ABS( IMAG ( UV(J20,IFRQ,J21,J18) ) ) >  VIS_MAX      ) THEN
                       UV(J20,IFRQ,J20,J18) = CMPLX ( 0.0, 0.0 )
                  END IF
!
                  IF ( J18 == 1 .AND. IFRQ == 1 .AND.  J20 == 1 ) THEN
                       IF ( PIM%CONF%CORR_FLAG_MIN .GE. -2 ) THEN
!
! ------------------------- 2(7) Apply calibration for the correlator flag
!
                            IF ( PIM%OBS(IND_OBS)%CORR_FLAG(J21,FRG_IND) .LE. &
     &                           PIM%CONF%CORR_FLAG_MIN ) THEN
                                 WEI_1D(J21,1:PIM%NSTK) = 0.0
                            END IF
                       END IF
!
                       IF ( ILEN(PIM%CONF%TIME_FLAG_FILE) > 0         .AND. & 
     &                      ASSOCIATED ( PIM%OBS(IND_OBS)%USER_FLAG ) .AND. &
     &                      IFRQ == 1                                 .AND. &
     &                      J20 == 1                                         ) THEN
!
! ------------------------- 3(7) Apply calibration for user time flag
!
                            WEI_1D(J21,1:PIM%NSTK) = WEI_1D(J21,1:PIM%NSTK)*PIM%OBS(IND_OBS)%USER_FLAG(J21)
                       END IF
                  END IF
                  IF ( PIM%BANDPASS_MASK_STYLE .NE. PIMA__NO ) THEN
!
! -------------------- 4(7) Apply fine bandpass mask if available
!
                       UV(J20,IFRQ,J21,J18) = UV(J20,IFRQ,J21,J18)* &
     &                       PIM%BANDPASS_MASK(J20,J19,PIM%OBS(IND_OBS)%STA_IND(1),PIMA__MASK_FRNG) * &
     &                       PIM%BANDPASS_MASK(J20,J19,PIM%OBS(IND_OBS)%STA_IND(2),PIMA__MASK_FRNG)
                  END IF
!
                  IF ( PIM%CONF%SAMPLER_CAL_CODE == PIMA__SMPL_USE ) THEN
!
! -------------------- 5(7) Correction for digitization in the sampler
!
                       IF ( PIM%NLEV(PIM%OBS(IND_OBS)%STA_IND(1)) == 4 .AND. &
     &                      PIM%NLEV(PIM%OBS(IND_OBS)%STA_IND(2)) == 4      ) THEN
                            UV(J20,IFRQ,J21,J18) = UV(J20,IFRQ,J21,J18)/PIMA__DIG_22
                          ELSE IF ( PIM%NLEV(PIM%OBS(IND_OBS)%STA_IND(1)) == 4 .AND. &
     &                              PIM%NLEV(PIM%OBS(IND_OBS)%STA_IND(2)) == 2       ) THEN
                            UV(J20,IFRQ,J21,J18) = UV(J20,IFRQ,J21,J18)/PIMA__DIG_12
                          ELSE IF ( PIM%NLEV(PIM%OBS(IND_OBS)%STA_IND(1)) == 2 .AND. &
     &                              PIM%NLEV(PIM%OBS(IND_OBS)%STA_IND(2)) == 4       ) THEN
                            UV(J20,IFRQ,J21,J18) = UV(J20,IFRQ,J21,J18)/PIMA__DIG_12
                          ELSE
                            UV(J20,IFRQ,J21,J18) = UV(J20,IFRQ,J21,J18)/PIMA__DIG_11
                       END IF
                  END IF
!
                  IF ( PIM%CONF%FRIB_AMPL_FUDGE_TYPE == PIMA__FUDGE_VLBA ) THEN
!
! -------------------- 6(7) Amplitude correction for register saturation
! ----------------------    specific for the VLBA **hardware** correlator
!
                       IF ( PIM%NPOL == 1 ) THEN
                            UV(J20,IFRQ,J21,J18) = UV(J20,IFRQ,J21,J18)/(1.0D0 + WEI_1D(J21,1)/8.0D0)
                          ELSE IF ( PIM%NPOL .GE. 2 ) THEN
                            UV(J20,IFRQ,J21,J18) = UV(J20,IFRQ,J21,J18)/(1.0D0 + WEI_1D(J21,1)/4.0D0)
                       END IF
                    ELSE IF ( PIM%CONF%FRIB_AMPL_FUDGE_TYPE == PIMA__FUDGE_KOGAN ) THEN
!
! -------------------- 7(7) Amplitude correction for register saturation
! ----------------------    specific for the VLBA hardware correlator using
! ----------------------    original Kogan formula, considering weights are equal to 1.0
!
                       IF ( PIM%NPOL == 1 ) THEN
                            UV(J20,IFRQ,J21,J18) = UV(J20,IFRQ,J21,J18)/(1.0D0 + 1.0D0/8.0D0)
                          ELSE IF ( PIM%NPOL .GE. 2 ) THEN
                            UV(J20,IFRQ,J21,J18) = UV(J20,IFRQ,J21,J18)/(1.0D0 + 1.0D0/4.0D0)
                       END IF
                     ELSE IF ( PIM%CONF%FRIB_AMPL_FUDGE_TYPE == PIMA__FUDGE_DIFX ) THEN
                       CONTINUE 
                  END IF
!
                  IF ( PIM%CONF%DEBUG_LEVEL == 19 ) THEN
                       IF ( J18 == 1 .AND. IFRQ == 1 .AND. J20 == 1 ) THEN
                            WRITE ( 6, 260 ) IND_OBS, J21, PIM%OBS(IND_OBS)%CORR_FLAG(J21,FRG_IND)
 260                        FORMAT ( 'PIMA_GET_UV  Ind_obs: ', I6, ' AP_ind: ', I4, &
     &                               ' Corr_flag: ', I12 )
                       END IF
                  END IF
                  IF ( PIM%CONF%DEBUG_LEVEL == 14 .AND. &
     &                 J19 == PIM%CONF%BEG_FRQ    .AND. &
     &                 J20 == 4                         ) THEN
                       STR = MJDSEC_TO_DATE ( PIM%MJD_0, PIM%TAI_0 + &
     &                                 PIM%TIM_R8(PIM%UV_IND(PIM%OBS(IND_OBS)%UV_IND(J21,FRG_IND))%TIM_IND), &
     &                                 IER )
                       WRITE  ( 6, 270 ) IND_OBS, J19, J20, J21, STR(1:I_LEN(STR)), &
     &                                   J18, WEI_1D(J21,J18), UV(J20,IFRQ,J21,J18)
  270                  FORMAT ( 'PIMA_GET_UV Obs: ', I5, ' I_FRQ= ', I2, ' ICHN= ', I4, &
     &                          ' Ind_tim= ', I4, ' Date: ', A24, ' Poli= ', I1, ' Wei: ', F8.5, &
     &                          ' UV= ( ', 1PE11.4, ' , ', 1PE11.4 , ')' )
                  END IF
 4210          CONTINUE
 4200       CONTINUE
 4190    CONTINUE
 4180 CONTINUE
      IF ( ALLOCATED ( UV_DATA ) ) DEALLOCATE ( UV_DATA )
      IF ( ALLOCATED ( WEI_ARR ) ) DEALLOCATE ( WEI_ARR )
      IF ( ALLOCATED ( PCAL_C8 ) ) DEALLOCATE ( PCAL_C8 )
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  PIMA_GET_UV  !#!#
