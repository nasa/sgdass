      SUBROUTINE PIMA_PBP_FINE ( PIM, VTD, BPS, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine PIMA_PBP_FINE 
! *                                                                      *
! *  ### 29-AUG-2010  PIMA_PBP_FINE  v5.0 (c) L. Petrov  17-NOV-2020 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'astro_constants.i'
      INCLUDE   'pima.i'
      INCLUDE   'vtd.i'
      TYPE     ( PIMA__TYPE         ) :: PIM
      TYPE     ( PIM_BPS_STA__TYPE  ) :: BPS
      TYPE     ( VTD__TYPE          ) :: VTD
      TYPE     ( PIM_PBP_STA__TYPE  ) :: PBP_SAVE
      INTEGER*4  IUER
      INTEGER*4  LCHN, LFRQ, KCHN, ICHN, JCHN, J1, J2, J3, J4, J5, J6, J7, J8, &
     &           J9, J10, J11, J12, J13, J14, J15, J16, J17, J18, J19, J20, &
     &           J21, J22, J23, J24, J25, J26, J27, J28, J29, J30, J31, &
     &           J32, J33, J34, J35, J36, J37, J38, J39, J40, J41, J42, &
     &           ISTA, IND_STA(2), KP, L_EQU, L_PAR, LL_PAR, L_OBS, IOBS, IEQU, &
     &           IAMB, IFRQ, IPAR_REM, IPAR_REF, L_EQU_ORIG, IND_PHAS(3), &
     &           IND_AMPL(3), LL, IND_FRA, N_REJ, LN_AMP, LN_PHS, LTIM, &
     &           POL_MODE, ICHN_1ST(PIM__MFRQ), IND_REF, IND_REM, IND_POL, IER
      COMPLEX*8  RES_AVR(4), CORR_EST_CMPL, DRF_CIR, DRF_ARR(PIM__MCHN,PIM__MPLR)
      CHARACTER  SEARCH_MODE*8, STR*128, STR1*128, OUT*1024, POLAR_STYLE*8, &
     &           POLAR_SAVE*8, POL_CONF*7
      REAL*8,    ALLOCATABLE :: EQU_MAT(:,:), EQU_VEC(:), &
     &                          NOR_MAT(:),   NOR_VEC(:), &
     &                          COV_MAT(:),   ERR_VEC(:), &
     &                          WEI_VEC(:),   EST_VEC(:), CNS_SIG(:), &
     &                          RES_ARR(:,:,:), SNR_ARR(:)
      INTEGER*4, ALLOCATABLE :: IND_EQU(:), IND_OBS(:)
      REAL*8,    ALLOCATABLE :: SNR_NEW(:)
      REAL*4,    ALLOCATABLE :: AC_AVR(:,:,:)
      COMPLEX*8, ALLOCATABLE :: RES(:,:,:)
      LOGICAL*1, ALLOCATABLE :: FL_OBS(:,:)
      LOGICAL*1  FL_BMASK, FL_STA, FL_REJECT_PHAS, FL_REJECT_AMPL
      REAL*4     PHAS_REM_RES(PIM__MCHN), AMPL_REM_RES(PIM__MCHN), &
     &           PHAS_REF_RES(PIM__MCHN), AMPL_REF_RES(PIM__MCHN), &
     &           FREQ_CHN_ALL_R4(PIM__MCHN*PIM__MFRQ)
      REAL*8     FREQ_CHN(PIM__MCHN), &
     &           FREQ_ALL_CHN(PIM__MCHN*PIM__MFRQ), &
     &           PHAS_ALL_REM(PIM__MCHN*PIM__MFRQ), &
     &           PHAS_ALL_REF(PIM__MCHN*PIM__MFRQ), &
     &           AMPL_ALL_RES(PIM__MCHN*PIM__MFRQ), &
     &           PHAS_RMS, AMPL_RMS, FREQ_BEG(PIM__MFRQ), FREQ_END(PIM__MFRQ), RC, SIG, PHAS_EST, &
     &           AMPL_EST, AMPL_EST_LOG, RES_AMPL, RES_PHAS, WW_ACC(2), &
     &           SNR_CHN, RES_PHAS_MAX, RES_AMPL_MAX, SNR_OPP, &
     &           GR_DEL(PIM__MFRA), PH_RAT(PIM__MFRA), PHS(PIM__MFRA), &
     &           WEI_CHN(PIM__MCHN), PHS_AVR_FRQ_REM, GR_DEL_FRQ_REM, PHAS_MOD
      REAL*8     NOD_AMP_STEP, ARG_AMP_NOD(PIM__MCHN), &
     &           NOD_PHS_STEP, ARG_PHS_NOD(PIM__MCHN), SEQ_ARG
      REAL*8     T1(PIM__MCHN), X1(PIM__MCHN), X2(PIM__MCHN)
      REAL*4     FEED_ANG_DIF, PBP_AMP, PBP_PHS, FREQ_CHN_R4(PIM__MCHN)
      INTEGER*1  MASK_CHN, SGN_REM, SGN_REF
      LOGICAL*1  FL_DEBUG
      COMPLEX*8  PBP
      INTEGER*4    M_DEG
      PARAMETER  ( M_DEG = 3 )
      REAL*8     PHAS_CNS_SIG, AMPL_CNS_SIG, SNR_FLOOR, WEI__MIN, NOD_FUDGE, WEI_SCL
      PARAMETER  ( PHAS_CNS_SIG = 1.0D1    ) ! Good range 10-100
      PARAMETER  ( AMPL_CNS_SIG = 1.0D1    )
      PARAMETER  ( SNR_FLOOR    = 128.0D0  )
      PARAMETER  ( WEI__MIN     = 1.D-15   )
      PARAMETER  ( NOD_FUDGE    = 1.D-3    )
      PARAMETER  ( WEI_SCL      = 1.D3     )
      INTEGER*4  IPAR_STA(PIM__MSTA), NUM_USED_OBS(PIM__MSTA,2)
!
      REAL*8,    EXTERNAL :: DP_VV_V, LEGENDRE_POL, BSPL_VAL
      REAL*4,    EXTERNAL :: PHAS_CMPL_R4
      INTEGER*4, EXTERNAL :: ILEN, I_LEN, LTM_DIF
      CHARACTER, EXTERNAL :: PIMA_GET_POL_CONF*7
!
      FL_DEBUG = .TRUE. ! Should be false for a normal run
      LCHN = PIM%NCHN
      LFRQ = PIM%CONF%END_FRQ - PIM%CONF%BEG_FRQ + 1
      IF ( PIM%CONF%BPS_MSEG_ACCUM .LE. 0 ) PIM%CONF%BPS_MSEG_ACCUM = 1
      KCHN = PIM%NCHN/PIM%CONF%BPS_MSEG_ACCUM
!
      IF ( PIM%CONF%DEBUG_LEVEL .GE. 3 ) THEN
           WRITE ( 6, * ) '--------------------------- ' 
           WRITE ( 6, * ) ' PIMA_PBP_FINE               ' 
           WRITE ( 6, * ) '--------------------------- ' 
           CALL FLUSH ( 6 )
      END IF
!
      IF ( PIM%CONF%FRIB_FINE_SEARCH == PIMA__FINE_SEARCH_LSQ ) THEN
           IND_FRA = PIMA__LSQ
         ELSE
           IND_FRA = PIMA__DRF
      END IF
!
! --- Run a dry run in order to learn the number of observations
!
      L_EQU = 0
      L_OBS = 0
      L_PAR = 0
      CALL NOUT_I4 ( PIM__MSTA, IPAR_STA )
      CALL NOUT_I4 ( 2*PIM__MSTA, NUM_USED_OBS )
      LN_PHS = PIM%CONF%BPS_DEG_PHS 
      LN_AMP = PIM%CONF%BPS_DEG_AMP
      IF ( PIM%CONF%BPS_INTRP_METHOD == PIMA__LINEAR ) THEN
           LN_PHS = 1
           LN_AMP = 2
      END IF
!
      DO 410 J1=1,PIM%NSTA
         DO 420 J2=1,2
            IF ( BPS%NUM_OBS_FINE(J1,J2) .LE. 0 ) GOTO 420
            DO 430 J3=1,BPS%NUM_OBS_FINE(J1,J2)
                IF ( DABS(BPS%SNR(J3,J1,J2)) < PIM%CONF%BPS_SNR_MIN_FINE ) GOTO 430
                IND_STA(1) = PIM%OBS(BPS%IND_OBS_SEL(J3,J1,J2))%STA_IND(1)
                IND_STA(2) = PIM%OBS(BPS%IND_OBS_SEL(J3,J1,J2))%STA_IND(2)
                IF ( POL_CONF == PIMA__PC_LL ) THEN
                     IF ( IND_STA(1) == BPS%IND_STA_REF  .OR.  IND_STA(2) == BPS%IND_STA_REF ) THEN
                          NUM_USED_OBS(BPS%IND_STA_REF,1) = NUM_USED_OBS(BPS%IND_STA_REF,1) + 1
                          NUM_USED_OBS(BPS%IND_STA_REF,2) = NUM_USED_OBS(BPS%IND_STA_REF,2) + 1
                     END IF
                END IF
 430        CONTINUE 
 420     CONTINUE 
 410  CONTINUE 
!
      DO 450 J5=1,PIM%NSTA
         DO 460 J6=1,2
            IF ( BPS%NUM_OBS_FINE(J5,J6) .LE. 0 ) GOTO 460
            FL_STA = .FALSE.
            DO 470 J7=1,BPS%NUM_OBS_FINE(J5,J6)
               IF ( DABS(BPS%SNR(J7,J5,J6)) < PIM%CONF%BPS_SNR_MIN_FINE ) GOTO 470
               NUM_USED_OBS(J5,1) = NUM_USED_OBS(J5,1) + 1
               NUM_USED_OBS(J5,2) = NUM_USED_OBS(J5,2) + 1
               IF ( .NOT. FL_STA ) THEN
                    IPAR_STA(J5) = L_PAR
                    IF ( PIM%CONF%BPS_INTRP_METHOD == PIMA__LEGENDRE ) THEN
                         L_PAR = L_PAR + (LN_PHS + LN_AMP + 2)*LFRQ
                       ELSE IF ( PIM%CONF%BPS_INTRP_METHOD == PIMA__SPLINE ) THEN
                         L_PAR = L_PAR + (LN_PHS + LN_AMP + 2*(M_DEG-1))*LFRQ
                       ELSE  IF ( PIM%CONF%BPS_INTRP_METHOD == PIMA__LINEAR ) THEN
                         L_PAR = L_PAR + (LN_PHS + LN_AMP)*LFRQ
                    END IF
                    FL_STA = .TRUE.
               END IF
               L_OBS = L_OBS + 1
               L_EQU = L_EQU + 2*LFRQ*KCHN
 470        CONTINUE
 460     CONTINUE
 450  CONTINUE
      LL_PAR = (L_PAR*(L_PAR+1))/2
      IF ( PIM%CONF%BANDPASS_MASK_FILE .NE. PIMA__BPASS_NO  .AND. &
     &     ASSOCIATED ( PIM%BANDPASS_MASK )                       ) THEN
           FL_BMASK = .TRUE.
         ELSE
           FL_BMASK = .FALSE.
      END IF
!
      IF ( PIM%CONF%DEBUG_LEVEL .GE. 2 ) THEN
           WRITE ( 6, 100 ) L_PAR, L_EQU, L_OBS
 100       FORMAT ( 'PIMA_STA_FINE   L_PAR: ', I5,' L_EQU: ', I7, ' L_OBS: ', I7 )
      END IF
      ALLOCATE ( EQU_MAT(L_PAR,L_EQU), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( 8*L_EQU*L_PAR, STR )
           CALL ERR_LOG ( 8451, IUER, 'PIMA_PBP_FINE', 'Failure to '// &
     &          'allocate '//STR(1:I_LEN(STR))//' bytes of dynamic memory '// &
     &          'for matrix EQU_MAT' )
           GOTO 710
      END IF
      CALL NOUT_R8 ( L_EQU*L_PAR, EQU_MAT )
!
      ALLOCATE ( EQU_VEC(L_EQU), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( 8*L_EQU, STR )
           CALL ERR_LOG ( 8452, IUER, 'PIMA_PBP_FINE', 'Failure to '// &
     &          'allocate '//STR(1:I_LEN(STR))//' bytes of dynamic memory '// &
     &          'for vector EQU_VEC' )
           GOTO 710
      END IF
      CALL NOUT_R8 ( L_EQU, EQU_VEC )
!
      ALLOCATE ( NOR_MAT(LL_PAR), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( 8*LL_PAR, STR )
           CALL ERR_LOG ( 8453, IUER, 'PIMA_PBP_FINE', 'Failure to '// &
     &          'allocate '//STR(1:I_LEN(STR))//' bytes of dynamic memory '// &
     &          'for matrix NOR_MAT' )
           GOTO 710
      END IF
      CALL NOUT_R8 ( LL_PAR, NOR_MAT )
!
      ALLOCATE ( NOR_VEC(L_PAR), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( 8*L_PAR, STR )
           CALL ERR_LOG ( 8454, IUER, 'PIMA_PBP_FINE', 'Failure to '// &
     &          'allocate '//STR(1:I_LEN(STR))//' bytes of dynamic memory '// &
     &          'for vector NOR_VEC' )
           GOTO 710
      END IF
      CALL NOUT_R8 ( L_PAR, NOR_VEC )
!
      ALLOCATE ( COV_MAT(LL_PAR), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( 8*LL_PAR, STR )
           CALL ERR_LOG ( 8455, IUER, 'PIMA_PBP_FINE', 'Failure to '// &
     &          'allocate '//STR(1:I_LEN(STR))//' bytes of dynamic memory '// &
     &          'for matrix COV_MAT' )
           GOTO 710
      END IF
      CALL NOUT_R8 ( LL_PAR, COV_MAT )
!
      ALLOCATE ( ERR_VEC(L_PAR), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( 8*L_PAR, STR )
           CALL ERR_LOG ( 8456, IUER, 'PIMA_PBP_FINE', 'Failure to '// &
     &          'allocate '//STR(1:I_LEN(STR))//' bytes of dynamic memory '// &
     &          'for vector ERR_VEC' )
           GOTO 710
      END IF
      CALL NOUT_R8 ( L_PAR, ERR_VEC )
!
      ALLOCATE ( WEI_VEC(L_EQU), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( 8*L_EQU, STR )
           CALL ERR_LOG ( 8457, IUER, 'PIMA_PBP_FINE', 'Failure to '// &
     &          'allocate '//STR(1:I_LEN(STR))//' bytes of dynamic memory '// &
     &          'for vector WEI_VEC' )
           GOTO 710
      END IF
      CALL NOUT_R8 ( L_EQU, WEI_VEC )
!
      ALLOCATE ( EST_VEC(L_PAR), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( 8*L_EQU, STR )
           CALL ERR_LOG ( 8458, IUER, 'PIMA_PBP_FINE', 'Failure to '// &
     &          'allocate '//STR(1:I_LEN(STR))//' bytes of dynamic memory '// &
     &          'for vector EST_VEC' )
           GOTO 710
      END IF
      CALL NOUT_R8 ( L_PAR, EST_VEC )
!
      ALLOCATE ( RES(LCHN,PIM%NFRQ,4), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR  )
           CALL INCH  ( LCHN*PIM%NFRQ*4*8, STR )
           CALL ERR_LOG ( 8459, IUER, 'PIMA_PBP_FINE', 'Error in an '// &
     &         'attempt to allocate '//STR(1:I_LEN(STR))//' bytes of '// &
     &         'dynamic memory for residuals' )
           GOTO 710
      END IF
      RES = 0.0
!
      ALLOCATE ( AC_AVR(LCHN,LFRQ,4), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR  )
           CALL INCH  ( LCHN*LFRQ*4*8, STR )
           CALL ERR_LOG ( 8460, IUER, 'PIMA_PBP_FINE', 'Error in an '// &
     &         'attempt to allocate '//STR(1:I_LEN(STR))//' bytes of '// &
     &         'dynamic memory for array autospectrum AC_AVR' )
           GOTO 710
      END IF
      AC_AVR = 0.0
!
      ALLOCATE ( CNS_SIG(L_PAR), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( 8*L_PAR, STR )
           CALL ERR_LOG ( 8461, IUER, 'PIMA_PBP_FINE', 'Failure to '// &
     &          'allocate '//STR(1:I_LEN(STR))//' bytes of dynamic memory '// &
     &          'for vector CNS_SIG' )
           GOTO 710
      END IF
      CALL NOUT_R8 ( L_PAR, CNS_SIG )
!
      ALLOCATE ( RES_ARR(LFRQ,L_OBS,2), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( 8*L_OBS, STR )
           CALL ERR_LOG ( 8462, IUER, 'PIMA_PBP_FINE', 'Failure to '// &
     &          'allocate '//STR(1:I_LEN(STR))//' bytes of dynamic memory '// &
     &          'for vector RES_ARR' )
           GOTO 710
      END IF
      CALL NOUT_R8 ( LFRQ*L_OBS*2, RES_ARR )
!
      ALLOCATE ( IND_EQU(L_OBS), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( 8*L_OBS, STR )
           CALL ERR_LOG ( 8463, IUER, 'PIMA_PBP_FINE', 'Failure to '// &
     &          'allocate '//STR(1:I_LEN(STR))//' bytes of dynamic memory '// &
     &          'for vector IND_EQU' )
           GOTO 710
      END IF
      CALL NOUT_I4 ( L_OBS, IND_EQU )
!
      ALLOCATE ( IND_OBS(L_OBS), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( 8*L_OBS, STR )
           CALL ERR_LOG ( 8464, IUER, 'PIMA_PBP_FINE', 'Failure to '// &
     &          'allocate '//STR(1:I_LEN(STR))//' bytes of dynamic memory '// &
     &          'for vector IND_OBS' )
           GOTO 710
      END IF
      CALL NOUT_I4 ( L_OBS, IND_OBS )
!
      ALLOCATE ( SNR_NEW(L_OBS), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( 8*L_OBS, STR )
           CALL ERR_LOG ( 8465, IUER, 'PIMA_PBP_FINE', 'Failure to '// &
     &          'allocate '//STR(1:I_LEN(STR))//' bytes of dynamic memory '// &
     &          'for vector SNR_NEW' )
           GOTO 710
      END IF
      CALL NOUT_I4 ( L_OBS, IND_OBS )
!
      ALLOCATE ( FL_OBS(L_OBS,3), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( 8*L_OBS, STR )
           CALL ERR_LOG ( 8466, IUER, 'PIMA_PBP_FINE', 'Failure to '// &
     &          'allocate '//STR(1:I_LEN(STR))//' bytes of dynamic memory '// &
     &          'for vector FL_OBS' )
           GOTO 710
      END IF
      CALL NOUT ( L_OBS*3, FL_OBS )
!
      ALLOCATE ( SNR_ARR(L_OBS), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( 8*L_OBS, STR )
           CALL ERR_LOG ( 8467, IUER, 'PIMA_PBP_FINE', 'Failure to '// &
     &          'allocate '//STR(1:I_LEN(STR))//' bytes of dynamic memory '// &
     &          'for vector SNR_ARR' )
           GOTO 710
      END IF
      CALL NOUT_R8 ( L_OBS, SNR_ARR )
!
      IOBS = 0
      IEQU = 0
      N_REJ = 0
      PHAS_ALL_REM = 0.0
      AMPL_ALL_RES = 0.0
      PHAS_ALL_REF = 0.0
!
      DO 480 J8=1,PIM%NSTA
         DO 490 J9=1,2
            IF ( BPS%NUM_OBS_FINE(J8,J9) .LE. 0 ) GOTO 490
            DO 4100 J10=1,BPS%NUM_OBS_FINE(J8,J9)
               IF ( BPS%SNR(J10,J8,J9) < 0.0 ) BPS%SNR(J10,J8,J9) = -BPS%SNR(J10,J8,J9)
               IF ( BPS%SNR(J10,J8,J9) < PIM%CONF%BPS_SNR_MIN_FINE ) GOTO 4100
               IF ( BPS%SNR(J10,J8,J9) < PIM%CONF%BPS_NOBS_FINE    ) GOTO 4100
               IOBS = IOBS + 1
               IND_OBS(IOBS) = BPS%IND_OBS_SEL(J10,J8,J9)
               IND_STA(1) = PIM%OBS(IND_OBS(IOBS))%STA_IND(1)
               IND_STA(2) = PIM%OBS(IND_OBS(IOBS))%STA_IND(2)
               IF ( IND_STA(1) == BPS%IND_STA_REF ) THEN
                    IND_REF =  1
                    IND_REM =  2
                    SGN_REM =  1
                    SGN_REF = -1
                  ELSE
                    IND_REF =  2
                    IND_REM =  1
                    SGN_REM = -1
                    SGN_REF =  1
               END IF
!
               GR_DEL(IND_FRA) = BPS%GR_DEL(J10,J8,J9)
               PH_RAT(IND_FRA) = BPS%PH_RAT(J10,J8,J9)
               PHS(IND_FRA)    = BPS%PHS(J10,J8,J9)
!
               SNR_NEW(IOBS) = DABS(BPS%SNR(J10,J8,J9))
               LTIM = PIM%OBS(IND_OBS(IOBS))%NUM_EPC(PIM%OBS(IND_OBS(IOBS))%REF_FRG_INDS(PIM%CONF%FRQ_GRP))
!
               CALL ERR_PASS ( IUER, IER )
               POL_CONF = PIMA_GET_POL_CONF ( PIM, IND_OBS(IOBS), IER ) 
               IF ( IER .NE. 0 ) THEN
                    CALL ERR_LOG ( 8468, IUER, 'PIMA_PBP_FINE', 'Error in '// &
     &                  'an attempt to get polarizartion code' )
                    RETURN 
               END IF
!
               POLAR_SAVE = PIM%CONF%POLAR
               IF ( POLAR_SAVE .NE. PIMA__POLAR_I ) THEN
                    CALL ERR_LOG ( 8469, IUER, 'PIMA_PBP_FINE', 'Trap '// &
     &                  'of internal control: polarization '// &
     &                   PIM%CONF%POLAR//' is not suitable '// &
     &                  'for polarization bandpass computation, only I '// &
     &                  'is supported' )
                    GOTO 710
               END IF
               RES = 0.0
!
! ------------ Compute residuals of the direct polarization
!
               CALL ERR_PASS  ( IUER, IER )
               IF ( POL_CONF == PIMA__PC_CC ) THEN
                    POL_MODE = PIMA__PAR
                    SEARCH_MODE = PIMA__2FFT
                  ELSE
                    POL_MODE = PIMA__PALL_MIXED
                    SEARCH_MODE = PIMA__2FFT
                END IF
!
                IND_POL = 1
                CALL PIMA_GET_RESID ( PIM, VTD, BPS%IND_OBS_SEL(J10,J8,J9), IND_POL, &
     &                     LCHN, LFRQ, LTIM, &
     &                     GR_DEL, PH_RAT, BPS%GR_RAT(J10,J8,J9), PHS, &
     &                     SNR_NEW(IOBS), BPS%TIME_FRT(J10,J8,J9), &
     &                     SEARCH_MODE, PIMA__BPASS_PHS, POL_MODE, &
     &                     RES, RES_AVR, SNR_ARR(IOBS), IER )
                IF ( IER .NE. 0 ) THEN
                     WRITE ( 6, * ) ' IER = ', IER
                     CALL CLRCH ( STR )
                     CALL INCH  ( BPS%IND_OBS_SEL(J10,J8,J9), STR )
                     CALL ERR_LOG ( 8471, IUER, 'PIMA_PBP_FINE', 'Error in '// &
     &                   'an attempt to compute residuals for observation '//STR )
                     GOTO 710
                END IF
!
                IPAR_REM = IPAR_STA(J8)
                IFRQ = 0
                IND_EQU(IOBS)  = IEQU + 1
                FL_OBS(IOBS,1) = .TRUE.
                FL_OBS(IOBS,2) = .TRUE.
                FL_OBS(IOBS,3) = .TRUE.
                FEED_ANG_DIF = PIM%OBS(IND_OBS(IOBS))%FEED_ANG(1) - &
     &                         PIM%OBS(IND_OBS(IOBS))%FEED_ANG(2) 
                KP       = 0
                ICHN_1ST = 0
                JCHN     = 0
                AMPL_ALL_RES = 0.0
                DO 4110 J11=PIM%CONF%BEG_FRQ,PIM%CONF%END_FRQ
                   IFRQ = IFRQ + 1
                   ICHN = 0
                   DO 4120 J12=1,KCHN ! cycle over spectral channels
                      JCHN = JCHN + 1
                      IF ( ICHN_1ST(J11) == 0 ) THEN
                           ICHN_1ST(J11) = JCHN
                      END IF 
                      FREQ_CHN_R4(J12) = 0.0
                      FREQ_CHN(J12) = 0.0
                      WEI_CHN(J12)  = 0.0
                      DRF_ARR(J12,1:PIM__MPLR) = CMPLX ( 0.0, 0.0 )
                      DRF_CIR = CMPLX (0.0, 0.0 )
                      DO 4130 J13=1,PIM%CONF%BPS_MSEG_ACCUM
                         ICHN = ICHN + 1
                         IF ( FL_BMASK ) THEN
                              MASK_CHN = PIM%BANDPASS_MASK(ICHN,J11,IND_STA(1),PIMA__MASK_BPAS) * &
     &                                   PIM%BANDPASS_MASK(ICHN,J11,IND_STA(2),PIMA__MASK_BPAS)
                            ELSE
                              MASK_CHN = 1
                         END IF
!
                         IF ( ABS(RES(ICHN,IFRQ,1)) < PIMA__AMP_MIN ) THEN
                              MASK_CHN = 0
                         END IF
                         IF ( MASK_CHN == 1 ) THEN
                              FREQ_CHN_R4(J12) = FREQ_CHN_R4(J12) + PIM%FREQ_ARR(ICHN,J11,PIM%CONF%FRQ_GRP)
                              FREQ_CHN(J12)    = FREQ_CHN(J12)    + PIM%FREQ_ARR(ICHN,J11,PIM%CONF%FRQ_GRP)
                              IF ( ABS(RES(ICHN,IFRQ,1)) > PIMA__AMP_MIN ) THEN
                                   IF ( POL_CONF == PIMA__PC_CC ) THEN
                                        IF ( SGN_REM == -1 ) THEN
                                             DRF_CIR = DRF_CIR + MASK_CHN * CONJG ( RES(ICHN,IFRQ,2)/RES(ICHN,IFRQ,1) )
                                           ELSE
                                             DRF_CIR = DRF_CIR + MASK_CHN *         RES(ICHN,IFRQ,2)/RES(ICHN,IFRQ,1)
                                        END IF
                                      ELSE                             
                                        DRF_ARR(J12,1:PIM__MPLR) = DRF_ARR(J12,1:PIM__MPLR) + &
     &                                                                MASK_CHN*RES(ICHN,IFRQ,1:PIM__MPLR)
                                   END IF
                              END IF
                              WEI_CHN(J12)  = WEI_CHN(J12)  + MASK_CHN
                         END IF
 4130                 CONTINUE 
!
! ------------------- Get the effective frequency of the multi-channel
!
                      IF ( WEI_CHN(J12) > 0.0 ) THEN
                           FREQ_CHN_R4(J12) = FREQ_CHN_R4(J12)/WEI_CHN(J12)
                           FREQ_CHN(J12) = FREQ_CHN(J12)/WEI_CHN(J12)
                           IF ( POL_CONF == PIMA__PC_CC ) THEN
                                PHAS_REM_RES(J12) = PHAS_CMPL_R4 ( DRF_CIR )
                                AMPL_REM_RES(J12) = ABS ( DRF_CIR )/WEI_CHN(J12)
                                PHAS_ALL_REM(JCHN) = PHAS_REM_RES(J12) 
                                AMPL_ALL_RES(JCHN) = AMPL_REM_RES(J12)
                              ELSE
                                DRF_ARR(J12,1:PIM__MPLR) = DRF_ARR(J12,1:PIM__MPLR)/WEI_CHN(J12)
                           END IF
                         ELSE 
                           FREQ_CHN_R4(J12) = PIM%FREQ_ARR(ICHN,J11,PIM%CONF%FRQ_GRP)
                           FREQ_CHN(J12)    = PIM%FREQ_ARR(ICHN,J11,PIM%CONF%FRQ_GRP)
                           WEI_CHN(J12)  = 0.0
                           IF ( POL_CONF == PIMA__PC_CC ) THEN
                                AMPL_REM_RES(J12) = 1.0
                                PHAS_REM_RES(J12) = 0.0
                              ELSE
                                DRF_ARR(J12,1:PIM__MPLR) = 0.0
                           END IF
                      END IF
                      FREQ_ALL_CHN(JCHN) = FREQ_CHN(J12) 
                      IF ( PIM%CONF%DEBUG_LEVEL == 33 ) THEN
                           KP = KP + 1
                           T1(KP) = FREQ_CHN(J12)
                           X1(KP) = PHAS_REM_RES(J12) 
                      END IF
 4120              CONTINUE 
!
                   FREQ_BEG(IFRQ) = PIM%FRQ(J11,PIM%CONF%FRQ_GRP)%FREQ
                   FREQ_END(IFRQ) = PIM%FRQ(J11,PIM%CONF%FRQ_GRP)%FREQ + &
     &                                      PIM%FRQ(J11,PIM%CONF%FRQ_GRP)%BAND_WIDTH
!
                   IF ( POL_CONF == PIMA__PC_LL ) THEN
                        CALL ERR_PASS ( IUER, IER )
                        IF ( IND_STA(1) == BPS%IND_STA_REF ) THEN
                             CALL PIMA_LIN_PHAOFF_ARR ( PIM, IND_OBS(IOBS), KCHN, DRF_ARR, RES_AVR, &
     &                                                  FREQ_CHN_R4, FEED_ANG_DIF, &
     &                                                  PIM%FREQ_ARR(1,J11,PIM%CONF%FRQ_GRP), &
     &                                                  AMPL_REF_RES, AMPL_REM_RES, &
     &                                                  PHAS_REF_RES, PHAS_REM_RES, IER )
                           ELSE
                             CALL PIMA_LIN_PHAOFF_ARR ( PIM, IND_OBS(IOBS), KCHN, DRF_ARR, RES_AVR, &
     &                                                  FREQ_CHN_R4, FEED_ANG_DIF, &
     &                                                  PIM%FREQ_ARR(1,J11,PIM%CONF%FRQ_GRP), &
     &                                                  AMPL_REM_RES, AMPL_REF_RES, &
     &                                                  PHAS_REM_RES, PHAS_REF_RES, IER )
                        END IF
                        IF ( IER .NE. 0 ) THEN
                             CALL CLRCH ( STR )
                             CALL INCH  ( IND_OBS(IOBS), STR )
                             CALL CLRCH ( STR1 )
                             CALL INCH  ( J11, STR1 )
                             CALL ERR_LOG ( 8472, IUER, 'PIMA_PBP_FINE', 'Error in '// &
     &                           'computation of polarization phase offsets during '// &
     &                           'processing observation '//TRIM(STR)//' IF '//STR1 )
                             RETURN 
                        END IF
!
                        DO 4140 J14=1,KCHN
                           IF ( IND_STA(1) == BPS%IND_STA_REF ) THEN
                                PHAS_ALL_REF(ICHN_1ST(J11)+J14-1) = PHAS_REF_RES(J14) 
                                PHAS_ALL_REM(ICHN_1ST(J11)+J14-1) = 0.0
                              ELSE
                                PHAS_ALL_REF(ICHN_1ST(J11)+J14-1) = 0.0
                                PHAS_ALL_REM(ICHN_1ST(J11)+J14-1) = PHAS_REM_RES(J14) 
                           END IF
                           AMPL_ALL_RES(ICHN_1ST(J11)+J14-1) = AMPL_REM_RES(J14)
 4140                   CONTINUE 
                   END IF !! POL_CONF == PIMA__PC_LL 
 4110           CONTINUE
!
! ------------- Resolve phase ambiguities
!
                PHS_AVR_FRQ_REM = 0.0D0
                GR_DEL_FRQ_REM  = 0.0D0
!
                CALL ERR_PASS ( IUER, IER )
                CALL PIMA_AMB_RES_R8 ( JCHN, FREQ_ALL_CHN, PHAS_ALL_REM, AMPL_ALL_RES, &
     &                                 PHS_AVR_FRQ_REM, GR_DEL_FRQ_REM, IER  )
                IF ( IER .NE. 0 ) THEN
                     CALL CLRCH ( STR  )
                     CALL INCH  ( IND_OBS(IOBS), STR )
                     CALL ERR_LOG ( 8329, IUER, 'PIMA_BPASS_FINE', 'Error in '// &
     &                   'an attempt to resolve phase ambiguity while processing '// &
     &                   ' of observation '//STR )
                     RETURN 
                END IF
                IF ( PIM%CONF%DEBUG_LEVEL .GE. 4 ) THEN
                     WRITE ( 6, 210 ) PIM%C_STA(J8), IND_OBS(IOBS), GR_DEL_FRQ_REM, PHS_AVR_FRQ_REM
 210                 FORMAT ( 'PIMA_PBP_FINE-581 Sta: ', A, ' Iobs: ', I5, ' Gr_del: ', 1PD12.5, ' Phs_avr: ', F7.4 )
                END IF
!
                IFRQ = 0
                DO 4150 J15=PIM%CONF%BEG_FRQ,PIM%CONF%END_FRQ
                   IFRQ = IFRQ + 1
                   DO 4160 J16=1,KCHN ! cycle over spectral channels
                      FREQ_CHN(J16) = FREQ_ALL_CHN(ICHN_1ST(J15)-1+J16)
                      AMPL_REM_RES(J16) = AMPL_ALL_RES(ICHN_1ST(J15)-1+J16)
                      PHAS_REM_RES(J16) = PHAS_ALL_REM(ICHN_1ST(J15)-1+J16)
                      PHAS_MOD = PHS_AVR_FRQ_REM + PI2*GR_DEL_FRQ_REM*(FREQ_CHN(J16) - FREQ_CHN(1))
                      IAMB     = IDNINT ( (PHAS_REM_RES(J16) - PHAS_MOD)/PI2 )
                      PHAS_REM_RES(J16) = PHAS_REM_RES(J16) - PI2*IAMB
                      IF ( POL_CONF == PIMA__PC_LL ) THEN
                           CONTINUE 
                      END IF
!
! ------------------- Equation for residual phase
!
                      IEQU = IEQU + 1
                      IF ( PIM%CONF%BPS_INTRP_METHOD == PIMA__LEGENDRE ) THEN
                           DO 4170 J17=0,PIM%CONF%BPS_DEG_PHS
                              EQU_MAT(IPAR_REM+J17+1,IEQU) = LEGENDRE_POL ( J17, FREQ_BEG(IFRQ), &
     &                                                                      FREQ_END(IFRQ), FREQ_CHN(J16) )
                              CNS_SIG(IPAR_REM+J17+1) = PHAS_CNS_SIG
                              IF ( FL_DEBUG ) THEN
                                   WRITE ( 6, 220 ) 'PHS', IND_OBS(IOBS), PIM%C_STA(J8), IFRQ, IEQU, &
     &                                               J16, IPAR_REM+J17+1, EQU_MAT(IPAR_REM+J17+1,IEQU), &
     &                                               PHAS_REM_RES(J16), ICHN_1ST(J15)
 220                               FORMAT ( 'PIMA_PPF_FINE-597 ', A, 1X, I6, 1X, A, ' IFRQ= ', I2, ' IEQU= ', I6, &
     &                                      ' Ichn= ', I5, ' IPAR= ', I5, ' EQU= ', 1PD12.5, ' RH= ', 1PD12.5, &
     &                                      ' ic1= ', I6 )
                              END IF
 4170                      CONTINUE
                         ELSE IF ( PIM%CONF%BPS_INTRP_METHOD == PIMA__SPLINE ) THEN
!
! ------------------------ Generate the equidistant sequence of knots
!
                           NOD_PHS_STEP = (FREQ_END(IFRQ) - FREQ_BEG(IFRQ))/(LN_PHS -1)
                           DO 4180 J18=1,LN_PHS
                              ARG_PHS_NOD(J18) = FREQ_BEG(IFRQ) + (J18-1)*NOD_PHS_STEP
 4180                      CONTINUE 
                           ARG_PHS_NOD(1)      = ARG_PHS_NOD(1)      - NOD_FUDGE*NOD_PHS_STEP
                           ARG_PHS_NOD(LN_PHS) = ARG_PHS_NOD(LN_PHS) + NOD_FUDGE*NOD_PHS_STEP
!
! ------------------------ Collect partial derivatives for the spline and weights
!
                           DO 4190 J19=1-M_DEG,LN_PHS-1
                              EQU_MAT(IPAR_REM + J19+M_DEG,IEQU) = BSPL_VAL ( LN_PHS, ARG_PHS_NOD, &
     &                                                                        M_DEG, J19, FREQ_CHN(J16) )
                              CNS_SIG(IPAR_REM + J19+M_DEG) = PHAS_CNS_SIG
 4190                      CONTINUE
                         ELSE IF ( PIM%CONF%BPS_INTRP_METHOD == PIMA__LINEAR ) THEN
                           IF ( IPAR_REM+2 > L_PAR ) THEN
                                WRITE ( 6, * ) 'L_PAR= ', L_PAR, ' IPAR_REM+2= ', IPAR_REM + 2
                                CALL ERR_LOG ( 8473, IUER, 'PIMA_PBP_FINE', 'Trap of '// &
     &                              'internal control: equation index is too big' )
                                RETURN 
                           END IF
                           EQU_MAT(IPAR_REM+1,IEQU) = 1.0
                           EQU_MAT(IPAR_REM+2,IEQU) = (FREQ_CHN(J16) - FREQ_CHN(1))/ &
     &                                             (PIM%FREQ_ARR(PIM%NCHN,J15,PIM%CONF%FRQ_GRP) - &
     &                                              PIM%FREQ_ARR(1,J15,PIM%CONF%FRQ_GRP))
                           CNS_SIG(IPAR_REM+1) = PHAS_CNS_SIG
                           CNS_SIG(IPAR_REM+2) = PHAS_CNS_SIG
                      END IF
!
                      IF ( AMPL_REM_RES(J16) > PIMA__AMP_MIN .AND. &
     &                     AMPL_REM_RES(J16) < PIMA__AMP_MAX       ) THEN
                           IF ( POL_CONF == PIMA__PC_CC ) THEN
                                WEI_VEC(IEQU) = AMPL_REM_RES(J16)*ABS(RES_AVR(1))
                              ELSE 
                                WEI_VEC(IEQU) = SNR_ARR(IOBS)
                           END IF
                         ELSE
                           WEI_VEC(IEQU) = WEI__MIN
                      END IF
                      EQU_VEC(IEQU) = PHAS_REM_RES(J16)
!
! ------------------- Get the residual amplitude
!
                      IEQU = IEQU + 1
                      IF ( PIM%CONF%BPS_INTRP_METHOD == PIMA__LEGENDRE ) THEN
                           DO 4200 J20=0,PIM%CONF%BPS_DEG_AMP
                              EQU_MAT(IPAR_REM+PIM%CONF%BPS_DEG_PHS+1 + J20+1,IEQU) = &
     &                            LEGENDRE_POL ( J20, FREQ_BEG(IFRQ), FREQ_END(IFRQ), FREQ_CHN(J16) )
                              CNS_SIG(IPAR_REM+PIM%CONF%BPS_DEG_PHS+1 + J20+1) = AMPL_CNS_SIG
       write ( 6, * ) 'PPF-668  ipar= ', IPAR_REM+PIM%CONF%BPS_DEG_PHS+1 + J20+1, ' iequ= ', iequ, ' equ= ', equ_mat(ipar_rem+pim%conf%bps_deg_phs+1 + j20+1,iequ) ; call flush ( 6 ) ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 4200                      CONTINUE
                         ELSE IF ( PIM%CONF%BPS_INTRP_METHOD == PIMA__SPLINE ) THEN
!
! ------------------------ Generate the sequence of knots according to the square root rule
!
                           DO 4210 J21=1,LN_AMP
                              SEQ_ARG = (J21-1)/DBLE(LN_AMP-1)
                              IF ( SEQ_ARG < 0.5D0 ) THEN
                                   ARG_AMP_NOD(J21) = FREQ_BEG(IFRQ) + (FREQ_END(IFRQ) - FREQ_BEG(IFRQ))* &
     &                                                                 (1.0D0 - DSQRT(DABS(1.0D0 - 2.D0*SEQ_ARG)))/2.0D0
                                ELSE
                                   ARG_AMP_NOD(J21) = FREQ_BEG(IFRQ) + (FREQ_END(IFRQ) - FREQ_BEG(IFRQ))* &
     &                                                                 (1.0D0 + DSQRT(DABS(1.0D0 - 2.D0*SEQ_ARG)))/2.0D0
                              END IF    
 4210                      CONTINUE 
                           NOD_AMP_STEP = ARG_AMP_NOD(LN_AMP) - ARG_AMP_NOD(LN_AMP-1)
                           ARG_AMP_NOD(1)      = ARG_AMP_NOD(1)      - NOD_FUDGE*NOD_AMP_STEP
                           ARG_AMP_NOD(LN_AMP) = ARG_AMP_NOD(LN_AMP) + NOD_FUDGE*NOD_AMP_STEP
                           DO 4220 J22=1-M_DEG,LN_AMP-1
                              EQU_MAT(IPAR_REM+LN_PHS+M_DEG-1 + J22+M_DEG,IEQU) = BSPL_VAL ( LN_AMP, ARG_AMP_NOD, &
     &                                                                                       M_DEG, J22, FREQ_CHN(J16) )
                              CNS_SIG(IPAR_REM+LN_PHS+M_DEG-1 + J22+M_DEG) = AMPL_CNS_SIG
 4220                      CONTINUE
                         ELSE IF ( PIM%CONF%BPS_INTRP_METHOD == PIMA__LINEAR ) THEN
                           IF ( IPAR_REM+3 > L_PAR ) THEN
                                WRITE ( 6, * ) 'L_PAR= ', L_PAR, ' IPAR_REM+3= ', IPAR_REM+ 3
                                CALL ERR_LOG ( 8474, IUER, 'PIMA_PBP_FINE', 'Trap of '// &
     &                              'internal control: equation index is too big' )
                                RETURN 
                           END IF
                           EQU_MAT(IPAR_REM+3,IEQU) = 1.0
                           CNS_SIG(IPAR_REM+3) = AMPL_CNS_SIG
                      END IF
                      IF ( AMPL_REM_RES(J16) > PIMA__AMP_MIN .AND. &
     &                     AMPL_REM_RES(J16) < PIMA__AMP_MAX       ) THEN
                           WEI_VEC(IEQU) = SNR_ARR(IOBS)/1.D4
                           EQU_VEC(IEQU) = LOG(AMPL_REM_RES(J16))
                         ELSE
                           EQU_VEC(IEQU) = 0.0D0
                      END IF
       write ( 6, * ) 'PPF-709  iequ= ', iequ, ' rh= ', equ_vec(iequ), ' wei= ', wei_vec(iequ) ; call flush ( 6 ) ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 4160              CONTINUE
                   IF ( PIM%CONF%BPS_INTRP_METHOD  == PIMA__LEGENDRE ) THEN
                        IPAR_REM = IPAR_REM + (LN_PHS + LN_AMP + 2)
                      ELSE IF ( PIM%CONF%BPS_INTRP_METHOD == PIMA__SPLINE ) THEN
                        IPAR_REM = IPAR_REM + (LN_PHS + LN_AMP + 2*(M_DEG-1))
                      ELSE IF ( PIM%CONF%BPS_INTRP_METHOD == PIMA__LINEAR ) THEN
                        IPAR_REM = IPAR_REM + (LN_PHS + LN_AMP)
                   END IF
 4150           CONTINUE
                IF ( PIM%CONF%DEBUG_LEVEL == 33 ) THEN
                     CALL CLRCH ( STR )
                     CALL INCH  ( IND_OBS(IOBS), STR )
                     CALL DIAGI_SETDEF ( IER, 'DIAGI_CTIT', 'PBP_FINE Res_phase Sta: '// &
     &                                   PIM%C_STA(J8)//' Obs: '//STR )
                     CALL DIAGI_1 ( KP, T1, X1, IER )
                END IF
 4100       CONTINUE
 490     CONTINUE
 480  CONTINUE
!
! --- Rescale the weight
!
      WEI_VEC = WEI_SCL * WEI_VEC
!
! --- Solve the LSQ system with constraints
!
      CALL ERR_PASS ( IUER, IER )
      CALL LSQW_CNS_SAVE ( L_PAR, L_EQU, EQU_MAT, EQU_VEC, WEI_VEC, &
     &                     CNS_SIG, NOR_MAT, NOR_VEC, COV_MAT, EST_VEC, &
     &                     ERR_VEC, RC, IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( L_EQU, STR )
           CALL CLRCH ( STR1 )
           CALL INCH  ( L_PAR, STR1 )
           CALL ERR_LOG ( 8475, IUER, 'PIMA_PBP_FINE', 'Failure to '// &
     &          'solve the system of '//STR(1:I_LEN(STR))//' normal '// &
     &          'equations with '//STR1(1:I_LEN(STR1))//' unknowns' )
           WRITE ( 6, * ) 'Block_len: ', (PIM%CONF%BPS_DEG_PHS + &
     &                                    PIM%CONF%BPS_DEG_AMP + 2)*LFRQ
           GOTO 710
      END IF
      L_EQU_ORIG = L_EQU
      IF ( FL_DEBUG ) THEN
           DO 5230 J23=1,L_PAR
              WRITE ( 6, 230 ) J23, EST_VEC(J23)
 230          FORMAT ( 'PIMA_PBP_FINE-744 Ind= ', I6, ' Est_vec= ', 1PD12.5 )
 5230      CONTINUE 
      END IF
!
      IF ( PIM%CONF%DEBUG_LEVEL .GE. 2 ) THEN
           WRITE ( 6, * ) 'PIMA_PBP_FINE: normal system has been solved'
      END IF
      IF ( PIM%CONF%DEBUG_LEVEL .GE. 4 ) THEN
           WRITE ( 6, * ) 'PIMA_PBP_FINE: RC= ', RC
      END IF
!
      DO 4230 J23=1,L_EQU_ORIG
         IOBS = 0
         RES_PHAS_MAX = 0.0D0
         RES_AMPL_MAX = 0.0D0
         IND_AMPL = 0
         IND_PHAS = 0
         DO 4240 J24=1,PIM%NSTA
            DO 4250 J25=1,2
               IF ( BPS%NUM_OBS_FINE(J24,J25) .LE. 0 ) GOTO 4250
               DO 4260 J26=1,BPS%NUM_OBS_FINE(J24,J25)
                  IF ( DABS(BPS%SNR(J26,J24,J25)) < PIM%CONF%BPS_SNR_MIN_FINE ) GOTO 4260
                  IF ( DABS(BPS%SNR(J26,J24,J25)) < PIM%CONF%BPS_NOBS_FINE    ) GOTO 4260
                  IOBS = IOBS + 1
                  IND_OBS(IOBS) = BPS%IND_OBS_SEL(J26,J24,J25)
                  IEQU = IND_EQU(IOBS) - 1
                  IPAR_REM = IPAR_STA(J24)
                  IF ( PIM%CONF%DEBUG_LEVEL .GE. 3  .AND. J23 == 1 ) THEN
                       CALL CLRCH ( OUT )
                       WRITE ( UNIT=OUT, FMT=240 ) IOBS, &
     &                         PIM%C_STA(PIM%OBS(IND_OBS(IOBS))%STA_IND(1)), &
     &                         PIM%C_STA(PIM%OBS(IND_OBS(IOBS))%STA_IND(2)), &
     &                         IND_OBS(IOBS), DABS(SNR_NEW(IOBS)) 
 240                   FORMAT ( 'PBP_FINE_RES   # ', I4, 2X, A,'/',A, ' Obs: ', I6, &
     &                          ' SNR: ', F8.2 )
                  END IF
                  DO 4270 J27=1,LFRQ
                     RES_ARR(J27,IOBS,1) = 0.0D0
                     RES_ARR(J27,IOBS,2) = 0.0D0
                     WW_ACC(1) = 0.0D0
                     WW_ACC(2) = 0.0D0
                     DO 4280 J28=1,KCHN
!
! --------------------- Compute the rms of phase residuals
!
                        IEQU = IEQU + 1
                        IF ( IEQU .LE. 0  .OR. IEQU .GT. L_EQU ) THEN
                             CALL CLRCH ( STR  )
                             CALL CLRCH ( STR1 )
                             CALL INCH  ( IEQU,  STR  )
                             CALL INCH  ( L_EQU, STR1 )
                             CALL ERR_LOG ( 8476, IUER, 'PIMA_PBP_FINE', &
     &                           'Trap of internal control: IEQU  '// &
     &                            STR(1:I_LEN(STR))//' is out of range '// &
     &                           '[1, '//STR1(1:I_LEN(STR1))//' ] during '// &
     &                           'procesinf station '//PIM%STA(J24)%IVS_NAME )
                             GOTO 710 
                        END IF
                        RES_PHAS = EQU_VEC(IEQU) - &                              
     &                                 DP_VV_V ( L_PAR, EQU_MAT(1,IEQU), EST_VEC )
                        IF ( FL_OBS(IOBS,1) ) THEN
                            RES_ARR(J27,IOBS,1) = RES_ARR(J27,IOBS,1) + &
     &                                            (RES_PHAS*WEI_VEC(IEQU))**2
                             WW_ACC(1) = WW_ACC(1) + WEI_VEC(IEQU)**2
                        END IF
!
! --------------------- Compute rms of amplitude residuals
!
                        IEQU = IEQU + 1
                        RES_AMPL = EQU_VEC(IEQU) - &
     &                                 DP_VV_V ( L_PAR, EQU_MAT(1,IEQU), EST_VEC )
                        IF ( FL_OBS(IOBS,2) ) THEN
                             RES_ARR(J27,IOBS,2) = RES_ARR(J27,IOBS,2) + &
     &                                            (RES_AMPL*WEI_VEC(IEQU))**2
                             WW_ACC(2) = WW_ACC(2) + WEI_VEC(IEQU)**2
                        END IF
 4280                CONTINUE
!
                     IF ( FL_OBS(IOBS,1) .AND. WW_ACC(1) > 0.0D0 ) THEN
                          RES_ARR(J27,IOBS,1) = DSQRT ( RES_ARR(J27,IOBS,1)/WW_ACC(1) )
                        ELSE
                          RES_ARR(J27,IOBS,1) = 0.0
                     END IF
                     IF ( FL_OBS(IOBS,2) .AND. WW_ACC(2) > 0.0D0 ) THEN
                          RES_ARR(J27,IOBS,2) = DSQRT ( RES_ARR(J27,IOBS,2)/WW_ACC(2) )
                        ELSE
                          RES_ARR(J27,IOBS,2) = 0.0
                     END IF
!
! ------------------ Compute SNR in one channel: SNR_CHN
!
                     SNR_CHN = 1.0D0/DSQRT( 1.0D0/SNR_ARR(IOBS)**2 + 1.0D0/SNR_FLOOR**2)/ &
     &                               DSQRT(1.0D0*LCHN*KCHN)
                     IF ( RES_ARR(J27,IOBS,1)*SNR_CHN > RES_PHAS_MAX  .AND. &
     &                    NUM_USED_OBS(J24,1) >  PIM%CONF%BPS_MINOBS_FINE .AND. &
     &                    FL_OBS(IOBS,1) ) THEN
!
                          RES_PHAS_MAX = RES_ARR(J27,IOBS,1)*SNR_CHN
                          IND_PHAS(1) = J24  ! station
                          IND_PHAS(2) = J27  ! freq ind
                          IND_PHAS(3) = IOBS ! obs ind
                     END IF
                     IF ( RES_ARR(J27,IOBS,2)*SNR_CHN > RES_AMPL_MAX .AND. &
     &                    NUM_USED_OBS(J24,2) >  PIM%CONF%BPS_MINOBS_FINE .AND. &
     &                    FL_OBS(IOBS,2) ) THEN
!
                          RES_AMPL_MAX = RES_ARR(J27,IOBS,2)*SNR_CHN
                          IND_AMPL(1) = J24  ! station
                          IND_AMPL(2) = J27  ! freq ind
                          IND_AMPL(3) = IOBS ! obs ind
                     END IF
                     IF ( PIM%CONF%DEBUG_LEVEL .GE. 3  .AND. J23 == 1 ) THEN
                          WRITE ( UNIT=STR, FMT=250 ) PIM%CONF%BEG_FRQ + J27-1, &
     &                                                RES_ARR(J27,IOBS,1), &
     &                                                RES_ARR(J27,IOBS,2)
 250                      FORMAT ( 'Frq: ',I2, 2X, F5.3, 1X, F5.3 )
                     END IF
                     OUT = OUT(1:I_LEN(OUT))//'  '//STR
 4270             CONTINUE
                  IF ( PIM%CONF%DEBUG_LEVEL .GE. 3  .AND.  J23 == 1 ) THEN
                       WRITE ( 6, '(A)' ) OUT(1:I_LEN(OUT))
                  END IF
 4260          CONTINUE
 4250       CONTINUE
 4240    CONTINUE
         FL_REJECT_PHAS = .FALSE.
         FL_REJECT_AMPL = .FALSE.
         IF ( IND_PHAS(1) == 0  .OR.  IND_AMPL(1) == 0 ) GOTO 8230
         IF ( RES_PHAS_MAX > PIM%CONF%BPS_PHAS_REJECT  .AND. &
     &        NUM_USED_OBS(IND_PHAS(1),1) >  PIM%CONF%BPS_MINOBS_FINE ) THEN
!
              FL_REJECT_PHAS = .TRUE.
              ISTA = IND_PHAS(1)
              IFRQ = IND_PHAS(2)
              IOBS = IND_PHAS(3)
              IEQU = IND_EQU(IOBS) + (IFRQ-1)*2*KCHN - 2
              NUM_USED_OBS(ISTA,1) = NUM_USED_OBS(ISTA,1) - 1
              DO 4290 J29=1,KCHN
                 IEQU = IEQU + 2
!
! -------------- Update the normal matrix and normal vector by removing
! -------------- the IEQU -th observation
!
                 CALL DIAD_CVT_S ( -WEI_VEC(IEQU)**2, L_PAR, &
     &                              EQU_MAT(1,IEQU), EQU_MAT(1,IEQU), NOR_MAT )
                 CALL DAXPY ( L_PAR, -WEI_VEC(IEQU)**2*EQU_VEC(IEQU), &
     &                        EQU_MAT(1,IEQU), 1, NOR_VEC, 1 )
 4290         CONTINUE
!
! ----------- Mark this observation as a phase  outlier
!
              FL_OBS(IOBS,1) = .FALSE.
              IF ( PIM%CONF%DEBUG_LEVEL .GE. 3 ) THEN
                   SNR_CHN = 1.0D0/DSQRT( 1.0D0/SNR_ARR(IOBS)**2 + 1.0D0/SNR_FLOOR**2)/ &
     &                       DSQRT(1.0D0*LCHN*KCHN)
!
                   WRITE ( 6, 260 ) 'Phas', IND_OBS(IOBS), &
     &                              PIM%C_STA(PIM%OBS(IND_OBS(IOBS))%STA_IND(1)), &
     &                              PIM%C_STA(PIM%OBS(IND_OBS(IOBS))%STA_IND(2)), &
     &                              RES_ARR(IFRQ,IOBS,1)*SNR_CHN
 260               FORMAT ( 'PBP   Removed  BAD ', A, ' Obs # ', I6, 2X, A, '/', A, &
     &                      '  factor: ', F10.3 )
             END IF
         END IF
         IF ( RES_AMPL_MAX > PIM%CONF%BPS_AMPL_REJECT  .AND. &
     &        NUM_USED_OBS(IND_AMPL(1),2) >  PIM%CONF%BPS_MINOBS_FINE ) THEN
!
              FL_REJECT_AMPL = .TRUE.
              ISTA = IND_AMPL(1)
              IFRQ = IND_AMPL(2)
              IOBS = IND_AMPL(3)
              IEQU = IND_EQU(IOBS) + (IFRQ-1)*2*KCHN - 1
              NUM_USED_OBS(ISTA,2) = NUM_USED_OBS(ISTA,2) - 1
              DO 4300 J30=1,KCHN
                 IEQU = IEQU + 2
                 CALL DIAD_CVT_S ( -WEI_VEC(IEQU)**2, L_PAR, &
     &                              EQU_MAT(1,IEQU), EQU_MAT(1,IEQU), NOR_MAT )
                 CALL DAXPY ( L_PAR, -WEI_VEC(IEQU)**2*EQU_VEC(IEQU), &
     &                        EQU_MAT(1,IEQU), 1, NOR_VEC, 1 )
 4300         CONTINUE
!
! ----------- Mark this observation as an amplitude outlier
!
              FL_OBS(IOBS,2) = .FALSE.
              IF ( PIM%CONF%DEBUG_LEVEL .GE. 3 ) THEN
                   SNR_CHN = 1.0D0/DSQRT( 1.0D0/SNR_ARR(IOBS)**2 + 1.0D0/SNR_FLOOR**2)/ &
     &                       DSQRT(1.0D0*LCHN*KCHN)
                   WRITE ( 6, 260 ) 'AMPL', IND_OBS(IOBS), &
     &                              PIM%C_STA(PIM%OBS(IND_OBS(IOBS))%STA_IND(1)), &
     &                              PIM%C_STA(PIM%OBS(IND_OBS(IOBS))%STA_IND(2)), &
     &                              RES_ARR(IFRQ,IOBS,2)*SNR_CHN
              END IF
         END IF
         IF ( FL_REJECT_PHAS .OR. FL_REJECT_AMPL ) THEN
!
! ----------- Invert normal matrix
!
              CALL COPY_R8  ( LL_PAR, NOR_MAT, COV_MAT )
              CALL ERR_PASS ( IUER, IER )
              CALL INVS     ( L_PAR, COV_MAT, RC, IER )
              IF ( IER > 0 ) THEN
                   CALL ERR_LOG ( 8477, IUER, 'PIMA_PBP_FINE', 'Error '// &
     &                 'in an attempt to invert the normal matrix during '// &
     &                 'solution update for removing the outlier' )
                   GOTO 710
              END IF
!
! ----------- Find vector of parameters estimates
!
              IER=-1
              CALL MUL_MV_SV_V ( L_PAR, COV_MAT, L_PAR, NOR_VEC, &
     &                           L_PAR, EST_VEC, IER )
!
! ----------- Compute the error vector
!
              DO 4310 J31=1,L_PAR
                 LL = (J31*(J31+1))/2
                 ERR_VEC(J31)=DSQRT( COV_MAT(LL) )
 4310         CONTINUE
            ELSE
              GOTO 8230
         END IF
 4230 CONTINUE
 8230 CONTINUE
      IF ( PIM%CONF%DEBUG_LEVEL .GE. 3 ) THEN
           IF ( IND_PHAS(2) > 0 ) THEN
                IFRQ = IND_PHAS(2)
                IOBS = IND_PHAS(3)
                SNR_CHN = 1.0D0/DSQRT( 1.0D0/SNR_ARR(IOBS)**2 + 1.0D0/SNR_FLOOR**2)/ &
     &                          DSQRT(1.0D0*LCHN*KCHN)
!
                WRITE ( 6, 270 ) 'Phas', IND_OBS(IOBS), &
          &                      PIM%C_STA(PIM%OBS(IND_OBS(IOBS))%STA_IND(1)), &
          &                      PIM%C_STA(PIM%OBS(IND_OBS(IOBS))%STA_IND(2)), &
          &                      RES_ARR(IFRQ,IOBS,1)*SNR_CHN
 270            FORMAT ( 'PBP   Remained BAD ', A, ' Obs # ', I6, 2X, A, '/', A, &
          &              '  factor: ', F10.3 )
           END IF
           IF ( IND_AMPL(2) > 0 ) THEN
                IFRQ = IND_AMPL(2)
                IOBS = IND_AMPL(3)
                SNR_CHN = 1.0D0/DSQRT( 1.0D0/SNR_ARR(IOBS)**2 + 1.0D0/SNR_FLOOR**2)/ &
     &                          DSQRT(1.0D0*LCHN*KCHN)
                WRITE ( 6, 270 ) 'Ampl', IND_OBS(IOBS), &
     &                      PIM%C_STA(PIM%OBS(IND_OBS(IOBS))%STA_IND(1)), &
     &                      PIM%C_STA(PIM%OBS(IND_OBS(IOBS))%STA_IND(2)), &
     &                      RES_ARR(IFRQ,IOBS,2)*SNR_CHN
           END IF
      END IF
!
      DO 4320 J32=1,PIM%NSTA
         DO 4330 J33=1,2
            IF ( BPS%NUM_OBS_FINE(J32,J33) .LE. 0 ) GOTO 4330
            IFRQ = 0
            IPAR_REM = IPAR_STA(J32)
            DO 4340 J34=PIM%CONF%BEG_FRQ,PIM%CONF%END_FRQ
               IFRQ = IFRQ + 1
               DO 4350 J35=1,PIM%NCHN
                  PHAS_EST = 0.0D0
                  AMPL_EST = 0.0D0
                  AMPL_EST_LOG = 0.0D0
                  FREQ_BEG(IFRQ) = PIM%FRQ(J34,PIM%CONF%FRQ_GRP)%FREQ
                  FREQ_END(IFRQ) = PIM%FRQ(J34,PIM%CONF%FRQ_GRP)%FREQ + &
     &                                 PIM%FRQ(J34,PIM%CONF%FRQ_GRP)%BAND_WIDTH
!
! --------------- Phase
!
                  IF ( PIM%CONF%BPS_INTRP_METHOD == PIMA__LEGENDRE ) THEN
                       DO 4360 J36=0,PIM%CONF%BPS_DEG_PHS
                          PHAS_EST = PHAS_EST + EST_VEC(IPAR_REM + J36+1)* &
     &                               LEGENDRE_POL ( J36, FREQ_BEG(IFRQ), FREQ_END(IFRQ), &
     &                                              PIM%FREQ_ARR(J35,J34,PIM%CONF%FRQ_GRP) )
 4360                  CONTINUE
                     ELSE IF ( PIM%CONF%BPS_INTRP_METHOD == PIMA__SPLINE ) THEN
                       NOD_PHS_STEP = (FREQ_END(IFRQ) - FREQ_BEG(IFRQ))/(LN_PHS -1)
                       DO 4370 J37=1,LN_PHS
                          ARG_PHS_NOD(J37) = FREQ_BEG(IFRQ) + (J37-1)*NOD_PHS_STEP
 4370                  CONTINUE 
                       ARG_PHS_NOD(1)      = ARG_PHS_NOD(1)      - NOD_FUDGE*NOD_PHS_STEP
                       ARG_PHS_NOD(LN_PHS) = ARG_PHS_NOD(LN_PHS) + NOD_FUDGE*NOD_PHS_STEP
                       DO 4380 J38=1-M_DEG,LN_PHS-1
                          PHAS_EST = PHAS_EST + EST_VEC(IPAR_REM + J38+M_DEG)* &
                                                BSPL_VAL ( LN_PHS, ARG_PHS_NOD, M_DEG, &
     &                                                     J38, PIM%FREQ_ARR(J35,J34,PIM%CONF%FRQ_GRP) )
 4380                  CONTINUE 
                     ELSE IF ( PIM%CONF%BPS_INTRP_METHOD == PIMA__LINEAR ) THEN
                       EQU_MAT(IPAR_REM+1,IEQU) = 1.0
                       EQU_MAT(IPAR_REM+2,IEQU) = (PIM%FREQ_ARR(J35,J34,PIM%CONF%FRQ_GRP) -  &
     &                                             PIM%FREQ_ARR(1,J34,PIM%CONF%FRQ_GRP)      )/ &
     &                                            (PIM%FREQ_ARR(PIM%NCHN,J34,PIM%CONF%FRQ_GRP) - &
     &                                             PIM%FREQ_ARR(1,J34,PIM%CONF%FRQ_GRP))
                  END IF
!
! --------------- Amplitude
!
                  IF ( PIM%CONF%BPS_INTRP_METHOD == PIMA__LEGENDRE ) THEN
                       DO 4390 J39=0,PIM%CONF%BPS_DEG_AMP
                          AMPL_EST_LOG = AMPL_EST_LOG + EST_VEC(IPAR_REM+PIM%CONF%BPS_DEG_AMP+1 + J39+1)* &
     &                                   LEGENDRE_POL ( J39, FREQ_BEG(IFRQ), FREQ_END(IFRQ), &
     &                                                  PIM%FREQ_ARR(J35,J34,PIM%CONF%FRQ_GRP) )
 4390                  CONTINUE
                     ELSE IF ( PIM%CONF%BPS_INTRP_METHOD == PIMA__SPLINE ) THEN
                       DO 4400 J40=1,LN_AMP
                          SEQ_ARG = (J40-1)/DBLE(LN_AMP-1)
                          IF ( SEQ_ARG < 0.5D0 ) THEN
                               ARG_AMP_NOD(J40) = FREQ_BEG(IFRQ) + (FREQ_END(IFRQ) - FREQ_BEG(IFRQ))* &
     &                                                             (1.0D0 - DSQRT(DABS(1.0D0 - 2.D0*SEQ_ARG)))/2.0D0
                             ELSE
                               ARG_AMP_NOD(J40) = FREQ_BEG(IFRQ) + (FREQ_END(IFRQ) - FREQ_BEG(IFRQ))* &
     &                                                             (1.0D0 + DSQRT(DABS(1.0D0 - 2.D0*SEQ_ARG)))/2.0D0
                          END IF    
 4400                  CONTINUE 
                       NOD_AMP_STEP        = ARG_AMP_NOD(LN_AMP) - ARG_AMP_NOD(LN_AMP-1)
                       ARG_AMP_NOD(1)      = ARG_AMP_NOD(1)      - NOD_FUDGE*NOD_AMP_STEP
                       ARG_AMP_NOD(LN_AMP) = ARG_AMP_NOD(LN_AMP) + NOD_FUDGE*NOD_AMP_STEP
                       DO 4410 J41=1-M_DEG,LN_AMP-1
                          AMPL_EST_LOG = AMPL_EST_LOG + EST_VEC(IPAR_REM+LN_PHS+M_DEG-1 + J41+M_DEG)* &
                                              BSPL_VAL ( LN_AMP, ARG_AMP_NOD, M_DEG, &
     &                                                   J41, PIM%FREQ_ARR(J35,J34,PIM%CONF%FRQ_GRP) )
 4410                  CONTINUE
                     ELSE IF ( PIM%CONF%BPS_INTRP_METHOD == PIMA__LINEAR ) THEN
                       EQU_MAT(IPAR_REM+3,IEQU) = 1.0
                  END IF
                  IF ( AMPL_EST_LOG < -20.0 ) THEN
                       AMPL_EST = DEXP ( -20.0D0 )
                     ELSE IF ( AMPL_EST_LOG > 10.0 ) THEN
                       AMPL_EST = DEXP (  10.0D0 )
                     ELSE 
                       AMPL_EST = DEXP ( AMPL_EST_LOG )
                  END IF
                  IF ( PIM%CONF%DEBUG_LEVEL .GE. 4 ) THEN
                       WRITE  ( 6, 280 ) PIM%C_STA(J32), J34, J35, PHAS_EST, AMPL_EST, AMPL_EST_LOG
 280                   FORMAT ( 'PBF_fine Sta: ', A, ' Ifrq: ', I3, ' Ichn: ', I3, &
     &                          ' Phas: ', F8.4,' Ampl: ', F8.4, ' ampl_est_log= ', 1pd12.5 )
                  END IF
                  CORR_EST_CMPL = CMPLX ( AMPL_EST*COS(PHAS_EST), &
     &                                    AMPL_EST*SIN(PHAS_EST) )
                  PIM%PBP(J32)%CMPL(J35,J34) = PIM%PBP(J32)%CMPL(J35,J34)* CORR_EST_CMPL
!
 4350          CONTINUE
               IF ( PIM%CONF%DEBUG_LEVEL .GE. 4 ) THEN
                    WRITE  ( 6, '(A)' ) '#'
               END IF
               IF ( PIM%CONF%BPS_INTRP_METHOD == PIMA__LEGENDRE ) THEN
                    IPAR_REM = IPAR_REM + (LN_PHS + LN_AMP + 2)
                  ELSE IF ( PIM%CONF%BPS_INTRP_METHOD == PIMA__SPLINE ) THEN
                    IPAR_REM = IPAR_REM + (LN_PHS + LN_AMP + 2*(M_DEG-1))
                  ELSE IF ( PIM%CONF%BPS_INTRP_METHOD == PIMA__LINEAR ) THEN
                    IPAR_REM = IPAR_REM + (LN_PHS + LN_AMP)
               END IF
 4340       CONTINUE
            IF ( PIM%CONF%DEBUG_LEVEL .GE. 4 ) THEN
                 WRITE  ( 6, '(A)' ) '#'
            END IF
 4330    CONTINUE
 4320 CONTINUE
!
      CALL ERR_LOG ( 0, IUER )
 710  CONTINUE 
      IF ( ALLOCATED ( SNR_ARR ) ) DEALLOCATE ( SNR_ARR )
      IF ( ALLOCATED ( FL_OBS  ) ) DEALLOCATE ( FL_OBS )
      IF ( ALLOCATED ( SNR_NEW ) ) DEALLOCATE ( SNR_NEW )
      IF ( ALLOCATED ( IND_OBS ) ) DEALLOCATE ( IND_OBS )
      IF ( ALLOCATED ( IND_EQU ) ) DEALLOCATE ( IND_EQU )
      IF ( ALLOCATED ( RES_ARR ) ) DEALLOCATE ( RES_ARR )
      IF ( ALLOCATED ( CNS_SIG ) ) DEALLOCATE ( CNS_SIG )
      IF ( ALLOCATED ( AC_AVR  ) ) DEALLOCATE ( AC_AVR )
      IF ( ALLOCATED ( RES     ) ) DEALLOCATE ( RES )
      IF ( ALLOCATED ( EST_VEC ) ) DEALLOCATE ( EST_VEC )
      IF ( ALLOCATED ( WEI_VEC ) ) DEALLOCATE ( WEI_VEC )
      IF ( ALLOCATED ( ERR_VEC ) ) DEALLOCATE ( ERR_VEC )
      IF ( ALLOCATED ( COV_MAT ) ) DEALLOCATE ( COV_MAT )
      IF ( ALLOCATED ( NOR_VEC ) ) DEALLOCATE ( NOR_VEC )
      IF ( ALLOCATED ( NOR_MAT ) ) DEALLOCATE ( NOR_MAT )
      IF ( ALLOCATED ( EQU_VEC ) ) DEALLOCATE ( EQU_VEC )
      IF ( ALLOCATED ( EQU_MAT ) ) DEALLOCATE ( EQU_MAT )
!
      RETURN
      END  SUBROUTINE  PIMA_PBP_FINE  !#!#
