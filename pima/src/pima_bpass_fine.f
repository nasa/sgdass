      SUBROUTINE PIMA_BPASS_FINE ( PIM, VTD, BPS, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine PIMA_BPASS_FINE computes the station-dependent bandpass    *
! *   in the initial mode.                                               *
! *                                                                      *
! * ### 14-NOV-2009  PIMA_BPASS_FINE v3.3 (c) L. Petrov  03-JUN-2021 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'astro_constants.i'
      INCLUDE   'pima.i'
      INCLUDE   'vtd.i'
      TYPE     ( PIMA__TYPE         ) :: PIM
      TYPE     ( PIM_BPS_STA__TYPE  ) :: BPS
      TYPE     ( VTD__TYPE          ) :: VTD
      INTEGER*4  IUER
      INTEGER*4  LCHN, LFRQ, KCHN, ICHN, JCHN, J1, J2, J3, J4, J5, J6, J7, &
     &           J8, J9, J10, J11, J12, J13, J14, J15, J16, J17, J18, J19, &
     &           J20, J21, J22, J23, J24, J25, J26, J27, J28, J29, J30, &
     &           J31, J32, J33, J34, J35, LTIM, LCHN_SEG, IND_STA(2), &
     &           L_EQU, L_PAR, LL_PAR, L_OBS, IOBS, IEQU, IAMB, IFRQ, &
     &           IPAR, ISTA, L_EQU_ORIG, IND_PHAS(3), IND_AMPL(3), LL, &
     &           IND_FRA, LN_AMP, LN_PHS, NUSED_OBS_STA(PIM__MSTA), &
     &           POL_MODE, IND_REF, IND_REM, ICHN_1ST(PIM__MFRQ), PCI, &
     &           IND_POL, IER
      COMPLEX*8  RES_AVR(2), RES_CMPL, CORR_EST_CMPL
      CHARACTER  POLAR_SAVE*8, STR*128, STR1*128, STR2*128, OUT*1024
      REAL*8,    ALLOCATABLE :: EQU_MAT(:,:), EQU_VEC(:), &
     &                          NOR_MAT(:),   NOR_VEC(:), &
     &                          COV_MAT(:),   ERR_VEC(:), &
     &                          WEI_VEC(:),   EST_VEC(:), CNS_SIG(:), &
     &                          RES_ARR(:,:,:), SNR_ARR(:)
      INTEGER*4, ALLOCATABLE :: IND_EQU(:), IND_OBS(:)
      REAL*8,    ALLOCATABLE :: SNR_NEW(:)
      REAL*4,    ALLOCATABLE :: AC_AVR(:,:,:)
      COMPLEX*8, ALLOCATABLE :: RES(:,:)
      INTEGER*4  MASK_CHN
      LOGICAL*1, ALLOCATABLE :: FL_OBS(:,:)
      LOGICAL*1  FL_BMASK, FL_STA, FL_REJECT_PHAS, FL_REJECT_AMPL
      REAL*8     FREQ_CHN(PIM__MCHN), PHAS_RES(PIM__MCHN), AMPL_RES(PIM__MCHN), &
     &           FREQ_ALL_CHN(PIM__MCHN*PIM__MFRQ), &
     &           PHAS_ALL_RES(PIM__MCHN*PIM__MFRQ), &
     &           AMPL_ALL_RES(PIM__MCHN*PIM__MFRQ), &
     &           FREQ_BEG(PIM__MFRQ), FREQ_END(PIM__MFRQ), &
     &           RC, SIG, PHAS_EST, &
     &           AMPL_EST, AMPL_EST_LOG, RES_AMPL, RES_PHAS, WW_ACC(2), &
     &           SNR_CHN, RES_PHAS_MAX, RES_AMPL_MAX, &
     &           GR_DEL(PIM__MFRA), PH_RAT(PIM__MFRA), PHS(PIM__MFRA), SEQ_ARG
      CHARACTER  POL_CONF*7
      INTEGER*4  M_DEG
      REAL*4     BPS_AMP_AVR, AMPL_FRQ_NRML(PIM__MFRQ), AMPL_BAND_NRML, &
     &           BPS_AMP, BPS_PHAS
      REAL*8     PHAS_CNS_SIG, AMPL_CNS_SIG, SNR_FLOOR, WEI__MIN, NOD_FUDGE, &
     &           PHS_AVR_FRQ, GR_DEL_FRQ 
      REAL*8     SUM_WEI, DELTA, DELTA_SQR, PHASE_AVR, PHASE_RMS, WEI_SCL
      INTEGER*1  SGN_REF, SGN_REM
      PARAMETER  ( M_DEG = 3 )
      PARAMETER  ( PHAS_CNS_SIG = 10.0D0   ) ! Good range 5-50
      PARAMETER  ( AMPL_CNS_SIG = 10.0D0   )
      PARAMETER  ( SNR_FLOOR    = 128.0D0  )
      PARAMETER  ( WEI__MIN     = 1.D-15   )
      PARAMETER  ( NOD_FUDGE    = 1.D-3    )
      PARAMETER  ( WEI_SCL      = 30.0D0   )
      REAL*8     NOD_AMP_STEP, ARG_AMP_NOD(PIM__MCHN), &
     &           NOD_PHS_STEP, ARG_PHS_NOD(PIM__MCHN)
      INTEGER*4  K_RES, IPAR_STA(PIM__MSTA), NUM_USED_OBS(PIM__MSTA,2)
!
      CHARACTER, EXTERNAL :: GET_CDATE*19, PIMA_GET_POL_CONF*7
      REAL*8,    EXTERNAL :: DP_VV_V, LEGENDRE_POL, BSPL_VAL
      REAL*4,    EXTERNAL :: PHAS_CMPL_R4
      LOGICAL*4, EXTERNAL :: IS_R4_NAN 
      INTEGER*4, EXTERNAL :: ILEN, I_LEN, LTM_DIF
!
      LCHN = PIM%NCHN
      LFRQ = PIM%CONF%END_FRQ - PIM%CONF%BEG_FRQ + 1
      IF ( PIM%CONF%BPS_MSEG_FINE .LE. 0 ) PIM%CONF%BPS_MSEG_FINE = 1
      KCHN = PIM%NCHN/PIM%CONF%BPS_MSEG_FINE
!
      IF ( PIM%CONF%DEBUG_LEVEL .GE. 3 ) THEN
           WRITE ( 6, * ) '--------------------------- ' 
           WRITE ( 6, * ) ' PIMA_BPASS_FINE            ' 
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
           LN_PHS = 2
           LN_AMP = 1
      END IF
!
      DO 410 J1=1,PIM%NSTA
         PCI = PIM%BPS%PCI(J1)
         IF ( PCI < 1 ) GOTO 410
         IF ( BPS%NUM_OBS_FINE(J1,PCI) .LE. 0 ) GOTO 410
         FL_STA = .FALSE.
         DO 420 J2=1,BPS%NUM_OBS_FINE(J1,PCI)
            IF ( DABS(BPS%SNR(J2,J1,PCI)) < PIM%CONF%BPS_SNR_MIN_FINE ) GOTO 420
            NUM_USED_OBS(J1,1) = NUM_USED_OBS(J1,1) + 1
            NUM_USED_OBS(J1,2) = NUM_USED_OBS(J1,2) + 1
            IF ( .NOT. FL_STA ) THEN
                 IPAR_STA(J1) = L_PAR
                 IF ( PIM%CONF%BPS_INTRP_METHOD == PIMA__LEGENDRE ) THEN
                      L_PAR = L_PAR + (LN_PHS + LN_AMP + 2)*LFRQ
                    ELSE IF ( PIM%CONF%BPS_INTRP_METHOD == PIMA__SPLINE ) THEN
                      L_PAR = L_PAR + (LN_PHS + LN_AMP + 2*(M_DEG-1))*LFRQ
                    ELSE IF ( PIM%CONF%BPS_INTRP_METHOD == PIMA__LINEAR ) THEN
                      L_PAR = L_PAR + (LN_PHS + LN_AMP)*LFRQ
                 END IF
                 FL_STA = .TRUE.
            END IF
            L_OBS = L_OBS + 1
            L_EQU = L_EQU + 2*LFRQ*KCHN
 420     CONTINUE
 410  CONTINUE
      LL_PAR = (L_PAR*(L_PAR+1))/2
      IF ( PIM%CONF%BANDPASS_MASK_FILE .NE. PIMA__BPASS_NO  .AND. &
     &     ASSOCIATED ( PIM%BANDPASS_MASK )                       ) THEN
           FL_BMASK = .TRUE.
         ELSE
           FL_BMASK = .FALSE.
      END IF
!
      IF ( PIM%CONF%DEBUG_LEVEL .GE. 2  ) THEN
           WRITE ( 6, 100 ) L_PAR, L_EQU, L_OBS
 100       FORMAT ( 'PIMA_STA_FINE   L_PAR: ', I5,' L_EQU: ', I7, ' L_OBS: ', I7 )
           CALL FLUSH ( 6 )
      END IF
      ALLOCATE ( EQU_MAT(L_PAR,L_EQU), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH  ( STR )
           CALL IINCH8 ( INT8(8)*INT8(L_EQU)*INT8(L_PAR), STR )
           CALL ERR_LOG ( 8311, IUER, 'PIMA_BPASS_FINE', 'Failure to '// &
     &          'allocate '//STR(1:I_LEN(STR))//' bytes of dynamic memory '// &
     &          'for matrix EQU_MAT' )
           RETURN
      END IF
      CALL NOUT_R8 ( L_EQU*L_PAR, EQU_MAT )
!
      ALLOCATE ( EQU_VEC(L_EQU), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( 8*L_EQU, STR )
           CALL ERR_LOG ( 8312, IUER, 'PIMA_BPASS_FINE', 'Failure to '// &
     &          'allocate '//STR(1:I_LEN(STR))//' bytes of dynamic memory '// &
     &          'for vector EQU_VEC' )
           RETURN
      END IF
      CALL NOUT_R8 ( L_EQU, EQU_VEC )
!
      ALLOCATE ( NOR_MAT(LL_PAR), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( 8*LL_PAR, STR )
           CALL ERR_LOG ( 8313, IUER, 'PIMA_BPASS_FINE', 'Failure to '// &
     &          'allocate '//STR(1:I_LEN(STR))//' bytes of dynamic memory '// &
     &          'for matrix NOR_MAT' )
           RETURN
      END IF
      CALL NOUT_R8 ( LL_PAR, NOR_MAT )
!
      ALLOCATE ( NOR_VEC(L_PAR), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( 8*L_PAR, STR )
           CALL ERR_LOG ( 8314, IUER, 'PIMA_BPASS_FINE', 'Failure to '// &
     &          'allocate '//STR(1:I_LEN(STR))//' bytes of dynamic memory '// &
     &          'for vector NOR_VEC' )
           RETURN
      END IF
      CALL NOUT_R8 ( L_PAR, NOR_VEC )
!
      ALLOCATE ( COV_MAT(LL_PAR), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( 8*LL_PAR, STR )
           CALL ERR_LOG ( 8315, IUER, 'PIMA_BPASS_FINE', 'Failure to '// &
     &          'allocate '//STR(1:I_LEN(STR))//' bytes of dynamic memory '// &
     &          'for matrix COV_MAT' )
           RETURN
      END IF
      CALL NOUT_R8 ( LL_PAR, COV_MAT )
!
      ALLOCATE ( ERR_VEC(L_PAR), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( 8*L_PAR, STR )
           CALL ERR_LOG ( 8316, IUER, 'PIMA_BPASS_FINE', 'Failure to '// &
     &          'allocate '//STR(1:I_LEN(STR))//' bytes of dynamic memory '// &
     &          'for vector ERR_VEC' )
           RETURN
      END IF
      CALL NOUT_R8 ( L_PAR, ERR_VEC )
!
      ALLOCATE ( WEI_VEC(L_EQU), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( 8*L_EQU, STR )
           CALL ERR_LOG ( 8317, IUER, 'PIMA_BPASS_FINE', 'Failure to '// &
     &          'allocate '//STR(1:I_LEN(STR))//' bytes of dynamic memory '// &
     &          'for vector WEI_VEC' )
           RETURN
      END IF
      CALL NOUT_R8 ( L_EQU, WEI_VEC )
!
      ALLOCATE ( EST_VEC(L_PAR), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( 8*L_EQU, STR )
           CALL ERR_LOG ( 8318, IUER, 'PIMA_BPASS_FINE', 'Failure to '// &
     &          'allocate '//STR(1:I_LEN(STR))//' bytes of dynamic memory '// &
     &          'for vector EST_VEC' )
           RETURN
      END IF
      CALL NOUT_R8 ( L_PAR, EST_VEC )
!
      ALLOCATE ( RES(LCHN,PIM%NFRQ), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR  )
           CALL INCH  ( 8*LCHN*PIM%NFRQ, STR )
           CALL ERR_LOG ( 8319, IUER, 'PIMA_BPASS_FINE', 'Error in an '// &
     &         'attempt to allocate '//STR(1:I_LEN(STR))//' bytes of '// &
     &         'dynamic memory for residuals' )
           RETURN
      END IF
      CALL NOUT_R4 ( 2*LCHN*PIM%NFRQ, RES )
!
      ALLOCATE ( AC_AVR(LCHN,PIM%NFRQ,2), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR  )
           CALL INCH  ( 8*LCHN*LFRQ, STR )
           CALL ERR_LOG ( 8320, IUER, 'PIMA_BPASS_FINE', 'Error in an '// &
     &         'attempt to allocate '//STR(1:I_LEN(STR))//' bytes of '// &
     &         'dynamic memory for array autspectrum AC_AVR' )
           RETURN
      END IF
      CALL NOUT_R4 ( 2*LCHN*PIM%NFRQ, AC_AVR )
!
      ALLOCATE ( CNS_SIG(L_PAR), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( 8*L_PAR, STR )
           CALL ERR_LOG ( 8321, IUER, 'PIMA_BPASS_FINE', 'Failure to '// &
     &          'allocate '//STR(1:I_LEN(STR))//' bytes of dynamic memory '// &
     &          'for vector CNS_SIG' )
           RETURN
      END IF
      CALL NOUT_R8 ( L_PAR, CNS_SIG )
!
      ALLOCATE ( RES_ARR(LFRQ,L_OBS,2), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( 8*L_OBS, STR )
           CALL ERR_LOG ( 8322, IUER, 'PIMA_BPASS_FINE', 'Failure to '// &
     &          'allocate '//STR(1:I_LEN(STR))//' bytes of dynamic memory '// &
     &          'for vector RES_ARR' )
           RETURN
      END IF
      CALL NOUT_R8 ( LFRQ*L_OBS*2, RES_ARR )
!
      ALLOCATE ( IND_EQU(L_OBS), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( 8*L_OBS, STR )
           CALL ERR_LOG ( 8323, IUER, 'PIMA_BPASS_FINE', 'Failure to '// &
     &          'allocate '//STR(1:I_LEN(STR))//' bytes of dynamic memory '// &
     &          'for vector IND_EQU' )
           RETURN
      END IF
      CALL NOUT_I4 ( L_OBS, IND_EQU )
!
      ALLOCATE ( IND_OBS(L_OBS), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( 8*L_OBS, STR )
           CALL ERR_LOG ( 8324, IUER, 'PIMA_BPASS_FINE', 'Failure to '// &
     &          'allocate '//STR(1:I_LEN(STR))//' bytes of dynamic memory '// &
     &          'for vector IND_OBS' )
           RETURN
      END IF
      CALL NOUT_I4 ( L_OBS, IND_OBS )
!
      ALLOCATE ( SNR_NEW(L_OBS), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( 8*L_OBS, STR )
           CALL ERR_LOG ( 8325, IUER, 'PIMA_BPASS_FINE', 'Failure to '// &
     &          'allocate '//STR(1:I_LEN(STR))//' bytes of dynamic memory '// &
     &          'for vector SNR_NEW' )
           RETURN
      END IF
      CALL NOUT_I4 ( L_OBS, IND_OBS )
!
      ALLOCATE ( FL_OBS(L_OBS,2), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( 8*L_OBS, STR )
           CALL ERR_LOG ( 8326, IUER, 'PIMA_BPASS_FINE', 'Failure to '// &
     &          'allocate '//STR(1:I_LEN(STR))//' bytes of dynamic memory '// &
     &          'for vector FL_OBS' )
           RETURN
      END IF
      CALL NOUT ( L_OBS*2, FL_OBS )
!
      ALLOCATE ( SNR_ARR(L_OBS), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( 8*L_OBS, STR )
           CALL ERR_LOG ( 8327, IUER, 'PIMA_BPASS_FINE', 'Failure to '// &
     &          'allocate '//STR(1:I_LEN(STR))//' bytes of dynamic memory '// &
     &          'for vector SNR_ARR' )
           RETURN
      END IF
      CALL NOUT_R8 ( L_OBS, SNR_ARR )
!
      IOBS = 0
      IEQU = 0
      DO 430 J3=1,PIM%NSTA
         PCI = PIM%BPS%PCI(J3)
         IF ( PCI == 0 ) GOTO 430
         IF ( BPS%NUM_OBS_FINE(J3,PCI) .LE. 0 ) GOTO 430
         NUSED_OBS_STA(J3) = 0
         DO 440 J4=1,BPS%NUM_OBS_FINE(J3,PCI)
            IF ( BPS%SNR(J4,J3,PCI) < 0.0 ) BPS%SNR(J4,J3,PCI) = -BPS%SNR(J4,J3,PCI) 
            IF ( BPS%SNR(J4,J3,PCI) < PIM%CONF%BPS_SNR_MIN_FINE ) GOTO 440
            IF ( BPS%SNR(J4,J3,PCI) < PIM%CONF%BPS_NOBS_FINE    ) GOTO 440
            IOBS = IOBS + 1
            IND_OBS(IOBS) = BPS%IND_OBS_SEL(J4,J3,PCI)
            IF ( PIM%OBS(IND_OBS(IOBS))%REF_FRG_INDS(PIM%CONF%FRQ_GRP) == 0 ) THEN
                 GOTO 440
            END IF
            NUSED_OBS_STA(J3) = NUSED_OBS_STA(J3) + 1
            IND_STA(1) = PIM%OBS(IND_OBS(IOBS))%STA_IND(1)
            IND_STA(2) = PIM%OBS(IND_OBS(IOBS))%STA_IND(2)
!
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
            CALL ERR_PASS ( IUER, IER )
            POL_CONF = PIMA_GET_POL_CONF ( PIM, IND_OBS, IER ) 
            IF ( IER .NE. 0 ) THEN
                 CALL ERR_LOG ( 7588, IUER, 'PIMA_BPASS_ACCUM', 'Trap of internal conotrol ' )
                 CALL EXIT ( 1 )
                 RETURN
            END IF
!
            GR_DEL(IND_FRA) = BPS%GR_DEL(J4,J3,PCI)
            PH_RAT(IND_FRA) = BPS%PH_RAT(J4,J3,PCI)
            PHS(IND_FRA) = BPS%PHS(J4,J3,PCI)
!
! --------- Get residuals using phase bandpass computed in the ACCUM mode
!
            SNR_NEW(IOBS) = DABS(BPS%SNR(J4,J3,PCI))
            LTIM = PIM%OBS(IND_OBS(IOBS))%NUM_EPC(PIM%OBS(IND_OBS(IOBS))%REF_FRG_INDS(PIM%CONF%FRQ_GRP))
!
            CALL ERR_PASS  ( IUER, IER )
            PIM%BPASS(IND_STA(IND_REM))%BPS(1:PIM%NCHN,1:PIM%NFRQ) = BPS%CMPL(1:PIM%NCHN,1:PIM%NFRQ,IND_STA(IND_REM))
            IF ( POL_CONF == PIMA__PC_CC ) THEN
                 IF ( PIM%CONF%POLAR == PIMA__POLAR_I ) THEN
                      IF ( PIM%STA(PIM%BPS%IND_STA_REF)%POL_TYP(1) == PIMA__POL_R ) THEN
                           POL_MODE = PIMA__RRCC
                         ELSE
                           POL_MODE = PIMA__LLCC
                      END IF
                    ELSE IF ( PIM%CONF%POLAR == PIMA__POLAR_RR ) THEN
                      POL_MODE = PIMA__RRCC
                    ELSE IF ( PIM%CONF%POLAR == PIMA__POLAR_LL ) THEN
                      POL_MODE = PIMA__LLCC
                    ELSE IF ( PIM%CONF%POLAR == PIMA__POLAR_RL ) THEN
                      POL_MODE = PIMA__RLCC
                    ELSE IF ( PIM%CONF%POLAR == PIMA__POLAR_LR ) THEN
                      POL_MODE = PIMA__LRCC
                 END IF
               ELSE IF ( POL_CONF == PIMA__PC_LL ) THEN
                 POL_MODE = PIMA__HHLL
               ELSE IF ( POL_CONF == PIMA__PC_LC ) THEN
                 POL_MODE = PIMA__HRLC
               ELSE IF ( POL_CONF == PIMA__PC_CL ) THEN
                 POL_MODE = PIMA__RHCL
            END IF
            IND_POL = 1
            CALL PIMA_GET_RESID ( PIM, VTD, IND_OBS(IOBS), IND_POL, &
     &                 LCHN, LFRQ, LTIM, &
     &                 GR_DEL, PH_RAT, BPS%GR_RAT(J4,J3,PCI), PHS, &
     &                 SNR_NEW(IOBS), BPS%TIME_FRT(J4,J3,PCI), &
     &                 PIM%CONF%FRIB_SEARCH_TYPE, PIMA__BPASS_PHS, &
     &                 POL_MODE, RES, RES_AVR, SNR_ARR(IOBS), IER )
            IF ( IER .NE. 0 ) THEN
                 WRITE ( 6, * ) ' IER = ', IER
                 CALL CLRCH ( STR )
                 CALL INCH  ( BPS%IND_OBS_SEL(J4,J3,PCI), STR )
                 CALL ERR_LOG ( 8328, IUER, 'PIMA_BPASS_FINE', 'Error in '// &
     &               'an attempt to compute residuals for observation '// &
     &                STR )
                 RETURN
            END IF
!
            IF ( SNR_ARR(IOBS) < PIM%CONF%FRIB_SNR_DETECTION ) THEN
                 CALL CLRCH ( STR  )
                 CALL CLRCH ( STR1 )
                 CALL CLRCH ( STR2 )
                 CALL INCH ( IND_OBS(IOBS), STR )
                 WRITE  ( UNIT=STR1, FMT='(F8.2)', IOSTAT=IER ) SNR_NEW
                 WRITE  ( UNIT=STR2, FMT='(F8.2)', IOSTAT=IER ) PIM%CONF%FRIB_SNR_DETECTION           
                 CALL ERR_LOG ( 8329, IUER, 'PIMA_BPASS_FINE', 'Observation '// &
     &                STR(1:I_LEN(STR))//' has too low SNR: '//STR1(1:I_LEN(STR1))// &
     &                ' -- lower than the detection limit '//STR2 )
                 RETURN 
            END IF
!
            BPS%GR_DEL(J4,J3,PCI) = GR_DEL(IND_FRA)
            BPS%PH_RAT(J4,J3,PCI) = PH_RAT(IND_FRA)
            BPS%PHS(J4,J3,PCI)    = PHS(IND_FRA)
!
            IPAR = IPAR_STA(J3)
            IFRQ = 0
            IND_EQU(IOBS) = IEQU + 1
            FL_OBS(IOBS,1) = .TRUE.
            FL_OBS(IOBS,2) = .TRUE.
            ICHN_1ST       = 0
            JCHN = 0
            DO 450 J5=PIM%CONF%BEG_FRQ,PIM%CONF%END_FRQ
               IFRQ = IFRQ + 1
               ICHN = 0
               DO 460 J6=1,KCHN ! cycle over spectral channels
                  JCHN = JCHN + 1
                  IF ( ICHN_1ST(J5) == 0 ) THEN
                       ICHN_1ST(J5) = JCHN
                  END IF 
                  FREQ_CHN(J6) = 0.0
                  RES_CMPL     = 0.0
                  BPS_AMP_AVR  = 0.0
                  LCHN_SEG     = 0
                  DO 470 J7=1,PIM%CONF%BPS_MSEG_FINE
                     ICHN = ICHN + 1
                     IF ( FL_BMASK ) THEN
                           MASK_CHN = PIM%BANDPASS_MASK(ICHN,J5,IND_STA(1),PIMA__MASK_BPAS) * &
     &                                PIM%BANDPASS_MASK(ICHN,J5,IND_STA(2),PIMA__MASK_BPAS)
                         ELSE
                           MASK_CHN = 1
                     END IF
                     FREQ_CHN(J6) = FREQ_CHN(J6) + MASK_CHN * PIM%FREQ_ARR(ICHN,J5,PIM%CONF%FRQ_GRP)
                     RES_CMPL     = RES_CMPL     + MASK_CHN * RES(ICHN,IFRQ)
                     LCHN_SEG     = LCHN_SEG     + MASK_CHN 
!
! ------------------ Compute the amplitude of the bandpass in the accumulative mode
!
                     BPS_AMP = ABS(BPS%CMPL( ICHN,J5,PIM%OBS(IND_OBS(IOBS))%STA_IND(1)))* &
     &                         ABS(BPS%CMPL( ICHN,J5,PIM%OBS(IND_OBS(IOBS))%STA_IND(2)))
                     BPS_AMP_AVR = BPS_AMP_AVR + MASK_CHN*BPS_AMP
 470              CONTINUE
!
! --------------- Get the effective frequency of the multi-channel
!
                  IF ( LCHN_SEG > 0 ) THEN
                       FREQ_CHN(J6) = FREQ_CHN(J6)/LCHN_SEG
                       RES_CMPL = RES_CMPL/ABS(RES_AVR(1))/LCHN_SEG
                       BPS_AMP_AVR = BPS_AMP_AVR/LCHN_SEG
                     ELSE
!
! -------------------- All spectral channels were masked out
!
                       RES_CMPL     = CMPLX ( 1.0, 0.0 )
                       BPS_AMP_AVR  = 1.0
                       FREQ_CHN(J6) = PIM%FREQ_ARR(1 + (J6-1)*PIM%CONF%BPS_MSEG_FINE,J5,PIM%CONF%FRQ_GRP)
                  END IF
!
! --------------- Apply the segment averaged amplitude of the bandpass.
! --------------- Thus, AMPL_RES -- is residual AFTER applying accum 
! --------------- bandpass
!
                  IF ( BPS_AMP_AVR > PIMA__WEI_MIN ) THEN
                       AMPL_RES(J6) = ABS(RES_CMPL)/BPS_AMP_AVR
                  END IF
                  IF ( IS_R4_NAN ( REAL(RES_CMPL) ) .OR. &
     &                 IS_R4_NAN ( IMAG(RES_CMPL) )      ) THEN
!
! -------------------- A pathological case
!
                       AMPL_RES(J6) = 1.0
                  END IF
!
! --------------- Get the residual phase
!
                  PHAS_RES(J6) = -PHAS_CMPL_R4 ( RES_CMPL )
!
                  FREQ_ALL_CHN(JCHN) = FREQ_CHN(J6)
                  PHAS_ALL_RES(JCHN) = PHAS_RES(J6)
                  AMPL_ALL_RES(JCHN) = AMPL_RES(J6)
 460           CONTINUE 
               FREQ_BEG(IFRQ) = PIM%FRQ(J5,PIM%CONF%FRQ_GRP)%FREQ
               FREQ_END(IFRQ) = PIM%FRQ(J5,PIM%CONF%FRQ_GRP)%FREQ + &
     &                                  PIM%FRQ(J5,PIM%CONF%FRQ_GRP)%BAND_WIDTH
 450        CONTINUE 
!
! --------- Resolve phase ambiguities
!
            CALL ERR_PASS ( IUER, IER )
            CALL PIMA_AMB_RES_R8 ( JCHN, FREQ_ALL_CHN, PHAS_ALL_RES, AMPL_ALL_RES, &
     &                             PHS_AVR_FRQ, GR_DEL_FRQ, IER  )
            IF ( IER .NE. 0 ) THEN
                 CALL CLRCH ( STR  )
                 CALL CLRCH ( STR1 )
                 CALL INCH  ( J5, STR  )
                 CALL INCH  ( IND_OBS(IOBS), STR1 )
                 CALL ERR_LOG ( 8329, IUER, 'PIMA_BPASS_FINE', 'Error in '// &
     &               'an attempt to resolve phase ambiguity while processing '// &
     &               ' IF '//STR(1:I_LEN(STR))//' of observation '//STR1 )
                 RETURN 
            END IF
!
            IFRQ = 0
            DO 480 J8=PIM%CONF%BEG_FRQ,PIM%CONF%END_FRQ
               IFRQ = IFRQ + 1
               DO 490 J9=1,KCHN ! cycle over spectral channels
                  IEQU = IEQU + 1
                  FREQ_CHN(J9) = FREQ_ALL_CHN(ICHN_1ST(J8)-1+J9)
                  AMPL_RES(J9) = AMPL_ALL_RES(ICHN_1ST(J8)-1+J9)
                  PHAS_RES(J9) = PHAS_ALL_RES(ICHN_1ST(J8)-1+J9) - PHS_AVR_FRQ
                  IAMB         = IDNINT ( PHAS_RES(J9)/PI2 )
                  PHAS_RES(J9) = PHAS_RES(J9) - PI2*IAMB
!
                  IF ( PIM%CONF%BPS_INTRP_METHOD == PIMA__LEGENDRE ) THEN
                       DO 4110 J11=0,LN_PHS
                          EQU_MAT(IPAR+J11+1,IEQU) = LEGENDRE_POL ( J11, FREQ_BEG(IFRQ), &
     &                                                              FREQ_END(IFRQ), FREQ_CHN(J9) )
                          CNS_SIG(IPAR+J11+1) = PHAS_CNS_SIG
 4110                  CONTINUE
                     ELSE IF ( PIM%CONF%BPS_INTRP_METHOD == PIMA__SPLINE ) THEN
!
! -------------------- Generate the equidistant sequence of knots
!
                       NOD_PHS_STEP = (FREQ_END(IFRQ) - FREQ_BEG(IFRQ))/(LN_PHS -1)
                       DO 4120 J12=1,LN_PHS
                          ARG_PHS_NOD(J12) = FREQ_BEG(IFRQ) + (J12-1)*NOD_PHS_STEP
 4120                  CONTINUE 
                       ARG_PHS_NOD(1)      = ARG_PHS_NOD(1)      - NOD_FUDGE*NOD_PHS_STEP
                       ARG_PHS_NOD(LN_PHS) = ARG_PHS_NOD(LN_PHS) + NOD_FUDGE*NOD_PHS_STEP
                       DO 4130 J13=1-M_DEG,LN_PHS-1
                          EQU_MAT(IPAR + J13+M_DEG,IEQU) = BSPL_VAL ( LN_PHS, ARG_PHS_NOD, &
     &                                                              M_DEG, J13, FREQ_CHN(J9) )
                          CNS_SIG(IPAR+J13+M_DEG) = PHAS_CNS_SIG
 4130                  CONTINUE
                     ELSE IF ( PIM%CONF%BPS_INTRP_METHOD == PIMA__LINEAR ) THEN
                       EQU_MAT(IPAR+1,IEQU) = 1.0
                       EQU_MAT(IPAR+2,IEQU) = (FREQ_CHN(J9) - FREQ_CHN(1))/ &
     &                                        (PIM%FREQ_ARR(PIM%NCHN,J8,PIM%CONF%FRQ_GRP) - &
     &                                         PIM%FREQ_ARR(1,J8,PIM%CONF%FRQ_GRP))
                       CNS_SIG(IPAR+1) = PHAS_CNS_SIG
                       CNS_SIG(IPAR+2) = PHAS_CNS_SIG
                  END IF
!
                  IF ( AMPL_RES(J9) < PIMA__AMP_MIN .OR. &
     &                 AMPL_RES(J9) > PIMA__AMP_MAX      ) THEN
                       WEI_VEC(IEQU) = WEI__MIN
                     ELSE
                       WEI_VEC(IEQU) = AMPL_RES(J9)*ABS(RES_AVR(1))
                  END IF
                  EQU_VEC(IEQU) = PHAS_RES(J9)
!
! --------------- Get the residual amplitude
!
                  IEQU = IEQU + 1
                  IF ( PIM%CONF%BPS_INTRP_METHOD == PIMA__LEGENDRE ) THEN
                       DO 4140 J14=0,LN_AMP
                          EQU_MAT(IPAR+LN_PHS+1 + J14+1,IEQU) = &
     &                        LEGENDRE_POL ( J14, FREQ_BEG(IFRQ), FREQ_END(IFRQ), FREQ_CHN(J9) )
                          CNS_SIG(IPAR+LN_PHS+1 + J14+1) = AMPL_CNS_SIG
 4140                  CONTINUE
                     ELSE IF ( PIM%CONF%BPS_INTRP_METHOD == PIMA__SPLINE ) THEN
!
! -------------------- Generate the sequence of knots according to the square root rule
!
                       NOD_AMP_STEP = (FREQ_END(IFRQ) - FREQ_BEG(IFRQ))/(LN_AMP -1)
                       DO 4150 J15=1,LN_AMP
                          SEQ_ARG = (J15-1)/DBLE(LN_AMP-1)
                          IF ( SEQ_ARG < 0.5D0 ) THEN
                               ARG_AMP_NOD(J15) = FREQ_BEG(IFRQ) + (FREQ_END(IFRQ) - FREQ_BEG(IFRQ))* &
     &                                                             (1.0D0 - DSQRT(DABS(1.0D0 - 2.D0*SEQ_ARG)))/2.0D0
                             ELSE
                               ARG_AMP_NOD(J15) = FREQ_BEG(IFRQ) + (FREQ_END(IFRQ) - FREQ_BEG(IFRQ))* &
     &                                                             (1.0D0 + DSQRT(DABS(1.0D0 - 2.D0*SEQ_ARG)))/2.0D0
                          END IF    
  4150                 CONTINUE 
                       NOD_AMP_STEP        = ARG_AMP_NOD(LN_AMP) - ARG_AMP_NOD(LN_AMP-1)
                       ARG_AMP_NOD(1)      = ARG_AMP_NOD(1) - NOD_FUDGE*NOD_AMP_STEP
                       ARG_AMP_NOD(LN_AMP) = ARG_AMP_NOD(LN_AMP) + NOD_FUDGE*NOD_AMP_STEP
                       DO 4160 J16=1-M_DEG,LN_AMP-1
                          EQU_MAT(IPAR+LN_PHS+M_DEG-1 + J16+M_DEG,IEQU) = BSPL_VAL ( LN_AMP, ARG_AMP_NOD, &
     &                                                                               M_DEG, J16, FREQ_CHN(J9) )
                          CNS_SIG(IPAR+LN_PHS+M_DEG-1 + J16+M_DEG) = AMPL_CNS_SIG
 4160                  CONTINUE
                     ELSE IF ( PIM%CONF%BPS_INTRP_METHOD == PIMA__LINEAR ) THEN
                       EQU_MAT(IPAR+3,IEQU) = 1.0
                       IF ( AMPL_RES(J9) < PIMA__AMP_MIN .OR. &
     &                      AMPL_RES(J9) > PIMA__AMP_MAX      ) THEN
                            WEI_VEC(IEQU) = WEI__MIN
                          ELSE
                            WEI_VEC(IEQU) = AMPL_RES(J9)*ABS(RES_AVR(1))
                       END IF
                       CNS_SIG(IPAR+3) = AMPL_CNS_SIG
                  END IF
!
                  IF ( AMPL_RES(J9) < PIMA__AMP_MIN .OR. &
     &                 AMPL_RES(J9) > PIMA__AMP_MAX      ) THEN
                       WEI_VEC(IEQU) = WEI__MIN
                       EQU_VEC(IEQU) = 0.0D0
                     ELSE
                       EQU_VEC(IEQU) = DLOG(AMPL_RES(J9))
                  END IF
!
                  IF ( AMPL_RES(J9) < PIMA__AMP_MIN .OR. &
     &                 AMPL_RES(J9) > PIMA__AMP_MAX      ) THEN
                       WEI_VEC(IEQU) = WEI__MIN
                     ELSE
                       WEI_VEC(IEQU) = AMPL_RES(J9)*ABS(RES_AVR(1))
                  END IF
 490           CONTINUE
!
               IF ( PIM%CONF%BPS_INTRP_METHOD == PIMA__LEGENDRE ) THEN
                    IPAR = IPAR + (LN_PHS + LN_AMP + 2)
                  ELSE IF ( PIM%CONF%BPS_INTRP_METHOD == PIMA__SPLINE ) THEN
                    IPAR = IPAR + (LN_PHS + LN_AMP + 2*(M_DEG-1))
                  ELSE IF ( PIM%CONF%BPS_INTRP_METHOD == PIMA__LINEAR ) THEN
                    IPAR = IPAR + (LN_PHS + LN_AMP)
               END IF
 480        CONTINUE
 440     CONTINUE
         IF ( PIM%CONF%DEBUG_LEVEL .GE. 4 ) THEN
              WRITE ( 6, * ) 'PIMA_BPASS_FINE: station '//PIM%C_STA(J3)// &
     &                       ' #obs: ', NUSED_OBS_STA(J3)
         END IF
         IF ( NUSED_OBS_STA(J3) == 0 ) THEN
              CALL ERR_LOG ( 8330, IUER, 'PIMA_BPASS_FINE', 'Cannot compute '// &
     &            'fine bandpass for experiment '//TRIM(PIM%CONF%SESS_CODE)// &
     &            ', because station '//PIM%C_STA(J3)//' has no observations '// &
     &            ' suitable for fine bandpass computation. Suggestions: '// &
     &            'you may need reduce BPS.SNR_MIN_FINE and BPS.SNR_MIN_ACCUM '// &
     &            'parameter or to change the reference station or to eliminate '// &
     &            'all records related to station '//PIM%C_STA(J3)//' in the '//&
     &            'used fringe file '//PIM%CONF%FRINGE_FILE )
              RETURN
         END IF
 430  CONTINUE
!
! --- Rescale the weight
!
      WEI_VEC = WEI_SCL * WEI_VEC
!
! --- Solve the LSQ system with constraints
!
      CALL ERR_PASS ( IUER, IER )
      CALL LSQW_CNS_SAVE ( L_PAR, L_EQU, EQU_MAT, EQU_VEC, WEI_VEC, CNS_SIG, &
     &                     NOR_MAT, NOR_VEC, COV_MAT, EST_VEC, ERR_VEC, &
     &                     RC, IER )
      IF ( IER .NE. 0 ) THEN
           WRITE ( 6, * ) 'L_PAR= ', L_PAR
           WRITE ( 6, * ) 'IPAR_STA= ', IPAR_STA(1:PIM%NSTA)
           CALL CLRCH ( STR )
           CALL INCH  ( L_EQU, STR )
           CALL CLRCH ( STR1 )
           CALL INCH  ( L_PAR, STR1 )
           CALL ERR_LOG ( 8331, IUER, 'PIMA_BPASS_FINE', 'Failure to '// &
     &          'solve the system of '//STR(1:I_LEN(STR))//' normal '// &
     &          'equations with '//STR1(1:I_LEN(STR1))//' unknowns' )
           RETURN
      END IF
      L_EQU_ORIG = L_EQU
!
      IF ( PIM%CONF%DEBUG_LEVEL .GE. 2 ) THEN
           WRITE ( 6, * ) 'PIMA_BPASS_FINE: normal system has been solved'
           CALL FLUSH ( 6 )
      END IF
      IF ( PIM%CONF%DEBUG_LEVEL .GE. 4 ) THEN
           WRITE ( 6, * ) 'PIMA_BPASS_FINE: RC= ', RC
      END IF
!
      DO 4170 J17=1,L_EQU_ORIG
         IOBS = 0
         RES_PHAS_MAX = 0.0D0
         RES_AMPL_MAX = 0.0D0
         IND_AMPL = 0
         IND_PHAS = 0
         DO 4180 J18=1,PIM%NSTA
            PCI = PIM%BPS%PCI(J18)
            IF ( PCI == 0 ) GOTO 4180
            IF ( BPS%NUM_OBS_FINE(J18,PCI) .LE. 0 ) GOTO 4180
            DO 4190 J19=1,BPS%NUM_OBS_FINE(J18,PCI)
               IF ( DABS(BPS%SNR(J19,J18,PCI)) < PIM%CONF%BPS_SNR_MIN_FINE ) GOTO 4190
               IF ( DABS(BPS%SNR(J19,J18,PCI)) < PIM%CONF%BPS_NOBS_FINE    ) GOTO 4190
               IOBS = IOBS + 1
               IND_OBS(IOBS) = BPS%IND_OBS_SEL(J19,J18,PCI)
               IF ( PIM%OBS(IND_OBS(IOBS))%REF_FRG_INDS(PIM%CONF%FRQ_GRP) == 0 ) THEN
                    GOTO 4190
               END IF
               IEQU = IND_EQU(IOBS) - 1
               IPAR = IPAR_STA(J18)
               IF ( PIM%CONF%DEBUG_LEVEL .GE. 3  .AND. J17 == 1 ) THEN
                    CALL CLRCH ( OUT )
                    WRITE ( UNIT=OUT, FMT=110 ) IOBS, &
     &                      PIM%C_STA(PIM%OBS(IND_OBS(IOBS))%STA_IND(1)), &
     &                      PIM%C_STA(PIM%OBS(IND_OBS(IOBS))%STA_IND(2)), &
     &                      IND_OBS(IOBS), DABS(SNR_NEW(IOBS)) ! BPS%SNR(J19,J18)
 110                FORMAT ( 'BPS_RES   # ', I4, 2X, A,'/',A, ' Obs: ', I6, &
     &                       ' SNR: ', F8.2 )
                    CALL FLUSH ( 6 )
               END IF
               DO 4200 J20=1,LFRQ
                  RES_ARR(J20,IOBS,1) = 0.0D0
                  RES_ARR(J20,IOBS,2) = 0.0D0
                  K_RES     = 0
                  PHASE_AVR = 0.0D0
                  PHASE_RMS = 0.0D0
                  SUM_WEI   = 0.0D0
                  WW_ACC(1) = 0.0D0
                  WW_ACC(2) = 0.0D0
                  IFRQ = J20 + PIM%CONF%BEG_FRQ - 1 
                  DO 4210 J21=1,KCHN
!
! ------------------ Check mask
!
                     IF ( FL_BMASK ) THEN
                          ICHN = 1 + (J21-1)*PIM%CONF%BPS_MSEG_FINE
                          IF ( PIM%BANDPASS_MASK(ICHN,IFRQ,J18,PIMA__MASK_BPAS) == 0 ) THEN
                               IEQU = IEQU + 2
                               GOTO 4210
                          END IF
!
                          ICHN = J21*PIM%CONF%BPS_MSEG_FINE
                          IF ( PIM%BANDPASS_MASK(ICHN,IFRQ,J18,PIMA__MASK_BPAS) == 0 ) THEN
                               IEQU = IEQU + 2
                               GOTO 4210
                          END IF
                     END IF
                     IEQU = IEQU + 1
!
! ------------------ Compute rms of phase residuals
!
                     IF ( IEQU .LE. 0  .OR. IEQU .GT. L_EQU ) THEN
                          CALL CLRCH ( STR  )
                          CALL CLRCH ( STR1 )
                          CALL CLRCH ( STR2 )
                          CALL INCH  ( IEQU,  STR  )
                          CALL INCH  ( L_EQU, STR1 )
                          CALL INCH  ( IND_OBS(IOBS), STR2 )
                          CALL ERR_LOG ( 8332, IUER, 'PIMA_BPASS_FINE', &
     &                        'Trap of internal control: IEQU  '// &
     &                         STR(1:I_LEN(STR))//' is out of range '// &
     &                        '[1, '//STR1(1:I_LEN(STR1))//' ] during '// &
     &                        'procesing station '//PIM%STA(J18)%IVS_NAME// &
     &                        ' Observation '//STR2 )
                          RETURN 
                     END IF
                     RES_PHAS = EQU_VEC(IEQU) - &
     &                          DP_VV_V ( L_PAR, EQU_MAT(1,IEQU), EST_VEC )
                     IF ( FL_OBS(IOBS,1) ) THEN
                          K_RES = K_RES + 1
                          SUM_WEI   = SUM_WEI + WEI_VEC(IEQU)
                          DELTA     = WEI_VEC(IEQU)*RES_PHAS
                          DELTA_SQR = WEI_VEC(IEQU)*RES_PHAS**2
!!                          PHASE_AVR = PHASE_AVR + DELTA/SUM_WEI
!!                          PHASE_RMS = PHASE_RMS + DELTA*(RES_PHAS - PHASE_AVR)*WEI_VEC(IEQU)
                          PHASE_AVR = PHASE_AVR + DELTA
                          PHASE_RMS = PHASE_RMS + DELTA_SQR
                          RES_ARR(J20,IOBS,1) = RES_ARR(J20,IOBS,1) + &
     &                                         (RES_PHAS*WEI_VEC(IEQU))**2
                          WW_ACC(1) = WW_ACC(1) + WEI_VEC(IEQU)**2
                     END IF
!
! ------------------ Compute rms of amplitude residuals
!
                     IEQU = IEQU + 1
                     RES_AMPL = EQU_VEC(IEQU) - &
     &                          DP_VV_V ( L_PAR, EQU_MAT(1,IEQU), EST_VEC )
                     IF ( FL_OBS(IOBS,2) ) THEN
                          RES_ARR(J20,IOBS,2) = RES_ARR(J20,IOBS,2) + &
     &                                         (RES_AMPL*WEI_VEC(IEQU))**2
                          WW_ACC(2) = WW_ACC(2) + WEI_VEC(IEQU)**2
                     END IF
 4210             CONTINUE
!
                  IF ( FL_OBS(IOBS,1) ) THEN
                       IF ( WW_ACC(1) > 0 ) THEN
                            DELTA = DSQRT ( RES_ARR(J20,IOBS,1)/WW_ACC(1) ) ! For historical reasons for tests
                            PHASE_AVR = PHASE_AVR/SUM_WEI
                            IF ( PHASE_RMS/SUM_WEI - PHASE_AVR**2 > 0.0 ) THEN
                                 RES_ARR(J20,IOBS,1) = DSQRT ( PHASE_RMS/SUM_WEI - PHASE_AVR**2 )
                               ELSE
                                 RES_ARR(J20,IOBS,1) = 0.001
                                 IF ( K_RES > 1 ) THEN
                                      WRITE ( 6, * ) 'PIMA_BPAS_FINE negative RMS IOBS= ', IOBS, ' IFRQ= ', INT2(IFRQ), ' K_RES= ', INT2(K_RES)
                                 END IF
                            END IF
                          ELSE 
                            RES_ARR(J20,IOBS,1) = 0.0D0
                       END IF
                     ELSE
                       RES_ARR(J20,IOBS,1) = 0.0
                  END IF
                  IF ( FL_OBS(IOBS,2) ) THEN
                       IF ( WW_ACC(2) > 0 ) THEN
                            RES_ARR(J20,IOBS,2) = DSQRT ( RES_ARR(J20,IOBS,2)/WW_ACC(2) )
                          ELSE 
                            RES_ARR(J20,IOBS,2) = 0.0D0
                       END IF
                     ELSE
                       RES_ARR(J20,IOBS,2) = 0.0
                  END IF
!
! --------------- Compute SNR in one channel: SNR_CHN
!
                  SNR_CHN = 1.0D0/DSQRT( 1.0D0/SNR_ARR(IOBS)**2 + &
     &                                   1.0D0/SNR_FLOOR**2 &
     &                                 )/DSQRT(1.0D0*LCHN*KCHN)
                  IF ( RES_ARR(J20,IOBS,1)*SNR_CHN > RES_PHAS_MAX  .AND. &
     &                 NUM_USED_OBS(J18,1) >  PIM%CONF%BPS_MINOBS_FINE .AND. &
     &                 FL_OBS(IOBS,1) ) THEN
!
                       RES_PHAS_MAX = RES_ARR(J20,IOBS,1)*SNR_CHN
                       IND_PHAS(1) = J18  ! station
                       IND_PHAS(2) = J20  ! freq ind
                       IND_PHAS(3) = IOBS ! obs ind
                  END IF
                  IF ( RES_ARR(J20,IOBS,2)*SNR_CHN > RES_AMPL_MAX .AND. &
     &                 NUM_USED_OBS(J18,2) >  PIM%CONF%BPS_MINOBS_FINE .AND. &
     &                 FL_OBS(IOBS,2) ) THEN
!
                       RES_AMPL_MAX = RES_ARR(J20,IOBS,2)*SNR_CHN
                       IND_AMPL(1) = J18  ! station
                       IND_AMPL(2) = J20  ! freq ind
                       IND_AMPL(3) = IOBS ! obs ind
                  END IF
                  IF ( PIM%CONF%DEBUG_LEVEL .GE. 3  .AND. J17 == 1 ) THEN
                       WRITE ( UNIT=STR, FMT=120 ) PIM%CONF%BEG_FRQ + J20-1, &
     &                                             RES_ARR(J20,IOBS,1), &
     &                                             RES_ARR(J20,IOBS,2)
 120                   FORMAT ( 'Frq: ',I2, 2X, F6.3, 1X, F5.3 )
                       CALL FLUSH ( 6 )
                  END IF
                  OUT = OUT(1:I_LEN(OUT))//'  '//STR
 4200          CONTINUE
               IF ( PIM%CONF%DEBUG_LEVEL .GE. 3  .AND.  J17 == 1 ) THEN
                    WRITE ( 6, '(A)' ) OUT(1:I_LEN(OUT))
                    CALL FLUSH ( 6 )
               END IF
 4190       CONTINUE
 4180   CONTINUE
        FL_REJECT_PHAS = .FALSE.
        FL_REJECT_AMPL = .FALSE.
        IF ( IND_PHAS(1) == 0  .OR.  IND_AMPL(1) == 0 ) GOTO 8170
        IF ( RES_PHAS_MAX > PIM%CONF%BPS_PHAS_REJECT  .AND. &
     &       NUM_USED_OBS(IND_PHAS(1),1) >  PIM%CONF%BPS_MINOBS_FINE ) THEN
!
             FL_REJECT_PHAS = .TRUE.
             ISTA = IND_PHAS(1)
             IFRQ = IND_PHAS(2)
             IOBS = IND_PHAS(3)
             IEQU = IND_EQU(IOBS) + (IFRQ-1)*2*KCHN - 2
             NUM_USED_OBS(ISTA,1) = NUM_USED_OBS(ISTA,1) - 1
             DO 4220 J22=1,KCHN
                IEQU = IEQU + 2
!
! ------------- Update the normal matrix and normal vector by removing
! ------------- the IEQU -th observation
!
                CALL DIAD_CVT_S ( -WEI_VEC(IEQU)**2, L_PAR, &
     &                             EQU_MAT(1,IEQU), EQU_MAT(1,IEQU), NOR_MAT )
                CALL DAXPY ( L_PAR, -WEI_VEC(IEQU)**2*EQU_VEC(IEQU), &
     &                       EQU_MAT(1,IEQU), 1, NOR_VEC, 1 )
 4220        CONTINUE
!
! ---------- Mark this observation as a phase  outlier
!
             FL_OBS(IOBS,1) = .FALSE.
             IF ( PIM%CONF%DEBUG_LEVEL .GE. 3 ) THEN
                  SNR_CHN = 1.0D0/DSQRT( 1.0D0/SNR_ARR(IOBS)**2 + &
     &                                   1.0D0/SNR_FLOOR**2)/ &
     &                      DSQRT(1.0D0*LCHN*KCHN)
!
                  WRITE ( 6, 130 ) 'PHAS', IND_OBS(IOBS), &
     &                             PIM%C_STA(PIM%OBS(IND_OBS(IOBS))%STA_IND(1)), &
     &                             PIM%C_STA(PIM%OBS(IND_OBS(IOBS))%STA_IND(2)), &
     &                             RES_ARR(IFRQ,IOBS,1)*SNR_CHN, SNR_CHN
 130              FORMAT ( 'BPASS Removed  bad ', A, ' Obs # ', I6, 2X, A, '/', A, &
     &                     ' fct: ', F7.3, ' sc: ', f7.1 )
                  CALL FLUSH ( 6 )
             END IF
        END IF
        IF ( RES_AMPL_MAX > PIM%CONF%BPS_AMPL_REJECT  .AND. &
     &       NUM_USED_OBS(IND_AMPL(1),2) >  PIM%CONF%BPS_MINOBS_FINE ) THEN
!
             FL_REJECT_AMPL = .TRUE.
             ISTA = IND_AMPL(1)
             IFRQ = IND_AMPL(2)
             IOBS = IND_AMPL(3)
             IEQU = IND_EQU(IOBS) + (IFRQ-1)*2*KCHN - 1
             NUM_USED_OBS(ISTA,2) = NUM_USED_OBS(ISTA,2) - 1
             DO 4230 J23=1,KCHN
                IEQU = IEQU + 2
                CALL DIAD_CVT_S ( -WEI_VEC(IEQU)**2, L_PAR, &
     &                             EQU_MAT(1,IEQU), EQU_MAT(1,IEQU), NOR_MAT )
                CALL DAXPY ( L_PAR, -WEI_VEC(IEQU)**2*EQU_VEC(IEQU), &
     &                       EQU_MAT(1,IEQU), 1, NOR_VEC, 1 )
 4230        CONTINUE
!
! ---------- Mark this observation as an amplitude outlier
!
             FL_OBS(IOBS,2) = .FALSE.
             IF ( PIM%CONF%DEBUG_LEVEL .GE. 3 ) THEN
                  SNR_CHN = 1.0D0/DSQRT( 1.0D0/SNR_ARR(IOBS)**2 + 1.0D0/SNR_FLOOR**2)/ &
     &                      DSQRT(1.0D0*LCHN*KCHN)
!
                  WRITE ( 6, 130 ) 'AMPL', IND_OBS(IOBS), &
     &                             PIM%C_STA(PIM%OBS(IND_OBS(IOBS))%STA_IND(1)), &
     &                             PIM%C_STA(PIM%OBS(IND_OBS(IOBS))%STA_IND(2)), &
     &                             RES_ARR(IFRQ,IOBS,2)*SNR_CHN, SNR_CHN
                  CALL FLUSH ( 6 )
             END IF
        END IF
        IF ( FL_REJECT_PHAS .OR. FL_REJECT_AMPL ) THEN
!
! ---------- Invert normal matrix
!
             CALL COPY_R8  ( LL_PAR, NOR_MAT, COV_MAT )
             CALL ERR_PASS ( IUER, IER )
             CALL INVS     ( L_PAR, COV_MAT, RC, IER )
             IF ( IER > 0 ) THEN
                  CALL ERR_LOG ( 8333, IUER, 'PIMA_BPASS_FINE', 'Error '// &
     &                'in an attempt to invert the normal matrix during '// &
     &                'solution update for removing the outlier' )
                  RETURN
             END IF
!
! ---------- Find vector of parameters estimates
!
             IER=-1
             CALL MUL_MV_SV_V ( L_PAR, COV_MAT, L_PAR, NOR_VEC, &
     &                          L_PAR, EST_VEC, IER )
!
! ---------- Compute the error vector
!
             DO 4240 J24=1,L_PAR
                LL = (J24*(J24+1))/2
                ERR_VEC(J24)=DSQRT( COV_MAT(LL) )
 4240        CONTINUE
           ELSE
             GOTO 8170
        END IF
 4170 CONTINUE
 8170 CONTINUE
      IF ( PIM%CONF%DEBUG_LEVEL .GE. 3 ) THEN
           IF ( IND_PHAS(2) > 0 ) THEN
                IFRQ = IND_PHAS(2)
                IOBS = IND_PHAS(3)
                SNR_CHN = 1.0D0/DSQRT( 1.0D0/SNR_ARR(IOBS)**2 + 1.0D0/SNR_FLOOR**2)/ &
     &                          DSQRT(1.0D0*LCHN*KCHN)
!
                WRITE ( 6, 140 ) 'PHAS', IND_OBS(IOBS), &
          &                      PIM%C_STA(PIM%OBS(IND_OBS(IOBS))%STA_IND(1)), &
          &                      PIM%C_STA(PIM%OBS(IND_OBS(IOBS))%STA_IND(2)), &
          &                      RES_ARR(IFRQ,IOBS,1)*SNR_CHN, SNR_CHN
 140            FORMAT ( 'BPASS Remained bad ', A, ' Obs # ', I6, 2X, &
     &                    A, '/', A, ' fct: ', F7.3, ' sc: ', F7.1 )
                CALL FLUSH ( 6 )
           END IF
           IF ( IND_AMPL(2) > 0 ) THEN
                IFRQ = IND_AMPL(2)
                IOBS = IND_AMPL(3)
                SNR_CHN = 1.0D0/DSQRT( 1.0D0/SNR_ARR(IOBS)**2 + 1.0D0/SNR_FLOOR**2)/ &
     &                          DSQRT(1.0D0*LCHN*KCHN)
                WRITE ( 6, 140 ) 'AMPL', IND_OBS(IOBS), &
     &                      PIM%C_STA(PIM%OBS(IND_OBS(IOBS))%STA_IND(1)), &
     &                      PIM%C_STA(PIM%OBS(IND_OBS(IOBS))%STA_IND(2)), &
     &                      RES_ARR(IFRQ,IOBS,2)*SNR_CHN, SNR_CHN
                CALL FLUSH ( 6 )
           END IF
      END IF
!
      DO 4250 J25=1,PIM%NSTA
         PCI = PIM%BPS%PCI(J25)
         IF ( PCI == 0 ) GOTO 4250
         IF ( BPS%NUM_OBS_FINE(J25,PCI) .LE. 0 ) GOTO 4250
         IFRQ = 0
         IPAR = IPAR_STA(J25)
         DO 4260 J26=PIM%CONF%BEG_FRQ,PIM%CONF%END_FRQ
            IFRQ = IFRQ + 1
            DO 4270 J27=1,PIM%NCHN
               PHAS_EST = 0.0D0
               AMPL_EST = 0.0D0
               AMPL_EST_LOG = 0.0D0
               FREQ_BEG(IFRQ) = PIM%FRQ(J26,PIM%CONF%FRQ_GRP)%FREQ
               FREQ_END(IFRQ) = PIM%FRQ(J26,PIM%CONF%FRQ_GRP)%FREQ + &
     &                              PIM%FRQ(J26,PIM%CONF%FRQ_GRP)%BAND_WIDTH
               IF ( PIM%CONF%BPS_INTRP_METHOD == PIMA__LEGENDRE ) THEN
                    DO 4280 J28=0,LN_PHS
                       PHAS_EST = PHAS_EST + EST_VEC(IPAR+J28+1)* &
     &                            LEGENDRE_POL ( J28, FREQ_BEG(IFRQ), FREQ_END(IFRQ), &
     &                                           PIM%FREQ_ARR(J27,J26,PIM%CONF%FRQ_GRP) )
 4280               CONTINUE
                  ELSE IF ( PIM%CONF%BPS_INTRP_METHOD == PIMA__SPLINE ) THEN
                    NOD_PHS_STEP = (FREQ_END(IFRQ) - FREQ_BEG(IFRQ))/(LN_PHS -1)
                    DO 4290 J29=1,LN_PHS
                       ARG_PHS_NOD(J29) = FREQ_BEG(IFRQ) + (J29-1)*NOD_PHS_STEP
 4290               CONTINUE 
                    ARG_PHS_NOD(1)      = ARG_PHS_NOD(1)      - NOD_FUDGE*NOD_PHS_STEP
                    ARG_PHS_NOD(LN_PHS) = ARG_PHS_NOD(LN_PHS) + NOD_FUDGE*NOD_PHS_STEP
                    DO 4300 J30=1-M_DEG,LN_PHS-1
                       PHAS_EST = PHAS_EST + EST_VEC(IPAR+J30+M_DEG)* &
                                             BSPL_VAL ( LN_PHS, ARG_PHS_NOD, M_DEG, &
     &                                                  J30, PIM%FREQ_ARR(J27,J26,PIM%CONF%FRQ_GRP) )
 4300               CONTINUE 
                  ELSE IF ( PIM%CONF%BPS_INTRP_METHOD == PIMA__LINEAR ) THEN
                    EQU_MAT(IPAR+1,IEQU) = 1.0
                    EQU_MAT(IPAR+2,IEQU) = (PIM%FREQ_ARR(J27,J26,PIM%CONF%FRQ_GRP) - &
     &                                      PIM%FREQ_ARR(1,J26,PIM%CONF%FRQ_GRP)     )/ &
     &                                     (PIM%FREQ_ARR(PIM%NCHN,J26,PIM%CONF%FRQ_GRP) - &
     &                                      PIM%FREQ_ARR(1,J26,PIM%CONF%FRQ_GRP))
               END IF
               IF ( PIM%CONF%BPS_INTRP_METHOD == PIMA__LEGENDRE ) THEN
                    DO 4310 J31=0,LN_AMP
                       AMPL_EST_LOG = AMPL_EST_LOG + EST_VEC(IPAR+LN_PHS+1+J31+1)* &
     &                                LEGENDRE_POL ( J31, FREQ_BEG(IFRQ), FREQ_END(IFRQ), &
     &                                               PIM%FREQ_ARR(J27,J26,PIM%CONF%FRQ_GRP) )
 4310               CONTINUE
                  ELSE IF ( PIM%CONF%BPS_INTRP_METHOD == PIMA__SPLINE ) THEN
                    NOD_AMP_STEP = (FREQ_END(IFRQ) - FREQ_BEG(IFRQ))/(LN_AMP -1)
                    DO 4320 J32=1,LN_AMP
                       SEQ_ARG = (J32-1)/DBLE(LN_AMP-1)
                       IF ( SEQ_ARG < 0.5D0 ) THEN
                            ARG_AMP_NOD(J15) = FREQ_BEG(IFRQ) + (FREQ_END(IFRQ) - FREQ_BEG(IFRQ))* &
     &                                                          (1.0D0 - DSQRT(DABS(1.0D0 - 2.D0*SEQ_ARG)))/2.0D0
                          ELSE
                            ARG_AMP_NOD(J15) = FREQ_BEG(IFRQ) + (FREQ_END(IFRQ) - FREQ_BEG(IFRQ))* &
     &                                                          (1.0D0 + DSQRT(DABS(1.0D0 - 2.D0*SEQ_ARG)))/2.0D0
                       END IF    
 4320               CONTINUE 
                    NOD_AMP_STEP        = ARG_AMP_NOD(LN_AMP) - ARG_AMP_NOD(LN_AMP-1) 
                    ARG_AMP_NOD(1)      = ARG_AMP_NOD(1)      - NOD_FUDGE*NOD_AMP_STEP
                    ARG_AMP_NOD(LN_AMP) = ARG_AMP_NOD(LN_AMP) + NOD_FUDGE*NOD_AMP_STEP
                    DO 4330 J33=1-M_DEG,LN_AMP-1
                       AMPL_EST_LOG = AMPL_EST_LOG + EST_VEC(IPAR+LN_PHS+M_DEG-1 + J33+M_DEG)* &
                                           BSPL_VAL ( LN_PHS, ARG_PHS_NOD, M_DEG, &
     &                                                J33, PIM%FREQ_ARR(J27,J26,PIM%CONF%FRQ_GRP) )
 4330               CONTINUE
                  ELSE IF ( PIM%CONF%BPS_INTRP_METHOD == PIMA__LINEAR ) THEN
                    EQU_MAT(IPAR+3,IEQU) = 1.0
               END IF
               IF ( AMPL_EST_LOG > DLOG(10.0D0) ) AMPL_EST_LOG = DLOG(10.0D0)
               AMPL_EST = DEXP ( AMPL_EST_LOG )
               IF ( PIM%CONF%DEBUG_LEVEL .GE. 4 ) THEN
                    WRITE  ( 6, 250 ) PIM%C_STA(J25), J26, J27, PHAS_EST, AMPL_EST
 250                FORMAT ( 'BPS_fine Sta: ', A, ' Ifrq: ', I3, ' Ichn: ', I3, &
     &                       ' Phas: ', F8.4,' Ampl: ', F8.4 )
                    CALL FLUSH ( 6 )
               END IF
               CORR_EST_CMPL = CMPLX ( AMPL_EST*COS(PHAS_EST), &
     &                                 AMPL_EST*SIN(PHAS_EST) )
               BPS%CMPL(J27,J26,J25) = BPS%CMPL(J27,J26,J25)*CORR_EST_CMPL
 4270       CONTINUE
            IF ( PIM%CONF%DEBUG_LEVEL .GE. 4 ) THEN
                 WRITE  ( 6, '(A)' ) '#'
            END IF
            IF ( PIM%CONF%BPS_INTRP_METHOD == PIMA__LEGENDRE ) THEN
                 IPAR = IPAR + (LN_PHS + LN_AMP + 2)
               ELSE IF ( PIM%CONF%BPS_INTRP_METHOD == PIMA__SPLINE ) THEN
                 IPAR = IPAR + (LN_PHS + LN_AMP + 2*(M_DEG-1))
               ELSE IF ( PIM%CONF%BPS_INTRP_METHOD == PIMA__LINEAR ) THEN
                 IPAR = IPAR + (LN_PHS + LN_AMP)
            END IF
 4260    CONTINUE
         IF ( PIM%CONF%DEBUG_LEVEL .GE. 4 ) THEN
              WRITE  ( 6, '(A)' ) '#'
         END IF
!
! ------ Normalization of the amplitude
!
         CALL BPASS_AMP_NRML ( PIM, J25, BPS%IND_STA_REF,                        &
     &                         BPS%CMPL(1,1,J25), BPS%CMPL(1,1,BPS%IND_STA_REF), &
     &                         AMPL_FRQ_NRML,             &
     &                         AMPL_BAND_NRML,            &
     &                         BPS%AMPL_FRQ_AVR(1,J25),   &
     &                         BPS%AMPL_FRQ_RMS(1,J25),   &
     &                         BPS%AMPL_TOT_RMS(J25),     &
     &                         BPS%AMPL_INTEGRAL(J25)     )
!
         CALL BPASS_GR_DEL ( J25, PIM, BPS, &
     &                       PIM%BPASS(J25)%BPS_MB_GRDEL, &
     &                       BPS_PHAS )
!
! ------ Remove constant phase from the bandpass
!
         DO 4340 J34=PIM%CONF%BEG_FRQ,PIM%CONF%END_FRQ
            DO 4350 J35=1,PIM%NCHN
               BPS%CMPL(J35,J34,J25) = BPS%CMPL(J35,J34,J25)*CMPLX(COS(BPS_PHAS),SIN(BPS_PHAS))
 4350       CONTINUE 
 4340    CONTINUE 
!
         IF ( PIM%CONF%DEBUG_LEVEL .GE. 4 ) THEN
              WRITE  ( 6, 150 ) PIM%C_STA(J25), PIM%BPASS(J25)%BPS_MB_GRDEL
 150          FORMAT ( 'PIMA_BPASS_FINE: Station ',A, ' Bpass_gr_del: ', 1PD13.6 )
              CALL FLUSH ( 6 )
         END IF
 4250 CONTINUE
!
      DEALLOCATE ( SNR_ARR )
      DEALLOCATE ( FL_OBS )
      DEALLOCATE ( SNR_NEW )
      DEALLOCATE ( IND_OBS )
      DEALLOCATE ( IND_EQU )
      DEALLOCATE ( RES_ARR )
      DEALLOCATE ( CNS_SIG )
      DEALLOCATE ( RES )
      DEALLOCATE ( EST_VEC )
      DEALLOCATE ( WEI_VEC )
      DEALLOCATE ( ERR_VEC )
      DEALLOCATE ( COV_MAT )
      DEALLOCATE ( NOR_VEC )
      DEALLOCATE ( NOR_MAT )
      DEALLOCATE ( EQU_VEC )
      DEALLOCATE ( EQU_MAT )
!
      IF ( PIM%CONF%DEBUG_LEVEL .GE. 4 ) THEN
           WRITE  ( 6, '(A)' ) '#'
           WRITE ( 6, * ) ' PIMA_BPASS_FINE finished at '//GET_CDATE()
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  PIMA_BPASS_FINE  !#!#
