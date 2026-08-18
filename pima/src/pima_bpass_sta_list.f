      SUBROUTINE PIMA_BPASS_STA_LIST ( PIM, POL_ARR, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  PIMA_BPASS_STA_LIST
! *                                                                      *
! *   Copyright (c) 1975-2025 United States Government as represented by *
! *   the Administrator of the National Aeronautics and Space            *
! *   Administration. All Rights Reserved.                               *
! *   License: NASA Open Source Software Agreement (NOSA).               *
! *                                                                      *
! * ## 25-JAN-2009 PIMA_BPASS_STA_LIST v5.3 (d) L. Petrov 17-JUL-2026 ## *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'pima.i'
      TYPE     ( PIMA__TYPE         ) :: PIM
      TYPE     ( PIM_CONF__TYPE     ) :: CONF
      INTEGER*4  IUER
      CHARACTER  POL_ARR(PIMA__POL_MAX)*1
      CHARACTER*1536, ALLOCATABLE :: BUF(:)
      CHARACTER  STR*128, STR1*128, BAS*17
      CHARACTER  KEYWORD(PIM__MOPT)*80, VALUE(PIM__MOPT)*80
      REAL*8     SNR_VAL_MIN, AMPL_VAL(PIM__MFRA), GR_DEL_VAL(PIM__MFRA), &
     &           GR_RAT_VAL, PH_RAT_VAL(PIM__MFRA), PHS_VAL(PIM__MFRA), &
     &           TIME_FRT_VAL
      REAL*8     SNR_ARR(PIM__MSCA), SNR_IND(2*PIM__MSCA), &
     &           PH_ACC_VAL, PH_ACC_ERR, FREQ_REF, &
     &           AMPL(PIM__MSCA,PIM__MSTA,PIM__MPLR), &
     &           SNR(PIM__MSCA,PIM__MSTA,PIM__MPLR), &
     &           SNR_PAR(PIM__MSCA,PIM__MSTA), &
     &           SNR_CRS(PIM__MSCA,PIM__MSTA), &
     &           SNR_ALL(PIM__MSCA,PIM__MSTA), &
     &           GR_DEL(PIM__MSCA,PIM__MSTA,PIM__MPLR), &
     &           PH_RAT(PIM__MSCA,PIM__MSTA,PIM__MPLR), &
     &           GR_RAT(PIM__MSCA,PIM__MSTA,PIM__MPLR), &
     &           TIME_FRT(PIM__MSCA,PIM__MSTA,PIM__MPLR), &
     &           PHS(PIM__MSCA,PIM__MSTA,PIM__MPLR), AMPL_ARR(PIM__MPLR), &
     &           TIME_FRT_ARR(PIM__MPLR), GR_DEL_ARR(PIM__MPLR), &
     &           PH_RAT_ARR(PIM__MPLR), GR_RAT_ARR(PIM__MPLR), &
     &           PHS_ARR(PIM__MPLR)
      INTEGER*4  IND_OBS_SEL(PIM__MSCA,PIM__MSTA), &
     &           IND_STA_ARR(PIM__MSCA,PIM__MSTA), &
     &           IND_OBS_FRI(PIM__MPLR,PIM__MOBS)
      INTEGER*1  SGN_STA_ARR(PIM__MSCA,PIM__MSTA)
      LOGICAL*1  FL_CNT, FL_ORIG
      INTEGER*4    MBUF__EXTRA, MIND
      PARAMETER  ( MBUF__EXTRA = 256 )
      PARAMETER  ( MIND = 256 )
      INTEGER*4  J1, J2, J3, J4, J5, J6, J7, J8, J9, J10, IP, I_BAS, NOBS, IND_OBS, &
     &           IND_SCA, IND_SOU, IND_REC, IND_STA(2), IND_STA_REM, &
     &           SGN_STA_REM, IND_REF, IND_REM, I_REF, I_REM, LCHN, FRI_STS, &
     &           NOB(PIM__MSTA), IND_FRA, POL_IND, K_POL, PCI, &
     &           LIND, IND(2,MIND), IND_SLOT, NRF, NRM, &
     &           NPOL, NBUF, FRI_STS_ARR(PIM__MPLR), N_ACC, PCI_ARR(PIM__MPLR), &
     &           L_POL, I_POL, IER
      REAL*8     AMPL_INTG, SB_DEL, PH_RAT_ERR(PIM__MFRA), GR_RAT_ERR, &
     &           GR_DEL_ERR(PIM__MFRA), PH_DEL_ERR(PIM__MFRA), &
     &           SB_DEL_ERR, GRAMBSP, SCAN_DUR, AP_LEN, EFF_FRQ_PHS, &
     &           EFF_FRQ_GRP, EFF_FRQ_RAT, SNR_MIN, COV_PR_PH, &
     &           COV_GR_MD, TEC, TEC_RATE, TEC_ERR, TEC_RATE_ERR, &
     &           PAR_ANG(2), PA_USED, AMPL_PLR(PIM__MPLR), &
     &           DECOR_TIM_ARR(PIM__MPLR), AMPL_SQR_ACC, NOI_ACC
      REAL*4     PCAL_GDEL(2,2)
      CHARACTER  POLAR_LL_CODES(PIM__MPLR)*2, POLAR_CC_CODES(PIM__MPLR)*2, &
     &           POLAR_LC_CODES(PIM__MPLR)*2, POLAR_CL_CODES(PIM__MPLR)*2,  &
     &           BAS_STA_REF*8, BAS_STA_REM*8, FIL_PIM*128, &
     &           BPS_FINE_SEARCH*8, OUT*512, POLAR_USED_ARR(PIM__MPLR)*2, &
     &           POL_STR*2, POLAR_TYP*7, POL_LAB(2)*1
      DATA       POLAR_CC_CODES  / 'RR', 'LR', 'RL', 'LL' /
      DATA       POLAR_LL_CODES  / 'HH', 'VH', 'HV', 'VV' /
      DATA       POLAR_LC_CODES  / 'HR', 'VR', 'HL', 'VL' /
      DATA       POLAR_CL_CODES  / 'RH', 'LH', 'RV', 'LV' /
      LOGICAL*4, EXTERNAL :: IS_R8_NAN
      INTEGER*4, EXTERNAL :: ILEN, I_LEN, LTM_DIF, ADD_CLIST
!
      CALL NOUT ( SIZEOF(PIM%BPS), PIM%BPS )
!
      ALLOCATE ( BUF(PIM__MOBS+MBUF__EXTRA), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( (PIM__MOBS+MBUF__EXTRA)*SIZEOF(BUF(1)), STR )
           CALL ERR_LOG ( 6331, IUER, 'PIMA_BPASS_STA_LIST', 'Error in an '// &
     &         'attempt to allocate '//STR(1:I_LEN(STR))//' bytes of '// &
     &         'dynamic memory for the fringe file contents buffer' )
           RETURN
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL RD_TEXT  ( PIM%CONF%FRINGE_FILE, PIM__MOBS, BUF, NBUF, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 6332, IUER, 'PIMA_BPASS_STA_LIST', 'Error in an '// &
     &         'attempt to read file '//PIM%CONF%FRINGE_FILE )
           DEALLOCATE ( BUF )
           RETURN
      END IF
!
      IF ( BUF(1)(1:LEN(PIMA__FRIRES_LABEL)) .NE. PIMA__FRIRES_LABEL          .AND. &
     &     BUF(1)(1:LEN(PIMA__FRIRES_LABEL)) .NE. PIMA__FRIRES_LABEL_20100405 .AND. &
     &     BUF(1)(1:LEN(PIMA__FRIRES_LABEL)) .NE. PIMA__FRIRES_LABEL_20140208 .AND. &
     &     BUF(1)(1:LEN(PIMA__FRIRES_LABEL)) .NE. PIMA__FRIRES_LABEL_20141224 .AND. &
     &     BUF(1)(1:LEN(PIMA__FRIRES_LABEL)) .NE. PIMA__FRIRES_LABEL_20190224 .AND. &
     &     BUF(1)(1:LEN(PIMA__FRIRES_LABEL)) .NE. PIMA__FRIRES_LABEL_20190420 .AND. &
     &     BUF(1)(1:LEN(PIMA__FRIRES_LABEL)) .NE. PIMA__FRIRES_LABEL_20221215       ) THEN
           CALL CLRCH ( STR )
           CALL TRAN ( 13, BUF(1), STR )
           CALL ERR_LOG ( 6333, IUER, 'PIMA_BPASS_STA_LIST', 'Wrong format '// &
     &         'of file '//PIM%CONF%FRINGE_FILE(1:I_LEN(PIM%CONF%FRINGE_FILE))// &
     &         ' -- the first line does not have a signature '// &
     &         PIMA__FRIRES_LABEL//' but instead of that is '//STR )
           DEALLOCATE ( BUF )
           RETURN
      END IF
!
      PIM%BPS%NUM_OBS_ACCUM = 0
      PIM%BPS%NUM_OBS_FINE  = 0
      NOB = 0
!
      IF ( PIM%CONF%MKDB_FRINGE_ALGORITHM == PIMA__FRA_DRF ) THEN
           IND_FRA = PIMA__DRF
         ELSE IF ( PIM%CONF%MKDB_FRINGE_ALGORITHM == PIMA__FRA_LSQ ) THEN
           IND_FRA = PIMA__LSQ
         ELSE IF ( PIM%CONF%MKDB_FRINGE_ALGORITHM == PIMA__FRA_MUL ) THEN
           IND_FRA = PIMA__MUL
         ELSE IF ( PIM%CONF%MKDB_FRINGE_ALGORITHM == PIMA__FRA_ADD ) THEN
           IND_FRA = PIMA__ADD
      END IF
      SNR_MIN = MIN(PIM%CONF%BPS_SNR_MIN_ACCUM, &
     &              PIM%CONF%BPS_SNR_MIN_FINE) 
      BPS_FINE_SEARCH = 'UNDF    '
      FL_CNT  = .FALSE.
      FL_ORIG = .FALSE.
      K_POL = 0
      POLAR_TYP = 'uknown '
      CALL CLRCH ( POL_ARR )
!
      IND_OBS_FRI = 0
      SNR         = 0.0
      L_POL = 0
      DO 410 J1=1,NBUF
         CALL EXWORD ( BUF(J1), MIND, LIND, IND, CHAR(32)//CHAR(9), IER )
         IF ( BUF(J1)(1:6) == '# FR1P' ) BUF(J1)(1:6) = '# FRIB'
         IF ( LIND .LE. 2 ) GOTO 410
         IF ( BUF(J1)(1:15) == '# Control file:' .AND. .NOT. FL_CNT ) THEN
              FL_CNT = .TRUE.
              CALL CLRCH ( PIM%BPS%FIL_CNT )
              PIM%BPS%FIL_CNT = BUF(J1)(16:)
              CALL CHASHL ( PIM%BPS%FIL_CNT )
              PIM%BPS%IND_STA_REF = LTM_DIF ( 0, PIM%NSTA, PIM%C_STA, &
     &                                    PIM%CONF%STA_REF )
              IF ( PIM%BPS%IND_STA_REF .LE. 0 ) THEN
                   WRITE ( 6, * ) ' PIM%NSTA = ', PIM%NSTA
                   CALL LIST_TO_LINE ( PIM%NSTA, PIM%C_STA, ", ", OUT )
                   CALL ERR_LOG ( 6334, IUER, 'PIMA_BPASS_STA_LIST', 'Station '// &
     &                  PIM%CONF%STA_REF//' was not found in the list of stations '// &
     &                  'that participated in that experiment '//OUT )
                   DEALLOCATE ( BUF )
                   RETURN
              END IF
!
              IF ( PIM%CONF%BANDPASS_MASK_FILE .NE. PIMA__BPASS_NO ) THEN
!
! ---------------- Read the file with bandpass mask
!
                   CALL ERR_PASS ( IUER, IER )
                   CALL READ_BANDPASS_MASK ( PIM, IER )
                   IF ( IER .NE. 0 ) THEN
                        CALL ERR_LOG ( 6335, IUER, 'PIMA_BPASS_STA_LIST', &
     &                      'Error in attempt to load the bandpass mask '// &
     &                      'from file '//PIM%CONF%BANDPASS_MASK_FILE )
                        DEALLOCATE ( BUF )
                        RETURN
                   END IF
              END IF
!
              IF ( ILEN(PIM%CONF%TIME_FLAG_FILE) > 0 ) THEN
                   CALL ERR_PASS ( IUER, IER )
                   CALL PIMA_READ_TIME_FLAG ( PIM, IER )
                   IF ( IER .NE. 0 ) THEN
                        CALL ERR_LOG ( 6336, IUER, 'PIMA_BPASS_STA_LIST', 'Error '// &
     &                      'an attempt to read and parse the time flag file '// &
     &                       PIM%CONF%TIME_FLAG_FILE )
                        RETURN
                   END IF
              END IF
            ELSE IF ( BUF(J1)(1:19) == '# FRIB.FINE_SEARCH:' ) THEN
              BPS_FINE_SEARCH = BUF(J1)(30:37)
            ELSE IF ( BUF(J1)(1:15) == '# FRIB.FRQ_GRP:' ) THEN
              CALL CHIN ( BUF(J1)(32:33), PIM%BPS%IFRG )
         END IF
         IF ( BUF(J1)(1:1) == '#') GOTO 410
         IF ( INDEX ( BUF(J1), 'FAILURE' ) > 0 ) GOTO 410
         CALL CHIN ( BUF(J1)(1:6), IND_OBS )
!
         IF ( BUF(J1)(1456:1459) == 'Sts:' ) THEN
              POL_STR = BUF(J1)(1453:1454) 
            ELSE IF ( BUF(J1)(1368:1371) == 'Sts:' ) THEN
              POL_STR = BUF(J1)(1365:1366) 
           ELSE
              CALL CLRCH ( STR )
              CALL INCH  ( J1, STR )
              CALL ERR_LOG ( 6337, IUER, 'PIMA_BPASS_STA_LIST', 'Trap of internal '// &
     &            'control: wrong string in fields 1456:1459 '//BUF(J1)(1456:1459)// &
     &            ' of the exepcted polarizaton code in line '//TRIM(STR)// &
     &            ' of the fringe file '//PIM%CONF%FRINGE_FILE )
              RETURN
         END IF
!
         PCI = LTM_DIF ( 0, PIM__MPLR, POLAR_CC_CODES, POL_STR )
         IF ( PCI < 1 ) THEN
              PCI = LTM_DIF ( 0, PIM__MPLR, POLAR_LL_CODES, POL_STR )
              IF ( PCI < 1 ) THEN
                   PCI = LTM_DIF ( 0, PIM__MPLR, POLAR_CL_CODES, POL_STR )
                   IF ( PCI < 1 ) THEN
                        PCI = LTM_DIF ( 0, PIM__MPLR, POLAR_LC_CODES, POL_STR )
                   END IF
              END IF
         END IF
         IF ( PCI < 1 ) THEN
              CALL ERR_LOG ( 6338, IUER, 'PIMA_BPASS_STA_LIST', 'Unsupported '// &
     &            'polarizaton code '//POL_STR//' in line '// &
     &             BUF(J1)(1:6)//' of the fringe file '//PIM%CONF%FRINGE_FILE )
              RETURN
         END IF
         IND_OBS_FRI(PCI,IND_OBS) = J1
 410  CONTINUE 
!
      DO 420 J2=1,PIM%NOBS
         IND_OBS = J2
         CALL ERR_PASS ( IUER, IER )
         NPOL = 0
!
! ------ Read observations from all polarization defined in 
! ------ IND_OBS_FRI
!
         DO 430 J3=1,PIM__MPLR
            IF ( IND_OBS_FRI(J3,J2) == 0 ) GOTO 430
            CALL PIMA_FRI_REA_OBS ( PIM, BUF(IND_OBS_FRI(J3,J2)), IND_OBS, &
     &             IND_SCA, IND_SOU, IND_STA, SNR_ARR(J3), AMPL_VAL, &
     &             AMPL_INTG, TIME_FRT_VAL, GR_DEL_VAL, &
     &             PH_RAT_VAL, GR_RAT_ARR(J3), PH_ACC_VAL, SB_DEL, PHS_VAL, &
     &             GR_DEL_ERR, PH_RAT_ERR, GR_RAT_ERR, PH_ACC_ERR, &
     &             SB_DEL_ERR, PH_DEL_ERR, GRAMBSP, SCAN_DUR, &
     &             AP_LEN, FREQ_REF, EFF_FRQ_PHS, EFF_FRQ_GRP, &
     &             EFF_FRQ_RAT, COV_PR_PH, COV_GR_MD, &
     &             TEC, TEC_RATE, TEC_ERR, TEC_RATE_ERR, &
     &             POLAR_USED_ARR(J3), PAR_ANG, PA_USED, &
     &             DECOR_TIM_ARR(J3), PCAL_GDEL, FRI_STS_ARR(J3), IER )
            IF ( IER .NE. 0 ) THEN
                 CALL CLRCH ( STR )
                 CALL INCH  ( J2, STR )
                 CALL ERR_LOG ( 6338, IUER, 'PIMA_BPASS_STA_LIST', 'Error '// &
     &               'an attempt get information from from fringe file '// &
     &                TRIM(PIM%CONF%FRINGE_FILE)//' about observation '//STR )
                 RETURN
            END IF
            I_POL = ADD_CLIST ( PIM__MPLR, L_POL, POL_ARR, POLAR_USED_ARR(J3)(1:1), IER )
            I_POL = ADD_CLIST ( PIM__MPLR, L_POL, POL_ARR, POLAR_USED_ARR(J3)(2:2), IER )
!
            AMPL_ARR(J3)       = AMPL_VAL(IND_FRA)
            GR_DEL_ARR(J3)     = GR_DEL_VAL(IND_FRA)
            PH_RAT_ARR(J3)     = PH_RAT_VAL(IND_FRA)
            GR_RAT_ARR(J3)     = GR_RAT_VAL
            PHS_ARR(J3)        = PHS_VAL(IND_FRA)
!
! --------- Store the polarization combination index
!
            NPOL = NPOL + 1
            PCI_ARR(NPOL) = J3
 430     CONTINUE 
         IF ( NPOL == 0 ) GOTO 420
!
         POL_IND = LTM_DIF ( 0, PIM__MPLR, POLAR_LL_CODES, POLAR_USED_ARR(1) )
         IF ( POL_IND .GE. 1 ) THEN
              POLAR_TYP = PIMA__PC_LL
            ELSE
              POL_IND = LTM_DIF ( 0, PIM__MPLR, POLAR_CC_CODES, POLAR_USED_ARR(1) )
              IF ( POL_IND .GE. 1 ) THEN
                   POLAR_TYP = PIMA__PC_CC
                 ELSE 
                   POL_IND = LTM_DIF ( 0, PIM__MPLR, POLAR_LC_CODES, POLAR_USED_ARR(1) )
                   IF ( POL_IND .GE. 1 ) THEN
                        POLAR_TYP = PIMA__PC_LC
                      ELSE
                        POL_IND = LTM_DIF ( 0, PIM__MPLR, POLAR_CL_CODES, POLAR_USED_ARR(1) )
                        IF ( POL_IND .GE. 1 ) THEN
                             POLAR_TYP = PIMA__PC_CL
                           ELSE
                             CALL CLRCH ( STR )
                             CALL INCH  ( IND_OBS, STR )
                             CALL ERR_LOG ( 6339, IUER, 'PIMA_BPASS_STA_LIST', 'Unsupported '// &
     &                           'used polarization combination '//POLAR_USED_ARR(1)// &
     &                           ' during parsing the '//TRIM(STR)//' observation in '// &
     &                           'fringe file '//PIM%CONF%FRINGE_FILE )
                             RETURN
                        END IF
                   END IF
              END IF
         END IF
!
! ------ Deselect observations that 
! ------ 1) are marked as "not used"
! ------ 2) non-detections
! ------ 3) with SNR < 1.0
! ------ 4) without all necessary polarizations
!
         IF ( IND_OBS > 0 ) THEN
              IF ( .NOT. PIM%USE_OBS(IND_OBS) ) GOTO 420
         END IF
!
! ------ The numbmer of polarization for this observations
!
         IF ( NPOL == 1 .OR. NPOL == 2 ) THEN
              IF ( POLAR_USED_ARR(PCI_ARR(1)) == 'RR' .OR. POLAR_USED_ARR(PCI_ARR(1)) == 'LL' ) THEN
                   CONTINUE 
                 ELSE 
!@                   CALL CLRCH ( STR )
!@                   CALL INCH  ( J2, STR )
!@                   CALL ERR_LOG ( 6340, IUER, 'PIMA_BPASS_STA_LIST', 'Trap '// &
!@     &                 'of internal control in procssing observation '// &
!@     &                  TRIM(STR)//' of the fringe file '//TRIM(PIM%CONF%FRINGE_FILE)// &
!@     &                  ' -- there was only one polarization '//POLAR_USED_ARR(1)// &
!@     &                  '. This case is not supported. Please rerun the coarse '// &
!@     &                  'fringe search with POLAR: ALL' )
!@                  RETURN
                   IF ( PIM%CONF%DEBUG_LEVEL .GE. 2 ) THEN
                        WRITE ( 6, '(A,I5,A,I1,A)' ) 'PIMA_BPAS_STA_LIST: Skipping observation ', J2, &
     &                                               ' because it has only ', NPOL, ' polarizations' 
                   END IF
                   GOTO 420
              END IF 
              IF ( SNR_ARR(PCI_ARR(1)) < 1.0D0                 ) GOTO  420
              IF ( BTEST ( FRI_STS_ARR(PCI_ARR(1)), NOC__PIM ) ) GOTO  420
              IF ( NPOL > 1 ) THEN
                   IF ( SNR_ARR(PCI_ARR(2)) < 1.0D0        ) GOTO  420
                   IF ( BTEST ( FRI_STS_ARR(2), NOC__PIM ) ) GOTO  420
              END IF
              IF ( PIM%CONF%PHAS_CAL_CODE .NE. PIMA__PCAL_NO ) THEN
!
! ---------------- Check: if pcal is used, but it was not found for a given observation,
! ---------------- then we bypass this observation
!
                   IF ( BTEST ( FRI_STS_ARR(PCI_ARR(1)), NPC__PIM ) ) GOTO 420
                   IF ( NPOL > 1 ) THEN
                        IF ( BTEST ( FRI_STS_ARR(PCI_ARR(2)), NPC__PIM ) ) GOTO  420
                   END IF
               END IF
            ELSE IF ( NPOL ==  4 ) THEN
               IF ( BTEST ( FRI_STS_ARR(1), NOC__PIM ) .AND. &
     &              BTEST ( FRI_STS_ARR(2), NOC__PIM ) .AND. &
     &              BTEST ( FRI_STS_ARR(3), NOC__PIM ) .AND. &
     &              BTEST ( FRI_STS_ARR(4), NOC__PIM )       ) THEN
                    GOTO 420
               END IF
!
               IF ( PIM%CONF%PHAS_CAL_CODE .NE. PIMA__PCAL_NO ) THEN
                    IF ( BTEST ( FRI_STS_ARR(1), NPC__PIM ) .OR. &
     &                   BTEST ( FRI_STS_ARR(2), NPC__PIM ) .OR. &
     &                   BTEST ( FRI_STS_ARR(3), NPC__PIM ) .OR. &
     &                   BTEST ( FRI_STS_ARR(4), NPC__PIM )      ) THEN
                         GOTO 420
                    END IF
               END IF
         END IF
!
         BAS_STA_REF = PIM%C_STA(PIM%OBS(IND_OBS)%STA_IND(1))
         BAS_STA_REM = PIM%C_STA(PIM%OBS(IND_OBS)%STA_IND(2))
!
         IF ( BUF(1)(1:LEN(PIMA__FRIRES_LABEL)) .EQ. PIMA__FRIRES_LABEL_20100405 .OR. &
     &        BUF(1)(1:LEN(PIMA__FRIRES_LABEL)) .EQ. PIMA__FRIRES_LABEL_20140208 .OR. &
     &        BUF(1)(1:LEN(PIMA__FRIRES_LABEL)) .EQ. PIMA__FRIRES_LABEL_20141224 .OR. &
     &        BUF(1)(1:LEN(PIMA__FRIRES_LABEL)) .EQ. PIMA__FRIRES_LABEL_20190224      ) THEN
!
! ----------- We do it for backward compatibility: long time ago DECOR_TIM values
! ----------- were not put into the fringe residual file
!
              DECOR_TIM_ARR = 1.0
         END IF
!
         IF ( PIM%CONF%DEBUG_LEVEL .GE. 8 ) THEN
              WRITE ( 6, * ) 'PIMA_BPAS_STA_LIST-361 IND_OBS= ', IND_OBS, ' PCI(1) = ', PCI_ARR(1), ' NPOL= ', INT2(NPOL), ' SNR= ', SNR_ARR(1)
         END IF
         IF ( PIM%CONF%STA_REF == BAS_STA_REF .OR. &
     &        PIM%CONF%STA_REF == BAS_STA_REM      ) THEN
!
! ----------- IND_REF -- index of the station that is the reference station in this baseline
! ----------- IND_REM -- index of the station that is the remote    station in this baseline
!
! ----------- I_REF   -- index of the bandpass reference station
! ----------- I_REM   -- index of the bandpass remote    station
!
              IND_REM = LTM_DIF ( 1, PIM%NSTA, PIM%C_STA, BAS_STA_REM )
              IND_REF = LTM_DIF ( 1, PIM%NSTA, PIM%C_STA, BAS_STA_REF )
              IF ( PIM%CONF%STA_REF == BAS_STA_REF ) THEN
                   SGN_STA_REM = -1
                   I_REF = IND_REF
                   I_REM = IND_REM
                 ELSE
                   SGN_STA_REM = 1
                   I_REF = IND_REM
                   I_REM = IND_REF
              END IF
!
              IF ( ( NPOL == 1 .OR. NPOL == 2 )                               .AND. &
     &             DECOR_TIM_ARR(PCI_ARR(1)) .GE. PIM%CONF%BPS_DECOR_TIM_MIN  .AND. &
     &             SNR_ARR(PCI_ARR(1)) > SNR_MIN                                    ) THEN
!
! ---------------- Cir-cir case. 
! ---------------- Update counters for the ACCUM mode
!
                   NOB(I_REM) = NOB(I_REM) + 1
                   NRM = NOB(I_REM)  ! just a shortcut
                   IND_OBS_SEL(NRM,I_REM) = IND_OBS
                   IND_STA_ARR(NRM,I_REM) = I_REM
                   SGN_STA_ARR(NRM,I_REM) = SGN_STA_REM
                   TIME_FRT(NRM,I_REM,1)  = TIME_FRT_ARR(PCI_ARR(1))
                   GR_DEL(NRM,I_REM,1) = GR_DEL_ARR(PCI_ARR(1))
                   PH_RAT(NRM,I_REM,1) = PH_RAT_ARR(PCI_ARR(1))
                   GR_RAT(NRM,I_REM,1) = GR_RAT_ARR(PCI_ARR(1))
                   PHS(NRM,I_REM,1)    = PHS_ARR(PCI_ARR(1))
                   SNR(NRM,I_REM,1)    = SNR_ARR(PCI_ARR(1))
                   SNR_ALL(NRM,I_REM)  = SNR_ARR(PCI_ARR(1))
                   AMPL(NRM,I_REM,1)   = AMPL_VAL(PCI_ARR(1))
                 ELSE IF ( POLAR_TYP .NE. PIMA__PC_CC ) THEN
                   AMPL_SQR_ACC = 0.0D0
                   N_ACC = 0
                   NOI_ACC = 0.0D0
                   DO 440 J4=1,PIM__MPLR
                      IF ( IS_R8_NAN(SNR_ARR(J4)) ) SNR_ARR(J4) = 0.0
 440               CONTINUE 
                   IF ( MIN ( SNR_ARR(1), SNR_ARR(4) ) > SNR_MIN .OR. &
     &                  MIN ( SNR_ARR(2), SNR_ARR(3) ) > SNR_MIN      ) THEN
!
! --------------------- Remote station
!
                        NOB(I_REM) = NOB(I_REM) + 1
                        NRM = NOB(I_REM)  ! just a shortcut
                        SNR_PAR(NRM,I_REM) = MIN ( SNR_ARR(1), SNR_ARR(4) )
                        SNR_CRS(NRM,I_REM) = MIN ( SNR_ARR(2), SNR_ARR(3) )
                        SNR_ALL(NRM,I_REM) = MIN ( SNR_ARR(1), SNR_ARR(2), &
     &                                             SNR_ARR(3), SNR_ARR(4) )
                        IND_OBS_SEL(NRM,I_REM) = IND_OBS
!!   write ( 6, * ) 'Sta: ', int2(i_rem), int2(i_ref), ' snr_all= ', sngl(snr_all(nrm,i_rem)), ' nrm= ', int2(nrm), ' ind_obs= ', int2(ind_obs) ! %%%%%%%%%%%%%
                        IND_STA_ARR(NRM,I_REM) = I_REM
                        SGN_STA_ARR(NRM,I_REM) = SGN_STA_REM
                        DO 450 J5=1,PIM__MPLR
                           TIME_FRT(NRM,I_REM,J5) = TIME_FRT_ARR(J5)
                           GR_DEL(NRM,I_REM,J5) = GR_DEL_ARR(J5)
                           PH_RAT(NRM,I_REM,J5) = PH_RAT_ARR(J5)
                           GR_RAT(NRM,I_REM,J5) = GR_RAT_ARR(J5)
                           PHS(NRM,I_REM,J5)  = PHS_ARR(J5)
                           SNR(NRM,I_REM,J5)  = SNR_ARR(J5)
                           AMPL(NRM,I_REM,J5) = AMPL_VAL(J5)
 450                    CONTINUE 
!
! --------------------- Reference station
!
                        NOB(I_REF) = NOB(I_REF) + 1
                        NRF = NOB(I_REF)  ! just a shortcut
                        SNR_PAR(NRF,I_REF) = MIN ( SNR_ARR(1), SNR_ARR(4) )
                        SNR_CRS(NRF,I_REF) = MIN ( SNR_ARR(2), SNR_ARR(3) )
                        SNR_ALL(NRF,I_REF) = MIN ( SNR_ARR(1), SNR_ARR(2), &
     &                                             SNR_ARR(3), SNR_ARR(4) )
                        IND_OBS_SEL(NRF,I_REF) = IND_OBS
                        IND_STA_ARR(NRF,I_REF) = I_REF
                        SGN_STA_ARR(NRF,I_REF) = -SGN_STA_REM
                        DO 460 J6=1,PIM__MPLR
                           TIME_FRT(NRF,I_REF,J6) = TIME_FRT_ARR(J6)
                           GR_DEL(NRF,I_REF,J6) = GR_DEL_ARR(J6)
                           PH_RAT(NRF,I_REF,J6) = PH_RAT_ARR(J6)
                           GR_RAT(NRF,I_REF,J6) = GR_RAT_ARR(J6)
                           PHS(NRF,I_REF,J6)  = PHS_ARR(J6)
                           SNR(NRF,I_REF,J6)  = SNR_ARR(J6)
                           AMPL(NRF,I_REF,J6) = AMPL_VAL(J6)
 460                    CONTINUE 
                   END IF
              END IF
         END IF
 420  CONTINUE 
!
      PIM%BPS%POLAR = PIM%CONF%POLAR
      IF ( BPS_FINE_SEARCH == 'UNDF    ' ) THEN
           CALL ERR_LOG ( 6341, IUER, 'PIMA_BPASS_STA_LIST', 'Did not find '// &
     &         'FINE_SEARCH algorithm in section comment of file '// &
     &          PIM%CONF%FRINGE_FILE )
           RETURN
         ELSE IF ( BPS_FINE_SEARCH .EQ. PIMA__FINE_SEARCH_PAR ) THEN
           CONTINUE 
         ELSE IF ( BPS_FINE_SEARCH .NE. PIM%CONF%FRIB_FINE_SEARCH ) THEN
           CALL ERR_LOG ( 6342, IUER, 'PIMA_BPASS_STA_LIST', 'Mismatch in '// &
     &         'FINE_SEARCH algorithm wuil processing fringe file '// &
     &         TRIM(PIM%CONF%FRINGE_FILE)//' -- it is '//BPS_FINE_SEARCH// &
     &         ' in fringe file, while '//TRIM(PIM%CONF%FRIB_FINE_SEARCH)// &
     &         ' in the control file -- such a case is not supported since '// &
     &         ' group delays and and phase delay rates may not be '// &
     &         'computed for such an algorithm' )
           RETURN
      END IF
!
      DO 470 J7=1,PIM%NSTA
         IF ( NOB(J7) < 1 ) GOTO 470
         DO 480 J8=1,NOB(J7)
            SNR_ALL(J8,J7) = -SNR_ALL(J8,J7)
            SNR_IND(J8) = J8 + 1.D-8
 480     CONTINUE
         CALL SORT8 ( NOB(J7), SNR_ALL(1,J7), SNR_IND )
!
! ------ Enforce the cap for the number of observations per station
! ------ used in ACCUM and FINE modes
!
         PIM%BPS%NUM_OBS_ACCUM(J7) = MIN ( PIM%CONF%BPS_NOBS_ACCUM, NOB(J7) )
         PIM%BPS%NUM_OBS_FINE(J7)  = MIN ( PIM%CONF%BPS_NOBS_FINE,  NOB(J7) )
         NOB(J7) = MAX ( PIM%BPS%NUM_OBS_ACCUM(J7), PIM%BPS%NUM_OBS_FINE(J7) )
!
         DO 490 J9=1,NOB(J7)
            IND_REC = SNR_IND(J9)
            PIM%BPS%IND_OBS_SEL(J9,J7) = IND_OBS_SEL(IND_REC,J7)
            PIM%BPS%SNR_ALL(J9,J7) = -SNR_ALL(J9,J7)
            PIM%BPS%SNR_PAR(J9,J7) =  SNR_PAR(IND_REC,J7)
            PIM%BPS%SNR_CRS(J9,J7) =  SNR_CRS(IND_REC,J7)
!
            NPOL = 0
            DO 4100 J10=1,PIM%NSTK
               PIM%BPS%SNR(J9,J7,J10)      = SNR(IND_REC,J7,J10)     
               PIM%BPS%AMPL(J9,J7,J10)     = AMPL(IND_REC,J7,J10)
               PIM%BPS%TIME_FRT(J9,J7,J10) = TIME_FRT(IND_REC,J7,J10)
               PIM%BPS%GR_DEL(J9,J7,J10)   = GR_DEL(IND_REC,J7,J10)
               PIM%BPS%PH_RAT(J9,J7,J10)   = PH_RAT(IND_REC,J7,J10)
               PIM%BPS%GR_RAT(J9,J7,J10)   = GR_RAT(IND_REC,J7,J10)
               PIM%BPS%PHS(J9,J7,J10)      = PHS(IND_REC,J7,J10)
!
               IF ( PIM%CONF%DEBUG_LEVEL .GE. 5 .AND. &
     &              PIM%BPS%SNR(J9,J7,J10) > 0.0D0 ) THEN
                    IF ( PIM%C_STA(J7) == PIM%CONF%STA_REF ) THEN
                         STR = 'REF'
                       ELSE
                         STR = 'rem'
                    END IF
                    WRITE ( 6, 110 ) PIM%C_STA(J7), STR(1:3), PIMA__PCI_NAM(J10), &
     &                               PIM%BPS%IND_OBS_SEL(J9,J7), &
     &                               PIM%BPS%SNR(J9,J7,J10), PIM%BPS%SNR_ALL(J9,J7), &
     &                               PIM%BPS%SNR_PAR(J9,J7), PIM%BPS%SNR_CRS(J9,J7) 
 110                FORMAT ( 'PIMA_BPASS_STA_LIST: ',A, 1X, A, 1X, A, 1X, &
     &                       'Obs: ', I6, ' SNR= ', F7.1, &
     &                       ' SNR_all: ', F7.1, ' SNR_par: ', F7.1, ' SNR_crs: ', F7.1 )
                    NPOL = NPOL + 1
               END IF
 4100       CONTINUE 
!
            IF ( PIM%CONF%DEBUG_LEVEL .GE. 5 ) THEN
                 IF ( NPOL > 1 ) THEN
                      WRITE ( 6, '(A)' ) ' '
                 END IF
            END IF
 490     CONTINUE 
 470  CONTINUE
!
      LCHN = PIM%NCHN
      ALLOCATE ( PIM%BPS%CMPL(LCHN,PIM%NFRQ,PIM%NSTA), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( 8*LCHN*PIM%NFRQ*PIM%NSTA, STR )
           CALL ERR_LOG ( 6343, IUER, 'PIMA_BPASS_STA_LIST', 'Error in an '// &
     &         'attempt to allocate '//STR(1:I_LEN(STR))//' bytes of '// &
     &         'dynamic memory for PIM%BPS%CMPL' )
           DEALLOCATE ( BUF )
           RETURN
      END IF
!
! --- Initialization
!
      PIM%BPS%CMPL = CMPLX ( 1.0, 0.0 )
!
      ALLOCATE ( PIM%BPS%AMPL_FRQ_AVR(PIM%NFRQ,PIM%NSTA), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( 4*PIM%NFRQ*PIM%NSTA, STR )
           CALL ERR_LOG ( 6344, IUER, 'PIMA_BPASS_STA_LIST', 'Error in an '// &
     &         'attempt to allocate '//STR(1:I_LEN(STR))//' bytes of '// &
     &         'dynamic memory for PIM%BPS%AMPL_FRQ_AVR' )
           DEALLOCATE ( BUF )
           RETURN
      END IF
      CALL NOUT_R4 ( PIM%NFRQ*PIM%NSTA, PIM%BPS%AMPL_FRQ_AVR )
!
      ALLOCATE ( PIM%BPS%PHAS_FRQ_AVR(PIM%NFRQ,PIM%NSTA), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( 4*PIM%NFRQ*PIM%NSTA, STR )
           CALL ERR_LOG ( 6345, IUER, 'PIMA_BPASS_STA_LIST', 'Error in an '// &
     &         'attempt to allocate '//STR(1:I_LEN(STR))//' bytes of '// &
     &         'dynamic memory for PIM%BPS%PHAS_FRQ_AVR' )
           DEALLOCATE ( BUF )
           RETURN
      END IF
      CALL NOUT_R4 ( PIM%NFRQ*PIM%NSTA, PIM%BPS%PHAS_FRQ_AVR )
!
      ALLOCATE ( PIM%BPS%AMPL_FRQ_RMS(PIM%NFRQ,PIM%NSTA), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( 4*PIM%NFRQ*PIM%NSTA, STR )
           CALL ERR_LOG ( 6346, IUER, 'PIMA_BPASS_STA_LIST', 'Error in an '// &
     &         'attempt to allocate '//STR(1:I_LEN(STR))//' bytes of '// &
     &         'dynamic memory for PIM%BPS%AMPL_FRQ_RMS' )
           DEALLOCATE ( BUF )
           RETURN
      END IF
      CALL NOUT_R4 ( PIM%NFRQ*PIM%NSTA, PIM%BPS%AMPL_FRQ_RMS )
!
      ALLOCATE ( PIM%BPS%PHAS_FRQ_RMS(PIM%NFRQ,PIM%NSTA), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( 4*PIM%NFRQ*PIM%NSTA, STR )
           CALL ERR_LOG ( 6347, IUER, 'PIMA_BPASS_STA_LIST', 'Error in an '// &
     &         'attempt to allocate '//STR(1:I_LEN(STR))//' bytes of '// &
     &         'dynamic memory for PIM%BPS%PHAS_FRQ_RMS' )
           DEALLOCATE ( BUF )
           RETURN
      END IF
      CALL NOUT_R4 ( PIM%NFRQ*PIM%NSTA, PIM%BPS%PHAS_FRQ_RMS )
!
      ALLOCATE ( PIM%BPS%PHAS_FRQ_RATE(PIM%NFRQ,PIM%NSTA), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( 4*PIM%NFRQ*PIM%NSTA, STR )
           CALL ERR_LOG ( 6348, IUER, 'PIMA_BPASS_STA_LIST', 'Error in an '// &
     &         'attempt to allocate '//STR(1:I_LEN(STR))//' bytes of '// &
     &         'dynamic memory for PIM%BPS%PHAS_FRQ_RATE' )
           DEALLOCATE ( BUF )
           RETURN
      END IF
      CALL NOUT_R4 ( PIM%NFRQ*PIM%NSTA, PIM%BPS%PHAS_FRQ_RATE )
!
      PIM%BPS%STATUS = PIMA__BPASS_ALLOC
      IF ( PIM%CONF%DEBUG_LEVEL .GE. 5 ) THEN
           CALL FLUSH ( 6 ) 
      END IF
!
      DEALLOCATE ( BUF )
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  PIMA_BPASS_STA_LIST  !#!#
