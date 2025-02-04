      SUBROUTINE PIMA_BPASS_STA_LIST ( PIM, POL_ARR, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  PIMA_BPASS_STA_LIST
! *                                                                      *
! * ## 25-JAN-2009 PIMA_BPASS_STA_LIST v4.0 (c) L. Petrov 06-NOV-2020 ## *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'pima.i'
      TYPE     ( PIMA__TYPE         ) :: PIM
      TYPE     ( PIM_CONF__TYPE     ) :: CONF
      INTEGER*4  IUER
      CHARACTER  POL_ARR*(*)
      CHARACTER*1536, ALLOCATABLE :: BUF(:)
      CHARACTER  STR*128, STR1*128, BAS*17
      CHARACTER  KEYWORD(PIM__MOPT)*80, VALUE(PIM__MOPT)*80
      REAL*8     SNR_VAL, SNR_VAL_MIN, AMPL_VAL(PIM__MFRA), GR_DEL_VAL(PIM__MFRA), &
     &           GR_RAT_VAL, PH_RAT_VAL(PIM__MFRA), PHS_VAL(PIM__MFRA), &
     &           TIME_FRT_VAL
      REAL*8     SNR_ARR(2*PIM__MSCA), SNR_IND(2*PIM__MSCA), &
     &           PH_ACC_VAL, PH_ACC_ERR, FREQ_REF, &
     &           AMPL(PIM__MSCA,PIM__MSTA,PIM__MPLR), &
     &           SNR(PIM__MSCA,PIM__MSTA,PIM__MPLR), &
     &           GR_DEL(PIM__MSCA,PIM__MSTA,PIM__MPLR), &
     &           PH_RAT(PIM__MSCA,PIM__MSTA,PIM__MPLR), &
     &           GR_RAT(PIM__MSCA,PIM__MSTA,PIM__MPLR), &
     &           TIME_FRT(PIM__MSCA,PIM__MSTA,PIM__MPLR), &
     &           PHS(PIM__MSCA,PIM__MSTA,PIM__MPLR)
      INTEGER*4  IND_OBS_SEL(PIM__MSCA,PIM__MSTA,PIM__MPLR), &
     &           IND_OBS_POL(PIM__MSCA,PIM__MSTA,PIM__MPLR), &
     &           IND_STA_ARR(PIM__MSCA,PIM__MSTA,PIM__MPLR)
      INTEGER*1  SGN_STA_ARR(PIM__MSCA,PIM__MSTA,PIM__MPLR)
      LOGICAL*1  FL_CNT, FL_ORIG
      INTEGER*4    MBUF__EXTRA, MIND
      PARAMETER  ( MBUF__EXTRA = 256 )
      PARAMETER  ( MIND = 256 )
      INTEGER*4  J1, J2, J3, J4, J5, J6, J7, J8, IP, I_BAS, NOBS, IND_OBS, &
     &           IND_SCA, IND_SOU, IND_REC, IND_STA(2), IND_STA_REM, &
     &           SGN_STA_REM, IND_REF, IND_REM, I_REF, I_REM, LCHN, FRI_STS, &
     &           NOB(PIM__MSTA,PIM__MPLR), IND_FRA, POL_IND, K_POL, &
     &           LIND, IND(2,MIND), IND_OBS_LAST, IND_SLOT, NRF, NRM, IER
      REAL*8     AMPL_INTG, SB_DEL, PH_RAT_ERR(PIM__MFRA), GR_RAT_ERR, &
     &           GR_DEL_ERR(PIM__MFRA), PH_DEL_ERR(PIM__MFRA), &
     &           SB_DEL_ERR, GRAMBSP, SCAN_DUR, AP_LEN, EFF_FRQ_PHS, &
     &           EFF_FRQ_GRP, EFF_FRQ_RAT, SNR_MIN, COV_PR_PH, &
     &           COV_GR_MD, TEC, TEC_RATE, TEC_ERR, TEC_RATE_ERR, &
     &           PAR_ANG(2), PA_USED, SNR_PLR(PIM__MPLR), AMPL_PLR(PIM__MPLR), &
     &           DECOR_TIM, DECOR_PLR(PIM__MPLR), PC_IND
      REAL*4     PCAL_GDEL(2,2)
      CHARACTER  POLAR_LL_CODES(PIM__MPLR)*2, POLAR_CC_CODES(PIM__MPLR)*2, &
     &           POLAR_LC_CODES(PIM__MPLR)*2, POLAR_CL_CODES(PIM__MPLR)*2,  &
     &           BAS_STA_REF*8, BAS_STA_REM*8, FIL_PIM*128, &
     &           BPS_FINE_SEARCH*8, OUT*512, POLAR_USED*2, POLAR_TYP*7, POL_LAB(2)*1
      DATA       POLAR_LL_CODES / 'HH', 'VH', 'HV', 'VV' /
      DATA       POLAR_CC_CODES / 'RR', 'LR', 'RL', 'LL' /
      DATA       POLAR_LC_CODES  / 'HR', 'VR', 'HL', 'VL' /
      DATA       POLAR_CL_CODES  / 'RH', 'LH', 'RV', 'LV' /
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
     &         'dynamic memory' )
           RETURN
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL RD_TEXT  ( PIM%CONF%FRINGE_FILE, PIM__MOBS, BUF, NOBS, IER )
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
      IND_OBS_LAST = -1
      SNR_PLR   = 0.0D0
      AMPL_PLR  = 0.0D0
      DECOR_PLR = 0.0D0
      K_POL = 0
      POLAR_TYP = 'uknown '
      CALL CLRCH ( POL_ARR )
!
      DO 410 J1=1,NOBS
         IF ( BUF(J1)(1:6) == '# FR1P' ) BUF(J1)(1:6) = '# FRIB'
         CALL EXWORD ( BUF(J1), MIND, LIND, IND, CHAR(32)//CHAR(9), IER )
         IF ( LIND .GE. 3 ) THEN
              IF ( BUF(J1)(IND(1,1):IND(2,1)) == '#'           .AND. &
     &             BUF(J1)(IND(1,2):IND(2,2)) == 'FRIB.POLAR:' .AND. &
     &             BUF(J1)(IND(1,3):IND(2,3)) == 'ORIG'              ) THEN
                   FL_ORIG = .TRUE.
              END IF
         END IF
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
                        CALL ERR_LOG ( 6336, IUER, 'PIMA_BPASS_STA_LIST', 'Station '// &
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
         IF ( BUF(J1)(1:1) == '#' ) GOTO 410
         PIM%BPS%POLAR = PIM%CONF%POLAR
!
         CALL ERR_PASS ( IUER, IER )
         CALL PIMA_FRI_REA_OBS ( PIM, BUF(J1), IND_OBS, IND_SCA, IND_SOU, &
     &                           IND_STA, SNR_VAL, AMPL_VAL, &
     &                           AMPL_INTG, TIME_FRT_VAL, GR_DEL_VAL, &
     &                           PH_RAT_VAL, GR_RAT_VAL, PH_ACC_VAL, SB_DEL, PHS_VAL, &
     &                           GR_DEL_ERR, PH_RAT_ERR, GR_RAT_ERR, PH_ACC_ERR, &
     &                           SB_DEL_ERR, PH_DEL_ERR, GRAMBSP, SCAN_DUR, &
     &                           AP_LEN, FREQ_REF, EFF_FRQ_PHS, EFF_FRQ_GRP, &
     &                           EFF_FRQ_RAT, COV_PR_PH, COV_GR_MD, &
     &                           TEC, TEC_RATE, TEC_ERR, TEC_RATE_ERR, &
     &                           POLAR_USED, PAR_ANG, PA_USED, DECOR_TIM, PCAL_GDEL, &
     &                           FRI_STS, IER )
         IF ( IER .NE. 0 ) THEN
              CALL CLRCH ( STR )
              CALL INCH  ( J1, STR )
              CALL ERR_LOG ( 6337, IUER, 'PIMA_BPASS_STA_LIST', 'Error in '// &
     &            'an attempt to parse line '//STR(1:I_LEN(STR))// &
     &            ' of the fringe file '//PIM%CONF%FRINGE_FILE )
              RETURN
         END IF
!
! ------ Deselect observations that 
! ------ 1) are marked as "not used"
! ------ 2) non-detections
! ------ 3) with SNR < 1.0
!
         IF ( IND_OBS > 0 ) THEN
              IF ( .NOT. PIM%USE_OBS(IND_OBS) ) GOTO 410
         END IF
         IF ( SNR_VAL < 1.0D0 ) GOTO  410
         IF ( BTEST ( FRI_STS, NOC__PIM ) ) GOTO  410
         IF ( PIM%CONF%PHAS_CAL_CODE .NE. PIMA__PCAL_NO ) THEN
!
! ----------- Check: if pcal is used, but it was not found for a given observation,
! ----------- then we bypass this observation
!
              IF ( BTEST ( FRI_STS, NPC__PIM ) ) GOTO 410
         END IF
         IF ( FL_ORIG ) THEN
              POL_IND = LTM_DIF ( 0, PIM__MPLR, POLAR_LL_CODES, POLAR_USED )
              IF ( POL_IND .GE. 1 ) THEN
                   POLAR_TYP = PIMA__PC_LL
                 ELSE
                   POL_IND = LTM_DIF ( 0, PIM__MPLR, POLAR_CC_CODES, POLAR_USED )
                   IF ( POL_IND .GE. 1 ) THEN
                        POLAR_TYP = PIMA__PC_CC
                      ELSE 
                        POL_IND = LTM_DIF ( 0, PIM__MPLR, POLAR_LC_CODES, POLAR_USED )
                        IF ( POL_IND .GE. 1 ) THEN
                             POLAR_TYP = PIMA__PC_LC
                           ELSE
                             POL_IND = LTM_DIF ( 0, PIM__MPLR, POLAR_CL_CODES, POLAR_USED )
                             IF ( POL_IND .GE. 1 ) THEN
                                  POLAR_TYP = PIMA__PC_CL
                                ELSE
                                  CALL CLRCH ( STR )
                                  CALL INCH  ( IND_OBS, STR )
                                  CALL ERR_LOG ( 6338, IUER, 'PIMA_BPASS_STA_LIST', 'Unsupported '// &
     &                                'used polarization combination '//POLAR_USED// &
     &                                ' during parsing the '//TRIM(STR)//' observation in '// &
     &                                'fringe file '//PIM%CONF%FRINGE_FILE )
                                  RETURN
                             END IF
                        END IF
                   END IF
              END IF
            ELSE
              POLAR_TYP = PIMA__PC_CC
         END IF
!
         IF ( PIM%CONF%POLAR == PIMA__POLAR_I .AND. POLAR_TYP .NE. PIMA__PC_CC ) THEN
              IF ( PIM%CONF%BPS_MODE == PIMA__BPASS_FINE ) THEN
                   CALL ERR_LOG ( 6339, IUER, 'PIMA_BPASS_STA_LIST', 'The current '// &
     &                  PIMA__LABEL//' supports ACCUM or FINE mode only for single '// &
     &                 'polarization or dual circular polarization. This limitation '// &
     &                 'will be lifted in the new version' )
                   RETURN 
              END IF
         END IF
!
         BAS_STA_REF = PIM%C_STA(PIM%OBS(IND_OBS)%STA_IND(1))
         BAS_STA_REM = PIM%C_STA(PIM%OBS(IND_OBS)%STA_IND(2))
         IF ( FL_ORIG .AND. J1 < NOBS ) THEN
!
! ----------- Peek for the next record. If it belong to the same observation, jump to it, since
! ----------- we want to process the last record of the same observation.
!
              SNR_PLR(POL_IND)   = SNR_VAL
              AMPL_PLR(POL_IND)  = AMPL_VAL(IND_FRA)
              DECOR_PLR(POL_IND) = DECOR_TIM
              IF ( BUF(J1+1)(1:49) == BUF(J1)(1:49) ) GOTO 410
         END IF
!
         IF ( BUF(1)(1:LEN(PIMA__FRIRES_LABEL)) .EQ. PIMA__FRIRES_LABEL_20100405 .OR. &
     &        BUF(1)(1:LEN(PIMA__FRIRES_LABEL)) .EQ. PIMA__FRIRES_LABEL_20140208 .OR. &
     &        BUF(1)(1:LEN(PIMA__FRIRES_LABEL)) .EQ. PIMA__FRIRES_LABEL_20141224 .OR. &
     &        BUF(1)(1:LEN(PIMA__FRIRES_LABEL)) .EQ. PIMA__FRIRES_LABEL_20190224      ) THEN
!
! ----------- We do it for backward compatibility: long time ago DECOR_TIM values
! ----------- were not put into the fringe residual file
!
              DECOR_TIM = 1.0
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
              IF ( POLAR_TYP == PIMA__PC_CC                  .AND. &
     &             DECOR_TIM .GE. PIM%CONF%BPS_DECOR_TIM_MIN .AND. &
     &             SNR_VAL > SNR_MIN                               ) THEN
!
! ---------------- Cir-cir case. 
! ---------------- Update counters for the ACCUM mode
!
                   NOB(I_REM,PIMA__PCI_PAR) = NOB(I_REM,PIMA__PCI_PAR) + 1
                   NRM = NOB(I_REM,PIMA__PCI_PAR)  ! just a shortcut
                   IND_OBS_SEL(NRM,I_REM,PIMA__PCI_PAR) = IND_OBS
                   IND_OBS_POL(NRM,I_REM,PIMA__PCI_PAR) = 1
                   IND_STA_ARR(NRM,I_REM,PIMA__PCI_PAR) = I_REM
                   SGN_STA_ARR(NRM,I_REM,PIMA__PCI_PAR) = SGN_STA_REM
                   TIME_FRT(NRM,I_REM,PIMA__PCI_PAR) = TIME_FRT_VAL
                   GR_DEL(NRM,I_REM,PIMA__PCI_PAR) = GR_DEL_VAL(IND_FRA)
                   PH_RAT(NRM,I_REM,PIMA__PCI_PAR) = PH_RAT_VAL(IND_FRA)
                   GR_RAT(NRM,I_REM,PIMA__PCI_PAR) = GR_RAT_VAL
                   PHS(NRM,I_REM,PIMA__PCI_PAR)  = PHS_VAL(IND_FRA)
                   SNR(NRM,I_REM,PIMA__PCI_PAR)  = SNR_VAL
                   AMPL(NRM,I_REM,PIMA__PCI_PAR) = AMPL_VAL(1)
                 ELSE IF ( POLAR_TYP .NE. PIMA__PC_CC ) THEN
                   DO 420 J2=1,PIM__MPLR
                      IF ( SNR_PLR(PIMA__PCC_COD(1,J2)) > SNR_MIN .AND. &
     &                     SNR_PLR(PIMA__PCC_COD(2,J2)) > SNR_MIN .AND. &
     &                     DECOR_PLR(PIMA__PCC_COD(1,J2)) > PIM%CONF%BPS_DECOR_TIM_MIN .AND. &
     &                     DECOR_PLR(PIMA__PCC_COD(2,J2)) > PIM%CONF%BPS_DECOR_TIM_MIN       ) THEN
!
! ------------------------ Remote station
!
                           NOB(I_REM,J2) = NOB(I_REM,J2) + 1
                           NRM = NOB(I_REM,J2) ! just a shortcut
                           IND_OBS_SEL(NRM,I_REM,J2) = IND_OBS
                           IND_STA_ARR(NRM,I_REM,J2) = I_REM
                           SGN_STA_ARR(NRM,I_REM,J2) = SGN_STA_REM
                           TIME_FRT(NRM,I_REM,J2) = TIME_FRT_VAL
                           GR_DEL(NRM,I_REM,J2) = GR_DEL_VAL(IND_FRA)
                           PH_RAT(NRM,I_REM,J2) = PH_RAT_VAL(IND_FRA)
                           GR_RAT(NRM,I_REM,J2) = GR_RAT_VAL
                           PHS(NRM,I_REM,J2)  = PHS_VAL(IND_FRA)
                           SNR(NRM,I_REM,J2)  = MIN ( SNR_PLR(PIMA__PCC_COD(1,J2)),  &
          &                                           SNR_PLR(PIMA__PCC_COD(2,J2))   )
                           AMPL(NRM,I_REM,J2) = MIN ( AMPL_PLR(PIMA__PCC_COD(1,J2)), &
     &                                                AMPL_PLR(PIMA__PCC_COD(2,J2))  )
                           IF ( SNR_PLR(PIMA__PCC_COD(1,J2)) > SNR_PLR(PIMA__PCC_COD(2,J2)) ) THEN
                                IND_OBS_POL(NRM,I_REM,J2) = PIMA__PCC_COD(1,J2)
                              ELSE
                                IND_OBS_POL(NRM,I_REM,J2) = PIMA__PCC_COD(2,J2)
                           END IF
!
! ------------------------ Reference station
!
                           NOB(I_REF,J2) = NOB(I_REF,J2) + 1
                           NRF = NOB(I_REF,J2) ! just a shortcut
                           IND_OBS_SEL(NRF,I_REF,J2) = IND_OBS_SEL(NRM,I_REM,J2) 
                           IND_STA_ARR(NRF,I_REF,J2) = IND_STA_ARR(NRM,I_REM,J2) 
                           SGN_STA_ARR(NRF,I_REF,J2) = SGN_STA_ARR(NRM,I_REM,J2) 
                           TIME_FRT(NRF,I_REF,J2) = TIME_FRT(NRM,I_REM,J2) 
                           GR_DEL(NRF,I_REF,J2) = GR_DEL(NRM,I_REM,J2) 
                           PH_RAT(NRF,I_REF,J2) = PH_RAT(NRM,I_REM,J2) 
                           GR_RAT(NRF,I_REF,J2) = GR_RAT(NRM,I_REM,J2) 
                           PHS(NRF,I_REF,J2)  = PHS(NRM,I_REM,J2) 
                           SNR(NRF,I_REF,J2)  = MIN ( SNR_PLR(PIMA__PCC_COD(1,J2)), &
          &                                           SNR_PLR(PIMA__PCC_COD(2,J2))  )
                           AMPL(NRF,I_REF,J2) = MIN ( AMPL_PLR(PIMA__PCC_COD(1,J2)), &
          &                                           AMPL_PLR(PIMA__PCC_COD(2,J2))  )
                           IF ( SNR_PLR(PIMA__PCC_COD(1,J2)) > SNR_PLR(PIMA__PCC_COD(2,J2)) ) THEN
                                IND_OBS_POL(NRF,I_REF,J2) = PIMA__PCC_COD(1,J2)
                              ELSE
                                IND_OBS_POL(NRF,I_REF,J2) = PIMA__PCC_COD(2,J2)
                           END IF
                      END IF
 420               CONTINUE 
              END IF
!
              DO 430 J3=1,2
                 DO 440 J4=1,2
!
! ----------------- Extract polarization label
!
                    POL_LAB(J4) = PIMA__POL_STR(PIM%STA(IND_STA(J3))%POL_TYP(J4):PIM%STA(IND_STA(J3))%POL_TYP(J4))
                    IF ( INDEX ( POL_ARR, POL_LAB(J4) ) < 1 ) THEN
                         IF ( ILEN(POL_ARR) == 0 ) THEN
                              POL_ARR = POL_LAB(J4)
                            ELSE 
                              POL_ARR = POL_ARR(1:I_LEN(POL_ARR))//POL_LAB(J4)
                         END IF
                    END IF
 440             CONTINUE 
 430         CONTINUE 
         END IF
 410  CONTINUE
      IF ( BPS_FINE_SEARCH == 'UNDF    ' ) THEN
           CALL ERR_LOG ( 6340, IUER, 'PIMA_BPASS_STA_LIST', 'Did not find '// &
     &         'FINE_SEARCH algorithm in section comment of file '// &
     &          PIM%CONF%FRINGE_FILE )
           RETURN
         ELSE IF ( BPS_FINE_SEARCH .EQ. PIMA__FINE_SEARCH_PAR ) THEN
           CONTINUE 
         ELSE IF ( BPS_FINE_SEARCH .NE. PIM%CONF%FRIB_FINE_SEARCH ) THEN
           CALL ERR_LOG ( 6341, IUER, 'PIMA_BPASS_STA_LIST', 'Mismatch in '// &
     &         'FINE_SEARCH algorithm: '//BPS_FINE_SEARCH//' for fringe '// &
     &         ' file, while '//PIM%CONF%FRIB_FINE_SEARCH//' for the '// &
     &         'control file -- such a case is not supported since '// &
     &         ' group delays and and phase delay rates may not be '// &
     &         'computed for such an algorithm' )
           RETURN
      END IF
!
      DO 450 J5=1,PIM%NSTA
         DO 460 J6=1,PIM__MPLR ! Cycle over parallel/cross/refrence/remote polarization combinations
            IF ( NOB(J5,J6) > 0 ) THEN
                 DO 470 J7=1,NOB(J5,J6)
                    SNR_ARR(J7) = -SNR(J7,J5,J6)
                    SNR_IND(J7) = J7 + 1.D-8
 470             CONTINUE
                 CALL SORT8 ( NOB(J5,J6), SNR_ARR, SNR_IND )
!
                 DO 480 J8=1,NOB(J5,J6)
                    IND_REC = SNR_IND(J8)
                    PIM%BPS%IND_OBS_SEL(J8,J5,J6) = IND_OBS_SEL(IND_REC,J5,J6)
                    PIM%BPS%IND_OBS_POL(J8,J5,J6) = IND_OBS_POL(IND_REC,J5,J6)
                    PIM%BPS%SNR(J8,J5,J6)         = SNR(IND_REC,J5,J6)
                    PIM%BPS%AMPL(J8,J5,J6)        = AMPL(IND_REC,J5,J6)
                    PIM%BPS%TIME_FRT(J8,J5,J6) = TIME_FRT(IND_REC,J5,J6)
                    PIM%BPS%GR_DEL(J8,J5,J6)   = GR_DEL(IND_REC,J5,J6)
                    PIM%BPS%PH_RAT(J8,J5,J6)   = PH_RAT(IND_REC,J5,J6)
                    PIM%BPS%GR_RAT(J8,J5,J6)   = GR_RAT(IND_REC,J5,J6)
                    PIM%BPS%PHS(J8,J5,J6)      = PHS(IND_REC,J5,J6)
!
                    IF ( PIM%CONF%DEBUG_LEVEL .GE. 5 ) THEN
                         IF ( PIM%C_STA(J5) == PIM%CONF%STA_REF ) THEN
                              STR = 'REF'
                            ELSE
                              STR = 'rem'
                         END IF
                         WRITE ( 6, 110 ) PIM%C_STA(J5), STR(1:3), PIMA__PCI_NAM(J6), &
     &                                    PIM%BPS%IND_OBS_SEL(J8,J5,J6), &
     &                                    PIM%BPS%SNR(J8,J5,J6), PIM%BPS%IND_OBS_POL(J8,J5,J6) 
 110                     FORMAT ( 'PIMA_BPASS_STA_LIST: ',A, 1X, A, 1X, A, 1X, &
     &                            'Obs: ', I6, ' SNR= ', F7.1, ' Ind_pol: ', I1 )
                 END IF
 480          CONTINUE
              IF ( PIM%CONF%DEBUG_LEVEL .GE. 5 ) THEN
                   WRITE ( 6, 110 ) ' '
              END IF
              PIM%BPS%NUM_OBS_ACCUM(J5,J6) = NOB(J5,J6)
              PIM%BPS%NUM_OBS_FINE(J5,J6)  = NOB(J5,J6)
              IF ( PIM%BPS%NUM_OBS_ACCUM(J5,J6) > PIM%CONF%BPS_NOBS_ACCUM +1 ) THEN
                   PIM%BPS%NUM_OBS_ACCUM(J5,J6) = PIM%CONF%BPS_NOBS_ACCUM
              END IF
              IF ( PIM%BPS%NUM_OBS_FINE(J5,J6) > PIM%CONF%BPS_NOBS_FINE ) THEN
                   PIM%BPS%NUM_OBS_FINE(J5,J6) = PIM%CONF%BPS_NOBS_FINE
              END IF
         END IF
 460     CONTINUE
 450  CONTINUE
!
!@      DO 490 J9=1,PIM%NSTA
!@         IF ( J9 == BPS%IND_STA_REF ) GOTO 490
!@         DO 4100 J10=1,2
!@            DO 4110 J11=1,NOB(J9,J10)
!@               SNR(J11,J10,BPS%IND_STA_REF) = SNR(J11,J10,J9) 
!@ 4100        CONTINUE 
!@ 4110    CONTINUE 
!@ 490  CONTINUE 
!
      LCHN = PIM%NCHN
      ALLOCATE ( PIM%BPS%CMPL(LCHN,PIM%NFRQ,PIM%NSTA), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( 8*LCHN*PIM%NFRQ*PIM%NSTA, STR )
           CALL ERR_LOG ( 6342, IUER, 'PIMA_BPASS_STA_LIST', 'Error in an '// &
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
           CALL ERR_LOG ( 6343, IUER, 'PIMA_BPASS_STA_LIST', 'Error in an '// &
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
           CALL ERR_LOG ( 6344, IUER, 'PIMA_BPASS_STA_LIST', 'Error in an '// &
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
           CALL ERR_LOG ( 6345, IUER, 'PIMA_BPASS_STA_LIST', 'Error in an '// &
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
           CALL ERR_LOG ( 6346, IUER, 'PIMA_BPASS_STA_LIST', 'Error in an '// &
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
           CALL ERR_LOG ( 6347, IUER, 'PIMA_BPASS_STA_LIST', 'Error in an '// &
     &         'attempt to allocate '//STR(1:I_LEN(STR))//' bytes of '// &
     &         'dynamic memory for PIM%BPS%PHAS_FRQ_RATE' )
           DEALLOCATE ( BUF )
           RETURN
      END IF
      CALL NOUT_R4 ( PIM%NFRQ*PIM%NSTA, PIM%BPS%PHAS_FRQ_RATE )
!
      PIM%BPS%STATUS = PIMA__BPASS_ALLOC
!
      DEALLOCATE ( BUF )
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  PIMA_BPASS_STA_LIST  !#!#
