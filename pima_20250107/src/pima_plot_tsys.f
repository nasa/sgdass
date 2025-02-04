      SUBROUTINE PIMA_PLOT_TSYS ( PIM, TSPL_DATA, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine PIMA_PLOT_TSYS
! *                                                                      *
! * ### 30-JAN-2006  PIMA_PLOT_TSYS  v6.0 (c)  L. Petrov 05-AUG-2020 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'astro_constants.i'
      INCLUDE   'pima.i'
      INCLUDE   'vtd.i'
      INCLUDE   'diagi.i'
      TYPE     ( PIMA__TYPE ) :: PIM
      CHARACTER  TSPL_DATA*(*)
      INTEGER*4  IUER
      TYPE     ( DIAGI_STRU ) :: DIA(PIM__MSTA)
      INTEGER*4  MARR, MPB, NN
      PARAMETER  ( MARR = 8192 )
      PARAMETER  ( MPB  =    6 )
      REAL*8     T8(MARR,PIM__MSTA), X8(MARR,PIM__MSTA),  &
     &           T9(MARR,PIM__MSTA), X9(MARR,PIM__MSTA),  &
     &           TIME(MARR), TSYS(MARR), ELEV(MARR), ELEV_RAD(MARR)
      REAL*8     TIME_MOD(MARR), TSYS_MOD(MARR), TSYS_ZEN(MARR), TSYS_T0(MARR), &
     &           ELEV_T0(MARR), TIME_OUT(MARR), ELEV_OUT(MARR), TSYS_OUT(MARR)
      REAL*8     TIME_T(MARR), TSYS_T(MARR), ELEV_E(MARR), TSYS_E(MARR)
      REAL*8     TSYS_ZEN_MEAN, TSYS_RMS, TSYS_MIN, TSYS_MAX, TSYS_0, TIM_DIF, &
     &           TSPILL, EL_DEG, TSYS_VAL(PIM__MFRQ), TIM_DIF_MIN, TSYS_NO, &
     &           TSYS_NOM, TSPL_FUDGE, TIM_STMO, TATM__MIN, EL__MIN
      PARAMETER  ( TATM__MIN = 0.001 )
      PARAMETER  ( EL__MIN   = 3.0D0*DEG__TO__RAD )
      CHARACTER  COMMON_TIT*80, TITS(PIM__MSTA)*16, BUTTON_NAME(MPB)*32, &
     &           BUTTON_LET(MPB)*2, PREF_NAME*128, VALUE_STR*32, FILOUT*128, &
     &           STR*128, POLAR_STR*2, TIM_STR*30, FRQ_STR*21, TAT_STR*20, &
     &           OPA_STR*20, SOU_NAME*8, PIMAVAR_TSPL_FUDGE*21
      CHARACTER, ALLOCATABLE :: OUT(:)*4096
      LOGICAL*1  FLAG_INI(MARR)
      INTEGER*4  IND_STA, IND_CHA, NP(PIM__MSTA), NZ(PIM__MSTA), IND_FRQ, MODE, &
     &           NC, NR, NM, NT, NE, IREF_INI(MARR), ICODE, IVRB, IFRQ, &
     &           J1, J2, J3, J4, J5, J6, J7, J8, J9, J10, J11, J12, J13, J14, &
     &           J15, J16, J17, MO, NO, IB, IE, IL, &
     &           IND_POL, IND_TAT, IND_SCA, IND_SOU, IND_TSYS, IER
      CHARACTER  ZAG*128, UNIT*128
      REAL*8     EL_LIM_MIN, TSYS_LIM_MIN, TSYS_LIM_MAX
      REAL*8       PIMA__SCM
      PARAMETER  ( PIMA__SCM = 1.25D0 )
      PARAMETER  ( EL_LIM_MIN   = 2.0D0*DEG__TO__RAD )
      PARAMETER  ( TSYS_LIM_MIN = 10.0D0 )
      PARAMETER  ( TSYS_LIM_MAX = 5000.0D0 )
      LOGICAL*1  FL_TSYS, FL_TATM, FL_MAPFUN_ELEV
      INTEGER*4  IBST, ILST, IOST, IPST, IWST, IDEV, &
     &           ICL1, ICL2, ICL3
      INTEGER*4, EXTERNAL :: ILEN, I_LEN
      LOGICAL*4, EXTERNAL :: IS_R8_NAN
      CHARACTER, EXTERNAL :: MJDSEC_TO_DATE*30, GET_CDATE*19
#ifdef NO_PLOT
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  PIMA_PLOT_TSYS  !#!
#else
      T8 = 0.0D0
      X8 = 0.0D0
      T9 = 0.0D0
      X9 = 0.0D0
      TIME = 0.0D0
      TSYS = 0.0D0
      ELEV = 0.0D0
      ELEV_RAD = 0.0D0
!
      CALL GETENVAR ( 'PIMAVAR_TSPL_FUDGE', PIMAVAR_TSPL_FUDGE )
      IF ( ILEN(PIMAVAR_TSPL_FUDGE) == 0 ) THEN
           TSPL_FUDGE = 1.0D0
         ELSE
           READ ( UNIT=PIMAVAR_TSPL_FUDGE, FMT=*, IOSTAT=IER ) TSPL_FUDGE
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 5711, IUER, 'PIMA_PLOT_TSYS', 'Wringe value of '// &
     &              'PIMAVAR_TSPL_FUDGE enviroment variable '//TRIM(STR)// &
     &              ' while a float number was expected' )
                RETURN 
           END IF
           WRITE ( 6, '(A,F8.4)' ) 'PIMAVAR_TSPL_FUDGE= ', TSPL_FUDGE
      END IF
!
      CALL GETENVAR ( 'PIMAVAR_MAPFUN_INSTEAD_OF_ELEV', STR )
      IF ( STR(1:3) == 'YES' .OR. STR(1:3) == 'yes' ) THEN
           FL_MAPFUN_ELEV = .TRUE.
           WRITE ( 6, '(A)' ) 'PIMAVAR_MAPFUN_INSTEAD_OF_ELEV= '//TRIM(STR)
         ELSE 
           FL_MAPFUN_ELEV = .FALSE.
      END IF
!
! --- Setting defaults values of the plotting parameters
!
      CALL ERR_PASS   ( IUER, IER )
      CALL DIAGI_DEF  ( IBST, ILST, IOST, IPST, IWST, IDEV, ZAG, UNIT, &
     &                  ICL1, ICL2, ICL3, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 5712, IUER, 'PIMA_PLOT_TSYS', 'Error in setting '// &
     &         'default values for the plot' )
           RETURN
      END IF
!
      MODE = 1
      IF ( PIM%CONF%DEBUG_LEVEL == 5 ) MODE = 3
      IND_FRQ = PIM%CONF%BEG_FRQ
      IVRB = 1
!
 910  CONTINUE
      IF ( MODE == 1 ) THEN
           PREF_NAME = '/tmp/tsys_'// &
     &               PIM%CONF%SESS_CODE(1:I_LEN(PIM%CONF%SESS_CODE))//'_time_'
         ELSE IF ( MODE == 2 ) THEN
           PREF_NAME = '/tmp/tsys_'// &
     &               PIM%CONF%SESS_CODE(1:I_LEN(PIM%CONF%SESS_CODE))//'_elev_'
         ELSE IF ( MODE == 3 ) THEN
           PREF_NAME = '/tmp/tsys_zen_'// &
     &               PIM%CONF%SESS_CODE(1:I_LEN(PIM%CONF%SESS_CODE))//'_time_'
         ELSE IF ( MODE == 4 ) THEN
           PREF_NAME = '/tmp/tsys_dep_'// &
     &               PIM%CONF%SESS_CODE(1:I_LEN(PIM%CONF%SESS_CODE))//'_elev_'
         ELSE IF ( MODE == 5 ) THEN
           PREF_NAME = '/tmp/tsys_dep_'// &
     &               PIM%CONF%SESS_CODE(1:I_LEN(PIM%CONF%SESS_CODE))//'_timel_'
      END IF
!
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
      IF ( TSPL_DATA == PIMA__TABL .OR. TSPL_DATA == PIMA__TABMOD ) THEN
           MODE = 0
           IF ( TSPL_DATA == PIMA__TABMOD ) THEN
                FILOUT = TRIM(PIM%CONF%EXPER_DIR)//'/'//TRIM(PIM%CONF%SESS_CODE)//'_tzen_tsys.txt'
              ELSE 
                IF ( TSPL_DATA == PIMA__TABL .AND. PIM%CONF%TSYS_CAL_CODE == PIMA__TSYS_MEASURED ) THEN
                     FILOUT = TRIM(PIM%CONF%EXPER_DIR)//'/'//TRIM(PIM%CONF%SESS_CODE)//'_mea_tsys.txt'
                   ELSE IF ( PIM%CONF%TSYS_CAL_CODE == PIMA__TSYS_CLEANED ) THEN
                     FILOUT = TRIM(PIM%CONF%EXPER_DIR)//'/'//TRIM(PIM%CONF%SESS_CODE)//'_cln_tsys.txt'
                   ELSE IF ( PIM%CONF%TSYS_CAL_CODE == PIMA__TSYS_MODELED ) THEN
                     FILOUT = TRIM(PIM%CONF%EXPER_DIR)//'/'//TRIM(PIM%CONF%SESS_CODE)//'_mdl_tsys.txt'
                   ELSE
                     CALL ERR_LOG ( 5713, IUER, 'PIMA_PLOT_TSYS', 'Tsys mode '// &
     &                    PIM%CONF%TSYS_CAL_CODE//' is not compatible with the '// &
     &                   'data type '//TSPL_DATA )
                     RETURN
                END IF
           END IF 
           MO = 256 + 8*PIM%NOBS
           ALLOCATE ( OUT(MO), STAT=IER )
           IF ( IER .NE. 0 ) THEN
                CALL CLRCH ( STR )
                CALL IINCH ( MO*LEN(OUT(1)), STR )
                CALL ERR_LOG ( 5714, IUER, 'PIMA_PLOT_TSYS', 'Error in allocating '// &
     &               STR(1:I_LEN(STR))//' bytes for array OUT' )
                RETURN
           END IF
           NO = 1
           IF ( TSPL_DATA == PIMA__TABL ) THEN
                OUT(NO) = PIMA__TSOU_LABEL
              ELSE IF ( TSPL_DATA == PIMA__TABMOD ) THEN
                OUT(NO) = PIMA__TSZE_LABEL
           ENDIF
           NO = NO + 1 ; OUT(NO) = '#' 
           NO = NO + 1 ; OUT(NO) = '# Generated on '//GET_CDATE()
           NO = NO + 1 ; OUT(NO) = '#' 
           NO = NO + 1 ; OUT(NO) = '# EXPER: '//PIM%CONF%SESS_CODE
           IF ( TSPL_DATA == PIMA__TABMOD ) THEN
                NO = NO + 1 ; OUT(NO) = '# MODE:  tzen_elev'
              ELSE 
                NO = NO + 1 ; OUT(NO) = '# MODE:  '//PIM%CONF%TSYS_CAL_CODE
           END IF
           NO = NO + 1 ; OUT(NO) = '# POLAR: '//POLAR_STR
           CALL INCH ( PIM%NFRQ, STR )
           NO = NO + 1 ; OUT(NO) = '# N_FRQ: '//TRIM(STR)
      END IF
      DO 410 J1=1,PIM%NSTA
         IF ( ( PIM%CONF%TSYS_CAL_CODE == PIMA__TSYS_CLEANED .OR. &
     &          PIM%CONF%TSYS_CAL_CODE == PIMA__TSYS_MODELED      ) .AND. &
     &              PIM%STA(J1)%TSYS(PIM%CONF%FRQ_GRP)%AVAIL        .AND. &
     &        .NOT. PIM%STA(J1)%STMO(PIM%CONF%FRQ_GRP)%TSYS_AVAIL         ) THEN
              IF ( PIM%CONF%CHECK_SEVERITY < 2 ) THEN
                   WRITE ( 6, '(A)' ) 'PIMA_PLOT_TSYS: Tsys model was not computed for station '// &
     &                                 PIM%STA(J1)%IVS_NAME//' while TSYS: '// &
     &                                 PIM%CONF%TSYS_CAL_CODE//' is specified. '
                   WRITE ( 6, '(A)' ) 'You need run task tsmo'
                   WRITE ( 6, '(A)' ) 'Nevertheless, continue' 
                   GOTO 410
                ELSE
                   CALL ERR_LOG ( 5715, IUER, 'PIMA_PLOT_TSYS', 'Trap of internal '// &
     &                 'control: Tsys model was not computed for station '// &
     &                  PIM%STA(J1)%IVS_NAME//' while TSYS: '//PIM%CONF%TSYS_CAL_CODE// &
     &                 ' is specified. You need run task tsmo' )
                   RETURN
              END IF
         END IF
!
         IF ( TSPL_DATA == PIMA__TABL .AND.  PIM%STA(J1)%STMO(PIM%CONF%FRQ_GRP)%N_OPA > 0 ) THEN
              NO = NO + 1 ; OUT(NO) = '#' 
         END IF
         IF ( TSPL_DATA == PIMA__TABMOD ) THEN
              NO = NO + 1 ; OUT(NO) = '#' 
         END IF
!
         NP(J1) = 0
         CALL NOUT ( SIZEOF(DIA(J1)), DIA(J1) )
         IF ( .NOT. PIM%STA(J1)%TSYS(PIM%CONF%FRQ_GRP)%AVAIL ) THEN
              DIA(J1)%NAME   = PIM%STA(J1)%IVS_NAME
              IF ( MODE == 5 ) THEN
                   TITS(J1) = 'Elev '//PIM%STA(J1)%IVS_NAME
                ELSE
                   TITS(J1) = 'Tsys '//PIM%STA(J1)%IVS_NAME
              END IF
              GOTO 410
         END IF
         IF ( PIM%CONF%DEBUG_LEVEL .GE. 3 ) THEN
              WRITE ( 6, 230 ) PIM%C_STA(J1), PIM%STA(J1)%TSYS(PIM%CONF%FRQ_GRP)%NPOI
 230          FORMAT ( 'PIMS_PLOT_TSYS Sta: ', A, ' Num_tsys: ', I4 )
         END IF
         IF ( TSPL_DATA == PIMA__MEASURED .OR. TSPL_DATA == PIMA__TABMOD ) THEN
              IF ( PIM%CONF%TSYS_CAL_CODE == PIMA__TSYS_MEASURED ) THEN
                   DO 420 J2=1,PIM%STA(J1)%TSYS(PIM%CONF%FRQ_GRP)%NPOI
                      IF ( IS_R8_NAN ( PIM%STA(J1)%TSYS(PIM%CONF%FRQ_GRP)%TSYS(IND_FRQ,J2,IND_POL) ) ) GOTO 420
                      NP(J1) = NP(J1) + 1
                      IF ( MODE == 1 ) THEN
                           T8(NP(J1),J1) = PIM%STA(J1)%TSYS(PIM%CONF%FRQ_GRP)%TIME_MID_R8(J2)
                           X8(NP(J1),J1) = PIM%STA(J1)%TSYS(PIM%CONF%FRQ_GRP)%TSYS(IND_FRQ,J2,IND_POL)
                        ELSE IF ( MODE == 2  ) THEN
                           T8(NP(J1),J1) = PIM%STA(J1)%TSYS(PIM%CONF%FRQ_GRP)%ELEV_R4(J2)/DEG__TO__RAD
                           IF ( FL_MAPFUN_ELEV ) THEN
                                IF ( PIM%STA(J1)%TSYS(PIM%CONF%FRQ_GRP)%ELEV_R4(J2) > EL__MIN ) THEN
                                     T8(NP(J1),J1) = 1.D0/SIN(PIM%STA(J1)%TSYS(PIM%CONF%FRQ_GRP)%ELEV_R4(J2))
                                   ELSE 
                                     T8(NP(J1),J1) = 1.D0/SIN(EL__MIN)
                                END IF
                           END IF
                           X8(NP(J1),J1) = PIM%STA(J1)%TSYS(PIM%CONF%FRQ_GRP)%TSYS(IND_FRQ,J2,IND_POL)
                        ELSE IF ( MODE == 3  .OR.  MODE == 4  ) THEN
                           TIME(NP(J1)) = PIM%STA(J1)%TSYS(PIM%CONF%FRQ_GRP)%TIME_MID_R8(J2)
                           TSYS(NP(J1)) = PIM%STA(J1)%TSYS(PIM%CONF%FRQ_GRP)%TSYS(IND_FRQ,J2,IND_POL)
                           ELEV(NP(J1)) = PIM%STA(J1)%TSYS(PIM%CONF%FRQ_GRP)%ELEV_R4(J2)/DEG__TO__RAD
                           IF ( FL_MAPFUN_ELEV ) THEN
                                IF ( PIM%STA(J1)%TSYS(PIM%CONF%FRQ_GRP)%ELEV_R4(J2) > EL__MIN ) THEN
                                     ELEV(NP(J1)) = 1.D0/SIN(PIM%STA(J1)%TSYS(PIM%CONF%FRQ_GRP)%ELEV_R4(J2))
                                   ELSE 
                                     ELEV(NP(J1)) = 1.D0/SIN(EL__MIN)
                                END IF
                           END IF
                           IF ( TSYS(NP(J1)) > TSYS_LIM_MAX ) TSYS(NP(J1)) = 0.0D0
                        ELSE IF ( MODE == 5 ) THEN
                           T8(NP(J1),J1) = PIM%STA(J1)%TSYS(PIM%CONF%FRQ_GRP)%TIME_MID_R8(J2)
                           X8(NP(J1),J1) = PIM%STA(J1)%TSYS(PIM%CONF%FRQ_GRP)%ELEV_R4(J2)/DEG__TO__RAD
                           IF ( FL_MAPFUN_ELEV ) THEN
                                IF ( PIM%STA(J1)%TSYS(PIM%CONF%FRQ_GRP)%ELEV_R4(J2) > EL__MIN ) THEN
                                     X8(NP(J1),J1) = 1.D0/SIN(PIM%STA(J1)%TSYS(PIM%CONF%FRQ_GRP)%ELEV_R4(J2))
                                   ELSE 
                                     X8(NP(J1),J1) = 1.D0/SIN(EL__MIN)
                                END IF
                           END IF
                      END IF
!
                      IF ( PIM%CONF%DEBUG_LEVEL .GE. 4 ) THEN
                           STR = MJDSEC_TO_DATE ( PIM%MJD_0, PIM%TAI_0 + PIM%STA(J1)%TSYS(PIM%CONF%FRQ_GRP)%TIME_MID_R8(J2), -2 )
                           IF ( PIM%STA(J1)%TSYS(PIM%CONF%FRQ_GRP)%SOU_IND(J2) > 0 ) THEN
                                SOU_NAME = PIM%SOU(PIM%STA(J1)%TSYS(PIM%CONF%FRQ_GRP)%SOU_IND(J2))%IVS_NAME
                              ELSE
                                SOU_NAME = 'unknown'
                           END IF
                           WRITE ( 6, 210 ) J2, PIM%STA(J1)%IVS_NAME, &
     &                                      SOU_NAME, &
     &                                      STR(1:21), PIM%STA(J1)%TSYS(PIM%CONF%FRQ_GRP)%ELEV_R4(J2)/DEG__TO__RAD, &
     &                                      'meas', &
     &                                      PIM%STA(J1)%TSYS(PIM%CONF%FRQ_GRP)%TSYS(1:PIM%NFRQ,J2,IND_POL)
 210                       FORMAT ( I4, ' ) ', A, 2X, A, 2X, A, ' Elev:   ', F4.1, '  Tsys_', A, ': ', &
     &                              16( F7.1,1X) )
 220                       FORMAT ( I4, ' ) ', A, 2X, A, 2X, A, ' Tim: ', F7.1, '  Tsys_', A, ': ', &
     &                              16( F7.1,1X) )
                      END IF
 420               CONTINUE
                ELSE IF ( PIM%CONF%TSYS_CAL_CODE == PIMA__TSYS_CLEANED .OR. &
     &                    PIM%CONF%TSYS_CAL_CODE == PIMA__TSYS_MODELED      ) THEN
                   DO 430 J3=1,PIM%STA(J1)%STMO(PIM%CONF%FRQ_GRP)%N_TSYS-1,2
                      IF ( IS_R8_NAN ( PIM%STA(J1)%STMO(PIM%CONF%FRQ_GRP)%TSYS_MOD(J3,IND_FRQ,IND_POL)  ) ) GOTO 430
                      IF ( PIM%STA(J1)%STMO(PIM%CONF%FRQ_GRP)%TSYS_MOD(J3,IND_FRQ,IND_POL) < TSYS_LIM_MIN ) GOTO 430
                      NP(J1) = NP(J1) + 1
!
                      IF ( PIM%CONF%DEBUG_LEVEL .GE. 4 .AND. MODE .NE. 5 ) THEN
                           STR = MJDSEC_TO_DATE ( PIM%MJD_0, PIM%TAI_0 + PIM%STA(J1)%STMO(PIM%CONF%FRQ_GRP)%TIM(J3), -2 )
                           IF ( PIM%CONF%TSYS_CAL_CODE == PIMA__TSYS_CLEANED ) STR(31:34) = 'clea'
                           IF ( PIM%CONF%TSYS_CAL_CODE == PIMA__TSYS_MODELED ) STR(31:34) = 'modl'
                      END IF
!
                      IF ( MODE == 1 ) THEN
                           T8(NP(J1),J1) = PIM%STA(J1)%STMO(PIM%CONF%FRQ_GRP)%TIM(J3)
                           IF ( PIM%CONF%TSYS_CAL_CODE == PIMA__TSYS_CLEANED ) THEN
                                X8(NP(J1),J1) = PIM%STA(J1)%STMO(PIM%CONF%FRQ_GRP)%TSYS_CLN(J3,IND_FRQ,IND_POL)
                              ELSE IF ( PIM%CONF%TSYS_CAL_CODE == PIMA__TSYS_MODELED ) THEN
                                X8(NP(J1),J1) = PIM%STA(J1)%STMO(PIM%CONF%FRQ_GRP)%TSYS_MOD(J3,IND_FRQ,IND_POL)
                           END IF
                           IF ( PIM%CONF%DEBUG_LEVEL .GE. 4 ) THEN
                                WRITE ( 6, 220 ) J3, PIM%STA(J1)%IVS_NAME, 'tzen-dep', STR(1:21), &
     &                                           T8(NP(J1),J1), STR(31:34), X8(NP(J1),J1)
                           END IF
                        ELSE IF ( MODE == 2  ) THEN
                           T8(NP(J1),J1) = PIM%STA(J1)%STMO(PIM%CONF%FRQ_GRP)%EL(J3)/DEG__TO__RAD
                           IF ( FL_MAPFUN_ELEV ) THEN
                                IF ( PIM%STA(J1)%STMO(PIM%CONF%FRQ_GRP)%EL(J3) > EL__MIN ) THEN
                                     T8(NP(J1),J1) = 1.D0/SIN(PIM%STA(J1)%STMO(PIM%CONF%FRQ_GRP)%EL(J3))
                                   ELSE 
                                     T8(NP(J1),J1) = 1.D0/SIN(EL__MIN)
                                END IF
                           END IF
                           IF ( PIM%CONF%TSYS_CAL_CODE == PIMA__TSYS_CLEANED ) THEN
                                X8(NP(J1),J1) = PIM%STA(J1)%STMO(PIM%CONF%FRQ_GRP)%TSYS_CLN(J3,IND_FRQ,IND_POL)
                              ELSE IF ( PIM%CONF%TSYS_CAL_CODE == PIMA__TSYS_MODELED ) THEN
                                X8(NP(J1),J1) = PIM%STA(J1)%STMO(PIM%CONF%FRQ_GRP)%TSYS_MOD(J3,IND_FRQ,IND_POL)
                           END IF                     
                           IF ( PIM%CONF%DEBUG_LEVEL .GE. 4 ) THEN
                                WRITE ( 6, 210 ) J3, PIM%STA(J1)%IVS_NAME, 'elev-dep', &
     &                                           STR(1:21), T8(NP(J1),J1), STR(31:34), X8(NP(J1),J1)
                           END IF
                        ELSE IF ( MODE == 3  .OR.  MODE == 4  ) THEN
                           TIME(NP(J1)) = PIM%STA(J1)%STMO(PIM%CONF%FRQ_GRP)%TIM(J3)
                           IF ( PIM%CONF%TSYS_CAL_CODE == PIMA__TSYS_CLEANED ) THEN
                                TSYS(NP(J1)) = PIM%STA(J1)%STMO(PIM%CONF%FRQ_GRP)%TSYS_CLN(J3,IND_FRQ,IND_POL)
                              ELSE
                                TSYS(NP(J1)) = PIM%STA(J1)%STMO(PIM%CONF%FRQ_GRP)%TSYS_MOD(J3,IND_FRQ,IND_POL)
                           END IF                     
                           ELEV(NP(J1)) = PIM%STA(J1)%STMO(PIM%CONF%FRQ_GRP)%EL(J3)/DEG__TO__RAD
                           IF ( FL_MAPFUN_ELEV ) THEN
                                IF ( PIM%STA(J1)%STMO(PIM%CONF%FRQ_GRP)%EL(J3) > EL__MIN ) THEN
                                     ELEV(NP(J1)) = 1.D0/SIN(PIM%STA(J1)%STMO(PIM%CONF%FRQ_GRP)%EL(J3))
                                   ELSE 
                                     ELEV(NP(J1)) = 1.D0/SIN(EL__MIN)
                                END IF
                           END IF
                           IF ( TSYS(NP(J1)) > TSYS_LIM_MAX ) TSYS(NP(J1)) = 0.0D0
!
                           IF ( PIM%CONF%DEBUG_LEVEL .GE. 4 ) THEN
                                WRITE ( 6, 210 ) J3, PIM%STA(J1)%IVS_NAME, 'elev-dep', &
     &                                           STR(1:21), ELEV(NP(J1)), STR(31:34), TSYS(NP(J1))
                                WRITE ( 6, 220 ) J3, PIM%STA(J1)%IVS_NAME, 'tzen-dep', STR(1:21), &
     &                                           TIME(NP(J1)), STR(31:34), TSYS(NP(J1))
                           END IF
                        ELSE IF ( MODE == 5 ) THEN
                           T8(NP(J1),J1) = PIM%STA(J1)%STMO(PIM%CONF%FRQ_GRP)%TIM(J3)
                           X8(NP(J1),J1) = PIM%STA(J1)%STMO(PIM%CONF%FRQ_GRP)%EL(J3)/DEG__TO__RAD
                           IF ( FL_MAPFUN_ELEV ) THEN
                                IF ( PIM%STA(J1)%STMO(PIM%CONF%FRQ_GRP)%EL(J3) > EL__MIN ) THEN
                                     X8(NP(J1),J1) = 1.D0/SIN(PIM%STA(J1)%STMO(PIM%CONF%FRQ_GRP)%EL(J3))
                                   ELSE 
                                     X8(NP(J1),J1) = 1.D0/SIN(EL__MIN)
                                END IF
                           END IF
                      END IF
 430               CONTINUE 
              END IF
           ELSE IF ( TSPL_DATA == PIMA__NWM  .AND.  PIM%STA(J1)%STMO(PIM%CONF%FRQ_GRP)%N_TAT > 0 ) THEN
              DO 440 J4=1,PIM%STA(J1)%STMO(PIM%CONF%FRQ_GRP)%N_TAT 
                 IF ( PIM%STA(J1)%STMO(PIM%CONF%FRQ_GRP)%EL(J4) < PIMA__SPD_ELMIN ) GOTO 440
                 NP(J1) = NP(J1) + 1
                 IF ( MODE == 1 ) THEN
                      T8(NP(J1),J1) = PIM%STA(J1)%STMO(PIM%CONF%FRQ_GRP)%TIM(J4)
                      X8(NP(J1),J1) = PIM%STA(J1)%STMO(PIM%CONF%FRQ_GRP)%TAT(J4,IND_FRQ)
                   ELSE IF ( MODE == 2  ) THEN
                      T8(NP(J1),J1) = PIM%STA(J1)%STMO(PIM%CONF%FRQ_GRP)%EL(J4)/DEG__TO__RAD
                      IF ( FL_MAPFUN_ELEV ) THEN
                           IF ( PIM%STA(J1)%STMO(PIM%CONF%FRQ_GRP)%EL(J4) > EL__MIN ) THEN
                                T8(NP(J1),J1) = 1.D0/SIN(PIM%STA(J1)%STMO(PIM%CONF%FRQ_GRP)%EL(J4))
                              ELSE 
                                T8(NP(J1),J1) = 1.D0/SIN(EL__MIN)
                           END IF
                      END IF
                      X8(NP(J1),J1) = PIM%STA(J1)%STMO(PIM%CONF%FRQ_GRP)%TAT(J4,IND_FRQ)
                   ELSE IF ( MODE == 3  .OR.  MODE == 4  ) THEN
                      TIME(NP(J1)) = PIM%STA(J1)%STMO(PIM%CONF%FRQ_GRP)%TIM(J4)
                      TSYS(NP(J1)) = PIM%STA(J1)%STMO(PIM%CONF%FRQ_GRP)%TAT(J4,IND_FRQ)
                      ELEV(NP(J1)) = PIM%STA(J1)%STMO(PIM%CONF%FRQ_GRP)%EL(J4)/DEG__TO__RAD
                      IF ( FL_MAPFUN_ELEV ) THEN
                           IF ( PIM%STA(J1)%STMO(PIM%CONF%FRQ_GRP)%EL(J4) > EL__MIN ) THEN
                                ELEV(NP(J1)) = 1.D0/SIN(PIM%STA(J1)%STMO(PIM%CONF%FRQ_GRP)%EL(J4))
                              ELSE 
                                ELEV(NP(J1)) = 1.D0/SIN(EL__MIN)
                           END IF
                      END IF
                      IF ( TSYS(NP(J1)) > TSYS_LIM_MAX ) TSYS(NP(J1)) = 0.0D0
                   ELSE IF ( MODE == 5 ) THEN
                      T8(NP(J1),J1) = PIM%STA(J1)%STMO(PIM%CONF%FRQ_GRP)%TIM(J4)
                      X8(NP(J1),J1) = PIM%STA(J1)%STMO(PIM%CONF%FRQ_GRP)%EL(J4)/DEG__TO__RAD 
                      IF ( FL_MAPFUN_ELEV ) THEN
                           IF ( PIM%STA(J1)%STMO(PIM%CONF%FRQ_GRP)%EL(J4) > EL__MIN ) THEN
                                X8(NP(J1),J1) = 1.D0/SIN(PIM%STA(J1)%STMO(PIM%CONF%FRQ_GRP)%EL(J4))
                              ELSE 
                                X8(NP(J1),J1) = 1.D0/SIN(EL__MIN)
                           END IF
                      END IF
                 END IF
 440          CONTINUE 
           ELSE IF ( TSPL_DATA == PIMA__OPACITY .AND.  PIM%STA(J1)%STMO(PIM%CONF%FRQ_GRP)%N_OPA > 0 ) THEN
              DO 450 J5=1,PIM%STA(J1)%STMO(PIM%CONF%FRQ_GRP)%N_TAT 
                 IF ( PIM%STA(J1)%STMO(PIM%CONF%FRQ_GRP)%EL(J5) < PIMA__SPD_ELMIN ) GOTO 450
                 NP(J1) = NP(J1) + 1
                 IF ( MODE == 1 ) THEN
                      T8(NP(J1),J1) = PIM%STA(J1)%STMO(PIM%CONF%FRQ_GRP)%TIM(J5)
                      X8(NP(J1),J1) = PIM%STA(J1)%STMO(PIM%CONF%FRQ_GRP)%OPA(J5,IND_FRQ)
                   ELSE IF ( MODE == 2  ) THEN
                      T8(NP(J1),J1) = PIM%STA(J1)%STMO(PIM%CONF%FRQ_GRP)%EL(J5)/DEG__TO__RAD
                      X8(NP(J1),J1) = PIM%STA(J1)%STMO(PIM%CONF%FRQ_GRP)%OPA(J5,IND_FRQ)
                   ELSE IF ( MODE == 3  .OR.  MODE == 4  ) THEN
                      TIME(NP(J1)) = PIM%STA(J1)%STMO(PIM%CONF%FRQ_GRP)%TIM(J5)
                      TSYS(NP(J1)) = PIM%STA(J1)%STMO(PIM%CONF%FRQ_GRP)%OPA(J5,IND_FRQ)
                      ELEV(NP(J1)) = PIM%STA(J1)%STMO(PIM%CONF%FRQ_GRP)%EL(J5)/DEG__TO__RAD
                      IF ( TSYS(NP(J1)) > TSYS_LIM_MAX ) TSYS(NP(J1)) = 0.0D0
                   ELSE IF ( MODE == 5 ) THEN
                      T8(NP(J1),J1) = PIM%STA(J1)%STMO(PIM%CONF%FRQ_GRP)%TIM(J5)
                      X8(NP(J1),J1) = PIM%STA(J1)%STMO(PIM%CONF%FRQ_GRP)%EL(J5)/DEG__TO__RAD 
                 END IF
!
                 IF ( FL_MAPFUN_ELEV ) THEN
                      IF ( PIM%STA(J1)%STMO(PIM%CONF%FRQ_GRP)%EL(J4) > EL__MIN ) THEN
                           ELEV(NP(J1)) = 1.D0/SIN(PIM%STA(J1)%STMO(PIM%CONF%FRQ_GRP)%EL(J4))
                         ELSE 
                           ELEV(NP(J1)) = 1.D0/SIN(EL__MIN)
                      END IF
                 END IF
 450          CONTINUE 
           ELSE IF ( TSPL_DATA == PIMA__TREC .AND.  PIM%STA(J1)%STMO(PIM%CONF%FRQ_GRP)%N_OPA > 0 ) THEN
              DO 460 J6=1,PIM%STA(J1)%TSYS(PIM%CONF%FRQ_GRP)%NPOI
                 IF ( PIM%STA(J1)%TSYS(PIM%CONF%FRQ_GRP)%SOU_IND(J6) == 0 ) GOTO 460
                 NP(J1) = NP(J1) + 1
                 IF ( MODE == 1 ) THEN
                      T8(NP(J1),J1) = PIM%STA(J1)%TSYS(PIM%CONF%FRQ_GRP)%TIME_MID_R8(J6)
                    ELSE IF ( MODE == 2 .OR. MODE == 3 .OR. MODE == 4 .OR. MODE == 5 ) THEN
                      T8(NP(J1),J1) = PIM%STA(J1)%TSYS(PIM%CONF%FRQ_GRP)%ELEV_R4(J6)/DEG__TO__RAD
                      IF ( FL_MAPFUN_ELEV ) THEN
                           IF ( PIM%STA(J1)%TSYS(PIM%CONF%FRQ_GRP)%ELEV_R4(J6) > EL__MIN ) THEN
                                X8(NP(J1),J1) = 1.D0/SIN(PIM%STA(J1)%TSYS(PIM%CONF%FRQ_GRP)%ELEV_R4(J6))
                              ELSE 
                                X8(NP(J1),J1) = 1.D0/SIN(EL__MIN)
                           END IF
                     END IF
                 END IF
                 TIM_DIF = 1.D6
                 DO 470 J7=1,PIM%STA(J1)%STMO(PIM%CONF%FRQ_GRP)%N_TAT 
                    IF ( PIM%STA(J1)%STMO(PIM%CONF%FRQ_GRP)%EL(J7) < PIMA__SPD_ELMIN ) GOTO 470
                    IF ( DABS(PIM%STA(J1)%STMO(PIM%CONF%FRQ_GRP)%TIM(J7) - &
     &                        PIM%STA(J1)%TSYS(PIM%CONF%FRQ_GRP)%TIME_MID_R8(J6) ) < TIM_DIF ) THEN
                         TIM_DIF = DABS(PIM%STA(J1)%STMO(PIM%CONF%FRQ_GRP)%TIM(J7) - &
     &                                  PIM%STA(J1)%TSYS(PIM%CONF%FRQ_GRP)%TIME_MID_R8(J6))
                         IND_TAT = J7
                    END IF
 470             CONTINUE 
                 IF ( TIM_DIF > 300.0D0 ) THEN
                      NP(J1) = NP(J1) - 1
                     GOTO 460
                 END IF
                 EL_DEG = PIM%STA(J1)%TSYS(PIM%CONF%FRQ_GRP)%ELEV_R4(J6)/DEG__TO__RAD
                 IF ( EL_DEG > 60 ) THEN
                      TSPILL = 0.0D0
                    ELSE IF ( EL_DEG > 50.D0 ) THEN
                      TSPILL = 1.0D0
                    ELSE IF ( EL_DEG > 40.D0 ) THEN
                      TSPILL = 2.0D0
                    ELSE IF ( EL_DEG > 30.D0 ) THEN
                      TSPILL = 5.0D0
                    ELSE IF ( EL_DEG > 25.D0 ) THEN
                      TSPILL = 6.5D0
                    ELSE IF ( EL_DEG > 20.D0 ) THEN
                      TSPILL = 9.0D0
                    ELSE IF ( EL_DEG > 15.D0 ) THEN
                      TSPILL = 11.0D0
                    ELSE IF ( EL_DEG >  2.D0 ) THEN
                      TSPILL = 12.0D0
                 END IF
              TSPILL = 0.0 ! %%% $$$$$$$$$$$$$$$$
                 X8(NP(J1),J1) = PIM%STA(J1)%TSYS(PIM%CONF%FRQ_GRP)%TSYS(IND_FRQ,J6,IND_POL) - &
     &                           TSPL_FUDGE*PIM%STA(J1)%STMO(PIM%CONF%FRQ_GRP)%TAT(IND_TAT,IND_FRQ) - &
     &                           TSPILL
 460          CONTINUE 
         END IF
!
         IF ( TSPL_DATA == PIMA__TABL .AND. PIM%CONF%TSYS_CAL_CODE == PIMA__TSYS_MEASURED ) THEN
              DO 580 J8=1,PIM%STA(J1)%TSYS(PIM%CONF%FRQ_GRP)%NPOI
                NO = NO + 1
                CALL CLRCH ( OUT(NO) ) 
                TIM_STR = MJDSEC_TO_DATE ( PIM%MJD_0, PIM%TAI_0 + &
     &                                      PIM%STA(J1)%TSYS(PIM%CONF%FRQ_GRP)%TIME_MID_R8(J8), IER )
                 WRITE ( UNIT=OUT(NO), FMT=110 ) PIM%STA(J1)%IVS_NAME, TIM_STR(1:19), &
     &                                           PIM%STA(J1)%TSYS(PIM%CONF%FRQ_GRP)%ELEV_R4(J8)/DEG__TO__RAD, &
     &                                           PIM%STA(J1)%TSYS(PIM%CONF%FRQ_GRP)%AZ_R4(J8)/DEG__TO__RAD, &
     &                                           PIM%C_SOU(PIM%STA(J1)%TSYS(PIM%CONF%FRQ_GRP)%SOU_IND(J8))
 110             FORMAT ( 'Tsys  Sta: ', A, ' Date: ', A, ' El: ', F5.2, ' Az: ', F6.2, ' Sou: ', A )
                 FL_TSYS = .FALSE.
                 DO 590 J9=1,PIM%NFRQ
                    IF  ( PIM%STA(J1)%TSYS(PIM%CONF%FRQ_GRP)%TSYS(J9,J8,IND_POL) > TSYS_LIM_MIN .AND. &
     &                    PIM%STA(J1)%TSYS(PIM%CONF%FRQ_GRP)%TSYS(J9,J8,IND_POL) < TSYS_LIM_MAX       ) THEN
!
                          WRITE ( UNIT=FRQ_STR, FMT=120 ) J9, PIM%STA(J1)%TSYS(PIM%CONF%FRQ_GRP)%TSYS(J9,J8,IND_POL)
                          FL_TSYS = .TRUE.
                       ELSE
!
! ----------------------- Tsys is out of range. Flag it out
!
                         WRITE ( UNIT=FRQ_STR, FMT=120 ) J9, -1.0D0
!!                          FRQ_STR(16:21) = '  -1.0'
                    END IF
                    IB = 82 + (J9-1)*LEN(FRQ_STR)
                    IE = IB + LEN(FRQ_STR) - 1
                    OUT(NO)(IB:IE) = FRQ_STR
                    OUT(NO)(IB:IE) = FRQ_STR
 590             CONTINUE 
                 IF ( .NOT. FL_TSYS ) THEN
                      NO = NO - 1
                 END IF
 580          CONTINUE 
           ELSE IF ( TSPL_DATA == PIMA__TABL .AND. &
     &        PIM%STA(J1)%STMO(PIM%CONF%FRQ_GRP)%N_OPA > 0 ) THEN
!
! ----------- Table mode
!
! ----------- NB: STMO has values for the beginning and the end of the san
!
              DO 480 J8=1,PIM%STA(J1)%STMO(PIM%CONF%FRQ_GRP)%N_TAT-1,2
                 IF ( PIM%STA(J1)%STMO(PIM%CONF%FRQ_GRP)%EL(J8) < 0.0 ) GOTO 480
                 NO = NO + 1
                 IND_SCA = PIM%STA(J1)%STMO(PIM%CONF%FRQ_GRP)%IND_SCA(J8)
                 IND_SOU = PIM%SCA(IND_SCA)%SOU_IND
                 TIM_STMO = (PIM%STA(J1)%STMO(PIM%CONF%FRQ_GRP)%TIM(J8) + &
     &                       PIM%STA(J1)%STMO(PIM%CONF%FRQ_GRP)%TIM(J8+1))/2.0D0
                 TIM_STR = MJDSEC_TO_DATE ( PIM%MJD_0, PIM%TAI_0 + TIM_STMO, IER )
!
                 IF ( IND_SOU > 0 ) THEN
                      SOU_NAME = PIM%SOU(IND_SOU)%IVS_NAME
                    ELSE
                      SOU_NAME = '????????'
                 END IF
                 CALL CLRCH  ( OUT(NO) )
                 WRITE ( UNIT=OUT(NO), FMT=110 ) PIM%STA(J1)%IVS_NAME, TIM_STR(1:19), &
     &                                           PIM%STA(J1)%STMO(PIM%CONF%FRQ_GRP)%EL(J8)/DEG__TO__RAD, &
     &                                           PIM%STA(J1)%STMO(PIM%CONF%FRQ_GRP)%AZ(J8)/DEG__TO__RAD, &
     &                                           SOU_NAME
                 IF ( PIM%STA(J1)%STMO(PIM%CONF%FRQ_GRP)%EL(J8)/DEG__TO__RAD < EL_LIM_MIN ) THEN
                      OUT(NO)(45:49) = '-1.00'
                 END IF
                 FL_TSYS = .FALSE.
                 FL_TATM = .FALSE.
                 TIM_DIF_MIN = PIMA__SCM*PIM%CONF%MAX_SCAN_LEN
                 IND_TSYS = 0
                 DO 490 J9=1,PIM%STA(J1)%TSYS(PIM%CONF%FRQ_GRP)%NPOI
                    IF ( PIM%STA(J1)%TSYS(PIM%CONF%FRQ_GRP)%SOU_IND(J9) == IND_SOU ) THEN
                         IF ( DABS(PIM%STA(J1)%TSYS(PIM%CONF%FRQ_GRP)%TIME_MID_R8(J9) - TIM_STMO) < &
     &                        TIM_DIF_MIN ) THEN
!
                              TIM_DIF_MIN = DABS(PIM%STA(J1)%TSYS(PIM%CONF%FRQ_GRP)%TIME_MID_R8(J9) - TIM_STMO)
                              IND_TSYS = J9
                         END IF
                    END IF
 490             CONTINUE 
!
                 DO 4100 J10=1,PIM%NFRQ
                    IF ( IND_TSYS > 0 .AND. PIM%CONF%TSYS_CAL_CODE == PIMA__TSYS_MEASURED ) THEN
                         IF  ( PIM%STA(J1)%TSYS(PIM%CONF%FRQ_GRP)%TSYS(J10,IND_TSYS,IND_POL) > TSYS_LIM_MIN .AND. &
     &                         PIM%STA(J1)%TSYS(PIM%CONF%FRQ_GRP)%TSYS(J10,IND_TSYS,IND_POL) < TSYS_LIM_MAX       ) THEN
!
                               WRITE ( UNIT=FRQ_STR, FMT=120 ) J10, PIM%STA(J1)%TSYS(PIM%CONF%FRQ_GRP)%TSYS(J10,IND_TSYS,IND_POL)
                               FL_TSYS = .TRUE.
                            ELSE
!
! ---------------------------- Tsys is out of range. Flag it out
!
                               FRQ_STR(16:21) = '  -1.0'
                         END IF
                      ELSE IF ( PIM%CONF%TSYS_CAL_CODE == PIMA__TSYS_CLEANED ) THEN
                         IF ( PIM%STA(J1)%STMO(PIM%CONF%FRQ_GRP)%TSYS_CLN(J8,J10,IND_POL) > 0.001D0 ) THEN
                              WRITE ( UNIT=FRQ_STR, FMT=120 ) J10, PIM%STA(J1)%STMO(PIM%CONF%FRQ_GRP)%TSYS_CLN(J8,J10,IND_POL)
                              FL_TSYS = .FALSE.
                            ELSE 
                              WRITE ( UNIT=FRQ_STR, FMT=120 ) J10, -1.0D0
                         END IF
                      ELSE IF ( PIM%CONF%TSYS_CAL_CODE == PIMA__TSYS_MODELED ) THEN
                         IF ( PIM%STA(J1)%STMO(PIM%CONF%FRQ_GRP)%TSYS_MOD(J8,J10,IND_POL) > 0.001D0 ) THEN
                              WRITE ( UNIT=FRQ_STR, FMT=120 ) J10, PIM%STA(J1)%STMO(PIM%CONF%FRQ_GRP)%TSYS_MOD(J8,J10,IND_POL)
                            ELSE
                              FL_TSYS = .FALSE.
                              WRITE ( UNIT=FRQ_STR, FMT=120 ) J10, -1.0D0
                         END IF                     
                      ELSE
!
! ---------------------- Did not find corresponding Tsys
!
                         WRITE ( UNIT=FRQ_STR, FMT=120 ) J10, -1.0D0
                    END IF
                    WRITE ( UNIT=TAT_STR, FMT=130 ) J10, PIM%STA(J1)%STMO(PIM%CONF%FRQ_GRP)%TAT(J8,J10)
                    WRITE ( UNIT=OPA_STR, FMT=140 ) J10, PIM%STA(J1)%STMO(PIM%CONF%FRQ_GRP)%OPA(J8,J10)
                    IF ( PIM%STA(J1)%STMO(PIM%CONF%FRQ_GRP)%TAT(J8,J10) > TATM__MIN ) THEN
                         FL_TATM = .TRUE.
                    END IF
 120                FORMAT ( ' Frq: ', I2, ' Tsys: ', F6.1 )
 130                FORMAT ( ' Frq: ', I2, ' Tatm: ', F5.1 )
 140                FORMAT ( ' Frq: ', I2, ' Opa: ',  F6.4 )
                    IB = 82 + (J10-1)*LEN(FRQ_STR)
                    IE = IB + LEN(FRQ_STR) - 1
                    OUT(NO)(IB:IE) = FRQ_STR
!
                    IB = 82 + PIM%NFRQ*LEN(FRQ_STR) + (J10-1)*LEN(TAT_STR)
                    IE = IB + LEN(TAT_STR) - 1
                    OUT(NO)(IB:IE) = TAT_STR
!
                    IB = 82 + PIM%NFRQ*LEN(FRQ_STR) + PIM%NFRQ*LEN(TAT_STR) + (J10-1)*LEN(OPA_STR)
                    IE = IB + LEN(OPA_STR) - 1
                    OUT(NO)(IB:IE) = OPA_STR
 4100            CONTINUE 
                 IF ( .NOT. FL_TSYS .AND. .NOT. FL_TATM ) THEN
!
! ------------------- No Tsys and not Tatm. That means the station did not observe the source.
! ------------------- Remove the record
!
                      NO = NO - 1
                    ELSE
                      IF ( OUT(NO)(1:60)  == OUT(NO-1)(1:60) .AND. &
        &                  OUT(NO)(67:74) == OUT(NO)(67:74)        ) THEN
!
! ------------------------ Deal with the situation when the time tag is the same, but the sources
! ------------------------ are different, because of source splitting. One of the sources may
! ------------------------ not have Tsys because the originial Tsys measurement was related to
! ------------------------ the main source
!
                           IL = ILEN(OUT(NO))
                           READ ( OUT(NO)(90:95),   FMT='(F6.1)' ) TSYS_NO
                           READ ( OUT(NO-1)(90:95), FMT='(F6.1)' ) TSYS_NOM
                           IF (         TSYS_NO > TSYS_LIM_MIN  .AND.  TSYS_NOM < TSYS_LIM_MIN ) THEN
                                 OUT(NO-1)(76:IL) = OUT(NO)(76:IL) 
                              ELSE IF ( TSYS_NO < TSYS_LIM_MIN  .AND.  TSYS_NOM > TSYS_LIM_MIN ) THEN
                                 OUT(NO)(76:IL) = OUT(NO-1)(76:IL) 
                           END IF
                     END IF
                 END IF
 480          CONTINUE 
              GOTO 410
            ELSE IF ( TSPL_DATA == PIMA__TABMOD                             .AND. &
     &                ( PIM%CONF%TSYS_CAL_CODE == PIMA__TSYS_CLEANED .OR.         &
     &                  PIM%CONF%TSYS_CAL_CODE == PIMA__TSYS_MODELED      ) .AND. &
     &                  PIM%STA(J1)%STMO(PIM%CONF%FRQ_GRP)%N_OPA > 0              ) THEN
              IFRQ = 0
              DO 4110 J11=PIM%CONF%BEG_FRQ,PIM%CONF%END_FRQ
                 IFRQ = IFRQ + 1
                 NP(J1) = 0
                 DO 5120 J12=1,PIM%STA(J1)%STMO(PIM%CONF%FRQ_GRP)%N_TSYS-1,2
                    IF ( IS_R8_NAN ( PIM%STA(J1)%STMO(PIM%CONF%FRQ_GRP)%TSYS_MOD(J12,J11,IND_POL)  ) ) GOTO 5120
                    IF ( PIM%STA(J1)%STMO(PIM%CONF%FRQ_GRP)%TSYS_MOD(J12,J11,IND_POL) < TSYS_LIM_MIN ) GOTO 5120
                    NP(J1) = NP(J1) + 1
                    TIME(NP(J1)) = PIM%STA(J1)%STMO(PIM%CONF%FRQ_GRP)%TIM(J12)
                    IF ( PIM%CONF%TSYS_CAL_CODE == PIMA__TSYS_CLEANED ) THEN
                         TSYS(NP(J1)) = PIM%STA(J1)%STMO(PIM%CONF%FRQ_GRP)%TSYS_CLN(J12,J11,IND_POL)
                       ELSE IF ( PIM%CONF%TSYS_CAL_CODE == PIMA__TSYS_MODELED ) THEN
                         TSYS(NP(J1)) = PIM%STA(J1)%STMO(PIM%CONF%FRQ_GRP)%TSYS_MOD(J12,J11,IND_POL)
                    END IF                     
                    ELEV_RAD(NP(J1)) = PIM%STA(J1)%STMO(PIM%CONF%FRQ_GRP)%EL(J12)
                    IF ( TSYS(NP(J1)) > TSYS_LIM_MAX ) TSYS(NP(J1)) = 0.0D0
 5120            CONTINUE 
                 TIME_OUT = TIME
                 ELEV_OUT = ELEV_RAD
                 CALL ERR_PASS ( IUER, IER )
                 CALL TSYS_MODEL ( NP(J1),  TIME, ELEV_RAD, TSYS, &
     &                             NM, TIME_MOD,  TSYS_MOD, TSYS_ZEN, TSYS_T0, ELEV_T0, &
     &                             NT, TIME_T,    TSYS_T, &
     &                             NE, ELEV_E,    TSYS_E, &
     &                             NP(J1),   TIME_OUT, ELEV_OUT, TSYS_OUT, &
     &                             IREF_INI, FLAG_INI, TSYS_ZEN_MEAN, TSYS_RMS, &
     &                             TSYS_MIN, TSYS_MAX, TSYS_0, IVRB, IER )
                 IF ( IER .NE. 0 ) THEN
                      WRITE ( 6, * ) 'Station ', J1, ' Name: ', PIM%STA(J1)%IVS_NAME
                      CALL ERR_LOG ( 5716, IUER, 'PIMA_PLOT_TSYS', 'Error in '// &
     &                    'decomposition of system temerature on time dependence '// &
     &                    'of Tsys in zenith direction and elevation dependence' )
                      RETURN
                 END IF
!
                 IF ( NM .GE. 3 ) THEN
                      DO 4120 J12=1,NT
                         TIM_STR = MJDSEC_TO_DATE ( PIM%MJD_0, PIM%TAI_0 + TIME_T(J12), IER )
                         NO = NO + 1
                         CALL CLRCH  ( OUT(NO) )
                         WRITE ( UNIT=OUT(NO), FMT=150 ) PIM%STA(J1)%IVS_NAME, J12, &
     &                                                   IFRQ, 1.D-6*PIM%FREQ_ARR(1,J11,PIM%CONF%FRQ_GRP), &
     &                                                   TIM_STR(1:19), TSYS_T(J12)
  150                    FORMAT ( 'Tsys_zen  Sta: ', A, ' Tim_ind: ', I3, ' Freq_ind: ', I2, &
     &                            ' Freq_sky: ', F10.2, ' MHz  Tim: ', A, &
     &                            ' Tsys: ', F6.1, ' K' )
 4120                 CONTINUE 
!
                      DO 4130 J13=1,NE
                         NO = NO + 1
                         CALL CLRCH  ( OUT(NO) )
                         WRITE ( UNIT=OUT(NO), FMT=160 ) PIM%STA(J1)%IVS_NAME, J13, &
     &                                                   IFRQ, 1.D-6*PIM%FREQ_ARR(1,J11,PIM%CONF%FRQ_GRP), &
     &                                                   ELEV_E(J13)/DEG__TO__RAD, TSYS_E(J13)
  160                    FORMAT ( 'Tsys_elv  Sta: ', A, ' Elv_ind: ', I3, ' Freq_ind: ', I2, ' Freq_sky: ', &
     &                            F10.2, ' MHz  Elev: ', F6.2, 12X, ' Tsys: ', F6.1, ' K' )
 4130                 CONTINUE 
                 END IF
 4110         CONTINUE 
              GOTO 410
         END IF
         IF ( MODE == 3  .OR.  MODE == 4 ) THEN
              CALL ERR_PASS ( IUER, IER )
              ELEV_RAD = ELEV*DEG__TO__RAD
              TIME_OUT = TIME
              ELEV_OUT = ELEV_RAD
              CALL TSYS_MODEL ( NP(J1),  TIME, ELEV_RAD, TSYS, &
     &                          NM, TIME_MOD,  TSYS_MOD, TSYS_ZEN, TSYS_T0, ELEV_T0, &
     &                          NT, TIME_T,    TSYS_T, &
     &                          NE, ELEV_E,    TSYS_E, &
     &                          NP(J1),   TIME_OUT,  ELEV_OUT, TSYS_OUT, &
     &                          IREF_INI, FLAG_INI, TSYS_ZEN_MEAN, TSYS_RMS, &
     &                          TSYS_MIN, TSYS_MAX, TSYS_0, IVRB, IER )
              IF ( IER .NE. 0 ) THEN
                   WRITE ( 6, * ) 'Station ',J1,' Name: ', PIM%STA(J1)%IVS_NAME
                   CALL ERR_LOG ( 5717, IUER, 'PIMA_PLOT_TSYS', 'Error in '// &
     &                 'decomposition of system temerature on time dependence '// &
     &                 'of Tsys in zenith direction and elevation dependence' )
                   RETURN
              END IF
              ELEV_E  = ELEV_E/DEG__TO__RAD
              ELEV_T0 = ELEV_T0/DEG__TO__RAD
              IF ( MODE == 3 ) THEN
                   NP(J1) = NT
                   DO 4140 J14=1,NP(J1)
                      T8(J14,J1) = TIME_T(J14)
                      X8(J14,J1) = TSYS_T(J14)
 4140              CONTINUE
                   NZ(J1) = NM
                   DO 4150 J15=1,NZ(J1)
                      T9(J15,J1) = TIME_MOD(J15)
                      X9(J15,J1) = TSYS_ZEN(J15)
 4150              CONTINUE
                ELSE IF ( MODE == 4 ) THEN
                   NP(J1) = NE
                   DO 4160 J16=1,NP(J1)
                      T8(J16,J1) = ELEV_E(J16)
                      X8(J16,J1) = TSYS_E(J16)
 4160              CONTINUE
                   NZ(J1) = NM
                   DO 4170 J17=1,NZ(J1)
                      T9(J17,J1) = ELEV_T0(J17)
                      X9(J17,J1) = TSYS_T0(J17)
 4170              CONTINUE
              END IF
         END IF
!
         DIA(J1)%IDEV = IDEV
         DIA(J1)%NCLR = 1
         DIA(J1)%ICLR = 1
         DIA(J1)%NPOI(1)   = NP(J1)
         DIA(J1)%ADR_X8(1) = LOC(T8(1,J1))
         DIA(J1)%ADR_Y8(1) = LOC(X8(1,J1))
         DIA(J1)%ADR_E8(1) = 0
         DIA(J1)%LER(1)    = .FALSE.
         DIA(J1)%ICOL(1)   = ICL1
         DIA(J1)%IBST(1)   = 0
         IF ( MODE == 3 ) THEN
              DIA(J1)%ILST(1)   = 2
              DIA(J1)%IPST(1)   = 5
            ELSE IF ( MODE == 4 ) THEN
              DIA(J1)%ILST(1)   = 2
              DIA(J1)%IPST(1)   = 5
            ELSE
              DIA(J1)%ILST(1)   = 1
              DIA(J1)%IPST(1)   = 4
         END IF
         DIA(J1)%IOST(1)   = 1
         DIA(J1)%IWST(1)   = 1
!
         IF ( MODE == 3 .OR. MODE == 4 ) THEN
              DIA(J1)%NCLR = 2
              DIA(J1)%NPOI(2)   = NZ(J1)
              DIA(J1)%ADR_X8(2) = LOC(T9(1,J1))
              DIA(J1)%ADR_Y8(2) = LOC(X9(1,J1))
              DIA(J1)%ADR_E8(2) = 0
              DIA(J1)%LER(2)    = .FALSE.
              DIA(J1)%ICOL(2)   = ICL2
              DIA(J1)%IBST(2)   = 0
              DIA(J1)%ILST(2)   = 1
              DIA(J1)%IPST(2)   = 4
              DIA(J1)%IOST(2)   = 1
              DIA(J1)%IWST(2)   = 1
         END IF
         IF ( MODE == 1  .OR.  MODE == 3  .OR.  MODE == 5 ) THEN
              DIA(J1)%XMIN  =  1.0
              DIA(J1)%XMAX  = -1.0
              DIA(J1)%ARG_UNITS = 'Time in seconds'
           ELSE IF ( MODE == 2  .OR.  MODE == 4 ) THEN
              DIA(J1)%XMIN  =  0.0
              DIA(J1)%XMAX  = 90.0
              DIA(J1)%ARG_UNITS = 'Elevation in deg'
         END IF
         DIA(J1)%YMIN   =  1.0
         DIA(J1)%YMAX   = -1.0
         IF ( TSPL_DATA == PIMA__MEASURED ) THEN
              VALUE_STR = 'System temperature'
            ELSE IF ( TSPL_DATA == PIMA__NWM ) THEN
              VALUE_STR = 'Sky temperature'
            ELSE IF ( TSPL_DATA == PIMA__OPACITY ) THEN
              VALUE_STR = 'Atmosphere opacity'
            ELSE IF ( TSPL_DATA == PIMA__TREC ) THEN
              VALUE_STR = 'Receiver temperaure'
         END IF
!
         CALL CLRCH ( STR )
         WRITE ( UNIT=STR(1:9), FMT='(F9.2)' ) PIM%FREQ_ARR(1,IND_FRQ,PIM%CONF%FRQ_GRP)*1.D-6
         CALL CHASHL ( STR(1:9) )
         DIA(J1)%ZAG = VALUE_STR(1:I_LEN(VALUE_STR))//' at '// &
     &                 PIM%STA(J1)%IVS_NAME//' at '//STR(1:I_LEN(STR))//' MHz '// &
     &                 ' in '//PIM%CONF%SESS_CODE
         IF ( MODE == 3  .OR.  MODE == 4 ) THEN
              CALL CLRCH ( STR )
              WRITE ( UNIT=STR(1:6), FMT='(F6.1)' ) TSYS_0
              CALL CHASHR ( STR(1:6) )
              DIA(J1)%ZAG = DIA(J1)%ZAG(1:I_LEN(DIA(J1)%ZAG))//' Tsys_av='// &
     &                      STR(1:6)//' K'
           ELSE IF ( MODE == 5  ) THEN
              DIA(J1)%ZAG = 'Elevation at '//PIM%STA(J1)%IVS_NAME//' in deg'
         END IF
         DIA(J1)%NAME   = PIM%STA(J1)%IVS_NAME
         DIA(J1)%ITRM   = 0
         DIA(J1)%IBATCH = 0
         DIA(J1)%STATUS = DIA__DEF
         IF ( MODE == 5 ) THEN
             TITS(J1) = 'Elev '//PIM%STA(J1)%IVS_NAME
           ELSE
             TITS(J1) = 'Tsys '//PIM%STA(J1)%IVS_NAME
         END IF
 410  CONTINUE
!
      IF ( TSPL_DATA == PIMA__TABL .OR. TSPL_DATA == PIMA__TABMOD ) THEN
           CALL ERR_PASS ( IUER, IER )
           CALL WR_TEXT ( NO, OUT, FILOUT, IER )
           DEALLOCATE ( OUT )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 5718, IUER, 'PIMA_PLOT_TSYS', 'Error in an attempt '// &
     &              'to write into the output file '//FILOUT )
                RETURN
           END IF
           IF ( PIM%CONF%DEBUG_LEVEL .GE. 1 ) THEN
                WRITE ( 6, '(A)' ) 'Written into the output file '//FILOUT(1:I_LEN(FILOUT))
           END IF
           CALL ERR_LOG ( 0, IUER )
           RETURN
      END IF
      IF ( PIM%NSTA < 3 ) THEN
           NC = 1
           NR = PIM%NSTA
         ELSE IF ( PIM%NSTA .LE. 4 ) THEN
           NC = 2
           NR = 2
         ELSE IF ( PIM%NSTA .LE. 9 ) THEN
           NC = 3
           NR = 3
         ELSE IF ( PIM%NSTA .LE. 16 ) THEN
           NC = 4
           NR = 4
         ELSE
           NC = 5
           NR = 5
      END IF
!
      IF ( MODE == 1 ) THEN
           COMMON_TIT = VALUE_STR(1:I_LEN(VALUE_STR))//' versus time during '// &
                        PIM%CONF%SESS_CODE
           BUTTON_LET(1)  = 'Ee'
           BUTTON_NAME(1) = 'Versus elevation|plain'
           BUTTON_LET(2)  = 'Zz'
           BUTTON_NAME(2) = 'Tsys in zenith|versus time'
           BUTTON_LET(3)  = 'Dd'
           BUTTON_NAME(3) = 'Elevation |dependence'
           BUTTON_LET(4)  = 'Ll'
           BUTTON_NAME(4) = 'Elevation|versus time'
           BUTTON_LET(5)  = 'Vv'
           BUTTON_NAME(5) = 'Change|frequency'
           BUTTON_LET(6)  = 'Qq'
           BUTTON_NAME(6) = 'Quit'
         ELSE IF ( MODE == 2 ) THEN
           COMMON_TIT = VALUE_STR(1:I_LEN(VALUE_STR))//' versus elevation during '// &
                         PIM%CONF%SESS_CODE
           BUTTON_LET(1)  = 'Tt'
           BUTTON_NAME(1) = 'Versus time'
           BUTTON_LET(2)  = 'Zz'
           BUTTON_NAME(2) = 'Tsys in zenith|versus time'
           BUTTON_LET(3)  = 'Dd'
           BUTTON_NAME(3) = 'Elevation |dependence'
           BUTTON_LET(4)  = 'Ll'
           BUTTON_NAME(4) = 'Elevation|versus time'
           BUTTON_LET(5)  = 'Vv'
           BUTTON_NAME(5) = 'Change|frequency'
           BUTTON_NAME(6) = 'Quit'
           BUTTON_LET(6)  = 'Qq'
         ELSE IF ( MODE == 3 ) THEN
           COMMON_TIT = 'Zenith T_sys versus time during '// &
                         PIM%CONF%SESS_CODE
           BUTTON_LET(1)  = 'Tt'
           BUTTON_NAME(1) = 'Versus time'
           BUTTON_LET(2)  = 'Ee'
           BUTTON_NAME(2) = 'Versus elevation|plain'
           BUTTON_LET(3)  = 'Dd'
           BUTTON_NAME(3) = 'Elevation |dependence'
           BUTTON_LET(4)  = 'Ll'
           BUTTON_NAME(4) = 'Elevation|versus time'
           BUTTON_LET(5)  = 'Vv'
           BUTTON_NAME(5) = 'Change|frequency'
           BUTTON_NAME(6) = 'Quit'
           BUTTON_LET(6)  = 'Qq'
         ELSE IF ( MODE == 4 ) THEN
           COMMON_TIT = 'Elevation dependence of T_sys during '// &
                         PIM%CONF%SESS_CODE
           BUTTON_LET(1)  = 'Tt'
           BUTTON_NAME(1) = 'Versus time'
           BUTTON_LET(2)  = 'Ee'
           BUTTON_NAME(2) = 'Versus elevation|plain'
           BUTTON_LET(3)  = 'Zz'
           BUTTON_NAME(3) = 'Tsys in zenith|versus time'
           BUTTON_LET(4)  = 'Ll'
           BUTTON_NAME(4) = 'Elevation|versus time'
           BUTTON_LET(5)  = 'Vv'
           BUTTON_NAME(5) = 'Change|frequency'
           BUTTON_NAME(6) = 'Quit'
           BUTTON_LET(6)  = 'Qq'
         ELSE IF ( MODE == 5 ) THEN
           COMMON_TIT = 'Frequency selection'
           BUTTON_LET(1)  = 'Tt'
           BUTTON_NAME(1) = 'Versus time'
           BUTTON_LET(2)  = 'Ee'
           BUTTON_NAME(2) = 'Versus elevation|plain'
           BUTTON_LET(3)  = 'Zz'
           BUTTON_NAME(3) = 'Tsys in zenith|versus time'
           BUTTON_LET(4)  = 'Dd'
           BUTTON_LET(5)  = 'Vv'
           BUTTON_NAME(5) = 'Elevation|versus time'
           BUTTON_NAME(6) = 'Quit'
           BUTTON_LET(6)  = 'Qq'
         ELSE IF ( MODE == 6 ) THEN
           COMMON_TIT = 'Elevation versus time for '// &
                         PIM%CONF%SESS_CODE
      END IF
!
      ICODE = 0
      CALL ERR_PASS ( IUER, IER )
      CALL MULTI_DIAGI ( COMMON_TIT, PIM%NSTA, NC, NR, TITS, MPB, &
     &                   BUTTON_NAME, BUTTON_LET, PREF_NAME, DIA, &
     &                   ICODE, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 5718, IUER, 'PIMA_PLOT_TSYS', 'Failure to make '// &
     &         'a plot of system temperature' )
           RETURN
      END IF
      IF ( MODE == 1 ) THEN
           IF ( ICODE == 0 ) THEN
                GOTO 810
              ELSE IF ( ICODE == 1 ) THEN
                MODE = 2
              ELSE IF ( ICODE == 2 ) THEN
                MODE = 3
              ELSE IF ( ICODE == 3 ) THEN
                MODE = 4
              ELSE IF ( ICODE == 4 ) THEN
                MODE = 5
              ELSE IF ( ICODE == 6 ) THEN
                GOTO 810
           END IF
        ELSE IF ( MODE == 2 ) THEN
           IF ( ICODE == 0 ) THEN
                GOTO 810
              ELSE IF ( ICODE == 1 ) THEN
                MODE = 1
              ELSE IF ( ICODE == 2 ) THEN
                MODE = 3
              ELSE IF ( ICODE == 3 ) THEN
                MODE = 4
              ELSE IF ( ICODE == 4 ) THEN
                MODE = 5
              ELSE IF ( ICODE == 6 ) THEN
                GOTO 810
           END IF
        ELSE IF ( MODE == 3 ) THEN
           IF ( ICODE == 0 ) THEN
                GOTO 810
              ELSE IF ( ICODE == 1 ) THEN
                MODE = 1
              ELSE IF ( ICODE == 2 ) THEN
                MODE = 2
              ELSE IF ( ICODE == 3 ) THEN
                MODE = 4
              ELSE IF ( ICODE == 4 ) THEN
                MODE = 5
              ELSE IF ( ICODE == 6 ) THEN
                GOTO 810
           END IF
        ELSE IF ( MODE == 4 ) THEN
           IF ( ICODE == 0 ) THEN
                GOTO 810
              ELSE IF ( ICODE == 1 ) THEN
                MODE = 1
              ELSE IF ( ICODE == 2 ) THEN
                MODE = 2
              ELSE IF ( ICODE == 3 ) THEN
                MODE = 3
              ELSE IF ( ICODE == 4 ) THEN
                MODE = 5
              ELSE IF ( ICODE == 6 ) THEN
                GOTO 810
           END IF
        ELSE IF ( MODE == 5 ) THEN
           IF ( ICODE == 0 ) THEN
                GOTO 810
              ELSE IF ( ICODE == 1 ) THEN
                MODE = 1
              ELSE IF ( ICODE == 2 ) THEN
                MODE = 2
              ELSE IF ( ICODE == 3 ) THEN
                MODE = 3
              ELSE IF ( ICODE == 4 ) THEN
                MODE = 4
              ELSE IF ( ICODE == 6 ) THEN
                GOTO 810
           END IF
      END IF
      IF ( ICODE == 5 ) THEN
 920       CONTINUE 
           WRITE ( 6, 170 ) IND_FRQ 
 170       FORMAT ( ' Current frequency index: ', I3/ &
     &              ' Please select the frequency index >> '$ )
           READ ( UNIT=5, FMT='(I3)', IOSTAT=IER ) IND_FRQ
           IF ( IER .NE. 0 ) GOTO 920
           IF ( IND_FRQ < PIM%CONF%BEG_FRQ ) IND_FRQ = PIM%CONF%BEG_FRQ
           IF ( IND_FRQ > PIM%CONF%END_FRQ ) IND_FRQ = PIM%CONF%END_FRQ
      END IF
      GOTO 910
!
 810  CONTINUE
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  PIMA_PLOT_TSYS  !#!
#endif
