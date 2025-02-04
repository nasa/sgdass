      PROGRAM    DIF_EOP
! ************************************************************************
! *                                                                      *
! *   Program DIF_EOP compares two EOP series in EOB format and computes *
! *   statistics plts the differences etc.                               *
! *                                                                      *
! *  ### 01-JUL-2002     DIF_EOP   v3.4 (c)  L. Petrov  19-JAN-2006 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'astro_constants.i'
      INCLUDE   'solve.i'
      INCLUDE   'getpar.i'
      REAL*8     RSIG, MAX_NUT_SIG, HW_GAUSS, REF_EPOCH
      PARAMETER  ( RSIG = 30000000.0D0 )
!@      PARAMETER  ( MAX_NUT_SIG = 200.0D8  ) 
!@      PARAMETER  ( HW_GAUSS    = -30.0D0/365.25D0 )
!!      PARAMETER  ( MAX_NUT_SIG = 2.D8 ) 
!@      PARAMETER  ( MAX_NUT_SIG = 1000.0 ) ! 1mas
      PARAMETER  ( MAX_NUT_SIG = 2.d-9 ) ! 1mas
      PARAMETER  ( HW_GAUSS    = -30.0D0/365.25D0 )
      PARAMETER  ( REF_EPOCH   = 1997.0 )
!
      CHARACTER  FILEOB1*128, FILEOB2*128, FILCNT(2)*128, FILSES*128, &
     &           HEAD_BUF(M_HEAD_EOB)*80, BUF_CNT(M_SES)*1024, &
     &           BUF_TMP(M_SES)*1024, &
     &           SESS_NAME(M_SES,NPAR__GTP)*10, OUT*32, POL_UNITS*12, &
     &           DB_ECS(M_SES,2)*10, UT1_UNITS*12
      TYPE ( EOP__STRU ) ::  EOP1(M_SES), EOP2(M_SES)
      CHARACTER  STR*128, FINAM*128, GENNAM1*80, GENNAM2*80
      REAL*8     TIM_DIF(M_SES,NPAR__GTP), &
     &           EOP_VAL1(M_SES,NPAR__GTP), EOP_ERR1(M_SES,NPAR__GTP), &
     &           EOP_VAL2(M_SES,NPAR__GTP), EOP_ERR2(M_SES,NPAR__GTP), &
     &           EOP_DIF(M_SES,NPAR__GTP),  ERR_DIF(M_SES,NPAR__GTP), &
     &           W_DIF(M_SES,NPAR__GTP), UNITS(NPAR__GTP)
      REAL*8     ATIM(2), AVAL(2), AERR(2), ARR1_TMP(M_SES), ARR2_TMP(M_SES)
      REAL*8     MEAN_T, DR_VAL, SH_VAL, DR_SIG, SH_SIG, DW
      REAL*8     WRMS, WW, SCL
      REAL*8     MODVAL, MIN_ERR 
      PARAMETER  ( MIN_ERR = 1.D-20 )
      INTEGER*4  MIND
      PARAMETER  ( MIND = 128 )
      LOGICAL*1  LEX
      INTEGER*4  NHEAD, NSES1, NSES2, NP1, NP2, NP, KP(NPAR__GTP), IP, &
     &           IV(M_SES), NZ, IE, NUMARG, NBAD, J1, J2, J3, J4, J5, &
     &           J6, J7, J8, N_ECS(2), N_CNT, LIND, IND(2,MIND), NSA, IUER
      LOGICAL*4  FL_TOTAL, FL_BOTH, FL_PLOT, FL_SI_UNIT
      INTEGER*4, EXTERNAL :: LINDEX, I_LEN, LTM_DIF
!
      NUMARG = IARGC ()
      FL_TOTAL = .FALSE.
      FL_BOTH  = .FALSE.
      FL_PLOT  = .TRUE.
      FL_SI_UNIT = .TRUE.
!
      IF ( NUMARG .GE. 2 ) THEN
           CALL CLRCH  ( FINAM )
           CALL CLRCH  ( GENNAM1  )
           CALL GETARG ( 1, FINAM )
           IE = LINDEX ( FINAM, '.eob' ) -1
           IF ( IE .LE. 0 ) IE = I_LEN(FINAM)
           GENNAM1 = FINAM(LINDEX(FINAM,  '/')+1:IE)
           FILEOB1 = FINAM(1:I_LEN(FINAM))//'.eob'
           FILCNT(1) = FINAM(1:I_LEN(FINAM))//'.cnt'
!
           CALL CLRCH ( FINAM )
           CALL CLRCH  ( GENNAM2  )
           CALL GETARG ( 2, FINAM )
           IE = LINDEX ( FINAM, '.eob' ) -1
           IF ( IE .LE. 0 ) IE = I_LEN(FINAM)
           GENNAM2 = FINAM(LINDEX(FINAM,  '/')+1:IE)
           GENNAM2 = FINAM(LINDEX(FINAM, '/')+1:)
           FILEOB2 = FINAM(1:I_LEN(FINAM))//'.eob'
           FILCNT(2) = FINAM(1:I_LEN(FINAM))//'.cnt'
!
           IF ( NUMARG .GE. 3 ) THEN
                CALL GETARG ( 3, STR )
                IF ( STR(1:2) .EQ. '-t'   ) FL_TOTAL   = .TRUE.
                IF ( STR(1:2) .EQ. '-b'   ) FL_BOTH    = .TRUE.
                IF ( STR(1:4) .EQ. '-nop' ) FL_PLOT    = .FALSE.
                IF ( STR(1:4) .EQ. '-nos' ) FL_SI_UNIT = .FALSE.
           END IF
!
           IF ( NUMARG .GE. 4 ) THEN
                CALL GETARG ( 4, STR )
                IF ( STR(1:2) .EQ. '-t'   ) FL_TOTAL = .TRUE.
                IF ( STR(1:2) .EQ. '-b'   ) FL_BOTH  = .TRUE.
                IF ( STR(1:4) .EQ. '-nop' ) FL_PLOT  = .FALSE.
                IF ( STR(1:4) .EQ. '-nos' ) FL_SI_UNIT = .FALSE.
           END IF
!
           IF ( NUMARG .GE. 5 ) THEN
                CALL GETARG ( 5, STR )
                IF ( STR(1:2) .EQ. '-t'   ) FL_TOTAL = .TRUE.
                IF ( STR(1:2) .EQ. '-b'   ) FL_BOTH  = .TRUE.
                IF ( STR(1:4) .EQ. '-nop' ) FL_PLOT  = .FALSE.
                IF ( STR(1:4) .EQ. '-nos' ) FL_SI_UNIT = .FALSE.
           END IF
        ELSE
           WRITE ( 6, 110 )
 110       FORMAT ( 1X,'Usage: dif_eop <generic_file_name_1> ', &
     &                 '<generic_file_name_2> [-total/-both] '/ &
     &                 '[-noplot] [-nostandard_units]' )
           CALL EXIT ( 1 )
      END IF
!
      IF ( FL_SI_UNIT ) THEN
           UNITS(XPL__GTP)  = 1.0D0
           UNITS(YPL__GTP)  = 1.0D0
           UNITS(U1__GTP)   = 1.0D0
           UNITS(XPR__GTP)  = 1.0D0
           UNITS(YPR__GTP)  = 1.0D0
           UNITS(UTR__GTP)  = 1.0D0
           UNITS(DPSI__GTP) = 1.0D0
           UNITS(DEPS__GTP) = 1.0D0
!
           POL_UNITS = '(rad)'
           UT1_UNITS = '(rad)'
         ELSE 
           UNITS(XPL__GTP)  = RAD__TO__MAS *1.D3
           UNITS(YPL__GTP)  = RAD__TO__MAS *1.D3
           UNITS(U1__GTP)   = RAD__TO__MSEC*1.D3
           UNITS(XPR__GTP)  = RAD__TO__MAS *1.D3*86400.0
           UNITS(YPR__GTP)  = RAD__TO__MAS *1.D3*86400.0
           UNITS(UTR__GTP)  = RAD__TO__MSEC*1.D3*86400.0
           UNITS(DPSI__GTP) = RAD__TO__MAS *1.D3
           UNITS(DEPS__GTP) = RAD__TO__MAS *1.D3
           POL_UNITS = '(muas)'
           UT1_UNITS = '(musec)'
      END IF
      DO 410 J1=1,2
!
! ------ Check: whether input file with solution control file exists
!
         INQUIRE ( FILE=FILCNT(J1), EXIST=LEX )
         IF ( .NOT. LEX ) THEN
              CALL ERR_LOG ( 8207, -1, 'DIF_EOP', 'Control file '// &
        &          TRIM(FILCNT(J1))//' was not found' )
              CALL EXIT ( 1 )
         END IF
!
         IUER = -1
         CALL RD_TEXT ( FILCNT(J1), M_SES, BUF_CNT, N_CNT, IUER )
         IF ( IUER .NE. 0 ) THEN
              IUER = -1
              CALL ERR_LOG ( 8208, -1, 'DIF_EOP', 'Error in reading '// &
     &            'control file '//FILCNT )
              CALL EXIT ( 1 )
         END IF
         N_ECS(J1) = 0
         DO 420 J2=1,N_CNT
            CALL EXWORD ( BUF_CNT(J2), MIND, LIND, IND, CHAR(32)//CHAR(9), IUER )
            IF ( BUF_CNT(J2)(IND(1,1):IND(2,1)) == 'OBS' ) THEN
                 IF ( INDEX ( BUF_CNT(J2), 'EOP_CONST' ) > 0 ) THEN
                      N_ECS(J1) = N_ECS(J1) + 1
                      DB_ECS(N_ECS(J1),J1) = BUF_CNT(J2)(IND(1,2):IND(2,2)) 
                 END IF
            END IF
            IF ( BUF_CNT(J2)(IND(1,1):IND(2,1)) == 'ARCFILE' ) THEN
                 FILSES = BUF_CNT(J2)(IND(1,2):IND(2,2))
                 INQUIRE ( FILE=FILSES, EXIST=LEX )
                 IF ( .NOT. LEX ) THEN
                      CALL ERR_LOG ( -1, 'DIF_EOP', 'Session file '// &
     &                     FILSES(1:I_LEN(FILSES))//' specfied as ARCFILE in '// &
     &                    'the control fil '//TRIM(FILCNT(J1))// &
     &                    ' was not found' )
                      CALL EXIT ( 1 )
                 END IF
!
                 IUER = -1
                 CALL RD_TEXT ( FILSES, M_SES, BUF_TMP, NSA, IUER )
                 IF ( IUER .NE. 0 ) THEN
                      IUER = -1
                      CALL ERR_LOG ( 8208, -1, 'EOP_ALIGNMENT', 'Error in reading '// &
     &                    'session file '//FILSES )
                      CALL EXIT ( 1 )
                 END IF
!
                 DO 430 J3=1,NSA
                    IF ( BUF_TMP(J3)(1:1) == '*' ) GOTO 430
                    IUER = -1
                    CALL EXWORD ( BUF_TMP(J3), MIND, LIND, IND, CHAR(32)//CHAR(9), IUER )
                    IF ( LIND < 2 ) GOTO 420
                    IF ( BUF_TMP(J3)(IND(1,1):IND(2,1)) == 'OBS' ) THEN
                         IF ( INDEX ( BUF_TMP(J3), 'EOP_CONST' ) > 0 ) THEN
                              N_ECS(J1) = N_ECS(J1) + 1
                              DB_ECS(N_ECS(J1),J1) = BUF_TMP(J3)(IND(1,2):IND(2,2)) 
                         END IF
                    END IF
 430             CONTINUE 
            END IF
 420     CONTINUE 
 410  CONTINUE 
!
      IUER = -1
      CALL READ_EOB ( FILEOB1, M_HEAD_EOB, NHEAD, HEAD_BUF, M_SES, NSES1, &
     &                EOP1, IUER)
      IF ( IUER .NE. 0 ) THEN
           STOP 'Error in READ_EOB'
      END IF
!
      IUER = -1
      CALL READ_EOB ( FILEOB2, M_HEAD_EOB, NHEAD, HEAD_BUF, M_SES, NSES2, &
     &                EOP2, IUER)
      IF ( IUER .NE. 0 ) THEN
           STOP 'Error in READ_EOB'
      END IF
!
         write ( 6, * ) ' nses1= ', nses1, ' nses2= ', nses2 ! %%%%%%%%%%%%
!
      NP = 0
      KP = 0
      DO 440 J4=1,NSES1
         IF ( LTM_DIF ( 0, N_ECS(1), DB_ECS(1,1), EOP1(J4)%DBNAME ) > 0 ) GOTO 440
         DO 450 J5=1,NSES2
            IF ( LTM_DIF ( 0, N_ECS(2), DB_ECS(1,2), EOP2(J5)%DBNAME ) > 0 ) GOTO 450
            IF ( EOP1(J4)%DBNAME .EQ. EOP2(J5)%DBNAME ) THEN
                 NP = NP + 1
                 DO 460 J6=1,NPAR__GTP
                    IF ( .NOT. BTEST ( EOP1(J4)%STATUS, INT2(J6) ) ) GOTO 460
                    IF ( .NOT. BTEST ( EOP2(J5)%STATUS, INT2(J6) ) ) GOTO 460
!
                    IF ( EOP1(J4)%DPSI_E*UNITS(DPSI__GTP) .GT. UNITS(DPSI__GTP)*MAX_NUT_SIG/0.4D0 ) GOTO 460
                    IF ( EOP2(J5)%DPSI_E*UNITS(DPSI__GTP) .GT. UNITS(DPSI__GTP)*MAX_NUT_SIG/0.4D0 ) GOTO 460
                    IF ( EOP1(J4)%DEPS_E*UNITS(DEPS__GTP) .GT. UNITS(DEPS__GTP)*MAX_NUT_SIG ) GOTO 460
                    IF ( EOP2(J5)%DEPS_E*UNITS(DEPS__GTP) .GT. UNITS(DEPS__GTP)*MAX_NUT_SIG ) GOTO 460
!
                    KP(J6) = KP(J6) + 1
                    IF ( J6 .EQ. DPSI__GTP  .OR.  J6 .EQ. DEPS__GTP ) THEN
                         TIM_DIF(KP(J6),J6) = 2000.0D0 + &
     &                           (EOP1(J4)%MJD_NUT - 51544.5D0)/YEAR__TO__DAY
                       ELSE
                         TIM_DIF(KP(J6),J6) = 2000.0D0 + &
     &                           (EOP1(J4)%MJD_EOP - 51544.5D0)/YEAR__TO__DAY
                    END IF
!
                    IF ( J6 .EQ. XPL__GTP ) THEN
                         EOP_VAL1(KP(J6),J6) = EOP1(J4)%XPL_V*UNITS(XPL__GTP)
                         EOP_ERR1(KP(J6),J6) = EOP1(J4)%XPL_E*UNITS(XPL__GTP)
                         EOP_VAL2(KP(J6),J6) = EOP2(J5)%XPL_V*UNITS(XPL__GTP)
                         EOP_ERR2(KP(J6),J6) = EOP2(J5)%XPL_E*UNITS(XPL__GTP)
                       ELSE IF ( J6 .EQ. YPL__GTP ) THEN
                         EOP_VAL1(KP(J6),J6) = EOP1(J4)%YPL_V*UNITS(YPL__GTP)
                         EOP_ERR1(KP(J6),J6) = EOP1(J4)%YPL_E*UNITS(YPL__GTP)
                         EOP_VAL2(KP(J6),J6) = EOP2(J5)%YPL_V*UNITS(YPL__GTP)
                         EOP_ERR2(KP(J6),J6) = EOP2(J5)%YPL_E*UNITS(YPL__GTP)
                       ELSE IF ( J6 .EQ. U1__GTP ) THEN
                         EOP_VAL1(KP(J6),J6) = EOP1(J4)%U1_V*UNITS(U1__GTP)
                         EOP_ERR1(KP(J6),J6) = EOP1(J4)%U1_E*UNITS(U1__GTP)
                         EOP_VAL2(KP(J6),J6) = EOP2(J5)%U1_V*UNITS(U1__GTP)
                         EOP_ERR2(KP(J6),J6) = EOP2(J5)%U1_E*UNITS(U1__GTP)
                       ELSE IF ( J6 .EQ. XPR__GTP ) THEN
                         EOP_VAL1(KP(J6),J6) = EOP1(J4)%XPR_V*UNITS(XPR__GTP)
                         EOP_ERR1(KP(J6),J6) = EOP1(J4)%XPR_E*UNITS(XPR__GTP)
                         EOP_VAL2(KP(J6),J6) = EOP2(J5)%XPR_V*UNITS(XPR__GTP)
                         EOP_ERR2(KP(J6),J6) = EOP2(J5)%XPR_E*UNITS(XPR__GTP)
                       ELSE IF ( J6 .EQ. YPR__GTP ) THEN
                         EOP_VAL1(KP(J6),J6) = EOP1(J4)%YPR_V*UNITS(YPR__GTP)
                         EOP_ERR1(KP(J6),J6) = EOP1(J4)%YPR_E*UNITS(YPR__GTP)
                         EOP_VAL2(KP(J6),J6) = EOP2(J5)%YPR_V*UNITS(YPR__GTP)
                         EOP_ERR2(KP(J6),J6) = EOP2(J5)%YPR_E*UNITS(YPR__GTP)
                       ELSE IF ( J6 .EQ. UTR__GTP ) THEN
                         EOP_VAL1(KP(J6),J6) = EOP1(J4)%UTR_V*UNITS(UTR__GTP)
                         EOP_ERR1(KP(J6),J6) = EOP1(J4)%UTR_E*UNITS(UTR__GTP)
                         EOP_VAL2(KP(J6),J6) = EOP2(J5)%UTR_V*UNITS(UTR__GTP)
                         EOP_ERR2(KP(J6),J6) = EOP2(J5)%UTR_E*UNITS(UTR__GTP)
                       ELSE IF ( J6 .EQ. DPSI__GTP) THEN
                         EOP_VAL1(KP(J6),J6) = EOP1(J4)%DPSI_V*UNITS(DPSI__GTP)
                         EOP_ERR1(KP(J6),J6) = EOP1(J4)%DPSI_E*UNITS(DPSI__GTP)
                         EOP_VAL2(KP(J6),J6) = EOP2(J5)%DPSI_V*UNITS(DPSI__GTP)
                         EOP_ERR2(KP(J6),J6) = EOP2(J5)%DPSI_E*UNITS(DPSI__GTP)
                       ELSE IF ( J6 .EQ. DEPS__GTP) THEN
                         EOP_VAL1(KP(J6),J6) = EOP1(J4)%DEPS_V*UNITS(DEPS__GTP)
                         EOP_ERR1(KP(J6),J6) = EOP1(J4)%DEPS_E*UNITS(DEPS__GTP)
                         EOP_VAL2(KP(J6),J6) = EOP2(J5)%DEPS_V*UNITS(DEPS__GTP)
                         EOP_ERR2(KP(J6),J6) = EOP2(J5)%DEPS_E*UNITS(DEPS__GTP)
                    END IF
!
                    IF ( .NOT. FL_TOTAL ) THEN
                         EOP_DIF(KP(J6),J6) = EOP_VAL1(KP(J6),J6) - &
     &                                        EOP_VAL2(KP(J6),J6)
                       ELSE
                         EOP_DIF(KP(J6),J6) = 0.0D0
                    END IF
                    ERR_DIF(KP(J6),J6)   = EOP_ERR1(KP(J6),J6)
                    IF ( EOP_ERR1(KP(J6),J6) .GT. MIN_ERR ) THEN
                         W_DIF(KP(J6),J6) = 1.D0/EOP_ERR1(KP(J6),J6)
                       ELSE 
                         W_DIF(KP(J6),J6) = 1.D0
                    END IF
                    SESS_NAME(KP(J6),J6) = EOP1(J4)%DBNAME
                    IV(KP(J6)) = 1
 460             CONTINUE
!
                 GOTO 840
            END IF
 450    CONTINUE
 840    CONTINUE
 440  CONTINUE
!
      WRITE ( 6, '(A,F7.2)' ) '                               '// &
     &                      GENNAM1(1:I_LEN(GENNAM1))//' minus '// &
     &                      GENNAM2(1:I_LEN(GENNAM2))// &
     &                      '  Reference epoch: ', REF_EPOCH
      WRITE ( 6, '(A)' ) '|------------------------------|-------'// &
     &                   '-----------|------------------|--------|'
      WRITE ( 6, '(A)' ) '|                              |      '// &
     &                   'shift       | drift (per year) |  wrms  |'
      WRITE ( 6, '(A)' ) '|------------------------------|-------'// &
     &                   '-----------|------------------|--------|'
!
      DO 470 J7=1,NPAR__GTP
         IF ( KP(J7) .LT. 2 ) GOTO 470
         CALL CLRCH ( STR )
         IF ( J4 .EQ. XPL__GTP ) THEN
              IF ( FL_TOTAL ) THEN
                   STR = 'X pole coordinates '//POL_UNITS
                 ELSE
                   STR = 'Differences in X pole '// &
     &                    POL_UNITS(1:I_LEN(POL_UNITS))//' '// &
     &                    GENNAM1(1:I_LEN(GENNAM1))//' minus '// &
     &                    GENNAM2(1:I_LEN(GENNAM2))
              END IF
           ELSE IF ( J7 .EQ. YPL__GTP ) THEN
              IF ( FL_TOTAL ) THEN
                   STR = 'Y pole coordinates '//POL_UNITS
                 ELSE
                   STR = 'Differences in Y pole '// &
     &                    POL_UNITS(1:I_LEN(POL_UNITS))//' '// &
     &                    GENNAM1(1:I_LEN(GENNAM1))//' minus '// &
     &                    GENNAM2(1:I_LEN(GENNAM2))
              END IF
           ELSE IF ( J7 .EQ. U1__GTP ) THEN
              IF ( FL_TOTAL ) THEN
                   STR = 'UT1 angles '//UT1_UNITS
                 ELSE
                   STR = 'Differences in UT1 '//UT1_UNITS(1:I_LEN(UT1_UNITS))// &
     &                   ' '// &
     &                   GENNAM1(1:I_LEN(GENNAM1))//' minus '// &
     &                   GENNAM2(1:I_LEN(GENNAM2))
              END IF
           ELSE IF ( J7 .EQ. XPR__GTP ) THEN
              IF ( FL_TOTAL ) THEN
                   STR = 'X pole coordinate rate (muas/day)'
                 ELSE
                   STR = 'Differences in X pole rate (muas/day)  '// &
     &                    GENNAM1(1:I_LEN(GENNAM1))//' minus '// &
     &                    GENNAM2(1:I_LEN(GENNAM2))
              END IF
           ELSE IF ( J7 .EQ. YPR__GTP ) THEN
              IF ( FL_TOTAL ) THEN
                   STR = 'Y pole coordinate rate (muas/day)'
                 ELSE
                   STR = 'Differences in Y pole rate (muas/day)  '// &
     &                    GENNAM1(1:I_LEN(GENNAM1))//' minus '// &
     &                    GENNAM2(1:I_LEN(GENNAM2))
              END IF
           ELSE IF ( J7 .EQ. UTR__GTP ) THEN
              IF ( FL_TOTAL ) THEN
                   STR = 'UT1 rate (musec/day)'
                 ELSE
                   STR = 'Differences in UT1 rate (musec/day)  '// &
     &                    GENNAM1(1:I_LEN(GENNAM1))//' minus '// &
     &                    GENNAM2(1:I_LEN(GENNAM2))
              END IF
           ELSE IF ( J7 .EQ. DPSI__GTP ) THEN
              IF ( FL_TOTAL ) THEN
                   STR = 'Nutation in longitude angles '//POL_UNITS
                 ELSE
                   STR = 'Differences in nutation PSI '// &
     &                    POL_UNITS(1:I_LEN(POL_UNITS))//' '// &
     &                    GENNAM1(1:I_LEN(GENNAM1))//' minus '// &
     &                    GENNAM2(1:I_LEN(GENNAM2))
              END IF
           ELSE IF ( J7 .EQ. DEPS__GTP ) THEN
              IF ( FL_TOTAL ) THEN
                   STR = 'Nutation in obliquity angles '//POL_UNITS
                 ELSE
                   STR = 'Differences in nutation EPS '// &
     &                    POL_UNITS(1:I_LEN(POL_UNITS))//' '// &
     &                    GENNAM1(1:I_LEN(GENNAM1))//' minus '// &
     &                    GENNAM2(1:I_LEN(GENNAM2))
              END IF
         END IF
!
         CALL RGRW8  ( KP(J7), TIM_DIF(1,J7), EOP_DIF(1,J7), W_DIF(1,J7), &
     &                 %VAL(0), MEAN_T, DR_VAL, SH_VAL, DR_SIG, SH_SIG, -3 )
!
         CALL DISP_WTR8 ( KP(J7), TIM_DIF(1,J7), EOP_DIF(1,J7), W_DIF(1,J7), &
     &                    DR_VAL, SH_VAL + (TIM_DIF(1,J7)-MEAN_T)*DR_VAL, IV, &
     &                    DW, NZ, -3 )
!
         ATIM(1) = TIM_DIF(1,J7)
         ATIM(2) = TIM_DIF(KP(J7),J7)
         AVAL(1) = SH_VAL + (ATIM(1) -  MEAN_T)*DR_VAL
         AVAL(2) = SH_VAL + (ATIM(2) -  MEAN_T)*DR_VAL
         IF ( FL_SI_UNIT ) THEN
              AERR(1) = 1.D-12
              AERR(2) = 1.D-12
            ELSE
              AERR(1) = 1.D-3
              AERR(2) = 1.D-3
         END IF
!
         NBAD = 0
         WW = 0.0D0
         WRMS = 0.0D0
         DO 480 J8=1,KP(J7)
            MODVAL = SH_VAL + (TIM_DIF(J8,J7) -  MEAN_T)*DR_VAL
            IF ( DABS(ERR_DIF(J8,J7)) .LT. MIN_ERR ) GOTO 480
            IF ( DABS(EOP_DIF(J8,J7) - MODVAL)/ERR_DIF(J8,J7) .GT. RSIG  .AND. &
     &           .NOT. FL_TOTAL ) THEN
!
                 NBAD = NBAD + 1
                 WRITE (  6, 120 ) NBAD, SESS_NAME(J8,J7), &
     &                             DABS(EOP_DIF(J8,J7) - MODVAL)/ERR_DIF(J8,J7), &
     &                             ERR_DIF(J8,J7)
 120             FORMAT ( 1X, I4, 1X, A, ' ', F6.2,' | ',F10.3 )
            END IF
!
            IF ( TIM_DIF(J8,J7) > 1996.0D0 .AND. TIM_DIF(J8,J7) < 2090.0D0 ) THEN
                 WRMS = WRMS + EOP_VAL1(J8,J7)**2/EOP_ERR1(J8,J7)**2
                 WW   = WW + 1.D0/EOP_ERR1(J8,J7)**2
            END IF
 480     CONTINUE
         IF ( WW > 1.D-22 ) THEN
               WRMS = DSQRT ( WRMS/WW )
!@               WRITE ( 6, * ) ' WRMS = ',WRMS 
         END IF
!
         IF ( .NOT. FL_TOTAL ) THEN
              IP = INDEX ( STR, GENNAM1(1:I_LEN(GENNAM1)) )-1
              IF ( IP .LE. 0 ) IP = I_LEN(STR)
              CALL CLRCH ( OUT )
              OUT = 'Dif. '//STR(13:IP)
              CALL CHASHL ( OUT )
              IF ( FL_SI_UNIT ) THEN
                   SCL = 1.D12
                   WRITE ( 6, 130 ) OUT(1:31), &
     &                         (SH_VAL + DR_VAL*(REF_EPOCH-MEAN_T))*SCL, &
     &                          SH_SIG*SCL, DR_VAL*SCL, DR_SIG*SCL, DW*SCL
  130              FORMAT ( A31, '|', 2(F8.0, ' -+ ', F5.0, ' |'), F7.0, ' |' )
                 ELSE
                   SCL = 1.0D0
                   WRITE ( 6, 140 ) OUT(1:31), &
     &                         (SH_VAL + DR_VAL*(REF_EPOCH-MEAN_T))*SCL, &
     &                         SH_SIG*SCL, DR_VAL*SCL, DR_SIG*SCL, DW
  140              FORMAT ( A31, '|', 2(F8.2, ' -+ ', F5.2, ' |'), F7.2, ' |' )
              END IF
         END IF
!
         CALL SORT83   ( KP(J7), TIM_DIF(1,J7), EOP_DIF(1,J7), ERR_DIF(1,J7) )
!
         CALL DIAGI_SETDEF ( -3, 'DIAGI_CTIT', STR )
         CALL DIAGI_SETDEF ( -3, 'DIAGI_UNIT', 'Time in years' )
         CALL DIAGI_SETDEF ( -3, 'DIAGI_ILST', 2   )
         CALL DIAGI_SETDEF ( -3, 'DIAGI_IBST', 4   )
         IF ( HW_GAUSS .GT. 1.D-6 ) THEN
              CALL GAUSS_FILTER ( KP(J7), HW_GAUSS, TIM_DIF(1,J7), &
     &                            EOP_VAL1(1,J7), EOP_ERR1(1,J7), &
     &                            ARR1_TMP, ARR2_TMP )
              CALL COPY_V ( KP(J7), ARR1_TMP, EOP_VAL1(1,J7) )
              CALL COPY_V ( KP(J7), ARR2_TMP, EOP_ERR1(1,J7) )
              CALL DIAGI_SETDEF ( -3, 'DIAGI_ILST', 2 )
         END IF
!
         IF ( FL_PLOT ) THEN
              IF ( FL_TOTAL ) THEN
                   CALL DIAGI_1E ( KP(J7), TIM_DIF(1,J7), EOP_VAL1(1,J7), &
     &                             EOP_ERR1(1,J7), -3 )
                ELSE IF ( FL_BOTH ) THEN
                   CALL DIAGI_2E ( KP(J7), TIM_DIF(1,J7), EOP_VAL1(1,J7), EOP_ERR1(1,J7), &
     &                             KP(J7), TIM_DIF(1,J7), EOP_VAL2(1,J7), EOP_ERR2(1,J7), -3 )
                ELSE
                   CALL DIAGI_2E ( 2, ATIM, AVAL, AERR, KP(J7), TIM_DIF(1,J7), &
     &                             EOP_DIF(1,J7), ERR_DIF(1,J7), -3 )
              END IF
         END IF
!
         IF ( J7 .EQ. NPAR__GTP ) THEN
              WRITE ( 6, '(A)' ) '|------------------------------|-------'// &
     &                           '-----------|------------------|--------|'
         END IF
 470  CONTINUE
!
      END  !#!   DIF_EOP  #!#
!
! ------------------------------------------------------------------------
!
        SUBROUTINE WDESIG8 ( N, T, X, IV, NZ, ED_I, IUER )
! ************************************************************************
! *                                                                      *
! *     נןהנעןחעבםםב  DESIG8  ןפםו‏בופ üלוםומפש, כןפןעשו נעוקשûבאפ       *
! *     תבהבממןו ‏יףלן ףפבמהבעפמשט ןפכלןמומיך ןפמןףיפולרמן עוחעוףףיןממןך *
! *     נעסםןך פ.ו. עבתמןףפי : X(I) - ( A*T(I) + B )                     *
! *            חהו A  --  כןüזזידיומפ מבכלןמב עוחעוףףיןממןך נעסםןך, ב    *
! *                B  --  ףקןגןהמשך ‏לומ עוחעוףףיןממןך נעסםןך, ב         *
! *                                                                      *
! *________________________ קטןהמשו נבעבםופעש: __________________________*
! *                                                                      *
! *       N  ( INTEGER*4 )  --  הלימב םבףףיקןק.                          *
! *       T  ( REAL*8    )  --  םבףףיק בעחץםומפב.                        *
! *       X  ( REAL*8    )  --  םבףףיק תמב‏ומיך.                         *
! *                                                                      *
! *_____________________ םןהיזידיעץוםשו נבעבםופעש: ______________________*
! *                                                                      *
! *      IV  ( INTEGER*4 )  --  דולשך םבףףיק הלימןך N, ןפםו‏בא‎יך        *
! *                             קשגעבףשקבוםשו üלוםומפש. וףלי IV(I)=0,    *
! *                             פן I-פשך üלוםומפ ןפםו‏בופףס כבכ קשגעןף,  *
! *                             וףלי  IV(J)=1, פן J-פשך üלוםומפ          *
! *                             ץ‏בףפקץופ ק הבלרמוךûיט קש‏יףלומיסט.      *
! *                                                                      *
! *_______________________ קשטןהמשו נבעבםופעש: __________________________*
! *                                                                      *
! *      NZ  ( INTEGER*4 )  --  ‏יףלן מוןפגעןûוממשט üלוםומפןק.           *
! *                                                                      *
! * ___________________ קטןהמןך ןנץףכבוםשך נבעבםופע: ___________________ *
! *                                                                      *
! *      ED_I  ( REAL*8  )  --  כןלי‏וףפקן ףפבמהבעפמשט ןפכלןמומיך,       *
! *                             כןפןעןו מבהן נעוקשףיפר, ‏פןגש גשפר       *
! *                             ןפגעןûוממשם .                            *
! *                                                                      *
! * ___________________ םןהיזידיעץוםשו נבעבםופעש: ______________________ *
! *                                                                      *
! *  IUER  ( INTEGER*4, OPT )  -- נבעבםופע ןûיגכי:                       *
! *             קטןהמןו תמב‏ומיו  --  עוצים ןגעבגןפכי ןûיגכי:            *
! *             ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~             *
! *      IUER>-1 -- קןתקעב‎ומיו כןהב ןûיגכי.                             *
! *      IUER=-1 -- קןתקעב‎ומיו כןהב IUER=0 ק ףלץ‏בו מןעםבלרמןחן         *
! *                 תבקועûומיס י קשקןה היבחמןףפי‏וףכןחן ףןןג‎ומיס        *
! *                 ק ףלץ‏בו קןתמיכמןקומיס ןûיגכי.                       *
! *      IUER<-1 -- קןתקעב‎ומיו כןהב IUER=0 ק ףלץ‏בו מןעםבלרמןחן         *
! *                 תבקועûומיס, קשקןה היבחמןףפי‏וףכןחן ףןןג‎ומיס י       *
! *                 תבקועûומיס ןגעבתב ק ףלץ‏בו קןתמיכמןקומיס ןûיגכי.     *
! *      וףלי IUER ןנץ‎ומ, פן קטןהמןו תמב‏ומיו נעימיםבופףס עבקמשם -1     *
! *             קשטןהמןו תמב‏ומיו  --  כןה ןûיגכי ( וףלי IUER            *
! *             ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~                        *
! *             הןףפץנומ הלס תבניףי ):                                   *
! *      IUER=0  --  מןעםבלרמןו תבקועûומיו.                              *
! *                                                                      *
! *      נעיםו‏במיו: וףלי נבעבםופע  ED ןנץ‎ומ, פן ןמ תבנעבûיקבופףס ף     *
! *      פועםימבלב.  וףלי י מב תבנעןף ןפקופיפר <קכ>, פן נן ץםןל‏במיא     *
! *      ED קשגועופףס עבקמשם 3 .                                         *
! *                                                                      *
! *  ###  06-MAY-91     DESIG8    V1.4  (c) נÅÔÒÏ× ל.א.  08-AUG-94  ###  *
! *                                                                      *
! ************************************************************************
        REAL*8  T(N), X(N), AV, D, VALKR, A, B
        INTEGER IV(N)
!
!        NA=NUM$ARG()
!        IF ( NA.LT.5 .AND. NA.GT.7 ) CALL VER$ARG ( 7 )
!
! ----- קש‏יףלומיו כןüזזידיומפןק עוחעוףףיי A י B
!
        CALL REGRW8 ( N, T, X, %VAL(0), IV, A, B, -3 )
!
! ----- קש‏יףלומיו ףעוהמוחן  --  AV  י היףנועףיי  --  ה  עבתמןףפי
! ----- םבףףיק-עוחעוףףיס
!
        CALL ERR_PASS ( IUER, IER )
        CALL DISP_TR8 ( N, T, X, A, B, IV, AV, D, NZ, IER )
        IF ( IER.GT.0 ) THEN
             CALL ERR_LOG ( IER, IUER, 'DESIG8', 'Something in disp_tr8 1' )
             RETURN
        END IF
!
! ----- תבהבמיו  ED  --  ‏יףלן ףפבמהבעפמשט ןפכלןמומיך ןפמןףיפולרמן
! ----- עוחעוףףיןממןך נעסםןך, גןלרûו כןפןעשט ףלוהץוT üלוםומפ ןפםו‏בפר כבכ
! ----- קשגעןף
!
!        IF ( NA.EQ.5 ) THEN
!             TYPE 110
!  110        FORMAT(1X/2X,'???  יףכלא‏יפר קשגעןףש, כןפןעשו נעוקשûבאפ'/
!     *       2X,'ףכןלרכן ףעוהמוכקבהעבפי‏משט ןפכלןמומיך  <3.>  ? '$)
!             ACCEPT 120,IQ,ED
!  120        FORMAT(Q,G15.7)
!             IF( IQ.EQ.0 ) ED=3.   !  נן ץםןל‏במיא  ED=3
!          ELSE
!             ED=ED_I
!             IF ( ED .LT. 1.D-5 ) ED=3.
!        END IF
         ED=ED_I
         IF ( ED .LT. 1.D-5 ) ED=3.0
!
! ##### מב‏בלן יפועבפיקמןחן דיכלב
!
!            type *,' av=',av,' a=',a,' b=',b ! %%%
  610   CONTINUE
        AV = 0.0
        IDEL=0
        VALKR=ED*D
        DO 410 J1=1,N
           IF( IV(J1) .EQ. 0 ) GOTO 410
!
! -------- IDEL  --  ‏יףלן יףכלא‏וממשט üלםומפןק מב הבממןם ûבחו דיכלב
!
           TT=T(J1)-T(1)
           IF( DABS( (X(J1)-AV)-(A*TT+B) ) .GT. VALKR ) IDEL=IDEL+1
           IF( DABS( (X(J1)-AV)-(A*TT+B) ) .GT. VALKR ) IV(J1)=0
  410   CONTINUE
           WRITE ( 6, * ) ' valkr=',valkr,' idel=',idel,' d=',d  ! %%%%%%%
!
        CALL ERR_PASS ( IUER, IER )
        CALL DISP_TR8 ( N, T, X, A, B, IV, AV, D, NZ, IER )
        IF ( IER.GT.0 ) THEN
             CALL ERR_LOG ( IER, IUER, 'DESIG8', 'Something in disp_tr8 1' )
             RETURN
        END IF
!
! ----- ו‎ו עבת קש‏יףלומיו ףעוהמוחן י היףנועףיי
!
        CALL ERR_PASS ( IUER, IER )
        CALL REGRW8 ( N, T, X, %VAL(0), IV, A, B, IER )
        IF ( IER.GT.0 ) THEN
             CALL ERR_LOG ( IER, IUER, 'DESIG8', 'Something in disp_tr8 2' )
             RETURN
        END IF
!
! ----- וףלי מב הבממןם ûבחו יףכלא‏וממשט üלוםומפןק מופ  --  פן קשטןה ית דיכלב
!
        IF ( IDEL .EQ. 0 ) THEN
             CALL ERR_LOG ( 0, IUER )
             RETURN
        END IF
        GOTO 610
!
! ##### כןמוד יפועבפיקמןחן דיכלב
!
        END  !#!  WDESIG8  #!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE GAUSS_FILTER ( M, HW, DAT, ARR1, SIG1, ARR2, SIG2 )
      IMPLICIT   NONE 
      INTEGER*4  M
      REAL*8     HW, DAT(M), ARR1(M), SIG1(M), ARR2(M), SIG2(M)
      REAL*8     WIN, WIN_SUM, EXP_VAR
      INTEGER*4  J1, J2
!
      DO 410 J1=1,M
         WIN_SUM = 0.0D0
         ARR2(J1) = 0.0D0
         DO 420 J2=1,M
            EXP_VAR = -( (DAT(J2) - DAT(J1))/HW )**2
            IF ( EXP_VAR .LT. -40.0D0 ) EXP_VAR = -40.0D0
            WIN_SUM = WIN_SUM + DEXP(EXP_VAR)
            ARR2(J1) = ARR2(J1) + ARR1(J2)*DEXP(EXP_VAR)
 420     CONTINUE 
         ARR2(J1) = ARR2(J1)/WIN_SUM
         SIG2(J1) = SIG1(J1)
 410  CONTINUE 
!      
      RETURN
      END  SUBROUTINE  GAUSS_FILTER 
