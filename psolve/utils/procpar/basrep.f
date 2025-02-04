      PROGRAM    BASREP
! ************************************************************************
! *                                                                      *
! *   Program basrep computs baseline length repeatability.              *
! *                                                                      *
! *  ### 17-APR-1999     BASREP    v3.2 (c)  L. Petrov  22-APR-2023 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'solve.i'
      INTEGER*4  MBUF, MBAS, MSES, M_STA, M_EPC, LBUF, LIM_OBS, IUER
      REAL*8     LIM_LEN
      PARAMETER  ( MBUF = 256*1024, MBAS = 16*1024, MSES = 8192, &
     &             M_STA = 256, M_EPC = 64 )
      CHARACTER  FINAM*128
      CHARACTER  BUF(MBUF)*128, BASNAM(MBAS)*17, STR*32, FILOUT*128, &
     &           FILESM*128, SAVE_DIR*128, CE_STA(M_STA)*8, ST1*8, ST2*8, &
     &           STR_NO_ESM*8
      REAL*8,    ALLOCATABLE :: XDAT(:,:), XVAL(:,:), XSIG(:,:)
!!      REAL*8     XDAT(MSES,MBAS), XVAL(MSES,MBAS), XSIG(MSES,MBAS), &
      REAL*8     EPOCH_ESM(M_STA), TIM_EPC(M_EPC)
      REAL*8     SH_VAL(M_EPC), SH_SIG(M_EPC), DR_VAL, DR_SIG, &
     &           W(MSES), RBAS(MBAS), RLEN(MBAS), REP_0, REP_1, &
     &           FBAS(MBAS), FLEN(MBAS), FCT_0, FCT_1, &
     &           GARG(MBAS), GBAS(MBAS), YEAR_START, YEAR_END, A, B, CHI, &
     &           SH_REP, DR_REP, SQ_REP
      REAL*8     YEAR_MIN, YEAR_MAX, LEN_MIN, LEN_MAX, BAS_ERR_MAX 
      REAL*8     XSIG_MEAN
      LOGICAL*4  FL_PLOT, FL_NL(M_STA)
      INTEGER*4  NBAS(MBAS), LBAS, KBAS, IBAS, NUMARG, J1, J2, J3, J4, J5, &
     &           L_EPC, LE_STA, IOS, IPAR, IV(MSES), ID, KP, IND, NSGL, IE
      INTEGER*4, EXTERNAL :: LTM_DIF, ILEN, I_LEN
!
      ALLOCATE ( XDAT(MSES,MBAS), XVAL(MSES,MBAS), XSIG(MSES,MBAS) )
!
      NUMARG = IARGC ()
      IF ( NUMARG < 6 ) THEN
           WRITE ( 6, * ) ' Usage: basrep <finam> <lim_obs> <lim_len>'// &
     &            ' <year_start> <year_end> <bas_err_max> [noplot]'
           STOP 'BASREP'
        ELSE
           FL_PLOT = .TRUE.
           CALL GETARG ( 1, FINAM  )
           CALL GETARG ( 2, STR )
           CALL CHIN   ( STR, LIM_OBS )
           CALL GETARG ( 3, STR )
           IF ( INDEX ( STR, '.' ) .EQ. 0 ) STR=STR(1:I_LEN(STR))//'.0'
           READ ( UNIT=STR, FMT='(F15.5)' ) LIM_LEN
           LIM_LEN = 1.0D6 * LIM_LEN
!
           CALL GETARG ( 4, STR )
           IF ( INDEX ( STR, '.' ) .EQ. 0 ) STR=STR(1:I_LEN(STR))//'.0'
           READ ( UNIT=STR, FMT='(F15.5)' ) YEAR_START
!
           CALL GETARG ( 5, STR )
           IF ( INDEX ( STR, '.' ) .EQ. 0 ) STR=STR(1:I_LEN(STR))//'.0'
           READ ( UNIT=STR, FMT='(F15.5)' ) YEAR_END
!
           CALL GETARG ( 6, STR )
           IF ( INDEX ( STR, '.' ) .EQ. 0 ) STR=STR(1:I_LEN(STR))//'.0'
           READ ( UNIT=STR, FMT='(F15.5)' ) BAS_ERR_MAX
           IF ( NUMARG .GE. 7 ) THEN
                CALL GETARG ( 7, STR )
                CALL TRAN ( 11, STR, STR )
                IF ( STR == 'NOPLOT' ) FL_PLOT = .FALSE.
           END IF
      END IF
      CALL GETENVAR ( "SOLVE_NOESM", STR_NO_ESM )
      IF ( ILEN(STR_NO_ESM) > 0 ) CALL TRAN ( 11, STR_NO_ESM, STR_NO_ESM )
!
      FILOUT = '/tmp/basrep.out'
      IF ( INDEX ( FINAM, '.' ) .EQ. 0 ) THEN
           FINAM = FINAM(1:I_LEN(FINAM))//'.bas'
      END IF
!
      IUER = -1
      CALL RD_TEXT ( FINAM, MBUF, BUF, LBUF, IUER )
      IF ( IUER .NE. 0 ) THEN
           STOP 'BASREP'
      END IF
!
! --- Get directory name SAVE_DIR
!
      CALL GETENVAR ( 'PSOLVE_SAVE_DIR', STR )
      IF ( ILEN(STR) .GT. 0 ) THEN
           SAVE_DIR = STR
         ELSE
           SAVE_DIR = SOLVE_SAVE_DIR
      ENDIF
      ID = ILEN(SAVE_DIR)
      IF ( SAVE_DIR(ID:ID) .NE. '/' ) THEN
           ID = ID + 1
           SAVE_DIR(ID:ID) = '/'
      ENDIF
!
! --- Build the name of the epoch of the episodic site motions
!
      FILESM = SAVE_DIR(1:ID)//'glo.esm'
!
! --- Read the file with the epochs of the eposiodic site motion
!
      IUER = -1
      CALL RD_ESM ( FILESM, M_STA, LE_STA, CE_STA, EPOCH_ESM, FL_NL, IUER )
      IF ( IUER .NE. 0 ) THEN
           CALL ERR_LOG ( 715, IUER, 'BASPLO', 'Error in reading the '// &
     &         'episodic site motion file '//FILESM )
           CALL EXIT ( 1 )
      END IF
      IF ( STR_NO_ESM == 'YES' ) THEN
           LE_STA = 0
      END IF
!        write ( 6, * ) ' le_sta = ',le_sta ! %%%
!
      YEAR_MIN =  3000.0
      YEAR_MAX =  1000.0
      LEN_MIN  =  1.5D12
      LEN_MAX  = -1.5D12
      LBAS = 0
      DO 410 J1=1,LBUF
         IF ( BUF(J1)(1:1)  .EQ. '#' ) GOTO 410
         IF ( ILEN(BUF(J1)) .LT. 62  ) GOTO 410
!
! ------ Through away stations with non-linear motion
!
         IE = LTM_DIF ( 0, LE_STA, CE_STA, BUF(J1)(46:53)  )
         IF ( IE > 0 ) THEN
              IF ( FL_NL(IE) ) GOTO 410
         END IF
!
         IE = LTM_DIF ( 0, LE_STA, CE_STA, BUF(J1)(55:62)  )
         IF ( IE > 0 ) THEN
              IF ( FL_NL(IE) ) GOTO 410
         END IF
!
         IBAS = LTM_DIF ( 1, LBAS, BASNAM, BUF(J1)(46:62) )
         IF ( IBAS .LE. 0 ) THEN
              LBAS = LBAS + 1
              BASNAM(LBAS) = BUF(J1)(46:62)
              IBAS = LBAS
              NBAS(IBAS) = 0
         END IF
!
         NBAS(IBAS) = NBAS(IBAS) + 1
!
         READ ( UNIT=BUF(J1)(35:44), FMT='(F10.5)', IOSTAT=IOS ) &
     &          XDAT(NBAS(IBAS),IBAS)
         IF ( IOS .NE. 0 ) THEN
              WRITE ( 6, * )  '1. IOS=',IOS,' J1=',J1
              STOP 'BASDEP'
         END IF
         IF ( XDAT(NBAS(IBAS),IBAS) .LT. YEAR_START ) THEN
              NBAS(IBAS) = NBAS(IBAS) - 1
              GOTO 410
         END IF
         IF ( XDAT(NBAS(IBAS),IBAS) .GT. YEAR_END ) THEN
              NBAS(IBAS) = NBAS(IBAS) - 1
              GOTO 410
         END IF
!
         READ ( UNIT=BUF(J1)(64:77), FMT='(F14.5)', IOSTAT=IOS) XVAL(NBAS(IBAS),IBAS)
         IF ( IOS .NE. 0 ) THEN
              WRITE ( 6, * ) '2. IOS=',IOS,' J1=',J1
              STOP 'BASDEP'
         END IF
!
         IF ( BUF(J1)(78:83) .EQ. '******' ) BUF(J1)(78:83) = '99999.'
         READ ( UNIT=BUF(J1)(78:83), FMT='(F10.5)', IOSTAT=IOS) XSIG(NBAS(IBAS),IBAS)
         IF ( IOS .NE. 0 ) THEN
              WRITE ( 6, * ) '3. IOS=',IOS,' J1=',J1
              STOP 'BASDEP'
         END IF
!
         IF ( XDAT(NBAS(IBAS),IBAS) .LT. YEAR_MIN ) THEN
              YEAR_MIN = XDAT(NBAS(IBAS),IBAS)
         END IF
         IF ( XDAT(NBAS(IBAS),IBAS) .GT. YEAR_MAX ) THEN
              YEAR_MAX = XDAT(NBAS(IBAS),IBAS)
         END IF
         IF ( XVAL(NBAS(IBAS),IBAS) .LT. LEN_MIN ) THEN
              LEN_MIN = XVAL(NBAS(IBAS),IBAS)
         END IF
         IF ( XVAL(NBAS(IBAS),IBAS) .GT. LEN_MAX ) THEN
              LEN_MAX = XVAL(NBAS(IBAS),IBAS)
         END IF
 410  CONTINUE
!
      KBAS = 0
      NSGL = 0
      OPEN ( UNIT=11, FILE=FILOUT, STATUS='UNKNOWN' )
      WRITE ( 11, '(A)' ) 'BASREP Format version v 3.0 2003.01.29'
      WRITE ( 11, '(A)' ) '# Input file: '//FINAM(1:I_LEN(FINAM))
      WRITE ( 11, '(A)' ) '# '
      WRITE ( 11, '(A)' ) '# Baseline          # sess '// &
     &                    'Length in mm    Repeat.  sqrt(chi/ndg)'
      DO 420 J2=1,LBAS
         IF ( NBAS(J2) .GE. LIM_OBS  .AND.  XVAL(1,J2) .LE. LIM_LEN ) THEN
              KBAS = KBAS + 1
!
              FBAS(KBAS) = 0
              XSIG_MEAN = 0.0D0
              DO 430 J3=1,NBAS(J2)
                 W(J3) = 1.D0/XSIG(J3,J2)
                 IF ( XSIG(J3,J2) > BAS_ERR_MAX ) THEN
                      W(J3) = 1.D-6
                 END IF
                 IV(J3) = 1
                 NSGL = NSGL + 1
                 FBAS(KBAS) = FBAS(KBAS) + XSIG(J3,J2)
                 XSIG_MEAN = XSIG_MEAN + XSIG(J3,J2)
 430          CONTINUE
              XSIG_MEAN = XSIG_MEAN/NBAS(J2)
              FBAS(KBAS) = FBAS(KBAS)/NBAS(J2)
!
              ST1 = BASNAM(J2)(1:8)
              ST2 = BASNAM(J2)(10:17)
              IUER = -1
              CALL REPEATABILITY ( 1, ST1, ST2, M_STA, M_EPC, LE_STA, CE_STA, &
     &                         EPOCH_ESM, NBAS(J2), XDAT(1,J2), XVAL(1,J2), W, &
     &                         XSIG(1,J2), IV, L_EPC, KP, DR_VAL, DR_SIG, &
     &                         TIM_EPC, SH_VAL, SH_SIG, RBAS(KBAS), CHI, IUER )
              IF ( CHI < 0.1 ) THEN
                   KBAS = KBAS - 1
                   GOTO 420
              END IF
              RLEN(KBAS) = SH_VAL(1)
              FLEN(KBAS) = SH_VAL(1)
              GARG(KBAS) = J2
              WRITE ( 11, 160 ) BASNAM(J2), NBAS(J2), RLEN(KBAS), RBAS(KBAS), &
     &                          CHI
 160          FORMAT ( 1X, A, 1X, I6, 1X, F15.2, 1X, F8.2, 1X, F9.3 )
         END IF
 420  CONTINUE
      CLOSE ( UNIT=11 )
!
      CALL SORT83 ( KBAS, RLEN, RBAS, GARG )
      CALL SORT8  ( KBAS, FLEN, FBAS )
      DO 440 J4=1,KBAS
         W(J4) = 1.D0
         IV(J4) = 1
         IND = GARG(J4) + 0.1
 440  CONTINUE
      WRITE ( 6, * ) ' LBAS=',LBAS, ' KBAS=',KBAS, ' NSGL = ',NSGL, &
     &               ' LIM_OBS = ',LIM_OBS,' LIM_LEN=',LIM_LEN
      IF ( KBAS .LE. 3 ) THEN
           WRITE ( 6, * ) 'Too few baselines: ',KBAS
           WRITE ( 6, * ) 'Please checks arguments and the input file'
           CALL EXIT ( 1 )
      END IF
      CALL REGRW8 ( KBAS, RLEN, RBAS, W, IV, DR_REP, SH_REP, -3 )
      REP_0 = SH_REP - DR_REP*RLEN(1)
      FCT_0 = DR_REP
!
      CALL REGRW8 ( KBAS, FLEN, FBAS, W, IV, DR_REP, SH_REP, -3 )
      REP_1 = SH_REP - DR_REP*FLEN(1)
      FCT_1 = DR_REP
!
      CALL ROOT_TREND ( KBAS, RLEN, RBAS, A, B, -3 )
      WRITE ( 6, 110 ) FINAM(1:I_LEN(FINAM)), LIM_OBS, &
     &          LEN_MIN*1.D-6, MIN(LIM_LEN,LEN_MAX)*1.D-6, YEAR_MIN, &
     &          YEAR_MAX, A, B, REP_0, FCT_0, &
     &          DSQRT ( A**2 + (B*1.0D9)**2 ), &
     &          DSQRT ( A**2 + (B*2.0D9)**2 ), &
     &          DSQRT ( A**2 + (B*5.0D9)**2 ), &
     &          DSQRT ( A**2 + (B*9.0D9)**2 ), &
     &          DSQRT ( A**2 + (B*10.0D9)**2 ), &
     &          DSQRT ( A**2 + (B*12.756D9)**2 )
 110  FORMAT ( '=======================================', &
     &         '======================================='// &
     &         'Spool file: ',A/ &
     &         'Lower limit of number of sessions: ',I3/ &
     &         'Baseline lengths range: [',F7.1,', ', F7.1,' ]  km'/ &
     &         'Date range:             [',F7.2,', ',F7.2, ' ] '/ &
     &         'Root   Repeatability: ',0PF7.2,' mm + ',1PD10.2,'*L'/ &
     &         'Linear Repeatability: ',0PF7.2,' mm + ',1PD10.2,'*L'/ &
     &         '( rep = sqrt ( A**2 + (B*L)**2 )'// &
     &         'Distance  1000 km : ',0PF7.2,' mm'/ &
     &         'Distance  2000 km : ',0PF7.2,' mm'/ &
     &         'Distance  5000 km : ',0PF7.2,' mm'/ &
     &         'Distance  9000 km : ',0PF7.2,' mm'/ &
     &         'Distance 10000 km : ',0PF7.2,' mm'/ &
     &         'Distance 12756 km : ',0PF7.2,' mm'// &
     &         '=======================================', &
     &         '======================================='/ )
!
      CALL GET_QUAD_TREND ( KBAS, RLEN, RBAS, SH_REP, DR_REP, SQ_REP, IUER )
!!      type *,' sh=',sh,' dr=',dr,' sq=',sq  ! %%
!      WRITE ( 6, 120 ) SH, DR, SQ_REP,
!     #          SH + DR*1.D9 + SQ_REP*(1.D9)**2,
!     #          SH + DR*2.D9 + SQ_REP*(2.D9)**2,
!     #          SH + DR*5.D9 + SQ_REP*(5.D9)**2,
!     #          SH + DR*10.D9 + SQ_REP*(10.D9)**2,
!     #          SH + DR*12.756D9 + SQ_REP*(12.756D9)**2
 120  FORMAT ( 'Quadratic repeatability: ',0PF6.1,'mm + ',1PD10.3, &
     &         '*L + ',1PD10.3,'*L**2'/ &
     &         'Distance  1000 km : ',0PF6.1,' mm'/ &
     &         'Distance  2000 km : ',0PF6.1,' mm'/ &
     &         'Distance  5000 km : ',0PF6.1,' mm'/ &
     &         'Distance 10000 km : ',0PF6.1,' mm'/ &
     &         'Distance 12756 km : ',0PF6.1,' mm' )
!
      DO 450 J5=1,KBAS
         GBAS(J5) = DSQRT ( A**2 + (B*RLEN(J5))**2 )
         GARG(J5) = RLEN(J5)*1.D-6
         RLEN(J5) = RLEN(J5)*1.D-6
         FLEN(J5) = FLEN(J5)*1.D-6
 450  CONTINUE
      GARG(1) = 0.0
      GBAS(1) = SH_REP
      GARG(KBAS) = 12.769D3
      GBAS(KBAS) = SH_REP + DR_REP*12.756D9 + SQ_REP*(12.756D9)**2
!
      GARG(1) = 0.0
      GBAS(1) = A
      GARG(KBAS) = 12.769D3
      GBAS(KBAS) = DSQRT ( A**2 + (B*GARG(KBAS)*1.D6)**2 )
!
      IF ( FL_PLOT ) THEN
           CALL DIAGI_SETDEF ( IUER, 'DIAGI_UNIT', 'Baseline length (km)' )
           CALL DIAGI_2 ( KBAS, RLEN, RBAS, KBAS, GARG, GBAS, IUER )
      END IF
      WRITE ( 6, * ) ' Output file: '//FILOUT(1:I_LEN(FILOUT))
!
      END  !#!  BASREP  #!#
