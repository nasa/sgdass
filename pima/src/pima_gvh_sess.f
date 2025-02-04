      SUBROUTINE PIMA_GVH_SESS ( PIM, PIM_2ND, GVH, NUMB_OBS, NUMB_STA, &
     &                           NUMB_SOU, NUMB_SCA, NUMB_BAS, NOBS_STA, &
     &                           OBS_TAB, C_STA, C_SOU, C_SCA, C_BAS, &
     &                           L_STA_CAB, C_STA_CAB, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  PIMA_GVH_SESS
! *                                                                      *
! * ### 10-JUL-2009  PIMA_GVH_SESS   v1.4 (c)  L. Petrov 24-NOV-2019 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'pima.i'
      INCLUDE   'pima_db.i'
      INCLUDE   'gvh.i'
      TYPE     ( PIMA__TYPE ) :: PIM, PIM_2ND
      TYPE     ( GVH__STRU  ) :: GVH
      INTEGER*4  NUMB_OBS, NUMB_STA, NUMB_SOU, NUMB_SCA, NUMB_BAS, &
     &           NOBS_STA(NUMB_STA), OBS_TAB(3,NUMB_OBS), L_STA_CAB, IUER
      CHARACTER  C_STA(NUMB_STA)*(*), C_SOU(NUMB_SOU)*(*), &
     &           C_SCA(NUMB_SCA)*(*), C_BAS(NUMB_BAS)*(*), &
     &           C_STA_CAB(PIM__MSTA)*(*)
      INTEGER*4  MBUF, MIND
      PARAMETER  ( MBUF = 512 )
      PARAMETER  ( MIND =  64 )
      CHARACTER  BUF(MBUF)*128, STR*128, REG*3
      PARAMETER  ( REG = CHAR(0)//CHAR(32)//CHAR(9) )
      REAL*8     RW_DEL__DEF, RW_RAT__DEF
      PARAMETER  ( RW_DEL__DEF = 1.D-13 ) ! Default reweighting
      PARAMETER  ( RW_RAT__DEF = 1.D-14 )
      TYPE     ( CAL__INFO__TYPE ) :: CAL_INFO(M__CAL)
      INTEGER*4  J1, J2, J3, J4, J5, J6, J7, J8, J9, J10, J11, J12, &
     &           LIND, IND(2,MIND), NBUF, USE_BITS, N_CALIB, &
     &           NUMB_FRQ(PIM__MFRQ), NUMB_BND, BITSAMPL, EXPSERNO, &
     &           BAS_USE(PIM__MBAS), SOU_USE(PIM__MSOU), &
     &           CAL_STS(M__CAL*SOLVE__MAX_ARC_STA), IP, KP, &
     &           CAB_SIGN(PIM__MSTA), IER
      REAL*8     SAMPLRAT, SKY_FREQ(PIM__MFRQ), COO_STA(3,PIM__MSTA), &
     &           COO_SOU(2,PIM__MSOU), RWDELVAL(SLV__MAX_SOLTYP,PIM__MBAS), &
     &           RWRATVAL(SLV__MAX_SOLTYP,PIM__MBAS), MEAN_CABLE(PIM__MSTA), &
     &           CHAN_WDT(PIM__MFRQ), SPCH_WDT(PIM__MFRQ)
      INTEGER*2  CHAN_SDB(PIM__MFRQ)
      REAL*8,    ALLOCATABLE :: ATM_CNS(:,:), CLO_CNS(:,:), BSCL_CNS(:,:)
      CHARACTER  EXP_NAME*128, CORPLACE*128, COR_TYPE*8, EXP_CODE*32, &
     &           EXP_DESC*128, MK3_DBNM*10, PI_NAME*128, REC_MODE*128, &
     &           BAND_NAM(2)*1, CAL_NAME(M__CAL)*8
      INTEGER*4, EXTERNAL :: ILEN, I_LEN, LTM_DIF
!
      IF ( ILEN(PIM%CONF%MKDB_2ND_BAND_FILE) > 0 ) THEN
           NUMB_BND = 2
           NUMB_FRQ(1) = PIM%CONF%END_FRQ - PIM%CONF%BEG_FRQ + 1
           NUMB_FRQ(2) = PIM_2ND%CONF%END_FRQ - PIM_2ND%CONF%BEG_FRQ + 1
           BAND_NAM(1) = PIM%CONF%BAND
           BAND_NAM(2) = PIM_2ND%CONF%BAND
         ELSE
           NUMB_BND = 1
           NUMB_FRQ(1) = PIM%NFRQ
           BAND_NAM(1) = PIM%CONF%BAND
           BAND_NAM(2) = ' '
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL PIMA_GVH_DEFINE ( PIM, GVH, NUMB_OBS, NUMB_STA, NUMB_SOU, &
     &                       NUMB_SCA, NUMB_BAS, NUMB_BND, NUMB_FRQ, &
     &                       OBS_TAB, NOBS_STA, C_STA, L_STA_CAB, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 7801, IUER, 'PIMA_GVH_SESS', 'Error in an '// &
     &         'attempt to define the list of parameters to be put in '// &
     &         'the database in the GVH format' )
           RETURN
      END IF
!
      ALLOCATE ( ATM_CNS(NUMB_STA,SLV__MAX_SOLTYP), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( 8*NUMB_STA*SLV__MAX_SOLTYP, STR )
           CALL ERR_LOG ( 7802, IUER, 'PIMA_GVH_SESS', 'Failure in an '// &
     &         'attempt to allocate '//STR(1:I_LEN(STR))//' bytes of '// &
     &         'dynamic memory ATM_CNS' )
           RETURN
      END IF
      ALLOCATE ( CLO_CNS(NUMB_STA,SLV__MAX_SOLTYP), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( 8*NUMB_STA*SLV__MAX_SOLTYP, STR )
           CALL ERR_LOG ( 7803, IUER, 'PIMA_GVH_SESS', 'Failure in an '// &
     &         'attempt to allocate '//STR(1:I_LEN(STR))//' bytes of '// &
     &         'dynamic memory for CLS_CNS' )
           RETURN
      END IF
!
      ALLOCATE ( BSCL_CNS(NUMB_BAS,SLV__MAX_SOLTYP), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( 8*NUMB_BAS*SLV__MAX_SOLTYP, STR )
           CALL ERR_LOG ( 7804, IUER, 'PIMA_GVH_SESS', 'Failure in an '// &
     &         'attempt to allocate '//STR(1:I_LEN(STR))//' bytes of '// &
     &         'dynamic memory for BSCL_CNS' )
           RETURN
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL RD_TEXT  ( PIM%CONF%MKDB_DESC_FILE, MBUF, BUF, NBUF, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 7805, IUER, 'PIMA_GVH_SESS', 'Error in an '// &
     &         'attempt to read experiment description file '// &
     &          PIM%CONF%MKDB_DESC_FILE )
           RETURN
      END IF
!
      DO 410 J1=1,NBUF
         IF ( BUF(J1)(1:1)  == '#' ) GOTO 410
         IF ( ILEN(BUF(J1)) ==  0  ) GOTO 410
         CALL EXWORD ( BUF(J1), MIND, LIND, IND, REG, -2 )
         IF ( LIND < 2 ) GOTO 410
         IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'EXP_NAME:' ) THEN
              CALL CLRCH ( EXP_NAME )
              EXP_NAME = BUF(J1)(IND(1,2):IND(2,LIND))
            ELSE IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'CORPLACE:' ) THEN
              CALL CLRCH ( CORPLACE )
              CORPLACE = BUF(J1)(IND(1,2):IND(2,LIND))
            ELSE IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'EXPSERNO:' ) THEN
              READ ( UNIT=BUF(J1)(IND(1,2):IND(2,2)), FMT='(I8)' ) EXPSERNO
            ELSE IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'EXP_CODE:' ) THEN
              CALL CLRCH ( EXP_CODE )
              EXP_CODE = BUF(J1)(IND(1,2):IND(2,LIND))
            ELSE IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'EXP_DESC:' ) THEN
              CALL CLRCH ( EXP_DESC )
              EXP_DESC = BUF(J1)(IND(1,2):IND(2,LIND))
            ELSE IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'MK3_DBNM:' ) THEN
              CALL CLRCH ( MK3_DBNM )
              MK3_DBNM = BUF(J1)(IND(1,2):IND(2,LIND))
            ELSE IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'PI_NAME:'  ) THEN
              CALL CLRCH ( PI_NAME )
              PI_NAME = BUF(J1)(IND(1,2):IND(2,LIND))
            ELSE IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'REC_MODE:' ) THEN
              CALL CLRCH ( REC_MODE )
              REC_MODE = BUF(J1)(IND(1,2):IND(2,LIND))
        END IF
 410  CONTINUE
!
      SAMPLRAT = 0.0D0
      DO 420 J2=1,PIM%NFRQ
         SKY_FREQ(J2) = PIM%FRQ(J2,PIM%CONF%FRQ_GRP)%FREQ
         CHAN_WDT(J2) = PIM%FRQ(J2,PIM%CONF%FRQ_GRP)%BAND_WIDTH
         SPCH_WDT(J2) = PIM%FRQ(J2,PIM%CONF%FRQ_GRP)%CHAN_WIDTH
         CHAN_SDB(J2) = PIM%FRQ(J2,PIM%CONF%FRQ_GRP)%SIDE_BAND
         SAMPLRAT = SAMPLRAT + CHAN_WDT(J2)*PIM%NLEV(1)
 420  CONTINUE
!
      DO 430 J3=1,NUMB_STA
         COO_STA(1:3,J3) = 0.0D0
         DO 440 J4=1,PIM%NSTA
            IF ( PIM%STA(J4)%IVS_NAME == C_STA(J3) ) THEN
                 COO_STA(1:3,J3) = PIM%STA(J4)%COO(1:3)
            END IF
 440     CONTINUE
         ATM_CNS(J3,1:SLV__MAX_SOLTYP) = ATM_CNS__DEF
         CLO_CNS(J3,1:SLV__MAX_SOLTYP) = CLO_CNS__DEF
 430  CONTINUE
!
      DO 450 J5=1,NUMB_SOU
         COO_SOU(1:2,J5) = 0.0D0
         DO 460 J6=1,PIM%NSOU
            IF ( PIM%SOU(J6)%IVS_NAME == C_SOU(J5) ) THEN
                 COO_SOU(1,J5) = PIM%SOU(J6)%ALPHA
                 COO_SOU(2,J5) = PIM%SOU(J6)%DELTA
            END IF
 460     CONTINUE
 450  CONTINUE
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PLCODE ( GVH, 'EXP_NAME', 1, 0, EXP_NAME, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 7806, IUER, 'PIMA_GVH_SESS', 'Error in putting '// &
     &         '"EXP_NAME " lcode' )
           RETURN
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PLCODE ( GVH, 'CORPLACE', 1, 0, CORPLACE, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 7807, IUER, 'PIMA_GVH_SESS', 'Error in putting '// &
     &         '"CORPLACE" lcode' )
           RETURN
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PLCODE ( GVH, 'COR_TYPE', 1, 0, PIM%CORR_NAME, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 7808, IUER, 'PIMA_GVH_SESS', 'Error in putting '// &
     &         '"COR_TYPE" lcode' )
           RETURN
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PLCODE ( GVH, 'COR_VERS', 1, 0, PIM%CORR_VERS, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 7809, IUER, 'PIMA_GVH_SESS', 'Error in putting '// &
     &         '"COR_TYPE" lcode' )
           RETURN
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PLCODE ( GVH, 'EXPSERNO', 1, 0, EXPSERNO, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 7810, IUER, 'PIMA_GVH_SESS', 'Error in putting '// &
     &         '"EXPSERNO" lcode' )
           RETURN
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PLCODE ( GVH, 'EXP_CODE', 1, 0, EXP_CODE, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 7811, IUER, 'PIMA_GVH_SESS', 'Error in putting '// &
     &         '"EXP_CODE" lcode' )
           RETURN
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PLCODE ( GVH, 'EXP_DESC', 1, 0, EXP_DESC, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 7812, IUER, 'PIMA_GVH_SESS', 'Error in putting '// &
     &         '"EXP_DESC" lcode' )
           RETURN
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PLCODE ( GVH, 'MK3_DBNM', 1, 0, MK3_DBNM, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 7813, IUER, 'PIMA_GVH_SESS', 'Error in putting '// &
     &         '"MKD_DBNM" lcode' )
           RETURN
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PLCODE ( GVH, 'PI_NAME ', 1, 0, PI_NAME, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 7814, IUER, 'PIMA_GVH_SESS', 'Error in putting '// &
     &         '"PI_NAME " lcode' )
           RETURN
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PLCODE ( GVH, 'REC_MODE', 1, 0, REC_MODE, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 7815, IUER, 'PIMA_GVH_SESS', 'Error in putting '// &
     &         '"REC_MODE" lcode' )
           RETURN
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PLCODE ( GVH, 'DB_VERS ', 1, 0, INT2(1), IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 7816, IUER, 'PIMA_GVH_SESS', 'Error in putting '// &
     &         '"DB_VERS" lcode' )
           RETURN
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PLCODE ( GVH, 'APLENGTH', 1, 0, DBLE(PIM%AP_LEN_MAX), IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 7817, IUER, 'PIMA_GVH_SESS', 'Error in putting '// &
     &         '"APLENGTH" lcode' )
           RETURN
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PLCODE ( GVH, 'NUM_SPCH', 1, 0, PIM%NCHN, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 7818, IUER, 'PIMA_GVH_SESS', 'Error in putting '// &
     &         '"APLENGTH" lcode' )
           RETURN
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PLCODE ( GVH, 'BITSAMPL', 1, 0, PIM%NLEV(1), IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 7819, IUER, 'PIMA_GVH_SESS', 'Error in putting '// &
     &         '"BITSAMPL" lcode' )
           RETURN
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PLCODE ( GVH, 'SAMPLRAT', 1, 0, SAMPLRAT, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 7820, IUER, 'PIMA_GVH_SESS', 'Error in putting '// &
     &         '"SAMPLRAT" lcode' )
           RETURN
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PLCODE ( GVH, 'PIMA_VER', 1, 0, PIMA__LABEL, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 7821, IUER, 'PIMA_GVH_SESS', 'Error in putting '// &
     &         '"PIMA_VER" lcode' )
           RETURN
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PLCODE ( GVH, 'PIMA_CNT', 1, 0, PIM%CONF_FILE, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 7822, IUER, 'PIMA_GVH_SESS', 'Error in putting '// &
     &         '"PIMA_CNT" lcode' )
           RETURN
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PLCODE ( GVH, 'CHAN_WDT', 1, 0, CHAN_WDT, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 7823, IUER, 'PIMA_GVH_SESS', 'Error in putting '// &
     &         '"CHAN_WDT" lcode' )
           RETURN
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PLCODE ( GVH, 'CHAN_SDB', 1, 0, CHAN_SDB, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 7824, IUER, 'PIMA_GVH_SESS', 'Error in putting '// &
     &         '"CHAN_SDB" lcode' )
           RETURN
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PLCODE ( GVH, 'SPCH_WDT', 1, 0, SPCH_WDT, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 7825, IUER, 'PIMA_GVH_SESS', 'Error in putting '// &
     &         '"SPCH_WDT" lcode' )
           RETURN
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PLCODE ( GVH, 'BAND_NAM', 1, 0, BAND_NAM, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 7826, IUER, 'PIMA_GVH_SESS', 'Error in putting '// &
     &         '"BAND_NAM" lcode' )
           RETURN
      END IF

!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PLCODE ( GVH, 'NUM_BAND', 1, 0, NUMB_BND, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 7827, IUER, 'PIMA_GVH_SESS', 'Error in putting '// &
     &         '"NUM_BAND" lcode' )
           RETURN
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PLCODE ( GVH, 'BAND_NAM', 1, 0, BAND_NAM, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 7828, IUER, 'PIMA_GVH_SESS', 'Error in putting '// &
     &         '"BAND_NAM" lcode' )
           RETURN
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PLCODE ( GVH, 'NUMB_SOU', 1, 0, NUMB_SOU, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 7829, IUER, 'PIMA_GVH_SESS', 'Error in putting '// &
     &         '"NUMB_SOU" lcode' )
           RETURN
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PLCODE ( GVH, 'SRCNAMES', 1, 0, C_SOU, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 7830, IUER, 'PIMA_GVH_SESS', 'Error in putting '// &
     &         '"SRCNAMES" lcode' )
           RETURN
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PLCODE ( GVH, 'NUM_CHAN', 1, 0, PIM%NCHN, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 7831, IUER, 'PIMA_GVH_SESS', 'Error in putting '// &
     &         '"NUM_CHAN" lcode' )
           RETURN
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PLCODE ( GVH, 'NUM_CHBN', 1, 0, NUMB_FRQ, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 7832, IUER, 'PIMA_GVH_SESS', 'Error in putting '// &
     &         '"NUM_CHBN" lcode' )
           RETURN
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PLCODE ( GVH, 'N_AVBAND', 1, 0, NUMB_BND, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 7833, IUER, 'PIMA_GVH_SESS', 'Error in putting '// &
     &         '"N_AVBAND" lcode' )
           RETURN
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PLCODE ( GVH, 'SKYFRQCH', 1, 0, SKY_FREQ, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 7834, IUER, 'PIMA_GVH_SESS', 'Error in putting '// &
     &         '"SKYFRQCH" lcode' )
           RETURN
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PLCODE ( GVH, 'SIT_COOR', 1, 0, COO_STA, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 7835, IUER, 'PIMA_GVH_SESS', 'Error in putting '// &
     &         '"SIT_COOR" lcode' )
           RETURN
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PLCODE ( GVH, 'SOU_COOR', 1, 0, COO_SOU, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 7836, IUER, 'PIMA_GVH_SESS', 'Error in putting '// &
     &         '"SOU_COOR" lcode' )
           RETURN
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PLCODE ( GVH, 'UTC_MTAI', 1, 0, PIM%UTC_MTAI, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 7837, IUER, 'PIMA_GVH_SESS', 'Error in '// &
     &         'putting "UTC_MTAI" lcode' )
           RETURN
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PLCODE ( GVH, 'DATYP   ', 1, 0, DATYP__DEF, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 7838, IUER, 'PIMA_GVH_SESS', 'Error in '// &
     &         'putting "DATYP   " lcode' )
           RETURN
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PLCODE ( GVH, 'SUPMET  ', 1, 0, SUPMET__DEF, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 7839, IUER, 'PIMA_GVH_SESS', 'Error in '// &
     &         'putting "SUPMET   " lcode' )
           RETURN
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PLCODE ( GVH, 'RWBASNAM', 1, 0, C_BAS, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 7840, IUER, 'PIMA_GVH_SESS', 'Error in '// &
     &         'putting "RWBASNAM" lcode' )
           RETURN
      END IF
!
      USE_BITS = 0
      USE_BITS = IBSET ( USE_BITS, GRPRAT__DTP )
      USE_BITS = IBSET ( USE_BITS, PHSRAT__DTP )
      USE_BITS = IBSET ( USE_BITS, SNBRAT__DTP )
      USE_BITS = IBSET ( USE_BITS, GRPONL__DTP )
      USE_BITS = IBSET ( USE_BITS, PHSONL__DTP )
      USE_BITS = IBSET ( USE_BITS, SNBONL__DTP )
      USE_BITS = IBSET ( USE_BITS, RATONL__DTP )
      USE_BITS = IBSET ( USE_BITS,  PX_GX__DTP )
      USE_BITS = IBSET ( USE_BITS,     GX__DTP )
      USE_BITS = IBSET ( USE_BITS,     PX__DTP )
      USE_BITS = IBSET ( USE_BITS,  SNG_X__DTP )
      IF ( ILEN(PIM%CONF%MKDB_2ND_BAND_FILE) > 0 ) THEN
           USE_BITS = IBSET ( USE_BITS,  G_GXS__DTP )
           USE_BITS = IBSET ( USE_BITS, PX_GXS__DTP )
           USE_BITS = IBSET ( USE_BITS, PS_GXS__DTP )
           USE_BITS = IBSET ( USE_BITS,  PX_GS__DTP )
           USE_BITS = IBSET ( USE_BITS,  PS_GX__DTP )
           USE_BITS = IBSET ( USE_BITS,     GS__DTP )
           USE_BITS = IBSET ( USE_BITS,  PS_GS__DTP )
           USE_BITS = IBSET ( USE_BITS,     PS__DTP )
           USE_BITS = IBSET ( USE_BITS,  P_PXS__DTP )
           USE_BITS = IBSET ( USE_BITS,  SNG_S__DTP )
      END IF
      DO 470 J7=1,NUMB_BAS
         DO 480 J8=1,SLV__MAX_SOLTYP
            RWDELVAL(J8,J7) = RW_DEL__DEF
            RWRATVAL(J8,J7) = RW_RAT__DEF
 480     CONTINUE 
         BAS_USE(J7)  = USE_BITS
         BSCL_CNS(J7,1:SLV__MAX_SOLTYP) = BSCL_CNS__DEF
 470  CONTINUE
      DO 490 J9=1,NUMB_SOU
         SOU_USE(J9)  = USE_BITS
 490  CONTINUE
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PLCODE ( GVH, 'RWDELVAL', 1, 0, RWDELVAL, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 7841, IUER, 'PIMA_GVH_SESS', 'Error in '// &
     &         'putting "RWBASNAM" lcode' )
           RETURN
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PLCODE ( GVH, 'RWRATVAL', 1, 0, RWRATVAL, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 7842, IUER, 'PIMA_GVH_SESS', 'Error in '// &
     &         'putting "RWRATVAL" lcode' )
           RETURN
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PLCODE ( GVH, 'BAS_USE ', 1, 0, BAS_USE, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 7843, IUER, 'PIMA_GVH_SESS', 'Error in '// &
     &         'putting "BAS_USE" lcode' )
           RETURN
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PLCODE ( GVH, 'SOU_USE ', 1, 0, SOU_USE, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 7844, IUER, 'PIMA_GVH_SESS', 'Error in '// &
     &         'putting "SOU_USE" lcode' )
           RETURN
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PLCODE ( GVH, 'NUM_CLRF', 1, 0, 1, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 7845, IUER, 'PIMA_GVH_SESS', 'Error in '// &
     &         'putting "NUM_CLRF" lcode' )
           RETURN
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PLCODE ( GVH, 'STA_CLRF', 1, 0, C_STA(1), IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 7846, IUER, 'PIMA_GVH_SESS', 'Error in '// &
     &         'putting "STA_CLRF" lcode' )
           RETURN
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PLCODE ( GVH, 'ATM_CNS ', 1, 0, ATM_CNS, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 7847, IUER, 'PIMA_GVH_SESS', 'Error in '// &
     &         'putting "ATM_CNS " lcode' )
           RETURN
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PLCODE ( GVH, 'CLO_CNS ', 1, 0, CLO_CNS, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 7848, IUER, 'PIMA_GVH_SESS', 'Error in '// &
     &         'putting "CLO_CNS " lcode' )
           RETURN
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PLCODE ( GVH, 'BSCL_CNS', 1, 0, CLO_CNS, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 7849, IUER, 'PIMA_GVH_SESS', 'Error in '// &
     &         'putting "BSCL_CNS" lcode' )
           RETURN
      END IF
!
      IF ( L_STA_CAB > 0 ) THEN
           N_CALIB = 1
         ELSE
           N_CALIB = 0
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PLCODE ( GVH, 'N_CALIB ', 1, 0, N_CALIB, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 7850, IUER, 'PIMA_GVH_SESS', 'Error in '// &
     &         'putting "N_CALIB " lcode' )
           RETURN
      END IF
!
      IF ( L_STA_CAB > 0 ) THEN
           CALL NOUT_I4 ( M__CAL*SOLVE__MAX_ARC_STA, CAL_STS )
           DO 4100 J10=1,NUMB_STA
              CAL_NAME(1)       = 'CABL_DEL'
              CAL_INFO(1)%CLASS = GVH__STA
              CAL_INFO(1)%MODE  = CAL__DEL
              IP = LTM_DIF ( 0, L_STA_CAB, C_STA_CAB, C_STA(J10) )
              KP = J10
              DO 4110 J11=FIRST__DTP,LAST__DTP
                 CAL_STS(KP) = IBSET ( CAL_STS(KP), J11 )
 4110         CONTINUE
              CAL_STS(KP) = IBSET ( CAL_STS(KP), SLV__MAX_SOLTYP-1 )
 4100      CONTINUE
!
           CALL ERR_PASS   ( IUER, IER )
           CALL GVH_PLCODE ( GVH, 'CAL_NAME', 1, 0, CAL_NAME, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 7851, IUER, 'PIMA_GVH_SESS', 'Error in '// &
     &              'putting "CAL_NAME" lcode' )
                RETURN
           END IF
!
           CALL ERR_PASS   ( IUER, IER )
           CALL GVH_PLCODE ( GVH, 'CAL_INFO', 1, 0, CAL_INFO, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 7852, IUER, 'PIMA_GVH_SESS', 'Error in '// &
     &              'putting "CAL_INFO" lcode' )
                RETURN
           END IF
!
           CALL ERR_PASS   ( IUER, IER )
           CALL GVH_PLCODE ( GVH, 'CAL_STS ', 1, 0, CAL_STS, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 7853, IUER, 'PIMA_GVH_SESS', 'Error in '// &
     &              'putting "CAL_STS " lcode' )
                RETURN
           END IF
!
! -------- Collect information about the mean cable and the cable sign
!
           DO 4120 J12=1,PIM%NSTA
              IF ( PIM%STA(J12)%CABLE%CAB_AVAIL .AND. &
     &             PIM%STA(J12)%CABLE%NPOI > 0        ) THEN
                   MEAN_CABLE(J12) = PIM%STA(J12)%CABLE%MEAN_CABLE
                   CAB_SIGN(J12)   = PIM%STA(J12)%CABLE%CABLE_SIGN
                 ELSE
                   MEAN_CABLE(J12) = 0.0D0
                   CAB_SIGN(J12)   = 0
              END IF
 4120      CONTINUE
!
           CALL ERR_PASS ( IUER, IER )
           CALL GVH_PLCODE ( GVH, 'MEANCABL', 1, 0, MEAN_CABLE, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 7854, IUER, 'PIMA_GVH_SESS', 'Error in '// &
     &              'putting "MEANCABL" lcode' )
                RETURN
           END IF
!
           CALL ERR_PASS ( IUER, IER )
           CALL GVH_PLCODE ( GVH, 'CABL_SGN', 1, 0, CAB_SIGN, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 7855, IUER, 'PIMA_GVH_SESS', 'Error in '// &
     &              'putting "CAB_SIGN" lcode' )
                RETURN
           END IF
      END IF
!
      DEALLOCATE ( ATM_CNS  )
      DEALLOCATE ( CLO_CNS  )
      DEALLOCATE ( BSCL_CNS )
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  PIMA_GVH_SESS  !#!#

