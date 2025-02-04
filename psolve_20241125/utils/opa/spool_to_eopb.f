      SUBROUTINE SPOOL_TO_EOPB ( SPOOL_FILE, NEW_EOPB_FILE, MASTER_DIR, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  SPOOL_TO_EOPB  calls subroutine getpar_parse for parsing  *
! *   spool-file. Later it writes EOP series extracted from parsing      *
! *   spool file into NEW_EOPB_FILE in EOPB format.                      *
! *                                                                      *
! * ________________________ Input parameters: _________________________ *
! *                                                                      *
! *    SPOOL_FILE ( CHARACTER ) -- Spool file name to be parsed.         *
! * NEW_EOPB_FILE ( CHARACTER ) -- Output file of EOP series in EOPB     *
! *                                format.                               *
! *    MASTER_DIR ( CHARACTER ) -- Directory name where local copy of    *
! *                                master files is located.              *
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
! *    IUER ( INTEGER*4, OPT ) -- Universal error handler.               *
! *                           Input: switch IUER=0 -- no error messages  *
! *                                  will be generated even in the case  *
! *                                  of error. IUER=-1 -- in the case of *
! *                                  error the message will be put on    *
! *                                  stdout.                             *
! *                           Output: 0 in the case of successful        *
! *                                   completion and non-zero in the     *
! *                                   case of error.                     *
! *                                                                      *
! * ### 19-SEP-2000  SPOOL_TO_EOPB  v1.14 (c) L. Petrov 26-OCT-2007 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'solve.i'
      INCLUDE   'erm.i'
      CHARACTER  SPOOL_FILE*(*), NEW_EOPB_FILE*(*), MASTER_DIR*(*)
      INTEGER*4  IUER
      INTEGER*4  M_SES, M_SOU, M_STA, M_COMP, M_LSO, M_LST, M_BAS, M_LS, &
     &           M_TRP, M_ERM, M_HEO, M_NPV, M_APR, MA_STA, MA_BAS
      PARAMETER  ( M_SES = 8, M_SOU = 3072, M_STA = 256, M_COMP = 6, &
     &             M_LSO = 1024, M_LST = 1024, M_BAS = 1024, M_LS = 512, &
     &             M_TRP = 2048, M_HEO = 8192, M_NPV = 128*1024, &
     &             M_APR = 256 )
      PARAMETER  ( MA_STA = MAX_ARC_STA )
      PARAMETER  ( MA_BAS = MAX_ARC_BSL )
      PARAMETER  ( M_ERM = 512 + (ERM__MSPL**2 + 1)*ERM__MKNOT*3 )
      CHARACTER  C_SOU(M_SOU)*8,         RA_VAL(M_SOU)*17,  RA_ERR(M_SOU)*10, &
     &                                   DL_VAL(M_SOU)*17,  DL_ERR(M_SOU)*10, &
     &           LSO_NAME(M_LSO)*8, &
     &                                  LRA_VAL(M_LSO)*17, LRA_ERR(M_LSO)*10, &
     &                                  LDL_VAL(M_LSO)*17, LDL_ERR(M_LSO)*10, &
     &                                  USO_VAL(M_LSO)*4,  TSO_VAL(M_LSO)*4, &
     &                                  LCR_VAL(M_LSO)*7, &
     &           LST_NAME(M_LST)*8, &
     &           CL_VAL(M_COMP,M_LST)*14, CL_ERR(M_COMP,M_LST)*10,  &
     &           C_COO(M_STA)*15,         CSTA_SRT(M_STA)*20,       &
     &           C_CRL(15,M_STA)*5,       S_CRL(M_SOU)*6,           &
     &           C_VAL(M_COMP,M_STA)*14,  C_ERR(M_COMP,M_STA)*10,   &
     &           C_VEL(M_STA)*8,          CVEL_SRT(M_STA)*13,       &
     &           V_VAL(M_COMP,M_STA)*8,   V_ERR(M_COMP,M_STA)*8,    &
     &           C_BAS(M_BAS)*112,        C_HEO(M_HEO)*128,         &
     &           C_NPV(M_NPV)*128,        C_APR(M_APR)*128,         &
     &           START(M_SES)*14,         CN_BAS(M_SES)*(6*MA_BAS), &
     &           C_NET(M_SES)*(2*MA_STA), SOL_ID*64, SOL_DATE*19
      CHARACTER  XEOP_VAL(M_SES)*11,  XEOP_ERR(M_SES)*10,  &
     &           YEOP_VAL(M_SES)*11,  YEOP_ERR(M_SES)*10,  &
     &           XREOP_VAL(M_SES)*11, XREOP_ERR(M_SES)*10, &
     &           YREOP_VAL(M_SES)*11, YREOP_ERR(M_SES)*10, &
     &           UEOP_VAL(M_SES)*11,  UEOP_ERR(M_SES)*10,  &
     &           REOP_VAL(M_SES)*11,  REOP_ERR(M_SES)*10,  &
     &           QEOP_VAL(M_SES)*11,  QEOP_ERR(M_SES)*10,  &
     &           PEOP_VAL(M_SES)*11,  PEOP_ERR(M_SES)*10,  &
     &           EEOP_VAL(M_SES)*11,  EEOP_ERR(M_SES)*10,  &
     &           CEOP(28,M_SES)*6,    RMS_STR(M_SES)*64
      CHARACTER  DBNAME(M_SES)*16, USED(M_SES)*6, &
     &           DURA(M_SES)*10, TAG(M_SES)*14, EPOCH(M_SES)*10, &
     &           CSTA_TRP(M_TRP)*8, RMS_GLO_STR*64
      CHARACTER  OBU_SOU(M_SOU)*7, OBT_SOU(M_SOU)*7, &
     &           SEU_SOU(M_SOU)*5, SET_SOU(M_SOU)*5, &
     &           DAF_SOU(M_SOU)*10, DAL_SOU(M_SOU)*10
      CHARACTER  OBU_STA(M_STA)*7, OBT_STA(M_STA)*7, &
     &           SEU_STA(M_STA)*5, SET_STA(M_STA)*5, &
     &           DAF_STA(M_STA)*10, DAL_STA(M_STA)*10
      CHARACTER  C_ERM(M_ERM)*128
      INTEGER*4  N_SES, N_LSO, N_LST, L_COO, L_VEL, L_BAS, NUT_USAGE
      INTEGER*4  L_SOU, L_TRP, L_HEO, L_NPV, L_ERM, L_APR, IND_SOU(M_SOU), &
     &           LSO_SESIND(M_LSO), LST_SESIND(M_LST), IEXP_TRP(M_TRP), &
     &           N_BAS(M_SES), KR_BAS(MA_BAS,M_SES), KU_BAS(MA_BAS,M_SES)
      REAL*8     RMS_VAL(M_SES), RMS_IND(M_SES)
      REAL*8     MJD_EOP(M_SES), MJD_NUT(M_SES), MJD_TRP(M_TRP), &
     &           ZEN_TRP(M_TRP), ADJ_TRP(M_TRP), ERR_TRP(M_TRP)
      CHARACTER  COMP*(M_COMP), STR*32
      DATA       COMP / 'XYZUEN' /
      LOGICAL*4  FL_SESCODE
      PARAMETER  ( FL_SESCODE = .TRUE. ) ! Yes, we want to resolve db_name
      INTEGER*4  N_LIN, IER
      INTEGER*4  I_LEN
!
! --- Parsing spool file
!
      NUT_USAGE = 2 ! Get nutation angle wrt IAU1980
!
      CALL ERR_PASS ( IUER, IER )
      CALL GETPAR_PARSE ( SPOOL_FILE, &
     &           M_SES, M_SOU, M_STA, M_COMP, M_LSO, M_LST, M_BAS, M_TRP,   &
     &           M_ERM, M_APR, MA_STA, MA_BAS, NUT_USAGE, C_SOU, IND_SOU,   &
     &           RA_VAL, RA_ERR, DL_VAL, DL_ERR,                            &
     &           OBU_SOU, OBT_SOU, SEU_SOU, SET_SOU, DAF_SOU, DAL_SOU,      &
     &           LSO_NAME, LRA_VAL, LRA_ERR, LDL_VAL, LDL_ERR, LCR_VAL,     &
     &           USO_VAL, TSO_VAL, &
     &           LST_NAME, L_ERM,  &
     &           CL_VAL, CL_ERR, C_COO, CSTA_SRT, C_CRL, S_CRL,               &
     &           C_VAL, C_ERR, C_VEL, CVEL_SRT, V_VAL, V_ERR, C_BAS,   C_ERM, &
     &           OBU_STA, OBT_STA, SEU_STA, SET_STA, DAF_STA, DAL_STA,        &
     &           XEOP_VAL, XEOP_ERR, XREOP_VAL, XREOP_ERR,                    &
     &           YEOP_VAL, YEOP_ERR, YREOP_VAL, YREOP_ERR,                    &
     &           UEOP_VAL, UEOP_ERR, &
     &           REOP_VAL, REOP_ERR, &
     &           QEOP_VAL, QEOP_ERR, &
     &           PEOP_VAL, PEOP_ERR, &
     &           EEOP_VAL, EEOP_ERR, &
     &           CEOP, &
     &           M_HEO, L_HEO, C_HEO, &
     &           M_NPV, L_NPV, C_NPV, &
     &           RMS_STR, RMS_VAL, RMS_IND, RMS_GLO_STR, &
     &           DBNAME, USED, START, DURA, TAG, EPOCH, MJD_EOP, MJD_NUT, &
     &           LSO_SESIND, LST_SESIND, &
     &           N_LIN, N_SES, N_LSO, N_LST, L_SOU, L_COO, L_VEL, L_BAS, &
     &           L_TRP, IEXP_TRP, CSTA_TRP, MJD_TRP, ZEN_TRP, &
     &           ADJ_TRP, ERR_TRP, L_APR, C_APR, SOL_ID, SOL_DATE, &
     &           N_BAS, CN_BAS, KR_BAS, KU_BAS, C_NET, IER )
!
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 4291, IUER, 'SPOOL_TO_EOPB', 'Error in parsing '// &
     &         'spool file '//SPOOL_FILE(1:I_LEN(SPOOL_FILE)) )
           RETURN
      END IF
!
      IF ( N_SES .LE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( N_SES, STR )
           CALL ERR_LOG ( 4292, IUER, 'SPOOL_TO_EOPB', 'Error in parsing '// &
     &         'spool file '//SPOOL_FILE(1:I_LEN(SPOOL_FILE))//' number of '// &
     &         'session is '//STR )
           RETURN
      END IF
!
! --- Writing EOP in EOB-format
!
      CALL ERR_PASS ( IUER, IER )
      CALL GETPAR_TO_EOB ( NEW_EOPB_FILE, SPOOL_FILE, FL_SESCODE, MASTER_DIR, &
     &           N_SES, DBNAME, USED, DURA, TAG, XEOP_VAL,  XEOP_ERR, YEOP_VAL, &
     &           YEOP_ERR, XREOP_VAL, XREOP_ERR, YREOP_VAL, YREOP_ERR, &
     &           UEOP_VAL,  UEOP_ERR, REOP_VAL,  REOP_ERR, &
     &           QEOP_VAL,  QEOP_ERR, PEOP_VAL,  PEOP_ERR, &
     &           EEOP_VAL,  EEOP_ERR, CEOP,      RMS_STR, MJD_EOP, MJD_NUT, &
     &           NUT_USAGE, C_NET, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 4292, IUER, 'SPOOL_TO_EOPB', 'Error in an attempt'// &
     &         'to write EOP in EOB format in file '//NEW_EOPB_FILE )
           RETURN
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  SPOOL_TO_EOPB  #!#
