      SUBROUTINE IONO_DTEC_DB_UPDATE ( DB_NAME, EXP_VERS, FIL_DTEC, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine IONO_DTEC_DB_UPDATE
! *                                                                      *
! * ## 11-FEB-2022 IONO_DTEC_DB_UPDATE v3.0 (c) L. Petrov 08-DEC-2022 ## *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'solve.i'
      INCLUDE   'precm.i'
      INCLUDE   'vcat.i'
      INCLUDE   'gvh.i'
      INTEGER*4  EXP_VERS, IUER
      CHARACTER  DB_NAME*(*), FIL_DTEC*(*)
      TYPE     ( VCAT__TYPE ) :: VCAT
      TYPE     ( GVH__STRU  ) :: GVH
      INTEGER*4  M_FIL, MDB_FIL, MIND, M_ENV
      PARAMETER  ( M_FIL   = 8192 )
      PARAMETER  ( M_ENV   =   16  )
      PARAMETER  ( MDB_FIL =   32  )
      PARAMETER  ( MIND    =   32  )
      CHARACTER  DIR_DTEC*128, BUF(MAX_OBS)*256, &
     &           DB_FILES(MDB_FIL)*128, FINAM*128, &
     &           DTEC_OAP*128, DTEC_OAD*128, DESCR*128
      INTEGER*8  DIR_DESC(16), IP8
      CHARACTER  REPO__DEF*3
      PARAMETER  ( REPO__DEF = 'OBS' ) 
      CHARACTER  VCAT_CONF_FILE*128, GVF_DB_DIR*128, REPO*3, ENV_FILE*128, &
     &           GVF_ENV_DIR*128, MODE_STR*128, SAVE_DIR*128, &
     &           C_STA(MAX_ARC_STA)*8, BUF_ENV(M_ENV)*128, STR*128
      REAL*8     TEC_APR(2,MAX_OBS), DTEC_ADJ(2,MAX_OBS), DTEC_ERR(MAX_OBS), &
     &           DEL_BIAS(MAX_OBS)
      INTEGER*4  N_OBS, N_SCA, N_STA, NOBS_STA(MAX_ARC_STA), OBS_TAB(3,MAX_OBS)
      INTEGER*4  J1, J2, J3, J4, J5, J6, K6, IS, IB, IE, IL, ID, INDS, DIMS(2), &
     &           LIND, IND(2,MIND), LEV, LDB_FIL, IND_REP, REMAINED_BYTES, &
     &           NBUF, SEG_FR1, SEG_CL1, SEG_SL1, CLASS, TYP, NUM_FIELDS, &
     &           SEG_IND, LEN_REC, LEN_DATA, LEN_DATA_APR, LEN_DATA_DTEC, &
     &           LEN_DATA_STS, AUTO_SUP(MAX_OBS), USER_SUP(MAX_OBS), VERS, &
     &           N_ENV, IER
      INTEGER*2  DBDT_STS(MAX_OBS)
      INTEGER*8  ADR_DATA
      INTEGER*4, EXTERNAL :: GET_FILE_FROM_DIR, I_LEN, ILEN, GET_UNIT, WRITE, &
     &                       CLOSE, ADD_CLIST, LINDEX, LTM_DIF
      INTEGER*2  INT2_ARG
      INTEGER*4  INT4
      INT4(INT2_ARG) = INT(INT2_ARG,KIND=4)
!
      DTEC_OAP = 'GNSS TEC map CODE'
!
      CALL GETENVAR  ( 'VCAT_CONF', VCAT_CONF_FILE )
      IF ( ILEN(VCAT_CONF_FILE) == 0 ) THEN
           CALL GETENVAR ( 'PSOLVE_SAVE_DIR', SAVE_DIR )
           IF ( ILEN(SAVE_DIR) == 0 ) THEN
                VCAT_CONF_FILE = SOLVE_SAVE_DIR//'/vcat.conf'
              ELSE
                VCAT_CONF_FILE = TRIM(SAVE_DIR)//'/vcat.conf'
           END IF
      END IF
!
! --- Read and parse VCAT configuration
!
      CALL ERR_PASS ( IUER, IER )
      CALL VCAT_GET_CONF ( VCAT_CONF_FILE, VCAT, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 7241, IUER, 'IONO_DTEC_DB_UPDATE', 'Error in '// &
     &         'parsing VCAT configuration file '//VCAT_CONF_FILE )
           RETURN
      END IF
!
      CALL GETENVAR ( 'VCAT_REPO', REPO )
      IF ( ILEN(REPO) == 0 ) REPO = REPO__DEF
      CALL TRAN ( 12, MODE_STR, MODE_STR )
!
      IND_REP = 0
      DO 410 J1=1,VCAT%NREPS
         IF ( VCAT%GVF_REP_NAME(J1) == REPO ) THEN
              IND_REP = J1
         END IF
 410  CONTINUE 
      IF ( IND_REP == 0 ) THEN
           CALL ERR_LOG ( 7242, IUER, 'IONO_DTEC_DB_UPDATE', &
     &         'Repository '//TRIM(REPO)//' specified in the '// &
     &         'environment variable VCAT_REPO is not '// &
     &         'defined in the VCAT configuration file '// &
     &         VCAT%CONF_FILE )
           RETURN
      END IF
!
! --- Resolve database name
!
      CALL ERR_PASS ( IUER, IER )
      CALL VCAT_RESOLVE_DBNAME ( VCAT, DB_NAME, REPO, ENV_FILE, MDB_FIL, &
     &                           LDB_FIL, DB_FILES, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 7243, IUER, 'IONO_DTEC_DB_UPDATE', 'Error in an '// &
     &         'attempt to resolve database name '//DB_NAME )
           RETURN
      END IF
      ID = LINDEX ( ENV_FILE, '/' )
      GVF_ENV_DIR = ENV_FILE(1:ID-1)
      ID = LINDEX ( DB_FILES(1), '/' )
      GVF_DB_DIR  = DB_FILES(1)(1:ID-1)
!
      IF ( ILEN(FIL_DTEC) == 0 ) THEN
           CALL ERR_LOG ( 7244, IUER, 'IONO_DTEC_DB_UPDATE', 'Did not find '// &
     &         'DTEC file for database '//TRIM(DB_NAME)//' in directory '// &
     &          DIR_DTEC )
           RETURN
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_INIT ( GVH,  IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 7245, IUER, 'IONO_DTEC_DB_UPDATE', 'Failure to initialize GVH' )
           RETURN
      END IF
!
      DO 430 J3=1,LDB_FIL
         CALL ERR_PASS ( IUER, IER )
         CALL GVH_READ_BGV ( GVH, 1, DB_FILES(J3), REMAINED_BYTES, IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 7246, IUER, 'IONO_DTEC_DB_UPDATE', 'Error in '// &
     &            'an atttempt to read input database file '// &
     &             DB_FILES(J3) )
              RETURN
         END IF
         IF ( REMAINED_BYTES .NE. 0 ) THEN
              CALL CLRCH ( STR )
              CALL INCH  ( REMAINED_BYTES, STR )
              CALL ERR_LOG ( 7247, IUER, 'IONO_DTEC_DB_UPDATE', 'The number '// &
     &            'of remaining bytes after reading input database file '// &
     &             TRIM(DB_FILES(J3))//' is not 0, but '//STR )
              RETURN
         END IF
         IF ( INDEX ( DB_FILES(J3), '_fr1_' ) > 0 ) THEN
              SEG_FR1 = GVH%NSEG 
           ELSE IF ( INDEX ( DB_FILES(J3), '_cl1_' ) > 0  ) THEN
              SEG_CL1 = GVH%NSEG 
           ELSE IF ( INDEX ( DB_FILES(J3), '_sl1_'  ) > 0 ) THEN
              SEG_SL1 = GVH%NSEG 
         END IF
 430  CONTINUE 
!
      CALL ERR_PASS   ( IUER, IER )
      CALL GVH_PREGET ( GVH,  IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 7248, IUER, 'IONO_DTEC_DB_UPDATE', 'Error in an '// &
     &         'attempt to execute GVH_PREGET' )
           RETURN
      END IF
!
      CALL ERR_PASS   ( IUER, IER )
      CALL GVH_GLCODE ( GVH, 'NUMB_OBS', 0, 0, 4, DIMS(1), DIMS(2), N_OBS, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 7249, IUER, 'IONO_DTEC_DB_UPDATE', 'Error in '// &
     &         'getting lcode NUMB_OBS' )
           RETURN
      END IF
!
      CALL ERR_PASS   ( IUER, IER )
      CALL GVH_GLCODE ( GVH, 'NUMB_SCA', 0, 0, 4, DIMS(1), DIMS(2), N_SCA, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 7250, IUER, 'IONO_DTEC_DB_UPDATE', 'Error in '// &
     &         'getting lcode NUMB_SCA' )
           RETURN
      END IF
!
      CALL ERR_PASS   ( IUER, IER )
      CALL GVH_GLCODE ( GVH, 'NUMB_STA', 0, 0, 4, DIMS(1), DIMS(2), N_STA, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 7251, IUER, 'IONO_DTEC_DB_UPDATE', 'Error in '// &
     &         'getting lcode NUMB_STA' )
           RETURN
      END IF
!
      CALL ERR_PASS   ( IUER, IER )
      CALL GVH_GLCODE ( GVH, 'NOBS_STA', 0, 0, 4*N_STA, DIMS(1), DIMS(2), NOBS_STA, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 7252, IUER, 'IONO_DTEC_DB_UPDATE', 'Error in '// &
     &         'getting lcode NOBS_STA' )
           RETURN
      END IF
!
      CALL ERR_PASS   ( IUER, IER )
      CALL GVH_GLCODE ( GVH, 'OBS_TAB ', 0, 0, 4*3*N_OBS, DIMS(1), DIMS(2), OBS_TAB, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 7253, IUER, 'IONO_DTEC_DB_UPDATE', 'Error in '// &
     &         'getting lcode OBS_TAB' )
           RETURN
      END IF
!
      CALL ERR_PASS   ( IUER, IER )
      CALL GVH_GLCODE ( GVH, 'SITNAMES', 0, 0, 8*N_STA, DIMS(1), DIMS(2), %REF(C_STA), IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 7254, IUER, 'IONO_DTEC_DB_UPDATE', 'Error in '// &
     &         'getting lcode SITNAMES' )
           RETURN
      END IF
!
      DO 440 J4=1,N_OBS
         CALL ERR_PASS   ( IUER, IER )
         CALL GVH_GLCODE ( GVH, 'USER_SUP', J4, 1, 4, DIMS(1), DIMS(2), USER_SUP(J4), IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 7255, IUER, 'IONO_DTEC_DB_UPDATE', 'Error in '// &
     &            'getting lcode USER_SUP' )
              RETURN
         END IF
!
         CALL ERR_PASS   ( IUER, IER )
         CALL GVH_GLCODE ( GVH, 'AUTO_SUP', J4, 1, 4, DIMS(1), DIMS(2), AUTO_SUP(J4), IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 7256, IUER, 'IONO_DTEC_DB_UPDATE', 'Error in '// &
     &            'getting lcode AUTO_SUP' )
              RETURN
         END IF
 440  CONTINUE 
!
      IER = 0
      CALL GVH_INQ_LCODE ( GVH, 'DTEC    ', DESCR, CLASS, TYP, DIMS, &
     &                           NUM_FIELDS, SEG_IND, LEN_REC, LEN_DATA_DTEC, &
     &                           ADR_DATA, IER )
!
      IER = 0
      CALL GVH_INQ_LCODE ( GVH, 'DTEC_ADJ', DESCR, CLASS, TYP, DIMS, &
     &                           NUM_FIELDS, SEG_IND, LEN_REC, LEN_DATA, &
     &                           ADR_DATA, IER )
      IF ( LEN_DATA == 0 ) THEN
           CALL ERR_PASS   ( IUER, IER )
           CALL GVH_PTOC ( GVH, 'DTEC_ADJ', GVH__R8, GVH__BAS, 1, 1, &
     &         'Adjustement of the differential total electron contents, TEC units', &
     &          SEG_FR1, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 7257, IUER, 'IONO_DTEC_DB_UPDATE', 'Error in '// &
     &              'defining lcode DTEC_ADJ' )
                RETURN
           END IF
      END IF
!
      CALL ERR_PASS   ( IUER, IER )
      CALL GVH_INQ_LCODE ( GVH, 'DTEC_SIG', DESCR, CLASS, TYP, DIMS, &
     &                           NUM_FIELDS, SEG_IND, LEN_REC, LEN_DATA, &
     &                           ADR_DATA, IER )
      IF ( LEN_DATA == 0 ) THEN
           CALL ERR_PASS   ( IUER, IER )
           CALL GVH_PTOC ( GVH, 'DTEC_SIG', GVH__R8, GVH__BAS, 1, 1, &
     &         'Standard deviation of dTec estimate, TEC units', &
     &          SEG_FR1, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 7258, IUER, 'IONO_DTEC_DB_UPDATE', 'Error in '// &
     &              'defining lcode DTEC_SIG' )
                RETURN
           END IF
      END IF
!
      CALL ERR_PASS   ( IUER, IER )
      CALL GVH_INQ_LCODE ( GVH, 'DTEC_APR', DESCR, CLASS, TYP, DIMS, &
     &                           NUM_FIELDS, SEG_IND, LEN_REC, LEN_DATA_APR, &
     &                           ADR_DATA, IER )
!
      CALL ERR_PASS   ( IUER, IER )
      CALL GVH_INQ_LCODE ( GVH, 'TEC_APR ', DESCR, CLASS, TYP, DIMS, &
     &                           NUM_FIELDS, SEG_IND, LEN_REC, LEN_DATA, &
     &                           ADR_DATA, IER )
      IF ( LEN_DATA == 0 ) THEN
           CALL ERR_PASS ( IUER, IER )
           CALL GVH_PTOC ( GVH, 'TEC_APR ', GVH__R8, GVH__BAS, 2, 1, &
     &         'A priori total electron contents, TEC units', &
     &          SEG_CL1, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 7259, IUER, 'IONO_DTEC_DB_UPDATE', 'Error in '// &
     &              'defining lcode DTEC' )
                RETURN
           END IF
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_INQ_LCODE ( GVH, 'DTEC_OAP', DESCR, CLASS, TYP, DIMS, &
     &                           NUM_FIELDS, SEG_IND, LEN_REC, LEN_DATA, &
     &                           ADR_DATA, IER )
      IF ( LEN_DATA == 0 ) THEN
           CALL ERR_PASS ( IUER, IER )
           CALL GVH_PTOC ( GVH, 'DTEC_OAP', GVH__C1, GVH__SES, LEN(DTEC_OAP), &
     &          1, 'Origin of the a priori dTEC', SEG_CL1, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 7260, IUER, 'IONO_DTEC_DB_UPDATE', 'Error in '// &
     &              'defining lcode DTEC_OAP' )
                RETURN
           END IF
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_INQ_LCODE ( GVH, 'DTEC_OAD', DESCR, CLASS, TYP, DIMS, &
     &                           NUM_FIELDS, SEG_IND, LEN_REC, LEN_DATA, &
     &                           ADR_DATA, IER )
      IF ( LEN_DATA == 0 ) THEN
           CALL ERR_PASS ( IUER, IER )
           CALL GVH_PTOC ( GVH, 'DTEC_OAD', GVH__C1, GVH__SES, LEN(DTEC_OAD), 1, &
     &         'Software that generated adjustments to dTEC', SEG_CL1, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 7261, IUER, 'IONO_DTEC_DB_UPDATE', 'Error in '// &
     &              'defining lcode DTEC_OAD' )
                RETURN
           END IF
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_INQ_LCODE ( GVH, 'DEL_BIAS', DESCR, CLASS, TYP, DIMS, &
     &                           NUM_FIELDS, SEG_IND, LEN_REC, LEN_DATA, &
     &                           ADR_DATA, IER )
      IF ( LEN_DATA == 0 ) THEN
           CALL ERR_PASS ( IUER, IER )
           CALL GVH_PTOC ( GVH, 'DEL_BIAS', GVH__R8, GVH__BAS, 1, 1, &
     &         'Delay bias of the upper band wrt the low band in sec', &
     &          SEG_CL1, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 7262, IUER, 'IONO_DTEC_DB_UPDATE', 'Error in '// &
     &              'defining lcode DEL_BIAS' )
                RETURN
           END IF
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_INQ_LCODE ( GVH, 'DBDT_STS', DESCR, CLASS, TYP, DIMS, &
     &                           NUM_FIELDS, SEG_IND, LEN_REC, LEN_DATA_STS, &
     &                           ADR_DATA, IER )
      IF ( LEN_DATA_STS == 0 ) THEN
           CALL ERR_PASS ( IUER, IER )
           CALL GVH_PTOC ( GVH, 'DBDT_STS', GVH__I2, GVH__BAS, 1, 1, &
     &         'Status of dual-band observations for dTEC adjustment', &
     &          SEG_CL1, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 7263, IUER, 'IONO_DTEC_DB_UPDATE', 'Error in '// &
     &              'defining lcode DBDT_STS' )
                RETURN
           END IF
      END IF
!
! --- Remove obsolete data
!
      IF ( LEN_DATA_DTEC > 0 ) THEN
           CALL ERR_PASS ( IUER, IER )
           CALL GVH_DTOC ( GVH, 'DTEC    ', SEG_FR1, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 7264, IUER, 'IONO_DTEC_DB_UPDATE', 'Error in '// &
     &              'deleting lcode DTEC' )
                RETURN
           END IF
      END IF
      IF ( LEN_DATA_APR > 0 ) THEN
           CALL ERR_PASS ( IUER, IER )
           CALL GVH_DTOC ( GVH, 'DTEC_APR', SEG_CL1, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 7265, IUER, 'IONO_DTEC_DB_UPDATE', 'Error in '// &
     &              'deleting lcode DTEC_APR' )
                RETURN
           END IF
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PREPUT ( GVH, N_OBS, N_SCA, N_STA, NOBS_STA, C_STA, OBS_TAB, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 7266, IUER, 'IONO_DTEC_DB_UPDATE', 'Error in an attempt to '// &
     &                   'insert mandatory lcodes an initialize cache' )
           RETURN
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL RD_TEXT ( FIL_DTEC, MAX_OBS, BUF, NBUF, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 7267, IUER, 'IONO_DTEC_DB_UPDATE', 'Error in an attempt to '// &
     &                   'read dtec file '//FIL_DTEC )
           RETURN
      END IF
!
      TEC_APR = 0.0D0
      DTEC_ADJ = 0.0D0
      DTEC_ERR = 0.0D0
      DBDT_STS = 0
      DO 450 J5=1,NBUF
         CALL ERR_PASS ( IUER, IER )
         CALL EXWORD ( BUF(J5), MIND, LIND, IND, CHAR(0)//CHAR(32)//CHAR(9), IER )
         IF ( LIND .GE. 3 ) THEN
              IF ( BUF(J5)(IND(1,2):IND(2,2)) == 'Experiment:' ) THEN
                   IF ( BUF(J5)(IND(1,3):IND(2,3)) .NE. DB_NAME ) THEN
                        CALL ERR_LOG ( 7268, IUER, 'IONO_DTEC_DB_UPDATE', 'Database name '// &
     &                      'mismatch: requested db_name: '//TRIM(DB_NAME)// &
     &                      ' the database name in DTEC file '//TRIM(FIL_DTEC)// &
     &                      ' : '//BUF(J5)(IND(1,3):IND(2,3)) )
                        RETURN
                   END IF
                ELSE IF ( BUF(J5)(IND(1,2):IND(2,2)) == 'Generator:' ) THEN
                   DTEC_OAD = BUF(J5)(IND(1,3):IND(2,LIND)) 
              END IF
         END IF
!
         IF ( BUF(J5)(IND(1,1):IND(2,1)) == 'Ind_obs:' ) THEN
              CALL CHIN ( BUF(J5)(IND(1,2):IND(2,2)), INDS )
              READ ( UNIT=BUF(J5)(IND(1,4):IND(2,4)),   FMT='(F10.2)' ) TEC_APR(1,INDS)
              READ ( UNIT=BUF(J5)(IND(1,5):IND(2,5)),   FMT='(F10.2)' ) TEC_APR(2,INDS)
              READ ( UNIT=BUF(J5)(IND(1,7):IND(2,7)),   FMT='(F9.3)'  ) DTEC_ADJ(1,INDS)
              READ ( UNIT=BUF(J5)(IND(1,8):IND(2,8)),   FMT='(F9.3)'  ) DTEC_ADJ(2,INDS)
              READ ( UNIT=BUF(J5)(IND(1,10):IND(2,10)), FMT='(F8.3)'  ) DTEC_ERR(INDS)
              READ ( UNIT=BUF(J5)(IND(1,12):IND(2,12)), FMT='(F13.6)' ) DEL_BIAS(INDS)
              DBDT_STS(INDS) = IBSET ( DBDT_STS(INDS), DTD__STS )
              IF ( BUF(J5)(IND(1,14):IND(2,14)) == 'T' ) THEN
                   DBDT_STS(INDS) = IBSET ( DBDT_STS(INDS), DTH__STS )
              END IF
              IF ( BUF(J5)(IND(1,15):IND(2,15)) == 'T' ) THEN
                   DBDT_STS(INDS) = IBSET ( DBDT_STS(INDS), DTL__STS )
              END IF
              IF ( BUF(J5)(IND(1,16):IND(2,16)) == 'T' ) THEN
                   DBDT_STS(INDS) = IBSET ( DBDT_STS(INDS), DTHL__STS )
              END IF
!
              CALL ERR_PASS   ( IUER, IER )
              CALL GVH_PLCODE ( GVH, 'DTEC_ADJ', INDS, 1, DTEC_ADJ(2,INDS) - DTEC_ADJ(1,INDS), IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 7269, IUER, 'IONO_DTEC_DB_UPDATE', 'Error in '// &
     &                 'putting "DTEC    " lcode while processing database '//DB_NAME )
                   RETURN
              END IF
!
              CALL ERR_PASS   ( IUER, IER )
              CALL GVH_PLCODE ( GVH, 'TEC_APR ', INDS, 1, TEC_APR(1,INDS), IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 7270, IUER, 'IONO_DTEC_DB_UPDATE', 'Error in '// &
     &                 'putting "TEC_APR" lcode while processing database '//DB_NAME )
                   RETURN
              END IF
!
              CALL ERR_PASS   ( IUER, IER )
              CALL GVH_PLCODE ( GVH, 'DTEC_SIG', INDS, 1, DTEC_ERR(INDS), IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 7271, IUER, 'IONO_DTEC_DB_UPDATE', 'Error in '// &
     &                 'putting "DTEC_ERR" lcode while processing database '//DB_NAME )
                   RETURN 
              END IF
!
              CALL ERR_PASS   ( IUER, IER )
              CALL GVH_PLCODE ( GVH, 'DEL_BIAS', INDS, 1, DEL_BIAS(INDS), IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 7272, IUER, 'IONO_DTEC_DB_UPDATE', 'Error in '// &
     &                 'putting "DEL_BIAS" lcode while processing database '//DB_NAME )
                   RETURN 
              END IF
!
              IF ( LEN_DATA_STS == 0 ) THEN
!
! ---------------- No prior DTEC data?
!
                   CALL ERR_PASS   ( IUER, IER )
                   CALL GVH_PLCODE ( GVH, 'DBDT_STS', INDS, 1, DBDT_STS(INDS), IER )
                   IF ( IER .NE. 0 ) THEN
                        CALL ERR_LOG ( 7273, IUER, 'IONO_DTEC_DB_UPDATE', 'Error in '// &
     &                      'putting "DBDT_STS" lcode while processing database '//DB_NAME )
                        RETURN 
                   END IF
!
! ---------------- Perform initialization of the suppression status for fused data type
!
                   IF ( BUF(J5)(IND(1,14):IND(2,14)) == 'T' .OR. BUF(J5)(IND(1,15):IND(2,15)) == 'T' ) THEN
                        USER_SUP(INDS) = IBCLR ( USER_SUP(INDS), INT4(FUSED__DTP) )
!
! --------------------- Set the FURE__SPS bit indicating whether a given observation is
! --------------------- recoverable for the FUSED data type because it was used either
! --------------------- at the higher or at the lower band
!
                        AUTO_SUP(INDS) = IBSET ( AUTO_SUP(INDS), INT4(FURE__SPS) )
                     ELSE
                        USER_SUP(INDS) = IBSET ( USER_SUP(INDS), INT4(FUSED__DTP) )
                        AUTO_SUP(INDS) = IBCLR ( AUTO_SUP(INDS), INT4(FURE__SPS) )
                   END IF
!
                   CALL ERR_PASS   ( IUER, IER )
                   CALL GVH_PLCODE ( GVH, 'AUTO_SUP', INDS, 1, AUTO_SUP(INDS), IER )
                   IF ( IER .NE. 0 ) THEN
                        CALL ERR_LOG ( 7274, IUER, 'IONO_DTEC_DB_UPDATE', 'Error in '// &
          &                 'putting "AUTO_SUP" lcode while processing database '//DB_NAME )
                        RETURN
                   END IF
!
                   CALL ERR_PASS   ( IUER, IER )
                   CALL GVH_PLCODE ( GVH, 'USER_SUP', INDS, 1, USER_SUP(INDS), IER )
                   IF ( IER .NE. 0 ) THEN
                        CALL ERR_LOG ( 7275, IUER, 'IONO_DTEC_DB_UPDATE', 'Error in '// &
          &                 'putting "USER_SUP" lcode while processing database '//DB_NAME )
                        RETURN
                   END IF
              END IF
         END IF
 450  CONTINUE 
!
      CALL ERR_PASS   ( IUER, IER )
      CALL GVH_PLCODE ( GVH, 'DTEC_OAP', 1, 1, %REF(DTEC_OAP), IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 7276, IUER, 'IONO_DTEC_DB_UPDATE', 'Error in '// &
     &         'putting "DTEC_OAP" lcode while processing database '//DB_NAME )
           RETURN
      END IF
!
      CALL ERR_PASS   ( IUER, IER )
      CALL GVH_PLCODE ( GVH, 'DTEC_OAD', 1, ILEN(STR), %REF(DTEC_OAD), IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 7277, IUER, 'IONO_DTEC_DB_UPDATE', 'Error in '// &
     &         'putting "DTEC_OAD" lcode while processing database '//DB_NAME )
           RETURN
      END IF
!
      IF ( LEN_DATA_DTEC == 0 ) THEN
           CALL ERR_PASS ( IUER, IER )
           CALL RD_TEXT  ( ENV_FILE, M_ENV, BUF_ENV, N_ENV, IER ) 
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 7278, IUER, 'IONO_DTEC_DB_UPDATE', 'Error in '// &
     &              'reading  file '//ENV_FILE )
                RETURN
          END IF
      END IF
!
      DO 460 J6=1,LDB_FIL
         IF ( INDEX ( DB_FILES(J6), '_fr1_' ) > 0 .OR. &
     &        INDEX ( DB_FILES(J6), '_cl1_' ) > 0 .OR. &
     &        INDEX ( DB_FILES(J6), '_sl1_' ) > 0      ) THEN
!
              IF ( LEN_DATA_STS == 0 ) THEN
!
! ---------------- No prior dTEC data? We crete a new version.
!
                   IL = ILEN(DB_FILES(J6))
                   CALL CHIN ( DB_FILES(J6)(IL-6:IL-4), VERS )
                   VERS = VERS + 1
                   CALL INCH    ( VERS, DB_FILES(J6)(IL-6:IL-4) )
                   CALL CHASHR   (      DB_FILES(J6)(IL-6:IL-4) )
                   CALL BLANK_TO_ZERO ( DB_FILES(J6)(IL-6:IL-4) )
                   BUF_ENV(J6)(13:15) = DB_FILES(J6)(IL-6:IL-4)
              END IF
!
              CALL ERR_PASS ( IUER, IER )
              CALL GVH_WRITE_BGV ( GVH, J6, GVH__CRT, DB_FILES(J6), IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 7279, IUER, 'IONO_DTEC_DB_UPDATE', 'Error in '// &
     &                 'an attempt to write output database file '//DB_FILES(J6) )
                   RETURN
              END IF
         END IF
 460  CONTINUE
      IF ( LEN_DATA_STS == 0 ) THEN
!
! -------- No prior dTEC data? We create a new version of the envelope file
!
           IL = ILEN(ENV_FILE)
           CALL CHIN ( ENV_FILE(IL-6:IL-4), VERS )
           VERS = VERS + 1
           CALL INCH    ( VERS, ENV_FILE(IL-6:IL-4) )
           CALL CHASHR   (      ENV_FILE(IL-6:IL-4) )
           CALL BLANK_TO_ZERO ( ENV_FILE(IL-6:IL-4) )
!
           CALL ERR_PASS ( IUER, IER )
           CALL WR_TEXT ( N_ENV, BUF_ENV, ENV_FILE, IER ) 
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 7280, IUER, 'IONO_DTEC_DB_UPDATE', 'Error in '// &
     &              'an attemtp to write update envelope file '//ENV_FILE )
                RETURN 
          END IF
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_RELEASE ( GVH, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 7281, IUER, 'IONO_DTEC_DB_UPDATE', 'Error in '// &
     &         'attempt to release memory allocated by GVH' )
           RETURN
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  IONO_DTEC_DB_UPDATE  !#!#
