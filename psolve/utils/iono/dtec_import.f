      PROGRAM    DTEC_IMPORT
! ************************************************************************
! *                                                                      *
! *   Program DTEC_IMPORT
! *                                                                      *
! *  ### 11-FEB-2022  DTEC_IMPORT  v2.3 (c) L. Petrov  02-APR-2023  ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'solve.i'
      INCLUDE   'vcat.i'
      INCLUDE   'gvh.i'
      TYPE     ( VCAT__TYPE ) :: VCAT
      TYPE     ( GVH__STRU  ) :: GVH
      INTEGER*4  M_FIL, MDB_FIL, MIND, M_ENV
      PARAMETER  ( M_FIL   = 8192 )
      PARAMETER  ( M_ENV   =   16  )
      PARAMETER  ( MDB_FIL =   32  )
      PARAMETER  ( MIND    =   32  )
      CHARACTER  DB_NAME*16, DIR_DTEC*128, DTEC_FIL*128, BUF(MAX_OBS)*256, &
     &           DB_FILES(MDB_FIL)*128, FINAM*128, &
     &           DTEC_OAP*128, DTEC_OAD*128, DESCR*128
      INTEGER*8  DIR_DESC(16), IP8
      CHARACTER  REPO__DEF*3
      PARAMETER  ( REPO__DEF = 'OBS' ) 
      CHARACTER  VCAT_CONF_FILE*128, GVF_DB_DIR*128, REPO*3, ENV_FILE*128, &
     &           GVF_ENV_DIR*128, EXP_NAME*128, MODE_STR*128, &
     &           C_STA(MAX_ARC_STA)*8, BUF_ENV(M_ENV)*128, STR*128
      REAL*8     TEC_APR(2,MAX_OBS), DTEC_ADJ(2,MAX_OBS), DTEC_ERR(MAX_OBS), &
     &           DEL_BIAS(MAX_OBS)
      INTEGER*4  N_OBS, N_SCA, N_STA, NOBS_STA(MAX_ARC_STA), OBS_TAB(3,MAX_OBS)
      INTEGER*4  J1, J2, J3, J4, J5, J6, K6, IS, IB, IE, IL, ID, INDS, DIMS(2), &
     &           LIND, IND(2,MIND), LEV, LDB_FIL, IND_REP, REMAINED_BYTES, &
     &           NBUF, SEG_FR1, SEG_CL1, SEG_SL1, CLASS, TYP, NUM_FIELDS, &
     &           SEG_IND, LEN_REC, LEN_DATA, LEN_DATA_APR, LEN_DATA_DTEC, &
     &           LEN_DATA_STS, AUTO_SUP(MAX_OBS), USER_SUP(MAX_OBS), VERS, &
     &           N_ENV, IUER
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
      IF ( IARGC() < 2 ) THEN
           WRITE ( 6, * ) 'Usage: dtec_import  db_name dtec_dir'
           CALL EXIT ( 1 )
         ELSE 
           CALL GETARG ( 1, DB_NAME  )
           CALL GETARG ( 2, DIR_DTEC )
      END IF
!
      CALL GETENVAR  ( 'VCAT_CONF', STR )
      IF ( ILEN(STR) > 0 ) THEN
           VCAT_CONF_FILE = STR
        ELSE
           CALL GETENVAR  ( 'PSOLVE_SAVE_DIR', STR )
           IF ( ILEN(STR) == 0 ) THEN
                STR = SOLVE_SAVE_DIR
           END IF
           IF ( STR(I_LEN(STR):I_LEN(STR)) .NE. '/' ) THEN
                STR = STR(1:I_LEN(STR))//'/'
           END IF
           VCAT_CONF_FILE = STR(1:I_LEN(STR))//'vcat.conf'
      END IF
!
      IUER = -1
      CALL VCAT_GET_CONF ( VCAT_CONF_FILE, VCAT, IUER )
      IF ( IUER .NE. 0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 6201, IUER, 'DTEC_IMPORT', 'Error in '// &
     &         'parsing VCAT configuration file '//VCAT_CONF_FILE )
           CALL EXIT ( 1 )
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
           IUER = -1
           CALL ERR_LOG ( 6202, IUER, 'DTEC_IMPORT', &
     &         'Repository '//TRIM(REPO)//' specified in the '// &
     &         'environment variable VCAT_REPO is not '// &
     &         'defined in the VCAT configuration file '// &
     &         VCAT%CONF_FILE )
           CALL EXIT ( 1 )
      END IF
!
! --- Resolve database name
!
      IUER = -1
      CALL VCAT_RESOLVE_DBNAME ( VCAT, DB_NAME, REPO, ENV_FILE, MDB_FIL, &
     &                           LDB_FIL, DB_FILES, IUER )
      IF ( IUER .NE. 0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 6203, IUER, 'DTEC_IMPORT', 'Error in an '// &
     &         'attempt to resolve database name '//DB_NAME )
           CALL EXIT ( 1 )
      END IF
      ID = LINDEX ( ENV_FILE, '/' )
      GVF_ENV_DIR = ENV_FILE(1:ID-1)
      ID = LINDEX ( DB_FILES(1), '/' )
      GVF_DB_DIR  = DB_FILES(1)(1:ID-1)
!
      CALL CLRCH ( DTEC_FIL )
      LEV   = 0
      DO 420 J2=1,M_FIL
         IS = GET_FILE_FROM_DIR ( LEV, DIR_DESC, DIR_DTEC, FINAM )
         IF ( IS .NE. 0 ) THEN
              IUER = -1
              CALL ERR_LOG ( 6204, IUER, 'DTEC_IMPORT', 'Error in '// &
     &            'reading input directory '//TRIM(DIR_DTEC)// &
     &            '  '//FINAM )
              CALL EXIT ( 1 )
         END IF
         IF ( LEV == 0 ) GOTO 820 ! End of work
         IF ( LINDEX ( FINAM, '~' ) > 0 ) GOTO 420
         IF ( LINDEX ( FINAM, '#' ) > 0 ) GOTO 420
         ID = LINDEX ( FINAM, '/' )
         IF ( FINAM(ID+1:ID+10) == DB_NAME(1:10) ) THEN
              DTEC_FIL = FINAM
         END IF
 420  CONTINUE 
 820  CONTINUE 
!
      IF ( ILEN(DTEC_FIL) == 0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 6205, IUER, 'DTEC_IMPORT', 'Did not find '// &
     &         'DTEC file for database '//TRIM(DB_NAME)//' in directory '// &
     &          DIR_DTEC )
           CALL EXIT ( 1 )
      END IF
!
      IUER = -1
      CALL GVH_INIT ( GVH,  IUER ) 
      IF ( IUER .NE. 0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 6206, IUER, 'DTEC_IMPORT', 'Failure to initialize GVH' )
           CALL EXIT ( 1  )
      END IF
!
      DO 430 J3=1,LDB_FIL
         IUER = -1
!
         CALL GVH_READ_BGV ( GVH, 1, DB_FILES(J3), REMAINED_BYTES, IUER )
         IF ( IUER .NE. 0 ) THEN
              IUER = -1
              CALL ERR_LOG ( 6207, IUER, 'DTEC_IMPORT', 'Error in '// &
     &            'an atttempt to read input database file '// &
     &             DB_FILES(J3) )
              CALL EXIT ( 1 )
         END IF
         IF ( REMAINED_BYTES .NE. 0 ) THEN
              CALL CLRCH ( STR )
              CALL INCH  ( REMAINED_BYTES, STR )
              CALL ERR_LOG ( 6208, IUER, 'DTEC_IMPORT', 'The number '// &
     &            'of remaining bytes after reading input database file '// &
     &             TRIM(DB_FILES(J3))//' is not 0, but '//STR )
              CALL EXIT ( 1 ) 
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
      IUER = -1
      CALL GVH_PREGET ( GVH, IUER )
      IF ( IUER .NE. 0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 6209, IUER, 'DTEC_IMPORT', 'Error in an '// &
     &         'attempt to execute GVH_PREGET' )
           CALL EXIT ( 1 ) 
      END IF
!
      IUER = -1
      CALL GVH_GLCODE ( GVH, 'NUMB_OBS', 0, 0, 4, DIMS(1), DIMS(2), N_OBS, IUER )
      IF ( IUER .NE. 0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 6210, IUER, 'DTEC_IMPORT', 'Error in '// &
     &         'getting lcode NUMB_OBS' )
           CALL EXIT ( 1 ) 
      END IF
!
      IUER = -1
      CALL GVH_GLCODE ( GVH, 'NUMB_SCA', 0, 0, 4, DIMS(1), DIMS(2), N_SCA, IUER )
      IF ( IUER .NE. 0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 6211, IUER, 'DTEC_IMPORT', 'Error in '// &
     &         'getting lcode NUMB_SCA' )
           CALL EXIT ( 1 ) 
      END IF
!
      IUER = -1
      CALL GVH_GLCODE ( GVH, 'NUMB_STA', 0, 0, 4, DIMS(1), DIMS(2), N_STA, IUER )
      IF ( IUER .NE. 0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 6212, IUER, 'DTEC_IMPORT', 'Error in '// &
     &         'getting lcode NUMB_STA' )
           CALL EXIT ( 1 ) 
      END IF
!
      IUER = -1
      CALL GVH_GLCODE ( GVH, 'NOBS_STA', 0, 0, 4*N_STA, DIMS(1), DIMS(2), NOBS_STA, IUER )
      IF ( IUER .NE. 0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 6213, IUER, 'DTEC_IMPORT', 'Error in '// &
     &         'getting lcode NOBS_STA' )
           CALL EXIT ( 1 ) 
      END IF
!
      IUER = -1
      CALL GVH_GLCODE ( GVH, 'OBS_TAB ', 0, 0, 4*3*N_OBS, DIMS(1), DIMS(2), OBS_TAB, IUER )
      IF ( IUER .NE. 0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 6214, IUER, 'DTEC_IMPORT', 'Error in '// &
     &         'getting lcode OBS_TAB' )
           CALL EXIT ( 1 ) 
      END IF
!
      IUER = -1
      CALL GVH_GLCODE ( GVH, 'SITNAMES', 0, 0, 8*N_STA, DIMS(1), DIMS(2), %REF(C_STA), IUER )
      IF ( IUER .NE. 0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 6215, IUER, 'DTEC_IMPORT', 'Error in '// &
     &         'getting lcode SITNAMES' )
           CALL EXIT ( 1 ) 
      END IF
!
      DO 440 J4=1,N_OBS
         IUER = -1
         CALL GVH_GLCODE ( GVH, 'USER_SUP', J4, 1, 4, DIMS(1), DIMS(2), USER_SUP(J4), IUER )
         IF ( IUER .NE. 0 ) THEN
              IUER = -1
              CALL ERR_LOG ( 6216, IUER, 'DTEC_IMPORT', 'Error in '// &
     &            'getting lcode USER_SUP' )
              CALL EXIT ( 1 ) 
         END IF
!
         IUER = -1
         CALL GVH_GLCODE ( GVH, 'AUTO_SUP', J4, 1, 4, DIMS(1), DIMS(2), AUTO_SUP(J4), IUER )
         IF ( IUER .NE. 0 ) THEN
              IUER = -1
              CALL ERR_LOG ( 6217, IUER, 'DTEC_IMPORT', 'Error in '// &
     &            'getting lcode AUTO_SUP' )
              CALL EXIT ( 1 ) 
         END IF
 440  CONTINUE 
!
      IUER = 0
      CALL GVH_INQ_LCODE ( GVH, 'DTEC    ', DESCR, CLASS, TYP, DIMS, &
     &                           NUM_FIELDS, SEG_IND, LEN_REC, LEN_DATA_DTEC, &
     &                           ADR_DATA, IUER )
!
      IUER = 0
      CALL GVH_INQ_LCODE ( GVH, 'DTEC_ADJ', DESCR, CLASS, TYP, DIMS, &
     &                           NUM_FIELDS, SEG_IND, LEN_REC, LEN_DATA, &
     &                           ADR_DATA, IUER )
      IF ( LEN_DATA == 0 ) THEN
           IUER = -1
           CALL GVH_PTOC ( GVH, 'DTEC_ADJ', GVH__R8, GVH__BAS, 1, 1, &
     &         'Adjustement of the differential total electron contents, TEC units', &
     &          SEG_FR1, IUER )
           IF ( IUER .NE. 0 ) THEN
                IUER = -1
                CALL ERR_LOG ( 6218, IUER, 'DTEC_IMPORT', 'Error in '// &
     &              'defining lcode DTEC_ADJ' )
                CALL EXIT ( 1 ) 
           END IF
      END IF
!
      IUER = 0
      CALL GVH_INQ_LCODE ( GVH, 'DTEC_SIG', DESCR, CLASS, TYP, DIMS, &
     &                           NUM_FIELDS, SEG_IND, LEN_REC, LEN_DATA, &
     &                           ADR_DATA, IUER )
      IF ( LEN_DATA == 0 ) THEN
           CALL GVH_PTOC ( GVH, 'DTEC_SIG', GVH__R8, GVH__BAS, 1, 1, &
     &         'Standard deviation of dTec estimate, TEC units', &
     &          SEG_FR1, IUER )
           IF ( IUER .NE. 0 ) THEN
                IUER = -1
                CALL ERR_LOG ( 6219, IUER, 'DTEC_IMPORT', 'Error in '// &
     &              'defining lcode DTEC_SIG' )
                CALL EXIT ( 1 ) 
           END IF
      END IF
!
      IUER = 0
      CALL GVH_INQ_LCODE ( GVH, 'DTEC_APR', DESCR, CLASS, TYP, DIMS, &
     &                           NUM_FIELDS, SEG_IND, LEN_REC, LEN_DATA_APR, &
     &                           ADR_DATA, IUER )
!
      IUER = 0
      CALL GVH_INQ_LCODE ( GVH, 'TEC_APR ', DESCR, CLASS, TYP, DIMS, &
     &                           NUM_FIELDS, SEG_IND, LEN_REC, LEN_DATA, &
     &                           ADR_DATA, IUER )
      IF ( LEN_DATA == 0 ) THEN
           IUER = -1
           CALL GVH_PTOC ( GVH, 'TEC_APR ', GVH__R8, GVH__BAS, 2, 1, &
     &         'A priori total electron contents, TEC units', &
     &          SEG_CL1, IUER )
           IF ( IUER .NE. 0 ) THEN
                IUER = -1
                CALL ERR_LOG ( 6220, IUER, 'DTEC_IMPORT', 'Error in '// &
     &              'defining lcode DTEC' )
                CALL EXIT ( 1 ) 
           END IF
      END IF
!
      IUER = 0
      CALL GVH_INQ_LCODE ( GVH, 'DTEC_OAP', DESCR, CLASS, TYP, DIMS, &
     &                           NUM_FIELDS, SEG_IND, LEN_REC, LEN_DATA, &
     &                           ADR_DATA, IUER )
      IF ( LEN_DATA == 0 ) THEN
           IUER = -1
           CALL GVH_PTOC ( GVH, 'DTEC_OAP', GVH__C1, GVH__SES, LEN(DTEC_OAP), &
     &          1, 'Origin of the a priori dTEC', SEG_CL1, IUER )
           IF ( IUER .NE. 0 ) THEN
                IUER = -1
                CALL ERR_LOG ( 6221, IUER, 'DTEC_IMPORT', 'Error in '// &
     &              'defining lcode DTEC_OAP' )
                CALL EXIT ( 1 ) 
           END IF
      END IF
!
      IUER = 0
      CALL GVH_INQ_LCODE ( GVH, 'DTEC_OAD', DESCR, CLASS, TYP, DIMS, &
     &                           NUM_FIELDS, SEG_IND, LEN_REC, LEN_DATA, &
     &                           ADR_DATA, IUER )
      IF ( LEN_DATA == 0 ) THEN
           IUER = -1
           CALL GVH_PTOC ( GVH, 'DTEC_OAD', GVH__C1, GVH__SES, LEN(DTEC_OAD), 1, &
     &         'Software that generated adjustments to dTEC', SEG_CL1, IUER )
           IF ( IUER .NE. 0 ) THEN
                IUER = -1
                CALL ERR_LOG ( 6222, IUER, 'DTEC_IMPORT', 'Error in '// &
     &              'defining lcode DTEC_OAD' )
                CALL EXIT ( 1 ) 
           END IF
      END IF
!
      IUER = 0
      CALL GVH_INQ_LCODE ( GVH, 'DEL_BIAS', DESCR, CLASS, TYP, DIMS, &
     &                           NUM_FIELDS, SEG_IND, LEN_REC, LEN_DATA, &
     &                           ADR_DATA, IUER )
      IF ( LEN_DATA == 0 ) THEN
           IUER = -1
           CALL GVH_PTOC ( GVH, 'DEL_BIAS', GVH__R8, GVH__BAS, 1, 1, &
     &         'Delay bias of the upper band wrt the low band in sec', &
     &          SEG_CL1, IUER )
           IF ( IUER .NE. 0 ) THEN
                IUER = -1
                CALL ERR_LOG ( 6223, IUER, 'DTEC_IMPORT', 'Error in '// &
     &              'defining lcode DEL_BIAS' )
                CALL EXIT ( 1 ) 
           END IF
      END IF
!
      IUER = 0
      CALL GVH_INQ_LCODE ( GVH, 'DBDT_STS', DESCR, CLASS, TYP, DIMS, &
     &                           NUM_FIELDS, SEG_IND, LEN_REC, LEN_DATA_STS, &
     &                           ADR_DATA, IUER )
      IF ( LEN_DATA_STS == 0 ) THEN
           IUER = -1
           CALL GVH_PTOC ( GVH, 'DBDT_STS', GVH__I2, GVH__BAS, 1, 1, &
     &         'Status of dual-band observations for dTEC adjustment', &
     &          SEG_CL1, IUER )
           IF ( IUER .NE. 0 ) THEN
                IUER = -1
                CALL ERR_LOG ( 6224, IUER, 'DTEC_IMPORT', 'Error in '// &
     &              'defining lcode DBDT_STS' )
                CALL EXIT ( 1 ) 
           END IF
      END IF
!
! --- Remove obsolete data
!
      IF ( LEN_DATA_DTEC > 0 ) THEN
           IUER = -1
           CALL GVH_DTOC ( GVH, 'DTEC    ', SEG_FR1, IUER )
           IF ( IUER .NE. 0 ) THEN
                IUER = -1
                CALL ERR_LOG ( 6225, IUER, 'DTEC_IMPORT', 'Error in '// &
     &              'deleting lcode DTEC' )
                CALL EXIT ( 1 ) 
           END IF
      END IF
      IF ( LEN_DATA_APR > 0 ) THEN
           IUER = -1
           CALL GVH_DTOC ( GVH, 'DTEC_APR', SEG_CL1, IUER )
           IF ( IUER .NE. 0 ) THEN
                IUER = -1
                CALL ERR_LOG ( 6226, IUER, 'DTEC_IMPORT', 'Error in '// &
     &              'deleting lcode DTEC_APR' )
                CALL EXIT ( 1 ) 
           END IF
      END IF
!
      IUER = -1
      CALL GVH_PREPUT ( GVH, N_OBS, N_SCA, N_STA, NOBS_STA, C_STA, OBS_TAB, IUER )
      IF ( IUER .NE. 0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 6227, IUER, 'DTEC_IMPORT', 'Error in an attempt to '// &
     &                   'insert mandatory lcodes an initialize cache' )
           CALL EXIT ( 1 ) 
      END IF
!
      IUER = -1
      CALL RD_TEXT ( DTEC_FIL, MAX_OBS, BUF, NBUF, IUER )
      IF ( IUER .NE. 0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 6228, IUER, 'DTEC_IMPORT', 'Error in an attempt to '// &
     &                   'read dtec file '//DTEC_FIL )
           CALL EXIT ( 1 ) 
      END IF
!
      TEC_APR = 0.0D0
      DTEC_ADJ = 0.0D0
      DTEC_ERR = 0.0D0
      DBDT_STS = 0
      DO 450 J5=1,NBUF
         CALL EXWORD ( BUF(J5), MIND, LIND, IND, CHAR(0)//CHAR(32)//CHAR(9), IUER )
         IF ( LIND .GE. 3 ) THEN
              IF ( BUF(J5)(IND(1,2):IND(2,2)) == 'Experiment:' ) THEN
                   IF ( BUF(J5)(IND(1,3):IND(2,3)) .NE. DB_NAME ) THEN
                        IUER = -1
                        CALL ERR_LOG ( 6229, IUER, 'DTEC_IMPORT', 'Database name '// &
     &                      'mismatch: requested db_name: '//TRIM(DB_NAME)// &
     &                      ' the database name in DTEC file '//TRIM(DTEC_FIL)// &
     &                      ' : '//BUF(J5)(IND(1,3):IND(2,3)) )
                        CALL EXIT ( 1 ) 
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
              IF ( BUF(J5)(IND(1,10):IND(2,10)) == '********' ) THEN
                   BUF(J5)(IND(1,10):IND(2,10)) = '9999.999'
              END IF
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
              IUER = -1
              CALL GVH_PLCODE ( GVH, 'DTEC_ADJ', INDS, 1, DTEC_ADJ(2,INDS) - DTEC_ADJ(1,INDS), IUER )
              IF ( IUER .NE. 0 ) THEN
                   IUER = -1
                   CALL ERR_LOG ( 6230, IUER, 'DTEC_IMPORT', 'Error in '// &
     &                 'putting "DTEC    " lcode while processing database '//DB_NAME )
                   CALL EXIT ( 1 ) 
              END IF
!
              IUER = -1
              CALL GVH_PLCODE ( GVH, 'TEC_APR ', INDS, 1, TEC_APR(1,INDS), IUER )
              IF ( IUER .NE. 0 ) THEN
                   IUER = -1
                   CALL ERR_LOG ( 6231, IUER, 'DTEC_IMPORT', 'Error in '// &
     &                 'putting "TEC_APR" lcode while processing database '//DB_NAME )
                   CALL EXIT ( 1 ) 
              END IF
!
              IUER = -1
              CALL GVH_PLCODE ( GVH, 'DTEC_SIG', INDS, 1, DTEC_ERR(INDS), IUER )
              IF ( IUER .NE. 0 ) THEN
                   IUER = -1
                   CALL ERR_LOG ( 6232, IUER, 'DTEC_IMPORT', 'Error in '// &
     &                 'putting "DTEC_ERR" lcode while processing database '//DB_NAME )
                   CALL EXIT ( 1 ) 
              END IF
!
              IUER = -1
              CALL GVH_PLCODE ( GVH, 'DEL_BIAS', INDS, 1, DEL_BIAS(INDS), IUER )
              IF ( IUER .NE. 0 ) THEN
                   IUER = -1
                   CALL ERR_LOG ( 6233, IUER, 'DTEC_IMPORT', 'Error in '// &
     &                 'putting "DEL_BIAS" lcode while processing database '//DB_NAME )
                   CALL EXIT ( 1 ) 
              END IF
!
              IUER = -1
              CALL GVH_PLCODE ( GVH, 'DBDT_STS', INDS, 1, DBDT_STS(INDS), IUER )
              IF ( IUER .NE. 0 ) THEN
                   IUER = -1
                   CALL ERR_LOG ( 6234, IUER, 'DTEC_IMPORT', 'Error in '// &
     &                 'putting "DBDT_STS" lcode while processing database '//DB_NAME )
                   CALL EXIT ( 1 ) 
              END IF
!
!!              IF ( LEN_DATA_STS == 0 ) THEN
              IF ( LEN_DATA_STS .NE. -777 ) THEN
!
! ---------------- Perform initialization of suppression status for fused data type
!
                   IF ( BTEST ( DBDT_STS(INDS), DTH__STS ) .OR. BTEST ( DBDT_STS(INDS), DTL__STS ) ) THEN
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
                   IUER = -1
                   CALL GVH_PLCODE ( GVH, 'AUTO_SUP', INDS, 1, AUTO_SUP(INDS), IUER )
                   IF ( IUER .NE. 0 ) THEN
                        IUER = -1
                        CALL ERR_LOG ( 6235, IUER, 'DTEC_IMPORT', 'Error in '// &
          &                 'putting "AUTO_SUP" lcode while processing database '//DB_NAME )
                        CALL EXIT ( 1 ) 
                   END IF
!
                   IUER = -1
                   CALL GVH_PLCODE ( GVH, 'USER_SUP', INDS, 1, USER_SUP(INDS), IUER )
                   IF ( IUER .NE. 0 ) THEN
                        IUER = -1
                        CALL ERR_LOG ( 6236, IUER, 'DTEC_IMPORT', 'Error in '// &
          &                 'putting "USER_SUP" lcode while processing database '//DB_NAME )
                        CALL EXIT ( 1 ) 
                   END IF
              END IF
         END IF
 450  CONTINUE 
!
      IUER = -1
      CALL GVH_PLCODE ( GVH, 'DTEC_OAP', 1, 1, %REF(DTEC_OAP), IUER )
      IF ( IUER .NE. 0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 6237, IUER, 'DTEC_IMPORT', 'Error in '// &
     &         'putting "DTEC_OAP" lcode while processing database '//DB_NAME )
           CALL EXIT ( 1 ) 
      END IF
!
      IUER = -1
      CALL GVH_PLCODE ( GVH, 'DTEC_OAD', 1, ILEN(STR), %REF(DTEC_OAD), IUER )
      IF ( IUER .NE. 0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 6238, IUER, 'DTEC_IMPORT', 'Error in '// &
     &         'putting "DTEC_OAD" lcode while processing database '//DB_NAME )
           CALL EXIT ( 1 ) 
      END IF
!
      IF ( LEN_DATA_DTEC == 0 ) THEN
           CALL RD_TEXT ( ENV_FILE, M_ENV, BUF_ENV, N_ENV, IUER ) 
           IF ( IUER .NE. 0 ) THEN
                IUER = -1
                CALL ERR_LOG ( 6239, IUER, 'DTEC_IMPORT', 'Error in '// &
     &              'reading  file '//ENV_FILE )
               CALL EXIT ( 1 ) 
          END IF
      END IF
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
              IUER = -1
              CALL GVH_WRITE_BGV ( GVH, J6, GVH__CRT, DB_FILES(J6), IUER )
              IF ( IUER .NE. 0 ) THEN
                   IUER = -1
                   CALL ERR_LOG ( 6240, IUER, 'DTEC_IMPORT', 'Error in '// &
     &                 'an attempt to write output database file '//DB_FILES(J6) )
                   CALL EXIT ( 1 ) 
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
           CALL WR_TEXT ( N_ENV, BUF_ENV, ENV_FILE, IUER ) 
           IF ( IUER .NE. 0 ) THEN
                IUER = -1
                CALL ERR_LOG ( 6241, IUER, 'DTEC_IMPORT', 'Error in '// &
     &              'an attemtp to write update envelope file '//ENV_FILE )
               CALL EXIT ( 1 ) 
          END IF
      END IF
!
      IUER = -1
      CALL GVH_RELEASE ( GVH, IUER )
      IF ( IUER .NE. 0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 6242, IUER, 'DTEC_IMPORT', 'Error in '// &
     &         'attempt to release memory allocated by GVH' )
           CALL EXIT ( 1 )
      END IF
!
      END  PROGRAM   DTEC_IMPORT  !#!  
