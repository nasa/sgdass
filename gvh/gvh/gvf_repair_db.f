      PROGRAM    GVF_REPAIR_DB
! ************************************************************************
! *                                                                      *
! *   Program  GVF_REPAIR_DB  checks VGOSDA files and if requested,      *
! *   repair them.                                                       *
! *                                                                      *
! *                                                                      *
! *   Usage: gvf_repair_db fil_sess operation [out_dir]                  *
! *                                                                      *
! *          where fil_sess either vgosa-file of envelop of the gvf      *
! *          database file and out_dir name of the directory where       *
! *          output file in vgosda format is written.                    *       
! *                                                                      *
! *          Supported  operations:                                      *
! *                                                                      *
! *          check     -- run general checks of the database contents    *
! *          check_len -- check lengths of vgodsa sectinos.              *
! *          fix       -- fix various inconsitencies of the database file*
! *          aplen     -- adds lcodes APLENGTH and NUM_SPCH if they      *
! *                       missed.                                        *
! *                                                                      *
! * ### 25-OCT-2019   GVF_REPAIR_DB   v2.1 (c) L. Petrov 23-MAR-2024 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'vcat.i'
      INTEGER*4  MS, MV, ME, MIND, LL
      PARAMETER  ( MS = 512*1024 )
      PARAMETER  ( MV = 20*1024*1024 )
      PARAMETER  ( ME = 32 )
      PARAMETER  ( MIND = 256 )
      PARAMETER  ( LL = 384 )
      TYPE     ( VCAT__TYPE ) :: VCAT
      CHARACTER  FIL_SESS*128, FIL_ENV*128, FIL_VDA*128, DIR_VDA_OUT*128
      CHARACTER  BUF_SESS(MS)*128, BUF_ENV(ME)*128
      CHARACTER, ALLOCATABLE :: BUF_VDA(:)*(LL)
      CHARACTER  FILNAM*128, SESS_NAME*10, SUFFIX*1, EXP_ENV*8, EXP_DB*8, &
     &           BAND_NAM(2)*1, FILOUT*128
      CHARACTER  VCAT_CONF_FILE*128, GVF_DB_DIR*128, GVF_ENV_DIR*128, VTD_CONF_SES*128, &
     &           VCAT_REPO_NAME*128, FIL_AUX*128, EXP_OLD*16, EXP_NEW*16, ENV_FILE*128
      CHARACTER  TEMP_FILE*128, STR*128, COM*256, EXP_STATUS*9, NUS_STATUS*8, &
     &           NUB_STATUS*10, NUD_STATUS*9, QC_STATUS*7, DB_VERS_STR*3, NS2_STATUS*8, &
     &           GEN_STATUS*11, CLBR_STATUS*32
      INTEGER*8  DIR_DESC(16), IP8 
      INTEGER*4  NS, NE, NV, LEV, LS, IL, IS, IP, IB, IE, LIND, IND(2,MIND), &
     &           ID, J1, J2, J3, J4, J5, J6, J7, IND_BAND, NUM_BAND, IND_BND_STR, &
     &           KOBS_NUSEDCHN(2), KMAX_NUSEDCHN(2), NUMB_OBS, IND_OBS, &
     &           IND_B2, IND_TO1, IND_TO4, IND_DA1, IND_DA2, IND_DA4, NUM_REC, &
     &           IND_DATYP_TOCS, IND_DATYP_DATA, IND_DBVERS, DB_VERS, IVAL, &
     &           QC_MODE, IND_N2, IND_PC, IND_GEN, FIX_AP, NR, L_FIL, IND_REP, &
     &           IND_DELRESID_TOCS, IND_DELRESID_DATA, NUM_CLBR, IUER
      REAL*8     REF_FREQ(2), VAL
      LOGICAL*1  FL_VDA, FL_SHORT_LINE, FL_NOVALUE, FL_FIX, FL_AP, FL_CHECK_LEN, &
     &           FL_EXP_NAME, LEX, FL_NUM_CLBR, FL_MJD_CLBR, FL_UTC_CLBR
      INTEGER*8, EXTERNAL :: GET_FILE_FROM_DIR
      INTEGER*4, EXTERNAL :: OPENDIR, CLOSEDIR, ILEN, I_LEN, GETPID, LINDEX, SYSTEM, &
     &                       FIX_EXP_NAME, LTM_DIF
!
      FIL_AUX = '/vlbi/misc/aux_info.txt'
      VCAT%STATUS = VCAT__UNDF
!
      FL_FIX = .FALSE.
      FL_AP  = .FALSE.
      FL_CHECK_LEN = .FALSE.
      FL_EXP_NAME  = .FALSE.
      CALL CLRCH ( SESS_NAME )
      CALL CLRCH ( EXP_ENV   )
      IF ( IARGC() < 2 ) THEN
           WRITE ( 6, * ) 'Usage: gvf_repair_db fil_sess check/fix/exp_name [dir_vda_out]'
           CALL EXIT ( 1 )
         ELSE
           CALL GETARG ( 1, FIL_SESS    )
           CALL GETARG ( 2, STR         )
           CALL TRAN ( 12, STR, STR     )
           IF ( STR == 'check_len' ) THEN
                FL_CHECK_LEN = .TRUE.
              ELSE IF ( STR == 'check' ) THEN
                CONTINUE 
              ELSE IF ( STR == 'aplen' .OR. STR == 'fix' .OR. STR(1:8) == 'exp_name' ) THEN
                IF ( STR == 'fix'           ) FL_FIX = .TRUE.
                IF ( STR == 'aplen'         ) FL_AP  = .TRUE.
                IF ( STR(1:8) == 'exp_name' ) THEN
                     FL_EXP_NAME = .TRUE. 
                     IB =  INDEX ( STR(10:), '@' ) + 9
                     IE = LINDEX ( STR,     '@' )
                     IF ( STR(9:9) .NE. '@' ) THEN
                          WRITE ( 6, '(A)' ) 'Wrong format of exp_name substitution: '// &
     &                                       'the 9th character of '//TRIM(STR)//' should be @' 
                          CALL EXIT ( 1 )
                     END IF
                     IF ( IB < 10 ) THEN
                          WRITE ( 6, '(A)' ) 'Wrong format of exp_name substitution: '// &
     &                                       TRIM(STR)//' should have three @' 
                          CALL EXIT ( 1 )
                     END IF
                     IF ( IE < 10 ) THEN
                          WRITE ( 6, '(A)' ) 'Wrong format of exp_name substitution: '// &
     &                                       TRIM(STR)//' should have three @' 
                          CALL EXIT ( 1 )
                     END IF
                     EXP_OLD = STR(10:IB-1)
                     EXP_NEW = STR(IB+1:IE-1)
                     FL_FIX  = .TRUE.
                END IF
!
                IF ( IARGC() < 3 ) THEN
                     WRITE ( 6, '(A)' ) 'The third argument is required in the fix mode' 
                     CALL EXIT ( 1 ) 
                   ELSE
                     CALL GETARG ( 3, DIR_VDA_OUT )
                     IF ( DIR_VDA_OUT(ILEN(DIR_VDA_OUT):ILEN(DIR_VDA_OUT)) == '/' ) THEN
                          DIR_VDA_OUT(ILEN(DIR_VDA_OUT):ILEN(DIR_VDA_OUT)) = ' '
                     END IF
!
! ------------------ Check whether the outut directory exists
!
                     DIR_DESC(1) = OPENDIR ( TRIM(DIR_VDA_OUT)//CHAR(0) ) 
                     IF ( DIR_DESC(1) == -1 ) THEN
                          WRITE ( 6, '(A)' ) 'Cannot open directory '//TRIM(DIR_VDA_OUT)
                          CALL EXIT ( 1 ) 
                     END IF
                     IP = CLOSEDIR ( %VAL(DIR_DESC(1)) )
                END IF
           END IF
      END IF
!
      ID = LINDEX ( FIL_SESS, '/' )
      IL = ILEN(FIL_SESS)
      IF ( ID == 0 ) THEN
           CALL CLRCH    ( VCAT_REPO_NAME )
           CALL GETENVAR ( 'VCAT_CONF', VCAT_CONF_FILE )
           CALL GETENVAR ( 'VCAT_REPO', VCAT_REPO_NAME )
           IF ( ILEN(VCAT_CONF_FILE) > 0 ) THEN
                IUER = -1
                CALL VCAT_GET_CONF ( VCAT_CONF_FILE, VCAT, IUER )
                IF ( IUER .NE. 0 ) THEN
                     CALL EXIT ( 1 ) 
                END IF
                IF ( FIL_SESS(IL-3:IL) .NE. '.vda' ) THEN
                     CALL VCAT_RESOLVE_DBNAME ( VCAT, FIL_SESS, VCAT_REPO_NAME, &
     &                                          ENV_FILE, ME, L_FIL, BUF_ENV, IUER )
                     FIL_SESS = ENV_FILE
                     ID = LINDEX ( FIL_SESS, '/' )
                END IF
           END IF
      END IF
!
! --- Generate the name of the output file
!
      FILOUT = TRIM(DIR_VDA_OUT)//'/'//FIL_SESS(ID+1:ID+10)//'.vda'
!
      WRITE ( UNIT=STR, FMT='(I8.8)' ) GETPID()
!
! --- End temporary file
!
      TEMP_FILE = '/dev/shm/fix_db__'//STR(1:8)//'.vda'
!
      ALLOCATE (  BUF_VDA(MV) )
!
      IL = ILEN(FIL_SESS)
      IF ( FIL_SESS(IL-3:IL) .EQ. '.vda' ) THEN
           FL_VDA = .TRUE.
           TEMP_FILE = FIL_SESS
         ELSE
           IF ( VCAT%STATUS == VCAT__UNDF ) THEN
                CALL GETENVAR ( 'VCAT_CONF', VCAT_CONF_FILE )
                IUER = -1
                CALL VCAT_GET_CONF ( VCAT_CONF_FILE, VCAT, IUER )
                IF ( IUER .NE. 0 ) THEN
                     CALL EXIT ( 1 ) 
                END IF
           ENDIF
           CALL CLRCH    ( VCAT_REPO_NAME )
           CALL GETENVAR ( 'VCAT_REPO', VCAT_REPO_NAME )
           IF ( ILEN(VCAT_REPO_NAME) == 0 ) THEN
                VCAT_REPO_NAME = VCAT%GVF_REP_NAME(1)
           END IF
           IND_REP = LTM_DIF ( 0, VCAT%NREPS, VCAT%GVF_REP_NAME, VCAT_REPO_NAME )
           IF ( IND_REP < 1 ) THEN
                WRITE ( 6, '(A)' ) 'Undefined VCAT repository '//TRIM(VCAT_REPO_NAME)// &
     &                             ' -- please check environment variable VCAT_REPO_NAME'
                CALL EXIT ( 1 ) 
           END IF
           GVF_ENV_DIR = VCAT%GVF_ENV_DIR(IND_REP)
           GVF_DB_DIR  = VCAT%GVF_DB_DIR(IND_REP)
!
! -------- The input is the envelop file
!
           FL_VDA = .FALSE.
           IF ( FIL_SESS(IL-3:IL) .NE. '.env' ) THEN
                IF ( FIL_SESS(IL-4:IL-3) .NE. '_v' ) THEN
                     FIL_SESS = TRIM(FIL_SESS)//'_v003'
                END IF
                FIL_SESS = TRIM(FIL_SESS)//'.env'
           END IF
!
           IL = ILEN(FIL_SESS)
           INQUIRE  ( FILE=FIL_SESS, EXIST=LEX )
           IF ( .NOT. LEX ) THEN
                FIL_SESS(IL-4:IL-4) = '2'
                INQUIRE  ( FILE=FIL_SESS, EXIST=LEX )
                IF ( .NOT. LEX ) THEN
                     FIL_SESS(IL-4:IL-4) = '1'
                     INQUIRE  ( FILE=SESS_NAME, EXIST=LEX )
                     IF ( .NOT. LEX ) THEN
                          WRITE ( 6, '(A)' ) 'Cannot find envelope file '//TRIM(FIL_SESS)
                     END IF
                END IF
           END IF
!
           IL = ILEN(FIL_SESS)
           SESS_NAME = FIL_SESS(IL-18:IL-9)
!
! -------- Read the envelop file
!
           IUER = -1
           CALL RD_TEXT ( FIL_SESS, ME, BUF_ENV, NE, IUER )
           IF ( IUER .NE. 0 ) CALL EXIT ( 1 )
           EXP_ENV = BUF_ENV(1)(23:30)
!
! -------- Build a command for transforming the VLBI data from from GVF to VDA format
!
           COM = 'gvf_transform -to_ascii '//TRIM(FIL_SESS)//' '//TEMP_FILE
           IS = SYSTEM ( TRIM(COM)//CHAR(0) )
           IF ( IS .NE. 0 ) THEN
                 CALL UNLINK ( TRIM(TEMP_FILE)//CHAR(0) )
                WRITE ( 6, '(A)' ) 'Failure in tranforming to vda for '//TRIM(FIL_SESS)
                CALL EXIT ( 1 )
           END IF
      END IF
!
! --- Read the VGODSA file
!
      IUER = -1
      CALL RD_TEXT ( TEMP_FILE, MV, BUF_VDA, NV, IUER )
      IF ( NV .GE. MV ) THEN
          WRITE ( 6, '(A,2X,A)' ) 'CHECK '//TRIM(FIL_SESS), 'SHORT BUFFER'
          CALL UNLINK ( TRIM(TEMP_FILE)//CHAR(0) )
          IF ( IUER .NE. 0 ) CALL EXIT ( 1 )
      END IF
      IF ( FL_CHECK_LEN ) THEN
!
! -------- Run check of section lengths 
!
           IUER = -1
           CALL CHECK_LEN ( NV, BUF_VDA, FIL_SESS, IUER )
           IF ( IUER .NE. 0 ) THEN
                CALL EXIT ( 1 )
           END IF
!
! -------- ... and exit
!
           CALL EXIT ( 0 )
      END IF
      IF ( FL_AP ) THEN
!
! -------- Fix VGOSDA file by adding missed APLEN and NUM_CHSP lcode
!
           IUER = -1
           IP = FIX_AP ( NV, BUF_VDA, FIL_AUX, IUER )
           IF ( IUER .NE. 0 ) THEN
                WRITE ( 6, '(A)' ) 'GV_REPAIR_DB: failure in processing '//TRIM(FIL_SESS)
                CALL EXIT ( 1 )
           END IF
           IF ( IP == 1 ) THEN
!
! ------------- Write updated vgosda database file
!
                IUER = -1
                CALL WR_TEXT ( NV, BUF_VDA, FILOUT, IUER )
                IF ( IUER .NE. 0 ) CALL EXIT ( 1 )
                WRITE ( 6, '(A)' ) 'FIX_AP -- Output file: '//TRIM(FILOUT)
!
! ------------- ... and exit
! 
                CALL EXIT ( 0 ) 
              ELSE
                WRITE ( 6, '(A)' ) 'FIX_AP -- nothing to do for '//TRIM(FIL_SESS)
                CALL EXIT ( 0 ) 
           END IF
      END IF
      IF ( FL_EXP_NAME ) THEN
           IUER = -1
           NR = FIX_EXP_NAME ( NV, BUF_VDA, FIL_AUX, EXP_OLD(1:ILEN(EXP_OLD)), &
     &                                               EXP_NEW(1:ILEN(EXP_NEW)), IUER )
           IF ( IUER .NE. 0 ) THEN
                WRITE ( 6, '(A)' ) 'GV_REPAIR_DB: Failura in processing '//TRIM(FIL_SESS)
                CALL EXIT ( 1 )
           END IF
           WRITE ( 6, '(A,I6,A)' ) 'GV_REPAIR_DB: ', NR, &
     &                             ' exp_name subsitutions were made for '//TRIM(FIL_SESS)
      END IF
!
! --- Run extended check
!
      FL_SHORT_LINE = .FALSE.
      FL_NOVALUE    = .FALSE.
      FL_NUM_CLBR   = .FALSE.
      FL_MJD_CLBR   = .FALSE.
      FL_UTC_CLBR   = .FALSE.
      NUM_CLBR      = 0
      EXP_DB = '????????'
      NUM_BAND = 0
      REF_FREQ = -1.D9
      IND_BND_STR = -1
      KOBS_NUSEDCHN = 0
      KMAX_NUSEDCHN = 0
      NUMB_OBS = 0
      IND_B2   = 0
      IND_DATYP_TOCS  = 0
      IND_DATYP_DATA  = 0
      IND_DELRESID_TOCS = 0
      IND_DELRESID_DATA = 0
      IND_DBVERS = 0
      DB_VERS = 1
      IND_N2 = 0
      IND_PC = 0
      IND_DA2 = 0
      IND_DA4 = 0
      IND_GEN = 0
      GEN_STATUS = 'gen_undef'
      DO 420 J2=1,NV
         IF ( BUF_VDA(J2)(LL:LL) .NE. ' ' ) FL_SHORT_LINE = .TRUE.
         IF ( BUF_VDA(J2)(1:17) == 'PREA.1 GENERATOR:' ) THEN
!
! ----------- Check the generator
!
              IND_GEN = J2
              CALL EXWORD ( BUF_VDA(J2), MIND, LIND, IND, CHAR(32), IUER )
              IF ( BUF_VDA(J2)(IND(1,3):IND(1,3)+6) == 'nuSolve'          .OR. &
     &             BUF_VDA(J2)(IND(1,3):IND(2,3)) == 'PIMA'               .OR. &
     &             BUF_VDA(J2)(IND(1,3):IND(2,3)) == 'mark3_to_gvf'       .OR. &
     &             BUF_VDA(J2)(IND(1,3):IND(2,3)) == 'mark3_to_gvh_proto' .OR. &
     &             BUF_VDA(J2)(IND(1,3):IND(2,3)) == 'gvf_transform'           ) THEN
                   GEN_STATUS = 'gen_ok'
                 ELSE
                   GEN_STATUS = 'gen_unknown: '//BUF_VDA(J2)(IND(1,3):IND(2,3))
              END IF
            ELSE IF ( BUF_VDA(J2)(1:6) == 'FILE.1' .AND. BUF_VDA(J2)(1:23) == 'FILE.1 @section_length:' ) THEN
              IL = ILEN(EXP_ENV) 
              IF ( IL == 0 ) THEN
!
! ---------------- We do not have exeriment name. Let us extrtact it from the 
! ---------------- name of the first file
!
                   IP = 1
                   DO 410 J1=IL-13,1,-1
                      IF ( BUF_VDA(J2)(J1:J1) == '_' ) THEN
                           IP = J1 + 1
                           GOTO 810
                      END IF
 410               CONTINUE 
 810               CONTINUE 
                   EXP_ENV = BUF_VDA(J2)(IP:IL-13)
              END IF
            ELSE IF ( BUF_VDA(J2)(1:45) == 'TOCS.1 BAND_NAM   SES  C1   2   1  Band names' ) THEN
              IND_B2 = J2
            ELSE IF ( BUF_VDA(J2)(1:23) == 'TOCS.1 @section_length:' ) THEN
              IND_TO1 = J2 
            ELSE IF ( BUF_VDA(J2)(1:33) == 'TOCS.1 QUALCODE   BAS  C1   1   1' ) THEN
              QC_MODE = 11
            ELSE IF ( BUF_VDA(J2)(1:33) == 'TOCS.1 QUALCODE   BAS  C1   1   2' ) THEN
              QC_MODE = 12
            ELSE IF ( BUF_VDA(J2)(1:33) == 'TOCS.1 QUALCODE   BAS  C1   2   1' ) THEN
              QC_MODE = 21
            ELSE IF ( BUF_VDA(J2)(1:33) == 'TOCS.1 QUALCODE   BAS  C1   2   2' ) THEN
              QC_MODE = 22
            ELSE IF ( BUF_VDA(J2)(1:15) == 'TOCS.1 PIMA_CNT' )   THEN
              IND_PC  = J2
            ELSE IF ( BUF_VDA(J2)(1:23) == 'DATA.1 @section_length:' ) THEN
              IND_DA1 = J2 
            ELSE IF ( BUF_VDA(J2)(1:15) == 'DATA.1 EXP_CODE' ) THEN
              CALL EXWORD ( BUF_VDA(J2), MIND, LIND, IND, CHAR(32), IUER )
              EXP_DB = BUF_VDA(J2)(IND(1,7):IND(2,7))
              IF ( EXP_DB .NE. EXP_ENV .AND. ILEN(EXP_ENV) > 0 ) THEN
                   CALL CLRCH ( BUF_VDA(J2)(IND(1,7):) )
                   BUF_VDA(J2)(IND(1,7):) = EXP_ENV
              END IF
            ELSE IF ( BUF_VDA(J2)(1:15) == 'DATA.1 NUM_CLBR' ) THEN
              FL_NUM_CLBR = .TRUE.
              CALL EXWORD ( BUF_VDA(J2), MIND, LIND, IND, CHAR(32), IUER )
              CALL CHIN ( BUF_VDA(J2)(IND(1,7):IND(2,7)), NUM_CLBR ) 
            ELSE IF ( BUF_VDA(J2)(1:15) == 'DATA.4 NUM_CLBR' ) THEN
              FL_NUM_CLBR = .TRUE.
              CALL EXWORD ( BUF_VDA(J2), MIND, LIND, IND, CHAR(32), IUER )
              CALL CHIN ( BUF_VDA(J2)(IND(1,7):IND(2,7)), NUM_CLBR ) 
            ELSE IF ( BUF_VDA(J2)(1:15) == 'DATA.1 MJD_CLBR' ) THEN
              FL_MJD_CLBR = .TRUE.
            ELSE IF ( BUF_VDA(J2)(1:15) == 'DATA.4 MJD_CLBR' ) THEN
              FL_MJD_CLBR = .TRUE.
            ELSE IF ( BUF_VDA(J2)(1:15) == 'DATA.1 UTC_CLBR' ) THEN
              FL_UTC_CLBR = .TRUE.
            ELSE IF ( BUF_VDA(J2)(1:15) == 'DATA.4 UTC_CLBR' ) THEN
              FL_UTC_CLBR = .TRUE.
            ELSE IF ( BUF_VDA(J2)(1:6) == 'DATA.1' ) THEN
              CALL EXWORD ( BUF_VDA(J2), MIND, LIND, IND, CHAR(32), IUER )
              IF ( BUF_VDA(J2)(1:15) == 'DATA.1 NUM_BAND' ) THEN
                   CALL CHIN ( BUF_VDA(J2)(IND(1,7):IND(2,7)), NUM_BAND )
              END IF
              IF ( BUF_VDA(J2)(1:15) == 'DATA.1 NUMB_OBS' ) THEN
                   CALL CHIN ( BUF_VDA(J2)(IND(1,7):IND(2,7)), NUMB_OBS )
              END IF
              IF ( BUF_VDA(J2)(1:15) == 'DATA.1 DELRESID' ) THEN
                   IF ( IND_DELRESID_DATA == 0 ) THEN
                        IND_DELRESID_DATA = J2 
                   END IF
              END IF
!
              CALL EXWORD ( BUF_VDA(J2), MIND, LIND, IND, CHAR(32), IUER )
              IF ( LIND < 7 .AND. INDEX ( BUF_VDA(J2), '@section_length:' ) < 1 ) THEN
!
! ---------------- Check records with missed values
!
                   IF ( .NOT. FL_FIX ) THEN
                        WRITE ( 6, '(A,2X,A,2X,A)' ) 'NOVALUE '//TRIM(FIL_SESS), ' || ', TRIM(BUF_VDA(J2))
                   END IF
                   FL_NOVALUE = .TRUE.
                   IF ( BUF_VDA(J2)(IND(1,2):IND(2,2)) == 'BAND_NAM' ) THEN
                        BUF_VDA(J2) = TRIM(BUF_VDA(J2))//' X'
                        IND_BND_STR = J2
                        NUB_STATUS = 'nub_failed'
                     ELSE IF ( BUF_VDA(J2)(IND(1,2):IND(2,2)) == 'CORPLACE' ) THEN
                        BUF_VDA(J2) = TRIM(BUF_VDA(J2))//' unknown'
                     ELSE IF ( BUF_VDA(J2)(IND(1,2):IND(2,2)) == 'COR_VERS' ) THEN
                        BUF_VDA(J2) = TRIM(BUF_VDA(J2))//' unknown'
                     ELSE IF ( BUF_VDA(J2)(IND(1,2):IND(2,2)) == 'COR_TYPE' ) THEN
                        BUF_VDA(J2) = TRIM(BUF_VDA(J2))//' unknown'
                     ELSE IF ( BUF_VDA(J2)(IND(1,2):IND(2,2)) == 'EXP_CODE' ) THEN
                        BUF_VDA(J2) = TRIM(BUF_VDA(J2))//' unknown'
                     ELSE IF ( BUF_VDA(J2)(IND(1,2):IND(2,2)) == 'EXP_DESC' ) THEN
                        BUF_VDA(J2) = TRIM(BUF_VDA(J2))//' unknown'
                     ELSE IF ( BUF_VDA(J2)(IND(1,2):IND(2,2)) == 'EXP_NAME' ) THEN
                        BUF_VDA(J2) = TRIM(BUF_VDA(J2))//' unknown'
                     ELSE IF ( BUF_VDA(J2)(IND(1,2):IND(2,2)) == 'PI_NAME' ) THEN
                        BUF_VDA(J2) = TRIM(BUF_VDA(J2))//' unknown'
                     ELSE IF ( BUF_VDA(J2)(IND(1,2):IND(2,2)) == 'REC_MODE' ) THEN
                        BUF_VDA(J2) = TRIM(BUF_VDA(J2))//' unknown'
                     ELSE IF ( BUF_VDA(J2)(IND(1,2):IND(2,2)) == 'QUALCODE' ) THEN
                        BUF_VDA(J2) = TRIM(BUF_VDA(J2))//' 0'
                   END IF
              END IF
              IF ( BUF_VDA(J2)(1:15) == 'DATA.1 REF_FREQ' ) THEN
!
! ---------------- Get the refrence frequency
!
                   CALL CHIN ( BUF_VDA(J2)(IND(1,5):IND(2,5)), IND_BAND )
                   READ ( BUF_VDA(J2)(IND(1,7):IND(2,7)), FMT='(F22.15)' ) VAL
                   IF ( VAL > REF_FREQ(IND_BAND) ) REF_FREQ(IND_BAND) = VAL
              END IF
              IF ( BUF_VDA(J2)(IND(1,2):IND(2,2)) == 'BAND_NAM' ) THEN
                   IND_BND_STR = J2
              END IF
            ELSE IF ( BUF_VDA(J2)(1:25) == 'TOCS.2 NUM_SAM2   BAS  R8' .AND. &
     &                BUF_VDA(J2)(33:33) == '2' ) THEN
              IND_N2  = J2
            ELSE IF ( BUF_VDA(J2)(1:23) == 'DATA.2 @section_length:' ) THEN
              IND_DA2 = J2 
            ELSE IF ( BUF_VDA(J2)(1:15) == 'DATA.2 NUSEDCHN' ) THEN
              CALL EXWORD ( BUF_VDA(J2), MIND, LIND, IND, CHAR(32), IUER )
              CALL CHIN ( BUF_VDA(J2)(IND(1,3):IND(2,3)), IND_OBS  )
              CALL CHIN ( BUF_VDA(J2)(IND(1,5):IND(2,5)), IND_BAND )
              CALL CHIN ( BUF_VDA(J2)(IND(1,7):IND(2,7)), IVAL     )
              KMAX_NUSEDCHN(IND_BAND) = MAX ( IVAL, KMAX_NUSEDCHN(IND_BAND) )
              IF ( IVAL > 0 ) KOBS_NUSEDCHN(IND_BAND) = IND_OBS
            ELSE IF ( BUF_VDA(J2)(1:23) == 'TOCS.4 @section_length:' ) THEN
              IND_TO4 = J2 
            ELSE IF ( BUF_VDA(J2)(1:23) == 'DATA.4 @section_length:' ) THEN
              IND_DA4 = J2 
            ELSE IF ( BUF_VDA(J2)(1:12) == 'TOCS.1 DATYP' ) THEN
              IND_DATYP_TOCS = J2 
            ELSE IF ( BUF_VDA(J2)(1:15) == 'TOCS.1 DELRESID' ) THEN
              IND_DELRESID_TOCS = J2 
            ELSE IF ( BUF_VDA(J2)(1:12) == 'TOCS.1 DATYP' ) THEN
              IND_DATYP_TOCS = J2 
            ELSE IF ( BUF_VDA(J2)(1:12) == 'TOCS.4 DATYP' ) THEN
              IND_DATYP_TOCS = J2 
            ELSE IF ( BUF_VDA(J2)(1:12) == 'DATA.4 DATYP' ) THEN
              IND_DATYP_DATA = J2 
            ELSE IF ( BUF_VDA(J2)(1:21) == 'TOCS.4 DB_VERS    SES' ) THEN
              IND_DBVERS = J2 
            ELSE IF ( BUF_VDA(J2)(1:5) == 'FILE.' ) THEN
              IL = ILEN(BUF_VDA(J2))
              CALL CHIN ( BUF_VDA(J2)(IL-6:IL-4), IVAL )
              IF ( IVAL > 0 ) DB_VERS = MAX ( DB_VERS, IVAL )
         END IF
 420  CONTINUE 
      IF ( IND_N2 > 0 .AND. IND_PC > 0 ) THEN
           NS2_STATUS = 'n2_fail'
         ELSE 
           NS2_STATUS = 'n2_ok  '
      END IF
!
! --- Re-compute band name
!
      BAND_NAM(1) = ' '
      BAND_NAM(2) = ' '
      DO 430 J3=1,2
         IF ( REF_FREQ(J3) < 0.6D9 ) THEN
              BAND_NAM(J3) = '?'
            ELSE IF ( REF_FREQ(J3) < 1.8D9 ) THEN
              BAND_NAM(J3) = 'L'
            ELSE IF ( REF_FREQ(J3) < 2.8D9 ) THEN
              BAND_NAM(J3) = 'S'
            ELSE IF ( REF_FREQ(J3) < 6.5D9 ) THEN
              BAND_NAM(J3) = 'C'
            ELSE IF ( REF_FREQ(J3) < 9.2D9 ) THEN
              BAND_NAM(J3) = 'X'
            ELSE IF ( REF_FREQ(J3) < 16.2D9 ) THEN
              BAND_NAM(J3) = 'U'
            ELSE IF ( REF_FREQ(J3) < 26.D9 ) THEN
              BAND_NAM(J3) = 'K'
            ELSE IF ( REF_FREQ(J3) < 46.D9 ) THEN
              BAND_NAM(J3) = 'Q'
            ELSE IF ( REF_FREQ(J3) < 95.D9 ) THEN
              BAND_NAM(J3) = 'W'
            ELSE IF ( REF_FREQ(J3) < 130.D9 ) THEN
              BAND_NAM(J3) = 'D'
            ELSE
              BAND_NAM(J3) = '?'
         END IF
 430  CONTINUE 
      IF ( NUM_BAND == 1 ) THEN
           BAND_NAM(2) = ' '
      END IF
      IF ( IND_B2 > 0 .AND. NUM_BAND == 1 ) THEN
!
! -------- Update band name
!
           BUF_VDA(IND_B2) = 'TOCS.1 BAND_NAM   SES  C1   1   1  Band names'
           BUF_VDA(IND_BND_STR)(27:27) = BAND_NAM(1)
           NUB_STATUS = 'single'
         ELSE IF ( IND_B2 > 0 .AND. NUM_BAND == 2 ) THEN
           BUF_VDA(IND_B2) = 'TOCS.1 BAND_NAM   SES  C1   1   2  Band names'
           READ ( UNIT=BUF_VDA(IND_DA1)(25:34),  FMT='(I10)' ) NUM_REC 
           NUM_REC = NUM_REC + 1
           WRITE ( UNIT=BUF_VDA(IND_DA1)(25:34), FMT='(I10)' ) NUM_REC 
           BUF_VDA(IND_BND_STR)(27:28) = BAND_NAM(1)//' '
           DO 440 J4=NV,IND_BND_STR+1,-1
              BUF_VDA(J4+1) = BUF_VDA(J4)
 440       CONTINUE 
           BUF_VDA(IND_BND_STR+1) = 'DATA.1 BAND_NAM 0 0  1  2 '//BAND_NAM(2)
           IND_TO4 = IND_TO4 + 1
           IND_DA4 = IND_DA4 + 1
           IND_DA2 = IND_DA2 + 1
           IF ( IND_DATYP_TOCS     > 0 ) IND_DATYP_TOCS    = IND_DATYP_TOCS    + 1
           IF ( IND_DATYP_DATA     > 0 ) IND_DATYP_DATA    = IND_DATYP_DATA    + 1
           IF ( IND_DELRESID_TOCS  > 0 ) IND_DELRESID_TOCS = IND_DELRESID_TOCS + 1
           IF ( IND_DELRESID_DATA  > 0 ) IND_DELRESID_DATA = IND_DELRESID_DATA + 1
           IF ( IND_DBVERS         > 0 ) IND_DBVERS        = IND_DBVERS        + 1
           NV = NV + 1
           NUB_STATUS = 'nub_failed'
        ELSE
           NUB_STATUS = 'nub_ok'
      END IF
!
      NUS_STATUS = 'nus_ok'
      IF ( KOBS_NUSEDCHN(1) > 0 .AND. KOBS_NUSEDCHN(1) < 0.80*NUMB_OBS ) THEN
           WRITE ( 6, '(A,2X,I6,2X,I6,2X, I3)' ) 'NUSEDCHN-1 '//TRIM(FIL_SESS), KOBS_NUSEDCHN(1), NUMB_OBS, KMAX_NUSEDCHN(1)
           NUS_STATUS = 'nus_fail'
      END IF
      IF ( KOBS_NUSEDCHN(2) > 0 .AND. KOBS_NUSEDCHN(2) < 0.80*NUMB_OBS ) THEN
           WRITE ( 6, '(A,2X,I6,2X,I6,2X,I3)' ) 'NUSEDCHN-2 '//TRIM(FIL_SESS), KOBS_NUSEDCHN(2), NUMB_OBS, KMAX_NUSEDCHN(2)
           NUS_STATUS = 'nus_fail'
      END IF
!@      IF ( NUS_STATUS == 'nus_fail' ) THEN
!@           CALL NUS_UPDATE ( NUMB_OBS, NV, KMAX_NUSEDCHN, BUF_VDA )
!@      END IF
      IF ( EXP_ENV == EXP_DB ) THEN
           EXP_STATUS = 'name_ok'
         ELSE
           EXP_STATUS = 'name_diff'
      END IF
!
      IF ( IND_DBVERS == 0 .AND. IND_DATYP_TOCS > 0 ) THEN
           NUD_STATUS = 'dv_failed'
           IF ( IND_DATYP_DATA > 0 ) THEN
                DO 450 J5=NV,IND_DATYP_DATA+1,-1
                   BUF_VDA(J5+1) = BUF_VDA(J5)
 450            CONTINUE 
                CALL INCH ( DB_VERS, DB_VERS_STR )
                IF ( IND_DA4 > 0 ) THEN
                     BUF_VDA(IND_DATYP_DATA+1) = 'DATA.4 DB_VERS  0 0  1  1 '//DB_VERS_STR
                   ELSE
                     BUF_VDA(IND_DATYP_DATA+1) = 'DATA.1 DB_VERS  0 0  1  1 '//DB_VERS_STR
                END IF
                NV = NV + 1
!
                DO 460 J6=NV,IND_DATYP_TOCS+1,-1
                   BUF_VDA(J6+1) = BUF_VDA(J6)
 460            CONTINUE 
             ELSE IF ( IND_DELRESID_DATA > 0 ) THEN 
!
! ------------- This part is not worling
!
                DO 550 J5=NV,IND_DELRESID_DATA,-1
                   BUF_VDA(J5+1) = BUF_VDA(J5)
 550            CONTINUE 
                CALL INCH ( DB_VERS, DB_VERS_STR )
                IF ( IND_DA4 > 0 ) THEN
                     BUF_VDA(IND_DELRESID_DATA) = 'DATA.4 DB_VERS  0 0  1  1 '//DB_VERS_STR
                   ELSE
                     BUF_VDA(IND_DELRESID_DATA) = 'DATA.1 DB_VERS  0 0  1  1 '//DB_VERS_STR
                END IF
                NV = NV + 1
!
                DO 560 J6=NV,IND_DELRESID_TOCS,-1
                   BUF_VDA(J6+1) = BUF_VDA(J6)
 560            CONTINUE 
           END IF
           IF ( IND_DA4 > 0 ) THEN
                BUF_VDA(IND_DATYP_TOCS+1) = 'TOCS.4 DB_VERS    SES  I2   1   1  Database version counter'
                IND_DA4 = IND_DA4 + 1
                READ  ( UNIT=BUF_VDA(IND_DA4)(25:34), FMT='(I10)' ) NUM_REC 
                NUM_REC = NUM_REC + 1
                WRITE ( UNIT=BUF_VDA(IND_DA4)(25:34), FMT='(I10)' ) NUM_REC 
!
                READ  ( UNIT=BUF_VDA(IND_TO4)(25:30), FMT='(I6)'  ) NUM_REC 
                NUM_REC = NUM_REC + 1
                WRITE ( UNIT=BUF_VDA(IND_TO4)(25:30), FMT='(I6)'  ) NUM_REC 
              ELSE
                IF ( IND_DATYP_TOCS > 0 ) THEN
                     BUF_VDA(IND_DATYP_TOCS+1) = 'TOCS.1 DB_VERS    SES  I2   1   1  Database version counter'
                  ELSE IF ( IND_DELRESID_TOCS > 0 ) THEN
                     BUF_VDA(IND_DELRESID_TOCS) = 'TOCS.1 DB_VERS    SES  I2   1   1  Database version counter'
                END IF
                IND_DA1 = IND_DA1 + 1
                CALL EXWORD ( BUF_VDA(IND_DA1), MIND, LIND, IND, CHAR(32), IUER )
                READ  ( UNIT=BUF_VDA(IND_DA1)(IND(1,3):IND(2,3)), FMT='(I6)'  ) NUM_REC 
                WRITE ( UNIT=STR(1:10), FMT='(I10)' ) NUM_REC 
                BUF_VDA(IND_DA1) = BUF_VDA(IND_DA1)(1:IND(1,3)-1)//STR(1:10)//' '//BUF_VDA(IND_DA1)(IND(1,4):)
!
                CALL EXWORD ( BUF_VDA(IND_TO1), MIND, LIND, IND, CHAR(32), IUER )
                READ  ( UNIT=BUF_VDA(IND_TO1)(IND(1,3):IND(2,3)), FMT='(I6)'  ) NUM_REC 
                NUM_REC = NUM_REC + 1
                WRITE ( UNIT=STR, FMT='(I6)'  ) NUM_REC 
                BUF_VDA(IND_TO1) = BUF_VDA(IND_TO1)(1:IND(1,3))//STR(1:6)//' '//BUF_VDA(IND_TO1)(IND(1,4):IND(2,LIND))
           END IF
           NV = NV + 1
         ELSE
           NUD_STATUS = 'dv_ok'
      END IF
!
      QC_STATUS = "qc_??"
      IF ( QC_MODE == 11 .AND. NUM_BAND == 1 ) THEN
           QC_STATUS = "qc_ok"
         ELSE IF ( QC_MODE == 12 .AND. NUM_BAND == 2 ) THEN
           QC_STATUS = "ok"
         ELSE IF ( QC_MODE == 12 .AND. NUM_BAND == 1 ) THEN
           QC_STATUS = "qc_12_1"
         ELSE IF ( QC_MODE == 21 .AND. NUM_BAND == 1 ) THEN
           QC_STATUS = "qc_21_1"
         ELSE IF ( QC_MODE == 21 .AND. NUM_BAND == 2 ) THEN
           QC_STATUS = "qc_21_2"
         ELSE IF ( QC_MODE == 22 .AND. NUM_BAND == 1 ) THEN
           QC_STATUS = "qc_22_1"
         ELSE IF ( QC_MODE == 22 .AND. NUM_BAND == 2 ) THEN
           QC_STATUS = "qc_22_2"
      END IF
      IF ( QC_STATUS .NE. 'ok' .AND. FL_FIX ) THEN
           CALL QC_FIX ( NV, MS, QC_STATUS, BUF_VDA )
      END IF
      IF ( IND_PC > 0 ) THEN
           CALL NS2_FIX ( NV, BUF_VDA )
      END IF 
      IF ( GEN_STATUS == 'gen_undef' ) THEN
           CALL GEN_FIX ( NV, BUF_VDA )
      END IF 
!
      CALL CLRCH ( CLBR_STATUS )
      IF ( FL_NUM_CLBR .AND. NUM_CLBR > 0 ) THEN
           IF ( .NOT. FL_MJD_CLBR .AND. .NOT. FL_UTC_CLBR ) THEN
                CLBR_STATUS = 'Missing MJD_CLBR and UTC_CLBR'
             ELSE IF ( .NOT. FL_MJD_CLBR .AND. FL_UTC_CLBR ) THEN
                CLBR_STATUS = 'Missing MJD_CLBR'
             ELSE IF ( FL_MJD_CLBR .AND. .NOT. FL_UTC_CLBR ) THEN
                CLBR_STATUS = 'Missing UTC_CLBR'
             ELSE
                CLBR_STATUS = 'clbr_ok'
           END IF
      END IF
      IF ( CLBR_STATUS(1:7) == 'Missing' )  THEN
           CALL CLBR_FIX ( NV, BUF_VDA )
      END IF 
!
      CALL STR_BLANK_TRAILER ( FIL_SESS )
      IF ( FL_SHORT_LINE ) THEN
           WRITE ( 6, '(A,2X,A,2X,A)' ) 'CHECK '//TRIM(FIL_SESS), SESS_NAME, 'SHORT_LINE'
        ELSE
           IF ( FL_NOVALUE ) THEN
                WRITE ( 6, 210 ) 'CHECK '//TRIM(FIL_SESS), SESS_NAME, EXP_ENV, EXP_DB, EXP_STATUS, &
     &                                                     NUS_STATUS, 'novalue', NUB_STATUS, &
     &                                                     NUD_STATUS, QC_STATUS, NS2_STATUS, &
     &                                                     GEN_STATUS
 210            FORMAT ( A,2X,A,2X,A,2X,A,2X,A,2X,A,2X,A,2X,A,2X,A,2X,A,2X,A,2X,A)
             ELSE
                WRITE ( 6, 220 ) 'CHECK '//TRIM(FIL_SESS), TRIM(SESS_NAME), TRIM(EXP_ENV), TRIM(EXP_DB), &
     &                                                     TRIM(EXP_STATUS), TRIM(NUS_STATUS), &
     &                                                     TRIM(NUB_STATUS), TRIM(NUD_STATUS), &
     &                                                     TRIM(QC_STATUS), TRIM(NS2_STATUS), &
     &                                                     TRIM(GEN_STATUS), TRIM(CLBR_STATUS)
 220            FORMAT ( A,2X,A,2X,A,2X,A,2X,A,2X,A,2X,A,2X,A,2X,A,2X,A,2X,A,2X,A)
           END IF
      END IF
      IF ( .NOT. FL_VDA ) THEN
           CALL UNLINK ( TRIM(TEMP_FILE)//CHAR(0) )
      END IF
!
      IF ( FL_FIX ) THEN
           IUER = -1
           CALL WR_TEXT ( NV, BUF_VDA, FILOUT, IUER )
           IF ( IUER .NE. 0 ) CALL EXIT ( 1 )
           WRITE ( 6, '(A)' ) 'Output repaired file: '//TRIM(FILOUT)
      END IF
!
      END  PROGRAM    GVF_REPAIR_DB  !#!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE NUS_UPDATE ( NUMB_OBS, NV, KMAX_NUSEDCHN, BUF_VDA )
! ************************************************************************
! *                                                                      *
! *   Auxilliary program  NUS_UPDATE
! *                                                                      *
! *  ### 14-NOV-2019  NUS_UPDATE   v1.0 (c)  L. Petrov  21-NOV-2019 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INTEGER*4  NV, KMAX_NUSEDCHN(2), NUMB_OBS 
      CHARACTER  BUF_VDA(NV)*(*)
      INTEGER*4    MIND
      PARAMETER  ( MIND = 256 )
      INTEGER*4  J1, LIND, IND(2,MIND), IVAL, IND_OBS, IND_BAND, IER
!
      DO 410 J1=1,NV
         IF ( BUF_VDA(J1)(1:15) == 'DATA.2 NUSEDCHN' ) THEN
              CALL EXWORD ( BUF_VDA(J1), MIND, LIND, IND, CHAR(32), IER )
              CALL CHIN ( BUF_VDA(J1)(IND(1,3):IND(2,3)), IND_OBS  )
              CALL CHIN ( BUF_VDA(J1)(IND(1,5):IND(2,5)), IND_BAND )
              CALL CHIN ( BUF_VDA(J1)(IND(1,7):IND(2,7)), IVAL     )
              IF ( IVAL == 0 ) THEN
                   WRITE ( UNIT=BUF_VDA(J1)(IND(1,7):), FMT='(I3)' ) KMAX_NUSEDCHN(IND_BAND)
                   CALL CHASHL ( BUF_VDA(J1)(IND(1,7):) )
              END IF
         END IF
 410  CONTINUE 
!
      RETURN
      END  SUBROUTINE  NUS_UPDATE  !#!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE QC_FIX ( NV, MS, QC_STATUS, BUF_VDA )
! ************************************************************************
! *                                                                      *
! *   Auxilliary program  QC_UPDATE
! *                                                                      *
! *  ### 16-NOV-2019  QC_UPDATE   v1.0 (c)  L. Petrov  21-NOV-2019 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INTEGER*4  MS, NV
      CHARACTER  QC_STATUS*(*), BUF_VDA(NV)*(*)
      INTEGER*4    MIND
      PARAMETER  ( MIND = 256 )
      INTEGER*4, ALLOCATABLE :: QC_VAL(:,:)
      CHARACTER, ALLOCATABLE :: BUF_TMP(:)*384
      INTEGER*4  J1, J2, J3, J4, J5, J6, LIND, IND(2,MIND), IVAL, &
     &           NOBS, IND_OBS, IND_BAND, QC_NOZERO_ACC(2), IDA1, IP1, &
     &           KV, NUM_REC, N_SKIPPED, IER
!
      ALLOCATE ( QC_VAL(MS,2) )
      NOBS = 0
      QC_NOZERO_ACC = 0
      IDA1 = 0
      DO 410 J1=1,NV
         CALL EXWORD ( BUF_VDA(J1), MIND, LIND, IND, CHAR(0)//CHAR(32)//CHAR(9), IER )
         IF ( BUF_VDA(J1)(1:15) == "TOCS.1 QUALCODE" ) THEN
              BUF_VDA(J1)(36:) = 'Quality code as char*1 value: 5-9 is good, 0 -- non-detection, letter -- failure'
              BUF_VDA(J1)(29:29) = '1'
              IF ( QC_STATUS == "qc_21_1" .OR. QC_STATUS == "qc_12_1" .OR. QC_STATUS == "qc_22_1" ) THEN
                   BUF_VDA(J1)(33:33) = '1'
              END IF
           ELSE IF ( BUF_VDA(J1)(1:23) == 'DATA.1 @section_length:' ) THEN
              IDA1 = J1
           ELSE IF ( BUF_VDA(J1)(1:15) == "DATA.1 QUALCODE" ) THEN
              IF ( QC_STATUS == "qc_22_2" .OR. QC_STATUS == "qc_21_1" ) THEN
                   IF ( BUF_VDA(J1)(IND(1,7):IND(1,7)) == '_' ) THEN
                        BUF_VDA(J1)(IND(1,7):) = BUF_VDA(J1)(IND(1,7)+1:IND(1,7)+1)//' '
                      ELSE
                        BUF_VDA(J1)(IND(1,7)+1:IND(1,7)+1) = ' '
                   END IF
                ELSE IF ( QC_STATUS == "qc_22_1" .OR. QC_STATUS == "qc_12_1" ) THEN
                   CALL CHIN ( BUF_VDA(J1)(IND(1,3):IND(1,3)), IND_OBS  )
                   CALL CHIN ( BUF_VDA(J1)(IND(1,6):IND(1,6)), IND_BAND )
                   NOBS = MAX ( NOBS, IND_OBS )
                   IF ( BUF_VDA(J1)(IND(1,7):IND(1,7)) == '_' ) THEN
                        CALL CHIN ( BUF_VDA(J1)(IND(1,7)+1:IND(1,7)+1), QC_VAL(IND_OBS,IND_BAND) )
                      ELSE
                        CALL CHIN ( BUF_VDA(J1)(IND(1,7):IND(1,7)), QC_VAL(IND_OBS,IND_BAND) )
                   END IF
                   IF ( QC_VAL(IND_OBS,IND_BAND) > 0 ) QC_NOZERO_ACC(IND_BAND) = QC_NOZERO_ACC(IND_BAND) + 1
              END IF
         END IF
 410  CONTINUE 
!
      IF ( QC_STATUS == "qc_22_1" .OR. QC_STATUS == "qc_12_1" ) THEN
           N_SKIPPED = 0
           DO 420 J2=1,NV
              CALL EXWORD ( BUF_VDA(J2), MIND, LIND, IND, CHAR(0)//CHAR(32)//CHAR(9), IER )
              CALL CHIN ( BUF_VDA(J1)(IND(1,3):IND(1,3)), IND_OBS  )
              CALL CHIN ( BUF_VDA(J1)(IND(1,6):IND(1,6)), IND_BAND )
              IF ( BUF_VDA(J2)(1:15) == "DATA.1 QUALCODE" ) THEN
                   IP1 = IND(1,7)
                   IF ( QC_NOZERO_ACC(1) < QC_NOZERO_ACC(2) ) THEN
                        CALL EXWORD ( BUF_VDA(J2+1), MIND, LIND, IND, CHAR(0)//CHAR(32)//CHAR(9), IER )
                        BUF_VDA(J2)(IP1:) = BUF_VDA(J2+1)(IND(1,7):IND(1,7))
                   END IF
                   IF ( BUF_VDA(J2)(IP1:IP1) == '_' ) THEN
                        BUF_VDA(J2)(IP1:) = BUF_VDA(J2)(IP1+1:)
                   END IF
                   BUF_VDA(J2+1)(1:1) = '@'
                   N_SKIPPED = N_SKIPPED + 1
              END IF
 420       CONTINUE 
      END IF
!
      IF ( QC_STATUS == "qc_22_1" .OR. QC_STATUS == "qc_12_1" ) THEN
           ALLOCATE ( BUF_TMP(NV) )
           KV = 0
           DO 450 J5=1,NV
              IF ( BUF_VDA(J5)(1:1) == '@' ) then
                   CONTINUE 
                 ELSE 
                   KV = KV + 1
                   BUF_TMP(KV) = BUF_VDA(J5)
                   IF ( J5 == IDA1 ) THEN
                        READ ( UNIT=BUF_TMP(KV)(25:34),  FMT='(I10)' ) NUM_REC 
                        NUM_REC = NUM_REC - N_SKIPPED
                        WRITE ( UNIT=BUF_TMP(KV)(25:34), FMT='(I10)' ) NUM_REC 
                   END IF
              END IF
 450       CONTINUE 
!
           NV = KV
           DO 460 J6=1,NV
              BUF_VDA(J6) = BUF_TMP(J6)
 460       CONTINUE 
           DEALLOCATE ( BUF_TMP )
      END IF
!
      DEALLOCATE ( QC_VAL )
      RETURN
      END  SUBROUTINE  QC_FIX  !#!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE NS2_FIX ( NV, BUF_VDA )
! ************************************************************************
! *                                                                      *
! *   Auxilliary program  NS2_UPDATE
! *                                                                      *
! *  ### 14-NOV-2019  NS2_UPDATE   v1.0 (c)  L. Petrov  22-NOV-2019 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INTEGER*4  MS, NV
      CHARACTER  BUF_VDA(NV)*(*)
      INTEGER*4    MIND
      PARAMETER  ( MIND = 256 )
      INTEGER*4, ALLOCATABLE :: QC_VAL(:,:)
      CHARACTER, ALLOCATABLE :: BUF_TMP(:)*384
      INTEGER*4  J1, J2, J3, J4, J5, J6, LIND, IND(2,MIND), IVAL, &
     &           NOBS, IND_OBS, IND_BAND, QC_NOZERO_ACC(2), IDA2, IP1, &
     &           KV, NUM_REC, N_SKIPPED, IER
!
      NOBS = 0
      IDA2 = 0
      N_SKIPPED = 0
      DO 410 J1=1,NV
         CALL EXWORD ( BUF_VDA(J1), MIND, LIND, IND, CHAR(0)//CHAR(32)//CHAR(9), IER )
         IF ( BUF_VDA(J1)(1:23) == 'DATA.2 @section_length:' ) THEN
              IDA2 = J1
            ELSE IF ( BUF_VDA(J1)(1:25) == "TOCS.2 NUM_SAM1   BAS  R8" ) THEN
              BUF_VDA(J1)(33:) = '1  Number of samples used in bandwidth synth. in band 1 per frequency chan'
            ELSE IF ( BUF_VDA(J1)(1:25) == "TOCS.2 NUM_SAM2   BAS  R8" ) THEN
              BUF_VDA(J1)(33:) = '1  Number of samples used in bandwidth synth. in band 2 per frequency chan'
            ELSE IF ( BUF_VDA(J1)(1:15) == "DATA.2 NUM_SAM2" .AND. &
     &                BUF_VDA(J1)(IND(1,6):IND(2,6)) == '2'        ) THEN
              BUF_VDA(J1)(1:1) = '@'
              N_SKIPPED = N_SKIPPED + 1
         END IF
 410  CONTINUE 
!
      IF ( N_SKIPPED > 0 ) THEN
           ALLOCATE ( BUF_TMP(NV) )
           KV = 0
           DO 420 J2=1,NV
              IF ( BUF_VDA(J2)(1:1) == '@' ) then
                   CONTINUE 
                 ELSE 
                   KV = KV + 1
                   BUF_TMP(KV) = BUF_VDA(J2)
                   IF ( J2 == IDA2 ) THEN
                        READ ( UNIT=BUF_TMP(KV)(25:34),  FMT='(I10)' ) NUM_REC 
                        NUM_REC = NUM_REC - N_SKIPPED
                        WRITE ( UNIT=BUF_TMP(KV)(25:34), FMT='(I10)' ) NUM_REC 
                   END IF
              END IF
 420       CONTINUE 
!
           NV = KV
           DO 430 J3=1,NV
              BUF_VDA(J3) = BUF_TMP(J3)
 430       CONTINUE 
           DEALLOCATE ( BUF_TMP )
      END IF
!
      RETURN
      END  SUBROUTINE  NS2_FIX  !#!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE GEN_FIX ( NV, BUF_VDA )
! ************************************************************************
! *                                                                      *
! *   Auxilliary program  GEN_UPDATE
! *                                                                      *
! *  ### 14-NOV-2019  GEN_UPDATE   v1.0 (c)  L. Petrov  21-NOV-2019 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INTEGER*4  MS, NV
      CHARACTER  BUF_VDA(NV+1)*(*)
      INTEGER*4    MIND
      PARAMETER  ( MIND = 256 )
      INTEGER*4  J1, J2, IND_TEXT, NUM_REC, LIND, IND(2,MIND), IER
!
      DO 410 J1=1,NV
         IF ( BUF_VDA(J1)(1:23) == 'PREA.1 @section_length:' ) THEN
              CALL EXWORD ( BUF_VDA(J1), MIND, LIND, IND, CHAR(32), IER )
              READ  ( UNIT=BUF_VDA(J1)(IND(1,3):IND(2,3)), FMT='(I6)' ) NUM_REC 
              NUM_REC = NUM_REC + 1
              WRITE ( UNIT=BUF_VDA(J1)(IND(1,3):IND(2,3)), FMT='(I6)' ) NUM_REC 
            ELSE IF ( BUF_VDA(J1)(1:6) == 'TEXT.1' ) THEN
              IND_TEXT = J1
              GOTO 810
         END IF
 410  CONTINUE 
 810  CONTINUE 
!
      DO 420 J2=NV,IND_TEXT,-1
         BUF_VDA(J2+1) = BUF_VDA(J2) 
 420  CONTINUE 
      BUF_VDA(IND_TEXT) = 'PREA.1 GENERATOR: mark3_to_gvh_proto GVH release of 2005.11.22'
      NV = NV + 1
!
      RETURN
      END  SUBROUTINE  GEN_FIX  !#!#
!
! ------------------------------------------------------------------------
!
        SUBROUTINE STR_BLANK_TRAILER ( STR )
! ************************************************************************
! *                                                                      *
! *     Routine  BZERO_TO_BLANK  replaces binary zeroes (code=0) with    *
! *   blanks (code=32) in the in the string STR.                         *
! *                                                                      *
! ************************************************************************
        IMPLICIT   NONE
        CHARACTER  STR*(*)
        INTEGER*4  J1
        LOGICAL*1  FL_BLANK
!
        FL_BLANK = .FALSE.
        DO 410 J1=1,LEN(STR)
           IF ( STR(J1:J1) .EQ. CHAR(0) ) FL_BLANK = .TRUE.
           IF ( FL_BLANK ) THEN
                STR(J1:J1) = ' '
           END IF
  410   CONTINUE
        RETURN
        END  !#!  STR_BLANK_TRAILER #!#
!
! ------------------------------------------------------------------------
!
      FUNCTION FIX_AP ( NV, BUF_VDA, FIL_AUX, IUER )
! ************************************************************************
! *                                                                      *
! *   Auxilliary program  FIX_AP
! *                                                                      *
! *  ### 14-NOV-2019  NUS_UPDATE   v1.0 (c)  L. Petrov  25-NOV-2019 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INTEGER*4  FIX_AP
      INTEGER*4  NV, IUER
      INTEGER*4  MS, MIND
      PARAMETER  ( MS = 8192 )
      PARAMETER  ( MIND = 128 )
      CHARACTER  BUF_VDA(NV+4)*(*), FIL_AUX*(*)
      CHARACTER  EXP_CODE*8, BUF_AUX(MS)*64, AP_LEN_STR*12, NUM_SPCH_STR*8
      INTEGER*4  J1, J2, J3, J4, J5, J6, J7, J8, IND_TO1, IND_DA1, &
     &           IND_AP, IND_NS, NA, LIND, IND(2,MIND), NUM_REC, IER, &
     &           IND_BAN_TOCS, IND_NAV_TOCS, IND_BAN_DATA, IND_NAV_DATA
      INTEGER*4, EXTERNAL :: ILEN, I_LEN
!
      FIX_AP = 1
!
      IND_DA1 = 0
      IND_TO1 = 0
      IND_AP  = 0
      IND_NS  = 0
      IND_BAN_TOCS = 0
      IND_NAV_TOCS = 0
      IND_BAN_DATA = 0
      IND_NAV_DATA = 0
!
      DO 410 J1=1,NV
         CALL EXWORD ( BUF_VDA(J1), MIND, LIND, IND, CHAR(32), IER )
         IF ( BUF_VDA(J1)(1:15) == 'DATA.1 EXP_CODE' ) THEN
              IF ( LIND < 7 ) THEN
                   CALL ERR_LOG ( 8001, IUER, 'FIX_AP', 'Trap of internal '// &
     &                 'control: a blank on place for EXP_CODE' )
                   RETURN 
              END IF
              EXP_CODE = BUF_VDA(J1)(IND(1,7):IND(2,7))
            ELSE IF ( BUF_VDA(J1)(1:23) == 'TOCS.1 @section_length:' ) THEN
              IND_TO1 = J1
            ELSE IF ( BUF_VDA(J1)(1:23) == 'DATA.1 @section_length:' ) THEN
              IND_DA1 = J1
            ELSE IF ( BUF_VDA(J1)(1:15) == 'DATA.1 APLENGTH' ) THEN
              IND_AP = J1
            ELSE IF ( BUF_VDA(J1)(1:15) == 'DATA.1 NUM_SPCH' ) THEN
              IND_NS = J1
            ELSE IF ( BUF_VDA(J1)(1:15) == 'TOCS.1 BAND_NAM' ) THEN
              IND_BAN_TOCS = J1
            ELSE IF ( BUF_VDA(J1)(1:15) == 'TOCS.1 N_AVBAND' ) THEN
              IND_NAV_TOCS = J1
            ELSE IF ( BUF_VDA(J1)(1:15) == 'TOCS.1 PHRATERR' ) THEN
              IF ( IND_NAV_TOCS == 0 ) IND_NAV_TOCS = J1
            ELSE IF ( BUF_VDA(J1)(1:15) == 'TOCS.1 PI_NAME ' ) THEN
              IF ( IND_NAV_TOCS == 0 ) IND_NAV_TOCS = J1
            ELSE IF ( BUF_VDA(J1)(1:15) == 'DATA.1 BAND_NAM' ) THEN
              IND_BAN_DATA = J1
            ELSE IF ( BUF_VDA(J1)(1:15) == 'DATA.1 N_AVBAND' ) THEN
              IND_NAV_DATA = J1
            ELSE IF ( BUF_VDA(J1)(1:15) == 'DATA.1 PHRATERR' ) THEN
              IF ( IND_NAV_DATA == 0 ) IND_NAV_DATA = J1
            ELSE IF ( BUF_VDA(J1)(1:15) == 'DATA.1 PI_NAME ' ) THEN
              IF ( IND_NAV_DATA == 0 ) IND_NAV_DATA = J1
         END IF
 410  CONTINUE 
      IF ( IND_AP == 0 .AND. IND_NS == 0 ) THEN
           CALL ERR_PASS ( IUER, IER ) 
           CALL RD_TEXT ( FIL_AUX, MS, BUF_AUX, NA, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 8002, IUER, 'FIX_AP', 'Failure in reading '// &
     &              'auxilliary file '//FIL_AUX )
                RETURN 
           END IF
           CALL CLRCH ( AP_LEN_STR  )
           CALL CLRCH ( NUM_SPCH_STR )
           DO 420 J2=1,NA
              IF ( EXP_CODE == BUF_AUX(J2)(1:LEN(EXP_CODE)) ) THEN
                   NUM_SPCH_STR = BUF_AUX(J2)(12:19) 
                   AP_LEN_STR   = BUF_AUX(J2)(20:31)
                   CALL CHASHL ( NUM_SPCH_STR )
                   CALL CHASHL ( AP_LEN_STR   )
              END IF
 420       CONTINUE 
           IF ( ILEN(NUM_SPCH_STR) == 0 ) THEN
                FIX_AP = 0
                CALL ERR_LOG ( 0, IUER )
                RETURN 
           END IF
           IF ( IND_BAN_TOCS == 0 ) THEN
                CALL ERR_LOG ( 8002, IUER, 'FIX_AP', 'Trap of internal '// &
     &              'control: Cannot find BAND_NAM lcodes' )
                RETURN 
           END IF
           IF ( IND_NAV_TOCS == 0 ) THEN
                CALL ERR_LOG ( 8003, IUER, 'FIX_AP', 'Trap of internal '// &
     &              'control: Cannot find neither PHRATERR no PI_CODE lcode' )
                RETURN 
           END IF
!
           DO 430 J3=NV,IND_NAV_DATA,-1
              BUF_VDA(J3+1) = BUF_VDA(J3) 
 430       CONTINUE 
           BUF_VDA(IND_NAV_DATA) = 'DATA.1 NUM_SPCH 0 0  1  1 '//TRIM(NUM_SPCH_STR)
           NV = NV + 1
!
           DO 440 J4=NV,IND_BAN_DATA,-1
              BUF_VDA(J4+1) = BUF_VDA(J4) 
 440       CONTINUE 
           BUF_VDA(IND_BAN_DATA)  = 'DATA.1 APLENGTH 0 0  1  1 '//TRIM(AP_LEN_STR)
           NV = NV + 1
!
           READ  ( UNIT=BUF_VDA(IND_DA1)(25:34),  FMT='(I10)' ) NUM_REC
           NUM_REC = NUM_REC + 2
           WRITE ( UNIT=BUF_VDA(IND_DA1)(25:34),  FMT='(I10)' ) NUM_REC
!
           DO 450 J5=NV,IND_NAV_TOCS,-1
              BUF_VDA(J5+1) = BUF_VDA(J5) 
 450       CONTINUE 
           BUF_VDA(IND_NAV_TOCS) = 'TOCS.1 NUM_SPCH   SES  I4   1   1  Number of spectral channels within an intermediate frequency band'
           NV = NV + 1
!
           DO 460 J6=NV,IND_BAN_TOCS,-1
              BUF_VDA(J6+1) = BUF_VDA(J6) 
 460       CONTINUE 
           BUF_VDA(IND_BAN_TOCS) = 'TOCS.1 APLENGTH   SES  R8   1   1  Length of accumul. period in sec'
           NV = NV + 1
           READ  ( UNIT=BUF_VDA(IND_TO1)(25:30),  FMT='(I6)' ) NUM_REC
           NUM_REC = NUM_REC + 2
           WRITE ( UNIT=BUF_VDA(IND_TO1)(25:30),  FMT='(I6)' ) NUM_REC
           FIX_AP = 1
         ELSE
           WRITE ( 6, * ) 'APLENGTH and NUM_SPCH are already present in the database for '//EXP_CODE 
           CALL ERR_LOG ( 0, IUER )
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  FUNCTION   FIX_AP  !#!  
!
! ------------------------------------------------------------------------
!
      SUBROUTINE CHECK_LEN ( NV, BUF_VDA, FIL_SESS, IUER )
! ************************************************************************
! *                                                                      *
! *   Auxilliary routine CHECK_LEN whether @section_length fields        *
! *   correctly descirve lenghts of a section in VGOSDA file.            *
! *                                                                      *
! *  ### 24-NOV-2019   CHECK_LEN   v1.0 (c)  L. Petrov  24-NOV-2019 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INTEGER*4  NV, IUER
      CHARACTER  BUF_VDA(NV)*(*), FIL_SESS*(*)
      INTEGER*4    MIND 
      PARAMETER  ( MIND = 128 )
      LOGICAL*1  FL_FAIL
      INTEGER*4  LIND, IND(2,MIND), NLEN, IND_LEN, NLEN_OLD, &
     &           IND_LEN_OLD, ID, IL, J1, IER 
      INTEGER*4, EXTERNAL :: ILEN, LINDEX
!
      FL_FAIL = .FALSE.
      NLEN_OLD = -1
      IND_LEN_OLD = -1 
      DO 410 J1=1,NV
         IF ( BUF_VDA(J1)(8:23) == '@section_length:' ) then
              CALL EXWORD ( BUF_VDA(J1), MIND, LIND, IND, CHAR(32), IER )
              CALL CHIN ( BUF_VDA(J1)(IND(1,3):IND(2,3)), NLEN )
              IND_LEN = J1
              IF ( .NOT. ( IND_LEN == IND_LEN_OLD + NLEN_OLD + 1 ) .AND. &
     &             IND_LEN_OLD > 0 .AND. &
     &             NLEN_OLD > 0          ) THEN
                   IF ( INDEX ( BUF_VDA(IND_LEN_OLD), 'chapters' ) < 1 ) THEN
                        WRITE ( 6, '(A,2X,I8,2X,I8,2X,I8)' ) 'Wrong length for section '//TRIM(BUF_VDA(IND_LEN_OLD))//' || ', &
     &                         IND_LEN, IND_LEN_OLD, NLEN_OLD 
                        FL_FAIL = .TRUE.
                   END IF
                ELSE IF ( NLEN_OLD == 0 .AND. IND_LEN_OLD > 0 ) THEN
                   IF ( BUF_VDA(IND_LEN_OLD)(8:8) .NE. '@' ) THEN
                        WRITE ( 6, '(A,2X,I8,2X,I8,2X,I8)' ) 'Wrong length for section '//TRIM(BUF_VDA(IND_LEN_OLD))//' || ', &
     &                          IND_LEN, IND_LEN_OLD, NLEN_OLD 
                        FL_FAIL = .TRUE.
                   END IF
              END IF
              NLEN_OLD= NLEN
              IND_LEN_OLD = IND_LEN
            ELSE IF ( BUF_VDA(J1)(1:19) == 'TEXT.1   @@chapter:' ) then
              CALL EXWORD ( BUF_VDA(J1), MIND, LIND, IND, CHAR(32), IER )
              CALL CHIN ( BUF_VDA(J1)(IND(1,4):IND(2,4)), NLEN )
              NLEN_OLD    = NLEN
              IND_LEN_OLD = J1
         END IF
 410  CONTINUE 
      ID = LINDEX ( FIL_SESS, '/' )
      IL = ILEN(FIL_SESS)
      IF ( FL_FAIL ) THEN
           WRITE ( 6, '(A)' ) FIL_SESS(ID+1:IL)//'   len_fail'
         ELSE
           WRITE ( 6, '(A)' ) FIL_SESS(ID+1:IL)//'   len_ok'
      END IF
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE CHECK_LEN   !#!#
!
! ------------------------------------------------------------------------
!
      FUNCTION FIX_EXP_NAME ( NV, BUF_VDA, FIL_AUX, EXP_OLD, EXP_NEW, IUER )
! ************************************************************************
! *                                                                      *
! *   Roputine FIX_EXP_NAME
! *                                                                      *
! *  ### 17-DEC-2019               v1.0 (c)  L. Petrov  17-DEC-2019 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INTEGER*4  FIX_EXP_NAME 
      INTEGER*4  NV, IUER
      CHARACTER  BUF_VDA(NV)*(*), FIL_AUX*(*), EXP_OLD*(*), EXP_NEW*(*)
      INTEGER*4    MIND 
      PARAMETER  ( MIND = 128 )
      LOGICAL*1  FL_FAIL
      INTEGER*4  LIND, IND(2,MIND), NLEN, IND_LEN, NLEN_OLD, &
     &           IND_LEN_OLD, IB, ID, IL, J1, NR, IER 
      INTEGER*4, EXTERNAL :: ILEN, LINDEX
!
      FIX_EXP_NAME = 0
      DO 410 J1=1,NV
         IF ( BUF_VDA(J1)(1:5) == 'FILE.' .OR. &
     &        BUF_VDA(J1)(1:16) == 'PREA.1 FILENAME:' ) THEN
!
              IL = ILEN(BUF_VDA(J1))
              IB = LINDEX ( BUF_VDA(J1)(1:IL-13), '_' ) + 1
              IF ( BUF_VDA(J1)(IB:IL-13) == EXP_OLD ) THEN
                   BUF_VDA(J1) = BUF_VDA(J1)(1:IB-1)//EXP_NEW//BUF_VDA(J1)(IL-12:IL)
                   FIX_EXP_NAME = FIX_EXP_NAME + 1
              END IF
         END IF
         IF ( BUF_VDA(J1)(1:25) == 'DATA.1 EXP_CODE 0 0  1  1' ) THEN
              IF ( BUF_VDA(J1)(27:27+ILEN(EXP_OLD)-1) == EXP_OLD(1:ILEN(EXP_OLD)) ) THEN
                   CALL CLRCH ( BUF_VDA(J1)(27:) )
                   BUF_VDA(J1)(27:) = EXP_NEW
                   FIX_EXP_NAME = FIX_EXP_NAME + 1
              END IF
         END IF
 410  CONTINUE 
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  FUNCTION   FIX_EXP_NAME  !#!  
!
! ------------------------------------------------------------------------
!
      SUBROUTINE CLBR_FIX ( NV, BUF_VDA )
! ************************************************************************
! *                                                                      *
! *   Auxilliary program  CLBR_FIX sets the number of clock breaks to    *
! *   zero.                                                              *
! *                                                                      *
! *  ### 23-MAR-2024    CLBR_FIX   v1.0 (c)  L. Petrov  23-MAR-2024 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INTEGER*4  NV
      CHARACTER  BUF_VDA(NV)*(*)
      INTEGER*4    MIND
      PARAMETER  ( MIND = 256 )
      INTEGER*4  J1, LIND, IND(2,MIND), IER
!
      DO 410 J1=1,NV
         IF ( BUF_VDA(J1)(1:15) == 'DATA.1 NUM_CLBR' ) THEN
              CALL EXWORD ( BUF_VDA(J1), MIND, LIND, IND, CHAR(32), IER )
              BUF_VDA(J1)(IND(1,7):IND(1,7)) = '0'
              CALL CLRCH ( BUF_VDA(J1)(IND(1,7)+1:) )
         END IF
 410  CONTINUE 
!
      RETURN
      END  SUBROUTINE  CLBR_FIX   !#!#
