      SUBROUTINE OPA_CONFIG ( CONFIG_FILE, OPA, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine OPA_CONFIG reads file CONFIG_FILE with configuration of    *
! *   OPA utility, parses its content and fills field of a record OPA.   *
! *   Syntax of configuration file:                                      *
! *   1) Lines starting with ##  ( Two symbols: <DIES> <DIES>) are       *
! *      recognized as comments and ignored.                             *
! *   2) Lines starting with #  ( Two symbols: <DIES> <BLANK>) are       *
! *      considered as a specifications of opa configuration.            *
! *      Format: keyword, one or more delimiters, value.                 *
! *      Keywords are case-insensitive, but values are case-sensitive.   *
! *      Delimiter is one of <BLANK> <TAB> <BINARY_ZERO>.                *
! *   No defaults can be used. All keywords are to be specified.         *
! *                                                                      *
! * ________________________ Input parameters: _________________________ *
! *                                                                      *
! *  CONFIG_FILE ( CHARACTER ) -- File name of the OPA configuration    *
! *                               file.                                  *
! *                                                                      *
! * ________________________ Output parameters: ________________________ *
! *                                                                      *
! *          OPA ( RECORD    ) -- Object with data structure for keeping *
! *                               configuration of OPA.                  *
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
! *  ### 14-AUG-2000   OPA_CONFIG  v1.5 (c)  L. Petrov  27-SEP-2007###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE    'opa.i'
      CHARACTER  CONFIG_FILE*(*)
      TYPE ( OPA__STRU ) ::  OPA
      INTEGER*4  IUER, MIND
      PARAMETER  ( MIND= 32 )
      CHARACTER  STR*80, STR1*80, BUF(M_BUF)*128, DELIM*3
      PARAMETER  ( DELIM = CHAR(0)//CHAR(32)//CHAR(9) )
      INTEGER*4  LEN_OPR, J1, N_BUF, I_PAR, LIND, IND(2,MIND), IO, IER
      INTEGER*4, EXTERNAL :: ILEN, I_LEN
!
! --- Clean record OPA
!
      LEN_OPR = LOC(OPA%LAST_FIELD) - LOC(OPA%FIRST_FIELD) + 4
      CALL NOUT ( LEN_OPR, OPA )
!
! --- Save name of the configuration file
!
      OPA%CONFIG_FILE = CONFIG_FILE
!
! --- Read configuration file into the buffer BUF
!
      CALL ERR_PASS ( IUER, IER )
      CALL RD_TEXT  ( CONFIG_FILE, M_BUF, BUF, N_BUF, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 4121, IUER, 'OPA_CONFIG', 'Error in attempt '// &
     &         'to open OPA configuration file '//CONFIG_FILE )
           RETURN
      END IF
!
! --- Scan the buffer with copy of configuration file
!
      I_PAR = 0
      DO 410 J1=1,N_BUF
!
! ------ Skip comments
!
         IF ( ILEN(BUF(J1)) .EQ.  0   ) GOTO 410
         IF ( BUF(J1)(1:1)  .NE. '#'  ) GOTO 410
         IF ( BUF(J1)(1:2)  .EQ. '#!' ) GOTO 410
         IF ( BUF(J1)(1:2)  .EQ. '##' ) GOTO 410
!
! ------ Split the line onto words
!
         CALL EXWORD ( BUF(J1), MIND, LIND, IND, DELIM, -3 )
         CALL CLRCH ( STR )
         CALL INCH  ( J1, STR )
!
! ------ It should be exactly three words. Let's check it
!
         IF ( LIND .LT. 3 ) THEN
              CALL ERR_LOG ( 4122, IUER, 'OPA_CONFIG', 'Error in '// &
     &            'parsing the '//STR(1:I_LEN(STR))//'-th line in the '// &
     &            'configuration file '//CONFIG_FILE(1:I_LEN(CONFIG_FILE))// &
     &            ': too few words' )
              RETURN
         END IF
!
! ------ Well. Three words. Parse them
!
! ------ Transform the keyword to the letters of upper case
!
         CALL TRAN ( 11, BUF(J1)(IND(1,2):IND(2,2)), &
     &                   BUF(J1)(IND(1,2):IND(2,2))  )
!
         IF ( BUF(J1)(IND(1,2):IND(2,2)) .EQ. 'MASTER_DIR:' ) THEN
              OPA%MASTER_DIR  = BUF(J1)(IND(1,3):IND(2,LIND))
            ELSE IF ( BUF(J1)(IND(1,2):IND(2,2)) .EQ. 'URL_IVSCONTROL:' ) THEN
              OPA%URL_IVSCONTROL = BUF(J1)(IND(1,3):IND(2,LIND))
            ELSE IF ( BUF(J1)(IND(1,2):IND(2,2)) .EQ. 'SESSION_DIR:' ) THEN
              OPA%SESSION_DIR = BUF(J1)(IND(1,3):IND(2,LIND))
            ELSE IF ( BUF(J1)(IND(1,2):IND(2,2)) .EQ. 'BAS_WEIGHT_CNT:' ) THEN
              OPA%BAS_WEIGHT_CNT = BUF(J1)(IND(1,3):IND(2,LIND))
            ELSE IF ( BUF(J1)(IND(1,2):IND(2,2)) .EQ. 'SIT_WEIGHT_CNT:' ) THEN
              OPA%SIT_WEIGHT_CNT = BUF(J1)(IND(1,3):IND(2,LIND))
            ELSE IF ( BUF(J1)(IND(1,2):IND(2,2)) .EQ. 'EOPS_CNT:' ) THEN
              OPA%EOPS_CNT = BUF(J1)(IND(1,3):IND(2,LIND))
            ELSE IF ( BUF(J1)(IND(1,2):IND(2,2)) .EQ. 'STANDALONE_CNT:' ) THEN
              OPA%STANDALONE_CNT = BUF(J1)(IND(1,3):IND(2,LIND))
            ELSE IF ( BUF(J1)(IND(1,2):IND(2,2)) .EQ. 'STANDALONE_ID:' ) THEN
              OPA%STANDALONE_ID = BUF(J1)(IND(1,3):IND(2,LIND))
            ELSE IF ( BUF(J1)(IND(1,2):IND(2,2)) .EQ. 'EOPS_CGM:' ) THEN
              OPA%EOPS_CGM = BUF(J1)(IND(1,3):IND(2,LIND))
            ELSE IF ( BUF(J1)(IND(1,2):IND(2,2)) .EQ. 'GEN_INPERP:' ) THEN
              OPA%GEN_INPERP = BUF(J1)(IND(1,3):IND(2,LIND))
            ELSE IF ( BUF(J1)(IND(1,2):IND(2,2)) .EQ. 'BAS_WEIGHT_FILE:' ) THEN
              OPA%BAS_WEIGHT_FILE = BUF(J1)(IND(1,3):IND(2,LIND))
            ELSE IF ( BUF(J1)(IND(1,2):IND(2,2)) .EQ. 'SIT_WEIGHT_FILE:' ) THEN
              OPA%SIT_WEIGHT_FILE = BUF(J1)(IND(1,3):IND(2,LIND))
            ELSE IF ( BUF(J1)(IND(1,2):IND(2,2)) .EQ. 'GLO_STA_FILE:' ) THEN
              OPA%GLO_STA_FILE = BUF(J1)(IND(1,3):IND(2,LIND))
            ELSE IF ( BUF(J1)(IND(1,2):IND(2,2)) .EQ. 'GLO_SRC_FILE:' ) THEN
              OPA%GLO_SRC_FILE = BUF(J1)(IND(1,3):IND(2,LIND))
            ELSE IF ( BUF(J1)(IND(1,2):IND(2,2)) .EQ. 'GLO_ARC_FILE:' ) THEN
              OPA%GLO_ARC_FILE = BUF(J1)(IND(1,3):IND(2,LIND))
            ELSE IF ( BUF(J1)(IND(1,2):IND(2,2)) .EQ. 'SESSION_TYPE:' ) THEN
              OPA%SESSION_TYPE = BUF(J1)(IND(1,3):IND(2,LIND))
            ELSE IF ( BUF(J1)(IND(1,2):IND(2,2)) .EQ. 'MIN_DIRATION:' ) THEN
              CALL CLRCH ( STR )
              STR = BUF(J1)(IND(1,3):IND(2,LIND))
              IF ( INDEX ( STR, '.' ) .EQ. 0 ) STR = STR(1:I_LEN(STR))//'.0'
              READ ( UNIT=STR, FMT='(F12.6)', IOSTAT=IO ) OPA%MIN_DURATION
              IF ( IO .NE. 0 ) THEN
                   CALL ERR_LOG ( 4123, IUER, 'OPA_CONFIG', 'Error in '// &
     &                 'parsing the '//STR(1:I_LEN(STR))//'-th line in the '// &
     &                 'configuration file '// &
     &                  CONFIG_FILE(1:I_LEN(CONFIG_FILE))// &
     &                 ': eror of format transformation' )
                   RETURN
              END IF
            ELSE IF ( BUF(J1)(IND(1,2):IND(2,2)) .EQ. 'MAX_DIRATION:' ) THEN
              CALL CLRCH ( STR )
              STR = BUF(J1)(IND(1,3):IND(2,LIND))
              IF ( INDEX ( STR, '.' ) .EQ. 0 ) STR = STR(1:I_LEN(STR))//'.0'
              READ ( UNIT=STR, FMT='(F12.6)', IOSTAT=IO ) OPA%MAX_DURATION
              IF ( IO .NE. 0 ) THEN
                   CALL ERR_LOG ( 4124, IUER, 'OPA_CONFIG', 'Error in '// &
     &                 'parsing the '//STR(1:I_LEN(STR))//'-th line in the '// &
     &                 'configuration file '// &
     &                  CONFIG_FILE(1:I_LEN(CONFIG_FILE))// &
     &                 ': eror of format transformation' )
                   RETURN
              END IF
            ELSE IF ( BUF(J1)(IND(1,2):IND(2,2)) .EQ. 'EOPB_FILE:' ) THEN
              OPA%EOPB_FILE = BUF(J1)(IND(1,3):IND(2,LIND))
            ELSE IF ( BUF(J1)(IND(1,2):IND(2,2)) .EQ. 'EOPT_FILE:' ) THEN
              OPA%EOPT_FILE = BUF(J1)(IND(1,3):IND(2,LIND))
            ELSE IF ( BUF(J1)(IND(1,2):IND(2,2)) .EQ. 'EOPK_FILE:' ) THEN
              OPA%EOPK_FILE = BUF(J1)(IND(1,3):IND(2,LIND))
            ELSE IF ( BUF(J1)(IND(1,2):IND(2,2)) .EQ. 'EOPS_FILE:' ) THEN
              OPA%EOPS_FILE = BUF(J1)(IND(1,3):IND(2,LIND))
            ELSE IF ( BUF(J1)(IND(1,2):IND(2,2)) .EQ. 'EOPM_FILE:' ) THEN
              OPA%EOPM_FILE = BUF(J1)(IND(1,3):IND(2,LIND))
            ELSE IF ( BUF(J1)(IND(1,2):IND(2,2)) .EQ. 'EOPM_ONLY_SINGLE_BASELINE:' ) THEN
              CALL TRAN ( 11, BUF(J1)(IND(1,3):IND(2,3)), &
     &                        BUF(J1)(IND(1,3):IND(2,3)) )
              IF ( BUF(J1)(IND(1,3):IND(2,3)) == 'YES'  .OR. &
     &             BUF(J1)(IND(1,3):IND(2,3)) == 'TRUE'      ) THEN
                   OPA%EOPM_ONLY_SINGLE_BASELINE = .TRUE.
                ELSE IF ( BUF(J1)(IND(1,3):IND(2,3)) == 'NO'    .OR. &
     &                    BUF(J1)(IND(1,3):IND(2,3)) == 'FALSE'      ) THEN
                   OPA%EOPM_ONLY_SINGLE_BASELINE = .FALSE.
                ELSE 
                   CALL ERR_LOG ( 4125, IUER, 'OPE_CONFIG', 'Wrong value '// &
     &                 'of the filed EOPM_ONLY_SINGLE_BASELINE: '// &
     &                 BUF(J1)(IND(1,3):IND(2,3))//' in the configuration '// &
     &                 'file '//CONFIG_FILE(1:I_LEN(CONFIG_FILE))// &
     &                 ' : only YES or NO are allowed' )
                   RETURN 
              END IF
            ELSE IF ( BUF(J1)(IND(1,2):IND(2,2)) .EQ. 'IVS_DB_URL:' ) THEN
              OPA%IVS_DB_URL = BUF(J1)(IND(1,3):IND(2,LIND))
            ELSE IF ( BUF(J1)(IND(1,2):IND(2,2)) .EQ. 'DBS_CONF:' ) THEN
              OPA%DBS_CONF = BUF(J1)(IND(1,3):IND(2,LIND))
            ELSE IF ( BUF(J1)(IND(1,2):IND(2,2)) .EQ. 'EOS_CONF:' ) THEN
              OPA%EOS_CONF = BUF(J1)(IND(1,3):IND(2,LIND))
            ELSE IF ( BUF(J1)(IND(1,2):IND(2,2)) .EQ. 'EOM_CONF:' ) THEN
              OPA%EOM_CONF = BUF(J1)(IND(1,3):IND(2,LIND))
            ELSE IF ( BUF(J1)(IND(1,2):IND(2,2)) .EQ. 'SNX_CONF:' ) THEN
              OPA%SNX_CONF = BUF(J1)(IND(1,3):IND(2,LIND))
            ELSE IF ( BUF(J1)(IND(1,2):IND(2,2)) .EQ. 'SKED_EXE:' ) THEN
              OPA%SKED_EXE = BUF(J1)(IND(1,3):IND(2,LIND))
            ELSE IF ( BUF(J1)(IND(1,2):IND(2,2)) .EQ. 'NUM_USED_MIN:' ) THEN
              CALL CHIN ( BUF(J1)(IND(1,3):IND(2,LIND)), OPA%NUM_USED_MIN )
              IF ( OPA%NUM_USED_MIN < 0 .OR. OPA%NUM_USED_MIN > 32768 ) THEN
                   CALL ERR_LOG ( 4126, IUER, 'OPA_CONFIG', 'Error in '// &
     &                 'parsing parameter NUM_USED_MIN in the configuration '// &
     &                'file '//CONFIG_FILE(1:I_LEN(CONFIG_FILE))// &
     &                ': wrong value '//BUF(J1)(IND(1,3):IND(2,LIND))// &
     &                ' was found, but a non-negative integer was expected' )
                   RETURN
              END IF
            ELSE IF ( BUF(J1)(IND(1,2):IND(2,2)) .EQ. 'VDB_UPDATE_EXE:' ) THEN
              OPA%VDB_UPDATE_EXE = BUF(J1)(IND(1,3):IND(2,LIND))
            ELSE IF ( BUF(J1)(IND(1,2):IND(2,2)) .EQ. 'SNR_PLOT:' ) THEN
              STR = BUF(J1)(IND(1,3):IND(2,LIND))
              CALL TRAN ( 11, STR, STR )
              IF ( STR .EQ. 'F'      .OR. &
     &             STR .EQ. 'FALSE'  .OR. &
     &             STR .EQ. 'N'      .OR. &
     &             STR .EQ. 'NO'          ) THEN
                   OPA%SNR_PLOT = .FALSE.
                 ELSE IF ( STR .EQ. 'T'    .OR. &
     &                     STR .EQ. 'TRUE' .OR. &
     &                     STR .EQ. 'Y'    .OR. &
     &                     STR .EQ. 'YES'       ) THEN
                   OPA%SNR_PLOT = .TRUE.
                ELSE
                   CALL ERR_LOG ( 4127, IUER, 'OPA_CONFIG', 'Error in '// &
     &                 'parsing the '//STR(1:I_LEN(STR))//'-th line in the '// &
     &                'configuration file '//CONFIG_FILE(1:I_LEN(CONFIG_FILE))// &
     &                ': wrong value '//STR(1:I_LEN(STR))//' was found . '// &
     &                'One of YES or NO was expected' )
                   RETURN
              END IF
            ELSE IF ( BUF(J1)(IND(1,2):IND(2,2)) .EQ. 'SNR_HIST:' ) THEN
              STR = BUF(J1)(IND(1,3):IND(2,LIND))
              CALL TRAN ( 11, STR, STR )
              IF ( STR .EQ. 'F'      .OR. &
     &             STR .EQ. 'FALSE'  .OR. &
     &             STR .EQ. 'N'      .OR. &
     &             STR .EQ. 'NO'          ) THEN
                   OPA%SNR_HIST = .FALSE.
                 ELSE IF ( STR .EQ. 'T'    .OR. &
     &                     STR .EQ. 'TRUE' .OR. &
     &                     STR .EQ. 'Y'    .OR. &
     &                     STR .EQ. 'YES'       ) THEN
                   OPA%SNR_HIST = .TRUE.
                ELSE
                   CALL ERR_LOG ( 4128, IUER, 'OPA_CONFIG', 'Error in '// &
     &                 'parsing the '//STR(1:I_LEN(STR))//'-th line in the '// &
     &                'configuration file '//CONFIG_FILE(1:I_LEN(CONFIG_FILE))// &
     &                ': wrong value '//STR(1:I_LEN(STR))//' was found . '// &
     &                'One of YES or NO was expected' )
                   RETURN
              END IF
            ELSE IF ( BUF(J1)(IND(1,2):IND(2,2)) .EQ. 'TMP_DIR:' ) THEN
              OPA%TMP_DIR = BUF(J1)(IND(1,3):IND(2,LIND))
            ELSE IF ( BUF(J1)(IND(1,2):IND(2,2)) .EQ. 'SESUPD_LOG:' ) THEN
              OPA%SESUPD_LOG = BUF(J1)(IND(1,3):IND(2,LIND))
            ELSE IF ( BUF(J1)(IND(1,2):IND(2,2)) .EQ. 'WGET_EXE:' ) THEN
              OPA%WGET_EXE = BUF(J1)(IND(1,3):IND(2,LIND))
            ELSE
              CALL ERR_LOG ( 4129, IUER, 'OPA_CONFIG', 'Error in '// &
     &            'parsing the '//STR(1:I_LEN(STR))//'-th line in the '// &
     &            'configuration file '//CONFIG_FILE(1:I_LEN(CONFIG_FILE))// &
     &            ': unsupported keyword '//BUF(J1)(IND(1,2):IND(2,2)) )
              RETURN
         END IF
         I_PAR = I_PAR + 1
 410  CONTINUE
!
! --- Check: did we define all fields of OPA?
!
      IF ( I_PAR .LT. M_PAR ) THEN
           CALL CLRCH ( STR  )
           CALL INCH  ( I_PAR, STR  )
           CALL CLRCH ( STR1 )
           CALL INCH  ( M_PAR, STR1 )
           CALL ERR_LOG ( 4130, IUER, 'OPA_CONFIG', 'Not all parameters '// &
     &         'were found in OPA configuration file '// &
     &          CONFIG_FILE(1:I_LEN(CONFIG_FILE))//' --  '//STR(1:I_LEN(STR))// &
     &         ' instead of '//STR1 )
           RETURN
      END IF
!
      IF ( OPA%SESSION_TYPE(1:7) .EQ. 'DIURNAL'   .OR. &
     &     OPA%SESSION_TYPE(1:9) .EQ. 'INTENSIVE'      ) THEN
           CONTINUE
         ELSE
           CALL ERR_LOG ( 4131, IUER, 'OPA_CONFIG', 'Wrong value of the '// &
     &         'keyword SESSION_TYPE: "'// &
     &          OPA%SESSION_TYPE(1:I_LEN(OPA%SESSION_TYPE))//'" one of '// &
     &         'DIURNAL or INTENSIVE were expected' )
           RETURN
      END IF
!
! --- Add trailing "/" to directory names, if necessary
!
      IF ( OPA%SESSION_DIR(I_LEN(OPA%SESSION_DIR):I_LEN(OPA%SESSION_DIR)) &
     &     .NE. '/' ) THEN
           OPA%SESSION_DIR(I_LEN(OPA%SESSION_DIR)+1:) = '/'
      END IF
      IF ( OPA%MASTER_DIR(I_LEN(OPA%MASTER_DIR):I_LEN(OPA%MASTER_DIR)) &
     &     .NE. '/' ) THEN
           OPA%MASTER_DIR(I_LEN(OPA%MASTER_DIR)+1:) = '/'
      END IF
      IF ( OPA%TMP_DIR(I_LEN(OPA%TMP_DIR):I_LEN(OPA%TMP_DIR)) .NE. '/' ) THEN
           OPA%TMP_DIR(I_LEN(OPA%TMP_DIR)+1:) = '/'
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  OPA_CONFIG  #!#
